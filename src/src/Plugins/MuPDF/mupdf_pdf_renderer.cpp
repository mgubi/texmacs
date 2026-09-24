
/******************************************************************************
* MODULE     : mupdf_pdf_renderer.cpp
* DESCRIPTION: Renderer which writes a PDF document using MuPDF
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
*
* PROTOTYPE. An alternative to pdf_hummus_renderer which produces the PDF
* with MuPDF instead of the vendored PDFHummus. What it does and what it
* still cannot do is written down in docs/pdf-output-with-mupdf.md; the
* short version is that the page contents (paths, images, text in fonts
* MuPDF can embed) come out right, while the Type 1 fonts of TeX are
* embedded whole because MuPDF subsets TrueType and CFF only.
*
* The structure follows pdf_hummus_renderer: one page at a time, a device
* to draw into, and the objects which are not part of a page (the links,
* the outline, the metadata) collected and written when the document is
* closed.
*
* Coordinates: TeXmacs works in SI, the renderer in "pixels" of 1/dpi inch
* (to_x, to_y, y upwards from the bottom left of the paper, as in the PDF
* coordinate system), and the fz device takes points (1/72 inch) with y
* downwards from the top left. to_fx and to_fy do the whole conversion.
*
******************************************************************************/

#include "mupdf_pdf_renderer.hpp"
#include "mupdf_renderer.hpp"
#include "mupdf_picture.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "scheme.hpp"
#include "tm_timer.hpp"
#include "hashmap.hpp"
#include "iterator.hpp"

#include <mupdf/fitz.h>
#include <mupdf/pdf.h>

extern url tt_font_find (string name); // font_select.cpp

/******************************************************************************
* The renderer
******************************************************************************/

// an entry of the outline tree, collected by toc_entry
struct pdf_outline_item {
  string title;
  int    page;      // 0 based
  double x, y;      // in points, y upwards
  int    level;     // 1 is the outermost
  pdf_outline_item (string t, int p, double x2, double y2, int l)
    : title (t), page (p), x (x2), y (y2), level (l) {}
  pdf_outline_item () : page (0), x (0), y (0), level (1) {}
};

// a link, collected by href and written with its page
struct pdf_link_item {
  string label;                 // "#anchor" or a URL
  double x1, y1, x2, y2;        // in points, y upwards
  pdf_link_item (string l, double a, double b, double c, double d)
    : label (l), x1 (a), y1 (b), x2 (c), y2 (d) {}
  pdf_link_item () : x1 (0), y1 (0), x2 (0), y2 (0) {}
};

// a named destination, collected by anchor
struct pdf_dest_item {
  int    page;
  double x, y;
  pdf_dest_item (int p, double x2, double y2) : page (p), x (x2), y (y2) {}
  pdf_dest_item () : page (0), x (0), y (0) {}
};

class mupdf_pdf_renderer_rep : public renderer_rep {
  static const int default_dpi= 72; // the PDF coordinate system

  url    pdf_file_name;
  int    dpi;
  int    nr_pages;
  string page_type;
  bool   landscape;
  double paper_w, paper_h;   // cm
  double width, height;      // points
  bool   started;
  int    page_num;

  fz_context*   ctx;
  pdf_document* doc;
  fz_device*    dev;         // the device of the page being written
  fz_buffer*    contents;
  pdf_obj*      resources;
  array<pdf_obj*> pages;     // the page objects, to hang the links on

  // graphics state
  pencil pen;
  brush  bgb, fgb;
  int    clip_level;

  // the text of the current run: the glyphs are accumulated and handed to
  // the device in one go, so that the content stream holds one text object
  // per colour rather than one per glyph
  fz_text* text;
  color    text_color;

  // fonts, by the res_name of the TeXmacs font. A NULL means a font which
  // MuPDF cannot embed: its glyphs are drawn as bitmaps (see draw)
  hashmap<string,pointer> fonts;

  // what is written when the document is closed
  array<pdf_outline_item> outlines;
  array<pdf_link_item>    links;    // of the page being written
  hashmap<string,pdf_dest_item> dests;
  hashmap<string,string>  metadata;

  double to_x (SI x) { x += ox; return (x>=0 ? x : x-pixel+1) / (double) pixel; }
  double to_y (SI y) { y += oy; return (y>=0 ? y : y-pixel+1) / (double) pixel; }
  // the same, in the coordinates of the fz device (points, y downwards)
  float to_fx (SI x) { return (float) (to_x (x) * default_dpi / dpi); }
  float to_fy (SI y) { return (float) (height - to_y (y) * default_dpi / dpi); }
  float to_fw (SI w) { return (float) (((double) w / pixel) * default_dpi / dpi); }

  void begin_page ();
  void end_page ();
  void flush_text ();
  fz_font* get_font (font_glyphs fn);
  void draw_bitmap_glyph (int ch, font_glyphs fn, SI x, SI y);
  void fill_fz_path (fz_path* path, color c, bool stroke);
  void write_outline ();
  void write_links (int page, pdf_obj* pobj, array<pdf_link_item> ls);
  void write_metadata ();

public:
  mupdf_pdf_renderer_rep (url pdf_file_name, int dpi, int nr_pages,
                          string page_type, bool landscape,
                          double paper_w, double paper_h);
  ~mupdf_pdf_renderer_rep ();

  bool is_printer ();
  bool is_started ();
  void next_page ();

  void set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  pencil get_pencil ();
  brush  get_background ();
  void   set_pencil (pencil p);
  void   set_brush (brush b);
  void   set_background (brush b);

  void clear_device (SI x1, SI y1, SI x2, SI y2) { (void) x1; (void) y1; (void) x2; (void) y2; }
  void draw (int char_code, font_glyphs fn, SI x, SI y);
  void line (SI x1, SI y1, SI x2, SI y2);
  void lines (array<SI> x, array<SI> y);
  void clear (SI x1, SI y1, SI x2, SI y2);
  void fill (SI x1, SI y1, SI x2, SI y2);
  void arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void polygon (array<SI> x, array<SI> y, bool convex= true);
  void draw_picture (picture p, SI x, SI y, int alpha);
  void draw_scalable (scalable im, SI x, SI y, int alpha);

  // not meaningful for a file: a printer renderer has no shadows
  void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y);
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);

  void anchor (string label, SI x1, SI y1, SI x2, SI y2);
  void href (string label, SI x1, SI y1, SI x2, SI y2);
  void toc_entry (string kind, string title, SI x, SI y);
  void set_metadata (string kind, string val);
};

/******************************************************************************
* Colours
******************************************************************************/

static void
tm_color_to_fz (color c, float* rgb, float& alpha) {
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  rgb[0]= r / 255.0f; rgb[1]= g / 255.0f; rgb[2]= b / 255.0f;
  alpha= a / 255.0f;
}

/******************************************************************************
* Construction and destruction
******************************************************************************/

mupdf_pdf_renderer_rep::mupdf_pdf_renderer_rep (
  url pdf_file_name2, int dpi2, int nr_pages2,
  string page_type2, bool landscape2, double paper_w2, double paper_h2)
  : renderer_rep (false),
    pdf_file_name (pdf_file_name2), dpi (dpi2), nr_pages (nr_pages2),
    page_type (page_type2), landscape (landscape2),
    paper_w (paper_w2), paper_h (paper_h2),
    started (false), page_num (0),
    ctx (mupdf_context ()), doc (NULL), dev (NULL),
    contents (NULL), resources (NULL),
    pen (black), bgb (white), fgb (black), clip_level (0),
    text (NULL), text_color (black),
    fonts ((pointer) NULL)
{
  width = default_dpi * paper_w / 2.54;
  height= default_dpi * paper_h / 2.54;
  fz_try (ctx) {
    doc= pdf_create_document (ctx);
    started= true;
  }
  fz_catch (ctx) {
    convert_error << "MuPDF cannot create the document: "
                  << fz_caught_message (ctx) << LF;
    doc= NULL;
  }
  if (started) begin_page ();
}

mupdf_pdf_renderer_rep::~mupdf_pdf_renderer_rep () {
  if (!started) return;
  end_page ();
  fz_try (ctx) {
    write_outline ();
    write_metadata ();
    // Subsetting: MuPDF rewrites the font files of the whole document from
    // the glyphs it finds used. It handles TrueType (glyf) and CFF; the
    // Type 1 fonts of TeX are left whole, which is the main difference
    // from the output of pdf_hummus_renderer (see the notes).
    pdf_subset_fonts (ctx, doc, 0, NULL);
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF could not subset the fonts: "
                    << fz_caught_message (ctx) << LF;
  }
  fz_try (ctx) {
    pdf_write_options opts= pdf_default_write_options;
    opts.do_compress= 1;
    opts.do_compress_images= 1;
    opts.do_compress_fonts= 1;
    opts.do_garbage= 4;
    c_string name (concretize (pdf_file_name));
    pdf_save_document (ctx, doc, name, &opts);
  }
  fz_catch (ctx) {
    convert_error << "MuPDF cannot write " << pdf_file_name << ": "
                  << fz_caught_message (ctx) << LF;
  }
  // the fonts are dropped, the page objects released
  iterator<string> it= iterate (fonts);
  while (it->busy ()) {
    fz_font* f= (fz_font*) fonts [it->next ()];
    if (f != NULL) fz_drop_font (ctx, f);
  }
  for (int i=0; i<N(pages); i++) pdf_drop_obj (ctx, pages[i]);
  pdf_drop_document (ctx, doc);
}

bool mupdf_pdf_renderer_rep::is_printer () { return true; }
bool mupdf_pdf_renderer_rep::is_started () { return started; }

/******************************************************************************
* Pages
******************************************************************************/

void
mupdf_pdf_renderer_rep::begin_page () {
  fz_try (ctx) {
    fz_rect mediabox= fz_make_rect (0, 0, (float) width, (float) height);
    dev= pdf_page_write (ctx, doc, mediabox, &resources, &contents);
  }
  fz_catch (ctx) {
    convert_error << "MuPDF cannot start a page: "
                  << fz_caught_message (ctx) << LF;
    dev= NULL;
    return;
  }
  pen= pencil (black);
  bgb= brush (white);
  fgb= brush (black);
  clip_level= 0;
  text= NULL;
  links= array<pdf_link_item> ();
  set_origin (0, (SI) (paper_h * dpi * pixel / 2.54));
  set_clipping (0, (SI) ((-dpi * pixel * paper_h) / 2.54),
                (SI) (( dpi * pixel * paper_w) / 2.54), 0);
}

void
mupdf_pdf_renderer_rep::end_page () {
  if (dev == NULL) return;
  flush_text ();
  fz_try (ctx) {
    while (clip_level-- > 0) fz_pop_clip (ctx, dev);
    clip_level= 0;
    fz_close_device (ctx, dev);
    fz_rect mediabox= fz_make_rect (0, 0, (float) width, (float) height);
    pdf_obj* page= pdf_add_page (ctx, doc, mediabox, 0, resources, contents);
    write_links (page_num, page, links);
    pdf_insert_page (ctx, doc, -1, page);
    pages << page;   // kept: the destinations point at it
  }
  fz_catch (ctx) {
    convert_error << "MuPDF cannot close a page: "
                  << fz_caught_message (ctx) << LF;
  }
  fz_drop_device (ctx, dev); dev= NULL;
  fz_drop_buffer (ctx, contents); contents= NULL;
  pdf_drop_obj (ctx, resources); resources= NULL;
  page_num++;
}

void
mupdf_pdf_renderer_rep::next_page () {
  end_page ();
  begin_page ();
}

/******************************************************************************
* The graphics state
******************************************************************************/

pencil mupdf_pdf_renderer_rep::get_pencil () { return pen; }
brush  mupdf_pdf_renderer_rep::get_background () { return bgb; }

void
mupdf_pdf_renderer_rep::set_pencil (pencil p) {
  flush_text ();
  pen= p;
  fgb= brush (pen->get_color ());
}

void
mupdf_pdf_renderer_rep::set_brush (brush b) {
  flush_text ();
  fgb= b;
  pen= pencil (b->get_color (), pen->get_width ());
}

void
mupdf_pdf_renderer_rep::set_background (brush b) { bgb= b; }

void
mupdf_pdf_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  if (dev == NULL) return;
  flush_text ();
  renderer_rep::set_clipping (x1, y1, x2, y2, restore);
  fz_try (ctx) {
    if (restore) {
      if (clip_level > 0) { fz_pop_clip (ctx, dev); clip_level--; }
    }
    else {
      fz_rect r= fz_make_rect (to_fx (x1), to_fy (y2), to_fx (x2), to_fy (y1));
      fz_path* path= fz_new_path (ctx);
      fz_moveto (ctx, path, r.x0, r.y0);
      fz_lineto (ctx, path, r.x1, r.y0);
      fz_lineto (ctx, path, r.x1, r.y1);
      fz_lineto (ctx, path, r.x0, r.y1);
      fz_closepath (ctx, path);
      fz_clip_path (ctx, dev, path, 0, fz_identity, fz_infinite_rect);
      fz_drop_path (ctx, path);
      clip_level++;
    }
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF clipping failed: "
                    << fz_caught_message (ctx) << LF;
  }
}

/******************************************************************************
* Paths
******************************************************************************/

void
mupdf_pdf_renderer_rep::fill_fz_path (fz_path* path, color c, bool stroke) {
  if (dev == NULL) { fz_drop_path (ctx, path); return; }
  flush_text ();
  float rgb[3], alpha;
  tm_color_to_fz (c, rgb, alpha);
  fz_try (ctx) {
    if (stroke) {
      fz_stroke_state* st= fz_new_stroke_state (ctx);
      st->linewidth= to_fw (pen->get_width ());
      if (st->linewidth <= 0) st->linewidth= 0.4f;
      fz_linecap cap= (pen->get_cap () == cap_round) ? FZ_LINECAP_ROUND
                                                     : FZ_LINECAP_BUTT;
      st->start_cap= st->end_cap= st->dash_cap= cap;
      st->linejoin= FZ_LINEJOIN_ROUND;
      fz_stroke_path (ctx, dev, path, st, fz_identity, fz_device_rgb (ctx),
                      rgb, alpha, fz_default_color_params);
      fz_drop_stroke_state (ctx, st);
    }
    else
      fz_fill_path (ctx, dev, path, 0, fz_identity, fz_device_rgb (ctx),
                    rgb, alpha, fz_default_color_params);
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF path failed: " << fz_caught_message (ctx) << LF;
  }
  fz_drop_path (ctx, path);
}

void
mupdf_pdf_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  if (dev == NULL) return;
  fz_path* path= fz_new_path (ctx);
  fz_moveto (ctx, path, to_fx (x1), to_fy (y1));
  fz_lineto (ctx, path, to_fx (x2), to_fy (y2));
  fill_fz_path (path, pen->get_color (), true);
}

void
mupdf_pdf_renderer_rep::lines (array<SI> x, array<SI> y) {
  if (dev == NULL || N(x) == 0 || N(x) != N(y)) return;
  fz_path* path= fz_new_path (ctx);
  fz_moveto (ctx, path, to_fx (x[0]), to_fy (y[0]));
  for (int i=1; i<N(x); i++) fz_lineto (ctx, path, to_fx (x[i]), to_fy (y[i]));
  fill_fz_path (path, pen->get_color (), true);
}

void
mupdf_pdf_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  if (dev == NULL) return;
  if ((x1 >= x2) || (y1 >= y2)) return;
  fz_path* path= fz_new_path (ctx);
  fz_moveto (ctx, path, to_fx (x1), to_fy (y1));
  fz_lineto (ctx, path, to_fx (x2), to_fy (y1));
  fz_lineto (ctx, path, to_fx (x2), to_fy (y2));
  fz_lineto (ctx, path, to_fx (x1), to_fy (y2));
  fz_closepath (ctx, path);
  fill_fz_path (path, pen->get_color (), false);
}

void
mupdf_pdf_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  if (dev == NULL) return;
  if ((x1 >= x2) || (y1 >= y2)) return;
  fz_path* path= fz_new_path (ctx);
  fz_moveto (ctx, path, to_fx (x1), to_fy (y1));
  fz_lineto (ctx, path, to_fx (x2), to_fy (y1));
  fz_lineto (ctx, path, to_fx (x2), to_fy (y2));
  fz_lineto (ctx, path, to_fx (x1), to_fy (y2));
  fz_closepath (ctx, path);
  fill_fz_path (path, bgb->get_color (), false);
}

void
mupdf_pdf_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  (void) convex;
  if (dev == NULL || N(x) < 2 || N(x) != N(y)) return;
  fz_path* path= fz_new_path (ctx);
  fz_moveto (ctx, path, to_fx (x[0]), to_fy (y[0]));
  for (int i=1; i<N(x); i++) fz_lineto (ctx, path, to_fx (x[i]), to_fy (y[i]));
  fz_closepath (ctx, path);
  fill_fz_path (path, pen->get_color (), false);
}

// an arc of the ellipse inscribed in the box, from alpha to alpha+delta
// (in 1/64 degrees, as in X11), approximated by cubic segments
static void
append_arc (fz_context* ctx, fz_path* path, float cx, float cy,
            float rx, float ry, double a0, double a1, bool move) {
  const int n= 16;
  for (int i=0; i<=n; i++) {
    double a= a0 + (a1 - a0) * i / n;
    float px= cx + (float) (rx * cos (a));
    float py= cy - (float) (ry * sin (a));   // y downwards in the device
    if (i == 0 && move) fz_moveto (ctx, path, px, py);
    else fz_lineto (ctx, path, px, py);
  }
}

void
mupdf_pdf_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if (dev == NULL) return;
  float cx= (to_fx (x1) + to_fx (x2)) / 2, cy= (to_fy (y1) + to_fy (y2)) / 2;
  float rx= (to_fx (x2) - to_fx (x1)) / 2, ry= (to_fy (y1) - to_fy (y2)) / 2;
  fz_path* path= fz_new_path (ctx);
  double a0= alpha * M_PI / (64 * 180), a1= (alpha + delta) * M_PI / (64 * 180);
  append_arc (ctx, path, cx, cy, rx, ry, a0, a1, true);
  fill_fz_path (path, pen->get_color (), true);
}

void
mupdf_pdf_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if (dev == NULL) return;
  float cx= (to_fx (x1) + to_fx (x2)) / 2, cy= (to_fy (y1) + to_fy (y2)) / 2;
  float rx= (to_fx (x2) - to_fx (x1)) / 2, ry= (to_fy (y1) - to_fy (y2)) / 2;
  fz_path* path= fz_new_path (ctx);
  double a0= alpha * M_PI / (64 * 180), a1= (alpha + delta) * M_PI / (64 * 180);
  fz_moveto (ctx, path, cx, cy);
  append_arc (ctx, path, cx, cy, rx, ry, a0, a1, false);
  fz_closepath (ctx, path);
  fill_fz_path (path, pen->get_color (), false);
}

/******************************************************************************
* Text
******************************************************************************/

// The fz_font of a TeXmacs font, NULL when MuPDF cannot embed it. The file
// is found the way pdf_hummus_renderer finds it, so that the two renderers
// agree on which fonts are "native".
fz_font*
mupdf_pdf_renderer_rep::get_font (font_glyphs fn) {
  string name= fn->res_name;
  if (fonts->contains (name)) return (fz_font*) fonts (name);
  fz_font* font= NULL;
  int pos= search_forwards (":", name);
  string fname= (pos == -1 ? name : name (0, pos));
  url u= tt_font_find (fname);
  if (!is_none (u)) {
    c_string path (concretize (u));
    fz_try (ctx) {
      font= fz_new_font_from_file (ctx, NULL, path, 0, 0);
    }
    fz_catch (ctx) { font= NULL; }
    if (font != NULL && !pdf_font_writing_supported (ctx, font)) {
      fz_drop_font (ctx, font);
      font= NULL;
    }
  }
  if (font == NULL)
    convert_warning << "mupdf_pdf_renderer: " << fname
                    << " cannot be embedded, its glyphs are drawn as bitmaps"
                    << LF;
  fonts (name)= (pointer) font;
  return font;
}

void
mupdf_pdf_renderer_rep::flush_text () {
  if (text == NULL) return;
  if (dev != NULL) {
    float rgb[3], alpha;
    tm_color_to_fz (text_color, rgb, alpha);
    fz_try (ctx) {
      fz_fill_text (ctx, dev, text, fz_identity, fz_device_rgb (ctx),
                    rgb, alpha, fz_default_color_params);
    }
    fz_catch (ctx) {
      convert_warning << "MuPDF text failed: " << fz_caught_message (ctx) << LF;
    }
  }
  fz_drop_text (ctx, text);
  text= NULL;
}

void
mupdf_pdf_renderer_rep::draw (int ch, font_glyphs fn, SI x, SI y) {
  if (dev == NULL) return;
  glyph gl= fn->get (ch);
  if (is_nil (gl)) return;
  fz_font* font= get_font (fn);
  if (font == NULL) { draw_bitmap_glyph (ch, fn, x, y); return; }
  color c= pen->get_color ();
  if (text != NULL && c != text_color) flush_text ();
  text_color= c;
  // The size is in the name of the font, "ecrm10.600" being the 10 point
  // design of ecrm at 600 dpi. pdf_hummus_renderer turns that into
  // size * dpi_name / 72, which is a length in the pixels of its content
  // stream (it scales the whole stream by 72/dpi); here the device takes
  // points, so the same length is size * dpi_name / dpi.
  string name= fn->res_name;
  int pos= search_backwards (".", name);
  double size= 10;
  if (pos > 0) {
    int sz= pos - 1;
    while (sz > 0 && is_numeric (name[sz-1])) sz--;
    double s= as_double (name (sz, pos));
    if (s != 0) size= s;
    int end= pos + 1;
    while (end < N(name) && is_numeric (name[end])) end++;
    double d= as_double (name (pos+1, end));
    if (d != 0 && dpi != 0) size= size * d / (double) dpi;
  }
  fz_try (ctx) {
    if (text == NULL) text= fz_new_text (ctx);
    // the outlines of a glyph are y upwards and the device is y
    // downwards, hence the negative scale
    fz_matrix trm= fz_make_matrix ((float) size, 0, 0, (float) -size,
                                   to_fx (x), to_fy (y));
    // the glyph is addressed by its index in the font, the character by
    // its code: the second is what the ToUnicode map is built from, so
    // that the text can be searched and copied out of the document
    fz_show_glyph (ctx, text, font, trm, (int) gl->index, ch, 0, 0,
                   FZ_BIDI_LTR, FZ_LANG_UNSET);
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF glyph failed: " << fz_caught_message (ctx) << LF;
  }
}

// A glyph of a font MuPDF cannot embed, drawn as a bitmap. The Hummus
// renderer builds a Type 3 font out of these instead, which is both
// smaller and searchable; doing the same here is the main piece of work
// still missing (see the notes).
void
mupdf_pdf_renderer_rep::draw_bitmap_glyph (int ch, font_glyphs fn, SI x, SI y) {
  glyph gl= fn->get (ch);
  if (is_nil (gl)) return;
  int w= gl->width, h= gl->height;
  if (w <= 0 || h <= 0) return;
  flush_text ();
  color c= pen->get_color ();
  int cr, cg, cb, ca;
  get_rgb_color (c, cr, cg, cb, ca);
  fz_pixmap* pix= NULL;
  fz_image* img= NULL;
  fz_try (ctx) {
    pix= fz_new_pixmap (ctx, fz_device_rgb (ctx), w, h, NULL, 1);
    unsigned char* p= fz_pixmap_samples (ctx, pix);
    int stride= fz_pixmap_stride (ctx, pix);
    for (int j=0; j<h; j++) {
      unsigned char* row= p + j * stride;
      for (int i=0; i<w; i++) {
        int on= gl->get_x (i, j);           // 0..255 coverage
        row[4*i+0]= (unsigned char) cr;
        row[4*i+1]= (unsigned char) cg;
        row[4*i+2]= (unsigned char) cb;
        row[4*i+3]= (unsigned char) ((on * ca) / 255);
      }
    }
    img= fz_new_image_from_pixmap (ctx, pix, NULL);
    // the glyph box in device coordinates
    float x0= to_fx (x - gl->xoff * pixel);
    float y0= to_fy (y + gl->yoff * pixel);
    float sx= to_fw (w * pixel), sy= to_fw (h * pixel);
    fz_matrix m= fz_make_matrix (sx, 0, 0, sy, x0, y0);
    fz_fill_image (ctx, dev, img, m, 1.0f, fz_default_color_params);
  }
  fz_always (ctx) {
    fz_drop_image (ctx, img);
    fz_drop_pixmap (ctx, pix);
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF bitmap glyph failed: "
                    << fz_caught_message (ctx) << LF;
  }
}

/******************************************************************************
* Pictures
******************************************************************************/

void
mupdf_pdf_renderer_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  if (dev == NULL) return;
  flush_text ();
  picture p2= as_mupdf_picture (p);
  mupdf_picture_rep* rep= (mupdf_picture_rep*) p2->get_handle ();
  if (rep == NULL || rep->pix == NULL) return;
  int w= rep->w, h= rep->h;
  fz_image* img= NULL;
  fz_try (ctx) {
    img= mupdf_image_from_pixmap (rep->pix);
    float x0= to_fx (x - rep->ox * pixel);
    float y0= to_fy (y + (h - rep->oy) * pixel);
    float sx= to_fw (w * pixel), sy= to_fw (h * pixel);
    fz_matrix m= fz_make_matrix (sx, 0, 0, sy, x0, y0);
    fz_fill_image (ctx, dev, img, m, alpha / 255.0f, fz_default_color_params);
  }
  fz_always (ctx) { fz_drop_image (ctx, img); }
  fz_catch (ctx) {
    convert_warning << "MuPDF picture failed: "
                    << fz_caught_message (ctx) << LF;
  }
}

void
mupdf_pdf_renderer_rep::draw_scalable (scalable im, SI x, SI y, int alpha) {
  // the generic implementation rasterizes it into a picture
  renderer_rep::draw_scalable (im, x, y, alpha);
}

/******************************************************************************
* Links, destinations, the outline and the metadata
******************************************************************************/

void
mupdf_pdf_renderer_rep::anchor (string label, SI x1, SI y1, SI x2, SI y2) {
  (void) x2; (void) y2;
  dests (label)= pdf_dest_item (page_num, to_fx (x1),
                                height - to_fy (y1)); // y upwards, as in a PDF
}

void
mupdf_pdf_renderer_rep::href (string label, SI x1, SI y1, SI x2, SI y2) {
  links << pdf_link_item (label,
                          to_fx (x1 - 5*pixel), height - to_fy (y1 - 10*pixel),
                          to_fx (x2 + 5*pixel), height - to_fy (y2 + 10*pixel));
}

void
mupdf_pdf_renderer_rep::toc_entry (string kind, string title, SI x, SI y) {
  int ls= 1;
  if (kind == "toc-strong-1") ls= 1;
  else if (kind == "toc-strong-2") ls= 2;
  else if (kind == "toc-1") ls= 3;
  else if (kind == "toc-2") ls= 4;
  else if (kind == "toc-3") ls= 5;
  else if (kind == "toc-4") ls= 6;
  else if (kind == "toc-5") ls= 7;
  outlines << pdf_outline_item (title, page_num, to_fx (x),
                                height - to_fy (y + 20*pixel), ls);
}

void
mupdf_pdf_renderer_rep::set_metadata (string kind, string val) {
  metadata (kind)= val;
}

void
mupdf_pdf_renderer_rep::write_links (int page, pdf_obj* pobj,
                                     array<pdf_link_item> ls) {
  (void) page;
  if (N(ls) == 0) return;
  pdf_obj* annots= pdf_dict_put_array (ctx, pobj, PDF_NAME(Annots), N(ls));
  for (int i=0; i<N(ls); i++) {
    pdf_obj* a= pdf_new_dict (ctx, doc, 4);
    pdf_dict_put (ctx, a, PDF_NAME(Type), PDF_NAME(Annot));
    pdf_dict_put (ctx, a, PDF_NAME(Subtype), PDF_NAME(Link));
    pdf_obj* rect= pdf_dict_put_array (ctx, a, PDF_NAME(Rect), 4);
    pdf_array_push_real (ctx, rect, ls[i].x1);
    pdf_array_push_real (ctx, rect, ls[i].y1);
    pdf_array_push_real (ctx, rect, ls[i].x2);
    pdf_array_push_real (ctx, rect, ls[i].y2);
    pdf_obj* border= pdf_dict_put_array (ctx, a, PDF_NAME(Border), 3);
    pdf_array_push_int (ctx, border, 16);
    pdf_array_push_int (ctx, border, 16);
    pdf_array_push_int (ctx, border, 0);
    if (starts (ls[i].label, "#")) {
      // an internal link: the destination is resolved when the document is
      // closed, since the page it points at may not exist yet
      c_string s (ls[i].label (1, N(ls[i].label)));
      pdf_dict_put_text_string (ctx, a, PDF_NAME(Dest), s);
    }
    else {
      pdf_obj* act= pdf_dict_put_dict (ctx, a, PDF_NAME(A), 2);
      pdf_dict_put (ctx, act, PDF_NAME(S), PDF_NAME(URI));
      c_string s (ls[i].label);
      pdf_dict_put_text_string (ctx, act, PDF_NAME(URI), s);
    }
    pdf_array_push_drop (ctx, annots, pdf_add_object_drop (ctx, doc, a));
  }
}

void
mupdf_pdf_renderer_rep::write_outline () {
  if (N(outlines) == 0) return;
  // A flat outline for the prototype: the levels are recorded but not yet
  // nested (pdf_hummus_renderer builds the tree in recurse ()).
  pdf_obj* root= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Root));
  pdf_obj* out= pdf_dict_put_dict (ctx, root, PDF_NAME(Outlines), 4);
  pdf_dict_put (ctx, out, PDF_NAME(Type), PDF_NAME(Outlines));
  pdf_obj* first= NULL; pdf_obj* prev= NULL;
  int count= 0;
  for (int i=0; i<N(outlines); i++) {
    if (outlines[i].page >= N(pages)) continue;
    pdf_obj* item= pdf_new_dict (ctx, doc, 5);
    c_string t (outlines[i].title);
    pdf_dict_put_text_string (ctx, item, PDF_NAME(Title), t);
    pdf_obj* dest= pdf_dict_put_array (ctx, item, PDF_NAME(Dest), 5);
    pdf_array_push (ctx, dest, pages[outlines[i].page]);
    pdf_array_push (ctx, dest, PDF_NAME(XYZ));
    pdf_array_push_real (ctx, dest, outlines[i].x);
    pdf_array_push_real (ctx, dest, outlines[i].y);
    pdf_array_push_int (ctx, dest, 0);
    pdf_obj* ref= pdf_add_object_drop (ctx, doc, item);
    if (first == NULL) first= ref;
    if (prev != NULL) {
      pdf_dict_put (ctx, prev, PDF_NAME(Next), ref);
      pdf_dict_put (ctx, ref, PDF_NAME(Prev), prev);
    }
    pdf_dict_put (ctx, ref, PDF_NAME(Parent), out);
    prev= ref;
    count++;
  }
  if (first != NULL) {
    pdf_dict_put (ctx, out, PDF_NAME(First), first);
    pdf_dict_put (ctx, out, PDF_NAME(Last), prev);
    pdf_dict_put_int (ctx, out, PDF_NAME(Count), count);
  }
}

void
mupdf_pdf_renderer_rep::write_metadata () {
  if (N(metadata) == 0) return;
  pdf_obj* info= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Info));
  if (info == NULL) {
    info= pdf_add_new_dict (ctx, doc, 8);
    pdf_dict_put (ctx, pdf_trailer (ctx, doc), PDF_NAME(Info), info);
  }
  iterator<string> it= iterate (metadata);
  while (it->busy ()) {
    string key= it->next ();
    c_string val (metadata (key));
    if (key == "title")    pdf_dict_put_text_string (ctx, info, PDF_NAME(Title), val);
    else if (key == "author")  pdf_dict_put_text_string (ctx, info, PDF_NAME(Author), val);
    else if (key == "subject") pdf_dict_put_text_string (ctx, info, PDF_NAME(Subject), val);
  }
  c_string producer ("GNU TeXmacs (MuPDF)");
  pdf_dict_put_text_string (ctx, info, PDF_NAME(Producer), producer);
}

/******************************************************************************
* No shadows on paper
******************************************************************************/

void mupdf_pdf_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y) {
  (void) x1; (void) y1; (void) x2; (void) y2; (void) ren; (void) x; (void) y; }
void mupdf_pdf_renderer_rep::new_shadow (renderer& ren) { (void) ren; }
void mupdf_pdf_renderer_rep::delete_shadow (renderer& ren) { (void) ren; }
void mupdf_pdf_renderer_rep::get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2; }
void mupdf_pdf_renderer_rep::put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2) {
  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2; }
void mupdf_pdf_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2; }

/******************************************************************************
* The entry point
******************************************************************************/

renderer
mupdf_pdf_renderer (url pdf_file_name, int dpi, int nr_pages,
                    string page_type, bool landscape,
                    double paper_w, double paper_h) {
  return tm_new<mupdf_pdf_renderer_rep> (pdf_file_name, dpi, nr_pages,
                                         page_type, landscape,
                                         paper_w, paper_h);
}
