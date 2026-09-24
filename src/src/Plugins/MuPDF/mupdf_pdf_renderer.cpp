
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
* with MuPDF instead of the vendored PDFHummus. See
* docs/pdf-output-with-mupdf.md for what it does and what it does not.
*
* The content stream of a page is written here rather than through the fz
* device of pdf_page_write. The device would be less code, but it hands
* every embedded font to pdf_add_cid_font, and that is wrong for a Type 1:
* it puts the Type 1 program in /FontFile under a CIDFontType0 descendant,
* which no reader but MuPDF accepts (Ghostscript substitutes the font).
* Every TeX font is a Type 1, so the choice of font has to be ours, and
* with it the stream which addresses the glyphs.
*
* Coordinates: as in pdf_hummus_renderer, the stream is scaled by 72/dpi
* once per page, so everything below is written in the "pixels" of the
* renderer (1/dpi inch), y upwards, which is what to_x and to_y give.
*
******************************************************************************/

#include "mupdf_pdf_renderer.hpp"
#include "mupdf_renderer.hpp"
#include "mupdf_picture.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "scheme.hpp"
#include "hashmap.hpp"
#include "iterator.hpp"

#include <mupdf/fitz.h>
#include <mupdf/pdf.h>
#include <ft2build.h>
#include FT_FREETYPE_H

extern url tt_font_find (string name); // font_select.cpp

/******************************************************************************
* The pieces which are written when the document is closed
******************************************************************************/

struct pdf_outline_item {
  string title; int page; double x, y; int level;
  pdf_outline_item (string t, int p, double x2, double y2, int l)
    : title (t), page (p), x (x2), y (y2), level (l) {}
  pdf_outline_item () : page (0), x (0), y (0), level (1) {}
};

struct pdf_link_item {
  string label; double x1, y1, x2, y2;
  pdf_link_item (string l, double a, double b, double c, double d)
    : label (l), x1 (a), y1 (b), x2 (c), y2 (d) {}
  pdf_link_item () : x1 (0), y1 (0), x2 (0), y2 (0) {}
};

// A font of the document. Two kinds:
//
//  * simple: a Type 1, written as a /Type1 font whose glyphs are selected
//    by a one byte code through an /Encoding /Differences array of glyph
//    names. A TeX font has at most 256 glyphs, so one code per glyph is
//    always enough, and this is both valid and what PDFHummus does.
//  * CID: everything else, through pdf_add_cid_font: /Type0 with
//    Identity-H, the code being the glyph index. MuPDF writes those
//    correctly and its subsetter handles them.
//
// The dictionary of a simple font is completed when the document is
// closed, since the codes in use are only known then.
struct pdf_font_item {
  fz_font* font;
  pdf_obj* obj;        // the font dictionary
  int      num;        // /F<num> in the resources
  bool     simple;
  array<int> gid;      // 256 entries: the glyph each code selects, -1 if free
  pdf_font_item () : font (NULL), obj (NULL), num (0), simple (false) {}
};

class mupdf_pdf_renderer_rep : public renderer_rep {
  static const int default_dpi= 72;

  url    pdf_file_name;
  int    dpi, nr_pages;
  string page_type;
  bool   landscape;
  double paper_w, paper_h;   // cm
  double width, height;      // points
  bool   started;
  int    page_num;

  fz_context*   ctx;
  pdf_document* doc;
  fz_buffer*    contents;    // the stream of the page being written
  pdf_obj*      resources;   // shared by every page
  pdf_obj*      res_font;
  pdf_obj*      res_xobj;
  pdf_obj*      res_gs;
  array<pdf_obj*> pages;

  // the graphics state as it stands in the stream
  pencil pen;
  brush  bgb, fgb;
  int    clip_level;
  // a colour is an ARGB word and white is 0xffffffff, which is -1 as an
  // int: a sentinel value would collide with it, hence the flags
  color  cur_fill, cur_stroke;
  bool   has_fill, has_stroke;
  double cur_width;
  int    cur_alpha;
  bool   in_text;
  int    cur_font;           // index in font_list, -1 if none
  double cur_size, text_x, text_y;

  array<pdf_font_item> font_list;
  hashmap<string,int>  font_index;
  hashmap<int,int>     alpha_gs;   // alpha -> the number of its ExtGState
  int                  n_alpha, n_xobj;

  array<pdf_outline_item> outlines;
  array<pdf_link_item>    links;
  hashmap<string,string>  metadata;

  double to_x (SI x) { x += ox; return (x>=0 ? x : x-pixel+1) / (double) pixel; }
  double to_y (SI y) { y += oy; return (y>=0 ? y : y-pixel+1) / (double) pixel; }
  double to_w (SI w) { return ((double) w) / pixel; }

  void begin_page ();
  void end_page ();
  void put (const char* s) { fz_append_string (ctx, contents, s); }
  void end_text ();
  void select_fill (color c);
  void select_stroke (color c);
  void select_width (SI w);
  void select_alpha (int a);
  int  get_font (font_glyphs fn);
  void write_fonts ();
  void write_outline ();
  void write_links (pdf_obj* pobj);
  void write_metadata ();
  void draw_bitmap_glyph (int ch, font_glyphs fn, SI x, SI y);

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

  void clear_device (SI x1, SI y1, SI x2, SI y2) {
    (void) x1; (void) y1; (void) x2; (void) y2; }
  void draw (int char_code, font_glyphs fn, SI x, SI y);
  void line (SI x1, SI y1, SI x2, SI y2);
  void lines (array<SI> x, array<SI> y);
  void clear (SI x1, SI y1, SI x2, SI y2);
  void fill (SI x1, SI y1, SI x2, SI y2);
  void arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void polygon (array<SI> x, array<SI> y, bool convex= true);
  void draw_picture (picture p, SI x, SI y, int alpha);

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
* Construction
******************************************************************************/

mupdf_pdf_renderer_rep::mupdf_pdf_renderer_rep (
  url pdf_file_name2, int dpi2, int nr_pages2,
  string page_type2, bool landscape2, double paper_w2, double paper_h2)
  : renderer_rep (false),
    pdf_file_name (pdf_file_name2), dpi (dpi2), nr_pages (nr_pages2),
    page_type (page_type2), landscape (landscape2),
    paper_w (paper_w2), paper_h (paper_h2),
    started (false), page_num (0),
    ctx (mupdf_context ()), doc (NULL), contents (NULL),
    resources (NULL), res_font (NULL), res_xobj (NULL), res_gs (NULL),
    pen (black), bgb (white), fgb (black), clip_level (0),
    cur_fill (0), cur_stroke (0), has_fill (false), has_stroke (false),
    cur_width (-1), cur_alpha (255),
    in_text (false), cur_font (-1), cur_size (0), text_x (0), text_y (0),
    font_index (-1), alpha_gs (-1), n_alpha (0), n_xobj (0)
{
  width = default_dpi * paper_w / 2.54;
  height= default_dpi * paper_h / 2.54;
  fz_try (ctx) {
    doc= pdf_create_document (ctx);
    // one resource dictionary for the whole document: the pages share
    // their fonts, and a PDF may well have them point at the same object
    resources= pdf_add_new_dict (ctx, doc, 4);
    res_font= pdf_dict_put_dict (ctx, resources, PDF_NAME(Font), 8);
    res_xobj= pdf_dict_put_dict (ctx, resources, PDF_NAME(XObject), 8);
    res_gs=   pdf_dict_put_dict (ctx, resources, PDF_NAME(ExtGState), 4);
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
    write_fonts ();
    write_outline ();
    write_metadata ();
    // MuPDF subsets TrueType and CFF; the Type 1 fonts are left whole
    pdf_subset_fonts (ctx, doc, 0, NULL);
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF could not finish the document: "
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
  for (int i=0; i<N(font_list); i++) {
    if (font_list[i].font != NULL) fz_drop_font (ctx, font_list[i].font);
    pdf_drop_obj (ctx, font_list[i].obj);
  }
  for (int i=0; i<N(pages); i++) pdf_drop_obj (ctx, pages[i]);
  pdf_drop_obj (ctx, resources);
  pdf_drop_document (ctx, doc);
}

bool mupdf_pdf_renderer_rep::is_printer () { return true; }
bool mupdf_pdf_renderer_rep::is_started () { return started; }

/******************************************************************************
* Pages
******************************************************************************/

void
mupdf_pdf_renderer_rep::begin_page () {
  fz_try (ctx) { contents= fz_new_buffer (ctx, 8192); }
  fz_catch (ctx) { contents= NULL; return; }
  pen= pencil (black); bgb= brush (white); fgb= brush (black);
  clip_level= 0;
  has_fill= has_stroke= false; cur_width= -1; cur_alpha= 255;
  in_text= false; cur_font= -1; cur_size= 0;
  links= array<pdf_link_item> ();
  // the whole page is written in the pixels of the renderer
  fz_append_printf (ctx, contents, "q\n%g 0 0 %g 0 0 cm\n",
                    (double) default_dpi / dpi, (double) default_dpi / dpi);
  set_origin (0, (SI) (paper_h * dpi * pixel / 2.54));
  set_clipping (0, (SI) ((-dpi * pixel * paper_h) / 2.54),
                (SI) (( dpi * pixel * paper_w) / 2.54), 0);
}

void
mupdf_pdf_renderer_rep::end_page () {
  if (contents == NULL) return;
  end_text ();
  while (clip_level-- > 0) put ("Q\n");
  clip_level= 0;
  put ("Q\n");
  fz_try (ctx) {
    fz_rect mediabox= fz_make_rect (0, 0, (float) width, (float) height);
    pdf_obj* page= pdf_add_page (ctx, doc, mediabox, 0, resources, contents);
    write_links (page);
    pdf_insert_page (ctx, doc, -1, page);
    pages << page;
  }
  fz_catch (ctx) {
    convert_error << "MuPDF cannot close a page: "
                  << fz_caught_message (ctx) << LF;
  }
  fz_drop_buffer (ctx, contents); contents= NULL;
  page_num++;
}

void
mupdf_pdf_renderer_rep::next_page () { end_page (); begin_page (); }

/******************************************************************************
* The graphics state
******************************************************************************/

pencil mupdf_pdf_renderer_rep::get_pencil () { return pen; }
brush  mupdf_pdf_renderer_rep::get_background () { return bgb; }

void
mupdf_pdf_renderer_rep::set_pencil (pencil p) {
  pen= p; fgb= brush (pen->get_color ());
}

void
mupdf_pdf_renderer_rep::set_brush (brush b) {
  fgb= b; pen= pencil (b->get_color (), pen->get_width ());
}

void
mupdf_pdf_renderer_rep::set_background (brush b) { bgb= b; }

void
mupdf_pdf_renderer_rep::select_fill (color c) {
  if (has_fill && c == cur_fill) return;
  cur_fill= c; has_fill= true;
  int r, g, b, a; get_rgb_color (c, r, g, b, a);
  select_alpha (a);
  fz_append_printf (ctx, contents, "%g %g %g rg\n",
                    r/255.0, g/255.0, b/255.0);
}

void
mupdf_pdf_renderer_rep::select_stroke (color c) {
  if (has_stroke && c == cur_stroke) return;
  cur_stroke= c; has_stroke= true;
  int r, g, b, a; get_rgb_color (c, r, g, b, a);
  select_alpha (a);
  fz_append_printf (ctx, contents, "%g %g %g RG\n",
                    r/255.0, g/255.0, b/255.0);
}

void
mupdf_pdf_renderer_rep::select_width (SI w) {
  double lw= to_w (w);
  if (lw == cur_width) return;
  cur_width= lw;
  fz_append_printf (ctx, contents, "%g w\n", lw);
}

// the transparency of a colour is a graphics state of its own in a PDF
void
mupdf_pdf_renderer_rep::select_alpha (int a) {
  if (a == cur_alpha) return;
  cur_alpha= a;
  int num;
  if (alpha_gs->contains (a)) num= alpha_gs (a);
  else {
    num= n_alpha++;
    pdf_obj* gs= pdf_new_dict (ctx, doc, 3);
    pdf_dict_put (ctx, gs, PDF_NAME(Type), PDF_NAME(ExtGState));
    pdf_dict_put_real (ctx, gs, PDF_NAME(ca), a / 255.0);
    pdf_dict_put_real (ctx, gs, PDF_NAME(CA), a / 255.0);
    string nm= "GS" * as_string (num);
    c_string cnm (nm);
    pdf_dict_puts_drop (ctx, res_gs, cnm, pdf_add_object_drop (ctx, doc, gs));
    alpha_gs (a)= num;
  }
  fz_append_printf (ctx, contents, "/GS%d gs\n", num);
}

void
mupdf_pdf_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  if (contents == NULL) return;
  end_text ();
  renderer_rep::set_clipping (x1, y1, x2, y2, restore);
  if (restore) {
    if (clip_level > 0) {
      put ("Q\n"); clip_level--;
      // the state which comes back is not the one we were tracking
      has_fill= has_stroke= false; cur_width= -1; cur_alpha= 255;
    }
  }
  else {
    fz_append_printf (ctx, contents, "q\n%g %g %g %g re W n\n",
                      to_x (x1), to_y (y1),
                      to_x (x2) - to_x (x1), to_y (y2) - to_y (y1));
    clip_level++;
  }
}

/******************************************************************************
* Paths
******************************************************************************/

void
mupdf_pdf_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  if (contents == NULL) return;
  end_text ();
  select_stroke (pen->get_color ());
  select_width (pen->get_width ());
  fz_append_printf (ctx, contents, "%g %g m %g %g l S\n",
                    to_x (x1), to_y (y1), to_x (x2), to_y (y2));
}

void
mupdf_pdf_renderer_rep::lines (array<SI> x, array<SI> y) {
  if (contents == NULL || N(x) == 0 || N(x) != N(y)) return;
  end_text ();
  select_stroke (pen->get_color ());
  select_width (pen->get_width ());
  fz_append_printf (ctx, contents, "%g %g m\n", to_x (x[0]), to_y (y[0]));
  for (int i=1; i<N(x); i++)
    fz_append_printf (ctx, contents, "%g %g l\n", to_x (x[i]), to_y (y[i]));
  put ("S\n");
}

void
mupdf_pdf_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  if (contents == NULL || x1 >= x2 || y1 >= y2) return;
  end_text ();
  select_fill (pen->get_color ());
  fz_append_printf (ctx, contents, "%g %g %g %g re f\n",
                    to_x (x1), to_y (y1),
                    to_x (x2) - to_x (x1), to_y (y2) - to_y (y1));
}

void
mupdf_pdf_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  if (contents == NULL || x1 >= x2 || y1 >= y2) return;
  end_text ();
  select_fill (bgb->get_color ());
  fz_append_printf (ctx, contents, "%g %g %g %g re f\n",
                    to_x (x1), to_y (y1),
                    to_x (x2) - to_x (x1), to_y (y2) - to_y (y1));
}

void
mupdf_pdf_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  (void) convex;
  if (contents == NULL || N(x) < 2 || N(x) != N(y)) return;
  end_text ();
  select_fill (pen->get_color ());
  fz_append_printf (ctx, contents, "%g %g m\n", to_x (x[0]), to_y (y[0]));
  for (int i=1; i<N(x); i++)
    fz_append_printf (ctx, contents, "%g %g l\n", to_x (x[i]), to_y (y[i]));
  put ("h f\n");
}

// an arc of the ellipse inscribed in the box, in 1/64 degrees as in X11
static void
arc_path (fz_context* ctx, fz_buffer* buf, double cx, double cy,
          double rx, double ry, double a0, double a1, bool move) {
  const int n= 24;
  for (int i=0; i<=n; i++) {
    double a= a0 + (a1 - a0) * i / n;
    double px= cx + rx * cos (a), py= cy + ry * sin (a);
    fz_append_printf (ctx, buf, "%g %g %s\n", px, py,
                      (i == 0 && move) ? "m" : "l");
  }
}

void
mupdf_pdf_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if (contents == NULL) return;
  end_text ();
  select_stroke (pen->get_color ());
  select_width (pen->get_width ());
  double cx= (to_x (x1) + to_x (x2)) / 2, cy= (to_y (y1) + to_y (y2)) / 2;
  double rx= (to_x (x2) - to_x (x1)) / 2, ry= (to_y (y2) - to_y (y1)) / 2;
  arc_path (ctx, contents, cx, cy, rx, ry,
            alpha * M_PI / (64*180), (alpha+delta) * M_PI / (64*180), true);
  put ("S\n");
}

void
mupdf_pdf_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  if (contents == NULL) return;
  end_text ();
  select_fill (pen->get_color ());
  double cx= (to_x (x1) + to_x (x2)) / 2, cy= (to_y (y1) + to_y (y2)) / 2;
  double rx= (to_x (x2) - to_x (x1)) / 2, ry= (to_y (y2) - to_y (y1)) / 2;
  fz_append_printf (ctx, contents, "%g %g m\n", cx, cy);
  arc_path (ctx, contents, cx, cy, rx, ry,
            alpha * M_PI / (64*180), (alpha+delta) * M_PI / (64*180), false);
  put ("h f\n");
}

/******************************************************************************
* Fonts and text
******************************************************************************/

// is this font file a Type 1 program? (the TeX fonts all are)
static bool
is_type1_file (url u) {
  string s= locase_all (suffix (u));
  return s == "pfb" || s == "pfa";
}

int
mupdf_pdf_renderer_rep::get_font (font_glyphs fn) {
  string name= fn->res_name;
  if (font_index->contains (name)) return font_index (name);
  int pos= search_forwards (":", name);
  string fname= (pos == -1 ? name : name (0, pos));
  url u= tt_font_find (fname);
  pdf_font_item it;
  it.num= N(font_list);
  if (!is_none (u)) {
    c_string path (concretize (u));
    fz_try (ctx) { it.font= fz_new_font_from_file (ctx, NULL, path, 0, 0); }
    fz_catch (ctx) { it.font= NULL; }
    if (it.font != NULL && !pdf_font_writing_supported (ctx, it.font)) {
      fz_drop_font (ctx, it.font); it.font= NULL;
    }
  }
  if (it.font != NULL) {
    it.simple= is_type1_file (u);
    fz_try (ctx) {
      if (it.simple) {
        // pdf_add_simple_font embeds the program and writes a descriptor;
        // the encoding and the widths it puts there are for a text font
        // read as WinAnsi, which a TeX font is not, so they are replaced
        // by write_fonts once the codes in use are known
        it.obj= pdf_add_simple_font (ctx, doc, it.font,
                                     PDF_SIMPLE_ENCODING_LATIN);
        for (int i=0; i<256; i++) it.gid << -1;
      }
      else it.obj= pdf_add_cid_font (ctx, doc, it.font);
      string nm= "F" * as_string (it.num);
      c_string cnm (nm);
      pdf_dict_puts (ctx, res_font, cnm, it.obj);
    }
    fz_catch (ctx) {
      convert_warning << "mupdf_pdf_renderer: " << fname
                      << " cannot be embedded: " << fz_caught_message (ctx) << LF;
      fz_drop_font (ctx, it.font); it.font= NULL; it.obj= NULL;
    }
  }
  if (it.font == NULL)
    convert_warning << "mupdf_pdf_renderer: " << fname
                    << " is drawn as bitmaps" << LF;
  font_list << it;
  font_index (name)= it.num;
  return it.num;
}

void
mupdf_pdf_renderer_rep::end_text () {
  if (!in_text) return;
  put ("ET\n");
  in_text= false;
  cur_font= -1;
}

// the design size of a TeXmacs font, in the pixels of the renderer, as in
// pdf_hummus_renderer: "ecrm10.600" is the 10 point design at 600 dpi
static double
tm_font_size (string name) {
  int pos= search_backwards (".", name);
  if (pos <= 0) return 10;
  int sz= pos - 1;
  while (sz > 0 && is_numeric (name[sz-1])) sz--;
  double size= as_double (name (sz, pos));
  if (size == 0) size= 10;
  int end= pos + 1;
  while (end < N(name) && is_numeric (name[end])) end++;
  double d= as_double (name (pos+1, end));
  if (d == 0) d= 72;
  return size * d / 72.0;
}

void
mupdf_pdf_renderer_rep::draw (int ch, font_glyphs fn, SI x, SI y) {
  if (contents == NULL) return;
  glyph gl= fn->get (ch);
  if (is_nil (gl)) return;
  int k= get_font (fn);
  if (font_list[k].font == NULL) { draw_bitmap_glyph (ch, fn, x, y); return; }
  select_fill (pen->get_color ());
  double size= tm_font_size (fn->res_name);
  if (!in_text) {
    put ("BT\n"); in_text= true; cur_font= -1; text_x= text_y= 0;
  }
  if (cur_font != k || cur_size != size) {
    fz_append_printf (ctx, contents, "/F%d %g Tf\n", font_list[k].num, size);
    cur_font= k; cur_size= size;
  }
  double px= to_x (x), py= to_y (y);
  fz_append_printf (ctx, contents, "%g %g Td\n", px - text_x, py - text_y);
  text_x= px; text_y= py;
  if (font_list[k].simple) {
    // one byte per glyph; the code is the one TeXmacs uses, and the
    // Differences array will say which glyph it selects
    int code= ch & 255;
    if (font_list[k].gid[code] < 0) font_list[k].gid[code]= (int) gl->index;
    fz_append_printf (ctx, contents, "<%02x> Tj\n", code);
  }
  else
    fz_append_printf (ctx, contents, "<%04x> Tj\n", ((int) gl->index) & 0xffff);
}

// The encoding and the widths of the simple fonts, once every page has
// been written and the codes in use are known.
void
mupdf_pdf_renderer_rep::write_fonts () {
  for (int k=0; k<N(font_list); k++) {
    pdf_font_item& it= font_list[k];
    if (it.font == NULL || !it.simple || it.obj == NULL) continue;
    FT_Face face= (FT_Face) fz_font_ft_face (ctx, it.font);
    int first= 256, last= -1;
    for (int c=0; c<256; c++)
      if (it.gid[c] >= 0) { if (c < first) first= c; last= c; }
    if (last < 0) continue;
    // /Encoding: the glyph names of the codes which are used
    pdf_obj* enc= pdf_new_dict (ctx, doc, 2);
    pdf_dict_put (ctx, enc, PDF_NAME(Type), PDF_NAME(Encoding));
    pdf_obj* diff= pdf_dict_put_array (ctx, enc, PDF_NAME(Differences), 16);
    int prev= -2;
    for (int c=first; c<=last; c++) {
      if (it.gid[c] < 0) continue;
      char nm[128];
      nm[0]= 0;
      if (face != NULL && FT_HAS_GLYPH_NAMES (face))
        if (FT_Get_Glyph_Name (face, it.gid[c], nm, sizeof (nm)) != 0) nm[0]= 0;
      if (nm[0] == 0) continue;   // no name: the built in encoding stands
      if (c != prev + 1) pdf_array_push_int (ctx, diff, c);
      pdf_array_push_name (ctx, diff, nm);
      prev= c;
    }
    pdf_dict_put_drop (ctx, it.obj, PDF_NAME(Encoding),
                       pdf_add_object_drop (ctx, doc, enc));
    // /Widths, in thousandths of the size
    pdf_dict_put_int (ctx, it.obj, PDF_NAME(FirstChar), first);
    pdf_dict_put_int (ctx, it.obj, PDF_NAME(LastChar), last);
    pdf_obj* w= pdf_dict_put_array (ctx, it.obj, PDF_NAME(Widths), last-first+1);
    for (int c=first; c<=last; c++) {
      double adv= 0;
      if (it.gid[c] >= 0)
        adv= fz_advance_glyph (ctx, it.font, it.gid[c], 0) * 1000.0;
      pdf_array_push_real (ctx, w, adv);
    }
  }
}

// A glyph of a font which cannot be embedded, drawn as an image. Hummus
// builds a Type 3 font instead, which is smaller and searchable.
void
mupdf_pdf_renderer_rep::draw_bitmap_glyph (int ch, font_glyphs fn, SI x, SI y) {
  glyph gl= fn->get (ch);
  if (is_nil (gl)) return;
  int w= gl->width, h= gl->height;
  if (w <= 0 || h <= 0) return;
  end_text ();
  int cr, cg, cb, ca;
  get_rgb_color (pen->get_color (), cr, cg, cb, ca);
  fz_pixmap* pix= NULL; fz_image* img= NULL;
  fz_try (ctx) {
    pix= fz_new_pixmap (ctx, fz_device_rgb (ctx), w, h, NULL, 1);
    unsigned char* p= fz_pixmap_samples (ctx, pix);
    int stride= fz_pixmap_stride (ctx, pix);
    for (int j=0; j<h; j++) {
      unsigned char* row= p + j * stride;
      for (int i=0; i<w; i++) {
        int on= gl->get_x (i, j);
        row[4*i+0]= (unsigned char) cr;
        row[4*i+1]= (unsigned char) cg;
        row[4*i+2]= (unsigned char) cb;
        row[4*i+3]= (unsigned char) ((on * ca) / 255);
      }
    }
    img= fz_new_image_from_pixmap (ctx, pix, NULL);
    pdf_obj* ref= pdf_add_image (ctx, doc, img);
    string nm= "Im" * as_string (n_xobj++);
    c_string cnm (nm);
    pdf_dict_puts_drop (ctx, res_xobj, cnm, ref);
    double x0= to_x (x) - gl->xoff, y0= to_y (y) - h + gl->yoff;
    fz_append_printf (ctx, contents, "q %g 0 0 %g %g %g cm /%s Do Q\n",
                      (double) w, (double) h, x0, y0, (const char*) cnm);
  }
  fz_always (ctx) { fz_drop_image (ctx, img); fz_drop_pixmap (ctx, pix); }
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
  if (contents == NULL) return;
  end_text ();
  picture p2= as_mupdf_picture (p);
  mupdf_picture_rep* rep= (mupdf_picture_rep*) p2->get_handle ();
  if (rep == NULL || rep->pix == NULL) return;
  select_alpha (alpha);
  fz_image* img= NULL;
  fz_try (ctx) {
    img= mupdf_image_from_pixmap (rep->pix);
    pdf_obj* ref= pdf_add_image (ctx, doc, img);
    string nm= "Im" * as_string (n_xobj++);
    c_string cnm (nm);
    pdf_dict_puts_drop (ctx, res_xobj, cnm, ref);
    double x0= to_x (x) - rep->ox, y0= to_y (y) - rep->oy;
    fz_append_printf (ctx, contents, "q %g 0 0 %g %g %g cm /%s Do Q\n",
                      (double) rep->w, (double) rep->h, x0, y0,
                      (const char*) cnm);
  }
  fz_always (ctx) { fz_drop_image (ctx, img); }
  fz_catch (ctx) {
    convert_warning << "MuPDF picture failed: "
                    << fz_caught_message (ctx) << LF;
  }
}

/******************************************************************************
* Links, the outline and the metadata
******************************************************************************/

void
mupdf_pdf_renderer_rep::anchor (string label, SI x1, SI y1, SI x2, SI y2) {
  (void) label; (void) x1; (void) y1; (void) x2; (void) y2;
  // the destination tree is not written yet, see the notes
}

void
mupdf_pdf_renderer_rep::href (string label, SI x1, SI y1, SI x2, SI y2) {
  double f= (double) default_dpi / dpi;
  links << pdf_link_item (label,
                          f * to_x (x1 - 5*pixel), f * to_y (y1 - 10*pixel),
                          f * to_x (x2 + 5*pixel), f * to_y (y2 + 10*pixel));
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
  double f= (double) default_dpi / dpi;
  outlines << pdf_outline_item (title, page_num, f * to_x (x),
                                f * to_y (y + 20*pixel), ls);
}

void
mupdf_pdf_renderer_rep::set_metadata (string kind, string val) {
  metadata (kind)= val;
}

void
mupdf_pdf_renderer_rep::write_links (pdf_obj* pobj) {
  if (N(links) == 0) return;
  pdf_obj* annots= pdf_dict_put_array (ctx, pobj, PDF_NAME(Annots), N(links));
  for (int i=0; i<N(links); i++) {
    pdf_obj* a= pdf_new_dict (ctx, doc, 5);
    pdf_dict_put (ctx, a, PDF_NAME(Type), PDF_NAME(Annot));
    pdf_dict_put (ctx, a, PDF_NAME(Subtype), PDF_NAME(Link));
    pdf_obj* rect= pdf_dict_put_array (ctx, a, PDF_NAME(Rect), 4);
    pdf_array_push_real (ctx, rect, links[i].x1);
    pdf_array_push_real (ctx, rect, links[i].y1);
    pdf_array_push_real (ctx, rect, links[i].x2);
    pdf_array_push_real (ctx, rect, links[i].y2);
    pdf_obj* border= pdf_dict_put_array (ctx, a, PDF_NAME(Border), 3);
    pdf_array_push_int (ctx, border, 16);
    pdf_array_push_int (ctx, border, 16);
    pdf_array_push_int (ctx, border, 0);
    if (!starts (links[i].label, "#")) {
      pdf_obj* act= pdf_dict_put_dict (ctx, a, PDF_NAME(A), 2);
      pdf_dict_put (ctx, act, PDF_NAME(S), PDF_NAME(URI));
      c_string s (links[i].label);
      pdf_dict_put_text_string (ctx, act, PDF_NAME(URI), s);
    }
    pdf_array_push_drop (ctx, annots, pdf_add_object_drop (ctx, doc, a));
  }
}

void
mupdf_pdf_renderer_rep::write_outline () {
  if (N(outlines) == 0) return;
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
    prev= ref; count++;
  }
  if (first != NULL) {
    pdf_dict_put (ctx, out, PDF_NAME(First), first);
    pdf_dict_put (ctx, out, PDF_NAME(Last), prev);
    pdf_dict_put_int (ctx, out, PDF_NAME(Count), count);
  }
}

void
mupdf_pdf_renderer_rep::write_metadata () {
  pdf_obj* info= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Info));
  if (info == NULL) {
    info= pdf_add_new_dict (ctx, doc, 8);
    pdf_dict_put (ctx, pdf_trailer (ctx, doc), PDF_NAME(Info), info);
  }
  iterator<string> it= iterate (metadata);
  while (it->busy ()) {
    string key= it->next ();
    c_string val (metadata (key));
    if (key == "title") pdf_dict_put_text_string (ctx, info, PDF_NAME(Title), val);
    else if (key == "author") pdf_dict_put_text_string (ctx, info, PDF_NAME(Author), val);
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
