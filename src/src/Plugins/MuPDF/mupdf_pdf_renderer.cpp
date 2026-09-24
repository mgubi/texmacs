
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
#include "frame.hpp"
#include "image_files.hpp"
#include "scalable.hpp"

#include <mupdf/fitz.h>
#include <mupdf/pdf.h>
#include <ft2build.h>
#include FT_FREETYPE_H
extern "C" {
#include "mupdf_writet1.h"
}

extern url tt_font_find (string name); // font_select.cpp
bool is_percentage (tree t, string s= "%"); // env_length.cpp, declared by
double as_percentage (tree t);              // each user, as renderer.cpp does

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
  bool     t3;         // a Type 3 font: the glyphs are drawn as bitmaps
  font_glyphs fn;      // for a Type 3 font: where the bitmaps come from
  int      chunk;      // ... and which 256 characters of it this is
  string   path;       // the file the program was read from (for subsetting)
  array<int> gid;      // 256 entries: the glyph TeXmacs asked for, -1 if free
  // the letters a ligature glyph stands for, by the code (a simple font)
  // or the glyph (a CID font) it is written with; "" for a glyph which is
  // not a ligature, nothing when it has not been asked yet
  hashmap<int,string> lig;
  pdf_font_item ()
    : font (NULL), obj (NULL), num (0), simple (false), t3 (false), chunk (0),
      lig ("") {}
};

// The letters of a ligature, from the name of its glyph, or "" when the
// glyph is not one. The names are those of the Adobe glyph list: the five
// of the Latin ligatures as TeX fonts name them, their uniFB0x forms, and
// the f_f_i of OpenType fonts, whose parts are joined by underscores.
static string
ligature_letters (string name) {
  if (name == "ff" || name == "fi" || name == "fl" ||
      name == "ffi" || name == "ffl") return name;
  if (name == "uniFB00") return "ff";
  if (name == "uniFB01") return "fi";
  if (name == "uniFB02") return "fl";
  if (name == "uniFB03") return "ffi";
  if (name == "uniFB04") return "ffl";
  if (search_forwards ("_", name) < 0) return "";
  string r;
  int start= 0;
  for (int i=0; i<=N(name); i++)
    if (i == N(name) || name[i] == '_') {
      string part= name (start, i);
      // only parts which are single letters: "f_f_i", not "a_acute"
      if (N(part) != 1 || !is_alpha (part[0])) return "";
      r << part;
      start= i + 1;
    }
  return N(r) >= 2 ? r : string ("");
}

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
  pdf_obj*      res_pat;
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
  int    cur_alpha;         // -1 when we no longer know what it is
  void   forget_state () {  // after a Q: the state which comes back is
    has_fill= has_stroke= false;   // not the one we were tracking
    cur_width= -1; cur_alpha= -1;
  }
  bool   in_text;
  int    cur_font;           // index in font_list, -1 if none
  double cur_size, text_x, text_y;

  array<pdf_font_item> font_list;
  hashmap<string,int>  font_index;   // by the res_name of the TeXmacs font
  hashmap<string,int>  font_by_file; // by the file the program comes from
  hashmap<int,int>     alpha_gs;   // alpha -> the number of its ExtGState
  hashmap<string,int>  image_pool; // a file -> the number of its XObject
  // MuPDF gives the same object back for the same image, so this is what
  // keeps a picture which is drawn again -- every tile of a pattern, say
  // -- from being named again in the resources
  hashmap<pointer,int> xobj_num;
  hashmap<string,int>  pattern_pool;  // a tile and its lattice -> /P<n>
  int                  n_pat;
  int                  transform_level; // how deep in set_transformation
  int                  n_alpha, n_xobj;

  array<pdf_outline_item> outlines;
  array<pdf_link_item>    links;
  array<string>           dest_name;   // the anchors, in the order they came
  array<pdf_link_item>    dest_pos;    // the page in x1, the point in y1, x2
  hashmap<string,int>     dest_index;
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
  int  alpha_state (int a);
  int  get_font (font_glyphs fn, int ch);
  string ligature_of (pdf_font_item& it, int key, int fallback);
  string glyph_name (pdf_font_item& it, int key, int fallback, int& gid);
  void write_type3 (pdf_font_item& it);
  void write_fonts ();
  void subset_type1 (pdf_font_item& it, array<string> keep);
  void write_outline ();
  void write_dests ();
  void write_links (pdf_obj* pobj);
  void write_metadata ();
  void draw_bitmap_glyph (int ch, font_glyphs fn, SI x, SI y);
  int  embed_image (url u);
  void place_image (int num, double w, double h, SI x, SI y, int alpha);
  int  name_xobject (pdf_obj* ref);
  void xobject_fit (pdf_obj* xo, double w, double h, double x, double y,
                    double m[6]);
  int  tiling_pattern (int img, double w, double h, double ax, double ay);
  bool opaque_tile (int img);

public:
  mupdf_pdf_renderer_rep (url pdf_file_name, int dpi, int nr_pages,
                          string page_type, bool landscape,
                          double paper_w, double paper_h);
  ~mupdf_pdf_renderer_rep ();

  bool is_printer ();
  bool is_started ();
  void next_page ();

  void set_transformation (frame fr);
  void reset_transformation ();
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
  using renderer_rep::clear_pattern;
  void clear_pattern (SI mx1, SI my1, SI mx2, SI my2,
                      SI x1, SI y1, SI x2, SI y2);
  void fill (SI x1, SI y1, SI x2, SI y2);
  void arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void polygon (array<SI> x, array<SI> y, bool convex= true);
  void draw_picture (picture p, SI x, SI y, int alpha);
  void draw_scalable (scalable im, SI x, SI y, int alpha);

  renderer shadow (picture& pic, SI x1, SI y1, SI x2, SI y2);
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
    res_pat (NULL),
    pen (black), bgb (white), fgb (black), clip_level (0),
    cur_fill (0), cur_stroke (0), has_fill (false), has_stroke (false),
    cur_width (-1), cur_alpha (255),
    in_text (false), cur_font (-1), cur_size (0), text_x (0), text_y (0),
    font_index (-1), font_by_file (-1), dest_index (-1), alpha_gs (-1),
    image_pool (-1), xobj_num (-1), pattern_pool (-1), n_pat (0),
    transform_level (0),
    n_alpha (0), n_xobj (0)
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
    res_pat=  pdf_dict_put_dict (ctx, resources, PDF_NAME(Pattern), 2);
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
    write_dests ();
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
  has_fill= has_stroke= false; cur_width= -1;
  cur_alpha= 255;   // what a page starts with
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
  fz_append_printf (ctx, contents, "/GS%d gs\n", alpha_state (a));
}

// the ExtGState of an alpha, made the first time it is asked for
int
mupdf_pdf_renderer_rep::alpha_state (int a) {
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
  return num;
}

// A frame of the graphics: the stream gets the matrix and the clipping
// rectangle follows it, as in pdf_hummus_renderer. Without this the
// transformation was simply dropped and a rotated graphic came out
// straight (renderer_rep::set_transformation is a no-op).
void
mupdf_pdf_renderer_rep::set_transformation (frame fr) {
  ASSERT (fr->linear, "only linear transformations have been implemented");
  if (contents == NULL) return;
  end_text ();
  SI cx1, cy1, cx2, cy2;
  get_clipping (cx1, cy1, cx2, cy2);
  rectangle oclip (cx1, cy1, cx2, cy2);
  frame cv= scaling (point (pixel, pixel), point (-ox, -oy));
  frame tr= invert (cv) * fr * cv;
  point o = tr (point (0.0, 0.0));
  point ux= tr (point (1.0, 0.0)) - o;
  point uy= tr (point (0.0, 1.0)) - o;
  // a q saves the state and changes nothing, so what we track still holds
  put ("q\n");
  transform_level++;
  fz_append_printf (ctx, contents, "%g %g %g %g %g %g cm\n",
                    ux[0], ux[1], uy[0], uy[1], o[0], o[1]);
  rectangle nclip= fr [oclip];
  renderer_rep::clip (nclip->x1, nclip->y1, nclip->x2, nclip->y2);
}

void
mupdf_pdf_renderer_rep::reset_transformation () {
  if (contents == NULL) return;
  end_text ();
  renderer_rep::unclip ();
  put ("Q\n");
  if (transform_level > 0) transform_level--;
  forget_state ();
}

void
mupdf_pdf_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  if (contents == NULL) return;
  end_text ();
  renderer_rep::set_clipping (x1, y1, x2, y2, restore);
  if (restore) {
    if (clip_level > 0) {
      put ("Q\n"); clip_level--;
      forget_state ();
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
mupdf_pdf_renderer_rep::get_font (font_glyphs fn, int ch) {
  string name= fn->res_name;
  if (font_index->contains (name)) {
    int k= font_index (name);
    // a font drawn as bitmaps holds 256 characters at a time, so a
    // character outside the chunk of the entry we found asks for another
    if (!font_list[k].t3 || font_list[k].chunk == ch / 256) return k;
    name= name * "-c" * as_string (ch / 256);
    if (font_index->contains (name)) return font_index (name);
  }
  int pos= search_forwards (":", name);
  string fname= (pos == -1 ? name : name (0, pos));
  url u= tt_font_find (fname);
  // Several TeXmacs fonts share a file -- the sizes of a TeX font are one
  // program each, and its chunks are the same program again -- and MuPDF
  // hands the same PDF object back for it. They must therefore share one
  // entry here as well, or the encoding one of them writes would undo the
  // encoding of the next.
  string key= is_none (u) ? string ("") : string (concretize (u));
  if (N(key) > 0 && font_by_file->contains (key)) {
    int k= font_by_file (key);
    font_index (name)= k;
    return k;
  }
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
    it.path= concretize (u);
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
  if (it.font == NULL) {
    // no font program MuPDF can embed: the glyphs go in as the bitmaps
    // TeXmacs has, in a Type 3 font, which is what a PK font is anyway
    it.t3= true;
    it.fn= fn;
    it.chunk= ch / 256;
    for (int i=0; i<256; i++) it.gid << -1;
    fz_try (ctx) {
      it.obj= pdf_add_new_dict (ctx, doc, 8);
      string nm= "F" * as_string (it.num);
      c_string cnm (nm);
      pdf_dict_puts (ctx, res_font, cnm, it.obj);
    }
    fz_catch (ctx) { it.obj= NULL; }
  }
  font_list << it;
  font_index (name)= it.num;
  if (N(key) > 0) font_by_file (key)= it.num;
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
  int k= get_font (fn, ch);
  if (font_list[k].t3 && font_list[k].obj == NULL) {
    draw_bitmap_glyph (ch, fn, x, y); return;
  }
  select_fill (pen->get_color ());
  // A Type 3 font is set at 100: its FontMatrix is a hundredth, so a unit
  // of its glyph space is a pixel of the renderer, which is what the
  // bitmaps of TeXmacs are drawn in. pdf_hummus_renderer does the same.
  double size= font_list[k].t3 ? 100.0 : tm_font_size (fn->res_name);
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
  // A ligature says which letters it stands for. A reader which takes the
  // text from the glyph names gets ﬁ (U+FB01) out of "fi", so a search for
  // "first" misses it and a copy yields the ligature: Ghostscript does
  // exactly that, while MuPDF happens to decompose it on its own.
  string lig;
  if (!font_list[k].t3) {
    int key= font_list[k].simple ? (ch & 255) : (int) gl->index;
    lig= ligature_of (font_list[k], key, (int) gl->index);
  }
  if (N(lig) > 0) {
    c_string cl (lig);
    fz_append_printf (ctx, contents, "/Span << /ActualText (%s) >> BDC\n",
                      (const char*) cl);
  }
  if (font_list[k].simple || font_list[k].t3) {
    // one byte per glyph; the code is the one TeXmacs uses, and the
    // Differences array will say which glyph it selects
    int code= ch & 255;
    if (font_list[k].gid[code] < 0)
      font_list[k].gid[code]= font_list[k].t3 ? ch : (int) gl->index;
    fz_append_printf (ctx, contents, "<%02x> Tj\n", code);
  }
  else
    fz_append_printf (ctx, contents, "<%04x> Tj\n", ((int) gl->index) & 0xffff);
  if (N(lig) > 0) put ("EMC\n");
}

// The name of the glyph a font draws for key -- the code in a simple
// font, looked up through its built in ("Adobe custom") encoding, the
// glyph itself otherwise; fallback is the glyph to take when the encoding
// has nothing for the code. The glyph goes in gid, and "" is returned when
// there is no name.
//
// The index TeXmacs carries is its own and does not have to be FreeType's:
// in the EC fonts it is the code, while FreeType numbers the glyphs from
// the CharStrings, one less. Hence the encoding.
//
// FreeType is only ever reached with MuPDF's lock held (fz_ft_lock, and
// fz_get_glyph_name, which takes it). The lock is also what points
// FreeType's allocator at the calling context, and an OpenType face
// allocates the first time a name is asked of it -- its table of names is
// loaded lazily. Asked directly, that allocation crashed the renderer.
string
mupdf_pdf_renderer_rep::glyph_name (pdf_font_item& it, int key, int fallback,
                                    int& gid) {
  gid= -1;
  if (it.font == NULL) return "";
  FT_Face face= (FT_Face) fz_font_ft_face (ctx, it.font);
  if (face == NULL) return "";
  int g= key, n= 0;
  bool names= false;
  fz_ft_lock (ctx);
  names= FT_HAS_GLYPH_NAMES (face);
  n= (int) face->num_glyphs;
  if (it.simple) {
    if (FT_Select_Charmap (face, FT_ENCODING_ADOBE_CUSTOM) != 0)
      FT_Select_Charmap (face, FT_ENCODING_ADOBE_STANDARD);
    g= (int) FT_Get_Char_Index (face, key);
    if (g == 0) g= fallback;
  }
  fz_ft_unlock (ctx);
  if (!names || g <= 0 || g >= n) return "";
  char nm[128];
  nm[0]= 0;
  fz_try (ctx) { fz_get_glyph_name (ctx, it.font, g, nm, sizeof (nm)); }
  fz_catch (ctx) { nm[0]= 0; }
  if (nm[0] == 0) return "";
  gid= g;
  return string (nm);
}

// The letters of the glyph a font draws for key, when it is a ligature;
// fallback as in glyph_name, the same glyph write_fonts will name.
string
mupdf_pdf_renderer_rep::ligature_of (pdf_font_item& it, int key,
                                     int fallback) {
  if (it.lig->contains (key)) return it.lig [key];
  int g;
  string r= ligature_letters (glyph_name (it, key, fallback, g));
  it.lig (key)= r;
  return r;
}

// The encoding and the widths of the simple fonts, once every page has
// been written and the codes in use are known.
void
mupdf_pdf_renderer_rep::write_fonts () {
  for (int k=0; k<N(font_list); k++) {
    pdf_font_item& it= font_list[k];
    if (it.t3) { write_type3 (it); continue; }
    if (it.font == NULL || !it.simple || it.obj == NULL) continue;
    // the names of the glyphs the codes select are asked of the font
    // itself, see glyph_name
    int first= 256, last= -1;
    for (int c=0; c<256; c++)
      if (it.gid[c] >= 0) { if (c < first) first= c; last= c; }
    if (last < 0) continue;
    // /Encoding: the glyph names of the codes which are used
    pdf_obj* enc= pdf_new_dict (ctx, doc, 2);
    pdf_dict_put (ctx, enc, PDF_NAME(Type), PDF_NAME(Encoding));
    pdf_obj* diff= pdf_dict_put_array (ctx, enc, PDF_NAME(Differences), 16);
    int prev= -2;
    array<int> ftgid;    // what FreeType calls the glyph of each code
    array<string> gname;
    for (int c=0; c<256; c++) { ftgid << -1; gname << string (); }
    for (int c=first; c<=last; c++) {
      if (it.gid[c] < 0) continue;
      // with no built in encoding for the code, trust the index of TeXmacs
      int g;
      string nm= glyph_name (it, c, it.gid[c], g);
      if (N(nm) == 0) continue;   // no name: the built in encoding stands
      ftgid[c]= g; gname[c]= nm;
      if (c != prev + 1) pdf_array_push_int (ctx, diff, c);
      c_string cnm (nm);
      pdf_array_push_name (ctx, diff, cnm);
      prev= c;
    }
    pdf_dict_put_drop (ctx, it.obj, PDF_NAME(Encoding),
                       pdf_add_object_drop (ctx, doc, enc));
    // The font program, cut down to the glyphs which are used. MuPDF
    // subsets TrueType and CFF only, so a Type 1 would go in whole -- some
    // eighty kilobytes for a handful of letters; mupdf_t1_subset is
    // pdfTeX's writet1.c, which knows how to do it.
    array<string> keep;
    for (int c=first; c<=last; c++)
      if (N(gname[c]) > 0) keep << gname[c];
    if (N(keep) > 0) subset_type1 (it, keep);
    // /Widths, in thousandths of the size
    pdf_dict_put_int (ctx, it.obj, PDF_NAME(FirstChar), first);
    pdf_dict_put_int (ctx, it.obj, PDF_NAME(LastChar), last);
    pdf_obj* w= pdf_dict_put_array (ctx, it.obj, PDF_NAME(Widths), last-first+1);
    for (int c=first; c<=last; c++) {
      double adv= 0;
      if (ftgid[c] >= 0)
        adv= fz_advance_glyph (ctx, it.font, ftgid[c], 0) * 1000.0;
      pdf_array_push_real (ctx, w, adv);
    }
  }
}

// Replace the font program of a simple font by a subset of itself which
// has only the glyphs in `keep`, and rename the font as a subset is named.
void
mupdf_pdf_renderer_rep::subset_type1 (pdf_font_item& it, array<string> keep) {
  pdf_obj* fdesc= pdf_dict_get (ctx, it.obj, PDF_NAME(FontDescriptor));
  if (fdesc == NULL) return;
  pdf_obj* ff= pdf_dict_get (ctx, fdesc, PDF_NAME(FontFile));
  if (ff == NULL) return;   // not a Type 1 after all
  c_string path (it.path);
  const char** names= (const char**) fz_malloc (ctx, N(keep) * sizeof (char*));
  c_string** cs= (c_string**) fz_malloc (ctx, N(keep) * sizeof (c_string*));
  for (int i=0; i<N(keep); i++) {
    cs[i]= new c_string (keep[i]);
    names[i]= (const char*) *(cs[i]);
  }
  int size= 0, l1= 0, l2= 0, l3= 0;
  char* psname= NULL;
  const char* err= NULL;
  unsigned char* sub= mupdf_t1_subset (path, names, N(keep), &size,
                                       &l1, &l2, &l3, &psname, &err);
  for (int i=0; i<N(keep); i++) delete cs[i];
  fz_free (ctx, cs); fz_free (ctx, names);
  if (sub == NULL) {
    convert_warning << "the Type 1 font " << it.path
                    << " could not be subsetted: "
                    << string (err == NULL ? "?" : err) << LF;
    return;
  }
  fz_try (ctx) {
    fz_buffer* buf= fz_new_buffer_from_copied_data (ctx, sub, (size_t) size);
    pdf_update_stream (ctx, doc, ff, buf, 0);
    fz_drop_buffer (ctx, buf);
    pdf_dict_put_int (ctx, ff, PDF_NAME(Length1), l1);
    pdf_dict_put_int (ctx, ff, PDF_NAME(Length2), l2);
    pdf_dict_put_int (ctx, ff, PDF_NAME(Length3), l3);
    if (psname != NULL) {
      // the tag says the font is a subset, and the two names must agree
      // with the /FontName the program itself now carries
      pdf_dict_put_name (ctx, it.obj, PDF_NAME(BaseFont), psname);
      pdf_dict_put_name (ctx, fdesc, PDF_NAME(FontName), psname);
    }
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF could not store the subsetted font: "
                    << fz_caught_message (ctx) << LF;
  }
  free (sub);
  if (psname != NULL) free (psname);
}

// A Type 3 font: the glyphs are little content streams which draw the
// bitmap TeXmacs has, and the font is set at 100 because its FontMatrix is
// a hundredth (see draw). This is what a PK font is, and what
// pdf_hummus_renderer makes of one; the alternative, an image XObject per
// glyph, is both larger and not text any more.
void
mupdf_pdf_renderer_rep::write_type3 (pdf_font_item& it) {
  if (it.obj == NULL || is_nil (it.fn)) return;
  int first= 256, last= -1;
  for (int c=0; c<256; c++)
    if (it.gid[c] >= 0) { if (c < first) first= c; last= c; }
  if (last < 0) return;
  int b0= 0, b1= 0, b2= 0, b3= 0;   // the box of the whole font
  fz_try (ctx) {
    pdf_obj* procs= pdf_new_dict (ctx, doc, last - first + 1);
    pdf_obj* enc= pdf_new_dict (ctx, doc, 2);
    pdf_dict_put (ctx, enc, PDF_NAME(Type), PDF_NAME(Encoding));
    pdf_obj* diff= pdf_dict_put_array (ctx, enc, PDF_NAME(Differences), 16);
    int prev= -2;
    for (int c=first; c<=last; c++) {
      if (it.gid[c] < 0) continue;
      glyph gl= it.fn->get (it.gid[c]);
      if (is_nil (gl)) continue;
      int llx= -gl->xoff, lly= gl->yoff - gl->height + 1;
      int urx= gl->width - gl->xoff + 1, ury= gl->yoff + 1;
      int w= gl->width, h= gl->height;
      if (b2 <= b0) { b0= llx; b1= lly; b2= urx; b3= ury; }
      else {
        if (llx < b0) b0= llx; if (lly < b1) b1= lly;
        if (urx > b2) b2= urx; if (ury > b3) b3= ury;
      }
      fz_buffer* cs= fz_new_buffer (ctx, 256 + (size_t) w * h / 4);
      // d1 says the glyph is a mask, so the colour is the one in force
      fz_append_printf (ctx, cs, "%d 0 %d %d %d %d d1\n",
                        (int) gl->lwidth, llx, lly, urx, ury);
      if (w > 0 && h > 0) {
        fz_append_printf (ctx, cs, "q\n%d 0 0 %d %d %d cm\n", w, h, llx, lly);
        fz_append_printf (ctx, cs, "BI\n/W %d\n/H %d\n", w, h);
        fz_append_string (ctx, cs, "/BPC 1 /F /AHx /D [0.0 1.0] /IM true\nID\n");
        static const char* hex= "0123456789ABCDEF";
        int cur= 0, count= 0;
        for (int j=0; j<h; j++)
          for (int i=0; i < ((w + 7) & (-8)); i++) {
            cur= cur << 1;
            if (i < w && gl->get_x (i, j) == 0) cur++;
            count++;
            if (count == 4) {
              char d[2]; d[0]= hex[cur]; d[1]= 0;
              fz_append_string (ctx, cs, d);
              cur= 0; count= 0;
            }
          }
        fz_append_string (ctx, cs, ">\nEI\nQ\n");
      }
      string nm= "ch" * as_string (c);
      c_string cnm (nm);
      pdf_obj* ref= pdf_add_stream (ctx, doc, cs, NULL, 0);
      fz_drop_buffer (ctx, cs);
      pdf_dict_puts_drop (ctx, procs, cnm, ref);
      if (c != prev + 1) pdf_array_push_int (ctx, diff, c);
      pdf_array_push_name (ctx, diff, cnm);
      prev= c;
    }
    pdf_dict_put (ctx, it.obj, PDF_NAME(Type), PDF_NAME(Font));
    pdf_dict_put (ctx, it.obj, PDF_NAME(Subtype), PDF_NAME(Type3));
    pdf_obj* box= pdf_dict_put_array (ctx, it.obj, PDF_NAME(FontBBox), 4);
    pdf_array_push_int (ctx, box, b0); pdf_array_push_int (ctx, box, b1);
    pdf_array_push_int (ctx, box, b2); pdf_array_push_int (ctx, box, b3);
    pdf_obj* mat= pdf_dict_put_array (ctx, it.obj, PDF_NAME(FontMatrix), 6);
    pdf_array_push_real (ctx, mat, 0.01); pdf_array_push_int (ctx, mat, 0);
    pdf_array_push_int (ctx, mat, 0); pdf_array_push_real (ctx, mat, 0.01);
    pdf_array_push_int (ctx, mat, 0); pdf_array_push_int (ctx, mat, 0);
    pdf_dict_put_int (ctx, it.obj, PDF_NAME(FirstChar), first);
    pdf_dict_put_int (ctx, it.obj, PDF_NAME(LastChar), last);
    pdf_obj* w= pdf_dict_put_array (ctx, it.obj, PDF_NAME(Widths), last-first+1);
    for (int c=first; c<=last; c++) {
      double adv= 0;
      if (it.gid[c] >= 0) {
        glyph gl= it.fn->get (it.gid[c]);
        if (!is_nil (gl)) adv= gl->lwidth;
      }
      pdf_array_push_real (ctx, w, adv);
    }
    pdf_dict_put_drop (ctx, it.obj, PDF_NAME(CharProcs),
                       pdf_add_object_drop (ctx, doc, procs));
    pdf_dict_put_drop (ctx, it.obj, PDF_NAME(Encoding),
                       pdf_add_object_drop (ctx, doc, enc));
    pdf_obj* res= pdf_dict_put_dict (ctx, it.obj, PDF_NAME(Resources), 1);
    pdf_obj* ps= pdf_dict_put_array (ctx, res, PDF_NAME(ProcSet), 2);
    pdf_array_push (ctx, ps, PDF_NAME(PDF));
    pdf_array_push (ctx, ps, PDF_NAME(ImageB));
    // the codes of a Type 3 font mean nothing to a reader: a CMap says
    // which character each of them stands for, so the text can be found
    string cmap;
    cmap << "/CIDInit /ProcSet findresource begin 12 dict begin begincmap\n"
         << "/CIDSystemInfo << /Registry (TeXmacs) /Ordering (Type3) "
         << "/Supplement 0 >> def\n"
         << "/CMapName /TeXmacs-Type3 def /CMapType 2 def\n"
         << "1 begincodespacerange <" << as_hexadecimal (first, 2)
         << "> <" << as_hexadecimal (last, 2) << "> endcodespacerange\n";
    int n= 0;
    for (int c=first; c<=last; c++) if (it.gid[c] >= 0) n++;
    cmap << as_string (n) << " beginbfchar\n";
    for (int c=first; c<=last; c++)
      if (it.gid[c] >= 0)
        cmap << "<" << as_hexadecimal (c, 2) << "> <"
             << as_hexadecimal (it.gid[c], 4) << ">\n";
    cmap << "endbfchar\nendcmap CMapName currentdict /CMap defineresource "
         << "pop end end\n";
    c_string cm (cmap);
    fz_buffer* cb= fz_new_buffer_from_copied_data (ctx, (unsigned char*) (char*) cm,
                                                   strlen (cm));
    pdf_dict_put_drop (ctx, it.obj, PDF_NAME(ToUnicode),
                       pdf_add_stream (ctx, doc, cb, NULL, 0));
    fz_drop_buffer (ctx, cb);
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF could not write a Type 3 font: "
                    << fz_caught_message (ctx) << LF;
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
    int num= name_xobject (pdf_add_image (ctx, doc, img));
    if (num >= 0) {
      string nm= "Im" * as_string (num);
      c_string cnm (nm);
      double x0= to_x (x) - gl->xoff, y0= to_y (y) - h + gl->yoff;
      fz_append_printf (ctx, contents, "q %g 0 0 %g %g %g cm /%s Do Q\n",
                        (double) w, (double) h, x0, y0, (const char*) cnm);
    }
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

// Give an XObject a name in the resources, or find the one it has. MuPDF
// stores an image once however often it is added, so the same object
// comes back and the same name serves: without this, a pattern of a
// thousand tiles named a thousand resources for one image.
int
mupdf_pdf_renderer_rep::name_xobject (pdf_obj* ref) {
  if (ref == NULL) return -1;
  pointer key= (pointer) ref;
  if (xobj_num->contains (key)) { pdf_drop_obj (ctx, ref); return xobj_num (key); }
  int num= n_xobj++;
  string nm= "Im" * as_string (num);
  c_string cnm (nm);
  pdf_dict_puts_drop (ctx, res_xobj, cnm, ref);
  xobj_num (key)= num;
  return num;
}

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
    int num= name_xobject (pdf_add_image (ctx, doc, img));
    if (num >= 0) {
      string nm= "Im" * as_string (num);
      c_string cnm (nm);
      double x0= to_x (x) - rep->ox, y0= to_y (y) - rep->oy;
      fz_append_printf (ctx, contents, "q %g 0 0 %g %g %g cm /%s Do Q\n",
                        (double) rep->w, (double) rep->h, x0, y0,
                        (const char*) cnm);
    }
  }
  fz_always (ctx) { fz_drop_image (ctx, img); }
  fz_catch (ctx) {
    convert_warning << "MuPDF picture failed: "
                    << fz_caught_message (ctx) << LF;
  }
}

/******************************************************************************
* Images which are not pictures: a figure included in the document
******************************************************************************/

// the content streams of a page, which may be one stream or a list of them
static fz_buffer*
page_contents (fz_context* ctx, pdf_obj* contents) {
  if (!pdf_is_array (ctx, contents)) return pdf_load_stream (ctx, contents);
  fz_buffer* all= fz_new_buffer (ctx, 4096);
  int n= pdf_array_len (ctx, contents);
  for (int i=0; i<n; i++) {
    fz_buffer* b= NULL;
    fz_try (ctx) { b= pdf_load_stream (ctx, pdf_array_get (ctx, contents, i)); }
    fz_catch (ctx) { b= NULL; }
    if (b == NULL) continue;
    unsigned char* d= NULL;
    size_t len= fz_buffer_storage (ctx, b, &d);
    fz_append_data (ctx, all, d, len);
    fz_append_byte (ctx, all, '\n');
    fz_drop_buffer (ctx, b);
  }
  return all;
}

// The XObject for a file, added once and used as often as it occurs.
// A PDF goes in as a form -- its own drawing, kept as drawing -- and a
// raster image as an image; anything else (EPS, PostScript, SVG) is
// turned into a PDF first, which is what pdf_hummus_renderer does too.
// Returns the number of the XObject, or -1.
int
mupdf_pdf_renderer_rep::embed_image (url u) {
  url name= resolve (u);
  if (is_none (name)) return -1;
  string key= concretize (name);
  if (image_pool->contains (key)) return image_pool (key);
  string s= locase_all (suffix (name));
  int num= -1;
  bool raster= (s == "png" || s == "jpg" || s == "jpeg" || s == "gif" ||
                s == "bmp" || s == "tif" || s == "tiff");
  url tmp= url_none ();
  if (raster) {
    fz_image* img= NULL;
    fz_try (ctx) {
      img= mupdf_load_image (name);
      if (img != NULL) num= name_xobject (pdf_add_image (ctx, doc, img));
    }
    fz_always (ctx) { fz_drop_image (ctx, img); }
    fz_catch (ctx) { num= -1; }
  }
  else {
    url pdf= name;
    if (s != "pdf") {
      // let the converters of TeXmacs make a PDF of it
      tmp= url_temp (".pdf");
      int w= 0, h= 0;
      image_size (name, w, h);
      image_to_pdf (name, tmp, w, h, 300);
      pdf= tmp;
    }
    pdf_document* src= NULL;
    fz_buffer* buf= NULL;
    pdf_graft_map* map= NULL;
    fz_try (ctx) {
      c_string path (concretize (pdf));
      src= pdf_open_document (ctx, path);
      pdf_obj* spage= pdf_lookup_page_obj (ctx, src, 0);
      fz_rect box; fz_matrix m;
      pdf_page_obj_transform (ctx, spage, &box, &m);
      buf= page_contents (ctx, pdf_dict_get (ctx, spage, PDF_NAME(Contents)));
      pdf_obj* sres= pdf_dict_get_inheritable (ctx, spage, PDF_NAME(Resources));
      map= pdf_new_graft_map (ctx, doc);
      pdf_obj* res= (sres == NULL) ? NULL
                                   : pdf_graft_mapped_object (ctx, map, sres);
      pdf_obj* xo= pdf_new_xobject (ctx, doc, box, m, res, buf);
      num= n_xobj++;
      string nm= "Im" * as_string (num);
      c_string cnm (nm);
      pdf_dict_puts_drop (ctx, res_xobj, cnm, xo);
      pdf_drop_obj (ctx, res);
    }
    fz_always (ctx) {
      pdf_drop_graft_map (ctx, map);
      fz_drop_buffer (ctx, buf);
      pdf_drop_document (ctx, src);
    }
    fz_catch (ctx) {
      convert_warning << "MuPDF cannot include " << name << ": "
                      << fz_caught_message (ctx) << LF;
      num= -1;
    }
    if (!is_none (tmp)) remove (tmp);
  }
  image_pool (key)= num;
  return num;
}

// w and h are the size the picture must have, in the pixels of the
// renderer; an XObject is drawn in the unit square, or in its own BBox,
// which the matrix maps onto that size
void
mupdf_pdf_renderer_rep::place_image (int num, double w, double h,
                                     SI x, SI y, int alpha) {
  if (num < 0 || contents == NULL) return;
  end_text ();
  select_alpha (alpha);
  string nm= "Im" * as_string (num);
  c_string cnm (nm);
  double m[6];
  xobject_fit (pdf_dict_gets (ctx, res_xobj, cnm), w, h, to_x (x), to_y (y), m);
  fz_append_printf (ctx, contents, "q %g %g %g %g %g %g cm /%s Do Q\n",
                    m[0], m[1], m[2], m[3], m[4], m[5], (const char*) cnm);
}

// The matrix which draws an XObject into the box of size w by h whose
// lower left corner is at (x, y). An image is drawn in the unit square. A
// form is drawn in its /BBox as its own /Matrix places it -- a page turned
// by /Rotate comes with a matrix which turns it back -- so it is that
// placed box which must be mapped onto the one asked for.
void
mupdf_pdf_renderer_rep::xobject_fit (pdf_obj* xo, double w, double h,
                                     double x, double y, double m[6]) {
  m[0]= w; m[1]= 0; m[2]= 0; m[3]= h; m[4]= x; m[5]= y;
  if (xo == NULL ||
      !pdf_name_eq (ctx, pdf_dict_get (ctx, xo, PDF_NAME(Subtype)),
                    PDF_NAME(Form))) return;
  fz_rect b= pdf_dict_get_rect (ctx, xo, PDF_NAME(BBox));
  fz_matrix fm= pdf_dict_get_matrix (ctx, xo, PDF_NAME(Matrix));
  b= fz_transform_rect (b, fm);
  double bw= b.x1 - b.x0, bh= b.y1 - b.y0;
  if (bw <= 0 || bh <= 0) { m[0]= m[3]= 1; return; }
  m[0]= w / bw; m[3]= h / bh;
  m[4]= x - b.x0 * m[0];
  m[5]= y - b.y0 * m[3];
}

// A tiling pattern of the XObject img, the cell w by h in the pixels of
// the renderer, the lattice anchored at (ax, ay). A pattern lives in the
// default space of the page, not in the space of the stream where it is
// used, so its matrix carries the scaling to points which begin_page
// gives the stream.
int
mupdf_pdf_renderer_rep::tiling_pattern (int img, double w, double h,
                                        double ax, double ay) {
  string key= as_string (img) * ":" * as_string (w) * ":" * as_string (h)
              * ":" * as_string (ax) * ":" * as_string (ay);
  if (pattern_pool->contains (key)) return pattern_pool (key);
  string inm= "Im" * as_string (img);
  c_string cinm (inm);
  pdf_obj* xo= pdf_dict_gets (ctx, res_xobj, cinm);
  if (xo == NULL) return -1;
  int num= -1;
  fz_buffer* buf= NULL;
  pdf_obj* dict= NULL;
  fz_var (buf); fz_var (dict); fz_var (num);
  fz_try (ctx) {
    double m[6];
    xobject_fit (xo, w, h, 0, 0, m);
    buf= fz_new_buffer (ctx, 128);
    fz_append_printf (ctx, buf, "q %g %g %g %g %g %g cm /%s Do Q\n",
                      m[0], m[1], m[2], m[3], m[4], m[5], (const char*) cinm);
    dict= pdf_new_dict (ctx, doc, 9);
    pdf_dict_put (ctx, dict, PDF_NAME(Type), PDF_NAME(Pattern));
    pdf_dict_put_int (ctx, dict, PDF_NAME(PatternType), 1);
    pdf_dict_put_int (ctx, dict, PDF_NAME(PaintType), 1);   // coloured
    pdf_dict_put_int (ctx, dict, PDF_NAME(TilingType), 1);  // constant spacing
    pdf_dict_put_rect (ctx, dict, PDF_NAME(BBox), fz_make_rect (0, 0, w, h));
    pdf_dict_put_real (ctx, dict, PDF_NAME(XStep), w);
    pdf_dict_put_real (ctx, dict, PDF_NAME(YStep), h);
    double f= (double) default_dpi / dpi;
    pdf_dict_put_matrix (ctx, dict, PDF_NAME(Matrix),
                         fz_make_matrix (f, 0, 0, f, f * ax, f * ay));
    pdf_obj* res= pdf_dict_put_dict (ctx, dict, PDF_NAME(Resources), 1);
    pdf_obj* xres= pdf_dict_put_dict (ctx, res, PDF_NAME(XObject), 1);
    pdf_dict_puts (ctx, xres, cinm, xo);
    pdf_obj* ref= pdf_add_stream (ctx, doc, buf, dict, 0);
    num= n_pat++;
    string pnm= "P" * as_string (num);
    c_string cpnm (pnm);
    pdf_dict_puts_drop (ctx, res_pat, cpnm, ref);
  }
  fz_always (ctx) {
    fz_drop_buffer (ctx, buf);
    pdf_drop_obj (ctx, dict);
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF could not make a pattern: "
                    << fz_caught_message (ctx) << LF;
    num= -1;
  }
  pattern_pool (key)= num;
  return num;
}

// Can this tile go into a tiling pattern and be drawn by every reader as
// it is drawn outside one? Not when it has transparency: Ghostscript 10
// draws an image with a soft mask differently inside a tiling pattern
// than as the same image placed tile by tile (46% of the pixels alike on
// a photo tile, where MuPDF agrees with itself at 97.6%), and printing
// often goes through Ghostscript. An opaque image, or a form with nothing
// transparent in it, is safe; anything else is drawn tile by tile.
static bool
transparent_state (fz_context* ctx, pdf_obj* g) {
  pdf_obj* sm= pdf_dict_get (ctx, g, PDF_NAME(SMask));
  if (sm != NULL && !pdf_name_eq (ctx, sm, PDF_NAME(None))) return true;
  if (pdf_dict_get (ctx, g, PDF_NAME(ca)) != NULL &&
      pdf_dict_get_real (ctx, g, PDF_NAME(ca)) < 1) return true;
  if (pdf_dict_get (ctx, g, PDF_NAME(CA)) != NULL &&
      pdf_dict_get_real (ctx, g, PDF_NAME(CA)) < 1) return true;
  pdf_obj* bm= pdf_dict_get (ctx, g, PDF_NAME(BM));
  if (bm != NULL && pdf_is_name (ctx, bm)) {
    const char* b= pdf_to_name (ctx, bm);
    if (strcmp (b, "Normal") != 0 && strcmp (b, "Compatible") != 0) return true;
  }
  return false;
}

static bool
transparent_xobject (fz_context* ctx, pdf_obj* xo, int depth) {
  if (xo == NULL || depth > 8) return true;
  if (pdf_dict_get (ctx, xo, PDF_NAME(SMask)) != NULL) return true;
  if (!pdf_name_eq (ctx, pdf_dict_get (ctx, xo, PDF_NAME(Subtype)),
                    PDF_NAME(Form))) return false;
  if (pdf_dict_get (ctx, xo, PDF_NAME(Group)) != NULL) return true;
  pdf_obj* res= pdf_dict_get (ctx, xo, PDF_NAME(Resources));
  pdf_obj* gs= pdf_dict_get (ctx, res, PDF_NAME(ExtGState));
  for (int i=0; i<pdf_dict_len (ctx, gs); i++)
    if (transparent_state (ctx, pdf_dict_get_val (ctx, gs, i))) return true;
  pdf_obj* xs= pdf_dict_get (ctx, res, PDF_NAME(XObject));
  for (int i=0; i<pdf_dict_len (ctx, xs); i++)
    if (transparent_xobject (ctx, pdf_dict_get_val (ctx, xs, i), depth + 1))
      return true;
  return false;
}

bool
mupdf_pdf_renderer_rep::opaque_tile (int img) {
  string nm= "Im" * as_string (img);
  c_string cnm (nm);
  pdf_obj* xo= pdf_dict_gets (ctx, res_xobj, cnm);
  bool r= false;
  fz_try (ctx) { r= (xo != NULL) && !transparent_xobject (ctx, xo, 0); }
  fz_catch (ctx) { r= false; }
  return r;
}

// A pattern background, as one fill with a tiling pattern instead of a
// Do for every tile (renderer_rep::clear_pattern draws the tiles one by
// one, which is right for a screen and five hundred operators on a page).
// The size of the tile and the anchor of the lattice are computed exactly
// as renderer_rep::clear_pattern computes them, so that the two draw the
// same thing; under a transformation of the graphics the pattern would
// have to follow it, and there the tiles are left to the generic code.
void
mupdf_pdf_renderer_rep::clear_pattern (SI mx1, SI my1, SI mx2, SI my2,
                                       SI x1, SI y1, SI x2, SI y2) {
  brush b= get_background ();
  if (contents == NULL || transform_level > 0 ||
      b->get_type () != brush_pattern || !is_func (b->get_pattern (), _PATTERN)) {
    renderer_rep::clear_pattern (mx1, my1, mx2, my2, x1, y1, x2, y2);
    return;
  }
  tree pattern= b->get_pattern ();
  int pattern_alpha= b->get_alpha ();
  outer_round (x1, y1, x2, y2);
  // -- as in renderer_rep::clear_pattern ---------------------------------
  url u= b->get_pattern_url ();
  int imw_pt, imh_pt;
  image_size (u, imw_pt, imh_pt);
  double pt= ((double) 600*PIXEL) / 72.0;
  SI imw= (SI) (((double) imw_pt) * pt);
  SI imh= (SI) (((double) imh_pt) * pt);
  double ratio= ((double) max (imw_pt, 1)) / ((double) max (imh_pt, 1));
  bool flag= false;
  SI w= mx2 - mx1, h= my2 - my1;
  if (pattern[1] == "") w= imw;
  else if (is_int (pattern[1])) w= as_int (pattern[1]);
  else if (is_percentage (pattern[1]))
    w= (SI) (as_percentage (pattern[1]) * ((double) w));
  else flag= true;
  if (pattern[1] == "") h= imh;
  else if (is_int (pattern[2])) h= as_int (pattern[2]);
  else if (is_percentage (pattern[2]))
    h= (SI) (as_percentage (pattern[2]) * ((double) h));
  else if (is_percentage (pattern[2], "@"))
    h= (SI) (as_percentage (pattern[2]) * ((double) w) / ratio);
  if (flag && is_percentage (pattern[1], "@"))
    w= (SI) (as_percentage (pattern[1]) * ((double) h) * ratio);
  w= ((w + pixel - 1) / pixel) * pixel;
  h= ((h + pixel - 1) / pixel) * pixel;
  tree eff= "";
  if (N(pattern) == 4 && is_compound (pattern[3])) eff= pattern[3];
  // -----------------------------------------------------------------------
  if (w <= 0 || h <= 0) return;
  // the tile: the file itself, or the picture its effect makes of it
  int img= -1;
  if (eff == tree ("")) img= embed_image (u);
  else {
    picture pic= cached_load_picture (u, w/pixel, h/pixel, eff, pixel, false);
    picture p2= as_mupdf_picture (pic);
    mupdf_picture_rep* rep= (mupdf_picture_rep*) p2->get_handle ();
    if (rep != NULL && rep->pix != NULL) {
      fz_image* fim= NULL;
      fz_try (ctx) {
        fim= mupdf_image_from_pixmap (rep->pix);
        img= name_xobject (pdf_add_image (ctx, doc, fim));
      }
      fz_always (ctx) { fz_drop_image (ctx, fim); }
      fz_catch (ctx) { img= -1; }
    }
  }
  // a transparent tile, or a pattern drawn with an alpha of its own, is
  // left to be drawn tile by tile (see opaque_tile)
  if (img >= 0 && (pattern_alpha < 255 || !opaque_tile (img))) img= -1;
  // the tiles meet at mx1 across and at my2 down (sx= -mx1, sy= -my2)
  int pat= (img < 0) ? -1 :
    tiling_pattern (img, ((double) w) / pixel, ((double) h) / pixel,
                    to_x (mx1), to_y (my2));
  if (pat < 0) {
    renderer_rep::clear_pattern (mx1, my1, mx2, my2, x1, y1, x2, y2);
    return;
  }
  end_text ();
  // all of it inside q ... Q: the colour space, the pattern and the alpha
  // are gone afterwards and what we track still holds
  put ("q\n");
  if (pattern_alpha < 255)
    fz_append_printf (ctx, contents, "/GS%d gs\n", alpha_state (pattern_alpha));
  fz_append_printf (ctx, contents, "/Pattern cs /P%d scn %g %g %g %g re f\nQ\n",
                    pat, to_x (x1), to_y (y1),
                    to_x (x2) - to_x (x1), to_y (y2) - to_y (y1));
}

void
mupdf_pdf_renderer_rep::draw_scalable (scalable im, SI x, SI y, int alpha) {
  // an image with an effect on it has to be computed, so it is rasterized
  // (at a print resolution, see shadow); a plain one is included as it is
  if (im->get_type () != scalable_image || im->get_effect () != tree ("")) {
    renderer_rep::draw_scalable (im, x, y, alpha);
    return;
  }
  int num= embed_image (im->get_name ());
  if (num < 0) { renderer_rep::draw_scalable (im, x, y, alpha); return; }
  rectangle r= im->get_logical_extents ();
  double w= ((double) (r->x2 - r->x1)) / pixel;
  double h= ((double) (r->y2 - r->y1)) / pixel;
  place_image (num, w, h, x - r->x1, y - r->y1, alpha);
}

/******************************************************************************
* Links, the outline and the metadata
******************************************************************************/

// A place a link can point at. They are collected here and written as a
// name tree when the document is closed, since a link may well come
// before the page it points at has been laid out.
void
mupdf_pdf_renderer_rep::anchor (string label, SI x1, SI y1, SI x2, SI y2) {
  (void) y1; (void) x2;
  if (dest_index->contains (label)) return;
  double f= (double) default_dpi / dpi;
  dest_index (label)= N(dest_name);
  dest_name << label;
  dest_pos << pdf_link_item (label, (double) page_num,
                             f * to_x (x1), f * to_y (y2 + 20*pixel), 0);
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
    if (starts (links[i].label, "#")) {
      // a place in the document: the name is the one anchor () registered,
      // resolved through the tree write_dests puts in the catalogue
      c_string s (links[i].label);
      pdf_dict_put_text_string (ctx, a, PDF_NAME(Dest), s);
    }
    else {
      pdf_obj* act= pdf_dict_put_dict (ctx, a, PDF_NAME(A), 2);
      pdf_dict_put (ctx, act, PDF_NAME(S), PDF_NAME(URI));
      c_string s (links[i].label);
      pdf_dict_put_text_string (ctx, act, PDF_NAME(URI), s);
    }
    pdf_array_push_drop (ctx, annots, pdf_add_object_drop (ctx, doc, a));
  }
}

// The outline, as the tree its levels describe. An entry hangs under the
// last one of a smaller level; the count of an entry is negative when its
// children start folded, which is what a reader expects of a deep tree.
void
mupdf_pdf_renderer_rep::write_outline () {
  if (N(outlines) == 0) return;
  pdf_obj* root= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Root));
  pdf_obj* out= pdf_dict_put_dict (ctx, root, PDF_NAME(Outlines), 4);
  pdf_dict_put (ctx, out, PDF_NAME(Type), PDF_NAME(Outlines));
  // the chain of parents: parent[l] is the open entry of level l
  array<pdf_obj*> parent;  parent << out;
  array<int>      level;   level  << 0;
  array<pdf_obj*> first;   first  << (pdf_obj*) NULL;
  array<pdf_obj*> last;    last   << (pdf_obj*) NULL;
  array<int>      count;   count  << 0;
  for (int i=0; i<N(outlines); i++) {
    if (outlines[i].page >= N(pages)) continue;
    int l= outlines[i].level;
    while (N(parent) > 1 && level[N(level)-1] >= l) {
      // close the entries which this one is not under
      int k= N(parent) - 1;
      if (first[k] != NULL) {
        pdf_dict_put (ctx, parent[k], PDF_NAME(First), first[k]);
        pdf_dict_put (ctx, parent[k], PDF_NAME(Last), last[k]);
        pdf_dict_put_int (ctx, parent[k], PDF_NAME(Count), -count[k]);
      }
      parent->resize (k); level->resize (k);
      first->resize (k); last->resize (k); count->resize (k);
    }
    int k= N(parent) - 1;
    pdf_obj* item= pdf_new_dict (ctx, doc, 6);
    c_string t (outlines[i].title);
    pdf_dict_put_text_string (ctx, item, PDF_NAME(Title), t);
    pdf_obj* dest= pdf_dict_put_array (ctx, item, PDF_NAME(Dest), 5);
    pdf_array_push (ctx, dest, pages[outlines[i].page]);
    pdf_array_push (ctx, dest, PDF_NAME(XYZ));
    pdf_array_push_real (ctx, dest, outlines[i].x);
    pdf_array_push_real (ctx, dest, outlines[i].y);
    pdf_array_push_int (ctx, dest, 0);
    pdf_obj* ref= pdf_add_object_drop (ctx, doc, item);
    pdf_dict_put (ctx, ref, PDF_NAME(Parent), parent[k]);
    if (first[k] == NULL) first[k]= ref;
    else {
      pdf_dict_put (ctx, last[k], PDF_NAME(Next), ref);
      pdf_dict_put (ctx, ref, PDF_NAME(Prev), last[k]);
    }
    last[k]= ref; count[k]++;
    // this entry is now open for the deeper ones
    parent << ref; level << l;
    first << (pdf_obj*) NULL; last << (pdf_obj*) NULL; count << 0;
  }
  for (int k= N(parent) - 1; k >= 0; k--) {
    if (first[k] == NULL) continue;
    pdf_dict_put (ctx, parent[k], PDF_NAME(First), first[k]);
    pdf_dict_put (ctx, parent[k], PDF_NAME(Last), last[k]);
    pdf_dict_put_int (ctx, parent[k], PDF_NAME(Count),
                      (k == 0) ? count[k] : -count[k]);
  }
}

// The places the links point at, as the name tree of the catalogue. The
// names must be sorted, which is what a reader relies on to find one.
void
mupdf_pdf_renderer_rep::write_dests () {
  if (N(dest_name) == 0) return;
  array<int> ord;
  for (int i=0; i<N(dest_name); i++) ord << i;
  for (int i=1; i<N(ord); i++)     // few and nearly sorted: insertion
    for (int j=i; j>0 && dest_name[ord[j]] < dest_name[ord[j-1]]; j--) {
      int t= ord[j]; ord[j]= ord[j-1]; ord[j-1]= t;
    }
  pdf_obj* root= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Root));
  pdf_obj* names= pdf_dict_put_dict (ctx, root, PDF_NAME(Names), 1);
  pdf_obj* dests= pdf_dict_put_dict (ctx, names, PDF_NAME(Dests), 1);
  pdf_obj* arr= pdf_dict_put_array (ctx, dests, PDF_NAME(Names),
                                    2 * N(ord));
  for (int i=0; i<N(ord); i++) {
    int k= ord[i];
    int page= (int) dest_pos[k].x1;
    if (page < 0 || page >= N(pages)) continue;
    c_string nm (dest_name[k]);
    pdf_array_push_string (ctx, arr, nm, strlen (nm));
    pdf_obj* d= pdf_new_array (ctx, doc, 5);
    pdf_array_push (ctx, d, pages[page]);
    pdf_array_push (ctx, d, PDF_NAME(XYZ));
    pdf_array_push_real (ctx, d, dest_pos[k].y1);
    pdf_array_push_real (ctx, d, dest_pos[k].x2);
    pdf_array_push_int (ctx, d, 0);
    pdf_array_push_drop (ctx, arr, pdf_add_object_drop (ctx, doc, d));
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

// Whatever falls back to being rasterized -- a pattern, a scalable image
// with an effect -- is drawn into a picture through this; on paper it must
// be made at a print resolution and not at the resolution of a screen.
renderer
mupdf_pdf_renderer_rep::shadow (picture& pic, SI x1, SI y1, SI x2, SI y2) {
  double old_zoomf= this->zoomf;
  set_zoom_factor (5.0 * PICTURE_ZOOM);
  renderer ren= renderer_rep::shadow (pic, x1, y1, x2, y2);
  set_zoom_factor (old_zoomf);
  return ren;
}

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
* Attachments
******************************************************************************/

// The files go into the /EmbeddedFiles name tree of the catalogue, and
// into /AF, which is what a reader looks at to offer them.
bool
mupdf_pdf_make_attachments (url pdf_path, array<url> attachments,
                            url out_path) {
  fz_context* ctx= mupdf_context ();
  pdf_document* doc= NULL;
  bool ok= false;
  fz_var (doc); fz_var (ok);
  fz_try (ctx) {
    c_string in (concretize (pdf_path));
    doc= pdf_open_document (ctx, in);
    pdf_obj* root= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Root));
    pdf_obj* names= pdf_dict_get (ctx, root, PDF_NAME(Names));
    if (names == NULL) names= pdf_dict_put_dict (ctx, root, PDF_NAME(Names), 2);
    pdf_obj* ef= pdf_dict_get (ctx, names, PDF_NAME(EmbeddedFiles));
    if (ef == NULL) ef= pdf_dict_put_dict (ctx, names, PDF_NAME(EmbeddedFiles), 1);
    pdf_obj* arr= pdf_dict_get (ctx, ef, PDF_NAME(Names));
    if (arr == NULL)
      arr= pdf_dict_put_array (ctx, ef, PDF_NAME(Names), 2 * N(attachments));
    pdf_obj* af= pdf_dict_get (ctx, root, PDF_NAME(AF));
    if (af == NULL) af= pdf_dict_put_array (ctx, root, PDF_NAME(AF),
                                            N(attachments));
    for (int i=0; i<N(attachments); i++) {
      string body;
      if (load_string (attachments[i], body, false)) continue;  // unreadable
      string base= as_string (tail (attachments[i]));
      c_string nm (base), data (body);
      fz_buffer* buf=
        fz_new_buffer_from_copied_data (ctx, (unsigned char*) (char*) data,
                                        (size_t) N(body));
      pdf_obj* fs= pdf_add_embedded_file (ctx, doc, nm,
                                          "application/octet-stream",
                                          buf, 0, 0, 0);
      fz_drop_buffer (ctx, buf);
      pdf_array_push_string (ctx, arr, nm, strlen (nm));
      pdf_array_push (ctx, arr, fs);
      pdf_array_push_drop (ctx, af, fs);
    }
    pdf_write_options opts= pdf_default_write_options;
    opts.do_compress= 1;
    opts.do_garbage= 3;
    c_string out (concretize (out_path));
    pdf_save_document (ctx, doc, out, &opts);
    ok= true;
  }
  fz_always (ctx) { pdf_drop_document (ctx, doc); }
  fz_catch (ctx) {
    convert_error << "MuPDF cannot attach to " << pdf_path << ": "
                  << fz_caught_message (ctx) << LF;
    ok= false;
  }
  return ok;
}

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
