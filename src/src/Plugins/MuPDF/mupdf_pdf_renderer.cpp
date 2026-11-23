
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
* The PDF writer of the builds without PDFHummus (Vue, SDL, the browser),
* and an alternative to pdf_hummus_renderer elsewhere, which produces the
* PDF with MuPDF instead of the vendored PDFHummus (use_mupdf_pdf in
* edit_main.cpp). See docs/pdf-output-with-mupdf.md for what it does and
* what it does not.
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
#include "Metafont/tex_unicode.hpp"
#include "pdf_tags.hpp"
#include "Freetype/pdf_font_subst.hpp"
#include "mupdf_renderer.hpp"
#include "mupdf_picture.hpp"
#include "analyze.hpp"
#include "file.hpp"
#include "scheme.hpp"
#include "hashmap.hpp"
#include "iterator.hpp"
#include "hashset.hpp"
#include "font.hpp"
#include "frame.hpp"
#include "image_files.hpp"
#include "scalable.hpp"
#include "converter.hpp"   // decode_from_utf8
#include "link.hpp"        // get_locus_rendering
#include "tm_configure.hpp"  // TEXMACS_VERSION

#include <time.h> // time, for the creation date
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
  string   tex_name;   // the TeX font (cmex10...) whose positions the codes
                       // are, for the characters of the text layer
  array<int> gid;      // 256 entries: the glyph TeXmacs asked for, -1 if free
  // the letters a ligature glyph stands for, by the code (a simple font)
  // or the glyph (a CID font) it is written with; "" for a glyph which is
  // not a ligature, nothing when it has not been asked yet
  hashmap<int,string> lig;
  // a CID font: the characters of the glyphs drawn by their number which
  // the font recorded (the larger variants of a math symbol), which its
  // cmap does not have
  hashmap<int,int> drawn;
  // ... and of those of which the font recorded none, the character of
  // the name of the glyph, a guess (see glyph_name_unicode)
  hashmap<int,int> named;
  // a CID font: the glyphs drawn, whose characters the map gives
  hashmap<int,bool> used;
  pdf_font_item ()
    : font (NULL), obj (NULL), num (0), simple (false), t3 (false), chunk (0),
      lig (""), drawn (0), named (0), used (false) {}
};

// The UTF-16 of the character u in hexadecimal, as a CMap wants it: a
// surrogate pair beyond U+FFFF
static string
utf16_hex (int u) {
  if (u <= 0xffff) return as_hexadecimal (u, 4);
  int v= u - 0x10000;
  return as_hexadecimal (0xd800 + (v >> 10), 4) *
         as_hexadecimal (0xdc00 + (v & 0x3ff), 4);
}

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

// how names and addresses are written (see the links, below)
static string pdf_text_bytes (string s);
static string uri_bytes (string s);

// The keys of the parent tree of the annotations come after those of the
// pages, of which there are fewer
static const int tag_annot_key0= 1000000;

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

  // tagged PDF (when tagger is not NULL): the elements, by their key, and
  // their children; the block of marked content open on the page (-2:
  // none, -1: an artifact, else its element) and the element of each MCID
  // of the page; the last box resolved, its elements and its place
  pdf_tags            tags;          // NULL until there is a tagger
  array<pdf_obj*>     tag_objs;      // the objects of the elements
  array<pdf_obj*>     tag_annots;    // the annotations, for their OBJR
  array<int>          tag_annot_page;
  array<int>          link_elem;     // the element of each link of the page
  array<path>         link_pos;      // ... and the place of its text
  array<string>       link_text;     // its /Contents, ending with a 0
  int                 link_annot0;   // the first of them in tag_annots

  // embedded videos (see embed_video): the file specification of each MP4
  // file, by the image and its delay (-1: it could not be made), and the
  // videos of the page being written
  hashmap<string,int> video_index;
  array<pdf_obj*> video_fs;
  array<string>   video_fname;     // the name of the file, ending with a 0
  array<url>      temp_videos;
  array<pdf_link_item> page_videos;  // label: the name of the annotation
  array<int>      page_video_file;
  array<string>   page_video_vars; // its FlashVars, ending with a 0
  array<string>   page_video_nm;   // its /NM, ending with a 0
  array<int>      page_video_elem; // its Annot element (tagged PDF)
  array<path>     page_video_pos;
  int             video_annot0;    // the first of them in tag_annots

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

  // MuPDF reports an error with a longjmp (fz_throw), and one which no
  // fz_try catches ends the process. Every MuPDF call is therefore made
  // inside an fz_try, and the body of an fz_try creates nothing which has
  // a destructor, in its own frame or in a frame it calls: the longjmp
  // would skip it. Hence what TeXmacs has to compute -- strings, arrays --
  // is computed first, and MuPDF is handed the results.
  //
  // The page stream is written through put and app, which catch the error
  // themselves (they are called with C++ objects alive around them, from
  // everywhere). A page stream which failed halfway is not a page: broken
  // says so, and the file is then not written.
  bool   broken;
  void   fail (const char* what);
  void   put (const char* s);
  void   app (const char* fmt, ...) __attribute__ ((format (printf, 2, 3)));
  void   arc_path (double cx, double cy, double rx, double ry,
                   double a0, double a1, bool move);

  void begin_page ();
  void end_page ();
  void end_text ();
  void begin_actual_text (string s);
  void end_actual_text ();
  void draw_invisible (int c, font_glyphs fn, SI x, SI y, double hscale);
  void fix_to_unicode (pdf_font_item& it);
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
  void write_tex_to_unicode (pdf_font_item& it, int first, int last,
                             array<string>& gname);
  int  glyph_name_unicode (pdf_font_item& it, int gid);
  void write_encoding (pdf_font_item& it, int first, int last,
                       array<string>& gname);
  void write_widths (pdf_font_item& it, int first, int last,
                     array<int>& ftgid);
  void subset_type1 (pdf_font_item& it, array<string> keep);
  void write_outline ();
  void write_dests ();
  void write_videos (pdf_obj* pobj);
  pdf_tags tg ();
  void tag_mark (int kind);
  void tag_close ();
  void write_structure ();
  void write_links (pdf_obj* pobj, array<string>& target, array<int>& local,
                    int border);
  void write_metadata ();
  void draw_bitmap_glyph (int ch, font_glyphs fn, SI x, SI y);
  int  embed_image (url u, bool graft= false);
  array<pdf_tag_graft> grafts;     // the structures of tagged pictures
  hashmap<int,int> graft_of;       // by number of their forms
  hashset<string> untagged_files;  // the pictures which have none
  array<pdf_obj*> graft_forms;     // the form of each graft (pdf_tags)
  hashmap<string,pointer> graft_docs;   // a grafted picture's document and
  hashmap<string,pointer> graft_maps;   // map, for its other uses: one
                                        // copy of its resources
  void embed_picture_fonts (pdf_obj* res, pdf_obj* form);
  bool strip_picture_tags (pdf_obj* res, pdf_obj* form, hashset<int>* keep);
  array<pdf_obj*> picture_font_objs;   // the programs embedded for them
  int picture_font_tags= 0;            // the tags of their subsets
  hashmap<string,int> picture_font_files;   // whole programs, by file
  void merge_layers (pdf_document* src, pdf_graft_map* map);
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
  void embed_video (url u, int ms, bool repeat, SI x1, SI y1, SI x2, SI y2);
  void set_metadata (string kind, string val);
  bool supports_tagging ();
};

/******************************************************************************
* Construction
******************************************************************************/

// Whether the last PDF was saved: the writer saves it when it is closed,
// and print_doc asks afterwards (an error there is only a message)
static bool last_save_ok= false;

bool
mupdf_pdf_saved () {
  return last_save_ok;
}

mupdf_pdf_renderer_rep::mupdf_pdf_renderer_rep (
  url pdf_file_name2, int dpi2, int nr_pages2,
  string page_type2, bool landscape2, double paper_w2, double paper_h2)
  : renderer_rep (false),
    pdf_file_name (pdf_file_name2), dpi (dpi2), nr_pages (nr_pages2),
    page_type (page_type2), landscape (landscape2),
    paper_w (paper_w2), paper_h (paper_h2),
    started (false), page_num (0), broken (false),
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
    n_alpha (0), n_xobj (0),
    tags (NULL), link_annot0 (0), video_annot0 (0)
{
  last_save_ok= false;
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

// Encryption. The passwords come from the environment, where the command
// which asks for them (File -> Export -> Pdf with password, tm-print.scm)
// puts them for one export and takes them away again: a preference or the
// document would keep them on the disk in clear. With no password nothing
// is encrypted. The permissions are a list separated by commas (print,
// print-hq, copy, modify, annotate, form, assemble, accessibility), or all,
// the default, or none: what a reader lets someone who has only the password to
// open it do; the owner password (the password to open it when none is
// given) lifts them. AES with a key of 256 bits.
static int
pdf_permissions (string s) {
  if (N(s) == 0 || s == "all") return ~0;
  if (s == "none") return 0;   // to be read, and nothing else
  int r= 0;
  array<string> l= tokenize (s, ",");
  for (int i=0; i<N(l); i++) {
    string w= trim_spaces (l[i]);
    if (w == "print") r |= PDF_PERM_PRINT;
    else if (w == "print-hq") r |= PDF_PERM_PRINT | PDF_PERM_PRINT_HQ;
    else if (w == "copy") r |= PDF_PERM_COPY;
    else if (w == "modify") r |= PDF_PERM_MODIFY;
    else if (w == "annotate") r |= PDF_PERM_ANNOTATE;
    else if (w == "form") r |= PDF_PERM_FORM;
    else if (w == "assemble") r |= PDF_PERM_ASSEMBLE;
    else if (w == "accessibility") r |= PDF_PERM_ACCESSIBILITY;
    else if (w == "all") r= ~0;
    else convert_warning << "unknown PDF permission " << w << LF;
  }
  return r;
}

// the options for encryption, false when a password does not fit (it is
// then not written at all: a password cut short would not be the one given)
static bool
pdf_encryption (pdf_write_options& opts) {
  string upw= get_env ("TEXMACS_PDF_USER_PASSWORD");
  string opw= get_env ("TEXMACS_PDF_OWNER_PASSWORD");
  if (N(upw) == 0 && N(opw) == 0) return true;
  if (N(opw) == 0) opw= upw;
  if (N(upw) >= (int) sizeof (opts.upwd_utf8) ||
      N(opw) >= (int) sizeof (opts.opwd_utf8)) {
    convert_error << "a PDF password may have at most "
                  << (int) sizeof (opts.upwd_utf8) - 1 << " bytes" << LF;
    return false;
  }
  memset (opts.upwd_utf8, 0, sizeof (opts.upwd_utf8));
  memset (opts.opwd_utf8, 0, sizeof (opts.opwd_utf8));
  if (N(upw) > 0) memcpy (opts.upwd_utf8, &upw[0], N(upw));
  memcpy (opts.opwd_utf8, &opw[0], N(opw));
  opts.do_encrypt= PDF_ENCRYPT_AES_256;
  opts.permissions= pdf_permissions (get_env ("TEXMACS_PDF_PERMISSIONS"));
  return true;
}

mupdf_pdf_renderer_rep::~mupdf_pdf_renderer_rep () {
  // (another renderer may have saved its PDF meanwhile)
  last_save_ok= false;
  if (!started) return;
  end_page ();
  // each of these catches its own errors (see put): an error in one part
  // loses that part, as a warning, and not the document
  write_fonts ();
  write_outline ();
  write_dests ();
  write_metadata ();
  write_structure ();
  // MuPDF subsets TrueType and CFF; the Type 1 fonts are left whole
  fz_try (ctx) { pdf_subset_fonts (ctx, doc, 0, NULL); }
  fz_catch (ctx) {
    convert_warning << "MuPDF could not subset the fonts: "
                    << fz_caught_message (ctx) << LF;
  }
  // A relative name stands for the current directory, as in save_string
  url target= pdf_file_name;
  if (!is_rooted_name (target)) target= resolve (target, "");
  c_string name (concretize (target)); // not in fz_try: a longjmp
  pdf_write_options opts= pdf_default_write_options;
  opts.do_compress= 1;
  opts.do_compress_images= 1;
  opts.do_compress_fonts= 1;
  opts.do_garbage= 4;
  // the elements of the structure of a tagged PDF, tens of thousands of
  // small objects, go in compressed object streams (PDF 1.5, as PDF/UA's
  // 1.7): the file is about half as large (TEXMACS_PDF_OBJSTMS=0: not, for
  // the tests which read the objects)
  if (tagger != NULL && get_env ("TEXMACS_PDF_OBJSTMS") != "0")
    opts.do_use_objstms= 1;
  bool encrypt_ok= pdf_encryption (opts);
  if (broken)
    convert_error << "The PDF " << pdf_file_name << " is not written: "
                  << "a page of it could not be" << LF;
  else if (is_directory (target))
    // MuPDF removes the file it writes first (fz_new_output_with_path),
    // and remove () takes an empty folder as well
    convert_error << "The PDF " << pdf_file_name << " is not written: "
                  << "a folder has its name" << LF;
  else if (!encrypt_ok)
    convert_error << "The PDF " << pdf_file_name << " is not written" << LF;
  else {
    // (braced: fz_catch is a statement of its own, which an else before
    // fz_try does not cover)
    fz_try (ctx) {
      pdf_save_document (ctx, doc, name, &opts);
      last_save_ok= true;
    }
    fz_catch (ctx) {
      convert_error << "MuPDF cannot write " << pdf_file_name << ": "
                    << fz_caught_message (ctx) << LF;
    }
  }
  for (int i=0; i<N(font_list); i++) {
    if (font_list[i].font != NULL) fz_drop_font (ctx, font_list[i].font);
    pdf_drop_obj (ctx, font_list[i].obj);
  }
  for (int i=0; i<N(pages); i++) pdf_drop_obj (ctx, pages[i]);
  for (int i=0; i<N(tag_objs); i++) pdf_drop_obj (ctx, tag_objs[i]);
  for (int i=0; i<N(graft_forms); i++) pdf_drop_obj (ctx, graft_forms[i]);
  iterator<string> git= iterate (graft_maps);
  while (git->busy ()) {
    string k= git->next ();
    pdf_drop_graft_map (ctx, (pdf_graft_map*) graft_maps [k]);
    pdf_drop_document (ctx, (pdf_document*) graft_docs [k]);
  }
  for (int i=0; i<N(picture_font_objs); i++)
    pdf_drop_obj (ctx, picture_font_objs[i]);
  if (tags != NULL) tm_delete (tags);
  for (int i=0; i<N(tag_annots); i++) pdf_drop_obj (ctx, tag_annots[i]);
  for (int i=0; i<N(video_fs); i++) pdf_drop_obj (ctx, video_fs[i]);
  for (int i=0; i<N(temp_videos); i++)
    if (exists (temp_videos[i])) remove (temp_videos[i]);
  pdf_drop_obj (ctx, resources);
  pdf_drop_document (ctx, doc);
}

void
mupdf_pdf_renderer_rep::fail (const char* what) {
  if (!broken)
    convert_error << "MuPDF could not write " << what << " of "
                  << pdf_file_name << ": " << fz_caught_message (ctx) << LF;
  broken= true;
}

void
mupdf_pdf_renderer_rep::put (const char* s) {
  if (contents == NULL) return;
  fz_try (ctx) { fz_append_string (ctx, contents, s); }
  fz_catch (ctx) { fail ("a page"); }
}

void
mupdf_pdf_renderer_rep::app (const char* fmt, ...) {
  if (contents == NULL) return;
  va_list args;
  va_start (args, fmt);
  fz_try (ctx) { fz_append_vprintf (ctx, contents, fmt, args); }
  fz_catch (ctx) { fail ("a page"); }
  va_end (args);
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
  link_elem= array<int> ();
  link_pos= array<path> ();
  page_videos= array<pdf_link_item> ();
  page_video_file= array<int> ();
  page_video_vars= array<string> ();
  page_video_nm= array<string> ();
  page_video_elem= array<int> ();
  page_video_pos= array<path> ();
  // the whole page is written in the pixels of the renderer
  app ("q\n%g 0 0 %g 0 0 cm\n",
                    (double) default_dpi / dpi, (double) default_dpi / dpi);
  set_origin (0, (SI) (paper_h * dpi * pixel / 2.54));
  set_clipping (0, (SI) ((-dpi * pixel * paper_h) / 2.54),
                (SI) (( dpi * pixel * paper_w) / 2.54), 0);
}

void
mupdf_pdf_renderer_rep::end_page () {
  if (contents == NULL) return;
  tag_close ();
  end_text ();
  while (clip_level-- > 0) put ("Q\n");
  clip_level= 0;
  put ("Q\n");
  // the targets of the links as the bytes which go in the file: a place in
  // the document by the name write_dests gives it, an address as a URI
  array<string> target;
  array<int> local;
  for (int i=0; i<N(links); i++) {
    bool loc= starts (links[i].label, "#");
    local << (loc ? 1 : 0);
    target << (loc ? pdf_text_bytes (links[i].label) : uri_bytes (links[i].label));
  }
  // as pdf_hummus_renderer: a dashed border, drawn only when the loci are
  // to keep their look on paper
  int border= (get_locus_rendering ("locus-on-paper") == "preserve") ? 1 : 0;
  // a tagged PDF: a place for each link among the annotations of the
  // structure, and its description
  link_annot0= N(tag_annots);
  link_text= array<string> ();
  if (tagger != NULL)
    for (int i=0; i<N(links); i++) {
      tag_annots << ((pdf_obj*) NULL);
      tag_annot_page << page_num;
      string l= links[i].label;
      // (PDF/UA: a description, never empty: a link of no target)
      string d= cork_to_utf8 (starts (l, "#")? "Link to " * l (1, N(l)): l);
      if (N(trim_spaces (d)) == 0 || trim_spaces (d) == "Link to") d= "Link";
      link_text << (d * string ((char) 0));
    }
  video_annot0= N(tag_annots);
  if (tagger != NULL)
    for (int i=0; i<N(page_videos); i++) {
      tag_annots << ((pdf_obj*) NULL);
      tag_annot_page << page_num;
    }
  pdf_obj* page= NULL;
  fz_var (page);
  fz_try (ctx) {
    fz_rect mediabox= fz_make_rect (0, 0, (float) width, (float) height);
    page= pdf_add_page (ctx, doc, mediabox, 0, resources, contents);
    write_links (page, target, local, border);
    write_videos (page);
    if (tagger != NULL) {
      // the marked contents of the page are those of entry page_num of the
      // parent tree, and the annotations are visited in the order of the
      // structure (PDF/UA)
      pdf_dict_put_int (ctx, page, PDF_NAME(StructParents), page_num);
      pdf_dict_puts_drop (ctx, page, "Tabs", pdf_new_name (ctx, "S"));
    }
    pdf_insert_page (ctx, doc, -1, page);
  }
  fz_catch (ctx) {
    pdf_drop_obj (ctx, page); page= NULL;
    fail ("a page");
  }
  if (page != NULL) pages << page;
  if (tags != NULL) tags->end_page ();
  if (tagger != NULL)
    for (int i=0; i<N(links) && i<N(link_elem); i++)
      if (link_elem[i] >= 0 && tag_annots[link_annot0 + i] != NULL) {
        tg ()->add_annotation (link_elem[i], link_annot0 + i, page_num,
                               link_pos[i]);
      }
  if (tagger != NULL)
    for (int i=0; i<N(page_videos) && i<N(page_video_elem); i++)
      if (page_video_elem[i] >= 0 && tag_annots[video_annot0 + i] != NULL) {
        tg ()->add_annotation (page_video_elem[i], video_annot0 + i,
                               page_num, page_video_pos[i]);
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
  app ("%g %g %g rg\n",
                    r/255.0, g/255.0, b/255.0);
}

void
mupdf_pdf_renderer_rep::select_stroke (color c) {
  if (has_stroke && c == cur_stroke) return;
  cur_stroke= c; has_stroke= true;
  int r, g, b, a; get_rgb_color (c, r, g, b, a);
  select_alpha (a);
  app ("%g %g %g RG\n",
                    r/255.0, g/255.0, b/255.0);
}

void
mupdf_pdf_renderer_rep::select_width (SI w) {
  double lw= to_w (w);
  if (lw == cur_width) return;
  cur_width= lw;
  app ("%g w\n", lw);
}

// the transparency of a colour is a graphics state of its own in a PDF
void
mupdf_pdf_renderer_rep::select_alpha (int a) {
  if (a == cur_alpha) return;
  cur_alpha= a;
  app ("/GS%d gs\n", alpha_state (a));
}

// the ExtGState of an alpha, made the first time it is asked for
int
mupdf_pdf_renderer_rep::alpha_state (int a) {
  int num;
  if (alpha_gs->contains (a)) num= alpha_gs (a);
  else {
    num= n_alpha++;
    char cnm[32];
    snprintf (cnm, sizeof (cnm), "GS%d", (int) (num));
    pdf_obj* gs= NULL;
    fz_var (gs);
    fz_try (ctx) {
      gs= pdf_new_dict (ctx, doc, 3);
      pdf_dict_put (ctx, gs, PDF_NAME(Type), PDF_NAME(ExtGState));
      pdf_dict_put_real (ctx, gs, PDF_NAME(ca), a / 255.0);
      pdf_dict_put_real (ctx, gs, PDF_NAME(CA), a / 255.0);
      pdf_obj* g= gs; gs= NULL;   // pdf_add_object_drop drops it, error or not
      pdf_dict_puts_drop (ctx, res_gs, cnm, pdf_add_object_drop (ctx, doc, g));
    }
    fz_catch (ctx) {
      pdf_drop_obj (ctx, gs);
      fail ("a transparency");   // a /GS the page names and the file lacks
    }
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
  app ("%g %g %g %g %g %g cm\n",
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
    app ("q\n%g %g %g %g re W n\n",
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
  tag_mark (1);
  if (contents == NULL) return;
  end_text ();
  select_stroke (pen->get_color ());
  select_width (pen->get_width ());
  app ("%g %g m %g %g l S\n",
                    to_x (x1), to_y (y1), to_x (x2), to_y (y2));
}

void
mupdf_pdf_renderer_rep::lines (array<SI> x, array<SI> y) {
  tag_mark (1);
  if (contents == NULL || N(x) == 0 || N(x) != N(y)) return;
  end_text ();
  select_stroke (pen->get_color ());
  select_width (pen->get_width ());
  app ("%g %g m\n", to_x (x[0]), to_y (y[0]));
  for (int i=1; i<N(x); i++)
    app ("%g %g l\n", to_x (x[i]), to_y (y[i]));
  put ("S\n");
}

void
mupdf_pdf_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  tag_mark (1);
  if (contents == NULL || x1 >= x2 || y1 >= y2) return;
  end_text ();
  select_fill (pen->get_color ());
  app ("%g %g %g %g re f\n",
                    to_x (x1), to_y (y1),
                    to_x (x2) - to_x (x1), to_y (y2) - to_y (y1));
}

void
mupdf_pdf_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  tag_mark (1);
  if (contents == NULL || x1 >= x2 || y1 >= y2) return;
  end_text ();
  select_fill (bgb->get_color ());
  app ("%g %g %g %g re f\n",
                    to_x (x1), to_y (y1),
                    to_x (x2) - to_x (x1), to_y (y2) - to_y (y1));
}

void
mupdf_pdf_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  tag_mark (1);
  (void) convex;
  if (contents == NULL || N(x) < 2 || N(x) != N(y)) return;
  end_text ();
  select_fill (pen->get_color ());
  app ("%g %g m\n", to_x (x[0]), to_y (y[0]));
  for (int i=1; i<N(x); i++)
    app ("%g %g l\n", to_x (x[i]), to_y (y[i]));
  put ("h f\n");
}

// an arc of the ellipse inscribed in the box, in 1/64 degrees as in X11
void
mupdf_pdf_renderer_rep::arc_path (double cx, double cy, double rx, double ry,
                                  double a0, double a1, bool move) {
  const int n= 24;
  for (int i=0; i<=n; i++) {
    double a= a0 + (a1 - a0) * i / n;
    double px= cx + rx * cos (a), py= cy + ry * sin (a);
    app ("%g %g %s\n", px, py, (i == 0 && move) ? "m" : "l");
  }
}

void
mupdf_pdf_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  tag_mark (1);
  if (contents == NULL) return;
  end_text ();
  select_stroke (pen->get_color ());
  select_width (pen->get_width ());
  double cx= (to_x (x1) + to_x (x2)) / 2, cy= (to_y (y1) + to_y (y2)) / 2;
  double rx= (to_x (x2) - to_x (x1)) / 2, ry= (to_y (y2) - to_y (y1)) / 2;
  arc_path (cx, cy, rx, ry,
            alpha * M_PI / (64*180), (alpha+delta) * M_PI / (64*180), true);
  put ("S\n");
}

void
mupdf_pdf_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta) {
  tag_mark (1);
  if (contents == NULL) return;
  end_text ();
  select_fill (pen->get_color ());
  double cx= (to_x (x1) + to_x (x2)) / 2, cy= (to_y (y1) + to_y (y2)) / 2;
  double rx= (to_x (x2) - to_x (x1)) / 2, ry= (to_y (y2) - to_y (y1)) / 2;
  app ("%g %g m\n", cx, cy);
  arc_path (cx, cy, rx, ry,
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
    fz_font* f= NULL;
    fz_var (f);
    fz_try (ctx) {
      f= fz_new_font_from_file (ctx, NULL, path, 0, 0);
      if (!pdf_font_writing_supported (ctx, f)) { fz_drop_font (ctx, f); f= NULL; }
    }
    fz_catch (ctx) { fz_drop_font (ctx, f); f= NULL; }
    it.font= f;
  }
  if (it.font != NULL) {
    it.simple= is_type1_file (u);
    it.path= concretize (u);
    it.tex_name= basename (u);
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
      else {
        it.obj= pdf_add_cid_font (ctx, doc, it.font);
        // (its character map is written again by write_fonts, see
        // fix_to_unicode)
      }
      char cnm[32];
      snprintf (cnm, sizeof (cnm), "F%d", (int) (it.num));
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
    it.tex_name= fn->res_name;
    it.chunk= ch / 256;
    for (int i=0; i<256; i++) it.gid << -1;
    fz_try (ctx) {
      it.obj= pdf_add_new_dict (ctx, doc, 8);
      char cnm[32];
      snprintf (cnm, sizeof (cnm), "F%d", (int) (it.num));
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

// The span is in the text object, as the glyphs it covers (draw does not
// end the text object)
void
mupdf_pdf_renderer_rep::begin_actual_text (string s) {
  if (contents == NULL) return;
  tag_mark (0);
  if (tagger != NULL) tg ()->span++;   // no new block in the span
  if (!in_text) {
    put ("BT\n"); in_text= true; cur_font= -1; text_x= text_y= 0;
  }
  c_string hex (utf8_to_pdf_hex_string (s));
  app ("/Span << /ActualText %s >> BDC\n", (const char*) hex);
}

void
mupdf_pdf_renderer_rep::end_actual_text () {
  if (contents == NULL) return;
  if (tagger != NULL && tg ()->span > 0) tg ()->span--;
  put ("EMC\n");
}

void
mupdf_pdf_renderer_rep::draw_invisible (int c, font_glyphs fn, SI x, SI y,
                                        double hscale) {
  tag_mark (0);
  if (contents == NULL) return;
  if (!in_text) {
    put ("BT\n"); in_text= true; cur_font= -1; text_x= text_y= 0;
  }
  put ("3 Tr\n");
  if (hscale != 1.0) app ("%g Tz\n", 100.0 * hscale);
  draw (c, fn, x, y);
  if (hscale != 1.0) put ("100 Tz\n");
  put ("0 Tr\n");
}

void
mupdf_pdf_renderer_rep::draw (int ch, font_glyphs fn, SI x, SI y) {
  if (contents == NULL) return;
  glyph gl= fn->get (ch);
  if (is_nil (gl)) return;
  tag_mark (0);
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
    app ("/F%d %g Tf\n", font_list[k].num, size);
    cur_font= k; cur_size= size;
  }
  double px= to_x (x), py= to_y (y);
  app ("%g %g Td\n", px - text_x, py - text_y);
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
  // A glyph drawn by its number stands for the character the font recorded
  // for it (a larger variant of a symbol), or for none: the further pieces
  // of an assembly, which then add nothing to the text (as in
  // pdf_hummus_renderer)
  bool blank= false;
  if (!font_list[k].simple && !font_list[k].t3)
    font_list[k].used ((int) gl->index)= true;
  if (ch >= 0xc000000 && !font_list[k].simple && !font_list[k].t3) {
    int code= native_glyph_get_unicode (fn->res_name, ch - 0xc000000);
    if (code > 0) font_list[k].drawn ((int) gl->index)= code;
    else if (code == 0) blank= (N(lig) == 0);
    else {
      // nothing recorded (the variants of the MATH table, #943): the name
      // of the glyph says it often enough (uni27E8.size1, braceleft.v2)
      int u= glyph_name_unicode (font_list[k], (int) gl->index);
      if (u > 0) font_list[k].named ((int) gl->index)= u;
    }
  }
  if (N(lig) > 0) {
    c_string cl (lig);
    app ("/Span << /ActualText (%s) >> BDC\n",
                      (const char*) cl);
  }
  else if (blank) put ("/Span << /ActualText () >> BDC\n");
  if (font_list[k].simple || font_list[k].t3) {
    // one byte per glyph; the code is the one TeXmacs uses, and the
    // Differences array will say which glyph it selects
    int code= ch & 255;
    if (font_list[k].gid[code] < 0)
      font_list[k].gid[code]= font_list[k].t3 ? ch : (int) gl->index;
    app ("<%02x> Tj\n", code);
  }
  else
    app ("<%04x> Tj\n", ((int) gl->index) & 0xffff);
  if (N(lig) > 0 || blank) put ("EMC\n");
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

// Whether u is a form of another character which fonts draw with the same
// glyph: the Kangxi and CJK radicals (U+2F47 for U+65E5), the compatibility
// ideographs and the CJK compatibility forms, and the private use codes,
// which a font may give to a glyph which is also a real character
static bool
compatibility_char (int u) {
  return (u >= 0x2e80 && u <= 0x2fdf) || (u >= 0xf900 && u <= 0xfaff) ||
         (u >= 0xfe30 && u <= 0xfe4f) || (u >= 0x2f800 && u <= 0x2fa1f) ||
         (u >= 0xe000 && u <= 0xf8ff) || u >= 0xf0000;
}

// The character map of a CID font (glyph to characters) as MuPDF writes it
// gives the characters beyond U+FFFF as <1f600>, which no reader takes:
// they are in UTF-16, two units (the emoji, the mathematical letters). And
// it takes the first character of a glyph, which for the ideographs of a
// CJK font is the radical of the same shape (U+2F47 rather than U+65E5), so
// that the text of the PDF cannot be searched. And it knows nothing of the
// glyphs which the cmap does not reach, such as the larger variants of the
// symbols of a math font, which TeXmacs draws by their number (see draw).
// The map is written again in these cases, once the pages are written,
// from the cmap of the font, the character proper taken, and the glyphs
// drawn by their number.
void
mupdf_pdf_renderer_rep::fix_to_unicode (pdf_font_item& it) {
  if (it.font == NULL || it.obj == NULL) return;
  FT_Face face= (FT_Face) fz_font_ft_face (ctx, it.font);
  if (face == NULL) return;
  hashmap<int,int> uni (0);
  array<int> gids;
  bool beyond= false, changed= false;
  fz_ft_lock (ctx);
  FT_UInt gid= 0;
  FT_ULong ucs= FT_Get_First_Char (face, &gid);
  while (gid > 0) {
    if (gid < (FT_UInt) face->num_glyphs && gid <= 0xffff && ucs >= 0x20) {
      if (!uni->contains ((int) gid)) {
        uni ((int) gid)= (int) ucs;
        gids << (int) gid;
      }
      else if (compatibility_char (uni [(int) gid]) &&
               !compatibility_char ((int) ucs)) {
        uni ((int) gid)= (int) ucs;
        changed= true;
      }
    }
    ucs= FT_Get_Next_Char (face, ucs, &gid);
  }
  fz_ft_unlock (ctx);
  // the characters the font recorded for the glyphs drawn by their number,
  // which also replace a private use code of the cmap (the variants of
  // KpMath: lparen.v4 is U+E327); the characters of the names, a guess,
  // only for the glyphs which the cmap does not reach
  for (int pass=0; pass<2; pass++) {
    hashmap<int,int>& m= (pass == 0? it.drawn: it.named);
    iterator<int> dit= iterate (m);
    while (dit->busy ()) {
      int g= dit->next (), u= m [g];
      if (g <= 0 || g > 0xffff) continue;
      if (!uni->contains (g)) gids << g;
      else if (pass == 1 || compatibility_char (u) ||
               !compatibility_char (uni [g])) continue;
      uni (g)= u;
      changed= true;
    }
  }
  // a tagged PDF maps every glyph drawn (PDF/UA), those of no character
  // known (#943) to U+FFFD, as readers took them
  if (tagger != NULL) {
    iterator<int> uit= iterate (it.used);
    while (uit->busy ()) {
      int g= uit->next ();
      if (g <= 0 || g > 0xffff || uni->contains (g)) continue;
      uni (g)= 0xfffd; gids << g; changed= true;
    }
  }
  for (int i=0; i<N(gids); i++)
    if (uni [gids[i]] > 0xffff) beyond= true;
  if (!beyond && !changed) return;
  // only the glyphs drawn: the map of every glyph of a CJK font is tens of
  // thousands of entries, for the glyphs which subsetting takes out
  if (N(it.used) > 0) {
    array<int> kept;
    for (int i=0; i<N(gids); i++)
      if (it.used->contains (gids[i])) kept << gids[i];
    gids= kept;
    if (N(gids) == 0) return;
  }
  string cmap;
  cmap << "/CIDInit /ProcSet findresource begin 12 dict begin begincmap\n"
       << "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) "
       << "/Supplement 0 >> def\n"
       << "/CMapName /Adobe-Identity-UCS def /CMapType 2 def\n"
       << "1 begincodespacerange <0000> <FFFF> endcodespacerange\n";
  for (int i= 0; i < N(gids); i += 100) {
    int n= min (100, N(gids) - i);
    cmap << as_string (n) << " beginbfchar\n";
    for (int j= i; j < i + n; j++) {
      int u= uni [gids[j]];
      string dst;
      if (u > 0xffff) {
        int v= u - 0x10000;
        dst= as_hexadecimal (0xd800 + (v >> 10), 4) *
             as_hexadecimal (0xdc00 + (v & 0x3ff), 4);
      }
      else dst= as_hexadecimal (u, 4);
      cmap << "<" << as_hexadecimal (gids[j], 4) << "> <" << dst << ">\n";
    }
    cmap << "endbfchar\n";
  }
  cmap << "endcmap CMapName currentdict /CMap defineresource pop end end\n";
  fz_buffer* cb= fz_new_buffer_from_copied_data (ctx,
                   (unsigned char*) &cmap[0], (size_t) N(cmap));
  fz_try (ctx) {
    pdf_dict_put_drop (ctx, it.obj, PDF_NAME(ToUnicode),
                       pdf_add_stream (ctx, doc, cb, NULL, 0));
  }
  fz_always (ctx) fz_drop_buffer (ctx, cb);
  fz_catch (ctx) {
    convert_warning << "mupdf_pdf_renderer: no character map for "
                    << it.path << LF;
  }
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
    if (it.font != NULL && !it.simple && it.obj != NULL) {
      // a TrueType CID font says how its CIDs are glyphs (PDF/UA)
      // (braced: fz_catch is a statement of its own)
      if (tagger != NULL) {
        fz_try (ctx) {
          pdf_obj* df= pdf_array_get (ctx, pdf_dict_get (ctx, it.obj,
                                      PDF_NAME(DescendantFonts)), 0);
          if (df != NULL &&
              pdf_name_eq (ctx, pdf_dict_get (ctx, df, PDF_NAME(Subtype)),
                           PDF_NAME(CIDFontType2)) &&
              pdf_dict_get (ctx, df, PDF_NAME(CIDToGIDMap)) == NULL)
            pdf_dict_put (ctx, df, PDF_NAME(CIDToGIDMap), PDF_NAME(Identity));
        }
        fz_catch (ctx) {}
      }
      fix_to_unicode (it); continue; }
    if (it.font == NULL || !it.simple || it.obj == NULL) continue;
    int first= 256, last= -1;
    for (int c=0; c<256; c++)
      if (it.gid[c] >= 0) { if (c < first) first= c; last= c; }
    if (last < 0) continue;
    // The names of the glyphs the codes select, asked of the font itself
    // (see glyph_name), first: the fz_try of write_encoding must create
    // nothing with a destructor. They end with a 0, to be C strings as
    // they are.
    array<int> ftgid;    // what FreeType calls the glyph of each code
    array<string> gname; // its name, "" if it has none
    array<string> keep;  // the names, for the subsetter
    for (int c=0; c<256; c++) { ftgid << -1; gname << string (); }
    for (int c=first; c<=last; c++) {
      if (it.gid[c] < 0) continue;
      // with no built in encoding for the code, trust the index of TeXmacs
      int g;
      string nm= glyph_name (it, c, it.gid[c], g);
      if (N(nm) == 0) continue;   // no name: the built in encoding stands
      ftgid[c]= g;
      gname[c]= nm * string ((char) 0);
      keep << nm;
    }
    write_encoding (it, first, last, gname);
    write_tex_to_unicode (it, first, last, gname);
    // The font program, cut down to the glyphs which are used. MuPDF
    // subsets TrueType and CFF only, so a Type 1 would go in whole -- some
    // eighty kilobytes for a handful of letters; mupdf_t1_subset is
    // pdfTeX's writet1.c, which knows how to do it.
    if (N(keep) > 0) subset_type1 (it, keep);
    write_widths (it, first, last, ftgid);
  }
}

// The /Encoding of a simple font: the glyph names of the codes in use.
// (Apart from write_fonts, whose loop GCC 13 could not compile with them:
// an internal error of its -Wmaybe-uninitialized analysis.)
void
mupdf_pdf_renderer_rep::write_encoding (pdf_font_item& it, int first,
                                        int last, array<string>& gname) {
  pdf_obj* enc= NULL;
  fz_var (enc);
  fz_try (ctx) {
    enc= pdf_new_dict (ctx, doc, 2);
    pdf_dict_put (ctx, enc, PDF_NAME(Type), PDF_NAME(Encoding));
    pdf_obj* diff= pdf_dict_put_array (ctx, enc, PDF_NAME(Differences), 16);
    int prev= -2;
    for (int c=first; c<=last; c++) {
      if (N(gname[c]) == 0) continue;
      if (c != prev + 1) pdf_array_push_int (ctx, diff, c);
      pdf_array_push_name (ctx, diff, &gname[c][0]);
      prev= c;
    }
    pdf_obj* e= enc; enc= NULL;   // pdf_add_object_drop drops it, error or not
    pdf_dict_put_drop (ctx, it.obj, PDF_NAME(Encoding),
                       pdf_add_object_drop (ctx, doc, e));
  }
  fz_catch (ctx) {
    pdf_drop_obj (ctx, enc);
    convert_warning << "MuPDF could not write the encoding of "
                    << it.path << ": " << fz_caught_message (ctx) << LF;
  }
}

// The character of a glyph after its name, without its suffix (uni27E8.size1
// is U+27E8, braceleft.v2 is {), -1 if the name says nothing
int
mupdf_pdf_renderer_rep::glyph_name_unicode (pdf_font_item& it, int gid) {
  if (it.font == NULL) return -1;
  char nm[64];
  nm[0]= 0;
  fz_try (ctx) fz_get_glyph_name (ctx, it.font, gid, nm, (int) sizeof (nm));
  fz_catch (ctx) nm[0]= 0;
  nm[sizeof (nm) - 1]= 0;
  char* dot= strchr (nm, '.');
  if (dot != NULL) *dot= 0;
  // A font without names gives the number of the glyph, which MuPDF would
  // read as a decimal character code, as it reads a65 as A: such names say
  // nothing. An unknown name gives U+FFFD. (The strict lookup knows no
  // uniXXXX names.)
  if (nm[0] == 0 || (nm[0] >= '0' && nm[0] <= '9')) return -1;
  if (nm[0] == 'a' && nm[1] >= '0' && nm[1] <= '9') return -1;
  // g or G and hexadecimal digits: MuPDF reads a character code (g12 is /)
  if (nm[0] == 'g' || nm[0] == 'G') {
    int i= 1;
    while (nm[i] != 0 && isxdigit ((unsigned char) nm[i])) i++;
    if (i > 1 && nm[i] == 0) return -1;
  }
  // the legacy names of the Adobe glyph list for the parenthesized small
  // letters (lparen is U+24A7) are what math fonts call their parentheses
  int legacy= fz_unicode_from_glyph_name_strict (nm);
  if (legacy >= 0x249c && legacy <= 0x24b5) return -1;
  int u= fz_unicode_from_glyph_name (nm);
  return (u >= 0x20 && u != 0xfffd)? u: -1;
}

// The characters of the codes of a TeX math font (cmmi, cmsy, cmex, msbm...)
// or of a Cyrillic TeX font (T2A), as a /ToUnicode CMap: the names of their glyphs (summationdisplay,
// parenleftbig...) mean nothing to a reader, which then gave U+FFFD for
// the big operators and delimiters. Text fonts keep their glyph names,
// which readers understand.
void
mupdf_pdf_renderer_rep::write_tex_to_unicode (pdf_font_item& it, int first,
                                              int last, array<string>& gname) {
  array<int> tex;
  if (is_t2a_font_file (url_system (it.path))) {
    // the Cyrillic fonts name their glyphs after the Latin-1 letters at
    // the same positions: a reader took å for е
    for (int c=0; c<256; c++) tex << tex_glyph_unicode (c, true);
  }
  else tex= tex_math_unicode (it.tex_name);
  if (N(tex) == 0 && tagger != NULL) {
    // a tagged PDF maps every code used (PDF/UA): those of a font of no
    // table after the names of their glyphs, which a reader would read,
    // also uniXXXX, which veraPDF does not
    for (int c=0; c<256; c++) tex << -1;
    for (int c=first; c<=last && c<N(gname); c++) {
      if (it.gid[c] < 0 || N(gname[c]) <= 1) continue;
      string nm= gname[c] (0, N(gname[c]) - 1);
      int dot= search_forwards (".", nm);
      if (dot > 0) nm= nm (0, dot);
      // (aNN is a number for MuPDF, which says nothing)
      if (N(nm) >= 2 && nm[0] == 'a' && is_digit (nm[1])) continue;
      c_string cn (nm);
      int u= fz_unicode_from_glyph_name (cn);
      if (u >= 0x20 && u != 0xfffd && !(u >= 0xe000 && u <= 0xf8ff))
        tex[c]= u;
    }
  }
  if (N(tex) == 0) return;
  // (a position without a single character, below U+0020, is left out)
  for (int c=0; c<N(tex); c++) if (tex[c] < 0x20) tex[c]= -1;
  // a tagged PDF maps every code used (PDF/UA): a code of no character in
  // the table, the character of its glyph's name, which a reader would
  // read (a vector accent, the symbols of msam), else U+FFFD (the pieces
  // of the horizontal braces of cmex have none)
  if (tagger != NULL)
    for (int c=first; c<=last && c<N(tex); c++)
      if (it.gid[c] >= 0 && tex[c] < 0) {
        int u= -1;
        // (a piece is no character: mapsto of cmsy is the bar of the
        // arrows which it begins, and varshortmid)
        bool piece= (c < N(gname) && gname[c] (0, max (0, N(gname[c]) - 1))
                     == "mapsto");
        if (piece) { tex[c]= 0xfffd; continue; }
        if (c < N(gname) && N(gname[c]) > 1)
          u= pdf_glyph_name_unicode (gname[c] (0, N(gname[c]) - 1));
        if (u < 0x20 || u == 0xfffd) {
          // (aNN is a number for MuPDF, which says nothing: the line
          // font; nor a private use character)
          string nm= (c < N(gname) && N(gname[c]) > 1?
                      gname[c] (0, N(gname[c]) - 1): string (""));
          bool number= (N(nm) >= 2 && nm[0] == 'a' && is_digit (nm[1]));
          if (N(nm) > 0 && !number) {
            c_string cn (nm);
            u= fz_unicode_from_glyph_name (cn);
          }
        }
        if (u >= 0xe000 && u <= 0xf8ff) u= 0xfffd;
        tex[c]= (u >= 0x20 && u != 0xfffd)? u: 0xfffd;
      }
  int nused= 0;
  for (int c=first; c<=last; c++)
    if (it.gid[c] >= 0 && c < N(tex) && tex[c] >= 0) nused++;
  if (nused == 0) return;
  string cmap;
  cmap << "/CIDInit /ProcSet findresource begin 12 dict begin begincmap\n"
       << "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) "
       << "/Supplement 0 >> def\n"
       << "/CMapName /Adobe-Identity-UCS def /CMapType 2 def\n"
       << "1 begincodespacerange <00> <FF> endcodespacerange\n";
  int done= 0;
  for (int c=first; c<=last; c++) {
    if (it.gid[c] < 0 || c >= N(tex) || tex[c] < 0) continue;
    if (done % 100 == 0)
      cmap << as_string (min (100, nused - done)) << " beginbfchar\n";
    cmap << "<" << as_hexadecimal (c, 2) << "> <" << utf16_hex (tex[c])
         << ">\n";
    done++;
    if (done % 100 == 0 || done == nused) cmap << "endbfchar\n";
  }
  cmap << "endcmap CMapName currentdict /CMap defineresource pop end end\n";
  fz_buffer* cb= fz_new_buffer_from_copied_data (ctx,
                   (unsigned char*) &cmap[0], (size_t) N(cmap));
  fz_try (ctx) {
    pdf_dict_put_drop (ctx, it.obj, PDF_NAME(ToUnicode),
                       pdf_add_stream (ctx, doc, cb, NULL, 0));
  }
  fz_always (ctx) fz_drop_buffer (ctx, cb);
  fz_catch (ctx) {
    convert_warning << "mupdf_pdf_renderer: no character map for "
                    << it.path << LF;
  }
}

// The /Widths of a simple font, in thousandths of the size
void
mupdf_pdf_renderer_rep::write_widths (pdf_font_item& it, int first, int last,
                                      array<int>& ftgid) {
  fz_try (ctx) {
    pdf_dict_put_int (ctx, it.obj, PDF_NAME(FirstChar), first);
    pdf_dict_put_int (ctx, it.obj, PDF_NAME(LastChar), last);
    pdf_obj* w= pdf_dict_put_array (ctx, it.obj, PDF_NAME(Widths),
                                    last - first + 1);
    for (int c=first; c<=last; c++) {
      double adv= 0;
      if (ftgid[c] >= 0)
        adv= fz_advance_glyph (ctx, it.font, ftgid[c], 0) * 1000.0;
      pdf_array_push_real (ctx, w, adv);
    }
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF could not write the widths of "
                    << it.path << ": " << fz_caught_message (ctx) << LF;
  }
}

// Replace the font program of a simple font by a subset of itself which
// has only the glyphs in `keep`, and rename the font as a subset is named.
void
mupdf_pdf_renderer_rep::subset_type1 (pdf_font_item& it, array<string> keep) {
  pdf_obj* fdesc= NULL;
  pdf_obj* ff= NULL;
  fz_try (ctx) {
    fdesc= pdf_dict_get (ctx, it.obj, PDF_NAME(FontDescriptor));
    if (fdesc != NULL) ff= pdf_dict_get (ctx, fdesc, PDF_NAME(FontFile));
  }
  fz_catch (ctx) { ff= NULL; }
  if (ff == NULL) return;   // not a Type 1 after all
  // the subsetter is C, with an error handling of its own which unwinds
  // only its own frames (mupdf_type1.c); it wants C strings
  c_string path (it.path);
  array<string> kz;
  for (int i=0; i<N(keep); i++) kz << (keep[i] * string ((char) 0));
  const char** names= tm_new_array<const char*> (N(kz));
  for (int i=0; i<N(kz); i++) names[i]= &kz[i][0];
  int size= 0, l1= 0, l2= 0, l3= 0;
  char* psname= NULL;
  const char* err= NULL;
  unsigned char* sub= mupdf_t1_subset (path, names, N(kz), &size,
                                       &l1, &l2, &l3, &psname, &err);
  tm_delete_array (names);
  if (sub == NULL) {
    convert_warning << "the Type 1 font " << it.path
                    << " could not be subsetted: "
                    << string (err == NULL ? "?" : err) << LF;
    return;
  }
  fz_buffer* buf= NULL;
  fz_var (buf);
  fz_try (ctx) {
    buf= fz_new_buffer_from_copied_data (ctx, sub, (size_t) size);
    pdf_update_stream (ctx, doc, ff, buf, 0);
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
  fz_always (ctx) { fz_drop_buffer (ctx, buf); }
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
  // Everything TeXmacs computes is computed first, and MuPDF is given the
  // results in the fz_try below, which creates nothing with a destructor:
  // a throw is a longjmp, which would skip it.
  int n= last - first + 1;
  array<string> proc (n);   // the content stream of each glyph, "" if none
  array<double> adv (n);    // the widths
  int b0= 0, b1= 0, b2= 0, b3= 0;   // the box of the whole font
  for (int c=first; c<=last; c++) {
    adv[c-first]= 0;
    if (it.gid[c] < 0) continue;
    glyph gl= it.fn->get (it.gid[c]);
    if (is_nil (gl)) continue;
    adv[c-first]= gl->lwidth;
    int llx= -gl->xoff, lly= gl->yoff - gl->height + 1;
    int urx= gl->width - gl->xoff + 1, ury= gl->yoff + 1;
    int w= gl->width, h= gl->height;
    if (b2 <= b0) { b0= llx; b1= lly; b2= urx; b3= ury; }
    else {
      if (llx < b0) b0= llx; if (lly < b1) b1= lly;
      if (urx > b2) b2= urx; if (ury > b3) b3= ury;
    }
    // d1 says the glyph is a mask, so the colour is the one in force
    string& cs= proc[c-first];
    cs << as_string ((int) gl->lwidth) << " 0 " << as_string (llx) << " "
       << as_string (lly) << " " << as_string (urx) << " "
       << as_string (ury) << " d1\n";
    if (w > 0 && h > 0) {
      cs << "q\n" << as_string (w) << " 0 0 " << as_string (h) << " "
         << as_string (llx) << " " << as_string (lly) << " cm\n"
         << "BI\n/W " << as_string (w) << "\n/H " << as_string (h) << "\n"
         << "/BPC 1 /F /AHx /D [0.0 1.0] /IM true\nID\n";
      static const char* hex= "0123456789ABCDEF";
      int cur= 0, count= 0;
      for (int j=0; j<h; j++)
        for (int i=0; i < ((w + 7) & (-8)); i++) {
          cur= cur << 1;
          if (i < w && gl->get_x (i, j) == 0) cur++;
          count++;
          if (count == 4) { cs << hex[cur]; cur= 0; count= 0; }
        }
      cs << ">\nEI\nQ\n";
    }
  }
  // the codes of a Type 3 font mean nothing to a reader: a CMap says
  // which character each of them stands for, so the text can be found
  string cmap;
  cmap << "/CIDInit /ProcSet findresource begin 12 dict begin begincmap\n"
       << "/CIDSystemInfo << /Registry (TeXmacs) /Ordering (Type3) "
       << "/Supplement 0 >> def\n"
       << "/CMapName /TeXmacs-Type3 def /CMapType 2 def\n"
       << "1 begincodespacerange <" << as_hexadecimal (first, 2)
       << "> <" << as_hexadecimal (last, 2) << "> endcodespacerange\n";
  // (the glyphs of a TeX math font are positions: their characters come
  // from the encoding of the font, see tex_math_unicode)
  array<int> tex= tex_math_unicode (it.tex_name);
  int nused= 0;
  for (int c=first; c<=last; c++) if (it.gid[c] >= 0) nused++;
  cmap << as_string (nused) << " beginbfchar\n";
  for (int c=first; c<=last; c++)
    if (it.gid[c] >= 0) {
      int u= it.gid[c];
      if (u >= 0 && u < N(tex) && tex[u] >= 0) u= tex[u];
      // (a tagged PDF: a text font of no table is of the OT1 encoding,
      // and no position is a control, PDF/UA)
      else if (tagger != NULL) u= tex_ot1_unicode (u);
      if (tagger != NULL && (u < 0x20 || u == 0xfeff || u == 0xfffe))
        u= 0xfffd;
      cmap << "<" << as_hexadecimal (c, 2) << "> <"
           << utf16_hex (u) << ">\n";
    }
  cmap << "endbfchar\nendcmap CMapName currentdict /CMap defineresource "
       << "pop end end\n";

  fz_try (ctx) {
    pdf_obj* procs= pdf_new_dict (ctx, doc, n);
    pdf_obj* enc= pdf_new_dict (ctx, doc, 2);
    pdf_dict_put (ctx, enc, PDF_NAME(Type), PDF_NAME(Encoding));
    pdf_obj* diff= pdf_dict_put_array (ctx, enc, PDF_NAME(Differences), 16);
    int prev= -2;
    for (int c=first; c<=last; c++) {
      string& cs= proc[c-first];
      if (N(cs) == 0) continue;
      char cnm[32];
      snprintf (cnm, sizeof (cnm), "ch%d", c);
      fz_buffer* buf= fz_new_buffer_from_copied_data (ctx,
                        (unsigned char*) &cs[0], (size_t) N(cs));
      pdf_obj* ref= pdf_add_stream (ctx, doc, buf, NULL, 0);
      fz_drop_buffer (ctx, buf);
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
    pdf_obj* w= pdf_dict_put_array (ctx, it.obj, PDF_NAME(Widths), n);
    for (int k=0; k<n; k++) pdf_array_push_real (ctx, w, adv[k]);
    pdf_dict_put_drop (ctx, it.obj, PDF_NAME(CharProcs),
                       pdf_add_object_drop (ctx, doc, procs));
    pdf_dict_put_drop (ctx, it.obj, PDF_NAME(Encoding),
                       pdf_add_object_drop (ctx, doc, enc));
    pdf_obj* res= pdf_dict_put_dict (ctx, it.obj, PDF_NAME(Resources), 1);
    pdf_obj* ps= pdf_dict_put_array (ctx, res, PDF_NAME(ProcSet), 2);
    pdf_array_push (ctx, ps, PDF_NAME(PDF));
    pdf_array_push (ctx, ps, PDF_NAME(ImageB));
    fz_buffer* cb= fz_new_buffer_from_copied_data (ctx,
                     (unsigned char*) &cmap[0], (size_t) N(cmap));
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
      char cnm[32];
      snprintf (cnm, sizeof (cnm), "Im%d", (int) (num));
      double x0= to_x (x) - gl->xoff, y0= to_y (y) - h + gl->yoff;
      app ("q %g 0 0 %g %g %g cm /%s Do Q\n",
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
  char cnm[32];
  snprintf (cnm, sizeof (cnm), "Im%d", (int) (num));
  pdf_dict_puts_drop (ctx, res_xobj, cnm, ref);
  xobj_num (key)= num;
  return num;
}

void
mupdf_pdf_renderer_rep::draw_picture (picture p, SI x, SI y, int alpha) {
  tag_mark (2);
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
      char cnm[32];
      snprintf (cnm, sizeof (cnm), "Im%d", (int) (num));
      double x0= to_x (x) - rep->ox, y0= to_y (y) - rep->oy;
      app ("q %g 0 0 %g %g %g cm /%s Do Q\n",
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

// The layers of a figure (optional content, /OC in its content) go with
// it: the objects which name them come over with its resources, but whether
// a layer is seen is said in the catalogue of the figure (/OCProperties),
// and without it a layer which the figure hides by default was drawn -- in
// every reader. Its layers are added to the list of the document, with the
// same graft map as the resources, so that they are the same objects; the
// ones it hides go into the /OFF of the document, and its order, which is
// what a reader lists, into /Order. Called inside the fz_try of
// embed_image: nothing here has a destructor.
void
mupdf_pdf_renderer_rep::merge_layers (pdf_document* src, pdf_graft_map* map) {
  pdf_obj* sroot= pdf_dict_get (ctx, pdf_trailer (ctx, src), PDF_NAME(Root));
  pdf_obj* socp= pdf_dict_get (ctx, sroot, PDF_NAME(OCProperties));
  pdf_obj* socgs= pdf_dict_get (ctx, socp, PDF_NAME(OCGs));
  int n= pdf_array_len (ctx, socgs);
  if (n == 0) return;
  pdf_obj* sd= pdf_dict_get (ctx, socp, PDF_NAME(D));
  pdf_obj* root= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Root));
  pdf_obj* ocp= pdf_dict_get (ctx, root, PDF_NAME(OCProperties));
  if (ocp == NULL) ocp= pdf_dict_put_dict (ctx, root, PDF_NAME(OCProperties), 2);
  pdf_obj* ocgs= pdf_dict_get (ctx, ocp, PDF_NAME(OCGs));
  if (ocgs == NULL) ocgs= pdf_dict_put_array (ctx, ocp, PDF_NAME(OCGs), n);
  pdf_obj* d= pdf_dict_get (ctx, ocp, PDF_NAME(D));
  if (d == NULL) d= pdf_dict_put_dict (ctx, ocp, PDF_NAME(D), 2);
  pdf_obj* off= pdf_dict_get (ctx, d, PDF_NAME(OFF));
  pdf_obj* order= pdf_dict_get (ctx, d, PDF_NAME(Order));
  // off by default: the ones in /OFF, or, when the figure starts with all
  // of them off (/BaseState /OFF), all but the ones in /ON
  bool base_off= pdf_name_eq (ctx, pdf_dict_get (ctx, sd, PDF_NAME(BaseState)),
                              PDF_NAME(OFF));
  pdf_obj* son= pdf_dict_get (ctx, sd, PDF_NAME(ON));
  pdf_obj* soff= pdf_dict_get (ctx, sd, PDF_NAME(OFF));
  for (int i=0; i<n; i++) {
    pdf_obj* g= pdf_array_get (ctx, socgs, i);
    pdf_array_push_drop (ctx, ocgs, pdf_graft_mapped_object (ctx, map, g));
    bool hidden= base_off ? !pdf_array_contains (ctx, son, g)
                          : pdf_array_contains (ctx, soff, g);
    if (hidden) {
      if (off == NULL) off= pdf_dict_put_array (ctx, d, PDF_NAME(OFF), 4);
      pdf_array_push_drop (ctx, off, pdf_graft_mapped_object (ctx, map, g));
    }
  }
  pdf_obj* sorder= pdf_dict_get (ctx, sd, PDF_NAME(Order));
  for (int i=0; i < pdf_array_len (ctx, sorder); i++) {
    if (order == NULL) order= pdf_dict_put_array (ctx, d, PDF_NAME(Order), 4);
    pdf_array_push_drop (ctx, order,
                         pdf_graft_mapped_object (ctx, map,
                                                  pdf_array_get (ctx, sorder, i)));
  }
}

bool
mupdf_deflate (string in, string& out) {
  fz_context* ctx= mupdf_context ();
  if (N(in) == 0) return false;
  const unsigned char* bytes= (const unsigned char*) &(in[0]);
  size_t len= N(in);
  unsigned char* data= NULL;
  size_t n= 0;
  bool ok= false;
  fz_var (data); fz_var (ok);
  fz_try (ctx) {
    data= fz_new_deflated_data (ctx, &n, bytes, len, FZ_DEFLATE_DEFAULT);
    // a stream of zlib: two bytes, the deflated data, four bytes
    if (data != NULL && n > 6) {
      out= string ((char*) (data + 2), (int) (n - 6));
      ok= true;
    }
  }
  fz_always (ctx) {
    fz_free (ctx, data);
  }
  fz_catch (ctx) {
    ok= false;
  }
  return ok;
}

// The other way: the first page of a PDF as an SVG, for the pictures of a
// document which is exported to a format which has SVG images. The PDF is
// read with load_string, and the SVG is written to a buffer and saved with
// save_string, so that the files of the ramdisc work too.
bool
mupdf_pdf_to_svg (url pdf, url svg) {
  fz_context* ctx= mupdf_context ();
  string data;
  if (load_string (pdf, data, false) || N(data) == 0) return false;
  const unsigned char* bytes= (const unsigned char*) &(data[0]);
  size_t len= N(data);
  bool ok= false;
  string result;
  fz_buffer* in= NULL;
  fz_buffer* buf= NULL;
  fz_document* d= NULL;
  fz_page* page= NULL;
  fz_output* out= NULL;
  fz_device* dev= NULL;
  fz_var (in); fz_var (buf); fz_var (d); fz_var (page); fz_var (out);
  fz_var (dev); fz_var (ok);
  fz_try (ctx) {
    in= fz_new_buffer_from_copied_data (ctx, bytes, len);
    d= fz_open_document_with_buffer (ctx, "application/pdf", in);
    page= fz_load_page (ctx, d, 0);
    fz_rect box= fz_bound_page (ctx, page);
    buf= fz_new_buffer (ctx, 65536);
    out= fz_new_output_with_buffer (ctx, buf);
    dev= fz_new_svg_device (ctx, out, box.x1 - box.x0, box.y1 - box.y0,
                            FZ_SVG_TEXT_AS_PATH, 1);
    fz_run_page (ctx, page, dev, fz_translate (-box.x0, -box.y0), NULL);
    fz_close_device (ctx, dev);
    fz_close_output (ctx, out);
    unsigned char* s= NULL;
    size_t n= fz_buffer_storage (ctx, buf, &s);
    result= string ((char*) s, (int) n);
    ok= n > 0;
  }
  fz_always (ctx) {
    fz_drop_device (ctx, dev);
    fz_drop_output (ctx, out);
    fz_drop_buffer (ctx, buf);
    fz_drop_page (ctx, page);
    fz_drop_document (ctx, d);
    fz_drop_buffer (ctx, in);
  }
  fz_catch (ctx) {
    cout << "TeXmacs] PDF to SVG: " << fz_caught_message (ctx) << LF;
    ok= false;
  }
  if (ok) ok= !save_string (svg, result, false);
  return ok;
}

// An SVG as a PDF, by MuPDF itself, which reads SVG (the screen draws them
// so): the converters of TeXmacs run programs (rsvg-convert, inkscape)
// which a web browser does not have, and a picture of TikZ or of an AI
// answer came out as the "unknown image" sign. Its contents are read with
// load_string, so that a picture of the ramdisc is read too.
static bool
mupdf_svg_to_pdf (fz_context* ctx, url svg, url pdf) {
  string data;
  if (load_string (svg, data, false) || N(data) == 0) {
    cout << "TeXmacs] SVG to PDF: cannot read " << svg << LF;
    return false;
  }
  // made before fz_try: a throw is a longjmp, which skips destructors
  c_string out (concretize (pdf));
  const unsigned char* bytes= (const unsigned char*) &(data[0]);
  size_t len= N(data);
  bool ok= false;
  fz_buffer* buf= NULL;
  fz_document* d= NULL;
  fz_page* page= NULL;
  fz_document_writer* w= NULL;
  fz_var (buf); fz_var (d); fz_var (page); fz_var (w); fz_var (ok);
  // (the handlers of the documents are registered with the context, see
  // mupdf_context)
  fz_try (ctx) {
    buf= fz_new_buffer_from_copied_data (ctx, bytes, len);
    d= fz_open_document_with_buffer (ctx, "image/svg+xml", buf);
    page= fz_load_page (ctx, d, 0);
    fz_rect box= fz_bound_page (ctx, page);
    w= fz_new_pdf_writer (ctx, out, NULL);
    fz_device* dev= fz_begin_page (ctx, w, box);
    fz_run_page (ctx, page, dev, fz_identity, NULL);
    fz_end_page (ctx, w);
    fz_close_document_writer (ctx, w);
    ok= true;
  }
  fz_always (ctx) {
    fz_drop_document_writer (ctx, w);
    fz_drop_page (ctx, page);
    fz_drop_document (ctx, d);
    fz_drop_buffer (ctx, buf);
  }
  fz_catch (ctx) {
    cout << "TeXmacs] SVG to PDF: " << fz_caught_message (ctx) << LF;
    ok= false;
  }
  return ok;
}

// The XObject for a file, added once and used as often as it occurs.
// A PDF goes in as a form -- its own drawing, kept as drawing -- and a
// raster image as an image; anything else (EPS, PostScript, SVG) is
// turned into a PDF first, which is what pdf_hummus_renderer does too.
// Returns the number of the XObject, or -1.
// The structure of a tagged picture, read in C (no destructors in fz_try)
#define GRAFT_MAX 4096
#define GRAFT_ITEMS 16384

struct graft_c {
  int  n, nitems, mcids;
  int  steps;            // of the walk (a budget)
  int  stm;              // marked content in forms of the picture: no graft
  int  nmarked;          // the objects marked (visited), to unmark
  pdf_obj* marked[GRAFT_ITEMS];
  int  parent[GRAFT_MAX];
  char role[GRAFT_MAX][16];
  char* alt[GRAFT_MAX];
  int  owner[GRAFT_ITEMS], val[GRAFT_ITEMS];   // val: element, or -1-MCID
};

// the standard type of a type of the picture, through its role map; another
// type is NonStruct
static void
graft_role (fz_context* ctx, pdf_obj* rolemap, const char* s, char* out) {
  for (int k=0; k<8 && s != NULL; k++) {
    const char* r= pdf_graft_role (s);
    if (r != NULL) { fz_strlcpy (out, r, 16); return; }
    pdf_obj* m= pdf_dict_gets (ctx, rolemap, s);
    s= (pdf_is_name (ctx, m)? pdf_to_name (ctx, m): NULL);
  }
  fz_strlcpy (out, "NonStruct", 16);
}

static void
graft_item (graft_c* g, int owner, int val) {
  if (g->nitems >= GRAFT_ITEMS) return;
  g->owner[g->nitems]= owner;
  g->val[g->nitems++]= val;
}

// the content k of the element owner (-1: the root), on the page pg, of
// which only what is on the page page0 of the picture is kept
static void
graft_walk (fz_context* ctx, pdf_obj* rolemap, pdf_obj* k, int owner,
            pdf_obj* pg, pdf_obj* page0, graft_c* g, int depth) {
  if (k == NULL || depth > 64 || ++g->steps > 200000) return;
  // (each object once: a tree may share or loop)
  if (pdf_is_indirect (ctx, k)) {
    if (g->nmarked >= GRAFT_ITEMS) { g->steps= 200001; return; }
    if (pdf_mark_obj (ctx, k)) return;
    g->marked[g->nmarked++]= k;
  }
  if (pdf_is_array (ctx, k)) {
    for (int i=0; i < pdf_array_len (ctx, k); i++)
      graft_walk (ctx, rolemap, pdf_array_get (ctx, k, i), owner, pg, page0,
                  g, depth + 1);
    return;
  }
  if (pdf_is_int (ctx, k)) {
    int m= pdf_to_int (ctx, k);
    if (owner >= 0 && m >= 0 && m < 65536 && pg == page0) {
      graft_item (g, owner, -1-m);
      if (m >= g->mcids) g->mcids= m + 1;
    }
    return;
  }
  if (!pdf_is_dict (ctx, k)) return;
  pdf_obj* p= pdf_dict_gets (ctx, k, "Pg");
  if (p != NULL) pg= pdf_resolve_indirect (ctx, p);
  pdf_obj* type= pdf_dict_get (ctx, k, PDF_NAME(Type));
  if (pdf_name_eq (ctx, type, PDF_NAME(MCR))) {
    // (the marked content of a form of the picture: such a picture is a
    // Figure, its tags taken out)
    if (pdf_dict_gets (ctx, k, "Stm") != NULL) { g->stm= 1; return; }
    graft_walk (ctx, rolemap, pdf_dict_get (ctx, k, PDF_NAME(MCID)), owner,
                pg, page0, g, depth + 1);
    return;
  }
  if (pdf_name_eq (ctx, type, PDF_NAME(OBJR))) return;
  pdf_obj* st= pdf_dict_get (ctx, k, PDF_NAME(S));
  if (!pdf_is_name (ctx, st) || g->n >= GRAFT_MAX) return;
  int e= g->n++;
  g->parent[e]= owner;
  graft_role (ctx, rolemap, pdf_to_name (ctx, st), g->role[e]);
  pdf_obj* alt= pdf_dict_get (ctx, k, PDF_NAME(Alt));
  g->alt[e]= (alt == NULL? NULL: fz_strdup (ctx, pdf_to_text_string (ctx, alt)));
  graft_item (g, owner, e);
  graft_walk (ctx, rolemap, pdf_dict_get (ctx, k, PDF_NAME(K)), e, pg, page0,
              g, depth + 1);
}

// The structure read, without the elements of no content on the page;
// keep: the MCIDs which the elements have
static pdf_tag_graft
graft_structure (graft_c* gc, hashset<int>& keep) {
  array<int> parent, owner, val;
  array<string> role, alt;
  for (int i=0; i<gc->n; i++) {
    parent << gc->parent[i];
    role << string (gc->role[i]);
    alt << (gc->alt[i] == NULL? string (""): string (gc->alt[i]));
  }
  for (int j=0; j<gc->nitems; j++) { owner << gc->owner[j]; val << gc->val[j]; }
  return pdf_graft_prune (parent, role, alt, owner, val, gc->mcids, keep);
}

int
mupdf_pdf_renderer_rep::embed_image (url u, bool graft) {
  // graft: the structure of the picture is read, if it is a tagged PDF,
  // for draw_scalable to graft it; such a picture has a form for each of
  // its uses (an element is in one place of the structure tree)
  url name= resolve (u);
  if (is_none (name)) return -1;
  string key= concretize (name);
  if (image_pool->contains (key) &&
      (!graft || untagged_files->contains (key))) return image_pool (key);
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
      if (s != "svg" || !mupdf_svg_to_pdf (ctx, name, tmp)) {
        int w= 0, h= 0;
        image_size (name, w, h);
        image_to_pdf (name, tmp, w, h, 300);
      }
      pdf= tmp;
    }
    pdf_document* src= NULL;
    fz_buffer* buf= NULL;
    pdf_graft_map* map= NULL;
    pdf_obj* kept= NULL;   // the resources, for embed_picture_fonts
    pdf_obj* kept_form= NULL;   // and the form of the picture
    graft_c* gc= NULL;     // its structure (graft)
    fz_var (gc);
    // (all the uses of a PDF picture share its document and its map: one
    // copy of its resources, and of its layers)
    bool reuse= (s == "pdf" && graft_docs->contains (key));
    // made before fz_try: a throw is a longjmp, which skips destructors
    c_string path (concretize (pdf));
    fz_try (ctx) {
      if (reuse) src= pdf_keep_document (ctx, (pdf_document*) graft_docs [key]);
      else src= pdf_open_document (ctx, path);
      pdf_obj* spage= pdf_lookup_page_obj (ctx, src, 0);
      if (graft && s == "pdf") {
        pdf_obj* str= pdf_dict_getp (ctx, pdf_trailer (ctx, src),
                                     "Root/StructTreeRoot");
        if (str != NULL) {
          gc= (graft_c*) fz_calloc (ctx, 1, sizeof (graft_c));
          graft_walk (ctx, pdf_dict_get (ctx, str, PDF_NAME(RoleMap)),
                      pdf_dict_get (ctx, str, PDF_NAME(K)), -1, NULL,
                      pdf_resolve_indirect (ctx, spage), gc, 0);
          for (int i=0; i<gc->nmarked; i++) pdf_unmark_obj (ctx, gc->marked[i]);
          gc->nmarked= 0;
        }
      }
      fz_rect box; fz_matrix m;
      pdf_page_obj_transform (ctx, spage, &box, &m);
      // pdf_page_obj_transform gives fitz's transform of the page, which
      // turns PDF space (y up) into fitz space (y down) as well as undoing
      // /Rotate; a form's /Matrix lives in PDF space, so the turn upside
      // down is taken back out, or every figure comes out upside down
      m= fz_concat (m, fz_scale (1, -1));
      buf= page_contents (ctx, pdf_dict_get (ctx, spage, PDF_NAME(Contents)));
      pdf_obj* sres= pdf_dict_get_inheritable (ctx, spage, PDF_NAME(Resources));
      if (reuse) map= pdf_keep_graft_map (ctx, (pdf_graft_map*) graft_maps [key]);
      else map= pdf_new_graft_map (ctx, doc);
      pdf_obj* res= (sres == NULL) ? NULL
                                   : pdf_graft_mapped_object (ctx, map, sres);
      if (!reuse) merge_layers (src, map);   // (once for all the uses)
      if (s == "pdf" && !reuse) {
        graft_docs (key)= (pointer) pdf_keep_document (ctx, src);
        graft_maps (key)= (pointer) pdf_keep_graft_map (ctx, map);
      }
      pdf_obj* xo= pdf_new_xobject (ctx, doc, box, m, res, buf);
      // a group, so that an alpha applies to the figure as a whole and not
      // to each of its paths and fills on its own (place_image); the form
      // is shared by every use, so it is a group for all of them
      pdf_obj* grp= pdf_dict_put_dict (ctx, xo, PDF_NAME(Group), 2);
      pdf_dict_put (ctx, grp, PDF_NAME(S), PDF_NAME(Transparency));
      num= n_xobj++;
      char cnm[32];
      snprintf (cnm, sizeof (cnm), "Im%d", (int) (num));
      kept_form= pdf_keep_obj (ctx, xo);
      pdf_dict_puts_drop (ctx, res_xobj, cnm, xo);
      kept= res;
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
    // the structure of a tagged picture, its elements with marked content
    pdf_tag_graft g;
    hashset<int> keep;
    if (gc != NULL) {
      if (num >= 0 && !gc->stm && gc->steps <= 200000)
        g= graft_structure (gc, keep);
      for (int i=0; i<gc->n; i++) fz_free (ctx, gc->alt[i]);
      fz_free (ctx, gc);
    }
    bool grafted= (num >= 0 && N(g.roles) > 0);
    if (grafted) {
      graft_of (num)= N(grafts);
      grafts << g;
    }
    // a tagged PDF embeds the fonts which the picture names only (PDF/UA)
    if (kept_form != NULL && tagger != NULL) {
      if (kept != NULL) embed_picture_fonts (kept, kept_form);
        if (!strip_picture_tags (kept, kept_form, grafted? &keep: NULL) &&
          grafted) {
        // (a Figure then, as a picture which is not tagged)
        graft_of->reset (num);
        grafts->resize (N(grafts) - 1);
        grafted= false;
      }
    }
    if (graft && !grafted) untagged_files->insert (key);
    if (grafted) {
      pdf_drop_obj (ctx, kept);
      pdf_drop_obj (ctx, kept_form);
      if (!is_none (tmp)) remove (tmp);
      return num;   // (not in the pool: a form for each use)
    }
    pdf_drop_obj (ctx, kept);
    pdf_drop_obj (ctx, kept_form);
    if (!is_none (tmp)) remove (tmp);
  }
  image_pool (key)= num;
  return num;
}


/******************************************************************************
* The fonts which a PDF picture names only (docs/pdf-tagging.md): in a
* tagged PDF, a font which TeXmacs ships is embedded instead (pdf_font_subst)
******************************************************************************/

#define PICTURE_FONTS_MAX 64

// The codes which a picture draws with each of its fonts: a processor of
// its content, which follows the font of each text operator through the
// resources, the forms and the graphics states
struct picture_usage {
  pdf_processor super;
  pdf_document* doc;
  pdf_obj* res[32];
  int      depth;
  pdf_obj* font;
  pdf_obj* saved[32];   // the font of each graphics state (q)
  int      level;
  int      nesting;     // of the forms
  pdf_obj** fonts;
  int      n;
  unsigned char (*used)[256];
  bool*    seen;
  bool     lost;        // text of a font which was not found
  pdf_obj* done[256];   // the forms processed (each once: no cycles, no
  int      ndone;       // fan-out)
};

static void
pu_push (fz_context* ctx, pdf_processor* proc, pdf_obj* res) {
  picture_usage* p= (picture_usage*) proc;
  if (p->depth < 32) p->res[p->depth]= pdf_keep_obj (ctx, res);
  p->depth++;
}

static pdf_obj*
pu_pop (fz_context* ctx, pdf_processor* proc) {
  (void) ctx;
  picture_usage* p= (picture_usage*) proc;
  if (p->depth == 0) return NULL;
  p->depth--;
  return (p->depth < 32? p->res[p->depth]: NULL);
}

static void
pu_q (fz_context* ctx, pdf_processor* proc) {
  (void) ctx;
  picture_usage* p= (picture_usage*) proc;
  if (p->level < 32) p->saved[p->level]= p->font;
  p->level++;
}

static void
pu_Q (fz_context* ctx, pdf_processor* proc) {
  (void) ctx;
  picture_usage* p= (picture_usage*) proc;
  if (p->level == 0) return;
  p->level--;
  if (p->level < 32) p->font= p->saved[p->level];
}

static void
pu_Tf (fz_context* ctx, pdf_processor* proc, const char* name,
       pdf_font_desc* font, float size) {
  (void) font; (void) size;
  picture_usage* p= (picture_usage*) proc;
  p->font= NULL;
  if (p->depth == 0 || p->depth > 32) return;
  pdf_obj* fd= pdf_dict_get (ctx, p->res[p->depth - 1], PDF_NAME(Font));
  p->font= pdf_resolve_indirect (ctx, pdf_dict_gets (ctx, fd, name));
}

static void
pu_text (picture_usage* p, const char* str, size_t len) {
  // (a font set otherwise than by Tf, an ExtGState: its codes are unknown)
  if (p->font == NULL) p->lost= true;
  for (int k=0; k<p->n; k++)
    if (p->fonts[k] == p->font) {
      p->seen[k]= true;
      for (size_t i=0; i<len; i++) p->used[k][(unsigned char) str[i]]= 1;
    }
}

static void
pu_Tj (fz_context* ctx, pdf_processor* proc, char* str, size_t len) {
  (void) ctx;
  pu_text ((picture_usage*) proc, str, len);
}

static void
pu_squote (fz_context* ctx, pdf_processor* proc, char* str, size_t len) {
  (void) ctx;
  pu_text ((picture_usage*) proc, str, len);
}

static void
pu_dquote (fz_context* ctx, pdf_processor* proc, float aw, float ac,
           char* str, size_t len) {
  (void) ctx; (void) aw; (void) ac;
  pu_text ((picture_usage*) proc, str, len);
}

static void
pu_TJ (fz_context* ctx, pdf_processor* proc, pdf_obj* a) {
  for (int i=0; i < pdf_array_len (ctx, a); i++) {
    pdf_obj* it= pdf_array_get (ctx, a, i);
    if (pdf_is_string (ctx, it))
      pu_text ((picture_usage*) proc, pdf_to_str_buf (ctx, it),
               pdf_to_str_len (ctx, it));
  }
}

static void
pu_Do_form (fz_context* ctx, pdf_processor* proc, const char* name,
            pdf_obj* form) {
  (void) name;
  picture_usage* p= (picture_usage*) proc;
  if (p->nesting >= 8) return;
  pdf_obj* fo= pdf_resolve_indirect (ctx, form);
  for (int i=0; i<p->ndone; i++) if (p->done[i] == fo) return;
  if (p->ndone >= 256) { p->lost= true; return; }
  p->done[p->ndone++]= fo;
  pdf_obj* fr= pdf_dict_get (ctx, form, PDF_NAME(Resources));
  if (fr == NULL && p->depth > 0 && p->depth <= 32) fr= p->res[p->depth - 1];
  pdf_obj* font= p->font;
  int level= p->level;
  pdf_obj* out= NULL;
  p->nesting++;
  fz_try (ctx) {
    pdf_process_contents (ctx, proc, p->doc, fr, form, NULL, &out);
  }
  fz_always (ctx) {
    p->nesting--;
    pdf_drop_obj (ctx, out);
  }
  fz_catch (ctx) {
    fz_rethrow (ctx);
  }
  p->font= font;
  p->level= level;
}

void
mupdf_pdf_renderer_rep::embed_picture_fonts (pdf_obj* res, pdf_obj* form) {
  // the fonts of the resources and of the forms in them, which have no
  // program; read into C buffers (no destructors in fz_try)
  pdf_obj* fonts[PICTURE_FONTS_MAX];
  char base[PICTURE_FONTS_MAX][128];
  char enc[PICTURE_FONTS_MAX][64];
  static char diff[PICTURE_FONTS_MAX][256][64];
  int n= 0;
  pdf_obj* todo[256];
  bool special[256];     // the resources of a pattern or a Type 3 font,
  bool whole[PICTURE_FONTS_MAX];   // whose content is not processed: the
  int ntodo= 0, seen= 0;           // fonts there are embedded whole
  fz_var (n);
  fz_try (ctx) {
    special[ntodo]= false;
    todo[ntodo++]= pdf_resolve_indirect (ctx, res);
    while (seen < ntodo && n < PICTURE_FONTS_MAX) {
      bool sp= special[seen];
      pdf_obj* r= todo[seen++];
      pdf_obj* fd= pdf_dict_get (ctx, r, PDF_NAME(Font));
      for (int i=0; fd != NULL && i < pdf_dict_len (ctx, fd) &&
                    n < PICTURE_FONTS_MAX; i++) {
        // (by identity: two fonts may be the same direct dictionary)
        pdf_obj* f= pdf_resolve_indirect (ctx, pdf_dict_get_val (ctx, fd, i));
        pdf_obj* st= pdf_dict_get (ctx, f, PDF_NAME(Subtype));
        // (the fonts of the glyphs of a Type 3 font)
        if (pdf_name_eq (ctx, st, PDF_NAME(Type3))) {
          pdf_obj* tr= pdf_resolve_indirect (ctx, pdf_dict_get (ctx, f, PDF_NAME(Resources)));
          int dup= 0;
          for (int j=0; j<ntodo; j++) if (todo[j] == tr) dup= 1;
          if (tr != NULL && !dup && ntodo < 256) {
            special[ntodo]= true; todo[ntodo++]= tr; }
          continue;
        }
        if (!pdf_name_eq (ctx, st, PDF_NAME(Type1)) &&
            !pdf_name_eq (ctx, st, PDF_NAME(TrueType)) &&
            !pdf_name_eq (ctx, st, PDF_NAME(MMType1))) continue;
        pdf_obj* desc= pdf_dict_get (ctx, f, PDF_NAME(FontDescriptor));
        if (desc != NULL &&
            (pdf_dict_get (ctx, desc, PDF_NAME(FontFile)) != NULL ||
             pdf_dict_get (ctx, desc, PDF_NAME(FontFile2)) != NULL ||
             pdf_dict_get (ctx, desc, PDF_NAME(FontFile3)) != NULL)) continue;
        int dup= 0;
        for (int j=0; j<n; j++)
          if (fonts[j] == f) { dup= 1; if (sp) whole[j]= true; }
        if (dup) continue;
        whole[n]= sp;
        fonts[n]= f;
        fz_strlcpy (base[n], pdf_to_name (ctx, pdf_dict_get (ctx, f, PDF_NAME(BaseFont))), 128);
        pdf_obj* e= pdf_dict_get (ctx, f, PDF_NAME(Encoding));
        enc[n][0]= '\0';
        for (int c=0; c<256; c++) diff[n][c][0]= '\0';
        if (pdf_is_name (ctx, e)) fz_strlcpy (enc[n], pdf_to_name (ctx, e), 64);
        else if (pdf_is_dict (ctx, e)) {
          fz_strlcpy (enc[n], pdf_to_name (ctx, pdf_dict_get (ctx, e, PDF_NAME(BaseEncoding))), 64);
          pdf_obj* d= pdf_dict_get (ctx, e, PDF_NAME(Differences));
          int code= 0;
          for (int j=0; j < pdf_array_len (ctx, d); j++) {
            pdf_obj* it= pdf_array_get (ctx, d, j);
            if (pdf_is_int (ctx, it)) code= pdf_to_int (ctx, it);
            else if (pdf_is_name (ctx, it) && code >= 0 && code < 256)
              fz_strlcpy (diff[n][code++], pdf_to_name (ctx, it), 64);
          }
        }
        n++;
      }
      // the resources of the forms and of the tiling patterns
      pdf_obj* xd= pdf_dict_get (ctx, r, PDF_NAME(XObject));
      pdf_obj* pd= pdf_dict_get (ctx, r, PDF_NAME(Pattern));
      int nx= (xd == NULL? 0: pdf_dict_len (ctx, xd));
      int np= (pd == NULL? 0: pdf_dict_len (ctx, pd));
      for (int i=0; i < nx + np && ntodo < 256; i++) {
        pdf_obj* x= (i < nx? pdf_dict_get_val (ctx, xd, i):
                             pdf_dict_get_val (ctx, pd, i - nx));
        if (i < nx && !pdf_name_eq (ctx, pdf_dict_get (ctx, x, PDF_NAME(Subtype)),
                                    PDF_NAME(Form))) continue;
        pdf_obj* xr= pdf_resolve_indirect (ctx, pdf_dict_get (ctx, x, PDF_NAME(Resources)));
        if (xr == NULL) continue;
        int dup= 0;
        for (int j=0; j<ntodo; j++)
          if (todo[j] == xr) { dup= 1; if (i >= nx) special[j]= true; }
        if (!dup) { special[ntodo]= (sp || i >= nx); todo[ntodo++]= xr; }
      }
    }
  }
  fz_catch (ctx) { n= 0; }
  // the codes which the picture draws with each font: only their glyphs
  // go in (a font of no text seen, in a pattern or a Type 3 font, has all)
  static unsigned char used[PICTURE_FONTS_MAX][256];
  bool drawn[PICTURE_FONTS_MAX];
  for (int k=0; k<n; k++) {
    drawn[k]= false;
    for (int c=0; c<256; c++) used[k][c]= 0;
  }
  picture_usage* pu= NULL;
  int lost= 0;
  fz_var (pu); fz_var (lost);
  if (n > 0 && form != NULL) {
    fz_try (ctx) {
      pu= (picture_usage*) pdf_new_processor (ctx, sizeof (picture_usage));
      pu->super.push_resources= pu_push;
      pu->super.pop_resources = pu_pop;
      pu->super.op_q = pu_q;
      pu->super.op_Q = pu_Q;
      pu->super.op_Tf= pu_Tf;
      pu->super.op_Tj= pu_Tj;
      pu->super.op_TJ= pu_TJ;
      pu->super.op_squote= pu_squote;
      pu->super.op_dquote= pu_dquote;
      pu->super.op_Do_form= pu_Do_form;
      pu->doc= doc; pu->fonts= fonts; pu->n= n;
      pu->used= used; pu->seen= drawn;
      pdf_obj* out= NULL;
      pdf_process_contents (ctx, &pu->super, doc, res, form, NULL, &out);
      pdf_drop_obj (ctx, out);
      pdf_close_processor (ctx, &pu->super);
      lost= (pu->lost? 1: 0);
    }
    fz_always (ctx) {
      pdf_drop_processor (ctx, (pdf_processor*) pu);
    }
    fz_catch (ctx) {
      for (int k=0; k<n; k++) drawn[k]= false;
    }
    // (text whose font is unknown, or a font also drawn by a pattern or a
    // Type 3 glyph: those keep all their codes)
    for (int k=0; k<n; k++)
      if (lost || whole[k]) drawn[k]= false;
  }
  for (int k=0; k<n; k++) {
    // what goes in the font, made before the fz_try below
    array<string> diffs (256);
    for (int c=0; c<256; c++) diffs[c]= string (diff[k][c]);
    string bname (base[k]);
    array<string> names= pdf_encoding_names (bname, string (enc[k]), diffs);
    if (drawn[k])
      for (int c=0; c<256; c++)
        if (!used[k][c]) names[c]= "";
    pdf_font_subst r;
    if (!pdf_font_substitute (bname, names, r)) {
      convert_warning << "no font to embed for " << bname
                      << " (a picture), the PDF is not PDF/UA" << LF;
      continue;
    }
    // a program subset to the glyphs of the font, or else the whole one,
    // once for the document
    string key= as_string (r.file);
    int fi= (picture_font_files->contains (key)? picture_font_files [key]: -1);
    // the bare CFF program (Type1C), which pdf_subset_fonts cuts down to
    // the glyphs used, else the whole OpenType font
    string prog;
    bool cff= false;
    if (fi < 0) {
      prog= pdf_cff_program (r.file);
      cff= (N(prog) > 0);
      if (!cff && load_string (r.file, prog, false)) continue;
    }
    if (r.missing > 0)
      convert_warning << r.missing << " characters of "
                      << bname << " (a picture) have no glyph in "
                      << r.ps_name << LF;
    // (a subset is named with a tag of its own)
    string tag;
    for (int t= picture_font_tags++, j=0; j<6; j++, t /= 26)
      tag << (char) ('A' + (t % 26));
    string ps_subset= tag * "+" * r.ps_name * string ((char) 0);
    string ps= r.ps_name * string ((char) 0);
    array<string> gn (256);
    for (int c=0; c<256; c++) gn[c]= r.glyph[c] * string ((char) 0);
    string tu= pdf_simple_to_unicode (r.unicode);
    int ng= N(r.gids);
    int* gids= tm_new_array<int> (ng);
    for (int j=0; j<ng; j++) gids[j]= r.gids[j];
    fz_buffer* fb= NULL;
    fz_buffer* tb= NULL;
    fz_buffer* sb= NULL;
    fz_var (fb); fz_var (tb); fz_var (sb);
    pdf_obj* ff= NULL;
    pdf_obj* d = NULL;
    pdf_obj* w = NULL;
    pdf_obj* e = NULL;
    pdf_obj* tu_obj= NULL;
    int subset= 0;
    fz_var (subset); fz_var (ff); fz_var (d); fz_var (w); fz_var (e);
    fz_var (tu_obj);
    fz_try (ctx) {
      if (fi < 0) {
        fb= fz_new_buffer_from_copied_data (ctx, (unsigned char*) &prog[0],
                                            (size_t) N(prog));
        // (the subsetter of MuPDF 1.28 fails on some glyphs of the TeX Gyre
        // fonts: the whole program then)
        if (cff) {
          fz_try (ctx) {
            sb= fz_subset_cff_for_gids (ctx, fb, gids, ng, 0, 0);
          }
          fz_catch (ctx) {
            sb= NULL;
          }
          if (sb != NULL) { fz_drop_buffer (ctx, fb); fb= sb; sb= NULL; subset= 1; }
        }
        pdf_obj* sd= pdf_new_dict (ctx, doc, 1);
        pdf_dict_put (ctx, sd, PDF_NAME(Subtype),
                      cff? PDF_NAME(Type1C): PDF_NAME(OpenType));
        ff= pdf_add_stream (ctx, doc, fb, sd, 0);
        pdf_drop_obj (ctx, sd);
      }
      else ff= pdf_keep_obj (ctx, picture_font_objs[fi]);
      // the new objects first, then the font dictionary, all at once
      pdf_obj* f= fonts[k];
      char* name= (subset? &ps_subset[0]: &ps[0]);
      d= pdf_add_new_dict (ctx, doc, 10);
      pdf_dict_put (ctx, d, PDF_NAME(Type), PDF_NAME(FontDescriptor));
      pdf_dict_put_name (ctx, d, PDF_NAME(FontName), name);
      pdf_dict_put_int (ctx, d, PDF_NAME(Flags), r.flags);
      pdf_obj* bb= pdf_dict_put_array (ctx, d, PDF_NAME(FontBBox), 4);
      for (int j=0; j<4; j++) pdf_array_push_int (ctx, bb, r.bbox[j]);
      pdf_dict_put_int (ctx, d, PDF_NAME(ItalicAngle), r.italic);
      pdf_dict_put_int (ctx, d, PDF_NAME(Ascent), r.ascent);
      pdf_dict_put_int (ctx, d, PDF_NAME(Descent), r.descent);
      pdf_dict_put_int (ctx, d, PDF_NAME(CapHeight), r.cap_height);
      pdf_dict_put_int (ctx, d, PDF_NAME(StemV), r.stem_v);
      pdf_dict_put_int (ctx, d, PDF_NAME(MissingWidth), r.missing_width);
      pdf_dict_put (ctx, d, PDF_NAME(FontFile3), ff);
      w= pdf_new_array (ctx, doc, N(r.widths));
      for (int j=0; j<N(r.widths); j++) pdf_array_push_int (ctx, w, r.widths[j]);
      // the glyphs of the program by their names, whatever the encoding said
      e= pdf_new_dict (ctx, doc, 2);
      pdf_dict_put (ctx, e, PDF_NAME(Type), PDF_NAME(Encoding));
      pdf_obj* da= pdf_dict_put_array (ctx, e, PDF_NAME(Differences), 64);
      int prev= -2;
      for (int c= 0; c < 256; c++) {
        if (N(gn[c]) <= 1) continue;
        if (c != prev + 1) pdf_array_push_int (ctx, da, c);
        pdf_array_push_name (ctx, da, &gn[c][0]);
        prev= c;
      }
      tb= fz_new_buffer_from_copied_data (ctx, (unsigned char*) &tu[0],
                                          (size_t) N(tu));
      tu_obj= pdf_add_stream (ctx, doc, tb, NULL, 0);
      pdf_dict_put (ctx, f, PDF_NAME(Subtype), PDF_NAME(Type1));
      pdf_dict_put_name (ctx, f, PDF_NAME(BaseFont), name);
      pdf_dict_put_int (ctx, f, PDF_NAME(FirstChar), r.first);
      pdf_dict_put_int (ctx, f, PDF_NAME(LastChar), r.last);
      pdf_dict_put (ctx, f, PDF_NAME(Widths), w);
      pdf_dict_put (ctx, f, PDF_NAME(Encoding), e);
      pdf_dict_put (ctx, f, PDF_NAME(ToUnicode), tu_obj);
      pdf_dict_put (ctx, f, PDF_NAME(FontDescriptor), d);
    }
    fz_always (ctx) {
      fz_drop_buffer (ctx, fb); fz_drop_buffer (ctx, tb);
      fz_drop_buffer (ctx, sb);
      pdf_drop_obj (ctx, d); pdf_drop_obj (ctx, w);
      pdf_drop_obj (ctx, e); pdf_drop_obj (ctx, tu_obj);
    }
    fz_catch (ctx) {
      pdf_drop_obj (ctx, ff); ff= NULL;
      convert_warning << "MuPDF could not embed a font for " << bname
                      << " (a picture): " << fz_caught_message (ctx) << LF;
    }
    tm_delete_array (gids);
    if (ff != NULL && fi < 0) {
      if (!subset) picture_font_files (key)= N(picture_font_objs);
      picture_font_objs << ff;
    }
    else if (ff != NULL) pdf_drop_obj (ctx, ff);
  }
}

// A picture which is itself a tagged PDF: its marked content (its tags, its
// artifacts) would nest in its Figure, and its MCIDs are in no parent tree.
// In a tagged PDF it is taken out of the content of the picture and of its
// forms (pdf_strip_marked_content), but optional content.
bool
mupdf_pdf_renderer_rep::strip_picture_tags (pdf_obj* res, pdf_obj* form,
                                            hashset<int>* keep) {
  // keep: the picture is grafted; false when it cannot be (it paints
  // outside its marked content), and its tags are then taken out
  bool ok= true;
  pdf_obj* forms[256];   // (the references: a stream is loaded by them)
  pdf_obj* ids[256];     // (and the objects, to find them once each)
  int nf= 0;
  fz_var (nf);
  fz_try (ctx) {
    ids[nf]= pdf_resolve_indirect (ctx, form);
    forms[nf++]= form;
    pdf_obj* todo[256];
    int ntodo= 0, seen= 0;
    todo[ntodo++]= pdf_resolve_indirect (ctx, res);
    while (seen < ntodo) {
      // the forms, and the tiling patterns, which have content too
      pdf_obj* rs= todo[seen++];
      pdf_obj* xd= pdf_dict_get (ctx, rs, PDF_NAME(XObject));
      pdf_obj* pd= pdf_dict_get (ctx, rs, PDF_NAME(Pattern));
      int nx= (xd == NULL? 0: pdf_dict_len (ctx, xd));
      int np= (pd == NULL? 0: pdf_dict_len (ctx, pd));
      for (int i=0; i < nx + np; i++) {
        pdf_obj* xref= (i < nx? pdf_dict_get_val (ctx, xd, i):
                                pdf_dict_get_val (ctx, pd, i - nx));
        pdf_obj* x= pdf_resolve_indirect (ctx, xref);
        if (!pdf_is_indirect (ctx, xref) || !pdf_is_stream (ctx, xref)) continue;
        if (i < nx && !pdf_name_eq (ctx, pdf_dict_get (ctx, x, PDF_NAME(Subtype)),
                                    PDF_NAME(Form))) continue;
        int dup= 0;
        for (int j=0; j<nf; j++) if (ids[j] == x) dup= 1;
        if (dup || nf >= 256) continue;
        ids[nf]= x;
        forms[nf++]= xref;
        pdf_obj* xr= pdf_resolve_indirect (ctx, pdf_dict_get (ctx, x, PDF_NAME(Resources)));
        if (xr != NULL && ntodo < 256) todo[ntodo++]= xr;
      }
    }
  }
  fz_catch (ctx) {}
  // the MCIDs of the names of the properties of the picture's resources
  hashmap<string,int> props (-1);
  fz_try (ctx) {
    pdf_obj* pr= pdf_dict_get (ctx, pdf_resolve_indirect (ctx, res),
                               PDF_NAME(Properties));
    for (int i=0; pr != NULL && i < pdf_dict_len (ctx, pr) && i < 4096; i++) {
      pdf_obj* v= pdf_dict_get_val (ctx, pr, i);
      pdf_obj* m= pdf_dict_get (ctx, v, PDF_NAME(MCID));
      if (pdf_is_int (ctx, m)) {
        char nm[256];
        nm[0]= '/';
        fz_strlcpy (nm + 1, pdf_to_name (ctx, pdf_dict_get_key (ctx, pr, i)), 255);
        props (string (nm))= pdf_to_int (ctx, m);
      }
    }
  }
  fz_catch (ctx) {}
  for (int k=0; k<nf; k++) {
    fz_buffer* b= NULL;
    unsigned char* data= NULL;
    size_t len= 0;
    fz_var (b);
    fz_try (ctx) {
      if (k > 0 || keep == NULL) {
        pdf_dict_del (ctx, forms[k], PDF_NAME(StructParents));
        pdf_dict_del (ctx, forms[k], PDF_NAME(StructParent));
      }
      b= pdf_load_stream (ctx, forms[k]);
      len= fz_buffer_storage (ctx, b, &data);
    }
    fz_catch (ctx) { fz_drop_buffer (ctx, b); continue; }
    // (made outside fz_try: strings have destructors)
    string s ((int) len);
    for (size_t j=0; j<len; j++) s[(int) j]= (char) data[j];
    fz_drop_buffer (ctx, b);
    array<bool> open;
    bool changed= false;
    // (the content of a grafted picture keeps its marked content, if all
    // it paints is then tagged or an artifact: else the picture is not
    // grafted, and its tags are taken out)
    string t;
    bool gok= false;
    if (k == 0 && keep != NULL)
      t= pdf_graft_marked_content (s, *keep, props, changed, gok);
    if (k == 0 && keep != NULL && !gok) { ok= false; keep= NULL; }
    if (k > 0 || keep == NULL) {
      changed= false;
      t= pdf_strip_marked_content (s, open, changed);
    }
    if (!changed) continue;
    fz_buffer* nb= NULL;
    fz_var (nb);
    fz_try (ctx) {
      nb= fz_new_buffer_from_copied_data (ctx, (unsigned char*) &t[0],
                                          (size_t) N(t));
      pdf_update_stream (ctx, doc, forms[k], nb, 0);
    }
    fz_always (ctx) { fz_drop_buffer (ctx, nb); }
    fz_catch (ctx) {
      convert_warning << "MuPDF could not take the tags out of a picture: "
                      << fz_caught_message (ctx) << LF;
    }
  }
  return ok;
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
  char cnm[32];
  snprintf (cnm, sizeof (cnm), "Im%d", (int) (num));
  double m[6];
  pdf_obj* xo= NULL;
  fz_try (ctx) { xo= pdf_dict_gets (ctx, res_xobj, cnm); }
  fz_catch (ctx) { xo= NULL; }
  xobject_fit (xo, w, h, to_x (x), to_y (y), m);
  app ("q %g %g %g %g %g %g cm /%s Do Q\n",
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
  bool form= false;
  fz_rect b= fz_empty_rect;
  fz_try (ctx) {
    form= xo != NULL &&
          pdf_name_eq (ctx, pdf_dict_get (ctx, xo, PDF_NAME(Subtype)),
                       PDF_NAME(Form));
    if (form)
      b= fz_transform_rect (pdf_dict_get_rect (ctx, xo, PDF_NAME(BBox)),
                            pdf_dict_get_matrix (ctx, xo, PDF_NAME(Matrix)));
  }
  fz_catch (ctx) { form= false; }
  if (!form) return;
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
  char cinm[32];
  snprintf (cinm, sizeof (cinm), "Im%d", (int) (img));
  pdf_obj* xo= NULL;
  fz_try (ctx) { xo= pdf_dict_gets (ctx, res_xobj, cinm); }
  fz_catch (ctx) { xo= NULL; }
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
    char cpnm[32];
    snprintf (cpnm, sizeof (cpnm), "P%d", (int) (num));
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
  char cnm[32];
  snprintf (cnm, sizeof (cnm), "Im%d", (int) (img));
  bool r= false;
  fz_try (ctx) {
    pdf_obj* xo= pdf_dict_gets (ctx, res_xobj, cnm);
    r= (xo != NULL) && !transparent_xobject (ctx, xo, 0);
  }
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
  tag_mark (1);
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
  if (N(pattern) >= 4 && is_compound (pattern[3])) eff= pattern[3];
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
    app ("/GS%d gs\n", alpha_state (pattern_alpha));
  app ("/Pattern cs /P%d scn %g %g %g %g re f\nQ\n",
                    pat, to_x (x1), to_y (y1),
                    to_x (x2) - to_x (x1), to_y (y2) - to_y (y1));
}

void
mupdf_pdf_renderer_rep::draw_scalable (scalable im, SI x, SI y, int alpha) {
  // an image with an effect on it has to be computed, so it is rasterized
  // (at a print resolution, see shadow); a plain one is included as it is
  if (im->get_type () != scalable_image || im->get_effect () != tree ("")) {
    tag_mark (2);
    renderer_rep::draw_scalable (im, x, y, alpha);
    return;
  }
  // a tagged picture of no alternative text of the user's has its own
  // structure grafted in place of its Figure (pdf_tags_rep::graft)
  bool graft= (tagger != NULL && contents != NULL && tg ()->can_graft (tag_ip));
  int num= embed_image (im->get_name (), graft);
  if (num < 0) {
    tag_mark (2);
    renderer_rep::draw_scalable (im, x, y, alpha);
    return;
  }
  if (graft && graft_of->contains (num)) {
    int stream;
    string ops= tg ()->graft (tag_ip, page_num, grafts[graft_of[num]], stream);
    if (N(ops) > 0) { end_text (); c_string cops (ops); put (cops); }
    char cnm[32];
    snprintf (cnm, sizeof (cnm), "Im%d", (int) (num));
    fz_try (ctx) {
      pdf_obj* xo= pdf_dict_gets (ctx, res_xobj, cnm);
      pdf_dict_put_int (ctx, xo, PDF_NAME(StructParents),
                        pdf_tag_stream_key0 + stream);
      graft_forms << pdf_keep_obj (ctx, xo);
    }
    fz_catch (ctx) {
      graft_forms << (pdf_obj*) NULL;
    }
  }
  else tag_mark (2);
  rectangle r= im->get_logical_extents ();
  double w= ((double) (r->x2 - r->x1)) / pixel;
  double h= ((double) (r->y2 - r->y1)) / pixel;
  place_image (num, w, h, x - r->x1, y - r->y1, alpha);
}

/******************************************************************************
* Links, the outline and the metadata
******************************************************************************/

// The labels, titles and addresses reach the renderer in UTF-8 (checked:
// the metadata, the entries of the outline, the anchors and the targets of
// links all do), so they are not converted from Cork as pdf_hummus_renderer
// converts them.

// A text string of PDF, as bytes: ASCII as it is, anything else in UTF-16BE
// after a byte order mark -- what pdf_new_text_string makes, spelled out so
// that the same bytes can be written wherever the same name must be found.
// A named destination is looked up by its bytes, and a link which names it
// in other bytes than the name tree leads nowhere, in a reader which does
// not decode both before comparing (MuPDF does).
static string
pdf_text_bytes (string s) {
  bool ascii= true;
  for (int i=0; i<N(s); i++) if (((unsigned char) s[i]) >= 128) ascii= false;
  if (ascii) return s;
  string r;
  r << (char) 0xFE << (char) 0xFF;
  int i= 0;
  while (i < N(s)) {
    unsigned int c= decode_from_utf8 (s, i);
    if (c >= 0x10000) {   // a surrogate pair
      c -= 0x10000;
      unsigned int hi= 0xD800 + (c >> 10), lo= 0xDC00 + (c & 0x3FF);
      r << (char) (hi >> 8) << (char) (hi & 0xFF)
        << (char) (lo >> 8) << (char) (lo & 0xFF);
    }
    else r << (char) (c >> 8) << (char) (c & 0xFF);
  }
  return r;
}

// the order of the keys of a name tree: by their bytes
static bool
bytes_less (string a, string b) {
  int n= min (N(a), N(b));
  for (int i=0; i<n; i++)
    if (a[i] != b[i]) return ((unsigned char) a[i]) < ((unsigned char) b[i]);
  return N(a) < N(b);
}

// A URI is an ASCII string: what is not ASCII -- and a space, a control
// character -- is percent encoded, from its UTF-8 bytes (RFC 3987). Hummus
// writes the address as a text string, UTF-16 as soon as it has an accent.
static string
uri_bytes (string s) {
  static const char* hex= "0123456789ABCDEF";
  string r;
  for (int i=0; i<N(s); i++) {
    unsigned char c= (unsigned char) s[i];
    if (c <= 32 || c >= 127) r << '%' << hex[c >> 4] << hex[c & 15];
    else r << (char) c;
  }
  return r;
}

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
  pdf_link_item it (label,
                    f * to_x (x1 - 5*pixel), f * to_y (y1 - 10*pixel),
                    f * to_x (x2 + 5*pixel), f * to_y (y2 + 10*pixel));
  // The words of a link come one at a time, a box each: one which follows
  // the last on its line, to the same place, makes the last one longer,
  // so that a link is one annotation on each line it runs over, and not
  // one a word ("the", "TeXmacs", "site"). On the same line: the heights
  // overlap for the most part; following it: no further than a space of
  // large type (12 pt)
  int n= N(links);
  if (n > 0) {
    pdf_link_item& last= links[n-1];
    double lo= max (last.y1, it.y1), hi= min (last.y2, it.y2);
    double h = min (last.y2 - last.y1, it.y2 - it.y1);
    if (last.label == label && h > 0 && hi - lo > 0.5 * h &&
        it.x1 >= last.x1 && it.x1 - last.x2 <= 12.0) {
      last.x2= max (last.x2, it.x2);
      last.y1= min (last.y1, it.y1);
      last.y2= max (last.y2, it.y2);
      return;
    }
  }
  links << it;
  // a tagged PDF: the Link element of its text (or what holds the text)
  int e= -1;
  path pos;
  if (tagger != NULL) e= tg ()->link_element (tag_ip, pos);
  link_elem << e;
  link_pos << pos;
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
  // the metadata of the document are Cork, the PDF wants UTF-8 (as
  // as_hummus_string in pdf_hummus_renderer)
  metadata (kind)= cork_to_utf8 (val);
}

// called inside the fz_try of end_page, with the labels prepared there:
// nothing here may have a destructor
void
mupdf_pdf_renderer_rep::write_links (pdf_obj* pobj, array<string>& target,
                                     array<int>& local, int border) {
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
    // /Border [16 16 w [3 10]] /Color [0.75 0.5 1.0], as pdf_hummus_renderer
    pdf_obj* b= pdf_dict_put_array (ctx, a, PDF_NAME(Border), 4);
    pdf_array_push_int (ctx, b, 16);
    pdf_array_push_int (ctx, b, 16);
    pdf_array_push_int (ctx, b, border);
    pdf_obj* dash= pdf_array_push_array (ctx, b, 2);
    pdf_array_push_int (ctx, dash, 3);
    pdf_array_push_int (ctx, dash, 10);
    pdf_obj* col= pdf_dict_put_array (ctx, a, PDF_NAME(C), 3);
    pdf_array_push_real (ctx, col, 0.75);
    pdf_array_push_real (ctx, col, 0.5);
    pdf_array_push_real (ctx, col, 1.0);
    string& t= target[i];
    if (local[i]) {
      // a place in the document: the name anchor () registered, in the
      // bytes write_dests gives the key of the name tree
      pdf_dict_put_string (ctx, a, PDF_NAME(Dest), &t[0], N(t));
    }
    else {
      pdf_obj* act= pdf_dict_put_dict (ctx, a, PDF_NAME(A), 2);
      pdf_dict_put (ctx, act, PDF_NAME(S), PDF_NAME(URI));
      pdf_dict_put_string (ctx, act, PDF_NAME(URI), &t[0], N(t));
    }
    if (tagger != NULL && i < N(link_text)) {
      // PDF/UA: a description, and the place in the structure
      pdf_dict_put_text_string (ctx, a, PDF_NAME(Contents), &link_text[i][0]);
      pdf_dict_put_int (ctx, a, PDF_NAME(StructParent),
                        tag_annot_key0 + link_annot0 + i);
      pdf_obj* ref= pdf_add_object_drop (ctx, doc, a);
      pdf_array_push (ctx, annots, ref);
      tag_annots[link_annot0 + i]= ref;
    }
    else
    pdf_array_push_drop (ctx, annots, pdf_add_object_drop (ctx, doc, a));
  }
}

/******************************************************************************
* Embedded videos (as pdf_hummus_renderer, #249)
*
* A video (an animated gif inserted with <video>) becomes a RichMedia
* annotation, as the media9 LaTeX package makes them, over the picture of
* its current frame; its asset is an MP4 file made from the frames of the
* image (video_for_pdf) and embedded in the PDF. Okular plays it when the
* page is shown; a reader which plays no video shows the picture.
******************************************************************************/

void
mupdf_pdf_renderer_rep::embed_video (url u, int ms, bool repeat,
                                     SI x1, SI y1, SI x2, SI y2) {
  string key= as_string (u) * "|" * as_string (ms);
  if (!video_index->contains (key)) {
    video_index (key)= -1;
    url v= video_for_pdf (u, ms, temp_videos);
    string data;
    if (!is_none (v) && !load_string (v, data, false) && N(data) > 0) {
      // made before the fz_try, which must create nothing with a destructor
      string fname= "video-" * as_string (N(video_fs) + 1) * ".mp4" *
                    string ((char) 0);
      pdf_obj* fs= NULL;
      fz_buffer* buf= NULL;
      fz_var (fs);
      fz_var (buf);
      fz_try (ctx) {
        buf= fz_new_buffer_from_copied_data (ctx, (unsigned char*) &data[0],
                                             (size_t) N(data));
        fs= pdf_add_embedded_file (ctx, doc, &fname[0], "video/mp4", buf,
                                   -1, -1, 0);
      }
      fz_always (ctx) fz_drop_buffer (ctx, buf);
      fz_catch (ctx) { pdf_drop_obj (ctx, fs); fs= NULL; }
      if (fs != NULL) {
        video_index (key)= N(video_fs);
        video_fs << fs;
        video_fname << fname;
      }
    }
    if (video_index [key] < 0)
      convert_warning << "Cannot embed video " << u << " in PDF" << LF;
  }
  int k= video_index [key];
  if (k < 0) return;
  string fname= video_fname[k] (0, N(video_fname[k]) - 1);
  double f= (double) default_dpi / dpi;
  page_videos << pdf_link_item (as_string (tail (u)) * string ((char) 0),
                                f * to_x (x1), f * to_y (y1),
                                f * to_x (x2), f * to_y (y2));
  page_video_file << k;
  // a tagged PDF: an Annot element in the element of the picture
  int ve= -1;
  path vpos;
  if (tagger != NULL) ve= tg ()->annot_element (tag_ip, vpos);
  page_video_elem << ve;
  page_video_pos << vpos;
  page_video_nm << ("video-" * as_string (N(page_videos)) * "-" *
                    as_string (tail (u)) * string ((char) 0));
  page_video_vars << ("source=" * fname * "&loop=" *
                      (repeat? string ("true"): string ("false")) *
                      string ((char) 0));
}

// called inside the fz_try of end_page, after write_links: nothing here
// may have a destructor
void
mupdf_pdf_renderer_rep::write_videos (pdf_obj* pobj) {
  if (N(page_videos) == 0) return;
  pdf_obj* annots= pdf_dict_get (ctx, pobj, PDF_NAME(Annots));
  if (annots == NULL)
    annots= pdf_dict_put_array (ctx, pobj, PDF_NAME(Annots), N(page_videos));
  for (int i=0; i<N(page_videos); i++) {
    pdf_link_item& v= page_videos[i];
    const char* name = &v.label[0];
    const char* fname= &video_fname[page_video_file[i]][0];
    pdf_obj* a= pdf_new_dict (ctx, doc, 9);
    pdf_dict_put (ctx, a, PDF_NAME(Type), PDF_NAME(Annot));
    pdf_dict_put (ctx, a, PDF_NAME(Subtype), PDF_NAME(RichMedia));
    pdf_dict_put_int (ctx, a, PDF_NAME(F), 4);
    pdf_obj* rect= pdf_dict_put_array (ctx, a, PDF_NAME(Rect), 4);
    pdf_array_push_real (ctx, rect, v.x1);
    pdf_array_push_real (ctx, rect, v.y1);
    pdf_array_push_real (ctx, rect, v.x2);
    pdf_array_push_real (ctx, rect, v.y2);
    // (the name of an annotation is unique on its page)
    pdf_dict_put_text_string (ctx, a, PDF_NAME(NM), &page_video_nm[i][0]);
    pdf_dict_put_text_string (ctx, a, PDF_NAME(Contents), name);
    pdf_dict_put (ctx, a, PDF_NAME(P), pobj);
    // an empty appearance: a reader which plays no video shows the picture
    // of the frame under it, and MuPDF, which cannot make the appearance of
    // a RichMedia annotation, does not try to (with an error each time)
    fz_rect bbox= fz_make_rect (0, 0, (float) (v.x2 - v.x1),
                                (float) (v.y2 - v.y1));
    fz_buffer* empty= fz_new_buffer (ctx, 1);
    pdf_obj* ap= NULL;
    fz_try (ctx) ap= pdf_new_xobject (ctx, doc, bbox, fz_identity, NULL, empty);
    fz_always (ctx) fz_drop_buffer (ctx, empty);
    fz_catch (ctx) fz_rethrow (ctx);
    pdf_obj* apd= pdf_dict_put_dict (ctx, a, PDF_NAME(AP), 1);
    pdf_dict_put_drop (ctx, apd, PDF_NAME(N), ap);
    pdf_obj* set= pdf_new_dict (ctx, doc, 3);
    pdf_dict_puts_drop (ctx, a, "RichMediaSettings", set);
    pdf_dict_put_name (ctx, set, PDF_NAME(Type), "RichMediaSettings");
    pdf_obj* act= pdf_new_dict (ctx, doc, 3);
    pdf_dict_puts_drop (ctx, set, "Activation", act);
    pdf_dict_put_name (ctx, act, PDF_NAME(Type), "RichMediaActivation");
    pdf_dict_puts_drop (ctx, act, "Condition", pdf_new_name (ctx, "PV"));
    pdf_obj* pres= pdf_new_dict (ctx, doc, 2);
    pdf_dict_puts_drop (ctx, act, "Presentation", pres);
    pdf_dict_put_name (ctx, pres, PDF_NAME(Type), "RichMediaPresentation");
    pdf_dict_puts_drop (ctx, pres, "Style", pdf_new_name (ctx, "Embedded"));
    pdf_obj* deact= pdf_new_dict (ctx, doc, 2);
    pdf_dict_puts_drop (ctx, set, "Deactivation", deact);
    pdf_dict_put_name (ctx, deact, PDF_NAME(Type), "RichMediaDeactivation");
    pdf_dict_puts_drop (ctx, deact, "Condition", pdf_new_name (ctx, "PI"));
    pdf_obj* con= pdf_new_dict (ctx, doc, 3);
    pdf_dict_puts_drop (ctx, a, "RichMediaContent", con);
    pdf_dict_put_name (ctx, con, PDF_NAME(Type), "RichMediaContent");
    pdf_obj* assets= pdf_new_dict (ctx, doc, 1);
    pdf_dict_puts_drop (ctx, con, "Assets", assets);
    pdf_obj* names= pdf_dict_put_array (ctx, assets, PDF_NAME(Names), 2);
    pdf_array_push_string (ctx, names, fname, strlen (fname));
    pdf_array_push (ctx, names, video_fs[page_video_file[i]]);
    pdf_obj* confs= pdf_new_array (ctx, doc, 1);
    pdf_dict_puts_drop (ctx, con, "Configurations", confs);
    pdf_obj* conf= pdf_array_push_dict (ctx, confs, 3);
    pdf_dict_put_name (ctx, conf, PDF_NAME(Type), "RichMediaConfiguration");
    pdf_dict_put_name (ctx, conf, PDF_NAME(Subtype), "Video");
    pdf_obj* insts= pdf_new_array (ctx, doc, 1);
    pdf_dict_puts_drop (ctx, conf, "Instances", insts);
    pdf_obj* inst= pdf_array_push_dict (ctx, insts, 3);
    pdf_dict_put_name (ctx, inst, PDF_NAME(Type), "RichMediaInstance");
    pdf_dict_put_name (ctx, inst, PDF_NAME(Subtype), "Video");
    pdf_obj* par= pdf_dict_put_dict (ctx, inst, PDF_NAME(Params), 2);
    pdf_dict_put_name (ctx, par, PDF_NAME(Type), "RichMediaParams");
    const char* vars= &page_video_vars[i][0];
    pdf_dict_puts_drop (ctx, par, "FlashVars",
                        pdf_new_string (ctx, vars, strlen (vars)));
    if (tagger != NULL) {
      // PDF/UA: its place in the structure
      pdf_dict_put_int (ctx, a, PDF_NAME(StructParent),
                        tag_annot_key0 + video_annot0 + i);
      pdf_obj* ref= pdf_add_object_drop (ctx, doc, a);
      pdf_array_push (ctx, annots, ref);
      tag_annots[video_annot0 + i]= ref;
    }
    else
      pdf_array_push_drop (ctx, annots, pdf_add_object_drop (ctx, doc, a));
  }
}

/******************************************************************************
* Tagged PDF (docs/pdf-tagging.md)
*
* Each box tells, as it is drawn, where it comes from (tag_ip, set by
* box_rep::redraw); the tagger of print_doc turns that into the elements
* around it. The drawing of an element is a block of marked content, with
* an MCID; what belongs to no element (the headers, footers and page
* numbers), and the lines and fills outside a formula or a figure (rules,
* borders, backgrounds), are artifacts. The structure tree is written when
* the document is closed (write_structure).
******************************************************************************/

bool
mupdf_pdf_renderer_rep::supports_tagging () {
  return true;
}

// the structure of the PDF, made when the tagger is known (print_doc gives
// it after the renderer is made)
pdf_tags
mupdf_pdf_renderer_rep::tg () {
  if (tags == NULL) tags= tm_new<pdf_tags_rep> (tagger);
  return tags;
}

// The drawing which follows is of the box being drawn: of kind 0 (text),
// 1 (lines, fills) or 2 (images)
void
mupdf_pdf_renderer_rep::tag_mark (int kind) {
  if (tagger == NULL || contents == NULL) return;
  string ops= tg ()->mark (tag_ip, kind, page_num);
  if (N(ops) == 0) return;
  // a block of marked content may not cross a text object
  end_text ();
  c_string cops (ops);
  put (cops);
}

void
mupdf_pdf_renderer_rep::tag_close () {
  if (tags == NULL) return;
  string ops= tags->close ();
  if (N(ops) == 0) return;
  end_text ();
  c_string cops (ops);
  put (cops);
}

// The structure tree, the parent tree, and what the catalog of a tagged
// PDF says (PDF/UA-1: /MarkInfo, /Lang, /ViewerPreferences, the metadata
// with the part of PDF/UA)
void
mupdf_pdf_renderer_rep::write_structure () {
  if (tags == NULL || N(tags->elems) == 0) return;
  // everything with a destructor first (see put): the children of each
  // element in their order, and the strings, as C strings
  int n= N(tags->elems);
  array<array<int> > kids= tags->children ();
  array<pdf_tag_kid>& tag_kids= tags->kids;
  array<string> role (n), alt (n), note_id (n);
  array<array<string> > akeys (n);
  for (int e=0; e<n; e++) {
    pdf_tag_elem& el= tags->elems[e];
    role[e]= el.role * string ((char) 0);
    if (N(el.alt) > 0) alt[e]= el.alt * string ((char) 0);
    for (int j=0; j<N(el.akeys); j++)
      akeys[e] << (el.akeys[j] * string ((char) 0));
    if (el.role == "Note")
      note_id[e]= "note-" * as_string (e) * string ((char) 0);
  }
  string lang= tagger->language () * string ((char) 0);
  string title= cork_to_utf8 (tagger->title ());
  if (N(title) == 0 && metadata->contains ("title")) title= metadata ("title");
  if (N(title) == 0) title= as_string (tail (pdf_file_name));
  string author= (metadata->contains ("author")? metadata ("author"): "");
  string xmp= pdf_tags_xmp (title, author,
                            "TeXmacs " TEXMACS_VERSION " + MuPDF " FZ_VERSION);
  tag_objs= array<pdf_obj*> (n);
  for (int e=0; e<n; e++) tag_objs[e]= NULL;
  int annots_base= tag_annot_key0;   // the keys of the annotations
  fz_buffer* xb= NULL;
  fz_var (xb);
  fz_try (ctx) {
    pdf_obj* root= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Root));
    pdf_obj* tree= pdf_add_new_dict (ctx, doc, 4);
    pdf_dict_put_drop (ctx, root, PDF_NAME(StructTreeRoot), tree);
    pdf_dict_put (ctx, tree, PDF_NAME(Type), PDF_NAME(StructTreeRoot));
    for (int e=0; e<n; e++)
      tag_objs[e]= pdf_add_new_dict (ctx, doc, 6);
    for (int e=0; e<n; e++) {
      pdf_tag_elem& el= tags->elems[e];
      pdf_obj* d= tag_objs[e];
      pdf_dict_put (ctx, d, PDF_NAME(Type), PDF_NAME(StructElem));
      pdf_dict_put_name (ctx, d, PDF_NAME(S), &role[e][0]);
      pdf_dict_put (ctx, d, PDF_NAME(P),
                    el.parent < 0? tree: tag_objs[el.parent]);
      if (N(alt[e]) > 0)
        pdf_dict_put_text_string (ctx, d, PDF_NAME(Alt), &alt[e][0]);
      if (N(akeys[e]) > 0) {
        // the attributes of a cell (its spans), owner Table
        pdf_obj* a= pdf_dict_put_dict (ctx, d, PDF_NAME(A), 1 + N(akeys[e]));
        pdf_dict_put_name (ctx, a, PDF_NAME(O), "Table");
        for (int j=0; j<N(akeys[e]); j++)
          pdf_dict_puts_drop (ctx, a, &akeys[e][j][0],
                              pdf_new_int (ctx, el.avals[j]));
      }
      if (N(note_id[e]) > 0)
        pdf_dict_put_string (ctx, d, PDF_NAME(ID), &note_id[e][0],
                             N(note_id[e]) - 1);
      pdf_obj* k= pdf_dict_put_array (ctx, d, PDF_NAME(K), N(kids[e]));
      for (int i=0; i<N(kids[e]); i++) {
        pdf_tag_kid& kd= tag_kids[kids[e][i]];
        if (kd.kind == 0) pdf_array_push (ctx, k, tag_objs[kd.id]);
        else if (kd.kind == 1 && kd.page < N(pages)) {
          pdf_obj* m= pdf_array_push_dict (ctx, k, 3);
          pdf_dict_put (ctx, m, PDF_NAME(Type), PDF_NAME(MCR));
          pdf_dict_puts (ctx, m, "Pg", pages[kd.page]);
          pdf_dict_put_int (ctx, m, PDF_NAME(MCID), kd.id);
        }
        else if (kd.kind == 2 && kd.page < N(pages)) {
          pdf_obj* m= pdf_array_push_dict (ctx, k, 3);
          pdf_dict_put (ctx, m, PDF_NAME(Type), PDF_NAME(OBJR));
          pdf_dict_puts (ctx, m, "Obj", tag_annots[kd.id]);
          pdf_dict_puts (ctx, m, "Pg", pages[kd.page]);
        }
        else if (kd.kind == 3 && kd.page < N(pages) &&
                 kd.stream < N(graft_forms) && graft_forms[kd.stream] != NULL) {
          // the marked content of a grafted picture, in its form
          pdf_obj* m= pdf_array_push_dict (ctx, k, 4);
          pdf_dict_put (ctx, m, PDF_NAME(Type), PDF_NAME(MCR));
          pdf_dict_puts (ctx, m, "Pg", pages[kd.page]);
          pdf_dict_puts (ctx, m, "Stm", graft_forms[kd.stream]);
          pdf_dict_put_int (ctx, m, PDF_NAME(MCID), kd.id);
        }
      }
    }
    pdf_dict_put (ctx, tree, PDF_NAME(K), tag_objs[0]);
    // the parent tree: for each page its elements by MCID, then each link
    pdf_obj* pt= pdf_add_new_dict (ctx, doc, 1);
    pdf_dict_put_drop (ctx, tree, PDF_NAME(ParentTree), pt);
    pdf_obj* nums= pdf_dict_put_array (ctx, pt, PDF_NAME(Nums),
                                       2 * (N(pages) + N(tag_annots)));
    array<array<int> >& pm= tags->page_mcids;
    for (int p=0; p<N(pages) && p<N(pm); p++) {
      pdf_array_push_int (ctx, nums, p);
      pdf_obj* a= pdf_array_push_array (ctx, nums, N(pm[p]));
      for (int m=0; m<N(pm[p]); m++)
        pdf_array_push (ctx, a, tag_objs[pm[p][m]]);
    }
    for (int i=0; i<N(tag_kids); i++)
      if (tag_kids[i].kind == 2) {
        pdf_array_push_int (ctx, nums, annots_base + tag_kids[i].id);
        pdf_array_push (ctx, nums, tag_objs[tag_kids[i].parent]);
      }
    // the forms of the grafted pictures: their elements by MCID
    array<array<int> >& sm= tags->stream_mcids;
    for (int s=0; s<N(sm); s++) {
      pdf_array_push_int (ctx, nums, pdf_tag_stream_key0 + s);
      pdf_obj* a= pdf_array_push_array (ctx, nums, N(sm[s]));
      for (int m=0; m<N(sm[s]); m++)
        if (sm[s][m] >= 0) pdf_array_push (ctx, a, tag_objs[sm[s][m]]);
        else pdf_array_push (ctx, a, PDF_NULL);
    }
    pdf_dict_puts_drop (ctx, tree, "ParentTreeNextKey",
                        pdf_new_int (ctx, N(sm) > 0? pdf_tag_stream_key0 + N(sm):
                                          annots_base + N(tag_annots)));
    // the configurations of the layers (merged from the pictures,
    // merge_layers) have a name (PDF/UA)
    pdf_obj* ocp= pdf_dict_get (ctx, root, PDF_NAME(OCProperties));
    if (ocp != NULL) {
      pdf_obj* dd= pdf_dict_get (ctx, ocp, PDF_NAME(D));
      if (dd != NULL && pdf_dict_get (ctx, dd, PDF_NAME(Name)) == NULL)
        pdf_dict_put_text_string (ctx, dd, PDF_NAME(Name), "Default");
      pdf_obj* cf= pdf_dict_get (ctx, ocp, PDF_NAME(Configs));
      for (int i=0; i < pdf_array_len (ctx, cf); i++) {
        pdf_obj* c= pdf_array_get (ctx, cf, i);
        if (pdf_dict_get (ctx, c, PDF_NAME(Name)) == NULL)
          pdf_dict_put_text_string (ctx, c, PDF_NAME(Name), "Configuration");
      }
    }
    // the catalog
    pdf_obj* mi= pdf_new_dict (ctx, doc, 1);
    pdf_dict_puts_drop (ctx, root, "MarkInfo", mi);
    pdf_dict_puts_drop (ctx, mi, "Marked", PDF_TRUE);
    if (N(lang) > 1) pdf_dict_put_text_string (ctx, root, PDF_NAME(Lang), &lang[0]);
    pdf_obj* vp= pdf_new_dict (ctx, doc, 1);
    pdf_dict_puts_drop (ctx, root, "ViewerPreferences", vp);
    pdf_dict_puts_drop (ctx, vp, "DisplayDocTitle", PDF_TRUE);
    xb= fz_new_buffer_from_copied_data (ctx, (unsigned char*) &xmp[0],
                                        (size_t) N(xmp));
    pdf_obj* md= pdf_new_dict (ctx, doc, 2);
    pdf_dict_put (ctx, md, PDF_NAME(Type), PDF_NAME(Metadata));
    pdf_dict_put (ctx, md, PDF_NAME(Subtype), PDF_NAME(XML));
    pdf_obj* ms= pdf_add_stream (ctx, doc, xb, md, 0);
    pdf_drop_obj (ctx, md);
    pdf_dict_put_drop (ctx, root, PDF_NAME(Metadata), ms);
  }
  fz_always (ctx) fz_drop_buffer (ctx, xb);
  fz_catch (ctx) {
    convert_warning << "MuPDF could not write the structure of the PDF: "
                    << fz_caught_message (ctx) << LF;
  }
}

// The outline, as the tree its levels describe. An entry hangs under the
// last one of a smaller level; the count of an entry is negative when its
// children start folded, which is what a reader expects of a deep tree.
// The tree is worked out first, and the objects made from it after, in an
// fz_try which creates nothing with a destructor (see put).
void
mupdf_pdf_renderer_rep::write_outline () {
  // entry 0 is the root, the /Outlines dictionary; entries 1..n are the
  // outline items which point at a page there is; 0 as a link means none
  array<int> item;      // item[j]: the index in outlines of entry j
  item << -1;
  for (int i=0; i<N(outlines); i++)
    if (outlines[i].page >= 0 && outlines[i].page < N(pages)) item << i;
  int n= N(item) - 1;
  if (n == 0) return;
  array<int> par, fst, lst, prv, nxt, cnt;
  array<string> title;  // with a 0 at the end, to be a C string
  for (int j=0; j<=n; j++) {
    par << 0; fst << 0; lst << 0; prv << 0; nxt << 0; cnt << 0;
    title << (j == 0 ? string () : outlines[item[j]].title * string ((char) 0));
  }
  // the open entries, from the root down, and their levels
  array<int> open;  open << 0;
  array<int> lev;   lev  << 0;
  for (int j=1; j<=n; j++) {
    int l= outlines[item[j]].level;
    while (N(open) > 1 && lev[N(lev)-1] >= l) {
      open->resize (N(open) - 1); lev->resize (N(lev) - 1);
    }
    int p= open[N(open)-1];
    par[j]= p;
    if (fst[p] == 0) fst[p]= j;
    else { nxt[lst[p]]= j; prv[j]= lst[p]; }
    lst[p]= j; cnt[p]++;
    open << j; lev << l;
  }
  array<pdf_obj*> ref;  // the objects of the entries (0: /Outlines)
  for (int j=0; j<=n; j++) ref << ((pdf_obj*) NULL);
  pdf_obj* entry= NULL;
  fz_var (entry);
  fz_try (ctx) {
    // /Outlines is an object of its own, an indirect one: every entry of
    // the first level points at it as its /Parent. A direct dictionary put
    // there would be shared by all of them -- invalid, and renumbered once
    // for each when the file is compacted, which sent /First to whatever
    // object ended up with the number (a link, in one file)
    pdf_obj* root= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Root));
    ref[0]= pdf_add_new_dict (ctx, doc, 4);
    pdf_dict_put (ctx, ref[0], PDF_NAME(Type), PDF_NAME(Outlines));
    pdf_dict_put (ctx, root, PDF_NAME(Outlines), ref[0]);
    for (int j=1; j<=n; j++) {
      pdf_outline_item& o= outlines[item[j]];
      entry= pdf_new_dict (ctx, doc, 6);
      pdf_dict_put_text_string (ctx, entry, PDF_NAME(Title), &title[j][0]);
      pdf_obj* dest= pdf_dict_put_array (ctx, entry, PDF_NAME(Dest), 5);
      pdf_array_push (ctx, dest, pages[o.page]);
      pdf_array_push (ctx, dest, PDF_NAME(XYZ));
      pdf_array_push_real (ctx, dest, o.x);
      pdf_array_push_real (ctx, dest, o.y);
      pdf_array_push (ctx, dest, PDF_NULL);   // the zoom: the reader's, kept
      pdf_obj* e= entry; entry= NULL;  // pdf_add_object_drop drops it
      ref[j]= pdf_add_object_drop (ctx, doc, e);
    }
    for (int j=1; j<=n; j++) {
      pdf_dict_put (ctx, ref[j], PDF_NAME(Parent), ref[par[j]]);
      if (prv[j] != 0) pdf_dict_put (ctx, ref[j], PDF_NAME(Prev), ref[prv[j]]);
      if (nxt[j] != 0) pdf_dict_put (ctx, ref[j], PDF_NAME(Next), ref[nxt[j]]);
    }
    for (int j=0; j<=n; j++) {
      if (fst[j] == 0) continue;
      pdf_dict_put (ctx, ref[j], PDF_NAME(First), ref[fst[j]]);
      pdf_dict_put (ctx, ref[j], PDF_NAME(Last), ref[lst[j]]);
      pdf_dict_put_int (ctx, ref[j], PDF_NAME(Count), j == 0 ? cnt[j] : -cnt[j]);
    }
  }
  fz_always (ctx) {
    pdf_drop_obj (ctx, entry);
    for (int j=0; j<=n; j++) pdf_drop_obj (ctx, ref[j]);
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF could not write the outline: "
                    << fz_caught_message (ctx) << LF;
  }
}

// The places the links point at, as the name tree of the catalogue. The
// names must be sorted, which is what a reader relies on to find one.
void
mupdf_pdf_renderer_rep::write_dests () {
  if (N(dest_name) == 0) return;
  array<int> ord;
  for (int i=0; i<N(dest_name); i++) {
    int page= (int) dest_pos[i].x1;
    if (page >= 0 && page < N(pages)) ord << i;
  }
  // the keys, in the bytes the links name them by (see pdf_text_bytes),
  // sorted by those bytes, which is the order a reader searches them in
  array<string> key;
  for (int i=0; i<N(dest_name); i++) key << pdf_text_bytes (dest_name[i]);
  for (int i=1; i<N(ord); i++)     // few and nearly sorted: insertion
    for (int j=i; j>0 && bytes_less (key[ord[j]], key[ord[j-1]]); j--) {
      int t= ord[j]; ord[j]= ord[j-1]; ord[j-1]= t;
    }
  fz_try (ctx) {
    pdf_obj* root= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Root));
    pdf_obj* names= pdf_dict_put_dict (ctx, root, PDF_NAME(Names), 1);
    pdf_obj* dests= pdf_dict_put_dict (ctx, names, PDF_NAME(Dests), 1);
    pdf_obj* arr= pdf_dict_put_array (ctx, dests, PDF_NAME(Names),
                                      2 * N(ord));
    for (int i=0; i<N(ord); i++) {
      pdf_link_item& pos= dest_pos[ord[i]];
      string& k= key[ord[i]];
      pdf_array_push_string (ctx, arr, &k[0], N(k));
      pdf_obj* d= pdf_array_push_array (ctx, arr, 5);
      pdf_array_push (ctx, d, pages[(int) pos.x1]);
      pdf_array_push (ctx, d, PDF_NAME(XYZ));
      pdf_array_push_real (ctx, d, pos.y1);
      pdf_array_push_real (ctx, d, pos.x2);
      // the zoom: null, the reader's own, as pdf_hummus_renderer writes it;
      // 0 means the same in the specification, but MuPDF takes it for 100%
      pdf_array_push (ctx, d, PDF_NULL);
    }
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF could not write the destinations: "
                    << fz_caught_message (ctx) << LF;
  }
}

void
mupdf_pdf_renderer_rep::write_metadata () {
  // with a 0 at the end, to be C strings; "" when absent
  string title, author, subject;
  if (metadata->contains ("title")) title= metadata ("title") * string ((char) 0);
  if (metadata->contains ("author")) author= metadata ("author") * string ((char) 0);
  if (metadata->contains ("subject")) subject= metadata ("subject") * string ((char) 0);
  fz_try (ctx) {
    pdf_obj* info= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Info));
    if (info == NULL) {
      info= pdf_add_new_dict (ctx, doc, 8);
      pdf_dict_put_drop (ctx, pdf_trailer (ctx, doc), PDF_NAME(Info), info);
    }
    if (N(title) > 0) pdf_dict_put_text_string (ctx, info, PDF_NAME(Title), &title[0]);
    if (N(author) > 0) pdf_dict_put_text_string (ctx, info, PDF_NAME(Author), &author[0]);
    if (N(subject) > 0) pdf_dict_put_text_string (ctx, info, PDF_NAME(Subject), &subject[0]);
    // as pdf_hummus_renderer: TeXmacs made it, with the library named
    pdf_dict_put_text_string (ctx, info, PDF_NAME(Creator), "TeXmacs " TEXMACS_VERSION);
    pdf_dict_put_text_string (ctx, info, PDF_NAME(Producer),
                              "TeXmacs " TEXMACS_VERSION " + MuPDF " FZ_VERSION);
    pdf_dict_put_date (ctx, info, PDF_NAME(CreationDate), (int64_t) time (NULL));
    // RichMedia (the videos) is of PDF 1.7 with Adobe's extension level 3,
    // which the catalog declares, as pdf_hummus_renderer does
    if (N(video_fs) > 0) {
      pdf_obj* root= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Root));
      pdf_dict_put_name (ctx, root, PDF_NAME(Version), "1.7");
      pdf_obj* ext= pdf_new_dict (ctx, doc, 1);
      pdf_dict_puts_drop (ctx, root, "Extensions", ext);
      pdf_obj* adbe= pdf_new_dict (ctx, doc, 2);
      pdf_dict_puts_drop (ctx, ext, "ADBE", adbe);
      pdf_dict_puts_drop (ctx, adbe, "BaseVersion", pdf_new_name (ctx, "1.7"));
      pdf_dict_puts_drop (ctx, adbe, "ExtensionLevel", pdf_new_int (ctx, 3));
    }
  }
  fz_catch (ctx) {
    convert_warning << "MuPDF could not write the metadata: "
                    << fz_caught_message (ctx) << LF;
  }
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
  // what TeXmacs has to compute is computed first: the fz_try below must
  // create nothing with a destructor, since a throw is a longjmp
  c_string in (concretize (pdf_path));
  c_string out (concretize (out_path));
  bool objstms_ok= (get_env ("TEXMACS_PDF_OBJSTMS") != "0");
  array<string> att_names, att_bodies;
  for (int i=0; i<N(attachments); i++) {
    string body;
    if (load_string (attachments[i], body, false)) continue;  // unreadable
    att_names << (as_string (tail (attachments[i])) * string ((char) 0));
    att_bodies << body;
  }
  fz_var (doc); fz_var (ok);
  // a PDF written with a password is opened with it (see pdf_encryption),
  // and written again as it was encrypted (the default: keep)
  string upw= get_env ("TEXMACS_PDF_USER_PASSWORD");
  string opw= get_env ("TEXMACS_PDF_OWNER_PASSWORD");
  if (N(opw) == 0) opw= upw;
  c_string cpw (opw);
  fz_try (ctx) {
    doc= pdf_open_document (ctx, in);
    if (pdf_needs_password (ctx, doc) &&
        !pdf_authenticate_password (ctx, doc, cpw))
      fz_throw (ctx, FZ_ERROR_GENERIC, "the PDF is encrypted, and the password does not open it");
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
    for (int i=0; i<N(att_names); i++) {
      // the names end with a 0, so that they are C strings as they are
      const char* nm= &att_names[i][0];
      int len= N(att_bodies[i]);   // an empty string may have no storage
      fz_buffer* buf= fz_new_buffer_from_copied_data (ctx,
                        (unsigned char*) (len == 0? "": &att_bodies[i][0]),
                        (size_t) len);
      pdf_obj* fs= pdf_add_embedded_file (ctx, doc, nm,
                                          "application/octet-stream",
                                          buf, 0, 0, 0);
      fz_drop_buffer (ctx, buf);
      // In the name tree, a file specification written out in full, not the
      // reference MuPDF returns: the reader of pdf_hummus_extract_attachment
      // takes /Names, /EmbeddedFiles, their array and each specification as
      // direct objects, and finds nothing otherwise -- a PDF made here could
      // not be opened by a build with PDFHummus. /AF keeps the reference.
      // The direct one is made afresh, sharing no direct object with it.
      pdf_obj* ef= pdf_dict_get (ctx, fs, PDF_NAME(EF));
      pdf_array_push_string (ctx, arr, nm, strlen (nm));
      pdf_obj* d= pdf_array_push_dict (ctx, arr, 4);
      pdf_dict_put (ctx, d, PDF_NAME(Type), PDF_NAME(Filespec));
      pdf_dict_put_string (ctx, d, PDF_NAME(F), nm, strlen (nm));
      pdf_dict_put_text_string (ctx, d, PDF_NAME(UF), nm);
      pdf_obj* def= pdf_dict_put_dict (ctx, d, PDF_NAME(EF), 2);
      pdf_dict_put (ctx, def, PDF_NAME(F), pdf_dict_get (ctx, ef, PDF_NAME(F)));
      pdf_array_push_drop (ctx, af, fs);
    }
    pdf_write_options opts= pdf_default_write_options;
    opts.do_compress= 1;
    opts.do_garbage= 3;
    // (a tagged PDF keeps its structure in object streams, as it was made)
    pdf_obj* sroot= pdf_dict_get (ctx, pdf_trailer (ctx, doc), PDF_NAME(Root));
    if (objstms_ok && pdf_dict_get (ctx, sroot, PDF_NAME(StructTreeRoot)) != NULL)
      opts.do_use_objstms= 1;
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
