
/******************************************************************************
* MODULE     : html_math.cpp
* DESCRIPTION: static formulas for the HTML export, as SVG
* COPYRIGHT  : (C) 2026 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Html_math/html_math.hpp"
#include "Freetype/tt_face.hpp"
#include "analyze.hpp"
#include "array.hpp"
#include "bitmap_font.hpp"
#include "brush.hpp"
#include "colors.hpp"
#include "converter.hpp"
#include "frame.hpp"
#include "gui.hpp"
#include "hashmap.hpp"
#include "pencil.hpp"
#include <math.h>
#include <stdio.h>

#ifdef USE_FREETYPE
#include FT_OUTLINE_H
#endif

/******************************************************************************
* Numbers and attributes
******************************************************************************/

static string
num (double x, int digits= 3) {
  // the shortest decimal notation with at most the given number of decimals
  char buf[64];
  snprintf (buf, sizeof (buf), "%.*f", digits, x);
  string s (buf);
  if (digits > 0) {
    int n= N(s);
    while (n > 0 && s[n-1] == '0') n--;
    if (n > 0 && s[n-1] == '.') n--;
    s= s (0, n);
  }
  if (s == "-0" || s == "") return "0";
  return s;
}

static string
attr (string name, string val) {
  return " " * name * "=\"" * val * "\"";
}

// A Cork string as the value of an attribute: the writer of HTML puts these
// out verbatim
static string
html_attribute (string s) {
  s= cork_to_utf8 (s);
  string r;
  for (int i=0; i<N(s); i++)
    if (s[i] == '&') r << "&amp;";
    else if (s[i] == '"') r << "&quot;";
    else if (s[i] == '<') r << "&lt;";
    else r << s[i];
  return r;
}

static void
out_pair (string& d, int x, int y, bool first) {
  if (!first && x >= 0) d << " ";
  d << as_string (x);
  if (y >= 0) d << " ";
  d << as_string (y);
}

#ifdef USE_FREETYPE
static string
sanitize_id (string s) {
  string r;
  for (int i=0; i<N(s); i++)
    if (is_alpha (s[i]) || is_digit (s[i])) r << s[i];
    else r << '_';
  return r;
}
#endif

/******************************************************************************
* The definitions of a document
******************************************************************************/

// The cache of outlines lasts for the whole session, the list of definitions
// to write for the document (or the snippet) in progress up to html_math_defs.

static hashmap<string,string> glyph_ids ("?");  // font and code -> id, or ""
static hashmap<string,string> glyph_paths ("");  // id -> path
static hashset<string> pending_set;
static string pending;

static void
use_glyph (string id) {
  if (pending_set->contains (id)) return;
  pending_set->insert (id);
  pending << "<path" << attr ("id", id) << attr ("d", glyph_paths [id]) << "/>";
}

static hashmap<string,string> clip_ids ("");  // clip and its parent -> id
static hashmap<string,string> clip_keys ("");  // id -> clip and its parent

static string
use_clip (string d, string parent) {
  // A clip in another one: browsers intersect a clipPath with its clip-path
  string key= d * "|" * parent;
  string id= clip_ids [key];
  if (id == "") {
    // a hash, as for the bitmaps: the path would make long ids
    string base= "tm-c" * as_hexadecimal (hash (key));
    id= base;
    for (int i=1; clip_keys->contains (id); i++)
      id= base * "x" * as_string (i);
    clip_ids (key)= id;
    clip_keys (id)= key;
  }
  if (pending_set->contains (id)) return id;
  pending_set->insert (id);
  pending << "<clipPath" << attr ("id", id);
  if (parent != "") pending << attr ("clip-path", "url(#" * parent * ")");
  pending << "><path" << attr ("d", d) << "/></clipPath>";
  return id;
}

scheme_tree
html_math_defs () {
  if (N(pending) == 0) return tree (TUPLE);
  string s= "<svg xmlns=\"http://www.w3.org/2000/svg\" aria-hidden=\"true\" "
            "style=\"position:absolute;width:0;height:0;overflow:hidden\">"
            "<defs>" * pending * "</defs></svg>";
  pending= "";
  pending_set= hashset<string> ();
  return scm_quote (s);
}

/******************************************************************************
* Outlines of glyphs
******************************************************************************/

#ifdef USE_FREETYPE

// Paths have 1000 units per em and go downwards, in relative coordinates;
// the rounded current point is tracked so that errors do not add up.

struct outline_state {
  string d;
  double scale;
  int    cx, cy;
  bool   open;
};

static inline int
out_x (outline_state* st, const FT_Vector* v) {
  return (int) floor (st->scale * v->x + 0.5);
}

static inline int
out_y (outline_state* st, const FT_Vector* v) {
  return (int) floor (0.5 - st->scale * v->y);
}

static int
outline_move (const FT_Vector* to, void* user) {
  outline_state* st= (outline_state*) user;
  int x= out_x (st, to), y= out_y (st, to);
  if (st->open) st->d << "z";
  st->d << "m";
  out_pair (st->d, x - st->cx, y - st->cy, true);
  st->cx= x; st->cy= y; st->open= true;
  return 0;
}

static int
outline_line (const FT_Vector* to, void* user) {
  outline_state* st= (outline_state*) user;
  int x= out_x (st, to), y= out_y (st, to);
  st->d << "l";
  out_pair (st->d, x - st->cx, y - st->cy, true);
  st->cx= x; st->cy= y;
  return 0;
}

static int
outline_conic (const FT_Vector* c, const FT_Vector* to, void* user) {
  outline_state* st= (outline_state*) user;
  int x= out_x (st, to), y= out_y (st, to);
  st->d << "q";
  out_pair (st->d, out_x (st, c) - st->cx, out_y (st, c) - st->cy, true);
  out_pair (st->d, x - st->cx, y - st->cy, false);
  st->cx= x; st->cy= y;
  return 0;
}

static int
outline_cubic (const FT_Vector* c1, const FT_Vector* c2,
               const FT_Vector* to, void* user) {
  outline_state* st= (outline_state*) user;
  int x= out_x (st, to), y= out_y (st, to);
  st->d << "c";
  out_pair (st->d, out_x (st, c1) - st->cx, out_y (st, c1) - st->cy, true);
  out_pair (st->d, out_x (st, c2) - st->cx, out_y (st, c2) - st->cy, false);
  out_pair (st->d, x - st->cx, y - st->cy, false);
  st->cx= x; st->cy= y;
  return 0;
}

// The family, the size and the resolutions of a font of tt_font_glyphs, whose
// name is family:size.hdpi[xvdpi]tt. The transformed fonts have names such as
// bolden[...] and the fonts of Metafont end in pk: these have no outlines.
static bool
parse_tt_name (string res, string& family, int& size, int& hdpi, int& vdpi) {
  if (!ends (res, "tt") || search_forwards ("[", res) >= 0) return false;
  int colon= search_backwards (":", res);
  if (colon <= 0) return false;
  int dot= search_forwards (".", colon, res);
  if (dot < 0) return false;
  string ss= res (colon + 1, dot);
  string dd= res (dot + 1, N(res) - 2);
  int x= search_forwards ("x", dd);
  string hh= x < 0? dd: dd (0, x);
  string vv= x < 0? dd: dd (x + 1, N(dd));
  if (!is_int (ss) || !is_int (hh) || !is_int (vv)) return false;
  family= res (0, colon);
  size= as_int (ss);
  hdpi= as_int (hh);
  vdpi= as_int (vv);
  return true;
}

// The id of the outline of a character of a font, "" for an empty glyph;
// false if the font has no outline for it.
static bool
tt_glyph_id (string family, int c, string& id) {
  string key= family * ":" * as_string (c);
  string known= glyph_ids [key];
  if (known != "?") {
    id= known;
    return known != "!";
  }
  tt_face face= load_tt_face (family);
  if (face->bad_face) { glyph_ids (key)= "!"; return false; }
  FT_Face ft= face->ft_face;
  FT_UInt index= decode_index (ft, c);
  if (ft_load_glyph (ft, index, FT_LOAD_NO_SCALE) != 0 ||
      ft->glyph->format != FT_GLYPH_FORMAT_OUTLINE ||
      ft->units_per_EM == 0) {
    glyph_ids (key)= "!";
    return false;
  }
  id= "";
  if (ft->glyph->outline.n_points > 0) {
    id= "tm-" * sanitize_id (family) * "-" * as_string ((int) index);
    if (!glyph_paths->contains (id)) {
      FT_Outline_Funcs funcs;
      funcs.move_to = outline_move;
      funcs.line_to = outline_line;
      funcs.conic_to= outline_conic;
      funcs.cubic_to= outline_cubic;
      funcs.shift= 0;
      funcs.delta= 0;
      outline_state st;
      st.scale= 1000.0 / ((double) ft->units_per_EM);
      st.cx= st.cy= 0;
      st.open= false;
      if (ft_outline_decompose (&ft->glyph->outline, &funcs, &st) != 0) {
        glyph_ids (key)= "!";
        return false;
      }
      if (st.open) st.d << "z";
      glyph_paths (id)= st.d;
    }
  }
  glyph_ids (key)= id;
  return true;
}

#endif // USE_FREETYPE

/******************************************************************************
* Bitmaps of glyphs, for what has no outlines
******************************************************************************/

// Rectangles of black pixels, in pixels from the origin of the glyph, going
// downwards: runs of each row, extended over the rows where they are the same.
// A row or column missing between two others is filled: where the slices of a
// stretched symbol meet, the compiled glyph has a pale seam there.
static string
bitmap_path (glyph gl) {
  string d;
  int w= gl->width, h= gl->height;
  int threshold= gl->depth > 1? (1 << (gl->depth - 1)): 1;
  array<bool> on (w * h);
  array<bool> done (w * h);
  for (int j=0; j<h; j++)
    for (int i=0; i<w; i++) {
      bool black= gl->get_x (i, j) >= threshold;
      if (!black && j > 0 && j < h-1)
        black= (gl->get_x (i, j-1) >= threshold &&
                gl->get_x (i, j+1) >= threshold);
      if (!black && i > 0 && i < w-1)
        black= (gl->get_x (i-1, j) >= threshold &&
                gl->get_x (i+1, j) >= threshold);
      on[i + j*w]= black;
      done[i + j*w]= false;
    }
  int cx= 0, cy= 0;
  for (int j=0; j<h; j++)
    for (int i=0; i<w; i++) {
      if (done[i + j*w] || !on[i + j*w]) continue;
      int e= i;
      while (e < w && !done[e + j*w] && on[e + j*w]) e++;
      int f= j + 1;
      while (f < h) {
        bool same= true;
        for (int k=i; k<e && same; k++)
          same= !done[k + f*w] && on[k + f*w];
        if (!same) break;
        f++;
      }
      for (int l=j; l<f; l++)
        for (int k=i; k<e; k++)
          done[k + l*w]= true;
      int x= i - gl->xoff, y= j - gl->yoff;
      d << "m";
      out_pair (d, x - cx, y - cy, true);
      d << "h" << as_string (e - i) << "v" << as_string (f - j)
        << "h" << as_string (i - e) << "z";
      cx= x; cy= y;
    }
  return d;
}

static string
bitmap_glyph_id (font_glyphs fn, int c) {
  string key= fn->res_name * "#" * as_string (c);
  string known= glyph_ids [key];
  if (known != "?") return known;
  string id= "";
  glyph gl= fn->get (c);
  if (!is_nil (gl) && gl->width * gl->height != 0) {
    // the names of transformed fonts are long: a hash, which may collide
    string base= "tm-b" * as_hexadecimal (hash (fn->res_name));
    id= base * "-" * as_string (c);
    for (int i=1; glyph_paths->contains (id); i++)
      id= base * "x" * as_string (i) * "-" * as_string (c);
    glyph_paths (id)= bitmap_path (gl);
  }
  glyph_ids (key)= id;
  return id;
}

/******************************************************************************
* The renderer
******************************************************************************/

static const SI no_clip= 1 << 29;

class html_math_renderer_rep: public renderer_rep {
public:
  SI em;
  string inherit;              // the color of the text around, as written here
  pencil pen;
  brush  fg_brush, bg_brush;
  double a, b, c, d, e, f;     // current affine transformation of absolute SI
  array<array<double> > tstack;
  string out;
  string last_clip;
  string clip_id;              // the current clip, "" when none
  array<string> clip_stack_ids;
  string held_key, held_single, held_plain, held_opacity;
  int    held_n;
  double held_x, held_y;
  bool   unsupported;

public:
  html_math_renderer_rep (SI em2, string inherit2):
    renderer_rep (false), em (em2), inherit (inherit2), pen (black),
    fg_brush (black), bg_brush (white),
    a (1.0), b (0.0), c (0.0), d (1.0), e (0.0), f (0.0),
    last_clip (""), clip_id (""), held_n (0), unsupported (false)
  {
    cx1= cy1= -no_clip;
    cx2= cy2=  no_clip;
  }

  bool is_printer () { return true; }
  // slices narrower than a pixel under clips leave seams in the browsers
  bool compiled_extensions () { return true; }

  /* coordinates of the SVG, 1000 units per em going downwards */
  double dx (double x) { return 1000.0 * x / ((double) em); }
  double dy (double y) { return -1000.0 * y / ((double) em); }
  double tx (SI x, SI y);
  double ty (SI x, SI y);
  string pt (SI x, SI y);

  /* transformations */
  void set_transformation (frame fr);
  void reset_transformation ();

  /* clipping */
  void set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);

  /* output */
  void flush_held ();
  void emit (string t);
  string paint (bool fill, bool stroke, bool outline= false);
  void path (string dd, bool fill, bool stroke);
  void rect (SI x1, SI y1, SI x2, SI y2, color col);

  /* graphical state */
  pencil get_pencil () { return pen; }
  brush  get_brush () { return fg_brush; }
  brush  get_background () { return bg_brush; }
  void   set_pencil (pencil p);
  void   set_brush (brush b2);
  void   set_background (brush b2) { bg_brush= b2; }

  /* links */
  void href (string label, SI x1, SI y1, SI x2, SI y2);

  /* drawing */
  void clear_device (SI, SI, SI, SI) {}
  void draw (int char_code, font_glyphs fn, SI x, SI y);
  void line (SI x1, SI y1, SI x2, SI y2);
  void lines (array<SI> x, array<SI> y);
  void clear (SI x1, SI y1, SI x2, SI y2);
  void fill (SI x1, SI y1, SI x2, SI y2);
  void arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void polygon (array<SI> x, array<SI> y, bool convex= true);
  string arc_path (SI x1, SI y1, SI x2, SI y2, int alpha, int delta,
                   bool sector);

  /* regions and images: formulas with these are exported in another way */
  void fetch (SI, SI, SI, SI, renderer, SI, SI) {}
  void new_shadow (renderer& ren) { ren= this; }
  void delete_shadow (renderer& ren) { ren= NULL; }
  void get_shadow (renderer, SI, SI, SI, SI) {}
  void put_shadow (renderer, SI, SI, SI, SI) {}
  void apply_shadow (SI, SI, SI, SI) {}
  void draw_picture (picture, SI, SI, int= 255) { unsupported= true; }
  void draw_scalable (scalable, SI, SI, int= 255) { unsupported= true; }
};

double
html_math_renderer_rep::tx (SI x, SI y) {
  double X= (double) (x + ox), Y= (double) (y + oy);
  return dx (a * X + c * Y + e);
}

double
html_math_renderer_rep::ty (SI x, SI y) {
  double X= (double) (x + ox), Y= (double) (y + oy);
  return dy (b * X + d * Y + f);
}

string
html_math_renderer_rep::pt (SI x, SI y) {
  return num (tx (x, y), 1) * " " * num (ty (x, y), 1);
}

/******************************************************************************
* Graphical state
******************************************************************************/

void
html_math_renderer_rep::set_pencil (pencil p) {
  if (p->get_type () == pencil_brush) unsupported= true;
  pen= p;
}

void
html_math_renderer_rep::set_brush (brush b2) {
  // as renderer_rep: what is filled has the color of the pencil
  if (b2->get_type () == brush_pattern) unsupported= true;
  fg_brush= b2;
  pen= pencil (b2->get_color ());
}

/******************************************************************************
* Transformations
******************************************************************************/

void
html_math_renderer_rep::set_transformation (frame fr) {
  // fr acts on the coordinates relative to the current origin
  if (!fr->linear) unsupported= true;
  array<double> saved;
  saved << a << b << c << d << e << f;
  tstack << saved;
  point o = fr (point (0.0, 0.0));
  point ux= fr (point (1.0, 0.0)) - o;
  point uy= fr (point (0.0, 1.0)) - o;
  double X= (double) ox, Y= (double) oy;
  // the frame in absolute coordinates: x -> F (x - O) + O
  double fa= ux[0], fb= ux[1], fc= uy[0], fd= uy[1];
  double fe= o[0] + X - fa * X - fc * Y;
  double ff= o[1] + Y - fb * X - fd * Y;
  // the current transformation applied after it
  double na= a * fa + c * fb;
  double nb= b * fa + d * fb;
  double nc= a * fc + c * fd;
  double nd= b * fc + d * fd;
  double ne= a * fe + c * ff + e;
  double nf= b * fe + d * ff + f;
  a= na; b= nb; c= nc; d= nd; e= ne; f= nf;
}

void
html_math_renderer_rep::reset_transformation () {
  if (N(tstack) == 0) return;
  array<double> t= tstack[N(tstack) - 1];
  tstack->resize (N(tstack) - 1);
  a= t[0]; b= t[1]; c= t[2]; d= t[3]; e= t[4]; f= t[5];
}

/******************************************************************************
* Clipping
******************************************************************************/

void
html_math_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2,
                                      bool restore) {
  flush_held ();
  renderer_rep::set_clipping (x1, y1, x2, y2, restore);
  if (restore) {
    if (N(clip_stack_ids) == 0) return;
    clip_id= clip_stack_ids[N(clip_stack_ids) - 1];
    clip_stack_ids->resize (N(clip_stack_ids) - 1);
  }
  else {
    clip_stack_ids << clip_id;
    // in the transformation at this point, which may change inside the clip
    string d= "M" * pt (x1, y1) * "L" * pt (x2, y1) * "L" * pt (x2, y2) *
              "L" * pt (x1, y2) * "z";
    clip_id= use_clip (d, clip_id);
  }
}

/******************************************************************************
* Output
******************************************************************************/

// Copies of a glyph with an alpha, nearly at the same place (a bold made by
// repeating it, see poor_bold.cpp), have the alpha as a group: otherwise it
// accumulates and they are nearly opaque.
void
html_math_renderer_rep::flush_held () {
  int n= held_n;
  if (n == 0) return;
  held_n= 0;
  if (n == 1) emit (held_single);
  else emit ("<g" * attr ("opacity", held_opacity) * ">" * held_plain * "</g>");
}

void
html_math_renderer_rep::emit (string t) {
  flush_held ();
  bool clipped= cx1 != -no_clip || cy1 != -no_clip ||
                cx2 !=  no_clip || cy2 !=  no_clip;
  if (clipped && (cx2 <= cx1 || cy2 <= cy1)) return;
  string id= clipped? clip_id: string ("");
  if (id != last_clip) {
    if (last_clip != "") out << "</g>";
    if (id != "") out << "<g clip-path=\"url(#" << id << ")\">";
    last_clip= id;
  }
  out << t;
}

static string
color_string (color col, int& alpha) {
  int r, g, b;
  get_rgb_color (col, r, g, b, alpha);
  static const char* hex= "0123456789abcdef";
  string s= "#";
  s << hex[r >> 4] << hex[r & 15]
    << hex[g >> 4] << hex[g & 15]
    << hex[b >> 4] << hex[b & 15];
  return s;
}

// What has the color of the text around the formula takes it in the HTML too
// (currentColor). Only the outlines of glyphs have the stroke of the formula
// (see html_math_svg): it would thicken rules, and be scaled with bitmap
// glyphs, which are large paths.
string
html_math_renderer_rep::paint (bool fill, bool stroke, bool outline) {
  string at;
  int alpha= 255;
  string s= color_string (pen->get_color (), alpha);
  if (stroke) {
    at << attr ("fill", "none")
       << attr ("stroke", s == inherit? string ("currentColor"): s)
       << attr ("stroke-width", num (dx (pen->get_width ()), 1))
       << attr ("stroke-linecap", "round");
    if (alpha != 255)
      at << attr ("stroke-opacity", num (alpha / 255.0, 3));
  }
  else if (fill) {
    if (s != inherit) at << attr ("fill", s);
    if (alpha != 255) at << attr ("fill-opacity", num (alpha / 255.0, 3));
    // with an alpha, a stroke would darken where it overlaps the fill
    if (!outline || alpha != 255) at << attr ("stroke", "none");
    else if (s != inherit) at << attr ("stroke", s);
  }
  return at;
}

void
html_math_renderer_rep::path (string dd, bool fill, bool stroke) {
  emit ("<path" * attr ("d", dd) * paint (fill, stroke) * "/>");
}

void
html_math_renderer_rep::href (string label, SI x1, SI y1, SI x2, SI y2) {
  if (x2 <= x1 || y2 <= y1) return;
  emit ("<a" * attr ("href", html_attribute (label)) * "><path" *
        attr ("d", "M" * pt (x1, y1) * "L" * pt (x2, y1) * "L" * pt (x2, y2) *
                   "L" * pt (x1, y2) * "z") *
        attr ("fill", "transparent") * attr ("stroke", "none") * "/></a>");
}

void
html_math_renderer_rep::draw (int ch, font_glyphs fn, SI x, SI y) {
  string id;
  double sx, sy;   // the scales of a glyph with respect to the text around
  bool done= false;
#ifdef USE_FREETYPE
  string family;
  int size, hdpi, vdpi;
  if (parse_tt_name (fn->res_name, family, size, hdpi, vdpi) &&
      tt_glyph_id (family, ch, id)) {
    sx= size * (hdpi / 72.0) * PIXEL / ((double) em);
    sy= size * (vdpi / 72.0) * PIXEL / ((double) em);
    done= true;
  }
#endif
  if (!done) {
    id= bitmap_glyph_id (fn, ch);
    sx= sy= 1000.0 * PIXEL / ((double) em);
  }
  if (id == "") return;
  use_glyph (id);

  double X= tx (x, y), Y= ty (x, y);
  string at= attr ("href", "#" * id);
  double A= a * sx, B= -b * sx, C= -c * sy, D= d * sy;
  if (fabs (B) < 1e-6 && fabs (C) < 1e-6 && fabs (A - D) < 1e-4) {
    if (fabs (A - 1.0) < 1e-4) {
      if (fabs (X) >= 0.05) at << attr ("x", num (X, 1));
      if (fabs (Y) >= 0.05) at << attr ("y", num (Y, 1));
    }
    else
      at << attr ("transform", "translate(" * num (X, 1) * " " * num (Y, 1) *
                  ")scale(" * num (A, 4) * ")");
  }
  else
    at << attr ("transform", "matrix(" * num (A, 4) * " " * num (B, 4) * " " *
                num (C, 4) * " " * num (D, 4) * " " *
                num (X, 1) * " " * num (Y, 1) * ")");
  int alpha= 255;
  string col= color_string (pen->get_color (), alpha);
  if (alpha != 255) {
    // a tenth of an em: other glyphs of a text are further away
    string key= id * col * as_string (alpha);
    if (held_n > 0 && (key != held_key ||
                       fabs (X - held_x) + fabs (Y - held_y) > 100.0))
      flush_held ();
    if (held_n == 0) {
      held_key= key;
      held_x= X; held_y= Y;
      held_opacity= num (alpha / 255.0, 3);
      held_plain= "";
      held_single= "<use" * at * paint (true, false, done) * "/>";
    }
    held_plain << "<use" << at;
    if (col != inherit) held_plain << attr ("fill", col);
    held_plain << attr ("stroke", "none") << "/>";
    held_n++;
    return;
  }
  emit ("<use" * at * paint (true, false, done) * "/>");
}

void
html_math_renderer_rep::line (SI x1, SI y1, SI x2, SI y2) {
  path ("M" * pt (x1, y1) * "L" * pt (x2, y2), false, true);
}

void
html_math_renderer_rep::lines (array<SI> x, array<SI> y) {
  int n= N(x);
  if (n < 1 || N(y) != n) return;
  string dd= "M" * pt (x[0], y[0]);
  for (int i=1; i<n; i++) dd << "L" * pt (x[i], y[i]);
  path (dd, false, true);
}

void
html_math_renderer_rep::rect (SI x1, SI y1, SI x2, SI y2, color col) {
  if (x2 <= x1 || y2 <= y1) return;
  pencil old= pen;
  pen= pencil (col);
  path ("M" * pt (x1, y1) * "L" * pt (x2, y1) * "L" * pt (x2, y2) *
        "L" * pt (x1, y2) * "z", true, false);
  pen= old;
}

void
html_math_renderer_rep::clear (SI x1, SI y1, SI x2, SI y2) {
  // the background of the page is the one of the HTML text
  color col= bg_brush->get_color ();
  if (col == white) return;
  rect (x1, y1, x2, y2, col);
}

void
html_math_renderer_rep::fill (SI x1, SI y1, SI x2, SI y2) {
  rect (x1, y1, x2, y2, pen->get_color ());
}

string
html_math_renderer_rep::arc_path (SI x1, SI y1, SI x2, SI y2,
                                  int alpha, int delta, bool sector) {
  // alpha and delta are in 64ths of degrees, counterclockwise
  double cx= (x1 + x2) / 2.0, cy= (y1 + y2) / 2.0;
  double rx= (x2 - x1) / 2.0, ry= (y2 - y1) / 2.0;
  int n= max (1, (int) ceil (fabs ((double) delta) / (64.0 * 45.0)));
  string dd;
  for (int i=0; i<=n; i++) {
    // polygonal enough at the resolution of the output; arcs of the SVG
    // would not survive the transformations of the renderer
    double t= (alpha + (delta * ((double) i)) / n) * (M_PI / (180.0 * 64.0));
    double px= cx + rx * cos (t), py= cy + ry * sin (t);
    if (i == 0) dd << "M" * pt ((SI) px, (SI) py);
    else {
      double t0= (alpha + (delta * ((double) (i-1))) / n) *
                 (M_PI / (180.0 * 64.0));
      double k= (4.0 / 3.0) * tan ((t - t0) / 4.0);
      double ax= cx + rx * (cos (t0) - k * sin (t0));
      double ay= cy + ry * (sin (t0) + k * cos (t0));
      double bx= px + rx * k * sin (t);
      double by= py - ry * k * cos (t);
      dd << "C" * pt ((SI) ax, (SI) ay) * " " * pt ((SI) bx, (SI) by) * " " *
            pt ((SI) px, (SI) py);
    }
  }
  if (sector) dd << "L" * pt ((SI) cx, (SI) cy) * "z";
  return dd;
}

void
html_math_renderer_rep::arc (SI x1, SI y1, SI x2, SI y2,
                             int alpha, int delta) {
  path (arc_path (x1, y1, x2, y2, alpha, delta, false), false, true);
}

void
html_math_renderer_rep::fill_arc (SI x1, SI y1, SI x2, SI y2,
                                  int alpha, int delta) {
  path (arc_path (x1, y1, x2, y2, alpha, delta, true), true, false);
}

void
html_math_renderer_rep::polygon (array<SI> x, array<SI> y, bool convex) {
  (void) convex;
  int n= N(x);
  if (n < 2 || N(y) != n) return;
  string dd= "M" * pt (x[0], y[0]);
  for (int i=1; i<n; i++) dd << "L" * pt (x[i], y[i]);
  path (dd * "z", true, false);
}

/******************************************************************************
* The interface
******************************************************************************/

renderer
html_math_renderer (SI em, color inherit) {
  int alpha;
  return tm_new<html_math_renderer_rep> (em, color_string (inherit, alpha));
}

scheme_tree
html_math_svg (renderer ren, SI x1, SI y1, SI x2, SI y2) {
#ifdef USE_FREETYPE
  html_math_renderer_rep* r= (html_math_renderer_rep*) ren;
  if (r->unsupported) return tree (TUPLE);
  r->flush_held ();
  double em= (double) r->em;
  double vx= 1000.0 * x1 / em, vy= -1000.0 * y2 / em;
  double vw= 1000.0 * (x2 - x1) / em, vh= 1000.0 * (y2 - y1) / em;
  // browsers do not draw an <svg> without width or height, whatever overflows
  if (vw < 1.0) vw= 1.0;
  if (vh < 1.0) vh= 1.0;
  if (r->last_clip != "") r->out << "</g>";
  r->last_clip= "";
  // the stroke: browsers thicken the glyphs of fonts, not paths, and
  // thin glyphs such as those of Computer Modern look pale without it
  string s= "<svg xmlns=\"http://www.w3.org/2000/svg\" aria-hidden=\"true\" "
            "fill=\"currentColor\" stroke=\"currentColor\" "
            "stroke-width=\"8\"";
  s << attr ("width", num (vw / 1000.0, 3) * "em")
    << attr ("height", num (vh / 1000.0, 3) * "em")
    << attr ("viewBox", num (vx, 1) * " " * num (vy, 1) * " " *
                        num (vw, 1) * " " * num (vh, 1))
    << attr ("style", "vertical-align:" * num (y1 / em, 3) *
                      "em;overflow:visible")
    << ">" << r->out << "</svg>";
  return scm_quote (s);
#else
  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2;
  return tree (TUPLE);
#endif
}
