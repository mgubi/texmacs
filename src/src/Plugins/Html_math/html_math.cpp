
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

static scheme_tree
attr (string name, string val) {
  return tuple (name, scm_quote (val));
}

static scheme_tree
node (string name, scheme_tree attrs) {
  return tuple (name, attrs);
}

static scheme_tree
attrs_0 () {
  return tuple ("@");
}

static string
sanitize_id (string s) {
  string r;
  for (int i=0; i<N(s); i++)
    if (is_alpha (s[i]) || is_digit (s[i])) r << s[i];
    else r << '_';
  return r;
}

/******************************************************************************
* The definitions of a document
******************************************************************************/

// The cache of outlines lasts for the whole session, the list of definitions
// to write for the document (or the snippet) in progress up to html_math_defs.

static hashmap<string,string> glyph_ids ("?");  // font and code -> id, or ""
static hashmap<string,string> glyph_paths ("");  // id -> path
static hashset<string> pending_set;
static array<scheme_tree> pending;

static void
use_glyph (string id) {
  if (pending_set->contains (id)) return;
  pending_set->insert (id);
  scheme_tree a= attrs_0 ();
  a << attr ("id", id) << attr ("d", glyph_paths [id]);
  pending << node ("h:path", a);
}

static string
use_clip (SI x1, SI y1, SI x2, SI y2, string rect_x, string rect_y,
          string rect_w, string rect_h) {
  string id= "tm-c" * as_string (x1) * "_" * as_string (y1) * "_" *
             as_string (x2) * "_" * as_string (y2);
  id= replace (id, "-", "m");
  if (pending_set->contains (id)) return id;
  pending_set->insert (id);
  scheme_tree r= attrs_0 ();
  r << attr ("x", rect_x) << attr ("y", rect_y)
    << attr ("width", rect_w) << attr ("height", rect_h);
  scheme_tree a= attrs_0 ();
  a << attr ("id", id);
  scheme_tree c= node ("h:clipPath", a);
  c << node ("h:rect", r);
  pending << c;
  return id;
}

scheme_tree
html_math_defs () {
  if (N(pending) == 0) return tree (TUPLE);
  scheme_tree a= attrs_0 ();
  a << attr ("xmlns", "http://www.w3.org/2000/svg")
    << attr ("aria-hidden", "true")
    << attr ("style", "position:absolute;width:0;height:0;overflow:hidden");
  scheme_tree defs= tuple ("h:defs");
  for (int i=0; i<N(pending); i++) defs << pending[i];
  scheme_tree r= node ("h:svg", a);
  r << defs;
  pending= array<scheme_tree> ();
  pending_set= hashset<string> ();
  return r;
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

static void
out_pair (string& d, int x, int y, bool first) {
  if (!first && x >= 0) d << " ";
  d << as_string (x);
  if (y >= 0) d << " ";
  d << as_string (y);
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
static string
bitmap_path (glyph gl) {
  string d;
  int w= gl->width, h= gl->height;
  int threshold= gl->depth > 1? (1 << (gl->depth - 1)): 1;
  array<bool> done (w * h);
  for (int k=0; k<w*h; k++) done[k]= false;
  int cx= 0, cy= 0;
  for (int j=0; j<h; j++)
    for (int i=0; i<w; i++) {
      if (done[i + j*w] || gl->get_x (i, j) < threshold) continue;
      int e= i;
      while (e < w && !done[e + j*w] && gl->get_x (e, j) >= threshold) e++;
      int f= j + 1;
      while (f < h) {
        bool same= true;
        for (int k=i; k<e && same; k++)
          same= !done[k + f*w] && gl->get_x (k, f) >= threshold;
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
  pencil pen;
  brush  fg_brush, bg_brush;
  double a, b, c, d, e, f;  // current affine transformation of absolute SI
  array<array<double> > tstack;
  array<scheme_tree> items;
  string last_clip;
  bool unsupported;

  html_math_renderer_rep (SI em2):
    renderer_rep (false), em (em2), pen (black), fg_brush (black),
    bg_brush (white), a (1.0), b (0.0), c (0.0), d (1.0), e (0.0), f (0.0),
    last_clip (""), unsupported (false)
  {
    cx1= cy1= -no_clip;
    cx2= cy2=  no_clip;
  }

  bool is_printer () { return true; }

  /* coordinates of the SVG, 1000 units per em going downwards */
  double dx (double x) { return 1000.0 * x / ((double) em); }
  double dy (double y) { return -1000.0 * y / ((double) em); }
  double tx (SI x, SI y) {
    double X= (double) (x + ox), Y= (double) (y + oy);
    return dx (a * X + c * Y + e); }
  double ty (SI x, SI y) {
    double X= (double) (x + ox), Y= (double) (y + oy);
    return dy (b * X + d * Y + f); }
  string pt (SI x, SI y) { return num (tx (x, y), 1) * " " * num (ty (x, y), 1); }

  /* transformations */
  void set_transformation (frame fr);
  void reset_transformation ();

  /* output */
  void emit (scheme_tree t);
  void add_paint (scheme_tree& at, bool fill, bool stroke);
  void path (string dd, bool fill, bool stroke);
  void rect (SI x1, SI y1, SI x2, SI y2, color col);

  /* graphical state */
  pencil get_pencil () { return pen; }
  brush  get_brush () { return fg_brush; }
  brush  get_background () { return bg_brush; }
  void   set_pencil (pencil p) { pen= p; }
  void   set_brush (brush b2) { fg_brush= b2; }
  void   set_background (brush b2) { bg_brush= b2; }

  /* drawing */
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
  string arc_path (SI x1, SI y1, SI x2, SI y2, int alpha, int delta,
                   bool sector);

  /* regions and images: formulas with these are exported in another way */
  void fetch (SI, SI, SI, SI, renderer, SI, SI) {}
  void new_shadow (renderer& ren) { ren= this; }
  void delete_shadow (renderer& ren) { ren= NULL; }
  void get_shadow (renderer, SI, SI, SI, SI) {}
  void put_shadow (renderer, SI, SI, SI, SI) {}
  void apply_shadow (SI, SI, SI, SI) {}
  void draw_picture (picture pic, SI x, SI y, int alpha= 255) {
    (void) pic; (void) x; (void) y; (void) alpha; unsupported= true; }
  void draw_scalable (scalable im, SI x, SI y, int alpha= 255) {
    (void) im; (void) x; (void) y; (void) alpha; unsupported= true; }
};

/******************************************************************************
* Transformations
******************************************************************************/

void
html_math_renderer_rep::set_transformation (frame fr) {
  // fr acts on the coordinates relative to the current origin
  if (!fr->linear) { unsupported= true; }
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
* Output
******************************************************************************/

void
html_math_renderer_rep::emit (scheme_tree t) {
  if (cx1 == -no_clip && cy1 == -no_clip &&
      cx2 ==  no_clip && cy2 ==  no_clip) {
    last_clip= "";
    items << t;
    return;
  }
  if (cx2 <= cx1 || cy2 <= cy1) return;
  // clips are rectangles in the coordinates of the formula
  string id= use_clip (cx1, cy1, cx2, cy2, num (dx (cx1), 1), num (dy (cy2), 1),
                       num (dx (cx2 - cx1), 1), num (dx (cy2 - cy1), 1));
  if (id != last_clip) {
    scheme_tree at= attrs_0 ();
    at << attr ("clip-path", "url(#" * id * ")");
    items << node ("h:g", at);
    last_clip= id;
  }
  items[N(items) - 1] << t;
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

// Black is the color of the text around the formula
void
html_math_renderer_rep::add_paint (scheme_tree& at, bool fill, bool stroke) {
  int alpha= 255;
  string s= color_string (pen->get_color (), alpha);
  if (stroke) {
    at << attr ("fill", "none")
       << attr ("stroke", s == "#000000"? string ("currentColor"): s)
       << attr ("stroke-width", num (dx (pen->get_width ()), 1));
    at << attr ("stroke-linecap", "round");
    if (alpha != 255)
      at << attr ("stroke-opacity", num (alpha / 255.0, 3));
  }
  else if (fill) {
    if (s != "#000000") at << attr ("fill", s);
    if (alpha != 255) at << attr ("fill-opacity", num (alpha / 255.0, 3));
  }
}

void
html_math_renderer_rep::path (string dd, bool fill, bool stroke) {
  scheme_tree at= attrs_0 ();
  at << attr ("d", dd);
  add_paint (at, fill, stroke);
  emit (node ("h:path", at));
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
  scheme_tree at= attrs_0 ();
  at << attr ("href", "#" * id);
  double A= a * sx, B= -b * sx, C= -c * sy, D= d * sy;
  if (fabs (B) < 1e-6 && fabs (C) < 1e-6 && fabs (A - D) < 1e-4) {
    if (fabs (A - 1.0) < 1e-4)
      at << attr ("x", num (X, 1)) << attr ("y", num (Y, 1));
    else
      at << attr ("transform", "translate(" * num (X, 1) * " " * num (Y, 1) *
                  ")scale(" * num (A, 4) * ")");
  }
  else
    at << attr ("transform", "matrix(" * num (A, 4) * " " * num (B, 4) * " " *
                num (C, 4) * " " * num (D, 4) * " " *
                num (X, 1) * " " * num (Y, 1) * ")");
  add_paint (at, true, false);
  emit (node ("h:use", at));
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
html_math_renderer (SI em) {
  return tm_new<html_math_renderer_rep> (em);
}

scheme_tree
html_math_svg (renderer ren, SI x1, SI y1, SI x2, SI y2) {
#ifdef USE_FREETYPE
  html_math_renderer_rep* r= (html_math_renderer_rep*) ren;
  if (r->unsupported) return tree (TUPLE);
  double em= (double) r->em;
  double vx= 1000.0 * x1 / em, vy= -1000.0 * y2 / em;
  double vw= 1000.0 * (x2 - x1) / em, vh= 1000.0 * (y2 - y1) / em;
  scheme_tree at= attrs_0 ();
  at << attr ("xmlns", "http://www.w3.org/2000/svg")
     << attr ("class", "tm-math")
     << attr ("aria-hidden", "true")
     << attr ("width", num (vw / 1000.0, 3) * "em")
     << attr ("height", num (vh / 1000.0, 3) * "em")
     << attr ("viewBox", num (vx, 1) * " " * num (vy, 1) * " " *
                         num (vw, 1) * " " * num (vh, 1))
     << attr ("style", "vertical-align:" * num (y1 / em, 3) *
                       "em;overflow:visible");
  scheme_tree s= node ("h:svg", at);
  for (int i=0; i<N(r->items); i++) s << r->items[i];
  return s;
#else
  (void) ren; (void) x1; (void) y1; (void) x2; (void) y2;
  return tree (TUPLE);
#endif
}
