
/******************************************************************************
* MODULE     : mupdf_picture.cpp
* DESCRIPTION: Picture objects for MuPDF
* COPYRIGHT  : (C) 2022 Massimiliano Gubinelli, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mupdf_picture.hpp"

#include "file.hpp"
#include "image_files.hpp"
#include "effect.hpp"
#include "analyze.hpp"
#include "hashmap.hpp"
#include "Freetype/tt_file.hpp"

/******************************************************************************
* Protected MuPDF calls (see mupdf_picture.hpp)
******************************************************************************/

bool
mupdf_protected_call (const char* what, void (*f) (void*), void* data) {
  fz_context* ctx= mupdf_context ();
  int ok= 1;
  fz_var (ok);
  fz_try (ctx) {
    f (data);
  }
  fz_catch (ctx) {
    ok= 0;
    cout << "TeXmacs] MuPDF error in " << what << ": "
         << fz_caught_message (ctx) << LF;
  }
  return ok != 0;
}

fz_image*
mupdf_image_from_file (const char* path) {
  fz_context* ctx= mupdf_context ();
  fz_image* im= NULL;
  fz_var (im);
  fz_try (ctx) {
    im= fz_new_image_from_file (ctx, path);
  }
  fz_catch (ctx) {
    im= NULL;
    cout << "TeXmacs] MuPDF cannot load image " << path << ": "
         << fz_caught_message (ctx) << LF;
  }
  return im;
}

fz_image*
mupdf_image_from_pixmap (fz_pixmap* pix) {
  if (pix == NULL) return NULL;
  fz_context* ctx= mupdf_context ();
  fz_image* im= NULL;
  fz_var (im);
  fz_try (ctx) {
    im= fz_new_image_from_pixmap (ctx, pix, NULL);
  }
  fz_catch (ctx) {
    im= NULL;
    cout << "TeXmacs] MuPDF cannot make an image: "
         << fz_caught_message (ctx) << LF;
  }
  return im;
}

fz_pixmap*
mupdf_pixmap_from_image (fz_image* im) {
  if (im == NULL) return NULL;
  fz_context* ctx= mupdf_context ();
  fz_pixmap* pix= NULL;
  fz_var (pix);
  fz_try (ctx) {
    pix= fz_get_pixmap_from_image (ctx, im, NULL, NULL, NULL, NULL);
  }
  fz_catch (ctx) {
    pix= NULL;
    cout << "TeXmacs] MuPDF cannot decode an image: "
         << fz_caught_message (ctx) << LF;
  }
  return pix;
}

// The channel order of the window surfaces. SDL gives the format which
// suits the window best; that is BGRA on the platforms supported here, and
// native_picture_from_SDL_Surface wraps the surface with this colorspace.
// A pixmap which is going to be blitted into a window is allocated with it
// too, so that the blit is a copy per row rather than a conversion per
// pixel (see mupdf_renderer_rep::draw_pixmap_direct).
fz_colorspace*
mupdf_screen_colorspace () {
#ifdef __EMSCRIPTEN__
  // the canvas of the browser takes its pixels as R, G, B, A (SDL's window
  // surface is SDL_PIXELFORMAT_RGBA32 there)
  return fz_device_rgb (mupdf_context ());
#else
  return fz_device_bgr (mupdf_context ());
#endif
}

picture raw_load_xpm (url file_name); // Graphics/Pictures

bool
mupdf_image_size (url u, int& w, int& h) {
  fz_context* ctx= mupdf_context ();
  string suf= locase_all (suffix (u));
  if (suf == "xpm") {
    // MuPDF has no XPM: the size of the icon's svg or 1x png beside it
    // (misc/pixmaps, the icons of the manuals), else that of our reader
    url base= unglue (u, 4);
    url svg= glue (base, ".svg"), png= glue (base, ".png");
    if (exists (svg) && mupdf_image_size (svg, w, h)) return true;
    if (exists (png) && mupdf_image_size (png, w, h)) return true;
    picture p= raw_load_xpm (u);
    w= p->get_width (); h= p->get_height ();
    return w > 0 && h > 0;
  }
  c_string path (concretize (u));
  if (suf == "pdf" || suf == "svg") {
    float fw= 0, fh= 0;
    fz_document* doc= NULL;
    fz_page* page= NULL;
    bool ok= mupdf_protected ("mupdf_image_size", [&] () {
      doc= fz_open_document (ctx, path);
      page= fz_load_page (ctx, doc, 0);
      fz_rect r= fz_bound_page (ctx, page);
      fw= r.x1 - r.x0; fh= r.y1 - r.y0;
    });
    // dropped outside: the protected body must not leak them on an error
    if (page != NULL) fz_drop_page (ctx, page);
    if (doc != NULL) fz_drop_document (ctx, doc);
    if (!ok || fw <= 0 || fh <= 0) return false;
    w= (int) (fw + 0.5); h= (int) (fh + 0.5);
    return true;
  }
  fz_image* im= mupdf_image_from_file (path);
  if (im == NULL) return false;
  w= im->w; h= im->h; // a point per pixel, as the other loaders
  fz_drop_image (ctx, im);
  return w > 0 && h > 0;
}

fz_pixmap*
mupdf_new_pixmap (int w, int h) {
  fz_context* ctx= mupdf_context ();
  fz_pixmap* pix= NULL;
  fz_var (pix);
  fz_try (ctx) {
    pix= fz_new_pixmap (ctx, fz_device_rgb (ctx), w, h, NULL, 1);
  }
  fz_catch (ctx) {
    pix= NULL;
    cout << "TeXmacs] MuPDF cannot allocate a " << w << "x" << h
         << " pixmap: " << fz_caught_message (ctx) << LF;
  }
  // a 1x1 pixmap stands for what could not be allocated (out of memory
  // for a 1x1 pixmap would terminate the process, nothing to do then)
  if (pix == NULL) pix= fz_new_pixmap (ctx, fz_device_rgb (ctx), 1, 1, NULL, 1);
  fz_clear_pixmap (ctx, pix);
  return pix;
}

/******************************************************************************
* Abstract mupdf pictures
******************************************************************************/

mupdf_picture_rep::mupdf_picture_rep (fz_pixmap *_pix, int ox2, int oy2):
  pix (_pix), im (NULL),
  w (fz_pixmap_width (mupdf_context (), pix)),
  h (fz_pixmap_height (mupdf_context (), pix)),
  ox (ox2), oy (oy2), opaque (false) {
  fz_keep_pixmap (mupdf_context (), pix);
}

mupdf_picture_rep::~mupdf_picture_rep () {
  fz_drop_pixmap (mupdf_context (), pix);
  fz_drop_image (mupdf_context (), im);
}

picture_kind mupdf_picture_rep::get_type () { return picture_native; }
void* mupdf_picture_rep::get_handle () { return (void*) this; }

int mupdf_picture_rep::get_width () { return w; }
int mupdf_picture_rep::get_height () { return h; }
int mupdf_picture_rep::get_origin_x () { return ox; }
int mupdf_picture_rep::get_origin_y () { return oy; }
void mupdf_picture_rep::set_origin (int ox2, int oy2) { ox= ox2; oy= oy2; }

color
mupdf_picture_rep::internal_get_pixel (int x, int y) {
  unsigned char *samples= fz_pixmap_samples (mupdf_context (), pix);
  return  rgbap_to_argb (((color*)samples)[x+w*(h-1-y)]);
}

void
mupdf_picture_rep::internal_set_pixel (int x, int y, color c) {
  unsigned char *samples= fz_pixmap_samples (mupdf_context (), pix);
  ((color*)samples)[x+w*(h-1-y)]= argb_to_rgbap (c);
}

picture
mupdf_picture (fz_pixmap *_pix, int ox, int oy) {
  return (picture)tm_new<mupdf_picture_rep, fz_pixmap*,int,int> (_pix, ox, oy);
}

picture
as_mupdf_picture (picture pic) {
  if (pic->get_type () == picture_native) return pic;
  fz_pixmap *pix= mupdf_new_pixmap (pic->get_width (), pic->get_height ());
  picture ret= mupdf_picture (pix, pic->get_origin_x (), pic->get_origin_y ());
  fz_drop_pixmap (mupdf_context (), pix);
  ret->copy_from (pic); // FIXME: is this inefficient???
  return ret;
}

#ifdef MUPDF_RENDERER
picture
as_native_picture (picture pict) {
  return as_mupdf_picture (pict);
}

picture
native_picture (int w, int h, int ox, int oy) {
  fz_pixmap *pix= mupdf_new_pixmap (w, h);
  picture p= mupdf_picture (pix, ox, oy);
  fz_drop_pixmap (mupdf_context (), pix);
  return p;
}

// A picture which is opaque from the start and stays so: source-over on an
// opaque destination leaves the alpha at 255, whatever is drawn. Blitting
// such a picture needs no test per pixel, which is what the backing store
// of an editor is for: with the test the blit of a full window costs about
// 4.4 ms, without it 0.4 ms, the speed of a memcpy (the reordering of the
// channels into the window, which MuPDF does not do for us, is free once
// the loop has no branch in it).
picture
native_opaque_picture (int w, int h, int ox, int oy) {
  fz_context* ctx= mupdf_context ();
  fz_pixmap* pix= mupdf_new_pixmap (w, h);
  fz_clear_pixmap_with_value (ctx, pix, 0xff); // white, and opaque
  picture p= mupdf_picture (pix, ox, oy);
  ((mupdf_picture_rep*) p->get_handle ())->opaque= true;
  fz_drop_pixmap (ctx, pix);
  return p;
}
#endif

/******************************************************************************
* Rendering on images
******************************************************************************/

class mupdf_image_renderer_rep: public mupdf_renderer_rep {
public:
  picture pict;
  
public:
  mupdf_image_renderer_rep (picture pict, double zoom);
  void* get_data_handle ();
};

mupdf_image_renderer_rep::mupdf_image_renderer_rep (picture p, double zoom)
  : mupdf_renderer_rep (), pict (p)
{
  zoomf  = zoom;
  shrinkf= (int) tm_round (std_shrinkf / zoomf);
  pixel  = (SI)  tm_round ((std_shrinkf * PIXEL) / zoomf);
  thicken= (shrinkf >> 1) * PIXEL;

  int pw = p->get_width ();
  int ph = p->get_height ();
  int pox= p->get_origin_x ();
  int poy= p->get_origin_y ();

  ox = pox * pixel;
  oy = poy * pixel;
  /*
  cx1= 0;
  cy1= 0;
  cx2= pw * pixel;
  cy2= ph * pixel;
  */
  cx1= 0;
  cy1= -ph * pixel;
  cx2= pw * pixel;
  cy2= 0;

  mupdf_picture_rep* handle= (mupdf_picture_rep*) pict->get_handle ();
  begin (handle->pix);
}

void*
mupdf_image_renderer_rep::get_data_handle () {
  return (void*) this;
}

#ifdef MUPDF_RENDERER
renderer
picture_renderer (picture p, double zoomf) {
  return (renderer) tm_new<mupdf_image_renderer_rep> (p, zoomf);
}
#endif

/******************************************************************************
* Loading pictures
******************************************************************************/

picture raw_load_xpm (url file_name);

/******************************************************************************
* Vector pictures
*
* MuPDF draws SVG itself (its source/svg), so the icons of TeXmacs are
* rendered from their vector originals instead of from the rasters, sharp at
* every resolution and available in a light and a dark variant. Its parser
* reads the presentation attributes and the inline style attribute only, not
* a <style> element with class selectors: an icon written that way comes out
* black, so the files of misc/pixmaps carry their styles inline.
******************************************************************************/

// The value of the attribute a of the element starting at the tag t (the
// text from its '<' to its '>'), "" when it has none
static string
svg_attribute (string t, string a) {
  int i= 0;
  while (true) {
    i= search_forwards (a * "=", i, t);
    if (i < 0) return "";
    if (i > 0 && (t[i-1] == ' ' || t[i-1] == '\n' || t[i-1] == '\t' ||
                  t[i-1] == '\r')) break;
    i++;
  }
  i += N(a) + 1;
  if (i >= N(t) || (t[i] != '"' && t[i] != '\'')) return "";
  char q= t[i];
  int j= search_forwards (string (q), i + 1, t);
  return j < 0 ? string ("") : t (i + 1, j);
}

// a colour #rgb or #rrggbb as its components (false when it is not one)
static bool
svg_hex_color (string c, int& r, int& g, int& b) {
  c= trim_spaces (c);
  if (N(c) == 4 && c[0] == '#') c= string ("#") * c(1,2) * c(1,2) *
                                   c(2,3) * c(2,3) * c(3,4) * c(3,4);
  if (N(c) != 7 || c[0] != '#') return false;
  for (int i= 1; i < 7; i++) if (!is_hex_digit (c[i])) return false;
  r= from_hexadecimal (c (1, 3)); g= from_hexadecimal (c (3, 5)); b= from_hexadecimal (c (5, 7));
  return true;
}

// MuPDF does not draw the gradients of SVG: a fill or a stroke with one
// ("url(#id)") comes out black. The icons of some sets (neoclassical) are
// shaded with them: each reference to a gradient is replaced by the average
// of the colours of its stops (those of the gradient it refers to by href,
// when it has none of its own), which is the colour the shading is made
// around. Returns s itself when it has no gradient.
static string
svg_flatten_gradients (string s) {
  if (search_forwards ("Gradient", 0, s) < 0) return s;
  hashmap<string,string> stops_of (""), href_of ("");
  array<string> ids;
  int i= 0;
  while ((i= search_forwards ("Gradient", i, s)) >= 0) {
    int b= i;
    while (b > 0 && s[b] != '<') b--;
    int e= search_forwards (">", i, s);
    if (e < 0) break;
    string tag= s (b, e + 1);
    i= e;
    if (s[b+1] == '/') continue; // a closing tag
    string id= svg_attribute (tag, "id");
    if (id == "") continue;
    ids << id;
    string href= svg_attribute (tag, "xlink:href");
    if (href == "") href= svg_attribute (tag, "href");
    if (N(href) > 1 && href[0] == '#') href_of (id)= href (1, N(href));
    if (tag[N(tag)-2] == '/') continue; // no stops of its own
    int end= search_forwards ("Gradient>", e, s);
    if (end < 0) end= N(s);
    string body= s (e, end), cols;
    int k= 0;
    while ((k= search_forwards ("<stop", k, body)) >= 0) {
      int ke= search_forwards (">", k, body);
      if (ke < 0) break;
      string st= body (k, ke + 1);
      string c= svg_attribute (st, "stop-color");
      if (c == "") {
        string style= svg_attribute (st, "style");
        int p= search_forwards ("stop-color:", 0, style);
        if (p >= 0) {
          int q= search_forwards (";", p, style);
          c= style (p + 11, q < 0 ? N(style) : q);
        }
      }
      cols << trim_spaces (c) << " ";
      k= ke;
    }
    stops_of (id)= cols;
  }
  for (int n= 0; n < N(ids); n++) {
    string id= ids[n], cols= stops_of[id];
    for (int d= 0; d < 8 && cols == "" && href_of->contains (id); d++) {
      id= href_of[id];
      cols= stops_of[id];
    }
    array<string> l= tokenize (cols, " ");
    int r= 0, g= 0, b= 0, m= 0;
    for (int k= 0; k < N(l); k++) {
      int cr, cg, cb;
      if (svg_hex_color (l[k], cr, cg, cb)) { r += cr; g += cg; b += cb; m++; }
    }
    if (m == 0) continue;
    string col= "#" * as_hexadecimal (r / m, 2) * as_hexadecimal (g / m, 2) *
                as_hexadecimal (b / m, 2);
    s= replace (s, "url(#" * ids[n] * ")", col);
    s= replace (s, "url('#" * ids[n] * "')", col);
    s= replace (s, "url(\"#" * ids[n] * "\")", col);
  }
  return s;
}

/******************************************************************************
* The text of TeX fonts in an SVG
*
* MuPDF draws the text of an SVG in one of its standard fonts, whatever its
* family (svg-run.c), and the browser has those 14 fonts only. The pictures
* of the TikZ plugin in the browser (plugins/tikz/web/tm-tikz.js) have their
* text set by TeXmacs over them; what it cannot set (cmex, a rotated run...)
* stays in their SVG, marked data-tm-tex, its characters being U+F000 plus
* their position in the TeX font of the element. Such text becomes the
* outlines of its glyphs, from the Type 1 fonts of TeXmacs (the Blue Sky
* Computer Modern), whose built-in encodings give the glyph of a position.
******************************************************************************/

struct tex_svg_font {
  fz_font* font;
  array<string> names; // the glyph of each position, "" if none
};
static hashmap<string,pointer> tex_svg_fonts (NULL);

// the Type 1 font of TeXmacs of that name (cmr10...), NULL if none
static tex_svg_font*
tex_svg_font_get (string name) {
  if (tex_svg_fonts->contains (name)) return (tex_svg_font*) tex_svg_fonts[name];
  tex_svg_font* r= NULL;
  url u= tt_font_find (name);
  string data;
  if (!is_none (u) && suffix (u) == "pfb" && !load_string (u, data, false) &&
      N(data) > 6 && (unsigned char) data[0] == 0x80 && data[1] == 1) {
    // the encoding, in the clear text of the first segment of the file
    int n= ((unsigned char) data[2]) | ((unsigned char) data[3] << 8) |
           ((unsigned char) data[4] << 16) | ((unsigned char) data[5] << 24);
    string head= data (6, min (N(data), 6 + n));
    array<string> names (256);
    for (int i= 0; i < 256; i++) names[i]= "";
    int k= 0;
    while ((k= search_forwards ("dup ", k, head)) >= 0) {
      k += 4;
      int j= k, pos= 0;
      while (j < N(head) && is_digit (head[j])) pos= 10 * pos + (head[j++] - '0');
      if (j == k || j >= N(head) || head[j] != ' ' || j + 1 >= N(head) ||
          head[j+1] != '/') continue;
      int e= j + 2;
      while (e < N(head) && head[e] != ' ') e++;
      if (pos < 256) names[pos]= head (j + 2, e);
    }
    fz_context* ctx= mupdf_context ();
    fz_font* f= NULL;
    fz_buffer* buf= NULL;
    fz_var (f);
    fz_var (buf);
    fz_try (ctx) {
      c_string cd (data);
      buf= fz_new_buffer_from_copied_data (ctx, (const unsigned char*) (char*) cd, N(data));
      c_string cn (name);
      f= fz_new_font_from_buffer (ctx, cn, buf, 0, 0);
    }
    fz_always (ctx) fz_drop_buffer (ctx, buf);
    fz_catch (ctx) f= NULL;
    if (f != NULL) r= new tex_svg_font { f, names };
  }
  tex_svg_fonts (name)= (pointer) r;
  return r;
}

// a path of MuPDF as SVG path data
static void
tex_svg_moveto (fz_context*, void* a, float x, float y) {
  *((string*) a) << "M" << as_string (x) << " " << as_string (y); }
static void
tex_svg_lineto (fz_context*, void* a, float x, float y) {
  *((string*) a) << "L" << as_string (x) << " " << as_string (y); }
static void
tex_svg_curveto (fz_context*, void* a, float x1, float y1, float x2, float y2,
                 float x3, float y3) {
  *((string*) a) << "C" << as_string (x1) << " " << as_string (y1) << " "
                 << as_string (x2) << " " << as_string (y2) << " "
                 << as_string (x3) << " " << as_string (y3); }
static void
tex_svg_closepath (fz_context*, void* a) { *((string*) a) << "Z"; }

// the characters of a <text>: its character references (&#xF00B;), as
// tm-tikz.js writes them
static array<int>
tex_svg_codes (string s) {
  array<int> r;
  int i= 0;
  while (i < N(s)) {
    if (s[i] == '&' && i + 2 < N(s) && s[i+1] == '#') {
      int j= i + 2, c= 0;
      bool hex= j < N(s) && (s[j] == 'x' || s[j] == 'X');
      if (hex) j++;
      while (j < N(s) && s[j] != ';') {
        char d= s[j++];
        if (hex) c= 16 * c + (is_digit (d)? d - '0': (d | 32) - 'a' + 10);
        else c= 10 * c + (d - '0');
      }
      r << c;
      i= j + 1;
    }
    else r << (int) (unsigned char) s[i++];
  }
  return r;
}

// one <text> as <path>s, or "" if it cannot be
static string
tex_svg_text_to_paths (string tag, string body) {
  tex_svg_font* tf= tex_svg_font_get (svg_attribute (tag, "font-family"));
  if (tf == NULL) return "";
  fz_context* ctx= mupdf_context ();
  double x = as_double (svg_attribute (tag, "x"));
  double y = as_double (svg_attribute (tag, "y"));
  double sz= as_double (svg_attribute (tag, "font-size"));
  if (sz <= 0) sz= 10;
  string d;
  array<int> codes= tex_svg_codes (body);
  for (int i= 0; i < N(codes); i++) {
    int pos= codes[i] - 0xF000;
    if (pos < 0 || pos > 255 || tf->names[pos] == "") return "";
    c_string gn (tf->names[pos]);
    int gid= fz_encode_character_by_glyph_name (ctx, tf->font, gn);
    if (gid <= 0) return "";
    fz_path* p= NULL;
    fz_var (p);
    fz_try (ctx) {
      // the glyph at (x, y), the y axis of the SVG going down
      fz_matrix m= fz_make_matrix ((float) sz, 0, 0, (float) -sz, (float) x, (float) y);
      p= fz_outline_glyph (ctx, tf->font, gid, m);
      if (p != NULL) {
        fz_path_walker w= { tex_svg_moveto, tex_svg_lineto, tex_svg_curveto,
                            tex_svg_closepath, NULL, NULL, NULL, NULL };
        fz_walk_path (ctx, p, &w, &d);
      }
    }
    fz_always (ctx) fz_drop_path (ctx, p);
    fz_catch (ctx) return "";
    x += sz * fz_advance_glyph (ctx, tf->font, gid, 0);
  }
  string r= "<path d=\"" * d * "\"";
  string fill= svg_attribute (tag, "fill");
  if (fill != "") r << " fill=\"" << fill << "\"";
  string tr= svg_attribute (tag, "transform");
  if (tr != "") r << " transform=\"" << tr << "\"";
  return r * "/>";
}

// the SVG with its text of TeX fonts as outlines; s itself if it has none
static string
svg_outline_tex_text (string s) {
  if (search_forwards ("data-tm-tex", 0, s) < 0) return s;
  string r;
  int i= 0;
  while (true) {
    int a= search_forwards ("<text", i, s);
    if (a < 0) break;
    int b= search_forwards (">", a, s);
    int c= b < 0 ? -1 : search_forwards ("</text>", b, s);
    if (c < 0) break;
    string tag= s (a, b + 1);
    string paths;
    if (search_forwards ("data-tm-tex", 0, tag) >= 0)
      paths= tex_svg_text_to_paths (tag, s (b + 1, c));
    r << s (i, a);
    if (paths != "") r << paths;
    else r << s (a, c + 7);
    i= c + 7;
  }
  r << s (i, N(s));
  return r;
}

// The first page of the PDF u drawn in a box of w x h points, at scale
// device pixels per point, as mupdf_render_svg below (a side given as zero
// taken from the page, the proportions kept, centered, on a transparent
// ground). The renderer of MuPDF draws a PDF picture itself (draw_scalable,
// as vectors); the others ask for its pixels (scalable_image_rep::draw),
// which came from an external converter (image_to_png), and there is none
// in the browser: the picture was a question mark with the GPU renderer.
// NULL when the file cannot be read.
fz_pixmap*
mupdf_render_pdf (url u, int w, int h, int scale) {
  fz_context* ctx= mupdf_context ();
  c_string path (concretize (u));
  fz_document* doc= NULL;
  fz_page* page= NULL;
  fz_device* dev= NULL;
  fz_pixmap* pix= NULL;
  if (scale < 1) scale= 1;
  fz_var (doc);
  fz_var (page);
  fz_var (dev);
  fz_var (pix);
  fz_try (ctx) {
    doc= fz_open_document (ctx, path);
    page= fz_load_page (ctx, doc, 0);
    fz_rect b= fz_bound_page (ctx, page);
    float dw= b.x1 - b.x0, dh= b.y1 - b.y0;
    if (dw <= 0.0f || dh <= 0.0f) fz_throw (ctx, FZ_ERROR_GENERIC, "empty page");
    if (w <= 0 && h <= 0) { w= (int) (dw + 0.5f); h= (int) (dh + 0.5f); }
    else if (w <= 0) w= (int) ((dw * h) / dh + 0.5f);
    else if (h <= 0) h= (int) ((dh * w) / dw + 0.5f);
    if (w < 1) w= 1;
    if (h < 1) h= 1;
    float f= ((float) w) / dw;
    if (((float) h) / dh < f) f= ((float) h) / dh;
    pix= fz_new_pixmap (ctx, fz_device_rgb (ctx), w * scale, h * scale, NULL, 1);
    fz_clear_pixmap (ctx, pix); // transparent
    fz_matrix m= fz_concat (fz_translate (-b.x0, -b.y0),
                            fz_concat (fz_scale (f * scale, f * scale),
                                       fz_translate (0.5f * (w - f * dw) * scale,
                                                     0.5f * (h - f * dh) * scale)));
    dev= fz_new_draw_device (ctx, m, pix);
    fz_run_page (ctx, page, dev, fz_identity, NULL);
    fz_close_device (ctx, dev);
  }
  fz_always (ctx) {
    fz_drop_device (ctx, dev);
    fz_drop_page (ctx, page);
    fz_drop_document (ctx, doc);
  }
  fz_catch (ctx) {
    fz_drop_pixmap (ctx, pix);
    pix= NULL;
    cout << "TeXmacs] MuPDF cannot draw " << concretize (u) << ": "
         << fz_caught_message (ctx) << LF;
  }
  return pix;
}

// Draw u in a box of w x h points, at scale device pixels per point. A side
// given as zero is taken from the size the file declares; the drawing keeps
// its proportions and is centered in the box. NULL when the file cannot be
// read or parsed.
fz_pixmap*
mupdf_render_svg (url u, int w, int h, int scale) {
  fz_context* ctx= mupdf_context ();
  c_string path (concretize (u));
  fz_buffer* buf= NULL;
  fz_display_list* list= NULL;
  fz_device* dev= NULL;
  fz_pixmap* pix= NULL;
  float dw= 0.0f, dh= 0.0f;
  if (scale < 1) scale= 1;
  fz_var (buf);
  fz_var (list);
  fz_var (dev);
  fz_var (pix);
  fz_try (ctx) {
    buf= fz_read_file (ctx, path);
    {
      // the gradients as plain colours (see svg_flatten_gradients)
      unsigned char* data= NULL;
      size_t len= fz_buffer_storage (ctx, buf, &data);
      string text ((char*) data, (int) len);
      string flat= svg_outline_tex_text (svg_flatten_gradients (text));
      if (N(flat) != N(text) || flat != text) {
        fz_drop_buffer (ctx, buf);
        buf= NULL;
        c_string cs (flat);
        buf= fz_new_buffer_from_copied_data (ctx, (const unsigned char*) (char*) cs, N(flat));
      }
    }
    list= fz_new_display_list_from_svg (ctx, buf, NULL, NULL, &dw, &dh);
    if (dw <= 0.0f || dh <= 0.0f) fz_throw (ctx, FZ_ERROR_GENERIC, "empty svg");
    // the box: what was asked for, completed with what the file declares
    if (w <= 0 && h <= 0) { w= (int) (dw + 0.5f); h= (int) (dh + 0.5f); }
    else if (w <= 0) w= (int) ((dw * h) / dh + 0.5f);
    else if (h <= 0) h= (int) ((dh * w) / dw + 0.5f);
    if (w < 1) w= 1;
    if (h < 1) h= 1;
    float f= ((float) w) / dw;
    if (((float) h) / dh < f) f= ((float) h) / dh;
    pix= fz_new_pixmap (ctx, fz_device_rgb (ctx), w * scale, h * scale, NULL, 1);
    fz_clear_pixmap (ctx, pix); // transparent
    fz_matrix m= fz_concat (fz_scale (f * scale, f * scale),
                            fz_translate (0.5f * (w - f * dw) * scale,
                                          0.5f * (h - f * dh) * scale));
    dev= fz_new_draw_device (ctx, m, pix);
    fz_run_display_list (ctx, list, dev, fz_identity, fz_infinite_rect, NULL);
    fz_close_device (ctx, dev);
  }
  fz_always (ctx) {
    fz_drop_device (ctx, dev);
    fz_drop_display_list (ctx, list);
    fz_drop_buffer (ctx, buf);
  }
  fz_catch (ctx) {
    fz_drop_pixmap (ctx, pix);
    pix= NULL;
    cout << "TeXmacs] MuPDF cannot render " << path << ": "
         << fz_caught_message (ctx) << LF;
  }
  return pix;
}

picture
mupdf_load_svg (url u, int w, int h) {
  fz_pixmap* pix= mupdf_render_svg (u, w, h, retina_factor);
  if (pix == NULL) return picture ();
  picture pic= mupdf_picture (pix, 0, 0);
  fz_drop_pixmap (mupdf_context (), pix);
  return pic;
}

fz_image *
mupdf_load_image (url u) {
  //cout << "mupdf_load_image " << u << LF;
  fz_image *im = NULL;
  string suf= suffix (u);
  if (suf == "svg") {
      // at the size the file declares, and at the resolution we draw at:
      // mupdf_load_pixmap scales the result when a size is asked for
      fz_pixmap* pix= mupdf_render_svg (u, 0, 0, retina_factor);
      if (pix != NULL) {
        im= mupdf_image_from_pixmap (pix);
        fz_drop_pixmap (mupdf_context (), pix);
      }
    } else if ((suf == "jpg") || (suf == "png")) {
      // FIXME: add more supported formats
      c_string path (concretize (u));
      //cout << "path :" << path << LF;
      im= mupdf_image_from_file (path);
    } else if (suf == "xpm") {
      // try to load higher definition png equivalent if available
      url png_equiv= glue (unglue (u, 4), "_x4.png");
      if (exists(png_equiv)) {
        return mupdf_load_image (png_equiv);
      }
      png_equiv= glue (unglue (u, 4), "_x2.png");
      if (exists(png_equiv)) {
        return mupdf_load_image (png_equiv);
      }
      png_equiv= glue (unglue (u, 4), ".png");
      if (exists(png_equiv)) {
        return mupdf_load_image (png_equiv);
      }
      // ok, try to load the xpm finally
      picture xp= as_mupdf_picture (raw_load_xpm (u));
      fz_pixmap *pix= ((mupdf_picture_rep*)xp->get_handle())->pix;
      im= mupdf_image_from_pixmap (pix);
    }
  return im;
}

static fz_pixmap* mupdf_apply_effect (fz_pixmap* pix, tree eff, SI pixel);

fz_pixmap*
mupdf_load_pixmap (url u, int w, int h, tree eff, SI pixel) {
  // a vector picture asked at a size is drawn at that size, rather than at
  // its own and then scaled, which blurs it: an svg, or the svg an icon has
  // beside its xpm (misc/pixmaps: the icons the manuals show)
  url vec= u;
  if (suffix (u) == "xpm" && exists (glue (unglue (u, 4), ".svg")))
    vec= glue (unglue (u, 4), ".svg");
  fz_pixmap* drawn= NULL;
  if (suffix (vec) == "svg" && w > 0 && h > 0)
    drawn= mupdf_render_svg (vec, w, h, 1);
  else if (locase_all (suffix (u)) == "pdf")
    drawn= mupdf_render_pdf (u, w, h, 1);
  if (drawn != NULL) return mupdf_apply_effect (drawn, eff, pixel);

  fz_image *im = mupdf_load_image (u);

  if (im == NULL) {
    // attempt to convert to png
    url temp= url_temp (".png");
    image_to_png (u, temp, w, h);
    c_string path (as_string (temp));
    im= mupdf_image_from_file (path);
    remove (temp);
  }
  
  // Error Handling
  if (im == NULL) {
      cout << "TeXmacs] warning: cannot render " << concretize (u) << "\n";
      return NULL;
  }

  // Scaling
  fz_pixmap *pix= mupdf_pixmap_from_image (im);
  fz_drop_image (mupdf_context (), im); // we do not need it anymore
  if (pix == NULL) {
    cout << "TeXmacs] warning: cannot render " << concretize (u) << "\n";
    return NULL;
  }

  // Scaling to the requested size (patterns are given with a size)
  fz_context* ctx= mupdf_context ();
  if (w > 0 && h > 0 &&
      (fz_pixmap_width (ctx, pix) != w || fz_pixmap_height (ctx, pix) != h)) {
    fz_pixmap* scaled= NULL;
    mupdf_protected ("image scaling", [&] () {
      scaled= fz_scale_pixmap (ctx, pix, 0, 0, w, h, NULL);
    });
    if (scaled != NULL) {
      fz_drop_pixmap (ctx, pix);
      pix= scaled;
    }
    else cout << "TeXmacs] warning: cannot scale " << concretize (u) << "\n";
  }
  return mupdf_apply_effect (pix, eff, pixel);
}

// the effect eff applied to pix (which it takes), for mupdf_load_pixmap
static fz_pixmap*
mupdf_apply_effect (fz_pixmap* pix, tree eff, SI pixel) {
  if (eff != "") {
    effect e= build_effect (eff);
    picture src= mupdf_picture (pix, 0, 0);
    array<picture> a;
    a << src;
    picture pic= e->apply (a, pixel);
    picture dest= as_mupdf_picture (pic);
    mupdf_picture_rep* rep= (mupdf_picture_rep*) dest->get_handle ();
    fz_pixmap* tpix= rep->pix;
    fz_drop_pixmap (mupdf_context (), pix);
    pix= tpix;
  }
  return pix;
}

picture 
mupdf_load_picture (url file_name) {
  fz_image* fzim= mupdf_load_image (file_name);  
  fz_pixmap *pix= mupdf_pixmap_from_image (fzim);
  if (fzim != NULL) fz_drop_image (mupdf_context (), fzim); // not needed anymore
  if (pix == NULL) {
    cout << "TeXmacs] warning: cannot load picture " << file_name << "\n";
    pix= mupdf_new_pixmap (1, 1);
  }

  picture pic= mupdf_picture (pix, 0, 0);
  fz_drop_pixmap (mupdf_context (), pix);
  return pic;
}

// The theme in which the vector icons are looked up: misc/pixmaps/light and
// misc/pixmaps/dark hold one recoloured copy of every icon each, and both
// are on $TEXMACS_PIXMAP_PATH. The GUI sets it from its own theme.
static string mupdf_icon_theme= "light";

void
mupdf_set_icon_theme (string theme) {
  if (theme != "dark") theme= "light";
  mupdf_icon_theme= theme;
}

string
mupdf_get_icon_theme () { return mupdf_icon_theme; }

// The size in points at which an icon set is drawn is the name of the
// directory it sits in (modern/24x24/main, traditional/--x17, where a dash
// means that the side is free). The sizes the SVG files declare cannot be
// used instead: a few dozen of them, the flags in particular, declare the
// size of the drawing they were made from rather than that of the icon.
static void
icon_box_size (url dir, int& w, int& h) {
  w= h= 0;
  for (int i= 0; i < 2 && !is_none (dir) && !is_root (dir); i++) {
    string s= as_string (tail (dir));
    int k= search_forwards ("x", 0, s);
    if (k > 0) {
      string sw= s (0, k), sh= s (k+1, N(s));
      bool free_w= (sw == "--"), free_h= (sh == "--");
      if ((free_w || is_int (sw)) && (free_h || is_int (sh))) {
        w= free_w ? 0 : as_int (sw);
        h= free_h ? 0 : as_int (sh);
        return;
      }
    }
    dir= head (dir);
  }
}

// The icons ship in several variants: name.svg (vector, in a light and a
// dark version), name.xpm (the legacy 1x format), name.png (1x),
// name_x2.png (2x) and name_x4.png (4x), all of the same size in points.
// Draw the vector one when there is one, and otherwise pick the raster which
// matches the resolution we draw at (retina_factor device pixels per point),
// falling back on the smaller ones, then on the file which was asked for.
// An icon of a lower resolution than the one we draw at (a 1x png, or an
// xpm, where no _x2 variant exists) is enlarged by f: the widgets take the
// size of an icon in device pixels, so that at 2x such an icon was drawn at
// half its size (the tabs of the preferences)
static picture
mupdf_icon_at_resolution (picture p, int f) {
  if (f <= 1) return p;
  fz_context* ctx= mupdf_context ();
  fz_pixmap* pix= ((mupdf_picture_rep*) p->get_handle ())->pix;
  int w= fz_pixmap_width (ctx, pix), h= fz_pixmap_height (ctx, pix);
  fz_pixmap* scaled= NULL;
  mupdf_protected ("icon scaling", [&] () {
    scaled= fz_scale_pixmap (ctx, pix, 0, 0, f * w, f * h, NULL);
  });
  if (scaled == NULL) return p;
  picture q= mupdf_picture (scaled, 0, 0);
  fz_drop_pixmap (ctx, scaled);
  return q;
}

picture 
mupdf_load_xpm (url file_name) {
  if (suffix (file_name) != "xpm") return mupdf_load_picture (file_name);
  url base= unglue (file_name, 4); // without ".xpm"
  url svg= resolve (url ("$TEXMACS_PIXMAP_PATH") * url (mupdf_icon_theme) *
                    glue (tail (base), ".svg") |
                    glue (base, ".svg"));
  if (!is_none (svg)) {
    int w= 0, h= 0;
    icon_box_size (head (file_name), w, h);
    picture pic= mupdf_load_svg (svg, w, h);
    if (!is_nil (pic)) return pic;
  }
  array<string> tried;
  array<int> factor; // the resolution of each variant
  if (retina_factor >= 4) { tried << string ("_x4.png"); factor << 4; }
  if (retina_factor >= 2) { tried << string ("_x2.png"); factor << 2; }
  tried << string (".png"); factor << 1;
  for (int i= 0; i < N(tried); i++) {
    url variant= glue (base, tried[i]);
    if (exists (resolve ("$TEXMACS_PIXMAP_PATH" * variant)))
      return mupdf_icon_at_resolution (mupdf_load_picture (variant),
                                       retina_factor / factor[i]);
  }
  // the xpm itself, at 1x
  return mupdf_icon_at_resolution (mupdf_load_picture (file_name),
                                   retina_factor);
}  

#ifdef MUPDF_RENDERER
picture
load_picture (url u, int w, int h, tree eff, int pixel) {
  fz_pixmap* pix= mupdf_load_pixmap (u, w, h, eff, pixel);
  if (pix == NULL) return error_picture (w, h);
  picture p= mupdf_picture (pix, 0, 0);
  fz_drop_pixmap (mupdf_context(), pix);
  return p;
}

void
save_picture (url dest, picture p) {
  if (suffix(dest) != "png") {
    cout << "TeXmacs] warning: cannot save " << concretize (dest)
         << ", format not supported\n";
    return;
  }
  picture q= as_mupdf_picture (p);
  mupdf_picture_rep* pict= (mupdf_picture_rep*) q->get_handle ();
  if (exists (dest)) remove (dest);
  // a file which cannot be written is an error: caught, or it would end
  // the process (made before fz_try: a throw is a longjmp)
  c_string path= concretize (dest);
  fz_context* ctx= mupdf_context ();
  fz_output* out= NULL;
  fz_var (out);
  fz_try (ctx) {
    out= fz_new_output_with_path (ctx, path, 0);
    fz_write_pixmap_as_png (ctx, out, pict->pix);
    fz_close_output (ctx, out);
  }
  fz_always (ctx) { fz_drop_output (ctx, out); }
  fz_catch (ctx) {
    cout << "TeXmacs] cannot save " << concretize (dest) << ": "
         << fz_caught_message (ctx) << LF;
  }
}
#endif
