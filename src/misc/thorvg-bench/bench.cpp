/******************************************************************************
* MODULE     : bench.cpp
* DESCRIPTION: How fast can ThorVG draw a screenful of a TeXmacs document?
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// A screenful of the document used to profile the Vue renderer (paragraphs
// of text in TeX Gyre Pagella, with a fraction rule and a radical in each),
// drawn by ThorVG in the browser, the way a renderer behind TeXmacs'
// immediate "draw this glyph here" interface would draw it. The mode is
// chosen in the address (texmacs.html?mode=...):
//
//   sw-glyphs   CPU engine, one new Shape per glyph at every frame
//   sw-runs     CPU engine, one new Shape per run of glyphs (a line)
//   gl-glyphs   GL engine (WebGL2), one new Shape per glyph at every frame
//   gl-runs     GL engine, one new Shape per run of glyphs
//   gl-kept     GL engine, the shapes made once and drawn again, moved by a
//               pixel at every frame (a scroll)
//   gl-atlas    GL engine for the page, the rules and the radicals; the
//               glyphs from an atlas of FreeType bitmaps, as textured quads
//               in one draw call made at every frame
//   gl-none     the page, the rules and the radicals without the text (what
//               the text costs in the other modes is the difference)
//   gl-slug     as gl-atlas, the glyphs drawn from their outlines by the
//               fragment shader (Slug: E. Lengyel, "GPU-Centered Font
//               Rendering Directly from Glyph Outlines", JCGT 6 (2), 2017),
//               one instance per glyph, in one draw call made at every frame
//
// ?em=<pixels> sets the size of the text (28 by default: 10pt at zoom 1 on
// a Retina screen); the atlas is made at that size, the outlines of Slug
// serve every size.
//
// Each frame is timed until the GPU is done (a readPixels of one pixel), and
// the median and the mean of FRAMES frames after WARMUP are printed:
//   RESULT mode=... glyphs=... median=... mean=... ms

#include <thorvg.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_OUTLINE_H
#include <emscripten.h>
#include <emscripten/html5.h>
#include <GLES3/gl3.h>
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <string>
#include <vector>

using namespace std;

static const int W= 2560, H= 1000;       // the editor of the profiled window
static float EM= 28.f;                   // 10pt at zoom 1 on a Retina screen (?em=)
static const int WARMUP= 20, FRAMES= 200;

/******************************************************************************
* The glyphs: outlines and bitmaps from FreeType
******************************************************************************/

struct glyph_path {
  vector<tvg::PathCommand> cmds;
  vector<tvg::Point> pts;    // pixels, y down, origin on the baseline
  float advance;
};

struct glyph_bitmap {
  int x, y, w, h;            // in the atlas
  int left, top;             // FreeType's bitmap_left, bitmap_top
};

static FT_Face face;
static vector<glyph_path> paths;     // by glyph index
static vector<glyph_bitmap> bitmaps;

struct decompose {
  glyph_path* g;
  float s;
  tvg::Point last;
};

static tvg::Point pt (decompose* d, const FT_Vector* v) {
  return tvg::Point { v->x * d->s, -v->y * d->s };
}

static int move_to (const FT_Vector* to, void* u) {
  decompose* d= (decompose*) u;
  if (!d->g->cmds.empty ()) d->g->cmds.push_back (tvg::PathCommand::Close);
  d->last= pt (d, to);
  d->g->cmds.push_back (tvg::PathCommand::MoveTo);
  d->g->pts.push_back (d->last);
  return 0;
}

static int line_to (const FT_Vector* to, void* u) {
  decompose* d= (decompose*) u;
  d->last= pt (d, to);
  d->g->cmds.push_back (tvg::PathCommand::LineTo);
  d->g->pts.push_back (d->last);
  return 0;
}

static int conic_to (const FT_Vector* c, const FT_Vector* to, void* u) {
  decompose* d= (decompose*) u;
  tvg::Point p0= d->last, p1= pt (d, c), p2= pt (d, to);
  d->g->cmds.push_back (tvg::PathCommand::CubicTo);
  d->g->pts.push_back ({ p0.x + 2.f/3.f*(p1.x-p0.x), p0.y + 2.f/3.f*(p1.y-p0.y) });
  d->g->pts.push_back ({ p2.x + 2.f/3.f*(p1.x-p2.x), p2.y + 2.f/3.f*(p1.y-p2.y) });
  d->g->pts.push_back (p2);
  d->last= p2;
  return 0;
}

static int cubic_to (const FT_Vector* c1, const FT_Vector* c2,
                     const FT_Vector* to, void* u) {
  decompose* d= (decompose*) u;
  d->g->cmds.push_back (tvg::PathCommand::CubicTo);
  d->g->pts.push_back (pt (d, c1));
  d->g->pts.push_back (pt (d, c2));
  d->last= pt (d, to);
  d->g->pts.push_back (d->last);
  return 0;
}

static const glyph_path& get_path (unsigned gi) {
  if (gi >= paths.size ()) paths.resize (gi + 1);
  glyph_path& g= paths[gi];
  if (g.advance != 0 || !g.cmds.empty ()) return g;
  FT_Load_Glyph (face, gi, FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING);
  decompose d { &g, EM / face->units_per_EM, { 0, 0 } };
  FT_Outline_Funcs f= { move_to, line_to, conic_to, cubic_to, 0, 0 };
  FT_Outline_Decompose (&face->glyph->outline, &f, &d);
  if (!g.cmds.empty ()) g.cmds.push_back (tvg::PathCommand::Close);
  g.advance= face->glyph->advance.x * d.s;
  if (g.advance == 0) g.advance= 0.001f;
  return g;
}

/******************************************************************************
* The page: positions of the glyphs, the rules, the radicals
******************************************************************************/

struct placed { unsigned gi; float x, y; };
struct rule { float x, y, w, h; };
struct line_run { int first, n; };   // glyphs of one line

static vector<placed> glyphs;
static vector<line_run> runs;
static vector<rule> rules;
static vector<vector<tvg::Point>> radicals; // polylines

static const float page_x= 470, page_w= 1620;

static void
lay_out () {
  const char* words= "lorem ipsum dolor sit amet, consectetur adipiscing elit, "
    "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
    "x n e x a b and some more text to fill the line, ut enim ad minim veniam. ";
  float lead= EM * 1.35f, x0= page_x + 60, x1= page_x + page_w - 60;
  float y= 40 + EM;
  int par= 190;
  while (y < H + EM) {
    string s= "Paragraph " + to_string (par++) + " " + words;
    float x= x0;
    int first= (int) glyphs.size ();
    size_t i= 0;
    int line_in_par= 0;
    while (i < s.size () && y < H + EM) {
      // the next word, moved to the next line if it does not fit
      size_t j= s.find (' ', i);
      if (j == string::npos) j= s.size ();
      float ww= 0;
      for (size_t k= i; k <= j && k < s.size (); k++)
        ww += get_path (FT_Get_Char_Index (face, (unsigned char) s[k])).advance;
      if (x + ww > x1 && x > x0) {
        runs.push_back ({ first, (int) glyphs.size () - first });
        if (line_in_par == 0) {
          // the formula of the paragraph: a fraction rule and a radical
          rules.push_back ({ x0 + 700, y - EM * 0.3f, EM * 1.2f, 1.5f });
          float rx= x0 + 900;
          radicals.push_back ({ { rx, y - EM*0.3f }, { rx + 5, y - EM*0.4f },
                                { rx + 12, y + 4 }, { rx + 22, y - EM*0.9f },
                                { rx + 140, y - EM*0.9f } });
        }
        line_in_par++;
        y += lead; x= x0; first= (int) glyphs.size ();
        if (y >= H + EM) break;
      }
      for (size_t k= i; k <= j && k < s.size (); k++) {
        unsigned gi= FT_Get_Char_Index (face, (unsigned char) s[k]);
        if (s[k] != ' ') glyphs.push_back ({ gi, floorf (x), y });
        x += get_path (gi).advance;
      }
      i= j + 1;
    }
    runs.push_back ({ first, (int) glyphs.size () - first });
    y += lead * 1.4f;
  }
}

/******************************************************************************
* Drawing with ThorVG
******************************************************************************/

static tvg::Canvas* canvas= NULL;
static uint32_t* sw_buffer= NULL;
static string mode;
static vector<tvg::Paint*> kept;
static tvg::Scene* kept_scene= NULL;

static tvg::Shape*
rect_shape (float x, float y, float w, float h, uint8_t g) {
  tvg::Shape* s= tvg::Shape::gen ();
  s->appendRect (x, y, w, h);
  s->fill (g, g, g, 255);
  return s;
}

static tvg::Shape*
glyph_shape (const placed& p) {
  const glyph_path& g= paths[p.gi];
  tvg::Shape* s= tvg::Shape::gen ();
  s->appendPath (g.cmds.data (), (uint32_t) g.cmds.size (),
                 g.pts.data (), (uint32_t) g.pts.size ());
  s->translate (p.x, p.y);
  s->fill (0, 0, 0, 255);
  return s;
}

// one shape for the glyphs of a run, their points moved to their places
static tvg::Shape*
run_shape (const line_run& r) {
  tvg::Shape* s= tvg::Shape::gen ();
  static vector<tvg::Point> pts;
  for (int k= r.first; k < r.first + r.n; k++) {
    const placed& p= glyphs[k];
    const glyph_path& g= paths[p.gi];
    pts.resize (g.pts.size ());
    for (size_t i= 0; i < g.pts.size (); i++)
      pts[i]= { g.pts[i].x + p.x, g.pts[i].y + p.y };
    s->appendPath (g.cmds.data (), (uint32_t) g.cmds.size (),
                   pts.data (), (uint32_t) pts.size ());
  }
  s->fill (0, 0, 0, 255);
  return s;
}

static tvg::Shape*
radical_shape (const vector<tvg::Point>& r) {
  tvg::Shape* s= tvg::Shape::gen ();
  s->moveTo (r[0].x, r[0].y);
  for (size_t i= 1; i < r.size (); i++) s->lineTo (r[i].x, r[i].y);
  s->strokeWidth (1.5f);
  s->strokeFill (0, 0, 0, 255);
  return s;
}

// the page, the rules and the radicals: what is not text
static void
add_page (bool glyphs_too) {
  canvas->add (rect_shape (0, 0, W, H, 160));             // the surround
  canvas->add (rect_shape (page_x, 0, page_w, H, 255));   // the page
  for (const rule& r: rules)
    canvas->add (rect_shape (r.x, r.y, r.w, r.h, 0));
  for (auto& r: radicals) canvas->add (radical_shape (r));
  if (!glyphs_too) return;
  if (mode == "sw-glyphs" || mode == "gl-glyphs")
    for (const placed& p: glyphs) canvas->add (glyph_shape (p));
  else
    for (const line_run& r: runs) canvas->add (run_shape (r));
}

/******************************************************************************
* The atlas: FreeType bitmaps, drawn as textured quads (gl-atlas)
******************************************************************************/

static GLuint atlas_tex, atlas_prog, atlas_vbo, atlas_vao;
static const int ATLAS= 1024;

static GLuint
compile (GLenum type, const char* src) {
  GLuint s= glCreateShader (type);
  glShaderSource (s, 1, &src, NULL);
  glCompileShader (s);
  GLint ok; glGetShaderiv (s, GL_COMPILE_STATUS, &ok);
  if (!ok) { char log[1000]; glGetShaderInfoLog (s, 1000, NULL, log); printf ("shader: %s\n", log); }
  return s;
}

static void
make_atlas () {
  FT_Set_Pixel_Sizes (face, 0, (FT_UInt) EM);
  vector<unsigned char> pix (ATLAS * ATLAS, 0);
  int x= 0, y= 0, row= 0;
  bitmaps.resize (paths.size ());
  for (unsigned gi= 0; gi < paths.size (); gi++) {
    if (paths[gi].advance == 0 && paths[gi].cmds.empty ()) continue;
    FT_Load_Glyph (face, gi, FT_LOAD_RENDER | FT_LOAD_NO_HINTING);
    FT_Bitmap& b= face->glyph->bitmap;
    if (x + (int) b.width + 1 > ATLAS) { x= 0; y += row + 1; row= 0; }
    for (unsigned r= 0; r < b.rows; r++)
      memcpy (&pix[(y + r) * ATLAS + x], b.buffer + r * b.pitch, b.width);
    bitmaps[gi]= { x, y, (int) b.width, (int) b.rows,
                   face->glyph->bitmap_left, face->glyph->bitmap_top };
    x += b.width + 1; row= max (row, (int) b.rows);
  }
  glGenTextures (1, &atlas_tex);
  glBindTexture (GL_TEXTURE_2D, atlas_tex);
  glPixelStorei (GL_UNPACK_ALIGNMENT, 1);
  glTexImage2D (GL_TEXTURE_2D, 0, GL_R8, ATLAS, ATLAS, 0, GL_RED, GL_UNSIGNED_BYTE, pix.data ());
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  const char* vs= "#version 300 es\n"
    "layout(location=0) in vec4 a; uniform vec2 size; out vec2 uv;\n"
    "void main () { uv= a.zw; gl_Position= vec4 (a.x/size.x*2.0-1.0, 1.0-a.y/size.y*2.0, 0.0, 1.0); }\n";
  const char* fs= "#version 300 es\n precision mediump float;\n"
    "uniform sampler2D tex; in vec2 uv; out vec4 color;\n"
    "void main () { float c= texture (tex, uv).r; color= vec4 (0.0, 0.0, 0.0, c); }\n";
  atlas_prog= glCreateProgram ();
  glAttachShader (atlas_prog, compile (GL_VERTEX_SHADER, vs));
  glAttachShader (atlas_prog, compile (GL_FRAGMENT_SHADER, fs));
  glLinkProgram (atlas_prog);
  glGenBuffers (1, &atlas_vbo);
  glGenVertexArrays (1, &atlas_vao);
}

static void
draw_atlas (float dy) {
  // the quads are made again at every frame, as a renderer told "draw this
  // glyph here" would make them
  static vector<float> v;
  v.clear ();
  for (const placed& p: glyphs) {
    const glyph_bitmap& b= bitmaps[p.gi];
    if (b.w == 0) continue;
    float x0= p.x + b.left, y0= p.y + dy - b.top, x1= x0 + b.w, y1= y0 + b.h;
    float u0= (float) b.x / ATLAS, v0= (float) b.y / ATLAS;
    float u1= (float) (b.x + b.w) / ATLAS, v1= (float) (b.y + b.h) / ATLAS;
    float q[24]= { x0,y0,u0,v0, x1,y0,u1,v0, x0,y1,u0,v1,
                   x1,y0,u1,v0, x1,y1,u1,v1, x0,y1,u0,v1 };
    v.insert (v.end (), q, q + 24);
  }
  glBindFramebuffer (GL_FRAMEBUFFER, 0);
  glViewport (0, 0, W, H);
  glDisable (GL_SCISSOR_TEST);
  glDisable (GL_STENCIL_TEST);
  glDisable (GL_DEPTH_TEST);
  glEnable (GL_BLEND);
  glBlendFunc (GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  glUseProgram (atlas_prog);
  glUniform2f (glGetUniformLocation (atlas_prog, "size"), W, H);
  glActiveTexture (GL_TEXTURE0);
  glBindTexture (GL_TEXTURE_2D, atlas_tex);
  glUniform1i (glGetUniformLocation (atlas_prog, "tex"), 0);
  glBindVertexArray (atlas_vao);
  glBindBuffer (GL_ARRAY_BUFFER, atlas_vbo);
  glBufferData (GL_ARRAY_BUFFER, v.size () * 4, v.data (), GL_STREAM_DRAW);
  glEnableVertexAttribArray (0);
  glVertexAttribPointer (0, 4, GL_FLOAT, GL_FALSE, 16, 0);
  glDrawArrays (GL_TRIANGLES, 0, (GLsizei) (v.size () / 4));
  glBindVertexArray (0);
}

/******************************************************************************
* Slug: glyphs drawn from their outlines (gl-slug)
*
* Each glyph is a list of quadratic Bezier curves in em units (y up), in a
* texture of curves (two RGBA32F texels a curve: p1 p2, p3). Its box is cut
* into NB horizontal and NB vertical bands, each with the list of the curves
* which cross it, sorted by decreasing maximal x (horizontal bands) or y
* (vertical bands), in a texture of indices (R32UI): a header of NB+NB
* (start, count) pairs, then the lists. The fragment shader casts a ray from
* the center of the pixel towards +x through the curves of its horizontal
* band, and one towards +y through those of its vertical band; each curve
* adds or removes the part of the pixel which its crossings leave on the
* ray's side, the roots which count chosen by the signs of the curve's
* control points (the 0x2E74 table of the paper), which keeps the winding
* number consistent where curves meet. The two estimates are averaged.
******************************************************************************/

struct slug_glyph {
  float bx0, by0, bx1, by1;  // the box, in em units (y up)
  unsigned offset;           // of its header in the index texture
  unsigned nb;               // bands in each direction
  bool made;
};

static vector<slug_glyph> slug_glyphs;
static vector<float> slug_curves;     // 4 floats a texel, 2 texels a curve
static vector<unsigned> slug_index;
static GLuint slug_curve_tex, slug_index_tex, slug_prog, slug_vbo, slug_vao;
static const int SLUG_W= 4096;        // width of the two textures
static int slug_total_curves= 0;

struct quad_curve { float x1, y1, x2, y2, x3, y3; };

struct slug_decompose {
  vector<quad_curve>* out;
  float s;
  float lx, ly;     // current point
  float sx, sy;     // start of the contour
};

static void
slug_add (slug_decompose* d, float x2, float y2, float x3, float y3) {
  d->out->push_back ({ d->lx, d->ly, x2, y2, x3, y3 });
  d->lx= x3; d->ly= y3;
}

static int
slug_move (const FT_Vector* to, void* u) {
  slug_decompose* d= (slug_decompose*) u;
  if (d->lx != d->sx || d->ly != d->sy)   // close the last contour
    slug_add (d, (d->lx + d->sx) / 2, (d->ly + d->sy) / 2, d->sx, d->sy);
  d->lx= d->sx= to->x * d->s; d->ly= d->sy= to->y * d->s;
  return 0;
}

static int
slug_line (const FT_Vector* to, void* u) {
  slug_decompose* d= (slug_decompose*) u;
  float x= to->x * d->s, y= to->y * d->s;
  slug_add (d, (d->lx + x) / 2, (d->ly + y) / 2, x, y);  // a straight quadratic
  return 0;
}

static int
slug_conic (const FT_Vector* c, const FT_Vector* to, void* u) {
  slug_decompose* d= (slug_decompose*) u;
  slug_add (d, c->x * d->s, c->y * d->s, to->x * d->s, to->y * d->s);
  return 0;
}

// a cubic as n quadratics, n from the size of its third derivative (the
// error of a quadratic for a piece of length 1/n goes as 1/n^3)
static int
slug_cubic (const FT_Vector* c1, const FT_Vector* c2, const FT_Vector* to, void* u) {
  slug_decompose* d= (slug_decompose*) u;
  float x0= d->lx, y0= d->ly;
  float x1= c1->x * d->s, y1= c1->y * d->s, x2= c2->x * d->s, y2= c2->y * d->s;
  float x3= to->x * d->s, y3= to->y * d->s;
  float ex= x3 - 3*x2 + 3*x1 - x0, ey= y3 - 3*y2 + 3*y1 - y0;
  float err= sqrtf (ex*ex + ey*ey) * sqrtf (3.f) / 36.f;
  const float tol= 1.f / 2048.f;  // of an em
  int n= max (1, min (8, (int) ceilf (cbrtf (err / tol))));
  auto P= [&] (float t, float& x, float& y) {
    float m= 1 - t;
    x= m*m*m*x0 + 3*m*m*t*x1 + 3*m*t*t*x2 + t*t*t*x3;
    y= m*m*m*y0 + 3*m*m*t*y1 + 3*m*t*t*y2 + t*t*t*y3; };
  auto D= [&] (float t, float& x, float& y) {   // derivative
    float m= 1 - t;
    x= 3*(m*m*(x1-x0) + 2*m*t*(x2-x1) + t*t*(x3-x2));
    y= 3*(m*m*(y1-y0) + 2*m*t*(y2-y1) + t*t*(y3-y2)); };
  for (int i= 0; i < n; i++) {
    float ta= (float) i / n, tb= (float) (i+1) / n;
    float ax, ay, bx, by, dax, day, dbx, dby;
    P (ta, ax, ay); P (tb, bx, by); D (ta, dax, day); D (tb, dbx, dby);
    float h= (tb - ta);
    // the control point: the mean of the two tangent estimates
    float cx= (ax + dax*h/2 + bx - dbx*h/2) / 2, cy= (ay + day*h/2 + by - dby*h/2) / 2;
    slug_add (d, cx, cy, bx, by);
  }
  return 0;
}

static void
slug_make_glyph (unsigned gi) {
  if (gi >= slug_glyphs.size ()) slug_glyphs.resize (gi + 1, { 0, 0, 0, 0, 0, 0, false });
  slug_glyph& g= slug_glyphs[gi];
  if (g.made) return;
  g.made= true;
  vector<quad_curve> cs;
  FT_Load_Glyph (face, gi, FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING);
  slug_decompose d { &cs, 1.f / face->units_per_EM, 0, 0, 0, 0 };
  FT_Outline_Funcs f= { slug_move, slug_line, slug_conic, slug_cubic, 0, 0 };
  FT_Outline_Decompose (&face->glyph->outline, &f, &d);
  if (d.lx != d.sx || d.ly != d.sy)
    slug_add (&d, (d.lx + d.sx) / 2, (d.ly + d.sy) / 2, d.sx, d.sy);
  if (cs.empty ()) { g.nb= 0; return; }
  float bx0= 1e9, by0= 1e9, bx1= -1e9, by1= -1e9;
  for (auto& c: cs) {
    bx0= min (bx0, min (c.x1, min (c.x2, c.x3))); bx1= max (bx1, max (c.x1, max (c.x2, c.x3)));
    by0= min (by0, min (c.y1, min (c.y2, c.y3))); by1= max (by1, max (c.y1, max (c.y2, c.y3)));
  }
  g.bx0= bx0; g.by0= by0; g.bx1= bx1; g.by1= by1;
  // the curves, in the texture of curves
  unsigned first= (unsigned) (slug_curves.size () / 8);
  for (auto& c: cs) {
    float v[8]= { c.x1, c.y1, c.x2, c.y2, c.x3, c.y3, 0, 0 };
    slug_curves.insert (slug_curves.end (), v, v + 8);
  }
  slug_total_curves += (int) cs.size ();
  // the bands
  unsigned nb= (unsigned) max (1, min (8, (int) cs.size () / 3));
  g.nb= nb;
  g.offset= (unsigned) slug_index.size ();
  slug_index.resize (slug_index.size () + 4 * nb, 0);
  for (int dir= 0; dir < 2; dir++)
    for (unsigned b= 0; b < nb; b++) {
      float lo, hi;
      if (dir == 0) { float bh= (by1 - by0) / nb; lo= by0 + b*bh; hi= lo + bh; }
      else          { float bw= (bx1 - bx0) / nb; lo= bx0 + b*bw; hi= lo + bw; }
      vector<pair<float, unsigned> > in;
      for (unsigned k= 0; k < cs.size (); k++) {
        const quad_curve& c= cs[k];
        float a0= dir == 0 ? min (c.y1, min (c.y2, c.y3)) : min (c.x1, min (c.x2, c.x3));
        float a1= dir == 0 ? max (c.y1, max (c.y2, c.y3)) : max (c.x1, max (c.x2, c.x3));
        if (a1 < lo || a0 > hi) continue;
        if (a0 == a1 && ((dir == 0 && c.y1 == c.y3) || (dir == 1 && c.x1 == c.x3)))
          continue;  // parallel to the ray: never crossed
        float key= dir == 0 ? max (c.x1, max (c.x2, c.x3)) : max (c.y1, max (c.y2, c.y3));
        in.push_back ({ key, first + k });
      }
      sort (in.begin (), in.end (), [] (const pair<float, unsigned>& p, const pair<float, unsigned>& q) {
        return p.first > q.first; });
      unsigned h= g.offset + 2 * (dir * nb + b);
      slug_index[h]= (unsigned) slug_index.size ();
      slug_index[h + 1]= (unsigned) in.size ();
      for (auto& p: in) slug_index.push_back (p.second);
    }
}

static const char* slug_vs= "#version 300 es\n"
  "layout(location=0) in vec2 i_org;\n"    // the origin of the glyph (pixels)
  "layout(location=1) in vec4 i_box;\n"    // its box (em)
  "layout(location=2) in uvec2 i_band;\n"  // offset of its header, bands
  "layout(location=3) in vec4 i_col;\n"
  "uniform vec2 size; uniform float scale;\n" // pixels per em
  "out vec2 v_em; flat out vec4 v_box; flat out uvec2 v_band; flat out vec4 v_col;\n"
  "void main () {\n"
  "  int id= gl_VertexID;\n"
  "  vec2 c= vec2 ((id == 1 || id == 3 || id == 4) ? 1.0 : 0.0,\n"
  "                (id == 2 || id == 4 || id == 5) ? 1.0 : 0.0);\n"
  "  float pad= 1.0 / scale;\n"             // a pixel around, for the edges
  "  vec2 em= mix (i_box.xy - pad, i_box.zw + pad, c);\n"
  "  vec2 p= i_org + vec2 (em.x, -em.y) * scale;\n"
  "  v_em= em; v_box= i_box; v_band= i_band; v_col= i_col;\n"
  "  gl_Position= vec4 (p.x / size.x * 2.0 - 1.0, 1.0 - p.y / size.y * 2.0, 0.0, 1.0);\n"
  "}\n";

static const char* slug_fs= "#version 300 es\n"
  "precision highp float; precision highp int; precision highp usampler2D;\n"
  "uniform sampler2D curves; uniform usampler2D bands; uniform float scale;\n"
  "in vec2 v_em; flat in vec4 v_box; flat in uvec2 v_band; flat in vec4 v_col;\n"
  "out vec4 o_col;\n"
  "const int W= 4096;\n"
  "uint idx (uint i) { return texelFetch (bands, ivec2 (int (i) % W, int (i) / W), 0).r; }\n"
  "vec4 crv (uint k, int j) { int i= int (k) * 2 + j; return texelFetch (curves, ivec2 (i % W, i / W), 0); }\n"
  // the coverage along a ray towards +x through the curves of a band, the
  // points translated to the pixel and swapped for the vertical ray
  "float ray (uint start, uint count, bool vertical, out float near) {\n"
  "  float cov= 0.0; near= 1.0e9;\n"
  "  for (uint n= 0u; n < count; n++) {\n"
  "    uint k= idx (start + n);\n"
  "    vec4 a= crv (k, 0); vec4 b= crv (k, 1);\n"
  "    vec2 p1= a.xy - v_em, p2= a.zw - v_em, p3= b.xy - v_em;\n"
  "    if (vertical) { p1= p1.yx; p2= p2.yx; p3= p3.yx; }\n"
  "    if (max (max (p1.x, p2.x), p3.x) * scale < -0.5) break;\n" // sorted
  "    uint code= (0x2E74u >> ((p1.y > 0.0 ? 2u : 0u) + (p2.y > 0.0 ? 4u : 0u) +\n"
  "                            (p3.y > 0.0 ? 8u : 0u))) & 3u;\n"
  "    if (code == 0u) continue;\n"
  "    vec2 A= p1 - p2 * 2.0 + p3, B= p1 - p2;\n"
  "    float t1, t2;\n"
  "    if (abs (A.y) < 1.0e-5) { t1= t2= p1.y * 0.5 / B.y; }\n"
  "    else {\n"
  "      float ra= 1.0 / A.y, d= sqrt (max (B.y * B.y - A.y * p1.y, 0.0));\n"
  "      t1= (B.y - d) * ra; t2= (B.y + d) * ra;\n"
  "    }\n"
  "    float x1= (A.x * t1 - B.x * 2.0) * t1 + p1.x;\n"
  "    float x2= (A.x * t2 - B.x * 2.0) * t2 + p1.x;\n"
  "    if ((code & 1u) != 0u) {\n"
  "      cov += clamp (x1 * scale + 0.5, 0.0, 1.0); near= min (near, abs (x1 * scale)); }\n"
  "    if (code > 1u) {\n"
  "      cov -= clamp (x2 * scale + 0.5, 0.0, 1.0); near= min (near, abs (x2 * scale)); }\n"
  "  }\n"
  "  return cov;\n"
  "}\n"
  "void main () {\n"
  "  uint nb= v_band.y, h= v_band.x;\n"
  "  vec2 rel= (v_em - v_box.xy) / (v_box.zw - v_box.xy);\n"
  "  uint by= uint (clamp (floor (rel.y * float (nb)), 0.0, float (nb) - 1.0));\n"
  "  uint bx= uint (clamp (floor (rel.x * float (nb)), 0.0, float (nb) - 1.0));\n"
  "  float nx, ny;\n"
  "  float cx= abs (ray (idx (h + 2u * by), idx (h + 2u * by + 1u), false, nx));\n"
  "  float cy= abs (ray (idx (h + 2u * (nb + bx)), idx (h + 2u * (nb + bx) + 1u), true, ny));\n"
  // each direction weighted by how close its nearest crossing is to the
  // pixel (the paper): the one along which an edge crosses the pixel
  // decides; far from any edge both agree
  "  float wx= clamp (1.0 - nx * 2.0, 0.0, 1.0), wy= clamp (1.0 - ny * 2.0, 0.0, 1.0);\n"
  "  float c= max ((cx * wx + cy * wy) / max (wx + wy, 1.0 / 65536.0), min (cx, cy));\n"
  "  c= clamp (c, 0.0, 1.0);\n"
  "  o_col= v_col * c;\n"
  "}\n";

static void
make_slug () {
  for (const placed& p: glyphs) slug_make_glyph (p.gi);
  // the two textures, padded to whole rows
  auto rows= [] (size_t n) { return (int) ((n + SLUG_W - 1) / SLUG_W); };
  int ct= rows (slug_curves.size () / 4), it= rows (slug_index.size ());
  slug_curves.resize ((size_t) ct * SLUG_W * 4, 0);
  slug_index.resize ((size_t) it * SLUG_W, 0);
  glGenTextures (1, &slug_curve_tex);
  glBindTexture (GL_TEXTURE_2D, slug_curve_tex);
  glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA32F, SLUG_W, ct, 0, GL_RGBA, GL_FLOAT, slug_curves.data ());
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glGenTextures (1, &slug_index_tex);
  glBindTexture (GL_TEXTURE_2D, slug_index_tex);
  glTexImage2D (GL_TEXTURE_2D, 0, GL_R32UI, SLUG_W, it, 0, GL_RED_INTEGER, GL_UNSIGNED_INT, slug_index.data ());
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  slug_prog= glCreateProgram ();
  glAttachShader (slug_prog, compile (GL_VERTEX_SHADER, slug_vs));
  glAttachShader (slug_prog, compile (GL_FRAGMENT_SHADER, slug_fs));
  glLinkProgram (slug_prog);
  GLint ok; glGetProgramiv (slug_prog, GL_LINK_STATUS, &ok);
  if (!ok) { char log[1000]; glGetProgramInfoLog (slug_prog, 1000, NULL, log); printf ("slug link: %s\n", log); }
  glGenBuffers (1, &slug_vbo);
  glGenVertexArrays (1, &slug_vao);
  int distinct= 0;
  for (auto& g: slug_glyphs) if (g.made && g.nb > 0) distinct++;
  printf ("slug: %d distinct glyphs, %d curves, %d KB of curves, %d KB of bands\n",
          distinct, slug_total_curves, (int) (slug_curves.size () * 4 / 1024),
          (int) (slug_index.size () * 4 / 1024));
}

static void
draw_slug () {
  // one instance a glyph, made again at every frame
  struct inst { float x, y, b0, b1, b2, b3; unsigned off, nb; float r, g, b, a; };
  static vector<inst> v;
  v.clear ();
  for (const placed& p: glyphs) {
    const slug_glyph& g= slug_glyphs[p.gi];
    if (g.nb == 0) continue;
    v.push_back ({ p.x, p.y, g.bx0, g.by0, g.bx1, g.by1, g.offset, g.nb, 0, 0, 0, 1 });
  }
  glBindFramebuffer (GL_FRAMEBUFFER, 0);
  glViewport (0, 0, W, H);
  glDisable (GL_SCISSOR_TEST); glDisable (GL_STENCIL_TEST); glDisable (GL_DEPTH_TEST);
  glEnable (GL_BLEND);
  glBlendFunc (GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
  glUseProgram (slug_prog);
  glUniform2f (glGetUniformLocation (slug_prog, "size"), W, H);
  glUniform1f (glGetUniformLocation (slug_prog, "scale"), EM);
  glActiveTexture (GL_TEXTURE0); glBindTexture (GL_TEXTURE_2D, slug_curve_tex);
  glUniform1i (glGetUniformLocation (slug_prog, "curves"), 0);
  glActiveTexture (GL_TEXTURE1); glBindTexture (GL_TEXTURE_2D, slug_index_tex);
  glUniform1i (glGetUniformLocation (slug_prog, "bands"), 1);
  glActiveTexture (GL_TEXTURE0);
  glBindVertexArray (slug_vao);
  glBindBuffer (GL_ARRAY_BUFFER, slug_vbo);
  glBufferData (GL_ARRAY_BUFFER, v.size () * sizeof (inst), v.data (), GL_STREAM_DRAW);
  int st= sizeof (inst);
  glEnableVertexAttribArray (0); glVertexAttribPointer (0, 2, GL_FLOAT, GL_FALSE, st, (void*) 0);
  glEnableVertexAttribArray (1); glVertexAttribPointer (1, 4, GL_FLOAT, GL_FALSE, st, (void*) 8);
  glEnableVertexAttribArray (2); glVertexAttribIPointer (2, 2, GL_UNSIGNED_INT, st, (void*) 24);
  glEnableVertexAttribArray (3); glVertexAttribPointer (3, 4, GL_FLOAT, GL_FALSE, st, (void*) 32);
  for (int i= 0; i < 4; i++) glVertexAttribDivisor (i, 1);
  glDrawArraysInstanced (GL_TRIANGLES, 0, 6, (GLsizei) v.size ());
  glBindVertexArray (0);
}

/******************************************************************************
* The frames
******************************************************************************/

static bool gl= false;
static vector<double> times;
static int frame= 0;

static void
wait_gpu () {
  if (!gl) return;
  unsigned char px[4];
  glReadPixels (0, 0, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, px);
}

static void
one_frame () {
  double t0= emscripten_get_now ();
  if (mode == "gl-kept") {
    kept_scene->translate (0, (float) (frame & 1));  // a scroll by a pixel
    canvas->update ();
  }
  else {
    canvas->remove ();
    add_page (mode != "gl-atlas" && mode != "gl-slug" && mode != "gl-none");
  }
  canvas->draw (true);
  canvas->sync ();
  if (mode == "gl-atlas") draw_atlas (0);
  if (mode == "gl-slug") draw_slug ();
  wait_gpu ();
  double t1= emscripten_get_now ();
  if (frame >= WARMUP) times.push_back (t1 - t0);
  if (sw_buffer && frame == 0) {
    // the CPU engines draw into memory: shown once, to look at
    EM_ASM ({
      var c= document.getElementById ('canvas2d'); c.width= $1; c.height= $2;
      var d= new ImageData (new Uint8ClampedArray (HEAPU8.buffer, $0, $1*$2*4).slice (), $1, $2);
      c.getContext ('2d').putImageData (d, 0, 0);
    }, sw_buffer, W, H);
  }
  if (++frame == WARMUP + FRAMES) {
    sort (times.begin (), times.end ());
    double sum= 0; for (double t: times) sum += t;
    // the shapes of text ThorVG draws: one per glyph, one per line, or none
    // (the atlas draws the glyphs)
    int text_shapes= (mode == "gl-atlas" || mode == "gl-slug" || mode == "gl-none") ? 0 :
                     (mode == "sw-runs" || mode == "gl-runs") ? (int) runs.size () :
                     (int) glyphs.size ();
    printf ("RESULT mode=%s em=%g glyphs=%d text-shapes=%d median=%.2f mean=%.2f min=%.2f max=%.2f ms\n",
            mode.c_str (), EM, (int) glyphs.size (), text_shapes,
            times[times.size () / 2], sum / times.size (), times.front (), times.back ());
    emscripten_cancel_main_loop ();
  }
}

int
main () {
  mode= (char*) EM_ASM_PTR ({
    var m= new URLSearchParams (location.search).get ('mode') || 'gl-glyphs';
    return stringToNewUTF8 (m);
  });
  FT_Library lib;
  FT_Init_FreeType (&lib);
  if (FT_New_Face (lib, "/font.otf", 0, &face)) { printf ("no font\n"); return 1; }
  EM= (float) EM_ASM_DOUBLE ({
    return Number (new URLSearchParams (location.search).get ('em') || 28);
  });
  lay_out ();
  tvg::Initializer::init (0);
  gl= mode.rfind ("gl-", 0) == 0;
  if (gl) {
    EmscriptenWebGLContextAttributes a;
    emscripten_webgl_init_context_attributes (&a);
    a.majorVersion= 2; a.minorVersion= 0;
    a.alpha= false; a.depth= true; a.stencil= true; a.antialias= false;
    a.preserveDrawingBuffer= true;
    EMSCRIPTEN_WEBGL_CONTEXT_HANDLE ctx= emscripten_webgl_create_context ("#canvas", &a);
    if (ctx <= 0) { printf ("no WebGL2 context\n"); return 1; }
    emscripten_webgl_make_context_current (ctx);
    emscripten_set_canvas_element_size ("#canvas", W, H);
    tvg::GlCanvas* c= tvg::GlCanvas::gen ();
    if (c == NULL) { printf ("no GlCanvas\n"); return 1; }
    tvg::Result r= c->target (NULL, NULL, (void*) (intptr_t) ctx, 0, W, H,
                              tvg::ColorSpace::ABGR8888S);
    if (r != tvg::Result::Success) { printf ("GlCanvas::target failed (%d)\n", (int) r); return 1; }
    canvas= c;
    printf ("renderer: %s / %s\n", (const char*) glGetString (GL_VENDOR),
            (const char*) glGetString (GL_RENDERER));
    if (mode == "gl-atlas") make_atlas ();
    if (mode == "gl-slug") make_slug ();
  }
  else {
    tvg::SwCanvas* c= tvg::SwCanvas::gen ();
    sw_buffer= new uint32_t[W * H];
    c->target (sw_buffer, W, W, H, tvg::ColorSpace::ABGR8888);
    canvas= c;
  }
  if (mode == "gl-kept") {
    kept_scene= tvg::Scene::gen ();
    kept_scene->add (rect_shape (0, 0, W, H, 160));
    kept_scene->add (rect_shape (page_x, 0, page_w, H, 255));
    for (const rule& r: rules) kept_scene->add (rect_shape (r.x, r.y, r.w, r.h, 0));
    for (auto& r: radicals) kept_scene->add (radical_shape (r));
    for (const placed& p: glyphs) kept_scene->add (glyph_shape (p));
    canvas->add (kept_scene);
  }
  printf ("mode %s: %d glyphs in %d lines, %d rules, %d radicals\n", mode.c_str (),
          (int) glyphs.size (), (int) runs.size (), (int) rules.size (), (int) radicals.size ());
  emscripten_set_main_loop (one_frame, 0, false);
  return 0;
}
