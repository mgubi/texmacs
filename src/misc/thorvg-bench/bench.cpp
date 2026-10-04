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
static const float EM= 28.f;             // 10pt at zoom 1 on a Retina screen
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
    add_page (mode != "gl-atlas");
  }
  canvas->draw (true);
  canvas->sync ();
  if (mode == "gl-atlas") draw_atlas (0);
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
    int text_shapes= (mode == "gl-atlas") ? 0 :
                     (mode == "sw-runs" || mode == "gl-runs") ? (int) runs.size () :
                     (int) glyphs.size ();
    printf ("RESULT mode=%s glyphs=%d text-shapes=%d median=%.2f mean=%.2f min=%.2f max=%.2f ms\n",
            mode.c_str (), (int) glyphs.size (), text_shapes,
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
