/******************************************************************************
* MODULE     : gpu_backend_wg.cpp
* DESCRIPTION: The WebGPU backend of the GPU renderer of the Vue port, with
*              ThorVG's WebGPU engine for the vector shapes: in the browser
*              (Emscripten, Dawn's webgpu.h for it: emdawnwebgpu)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*              Slug shaders adapted from E. Lengyel, (C) 2017, MIT (see below)
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// In the browser (Emscripten) the calls of webgpu.h are those of the WebGPU
// of the page (emdawnwebgpu), the window the canvas of SDL (#canvas). The
// device is asked for by the page before TeXmacs starts (web-pre.js: it
// comes asynchronously, and the program cannot wait without ASYNCIFY), and
// taken here with emscripten_webgpu_get_device. A frame is drawn in one
// iteration of the main loop, a task of the page, at whose end the browser
// shows the canvas: nothing is presented here. Nothing comes back from
// WebGPU at once (it is asynchronous in a browser): no pixels are read
// (read fails).
// The canvas is BGRA8 (the format browsers prefer: another one costs them a
// copy at each frame), as the targets, so that a copy between them is one
// of WebGPU; the textures of the pictures are RGBA8 (their data). The
// targets have y = 0 at the top, as the windows.
//
// The draws of a target follow each other in one render pass, until the
// target changes or a copy comes; the vertices and the uniforms of the
// draws of a submission are written at once when it is submitted (a write
// of the queue comes before all of the commands submitted after it), each
// draw at its own offset. ThorVG submits its own commands: what was drawn
// before is submitted first.

#include "gpu_backend.hpp"

#if !defined (__EMSCRIPTEN__) || !defined (USE_THORVG)

gpu_backend* gpu_webgpu_backend () { return NULL; }

#else

#include "tm_ostream.hpp"
#include "basic.hpp"
#include <SDL3/SDL.h>
#include <thorvg.h>
#include <webgpu/webgpu.h>
#include <emscripten.h>
// whether the page has asked for a device (web-pre.js): without it,
// emscripten_webgpu_get_device fails
EM_JS (int, tm_webgpu_has_device, (), {
  return Module['preinitializedWebGPUDevice'] ? 1 : 0;
});
#include <cmath>
#include <map>
#include <tuple>
#include <list>

#define SV(s) (WGPUStringView { s, WGPU_STRLEN })

struct gpu_texture_rep {
  WGPUTexture tex;
  WGPUTextureView view;
  int w, h;
  gpu_format f;
  bool linear, repeat;
};

/******************************************************************************
* The shaders (those of gpu_backend_gl.cpp, in WGSL)
******************************************************************************/

static const char* quad_src=
  "struct U { size: vec2f, mode: i32, pad: i32, porig: vec2f, ptile: vec2f };\n"
  "@group(0) @binding(0) var<uniform> u: U;\n"
  "@group(0) @binding(1) var tex: texture_2d<f32>;\n"
  "@group(0) @binding(2) var tex_s: sampler;\n"
  "@group(0) @binding(3) var atlas: texture_2d<f32>;\n"
  "@group(0) @binding(4) var atlas_s: sampler;\n"
  "struct VO { @builtin(position) p: vec4f, @location(0) uv: vec2f,\n"
  "            @location(1) col: vec4f, @location(2) pos: vec2f };\n"
  "@vertex fn vs (@location(0) pos: vec2f, @location(1) uv: vec2f,\n"
  "               @location(2) col: vec4f) -> VO {\n"
  "  var o: VO;\n"
  "  o.p= vec4f (pos.x / u.size.x * 2.0 - 1.0, 1.0 - pos.y / u.size.y * 2.0, 0.0, 1.0);\n"
  "  o.uv= uv; o.col= col; o.pos= pos;\n"
  "  return o;\n"
  "}\n"
  "@fragment fn fs (i: VO) -> @location(0) vec4f {\n"
  // (all sampled: in uniform control flow, as WGSL wants)
  "  let t= textureSample (tex, tex_s, i.uv);\n"
  "  let a= textureSample (atlas, atlas_s, i.uv).r;\n"
  "  let pt= textureSample (tex, tex_s, (i.pos - u.porig) / u.ptile);\n"
  "  if (u.mode == 1) { return t * i.col.a; }\n"
  "  if (u.mode == 3) { return a * pt * i.col.a; }\n"   // a glyph filled with a pattern
  "  return a * i.col;\n"
  "}\n";

// The Slug shaders below are adapted from the reference shaders of Eric
// Lengyel (https://github.com/EricLengyel/Slug, SlugPixelShader.hlsl), the
// algorithm of "GPU-Centered Font Rendering Directly from Glyph Outlines"
// (JCGT 6 (2), 2017); rewritten for TeXmacs, with its own layout of the
// curves and the bands. Copyright (c) 2017 Eric Lengyel, MIT License (see
// TeXmacs/misc/licenses/Slug-MIT.txt): "The above copyright notice and this
// permission notice shall be included in all copies or substantial portions
// of the Software."
static const char* slug_src=
  "struct U { size: vec2f, bold: f32, pad: f32 };\n"
  "@group(0) @binding(0) var<uniform> u: U;\n"
  "@group(0) @binding(1) var curves: texture_2d<f32>;\n"
  "@group(0) @binding(2) var bands: texture_2d<u32>;\n"
  "struct VO { @builtin(position) p: vec4f, @location(0) em: vec2f,\n"
  "  @location(1) @interpolate(flat) box: vec4f, @location(2) @interpolate(flat) band: vec2u,\n"
  "  @location(3) @interpolate(flat) col: vec4f, @location(4) @interpolate(flat) scale: f32 };\n"
  "@vertex fn vs (@builtin(vertex_index) id: u32, @location(0) org: vec3f,\n"
  "               @location(1) box: vec4f, @location(2) band: vec2u,\n"
  "               @location(3) col: vec4f) -> VO {\n"
  "  let c= vec2f (select (0.0, 1.0, id == 1u || id == 3u || id == 4u),\n"
  "                select (0.0, 1.0, id == 2u || id == 4u || id == 5u));\n"
  "  let pad= (1.0 + u.bold) / org.z;\n"
  "  let em= mix (box.xy - pad, box.zw + pad, c);\n"
  "  let p= org.xy + vec2f (em.x, -em.y) * org.z;\n"
  "  var o: VO;\n"
  "  o.p= vec4f (p.x / u.size.x * 2.0 - 1.0, 1.0 - p.y / u.size.y * 2.0, 0.0, 1.0);\n"
  "  o.em= em; o.box= box; o.band= band; o.col= col; o.scale= org.z;\n"
  "  return o;\n"
  "}\n"
  "const W: u32 = 1024u;\n"
  "fn idx (i: u32) -> u32 { return textureLoad (bands, vec2i (i32 (i % W), i32 (i / W)), 0).r; }\n"
  "fn crv (k: u32, j: u32) -> vec4f { let i= k * 2u + j;\n"
  "  return textureLoad (curves, vec2i (i32 (i % W), i32 (i / W)), 0); }\n"
  "struct R { cov: f32, near: f32 };\n"
  "fn ray (start: u32, count: u32, vertical: bool, hb: f32, em: vec2f, scale: f32) -> R {\n"
  "  var r: R; r.cov= 0.0; r.near= 1.0e9;\n"
  "  for (var n= 0u; n < count; n++) {\n"
  "    let k= idx (start + n);\n"
  "    let a= crv (k, 0u); let b= crv (k, 1u);\n"
  "    var p1= a.xy - em; var p2= a.zw - em; var p3= b.xy - em;\n"
  "    if (vertical) { p1= p1.yx; p2= p2.yx; p3= p3.yx; }\n"
  "    if (max (max (p1.x, p2.x), p3.x) * scale < -0.5 - abs (hb)) { break; }\n"
  "    let code= (0x2E74u >> (select (0u, 2u, p1.y > 0.0) + select (0u, 4u, p2.y > 0.0) +\n"
  "                           select (0u, 8u, p3.y > 0.0))) & 3u;\n"
  "    if (code == 0u) { continue; }\n"
  "    let A= p1 - p2 * 2.0 + p3; let B= p1 - p2;\n"
  "    var t1: f32; var t2: f32;\n"
  "    if (abs (A.y) < 1.0e-5) { t1= p1.y * 0.5 / B.y; t2= t1; }\n"
  "    else {\n"
  "      let ra= 1.0 / A.y; let d= sqrt (max (B.y * B.y - A.y * p1.y, 0.0));\n"
  "      t1= (B.y - d) * ra; t2= (B.y + d) * ra;\n"
  "    }\n"
  "    let x1= (A.x * t1 - B.x * 2.0) * t1 + p1.x;\n"
  "    let x2= (A.x * t2 - B.x * 2.0) * t2 + p1.x;\n"
  "    if ((code & 1u) != 0u) {\n"
  "      r.cov += clamp (x1 * scale + 0.5 + hb, 0.0, 1.0); r.near= min (r.near, abs (x1 * scale + hb)); }\n"
  "    if (code > 1u) {\n"
  "      r.cov -= clamp (x2 * scale + 0.5 - hb, 0.0, 1.0); r.near= min (r.near, abs (x2 * scale - hb)); }\n"
  "  }\n"
  "  return r;\n"
  "}\n"
  "@fragment fn fs (i: VO) -> @location(0) vec4f {\n"
  "  let nb= i.band.y & 0x7FFFFFFFu; let h= i.band.x;\n"
  "  let hb= select (-0.5, 0.5, (i.band.y >> 31u) != 0u) * u.bold;\n"
  "  let rel= (i.em - i.box.xy) / max (i.box.zw - i.box.xy, vec2f (1.0e-6));\n"
  "  let by= u32 (clamp (floor (rel.y * f32 (nb)), 0.0, f32 (nb) - 1.0));\n"
  "  let bx= u32 (clamp (floor (rel.x * f32 (nb)), 0.0, f32 (nb) - 1.0));\n"
  "  let rx= ray (idx (h + 2u * by), idx (h + 2u * by + 1u), false, hb, i.em, i.scale);\n"
  "  let ry= ray (idx (h + 2u * (nb + bx)), idx (h + 2u * (nb + bx) + 1u), true, -hb, i.em, i.scale);\n"
  "  let cx= abs (rx.cov); let cy= abs (ry.cov);\n"
  "  let wx= clamp (1.0 - rx.near * 2.0, 0.0, 1.0); let wy= clamp (1.0 - ry.near * 2.0, 0.0, 1.0);\n"
  "  let c= max ((cx * wx + cy * wy) / max (wx + wy, 1.0 / 65536.0), min (cx, cy));\n"
  "  return i.col * clamp (c, 0.0, 1.0);\n"
  "}\n";

/******************************************************************************
* The backend
******************************************************************************/

static const int SMALL= 512;
static const int UNIFORM_STRIDE= 256; // minUniformBufferOffsetAlignment
static const WGPUTextureFormat TARGET_FORMAT= WGPUTextureFormat_BGRA8Unorm;

class wg_backend: public gpu_backend {
  bool failed= false, started= false;
  WGPUInstance instance= NULL;
  WGPUAdapter adapter= NULL;
  WGPUDevice device= NULL;
  WGPUQueue queue= NULL;
  WGPUSurface surface= NULL;
  int surf_w= 0, surf_h= 0;
  gpu_texture_rep screen= { NULL, NULL, 0, 0, GPU_RGBA8, false, false };
  WGPURenderPipeline quad_pipe= NULL, slug_pipe= NULL;
  WGPUBindGroupLayout quad_bgl= NULL, slug_bgl= NULL;
  WGPUSampler samplers[4];         // linear | 2 * repeat
  gpu_texture_rep blank= { NULL, NULL, 0, 0, GPU_RGBA8, false, false };
  // the submission being encoded
  WGPUCommandEncoder enc= NULL;
  WGPURenderPassEncoder pass= NULL;
  WGPUTextureView pass_view= NULL;
  std::vector<unsigned char> verts, uniforms;
  WGPUBuffer vbuf= NULL, ubuf= NULL;
  uint64_t vbuf_size= 0, ubuf_size= 0;
  // the bind groups of the pairs of textures (quads, and whether the first
  // is sampled linearly whatever it says: the scaled copies) or of the
  // curves and bands (Slug), forgotten with the textures and with the
  // uniform buffer
  typedef std::tuple<gpu_texture, gpu_texture, bool> group_key;
  std::map<group_key, WGPUBindGroup> quad_groups, slug_groups;
  // ThorVG
  tvg::WgCanvas* tvg= NULL;
  gpu_texture_rep scratch= { NULL, NULL, 0, 0, GPU_RGBA8, false, false };
  tvg::WgCanvas* tvg_small= NULL;
  gpu_texture_rep small= { NULL, NULL, 0, 0, GPU_RGBA8, false, false };
  // what ThorVG drew in a box of at most SMALL, kept by the hash of the
  // shapes and of the box: the frames which draw the same (the frames of
  // the windows, at every turn of the loop) draw it without ThorVG
  struct shape_entry { unsigned long long key; gpu_texture tex; };
  std::list<shape_entry> shape_cache; // the last used first

  bool init ();
  bool ensure_screen (int dw, int dh);
  WGPURenderPipeline make_pipeline (const char* src, WGPUBindGroupLayout bgl,
                                    const WGPUVertexBufferLayout* vb);
  bool make_texture (gpu_texture_rep& t, int w, int h, gpu_format f, bool target,
                     bool bgra= false);
  void free_texture (gpu_texture_rep& t);
  void forget_groups (gpu_texture t);
  WGPUSampler sampler (gpu_texture t, bool linear= false);
  WGPUBindGroup quad_group (gpu_texture tex, gpu_texture atlas, bool linear= false);
  WGPUBindGroup slug_group (gpu_texture curves, gpu_texture bands);
  WGPUCommandEncoder encoder ();
  void end_pass ();
  WGPURenderPassEncoder begin_pass (const gpu_pass& p, bool clear= false,
                                    float r= 0, float g= 0, float b= 0, float a= 0);
  void set_scissor (WGPURenderPassEncoder e, const gpu_pass& p);
  uint32_t push_uniform (const void* data, size_t n);
  uint64_t push_verts (const void* data, size_t n);
  void submit ();
  bool ensure_scratch (int w, int h);
  bool ensure_small ();
  tvg::Shape* as_tvg (const gpu_shape& s);
  void compose (const gpu_pass& p, gpu_texture_rep& from, int ox, int oy,
                int x1, int y1, int x2, int y2);

public:
  const char* name () { return "WebGPU"; }
  unsigned long long window_flags () { return 0; }
  bool prepare () { return !failed; }
  bool attach (SDL_Window* w);
  string description () { return started ? string ("WebGPU") : string (""); }
  void begin_screen (int dw, int dh);
  void present (SDL_Window* w);
  void skip_frame ();
  void release_screen ();

  gpu_texture new_texture (int w, int h, gpu_format f, bool linear, bool repeat,
                           const void* data);
  void update_texture (gpu_texture t, int x, int y, int w, int h, const void* data);
  void delete_texture (gpu_texture t);
  gpu_texture new_target (int w, int h, bool transparent= false);
  int max_texture_side () { return 8192; }
  bool targets_flipped () { return false; }

  void draw_quads (const gpu_pass& p, const gpu_quads& q) { draw_quads (p, q, false); }
  void draw_quads (const gpu_pass& p, const gpu_quads& q, bool linear);
  void draw_slug (const gpu_pass& p, gpu_texture curves, gpu_texture bands,
                  const float* inst, int n, float bold);
  void draw_shapes (const gpu_pass& p, const std::vector<gpu_shape>& s,
                    int x1, int y1, int x2, int y2);
  void copy (gpu_texture src, int sw, int sh, int x1, int y1, int x2, int y2,
             gpu_texture dst, int dw, int dh, int dx1, int dy1, int dx2, int dy2);
  bool read (gpu_texture src, int sw, int sh, int x, int y, int w, int h,
             unsigned char* rgba) {
    // (no way back from the host)
    (void) src; (void) sw; (void) sh; (void) x; (void) y; (void) w; (void) h;
    (void) rgba; return false; }
  void flush () { submit (); }
  void finish () { submit (); }
};

/******************************************************************************
* Starting
******************************************************************************/

bool
wg_backend::attach (SDL_Window* w) {
  (void) w;
  if (failed) return false;
  if (started) return true;
  instance= wgpuCreateInstance (NULL);
  if (!tm_webgpu_has_device ()) {
    cout << "TeXmacs] GPU: the page has no WebGPU device, drawing with MuPDF" << LF;
    failed= true; return false;
  }
  device= emscripten_webgpu_get_device ();
  queue= wgpuDeviceGetQueue (device);
  WGPUEmscriptenSurfaceSourceCanvasHTMLSelector canvas= {};
  canvas.chain.sType= WGPUSType_EmscriptenSurfaceSourceCanvasHTMLSelector;
  canvas.selector= SV ("#canvas");
  WGPUSurfaceDescriptor csd= {};
  csd.nextInChain= &canvas.chain;
  surface= wgpuInstanceCreateSurface (instance, &csd);
  if (surface == NULL || !init ()) { failed= true; return false; }
  started= true;
  return true;
}

WGPURenderPipeline
wg_backend::make_pipeline (const char* src, WGPUBindGroupLayout bgl,
                           const WGPUVertexBufferLayout* vb) {
  WGPUShaderSourceWGSL wgsl= {};
  wgsl.chain.sType= WGPUSType_ShaderSourceWGSL;
  wgsl.code= SV (src);
  WGPUShaderModuleDescriptor md= {};
  md.nextInChain= &wgsl.chain;
  WGPUShaderModule m= wgpuDeviceCreateShaderModule (device, &md);
  WGPUPipelineLayoutDescriptor pld= {};
  pld.bindGroupLayoutCount= 1; pld.bindGroupLayouts= &bgl;
  WGPUPipelineLayout layout= wgpuDeviceCreatePipelineLayout (device, &pld);
  // premultiplied colors over what is there
  WGPUBlendState blend= {};
  blend.color= { WGPUBlendOperation_Add, WGPUBlendFactor_One, WGPUBlendFactor_OneMinusSrcAlpha };
  blend.alpha= blend.color;
  WGPUColorTargetState target= {};
  target.format= TARGET_FORMAT;
  target.blend= &blend;
  target.writeMask= WGPUColorWriteMask_All;
  WGPUFragmentState fs= {};
  fs.module= m; fs.entryPoint= SV ("fs");
  fs.targetCount= 1; fs.targets= &target;
  WGPURenderPipelineDescriptor rpd= {};
  rpd.layout= layout;
  rpd.vertex.module= m; rpd.vertex.entryPoint= SV ("vs");
  rpd.vertex.bufferCount= 1; rpd.vertex.buffers= vb;
  rpd.primitive.topology= WGPUPrimitiveTopology_TriangleList;
  rpd.multisample.count= 1; rpd.multisample.mask= 0xffffffff;
  rpd.fragment= &fs;
  WGPURenderPipeline p= wgpuDeviceCreateRenderPipeline (device, &rpd);
  wgpuPipelineLayoutRelease (layout);
  wgpuShaderModuleRelease (m);
  return p;
}

bool
wg_backend::init () {
  // the samplers
  for (int i= 0; i < 4; i++) {
    WGPUSamplerDescriptor s= {};
    WGPUAddressMode am= (i & 2) ? WGPUAddressMode_Repeat : WGPUAddressMode_ClampToEdge;
    WGPUFilterMode fm= (i & 1) ? WGPUFilterMode_Linear : WGPUFilterMode_Nearest;
    s.addressModeU= s.addressModeV= s.addressModeW= am;
    s.magFilter= s.minFilter= fm;
    s.mipmapFilter= WGPUMipmapFilterMode_Nearest;
    s.lodMinClamp= 0; s.lodMaxClamp= 32;
    s.maxAnisotropy= 1;
    samplers[i]= wgpuDeviceCreateSampler (device, &s);
  }
  // the quads: a uniform at a dynamic offset, a texture and the atlas
  {
    WGPUBindGroupLayoutEntry e[5]= {};
    e[0].binding= 0; e[0].visibility= WGPUShaderStage_Vertex | WGPUShaderStage_Fragment;
    e[0].buffer.type= WGPUBufferBindingType_Uniform; e[0].buffer.hasDynamicOffset= 1;
    e[0].buffer.minBindingSize= 32;
    for (int i= 1; i < 5; i += 2) {
      e[i].binding= i; e[i].visibility= WGPUShaderStage_Fragment;
      e[i].texture.sampleType= WGPUTextureSampleType_Float;
      e[i].texture.viewDimension= WGPUTextureViewDimension_2D;
      e[i+1].binding= i + 1; e[i+1].visibility= WGPUShaderStage_Fragment;
      e[i+1].sampler.type= WGPUSamplerBindingType_Filtering;
    }
    WGPUBindGroupLayoutDescriptor d= {};
    d.entryCount= 5; d.entries= e;
    quad_bgl= wgpuDeviceCreateBindGroupLayout (device, &d);
    WGPUVertexAttribute a[3]= {};
    a[0].format= WGPUVertexFormat_Float32x2; a[0].offset= 0; a[0].shaderLocation= 0;
    a[1].format= WGPUVertexFormat_Float32x2; a[1].offset= 8; a[1].shaderLocation= 1;
    a[2].format= WGPUVertexFormat_Float32x4; a[2].offset= 16; a[2].shaderLocation= 2;
    WGPUVertexBufferLayout vb= {};
    vb.stepMode= WGPUVertexStepMode_Vertex; vb.arrayStride= 32;
    vb.attributeCount= 3; vb.attributes= a;
    quad_pipe= make_pipeline (quad_src, quad_bgl, &vb);
  }
  // Slug: instances of 13 floats, the curves and the bands (loaded, not
  // sampled)
  {
    WGPUBindGroupLayoutEntry e[3]= {};
    e[0].binding= 0; e[0].visibility= WGPUShaderStage_Vertex | WGPUShaderStage_Fragment;
    e[0].buffer.type= WGPUBufferBindingType_Uniform; e[0].buffer.hasDynamicOffset= 1;
    e[0].buffer.minBindingSize= 16;
    e[1].binding= 1; e[1].visibility= WGPUShaderStage_Fragment;
    e[1].texture.sampleType= WGPUTextureSampleType_UnfilterableFloat;
    e[1].texture.viewDimension= WGPUTextureViewDimension_2D;
    e[2].binding= 2; e[2].visibility= WGPUShaderStage_Fragment;
    e[2].texture.sampleType= WGPUTextureSampleType_Uint;
    e[2].texture.viewDimension= WGPUTextureViewDimension_2D;
    WGPUBindGroupLayoutDescriptor d= {};
    d.entryCount= 3; d.entries= e;
    slug_bgl= wgpuDeviceCreateBindGroupLayout (device, &d);
    WGPUVertexAttribute a[4]= {};
    a[0].format= WGPUVertexFormat_Float32x3; a[0].offset= 0; a[0].shaderLocation= 0;
    a[1].format= WGPUVertexFormat_Float32x4; a[1].offset= 12; a[1].shaderLocation= 1;
    a[2].format= WGPUVertexFormat_Uint32x2; a[2].offset= 28; a[2].shaderLocation= 2;
    a[3].format= WGPUVertexFormat_Float32x4; a[3].offset= 36; a[3].shaderLocation= 3;
    WGPUVertexBufferLayout vb= {};
    vb.stepMode= WGPUVertexStepMode_Instance; vb.arrayStride= 52;
    vb.attributeCount= 4; vb.attributes= a;
    slug_pipe= make_pipeline (slug_src, slug_bgl, &vb);
  }
  // a texture of 1x1, white, for the bindings which a draw does not use
  if (!make_texture (blank, 1, 1, GPU_RGBA8, false)) return false;
  unsigned char white[4]= { 255, 255, 255, 255 };
  update_texture (&blank, 0, 0, 1, 1, white);
  // ThorVG
  if (tvg::Initializer::init (0) != tvg::Result::Success) {
    cout << "TeXmacs] GPU: ThorVG does not start" << LF; return false; }
  tvg= tvg::WgCanvas::gen ();
  tvg_small= tvg::WgCanvas::gen ();
  if (tvg == NULL || tvg_small == NULL) {
    cout << "TeXmacs] GPU: no WebGPU canvas in ThorVG" << LF; return false; }
  return true;
}

/******************************************************************************
* The screen: the canvas of the host
******************************************************************************/

// the texture of the canvas for this frame, at the first draw into it (the
// renderer draws into the window as into a target NULL, with its size)
bool
wg_backend::ensure_screen (int dw, int dh) {
  if (!started || dw <= 0 || dh <= 0) return false;
  if (screen.tex != NULL && screen.w == dw && screen.h == dh) return true;
  begin_screen (dw, dh);
  return screen.tex != NULL;
}

void
wg_backend::begin_screen (int dw, int dh) {
  if (!started) return;
  submit ();
  if (dw != surf_w || dh != surf_h) {
    WGPUSurfaceConfiguration c= {};
    c.device= device;
    c.format= TARGET_FORMAT;
    c.usage= WGPUTextureUsage_RenderAttachment | WGPUTextureUsage_CopyDst |
             WGPUTextureUsage_CopySrc | WGPUTextureUsage_TextureBinding;
    c.width= dw; c.height= dh;
    c.alphaMode= WGPUCompositeAlphaMode_Opaque;
    c.presentMode= WGPUPresentMode_Fifo;
    wgpuSurfaceConfigure (surface, &c);
    surf_w= dw; surf_h= dh;
  }
  if (screen.view) { wgpuTextureViewRelease (screen.view); screen.view= NULL; }
  if (screen.tex) { wgpuTextureRelease (screen.tex); screen.tex= NULL; }
  WGPUSurfaceTexture st= {};
  wgpuSurfaceGetCurrentTexture (surface, &st);
  screen.tex= st.texture;
  screen.view= wgpuTextureCreateView (screen.tex, NULL);
  screen.w= dw; screen.h= dh;
}

void
wg_backend::release_screen () {
  // (the next frame draws into the next texture of the canvas)
  if (screen.view) { wgpuTextureViewRelease (screen.view); screen.view= NULL; }
  if (screen.tex) { wgpuTextureRelease (screen.tex); screen.tex= NULL; }
}

void
wg_backend::present (SDL_Window* w) {
  (void) w;
  if (!started) return;
  if (screen.tex != NULL) {
    // (the canvas is shown at the end of the iteration of the main loop)
    submit ();
    release_screen ();
  }
}

// a frame which is not presented (the same as the last one): what it drew
// into the texture of the canvas, which is gone at the end of the iteration
// of the main loop, is submitted, and the next frame takes the next one
void
wg_backend::skip_frame () {
  if (!started) return;
  submit ();
  release_screen ();
}

/******************************************************************************
* Textures and targets
******************************************************************************/

static WGPUTextureFormat
format_of (gpu_format f, int& bpp) {
  switch (f) {
  case GPU_R8: bpp= 1; return WGPUTextureFormat_R8Unorm;
  case GPU_RGBA32F: bpp= 16; return WGPUTextureFormat_RGBA32Float;
  case GPU_R32UI: bpp= 4; return WGPUTextureFormat_R32Uint;
  default: bpp= 4; return WGPUTextureFormat_RGBA8Unorm;
  }
}

// (bgra: the targets, as the canvas, and those of ThorVG, whose last pass,
// a blit, is made for the format of its context, BGRA8 when it draws into a
// texture)
bool
wg_backend::make_texture (gpu_texture_rep& t, int w, int h, gpu_format f, bool target,
                          bool bgra) {
  int bpp;
  WGPUTextureDescriptor d= {};
  d.size= { (uint32_t) w, (uint32_t) h, 1 };
  d.format= bgra ? WGPUTextureFormat_BGRA8Unorm : format_of (f, bpp);
  d.usage= WGPUTextureUsage_TextureBinding | WGPUTextureUsage_CopyDst |
           WGPUTextureUsage_CopySrc;
  if (target) d.usage |= WGPUTextureUsage_RenderAttachment;
  d.dimension= WGPUTextureDimension_2D;
  d.mipLevelCount= 1; d.sampleCount= 1;
  t.tex= wgpuDeviceCreateTexture (device, &d);
  t.view= wgpuTextureCreateView (t.tex, NULL);
  t.w= w; t.h= h; t.f= f;
  return t.tex != NULL;
}

void
wg_backend::free_texture (gpu_texture_rep& t) {
  forget_groups (&t);
  if (t.view) wgpuTextureViewRelease (t.view);
  if (t.tex) { wgpuTextureDestroy (t.tex); wgpuTextureRelease (t.tex); }
  t.view= NULL; t.tex= NULL;
}

void
wg_backend::forget_groups (gpu_texture t) {
  for (auto* m: { &quad_groups, &slug_groups })
    for (auto it= m->begin (); it != m->end (); )
      if (std::get<0> (it->first) == t || std::get<1> (it->first) == t) {
        wgpuBindGroupRelease (it->second);
        it= m->erase (it);
      }
      else ++it;
}

gpu_texture
wg_backend::new_texture (int w, int h, gpu_format f, bool linear, bool repeat,
                         const void* data) {
  if (!started || w <= 0 || h <= 0 || w > 8192 || h > 8192) return NULL;
  gpu_texture t= tm_new<gpu_texture_rep> ();
  t->linear= linear; t->repeat= repeat;
  make_texture (*t, w, h, f, false);
  if (data != NULL) update_texture (t, 0, 0, w, h, data);
  return t;
}

void
wg_backend::update_texture (gpu_texture t, int x, int y, int w, int h,
                            const void* data) {
  if (t == NULL || w <= 0 || h <= 0 || data == NULL) return;
  // (a write of the queue comes before the commands not yet submitted: they
  // go first, else a texture drawn before its update showed the update)
  submit ();
  int bpp;
  format_of (t->f, bpp);
  WGPUTexelCopyTextureInfo dst= {};
  dst.texture= t->tex;
  dst.origin= { (uint32_t) x, (uint32_t) y, 0 };
  dst.aspect= WGPUTextureAspect_All;
  WGPUTexelCopyBufferLayout layout= {};
  layout.bytesPerRow= (uint32_t) (w * bpp);
  layout.rowsPerImage= (uint32_t) h;
  WGPUExtent3D size= { (uint32_t) w, (uint32_t) h, 1 };
  wgpuQueueWriteTexture (queue, &dst, data, (size_t) w * h * bpp, &layout, &size);
}

void
wg_backend::delete_texture (gpu_texture t) {
  if (t == NULL) return;
  submit (); // (the commands which use it)
  free_texture (*t);
  tm_delete (t);
}

gpu_texture
wg_backend::new_target (int w, int h, bool transparent) {
  if (!started || w <= 0 || h <= 0 || w > 8192 || h > 8192) return NULL;
  gpu_texture t= tm_new<gpu_texture_rep> ();
  t->linear= false; t->repeat= false;
  make_texture (*t, w, h, GPU_RGBA8, true, true);
  // opaque white, as native_opaque_picture, or transparent
  gpu_pass p= { t, w, h, w, h, 0, 0, w, h };
  if (transparent) begin_pass (p, true, 0, 0, 0, 0);
  else begin_pass (p, true, 1, 1, 1, 1);
  return t;
}

WGPUSampler
wg_backend::sampler (gpu_texture t, bool linear) {
  if (t == NULL) return samplers[0];
  return samplers[(t->linear || linear ? 1 : 0) | (t->repeat ? 2 : 0)];
}

/******************************************************************************
* Encoding
******************************************************************************/

WGPUCommandEncoder
wg_backend::encoder () {
  if (enc == NULL) enc= wgpuDeviceCreateCommandEncoder (device, NULL);
  return enc;
}

void
wg_backend::end_pass () {
  if (pass == NULL) return;
  wgpuRenderPassEncoderEnd (pass);
  wgpuRenderPassEncoderRelease (pass);
  pass= NULL; pass_view= NULL;
}

WGPURenderPassEncoder
wg_backend::begin_pass (const gpu_pass& p, bool clear, float r, float g, float b, float a) {
  if (p.target == NULL && !ensure_screen (p.dw, p.dh)) return NULL;
  WGPUTextureView view= (p.target == NULL) ? screen.view : p.target->view;
  if (view == NULL) return NULL;
  if (pass != NULL && pass_view == view && !clear) return pass;
  end_pass ();
  WGPURenderPassColorAttachment ca= {};
  ca.view= view;
  ca.depthSlice= WGPU_DEPTH_SLICE_UNDEFINED;
  ca.loadOp= clear ? WGPULoadOp_Clear : WGPULoadOp_Load;
  ca.storeOp= WGPUStoreOp_Store;
  ca.clearValue= { r, g, b, a };
  WGPURenderPassDescriptor rp= {};
  rp.colorAttachmentCount= 1; rp.colorAttachments= &ca;
  pass= wgpuCommandEncoderBeginRenderPass (encoder (), &rp);
  pass_view= view;
  return pass;
}

void
wg_backend::set_scissor (WGPURenderPassEncoder e, const gpu_pass& p) {
  int x1= p.sx1, y1= p.sy1, x2= p.sx2, y2= p.sy2;
  if (p.dw != p.w || p.dh != p.h) {
    // a scaled window: the pixels which the box touches
    double fx= (double) p.dw / p.w, fy= (double) p.dh / p.h;
    x1= (int) floor (x1 * fx); x2= (int) ceil (x2 * fx);
    y1= (int) floor (y1 * fy); y2= (int) ceil (y2 * fy);
  }
  x1= max (0, min (x1, p.dw)); x2= max (x1, min (x2, p.dw));
  y1= max (0, min (y1, p.dh)); y2= max (y1, min (y2, p.dh));
  wgpuRenderPassEncoderSetScissorRect (e, x1, y1, x2 - x1, y2 - y1);
}

uint32_t
wg_backend::push_uniform (const void* data, size_t n) {
  uint32_t at= (uint32_t) uniforms.size ();
  uniforms.resize (at + UNIFORM_STRIDE, 0);
  memcpy (&uniforms[at], data, n);
  return at;
}

uint64_t
wg_backend::push_verts (const void* data, size_t n) {
  uint64_t at= verts.size ();
  verts.insert (verts.end (), (const unsigned char*) data,
                (const unsigned char*) data + n);
  while (verts.size () % 4) verts.push_back (0);
  return at;
}

// the buffers of the submission: written, then the commands submitted
void
wg_backend::submit () {
  if (!started) return;
  end_pass ();
  if (enc == NULL) { verts.clear (); uniforms.clear (); return; }
  if (!verts.empty ()) {
    // (grown before the commands are finished: they use the buffer)
    wgpuQueueWriteBuffer (queue, vbuf, 0, verts.data (), verts.size ());
  }
  if (!uniforms.empty ())
    wgpuQueueWriteBuffer (queue, ubuf, 0, uniforms.data (), uniforms.size ());
  WGPUCommandBuffer cb= wgpuCommandEncoderFinish (enc, NULL);
  wgpuQueueSubmit (queue, 1, &cb);
  wgpuCommandBufferRelease (cb);
  wgpuCommandEncoderRelease (enc);
  enc= NULL;
  verts.clear (); uniforms.clear ();
}

// the buffers must be large enough for the submission before a draw uses
// them: a larger one is made, which the commands encoded before would not
// see, so they are submitted first
static uint64_t
grown (uint64_t need) {
  uint64_t s= 1 << 16;
  while (s < need) s <<= 1;
  return s;
}

/******************************************************************************
* Drawing
******************************************************************************/

WGPUBindGroup
wg_backend::quad_group (gpu_texture tex, gpu_texture atlas, bool linear) {
  group_key key (tex, atlas, linear);
  auto it= quad_groups.find (key);
  if (it != quad_groups.end ()) return it->second;
  gpu_texture t= tex ? tex : &blank, a= atlas ? atlas : &blank;
  WGPUBindGroupEntry e[5]= {};
  e[0].binding= 0; e[0].buffer= ubuf; e[0].offset= 0; e[0].size= 32;
  e[1].binding= 1; e[1].textureView= t->view;
  e[2].binding= 2; e[2].sampler= sampler (t, linear);
  e[3].binding= 3; e[3].textureView= a->view;
  e[4].binding= 4; e[4].sampler= sampler (a);
  WGPUBindGroupDescriptor d= {};
  d.layout= quad_bgl; d.entryCount= 5; d.entries= e;
  WGPUBindGroup g= wgpuDeviceCreateBindGroup (device, &d);
  quad_groups[key]= g;
  return g;
}

WGPUBindGroup
wg_backend::slug_group (gpu_texture curves, gpu_texture bands) {
  group_key key (curves, bands, false);
  auto it= slug_groups.find (key);
  if (it != slug_groups.end ()) return it->second;
  WGPUBindGroupEntry e[3]= {};
  e[0].binding= 0; e[0].buffer= ubuf; e[0].offset= 0; e[0].size= 16;
  e[1].binding= 1; e[1].textureView= curves->view;
  e[2].binding= 2; e[2].textureView= bands->view;
  WGPUBindGroupDescriptor d= {};
  d.layout= slug_bgl; d.entryCount= 3; d.entries= e;
  WGPUBindGroup g= wgpuDeviceCreateBindGroup (device, &d);
  slug_groups[key]= g;
  return g;
}

// room for n more bytes of vertices and one more uniform in the buffers
#define ENSURE_ROOM(nverts) \
  do { \
    uint64_t nv= verts.size () + (nverts) + 4, nu= uniforms.size () + UNIFORM_STRIDE; \
    if (nv > vbuf_size || nu > ubuf_size) { \
      submit (); \
      if (nv > vbuf_size) { \
        if (vbuf) { wgpuBufferDestroy (vbuf); wgpuBufferRelease (vbuf); } \
        vbuf_size= grown (nv > (1u << 20) ? nv : (1u << 20));  \
        WGPUBufferDescriptor bd= {}; \
        bd.size= vbuf_size; \
        bd.usage= WGPUBufferUsage_Vertex | WGPUBufferUsage_CopyDst; \
        vbuf= wgpuDeviceCreateBuffer (device, &bd); \
      } \
      if (nu > ubuf_size) { \
        if (ubuf) { wgpuBufferDestroy (ubuf); wgpuBufferRelease (ubuf); } \
        ubuf_size= grown (nu > (1u << 16) ? nu : (1u << 16));  \
        WGPUBufferDescriptor bd= {}; \
        bd.size= ubuf_size; \
        bd.usage= WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst; \
        ubuf= wgpuDeviceCreateBuffer (device, &bd); \
        for (auto& g: quad_groups) wgpuBindGroupRelease (g.second); \
        for (auto& g: slug_groups) wgpuBindGroupRelease (g.second); \
        quad_groups.clear (); slug_groups.clear (); \
      } \
    } \
  } while (0)

void
wg_backend::draw_quads (const gpu_pass& p, const gpu_quads& q, bool linear) {
  if (!started || q.nr_verts <= 0) return;
  size_t n= (size_t) q.nr_verts * 8 * sizeof (float);
  ENSURE_ROOM (n);
  WGPURenderPassEncoder e= begin_pass (p);
  if (e == NULL) return;
  struct { float w, h; int32_t mode, pad; float ox, oy, tw, th; } u=
    { (float) p.w, (float) p.h, q.mode, 0, q.porig_x, q.porig_y, q.ptile_w, q.ptile_h };
  uint32_t uo= push_uniform (&u, sizeof (u));
  uint64_t vo= push_verts (q.verts, n);
  set_scissor (e, p);
  wgpuRenderPassEncoderSetPipeline (e, quad_pipe);
  wgpuRenderPassEncoderSetBindGroup (e, 0, quad_group (q.mode != 2 ? q.tex : NULL,
                                                      q.atlas, linear), 1, &uo);
  wgpuRenderPassEncoderSetVertexBuffer (e, 0, vbuf, vo, n);
  wgpuRenderPassEncoderDraw (e, (uint32_t) q.nr_verts, 1, 0, 0);
}

void
wg_backend::draw_slug (const gpu_pass& p, gpu_texture curves, gpu_texture bands,
                       const float* inst, int n, float bold) {
  if (!started || n <= 0 || curves == NULL || bands == NULL) return;
  size_t bytes= (size_t) n * 13 * 4;
  ENSURE_ROOM (bytes);
  WGPURenderPassEncoder e= begin_pass (p);
  if (e == NULL) return;
  struct { float w, h, bold, pad; } u= { (float) p.w, (float) p.h, bold, 0 };
  uint32_t uo= push_uniform (&u, sizeof (u));
  uint64_t vo= push_verts (inst, bytes);
  set_scissor (e, p);
  wgpuRenderPassEncoderSetPipeline (e, slug_pipe);
  wgpuRenderPassEncoderSetBindGroup (e, 0, slug_group (curves, bands), 1, &uo);
  wgpuRenderPassEncoderSetVertexBuffer (e, 0, vbuf, vo, bytes);
  wgpuRenderPassEncoderDraw (e, 6, (uint32_t) n, 0, 0);
}

/******************************************************************************
* Vector shapes (ThorVG)
******************************************************************************/

bool
wg_backend::ensure_scratch (int w, int h) {
  if (scratch.tex != NULL && scratch.w >= w && scratch.h >= h) return true;
  int nw= max (w, scratch.w), nh= max (h, scratch.h);
  submit ();
  if (scratch.tex != NULL) free_texture (scratch);
  if (!make_texture (scratch, nw, nh, GPU_RGBA8, true, true)) return false;
  tvg::Result r= tvg->target (device, instance, scratch.tex, nw, nh,
                              tvg::ColorSpace::ABGR8888S, 1);
  if (r != tvg::Result::Success) {
    cout << "TeXmacs] GPU: ThorVG cannot draw into its target (" << (int) r << ")" << LF;
    return false;
  }
  return true;
}

bool
wg_backend::ensure_small () {
  if (small.tex != NULL) return true;
  if (!make_texture (small, SMALL, SMALL, GPU_RGBA8, true, true)) return false;
  return tvg_small->target (device, instance, small.tex, SMALL, SMALL,
                            tvg::ColorSpace::ABGR8888S, 1) == tvg::Result::Success;
}

tvg::Shape*
wg_backend::as_tvg (const gpu_shape& s) {
  tvg::Shape* p= tvg::Shape::gen ();
  size_t k= 0;
  for (unsigned char c: s.cmds)
    switch (c) {
    case 0: p->moveTo (s.pts[k], s.pts[k+1]); k += 2; break;
    case 1: p->lineTo (s.pts[k], s.pts[k+1]); k += 2; break;
    case 2: p->cubicTo (s.pts[k], s.pts[k+1], s.pts[k+2], s.pts[k+3],
                        s.pts[k+4], s.pts[k+5]); k += 6; break;
    default: p->close (); break;
    }
  if (s.fill) {
    p->fill (s.fill_rgba[0], s.fill_rgba[1], s.fill_rgba[2], s.fill_rgba[3]);
    p->fillRule (s.even_odd ? tvg::FillRule::EvenOdd : tvg::FillRule::NonZero);
  }
  if (s.stroke) {
    p->strokeWidth (s.width);
    p->strokeFill (s.stroke_rgba[0], s.stroke_rgba[1], s.stroke_rgba[2], s.stroke_rgba[3]);
    p->strokeCap (s.cap == 1 ? tvg::StrokeCap::Round :
                  s.cap == 2 ? tvg::StrokeCap::Square : tvg::StrokeCap::Butt);
    p->strokeJoin (tvg::StrokeJoin::Round);
  }
  return p;
}

static unsigned long long
hash_shapes (const std::vector<gpu_shape>& shapes, int x1, int y1, int x2, int y2) {
  unsigned long long h= 14695981039346656037ULL;
  auto mix= [&h] (const void* d, size_t n) {
    const unsigned char* p= (const unsigned char*) d;
    for (size_t i= 0; i < n; i++) { h ^= p[i]; h *= 1099511628211ULL; } };
  int box[4]= { x1, y1, x2, y2 };
  mix (box, sizeof (box));
  for (const gpu_shape& s: shapes) {
    mix (s.cmds.data (), s.cmds.size ());
    mix (s.pts.data (), s.pts.size () * sizeof (float));
    unsigned char f[4]= { s.fill, s.even_odd, s.stroke, (unsigned char) s.cap };
    mix (f, 4);
    mix (s.fill_rgba, 4); mix (s.stroke_rgba, 4); mix (&s.width, sizeof (float));
  }
  return h;
}

// what ThorVG drew (premultiplied, y down), the box x1..x2, y1..y2 of the
// target at ox, oy in from, composed over the target
void
wg_backend::compose (const gpu_pass& p, gpu_texture_rep& from, int ox, int oy,
                     int x1, int y1, int x2, int y2) {
  float sw= (float) from.w, sh= (float) from.h;
  float u0= (x1 - ox) / sw, u1= (x2 - ox) / sw;
  float v0= (y1 - oy) / sh, v1= (y2 - oy) / sh;
  float X1= (float) x1, X2= (float) x2, Y1= (float) y1, Y2= (float) y2;
  float v[6][8]= { { X1, Y1, u0, v0, 1, 1, 1, 1 }, { X2, Y1, u1, v0, 1, 1, 1, 1 },
                   { X1, Y2, u0, v1, 1, 1, 1, 1 }, { X2, Y1, u1, v0, 1, 1, 1, 1 },
                   { X2, Y2, u1, v1, 1, 1, 1, 1 }, { X1, Y2, u0, v1, 1, 1, 1, 1 } };
  gpu_quads q= { 1, &from, NULL, 0, 0, 1, 1, &v[0][0], 6 };
  draw_quads (p, q);
}

void
wg_backend::draw_shapes (const gpu_pass& p, const std::vector<gpu_shape>& shapes,
                         int x1, int y1, int x2, int y2) {
  if (!started || shapes.empty () || x1 >= x2 || y1 >= y2) return;
  bool sm= (x2 - x1 <= SMALL && y2 - y1 <= SMALL);
  unsigned long long key= sm ? hash_shapes (shapes, x1, y1, x2, y2) : 0;
  if (sm)
    for (auto it= shape_cache.begin (); it != shape_cache.end (); ++it)
      if (it->key == key) {
        shape_cache.splice (shape_cache.begin (), shape_cache, it);
        compose (p, *it->tex, x1, y1, x1, y1, x2, y2);
        return;
      }
  if (sm ? !ensure_small () : !ensure_scratch (p.w, p.h)) return;
  // what was drawn before goes first: ThorVG submits its own commands (into
  // its targets, kept: they change ThorVG's state)
  submit ();
  tvg::WgCanvas* c= sm ? tvg_small : tvg;
  gpu_texture_rep& sc= sm ? small : scratch;
  int ox= sm ? x1 : 0, oy= sm ? y1 : 0;
  if (sm) {
    tvg::Scene* scene= tvg::Scene::gen ();
    for (const gpu_shape& s: shapes) scene->add (as_tvg (s));
    scene->translate ((float) -ox, (float) -oy);
    c->add (scene);
  }
  else for (const gpu_shape& s: shapes) c->add (as_tvg (s));
  c->draw (true);
  c->sync ();
  c->remove ();
  if (!sm) { compose (p, sc, ox, oy, x1, y1, x2, y2); return; }
  // kept (in a texture of the box, of the format of ThorVG's targets)
  gpu_texture t= tm_new<gpu_texture_rep> ();
  t->linear= false; t->repeat= false;
  make_texture (*t, x2 - x1, y2 - y1, GPU_RGBA8, true, true);
  WGPUTexelCopyTextureInfo a= {}, b= {};
  a.texture= sc.tex; a.aspect= WGPUTextureAspect_All;
  b.texture= t->tex; b.aspect= WGPUTextureAspect_All;
  WGPUExtent3D size= { (uint32_t) (x2 - x1), (uint32_t) (y2 - y1), 1 };
  wgpuCommandEncoderCopyTextureToTexture (encoder (), &a, &b, &size);
  shape_cache.push_front (shape_entry { key, t });
  if (shape_cache.size () > 256) {
    delete_texture (shape_cache.back ().tex);
    shape_cache.pop_back ();
  }
  compose (p, *t, x1, y1, x1, y1, x2, y2);
}

/******************************************************************************
* Copies
******************************************************************************/

void
wg_backend::copy (gpu_texture src, int sw, int sh, int x1, int y1, int x2, int y2,
                  gpu_texture dst, int dw, int dh,
                  int dx1, int dy1, int dx2, int dy2) {
  if (!started || x2 <= x1 || y2 <= y1 || dx2 <= dx1 || dy2 <= dy1) return;
  if (src == NULL && !ensure_screen (sw, sh)) return;
  if (dst == NULL && !ensure_screen (dw, dh)) return;
  gpu_texture s= src ? src : &screen, d= dst ? dst : &screen;
  if (s->tex == NULL || d->tex == NULL) return;
  int w= x2 - x1, h= y2 - y1;
  if (w == dx2 - dx1 && h == dy2 - dy1 && s != d) {
    // the same size: a copy, within both textures
    int cx= max (0, max (-x1, -dx1)), cy= max (0, max (-y1, -dy1));
    x1 += cx; dx1 += cx; y1 += cy; dy1 += cy; w -= cx; h -= cy;
    w= min (w, min (s->w - x1, d->w - dx1));
    h= min (h, min (s->h - y1, d->h - dy1));
    if (w <= 0 || h <= 0) return;
    end_pass ();
    WGPUTexelCopyTextureInfo a= {}, b= {};
    a.texture= s->tex; a.origin= { (uint32_t) x1, (uint32_t) y1, 0 };
    a.aspect= WGPUTextureAspect_All;
    b.texture= d->tex; b.origin= { (uint32_t) dx1, (uint32_t) dy1, 0 };
    b.aspect= WGPUTextureAspect_All;
    WGPUExtent3D size= { (uint32_t) w, (uint32_t) h, 1 };
    wgpuCommandEncoderCopyTextureToTexture (encoder (), &a, &b, &size);
    return;
  }
  // scaled: a quad which samples the source linearly (the source is
  // opaque, as the targets)
  float u0= (float) x1 / sw, u1= (float) x2 / sw, v0= (float) y1 / sh, v1= (float) y2 / sh;
  float X1= (float) dx1, X2= (float) dx2, Y1= (float) dy1, Y2= (float) dy2;
  float v[6][8]= { { X1, Y1, u0, v0, 1, 1, 1, 1 }, { X2, Y1, u1, v0, 1, 1, 1, 1 },
                   { X1, Y2, u0, v1, 1, 1, 1, 1 }, { X2, Y1, u1, v0, 1, 1, 1, 1 },
                   { X2, Y2, u1, v1, 1, 1, 1, 1 }, { X1, Y2, u0, v1, 1, 1, 1, 1 } };
  gpu_pass p= { dst, dw, dh, dw, dh, dx1, dy1, dx2, dy2 };
  gpu_quads q= { 1, s, NULL, 0, 0, 1, 1, &v[0][0], 6 };
  draw_quads (p, q, true);
}

gpu_backend*
gpu_webgpu_backend () {
  static wg_backend* b= NULL;
  if (b == NULL) b= tm_new<wg_backend> ();
  return b;
}

#endif // __EMSCRIPTEN__ && USE_THORVG
