/******************************************************************************
* MODULE     : gpu_backend_metal.mm
* DESCRIPTION: The Metal backend of the GPU renderer of the Vue port (macOS),
*              with Core Graphics for the vector shapes
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// One device and one queue for all the windows; each window has a
// CAMetalLayer (SDL_Metal_CreateView), whose drawable is taken when the
// window is first drawn into in a frame and presented by present. The
// commands go into one command buffer, committed when it is presented,
// flushed or read back; consecutive draws into one target share a render
// encoder. A change of a texture which may still be read by commands
// already encoded (the atlas, the textures of Slug) is a copy from a
// buffer, encoded after them. The targets and the drawables are BGRA8:
// the texture of a target has its first row at the top (y down, as the
// renderer), and the pixels read back are turned into RGBA. The vector
// shapes are rasterized by Core Graphics over their box (antialiased, in
// the CPU, as the Cocoa port draws them), uploaded and composed.

#include "gpu_backend.hpp"

#ifndef USE_METAL

gpu_backend* gpu_metal_backend () { return NULL; }

#else

#include "tm_ostream.hpp"
#include "basic.hpp"
#ifdef VUETEXMACS
#include <SDL3/SDL.h>
#include <SDL3/SDL_metal.h>
#endif
#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#import <CoreGraphics/CoreGraphics.h>
#include <unordered_map>
#include <cmath>
#include <time.h>

struct gpu_texture_rep {
  id<MTLTexture> tex;
  int w, h;
  gpu_format f;
  bool linear, repeat;
  bool target;
};

/******************************************************************************
* The shaders (Metal Shading Language): those of gpu_backend_gl.cpp
******************************************************************************/

static const char* metal_src= R"MSL(
#include <metal_stdlib>
using namespace metal;

struct QU { float2 size; float2 porig; float2 ptile; int mode; };
struct QOut { float4 position [[position]]; float2 uv; float4 col; float2 pos; };

vertex QOut quad_vs (uint vid [[vertex_id]], const device float* v [[buffer(0)]],
                     constant QU& u [[buffer(1)]]) {
  const device float* p= v + vid * 8;
  QOut o;
  o.pos= float2 (p[0], p[1]);
  o.uv= float2 (p[2], p[3]);
  o.col= float4 (p[4], p[5], p[6], p[7]);
  o.position= float4 (o.pos.x / u.size.x * 2.0 - 1.0,
                      1.0 - o.pos.y / u.size.y * 2.0, 0.0, 1.0);
  return o;
}

fragment float4 quad_fs (QOut in [[stage_in]], constant QU& u [[buffer(0)]],
                         texture2d<float> tex [[texture(0)]], sampler ts [[sampler(0)]],
                         texture2d<float> atlas [[texture(1)]], sampler as [[sampler(1)]]) {
  if (u.mode == 1) return tex.sample (ts, in.uv) * in.col.a;
  if (u.mode == 3)
    return atlas.sample (as, in.uv).r *
           tex.sample (ts, (in.pos - u.porig) / u.ptile) * in.col.a;
  return atlas.sample (as, in.uv).r * in.col;
}

struct SU { float2 size; float bold; };
struct SOut {
  float4 position [[position]];
  float2 em;
  float4 box [[flat]];
  uint2 band [[flat]];
  float4 col [[flat]];
  float scale [[flat]];
};

vertex SOut slug_vs (uint vid [[vertex_id]], uint iid [[instance_id]],
                     const device float* I [[buffer(0)]], constant SU& u [[buffer(1)]]) {
  const device float* p= I + iid * 13;
  float3 org= float3 (p[0], p[1], p[2]);
  float4 box= float4 (p[3], p[4], p[5], p[6]);
  float2 c= float2 ((vid == 1 || vid == 3 || vid == 4) ? 1.0 : 0.0,
                    (vid == 2 || vid == 4 || vid == 5) ? 1.0 : 0.0);
  float pad= (1.0 + u.bold) / org.z;
  float2 em= mix (box.xy - pad, box.zw + pad, c);
  float2 q= org.xy + float2 (em.x, -em.y) * org.z;
  SOut o;
  o.em= em; o.box= box;
  o.band= uint2 (as_type<uint> (p[7]), as_type<uint> (p[8]));
  o.col= float4 (p[9], p[10], p[11], p[12]);
  o.scale= org.z;
  o.position= float4 (q.x / u.size.x * 2.0 - 1.0, 1.0 - q.y / u.size.y * 2.0, 0.0, 1.0);
  return o;
}

constant int W= 1024;

static uint idx (texture2d<uint> b, uint i) {
  return b.read (uint2 (i % W, i / W)).r; }
static float4 crv (texture2d<float> c, uint k, int j) {
  uint i= k * 2 + j; return c.read (uint2 (i % W, i / W)); }

static float ray (texture2d<float> cv, texture2d<uint> bd, float2 em, float scale,
                  uint start, uint count, bool vertical, float hb, thread float& near) {
  float cov= 0.0; near= 1.0e9;
  for (uint n= 0; n < count; n++) {
    uint k= idx (bd, start + n);
    float4 a= crv (cv, k, 0); float4 b= crv (cv, k, 1);
    float2 p1= a.xy - em, p2= a.zw - em, p3= b.xy - em;
    if (vertical) { p1= p1.yx; p2= p2.yx; p3= p3.yx; }
    if (max (max (p1.x, p2.x), p3.x) * scale < -0.5 - abs (hb)) break;
    uint code= (0x2E74u >> ((p1.y > 0.0 ? 2u : 0u) + (p2.y > 0.0 ? 4u : 0u) +
                            (p3.y > 0.0 ? 8u : 0u))) & 3u;
    if (code == 0u) continue;
    float2 A= p1 - p2 * 2.0 + p3, B= p1 - p2;
    float t1, t2;
    if (abs (A.y) < 1.0e-5) { t1= t2= p1.y * 0.5 / B.y; }
    else {
      float ra= 1.0 / A.y, d= sqrt (max (B.y * B.y - A.y * p1.y, 0.0));
      t1= (B.y - d) * ra; t2= (B.y + d) * ra;
    }
    float x1= (A.x * t1 - B.x * 2.0) * t1 + p1.x;
    float x2= (A.x * t2 - B.x * 2.0) * t2 + p1.x;
    if ((code & 1u) != 0u) {
      cov += clamp (x1 * scale + 0.5 + hb, 0.0, 1.0); near= min (near, abs (x1 * scale + hb)); }
    if (code > 1u) {
      cov -= clamp (x2 * scale + 0.5 - hb, 0.0, 1.0); near= min (near, abs (x2 * scale - hb)); }
  }
  return cov;
}

fragment float4 slug_fs (SOut in [[stage_in]], constant SU& u [[buffer(0)]],
                         texture2d<float> curves [[texture(0)]],
                         texture2d<uint> bands [[texture(1)]]) {
  uint nb= in.band.y & 0x7FFFFFFFu, h= in.band.x;
  // the edges moved outwards by half of the boldness (the direction of the
  // outline in the high bit; the vertical ray sees it mirrored)
  float hb= ((in.band.y >> 31) != 0u ? 0.5 : -0.5) * u.bold;
  float2 rel= (in.em - in.box.xy) / max (in.box.zw - in.box.xy, float2 (1.0e-6));
  uint by= uint (clamp (floor (rel.y * float (nb)), 0.0, float (nb) - 1.0));
  uint bx= uint (clamp (floor (rel.x * float (nb)), 0.0, float (nb) - 1.0));
  float nx, ny;
  float cx= abs (ray (curves, bands, in.em, in.scale, idx (bands, h + 2u * by),
                      idx (bands, h + 2u * by + 1u), false, hb, nx));
  float cy= abs (ray (curves, bands, in.em, in.scale, idx (bands, h + 2u * (nb + bx)),
                      idx (bands, h + 2u * (nb + bx) + 1u), true, -hb, ny));
  float wx= clamp (1.0 - nx * 2.0, 0.0, 1.0), wy= clamp (1.0 - ny * 2.0, 0.0, 1.0);
  float c= max ((cx * wx + cy * wy) / max (wx + wy, 1.0 / 65536.0), min (cx, cy));
  return in.col * clamp (c, 0.0, 1.0);
}
)MSL";

struct quad_uniforms { float size[2]; float porig[2]; float ptile[2]; int mode; int pad; };

/******************************************************************************
* The backend
******************************************************************************/

class metal_backend: public gpu_backend {
  id<MTLDevice> device= nil;
  id<MTLCommandQueue> queue= nil;
  id<MTLRenderPipelineState> quad_pipe= nil, slug_pipe= nil;
  id<MTLSamplerState> samplers[4];       // linear + 2 * repeat
  id<MTLTexture> blank= nil;             // 1x1 white, for unused bindings
  bool failed= false;
  bool started= false;
  // the shaders and the pipelines, compiled while TeXmacs starts (warm_up:
  // some 0.3 s when Metal has not cached them, at the first start after an
  // update); what went wrong, printed by prepare
  dispatch_group_t compiling= nil;
  NSMutableString* compile_log= nil;
  string device_name;
  // the windows, by their SDL id: their layers
  std::unordered_map<unsigned, CAMetalLayer*> layers;
  CAMetalLayer* layer= nil;              // of the window being drawn
  id<CAMetalDrawable> drawable= nil;     // of this frame, once drawn into
  // the presents into each layer: when the last one was made, and the
  // refresh period of its screen (0: not paced, as the layers of Cocoa,
  // presented by a display link)
  struct pacing { double last; double period; };
  std::unordered_map<void*, pacing> pacings;
  // the commands being encoded
  id<MTLCommandBuffer> cmd= nil;
  id<MTLCommandBuffer> last= nil;        // committed last (finish waits)
  id<MTLRenderCommandEncoder> enc= nil;
  id<MTLTexture> enc_target= nil;        // what enc draws into

  void compile ();
  bool init ();
  id<MTLCommandBuffer> commands ();
  void end_encoder ();
  id<MTLTexture> texture_of (gpu_texture t);
  id<MTLRenderCommandEncoder> encoder (id<MTLTexture> target);
  void set_pass (id<MTLRenderCommandEncoder> e, const gpu_pass& p);
  id<MTLSamplerState> sampler (gpu_texture t);
  void upload (id<MTLTexture> t, int x, int y, int w, int h, int bpp,
               const void* data);

public:
  const char* name () { return "Metal"; }
#ifdef VUETEXMACS
  unsigned long long window_flags () { return SDL_WINDOW_METAL; }
#else
  unsigned long long window_flags () { return 0; }
#endif
  void warm_up ();
  bool prepare ();
  bool attach (SDL_Window* w);
  bool attach_layer (void* layer);
  string description () { return device_name * " (Metal)"; }
  void begin_screen (int dw, int dh);
  void present (SDL_Window* w);
  bool ready_to_present ();

  gpu_texture new_texture (int w, int h, gpu_format f, bool linear, bool repeat,
                           const void* data);
  void update_texture (gpu_texture t, int x, int y, int w, int h, const void* data);
  void delete_texture (gpu_texture t);
  gpu_texture new_target (int w, int h, bool transparent= false);
  bool targets_flipped () { return false; }

  void draw_quads (const gpu_pass& p, const gpu_quads& q);
  void draw_slug (const gpu_pass& p, gpu_texture curves, gpu_texture bands,
                  const float* inst, int n, float bold);
  void draw_shapes (const gpu_pass& p, const std::vector<gpu_shape>& s,
                    int x1, int y1, int x2, int y2);
  void copy (gpu_texture src, int sw, int sh, int x1, int y1, int x2, int y2,
             gpu_texture dst, int dw, int dh, int dx1, int dy1, int dx2, int dy2);
  bool read (gpu_texture src, int sw, int sh, int x, int y, int w, int h,
             unsigned char* rgba);
  void flush ();
  void finish ();
};

void
metal_backend::warm_up () {
  if (device != nil || compiling != nil) return;
  device= MTLCreateSystemDefaultDevice ();
  if (device == nil) return;
  compiling= dispatch_group_create ();
  metal_backend* me= this;
  dispatch_group_async (compiling,
                        dispatch_get_global_queue (QOS_CLASS_USER_INITIATED, 0),
                        ^{ me->compile (); });
}

bool
metal_backend::prepare () {
  static bool done= false;
  if (done) return !failed;
  done= true;
  if (device == nil) device= MTLCreateSystemDefaultDevice ();
  if (device == nil) {
    cout << "TeXmacs] GPU: no Metal device, drawing on the CPU" << LF;
    failed= true;
    return false;
  }
  // (the pipelines before any window: the Cocoa port makes the backing
  // stores of its canvases before their layers are drawn into)
  if (compiling != nil) dispatch_group_wait (compiling, DISPATCH_TIME_FOREVER);
  else compile ();
  if (compile_log != nil && [compile_log length] > 0)
    cout << [compile_log UTF8String];
  if (!init ()) { failed= true; return false; }
  started= true;
  return true;
}

static id<MTLRenderPipelineState>
make_pipeline (id<MTLDevice> dev, id<MTLLibrary> lib, const char* vs, const char* fs,
               NSMutableString* log) {
  MTLRenderPipelineDescriptor* d= [[MTLRenderPipelineDescriptor alloc] init];
  d.vertexFunction= [lib newFunctionWithName: [NSString stringWithUTF8String: vs]];
  d.fragmentFunction= [lib newFunctionWithName: [NSString stringWithUTF8String: fs]];
  MTLRenderPipelineColorAttachmentDescriptor* c= d.colorAttachments[0];
  c.pixelFormat= MTLPixelFormatBGRA8Unorm;
  // premultiplied: ONE, ONE_MINUS_SRC_ALPHA, as the GL backend
  c.blendingEnabled= YES;
  c.rgbBlendOperation= MTLBlendOperationAdd;
  c.alphaBlendOperation= MTLBlendOperationAdd;
  c.sourceRGBBlendFactor= MTLBlendFactorOne;
  c.sourceAlphaBlendFactor= MTLBlendFactorOne;
  c.destinationRGBBlendFactor= MTLBlendFactorOneMinusSourceAlpha;
  c.destinationAlphaBlendFactor= MTLBlendFactorOneMinusSourceAlpha;
  NSError* err= nil;
  id<MTLRenderPipelineState> p= [dev newRenderPipelineStateWithDescriptor: d error: &err];
  if (p == nil)
    [log appendFormat: @"TeXmacs] GPU: Metal pipeline %s: %@\n",
                       fs, [err localizedDescription]];
  return p;
}

// the shaders and the pipelines (on any thread: nothing else is touched;
// the messages go to compile_log)
void
metal_backend::compile () {
  NSMutableString* log= [NSMutableString string];
  NSError* err= nil;
  id<MTLLibrary> lib= [device newLibraryWithSource: [NSString stringWithUTF8String: metal_src]
                                           options: nil error: &err];
  if (lib == nil)
    [log appendFormat: @"TeXmacs] GPU: the Metal shaders do not compile: %@\n",
                       [err localizedDescription]];
  else {
    quad_pipe= make_pipeline (device, lib, "quad_vs", "quad_fs", log);
    slug_pipe= make_pipeline (device, lib, "slug_vs", "slug_fs", log);
  }
  compile_log= log;
}

bool
metal_backend::init () {
  if (quad_pipe == nil) return false;
  queue= [device newCommandQueue];
  for (int i= 0; i < 4; i++) {
    MTLSamplerDescriptor* s= [[MTLSamplerDescriptor alloc] init];
    s.minFilter= s.magFilter= (i & 1) ? MTLSamplerMinMagFilterLinear
                                      : MTLSamplerMinMagFilterNearest;
    s.sAddressMode= s.tAddressMode= (i & 2) ? MTLSamplerAddressModeRepeat
                                            : MTLSamplerAddressModeClampToEdge;
    samplers[i]= [device newSamplerStateWithDescriptor: s];
  }
  MTLTextureDescriptor* bd=
    [MTLTextureDescriptor texture2DDescriptorWithPixelFormat: MTLPixelFormatRGBA8Unorm
                                                       width: 1 height: 1 mipmapped: NO];
  bd.storageMode= MTLStorageModeManaged;
  blank= [device newTextureWithDescriptor: bd];
  unsigned char white[4]= { 255, 255, 255, 255 };
  [blank replaceRegion: MTLRegionMake2D (0, 0, 1, 1) mipmapLevel: 0
             withBytes: white bytesPerRow: 4];
  device_name= string ([[device name] UTF8String]);
  return true;
}

// a layer drawn into by this device (made by SDL or by the Cocoa port)
static void
configure_layer (CAMetalLayer* l, id<MTLDevice> device) {
  if (l.device == device) return;
  l.device= device;
  l.pixelFormat= MTLPixelFormatBGRA8Unorm;
  l.framebufferOnly= NO;          // read back (snapshots) and copied
  l.displaySyncEnabled= NO;       // several windows: each would wait
  CGColorSpaceRef cs= CGColorSpaceCreateWithName (kCGColorSpaceSRGB);
  l.colorspace= cs;
  CGColorSpaceRelease (cs);
}

bool
metal_backend::attach_layer (void* lp) {
  if (failed || lp == NULL || !started) return false;
  CAMetalLayer* l= (__bridge CAMetalLayer*) lp;
  configure_layer (l, device);
  if (layer != l) {
    // (what was drawn into the last layer is presented by present)
    end_encoder ();
    drawable= nil;
    layer= l;
  }
  return true;
}

bool
metal_backend::attach (SDL_Window* w) {
#ifndef VUETEXMACS
  (void) w;
  return false;
#else
  if (failed || w == NULL || !started) return false;
  unsigned id= (unsigned) SDL_GetWindowID (w);
  auto it= layers.find (id);
  if (it == layers.end ()) {
    SDL_MetalView v= SDL_Metal_CreateView (w);
    if (v == NULL) {
      cout << "TeXmacs] GPU: no Metal view (" << SDL_GetError () << ")" << LF;
      return false;
    }
    CAMetalLayer* l= (__bridge CAMetalLayer*) SDL_Metal_GetLayer (v);
    layers[id]= l;
    it= layers.find (id);
  }
  if (!attach_layer ((__bridge void*) it->second)) return false;
  // the refresh period of the screen of the window (it may move)
  double rate= 60.0;
  SDL_DisplayID d= SDL_GetDisplayForWindow (w);
  const SDL_DisplayMode* m= d != 0 ? SDL_GetCurrentDisplayMode (d) : NULL;
  if (m != NULL && m->refresh_rate > 0) rate= m->refresh_rate;
  pacings[(__bridge void*) it->second].period= 1000.0 / rate;
  return true;
#endif
}

void
metal_backend::begin_screen (int dw, int dh) {
  if (layer == nil) return;
  CGSize s= layer.drawableSize;
  if ((int) s.width != dw || (int) s.height != dh)
    layer.drawableSize= CGSizeMake (dw, dh);
  if (drawable != nil && enc_target == drawable.texture) end_encoder ();
  drawable= nil;  // taken when first drawn into
}

static double
now_ms () {
  struct timespec ts;
  clock_gettime (CLOCK_MONOTONIC, &ts);
  return ts.tv_sec * 1e3 + ts.tv_nsec * 1e-6;
}

void
metal_backend::present (SDL_Window* w) {
  (void) w;
  end_encoder ();
  if (drawable != nil) {
    if (layer != nil) pacings[(__bridge void*) layer].last= now_ms ();
    [commands () presentDrawable: drawable];
    drawable= nil;
  }
  flush ();
}

bool
metal_backend::ready_to_present () {
  // A layer has three drawables, which the compositor gives back once it
  // has shown them (some two refreshes later): presented in bursts, faster
  // than the screen refreshes, they ran out and nextDrawable waited (3 to
  // 5 ms a frame while scrolling, presents 10 ms apart). So no two presents
  // within half a refresh period; the window is drawn at a later turn of
  // the loop otherwise. While scrolling on a screen at 120 Hz, the presents
  // are then 6 to 7 ms apart and nextDrawable no longer waits; a whole
  // period skipped a turn of the loop each time (presents 15 ms apart).
  // (Counting the frames not shown yet with presented handlers did not
  // work: a frame replaced before a refresh is not reported, and the count
  // stuck.)
  if (layer == nil) return true;
  auto it= pacings.find ((__bridge void*) layer);
  if (it == pacings.end () || it->second.period <= 0) return true;
  return now_ms () - it->second.last >= 0.5 * it->second.period;
}

/******************************************************************************
* Commands
******************************************************************************/

id<MTLCommandBuffer>
metal_backend::commands () {
  if (cmd == nil) cmd= [queue commandBuffer];
  return cmd;
}

void
metal_backend::end_encoder () {
  if (enc != nil) { [enc endEncoding]; enc= nil; enc_target= nil; }
}

void
metal_backend::flush () {
  end_encoder ();
  if (cmd != nil) { [cmd commit]; last= cmd; cmd= nil; }
}

void
metal_backend::finish () {
  flush ();
  if (last != nil) { [last waitUntilCompleted]; last= nil; }
}

id<MTLTexture>
metal_backend::texture_of (gpu_texture t) {
  if (t != NULL) return t->tex;
  if (drawable == nil && layer != nil) drawable= [layer nextDrawable];
  return drawable == nil ? nil : drawable.texture;
}

id<MTLRenderCommandEncoder>
metal_backend::encoder (id<MTLTexture> target) {
  if (enc != nil && enc_target == target) return enc;
  end_encoder ();
  MTLRenderPassDescriptor* d= [MTLRenderPassDescriptor renderPassDescriptor];
  d.colorAttachments[0].texture= target;
  d.colorAttachments[0].loadAction= MTLLoadActionLoad;
  d.colorAttachments[0].storeAction= MTLStoreActionStore;
  enc= [commands () renderCommandEncoderWithDescriptor: d];
  enc_target= target;
  return enc;
}

void
metal_backend::set_pass (id<MTLRenderCommandEncoder> e, const gpu_pass& p) {
  MTLViewport vp= { 0, 0, (double) p.dw, (double) p.dh, 0, 1 };
  [e setViewport: vp];
  int a1= p.sx1, a2= p.sx2, b1= p.sy1, b2= p.sy2;
  if (p.dw != p.w || p.dh != p.h) {
    // a scaled window: the pixels which the box touches
    double fx= (double) p.dw / p.w, fy= (double) p.dh / p.h;
    a1= (int) floor (p.sx1 * fx); a2= (int) ceil (p.sx2 * fx);
    b1= (int) floor (p.sy1 * fy); b2= (int) ceil (p.sy2 * fy);
  }
  a1= max (0, min (a1, p.dw)); a2= max (a1, min (a2, p.dw));
  b1= max (0, min (b1, p.dh)); b2= max (b1, min (b2, p.dh));
  MTLScissorRect r= { (NSUInteger) a1, (NSUInteger) b1,
                      (NSUInteger) (a2 - a1), (NSUInteger) (b2 - b1) };
  [e setScissorRect: r];
}

id<MTLSamplerState>
metal_backend::sampler (gpu_texture t) {
  if (t == NULL) return samplers[0];
  return samplers[(t->linear ? 1 : 0) + (t->repeat ? 2 : 0)];
}

/******************************************************************************
* Textures and targets
******************************************************************************/

static MTLPixelFormat
pixel_format (gpu_format f, int& bpp) {
  switch (f) {
  case GPU_R8: bpp= 1; return MTLPixelFormatR8Unorm;
  case GPU_RGBA32F: bpp= 16; return MTLPixelFormatRGBA32Float;
  case GPU_R32UI: bpp= 4; return MTLPixelFormatR32Uint;
  default: bpp= 4; return MTLPixelFormatRGBA8Unorm;
  }
}

// The largest side of a texture: Metal stops the program (an assertion of
// the validation of its descriptor) for a larger one, as a box of text with
// a very long line asked for one 30638 pixels wide. Such a texture is not
// made: what would be drawn with it is left out (the callers check)
static const int metal_max_side= 16384;

static bool
too_large (int w, int h) {
  if (w <= metal_max_side && h <= metal_max_side) return false;
  static bool warned= false;
  if (!warned) {
    warned= true;
    cout << "TeXmacs] GPU: a texture of " << w << "x" << h
         << " is larger than " << metal_max_side << ", not drawn" << LF;
  }
  return true;
}

gpu_texture
metal_backend::new_texture (int w, int h, gpu_format f, bool linear, bool repeat,
                            const void* data) {
  if (!started || w <= 0 || h <= 0 || too_large (w, h)) return NULL;
  int bpp;
  MTLPixelFormat pf= pixel_format (f, bpp);
  MTLTextureDescriptor* d=
    [MTLTextureDescriptor texture2DDescriptorWithPixelFormat: pf
                                                       width: w height: h mipmapped: NO];
  d.storageMode= MTLStorageModeManaged;
  d.usage= MTLTextureUsageShaderRead;
  id<MTLTexture> t= [device newTextureWithDescriptor: d];
  if (t == nil) return NULL;
  // a new texture: nothing encoded reads it yet
  if (data != NULL)
    [t replaceRegion: MTLRegionMake2D (0, 0, w, h) mipmapLevel: 0
           withBytes: data bytesPerRow: (NSUInteger) w * bpp];
  else {
    std::vector<unsigned char> zero ((size_t) w * h * bpp, 0);
    [t replaceRegion: MTLRegionMake2D (0, 0, w, h) mipmapLevel: 0
           withBytes: zero.data () bytesPerRow: (NSUInteger) w * bpp];
  }
  gpu_texture r= tm_new<gpu_texture_rep> ();
  r->tex= t; r->w= w; r->h= h; r->f= f;
  r->linear= linear; r->repeat= repeat; r->target= false;
  return r;
}

void
metal_backend::upload (id<MTLTexture> t, int x, int y, int w, int h, int bpp,
                       const void* data) {
  // after what was encoded before (which may read the texture): a copy
  // from a buffer, in the order of the commands
  size_t row= (size_t) w * bpp, stride= (row + 3) & ~((size_t) 3);
  id<MTLBuffer> b= [device newBufferWithLength: std::max ((size_t) 4, stride * h)
                                       options: MTLResourceStorageModeShared];
  unsigned char* dst= (unsigned char*) [b contents];
  for (int r= 0; r < h; r++)
    memcpy (dst + r * stride, (const unsigned char*) data + r * row, row);
  end_encoder ();
  id<MTLBlitCommandEncoder> be= [commands () blitCommandEncoder];
  [be copyFromBuffer: b sourceOffset: 0 sourceBytesPerRow: stride
     sourceBytesPerImage: stride * h sourceSize: MTLSizeMake (w, h, 1)
               toTexture: t destinationSlice: 0 destinationLevel: 0
       destinationOrigin: MTLOriginMake (x, y, 0)];
  [be endEncoding];
}

void
metal_backend::update_texture (gpu_texture t, int x, int y, int w, int h,
                               const void* data) {
  if (t == NULL || w <= 0 || h <= 0) return;
  int bpp;
  pixel_format (t->f, bpp);
  upload (t->tex, x, y, w, h, bpp, data);
}

void
metal_backend::delete_texture (gpu_texture t) {
  if (t == NULL) return;
  if (enc_target == t->tex) end_encoder ();
  // (the commands which use it keep it until they are done)
  tm_delete (t);
}

gpu_texture
metal_backend::new_target (int w, int h, bool transparent) {
  if (!started || w <= 0 || h <= 0 || too_large (w, h)) return NULL;
  MTLTextureDescriptor* d=
    [MTLTextureDescriptor texture2DDescriptorWithPixelFormat: MTLPixelFormatBGRA8Unorm
                                                       width: w height: h mipmapped: NO];
  d.storageMode= MTLStorageModePrivate;
  d.usage= MTLTextureUsageRenderTarget | MTLTextureUsageShaderRead;
  id<MTLTexture> t= [device newTextureWithDescriptor: d];
  if (t == nil) return NULL;
  // opaque white, as native_opaque_picture, or transparent
  end_encoder ();
  MTLRenderPassDescriptor* pd= [MTLRenderPassDescriptor renderPassDescriptor];
  pd.colorAttachments[0].texture= t;
  pd.colorAttachments[0].loadAction= MTLLoadActionClear;
  pd.colorAttachments[0].clearColor= transparent ? MTLClearColorMake (0, 0, 0, 0)
                                                 : MTLClearColorMake (1, 1, 1, 1);
  pd.colorAttachments[0].storeAction= MTLStoreActionStore;
  [[commands () renderCommandEncoderWithDescriptor: pd] endEncoding];
  gpu_texture r= tm_new<gpu_texture_rep> ();
  r->tex= t; r->w= w; r->h= h; r->f= GPU_RGBA8;
  r->linear= false; r->repeat= false; r->target= true;
  return r;
}

/******************************************************************************
* Drawing
******************************************************************************/

void
metal_backend::draw_quads (const gpu_pass& p, const gpu_quads& q) {
  if (!started || q.nr_verts <= 0) return;
  id<MTLTexture> target= texture_of (p.target);
  if (target == nil) return;
  id<MTLRenderCommandEncoder> e= encoder (target);
  set_pass (e, p);
  [e setRenderPipelineState: quad_pipe];
  size_t bytes= (size_t) q.nr_verts * 8 * sizeof (float);
  if (bytes <= 4096) [e setVertexBytes: q.verts length: bytes atIndex: 0];
  else {
    id<MTLBuffer> b= [device newBufferWithBytes: q.verts length: bytes
                                        options: MTLResourceStorageModeShared];
    [e setVertexBuffer: b offset: 0 atIndex: 0];
  }
  quad_uniforms u= { { (float) p.w, (float) p.h }, { q.porig_x, q.porig_y },
                     { q.ptile_w, q.ptile_h }, q.mode, 0 };
  [e setVertexBytes: &u length: sizeof (u) atIndex: 1];
  [e setFragmentBytes: &u length: sizeof (u) atIndex: 0];
  gpu_texture tex= (q.mode != 2) ? q.tex : NULL;
  [e setFragmentTexture: (tex != NULL ? tex->tex : blank) atIndex: 0];
  [e setFragmentSamplerState: sampler (tex) atIndex: 0];
  [e setFragmentTexture: (q.atlas != NULL ? q.atlas->tex : blank) atIndex: 1];
  [e setFragmentSamplerState: sampler (q.atlas) atIndex: 1];
  [e drawPrimitives: MTLPrimitiveTypeTriangle vertexStart: 0 vertexCount: q.nr_verts];
}

void
metal_backend::draw_slug (const gpu_pass& p, gpu_texture curves, gpu_texture bands,
                          const float* inst, int n, float bold) {
  if (!started || slug_pipe == nil || n <= 0 || curves == NULL || bands == NULL)
    return;
  id<MTLTexture> target= texture_of (p.target);
  if (target == nil) return;
  id<MTLRenderCommandEncoder> e= encoder (target);
  set_pass (e, p);
  [e setRenderPipelineState: slug_pipe];
  size_t bytes= (size_t) n * 13 * sizeof (float);
  if (bytes <= 4096) [e setVertexBytes: inst length: bytes atIndex: 0];
  else {
    id<MTLBuffer> b= [device newBufferWithBytes: inst length: bytes
                                        options: MTLResourceStorageModeShared];
    [e setVertexBuffer: b offset: 0 atIndex: 0];
  }
  float su[4]= { (float) p.w, (float) p.h, bold, 0 };
  [e setVertexBytes: su length: sizeof (su) atIndex: 1];
  [e setFragmentBytes: su length: sizeof (su) atIndex: 0];
  [e setFragmentTexture: curves->tex atIndex: 0];
  [e setFragmentTexture: bands->tex atIndex: 1];
  [e drawPrimitives: MTLPrimitiveTypeTriangle vertexStart: 0 vertexCount: 6
      instanceCount: n];
}

void
metal_backend::draw_shapes (const gpu_pass& p, const std::vector<gpu_shape>& shapes,
                            int x1, int y1, int x2, int y2) {
  if (!started || shapes.empty () || x1 >= x2 || y1 >= y2) return;
  int w= x2 - x1, h= y2 - y1;
  // the box, rasterized by Core Graphics (premultiplied RGBA, y down)
  std::vector<unsigned char> px ((size_t) w * h * 4, 0);
  CGColorSpaceRef cs= CGColorSpaceCreateDeviceRGB ();
  CGContextRef ctx= CGBitmapContextCreate (px.data (), w, h, 8, w * 4, cs,
                                           (uint32_t) kCGImageAlphaPremultipliedLast |
                                           (uint32_t) kCGBitmapByteOrder32Big);
  CGColorSpaceRelease (cs);
  if (ctx == NULL) return;
  CGContextTranslateCTM (ctx, 0, h);
  CGContextScaleCTM (ctx, 1, -1);
  CGContextTranslateCTM (ctx, -x1, -y1);
  CGContextSetShouldAntialias (ctx, true);
  for (const gpu_shape& s: shapes) {
    CGMutablePathRef path= CGPathCreateMutable ();
    size_t k= 0;
    for (unsigned char c: s.cmds)
      switch (c) {
      case 0: CGPathMoveToPoint (path, NULL, s.pts[k], s.pts[k+1]); k += 2; break;
      case 1: CGPathAddLineToPoint (path, NULL, s.pts[k], s.pts[k+1]); k += 2; break;
      case 2: CGPathAddCurveToPoint (path, NULL, s.pts[k], s.pts[k+1], s.pts[k+2],
                                     s.pts[k+3], s.pts[k+4], s.pts[k+5]); k += 6; break;
      default: CGPathCloseSubpath (path); break;
      }
    if (s.fill) {
      CGContextSetRGBFillColor (ctx, s.fill_rgba[0] / 255.0, s.fill_rgba[1] / 255.0,
                                s.fill_rgba[2] / 255.0, s.fill_rgba[3] / 255.0);
      CGContextAddPath (ctx, path);
      if (s.even_odd) CGContextEOFillPath (ctx); else CGContextFillPath (ctx);
    }
    if (s.stroke) {
      CGContextSetRGBStrokeColor (ctx, s.stroke_rgba[0] / 255.0, s.stroke_rgba[1] / 255.0,
                                  s.stroke_rgba[2] / 255.0, s.stroke_rgba[3] / 255.0);
      CGContextSetLineWidth (ctx, s.width);
      CGContextSetLineJoin (ctx, kCGLineJoinRound);
      CGContextSetLineCap (ctx, s.cap == 1 ? kCGLineCapRound :
                                s.cap == 2 ? kCGLineCapSquare : kCGLineCapButt);
      CGContextAddPath (ctx, path);
      CGContextStrokePath (ctx);
    }
    CGPathRelease (path);
  }
  CGContextRelease (ctx);
  // composed over the box
  gpu_texture t= new_texture (w, h, GPU_RGBA8, true, false, px.data ());
  if (t == NULL) return;
  float X1= (float) x1, X2= (float) x2, Y1= (float) y1, Y2= (float) y2;
  float v[6][8]= { { X1, Y1, 0, 0, 1, 1, 1, 1 }, { X2, Y1, 1, 0, 1, 1, 1, 1 },
                   { X1, Y2, 0, 1, 1, 1, 1, 1 }, { X2, Y1, 1, 0, 1, 1, 1, 1 },
                   { X2, Y2, 1, 1, 1, 1, 1, 1 }, { X1, Y2, 0, 1, 1, 1, 1, 1 } };
  gpu_quads q= { 1, t, NULL, 0, 0, 1, 1, &v[0][0], 6 };
  draw_quads (p, q);
  delete_texture (t);
}

/******************************************************************************
* Copies and reading
******************************************************************************/

void
metal_backend::copy (gpu_texture src, int sw, int sh, int x1, int y1, int x2, int y2,
                     gpu_texture dst, int dw, int dh,
                     int dx1, int dy1, int dx2, int dy2) {
  if (!started || x2 <= x1 || y2 <= y1 || dx2 <= dx1 || dy2 <= dy1) return;
  id<MTLTexture> s= texture_of (src), d= texture_of (dst);
  if (s == nil || d == nil) return;
  int w= x2 - x1, h= y2 - y1;
  if (w == dx2 - dx1 && h == dy2 - dy1) {
    // the same size: a copy, within both textures
    int cx= max (0, max (-x1, -dx1)), cy= max (0, max (-y1, -dy1));
    x1 += cx; dx1 += cx; y1 += cy; dy1 += cy; w -= cx; h -= cy;
    w= min (w, min ((int) s.width - x1, (int) d.width - dx1));
    h= min (h, min ((int) s.height - y1, (int) d.height - dy1));
    if (w <= 0 || h <= 0) return;
    end_encoder ();
    id<MTLBlitCommandEncoder> be= [commands () blitCommandEncoder];
    [be copyFromTexture: s sourceSlice: 0 sourceLevel: 0
           sourceOrigin: MTLOriginMake (x1, y1, 0) sourceSize: MTLSizeMake (w, h, 1)
              toTexture: d destinationSlice: 0 destinationLevel: 0
      destinationOrigin: MTLOriginMake (dx1, dy1, 0)];
    [be endEncoding];
    return;
  }
  // scaled: a quad which samples the source linearly, replacing what is
  // under it (the source is opaque, as the targets)
  gpu_texture_rep tmp= { s, (int) s.width, (int) s.height, GPU_RGBA8, true, false, true };
  float u0= (float) x1 / sw, u1= (float) x2 / sw, v0= (float) y1 / sh, v1= (float) y2 / sh;
  float X1= (float) dx1, X2= (float) dx2, Y1= (float) dy1, Y2= (float) dy2;
  float v[6][8]= { { X1, Y1, u0, v0, 1, 1, 1, 1 }, { X2, Y1, u1, v0, 1, 1, 1, 1 },
                   { X1, Y2, u0, v1, 1, 1, 1, 1 }, { X2, Y1, u1, v0, 1, 1, 1, 1 },
                   { X2, Y2, u1, v1, 1, 1, 1, 1 }, { X1, Y2, u0, v1, 1, 1, 1, 1 } };
  gpu_pass p= { dst, dw, dh, dw, dh, dx1, dy1, dx2, dy2 };
  gpu_quads q= { 1, &tmp, NULL, 0, 0, 1, 1, &v[0][0], 6 };
  draw_quads (p, q);
}

bool
metal_backend::read (gpu_texture src, int sw, int sh, int x, int y, int w, int h,
                     unsigned char* rgba) {
  (void) sw; (void) sh;
  if (!started || w <= 0 || h <= 0) return false;
  id<MTLTexture> s= texture_of (src);
  if (s == nil) return false;
  if (x < 0 || y < 0 || x + w > (int) s.width || y + h > (int) s.height) return false;
  size_t stride= (size_t) w * 4;
  id<MTLBuffer> b= [device newBufferWithLength: stride * h
                                       options: MTLResourceStorageModeShared];
  end_encoder ();
  id<MTLBlitCommandEncoder> be= [commands () blitCommandEncoder];
  [be copyFromTexture: s sourceSlice: 0 sourceLevel: 0
         sourceOrigin: MTLOriginMake (x, y, 0) sourceSize: MTLSizeMake (w, h, 1)
             toBuffer: b destinationOffset: 0 destinationBytesPerRow: stride
  destinationBytesPerImage: stride * h];
  [be endEncoding];
  finish ();
  // BGRA to RGBA
  const unsigned char* p= (const unsigned char*) [b contents];
  for (size_t i= 0; i < (size_t) w * h; i++) {
    rgba[4*i]= p[4*i+2]; rgba[4*i+1]= p[4*i+1];
    rgba[4*i+2]= p[4*i]; rgba[4*i+3]= p[4*i+3];
  }
  return true;
}

gpu_backend*
gpu_metal_backend () {
  static metal_backend* b= NULL;
  if (b == NULL) b= tm_new<metal_backend> ();
  return b;
}

#endif // USE_METAL
