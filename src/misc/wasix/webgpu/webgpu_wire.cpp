/******************************************************************************
* MODULE     : webgpu_wire.cpp
* DESCRIPTION: WebGPU for WASIX: the calls of webgpu.h sent to a host which
*              makes them
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*******************************************************************************
*
* WASIX gives a program no GPU, and the Wasmer SDK no way to add imports to
* it. This implements webgpu.h (webgpu-native/webgpu-headers, the version of
* wgpu-native 29) by sending each call to a host, which makes it on a real
* WebGPU: the page of the browser on navigator.gpu (misc/wasix/web/
* webgpu-host.js). As Chrome sends the calls of a page to its GPU process.
*
* No call waits for the host. The objects are numbers which the program
* gives (the host keeps a table of them); what the program may ask of an
* object (the size of a buffer, the format of a texture) is kept here, from
* its descriptor. The adapter and the device are those of the host: asked
* for, they are given at once. So the calls of a frame flow one way. Not
* done: mapping buffers (a read of the GPU), errors back to the program.
*
* A call is a value [method, target, arguments, result], the method that of
* the WebGPU of JavaScript (createBuffer...), the target and the result
* numbers of objects (0: none), the descriptors objects with the keys of
* JavaScript. The values (little-endian):
*   0 null   1 false   2 true   3 u32   4 f64   5 string (u32 n, utf8)
*   6 bytes (u32 n, data)   7 array (u32 n, values)
*   8 object (u32 n, then n times a key (u16 n, utf8) and a value)
*   9 object of the host (u32 number)
*   10 enumerated (u8 kind, u32 value of webgpu.h): the host has the
*      strings of JavaScript (webgpu-enums.js, made from webgpu.h)
* The calls go in messages GPU (6) of the stream of the display (vue_stream.cpp),
* in batches: at each submit and present, and when a batch is large.
******************************************************************************/

#include "webgpu_wire.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <vector>

/******************************************************************************
* The values
******************************************************************************/

// the kinds of the enumerated values (as in webgpu-host.js)
enum wire_kind {
  K_TextureFormat= 1, K_VertexFormat, K_PrimitiveTopology, K_IndexFormat,
  K_FrontFace, K_CullMode, K_CompareFunction, K_StencilOperation,
  K_BlendOperation, K_BlendFactor, K_LoadOp, K_StoreOp, K_AddressMode,
  K_FilterMode, K_MipmapFilterMode, K_TextureDimension,
  K_TextureViewDimension, K_TextureAspect, K_BufferBindingType,
  K_SamplerBindingType, K_TextureSampleType, K_StorageTextureAccess,
  K_VertexStepMode, K_CompositeAlphaMode
};

static std::vector<unsigned char> batch;
static std::vector<size_t> open_objs; // the counts of the keys to patch

static void put8 (unsigned v) { batch.push_back ((unsigned char) v); }
static void put32 (uint32_t v) {
  for (int i= 0; i < 4; i++) batch.push_back ((v >> (8 * i)) & 255); }
static void patch32 (size_t at, uint32_t v) {
  for (int i= 0; i < 4; i++) batch[at + i]= (v >> (8 * i)) & 255; }

static void v_null () { put8 (0); }
static void v_bool (bool b) { put8 (b ? 2 : 1); }
static void v_u32 (uint32_t v) { put8 (3); put32 (v); }
static void v_f64 (double d) {
  put8 (4);
  uint64_t u;
  memcpy (&u, &d, 8);
  for (int i= 0; i < 8; i++) batch.push_back ((u >> (8 * i)) & 255);
}
static void v_num (uint64_t v) {
  if (v <= 0xffffffffu) v_u32 ((uint32_t) v); else v_f64 ((double) v); }
static void v_str (const char* s, size_t n) {
  put8 (5); put32 ((uint32_t) n); batch.insert (batch.end (), s, s + n); }
static void v_str (const char* s) { v_str (s, strlen (s)); }
static void v_view (WGPUStringView sv) {
  if (sv.data == NULL) { v_null (); return; }
  v_str (sv.data, sv.length == WGPU_STRLEN ? strlen (sv.data) : sv.length);
}
static void v_bytes (const void* p, size_t n) {
  put8 (6); put32 ((uint32_t) n);
  batch.insert (batch.end (), (const unsigned char*) p, (const unsigned char*) p + n);
}
static void v_arr (uint32_t n) { put8 (7); put32 (n); }
static void v_obj () { put8 (8); open_objs.push_back (batch.size ()); put32 (0); }
// (the count of the keys of an object is patched by each key)
static void v_end () { open_objs.pop_back (); }
static void key (const char* k) {
  size_t at= open_objs.back ();
  uint32_t n= (uint32_t) batch[at] | ((uint32_t) batch[at+1] << 8) |
              ((uint32_t) batch[at+2] << 16) | ((uint32_t) batch[at+3] << 24);
  patch32 (at, n + 1);
  size_t l= strlen (k);
  batch.push_back (l & 255); batch.push_back ((l >> 8) & 255);
  batch.insert (batch.end (), k, k + l);
}
static void v_enum (wire_kind k, uint32_t v) { put8 (10); put8 (k); put32 (v); }

// the keys with values, those which are undefined left out (the default of
// JavaScript)
static void k_u32 (const char* k, uint32_t v) { key (k); v_u32 (v); }
static void k_num (const char* k, uint64_t v) { key (k); v_num (v); }
static void k_f64 (const char* k, double v) { key (k); v_f64 (v); }
static void k_bool (const char* k, bool b) { key (k); v_bool (b); }
static void k_enum (const char* k, wire_kind kind, uint32_t v) {
  if (v != 0) { key (k); v_enum (kind, v); } }
static void k_label (WGPUStringView sv) {
  if (sv.data != NULL) { key ("label"); v_view (sv); } }

/******************************************************************************
* The objects
******************************************************************************/

struct wire_object {
  uint32_t id;
  uint32_t refs;
};

struct WGPUInstanceImpl { wire_object o; };
struct WGPUAdapterImpl { wire_object o; };
struct WGPUDeviceImpl { wire_object o; WGPUQueue queue; };
struct WGPUQueueImpl { wire_object o; };
struct WGPUSurfaceImpl {
  wire_object o; uint32_t w, h; WGPUTextureFormat format; WGPUTextureUsage usage; };
struct WGPUBufferImpl {
  wire_object o; uint64_t size; WGPUBufferUsage usage; void* mapped; };
struct WGPUTextureImpl {
  wire_object o; uint32_t w, h, layers, mips, samples;
  WGPUTextureFormat format; WGPUTextureDimension dim; WGPUTextureUsage usage; };
struct WGPUTextureViewImpl { wire_object o; };
struct WGPUSamplerImpl { wire_object o; };
struct WGPUShaderModuleImpl { wire_object o; };
struct WGPUBindGroupLayoutImpl { wire_object o; };
struct WGPUBindGroupImpl { wire_object o; };
struct WGPUPipelineLayoutImpl { wire_object o; };
struct WGPURenderPipelineImpl { wire_object o; };
struct WGPUComputePipelineImpl { wire_object o; };
struct WGPUCommandEncoderImpl { wire_object o; };
struct WGPUCommandBufferImpl { wire_object o; };
struct WGPURenderPassEncoderImpl { wire_object o; };
struct WGPUComputePassEncoderImpl { wire_object o; };

static uint32_t next_id= 16; // (below: the objects of the host)
enum { ID_INSTANCE= 1, ID_ADAPTER, ID_DEVICE, ID_QUEUE };

template<typename T> static T*
make (uint32_t id= 0) {
  T* t= (T*) calloc (1, sizeof (T));
  t->o.id= id ? id : next_id++;
  t->o.refs= 1;
  return t;
}

static void v_obj_ref (const wire_object* o) {
  if (o == NULL) { v_null (); return; }
  put8 (9); put32 (o->id);
}
#define REF(x) v_obj_ref ((x) ? &(x)->o : (const wire_object*) NULL)

/******************************************************************************
* The calls
******************************************************************************/

static wgpu_wire_sink the_sink= NULL;

static void
default_sink (const void* data, size_t n) {
  // a message GPU (6) of the stream on the standard output
  unsigned char h[8];
  uint32_t t= 6, len= (uint32_t) n;
  for (int i= 0; i < 4; i++) { h[i]= (t >> (8*i)) & 255; h[4+i]= (len >> (8*i)) & 255; }
  const unsigned char* parts[2]= { h, (const unsigned char*) data };
  size_t sizes[2]= { 8, n };
  for (int k= 0; k < 2; k++) {
    const unsigned char* p= parts[k];
    size_t left= sizes[k];
    while (left > 0) {
      ssize_t w= write (1, p, left);
      if (w < 0) { if (errno == EINTR || errno == EAGAIN) continue; _exit (0); }
      p += w; left -= w;
    }
  }
}

extern "C" void
wgpuWireSetSink (wgpu_wire_sink sink) {
  the_sink= sink;
}

extern "C" void
wgpuWireFlush () {
  if (batch.empty ()) return;
  (the_sink ? the_sink : default_sink) (batch.data (), batch.size ());
  batch.clear ();
}

// a call: [method, target, arguments (n of them, written next), result]
// (the result first written as 0 and patched once known: it is written
// after the arguments)
static void
call (const char* method, const wire_object* target, uint32_t nargs) {
  v_arr (4);
  v_str (method);
  if (target) { put8 (9); put32 (target->id); } else v_null ();
  v_arr (nargs);
}
static void result (const wire_object* r) { v_u32 (r ? r->id : 0); }
static void no_result () { v_u32 (0); }
static void maybe_flush () { if (batch.size () > (1 << 20)) wgpuWireFlush (); }

static void
release (wire_object* o, const char* what) {
  if (o == NULL || --o->refs > 0) return;
  call ("release", NULL, 1);
  v_u32 (o->id);
  no_result ();
  (void) what;
  free (o);
}

#define RELEASE_FN(T, name) \
  extern "C" void wgpu##name##Release (WGPU##name x) { release (x ? &x->o : NULL, #name); } \
  extern "C" void wgpu##name##AddRef (WGPU##name x) { if (x) x->o.refs++; }

RELEASE_FN (Impl, Instance)
RELEASE_FN (Impl, Adapter)
RELEASE_FN (Impl, Device)
RELEASE_FN (Impl, Queue)
RELEASE_FN (Impl, Surface)
RELEASE_FN (Impl, Buffer)
RELEASE_FN (Impl, Texture)
RELEASE_FN (Impl, TextureView)
RELEASE_FN (Impl, Sampler)
RELEASE_FN (Impl, ShaderModule)
RELEASE_FN (Impl, BindGroupLayout)
RELEASE_FN (Impl, BindGroup)
RELEASE_FN (Impl, PipelineLayout)
RELEASE_FN (Impl, RenderPipeline)
RELEASE_FN (Impl, ComputePipeline)
RELEASE_FN (Impl, CommandEncoder)
RELEASE_FN (Impl, CommandBuffer)
RELEASE_FN (Impl, RenderPassEncoder)
RELEASE_FN (Impl, ComputePassEncoder)

/******************************************************************************
* Instance, adapter, device: those of the host
******************************************************************************/

static WGPUInstance the_instance= NULL;
static WGPUAdapter the_adapter= NULL;
static WGPUDevice the_device= NULL;

extern "C" WGPUInstance
wgpuCreateInstance (const WGPUInstanceDescriptor* d) {
  (void) d;
  if (the_instance == NULL) the_instance= make<WGPUInstanceImpl> (ID_INSTANCE);
  else the_instance->o.refs++;
  return the_instance;
}

extern "C" WGPUFuture
wgpuInstanceRequestAdapter (WGPUInstance inst, const WGPURequestAdapterOptions* opt,
                            WGPURequestAdapterCallbackInfo cb) {
  (void) inst; (void) opt;
  if (the_adapter == NULL) the_adapter= make<WGPUAdapterImpl> (ID_ADAPTER);
  else the_adapter->o.refs++;
  if (cb.callback)
    cb.callback (WGPURequestAdapterStatus_Success, the_adapter,
                 WGPUStringView { NULL, WGPU_STRLEN }, cb.userdata1, cb.userdata2);
  return WGPUFuture { 1 };
}

extern "C" WGPUFuture
wgpuAdapterRequestDevice (WGPUAdapter a, const WGPUDeviceDescriptor* d,
                          WGPURequestDeviceCallbackInfo cb) {
  (void) a; (void) d;
  if (the_device == NULL) {
    the_device= make<WGPUDeviceImpl> (ID_DEVICE);
    the_device->queue= make<WGPUQueueImpl> (ID_QUEUE);
  }
  else the_device->o.refs++;
  if (cb.callback)
    cb.callback (WGPURequestDeviceStatus_Success, the_device,
                 WGPUStringView { NULL, WGPU_STRLEN }, cb.userdata1, cb.userdata2);
  return WGPUFuture { 2 };
}

extern "C" void wgpuInstanceProcessEvents (WGPUInstance i) { (void) i; }

extern "C" WGPUWaitStatus
wgpuInstanceWaitAny (WGPUInstance i, size_t n, WGPUFutureWaitInfo* f, uint64_t t) {
  (void) i; (void) t;
  for (size_t k= 0; k < n; k++) f[k].completed= 1;
  return WGPUWaitStatus_Success;
}

extern "C" WGPUQueue
wgpuDeviceGetQueue (WGPUDevice d) {
  d->queue->o.refs++;
  return d->queue;
}

static const char* adapter_name= "WebGPU of the host (webgpu_wire)";

extern "C" WGPUStatus
wgpuAdapterGetInfo (WGPUAdapter a, WGPUAdapterInfo* info) {
  (void) a;
  memset (info, 0, sizeof (*info));
  info->vendor= WGPUStringView { "", 0 };
  info->architecture= WGPUStringView { "", 0 };
  info->device= WGPUStringView { adapter_name, strlen (adapter_name) };
  info->description= WGPUStringView { adapter_name, strlen (adapter_name) };
  info->backendType= WGPUBackendType_WebGPU;
  info->adapterType= WGPUAdapterType_Unknown;
  return WGPUStatus_Success;
}

extern "C" void wgpuAdapterInfoFreeMembers (WGPUAdapterInfo info) { (void) info; }

// the limits which WebGPU guarantees (those of the host may be higher)
static void
default_limits (WGPULimits* l) {
  WGPUChainedStruct* next= l->nextInChain;
  memset (l, 0, sizeof (*l));
  l->nextInChain= next;
  l->maxTextureDimension1D= 8192;
  l->maxTextureDimension2D= 8192;
  l->maxTextureDimension3D= 2048;
  l->maxTextureArrayLayers= 256;
  l->maxBindGroups= 4;
  l->maxBindGroupsPlusVertexBuffers= 24;
  l->maxBindingsPerBindGroup= 1000;
  l->maxDynamicUniformBuffersPerPipelineLayout= 8;
  l->maxDynamicStorageBuffersPerPipelineLayout= 4;
  l->maxSampledTexturesPerShaderStage= 16;
  l->maxSamplersPerShaderStage= 16;
  l->maxStorageBuffersPerShaderStage= 8;
  l->maxStorageTexturesPerShaderStage= 4;
  l->maxUniformBuffersPerShaderStage= 12;
  l->maxUniformBufferBindingSize= 65536;
  l->maxStorageBufferBindingSize= 134217728;
  l->minUniformBufferOffsetAlignment= 256;
  l->minStorageBufferOffsetAlignment= 256;
  l->maxVertexBuffers= 8;
  l->maxBufferSize= 268435456;
  l->maxVertexAttributes= 16;
  l->maxVertexBufferArrayStride= 2048;
  l->maxInterStageShaderVariables= 16;
  l->maxColorAttachments= 8;
  l->maxColorAttachmentBytesPerSample= 32;
  l->maxComputeWorkgroupStorageSize= 16384;
  l->maxComputeInvocationsPerWorkgroup= 256;
  l->maxComputeWorkgroupSizeX= 256;
  l->maxComputeWorkgroupSizeY= 256;
  l->maxComputeWorkgroupSizeZ= 64;
  l->maxComputeWorkgroupsPerDimension= 65535;
}

extern "C" WGPUStatus
wgpuAdapterGetLimits (WGPUAdapter a, WGPULimits* l) {
  (void) a; default_limits (l); return WGPUStatus_Success; }
extern "C" WGPUStatus
wgpuDeviceGetLimits (WGPUDevice d, WGPULimits* l) {
  (void) d; default_limits (l); return WGPUStatus_Success; }

/******************************************************************************
* Surfaces: the canvas of the host
******************************************************************************/

extern "C" WGPUSurface
wgpuInstanceCreateSurface (WGPUInstance i, const WGPUSurfaceDescriptor* d) {
  (void) d;
  WGPUSurface s= make<WGPUSurfaceImpl> ();
  call ("createSurface", &i->o, 0);
  result (&s->o);
  return s;
}

extern "C" WGPUStatus
wgpuSurfaceGetCapabilities (WGPUSurface s, WGPUAdapter a, WGPUSurfaceCapabilities* c) {
  (void) s; (void) a;
  static WGPUTextureFormat formats[]= { WGPUTextureFormat_BGRA8Unorm,
                                        WGPUTextureFormat_RGBA8Unorm };
  static WGPUPresentMode modes[]= { WGPUPresentMode_Fifo };
  static WGPUCompositeAlphaMode alphas[]= { WGPUCompositeAlphaMode_Opaque,
                                            WGPUCompositeAlphaMode_Premultiplied };
  WGPUChainedStruct* next= c->nextInChain;
  memset (c, 0, sizeof (*c));
  c->nextInChain= next;
  c->usages= WGPUTextureUsage_RenderAttachment | WGPUTextureUsage_CopySrc |
             WGPUTextureUsage_CopyDst | WGPUTextureUsage_TextureBinding;
  c->formatCount= 2; c->formats= formats;
  c->presentModeCount= 1; c->presentModes= modes;
  c->alphaModeCount= 2; c->alphaModes= alphas;
  return WGPUStatus_Success;
}

extern "C" void
wgpuSurfaceCapabilitiesFreeMembers (WGPUSurfaceCapabilities c) { (void) c; }

extern "C" void
wgpuSurfaceConfigure (WGPUSurface s, const WGPUSurfaceConfiguration* c) {
  s->w= c->width; s->h= c->height;
  s->format= c->format; s->usage= c->usage;
  call ("configure", &s->o, 1);
  v_obj ();
  key ("device"); REF (c->device);
  k_enum ("format", K_TextureFormat, c->format);
  if (c->usage) k_num ("usage", c->usage);
  k_enum ("alphaMode", K_CompositeAlphaMode, c->alphaMode);
  if (c->viewFormatCount > 0) {
    key ("viewFormats");
    v_arr ((uint32_t) c->viewFormatCount);
    for (size_t i= 0; i < c->viewFormatCount; i++)
      v_enum (K_TextureFormat, c->viewFormats[i]);
  }
  // (the size of a canvas is its own: the host sizes it)
  k_u32 ("width", c->width); k_u32 ("height", c->height);
  v_end ();
  no_result ();
}

extern "C" void
wgpuSurfaceUnconfigure (WGPUSurface s) {
  call ("unconfigure", &s->o, 0);
  no_result ();
}

extern "C" void
wgpuSurfaceGetCurrentTexture (WGPUSurface s, WGPUSurfaceTexture* st) {
  WGPUTexture t= make<WGPUTextureImpl> ();
  t->w= s->w; t->h= s->h; t->layers= 1; t->mips= 1; t->samples= 1;
  t->format= s->format; t->dim= WGPUTextureDimension_2D; t->usage= s->usage;
  call ("getCurrentTexture", &s->o, 0);
  result (&t->o);
  st->texture= t;
  st->status= WGPUSurfaceGetCurrentTextureStatus_SuccessOptimal;
}

extern "C" WGPUStatus
wgpuSurfacePresent (WGPUSurface s) {
  call ("present", &s->o, 0);
  no_result ();
  wgpuWireFlush ();
  return WGPUStatus_Success;
}

/******************************************************************************
* Resources
******************************************************************************/

extern "C" WGPUBuffer
wgpuDeviceCreateBuffer (WGPUDevice d, const WGPUBufferDescriptor* desc) {
  WGPUBuffer b= make<WGPUBufferImpl> ();
  b->size= desc->size; b->usage= desc->usage;
  call ("createBuffer", &d->o, 1);
  v_obj ();
  k_label (desc->label);
  k_num ("size", desc->size);
  k_num ("usage", desc->usage);
  if (desc->mappedAtCreation) k_bool ("mappedAtCreation", true);
  v_end ();
  result (&b->o);
  return b;
}

extern "C" uint64_t wgpuBufferGetSize (WGPUBuffer b) { return b->size; }
extern "C" WGPUBufferUsage wgpuBufferGetUsage (WGPUBuffer b) { return b->usage; }

extern "C" void
wgpuBufferDestroy (WGPUBuffer b) {
  call ("destroy", &b->o, 0);
  no_result ();
}

extern "C" WGPUTexture
wgpuDeviceCreateTexture (WGPUDevice d, const WGPUTextureDescriptor* desc) {
  WGPUTexture t= make<WGPUTextureImpl> ();
  t->w= desc->size.width; t->h= desc->size.height;
  t->layers= desc->size.depthOrArrayLayers;
  t->mips= desc->mipLevelCount; t->samples= desc->sampleCount;
  t->format= desc->format; t->dim= desc->dimension; t->usage= desc->usage;
  call ("createTexture", &d->o, 1);
  v_obj ();
  k_label (desc->label);
  key ("size"); v_arr (3);
  v_u32 (desc->size.width); v_u32 (desc->size.height);
  v_u32 (desc->size.depthOrArrayLayers ? desc->size.depthOrArrayLayers : 1);
  k_enum ("format", K_TextureFormat, desc->format);
  k_num ("usage", desc->usage);
  k_enum ("dimension", K_TextureDimension, desc->dimension);
  if (desc->mipLevelCount > 1) k_u32 ("mipLevelCount", desc->mipLevelCount);
  if (desc->sampleCount > 1) k_u32 ("sampleCount", desc->sampleCount);
  if (desc->viewFormatCount > 0) {
    key ("viewFormats");
    v_arr ((uint32_t) desc->viewFormatCount);
    for (size_t i= 0; i < desc->viewFormatCount; i++)
      v_enum (K_TextureFormat, desc->viewFormats[i]);
  }
  v_end ();
  result (&t->o);
  return t;
}

extern "C" uint32_t wgpuTextureGetWidth (WGPUTexture t) { return t->w; }
extern "C" uint32_t wgpuTextureGetHeight (WGPUTexture t) { return t->h; }
extern "C" uint32_t wgpuTextureGetDepthOrArrayLayers (WGPUTexture t) { return t->layers; }
extern "C" uint32_t wgpuTextureGetMipLevelCount (WGPUTexture t) { return t->mips; }
extern "C" uint32_t wgpuTextureGetSampleCount (WGPUTexture t) { return t->samples; }
extern "C" WGPUTextureFormat wgpuTextureGetFormat (WGPUTexture t) { return t->format; }
extern "C" WGPUTextureDimension wgpuTextureGetDimension (WGPUTexture t) { return t->dim; }
extern "C" WGPUTextureUsage wgpuTextureGetUsage (WGPUTexture t) { return t->usage; }

extern "C" void
wgpuTextureDestroy (WGPUTexture t) {
  call ("destroy", &t->o, 0);
  no_result ();
}

extern "C" WGPUTextureView
wgpuTextureCreateView (WGPUTexture t, const WGPUTextureViewDescriptor* desc) {
  WGPUTextureView v= make<WGPUTextureViewImpl> ();
  call ("createView", &t->o, desc ? 1 : 0);
  if (desc) {
    v_obj ();
    k_label (desc->label);
    k_enum ("format", K_TextureFormat, desc->format);
    k_enum ("dimension", K_TextureViewDimension, desc->dimension);
    if (desc->baseMipLevel) k_u32 ("baseMipLevel", desc->baseMipLevel);
    if (desc->mipLevelCount != WGPU_MIP_LEVEL_COUNT_UNDEFINED)
      k_u32 ("mipLevelCount", desc->mipLevelCount);
    if (desc->baseArrayLayer) k_u32 ("baseArrayLayer", desc->baseArrayLayer);
    if (desc->arrayLayerCount != WGPU_ARRAY_LAYER_COUNT_UNDEFINED)
      k_u32 ("arrayLayerCount", desc->arrayLayerCount);
    k_enum ("aspect", K_TextureAspect, desc->aspect);
    if (desc->usage) k_num ("usage", desc->usage);
    v_end ();
  }
  result (&v->o);
  return v;
}

extern "C" WGPUSampler
wgpuDeviceCreateSampler (WGPUDevice d, const WGPUSamplerDescriptor* desc) {
  WGPUSampler s= make<WGPUSamplerImpl> ();
  call ("createSampler", &d->o, desc ? 1 : 0);
  if (desc) {
    v_obj ();
    k_label (desc->label);
    k_enum ("addressModeU", K_AddressMode, desc->addressModeU);
    k_enum ("addressModeV", K_AddressMode, desc->addressModeV);
    k_enum ("addressModeW", K_AddressMode, desc->addressModeW);
    k_enum ("magFilter", K_FilterMode, desc->magFilter);
    k_enum ("minFilter", K_FilterMode, desc->minFilter);
    k_enum ("mipmapFilter", K_MipmapFilterMode, desc->mipmapFilter);
    k_f64 ("lodMinClamp", desc->lodMinClamp);
    k_f64 ("lodMaxClamp", desc->lodMaxClamp);
    k_enum ("compare", K_CompareFunction, desc->compare);
    if (desc->maxAnisotropy > 1) k_u32 ("maxAnisotropy", desc->maxAnisotropy);
    v_end ();
  }
  result (&s->o);
  return s;
}

extern "C" WGPUShaderModule
wgpuDeviceCreateShaderModule (WGPUDevice d, const WGPUShaderModuleDescriptor* desc) {
  WGPUShaderModule m= make<WGPUShaderModuleImpl> ();
  call ("createShaderModule", &d->o, 1);
  v_obj ();
  k_label (desc->label);
  for (const WGPUChainedStruct* c= desc->nextInChain; c; c= c->next)
    if (c->sType == WGPUSType_ShaderSourceWGSL) {
      key ("code");
      v_view (((const WGPUShaderSourceWGSL*) c)->code);
    }
  v_end ();
  result (&m->o);
  return m;
}

/******************************************************************************
* Bind groups and pipelines
******************************************************************************/

extern "C" WGPUBindGroupLayout
wgpuDeviceCreateBindGroupLayout (WGPUDevice d, const WGPUBindGroupLayoutDescriptor* desc) {
  WGPUBindGroupLayout l= make<WGPUBindGroupLayoutImpl> ();
  call ("createBindGroupLayout", &d->o, 1);
  v_obj ();
  k_label (desc->label);
  key ("entries");
  v_arr ((uint32_t) desc->entryCount);
  for (size_t i= 0; i < desc->entryCount; i++) {
    const WGPUBindGroupLayoutEntry& e= desc->entries[i];
    v_obj ();
    k_u32 ("binding", e.binding);
    k_num ("visibility", e.visibility);
    if (e.buffer.type != WGPUBufferBindingType_BindingNotUsed) {
      key ("buffer"); v_obj ();
      if (e.buffer.type != WGPUBufferBindingType_Undefined)
        k_enum ("type", K_BufferBindingType, e.buffer.type);
      if (e.buffer.hasDynamicOffset) k_bool ("hasDynamicOffset", true);
      if (e.buffer.minBindingSize) k_num ("minBindingSize", e.buffer.minBindingSize);
      v_end ();
    }
    if (e.sampler.type != WGPUSamplerBindingType_BindingNotUsed) {
      key ("sampler"); v_obj ();
      if (e.sampler.type != WGPUSamplerBindingType_Undefined)
        k_enum ("type", K_SamplerBindingType, e.sampler.type);
      v_end ();
    }
    if (e.texture.sampleType != WGPUTextureSampleType_BindingNotUsed) {
      key ("texture"); v_obj ();
      if (e.texture.sampleType != WGPUTextureSampleType_Undefined)
        k_enum ("sampleType", K_TextureSampleType, e.texture.sampleType);
      k_enum ("viewDimension", K_TextureViewDimension, e.texture.viewDimension);
      if (e.texture.multisampled) k_bool ("multisampled", true);
      v_end ();
    }
    if (e.storageTexture.access != WGPUStorageTextureAccess_BindingNotUsed) {
      key ("storageTexture"); v_obj ();
      if (e.storageTexture.access != WGPUStorageTextureAccess_Undefined)
        k_enum ("access", K_StorageTextureAccess, e.storageTexture.access);
      k_enum ("format", K_TextureFormat, e.storageTexture.format);
      k_enum ("viewDimension", K_TextureViewDimension, e.storageTexture.viewDimension);
      v_end ();
    }
    v_end ();
  }
  v_end ();
  result (&l->o);
  return l;
}

extern "C" WGPUBindGroup
wgpuDeviceCreateBindGroup (WGPUDevice d, const WGPUBindGroupDescriptor* desc) {
  WGPUBindGroup g= make<WGPUBindGroupImpl> ();
  call ("createBindGroup", &d->o, 1);
  v_obj ();
  k_label (desc->label);
  key ("layout"); REF (desc->layout);
  key ("entries");
  v_arr ((uint32_t) desc->entryCount);
  for (size_t i= 0; i < desc->entryCount; i++) {
    const WGPUBindGroupEntry& e= desc->entries[i];
    v_obj ();
    k_u32 ("binding", e.binding);
    key ("resource");
    if (e.buffer) {
      v_obj ();
      key ("buffer"); REF (e.buffer);
      if (e.offset) k_num ("offset", e.offset);
      if (e.size != WGPU_WHOLE_SIZE) k_num ("size", e.size);
      v_end ();
    }
    else if (e.sampler) REF (e.sampler);
    else REF (e.textureView);
    v_end ();
  }
  v_end ();
  result (&g->o);
  return g;
}

extern "C" WGPUPipelineLayout
wgpuDeviceCreatePipelineLayout (WGPUDevice d, const WGPUPipelineLayoutDescriptor* desc) {
  WGPUPipelineLayout l= make<WGPUPipelineLayoutImpl> ();
  call ("createPipelineLayout", &d->o, 1);
  v_obj ();
  k_label (desc->label);
  key ("bindGroupLayouts");
  v_arr ((uint32_t) desc->bindGroupLayoutCount);
  for (size_t i= 0; i < desc->bindGroupLayoutCount; i++)
    REF (desc->bindGroupLayouts[i]);
  v_end ();
  result (&l->o);
  return l;
}

static void
constants (size_t n, const WGPUConstantEntry* c) {
  if (n == 0) return;
  key ("constants");
  v_obj ();
  for (size_t i= 0; i < n; i++) {
    WGPUStringView k= c[i].key;
    size_t len= (k.length == WGPU_STRLEN) ? strlen (k.data) : k.length;
    char buf[256];
    if (len >= sizeof (buf)) len= sizeof (buf) - 1;
    memcpy (buf, k.data, len); buf[len]= 0;
    k_f64 (buf, c[i].value);
  }
  v_end ();
}

static void
entry_point (WGPUStringView e) {
  if (e.data != NULL) { key ("entryPoint"); v_view (e); }
}

static void
stencil_face (const char* k, const WGPUStencilFaceState& s) {
  key (k);
  v_obj ();
  k_enum ("compare", K_CompareFunction, s.compare);
  k_enum ("failOp", K_StencilOperation, s.failOp);
  k_enum ("depthFailOp", K_StencilOperation, s.depthFailOp);
  k_enum ("passOp", K_StencilOperation, s.passOp);
  v_end ();
}

static void
blend_component (const char* k, const WGPUBlendComponent& b) {
  key (k);
  v_obj ();
  k_enum ("operation", K_BlendOperation, b.operation);
  k_enum ("srcFactor", K_BlendFactor, b.srcFactor);
  k_enum ("dstFactor", K_BlendFactor, b.dstFactor);
  v_end ();
}

extern "C" WGPURenderPipeline
wgpuDeviceCreateRenderPipeline (WGPUDevice d, const WGPURenderPipelineDescriptor* desc) {
  WGPURenderPipeline p= make<WGPURenderPipelineImpl> ();
  call ("createRenderPipeline", &d->o, 1);
  v_obj ();
  k_label (desc->label);
  key ("layout");
  if (desc->layout) REF (desc->layout); else v_str ("auto");
  // vertex
  key ("vertex");
  v_obj ();
  key ("module"); REF (desc->vertex.module);
  entry_point (desc->vertex.entryPoint);
  constants (desc->vertex.constantCount, desc->vertex.constants);
  key ("buffers");
  v_arr ((uint32_t) desc->vertex.bufferCount);
  for (size_t i= 0; i < desc->vertex.bufferCount; i++) {
    const WGPUVertexBufferLayout& b= desc->vertex.buffers[i];
    if (b.stepMode == WGPUVertexStepMode_Undefined && b.attributeCount == 0) {
      v_null (); continue; }
    v_obj ();
    k_num ("arrayStride", b.arrayStride);
    k_enum ("stepMode", K_VertexStepMode, b.stepMode);
    key ("attributes");
    v_arr ((uint32_t) b.attributeCount);
    for (size_t j= 0; j < b.attributeCount; j++) {
      const WGPUVertexAttribute& a= b.attributes[j];
      v_obj ();
      k_enum ("format", K_VertexFormat, a.format);
      k_num ("offset", a.offset);
      k_u32 ("shaderLocation", a.shaderLocation);
      v_end ();
    }
    v_end ();
  }
  v_end ();
  // primitive
  key ("primitive");
  v_obj ();
  k_enum ("topology", K_PrimitiveTopology, desc->primitive.topology);
  k_enum ("stripIndexFormat", K_IndexFormat, desc->primitive.stripIndexFormat);
  k_enum ("frontFace", K_FrontFace, desc->primitive.frontFace);
  k_enum ("cullMode", K_CullMode, desc->primitive.cullMode);
  if (desc->primitive.unclippedDepth) k_bool ("unclippedDepth", true);
  v_end ();
  // depth and stencil
  if (desc->depthStencil) {
    const WGPUDepthStencilState& s= *desc->depthStencil;
    key ("depthStencil");
    v_obj ();
    k_enum ("format", K_TextureFormat, s.format);
    if (s.depthWriteEnabled != WGPUOptionalBool_Undefined)
      k_bool ("depthWriteEnabled", s.depthWriteEnabled == WGPUOptionalBool_True);
    k_enum ("depthCompare", K_CompareFunction, s.depthCompare);
    stencil_face ("stencilFront", s.stencilFront);
    stencil_face ("stencilBack", s.stencilBack);
    k_num ("stencilReadMask", s.stencilReadMask);
    k_num ("stencilWriteMask", s.stencilWriteMask);
    if (s.depthBias) { key ("depthBias"); v_f64 (s.depthBias); }
    if (s.depthBiasSlopeScale != 0) k_f64 ("depthBiasSlopeScale", s.depthBiasSlopeScale);
    if (s.depthBiasClamp != 0) k_f64 ("depthBiasClamp", s.depthBiasClamp);
    v_end ();
  }
  // multisample
  key ("multisample");
  v_obj ();
  if (desc->multisample.count) k_u32 ("count", desc->multisample.count);
  k_num ("mask", desc->multisample.mask);
  if (desc->multisample.alphaToCoverageEnabled) k_bool ("alphaToCoverageEnabled", true);
  v_end ();
  // fragment
  if (desc->fragment) {
    const WGPUFragmentState& f= *desc->fragment;
    key ("fragment");
    v_obj ();
    key ("module"); REF (f.module);
    entry_point (f.entryPoint);
    constants (f.constantCount, f.constants);
    key ("targets");
    v_arr ((uint32_t) f.targetCount);
    for (size_t i= 0; i < f.targetCount; i++) {
      const WGPUColorTargetState& t= f.targets[i];
      if (t.format == WGPUTextureFormat_Undefined) { v_null (); continue; }
      v_obj ();
      k_enum ("format", K_TextureFormat, t.format);
      if (t.blend) {
        key ("blend");
        v_obj ();
        blend_component ("color", t.blend->color);
        blend_component ("alpha", t.blend->alpha);
        v_end ();
      }
      k_num ("writeMask", t.writeMask);
      v_end ();
    }
    v_end ();
  }
  v_end ();
  result (&p->o);
  return p;
}

extern "C" WGPUComputePipeline
wgpuDeviceCreateComputePipeline (WGPUDevice d, const WGPUComputePipelineDescriptor* desc) {
  WGPUComputePipeline p= make<WGPUComputePipelineImpl> ();
  call ("createComputePipeline", &d->o, 1);
  v_obj ();
  k_label (desc->label);
  key ("layout");
  if (desc->layout) REF (desc->layout); else v_str ("auto");
  key ("compute");
  v_obj ();
  key ("module"); REF (desc->compute.module);
  entry_point (desc->compute.entryPoint);
  constants (desc->compute.constantCount, desc->compute.constants);
  v_end ();
  v_end ();
  result (&p->o);
  return p;
}

/******************************************************************************
* Commands
******************************************************************************/

extern "C" WGPUCommandEncoder
wgpuDeviceCreateCommandEncoder (WGPUDevice d, const WGPUCommandEncoderDescriptor* desc) {
  WGPUCommandEncoder e= make<WGPUCommandEncoderImpl> ();
  call ("createCommandEncoder", &d->o, desc && desc->label.data ? 1 : 0);
  if (desc && desc->label.data) { v_obj (); k_label (desc->label); v_end (); }
  result (&e->o);
  return e;
}

extern "C" WGPURenderPassEncoder
wgpuCommandEncoderBeginRenderPass (WGPUCommandEncoder e, const WGPURenderPassDescriptor* desc) {
  WGPURenderPassEncoder p= make<WGPURenderPassEncoderImpl> ();
  call ("beginRenderPass", &e->o, 1);
  v_obj ();
  k_label (desc->label);
  key ("colorAttachments");
  v_arr ((uint32_t) desc->colorAttachmentCount);
  for (size_t i= 0; i < desc->colorAttachmentCount; i++) {
    const WGPURenderPassColorAttachment& a= desc->colorAttachments[i];
    if (a.view == NULL) { v_null (); continue; }
    v_obj ();
    key ("view"); REF (a.view);
    if (a.depthSlice != WGPU_DEPTH_SLICE_UNDEFINED) k_u32 ("depthSlice", a.depthSlice);
    if (a.resolveTarget) { key ("resolveTarget"); REF (a.resolveTarget); }
    k_enum ("loadOp", K_LoadOp, a.loadOp);
    k_enum ("storeOp", K_StoreOp, a.storeOp);
    key ("clearValue"); v_arr (4);
    v_f64 (a.clearValue.r); v_f64 (a.clearValue.g);
    v_f64 (a.clearValue.b); v_f64 (a.clearValue.a);
    v_end ();
  }
  if (desc->depthStencilAttachment) {
    const WGPURenderPassDepthStencilAttachment& a= *desc->depthStencilAttachment;
    key ("depthStencilAttachment");
    v_obj ();
    key ("view"); REF (a.view);
    k_enum ("depthLoadOp", K_LoadOp, a.depthLoadOp);
    k_enum ("depthStoreOp", K_StoreOp, a.depthStoreOp);
    if (a.depthLoadOp == WGPULoadOp_Clear) k_f64 ("depthClearValue", a.depthClearValue);
    if (a.depthReadOnly) k_bool ("depthReadOnly", true);
    k_enum ("stencilLoadOp", K_LoadOp, a.stencilLoadOp);
    k_enum ("stencilStoreOp", K_StoreOp, a.stencilStoreOp);
    if (a.stencilLoadOp == WGPULoadOp_Clear) k_u32 ("stencilClearValue", a.stencilClearValue);
    if (a.stencilReadOnly) k_bool ("stencilReadOnly", true);
    v_end ();
  }
  v_end ();
  result (&p->o);
  return p;
}

extern "C" WGPUComputePassEncoder
wgpuCommandEncoderBeginComputePass (WGPUCommandEncoder e, const WGPUComputePassDescriptor* desc) {
  WGPUComputePassEncoder p= make<WGPUComputePassEncoderImpl> ();
  call ("beginComputePass", &e->o, desc && desc->label.data ? 1 : 0);
  if (desc && desc->label.data) { v_obj (); k_label (desc->label); v_end (); }
  result (&p->o);
  return p;
}

static void
copy_texture (const WGPUTexelCopyTextureInfo* t) {
  v_obj ();
  key ("texture"); REF (t->texture);
  if (t->mipLevel) k_u32 ("mipLevel", t->mipLevel);
  key ("origin"); v_arr (3);
  v_u32 (t->origin.x); v_u32 (t->origin.y); v_u32 (t->origin.z);
  k_enum ("aspect", K_TextureAspect, t->aspect);
  v_end ();
}

static void
extent (const WGPUExtent3D* e) {
  v_arr (3);
  v_u32 (e->width); v_u32 (e->height);
  v_u32 (e->depthOrArrayLayers ? e->depthOrArrayLayers : 1);
}

static void
buffer_layout (const WGPUTexelCopyBufferLayout* l, bool with_offset) {
  v_obj ();
  if (with_offset && l->offset) k_num ("offset", l->offset);
  if (l->bytesPerRow != WGPU_COPY_STRIDE_UNDEFINED) k_u32 ("bytesPerRow", l->bytesPerRow);
  if (l->rowsPerImage != WGPU_COPY_STRIDE_UNDEFINED) k_u32 ("rowsPerImage", l->rowsPerImage);
  v_end ();
}

extern "C" void
wgpuCommandEncoderCopyTextureToTexture (WGPUCommandEncoder e, const WGPUTexelCopyTextureInfo* s,
                                        const WGPUTexelCopyTextureInfo* d, const WGPUExtent3D* size) {
  call ("copyTextureToTexture", &e->o, 3);
  copy_texture (s); copy_texture (d); extent (size);
  no_result ();
}

extern "C" void
wgpuCommandEncoderCopyBufferToBuffer (WGPUCommandEncoder e, WGPUBuffer s, uint64_t so,
                                      WGPUBuffer d, uint64_t dof, uint64_t size) {
  call ("copyBufferToBuffer", &e->o, 5);
  REF (s); v_num (so); REF (d); v_num (dof); v_num (size);
  no_result ();
}

extern "C" WGPUCommandBuffer
wgpuCommandEncoderFinish (WGPUCommandEncoder e, const WGPUCommandBufferDescriptor* desc) {
  (void) desc;
  WGPUCommandBuffer b= make<WGPUCommandBufferImpl> ();
  call ("finish", &e->o, 0);
  result (&b->o);
  return b;
}

// render passes

extern "C" void
wgpuRenderPassEncoderSetPipeline (WGPURenderPassEncoder p, WGPURenderPipeline pl) {
  call ("setPipeline", &p->o, 1); REF (pl); no_result ();
}

extern "C" void
wgpuRenderPassEncoderSetBindGroup (WGPURenderPassEncoder p, uint32_t i, WGPUBindGroup g,
                                   size_t n, const uint32_t* offsets) {
  call ("setBindGroup", &p->o, n ? 3 : 2);
  v_u32 (i); REF (g);
  if (n) { v_arr ((uint32_t) n); for (size_t k= 0; k < n; k++) v_u32 (offsets[k]); }
  no_result ();
}

extern "C" void
wgpuRenderPassEncoderSetVertexBuffer (WGPURenderPassEncoder p, uint32_t slot, WGPUBuffer b,
                                      uint64_t offset, uint64_t size) {
  bool whole= (size == WGPU_WHOLE_SIZE);
  call ("setVertexBuffer", &p->o, whole ? 3 : 4);
  v_u32 (slot); REF (b); v_num (offset);
  if (!whole) v_num (size);
  no_result ();
}

extern "C" void
wgpuRenderPassEncoderSetIndexBuffer (WGPURenderPassEncoder p, WGPUBuffer b, WGPUIndexFormat f,
                                     uint64_t offset, uint64_t size) {
  bool whole= (size == WGPU_WHOLE_SIZE);
  call ("setIndexBuffer", &p->o, whole ? 3 : 4);
  REF (b); v_enum (K_IndexFormat, f); v_num (offset);
  if (!whole) v_num (size);
  no_result ();
}

extern "C" void
wgpuRenderPassEncoderSetScissorRect (WGPURenderPassEncoder p, uint32_t x, uint32_t y,
                                     uint32_t w, uint32_t h) {
  call ("setScissorRect", &p->o, 4);
  v_u32 (x); v_u32 (y); v_u32 (w); v_u32 (h);
  no_result ();
}

extern "C" void
wgpuRenderPassEncoderSetViewport (WGPURenderPassEncoder p, float x, float y, float w, float h,
                                  float mind, float maxd) {
  call ("setViewport", &p->o, 6);
  v_f64 (x); v_f64 (y); v_f64 (w); v_f64 (h); v_f64 (mind); v_f64 (maxd);
  no_result ();
}

extern "C" void
wgpuRenderPassEncoderSetStencilReference (WGPURenderPassEncoder p, uint32_t r) {
  call ("setStencilReference", &p->o, 1); v_u32 (r); no_result ();
}

extern "C" void
wgpuRenderPassEncoderSetBlendConstant (WGPURenderPassEncoder p, const WGPUColor* c) {
  call ("setBlendConstant", &p->o, 1);
  v_arr (4); v_f64 (c->r); v_f64 (c->g); v_f64 (c->b); v_f64 (c->a);
  no_result ();
}

extern "C" void
wgpuRenderPassEncoderDraw (WGPURenderPassEncoder p, uint32_t vc, uint32_t ic,
                           uint32_t fv, uint32_t fi) {
  call ("draw", &p->o, 4);
  v_u32 (vc); v_u32 (ic); v_u32 (fv); v_u32 (fi);
  no_result ();
}

extern "C" void
wgpuRenderPassEncoderDrawIndexed (WGPURenderPassEncoder p, uint32_t ic, uint32_t inst,
                                  uint32_t fi, int32_t bv, uint32_t finst) {
  call ("drawIndexed", &p->o, 5);
  v_u32 (ic); v_u32 (inst); v_u32 (fi);
  if (bv >= 0) v_u32 ((uint32_t) bv); else v_f64 (bv);
  v_u32 (finst);
  no_result ();
}

extern "C" void
wgpuRenderPassEncoderEnd (WGPURenderPassEncoder p) {
  call ("end", &p->o, 0); no_result ();
}

// compute passes

extern "C" void
wgpuComputePassEncoderSetPipeline (WGPUComputePassEncoder p, WGPUComputePipeline pl) {
  call ("setPipeline", &p->o, 1); REF (pl); no_result ();
}

extern "C" void
wgpuComputePassEncoderSetBindGroup (WGPUComputePassEncoder p, uint32_t i, WGPUBindGroup g,
                                    size_t n, const uint32_t* offsets) {
  call ("setBindGroup", &p->o, n ? 3 : 2);
  v_u32 (i); REF (g);
  if (n) { v_arr ((uint32_t) n); for (size_t k= 0; k < n; k++) v_u32 (offsets[k]); }
  no_result ();
}

extern "C" void
wgpuComputePassEncoderDispatchWorkgroups (WGPUComputePassEncoder p, uint32_t x,
                                          uint32_t y, uint32_t z) {
  call ("dispatchWorkgroups", &p->o, 3);
  v_u32 (x); v_u32 (y); v_u32 (z);
  no_result ();
}

extern "C" void
wgpuComputePassEncoderEnd (WGPUComputePassEncoder p) {
  call ("end", &p->o, 0); no_result ();
}

/******************************************************************************
* The queue
******************************************************************************/

extern "C" void
wgpuQueueSubmit (WGPUQueue q, size_t n, const WGPUCommandBuffer* cmds) {
  call ("submit", &q->o, 1);
  v_arr ((uint32_t) n);
  for (size_t i= 0; i < n; i++) REF (cmds[i]);
  no_result ();
  wgpuWireFlush ();
}

extern "C" void
wgpuQueueWriteBuffer (WGPUQueue q, WGPUBuffer b, uint64_t offset, const void* data, size_t n) {
  call ("writeBuffer", &q->o, 3);
  REF (b); v_num (offset); v_bytes (data, n);
  no_result ();
  maybe_flush ();
}

extern "C" void
wgpuQueueWriteTexture (WGPUQueue q, const WGPUTexelCopyTextureInfo* dst, const void* data,
                       size_t n, const WGPUTexelCopyBufferLayout* layout,
                       const WGPUExtent3D* size) {
  call ("writeTexture", &q->o, 4);
  copy_texture (dst);
  v_bytes (data, n);
  buffer_layout (layout, true);
  extent (size);
  no_result ();
  maybe_flush ();
}

extern "C" WGPUFuture
wgpuQueueOnSubmittedWorkDone (WGPUQueue q, WGPUQueueWorkDoneCallbackInfo cb) {
  (void) q;
  // (the work is the host's: taken as done)
  if (cb.callback)
    cb.callback (WGPUQueueWorkDoneStatus_Success, WGPUStringView { NULL, WGPU_STRLEN },
                 cb.userdata1, cb.userdata2);
  return WGPUFuture { 3 };
}
