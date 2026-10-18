/******************************************************************************
* MODULE     : hello-triangle.c
* DESCRIPTION: The test of the WebGPU of WASIX (webgpu_wire.cpp): a program
*              which draws a turning triangle with webgpu.h, shown by the
*              page of the WASIX build (index.html?program=hello-triangle.wasm)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <poll.h>
#include "webgpu_wire.h"

#define SV(s) ((WGPUStringView) { s, WGPU_STRLEN })

static WGPUAdapter adapter;
static WGPUDevice device;

static void
got_adapter (WGPURequestAdapterStatus st, WGPUAdapter a, WGPUStringView m,
             void* u1, void* u2) {
  (void) st; (void) m; (void) u1; (void) u2; adapter= a;
}

static void
got_device (WGPURequestDeviceStatus st, WGPUDevice d, WGPUStringView m,
            void* u1, void* u2) {
  (void) st; (void) m; (void) u1; (void) u2; device= d;
}

static const char* shader=
  "struct U { angle: f32 };\n"
  "@group(0) @binding(0) var<uniform> u: U;\n"
  "struct V { @builtin(position) pos: vec4f, @location(0) color: vec3f };\n"
  "@vertex fn vs(@builtin(vertex_index) i: u32) -> V {\n"
  "  let a = u.angle + f32(i) * 2.0943951;\n"
  "  var v: V;\n"
  "  v.pos = vec4f(0.7 * cos(a), 0.7 * sin(a), 0.0, 1.0);\n"
  "  v.color = vec3f(f32(i == 0u), f32(i == 1u), f32(i == 2u));\n"
  "  return v;\n"
  "}\n"
  "@fragment fn fs(v: V) -> @location(0) vec4f { return vec4f(v.color, 1.0); }\n";

int
main (int argc, char** argv) {
  int frames= argc > 1 ? atoi (argv[1]) : 120;
  // the start of the stream (vue_stream.cpp): HELLO, "TMWX" and version 1
  unsigned char hello[]= { 1,0,0,0, 8,0,0,0, 'T','M','W','X', 1,0,0,0 };
  write (1, hello, sizeof (hello));

  WGPUInstance inst= wgpuCreateInstance (NULL);
  WGPURequestAdapterCallbackInfo ac= { 0 };
  ac.callback= got_adapter;
  wgpuInstanceRequestAdapter (inst, NULL, ac);
  WGPURequestDeviceCallbackInfo dc= { 0 };
  dc.callback= got_device;
  wgpuAdapterRequestDevice (adapter, NULL, dc);
  WGPUQueue queue= wgpuDeviceGetQueue (device);

  WGPUSurfaceDescriptor sd= { 0 };
  WGPUSurface surface= wgpuInstanceCreateSurface (inst, &sd);
  WGPUSurfaceConfiguration sc= { 0 };
  sc.device= device;
  sc.format= WGPUTextureFormat_BGRA8Unorm;
  sc.usage= WGPUTextureUsage_RenderAttachment;
  sc.width= 640; sc.height= 480;
  sc.alphaMode= WGPUCompositeAlphaMode_Opaque;
  sc.presentMode= WGPUPresentMode_Fifo;
  wgpuSurfaceConfigure (surface, &sc);

  WGPUShaderSourceWGSL wgsl= { 0 };
  wgsl.chain.sType= WGPUSType_ShaderSourceWGSL;
  wgsl.code= SV (shader);
  WGPUShaderModuleDescriptor md= { 0 };
  md.nextInChain= &wgsl.chain;
  md.label= SV ("triangle");
  WGPUShaderModule module= wgpuDeviceCreateShaderModule (device, &md);

  WGPUBufferDescriptor bd= { 0 };
  bd.size= 16;
  bd.usage= WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst;
  WGPUBuffer uniform= wgpuDeviceCreateBuffer (device, &bd);

  WGPUBindGroupLayoutEntry le= { 0 };
  le.binding= 0;
  le.visibility= WGPUShaderStage_Vertex;
  le.buffer.type= WGPUBufferBindingType_Uniform;
  WGPUBindGroupLayoutDescriptor lgd= { 0 };
  lgd.entryCount= 1; lgd.entries= &le;
  WGPUBindGroupLayout bgl= wgpuDeviceCreateBindGroupLayout (device, &lgd);
  WGPUPipelineLayoutDescriptor pld= { 0 };
  pld.bindGroupLayoutCount= 1; pld.bindGroupLayouts= &bgl;
  WGPUPipelineLayout layout= wgpuDeviceCreatePipelineLayout (device, &pld);
  WGPUBindGroupEntry be= { 0 };
  be.binding= 0; be.buffer= uniform; be.size= 16;
  WGPUBindGroupDescriptor bgd= { 0 };
  bgd.layout= bgl; bgd.entryCount= 1; bgd.entries= &be;
  WGPUBindGroup group= wgpuDeviceCreateBindGroup (device, &bgd);

  WGPUColorTargetState target= { 0 };
  target.format= WGPUTextureFormat_BGRA8Unorm;
  target.writeMask= WGPUColorWriteMask_All;
  WGPUFragmentState fs= { 0 };
  fs.module= module; fs.entryPoint= SV ("fs");
  fs.targetCount= 1; fs.targets= &target;
  WGPURenderPipelineDescriptor rpd= { 0 };
  rpd.layout= layout;
  rpd.vertex.module= module; rpd.vertex.entryPoint= SV ("vs");
  rpd.primitive.topology= WGPUPrimitiveTopology_TriangleList;
  rpd.multisample.count= 1; rpd.multisample.mask= 0xffffffff;
  rpd.fragment= &fs;
  WGPURenderPipeline pipeline= wgpuDeviceCreateRenderPipeline (device, &rpd);

  for (int f= 0; f < frames; f++) {
    float angle= f * 0.05f;
    wgpuQueueWriteBuffer (queue, uniform, 0, &angle, sizeof (angle));
    WGPUSurfaceTexture st;
    wgpuSurfaceGetCurrentTexture (surface, &st);
    WGPUTextureView view= wgpuTextureCreateView (st.texture, NULL);
    WGPUCommandEncoder enc= wgpuDeviceCreateCommandEncoder (device, NULL);
    WGPURenderPassColorAttachment ca= { 0 };
    ca.view= view;
    ca.depthSlice= WGPU_DEPTH_SLICE_UNDEFINED;
    ca.loadOp= WGPULoadOp_Clear; ca.storeOp= WGPUStoreOp_Store;
    ca.clearValue= (WGPUColor) { 0.1, 0.1, 0.15 + 0.1 * sin (angle), 1.0 };
    WGPURenderPassDescriptor rp= { 0 };
    rp.colorAttachmentCount= 1; rp.colorAttachments= &ca;
    WGPURenderPassEncoder pass= wgpuCommandEncoderBeginRenderPass (enc, &rp);
    wgpuRenderPassEncoderSetPipeline (pass, pipeline);
    wgpuRenderPassEncoderSetBindGroup (pass, 0, group, 0, NULL);
    wgpuRenderPassEncoderDraw (pass, 3, 1, 0, 0);
    wgpuRenderPassEncoderEnd (pass);
    WGPUCommandBuffer cb= wgpuCommandEncoderFinish (enc, NULL);
    wgpuQueueSubmit (queue, 1, &cb);
    wgpuSurfacePresent (surface);
    wgpuCommandBufferRelease (cb);
    wgpuRenderPassEncoderRelease (pass);
    wgpuCommandEncoderRelease (enc);
    wgpuTextureViewRelease (view);
    wgpuTextureRelease (st.texture);
    usleep (16000);
  }
  // the last frame stays until the page closes the standard input
  char c;
  while (read (0, &c, 1) > 0) ;
  return 0;
}
