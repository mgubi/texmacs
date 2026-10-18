/******************************************************************************
* MODULE     : thorvg-shapes.cpp
* DESCRIPTION: The test of ThorVG's WebGPU engine on the WebGPU of WASIX
*              (webgpu_wire.cpp): shapes drawn into the surface of the page
*              (index.html?program=thorvg-shapes.wasm)
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <thorvg.h>
#include <unistd.h>
#include <stdlib.h>
#include "webgpu_wire.h"

static WGPUAdapter adapter;
static WGPUDevice device;

int
main (int argc, char** argv) {
  int frames= argc > 1 ? atoi (argv[1]) : 60;
  const int W= 640, H= 480;
  unsigned char hello[]= { 1,0,0,0, 8,0,0,0, 'T','M','W','X', 1,0,0,0 };
  if (write (1, hello, sizeof (hello)) < 0) return 1;

  WGPUInstance inst= wgpuCreateInstance (NULL);
  WGPURequestAdapterCallbackInfo ac= {};
  ac.callback= [] (WGPURequestAdapterStatus, WGPUAdapter a, WGPUStringView,
                   void*, void*) { adapter= a; };
  wgpuInstanceRequestAdapter (inst, NULL, ac);
  WGPURequestDeviceCallbackInfo dc= {};
  dc.callback= [] (WGPURequestDeviceStatus, WGPUDevice d, WGPUStringView,
                   void*, void*) { device= d; };
  wgpuAdapterRequestDevice (adapter, NULL, dc);
  WGPUSurfaceDescriptor sd= {};
  WGPUSurface surface= wgpuInstanceCreateSurface (inst, &sd);

  tvg::Initializer::init (0);
  tvg::WgCanvas* canvas= tvg::WgCanvas::gen ();
  if (canvas->target (device, inst, surface, W, H, tvg::ColorSpace::ABGR8888S, 0)
      != tvg::Result::Success) return 2;

  for (int f= 0; f < frames; f++) {
    canvas->remove ();
    tvg::Shape* bg= tvg::Shape::gen ();
    bg->appendRect (0, 0, W, H);
    bg->fill (245, 240, 230);
    canvas->add (bg);
    // a rounded rectangle with a linear gradient
    tvg::Shape* r= tvg::Shape::gen ();
    r->appendRect (40, 40, 260, 160, 24, 24);
    tvg::LinearGradient* g= tvg::LinearGradient::gen ();
    g->linear (40, 40, 300, 200);
    tvg::Fill::ColorStop stops[2]= { { 0, 40, 90, 200, 255 }, { 1, 200, 60, 120, 255 } };
    g->colorStops (stops, 2);
    r->fill (g);
    canvas->add (r);
    // a stroked circle
    tvg::Shape* c= tvg::Shape::gen ();
    c->appendCircle (460, 140, 90, 90);
    c->fill (255, 200, 0, 180);
    c->strokeWidth (8);
    c->strokeFill (30, 30, 30);
    canvas->add (c);
    // a turning star, even-odd
    tvg::Shape* s= tvg::Shape::gen ();
    float cx= 320, cy= 340, R= 110, a0= f * 0.05f;
    for (int i= 0; i < 5; i++) {
      float a= a0 + i * 4 * 3.14159265f / 5;
      float x= cx + R * cosf (a), y= cy + R * sinf (a);
      if (i == 0) s->moveTo (x, y); else s->lineTo (x, y);
    }
    s->close ();
    s->fillRule (tvg::FillRule::EvenOdd);
    s->fill (20, 140, 90);
    canvas->add (s);
    canvas->draw (true);
    canvas->sync ();
    wgpuSurfacePresent (surface);
    usleep (16000);
  }
  char ch;
  while (read (0, &ch, 1) > 0) ;
  return 0;
}
