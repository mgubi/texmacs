/******************************************************************************
* MODULE     : webgpu_wire.h
* DESCRIPTION: WebGPU for WASIX: the calls of webgpu.h sent to a host which
*              makes them (the page of the browser: misc/wasix/web/
*              webgpu-host.js), see webgpu_wire.cpp
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WEBGPU_WIRE_H
#define WEBGPU_WIRE_H

#include <stddef.h>
#include <webgpu/webgpu.h>

#ifdef __cplusplus
extern "C" {
#endif

// where the batches of calls go: by default a message GPU (6) of the stream
// of the display (vue_stream.cpp) on the standard output; TeXmacs gives
// the send of its stream
typedef void (*wgpu_wire_sink) (const void* data, size_t n);
void wgpuWireSetSink (wgpu_wire_sink sink);

// the calls made so far go to the host (also done by wgpuQueueSubmit,
// wgpuSurfacePresent and when a batch is large)
void wgpuWireFlush (void);

// a frame which may not be shown: its calls are held from wgpuWireHold
// (flushes do nothing meanwhile) until wgpuWireRelease, which sends them,
// or drops them (keep 0): then they must have made nothing which the
// program uses afterwards (it drew into the screen only)
void wgpuWireHold (void);
void wgpuWireRelease (int keep);

#ifdef __cplusplus
}
#endif

#endif // WEBGPU_WIRE_H
