// The host of the WebGPU of WASIX (misc/wasix/webgpu/webgpu_wire.cpp): the
// calls which the program sends in the messages GPU of its stream are made
// on the WebGPU of the page, which draws in its canvas. The encoding of the
// calls is at the top of webgpu_wire.cpp.

import { ENUMS } from "./webgpu-enums.js";

const ID_INSTANCE = 1, ID_ADAPTER = 2, ID_DEVICE = 3, ID_QUEUE = 4;

export class WebGPUHost {
  // canvas: where the surfaces of the program draw
  constructor (canvas) {
    this.canvas = canvas;
    this.objects = new Map ();
    this.frames = 0;
    this.calls = 0;
    this.errors = [];
    this.ready = this.init ();
  }

  async init () {
    if (!navigator.gpu) throw new Error ("this browser has no WebGPU");
    this.adapter = await navigator.gpu.requestAdapter ();
    if (!this.adapter) throw new Error ("no WebGPU adapter");
    this.device = await this.adapter.requestDevice ();
    this.device.addEventListener ("uncapturederror", e => {
      if (this.errors.length < 20) {
        this.errors.push (e.error.message);
        console.warn ("[webgpu]", e.error.message);
      }
    });
    this.objects.set (ID_INSTANCE, navigator.gpu);
    this.objects.set (ID_ADAPTER, this.adapter);
    this.objects.set (ID_DEVICE, this.device);
    this.objects.set (ID_QUEUE, this.device.queue);
  }

  // a message GPU: calls one after the other
  run (bytes) {
    const r = { bytes, dv: new DataView (bytes.buffer, bytes.byteOffset, bytes.byteLength), pos: 0 };
    while (r.pos < bytes.length) {
      const c = this.value (r);
      this.call (c[0], c[1], c[2], c[3]);
    }
  }

  value (r) {
    const t = r.bytes[r.pos++], dv = r.dv;
    switch (t) {
    case 0: return undefined;
    case 1: return false;
    case 2: return true;
    case 3: { const v = dv.getUint32 (r.pos, true); r.pos += 4; return v; }
    case 4: { const v = dv.getFloat64 (r.pos, true); r.pos += 8; return v; }
    case 5: {
      const n = dv.getUint32 (r.pos, true);
      const s = new TextDecoder ().decode (r.bytes.subarray (r.pos + 4, r.pos + 4 + n));
      r.pos += 4 + n; return s;
    }
    case 6: {
      const n = dv.getUint32 (r.pos, true);
      const b = r.bytes.subarray (r.pos + 4, r.pos + 4 + n);
      r.pos += 4 + n; return b;
    }
    case 7: {
      const n = dv.getUint32 (r.pos, true); r.pos += 4;
      const a = new Array (n);
      for (let i = 0; i < n; i++) a[i] = this.value (r);
      return a;
    }
    case 8: {
      const n = dv.getUint32 (r.pos, true); r.pos += 4;
      const o = {};
      for (let i = 0; i < n; i++) {
        const l = dv.getUint16 (r.pos, true);
        const k = new TextDecoder ().decode (r.bytes.subarray (r.pos + 2, r.pos + 2 + l));
        r.pos += 2 + l;
        o[k] = this.value (r);
      }
      return o;
    }
    case 9: {
      const id = dv.getUint32 (r.pos, true); r.pos += 4;
      const o = this.objects.get (id);
      if (o === undefined) throw new Error (`webgpu: no object ${id}`);
      return o;
    }
    case 10: {
      const k = r.bytes[r.pos], v = dv.getUint32 (r.pos + 1, true); r.pos += 5;
      return ENUMS[k][v];
    }
    default: throw new Error (`webgpu: value of type ${t} at ${r.pos - 1}`);
    }
  }

  call (method, target, args, result) {
    this.calls++;
    let v;
    switch (method) {
    case "release": // (those of the host stay)
      if (args[0] > ID_QUEUE) this.objects.delete (args[0]);
      return;
    case "createSurface":
      v = this.canvas.getContext ("webgpu");
      break;
    case "configure": {
      // the canvas has the size the page gives it
      const { width, height, ...c } = args[0];
      target.configure (c);
      return;
    }
    case "present":
      this.frames++;
      return;
    default:
      v = target[method] (...args);
    }
    if (result) this.objects.set (result, v);
  }
}
