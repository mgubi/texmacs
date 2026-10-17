// A client of the stream display of the WASIX build (vue_stream.cpp): the
// protocol for node, used by the tests and by the desktop runner.
//
//   import { StreamClient } from './stream-client.mjs';
//   const c = new StreamClient (child.stdout, bytes => child.stdin.write (bytes));
//   c.resize (1000, 700); c.onFrame = () => ...; c.png () ...

import zlib from 'node:zlib';

export const IN = { RESIZE: 1, MOTION: 2, BUTTON: 3, WHEEL: 4, KEY: 5, TEXT: 6,
                    FOCUS: 7, LEAVE: 8, QUIT: 9 };
export const OUT = { HELLO: 1, RECT: 2, FRAME: 3, CURSOR: 4, TITLE: 5 };

export class StreamClient {
  constructor (readable, write) {
    this.write = write;
    this.buf = Buffer.alloc (0);
    this.width = 0; this.height = 0;
    this.pixels = null;   // RGBA
    this.frames = 0; this.bytes = 0; this.rects = 0;
    this.title = ''; this.cursor = 0; this.version = 0;
    this.onFrame = null;
    readable.on ('data', d => this.receive (d));
  }

  receive (d) {
    this.bytes += d.length;
    this.buf = this.buf.length ? Buffer.concat ([this.buf, d]) : d;
    let pos = 0;
    if (!this.version) {
      // the stream starts at the HELLO with its magic word "TMWX"
      const k = this.buf.indexOf ('TMWX');
      if (k < 8) { this.buf = this.buf.subarray (Math.max (0, this.buf.length - 11)); return; }
      pos = k - 8;
    }
    while (this.buf.length - pos >= 8) {
      const type = this.buf.readUInt32LE (pos), n = this.buf.readUInt32LE (pos + 4);
      if (this.buf.length - pos - 8 < n) break;
      this.message (type, this.buf.subarray (pos + 8, pos + 8 + n));
      pos += 8 + n;
    }
    this.buf = this.buf.subarray (pos);
  }

  message (type, p) {
    if (type === OUT.HELLO) this.version = p.readUInt32LE (4);
    else if (type === OUT.RECT) {
      const x = p.readUInt32LE (0), y = p.readUInt32LE (4);
      const w = p.readUInt32LE (8), h = p.readUInt32LE (12);
      this.rects++;
      // the frame which follows gives the size; a rectangle may come first
      const W = Math.max (this.width, x + w), H = Math.max (this.height, y + h);
      if (W !== this.width || H !== this.height) this.reshape (W, H);
      for (let j = 0; j < h; j++)
        for (let i = 0; i < w; i++) {
          const s = 20 + 4 * (j * w + i), t = 4 * ((y + j) * this.width + x + i);
          this.pixels[t] = p[s + 2]; this.pixels[t + 1] = p[s + 1];
          this.pixels[t + 2] = p[s]; this.pixels[t + 3] = 255;
        }
    }
    else if (type === OUT.FRAME) {
      const w = p.readUInt32LE (0), h = p.readUInt32LE (4);
      if (w !== this.width || h !== this.height) this.reshape (w, h);
      this.frames++;
      if (this.onFrame) this.onFrame (this);
    }
    else if (type === OUT.CURSOR) this.cursor = p.readUInt32LE (0);
    else if (type === OUT.TITLE) this.title = p.toString ('utf8');
  }

  reshape (w, h) {
    const old = this.pixels, ow = this.width, oh = this.height;
    this.pixels = new Uint8Array (4 * w * h);
    if (old)
      for (let j = 0; j < Math.min (h, oh); j++)
        this.pixels.set (old.subarray (4 * j * ow, 4 * (j * ow + Math.min (w, ow))), 4 * j * w);
    this.width = w; this.height = h;
  }

  send (type, payload = Buffer.alloc (0)) {
    const h = Buffer.alloc (8);
    h.writeUInt32LE (type, 0); h.writeUInt32LE (payload.length, 4);
    this.write (Buffer.concat ([h, payload]));
  }

  static words (...v) {
    const b = Buffer.alloc (4 * v.length);
    // the numbers are u32, those made by f () f32
    v.forEach ((x, i) => typeof x === 'object'
      ? b.writeFloatLE (x.f, 4 * i) : b.writeUInt32LE (x >>> 0, 4 * i));
    return b;
  }
  static f (x) { return { f: x }; }

  resize (w, h) { this.send (IN.RESIZE, StreamClient.words (w, h)); }
  focus (on = true) { this.send (IN.FOCUS, StreamClient.words (on ? 1 : 0)); }
  motion (x, y, buttons = 0, mod = 0) {
    const f = StreamClient.f;
    this.send (IN.MOTION, StreamClient.words (f (x), f (y), buttons, mod));
  }
  button (x, y, button, down, clicks = 1, mod = 0) {
    const f = StreamClient.f;
    this.send (IN.BUTTON, StreamClient.words (f (x), f (y), button, down ? 1 : 0, clicks, mod));
  }
  click (x, y, button = 1) {
    this.motion (x, y);
    this.button (x, y, button, true);
    this.button (x, y, button, false);
  }
  wheel (x, y, dx, dy, mod = 0) {
    const f = StreamClient.f;
    this.send (IN.WHEEL, StreamClient.words (f (x), f (y), f (dx), f (dy), mod));
  }
  key (scancode, keycode, down = true, mod = 0, repeat = 0) {
    this.send (IN.KEY, StreamClient.words (scancode, keycode, mod, down ? 1 : 0, repeat));
  }
  text (s) { this.send (IN.TEXT, Buffer.from (s, 'utf8')); }
  quit () { this.send (IN.QUIT); }

  png () {
    const w = this.width, h = this.height;
    const raw = Buffer.alloc ((4 * w + 1) * h);
    for (let j = 0; j < h; j++) {
      raw[j * (4 * w + 1)] = 0;
      Buffer.from (this.pixels.buffer, 4 * j * w, 4 * w).copy (raw, j * (4 * w + 1) + 1);
    }
    const crcTable = new Int32Array (256).map ((_, n) => {
      let c = n; for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1; return c; });
    const crc = b => { let c = -1; for (const x of b) c = crcTable[(c ^ x) & 255] ^ (c >>> 8); return (c ^ -1) >>> 0; };
    const chunk = (t, d) => {
      const l = Buffer.alloc (4); l.writeUInt32BE (d.length);
      const td = Buffer.concat ([Buffer.from (t), d]);
      const c = Buffer.alloc (4); c.writeUInt32BE (crc (td));
      return Buffer.concat ([l, td, c]);
    };
    const ihdr = Buffer.alloc (13);
    ihdr.writeUInt32BE (w, 0); ihdr.writeUInt32BE (h, 4);
    ihdr[8] = 8; ihdr[9] = 6; ihdr[10] = ihdr[11] = ihdr[12] = 0;
    return Buffer.concat ([Buffer.from ([137, 80, 78, 71, 13, 10, 26, 10]),
                           chunk ('IHDR', ihdr), chunk ('IDAT', zlib.deflateSync (raw)),
                           chunk ('IEND', Buffer.alloc (0))]);
  }
}
