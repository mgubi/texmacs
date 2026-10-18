// The page of the WASIX build (misc/wasix/web/index.html): TeXmacs with its
// interface, run by the Wasmer SDK (sdk/) with TEXMACS_DISPLAY=stream. The
// program sends the pixels which changed on its standard output and reads
// the events of the page on its standard input (the protocol is at the top
// of src/Plugins/Vue/vue_stream.cpp); the page draws them in the canvas.
//
// The program (texmacs.wasm) and the whole tree of TEXMACS_PATH
// (texmacs-fs.bin, an index in texmacs-fs.json, made by pack-fs.py) make
// one package of the SDK, its files under /texmacs; the home of TeXmacs is
// /workspace/home in the sandbox (in memory: lost with the page).

import { Wasmer } from "./sdk/dist/index.js";
import { WebGPUHost } from "./webgpu-host.js";

const canvas = document.getElementById ("screen");
// the context of the canvas: 2d for the pixels of TeXmacs (RECT), webgpu
// for a program which draws with WebGPU (GPU, webgpu_wire.cpp); the first
// message which draws chooses
let ctx = null;
let gpu = null;
// ?program=<file.wasm>: another program than TeXmacs, without its files
// (the tests of the WebGPU of WASIX: hello-triangle.wasm)
const params = new URLSearchParams (location.search);
const program = params.get ("program");
const status = s => { document.getElementById ("status").textContent = s; };

const IN = { RESIZE: 1, MOTION: 2, BUTTON: 3, WHEEL: 4, KEY: 5, TEXT: 6,
             FOCUS: 7, LEAVE: 8, QUIT: 9 };
const OUT = { HELLO: 1, RECT: 2, FRAME: 3, CURSOR: 4, TITLE: 5, GPU: 6 };

/******************************************************************************
* Loading
******************************************************************************/

async function fetchBytes (url, label) {
  const r = await fetch (url);
  if (!r.ok) throw new Error (`${url}: ${r.status}`);
  const total = Number (r.headers.get ("content-length")) || 0;
  const reader = r.body.getReader ();
  const parts = [];
  let got = 0;
  for (;;) {
    const { done, value } = await reader.read ();
    if (done) break;
    parts.push (value);
    got += value.length;
    progress[label] = [got, total];
    showProgress ();
  }
  const all = new Uint8Array (got);
  let k = 0;
  for (const p of parts) { all.set (p, k); k += p.length; }
  return all;
}

const progress = {};
function showProgress () {
  let got = 0, total = 0;
  for (const [g, t] of Object.values (progress)) { got += g; total += t; }
  status (`downloading ${(got / 1e6).toFixed (0)} of ${(total / 1e6).toFixed (0)} MB`);
}

/******************************************************************************
* The stream from TeXmacs
******************************************************************************/

let pending = new Uint8Array (0);
let started = false;

let received = 0; // the bytes of the stream (for the tests)
function receive (chunk) {
  received += chunk.length;
  let buf = chunk;
  if (pending.length) {
    buf = new Uint8Array (pending.length + chunk.length);
    buf.set (pending); buf.set (chunk, pending.length);
  }
  let pos = 0;
  if (!started) {
    // the stream starts at HELLO, its magic word "TMWX"
    for (let i = 8; i + 4 <= buf.length; i++)
      if (buf[i] === 84 && buf[i + 1] === 77 && buf[i + 2] === 87 && buf[i + 3] === 88) {
        pos = i - 8; started = true; break;
      }
    if (!started) { pending = buf.slice (Math.max (0, buf.length - 11)); return; }
  }
  const dv = new DataView (buf.buffer, buf.byteOffset, buf.byteLength);
  while (buf.length - pos >= 8) {
    const type = dv.getUint32 (pos, true), n = dv.getUint32 (pos + 4, true);
    if (buf.length - pos - 8 < n) break;
    message (type, buf.subarray (pos + 8, pos + 8 + n), new DataView (buf.buffer, buf.byteOffset + pos + 8, n));
    pos += 8 + n;
  }
  pending = buf.slice (pos);
}

function message (type, p, dv) {
  if (type === OUT.RECT) {
    const x = dv.getUint32 (0, true), y = dv.getUint32 (4, true);
    const w = dv.getUint32 (8, true), h = dv.getUint32 (12, true);
    if (w === 0 || h === 0) return;
    if (!ctx) ctx = canvas.getContext ("2d");
    const img = new ImageData (w, h);
    const d = img.data;
    for (let i = 0, s = 20; i < 4 * w * h; i += 4, s += 4) {
      d[i] = p[s + 2]; d[i + 1] = p[s + 1]; d[i + 2] = p[s]; d[i + 3] = 255;
    }
    ctx.putImageData (img, x, y);
  }
  else if (type === OUT.FRAME) {
    frames++;
    if (frames === 1) document.getElementById ("loading").classList.add ("gone");
  }
  else if (type === OUT.GPU) {
    gpu.run (p);
    if (gpu.frames > 0) document.getElementById ("loading").classList.add ("gone");
  }
  else if (type === OUT.CURSOR) canvas.style.cursor = CURSORS[dv.getUint32 (0, true)] || "default";
  else if (type === OUT.TITLE) document.title = new TextDecoder ().decode (p) || "TeXmacs";
}

let frames = 0;
// SDL_SystemCursor, as CSS
const CURSORS = ["default", "text", "wait", "crosshair", "progress", "nwse-resize",
                 "nesw-resize", "ew-resize", "ns-resize", "move", "not-allowed",
                 "pointer", "nw-resize", "n-resize", "ne-resize", "e-resize",
                 "se-resize", "s-resize", "sw-resize", "w-resize"];

/******************************************************************************
* The events to TeXmacs
******************************************************************************/

let stdin = null;
let chain = Promise.resolve ();
function send (type, payload = new Uint8Array (0)) {
  if (!stdin) return;
  const m = new Uint8Array (8 + payload.length);
  const dv = new DataView (m.buffer);
  dv.setUint32 (0, type, true); dv.setUint32 (4, payload.length, true);
  m.set (payload, 8);
  chain = chain.then (() => stdin.write (m)).catch (() => {});
}

// the words of a message: numbers as u32, those given as {f: x} as f32
function words (...v) {
  const b = new Uint8Array (4 * v.length), dv = new DataView (b.buffer);
  v.forEach ((x, i) => typeof x === "object" ? dv.setFloat32 (4 * i, x.f, true)
                                             : dv.setUint32 (4 * i, x >>> 0, true));
  return b;
}
const f = x => ({ f: x });

// SDL_Keymod
function mods (e) {
  return (e.shiftKey ? 0x0001 : 0) | (e.ctrlKey ? 0x0040 : 0) |
         (e.altKey ? 0x0100 : 0) | (e.metaKey ? 0x0400 : 0) |
         (e.getModifierState && e.getModifierState ("CapsLock") ? 0x2000 : 0);
}

// SDL_Scancode of KeyboardEvent.code (the USB usage of the key)
const SCANCODES = { Enter: 40, Escape: 41, Backspace: 42, Tab: 43, Space: 44,
  Minus: 45, Equal: 46, BracketLeft: 47, BracketRight: 48, Backslash: 49,
  Semicolon: 51, Quote: 52, Backquote: 53, Comma: 54, Period: 55, Slash: 56,
  CapsLock: 57, PrintScreen: 70, ScrollLock: 71, Pause: 72, Insert: 73,
  Home: 74, PageUp: 75, Delete: 76, End: 77, PageDown: 78, ArrowRight: 79,
  ArrowLeft: 80, ArrowDown: 81, ArrowUp: 82, NumLock: 83, NumpadDivide: 84,
  NumpadMultiply: 85, NumpadSubtract: 86, NumpadAdd: 87, NumpadEnter: 88,
  NumpadDecimal: 99, IntlBackslash: 100, ContextMenu: 101, NumpadEqual: 103,
  ControlLeft: 224, ShiftLeft: 225, AltLeft: 226, MetaLeft: 227,
  ControlRight: 228, ShiftRight: 229, AltRight: 230, MetaRight: 231 };
for (let i = 0; i < 26; i++) SCANCODES["Key" + String.fromCharCode (65 + i)] = 4 + i;
for (let i = 1; i <= 9; i++) SCANCODES["Digit" + i] = 29 + i;
SCANCODES.Digit0 = 39;
for (let i = 1; i <= 12; i++) SCANCODES["F" + i] = 57 + i;
for (let i = 1; i <= 9; i++) SCANCODES["Numpad" + i] = 88 + i;
SCANCODES.Numpad0 = 98;

// SDL_Keycode: the character of the key without shift for the keys which
// type one (on a US layout), the scancode with bit 30 for the others
const KEY_CHARS = { Enter: 13, Escape: 27, Backspace: 8, Tab: 9, Space: 32,
  Minus: 45, Equal: 61, BracketLeft: 91, BracketRight: 93, Backslash: 92,
  Semicolon: 59, Quote: 39, Backquote: 96, Comma: 44, Period: 46, Slash: 47,
  Delete: 127 };
function keycode (e, sc) {
  if (e.code in KEY_CHARS) return KEY_CHARS[e.code];
  if (/^Key[A-Z]$/.test (e.code)) return e.code.charCodeAt (3) + 32;
  if (/^Digit[0-9]$/.test (e.code)) return e.code.charCodeAt (5);
  // a layout which types another letter: the key it types
  if (e.key.length === 1) return e.key.toLowerCase ().codePointAt (0);
  return sc | 0x40000000;
}

function key (e, down) {
  const sc = SCANCODES[e.code] || 0;
  send (IN.KEY, words (sc, keycode (e, sc), mods (e), down ? 1 : 0, e.repeat ? 1 : 0));
  // the text the key types, as SDL sends it after the key (not with the
  // shortcuts of control and command)
  if (down && e.key.length >= 1 && [...e.key].length === 1 && !e.ctrlKey && !e.metaKey)
    send (IN.TEXT, new TextEncoder ().encode (e.key));
}

function pos (e) {
  const r = canvas.getBoundingClientRect ();
  return [e.clientX - r.left, e.clientY - r.top];
}
// SDL buttons: left 1, middle 2, right 3; their masks 1, 2, 4
const BUTTON = [1, 2, 3];
function buttonMask (b) {
  return (b & 1 ? 1 : 0) | (b & 4 ? 2 : 0) | (b & 2 ? 4 : 0);
}

function listen () {
  canvas.addEventListener ("keydown", e => { key (e, true); e.preventDefault (); });
  canvas.addEventListener ("keyup", e => { key (e, false); e.preventDefault (); });
  canvas.addEventListener ("mousemove", e => {
    const [x, y] = pos (e);
    send (IN.MOTION, words (f (x), f (y), buttonMask (e.buttons), mods (e)));
  });
  canvas.addEventListener ("mousedown", e => {
    canvas.focus ();
    const [x, y] = pos (e);
    send (IN.BUTTON, words (f (x), f (y), BUTTON[e.button] || 1, 1, e.detail || 1, mods (e)));
    e.preventDefault ();
  });
  window.addEventListener ("mouseup", e => {
    const [x, y] = pos (e);
    send (IN.BUTTON, words (f (x), f (y), BUTTON[e.button] || 1, 0, e.detail || 1, mods (e)));
  });
  canvas.addEventListener ("wheel", e => {
    const [x, y] = pos (e);
    // SDL: lines, up and right positive; the page: pixels, down positive
    const k = e.deltaMode === 0 ? 1 / 40 : e.deltaMode === 1 ? 1 : 10;
    send (IN.WHEEL, words (f (x), f (y), f (e.deltaX * k), f (-e.deltaY * k), mods (e)));
    e.preventDefault ();
  }, { passive: false });
  canvas.addEventListener ("contextmenu", e => e.preventDefault ());
  canvas.addEventListener ("mouseleave", () => send (IN.LEAVE));
  canvas.addEventListener ("focus", () => send (IN.FOCUS, words (1)));
  canvas.addEventListener ("blur", () => send (IN.FOCUS, words (0)));
  window.addEventListener ("resize", resize);
}

function resize () {
  const w = window.innerWidth, h = window.innerHeight;
  if (canvas.width !== w || canvas.height !== h) {
    // (a new size clears the canvas: TeXmacs sends a whole frame for it)
    canvas.width = w; canvas.height = h;
    canvas.style.width = w + "px"; canvas.style.height = h + "px";
  }
  send (IN.RESIZE, words (w, h));
}

/******************************************************************************
* Starting TeXmacs
******************************************************************************/

async function main () {
  if (!crossOriginIsolated)
    throw new Error ("the page is not cross-origin isolated (COOP/COEP): the SDK needs it");
  // the WebGPU of the page, for the programs which draw with it
  if (navigator.gpu) {
    gpu = new WebGPUHost (canvas);
    await gpu.ready.catch (e => { console.warn (e); gpu = null; });
  }
  const files = {};
  let wasm;
  if (program) wasm = await fetchBytes (program, "wasm");
  else {
    let blob, index;
    [wasm, blob, index] = await Promise.all ([
      fetchBytes ("texmacs.wasm", "wasm"), fetchBytes ("texmacs-fs.bin", "fs"),
      fetch ("texmacs-fs.json").then (r => r.json ())]);
    for (const [path, offset, size] of index)
      files[path] = blob.subarray (offset, offset + size);
  }
  status ("starting the runtime");
  const wasmer = new Wasmer ();
  await wasmer.ready ();
  const pkg = await wasmer.packages.create ({
    modules: { texmacs: wasm }, commands: { texmacs: { module: "texmacs" } }, files });
  status ("starting TeXmacs");
  const sandbox = await wasmer.sandboxes.create ({
    packages: [pkg],
    env: { TEXMACS_PATH: "/texmacs", TEXMACS_HOME_PATH: "/workspace/home",
           HOME: "/workspace/home", TEXMACS_DISPLAY: "stream",
           // TeXmacs draws with the WebGPU of the page (its GPU renderer),
           // unless ?gpu=0 (the pixels which MuPDF draws)
           ...(gpu && params.get ("gpu") !== "0" ? { TEXMACS_VUE_GPU: "webgpu" } : {}) } });
  const proc = await sandbox.command (pkg, [], { cwd: "/workspace" })
    .spawn ({ stdin: "pipe", stdout: "pipe", stderr: "pipe" });
  stdin = proc.stdin;
  (async () => {
    const dec = new TextDecoder ();
    for await (const d of proc.stderr) console.log ("[texmacs]", dec.decode (d).trimEnd ());
  }) ();
  resize ();
  listen ();
  canvas.focus ();
  send (IN.FOCUS, words (1));
  // (the frames of the pixels, or of WebGPU)
  window.tmWasix = { get frames () { return frames + (gpu ? gpu.frames : 0); }, proc,
                     get bytes () { return received; },
                     get gpu () { return gpu && { frames: gpu.frames, calls: gpu.calls,
                                                  errors: gpu.errors, methods: gpu.methods,
                                                  bytes: gpu.bytes }; } };
  for await (const chunk of proc.stdout) receive (chunk);
  const out = await proc.wait ();
  status (`TeXmacs has ended (exit ${out.exitCode})`);
  document.getElementById ("loading").classList.remove ("gone");
}

main ().catch (e => { status ("error: " + (e.message || e)); console.error (e); });
