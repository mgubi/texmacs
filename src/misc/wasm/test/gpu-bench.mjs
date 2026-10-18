// The renderers of the page against each other: WebGPU (?gpu=webgpu),
// WebGL2 (the default) and MuPDF (?gpu=0), on a long document.
//
//   node misc/wasm/test/gpu-bench.mjs --browser <Chrome> --doc <file.tm>
//        [--dir build-wasm/out/web] [--runs 3] [--scale 2]
//        [--modes webgpu,webgl,mupdf] [--size 1280x800]
//
// The document is copied into the page as bench-doc.tm and opened
// (?open=); each run is a fresh browser. Three workloads, each action
// followed by frames of the page (requestAnimationFrame) until it is
// drawn:
//   repaint  full repaints of the editor, (refresh-window), 20 times
//   scroll   PageDown 30 times, PageUp 30 times
//   zoom     (set-window-zoom-factor) through 1.2 1.6 2 1.6 1.2 1, twice
//            (each zoom makes glyphs at a new size)
// The profile of TeXmacs (?profile=1: the times of each frame, in the
// console) gives, for each workload, the CPU time of TeXmacs (layout,
// repaint of the editor, redraw of the window) and the time of its frames
// (what the GPU makes them wait shows there). Medians of the runs.

import fs from 'node:fs';
import path from 'node:path';
import { createRequire } from 'node:module';
import { serve } from '../serve.mjs';

const args = process.argv.slice (2);
const opt = (k, d) => { const i = args.indexOf (k); return i >= 0 ? args[i + 1] : d; };
const browserPath = opt ('--browser', null);
const docFile = opt ('--doc', null);
if (!browserPath || !docFile) {
  console.error ('usage: gpu-bench.mjs --browser <Chrome> --doc <file.tm> [options]');
  process.exit (2);
}
const dir = path.resolve (opt ('--dir', 'build-wasm/out/web'));
const runs = Number (opt ('--runs', '3'));
const scale = Number (opt ('--scale', '2'));
const [W, H] = opt ('--size', '1280x800').split ('x').map (Number);
const modes = opt ('--modes', 'webgpu,webgl,mupdf').split (',');
const QUERY = { webgpu: 'gpu=webgpu', webgl: '', mupdf: 'gpu=0' };

const require = createRequire (path.resolve ('build-wasm/tools/package.json'));
const puppeteer = require ('puppeteer-core');
fs.copyFileSync (docFile, path.join (dir, 'bench-doc.tm'));
const server = await serve (dir, 0, '127.0.0.1');
const port = server.address ().port;
const sleep = ms => new Promise (r => setTimeout (r, ms));

// frames of the page: n turns of requestAnimationFrame (the main loop of
// TeXmacs runs in them)
const frames = (page, n) => page.evaluate (n => new Promise (r => {
  const step = k => k ? requestAnimationFrame (() => step (k - 1)) : r ();
  step (n); }), n);

async function workload (page, name, actions) {
  await frames (page, 10);
  await page.evaluate (n => console.log ('BENCH start ' + n), name);
  const t0 = Date.now ();
  for (const a of actions) await a ();
  await frames (page, 10);
  await page.evaluate (n => console.log ('BENCH end ' + n), name);
  return Date.now () - t0;
}

async function run (mode) {
  const browser = await puppeteer.launch ({
    executablePath: browserPath, headless: true,
    defaultViewport: { width: W, height: H, deviceScaleFactor: scale } });
  const lines = [];
  try {
    const page = await browser.newPage ();
    page.on ('console', m => lines.push (m.text ()));
    const q = ['open=bench-doc.tm', 'profile=1', QUERY[mode]].filter (x => x).join ('&');
    await page.goto (`http://127.0.0.1:${port}/texmacs.html?${q}`);
    await page.waitForFunction (() => document.title.includes ('bench-doc'),
                                { timeout: 180000, polling: 250 });
    await sleep (3000);
    // (the keys go to the canvas)
    await page.mouse.click (W * 0.6, H * 0.5);
    await frames (page, 30);
    const later = c => () => page.evaluate (c => window.TeXmacs.later (c), c);
    const wall = {};
    const rep = [];
    for (let i = 0; i < 20; i++) rep.push (later ('(refresh-window)'), () => frames (page, 4));
    wall.repaint = await workload (page, 'repaint', rep);
    const scr = [];
    for (let i = 0; i < 30; i++) scr.push (() => page.keyboard.press ('PageDown'), () => frames (page, 3));
    for (let i = 0; i < 30; i++) scr.push (() => page.keyboard.press ('PageUp'), () => frames (page, 3));
    wall.scroll = await workload (page, 'scroll', scr);
    const zoom = [];
    for (let k = 0; k < 2; k++)
      for (const z of [1.2, 1.6, 2, 1.6, 1.2, 1])
        zoom.push (later (`(set-window-zoom-factor ${z})`), () => frames (page, 6));
    wall.zoom = await workload (page, 'zoom', zoom);
    const drawn = await page.evaluate (() => {
      const c = document.getElementById ('canvas');
      return window.tmWebGPU ? 'webgpu' : (c.getContext ('webgl2') ? 'webgl2' : '2d'); });
    return { wall, drawn, profile: parse (lines) };
  } finally {
    await browser.close ();
  }
}

// the times of the phases of the frames between BENCH start and end
function parse (lines) {
  const out = {};
  let cur = null, frame = false;
  for (const l of lines) {
    let m;
    if ((m = /^BENCH start (\w+)/.exec (l))) { cur = { frames: 0, ms: {} }; out[m[1]] = cur; continue; }
    if (/^BENCH end/.test (l)) { cur = null; continue; }
    if (!cur) continue;
    // a report may come in one message of several lines
    for (const s of l.split ('\n')) {
      if ((m = /--- Vue: \d+ frames, ([\d.e+-]+) ms/.exec (s))) {
        cur.frames++; cur.ms.frame = (cur.ms.frame || 0) + Number (m[1]); continue; }
      if ((m = /^\s+(\w[\w ]*?)\t[\d.e+-]+%\t mean ([\d.e+-]+) ms/.exec (s)))
        cur.ms[m[1]] = (cur.ms[m[1]] || 0) + Number (m[2]);
    }
  }
  return out;
}

const median = a => { const s = [...a].sort ((x, y) => x - y); return s[Math.floor (s.length / 2)]; };
const results = {};
for (const mode of modes) {
  results[mode] = [];
  for (let r = 0; r < runs; r++) {
    const res = await run (mode);
    results[mode].push (res);
    console.log (`${mode} run ${r + 1}: drawn by ${res.drawn}; wall ` +
                 Object.entries (res.wall).map (([k, v]) => `${k} ${v} ms`).join (', '));
  }
}
server.close ();

const phases = ['layout', 'repaint', 'redraw', 'frame'];
console.log (`\nmedians of ${runs} runs, ${W}x${H} at scale ${scale} (ms; frame: the time of the frames)`);
for (const w of ['repaint', 'scroll', 'zoom']) {
  console.log (`\n${w}`);
  console.log ('  mode     ' + phases.map (p => p.padStart (9)).join ('') + '   frames     wall');
  for (const mode of modes) {
    const rs = results[mode].map (r => r.profile[w] || { frames: 0, ms: {} });
    const cell = p => median (rs.map (r => r.ms[p] || 0)).toFixed (1).padStart (9);
    console.log ('  ' + mode.padEnd (9) + phases.map (cell).join ('') +
                 String (median (rs.map (r => r.frames))).padStart (9) +
                 String (median (results[mode].map (r => r.wall[w]))).padStart (9));
  }
}
