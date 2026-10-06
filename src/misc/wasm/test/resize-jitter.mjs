// The page of the document must not jump while the edge of the column of
// the tabs is dragged (misc/wasm/frame.js), in a headless Firefox or Chrome:
//
//   node misc/wasm/test/resize-jitter.mjs [src directory] [--browser <path>] [--scale=2]
//
// from src/, after the "web" target of misc/wasm/Makefile. After every
// frame (requestAnimationFrame wrapped, the last reading of a frame being
// what it shows) the width of the canvas and the left edge of the white
// page in it are read: a width must always come with the same edge. The
// frame drawn at once by a resize (event_filter in vue_gui.cpp) showed the
// editor where it was, and the next frame where it goes: the page jumped
// at each step of a drag. Exits with 1 when a width came with two edges.
import path from 'node:path';
import { createRequire } from 'node:module';

const SRC = path.resolve (process.argv.slice (2).filter ((a, i, l) => !a.startsWith ('--') && l[i - 1] !== '--browser')[0] || '.');
const bi = process.argv.indexOf ('--browser');
const BROWSER = bi > 0 ? process.argv[bi + 1] : '/Applications/Firefox.app/Contents/MacOS/firefox';
const CHROME = /chrom/i.test (BROWSER);
const require = createRequire (path.join (SRC, 'build-wasm/tools/package.json'));
const puppeteer = require ('puppeteer-core');
const { serve } = await import (path.join (SRC, 'misc/wasm/serve.mjs'));
const server = await serve (path.join (SRC, 'build-wasm/out/web'), 0, '127.0.0.1', () => {}, 0);
const url = `http://127.0.0.1:${server.address ().port}/texmacs.html`;
const sleep = ms => new Promise (ok => setTimeout (ok, ms));

const SCALE = Number ((process.argv.find (a => a.startsWith ('--scale=')) || '--scale=1').slice (8));
const b = await puppeteer.launch ({ browser: CHROME ? 'chrome' : 'firefox', executablePath: BROWSER,
                                    headless: true,
                                    args: CHROME ? ['--window-size=1280,800', '--use-angle=swiftshader',
                                                    '--enable-unsafe-swiftshader', '--ignore-gpu-blocklist'] : [] });
const page = await b.newPage ();
await page.setViewport ({ width: 1280, height: 800, deviceScaleFactor: SCALE });
await page.goto (url, { waitUntil: 'load', timeout: 120000 });
await page.waitForFunction (() => typeof runtimeInitialized !== 'undefined' && runtimeInitialized &&
                                  typeof tmFrame !== 'undefined' && tmFrame.tabs ().length > 0,
                            { timeout: 120000, polling: 500 });
await sleep (4000);

// every callback of a frame is wrapped: after it, the left edge of the page
// on a row at 60 % of the height; the last reading of a frame is what it shows
const kind = await page.evaluate (() => {
  const c = document.getElementById ('canvas');
  const gl = c.getContext ('webgl2') || c.getContext ('webgl');
  if (gl) return 'webgl';
  const ctx = c.getContext ('2d');
  window.tmFrames = new Map ();
  window.tmSampling = false;
  const raf = window.requestAnimationFrame.bind (window);
  window.requestAnimationFrame = function (cb) {
    return raf (function (t) {
      cb (t);
      if (!window.tmSampling) return;
      const y = Math.floor (c.height * 0.6);
      const row = ctx.getImageData (0, y, c.width, 1).data;
      let edge = -1;
      for (let x = 0; x < c.width; x++)
        if (row[4*x] > 245 && row[4*x+1] > 245 && row[4*x+2] > 245) { edge = x; break; }
      window.tmFrames.set (t, [c.width, edge]);
    });
  };
  return '2d';
});
console.log ('canvas:', kind);
if (kind !== '2d') { console.log ('only for a canvas drawn with MuPDF'); process.exit (2); }
await page.evaluate (() => { window.tmSampling = true; });
// a drag of the edge, one step every 120 ms (several frames each)
await page.mouse.move (198, 400);
await page.mouse.down ();
for (let k = 1; k <= 20; k++) { await page.mouse.move (198 + 6 * k, 400); await sleep (120); }
for (let k = 1; k <= 20; k++) { await page.mouse.move (318 - 6 * k, 400); await sleep (120); }
await page.mouse.up ();
await sleep (500);
const r = await page.evaluate (() => {
  window.tmSampling = false;
  const byWidth = new Map ();
  for (const [w, e] of window.tmFrames.values ()) {
    if (!byWidth.has (w)) byWidth.set (w, new Set ());
    byWidth.get (w).add (e);
  }
  const jumps = [...byWidth.entries ()].filter (([w, s]) => s.size > 1)
                                       .map (([w, s]) => w + ': ' + [...s].join (' and '));
  return { frames: window.tmFrames.size, widths: byWidth.size, jumps };
});
console.log (`frames ${r.frames}, widths ${r.widths}, widths with two places of the page ${r.jumps.length}` +
             (r.jumps.length ? ' (' + r.jumps.slice (0, 5).join ('; ') + ')' : ''));
await b.close ();
server.close ();
process.exit (r.jumps.length > 0 || r.widths < 10 ? 1 : 0);
