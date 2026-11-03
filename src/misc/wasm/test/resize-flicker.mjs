// The frames shown with an empty canvas while the edge of the column of the
// tabs is dragged (misc/wasm/frame.js), in a headless Firefox or Chrome:
//
//   node misc/wasm/test/resize-flicker.mjs [src directory] [--browser <path>] [--scale=2]
//
// from src/, after the "web" target of misc/wasm/Makefile. A change of the
// size of the canvas clears it; a frame which shows it before TeXmacs drew
// it again flickers (see event_filter in src/Plugins/Vue/vue_gui.cpp). The
// pixels at nine points of the canvas are read after every callback of a
// frame (requestAnimationFrame wrapped), the last reading being what the
// frame shows, and so is whether the canvas has as many pixels as its size
// on the page (else the browser stretches its last image). The edge is
// dragged there and back, then against the limit of 100 pixels and back
// (moves which leave the width as it is). --scale=2 is a screen of density
// 2. Exits with 1 when a frame was shown empty or stretched.
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
// drawn with MuPDF (?gpu=0): the main loop of the GPU renderer does not go
// through window.requestAnimationFrame, where the frames are read
const url = `http://127.0.0.1:${server.address ().port}/texmacs.html?gpu=0`;
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

// every callback of a frame is wrapped: after it, the pixel at the middle of
// the canvas is read; the last reading of a frame is what the frame shows
const kind = await page.evaluate (() => {
  const c = document.getElementById ('canvas');
  const gl = c.getContext ('webgl2') || c.getContext ('webgl');
  const ctx = gl ? null : c.getContext ('2d');
  window.tmFrames = new Map ();
  window.tmSampling = false;
  const raf = window.requestAnimationFrame.bind (window);
  window.requestAnimationFrame = function (cb) {
    return raf (function (t) {
      cb (t);
      if (!window.tmSampling) return;
      // nine points of the canvas, and whether its pixels match its size
      // on the page (else the browser stretches the last image)
      let empty = 0;
      for (const fx of [0.1, 0.5, 0.9]) for (const fy of [0.1, 0.5, 0.9]) {
        const x = Math.floor (c.width * fx), y = Math.floor (c.height * fy);
        let px;
        if (gl) { px = new Uint8Array (4); gl.readPixels (x, y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, px); }
        else px = ctx.getImageData (x, y, 1, 1).data;
        if (px[3] === 0) empty++;
      }
      const r = c.getBoundingClientRect (), dpr = window.devicePixelRatio || 1;
      const stretched = Math.abs (c.width - Math.round (r.width * dpr)) > 1 ||
                        Math.abs (c.height - Math.round (r.height * dpr)) > 1;
      window.tmFrames.set (t, empty ? 'empty' : stretched ? 'stretched' : 'drawn');
    });
  };
  return gl ? 'webgl' : ctx ? '2d' : 'none';
});
console.log ('canvas:', kind);
await page.evaluate (() => { window.tmSampling = true; });
// a slow drag of the edge, one step per frame or so, there and back
await page.mouse.move (198, 400);
await page.mouse.down ();
for (let k = 1; k <= 40; k++) { await page.mouse.move (198 + 4 * k, 400); await sleep (16); }
for (let k = 1; k <= 40; k++) { await page.mouse.move (358 - 4 * k, 400); await sleep (16); }
for (let k = 1; k <= 60; k++) { await page.mouse.move (198 - 1.7 * k, 400); await sleep (16); }
for (let k = 1; k <= 60; k++) { await page.mouse.move (96 + 1.7 * k, 400); await sleep (16); }
await page.mouse.up ();
await sleep (500);
const r = await page.evaluate (() => {
  window.tmSampling = false;
  const v = [...window.tmFrames.values ()];
  return { frames: v.length, empty: v.filter (x => x === 'empty').length,
           stretched: v.filter (x => x === 'stretched').length };
});
console.log (`frames ${r.frames}, shown empty ${r.empty}, stretched ${r.stretched}`);
await b.close ();
server.close ();
process.exit (r.empty > 0 || r.stretched > 0 || r.frames === 0 ? 1 : 0);
