// The WebGPU of WASIX (misc/wasix/webgpu) in a headless Chrome: the page
// runs a test program (hello-triangle.wasm, thorvg-shapes.wasm), whose calls
// of webgpu.h are made on the WebGPU of the page, until it has presented
// that many frames; the page is saved as <program>.png.
//
//   node misc/wasix/test/browser-webgpu.mjs <tools dir> <output dir>
//        [program [frames [web dir]]]

import fs from 'node:fs';
import path from 'node:path';
import { createRequire } from 'node:module';
import { serve } from '../serve.mjs';

const [tools, outDir, prog = 'hello-triangle.wasm', nframes = '120', webArg] =
  process.argv.slice (2);
if (!outDir) { console.error ('usage: browser-webgpu.mjs <tools dir> <output dir> [web dir]'); process.exit (2); }
fs.mkdirSync (outDir, { recursive: true });
const web = path.resolve (webArg || 'build-wasix/out/web');
const require = createRequire (path.join (path.resolve (tools), 'node_modules/'));
const puppeteer = require ('puppeteer-core');

function findChrome (dir) {
  for (const e of fs.readdirSync (dir, { withFileTypes: true })) {
    const p = path.join (dir, e.name);
    if (e.isDirectory () && e.name.endsWith ('.app'))
      return path.join (p, 'Contents/MacOS', e.name.replace (/\.app$/, ''));
    if (e.isDirectory ()) { const r = findChrome (p); if (r) return r; }
    if (e.isFile () && e.name === 'chrome') return p;
  }
  return null;
}

const server = await serve (web, 0);
const port = server.address ().port;
const browser = await puppeteer.launch ({
  executablePath: findChrome (path.join (tools, 'chrome')), headless: true,
  args: ['--no-first-run'], defaultViewport: { width: 640, height: 480 } });
let ok = false;
try {
  const page = await browser.newPage ();
  page.on ('console', m => console.log ('[page]', m.text ()));
  page.on ('pageerror', e => console.log ('[page error]', e.message));
  const t0 = Date.now ();
  await page.goto (`http://127.0.0.1:${port}/?program=${prog}`);
  await page.waitForFunction (n => window.tmWasix && window.tmWasix.gpu &&
                              window.tmWasix.gpu.frames >= n,
                              { timeout: 120000, polling: 100 }, Number (nframes));
  const g = await page.evaluate (() => window.tmWasix.gpu);
  console.log (`frames ${g.frames}, calls ${g.calls}, errors ${g.errors.length}, ${Date.now () - t0} ms`);
  for (const e of g.errors) console.log ('  ' + e);
  await page.screenshot ({ path: path.join (outDir, prog.replace (/\.wasm$/, '.png')) });
  ok = g.errors.length === 0;
} finally {
  await browser.close ();
  server.close ();
}
process.exit (ok ? 0 : 1);
