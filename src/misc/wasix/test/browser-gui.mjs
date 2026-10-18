// The interface of TeXmacs in the page of the WASIX build (index.html), in a
// headless Chrome: it starts, the File menu is opened by a click, a new
// document is made and text typed in it with the keyboard of the page.
//
//   node misc/wasix/test/browser-gui.mjs <tools dir> <output dir> [web dir] [query]
//
// query: that of the page (gpu=0: the pixels of MuPDF instead of WebGPU)
//
// <tools dir>: puppeteer-core and Chrome for Testing (see browser-pdf.mjs).
// Screenshots go to <output dir>.

import fs from 'node:fs';
import path from 'node:path';
import { createRequire } from 'node:module';
import { serve } from '../serve.mjs';

const [tools, outDir, webArg, query = ''] = process.argv.slice (2);
if (!outDir) { console.error ('usage: browser-gui.mjs <tools dir> <output dir> [web dir]'); process.exit (2); }
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
  args: ['--no-first-run'], defaultViewport: { width: 1000, height: 700 } });
const sleep = ms => new Promise (r => setTimeout (r, ms));
let ok = false;
try {
  const page = await browser.newPage ();
  const log = fs.createWriteStream (path.join (outDir, 'console.log'));
  page.on ('console', m => log.write (m.text () + '\n'));
  page.on ('pageerror', e => console.log ('[page error]', e.message));
  const t0 = Date.now ();
  await page.goto (`http://127.0.0.1:${port}/${query ? '?' + query : ''}`);
  const frames = () => page.evaluate (() => window.tmWasix ? window.tmWasix.frames : 0);
  // until the frames stop coming for a while
  async function settle (name, quiet = 1500, max = 180000) {
    const start = Date.now ();
    let n = await frames (), last = Date.now ();
    while (Date.now () - start < max) {
      await sleep (200);
      const m = await frames ();
      if (m !== n) { n = m; last = Date.now (); }
      else if (m > 0 && Date.now () - last > quiet) break;
    }
    await page.screenshot ({ path: path.join (outDir, name + '.png') });
    const mb = (await page.evaluate (() => window.tmWasix.bytes) / 1e6).toFixed (1);
    console.log (`${name}: ${n} frames, ${mb} MB, "${await page.title ()}", ${Date.now () - t0} ms`);
    return n;
  }
  await settle ('start', 3000);
  await page.mouse.click (28, 16);          // the File menu
  await settle ('menu');
  await page.mouse.click (40, 40);          // New
  await settle ('new');
  await page.keyboard.type ('Hello from the browser ');
  await page.keyboard.type ('$');
  await page.keyboard.type ('a^2+b^2');
  await page.keyboard.press ('ArrowRight');
  await settle ('typed');
  const g = await page.evaluate (() => window.tmWasix.gpu);
  if (g) console.log (`WebGPU: ${g.frames} frames, ${g.calls} calls, ${g.errors.length} errors`);
  for (const e of (g ? g.errors : [])) console.log ('  ' + e.split ('\n')[0]);
  ok = (await page.title ()).includes ('*') && !(g && g.errors.length);
} finally {
  await browser.close ();
  server.close ();
}
process.exit (ok ? 0 : 1);
