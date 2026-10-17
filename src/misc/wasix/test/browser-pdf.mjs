// The page of the WASIX build in a headless Chrome: it loads, TeXmacs runs
// in the Wasmer SDK and typesets the sample document to PDF.
//
//   node misc/wasix/test/browser-pdf.mjs <tools dir> [web dir] [screenshot.png]
//
// <tools dir>: a directory with puppeteer-core in node_modules and Chrome for
// Testing in chrome/ (that of the Emscripten build, misc/wasm/browser-run.mjs).
// Exit status 0 when a PDF came out.

import fs from 'node:fs';
import path from 'node:path';
import { createRequire } from 'node:module';
import { serve } from '../serve.mjs';

const [tools, webArg, shot] = process.argv.slice (2);
if (!tools) { console.error ('usage: browser-pdf.mjs <tools dir> [web dir] [shot.png]'); process.exit (2); }
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
  executablePath: findChrome (path.join (tools, 'chrome')),
  headless: true,
  args: ['--no-first-run'] });
let ok = false;
try {
  const page = await browser.newPage ();
  page.on ('console', m => console.log ('[page]', m.text ()));
  page.on ('pageerror', e => console.log ('[page error]', e.message));
  const t0 = Date.now ();
  await page.goto (`http://127.0.0.1:${port}/pdf.html`);
  console.log ('isolated:', await page.evaluate (() => crossOriginIsolated));
  await page.waitForFunction (() => window.tmWasix && window.tmWasix.ready,
                              { timeout: 300000, polling: 200 });
  console.log (`ready after ${Date.now () - t0} ms`);
  const first = await page.evaluate (() => window.tmWasix.typeset ());
  console.log ('first run:', JSON.stringify (first));
  const second = await page.evaluate (() => window.tmWasix.typeset ());
  console.log ('second run:', JSON.stringify (second));
  console.log ('log:\n' + await page.evaluate (() => document.getElementById ('log').textContent));
  if (shot) await page.screenshot ({ path: shot });
  ok = !!(second.bytes > 0);
} finally {
  await browser.close ();
  server.close ();
}
process.exit (ok ? 0 : 1);
