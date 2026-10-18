// The page drawing with WebGPU (?gpu=webgpu) in Safari, driven by its
// WebDriver (safaridriver; "Allow Remote Automation" in the Develop menu of
// Safari, or safaridriver --enable once): its window is on the screen.
//
//   node misc/wasm/test/safari-webgpu.mjs [web dir] [screenshot dir] [query]
//
// (default build-wasm/out/web, build-wasm/shots, ?gpu=webgpu) TeXmacs
// starts, the File menu is opened, text is typed in the empty document;
// screenshots of each step. Prints how the page draws and the errors of
// WebGPU (window.tmWebGPUErrors, web-pre.js); exit status 0 when it drew
// with WebGPU without error.

import fs from 'node:fs';
import path from 'node:path';
import { spawn } from 'node:child_process';
import { serve } from '../serve.mjs';

const dir = path.resolve (process.argv[2] || 'build-wasm/out/web');
const out = path.resolve (process.argv[3] || 'build-wasm/shots');
const query = process.argv[4] || '?gpu=webgpu';
fs.mkdirSync (out, { recursive: true });
const sleep = ms => new Promise (r => setTimeout (r, ms));

const server = await serve (dir, 0, '127.0.0.1');
const url = `http://127.0.0.1:${server.address ().port}/texmacs.html${query}`;
const port = 4400 + Math.floor (Math.random () * 500);
const driver = spawn ('safaridriver', ['-p', String (port)], { stdio: 'ignore' });
const base = `http://127.0.0.1:${port}`;
async function call (method, p, body) {
  const r = await fetch (base + p, { method, headers: { 'Content-Type': 'application/json' },
                                     body: body ? JSON.stringify (body) : undefined });
  const j = await r.json ();
  if (j.value && j.value.error) throw new Error (j.value.error + ': ' + j.value.message);
  return j.value;
}
let sid = null;
for (let i = 0; ; i++) {
  try { sid = (await call ('POST', '/session', { capabilities: { alwaysMatch: { browserName: 'safari' } } })).sessionId; break; }
  catch (e) { if (i > 20) throw e; await sleep (500); }
}
const S = '/session/' + sid;
const evaluate = async (js) => {
  const r = await call ('POST', S + '/execute/sync', { script: 'return (' + js + ');', args: [] });
  return r;
};
const shot = async (name) => {
  const png = await call ('GET', S + '/screenshot');
  fs.writeFileSync (path.join (out, name + '.png'), Buffer.from (png, 'base64'));
};
// the pointer and the keys (coordinates in CSS pixels of the page)
const click = (x, y) => call ('POST', S + '/actions', { actions: [{ type: 'pointer', id: 'mouse',
  parameters: { pointerType: 'mouse' }, actions: [
    { type: 'pointerMove', origin: 'viewport', x, y }, { type: 'pointerDown', button: 0 },
    { type: 'pointerUp', button: 0 }] }] });
const type = (text) => call ('POST', S + '/actions', { actions: [{ type: 'key', id: 'keyboard',
  actions: [...text].flatMap (c => [{ type: 'keyDown', value: c }, { type: 'keyUp', value: c }]) }] });

let ok = false;
try {
  await call ('POST', S + '/timeouts', { script: 120000, pageLoad: 120000 });
  await call ('POST', S + '/window/rect', { width: 1280, height: 860 });
  const t0 = Date.now ();
  await call ('POST', S + '/url', { url });
  for (;;) {
    if (await evaluate ("typeof tmProgress !== 'undefined' && tmProgress.running")) break;
    if (Date.now () - t0 > 180000) throw new Error ('TeXmacs did not start');
    await sleep (500);
  }
  await sleep (4000);
  console.log (`running after ${Date.now () - t0} ms`);
  const how = await evaluate (`(function () {
    var c = document.getElementById ('canvas');
    return { webgpu: !!window.tmWebGPU, gpu: typeof navigator.gpu,
             agent: navigator.userAgent }; }) ()`);
  console.log ('drawn with WebGPU: ' + how.webgpu + ' (navigator.gpu: ' + how.gpu + ')');
  console.log (how.agent);
  await shot ('safari-start');
  await click (264, 16);           // the File menu
  await sleep (1500);
  await shot ('safari-menu');
  await type ('');           // Escape
  await sleep (500);
  await click (60, 68);            // the tab of the empty document
  await sleep (1500);
  await click (700, 250);
  await sleep (500);
  await type ('Hello from WebGPU in Safari');
  await sleep (2000);
  await shot ('safari-typed');
  const errors = await evaluate ('window.tmWebGPUErrors || []');
  console.log (`errors of WebGPU: ${errors.length}`);
  for (const e of errors.slice (0, 10)) console.log ('  ' + e.split ('\n')[0]);
  console.log ('title: ' + await evaluate ('document.title'));
  ok = how.webgpu && errors.length === 0;
} finally {
  try { await call ('DELETE', S); } catch (e) {}
  driver.kill ();
  server.close ();
}
process.exit (ok ? 0 : 1);
