// The stream display of the WASIX build in the Wasmer SDK under node (the
// runtime of the browser): as stream-wasmer.mjs, TeXmacs spawned in a
// sandbox with its standard input and output piped.
//
//   node misc/wasix/test/stream-sdk.mjs <build dir> <output dir>

import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
import { EventEmitter } from 'node:events';
import { StreamClient } from '../stream-client.mjs';

const [build, outDir] = process.argv.slice (2);
if (!outDir) { console.error ('usage: stream-sdk.mjs <build dir> <output dir>'); process.exit (2); }
fs.mkdirSync (outDir, { recursive: true });
const { Wasmer } = await import (pathToFileURL (path.resolve (build, 'sdk/node_modules/@wasmer/sdk/dist/node.js')));
const wasm = fs.readFileSync (path.resolve (build, 'out/texmacs.wasm'));
const blob = fs.readFileSync (path.resolve (build, 'out/texmacs-fs.bin'));
const index = JSON.parse (fs.readFileSync (path.resolve (build, 'out/texmacs-fs.json'), 'utf8'));
const files = {};
for (const [p, o, n] of index) files[p] = blob.subarray (o, o + n);

const t0 = Date.now ();
const wasmer = new Wasmer ({ cache: false });
const pkg = await wasmer.packages.create ({ modules: { texmacs: wasm },
  commands: { texmacs: { module: 'texmacs' } }, files });
const sandbox = await wasmer.sandboxes.create ({ packages: [pkg],
  env: { TEXMACS_PATH: '/texmacs', TEXMACS_HOME_PATH: '/workspace/home',
         HOME: '/workspace/home', TEXMACS_DISPLAY: 'stream' } });
const proc = await sandbox.command (pkg, [], { cwd: '/workspace' })
  .spawn ({ stdin: 'pipe', stdout: 'pipe', stderr: 'pipe' });
const out = new EventEmitter ();
(async () => { for await (const d of proc.stdout) out.emit ('data', Buffer.from (d)); }) ();
const log = fs.createWriteStream (path.join (outDir, 'stderr.log'));
(async () => { for await (const d of proc.stderr) log.write (d); }) ();
const c = new StreamClient (out, b => proc.stdin.write (new Uint8Array (b)));
let last = Date.now ();
c.onFrame = () => { last = Date.now (); };
c.resize (1000, 700);
c.focus ();

const sleep = ms => new Promise (r => setTimeout (r, ms));
async function settle (name, quiet = 1500, max = 120000) {
  const start = Date.now ();
  last = Math.max (last, start);
  while (Date.now () - last < quiet || c.frames === 0) {
    if (Date.now () - start > max) break;
    await sleep (100);
  }
  if (c.pixels) fs.writeFileSync (path.join (outDir, name + '.png'), c.png ());
  console.log (`${name}: ${c.width}x${c.height}, ${c.frames} frames, ` +
               `${(c.bytes / 1e6).toFixed (1)} MB, title "${c.title}", ${Date.now () - t0} ms`);
}
try {
  await settle ('start');
  c.click (28, 16);
  await settle ('menu');
  c.click (40, 40);
  await settle ('new');
  c.text ('Hello from the SDK');
  await settle ('typed');
} finally {
  await proc.kill ();
  await sandbox.close ();
  await wasmer.close ();
}
