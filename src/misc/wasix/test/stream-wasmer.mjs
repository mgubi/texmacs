// The stream display of the WASIX build under wasmer: TeXmacs started with
// TEXMACS_DISPLAY=stream, given a size, a click in the document and some
// text; the frames are saved as PNG images.
//
//   node misc/wasix/test/stream-wasmer.mjs <output dir> [width height]

import { spawn } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { StreamClient } from '../stream-client.mjs';

const TM = path.resolve (path.dirname (fileURLToPath (import.meta.url)), '../../..');
const [outDir, W = '1000', H = '700'] = process.argv.slice (2);
if (!outDir) { console.error ('usage: stream-wasmer.mjs <output dir> [w h]'); process.exit (2); }
fs.mkdirSync (path.join (outDir, 'home'), { recursive: true });
const home = path.resolve (outDir, 'home');

const child = spawn ('wasmer', ['run',
  '--volume', `${TM}/TeXmacs:/texmacs`, '--volume', `${home}:/home`,
  '--env', 'TEXMACS_PATH=/texmacs', '--env', 'TEXMACS_HOME_PATH=/home',
  '--env', 'HOME=/home', '--env', 'TEXMACS_DISPLAY=stream',
  path.join (TM, 'build-wasix/out/texmacs.wasm')],
  { stdio: ['pipe', 'pipe', 'pipe'] });
const log = fs.createWriteStream (path.join (outDir, 'stderr.log'));
child.stderr.pipe (log);
const c = new StreamClient (child.stdout, b => child.stdin.write (b));
let last = Date.now ();
c.onFrame = () => { last = Date.now (); };
const t0 = Date.now ();
c.resize (Number (W), Number (H));
c.focus ();

const sleep = ms => new Promise (r => setTimeout (r, ms));
async function settle (name, quiet = 1500, max = 60000) {
  const start = Date.now ();
  last = Math.max (last, start); // quiet since the input just sent
  while (Date.now () - last < quiet || c.frames === 0) {
    if (Date.now () - start > max) break;
    await sleep (100);
  }
  if (c.pixels) fs.writeFileSync (path.join (outDir, name + '.png'), c.png ());
  console.log (`${name}: ${c.width}x${c.height}, ${c.frames} frames, ${c.rects} rects, ` +
               `${(c.bytes / 1e6).toFixed (1)} MB, title "${c.title}", ${Date.now () - t0} ms`);
}

try {
  await settle ('start');
  c.click (28, 16); // the File menu
  await settle ('menu');
  c.key (41, 27); // Escape
  // a new document (File > New), then text typed in it
  c.click (28, 16);
  await settle ('menu2', 800);
  c.click (40, 40);
  await settle ('new');
  c.text ('Hello from WASIX: ');
  c.text ('$');
  c.text ('x^2');
  await settle ('typed');
} finally {
  child.stdin.end ();
  await sleep (500);
  child.kill ();
}
