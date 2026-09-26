// Run the browser build in a headless browser, for tests without a display.
//
//   node misc/wasm/browser-run.mjs [options]
//
//   --dir <path>       the build (default build-wasm/out/web)
//   --out <path>       where the screenshots go (default build-wasm/shots)
//   --shot <ms>:<name> a screenshot <name>.png that many ms after the page
//                      has loaded (several may be given)
//   --size <w>x<h>     the size of the page (default 1280x800)
//   --browser <path>   the browser (default: the Firefox of /Applications)
//   --query <string>   appended to the url of the page
//   --port <n>         the port of the server (the IndexedDB of a page
//                      belongs to its origin: keep it with --profile)
//   --profile <dir>    the profile of the browser, kept between runs (the
//                      IndexedDB of the page: the home directory of TeXmacs)
//   --script <file>    actions after the load, one per line (# comments):
//                        wait <ms> | shot <name> | click <x> <y> |
//                        move <x> <y> | type <text> | key <name> |
//                        upload <x> <y> <file> (the click opens the file
//                        input of the page, which is given the file) |
//                        uploadto <selector> <file> (to a file input) |
//                        answer <text> (the next prompt of the page) |
//                        eval <js> (printed)
//                      (names of keys as in puppeteer: Enter, Backspace,
//                      Tab, ArrowDown, Meta+KeyS...; coordinates in CSS
//                      pixels of the page)
//
// The console of the page is printed with a "page:" prefix. puppeteer-core is
// looked for in build-wasm/tools (npm install puppeteer-core there).

import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';
import { createRequire } from 'node:module';

const args = process.argv.slice (2);
const opt = (name, dflt) => {
  const i = args.indexOf (name);
  return i >= 0 ? args[i + 1] : dflt;
};
const dir = path.resolve (opt ('--dir', 'build-wasm/out/web'));
const out = path.resolve (opt ('--out', 'build-wasm/shots'));
const [W, H] = opt ('--size', '1280x800').split ('x').map (Number);
const browserPath = opt ('--browser', '/Applications/Firefox.app/Contents/MacOS/firefox');
const query = opt ('--query', '');
const shots = [];
args.forEach ((a, i) => {
  if (a === '--shot') {
    const [ms, name] = args[i + 1].split (':');
    shots.push ({ ms: Number (ms), name });
  }
});
if (shots.length === 0) shots.push ({ ms: 20000, name: 'page' });
fs.mkdirSync (out, { recursive: true });

const require = createRequire (path.resolve ('build-wasm/tools/package.json'));
const puppeteer = require ('puppeteer-core');

const types = { '.html': 'text/html', '.js': 'text/javascript',
                '.wasm': 'application/wasm', '.data': 'application/octet-stream' };
const server = http.createServer ((req, res) => {
  const file = path.join (dir, decodeURIComponent (req.url.split ('?')[0]));
  fs.readFile (file, (err, data) => {
    if (err) { res.writeHead (404); res.end (); return; }
    res.writeHead (200, { 'Content-Type': types[path.extname (file)] || 'application/octet-stream' });
    res.end (data);
  });
});
await new Promise (ok => server.listen (Number (opt ('--port', '0')), '127.0.0.1', ok));
const url = `http://127.0.0.1:${server.address ().port}/texmacs.html${query}`;

const profile = opt ('--profile', null);
if (profile) fs.mkdirSync (profile, { recursive: true });
const browser = await puppeteer.launch ({
  browser: 'firefox', executablePath: browserPath, headless: true,
  ...(profile ? { userDataDir: path.resolve (profile) } : {}),
  args: [`--width=${W}`, `--height=${H}`]
});
const page = await browser.newPage ();
await page.setViewport ({ width: W, height: H });
page.on ('console', msg => console.log ('page:', msg.text ()));
page.on ('pageerror', err => console.log ('page error:', err.message));
let answer = null; // the answer to the next prompt of the page (see "answer")
page.on ('dialog', async d => {
  console.log (`dialog: ${d.type ()} "${d.message ()}" -> ${answer}`);
  if (answer !== null) await d.accept (answer); else await d.dismiss ();
  answer = null;
});
const t0 = Date.now ();
await page.goto (url, { waitUntil: 'load', timeout: 120000 });
console.log (`loaded in ${Date.now () - t0} ms`);
const t1 = Date.now ();
const script = opt ('--script', null);
if (script) {
  for (let line of fs.readFileSync (script, 'utf8').split ('\n')) {
    line = line.trim ();
    if (line === '' || line.startsWith ('#')) continue;
    const [cmd, ...a] = line.split (' ');
    console.log (`script: ${line}`);
    if (cmd === 'wait') await new Promise (ok => setTimeout (ok, Number (a[0])));
    else if (cmd === 'shot') {
      await page.screenshot ({ path: path.join (out, a[0] + '.png') });
    }
    else if (cmd === 'click') await page.mouse.click (Number (a[0]), Number (a[1]));
    else if (cmd === 'move') await page.mouse.move (Number (a[0]), Number (a[1]));
    else if (cmd === 'uploadto') {
      // give the file a[1] to the file input a[0] (a CSS selector)
      const el = await page.waitForSelector (a[0], { timeout: 15000 });
      await el.uploadFile (path.resolve (a[1]));
    }
    else if (cmd === 'answer') answer = line.slice (7);
    else if (cmd === 'eval') {
      try { console.log ('eval:', JSON.stringify (await page.evaluate (line.slice (5)))); }
      catch (e) { console.log ('eval error:', e.message); }
    }
    else if (cmd === 'upload') {
      // click (a[0], a[1]), which opens the file input of the page, and
      // choose the file a[2] in it
      const [chooser] = await Promise.all ([
        page.waitForFileChooser ({ timeout: 15000 }),
        page.mouse.click (Number (a[0]), Number (a[1]))]);
      await chooser.accept ([path.resolve (a[2])]);
    }
    else if (cmd === 'type') await page.keyboard.type (line.slice (5), { delay: 30 });
    else if (cmd === 'key') {
      const keys = a[0].split ('+');
      for (const k of keys) await page.keyboard.down (k);
      for (const k of keys.reverse ()) await page.keyboard.up (k);
    }
    else console.log (`script: unknown command ${cmd}`);
  }
}
else for (const s of shots.sort ((a, b) => a.ms - b.ms)) {
  const wait = s.ms - (Date.now () - t1);
  if (wait > 0) await new Promise (ok => setTimeout (ok, wait));
  const file = path.join (out, s.name + '.png');
  await page.screenshot ({ path: file });
  console.log (`screenshot ${file} at ${Date.now () - t1} ms`);
}
await browser.close ();
server.close ();
