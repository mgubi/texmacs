// The home directory of the browser build in IndexedDB, with several tabs
// (tmHome in misc/wasm/web-pre.js), in a headless Firefox:
//
//   node misc/wasm/test/home-tabs.mjs [src directory] [profile directory]
//
// from src/, after the "web" target of misc/wasm/Makefile (the defaults:
// the current directory, and build-wasm/home-profile, which is emptied
// first). puppeteer-core is looked for in build-wasm/tools, as by
// misc/wasm/browser-run.mjs. Each check prints PASS or FAIL.
import path from 'node:path';
import fs from 'node:fs';
import { createRequire } from 'node:module';

const SRC = path.resolve (process.argv[2] || '.');
const PROFILE = path.resolve (process.argv[3] || path.join (SRC, 'build-wasm/home-profile'));
fs.rmSync (PROFILE, { recursive: true, force: true });
fs.mkdirSync (PROFILE, { recursive: true });
const require = createRequire (path.join (SRC, 'build-wasm/tools/package.json'));
const puppeteer = require ('puppeteer-core');
const { serve } = await import (path.join (SRC, 'misc/wasm/serve.mjs'));
const server = await serve (path.join (SRC, 'build-wasm/out/web'), 0, '127.0.0.1', () => {}, 0);
const url = `http://127.0.0.1:${server.address ().port}/texmacs.html`;

let failures = 0;
function check (ok, what) {
  console.log ((ok ? 'PASS ' : 'FAIL ') + what);
  if (!ok) failures++;
}
const sleep = ms => new Promise (ok => setTimeout (ok, ms));

const browser = await puppeteer.launch ({
  browser: 'firefox', executablePath: '/Applications/Firefox.app/Contents/MacOS/firefox',
  headless: true, userDataDir: PROFILE, args: ['--width=1280', '--height=800']
});

async function open (name) {
  const page = await browser.newPage ();
  page.on ('console', m => { const t = m.text (); if (/home|TeXmacs:/.test (t)) console.log (name + ':', t); });
  page.on ('pageerror', e => console.log (name + ' error:', e.message));
  await page.goto (url, { waitUntil: 'load', timeout: 120000 });
  await ready (page);
  return page;
}

async function ready (page) {
  await page.waitForFunction (() => typeof runtimeInitialized !== 'undefined' && runtimeInitialized &&
                                    typeof tmFrame !== 'undefined' && tmFrame.tabs ().length > 0,
                              { timeout: 120000, polling: 500 });
  await sleep (3000);
}

// the keys and sizes of the database, and the tree in memory
const idb = page => page.evaluate (() => new Promise ((ok, ko) => {
  const r = indexedDB.open ('/home/web');
  r.onerror = () => ko (r.error);
  r.onsuccess = () => {
    const db = r.result, st = db.transaction (['FILE_DATA']).objectStore ('FILE_DATA');
    const out = {};
    const q = st.openCursor ();
    q.onsuccess = () => {
      const c = q.result;
      if (!c) { db.close (); ok (out); return; }
      out[c.key] = c.value.contents ? c.value.contents.length : -1;
      c.continue ();
    };
  };
}));
const mem = page => page.evaluate (() => {
  const out = {};
  (function walk (p) {
    FS.readdir (p).forEach (x => {
      if (x === '.' || x === '..') return;
      const q = p + '/' + x, st = FS.lstat (q);
      if (FS.isDir (st.mode)) { out[q] = -1; walk (q); }
      else if (FS.isLink (st.mode)) out[q] = -1;
      else out[q] = st.size;
    });
  }) ('/home/web');
  return out;
});
// the changes not yet written (a timer of 300 ms) are written first
const flushed = page => page.evaluate (() => new Promise (ok => tmHome.flush (ok)));
function same (a, b) {
  const diff = [];
  for (const k of Object.keys (a)) if (!(k in b) || a[k] !== b[k]) diff.push ('mem ' + k + ' ' + a[k] + ' / db ' + b[k]);
  for (const k of Object.keys (b)) if (!(k in a) && k !== '/home/web') diff.push ('db only ' + k);
  return diff;
}

// 1. one tab: its changes reach the database
const A = await open ('A');
check (await A.evaluate (() => !tmHome.readOnly ()), 'A writes the home directory');
await sleep (2000);
await flushed (A);
let d = same (await mem (A), await idb (A));
check (d.length === 0, 'after the start, memory and database agree' + (d.length ? ': ' + d.slice (0, 8).join ('; ') : ''));
await A.evaluate (() => {
  FS.mkdirTree ('/home/web/t1/sub');
  FS.writeFile ('/home/web/t1/sub/a.txt', 'hello');
  FS.writeFile ('/home/web/gone.txt', 'x');
});
await sleep (800);
let db = await idb (A);
check (db['/home/web/t1/sub/a.txt'] === 5 && db['/home/web/gone.txt'] === 1, 'new files are written');
await A.evaluate (() => { FS.rename ('/home/web/t1', '/home/web/t2'); FS.unlink ('/home/web/gone.txt'); });
await sleep (800);
db = await idb (A);
check (db['/home/web/t2/sub/a.txt'] === 5 && !('/home/web/t1' in db) && !('/home/web/t1/sub/a.txt' in db),
       'a renamed folder moves in the database');
check (!('/home/web/gone.txt' in db), 'a deleted file leaves the database');
await A.evaluate (() => { const s = FS.open ('/home/web/t2/sub/a.txt', 'r+'); FS.write (s, new Uint8Array ([65, 66, 67, 68, 69, 70, 71]), 0, 7, 0); FS.close (s); });
await sleep (800);
db = await idb (A);
check (db['/home/web/t2/sub/a.txt'] === 7, 'a file written through a stream is updated');
await flushed (A);
d = same (await mem (A), await idb (A));
check (d.length === 0, 'memory and database agree' + (d.length ? ': ' + d.slice (0, 8).join ('; ') : ''));

// 2. a second tab: read only
const B = await open ('B');
check (await B.evaluate (() => tmHome.readOnly ()), 'B is read only');
const noticeB = await B.evaluate (() => (document.getElementById ('tm-home-notice') || {}).textContent || '');
check (/already open in another tab/.test (noticeB), 'B says why: ' + noticeB.slice (0, 80));
check (await B.evaluate (() => FS.readFile ('/home/web/t2/sub/a.txt', { encoding: 'utf8' })) === 'ABCDEFG',
       'B reads what A wrote');
await B.evaluate (() => FS.writeFile ('/home/web/fromB.txt', 'b'));
await sleep (800);
check (!('/home/web/fromB.txt' in await idb (A)), 'B keeps none of its changes');
check (await A.evaluate (() => !tmHome.readOnly ()), 'A still writes');

// a third tab, read only too
const C = await open ('C');
check (await C.evaluate (() => tmHome.readOnly ()), 'C is read only');
// 3. B takes over: A writes its last change first
await A.evaluate (() => FS.writeFile ('/home/web/last.txt', 'last'));
const reload = B.waitForNavigation ({ waitUntil: 'load', timeout: 120000 });
await B.evaluate (() => document.querySelector ('#tm-home-notice button').click ());
await reload;
await ready (B);
check (await B.evaluate (() => !tmHome.readOnly ()), 'B writes after "Use TeXmacs here"');
check (await A.evaluate (() => tmHome.readOnly ()), 'A is read only then');
await sleep (7000);
const noticeA = await A.evaluate (() => (document.getElementById ('tm-home-notice') || {}).textContent || '');
check (/now used in another tab/.test (noticeA), 'A says why, still after 7 s: ' + noticeA.slice (0, 80));
const noticeC = await C.evaluate (() => (document.getElementById ('tm-home-notice') || {}).textContent || '');
check (/already open in another tab/.test (noticeC), 'C says nothing new during the takeover: ' + noticeC.slice (0, 80));
check (await B.evaluate (() => { try { return FS.readFile ('/home/web/last.txt', { encoding: 'utf8' }); } catch (e) { return null; } }) === 'last',
       'the last change of A reached B');
await B.bringToFront ();
await B.evaluate (() => FS.writeFile ('/home/web/fromB2.txt', 'bb'));
await sleep (800);
check ((await idb (B))['/home/web/fromB2.txt'] === 2, 'B keeps its changes now');

// 4. B is closed: A says that a reload brings TeXmacs back
await B.close ();
await sleep (8000);
const noticeC2 = await C.evaluate (() => (document.getElementById ('tm-home-notice') || {}).textContent || '');
check (/no longer open in another tab/.test (noticeC2), 'C offers to reload: ' + noticeC2.slice (0, 80));
await C.close ();
const noticeA2 = await A.evaluate (() => (document.getElementById ('tm-home-notice') || {}).textContent || '');
check (/no longer open in another tab/.test (noticeA2), 'A offers to reload: ' + noticeA2.slice (0, 80));
await A.evaluate (() => location.reload ());
await sleep (1000);
await ready (A);
check (await A.evaluate (() => !tmHome.readOnly ()), 'A writes again after the reload');
check (await A.evaluate (() => { try { return FS.readFile ('/home/web/fromB2.txt', { encoding: 'utf8' }); } catch (e) { return null; } }) === 'bb',
       'A has what B wrote');

// 5. TeXmacs itself saves a document (save-buffer-as through its own code)
await A.bringToFront ();
await A.evaluate (() => withStackSave (() => _vue_web_scheme (stringToUTF8OnStack (
  '(begin (new-document) (insert "saved by TeXmacs") (save-buffer-as (string->url "$HOME/saved-by-tm.tm") (lambda x (noop))))'))));
await sleep (3000);
db = await idb (A);
check ((db['/home/web/saved-by-tm.tm'] || 0) > 100, 'a document saved by TeXmacs is in the database (' + db['/home/web/saved-by-tm.tm'] + ' bytes)');
await flushed (A);
d = same (await mem (A), await idb (A));
check (d.length === 0, 'at the end, memory and database agree' + (d.length ? ': ' + d.slice (0, 8).join ('; ') : ''));
await browser.close ();
server.close ();
console.log (failures ? `${failures} FAILED` : 'all passed');
process.exit (failures ? 1 : 0);
