// The R plugin in the browser: the worker of its sessions and folds, a
// module (plugins/r, see src/docs/wasm/README.md).
//
// A plugin of the page is a Web Worker (src/System/Link/worker_link.cpp,
// misc/wasm/workers.js): it reads its input from {input} messages and writes
// its output in {out} and {err} messages, in the protocol of the plugins, as
// a program does on its pipes. This one runs R in WebAssembly (webR), loaded
// from webr.r-wasm.org by the first input. Each input is run as at the
// prompt of R (its values printed); the plots which it makes are shown as
// pictures (a PNG in an SVG, which TeXmacs draws). install.packages gets the
// packages built for webR (repo.r-wasm.org).
//
// R cannot be interrupted here (that needs a SharedArrayBuffer, which the
// page has not): while it runs, the worker says it is busy, and an
// interrupt then stops the worker (workers.js), R being started again by the
// next input.

const WEBR_VERSION = '0.6.0';
// (the build of the webR project for browsers; that of npm is for node too)
const WEBR_URL = 'https://webr.r-wasm.org/v' + WEBR_VERSION + '/';

const B = '\x02', E = '\x05';
const PROMPT = B + 'prompt#> ' + E;
const decoder = new TextDecoder ();

function out (s) { postMessage ({ out: s }); }
function err (s) { postMessage ({ err: s }); }
function busy (b) { postMessage ({ busy: b }); }

// a Scheme string, for the "scheme:" blocks
function schemeString (s) {
  return '"' + s.replace (/\\/g, '\\\\').replace (/"/g, '\\"') + '"';
}

// the text of an output: without the characters of the protocol
function verbatim (s) {
  return s.replace (/[\x02\x05\x1b]/g, '');
}

/******************************************************************************
* webR
******************************************************************************/

let webR = null;     // a promise of webR, started, with a shelter

function r () {
  if (webR) return webR;
  webR = (async function () {
    const mod = await import (WEBR_URL + 'webr.mjs');
    // (without a SharedArrayBuffer: the messages of a worker)
    const w = new mod.WebR ({ baseUrl: WEBR_URL,
                              channelType: mod.ChannelType.PostMessage });
    await w.init ();
    // install.packages as webr::install (the packages of repo.r-wasm.org)
    await w.evalRVoid ('webr::shim_install ()');
    const shelter = await new w.Shelter ();
    return { w: w, shelter: shelter };
  }) ();
  webR.catch (() => { webR = null; });
  return webR;
}

// a plot (an ImageBitmap of the canvas device of webR): a PNG in an SVG
async function plot (bitmap) {
  const canvas = new OffscreenCanvas (bitmap.width, bitmap.height);
  canvas.getContext ('2d').drawImage (bitmap, 0, 0);
  const blob = await canvas.convertToBlob ({ type: 'image/png' });
  const bytes = new Uint8Array (await blob.arrayBuffer ());
  let bin = '';
  for (let i = 0; i < bytes.length; i += 0x8000)
    bin += String.fromCharCode.apply (null, bytes.subarray (i, i + 0x8000));
  // (the canvas of webR draws at twice the size of the plot, in pixels of
  // 1/96 inch: 504 x 360, which are 378 x 270 points)
  const w = bitmap.width * 0.375, h = bitmap.height * 0.375;
  const svg = '<svg xmlns="http://www.w3.org/2000/svg" ' +
    'xmlns:xlink="http://www.w3.org/1999/xlink" width="' + w + 'pt" height="' + h +
    'pt" viewBox="0 0 ' + bitmap.width + ' ' + bitmap.height + '">' +
    '<image width="' + bitmap.width + '" height="' + bitmap.height +
    '" xlink:href="data:image/png;base64,' + btoa (bin) + '"/></svg>';
  return '(image (tuple (raw-data ' + schemeString (svg) + ') "plot.svg") "" "" "" "")';
}

/******************************************************************************
* The session
******************************************************************************/

async function evaluate (code) {
  let R;
  busy (true);
  try { R = await r (); }
  catch (e) {
    busy (false);
    err (B + 'utf8:R could not start (webR ' + WEBR_VERSION + ', from ' +
         WEBR_URL + '): ' + e + E);
    out (B + 'verbatim:' + PROMPT + E);
    return;
  }
  let text = '', error = '', images = [];
  try {
    const res = await R.shelter.captureR (code, {
      withAutoprint: true, captureStreams: true, captureConditions: false,
      captureGraphics: { width: 504, height: 360 } });
    for (const o of res.output) {
      if (o.type === 'stdout') text += o.data + '\n';
      else if (o.type === 'stderr') error += o.data + '\n';
    }
    for (const b of res.images) images.push (await plot (b));
    await R.shelter.purge ();
  }
  catch (e) {
    error += (e && e.message ? e.message : String (e)) + '\n';
  }
  busy (false);
  let answer = '';
  if (text !== '') answer += B + 'verbatim:' + verbatim (text.replace (/\n$/, '')) + E;
  for (const im of images) {
    if (answer !== '') answer += '\n';
    answer += B + 'scheme:' + im + E;
  }
  if (error !== '') err (B + 'utf8:' + verbatim (error.replace (/\n$/, '')) + E);
  out (B + 'verbatim:' + answer + PROMPT + E);
}

let input = '';
let queue = Promise.resolve ();

onmessage = function (e) {
  const msg = e.data || {};
  if (msg.interrupt) return;   // (stopped by the page while R runs)
  if (!msg.input) return;
  input += decoder.decode (msg.input, { stream: true });
  let i;
  // the serializer of the plugin, in a browser, ends an input by <EOF>
  while ((i = input.indexOf ('\n<EOF>\n')) >= 0) {
    const code = input.slice (0, i);
    input = input.slice (i + 7);
    queue = queue.then (() => evaluate (code));
  }
};

// the banner of a session: what runs R, and how to use the session
const HELP_URL = 'tmfs://help/article/tm/plugins/r/doc/r-browser.en.tm';
function banner () {
  const s = schemeString;
  const line = (...a) => '(concat ' + a.join (' ') + ')';
  const tt = (x) => '(verbatim ' + s (x) + ')';
  return '(with "mode" "text" "font-family" "rm" (document ' + [
    line ('(strong ' + s ('R in the browser') + ')'),
    line (s ('R runs in this page (webR ' + WEBR_VERSION + ', '),
          '(hlink ' + s ('webr.r-wasm.org') + ' ' + s ('https://webr.r-wasm.org') + ')',
          s ('), loaded by the first input (about 20 MB).')),
    line (s ('Plots are shown after the input which makes them; '), tt ('install.packages ("ggplot2")'),
          s (' installs the packages built for webR.')),
    line (s ('Stop ends R while it runs: its variables are then lost.')),
    line ('(hlink ' + s ('Help') + ' ' + s (HELP_URL) + ')', s (' (Help > Plug-ins > R)'))
  ].join (' ') + '))';
}

out (B + 'verbatim:' + B + 'scheme:' + banner () + E + PROMPT + E);
