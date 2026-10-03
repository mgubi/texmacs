// The TikZ plugin in the browser: the worker of its sessions (plugins/tikz,
// see src/docs/wasm/tikzjax.md).
//
// A plugin of the page is a Web Worker (src/System/Link/worker_link.cpp,
// misc/wasm/workers.js): it reads its input from {input} messages and writes
// its output in {out} and {err} messages, in the protocol of the plugins, as
// a program does on its pipes. This one runs TikZJax (TeX in WebAssembly,
// in a worker of its own: run-tex.js, next to this file) on each input, and
// answers with the picture.

'use strict';

var B = '\x02', E = '\x05';
var PROMPT = B + 'prompt#tikz] ' + E;
var decoder = new TextDecoder ();
var base = self.location.href.replace (/[^/]*$/, ''); // the directory of TikZJax

function out (s) { postMessage ({ out: s }); }
function err (s) { postMessage ({ err: s }); }

// a Scheme string, for the "scheme:" blocks
function schemeString (s) {
  return '"' + s.replace (/\\/g, '\\\\').replace (/"/g, '\\"') + '"';
}

/******************************************************************************
* TikZJax's worker, through the protocol of threads.js: {type: "run", uid,
* method, args} to it; "init" once, then "running", "result" or "error"
******************************************************************************/

var tex = null;        // { worker, ready (a promise), calls: uid -> handlers }
var nextUid = 1;

function texWorker () {
  if (tex) return tex;
  var w = new Worker (base + 'run-tex.js');
  var t = { worker: w, calls: {} };
  t.ready = new Promise (function (resolve, reject) {
    w.onmessage = function (e) {
      var m = e.data || {};
      if (m.type === 'init') { resolve (); return; }
      var c = t.calls[m.uid];
      if (!c) return;
      if (m.type === 'result' && (m.complete || m.payload !== undefined)) {
        delete t.calls[m.uid];
        c.resolve (m.payload);
      }
      else if (m.type === 'error') {
        delete t.calls[m.uid];
        c.reject (m.error || {});
      }
    };
    w.onerror = function (e) { reject (e); };
  }).then (function () { return call (t, 'load', [base.replace (/\/$/, '')]); });
  tex = t;
  return t;
}

function call (t, method, args) {
  return new Promise (function (resolve, reject) {
    var uid = nextUid++;
    t.calls[uid] = { resolve: resolve, reject: reject };
    t.worker.postMessage ({ type: 'run', uid: uid, method: method, args: args });
  });
}

function stopTex () {
  if (!tex) return;
  tex.worker.terminate ();
  Object.keys (tex.calls).forEach (function (uid) {
    tex.calls[uid].reject ({ message: 'interrupted' });
  });
  tex = null;
}

/******************************************************************************
* The input, as tm_tikz.py takes it
******************************************************************************/

// the TikZ code of an input, and what goes in the preamble: a full LaTeX
// document is cut into its preamble and its body; code without
// tikzpicture gets one; "% packages: a, b" and "% libraries: c, d" as first
// lines ask for packages and TikZ libraries
function prepare (code) {
  var options = { texPackages: {}, tikzLibraries: '', addToPreamble: '' };
  var libs = [];
  var lines = code.split ('\n');
  while (lines.length && /^\s*%\s*(packages|libraries)\s*:/.test (lines[0])) {
    var m = /^\s*%\s*(packages|libraries)\s*:(.*)$/.exec (lines.shift ());
    var names = m[2].split (',').map (function (x) { return x.trim (); }).filter (Boolean);
    if (m[1] === 'packages') names.forEach (function (p) { options.texPackages[p] = ''; });
    else libs = libs.concat (names);
  }
  code = lines.join ('\n');
  var body = code;
  if (/^\s*\\documentclass/.test (code)) {
    var b = code.indexOf ('\\begin{document}'), e = code.lastIndexOf ('\\end{document}');
    var preamble = b >= 0 ? code.slice (0, b) : '';
    body = b >= 0 ? code.slice (b + 16, e >= 0 ? e : code.length) : '';
    preamble.split ('\n').forEach (function (l) {
      var u = /^\s*\\usepackage(\[[^\]]*\])?\{([^}]*)\}/.exec (l);
      if (/^\s*\\documentclass/.test (l)) return;
      if (u) u[2].split (',').forEach (function (p) {
        p = p.trim ();
        if (p && p !== 'tikz') options.texPackages[p] = u[1] ? u[1].slice (1, -1) : '';
      });
      else options.addToPreamble += l + '\n';
    });
  }
  else {
    // \usetikzlibrary lines before the picture go to the preamble
    var keep = [];
    body.split ('\n').forEach (function (l) {
      var u = /^\s*\\usetikzlibrary\{([^}]*)\}\s*$/.exec (l);
      if (u) libs = libs.concat (u[1].split (',').map (function (x) { return x.trim (); }));
      else keep.push (l);
    });
    body = keep.join ('\n');
    if (!/\\begin\{tikzpicture\}/.test (body))
      body = '\\begin{tikzpicture}\n' + body + '\n\\end{tikzpicture}';
  }
  options.tikzLibraries = libs.filter (Boolean).join (',');
  return { body: body, options: options };
}

/******************************************************************************
* The picture
******************************************************************************/

// the size of the SVG ("111.2pt"), from its width and height
function svgSize (svg) {
  var w = /<svg[^>]*\swidth="([^"]*)"/.exec (svg), h = /<svg[^>]*\sheight="([^"]*)"/.exec (svg);
  return { w: w ? w[1] : '', h: h ? h[1] : '' };
}

function picture (svg) {
  var size = svgSize (svg);
  // MuPDF draws "currentColor" as nothing: the colour of the text
  svg = svg.replace (/currentColor/gi, '#000000');
  return '(image (tuple (raw-data ' + schemeString (svg) + ') "tikz.svg") ' +
         schemeString (size.w) + ' ' + schemeString (size.h) + ' "" "")';
}

// the end of the log of TeX, from its first error
function texError (e) {
  var msg = (e && (e.message || e.toString && e.toString ())) || String (e);
  var i = msg.indexOf ('\n!');
  if (i >= 0) msg = msg.slice (i + 1);
  var lines = msg.split ('\n');
  if (lines.length > 20) lines = lines.slice (0, 20).concat (['...']);
  return lines.join ('\n');
}

/******************************************************************************
* The session
******************************************************************************/

var input = '';
var queue = Promise.resolve ();

function evaluate (code) {
  var p = prepare (code);
  var t = texWorker ();
  return t.ready
    .then (function () { return call (t, 'texify', [p.body, p.options]); })
    .then (function (svg) {
      out (B + 'verbatim:' + B + 'scheme:' + picture (svg) + E + PROMPT + E);
    }, function (e) {
      err (B + 'utf8:' + texError (e) + E);
      out (B + 'verbatim:' + PROMPT + E);
    });
}

onmessage = function (e) {
  var m = e.data || {};
  if (m.interrupt) { stopTex (); return; }
  if (!m.input) return;
  input += decoder.decode (m.input, { stream: true });
  var i;
  // the serializer of the plugin ends an input by a line <EOF>
  while ((i = input.indexOf ('\n<EOF>\n')) >= 0) {
    var code = input.slice (0, i);
    input = input.slice (i + 7);
    queue = queue.then (function () { return evaluate (code); });
  }
};

out (B + 'verbatim:TeXmacs interface to TikZ (TikZJax, in the browser)' + PROMPT + E);
