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

// TeX marks the text of each node in the SVG, a group <g data-tm-node="n">
// numbered in the order it makes the nodes (dvisvgm specials, which the
// drivers of TikZJax and dvi2html speak)
var NODE_MARKERS =
  '\\newcount\\tmnode\n' +
  '\\tikzset{every node/.append style={' +
  'execute at begin node={\\global\\advance\\tmnode by 1 ' +
  '\\special{dvisvgm:raw <g data-tm-node="\\the\\tmnode">}},' +
  'execute at end node={\\special{dvisvgm:raw </g>}}}}\n';

// the nodes of a TikZ source, in the order of TeX: for each, where its text
// {...} is in the source, and its options; null for a coordinate (a node
// without text). A heuristic, checked against the nodes TeX made: a picture
// whose nodes do not match (a \foreach, label=...) keeps the runs of TeX
function scanNodes (code) {
  var nodes = [], i = 0, n = code.length;
  function blank () { while (i < n && /\s/.test (code[i])) i++; }
  function group (open, close) { // from code[i] == open, past its close
    var depth = 0;
    for (; i < n; i++) {
      var c = code[i];
      if (c === '\\') { i++; continue; }
      if (c === '%') { while (i < n && code[i] !== '\n') i++; continue; }
      if (c === open) depth++;
      else if (c === close && --depth === 0) { i++; return true; }
    }
    return false;
  }
  while (i < n) {
    var c = code[i];
    if (c === '%') { while (i < n && code[i] !== '\n') i++; continue; }
    var m = /^(\\?)(node|coordinate)(?![a-zA-Z@\/])/.exec (code.slice (i, i + 12));
    if (m && (m[1] || i === 0 || !/[a-zA-Z@\\]/.test (code[i - 1]))) {
      i += m[0].length;
      var opts = '';
      while (true) { // (name), [options], at (...), in any order
        blank ();
        if (code[i] === '(') { if (!group ('(', ')')) break; continue; }
        if (code[i] === '[') { var o = i; if (!group ('[', ']')) break; opts += code.slice (o, i); continue; }
        if (code.slice (i, i + 2) === 'at' && !/[a-zA-Z]/.test (code[i + 2] || '')) {
          i += 2; blank (); if (code[i] === '(') group ('(', ')'); continue;
        }
        break;
      }
      if (m[2] === 'coordinate') { nodes.push (null); continue; }
      if (code[i] === '{') {
        var b = i;
        if (group ('{', '}')) nodes.push ({ start: b + 1, end: i - 1, opts: opts });
        else nodes.push (null);
      }
      else nodes.push (null);
      continue;
    }
    if (c === '\\') { i += 2; continue; }
    i++;
  }
  return nodes;
}

// the TikZ code of an input, and what goes in the preamble: a full LaTeX
// document is cut into its preamble and its body; code without
// tikzpicture gets one; "% packages: a, b" and "% libraries: c, d" as first
// lines ask for packages and TikZ libraries
function prepare (code) {
  var options = { texPackages: {}, tikzLibraries: '', addToPreamble: '' };
  var libs = [];
  var lines = code.split ('\n');
  while (lines.length && /^\s*%\s*(packages|libraries|debug)\s*:/.test (lines[0])) {
    var m = /^\s*%\s*(packages|libraries|debug)\s*:(.*)$/.exec (lines.shift ());
    var names = m[2].split (',').map (function (x) { return x.trim (); }).filter (Boolean);
    if (m[1] === 'packages') names.forEach (function (p) { options.texPackages[p] = ''; });
    else if (m[1] === 'debug') options.debug = names;   // "svg": the SVG of TikZJax as text
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
  options.addToPreamble += NODE_MARKERS;
  return { body: body, options: options };
}

/******************************************************************************
* The picture
******************************************************************************/

// The picture: the SVG of TikZJax is split into its drawing, an image, and
// its text, typeset by TeXmacs over it (src/docs/wasm/tikzjax.md, 5 and 6).
// dvi2html writes each run of characters of the DVI as a <text> in a TeX
// font, under nested transforms; each run becomes TeXmacs text, placed by
// its baseline at the point where TeX put it, in the Computer Modern of
// TeXmacs at the size of TeX. What TeXmacs cannot set (cmex, a position
// without a symbol, a font it has not) stays in the SVG.

importScripts ('tables.js'); // TIKZ_GLYPHS, TIKZ_ENC (tikzjax-tables.mjs)

// the TeX fonts set as text by TeXmacs: family, series, shape
var TEXT_FONTS = {
  cmr: ['rm', 'medium', 'right'], cmb: ['rm', 'bold', 'right'],
  cmbx: ['rm', 'bold', 'right'], cmsl: ['rm', 'medium', 'slanted'],
  cmbxsl: ['rm', 'bold', 'slanted'], cmti: ['rm', 'medium', 'italic'],
  cmbxti: ['rm', 'bold', 'italic'], cmcsc: ['rm', 'medium', 'small-caps'],
  cmss: ['ss', 'medium', 'right'], cmssbx: ['ss', 'bold', 'right'],
  cmssi: ['ss', 'medium', 'italic'], cmssdc: ['ss', 'bold', 'right'],
  cmtt: ['tt', 'medium', 'right'], cmitt: ['tt', 'medium', 'italic'],
  cmsltt: ['tt', 'medium', 'slanted'], cmtcsc: ['tt', 'medium', 'small-caps']
};
// the TeX fonts set as mathematics: their encoding, bold or not
var MATH_FONTS = {
  cmmi: ['cmmi', false], cmmib: ['cmmi', true], cmsy: ['cmsy', false],
  cmbsy: ['cmsy', true], msam: ['msam', false], msbm: ['msbm', false]
};
// the positions of OT1 which TeXmacs writes otherwise: the ligatures (it
// makes them itself), the quotes
var TEXT_EXTRA = { 11: 'ff', 12: 'fi', 13: 'fl', 14: 'ffi', 15: 'ffl',
                   34: "''", 92: '``' };
var TT_EXTRA = { 13: "'", 32: ' ', 34: '"', 60: '<less>', 62: '<gtr>',
                 92: '\\', 95: '_', 123: '{', 124: '|', 125: '}', 126: '~' };

// transforms: [a, b, c, d, e, f] as in SVG (x' = a x + c y + e, ...)
function mul (m, n) {
  return [m[0]*n[0] + m[2]*n[1], m[1]*n[0] + m[3]*n[1],
          m[0]*n[2] + m[2]*n[3], m[1]*n[2] + m[3]*n[3],
          m[0]*n[4] + m[2]*n[5] + m[4], m[1]*n[4] + m[3]*n[5] + m[5]];
}
function transform (s) {
  var m = [1, 0, 0, 1, 0, 0];
  if (!s) return m;
  var re = /(\w+)\s*\(([^)]*)\)/g, t;
  while ((t = re.exec (s))) {
    var v = t[2].split (/[\s,]+/).filter (function (x) { return x !== ''; }).map (Number);
    var n = [1, 0, 0, 1, 0, 0];
    if (t[1] === 'translate') n = [1, 0, 0, 1, v[0], v[1] || 0];
    else if (t[1] === 'scale') n = [v[0], 0, 0, v.length > 1 ? v[1] : v[0], 0, 0];
    else if (t[1] === 'matrix') n = v;
    else if (t[1] === 'rotate') {
      var r = v[0] * Math.PI / 180, c = Math.cos (r), si = Math.sin (r);
      n = [c, si, -si, c, 0, 0];
      if (v.length > 2) n = mul (mul ([1, 0, 0, 1, v[1], v[2]], n), [1, 0, 0, 1, -v[1], -v[2]]);
    }
    m = mul (m, n);
  }
  return m;
}
function attr (tag, name) {
  var m = new RegExp ('\\s' + name + '="([^"]*)"').exec (tag);
  return m ? m[1] : null;
}
function decode (s) {
  return s.replace (/&#x([0-9a-f]+);/gi, function (_, h) { return String.fromCodePoint (parseInt (h, 16)); })
          .replace (/&#(\d+);/g, function (_, d) { return String.fromCodePoint (Number (d)); })
          .replace (/&lt;/g, '<').replace (/&gt;/g, '>').replace (/&quot;/g, '"')
          .replace (/&apos;/g, "'").replace (/&amp;/g, '&');
}
function num (x) { return (Math.round (x * 1000) / 1000).toString (); }

// a colour of the SVG as TeXmacs takes it; null for black, which is the
// colour of the text of the document
function color (c) {
  if (!c || /^(black|#000|#000000|currentcolor|none)$/i.test (c)) return null;
  var m = /^rgb\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)$/i.exec (c);
  if (m) return '#' + [m[1], m[2], m[3]].map (function (x) {
    return ('0' + Number (x).toString (16)).slice (-2); }).join ('');
  return c;
}

// the TeXmacs tree of a run, or null if TeXmacs cannot set it
function runTree (font, size, text, fill) {
  var f = /^([a-z]+?)(\d+)$/.exec (font);
  if (!f) return null;
  var base = f[1], codes = TIKZ_GLYPHS[font];
  if (!codes) return null;
  var tf = TEXT_FONTS[base], mf = MATH_FONTS[base];
  if (!tf && !mf) return null;
  var enc = TIKZ_ENC[tf ? 'cmr' : mf[0]];
  if (!enc) return null;
  var str = '';
  for (var ch of text) {
    var pos = codes[ch.codePointAt (0)];
    if (pos === undefined) return null;
    var sym = tf && tf[0] === 'tt' && TT_EXTRA[pos] !== undefined ? TT_EXTRA[pos]
            : tf && TEXT_EXTRA[pos] !== undefined ? TEXT_EXTRA[pos] : enc[pos];
    if (sym === undefined) return null;
    str += sym;
  }
  var env = ['"font" "roman"', '"font-base-size" "' + num (size) + '"', '"font-size" "1"'];
  var c = color (fill);
  if (c) env.push ('"color" ' + schemeString (c));
  // (the mode: the output of a session is in the mode of programs, whose
  // fonts are those of programs)
  if (tf)
    return '(with ' + env.concat (['"mode" "text"', '"font-family" "' + tf[0] + '"',
                                   '"font-series" "' + tf[1] + '"',
                                   '"font-shape" "' + tf[2] + '"']).join (' ') +
           ' ' + schemeString (str) + ')';
  env.push ('"mode" "text"', '"math-font" "roman"', '"math-level" "0"');
  if (mf[1]) env.push ('"math-font-series" "bold"');
  return '(with ' + env.join (' ') + ' (math ' + schemeString (str) + '))';
}

// the SVG without the text which TeXmacs sets, and that text as trees: the
// nodes whose source is known as labels (their LaTeX, typeset by TeXmacs),
// the other runs as runs; the source cut into its texts and the texts of
// its nodes, when they match the nodes of TeX
function split (svg, source) {
  var root = /<svg\b[^>]*>/.exec (svg)[0];
  var vb = (attr (root, 'viewBox') || '0 0 0 0').split (/[\s,]+/).map (Number);
  var w = parseFloat (attr (root, 'width')), h = parseFloat (attr (root, 'height'));
  var k = vb[2] > 0 && w > 0 ? w / vb[2] : 1; // points per unit of the SVG
  var stack = [{ m: [1, 0, 0, 1, 0, 0], fill: null, node: 0 }];
  var out = [], texts = [], nodeCount = 0;
  var re = /<(\/?)([\w:-]+)((?:[^>"']|"[^"]*"|'[^']*')*?)(\/?)>|([^<]+)/g, t;
  var text = null; // the <text> being read: its tag, its characters
  while ((t = re.exec (svg))) {
    if (t[5] !== undefined) {
      if (text) text.chars += t[5]; else out.push (t[5]);
      continue;
    }
    var closing = t[1] === '/', name = t[2], tag = t[0], self = t[4] === '/';
    var top = stack[stack.length - 1];
    if (name === 'text' && !closing) {
      text = { tag: tag, chars: '', m: mul (top.m, transform (attr (tag, 'transform'))),
               fill: attr (tag, 'fill') || top.fill, node: top.node };
      continue;
    }
    if (name === 'text' && closing && text) {
      text.end = tag;
      texts.push (place (text, vb, k));
      out.push (texts.length - 1); // decided below
      text = null;
      continue;
    }
    out.push (tag);
    if (closing) { if (stack.length > 1) stack.pop (); }
    else if (!self) {
      var nd = attr (tag, 'data-tm-node');
      if (nd) nodeCount = Math.max (nodeCount, Number (nd));
      stack.push ({ m: mul (top.m, transform (attr (tag, 'transform'))),
                    fill: attr (tag, 'fill') || top.fill,
                    node: nd ? Number (nd) : top.node });
    }
  }
  // the nodes of the source, if they are those of TeX
  var nodes = scanNodes (source);
  if (nodes.length !== nodeCount) nodes = null;
  var labels = {}, segments = null;
  if (nodes) {
    segments = [];
    var at = 0, content = 0;
    nodes.forEach (function (nd, j) {
      if (!nd) return;
      segments.push (source.slice (at, nd.start), source.slice (nd.start, nd.end));
      at = nd.end;
      content++; // the label of the n-th text of the source (coordinates have none)
      var l = label (content, j + 1, nd, source.slice (nd.start, nd.end), texts);
      if (l) labels[j + 1] = l;
    });
    segments.push (source.slice (at));
  }
  // the runs, and the SVG without what TeXmacs sets
  var runs = [];
  Object.keys (labels).forEach (function (j) { runs.push (labels[j]); });
  var svgOut = out.map (function (x) {
    if (typeof x !== 'number') return x;
    var tx = texts[x];
    if (tx.node && labels[tx.node]) return '';
    if (tx.run) { runs.push (tx.run); return ''; }
    return keep (tx) + tx.end; // left to the image
  }).join ('');
  return { svg: svgOut, runs: runs, segments: segments,
           w: attr (root, 'width'), h: attr (root, 'height') };
}

// where a <text> goes: its point and frame in the picture, in points (y
// up), and its tree as a run of TeXmacs, if it can be one
function place (text, vb, k) {
  var x = Number (attr (text.tag, 'x') || 0), y = Number (attr (text.tag, 'y') || 0);
  var m = text.m;
  var px = m[0]*x + m[2]*y + m[4], py = m[1]*x + m[3]*y + m[5];
  text.dx = (px - vb[0]) * k;
  text.dy = (vb[1] + vb[3] - py) * k;
  text.font = attr (text.tag, 'font-family') || '';
  // the frame of the text: its x axis (a, b), a rotation and a scale, no
  // mirror nor skew (y down in the SVG, y up in TeXmacs)
  var s = Math.hypot (m[0], m[1]), det = m[0]*m[3] - m[1]*m[2];
  text.straight = s !== 0 && Math.abs (det - s*s) <= 1e-3 * s*s &&
                  Math.abs (Math.atan2 (-m[1], m[0])) < 1e-4;
  text.size = Number (attr (text.tag, 'font-size') || 10) * s * k;
  // a rotated run stays in the image: TeXmacs' rotate (gr-transform) is
  // drawn mirrored and clipped by the renderer of the browser for now
  var t = text.straight ? runTree (text.font, text.size, decode (text.chars), text.fill) : null;
  text.run = t ? '(move (smash ' + t + ') "' + num (text.dx) + 'pt" "' + num (text.dy) + 'pt")' : null;
  return text;
}

// the label of a node (the n-th text of the source, the node-th node of
// TeX): its LaTeX, typeset by TeXmacs at the place of its
// text in TeX (its leftmost run, the baseline of its main runs), in the
// font of its first run of text; null for a node of several lines, a
// rotated one, or one without text
function label (n, node, nd, latex, texts) {
  var mine = texts.filter (function (t) { return t.node === node; });
  if (!mine.length || !latex.trim ()) return null;
  if (/text width|align\s*=/.test (nd.opts) || /\\\\/.test (latex)) return null;
  if (mine.some (function (t) { return !t.straight; })) return null;
  var size = Math.max.apply (null, mine.filter (function (t) { return !/^cmex/.test (t.font); })
                                     .map (function (t) { return t.size; }).concat ([0]));
  if (!size) size = mine[0].size;
  // a text which sets its own size (\tiny...) is set by TeXmacs from the
  // size of the document of TeX (10 pt), as TeX did: from the size it ended
  // with, it would be made smaller twice
  var base = /\\(tiny|scriptsize|footnotesize|small|normalsize|large|Large|LARGE|huge|Huge)\b/.test (latex)
             ? 10 : size;
  var main = mine.filter (function (t) { return Math.abs (t.size - size) < 0.01 && !/^cmex/.test (t.font); });
  if (!main.length) main = mine;
  var count = {}, dy = main[0].dy;
  main.forEach (function (t) {
    var key = num (t.dy); count[key] = (count[key] || 0) + 1;
    if (count[key] > (count[num (dy)] || 0)) dy = t.dy;
  });
  var dx = Math.min.apply (null, mine.map (function (t) { return t.dx; }));
  var f = /^([a-z]+?)(\d+)$/.exec ((mine.find (function (t) { return TEXT_FONTS[(/^([a-z]+?)\d+$/.exec (t.font) || [])[1]]; }) || {}).font || '');
  var tf = (f && TEXT_FONTS[f[1]]) || ['rm', 'medium', 'right'];
  var env = ['"mode" "text"', '"font" "roman"', '"font-family" "' + tf[0] + '"',
             '"font-series" "' + tf[1] + '"', '"font-shape" "' + tf[2] + '"',
             '"font-base-size" "' + num (base) + '"', '"font-size" "1"'];
  var c = color (mine[0].fill);
  if (c) env.push ('"color" ' + schemeString (c));
  var src = '(tikz-latex ' + schemeString (latex) + ')';
  // (not smashed: its box is what a click finds, before the image under it)
  return '(move (with ' + env.join (' ') + ' (tikz-label "' + n + '" ' + src + ' ' + src +
         ')) "' + num (dx) + 'pt" "' + num (dy) + 'pt")';
}

// a run left in the image, which TeXmacs draws as outlines (mupdf_picture.cpp,
// svg_outline_tex_text): its characters as positions in its TeX font
// (U+F000 plus the position), marked data-tm-tex
function keep (text) {
  var codes = TIKZ_GLYPHS[attr (text.tag, 'font-family') || ''];
  if (!codes) return text.tag + text.chars;
  var chars = '';
  for (var ch of decode (text.chars)) {
    var pos = codes[ch.codePointAt (0)];
    if (pos === undefined) return text.tag + text.chars;
    chars += '&#x' + (0xF000 + pos).toString (16) + ';';
  }
  return text.tag.replace (/^<text/, '<text data-tm-tex="1"') + chars;
}

function picture (source, svg) {
  var p = split (svg, source);
  var image = '(tikz-drawing (image (tuple (raw-data ' + schemeString (p.svg) + ') "tikz.svg") ' +
              schemeString (p.w || '') + ' ' + schemeString (p.h || '') + ' "" ""))';
  var src = p.segments ? '(tuple ' + p.segments.map (schemeString).join (' ') + ')'
                       : schemeString (source);
  return '(tikz-picture ' + src + ' (superpose ' + [image].concat (p.runs).join (' ') + '))';
}

// the error of TeX in its log: from its first line "! ...", the lines up to
// its prompt "?" (the help of an interactive TeX and what follows, the
// emergency stop of a TeX without terminal, are left out)
function texError (e) {
  var msg = (e && (e.message || e.toString && e.toString ())) || String (e);
  var i = msg.indexOf ('\n!');
  if (i < 0) return msg.split ('\n').slice (0, 20).join ('\n');
  var lines = msg.slice (i + 1).split ('\n'), r = [];
  for (var k = 0; k < lines.length && k < 20; k++) {
    if (/^\?/.test (lines[k])) break;
    r.push (lines[k]);
  }
  while (r.length && /^\s*$/.test (r[r.length - 1])) r.pop ();
  return r.join ('\n');
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
      if (p.options.debug && p.options.debug.indexOf ('svg') >= 0)
        out (B + 'verbatim:' + B + 'utf8:' + svg + E + PROMPT + E);
      else
        out (B + 'verbatim:' + B + 'scheme:' + picture (code, svg) + E + PROMPT + E);
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
