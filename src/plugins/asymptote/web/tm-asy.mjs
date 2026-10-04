// The Asymptote plugin in the browser: the worker of its sessions
// (plugins/asymptote, see docs/wasm/asymptote.md).
//
// A plugin of the page is a Web Worker (src/System/Link/worker_link.cpp,
// misc/wasm/workers.js), here a module worker (its name ends in .mjs): it
// reads its input from {input} messages and writes its output in {out} and
// {err} messages, in the protocol of the plugins. It runs Asymptote, compiled
// to WebAssembly by Asymptote-web (asymptote.js, asymptote.wasm, asy.data,
// next to this file), on each input, and answers with the picture.
//
// Asymptote-web has no TeX: Asymptote draws its labels with a font of its
// own, from their source ("$x$" with its dollars). The labels are therefore
// set by TeXmacs: the function of Asymptote which draws the text of a label
// (Label.label, plain_Label.asy) is changed, in the files of Asymptote, to
// draw it invisibly (for the size of the picture), to put a marker at its
// anchor (a tiny triangle whose colour is its number), and to write its
// LaTeX, its alignment, its size and its colour in a file. In the SVG of the
// picture, the markers give the places of the labels, whatever the picture
// went through; they are taken away, and the labels come as TeXmacs text
// over the image, aligned on their anchors as Asymptote aligns them
// (text-at of a graphics, its halign and valign). A scaled label is set at
// its scale; a rotated one is set upright (the rotation of TeXmacs is not
// right in the renderer of the browser yet), which beats the font of
// Asymptote-web; only a label squashed to nothing stays Asymptote's.

import factory from './asymptote.js';
import { epsToSvg } from './asymptote-web.js';
import { ASYMPTOTE_VERSION, ASYMPTOTE_WEB_VERSION } from './version.js';

const B = '\x02', E = '\x05';
const PROMPT = B + 'prompt#asy] ' + E;
const decoder = new TextDecoder ();
const BP = 72.27 / 72;            // pt per bp
const MARK = 19;                  // the red of the markers (0-255)
// the file of the labels: in the directory of the run, the only one where
// Asymptote writes (Asymptote runs in /w)
const LABELS_NAME = 'tm-labels.txt';
const LABELS = '/w/' + LABELS_NAME;

function out (s) { postMessage ({ out: s }); }
function err (s) { postMessage ({ err: s }); }
function schemeString (s) {
  return '"' + String (s).replace (/\\/g, '\\\\').replace (/"/g, '\\"') + '"';
}
function num (x) { return String (Math.round (x * 1000) / 1000); }

/******************************************************************************
* Asymptote
******************************************************************************/

let asyModule = null;      // the Emscripten module, once loaded
let printed = [];       // what Asymptote writes, for the current run
let labelsPatched = false;

async function asymptote () {
  if (asyModule) return asyModule;
  asyModule = await factory ({
    // the glue asks for asy.wasm, the package has asymptote.wasm
    locateFile: (f, prefix) => (prefix || '') + (f === 'asy.wasm' ? 'asymptote.wasm' : f),
    print: (s) => printed.push (s),
    printErr: (s) => printed.push (s)
  });
  patchLabels (asyModule);
  return asyModule;
}

// the labels of Asymptote, set by TeXmacs (see above)
const LABEL_FILE = '/usr/local/share/asymptote/plain_Label.asy';
const LABEL_FILL = 'fill(f,align(texpath(s,p0),S,align,p0),p0);';
// the declarations before plain_Label.asy: the counter and the file of the
// labels, and the box of a label for the size of the picture: its size as
// TeXmacs will set it (estimated by the worker, sizeTable), else the size
// of the text in the font of Asymptote-web (texpath)
function labelDecls (keys, sizes) {
  const str = (x) => '"' + String (x).replace (/"/g, '\\"') + '"';
  return '// TeXmacs: the labels of the picture, set by TeXmacs (tm-asy.mjs)\n' +
    'int tmLabelIndex=0;\n' +
    'file tmLabelOut=output("' + LABELS_NAME + '");\n' +
    'string[] tmSizeKeys={' + keys.map (str).join (',') + '};\n' +
    'real[][] tmSizes={' + sizes.map ((z) => '{' + z.map (num).join (',') + '}').join (',') + '};\n' +
    '// (none for a label of which it knows nothing: texpath, below)\n' +
    'path[] tmBox(string s, pen p) {\n' +
    '  real fs=fontsize(p);\n' +
    '  for(int i=0; i < tmSizeKeys.length; ++i)\n' +
    '    if(tmSizeKeys[i] == s) {\n' +
    '      real w=tmSizes[i][0]*fs, h=tmSizes[i][1]*fs, d=tmSizes[i][2]*fs;\n' +
    '      return new path[] {(0,-d)--(w,-d)--(w,h)--(0,h)--cycle};\n' +
    '    }\n' +
    '  return new path[];\n' +
    '}\n\n';
}
const LABEL_TEXMACS =
  '{\n' +
  '        transform tmR=embed(t)*shiftless(T);\n' +
  '        real tmScale=sqrt(abs(tmR.xx*tmR.yy-tmR.xy*tmR.yx));\n' +
  '        if(tmScale < 1e-6)\n' +
  '          ' + LABEL_FILL + '\n' +
  '        else {\n' +
  '          path[] tmG=tmBox(s,p0);\n' +
  '          if(tmG.length == 0) tmG=texpath(s,p0);\n' +
  '          fill(f,align(tmG,S,align,p0),invisible);\n' +
  '          ++tmLabelIndex;\n' +
  '          fill(f,S--(S+(0.01,0))--(S+(0,0.01))--cycle,\n' +
  '               rgb(' + MARK + '/255,quotient(tmLabelIndex,256)/255,(tmLabelIndex%256)/255));\n' +
  '          real[] tmC=colors(rgb(p0));\n' +
  '          // single quotes: Asymptote turns their \\t into a tab\n' +
  '          write(tmLabelOut,string(tmLabelIndex)+\'\\t\'+string(align.x)+\'\\t\'+string(align.y)+\'\\t\'+\n' +
  '                string(fontsize(p0)*tmScale)+\'\\t\'+string(tmC[0])+" "+string(tmC[1])+" "+string(tmC[2])+\'\\t\'+\n' +
  '                replace(s,\'\\n\',\' \'),endl);\n' +
  '          flush(tmLabelOut);\n' +
  '        }\n' +
  '      }';

let labelSource = null;   // plain_Label.asy as it came, with our label code

function patchLabels (m) {
  let src;
  try { src = m.FS.readFile (LABEL_FILE, { encoding: 'utf8' }); } catch (e) { return; }
  if (src.indexOf (LABEL_FILL) < 0) {
    console.warn ('tm-asy: plain_Label.asy has changed, the labels stay Asymptote\'s');
    return;
  }
  labelSource = src.replace (LABEL_FILL, LABEL_TEXMACS);
  writeLabels (m, [], []);
  labelsPatched = true;
}

// plain_Label.asy for the next run, with the sizes of these labels
function writeLabels (m, keys, sizes) {
  if (labelSource !== null) m.FS.writeFile (LABEL_FILE, labelDecls (keys, sizes) + labelSource);
}

// the labels written by Asymptote: number -> {ax, ay, size, color, latex}
function readLabels (m) {
  let text = '';
  try { text = m.FS.readFile (LABELS, { encoding: 'utf8' }); } catch (e) { return {}; }
  const r = {};
  for (const line of text.split ('\n')) {
    const f = line.split ('\t');
    if (f.length < 6) continue;
    const c = f[4].split (' ').map (Number);
    r[f[0]] = { ax: +f[1], ay: +f[2], size: +f[3], color: c, latex: f.slice (5).join ('\t') };
  }
  return r;
}

/******************************************************************************
* The size of a label, as TeXmacs will set it (estimated from its LaTeX)
******************************************************************************/

// width, height and depth in em: a rough reading of the LaTeX, enough for
// Asymptote to make room for the labels and to space its ticks (with the
// text in its own font, a label was made two or three times too wide)
const FUNCS = /^(sin|cos|tan|cot|sec|csc|log|ln|exp|lim|max|min|sup|inf|det|arg|deg|dim|gcd|ker|sinh|cosh|tanh)$/;
const BIG_OPS = /^(sum|prod|int|oint|bigcup|bigcap)$/;
const RELS = /^(le|leq|ge|geq|ne|neq|approx|sim|simeq|equiv|in|notin|subset|subseteq|to|mapsto|rightarrow|leftarrow|Rightarrow|iff|cdot|times|pm|mp|cup|cap|circ|wedge|vee|setminus)$/;

function sizeOf (latex) {
  let w = 0, h = 0.7, d = 0.2, math = false, i = 0;
  const n = latex.length;
  // a group {...} or one token, from i: its text
  function group () {
    while (i < n && latex[i] === ' ') i++;
    if (latex[i] === '{') {
      let depth = 0, j = i;
      for (; j < n; j++) {
        if (latex[j] === '{') depth++;
        else if (latex[j] === '}' && --depth === 0) break;
      }
      const g = latex.slice (i + 1, j);
      i = j + 1;
      return g;
    }
    if (latex[i] === '\\') {
      let j = i + 1;
      while (j < n && /[A-Za-z]/.test (latex[j])) j++;
      if (j === i + 1) j++;
      const g = latex.slice (i, j);
      i = j;
      return g;
    }
    return latex[i++] || '';
  }
  const inner = (g, k) => { const z = sizeOf ((math ? '$' : '') + g + (math ? '$' : '')); return { w: z.w * k, h: z.h * k, d: z.d * k }; };
  while (i < n) {
    const c = latex[i];
    if (c === '$') { math = !math; i++; continue; }
    if (c === '{' || c === '}') { i++; continue; }
    if (c === '^' || c === '_') {
      i++;
      const z = inner (group (), 0.7);
      w += z.w;
      if (c === '^') h = Math.max (h, 0.45 + z.h); else d = Math.max (d, 0.2 + z.d);
      continue;
    }
    if (c === '\\') {
      i++;
      let j = i;
      while (j < n && /[A-Za-z]/.test (latex[j])) j++;
      const cmd = latex.slice (i, j || i + 1);
      i = j > i ? j : i + 1;
      if (cmd === 'frac' || cmd === 'dfrac' || cmd === 'tfrac') {
        const k = math && cmd !== 'dfrac' ? 0.7 : 1;
        const a = inner (group (), k), b = inner (group (), k);
        w += Math.max (a.w, b.w) + 0.2;
        h = Math.max (h, 0.3 + a.h + a.d);
        d = Math.max (d, 0.2 + b.h + b.d);
      }
      else if (cmd === 'sqrt') { const a = inner (group (), 1); w += a.w + 0.8; h = Math.max (h, a.h + 0.15); }
      else if (/^(text|mathrm|mathbf|mathit|operatorname|textbf|textit|emph|mathsf|mathtt)$/.test (cmd)) {
        const a = inner (group (), 1); w += a.w;
      }
      else if (/^(left|right|big|Big|bigl|bigr|displaystyle|textstyle|scriptstyle|mathbb|mathcal|mathfrak)$/.test (cmd)) {}
      else if (FUNCS.test (cmd)) w += 0.5 * cmd.length + 0.15;
      else if (BIG_OPS.test (cmd)) { w += 1.0; h = Math.max (h, 0.9); d = Math.max (d, 0.4); }
      else if (RELS.test (cmd)) w += 0.95;
      else if (cmd === ',' || cmd === ';' || cmd === ':' || cmd === ' ') w += 0.25;
      else if (cmd === 'quad') w += 1; else if (cmd === 'qquad') w += 2;
      else w += 0.6;                     // a Greek letter or a symbol
      continue;
    }
    i++;
    if (c === ' ') w += math ? 0 : 0.33;
    else if (math && '=<>'.indexOf (c) >= 0) w += 0.95;
    else if (math && '+-*'.indexOf (c) >= 0) w += 0.95;
    else if (',;.:!'.indexOf (c) >= 0) w += 0.28;
    else if ('()[]|/'.indexOf (c) >= 0) w += 0.39;
    else if (/[A-Z]/.test (c)) w += 0.72;
    else if (/[mwMW]/.test (c)) w += 0.83;
    else if (/[ijlt1fr]/.test (c) && !math) w += 0.3;
    else w += 0.5;
  }
  return { w, h, d };
}

/******************************************************************************
* The options of a picture: a first line "% -width 300 -height 200", as in
* the plugin of the desktop (tmpy/graph/graph.py), taken out of the code
******************************************************************************/

// a length of the options in bp: a number alone is in pixels, as on the
// desktop; null for what is not a length
const UNITS = { px: 0.75, pt: 72 / 72.27, bp: 1, mm: 72 / 25.4, cm: 72 / 2.54, in: 72 };
function lengthBp (v) {
  const m = /^\s*([0-9]*\.?[0-9]+)\s*([a-z]*)\s*$/.exec (v || '');
  if (!m) return null;
  const u = m[2] || 'px';
  return u in UNITS ? parseFloat (m[1]) * UNITS[u] : null;
}

function options (code) {
  if (!/^\s*%/.test (code)) return { code, width: null, height: null };
  const nl = code.indexOf ('\n');
  const line = nl < 0 ? code : code.slice (0, nl);
  const args = line.replace (/^\s*%/, '').trim ().split (/\s+/);
  const r = { code: nl < 0 ? '' : code.slice (nl + 1), width: null, height: null };
  for (let i = 0; i + 1 < args.length; i += 2) {
    if (args[i] === '-width') r.width = lengthBp (args[i + 1]);
    else if (args[i] === '-height') r.height = lengthBp (args[i + 1]);
    // -output: the picture is always SVG here
  }
  return r;
}

/******************************************************************************
* The picture
******************************************************************************/

// the SVG without the markers of the labels, and their places (in bp, y
// down, as the SVG)
function markers (svg) {
  const places = {};
  const re = /<path\b[^>]*\bfill="rgb\(\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*\)"[^>]*\/>/g;
  svg = svg.replace (re, function (tag, r, g, b) {
    if (+r !== MARK) return tag;
    const d = /\bd="\s*M\s*([-\d.eE+]+)[ ,]+([-\d.eE+]+)/.exec (tag);
    if (!d) return tag;
    places[(+g) * 256 + (+b)] = { x: +d[1], y: +d[2] };
    return '';
  });
  return { svg, places };
}

function hex (c) {
  const h = (v) => Math.max (0, Math.min (255, Math.round (v * 255))).toString (16).padStart (2, '0');
  return '#' + h (c[0]) + h (c[1]) + h (c[2]);
}

// where a label goes for an alignment of Asymptote: the side of its box
// on the anchor (halign, valign of text-at)
function sides (ax, ay) {
  const m = Math.max (Math.abs (ax), Math.abs (ay));
  if (m < 1e-9) return ['center', 'center'];
  const h = ax > 0.3 * m ? 'left' : ax < -0.3 * m ? 'right' : 'center';
  const v = ay > 0.3 * m ? 'bottom' : ay < -0.3 * m ? 'top' : 'center';
  return [h, v];
}

function picture (source, eps, labels, opts) {
  const conv = epsToSvg (eps);
  const m = markers (typeof conv === 'string' ? conv : conv.svg);
  const root = /<svg\b[^>]*>/.exec (m.svg);
  const attr = (n) => { const a = root && new RegExp ('\\b' + n + '="([^"]*)"').exec (root[0]); return a ? parseFloat (a[1]) : 0; };
  const w = attr ('width'), h = attr ('height');
  // the scale of the options (-width, -height): the image, and the places
  // and the sizes of its labels with it
  let kx = 1, ky = 1;
  if (opts && opts.width && w > 0) kx = opts.width / w;
  if (opts && opts.height && h > 0) ky = opts.height / h;
  if (opts && opts.width && !opts.height) ky = kx;
  if (opts && opts.height && !opts.width) kx = ky;
  const W = num (w * kx * BP) + 'pt', H = num (h * ky * BP) + 'pt';
  // the SVG at that size (its viewBox unchanged): the renderer of the browser
  // draws an image at its own size, then stretches it
  if ((kx !== 1 || ky !== 1) && root && /\bviewBox=/.test (root[0]))
    m.svg = m.svg.replace (root[0], root[0]
      .replace (/\bwidth="[^"]*"/, 'width="' + num (w * kx) + '"')
      .replace (/\bheight="[^"]*"/, 'height="' + num (h * ky) + '"'));
  const items = [];
  for (const n of Object.keys (m.places)) {
    const l = labels[n], p = m.places[n];
    if (!l) continue;
    const [ha, va] = sides (l.ax, l.ay);
    // the anchor, one label margin (0.3 em) further in the direction of the
    // alignment, where Asymptote made room for the label (align, in
    // plain_picture.asy, adds it to the place of the label once more)
    const x = p.x + l.ax * 0.3 * l.size, y = p.y - l.ay * 0.3 * l.size;
    const env = ['"mode" "text"', '"font" "roman"',
                 '"font-base-size" "' + num (l.size * Math.min (kx, ky) * BP) + '"', '"font-size" "1"'];
    if (l.color.length === 3 && (l.color[0] || l.color[1] || l.color[2]))
      env.push ('"color" "' + hex (l.color) + '"');
    const src = '(asy-latex ' + schemeString (l.latex) + ')';
    items.push ('(with "text-at-halign" "' + ha + '" "text-at-valign" "' + va + '" ' +
                '(text-at (with ' + env.join (' ') + ' (asy-label "' + n + '" ' + src + ' ' + src + ')) ' +
                '(point "' + num (x * kx * BP) + '" "' + num ((h - y) * ky * BP) + '")))');
  }
  const image = '(asy-drawing (image (tuple (raw-data ' + schemeString (m.svg) + ') "asymptote.svg") "' +
                W + '" "' + H + '" "" ""))';
  const overlay = items.length === 0 ? '' :
    ' (with "gr-mode" "text" "gr-frame" (tuple "scale" "1pt" (tuple "0gw" "0gh")) ' +
    '"gr-geometry" (tuple "geometry" "' + W + '" "' + H + '" "bottom") ' +
    '(graphics ' + items.join (' ') + '))';
  return '(asy-picture ' + schemeString (source) + ' (superpose ' + image + overlay + '))';
}

// the messages of Asymptote, with the place in the input instead of the file
function asyError (lines) {
  return lines
    .filter ((l) => !/^\s*$/.test (l) && !/could not load module/.test (l))
    .map ((l) => l.replace (/\/w\/in\.asy:\s*(\d+)\.(\d+):\s*/, 'line $1, column $2: '))
    .slice (0, 20).join ('\n');
}

/******************************************************************************
* The session
******************************************************************************/

let input = '';
let queue = Promise.resolve ();

async function evaluate (source) {
  const opts = options (source);
  const code = opts.code;
  let m;
  try { m = await asymptote (); }
  catch (e) {
    err (B + 'utf8:Asymptote could not start: ' + e + E);
    out (B + 'verbatim:' + PROMPT + E);
    return;
  }
  try { m.FS.mkdirTree ('/w'); } catch (e) {}
  m.FS.writeFile ('/w/in.asy', code + '\n');
  m.FS.chdir ('/w');
  const run = () => {
    printed = [];
    try { m.FS.unlink ('/w/out.eps'); } catch (e) {}
    try { m.FS.unlink (LABELS); } catch (e) {}
    try { return m.callMain (['-f', 'eps', '-tex', 'none', '-noV', '-o', '/w/out', '/w/in.asy']); }
    catch (e) { printed.push (String (e)); asyModule = null; return -1; }
  };
  // a first run for the labels, a second one with their sizes as TeXmacs
  // will set them, for the room they take
  writeLabels (m, [], []);
  let rc = run ();
  if (rc === 0 && labelsPatched && asyModule) {
    const labels = readLabels (m);
    const keys = [...new Set (Object.values (labels).map ((l) => l.latex))];
    if (keys.length) {
      writeLabels (m, keys, keys.map ((k) => { const z = sizeOf (k); return [z.w, z.h, z.d]; }));
      rc = run ();
    }
  }
  let eps = '';
  try { eps = m.FS.readFile ('/w/out.eps', { encoding: 'utf8' }); } catch (e) {}
  if (rc !== 0 || !eps) {
    const msg = asyError (printed) || (rc === 0 ? 'No picture: the code draws nothing.' : 'Asymptote failed.');
    err (B + 'utf8:' + msg + E);
    out (B + 'verbatim:' + PROMPT + E);
    return;
  }
  // a first line "// debug: svg": the SVG and the labels as text
  if (/^\s*\/\/\s*debug:\s*svg/.test (code)) {
    let labels = '';
    try { labels = m.FS.readFile (LABELS, { encoding: 'utf8' }); } catch (e) {}
    const conv = epsToSvg (eps);
    out (B + 'verbatim:' + B + 'utf8:' + (typeof conv === 'string' ? conv : conv.svg) +
         '\n\nlabels (' + (labelsPatched ? 'set by TeXmacs' : 'Asymptote\'s') + '):\n' + labels +
         '\nfiles: ' + m.FS.readdir ('/w').join (' ') + '\nmessages:\n' + printed.join ('\n') + E + PROMPT + E);
    return;
  }
  let answer;
  try { answer = picture (source, eps, labelsPatched ? readLabels (m) : {}, opts); }
  catch (e) {
    err (B + 'utf8:The picture could not be converted: ' + e + E);
    out (B + 'verbatim:' + PROMPT + E);
    return;
  }
  out (B + 'verbatim:' + B + 'scheme:' + answer + E + PROMPT + E);
}

onmessage = function (e) {
  const msg = e.data || {};
  if (msg.interrupt) return;   // a run of Asymptote is short, and cannot be stopped
  if (!msg.input) return;
  input += decoder.decode (msg.input, { stream: true });
  let i;
  // the serializer of the plugin ends an input by a line <EOF>
  while ((i = input.indexOf ('\n<EOF>\n')) >= 0) {
    const code = input.slice (0, i);
    input = input.slice (i + 7);
    queue = queue.then (() => evaluate (code));
  }
};

// the banner of a session: what runs Asymptote, and how to use the session
const ASYWEB_URL = 'https://github.com/Julieisbaka/Asymptote-web';
// its help page, in the help of TeXmacs (Help > Plug-ins > Asymptote)
const HELP_URL = 'tmfs://help/article/tm/plugins/asymptote/doc/asymptote-browser.en.tm';
function banner () {
  const s = schemeString;
  const line = (...a) => '(concat ' + a.join (' ') + ')';
  const tt = (x) => '(verbatim ' + s (x) + ')';
  return '(with "mode" "text" "font-family" "rm" (document ' + [
    line ('(strong ' + s ('TeXmacs interface to Asymptote, in the browser') + ')'),
    line (s ('Asymptote ' + ASYMPTOTE_VERSION + ' runs in this page (Asymptote-web ' + ASYMPTOTE_WEB_VERSION + ', '),
          '(hlink ' + s (ASYWEB_URL.replace (/^https:\/\//, '')) + ' ' + s (ASYWEB_URL) + ')',
          s ('). The first picture loads it (about 7 MB).')),
    line (s ('Type the code of a picture ('), tt ('size(5cm); draw(unitcircle);'),
          s ('), Return to make it, Shift+Return for a new line.')),
    line (s ('The labels are set by TeXmacs from their LaTeX, and can be edited.')),
    line ('(hlink ' + s ('Examples and help') + ' ' + s (HELP_URL) + ')', s (' (Help > Plug-ins > Asymptote)'))
  ].join (' ') + '))';
}

out (B + 'verbatim:' + B + 'scheme:' + banner () + E + PROMPT + E);
