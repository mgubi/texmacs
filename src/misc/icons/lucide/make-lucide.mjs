#!/usr/bin/env node
// The Lucide set: icons of Lucide (ISC, https://lucide.dev) under the names
// of the icons of TeXmacs, in TeXmacs/misc/pixmaps/lucide/{light,dark}.
// Thin lines (one pixel at the size the icon is shown) and the inside of
// the closed shapes in a pastel colour, which tells the kind of the action
// (CATEGORY). The icons which have no good counterpart in Lucide (the
// mathematical symbols, the tags of the focus bar...) are taken from the
// neo-classical set, which comes next on the path (see apply_icon_set in
// src/System/Boot/init_texmacs.cpp).
//
//   npm pack lucide-static && tar xzf lucide-static-*.tgz
//   misc/icons/lucide/make-lucide.mjs package [names...]
//
// The icons are plain SVG with presentation attributes (the Qt renderer
// and MuPDF ignore style sheets). The fills are drawn first, all of them,
// and the lines over them, so that no fill covers a line.

import fs from 'node:fs';
import path from 'node:path';

const HERE = path.dirname (new URL (import.meta.url).pathname);
const PIXMAPS = path.resolve (HERE, '../../../TeXmacs/misc/pixmaps');
const OUT = path.join (PIXMAPS, 'lucide');

// the lines, and the pastel insides, which are translucent (FILL_OPACITY):
// the bar shows through them. Soft tints on the light theme; on the dark
// theme, where the lines are light, brighter pastels (the 300 tints of
// Tailwind), more transparent, dusty but still pastel over the dark bars,
// and the lines still show over them
const INK = { light: '#3F3F46', dark: '#E4E4E7' };
const FILL_OPACITY = { light: 0.7, dark: 0.55 };
const FILLS = {
  //         light      dark
  blue:   ['#BFDBFE', '#93C5FD'], // documents and files
  amber:  ['#FDE68A', '#FDE68A'], // the clipboard, editing
  violet: ['#DDD6FE', '#C4B5FD'], // searching, checking
  green:  ['#BBF7D0', '#86EFAC'], // inserting things, running
  rose:   ['#FECDD3', '#FDA4AF'], // the look of the text
  teal:   ['#99F6E4', '#5EEAD4'], // the structure of the document
  orange: ['#FED7AA', '#FDBA74'], // statements, ideas
  sky:    ['#BAE6FD', '#7DD3FC'], // help, moving around, viewing
  slate:  ['#D4D4D8', '#A1A1AA'], // settings and tools
  red:    ['#FECACA', '#FCA5A5'], // closing, deleting, stopping
};

// the margin around the drawing (in the units of the 24 x 24 drawing): the
// icons are a little smaller than their box, lighter than those of the
// other sets, which fill it
const MARGIN = 2;
// The icons of the mode bar (shown at 20 points) are drawn as large as
// those of the main bar (24 points, with the margin): the two columns of
// icons at the left of the editor match. Those of the focus bar (16) and
// of the preferences (32) keep the margin.
function margin (size) {
  if (size !== 20) return MARGIN;
  const drawn = 24 * 24 / (24 + 2 * MARGIN); // points, on the main bar
  return (24 * size / drawn - 24) / 2;
}

// TeXmacs name (without tm_) -> [Lucide name, colour]
const MAP = {
  // the main toolbar
  new: ['file-plus', 'blue'], open: ['folder-open', 'blue'], save: ['save', 'blue'],
  build: ['hammer', 'slate'], print: ['printer', 'blue'], preferences: ['settings', 'slate'],
  cancel: ['square-x', 'red'], cut: ['scissors', 'amber'], copy: ['copy', 'amber'],
  paste: ['clipboard-paste', 'amber'], find: ['search', 'violet'],
  replace: ['replace', 'violet'], spell: ['spell-check', 'violet'],
  undo: ['undo-2', 'amber'], redo: ['redo-2', 'amber'],
  back: ['arrow-left', 'sky'], reload: ['refresh-cw', 'sky'], forward: ['arrow-right', 'sky'],
  // the text toolbar
  title: ['heading', 'teal'], chapter: ['book-open', 'teal'], section: ['heading-2', 'teal'],
  theorem: ['lightbulb', 'orange'], list: ['list', 'teal'], numbered: ['list-ordered', 'teal'],
  program: ['code-xml', 'green'], footnote: ['superscript', 'teal'],
  emphasize: ['italic', 'rose'], strong: ['bold', 'rose'], verbatim: ['code', 'rose'],
  italic: ['italic', 'rose'], bold: ['bold', 'rose'], sansserif: ['type', 'rose'],
  smallcaps: ['case-upper', 'rose'], name: ['case-sensitive', 'rose'],
  color: ['palette', 'rose'], style: ['paintbrush', 'rose'], language: ['languages', 'sky'],
  table: ['table', 'green'], image: ['image', 'green'], link: ['link', 'green'],
  shell: ['square-terminal', 'green'], multicol: ['columns-2', 'teal'],
  parindent: ['list-indent-increase', 'teal'], index: ['tag', 'teal'],
  macro: ['square-code', 'green'], anchor: ['anchor', 'green'], camera: ['camera', 'green'],
  explain: ['info', 'sky'],
  block: ['notepad-text', 'teal'], prominent: ['text-quote', 'orange'],
  var_prominent: ['highlighter', 'orange'], parstyle: ['pilcrow', 'teal'],
  pageins: ['sticky-note', 'teal'], textual: ['type', 'rose'], math: ['radical', 'green'],
  switch: ['layers', 'green'], animate: ['clapperboard', 'green'],
  position_float: ['move', 'teal'], wide_float: ['move-horizontal', 'teal'],
  like: ['thumbs-up', 'orange'], theme: ['swatch-book', 'rose'],
  align_left: ['text-align-start', 'teal'], align_center: ['text-align-center', 'teal'],
  align_right: ['text-align-end', 'teal'], align_justify: ['text-align-justify', 'teal'],
  // the focus toolbar
  focus_search: ['search', 'violet'], focus_help: ['circle-question-mark', 'sky'],
  focus_prefs: ['sliders-horizontal', 'slate'], focus_delete: ['trash', 'red'],
  focus_load: ['folder-open', 'blue'], focus_save: ['save', 'blue'],
  focus_style: ['paintbrush', 'rose'], focus_font: ['type', 'rose'],
  similar_first: ['chevrons-up', 'sky'], similar_previous: ['chevron-up', 'sky'],
  similar_next: ['chevron-down', 'sky'], similar_last: ['chevrons-down', 'sky'],
  search_first: ['chevrons-left', 'sky'], search_previous: ['chevron-left', 'sky'],
  search_next: ['chevron-right', 'sky'], search_last: ['chevrons-right', 'sky'],
  exit_left: ['log-in', 'sky'], exit_right: ['log-out', 'sky'],
  show_hidden: ['eye', 'sky'], lock_closed: ['lock', 'amber'], lock_open: ['lock-open', 'amber'],
  go: ['circle-play', 'green'], stop: ['octagon-x', 'red'],
  add: ['circle-plus', 'green'], remove: ['circle-minus', 'red'],
  // elsewhere
  plus: ['plus', 'green'], minus: ['minus', 'red'], help: ['circle-question-mark', 'sky'],
  question: ['message-circle-question-mark', 'sky'], close_tool: ['x', 'red'],
  expand_tool: ['expand', 'slate'], compress_tool: ['shrink', 'slate'],
  filter: ['funnel', 'violet'], view: ['eye', 'sky'],
  prefs_general: ['settings', 'slate'], prefs_keyboard: ['keyboard', 'slate'],
  prefs_convert: ['arrow-left-right', 'blue'], prefs_security: ['shield', 'green'],
  prefs_other: ['ellipsis', 'slate'],
  cloud: ['cloud', 'sky'], cloud_download: ['cloud-download', 'sky'],
  cloud_upload: ['cloud-upload', 'sky'], cloud_server: ['server', 'slate'],
  cloud_file: ['file', 'blue'], cloud_dir: ['folder', 'blue'], cloud_mail: ['mail', 'sky'],
  cloud_chat: ['message-circle', 'sky'], cloud_share: ['share-2', 'sky'],
  cloud_home: ['house', 'sky'], cloud_admin: ['user-cog', 'slate'],
};

// the size the icon is shown at, from the directory of the classical set
// it is in (modern/24x24/main, 20x20/mode, 16x16/focus, 32x32/settings)
const SIZES = ['24x24', '20x20', '16x16', '32x32'];
function shownSize (name) {
  for (const s of SIZES) {
    const base = path.join (PIXMAPS, 'modern', s);
    if (!fs.existsSync (base)) continue;
    for (const d of fs.readdirSync (base))
      for (const ext of ['svg', 'png', 'xpm'])
        if (fs.existsSync (path.join (base, d, `tm_${name}.${ext}`)))
          return parseInt (s);
  }
  return 24;
}

// the first and the last point of a path, and its number of curves
function ends (d) {
  const tokens = d.match (/[a-zA-Z]|-?(?:\d*\.\d+|\d+\.?)(?:e-?\d+)?/g) || [];
  const ARGS = { m: 2, l: 2, h: 1, v: 1, c: 6, s: 4, q: 4, t: 2, a: 7, z: 0 };
  let x = 0, y = 0, sx = 0, sy = 0, first = null, cmd = null, curves = 0, i = 0;
  while (i < tokens.length) {
    if (/[a-zA-Z]/.test (tokens[i])) cmd = tokens[i++];
    const c = cmd.toLowerCase (), rel = (cmd !== cmd.toUpperCase ()), n = ARGS[c];
    if (c === 'z') { x = sx; y = sy; continue; }
    const v = tokens.slice (i, i + n).map (Number); i += n;
    const ox = rel ? x : 0, oy = rel ? y : 0;
    if (c === 'h') x = ox + v[0];
    else if (c === 'v') y = oy + v[0];
    else { x = ox + v[n - 2]; y = oy + v[n - 1]; }
    if ('csqta'.includes (c)) curves++;
    if (c === 'm') { sx = x; sy = y; if (!first) first = [x, y]; cmd = rel ? 'l' : 'L'; }
  }
  return { first: first || [0, 0], last: [x, y], curves };
}

// the open paths which are coloured all the same (closed by a straight line)
const FILL_OPEN = ['folder-open'];

// a shape which encloses something: its inside is coloured; a path which is
// not closed is, when it is curved and ends close to its start (the bulb of
// the lightbulb); a very small circle is a dot, drawn solid
function closed ([tag, a], lucide) {
  if (tag === 'rect' || tag === 'ellipse' || tag === 'polygon') return true;
  if (tag === 'circle') return true;
  if (tag !== 'path') return false;
  if (/z\s*$/i.test (a.d) || FILL_OPEN.includes (lucide)) return true;
  const e = ends (a.d);
  return e.curves >= 2 && Math.hypot (e.last[0] - e.first[0], e.last[1] - e.first[1]) <= 6;
}
function dot ([tag, a]) {
  return tag === 'circle' && parseFloat (a.r) <= 1.5;
}

// the geometry of an element (its own fill and stroke are replaced)
function attrs (a) {
  return Object.entries (a).filter (([k]) => k !== 'fill' && k !== 'stroke')
    .map (([k, v]) => `${k}="${v}"`).join (' ');
}

function svg (nodes, ink, fill, opacity, stroke, lucide, size) {
  // the dots are drawn solid, and large enough to be seen with thin lines
  const solid = ([tag, a]) => dot ([tag, a]) ? [tag, { ...a, r: '1.1' }] : [tag, a];
  const fills = nodes.filter (n => closed (n, lucide)).map (solid).map (([tag, a]) =>
    dot ([tag, a]) ? `  <${tag} ${attrs (a)} fill="${ink}" stroke="none"/>`
                   : `  <${tag} ${attrs (a)} fill="${fill}" fill-opacity="${opacity}" stroke="none"/>`);
  const lines = nodes.filter (n => !dot (n)).map (([tag, a]) =>
    `  <${tag} ${attrs (a)} fill="none" stroke="${ink}" stroke-width="${stroke}" ` +
    `stroke-linecap="round" stroke-linejoin="round"/>`);
  const m = +margin (size).toFixed (3), v = +(24 + 2 * m).toFixed (3);
  return '<?xml version="1.0" encoding="UTF-8"?>\n' +
    `<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="${-m} ${-m} ${v} ${v}">\n` +
    fills.concat (lines).join ('\n') + '\n</svg>\n';
}

function main () {
  const [pkg, ...only] = process.argv.slice (2);
  if (!pkg) {
    console.error ('usage: make-lucide.mjs <lucide-static directory> [names...]');
    process.exit (1);
  }
  const nodes = JSON.parse (fs.readFileSync (path.join (pkg, 'icon-nodes.json'), 'utf8'));
  let n = 0;
  for (const [name, [lucide, colour]] of Object.entries (MAP)) {
    if (only.length && !only.includes (name)) continue;
    if (!nodes[lucide]) { console.error (`${name}: no ${lucide}`); continue; }
    // one pixel on the screen: the drawing (24 + 2 MARGIN units) is shown
    // in a box of the size of the icon
    const size = shownSize (name);
    const stroke = +((24 + 2 * margin (size)) / size).toFixed (3);
    ['light', 'dark'].forEach ((theme, k) => {
      const dir = path.join (OUT, theme);
      fs.mkdirSync (dir, { recursive: true });
      fs.writeFileSync (path.join (dir, `tm_${name}.svg`),
                        svg (nodes[lucide], INK[theme], FILLS[colour][k],
                            FILL_OPACITY[theme], stroke, lucide, size));
    });
    n++;
  }
  fs.copyFileSync (path.join (pkg, 'LICENSE'), path.join (OUT, 'LICENSE'));
  console.log (`${n} icons in ${OUT}`);
}

main ();
