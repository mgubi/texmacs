#!/usr/bin/env node
// The Hugeicons set: icons of Hugeicons (the free stroke-rounded set,
// MIT, https://hugeicons.com) under the names of the icons of TeXmacs, in
// TeXmacs/misc/pixmaps/hugeicons/{light,dark}. Only the icons which have a
// good counterpart are made; the others (the letters of the text bar, the
// mathematical symbols, the tags of the focus bar...) are taken from the
// monochrome set, which comes next on the path (see apply_icon_set in
// src/System/Boot/init_texmacs.cpp).
//
//   npm pack @hugeicons/core-free-icons && tar xzf hugeicons-core-free-icons-*.tgz
//   misc/icons/hugeicons/make-hugeicons.mjs package [names...]
//
// The icons are written out as plain SVG with presentation attributes (the
// Qt renderer and MuPDF ignore style sheets), the colour of the stroke being
// that of the theme, red for removals and green for running.

import fs from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';

const HERE = path.dirname (new URL (import.meta.url).pathname);
const OUT = path.resolve (HERE, '../../../TeXmacs/misc/pixmaps/hugeicons');

const INK = { light: '#27272A', dark: '#F4F4F5' };
const TINTS = {
  red:   { light: '#DC2626', dark: '#F87171' },
  green: { light: '#16A34A', dark: '#4ADE80' },
  blue:  { light: '#2563EB', dark: '#60A5FA' },
};

// the icons of the focus bar are drawn at 16 points: a heavier line
const SMALL = 1.75, NORMAL = 1.5;

// the drawing in the middle of the box of the icon, with a margin: smaller
// and lighter than the icons of the other sets, which fill their box
const MARGIN = 2;

// TeXmacs name (without tm_) -> [Hugeicons name, tint, stroke]
const MAP = {
  // the main toolbar
  new: ['FileAdd'], open: ['FolderOpen'], save: ['FloppyDisk'],
  build: ['Hammer'], print: ['Printer'], preferences: ['Settings02'],
  cancel: ['CancelSquare'], cut: ['Scissor'], copy: ['Copy01'],
  paste: ['ClipboardPaste'], find: ['Search01'], replace: ['SearchReplace'],
  spell: ['SpellCheck'], undo: ['Undo02'], redo: ['Redo02'],
  back: ['ArrowLeft02'], reload: ['Refresh'], forward: ['ArrowRight02'],
  // the text toolbar
  title: ['Heading'], chapter: ['BookOpen01'], section: ['Heading01'],
  theorem: ['Idea01'], list: ['LeftToRightListBullet'],
  numbered: ['LeftToRightListNumber'], program: ['SourceCode'],
  italic: ['TextItalic'], bold: ['TextBold'], smallcaps: ['TextSmallcaps'],
  emphasize: ['TextItalic'], strong: ['TextBold'], verbatim: ['CodeSimple'],
  sansserif: ['TextFont'], name: ['TextSmallcaps'],
  color: ['PaintBoard'], table: ['Table01'], image: ['Image01'],
  link: ['Link01'], shell: ['ComputerTerminal01'], footnote: ['TextFootnote'],
  style: ['PaintBrush02'], language: ['Translate'], multicol: ['LayoutTwoColumn'],
  parindent: ['TextIndent'], index: ['Tag01'], macro: ['CodeSquare'],
  anchor: ['Anchor'], camera: ['Camera01'], explain: ['InformationCircle'],
  align_left: ['TextAlignLeft'], align_center: ['TextAlignCenter'],
  align_right: ['TextAlignRight'], align_justify: ['TextAlignJustifyLeft'],
  // the focus toolbar
  focus_search: ['Search01', null, SMALL], focus_help: ['HelpCircle', null, SMALL],
  focus_prefs: ['SlidersHorizontal', null, SMALL],
  focus_delete: ['Delete02', 'red', SMALL],
  focus_load: ['FolderOpen', null, SMALL], focus_save: ['FloppyDisk', null, SMALL],
  focus_style: ['PaintBrush02', null, SMALL], focus_font: ['TextFont', null, SMALL],
  similar_first: ['ArrowUpDouble', null, SMALL], similar_previous: ['ArrowUp01', null, SMALL],
  similar_next: ['ArrowDown01', null, SMALL], similar_last: ['ArrowDownDouble', null, SMALL],
  search_first: ['ArrowLeftDouble', null, SMALL], search_previous: ['ArrowLeft01', null, SMALL],
  search_next: ['ArrowRight01', null, SMALL], search_last: ['ArrowRightDouble', null, SMALL],
  exit_left: ['Login03', null, SMALL], exit_right: ['Logout03', null, SMALL],
  show_hidden: ['View', null, SMALL], lock_closed: ['SquareLock02', null, SMALL],
  lock_open: ['SquareUnlock02', null, SMALL],
  go: ['PlayCircle', 'green', SMALL], stop: ['StopCircle', 'red', SMALL],
  add: ['AddCircle', null, SMALL], remove: ['RemoveCircle', 'red', SMALL],
  // elsewhere
  plus: ['PlusSign'], minus: ['MinusSign'], help: ['HelpCircle'],
  question: ['HelpSquare'], close_tool: ['Cancel01'],
  expand_tool: ['ArrowExpand'], compress_tool: ['ArrowShrink'],
  filter: ['Filter'], view: ['View'],
  prefs_general: ['Settings01'], prefs_keyboard: ['Keyboard'],
  prefs_convert: ['ArrowDataTransferHorizontal'], prefs_security: ['SecurityLock'],
  prefs_other: ['MoreHorizontal'],
  cloud: ['Cloud'], cloud_download: ['CloudDownload'], cloud_upload: ['CloudUpload'],
  cloud_server: ['CloudServer'], cloud_file: ['File01'], cloud_dir: ['Folder01'],
  cloud_mail: ['Mail01'], cloud_chat: ['Chat01'], cloud_share: ['Share08'],
  cloud_home: ['Home01'], cloud_admin: ['UserSettings01'],
};

// React attribute names -> SVG presentation attributes
function attr (k) {
  return k.replace (/[A-Z]/g, c => '-' + c.toLowerCase ());
}

function svg (nodes, ink, stroke) {
  const body = nodes.map (([tag, a]) => {
    const as = Object.entries (a).filter (([k]) => k !== 'key').map (([k, v]) => {
      if (k === 'strokeWidth') v = String (stroke);
      if (v === 'currentColor') v = ink;
      return `${attr (k)}="${v}"`;
    });
    // an element with no fill of its own is a line: the default of SVG is black
    if (!('fill' in a)) as.push ('fill="none"');
    return `  <${tag} ${as.join (' ')}/>`;
  });
  return '<?xml version="1.0" encoding="UTF-8"?>\n' +
    `<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" ` +
    `viewBox="${-MARGIN} ${-MARGIN} ${24 + 2 * MARGIN} ${24 + 2 * MARGIN}">\n` +
    body.join ('\n') + '\n</svg>\n';
}

async function main () {
  const [pkg, ...only] = process.argv.slice (2);
  if (!pkg) {
    console.error ('usage: make-hugeicons.mjs <@hugeicons/core-free-icons directory> [names...]');
    process.exit (1);
  }
  const esm = path.resolve (pkg, 'dist/esm');
  let n = 0;
  for (const [name, [huge, tint, stroke]] of Object.entries (MAP)) {
    if (only.length && !only.includes (name)) continue;
    const file = path.join (esm, huge + 'Icon.js');
    if (!fs.existsSync (file)) { console.error (`${name}: no ${huge}`); continue; }
    const nodes = (await import (pathToFileURL (file).href)).default;
    for (const theme of ['light', 'dark']) {
      const ink = tint ? TINTS[tint][theme] : INK[theme];
      const dir = path.join (OUT, theme);
      fs.mkdirSync (dir, { recursive: true });
      fs.writeFileSync (path.join (dir, `tm_${name}.svg`), svg (nodes, ink, stroke || NORMAL));
    }
    n++;
  }
  fs.copyFileSync (path.resolve (pkg, 'LICENSE.md'), path.join (OUT, 'LICENSE.md'));
  console.log (`${n} icons in ${OUT}`);
}

main ();
