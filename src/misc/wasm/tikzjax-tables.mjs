// The tables of the worker of the TikZ plugin (plugins/tikz/web/tm-tikz.js,
// src/docs/wasm/tikzjax.md), made at build time:
//
//   node misc/wasm/tikzjax-tables.mjs <run-tex.js of TikZJax> <TeXmacs/fonts/enc> <out.js>
//
// - glyphs: for each TeX font of dvi2html (cmr10, cmmi7...), the position in
//   the font of each character of its SVG: dvi2html writes a character as a
//   private code (U+F000 plus its code in the BaKoMa fonts, the ligatures as
//   U+FB00...), by a table which is in its bundle; this is its inverse. Taken
//   from the bundle of the pinned version, so that a new one is checked.
// - enc: for each encoding of TeX fonts TeXmacs knows (fonts/enc/cmr.enc...),
//   the TeXmacs string of each position, as translator.cpp reads them: a
//   name of more than one character is the symbol <name>.

import fs from 'node:fs';
import path from 'node:path';

const [runTex, encDir, out] = process.argv.slice (2);
if (!out) {
  console.error ('usage: tikzjax-tables.mjs <run-tex.js> <fonts/enc> <out.js>');
  process.exit (2);
}

// dvi2html's table: a JSON string in the bundle, font -> position -> code
// (the bundle has other tables of the fonts: the one of the codes is the one
// whose values are private codes, U+F000 and above)
const bundle = fs.readFileSync (runTex, 'utf8');
let table = null;
for (const m of bundle.matchAll (/JSON\.parse\('(\{"cm[^']*\})'\)/g)) {
  const t = JSON.parse (m[1]);
  const v = Object.values (t.cmr10 || {});
  if (v.length && v.every ((x) => typeof x === 'number' && x >= 0xF000)) { table = t; break; }
}
if (!table) {
  console.error ('tikzjax-tables: no table of codes in ' + runTex + ' (a new TikZJax?)');
  process.exit (1);
}
const glyphs = {};
for (const [font, codes] of Object.entries (table)) {
  glyphs[font] = {};
  for (const [pos, code] of Object.entries (codes)) glyphs[font][code] = Number (pos);
}

// an .enc file of TeXmacs (translator.cpp, load_translator): numbers set the
// position, strings follow one another from it
function readEnc (file) {
  const s = fs.readFileSync (file, 'latin1');
  const r = {};
  let pos = 0, num = 0, inNum = false;
  for (let i = 0; i < s.length; i++) {
    const c = s[i];
    if (c === '"') {
      let str = '';
      for (i++; i < s.length; i++) {
        if (s[i] === '\\' && i < s.length - 1) { i++; str += s[i]; continue; }
        if (s[i] === '"') break;
        str += s[i];
      }
      // the first name of a position is its character; the others are
      // synonyms for the lookups of TeXmacs (up-A, large-(-0, space...)
      if (str.length > 0 && !(pos in r)) r[pos] = str.length > 1 ? '<' + str + '>' : str;
      pos++;
      inNum = false;
    }
    else if (c >= '0' && c <= '9') {
      num = inNum ? 10 * num + (c.charCodeAt (0) - 48) : c.charCodeAt (0) - 48;
      inNum = true;
      pos = num;
    }
    else inNum = false;
  }
  return r;
}
const enc = {};
for (const name of ['cmr', 'cmmi', 'cmsy', 'msam', 'msbm']) {
  const file = path.join (encDir, name + '.enc');
  if (fs.existsSync (file)) enc[name] = readEnc (file);
}

fs.writeFileSync (out,
  '// made by misc/wasm/tikzjax-tables.mjs: do not edit\n' +
  'var TIKZ_GLYPHS = ' + JSON.stringify (glyphs) + ';\n' +
  'var TIKZ_ENC = ' + JSON.stringify (enc) + ';\n');
console.log ('tikzjax-tables: ' + Object.keys (glyphs).length + ' fonts, encodings ' +
             Object.keys (enc).join (' '));
