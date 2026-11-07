# TikZJax: the sources

The files of this folder (`run-tex.js`, `tex.wasm.gz`, `core.dump.gz`,
`tex_files/`) are those of the npm package `@rod2ik/tikzjax` 1.6.0
(`misc/wasm/get-tikzjax.sh` of the TeXmacs sources checks its sha256), under
the GNU General Public License version 3 (`LICENSE`).

Their sources, with the programs which build them (TeX compiled to
WebAssembly by web2js, its memory dump, the TeX files):
<https://github.com/rod2ik/tikzjax>, the tag `v1.6.0` (commit
`8228128a03112efd735309bdaa292cca5a3500df`), a fork of
<https://github.com/drgrice1/tikzjax> and of
<https://github.com/kisonecat/tikzjax>.

TeXmacs adds to `tex_files/` the libraries of pgfplots and of pgf listed in
`EXTRA.md`, unchanged, compressed with gzip; `tm-tikz.js` and `tables.js`
are part of TeXmacs (GPL 3 or later), sources in
<https://github.com/mgubi/texmacs> (`plugins/tikz/web`, `misc/wasm`).
