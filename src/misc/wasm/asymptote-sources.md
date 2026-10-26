# Asymptote-web: the sources

The files `asymptote.js`, `asymptote.wasm`, `asy.data`, `asymptote-web.js`
and `utils.js` of this folder are those of the npm package `asymptote-web`
0.3.3 (`misc/wasm/get-asymptote.sh` of the TeXmacs sources checks its
sha256): Asymptote 3.15 compiled to WebAssembly, with its base files. They
are under the GNU Lesser General Public License version 3 (`LICENSE`, which
adds to the GNU General Public License version 3, `COPYING`).

Their sources:
- Asymptote-web: <https://github.com/Julieisbaka/Asymptote-web>, the commit
  `fe79f09eb070f2c6061118173742f961dfda0da5` (version 0.3.3), whose `wasm/`
  folder builds the WebAssembly (the patches, the Dockerfile with
  Emscripten 6.0.7);
- Asymptote 3.15: <https://sourceforge.net/projects/asymptote/files/3.15/>,
  `asymptote-3.15.src.tgz` (sha256
  `a32764fcfc83eb4eb2981aa9df2c0e6229ffab95929f1030abfc6a6d1abf5e8d`),
  by Andy Hammerlindl, John Bowman and Tom Prince
  (<https://asymptote.sourceforge.io>).

`tm-asy.mjs` and `version.js` are part of TeXmacs (GPL 3 or later), sources
in <https://github.com/mgubi/texmacs> (`plugins/asymptote/web`, `misc/wasm`).
