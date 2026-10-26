# The licenses of the software in TeXmacs Upsilon

TeXmacs is free software under the GNU General Public License version 3 or
later (`LICENSE` and `COPYING` at the root of the TeXmacs folder). Its
packages include the software of others, or are linked with it, and its
browser version is compiled with some of it. This folder has their licenses;
the fonts have theirs in their own folders (`fonts/truetype/*`, see
`fonts/LICENSE`), and so do the grammars of the program code
(`misc/tree-sitter/*`) and the Lucide icons (`misc/pixmaps/lucide`).

## Compiled into TeXmacs

| Component | Where | License | File |
|---|---|---|---|
| s7 Scheme (Bill Schottstaedt and others), with fpconv (Andreas Samoljuk) | `src/Scheme/S7`, all the builds of Upsilon | 0BSD; fpconv MIT | `s7-0BSD.txt` |
| femtolisp (Jeff Bezanson) | `src/Scheme/Femtolisp`, the femtolisp builds | BSD 3-clause | `femtolisp-BSD-3-Clause.txt` |
| TinyScheme (Dimitrios Souflis) | `src/Scheme/Tiny` | BSD 3-clause | `tinyscheme-BSD-3-Clause.txt` |
| tree-sitter (Max Brunsfeld), the runtime and the Scheme port of the generator (`progs/treesit`) | `src/Plugins/TreeSitter/tree-sitter` | MIT | `tree-sitter-MIT.txt` |
| the Unicode tables of tree-sitter (from ICU) | `src/Plugins/TreeSitter/tree-sitter/src/unicode`, `progs/treesit/ts-unicode.scm` | Unicode / ICU license | `tree-sitter-unicode-ICU.txt` |
| XZ Embedded | `src/Plugins/TreeSitter/xz` | 0BSD | `xz-embedded-0BSD.txt` |
| Clay (Nic Barker) | `src/Plugins/Vue`, the Vue builds | zlib | `clay-zlib.txt` |
| the Slug shaders (Eric Lengyel), adapted | `src/Plugins/Vue/gpu_backend_*`, the Vue builds | MIT | `Slug-MIT.txt` |
| puff (Mark Adler), the inflater, adapted | `src/System/Files/zip_files.cpp` | zlib | its notice in the file |
| the Mersenne Twister (Makoto Matsumoto, Takuji Nishimura) | `femtolisp/llt`, the femtolisp builds | BSD 3-clause | `femtolisp-BSD-3-Clause.txt` |
| fixes of the femtolisp of Julia | `src/Scheme/Femtolisp/patches` | BSD 3-clause | `femtolisp-BSD-3-Clause.txt` |
| `last-pair` of SRFI-1 (Olin Shivers), the interface of `hash` of SRFI-69 (Panu Kalliokoski) | `progs/kernel/boot/compat-*.scm` | their notices, MIT | in the files |

## Libraries linked with TeXmacs

On macOS they are compiled into the application (`packages/macos/build-deps.sh`
says which versions); on Linux and Windows the packages bring them with the
program, as the libraries of the system they were made on.

| Library | Builds | License | File |
|---|---|---|---|
| Qt (The Qt Company and others) | Qt | LGPL 3 | `LGPL-3.0.txt` |
| SDL3, SDL3_ttf (Sam Lantinga and others) | Vue | zlib | `SDL3-zlib.txt`, `SDL3_ttf-zlib.txt` |
| ThorVG (the ThorVG Project) | Vue | MIT | `ThorVG-MIT.txt` |
| MuPDF (Artifex Software) | Vue | AGPL 3 or later | `AGPL-3.0.txt` |
| GnuTLS | all | LGPL 2.1 or later | `LGPL-2.1.txt` |
| Nettle, GMP | all (with GnuTLS) | LGPL 3 or later (or GPL 2 or later) | `LGPL-3.0.txt` |
| FreeType (The FreeType Project) | all | FreeType License | `FreeType-LICENSE.txt`, `FreeType-FTL.txt` |
| libpng | all | libpng license | `libpng-LICENSE.txt` |

FreeType: portions of this software are copyright (C) The FreeType Project
(www.freetype.org). All rights reserved.

MuPDF comes with libraries of its own (`thirdparty` of its sources), which
are in the Vue builds; the browser version has only jbig2dec, HarfBuzz,
Little CMS, libjpeg and OpenJPEG of them (`misc/wasm/build-mupdf.sh`):

| Library | License | File |
|---|---|---|
| jbig2dec, extract (Artifex Software) | AGPL 3 or later | `AGPL-3.0.txt` |
| HarfBuzz | MIT | `HarfBuzz-MIT.txt` |
| Little CMS (lcms2mt) | MIT | `lcms2-MIT.txt` |
| libjpeg (the Independent JPEG Group) | IJG license | `libjpeg-IJG-README.txt` (LEGAL ISSUES) |
| OpenJPEG | BSD 2-clause | `OpenJPEG-BSD-2-Clause.txt` |
| MuJS (Artifex Software) | ISC | `MuJS-ISC.txt` |
| Gumbo (Google) | Apache 2.0 | `Apache-2.0.txt` |
| Brotli | MIT | `brotli-MIT.txt` |
| zlib (Jean-loup Gailly, Mark Adler; the browser version) | zlib | `zlib-LICENSE.txt` |
| zint | BSD 3-clause | `zint-BSD-3-Clause.txt` |
| ZXing-C++ | Apache 2.0 | `Apache-2.0.txt` |

This software is based in part on the work of the Independent JPEG Group.

The Linux and Windows packages also bring the libraries which these depend
on (on Windows those of MSYS2, on Linux those of Ubuntu): the bundles have
their licenses in `misc/licenses/system` when they were made with them
(`packages/collect-licenses.sh`).

## The sources

The sources of TeXmacs Upsilon, with those of the components compiled into
it, are at <https://github.com/mgubi/texmacs> (the branch `upsilon_texmacs`,
built by the branch `upsilon_ci`). The libraries come from:
Qt <https://download.qt.io>, SDL <https://github.com/libsdl-org>, ThorVG
<https://github.com/thorvg/thorvg>, MuPDF <https://mupdf.com/releases>
(with the patches in `misc/wasm/mupdf-*.patch` of the TeXmacs sources for
the browser version), GnuTLS <https://www.gnutls.org>, Nettle
<https://www.lysator.liu.se/~nisse/nettle>, GMP <https://gmplib.org>, FreeType
<https://freetype.org>, libpng <http://www.libpng.org>. For the browser
version, Help > TeXmacs in the browser gives the versions and the checksums
of the sources, and the plug-ins which run programs of others (TikZJax,
Asymptote-web, Pyodide, webR).

## The browser version

The browser version is compiled with s7, Clay, MuPDF (with the libraries
named above), FreeType, zlib, SDL3, ThorVG and Hunspell (László Németh, the
spell checker: MPL 1.1, GPL 2 or LGPL 2.1 at the choice of the user,
`hunspell-license.txt`, `MPL-1.1.txt`, `LGPL-2.1.txt`); these texts are
served with the page, in `licenses/`. The page also runs:
- TikZJax (GPL 3), the TeX of the TikZ plug-in: the license is served with
  it (`tikzjax/LICENSE`), the sources are at
  <https://github.com/rod2ik/tikzjax> (the tag of the version), the TeX
  libraries which TeXmacs adds to it are listed in `tikzjax/EXTRA.md`;
- Asymptote-web (LGPL 3), Asymptote 3.15 compiled to WebAssembly, the
  program of the Asymptote plug-in: the license is served with it
  (`asymptote/LICENSE`, `asymptote/SOURCES.md`).
