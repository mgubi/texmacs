# Libraries of pgfplots added to TikZJax

TikZJax (`misc/wasm/get-tikzjax.sh`) bundles pgfplots 1.18.2 without most of
its libraries: `fillbetween` (`\addplot fill between`, very common in the
pictures which the AI chatbots write), `groupplots`, `polar`, `statistics`,
`dateplot`, `units`, `patchplots`, `ternary`, `smithchart`, the color maps.
These are their files, from pgfplots 1.18.2 (the version of the bundle; TeX
Live 2025, `tex/generic/pgfplots/libs` and `pgfcontrib`), which the Makefile
adds, compressed, to `out/web/tikzjax/tex_files/`. `contourlua` (LuaTeX) and
`external` (it runs programs) are left out.

pgfplots is free software (GNU GPL version 3 or later, or the LaTeX Project
Public License), by Christian Feuersänger. The files keep their own notices:
- `tikzlibrary*.code.tex` but colorbrewer and colortol: the GNU General
  Public License version 3 or later (Christian Feuersänger, Patrick Häcker,
  Nick Papior Andersen), see `LICENSE` at the root of the TeXmacs sources;
- `pgflibraryfillbetween.code.tex` (part of pgf): the LaTeX Project Public
  License 1.3c or the GNU Free Documentation License (Christian
  Feuersänger), see `doc/generic/pgf/licenses` of TeX Live;
- `tikzlibrarycolorbrewer.code.tex`: the ColorBrewer color schemes,
  Copyright (c) 2002 Cynthia Brewer, Mark Harrower and The Pennsylvania State
  University, under the Apache License 2.0 (its text is in
  `TeXmacs/misc/licenses/Apache-2.0.txt`);
- `tikzlibrarycolortol.code.tex` (the schemes of Paul Tol) and the
  `pgflibrarypgfplots.color*.code.tex` which load the two above, with no
  notice of their own: pgfplots' terms (GPL 3 or later, or the LPPL).

They are not modified. The Makefile serves this file with them, as
`tikzjax/EXTRA.md` of the site.
