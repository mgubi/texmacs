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
Public License), by Christian Feuersänger.
