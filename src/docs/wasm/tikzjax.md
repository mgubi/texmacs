# TikZ in the browser: the TikZ plugin on TikZJax (design)

Branch `wip_tikzjax` (from `wip_wasm_vue`). Status: steps 1 to 4 below
done and tested in headless Firefox; step 5 to do. The text of a picture is typeset by TeXmacs, over an image
of its drawing. Measurements and checks quoted below were made on
2026-10-03 with `@rod2ik/tikzjax` 1.6.0.

## Goal

The TikZ plugin of TeXmacs (`plugins/tikz`, a session "TikZ" whose
pictures are inserted as images) works on the desktop only: it runs a
Python program (`tmpy/session/tm_tikz.py`) which calls `latex` and `dvips`.
A page has neither processes nor TeX. [TikZJax](https://github.com/rod2ik/tikzjax)
(GPL-3.0, as TeXmacs) runs TeX itself in the browser: an e-TeX compiled to
WebAssembly, with LaTeX and TikZ preloaded, turns TikZ code into SVG. The
plugin is to work in the page on TikZJax, with the same session, the same
input, and pictures which look as they do from `latex`, their text being
typeset by TeXmacs, in its own fonts, where TeX put it.

Out of scope for a first version: a TikZ picture as markup of the document
re-rendered when edited (see "Later"), the desktop (it keeps `latex`).

## What TikZJax is

```text
TikZ source --> worker: tex.wasm + core.dump (LaTeX, TikZ preloaded)
                  |  TeX files fetched as needed (tex_files/*.gz)
                  v
                 DVI --> dvi2html --> SVG (paths, and <text> elements)
```

- `dist/run-tex.js`: a Web Worker, with two methods: `load (assetRoot)`
  fetches `tex.wasm.gz` (120 KB) and `core.dump.gz` (2.8 MB, the memory of
  a TeX which has read LaTeX and TikZ), `texify (source, options)` runs TeX
  on `\begin{document} source \end{document}` (options: `texPackages`,
  `tikzLibraries`, `addToPreamble`) and returns the SVG of the page. TeX
  reads the files it needs from `tex_files/` (245 files, 1.9 MB, gzipped),
  fetched as it opens them.
- `dist/tikzjax.js`: the part of the page, which looks for
  `<script type="text/tikz">` in the HTML, keeps a pool of workers, a cache,
  and adapts the colours to light and dark themes. Not needed here.
- The worker speaks the protocol of threads.js: the page sends
  `{type: "run", uid, method, args}`, the worker answers `{type: "init"}`
  once, then `running`, `result` (`payload`) or `error` for each call. A
  client of 40 lines replaces threads.js on our side.

### The fonts of its SVG

The paths of a picture are SVG paths. Its text is not: each run of
characters is a `<text>` element naming a font of TeX,

```xml
<text x="-29.39" y="-19.43" font-family="cmr10" font-size="10"
      fill="currentColor">&#xF048;&#xF065;&#xF06C;&#xF06C;&#xF06F;</text>
```

("Hello"; `$x^2+\alpha$` is four more elements, in `cmmi10`, `cmr7`,
`cmr10`, `cmmi10`). The characters are private codes: dvi2html maps the
position of a character in its TeX font to U+F000 plus the code of that
character in the BaKoMa fonts (positions 0-32 are moved up to 0xA1-0xC4,
as in those fonts), the ligatures to U+FB00-U+FB04, by a table of 112 fonts
(the Computer Modern families in their sizes; 10 distinct tables). The page
is to supply the fonts: `dist/fonts.css` declares 152 web fonts
(`cmr10.woff2`..., 1.8 MB, converted from the BaKoMa OpenType fonts).

In TeXmacs, images are drawn by MuPDF (`mupdf_render_svg`,
`src/Plugins/MuPDF/mupdf_picture.cpp`), which ignores the family of an SVG
font: it takes Times, Helvetica or Courier according to words such as
"serif" or "monospace" in its name (`svg-run.c`), and the page has only
those 14 standard fonts. The text of a TikZ picture would be drawn in
Times, with the private codes, i.e. missing glyphs. CSS does not reach an
image, and MuPDF has no `@font-face`.

## Design

The drawing and the text of a picture go separate ways: the SVG keeps the
drawing (paths, fills, colours) and becomes an image; the text TeX set in
it is taken out of the SVG and typeset by TeXmacs, with its own fonts and
renderer, over the image, each run of characters at the point where TeX put
it. The text is then TeXmacs text: drawn as the rest of the document at any
zoom, real text in the exported PDF (searchable, selectable), in the
document's colour.

```text
 TeXmacs                       plugin worker (tm-tikz.js)      TikZJax worker
 -------                       --------------------------      --------------
 session "TikZ" input
   | worker link (pipe-like)
   v
 input ... <EOF>  ------------> source, preamble -------------> texify:
                                                                  TeX, DVI,
                                                                  dvi2html
                                split the SVG      <------------- SVG
                                  drawing: SVG without <text>
                                  text: runs (font, size, point,
                                        characters, colour)
                                runs -> TeXmacs text (the .enc
                                  tables: TeX position -> symbol)
 \2scheme:(tikz-picture  <------
   source (superpose (image drawing) (move run1 x1 y1) ...))\5
   |
   v
 the picture in the output of the session
```

### 1. The plugin: same name, another engine

`plugins/tikz/progs/init-tikz.scm` keeps one plugin, `tikz`, with the same
session and serializer: in a page (workers there, `vue_web_wake` defined),
`(:worker "tikzjax/tm-tikz.js")`; elsewhere `(:launch ...)` of the Python
program, with its `:require`s, as before. A document made on the desktop is
evaluated in the page, and the other way round. The input
is treated as `tm_tikz.py` treats it: a `tikzpicture` is added around code
which has none, `\usetikzlibrary` lines go to the preamble, a full document
(`\documentclass`) is cut to its body and its preamble. The "magic" first
line of tmpy (`%` options) gives `texPackages`, e.g. `% packages: circuitikz`.

### 2. The plugin is a Web Worker (a fifth kind of link)

A page has no processes, but it has workers: scripts which run apart from
it and exchange messages with it, as a program exchanges bytes through its
pipes. TeXmacs makes the link of a plugin from its connection info, of four
kinds (`connection.cpp`: `pipe` to a program, `dynlink`, `cmdline`,
`request`); a fifth one, `worker`, is a Web Worker:

```scheme
(plugin-configure tikz
  (:worker "tikzjax/tm-tikz.js")   ; the browser: its script, from the page
  (:serializer ,tikz-serialize)
  (:session "TikZ"))
```

- `src/System/Link/worker_link.cpp`: `make_worker_link (url)`, a
  `tm_link_rep` as the pipe link (`start`, `write`, `read`, `interrupt`,
  `stop`); its output is taken by `process_all_workers` in the interpose
  handler of the server, as that of the pipes.
- `misc/wasm/workers.js` (a `--pre-js`): `tmWorkers` makes the worker,
  posts the input to it, keeps what it sends until TeXmacs takes it, and
  wakes the loop of the page (`vue_web_wake`). The messages are those of a
  program: `{input}` (its stdin) and `{interrupt}` to the worker, `{out}`,
  `{err}` (stdout, stderr, in the protocol of the plugins) and `{exit}` from
  it.
- `(:worker url)` in `plugin-configure` (`tm-plugins.scm`) declares it.

Nothing else of TeXmacs changes: the sessions, `plugin-eval.scm`, the
protocol are those of every plugin, and an interruption terminates nothing
but the worker. Any other plugin of the browser is a worker the same way (a
Python session on Pyodide...). Done and tested with a worker which echoes
its input (`misc/wasm/test/echo-worker.js`).

The worker answers an input with one block of the protocol, which ends the
evaluation (`connection_rep::read`: the end of the outermost block), with
the other blocks in it: `\2verbatim:` ... `\2scheme:(tree)\5` ...
`\2prompt#TikZ] \5\5`.

### 3. The worker of the TikZ plugin: `tm-tikz.js`

The worker of the plugin speaks the protocol of the plugins on one side and
drives TikZJax on the other:

- it reads the input of the session up to the line `<EOF>` (the serializer
  of the plugin), and treats it as `tm_tikz.py` does (section 1);
- it starts TikZJax's own worker (`run-tex.js`, a worker in the worker),
  calls `load (assetRoot)` once and `texify (source, options)` for each
  picture, with the protocol of threads.js (40 lines);
- it splits the SVG into the drawing and the text (section 5; workers have
  no `DOMParser`: a small parser of dvi2html's regular output) and answers
  with the tree of the picture, `\2scheme:(tikz-picture ...)\5`, the
  characters already TeXmacs symbols (the `.enc` tables, converted to JSON
  at build time, section 6);
- on an error of TeX, the end of its log in `\2utf8:...\5` on `err`;
- an interruption terminates TikZJax's worker, which is made again for the
  next picture.

### 4. Where TikZJax comes from

Its `dist/` without the fonts and the part of the page: `run-tex.js`,
`tex.wasm.gz`, `core.dump.gz`, `tex_files/` (5 MB), pinned to a version.
Two ways, to choose:

- **Copied at build time** (proposed): `misc/wasm/get-tikzjax.sh` fetches
  the npm tarball of the pinned version into the build directory and checks
  its SHA-256, as `build-mupdf.sh` does for MuPDF; the Makefile copies it to
  `out/web/tikzjax/`. Same origin as the page (a worker must be), offline
  once cached, nothing fetched elsewhere -- what the README of the port
  says. GitHub Pages serves it as the rest.
- From jsDelivr: nothing to build, but the worker has to be made from a
  blob (cross-origin), and the page then depends on a CDN.

Nothing is fetched before the first TikZ evaluation. The browser caches
the files as the packages of TeXmacs (`packages.js`, the Cache API), so a
second visit loads nothing. The worker stays alive after a job, so the
memory dump is loaded once a session.

### 5. Taking the text out of the SVG (in the worker)

`tm-tikz.js` parses the SVG of TikZJax (a small parser: workers have no
`DOMParser`, and dvi2html's output is regular) and, for each `<text>`:

- its point: the `x`, `y` of the element through the transforms of the
  `<g>` around it (dvi2html nests several: `translate`, `scale(-1,1)`,
  `scale(1,-1)`; composed as matrices, not measured by the browser), in the
  coordinates of the picture, i.e. relative to its `viewBox`, in points,
  with the origin at the bottom left and y upwards as in TeXmacs;
- the rest of the matrix: a run which is rotated or scaled (`node[rotate=
  30]`) is flagged with its angle and factor;
- its font (`font-family`, e.g. `cmmi7`), size (`font-size`), colour
  (`fill`, `currentColor` kept as such) and characters, given back as
  positions in the TeX font through the inverse of dvi2html's table (see
  below);

then removes it. The result is the SVG of the drawing alone, its size, and
the list of runs, sent to Scheme as one Scheme expression.

dvi2html writes a character as U+F000 plus its code in the BaKoMa fonts
(positions 0-32 moved up to 0xA1-0xC4), the ligatures as U+FB00-U+FB04, by
a table of 112 fonts (the Computer Modern families in their sizes; 10
distinct tables). Its inverse is generated from the bundle of the pinned
version by a script (`misc/wasm/tikzjax-glyphs.mjs`), so that a new version
of TikZJax is checked rather than trusted.

### 6. The text in TeXmacs

**Characters.** TeXmacs already knows the encodings of the TeX fonts:
`TeXmacs/fonts/enc/cmr.enc`, `cmmi.enc`, `cmsy.enc`, `cmex.enc` (read by
`translator.cpp`) give the TeXmacs symbol of a position (in `cmmi`, 11 is
`alpha`, 65 is `A`; in `cmr`, 11 is the ligature `ff`). A run becomes a
string of TeXmacs symbols (`<alpha>`, `A`...).

**Fonts.** The name of the TeX font gives the TeXmacs font and the mode:

| TeX fonts | TeXmacs |
|---|---|
| `cmr`, `cmbx`, `cmti`, `cmsl`, `cmss`, `cmtt`, `cmcsc`... | text in the Computer Modern of TeXmacs (`font` `roman`), with the series, shape and family of the name (bold, italic, slanted, sans serif, typewriter, small capitals) |
| `cmmi`, `cmmib` | math: `<math|...>`, letters in math italic |
| `cmsy`, `cmbsy` | math symbols: `<math|<infty>>`... |
| `cmex` and the rest | not typeset by TeXmacs: left in the image (see below) |

**Size.** The size TeX used (`font-size` of the run: 7 for `cmr7`),
absolute: `font-base-size` set to it (and `font-size` 1), so that the text
has the size it had in TeX whatever the size of the document -- the drawing
was laid out for it. TeXmacs takes the design size of Computer Modern for
it, as TeX did (`cmr7` at 7 pt). TeX is given the size of the document
(`\fontsize` in the preamble), so that the text of a picture is that of the
document as long as the picture does not change it.

**Widths.** TeXmacs' Computer Modern has the metrics of the TFM files of
TeX, so that a run is as wide as in TeX. Within a run TeX did not kern
(dvi2html starts a new `<text>` where TeX moves otherwise: kerns, glue), and
TeXmacs, with the same tables, does not either; the ligatures TeX made are
in the run as their symbols.

**Placement.** The picture is a `superpose` of the image of the drawing and
of one `move` per run, its box `smash`ed and placed by its baseline at the
point of the run:

```scheme
(tikz-picture "<source>"
  (superpose
    (image (tuple (raw-data "<svg of the drawing>") "tikz.svg") "111.2pt" "95.7pt" "" "")
    (move (smash (with "font-base-size" "10" "Hello")) "29.4pt" "40.1pt")
    (move (smash (with "font-base-size" "10" (math "x"))) "55.3pt" "40.1pt")
    ...))
```

`tikz-picture` is a macro of the plugin's style package (`tikz.ts`): it
shows its second argument and keeps the source, for later (a picture made
again from its source, see "Later"). A rotated or scaled run goes into
TeXmacs' `rotate` (`std-graphics.ts`) and a size, or into the image (below) if the matrix is not
a rotation with a scale.

**Colour.** A run in `currentColor` takes the colour of the document's text;
a run with its own colour (`\node[red]`) gets it (`with color`). The
drawing in `currentColor` is drawn in the colour of the text of the
document where the picture is (the SVG is given it when inserted).

**What stays in the image.** Glyphs which are not text of TeXmacs: those of
`cmex` (big delimiters and operators, of the sizes TeX chose, which TeXmacs
makes itself otherwise), positions without a symbol in the `.enc` tables,
families TeXmacs does not have (the fonts of some TikZ packages). Their
`<text>` stays in the SVG, which is useless as such in MuPDF (it draws any
family with Times, Helvetica or Courier, and the page has only those 14
fonts): such text is turned into outlines -- `(svg-outline-tex-text svg)`
in C++, next to `svg_flatten_gradients`, with the glyphs of TeXmacs' own
Type 1 fonts (the Blue Sky Computer Modern, `TeXmacs/fonts/type1/bluesky/
cm/*.pfb`, which have the same outlines as the BaKoMa fonts and the TFM
widths; checked: position 11 of `cmmi10` is `alpha`, as is 174, its BaKoMa
duplicate), through MuPDF (`fz_new_font_from_buffer`, the built-in encoding
of the font through FreeType, `fz_outline_glyph`, `fz_walk_path`). A
picture with a big integral sign has its text from TeXmacs and its integral
sign from the image, both from the same Computer Modern.

Why not otherwise:

- The whole text as outlines in the image (the first version of this
  design): the text would not be TeXmacs text (no PDF text, drawn as an
  image at every zoom).
- The text re-typeset by TeXmacs from the source (`Hello $x^2+\alpha$`
  found in the TikZ code): its place in the picture is TeX's, which depends
  on its size in TeX; the runs of the DVI are the result of that, and
  TeXmacs sets the same characters in the same fonts at the same points.
- The picture as TeXmacs graphics (`text-at`, `cline`...): every TikZ path
  would have to become a TeXmacs object, for no gain in the drawing.

### 7. Errors and limits

- A TeX error: the end of the TeX log (from the first line starting with
  `!`) in the error output of the session.
- The packages TikZJax has: its `tex_files/` (TikZ and its libraries,
  pgfplots, tikz-cd, circuitikz, chemfig, tkz-tab, yquant, braids,
  kinematikz, tikz-feynhand, physics, pgf-spectra). A missing file is an
  error of TeX ("File ... not found"), shown as such.
- Time: a first picture costs the download (some 5 MB, once) and the start
  of TeX in the worker; then a picture takes what TeX takes (not measured
  yet: a circle, a line and two labels were in TikZJax's test page within
  15 s of its loading, download included).

## Editing the text of a picture

Each node of TikZ whose text can be found in the source is one label, its
LaTeX typeset by TeXmacs, and editable:

- TeX marks the text of each node: the preamble of every picture has
  `every node/.append style={execute at begin node=...}` with a dvisvgm
  special `<g data-tm-node="n">` around it, numbered in the order TeX
  makes the nodes (coordinates included).
- `scanNodes` (`tm-tikz.js`) finds the text `{...}` of each node in the
  source, in the same order (`\node`, `node` on a path, `\coordinate`;
  options, names and `at (...)` skipped, comments too). When its count is
  that of TeX, the source is cut into its texts and the texts of its nodes,
  `(tikz-picture (tuple text1 node1 text2 ...) ...)`, and each node of one
  line, not rotated, is a label: `(tikz-label n orig body)`, at the place of
  its leftmost run and on the baseline of its main runs, from its LaTeX
  (`(tikz-latex "...")`, converted when the output comes, `tikz-edit.scm`
  overloading `connection-notify`). Otherwise the runs of TeX, as before.
- A label can be edited (the image under it is `tikz-drawing`, not
  accessible, and a superpose now gives a click to the child drawn last,
  `superpose_box_rep::find_child`: the image used to take every click).
- In an executable fold (Insert > Fold > Executable > TikZ, whose source is
  code, `tikz-script-input`), Return on the picture shows its source with
  the edited labels in it (`alternate-toggle`, overloaded): an edited label
  (its body no longer its orig) as LaTeX in place of the text of its node.
  Return on the source makes the picture again with TeX, which places the
  new text. In a session, the edit stays in the picture.

## Later

- A tag `tikz` in documents (not only sessions), made again when its
  source changes, with a cache of the result by the hash of the source (the
  home in IndexedDB), so that opening a document does not run TeX again;
  `tikz-picture` already keeps the source for it.
- The same split for the desktop (`latex` and `dvisvgm`, or TikZJax under
  node), so that a picture is the same in both.
- Fonts other than Computer Modern if TikZJax adds them (a row of the
  table of fonts, an `.enc` table).

## Steps

1. Plugins which are Web Workers: `worker_link.cpp`, `workers.js`,
   `(:worker url)`. Done, tested with an echo worker.
2. `get-tikzjax.sh`, the files in `out/web/tikzjax/`, `tm-tikz.js`: a
   session returns the picture as an image, its text missing. Done.
3. The split of the SVG in the worker, the tables
   (`misc/wasm/tikzjax-tables.mjs`, `out/web/tikzjax/tables.js`), and the
   runs as TeXmacs text (`tikz-picture` in `packages/session/tikz.ts`,
   `superpose`, `move`, `smash`): text, math, colours, sizes. Done. The
   runs need `mode` `text` (the output of a session is in the mode of
   programs, whose fonts are typewriter).
4. What stays in the image: `svg_outline_tex_text` in
   `mupdf_picture.cpp` (called by `mupdf_render_svg` after
   `svg_flatten_gradients`) for `cmex` and the others: the worker writes
   their characters as U+F000 plus their position and marks them
   `data-tm-tex`. Done. Rotated runs go there too for now: TeXmacs'
   `rotate` (`gr-transform`) is drawn mirrored and clipped by the MuPDF
   renderer of the browser (a bug of its own, to fix; then they can be
   TeXmacs text again).
5. Errors, the timeout, interruption; the plugin's documentation; tests.

## Tests

- A TikZ session in the page (`browser-run.mjs`): a circle, a node with
  text and a formula, a red label, a label in `\tiny`; the image has no
  `<text>` left, the runs are in the document as TeXmacs text, of the
  sizes and colours of TeX.
- Placement: the same picture in TikZJax's own page (text from its web
  fonts) and in TeXmacs (text from TeXmacs), as images at the same scale,
  compared: the text where TeX put it, to a pixel.
- A big integral sign (`$\displaystyle\int$`): from the image, in Computer
  Modern; a rotated node.
- An error of TeX (an undefined control sequence) and a missing package:
  the log in the output, the session ready for the next input.
- Scheme sessions, as before; the echo worker (`misc/wasm/test/`).
