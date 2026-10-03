# TikZ in the browser: the TikZ plugin on TikZJax (design)

Branch `wip_tikzjax` (from `wip_wasm_vue`). Status: design, nothing
implemented yet. Measurements and checks quoted below were made on
2026-10-03 with `@rod2ik/tikzjax` 1.6.0.

## Goal

The TikZ plugin of TeXmacs (`plugins/tikz`, a session "TikZ" whose
pictures are inserted as images) works on the desktop only: it runs a
Python program (`tmpy/session/tm_tikz.py`) which calls `latex` and `dvips`.
A page has neither processes nor TeX. [TikZJax](https://github.com/rod2ik/tikzjax)
(GPL-3.0, as TeXmacs) runs TeX itself in the browser: an e-TeX compiled to
WebAssembly, with LaTeX and TikZ preloaded, turns TikZ code into SVG. The
plugin is to work in the page on TikZJax, with the same session, the same
input, and pictures which look as they do from `latex` -- in particular
their text, in the fonts of TeX.

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

```text
 TeXmacs (Scheme)                    page (JavaScript)          worker
 ----------------                    -----------------          ------
 session "TikZ" input
   | plugin-feed (in-process plugin)
   v
 (web-tikz id source options) ---->  tmTikz.render ------------> texify
                                       queue, timeout             TeX, DVI,
                                                                  dvi2html
 (tikz-done id svg log)       <----  _vue_web_scheme  <---------- SVG
   |
   v
 svg-outline-tex-text (C++): <text> of TeX fonts -> <path>,
   glyphs from TeXmacs' own Type 1 fonts
   |
   v
 image (tuple (raw-data svg) "svg") in the output of the session
```

### 1. The plugin: same name, another engine

`plugins/tikz/progs/init-tikz.scm` keeps one plugin, `tikz`, with the same
session and serializer. When the page offers TikZ (`(defined? 'web-tikz)`,
a function of `vue_gui.cpp` as `web-paste`), it is configured as an
in-process plugin evaluated by `tikz-browser-eval` instead of `:launch`ing
the Python program; otherwise it is configured as before. A document made
on the desktop is evaluated in the page, and the other way round. The input
is treated as `tm_tikz.py` treats it: a `tikzpicture` is added around code
which has none, `\usetikzlibrary` lines go to the preamble, a full document
(`\documentclass`) is cut to its body and its preamble. The "magic" first
line of tmpy (`%` options) gives `texPackages`, e.g. `% packages: circuitikz`.

### 2. In-process plugins with an asynchronous answer

`utils/plugins/plugin-eval.scm` has one plugin without a process, Scheme,
special-cased in four places (`plugin-status`, `plugin-start`,
`plugin-write`, `plugin-feed`), which answers at once. The design makes
it a table:

```scheme
(plugin-in-process! "tikz" tikz-browser-eval)
;; (tikz-browser-eval lan ses input done): runs, then calls
;; (done "output" tree) or (done "error" tree), once, possibly later
```

Its status is "running" (2) at once, `plugin-write` calls the evaluator,
and `done` does what the Scheme case does after its evaluation:
`connection-notify` with the channel and the tree, then
`connection-notify-status` 2, which goes to the next input. Scheme becomes
the first entry of the table (its evaluator calls `done` at once), so that
the special cases go. An interruption (`plugin-interrupt`) cancels the job
in the page (the worker is terminated and made again).

### 3. The bridge in the page: `misc/wasm/tikz.js`

A `--pre-js` of the browser build, as `clipboard.js`:

- `tmTikz.render (id, source, options)`: starts the worker the first time
  (`new Worker (assetRoot + "/run-tex.js")`, then `load (assetRoot)`), puts
  the job in a queue (one worker: a second one costs 2.8 MB of memory dump
  and is not worth it for a session), and when it is done calls
  `(tikz-done id svg log)` through `_vue_web_scheme`, as `files.js` does.
  A timeout (60 s, TeX has no other limit), and the TeX log on an error
  (`input.log`, which the worker returns in the message of its error).
- `vue_gui.cpp`: `(web-tikz id source options-json)` calls it (`EM_JS`, as
  `web-paste`).

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

### 5. The fonts: text to outlines, from TeXmacs' fonts

The SVG is rewritten before TeXmacs sees it: every `<text>` whose family is
a font of TeX becomes one `<path>` per character, with the outline of the
glyph. Done in C++, next to `svg_flatten_gradients`, as a function given to
Scheme, `(svg-outline-tex-text svg)`:

- **The glyphs come from TeXmacs' own Type 1 fonts**, the Blue Sky
  Computer Modern (`TeXmacs/fonts/type1/bluesky/cm/cmr10.pfb`..., also the
  AMS ones), which the page already loads one by one when a document uses
  them. They have the same outlines as the BaKoMa fonts of the web fonts
  (both are the AMS/Blue Sky Computer Modern), and widths which are those
  of the TFM files of TeX. No 1.8 MB of web fonts to download.
- **Position of a character**: the inverse of dvi2html's table (code point
  -> position in the TeX font, per font), kept as data in the plugin
  (`plugins/tikz/progs/tikzjax-glyphs.scm`, generated from the bundle of
  the pinned version by a script, so that a new version of TikZJax is
  checked). The built-in encoding of the Blue Sky fonts gives the glyph of
  a position (checked: position 11 of `cmmi10` is `alpha`, and 174, its
  BaKoMa duplicate, is `alpha` too).
- **Outline**: MuPDF loads the `.pfb` (`fz_new_font_from_buffer`, kept in a
  cache by font name), FreeType gives the glyph of the position through the
  built-in encoding of the font (`FT_ENCODING_ADOBE_CUSTOM` on
  `fz_font_ft_face`), `fz_outline_glyph` its outline for the size of the
  text, and `fz_walk_path` writes it as SVG path data. The characters of a
  run are placed one after the other by their advances
  (`fz_advance_glyph`), as the browser does for a `<text>` (dvi2html starts
  a new `<text>` where TeX moves otherwise: kerns, glue).
- **Colour**: the `fill` of the text, `currentColor` resolved to the colour
  of the text where the picture is inserted (black by default), as the paths
  of the picture (`stroke="currentColor"`).
- A family which is not a font of TeX, a character which is not in the
  table: the `<text>` is left as it is (MuPDF draws it as it can) and the
  plugin says so once in the output of the session.

Why not otherwise:

- Web fonts in the SVG (`@font-face` with data URIs): MuPDF does not read
  them.
- The font files of TikZJax (WOFF2, converted from BaKoMa): 1.8 MB more to
  fetch, a WOFF2 decoder, for the outlines TeXmacs already has.
- Text to paths in JavaScript (opentype.js): the same outlines, more code
  in the page, and a format (OpenType) the page does not have.
- Text as TeXmacs text: an image has no text of TeXmacs in it, and the
  picture would no longer be the one TeX made.

### 6. What goes into the document

An image whose data is in the document,
`(image (tuple (raw-data <svg>) "tikz.svg") <w> <h> "" "")`, the size
being that of the SVG (`width="111.2pt"`). Saved with the document,
exported to PDF as the other SVG images, drawn on the desktop too (MuPDF
there as well), without any file. The source stays in the input field of
the session, as with the desktop plugin.

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

## Later

- A tag `tikz` in documents (not only sessions), rendered when its source
  changes, with a cache of the SVG by the hash of the source (the home in
  IndexedDB), so that opening a document does not run TeX again.
- The same outlining for the desktop, where TikZ pictures could also be SVG
  (dvisvgm) rather than EPS.
- Fonts other than Computer Modern if TikZJax adds them (the table and the
  font resolution of TeXmacs cover the families TeXmacs has).

## Steps

1. In-process plugins with an answer later (`plugin-eval.scm`), Scheme
   moved to the table; the Scheme sessions tested as before.
2. `get-tikzjax.sh`, the files in `out/web/tikzjax/`, `tikz.js` and
   `web-tikz`: a session returns the raw SVG (text in Times).
3. `svg-outline-tex-text` and the glyph table: the text in its fonts.
   Checked against TikZJax's own page (the same picture with its web fonts),
   by comparing screenshots.
4. Errors, the timeout, interruption; the plugin's documentation; tests
   with `browser-run.mjs` (a session, a picture with text, an error).

## Tests

- A TikZ session in the page (`browser-run.mjs`): a circle and a node with
  text and a formula; the image is inserted, its text is paths (no
  `<text>` left), the glyphs those of TeX.
- Fonts: the same picture in TikZJax's own page with its web fonts and in
  TeXmacs, as images at the same scale, compared.
- An error of TeX (an undefined control sequence) and a missing package:
  the log in the output, the session ready for the next input.
- Scheme sessions, as before (the in-process table).
