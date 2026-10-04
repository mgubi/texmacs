# The Asymptote plugin in the browser

In the browser build, the Asymptote plugin (`plugins/asymptote`) runs
Asymptote itself, compiled to WebAssembly by
[Asymptote-web](https://github.com/Julieisbaka/Asymptote-web) (LGPL-3.0),
in a Web Worker. Sessions and executable folds work as on the desktop;
the labels of the pictures are set by TeXmacs.

## Pieces

- `misc/wasm/get-asymptote.sh`: the npm package `asymptote-web` of a pinned
  version (0.3.3, Asymptote 3.15), checked against its SHA-256, unpacked in
  `build-wasm/asymptote-web-<version>/`.
- `misc/wasm/Makefile`, target `asymptote` (part of `web`): copies what the
  worker needs (`asymptote.js`, `asymptote.wasm`, `asy.data`,
  `asymptote-web.js`, `utils.js`) to `out/web/asymptote/`, with brotli copies
  of the large ones (`asymptote.wasm` is 33 MB, 6.5 MB compressed), writes
  `version.js` (the versions, for the banner), and copies the worker.
- `plugins/asymptote/web/tm-asy.mjs`: the worker, a module worker
  (`misc/wasm/workers.js` starts a script whose name ends in `.mjs` with
  `type: 'module'`). It speaks the protocol of the plugins over the worker
  link (`src/System/Link/worker_link.cpp`), as `tm-tikz.js` does.
- `plugins/asymptote/progs/init-asymptote.scm`: `(:worker
  "asymptote/tm-asy.mjs")` in a browser, the Python launcher elsewhere.
- `plugins/asymptote/progs/asymptote-edit.scm`: converts the LaTeX of the
  labels when the output comes, and puts edited labels back into the source
  of an executable fold.
- `plugins/asymptote/packages/session/asymptote.ts`: `asy-picture`,
  `asy-drawing`, `asy-label`, and the input of a fold as code.

## A picture

The worker runs `asy -f eps -tex none -noV -o /w/out /w/in.asy` in `/w` (the
only directory where Asymptote writes), and converts the EPS to SVG with the
converter of Asymptote-web (`epsToSvg`). Its answer:

```
(asy-picture source
  (superpose
    (asy-drawing (image (tuple (raw-data svg) "asymptote.svg") W H "" ""))
    (with gr-frame (scale 1pt, origin at the bottom left) gr-geometry W H
      (graphics
        (with text-at-halign h text-at-valign v
          (text-at (with font... (asy-label n (asy-latex L) (asy-latex L)))
                   (point x y)))
        ...))))
```

## The labels

Asymptote-web has no TeX: with `-tex none`, Asymptote draws the text of a
label with a font of its own, from its source (the dollars of `$x$`
included), and the SVG says neither where a label is nor what it says.

Every label goes through one function, `Label.label` in `plain_Label.asy`
(the tick labels of `graph` too). When Asymptote has started, the worker
rewrites that file in the file system of the module: in the `-tex none`
branch, a label is now

- drawn invisibly, for the size of the picture: a box of its size as TeXmacs
  will set it, else its text in the font of Asymptote-web;
- marked at its anchor `S` by a tiny triangle whose colour is its number
  (`rgb(19, n div 256, n mod 256)`), which goes through every transform of
  the picture: in the SVG, its first point is the place of the label;
- written to `tm-labels.txt`: its number, its alignment, its font size
  (times the scale of its transform), its colour, its LaTeX.

The worker takes the markers out of the SVG and places each label with a
`text-at` of a graphics over the image, at the anchor plus one label margin
(0.3 em) in the direction of the alignment (where Asymptote made room for
it), with `text-at-halign` and `text-at-valign` from the alignment (left,
center, right; bottom, center, top): TeXmacs aligns the label with its true
size.

Asymptote makes room for the labels and spaces the ticks of its axes from
their sizes. The worker therefore runs Asymptote twice when a picture has
labels: the first run gives the labels, whose sizes are estimated from their
LaTeX (`sizeOf`: letters, Greek, functions, fractions, scripts, roots, in
em), and the second run gets them in a table at the top of
`plain_Label.asy` (`tmBox`). The ticks are still chosen partly from
Asymptote's own font: a step can be given (`LeftTicks(Step=1)`).

A rotated label is set upright (the rotation of TeXmacs is not right in the
renderer of the browser yet); a label squashed to nothing stays Asymptote's.

A first line `// debug: svg` returns the SVG and the label records as text.

## Editing

A label is TeXmacs text, editable. In an executable fold, when the source is
shown again (`alternate-toggle`, `asymptote-edit.scm`), the string `"orig"`
of each edited label, when the source has it once, is replaced by the LaTeX
of the label (`convert ... "latex-snippet"`).
