# ThorVG in the browser: a benchmark

Can ThorVG, on the GPU, draw the editor of TeXmacs faster than the MuPDF
renderer of the Vue port? `bench.cpp` draws a screenful of the document
used to profile that renderer (paragraphs of TeX Gyre Pagella, a fraction
rule and a radical in each: 2560x1000 device pixels, 1892 glyphs) in the
browser, the way a renderer behind TeXmacs' immediate interface
(`renderer_rep::draw (char, font, x, y)`, nothing kept between repaints)
would draw it.

## Build and run

From the top of the source tree, with Emscripten in the PATH:

```sh
. misc/wasm/emenv.sh <work dir>          # in a tree which has misc/wasm
sh misc/thorvg-bench/build.sh <work dir>
node misc/wasm/browser-run.mjs --dir <work dir>/out --size 1300x1100 \
  --query '?mode=gl-atlas' --script run.txt   # run.txt: "wait 12000"
```

`build.sh` fetches ThorVG (`THORVG_VERSION`, default v1.1.2), builds it
with its CPU and GL engines (meson and ninja in a Python venv of the work
directory), and writes `out/texmacs.html`, the name `browser-run.mjs`
opens. The page prints `RESULT mode=... median=... ms` in the console
after 200 frames; each frame is timed until the GPU is done (a
`readPixels` of one pixel).

## Modes

| mode | what is drawn at every frame |
|---|---|
| `sw-glyphs` | CPU engine, a new Shape per glyph |
| `sw-runs` | CPU engine, a new Shape per line (the glyphs of the line in one path) |
| `gl-glyphs` | GL engine (WebGL2), a new Shape per glyph |
| `gl-runs` | GL engine, a new Shape per line |
| `gl-kept` | GL engine, the shapes made once, the scene moved by a pixel (a scroll) |
| `gl-atlas` | GL engine for the page, the rules and the radicals; the glyphs from an atlas of FreeType bitmaps, as textured quads in one draw call, made anew |
| `gl-slug` | as `gl-atlas`, the glyphs drawn from their outlines by the fragment shader (Slug), one instance a glyph, in one draw call |
| `gl-none` | the page, the rules and the radicals without the text: what the text costs in the other modes is the difference |

`?em=<pixels>` sets the size of the text (28: 10pt at zoom 1 on a Retina
screen).

## Results (2026-10-04)

Firefox headless on an Apple M1 (WebGL2 on the GPU, "Apple M1"), median
of 200 frames:

| mode | ms a frame |
|---|---|
| `sw-glyphs` | 6.5 |
| `sw-runs` | 145 |
| `gl-glyphs` | 8.6 |
| `gl-runs` | 7.2 |
| `gl-kept` | 7.6 |
| `gl-atlas` | **3.1** |
| *MuPDF renderer of the Vue port, a full repaint of the same view* | *6.6* |

- Glyphs as ThorVG shapes are no faster than the MuPDF renderer, on
  either engine: the paths of every glyph are flattened and tessellated
  again at every repaint. Keeping the shapes does not help when the view
  moves (`gl-kept`), and merging a line into one shape is pathological on
  the CPU engine (`sw-runs`).
- With the glyphs from an atlas, the frame takes half the time of the
  MuPDF renderer, and it is already on the GPU: the upload of the canvas
  (`putImageData`) and the fill of the page, measured in the Vue port,
  would go too.

So a GPU renderer for the Vue port would draw the glyphs itself, from an
atlas, and leave ThorVG the vector graphics (rules, lines, polygons, arcs,
curves, rounded rectangles, gradients, clips), both into the textures of
the editors, composed on the GPU. ThorVG's GL engine draws into a
framebuffer of the caller (`GlCanvas::target (..., fbo, ...)`) and resets
its cached GL state at every `sync`, so it shares a context with such
drawing.

## Slug (2026-10-05)

`gl-slug` follows E. Lengyel, "GPU-Centered Font Rendering Directly from
Glyph Outlines" (JCGT 6 (2), 2017). The outline of a glyph is a list of
quadratic curves in em units (the cubic curves of a CFF font split into
quadratics, an error of at most 1/2048 em), in a texture of curves; its
box is cut into up to 8 horizontal and 8 vertical bands, each listing the
curves which cross it, sorted for an early exit. For each pixel the shader
casts a ray towards +x through the curves of its horizontal band and one
towards +y through those of its vertical band; each curve adds or removes
coverage from its crossings, the roots which count chosen by the signs of
its control points (the 0x2E74 table of the paper, which keeps the winding
number right where curves meet). The two estimates are weighted by how
close the nearest crossing of each is to the pixel: averaged equally, the
thin stems of small text came out too dark. The 34 glyphs of the page are
1032 curves, 64 KB of curves and 32 KB of bands, for every size.

What the text costs on top of `gl-none` (about 3.0 ms, the page through
ThorVG and the wait for the GPU), medians of three runs, Firefox, Apple M1:

| size of the text | glyphs | atlas | Slug |
|---|---|---|---|
| 14 px | 6364 | +0.4 ms | +1.3 ms |
| 28 px (10pt, Retina) | 1892 | +0.2 ms | +1.1 ms |
| 56 px (zoom 2) | 516 | +0.1 ms | +1.2 ms |

(The runs vary by about 0.5 ms.) Slug costs a millisecond more for a
whole screen of text, whatever the size, and its antialiasing matches the
FreeType bitmaps of the atlas at 28 px and, with the weights, at 14 px.
It costs nothing when the size changes, where the atlas needs new bitmaps:
in TeXmacs those are the glyph bitmaps made by `shrink`, half of a repaint
on a page of many fonts. In the Vue port the text is drawn when an editor
repaints its backing store (strips while scrolling, everything at a
change of zoom), not at every frame, so the millisecond is paid on those
repaints only.
