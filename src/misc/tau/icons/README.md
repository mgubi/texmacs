# Tau icon

![Tau icon](../web/icon.png)

The approved artwork combines a white upright sans serif tau, peach chalk
angle brackets, and a blue slate blackboard with a faint grid and soft light
gradient. `tau.png` is the 1254 × 1254 raster master, with transparency around
the rounded tile. The artwork was created and refined with OpenAI's image
generation tool.

`tau.svg` is an editable vector reconstruction of this master. Its tile and
three glyph silhouettes are traced as cubic paths; the grid, borders, gradient,
and chalk textures are separate named layers. The glyph borders have their own
layer, and the board uses a diagonal blue gradient with a gentle light bloom. It contains no embedded bitmap,
external resource, or font. The chalk texture is an approximation made from
editable stroke patterns and SVG filters, so it is not pixel-identical to the
raster. The approved PNG remains the source of the production web and macOS
exports.

For MuPDF, use one of the simplified variants:

- `tau-mupdf.svg` keeps the blue diagonal gradient, expressed as opaque colored
  paths rather than an SVG gradient paint server.
- `tau-mupdf-flat.svg` uses a solid blue tile for the smallest, simplest asset.

Both use only SVG paths, groups, and solid colors. They retain the glyph shapes,
peach and white fills, grid, and thick slate borders, with no filters, textures,
patterns, masks, clipping, fonts, embedded images, or external resources. The
installed MuPDF renderer painted an SVG gradient black, so the gradient variant
avoids that feature as well. The detailed `tau.svg` remains available for SVG
editors and renderers that support its effects.

To render either simplified variant with MuPDF:

```sh
mutool draw -F png -c rgba -w 256 -h 256 -o /tmp/tau-mupdf.png misc/tau/icons/tau-mupdf.svg
```

To preview the SVG with librsvg (optional):

```sh
rsvg-convert -w 1254 -h 1254 misc/tau/icons/tau.svg -o /tmp/tau-svg.png
```

The web app shows the vector icon itself, `../web/icon.svg` (a copy of
`tau.svg`): the app button, the loading panel and the icon of the tab. The
PNG files beside it are for what cannot take an SVG: `icon.png` (256 pixels)
and `icon-192.png` as fallbacks of the tab icon, `icon-180.png` for the Apple
touch icon.

The desktop program (`tau_shell`) draws with MuPDF, which has no SVG filters:
the icon of its window and in the Dock is `TeXmacs/misc/images/tau.svg`, a
copy of `tau-mupdf.svg`, which MuPDF draws at 256 pixels. When it is not
there, and in the page about Tau of the documentation (Help → About Tau), it
is `TeXmacs/misc/images/tau-256.png`, a picture of `tau.svg` made with
librsvg. `make-icons.sh` makes both.

`../../../packages/macos/Tau.icns` contains the standard macOS icon sizes
16, 32, 128, 256, and 512 points, each at 1x and 2x, up to 1024 pixels. It is a
Tau-specific asset for macOS app bundles; the current native Tau trial builds
standalone executables and has no app bundle to install it into yet.

To regenerate the web PNGs, the macOS `.icns`, and the full `.iconset` on macOS,
run from the source tree (`src`):

```sh
sh misc/tau/icons/make-icons.sh /tmp/tau-icons
```

This uses the built-in `sips` and `iconutil` tools and preserves the master
artwork and its alpha channel. The output directory receives `Tau.icns` and
`Tau.iconset`; the script also updates the committed web PNGs and
`packages/macos/Tau.icns`.
