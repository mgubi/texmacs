# Tau icon

![Tau icon](../web/icon.png)

The approved artwork combines a white upright sans serif tau, peach chalk
angle brackets, and a blue slate blackboard with a faint grid and soft light
gradient. `tau.png` is the 1254 × 1254 raster master, with transparency around
the rounded tile. The artwork was created and refined with OpenAI's image
generation tool.

The web app uses `../web/icon.png` (256 pixels), `icon-192.png` for the browser
fallback, and `icon-180.png` for the Apple touch icon. The app button and loading
panel use the same artwork.

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
