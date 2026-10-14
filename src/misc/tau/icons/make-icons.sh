#!/bin/sh
# Export the approved Tau artwork with macOS's image tools.
set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
src=$(CDPATH= cd -- "$here/../../.." && pwd)
out=${1:-"$PWD/tau-icons"}
mkdir -p "$out"
out=$(CDPATH= cd -- "$out" && pwd)
iconset="$out/Tau.iconset"
mkdir -p "$iconset"

command -v sips >/dev/null
command -v iconutil >/dev/null

# Logical sizes and Retina (2x) representations expected by macOS.
for size in 16 32 128 256 512; do
  sips -z "$size" "$size" "$here/tau.png" \
    --out "$iconset/icon_${size}x${size}.png" >/dev/null
  retina=$((size * 2))
  sips -z "$retina" "$retina" "$here/tau.png" \
    --out "$iconset/icon_${size}x${size}@2x.png" >/dev/null
done
iconutil -c icns "$iconset" -o "$out/Tau.icns"
cp "$out/Tau.icns" "$src/packages/macos/Tau.icns"

# The app button/loading panel, browser icon, and Apple touch icon.
for size in 256 192 180; do
  if [ "$size" = 256 ]; then name=icon.png; else name=icon-$size.png; fi
  sips -z "$size" "$size" "$here/tau.png" \
    --out "$here/../web/$name" >/dev/null
done
# The vector icon: the page shows it as it is (the app button, the loading
# panel, the icon of the tab).
cp "$here/tau.svg" "$here/../web/icon.svg"

# The window and the Dock of the desktop program (tau_shell): the variant
# without filters nor gradients, which MuPDF draws as it is (tau-mupdf.svg).
cp "$here/tau-mupdf.svg" "$src/TeXmacs/misc/images/tau.svg"

# The page about Tau in the documentation, and the desktop program when the
# SVG is not there: MuPDF has no SVG filters, so they get a picture of the
# vector icon (librsvg), or of the raster master without it.
if command -v rsvg-convert >/dev/null; then
  rsvg-convert -w 256 -h 256 "$here/tau.svg" \
    -o "$src/TeXmacs/misc/images/tau-256.png"
else
  cp "$here/../web/icon.png" "$src/TeXmacs/misc/images/tau-256.png"
fi
printf 'Tau icons exported to %s\n' "$out"
