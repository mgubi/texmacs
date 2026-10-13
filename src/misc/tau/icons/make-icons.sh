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
printf 'Tau icons exported to %s\n' "$out"
