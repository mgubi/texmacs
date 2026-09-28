#!/bin/bash
#
# Makes one application for Apple silicon and Intel from the ones made by
# build-ns-app.sh for each architecture (with the same sources and options):
# the files of the first one, with each program and library replaced by the
# union of both (lipo), signed again; optionally its disk image.
#
# Usage: packages/macos/merge-universal.sh ARM64.app X86_64.app OUT.app [OUT.dmg]
#   SIGN_IDENTITY   code signing identity (default: ad hoc signature)

set -e

[ $# -ge 3 ] || { echo "usage: $0 ARM64.app X86_64.app OUT.app [OUT.dmg]" >&2; exit 1; }
arm="$1" x86="$2" out="$3" dmg="$4"
here=$(cd "$(dirname "$0")" && pwd)

rm -rf "$out"
ditto "$arm" "$out"
while IFS= read -r f; do
  rel="${f#$out/}"
  file -b "$f" | grep -q "Mach-O" || continue
  [ -f "$x86/$rel" ] || { echo "error: $rel is missing in $x86" >&2; exit 1; }
  lipo -create "$arm/$rel" "$x86/$rel" -output "$f"
done < <(find "$out/Contents" -type f)

# the signatures of the separate architectures are no longer valid
sign="${SIGN_IDENTITY:--}"
[ "$sign" = - ] && ts="" || ts="--timestamp"
find "$out/Contents/Resources/lib" -name "*.dylib" -exec codesign --force $ts -s "$sign" {} \; 2> /dev/null || true
codesign --force $ts --deep -s "$sign" "$out"

"$here/check-app.sh" "$out"
echo "== $out is ready"

if [ -n "$dmg" ]; then
  rm -f "$dmg"
  hdiutil create -volname TeXmacs -srcfolder "$out" -format UDZO "$dmg"
  echo "== $dmg is ready"
fi
