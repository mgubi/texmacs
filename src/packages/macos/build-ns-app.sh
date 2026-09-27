#!/bin/bash
#
# Builds TeXmacs.app with the native interface of macOS (NS/Cocoa), and
# optionally its disk image.
#
# Usage: packages/macos/build-ns-app.sh [options]   (from the src directory)
#   --guile-config PATH   guile-config of Guile 1.8 (default: guile-config)
#   --sign IDENTITY       code signing identity (default: ad hoc signature)
#   --dmg                 also make the disk image in ../distr/macos
#   --no-configure        keep the current configuration
#   -j N                  parallel jobs (default: number of processors)
#
# The application is made in ../distr/TeXmacs.app; the libraries which do
# not come with macOS (Guile, FreeType, GMP, ...) are copied inside it.

set -e

guile_config=guile-config
sign=""
dmg=no
configure=yes
jobs=$(sysctl -n hw.ncpu)

while [ $# -gt 0 ]; do
  case "$1" in
    --guile-config) guile_config="$2"; shift 2;;
    --sign) sign="$2"; shift 2;;
    --dmg) dmg=yes; shift;;
    --no-configure) configure=no; shift;;
    -j) jobs="$2"; shift 2;;
    *) echo "unknown option $1" >&2; exit 1;;
  esac
done

if [ ! -f configure ] || [ ! -d packages/macos ]; then
  echo "run this script from the src directory of TeXmacs" >&2
  exit 1
fi

if [ $configure = yes ]; then
  args="--with-guile=$guile_config --disable-qt --enable-cocoa"
  [ -n "$sign" ] && args="$args --enable-sign=$sign"
  echo "== ./configure $args"
  ./configure $args
fi

# NOTE: editor.hpp includes the headers of the NS interface; after changing
# their classes, remove src/Objects/*.o (the dependencies miss them)
echo "== building TeXmacs"
make -j "$jobs"

echo "== making the application"
make MACOS_BUNDLE

app=../distr/TeXmacs.app
codesign --verify --deep --strict "$app"
plutil -lint "$app/Contents/Info.plist" > /dev/null
if otool -L "$app/Contents/MacOS/TeXmacs" "$app"/Contents/Resources/lib/*.dylib |
   grep -q "/opt/homebrew\|/usr/local/\|/opt/local"; then
  echo "error: libraries outside the application are still used" >&2
  exit 1
fi
echo "== $app is ready"

if [ $dmg = yes ]; then
  echo "== making the disk image"
  make MACOS_PACKAGE
  ls ../distr/macos/*.dmg
fi
