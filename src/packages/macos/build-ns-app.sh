#!/bin/bash
#
# Builds TeXmacs.app with the native interface of macOS (NS/Cocoa), or the
# Vue port (SDL3 and Clay), and optionally its disk image.
#
# Usage: packages/macos/build-ns-app.sh [options]   (from the src directory)
#   --gui GUI             cocoa (default) or vue
#   --scheme SCHEME       guile (default) or s7 (in the sources)
#   --guile-config PATH   guile-config of Guile 1.8 (default: guile-config;
#                         Guile 3, as installed by Homebrew, is rejected)
#   --mupdf PREFIX        MuPDF, for vue (default: the --deps PREFIX, or the
#                         one of Homebrew)
#   --thorvg PREFIX       ThorVG, for the GPU renderer of vue (default: the
#                         --deps PREFIX, when it has it)
#   --deps PREFIX         the libraries made by build-deps.sh in PREFIX (with
#                         the part vue for vue, guile for guile),
#                         instead of the ones of the system (Homebrew): the
#                         application then runs on older versions of macOS
#   --arch ARCH           arm64 or x86_64 (default: the one of the machine;
#                         x86_64 on Apple silicon is built under Rosetta)
#   --min-macos VERSION   the oldest version of macOS, with --deps (the
#                         version given to build-deps.sh; default: 12.0)
#   --sign IDENTITY       code signing identity (default: ad hoc signature)
#   --dmg                 also make the disk image in ../distr/macos
#   --no-configure        keep the current configuration
#   -j N                  parallel jobs (default: number of processors)
#
# The application is made in ../distr/TeXmacs.app; the libraries which do
# not come with macOS (Guile, FreeType, GMP, ...) are copied inside it, or,
# with --deps, linked statically. merge-universal.sh makes one application
# of the ones of both architectures.

set -e

guile_config=guile-config
gui=cocoa
scheme=guile
mupdf=""
thorvg=""
deps=""
arch=$(uname -m)
min=12.0
sign=""
dmg=no
configure=yes
jobs=$(sysctl -n hw.ncpu)

saved_args=("$@")
while [ $# -gt 0 ]; do
  case "$1" in
    --guile-config) guile_config="$2"; shift 2;;
    --gui) gui="$2"; shift 2;;
    --scheme) scheme="$2"; shift 2;;
    --mupdf) mupdf="$2"; shift 2;;
    --thorvg) thorvg="$2"; shift 2;;
    --deps) deps="$2"; shift 2;;
    --arch) arch="$2"; shift 2;;
    --min-macos) min="$2"; shift 2;;
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

# another architecture: the whole build under Rosetta (the compilers then
# make x86_64 code, and the configure tests run)
if [ "$(uname -m)" != "$arch" ]; then
  exec arch -"$arch" /bin/bash "$0" "${saved_args[@]}"
fi

case "$gui" in cocoa|vue) ;; *) echo "--gui cocoa or vue" >&2; exit 1;; esac
case "$scheme" in guile|s7) ;; *) echo "--scheme guile or s7" >&2; exit 1;; esac

if [ -n "$deps" ]; then
  deps=$(cd "$deps" && pwd)
  guile_config="$deps/bin/guile-config"
  [ -z "$mupdf" ] && mupdf="$deps"
  [ -z "$thorvg" ] && [ -f "$deps/lib/libthorvg-1.a" ] && thorvg="$deps"
  # the version of macOS for the compilers and the linker, and not the
  # libraries of Homebrew (built for the version of macOS of the machine)
  export MACOSX_DEPLOYMENT_TARGET="$min"
  # NOTE: pkg-config (for SDL3) of Homebrew, after the libraries
  export PATH="$deps/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/homebrew/bin:/usr/local/bin"
  export PKG_CONFIG_PATH="$deps/lib/pkgconfig" PKG_CONFIG_LIBDIR="$deps/lib/pkgconfig"
fi

if [ $configure = yes ]; then
  if [ $scheme = s7 ]; then args="--with-scheme=s7"
  else args="--with-scheme=guile --with-guile=$guile_config"; fi
  args="$args --with-gui=$gui"
  if [ $gui = vue ]; then
    [ -z "$mupdf" ] && mupdf=$(brew --prefix 2> /dev/null || echo /opt/homebrew)
    args="$args --with-sdl3 --with-mupdf=$mupdf"
    [ -n "$thorvg" ] && args="$args --with-thorvg=$thorvg"
  fi
  [ -n "$deps" ] && args="$args --with-freetype=$deps/bin --with-osx=$min"
  # the headers and the libraries of the dependencies for the other tests
  # (png.h and libpng for the PDF renderer, PDFHummus)
  [ -n "$deps" ] && args="$args CPPFLAGS=-I$deps/include LDFLAGS=-L$deps/lib"
  [ -n "$sign" ] && args="$args --enable-sign=$sign"
  echo "== ./configure $args"
  ./configure $args
fi

echo "== building TeXmacs"
make -j "$jobs"

echo "== making the application"
make MACOS_BUNDLE

app=../distr/TeXmacs.app
packages/macos/check-app.sh "$app"
echo "== $app is ready"

if [ $dmg = yes ]; then
  # NOTE: of the application checked above (make MACOS_PACKAGE would make
  # it again)
  echo "== making the disk image"
  version=$(plutil -extract CFBundleShortVersionString raw "$app/Contents/Info.plist")
  mkdir -p ../distr/macos
  dmg=../distr/macos/TeXmacs-$version.dmg
  rm -f "$dmg"
  hdiutil create -volname TeXmacs -srcfolder "$app" -format UDZO "$dmg"
  ls "$dmg"
fi
