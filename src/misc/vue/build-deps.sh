#!/bin/bash
#
# Builds the libraries of the Vue port which the distributions lack, or
# only have in other versions, for Linux and Windows (MSYS2): SDL3 and
# SDL3_ttf (shared), MuPDF (static, with its own third-party libraries but
# FreeType and zlib, those of the system) and ThorVG (static, its GL
# engine, misc/thorvg/build-thorvg.sh), into a prefix. macOS has its own,
# packages/macos/build-deps.sh, with the same versions.
#
# Usage: misc/vue/build-deps.sh PREFIX [PARTS]   (from the src directory)
#   PARTS  separated by commas: sdl, mupdf, thorvg (default: all three)
#
# It needs a C and C++ compiler, make, pkg-config, FreeType and zlib (and
# their headers), and for SDL cmake and ninja, for ThorVG meson and ninja
# (or Python, to install them). Then configure TeXmacs with
#   --with-gui=vue --with-sdl3 --with-mupdf=PREFIX --with-thorvg=PREFIX
#   PKG_CONFIG_PATH=PREFIX/lib/pkgconfig
# (SDL3 and SDL3_ttf are then in PREFIX/lib, or PREFIX/bin on Windows).

set -e

# the versions of packages/macos/build-deps.sh
SDL3_VERSION=3.4.18
SDL3_SHA256=9c75cf16330322c217dedd2e0609f1124f1b54b8633e763467b4684d0f4334a3
SDL3_TTF_VERSION=3.2.2
SDL3_TTF_SHA256=63547d58d0185c833213885b635a2c0548201cc8f301e6587c0be1a67e1e045d
MUPDF_VERSION=1.28.5
MUPDF_SHA256=98a5c10cda20c3992cdf76ff6b2a1149c32bd79cc796d3f703230b1185b7e934
THORVG_VERSION=v1.1.2

prefix="$1"
[ -n "$prefix" ] || { echo "usage: $0 PREFIX [sdl,mupdf,thorvg]" >&2; exit 1; }
parts=",${2:-sdl,mupdf,thorvg},"
has () { case "$parts" in *,$1,*) return 0;; *) return 1;; esac; }
here=$(cd "$(dirname "$0")" && pwd)
mkdir -p "$prefix"
prefix=$(cd "$prefix" && pwd)
work="$prefix/build"
mkdir -p "$work"
jobs=$(nproc 2> /dev/null || sysctl -n hw.ncpu)

fetch () { # url sha256
  local f="$work/$(basename "$1")"
  [ -s "$f" ] || curl -sSfL --retry 3 -o "$f" "$1"
  if [ "$(sha256sum "$f" | cut -d' ' -f1)" != "$2" ]; then
    echo "error: wrong SHA-256 for $(basename "$1")" >&2
    rm -f "$f"
    exit 1
  fi
  rm -rf "$work/src" && mkdir "$work/src"
  tar xf "$f" -C "$work/src" --strip-components 1
  cd "$work/src"
}

cmake_common="-G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$prefix
  -DCMAKE_INSTALL_LIBDIR=lib -DCMAKE_PREFIX_PATH=$prefix"

if has sdl; then
  echo "== SDL3 $SDL3_VERSION"
  fetch "https://github.com/libsdl-org/SDL/releases/download/release-$SDL3_VERSION/SDL3-$SDL3_VERSION.tar.gz" $SDL3_SHA256
  cmake -S . -B build $cmake_common -DSDL_SHARED=ON -DSDL_STATIC=OFF \
    -DSDL_TESTS=OFF -DSDL_EXAMPLES=OFF
  cmake --build build -j "$jobs"
  cmake --install build

  echo "== SDL3_ttf $SDL3_TTF_VERSION"
  fetch "https://github.com/libsdl-org/SDL_ttf/releases/download/release-$SDL3_TTF_VERSION/SDL3_ttf-$SDL3_TTF_VERSION.tar.gz" $SDL3_TTF_SHA256
  # the FreeType of the system, no HarfBuzz (the Vue port draws its text
  # with MuPDF)
  cmake -S . -B build $cmake_common -DBUILD_SHARED_LIBS=ON \
    -DSDLTTF_VENDORED=OFF -DSDLTTF_HARFBUZZ=OFF -DSDLTTF_PLUTOSVG=OFF \
    -DSDLTTF_SAMPLES=OFF -DSDLTTF_INSTALL_MAN=OFF
  cmake --build build -j "$jobs"
  cmake --install build
fi

if has mupdf; then
  echo "== MuPDF $MUPDF_VERSION"
  fetch "https://mupdf.com/downloads/archive/mupdf-$MUPDF_VERSION-source.tar.gz" $MUPDF_SHA256
  # its own third-party libraries (libmupdf-third), but FreeType and zlib
  # (those of TeXmacs); no viewers, no OCR; position independent, for the
  # programs of the distributions (PIE)
  make -j"$jobs" prefix="$prefix" build=release HAVE_X11=no HAVE_GLUT=no \
    HAVE_CURL=no USE_SYSTEM_LIBS=no USE_SYSTEM_FREETYPE=yes USE_SYSTEM_ZLIB=yes \
    XCFLAGS=-fPIC shared=no libs
  make prefix="$prefix" build=release HAVE_X11=no HAVE_GLUT=no \
    HAVE_CURL=no USE_SYSTEM_LIBS=no USE_SYSTEM_FREETYPE=yes USE_SYSTEM_ZLIB=yes \
    XCFLAGS=-fPIC shared=no install-libs
fi

if has thorvg; then
  echo "== ThorVG $THORVG_VERSION"
  rm -rf "$work/thorvg"
  THORVG_VERSION=$THORVG_VERSION sh "$here/../thorvg/build-thorvg.sh" "$work/thorvg"
  mkdir -p "$prefix/include" "$prefix/lib"
  cp -R "$work/thorvg/include/thorvg-1" "$prefix/include/"
  cp "$work/thorvg/lib/libthorvg-1.a" "$prefix/lib/"
fi

cd "$prefix"
rm -rf "$work"
echo "== the libraries are in $prefix"
