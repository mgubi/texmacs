#!/bin/bash
#
# Builds the libraries of TeXmacs which do not come with macOS (libpng,
# FreeType; GMP, libltdl and Guile 1.8 for Guile; SDL3, SDL3_ttf, MuPDF and
# ThorVG for the Vue port) from their sources, for one architecture and an
# old version of macOS, so that the application runs on this version and
# later ones (the libraries of Homebrew only run on the version of macOS
# where they were built). They are static, but for SDL3 and SDL3_ttf
# (copied into the application by bundle-libs.sh).
#
# Usage: packages/macos/build-deps.sh PREFIX [ARCH [MIN_MACOS [PARTS]]]
#   ARCH       arm64 or x86_64 (default: the architecture of the machine);
#              x86_64 on Apple silicon is built under Rosetta
#   MIN_MACOS  the oldest version of macOS (default: 12.0)
#   PARTS      what besides libpng and FreeType, separated by commas:
#              guile (default), vue; "none" for neither (with S7)
#
# Then configure TeXmacs with the same MACOSX_DEPLOYMENT_TARGET and
#   --with-guile=PREFIX/bin/guile-config --with-freetype=PREFIX/bin
#   [--with-sdl3 --with-mupdf=PREFIX --with-thorvg=PREFIX]
# (build-ns-app.sh --deps PREFIX does it). cmake, meson and ninja (for the
# Vue part) come from a Python venv in PREFIX/tools.

set -e

# the versions and the SHA-256 of their sources (the same as Homebrew's,
# except for Guile 1.8, which Homebrew no longer has, and libtool: the
# static libltdl of 2.6 needs an archive of its build, and Guile does not
# link with it)
GMP_VERSION=6.3.0
GMP_SHA256=a3c2b80201b89e68616f4ad30bc66aee4927c3ce50e33929ca819d5c43538898
LIBTOOL_VERSION=2.5.4
LIBTOOL_SHA256=f81f5860666b0bc7d84baddefa60d1cb9fa6fceb2398cc3baca6afaa60266675
LIBPNG_VERSION=1.6.58
LIBPNG_SHA256=28eb403f51f0f7405249132cecfe82ea5c0ef97f1b32c5a65828814ae0d34775
FREETYPE_VERSION=2.14.3
FREETYPE_SHA256=36bc4f1cc413335368ee656c42afca65c5a3987e8768cc28cf11ba775e785a5f
GUILE_VERSION=1.8.8
GUILE_SHA256=c3471fed2e72e5b04ad133bbaaf16369e8360283679bcf19800bc1b381024050
# the Vue port (those of Homebrew on 2026-10-05; ThorVG: its tag, cloned by
# misc/thorvg/build-thorvg.sh)
SDL3_VERSION=3.4.18
SDL3_SHA256=9c75cf16330322c217dedd2e0609f1124f1b54b8633e763467b4684d0f4334a3
SDL3_TTF_VERSION=3.2.2
SDL3_TTF_SHA256=63547d58d0185c833213885b635a2c0548201cc8f301e6587c0be1a67e1e045d
MUPDF_VERSION=1.28.5
MUPDF_SHA256=98a5c10cda20c3992cdf76ff6b2a1149c32bd79cc796d3f703230b1185b7e934
THORVG_VERSION=v1.1.2

prefix="$1"
arch="${2:-$(uname -m)}"
min="${3:-12.0}"
parts=",${4:-guile},"
has () { case "$parts" in *,$1,*) return 0;; *) return 1;; esac; }
case "$arch" in
  arm64|x86_64) ;;
  *) echo "usage: $0 PREFIX [arm64|x86_64 [MIN_MACOS]]" >&2; exit 1;;
esac
[ -n "$prefix" ] || { echo "usage: $0 PREFIX [ARCH [MIN_MACOS]]" >&2; exit 1; }

# x86_64 on Apple silicon: the whole build under Rosetta, so that the
# configure scripts see an x86_64 machine (and not a cross compilation)
if [ "$(uname -m)" != "$arch" ] && [ -z "$TM_DEPS_REEXEC" ]; then
  exec env TM_DEPS_REEXEC=1 arch -"$arch" /bin/bash "$0" "$prefix" "$arch" "$min" "${4:-guile}"
fi

mkdir -p "$prefix"
prefix=$(cd "$prefix" && pwd)
here=$(cd "$(dirname "$0")" && pwd)
work="$prefix/build"
mkdir -p "$work"

export MACOSX_DEPLOYMENT_TARGET="$min"
export CC="clang -arch $arch" CXX="clang++ -arch $arch"
export CFLAGS="-O2" CXXFLAGS="-O2"
export CPPFLAGS="-I$prefix/include" LDFLAGS="-L$prefix/lib"
# only the libraries built here, not the ones of Homebrew
export PATH="$prefix/bin:/usr/bin:/bin:/usr/sbin:/sbin"
export PKG_CONFIG_PATH="$prefix/lib/pkgconfig" PKG_CONFIG_LIBDIR="$prefix/lib/pkgconfig"
jobs=$(sysctl -n hw.ncpu)
# NOTE: make and make install on their own lines: set -e does not stop
# "make && make install" when make fails
common="--prefix=$prefix --disable-shared --enable-static --with-pic --disable-dependency-tracking"

fetch () { # url sha256
  local f="$work/$(basename "$1")"
  [ -s "$f" ] || curl -sSfL --retry 3 -o "$f" "$1"
  if [ "$(shasum -a 256 "$f" | cut -d' ' -f1)" != "$2" ]; then
    echo "error: wrong SHA-256 for $(basename "$1")" >&2
    rm -f "$f"
    exit 1
  fi
  rm -rf "$work/src" && mkdir "$work/src"
  tar xf "$f" -C "$work/src" --strip-components 1
  cd "$work/src"
}

echo "== libpng $LIBPNG_VERSION"
fetch "https://download.sourceforge.net/libpng/libpng-$LIBPNG_VERSION.tar.xz" $LIBPNG_SHA256
./configure $common
make -j"$jobs"
make install

echo "== FreeType $FREETYPE_VERSION"
fetch "https://download.savannah.gnu.org/releases/freetype/freetype-$FREETYPE_VERSION.tar.xz" $FREETYPE_SHA256
# zlib of macOS; libpng for the color emoji fonts (given here: there is no
# pkg-config)
./configure $common --enable-freetype-config --with-zlib=yes --with-png=yes \
  --with-bzip2=no --with-harfbuzz=no --with-brotli=no \
  LIBPNG_CFLAGS="-I$prefix/include/libpng16" LIBPNG_LIBS="-L$prefix/lib -lpng16 -lz"
make -j"$jobs"
make install

if has guile; then
  echo "== GMP $GMP_VERSION ($arch, macOS $min)"
  fetch "https://ftp.gnu.org/gnu/gmp/gmp-$GMP_VERSION.tar.xz" $GMP_SHA256
  # not tuned for the processor of the build machine: fat binaries on x86_64
  # (the code for each processor chosen at run time), generic code on arm64
  if [ "$arch" = x86_64 ]; then fat=--enable-fat; else fat=; fi
  ./configure $common $fat
  make -j"$jobs"
  make install

  echo "== libltdl (libtool $LIBTOOL_VERSION)"
  fetch "https://ftp.gnu.org/gnu/libtool/libtool-$LIBTOOL_VERSION.tar.xz" $LIBTOOL_SHA256
  ./configure $common --enable-ltdl-install
  make -j"$jobs"
  make install

  echo "== Guile $GUILE_VERSION"
  fetch "https://ftp.gnu.org/gnu/guile/guile-$GUILE_VERSION.tar.gz" $GUILE_SHA256
  ./configure $common --disable-error-on-warning
  # not guile-readline, which TeXmacs does not use (and which finds the
  # readline.h of libedit)
  sed -i '' 's/ guile-readline / /' Makefile
  make -j"$jobs"
  make install

fi

if has vue; then
  # the tools of the builds (cmake, meson, ninja) for this architecture
  if [ ! -x "$prefix/tools/bin/meson" ]; then
    /usr/bin/python3 -m venv "$prefix/tools"
    "$prefix/tools/bin/pip" -q install cmake meson ninja
  fi
  tools="$prefix/tools/bin"
  # NOTE: SDL3 and SDL3_ttf as libraries of their own, with their place as
  # their name (bundle-libs.sh copies them into the application and changes
  # it): with the static ones, configure would need the libraries of theirs
  cmake_common="-DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$prefix
    -DCMAKE_OSX_ARCHITECTURES=$arch -DCMAKE_OSX_DEPLOYMENT_TARGET=$min
    -DCMAKE_INSTALL_NAME_DIR=$prefix/lib -DCMAKE_MACOSX_RPATH=OFF
    -DCMAKE_PREFIX_PATH=$prefix -G Ninja -DCMAKE_MAKE_PROGRAM=$tools/ninja"

  echo "== SDL3 $SDL3_VERSION"
  fetch "https://github.com/libsdl-org/SDL/releases/download/release-$SDL3_VERSION/SDL3-$SDL3_VERSION.tar.gz" $SDL3_SHA256
  "$tools/cmake" -S . -B build $cmake_common -DSDL_SHARED=ON -DSDL_STATIC=OFF \
    -DSDL_TESTS=OFF -DSDL_EXAMPLES=OFF
  "$tools/cmake" --build build -j "$jobs"
  "$tools/cmake" --install build

  echo "== SDL3_ttf $SDL3_TTF_VERSION"
  fetch "https://github.com/libsdl-org/SDL_ttf/releases/download/release-$SDL3_TTF_VERSION/SDL3_ttf-$SDL3_TTF_VERSION.tar.gz" $SDL3_TTF_SHA256
  # the FreeType built above (static, with libpng and the zlib of macOS);
  # no HarfBuzz (the Vue port draws its text with MuPDF)
  "$tools/cmake" -S . -B build $cmake_common -DBUILD_SHARED_LIBS=ON \
    -DSDLTTF_VENDORED=OFF -DSDLTTF_HARFBUZZ=OFF -DSDLTTF_PLUTOSVG=OFF \
    -DSDLTTF_SAMPLES=OFF -DSDLTTF_INSTALL_MAN=OFF \
    -DFREETYPE_INCLUDE_DIRS="$prefix/include/freetype2" \
    -DFREETYPE_LIBRARY="$prefix/lib/libfreetype.a" \
    -DCMAKE_SHARED_LINKER_FLAGS="$prefix/lib/libpng16.a -lz"
  "$tools/cmake" --build build -j "$jobs"
  "$tools/cmake" --install build

  echo "== MuPDF $MUPDF_VERSION"
  fetch "https://mupdf.com/downloads/archive/mupdf-$MUPDF_VERSION-source.tar.gz" $MUPDF_SHA256
  # its own third-party libraries (libmupdf-third), but FreeType (the one
  # of TeXmacs) and zlib (of macOS); no viewers, no OCR
  mupdf_make="prefix=$prefix build=release HAVE_X11=no HAVE_GLUT=no
    HAVE_CURL=no USE_TESSERACT=no USE_SYSTEM_LIBS=no
    USE_SYSTEM_FREETYPE=yes SYS_FREETYPE_CFLAGS=-I$prefix/include/freetype2
    SYS_FREETYPE_LIBS=-L$prefix/lib\ -lfreetype\ -lpng16\ -lz
    USE_SYSTEM_ZLIB=yes SYS_ZLIB_CFLAGS= SYS_ZLIB_LIBS=-lz
    XCFLAGS=-arch\ $arch XLDFLAGS=-arch\ $arch shared=no verbose=yes"
  eval make -j"$jobs" $mupdf_make libs
  eval make $mupdf_make install-libs
  # NOTE: only the headers and the libraries (install-libs), not the tools

  echo "== ThorVG $THORVG_VERSION"
  # its GL engine, static (misc/thorvg/build-thorvg.sh, with the compilers
  # and the version of macOS above)
  rm -rf "$work/thorvg"
  PATH="$tools:$PATH" THORVG_VERSION=$THORVG_VERSION \
    sh "$here/../../misc/thorvg/build-thorvg.sh" "$work/thorvg"
  mkdir -p "$prefix/include" "$prefix/lib"
  cp -R "$work/thorvg/include/thorvg-1" "$prefix/include/"
  cp "$work/thorvg/lib/libthorvg-1.a" "$prefix/lib/"
fi

rm -rf "$work"
echo "== the libraries are in $prefix"
