#!/bin/bash
#
# Builds the libraries of TeXmacs which do not come with macOS (GMP,
# libltdl, libpng, FreeType, Guile 1.8) from their sources, as static
# libraries for one architecture and an old version of macOS, so that the
# application runs on this version and later ones (the libraries of
# Homebrew only run on the version of macOS where they were built).
#
# Usage: packages/macos/build-deps.sh PREFIX [ARCH [MIN_MACOS]]
#   ARCH       arm64 or x86_64 (default: the architecture of the machine);
#              x86_64 on Apple silicon is built under Rosetta
#   MIN_MACOS  the oldest version of macOS (default: 12.0)
#
# Then configure TeXmacs with the same MACOSX_DEPLOYMENT_TARGET and
#   --with-guile=PREFIX/bin/guile-config --with-freetype=PREFIX/bin
# (build-ns-app.sh --deps PREFIX does it).

set -e

GMP_VERSION=6.3.0
LIBTOOL_VERSION=2.5.4
LIBPNG_VERSION=1.6.50
FREETYPE_VERSION=2.13.3
GUILE_VERSION=1.8.8

prefix="$1"
arch="${2:-$(uname -m)}"
min="${3:-12.0}"
case "$arch" in
  arm64|x86_64) ;;
  *) echo "usage: $0 PREFIX [arm64|x86_64 [MIN_MACOS]]" >&2; exit 1;;
esac
[ -n "$prefix" ] || { echo "usage: $0 PREFIX [ARCH [MIN_MACOS]]" >&2; exit 1; }

# x86_64 on Apple silicon: the whole build under Rosetta, so that the
# configure scripts see an x86_64 machine (and not a cross compilation)
if [ "$(uname -m)" != "$arch" ] && [ -z "$TM_DEPS_REEXEC" ]; then
  exec env TM_DEPS_REEXEC=1 arch -"$arch" /bin/bash "$0" "$prefix" "$arch" "$min"
fi

mkdir -p "$prefix"
prefix=$(cd "$prefix" && pwd)
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
common="--prefix=$prefix --disable-shared --enable-static --with-pic --disable-dependency-tracking"

fetch () { # url
  local f="$work/$(basename "$1")"
  [ -s "$f" ] || curl -sSfL --retry 3 -o "$f" "$1"
  rm -rf "$work/src" && mkdir "$work/src"
  tar xf "$f" -C "$work/src" --strip-components 1
  cd "$work/src"
}

echo "== GMP $GMP_VERSION ($arch, macOS $min)"
fetch "https://ftp.gnu.org/gnu/gmp/gmp-$GMP_VERSION.tar.xz"
# not tuned for the processor of the build machine: fat binaries on x86_64
# (the code for each processor chosen at run time), generic code on arm64
if [ "$arch" = x86_64 ]; then fat=--enable-fat; else fat=; fi
./configure $common $fat
make -j"$jobs" && make install

echo "== libltdl (libtool $LIBTOOL_VERSION)"
fetch "https://ftp.gnu.org/gnu/libtool/libtool-$LIBTOOL_VERSION.tar.xz"
./configure $common --enable-ltdl-install
make -j"$jobs" && make install

echo "== libpng $LIBPNG_VERSION"
fetch "https://download.sourceforge.net/libpng/libpng-$LIBPNG_VERSION.tar.xz"
./configure $common
make -j"$jobs" && make install

echo "== FreeType $FREETYPE_VERSION"
fetch "https://download.savannah.gnu.org/releases/freetype/freetype-$FREETYPE_VERSION.tar.xz"
# zlib of macOS; libpng for the color emoji fonts (given here: there is no
# pkg-config)
./configure $common --enable-freetype-config --with-zlib=yes --with-png=yes \
  --with-bzip2=no --with-harfbuzz=no --with-brotli=no \
  LIBPNG_CFLAGS="-I$prefix/include/libpng16" LIBPNG_LIBS="-L$prefix/lib -lpng16 -lz"
make -j"$jobs" && make install

echo "== Guile $GUILE_VERSION"
fetch "https://ftp.gnu.org/gnu/guile/guile-$GUILE_VERSION.tar.gz"
./configure $common --disable-error-on-warning
# not guile-readline, which TeXmacs does not use (and which finds the
# readline.h of libedit)
sed -i '' 's/ guile-readline / /' Makefile
make -j"$jobs" && make install

rm -rf "$work"
echo "== the libraries are in $prefix"
