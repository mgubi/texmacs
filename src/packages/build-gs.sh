#!/bin/bash
#
# Builds Ghostscript, the program gs which TeXmacs runs to convert EPS and
# PostScript images and to write PostScript (#662), from its sources, for
# the packages of TeXmacs Upsilon: macOS (one architecture, for an old
# version of macOS, as packages/macos/build-deps.sh), Linux (the AppImages,
# built on Ubuntu 22.04) and Windows (MSYS2, MinGW-w64).
#
# Usage: packages/build-gs.sh PREFIX [ARCH [MIN_MACOS]]   (from the src directory)
#   ARCH       macOS only: arm64 or x86_64 (default: the architecture of the
#              machine; x86_64 on Apple silicon is built under Rosetta)
#   MIN_MACOS  macOS only: the oldest version of macOS (default: 13.0)
#
# It makes PREFIX/bin/gs (PREFIX/bin/gs.exe on Windows) and nothing else:
# the program has its initialization files, its resources, the 35 base
# fonts (URW) and its color profiles compiled into it (the default of the
# configure script, --enable-compile-inits), so it needs no folder of
# resources and no environment variable, and finds nothing of a Ghostscript
# which the system may have. Its other libraries (FreeType, libpng, libjpeg,
# zlib, Little CMS, OpenJPEG, jbig2dec, libtiff) are those of its sources,
# linked statically; on macOS it only uses the libraries of the system.
# PREFIX/share/licenses/ghostscript has the license of the sources.
#
# Ghostscript is under the GNU Affero General Public License 3 (the program
# is not modified here: TeXmacs runs it as a program of its own, beside the
# GPL 3 program, which is mere aggregation). The sources of the version in
# the packages are the archive given below, which the release of the
# packages keeps (misc/licenses/README.md of the TeXmacs folder).

set -e

# the version of the official builder of TeXmacs for Windows
# (misc/builder/scripts/os/windows-qt6/archives-urls.txt)
GS_VERSION=10.07.0
GS_SHA256=ed6ea62022e3f4d5a6569b6efc9361b63a6d118bfcad8f0beb897c37885b5cad
GS_URL=https://github.com/ArtifexSoftware/ghostpdl-downloads/releases/download/gs${GS_VERSION//./}/ghostscript-$GS_VERSION.tar.gz

prefix="$1"
[ -n "$prefix" ] || { echo "usage: $0 PREFIX [ARCH [MIN_MACOS]]" >&2; exit 1; }
os=$(uname -s)
arch="${2:-$(uname -m)}"
min="${3:-13.0}"

# x86_64 on Apple silicon: the whole build under Rosetta, so that the
# configure script sees an x86_64 machine
if [ "$os" = Darwin ] && [ "$(uname -m)" != "$arch" ] && [ -z "$TM_GS_REEXEC" ]; then
  exec env TM_GS_REEXEC=1 arch -"$arch" /bin/bash "$0" "$prefix" "$arch" "$min"
fi

mkdir -p "$prefix"
prefix=$(cd "$prefix" && pwd)
work="$prefix/build"
rm -rf "$work"
mkdir -p "$work"
jobs=$(nproc 2> /dev/null || sysctl -n hw.ncpu)

sha256 () { # file
  if command -v sha256sum > /dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1
  else shasum -a 256 "$1" | cut -d' ' -f1; fi
}

echo "== Ghostscript $GS_VERSION"
f="$work/ghostscript-$GS_VERSION.tar.gz"
curl -sSfL --retry 3 -o "$f" "$GS_URL"
if [ "$(sha256 "$f")" != "$GS_SHA256" ]; then
  echo "error: wrong SHA-256 for ghostscript-$GS_VERSION.tar.gz" >&2
  exit 1
fi
mkdir "$work/src"
tar xf "$f" -C "$work/src" --strip-components 1
cd "$work/src"

# no X, CUPS, GTK, fontconfig, D-Bus, tesseract, libpaper, libidn (the
# libraries of the system which it would otherwise find and link)
opts="--without-x --without-tesseract --disable-cups --disable-gtk
  --disable-fontconfig --disable-dbus --disable-threading
  --without-libpaper --without-libidn"
case "$os" in
  Darwin)
    export MACOSX_DEPLOYMENT_TARGET="$min"
    export CC="clang -arch $arch" CXX="clang++ -arch $arch"
    export CFLAGS="-O2" CXXFLAGS="-O2"
    # only the tools of the system (no libraries of Homebrew)
    export PATH="/usr/bin:/bin:/usr/sbin:/sbin"
    opts="$opts --with-libiconv=native";;
esac
# the prefix named in gscdefs.c (the default folders of the program, unused
# here): a Windows path of MSYS2 has backslashes, which C takes as escapes
case "$os" in
  MINGW*|MSYS*) opts="$opts --prefix=$(cygpath -m "$prefix")";;
esac
# NOTE: configure, make and make install on their own lines: set -e does
# not stop "make && make install" when make fails
./configure $opts
make -j"$jobs"

mkdir -p "$prefix/bin" "$prefix/share/licenses/ghostscript"
case "$os" in
  MINGW*|MSYS*) exe=gs.exe;;
  *) exe=gs;;
esac
[ -f "bin/$exe" ] || { echo "error: the build made no bin/$exe" >&2; ls bin; exit 1; }
cp "bin/$exe" "$prefix/bin/$exe"
[ "$os" = Darwin ] || strip "$prefix/bin/$exe"
cp LICENSE "$prefix/share/licenses/ghostscript/LICENSE"
cp doc/COPYING "$prefix/share/licenses/ghostscript/COPYING"

echo "== $prefix/bin/$exe"
ls -l "$prefix/bin/$exe"
"$prefix/bin/$exe" --version
case "$os" in
  Darwin) otool -L "$prefix/bin/$exe"
          otool -l "$prefix/bin/$exe" | grep -A3 LC_BUILD_VERSION;;
  Linux)  ldd "$prefix/bin/$exe";;
  *)      ldd "$prefix/bin/$exe" || true;;
esac

cd "$prefix"
rm -rf "$work"
