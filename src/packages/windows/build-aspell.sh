#!/bin/bash
#
# Builds Aspell, the spell checker, and its dictionaries for the Windows
# packages of TeXmacs Upsilon (MSYS2, MinGW-w64; it also runs on Linux and
# macOS, to try it), as the official builder of TeXmacs
# (misc/builder/scripts/os/windows-qt6) does: the library of Aspell 0.60.8.2,
# static, which TeXmacs links (configure finds aspell.h and libaspell: --with-aspell),
# and its dictionaries, which the package has in the folder aspell-0.60 of
# the TeXmacs folder: ispell.cpp (USE_ASPELL) gives that folder to Aspell as
# its data and dictionary folder when it exists. No program is needed:
# TeXmacs does not run aspell.exe when it has the library.
#
# Usage: packages/windows/build-aspell.sh PREFIX
#   PREFIX/include/aspell.h, PREFIX/lib/libaspell.a   what TeXmacs links
#   PREFIX/lib/aspell-0.60                            the folder of the package
#   PREFIX/share/licenses/aspell                      the licenses to ship
#   PREFIX/bin/aspell                                 (to try, not shipped)
#
# The dictionaries are a choice among the ones of the official builder
# (download_dictionaries.py has all 90, 100 MB or more of them installed;
# ftp.gnu.org/gnu/aspell/dict has the same archives): the languages of
# the TeXmacs interface and documentation that have an Aspell dictionary,
# without the big dictionaries of the others (Greek and Danish are 10 to
# 19 MB each). Aspell is under the GNU Lesser General Public License 2.1
# (the library, linked into texmacs.exe: the sources of the package are
# those of TeXmacs, and the archive below), the dictionaries are under
# various free licenses, whose texts are in PREFIX/share/licenses/aspell.

set -e

ASPELL_VERSION=0.60.8.2
ASPELL_SHA256=57fe4863eae6048f72245a8575b44b718fb85ca14b9f8c0afc41b254dfd76919
ASPELL_URL=https://ftp.gnu.org/gnu/aspell/aspell-$ASPELL_VERSION.tar.gz
DICT_URL=https://ftp.gnu.org/gnu/aspell/dict
# language, archive, SHA-256 (the same as the archives of ftp.texmacs.org
# which the official builder downloads)
DICTIONARIES="
en aspell6-en-2020.12.07-0 4c8f734a28a088b88bb6481fcf972d0b2c3dc8da944f7673283ce487eac49fb3
fr aspell-fr-0.50-3 f9421047519d2af9a7a466e4336f6e6ea55206b356cd33c8bd18cb626bf2ce91
de aspell6-de-20161207-7-0 c2125d1fafb1d4effbe6c88d4e9127db59da9ed92639c7cbaeae1b7337655571
es aspell6-es-1.11-2 ad367fa1e7069c72eb7ae37e4d39c30a44d32a6aa73cedccbd0d06a69018afcc
it aspell6-it-2.2_20050523-0 3b19dc709924783c8d87111aa9653dc6c000e845183778abee750215d83aaebd
pt_BR aspell6-pt_BR-20131030-12-0 eb0d99db0b5d5c442133a88bddfe96dd252c0c3df3da36e9326c241dc4bc14f7
pt_PT aspell6-pt_PT-20190329-1-0 e5708b890c2afff51276a6cc276af5e6b3b8a026db75eda48b58124f2368a051
nl aspell-nl-0.50-2 440e0b7df8c5903d728221fe4ba88a74658ce14c8bb04b290c41402dfd41cb39
ru aspell6-ru-0.99f7-1 5c29b6ccce57bc3f7c4fb0510d330446b9c769e59c92bdfede27333808b6e646
pl aspell6-pl-6.0_20061121-0 017741fcb70a885d718c534160c9de06b03cc72f352879bd106be165e024574d
uk aspell6-uk-1.4.0-0 35f9a7e840c1272706bc6dd172bc9625cbd843d021094da8598a6abba525f18c
"

prefix="$1"
[ -n "$prefix" ] || { echo "usage: $0 PREFIX" >&2; exit 1; }
mkdir -p "$prefix"
prefix=$(cd "$prefix" && pwd)
work="$prefix/build"
rm -rf "$work"
mkdir -p "$work"
jobs=$(nproc 2> /dev/null || sysctl -n hw.ncpu)
lic="$prefix/share/licenses/aspell"
mkdir -p "$lic"

sha256 () { # file
  if command -v sha256sum > /dev/null 2>&1; then sha256sum "$1" | cut -d' ' -f1
  else shasum -a 256 "$1" | cut -d' ' -f1; fi
}
fetch () { # url sha256: downloaded into $work, which is the result
  local f="$work/$(basename "$1")"
  curl -sSfL --retry 3 -o "$f" "$1"
  if [ "$(sha256 "$f")" != "$2" ]; then
    echo "error: wrong SHA-256 for $(basename "$1")" >&2
    exit 1
  fi
  echo "$f"
}

echo "== Aspell $ASPELL_VERSION"
f=$(fetch "$ASPELL_URL" $ASPELL_SHA256)
mkdir "$work/aspell"
tar xf "$f" -C "$work/aspell" --strip-components 1
cd "$work/aspell"
# static (it goes into texmacs.exe), no translations
./configure --prefix="$prefix" --enable-static --disable-shared --disable-nls
make -j"$jobs"
make install
cp COPYING "$lic/Aspell-COPYING"

export PATH="$prefix/bin:$PATH"
echo "$DICTIONARIES" | while read -r lang name sha; do
  [ -n "$lang" ] || continue
  echo "== dictionary $lang ($name)"
  f=$(fetch "$DICT_URL/$lang/$name.tar.bz2" "$sha")
  mkdir "$work/$name"
  tar xjf "$f" -C "$work/$name" --strip-components 1
  (cd "$work/$name"
   # each dictionary comes with its license (Copyright, COPYING...)
   for t in Copyright COPYING README; do
     [ -f $t ] && cp $t "$lic/dict-$lang-$t"
   done
   sh ./configure
   make
   make install)
done

echo "== $prefix"
ls "$prefix/lib" "$prefix/lib/aspell-0.60" | head -20
du -sk "$prefix/lib/aspell-0.60"
echo en | "$prefix/bin/aspell" -a --lang=en_US
cd "$prefix"
rm -rf "$work"
