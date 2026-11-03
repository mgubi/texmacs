#!/bin/sh
# Hunspell (https://github.com/hunspell/hunspell, MPL 1.1/GPL 2/LGPL 2.1):
# the spell checker of the browser build, compiled into the program
# (src/Plugins/Ispell/ispell_hunspell.cpp). The sources of a pinned
# release, checked, unpacked in the build directory; the dictionaries are
# fetched by the page when a language is checked.
#
#   sh misc/wasm/get-hunspell.sh [build directory]     (default: build-wasm)

set -e
V=1.7.2
SHA=11ddfa39afe28c28539fe65fc4f1592d410c1e9b6dd7d8a91ca25d85e9ec65b8
DIR="${1:-build-wasm}"
DEST="$DIR/hunspell-$V"
[ -f "$DEST/src/hunspell/hunspell.cxx" ] && { echo "hunspell $V: $DEST"; exit 0; }
mkdir -p "$DIR"
TGZ="$DIR/hunspell-$V.tar.gz"
curl -fsSL -o "$TGZ" "https://github.com/hunspell/hunspell/releases/download/v$V/hunspell-$V.tar.gz"
GOT=$(shasum -a 256 "$TGZ" 2>/dev/null || sha256sum "$TGZ")
GOT=${GOT%% *}
if [ "$GOT" != "$SHA" ]; then
  echo "hunspell $V: wrong checksum ($GOT)" >&2
  rm -f "$TGZ"
  exit 1
fi
rm -rf "$DEST"; mkdir -p "$DEST"
tar xzf "$TGZ" -C "$DEST" --strip-components=1 hunspell-$V/src/hunspell hunspell-$V/COPYING hunspell-$V/COPYING.LESSER hunspell-$V/COPYING.MPL hunspell-$V/license.hunspell hunspell-$V/license.myspell
rm -f "$TGZ"
echo "hunspell $V: $DEST"
