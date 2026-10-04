#!/bin/sh
# Asymptote-web (https://github.com/Julieisbaka/Asymptote-web, LGPL-3.0):
# Asymptote compiled to WebAssembly, the program of the Asymptote plugin in
# the browser (plugins/asymptote, docs/wasm/asymptote.md). The npm package of
# a pinned version, checked, unpacked in the build directory.
#
#   sh misc/wasm/get-asymptote.sh [build directory]     (default: build-wasm)
#
# The Makefile copies what the worker needs of it (asymptote.js,
# asymptote.wasm, asy.data, asymptote-web.js, utils.js) to
# out/web/asymptote/. A new version: also ASYMPTOTE in misc/wasm/Makefile
# (the session tells it) and the version named in the help of the plugin.

set -e
V=0.3.3
SHA=46baf8ed2faaec1f3ae04d8b923b19e5642d84803f830cb90a502767438622a1
DIR="${1:-build-wasm}"
DEST="$DIR/asymptote-web-$V"
[ -f "$DEST/dist/asymptote.wasm" ] && { echo "asymptote-web $V: $DEST"; exit 0; }
mkdir -p "$DIR"
TGZ="$DIR/asymptote-web-$V.tgz"
curl -fsSL -o "$TGZ" "https://registry.npmjs.org/asymptote-web/-/asymptote-web-$V.tgz"
GOT=$(shasum -a 256 "$TGZ" 2>/dev/null || sha256sum "$TGZ")
GOT=${GOT%% *}
if [ "$GOT" != "$SHA" ]; then
  echo "asymptote-web $V: wrong checksum ($GOT)" >&2
  rm -f "$TGZ"
  exit 1
fi
rm -rf "$DEST"; mkdir -p "$DEST"
tar xzf "$TGZ" -C "$DEST" --strip-components=1 package/dist package/package.json package/README.md
rm -f "$TGZ"
echo "asymptote-web $V: $DEST"
