#!/bin/sh
# TikZJax (https://github.com/rod2ik/tikzjax, GPL-3.0), the TeX of the TikZ
# plugin in the browser (plugins/tikz, src/docs/wasm/tikzjax.md): the npm
# package of a pinned version, checked, unpacked in the build directory.
#
#   sh misc/wasm/get-tikzjax.sh [build directory]     (default: build-wasm)
#
# The Makefile copies what the worker needs of it (run-tex.js, tex.wasm.gz,
# core.dump.gz, tex_files/) to out/web/tikzjax/: not its fonts, nor the part
# of its pages (the text of a picture is typeset by TeXmacs).

set -e
# a new version: also TIKZJAX in misc/wasm/Makefile (the session tells it)
# and the version named in plugins/tikz/doc/tikz-browser.en.tm
V=1.6.0
SHA=ca7d979a89136910d7f149810dd83b07d68c87fe9000fbb4ed86e28b5d780eed
DIR="${1:-build-wasm}"
DEST="$DIR/tikzjax-$V"
[ -f "$DEST/dist/run-tex.js" ] && { echo "tikzjax $V: $DEST"; exit 0; }
mkdir -p "$DIR"
TGZ="$DIR/tikzjax-$V.tgz"
curl -fsSL -o "$TGZ" "https://registry.npmjs.org/@rod2ik/tikzjax/-/tikzjax-$V.tgz"
GOT=$(shasum -a 256 "$TGZ" 2>/dev/null || sha256sum "$TGZ")
GOT=${GOT%% *}
if [ "$GOT" != "$SHA" ]; then
  echo "tikzjax $V: wrong checksum ($GOT)" >&2
  rm -f "$TGZ"
  exit 1
fi
rm -rf "$DEST"; mkdir -p "$DEST"
tar xzf "$TGZ" -C "$DEST" --strip-components=1 package/dist package/LICENSE
rm -f "$TGZ"
echo "tikzjax $V: $DEST"
