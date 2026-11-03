#!/bin/sh
#
# Build the ThorVG benchmark for the browser:
#
#   sh misc/thorvg-bench/build.sh <work directory>
#
# from the top of the source tree, with Emscripten in the PATH (for
# instance after ". misc/wasm/emenv.sh <dir>" in a tree which has it). It
# fetches ThorVG, builds it with its CPU and GL engines (meson and ninja in
# a Python venv of the work directory), and writes the page to
# <work directory>/out/texmacs.html (the name misc/wasm/browser-run.mjs
# opens). See README.md for the modes and how to run them.

set -e
WORK=${1:?usage: build.sh <work directory>}
THORVG_VERSION=${THORVG_VERSION:-v1.1.2}
HERE=$(cd "$(dirname "$0")" && pwd)
TM=$(cd "$HERE/../.." && pwd)
mkdir -p "$WORK"; WORK=$(cd "$WORK" && pwd)

if [ ! -x "$WORK/venv/bin/meson" ]; then
  python3 -m venv "$WORK/venv"
  "$WORK/venv/bin/pip" -q install meson ninja
fi
[ -d "$WORK/thorvg" ] ||
  git clone -q --depth 1 --branch "$THORVG_VERSION" https://github.com/thorvg/thorvg.git "$WORK/thorvg"

cat > "$WORK/wasm32.txt" <<EOF
[binaries]
c = 'emcc'
cpp = 'em++'
ar = 'emar'
strip = 'emstrip'

[built-in options]
c_args = ['-O2']
cpp_args = ['-O2']

[host_machine]
system = 'emscripten'
cpu_family = 'wasm32'
cpu = 'wasm32'
endian = 'little'
EOF

if [ ! -f "$WORK/build-wasm/build.ninja" ]; then
  (cd "$WORK/thorvg" && PATH="$WORK/venv/bin:$PATH" meson setup "$WORK/build-wasm" \
     --cross-file "$WORK/wasm32.txt" -Dengines=cpu,gl -Dloaders= -Dthreads=false \
     -Dfile=false -Dextra= -Dstatic=true -Ddefault_library=static -Dbuildtype=release)
fi
PATH="$WORK/venv/bin:$PATH" ninja -C "$WORK/build-wasm"

mkdir -p "$WORK/out"
cp "$TM/TeXmacs/fonts/truetype/texgyre/texgyrepagella-regular.otf" "$WORK/font.otf"
em++ -O2 -std=c++17 "$HERE/bench.cpp" -o "$WORK/out/texmacs.html" \
  -I"$WORK/thorvg/inc" "$WORK/build-wasm/src/libthorvg-1.a" \
  -sUSE_FREETYPE=1 -sMAX_WEBGL_VERSION=2 -sMIN_WEBGL_VERSION=2 -sFULL_ES3=1 \
  -sALLOW_MEMORY_GROWTH=1 -sEXPORTED_RUNTIME_METHODS=stringToNewUTF8 \
  --embed-file "$WORK/font.otf@/font.otf" --shell-file "$HERE/shell.html"
echo "built $WORK/out/texmacs.html"
