#!/bin/sh
#
# Build ThorVG for the GPU renderer of the Vue port:
#
#   sh misc/thorvg/build-thorvg.sh <prefix> [wasm]
#
# It fetches ThorVG (THORVG_VERSION, default v1.1.2) into <prefix>/src,
# builds it as a static library with its GL engine only (no loaders, no
# threads: TeXmacs gives it paths, and draws the glyphs and the images
# itself) and installs it in <prefix> (include/thorvg-1/thorvg.h,
# lib/libthorvg-1.a). With "wasm" it is built with Emscripten (emcc in the
# PATH, e.g. after ". misc/wasm/emenv.sh build-wasm") for WebGL2. meson and
# ninja come from a Python venv in <prefix>/venv. Then configure with
# --with-thorvg=<prefix>.

set -e
PREFIX=${1:?usage: build-thorvg.sh <prefix> [wasm]}
TARGET=${2:-native}
THORVG_VERSION=${THORVG_VERSION:-v1.1.2}
mkdir -p "$PREFIX"; PREFIX=$(cd "$PREFIX" && pwd)

# NOTE: meson and ninja of the PATH when there are both (MSYS2, where pip
# has no ninja), otherwise in a venv
if [ ! -x "$PREFIX/venv/bin/meson" ] &&
   ! { command -v meson > /dev/null && command -v ninja > /dev/null; }; then
  PY=python3
  for p in /opt/homebrew/opt/python@3.13/bin/python3.13 /opt/homebrew/bin/python3; do
    [ -x "$p" ] && { PY=$p; break; }
  done
  "$PY" -m venv "$PREFIX/venv"
  "$PREFIX/venv/bin/pip" -q install meson ninja
fi
[ -d "$PREFIX/src" ] ||
  git clone -q --depth 1 --branch "$THORVG_VERSION" https://github.com/thorvg/thorvg.git "$PREFIX/src"

OPTS="-Dengines=gl -Dloaders= -Dthreads=false -Dfile=false -Dextra= -Dstatic=true
      -Ddefault_library=static -Dbuildtype=release --libdir=lib"
BUILD="$PREFIX/build-$TARGET"
if [ ! -f "$BUILD/build.ninja" ]; then
  if [ "$TARGET" = wasm ]; then
    # the exceptions of the TeXmacs build (misc/wasm/Makefile, those of MuPDF)
    cat > "$PREFIX/wasm32.txt" <<EOT
[binaries]
c = 'emcc'
cpp = 'em++'
ar = 'emar'
strip = 'emstrip'

[built-in options]
c_args = ['-O2', '-fwasm-exceptions']
cpp_args = ['-O2', '-fwasm-exceptions']

[host_machine]
system = 'emscripten'
cpu_family = 'wasm32'
cpu = 'wasm32'
endian = 'little'
EOT
    (cd "$PREFIX/src" && PATH="$PREFIX/venv/bin:$PATH" meson setup "$BUILD" \
       --cross-file "$PREFIX/wasm32.txt" $OPTS --prefix="$PREFIX/wasm")
  else
    (cd "$PREFIX/src" && PATH="$PREFIX/venv/bin:$PATH" meson setup "$BUILD" $OPTS --prefix="$PREFIX")
  fi
fi
PATH="$PREFIX/venv/bin:$PATH" ninja -C "$BUILD" install > /dev/null
echo "ThorVG ($TARGET) installed in $( [ "$TARGET" = wasm ] && echo "$PREFIX/wasm" || echo "$PREFIX" )"
