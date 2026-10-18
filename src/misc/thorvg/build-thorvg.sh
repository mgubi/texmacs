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
# PATH, e.g. after ". misc/wasm/emenv.sh build-wasm") with its GL engine
# (WebGL2) and its WebGPU engine (Dawn's webgpu.h for Emscripten,
# emdawnwebgpu: the page draws with WebGPU when the browser has it, see
# gpu_backend_wg.cpp); THORVG_ENGINES=gl for the GL engine alone. meson and
# ninja come from a Python venv in <prefix>/venv. Then configure with
# --with-thorvg=<prefix> (or let configure find it through pkg-config:
# PKG_CONFIG_PATH=<prefix>/lib/pkgconfig).
#
# With "wasix" it is built with wasixcc (in ~/.wasixcc/bin, or WASIXCC) for
# the WASIX build (misc/wasix), with its WebGPU engine instead: webgpu.h is
# that of WEBGPU_HEADERS (build-wasix/webgpu-headers), whose functions the
# WebGPU of WASIX gives (misc/wasix/webgpu/webgpu_wire.cpp).

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

ENGINES=gl
[ "$TARGET" = wasm ] && ENGINES=${THORVG_ENGINES:-gl,wg}
[ "$TARGET" = wasix ] && ENGINES=wg
# (webgpu.h for Emscripten: the port emdawnwebgpu, at compile and link time)
EMWG=
case "$ENGINES" in *wg*) EMWG=", '--use-port=emdawnwebgpu'";; esac
OPTS="-Dengines=$ENGINES -Dloaders= -Dthreads=false -Dfile=false -Dextra= -Dstatic=true
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
c_args = ['-O2', '-fwasm-exceptions'$EMWG]
cpp_args = ['-O2', '-fwasm-exceptions'$EMWG]

[host_machine]
system = 'emscripten'
cpu_family = 'wasm32'
cpu = 'wasm32'
endian = 'little'
EOT
    (cd "$PREFIX/src" && PATH="$PREFIX/venv/bin:$PATH" meson setup "$BUILD" \
       --cross-file "$PREFIX/wasm32.txt" $OPTS --prefix="$PREFIX/wasm")
  elif [ "$TARGET" = wasix ]; then
    W=${WASIXCC:-$HOME/.wasixcc/bin}
    WH=${WEBGPU_HEADERS:?WEBGPU_HEADERS: the directory of webgpu/webgpu.h}
    WH=$(cd "$WH" && pwd)
    # the dependency wgpu_native of the WebGPU engine: the headers only
    mkdir -p "$PREFIX/wasix-pc"
    printf 'Name: wgpu_native\nDescription: webgpu.h of the WebGPU of WASIX\nVersion: 29.0.1.1\nCflags: -I%s\nLibs:\n' \
      "$WH" > "$PREFIX/wasix-pc/wgpu_native.pc"
    # the exceptions of WebAssembly, as the WASIX build (misc/wasix/Makefile)
    cat > "$PREFIX/wasix.txt" <<EOT
[binaries]
c = '$W/wasixcc'
cpp = '$W/wasix++'
ar = '$W/wasixar'
pkg-config = 'pkg-config'

[properties]
pkg_config_libdir = '$PREFIX/wasix-pc'

[built-in options]
c_args = ['-O2', '-sWASM_EXCEPTIONS=yes']
cpp_args = ['-O2', '-sWASM_EXCEPTIONS=yes']

[host_machine]
system = 'wasi'
cpu_family = 'wasm32'
cpu = 'wasm32'
endian = 'little'
EOT
    (cd "$PREFIX/src" && WASIXCC_WASM_EXCEPTIONS=yes WASIXCC_RUN_WASM_OPT=no \
       PATH="$PREFIX/venv/bin:$PATH" meson setup "$BUILD" \
       --cross-file "$PREFIX/wasix.txt" $OPTS -Db_staticpic=false --prefix="$PREFIX/wasix")
  else
    (cd "$PREFIX/src" && PATH="$PREFIX/venv/bin:$PATH" meson setup "$BUILD" $OPTS --prefix="$PREFIX")
  fi
fi
WASIXCC_WASM_EXCEPTIONS=yes PATH="$PREFIX/venv/bin:$PATH" ninja -C "$BUILD" install > /dev/null
case "$TARGET" in
  wasm|wasix) echo "ThorVG ($TARGET) installed in $PREFIX/$TARGET";;
  *) echo "ThorVG ($TARGET) installed in $PREFIX";;
esac
