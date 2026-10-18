#!/bin/sh
# The libraries of the WASIX build, built with wasixcc in the build
# directory (default build-wasix), from the top of the source tree:
#
#   sh misc/wasix/build-deps.sh [build directory]
#
# - MuPDF 1.28.5, the slim build of the browser build (misc/wasm/build-mupdf.sh,
#   its patches), in mupdf-1.28.5-source/build/wasix/slim
# - SDL3 3.4.2, with its dummy video driver only (the headless TeXmacs links
#   SDL but opens no display), installed in sdl3/
# - the sources of Hunspell (misc/wasm/get-hunspell.sh)
#
# wasixcc: cargo install wasixcc; wasixccenv install-executables ~/.wasixcc/bin;
# wasixccenv download-all (see docs/wasix/README.md)
set -eu
TM=$(cd "$(dirname "$0")/../.." && pwd)
B=${1:-build-wasix}
mkdir -p "$B"; B=$(cd "$B" && pwd)
WASIXCC=${WASIXCC:-$HOME/.wasixcc/bin}
export PATH="$WASIXCC:$PATH"
# the exceptions of WebAssembly: C++ exceptions, and setjmp/longjmp (MuPDF)
export WASIXCC_WASM_EXCEPTIONS=yes
export WASIX_SYSROOT=$("$WASIXCC/wasixccenv" print-sysroot)

V=1.28.5
cd "$B"
if [ ! -f mupdf-$V-source/build/wasix/slim/libmupdf.a ]; then
  [ -d mupdf-$V-source ] || {
    [ -f mupdf-$V-source.tar.gz ] ||
      curl -fLO https://mupdf.com/downloads/archive/mupdf-$V-source.tar.gz
    tar xzf mupdf-$V-source.tar.gz
  }
  cd mupdf-$V-source
  for p in "$TM"/misc/wasm/mupdf-*.patch; do
    s=.applied-$(basename "$p")
    [ -f "$s" ] || { patch -p1 < "$p" && touch "$s"; }
  done
  F="-DTOFU -DTOFU_CJK -DTOFU_SIL -DTOFU_EMOJI -DTOFU_HISTORIC -DTOFU_SYMBOL \
   -DFZ_ENABLE_XPS=0 -DFZ_ENABLE_CBZ=0 -DFZ_ENABLE_HTML=0 -DFZ_ENABLE_FB2=0 \
   -DFZ_ENABLE_MOBI=0 -DFZ_ENABLE_EPUB=0 -DFZ_ENABLE_OFFICE=0 -DFZ_ENABLE_TXT=0 \
   -DFZ_ENABLE_MD=0 -DFZ_ENABLE_HTML_ENGINE=0 -DFZ_ENABLE_OCR_OUTPUT=0 \
   -DFZ_ENABLE_DOCX_OUTPUT=0 -DFZ_ENABLE_ODT_OUTPUT=0 -DFZ_ENABLE_BROTLI=0 \
   -DFZ_ENABLE_JS=0 -DFZ_ENABLE_BARCODE=0 -DFZ_ENABLE_HYPHEN=0"
  make -j8 OS=wasix build=release OUT=build/wasix/slim \
    CC=wasixcc CXX=wasix++ AR=wasixar RANLIB=wasixranlib \
    HAVE_X11=no HAVE_GLUT=no HAVE_CURL=no XCFLAGS="$F" brotli=no tesseract=no libs
  cd "$B"
fi

SDLV=3.4.2
if [ ! -f sdl3/lib/libSDL3.a ]; then
  [ -d SDL-release-$SDLV ] || {
    curl -fL -o SDL-release-$SDLV.tar.gz \
      https://github.com/libsdl-org/SDL/archive/refs/tags/release-$SDLV.tar.gz
    tar xzf SDL-release-$SDLV.tar.gz
  }
  # SDL knows no WASI: it is built as for Linux, every device off; the
  # sysroot (without PIC) has no dlfcn.h nor priorities of threads, which
  # misc/wasix/include gives (sdl-compat.h, before each source); dlopen is
  # stubbed, never found (SDL's dynamic API needs it and knows no WASI)
  I="$TM/misc/wasix/include"
  # (the compat header for the build only: in the checks of CMake it made
  # clock_gettime conflict)
  WASIXCC_RUN_WASM_OPT=no cmake -S SDL-release-$SDLV -B sdl3-build -G "Unix Makefiles" \
    -DWASIX_SYSTEM_NAME=Linux -DHAVE_DLOPEN_IN_LIBC= -DHAVE_DLOPEN_IN_LIBDL= \
    -DCMAKE_TOOLCHAIN_FILE="$TM/misc/wasix/wasix-toolchain.cmake" \
    -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX="$B/sdl3" \
    -DSDL_SHARED=OFF -DSDL_STATIC=ON -DSDL_TESTS=OFF -DSDL_EXAMPLES=OFF \
    -DSDL_UNIX_CONSOLE_BUILD=ON -DSDL_X11=OFF -DSDL_WAYLAND=OFF \
    -DSDL_OPENGL=OFF -DSDL_OPENGLES=OFF -DSDL_VULKAN=OFF -DSDL_GPU=OFF \
    -DSDL_AUDIO=OFF -DSDL_JOYSTICK=OFF -DSDL_HAPTIC=OFF -DSDL_HIDAPI=OFF \
    -DSDL_CAMERA=OFF -DSDL_SENSOR=OFF -DSDL_POWER=OFF -DSDL_DIALOG=OFF \
    -DSDL_LIBUDEV=OFF -DSDL_DBUS=OFF -DSDL_IBUS=OFF -DSDL_TRAY=OFF \
    -DSDL_KMSDRM=OFF -DSDL_OFFSCREEN=ON -DSDL_DUMMYVIDEO=ON \
    -DSDL_PIPEWIRE=OFF -DSDL_PULSEAUDIO=OFF -DSDL_ALSA=OFF -DSDL_JACK=OFF \
    -DSDL_SNDIO=OFF -DSDL_OSS=OFF -DSDL_LIBURING=OFF
  WASIXCC_RUN_WASM_OPT=no WASIXCC_COMPILER_FLAGS="-I$I:-include:$I/sdl-compat.h" \
    make -C sdl3-build -j8
  make -C sdl3-build install
fi

sh "$TM/misc/wasm/get-hunspell.sh" "$B"

# webgpu.h (webgpu-native/webgpu-headers, BSD 3), the version of wgpu-native
# 29.0.1.1, which ThorVG's WebGPU engine is written for: the API of the
# WebGPU of WASIX (misc/wasix/webgpu)
WH=673658bc2bd70ec39fc55ebe6bb0173cf6d0a603
if [ ! -f webgpu-headers/webgpu/webgpu.h ]; then
  mkdir -p webgpu-headers/webgpu
  curl -fsSL -o webgpu-headers/webgpu/webgpu.h \
    https://raw.githubusercontent.com/webgpu-native/webgpu-headers/$WH/webgpu.h
  curl -fsSL -o webgpu-headers/LICENSE \
    https://raw.githubusercontent.com/webgpu-native/webgpu-headers/$WH/LICENSE
fi

# ThorVG with its WebGPU engine, for the GPU renderer (misc/thorvg)
[ -f thorvg/wasix/lib/libthorvg-1.a ] ||
  WEBGPU_HEADERS="$B/webgpu-headers" sh "$TM/misc/thorvg/build-thorvg.sh" "$B/thorvg" wasix
