# Source this file to use Emscripten for the TeXmacs wasm build:
#
#   . misc/wasm/emenv.sh [build directory]
#
# It points Emscripten at a Python >= 3.10 (EMSDK_PYTHON) and at a config
# file of its own (EM_CONFIG), written in the build directory (default:
# build-wasm): the Homebrew post-install could not write the global one when
# python3 was the system 3.9, and the build should not depend on it anyway.

WASM_BUILD="${1:-build-wasm}"
mkdir -p "$WASM_BUILD"

if [ -z "$EMSDK_PYTHON" ]; then
  for p in /opt/homebrew/opt/python@3.13/bin/python3.13 \
           /opt/homebrew/opt/python@3.14/bin/python3.14 \
           /opt/homebrew/bin/python3 /usr/local/bin/python3 python3; do
    if command -v "$p" > /dev/null 2>&1 &&
       "$p" -c 'import sys; sys.exit(sys.version_info < (3, 10))' 2> /dev/null; then
      EMSDK_PYTHON=$(command -v "$p"); break
    fi
  done
fi
export EMSDK_PYTHON

EM_ROOT=$(dirname "$(readlink -f "$(command -v emcc)")")
case "$EM_ROOT" in */bin) EM_ROOT="$EM_ROOT/../libexec" ;; esac
EM_CONFIG="$(cd "$WASM_BUILD" && pwd)/.emscripten"
cat > "$EM_CONFIG" <<CFG
LLVM_ROOT = '$EM_ROOT/llvm/bin'
BINARYEN_ROOT = '$EM_ROOT/binaryen'
NODE_JS = '$(command -v node)'
CACHE = '$(cd "$WASM_BUILD" && pwd)/emcache'
CFG
export EM_CONFIG
echo "emscripten: $(emcc --version 2>&1 | head -1) (python $EMSDK_PYTHON)"
