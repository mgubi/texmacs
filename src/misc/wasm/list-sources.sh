#!/bin/sh
# Write misc/wasm/sources.txt, the sources of the browser build: those of a
# desktop build of the Vue GUI with S7 (configure --with-gui=vue
# --with-scheme=s7 ...; make), read from its objects, without the Objective-C
# of the macOS plugin. Run from the top of the source tree after such a build.
cd src || exit 1
for o in Objects/*.o; do
  b=$(basename "$o" .o)
  find . -path ./Objects -prune -o \( -name "$b.cpp" -o -name "$b.c" \) -print
done | sed 's|^\./||' | grep -v '^Plugins/MacOS/' | sort > ../misc/wasm/sources.txt
wc -l < ../misc/wasm/sources.txt
