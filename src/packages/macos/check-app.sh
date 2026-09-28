#!/bin/bash
#
# Checks an application made by build-ns-app.sh or merge-universal.sh: its
# signature, its Info.plist, that it only uses the libraries of macOS and
# its own ones, and that its code (the programs of the plugins too) runs
# on the version of macOS given by LSMinimumSystemVersion (for each
# architecture).
#
# Usage: packages/macos/check-app.sh TeXmacs.app

set -e

app="$1"
[ -d "$app/Contents" ] || { echo "usage: $0 TeXmacs.app" >&2; exit 1; }

codesign --verify --deep --strict "$app"
plist="$app/Contents/Info.plist"
plutil -lint "$plist" > /dev/null
min=$(plutil -extract LSMinimumSystemVersion raw "$plist")

# a version as a number, for the comparisons (12.0.1 -> 120001)
num () { local IFS=.; set -- $1 0 0; echo $(($1 * 10000 + $2 * 100 + $3)); }

status=0
while IFS= read -r f; do
  file -b "$f" | grep -q "Mach-O" || continue
  for a in $(lipo -archs "$f"); do
    libs=$(otool -arch "$a" -L "$f" | tail -n +2 | awk '{print $1}' |
           grep -v "^/System/\|^/usr/lib/\|^@executable_path/\|^@loader_path/\|^@rpath/" || true)
    if [ -n "$libs" ]; then
      echo "error: ${f#$app/} ($a) uses libraries outside the application:" $libs >&2
      status=1
    fi
    # minos (LC_BUILD_VERSION) or version (LC_VERSION_MIN_MACOSX)
    v=$(otool -arch "$a" -l "$f" |
        awk '/LC_BUILD_VERSION|LC_VERSION_MIN_MACOSX/ {c=1}
             c && ($1 == "minos" || $1 == "version") {print $2; exit}')
    if [ -n "$v" ] && [ $(num "$v") -gt $(num "$min") ]; then
      echo "error: ${f#$app/} ($a) needs macOS $v (LSMinimumSystemVersion: $min)" >&2
      status=1
    fi
    echo "${f#$app/}: $a, macOS $v"
  done
done < <(find "$app/Contents" -type f \( -perm -u+x -o -name "*.dylib" -o -name "*.so" \))
exit $status
