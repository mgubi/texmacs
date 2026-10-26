#!/bin/bash
# The licenses of the libraries of the system which a bundle brings:
#
#   packages/collect-licenses.sh <folder of the libraries> <folder of the licenses>
#
# for each library of the folder (*.dll, *.so*), the license files of the
# package of the system it comes from: under MSYS2 (the Windows bundles,
# WINDOWS_BUNDLE of Makefile.in) those of /mingw64/share/licenses/<package>,
# under Debian or Ubuntu (the AppImages, packages/appimage/build-appimage.sh)
# /usr/share/doc/<package>/copyright. The libraries which no package of the
# system has (those built by the CI, whose licenses are in
# TeXmacs/misc/licenses) are skipped. It never fails the build.

libs="$1"
out="$2"
[ -d "$libs" ] || exit 0
mkdir -p "$out" || exit 0
seen=" "
for f in "$libs"/*.dll "$libs"/*.so*; do
  [ -f "$f" ] || continue
  name=$(basename "$f")
  pkg=
  if command -v pacman > /dev/null 2>&1; then
    [ -f "/mingw64/bin/$name" ] && pkg=$(pacman -Qqo "/mingw64/bin/$name" 2> /dev/null | head -1)
  elif command -v dpkg > /dev/null 2>&1; then
    pkg=$(dpkg -S "*/$name" 2> /dev/null | head -1 | cut -d: -f1)
  fi
  [ -n "$pkg" ] || continue
  case "$seen" in *" $pkg "*) continue ;; esac
  seen="$seen$pkg "
  if [ -d "/mingw64/share/licenses/$pkg" ]; then
    cp -R "/mingw64/share/licenses/$pkg" "$out/" 2> /dev/null
  elif [ -d "/mingw64/share/licenses/${pkg#mingw-w64-*-}" ]; then
    # (the folder is named without the prefix mingw-w64-x86_64-)
    cp -R "/mingw64/share/licenses/${pkg#mingw-w64-*-}" "$out/$pkg" 2> /dev/null
  elif [ -f "/usr/share/doc/$pkg/copyright" ]; then
    cp "/usr/share/doc/$pkg/copyright" "$out/$pkg.copyright" 2> /dev/null
  fi
done
echo "licenses of the libraries of the system: $(ls "$out" | wc -l) packages in $out"
exit 0
