#!/bin/sh
# Makes an AppImage of a built TeXmacs (the folder TeXmacs of the build):
#
#   packages/appimage/build-appimage.sh qt|vue OUTPUT.AppImage [LIBDIR]
#
# run from the top of the source tree, after make. The AppDir is the layout
# AppRun expects: usr/bin (texmacs.bin, tm_gs and the programs of the
# plugins), usr/share/TeXmacs, usr/lib; linuxdeploy puts in usr/lib the
# libraries texmacs.bin needs, but those of the system which an AppImage
# must not bring (glibc, libGL and the drivers, X11, Wayland: its
# excludelist), and with "qt" Qt and its plugins (linuxdeploy-plugin-qt,
# QMAKE in the environment). LIBDIR is where libraries which are not those of
# the system are found (SDL3 and SDL3_ttf of misc/vue/build-deps.sh for Vue).
# GS_BIN (the gs made by packages/build-gs.sh, with its licenses in
# GS_LICENSES) goes into usr/bin, which AppRun puts first in the PATH: the
# Ghostscript of the AppImage is the one TeXmacs and the programs it starts
# find, whatever the system has (#662). It is a program with everything
# built in: no library, no folder of resources, nothing in LD_LIBRARY_PATH.
# appimagetool then makes the AppImage, with the static runtime of type 2,
# which needs no libfuse2 on the system.
#
# An AppImage runs on the systems with a glibc as recent as the one of the
# system it is made on or more: it is made on Ubuntu 22.04 (glibc 2.35) by
# .github/workflows/upsilon-linux-windows.yml. The tools are AppImages
# themselves, run without FUSE (APPIMAGE_EXTRACT_AND_RUN), as in a container.
set -e
GUI=${1:?usage: build-appimage.sh qt|vue OUTPUT.AppImage [LIBDIR]}
OUT=${2:?usage: build-appimage.sh qt|vue OUTPUT.AppImage [LIBDIR]}
LIBDIR=${3:-}
case "$GUI" in qt|vue) ;; *) echo "build-appimage.sh: qt or vue, not $GUI" >&2; exit 1;; esac
[ -x TeXmacs/bin/texmacs.bin ] || { echo "build-appimage.sh: no TeXmacs/bin/texmacs.bin (run make first)" >&2; exit 1; }
case "$OUT" in /*) ;; *) OUT="$PWD/$OUT";; esac

WORK=${APPIMAGE_WORK:-$PWD/appimage-work}
APPDIR="$WORK/AppDir"
TOOLS="$WORK/tools"
mkdir -p "$TOOLS"
rm -rf "$APPDIR"
export APPIMAGE_EXTRACT_AND_RUN=1

fetch () { # fetch NAME URL: a tool, once
  [ -x "$TOOLS/$1" ] || { curl -fsSL -o "$TOOLS/$1" "$2"; chmod +x "$TOOLS/$1"; }
}
fetch linuxdeploy https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
fetch appimagetool https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-x86_64.AppImage
[ "$GUI" = qt ] &&
  fetch linuxdeploy-plugin-qt https://github.com/linuxdeploy/linuxdeploy-plugin-qt/releases/download/continuous/linuxdeploy-plugin-qt-x86_64.AppImage

# the layout of AppRun
mkdir -p "$APPDIR/usr/bin" "$APPDIR/usr/lib" "$APPDIR/usr/share"
cp packages/appimage/AppRun "$APPDIR/AppRun"
chmod 755 "$APPDIR/AppRun"
cp TeXmacs/bin/texmacs.bin TeXmacs/bin/tm_gs "$APPDIR/usr/bin/"
if [ -n "${GS_BIN:-}" ]; then
  cp "$GS_BIN" "$APPDIR/usr/bin/gs"
  chmod 755 "$APPDIR/usr/bin/gs"
else
  echo "build-appimage.sh: no GS_BIN, the AppImage has no Ghostscript" >&2
fi
for f in TeXmacs/plugins/*/bin/*; do [ -f "$f" ] && cp "$f" "$APPDIR/usr/bin/"; done
# the command of the desktop entry (below), through AppRun
printf '#!/bin/sh\nexec "$(dirname "$(readlink -f "$0")")/../../AppRun" "$@"\n' > "$APPDIR/usr/bin/texmacs-upsilon"
chmod 755 "$APPDIR"/usr/bin/*
# the data, without the programs (in usr/bin) and the libraries (usr/lib)
mkdir -p "$APPDIR/usr/share/TeXmacs"
(cd TeXmacs && tar cf - --exclude=./bin --exclude=./lib .) |
  (cd "$APPDIR/usr/share/TeXmacs" && tar xf -)
mkdir -p "$APPDIR/usr/share/TeXmacs/bin"
cp TeXmacs/bin/texmacs TeXmacs/bin/fig2ps "$APPDIR/usr/share/TeXmacs/bin/" 2>/dev/null || :

# the entry of the menus and the icon, with names of their own: an AppImage
# put in the menus (AppImageLauncher, Gear Lever...) must not replace the
# entry, the icon or the command of a TeXmacs installed beside it; .tm and
# .tex files only (not every text file)
DESKTOP="$WORK/org.texmacs.upsilon.desktop"
sed -e 's/^Name=.*/Name=TeXmacs Upsilon/' \
    -e 's/^Icon=.*/Icon=texmacs-upsilon/' \
    -e 's/^Exec=texmacs /Exec=texmacs-upsilon /' \
    -e 's|^MimeType=.*|MimeType=text/x-texmacs;text/x-tex;|' \
    packages/appimage/org.texmacs.texmacs.desktop > "$DESKTOP"
ICON="$WORK/texmacs-upsilon.png"
# (the flat icon of TeXmacs Upsilon, for Qt and Vue: not the chip of the
# stock TeXmacs)
cp TeXmacs/misc/images/texmacs-vue-256.png "$ICON"

# the libraries
[ -n "$LIBDIR" ] && export LD_LIBRARY_PATH="$LIBDIR${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
PLUGIN=
if [ "$GUI" = qt ]; then
  export PATH="$TOOLS:$PATH"            # (linuxdeploy finds its plugins in the PATH)
  export EXTRA_QT_MODULES="svg"         # the icons of the menus (Qt Svg)
  export EXTRA_PLATFORM_PLUGINS="libqwayland-generic.so"
  PLUGIN="--plugin qt"
fi
"$TOOLS/linuxdeploy" --appdir "$APPDIR" \
  --executable "$APPDIR/usr/bin/texmacs.bin" \
  --desktop-file "$DESKTOP" --icon-file "$ICON" $PLUGIN

# the C++ runtime is the one of the system, which is as recent or more: the
# drivers of OpenGL (Mesa, its llvmpipe) need its newer symbols, and an
# older libstdc++ loaded first left TeXmacs without OpenGL
rm -f "$APPDIR"/usr/lib/libstdc++.so* "$APPDIR"/usr/lib/libgcc_s.so*

# the license of Ghostscript
if [ -n "${GS_LICENSES:-}" ]; then
  mkdir -p "$APPDIR/usr/share/TeXmacs/misc/licenses/ghostscript"
  cp -R "$GS_LICENSES"/. "$APPDIR/usr/share/TeXmacs/misc/licenses/ghostscript/"
fi

# the licenses of the libraries of the system which were brought
packages/collect-licenses.sh "$APPDIR/usr/lib" "$APPDIR/usr/share/TeXmacs/misc/licenses/system"

# what was brought, for the log of the build
echo "== libraries of the AppImage:"
ls "$APPDIR/usr/lib"

ARCH=x86_64 "$TOOLS/appimagetool" --no-appstream "$APPDIR" "$OUT"
ls -la "$OUT"
