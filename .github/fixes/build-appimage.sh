#!/bin/sh
# Makes the AppImage of TeXmacs Fixes (wip_fixes with Qt and Guile 1.8) of a
# built TeXmacs (the folder TeXmacs of the build):
#
#   ../.github/fixes/build-appimage.sh OUTPUT.AppImage
#
# run from src/, after make (adapted from packages/appimage/build-appimage.sh
# of TeXmacs Upsilon). TeXmacs Fixes has the home of the stock TeXmacs
# (~/.TeXmacs), but menu entries, an icon and a command of its own. The AppDir is the layout
# AppRun expects: usr/bin (texmacs.bin, tm_gs and the programs of the
# plugins), usr/share/TeXmacs, usr/lib; linuxdeploy puts in usr/lib the
# libraries texmacs.bin needs, but those of the system which an AppImage
# must not bring (glibc, libGL and the drivers, X11, Wayland: its
# excludelist), and with "qt" Qt and its plugins (linuxdeploy-plugin-qt,
# QMAKE in the environment).
# appimagetool then makes the AppImage, with the static runtime of type 2,
# which needs no libfuse2 on the system.
#
# An AppImage runs on the systems with a glibc as recent as the one of the
# system it is made on or more: it is made on Ubuntu 22.04 (glibc 2.35) by
# .github/workflows/fixes.yml. The tools are AppImages
# themselves, run without FUSE (APPIMAGE_EXTRACT_AND_RUN), as in a container.
set -e
OUT=${1:?usage: build-appimage.sh OUTPUT.AppImage}
HERE=$(cd "$(dirname "$0")" && pwd)
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
fetch linuxdeploy-plugin-qt https://github.com/linuxdeploy/linuxdeploy-plugin-qt/releases/download/continuous/linuxdeploy-plugin-qt-x86_64.AppImage

# the layout of AppRun
mkdir -p "$APPDIR/usr/bin" "$APPDIR/usr/lib" "$APPDIR/usr/share"
cp "$HERE/AppRun" "$APPDIR/AppRun"
chmod 755 "$APPDIR/AppRun"
cp TeXmacs/bin/texmacs.bin TeXmacs/bin/tm_gs "$APPDIR/usr/bin/"
for f in TeXmacs/plugins/*/bin/*; do [ -f "$f" ] && cp "$f" "$APPDIR/usr/bin/"; done
# the command of the desktop entry (below), through AppRun
printf '#!/bin/sh\nexec "$(dirname "$(readlink -f "$0")")/../../AppRun" "$@"\n' > "$APPDIR/usr/bin/texmacs-fixes"
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
DESKTOP="$WORK/org.texmacs.fixes.desktop"
sed -e 's/^Name=.*/Name=TeXmacs Fixes/' \
    -e 's/^Icon=.*/Icon=texmacs-fixes/' \
    -e 's/^Exec=texmacs /Exec=texmacs-fixes /' \
    -e 's|^MimeType=.*|MimeType=text/x-texmacs;text/x-tex;|' \
    packages/appimage/org.texmacs.texmacs.desktop > "$DESKTOP"
ICON="$WORK/texmacs-fixes.png"
cp TeXmacs/misc/images/texmacs-256.png "$ICON"

# the libraries
export PATH="$TOOLS:$PATH"            # (linuxdeploy finds its plugins in the PATH)
export EXTRA_QT_MODULES="svg"         # the icons of the menus (Qt Svg)
export EXTRA_PLATFORM_PLUGINS="libqwayland-generic.so"
"$TOOLS/linuxdeploy" --appdir "$APPDIR" \
  --executable "$APPDIR/usr/bin/texmacs.bin" \
  --desktop-file "$DESKTOP" --icon-file "$ICON" --plugin qt

# the C++ runtime is the one of the system, which is as recent or more (the
# drivers of OpenGL need its newer symbols)
rm -f "$APPDIR"/usr/lib/libstdc++.so* "$APPDIR"/usr/lib/libgcc_s.so*

# what was brought, for the log of the build
echo "== libraries of the AppImage:"
ls "$APPDIR/usr/lib"

ARCH=x86_64 "$TOOLS/appimagetool" --no-appstream "$APPDIR" "$OUT"
ls -la "$OUT"
