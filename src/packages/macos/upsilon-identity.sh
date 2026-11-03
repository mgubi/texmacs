#!/bin/bash
#
# Gives an application of TeXmacs Upsilon a name, an icon and an identity of
# its own, so that it can be installed beside the stock TeXmacs, and its three
# interfaces beside each other: macOS keeps the preferences of an
# application (NSUserDefaults) and chooses the one which opens a document or
# a tmfs: link by the bundle identifier, which was org.texmacs.TeXmacs as in
# the stock TeXmacs, and the applications were all TeXmacs.app. The document
# types are claimed as an alternative (LSHandlerRank Alternate): a .tm file
# still opens with the stock TeXmacs when it is installed, and with Upsilon
# otherwise or when it is chosen (Open With).
#
# Usage: packages/macos/upsilon-identity.sh APP cocoa|vue|qt
#   prints the name of the application ("TeXmacs Upsilon Vue"...), which
#   the folder of the application should have (NAME.app); run it before
#   the application is signed.

set -e
[ $# -eq 2 ] || { echo "usage: $0 APP cocoa|vue|qt" >&2; exit 1; }
app="$1" gui="$2"
case "$gui" in
  cocoa) name="TeXmacs Upsilon";     id=org.texmacs.upsilon ;;
  vue)   name="TeXmacs Upsilon Vue"; id=org.texmacs.upsilon.vue ;;
  qt)    name="TeXmacs Upsilon Qt";  id=org.texmacs.upsilon.qt ;;
  *) echo "$0: cocoa, vue or qt, not $gui" >&2; exit 1 ;;
esac
plist="$app/Contents/Info.plist"
[ -f "$plist" ] || { echo "$0: no $plist" >&2; exit 1; }

plutil -replace CFBundleName -string "$name" "$plist"
plutil -replace CFBundleDisplayName -string "$name" "$plist"
plutil -replace CFBundleIdentifier -string "$id" "$plist"
if plutil -extract CFBundleURLTypes.0 xml1 -o /dev/null "$plist" 2> /dev/null; then
  plutil -replace CFBundleURLTypes.0.CFBundleURLName -string "$id.url" "$plist"
fi
# (raw gives the number of the elements of an array)
n=$(plutil -extract CFBundleDocumentTypes raw "$plist" 2> /dev/null || echo 0)
for ((i= 0; i < n; i++)); do
  plutil -replace CFBundleDocumentTypes.$i.LSHandlerRank -string Alternate "$plist"
done
# the flat icon of TeXmacs Upsilon (TeXmacs-Upsilon.icns, made from
# TeXmacs/misc/images/texmacs-vue.svg with iconutil), instead of the chip of
# the stock TeXmacs in Assets.car, which holds only that icon
res="$app/Contents/Resources"
cp "$(dirname "$0")/TeXmacs-Upsilon.icns" "$res/AppIcon.icns"
rm -f "$res/Assets.car"
plutil -replace CFBundleIconFile -string AppIcon "$plist"
plutil -remove CFBundleIconName "$plist" > /dev/null 2>&1 || true
plutil -lint "$plist" > /dev/null
echo "$name"
