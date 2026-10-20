#!/bin/bash
#
# Gives the application of TeXmacs Fixes (wip_fixes, Qt) a name and an
# identity of its own, so that it can be installed beside the stock TeXmacs
# (and TeXmacs Upsilon): macOS keeps the preferences of an application
# (NSUserDefaults) and chooses the one which opens a document or a tmfs: link
# by the bundle identifier, which is org.texmacs.TeXmacs in the stock
# TeXmacs. The document types are claimed as an alternative (LSHandlerRank
# Alternate): a .tm file still opens with the stock TeXmacs when it is
# installed, and with TeXmacs Fixes otherwise or when it is chosen (Open
# With). The home (~/.TeXmacs) and the icon are those of the stock TeXmacs.
# Made from packages/macos/upsilon-identity.sh of TeXmacs Upsilon.
#
# Usage: .github/fixes/macos-identity.sh APP
#   run it before the application is signed; the folder of the application
#   should then be named "TeXmacs Fixes.app".

set -e
[ $# -eq 1 ] || { echo "usage: $0 APP" >&2; exit 1; }
app="$1"
name="TeXmacs Fixes"
id=org.texmacs.fixes
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
plutil -lint "$plist" > /dev/null
