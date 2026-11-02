#!/bin/sh
#
# Puts the git commit into the Info.plist of an application (#753):
# CFBundleVersion becomes the hash, "ebf1221c79", and
# CFBundleShortVersionString stays the number 2.1.5, so that the displays
# which show "short (bundle)", as the About panel, read "2.1.5 (ebf1221c79)". The time of the commit goes into
# CFBundleGetInfoString. Without git (a source tarball) the Info.plist is left as it is.
#
# Usage: packages/macos/set-bundle-revision.sh INFO.PLIST [SRCDIR]
#   run before the application is signed.

plist="$1"
[ -f "$plist" ] || { echo "$0: no $plist" >&2; exit 1; }
here=$(cd "$(dirname "$0")" && pwd)
rev=$("$here/../../misc/scripts/git-revision.sh" "${2:-$here/../..}")
[ -n "$rev" ] || exit 0
hash=${rev%% *}
time=${rev#* }
version=$(plutil -extract CFBundleShortVersionString raw "$plist") || exit 1
plutil -replace CFBundleVersion -string "$hash" "$plist" || exit 1
plutil -replace CFBundleGetInfoString -string "TeXmacs $version, revision $hash, $time" "$plist" || exit 1
plutil -lint "$plist" > /dev/null
