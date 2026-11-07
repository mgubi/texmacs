#!/bin/bash
#
# Attaches a disk image read-only at a mount point, for the checks of the
# CI. On the runners of GitHub a disk image just made by hdiutil create
# cannot be attached for minutes at times ("hdiutil: attach failed -
# Resource temporarily unavailable", 3 times in November 2026, every time
# at the first attach after the image was made): the new file is held,
# most likely by XProtect scanning it. So, between the attempts: what holds
# the image (hdiutil info, lsof) is shown once, XProtect is stopped (only
# when sudo needs no password: the runners), and from the third attempt on
# a fresh copy of the image is attached instead (the copy is a new file
# which nobody holds yet).
#
# Usage: packages/macos/attach-dmg.sh IMAGE.dmg MOUNTPOINT
#   ATTACH_TRIES   number of attempts (default 8)

set -u

[ $# = 2 ] || { echo "usage: $0 IMAGE.dmg MOUNTPOINT" >&2; exit 2; }
dmg="$1" mnt="$2"
tries="${ATTACH_TRIES:-8}"
[ -f "$dmg" ] || { echo "error: no disk image $dmg" >&2; exit 1; }
mkdir -p "$mnt"

image="$dmg"
copy=""
for i in $(seq 1 "$tries"); do
  if hdiutil attach -nobrowse -readonly -mountpoint "$mnt" "$image"; then
    echo "== $image attached at $mnt (attempt $i)"
    exit 0
  fi
  [ "$i" = "$tries" ] && break
  if [ "$i" = 1 ]; then
    echo "== what holds the disk image"
    hdiutil info || true
    lsof "$dmg" 2> /dev/null || true
    pgrep -l XProtect || true
  fi
  if sudo -n true 2> /dev/null; then
    sudo -n pkill -9 XProtect 2> /dev/null || true
  fi
  if [ "$i" -ge 2 ]; then
    [ -n "$copy" ] && rm -f "$copy"
    copy="${RUNNER_TEMP:-${TMPDIR:-/tmp}}/attach-copy-$i-$(basename "$dmg")"
    cp "$dmg" "$copy" && image="$copy" || image="$dmg"
  fi
  delay=$((i < 5 ? i * 10 : 50))
  echo "== hdiutil attach failed, again in $delay seconds"
  sleep "$delay"
done
echo "error: $dmg cannot be attached" >&2
hdiutil info || true
exit 1
