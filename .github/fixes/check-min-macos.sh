#!/bin/bash
#
# Checks that no program or library of a macOS application needs a newer macOS
# than the LSMinimumSystemVersion of its Info.plist (#733). The Homebrew
# libraries of the Qt packages target the macOS of their bottles (15.0), and
# the workflows declare that minimum with plutil after macdeployqt.
#
# It is not src/packages/macos/check-app.sh (Upsilon only), which also
# checks the signature, the libraries outside the application and the rpaths:
# macdeployqt leaves the Homebrew rpaths (/opt/homebrew/lib) in the
# libraries it copies, which check-app.sh reports as errors (seen on the
# Qt package of upsilon-latest), so it does not suit these applications.
#
# It fails when the application has no Mach-O file, when a Mach-O file has no
# version command (LC_BUILD_VERSION or LC_VERSION_MIN_MACOSX), when find
# fails, or when a file needs more than the minimum. It prints how many
# files it looked at.
#
# Usage: .github/fixes/check-min-macos.sh TeXmacs.app

set -e -o pipefail
app="${1:?usage: $0 TeXmacs.app}"
[ -d "$app/Contents" ] || { echo "error: $app is not an application" >&2; exit 1; }
min=$(plutil -extract LSMinimumSystemVersion raw "$app/Contents/Info.plist" 2> /dev/null) || min=""
if [ -z "$min" ]; then
  echo "error: $app/Contents/Info.plist has no LSMinimumSystemVersion" >&2
  exit 1
fi
# a version as a number (15.0.1 -> 150001)
num () { local IFS=.; set -- $1 0 0; echo $(($1 * 10000 + $2 * 100 + $3)); }
echo "LSMinimumSystemVersion: $min"
list=$(mktemp)
trap 'rm -f "$list"' EXIT
if ! find "$app/Contents" -type f > "$list"; then
  echo "error: find could not read all of $app/Contents" >&2
  exit 1
fi
status=0
files=0
checks=0
while IFS= read -r f; do
  file -b "$f" | grep -q Mach-O || continue
  files=$((files + 1))
  archs=$(lipo -archs "$f" 2> /dev/null) || archs=""
  if [ -z "$archs" ]; then
    echo "error: ${f#$app/}: lipo cannot read its architectures (damaged Mach-O file?)" >&2
    status=1
    continue
  fi
  for a in $archs; do
    checks=$((checks + 1))
    v=$(otool -arch "$a" -l "$f" |
        awk '/LC_BUILD_VERSION|LC_VERSION_MIN_MACOSX/ {c=1}
             c && ($1 == "minos" || $1 == "version") {print $2; exit}')
    if [ -z "$v" ]; then
      echo "error: ${f#$app/} ($a) has no LC_BUILD_VERSION or LC_VERSION_MIN_MACOSX" >&2
      status=1
    elif [ $(num "$v") -gt $(num "$min") ]; then
      echo "error: ${f#$app/} ($a) needs macOS $v, more than LSMinimumSystemVersion $min" >&2
      status=1
    fi
  done
done < "$list"
echo "checked $files Mach-O files ($checks architectures)"
if [ $files -eq 0 ]; then
  echo "error: no Mach-O file in $app" >&2
  status=1
fi
if [ $status -ne 0 ]; then
  echo "If macOS $min is no longer the true minimum and the new one is accepted," >&2
  echo "raise the 15.0 of the plutil -replace LSMinimumSystemVersion line of the" >&2
  echo "workflow (Homebrew's libraries now need more); else find the library." >&2
fi
exit $status
