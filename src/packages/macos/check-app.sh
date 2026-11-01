#!/bin/bash
#
# Checks an application made by build-ns-app.sh or merge-universal.sh: its
# signature, its Info.plist, that it only uses the libraries of macOS and
# its own ones, and that its code (the programs of the plugins too) runs
# on the version of macOS given by LSMinimumSystemVersion (for each
# architecture).
#
# With --sdk-newer-than-min, the main program must also be linked against an
# SDK newer than that version: an SDK equal to it means the linker was told
# the wrong SDK (#662), and macOS then gives the program the behaviour of that
# old version. The check depends only on the application, not on this machine.
# It is for applications made for an older macOS (build-ns-app.sh --deps,
# merge-universal.sh). Without --deps the minimal version is the version of
# the machine, which may well be the SDK or newer, and the rule does not apply.
#
# When the application has a Ghostscript (the program gs of its TeXmacs
# folder, #662), its signature is verified and it must run alone (an empty
# environment, from another folder) and say its version, with each of its
# architectures this machine can run.
#
# Usage: packages/macos/check-app.sh [--sdk-newer-than-min] TeXmacs.app

set -e

sdk_rule=no
[ "${1:-}" = --sdk-newer-than-min ] && { sdk_rule=yes; shift; }
app="${1:-}"
[ -d "$app/Contents" ] || { echo "usage: $0 [--sdk-newer-than-min] TeXmacs.app" >&2; exit 1; }

codesign --verify --deep --strict "$app"
plist="$app/Contents/Info.plist"
plutil -lint "$plist" > /dev/null
# a version: 13, 13.0 or 13.0.1
is_version () { [[ "$1" =~ ^[0-9]+(\.[0-9]+){0,2}$ ]]; }
min=$(plutil -extract LSMinimumSystemVersion raw "$plist" 2> /dev/null) || min=""
if ! is_version "$min"; then
  echo "error: $plist: LSMinimumSystemVersion is missing or not a version (\"$min\")" >&2
  exit 1
fi
if [ $sdk_rule = yes ]; then
  echo "SDK rule: the main program must be linked against an SDK newer than macOS $min"
else
  echo "SDK rule: skipped (no --sdk-newer-than-min)"
fi

# a version as a number, for the comparisons (12.0.1 -> 120001)
num () { local IFS=.; set -- $1 0 0; echo $(($1 * 10000 + $2 * 100 + $3)); }

exe="$app/Contents/MacOS/$(plutil -extract CFBundleExecutable raw "$plist")"

# the file for a reference @executable_path/..., @loader_path/... (of the
# file $2) or an absolute one; nothing for another @rpath
resolve () { # ref file
  case "$1" in
    @executable_path/*) echo "$(dirname "$exe")/${1#@executable_path/}";;
    @loader_path/*) echo "$(dirname "$2")/${1#@loader_path/}";;
    @*) ;;
    *) echo "$1";;
  esac
}

# inside the application (and existing)?
inside () { # path
  [ -e "$1" ] || return 1
  local p=$(cd "$(dirname "$1")" 2> /dev/null && pwd -P)/$(basename "$1")
  case "$p" in "$app_real"/*) return 0;; *) return 1;; esac
}
app_real=$(cd "$app" && pwd -P)
main_seen=no

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
    # the paths where @rpath is searched: its own and those of the program
    rpaths=$( (otool -arch "$a" -l "$f"; [ "$f" != "$exe" ] &&
               otool -arch "$a" -l "$exe" 2> /dev/null) |
             awk '/cmd LC_RPATH/ {c=1} c && $1 == "path" {print $2; c=0}' | sort -u)
    for r in $rpaths; do
      case "$r" in
        @*) ;;
        *) echo "error: ${f#$app/} ($a) searches libraries in $r" >&2
           status=1;;
      esac
    done
    # the references to the libraries of the application must be found
    for l in $(otool -arch "$a" -L "$f" | tail -n +2 | awk '{print $1}' |
               grep "^@" || true); do
      found=no
      case "$l" in
        @rpath/*)
          for r in $rpaths; do
            d=$(resolve "$r" "$f")
            [ -n "$d" ] && inside "$d/${l#@rpath/}" && { found=yes; break; }
          done;;
        *) inside "$(resolve "$l" "$f")" && found=yes;;
      esac
      if [ $found = no ]; then
        echo "error: ${f#$app/} ($a) uses $l, which is not in the application" >&2
        status=1
      fi
    done
    # minos (LC_BUILD_VERSION) or version (LC_VERSION_MIN_MACOSX)
    v=$(otool -arch "$a" -l "$f" |
        awk '/LC_BUILD_VERSION|LC_VERSION_MIN_MACOSX/ {c=1}
             c && ($1 == "minos" || $1 == "version") {print $2; exit}')
    if [ -n "$v" ] && [ $(num "$v") -gt $(num "$min") ]; then
      echo "error: ${f#$app/} ($a) needs macOS $v (LSMinimumSystemVersion: $min)" >&2
      status=1
    fi
    # the SDK the program is linked against (#662: -Wl,-macosx_version_min
    # made it the minimal version, 13.0, so NSImage took EPS files as in
    # macOS 13); otool says n/a when the program does not record it
    s=$(otool -arch "$a" -l "$f" |
        awk '/LC_BUILD_VERSION|LC_VERSION_MIN_MACOSX/ {c=1}
             c && $1 == "sdk" {print $2; exit}')
    if [ "$f" = "$exe" ]; then
      main_seen=yes
      if ! is_version "$s"; then
        echo "error: ${f#$app/} ($a) does not say its SDK (\"$s\")" >&2
        status=1
      elif [ $sdk_rule = yes ] && [ $(num "$s") -le $(num "$min") ]; then
        echo "error: ${f#$app/} ($a) is linked against the SDK of macOS $s, not newer than LSMinimumSystemVersion ($min)" >&2
        status=1
      fi
    fi
    echo "${f#$app/}: $a, macOS $v, SDK $s"
  done
done < <(find "$app/Contents" -type f \( -perm -u+x -o -name "*.dylib" -o -name "*.so" \))
gs="$app/Contents/Resources/share/TeXmacs/bin/gs"
if [ -f "$gs" ]; then
  if codesign --verify --strict "$gs" 2> /dev/null; then
    echo "bin/gs: signature valid"
  else
    echo "error: the signature of ${gs#$app/} is not valid" >&2
    status=1
  fi
  for a in $(lipo -archs "$gs"); do
    if arch -$a /usr/bin/true 2> /dev/null; then
      v=$(cd / && env -i PATH=/usr/bin:/bin arch -$a "$gs" --version 2>&1) || v="(fails)"
      if [[ "$v" =~ ^[0-9]+\.[0-9]+(\.[0-9]+)?$ ]]; then
        echo "${gs#$app/} ($a) runs: Ghostscript $v"
      else
        echo "error: ${gs#$app/} ($a) does not run: $v" >&2
        status=1
      fi
    else
      echo "${gs#$app/} ($a): not run, this machine cannot run $a"
    fi
  done
fi
if [ $main_seen = no ]; then
  echo "error: the main program ${exe#$app/} is not an executable Mach-O file" >&2
  status=1
fi
exit $status
