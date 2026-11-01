#!/bin/bash
# The Ghostscript and the Aspell of a package of TeXmacs Upsilon (#662):
#
#   packages/bundled-tools/check.sh [-g GS] [-a] -r TEXMACS_PATH -v VERSION -- TEXMACS...
#
#   -r DIR      the TeXmacs folder of the package (TEXMACS_PATH): bin/gs
#               (bin/gs.exe) is the program of the package, unless -g
#   -g GS       the program of the package, when it is not DIR/bin/gs (the
#               AppImage: usr/bin/gs)
#   -v VERSION  the version of Ghostscript the package has (10.07.0)
#   -a          also the spell checker (the Windows packages): dictionaries
#               for English, French and German, found and used
#   -e          with -a: Greek, which the dictionaries of the package do not
#               have, goes to an external Hunspell or Aspell (a fake one is
#               built); the builds which run programs through pipes (Qt, not
#               Vue on Windows: "pipes not implemented")
#   TEXMACS...  the command which runs TeXmacs, with its options (it gets
#               -x commands added): the program of the package, under
#               xvfb-run, with -headless...
#
# Without a Ghostscript of the system: a gs which fails (exit status 99) is
# first in the PATH (on Windows the PATH has the folders of Windows only), so
# that a conversion made with it, instead of the one of the package, fails.
#  1. the gs of the package runs from where it is, alone (an empty
#     environment), says its version, extracts the text of an EPS file,
#     converts it to PDF with the 35 base fonts, which are built into it;
#  2. TeXmacs (check.scm) says which gs it runs (the one of the package),
#     and typesets an EPS image into a PDF, which is written by that gs
#     (its Producer says the version);
#  3. with -a: spell checking.
# (TM_CHECK_QPA: the Qt platform: empty for the default, else its name; the
# default is offscreen, but on Windows.) Everything is written in a scratch folder, which is removed; TeXmacs
# runs with TEXMACS_HOME_PATH there too.

here=$(cd "$(dirname "$0")" && pwd)
# (MSYS2: no conversion of -dPDFSETTINGS=/prepress into a Windows path)
export MSYS2_ARG_CONV_EXCL='*'
root="" gs="" version="" aspell=no external=no
while getopts "r:g:v:ae" o; do
  case $o in
    r) root="$OPTARG";; g) gs="$OPTARG";; v) version="$OPTARG";; a) aspell=yes;; e) external=yes;;
    *) exit 2;;
  esac
done
shift $((OPTIND - 1))
[ -n "$root" ] && [ -n "$version" ] && [ $# -gt 0 ] || {
  echo "usage: $0 [-g GS] [-a] -r TEXMACS_PATH -v VERSION -- TEXMACS..." >&2; exit 2; }
# absolute paths: the checks run in a scratch folder
root=$(cd "$root" && pwd)
[ -n "$gs" ] && gs=$(cd "$(dirname "$gs")" && pwd)/$(basename "$gs")
case "$1" in */*) set -- "$(cd "$(dirname "$1")" && pwd)/$(basename "$1")" "${@:2}";; esac
windows=no
case "$(uname -s)" in MINGW*|MSYS*) windows=yes;; esac
if [ -z "$gs" ]; then
  if [ $windows = yes ]; then gs="$root/bin/gs.exe"; else gs="$root/bin/gs"; fi
fi

work=$(mktemp -d "${TMPDIR:-/tmp}/bundled-tools.XXXXXX")
trap 'rm -rf "$work"' EXIT
status=0
fail () { echo "FAIL: $*"; status=1; }
ok () { echo "ok: $*"; }
# a path as the Windows programs print it (C:/...), else as it is
native () { if [ $windows = yes ]; then cygpath -m "$1"; else echo "$1"; fi; }

n_home=$(native "$work/home"); n_root=$(native "$root"); n_work=$(native "$work")
cp "$here/test.eps" "$here/check.scm" "$work/"
mkdir "$work/shim" "$work/home"
if [ $windows = yes ]; then
  clean_path='/c/Windows/System32:/c/Windows'
  run_path="$clean_path"
else
  printf '#!/bin/sh\necho used > "%s/shim-used"\necho "the gs of the system was used" >&2\nexit 99\n' "$work" > "$work/shim/gs"
  chmod +x "$work/shim/gs"
  clean_path="$work/shim:/usr/bin:/bin"
  run_path="$work/shim:$PATH"
fi

echo "== the licenses in $root/misc/licenses"
[ -f "$root/misc/licenses/ghostscript/LICENSE" ] && [ -f "$root/misc/licenses/ghostscript/COPYING" ] &&
  ok "the license of Ghostscript (ghostscript/LICENSE, COPYING)" ||
  { fail "no license of Ghostscript in $root/misc/licenses/ghostscript"; ls -R "$root/misc/licenses/ghostscript" 2>&1 | head -5; }
[ -f "$root/misc/licenses/README.md" ] && [ -f "$root/misc/licenses/AGPL-3.0.txt" ] &&
  ok "README.md and AGPL-3.0.txt" || fail "no README.md or AGPL-3.0.txt in $root/misc/licenses"
if [ $aspell = yes ]; then
  [ -f "$root/misc/licenses/aspell/Aspell-COPYING" ] && [ -f "$root/misc/licenses/aspell/dict-en-Copyright" ] &&
    ok "the licenses of Aspell and its dictionaries" || fail "no license of Aspell in $root/misc/licenses/aspell"
fi

echo "== the program: $gs"
[ -f "$gs" ] || { fail "$gs does not exist"; exit 1; }
[ -x "$gs" ] || fail "$gs is not executable"
v=$(cd "$work" && env -i MSYS2_ARG_CONV_EXCL="*" TMPDIR="$n_work" PATH="$clean_path" "$gs" --version | tr -d '\r')
[ "$v" = "$version" ] && ok "gs --version: $v" || fail "gs --version says \"$v\", not $version"
txt=$(cd "$work" && env -i MSYS2_ARG_CONV_EXCL="*" TMPDIR="$n_work" PATH="$clean_path" "$gs" -q -dNOPAUSE -dBATCH -dSAFER \
        -sDEVICE=txtwrite -sOutputFile=- test.eps | tr -d '\r')
case "$txt" in *"Hello Gs"*) ok "the text of the EPS file: $(echo $txt | head -c 40)";;
  *) fail "no text from the EPS file: $txt";; esac
(cd "$work" && env -i MSYS2_ARG_CONV_EXCL="*" TMPDIR="$n_work" PATH="$clean_path" "$gs" -dNOPAUSE -dBATCH -dSAFER \
   -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress -dEmbedAllFonts=true \
   -dCompressFonts=false -dCompressPages=false -dCompatibilityLevel=1.4 -dEPSCrop \
   -sOutputFile=direct.pdf test.eps > gs-pdf.log 2>&1) ||
  { fail "gs does not convert the EPS file"; tail -15 "$work/gs-pdf.log"; }
if [ -s "$work/direct.pdf" ] && head -c 4 "$work/direct.pdf" | grep -q '%PDF'; then
  ok "the EPS file converted to PDF ($(wc -c < "$work/direct.pdf" | tr -d ' ') bytes)"
  # the fonts of the EPS file (Helvetica, Times) are the URW fonts built in
  # (embedded as subsets: ABCDEF+Helvetica)
  for f in Helvetica Times-Roman Symbol; do
    LC_ALL=C grep -aq "+$f" "$work/direct.pdf" && ok "font $f embedded" || fail "font $f is not in the PDF"
  done
else
  fail "no PDF from the EPS file"
fi

# runs a command, killed after N seconds (macOS has no timeout)
run_timeout () {
  local t=$1; shift
  # (the PATH of the check only for the program: this function needs sleep)
  ( PATH="$run_path"; exec "$@" ) &
  local pid=$! n=0
  while kill -0 $pid 2> /dev/null; do
    n=$((n + 1))
    [ $n -gt $((t * 2)) ] && { echo "TIMEOUT"; kill -9 $pid; break; }
    sleep 0.5
  done
  wait $pid 2> /dev/null
}

# with absolute paths, as TeXmacs gives them (on Windows with backslashes,
# and the options it adds: a title, -c code...)
for form in native windows; do
  [ $form = windows ] && { [ $windows = yes ] || continue; }
  if [ $form = windows ]; then in=$(cygpath -w "$work/test.eps"); outp=$(cygpath -w "$work/abs-w.pdf")
  else in="$n_work/test.eps"; outp="$n_work/abs-n.pdf"; fi
  (cd "$work" && env -i MSYS2_ARG_CONV_EXCL="*" TMPDIR="$n_work" PATH="$clean_path" "$gs" -dNOPAUSE -dBATCH -dSAFER \
     -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 "-sOutputFile=$outp" "$in" > gs-abs.log 2>&1) &&
    ok "an EPS file with an absolute path ($form) converted to PDF" ||
    { fail "gs does not convert the EPS file with an absolute path ($form: $in)"; tail -10 "$work/gs-abs.log"; }
done

# one run of TeXmacs (the PATH of the check, the home in the scratch folder;
# TMPDIR is never set, as a real user has none: TEMP is the folder $3 if given,
# $4 is put first in the PATH); the log is $out
tm_cmd=("$@")   # the command which runs TeXmacs
run_tm () { # log what temp extra_path
  local what="$2" temp="$3" extra="$4" args="" w
  for w in $what; do args="$args \"$w\""; done
  out="$1"
  rm -f "$work/eps-doc.pdf" "$work/eps-doc.tm"
  (cd "$work" && export TEXMACS_HOME_PATH="$n_home" TEXMACS_PATH="$n_root" &&
   unset TMPDIR &&
   if [ -n "$temp" ]; then export TEMP="$(native "$temp")" TMP="$(native "$temp")"; fi &&
   if [ -n "$extra" ]; then run_path="$extra:$run_path"; fi &&
   # (Qt without a window, unless TM_CHECK_QPA says otherwise, even empty:
   # the Qt of an AppImage has no offscreen plugin, nor Windows runs need it)
   if [ -n "${TM_CHECK_QPA+x}" ]; then
     if [ -n "$TM_CHECK_QPA" ]; then export QT_QPA_PLATFORM="$TM_CHECK_QPA"; fi
   elif [ $windows = no ]; then export QT_QPA_PLATFORM=offscreen; fi &&
   run_timeout 300 "${tm_cmd[@]}" -x '(load "check.scm")' \
   -x "(bundled-tools-check \"$n_work\" $args)" > "$out" 2>&1)
  [ $windows = yes ] && taskkill //F //IM texmacs.exe > /dev/null 2>&1
  return 0
}
val () { grep "^CHECK $1 " "$out" | head -1 | tr -d '\r' | cut -d' ' -f3-; }

echo "== TeXmacs: $*"
what=ghostscript
[ $aspell = yes ] && what="ghostscript aspell"
run_tm "$work/texmacs.log" "$what" "" ""
grep "^CHECK" "$out" | tr -d '\r'
grep -q "^CHECK done yes" "$out" || { fail "TeXmacs did not run the check"; tail -30 "$out"; }
[ -z "$(val error)" ] || fail "an error in the check: $(val error)"
used=$(val gs-binary | tr '\\' /); appdir=$(val appdir)
want="$n_root"; [ -n "$appdir" ] && want="$appdir"
case "$used" in
  "$want"*) ok "TeXmacs runs the gs of the package: $used";;
  *) fail "TeXmacs runs $used, not the gs of the package ($want)";;
esac
if [ "$(val pdf)" = yes ]; then ok "an EPS image written to PDF"
else
  fail "no PDF with an EPS image: the gs of the package was not used (TMPDIR: $(val tmpdir))"
  grep -v "^CHECK" "$out" | tr -d '\r' | tail -25
fi
# the gs which TeXmacs runs (its debug output, gs_utilities.cpp): the one of
# the package
# (in an AppImage, the name gs alone: AppRun puts its usr/bin first in the PATH,
# before the failing gs of this script)
if tr '\\' / < "$out" | tr -d '\r' | grep -F "gs executable: " | grep -qF "gs executable: $want" ||
   { [ -n "$appdir" ] && grep -qx "TeXmacs\] debug-convert, gs executable: gs" "$out"; }; then
  ok "TeXmacs runs the gs of the package for its conversions: $(tr '\\' / < "$out" | tr -d '\r' | grep -F 'gs executable: ' | head -1 | cut -d: -f2- | cut -c1-120)"
else
  fail "TeXmacs did not run the gs of the package ($want) for a conversion"
  grep -F "gs executable" "$out" | head -3
fi
[ -e "$work/shim-used" ] && fail "the gs of the system was run"
# (the PDF of Qt is written by Qt: its Producer is not Ghostscript)
if [ -f "$work/eps-doc.pdf" ] && LC_ALL=C grep -aq "Ghostscript $version" "$work/eps-doc.pdf"; then
  ok "the PDF was written by Ghostscript $version"
else
  echo "note: the PDF does not say Ghostscript $version (written by Qt?)"
fi
if [ $aspell = yes ]; then
  for l in english french german; do
    [ "$(val spell-$l-start)" = ok ] && ok "dictionary for $l" || fail "no dictionary for $l: $(val spell-$l-start)"
    [ "$(val spell-$l-good)" = yes ] && ok "$l: a word is known" || fail "$l: a word is not known"
    [ "$(val spell-$l-bad)" = no ] && ok "$l: a misspelling is found" || fail "$l: a misspelling is not found"
  done
  # a language outside the dictionaries of the package: no external checker on
  # this machine, a clear error (the external route is tried, not an English
  # dictionary); one there: it works
  ext=$(val ext-greek-start)
  if command -v hunspell > /dev/null 2>&1 || command -v aspell > /dev/null 2>&1; then
    echo "note: this machine has an external spell checker (Greek: $ext)"
  else
    case "$ext" in Error:*) ok "Greek (no dictionary in the package, no external checker): $ext";;
      *) fail "Greek should give an error here, not \"$ext\"";; esac
  fi
  # with an external checker (a fake hunspell: the protocol of ispell -a, one
  # line "*" for a word, a suggestion for those with zzz), built here
  cc=${CC:-gcc}
  if [ $external = no ]; then
    echo "note: no external spell checker in this build (no pipes to programs): the fallback is not tried"
  elif command -v $cc > /dev/null 2>&1; then
    mkdir -p "$work/fakebin"
    cat > "$work/fake-ispell.c" << 'EOT'
#include <stdio.h>
#include <string.h>
int main (void) {
  char line[4096];
  printf ("@(#) International Ispell Version 3.1.20 (but really a fake)\n");
  fflush (stdout);
  while (fgets (line, sizeof (line), stdin)) {
    if (line[0] == '^') {
      if (strstr (line, "zzz")) printf ("& zzzz 1 0: yyyy\n\n");
      else printf ("*\n\n");
      fflush (stdout);
    }
  }
  return 0;
}
EOT
    fe=hunspell; [ $windows = yes ] && fe=hunspell.exe
    # (native paths: MSYS2 does not convert the arguments here)
    if $cc -o "$n_work/fakebin/$fe" "$n_work/fake-ispell.c" 2> "$work/cc.log"; then
      printf '^abc\n^zzzz\n' | "$work/fakebin/$fe" | tr -d '\r' | head -c 200 | od -c | head -4
      run_tm "$work/texmacs-ext.log" external "" "$work/fakebin"
      grep "^CHECK ext" "$out" | tr -d '\r'
      [ "$(val ext-greek-start)" = ok ] && [ "$(val ext-greek-good)" = yes ] && [ "$(val ext-greek-bad)" = no ] &&
        ok "Greek goes to the external spell checker (the dictionaries of the package do not have it)" ||
        { fail "Greek does not reach the external spell checker: $(val ext-greek-start)"; grep -v "^CHECK" "$out" | tr -d '\r' | tail -25; }
    else
      fail "no fake external spell checker (the compiler failed)"; head -5 "$work/cc.log"
    fi
  else
    echo "note: no C compiler: the fallback to an external spell checker is not tried"
  fi
fi

# Windows: gs.exe makes its temporary files in $TMPDIR (set by TeXmacs from TEMP
# when it is unset, as it is for a real user), with the ANSI functions of Windows
if [ $windows = yes ]; then
  echo "== TEMP, TMPDIR unset"
  run_tm "$work/texmacs-t0.log" ghostscript "" ""
  [ "$(val pdf)" = yes ] && ok "an EPS image written to PDF, TMPDIR unset (TeXmacs sets it: $(val tmpdir))" ||
    { fail "no PDF with TMPDIR unset (TMPDIR in TeXmacs: $(val tmpdir))"; tail -10 "$out"; }
  # the user name is in the path of the home of TeXmacs, where gs reads and
  # writes its files too (the ANSI functions of Windows): with letters of the
  # code page (ü) and outside it (Cyrillic, on a Western system)
  home0="$n_home"
  for h in "home ü" "home-ж"; do
    mkdir -p "$work/$h"; n_home=$(native "$work/$h")
    run_tm "$work/texmacs-h.log" ghostscript "" ""
    if [ "$(val pdf)" = yes ]; then ok "an EPS image written to PDF with TEXMACS_HOME_PATH=.../$h"
    else echo "note: no PDF with TEXMACS_HOME_PATH=.../$h (gs and the ANSI functions of Windows)"; fi
  done
  n_home="$home0"
  for t in "tmp dir ü" "tmp-ж"; do
    mkdir -p "$work/$t"
    run_tm "$work/texmacs-t1.log" ghostscript "$work/$t" ""
    [ "$(val pdf)" = yes ] && ok "an EPS image written to PDF with TEMP=.../$t (TMPDIR in TeXmacs: $(val tmpdir))" ||
      { fail "no PDF with TEMP=.../$t (TMPDIR in TeXmacs: $(val tmpdir))"; grep -v "^CHECK" "$out" | tr -d '\r' | tail -8; }
  done
fi
[ $status = 0 ] && echo "== the bundled tools are fine" || echo "== FAILED"
exit $status
