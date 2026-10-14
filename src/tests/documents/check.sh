#!/bin/sh
# Typeset the sample documents and compare them with stored references.
#
# usage: tests/documents/check.sh [-u] [-p] [-r dpi] [sample...]
#
#   sample   names of documents in tests/documents/samples, without .tm
#            (default: all of them)
#   -u       update the references instead of comparing with them
#   -p       also compare the pages pixel by pixel with references kept in
#            tests/build/documents/pixels (made by -u -p on this machine,
#            since anti-aliasing differs between systems and builds)
#   -r dpi   resolution of the pixel comparison (default 100)
#
# Each sample is exported to PDF by TeXmacs without a window, after its
# references and table of contents are brought up to date (export.scm), and
# mutool extracts the number of pages and the text of every page. The text is
# compared with tests/documents/ref/<sample>.txt, which is committed, so a
# change of the line breaks, the page breaks, the numbering or the glyphs
# shows up as a difference. A PDF which mutool reads with a syntax error
# fails as well.
#
# The documents are typeset with TEXMACS_PATH set to the source tree and a
# scratch TEXMACS_HOME_PATH under tests/build/documents, so the user's own
# settings and fonts do not leak in. Requires mutool (mupdf), and
# ImageMagick's compare for -p.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/documents"
pixref="$out/pixels"
update=""
pixels=""
dpi=100
while getopts "upr:" opt; do
  case $opt in
    u) update=1 ;;
    p) pixels=1 ;;
    r) dpi=$OPTARG ;;
    *) exit 2 ;;
  esac
done
shift $((OPTIND - 1))

bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
command -v mutool > /dev/null || { echo "mutool is needed" >&2; exit 1; }

export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="$out/home"
mkdir -p "$out" "$TEXMACS_HOME_PATH" "$here/ref"

if [ $# -eq 0 ]; then
  set -- $(cd "$here/samples" && ls *.tm | sed 's/\.tm$//')
fi

# the pages and the text of a PDF, in the form of the references
extract () {
  pdf=$1
  pages=$(mutool info "$pdf" 2>/dev/null | sed -n 's/^Pages: //p')
  echo "pages: $pages"
  p=1
  while [ "$p" -le "$pages" ]; do
    echo "--- page $p"
    mutool draw -q -F txt -o - "$pdf" "$p" 2>/dev/null | sed 's/[[:space:]]*$//'
    p=$((p + 1))
  done
}

status=0
for name in "$@"; do
  tm="$here/samples/$name.tm"
  [ -f "$tm" ] || { echo "$name: no such sample"; status=1; continue; }
  pdf="$out/$name.pdf"
  rm -f "$pdf"
  "$bin" -x "(load \"$here/export.scm\")" \
         -x "(test-export \"$tm\" \"$pdf\")" -q > "$out/$name.log" 2>&1
  if [ ! -f "$pdf" ]; then
    echo "$name: FAILED, no PDF (see $out/$name.log)"; status=1; continue
  fi
  errors=$(mutool draw -q -F txt -o /dev/null "$pdf" 2>&1 | grep -c 'syntax error')
  if [ "$errors" != "0" ]; then
    echo "$name: FAILED, $errors syntax errors in the PDF"; status=1
  fi
  extract "$pdf" > "$out/$name.txt"
  ref="$here/ref/$name.txt"
  if [ -n "$update" ]; then
    cp "$out/$name.txt" "$ref"
    echo "$name: reference updated ($(head -1 "$ref"))"
  elif [ ! -f "$ref" ]; then
    echo "$name: FAILED, no reference (make one with -u)"; status=1
  elif ! diff -u "$ref" "$out/$name.txt" > "$out/$name.diff"; then
    echo "$name: FAILED, the text differs (see $out/$name.diff)"
    head -20 "$out/$name.diff" | sed 's/^/    /'
    status=1
  else
    echo "$name: ok ($(head -1 "$ref"))"
  fi
  if [ -n "$pixels" ]; then
    mkdir -p "$pixref"
    rm -f "$out/$name"-*.png
    mutool draw -q -r "$dpi" -o "$out/$name-%d.png" "$pdf" 2>/dev/null
    for png in "$out/$name"-*.png; do
      [ -f "$png" ] || continue
      refpng="$pixref/$(basename "$png")"
      if [ -n "$update" ]; then
        cp "$png" "$refpng"
      elif [ -f "$refpng" ]; then
        n=$(compare -metric AE "$refpng" "$png" "${png%.png}-diff.png" 2>&1)
        n=${n%% *}
        if [ "$n" != "0" ]; then
          echo "    $(basename "$png"): $n pixels differ"; status=1
        fi
      fi
    done
  fi
done
exit $status
