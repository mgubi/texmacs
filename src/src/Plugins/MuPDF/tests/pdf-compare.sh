#!/bin/sh
#
# Check the PDF the MuPDF renderer writes.
#
#   src/Plugins/MuPDF/tests/pdf-compare.sh [document.tm ...]
#
# run from the top of the source tree. With no argument it takes the
# documents listed in DOCS below. For each one it exports the document
# twice -- once through the MuPDF renderer (TEXMACS_PDF_MUPDF=1) and once
# through whatever the build does otherwise -- and checks four things:
#
#   gs        Ghostscript reads the file without an error and without
#             substituting a font (an invalid font program is the mistake
#             which is easiest to make and hardest to see: the page still
#             looks like a page, in the wrong typeface)
#   text      the text extracts, and no character comes out as U+FFFD
#   render    the page Ghostscript draws and the page MuPDF draws agree;
#             they are two independent readers of the same file, so a
#             disagreement is a fault in the file
#   size      reported, not judged: the two routes do not embed the same
#             kind of font and the numbers are not comparable
#
# Needs gs, mutool and python3 with PIL.

set -u
BIN=TeXmacs/bin/texmacs.bin
OUT=${OUT:-/tmp/pdf-compare}
WAIT=${WAIT:-60}          # seconds to let a conversion run
AGREE=${AGREE:-88}        # percent of pixels within 32 levels

DOCS_DEFAULT="TeXmacs/doc/main/man-manual.en.tm
TeXmacs/doc/main/automated/tag-help.en.tm
TeXmacs/doc/main/start/man-conventions.en.tm"

[ -x "$BIN" ] || { echo "run me from the top of the source tree"; exit 2; }
export TEXMACS_PATH="$PWD/TeXmacs"
mkdir -p "$OUT"

convert () { # <doc> <out.pdf> <mupdf?>
  if [ "$3" = yes ]; then TEXMACS_PDF_MUPDF=1; export TEXMACS_PDF_MUPDF
  else unset TEXMACS_PDF_MUPDF; fi
  rm -f "$2"
  "$BIN" -c "$1" "$2" -q > "$2.log" 2>&1 &
  p=$!
  i=0
  while [ $i -lt "$WAIT" ]; do
    [ -s "$2" ] && sleep 2 && break
    sleep 1; i=$((i+1))
  done
  kill -9 $p 2>/dev/null
  wait $p 2>/dev/null
  [ -s "$2" ]
}

fail=0
for doc in ${*:-$DOCS_DEFAULT}; do
  name=$(basename "$doc" .tm)
  echo "=== $name"
  mu="$OUT/$name-mupdf.pdf"
  ref="$OUT/$name-ref.pdf"
  convert "$doc" "$mu" yes || { echo "  FAIL  nothing was written"; fail=1; continue; }
  convert "$doc" "$ref" no  || echo "  note  the other route wrote nothing"

  # gs: no error, no substituted font
  gserr=$(gs -dNOPAUSE -dBATCH -sDEVICE=nullpage -o /dev/null "$mu" 2>&1 |
          grep -i "error\|does not conform\|or substitute" | head -3)
  if [ -n "$gserr" ]; then
    echo "  FAIL  Ghostscript: $gserr"; fail=1
  else
    echo "  ok    Ghostscript reads it, no font substituted"
  fi

  # the text must come out, and come out whole
  txt=$(mutool draw -F txt -o - "$mu" 2>/dev/null)
  if [ -z "$txt" ]; then
    echo "  FAIL  no text could be extracted"; fail=1
  elif printf '%s' "$txt" | grep -q "$(printf '\357\277\275')"; then
    n=$(printf '%s' "$txt" | grep -c "$(printf '\357\277\275')")
    echo "  FAIL  $n lines of the extracted text have a replacement character"
    fail=1
  else
    echo "  ok    the text extracts ($(printf '%s' "$txt" | wc -w | tr -d ' ') words)"
  fi

  # the two readers must agree on what the file says
  pages=$(mutool info "$mu" 2>/dev/null | sed -n 's/^Pages: //p')
  [ -n "$pages" ] || pages=1
  rm -f "$OUT"/cmp-*.png
  gs -dNOPAUSE -dBATCH -sDEVICE=png16m -r72 -o "$OUT/cmp-gs%d.png" "$mu" >/dev/null 2>&1
  bad=""
  p=1
  while [ "$p" -le "$pages" ]; do
    mutool draw -r 72 -o "$OUT/cmp-mu$p.png" "$mu" "$p" >/dev/null 2>&1
    v=$(python3 - "$OUT/cmp-gs$p.png" "$OUT/cmp-mu$p.png" <<'PY'
import sys
try:
    from PIL import Image, ImageChops
    a= Image.open (sys.argv[1]).convert ("L")
    b= Image.open (sys.argv[2]).convert ("L").resize (a.size)
    h= ImageChops.difference (a, b).histogram ()
    print ("%.1f" % (100.0 * sum (h[:32]) / sum (h)))
except Exception as e:
    print ("0.0")
PY
)
    ok=$(python3 -c "print(1 if float('$v') >= $AGREE else 0)")
    [ "$ok" = 1 ] || bad="$bad $p($v%)"
    p=$((p+1))
  done
  if [ -n "$bad" ]; then
    echo "  FAIL  Ghostscript and MuPDF disagree on page$bad"; fail=1
  else
    echo "  ok    the two readers agree on all $pages page(s)"
  fi

  a=$(wc -c < "$mu" | tr -d ' ')
  b=$([ -s "$ref" ] && wc -c < "$ref" | tr -d ' ' || echo "-")
  echo "  size  $a bytes (the other route: $b)"
done

echo
[ "$fail" = 0 ] && echo "all checks passed" || echo "THERE WERE FAILURES"
exit $fail
