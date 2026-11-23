#!/bin/sh
# Export the sample documents as tagged PDF (PDF/UA-1) and check them with
# veraPDF (docs/pdf-tagging.md).
#
# usage: tests/documents/tagged.sh [sample...]
#
# Needs a build whose PDF is written by MuPDF (Vue, or TEXMACS_PDF_MUPDF=1),
# and verapdf (Homebrew: brew install verapdf). The PDF and the report of
# each sample are in tests/build/tagged; mutool run
# tests/documents/pdf-structure.js shows the structure of one.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/tagged"
bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
command -v verapdf > /dev/null || { echo "verapdf is needed" >&2; exit 1; }

export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TM_TEST_HOME:-$out/home}"
export TEXMACS_PDF_TAGGED=1
export QT_QPA_PLATFORM=offscreen
mkdir -p "$out" "$TEXMACS_HOME_PATH"

if [ $# -eq 0 ]; then
  set -- $(cd "$here/samples" && ls *.tm | sed 's/\.tm$//')
fi

failed=0
for s in "$@"; do
  pdf="$out/$s.pdf"
  rm -f "$pdf"
  "$bin" -headless -x "(load \"$here/export.scm\")" \
    -x "(begin (test-export \"$here/samples/$s.tm\" \"$pdf\") (quit-TeXmacs))" \
    -q > "$out/$s.log" 2>&1 &
  pid=$!
  t=0
  while kill -0 $pid 2> /dev/null; do
    rss=$(ps -o rss= -p $pid | tr -d ' ')
    if [ "${rss:-0}" -gt 2000000 ] || [ $t -gt 120 ]; then
      kill -9 $pid; echo "$s: killed (memory or time)"; break
    fi
    sleep 1; t=$((t+1))
  done
  wait $pid 2> /dev/null
  if [ ! -s "$pdf" ]; then
    echo "$s: FAILED, no PDF (see $out/$s.log)"; failed=1; continue
  fi
  verapdf -f ua1 --format text "$pdf" > "$out/$s.txt" 2>&1
  if grep -q "^PASS" "$out/$s.txt"; then echo "$s: ok"
  else echo "$s: FAILED, not PDF/UA-1 (see $out/$s.txt)"; failed=1; fi
done
exit $failed
