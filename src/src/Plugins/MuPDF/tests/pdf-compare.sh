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
AGREE=${AGREE:-90}        # percent of pixels within 32 levels, after a blur

DOCS_DEFAULT="TeXmacs/doc/main/man-manual.en.tm
TeXmacs/doc/main/automated/tag-help.en.tm
TeXmacs/doc/main/start/man-conventions.en.tm
src/Plugins/MuPDF/tests/figures.tm
src/Plugins/MuPDF/tests/pdf-figures.tm
src/Plugins/MuPDF/tests/structure.tm
src/Plugins/MuPDF/tests/pattern.tm
src/Plugins/MuPDF/tests/pattern-photo.tm
src/Plugins/MuPDF/tests/ligatures.tm
src/Plugins/MuPDF/tests/landscape.tm"

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

  # Ligatures: each ligature glyph must say which letters it stands for
  # (/ActualText), or a reader which takes the text from the glyph names
  # gives a search for "first" nothing to find. Neither reader can judge
  # this on its own -- MuPDF decomposes ligatures whatever the file says,
  # and Ghostscript ignores ActualText -- but the second is what makes the
  # check: what Ghostscript extracts counts the ligature glyphs drawn, and
  # there must be a span for each of them in the content streams.
  mutool clean -d "$mu" "$OUT/plain.pdf" >/dev/null 2>&1
  lig=$(gs -q -dNOPAUSE -dBATCH -sDEVICE=txtwrite -o - "$mu" 2>/dev/null |
        python3 -c "import sys; t=sys.stdin.buffer.read().decode('utf-8','replace'); print(sum(1 for c in t if 0xfb00 <= ord(c) <= 0xfb06))")
  span=$(python3 -c "import sys; print(open(sys.argv[1],'rb').read().count(b'/ActualText'))" "$OUT/plain.pdf" 2>/dev/null || echo 0)
  if [ "${lig:-0}" -gt "${span:-0}" ]; then
    echo "  FAIL  $lig ligature glyphs, only $span say which letters they are"
    fail=1
  else
    echo "  ok    ligatures: $lig drawn, $span with their letters"
  fi

  # PDF figures the right way up. The two readers cannot see a figure
  # upside down, since they draw the same file: the figures of
  # pdf-figures.tm have text in them which says where it must be -- UP
  # above the caption in the upright one, to its right in the one turned
  # by /Rotate 90
  if [ "$name" = pdf-figures ]; then
    way=$(mutool draw -F stext -o - "$mu" 2>/dev/null | python3 -c '
import sys, re
up, cap= [], []
for m in re.finditer (r"<line bbox=\"([^\"]*)\"[^>]*>(.*?)</line>", sys.stdin.read (), re.S):
    s= "".join (re.findall (r"c=\"([^\"]*)\"", m.group (2)))
    b= [float (v) for v in m.group (1).split ()]
    if s == "UP": up.append (b)
    elif s.startswith ("vector figure"): cap.append (b)
up.sort (); cap.sort ()
if len (up) != 2 or len (cap) != 2: print ("the figure text is not there")
elif not up[0][1] < cap[0][1]: print ("the upright figure is upside down")
elif not up[1][0] > cap[1][0]: print ("the rotated figure is turned the wrong way")
else: print ("ok")')
    if [ "$way" = ok ]; then
      echo "  ok    the figures are the right way up"
    else
      echo "  FAIL  $way"; fail=1
    fi
    # the half transparent figure is translucent as a whole (a group):
    # where its squares overlap, pale blue (128 128 255); purple (128 64
    # 191, measured), where the blue square went over a half transparent
    # red one, means it is not, in either reader
    for r in mu gs; do
      if [ $r = mu ]; then mutool draw -r 72 -o "$OUT/grp-$r.png" "$mu" 1 >/dev/null 2>&1
      else gs -q -dNOPAUSE -dBATCH -sDEVICE=png16m -r72 -o "$OUT/grp-$r.png" "$mu" >/dev/null 2>&1; fi
      purple=$(python3 -c "
from PIL import Image
a= Image.open ('$OUT/grp-$r.png').convert ('RGB')
print (sum (1 for p in a.getdata () if p[0] > 90 and p[2] > 150 and p[1] < 100))" 2>/dev/null || echo 1)
      if [ "$purple" = 0 ]; then
        echo "  ok    the translucent figure is a group ($r)"
      else
        echo "  FAIL  the translucent figure is not a group ($r: $purple purple pixels)"; fail=1
      fi
    done
  fi

  # the structure of structure.tm: its outline (seven headings, three
  # levels deep), the places its references point at, its links and its
  # title -- what the writer puts together when the document is closed
  if [ "$name" = structure ]; then
    mutool run "$(dirname "$0")/structure.js" "$mu" > "$OUT/structure.txt" 2>&1
    got=$(python3 "$(dirname "$0")/structure-check.py" "$OUT/structure.txt")
    if [ "$got" = ok ]; then
      echo "  ok    outline, destinations, links and title are there"
    else
      echo "  FAIL  $got"; fail=1
    fi
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
    from PIL import Image, ImageChops, ImageFilter
    # blurred by a pixel and a half: two readers anti-alias and resample
    # differently, which is not a fault in the file, and on a page of
    # hard edges it moves the raw numbers more than a real fault does.
    # Measured: documents 97.5% and up, a photo pattern 95%, a pattern
    # Ghostscript draws wrongly 42%
    a= Image.open (sys.argv[1]).convert ("L").filter (ImageFilter.GaussianBlur (1.5))
    b= Image.open (sys.argv[2]).convert ("L").resize (a.size).filter (ImageFilter.GaussianBlur (1.5))
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
