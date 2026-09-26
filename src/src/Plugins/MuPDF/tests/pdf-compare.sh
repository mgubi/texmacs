#!/bin/sh
#
# Check the PDF the MuPDF renderer writes.
#
#   src/Plugins/MuPDF/tests/pdf-compare.sh [document.tm ...]
#
# run from the top of the source tree. With no argument it takes the
# documents listed in DOCS below. Each one is exported through the MuPDF
# renderer (TEXMACS_PDF_MUPDF=1), and the file is checked:
#
#   gs        Ghostscript reads the file without an error and without
#             substituting a font (an invalid font program is the mistake
#             which is easiest to make and hardest to see: the page still
#             looks like a page, in the wrong typeface)
#   check     MuPDF finds nothing to repair in its cross reference table
#             and its objects (PDFDocument.check, pdf-check.js)
#   text      the text extracts, and no character comes out as U+FFFD
#   render    the page Ghostscript draws and the page MuPDF draws agree;
#             they are two independent readers of the same file, so a
#             disagreement is a fault in the file
#   size      reported, not judged
#
# and some documents have checks of their own (see below), and the round
# trip of a PDF with its document embedded is run as well
# (embed-roundtrip.sh).
#
# Every TeXmacs it starts has a home of its own, a copy of ~/.TeXmacs made
# for it (copy on write where the file system can): a test must not change
# the preferences, the list of recent files or anything else of the user's,
# and the runs side by side must not share their caches.
#
# Needs gs, mutool (1.27 or later for the check of the structure) and
# python3 with PIL. OUT is the output directory, JOBS how many TeXmacs run
# side by side (4), WAIT how long a conversion is given (60 s), AGREE the
# threshold of the rendering check (90), REF=1 exports every document
# through the other route of the build as well, for its size, EMBED=0
# leaves out the round trip.

set -u
BIN=TeXmacs/bin/texmacs.bin
OUT=${OUT:-/tmp/pdf-compare}
WAIT=${WAIT:-60}          # seconds to let a conversion run
AGREE=${AGREE:-90}        # percent of pixels within 32 levels, after a blur
JOBS=${JOBS:-4}           # TeXmacs side by side
REF=${REF:-0}             # also the other route, for the size
EMBED=${EMBED:-1}         # the round trip of an embedded document
HERE=$(dirname "$0")

DOCS_DEFAULT="TeXmacs/doc/main/man-manual.en.tm
TeXmacs/doc/main/automated/tag-help.en.tm
TeXmacs/doc/main/start/man-conventions.en.tm
src/Plugins/MuPDF/tests/figures.tm
src/Plugins/MuPDF/tests/pdf-figures.tm
src/Plugins/MuPDF/tests/structure.tm
src/Plugins/MuPDF/tests/layer.tm
src/Plugins/MuPDF/tests/pattern.tm
src/Plugins/MuPDF/tests/pattern-photo.tm
src/Plugins/MuPDF/tests/ligatures.tm
src/Plugins/MuPDF/tests/landscape.tm"
DOCS=${*:-$DOCS_DEFAULT}

[ -x "$BIN" ] || { echo "run me from the top of the source tree"; exit 2; }
export TEXMACS_PATH="$PWD/TeXmacs"
mkdir -p "$OUT/homes"

# a home of its own for one TeXmacs: a copy of the user's, copy on write
# (instant on APFS), or a plain copy where that cannot be done
new_home () { # <dir>
  rm -rf "${1:?}"
  if [ -d "$HOME/.TeXmacs" ]; then
    cp -Rc "$HOME/.TeXmacs" "$1" 2>/dev/null || cp -R "$HOME/.TeXmacs" "$1" || {
      echo "cannot make a home for TeXmacs in $1" >&2; exit 2; }
  else mkdir -p "$1"; fi
}

convert () { # <doc> <out.pdf> <mupdf?> <home>
  rm -f "$2"
  if [ "$3" = yes ]; then
    TEXMACS_HOME_PATH="$4" TEXMACS_PDF_MUPDF=1 "$BIN" -c "$1" "$2" -q > "$2.log" 2>&1 &
  else
    TEXMACS_HOME_PATH="$4" "$BIN" -c "$1" "$2" -q > "$2.log" 2>&1 &
  fi
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

# run the jobs given on stdin (one shell command a line), JOBS at a time
batch () {
  n=0
  while IFS= read -r job; do
    sh -c "$job" &
    n=$((n+1))
    if [ $n -ge "$JOBS" ]; then wait; n=0; fi
  done
  wait
}

check_doc () { # <name>: every check of one exported document
  name=$1
  mu="$OUT/$name-mupdf.pdf"
  ref="$OUT/$name-ref.pdf"
  if [ ! -s "$mu" ]; then echo "  FAIL  nothing was written"; return; fi

  # gs: no error, no substituted font
  gserr=$(gs -dNOPAUSE -dBATCH -sDEVICE=nullpage -o /dev/null "$mu" 2>&1 |
          grep -i "error\|does not conform\|or substitute" | head -3)
  if [ -n "$gserr" ]; then
    echo "  FAIL  Ghostscript: $gserr"
  else
    echo "  ok    Ghostscript reads it, no font substituted"
  fi

  # the text must come out, and come out whole
  txt=$(mutool draw -F txt -o - "$mu" 2>/dev/null)
  if [ -z "$txt" ]; then
    echo "  FAIL  no text could be extracted"
  elif printf '%s' "$txt" | grep -q "$(printf '\357\277\275')"; then
    n=$(printf '%s' "$txt" | grep -c "$(printf '\357\277\275')")
    echo "  FAIL  $n lines of the extracted text have a replacement character"
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
  mutool clean -d "$mu" "$OUT/$name-plain.pdf" >/dev/null 2>&1
  lig=$(gs -q -dNOPAUSE -dBATCH -sDEVICE=txtwrite -o - "$mu" 2>/dev/null |
        python3 -c "import sys; t=sys.stdin.buffer.read().decode('utf-8','replace'); print(sum(1 for c in t if 0xfb00 <= ord(c) <= 0xfb06))")
  span=$(python3 -c "import sys; print(open(sys.argv[1],'rb').read().count(b'/ActualText'))" "$OUT/$name-plain.pdf" 2>/dev/null || echo 0)
  if [ "${lig:-0}" -gt "${span:-0}" ]; then
    echo "  FAIL  $lig ligature glyphs, only $span say which letters they are"
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
      echo "  FAIL  $way"
    fi
    # the half transparent figure is translucent as a whole (a group):
    # where its squares overlap, pale blue (128 128 255); purple (128 64
    # 191, measured), where the blue square went over a half transparent
    # red one, means it is not, in either reader
    for r in mu gs; do
      if [ $r = mu ]; then mutool draw -r 72 -o "$OUT/$name-grp-$r.png" "$mu" 1 >/dev/null 2>&1
      else gs -q -dNOPAUSE -dBATCH -sDEVICE=png16m -r72 -o "$OUT/$name-grp-$r.png" "$mu" >/dev/null 2>&1; fi
      purple=$(python3 -c "
from PIL import Image
a= Image.open ('$OUT/$name-grp-$r.png').convert ('RGB')
print (sum (1 for p in a.getdata () if p[0] > 90 and p[2] > 150 and p[1] < 100))" 2>/dev/null || echo 1)
      if [ "$purple" = 0 ]; then
        echo "  ok    the translucent figure is a group ($r)"
      else
        echo "  FAIL  the translucent figure is not a group ($r: $purple purple pixels)"
      fi
    done
  fi

  # the structure of structure.tm: its outline (seven headings, three
  # levels deep), the places its references point at, its links and its
  # title -- what the writer puts together when the document is closed
  if [ "$name" = structure ]; then
    mutool run "$(dirname "$0")/structure.js" "$mu" > "$OUT/structure.txt" 2>&1
    got=$(python3 "$(dirname "$0")/structure-check.py" "$OUT/structure.txt")
    # and, in the file itself, every link names its destination in the
    # bytes of the name tree, and every URI is ASCII (dest-bytes.py)
    mutool clean -d "$mu" "$OUT/structure-plain.pdf" >/dev/null 2>&1
    bytes=$(python3 "$(dirname "$0")/dest-bytes.py" "$OUT/structure-plain.pdf")
    case "$bytes" in ok*) ;; *) got="$got; $bytes" ;; esac
    if [ "$got" = ok ]; then
      echo "  ok    outline, destinations, links and title are there"
    else
      echo "  FAIL  $got"
    fi
  fi

  # layer.tm: a PDF figure with a layer which is off by default, a red
  # square (layer.pdf, made by mklayer.js); the frame around it shows, the
  # square must not, in either reader -- which it did before the layers of a
  # figure went into the catalogue (merge_layers)
  if [ "$name" = layer ]; then
    for r in mu gs; do
      if [ $r = mu ]; then mutool draw -r 72 -o "$OUT/$name-lay-$r.png" "$mu" 1 >/dev/null 2>&1
      else gs -q -dNOPAUSE -dBATCH -sDEVICE=png16m -r72 -o "$OUT/$name-lay-$r.png" "$mu" >/dev/null 2>&1; fi
      counts=$(python3 -c "
from PIL import Image
a= Image.open ('$OUT/$name-lay-$r.png').convert ('RGB')
print (sum (1 for p in a.getdata () if p[0] > 200 and p[1] < 80 and p[2] < 80),
       sum (1 for p in a.getdata () if p[0] < 60 and p[1] < 60 and p[2] < 60))" 2>/dev/null || echo "1 0")
      red=${counts% *}; dark=${counts#* }
      if [ "$red" = 0 ] && [ "$dark" -gt 500 ]; then
        echo "  ok    the hidden layer of the figure stays hidden ($r)"
      else
        echo "  FAIL  the hidden layer of the figure shows ($r: $red red pixels, $dark dark)"
      fi
    done
  fi

  # the two readers must agree on what the file says
  pages=$(mutool info "$mu" 2>/dev/null | sed -n 's/^Pages: //p')
  [ -n "$pages" ] || pages=1
  rm -f "$OUT/$name"-cmp-*.png
  gs -dNOPAUSE -dBATCH -sDEVICE=png16m -r72 -o "$OUT/$name-cmp-gs%d.png" "$mu" >/dev/null 2>&1
  bad=""
  p=1
  while [ "$p" -le "$pages" ]; do
    mutool draw -r 72 -o "$OUT/$name-cmp-mu$p.png" "$mu" "$p" >/dev/null 2>&1
    v=$(python3 - "$OUT/$name-cmp-gs$p.png" "$OUT/$name-cmp-mu$p.png" <<'PY'
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
    echo "  FAIL  Ghostscript and MuPDF disagree on page$bad"
  else
    echo "  ok    the two readers agree on all $pages page(s)"
  fi


  # the structure, as MuPDF checks it
  st=$(mutool run "$HERE/pdf-check.js" "$mu" 2>&1 | tr '\n' ' ')
  case "$st" in
    "ok ") echo "  ok    MuPDF finds nothing to repair" ;;
    *) echo "  FAIL  MuPDF: $st" ;;
  esac

  a=$(wc -c < "$mu" | tr -d ' ')
  if [ -s "$ref" ]; then
    echo "  size  $a bytes (the other route: $(wc -c < "$ref" | tr -d ' '))"
  else
    echo "  size  $a bytes"
  fi
}

# with SELF= set, the script only runs the checks of one document, and
# with EXPORT= set (the document, the PDF, yes or no for MuPDF, the home,
# separated by |) only one export: used below to run them side by side
if [ -n "${SELF:-}" ]; then check_doc "$SELF"; exit 0; fi
if [ -n "${EXPORT:-}" ]; then
  IFS='|' read -r e_doc e_pdf e_mu e_home <<E
$EXPORT
E
  convert "$e_doc" "$e_pdf" "$e_mu" "$e_home"; exit 0
fi

# 1. the exports, side by side, each TeXmacs with a home of its own; the
#    round trip of the embedded document alongside them
start=$(date +%s)
{
  for doc in $DOCS; do
    name=$(basename "$doc" .tm)
    new_home "$OUT/homes/$name-mu"
    echo "EXPORT='$doc|$OUT/$name-mupdf.pdf|yes|$OUT/homes/$name-mu' OUT='$OUT' WAIT=$WAIT sh '$0'"
    if [ "$REF" = 1 ]; then
      new_home "$OUT/homes/$name-ref"
      echo "EXPORT='$doc|$OUT/$name-ref.pdf|no|$OUT/homes/$name-ref' OUT='$OUT' WAIT=$WAIT sh '$0'"
    else rm -f "$OUT/$name-ref.pdf"; fi
  done
  if [ "$EMBED" = 1 ]; then
    new_home "$OUT/homes/embed"
    echo "sh '$HERE/embed-roundtrip.sh' '$OUT' '$OUT/homes/embed' > '$OUT/embed.result' 2>&1"
  fi
} > "$OUT/jobs.txt"
batch < "$OUT/jobs.txt"

# 2. the checks, side by side too, and then their results in order
for doc in $DOCS; do
  name=$(basename "$doc" .tm)
  echo "SELF='$name' OUT='$OUT' AGREE=$AGREE sh '$0' > '$OUT/$name.result' 2>&1"
done | batch

fail=0
for doc in $DOCS; do
  name=$(basename "$doc" .tm)
  echo "=== $name"
  cat "$OUT/$name.result"
  grep -q "FAIL" "$OUT/$name.result" && fail=1
done
if [ "$EMBED" = 1 ]; then
  echo "=== embedded document"
  cat "$OUT/embed.result"
  grep -q "FAIL" "$OUT/embed.result" && fail=1
fi
rm -rf "${OUT:?}/homes"

echo
echo "($(($(date +%s) - start)) s)"
[ "$fail" = 0 ] && echo "all checks passed" || echo "THERE WERE FAILURES"
exit $fail
