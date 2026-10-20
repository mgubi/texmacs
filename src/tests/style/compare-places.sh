#!/bin/sh
# Check the places of the boxes when the editor typesets the rewritten tree:
# for each document, the paths of the document found at a grid of points of
# its boxes (xy->path, what a click finds) are computed by a TeXmacs which
# typesets the document as usual and by one which typesets the result of the
# style rewriter (TEXMACS_STYLE_TYPESET=1), and compared.
#
# usage: tests/style/compare-places.sh document.tm...
#
# The grid has 50 x 100 points, every 20000 units from the top left corner
# of the document. Needs a build configured with --enable-experimental. The
# maps are kept in tests/build/style-places.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/style-places"
bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
[ $# -gt 0 ] || { echo "usage: $0 document.tm..." >&2; exit 1; }
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TM_TEST_HOME:-$out/home}"
mkdir -p "$out" "$TEXMACS_HOME_PATH"

cat > "$out/map.scm" << 'EOF'
(define (place-map f out)
  (load-buffer (system->url f))
  (switch-to-buffer (system->url f))
  (update-forced)
  (with port (open-output-file out)
    (for (j (iota 100))
      (for (i (iota 50))
        (let ((x (* i 20000)) (y (- (* j 20000))))
          (write (list x y (xy->path x y)) port)
          (newline port))))
    (close-output-port port)))
EOF

status=0
for f in "$@"; do
  doc="$(cd "$(dirname "$f")" && pwd)/$(basename "$f")"
  name=$(basename "$f" .tm)
  for m in 0 1; do
    rm -f "$out/$name-$m.txt"
    TEXMACS_STYLE_TYPESET=$m perl -e 'alarm shift; exec @ARGV' "${TM_TEST_TIMEOUT:-600}" \
      "$bin" -headless \
      -x "(begin (load \"$out/map.scm\") (place-map \"$doc\" \"$out/$name-$m.txt\") (quit-TeXmacs))" \
      > "$out/$name-$m.log" 2>&1
  done
  n0=$(wc -l < "$out/$name-0.txt" 2> /dev/null | tr -d ' ')
  n1=$(wc -l < "$out/$name-1.txt" 2> /dev/null | tr -d ' ')
  if [ "$n1" != "$n0" ]; then
    echo "$name: the map with the rewritten tree stops after ${n1:-0} of $n0 points"
    status=1; continue
  fi
  d=$(diff "$out/$name-0.txt" "$out/$name-1.txt" | grep -c '^<')
  echo "$name: $d of $n0 points find another path"
  [ "$d" -gt 0 ] && status=1
done
exit $status
