#!/bin/sh
# Check the typesetting of the rewritten tree by the editor itself: every
# document is printed by a TeXmacs which typesets it as usual and by one
# which typesets the result of the style rewriter (TEXMACS_STYLE_TYPESET=1),
# and the pages are compared pixel by pixel.
#
# usage: tests/style/compare-editor.sh [document.tm...]
#
#   (no document: every English page of the documentation, about 700)
#
# Needs a build configured with --enable-experimental and mutool. The two
# printings run in parallel, each in one TeXmacs; the summary is
# tests/build/style-editor/summary.txt. The syntax highlighting of programs
# is switched off (see src/docs/pure-style.md).

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/style-editor"
bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
command -v mutool > /dev/null || { echo "mutool is needed" >&2; exit 1; }
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TM_TEST_HOME:-$out/home}"
rm -rf "$out/jobs" "$out/summary.txt"
mkdir -p "$out/jobs" "$TEXMACS_HOME_PATH"

list="$out/jobs/list.txt"
if [ $# -eq 0 ]; then
  find "$TEXMACS_PATH/doc" -name '*.en.tm' | sort > "$list"
else
  : > "$list"
  for f in "$@"; do echo "$(cd "$(dirname "$f")" && pwd)/$(basename "$f")" >> "$list"; done
fi

cat > "$out/jobs/print.scm" << 'EOF'
(define (print-list file)
  (when (defined? 'tree-sitter-set-highlighting)
    (tree-sitter-set-highlighting #f))
  (for (line (string-tokenize-by-char (string-load (system->url file)) #\newline))
    (with l (string-tokenize-by-char line #\space)
      (when (== (length l) 2)
        (catch #t
          (lambda ()
            (let ((u (system->url (car l))))
              (load-buffer u)
              (switch-to-buffer u)
              (update-forced)
              (update-current-buffer)
              (update-forced)
              (print-to-file (system->url (cadr l)))
              (buffer-close u)))
          (lambda args (display* "error: " (car l) " " args "\n")))))))
EOF

for m in 0 1; do
  : > "$out/jobs/jobs-$m.txt"
  i=0
  while read -r f; do
    i=$((i+1)); echo "$f $out/jobs/$(printf %04d $i)-$m.pdf" >> "$out/jobs/jobs-$m.txt"
  done < "$list"
  TEXMACS_STYLE_TYPESET=$m perl -e 'alarm shift; exec @ARGV' "${TM_TEST_TIMEOUT:-7200}" \
    "$bin" -headless \
    -x "(begin (load \"$out/jobs/print.scm\") (print-list \"$out/jobs/jobs-$m.txt\") (quit-TeXmacs))" \
    > "$out/run-$m.log" 2>&1 &
done
wait

i=0
while read -r f; do
  i=$((i+1)); k="$out/jobs/$(printf %04d $i)"
  if [ ! -s "$k-0.pdf" ] || [ ! -s "$k-1.pdf" ]; then
    echo "NOPDF $f" >> "$out/summary.txt"; continue
  fi
  mutool draw -q -r 72 -o "$k-0-%03d.pnm" "$k-0.pdf" 2> /dev/null
  mutool draw -q -r 72 -o "$k-1-%03d.pnm" "$k-1.pdf" 2> /dev/null
  n0=$(ls "$k"-0-*.pnm 2> /dev/null | wc -l | tr -d ' ')
  n1=$(ls "$k"-1-*.pnm 2> /dev/null | wc -l | tr -d ' ')
  pages=""
  for a in "$k"-0-*.pnm; do
    b=$(echo "$a" | sed 's/-0-\([0-9]*\)\.pnm$/-1-\1.pnm/')
    p=$(basename "$a" .pnm); p=${p##*-}
    cmp -s "$a" "$b" || pages="$pages $p"
  done
  if [ "$n0" != "$n1" ] || [ -n "$pages" ]; then
    echo "DIFF $f pages $n0/$n1:$pages" >> "$out/summary.txt"
  else
    echo "SAME $f" >> "$out/summary.txt"; rm -f "$k"-*.pnm
  fi
done < "$list"

grep '^DIFF\|^NOPDF' "$out/summary.txt" | sed "s|$TEXMACS_PATH/doc/||"
echo "same $(grep -c '^SAME' "$out/summary.txt")," \
     "different $(grep -c '^DIFF' "$out/summary.txt"),"\
     "not printed $(grep -c '^NOPDF' "$out/summary.txt"),"\
     "fallbacks $(grep -c 'failed, typesetting' "$out/run-1.log")"
grep -q '^DIFF\|^NOPDF' "$out/summary.txt" && exit 1
exit 0
