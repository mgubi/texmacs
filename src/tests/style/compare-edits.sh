#!/bin/sh
# Check the incremental typesetting of the rewritten tree: the same edits
# are made in each document (at six places: typing, a new paragraph, typing,
# a backspace, a second new paragraph joined again to the one before it, the
# document being typeset after each), by a TeXmacs which
# typesets the document as usual and by one which typesets the result of the
# style rewriter (TEXMACS_STYLE_TYPESET=1); the printed pages are compared
# pixel by pixel.
#
# usage: tests/style/compare-edits.sh document.tm...
#
# The documents are copied first (the edits are not saved). Needs a build
# configured with --enable-experimental and mutool; the results are in
# tests/build/style-edits.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/style-edits"
bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
command -v mutool > /dev/null || { echo "mutool is needed" >&2; exit 1; }
[ $# -gt 0 ] || { echo "usage: $0 document.tm..." >&2; exit 1; }
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TM_TEST_HOME:-$out/home}"
mkdir -p "$out" "$TEXMACS_HOME_PATH"

cat > "$out/edit.scm" << 'EOF'
(define (edit-print f out)
  (when (defined? 'tree-sitter-set-highlighting)
    (tree-sitter-set-highlighting #f))
  (load-buffer (system->url f))
  (switch-to-buffer (system->url f))
  (update-forced)
  (let* ((leaves (let walk ((t (buffer-tree)))
                   (cond ((tree-atomic? t) (list t))
                         ((tree-is? t 'assign) '())
                         (else (append-map walk (tree-children t))))))
         (n (length leaves))
         ;; (the strings themselves: their paths move with the edits)
         (spots (map (lambda (k) (list-ref leaves (quotient (* k n) 7)))
                     '(1 2 3 4 5 6))))
    (for (u spots)
      (when (tree->path u)
        (go-to-path (append (tree->path u) (list 0)))
        (insert "Edit") (update-forced)
        (insert-return) (update-forced)
        (insert "More") (update-forced)
        (kbd-backspace) (update-forced)
        (insert-return) (update-forced)
        (go-start-paragraph) (kbd-backspace) (update-forced))))
  (update-current-buffer)
  (update-forced)
  (print-to-file (system->url out)))
EOF

status=0
for f in "$@"; do
  name=$(basename "$f" .tm)
  for m in 0 1; do
    cp "$f" "$out/$name-$m.tm"
    rm -f "$out/$name-$m.pdf" "$out/$name-$m"-*.pnm
    TEXMACS_STYLE_TYPESET=$m perl -e 'alarm shift; exec @ARGV' "${TM_TEST_TIMEOUT:-600}" \
      "$bin" -headless \
      -x "(begin (load \"$out/edit.scm\") (edit-print \"$out/$name-$m.tm\" \"$out/$name-$m.pdf\") (quit-TeXmacs))" \
      > "$out/$name-$m.log" 2>&1
    mutool draw -q -r 72 -o "$out/$name-$m-%03d.pnm" "$out/$name-$m.pdf" 2> /dev/null
  done
  if [ ! -s "$out/$name-0.pdf" ] || [ ! -s "$out/$name-1.pdf" ]; then
    echo "$name: not printed"; status=1; continue
  fi
  n0=$(ls "$out/$name-0"-*.pnm | wc -l | tr -d ' ')
  n1=$(ls "$out/$name-1"-*.pnm | wc -l | tr -d ' ')
  pages=""
  for a in "$out/$name-0"-*.pnm; do
    b=$(echo "$a" | sed 's/-0-\([0-9]*\)\.pnm$/-1-\1.pnm/')
    p=$(basename "$a" .pnm); p=${p##*-}
    cmp -s "$a" "$b" || pages="$pages $p"
  done
  if [ "$n0" != "$n1" ] || [ -n "$pages" ]; then
    echo "$name: pages $n0/$n1, different:$pages"; status=1
  else
    echo "$name: identical ($n0 pages)"
  fi
done
exit $status
