#!/bin/sh
# The time of typing with and without the typesetting of the rewritten tree
# (TEXMACS_STYLE_TYPESET=1): in each document, 20 characters are typed in
# the middle of the document and 5 new paragraphs made there, the document
# being typeset after each, without a window.
#
# usage: tests/style/bench-typing.sh document.tm...
#
# Needs a build configured with --enable-experimental. The timings vary with
# the load of the machine: repeat before concluding.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/style-bench"
bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
[ $# -gt 0 ] || { echo "usage: $0 document.tm..." >&2; exit 1; }
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TM_TEST_HOME:-$out/home}"
mkdir -p "$out" "$TEXMACS_HOME_PATH"

cat > "$out/bench.scm" << 'EOF'
(define (middle-of-document)
  (let* ((leaves (let walk ((t (buffer-tree)))
                   (cond ((tree-atomic? t) (list t))
                         ((tree-is? t 'assign) '())
                         (else (append-map walk (tree-children t))))))
         (mid (list-ref leaves (quotient (length leaves) 2))))
    (go-to-path (append (tree->path mid) (list 0)))))

(define (bench f)
  (load-buffer (system->url f))
  (switch-to-buffer (system->url f))
  (update-forced)
  (middle-of-document)
  (update-forced)
  (let ((t0 (texmacs-time)))
    (for (i (iota 20)) (insert "x") (update-forced))
    (display* "keystroke " (/ (- (texmacs-time) t0) 20.0) " ms\n"))
  (let ((t0 (texmacs-time)))
    (for (i (iota 5)) (insert-return) (update-forced))
    (display* "paragraph " (/ (- (texmacs-time) t0) 5.0) " ms\n")))
EOF

for f in "$@"; do
  doc="$(cd "$(dirname "$f")" && pwd)/$(basename "$f")"
  name=$(basename "$f" .tm)
  for m in 0 1; do
    TEXMACS_STYLE_TYPESET=$m perl -e 'alarm shift; exec @ARGV' "${TM_TEST_TIMEOUT:-600}" \
      "$bin" -headless \
      -x "(begin (load \"$out/bench.scm\") (bench \"$doc\") (quit-TeXmacs))" \
      > "$out/$name-$m.log" 2>&1
    echo "$name, $( [ $m = 1 ] && echo rewritten || echo usual ):" \
         $(grep '^keystroke\|^paragraph' "$out/$name-$m.log" | tr '\n' ' ')
  done
done
