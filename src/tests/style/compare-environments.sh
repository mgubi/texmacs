#!/bin/sh
# Check the environment at the cursor when the editor typesets the rewritten
# tree: for each document, the cursor is put at the start of its strings (at
# most 300, spread over the document) and some variables of the environment
# are read (get-env), by a TeXmacs which typesets the document as usual and
# by one which typesets the result of the style rewriter
# (TEXMACS_STYLE_TYPESET=1); the two lists are compared.
#
# usage: tests/style/compare-environments.sh document.tm...
#
# Needs a build configured with --enable-experimental. The lists are kept in
# tests/build/style-environments. The macro definitions of the document are
# left out: putting the cursor inside one hangs, also without the rewriter.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/style-environments"
bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
[ $# -gt 0 ] || { echo "usage: $0 document.tm..." >&2; exit 1; }
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TM_TEST_HOME:-$out/home}"
# (the animations show the frame at a fixed time, the same in both prints)
export TEXMACS_ANIMATION_TIME="${TEXMACS_ANIMATION_TIME:-0}"
mkdir -p "$out" "$TEXMACS_HOME_PATH"

cat > "$out/env.scm" << 'EOF'
(define env-vars
  '("mode" "font-family" "font-series" "font-shape" "font-size" "color"
    "par-left" "par-first" "math-display" "prog-language" "language"))

(define (leaf-trees t)
  ;; (the paths are asked from TeXmacs, not built in Scheme)
  (cond ((tree-atomic? t) (list t))
        ((tree-is? t 'assign) '())
        (else (append-map leaf-trees (tree-children t)))))

(define (spread l n)
  (with k (max 1 (quotient (length l) n))
    (let loop ((l l) (i 0) (acc '()))
      (cond ((null? l) (reverse acc))
            ((= (modulo i k) 0) (loop (cdr l) (+ i 1) (cons (car l) acc)))
            (else (loop (cdr l) (+ i 1) acc))))))

(define (env-list f out)
  (load-buffer (system->url f))
  (switch-to-buffer (system->url f))
  (update-forced)
  (let ((paths (spread (map (lambda (u) (append (tree->path u) (list 0)))
                            (leaf-trees (buffer-tree)))
                       300)))
    (with port (open-output-file out)
      (for (p paths)
        (catch #t
          (lambda ()
            (go-to-path p)
            (write (cons p (cons (cursor-path) (map get-env env-vars))) port))
          (lambda args (write (list p 'error) port)))
        (newline port))
      (close-output-port port))))
EOF

status=0
for f in "$@"; do
  doc="$(cd "$(dirname "$f")" && pwd)/$(basename "$f")"
  name=$(basename "$f" .tm)
  for m in 0 1; do
    rm -f "$out/$name-$m.txt"
    TEXMACS_STYLE_TYPESET=$m perl -e 'alarm shift; exec @ARGV' "${TM_TEST_TIMEOUT:-600}" \
      "$bin" -headless \
      -x "(begin (load \"$out/env.scm\") (env-list \"$doc\" \"$out/$name-$m.txt\") (quit-TeXmacs))" \
      > "$out/$name-$m.log" 2>&1
  done
  n0=$(wc -l < "$out/$name-0.txt" 2> /dev/null | tr -d ' ')
  n1=$(wc -l < "$out/$name-1.txt" 2> /dev/null | tr -d ' ')
  if [ -z "$n0" ] || [ "$n1" != "$n0" ]; then
    echo "$name: incomplete (${n0:-0} and ${n1:-0} positions)"; status=1; continue
  fi
  d=$(diff "$out/$name-0.txt" "$out/$name-1.txt" | grep -c '^<')
  echo "$name: $d of $n0 positions have another environment"
  [ "$d" -gt 0 ] && status=1
done
exit $status
