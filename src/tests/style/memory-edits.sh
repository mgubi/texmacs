#!/bin/sh
# The memory of the style rewriter over many edits: in the middle of the
# document, a letter is typed and removed (or, with "para", a paragraph made
# and joined) n times, the document being typeset after each edit, without a
# window. Every 100 edits a line gives the number of edits, the statistics
# of the rewriter (evaluations, memorized, size of the memorizer table) and
# the memory of TeXmacs in bytes.
#
# usage: tests/style/memory-edits.sh document.tm [n [same|para [0|1]]]
#
#   n      the number of edits (1000)
#   0|1    TEXMACS_STYLE_TYPESET: 1 (default) typesets the rewritten tree,
#          0 the document as usual, for comparison
#
# Needs a build configured with --enable-experimental. The table should
# keep its size; the memory grows as with the usual typesetting (the undo
# history), see "Memory" in src/docs/pure-style.md.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="${TM_STYLE_OUT:-$top/tests/build/style-memory}"
bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
[ $# -ge 1 ] || { echo "usage: $0 document.tm [n [same|para [0|1]]]" >&2; exit 1; }
doc=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
n="${2:-1000}"
mode="${3:-same}"
flag="${4:-1}"
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TM_TEST_HOME:-$out/home}"
mkdir -p "$out" "$TEXMACS_HOME_PATH"
result="$out/memory.txt"
: > "$result"

cat > "$out/memory.scm" << 'EOF'
(define out-file "")
(define (line s)
  (string-append-to-file (string-append s "\n") (system->url out-file)))
(define (middle-of-document)
  (let* ((leaves (let walk ((t (buffer-tree)))
                   (cond ((tree-atomic? t) (list t))
                         ((tree-is? t 'assign) '())
                         (else (append-map walk (tree-children t))))))
         (mid (list-ref leaves (quotient (length leaves) 2))))
    (go-to-path (append (tree->path mid) (list 0)))))
(define (report i)
  (line (string-append (number->string i) " "
                       (object->string (style-rewrite-statistics)) " "
                       (number->string (texmacs-memory)))))
(define (run f out n mode)
  (set! out-file out)
  (load-buffer (system->url f))
  (switch-to-buffer (system->url f))
  (update-forced)
  (middle-of-document)
  (update-forced)
  (report 0)
  (for (i (iota n))
    (if (even? i)
        (if (== mode "para") (insert-return) (insert "x"))
        (remove-text #f))
    (update-forced)
    (when (== (modulo (+ i 1) 100) 0) (report (+ i 1)))))
EOF

TEXMACS_STYLE_TYPESET="$flag" "$bin" -headless \
  -x "(begin (load \"$out/memory.scm\") (run \"$doc\" \"$result\" $n \"$mode\") (quit-TeXmacs))" \
  > "$out/log.txt" 2>&1
cat "$result"
