#!/bin/sh
# The lookup of an evaluation by the values which it read (find_by_reads,
# src/src/Style/Evaluate/evaluate_main.cpp) must give the trees which the
# evaluations it replaces give: the rewritten tree of every document is
# written with the lookup (TEXMACS_STYLE_READS=on) and without it (off),
# also after a section inserted at the start of the document and after its
# removal, and the two are compared.
#
# usage: tests/style/compare-reads.sh [document.tm...]
#
#   (no document: every English page of the documentation, about 700)
#
# Needs a build configured with --enable-experimental. The current date and
# the identifiers made from addresses (hard-id) change from run to run and
# are masked. The summary is tests/build/style-reads/summary.txt; the exit
# code is 1 when a document differs. TM_STYLE_OUT chooses another folder.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="${TM_STYLE_OUT:-$top/tests/build/style-reads}"
bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TM_TEST_HOME:-$out/home}"
rm -rf "$out/on" "$out/off" "$out/summary.txt"
mkdir -p "$out/on" "$out/off" "$TEXMACS_HOME_PATH"

list="$out/list.txt"
if [ $# -eq 0 ]; then
  find "$TEXMACS_PATH/doc" -name '*.en.tm' | sort > "$list"
else
  : > "$list"
  for f in "$@"; do echo "$(cd "$(dirname "$f")" && pwd)/$(basename "$f")" >> "$list"; done
fi

cat > "$out/trees.scm" << 'EOF'
(define (save-result out tag)
  ;; (s7 cuts long lists with "..." unless print-length is raised)
  (string-save
    (if (defined? '*s7*)
        (let-temporarily (((*s7* 'print-length) 9223372036854775807))
          (object->string (tree->stree (style-rewrite-buffer))))
        (object->string (tree->stree (style-rewrite-buffer))))
    (system->url (string-append out "." tag))))
(define (one f out)
  (load-buffer (system->url f))
  (switch-to-buffer (system->url f))
  (catch #t
    (lambda ()
      (save-result out "a")
      (let ((b (buffer-tree)))
        (when (tree-is? b 'document)
          (tree-insert! b 0 (list (stree->tree '(section "Inserted"))))
          (save-result out "b")
          (tree-remove! b 0 1)
          (save-result out "c"))))
    (lambda args (string-save "error" (system->url (string-append out ".err")))))
  (buffer-pretend-saved (system->url f))
  (buffer-close (system->url f)))
(define (run-list lst dir)
  (let ((i 0))
    (for (f (string-tokenize-by-char (string-load (system->url lst)) #\newline))
      (when (!= f "")
        (set! i (+ i 1))
        (one f (string-append dir "/" (number->string i)))))))
EOF

for m in on off; do
  TEXMACS_STYLE_READS=$m perl -e 'alarm shift; exec @ARGV' "${TM_TEST_TIMEOUT:-7200}" \
    "$bin" -headless \
    -x "(begin (load \"$out/trees.scm\") (run-list \"$list\" \"$out/$m\") (quit-TeXmacs))" \
    > "$out/run-$m.log" 2>&1 &
done
wait

mask () {
  # (the trees hold bytes of any encoding: sed in the C locale; a failure
  # gives no output, which must not count as the same)
  LC_ALL=C sed -E -e 's/%[0-9A-F]+-[0-9A-F]+-?-?[0-9]*/%ID/g' \
    -e 's/20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]T[0-9:]+[+-][0-9]+/DATE/g' "$1" ||
    echo "mask failed $1"
}

same=0; diff=0
i=0
while read -r f; do
  i=$((i+1)); r="SAME"
  for t in a b c; do
    p="$out/on/$i.$t"; q="$out/off/$i.$t"
    if [ ! -s "$p" ] || [ ! -s "$q" ]; then
      if [ -e "$out/on/$i.err" ] && [ -e "$out/off/$i.err" ]; then continue; fi
      r="NONE"; break
    fi
    if [ "$(mask "$p" 2>&1 | cksum)" != "$(mask "$q" 2>&1 | cksum)" ]; then
      r="DIFF"; break
    fi
  done
  echo "$r $f" >> "$out/summary.txt"
  if [ "$r" = "SAME" ]; then same=$((same+1)); else diff=$((diff+1)); fi
done < "$list"

grep -v '^SAME' "$out/summary.txt"
echo "same $same, different $diff"
[ $diff -eq 0 ]
