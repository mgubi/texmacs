#!/bin/sh
# Check the style rewriter by typesetting: every document is printed as it
# is and with its body replaced by the rewritten body, and the pages are
# compared pixel by pixel. If the rewriter is right they are identical.
#
# usage: tests/style/compare.sh [document.tm...]
#
#   (no document: every English page of the documentation, about 700,
#   12 minutes)
#
# Needs a build configured with --enable-experimental (the commands
# style-rewrite-buffer and the like) and mutool. The pages are drawn at 72
# dpi under tests/build/style/<n>/; those of the identical documents are
# removed. The summary, one line SAME, DIFF (with the differing pages) or
# NOPDF per document, is tests/build/style/summary.txt. See
# src/docs/pure-style.md, "Typesetting the result", for what the
# differences were and the known ones left. To find where two pages differ:
# python3 -I tests/style/pnmdiff.py tests/build/style/jobs/<n>/o001.pnm
# tests/build/style/jobs/<n>/r001.pnm (the box around the differing pixels).

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/style"
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
jobs="$out/jobs/jobs.txt"
: > "$jobs"
i=0
while read -r f; do
  i=$((i+1)); d="$out/jobs/$(printf %04d $i)"; mkdir -p "$d"
  echo "$f $d" >> "$jobs"
done < "$list"

perl -e 'alarm shift; exec @ARGV' "${TM_TEST_TIMEOUT:-7200}" "$bin" -headless \
  -x "(begin (load \"$here/compare.scm\") (compare-list \"$jobs\") (quit-TeXmacs))" \
  > "$out/run.log" 2>&1

while read -r f d; do
  if [ ! -s "$d/orig.pdf" ] || [ ! -s "$d/rewritten.pdf" ]; then
    echo "NOPDF $f" >> "$out/summary.txt"; continue
  fi
  mutool draw -q -r 72 -o "$d/o%03d.pnm" "$d/orig.pdf" 2> /dev/null
  mutool draw -q -r 72 -o "$d/r%03d.pnm" "$d/rewritten.pdf" 2> /dev/null
  no=$(ls "$d"/o*.pnm 2> /dev/null | wc -l | tr -d ' ')
  nr=$(ls "$d"/r*.pnm 2> /dev/null | wc -l | tr -d ' ')
  pages=""
  for o in "$d"/o*.pnm; do
    p=$(basename "$o" .pnm); p=${p#o}
    cmp -s "$o" "$d/r$p.pnm" || pages="$pages $p"
  done
  if [ "$no" != "$nr" ] || [ -n "$pages" ]; then
    echo "DIFF $f pages $no/$nr:$pages" >> "$out/summary.txt"
  else
    echo "SAME $f" >> "$out/summary.txt"; rm -f "$d"/*.pnm
  fi
done < "$jobs"

grep '^DIFF\|^NOPDF' "$out/summary.txt" | sed "s|$TEXMACS_PATH/doc/||"
echo "same $(grep -c '^SAME' "$out/summary.txt")," \
     "different $(grep -c '^DIFF' "$out/summary.txt"),"\
     "not printed $(grep -c '^NOPDF' "$out/summary.txt")"
grep -q '^DIFF\|^NOPDF' "$out/summary.txt" && exit 1
exit 0
