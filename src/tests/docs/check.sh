#!/bin/sh
# Typeset the whole documentation of TeXmacs without a window: the books of
# Help > Full manuals, exported to PDF, and every .tm file of TeXmacs/doc one
# by one. The problems found are compared with a committed reference, so
# that a new problem fails while the known ones are only listed.
#
# usage: tests/docs/check.sh [-u] [-b | -f] [-l lan[,lan...]]
#
#   -b       only the books
#   -f       only the individual files
#   -l lans  only these languages (suffixes of the files: en, fr, de...);
#            by default all of them
#   -u       update the reference tests/docs/ref/summary.txt with the
#            results of this run (the lines of the parts which were not run
#            are kept)
#
# Books (doc/tmdoc.scm, Help > Full manuals): main/man-user-manual,
# devel/source/source and devel/scheme/scheme in every language in which
# they exist. Each is expanded as tmfs://help/book/... does, typeset, its
# table of contents and index generated three times like the menu does
# (tmdoc-expand-help-manual), then exported to PDF in tests/build/docs; mutool
# counts the pages, extracts the text and must read the PDF without syntax
# error. Problems: unknown tags, primitives with a wrong number of
# arguments, references to undefined labels, empty table of contents or
# index, warnings and errors of the log (labels defined twice...).
#
# Files: every TeXmacs/doc/**/*.tm is loaded and typeset in a buffer
# (update-forced), in one TeXmacs process (restarted after a crash, which
# is reported for the file). Problems: files which do not parse, a language
# which does not match the suffix, tags which are neither primitives nor
# defined by the style or the document (they are shown in red), primitives
# with a wrong number of arguments, branches of tmdoc (branch,
# extra-branch, continue) whose file tmdoc-expand would not find, hyperlinks
# to files which do not exist or to labels which the target does not
# define, missing images, and the warnings and errors of the log while the
# file is typeset.
#
# Output in tests/build/docs: the PDFs and their text, out-*.txt (raw
# results), log-*.txt (TeXmacs logs), summary.txt, which is compared with
# tests/docs/ref/summary.txt. The summary has one line per problem
#   file|book <tab> path in TeXmacs/doc <tab> kind <tab> detail
# and one line per statistic of a book
#   stat <tab> path <tab> pages|toc|idx <tab> number
# A problem which is not in the reference fails; a problem of the reference
# which is gone is reported (update with -u); the statistics may differ by
# 5% from the reference (fonts differ between machines).
#
# TeXmacs runs with TEXMACS_PATH set to the source tree and a scratch
# TEXMACS_HOME_PATH (tests/build/docs/home, or TM_TEST_HOME), so the user's
# settings do not leak in. Requires mutool (mupdf). Everything takes about
# 9 minutes (books 4, files 5); TM_DOCS_CHUNK files are checked per
# TeXmacs process (default 150) and TM_DOCS_TIMEOUT seconds stop a hung
# process (default 900).

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/docs"
doc="$top/TeXmacs/doc"
ref="$here/ref/summary.txt"
update=""
what="all"
lans=""
while getopts "ubfl:" opt; do
  case $opt in
    u) update=1 ;;
    b) what=books ;;
    f) what=files ;;
    l) lans=$(echo "$OPTARG" | tr ',' ' ') ;;
    *) exit 2 ;;
  esac
done
shift $((OPTIND - 1))

bin="$top/TeXmacs/bin/texmacs.bin"
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
command -v mutool > /dev/null || { echo "mutool is needed" >&2; exit 1; }

export LC_ALL=C   # the files are Cork-encoded, not UTF-8
export TEXMACS_PATH="$top/TeXmacs"
export TEXMACS_HOME_PATH="${TM_TEST_HOME:-$out/home}"
mkdir -p "$out" "$TEXMACS_HOME_PATH"
timeout=${TM_DOCS_TIMEOUT:-900}
scm="$here/build.scm"
[ -z "$lans" ] && lans=$(find "$doc" -name '*.tm' |
                         sed -n 's/.*\.\([a-z][a-z]\)\.tm$/\1/p' | sort -u)

start=$(date +%s)
: > "$out/summary.txt"
: > "$out/ran.txt"   # the parts of the reference which this run covers

run_tm () {
  # run_tm log expr: TeXmacs without a window, stopped after $timeout s;
  # an error in a -x expression would keep TeXmacs from quitting
  perl -e 'alarm shift; exec @ARGV' "$timeout" "$bin" \
    -x "(begin (catch #t (lambda () (load \"$scm\") $2)
                         (lambda args (display* \"error: \" args \"\\n\")))
               (quit))" -q >> "$1" 2>&1
}

# the warnings and errors which TeXmacs prints between the BEGIN and END
# lines of a file, as problems of that file
log_problems () {
  # log_problems log type
  awk -F '\t' -v type="$2" -v doc="$doc/" '
    /^BEGIN\t/ { cur = $2; next }
    /^END\t/   { cur = ""; next }
    /^TEXMACS_PATH is set|^Welcome to TeXmacs|^warning: guile hooks/ { cur = ""; next }
    /^(PROBLEM|INFO)\t/ { next }
    cur == "" { next }
    /approximating font|convert command is deprecated|Generating (table|index)/ { next }
    type == "book" && /Undefined reference/ { next }
    tolower($0) ~ /warning|error|abort|segmentation|exception|bad link/ {
      msg = $0; gsub(doc, "", msg); sub(/^TeXmacs\] /, "", msg)
      f = cur; sub(doc, "", f)
      key = f "\t" msg
      if (!(key in seen)) { seen[key] = 1; print type "\tlog\t" f "\t" msg }
    }' "$1" |
  awk -F '\t' '{ print $1 "\t" $3 "\t" $2 "\t" $4 }'
}

# the problems of an out file, relative to the doc directory
out_problems () {
  # out_problems out type
  awk -F '\t' -v type="$2" -v doc="$doc/" '
    /^PROBLEM\t/ { f = $2; sub(doc, "", f); print type "\t" f "\t" $3 "\t" $4 }
  ' "$1"
}

pct_diff () {
  # pct_diff a b: 1 if a and b differ by more than 5%
  awk -v a="$1" -v b="$2" 'BEGIN {
    d = a - b; if (d < 0) d = -d; m = (a > b ? a : b)
    print (m > 0 && d * 20 > m) ? 1 : 0 }'
}

#### books

if [ "$what" != files ]; then
  for lan in $lans; do
    for book in main/man-user-manual devel/source/source devel/scheme/scheme
    do
      root="$doc/$book.$lan.tm"
      [ -f "$root" ] || continue
      rel="$book.$lan.tm"
      name=$(echo "$rel" | sed 's|/|-|g; s|\.tm$||')
      pdf="$out/$name.pdf"
      res="$out/out-$name.txt"
      log="$out/log-$name.txt"
      rm -f "$pdf" "$res" "$log"
      printf "book\t%s\n" "$rel" >> "$out/ran.txt"
      t0=$(date +%s)
      run_tm "$log" "(docs-build-book \"$root\" \"$pdf\" \"$res\")"
      code=$?
      t1=$(date +%s)
      [ -f "$res" ] || : > "$res"
      if ! grep -q "^END" "$res"; then
        printf "book\t%s\tcrash\texit status %s\n" "$rel" "$code" \
          >> "$out/summary.txt"
      fi
      if [ ! -f "$pdf" ]; then
        printf "book\t%s\tpdf\tno PDF\n" "$rel" >> "$out/summary.txt"
        echo "$rel: no PDF ($((t1 - t0)) s, see $log)"
        continue
      fi
      pages=$(mutool info "$pdf" 2>/dev/null | sed -n 's/^Pages: //p')
      mutool draw -q -F txt -o "$out/$name.txt" "$pdf" > "$out/$name.mutool" 2>&1
      errors=$(grep -c 'syntax error' "$out/$name.mutool")
      [ "$errors" != 0 ] &&
        printf "book\t%s\tpdf\t%s syntax errors\n" "$rel" "$errors" \
          >> "$out/summary.txt"
      printf "stat\t%s\tpages\t%s\n" "$rel" "$pages" >> "$out/summary.txt"
      for k in toc idx; do
        n=$(awk -F '\t' -v k=$k '$1 == "INFO" && $3 == k { print $4 }' "$res")
        printf "stat\t%s\t%s\t%s\n" "$rel" "$k" "${n:-0}" >> "$out/summary.txt"
      done
      out_problems "$res" book >> "$out/summary.txt"
      log_problems "$log" book >> "$out/summary.txt"
      echo "$rel: $pages pages ($((t1 - t0)) s)"
    done
  done
fi

#### individual files

if [ "$what" != books ]; then
  list="$out/files.txt"
  : > "$list"
  for lan in $lans; do
    find "$doc" -name "*.$lan.tm" | sort >> "$list"
  done
  sed "s|^$doc/|file	|" "$list" >> "$out/ran.txt"
  res="$out/out-files.txt"
  log="$out/log-files.txt"
  rm -f "$res" "$log"
  : > "$res"
  todo="$out/todo.txt"
  # TeXmacs gets slower and slower when it typesets hundreds of documents
  # (three times slower after a thousand), so it is restarted after every
  # chunk of files
  chunk=${TM_DOCS_CHUNK:-150}
  cp "$list" "$todo"
  t0=$(date +%s)
  while [ -s "$todo" ]; do
    before=$(grep -c '^BEGIN' "$res")
    head -n "$chunk" "$todo" > "$todo.chunk"
    run_tm "$log" "(docs-check-files \"$todo.chunk\" \"$res\")"
    code=$?
    if [ "$(grep -c '^BEGIN' "$res")" = "$before" ]; then
      echo "TeXmacs fails (exit status $code, see $log)"; break
    fi
    # the files after the last one done still have to be checked; one
    # which was begun but not ended made TeXmacs crash or hang
    last=$(awk -F '\t' '$1 == "BEGIN" { f = $2 } END { print f }' "$res")
    if ! tail -1 "$res" | grep -q "^END"; then
      printf "PROBLEM\t%s\tcrash\texit status %s\nEND\t%s\t0\n" \
        "$last" "$code" "$last" >> "$res"
      echo "$last: crash (exit status $code)"
    fi
    awk -v last="$last" 'found { print } $0 == last { found = 1 }' \
      "$todo" > "$todo.new"
    mv "$todo.new" "$todo"
  done
  rm -f "$todo" "$todo.chunk"
  t1=$(date +%s)
  out_problems "$res" file >> "$out/summary.txt"
  log_problems "$log" file >> "$out/summary.txt"
  echo "files: $(grep -c '^END' "$res") of $(wc -l < "$list" | tr -d ' ') checked ($((t1 - t0)) s)"
fi

#### comparison with the reference

# no raw bytes above 127 (the documents are Cork-encoded): \xNN instead
perl -pe 's/([\x80-\xff])/sprintf("\\x%02x", ord $1)/ge' "$out/summary.txt" |
  sort -u > "$out/summary.tmp"
mv "$out/summary.tmp" "$out/summary.txt"
mkdir -p "$here/ref"
[ -f "$ref" ] || : > "$ref"
# the lines of the reference which belong to the parts of this run
awk -F '\t' 'NR == FNR { ran[$1 "\t" $2] = 1; if ($1 == "book") ran["stat\t" $2] = 1; next }
             (($1 "\t" $2) in ran) { print }' "$out/ran.txt" "$ref" |
  sort -u > "$out/ref-part.txt"

status=0
if [ -n "$update" ]; then
  awk -F '\t' 'NR == FNR { ran[$1 "\t" $2] = 1; if ($1 == "book") ran["stat\t" $2] = 1; next }
               !(($1 "\t" $2) in ran) { print }' "$out/ran.txt" "$ref" |
    cat - "$out/summary.txt" | sort -u > "$ref.new"
  mv "$ref.new" "$ref"
  echo "reference updated: $(grep -vc '^stat' "$ref") known problems"
else
  grep -v '^stat' "$out/summary.txt" > "$out/problems.txt"
  grep -v '^stat' "$out/ref-part.txt" > "$out/ref-problems.txt"
  comm -23 "$out/problems.txt" "$out/ref-problems.txt" > "$out/new.txt"
  comm -13 "$out/problems.txt" "$out/ref-problems.txt" > "$out/gone.txt"
  known=$(comm -12 "$out/problems.txt" "$out/ref-problems.txt" | wc -l | tr -d ' ')
  echo "known problems: $known (see $ref)"
  if [ -s "$out/new.txt" ]; then
    echo "FAILED, new problems:"; sed 's/^/  /' "$out/new.txt"; status=1
  fi
  if [ -s "$out/gone.txt" ]; then
    echo "problems of the reference which are gone (update with -u):"
    sed 's/^/  /' "$out/gone.txt"
  fi
  grep '^stat' "$out/summary.txt" | while IFS='	' read -r s rel k n; do
    r=$(awk -F '\t' -v p="$rel" -v k="$k" '$1 == "stat" && $2 == p && $3 == k { print $4 }' "$out/ref-part.txt")
    if [ -z "$r" ]; then
      echo "  $rel: $k $n (not in the reference)"
    elif [ "$(pct_diff "$n" "$r")" = 1 ]; then
      echo "FAILED, $rel: $k $n instead of $r"; echo fail >> "$out/stat-fail"
    fi
  done
  if [ -f "$out/stat-fail" ]; then rm -f "$out/stat-fail"; status=1; fi
fi
echo "time: $(( $(date +%s) - start )) s"
exit $status
