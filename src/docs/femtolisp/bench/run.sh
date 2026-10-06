#!/bin/sh
# Runs the scripts of docs/s7/bench with several builds, alternating them on
# each workload, all on the same TeXmacs/ directory (each build gets its own
# home, under $BENCH_OUT). Usage, from any directory:
#   TEXMACS_TREE=<src>/TeXmacs BUILDS="femto:<dir1> s7:<dir2> guile:<dir3>" \
#     sh docs/femtolisp/bench/run.sh boot suites latex conversions marshal manual
# One line per run, RUN build workload round real=<s> rss=<MB>, followed by
# the lines which the script prints with its own timings. The first run of a
# build in a new home also builds the font caches: run each workload once to
# warm up, or ignore the first round.
B=${BENCH_OUT:-/tmp/femto-bench}; mkdir -p $B
TP=${TEXMACS_TREE:?set TEXMACS_TREE to the TeXmacs/ directory to run}
BENCH=$(cd "$(dirname "$0")/../../s7/bench" && pwd)
BUILDS=${BUILDS:?set BUILDS to "name:build-dir ..." (each with TeXmacs/bin/texmacs.bin)}

run () { # build-name dir workload round expr
  n=$1; d=$2; w=$3; r=$4; expr=$5
  log=$B/log-$n-$w-$r.txt
  /usr/bin/time -l env TEXMACS_PATH=$TP TEXMACS_HOME_PATH=$B/home-$n \
    QT_QPA_PLATFORM=offscreen perl -e 'alarm shift; exec @ARGV' 900 \
    $d/TeXmacs/bin/texmacs.bin -x "$expr" $QUIT > $log 2>&1
  real=$(awk '/ real /{print $1}' $log)
  rss=$(awk '/maximum resident set size/{printf "%.0f", $1/1048576}' $log)
  echo "RUN $n $w $r real=$real rss=${rss}MB"
  LC_ALL=C grep -a -E "SUITES-TIME|LOOP-DONE|^BENCH|^MANUAL|^MARSHAL|FAILED" $log | sed 's/^/    /'
}

for w in "$@"; do
  QUIT=-q
  case $w in
    boot)        rounds=5; expr='(exit 0)' ;;
    suites)      rounds=3; expr="(load \"$BENCH/suites.scm\")" ;;
    latex)       rounds=3; expr="(load \"$BENCH/latex-loop.scm\")" ;;
    conversions) rounds=1; expr="(load \"$BENCH/conversions.scm\")" ;;
    marshal)     rounds=1; expr="(load \"$BENCH/marshal.scm\")" ;;
    manual)      rounds=3; QUIT=; expr="(load \"$BENCH/manual.scm\")" ;;
  esac
  r=1
  while [ $r -le $rounds ]; do
    for b in $BUILDS; do run "${b%%:*}" "${b#*:}" $w $r "$expr"; done
    r=$((r+1))
  done
done
