#!/bin/bash
# The build knows the git commit it was made from (#753): the header made by
# the build (src/Objects/git_revision.hpp) and the program BIN say the commit
# of the checkout, exactly (not "(unverified)", which a failing git leaves);
# a package which says "Custom" fails the job.
#   BIN is run with -v ("Revision <hash> <time>") unless --no-run is given
#   (Windows: a program without console); its bytes are searched for the
#   commit in any case.
# Usage (in src): ../.github/scripts/check-revision.sh [--no-run] BIN
set -u
run=yes
if [ "$1" = --no-run ]; then run=no; shift; fi
bin=$1
[ -f "$bin" ] || { [ -f "$bin.exe" ] && bin=$bin.exe; }
want=$(git log -1 --abbrev=10 --format=%h | tr -d '\r')
[ -n "$want" ] || { echo "error: git gives no commit in $PWD"; exit 1; }
hdr=src/Objects/git_revision.hpp
grep "TM_GIT_REVISION" "$hdr" | tr -d '\r'
grep -q "TM_GIT_REVISION \"$want [^ ]*\"\$" <(tr -d '\r' < "$hdr") \
  || { echo "error: $hdr does not have exactly the commit $want"; exit 1; }
[ -f "$bin" ] || { echo "error: no program $bin"; exit 1; }
grep -aq "$want [0-9]" "$bin" || { echo "error: $bin does not hold the commit $want"; exit 1; }
if [ $run = yes ]; then
  out=$(QT_QPA_PLATFORM=offscreen TEXMACS_HOME_PATH=$(mktemp -d) "$bin" -v 2>&1 | tr -d '\r')
  echo "$out"
  echo "$out" | grep -q "^Revision $want [^ ]*\$" || { echo "error: $bin -v does not say exactly the commit $want"; exit 1; }
fi
