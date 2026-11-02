#!/bin/bash
# The build knows the git commit it was made from (#753): the header made by
# the build (src/Objects/git_revision.hpp) and, unless BIN is "-", the output
# of BIN -v ("Revision <hash> <time>") say the commit of the checkout; a
# package which says "Custom" fails the job.
# Usage (in src): ../.github/scripts/check-revision.sh BIN|-
set -u
want=$(git log -1 --abbrev=10 --format=%h | tr -d '\r')
[ -n "$want" ] || { echo "error: git gives no commit in $PWD"; exit 1; }
hdr=src/Objects/git_revision.hpp
grep "TM_GIT_REVISION" "$hdr" | tr -d '\r'
grep -q "TM_GIT_REVISION \"$want " "$hdr" || { echo "error: $hdr does not have the commit $want"; exit 1; }
[ "$1" = - ] && exit 0
out=$(QT_QPA_PLATFORM=offscreen TEXMACS_HOME_PATH=$(mktemp -d) "$1" -v 2>&1 | tr -d '\r')
echo "$out"
echo "$out" | grep -q "^Revision $want " || { echo "error: $1 -v does not say the commit $want"; exit 1; }
