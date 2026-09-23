#!/bin/sh
# Run the headless Git tests against the TeXmacs built in this checkout.
# Usage: doc/tests/run-git-tests.sh [scratch-dir]
# Uses a private TEXMACS_HOME_PATH, so the user's settings are not touched.

here=$(cd "$(dirname "$0")" && pwd)
src=$(cd "$here/../../src" && pwd)
dir=${1:-$(mktemp -d)}
mkdir -p "$dir/home"
rm -rf "$dir/repo test" "$dir/wt test"
mkdir -p "$dir/repo test/sub dir"
(
  cd "$dir/repo test" || exit 1
  git init -q -b main
  git config user.email test@example.com
  git config user.name "Test User"
  printf '<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n  Hello world.\n</body>\n' > "sub dir/a b.tm"
  echo base > base.txt
  git add base.txt
  git commit -q -m base
  git worktree add -q "$dir/wt test" -b wt
)
cd "$src" || exit 1
GIT_TEST_DIR="$dir" TEXMACS_HOME_PATH="$dir/home" TEXMACS_PATH="$src/TeXmacs" \
  perl -e 'alarm 120; exec @ARGV' TeXmacs/bin/texmacs.bin -headless \
  -x "(begin (catch #t (lambda () (load \"$here/git-test.scm\")) (lambda args (display* \"TEST-ERROR \" args \"\\n\"))) (quit-TeXmacs))" 2>&1 \
  | grep -E '^(ok|FAIL|FAILURES|TEST-ERROR)'
