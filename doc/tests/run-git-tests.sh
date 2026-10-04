#!/bin/sh
# Run the Git tests against the TeXmacs built in this checkout.
# Usage: doc/tests/run-git-tests.sh [--gui] [scratch-dir]
#   without --gui: the suites git and version of tests/scheme/check.sh
#                  (TeXmacs/progs/check/git-test.scm and version-test.scm)
#   with --gui:    tests needing the event loop (git-gui-test.scm), run with
#                  the offscreen Qt platform, so that no window is shown
# A private TEXMACS_HOME_PATH is used, so the user's settings are not touched.

gui=no
if test "$1" = "--gui"; then gui=yes; shift; fi
here=$(cd "$(dirname "$0")" && pwd)
src=$(cd "$here/../../src" && pwd)
dir=${1:-$(mktemp -d)}
# every run starts from fresh preferences and repositories
rm -rf "$dir/home" "$dir/outside.tm"
mkdir -p "$dir/home"

tm () {
  printf '<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n  %s\n</body>\n' "$1"
}

if test $gui = no; then
  # the headless tests are the suites git and version of the test harness
  TM_TEST_HOME="$dir/home" exec "$src/tests/scheme/check.sh" git version
else
  rm -rf "$dir/remote" "$dir/conflict" "$dir/conflict2"
  mkdir -p "$dir/remote" "$dir/conflict"
  (
    cd "$dir/remote" || exit 1
    git init -q --bare origin.git && git -C origin.git symbolic-ref HEAD refs/heads/main
    for c in a b; do
      git clone -q origin.git "clone $c" 2> /dev/null
      git -C "clone $c" config user.email $c@example.com
      git -C "clone $c" config user.name "User $c"
    done
    cd "$dir/conflict" || exit 1
    git init -q && git symbolic-ref HEAD refs/heads/main
    git config user.email test@example.com
    git config user.name "Test User"
    tm "First paragraph." > paper.tm
    git add paper.tm
    git commit -q -m base
    git checkout -q -b theirs
    tm "First paragraph, as they wrote it." > paper.tm
    git commit -q -a -m theirs
    git checkout -q main
    tm "First paragraph, as we wrote it." > paper.tm
    git commit -q -a -m ours
    git merge theirs > /dev/null 2>&1
    # a conflict for git, but not for a structured merge
    mkdir -p "$dir/conflict2" && cd "$dir/conflict2" || exit 1
    git init -q && git symbolic-ref HEAD refs/heads/main
    git config user.email test@example.com
    git config user.name "Test User"
    tm2 () {
      printf '<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n  %s\n\n  %s\n</body>\n' "$1" "$2"
    }
    tm2 "The quick brown fox jumps." "Second." > paper.tm
    git add paper.tm
    git commit -q -m base
    git checkout -q -b theirs
    tm2 "The quick brown fox leaps." "Second." > paper.tm
    git commit -q -a -m theirs
    git checkout -q main
    tm2 "The slow brown fox jumps." "Second." > paper.tm
    git commit -q -a -m ours
    git merge theirs > /dev/null 2>&1
  )
  test=git-gui-test.scm
  opts=
  QT_QPA_PLATFORM=offscreen
  export QT_QPA_PLATFORM
fi

cd "$src" || exit 1
log="$dir/test.log"
GIT_TEST_DIR="$dir" TEXMACS_HOME_PATH="$dir/home" TEXMACS_PATH="$src/TeXmacs" \
  perl -e 'alarm 300; exec @ARGV' TeXmacs/bin/texmacs.bin $opts \
  -x "(begin (catch #t (lambda () (load \"$here/$test\")) (lambda args (display* \"TEST-ERROR \" args \"\\n\") (quit-TeXmacs))) (if (headless?) (quit-TeXmacs)))" \
  > "$log" 2>&1
grep -E '^(ok|FAIL|FAILURES|TEST-ERROR)' "$log"
# errors in call backs and widgets do not stop the tests: report them
n=$(grep -c -E 'Guile error|bad format' "$log")
if test "$n" != "0"; then
  echo "FAIL $n Scheme errors in $log:"
  grep -E -B2 'Guile error|bad format' "$log" | head -12
fi
# the exit status tells whether all tests ran and passed
if grep -q '^FAILURES: 0$' "$log" && test "$n" = "0" &&
   ! grep -q -E '^(FAIL |TEST-ERROR)' "$log"; then
  exit 0
else
  grep -q '^FAILURES:' "$log" || echo "FAIL the tests did not complete (crash or time out), see $log"
  exit 1
fi
