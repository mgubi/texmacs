#!/bin/sh
# Run the Git tests against the TeXmacs built in this checkout.
# Usage: doc/tests/run-git-tests.sh [--gui] [scratch-dir]
#   without --gui: headless tests (git-test.scm)
#   with --gui:    tests needing the event loop (git-gui-test.scm), run with
#                  the offscreen Qt platform, so that no window is shown
# A private TEXMACS_HOME_PATH is used, so the user's settings are not touched.

gui=no
if test "$1" = "--gui"; then gui=yes; shift; fi
here=$(cd "$(dirname "$0")" && pwd)
src=$(cd "$here/../../src" && pwd)
dir=${1:-$(mktemp -d)}
# every run starts from fresh preferences and repositories
rm -rf "$dir/home" "$dir/new repo" "$dir/outside.tm"
mkdir -p "$dir/home"

tm () {
  printf '<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n  %s\n</body>\n' "$1"
}

if test $gui = no; then
  rm -rf "$dir/repo test" "$dir/wt test"
  mkdir -p "$dir/repo test/sub dir"
  (
    cd "$dir/repo test" || exit 1
    git init -q -b main
    git config user.email test@example.com
    git config user.name "Test User"
    tm "Hello world." > "sub dir/a b.tm"
    echo base > base.txt
    git add base.txt
    git commit -q -m base
    git worktree add -q "$dir/wt test" -b wt
    # a repository for the merge driver
    rm -rf "$dir/drv" && mkdir -p "$dir/drv" && cd "$dir/drv" || exit 1
    git init -q -b main
    git config user.email test@example.com
    git config user.name "Test User"
    tm "The quick brown fox jumps." > paper.tm
    git add paper.tm
    git commit -q -m base
    git checkout -q -b theirs
    tm "The quick brown fox leaps." > paper.tm
    git commit -q -a -m theirs
    git checkout -q -b conflict
    tm "The fast brown fox leaps." > paper.tm
    git commit -q -a -m conflict
    git checkout -q main
    tm "The slow brown fox jumps." > paper.tm
    git commit -q -a -m ours
  )
  # a repository for blame, change descriptions, projects and snapshots
  rm -rf "$dir/proj" && mkdir -p "$dir/proj" && cd "$dir/proj" || exit 1
  git init -q -b main
  git config user.email test@example.com
  git config user.name "Test User"
  doc () {
    printf '<TeXmacs|2.1>\n\n<style|generic>\n\n<\\body>\n'
    for p in "$@"; do printf '  %s\n\n' "$p"; done
    printf '</body>\n'
  }
  doc "<section|Intro>" "One." "Two." "Three." > paper.tm
  git add paper.tm && git commit -q -m c1
  git config user.name "Second Author"
  doc "<section|Intro>" "One." "Two, revised." "Three." > paper.tm
  git commit -q -a -m c2
  git config user.name "Third Author"
  doc "<section|Intro>" "One." "Two, revised." "Three." "<section|Results>" "Four." > paper.tm
  git commit -q -a -m c3
  git config user.name "Test User"
  doc "<section|Intro>" "One, not committed." "Two, revised." "Three." "<section|Results>" "Four." > paper.tm
  doc "<include|part.tm>" "<image|fig.png|1par|||>" "<bibliography|bib|tm-plain|refs|<\\bib-list|0>\n  </bib-list>>" > main.tm
  doc "A part." > part.tm
  echo png > fig.png
  echo "@article{a, title={A}}" > refs.bib
  git add main.tm && git commit -q -m main
  # a repository whose configuration runs a program
  rm -rf "$dir/evil" "$dir/evil-pwned" && mkdir -p "$dir/evil" && cd "$dir/evil" || exit 1
  git init -q
  echo x > a.tm
  git config core.fsmonitor "touch '$dir/evil-pwned'; false"
  # a fake GnuPG, which signs anything
  cat > "$dir/fake-gpg" <<'GPG'
#!/bin/sh
cat > /dev/null
printf '\n[GNUPG:] SIG_CREATED D 1 8 00 1234567890 ABCDEF\n' >&2
printf -- '-----BEGIN PGP SIGNATURE-----\n\nfake\n-----END PGP SIGNATURE-----\n'
GPG
  chmod +x "$dir/fake-gpg"
  test=git-test.scm
  opts=-headless
else
  rm -rf "$dir/remote" "$dir/conflict" "$dir/conflict2"
  mkdir -p "$dir/remote" "$dir/conflict"
  (
    cd "$dir/remote" || exit 1
    git init -q --bare -b main origin.git
    for c in a b; do
      git clone -q origin.git "clone $c" 2> /dev/null
      git -C "clone $c" config user.email $c@example.com
      git -C "clone $c" config user.name "User $c"
    done
    cd "$dir/conflict" || exit 1
    git init -q -b main
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
    git init -q -b main
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
