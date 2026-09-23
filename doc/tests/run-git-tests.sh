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
GIT_TEST_DIR="$dir" TEXMACS_HOME_PATH="$dir/home" TEXMACS_PATH="$src/TeXmacs" \
  perl -e 'alarm 120; exec @ARGV' TeXmacs/bin/texmacs.bin $opts \
  -x "(begin (catch #t (lambda () (load \"$here/$test\")) (lambda args (display* \"TEST-ERROR \" args \"\\n\") (quit-TeXmacs))) (if (headless?) (quit-TeXmacs)))" 2>&1 \
  | grep -E '^(ok|FAIL|FAILURES|TEST-ERROR)'
