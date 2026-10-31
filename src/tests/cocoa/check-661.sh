#!/bin/sh
# Regression checks of the Cocoa interface for #661 (usage:
# tests/cocoa/check-661.sh, from the top of the source tree of a Cocoa build,
# or with BIN=<the executable of TeXmacs.app>):
#
#   tall     a document of 7*10^6 points (150000 cm in papyrus mode, taller
#            than the old cap of the frames, 5*10^6 points) is scrolled to
#            its end by go-end (the test aid "scroll" of the window test)
#   header   the window queries of the menus (visible-header?...) with the
#            editor of a console as the current one: false, not a "type
#            mismatch"
#   console  the console opens at its end (a race before the fix: the end
#            waited for an editor with the keyboard focus, so that this one
#            failed now and then only)
#
# The windows are shown (Cocoa has no offscreen mode). The logs stay in
# tests/build/cocoa-661.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/cocoa-661"
if [ -n "${BIN:-}" ]; then
  bin=$BIN
else
  bin="$top/TeXmacs/bin/texmacs.bin"
  export TEXMACS_PATH="$top/TeXmacs"
fi
[ -x "$bin" ] || { echo "no $bin, build TeXmacs first" >&2; exit 1; }
timeout=${TM_TEST_TIMEOUT:-60}
homes=${TM_TEST_HOME:-$out}
mkdir -p "$out"
total=0
failed=0

# run <case> <window test steps> <scheme expression>: TeXmacs in a fresh
# home, the expression after 0.5 s, the quit after 12 s
run () {
  name=$1; steps=$2; x=$3
  home="$homes/$name-home"
  rm -rf "${homes:?}/$name-home"
  mkdir -p "$home/system"
  HOME="$home" CFFIXED_USER_HOME="$home" \
  TEXMACS_HOME_PATH="$home" TEXMACS_NS_WINDOW_TEST="$steps" \
    perl -e 'alarm shift; exec @ARGV' "$timeout" "$bin" -x \
    "(begin (delayed (:pause 500) $x) (delayed (:pause 12000) (display \"CHECK alive\\n\") (quit-TeXmacs)))" \
    > "$out/$name.log" 2>&1
  echo "exit status $?" >> "$out/$name.log"
}

# verdict <case> <description> <command which says whether it passed>
verdict () {
  total=$((total + 1))
  log="$out/$1.log"
  why=""
  grep -aq '^CHECK alive' "$log" || why="TeXmacs did not reach its quit"
  grep -aq '^exit status 0$' "$log" || why="$why; $(tail -1 "$log")"
  grep -aqE 'Cocoa: an (error|Objective-C exception) was caught|type mismatch' "$log" &&
    why="$why; an error: $(grep -aE 'Cocoa: an|type mismatch' "$log" | head -2 | tr '\n' ' ')"
  shift; desc=$1; shift
  "$@" || why="$why; $fail"
  if [ -z "$why" ]; then echo "  ok     $desc"
  else
    echo "  FAILED $desc: ${why#; }"
    failed=$((failed + 1))
  fi
}

# (tall)
cat > "$out/tall.tm" <<'EOF'
<TeXmacs|2.1>

<style|generic>

<\body>
  Top

  <vspace|150000cm>

  The end
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>
EOF
run tall "scroll;scroll;scroll;scroll;scroll;scroll" \
    "(begin (load-buffer \"$out/tall.tm\") (delayed (:pause 2500) (go-end)))"
tall_ok () {
  last=$(grep -a "NSTEST scroll 'tall.tm'" "$out/tall.log" | tail -1)
  fail="not at the end of a document taller than 5*10^6 points: $last"
  h=$(echo "$last" | sed -n 's/.* document [0-9]*x\([0-9]*\) .*/\1/p')
  [ "${h:-0}" -gt 5000000 ] && echo "$last" | grep -aq 'at-end$'
}
verdict tall "a document of 7*10^6 points scrolls to its end" tall_ok

# (header)
probe='(with-buffer (string->url "tmfs://aux/console-error-messages-all") (display* "HEADER " (list (visible-header?) (visible-footer?) (visible-icon-bar? 0) (visible-side-tools? 0) (visible-bottom-tools? 0)) "\n"))'
run header "" "(begin (debug-message \"std-warning\" \"hello\n\") (open-error-messages) (delayed (:pause 2000) $probe))"
header_ok () {
  fail="no answer: $(grep -a '^HEADER' "$out/header.log")"
  grep -aq '^HEADER (#f #f #f #f #f)' "$out/header.log"
}
verdict header "the window queries of a console (the View menu)" header_ok

# (console)
msgs='(for-each (lambda (i) (debug-message "std-warning" (string-append "line " (number->string i) "\n"))) (iota 80))'
run console "windows;scroll:Error messages;scroll:Error messages;scroll:Error messages" \
    "(delayed (:pause 2000) $msgs (open-error-messages))"
console_ok () {
  last=$(grep -a "NSTEST scroll 'Error messages'" "$out/console.log" | tail -1)
  fail="the console is not at its end: $last"
  echo "$last" | grep -aq 'at-end$'
}
verdict console "the console opens at its end" console_ok

echo "$failed of $total failed"
[ "$failed" = 0 ]
