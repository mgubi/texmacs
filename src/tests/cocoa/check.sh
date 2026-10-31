#!/bin/sh
# Regression checks of the Cocoa interface (#621): failures which used to be
# silent, or to abort TeXmacs, while the windows come and go.
#
# usage: tests/cocoa/check.sh
#
# from the top of the source tree of a Cocoa build (--with-gui=cocoa), or
# with BIN=<the executable of TeXmacs.app> (CI: the application of the disk
# image, which finds its own TEXMACS_PATH). Each case starts TeXmacs with a
# home of its own (tests/build/cocoa/<case>-home, or under TM_TEST_HOME;
# also its HOME, for the defaults of macOS), drives
# it with the test aids of the Cocoa port (TEXMACS_NS_WINDOW_TEST: one step
# a second from the fourth second, see ns_tm_widget.mm) and commands of -x,
# and checks its output and its exit status:
#
#   title    a dialog whose title has an accent in French ("Preferences de
#            l'utilisateur", in the Cork encoding): the window is made with
#            the right title, and no string conversion warns (#468)
#   close    the console with several tabs, closed with its
#            close button: TeXmacs goes on, without an error (#619, #628)
#   idle     the console left idle: it is not typeset again and again (no
#            warning of a typesetting loop, #620)
#   tips     the buttons of the icon bars keep the tracking areas of their
#            tooltips when theirs is updated (#637)
#
# Every case also fails on an error caught at an entry point from AppKit
# ("Cocoa: an error was caught", "Cocoa: an Objective-C exception"), on an
# abort and on a timeout (TM_TEST_TIMEOUT seconds, default 60). The windows
# are shown (Cocoa has no offscreen mode): run it in a session where they
# may appear for a few seconds. The logs stay in tests/build/cocoa.

here=$(cd "$(dirname "$0")" && pwd)
top=$(cd "$here/../.." && pwd)
out="$top/tests/build/cocoa"
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

# run <case> <language> <window test steps> <scheme expression>: TeXmacs in
# a fresh home, the expression after 2.5 s, the quit after 14 s
run () {
  name=$1; lang=$2; steps=$3; x=$4
  home="$homes/$name-home"
  rm -rf "${homes:?}/$name-home"
  mkdir -p "$home/system"
  [ -n "$lang" ] &&
    printf '(("language" "%s"))\n' "$lang" > "$home/system/preferences.scm"
  # (HOME and CFFIXED_USER_HOME: the defaults of macOS are not those of
  # the user either)
  HOME="$home" CFFIXED_USER_HOME="$home" \
  TEXMACS_HOME_PATH="$home" TEXMACS_NS_WINDOW_TEST="$steps" \
    perl -e 'alarm shift; exec @ARGV' "$timeout" "$bin" -x \
    "(begin (delayed (:pause 2500) $x) (delayed (:pause 14000) (display \"CHECK alive\\n\") (quit-TeXmacs)))" \
    > "$out/$name.log" 2>&1
  echo "exit status $?" >> "$out/$name.log"
}

# verdict <case> <description> <condition (a command)>
verdict () {
  total=$((total + 1))
  log="$out/$1.log"
  why=""
  grep -aq '^CHECK alive' "$log" || why="TeXmacs did not reach its quit"
  grep -aq '^exit status 0$' "$log" || why="$why; $(tail -1 "$log")"
  grep -aqE 'Cocoa: an (error|Objective-C exception) was caught|terminating due to|Throwing ' "$log" &&
    why="$why; an error was caught: $(grep -aE 'Cocoa: an|terminating|Throwing ' "$log" | head -3 | tr '\n' ' ')"
  shift
  desc=$1; shift
  "$@" || why="$why; $fail"
  if [ -z "$why" ]; then echo "  ok     $desc"
  else
    echo "  FAILED $desc: ${why#; }"
    failed=$((failed + 1))
  fi
}

# (title) the title is printed in UTF-8 by the window test; the warnings of
# the conversions are on debug-aqua
run title french "windows;windows;windows" "(open-preferences)"
title=$(printf "Pr\303\251f\303\251rences de l'utilisateur")
title_ok () {
  fail="no window '$title'"
  grep -aq "NSTEST window '$title'" "$out/title.log" || return 1
  fail="a string conversion warned: $(grep -a 'to_nsstring\|from_nsstring' "$out/title.log" | head -2 | tr '\n' ' ')"
  ! grep -aq 'debug-aqua, \(to\|from\)_nsstring' "$out/title.log"
}
verdict title "a French dialog title with accents (#468)" title_ok

# (close) messages of several kinds give the console several tabs
msgs='(for-each (lambda (ch) (debug-message ch (string-append ch " line\n"))) (list "std-error" "std-warning" "debug-io" "debug-automatic"))'
run close "" "windows;windows;windows;windows;close:Error messages;windows;windows" \
    "(begin $msgs (open-error-messages))"
close_ok () {
  fail="the console was not open"
  grep -aq "NSTEST window 'Error messages' shown" "$out/close.log" || return 1
  fail="the console is still shown after its close"
  ! sed -n '/NSTEST step close:Error messages/,$p' "$out/close.log" |
    grep -aq "NSTEST window 'Error messages' shown"
}
verdict close "closing the console with several tabs (#619)" close_ok

# (idle) the warning of edit_interface_rep::apply_changes after 20 full
# typesettings in more than 5 s without input
run idle "" "windows;windows;windows;windows;windows;windows;windows" "(begin $msgs (open-error-messages))"
idle_ok () {
  fail="the console was not open"
  grep -aq "NSTEST window 'Error messages' shown" "$out/idle.log" || return 1
  fail="$(grep -a 'typeset again from scratch' "$out/idle.log" | head -1)"
  ! grep -aq 'typeset again from scratch' "$out/idle.log"
}
verdict idle "an idle console is not typeset again and again (#620)" idle_ok

# (tips) the main icon bar has buttons with a tooltip (New, Open...); each
# has one tracking area for its hover highlight and, while its tooltip is
# active, some of AppKit: they must survive an update of the tracking areas
run tips "" "windows;bar-tips" "(display \"\")"
tips_ok () {
  before=$(grep -a 'NSTEST bar-tips before' "$out/tips.log")
  after=$(grep -a 'NSTEST bar-tips after' "$out/tips.log")
  fail="no buttons with a tooltip: $before"
  n=$(echo "$before" | sed -n 's/.*: \([0-9]*\) buttons.*/\1/p')
  [ "${n:-0}" -gt 0 ] || return 1
  fail="the tracking areas changed: '$before' then '$after'"
  [ "${before#*before}" = "${after#*after}" ] || return 1
  fail="the buttons have no tooltip areas, or not one hover area each: $after"
  echo "$after" | grep -aq ": $n buttons with a tooltip, $n hover areas, [1-9]"
}
verdict tips "tooltips of the icon bars survive an update (#637)" tips_ok

echo "$failed of $total failed"
[ "$failed" = 0 ]
