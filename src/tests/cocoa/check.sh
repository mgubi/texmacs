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
#   sizes    two dialogs with a glue and a resize of negative sizes: the
#            sizes given to AppKit are replaced, with one warning on
#            debug-aqua for each place in all, and the dialogs are shown
#            (#621, #650, #666)
#   raise    an error thrown in an entry point from AppKit (a command of
#            the interface run by the update of -[TMDocView scrolled:]) is
#            reported once, and TeXmacs goes on: later commands and updates
#            run (#650, #666)
#   tall     a document of 7*10^6 points (150000 cm in papyrus mode, taller
#            than the old cap of the frames, 5*10^6 points) is scrolled to
#            its end by go-end (the test aid "scroll" of the window test)
#            (#661)
#   header   the window queries of the menus (visible-header?...) with the
#            editor of a console as the current one: false, not a "type
#            mismatch" (#661)
#   console  the console opens at its end (a race before the fix: the end
#            waited for an editor with the keyboard focus, so that this one
#            failed now and then only) (#661)
#   quiet    a warning from a delayed command opens the console, without
#            the focus: the document stays the key and main window and the
#            current buffer, and gets the keys typed. A click gives the
#            console the focus, and its close gives it back (#679, #619)
#   focused  the console which the user opens takes the focus (#679)
#
# The last two make TeXmacs the active application again and again, for about
# 35 seconds, which takes the keyboard from whoever works on that screen (keys
# typed then go into the test document): they run in CI, or with
# TM_COCOA_FOCUS=1. They are told to be skipped otherwise, and when TeXmacs does
# not manage to become active; do not use the screen while they run.
#
# Every case also fails on an error caught at an entry point from AppKit
# ("Cocoa: an error was caught", "Cocoa: an Objective-C exception",
# "Throwing"), on a "type mismatch", on an abort and on a timeout (TM_TEST_TIMEOUT seconds,
# default 60), except the errors which the case provokes itself and those
# of the list of known harmless messages below (extended with
# TM_COCOA_HARMLESS, an extended regular expression). The windows are shown
# (Cocoa has no offscreen mode): run it in a session where they may appear
# for a few seconds. The logs stay in tests/build/cocoa.

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

# the errors which do not fail a case (extended regular expressions, one
# per line): known harmless messages, none so far
harmless='
'
[ -n "${TM_COCOA_HARMLESS:-}" ] && harmless="$harmless
$TM_COCOA_HARMLESS"
# the errors which the next case provokes (set before its verdict)
expected=''

# run <case> <language> <window test steps> <scheme expression> [<scheme
# definitions>]: TeXmacs in a fresh home, the definitions at the start, the
# expression after 2.5 s, the quit after 14 s (or quit_after ms, for this run)
run () {
  name=$1; lang=$2; steps=$3; x=$4; defs=${5:-}
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
    "(begin $defs (delayed (:pause 2500) $x) (delayed (:pause ${quit_after:-14000}) (display \"CHECK alive\\n\") (quit-TeXmacs)))" \
    > "$out/$name.log" 2>&1
  echo "exit status $?" >> "$out/$name.log"
  quit_after=
}

# verdict <case> <description> <condition (a command)>
verdict () {
  total=$((total + 1))
  log="$out/$1.log"
  why=""
  grep -aq '^CHECK alive' "$log" || why="TeXmacs did not reach its quit"
  grep -aq '^exit status 0$' "$log" || why="$why; $(tail -1 "$log")"
  errors=$(grep -aE 'Cocoa: an (error|Objective-C exception) was caught|terminating due to|Throwing |type mismatch' "$log")
  allowed=$(printf '%s\n%s\n' "$harmless" "$expected" | grep -v '^$' | paste -sd '|' -)
  [ -n "$allowed" ] && errors=$(echo "$errors" | grep -avE "$allowed")
  [ -n "$errors" ] &&
    why="$why; an error was caught: $(echo "$errors" | head -3 | tr '\n' ' ')"
  expected=''
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

# (sizes) the glue and the resize of two dialogs have negative sizes: the
# lengths are replaced by 0 for AppKit, with one warning for each place
# (ns_safe_length, ns_safe_size) in all the run, and the dialogs are shown.
# (ns_decode_length also warns about the lengths of the resize, each time
# they are decoded: not counted here)
widget='(tm-widget (cocoa-check-sizes cmd) (resize "-100px" "-20px" (glue #f #f -10 -20)) (text "Bad sizes"))'
run sizes "" "windows;windows;windows" \
    '(begin (dialogue-window cocoa-check-sizes noop "Bad sizes") (dialogue-window cocoa-check-sizes noop "Bad sizes again"))' \
    "$widget"
sizes_ok () {
  for title in "Bad sizes" "Bad sizes again"; do
    fail="the dialog '$title' is not shown"
    grep -aq "NSTEST window '$title' shown" "$out/sizes.log" || return 1
  done
  for place in glue_widget resize_widget; do
    n=$(grep -ac "debug-aqua, make_widget ($place): a bad \(length\|size\) for AppKit" "$out/sizes.log")
    fail="$n warnings for the sizes of make_widget ($place), not one"
    [ "$n" = 1 ] || return 1
  done
}
verdict sizes "negative sizes of dialogs: one warning for each place (#650)" sizes_ok

# (raise) a command of the interface which throws a C++ error (cpp-error),
# run by the update which -[TMDocView scrolled:] forces: the error is
# reported once, at that entry point, and the commands and the updates go
# on (another scroll, a delayed command, the quit)
run raise "" 'windows;scroll-eval:(begin (display "CHECK raise\n") (cpp-error));scroll-eval:(display "CHECK scrolled again\n");eval:(display "CHECK later\n");windows' \
    '(display "")'
expected='Throwing an error occurred
Cocoa: an error was caught in -\[TMDocView scrolled:\]'
raise_ok () {
  log="$out/raise.log"
  fail="the error was not thrown"
  grep -aq '^CHECK raise' "$log" || return 1
  n=$(grep -ac '^Throwing an error occurred' "$log")
  m=$(grep -acF 'Cocoa: an error was caught in -[TMDocView scrolled:]' "$log")
  fail="the error was reported $n and $m times, not once at -[TMDocView scrolled:]: $(grep -a 'Cocoa: an' "$log" | head -2 | tr '\n' ' ')"
  [ "$n" = 1 ] && [ "$m" = 1 ] || return 1
  fail="no command after the error"
  sed -n '/^CHECK raise/,$p' "$log" | grep -aq '^CHECK scrolled again' &&
    sed -n '/^CHECK raise/,$p' "$log" | grep -aq '^CHECK later'
}
verdict raise "an error at an entry point is reported once, TeXmacs goes on (#650)" raise_ok

# (tall) the document is there before the go-end, the scroll steps come after
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
run tall "" "scroll:tall.tm;scroll:tall.tm;scroll:tall.tm;scroll:tall.tm;scroll:tall.tm;scroll:tall.tm" \
    "(begin (load-buffer \"$out/tall.tm\") (delayed (:pause 2500) (go-end)))"
tall_ok () {
  last=$(grep -a "NSTEST scroll 'tall.tm'" "$out/tall.log" | tail -1)
  fail="not at the end of a document taller than 5*10^6 points: $last"
  h=$(echo "$last" | sed -n 's/.* document [0-9]*x\([0-9]*\) .*/\1/p')
  [ "${h:-0}" -gt 5000000 ] && echo "$last" | grep -aq 'at-end$'
}
verdict tall "a document of 7*10^6 points scrolls to its end (#661)" tall_ok

# (header)
probe='(with-buffer (string->url "tmfs://aux/console-error-messages-all") (display* "HEADER " (list (visible-header?) (visible-footer?) (visible-icon-bar? 0) (visible-side-tools? 0) (visible-bottom-tools? 0)) "\n"))'
run header "" "" "(begin (debug-message \"std-warning\" \"hello\n\") (open-error-messages) (delayed (:pause 2000) $probe))"
header_ok () {
  fail="no answer: $(grep -a '^HEADER' "$out/header.log")"
  grep -aq '^HEADER (#f #f #f #f #f)' "$out/header.log"
}
verdict header "the window queries of a console, the View menu (#661)" header_ok

# (console)
lines='(for-each (lambda (i) (debug-message "std-warning" (string-append "line " (number->string i) "\n"))) (iota 80))'
run console "" "windows;scroll:Error messages;scroll:Error messages;scroll:Error messages;scroll:Error messages;scroll:Error messages;scroll:Error messages" \
    "(begin $lines (open-error-messages))"
console_ok () {
  last=$(grep -a "NSTEST scroll 'Error messages'" "$out/console.log" | tail -1)
  fail="the console is not at its end: $last"
  echo "$last" | grep -aq 'at-end$'
}
verdict console "the console opens at its end (#661)" console_ok

# (quiet, focused) the document comes a few seconds after the start, in the
# window of the welcome message of a fresh home. The focus needs TeXmacs to
# be the active application, which the steps ask for just before they look
# (another application may take over, and an editor without the focus has no
# idle time, which a console opening by itself waits for). When TeXmacs never
# gets there, the case says so instead of failing: nothing can be seen of the
# focus
cat > "$out/quiet.tm" <<'EOF'
<TeXmacs|2.1>

<style|generic>

<\body>
  Top
</body>
EOF
focus_defs='(tm-define (cocoa-check-state) (display* "CHECK current " (url-tail (current-buffer)) " " (tree->stree (buffer-get-body (current-buffer))) "\n"))'
focus_steps='front:quiet.tm;front:quiet.tm;front:quiet.tm;windows:activate'
state='eval:(cocoa-check-state)'
is_active () {
  grep -aq "^NSTEST type to '[^']" "$out/$1.log"
}

focus_cases=${CI:-}${TM_COCOA_FOCUS:-}

warnings='(for-each (lambda (ch) (debug-message ch (string-append ch " line\n"))) (list "std-warning" "debug-io" "debug-automatic"))'
quit_after=28000
[ -n "$focus_cases" ] && run quiet "" "$focus_steps;eval:$warnings;activate;windows;$state;type:xy;$state;$state;click:Error messages;windows;$state;$state;close:Error messages;windows;type:zw;$state;$state" \
    "(load-buffer \"$out/quiet.tm\")" "$focus_defs"
quiet_ok () {
  log="$out/quiet.log"
  opened=$(sed -n '1,/NSTEST step click:/p' "$log")
  fail="the console did not open"
  echo "$opened" | grep -aq "^NSTEST window 'Error messages' shown" || return 1
  fail="the console took the focus: $(echo "$opened" | grep -a "^NSTEST window 'Error messages' shown" | head -1)"
  ! echo "$opened" | grep -aqE "^NSTEST window 'Error messages' shown .*(main|key) " || return 1
  fail="the current buffer changed: $(echo "$opened" | grep -a '^CHECK current' | grep -av '<url quiet.tm>' | head -1 | cut -c1-80)"
  ! echo "$opened" | grep -a '^CHECK current' | grep -avq '<url quiet.tm>' || return 1
  fail="the document did not stay the key and main window: $(echo "$opened" | grep -a '^NSTEST type to')"
  echo "$opened" | grep -aq "^NSTEST type to 'quiet.tm', main 'quiet.tm'" || return 1
  fail="the document did not get the keys typed: $(echo "$opened" | grep -a '^CHECK current' | tail -1 | cut -c1-80)"
  echo "$opened" | grep -aq '^CHECK current <url quiet.tm> .*xyTop"' || return 1
  clicked=$(sed -n '/NSTEST step click:/,/NSTEST step close:/p' "$log")
  fail="a click did not give the console the focus: $(echo "$clicked" | grep -a "^CHECK current" | cut -c1-80 | tr '\n' ' ')"
  echo "$clicked" | grep -aq '^CHECK current <url console-' || return 1
  closed=$(sed -n '/NSTEST step close:/,$p' "$log")
  fail="its close did not leave the document focused: $(echo "$closed" | grep -a "^NSTEST window 'Error\|^NSTEST type to\|^CHECK current" | cut -c1-80 | tr '\n' ' ')"
  ! echo "$closed" | grep -aq "^NSTEST window 'Error messages' shown" &&
    echo "$closed" | grep -aq "^NSTEST type to 'quiet.tm', main 'quiet.tm'" &&
    echo "$closed" | grep -aq '^CHECK current <url quiet.tm> .*xyzwTop"'
}
desc="a console which opens by itself leaves the focus to the document (#679)"
if [ -z "$focus_cases" ]; then echo "  skip   $desc: set TM_COCOA_FOCUS=1"
elif is_active quiet; then verdict quiet "$desc" quiet_ok
else echo "  skip   $desc: TeXmacs is not the active application"; fi

[ -n "$focus_cases" ] && run focused "" "$focus_steps;eval:(open-error-messages);activate;type:a;$state;$state" \
    "(load-buffer \"$out/quiet.tm\")" "$focus_defs"
focused_ok () {
  log="$out/focused.log"
  fail="the console opened by the user did not take the focus: $(grep -a "^NSTEST type to\|^CHECK current" "$log" | cut -c1-80 | tr '\n' ' ')"
  grep -aq "^NSTEST type to 'Error messages', main 'Error messages'" "$log" &&
    grep -aq '^CHECK current <url console-' "$log"
}
desc="a console opened by the user takes the focus (#679)"
if [ -z "$focus_cases" ]; then echo "  skip   $desc: set TM_COCOA_FOCUS=1"
elif is_active focused; then verdict focused "$desc" focused_ok
else echo "  skip   $desc: TeXmacs is not the active application"; fi

echo "$failed of $total failed"
[ "$failed" = 0 ]
