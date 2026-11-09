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
#   colors   without a window (-headless): an unknown color name gives an
#            opaque black, also in a PDF (black, white... of colors.cpp are
#            set, #656)
#   headless -headless is parsed by the Cocoa build: (headless?) is true and
#            TeXmacs quits by itself after the command line
#   sizes    two dialogs with a glue and a resize of negative sizes: the
#            sizes given to AppKit are replaced, with one warning on
#            debug-aqua for each place in all, and the dialogs are shown
#            (#621, #650, #666)
#   columns  the icon bars in two columns, taller than the window, with the
#            scroll bars which are always shown: the scroller is beside the
#            last column, not over it (#683)
#   canvas   the surround of the pages (tm_background) follows the theme: 30,30,32
#            in the dark one, 160,160,160 in the light one, also in the
#            scroll views of the window, and it changes while TeXmacs runs
#            when the preference "gui theme" does
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
#   queries  the window queries of the View menu (header, icon bars, side
#            tools, status bar, prompt) to the widget of the window of the
#            console, and of the other windows: false, not a "type
#            mismatch" (#720)
#   menus    the menus computed by macOS without opening them (-[NSMenu
#            _simulateOpening:], Spotlight and Apple Intelligence; also
#            with a tracking notification and a menuDidClose: for each
#            menu, or within a tracking which has not ended, as macOS 26
#            may do, #792) do not fill the console with
#            their errors (Debug > Miscellaneous > Provoke menu error), and
#            that menu, really opened (a tracking), shows its error in the
#            console, as one error, not one for each line of its backtrace
#            (#720, #792)
#   console  the console opens at its end (a race before the fix: the end
#            waited for an editor with the keyboard focus, so that this one
#            failed now and then only) (#661)
#   tools    side tools made again let their former contents go: the
#            buffers of their texmacs-input without a name are closed (only
#            the present one is left), a named one stays when its tool is
#            closed (the drop command of the wrapped widgets, #776)
#   quiet    a warning from a delayed command opens the console, without
#            the focus: the document stays the key and main window and the
#            current buffer, and gets the keys typed. A click gives the
#            console the focus, and its close gives it back (#679, #619)
#   focused  the console which the user opens takes the focus, and its close
#            leaves the document focused (#679)
#
# Any case may take the keyboard from whoever works on that screen: macOS may
# make a process active when its parent, a Terminal for instance, is, and every
# window is shown (TEXMACS_NS_SNAPSHOT, which these cases do not use, also calls
# activateIgnoringOtherApps). The last two go further: they ask for it several
# times, for about 40 seconds (keys typed then go into the test document), so
# they run on GitHub Actions (GITHUB_ACTIONS=true) or with TM_COCOA_FOCUS=1, not
# with TM_COCOA_FOCUS=0, and are told to be skipped otherwise. Where they run,
# one in which TeXmacs says it is not active is skipped on the screen of a person
# (who may have taken the focus) and fails on GitHub Actions, where that would
# mean that the focus is never tested. Do not use the screen during this script.
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

# TM_COCOA_CASES="<case> <case>...": only those cases are run and counted
# (the runs and the verdicts of the others are skipped; the two -headless
# cases, colors and headless, still run, without a window). The names are
# those of the list above, except canvas, which is three cases:
# canvas-dark, canvas-light and canvas-live. A list which selects no case
# fails.
selected () {
  [ -z "${TM_COCOA_CASES:-}" ] && return 0
  case " $TM_COCOA_CASES " in *" $1 "*) return 0 ;; esac
  return 1
}

# run <case> <language> <window test steps> <scheme expression> [<scheme
# definitions>]: TeXmacs in a fresh home, the definitions at the start, the
# expression after 2.5 s, the quit after 14 s (or quit_after ms, for this run)
run () {
  selected "$1" || { quit_after=; return 0; }
  name=$1; lang=$2; steps=$3; x=$4; defs=${5:-}
  home="$homes/$name-home"
  rm -rf "${homes:?}/$name-home"
  mkdir -p "$home/system"
  prefs=""
  [ -n "$lang" ] && prefs="(\"language\" \"$lang\")"
  [ -n "${gui_theme:-}" ] && prefs="$prefs (\"gui theme\" \"$gui_theme\")"
  [ -n "${debug_menu:-}" ] && prefs="$prefs (\"debugging tool\" \"on\")"
  [ -n "$prefs" ] &&
    printf '(%s)\n' "$prefs" > "$home/system/preferences.scm"
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
  selected "$1" || { expected=''; return 0; }
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
# The window may come late on a slow runner (the first start of the job
# also loads the fonts and opens the Welcome first: run 41616292441 had no
# preferences window yet at the third step, one second later than usual), so
# the window test waits for it (at most 20 s, then the check fails as before)
# instead of looking at a fixed time; the quit is delayed accordingly.
title=$(printf "Pr\303\251f\303\251rences de l'utilisateur")
quit_after=30000
run title french "windows;wait:$title;windows;windows" "(open-preferences)"
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

# (colors) -headless: no window is shown, and TeXmacs is never the active
# application
home="$homes/colors-home"
rm -rf "${homes:?}/colors-home"
mkdir -p "$home"
printf '<TeXmacs|2.1.5>\n\n<\\body>\n  <with|color|nosuchcolor|some text>\n</body>\n' \
  > "$home/colors.tm"
HOME="$home" CFFIXED_USER_HOME="$home" TEXMACS_HOME_PATH="$home" \
  perl -e 'alarm shift; exec @ARGV' "$timeout" "$bin" -headless "$home/colors.tm" -x \
  "(begin (display* \"CHECK color \" (named-color->rgba \"nosuchcolor\") \"\\n\") (print-to-file \"$home/colors.pdf\") (display \"CHECK alive\\n\") (quit-TeXmacs))" \
  > "$out/colors.log" 2>&1
echo "exit status $?" >> "$out/colors.log"
colors_ok () {
  fail="not an opaque black: $(grep -a 'CHECK color' "$out/colors.log")"
  grep -aq '^CHECK color (0 0 0 255)' "$out/colors.log" || return 1
  fail="no PDF"
  [ -s "$home/colors.pdf" ] || return 1
  fail="a transparent color in the PDF (/ca 0)"
  ! grep -aqE '/ca 0([^.0-9]|$)' "$home/colors.pdf"
}
verdict colors "an unknown color name is an opaque black, on the screen and in a PDF (#656)" colors_ok

# (headless) -headless is a headless run: TeXmacs quits by itself after the
# command line, without (quit-TeXmacs) (the Cocoa build ignored the option,
# showed its windows and stayed until it was killed)
home="$homes/headless-home"
rm -rf "${homes:?}/headless-home"
mkdir -p "$home"
HOME="$home" CFFIXED_USER_HOME="$home" TEXMACS_HOME_PATH="$home" \
  perl -e 'alarm shift; exec @ARGV' "$timeout" "$bin" -headless -x \
  "(begin (display* \"CHECK headless \" (headless?) \"\\n\") (display \"CHECK alive\\n\"))" \
  > "$out/headless.log" 2>&1
echo "exit status $?" >> "$out/headless.log"
headless_ok () {
  fail="(headless?) is not true: $(grep -a 'CHECK headless' "$out/headless.log")"
  grep -aq '^CHECK headless #t' "$out/headless.log" || return 1
  fail="TeXmacs did not quit by itself: $(grep -a 'exit status' "$out/headless.log")"
  grep -aq '^exit status 0$' "$out/headless.log"
}
verdict headless "-headless: TeXmacs runs without a window and quits after the command line" headless_ok

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

# (columns) TEXMACS_NS_SCROLLERS: the setting of the system for this process
# only. In a low window the side bar widens by the scroller, which is at the
# right of the columns; in a high one there is no scroller and no extra room.
# Both in the same window: the Welcome of the new home may come in front after
# the first step (the steps follow the window of the last resize)
export TEXMACS_NS_BARS=left TEXMACS_NS_SCROLLERS=always
run columns "" "resize:800,300;side-bars;resize:800,1000;side-bars" '(display "")'
unset TEXMACS_NS_BARS TEXMACS_NS_SCROLLERS
columns_ok () {
  lines=$(grep -a 'NSTEST side-bars' "$out/columns.log")
  fail="no legacy scroller shown in the low window: $lines"
  low=$(echo "$lines" | sed -n 1p)
  case "$low" in *"style legacy"*"scroller shown"*) ;; *) return 1 ;; esac
  high=$(echo "$lines" | sed -n 2p)
  case "$high" in *"scroller hidden"*) ;; *) fail="a scroller in the high window: $lines"; return 1 ;; esac
  fail="the scroller is over the columns: $low"
  echo "$low" | awk '{ for (i = 1; i < NF; i++) { if ($i == "rows-right") r = $(i+1); if ($i == "shown") x = $(i+1) } exit !(r > 0 && x >= r) }' || return 1
  fail="the scroller takes no room: $lines"
  [ "$(echo "$low" | sed 's/.*pane \([0-9]*\)x.*/\1/')" -gt "$(echo "$high" | sed 's/.*pane \([0-9]*\)x.*/\1/')" ]
}
verdict columns "the scroller of the icon bars in columns is beside them (#683)" columns_ok
selected columns && grep -a 'NSTEST \(side-bars\|columns of\)' "$out/columns.log" | sed 's/^/         /'

# (canvas) the colour around the pages, in the scroll views of the main
# window too (the test aid "canvas" prints it): dark in the dark theme, mid
# grey in the light one, and at a change of the preference while running
gui_theme=dark
run canvas-dark "" "canvas;canvas" '(display "")'
gui_theme=light
run canvas-light "" "canvas;canvas" '(display "")'
gui_theme=light
run canvas-live "" 'canvas;eval:(set-preference "gui theme" "dark");canvas;canvas;eval:(set-preference "gui theme" "light");canvas;canvas' '(display "")'
gui_theme=
canvas_line () {  # <case> <n>: the n-th line of the test aid
  grep -a 'NSTEST canvas' "$out/$1.log" | sed -n "$2p"
}
canvas_is () {  # <case> <n> <r,g,b>: tm_background and the views follow it
  l=$(canvas_line "$1" "$2")
  fail="$1: $2th canvas line is '$l', not tm_background $3 on the scroll views"
  case "$l" in
    *"tm_background $3 scroll views: "[1-9]*" canvas "*" white 0 other") return 0 ;;
  esac
  return 1
}
canvas_dark_ok () { canvas_is canvas-dark 1 30,30,32 && canvas_is canvas-dark 2 30,30,32; }
canvas_light_ok () { canvas_is canvas-light 1 160,160,160 && canvas_is canvas-light 2 160,160,160; }
canvas_live_ok () {
  canvas_is canvas-live 1 160,160,160 && canvas_is canvas-live 3 30,30,32 &&
    canvas_is canvas-live 5 160,160,160
}
verdict canvas-dark "the surround of the pages is dark in the dark theme" canvas_dark_ok
verdict canvas-light "the surround of the pages is mid grey in the light theme" canvas_light_ok
verdict canvas-live "the surround of the pages follows a change of the theme while running" canvas_live_ok
selected canvas-dark && grep -ah 'NSTEST canvas' "$out"/canvas-*.log | sed 's/^/         /'

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

# (queries) the widgets of the windows of TeXmacs answer false to the queries
# of the View menu
run queries "" "queries;queries" "(open-error-messages)"
queries_ok () {
  fail="no answer for the console: $(grep -a "NSTEST queries" "$out/queries.log" | head -2 | tr '\n' ' ')"
  grep -aq "^NSTEST queries 'Error messages': 0 0 0 0 0 0 0 0 0 0 0$" "$out/queries.log" || return 1
  fail="a query failed: $(grep -a "NSTEST queries.*error" "$out/queries.log" | head -1)"
  ! grep -aq "NSTEST queries.*error" "$out/queries.log"
}
verdict queries "the window queries of the View menu answer false in every window (#720)" queries_ok

# (menus) a scan of the menus by macOS (two times, then two times with a
# tracking and a close for each menu, then two times while a tracking began
# and has not ended yet), then the menu of the deliberate error really
# opened. The number of the messages of the console is printed after
# each
errdefs='(tm-define (cocoa-check-errors tag) (display* "CHECK errors " tag " " (length (cdr (tree->stree (get-debug-messages "Error messages" 100)))) "\n"))'
debug_menu=1
# (12 steps, one a second from the fourth: more than the 14 s of the quit)
quit_after=22000
run menus "" "eval:(cocoa-check-errors 'before);scan;scan;scan:close;scan:close;begin-tracking;scan;scan;end-tracking;eval:(cocoa-check-errors 'scanned);track:Debug/Miscellaneous/Provoke menu error;eval:(cocoa-check-errors 'opened)" \
    "(open-error-messages)" "$errdefs"
debug_menu=
menus_ok () {
  # (what the notifications of tracking and the delegate calls did for the
  # scan and for the opening, for the log of the job)
  grep -a '^NSTEST stats' "$out/menus.log" | sed 's/^/         /'
  fail="no scan"
  grep -aq '^NSTEST scan done' "$out/menus.log" || return 1
  # (the helper defined by the -x of the run; a missing one only shows in
  # the log as an unbound variable)
  fail="the helper cocoa-check-errors was not defined: $(grep -a 'unbound variable cocoa-check-errors' "$out/menus.log" | head -1)"
  ! grep -aq 'unbound variable cocoa-check-errors' "$out/menus.log" || return 1
  count () { grep -a "^CHECK errors $1 " "$out/menus.log" | head -1 | cut -d' ' -f4; }
  before=$(count before); scanned=$(count scanned); opened=$(count opened)
  fail="no counts of the messages: '$before' '$scanned' '$opened'"
  [ -n "$before" ] && [ -n "$scanned" ] && [ -n "$opened" ] || return 1
  fail="the scan of the menus put errors in the console: $before before, $scanned after"
  [ "$scanned" = "$before" ] || return 1
  fail="the menu really opened did not show its error: $scanned after the scan, $opened after"
  [ "$opened" -gt "$scanned" ] || return 1
  # (the error, and the exception with its report: not one message for
  # each line of the backtrace, #792)
  fail="the error of the menu opened is more than 3 messages: $scanned after the scan, $opened after"
  [ "$opened" -le $((scanned + 3)) ] || return 1
  fail="a line of the backtrace is a message: $(grep -a 'failed-error, get_crash_report' "$out/menus.log" | head -1)"
  ! grep -aq 'failed-error, .*get_crash_report' "$out/menus.log" || return 1
  n=$(grep -ac 'caught while a menu was computed' "$out/menus.log")
  fail="a scan logs without bound: $n lines"
  [ "$n" -le 6 ]
}
expected='Throwing |caught while a menu was computed|caught in a menu|widget expected'
verdict menus "the errors of menus computed by macOS are not shown, those of menus opened are, as one error (#720, #792)" menus_ok

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

# (tools) as the case of tests/startup/check.sh for #776: the former
# widgets are let go at garbage collections, then close their buffers from
# the event queue (the drop command of ns_wrapped_widget_rep); the steps
# wait for that, with a bound
cat > "$out/tools.scm" <<'EOF'
(define (later f)
  (exec-delayed
    (lambda ()
      (catch #t f (lambda args (display* "CHECK error " args "\n"))))))
(define (inputs)
  (with prefix "tmfs://aux/TeXmacs-input-"
    (length (list-filter (buffer-list)
                         (lambda (u)
                           (string-starts? (url->string u) prefix))))))
(define (wait-until ok? n next)
  (lambda ()
    (gc)
    (if (or (ok?) (= n 0)) (next) (later (wait-until ok? (- n 1) next)))))
(define round 0)
(tm-tool (cocoa-side-tool win)
  (:name "cocoa side tool")
  (text (number->string round))
  (texmacs-input `(document ,(number->string round)) '(style "generic") #f))
(define named (string->url "tmfs://aux/cocoa-side-named"))
(tm-tool (cocoa-named-tool win)
  (:name "cocoa named tool")
  (texmacs-input '(document "named") '(style "generic") named)
  (texmacs-input '(document "sentinel") '(style "generic") #f))
(define (step)
  (set! round (+ round 1))
  (tool-select :right 'cocoa-side-tool)
  (update-menus)
  (later step2))
(define (step2)
  (update-forced)
  (if (< round 6)
      (later step)
      (later (wait-until (lambda () (= (inputs) 1)) 100 step3))))
(define (step3)
  (display* "CHECK buffers left " (inputs) "\n")
  (tool-select :right 'cocoa-named-tool)
  (update-menus)
  (later named-shown))
(define (named-shown)
  (update-forced)
  (set-window-tools (current-window) :right '())
  (update-menus)
  (later (wait-until (lambda () (= (inputs) 0)) 100 named-closed)))
(define (named-closed)
  (display* "CHECK sentinels left " (inputs) "\n")
  (display* "CHECK named buffer kept " (buffer-exists? named) "\n"))
(later step)
EOF
quit_after=30000
run tools "" "" "(load \"$out/tools.scm\")"
tools_ok () {
  got=$(grep -a '^CHECK \(buffers\|sentinels\|named\|error\)' "$out/tools.log" | tr '\n' ' ')
  fail="got: $got"
  [ "$got" = "CHECK buffers left 1 CHECK sentinels left 0 CHECK named buffer kept #t " ]
}
verdict tools "side tools made again close the buffers of their texmacs-input (#776)" tools_ok

# (quiet, focused) the document comes a few seconds after the start, in the
# window of the welcome message of a fresh home, and the console when TeXmacs
# has the focus (an editor without the focus has no idle time, which a console
# opening by itself waits for): the steps wait for them. The state is read
# after one explicit activate, whose effect is only the application: but it
# may give back a key window which was lost, so that the step just after the
# close of the console, without it, is checked when TeXmacs was active
cat > "$out/quiet.tm" <<'EOF'
<TeXmacs|2.1>

<style|generic>

<\body>
  Top
</body>
EOF
focus_defs='(tm-define (cocoa-check-state) (display* "CHECK current " (url-tail (current-buffer)) " " (tree->stree (buffer-get-body (current-buffer))) "\n"))'
focus_steps='wait:quiet.tm;front:quiet.tm;activate;windows'
state='eval:(cocoa-check-state)'
focus_quit='eval:(begin (display "CHECK alive\n") (quit-TeXmacs))'
on_actions=
[ "${GITHUB_ACTIONS:-}" = true ] && on_actions=1
case "${TM_COCOA_FOCUS:-}" in
  1) focus_cases=1 ;;
  0) focus_cases= ;;
  *) focus_cases=$on_actions ;;
esac

# focus_verdict <case> <description> <condition>: a case which crashed or did
# not reach its quit has failed, whatever else it says. Otherwise TeXmacs must
# have been active each time the case looked (the step after the close of the
# console is judged by the condition)
focus_verdict () {
  selected "$1" || return 0
  log="$out/$1.log"
  if [ -z "$focus_cases" ]; then
    echo "  skip   $2: set TM_COCOA_FOCUS=1"
  elif grep -aq '^CHECK alive' "$log" && grep -aq '^exit status 0$' "$log" &&
       sed '/NSTEST step close:/,/NSTEST step activate/d' "$log" |
         grep -aq '^NSTEST active no$'; then
    if [ -n "$on_actions" ]; then
      total=$((total + 1))
      failed=$((failed + 1))
      echo "  FAILED $2: TeXmacs did not become the active application on this runner"
    else
      echo "  skip   $2: TeXmacs is not the active application"
    fi
  else
    verdict "$@"
  fi
}

# close_ok <log> <the text which the document has at the end>: after the close
# of the console, the keys typed go to the document
close_ok () {
  # just after the close, before anything activates TeXmacs
  closing=$(sed -n '/NSTEST step close:/,/NSTEST step activate/p' "$1")
  if echo "$closing" | grep -aq '^NSTEST active yes$'; then
    fail="its close left no key window in the active TeXmacs: $(echo "$closing" | grep -a "^NSTEST window" | cut -c1-80 | tr '\n' ' ')"
    echo "$closing" | grep -aq "^NSTEST window 'quiet.tm' shown .*main key " || return 1
  fi
  closed=$(sed -n '/NSTEST step close:/,$p' "$1" | sed '1,/NSTEST step activate/d')
  fail="its close did not leave the document focused: $(echo "$closed" | grep -a "^NSTEST window\|^NSTEST type to\|^CHECK current" | cut -c1-80 | tr '\n' ' ')"
  ! echo "$closed" | grep -aq "^NSTEST window 'Error messages' shown" &&
    echo "$closed" | grep -aq "^NSTEST window 'quiet.tm' shown .*main key " &&
    echo "$closed" | grep -aq "^NSTEST type to 'quiet.tm', main 'quiet.tm'" &&
    echo "$closed" | grep -aq "^CHECK current <url quiet.tm> .*$2\""
}

warnings='(for-each (lambda (ch) (debug-message ch (string-append ch " line\n"))) (list "std-warning" "debug-io" "debug-automatic"))'
quit_after=50000
[ -n "$focus_cases" ] && run quiet "" "$focus_steps;eval:$warnings;activate;wait:Error messages;activate;windows;$state;activate;type:xy;$state;click:Error messages;windows;$state;close:Error messages;windows;activate;windows;$state;type:zw;$state;$focus_quit" \
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
  fail="the document did not stay the key and main window"
  echo "$opened" | sed -n '/NSTEST step wait:Error messages/,$p' |
    grep -aq "^NSTEST window 'quiet.tm' shown .*main key " || return 1
  fail="the keys did not go to the document: $(echo "$opened" | grep -a '^NSTEST type to')"
  echo "$opened" | grep -aq "^NSTEST type to 'quiet.tm', main 'quiet.tm'" || return 1
  fail="the document did not get the keys typed: $(echo "$opened" | grep -a '^CHECK current' | tail -1 | cut -c1-80)"
  echo "$opened" | grep -aq '^CHECK current <url quiet.tm> .*xyTop"' || return 1
  clicked=$(sed -n '/NSTEST step click:/,/NSTEST step close:/p' "$log")
  fail="a click did not give the console the focus: $(echo "$clicked" | grep -a "^NSTEST window 'Error\|^CHECK current" | cut -c1-80 | tr '\n' ' ')"
  echo "$clicked" | grep -aq "^NSTEST window 'Error messages' shown .*main key " &&
    echo "$clicked" | grep -aq '^CHECK current <url console-' || return 1
  close_ok "$log" xyzwTop
}
focus_verdict quiet "a console which opens by itself leaves the focus to the document (#679)" quiet_ok

quit_after=50000
[ -n "$focus_cases" ] && run focused "" "$focus_steps;eval:(open-error-messages);wait:Error messages;activate;windows;type:a;$state;close:Error messages;windows;activate;windows;$state;type:zw;$state;$focus_quit" \
    "(load-buffer \"$out/quiet.tm\")" "$focus_defs"
focused_ok () {
  log="$out/focused.log"
  opened=$(sed -n '1,/NSTEST step close:/p' "$log")
  fail="the console opened by the user did not take the focus: $(echo "$opened" | grep -a "^NSTEST window 'Error\|^NSTEST type to\|^CHECK current" | cut -c1-80 | tr '\n' ' ')"
  echo "$opened" | grep -aq "^NSTEST window 'Error messages' shown .*main key " &&
    echo "$opened" | grep -aq "^NSTEST type to 'Error messages', main 'Error messages'" &&
    echo "$opened" | grep -aq '^CHECK current <url console-' || return 1
  close_ok "$log" zwTop
}
focus_verdict focused "a console opened by the user takes the focus (#679)" focused_ok

echo "$failed of $total failed"
if [ "$total" = 0 ]; then
  echo "no case was selected by TM_COCOA_CASES='${TM_COCOA_CASES:-}'"
  exit 1
fi
[ "$failed" = 0 ]
