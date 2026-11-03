<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Keyboard bindings>

  Keyboard shortcuts are defined using the <scm|kbd-map> macro, which is
  implemented in <source-link|kernel/gui/kbd-define.scm|TeXmacs/progs/kernel/gui/kbd-define.scm>. The standard
  bindings can be found in the files <verbatim|*-kbd.scm>, such as
  <source-link|generic/generic-kbd.scm|TeXmacs/progs/generic/generic-kbd.scm>, <source-link|math/math-kbd.scm|TeXmacs/progs/math/math-kbd.scm> or
  <source-link|text/text-kbd.scm|TeXmacs/progs/text/text-kbd.scm> (relative to <source-link|src/TeXmacs/progs/|TeXmacs/progs>).
  See also the user manual section on <hlink|creating your own keyboard
  shortcuts|../../../main/scheme/man-custom-keyboard.en.tm>. The way
  keyboard events reach the editor is described in <hlink|the
  event loop|../../source/server-events.en.tm>.

  <\explain>
    <scm|(kbd-map <scm-arg|option> ... <scm-arg|binding>
    ...)><explain-synopsis|define keyboard shortcuts>
  <|explain>
    Each <scm-arg|binding> is of one of the following forms:

    <\description>
      <item*|<scm|(<scm-arg|keys> <scm-arg|string>)>>Typing <scm-arg|keys>
      inserts the <scm-arg|string>, which may contain symbols such as
      <verbatim|\<less\>alpha\<gtr\>>. An optional third element gives a
      help text.

      <item*|<scm|(<scm-arg|keys> <scm-arg|expr> ...)>>Typing
      <scm-arg|keys> evaluates the <scheme> expressions <scm-arg|expr>.
    </description>

    Here <scm-arg|keys> is a string with a space-separated sequence of
    keystrokes, such as <verbatim|"C-x C-s"> or <verbatim|"a var">.
    Modifiers are written as prefixes <verbatim|S-> (shift), <verbatim|C->
    (control), <verbatim|A-> (alt) and <verbatim|M-> (meta, which is the
    command key under <name|macOS>). Logical prefixes such as
    <verbatim|std>, <verbatim|cmd>, <verbatim|altcmd>, <verbatim|special>
    or <verbatim|structured:move> are rewritten into physical modifiers
    according to the current look and feel (see
    <source-link|texmacs/keyboard/prefix-kbd.scm|TeXmacs/progs/texmacs/keyboard/prefix-kbd.scm>); they should be preferred in
    portable key bindings. The key <verbatim|var> (usually the tab key) is
    used to cycle through variants.

    The bindings may be preceded by the following options:

    <\description>
      <item*|<scm|(:mode <scm-arg|mode?>)>>The bindings are only valid in
      the given mode, such as <scm|in-math?> (see <hlink|contextual
      overloading|utils-overload.en.tm>). The mode may also be given alone,
      as in <scm|(kbd-map in-math? ...)>. As for functions, the most recent
      binding of a key whose conditions hold is used.

      <item*|<scm|(:require <scm-arg|cond>)>>The bindings are only valid
      when the expression <scm-arg|cond> evaluates to true.

      <item*|<scm|(:profile <scm-arg|look-and-feel> ...)>>The bindings are
      only defined for the given look and feels (such as <scm|emacs>,
      <scm|gnome>, <scm|kde>, <scm|windows> or <scm|macos>). The profile
      <scm|std> stands for all look and feels except <scm|emacs>, and a
      prefix <scm|no-> negates a profile (<abbr|e.g.> <scm|no-macos>).
    </description>

    For instance:

    <\scm-code>
      (kbd-map

      \ \ (:mode in-math?)

      \ \ ("std F5" (make 'sqrt))

      \ \ ("\<less\> = var" "\<less\>leqslant\<gtr\>"))
    </scm-code>
  </explain>

  <\explain>
    <scm|(kbd-unmap <scm-arg|option> ... <scm-arg|keys> ...)><explain-synopsis|remove
    keyboard shortcuts>
  <|explain>
    Remove the bindings for the given key sequences, under the same
    conditions as in <scm|kbd-map>.
  </explain>

  <\explain>
    <scm|(kbd-wildcards <scm-arg|which> (<scm-arg|from> <scm-arg|to>)
    ...)><explain-synopsis|keyboard prefixes>
  <|explain>
    Declare that the key prefix <scm-arg|from> should be rewritten into
    <scm-arg|to>. The optional <scm-arg|which> is <scm|pre> (the rewriting
    is applied when the key bindings are defined) or <scm|post> (the
    default: the rewriting is applied to the actual keystrokes).
  </explain>

  <\explain>
    <scm|(kbd-commands (<scm-arg|name> <scm-arg|help> <scm-arg|expr>)
    ...)>

    <scm|(kbd-symbols <scm-arg|name> ...)><explain-synopsis|backslashed
    commands>
  <|explain>
    Define commands which can be entered by typing a backslash followed by
    <scm-arg|name>. The macro <scm|kbd-symbols> defines such commands for
    inserting the symbols <verbatim|\<less\><scm-arg|name>\<gtr\>>.
  </explain>

  Besides key bindings, many keys call overloadable routines, such as
  <scm|kbd-enter>, <scm|kbd-tab>, <scm|kbd-remove>, <scm|kbd-horizontal> or
  <scm|kbd-vertical> (see <source-link|generic/generic-edit.scm|TeXmacs/progs/generic/generic-edit.scm>). The
  behaviour of these keys inside specific tags can be customized by
  redefining these routines using <scm|tm-define> with a <scm|:require>
  option.

  <tmdoc-copyright|2005--2026|Joris van der Hoeven, the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
