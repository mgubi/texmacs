<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Editing source>

  <section|Creating tags>

  <cpp|make_compound (l, n)> (<source-link|edit_dynamic.cpp:94|src/Edit/Modify/edit_dynamic.cpp:94>),
  the <c++> side of <scm|make>, inserts a tag with label <cpp|l>. Without
  an explicit arity it takes the smallest arity which the DRD accepts. It
  wraps the selection into the tag when appropriate, starts block macros
  with a <markup|document>, and puts the cursor in the first accessible
  argument. If some argument of the tag is not accessible, the tag is
  inserted <em|inactive>, unless the document is in source mode (its
  initial <verbatim|mode> is <verbatim|src>) or the cursor is in a
  <markup|show-preamble>, so that its arguments can be filled in, and the
  footer says that <key|return> activates it.

  <cpp|activate> (<source-link|edit_dynamic.cpp:166|src/Edit/Modify/edit_dynamic.cpp:166>)
  removes the innermost <markup|inactive> around the cursor. An inactive
  <markup|compound> whose first argument is a name is turned into the tag
  of that name. The <scheme> hook <scm|notify-activated> is called on the
  result. <cpp|insert_argument> and <cpp|remove_argument> add or remove
  arguments of tags with a variable arity; in source mode, <key|tab> after
  an argument inserts a new one (<scm|kbd-variant> in <source-link|source-edit.scm|TeXmacs/progs/source/source-edit.scm>).

  <section|Entering a tag by name>

  <key|\\> calls <cpp|make_hybrid> (<source-link|edit_dynamic.cpp:521|src/Edit/Modify/edit_dynamic.cpp:521>),
  which inserts a <markup|hybrid> tag (inactive outside source mode) and
  waits for a name; a small selection becomes its second argument, or its
  name if it is the name of a known tag.
  <key|return> calls <cpp|activate_hybrid> (<source-link|edit_dynamic.cpp:559|src/Edit/Modify/edit_dynamic.cpp:559>),
  which tries in turn:

  <\enumerate>
    <item>a named command (<cpp|activate_latex>: <cpp|kbd_get_command>,
    the table of <scm|kbd-get-command>), which is executed; this is how
    <verbatim|\\alpha> inserts <math|\<alpha\>>;

    <item>an argument of the macro being edited (inside a <markup|macro> or
    the macro editor), which becomes <markup|arg>;

    <item>a primitive or a macro, for which a new tag is made with
    <cpp|make_compound>;

    <item>an environment variable, which becomes <markup|value>;

    <item>in source mode only, any other name, which becomes a new tag (with
    one argument if <key|tab> was used instead of <key|return>).
  </enumerate>

  Otherwise the footer says <em|unknown command> and the <markup|hybrid>
  tag remains. <markup|symbol> tags work the same way for symbols:
  <cpp|activate_symbol> replaces <verbatim|\<less\>symbol\|alpha\<gtr\>> by
  the symbol <verbatim|\<less\>alpha\<gtr\>>, and a number by the character
  with that code.

  <section|Keyboard and menus>

  <source-link|source-kbd.scm|TeXmacs/progs/source/source-kbd.scm> binds
  the source shortcuts (activation, deactivation, compactness) and
  <source-link|source-edit.scm|TeXmacs/progs/source/source-edit.scm>
  overloads the structured commands for source: <scm|kbd-enter> activates
  <markup|hybrid>, <markup|compound>, <markup|latex>, <markup|symbol> and
  <markup|inactive> tags, <scm|kbd-variant> inserts arguments, and
  <scm|inactive-toggle> switches a tag between active and inactive. The
  <menu|Source> menu (<source-link|source-menu.scm|TeXmacs/progs/source/source-menu.scm>)
  appears in source mode, or everywhere with <menu|Tools|Source macros tool>; it lists the
  primitives of the style language by group (definitions, macros, flow
  control, arithmetic, ...), the activation and presentation commands, and
  the macro commands of the next page.

  <menu|Source|Extract style file> and <menu|Source|Extract style package>
  (<scm|extract-style-file>) build a new <tmstyle|source> buffer from the
  current document: a title, the <markup|use-package> of its style, its
  initial environment as <markup|assign>s (minus a few view settings such
  as the zoom), and the macro definitions of its preamble.

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
