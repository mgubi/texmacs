<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Editing modes on the <scheme> side>

  <section|Introduction>

  Almost all of the interactive behaviour of <TeXmacs> is written in
  <scheme>. When the user presses <key|return>, the <c++> editor only finds
  out which command is bound to the key; the command itself, and the decision
  of what <key|return> means at the current position (a new paragraph, a new
  item of a list, a new row of a table, the evaluation of a session input,
  ...), are taken by <scheme> code which depends on the <em|mode> (text,
  mathematics, source code, ...) and on the tags around the cursor.

  This chapter describes how this code is organized: how modes are defined
  and tested, how functions, keyboard shortcuts and menus are specialized for
  a mode or a context, which generic hooks the editing modes implement, and
  what the main modes provide: text, mathematics, tables and the dynamic
  markup (folding, switches, slides, sessions, scripts, spreadsheets and
  animations).

  The chapter builds on the following documents, which it does not repeat:

  <\itemize>
    <item><hlink|contextual overloading|../scheme/overview/overview-overloading.en.tm>
    and <hlink|the module system and lazy
    definitions|../scheme/overview/overview-lazyness.en.tm>, for the
    principles of <scm|tm-define> and of the lazy loading of modules;

    <item><hlink|the <TeXmacs> editing model|../scheme/edit/edit-model.en.tm>
    and the other chapters on <hlink|programming routines for
    editing|../scheme/edit/scheme-edit.en.tm>, for the <scheme> interface
    to trees, paths and the cursor, and for a first example of a structured
    editing routine;

    <item><hlink|the DRD from Scheme|drd-scheme.en.tm>, for tag groups
    (<scm|define-group>) and for the <abbr|DRD> queries used by the
    context predicates;

    <item><hlink|the event loop|server-events.en.tm>, for
    the way the <c++> editor receives keys and rebuilds menus;

    <item><hlink|sessions, connections and links|plugin-machinery.en.tm>, for the
    communication with the plug-ins behind sessions, scripts and
    spreadsheets.
  </itemize>

  <section|Overview>

  An editing mode is a set of <scheme> modules under
  <verbatim|$TEXMACS_PATH/progs/<em|mode>/> which extend generic
  definitions. Three mechanisms make the extension possible:

  <\description>
    <item*|Modes>A mode is a named predicate, such as <scm|in-math?>,
    declared with <scm|texmacs-modes> in
    <source-link|kernel/texmacs/tm-modes.scm|TeXmacs/progs/kernel/texmacs/tm-modes.scm>. Modes form a hierarchy:
    <scm|in-math-in-session?> is a sub-mode of both <scm|in-math?> and
    <scm|in-session?>.

    <item*|Conditional definitions>Functions (<scm|tm-define>), keyboard
    shortcuts (<scm|kbd-map>) and menus (<scm|menu-bind>, <scm|tm-menu>) may
    be redefined under a condition, either a mode (<scm|:mode>) or an
    arbitrary predicate (<scm|:require>). The most recent definition whose
    condition holds wins.

    <item*|Generic hooks>Generic code calls a small set of functions, such
    as <scm|(kbd-enter <scm-arg|t> <scm-arg|shift?>)> or
    <scm|(structured-insert-horizontal <scm-arg|t> <scm-arg|forwards?>)>,
    on the <em|focus tree> <scm-arg|t>; their default implementations
    recurse to the parent tree. A mode implements the behaviour of its tags
    by redefining the hooks for those tags.
  </description>

  For instance, when the user presses <key|return> inside an
  <markup|itemize> list, the key is bound (in
  <source-link|generic/generic-kbd.scm|TeXmacs/progs/generic/generic-kbd.scm>) to <scm|(kbd-return)>, which calls
  <scm|(kbd-enter (focus-tree) #f)>. The focus tree is the innermost tag
  around the cursor, here the <markup|itemize> tag. Among all definitions
  of <scm|kbd-enter>, the one in <source-link|text/text-edit.scm|TeXmacs/progs/text/text-edit.scm> with the
  condition <scm|(list-context? t)> applies, and inserts a new item. Inside
  a table which is itself inside the list, the table definition in
  <source-link|table/table-edit.scm|TeXmacs/progs/table/table-edit.scm> applies first, because the focus tree is
  then the table.

  <section|Source files>

  The relevant directories (relative to <verbatim|$TEXMACS_PATH/progs/>)
  are:

  <\description>
    <item*|<source-link|kernel/texmacs/tm-modes.scm|TeXmacs/progs/kernel/texmacs/tm-modes.scm>>The macro
    <scm|texmacs-modes>, the standard modes, the sub-mode test and
    <scm|lazy-initialize>.

    <item*|<source-link|kernel/texmacs/tm-define.scm|TeXmacs/progs/kernel/texmacs/tm-define.scm>>Conditional definitions:
    <scm|tm-define>, <scm|tm-property> and their options.

    <item*|<source-link|kernel/gui/kbd-define.scm|TeXmacs/progs/kernel/gui/kbd-define.scm>>The keyboard tables,
    <scm|kbd-map>, <scm|kbd-wildcards> and <scm|lazy-keyboard>.

    <item*|<source-link|kernel/gui/menu-define.scm|TeXmacs/progs/kernel/gui/menu-define.scm>>Menus: <scm|menu-bind>,
    <scm|tm-menu>, <scm|lazy-menu>.

    <item*|<source-link|generic/|TeXmacs/progs/generic>>Behaviour common to all modes: the generic
    hooks (<source-link|generic-edit.scm|TeXmacs/progs/generic/generic-edit.scm>), the basic keyboard
    (<source-link|generic-kbd.scm|TeXmacs/progs/generic/generic-kbd.scm>), the focus menus and focus icon bar
    (<source-link|generic-menu.scm|TeXmacs/progs/generic/generic-menu.scm>) and the format, insert and document
    menus.

    <item*|<verbatim|text/>>Text mode: document titles, sections, lists,
    enunciations, floats and the language specific keyboards
    (<source-link|text/chinese/|TeXmacs/progs/text/chinese>, <source-link|text/cyrillic/|TeXmacs/progs/text/cyrillic>, ...).

    <item*|<verbatim|math/>>Mathematics: the large mathematical keyboard,
    brackets, scripts, equations and the semantic editing mode.

    <item*|<verbatim|table/>>Tables and cells.

    <item*|<source-link|dynamic/|TeXmacs/progs/dynamic>>Folding, switches, overlays and slides;
    sessions and programs; scripts, plots and converters; spreadsheets;
    animations.

    <item*|<source-link|texmacs/keyboard/|TeXmacs/progs/texmacs/keyboard>>Keyboard prefixes and wildcards
    common to all modes (<source-link|prefix-kbd.scm|TeXmacs/progs/texmacs/keyboard/prefix-kbd.scm>) and the
    <LaTeX> style shortcuts (<source-link|latex-kbd.scm|TeXmacs/progs/texmacs/keyboard/latex-kbd.scm>).

    <item*|<source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>>The lazy declarations which tell
    when each mode module is loaded.
  </description>

  Other modes follow the same pattern and are not described here, notably
  the source mode (<verbatim|source/>), the programming languages
  (<source-link|prog/|TeXmacs/progs/prog>, see <hlink|syntax highlighting and programming
  languages|syntax-highlighting.en.tm>), graphics (<verbatim|graphics/>,
  see <hlink|the graphics editor|graphics-editor.en.tm>) and the
  bibliographic database (<verbatim|database/>, see <hlink|the database and
  bibliographies|database.en.tm>).

  <section|Contents of this chapter>

  <\traverse>
    <branch|Modes, conditional definitions and lazy
    loading|modes-dispatch.en.tm>

    <branch|The generic structured editing hooks|modes-structured.en.tm>

    <branch|Text and mathematics|modes-text-math.en.tm>

    <branch|Tables and dynamic markup|modes-tables-dynamic.en.tm>
  </traverse>

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
