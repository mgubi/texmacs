<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Adding a feature or a provider of program code>

  A feature of program code is a question which a provider answers for the
  code at the cursor: which blocks fold here, what is the outline, where is
  the definition of this name. The core asks it
  (<source-link|code-features.scm|TeXmacs/progs/code/code-features.scm>), a
  command of <source-link|progs/code|TeXmacs/progs/code> uses
  the answer, and tree-sitter and the language servers give it from their
  adapters. Neither the command nor the core knows which provider answers,
  nor whether a provider is part of the build.

  <section|The features and their answers>

  A provider is a procedure of no argument, called with the cursor where
  the feature is asked for, and a predicate of no argument, which tells
  whether it can answer there (cheap: it is called for the menus). Its
  answer, or <scm|#f> (and <scm|'()>) when it has none, which lets the next
  provider answer:

  <\description>
    <item*|<scm|'fold-ranges>>The blocks around the cursor which may fold,
    as a list of <scm|(first last)> (rows of the lines of the code).

    <item*|<scm|'fold-all-ranges>>All the blocks which <menu|Fold all
    blocks> folds, as above.

    <item*|<scm|'outline>>The entries <scm|(level kind name detail path)>
    of the code, in their order: <scm|path> is where the cursor goes, the
    names are in Cork.

    <item*|<scm|'definition>>Goes to the definition of the name at the
    cursor; <scm|#t> when it did.

    <item*|<scm|'syntax-errors>>The items <scm|(severity start end
    message)> of the code at the cursor (<scm|severity>: <scm|'error>,
    <scm|'warning> or <scm|'info>; <scm|start>, <scm|end>: paths).

    <item*|<scm|'server-diagnostics>>No answer is used: a provider which
    is available means that a server gives the diagnostics of the code,
    and the syntax errors are not shown there.

    <item*|<scm|'completion-context>>Where the cursor is: <scm|'code>,
    <scm|'string> or <scm|'comment>.

    <item*|<scm|'local-names>>The names of the code (its definitions),
    which <key|tab> completes where no server follows the code.
  </description>

  <section|A provider>

  <\verbatim-code>
    (code-provider-set! 'outline 'syntax 20

    \ \ (lambda () (and (syntax-cursor-language) #t))

    \ \ syntax-outline-at-cursor)
  </verbatim-code>

  gives the feature <scm|'outline> a provider named <scm|'syntax> of rank
  20: the providers are asked by rank, the lowest first (the servers have
  10, tree-sitter 20, because a server sees the other files of a project).
  The preference <verbatim|code:outline> chooses one by its name instead
  (<verbatim|auto>, the default, by rank; <verbatim|off>, none).
  <scm|code-provider-remove!> takes a provider away, and
  <scm|code-available-providers> tells which ones can answer at the cursor.

  The providers of a feature which may be left out of a build are set in
  its adapter (<source-link|ts-code.scm|TeXmacs/progs/treesit/ts-code.scm>,
  <source-link|lsp-code.scm|TeXmacs/progs/lsp/lsp-code.scm>). A new
  adapter goes into the list <scm|code-adapters> of
  <source-link|code-features.scm|TeXmacs/progs/code/code-features.scm>, with a glue routine which only a build
  with the feature has: the adapter is loaded the first time a feature is
  asked for, if that routine is defined and its file is there. The adapter
  may use the modules of its feature directly; nothing else may (a module
  which needs the functions of the syntax tree loads
  <source-link|code-syntax.scm|TeXmacs/progs/code/code-syntax.scm>).

  <section|A feature>

  <\enumerate>
    <item>The command, in a module of <verbatim|progs/code>, asks the
    feature with <scm|code-ask> and says so when no provider answers
    (<scm|set-message>); <scm|code-available?> tells whether it can be
    offered at the cursor.

    <item>The command is defined lazily in
    <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>
    (<scm|lazy-define>), and has an entry in <scm|focus-extra-menu> of
    <source-link|code-menu.scm|TeXmacs/progs/code/code-menu.scm> and a key
    in <source-link|prog-kbd.scm|TeXmacs/progs/prog/prog-kbd.scm>, if it
    is for all code.

    <item>The providers which can answer register themselves in their
    adapters.

    <item>The suite <verbatim|code>
    (<source-link|code-test.scm|TeXmacs/progs/check/code-test.scm>) tests
    the command with providers of its own (names other than those of the
    adapters, ranks below theirs), which it removes at the end: it runs in
    every build. It ignores the preferences of the user
    (<scm|code-ignore-preferences!>); a test never writes them.

    <item><verbatim|doc/code-features.md> and the pages of this chapter
    say what the feature is, and the manual what the user sees
    (<hlink|editing program code|../../main/editing/man-program-code.en.tm>).
  </enumerate>

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
