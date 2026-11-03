<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The features of program code: syntax trees and language
  servers>

  Two features know program code: <hlink|tree-sitter|treesit.en.tm> (the
  syntax of a piece of code, at once, in source files, code blocks and
  sessions) and the <hlink|language servers|lsp.en.tm> (the meaning of the
  code, over a project, after a while, in source files and, when the user
  asks for it, in the code blocks of documents). Either may be left out of a build
  (<verbatim|--disable-tree-sitter>, <verbatim|--disable-lsp>, in
  <source-link|tm_code.m4|misc/m4/tm_code.m4>; <verbatim|USE_TREE_SITTER>,
  <verbatim|USE_LSP> in CMake). What both use is in the core, which names
  neither: the features register themselves. The design, its phases and
  the comparison of the two are in <source-link|src/docs/code-features.md|docs/code-features.md>.

  <section|The lines of program code>

  <source-link|code_lines.cpp|src/System/Language/code_lines.cpp> keeps the
  state of a line which is not in the document on the highlight observers
  of the line, which an edit of the line detaches (so that an edited line
  forgets it):

  <\itemize>
    <item>its folds: the number of the folds which hide it
    (<cpp|code_line_fold_state>, <scm|code-line-fold-state>); the bridge
    of the line does not show a hidden line, but keeps its boxes for the
    cursor (<cpp|bridge_rep::fold_line>), except when the document is
    printed;

    <item>its layers: colours of pieces of the line and hints before some
    of its characters (<cpp|code_set_line_layers>,
    <scm|code-set-line-layers>), which <cpp|typeset_layered_string>
    (<source-link|concat_text.cpp|src/Typeset/Concat/concat_text.cpp>)
    draws over the colours of the language of the line;

    <item>positions: the column of an offset, a tab <verbatim|\<less\>#9\<gtr\>>
    going to its stop (<cpp|code_column>), the UTF-16 units of the servers
    and back (<cpp|code_utf16_column>, <cpp|code_utf16_offset>), the end of
    the indentation (<cpp|code_indent_end>).
  </itemize>

  <source-link|code_regions.cpp|src/System/Language/code_regions.cpp> gives
  the changes of a region (a document of lines): the common first and last
  lines of two versions (<cpp|code_lines_common>, which the incremental
  parse of tree-sitter uses), and snapshots of the lines as a file has
  them, which give the change since them as a range of positions and its
  text (<scm|code-snapshot-new>, <scm|code-snapshot-change>), by which the
  language servers get the changes of a buffer.

  A feature which is not in all builds registers itself in the core:
  tree-sitter wraps the program languages (<cpp|set_prog_language_wrapper>,
  <source-link|language.hpp|src/System/Language/language.hpp>), the
  language servers process their messages at each turn of the loop of the
  server (<cpp|add_link_processor>,
  <source-link|tm_link.hpp|src/System/Link/tm_link.hpp>). Their glue is in
  <source-link|build-glue-treesit.scm|src/Scheme/Glue/build-glue-treesit.scm>
  and <source-link|build-glue-lsp.scm|src/Scheme/Glue/build-glue-lsp.scm>,
  compiled only with them (<source-link|glue.cpp|src/Scheme/Scheme/glue.cpp>).

  <section|The features and their providers>

  <source-link|code-features.scm|TeXmacs/progs/code/code-features.scm> has
  the table of the features of program code and of their providers
  (<scm|code-provider-set!>: a feature, a name, a rank, whether it is
  available at the cursor, a procedure). <scm|code-ask> asks the providers
  of a feature by rank, the next one when one answers <scm|#f>; the
  preference <verbatim|code:\<less\>feature\<gtr\>> chooses one. The
  providers are registered by the adapters
  <source-link|ts-code.scm|TeXmacs/progs/treesit/ts-code.scm> (rank 20,
  <scm|'syntax>) and <source-link|lsp-code.scm|TeXmacs/progs/lsp/lsp-code.scm>
  (rank 10, <scm|'server>), which are loaded when their feature is part of
  the build (its glue). Without tree-sitter, its functions which the
  editing of programs calls answer that the code has no syntax tree
  (<source-link|code-syntax-none.scm|TeXmacs/progs/code/code-syntax-none.scm>):
  the modules which call them load
  <source-link|code-syntax.scm|TeXmacs/progs/code/code-syntax.scm>, which
  loads <source-link|ts-edit.scm|TeXmacs/progs/treesit/ts-edit.scm> or those stubs. <scm|code-ask> goes to the
  next provider when one answers <scm|#f> or <scm|'()>: a server which
  has no block at the cursor lets tree-sitter fold.

  <\description>
    <item*|<source-link|code-fold.scm|TeXmacs/progs/code/code-fold.scm>>The
    folds (std ( and std )): the blocks (<scm|'fold-ranges>,
    <scm|'fold-all-ranges>) from the server when one follows the file,
    else from the syntax tree; the cursor (<scm|code-cursor-in-fold>,
    called by <source-link|edit_cursor.cpp|src/Edit/Interface/edit_cursor.cpp>
    with the last place of the cursor of its editor outside of the folds):
    the arrows go over a fold, a jump opens the folds around, from the
    innermost. A fold nests in another one (at most 9 deep: the states of
    the lines); the first line of a fold ends with a mark, which the bridge
    of the line adds when the next line is hidden.

    <item*|<source-link|code-navigate.scm|TeXmacs/progs/code/code-navigate.scm>>The
    outline (<scm|'outline>, std O, a page which follows the buffer) and the
    definitions (<scm|'definition>, F12).

    <item*|<source-link|code-diagnostics.scm|TeXmacs/progs/code/code-diagnostics.scm>>The
    diagnostics of a buffer by source (the server; the syntax errors of
    tree-sitter, <scm|'syntax-errors>, where no server follows the code),
    underlined (the alternative selections <verbatim|code-errors>,
    <verbatim|code-warnings>, <verbatim|code-infos>) and visited by F8.

    <item*|<source-link|code-complete.scm|TeXmacs/progs/code/code-complete.scm>>Where
    the cursor is (<scm|'completion-context>: no completion of a server in
    a string or a comment) and the names of the code (<scm|'local-names>,
    which tab completes where no server follows the code).

    <item*|<source-link|code-menu.scm|TeXmacs/progs/code/code-menu.scm>>The
    menu <menu|Focus> of program code: the entries of the features which a
    provider serves at the cursor, the diagnostics, then those which only a
    server gives (<scm|code-server-menu>, from <verbatim|lsp-code.scm>) and
    those of the syntax tree (<scm|code-syntax-menu>, from
    <verbatim|ts-edit.scm>), and those of a session or a program input.
  </description>

  <section|The code blocks of documents>

  The language servers see the code blocks of a document (when the
  preference <verbatim|lsp:code blocks> is on) as files which are never
  written, each block one file, or the blocks of a language together (a
  unit, the default):
  <source-link|lsp-blocks.scm|TeXmacs/progs/lsp/lsp-blocks.scm>.
  The functions of the buffer of the servers, called without a buffer,
  are those of the <em|target>, the code at the cursor: the lines of a
  source file, of the block or of its unit (<scm|lsp-target-root>, and
  <scm|lsp-target-path>, <scm|lsp-target-lo>, between the lines and the
  places of the document); the features of program code are given at the
  cursor in the same way in all. The diagnostics of
  all the blocks of a document are those of the source <scm|'server> of
  <source-link|code-diagnostics.scm|TeXmacs/progs/code/code-diagnostics.scm>, each at the place of its block.

  <section|The tokens of the typesetting>

  A line of program code is typeset as tokens (<cpp|typeset_prog_string>,
  <source-link|concat_text.cpp|src/Typeset/Concat/concat_text.cpp>), which
  the language of the line gives (<cpp|advance>, <cpp|get_color>). Where
  tree-sitter colours a line, they are the leaves of its syntax tree, cut
  at the spaces (<hlink|highlighting|treesit-highlight.en.tm>); elsewhere,
  those of the language of <TeXmacs> (<hlink|syntax
  highlighting|syntax-highlighting.en.tm>). The layers of the servers are
  drawn over either.

  The suite <verbatim|code>
  (<source-link|code-test.scm|TeXmacs/progs/check/code-test.scm>) tests the
  core with providers of its own, in every build; the suites
  <verbatim|treesit> and <verbatim|lsp> run when their feature is there.

  <\traverse>
    <branch|Adding a feature or a provider|code-features-howto.en.tm>

    <branch|Pitfalls of the features of program
    code|code-features-pitfalls.en.tm>
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
