<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Syntax trees of program code: tree-sitter>

  Tree-sitter is an incremental parser: from a grammar it makes a parser
  which gives a concrete syntax tree of a text, updates it after an edit by
  reusing the unchanged parts, and recovers from syntax errors (the tree
  always exists, with <verbatim|ERROR> and <verbatim|MISSING> nodes).
  <TeXmacs> keeps such a tree for every region of program code it
  typesets: a source file, a code block, the input of a session.

  For the user, the tree gives the colours of the code (a string or a
  comment over several lines is coloured as such, the scripts of an
  <abbr|HTML> page in their own language, brackets which match nothing in
  the colour of errors) and the editing of programs: balanced brackets and
  quotes, closing keywords, indentation, structured moves and selections,
  structural edits, the outline and the definitions of a file, folds, and
  Return in a session input which is not complete yet. The user level
  description is <hlink|editing program
  code|../../main/editing/man-program-code.en.tm>. For the developer, the
  tree is a service of the <c++> kernel which <scheme> asks through glue
  routines, with positions which are positions in the <TeXmacs> strings of
  the lines.

  <section|The two halves>

  A grammar of tree-sitter is data: its <verbatim|grammar.json>, which
  <verbatim|tree-sitter generate> writes from <verbatim|grammar.js> and
  which every grammar repository commits. Tree-sitter itself turns it into
  a <tt|parser.c> of several megabytes per language. <TeXmacs> does
  not: it ships the <verbatim|grammar.json> of each language and makes the
  tables itself.

  <\description>
    <item*|The generator>A port to <scheme> of the generator of
    tree-sitter 0.27.1 (<source-link|progs/treesit|TeXmacs/progs/treesit>)
    reads the grammar, builds its tokens and its <abbr|LR(1)> parse table
    with the conflict resolution of tree-sitter, merges the states and
    makes the lex tables. The stages which act on the states (about 100000
    of them for C++ before merging) are also written in <c++>
    (<source-link|ts_tables.cpp|src/Plugins/TreeSitter/ts_tables.cpp>),
    which gives the same tables byte for byte and is used when present.
    The result is a text, cached per grammar.

    <item*|The runtime>The C runtime of tree-sitter 0.27.1 (MIT), vendored
    in <source-link|tree-sitter|src/Plugins/TreeSitter/tree-sitter>, runs
    these tables: <source-link|ts_dynamic.c|src/Plugins/TreeSitter/ts_dynamic.c>
    fills a <cpp|TSLanguage> with them, so that no C code is generated per
    grammar. The external scanners of the grammars (indentation of Python,
    heredocs of Bash...) are ported to <scheme> and called back by the
    runtime.
  </description>

  This choice avoids both the generated C of the grammars (more than 100
  MB of sources for the languages of <TeXmacs>) and shared libraries per
  platform, which the browser build could not load. Its costs are the
  ports of the scanners and the time of the generation, which is why the
  tables of all the grammars are also shipped. The parser of
  <source-link|ts-parse.scm|TeXmacs/progs/treesit/ts-parse.scm>, a port of
  the runtime to <scheme>, is the reference for the tests; the editor
  always uses the C runtime.

  The data flows as follows:

  <\verbatim-code>
    misc/tree-sitter/\<less\>grammar\<gtr\>/grammar.json

    \ \ --(ts-grammar, ts-nfa, ts-tables or ts_tables.cpp, ts-build)--\<gtr\>
    parse and lex tables

    \ \ --(ts-generate-tsdyn)--\<gtr\> \<less\>grammar\<gtr\>-\<less\>hash\<gtr\>.tsdyn
    in the cache (or the shipped .tsdyn.xz)

    \ \ --(tree-sitter-language-load, ts_dynamic.c)--\<gtr\> a TSLanguage,
    and its scanner (tree-sitter-set-scanner)

    document of lines --(syntax_region.cpp)--\<gtr\> a TSTree, parsed again
    incrementally

    \ \ --(highlights.scm, ts_highlight.cpp)--\<gtr\> the colours of the
    lines

    \ \ --(glue syntax-region-...)--\<gtr\> ts-edit.scm, ts-code.scm
  </verbatim-code>

  <section|Source files>

  <\description>
    <item*|<source-link|tree_sitter_glue.cpp|src/Plugins/TreeSitter/tree_sitter_glue.cpp>>The
    loaded languages and queries (with the text predicates), the
    trampolines of the external scanners, the lexer functions which the
    <scheme> scanners call, the unpacking of the shipped tables.

    <item*|<source-link|ts_dynamic.c|src/Plugins/TreeSitter/ts_dynamic.c>>A
    <cpp|TSLanguage> from the exported tables: every state is a large one,
    the lex functions interpret the lex tables.

    <item*|<source-link|ts_tables.cpp|src/Plugins/TreeSitter/ts_tables.cpp>>The
    <c++> port of the stages of the generator which act on the states, and
    the writing of the exported text.

    <item*|<source-link|syntax_region.cpp|src/Plugins/TreeSitter/syntax_region.cpp>>The
    syntax trees of the code regions, their incremental update, the
    colours of their lines, the requests of <scheme> (nodes, errors,
    captures, indentation, moves).

    <item*|<source-link|ts_highlight.cpp|src/Plugins/TreeSitter/ts_highlight.cpp>>The
    language which wraps the usual language of a program, and the
    highlighters which <scheme> installs.

    <item*|<source-link|tree_sitter_lib.c|src/Plugins/TreeSitter/tree_sitter_lib.c>,
    <source-link|ts_xz.c|src/Plugins/TreeSitter/ts_xz.c>>The runtime of
    tree-sitter and the decoder of XZ Embedded (0BSD, in
    <source-link|xz|src/Plugins/TreeSitter/xz>), each compiled as one
    unit.

    <item*|<source-link|build-glue-treesit.scm|src/Scheme/Glue/build-glue-treesit.scm>>The
    glue: <scm|tree-sitter-...> and <scm|syntax-region-...>.

    <item*|<source-link|ts-registry.scm|TeXmacs/progs/treesit/ts-registry.scm>>The
    grammars, their scanners, the cache of the tables and their
    generation in the background.

    <item*|<source-link|ts-highlight.scm|TeXmacs/progs/treesit/ts-highlight.scm>>The
    highlighters: the queries and the colour classes of their captures.

    <item*|<source-link|ts-edit.scm|TeXmacs/progs/treesit/ts-edit.scm>,
    <source-link|ts-code.scm|TeXmacs/progs/treesit/ts-code.scm>>The
    editing of programs by the tree, and its adapter for the features of
    program code.

    <item*|<source-link|tm-program-languages.scm|TeXmacs/progs/kernel/texmacs/tm-program-languages.scm>>The
    one list of the program languages, with the grammar of each.

    <item*|<source-link|misc/tree-sitter|TeXmacs/misc/tree-sitter>>The
    grammars, their queries, licences and origins (<verbatim|SOURCE>),
    and the shipped tables.

    <item*|<source-link|misc/tree-sitter (tools)|misc/tree-sitter>>The
    tools: the shipped tables, the corpus and capture checks against
    tree-sitter, the generated queries.

    <item*|<source-link|treesit-test.scm|TeXmacs/progs/check/treesit-test.scm>>The
    suite <verbatim|treesit>.
  </description>

  <section|Builds without tree-sitter>

  The feature may be left out: <verbatim|--disable-tree-sitter> of
  <source-link|tm_code.m4|misc/m4/tm_code.m4> (which otherwise defines
  <verbatim|USE_TREE_SITTER> and adds the directory
  <verbatim|TreeSitter> to the build), the option
  <verbatim|USE_TREE_SITTER> of CMake (on by default), and the definition
  in <source-link|config.h|misc/wasm/config.h> for the browser build,
  whose sources are listed in
  <source-link|sources.txt|misc/wasm/sources.txt>. The glue is then not
  compiled, the program languages are not wrapped (the list of the
  program languages wraps them only when <scm|tree-sitter-wrap-language>
  is defined), the usual highlighters colour all the code, and the
  editing of programs takes the answers of
  <source-link|code-syntax-none.scm|TeXmacs/progs/code/code-syntax-none.scm>.
  How the features of program code are registered in the core is
  described in <hlink|the features of program
  code|code-features.en.tm>.

  <\traverse>
    <branch|Grammars and their tables|treesit-grammars.en.tm>

    <branch|The syntax trees of code regions|treesit-regions.en.tm>

    <branch|Highlighting by the syntax tree|treesit-highlight.en.tm>

    <branch|Editing programs with the syntax tree|treesit-editing.en.tm>

    <branch|Pitfalls of tree-sitter|treesit-pitfalls.en.tm>
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
