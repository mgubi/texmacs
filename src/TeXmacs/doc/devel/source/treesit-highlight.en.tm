<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Highlighting by the syntax tree>

  <section|A language over the usual language>

  The typesetter colours program code through the <cpp|language_rep> of
  the program language (<cpp|advance>, <cpp|get_color>, <cpp|highlight>).
  Tree-sitter does not replace these languages: it wraps them. The core
  lets a feature register a wrapper (<cpp|set_prog_language_wrapper>,
  <source-link|language.hpp|src/System/Language/language.hpp>), which
  <cpp|prog_language> in
  <source-link|prog_language.cpp|src/System/Language/prog_language.cpp>
  applies to the usual language when it first makes the language of a
  name. <cpp|tree_sitter_wrap_language> registers it and lists the
  languages to wrap; it is called when
  <source-link|tm-program-languages.scm|TeXmacs/progs/kernel/texmacs/tm-program-languages.scm>
  is loaded, for each program language with a grammar.

  <cpp|treesit_language_rep>
  (<source-link|ts_highlight.cpp|src/Plugins/TreeSitter/ts_highlight.cpp>)
  keeps the usual language as <cpp|base> and always asks it for the
  hyphenation and the groups. As long as no highlighter is installed for
  its language, or when highlighting by tree-sitter is off, it also asks
  it for <cpp|advance>, <cpp|get_color> and <cpp|highlight>: the code is
  coloured as without tree-sitter. The usual languages stay for this
  reason, and because the typesetter needs the properties of the tokens
  (spaces, breaks) which they give.

  Each wrapped language has a highlight code of its own, <cpp|hl_lan>,
  taken from a counter which starts at 1000000, far from the codes of the
  packrat languages and from those of the core for the folds (999991 to
  999999) and the layers of the lines (999980, 999981, see
  <source-link|code_lines.cpp|src/System/Language/code_lines.cpp>).

  <section|The highlighter>

  At its first <cpp|highlight>, a wrapped language calls the <scheme>
  function <scm|treesit-highlighter-request> of
  <source-link|ts-highlight.scm|TeXmacs/progs/treesit/ts-highlight.scm>
  (lazily defined at boot). When the preference <verbatim|tree-sitter
  highlighting> is on and the tables of the grammar are in the cache, the
  highlighter is installed at once; otherwise the tables are prepared in
  the background, and when they arrive the highlighter is installed,
  <scm|tree-sitter-reset-highlights> is called and the open buffers are
  typeset again.

  <scm|treesit-install-highlighter> compiles the query
  <verbatim|highlights.scm> of the grammar (after the queries named by a
  first line <verbatim|; inherits: ...>) and gives <c++> the colour class
  of each of its captures: <scm|treesit-capture-classes> maps a capture
  name to one of the classes of <cpp|initialize_color_encodings>, the
  longest dotted prefix deciding (<verbatim|@keyword.control.return> is
  <verbatim|keyword_control>, <verbatim|@string.special.key> is
  <verbatim|declare_identifier>); a name without class
  (<verbatim|@variable>) keeps the colour of the text.
  <cpp|tree_sitter_set_highlighter> turns the classes into colour codes,
  which <cpp|decode_color> turns into the preferences
  <verbatim|syntax:\<less\>language\<gtr\>:\<less\>class\<gtr\>>: the code
  of tree-sitter has the palette of the other program languages. The
  query <verbatim|injections.scm>, when there is one, is compiled too
  (<scm|tree-sitter-set-injections>).

  <section|The colours of the lines>

  <cpp|treesit_language_rep::highlight> is called by the typesetter for a
  document and for each line. A document is a region. For a line, the
  region is its parent, found from the ip of the line in the edited tree,
  when the parent is a document and its child at that place is the line;
  a line which has no such parent (a detached tree, a string inside
  markup) is a region by itself.

  <cpp|syntax_region_highlight> gives the colour code of each position of
  each line. It colours again the lines from the first to the last which
  are out of date (<cpp|hl_dirty>, with those between them): all of them
  when the query, the injections or the epoch changed, else those
  which an edit changed and those of the changed ranges of the parse. The
  query runs on the bytes of these lines
  (<cpp|ts_query_cursor_set_byte_range>), then:

  <\enumerate>
    <item><cpp|paint_captures> paints the captures as
    tree-sitter-highlight does: in the order of the text, an inner node
    over an outer one, and for one node the <em|last> pattern which
    captures it (none when its capture has no colour). The upstream
    queries are written for this rule.

    <item>The injections are painted over this (see below).

    <item><cpp|paint_unmatched> gives the colour of the class
    <verbatim|error> to the brackets which error recovery left unmatched:
    those directly in an <verbatim|ERROR> node, and the opening bracket of
    a node whose closing one is <verbatim|MISSING>.

    <item><cpp|mark_tokens> sets the bit <cpp|TS_TOKEN_START> (bit 24) at
    the start of each leaf of the tree.
  </enumerate>

  The colours of the lines are then stored on their highlight observers,
  as for the packrat languages (<cpp|set_line_colors>, runs of equal codes
  through <cpp|attach_highlight>). The first time, all the lines get their
  colours and the document a mark of the same code. A modification is
  only announced to what it modifies: an edited line loses its observer,
  the other lines keep theirs, and the next highlight of the edited line
  updates the region and stores the colours of the lines which were
  coloured again. A line whose stored colours change without it being
  modified (typing a quote or the start of a comment above it) is put in
  the pending updates. Inserting or removing lines modifies the document,
  which then loses its mark: all its lines are compared again, which
  costs one comparison of arrays per line.

  <section|Pending updates and epochs>

  The lines of the pending updates cannot be typeset again at once: the
  typesetter is in the middle of a pass. The first of them makes
  <c++> call <scm|treesit-schedule-updates>, which, when the editor is
  idle, takes them (<scm|tree-sitter-pending-updates>) and calls
  <scm|update-path> on those which are still in a document.

  <cpp|tree_sitter_reset_highlights> increments an epoch. At its next
  highlight, a wrapped language whose epoch is older takes a new highlight
  code: the colours stored with the old code are no longer read, and the
  regions colour all their lines again, since their epoch differs too.
  This is how a language which injects another one gets its colours again
  when the highlighter of the injected language arrives after the region
  was coloured.

  <section|Injections>

  Each <verbatim|@injection.content> of the query of injections gives a
  range and a language name, from <verbatim|@injection.language> or
  <verbatim|#set! injection.language>. The name is resolved to a program
  language through <scm|tree-sitter-set-injection-name>, filled from the
  names of <source-link|tm-program-languages.scm|TeXmacs/progs/kernel/texmacs/tm-program-languages.scm>; the first use of a language
  asks for its highlighter, which may arrive later (the epoch). The range
  is parsed with the grammar of that language (with the same decoding of
  Cork strings), painted with its highlight query over the colours of the
  region, and its own injections are painted in turn, down to a depth of
  3. The trees of the injected parts of the region are kept and parsed
  again incrementally; those of the parts nested inside them are parsed
  again at each colouring.

  <section|Tokens and the typesetting>

  <cpp|typeset_prog_string>
  (<source-link|concat_text.cpp|src/Typeset/Concat/concat_text.cpp>) cuts
  a line into tokens by calling <cpp|advance> of the language, and asks
  <cpp|get_color> for the colour of each token. The wrapper cuts where the
  tree has a token: at each position marked <cpp|TS_TOKEN_START>, at each
  change of colour (injected code), and around each space and each tab of
  a source file (<verbatim|\<less\>#9\<gtr\>>, four bytes, see
  <cpp|is_source_tab>), so that a line may also break in a comment or a
  string. <cpp|get_color> takes the colour of the first position of the
  token, without the bit (<cpp|TS_COLOR_MASK>). A line whose stored
  colours do not have its length is cut by the usual language, and left
  without colours until it is highlighted again.

  The glue <scm|tree-sitter-line-colors> and <scm|tree-sitter-line-tokens>
  give the coloured runs and the tokens of a line, as the typesetter sees
  them, for the tests; <scm|tree-sitter-set-highlighting> turns the
  highlighting by tree-sitter off and on without writing the preferences
  (the suite <verbatim|parse>, which tests the usual languages, does).

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
