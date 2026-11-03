<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The syntax trees of code regions>

  <section|Regions>

  A <em|region> is the unit which gets a syntax tree of its own: a
  <markup|document> whose children are the lines of a program typeset in
  one program language (a source file, the body of a code block, a
  session input), or a single string (inline code). This is the shape
  which <hlink|the features of program code|code-features.en.tm> call a
  document of lines; the syntax trees are derived data, never saved.

  <source-link|syntax_region.cpp|src/Plugins/TreeSitter/syntax_region.cpp>
  keeps one <cpp|syntax_region_rep> per region:

  <\description>
    <item*|<cpp|root>, <cpp|language>>The tree of the region, which
    identifies it (<cpp|strong_equal>: the same tree, not an equal one),
    and the index of its language in the C runtime.

    <item*|<cpp|lines>, <cpp|offsets>>The lines as they were at the last
    parse (<TeXmacs> strings, shared, so that the copy is cheap), and the
    offset of the start of each line in the text of the region, a virtual
    newline following each line.

    <item*|<cpp|parser>, <cpp|tree>>The <cpp|TSParser> and the
    <cpp|TSTree>, with the numbers of full and incremental parses
    (<scm|syntax-region-statistics>) and a <cpp|version> which changes at
    each parse.

    <item*|<cpp|hl_cols>, <cpp|hl_dirty>>The colour codes of each position
    of each line, and the lines whose colours are out of date (see
    <hlink|highlighting|treesit-highlight.en.tm>), for the query, the
    injections and the epoch in <cpp|hl_query>, <cpp|hl_injections>,
    <cpp|hl_epoch>.

    <item*|<cpp|ind_caps>>The captures of the query of indentation, for
    <cpp|ind_query> and <cpp|ind_version>.

    <item*|<cpp|injected>>The trees of the parts in other languages
    (injections), kept and edited with the region.
  </description>

  A line which is not a string (markup inside code) is given to the
  parser as its flattened text, the strings of its leaves one after the
  other.

  <section|Cork strings and byte offsets>

  No copy of the text is made for the parser: <cpp|region_read> gives
  tree-sitter the rest of a line from the requested byte, or the virtual
  newline. The lines are in the <TeXmacs> encoding (Cork bytes, and
  entities such as <verbatim|\<less\>less\<gtr\>>,
  <verbatim|\<less\>alpha\<gtr\>> or <verbatim|\<less\>#3B1\<gtr\>>), which
  is neither <abbr|UTF-8> nor a sequence of code points. The input
  therefore uses <cpp|TSInputEncodingCustom> with the decode function
  <cpp|texmacs_decode>: one Cork byte, or one entity (from
  <verbatim|\<less\>> to the next <verbatim|\<gtr\>>, at most 64 bytes, no
  <verbatim|\<less\>> nor newline inside), gives one code point and the
  number of bytes it took. Cork bytes above 127 are mapped through
  <cpp|cork_to_utf8> once (<cpp|init_cork_table>), entities through a
  table filled on first use; an unknown entity is U+FFFD.

  The consequence is that the byte offsets of tree-sitter are offsets in
  the <TeXmacs> strings: a point (row column) of a node is the line and
  the position in its string, with no conversion either way, and the
  lexers still see real characters (<verbatim|\<less\>alpha\<gtr\>> is a
  letter for the identifiers of Julia). The predicates of the queries,
  however, compare the text of the region as it is: a query must write
  <verbatim|"\<less\>less\<gtr\>"> where it means <verbatim|"\<less\>">
  (the Scheme query of <TeXmacs> does).

  <section|Incremental parsing>

  There is no observer: at each request (<cpp|get_region>) the lines of
  the root are compared with those of the last parse. <cpp|code_lines_common>
  (<source-link|code_regions.cpp|src/System/Language/code_regions.cpp>,
  which the language servers use too) gives the common first and last
  lines; between them, the common prefix and suffix of the changed text
  narrow the change to one range. That range becomes one
  <cpp|ts_tree_edit> (start, old and new end, as bytes and as points),
  then the parse starts from the edited tree, and
  <cpp|ts_tree_get_changed_ranges> between the old and the new tree tells
  which other lines changed in structure (a comment opened above them).
  Comparing the lines costs one comparison of addresses per line and
  request (the strings of the lines which did not change are those of the
  document), and covers every kind of change in the same way: typing,
  splitting and joining lines, undo, remote edits, a whole line assigned.
  When the number of the lines stays (a keystroke), the lines, their
  offsets, the text and the colours of the region are changed in place
  (<cpp|update_lines>).

  <paragraph*|Long regions>

  An incremental parse of tree-sitter costs more as the nodes at the top
  level are more (13 ms for 100000 lines of C, after one character). A
  region of more than 6000 lines is therefore cut into parts of about 2000
  lines (<cpp|syntax_group_rep>): each part is a region of its own over a
  slice of the lines, cut where a node at the top level of the parse of
  the whole region starts (after a node without error), so that an edit
  parses its part again only (about 1 ms for a part of C). The requests go to
  the part of their line, their lines made local to it and those of their
  answers global again (<cpp|row_base>); those about the whole region (the
  errors, the captures of the outline, the colours) gather the answers of
  all the parts. An edit which leaves an error in the last node of a part
  (an unclosed comment) joins it to the next one, whose text may now be in
  the comment; an edit over several parts joins them; a part longer than
  three parts is cut again when it ends without error. The parts do not
  see each other: a construct which goes over a cut (none at the top
  level, by its choice) would be parsed in two pieces until the parts are
  joined.

  The regions are kept in a list, the most recently used first, up to
  <cpp|max_regions> (256) regions and <cpp|max_region_bytes> (32 MB) of
  text in all; the least recently used is dropped first. Without that
  cache a document with many code blocks, edited in turn, would parse each
  block again at every edit. A region whose root is asked for with
  another language is parsed again from scratch.
  <scm|syntax-region-forget> drops a region at once (the tests do).

  The injected parts follow the region: an edit inside a part is made on
  its tree, which is parsed again incrementally; an edit before a part
  moves it; an edit across its bounds drops its tree. At most 64 trees of
  parts are kept per region.

  <section|The functions of <scheme>>

  No node object crosses into <scheme>: a tree changes at each keystroke.
  The glue takes the root of a region and the index of its language, and
  gives plain data, the points being lists <scm|(row column)>:

  <\description>
    <item*|<scm|syntax-region-sexp>>The tree as the S-expression of
    <cpp|ts_node_string>, for the tests.

    <item*|<scm|syntax-region-node-at>, <scm|syntax-region-node-range>>The
    nodes around a point or a range, innermost first, each as
    <scm|(type named? start end field)>.

    <item*|<scm|syntax-region-errors>>The <verbatim|ERROR> and
    <verbatim|MISSING> nodes, as <scm|("error" start end)> or
    <scm|("missing" start end)>.

    <item*|<scm|syntax-region-last-nodes>>The root, its last child, the
    last child of that one..., outermost first (where the text ends).

    <item*|<scm|syntax-region-captures>>The captures of a query, in the
    order of the text, after the predicates: <scm|(capture start end)>,
    capture being an index into <scm|tree-sitter-query-capture-names>.

    <item*|<scm|syntax-region-indent>>The indentation of a line by the
    query <verbatim|indents.scm> (<cpp|syntax_region_indent>, a port of
    <verbatim|get_indent> of nvim-treesitter, with the Lisp rules of
    <TeXmacs> for <scheme>); -1 for <verbatim|@indent.auto>, -2 without a
    tree.

    <item*|<scm|syntax-region-errors-with>>The number of errors if a text
    were inserted at a point: the edit and an incremental parse of a copy
    of the tree.

    <item*|<scm|syntax-region-node-move>, <scm|syntax-region-node-info>>The
    targets of the structured moves, and the ranges which the structural
    edits need (the node at the point, its siblings, its parent, the
    brackets around, the node which ends there).

    <item*|<scm|syntax-region-statistics>, <scm|syntax-region-forget>>The
    numbers of full and incremental parses; dropping a region.
  </description>

  Each of these brings the region up to date first, so that several
  requests after an edit parse once.

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
