<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Selections, the clipboard, search and replace>

  <section|Selections>

  The selection of an editor is a <cpp|range_set> <cpp|cur_sel> of
  <cpp|edit_select_rep> (<verbatim|Edit/Replace/edit_select.cpp>): a flat
  array of paths <math|(s<rsub|1>,e<rsub|1>,s<rsub|2>,e<rsub|2>,\<ldots\>)>,
  of which normally only the first range is used. The overall organization
  of the editor state is described in <hlink|the editor|server-editor.en.tm>; here are the details which matter for editing
  operations.

  <paragraph|Setting the selection.><cpp|select (p1, p2)> orders the two
  paths and stores them. When the common ancestor is not a table or a row
  and semantic editing is enabled (the preferences <verbatim|semantic
  editing> and <verbatim|semantic selections>), it first lets
  <cpp|semantic_select> enlarge the range to a syntactically meaningful
  subformula: inside mathematics (or code in the <verbatim|minimal>
  language), the packrat parser of the current language
  (<cpp|packrat_select>) is asked for the smallest grammatical unit
  containing the range. <cpp|select_enlarge> and
  <cpp|select_enlarge_environmental> implement the repeated
  \Pselect more\Q commands; <cpp|selection_set_start>,
  <cpp|selection_set_end> and <cpp|selection_set_paths>
  (<scm|selection-set-start>, ...) are the <scheme> level setters.

  <paragraph|Normal and table selections.>A selection is a <em|table
  selection> when both ends are in cells of the same table
  (<cpp|is_table_selection>); <cpp|selection_get_subtable> then returns the
  path of the table format and the rectangle of selected cells, enlarged by
  <cpp|table_bound> to whole spanning cells. <cpp|selection_active_any>,
  <cpp|selection_active_normal>, <cpp|selection_active_table> and
  <cpp|selection_active_small> (a normal selection which does not span
  several paragraphs) are the predicates used by the editing operations to
  decide whether to wrap the selection.

  <paragraph|Reading the selection.><cpp|selection_get ()>
  (<scm|selection-tree>) returns the selected material as a tree: a
  subtable (with its formats) for table selections, otherwise the result of
  <cpp|selection_compute> on the corrected range, simplified with
  <cpp|simplify_correct>. The range is first corrected by
  <cpp|selection_correct>, which moves its ends to positions that make a
  well-formed selection (and, in source mode, uses source access to the
  tree). <cpp|selection_get_cut ()> returns the selection and deletes it;
  this is what the math constructors use to wrap a selection.

  <paragraph|Cutting.><cpp|cut (p1, p2)> and its worker <cpp|raw_cut>
  delete everything between two paths: they recurse through
  <markup|document> and <markup|concat> nodes (deleting whole children in
  the middle and partial children at the ends, then joining the
  remainders), empty the cells of a table rectangle (and remove whole rows
  or columns when complete rows or columns are selected), replace a
  selected compound tree by the empty string, and remove characters in a
  string. <cpp|selection_cut (key)> copies the selection to the clipboard
  <cpp|key> (unless <cpp|key> is <verbatim|"none">) and cuts it.

  <paragraph|Focus and alternative selections.>The <em|focus> (the tree
  for which the focus menus and toolbars are shown) is computed by
  <cpp|focus_get>: the manually set focus (<cpp|manual_focus_set>,
  <scm|set-manual-focus-path>) if there is one, otherwise the selection or
  the cursor, after skipping \Ptransparent\Q nodes such as
  <markup|concat>, <markup|document>, table nodes and a few style specific
  tags. Named <em|alternative selections> (<cpp|set_alt_selection>,
  <scm|set-alt-selection>) are additional range sets drawn by the editor;
  the search tools use <verbatim|alternate> for all matches and
  <verbatim|search-reference> for the starting point.

  <section|The clipboard>

  Clipboards are identified by names: <verbatim|primary> is the ordinary
  clipboard, <verbatim|mouse> the selection of the X11 middle button, and
  any other name a private clipboard (for instance <verbatim|wrapbuf>,
  used by <scm|make> to wrap the selection, or <verbatim|nowhere>, used
  when a selection is deleted). The contents of a clipboard are a tuple
  <verbatim|(texmacs <em|tree> <em|mode> <em|language>)>, recording the
  mode and language at the selection, together with string versions for
  the system clipboard.

  <paragraph|Copy.><cpp|selection_set (key, t)> builds the tuple. For the
  <verbatim|primary> and <verbatim|mouse> clipboards it also converts the
  tree to a string in the <em|export format> (<cpp|selection_export>, set
  with <scm|clipboard-set-export>): <verbatim|verbatim>, <verbatim|html>
  and <verbatim|latex> are first expanded with the typesetter
  (<cpp|exec_verbatim>, <cpp|exec_html>, <cpp|exec_latex>) and then
  converted with the corresponding <verbatim|-snippet> converter; the
  default is the <TeXmacs> format, plus, under <name|Qt>, a verbatim version
  for other applications. The result is handed to the GUI with
  <cpp|::set_selection>. <cpp|selection_copy (key)> (<scm|clipboard-copy>)
  copies the current selection; inside an active graphics, the graphics
  editor's <scm|graphics-copy> is used instead.

  <paragraph|Paste.><cpp|selection_paste (key)> (<scm|clipboard-paste>)
  retrieves the clipboard through <cpp|::get_selection> in the <em|import
  format> (<scm|clipboard-set-import>):

  <\itemize>
    <item>contents from another application (<verbatim|(extern
    <em|string>)>) are converted with the import format, with some care for
    <LaTeX> in math mode (dollars are added) and for code in program mode;

    <item><TeXmacs> contents are inserted with <cpp|insert_tree>, after
    adapting the mode: text pasted into a formula is refused with an error
    message, a formula pasted into text is wrapped in <markup|math> (and
    conversely), a formula pasted into a program session is converted by
    the plug-in (<scm|plugin-math-input>), and a table pasted inside a table
    is written into the existing table at the cursor with
    <cpp|table_write_subtable>.
  </itemize>

  The <scheme> commands <scm|clipboard-copy>, <scm|clipboard-cut> and
  <scm|clipboard-paste> (<verbatim|utils/library/cpp-wrap.scm>) are
  overloaded in several contexts (sessions, folding, comments), and
  <verbatim|utils/edit/selections.scm> adds commands such as
  <scm|clipboard-copy-export>.

  <section|Structural searches>

  <cpp|edit_replace_rep> (<verbatim|Edit/Replace/edit_search.cpp>)
  provides the queries which editing code uses to find its context:

  <\description>
    <item*|<cpp|search_upwards (l)>, <cpp|inside (l)>>The innermost
    ancestor of the cursor with the label <cpp|l> (<scm|inside?>). Note
    that the search starts at the <em|grand>parent of the cursor path, that
    is, at the innermost tree that contains the cursor position.

    <item*|<cpp|search_parent_upwards (l)>>The path of the child of the
    innermost <cpp|l> ancestor in which the cursor is.

    <item*|<cpp|search_upwards_with (var, val)>,
    <cpp|inside_with>>The innermost <markup|with> setting <cpp|var> to
    <cpp|val>.

    <item*|<cpp|search_upwards_in_set (t)>, <cpp|inside_which>>The
    innermost ancestor whose label is in a tuple of names.

    <item*|<cpp|search_previous_compound>,
    <cpp|search_next_compound>>The previous or next accessible tree with a
    given label in document order.
  </description>

  <section|Search and replace>

  There are two independent implementations of search and replace.

  <subsection|The search and replace tools>

  The search and replace commands of the menus and keyboard
  (<scm|interactive-search>, <scm|interactive-replace>) are implemented in
  <scheme>, in <verbatim|generic/search-widgets.scm>. They open either a
  toolbar or a search tool with an embedded <TeXmacs> input field, whose
  contents is an arbitrary <TeXmacs> tree, the <em|pattern>. After each
  change of the pattern, <scm|perform-search>:

  <\enumerate>
    <item>calls the glue function <scm|tree-search-tree-at>, that is,
    <cpp|search (t, what, p, pos, limit)> in
    <verbatim|Data/Tree/tree_search.cpp>, on the body of the document
    being searched, starting near the cursor and with an initial limit of
    100 matches;

    <item>filters the results by mode and language
    (<scm|filter-search-results>): unless the document is in source mode,
    a match is only kept when the mode and the language of that mode at the
    match are the same as at the cursor when the search started;

    <item>stores all matches as the alternative selection
    <verbatim|alternate>, which the editor highlights, and selects the next
    match after the reference position;

    <item>if the limit was reached, schedules a new search with twice the
    limit, so that large documents are searched incrementally.
  </enumerate>

  The matcher compares the pattern with the document tree, recursing into
  accessible children. Its behaviour is controlled by preferences, read at
  the start of each search (<cpp|initialize_search>):
  <verbatim|allow-partial-match> (a string pattern matches inside a longer
  string), <verbatim|allow-initial-match> (it matches a prefix),
  <verbatim|allow-blank-match> (an empty argument of the pattern matches
  anything), <verbatim|allow-injective-match> (the arguments of a compound
  pattern only need to match a subsequence of the arguments of the
  document tree), <verbatim|allow-cascaded-match> (an argument of the
  pattern may match deeper inside the corresponding argument), and
  <verbatim|case-insensitive-match>. When the preference
  <verbatim|search-and-replace> is set (by <scm|interactive-replace>), the
  first four are replaced by their stricter <verbatim|allow-*-replace>
  counterparts, which are off by default. Patterns may also contain
  <markup|wildcard> tags, and a <markup|select-region> tag restricts the
  match to a part of the pattern.

  Replacing is also done in <scheme>: <scm|replace-one> and
  <scm|replace-all> select the next match, compute the replacement with
  <scm|adjust-by> (which substitutes wildcards and preserves the
  capitalization of the matched text), cut the match and insert the
  replacement, and search again; <scm|replace-all> repeats this inside a
  single <scm|start-editing> / <scm|end-editing> pair so that it can be
  undone at once.

  <subsection|The keyboard search mode>

  The <c++> class <cpp|edit_replace_rep> still contains an older,
  keyboard driven implementation. It is no longer bound to the standard
  shortcuts or menus (which call <scm|interactive-search> and
  <scm|interactive-replace>), but its glue routines are still exported
  (<scm|search-start>, <scm|replace-start>, and <scm|replace-start-forward>
  in <verbatim|utils/library/cursor.scm>). <cpp|search_start (forward)>
  (<scm|search-start>) puts the editor in the input mode
  <cpp|INPUT_SEARCH>; each typed key is then passed by the
  <scm|keyboard-press> overload for <scm|search-mode?>
  (<verbatim|generic/generic-edit.scm>) to <cpp|search_keypress>
  (<scm|key-press-search>), which extends the pattern and moves to the next
  match with <cpp|next_match>, a walk through accessible positions in
  document order (<cpp|step_horizontal>, <cpp|step_ascend>,
  <cpp|step_descend>). Backspace returns to the previous match and
  pattern. <cpp|replace_start> and <cpp|replace_keypress> implement the
  classical \Preplace (y, n, a)?\Q dialogue in the footer. Matches are
  restricted to the mode and language at the starting point, and patterns
  are compared literally (<cpp|test_match>, where an empty string in a
  compound pattern acts as a wildcard). The same input-mode mechanism is
  used by spell checking (<cpp|spell_start>, <cpp|spell_keypress>, see
  <hlink|languages, hyphenation and spell checking|language.en.tm>).

  <section|Pitfalls>

  <\itemize>
    <item>The case insensitive search only lowercases the <em|document>:
    <cpp|search_string> (<verbatim|Data/Tree/tree_search.cpp:256>) compares
    the lowercased text with the pattern as it is, and the other matching
    routines (<cpp|match>, <cpp|match_atomic>, used for compound and
    wildcard patterns) ignore the flag altogether. The search toolbar
    lowercases the pattern before searching
    (<scm|search-toolbar-search>), but the search tool
    (<scm|open-search>) does not, so with
    <verbatim|case-insensitive-match> on, a pattern containing capitals
    typed in the search tool never matches.

    <item><cpp|compute_selection> temporarily switches to source access
    mode, with a comment in the code explaining that the <abbr|DRD> may be
    the wrong one when searching from a popup window, which can cause a
    whole macro to be highlighted instead of the matched text inside it.

    <item>The keyboard search mode and the search tools do not share their
    state: the former stores its last pattern in the clipboard
    <verbatim|search>, the latter in the auxiliary buffer of the tool.
  </itemize>

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
