<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The typesetting algorithm>

  <section|Introduction>

  The <TeXmacs> typesetter converts the edit tree of a document into a tree
  of graphical boxes. The boxes themselves, the way they record their source
  location and the routines for cursor positioning and selection are
  described in <hlink|the boxes produced by the typesetter|boxes.en.tm>; the
  evaluation of macros, <markup|with> constructs and other environment
  primitives is described in the chapter on <hlink|macro
  expansion|macro-expansion.en.tm>. The present chapter explains what lies
  in between: how the typesetter walks the tree, how it produces lines,
  paragraphs and pages, and how it manages to redo only a small amount of
  work after each keystroke.

  Contrary to a batch system like <LaTeX>, the <TeXmacs> typesetter is
  <em|incremental>: the complete document is kept in typeset form, and every
  modification of the edit tree is notified to the typesetter, which marks
  the corresponding parts as invalid. When the screen has to be updated, only
  the invalid paragraphs are re-typeset; the cached results for all other
  paragraphs are reused. Page breaking, on the other hand, is redone for the
  whole document at each update, but it works on precomputed page items and
  is therefore relatively cheap.

  All C++ file names in this chapter are relative to <verbatim|src/src/>.
  The typesetter lives in <verbatim|Typeset/>, with the public interface in
  <verbatim|Typeset/typesetter.hpp>. It is driven by the editor from
  <verbatim|Edit/Editor/edit_typeset.cpp>.

  <section|Overview of the pipeline>

  Typesetting a document proceeds through the following stages. Each stage
  has its own data structure, and the output of one stage is the input of
  the next one.

  <\enumerate>
    <item><em|Bridges> (<verbatim|Typeset/Bridge/>). The top-level
    <markup|document> and the structural constructs that may contain whole
    paragraphs (<markup|with>, <markup|surround>, macro applications,
    <markup|arg>, <markup|locus>, <abbr|etc.>) are mirrored by a tree of
    <cpp|bridge> objects. Each bridge remembers the subtree it is responsible
    for, whether it is still valid, the lines it produced last time and the
    changes it made to the environment. Bridges receive the modification
    notifications and decide what has to be re-typeset.

    <item><em|Concatenation> (<verbatim|Typeset/Concat/>). Each paragraph
    is handed to a <cpp|concater_rep>, which traverses the paragraph's tree
    (text, mathematics, inline macros, tables, graphics, ...) and
    produces a flat array of <cpp|line_item>s: boxes decorated with the
    space that follows them, a line-breaking penalty, a type and, for control
    items, the tree of the control command.

    <item><em|Line breaking and paragraph formatting>
    (<verbatim|Typeset/Line/>). A <cpp|lazy_paragraph_rep> splits the line
    items into paragraph units, calls the line breaker
    (<verbatim|Typeset/Line/line_breaker.cpp>) to find optimal break points,
    justifies each line and turns it into a <cpp|phrase_box>.

    <item><em|Vertical stacking> (<verbatim|Typeset/Stack/>). The lines are
    passed to a <cpp|stacker_rep>, which computes the vertical distance
    between successive lines (shoving lines into each other when possible),
    paragraph separations and page-breaking penalties. The result is an
    array of <cpp|page_item>s.

    <item><em|Page breaking> (<verbatim|Typeset/Page/>). The page items of
    the whole document are handed to a <cpp|pager_rep>, which calls the page
    breaker to cut the item list into pages, places floats and footnotes,
    and finally builds page boxes with headers and footers. On screen, in
    <verbatim|papyrus> mode, a single tall page is produced.

    <item><em|Screen update> (<verbatim|Edit/Editor/edit_typeset.cpp>,
    <verbatim|Edit/Interface/edit_interface.cpp>). The editor stores the
    resulting box as <cpp|eb>, determines the rectangle of the screen that
    really changed and invalidates it.
  </enumerate>

  Tables (<verbatim|Typeset/Table/>) are typeset by a separate module,
  called by the concater for inline tables, and by the paragraph formatter
  for tables that may be broken across pages. The environment
  (<verbatim|Typeset/Env/>) is used by all stages to read style parameters
  and to evaluate macros.

  <section|Source map>

  <\description>
    <item*|<verbatim|Typeset/typesetter.hpp>>The public interface:
    <cpp|new_typesetter>, the <cpp|notify_*> functions, <cpp|typeset>, and
    the helpers <cpp|typeset_as_concat>, <cpp|typeset_as_box>,
    <cpp|typeset_as_atomic>, <cpp|typeset_as_stack>,
    <cpp|typeset_as_table>, <cpp|typeset_as_var_table>,
    <cpp|typeset_as_paragraph> and <cpp|typeset_as_document>.

    <item*|<verbatim|Typeset/Bridge/>>The <cpp|typesetter_rep> class
    (<verbatim|impl_typesetter.hpp>, <verbatim|typesetter.cpp>), the
    abstract <cpp|bridge_rep> (<verbatim|bridge.hpp>,
    <verbatim|bridge.cpp>) and one file per kind of bridge.

    <item*|<verbatim|Typeset/Format/>, <verbatim|Typeset/formatter.hpp>>The
    formatter data structures: <cpp|line_item>, <cpp|page_item>,
    <cpp|stack_border>, and the <cpp|format> and <cpp|lazy> classes.

    <item*|<verbatim|Typeset/Concat/>>The concater: strings and spacing
    (<verbatim|concat_text.cpp>), mathematics (<verbatim|concat_math.cpp>),
    macros (<verbatim|concat_macro.cpp>), other active and inactive markup,
    graphics, animations, and post-processing of brackets and scripts
    (<verbatim|concat_post.cpp>).

    <item*|<verbatim|Typeset/Line/>>Paragraph formatting
    (<verbatim|lazy_paragraph.cpp>), line breaking
    (<verbatim|line_breaker.cpp>), lazy structures for documents and other
    vertical material (<verbatim|lazy_typeset.cpp>,
    <verbatim|lazy_vstream.cpp>, <verbatim|lazy_gui.cpp>).

    <item*|<verbatim|Typeset/Stack/>>The stacker.

    <item*|<verbatim|Typeset/Table/>>Tables and cells.

    <item*|<verbatim|Typeset/Page/>>The pager (<verbatim|pager.cpp>,
    <verbatim|make_pages.cpp>), the page breakers
    (<verbatim|new_breaker.cpp>, <verbatim|columns_breaker.cpp> and the
    older <verbatim|page_breaker.cpp>), and the auxiliary types
    <cpp|pagelet>, <cpp|insertion> (<verbatim|skeleton.hpp>) and
    <cpp|vpenalty> (<verbatim|vpenalty.hpp>).

    <item*|<verbatim|Typeset/Boxes/>>The box classes; see <hlink|the
    boxes|boxes.en.tm>.

    <item*|<verbatim|Typeset/Env/>, <verbatim|Typeset/env.hpp>>The
    typesetting environment <cpp|edit_env>; see <hlink|macro
    expansion|macro-expansion.en.tm>.
  </description>

  <section|Main entry points>

  <\explain>
    <cpp|typesetter new_typesetter (edit_env& env, tree et, path
    ip)><explain-synopsis|create a typesetter>
  <|explain>
    Creates a typesetter for the document body <cpp|et>, whose inverse path
    is <cpp|ip>. The typesetter builds the root bridge immediately, but does
    not typeset anything yet. Each editor owns one typesetter (the member
    <cpp|ttt> of <cpp|edit_typeset_rep>).
  </explain>

  <\explain>
    <cpp|void notify_assign (typesetter ttt, path p, tree u)>

    <cpp|void notify_insert (typesetter ttt, path p, tree u)>

    <cpp|void notify_remove (typesetter ttt, path p, int
    nr)><explain-synopsis|inform the typesetter about modifications>
  <|explain>
    These functions (together with <cpp|notify_split>, <cpp|notify_join>,
    <cpp|notify_assign_node>, <cpp|notify_insert_node> and
    <cpp|notify_remove_node>) mirror the elementary modifications of the
    edit tree. The path <cpp|p> is relative to the root of the typeset
    document. They only invalidate bridges; no typesetting takes place.
  </explain>

  <\explain>
    <cpp|box typeset (typesetter ttt, SI& x1, SI& y1, SI& x2, SI&
    y2)><explain-synopsis|typeset the document>
  <|explain>
    Re-typesets the invalid parts of the document, reassembles the pages
    and returns the box of the whole document. On return, the four
    coordinates delimit the region whose appearance changed since the
    previous call.
  </explain>

  <\explain>
    <cpp|void exec_until (typesetter ttt, path p)><explain-synopsis|compute
    the environment at a path>
  <|explain>
    Brings the environment of the typesetter into the state it has just
    before the position <cpp|p>, reusing the environment changes cached in
    the bridges. This is used to determine the environment at the cursor.
  </explain>

  <\explain>
    <cpp|box typeset_as_document (edit_env e, tree t, path
    ip)><explain-synopsis|one-shot typesetting>
  <|explain>
    Creates a temporary typesetter, typesets the tree <cpp|t> as a complete
    document and deletes the typesetter again. This is used for printing
    (<cpp|edit_main_rep::print_doc>) and for counting pages. The other
    <cpp|typeset_as_*> helpers typeset fragments inside a given environment
    and are used recursively by the typesetter itself.
  </explain>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Incremental typesetting: the typesetter and its
    bridges|typesetter-bridges.en.tm>

    <branch|Concatenation, line breaking and paragraph
    formatting|typesetter-lines.en.tm>

    <branch|Typesetting tables|typesetter-tables.en.tm>

    <branch|Page breaking and the construction of pages|typesetter-pages.en.tm>
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
