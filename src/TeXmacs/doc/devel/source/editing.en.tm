<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Structured editing, search and automatic content>

  <section|Introduction>

  Almost every editing command of <TeXmacs>, from typing a character to
  inserting a table column or regenerating the index, ends up in one of the
  <c++> classes of the directories <source-link|Edit/Modify/|src/Edit/Modify>,
  <source-link|Edit/Replace/|src/Edit/Replace> and <source-link|Edit/Process/|src/Edit/Process>. These classes
  implement the <em|structured editing operations>: they know how text,
  formulas, tables and macro applications are represented as trees, and turn
  a request such as \Pdelete backwards\Q or \Pmake a fraction\Q into a short
  sequence of elementary tree modifications that keeps the document
  well-formed.

  This chapter describes these operations. It does not repeat the parts
  that are documented elsewhere:

  <\itemize>
    <item>the structure of the editor classes, the editor state (cursor,
    selection, focus) and the way elementary modifications are propagated
    to the typesetter and to the undo history are described in <hlink|the
    editor|server-editor.en.tm>;

    <item>the <scheme> routines for modifying trees directly
    (<scm|tree-assign!>, <scm|tree-insert!>, <scm|tree-set!>, ...) and for
    navigating by paths are documented in <hlink|programming routines for
    editing documents|../scheme/edit/scheme-edit.en.tm>;

    <item>the compilation of bibliographies is described in <hlink|the
    database and bibliographies|database-bibliography.en.tm>;

    <item>spell checking and the language machinery behind it are described
    in <hlink|languages, hyphenation and spell checking|language.en.tm>.
  </itemize>

  All file names below are relative to <source-link|src/src/|src> unless stated
  otherwise; <scheme> files are relative to <source-link|src/TeXmacs/progs/|TeXmacs/progs>.

  <section|Overview>

  An editing command usually goes through four layers:

  <\enumerate>
    <item>A <scheme> command, bound to a key or a menu entry. Keyboard
    commands are mostly generic dispatchers defined with <scm|tm-define>,
    which are overloaded for particular tags with <scm|:require> clauses;
    for instance <scm|kbd-backspace> calls <scm|(kbd-remove (focus-tree)
    #f)>, and <scm|kbd-remove> has specialized definitions for sessions,
    folding environments, databases, ... before falling back on
    <scm|remove-text> (<source-link|generic/generic-edit.scm|TeXmacs/progs/generic/generic-edit.scm>). Thin wrappers
    such as <scm|insert>, <scm|make>, <scm|make-fraction> or
    <scm|clipboard-paste> are defined in <source-link|utils/library/cpp-wrap.scm|TeXmacs/progs/utils/library/cpp-wrap.scm>.

    <item>A glue routine of <source-link|Scheme/Glue/build-glue-editor.scm|src/Scheme/Glue/build-glue-editor.scm>,
    such as <scm|cpp-insert> (<cpp|insert_tree>), <scm|remove-text>
    (<cpp|remove_text>), <scm|cpp-make> (<cpp|make_compound>) or
    <scm|table-insert-column> (<cpp|table_insert_column>), which calls the
    corresponding method of the current editor.

    <item>The structured operation itself, a method of one of the classes
    <cpp|edit_text_rep>, <cpp|edit_math_rep>, <cpp|edit_table_rep>,
    <cpp|edit_dynamic_rep>, <cpp|edit_select_rep>,
    <cpp|edit_replace_rep> or <cpp|edit_process_rep>, all of which derive
    virtually from <cpp|editor_rep> and are combined in
    <cpp|edit_main_rep>. Since the classes only see each other through the
    pure virtual methods of <cpp|editor_rep>, any operation can call any
    other: the deletion code of <cpp|edit_text_rep> calls the table code
    for cells and the math code for brackets, and almost every constructor
    ends with <cpp|insert_tree>.

    <item>Elementary modifications: <cpp|assign>, <cpp|insert>,
    <cpp|remove>, <cpp|split>, <cpp|join>, <cpp|assign_node>,
    <cpp|insert_node> and <cpp|remove_node> on absolute paths in the global
    edit tree. These are the only functions which change the document.
    They notify the observers, so that the typesetter, the undo history,
    position observers and the other views are updated automatically (see
    <hlink|the modification pipeline|server-editor.en.tm>).
  </enumerate>

  Two consequences of this design are worth keeping in mind. First, the
  structured operations work directly on the global tree <cpp|et> with
  paths starting with the root path <cpp|rp> of the buffer, and they
  re-read <cpp|subtree (et, p)> after each elementary modification, since
  a local copy would be stale. Second, after a sequence of modifications
  the cursor <cpp|tp> is corrected by the editor itself (in
  <cpp|post_notify>), so the operations only need to move it explicitly
  with <cpp|go_to> when the natural correction is not what the user
  expects.

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Edit/Modify/edit_text.hpp|src/Edit/Modify/edit_text.hpp>, <source-link|edit_text.cpp|src/Edit/Modify/edit_text.cpp>,
    <source-link|edit_delete.cpp|src/Edit/Modify/edit_delete.cpp>>The class <cpp|edit_text_rep>: insertion of
    trees and paragraphs, normalization of <markup|concat> nodes, spaces,
    images, and the general deletion algorithm (<source-link|edit_delete.cpp|src/Edit/Modify/edit_delete.cpp>).

    <item*|<source-link|Edit/Modify/edit_math.hpp|src/Edit/Modify/edit_math.hpp>,
    <source-link|edit_math.cpp|src/Edit/Modify/edit_math.cpp>>The class <cpp|edit_math_rep>: constructors for
    fractions, roots, scripts, primes, wide accents, negations and trees,
    and the deletion rules for brackets, primes, wide accents and trees.

    <item*|<source-link|Edit/Modify/edit_dynamic.hpp|src/Edit/Modify/edit_dynamic.hpp>,
    <source-link|edit_dynamic.cpp|src/Edit/Modify/edit_dynamic.cpp>>The class <cpp|edit_dynamic_rep>: insertion
    of arbitrary tags (<cpp|make_compound>), activation of inactive tags,
    insertion and removal of arguments, <markup|with> and
    <markup|style-with>, hybrid commands and <LaTeX>-like commands, and the
    general deletion rules for macro applications.

    <item*|<source-link|Edit/Modify/edit_table.hpp|src/Edit/Modify/edit_table.hpp>,
    <source-link|edit_table.cpp|src/Edit/Modify/edit_table.cpp>>The class <cpp|edit_table_rep>: everything
    about tables.

    <item*|<source-link|Edit/Modify/edit_modify.hpp|src/Edit/Modify/edit_modify.hpp>,
    <source-link|edit_modify.cpp|src/Edit/Modify/edit_modify.cpp>>The class <cpp|edit_modify_rep>, which
    receives the modifications and implements undo and redo; see
    <hlink|undo and redo|server-editor.en.tm>.

    <item*|<source-link|Edit/Replace/edit_select.hpp|src/Edit/Replace/edit_select.hpp>,
    <source-link|edit_select.cpp|src/Edit/Replace/edit_select.cpp>>The class <cpp|edit_select_rep>: the
    selection, semantic selections, the clipboard, cutting, the focus and
    alternative selections.

    <item*|<source-link|Edit/Replace/edit_replace.hpp|src/Edit/Replace/edit_replace.hpp>,
    <source-link|edit_search.cpp|src/Edit/Replace/edit_search.cpp>, <source-link|edit_spell.cpp|src/Edit/Replace/edit_spell.cpp>>The class
    <cpp|edit_replace_rep>: structural searches upwards from the cursor, the
    keyboard driven search and replace mode, and the spell checking mode.

    <item*|<source-link|Data/Tree/tree_search.cpp|src/Data/Tree/tree_search.cpp>>The pattern matcher used by
    the search and replace tools (<scm|tree-search-tree-at>).

    <item*|<source-link|Edit/Process/edit_process.hpp|src/Edit/Process/edit_process.hpp>,
    <source-link|edit_process.cpp|src/Edit/Process/edit_process.cpp>>The class <cpp|edit_process_rep>:
    generation of bibliographies, tables of contents, indexes, glossaries
    and lists of figures and tables.

    <item*|<source-link|utils/library/cpp-wrap.scm|TeXmacs/progs/utils/library/cpp-wrap.scm>>The <scheme> wrappers
    <scm|insert>, <scm|make>, <scm|make-with>, the math constructors and
    the clipboard commands.

    <item*|<source-link|generic/generic-edit.scm|TeXmacs/progs/generic/generic-edit.scm>>The generic keyboard
    dispatchers (<scm|kbd-remove>, <scm|kbd-enter>, <scm|kbd-variant>,
    <scm|structured-insert-horizontal>, ...).

    <item*|<source-link|generic/search-widgets.scm|TeXmacs/progs/generic/search-widgets.scm>>The search and replace
    tools and toolbars.

    <item*|<source-link|generic/document-edit.scm|TeXmacs/progs/generic/document-edit.scm>>The <scm|update-document>
    command behind <menu|Document|Update>.

    <item*|<source-link|packages/standard/std-automatic.ts|TeXmacs/packages/standard/std-automatic.ts>>(relative to
    <source-link|src/TeXmacs/|TeXmacs>) The macros which record entries for the
    automatic content with the <markup|write> primitive.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Inserting, deleting and making structure|editing-structure.en.tm>

    <branch|Editing tables|editing-tables.en.tm>

    <branch|Selections, the clipboard, search and
    replace|editing-search.en.tm>

    <branch|Automatic content: tables of contents, indexes and
    glossaries|editing-auxiliary.en.tm>
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
