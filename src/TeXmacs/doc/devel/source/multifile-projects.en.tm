<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Projects>

  A <em|project> consists of a master file and a number of chapter files.
  The master includes the chapters with <markup|include> (see <hlink|the
  next page|multifile-inclusions.en.tm>); each chapter which should be
  editable on its own is <em|attached> to the master. The buffer level
  routines (<cpp|project_attach>, <cpp|project_attached>,
  <cpp|project_get>, implicit projects) are described in <hlink|metadata:
  buffers, views, windows and projects|server-layer-metadata.en.tm>; this
  page explains what attaching a project actually changes.

  <section|Declaring masters and chapters>

  <\description>
    <item*|The master>A buffer is a master (an <em|implicit project>) if
    its file name ends in <verbatim|.tp>, or if the initial variable
    <verbatim|project-flag> is <verbatim|true>. The latter is toggled by
    <menu|Tools|Project|Use as master> (<scm|buffer-toggle-master> in
    <verbatim|generic/document-part.scm>, which calls <scm|init-env>).

    <item*|A chapter><menu|Tools|Project|Attach master> asks for a file and
    calls <scm|project-attach> with its name relative to the current buffer
    (<scm|project-attach*>). The <c++> routine <cpp|project_attach>
    (<verbatim|Texmacs/Data/new_project.cpp>) stores the name in the
    <cpp|project> field of the buffer data, re-initializes the editors
    (<cpp|init_update>, <cpp|notify_change (THE_DECORATIONS)>), marks the
    buffer as modified and loads the master buffer into <cpp|buf-\<gtr\>prj>
    with <cpp|concrete_buffer_insist>. <menu|Tools|Project|Detach master>
    (<scm|project-detach>) calls the same routine with an empty name. The
    name is saved in the <markup|project> part of the document.
  </description>

  When a chapter is loaded later, <cpp|set_buffer_tree>
  (<verbatim|new_buffer.cpp>) sees the <verbatim|project> attribute and loads
  the master in the background, without any view. The master therefore must
  exist on disk; unless it is already open, its references are those of its
  last save.

  <section|Shared reference and auxiliary tables>

  The typesetting environment of an editor holds <em|two> tables of each
  kind: a local one and a global one. They are bound in the constructor of
  <cpp|edit_typeset_rep> (<verbatim|Edit/Editor/edit_typeset.cpp>):

  <\description>
    <item*|References>local: <cpp|buf-\<gtr\>data-\<gtr\>ref>; global: the
    references of the master (<cpp|buf-\<gtr\>prj-\<gtr\>data-\<gtr\>ref>)
    if there is a project, otherwise the private table <cpp|grefs>.

    <item*|Auxiliary data>local: <cpp|buf-\<gtr\>data-\<gtr\>aux>; global:
    the auxiliary data of the master, or the local table again.

    <item*|Attachments>likewise, with <cpp|att>.
  </description>

  The two kinds of tables are used asymmetrically:

  <\itemize>
    <item><em|Writes always go to the local table>: labels
    (<verbatim|Typeset/Env/env_exec.cpp>), page numbers of labels
    (<verbatim|Typeset/Bridge/typesetter.cpp>) and <markup|write> entries
    (<verbatim|Typeset/Concat/concat_active.cpp>).

    <item><em|Reads try the local table first, then the global one>:
    <cpp|exec_get_binding> and <cpp|exec_has_binding> (the primitives
    behind references) for labels, and the lookup of attachments
    (<verbatim|env_exec.cpp>). The global auxiliary table is only read by
    <cpp|init_update>, for the <verbatim|parts> entry described below;
    automatic content such as the table of contents of a chapter is
    always taken from the chapter itself.
  </itemize>

  When the master is typeset, its chapters are typeset as part of it
  through <markup|include>, so the master's own table collects the labels of
  all chapters; saving the master stores them in its
  <markup|references> section. When a chapter is typeset on its own, its
  local table only contains its own labels, and a reference to a label in
  another chapter is found in the master's table. Consequently, cross
  references between chapters are only correct after the master has been
  typeset and saved; updating a chapter does not update the master.

  A buffer which has no project but whose initial variable
  <verbatim|part-flag> is <verbatim|true> gets a <em|copy> of its own
  references as global table (<cpp|grefs>, set by <cpp|init_update>). This
  is the case of the part views of <hlink|the last page|multifile-parts.en.tm>,
  whose references are those of the master.

  <section|Numbering of chapters and pages>

  The <markup|include> macro of the standard style calls
  <markup|part-info> with the name of the included file, which does two
  things while the master is typeset (<verbatim|std-automatic.ts>):

  <\itemize>
    <item>it sets the label <verbatim|part:<em|name>>, whose page number
    is the page on which the inclusion starts;

    <item>it writes an entry <verbatim|(tuple <em|name> chapter-nr
    <em|n> section-nr <em|m> subsection-nr <em|k>)> to the auxiliary
    channel <verbatim|parts>, recording the counters before the inclusion.
  </itemize>

  <cpp|edit_typeset_rep::init_update> uses this information when a chapter
  is (re)initialized: it computes the name of the chapter relative to the
  master (<cpp|delta (prj-\<gtr\>buf-\<gtr\>name, buf-\<gtr\>name)>), looks for
  an entry with that name in the master's <verbatim|parts> and copies the
  recorded counters, as well as the page of <verbatim|part:<em|name>>
  (into <verbatim|page-first>), to the initial environment of the editor
  <em|and> of the buffer data. As a result, a chapter edited on its own is
  numbered as in the book.

  Conversely, when a chapter is included, <cpp|extract_document>
  (<verbatim|Data/Convert/Texmacs/fromtm.cpp>) keeps its initial
  environment as a <markup|with> around the body, but drops the page layout
  variables and, if the included file belongs to a project, its
  <verbatim|page-first> and chapter and section counters, so that the copies
  stored in the chapter do not override the numbering of the master.

  <section|Menus and updates>

  <\description>
    <item*|<menu|Document|Project>>Shown when a project is attached
    (<verbatim|texmacs/menus/main-menu.scm>). <scm|project-menu>
    (<verbatim|generic/document-menu.scm>) offers the master and the list
    of files included by the master (<scm|project-file-list>, computed by
    <scm|include-list> from the master's tree).

    <item*|<menu|Tools|Project>><scm|project-manage-menu>: use as master,
    expand inclusions, attach and detach the master.

    <item*|<menu|Document|Update|Clear local information>>Only shown for a
    chapter; <cpp|clear_local_info> empties the local reference and
    auxiliary tables of the buffer, so that only the master's information
    is used.
  </description>

  The other update commands (<scm|update-document>) act on the current
  buffer only. To refresh the cross references of a book, update and save
  the master, then reopen or re-initialize the chapters.

  <section|Pitfalls>

  <\itemize>
    <item><em|The master's tables are bound once.> The references to the
    master's tables are taken when the editor is created; attaching or
    detaching a project in an open view changes <cpp|buf-\<gtr\>prj> but not
    the tables used by the typesetter until the view is recreated (see
    <hlink|links pitfalls|links-pitfalls.en.tm>). If the master buffer were
    closed while a chapter is open, the chapter's environment would refer
    to freed tables.

    <item><em|Names must match literally.> <cpp|init_update> compares the
    chapter name computed with <cpp|delta> with the name written by
    <markup|part-info>, which is the argument of the <markup|include> tag
    as written in the master. An inclusion written as
    <verbatim|./ch1.tm>, or through another relative path to the same
    file, is not matched, and the chapter then keeps its own numbering.

    <item><em|The project file list is shallow.> <scm|include-list> only
    looks at <markup|include> tags directly in the master's top level
    <markup|document>; inclusions inside a <markup|with> or another tag are
    not listed in <menu|Document|Project>. (The similar <scm|tm-get-includes>
    of <verbatim|document-part.scm> does descend into <markup|with>.)

    <item><em|Chapters write to the buffer data during initialization.>
    <cpp|init_update> stores the counters in
    <cpp|buf-\<gtr\>data-\<gtr\>init>, so they are saved with the chapter,
    even if they were only meant to be inherited from the master.
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
