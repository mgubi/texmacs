<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Multi-file documents: projects, inclusions and document
  parts>

  <section|Introduction>

  Long documents such as books are rarely kept in a single file. <TeXmacs>
  offers three mechanisms for working with documents which consist of several
  pieces, and they are often used together:

  <\description>
    <item*|Inclusions>The <markup|include> tag inserts the body of another
    file into a document. The master file of a book typically consists of a
    preamble and one <markup|include> per chapter.

    <item*|Projects>A chapter file can be <em|attached> to a master file.
    When the chapter is edited on its own, it then uses the references,
    auxiliary data, chapter and page numbers of the master, so that
    cross references between chapters work.

    <item*|Document parts>A single document can be split into parts
    (usually at its principal sections) of which only some are shown, and a
    document with inclusions can be opened as a <em|part view>
    <verbatim|tmfs://part/...> in which the included files are expanded
    and can be edited in place.
  </description>

  This chapter describes how these mechanisms are implemented. It assumes
  familiarity with the buffer data (<cpp|new_data_rep>) and the
  <cpp|prj> field of buffers, described in <hlink|buffers|server-buffers.en.tm>, with the reference and
  auxiliary tables of <hlink|links, loci and references|links.en.tm>, and
  with the generation of automatic content in <hlink|structured editing,
  search and automatic content|editing-auxiliary.en.tm>.

  <section|Overview>

  The three mechanisms live at different levels:

  <\itemize>
    <item><em|Inclusions> are a typesetting feature. In the standard styles
    <markup|include> is a macro (<verbatim|std-automatic.ts>) which records
    some information about the included file with the macro
    <markup|part-info> and expands to the primitive <markup|include*>. The
    primitive is evaluated like a macro: the typesetter loads the included
    file through a global cache (<cpp|load_inclusion>) and typesets its
    body as if it were part of the master. The included material is not
    part of the edit tree of the master.

    <item><em|Projects> are a buffer feature. A buffer whose data declare a
    <verbatim|project> has its <cpp|prj> field set to the buffer of the
    master, which is loaded in the background. The typesetting environment
    of the chapter reads labels and auxiliary data first from the chapter
    and then from the master, and <cpp|init_update> copies chapter and page
    numbers recorded by the master into the initial environment of the
    chapter.

    <item><em|Document parts> are a <scheme> feature. In-buffer parts are
    ordinary markup (<markup|show-part>, <markup|hide-part>,
    <markup|show-preamble>, <markup|hide-preamble>) manipulated by
    <verbatim|generic/document-part.scm>. Part views are documents of the
    <TeXmacs> file system, produced and saved by the handlers of
    <verbatim|part/part-tmfs.scm>; the expanded inclusions are wrapped in
    <markup|shared> tags, whose modifications are forwarded to the
    corresponding buffers by <verbatim|part/part-shared.scm>.
  </itemize>

  The following picture summarizes the data flow for a book
  <verbatim|book.tm> with a chapter <verbatim|ch1.tm>:

  <\verbatim-code>
    book.tm\ \ (master,\ project-flag\ =\ true)

    \ \ \<less\>include\|ch1.tm\<gtr\>

    \ \ \ \ part-info:\ \ label\ part:ch1.tm,\ write\ parts\ (chapter-nr,\ ...)

    \ \ \ \ include*:\ \ \ load_inclusion\ (ch1.tm),\ typeset\ as\ part\ of\ book.tm

    \;

    ch1.tm\ \ \ (project\ =\ "book.tm")

    \ \ buffer-\<gtr\>prj\ \ \ =\ buffer\ of\ book.tm\ (loaded\ in\ the\ background)

    \ \ labels\ \ \ \ \ \ \ \ =\ own\ table\ first,\ then\ the\ table\ of\ book.tm

    \ \ init_update\ \ \ :\ chapter-nr,\ page-first,\ ...\ from\ book.tm

    \;

    tmfs://part/.../book.tm\ \ \ (part\ view)

    \ \ includes\ expanded\ into\ \<less\>shared\|uid\|ch1.tm\|body\<gtr\>

    \ \ edits\ of\ body\ forwarded\ to\ the\ buffer\ ch1.tm\ when\ it\ is\ open
  </verbatim-code>

  <section|Source files>

  Paths are relative to <verbatim|src/src/> for <c++> files and to
  <verbatim|src/TeXmacs/> for the others.

  <\description>
    <item*|<verbatim|Texmacs/Data/new_project.cpp>>Attaching a project to
    the current buffer, implicit projects, <cpp|project_get>.

    <item*|<verbatim|Texmacs/Data/new_buffer.cpp>>Loading the project
    buffer in <cpp|set_buffer_tree>; the inclusion cache
    (<cpp|load_inclusion>, <cpp|reset_inclusions>, <cpp|reset_inclusion>).

    <item*|<verbatim|Edit/Editor/edit_typeset.cpp>>Binding of the
    reference and auxiliary tables of the master
    (<cpp|edit_typeset_rep> constructor), <cpp|init_update>,
    <cpp|clear_local_info>.

    <item*|<verbatim|Typeset/Env/env_exec.cpp>,
    <verbatim|Typeset/Bridge/bridge_rewrite.cpp>>Evaluation and
    typesetting of <markup|include*>.

    <item*|<verbatim|Typeset/Concat/concat_macro.cpp>>Typesetting of the
    <markup|include> primitive (<cpp|typeset_include>).

    <item*|<verbatim|Data/Convert/Texmacs/fromtm.cpp>>
    <cpp|extract_document>, which turns an included file into a body.

    <item*|<verbatim|Texmacs/Server/tm_server.cpp>>
    <cpp|inclusions_gc>.

    <item*|<verbatim|packages/standard/std-automatic.ts>>The macros
    <markup|include>, <markup|part-info> and <markup|shared>.

    <item*|<verbatim|packages/standard/std-fold.ts>>The macros
    <markup|show-part>, <markup|hide-part>, <markup|show-preamble> and
    <markup|hide-preamble>.

    <item*|<verbatim|progs/generic/document-part.scm>>Preamble mode,
    in-buffer document parts, the list of inclusions, expanding
    inclusions, the <menu|Document|Part> and project management menus.

    <item*|<verbatim|progs/generic/document-menu.scm>>The
    <menu|Document|Project> menu and the update commands.

    <item*|<verbatim|progs/part/part-tmfs.scm>>The <verbatim|part> handlers
    of the <TeXmacs> file system.

    <item*|<verbatim|progs/part/part-shared.scm>>Synchronization of
    <markup|shared> and <markup|mirror> tags with each other and with whole
    buffers.

    <item*|<verbatim|progs/part/part-menu.scm>>The menus which open the
    included files as part views.
  </description>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Projects|multifile-projects.en.tm>

    <branch|Inclusions|multifile-inclusions.en.tm>

    <branch|Document parts and part views|multifile-parts.en.tm>
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
