<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Automatic content: tables of contents, indexes and
  glossaries>

  <section|Principle>

  Tables of contents, indexes, glossaries, lists of figures and tables, and
  bibliographies are produced in two steps:

  <\enumerate>
    <item>While the document is typeset, macros such as <markup|toc-entry>,
    <markup|index>, <markup|glossary> or <markup|cite> record entries with
    the <markup|write> primitive. These entries are collected in the
    <em|auxiliary data> of the buffer.

    <item>On request (<menu|Document|Update>), the editor walks through the
    document, empties the body of every automatic section
    (<markup|table-of-contents>, <markup|the-index>, ...) and fills it with
    content computed from the auxiliary data.
  </enumerate>

  The generated content is ordinary document content: it is typeset like
  everything else, and is saved with the document. A second typesetting
  pass is needed afterwards, since the page numbers in the generated
  content are references.

  <section|Auxiliary data>

  The auxiliary data are kept in the hash tables <cpp|aux> and <cpp|ref>
  of the <cpp|new_data_rep> of the buffer (see <hlink|buffers|server-buffers.en.tm>), which are
  shared by all views on the buffer and are saved in the
  <markup|auxiliary> and <markup|references> parts of the file (unless
  the <verbatim|save-aux> variable of the document is false,
  <cpp|get_save_aux>).

  <\description>
    <item*|<cpp|aux>>Maps the name of a list (<verbatim|toc>,
    <verbatim|idx>, <verbatim|gly>, <verbatim|bib>, ...;
    the names are given by variables like <verbatim|toc-prefix> or
    <verbatim|index-prefix> of <source-link|std-automatic.ts|TeXmacs/packages/standard/std-automatic.ts>) to a
    <markup|document> of entries. It is filled by
    <cpp|concater_rep::typeset_write> (<source-link|Typeset/Concat/concat_active.cpp|src/Typeset/Concat/concat_active.cpp>),
    which evaluates the second argument of <markup|write>, removes its
    labels and appends it to the list named by the first argument, but only
    when the typesetting is <em|complete>, that is, when the whole document
    is being typeset. At the start of such a pass, the typesetter replaces
    the table by a fresh one (<source-link|Typeset/Bridge/typesetter.cpp|src/Typeset/Bridge/typesetter.cpp>), so
    that it always reflects the current document.

    <item*|<cpp|ref>>Maps labels to their value and page number; it is
    filled by <markup|label> during typesetting and used to resolve
    <markup|reference> and <markup|pageref>.
  </description>

  The typesetting environment holds references to these tables
  (<cpp|local_aux>, <cpp|local_ref>) and, for documents which belong to a
  project, to the tables of the project buffer (<cpp|global_aux>,
  <cpp|global_ref>), so that a chapter can refer to labels of other
  chapters. The generators described below also read the project tables
  when the buffer has a project (<cpp|buf-\<gtr\>prj>), so that a table of
  contents in the master document lists the entries of all chapters.

  From <scheme>, the tables are accessed with <scm|get-auxiliary>,
  <scm|set-auxiliary>, <scm|list-auxiliaries>, <scm|get-reference>,
  <scm|set-reference>, <scm|list-references> and <scm|find-references>
  (<source-link|build-glue-editor.scm|src/Scheme/Glue/build-glue-editor.scm>); the variants with a star take a flag
  which selects the tables of the project.

  <section|Regeneration>

  <paragraph|The traversal.><cpp|edit_process_rep::generate_aux (which)>
  (<source-link|Edit/Process/edit_process.cpp|src/Edit/Process/edit_process.cpp>) is exported as
  <scm|generate-all-aux> (without argument) and <scm|generate-aux>. It calls
  <cpp|generate_aux_recursively> on the body of the buffer, which looks for
  the automatic tags recognized by <cpp|is_aux>:
  <markup|bibliography> and <markup|bibliography*>,
  <markup|table-of-contents> and <markup|table-of-contents*>,
  <markup|the-index> and <markup|the-index*>, <markup|the-glossary> and
  <markup|the-glossary*>, <markup|list-of-figures> and
  <markup|list-of-tables> (with their exact arities). For each of them it
  assigns an empty <markup|document> to the last argument, puts the cursor
  inside, and, if <cpp|which> is empty or equal to the name of the tag,
  calls the generator for the list named by the first argument; the
  generator inserts its result at the cursor with <cpp|insert_tree>. The
  traversal does not look inside automatic tags. At the end,
  <cpp|init_update> refreshes the document information of the editor.

  <paragraph|Driver.>The menu entries <menu|Document|Update|...> call
  <scm|(update-document <scm-arg|what>)> in
  <source-link|generic/document-edit.scm|TeXmacs/progs/generic/document-edit.scm>. This schedules, as many times as
  the preference <verbatim|document update times> says (at most 5), a
  delayed command which for <verbatim|"all"> regenerates all automatic
  content, empties the caches of included documents and pictures and
  retypesets the buffer; for <verbatim|"bibliography"> does the same
  without the cache cleaning; for <verbatim|"buffer"> only retypesets; and
  otherwise calls <scm|(generate-aux <scm-arg|what>)>. The command is run
  inside <scm|cursor-after>, which restores the cursor afterwards. Since
  regeneration uses the auxiliary data of the <em|previous> typesetting
  pass, and the generated content changes page numbers, several updates may
  be needed before a document is stable.

  <section|The generators>

  <paragraph|Table of contents and lists.><cpp|generate_table_of_contents
  (toc)> inserts the recorded entries as they are, after removing labels
  (<cpp|remove_labels>), since labels inside the generated content would
  be redefinitions of the labels in the document. The entries are calls of
  macros like <markup|toc-1>, <markup|toc-2> or <markup|toc-strong-1>
  with the title and a <markup|pageref> (they are recorded by
  <markup|toc-entry>, called by <markup|toc-normal-1>, <markup|toc-main-1>,
  ...), whose rendering is up to the style.
  <markup|list-of-figures> and <markup|list-of-tables> are generated with
  <cpp|generate_glossary>.

  <paragraph|Index.>An index entry is recorded as a tuple. The forms used
  by <source-link|std-automatic.ts|TeXmacs/packages/standard/std-automatic.ts> are <verbatim|(<em|key> <em|ref>)> for an
  ordinary entry with a page reference, <verbatim|(<em|key> "" <em|text>)>
  for an entry with a fixed text instead of a page, and <verbatim|(<em|key>
  <em|how> <em|range> <em|entry> <em|ref>)> for the complex form, where
  <em|how> may be <verbatim|strong> and <em|range> opens or closes a page
  range. The key is itself a tuple of one to several levels (entry,
  subentry, ...). <cpp|generate_index (idx)>

  <\enumerate>
    <item>computes a sort key for every entry (<cpp|index_name>): the
    letters, digits and spaces of each level, in lower case, levels
    separated by tabs, with a marker <verbatim|*> inserted after the level which
    contains the first capital letter, so that capitalized variants are
    kept apart from the lower case ones; entries whose keys only differ by other characters are
    distinguished by a number (the table <cpp|followup>);

    <item>sorts the keys with the collation of the document language
    (<cpp|std::locale> through <cpp|get_std_locale>, or
    <cpp|CompareStringEx> on <name|Windows>);

    <item>groups the entries with the same key, and inserts missing parent
    entries for subentries (<cpp|insert_recursively>);

    <item>builds one line per key (<cpp|make_entry>), collecting the page
    references separated by commas, turning runs of consecutive pages into
    ranges, and joining explicit range starts and ends. The lines are
    calls of <markup|index-1>, <markup|index-2>, ... (the number is the
    level), or of <markup|index+1>, <markup|index+2>, ... which repeat all
    levels when the document variable <verbatim|index-break-style> is
    <verbatim|recall>; a starred variant is used for entries without
    pages.
  </enumerate>

  <paragraph|Glossary.><cpp|generate_glossary (gly)> turns the entries
  <verbatim|(<em|entry>)>, <verbatim|(normal <em|entry> <em|ref>)> and
  <verbatim|(normal <em|entry> <em|explanation> <em|ref>)> into
  <markup|glossary-1> and <markup|glossary-2> lines, and appends the page
  of a <verbatim|(dup <em|entry> <em|ref>)> entry to the line of the same
  entry. Unlike the index, the glossary keeps the order of the document.

  <paragraph|Bibliography.><cpp|generate_bibliography (bib, style,
  file)> is described in detail in <hlink|the database and
  bibliographies|database-bibliography.en.tm>. The list
  <cpp|aux[bib]> contains the keys cited in the document, written by
  <markup|cite> and <markup|nocite>.

  <section|Pitfalls>

  <\itemize>
    <item><menu|Document|Update|Index> and <menu|Document|Update|Glossary>
    do not work. The menu (<verbatim|generic/document-menu.scm:124-125>)
    calls <scm|(update-document "index")> and <scm|(update-document
    "glossary")>, which pass the strings <verbatim|"index"> and
    <verbatim|"glossary"> to <cpp|generate_aux>, while
    <cpp|generate_aux_recursively> compares them with the tag names
    <verbatim|"the-index"> and <verbatim|"the-glossary">
    (<verbatim|Edit/Process/edit_process.cpp:613-618>). Combined with the
    next item, the effect is that all automatic content of the document is
    emptied and none is regenerated.

    <item><cpp|generate_aux_recursively> empties <em|every> automatic
    section (<source-link|edit_process.cpp:594|src/Edit/Process/edit_process.cpp:594>) before testing whether it
    matches <cpp|which>. A restricted update such as
    <menu|Document|Update|Table of contents> (or <scm|(generate-aux
    "bibliography")>) therefore regenerates the requested section but leaves
    all the other ones empty.

    <item>The auxiliary data are only recorded during a complete
    typesetting of the document. If the document has never been typeset
    entirely in the current session (for instance because only some parts
    are shown, see <markup|show-part>), or if the auxiliary data were not
    saved (<verbatim|save-aux>), the generated content is empty until the
    document has been typeset once more.

    <item>The generators insert their result at the cursor and leave the
    cursor in the last regenerated section; callers which care should save
    and restore the cursor, as <scm|update-document> does with
    <scm|cursor-after>.
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
