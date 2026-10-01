<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Inclusions>

  <section|The two inclusion tags>

  The kernel knows two inclusion primitives, both with one argument, the
  name of the included file (<verbatim|Data/Drd/drd_std.cpp> gives it the
  type <abbr|URL>):

  <\description>
    <item*|<markup|include> (<cpp|INCLUDE>)>Typeset by
    <cpp|concater_rep::typeset_include>
    (<verbatim|Typeset/Concat/concat_macro.cpp>). The typesetter loads the
    file with <cpp|load_inclusion>, typesets the result with
    <cpp|typeset_dynamic>, and sets the current file name and the
    <cpp|secure> flag of the environment to those of the included file
    during that time.

    <item*|<markup|include*> (<cpp|VAR_INCLUDE>)>A <em|rewriting>
    primitive, like <markup|extern> and <markup|with-package>: the
    environment replaces it by the contents of the file
    (<cpp|edit_env_rep::rewrite>, <verbatim|Typeset/Env/env_exec.cpp>), and
    the bridge typesets the rewritten tree
    (<cpp|bridge_rewrite_rep::my_typeset>,
    <verbatim|Typeset/Bridge/bridge_rewrite.cpp>), again with the current
    file name and the <cpp|secure> flag of the included file. An
    inclusion which resolves to the base file name (the master, see
    below) is replaced by the error <verbatim|"invalid self include">.
    Since it is a rewriting, the inclusion is expanded again whenever its
    bridge is retypeset (the file itself comes from the cache described
    below).
  </description>

  In practice, documents use <markup|include>, but the standard style
  redefines it as a macro (<verbatim|packages/standard/std-automatic.ts>):

  <\verbatim-code>
    \<less\>assign\|include\|\<less\>macro\|name\|\<less\>surround\|\<less\>part-info\|\<less\>arg\|name\<gtr\>\<gtr\>\|\|\<less\>include*\|\<less\>arg\|name\<gtr\>\<gtr\>\<gtr\>\<gtr\>\<gtr\>
  </verbatim-code>

  so that every inclusion records its starting page and the current
  counters for <hlink|projects|multifile-projects.en.tm>, and then expands
  to <markup|include*>. The primitive <markup|include> and
  <cpp|typeset_include> are only used with styles which do not load
  <verbatim|std-automatic>.

  Neither form makes the included material part of the edit tree of the
  master: it is displayed, but the cursor cannot enter it and modifications
  have to be made in the included file itself, or in a <hlink|part
  view|multifile-parts.en.tm>.

  <section|Resolution of file names>

  The argument is evaluated to a string, interpreted as a <name|Unix> style
  relative or absolute name, and resolved with <cpp|relative
  (env-\<gtr\>base_file_name, file_name)>. The base file name is the
  <em|master> of the buffer (<cpp|edit_typeset_rep::typeset_prepare>), that
  is, its own name for an ordinary buffer.

  The base file name is not changed while an included file is typeset;
  only the current file name is, and the latter is only used for the
  <cpp|secure> flag. Hence all relative names inside an included file
  (nested inclusions, images, sounds and videos, which are all resolved
  against <cpp|base_file_name>) are interpreted relative to the directory
  of the <em|master>, not of the included file. Books whose chapters live
  in subdirectories must therefore use names relative to the master in
  the chapters.

  <section|The inclusion cache>

  All inclusions go through <cpp|load_inclusion (url)>
  (<verbatim|Texmacs/Data/new_buffer.cpp>), which keeps a global table
  <cpp|document_inclusions> from the file name (as a string) to the
  included tree:

  <\enumerate>
    <item>If the name is in the table, the cached tree is returned.

    <item>Otherwise the file is imported with <cpp|import_tree (name,
    "generic")>, so that any format with a converter can be included, and
    reduced to a body by <cpp|extract_document>
    (<verbatim|Data/Convert/Texmacs/fromtm.cpp>).
    <cpp|extract_document> wraps the body in a <markup|with> which sets the
    initial environment of the included file, except for the page layout
    variables and, when the included file belongs to a project, its page
    and sectional counters.

    <item>The result is cached unless it is an error.
  </enumerate>

  The cache is shared by all buffers and is never invalidated
  automatically. It is emptied by <cpp|reset_inclusions>, which is only
  called by <cpp|tm_server_rep::inclusions_gc> (<scm|inclusions-gc>,
  <menu|Tools|Update|Inclusions> and <menu|Document|Update|All>);
  <cpp|inclusions_gc> then retypesets all documents. The routine
  <cpp|reset_inclusion (url)>, which would forget a single file, exists
  but is not called anywhere. The same cache is used by the <scheme>
  functions <scm|tree-load-inclusion> and <scm|tree-inclusion> (two glue
  names for <cpp|load_inclusion>).

  <section|Inclusions on the <scheme> side>

  <\description>
    <item*|<scm|tm-get-includes>, <scm|buffer-get-includes>,
    <scm|buffer-contains-includes?>>(<verbatim|generic/document-part.scm>)
    List the names of the files included by a document, looking through
    <markup|document> and <markup|with> nodes. The list drives the
    <menu|Document|Part> menu of a master (<scm|document-master-menu> in
    <verbatim|part/part-menu.scm>), each entry of which opens the included
    file as a part view.

    <item*|<scm|include-list>, <scm|project-file-list>>(<verbatim|generic/document-menu.scm>)
    The same for the <menu|Document|Project> menu, but only at the top
    level of the master and with resolved <abbr|URL>s.

    <item*|<scm|buffer-expand-includes>><menu|Tools|Project|Expand
    inclusions> flattens a master into a single document: each
    <markup|include> which is a paragraph of a <markup|document> (at any
    depth) is replaced by the paragraphs
    of the included body (taken from the cache).
  </description>

  <section|Pitfalls>

  <\itemize>
    <item><em|Stale inclusions.> Saving an included file does not refresh
    the documents which include it: they keep showing the cached version
    until <menu|Tools|Update|Inclusions> or <menu|Document|Update|All>.
    Using <cpp|reset_inclusion> on save would fix this.

    <item><em|Names are relative to the master.> As explained above,
    nested inclusions and images in an included file are resolved with
    respect to the master's directory.

    <item><em|Cycles are only partly detected.> Since all names are
    resolved relative to the master, <markup|include*> catches any attempt
    to include the master itself, directly or from a chapter. A cycle
    which does not pass through the master (<verbatim|b.tm> including
    <verbatim|c.tm> including <verbatim|b.tm>) is not detected, and the
    plain primitive <markup|include> has no guard at all. Such cycles
    recurse without bound while typesetting. (Found by reading the code,
    not tested.)

    <item><em|Expanding inclusions drops their settings.>
    <scm|buffer-expand-includes> keeps only the paragraphs of the
    included body (<scm|inclusion-children> strips the <markup|with>
    created by <cpp|extract_document>), so initial environment settings of
    the chapters are lost; it also does not expand <markup|include*>, nor
    inclusions which are not a paragraph of a <markup|document> node (for
    instance the direct body of a <markup|with>).
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
