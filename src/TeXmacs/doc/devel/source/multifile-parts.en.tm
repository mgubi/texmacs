<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Document parts and part views>

  The word <em|part> is used for two different features. <em|In-buffer
  parts> let the user show only some of the principal sections of a long
  document, or only its preamble. <em|Part views> are documents of the
  <TeXmacs> file system, <verbatim|tmfs://part/...>, which present a file of
  a multi-file document together with the context of its master, and in
  which the inclusions are expanded and editable.

  <section|In-buffer parts and the preamble mode>

  In-buffer parts are implemented entirely in <scheme>, in
  <verbatim|generic/document-part.scm>, as rewritings of the top level
  <markup|document> of the buffer. The comment at the head of the file
  gives the representations of the four <em|part modes>:

  <\description>
    <item*|<verbatim|:preamble>><verbatim|(document (show-preamble
    <em|preamble>) (ignore <em|body>))>: only the preamble (the macro
    definitions of the document) is visible and editable.

    <item*|<verbatim|:one>, <verbatim|:several>><verbatim|(document
    [(hide-preamble <em|preamble>)] <em|parts>...)>, where each part is
    <verbatim|(show-part <em|id> <em|body> <em|alt-body>)> or
    <verbatim|(hide-part <em|id> <em|body> <em|alt-body>)>.

    <item*|<verbatim|:all>><verbatim|(document [(hide-preamble
    <em|preamble>)] <em|body>...)>: the ordinary form.
  </description>

  The macros are defined in <verbatim|packages/standard/std-fold.ts>.
  <markup|show-part> typesets its body inside <markup|set-part> (which
  extends the variable <verbatim|current-part> and resets
  <verbatim|auto-nr>, <verbatim|std-automatic.ts>); <markup|hide-part>
  does the same inside <markup|hidden>, choosing the body or the
  <em|alt-body> according to <markup|sectional-short-style>. Hidden parts
  are still evaluated, so that counters and labels remain correct, and
  <markup|hide-part> is declared with <verbatim|hidden> child 1 in its
  <abbr|DRD> properties. <markup|hide-preamble> shows nothing but still
  executes the definitions, through <markup|filter-style>.

  The parts are created by <scm|buffer-make-parts>, which calls
  <scm|principal-sections-to-document-parts>
  (<verbatim|text/text-structure.scm>): the paragraphs are split at each
  principal section (chapters in a book, sections in an article), the
  pieces are numbered <verbatim|"1">, <verbatim|"2">, ... as identifiers,
  and the <em|alt-body> is the section heading alone. Parts are referred to
  in the menus by the title of their first section
  (<scm|document-part-name>, <scm|buffer-parts-list>).
  <scm|buffer-flatten-parts> undoes the splitting.

  The main entry points are <scm|buffer-set-part-mode> (with a check mark
  for the current mode), <scm|buffer-show-part> (mode <verbatim|:one>),
  <scm|buffer-toggle-part> (mode <verbatim|:several>), <scm|buffer-go-to-part>,
  <scm|toggle-preamble-mode> and <scm|buffer-make-preamble>. They make up the
  <menu|Document|Part> menu (<scm|document-part-menu>). When the cursor ends
  up inside a hidden part, for instance after a search,
  <scm|cursor-show-hidden> (<verbatim|utils/edit/variants.scm>) calls the
  <scm|tree-show-hidden> overload of <verbatim|document-part.scm>, which
  shows the part.

  <section|Part views>

  <subsection|Names>

  A part view is named

  <\verbatim-code>
    tmfs://part/<em|master>

    tmfs://part/<em|master>/<em|file>
  </verbatim-code>

  where <em|master> is the <abbr|URL> of the master (as a <TeXmacs> file
  system string) and <em|file> is either a name relative to the master,
  prefixed with <verbatim|here/>, or an absolute one. <scm|part-url>,
  <scm|part-master>, <scm|part-file> and <scm|part-open-name>
  (<verbatim|part/part-tmfs.scm>) build and decompose such names; the split
  is made at the first occurrence of <verbatim|.tm/>, so the second form
  only works for masters with the suffix <verbatim|.tm> (and in
  directories whose names do not contain <verbatim|.tm/>). In a master with inclusions,
  <menu|Document|Part> lists the included files
  (<scm|document-master-menu>, <verbatim|part/part-menu.scm>), and choosing
  one opens <scm|(part-url master file)>.

  The handlers registered for the <verbatim|part> protocol are documented
  in general in <hlink|the TeXmacs file
  system|../scheme/api/tmfs/tmfs-handlers.en.tm>. The master handler returns
  the real file, so that links and relative names in the view are resolved
  as in the file; the title is <verbatim|<em|master> - <em|file>>.

  <subsection|Loading>

  The load handler reads the file and the master and builds a document with
  <scm|part-expand>:

  <\itemize>
    <item>In the body, every <markup|include> or <markup|include*> with a
    literal file name is replaced by <verbatim|(shared <em|uid> <em|name>
    <em|body>)>, where <em|uid> is a fresh unique identifier, <em|name> the
    <name|Unix> name of the included file and <em|body> its body. If the
    view is not the master itself, the whole body is also wrapped in a
    <markup|shared> tag for the file.

    <item>The <markup|style>, <markup|references> and <markup|auxiliary>
    sections are taken from the <em|master>, so that the view is typeset
    with the master's style and resolves references to the whole book.

    <item>The initial environment is that of the master, without
    <verbatim|preamble>, <verbatim|mode>, <verbatim|page-medium>,
    <verbatim|page-printed> and <verbatim|page-first>, plus
    <verbatim|part-flag> set to <verbatim|true>, the counters recorded by
    <markup|part-info> in the master's <verbatim|parts> channel, the first
    page from the label <verbatim|part:<em|file>>, and finally the file's own
    initial environment (<scm|master-inits>). <verbatim|part-flag> makes
    the editor use a copy of its references as global table; see
    <hlink|projects|multifile-projects.en.tm>.
  </itemize>

  <subsection|Saving>

  The save handler performs the inverse transformation
  (<scm|part-compress>): it removes the outer <markup|shared> wrapper,
  turns each remaining <markup|shared> back into an <markup|include> with a
  name relative to the file, stores only the <em|differences> between the
  edited initial environment and the inherited one, and takes the
  <markup|style>, <markup|references> and <markup|auxiliary> sections from
  the file as it is on disk. The result is written to the real file, which
  is then marked as saved (or modified, if writing failed).

  <subsection|Synchronization of shared material>

  The <markup|shared> macro (<verbatim|std-automatic.ts>) wraps its body in a
  locus with the identifier <em|name> and an observer which calls the
  <scheme> function <scm|mirror-notify> (<verbatim|part/part-shared.scm>) on
  each modification. The same machinery serves the <markup|mirror> tag of
  <verbatim|packages/utilities/relate.ts> (live copies of a piece of
  document, made with <scm|make-mirror>) and several comment tags. The
  general link and observer mechanism is described in <hlink|the link
  kernel|links-kernel.en.tm>; here is what happens on top of it:

  <\enumerate>
    <item>When the body of a <markup|shared> tag is modified,
    <scm|mirror-notify> calls <scm|buffer-initialize-shared>. If a buffer
    with the included file's name is open, it attaches a notifier to that
    buffer (<scm|buffer-attach-notifier>, see <hlink|metadata: buffers,
    views, windows and projects|server-layer-metadata.en.tm>) and marks it
    for an initial copy.

    <item>The modification is queued for that buffer
    (<scm|buffer-notify-shared>) and, at the next idle moment,
    <scm|buffer-treat-pending> applies the queued modifications to the
    buffer body with <scm|modification-apply!>. If one of them does not
    apply, the buffer body is replaced by a copy of the shared tree
    (<scm|buffer-restore>).

    <item>Conversely, modifications of the open file buffer reach
    <scm|buffer-notify> through its notifier and are queued, with
    <scm|mirror-list-notify>, for the other trees carrying the same
    identifier, that is, for the <markup|shared> tags in the open part
    views.

    <item>Between copies of a <markup|mirror> or <markup|shared> body,
    the same queue is used (<scm|mirror-treat-pending>). Copies which fall
    out of step are put on a black list, separated by giving them new
    unique identifiers if needed (<scm|mirror-separate>) and resynchronized
    by copying (<scm|mirror-synchronize>). All these changes are made under
    a dedicated author (<scm|mirror-author>), with
    <scm|mirror-idle?> set to false so that they are not echoed back.
  </enumerate>

  <section|Pitfalls>

  <\itemize>
    <item><em|Edits of included material can be lost.> Modifications made
    to an expanded inclusion in a part view are forwarded only to an
    <em|open> buffer of the included file. If that file is not open,
    <scm|buffer-initialize-shared> does nothing (the code which would load
    it in the background is commented out), and saving the view turns the
    <markup|shared> tag back into a plain <markup|include>, so the changes
    are silently discarded. If the file is open, its buffer has to be saved
    separately. (Found by reading the code, not tested.)

    <item><em|Initial synchronization of <markup|shared> never runs.> The
    <markup|shared> macro passes <verbatim|\<less\>quote-arg\|xbody\<gtr\>> to
    <scm|mirror-initialize> (<verbatim|std-automatic.ts:329>), but the
    macro has no argument <verbatim|xbody>; the <markup|mirror> macro passes
    <verbatim|\<less\>quote-arg\|body\<gtr\>> (<verbatim|relate.ts:31>).
    <scm|mirror-initialize> therefore never recognizes the body of a
    <markup|shared> tag, and copies which differ when a view is opened are
    not brought in sync until they are modified.

    <item><em|Style and references of a part view are not saved.> The save
    handler takes these sections from the file on disk, so changing the
    style in a part view has no effect on the file.

    <item><em|The part mode is global.> <scm|part-mode> is a single
    variable of <verbatim|document-part.scm>, shared by all buffers:
    choosing <verbatim|:several> in one document changes the meaning of the
    part commands in all others.

    <item><em|Two naming schemes for in-buffer parts.> Parts are identified
    by their number in the <markup|show-part> and <markup|hide-part> tags,
    but by their title in the menus and in <scm|buffer-show-part> and
    <scm|buffer-toggle-part>; <scm|show-hidden-part>, in contrast, expects
    the number. Two parts with the same title cannot be selected
    separately from the menu.
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
