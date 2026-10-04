<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Help files, the help browser and expansion into books>

  <section|The help path>

  At startup, <verbatim|TEXMACS_DOC_PATH> is set to its value from the
  environment (if any), followed by <verbatim|$TEXMACS_HOME_PATH/doc>,
  <verbatim|$TEXMACS_PATH/doc> and the <verbatim|doc> directories of all
  plug-ins (<verbatim|src/src/System/Boot/init_texmacs.cpp>, see <hlink|boot
  paths|system-boot.en.tm>). Users can therefore add or override pages in
  their home directory, and plug-ins ship their own documentation.

  The routines of <verbatim|progs/doc/help-funcs.scm> work with names
  relative to this path:

  <\description>
    <item*|<scm|url-exists-in-help?>>Tests whether a file (given <em|with>
    its language suffix, for instance <verbatim|"devel/devel.en.tm">) exists
    somewhere in the help path. The Help menu uses it to show only the
    entries whose documentation is installed. The answer is cached for the
    whole session.

    <item*|<scm|url-resolve-help>>(internal) Finds the file for a name
    <em|without> suffix: first <verbatim|<em|name>.<em|xx>.tmml>, then
    <verbatim|<em|name>.<em|xx>.tm>, where <verbatim|<em|xx>> is the two
    letter code of the output language, and finally
    <verbatim|<em|name>.en.tm>. A name which already ends in
    <verbatim|.tm> or <verbatim|.tex> and exists is used as is.

    <item*|<scm|help-file-title>>The title of a help file (the first
    <markup|title>, <markup|doc-title>, <markup|tmdoc-title> or
    <markup|tmweb-title>), cached and re-read when the file changes; used
    for the result lists of searches.
  </description>

  <section|Opening help pages>

  <scm|load-help-buffer>, <scm|load-help-article> and
  <scm|load-help-book> resolve a name with <scm|url-resolve-help> (showing
  an error message if there is none) and open it as a page of type
  <verbatim|normal>, <verbatim|article> and <verbatim|book> respectively.
  The first two go through <scm|tmdoc-expand-help>, which turns the file
  name into <verbatim|tmfs://help/<em|type>/<em|file>> and loads that
  <abbr|URL>; books go through <scm|tmdoc-expand-help-manual>, described
  below. Help menus normally use <scm|load-help-article> for single
  chapters and <scm|load-help-buffer> for the <menu|Browse> entries, so
  that the latter show the page itself, with its list of branches.
  <scm|load-help-online> is meant to open a page of the online
  documentation below <verbatim|https://www.texmacs.org/tmbrowse>; since it
  passes that <abbr|URL> to <scm|load-help-buffer>, which looks names up
  relative to the help path, it probably does not work (not tested). Finally,
  <scm|update-help-online> downloads an archive of the documentation into
  <verbatim|$TEXMACS_HOME_PATH> with <verbatim|wget> from an <verbatim|ftp>
  address.

  The <verbatim|tmfs://help/> handler itself (types, conversions of
  <name|HTML> and <name|TMML> files, the page shown for a missing file, the
  title <verbatim|Help - <em|title>>) is described in <hlink|the
  <TeXmacs> file system handlers|../scheme/api/tmfs/tmfs-handlers.en.tm>.
  Help buffers are read only.

  <section|Expansion>

  For all types except <verbatim|normal>, the handler calls
  <scm|tmdoc-expand> (<verbatim|progs/doc/tmdoc.scm>), which loads the file
  and rewrites its body:

  <\description>
    <item*|<markup|tmdoc-title>>becomes a sectional heading of the current
    <em|level>, followed by a label <verbatim|sec-<em|name>>, where
    <verbatim|<em|name>> is the base name of the file without language and
    suffix (for instance <verbatim|sec-server-buffers> for
    <verbatim|server-buffers.en.tm>). At the top of a book the level is
    <verbatim|title> and the title becomes the title of the book.

    <item*|Sectioning commands>(<markup|section>, <markup|subsection>,
    <markup|subsubsection>, <markup|paragraph>, <markup|subparagraph> and
    their starred variants) are taken relative to the level of the page:
    a <markup|section> becomes the level just below the heading of the
    page, a <markup|subsection> the level below that, and so on
    (<scm|tmdoc-demote>). In a page which becomes a chapter, sections thus
    stay sections; in a page which becomes a section, they become
    subsections. Pages without sectioning commands, like most pages of the
    user manual, are not affected.

    <item*|A heading which introduces the branches>(a heading such as
    <verbatim|Contents of this chapter> which is followed by a
    <markup|traverse> before any other heading) is removed, since the
    expanded branches become its siblings and it would remain empty; the
    text between the heading and the <markup|traverse> is kept.

    <item*|The opening of a page with branches>consists of the headings
    which come before its <markup|traverse>, typically
    <verbatim|Introduction>, <verbatim|Overview> and <verbatim|Source
    files>. A heading which directly follows the title is removed, so that
    its text opens the chapter (or section, ...) of the page; the other
    ones become unnumbered headings without entry in the table of contents
    (<scm|tmdoc-mark-opening>). The numbered divisions of the page are
    thus exactly its branches. In the help browser, where pages are shown
    one by one, the headings are unchanged.

    <item*|<markup|traverse>>is replaced by the expansion of its branches.

    <item*|<markup|branch>>expands the target one level deeper, according
    to the table of <scm|tmdoc-down>: <verbatim|title> and
    <verbatim|part> give <verbatim|chapter>, <verbatim|chapter> gives
    <verbatim|section>, then <verbatim|subsection>,
    <verbatim|subsubsection>, <verbatim|paragraph> and finally
    <verbatim|subparagraph> for all deeper levels. The level of the root
    page of an article is <verbatim|tmdoc-title>, so its branches become
    sections. A deeply nested book may start with parts instead of
    chapters: if the initial environment of its root file sets
    <verbatim|tmdoc-book-parts> to <verbatim|true>, the root level is
    <verbatim|title*>, whose branches become parts. The developer guide
    (<verbatim|devel/source/source.en.tm>) and the whole developer
    documentation (<verbatim|devel/devel.en.tm>) use this.

    <item*|<markup|continue>>expands the target at the <em|same> level and
    drops its title, so that a long page can be split into several files.

    <item*|<markup|extra-branch>>expands the target as an appendix.

    <item*|<markup|optional-branch>, <markup|tmdoc-copyright>,
    <markup|tmdoc-license>>are removed.

    <item*|<markup|hlink> and other links>are rebased: their targets, which
    are relative to the file they appear in, are made relative to the root
    file (<scm|tmdoc-substitute>).
  </description>

  Target names are resolved with <scm|tmdoc-relative>, which follows the
  same rules as the <markup|tmdoc-file> macro (the file itself, the
  translation for the current language, the English file). A table of the
  files already expanded makes sure that each file is included only once,
  even if several pages branch to it; a file which cannot be read is
  replaced by an empty paragraph and a message <verbatim|bad link or file>
  on the console.

  <section|Articles and books>

  For the type <verbatim|article> (and any other type except
  <verbatim|normal> and <verbatim|book>), the expanded body is put in a
  document with the <tmstyle|tmdoc> style and the language of the root
  file. Links stay external.

  For the type <verbatim|book>, three more steps follow:

  <\enumerate>
    <item><scm|tmdoc-internalize> collects all labels of the expanded
    document and turns every hyperlink whose target is part of the book
    (through its label <verbatim|sec-<em|name>> or an explicit
    <verbatim|#<em|label>>) into an internal link. Hyperlinks to targets
    outside the book are replaced by their text.

    <item><scm|tmdoc-add-aux> inserts a table of contents after the title,
    a bibliography if the document contains citations, and an index at
    the end; a chapter called <verbatim|Preface> becomes unnumbered.

    <item>The style is the one given by the preference <verbatim|manual
    style> (default <tmstyle|tmmanual>) and the page medium is
    <verbatim|paper>.
  </enumerate>

  <scm|tmdoc-expand-help-manual> then runs <scm|generate-all-aux> and
  updates the buffer three times, so that the table of contents, the index
  and the references settle, and finally marks the buffer as saved.
  <scm|tmdoc-expand-this> does the same for the current buffer; it is
  behind <menu|Help|Full manuals|Compile article> and <menu|Compile book>,
  which appear when the current document uses <tmstyle|tmdoc>. For
  <name|Mathemagix> documentation it switches the result to the
  <tmstyle|mmxmanual> style. The <menu|Full manuals> menu also contains
  ready-made entries for the user manual, the source code documentation
  and the <scheme> developer guide.

  <section|Building the PDF manuals>

  The command line option <verbatim|-build-manual <em|file>> calls
  <scm|build-manual> (<verbatim|progs/utils/test/test-convert.scm>). The
  file name has the form
  <verbatim|<em|dir>/<em|name>.<em|xx>.pdf>; <verbatim|<em|name>> must be
  one of <verbatim|texmacs-user-manual>, <verbatim|texmacs-reference-manual>
  and <verbatim|texmacs-scheme-manual>, which select
  <verbatim|main/man-manual>, <verbatim|main/man-reference> and
  <verbatim|devel/scheme/scheme>. The manual is expanded as a book in the
  language <verbatim|<em|xx>> (switching the output language if needed)
  and exported to <abbr|PDF>. Other names are silently ignored, and an
  existing <abbr|PDF> file is not rebuilt. Note also that in headless mode
  the automatic <scm|quit-TeXmacs> runs before this command unless
  <verbatim|-X> is given (see <hlink|startup commands|server-startup.en.tm>).

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
