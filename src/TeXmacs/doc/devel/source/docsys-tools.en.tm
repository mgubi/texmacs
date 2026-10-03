<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Web site, search and API documentation>

  <section|The web site generator>

  The <TeXmacs> web site is built from a directory tree of <TeXmacs>
  documents by <verbatim|progs/doc/tmweb.scm>. The entry points are

  <\description-paragraphs>
    <item*|<scm|tmweb-convert-dir>, <scm|tmweb-update-dir>>Convert, or
    update, the tree <scm-arg|tm-dir> into <scm-arg|html-dir>. They are
    called by the command line options <verbatim|-W <em|in> <em|out>> and
    <verbatim|-U <em|in> <em|out>> (see <hlink|command line
    options|server-startup.en.tm>).

    <item*|<scm|tmweb-convert-dir-keep-texmacs>,
    <scm|tmweb-update-dir-keep-texmacs>>The same, but the <verbatim|.tm>
    files are copied as well.

    <item*|<scm|tmweb-interactive-build>, <scm|tmweb-interactive-update>,
    <scm|open-website-builder>>Interactive variants which ask for the two
    directories; the last one opens a dialog and remembers the directories
    in the preferences <verbatim|website:src-dir> and
    <verbatim|website:dest-dir>. They are declared lazily in
    <verbatim|init-texmacs.scm> but no menu calls them.
  </description-paragraphs>

  <scm|tmweb-convert-directory> enumerates all files below the source
  directory. Every <verbatim|.tm> file is exported to <name|HTML> (or
  <name|XHTML>, if the preference <verbatim|texmacs-\<gtr\>html:mathml> is
  on) with <scm|export-buffer-main>, at the same relative place in the
  destination; the auxiliary data are generated first (<scm|with-aux>).
  All other files (images, style sheets, ...) are copied, except backup
  files, files below hidden directories and version control directories.
  Missing destination directories are created. In update mode a file is
  only converted or copied if the destination is missing or older than the
  source (<scm|url-newer?>); a known problem of <scm|url-newer?> with the
  file attribute cache can make updates skip changed files (see <hlink|the
  system layer|system-files.en.tm>).

  The <name|HTML> conversion itself is described in <hlink|the
  <name|HTML> export|convert-html-export.en.tm>; the web pages use the
  <tmstyle|tmweb> styles, whose packages add the navigation bars and the
  classification of pages (<scm|tmweb-insert-classifiers>).

  <section|Full text search>

  <menu|Help|Search|Documentation> and the neighbouring entries call
  <scm|docgrep-in-doc>, <scm|docgrep-in-src> and <scm|docgrep-in-recent>
  (<verbatim|progs/doc/docgrep.scm>). They open
  <verbatim|tmfs://grep/type=<em|type>&what=<em|words>>, whose handler
  collects the candidate files and ranks them:

  <\description>
    <item*|<verbatim|doc>>All files <verbatim|*.<em|xx>.tm> and
    <verbatim|*.<em|xx>.tmml> of <verbatim|$TEXMACS_DOC_PATH> for the
    current output language <verbatim|<em|xx>>.

    <item*|<verbatim|Scheme>, <verbatim|Styles>, <verbatim|C++>,
    <verbatim|All code>>The <scheme> files, style files and <c++> sources
    (the latter under <verbatim|$TEXMACS_SOURCE_PATH/src>).

    <item*|<verbatim|recent>, <verbatim|texts>>The 50 most recent files, or
    the documents in <verbatim|$TEXMACS_FILE_PATH>.
  </description>

  Each file is scored by the <c++> routine <cpp|search_score>
  (<verbatim|src/src/System/Files/file.cpp>, glue <scm|system-search-score>):
  the scores of the individual words are multiplied, a word counts ten
  times more when it is a whole word, and in <verbatim|.tm> files
  occurrences inside tag names do not count while occurrences in
  <markup|name>, <markup|tmstyle>, <markup|explain-macro> and a few other
  tags count more. Words are lower cased and, for <verbatim|.tm> files,
  converted to the escaped Cork form used in the files. The result page
  lists the matching files with their titles (<scm|help-file-title>),
  sorted by score in percent of the best one. The lists of files are cached
  per path and pattern for the whole session.

  <section|Finding the documentation of an item>

  <verbatim|progs/doc/tmdoc-search.scm> finds the <markup|explain> block
  which documents a given item, that is, whose header contains the item
  in the corresponding markup: <scm|tmdoc-search-tag> for a tag
  (<markup|explain-macro> or <markup|markup>), <scm|tmdoc-search-style>
  for a style or package (<markup|tmstyle> or <markup|tmpackage>),
  <scm|tmdoc-search-parameter> for an environment variable
  (<markup|var-val> or <markup|src-var>) and <scm|tmdoc-search-scheme> for
  a <scheme> function (<markup|scm> with the function name at the start). It first selects the files of the help
  path which contain the corresponding markup as a string (<scm|url-grep>)
  and then searches their trees; if nothing is found for the output
  language, English files are searched. These routines are used by the
  contextual help of the focus toolbar (<verbatim|generic/generic-doc.scm>)
  and by the macro editor (<verbatim|source/macro-widgets.scm>).

  <section|The API documentation>

  The <verbatim|tmfs://apidoc/> pages (<menu|Help|Scheme extensions|Browse
  modules documentation>, <menu|Browse symbols documentation>, and the
  module and symbol browsers of the developer menu) show, for <scheme>
  functions and macros, their documentation together with their source
  code and module. The documentation comes from a cache of
  <markup|explain> blocks (<verbatim|progs/doc/apidoc-collect.scm>):

  <\itemize>
    <item>The cache is built the first time documentation is requested in
    a language for which it has not been built yet (<scm|doc-check-cache>,
    which consults the preferences <verbatim|doc:collect-timestamp> and
    <verbatim|doc:collect-languages>).

    <item><scm|doc-collect-all> starts from a fixed list of root pages
    (<verbatim|devel/scheme/scheme>, <verbatim|devel/plugin/plugin>,
    <verbatim|devel/plugin/plugins>, <verbatim|devel/source/source>,
    <verbatim|devel/style/style> and <verbatim|main/man-reference>), follows
    their <markup|branch> links recursively, and stores every
    <markup|explain> block whose header contains <markup|scm> or
    <markup|explain-macro> under each documented name.

    <item>The entries are kept in two persistent tables in
    <verbatim|$TEXMACS_HOME_PATH/system/cache/> whose names are stored in
    the preferences <verbatim|doc:doc-scm-cache> and
    <verbatim|doc:doc-macro-cache>. Each entry records the name, the
    language, the source file and the tree; <scm|doc-retrieve> returns the
    entries for the output language, or the English ones.
  </itemize>

  The <c++> routines exported to <scheme> are documented separately in
  <hlink|the glue auto-documentation|../scheme/api/glue-auto-doc.en.tm>.
  That file and <verbatim|progs/prog/glue-symbols.scm> are generated from
  the glue declarations by <verbatim|src/src/Scheme/Glue/build-auto-doc>
  (with <verbatim|make-apidoc-doc.scm> and
  <verbatim|make-apidoc-module.scm>); see <hlink|the glue
  generator|scheme-bridge-glue.en.tm>.

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
