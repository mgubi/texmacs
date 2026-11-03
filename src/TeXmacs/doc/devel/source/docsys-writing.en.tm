<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Writing and validating developer documentation>

  <section|Conventions>

  The general conventions for the documentation (directories, file names,
  meta information, licenses) are given in <hlink|contributing to the
  <TeXmacs>
  documentation|../../about/contribute/documentation/documentation.en.tm>.
  For the developer documentation in <source-link|doc/devel/source/|TeXmacs/doc/devel/source>, the
  following conventions have proved useful:

  <\itemize>
    <item>One chapter per subsystem. The chapter consists of an index page
    <verbatim|<em|topic>.en.tm> with an <em|Introduction>, an
    <em|Overview>, a list of <em|Source files> and a <markup|traverse> to a
    small number of subpages <verbatim|<em|topic>-<em|subtopic>.en.tm>. A
    page on pitfalls and known bugs, with file and line references, is
    often worth adding.

    <item>Each chapter is reachable from one of the index pages of
    <hlink|about the source code|source.en.tm> by a <markup|branch>; pages
    which are only reached by <markup|hlink> are not included in compiled
    books, nor scanned for <markup|explain> blocks.

    <item>Source files are referred to with <markup|source-link> (see
    below), which shows the name as written and opens the file when it is
    clicked. In the shown text, paths of <c++> files are given relative to
    <source-link|src/src/|src> and paths of <scheme> files relative to
    <verbatim|progs/>. Links to other documentation are relative
    <markup|hlink>s, so that they work in the help browser, in books and on
    the web site.

    <item>Prefer linking to the existing page on a subject over repeating
    it; when describing behaviour, state what the code does, not what it
    was meant to do, and mark claims that were not checked.

    <item>Never put markup which writes an index entry (<markup|menu>,
    <markup|tmstyle>, <markup|tmpackage>, <markup|tmdtd>,
    <markup|name*>, <markup|indexed>, ...) in a title or sectioning
    command. The title is copied into the table of contents, so every
    regeneration of the table of contents adds automatic labels, all the
    following <verbatim|auto-<em|n>> labels are renumbered, and the page
    numbers of the table of contents and the index of a compiled book never
    converge.

    <item>Labels of description items are unbreakable boxes. When labels
    are long, for instance lists of several file names, use
    <markup|description-paragraphs> instead of <markup|description>.

    <item>Give every page a base name which is unique in the whole
    documentation: compiled books label each page <verbatim|sec-<em|name>>
    after its base name.

    <item>Use the markup of the <tmstyle|tmdoc> style:
    <markup|cpp>, <markup|scm>, <markup|verbatim>, <markup|markup>,
    <markup|tmstyle>, <markup|tmpackage>, <markup|explain> with
    <markup|explain-synopsis>, <markup|cpp-code> and <markup|scm-code> for
    blocks.
  </itemize>

  <section|Links to the source files>

  The pages of the developer documentation only make sense together with
  the code, so every reference to a source file is a link:

  <\verbatim-code>
    \<less\>source-link\|Typeset/Env/env_exec.cpp\|src/Typeset/Env/env_exec.cpp\<gtr\>

    \<less\>source-link\|ai.cpp:815\|src/Data/Convert/AI/ai.cpp:815\<gtr\>
  </verbatim-code>

  The first argument is the text which is shown, in the
  <markup|verbatim> font; the second one is the path of the file relative
  to the <verbatim|src> directory of the repository (the directory which
  contains <verbatim|src>, <verbatim|TeXmacs> and <verbatim|plugins>),
  optionally followed by <verbatim|:<em|line>>. The path may also name a
  directory, which is opened with the file manager of the system. A click
  calls
  <scm|open-source-link> of <source-link|doc/source-links.scm|TeXmacs/progs/doc/source-links.scm>,
  which looks for the file in

  <\enumerate>
    <item>the directory of the preference <verbatim|developer:source
    directory>, if it is set;

    <item>otherwise <verbatim|$TEXMACS_SOURCE_PATH>, the source tree
    <TeXmacs> was configured from (empty for the <name|Windows> builds);

    <item>for paths in <source-link|TeXmacs/|packages/macos/TeXmacs>, finally the installed
    <verbatim|$TEXMACS_PATH>, so that the <scheme> files, styles and
    packages can be opened from any installation.
  </enumerate>

  The file is then opened with the tool chosen in <menu|Developer|Open
  source links with> (preference <verbatim|developer:source editor>):
  <TeXmacs> itself, at the given line; the default application of the
  system; a predefined editor (<name|Visual Studio Code>, <name|Emacs>,
  <name|Xcode>, <name|Sublime Text>, <name|Zed>); or any command line, in
  which <verbatim|%f> is replaced by the quoted file name and
  <verbatim|%l> by the line (1 when no line is given). The same menu sets
  the source directory, which is needed for binary installations. The
  <menu|Developer> menu appears once <menu|Tools|Developer tool> is
  checked.

  The links are checked by <source-link|tests/docs/source-links.py|tests/docs/source-links.py>,
  which reports every <markup|source-link> whose file is no longer in the
  repository, and every <markup|verbatim> which names exactly one file
  (or one directory) of the repository and could be a link; with
  <verbatim|--convert> it turns the latter into links. A name is matched by
  the end of the paths, so <tt|Plugins/Qt/qt_gui.cpp> and
  <tt|qt_gui.cpp> both work; a name starting with <tt|src/> is
  read from the root of the repository. Names without a <verbatim|/> are
  only considered for source files (<verbatim|.cpp>, <verbatim|.scm>,
  <verbatim|.m4>, ...). Names which match several files or directories
  are left alone (write more of the path), as are generated files, files
  of other branches and files of the user's home directory, which stay in
  <markup|verbatim>.

  <section|Encoding>

  <verbatim|.tm> files are not <name|UTF-8> files: strings are stored in
  the Cork encoding of <TeXmacs>, with escapes for everything else. When
  documentation is written or edited outside <TeXmacs>, the file should
  contain only <name|ASCII> characters:

  <\itemize>
    <item>quotes are written <verbatim|\\P> and <verbatim|\\Q>, special
    symbols as <verbatim|\\\<less\>name\\\<gtr\>> (for instance
    <verbatim|\\\<less\>times\\\<gtr\>>) and other characters as
    <verbatim|\\\<less\>#<em|hex>\\\<gtr\>>;

    <item>the characters <verbatim|\<less\>>, <verbatim|\<gtr\>>,
    <verbatim|\|> and <verbatim|\\> are escaped as
    <verbatim|\\\<less\>less\\\<gtr\>>, <verbatim|\\\<less\>gtr\\\<gtr\>>,
    <verbatim|\\\|> and <verbatim|\\\\>;

    <item>leading spaces in code blocks are written <verbatim|\\ >.
  </itemize>

  A raw <name|UTF-8> character in a <verbatim|.tm> file is read as several
  Cork characters and shows up as garbage. A quick check is to search the
  file for bytes above 127, for instance with
  <verbatim|LC_ALL=C grep -nP '[\\x80-\\xff]' <em|file>>.

  <section|Validation>

  Before committing new pages, it is useful to check that

  <\enumerate>
    <item>every tag used is defined. A tag which is not defined by the
    style is shown in red in the editor and silently passes through
    conversions. A simple check is to compare the tags used in the new
    files with those used in the existing documentation;

    <item>every relative link points to an existing file, and
    <source-link|tests/docs/source-links.py|tests/docs/source-links.py> reports no broken source links;

    <item>every page can be typeset. This can be done without opening
    windows, by converting each page to <abbr|PDF> in headless mode:

    <\verbatim-code>
      TEXMACS_PATH=<em|worktree>/src/TeXmacs \\

      TEXMACS_HOME_PATH=<em|private-home> QT_QPA_PLATFORM=offscreen \\

      texmacs.bin -headless -c <em|page>.en.tm <em|page>.pdf -q
    </verbatim-code>

    A private <verbatim|TEXMACS_HOME_PATH> keeps the test from changing the
    user's preferences and caches, and allows several conversions to run in
    parallel;

    <item>the whole hierarchy expands: loading
    <verbatim|tmfs://help/book/...> for the root page (as <menu|Help|Full
    manuals> does) should give a document which contains the label
    <verbatim|sec-<em|name>> of every new page, and the console should show
    no <verbatim|bad link or file> message.
  </enumerate>

  <section|Pitfalls and known problems>

  <\itemize>
    <item><em|Deep sectioning in books.> Sectioning commands inside a page
    are taken relative to the level of the page (see <hlink|expansion|docsys-help.en.tm>).
    In a deep hierarchy this quickly reaches the unnumbered levels: the
    <markup|subsection>s of a page four levels below the root become
    <markup|paragraph>s, and its <markup|paragraph>s run-in
    <markup|subparagraph>s. Keep the hierarchy shallow, or let the root
    start with parts (<verbatim|tmdoc-book-parts>).

    <item><em|Labels from base names.> The label of each page in an
    expansion is <verbatim|sec-<em|name>>, built from the base name only.
    Two pages with the same base name in different directories get the same
    label, and internal links in books may then point to the wrong page.

    <item><em|External links in books.> <scm|tmdoc-internalize> replaces
    every hyperlink whose target is not part of the book by its text, so
    links to other parts of the documentation and to web pages disappear
    from compiled books.

    <item><em|<markup|help-link> has no English fallback.> The macro
    (<source-link|packages/documentation/standard/tmdoc-markup.ts:118|TeXmacs/packages/documentation/standard/tmdoc-markup.ts:118>) always
    appends the suffix of the current language, whereas
    <markup|tmdoc-file> and <scm|url-resolve-help> fall back to
    <verbatim|.en.tm>. With a user interface in a language for which the
    target has no translation, the link leads to the <verbatim|Broken
    link.> page. For instance, <source-link|main/automated/top-help.en.tm|TeXmacs/doc/main/automated/top-help.en.tm>
    links to <verbatim|main/editing/man-structured-variants>, which exists
    only in English, French and Chinese.

    <item><em|Full text search ignores English pages in other languages.>
    The search of type <verbatim|doc> (<source-link|progs/doc/docgrep.scm|TeXmacs/progs/doc/docgrep.scm>,
    handler of <verbatim|tmfs://grep/>) only scans the files of the current
    output language, unlike <scm|tmdoc-search>, which falls back to
    English. With a non English interface, pages which have no translation
    (which includes nearly all developer documentation) are never found.

    <item><em|Caches that are never refreshed.> <scm|url-exists-in-help?>
    and the file lists of the full text search are cached for the whole
    session, so pages added while <TeXmacs> runs are not seen; the
    documentation of <scheme> symbols is collected once per language and
    only rebuilt when the cache is deleted.

    <item><em|Deleting the API cache does not delete it.>
    <scm|doc-delete-cache> asks for confirmation to delete the two cache
    files and then reports them as deleted, but <scm|doc-delete-cache*>
    (<verbatim|progs/doc/apidoc-collect.scm:217-229>) only prints
    <verbatim|I WOULD HAVE deleted the cache at ...> and resets the
    preferences, so that new cache files are created next time and the old
    ones remain on disk.

    <item><em|Language of collected API documentation.> <scm|doctree-lan>
    (<verbatim|apidoc-collect.scm:64-69>) reads the language of a page from
    its initial environment only. Pages which declare their language in the
    style tuple (as many Chinese pages do) are recorded as English.

    <item><em|Fixed roots for the API documentation.> <scm|doc-collect-all>
    only scans the hierarchies below a fixed list of root pages, and only
    follows <markup|branch> (not <markup|continue> or
    <markup|extra-branch>); <markup|explain> blocks elsewhere are not
    collected.

    <item><em|Building PDF manuals.> <scm|build-manual> only knows three
    manuals, ignores other names without a message, and does not rebuild a
    <abbr|PDF> file which already exists.

    <item><em|Online resources.> The flags of <markup|tmdoc-translations>
    are images on <verbatim|www.texmacs.org>, and <scm|update-help-online>
    downloads the documentation from an <verbatim|ftp> address with
    <verbatim|wget>; both fail without network access (and the
    <verbatim|ftp> server may no longer exist).
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
