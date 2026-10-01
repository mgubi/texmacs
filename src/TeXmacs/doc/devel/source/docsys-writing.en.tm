<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Writing and validating developer documentation>

  <section|Conventions>

  The general conventions for the documentation (directories, file names,
  meta information, licenses) are given in <hlink|contributing to the
  <TeXmacs>
  documentation|../../about/contribute/documentation/documentation.en.tm>.
  For the developer documentation in <verbatim|doc/devel/source/>, the
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

    <item>Paths of <c++> files are given relative to <verbatim|src/src/>,
    paths of <scheme> files relative to <verbatim|progs/>, and links to
    other documentation are relative <markup|hlink>s, so that they work in
    the help browser, in books and on the web site.

    <item>Prefer linking to the existing page on a subject over repeating
    it; when describing behaviour, state what the code does, not what it
    was meant to do, and mark claims that were not checked.

    <item>Use the markup of the <tmstyle|tmdoc> style:
    <markup|cpp>, <markup|scm>, <markup|verbatim>, <markup|markup>,
    <markup|tmstyle>, <markup|tmpackage>, <markup|explain> with
    <markup|explain-synopsis>, <markup|cpp-code> and <markup|scm-code> for
    blocks.
  </itemize>

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

    <item>every relative link points to an existing file;

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
    <item><em|Sectional levels in books.> The expansion turns page titles
    into headings of the right level, but leaves the <markup|section>,
    <markup|subsection>, ... tags <em|inside> a page unchanged. In a book,
    a page which is three levels deep (and whose title therefore becomes a
    <markup|subsubsection>) may contain <markup|section> headings, so that
    the numbering and the table of contents of deep hierarchies are
    inconsistent (<verbatim|progs/doc/tmdoc.scm>, <scm|tmdoc-rewrite-one>).

    <item><em|Labels from base names.> The label of each page in an
    expansion is <verbatim|sec-<em|name>>, built from the base name only.
    Two pages with the same base name in different directories get the same
    label, and internal links in books may then point to the wrong page.

    <item><em|External links in books.> <scm|tmdoc-internalize> replaces
    every hyperlink whose target is not part of the book by its text, so
    links to other parts of the documentation and to web pages disappear
    from compiled books.

    <item><em|<markup|help-link> has no English fallback.> The macro
    (<verbatim|packages/documentation/standard/tmdoc-markup.ts:118>) always
    appends the suffix of the current language, whereas
    <markup|tmdoc-file> and <scm|url-resolve-help> fall back to
    <verbatim|.en.tm>. With a user interface in a language for which the
    target has no translation, the link leads to the <verbatim|Broken
    link.> page. For instance, <verbatim|main/automated/top-help.en.tm>
    links to <verbatim|main/editing/man-structured-variants>, which exists
    only in English, French and Chinese.

    <item><em|Full text search ignores English pages in other languages.>
    The search of type <verbatim|doc> (<verbatim|progs/doc/docgrep.scm>,
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
