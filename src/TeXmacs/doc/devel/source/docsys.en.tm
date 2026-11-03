<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The documentation system>

  <section|Introduction>

  The documentation of <TeXmacs>, including the present text, consists of
  ordinary <TeXmacs> documents in the directory
  <source-link|src/TeXmacs/doc/|TeXmacs/doc> (installed as <verbatim|$TEXMACS_PATH/doc>).
  They are written in the <tmstyle|tmdoc> style, linked together by
  <markup|traverse> blocks, and shown in the help browser, compiled into
  books, converted into the <TeXmacs> web site, scanned for the
  documentation of <scheme> functions and macros, and searched. This
  chapter describes how these pieces are implemented.

  How to <em|write> documentation as an author (meta information, the
  traversal tags, the <markup|explain> environments, conventions for file
  names) is explained in <hlink|contributing to the <TeXmacs>
  documentation|../../about/contribute/documentation/documentation.en.tm>.
  The <verbatim|tmfs://help/>, <verbatim|tmfs://apidoc/> and
  <verbatim|tmfs://grep/> handlers are listed in <hlink|the <TeXmacs> file
  system handlers|../scheme/api/tmfs/tmfs-handlers.en.tm>. The present
  chapter does not repeat these texts.

  <section|Overview>

  <\description>
    <item*|The style>The <tmstyle|tmdoc> style
    (<source-link|styles/documentation/texmacs/tmdoc.ts|TeXmacs/styles/documentation/texmacs/tmdoc.ts>) loads the package
    <tmpackage|doc>, which combines four packages: <tmpackage|tmdoc-markup>
    (markup for names, files, links and <markup|explain>),
    <tmpackage|tmdoc-gui>, <tmpackage|tmdoc-traversal> (titles, copyright,
    license and the traversal tags) and <tmpackage|tmdoc-framed>. In the
    editor, the traversal tags simply render as lists of hyperlinks.

    <item*|The help path>Documentation files are looked up in
    <verbatim|$TEXMACS_DOC_PATH>, which contains
    <verbatim|$TEXMACS_HOME_PATH/doc>, <verbatim|$TEXMACS_PATH/doc> and the
    <verbatim|doc> directories of all plug-ins. Each page exists in one
    file per language, <verbatim|<em|name>.<em|xx>.tm>; the language of the
    user interface selects the file, with English as fallback.

    <item*|The help browser>The Help menus call <scm|load-help-buffer>,
    <scm|load-help-article> or <scm|load-help-book>, which open
    <verbatim|tmfs://help/<em|type>/...> <abbr|URL>s. For the types other
    than <verbatim|normal>, the handler <em|expands> the page: it follows
    its <markup|traverse> branches recursively and concatenates all pages
    into one document, turning page titles into sectional headings.

    <item*|Tools>The web site generator (<source-link|doc/tmweb.scm|TeXmacs/progs/doc/tmweb.scm>) converts
    a directory tree of documents to <name|HTML>; the full text search
    (<source-link|doc/docgrep.scm|TeXmacs/progs/doc/docgrep.scm>) and the search for the documentation of a
    tag, style or function (<source-link|doc/tmdoc-search.scm|TeXmacs/progs/doc/tmdoc-search.scm>) scan the help
    path; the <abbr|API> documentation (<verbatim|doc/apidoc*.scm>) builds
    a cache of all <markup|explain> blocks for <scheme> functions and
    macros, and the glue generator produces the reference of the <c++>
    routines exported to <scheme>.
  </description>

  <section|Source files>

  Paths are relative to <source-link|src/TeXmacs/|TeXmacs>.

  <\description-paragraphs>
    <item*|<source-link|styles/documentation/texmacs/tmdoc.ts|TeXmacs/styles/documentation/texmacs/tmdoc.ts>>The
    <tmstyle|tmdoc> style: page layout, fonts and the code markup
    (<markup|verbatim>, <markup|scm>, <markup|cpp>, ...).

    <item*|<source-link|packages/documentation/doc.ts|TeXmacs/packages/documentation/doc.ts>,
    <verbatim|packages/documentation/standard/tmdoc-*.ts>>The
    documentation packages; <source-link|tmdoc-web.ts|TeXmacs/packages/documentation/standard/tmdoc-web.ts> and
    <source-link|tmdoc-web2.ts|TeXmacs/packages/documentation/standard/tmdoc-web2.ts> are used by the web site styles <tmstyle|tmweb> and
    <tmstyle|tmweb2> (and by the <name|Mathemagix> documentation styles),
    <source-link|scheme-api.ts|TeXmacs/packages/documentation/standard/scheme-api.ts> by the <abbr|API> pages. The style
    <tmstyle|tmmanual> is used for compiled books.

    <item*|<source-link|progs/doc/help-funcs.scm|TeXmacs/progs/doc/help-funcs.scm>>The help path, resolution of
    help files by language, <scm|load-help-buffer> and friends.

    <item*|<source-link|progs/doc/help-menu.scm|TeXmacs/progs/doc/help-menu.scm>>The Help menu.

    <item*|<source-link|progs/doc/tmdoc.scm|TeXmacs/progs/doc/tmdoc.scm>>Expansion into articles and
    books, the <verbatim|tmfs://help/> handler, <scm|tmdoc-include>.

    <item*|<source-link|progs/doc/tmdoc-edit.scm|TeXmacs/progs/doc/tmdoc-edit.scm>, <source-link|tmdoc-menu.scm|TeXmacs/progs/doc/tmdoc-menu.scm>,
    <source-link|tmdoc-kbd.scm|TeXmacs/progs/doc/tmdoc-kbd.scm>, <source-link|tmdoc-drd.scm|TeXmacs/progs/doc/tmdoc-drd.scm>,
    <source-link|tmdoc-markup.scm|TeXmacs/progs/doc/tmdoc-markup.scm>>Editing support for documentation
    documents.

    <item*|<source-link|progs/doc/tmweb.scm|TeXmacs/progs/doc/tmweb.scm>>The web site generator.

    <item*|<source-link|progs/doc/docgrep.scm|TeXmacs/progs/doc/docgrep.scm>,
    <source-link|progs/doc/tmdoc-search.scm|TeXmacs/progs/doc/tmdoc-search.scm>>Searching.

    <item*|<verbatim|progs/doc/apidoc*.scm>>The <abbr|API> documentation of
    <scheme> symbols and modules.

    <item*|<source-link|progs/utils/test/test-convert.scm|TeXmacs/progs/utils/test/test-convert.scm>>Building the
    <abbr|PDF> manuals (<scm|build-manual>).

    <item*|<source-link|src/src/Scheme/Glue/build-auto-doc|src/Scheme/Glue/build-auto-doc>,
    <source-link|make-apidoc-doc.scm|src/Scheme/Glue/make-apidoc-doc.scm>, <source-link|make-apidoc-module.scm|src/Scheme/Glue/make-apidoc-module.scm>>The
    generator of <source-link|doc/devel/scheme/api/glue-auto-doc.en.tm|TeXmacs/doc/devel/scheme/api/glue-auto-doc.en.tm> and
    <source-link|progs/prog/glue-symbols.scm|TeXmacs/progs/prog/glue-symbols.scm>.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|The tmdoc style and its packages|docsys-style.en.tm>

    <branch|Help files, the help browser and expansion into
    books|docsys-help.en.tm>

    <branch|Web site, search and API documentation|docsys-tools.en.tm>

    <branch|Writing and validating developer documentation|docsys-writing.en.tm>
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
