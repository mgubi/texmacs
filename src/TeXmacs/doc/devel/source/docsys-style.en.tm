<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The tmdoc style and its packages>

  The use of the documentation markup is described for authors in
  <hlink|the tmdoc style|../../about/contribute/documentation/tmdoc-style.en.tm>
  and <hlink|automatic traversal of the
  documentation|../../about/contribute/documentation/traversal.en.tm>. This
  page describes how that markup is implemented and how it behaves in the
  three contexts in which documentation is shown: a single page in the
  editor, an expanded article or book, and a web page.

  <section|The style files>

  <\description>
    <item*|<tmstyle|tmdoc>>(<source-link|styles/documentation/texmacs/tmdoc.ts|TeXmacs/styles/documentation/texmacs/tmdoc.ts>)
    loads <tmpackage|std>, <tmpackage|env>, <tmpackage|title-generic>,
    <tmpackage|header-article>, <tmpackage|section-article>,
    <tmpackage|doc> and <tmpackage|pagella-font>. It sets the page layout
    and paragraph spacing, disables saving of auxiliary data
    (<verbatim|save-aux> is <verbatim|false>), sets the <name|CSS> and
    <name|JavaScript> used on export to <name|HTML>, redefines the
    sectional titles with the <name|Fira> font, and defines the inline and
    block code markup: <markup|verbatim>, <markup|shell>, <markup|scm>,
    <markup|cpp>, <markup|python>, <markup|scilab>, <markup|mmx> and the
    session environments.

    <item*|<tmpackage|doc>>(<source-link|packages/documentation/doc.ts|TeXmacs/packages/documentation/doc.ts>) just
    loads <tmpackage|tmdoc-markup>, <tmpackage|tmdoc-gui>,
    <tmpackage|tmdoc-traversal> and <tmpackage|tmdoc-framed>. It is also
    used by <tmstyle|tmmanual>, the style of compiled books, together with
    <tmpackage|tmbook>.

    <item*|<tmstyle|tmweb>, <tmstyle|tmweb2>>The styles of the pages of the
    web site; they add <tmpackage|tmdoc-web> (respectively
    <tmpackage|tmdoc-web2>) to <tmstyle|tmdoc>.
  </description>

  <section|Titles, copyright and license>

  <markup|tmdoc-title> (<source-link|tmdoc-traversal.ts|TeXmacs/packages/documentation/standard/tmdoc-traversal.ts>) draws the logo and
  the title in the title font, with a rule below. <markup|tmdoc-title*>
  adds a subtitle and <markup|tmdoc-title**> a line above the title as
  well. <markup|tmdoc-copyright> takes a period and any number of holders
  and prints them after a rule; <markup|tmdoc-license> prints its body in a
  small grey font, always in English.

  These tags are also the hooks of the expansion into articles and books
  (<hlink|next page|docsys-help.en.tm>): <markup|tmdoc-title> becomes a
  sectional heading, and the copyright and license blocks are removed.

  <section|The traversal tags>

  In the editor, the traversal tags are plain markup:

  <\description>
    <item*|<markup|traverse>>An <markup|itemize> environment.

    <item*|<markup|branch>, <markup|extra-branch>, <markup|continue>,
    <markup|optional-branch>>An item with a hyperlink to the destination;
    the last three use different colors. The destination is resolved by
    <markup|tmdoc-file> (see below).
  </description>

  Their real meaning appears only in the expansion, where
  <markup|branch> inserts the target one sectional level deeper,
  <markup|continue> inserts it at the same level and without its title,
  <markup|extra-branch> inserts it as an appendix and
  <markup|optional-branch> is dropped. Hyperlinks inside the page and
  <markup|tmdoc-include> are handled by the expansion as well.

  <section|Resolution of file names>

  Links and branches give file names <em|without> language:
  <verbatim|<em|name>.en.tm> is common, but <verbatim|<em|name>> alone is
  also accepted. The macro <markup|tmdoc-file> (<source-link|tmdoc-markup.ts|TeXmacs/packages/documentation/standard/tmdoc-markup.ts>)
  tries, in this order,

  <\enumerate>
    <item>the name itself (<markup|find-file>);

    <item><verbatim|<em|name>.<em|xx>.tm>, where <verbatim|<em|xx>> is the
    two letter code of the language of the user interface
    (<markup|language-suffix>, which calls the <scheme> function
    <scm|ext-language-suffix>), searched in the directory of the document
    and upwards up to a directory named <verbatim|doc>, <verbatim|web> or
    <verbatim|texmacs> (<markup|find-file-upwards>);

    <item><verbatim|<em|name>.en.tm> in the same way;

    <item>the name unchanged.
  </enumerate>

  The expansion code uses the same rules (<scm|tmdoc-relative> in
  <source-link|progs/doc/tmdoc.scm|TeXmacs/progs/doc/tmdoc.scm>), so that a page written in one language
  can be read in a translation wherever one exists.

  <section|Translations>

  <markup|tmdoc-translations> takes a file name without suffix and shows a
  row of flags, one for each of the languages <verbatim|de>, <verbatim|en>,
  <verbatim|es>, <verbatim|fr>, <verbatim|it>, ... for which
  <verbatim|<em|name>.<em|xx>.tm> exists (<markup|tmdoc-translation>). The
  flag images are fetched from <verbatim|https://www.texmacs.org/Images/>,
  so they only appear when the machine is online.

  The language of a page itself is not taken from its file name. Translated
  pages set it in their initial environment
  (<verbatim|\<less\>associate\|language\|french\<gtr\>>), some in the style
  tuple (<verbatim|\<less\>style\|\<less\>tuple\|tmdoc\|chinese\<gtr\>\<gtr\>>);
  this matters for hyphenation, quotes and the language of the translated
  menu names shown by the <markup|menu> tag.

  <section|Links to other help pages>

  <\description>
    <item*|<markup|hlink>>An ordinary hyperlink with a relative file name,
    as used throughout the documentation. In expanded documents, links to
    pages which are part of the same expansion are turned into internal
    links (<scm|tmdoc-internalize>).

    <item*|<markup|help-link>>(<source-link|tmdoc-markup.ts|TeXmacs/packages/documentation/standard/tmdoc-markup.ts>) builds the
    <abbr|URL> <verbatim|tmfs://help/article/tm/doc/<em|name>.<em|xx>.tm>
    from a path relative to <verbatim|doc/> and opens it as an article. Unlike
    <markup|tmdoc-file>, it does not fall back to English; see the pitfalls
    on <hlink|the last page|docsys-writing.en.tm>.

    <item*|<markup|tmdoc-include>>Inserts the expanded contents of another
    documentation file (computed by the <scheme> function
    <scm|tmdoc-include>, which expands the file at chapter level and removes
    chapter headings and hyperlinks).
  </description>

  <section|Editing support>

  When a document uses <tmstyle|tmdoc> and is edited directly, that is,
  not opened through a <verbatim|tmfs://> <abbr|URL> (the mode
  <scm|in-manual?> of <source-link|kernel/texmacs/tm-modes.scm|TeXmacs/progs/kernel/texmacs/tm-modes.scm>), the files
  <source-link|tmdoc-edit.scm|TeXmacs/progs/doc/tmdoc-edit.scm>, <source-link|tmdoc-menu.scm|TeXmacs/progs/doc/tmdoc-menu.scm>,
  <source-link|tmdoc-kbd.scm|TeXmacs/progs/doc/tmdoc-kbd.scm> and <source-link|tmdoc-drd.scm|TeXmacs/progs/doc/tmdoc-drd.scm> add a
  <menu|Manual> menu (<source-link|texmacs/menus/main-menu.scm|TeXmacs/progs/texmacs/menus/main-menu.scm>) and icons with
  entries
  to insert the title, copyright and license (<scm|tmdoc-insert-title>,
  <scm|tmdoc-insert-copyright>, <scm|tmdoc-insert-license>,
  <scm|tmdoc-insert-gnu-fdl>), traversal tags (<scm|tmdoc-make-branch>
  inserts a new item after the current one), <markup|explain> blocks and
  the markup for keys, menus and GUI elements. <source-link|tmdoc-drd.scm|TeXmacs/progs/doc/tmdoc-drd.scm>
  declares tag groups such as <scm|tmdoc-traversal-tag> and
  <scm|tmdoc-link-tag>, which drive the variants and the focus toolbar
  (see <hlink|the DRD from Scheme|drd-scheme.en.tm>). The same files hold
  the corresponding <tmstyle|tmweb> entries for the pages of the web site
  (title, license and classification).

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
