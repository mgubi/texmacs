<TeXmacs|2.1.5>

<style|<tuple|tmdoc|old-dots|old-lengths>>

<\body>
  <tmdoc-title|<TeXmacs> Upsilon features>

  This <TeXmacs> is <em|<TeXmacs> Upsilon>, an enhanced and bleeding-edge
  version of <TeXmacs> with experimental features: the <TeXmacs> of the
  official releases together with several lines of work which are not (yet)
  part of them, gathered in one tree so that they can be built, used and
  tested together. Its sources are the branch <verbatim|upsilon_texmacs> on
  <hlink|GitHub|https://github.com/mgubi/texmacs/tree/upsilon_texmacs>, whose
  <verbatim|README.md> lists the branches it is made of. This page says
  what they bring, and where to find it in the menus.

  <section|Fixes and speed>

  <\itemize>
    <item>Many bugs of <TeXmacs> are fixed: those reported on
    <name|Savannah> and in the issues of the repository (the bridge to
    <name|Scheme>, the import and export of <LaTeX>, <name|BibTeX>, lists,
    editing modes, keyboard maps, paths on <name|Windows>...).

    <item><with|font-series|bold|Incremental typesetting>: after an edit,
    only what it changed is typeset again, so that typing stays fast in long
    documents.

    <item>The caches of styles and of files follow the files on disk: a
    style file or a package which you change is read again, also when it is
    changed in place by another program.

    <item><name|Scheme>: the programs of <TeXmacs> run on <name|s7> by
    default instead of <name|Guile> 1.8, which makes the first window ready
    about twice as fast. A build can use <name|femtolisp> (experimental) or
    <name|Guile> instead.
  </itemize>

  <section|Fonts and mathematics>

  <\itemize>
    <item><with|font-series|bold|OpenType mathematics>: formulas are laid
    out from the <verbatim|MATH> table of any <name|OpenType> math font, with
    the stretchable delimiters and accents of the font itself; some two
    dozen math fonts are paired with their text companions, most of them
    shipped with <TeXmacs>. See <hlink|How mathematical fonts
    work|../../main/math/fonts/man-math-fonts-intro.en.tm>.

    <item>More fonts (<name|DejaVu>, <name|Gentium Plus>, the
    <name|Antykwas>, <name|Iwona>, the weights of <name|IBM Plex>...), the
    weights and variants of a family (<menu|Format|Weight>), and a font
    menu by design.

    <item><with|font-series|bold|Font design>: the last entry of the font
    menu of <menu|Document> opens a page with a tab for each part of the
    document (text, mathematics, sans serif, typewriter, blackboard bold,
    calligraphic, fraktur), which shows samples of the fonts, applies a
    choice to the document and saves named choices.
  </itemize>

  <section|Interfaces>

  <\itemize>
    <item>Besides <name|Qt>, <TeXmacs> can be built with
    <with|font-series|bold|Vue>, an interface which draws all its widgets
    itself (with <name|Clay> and <name|SDL3>, by the <name|GPU> or by
    <name|MuPDF>), with a native <with|font-series|bold|Cocoa> interface for
    <name|macOS>, and with the <name|Widkit> widgets on <name|SDL3>,
    <name|Qt> or <name|X11>. The interface is chosen when <TeXmacs> is
    built.

    <item>Four sets of icons: the classical icons, a monochrome set, a
    neo-classical set (the default) and the <name|Lucide> icons, chosen in
    <menu|Edit|Preferences|General|Icon set>. <name|Vue> and <name|Cocoa>
    also have a dark theme, the icon bars at the left, and palettes of
    colours.

    <item>The same <TeXmacs> runs in a web browser:
    <hlink|mgubi.github.io/texmacs|https://mgubi.github.io/texmacs/>, see
    <hlink|<TeXmacs> in the browser|texmacs-vue.en.tm>.
  </itemize>

  <section|Documents and references>

  <\itemize>
    <item><with|font-series|bold|Converters>: import and export of
    <name|Markdown> (<menu|File|Import|Markdown>,
    <menu|File|Export|Markdown>) and of the documents of <name|Word>
    (<verbatim|.docx>) and <name|OpenDocument> (<verbatim|.odt>), with
    their tables, formulas, images, references and bibliographies. See
    <hlink|Converters for <name|Markdown>|../../main/convert/markdown/man-markdown.en.tm>
    and <hlink|Converters for <name|Word> and
    <name|OpenDocument>|../../main/convert/office/man-office.en.tm>.

    <item><with|font-series|bold|Citations from <name|Zotero>>: the
    references of the <name|Zotero> application are searched with those of
    the bibliography file, and a bibliography can be written from
    <name|Zotero> and kept up to date
    (<menu|Document|Bibliography|Zotero settings...>). See
    <hlink|Citations from <name|Zotero>|../../main/links/man-zotero.en.tm>.

    <item><with|font-series|bold|Styles of the Citation Style Language>
    (those of <name|Zotero>, <name|Mendeley> and <name|Pandoc>), processed
    by <TeXmacs> itself: a bibliography of style <verbatim|csl-apa>,
    <verbatim|csl-ieee>, <verbatim|csl-chicago-notes-bibliography>... and
    its citations are formatted by the <verbatim|.csl> file. See
    <hlink|Compiling a bibliography|../../main/links/man-bibliography.en.tm>.

    <item><with|font-series|bold|Git>: the <menu|Version> menu works with
    <name|Git>, with a side panel, commits, the history, branches, the
    synchronization with a remote, the comparison with any revision, and
    merges of <TeXmacs> documents which follow their structure.
    <name|Git> is never run in a folder before you trust it. See
    <hlink|Working with <name|Git>|../../main/editing/man-git.en.tm>.
  </itemize>

  <section|Program code>

  <\itemize>
    <item><with|font-series|bold|Tree-sitter>: the syntax of 22 languages,
    in source files, in the code blocks of documents and in the inputs of
    sessions: colours, folds, an outline (<menu|Focus|Outline>), the
    selection and the moves by structure, balanced brackets, indentation
    and comments. See <hlink|Editing program
    code|../../main/editing/man-program-code.en.tm> and <hlink|Editing
    source files|../../main/editing/man-source-files.en.tm>.

    <item><with|font-series|bold|Language servers>: source files opened in
    tabs of their own are followed by the language server of their language
    when it is installed (<menu|Tools|Language servers>): diagnostics,
    completion, information on hover, references, renaming, fixes and
    formatting. See <hlink|Language servers for source
    code|../../main/editing/man-language-servers.en.tm>.

    <item>Source files are saved exactly as they were loaded, with their
    tabs, their ends of lines and their indentation.
  </itemize>

  <section|Chatbots>

  <with|font-series|bold|AI sessions> (<menu|Insert|Session|AI>) and
  executable folds ask <name|ChatGPT>, <name|Claude>, <name|Gemini>,
  <name|Mistral>, <name|Ollama>, <name|OpenRouter> or <name|Albert>. The
  answers are streamed and typeset as they come, with their pictures, their
  tokens and their cost; files (texts, images, <name|PDF>) can be attached
  to a question with <with|font-series|bold|Attach> in the focus bar or by
  dropping them; the keys are kept in an encrypted wallet. See the
  <hlink|help of the AI plug-in|../../../plugins/ai/doc/ai.en.tm>.

  <section|Documentation and tests>

  <\itemize>
    <item><menu|Help|Developer documentation>: some four hundred pages on
    the document format, the styles, the plug-ins and the internals of the
    source code, which also compile into a book.

    <item>Unit tests of the kernel, some sixty test suites in <name|Scheme>
    (editing, conversions, typesetting, menus, links...) and regression tests
    on documents, run by the continuous integration of every platform.
  </itemize>

  <section|Builds and problems>

  Builds for <name|macOS> (<name|Cocoa>, <name|Vue> and <name|Qt>),
  <name|Linux> and <name|Windows> (<name|Qt> and <name|Vue>) are made by the
  continuous integration of the repository at each change, and can be
  downloaded from
  <hlink|mgubi.github.io/texmacs/downloads.html|https://mgubi.github.io/texmacs/downloads.html>.
  Problems are reported in the
  <hlink|issues|https://github.com/mgubi/texmacs/issues> of the
  repository, not to the official <TeXmacs>.

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>
