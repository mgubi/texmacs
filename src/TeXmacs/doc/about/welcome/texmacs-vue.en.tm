<TeXmacs|2.1.5>

<style|<tuple|tmdoc|old-dots|old-lengths>>

<\body>
  <tmdoc-title|<TeXmacs> in the browser>

  <TeXmacs> <name|Vue> is an experimental port of <TeXmacs> which runs in a page of
  a web browser. It is the same <TeXmacs>, compiled to WebAssembly, with a
  new interface, <name|Vue>, which draws the menus, the tool bars and the dialogs
  itself. Nothing is installed on your computer, and nothing you write is
  sent anywhere: the documents stay in the browser, on your computer, until
  you save a copy of them.

  This page describes what is different from the desktop program, and what
  does not work yet.

  <section|Windows, tabs and dialogs>

  A web page has a single window. The windows of <TeXmacs> are therefore the
  <em|tabs> above the page: one per document, with its name, a dot when it
  has unsaved changes, a cross to close it, and a <verbatim|+> for a new
  document. The dialogs of <TeXmacs> float over the page, and can be moved
  by their title bar and resized by their edges.

  The <with|font-series|bold|TeXmacs <name|Vue>> button at the top left of the
  page opens a menu of the page itself: the version of <TeXmacs>, the state
  of its files and the storage they use, <menu|Files in this browser...>,
  <menu|Reload>, <menu|Reset...> (which deletes your files and preferences),
  and <menu|Remove from this browser...>. Its panel also has more
  information and the options of the address of the page (see below).

  <section|Your files>

  Your documents are kept in the storage of the browser, for this site
  only. <menu|File|Files in this browser...> shows them:

  <\itemize>
    <item><with|font-series|bold|Add files...>, <with|font-series|bold|Add a
    folder...> and <with|font-series|bold|Add a zip...> bring files from
    your computer, with whole projects (a folder or a zip archive, with the
    images of its documents). Files and folders can also be dropped on the
    page: the documents open, and the images dropped on a document are
    inserted where they fall.

    <item><with|font-series|bold|save copy> saves a copy of a file on your
    computer (a folder as a zip archive).

    <item><with|font-series|bold|open> opens a document in <TeXmacs>. A PDF
    or an image, which <TeXmacs> does not open as a document, is shown in a
    new tab of the browser.

    <item>The dialogs <menu|File|Load> and <menu|File|Save as> are this same
    panel.

    <item><with|font-series|bold|Files of <TeXmacs>> shows the files of
    <TeXmacs> itself (its styles, packages and documentation). They cannot be
    changed, but <with|font-series|bold|customize> copies one into your own
    <verbatim|.TeXmacs> folder, where <TeXmacs> looks first.
  </itemize>

  <\warning*>
    Clearing the data of the site deletes your files, and Safari deletes the
    data of a site which was not visited for seven days. Save a copy of the
    documents you want to keep. Keep <TeXmacs> <name|Vue> open in a single tab of
    the browser: two tabs share the same storage, and their saves may
    overwrite each other.
  </warning*>

  <section|Keyboard, clipboard and printing>

  <TeXmacs> keeps its usual keyboard shortcuts, but the browser keeps some
  for itself (a new window, a new tab, closing a tab, reloading the page):
  use the menus of <TeXmacs>, or the <verbatim|+> of the tabs, for those. On
  a Mac, the shortcuts use <key|Cmd>, as those of the browser.

  Copy, cut and paste go through the clipboard of the system, so that text
  can be exchanged with the other programs; a copy made in <TeXmacs> keeps
  its structure when it is pasted back into <TeXmacs>.

  <menu|File|Print> makes a PDF of the document and opens it in a new tab
  of the browser, from which it can be printed or saved.

  <section|Loading and fonts>

  The first visit loads some 9<nbsp>MB before <TeXmacs> starts, and 8<nbsp>MB
  more in the background while you work. The fonts which come with
  <TeXmacs> are loaded one by one, the first time a document uses them. All
  of it is kept by the browser: the next visits load nothing.

  Only the fonts which come with <TeXmacs> are available: a web page cannot
  see the fonts installed on your computer.

  <section|Opening a document from the web>

  A link to the page can open a document published on the web, which makes
  the page a viewer of <TeXmacs> documents:

  <\verbatim-code>
    https://mgubi.github.io/texmacs/?open=https://example.org/paper.tm
  </verbatim-code>

  Any format <TeXmacs> opens will do (<verbatim|.tm>, <verbatim|.tex>,
  <verbatim|.html>, <verbatim|.md>...). The site of the document has to
  allow other pages to read it, and the images the document refers to are
  not fetched with it. Other options of the address run Scheme commands (the
  page asks before running them) or show debugging messages: see the
  options of the address in the <with|font-series|bold|TeXmacs <name|Vue>> menu.

  <section|What does not work>

  <\itemize>
    <item>A web page cannot run other programs. There are therefore no
    plugins and no sessions of other programs (Maxima, Python, R...), and
    the tools which need an external program are missing: the compilation
    with <LaTeX>, Ghostscript, ImageMagick, the spell checker, Git.

    <item>The <menu|Remote> menu connects to a <TeXmacs> server over
    WebSocket, on the same computer only for now.

    <item><TeXmacs> <name|Vue> is slower than the desktop program.
  </itemize>

  <section|Reporting problems>

  <TeXmacs> <name|Vue> is experimental. A problem which does not happen with the
  <TeXmacs> you install on your computer is a problem of this port: please
  report it on the <hlink|issue page of the
  port|https://github.com/mgubi/texmacs/issues>, not to the <TeXmacs>
  project. The sources and the notes of the port are on
  <hlink|GitHub|https://github.com/mgubi/texmacs/tree/wip_wasm_vue>.

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
