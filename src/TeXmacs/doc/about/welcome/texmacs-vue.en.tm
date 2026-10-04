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
  a Mac, they use the command key, as those of the browser: <key|M-c>,
  <key|M-x> and <key|M-v> copy, cut and paste. The keys which compose (dead
  keys, the accents of a Mac, the input methods of Chinese or Japanese) work
  as in the other programs.

  Copy, cut and paste go through the clipboard of the system, so that text
  can be exchanged with the other programs; a copy made in <TeXmacs> keeps
  its structure when it is pasted back into <TeXmacs>. The browser lets a
  page read its clipboard only when a key pastes, so that
  <menu|Edit|Paste> pastes the last copy or paste of <TeXmacs>; use
  <menu|Edit|Paste from browser...> for what was copied in another page or
  program since: it opens a small dialog, in which <key|M-v> (or its button)
  pastes.

  In the same way, some browsers (Safari) let a page write its clipboard
  only during a key or a click: <key|M-c> and <key|M-x> always copy for the
  other programs, but a copy from a menu (<menu|Edit|Copy>, the formats of
  <menu|Edit|Copy to>) opens a small dialog, <with|font-series|bold|Copy for
  other programs>, whose button gives it to them.

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
    sessions of other programs (Maxima, Python, R...), and the tools which
    need an external program are missing: the compilation with <LaTeX>,
    Ghostscript, ImageMagick, the spell checker, Git. The sessions which
    exist in the browser are those whose program runs in the page itself:
    <name|JavaScript>, <name|TikZ> and <name|Asymptote> (see the help of
    their plug-ins, in <menu|Help|Plug-ins>).

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

  <section|Recent changes>

  The additions to <TeXmacs> <name|Vue>, the newest first.

  <paragraph|4 October 2026>

  <\itemize>
    <item>The keys which compose: the dead keys of many keyboards (the
    <verbatim|^> or the diaeresis of a Swiss or a French keyboard), the accents of a Mac, and the input methods of Chinese,
    Japanese or Korean; the text being composed is shown in the document,
    and the panel of the system (the accents of a held key, the candidates of
    an input method) appears at the cursor.

    <item>A copy keeps its structure when it is pasted into another tab of
    <TeXmacs>, or into the page reloaded (it was pasted as text).

    <item>The shortcuts with Option, Control or Command work in browsers
    which hide these keys from the page, as LibreWolf (Option+arrows moved
    by characters instead of words).

    <item>The <name|Asymptote> plug-in in the browser: sessions and
    executable folds (<menu|Insert|Session|Asymptote>) make <name|Asymptote>
    pictures with <name|Asymptote> itself, compiled to <name|WebAssembly>;
    their labels are set by <TeXmacs> from their <LaTeX> and can be edited,
    and in a fold the code follows. See <menu|Help|Plug-ins|Asymptote>.

    <item><name|JavaScript> sessions and executable folds
    (<menu|Insert|Session|JavaScript>): the <name|JavaScript> of the page
    itself, with <TeXmacs> at hand (<verbatim|TeXmacs.scheme>), and a file
    <verbatim|my-init-javascript.js> run at each start
    (<menu|Developer|Open my-init-javascript.js>). See
    <menu|Help|Plug-ins|JavaScript>.

    <item>Copy in Safari: <key|M-c> and <key|M-x> copy for the other
    programs too (the system kept its old clipboard), with the HTML of the
    copy; a copy from a menu offers the dialog <with|font-series|bold|Copy
    for other programs>.

    <item>Executable folds evaluate again in every case (depending on the
    order <TeXmacs> started in, <key|return> could turn a fold into an
    empty output).

    <item>A crash of the page on some keys (in the editing of a formula)
    fixed in the interpreter of <name|Scheme>.

    <item>The help pages keep the backslashes of their examples of code.
  </itemize>

  <paragraph|3 October 2026>

  <\itemize>
    <item>The <name|TikZ> plug-in in the browser: sessions and executable
    folds (<menu|Insert|Session|TikZ>) make <name|TikZ> pictures with
    <name|TikZJax>, a <name|TeX> which runs in the page; the text of a
    picture is set by <TeXmacs> and can be edited, and in a fold the source
    follows. See <menu|Help|Plug-ins|TikZ>.

    <item>Plug-ins in the browser, which run beside the page (as <name|Web
    Workers>); the list of the plug-ins is made again after an update of the
    page, and a plug-in which fails no longer hides the others.
  </itemize>

  <paragraph|2 October 2026>

  <\itemize>
    <item>Tighter tool bars.

    <item>The full manuals (<menu|Help|Full manuals>) no longer crash the
    page.
  </itemize>

  <paragraph|1 October 2026>

  <\itemize>
    <item><menu|Edit|Paste from browser...>: a dialog which gets what was
    copied in another page or program, in the formats of
    <menu|Edit|Paste from>.

    <item>Fast typing no longer lags behind the keys.

    <item>The <menu|Remote> menu connects with secure WebSockets
    (<verbatim|wss>) when the page is served over <verbatim|https>.

    <item>The loading panel shows <TeXmacs>, its version, and what it loads.

    <item>The keys of the shortcuts are shown with the symbols of a Mac, in
    the font of the menus.
  </itemize>

  <paragraph|30 September 2026>

  <\itemize>
    <item>This page, <menu|Help|TeXmacs in the browser>.

    <item>The fonts are loaded one by one, the first time a document uses
    them, instead of all of them in the background (28<nbsp>MB less).

    <item>An interactive status bar: the character before the cursor, a
    swatch of the colour, and a right click on a tag opens its
    <menu|Focus> menu.

    <item>The panel of the files: a PDF or an image opens in a tab of the
    browser; its buttons say that the files stay in this browser.

    <item>On a Mac, Control+click is a right click.
  </itemize>

  <paragraph|29 September 2026>

  <\itemize>
    <item>Downloads and requests to the web are made by the browser:
    documents can be opened from the web.

    <item>The links to other sites open in the browser.

    <item>The logo of <TeXmacs> <name|Vue>, which is also the icon of the
    page.

    <item>A document at an address with a port opens instead of crashing.
  </itemize>

  <paragraph|28 September 2026>

  <\itemize>
    <item>The address of the page opens a document
    (<verbatim|?open=...>) and passes options to <TeXmacs>.

    <item>Presentation mode, in full screen.

    <item>The shortcuts with Shift and the command key on a Mac.

    <item>The page uses no processor while nothing happens.

    <item>The bars and the side tools of the <menu|View> menu apply to all
    the tabs.
  </itemize>

  <paragraph|27 September 2026>

  <\itemize>
    <item><TeXmacs> <name|Vue> is published on <name|GitHub Pages>, built
    by the continuous integration of the repository.

    <item>The clipboard of the system, with the shortcuts of the platform of
    the browser.

    <item>Printing: the PDF of the document in a tab of the browser.

    <item>Scrolling with a trackpad follows the fingers.

    <item>The <menu|Remote> menu, through a <TeXmacs> server which accepts
    WebSocket clients.

    <item>The storage of the page: its size, its removal, and the files of
    <TeXmacs> itself.

    <item>The <with|font-series|bold|TeXmacs <name|Vue>> menu: the progress
    of the loading, the software of the page, more information and the
    limits.

    <item>The fonts of the PDF files are embedded correctly (a fix to
    <name|MuPDF>).

    <item>A plug-in which cannot start its program no longer freezes the
    page.
  </itemize>

  <paragraph|26 September 2026>

  <\itemize>
    <item>The first version of <TeXmacs> in the browser: <TeXmacs> compiled
    to <name|WebAssembly> with the <name|Vue> interface, a tab per
    document, the frame of the page, the files of the page (with the files
    and folders dropped on it), and the files of <TeXmacs> loaded in the
    background.
  </itemize>

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
