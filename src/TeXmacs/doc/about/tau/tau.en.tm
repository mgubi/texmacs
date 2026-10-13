<TeXmacs|2.1.5>

<style|<tuple|tmdoc|old-dots|old-lengths>>

<\body>
  <tmdoc-title|About Tau>

  <name|Tau> (<math|<around*|\<langle\>|\<tau\>|\<rangle\>>>) is an
  experimental form of <TeXmacs> in which the editor and its interface are
  two programs. The <em|core> is <TeXmacs> without windows: the documents,
  the typesetter, the <scheme> interpreter (<name|S7>) and all the programs
  of <TeXmacs> written in <scheme>. The <em|interface> has the windows, the
  menus, the tool bars and the dialogs, and knows nothing about documents.
  The two talk with a small set of messages: the interface says what the
  user does (keys, the pointer, the item of a menu which was chosen) and
  the core answers with descriptions of the menus, the bars and the
  dialogs, and with the pixels of the views which changed.

  The same core is built in two ways.

  <\description>
    <item*|In a web browser>The core is compiled to WebAssembly and runs in
    a worker of the page; the interface is the page itself. Nothing is
    installed, and the documents stay in the browser until a copy of them
    is saved.

    <item*|On the desktop>The core is a process of its own
    (<verbatim|tau_core>), started by the program which has the window
    (<verbatim|tau_shell>), whose interface is <name|Vue>, the interface
    which <TeXmacs> draws itself. The typesetter never holds up the window:
    while the core works the footer says so, and the menus, the scroll bars
    and the panes still answer.
  </description>

  <name|Tau> is a work in progress: its sources are in the branch
  <verbatim|wip_tau> of <hlink|github.com/mgubi/texmacs|https://github.com/mgubi/texmacs/tree/wip_tau>,
  with the design in <verbatim|docs/tau-design.md>, and the page is
  published at <hlink|mgubi.github.io/tau|https://mgubi.github.io/tau/>.

  <section|Recent changes>

  <\description>
    <item*|13 October 2026, on the desktop>A first version for the desktop:
    <name|Vue> for the interface and the core in a process of its own, with
    the shortcuts and the font of the machine. Menus, icon bars, dialogs,
    the search tool, context menus, the clipboard of the machine (also in
    the fields of the dialogs), the file dialogs of the system, drops of
    files, input methods at the cursor. Tabs for the documents and panes
    side by side in the window, with a divider which is dragged. The
    interactive status bar, with the menu of a tag on a right click. The
    footer says when the core is busy. The preferences of the interface
    (the place of the icon bars, the theme, the icons) are followed.

    <item*|13 October 2026>The pages which did not change are kept from one
    pass of the typesetter to the next: typing in a long document with
    headers and footers is several times faster. The wheel and the pointer
    are paced with the core. A benchmark of the drawing and of an edit.

    <item*|11 October 2026>A panel of the files kept in the browser, where
    documents are opened and saved. Backups in the menu and an exact
    restore; files dropped on a view; documents named in the address of the
    page. The page shows when the core is busy.

    <item*|9 October 2026>The first versions in the browser: the menu bar,
    the icon bars and the footer in the page; the dialogs and the
    questions; tabs and panes; the files of the user and the clipboard;
    views in dialogs, the tools, the plugins and the remote client; partial
    paints, scroll bars, the preview, full screen and tooltips; the
    palettes of colours and the icon.
  </description>

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software
  Foundation; with no Invariant Sections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of the license is included in the
  section entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>
