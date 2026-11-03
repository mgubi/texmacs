<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The graphical user interface>

  These chapters describe how <TeXmacs> builds its graphical user
  interface. All menus, toolbars, side panes and dialogs are described by
  <em|abstract widgets>, which are mostly built from <scheme> and
  translated into concrete widgets by a <em|port> for a particular toolkit
  (<source-link|src/Plugins|src/Plugins>, chosen with
  <verbatim|./configure --with-gui=...>):

  <\itemize>
    <item><name|Qt> (<verbatim|qt>, the default): native <name|Qt> 5 or 6
    widgets, in <source-link|Plugins/Qt|src/Plugins/Qt>; <verbatim|--enable-qt-new> (the
    default on <name|Android>) takes the fork for <name|Qt> 6.10 and later
    in <source-link|Plugins/Qt6|src/Plugins/Qt6> instead.

    <item><name|Qtwk> (<verbatim|qtwk>): <name|Qt> as a platform layer
    only, with the <name|Widkit> widgets of <TeXmacs>.

    <item><name|X11> (<verbatim|x11>) and <name|SDL> (<verbatim|sdl>,
    <name|SDL> 3 and <name|MuPDF>): the <name|Widkit> widgets on these
    window systems.

    <item><name|Vue> (<verbatim|vue>): <name|SDL> 3 windows, widgets drawn
    in immediate mode by <name|Clay>, documents drawn by <name|MuPDF> or on
    the GPU; see <hlink|the Vue port|guiports-vue.en.tm>.

    <item><name|Cocoa> (<verbatim|cocoa> or <verbatim|aqua>): the native
    <name|macOS> port in <source-link|Plugins/NS|src/Plugins/NS>; see <hlink|the Cocoa
    port|guiports-cocoa.en.tm>.
  </itemize>

  The first chapter describes the abstract widget system and its
  <name|Qt> implementation; the second one builds the interface as
  <TeXmacs> documents instead (<hlink|the graphical user interface through
  markup|gui-markup.en.tm>); the third one compares the ports and explains
  how a port is selected and built; the last one describes the historical
  <name|Widkit> toolkit, which is used by the <name|X11>, <name|SDL> and
  <name|Qtwk> ports.

  The windows of <TeXmacs> documents, and the editors which they contain,
  are described in <hlink|the server: buffers, views and
  windows|server.en.tm>.

  <\traverse>
    <branch|The abstract widget system|widgets.en.tm>

    <branch|The graphical user interface through markup
    (experimental)|gui-markup.en.tm>

    <branch|The graphical user interface ports|guiports.en.tm>

    <branch|Handwriting recognition (experimental)|handwriting.en.tm>

    <branch|The graphical user interface (historical Widkit
    toolkit)|gui.en.tm>
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
