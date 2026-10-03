<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The graphical user interface>

  These chapters describe how <TeXmacs> builds its graphical user
  interface. All menus, toolbars, side panes and dialogs are described by
  <em|abstract widgets>, which are mostly built from <scheme> and
  translated into native widgets by a <em|port> for a particular toolkit.
  The first chapter describes the abstract widget system and its
  <name|Qt> implementation; the second one compares the ports (<name|Qt>,
  <name|X11>/<name|Widkit>, <name|Cocoa> and others) and explains how a
  port is selected and built; the last one describes the historical
  <name|Widkit> toolkit, which is only used when <TeXmacs> is configured
  with the <name|X11> interface.

  The windows of <TeXmacs> documents, and the editors which they contain,
  are described in <hlink|the server: buffers, views and
  windows|server.en.tm>.

  <\traverse>
    <branch|The abstract widget system|widgets.en.tm>

    <branch|The graphical user interface ports|guiports.en.tm>

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
