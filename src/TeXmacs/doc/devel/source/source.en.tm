<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|About the source code of <TeXmacs>>

  This part of the documentation describes the internals of <TeXmacs>: the
  <c++> kernel and the <scheme> code which is closely tied to it. It is
  meant for developers who want to understand, debug or extend the program
  itself. The first part gives the general architecture and explains how
  to build, test and document the program; the other parts are grouped by
  subsystem, roughly from the bottom up: the foundations on which
  everything is built, the document and its typesetting, fonts, the
  server and the editor, the user interface, data formats, and the
  connections to the outside world.

  The names of source files are links: a click opens the file, in
  <TeXmacs> or in the editor chosen in <menu|Developer|Open source links
  with>. The links need the source tree of <TeXmacs>; see <hlink|links to
  the source files|docsys-writing.en.tm> for how files are found and how
  to choose the editor.

  <\traverse>
    <branch|Working on <TeXmacs>|source-working.en.tm>

    <branch|Foundations: data types, strings, <scheme> and the system
    layer|source-foundations.en.tm>

    <branch|Documents, typesetting and rendering|source-documents.en.tm>

    <branch|Fonts|source-fonts.en.tm>

    <branch|The server and the editor|source-editing.en.tm>

    <branch|The graphical user interface|source-gui.en.tm>

    <branch|Data formats, converters and databases|source-data.en.tm>

    <branch|Plug-ins, collaboration and remote services|source-external.en.tm>
  </traverse>

  The document format itself, as seen by authors of documents and style
  files, is described in <hlink|the <TeXmacs> document
  format|../format/format.en.tm>, and the <scheme> programming interface in
  <hlink|the <TeXmacs> <scheme> developer guide|../scheme/scheme.en.tm>.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
  <associate|tmdoc-book-parts|true>
</collection>>
