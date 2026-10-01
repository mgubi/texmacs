<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|About the source code of <TeXmacs>>

  This part of the documentation describes the internals of <TeXmacs>: the
  <c++> kernel and the <scheme> code which is closely tied to it. It is
  meant for developers who want to understand, debug or extend the program
  itself. The first two chapters give the general architecture and the
  basic data types used everywhere; the other chapters are grouped by
  subsystem.

  <\traverse>
    <branch|General architecture of <TeXmacs>|architecture.en.tm>

    <branch|Building <TeXmacs> and running the tests|build.en.tm>

    <branch|Basic data types|types.en.tm>

    <branch|Strings, characters and encodings|strings.en.tm>

    <branch|The <scheme> interpreter and the <c++>/<scheme> glue|scheme-bridge.en.tm>

    <branch|The system layer: files, URLs, caches and platform support|system.en.tm>

    <branch|Documents, typesetting and rendering|source-documents.en.tm>

    <branch|Fonts|source-fonts.en.tm>

    <branch|The editor and the user interface|source-editing.en.tm>

    <branch|Data formats, converters and databases|source-data.en.tm>

    <branch|Plug-ins, collaboration and remote services|source-external.en.tm>

    <branch|The documentation system|docsys.en.tm>
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
</collection>>
