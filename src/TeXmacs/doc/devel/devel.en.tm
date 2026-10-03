<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <TeXmacs> developer documentation>

  This is the entry point to the documentation for people who want to go
  beyond the use of <TeXmacs> as an editor. It is organized from the outside
  in:

  <\description>
    <item*|The document format>How <TeXmacs> documents are represented as
    trees, which environment variables and primitives exist, and the
    language in which style files are written. This is the reference for
    anyone who writes style files or converts documents.

    <item*|Style files>How to write and customize style files and packages,
    with examples.

    <item*|<scheme> programming>The <scheme> extension language and its
    interface to <TeXmacs>: editing routines, buffers, the graphical user
    interface, bibliography styles, the <TeXmacs> file system and other
    <abbr|API>s.

    <item*|Plug-ins and interfaces>How to package extensions as plug-ins and
    how to connect <TeXmacs> to computer algebra systems and other external
    programs.

    <item*|The source code>The internals of the <c++> kernel and of the
    <scheme> code which is closely tied to it: data types and the system
    layer, typesetting, fonts, the server and the editor, the graphical
    user interface, converters, plug-ins and collaboration.
  </description>

  The first chapters only require some familiarity with <TeXmacs> itself;
  the last one is meant for developers who work on the program.

  <\traverse>
    <branch|The <TeXmacs> document format|format/format.en.tm>

    <branch|Writing <TeXmacs> style files|style/style.en.tm>

    <branch|The <TeXmacs> <scheme> developer guide|scheme/scheme.en.tm>

    <branch|The <TeXmacs> plug-in system|plugin/plugins.en.tm>

    <branch|Interfacing <TeXmacs> with other
    programs|interface/interface.en.tm>

    <branch|About the source code of <TeXmacs>|source/source.en.tm>
  </traverse>

  An older tutorial on writing interfaces, built around the
  <verbatim|mycas> example, is still available as <hlink|interfacing
  <TeXmacs> with other programs (older introduction)|plugin/plugin.en.tm>.

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
