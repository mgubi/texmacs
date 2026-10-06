<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Source mode: pitfalls>

  The items marked <em|(checked)> were verified on 2026-10-06 by scripts
  which call the editing commands in a headless <TeXmacs>.

  <\itemize>
    <item><em|Named commands take precedence over macros> (checked).
    <cpp|activate_hybrid> first tries the named commands of the keyboard
    tables, so <verbatim|\\alpha> always inserts the symbol
    <math|\<alpha\>>, even in a document which defines a macro
    <verbatim|alpha>; such a macro has to be inserted in another way, for
    instance as an inactive <markup|compound> tag which is then
    activated. The search order is that of
    <hlink|entering a tag by name|source-mode-editing.en.tm>.

    <item><em|Unknown names outside source mode> (checked). Outside source
    mode, <key|\\> followed by an unknown name and <key|return> leaves the
    inactive <markup|hybrid> tag in the document with an error message;
    only source mode creates new tags from unknown names.

    <item><em|Symbol codes above 255> (checked, issue #305 of
    <verbatim|mgubi/texmacs>). <cpp|activate_symbol>
    converts a numeric name with a cast to <cpp|char>
    (<source-link|edit_dynamic.cpp:625|src/Edit/Modify/edit_dynamic.cpp:625>):
    <verbatim|\<less\>symbol\|233\<gtr\>> gives the Cork character 233
    (e with an acute accent), but <verbatim|\<less\>symbol\|300\<gtr\>> gives
    <verbatim|,> (300 modulo 256). Unicode code points are not
    supported; the documentation of <markup|symbol> speaks of ASCII
    codes.

    <item><em|Invalid presentation values are ignored.> An unknown value of
    <verbatim|src-style>, <verbatim|src-special>, <verbatim|src-compact> or
    <verbatim|src-close> keeps the previous setting without a warning
    (<source-link|env_semantics.cpp:729|src/Typeset/Env/env_semantics.cpp:729>).

    <item><em|The macro editor writes to the document.> In the global mode,
    <em|Apply> rewrites the existing <markup|assign> or adds one to the
    preamble (and then also to the master); in the local mode it adds a
    <markup|with> around the tag. It never writes to a style file, so editing a macro of a style package
    creates a local override. Use <menu|Focus|Preferences|Edit source> to
    change the package itself.

    <item><em|Preamble and source tree modes are document settings.>
    <menu|Document|Source|Edit source tree> stores <verbatim|preamble> as an
    initial value of the document, and the preamble mode changes the tree
    (<markup|show-preamble> instead of <markup|hide-preamble>); both are
    saved with the document if it is saved in that state.
  </itemize>

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
