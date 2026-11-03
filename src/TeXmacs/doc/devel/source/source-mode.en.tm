<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Source mode, the macro editor and the shortcut editor>

  <section|Introduction>

  <TeXmacs> documents and style files are trees, and the editor can show a
  tree either as it is typeset or as its source: tags with their names and
  arguments, which can be edited structurally. The same machinery serves
  several purposes: editing style files and packages (<verbatim|.ts>
  files), editing the preamble or the whole source tree of a document,
  showing a single tag as source (<markup|inactive>), entering new tags by
  name with <key|\\>, and the dialogs which edit one macro or one keyboard
  shortcut.

  This chapter describes how source is displayed, how it is edited and how
  the macro and shortcut editors work. The user's view of the rendering
  options is explained in <hlink|rendering of style files and
  packages|../style/presentation/src-present.en.tm>, and the rewriting of
  inactive markup inside the typesetter in <hlink|macro expansion:
  inactive markup|macro-expansion-typeset.en.tm>.

  <section|Overview>

  <\description>
    <item*|Display>A document is shown as source when the environment
    variable <verbatim|preamble> is <verbatim|true> (the <tmstyle|source>
    style and <menu|Document|Source|Edit source tree> set it), and a single
    subtree when it is wrapped in <markup|inactive> or <markup|inactive*>.
    The typesetter rewrites such trees into ordinary markup which draws the
    tags.

    <item*|Editing>The <c++> class <cpp|edit_dynamic_rep> creates tags,
    activates them and inserts or removes arguments; the <scheme> module
    <verbatim|(source source-edit)> binds these commands to <key|return>,
    <key|tab> and the source menus.

    <item*|Macros>The macro editor (<verbatim|(source macro-widgets)>) edits
    one definition in a small auxiliary buffer and writes it back to the
    preamble; <verbatim|(source macro-edit)> finds definitions in the
    document and in style files, and <verbatim|(source macro-search)>
    collects the style options and parameters of a tag.

    <item*|Shortcuts>The shortcut editor (<verbatim|(source shortcut-edit)>)
    keeps the user's own keyboard shortcuts in
    <verbatim|$TEXMACS_HOME_PATH/system/shortcuts.scm>.
  </description>

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|Typeset/Bridge/bridge.cpp|src/Typeset/Bridge/bridge.cpp>,
    <source-link|Typeset/Env/env_inactive.cpp|src/Typeset/Env/env_inactive.cpp>,
    <source-link|Typeset/Concat/concat_inactive.cpp|src/Typeset/Concat/concat_inactive.cpp>>Inactive
    bridges, the rewriting of inactive markup and the drawing of source
    tags.

    <item*|<source-link|Edit/Modify/edit_dynamic.cpp|src/Edit/Modify/edit_dynamic.cpp>>Creating
    and activating tags, <key|\\> commands and symbols, inserting and
    removing arguments.

    <item*|<source-link|source/source-edit.scm|TeXmacs/progs/source/source-edit.scm>,
    <source-link|source-kbd.scm|TeXmacs/progs/source/source-kbd.scm>,
    <source-link|source-menu.scm|TeXmacs/progs/source/source-menu.scm>>Editing
    commands, keyboard and menus of source mode, extraction of style files.

    <item*|<source-link|source/macro-edit.scm|TeXmacs/progs/source/macro-edit.scm>,
    <source-link|macro-widgets.scm|TeXmacs/progs/source/macro-widgets.scm>,
    <source-link|macro-search.scm|TeXmacs/progs/source/macro-search.scm>,
    <source-link|macro-menu.scm|TeXmacs/progs/source/macro-menu.scm>>The
    macro editors and the search of definitions, options and parameters.

    <item*|<source-link|source/shortcut-edit.scm|TeXmacs/progs/source/shortcut-edit.scm>,
    <source-link|shortcut-widgets.scm|TeXmacs/progs/source/shortcut-widgets.scm>>User
    keyboard shortcuts.

    <item*|<source-link|styles/source.ts|TeXmacs/styles/source.ts>,
    <source-link|generic/document-edit.scm|TeXmacs/progs/generic/document-edit.scm>,
    <source-link|generic/document-part.scm|TeXmacs/progs/generic/document-part.scm>>The
    <tmstyle|source> style, the source tree mode and the preamble of a
    document.
  </description-paragraphs>

  <\traverse>
    <branch|Displaying source|source-mode-display.en.tm>

    <branch|Editing source|source-mode-editing.en.tm>

    <branch|The macro and shortcut editors|source-mode-macros.en.tm>

    <branch|Pitfalls|source-mode-pitfalls.en.tm>
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
