<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Displaying source>

  <section|Three ways to see source>

  <\description-paragraphs>
    <item*|The whole document>When the environment variable
    <verbatim|preamble> is <verbatim|true>, every bridge of the typesetter
    is built by <cpp|make_inactive_bridge> (<source-link|bridge.cpp:53|src/Typeset/Bridge/bridge.cpp:53>):
    paragraphs of a <markup|document> stay paragraphs, and every other
    subtree is typeset through the macro <cpp|inactive_auto>, that is
    <verbatim|\<less\>rewrite-inactive\|<em|x>\|recurse*\<gtr\>>. The
    <tmstyle|source> style (<source-link|styles/source.ts|TeXmacs/styles/source.ts>),
    which style files and packages use, assigns <verbatim|preamble> and
    sets <verbatim|mode> to <verbatim|src>; <menu|Document|Source|Edit
    source tree> toggles <verbatim|preamble> as an initial value of the
    document (<scm|toggle-source-mode> in <source-link|document-edit.scm|TeXmacs/progs/generic/document-edit.scm>).
    When <verbatim|preamble> is true, <cpp|typeset_exec_until> also sets
    the mode to <verbatim|src> when it computes the environment at the
    cursor (<source-link|edit_typeset.cpp:498|src/Edit/Editor/edit_typeset.cpp:498>).

    <item*|The preamble>The preamble of a document is its first paragraph,
    <markup|hide-preamble> or <markup|show-preamble>, which holds the local
    macro definitions. <menu|Document|Part|Show preamble> (<em|Create
    preamble> when there is none; the <menu|Part> menu is hidden in
    projects) or
    <menu|Source|Edit preamble> (<scm|toggle-preamble-mode>,
    <source-link|document-part.scm|TeXmacs/progs/generic/document-part.scm>)
    changes it into <markup|show-preamble> and hides the rest of the
    document. The macro <markup|show-preamble> sets <verbatim|mode> to
    <verbatim|src> and <verbatim|preamble> to <verbatim|true> for its body,
    while <markup|hide-preamble> only executes the style assignments in it
    (<source-link|std-fold.ts|TeXmacs/packages/standard/std-fold.ts>).

    <item*|A single tag>A subtree wrapped in <markup|inactive> is shown as
    source with its children typeset normally; <markup|inactive*> shows
    the children as source too. Inside source, <markup|active> and
    <markup|style-only> apply the tag to its arguments shown as source,
    while <markup|active*> and <markup|style-only*> typeset the whole
    subtree normally (<source-link|env_inactive.cpp:452|src/Typeset/Env/env_inactive.cpp:452>).
    Outside source, the <markup|active> and <markup|style-only> tags are
    the identity (<source-link|env_default.cpp:425|src/Typeset/Env/env_default.cpp:425>).
  </description-paragraphs>

  <section|From trees to source tags>

  <markup|inactive> and <markup|inactive*> are macros whose body is the
  primitive <markup|rewrite-inactive> with the mode <verbatim|once>,
  <abbr|resp.> <verbatim|recurse>; erroneous markup uses <verbatim|error>.
  <cpp|edit_env_rep::rewrite_inactive> (<source-link|env_inactive.cpp|src/Typeset/Env/env_inactive.cpp>)
  turns the tree into ordinary markup made of the style macros
  <markup|src-regular>, <markup|src-var>, <markup|src-arg>,
  <markup|src-textual>, <markup|src-numeric>, <markup|src-unknown>,
  <markup|src-error>, ... and of
  the four primitives <markup|inline-tag>, <markup|open-tag>,
  <markup|middle-tag> and <markup|close-tag>, whose children are
  <markup|arg> references back to the original tree. Typesetting these
  references gives boxes with the paths of the source, so that the cursor
  moves and edits in the source tree itself. The details of this rewriting,
  and its memoization by the style rewriter, are in <hlink|macro expansion:
  inactive markup|macro-expansion-typeset.en.tm>.

  A tag with inline arguments becomes one <markup|inline-tag>; a tag with
  block arguments (according to the DRD and to <verbatim|src-compact>)
  becomes an <markup|open-tag>, a <markup|middle-tag> between block
  arguments and a <markup|close-tag>, with the arguments as separate
  paragraphs. <cpp|typeset_src_tag> (<source-link|concat_inactive.cpp:168|src/Typeset/Concat/concat_inactive.cpp:168>)
  draws them: the name in the <verbatim|src-tag-color> (blue by default)
  and in sans serif (<cpp|typeset_blue>), and the delimiters as
  <em|ghosts>, characters which are shown but are not part of the
  document.

  <section|The presentation variables>

  The look is controlled by four environment variables, read in
  <cpp|update_src_style>, ... (<source-link|env_semantics.cpp:729|src/Typeset/Env/env_semantics.cpp:729>).
  Unrecognized values are ignored and the previous setting is kept.

  <\description-paragraphs>
    <item*|<verbatim|src-style>>The delimiters: <verbatim|angular>
    (<verbatim|\<less\>name\|a\|b\<gtr\>>, the default),
    <verbatim|scheme> (<verbatim|(name a b)>), <verbatim|latex>
    (<verbatim|name{a}{b}>) or <verbatim|functional>
    (<verbatim|name(a, b)>).

    <item*|<verbatim|src-special>>Which tags get a special rendering
    instead of the generic one: <verbatim|raw> (none), <verbatim|format>
    (formatting tags), <verbatim|normal> or <verbatim|maximal>.

    <item*|<verbatim|src-compact>>When arguments are put on separate lines:
    <verbatim|none>, <verbatim|inline>, <verbatim|normal>,
    <verbatim|inline args> or <verbatim|all>.

    <item*|<verbatim|src-close>>How block tags are closed:
    <verbatim|repeat> (the name again), <verbatim|long>,
    <verbatim|compact> (the default) or <verbatim|minimal>.
  </description-paragraphs>

  They are set for a document in the <em|Preferences> group of
  <menu|Document|Source> (<scm|document-source-preferences-menu> in
  <source-link|document-menu.scm|TeXmacs/progs/generic/document-menu.scm>;
  a dialog in the compressed menus). Locally, <menu|Source|Presentation|Compact>
  and <menu|Source|Presentation|Stretched> insert a <markup|style-with>
  for <verbatim|src-compact>; the other variables need a
  <markup|style-with> written by hand. The user documentation of these settings is
  <hlink|global presentation|../style/presentation/src-present-global.en.tm>.

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
