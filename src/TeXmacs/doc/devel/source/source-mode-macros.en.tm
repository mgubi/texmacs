<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The macro and shortcut editors>

  <section|Finding definitions>

  <source-link|macro-edit.scm|TeXmacs/progs/source/macro-edit.scm> looks
  for the definition of a tag in two places:

  <\itemize>
    <item><scm|get-definition*> searches the document, descending into
    <markup|document>, <markup|concat>, <markup|surround>, <markup|with>
    and the preamble, for an <verbatim|\<less\>assign\|<em|name>\|...\<gtr\>>.
    <scm|get-definition> falls back on the current value in the
    environment, so that it always returns an <markup|assign>.

    <item><scm|search-style-package> loads the style files and packages of
    the document (<scm|tree-load-style*>), following
    <markup|use-package>, and returns the last one which assigns the name.
  </itemize>

  <scm|edit-macro-source> (<menu|Focus|Preferences|Edit source>) goes to
  the definition: in the preamble, which is shown for the purpose, or in
  the <verbatim|.ts> file found on <verbatim|$TEXMACS_STYLE_PATH>, which is
  opened in a new buffer.

  <section|The macro editor>

  <scm|open-macro-editor> (<source-link|macro-widgets.scm|TeXmacs/progs/source/macro-widgets.scm>)
  edits one macro in a dialog, or in the side tool when side tools are
  enabled. It builds a small document in the auxiliary buffer
  <verbatim|tmfs://aux/edit-<em|name>>, whose master is the edited
  buffer, with the style package <tmpackage|macro-editor>: the preamble of
  the edited document (so that its macros are known) followed by an
  <markup|edit-macro> tag with the name, the arguments and the body of the
  macro, or an <markup|edit-tag> tag for a definition which is not a
  macro. The body is shown in one of three modes, chosen in the dialog:
  <em|Text>, <em|Source> (the body wrapped in <markup|inactive*>) or
  <em|Mathematics> (<markup|edit-math>).

  <em|Apply> (<scm|macro-apply>) reads the definition back and stores it
  according to the mode of the editor:

  <\description-paragraphs>
    <item*|<scm|:global>>The definition replaces the existing
    <markup|assign> of the document wherever it is, or, if there is none, is
    added to its preamble, which is created if needed
    (<scm|macro-set-value>); in the latter case, when the edited buffer has
    a master (a project), the definition is added to the master as well. The
    style files are never modified; a macro defined in a style file is
    overridden by a definition in the preamble.

    <item*|<scm|(:local <em|name>)>>The definition only applies to the tag
    under focus: it is stored in a <markup|with> around that tag
    (<scm|tree-with-set>). This is <menu|Focus|Rendering|Customize macro>.
    The macro-valued parameters of a tag are edited in either mode,
    depending on the menu: <scm|:global> from <menu|Focus|Preferences>,
    <scm|:local> from <menu|Focus|Rendering>.
  </description-paragraphs>

  Variants of the editor:

  <\itemize>
    <item><menu|Source|New macro> starts with an empty name, or with the
    selection as the body;

    <item><menu|Source|Create context macro> (<scm|create-context-macro>)
    makes a macro of one argument from the tags around the cursor
    (<markup|with>, unary tags, table formats, ...), so that the current
    layout can be reused;

    <item><menu|Source|Create table macro> does the same for the format of
    the current table;

    <item><menu|Source|Edit macros> (<scm|open-macros-editor>) shows the
    list of all names defined in the environment (macros of the style and
    of the document, and other variables) in one editor; there,
    <scm|edit-focus-macro> moves to the macro under the cursor and
    <scm|edit-previous-macro> goes back.
  </itemize>

  <section|Options and parameters of a tag>

  The focus menus of a tag offer its <em|style options> and its
  <em|parameters>, both computed by <source-link|macro-search.scm|TeXmacs/progs/source/macro-search.scm>
  from the definitions of the tag and of the macros it expands to.
  <scm|search-tag-options> collects the style packages declared for each
  of these tags with <scm|standard-options> (for instance the packages
  which change the rendering of theorems); <scm|search-tag-parameters>
  collects the environment variables which the macros read, so that they
  can be changed for the document (<scm|:global>) or around the tag. Both
  results are cached per tag during one search.

  <section|User keyboard shortcuts>

  <source-link|shortcut-edit.scm|TeXmacs/progs/source/shortcut-edit.scm>
  keeps a list of pairs (<em|key sequence>, <em|command>), where the
  command is <scheme> code in a string, in
  <verbatim|$TEXMACS_HOME_PATH/system/shortcuts.scm>. Shortly after
  start-up (when the editor is first idle), if the file exists, <scm|init-user-shortcuts> reads it and defines each pair with
  <scm|kbd-map> (<source-link|init-texmacs.scm:306|TeXmacs/progs/init-texmacs.scm:306>);
  <scm|set-user-shortcut> and <scm|remove-user-shortcut> update the list,
  save the file and apply the change with <scm|kbd-map> or
  <scm|kbd-unmap>. The shortcuts are defined without a mode condition.

  The shortcut editor (<source-link|shortcut-widgets.scm|TeXmacs/progs/source/shortcut-widgets.scm>)
  records the keys in a <markup|preview-shortcut> tag: while the cursor is
  in such a tag, <scm|keyboard-press> is overloaded to append each key to
  it instead of executing it. The macro editor and the focus menus open
  it with the command <verbatim|(make '<em|name>)>, to create a shortcut
  which inserts the tag.

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
