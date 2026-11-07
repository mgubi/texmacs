<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Pitfalls of the features of program code>

  What went wrong while the features of program code were made, and what to
  keep in mind when they change.

  <section|Builds without a feature>

  <\itemize>
    <item>A feature is there when its glue is (<scm|(defined?
    'tree-sitter-program-language)>, <scm|(defined? 'lsp-start)>), not
    when its <scheme> files are: they are installed in every build.

    <item>The functions of <source-link|ts-edit.scm|TeXmacs/progs/treesit/ts-edit.scm> are public: once it is
    loaded, they replace the stubs of <source-link|code-syntax-none.scm|TeXmacs/progs/code/code-syntax-none.scm> in
    all the modules. A module which <scm|:use>s it directly loads it in a
    build without tree-sitter, whose first call then fails (an unbound glue
    routine): it was so for sessions and program inputs. Use
    <scm|(code code-syntax)>.

    <item>The modules of tree-sitter call its glue when they are loaded: a
    test suite which <scm|:use>s them at the start makes every suite fail
    without tree-sitter. <source-link|check-master.scm|TeXmacs/progs/check/check-master.scm> loads the modules of
    the suites <verbatim|treesit> and <verbatim|lsp> when they run.

    <item>Run the suites in the three builds (both features, without
    tree-sitter, without the servers) after a change of the core: a test
    which expects what one feature does (the quotes which tree-sitter
    balances) checks for the feature.
  </itemize>

  <section|Providers and menus>

  <\itemize>
    <item>In S7, <scm|'()> is true: <scm|code-ask> skips it explicitly, so
    that an empty answer lets the next provider answer.

    <item>In a <scm|tm-menu>, <scm|when> greys its entries and
    <scm|assuming> hides them: a test of a menu sees the labels of the
    greyed entries.

    <item>An overload of <scm|focus-extra-menu> ends with
    <scm|(dynamic (former t))>, and an overload of a <scm|notify-> hook
    calls <scm|(former ...)>: the one which is defined last would else
    replace the others (the entries of sessions and program inputs).

    <item>In <scm|notify-cursor-moved> and <scm|notify-buffer-changed>, the
    mode predicates (<scm|in-prog?>) are those of an environment which is
    not up to date: decide from what is known of the buffer, or later
    (<scm|delayed>).
  </itemize>

  <section|Folds>

  <\itemize>
    <item>The state of a line is kept on its highlight observers: an edit
    of the line forgets it, an edit of another line does not. A line keeps
    at most 9 nested folds (<cpp|CODE_FOLD_MAX>); <scm|code-fold-lines>
    refuses a deeper one.

    <item>The mark of the first line of a fold depends on the next line,
    which is not one of the inputs of its bridge, and is drawn after the
    end of the line: the part of the window which is drawn again after a
    change is that of the line as it was. The commands of the folds draw
    the whole window again once (<scm|refresh-window>, delayed).

    <item>While the document is being modified (the cursor moved into a
    fold by an edit), <scm|tree-\<gtr\>path> of a line is <scm|#f>: its
    update is delayed.

    <item>Deleting the first line of a fold at the start of the code leaves
    hidden lines with no line before them: <scm|code-unfold-all> and the
    cursor which comes there show them.

    <item>The removal of a highlight leaves an empty place in the list of
    observers of a line (<cpp|remove_highlight>): <source-link|code_lines.cpp|src/System/Language/code_lines.cpp>
    makes the list shorter, else a line folded many times would have a long
    one, which every typesetting of the line goes through.
  </itemize>

  <section|Layers and code blocks>

  <\itemize>
    <item>An edit of a line removes its layers but keeps its tree: an undo
    gives it back its text without them. A decoration which is the same as
    before is given again when the line has none (<source-link|lsp-decorate.scm|TeXmacs/progs/lsp/lsp-decorate.scm>).

    <item>The diagnostics are kept as paths: those of the code blocks of a
    document are computed again from their lines when the document changes
    (a paragraph inserted above a block moves them), those of the syntax
    tree are cleared when the cursor leaves the code.

    <item>A code block is known by its tree and its language: a block whose
    language changes gets a new virtual file, the former one is closed.
    The virtual files are named after the document with its suffix (two
    documents <verbatim|notes.tm> and <verbatim|notes.html> do not share
    them).

    <item>Looking for the code blocks walks the whole document in <scheme>:
    it is done at the first sync of a document and at each save; the other
    syncs look at the known blocks and the one at the cursor.

    <item>The lines of a unit are made again from its blocks at each use
    at the cursor (<scm|lsp-unit-set-roots!>), which costs the lines of
    all the blocks of the language: fine for the documents with code which
    people write, not for one with thousands of lines of code in blocks.
    A line of a unit is a line of a block: a range of the server over two
    blocks is drawn from its start, and an edit which would join or split
    blocks is not applied.

    <item>The tests turn the code blocks on with <scm|lsp-blocks-force!>
    and choose the units with <scm|lsp-blocks-join-force!>, not with the
    preferences.
  </itemize>

  <section|In a window>

  <\itemize>
    <item>The scripts of the Vue GUI (<verbatim|TEXMACS_VUE_SCRIPT>) type
    <verbatim|key std (> as a parenthesis: the keys of the folds are
    <verbatim|key M-S-9> and <verbatim|key M-S-0>. What <scm|display*>
    writes there goes to the log of the process.

    <item>A fresh home shows the welcome page instead of the file: start
    from a copy of a home which has been used.
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
