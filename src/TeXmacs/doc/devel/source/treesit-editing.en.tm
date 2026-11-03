<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Editing programs with the syntax tree>

  <section|Where the editing asks the tree>

  The editing of programs by the tree is in
  <source-link|ts-edit.scm|TeXmacs/progs/treesit/ts-edit.scm>. It only
  uses the glue, so that the editing modules load it without the
  generator. They load it through
  <source-link|code-syntax.scm|TeXmacs/progs/code/code-syntax.scm>, never
  directly: in a build with tree-sitter this module uses
  <source-link|ts-edit.scm|TeXmacs/progs/treesit/ts-edit.scm>, otherwise
  <source-link|code-syntax-none.scm|TeXmacs/progs/code/code-syntax-none.scm>,
  whose functions answer that the code has no tree. The callers are
  <source-link|prog-edit.scm|TeXmacs/progs/prog/prog-edit.scm>, the
  indentation of
  <source-link|python-edit.scm|TeXmacs/progs/prog/python-edit.scm>,
  <source-link|java-edit.scm|TeXmacs/progs/prog/java-edit.scm> and
  <source-link|scheme-edit.scm|TeXmacs/progs/prog/scheme-edit.scm>, and
  Return in
  <source-link|session-edit.scm|TeXmacs/progs/dynamic/session-edit.scm>
  and <source-link|program-edit.scm|TeXmacs/progs/dynamic/program-edit.scm>.

  A function of the tree works where tree-sitter colours the code:
  <scm|syntax-cursor-language> gives, in program mode, the language of
  tree-sitter of the <verbatim|prog-language> at the cursor
  (<scm|tree-sitter-program-language>), which exists once the highlighter
  of the language is installed. Everywhere else the functions give
  <scm|#f>, and the callers, which test it first (<scm|(:require
  (syntax-cursor-language))> or <scm|or>), keep their behaviour without the
  tree. The code at the cursor is a region: the document of the line of
  the cursor, or the string itself when it is not a line of a document,
  with the point <scm|(row column)> of the cursor in it.

  <section|Brackets, quotes and closing keywords>

  <\itemize>
    <item>Selection: <scm|syntax-select-enlarge> (for
    <scm|program-select-enlarge> and <scm|kbd-select-enlarge>) selects the
    smallest node around the selection which is larger than it.

    <item>Matching: <scm|syntax-matching-bracket> matches a bracket token
    with the bracket at the other end of its parent node, so that brackets
    in strings and comments, which are not tokens of the tree, match
    nothing; <scm|syntax-select-brackets> shows the pair as the
    alternative selection <verbatim|brackets>.

    <item>Balance (preference <verbatim|prog:balanced brackets>, on by
    default): <scm|bracket-open> and <scm|bracket-close> of
    <source-link|prog-edit.scm|TeXmacs/progs/prog/prog-edit.scm> are redefined under the condition of a tree.
    An opening bracket or quote comes with its closing one (alone before a
    word; around the selection when there is one), a closing bracket is
    typed over the matching one in front of it, Backspace or Delete on one
    of an empty pair deletes both and on one of a pair with content
    selects the pair. In strings and comments (nodes whose types contain
    <verbatim|string>, <verbatim|comment>, <verbatim|char>,
    <verbatim|heredoc> or <verbatim|regex>) they are plain characters, but
    the closing quote of a string is typed over. Pasted text is left as it
    is.

    <item>Closing keywords: after Return (<scm|insert-return>),
    <scm|syntax-close-block> tries the keywords which close blocks in the
    grammar (<verbatim|closers.scm>: <verbatim|end>, <verbatim|fi>,
    <verbatim|done>...) and, in <LaTeX>, the <verbatim|\\end> of the last
    <verbatim|\\begin> of the previous line. Each candidate is tried on a
    copy of the tree (<scm|syntax-region-errors-with>), and the first one
    which leaves fewer errors is inserted on the next line: error recovery
    alone does not tell, since some grammars give a <verbatim|MISSING>
    closer and others an <verbatim|ERROR>. In <abbr|HTML>, the
    <verbatim|\<gtr\>> of a start tag inserts its end tag
    (<scm|syntax-close-tag>), but for void elements and elements which
    have their end tag. Like the balance of the brackets, both depend on
    the preference <verbatim|prog:balanced brackets>
    (<scm|prog-balanced-brackets?>).
  </itemize>

  <section|Indentation>

  <scm|syntax-compute-indentation>, which the definitions of
  <scm|program-compute-indentation> ask first, uses the query
  <verbatim|indents.scm> of the grammar when there is one
  (<scm|syntax-region-indent>, the rules of nvim-treesitter:
  <verbatim|@indent.begin>, <verbatim|.end>, <verbatim|.branch>,
  <verbatim|.dedent>, <verbatim|.align>, <verbatim|.auto>,
  <verbatim|.ignore>, <verbatim|.zero>, with their properties). The query
  of <scheme> is generated from the keywords of <source-link|tm-mode.el|TeXmacs/progs/tm-mode.el>
  (<source-link|gen-scheme-indents.py|misc/tree-sitter/gen-scheme-indents.py>):
  every list is <verbatim|@indent.lisp>, and <cpp|lisp_indent> applies on
  the tree the rules which <source-link|scheme-edit.scm|TeXmacs/progs/prog/scheme-edit.scm> applies on the
  characters (aligned with the first argument on the line of the last
  one, or the column of the operator plus 3 for the first arguments of a
  keyword and plus 1 for its body), which the captures of nvim-treesitter
  cannot say. The grammars without such a query have rules in
  <scm|syntax-indent-rules>: <abbr|YAML> and <LaTeX> (indenting node
  types, branches, the brackets of an offside language), Lean, and a rule
  of layout for Haskell. Columns are counted as the core counts them
  (<cpp|code_column>: an entity is one column, a tab goes to its stop).

  <section|Moves, structural edits and comments>

  The structured moves of <TeXmacs> in program code (<scm|traverse-left>,
  <scm|traverse-right>, <scm|traverse-up>, <scm|traverse-down>) go from
  node to node, to the node around and into it
  (<scm|syntax-move-node>, through <scm|syntax-region-node-move>), and
  <scm|traverse-next> and <scm|traverse-previous> from definition to
  definition (<scm|syntax-move-definition>). The structural edits take
  their ranges from <scm|syntax-region-node-info>: delete the node at the
  cursor or the one which ends there (<scm|syntax-delete-node>, also for
  the structured removals), swap it with a neighbour, raise it, remove
  the brackets around the cursor (<scm|syntax-splice-brackets>, for
  <scm|program-remove-structure>), wrap it in brackets, comment or
  uncomment it (line comments for whole lines, else the block comment of
  the language). They rewrite the lines of the region as strings, so that
  the brackets stay balanced. <scm|code-syntax-menu> adds these commands to
  the <menu|Focus> menu of program code. <scm|syntax-toggle-comment>
  comments the lines of the selection, with the line comment of the
  language or, without one, a block comment per line.

  <section|Outline, folds, diagnostics and completion>

  The definitions of a region (<scm|syntax-outline>) are the captures
  <verbatim|@definition.\<less\>kind\<gtr\>> of its <verbatim|tags.scm>,
  each with the first <verbatim|@name> inside it, as tree-sitter-tags
  reads them; a definition of the same kind and name just after another
  one (the equations of a Haskell function) is not repeated. Going to the
  definition of the name at the cursor stays in the same code.

  A fold of the tree hides the lines of a node of several lines after its
  first one, but its last line when it holds only the closing token of
  the node. The folds themselves are those of the core
  (<source-link|code-fold.scm|TeXmacs/progs/code/code-fold.scm>, through
  <scm|code-fold-lines> and <scm|code-unfold-lines>), which the language
  servers fold too; the tree only gives the blocks: those around the
  cursor, and the definitions of several lines, or the blocks at the top
  level in a language without <verbatim|tags.scm>.

  <source-link|ts-code.scm|TeXmacs/progs/treesit/ts-code.scm> registers
  these as providers of the features of program code, under the name
  <scm|'syntax> and the rank 20 (after the servers, rank 10, which see the
  other files of a project), available where <scm|syntax-cursor-language>
  is: <scm|'fold-ranges>, <scm|'fold-all-ranges>, <scm|'outline>,
  <scm|'definition>, <scm|'syntax-errors> (the <verbatim|ERROR> and
  <verbatim|MISSING> nodes as items of
  <source-link|code-diagnostics.scm|TeXmacs/progs/code/code-diagnostics.scm>,
  an empty node covering one character), <scm|'completion-context>
  (<scm|'string>, <scm|'comment> or <scm|'code>, from the node before the
  cursor) and <scm|'local-names> (the names of the definitions). How the
  features choose among their providers is described in <hlink|the
  features of program code|code-features.en.tm>.

  <section|Inputs of sessions>

  In a session and in a program field, Return evaluates the input unless
  <scm|syntax-input-incomplete?> says that only more lines can complete
  it: an error or a missing node reaches the end of the text (an open
  bracket), or a named node at the end of the text is empty
  (<scm|syntax-region-last-nodes>: Python accepts <verbatim|def f(x):> at
  the end of a text, with an empty block). Return then opens a new line.
  Other errors go to the interpreter, which reports them in its own
  terms.

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
