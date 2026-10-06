<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Editing support for code>

  <section|Overview>

  The editing behaviour inside code is implemented entirely in <scheme>, in
  the directory <verbatim|prog/>. It does not use the C++ language objects
  of the previous chapters: there is no shared tokenizer between highlighting
  and editing. The main files are:

  <\description-paragraphs>
    <item*|<source-link|prog/prog-edit.scm|TeXmacs/progs/prog/prog-edit.scm>>Generic routines: access to the
    lines of a program, preferences for brackets, bracket insertion,
    highlighting and selection, tab stops, the indentation framework, and
    copy and paste.

    <item*|<source-link|prog/prog-kbd.scm|TeXmacs/progs/prog/prog-kbd.scm>>Keyboard bindings in programming
    mode and per language. This module is loaded lazily the first time the
    cursor is in programming mode
    (<scm|(lazy-keyboard (prog prog-kbd) in-prog?)> in
    <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>), and it loads all the language specific
    edit modules.

    <item*|<source-link|prog/scheme-edit.scm|TeXmacs/progs/prog/scheme-edit.scm>, <source-link|cpp-edit.scm|TeXmacs/progs/prog/cpp-edit.scm>,
    <source-link|python-edit.scm|TeXmacs/progs/prog/python-edit.scm>, <source-link|fortran-edit.scm|TeXmacs/progs/prog/fortran-edit.scm>,
    <source-link|java-edit.scm|TeXmacs/progs/prog/java-edit.scm>, <source-link|scala-edit.scm|TeXmacs/progs/prog/scala-edit.scm>,
    <source-link|dot-edit.scm|TeXmacs/progs/prog/dot-edit.scm>>Language specific indentation and bracket
    handling.

    <item*|<source-link|prog/scheme-tools.scm|TeXmacs/progs/prog/scheme-tools.scm>,
    <source-link|scheme-autocomplete.scm|TeXmacs/progs/prog/scheme-autocomplete.scm>, <source-link|scheme-menu.scm|TeXmacs/progs/prog/scheme-menu.scm>>Extra
    developer tools for <scheme> code: help on symbols, jump to definition,
    completion, running a <scheme> file.

    <item*|<source-link|prog/prog-menu.scm|TeXmacs/progs/prog/prog-menu.scm>>The <menu|Format> menu and the icon
    bar in programming mode (loaded lazily as well).
  </description-paragraphs>

  <section|Mode predicates>

  Language specific behaviour is selected with the <scm|:mode> or
  <scm|:require> clauses of <scm|tm-define> and <scm|kbd-map>, using
  predicates defined with <scm|texmacs-modes> in
  <source-link|kernel/texmacs/tm-modes.scm|TeXmacs/progs/kernel/texmacs/tm-modes.scm>:

  <\scm-code>
    (in-prog% (and (== (get-env "mode") "prog") (not (in-graphics?))))

    ...

    (in-python% (== (get-env "prog-language") "python"))

    (in-prog-python% #t in-prog% in-python%)
  </scm-code>

  Each <scm|<em|name>%> entry defines a predicate <scm|<em|name>?>. Pairs
  <scm|in-<em|lan>?> and <scm|in-prog-<em|lan>?> exist for <verbatim|cpp>,
  <verbatim|dot>, <verbatim|octave>, <verbatim|java>, <verbatim|javascript>,
  <verbatim|json>, <verbatim|fortran>, <verbatim|scala>,
  <verbatim|scheme>, <verbatim|python> and <verbatim|julia>. The predicate
  <scm|in-code?> is defined by <scm|(in-code% (style-has?
  "code-style"))>; it is used for the focus bar in
  <source-link|prog/prog-menu.scm|TeXmacs/progs/prog/prog-menu.scm>.

  <section|Programs as documents of lines>

  All routines assume that a program is a <markup|document> whose children
  are strings, one per line, and that the cursor is inside one of these
  strings. They fail (or return <scm|#f>) for code containing markup.

  <\explain>
    <scm|(inside-program?)><explain-synopsis|cursor in a line of a program?>
  <|explain>
    True if the cursor tree is atomic and its parent is a <markup|document>.
  </explain>

  <\explain>
    <scm|(program-tree)><explain-synopsis|the whole program>

    <scm|(program-row <scm-arg|row>)><explain-synopsis|the string of a line>

    <scm|(program-row-number)><explain-synopsis|current line>

    <scm|(program-column-number)><explain-synopsis|current column>

    <scm|(program-go-to <scm-arg|row> <scm-arg|col>)><explain-synopsis|move
    the cursor>
  <|explain>
    Access to the enclosing <markup|document>, to its lines and to the
    cursor position in terms of rows and columns.
  </explain>

  <section|Brackets>

  Three user preferences control bracket handling; they are mirrored in the
  <scheme> variables <scm|prog-auto-close-brackets?>,
  <scm|prog-highlight-brackets?> and <scm|prog-select-brackets?>:

  <\description>
    <item*|<verbatim|prog:automatic brackets>>Insert the closing bracket or
    quote together with the opening one (not after the escape character, and
    around the selection if there is one).

    <item*|<verbatim|prog:highlight brackets>>Highlight the matching pair
    of brackets around the cursor, using the alternative selection named
    <verbatim|"brackets">.

    <item*|<verbatim|prog:select brackets>>Let the \Pselect enlarge\Q
    command (<scm|kbd-select-enlarge>) successively select larger bracketed
    regions.
  </description>

  These preferences can be set in the preferences dialog
  (<source-link|texmacs/menus/preferences-widgets.scm|TeXmacs/progs/texmacs/menus/preferences-widgets.scm>). The core routines are:

  <\explain>
    <scm|(bracket-open <scm-arg|lb> <scm-arg|rb>
    <scm-arg|esc>)><explain-synopsis|insert an opening bracket>

    <scm|(bracket-close <scm-arg|lb> <scm-arg|rb>
    <scm-arg|esc>)><explain-synopsis|insert a closing bracket>
  <|explain>
    Insert the bracket, honouring the preferences above. Each language wraps
    them (<scm|scheme-bracket-open>, <scm|cpp-bracket-open>,
    <scm|python-bracket-open>, <scm|fortran-bracket-open>,
    <scm|dot-bracket-open>, ...) with <verbatim|"\\\\"> as escape character
    and binds them to the bracket keys in a <scm|kbd-map> with the
    appropriate mode.
  </explain>

  <\explain>
    <scm|(select-brackets <scm-arg|path> <scm-arg|lb>
    <scm-arg|rb>)><explain-synopsis|highlight matching brackets>

    <scm|(select-brackets-after-movement <scm-arg|lbs> <scm-arg|rbs>
    <scm-arg|esc>)><explain-synopsis|highlight after cursor movement>
  <|explain>
    The first function finds the innermost pair of brackets around
    <scm-arg|path> with the C++ routines <scm|find-left-bracket> and
    <scm|find-right-bracket> (<cpp|find_left_bracket> and
    <cpp|find_right_bracket> in <source-link|Data/String/analyze.cpp|src/Data/String/analyze.cpp>, which
    count nesting levels and may cross line boundaries within the same
    <markup|document>), and sets the alternative selection
    <verbatim|"brackets">. The second is called from language specific
    overloads of <scm|notify-cursor-moved>, for instance

    <\scm-code>
      (tm-define (notify-cursor-moved status)

      \ \ (:require prog-highlight-brackets?)

      \ \ (:mode in-prog-python?)

      \ \ (select-brackets-after-movement "([{" ")]}" "\\\\"))
    </scm-code>

    and highlights the pair adjacent to the cursor, if any.
  </explain>

  <\explain>
    <scm|(program-select-enlarge <scm-arg|lb> <scm-arg|rb>)><explain-synopsis|enlarge
    the selection to the next brackets>
  <|explain>
    Used by the language specific overloads of <scm|kbd-select-enlarge> for
    <scheme> and C++.
  </explain>

  <scheme>-only helpers <scm|string-bracket-forward>,
  <scm|string-bracket-backward>, <scm|string-bracket-level> and
  <scm|program-previous-match> operate on plain strings and lines.

  <section|Tab stops and indentation>

  The tab width is the preference <verbatim|editor:verbatim:tabstop>
  (default 4), returned by <scm|(get-tabstop)>; <name|Java> and <name|dot>
  override it to 4 and <name|Scala> to 2 through <scm|:mode>.
  <scm|(insert-tabstop)> inserts spaces up to the next multiple of the tab
  width, and <scm|(remove-tabstop)> removes one level of indentation. There
  are no tab characters in programs edited in <TeXmacs>.

  Automatic indentation is organized around one overloadable function:

  <\explain>
    <scm|(program-compute-indentation <scm-arg|doc> <scm-arg|row>
    <scm-arg|col>)><explain-synopsis|indentation of a line>
  <|explain>
    Returns the number of spaces with which line <scm-arg|row> of the
    program <scm-arg|doc> should be indented. The default returns 0. The
    current overloads are:

    <\itemize>
      <item><scheme> (<source-link|prog/scheme-edit.scm|TeXmacs/progs/prog/scheme-edit.scm>): looks back for the
      previous arguments of the enclosing form and uses the indentation arity
      of the head symbol (<scm|indent-get-arity>, from the lists
      <verbatim|nullary-indent>, <verbatim|unary-indent>, ... in
      <verbatim|tm-mode.el>) to decide between aligning with the previous
      argument and indenting by a fixed amount;

      <item><name|Python> (<source-link|prog/python-edit.scm|TeXmacs/progs/prog/python-edit.scm>): the
      indentation of the previous line, plus one tab stop if that line ends
      with <verbatim|:> (after a naive removal of <verbatim|#> comments);

      <item><name|Fortran> (<source-link|prog/fortran-edit.scm|TeXmacs/progs/prog/fortran-edit.scm>): the
      indentation of the previous line, plus one tab stop if that line starts
      with a keyword like <verbatim|function>, <verbatim|program>,
      <verbatim|subroutine>, <verbatim|do> or <verbatim|module>;

      <item><name|Java>, <name|Scala> and <name|dot>: always one tab stop.
    </itemize>

    Other languages, including C++, use the default.
  </explain>

  <\explain>
    <scm|(program-indent-line <scm-arg|doc> <scm-arg|row>
    <scm-arg|unindent?>)><explain-synopsis|re-indent one line>

    <scm|(program-indent <scm-arg|unindent?>)><explain-synopsis|re-indent
    the current line>

    <scm|(program-indent-all <scm-arg|unindent?>)><explain-synopsis|re-indent
    all lines>
  <|explain>
    Set the indentation of lines according to
    <scm|program-compute-indentation>. The flag <scm-arg|unindent?> is not
    implemented yet.
  </explain>

  In programming mode, <scm|insert-return> is redefined to insert a raw line
  break and to re-indent the new line with <scm|(program-indent #f)>. In
  <source-link|prog/prog-kbd.scm|TeXmacs/progs/prog/prog-kbd.scm> the following keys are bound for all
  languages: <key|cmd i> and <key|cmd tab> re-indent the
  current line, <key|cmd A-tab> re-indents the whole program, and
  <key|space var> inserts a tab stop. Several text mode shortcuts are
  overridden so that characters such as <verbatim|$>, <verbatim|\\>,
  <verbatim|"> and the quotes are inserted literally.

  <section|Copy and paste>

  In <source-link|prog/prog-edit.scm|TeXmacs/progs/prog/prog-edit.scm>, <scm|kbd-cut> and <scm|kbd-paste> are
  overloaded in programming mode when the selection (or clipboard) is purely
  textual. They export and import through the converters
  <verbatim|<em|lan>-snippet> if they exist, where <em|lan> is the value of
  <src-var|prog-language>, and through <verbatim|verbatim> otherwise. For
  <scheme>, <scm|kbd-copy> exports to the <verbatim|scheme> format
  (<source-link|prog/scheme-edit.scm|TeXmacs/progs/prog/scheme-edit.scm>). The converters for the languages of the
  <verbatim|code> plugin are defined in
  <source-link|src/plugins/code/progs/code-format.scm|plugins/code/progs/code-format.scm>; they use
  <scm|texmacs-\<gtr\>verbatim> with the <verbatim|SourceCode> encoding and
  <scm|code-\<gtr\>texmacs>.

  <section|<scheme> specific tools>

  When <scm|developer-mode?> is on, <source-link|prog/prog-kbd.scm|TeXmacs/progs/prog/prog-kbd.scm> binds
  <key|A-F1> to <scm|scheme-popup-help>, <key|cmd A-F1> to
  <scm|scheme-inbuffer-help> and <key|std F1> to
  <scm|scheme-go-to-definition>, all applied to the word at the cursor; in a
  <scheme> file, <key|std R> runs the file. Completion (<scm|kbd-variant>, normally
  bound to <key|tab>) uses <scm|scheme-completions> from
  <source-link|prog/scheme-autocomplete.scm|TeXmacs/progs/prog/scheme-autocomplete.scm>, which collects the glued symbols listed in
  <source-link|prog/glue-symbols.scm|TeXmacs/progs/prog/glue-symbols.scm> (<scm|all-glued-symbols>) and the symbols
  used in <scheme> code (<scm|all-used-symbols>).

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
