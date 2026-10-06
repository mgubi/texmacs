<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Adding a language, performance and pitfalls>

  <section|Adding a new language>

  This section walks through the addition of a hypothetical language
  <verbatim|lua> with the generic highlighter <cpp|prog_language_rep>, which
  requires no C++ changes. The file names follow the conventions of the
  existing languages of the <verbatim|code> plugin; all code shown is an
  example to be adapted, not existing code.

  <subsection|Step 1: the language definition>

  Create <verbatim|src/plugins/code/progs/lua-lang.scm>. The module name
  must be <verbatim|(lua-lang)>, since <cpp|prog_language_rep> loads it with
  <scm|(use-modules (lua-lang))>, and the file must be found by
  <cpp|prog_lang_exists> (here: in <verbatim|$TEXMACS_PATH/plugins/code/progs/>
  after installation).

  <\scm-code>
    (texmacs-module (lua-lang)

    \ \ (:use (prog default-lang)))

    \;

    (tm-define (parser-feature lan key)

    \ \ (:require (and (== lan "lua") (== key "keyword")))

    \ \ `(,(string-\<gtr\>symbol key)

    \ \ \ \ (constant "true" "false" "nil")

    \ \ \ \ (declare_function "function")

    \ \ \ \ (keyword "local" "end" "in" "and" "or" "not")

    \ \ \ \ (keyword_conditional "if" "then" "else" "elseif" "for" "while"

    \ \ \ \ \ \ "do" "repeat" "until" "break")

    \ \ \ \ (keyword_control "return" "goto")))

    \;

    (tm-define (parser-feature lan key)

    \ \ (:require (and (== lan "lua") (== key "operator")))

    \ \ `(,(string-\<gtr\>symbol key)

    \ \ \ \ (operator "+" "-" "*" "/" "%" "^" "#" ".."

    \ \ \ \ \ \ "==" "~=" "\<less\>=" "\<gtr\>=" "\<less\>" "\<gtr\>" "=")

    \ \ \ \ (operator_field "." ":")

    \ \ \ \ (operator_openclose "{" "[" "(" ")" "]" "}")))

    \;

    (tm-define (parser-feature lan key)

    \ \ (:require (and (== lan "lua") (== key "number")))

    \ \ `(,(string-\<gtr\>symbol key)

    \ \ \ \ (bool_features "prefix_0x" "sci_notation")))

    \;

    (tm-define (parser-feature lan key)

    \ \ (:require (and (== lan "lua") (== key "string")))

    \ \ `(,(string-\<gtr\>symbol key)

    \ \ \ \ (bool_features "hex_with_8_bits")

    \ \ \ \ (escape_sequences "\\\\" "\\"" "'" "a" "b" "f" "n" "r" "t" "v")))

    \;

    (tm-define (parser-feature lan key)

    \ \ (:require (and (== lan "lua") (== key "comment")))

    \ \ `(,(string-\<gtr\>symbol key)

    \ \ \ \ (inline "--")))

    \;

    (define (notify-lua-syntax var val)

    \ \ (syntax-read-preferences "lua"))

    \;

    (define-preferences

    \ \ ("syntax:lua:comment" "brown" notify-lua-syntax)

    \ \ ("syntax:lua:keyword" "#309090" notify-lua-syntax))
  </scm-code>

  Note that the word operators <verbatim|and>, <verbatim|or> and
  <verbatim|not> are declared as keywords rather than operators: operators
  are matched before keywords and without regard to word boundaries (see
  the pitfalls below). Lua block comments <verbatim|--[[ ... ]]> cannot be
  expressed; only <verbatim|/* ... */> is supported for multi-line comments.

  <subsection|Step 2: the format>

  <cpp|prog_language> only builds a <cpp|prog_language_rep> if the format
  exists. Declare it next to the other formats of the plugin in
  <source-link|src/plugins/code/progs/code-format.scm|plugins/code/progs/code-format.scm>:

  <\scm-code>
    (define-format lua

    \ \ (:name "Lua source code")

    \ \ (:suffix "lua"))

    \;

    (define (texmacs-\<gtr\>lua x . opts)

    \ \ (texmacs-\<gtr\>verbatim x (acons "texmacs-\<gtr\>verbatim:encoding" "SourceCode" '())))

    \;

    (define (lua-\<gtr\>texmacs x . opts)

    \ \ (code-\<gtr\>texmacs x))

    \;

    (define (lua-snippet-\<gtr\>texmacs x . opts)

    \ \ (code-snippet-\<gtr\>texmacs x))

    \;

    (converter texmacs-tree lua-document (:function texmacs-\<gtr\>lua))

    (converter lua-document texmacs-tree (:function lua-\<gtr\>texmacs))

    (converter texmacs-tree lua-snippet (:function texmacs-\<gtr\>lua))

    (converter lua-snippet texmacs-tree (:function lua-snippet-\<gtr\>texmacs))
  </scm-code>

  and add <verbatim|lua> to the corresponding line of
  <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>:

  <\scm-code>
    (lazy-format (code-format) cpp julia scala java json csv lua)
  </scm-code>

  With the format in place, opening a <verbatim|.lua> file gives a buffer in
  programming mode with <src-var|prog-language> set to <verbatim|lua>
  (<cpp|attach_subformat>), and the snippet converters are used by copy and
  paste in programming mode.

  <subsection|Step 3: markup>

  Add an inline and a block macro to
  <source-link|src/TeXmacs/packages/environment/env-program.ts|TeXmacs/packages/environment/env-program.ts>:

  <\tm-fragment>
    <inactive*|<assign|lua-lang|<macro|body|<with|mode|prog|prog-language|lua|font-family|rm|<arg|body>>>>>

    <inactive*|<assign|lua-code|<macro|body|<pseudo-code|<lua-lang|<arg|body>>>>>>
  </tm-fragment>

  (in the existing file, the block macros are written in block form). Then
  register the tags in the groups <scm|inline-code-tag> and
  <scm|block-code-tag> of <source-link|text/text-drd.scm|TeXmacs/progs/text/text-drd.scm>, so that generic
  editing functions recognize them as code, and add entries
  <scm|("Lua" (make 'lua-lang))> and <scm|("Lua" (make 'lua-code))> to
  <scm|code-menu> in <source-link|text/text-menu.scm|TeXmacs/progs/text/text-menu.scm>.

  <subsection|Step 4: editing support (optional)>

  Add mode predicates to <source-link|kernel/texmacs/tm-modes.scm|TeXmacs/progs/kernel/texmacs/tm-modes.scm>:

  <\scm-code>
    (in-lua% (== (get-env "prog-language") "lua"))

    (in-prog-lua% #t in-prog% in-lua%)
  </scm-code>

  Create <verbatim|prog/lua-edit.scm> and add <verbatim|(prog lua-edit)> to
  the <scm|:use> list of <source-link|prog/prog-kbd.scm|TeXmacs/progs/prog/prog-kbd.scm>:

  <\scm-code>
    (texmacs-module (prog lua-edit)

    \ \ (:use (prog prog-edit)))

    \;

    (tm-define (program-compute-indentation doc row col)

    \ \ (:mode in-prog-lua?)

    \ \ (if (\<less\>= row 0) 0

    \ \ \ \ \ \ (let* ((r (or (program-row (- row 1)) ""))

    \ \ \ \ \ \ \ \ \ \ \ \ \ (i (string-get-indent r))

    \ \ \ \ \ \ \ \ \ \ \ \ \ (tr (tm-string-trim r)))

    \ \ \ \ \ \ \ \ (if (or (string-ends? tr "then") (string-ends? tr "do")

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (string-prefix? "function" tr))

    \ \ \ \ \ \ \ \ \ \ \ \ (+ i (get-tabstop))

    \ \ \ \ \ \ \ \ \ \ \ \ i))))

    \;

    (tm-define (notify-cursor-moved status)

    \ \ (:require prog-highlight-brackets?)

    \ \ (:mode in-prog-lua?)

    \ \ (select-brackets-after-movement "([{" ")]}" "\\\\"))

    \;

    (kbd-map

    \ \ (:mode in-prog-lua?)

    \ \ ("(" (bracket-open "(" ")" "\\\\"))

    \ \ (")" (bracket-close "(" ")" "\\\\"))

    \ \ ("\\"" (bracket-open "\\"" "\\"" "\\\\")))
  </scm-code>

  <subsection|Step 5: checking>

  After rebuilding and installing (the plugin files and packages are copied
  by the build), evaluate <scm|(format? "lua")> in a <scheme> session: it
  must return <scm|#t>. Insert a <markup|lua-code> block and type some code;
  if nothing is colored, the language most probably fell back to
  <cpp|verb_language_rep>. Enabling <scm|(debug-set "parser" #t)> before the
  language is first used prints the configuration of the string and
  preprocessor parsers on the debugging console. Remember that the language
  object, and hence the result of the format test, is cached for the whole
  session: restart <TeXmacs> after changing the definitions.

  <subsection|Alternatives>

  A user can also provide a language without modifying <TeXmacs>, as a
  plugin in <verbatim|$TEXMACS_HOME_PATH/plugins/lua/progs/>: the file
  <verbatim|lua-lang.scm> there is found by <cpp|prog_lang_exists>, and the
  plugin initialization must declare the format with <scm|define-format>
  and define the markup in a style package of the plugin.

  If the generic highlighter is not expressive enough (other string
  delimiters, nested comments, context dependent coloring, colors that
  follow themes), write a C++ class derived from
  <cpp|abstract_language_rep> as for <source-link|cpp_language.cpp|src/System/Language/cpp_language.cpp>: declare it
  in <source-link|System/Language/impl_language.hpp|src/System/Language/impl_language.hpp>, implement
  <cpp|advance>, <cpp|get_hyphens>, <cpp|hyphenate> and <cpp|get_color>, and
  add a test for its name to <cpp|prog_language> before the generic case.
  The sources of <verbatim|System/Language/> are collected by a glob in
  <source-link|src/CMakeLists.txt|src/CMakeLists.txt>, so it suffices to re-run <name|CMake>.
  Return names of environment variables from <cpp|get_color> to obtain
  theme-aware colors.

  <section|Performance considerations>

  Highlighting happens during typesetting, so its cost is paid for every
  line which is (re)typeset. The typesetter only re-typesets modified
  paragraphs, and every line of a program is a paragraph, so editing usually
  costs little; the cost matters when a long file is typeset as a whole
  (loading, changing the style or the page width, zooming).

  <\itemize>
    <item><em|Caching of language objects.> <cpp|prog_language> is a
    hash-table lookup after the first call; the expensive construction
    (loading the <scheme> module, evaluating <scm|parser-feature> six
    times) happens once per language and session. The environment only calls
    <cpp|prog_language> when <src-var|mode> or <src-var|prog-language>
    changes.

    <item><em|Caching of colors.> Decoded colors are cached per language in
    <cpp|color_decoding>. The <scheme> highlighter caches the result of
    <scm|defined?> for every symbol in <cpp|colored>.

    <item><em|Per-token rescanning.> For each token,
    <cpp|prog_language_rep::get_color> scans the line from its beginning
    for an inline comment start, and <cpp|in_comment> may scan the current
    line and all previous lines up to the nearest <verbatim|/*>, and then
    following lines up to a <verbatim|*/>. The C++ highlighter re-parses the
    line from the start for every token as well. The cost of a line is
    therefore at least quadratic in its length, and, in a long file with
    few comments, each token may cause a scan over many lines.

    <item><em|Operator and string tables.> <cpp|operator_parser_rep> and
    <cpp|string_parser_rep> iterate over all their entries at every
    position where they are tried; very large operator tables are slow.

    <item><em|Packrat highlighting.> The packrat mechanism parses the whole
    document with the start symbol, but stores the results in highlight
    observers and only re-parses the range of lines which lost their
    highlighting after a modification.
  </itemize>

  <section|Pitfalls and known problems>

  The following list is based on reading the current code; it is meant to
  help developers interpret surprising colors.

  <\itemize>
    <item><em|Two different language mechanisms coexist.> A language name
    may be handled by a hard-wired C++ class, by the generic
    <cpp|prog_language_rep>, or by the fallback. In particular
    <source-link|cpp-lang.scm|plugins/code/progs/cpp-lang.scm> does not influence the highlighting of C++, and
    languages with a <verbatim|-lang.scm> file but without a format
    (currently <verbatim|javascript>, <verbatim|dot>, <verbatim|octave>) are
    not highlighted.

    <item><em|Cached decisions.> Once <cpp|prog_language> has created an
    object for a name, it is used for the rest of the session. A
    <scm|parser-feature> or format defined later is ignored.

    <item><em|Keywords.> <cpp|keyword_parser_rep> reads a run of ASCII
    letters. Keywords containing digits, underscores or spaces (for
    instance <verbatim|wchar_t>, <verbatim|__debug__> or <verbatim|abstract
    type>) never match, and the letter prefix of an identifier is colored if
    it is a keyword: in <verbatim|for_each>, the part <verbatim|for> is
    colored as a keyword.

    <item><em|Word operators.> Operators are tried before keywords and
    identifiers and without checking word boundaries. With
    <source-link|python-lang.scm|plugins/python/progs/python-lang.scm>, which lists <verbatim|and>, <verbatim|or>
    and <verbatim|not> as operators, the beginning of identifiers like
    <verbatim|order> or <verbatim|notify> is colored as an operator.

    <item><em|Unknown color classes.> A group name not known to
    <cpp|encode_color> is drawn in the <verbatim|syntax:<em|lan>:none>
    color, red by default (for instance <verbatim|operator_decoration>).

    <item><em|Comments and strings.> The inline comment test in
    <cpp|prog_language_rep::get_color> does not know about strings, so that
    a <verbatim|#> inside a <name|Python> string turns the rest of the line
    into a comment. Multi-line comments are always C style. The detection of
    multi-line comments only works when the lines are the direct string
    children of a <markup|document> in the edited tree; it fails in inline
    code and for lines containing markup. It is also heuristic: a line such
    as <verbatim|/* a */ x = 1;> followed, somewhere below, by a line
    containing <verbatim|*/> has its code colored as a comment.

    <item><em|Stale colors across lines.> Inserting or removing
    <verbatim|/*> or <verbatim|*/> changes the color of other lines, but only
    the modified line is re-typeset; the other lines are updated when they
    are typeset again.

    <item><em|State shared across strings.> The string parser of a language
    object keeps its state between calls. An unterminated string literal
    therefore continues into the next string typeset with the same
    language, which may belong to another line, another code block or even
    another document, depending on the typesetting order.

    <item><em|Escape sequences.> In <cpp|escaped_char_parser_rep>, escape
    sequences of more than one character (such as <verbatim|newline> in
    <source-link|python-lang.scm|plugins/python/progs/python-lang.scm> and <source-link|julia-lang.scm|plugins/code/progs/julia-lang.scm>) advance one
    character too little, and octal escapes are never recognized because
    <cpp|can_parse> does not test for them.

    <item><em|Variable names versus macros.> The names returned by
    <cpp|get_color> are looked up with <cpp|env-\<gtr\>provides>. If a
    package defines a macro with the same name, its value is not a color.
    For example, <source-link|utilities/comment.ts|TeXmacs/packages/utilities/comment.ts> defines a macro
    <markup|comment-color>, which shadows the variable used for comments in
    <scheme> and C++ code.

    <item><em|Markup names.> <source-link|environment/env-program.ts|TeXmacs/packages/environment/env-program.ts> first
    defines <markup|java>, <markup|python>, <markup|julia>,
    <markup|scala>, <markup|r>, <markup|scilab> and <markup|fortran> as
    names of languages, and then redefines them as inline code macros with
    one argument; only the second definitions are effective. In
    documentation, use <markup|scheme> and <markup|c++> for the names, which
    are not overridden. The groups in <source-link|text/text-drd.scm|TeXmacs/progs/text/text-drd.scm> mention
    inline tags <markup|octave>, <markup|javascript> and <markup|json>,
    whereas the macros are called <markup|octave-lang>,
    <markup|javascript-lang> and <markup|json-lang>.

    <item><em|Editing is separate from highlighting.> Bracket matching and
    indentation work on raw characters and know nothing about strings and
    comments (see the <verbatim|FIXME> in <source-link|prog/python-edit.scm|TeXmacs/progs/prog/python-edit.scm>).
    For languages without an overload of
    <scm|program-compute-indentation>, such as C++, pressing return
    resets the indentation of the new line to zero.
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
