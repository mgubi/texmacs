<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Exporting to <LaTeX>>

  <section|The stages of the export>

  When the user exports a buffer to <LaTeX>, the following stages are
  traversed. The first stages are written in <c++>, the main conversion and
  the serialization in <scheme>.

  <\enumerate>
    <item><cpp|buffer_export> (<verbatim|Texmacs/Data/new_buffer.cpp>)
    attaches the current view to the document (attribute
    <verbatim|view>) and calls <cpp|export_tree>, which calls
    <cpp|tree_to_generic> with the format <verbatim|latex-document>. The
    converter graph selects the converter from <verbatim|texmacs-stree> to
    <verbatim|latex-document>, implemented by
    <scm|conservative-texmacs-\<gtr\>latex>, that is by the <c++> function
    <cpp|conservative_texmacs_to_latex>.

    <item><cpp|conservative_texmacs_to_latex>
    (<verbatim|Data/Convert/Tex/conservative_totex.cpp>) checks whether the
    document was itself imported from <LaTeX> with source tracking; if so,
    unchanged parts of the original <LaTeX> source are reused (see
    <hlink|source tracking and conservative
    conversion|convert-latex-tracking.en.tm>). Otherwise, or after this
    preparation, it calls <cpp|tracked_texmacs_to_latex>
    (<verbatim|tracked_totex.cpp>).

    <item><cpp|tracked_texmacs_to_latex> first calls <cpp|latex_expand>,
    which retrieves the view from the <verbatim|view> attribute and calls
    <cpp|edit_typeset_rep::exec_latex> on the body. This expands the
    <TeXmacs> macros which have no <LaTeX> counterpart (see below). Then it
    calls <cpp|tree_to_latex_document>, which calls the <scheme> function
    <scm|texmacs-\<gtr\>latex-document> from
    <verbatim|convert/latex/init-latex.scm>, possibly several times when
    source tracking is enabled.

    <item><scm|texmacs-\<gtr\>latex-document> converts the tree into an
    stree, calls <scm|texmacs-\<gtr\>latex>
    (<verbatim|convert/latex/tmtex.scm>) to obtain a <LaTeX> stree, and
    serializes it with <scm|serialize-latex>
    (<verbatim|convert/latex/texout.scm>).
  </enumerate>

  Snippets (copying a selection as <LaTeX>) skip the <c++> stages and go
  directly through <scm|texmacs-\<gtr\>latex> and <scm|serialize-latex>
  (converters <verbatim|texmacs-stree> <math|\<rightarrow\>>
  <verbatim|latex-stree> <math|\<rightarrow\>> <verbatim|latex-snippet>).

  <section|Macro expansion before the conversion>

  <LaTeX> does not know most <TeXmacs> macros. Instead of implementing a
  <LaTeX> counterpart for each of them, the exporter can let the
  <TeXmacs> evaluator expand them in the environment of the document, using
  the style files and packages of the document. This is done by

  <\explain>
    <cpp|tree edit_typeset_rep::exec_latex (tree t, path p)><explain-synopsis|expand
    macros unknown to the <LaTeX> converter>
  <|explain>
    Implemented in <verbatim|Edit/Editor/edit_typeset.cpp> and accessible
    from <scheme> as <scm|latex-expand>. Nothing is done unless one of the
    preferences <verbatim|"texmacs-\<gtr\>latex:expand-macros"> or
    <verbatim|"texmacs-\<gtr\>latex:expand-user-macros"> is
    <verbatim|"on">. Otherwise the environment at the start of the document
    is computed (<cpp|typeset_exec_until>), patched with the result of the
    <scheme> function <scm|tmtex-env-patch>, and the body is evaluated with
    <cpp|exec>. Before patching, every variable <verbatim|tmlatex-<em|name>>
    of the environment replaces the variable <verbatim|<em|name>> (function
    <cpp|prefix_specific>): a style file may thus provide <LaTeX>-specific
    variants of its macros, just as it may provide <verbatim|tmhtml-> variants
    for the <name|HTML> export. When user macros are not expanded, a
    <markup|hide-preamble> at the start of the document is evaluated
    separately so that the user's definitions survive.
  </explain>

  The patch returned by <scm|tmtex-env-patch> (end of <verbatim|tmtex.scm>)
  is a collection of <em|identity macros> <scm|(xmacro "x" (eval-args
  "x"))>, which protect a tag from expansion while still evaluating its
  arguments. Protected are:

  <\itemize>
    <item>the tags which have an explicit handler in the <LaTeX> converter
    (the tables <scm|tmtex-extra-methods%> and <scm|tmtex-tmstyle%>, see
    below);

    <item>the <LaTeX> commands known to the <LaTeX> tables
    (<scm|latex-tag%>) which are not symbols;

    <item>the macros defined in the document itself, unless
    <verbatim|"texmacs-\<gtr\>latex:expand-user-macros"> is on;

    <item>all other non-primitive tags which occur literally in the
    document;
  </itemize>

  with the exception of the tags listed in <scm|tmtex-always-expand> (for
  instance the rendering macros of theorems, algorithms, exercises and the
  tags of the <verbatim|tmdoc> style), which are always expanded. All other
  macros, in particular the ones which only appear inside the definitions
  of other macros, are expanded by the evaluator. As a consequence, a macro
  of a style file which is used directly in the document and which has no
  handler in the converter is <em|not> expanded: it is exported as a <LaTeX>
  command with the same name (see <hlink|the default rule|#default-rule>
  below), which has to be defined by the user on the <LaTeX> side.

  <section|The <LaTeX> stree>

  The result of <scm|texmacs-\<gtr\>latex> is a <scheme> expression in
  which ordinary lists <scm|(<em|cmd> <em|arg1> ... <em|argn>)> stand for
  the <LaTeX> command <verbatim|\\<em|cmd>{<em|arg1>}...{<em|argn>}>,
  strings stand for <LaTeX> text, and a number of special labels starting
  with <verbatim|!> stand for other constructs. The serializer
  <scm|texout> in <verbatim|convert/latex/texout.scm> recognizes the
  following labels:

  <\description-paragraphs>
    <item*|<scm|(!file <scm-arg|body> <scm-arg|styles> <scm-arg|needs>
    <scm-arg|init> <scm-arg|preamble>)>>A complete document; serialized by
    <scm|texout-file>, which writes the <verbatim|\\documentclass>, the
    <verbatim|\\usepackage> commands, the language, the <TeXmacs> macro
    definitions and the user's preamble. <scm-arg|needs> is a list of three
    lists: the languages, the colors and the color maps which were used.

    <item*|<scm|!document>, <scm|!paragraph>, <scm|!concat>,
    <scm|!append>>Vertical concatenation (paragraphs separated by blank
    lines), lines inside a paragraph, horizontal concatenation, and plain
    juxtaposition.

    <item*|<scm|((!begin <scm-arg|env> <scm-arg|arg> ...)
    <scm-arg|body>)>>An environment
    <verbatim|\\begin{<em|env>}{<em|arg>}...<em|body>\\end{<em|env>}>;
    <scm|!begin*> is a variant without line breaks.

    <item*|<scm|(!option <scm-arg|x>)>>An optional argument
    <verbatim|[<em|x>]> of the enclosing command.

    <item*|<scm|!math>, <scm|!eqn>>Inline math <verbatim|$...$> and display
    math <verbatim|\\[...\\]>.

    <item*|<scm|!sub>, <scm|!sup>>Subscripts and superscripts.

    <item*|<scm|!group>>A group <verbatim|{...}>.

    <item*|<scm|!table>, <scm|!row>>The contents of a tabular environment.

    <item*|<scm|!verb>, <scm|!verbatim>, <scm|!verbatim*>>Verbatim text.

    <item*|<scm|!symbol>, <scm|!widechar>>A symbol command and a non-ASCII
    character.

    <item*|<scm|!nbsp>, <scm|!nbhyph>, <scm|!newline>, <scm|!nextline>,
    <scm|!linefeed>, <scm|!indent>, <scm|!unindent>>Spacing and layout of
    the output.

    <item*|<scm|!arg>>A macro parameter <verbatim|#<em|n>> (in macro
    definitions).

    <item*|<scm|!comment>, <scm|!preamble>, <scm|!ignore>,
    <scm|!annotate>>Comments, verbatim preamble material, ignored material
    and transparent annotations.

    <item*|<scm|!invariant>, <scm|!marker>>Verbatim <LaTeX> source reused
    by the conservative converter, and the source tracking markers
    <verbatim|{\\btm{...}}> and <verbatim|{\\etm{...}}>.
  </description-paragraphs>

  Any other list is serialized as a command application by
  <scm|texout-apply>. The serializer writes into the output buffer of
  <verbatim|convert/tools/output.scm> (<scm|output-text>,
  <scm|output-verbatim>, <scm|output-lf>, <abbr|etc.>), which takes care of
  indentation and line breaking; <scm|serialize-latex> returns the
  accumulated string via <scm|output-produce>.

  <section|The main converter>

  <subsection|Entry point>

  <\explain>
    <scm|(texmacs-\<gtr\>latex <scm-arg|x> <scm-arg|opts>)><explain-synopsis|convert
    a <TeXmacs> stree into a <LaTeX> stree>
  <|explain>
    If <scm-arg|x> is a complete file (<scm|tmfile?>), the body, the style,
    the language, the initial environment and the attachments are extracted
    and assembled into <scm|(!file <scm-arg|body> <scm-arg|style>
    <scm-arg|lan> <scm-arg|init> <scm-arg|att> <scm-arg|path>)>. The global
    variables <scm|tmtex-style> and <scm|tmtex-packages> are set from the
    style, style-specific modules are imported (<scm|import-tmtex-styles>),
    the hooks <scm|tmtex-style-init> and <scm|tmtex-style-preprocess> are
    called and the function recurses on the <verbatim|!file> expression.
    Otherwise, equation numbers are normalized
    (<scm|tmtm-eqnumber-\<gtr\>nonumber>), brackets are matched
    (<scm|tmtm-match-brackets>), the options are decoded
    (<scm|tmtex-initialize>), the tree is pre-processed by
    <scm|tmpre-produce> (<verbatim|convert/tools/tmpre.scm>) and converted by
    <scm|tmtex>. Finally, if the option
    <verbatim|"texmacs-\<gtr\>latex:use-macros"> is off, the <LaTeX> macros
    introduced by <TeXmacs> are expanded in place by
    <scm|latex-expand-macros>; for <name|MathJax> (option
    <verbatim|"texmacs-\<gtr\>latex:mathjax">, only set by the <name|HTML>
    exporter) the result is cleaned up by <scm|latex-mathjax-pre> and
    <scm|latex-mathjax>.
  </explain>

  The handler of <verbatim|!file> is <scm|tmtex-file>. It separates the
  preamble of the document (macro definitions and the contents of
  <markup|hide-preamble>, see <scm|tmtex-filter-preamble>) from the body,
  converts the <TeXmacs> style into a <LaTeX> document class with
  <scm|tmtex-transform-style> (<abbr|e.g.> <verbatim|generic>,
  <verbatim|tmarticle> and <verbatim|tmdoc> become <verbatim|article>,
  <verbatim|book> and <verbatim|tmbook> become <verbatim|book>; other styles
  are kept only if <verbatim|"texmacs-\<gtr\>latex:replace-style"> is off),
  determines whether user macros are mainly used in text or in math mode
  (<scm|init-mode-stats>), and converts the definitions with
  <scm|tmtex-pre> and the body with <scm|tmtex>. The result is the
  <verbatim|!file> stree described above.

  <subsection|Dispatching>

  The converter proper is the function <scm|tmtex>: strings are converted by
  <scm|tmtex-string>, compound trees by <scm|tmtex-apply>:

  <\scm-code>
    (define (tmtex-apply key args)

    \ \ (let ((n (length args))

    \ \ \ \ \ \ \ \ (r (or (ahash-ref tmtex-dynamic key)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (logic-ref tmtex-methods% key))))

    \ \ \ \ ...

    \ \ \ \ (cond ((== r 'environment)

    \ \ \ \ \ \ \ \ \ \ \ (tmtex-std-env (symbol-\<gtr\>string key) args))

    \ \ \ \ \ \ \ \ \ \ (r (r args))

    \ \ \ \ \ \ \ \ \ \ (else

    \ \ \ \ \ \ \ \ \ \ \ \ (let ((p (logic-ref tmtex-tmstyle% key)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ (cond ((and p (or (= (cadr p) -1) (= (cadr p) n)))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ((car p) (symbol-\<gtr\>string key) args))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ...

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (else (tmtex-function key args))))))))
  </scm-code>

  There are thus four levels of dispatch.

  <\enumerate>
    <item>The hash table <scm|tmtex-dynamic>, filled during the conversion
    by <scm|tmtex-new-theorem>: theorem-like environments declared in the
    document are exported as <LaTeX> environments.

    <item>The logical table <scm|tmtex-methods%>, which is the union of
    <scm|tmtex-primitives%> (the <TeXmacs> primitives: <markup|concat>,
    <markup|frac>, <markup|with>, <markup|table>, <markup|image>,
    <abbr|etc.>) and <scm|tmtex-extra-methods%> (a few extra tags such as
    <markup|wide-float> and the marginal notes). These tables are declared
    with <scm|logic-dispatcher>, which is <scm|logic-table> with the
    implementations unquoted, so that the table stores procedures; see
    <hlink|logical programming extensions|../scheme/utils/utils-logic.en.tm>.
    The handlers take the list of (unconverted) arguments.

    <item>The logical table <scm|tmtex-tmstyle%>, for the tags defined in
    the standard style files (sectioning, theorems, lists, size changes,
    code, citations, <abbr|etc.>). Each entry is a pair
    <scm|(<scm-arg|handler> <scm-arg|arity>)>, where the arity
    <verbatim|-1> accepts any number of arguments; the handler is called with
    the tag name (a string) and the arguments. The arity <verbatim|-2> is
    used for handlers which take the whole tree.

    <item>Fallbacks: a tag whose last argument is a <markup|table> or
    <markup|tformat> is converted into a table environment with
    <scm|tmtex-table-apply>, and <label|default-rule>any other tag is
    converted by <scm|tmtex-function> into a <LaTeX> command whose name is
    computed by <scm|tmtex-var-name>: non-alphanumeric characters are
    dropped, digits are spelled out (<verbatim|my-tag2> becomes
    <verbatim|\\mytagtwo>), and names which would clash with short <TeX>
    or <LaTeX> commands (the group <scm|tmtex-protected%>, which contains
    for instance <verbatim|a>, <verbatim|it> and <verbatim|em>) get the
    prefix <verbatim|tm>.
  </enumerate>

  Many handlers are tiny. For instance the logical markup <markup|strong>,
  <markup|em>, <markup|name>, <abbr|etc.> is handled by

  <\scm-code>
    (define (tmtex-modifier s l)

    \ \ (tex-apply (string-\<gtr\>symbol (string-append "tm" s)) (tmtex (car l))))
  </scm-code>

  which produces <verbatim|\\tmstrong{...}>. The function <scm|tex-apply>
  wraps the application in a group in text mode, and <scm|tex-math-apply>
  wraps it in <verbatim|\\ensuremath> outside math mode.

  <subsection|The conversion environment>

  The converter keeps a small environment <scm|tmtex-env> (a hash table of
  stacks) with the functions <scm|tmtex-env-set>, <scm|tmtex-env-reset> and
  <scm|tmtex-env-assign>. The most important variable is
  <verbatim|"mode">, which decides whether strings are converted as text or
  as math (<scm|tmtex-math-mode?>). <markup|with> is handled by
  <scm|tmtex-with>, which pushes the variable and looks up a <LaTeX>
  command in the tables <scm|tex-with-cmd%> (text, <abbr|e.g.>
  <verbatim|font-series bold> gives <verbatim|\\tmtextbf>),
  <scm|tex-with-cmd-math%> (math fonts, <abbr|e.g.> <verbatim|math-font
  cal> gives <verbatim|\\mathcal>) and <scm|tex-assign-cmd%>
  (declarations such as <verbatim|\\bfseries>); paragraph parameters,
  languages and colors have dedicated handlers
  (<scm|tmtex-make-parmod>, <scm|tmtex-make-lang>,
  <scm|tmtex-make-color>).

  <subsection|Style-specific conversions>

  The behavior of the converter depends on the <TeXmacs> style of the
  document. The modes declared by <scm|texmacs-modes> at the top of
  <verbatim|tmtex.scm> (<scm|elsevier-style%>, <scm|acm-style%>,
  <scm|ams-style%>, <scm|revtex-style%>, <scm|springer-style%>,
  <scm|ieee-style%>, <scm|beamer-style%>, <scm|natbib-package%>, ...)
  test <scm|tmtex-style> and <scm|tmtex-packages>. The modules
  <verbatim|tmtex-acm.scm>, <verbatim|tmtex-ams.scm>, <abbr|etc.> are
  imported by <scm|import-tmtex-styles> and redefine some functions with
  <scm|tm-define> and the <scm|:mode> option (see <hlink|function
  definition and contextual overloading|../scheme/utils/utils-overload.en.tm>).
  For instance <verbatim|tmtex-ams.scm> contains

  <\scm-code>
    (tm-define (tmtex-transform-style x)

    \ \ (:mode ams-style?) x)

    \;

    (tm-define (tmtex-provided-packages)

    \ \ (:mode ams-style?)

    \ \ '("amsmath"))
  </scm-code>

  Tags whose conversion depends on the style (title and abstract metadata,
  <markup|equation>, <markup|bibliography>, ...) are declared with the macro
  <scm|tmtex-style-dependent>, which defines a default implementation and
  adds the corresponding entry to <scm|tmtex-tmstyle%>; a style module only
  needs to overload the function, <abbr|e.g.> <scm|tmtex-doc-title> or
  <scm|tmtex-make-doc-data>. Other hooks are <scm|tmtex-style-init>,
  <scm|tmtex-style-preprocess>, <scm|tmtex-postprocess>,
  <scm|tmtex-postprocess-body>, <scm|tmtex-provided-packages> (packages
  already loaded by the document class) and <scm|latex-extra-preamble>.

  <section|Macros and the preamble>

  <subsection|<LaTeX> macros introduced by <TeXmacs>>

  Many <TeXmacs> tags are exported to <LaTeX> commands which do not exist in
  standard <LaTeX>, such as <verbatim|\\tmstrong>, <verbatim|\\tmop>,
  <verbatim|\\tmtextbf>, or environments such as <verbatim|tmindent>. Their
  definitions are kept in <em|smart tables> (<verbatim|utils/library/smart-table.scm>)
  in <verbatim|convert/latex/latex-define.scm>:

  <\description-paragraphs>
    <item*|<scm|latex-texmacs-macro>>Macro bodies, written as <LaTeX>
    strees in which the integers <verbatim|1>, <verbatim|2>, ... denote the
    arguments, <abbr|e.g.> <scm|(tmstrong (textbf 1))>. The special forms
    <scm|(!recurse <scm-arg|x>)> and <scm|(!translate <scm-arg|s>)> expand a
    nested macro and translate a string into the document language.

    <item*|<scm|latex-texmacs-environment>>Environment bodies, where
    <scm|---> stands for the body of the environment.

    <item*|<scm|latex-texmacs-preamble>, <scm|latex-texmacs-env-preamble>>Arbitrary
    preamble material needed for a command <abbr|resp.> environment.
  </description-paragraphs>

  The arity of these macros is declared in
  <verbatim|latex-texmacs-drd.scm> (groups <scm|latex-texmacs-0%>,
  <scm|latex-texmacs-1%>, ..., <scm|latex-texmacs-environment-0%>,
  <abbr|etc.>, which feed <scm|latex-texmacs-arity%>). Entries of a smart
  table may be conditional; <verbatim|latex-overload.scm> uses this to
  change definitions according to the <LaTeX> document class or packages,
  for instance

  <\scm-code>
    (smart-table latex-texmacs-macro

    \ \ (:require (latex-depends? "amsthm"))

    \ \ (qed #f))
  </scm-code>

  The preamble is computed by <scm|latex-preamble> in
  <verbatim|latex-tools.scm>, called from <scm|texout-file>. It returns the
  document class options, the <verbatim|\\usepackage> lines, the page size
  settings and the macro definitions:

  <\itemize>
    <item><scm|latex-macro-defs> traverses the converted document and
    collects the definitions of all <TeXmacs> macros and environments which
    occur in it (recursively, since definitions may use other macros), and
    <scm|latex-serialize-preamble> turns them into
    <verbatim|\\newcommand> and <verbatim|\\newenvironment> lines. They are
    written between the comments <verbatim|%%%%%%%%%% Start TeXmacs macros>
    and <verbatim|%%%%%%%%%% End TeXmacs macros>; the <LaTeX> importer
    recognizes and skips this block.

    <item><scm|latex-use-package-command> collects the packages needed by
    the commands of the document from the table <scm|latex-needs%> in
    <verbatim|latex-drd.scm> (<abbr|e.g.> <scm|(includegraphics "graphicx")>),
    removes the ones implied by others (<scm|latex-depends%>) or provided by
    the class, and sorts them with <scm|latex-package-priority%>.

    <item><scm|latex-preamble-page-type> translates the page size of the
    initial environment using <scm|latex-paper-type%> and
    <scm|latex-paper-opts%>; <scm|latex-colors-defs> defines the colors which
    were used; <scm|latex-catcode-defs> defines active characters for
    non-ASCII characters when needed.
  </itemize>

  If the option <verbatim|"texmacs-\<gtr\>latex:use-macros"> is off, no
  definitions are written: <scm|latex-expand-macros> substitutes the macro
  bodies directly into the document, which yields a self-contained but less
  readable <LaTeX> file.

  <subsection|User macros>

  Macro definitions in the document (<scm|(assign "name" (macro ...))> in
  the body or in a <markup|hide-preamble>) are converted by
  <scm|tmtex-assign> into <verbatim|\\newcommand> (or
  <verbatim|\\providecommand> if the name is a known <LaTeX> command), with
  <markup|arg> replaced by <verbatim|#1>, <verbatim|#2>, ... (<scm|tmtex-args>).
  Since <LaTeX> commands are either text or math commands, <scm|tmtex-pre>
  uses the statistics of <scm|init-mode-stats> to convert the body in the
  mode in which the macro is mostly used, and protects it with
  <verbatim|\\text> or <verbatim|\\ensuremath> if it is used in both modes
  (<scm|mode-protect>). If <verbatim|"texmacs-\<gtr\>latex:expand-user-macros">
  is on, the definitions are not exported and the macros are expanded by
  <cpp|exec_latex> instead.

  <section|Text, symbols and encodings>

  Strings are converted by <scm|tmtex-string>, which splits them into
  characters and <TeXmacs> symbols <verbatim|\<less\>...\<gtr\>> and
  dispatches to <scm|tmtex-text-list> or <scm|tmtex-math-list> according
  to the mode. Special characters (<verbatim|#$%&_{}>) are escaped, the Cork
  ligature characters (quotes, dashes) are written as <TeX> ligatures, and
  in math mode sequences of letters which form a known operator
  (<scm|latex-operator%>) become <verbatim|\\sin>, <verbatim|\\log>,
  ..., while other multi-letter sequences are wrapped in
  <verbatim|\\tmop>.

  Symbols are handled by <scm|tmtex-token-sub>: a few special cases are
  listed in <scm|latex-special-symbols%> and <scm|latex-text-symbols%>, the
  prefixes <verbatim|up->, <verbatim|bbb->, <verbatim|cal->,
  <verbatim|frak->, <verbatim|b-> are mapped to <verbatim|\\mathrm>,
  <verbatim|\\mathbb>, <verbatim|\\mathcal>, <verbatim|\\mathfrak>,
  <verbatim|\\tmmathbf>, Unicode characters <verbatim|\<less\>#XXXX\<gtr\>>
  become <scm|!widechar>, and other symbols become the <LaTeX> command with
  the same name (hyphens removed) provided that it occurs in the symbol table
  <scm|latex-symbol%> (<verbatim|latex-symbol-drd.scm> and
  <verbatim|latex-texmacs-drd.scm>). Unknown symbols are reported on the
  console (<verbatim|non converted symbol>) and exported as
  <verbatim|\\nonconverted{...}>.

  The option <verbatim|"texmacs-\<gtr\>latex:encoding"> selects how
  non-ASCII characters are written:

  <\description>
    <item*|<verbatim|"ascii"> (default)>Characters are converted to
    <TeX> commands (<scm|string-convert> from <verbatim|"UTF-8"> to
    <verbatim|"LaTeX"> in <scm|output-tex>); <verbatim|fontenc> is loaded
    when needed for Cyrillic or extended Latin characters.

    <item*|<verbatim|"cork">>Characters are output in the Cork encoding and
    made active with <verbatim|\\catcode> definitions in the preamble.

    <item*|<verbatim|"utf-8">>Characters are output in UTF-8 and
    <verbatim|\\usepackage[utf8]{inputenc}> is added. This encoding is
    forced for Chinese, Japanese and Korean documents
    (<scm|tmtex-cjk-document?>), which also use the <verbatim|CJK>
    package.
  </description>

  The language of the document gives rise to a <verbatim|babel> package
  option; language changes inside the document are handled by
  <scm|tmtex-make-lang>.

  <section|Mathematics>

  Formulas are converted in the mode <verbatim|"math">: <markup|math> and
  <scm|(with "mode" "math" ...)> produce <scm|!math>, <markup|equation*>
  produces <scm|!eqn>, <markup|equation> the <verbatim|equation>
  environment, and the equation arrays (<markup|eqnarray*>,
  <markup|align>, ...) <verbatim|array> or <verbatim|tabular> constructions
  through <scm|tmtex-eqnarray> and <scm|tmtex-table-apply>. Before the
  conversion, <scm|tmtm-match-brackets> matches large brackets; the
  handlers <scm|tmtex-left>, <scm|tmtex-mid>, <scm|tmtex-right> and
  <scm|tmtex-big> produce <verbatim|\\left>, <verbatim|\\middle>,
  <verbatim|\\right> and big operators, and <scm|pre-brackets> turns
  large brackets back into ordinary ones where this is possible. Scripts are
  converted to <scm|!sub> and <scm|!sup>, <markup|frac> to
  <verbatim|\\frac>, <markup|sqrt> to <verbatim|\\sqrt>, wide accents by
  <scm|tmtex-wide> and <scm|tmtex-wide-star>. The semantic math markup
  (<markup|math-ordinary>, <markup|math-relation>, ...) is mapped to
  <verbatim|\\mathord>, <verbatim|\\mathrel>, <abbr|etc.>

  Tables are converted by <scm|tmtex-table-apply> with the help of the table
  parser of <verbatim|convert/tools/old-tmtable.scm>. The table
  <scm|tmtex-table-props%> gives, for each tabular macro
  (<markup|tabular>, <markup|matrix>, <markup|choice>, ...), the material
  to put before and after, the default alignment and whether borders are
  used.

  <section|Images and graphics>

  <markup|image> is handled by <scm|tmtex-image>. A linked image in a
  format which <LaTeX> can include (<verbatim|eps>, <verbatim|pdf>,
  <verbatim|png>, <verbatim|jpg>) is included with
  <verbatim|\\includegraphics>; other images are converted into a
  <verbatim|.pdf> or <verbatim|.eps> file (according to the preference
  <verbatim|"native pdf">) with <scm|convert-to-file>. Sizes are translated
  into <verbatim|\\resizebox> or <verbatim|\\scalebox>.

  <TeXmacs> graphics (<markup|graphics>), <markup|draw-over>,
  <markup|draw-under>, trees and the content of <scm|(specific "image"
  ...)> have no <LaTeX> counterpart; they are typeset by <TeXmacs> itself
  into a picture file by <scm|tmtex-eps>, which calls <scm|print-snippet>
  and computes the bounding box so that the picture is aligned on the
  baseline (<verbatim|\\raisebox> around <verbatim|\\includegraphics>).
  The picture files are named <verbatim|<em|base>-<em|n>.pdf> (or
  <verbatim|.eps>) next to the target file, where <em|base> is the name of
  <scm|current-save-target> without the suffix (<scm|tmtex-eps-names>);
  when no target is known (<abbr|e.g.> for snippets) the name
  <verbatim|image-<em|n>> is used.

  <section|Bibliographies, citations and indexes>

  <markup|cite> and its variants are exported as <verbatim|\\cite>, or as
  the <verbatim|natbib> commands when the package
  <verbatim|cite-author-year> is used (mode <scm|natbib-package%>).
  <markup|bibliography> is handled by <scm|tmtex-bib> and
  <scm|tmtex-biblio>: by default the bibliography which was generated in
  <TeXmacs> is exported as a <verbatim|thebibliography> environment with
  <verbatim|\\bibitem> entries; if the option
  <verbatim|"texmacs-\<gtr\>latex:indirect-bib"> is on, the commands
  <verbatim|\\bibliographystyle> and <verbatim|\\bibliography> are written
  instead, so that <name|BibTeX> has to be run on the <LaTeX> side. Labels
  and references become <verbatim|\\label>, <verbatim|\\ref> and
  <verbatim|\\pageref>; <markup|the-index> becomes
  <verbatim|\\printindex> (and <verbatim|\\makeindex> is added to the
  preamble), the table of contents <verbatim|\\tableofcontents>; glossaries
  have dedicated handlers.

  <section|Options>

  The options of the converters from <verbatim|texmacs-stree> to
  <verbatim|latex-stree> and to <verbatim|latex-document> are declared in
  <verbatim|init-latex.scm> and appear in the menu
  tab <menu|Convert> (sub-tab <menu|LaTeX>) of the preferences dialog. Some are passed in the option list
  of the converter, some are read directly as preferences.

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|font-series|bold>|<table|<row|<cell|Option>|<cell|Default>|<cell|Effect>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:replace-style>>|<cell|<verbatim|on>>|<cell|Map
  unknown styles to <verbatim|article>.>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:expand-macros>>|<cell|<verbatim|on>>|<cell|Expand
  macros with <cpp|exec_latex>.>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:expand-user-macros>>|<cell|<verbatim|off>>|<cell|Expand
  user macros instead of exporting them.>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:indirect-bib>>|<cell|<verbatim|off>>|<cell|Use
  <verbatim|\\bibliography>.>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:use-macros>>|<cell|<verbatim|on>>|<cell|Define
  <TeXmacs> macros in the preamble.>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:encoding>>|<cell|<verbatim|ascii>>|<cell|<verbatim|ascii>,
  <verbatim|cork> or <verbatim|utf-8>.>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:source-tracking>>|<cell|<verbatim|off>>|<cell|Add
  source tracking information.>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:conservative>>|<cell|<verbatim|on>>|<cell|Reuse
  the original <LaTeX> source.>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:transparent-source-tracking>>|<cell|<verbatim|on>>|<cell|Check
  that markers do not change the output.>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:attach-tracking-info>>|<cell|<verbatim|on>>|<cell|See
  below.>>>>>>
    Options of the <LaTeX> export.
  </big-table>

  <verbatim|"texmacs-\<gtr\>latex:expand-macros">,
  <verbatim|"texmacs-\<gtr\>latex:source-tracking">,
  <verbatim|"texmacs-\<gtr\>latex:conservative"> and
  <verbatim|"texmacs-\<gtr\>latex:transparent-source-tracking"> are read
  with <cpp|get_preference> in <c++>, and
  <verbatim|"texmacs-\<gtr\>latex:expand-user-macros"> is read with
  <scm|get-preference> in <scm|tmtex-file> and <scm|tmtex-env-patch>; for
  these options, a value passed explicitly to <scm|convert> has no effect.
  The other options are read from the option list by
  <scm|tmtex-initialize>. The preference
  <verbatim|"texmacs-\<gtr\>latex:attach-tracking-info"> appears in the
  preferences dialog, but is not read anywhere in the conversion code of
  this version; likewise <verbatim|init-latex.scm> declares a preference
  <verbatim|"texmacs-\<gtr\>latex:transparent-tracking"> which is not used.

  <section|How to add support for a new tag>

  Suppose that a style file defines a macro <markup|my-note> with one
  argument, which should become <verbatim|\\mynote{...}> in <LaTeX>, with a
  definition in the preamble.

  <\enumerate>
    <item>Without any work, the default rule already produces
    <verbatim|\\mynote{...}>, provided that the tag occurs literally in the
    document (so that <scm|tmtex-env-patch> protects it from expansion), but
    the generated file does not define <verbatim|\\mynote>.

    <item>To provide a definition, add an entry to the smart table
    <scm|latex-texmacs-macro> in <verbatim|latex-define.scm>, <abbr|e.g.>
    <scm|(mynote (footnote 1))>, and declare its arity by adding
    <scm|mynote> to <scm|latex-texmacs-1%> in <verbatim|latex-texmacs-drd.scm>.
    If the definition requires a package, add an entry to
    <scm|latex-needs%> in <verbatim|latex-drd.scm>.

    <item>If the conversion is not a plain renaming, add an entry to
    <scm|tmtex-tmstyle%> in <verbatim|tmtex.scm>, for instance
    <scm|(my-note (,tmtex-my-note 1))>, with a handler <scm|(define
    (tmtex-my-note s l) ...)> which receives the tag name and the list of
    unconverted arguments and returns a <LaTeX> stree; use <scm|tmtex> to
    convert the arguments. For a new <em|primitive>, add an entry to
    <scm|tmtex-primitives%> instead (the handler then only receives the
    arguments).

    <item>If the tag must be expanded by <TeXmacs> before the export
    (because its <LaTeX> rendering is too hard), add its name to
    <scm|tmtex-always-expand>, or give it a
    <verbatim|tmlatex-my-note> variant in the style file.

    <item>For the import in the other direction, see <hlink|importing
    <LaTeX>|convert-latex-import.en.tm>: the command <verbatim|\\mynote> is
    recognized as a <TeXmacs> extension because it belongs to
    <scm|latex-texmacs%>, and a rule converting it back has to be added to
    <cpp|latex_command_to_tree>.
  </enumerate>

  <section|Testing and debugging>

  <\itemize>
    <item>The menu <menu|Tools|LaTeX> (shown with detailed menus when <verbatim|pdflatex> is in
    the path, see <verbatim|convert/latex/tmtex-widgets.scm>) exports the
    current buffer, runs <verbatim|pdflatex> through <scm|try-latex-export>
    (<cpp|try_latex_export> in <verbatim|Data/Convert/Tex/latex_recover.cpp>)
    and displays the <LaTeX> errors together with the corresponding
    locations in the <TeXmacs> document (which are found with the source
    tracking markers).

    <item><scm|(check-latex-export <scm-arg|dir>)> and <scm|(run-checks)>
    in <verbatim|check/check-master.scm> export all <verbatim|.tm> files of
    a directory, run <verbatim|pdflatex> on them and report errors.

    <item><scm|(test-tmtex)> in <verbatim|convert/latex/test-tmtex.scm>
    returns a document testing the idempotence of the round trips
    <TeXmacs> <math|\<rightarrow\>> <LaTeX> <math|\<rightarrow\>>
    <TeXmacs> and <LaTeX> <math|\<rightarrow\>> <TeXmacs>
    <math|\<rightarrow\>> <LaTeX> on a list of markup.

    <item>In a <scheme> session, <scm|(serialize-latex (texmacs-\<gtr\>latex
    (tree-\<gtr\>stree <scm-arg|t>) '()))> converts a tree
    <scm-arg|t>; setting <scm|tmtex-debug-mode?> or inspecting the
    intermediate stree returned by <scm|texmacs-\<gtr\>latex> is usually
    the fastest way to locate a problem. Messages such as <verbatim|non
    converted symbol> are printed on the console.
  </itemize>

  <section|Known limitations>

  <\itemize>
    <item>The export is not a faithful translation of the typeset result:
    style macros without handler are either expanded (and lose their
    structure) or exported under their own name without definition.

    <item>Graphics and other constructs without <LaTeX> counterpart are
    exported as pictures, which only works for a buffer exported to a file.

    <item>The quality of the export for journal styles depends on the
    corresponding <verbatim|tmtex-*.scm> module.

    <item>When the export is requested on a document without the
    <verbatim|view> attribute (that is, not through <cpp|buffer_export>),
    <cpp|latex_expand> in <verbatim|Texmacs/Data/new_buffer.cpp> uses the
    view returned by <cpp|concrete_view> without checking it; scripts which
    want to export trees should use the converters to
    <verbatim|latex-stree> or <verbatim|latex-snippet>.
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
