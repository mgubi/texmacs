<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Importing <LaTeX>>

  <section|Overview>

  The import of <LaTeX> is almost entirely written in <c++>, in the
  directory <verbatim|Data/Convert/Tex>. The <scheme> side only declares the
  converters (<verbatim|convert/latex/init-latex.scm>) and provides the
  tables which describe the <LaTeX> commands
  (<verbatim|convert/latex/latex-command-drd.scm>,
  <verbatim|latex-symbol-drd.scm>, <verbatim|latex-texmacs-drd.scm>,
  <verbatim|latex-drd.scm>). A complete <LaTeX> document is imported by

  <\scm-code>
    (tm-define (latex-document-\<gtr\>texmacs x . opts)

    \ \ (if (list-1? opts) (set! opts (car opts)))

    \ \ (with as-pic (== (get-preference "latex-\<gtr\>texmacs:fallback-on-pictures") "on")

    \ \ \ \ (conservative-latex-\<gtr\>texmacs x as-pic)))
  </scm-code>

  The <c++> function <cpp|conservative_latex_to_texmacs> and the function
  <cpp|tracked_latex_to_texmacs> which it calls implement the source
  tracking and conservative import described in <hlink|source tracking and
  conservative conversion|convert-latex-tracking.en.tm>. When these
  features are disabled (the default), they reduce to

  <\explain>
    <cpp|tree latex_document_to_tree (string s, bool as_pic)><explain-synopsis|import
    a complete <LaTeX> document>
  <|explain>
    Defined in <verbatim|fromtex_post.cpp>. Opens a new layer of the
    command tables (see below), parses the document with
    <cpp|parse_latex_document (s, true, as_pic)>, renders the parts which
    must be imported as pictures (<cpp|latex_fallback_on_pictures>, only if
    <cpp|as_pic>), converts the parsed <LaTeX> with <cpp|latex_to_tree> and
    closes the layer.
  </explain>

  Snippets (pasting <LaTeX>) are imported by <scm|parse-latex> followed by
  <scm|latex-\<gtr\>texmacs>, that is <cpp|parse_latex> and
  <cpp|latex_to_tree>. The whole import thus consists of two phases: the
  <em|parser>, which turns the string into parsed <LaTeX>, and the
  <em|converter>, which turns parsed <LaTeX> into a <TeXmacs> tree. Both
  phases need to know the type and the arity of the <LaTeX> commands.

  <section|Knowledge about <LaTeX> commands>

  The type and arity of a command are obtained with

  <\cpp-code>
    string latex_type \ (string cmd);

    int \ \ \ latex_arity (string cmd);
  </cpp-code>

  declared in <verbatim|Tex/convert_tex.hpp> and implemented in
  <verbatim|inittex.cpp>. The argument is the command with its backslash,
  and environments are represented as <verbatim|\\begin-<em|name>> and
  <verbatim|\\end-<em|name>>. These functions first look into the
  hash maps <cpp|command_type> and <cpp|command_arity> (and
  <cpp|command_def> for the definitions), which contain the commands
  defined by the document being parsed, and otherwise call the <scheme>
  functions <scm|latex-type> and <scm|latex-arity> of
  <verbatim|latex-drd.scm> (the results are cached by <cpp|hashfunc>
  objects). The three maps are <cpp|rel_hashmap> objects: they are
  <cpp|extend>ed at the beginning of a parse and <cpp|merge>d or
  <cpp|shorten>ed at the end, so that definitions do not leak from one
  import to the next.

  <scm|latex-type> returns one of the following strings, according to the
  logical group in which the command occurs:

  <\description-paragraphs>
    <item*|<verbatim|"command">>Ordinary commands
    (<scm|latex-command%>, with arities from <scm|latex-command-0%>,
    <scm|latex-command-1%>, ...; the groups <scm|latex-command-1*%>
    <abbr|etc.> contain commands with an optional argument).

    <item*|<verbatim|"symbol">, <verbatim|"big-symbol">>Symbols
    (<verbatim|latex-symbol-drd.scm>, including the symbols of
    <verbatim|amssymb>, <verbatim|stmaryrd>, <verbatim|wasysym>,
    <verbatim|upgreek>, ...), converted into the <TeXmacs> symbol with the
    same name.

    <item*|<verbatim|"modifier">>Font and paragraph declarations and
    commands such as <verbatim|\\bf>, <verbatim|\\textbf>,
    <verbatim|\\centering>.

    <item*|<verbatim|"environment">, <verbatim|"math-environment">,
    <verbatim|"enunciation">, <verbatim|"list">>Environments; math
    environments switch the parser to math mode, enunciations are
    theorem-like environments.

    <item*|<verbatim|"length">, <verbatim|"counter">,
    <verbatim|"name">>Lengths, counters and names (such as
    <verbatim|\\LaTeX>).

    <item*|<verbatim|"operator">, <verbatim|"control">>Operators such as
    <verbatim|\\sin> and control commands.

    <item*|<verbatim|"ignore">>Commands which are dropped
    (<scm|latex-ignore%>: <verbatim|\\relax>, <verbatim|\\sloppy>, ...).

    <item*|<verbatim|"as-picture">>Commands and environments which are
    rendered as pictures (<scm|latex-as-pic%>: <verbatim|pspicture>,
    <verbatim|tikzpicture>, <verbatim|\\xymatrix>).

    <item*|<verbatim|"texmacs">>The extra commands which the <TeXmacs>
    exporter introduces (<scm|latex-texmacs%>, <abbr|e.g.>
    <verbatim|\\tmstrong>); this allows a round trip of exported documents.

    <item*|<verbatim|"undefined">>Unknown commands.
  </description-paragraphs>

  The parser adds its own types for commands defined in the document:
  <verbatim|"user"> (macros defined by <verbatim|\\def> or
  <verbatim|\\newcommand>), <verbatim|"enunciation"> (environments declared
  by <verbatim|\\newtheorem>), <verbatim|"length"> (declared by
  <verbatim|\\newlength>), and internal types such as
  <verbatim|"side-effect!">, <verbatim|"begin-end!">,
  <verbatim|"defined-env!"> and <verbatim|"replace"> for definitions which
  are substituted during the parse. A negative arity means that the first
  argument is optional.

  <section|The parser>

  <subsection|Pre-processing>

  <\explain>
    <cpp|tree parse_latex (string s, bool change, bool as_pic)><explain-synopsis|parse
    a <LaTeX> string>
  <|explain>
    Defined at the end of <verbatim|parsetex.cpp>. Converts line endings
    (<cpp|dos_to_better>), detects the language from the
    <verbatim|babel> options (<cpp|get_latex_language>) and the input
    encoding from <verbatim|inputenc> (<cpp|get_latex_encoding>,
    <cpp|latex_encoding_to_iconv>), converts the string to UTF-8 (with a
    heuristic <cpp|western_to_utf8> when no encoding is declared), converts
    <LaTeX> accent commands into characters, runs a <cpp|latex_parser> and
    converts the accented characters back to the internal Cork encoding
    (<cpp|accented_to_Cork>). If a language was found, the result is
    wrapped into <verbatim|(!language <em|tree> <em|lan>)>.
    <cpp|parse_latex_document> wraps the result into
    <verbatim|(!file <em|tree>)>.
  </explain>

  <cpp|latex_parser::parse (string s, int change)> first cuts the source at
  strategic places (before <verbatim|\\begin{document}>, sectioning
  commands, <verbatim|\\newcommand>, <verbatim|\\def> at brace depth zero),
  so that an error in one part does not spoil the rest. While cutting, it
  removes the block of macro definitions written by the <TeXmacs> exporter
  (between <verbatim|%%%%%%%%%% Start TeXmacs macros> and <verbatim|%%%%%%%%%%
  End TeXmacs macros>) and inlines the files loaded by
  <verbatim|\\input>, <verbatim|\\include> and <verbatim|\\usepackage>
  when they can be found relative to the current file (see
  <cpp|get_file_focus>), except for a list of well-known packages
  (<cpp|skip_expansion>) whose commands are known to the tables anyway.

  <subsection|Parsed <LaTeX>>

  The pieces are parsed by the recursive descent methods of
  <cpp|latex_parser>: <cpp|parse> (text up to a stop string),
  <cpp|parse_backslash>, <cpp|parse_command>, <cpp|parse_unknown>,
  <cpp|parse_symbol>, <cpp|parse_length>, <cpp|parse_verbatim> and so on.
  The result is a <c++> tree with the following conventions:

  <\itemize>
    <item>Text is a string; a sequence is a <markup|concat>.

    <item>A command application is a <markup|tuple> whose first child is
    the name of the command with its backslash, followed by the arguments:
    <verbatim|\\frac{1}{2}> becomes <verbatim|(tuple "\\frac" "1" "2")>.
    The parser reads as many arguments as given by <cpp|latex_arity>. If an
    optional argument is present, it is stored as the first argument and a
    star is appended to the name: <verbatim|\\sqrt[3]{x}> becomes
    <verbatim|(tuple "\\sqrt*" "3" "x")>.

    <item>Environments are not nested: <verbatim|\\begin{proof}> and
    <verbatim|\\end{proof}> become <verbatim|(tuple "\\begin-proof")> and
    <verbatim|(tuple "\\end-proof")> in the flow; matching is done
    afterwards.

    <item>Subscripts and superscripts become <verbatim|\\\<less\>sub\<gtr\>>
    and <verbatim|\\\<less\>sup\<gtr\>> applications, math shifts
    <verbatim|$> and <verbatim|$$> become <verbatim|\\begin-math> /
    <verbatim|\\end-math> and <verbatim|\\begin-displaymath> /
    <verbatim|\\end-displaymath> pairs.
  </itemize>

  The parser keeps the current mode in the pseudo-entry
  <verbatim|command_type ("!mode")> (<verbatim|"text"> or
  <verbatim|"math">), which matters for the arguments of commands such as
  <verbatim|\\text> (<cpp|is_text_argument>).

  <subsection|Definitions in the document>

  When the parser meets <verbatim|\\def>, <verbatim|\\newcommand>,
  <verbatim|\\renewcommand>, <verbatim|\\providecommand>,
  <verbatim|\\DeclareMathOperator> (all normalized to <verbatim|\\def>),
  <verbatim|\\newenvironment>, <verbatim|\\newtheorem>,
  <verbatim|\\newlength>, <abbr|etc.>, it records the type, arity and body
  of the new command in <cpp|command_type>, <cpp|command_arity> and
  <cpp|command_def>, so that later occurrences are parsed with the right
  number of arguments. The definitions themselves are kept in the tree and
  later become <TeXmacs> macro definitions in the preamble of the imported
  document. Only special definitions are substituted during the parse:
  shortcuts for <verbatim|\\begin>/<verbatim|\\end>, definitions with side
  effects, and (when importing as pictures) definitions whose body must be
  rendered as a picture.

  <subsection|Pictures>

  If the preference <verbatim|"latex-\<gtr\>texmacs:fallback-on-pictures">
  is on, commands and environments of type <verbatim|"as-picture"> are
  stored as <verbatim|(tuple "\\latex_preview" <em|name> <em|source>)>.
  After parsing, <cpp|latex_fallback_on_pictures> (<verbatim|fromtex.cpp>)
  merges the beginning and end of environments, calls <cpp|latex_preview>
  (<verbatim|Plugins/LaTeX_Preview/latex_preview.cpp>), which runs <LaTeX>
  on the fragments together with the preamble of the document, and replaces
  them by <markup|picture-mixed> trees containing both the picture and the
  original source.

  <section|From parsed <LaTeX> to <TeXmacs>>

  <subsection|The stages>

  The driver is <cpp|latex_to_tree (tree t)> in <verbatim|fromtex_post.cpp>.
  Abridged, it reads

  <\cpp-code>
    tree t1= kill_space_invaders (t0);

    ...

    tree t2= is_document? filter_preamble (t1): t1;

    tree t3= parsed_latex_to_tree (t2);

    tree t4= finalize_document (t3);

    tree t5= is_document? finalize_preamble (t4, style): t4;

    tree t6= handle_matches (t5);

    ...

    tree t7= upgrade_tex (t6);

    tree t8= finalize_floats (t7);

    tree t9= finalize_misc (t8);

    ...

    tree t10= finalize_textm (t9);

    tree t11= drd_correct (std_drd, t10);

    ...

    tree t13= latex_correct (t12);

    tree t14= guess_missing (t13);

    tree t15= postprocess_metadata (t14);
  </cpp-code>

  <\description>
    <item*|<cpp|kill_space_invaders>>Removes spaces and newlines which
    <TeX> would ignore (<verbatim|fromtex.cpp>).

    <item*|<cpp|filter_preamble>>Moves the preamble into a
    <markup|hide-preamble> environment, keeps the document class, and
    collects the title, authors and abstract with <cpp|collect_metadata>
    (<verbatim|metadata.cpp>), which has variants for the classes of various
    publishers (<verbatim|metadata-acm.cpp>, <verbatim|metadata-ams.cpp>,
    <verbatim|metadata-elsevier.cpp>, <verbatim|metadata-ieee.cpp>,
    <verbatim|metadata-revtex.cpp>, <verbatim|metadata-springer.cpp>).

    <item*|<cpp|parsed_latex_to_tree>>The main translation, see below.

    <item*|<cpp|finalize_document>, <cpp|finalize_layout>>Builds the
    paragraph structure (<cpp|make_paragraphs>): blank lines become
    paragraph breaks, and display environments, lists and sections are put
    in separate paragraphs. Also handles algorithms
    (<cpp|finalize_algorithms>) and matrices (<cpp|finalize_pmatrix>).

    <item*|<cpp|finalize_preamble>>Determines the style from
    <verbatim|\\documentclass>, drops <verbatim|\\usepackage>, converts
    <verbatim|\\bibliography> (loading the <verbatim|.bbl> file next to the
    source if it exists) and drops redefinitions of standard theorem
    environments.

    <item*|<cpp|handle_matches>>Matches the flat
    <markup|begin>/<markup|end> pairs into environments, closing and
    reopening environments which are improperly nested.

    <item*|<cpp|upgrade_tex>>A selection of the upgrade routines of
    <verbatim|Data/Convert/Texmacs/upgradetm.cpp> which turn the
    old-style markup (<markup|apply>, <markup|begin>, <markup|set>/<markup|reset>,
    ...) produced so far into modern <TeXmacs> markup.

    <item*|<cpp|finalize_floats>, <cpp|finalize_misc>,
    <cpp|finalize_textm>>Floats and captions, hyperlinks and various
    clean-ups (sections and labels, nested <markup|with>, vertical space).

    <item*|<cpp|drd_correct>, <cpp|latex_correct>,
    <cpp|simplify_correct>>Corrections with respect to the <abbr|DRD> and
    the conventions for math.

    <item*|<cpp|guess_missing>, <cpp|postprocess_metadata>>Adds missing
    definitions and structures the metadata (<markup|doc-data>,
    <markup|doc-author>, ...) with <verbatim|metadata_post.cpp>.
  </description>

  For a complete document, the result is assembled into a <TeXmacs> file
  whose style is the <LaTeX> class if a <TeXmacs> style with this name
  exists (and <verbatim|generic> otherwise), followed by the package
  <verbatim|std-latex> (except for a few classes) and by
  <verbatim|cite-author-year> if <verbatim|natbib> was detected. The
  language and a font for Chinese, Taiwanese and Russian are stored in the
  initial environment.

  <subsection|The translation of commands>

  <cpp|parsed_latex_to_tree> (abbreviated <cpp|l2e> in
  <verbatim|fromtex.cpp>) dispatches on the shape of the tree: strings are
  passed to <cpp|latex_symbol_to_tree>, <markup|concat> nodes to
  <cpp|latex_concat_to_tree> (which also deals with font declarations
  whose scope extends to the end of the group) and tuples to
  <cpp|latex_command_to_tree>.

  <cpp|latex_symbol_to_tree> handles commands without arguments: special
  characters, the commands of type <verbatim|"symbol"> (converted to the
  <TeXmacs> symbol <verbatim|\<less\><em|name>\<gtr\>>, with a few
  exceptions such as <verbatim|\\lnot> and the <verbatim|upgreek>
  letters), nullary modifiers (<verbatim|\\bf>, <verbatim|\\centering>,
  ...) which become <markup|set> nodes, and so on.

  <cpp|latex_command_to_tree> is a long sequence of tests of the form

  <\cpp-code>
    if (is_tuple (t, "\\\\frac", 2)) return tree (FRAC, l2e (t[1]), l2e (t[2]));

    if (is_tuple (t, "\\\\sqrt*", 2)) return tree (SQRT, l2e (t[2]), l2e (t[1]));

    ...

    // Start TeXmacs specific markup

    if (is_tuple (t, "\\\\tmstrong", 1)) return tree (APPLY, "strong", l2e (t[1]));
  </cpp-code>

  The arguments are converted recursively with <cpp|l2e>; arguments which
  must be kept as text use <cpp|parsed_text_to_tree> (<cpp|t2e>) and
  <cpp|string_arg>. Commands which are not handled explicitly fall through
  to the end of the function, where <verbatim|(tuple "\\<em|cmd>"
  <em|args>)> becomes <verbatim|(apply "<em|cmd>" <em|args>)> and
  environments become <markup|begin> nodes; <cpp|upgrade_tex> later turns
  these into ordinary <TeXmacs> tags. An unknown <LaTeX> command
  <verbatim|\\foo{x}> thus gives the tag <markup|foo> with one argument,
  which is displayed in red if no macro <markup|foo> is defined. Packages
  which are often used by journals, such as the theorem environments or
  <verbatim|algorithm2e>, have dedicated code.

  <section|<LaTeX> class and style files>

  The format <verbatim|latex-class> (suffixes <verbatim|ltx>,
  <verbatim|sty>, <verbatim|cls>) is imported by
  <cpp|latex_class_document_to_tree> (<verbatim|fromcls.cpp>). It sets the
  flag <cpp|textm_class_flag> (which makes the parser accept length
  assignments in <TeX> syntax), imports the file as a document and filters
  the result with <cpp|latex_class_filter> into a <TeXmacs> style file.

  <section|Options>

  <\description-paragraphs>
    <item*|<verbatim|"latex-\<gtr\>texmacs:fallback-on-pictures">
    (default <verbatim|"on">)>Import the constructs of type
    <verbatim|"as-picture"> as pictures (requires a working <LaTeX>
    installation).

    <item*|<verbatim|"latex-\<gtr\>texmacs:source-tracking">,
    <verbatim|"latex-\<gtr\>texmacs:conservative">,
    <verbatim|"latex-\<gtr\>texmacs:transparent-source-tracking">
    (default <verbatim|"off">)>See <hlink|source tracking and conservative
    conversion|convert-latex-tracking.en.tm>.
  </description-paragraphs>

  All these options are read with <scm|get-preference> or
  <cpp|get_preference>; the option list passed to the converter is
  ignored by <scm|latex-document-\<gtr\>texmacs>.

  <section|How to add support for a new command>

  <\enumerate>
    <item>Declare the command in the tables, so that the parser reads the
    right number of arguments: add it to the appropriate group of
    <verbatim|latex-command-drd.scm> (<abbr|e.g.> <scm|latex-command-2%>
    for a command with two arguments, <scm|latex-command-1*%> for one
    argument plus an optional one, <scm|latex-environment-0%> for an
    environment), or to <verbatim|latex-symbol-drd.scm> for a symbol. If
    the corresponding <TeXmacs> symbol has the same name, nothing else is
    needed for symbols.

    <item>Add a rule to <cpp|latex_command_to_tree> in
    <verbatim|fromtex.cpp> which builds the <TeXmacs> tree, <abbr|e.g.>

    <\cpp-code>
      if (is_tuple (t, "\\\\mycmd", 2))

      \ \ return compound ("my-tag", l2e (t[1]), l2e (t[2]));
    </cpp-code>

    Without such a rule, the command is imported as a tag with the same
    name (without backslash), which may be sufficient if a macro with this
    name is defined in a style package.

    <item>If the command introduces a block structure (an environment
    which should be a separate paragraph), check the lists of block
    environments in <verbatim|fromtex.cpp> and <verbatim|fromtex_post.cpp>
    (<cpp|is_block_environnement>, <cpp|finalize_layout>).

    <item>For commands which the <TeXmacs> exporter itself produces, add
    them to <verbatim|latex-texmacs-drd.scm> and convert them back in the
    section <verbatim|Start TeXmacs specific markup> of
    <cpp|latex_command_to_tree>.
  </enumerate>

  <section|Testing>

  <\itemize>
    <item>Open a <verbatim|.tex> file with <menu|File|Import|LaTeX> or paste
    a <LaTeX> snippet (<menu|Edit|Paste from|LaTeX>).

    <item>In a <scheme> session, <scm|(latex-\<gtr\>texmacs (parse-latex
    "$\\\\frac{1}{2}$"))> shows the result for a snippet;
    <scm|(parse-latex <scm-arg|s>)> alone shows the parsed <LaTeX>, which
    is usually the first thing to check when an import goes wrong.

    <item>The commented <cpp|cout> statements in <cpp|latex_to_tree> print
    the tree after each stage.

    <item>The idempotence tests of <verbatim|convert/latex/test-tmtex.scm>
    (<scm|(test-tmtex)>) also exercise the import.
  </itemize>

  <section|Known limitations>

  <\itemize>
    <item><LaTeX> is a programming language; the importer only understands
    the commands in its tables, user definitions of a reasonable form and
    some packages. Low-level <TeX> programming (<verbatim|\\catcode>,
    conditionals, <verbatim|\\expandafter>, ...) is not interpreted.

    <item>Included files are only expanded when they are found relative to
    the imported file.

    <item>Errors in the source are not reported; the parser tries to
    recover heuristically, which can produce unexpected results.

    <item>Layout-oriented constructs (explicit spacing, boxes, tabular
    tricks) are often imported in a simplified form; see also the user
    documentation <hlink|limitations of the current <LaTeX>
    converter|../../main/convert/latex/man-problems.en.tm>.
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
