<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Parsers and the language definition interface>

  <section|The parser classes>

  The directory <verbatim|Data/Parser/> contains a handful of small lexical
  recognizers, written in 2020 for the generic highlighter. They all derive
  from <cpp|parser_rep> (<verbatim|Data/Parser/parser.hpp>):

  <\cpp-code>
    class parser_rep {

    public:

    \ \ bool parse (string s, int& pos) {

    \ \ \ \ if (!can_parse (s, pos)) return false;

    \ \ \ \ int opos= pos;

    \ \ \ \ do_parse (s, pos);

    \ \ \ \ if (pos \<gtr\> opos) return true;

    \ \ \ \ else { ... return false; }

    \ \ }

    protected:

    \ \ virtual string get_parser_name () { return ""; }

    \ \ virtual void do_parse (string s, int& pos) { ... }

    \ \ virtual bool can_parse (string s, int pos) { return pos \<less\> N(s); }

    \ \ virtual string to_string () { ... }

    };
  </cpp-code>

  The non-virtual <cpp|parse> is the only entry point used by the languages:
  it first tests with <cpp|can_parse>, then lets <cpp|do_parse> advance the
  position, and succeeds only if the position actually moved. When debugging
  is enabled for parsers (<scm|(debug-set "parser" #t)>, which sets
  <cpp|DEBUG_FLAG_PARSER>; see <cpp|DEBUG_PARSER> in
  <verbatim|Kernel/Abstractions/basic.hpp>), a parser which accepted in
  <cpp|can_parse> but did not advance is reported on the
  <cpp|debug_packrat> stream. Most subclasses redeclare <cpp|can_parse> and
  <cpp|get_parser_name> as public; the names returned by
  <cpp|get_parser_name> (<verbatim|"string_parser">,
  <verbatim|"number_parser">, ...) are used by <cpp|prog_language_rep> to
  decide on the color of the token. Parsers are plain value objects; they
  are not reference counted.

  <\description>
    <item*|<cpp|blanks_parser_rep>>(<verbatim|blanks_parser.hpp>) Consumes
    exactly one space or tab.

    <item*|<cpp|identifier_parser_rep>>(<verbatim|identifier_parser.cpp>) An
    identifier starts with a letter or one of the <em|start characters>
    (default <verbatim|_>) and continues with letters, digits and the
    <em|extra characters> (default <verbatim|_>). Configured with
    <cpp|set_start_chars> and <cpp|set_extra_chars>; this is done by the
    <name|Mathemagix> and <name|R> highlighters, but there is no
    <scheme> interface for it.

    <item*|<cpp|keyword_parser_rep>>(<verbatim|keyword_parser.cpp>) Holds a
    map from keywords to <em|groups> (<cpp|put (keyword, group)>,
    <cpp|get (keyword)>). <cpp|can_parse> reads the maximal run of ASCII
    letters at the position with <cpp|read_word>
    (<verbatim|Data/String/analyze.cpp>) and succeeds if this word is a key
    of the map. The method <cpp|use_keywords_of_lang> is a leftover which
    is not called anywhere.

    <item*|<cpp|operator_parser_rep>>(<verbatim|operator_parser.cpp>) Holds
    a map from operator strings to groups. <cpp|can_parse> iterates over all
    operators and succeeds if one of them occurs at the position;
    <cpp|do_parse> then extends the match to the longest operator which
    starts with the one found, so that <verbatim|\<less\>\<less\>=> wins over
    <verbatim|\<less\>\<less\>> and <verbatim|\<less\>>.

    <item*|<cpp|number_parser_rep>>(<verbatim|number_parser.cpp>) Recognizes
    decimal numbers (digits and dots, possibly starting with a dot followed by
    a digit) and, depending on its <em|boolean features>, the prefixes
    <verbatim|0b>, <verbatim|0o>, <verbatim|0x> (<verbatim|"prefix_0b">,
    <verbatim|"prefix_0o">, <verbatim|"prefix_0x">) and an exponent
    <verbatim|e> or <verbatim|E> with optional minus sign
    (<verbatim|"sci_notation">). With <verbatim|"no_suffix_with_box">, a
    number with a binary, octal or hexadecimal prefix ends after its digits.
    A one-character digit <em|separator> such as <verbatim|_> may be
    allowed (<cpp|support_separator>). Finally an embedded keyword parser,
    returned by <cpp|get_suffix_parser>, recognizes suffixes like
    <verbatim|L> or <verbatim|j>.

    <item*|<cpp|escaped_char_parser_rep>>(<verbatim|escaped_char_parser.cpp>)
    Recognizes an escape character (default <verbatim|\\>) followed by one of
    a configurable set of characters or strings (<cpp|set_sequences>; strings
    of length one go to the character set, longer ones to the string list)
    and, depending on boolean features, <verbatim|\\x> plus two hexadecimal
    digits (<verbatim|"hex_with_8_bits">), <verbatim|\\u> plus four
    (<verbatim|"hex_with_16_bits">), <verbatim|\\U> plus eight
    (<verbatim|"hex_with_32_bits">), and up to three octal digits
    (<verbatim|"octal_upto_3_digits">).

    <item*|<cpp|string_parser_rep>>(<verbatim|string_parser.cpp>) Holds a
    map from opening to closing delimiters (<cpp|set_pairs>) and optionally
    an escaped character parser (<cpp|set_escaped_char_parser>). It is the
    only <em|stateful> parser: if a line ends before the closing delimiter,
    or if an escape sequence is met, <cpp|unfinished ()> remains true and
    the next call continues inside the string. When it meets an escape
    sequence, it stops just before it and sets <cpp|escaped ()>, so that
    the language can color the escape sequence separately by calling
    <cpp|parse_escaped>. (With <cpp|skip_escaped (true)>, escape sequences
    are swallowed instead; no language uses this.)

    <item*|<cpp|inline_comment_parser_rep>>(<verbatim|inline_comment_parser.cpp>)
    Succeeds if one of the configured comment starts (<cpp|set_starts>)
    occurs at the position, and then consumes the rest of the string.

    <item*|<cpp|preprocessor_parser_rep>>(<verbatim|preprocessor_parser.cpp>)
    Succeeds on a start character (default <verbatim|#>) which is the first
    non-blank character of the line, followed by one of the configured
    directives (<cpp|set_directives>) and then a space or the end of the
    line. Languages without directives never have preprocessor lines.
  </description>

  <section|The class <cpp|abstract_language_rep>>

  <verbatim|System/Language/impl_language.hpp> declares the common base of
  all parser based highlighters:

  <\cpp-code>
    struct abstract_language_rep: language_rep {

    \ \ hashmap\<less\>string,string\<gtr\> colored;

    \ \ string current_parser;

    \ \ blanks_parser_rep blanks_parser;

    \ \ inline_comment_parser_rep inline_comment_parser;

    \ \ number_parser_rep number_parser;

    \ \ escaped_char_parser_rep escaped_char_parser;

    \ \ keyword_parser_rep keyword_parser;

    \ \ operator_parser_rep operator_parser;

    \ \ identifier_parser_rep identifier_parser;

    \ \ string_parser_rep string_parser;

    \ \ preprocessor_parser_rep preprocessor_parser;

    \ \ ...

    };
  </cpp-code>

  The older hand-written highlighters (<cpp|cpp_language_rep>,
  <cpp|mathemagix_language_rep>, <cpp|r_language_rep>,
  <cpp|scilab_language_rep>, <cpp|fortran_language_rep>) also derive from
  it, use some of the parsers in their <cpp|advance> methods, and use the
  table <cpp|colored> and the helpers <cpp|parse_identifier>,
  <cpp|parse_keyword>, <cpp|parse_type> and <cpp|parse_constant> in their
  <cpp|get_color> methods. The class <cpp|prog_language_rep> uses only the
  parsers and <cpp|current_parser>.

  <section|The generic highlighter <cpp|prog_language_rep>>

  <subsection|Construction>

  The constructor (<verbatim|System/Language/prog_language.cpp>) loads the
  <scheme> module of the language and then queries six <em|features>:

  <\cpp-code>
    prog_language_rep::prog_language_rep (string name):

    \ \ abstract_language_rep (name)

    {

    \ \ string use_modules= "(use-modules (" * name * "-lang))";

    \ \ eval (use_modules);

    \ \ tree keyword_config= get_parser_config (name, "keyword");

    \ \ customize_keyword (keyword_parser, keyword_config);

    \ \ tree operator_config= get_parser_config (name, "operator");

    \ \ customize_operator (operator_config);

    \ \ ... \ // likewise for "number", "string", "comment", "preprocessor"

    }
  </cpp-code>

  The module must therefore be called <verbatim|(<em|name>-lang)> and be
  found on the load path; the <verbatim|progs> directory of every plugin is
  on that path (see <cpp|plugin_path> in
  <verbatim|System/Boot/init_texmacs.cpp>). <cpp|get_parser_config>
  evaluates <scm|(tm-\<gtr\>tree (parser-feature <scm-arg|lan>
  <scm-arg|key>))>, and the <cpp|customize_*> methods walk through the
  resulting tree and call the configuration methods of the parsers. The
  string parser always gets the pairs <verbatim|"> and <verbatim|'>, each
  closed by itself; these delimiters cannot be configured from <scheme>.
  The identifier parser keeps its defaults.

  <subsection|Scanning>

  <cpp|prog_language_rep::advance> tries the parsers in a fixed order and
  records in <cpp|current_parser> the name of the one which succeeded:

  <\enumerate>
    <item>if the string parser is in the middle of a string (from a previous
    call): first the pending escape sequence, if any, then the continuation
    of the string;

    <item>blanks (returns <cpp|&tp_space_rep>);

    <item>preprocessor directive;

    <item>string;

    <item>number;

    <item>operator;

    <item>keyword;

    <item>identifier;

    <item>otherwise, one character is skipped (<cpp|tm_char_forwards>, which
    treats a <TeXmacs> symbol such as <verbatim|\<less\>alpha\<gtr\>> as one
    character) and <cpp|current_parser> is cleared.
  </enumerate>

  Inline comments are <em|not> recognized by <cpp|advance>: the text of a
  comment is scanned token by token like code, and the comment color is
  applied by <cpp|get_color> instead.

  <subsection|Coloring>

  <cpp|prog_language_rep::get_color> computes the color of the token
  <verbatim|[start, end)> as follows:

  <\enumerate>
    <item>if <cpp|in_comment (start, t)> holds, the token is inside a
    multi-line comment: color <verbatim|comment>;

    <item>if an inline comment start occurs anywhere in the line at a
    position <math|\<leqslant\>> <cpp|start>: color <verbatim|comment>;

    <item>otherwise the color class is derived from <cpp|current_parser>:
    <verbatim|string_parser> gives <verbatim|constant_string>,
    <verbatim|escaped_char_parser> gives <verbatim|constant_char>,
    <verbatim|number_parser> gives <verbatim|constant_number>,
    <verbatim|preprocessor_parser> gives
    <verbatim|preprocessor_directive>, and for <verbatim|operator_parser>
    and <verbatim|keyword_parser> the class is the <em|group> under which the
    operator or keyword was registered. Identifiers and unrecognized
    characters get no color.
  </enumerate>

  The class name is finally mapped to a color by
  <cpp|decode_color (lan_name, encode_color (type))>, see <hlink|colors,
  preferences and themes|syntax-highlighting-colors.en.tm>. As a
  consequence, the group names used in the <scheme> definitions must be
  color classes known to <cpp|encode_color>.

  <subsection|Multi-line comments>

  The function <cpp|in_comment> and its helpers in
  <verbatim|System/Language/impl_language.cpp> (and a private copy,
  <cpp|in_cpp_comment>, in <verbatim|cpp_language.cpp>) implement C-style
  <verbatim|/* ... */> comments spanning several lines. They need the
  neighbouring lines, which is why <cpp|advance> and <cpp|get_color> take a
  tree:

  <\explain>
    <cpp|int line_number (tree t)><explain-synopsis|index of a line>
  <|explain>
    Uses <cpp|obtain_ip (t)> to locate <cpp|t> in the global edit tree
    <cpp|the_et>, and returns its index if its parent is a
    <markup|document>, or <math|-1> otherwise.
  </explain>

  <\explain>
    <cpp|tree line_inc (tree t, int i)><explain-synopsis|neighbouring line>
  <|explain>
    Returns the line <cpp|i> positions below (or above, for negative
    <cpp|i>) the line <cpp|t> in the same <markup|document>, or
    <cpp|tree (_ERROR)> if there is no such line.
  </explain>

  <cpp|in_comment (pos, t)> searches backwards, starting with the current
  line, for the nearest line containing <verbatim|/*> (ignoring occurrences
  inside quoted strings), then searches forwards from there for a
  <verbatim|*/>, and decides whether the current position lies in between.
  The delimiters <verbatim|/*> and <verbatim|*/> are hard-coded and apply to
  <em|every> language which uses <cpp|prog_language_rep>; they cannot be
  configured through <scm|parser-feature>.

  <section|The <scheme> interface: <scm|parser-feature>>

  <subsection|The protocol>

  A language for <cpp|prog_language_rep> is described by overloading the
  function <scm|parser-feature> with <scm|tm-define> and a
  <scm|:require> clause. The fallback definitions are in
  <verbatim|prog/default-lang.scm>:

  <\scm-code>
    (texmacs-module (prog default-lang))

    \;

    (tm-define (parser-feature lan key)

    \ \ `(,(string-\<gtr\>symbol key)))

    \;

    (tm-define (parser-feature lan key)

    \ \ (:require (== key "comment"))

    \ \ `(,(string-\<gtr\>symbol key)

    \ \ \ \ (inline "//")))
  </scm-code>

  So by default a feature is empty, except that <verbatim|//> starts an
  inline comment. A language module overrides the features it needs; for
  example (abridged from <verbatim|src/plugins/python/progs/python-lang.scm>):

  <\scm-code>
    (texmacs-module (python-lang)

    \ \ (:use (prog default-lang)))

    \;

    (tm-define (parser-feature lan key)

    \ \ (:require (and (== lan "python") (== key "keyword")))

    \ \ `(,(string-\<gtr\>symbol key)

    \ \ \ \ (constant "False" "None" "True" ...)

    \ \ \ \ (declare_function "def" "lambda")

    \ \ \ \ (declare_module "import")

    \ \ \ \ (declare_type "class")

    \ \ \ \ (keyword "as" "del" "from" "global" "in" "is" "with")

    \ \ \ \ (keyword_conditional "break" "continue" "elif" "else" "for" "if" "while")

    \ \ \ \ (keyword_control "assert" "except" ... "yield")))

    \;

    (tm-define (parser-feature lan key)

    \ \ (:require (and (== lan "python") (== key "comment")))

    \ \ `(,(string-\<gtr\>symbol key)

    \ \ \ \ (inline "#")))
  </scm-code>

  The value of a feature is a list whose head is the feature name as a
  symbol and whose elements are lists <scm|(<scm-arg|name>
  <scm-arg|item> ...)>. It is converted to a tree by <scm|tm-\<gtr\>tree>,
  so that each element becomes a tree with label <scm-arg|name> and string
  children.

  <subsection|Reference of the features>

  <\explain>
    <scm|(parser-feature <scm-arg|lan> "keyword")><explain-synopsis|keywords
    and their color classes>
  <|explain>
    Each element <scm|(<scm-arg|group> <scm-arg|word> ...)> registers the
    words as keywords of color class <scm-arg|group>. Useful classes are
    <verbatim|keyword>, <verbatim|keyword_conditional>,
    <verbatim|keyword_control>, <verbatim|constant>,
    <verbatim|constant_type>, <verbatim|declare_function>,
    <verbatim|declare_module>, <verbatim|declare_type> and the other classes
    listed in the next chapter. Only words consisting of ASCII letters can
    match (see <cpp|keyword_parser_rep>).
  </explain>

  <\explain>
    <scm|(parser-feature <scm-arg|lan> "operator")><explain-synopsis|operators
    and their color classes>
  <|explain>
    Each element <scm|(<scm-arg|group> <scm-arg|op> ...)> registers operator
    strings with the color class <scm-arg|group>, typically
    <verbatim|operator>, <verbatim|operator_openclose> (brackets),
    <verbatim|operator_field> (<verbatim|.>, <verbatim|::>) or
    <verbatim|operator_special>. Operators may contain non-ASCII characters
    (written in UTF-8 in the <scheme> file; they are converted with
    <cpp|tm_encode>).
  </explain>

  <\explain>
    <scm|(parser-feature <scm-arg|lan> "number")><explain-synopsis|syntax of
    numbers>
  <|explain>
    Recognized elements:

    <\itemize>
      <item><scm|(bool_features <scm-arg|f> ...)> with flags among
      <verbatim|"prefix_0b">, <verbatim|"prefix_0o">,
      <verbatim|"prefix_0x">, <verbatim|"no_suffix_with_box"> and
      <verbatim|"sci_notation">;

      <item><scm|(separator <scm-arg|c>)> with a one-character string, the
      digit separator;

      <item><scm|(suffix (<scm-arg|group> <scm-arg|s> ...) ...)>, the
      allowed suffixes, in the same format as the keyword feature (the group
      names are not used for coloring: the whole number is colored as
      <verbatim|constant_number>).
    </itemize>
  </explain>

  <\explain>
    <scm|(parser-feature <scm-arg|lan> "string")><explain-synopsis|escape
    sequences in strings>
  <|explain>
    Recognized elements: <scm|(bool_features <scm-arg|f> ...)> with flags
    among <verbatim|"hex_with_8_bits">, <verbatim|"hex_with_16_bits">,
    <verbatim|"hex_with_32_bits"> and <verbatim|"octal_upto_3_digits">, and
    <scm|(escape_sequences <scm-arg|s> ...)>, the strings which may follow
    the backslash. String delimiters are always <verbatim|"> and
    <verbatim|'>.
  </explain>

  <\explain>
    <scm|(parser-feature <scm-arg|lan> "comment")><explain-synopsis|inline
    comments>
  <|explain>
    The element <scm|(inline <scm-arg|start> ...)> lists the strings which
    start a comment extending to the end of the line, for instance
    <scm|(inline "#" "%")> for <name|Octave>. Other elements are ignored.
  </explain>

  <\explain>
    <scm|(parser-feature <scm-arg|lan> "preprocessor")><explain-synopsis|preprocessor
    directives>
  <|explain>
    The element <scm|(directives <scm-arg|d> ...)> lists the directive names
    which may follow a <verbatim|#> at the beginning of a line. Such
    directives get the color class <verbatim|preprocessor_directive>. In
    the current sources only <verbatim|cpp-lang.scm> defines directives, and
    it is not used by the C++ highlighter, so this feature is effectively
    unused.
  </explain>

  The features are queried only once, when the language object is created;
  redefining <scm|parser-feature> later has no effect until <TeXmacs> is
  restarted.

  <section|Highlighting with packrat grammars>

  An independent and more powerful mechanism highlights text according to a
  packrat grammar. It is used for the semantic analysis of mathematics, and
  can also be used in programming mode through <cpp|verb_language_rep>.
  Grammars are defined with the macro <scm|define-language>
  (<verbatim|kernel/texmacs/tm-language.scm>):

  <\scm-code>
    (define-language minimal-grammar

    \ \ (:synopsis "grammar for a minimal test language")

    \ \ (define Main

    \ \ \ \ (Spc Instructions Spc))

    \ \ ...

    \ \ (define Identifier

    \ \ \ \ (:highlight variable_identifier)

    \ \ \ \ (+ (or (- "a" "z") (- "A" "Z"))))

    \ \ (define Number

    \ \ \ \ (:highlight constant_number)

    \ \ \ \ ((+ (- "0" "9")) (or "" ("." (+ (- "0" "9"))))))

    \ \ ...)

    \;

    (define-language minimal

    \ \ (:synopsis "syntax for a minimal test language")

    \ \ (inherit minimal-operators)

    \ \ (inherit minimal-grammar))
  </scm-code>

  Each <scm|(define <scm-arg|Symbol> <scm-arg|property> ... <scm-arg|rule>
  ...)> defines a nonterminal by alternatives. Strings are terminals,
  lists are sequences, and <scm|(or ...)>, <scm|(* ...)>, <scm|(+ ...)>,
  <scm|(- <scm-arg|from> <scm-arg|to>)>, <scm|(not ...)> and
  <scm|(except <scm-arg|x> <scm-arg|y>)> are the usual packrat combinators;
  the keywords <scm|:any>, <scm|:args>, <scm|:leaf>, <scm|:char>,
  <scm|:cursor>, <scm|:\<less\>>, <scm|:/> and <scm|:\<gtr\>> match
  <TeXmacs> markup (see <scm|scheme-\<gtr\>packrat>). The properties
  relevant for highlighting are:

  <\description>
    <item*|<scm|(:highlight <scm-arg|class>)>>text matched by this
    nonterminal gets the color class <scm-arg|class> (a name known to
    <cpp|encode_color>); subterms are not highlighted further;

    <item*|<scm|(:transparent <scm-arg|class>)>>like <scm|:highlight>, but
    the subterms are highlighted too, possibly overriding the color.
  </description>

  Other properties (<scm|:type>, <scm|:penalty>, <scm|:spacing>,
  <scm|:limits>, <scm|:operator>, <scm|:focus>, <scm|:selectable>,
  <scm|:atomic>) are used for mathematics. A grammar can be loaded lazily
  with <scm|(lazy-language <scm-arg|module> <scm-arg|lan> ...)>; this is
  how <verbatim|init-texmacs.scm> declares <verbatim|minimal> and
  <verbatim|std-math>, and <cpp|find_packrat_grammar>
  (<verbatim|System/Language/packrat_grammar.cpp>) forces the promise with
  <scm|lazy-language-force>.

  At run time, <cpp|packrat_abbreviation (lan, "Main")> returns a small
  positive integer for each pair of a grammar and a start symbol, or 0 if
  the symbol does not exist. A language whose <cpp|hl_lan> is nonzero is
  highlighted as follows:

  <\enumerate>
    <item><cpp|concater_rep::typeset> calls
    <cpp|language_rep::highlight> on each subtree; if the tree does not yet
    carry highlighting information, <cpp|packrat_highlight> parses it (for a
    <markup|document>, only the range of lines which lost their highlighting
    is re-parsed, after <cpp|consistent_enlargement>) and calls
    <cpp|packrat_parser_rep::highlight>, which walks the parse and, for each
    nonterminal with a <verbatim|highlight> property, stores the encoded color
    class for the matched characters.

    <item>The colors are stored per string as an <cpp|array\<less\>int\<gtr\>>
    in a <em|highlight observer> attached to the tree
    (<verbatim|Data/Observers/highlight_observer.cpp>,
    <cpp|attach_highlight>, <cpp|obtain_highlight>, <cpp|has_highlight>,
    <cpp|detach_highlight>). Any modification of the tree removes the
    observer (<cpp|highlight_observer_rep::announce>), which invalidates the
    highlighting exactly where needed.

    <item><cpp|verb_language_rep::advance> splits tokens where the stored
    color changes, and <cpp|verb_language_rep::get_color> returns
    <cpp|decode_color (res_name, cols[start])>.
  </enumerate>

  Since the whole program must be parsed by the start symbol
  <verbatim|Main>, this method is mainly suitable for small, well-formed
  languages; nothing is highlighted if the parse fails.

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
