<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|From a code environment to colored text>

  <section|Where programming mode comes from>

  Programming mode is entered in three ways.

  <\description>
    <item*|Code markup>The package <verbatim|environment/env-program.ts>
    defines one inline macro per language, which simply changes the mode, the
    language and the font family:

    <\tm-fragment>
      <inactive*|<assign|python|<macro|body|<with|mode|prog|prog-language|python|font-family|rm|<arg|body>>>>>
    </tm-fragment>

    The inline macros are <markup|scm>, <markup|cpp>, <markup|java>,
    <markup|python>, <markup|julia>, <markup|scala>, <markup|r>,
    <markup|scilab>, <markup|fortran>, <markup|shell>, <markup|mmx>
    (<name|Mathemagix>), <markup|dot-lang>, <markup|javascript-lang>,
    <markup|json-lang>, <markup|octave-lang> and <markup|minimal>. For each
    of them there is a block variant (<markup|scm-code>, <markup|cpp-code>,
    <markup|python-code>, <markup|dot-code>, <markup|json-code>,
    <markup|mmx-code>, ...) which wraps the inline macro inside
    <markup|pseudo-code>, itself defined via <markup|render-code> (no
    indentation of the first line, no separation between paragraphs,
    padding above and below). The <scheme> groups <scm|inline-code-tag> and
    <scm|block-code-tag> in <verbatim|text/text-drd.scm> list these tags,
    and the menu <scm|code-menu> in <verbatim|text/text-menu.scm>
    (shown as the submenu \PProgram\Q) offers them to the user.

    <item*|Sessions>In <verbatim|compute/session.ts> the macro
    <markup|session> sets <src-var|prog-language> to the name of the plugin
    (<verbatim|python>, <verbatim|maxima>, ...) and <markup|input> and
    <markup|output> switch to <verbatim|prog> mode. Session input is
    therefore highlighted with the language of the plugin, if that language
    is known (otherwise the fallback language is used, see below). Similarly,
    <verbatim|compute/scripts.ts> typesets script input in programming mode
    with <src-var|prog-language> set to the scripting language.

    <item*|Source files>When a file in a programming format is loaded, the
    function <cpp|attach_subformat> (<verbatim|Texmacs/Data/new_buffer.cpp>)
    sets the initial environment of the buffer to <src-var|mode>=<verbatim|prog>
    and <src-var|prog-language>=<em|format>, and selects the style
    <verbatim|code> (<verbatim|src/TeXmacs/styles/test/code.ts>). This
    happens only if the format exists and either a
    <verbatim|<em|format>-lang.scm> file is found (<cpp|prog_lang_exists>) or
    the format is one of <verbatim|mathemagix>, <verbatim|scilab> or
    <verbatim|scheme>.
  </description>

  The default values are set in <verbatim|Typeset/Env/env_default.cpp>:
  <src-var|prog-language> is <verbatim|scheme> and <src-var|prog-scripts> is
  <verbatim|none>. The fonts used in programming mode are controlled by
  <src-var|prog-font>, <src-var|prog-font-family>,
  <src-var|prog-font-series> and <src-var|prog-font-shape>, and
  <src-var|prog-session> holds the name of the current session. All these
  variable names are declared in <verbatim|Data/Drd/vars.cpp>
  (<cpp|PROG_LANGUAGE>, <cpp|PROG_SCRIPTS>, ...). Note that
  <src-var|prog-scripts> is only used by the scripting markup in
  <verbatim|compute/scripts.ts>; it plays no role in highlighting.

  <section|Selecting the language object>

  The typesetting environment <cpp|edit_env_rep> (<verbatim|Typeset/env.hpp>)
  holds the current language in its member <cpp|language lan>, together with
  the integer <cpp|mode> (1 for text, 2 for math, 3 for programs, 0
  otherwise) and an integer <cpp|hl_lan> which is used by packrat based
  highlighting. In <verbatim|Typeset/Env/env_semantics.cpp>, the variable
  <src-var|prog-language> has the type <cpp|Env_Language> and
  <src-var|mode> has the type <cpp|Env_Mode>; whenever one of them changes,
  <cpp|update_language> is called:

  <\cpp-code>
    void

    edit_env_rep::update_language () {

    \ \ switch (mode) {

    \ \ case 0:

    \ \ case 1:

    \ \ \ \ lan= text_language (get_string (LANGUAGE));

    \ \ \ \ break;

    \ \ case 2:

    \ \ \ \ lan= math_language (get_string (MATH_LANGUAGE));

    \ \ \ \ break;

    \ \ case 3:

    \ \ \ \ lan= prog_language (get_string (PROG_LANGUAGE));

    \ \ \ \ break;

    \ \ }

    \ \ hl_lan= lan-\<gtr\>hl_lan;

    }
  </cpp-code>

  The editor uses the same rules in <cpp|edit_typeset_rep::get_env_language>
  (<verbatim|Edit/Editor/edit_typeset.cpp>) to obtain the language at the
  cursor.

  <section|The language registry>

  The class <cpp|language> is a <em|resource> (macro <cpp|RESOURCE> in
  <verbatim|Kernel/Abstractions/resource.hpp>): every <cpp|language_rep> has
  a name <cpp|res_name>, and its constructor registers the object in the
  static table <cpp|language::instances>. Language objects are not
  destroyed in practice; they are shared by all documents, views and typesetters for the
  whole session.

  <\explain>
    <cpp|language prog_language (string s)><explain-synopsis|language object
    for a programming language>
  <|explain>
    Returns the cached object if <cpp|language::instances> already contains
    <cpp|s>. Otherwise it creates one, using the following rules in this
    order:

    <\itemize>
      <item><verbatim|scheme>: <cpp|scheme_language_rep>;

      <item><verbatim|mathemagix>, <verbatim|mmi>, <verbatim|caas>,
      <verbatim|mmshell>: <cpp|mathemagix_language_rep>;

      <item><verbatim|cpp>: <cpp|cpp_language_rep>;

      <item><verbatim|scilab>: <cpp|scilab_language_rep>;

      <item><verbatim|r>: <cpp|r_language_rep>;

      <item><verbatim|fortran>: <cpp|fortran_language_rep>;

      <item>if <cpp|format_exists (s)> and <cpp|prog_lang_exists (s)>: the
      generic, <scheme>-configured <cpp|prog_language_rep>;

      <item>otherwise: the fallback <cpp|verb_language_rep>.
    </itemize>
  </explain>

  <\explain>
    <cpp|bool prog_lang_exists (string s)><explain-synopsis|is there a
    language definition file?>
  <|explain>
    Returns <cpp|true> if a file <verbatim|<em|s>-lang.scm> exists in one of
    <verbatim|$TEXMACS_PATH/progs/prog/>,
    <verbatim|$TEXMACS_PATH/plugins/<em|s>/progs/>,
    <verbatim|$TEXMACS_PATH/plugins/code/progs/>,
    <verbatim|$TEXMACS_HOME_PATH/plugins/<em|s>/progs/> or
    <verbatim|$TEXMACS_HOME_PATH/plugins/code/progs/>.
  </explain>

  <cpp|format_exists> (<verbatim|Data/Convert/Generic/generic.cpp>) calls the
  <scheme> predicate <scm|format?> (<verbatim|kernel/texmacs/tm-convert.scm>),
  which is true when the format was declared with <scm|define-format>. The
  formats of the programming languages are declared lazily in
  <verbatim|init-texmacs.scm>:

  <\scm-code>
    (lazy-format (prog prog-format) scheme)

    (lazy-format (code-format) cpp julia scala java json csv)

    (lazy-format (mathemagix-format) mathemagix)

    (lazy-format (caas-format) caas)

    (lazy-format (python-format) python)

    (lazy-format (scilab-format) scilab)
  </scm-code>

  The modules <verbatim|(code-format)> and <verbatim|(python-format)> live in
  the plugins <verbatim|code> and <verbatim|python>. Hence a language gets the
  generic highlighter only if <em|both> a format and a
  <verbatim|-lang.scm> file exist. For instance <verbatim|python>,
  <verbatim|java>, <verbatim|scala>, <verbatim|julia>, <verbatim|json> and
  <verbatim|csv> use <cpp|prog_language_rep>, whereas, in the current
  sources, there is no <scm|define-format> for <verbatim|javascript>,
  <verbatim|dot> or <verbatim|octave>, although their <verbatim|-lang.scm>
  files exist in the <verbatim|code> and <verbatim|octave> plugins.

  <section|The class <cpp|language_rep>>

  The abstract interface is shared with text and mathematics
  (<verbatim|System/Language/language.hpp>):

  <\cpp-code>
    struct language_rep: rep\<less\>language\<gtr\> {

    \ \ string lan_name; \ // name of the language

    \ \ int hl_lan;

    \ \ static hashmap\<less\>string,int\<gtr\> color_encoding;

    \ \ hashmap\<less\>int,string\<gtr\> color_decoding;

    \ \

    \ \ language_rep (string s);

    \ \ virtual text_property advance (tree t, int& pos) = 0;

    \ \ virtual array\<less\>int\<gtr\> get_hyphens (string s) = 0;

    \ \ virtual void hyphenate (string s, int after, string& l, string& r) = 0;

    \ \ virtual string get_group (string s);

    \ \ virtual array\<less\>string\<gtr\> get_members (string s);

    \ \ virtual void highlight (tree t);

    \ \ virtual string get_color (tree t, int start, int end);

    };
  </cpp-code>

  <\explain>
    <cpp|text_property advance (tree t, int& pos)><explain-synopsis|lexical
    scanner>
  <|explain>
    Advances <cpp|pos> over the next token of the string <cpp|t-\<gtr\>label>
    and returns a pointer to a <cpp|text_property_rep>, which tells the
    typesetter how much space and which line breaking penalties to put around
    the token. Programming languages return <cpp|&tp_space_rep> for a blank
    and <cpp|&tp_normal_rep> for everything else; these global objects are
    defined in <verbatim|language.cpp> and declared <cpp|extern> in
    <verbatim|impl_language.hpp>. The argument is a <cpp|tree> rather than a
    <cpp|string> so that implementations may find the position of the line
    in the document (see the section on multi-line comments in the <hlink|next
    chapter|syntax-highlighting-parsers.en.tm>).
  </explain>

  <\explain>
    <cpp|string get_color (tree t, int start, int end)><explain-synopsis|color
    of a token>
  <|explain>
    Returns a color specification for the token
    <cpp|t-\<gtr\>label (start, end)> or the empty string for the default
    color. The specification is either the name of an environment variable
    (like <verbatim|keyword-color>) or a color (like
    <verbatim|dark green> or <verbatim|#309090>). The default implementation
    returns <cpp|"">.
  </explain>

  <\explain>
    <cpp|void highlight (tree t)><explain-synopsis|packrat highlighting>
  <|explain>
    If <cpp|hl_lan> is nonzero and <cpp|t> does not yet carry highlighting
    information for this language, runs the packrat highlighter
    <cpp|packrat_highlight (res_name, "Main", t)>. This is called by
    <cpp|concater_rep::typeset> for every subtree before it is typeset, as
    long as <cpp|env-\<gtr\>hl_lan != 0>.
  </explain>

  For programming languages, <cpp|get_hyphens> and <cpp|hyphenate> are
  trivial: only a position directly after a <verbatim|-> followed by a letter
  is a (discouraged) break point, so long lines of code are essentially never
  hyphenated.

  <section|The concrete language classes>

  <\description>
    <item*|<cpp|prog_language_rep>>The generic highlighter
    (<verbatim|prog_language.cpp>). It derives from
    <cpp|abstract_language_rep>, which owns one instance of each parser of
    <verbatim|Data/Parser/>, and is configured at construction time from the
    <scheme> function <scm|parser-feature>. Its colors come from the
    preference based <em|color decoding> (see <hlink|colors, preferences and
    themes|syntax-highlighting-colors.en.tm>). Used for <name|Python>,
    <name|Java>, <name|Scala>, <name|Julia>, <abbr|JSON>, <abbr|CSV> and any
    new language added in the recommended way.

    <item*|<cpp|scheme_language_rep>>A small hand-written scanner for
    <scheme> (<verbatim|scheme_language.cpp>). Tokens are delimited by spaces
    and parentheses. <cpp|get_color> scans back at most 1000 characters on
    the current line to detect comments and strings, colors numbers,
    strings, keywords (<verbatim|:foo>) and the symbols listed in
    <scm|highlight-any>, and asks <scheme> whether other symbols are
    <scm|defined?> (the answer is cached in the member <cpp|colored>). The
    keyword list <scm|highlight-any> is read from <verbatim|tm-mode.el>, the
    <name|Emacs> mode for <TeXmacs> <scheme> code, by the module
    <verbatim|utils/misc/tm-keywords.scm>. The returned colors are names of
    environment variables (<verbatim|comment-color>,
    <verbatim|string-color>, <verbatim|number-color>,
    <verbatim|alt-keyword-color>, <verbatim|alt-constant-color>,
    <verbatim|defined-color>).

    <item*|<cpp|cpp_language_rep>>A hand-written highlighter for C and
    C++ (<verbatim|cpp_language.cpp>) with built-in tables of keywords, types
    and constants, support for preprocessor lines (the test for continuation
    lines, <cpp|end_preprocessing>, looks for a trailing <verbatim|/>
    rather than a backslash) and C-style comments spanning several lines. It returns
    environment variable names. Note that, although
    <verbatim|plugins/code/progs/cpp-lang.scm> contains a complete
    <scm|parser-feature> description for <verbatim|cpp>, the dispatcher
    <cpp|prog_language> never uses it, since <verbatim|cpp> is matched
    earlier.

    <item*|<cpp|mathemagix_language_rep>, <cpp|r_language_rep>>Hand-written
    highlighters (<verbatim|mathemagix_language.cpp>,
    <verbatim|r_language.cpp>). The former returns environment variable
    names, plus a fixed color <cpp|COLOR_MARKUP> for markup; the latter
    mostly returns literal colors such as <verbatim|#8020c0> or
    <verbatim|dark green>.

    <item*|<cpp|scilab_language_rep>, <cpp|fortran_language_rep>>Hand-written
    highlighters (<verbatim|scilab_language.cpp>,
    <verbatim|fortran_language.cpp>) which, like <cpp|prog_language_rep>,
    use <cpp|decode_color> and thus the preferences
    <verbatim|syntax:scilab:*> and <verbatim|syntax:fortran:*>.

    <item*|<cpp|verb_language_rep>>The fallback (<verbatim|verb_language.cpp>).
    It splits the text at spaces and at the characters <verbatim|->,
    <verbatim|/>, <verbatim|\\>, <verbatim|,> and <verbatim|?>, which become
    allowed line breaks. Its constructor sets
    <cpp|hl_lan= packrat_abbreviation (res_name, "Main")>, so that if a
    packrat grammar with the same name as the language and a symbol
    <verbatim|Main> exists, the text is highlighted according to that
    grammar; otherwise no colors are produced. The test language
    <verbatim|minimal> (<verbatim|language/minimal.scm>, markup
    <markup|minimal>) works in this way.
  </description>

  Text and mathematics use other subclasses (<verbatim|text_language.cpp>,
  <verbatim|math_language.cpp>), which are outside the scope of this
  chapter.

  <section|Typesetting a string in programming mode>

  The entry point for atomic trees is <cpp|concater_rep::typeset>
  (<verbatim|Typeset/Concat/concater.cpp>):

  <\cpp-code>
    if (env-\<gtr\>hl_lan != 0)

    \ \ env-\<gtr\>lan-\<gtr\>highlight (t);

    \;

    if (is_atomic (t)) {

    \ \ if \ \ \ \ \ (env-\<gtr\>mode == 1) typeset_text_string (t, ip, 0, N(t-\<gtr\>label));

    \ \ else if (env-\<gtr\>mode == 2) typeset_math_string (t, ip, 0, N(t-\<gtr\>label));

    \ \ else if (env-\<gtr\>mode == 3) typeset_prog_string (t, ip, 0, N(t-\<gtr\>label));

    \ \ else \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ typeset_text_string (t, ip, 0, N(t-\<gtr\>label));

    \ \ return;

    }
  </cpp-code>

  The same dispatch occurs in <cpp|concater_rep::typeset_range>
  (<verbatim|Typeset/Concat/concat_macro.cpp>), which typesets a substring
  <verbatim|[i1, i2)> of a string; this is why <cpp|typeset_prog_string>
  receives the whole tree together with a start and an end position. The
  core loop is (abridged):

  <\cpp-code>
    void

    concater_rep::typeset_prog_string (tree t, path ip, int pos, int end) {

    \ \ array\<less\>space\<gtr\> spc_tab= env-\<gtr\>fn-\<gtr\>get_normal_spacing (env-\<gtr\>spacing_policy);

    \ \ string s= t-\<gtr\>label;

    \ \ int \ \ \ start;

    \ \ do {

    \ \ \ \ start= pos;

    \ \ \ \ text_property tp= env-\<gtr\>lan-\<gtr\>advance (t, pos);

    \ \ \ \ if (pos \<gtr\> end) pos= end;

    \ \ \ \ if ((pos-start == 1) && (s[start]==' ')) { // spaces

    \ \ \ \ \ \ ...

    \ \ \ \ }

    \ \ \ \ else { // strings

    \ \ \ \ \ \ penalty_max (tp-\<gtr\>pen_before);

    \ \ \ \ \ \ PRINT_SPACE (tp-\<gtr\>spc_before)

    \ \ \ \ \ \ string color= env-\<gtr\>lan-\<gtr\>get_color (t, start, pos);

    \ \ \ \ \ \ string content= s (start, pos);

    \ \ \ \ \ \ typeset_colored_substring (content, ip, start, color);

    \ \ \ \ \ \ penalty_min (tp-\<gtr\>pen_after);

    \ \ \ \ \ \ PRINT_SPACE (tp-\<gtr\>spc_after)

    \ \ \ \ }

    \ \ } while (pos\<less\>end);

    }
  </cpp-code>

  Two properties of this loop matter for language implementors:

  <\itemize>
    <item><cpp|get_color> is always called <em|immediately after> the
    <cpp|advance> which produced the token, with the same tree. Several
    implementations exploit this by remembering in <cpp|advance> which parser
    recognized the token (the member <cpp|current_parser> of
    <cpp|abstract_language_rep>) and reading it back in <cpp|get_color>.

    <item>A single space is typeset as a space (subject to the spacing given
    by the text property); every other token, including runs of several
    characters which a language did not recognize, becomes one text box with
    a single color.
  </itemize>

  The color specification is resolved by:

  <\cpp-code>
    void

    concater_rep::typeset_colored_substring

    \ \ (string s, path ip, int pos, string col)

    {

    \ \ color c;

    \ \ if (col == "")

    \ \ \ \ c= apply_alpha (env-\<gtr\>pen-\<gtr\>get_color (), env-\<gtr\>alpha);

    \ \ else if (env-\<gtr\>provides (col)) {

    \ \ \ \ tree t= env-\<gtr\>read (col);

    \ \ \ \ if (t == "") c= apply_alpha (env-\<gtr\>pen-\<gtr\>get_color (), env-\<gtr\>alpha);

    \ \ \ \ else c= named_color (as_string (t), env-\<gtr\>alpha);

    \ \ }

    \ \ else c= named_color (col, env-\<gtr\>alpha);

    \ \ box b= text_box (ip, pos, s, env-\<gtr\>fn, c);

    \ \ a \<less\>\<less\> line_item (STRING_ITEM, OP_TEXT, b, HYPH_INVALID, env-\<gtr\>lan);

    }
  </cpp-code>

  An empty specification, or an environment variable whose value is empty
  (by default <src-var|operator-color> and <src-var|misc-lexeme-color>), means
  \Puse the current text color\Q. Since the color is looked up in the
  environment at typesetting time, colors given as variable names follow the
  current theme and any <markup|with> in the document, whereas literal colors
  do not.

  The resulting line items are ordinary text boxes; line breaking, cursor
  movement and selections work exactly as for text (see the chapters on the
  <hlink|typesetter|typesetter.en.tm> and on <hlink|boxes|boxes.en.tm>).

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
