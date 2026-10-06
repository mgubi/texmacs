<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Colors, preferences and themes>

  <section|Three kinds of color specifications>

  As explained in the chapter on the <hlink|typesetting of
  code|syntax-highlighting-pipeline.en.tm>, <cpp|language_rep::get_color>
  returns a string which <cpp|concater_rep::typeset_colored_substring>
  interprets either as the name of an environment variable or as a color.
  Historically, the various highlighters have made different choices, so
  that there are three families:

  <\description>
    <item*|Environment variables>The <scheme>, C++ and <name|Mathemagix>
    highlighters return names such as <verbatim|keyword-color> or
    <verbatim|comment-color>. The actual color is the value of that variable
    at the place where the code is typeset. These colors can be changed by
    style files, themes and <markup|with>, and they adapt to dark
    backgrounds.

    <item*|Color classes and preferences>The generic highlighter
    <cpp|prog_language_rep> (<name|Python>, <name|Java>, <name|Scala>,
    <name|Julia>, <abbr|JSON>, ...), the <name|Fortran> and <name|Scilab>
    highlighters and the packrat based highlighting of
    <cpp|verb_language_rep> compute a <em|color class> such as
    <verbatim|keyword_conditional> or <verbatim|constant_string>, encode it
    as an integer, and decode it into a color with user preferences. These
    colors are global for each language; they do not depend on the
    document or its theme.

    <item*|Literal colors>The <name|R> highlighter returns fixed colors
    (<verbatim|#8020c0>, <verbatim|dark green>, ...), and the <name|R> and
    <name|Mathemagix> highlighters use the fixed color <cpp|COLOR_MARKUP>
    for markup.
  </description>

  <section|Color classes>

  The color classes form a fixed vocabulary, defined in
  <cpp|initialize_color_encodings> (<source-link|System/Language/language.cpp|src/System/Language/language.cpp>):

  <\explain>
    <cpp|int encode_color (string s)><explain-synopsis|number of a color
    class>
  <|explain>
    Returns the integer code of the class <cpp|s>, or <math|-1> if the class
    is unknown. The table <cpp|language_rep::color_encoding> is static and
    shared by all languages; it is initialized on first use.
  </explain>

  <\explain>
    <cpp|string decode_color (string lan, int c)><explain-synopsis|color for a
    code in a language>
  <|explain>
    Returns the color associated to the code <cpp|c> for the language named
    <cpp|lan> (as given by <cpp|prog_language (lan)>), or <cpp|""> if there
    is none. The per-language table <cpp|color_decoding> is filled by
    <cpp|initialize_color_decodings> the first time it is needed.
  </explain>

  <\explain>
    <cpp|void initialize_color_decodings (string lan)><explain-synopsis|(re)read
    color preferences>
  <|explain>
    For each class <em|c>, reads the preference
    <verbatim|syntax:<em|lan>:<em|c>> with <cpp|get_preference>, falling back
    to a built-in default, and stores the result in the decoding table of the
    language. The code <math|-1> (unknown class) is decoded with the
    preference <verbatim|syntax:<em|lan>:none>. This function is exported to
    <scheme> as <scm|(syntax-read-preferences <scm-arg|lan>)>
    (<source-link|Scheme/Glue/build-glue-basic.scm|src/Scheme/Glue/build-glue-basic.scm>).
  </explain>

  The classes, their codes and the built-in defaults are:

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|font-series|bold>|<table|<row|<cell|Class>|<cell|Code>|<cell|Default>>|<row|<cell|<verbatim|comment>>|<cell|1>|<cell|<verbatim|brown>>>|<row|<cell|<verbatim|error>>|<cell|3>|<cell|<verbatim|dark
  red>>>|<row|<cell|<verbatim|preprocessor>>|<cell|4>|<cell|<verbatim|#004000>>>|<row|<cell|<verbatim|preprocessor_directive>>|<cell|5>|<cell|<verbatim|#20a000>>>|<row|<cell|<verbatim|constant>,
  <verbatim|constant_identifier>, <verbatim|constant_function>,
  <verbatim|constant_type>, <verbatim|constant_category>,
  <verbatim|constant_module>>|<cell|10\U15>|<cell|<verbatim|#4040c0>>>|<row|<cell|<verbatim|constant_number>>|<cell|16>|<cell|<verbatim|#3030b0>>>|<row|<cell|<verbatim|constant_string>>|<cell|17>|<cell|<verbatim|dark
  grey>>>|<row|<cell|<verbatim|constant_char>>|<cell|18>|<cell|<verbatim|#333333>>>|<row|<cell|<verbatim|variable>>|<cell|20>|<cell|<verbatim|#606060>>>|<row|<cell|<verbatim|variable_identifier>>|<cell|21>|<cell|<verbatim|#204080>>>|<row|<cell|<verbatim|variable_function>>|<cell|22>|<cell|<verbatim|#606060>>>|<row|<cell|<verbatim|variable_type>,
  <verbatim|variable_category>,
  <verbatim|variable_module>>|<cell|23\U25>|<cell|<verbatim|#00c000>>>|<row|<cell|<verbatim|variable_ioarg>>|<cell|26>|<cell|<verbatim|#00b000>>>|<row|<cell|<verbatim|declare>,
  <verbatim|declare_identifier>, <verbatim|declare_function>,
  <verbatim|declare_type>>|<cell|30\U33>|<cell|<verbatim|#0000c0>>>|<row|<cell|<verbatim|declare_category>>|<cell|34>|<cell|<verbatim|#d030d0>>>|<row|<cell|<verbatim|declare_module>>|<cell|35>|<cell|<verbatim|#0000c0>>>|<row|<cell|<verbatim|operator>>|<cell|40>|<cell|<verbatim|#8b008b>>>|<row|<cell|<verbatim|operator_openclose>>|<cell|41>|<cell|<verbatim|#B02020>>>|<row|<cell|<verbatim|operator_field>>|<cell|42>|<cell|<verbatim|#888888>>>|<row|<cell|<verbatim|operator_special>>|<cell|43>|<cell|<verbatim|orange>>>|<row|<cell|<verbatim|keyword>,
  <verbatim|keyword_conditional>>|<cell|50, 51>|<cell|<verbatim|#309090>>>|<row|<cell|<verbatim|keyword_control>>|<cell|52>|<cell|<verbatim|#000080>>>|<row|<cell|(unknown)>|<cell|<math|-1>>|<cell|<verbatim|red>>>>>>>
    Color classes for syntax highlighting.
  </big-table>

  Code 0 means \Pno color\Q; it is the initial value in the highlight arrays
  of the packrat mechanism. Note that a group name which is not in this
  table, such as <verbatim|operator_decoration> used by
  <source-link|python-lang.scm|plugins/python/progs/python-lang.scm> and <source-link|julia-lang.scm|plugins/code/progs/julia-lang.scm>, is encoded as
  <math|-1> and therefore drawn in the <verbatim|none> color, which is red by
  default.

  <section|Preferences>

  The preferences <verbatim|syntax:<em|lan>:<em|class>> are ordinary user
  preferences. Language modules declare defaults for them with
  <scm|define-preferences>, together with a notification function which
  re-reads the decoding table when the user changes a value:

  <\scm-code>
    (define (notify-python-syntax var val)

    \ \ (syntax-read-preferences "python"))

    \;

    (define-preferences

    \ \ ("syntax:python:none" "red" notify-python-syntax)

    \ \ ("syntax:python:comment" "brown" notify-python-syntax)

    \ \ ("syntax:python:keyword" "#309090" notify-python-syntax)

    \ \ ...)
  </scm-code>

  Such declarations exist for <verbatim|python> (in
  <source-link|src/plugins/python/progs/python-lang.scm|plugins/python/progs/python-lang.scm>), <verbatim|julia> and
  <verbatim|cpp> (in <verbatim|src/plugins/code/progs/>), <verbatim|scheme>
  (in <source-link|prog/scheme-edit.scm|TeXmacs/progs/prog/scheme-edit.scm>) and <verbatim|fortran> (in
  <source-link|prog/fortran-edit.scm|TeXmacs/progs/prog/fortran-edit.scm>). The <scheme> and C++ highlighters do
  not use the decoding tables, so their <verbatim|syntax:*> preferences
  currently have no visible effect. Other languages (<name|Java>,
  <name|Scala>, <abbr|JSON>, ...) simply use the built-in defaults of the
  table above unless the user sets the preferences by hand.

  Two practical consequences:

  <\itemize>
    <item>The decoding table is read once per language and session. After a
    change of preference, the notification refreshes the table, but boxes
    which were already typeset keep their colors until the corresponding
    text is typeset again (for instance after an edit or an explicit
    re-typesetting of the document).

    <item>Since these colors are global, a document which is shown with a
    dark background keeps the light-background colors of the preferences.
    Only the environment variable based highlighters adapt automatically.
  </itemize>

  <section|Environment variables and themes>

  The environment variables used by the <scheme>, C++ and <name|Mathemagix>
  highlighters are declared in <source-link|Data/Drd/vars.cpp|src/Data/Drd/vars.cpp> (C++ constants
  <cpp|KEYWORD_COLOR>, <cpp|CONSTANT_COLOR>, ...), get the <abbr|DRD> type
  <cpp|TYPE_COLOR> in <source-link|Data/Drd/drd_std.cpp|src/Data/Drd/drd_std.cpp>, and have the
  following defaults in <source-link|Typeset/Env/env_default.cpp|src/Typeset/Env/env_default.cpp>:

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|font-series|bold>|<table|<row|<cell|Variable>|<cell|Default>|<cell|Use>>|<row|<cell|<src-var|keyword-color>>|<cell|<verbatim|#8020c0>>|<cell|keywords>>|<row|<cell|<src-var|constant-color>>|<cell|<verbatim|#2060c0>>|<cell|constants>>|<row|<cell|<src-var|number-color>>|<cell|<verbatim|#2060c0>>|<cell|numbers>>|<row|<cell|<src-var|string-color>>|<cell|<verbatim|#a06040>>|<cell|strings>>|<row|<cell|<src-var|operator-color>>|<cell|(empty)>|<cell|operators>>|<row|<cell|<src-var|comment-color>>|<cell|<verbatim|brown>>|<cell|comments>>|<row|<cell|<src-var|preprocessor-color>>|<cell|<verbatim|#400040>>|<cell|preprocessor
  directives>>|<row|<cell|<src-var|modifier-color>>|<cell|<verbatim|#8020c0>>|<cell|declaration
  modifiers>>|<row|<cell|<src-var|declaration-color>>|<cell|<verbatim|#0000e0>>|<cell|declarations>>|<row|<cell|<src-var|macro-color>>|<cell|<verbatim|#00A0A0>>|<cell|macro
  declarations>>|<row|<cell|<src-var|function-color>>|<cell|<verbatim|#606060>>|<cell|functions>>|<row|<cell|<src-var|type-color>>|<cell|<verbatim|dark
  green>>|<cell|types>>|<row|<cell|<src-var|defined-color>>|<cell|<verbatim|#204080>>|<cell|defined
  identifiers>>|<row|<cell|<src-var|misc-lexeme-color>>|<cell|(empty)>|<cell|miscellaneous
  lexemes>>|<row|<cell|<src-var|alt-keyword-color>>|<cell|<verbatim|#309090>>|<cell|alternative
  keywords>>|<row|<cell|<src-var|alt-constant-color>>|<cell|<verbatim|#800080>>|<cell|alternative
  constants>>>>>>
    Environment variables for syntax highlighting.
  </big-table>

  These variables are also documented from the user's point of view among
  the <hlink|miscellaneous environment
  variables|../format/environment/env-misc.en.tm>.

  The theme mechanism groups them. In <source-link|themes/base/base-colors.ts|TeXmacs/packages/themes/base/base-colors.ts>:

  <\tm-fragment>
    <inactive*|<new-theme|highlight-colors|keyword-color|constant-color|number-color|string-color|operator-color|comment-color|preprocessor-color|modifier-color|declaration-color|macro-color|function-color|type-color|defined-color|misc-lexeme-color|alt-keyword-color|alt-constant-color>>

    <inactive*|<copy-theme|all-colors|colors|gui-colors|highlight-colors|session-colors>>
  </tm-fragment>

  The primitive <markup|new-theme> (<cpp|edit_env_rep::exec_new_theme> in
  <source-link|Typeset/Env/env_exec.cpp|src/Typeset/Env/env_exec.cpp>) creates, for each listed variable
  <em|v>, a variable <verbatim|highlight-colors-<em|v>> initialized with the
  current value of <em|v>, and a macro <markup|with-highlight-colors> which
  sets each <em|v> to the value of <verbatim|highlight-colors-<em|v>>.
  <markup|copy-theme> builds a new theme from existing ones in the same way.
  A concrete theme then overrides the prefixed variables; for instance
  <source-link|themes/dark/dark-scene.ts|TeXmacs/packages/themes/dark/dark-scene.ts> starts with
  <inactive*|<copy-theme|dark-scene|all-colors>> and sets
  <verbatim|dark-scene-keyword-color> to <verbatim|#d070f0>,
  <verbatim|dark-scene-comment-color> to <verbatim|#d06030>, and so on.
  Wherever the theme is applied (through <markup|with-dark-scene>,
  <markup|apply-theme> or <markup|select-theme>), code typeset by the
  environment variable based highlighters automatically gets colors which
  are readable on a dark background.

  To make a highlighter theme-aware, it is therefore enough to return
  variable names from <cpp|get_color>. Conversely, any new environment
  variable used in this way should be added to the
  <markup|highlight-colors> theme and to the dark themes.

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
