<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Language objects and the typesetter>

  <section|The class <cpp|language_rep>>

  <\explain>
    <cpp|struct language_rep: rep\<less\>language\<gtr\>><explain-synopsis|a
    language>
  <|explain>
    Declared in <verbatim|System/Language/language.hpp>. The macro
    <cpp|RESOURCE(language)> makes <cpp|language> a handle to a named
    resource: the constructor <cpp|language_rep (name)> registers the new
    object in the table <cpp|language::instances>, and <cpp|language
    (name)> retrieves it later. Language objects are therefore created once
    and live for the rest of the session; the factories below always first
    look for an existing instance.

    The fields are the name <cpp|lan_name>, the abbreviation <cpp|hl_lan>
    of the packrat grammar used for syntax highlighting (0 if there is
    none), and a table <cpp|color_decoding> of highlighting colors. The
    pure virtual methods are:

    <\description-paragraphs>
      <item*|<cpp|text_property advance (tree t, int& pos)>>Given an atomic
      tree <cpp|t> and a position <cpp|pos> in its string, move <cpp|pos>
      to the end of the next lexical unit (a word, a run of spaces, a run
      of punctuation, a number, a symbol <verbatim|\<less\>...\<gtr\>>) and
      return its <em|text property>.

      <item*|<cpp|array\<less\>int\<gtr\> get_hyphens (string s)>>The
      penalties for breaking the word <cpp|s> after each of its
      characters; see <hlink|hyphenation|language-hyphenation.en.tm>.

      <item*|<cpp|void hyphenate (string s, int after, string& l, string&
      r)>>Split <cpp|s> after position <cpp|after>, adding a hyphen to the
      left part.
    </description-paragraphs>

    The virtual methods <cpp|get_group>, <cpp|get_members>,
    <cpp|highlight> and <cpp|get_color> have default implementations; they
    are used by mathematical languages (symbol groups) and by programming
    languages (syntax highlighting).
  </explain>

  <\explain>
    <cpp|struct text_property_rep><explain-synopsis|what the typesetter
    needs to know about a lexical unit>
  <|explain>
    Its fields are

    <\description>
      <item*|<cpp|type>>One of the <verbatim|TP_*> constants: normal text,
      hyphen, the various spaces (thin, normal, double, and their
      non-breaking versions), period, the CJK variants, operator, other.

      <item*|<cpp|spc_before>, <cpp|spc_after>>The kind of space to insert
      before and after the unit (<verbatim|SPC_*> constants), looked up in
      the spacing tables of the current font.

      <item*|<cpp|pen_before>, <cpp|pen_after>>The line breaking penalties
      before and after the unit: 0 means a normal break point,
      <verbatim|HYPH_STD> (10000) a hyphenation point, <verbatim|HYPH_PANIC>
      a break which is only allowed as a last resort and
      <verbatim|HYPH_INVALID> a forbidden break.

      <item*|<cpp|op_type>, <cpp|priority>, <cpp|limits>,
      <cpp|macro>>Only used by mathematical languages.
    </description>

    The text languages do not allocate properties: they return pointers to
    the predefined global objects declared in <verbatim|impl_language.hpp>,
    such as <cpp|tp_normal_rep>, <cpp|tp_space_rep> (a breakable space),
    <cpp|tp_nb_space_rep> (a space before punctuation, which may not be
    broken), <cpp|tp_nb_thin_space_rep>, <cpp|tp_period_rep> (a space after
    a full stop, which may be wider), <cpp|tp_hyph_rep> (a run of dashes,
    after which a line may be broken) and the CJK properties.
  </explain>

  <section|Text languages>

  <cpp|text_language (name)> (<verbatim|text_language.cpp>) maps a
  language name to an implementation and to the name of a hyphenation
  pattern file. There are five implementations:

  <\description>
    <item*|<cpp|text_language_rep>>The default for Western languages
    written in the Cork encoding. Spaces are breakable unless followed by
    punctuation; punctuation followed by a space produces a normal space,
    or a period space after <verbatim|.>, <verbatim|!> and <verbatim|?>;
    runs of <verbatim|-> are hyphens; letters, digits and symbols form
    words.

    <item*|<cpp|french_language_rep>>The same, but with French typography:
    a non-breaking thin space before <verbatim|;>, <verbatim|!>,
    <verbatim|?> and the closing guillemet, and after the opening guillemet
    (the Cork characters <verbatim|\\23> and <verbatim|\\24>).

    <item*|<cpp|ucs_text_language_rep>>For languages whose letters are
    written as <name|Unicode> entities <verbatim|\<less\>#...\<gtr\>> in
    <TeXmacs> strings (Bulgarian, Russian, Ukrainian): entities are treated
    as letters, and the hyphenation patterns are kept in <name|UTF-8>.

    <item*|<cpp|oriental_language_rep>>Chinese, Japanese, Korean and
    Taiwanese. Every character is a potential break point
    (<cpp|tp_cjk_normal_rep>), except before punctuation, which is
    recognized from a fixed table of ASCII and CJK punctuation marks.
    There is no hyphenation: <cpp|get_hyphens> forbids all breaks inside a
    unit.

    <item*|<cpp|verb_language_rep>>The <verbatim|verbatim> language
    (<verbatim|verb_language.cpp>): no hyphenation; breaks are allowed at
    spaces and after the separators <verbatim|- / \\ , ?>. It is also
    returned, with a warning, for unknown language names.
  </description>

  The supported text languages, with their pattern files, are: american
  and english (<verbatim|us>), british (<verbatim|ukenglish>), bulgarian,
  croatian, czech, danish, dutch, esperanto, finnish, french, german,
  greek, hungarian, italian, polish, portuguese, romanian, russian, slovak,
  slovene, spanish, swedish and ukrainian, plus the four oriental
  languages. <cpp|get_supported_languages ()> returns the same list. To add
  a language, add its hyphenation file to
  <verbatim|src/TeXmacs/langs/natural/hyphen/>, a case to
  <cpp|text_language> and <cpp|get_supported_languages>, a style package in
  <verbatim|src/TeXmacs/packages/customize/language/>, an entry to
  <scm|supported-languages> in <verbatim|kernel/texmacs/tm-modes.scm>, the
  locale codes in <verbatim|locale.cpp> and, for the user interface, a
  translation dictionary (see <hlink|translation|language-translation.en.tm>).

  <section|Wrappers>

  Two functions of <verbatim|language.cpp> derive new languages from an
  existing one. Both forward <cpp|advance> to the base language and are
  created once per name.

  <\description>
    <item*|<cpp|hyphenless_language (base)>>Named
    <verbatim|<em|base>-hyphenless>; <cpp|get_hyphens> forbids all breaks.
    It implements the primitive <markup|hgroup>
    (<cpp|concater_rep::typeset_hgroup>, <verbatim|Typeset/Concat/concat_text.cpp>),
    which also forbids breaks between the boxes of its body.

    <item*|<cpp|ad_hoc_language (base, hyphs)>>Uses an explicit
    hyphenation such as <verbatim|"hy-phen-ation"> for the corresponding
    word, and the base language for all other words. Each distinct
    hyphenation gets a number, so the language is named
    <verbatim|<em|base>-<em|n>>. It implements the primitive
    <markup|hyphenate-as> (<cpp|concater_rep::typeset_hyphenate_as>,
    <verbatim|Typeset/Concat/concat_active.cpp>).
  </description>

  <section|How the typesetter uses languages>

  The typesetting environment holds the current language in
  <cpp|edit_env_rep::lan>. It is recomputed by
  <cpp|edit_env_rep::update_language> (<verbatim|Typeset/Env/env_semantics.cpp>)
  whenever the <verbatim|language>, <verbatim|math-language>,
  <verbatim|prog-language> or <verbatim|mode> variable changes:

  <\cpp-code>
    switch (mode) {

    case 0: case 1: lan= text_language (get_string (LANGUAGE)); break;

    case 2: \ \ \ \ \ \ \ \ lan= math_language (get_string (MATH_LANGUAGE)); break;

    case 3: \ \ \ \ \ \ \ \ lan= prog_language (get_string (PROG_LANGUAGE)); break;

    }
  </cpp-code>

  where mode 1 is text, 2 math, 3 prog and 0 any other mode (such as
  <verbatim|src>). The defaults are <verbatim|english>,
  <verbatim|std-math> and <verbatim|scheme>
  (<verbatim|Typeset/Env/env_default.cpp>).

  The concatenation typesetter (<verbatim|Typeset/Concat/concat_text.cpp>)
  calls <cpp|env-\<gtr\>lan-\<gtr\>advance> repeatedly to cut each string
  into units. For each unit it typesets the substring as a text box,
  records the language in the resulting line item, inserts the spaces
  requested by <cpp|spc_before> and <cpp|spc_after>, and sets the break
  penalties from <cpp|pen_before> and <cpp|pen_after>. Later, the line
  breaker (<verbatim|Typeset/Line/line_breaker.cpp>) calls
  <cpp|get_hyphens> and <cpp|hyphenate> on the language stored in the item
  when a word has to be split; see <hlink|line breaking|typesetter-lines.en.tm>.

  At the document level, the language is chosen with a style package: the
  submenu <menu|Document|Language> (shown with detailed menus;
  <scm|set-document-language> in
  <verbatim|generic/document-edit.scm>) adds the package named after the
  language to the style list, or removes it for English. These packages
  (<verbatim|src/TeXmacs/packages/customize/language/>) set the
  <verbatim|language> variable together with language specific typography
  such as the dots used in tables of contents. Inside a document, a
  different language can be set locally by changing the
  <verbatim|language> variable, for instance with a <markup|with>.

  <section|Mathematical and programming languages>

  For completeness: mathematical languages
  (<verbatim|math_language.cpp>) classify symbols by operator type,
  priority and spacing using the packrat grammar
  <verbatim|language/std-math.scm>; their <cpp|advance> returns properties
  with a meaningful <cpp|op_type>, and the table
  <cpp|succession_status_table> (<verbatim|language.cpp>, initialized by
  <cpp|init_succession_status_table> at startup) tells the typesetter
  which spaces to remove between two successive operators. They are
  described in <hlink|mathematical typesetting|maths.en.tm>. Programming
  languages (<verbatim|prog_language.cpp> and the <verbatim|*_language.cpp>
  files) are described in <hlink|syntax highlighting and programming
  languages|syntax-highlighting.en.tm>.

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
