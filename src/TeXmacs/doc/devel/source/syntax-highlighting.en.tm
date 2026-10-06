<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Programming languages and syntax highlighting>

  <section|Introduction>

  <TeXmacs> can display fragments of computer programs inside documents:
  inline code such as <markup|cpp> or <markup|python>, blocks of code such
  as <markup|cpp-code> or <markup|python-code>, the input fields of
  interactive sessions, and whole source files (a <verbatim|.scm> or
  <verbatim|.py> file opened in <TeXmacs>). In all these cases the text is
  typeset in a special <em|programming mode> (the environment variable
  <src-var|mode> equals <verbatim|prog>), and the programming language is
  given by the environment variable <src-var|prog-language>. While a string
  is typeset in programming mode, a <em|language object> chops it into
  lexical tokens and assigns a color to each of them. This is what users
  perceive as syntax highlighting.

  This chapter describes how this works inside the C++ kernel and the
  <scheme> layer. It covers:

  <\itemize>
    <item>the path from a code environment in a document to colored text
    boxes, through the typesetting environment and the typesetter;

    <item>the language registry, i.e. how the name stored in
    <src-var|prog-language> is mapped to a C++ object of class
    <cpp|language_rep>, and the various implementations of this class;

    <item>the small library of hand-written <em|parsers> in
    <verbatim|Data/Parser/>, the generic language class
    <cpp|prog_language_rep> which is configured from <scheme>, and the
    <scheme> definitions (<scm|parser-feature>) by which a language is
    described;

    <item>the alternative highlighting mechanism based on packrat grammars
    and the <scm|define-language> macro;

    <item>how colors are assigned, how they can be customized through
    preferences and how they are adapted by themes such as the dark theme;

    <item>editing support for code: keyboard modes, automatic brackets,
    bracket highlighting, indentation, copy and paste;

    <item>a complete walk-through for adding a new language, performance
    considerations and known pitfalls.
  </itemize>

  All C++ file names below are relative to <source-link|src/src/|src>, and all
  <scheme> file names are relative to <source-link|src/TeXmacs/progs/|TeXmacs/progs>, unless
  stated otherwise. The language definitions which live in plugins are found
  in the source tree in <source-link|src/plugins/|plugins> (for instance
  <source-link|src/plugins/code/progs/cpp-lang.scm|plugins/code/progs/cpp-lang.scm>); they are installed into
  <verbatim|$TEXMACS_PATH/plugins/> by the build. Style packages are relative
  to <source-link|src/TeXmacs/packages/|TeXmacs/packages>.

  <section|Overview of the data flow>

  The following chain of events turns a block of <name|Python> code in a
  document into colored text.

  <\enumerate>
    <item>The document contains <markup|python-code> whose body is a
    <markup|document>, one string per line. The macro <markup|python-code>
    (<source-link|environment/env-program.ts|TeXmacs/packages/environment/env-program.ts>) expands to <markup|python>, which
    sets <src-var|mode> to <verbatim|prog> and <src-var|prog-language> to
    <verbatim|python>.

    <item>When the typesetter executes this <markup|with>, the environment
    notices that a variable of type <cpp|Env_Mode> or <cpp|Env_Language> was
    modified and calls <cpp|edit_env_rep::update_language>
    (<source-link|Typeset/Env/env_semantics.cpp|src/Typeset/Env/env_semantics.cpp>). In programming mode this
    sets <cpp|env-\<gtr\>lan= prog_language ("python")>.

    <item><cpp|prog_language> (<source-link|System/Language/prog_language.cpp|src/System/Language/prog_language.cpp>)
    looks up the language object in a global cache. The first time, it
    creates a <cpp|prog_language_rep>, whose constructor loads the <scheme>
    module <verbatim|(python-lang)> and asks it, through
    <scm|parser-feature>, for keywords, operators, number and string syntax
    and comment delimiters.

    <item>Each line of the program is an atomic tree. The concatenator
    (<source-link|Typeset/Concat/concater.cpp|src/Typeset/Concat/concater.cpp>) sees that <cpp|env-\<gtr\>mode
    == 3> and calls <cpp|concater_rep::typeset_prog_string>
    (<source-link|Typeset/Concat/concat_text.cpp|src/Typeset/Concat/concat_text.cpp>).

    <item><cpp|typeset_prog_string> repeatedly calls
    <cpp|env-\<gtr\>lan-\<gtr\>advance (t, pos)>, which advances
    <cpp|pos> over one token, and then
    <cpp|env-\<gtr\>lan-\<gtr\>get_color (t, start, pos)>, which returns a
    color specification for the token.

    <item><cpp|concater_rep::typeset_colored_substring> turns this
    specification into a <cpp|color>: if it names an environment variable
    (such as <verbatim|keyword-color>) the value of that variable is used,
    otherwise the string is interpreted as a color name. A text box with
    this color is appended to the line.
  </enumerate>

  Editing commands (indentation, bracket handling, copy and paste) do not use
  the language object; they are implemented in <scheme> in the directory
  <source-link|prog/|TeXmacs/progs/prog> and dispatched on <src-var|prog-language> through the
  mode predicates <scm|in-prog-python?>, <scm|in-prog-cpp?>, and so on.

  <section|Main source files>

  <\description-paragraphs>
    <item*|<source-link|System/Language/language.hpp|src/System/Language/language.hpp>,
    <source-link|language.cpp|src/System/Language/language.cpp>>The abstract class <cpp|language_rep>, text
    properties, the registry functions and the encoding and decoding of
    syntax colors.

    <item*|<source-link|System/Language/impl_language.hpp|src/System/Language/impl_language.hpp>,
    <source-link|impl_language.cpp|src/System/Language/impl_language.cpp>>Declarations of the concrete language
    classes for programming languages, and shared helpers for multi-line
    comments.

    <item*|<source-link|System/Language/prog_language.cpp|src/System/Language/prog_language.cpp>>The generic
    <cpp|prog_language_rep> configured from <scheme>, and the dispatcher
    <cpp|prog_language>.

    <item*|<source-link|System/Language/scheme_language.cpp|src/System/Language/scheme_language.cpp>,
    <source-link|cpp_language.cpp|src/System/Language/cpp_language.cpp>, <source-link|mathemagix_language.cpp|src/System/Language/mathemagix_language.cpp>,
    <source-link|r_language.cpp|src/System/Language/r_language.cpp>, <source-link|scilab_language.cpp|src/System/Language/scilab_language.cpp>,
    <source-link|fortran_language.cpp|src/System/Language/fortran_language.cpp>>Hand-written highlighters for specific
    languages.

    <item*|<source-link|System/Language/verb_language.cpp|src/System/Language/verb_language.cpp>>The fallback
    language, which also implements highlighting through packrat grammars.

    <item*|<verbatim|Data/Parser/>>Small reusable parsers: blanks,
    identifiers, keywords, operators, numbers, strings, escaped characters,
    inline comments and preprocessor directives.

    <item*|<source-link|Typeset/Concat/concat_text.cpp|src/Typeset/Concat/concat_text.cpp>>The typesetting of
    strings in programming mode (<cpp|typeset_prog_string>).

    <item*|<source-link|kernel/texmacs/tm-language.scm|TeXmacs/progs/kernel/texmacs/tm-language.scm>>The
    <scm|define-language> macro for packrat grammars.

    <item*|<source-link|prog/default-lang.scm|TeXmacs/progs/prog/default-lang.scm> and the
    <verbatim|*-lang.scm> files>The default and the per-language
    <scm|parser-feature> definitions.

    <item*|<source-link|prog/prog-edit.scm|TeXmacs/progs/prog/prog-edit.scm>, <source-link|prog/prog-kbd.scm|TeXmacs/progs/prog/prog-kbd.scm>,
    <verbatim|prog/*-edit.scm>>Editing support for code.

    <item*|<source-link|environment/env-program.ts|TeXmacs/packages/environment/env-program.ts>>The markup for inline code
    and blocks of code.

    <item*|<source-link|themes/base/base-colors.ts|TeXmacs/packages/themes/base/base-colors.ts>,
    <source-link|themes/dark/dark-scene.ts|TeXmacs/packages/themes/dark/dark-scene.ts>>The theme for highlighting colors.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|From a code environment to colored
    text|syntax-highlighting-pipeline.en.tm>

    <branch|Parsers and the language definition
    interface|syntax-highlighting-parsers.en.tm>

    <branch|Colors, preferences and themes|syntax-highlighting-colors.en.tm>

    <branch|Editing support for code|syntax-highlighting-editing.en.tm>

    <branch|Adding a language, performance and
    pitfalls|syntax-highlighting-howto.en.tm>
  </traverse>

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
