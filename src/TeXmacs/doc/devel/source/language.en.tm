<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Languages, hyphenation and spell checking>

  <section|Introduction>

  <TeXmacs> needs to know about natural languages for four rather different
  purposes:

  <\itemize>
    <item>the <em|typesetter> must cut strings into words, decide where
    spaces may be stretched or broken, apply language specific spacing rules
    (such as the thin spaces before <verbatim|;> and <verbatim|?> in French)
    and hyphenate words at the end of lines;

    <item>the <em|spell checker> must find misspelled words in a document
    and propose corrections, and the <em|grammar checker> (based on the
    external <name|LanguageTool> server) must find and correct grammatical
    errors;

    <item>the <em|user interface> must be translated into the language of
    the user, and so must the parts of documents which are generated
    automatically (\PChapter\Q, \PTheorem\Q, \PTable of contents\Q, ...);

    <item>dates and other locale dependent data must be formatted.
  </itemize>

  This chapter describes the <c++> classes and the <scheme> code behind
  these tasks. Two closely related subjects are described elsewhere: the
  mathematical \Planguages\Q, which assign operator types to mathematical
  symbols, are part of <hlink|mathematical typesetting|maths.en.tm>, and
  the programming languages, which are mainly used for syntax highlighting,
  are the subject of <hlink|syntax highlighting and programming
  languages|syntax-highlighting.en.tm>. The line breaking algorithm which
  consumes the hyphenation points is described in <hlink|the typesetting
  algorithm|typesetter-lines.en.tm>.

  All <c++> file names are relative to <verbatim|src/src/>, and all
  <scheme> file names to <verbatim|src/TeXmacs/progs/>, unless stated
  otherwise.

  <section|Overview>

  The central abstraction is the class <cpp|language_rep>
  (<source-link|System/Language/language.hpp|src/System/Language/language.hpp>). A <em|language> is a named
  <em|resource>: an object which is created once per name and then found
  again by name. There are three families of languages, selected by the
  current mode of the typesetter:

  <\description>
    <item*|Text languages><cpp|text_language (name)> returns the language
    for the value of the <verbatim|language> environment variable
    (<verbatim|english>, <verbatim|french>, <verbatim|chinese>, ...). It
    knows how to split strings into words, spaces and punctuation, and how
    to hyphenate words using <TeX> hyphenation patterns.

    <item*|Mathematical languages><cpp|math_language (name)> for the
    <verbatim|math-language> variable, which assigns operator types,
    priorities and spacing to mathematical symbols.

    <item*|Programming languages><cpp|prog_language (name)> for the
    <verbatim|prog-language> variable, which mainly drives syntax
    highlighting.
  </description>

  Spell checking is <em|not> part of the language objects: it is a separate
  layer of free functions in <source-link|language.cpp|src/System/Language/language.cpp> which forwards to one
  of three external engines (the <name|Aspell> library, a <name|Hunspell>
  or <name|Aspell> subprocess, or the <name|macOS> spell service) and caches
  the results. On top of it, <source-link|Data/Tree/tree_spell.cpp|src/Data/Tree/tree_spell.cpp> finds all
  misspelled words of a tree, and the <scheme> modules in
  <source-link|generic/spell-widgets.scm|TeXmacs/progs/generic/spell-widgets.scm> and <verbatim|tools/spell/> implement
  the spell checking and grammar checking tools of the user interface.

  Translation of the user interface is done by <em|dictionaries>
  (<source-link|System/Language/dictionary.cpp|src/System/Language/dictionary.cpp>), which are also resources,
  loaded from <scheme> files of pairs of strings in
  <verbatim|src/TeXmacs/langs/natural/dic/>.

  <section|Source files>

  <\description-paragraphs>
    <item*|<source-link|System/Language/language.hpp|src/System/Language/language.hpp>,
    <source-link|language.cpp|src/System/Language/language.cpp>>The classes <cpp|text_property_rep> and
    <cpp|language_rep>, the predefined text properties, the table of
    successions of mathematical operators, the wrappers
    <cpp|hyphenless_language> and <cpp|ad_hoc_language>, the colors used
    for syntax highlighting, and the spell checking front end with its
    cache.

    <item*|<source-link|System/Language/impl_language.hpp|src/System/Language/impl_language.hpp>>Declarations shared
    by the implementations: the predefined <cpp|tp_..._rep> properties and
    the classes of the verbatim and programming languages.

    <item*|<source-link|System/Language/text_language.cpp|src/System/Language/text_language.cpp>>The text languages:
    the default Western implementation, French typography, the languages
    written with <name|Unicode> entities (Cyrillic), and the oriental
    languages (Chinese, Japanese, Korean, Taiwanese); the list of supported
    languages and the factory <cpp|text_language>.

    <item*|<source-link|System/Language/verb_language.cpp|src/System/Language/verb_language.cpp>>The
    <verbatim|verbatim> language, used for unknown languages and for
    source code without a specific language.

    <item*|<source-link|System/Language/hyphenate.hpp|src/System/Language/hyphenate.hpp>,
    <source-link|hyphenate.cpp|src/System/Language/hyphenate.cpp>>Loading of <TeX> hyphenation pattern files
    and Liang's hyphenation algorithm.

    <item*|<source-link|System/Language/dictionary.hpp|src/System/Language/dictionary.hpp>,
    <source-link|dictionary.cpp|src/System/Language/dictionary.cpp>>Translation dictionaries, the input and
    output languages, and the translation of strings and trees.

    <item*|<source-link|System/Language/locale.hpp|src/System/Language/locale.hpp>,
    <source-link|locale.cpp|src/System/Language/locale.cpp>>Conversions between <TeXmacs> language names and
    system locales, detection of the language of the user, and formatting
    of dates.

    <item*|<source-link|System/Language/math_language.cpp|src/System/Language/math_language.cpp>,
    <source-link|prog_language.cpp|src/System/Language/prog_language.cpp>, <verbatim|*_language.cpp>,
    <verbatim|packrat_*.cpp>>Mathematical and programming languages and
    the packrat parser; see the chapters mentioned in the introduction.

    <item*|<source-link|Plugins/Ispell/ispell.hpp|src/Plugins/Ispell/ispell.hpp>,
    <source-link|ispell.cpp|src/Plugins/Ispell/ispell.cpp>, <source-link|ispell_exe.cpp|src/Plugins/Ispell/ispell_exe.cpp>>The two
    implementations of the low level spell checking interface: the
    <name|Aspell> library (when <verbatim|USE_ASPELL> is set) and a pipe to
    a <name|Hunspell> or <name|Aspell> process.

    <item*|<source-link|Plugins/MacOS/mac_spellservice.mm|src/Plugins/MacOS/mac_spellservice.mm>>The same interface
    on top of the <name|macOS> spell checker, used instead of the above when
    <verbatim|MACOSX_EXTENSIONS> is defined.

    <item*|<source-link|Data/Tree/tree_spell.cpp|src/Data/Tree/tree_spell.cpp>>Spell checking of whole
    trees, with a cache.

    <item*|<source-link|Edit/Replace/edit_spell.cpp|src/Edit/Replace/edit_spell.cpp>>The historical, key
    driven spell checking mode of the editor.

    <item*|<source-link|Data/Convert/AI/lantool.cpp|src/Data/Convert/AI/lantool.cpp>,
    <source-link|compress.cpp|src/Data/Convert/AI/compress.cpp>>Merging the answers of <name|LanguageTool>
    into documents, and the compressed <name|HTML> representation of
    documents which is sent to <name|LanguageTool> (and to AI models).

    <item*|<source-link|generic/spell-widgets.scm|TeXmacs/progs/generic/spell-widgets.scm>>The spell checking tool and
    toolbar.

    <item*|<verbatim|tools/spell/>>Grammar checking: the interface with
    <name|LanguageTool> (<source-link|spell-lantool.scm|TeXmacs/progs/tools/spell/spell-lantool.scm>), the editing of
    <markup|spell-error> markup and personal dictionaries
    (<source-link|spell-edit.scm|TeXmacs/progs/tools/spell/spell-edit.scm>), the correction tool and toolbar
    (<source-link|correct-widgets.scm|TeXmacs/progs/tools/spell/correct-widgets.scm>) and the keyboard bindings
    (<source-link|spell-kbd.scm|TeXmacs/progs/tools/spell/spell-kbd.scm>).

    <item*|<verbatim|src/plugins/languagetool/>>The <verbatim|languagetool>
    plug-in, which only declares the preferences for the server.

    <item*|<source-link|language/natural.scm|TeXmacs/progs/language/natural.scm>,
    <source-link|utils/misc/translation-list.scm|TeXmacs/progs/utils/misc/translation-list.scm>>Tools for maintaining the
    translation dictionaries.

    <item*|<verbatim|src/TeXmacs/langs/natural/>>The data:
    <verbatim|hyphen/> (hyphenation patterns), <verbatim|dic/>
    (translations) and <verbatim|miss/> (lists of missing translations).

    <item*|<verbatim|src/TeXmacs/packages/customize/language/>>One style
    package per language, which sets the <verbatim|language> variable and
    language specific typography.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Language objects and the typesetter|language-objects.en.tm>

    <branch|Hyphenation|language-hyphenation.en.tm>

    <branch|Spell checking and grammar checking|language-spell.en.tm>

    <branch|Translation of the interface and locales|language-translation.en.tm>
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
