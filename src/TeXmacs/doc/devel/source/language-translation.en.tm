<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Translation of the interface and locales>

  <section|Dictionaries>

  <\explain>
    <cpp|struct dictionary_rep: rep\<less\>dictionary\<gtr\>><explain-synopsis|a
    translation table>
  <|explain>
    Declared in <verbatim|System/Language/dictionary.hpp>. A dictionary
    translates strings from one language <cpp|from> into another
    <cpp|to>; it is a resource named <verbatim|<em|from>-<em|to>>, created
    by <cpp|load_dictionary (from, to)> and kept for the session. Its
    <cpp|table> maps source strings to translations.
  </explain>

  Dictionaries are loaded from the files
  <verbatim|<em|from>-<em|to>.scm> in the search path
  <verbatim|$TEXMACS_DIC_PATH>, which consists of
  <verbatim|$TEXMACS_HOME_PATH/langs/natural/dic>,
  <verbatim|$TEXMACS_PATH/langs/natural/dic> and the
  <verbatim|langs/natural/dic> directories of the plug-ins
  (<verbatim|System/Boot/init_texmacs.cpp>). All matching files are
  loaded, so a plug-in or the user can add entries. Each file is a sequence
  of pairs

  <\scm-code>
    ("Bibtex command" "commande Bibtex")
  </scm-code>

  For some target languages (Chinese, Japanese, Korean, Taiwanese,
  Russian, Ukrainian, Bulgarian, German, Greek, Slovak) the translations
  are stored in <name|UTF-8> and converted to Cork when loaded; for the
  others the files must be in the Cork encoding, which coincides with
  <name|ISO-8859-1> for most accented letters. Only
  <verbatim|english-<em|lan>.scm> files exist; if a dictionary
  <verbatim|<em|lan>-english> is requested and no file exists, it is built
  by inverting <verbatim|english-<em|lan>>.

  <cpp|dictionary_rep::translate (s, guess)> looks for a translation in
  this order:

  <\enumerate>
    <item>the string itself;

    <item>the string with its first letter in lower case (the translation
    then gets an upper case first letter);

    <item>the part before a <verbatim|::> marker: a string such as
    <verbatim|"Ctrl::keyboard"> is translated as <verbatim|"Ctrl">, but
    may have its own entry, which allows to disambiguate homonyms;

    <item>if <cpp|guess> is set, the string is split: leading and trailing
    non letters (such as <verbatim|":">, <verbatim|"..."> or a number) are
    translated separately, with a space before <verbatim|:>, <verbatim|!>
    and <verbatim|?> in French, and otherwise the string is split at its
    last non letter which is not a space;

    <item>failing all this, the string is returned unchanged.
  </enumerate>

  <section|The translation interface>

  The output language is a global variable of <verbatim|dictionary.cpp>,
  set with <cpp|set_output_language> and read with
  <cpp|get_output_language>. At the <scheme> level,
  <scm|set-output-language> is bound to <cpp|gui_set_output_language>
  (<verbatim|Texmacs/Server/tm_server.cpp>), which also refreshes all menus
  and widgets. It is called when the <verbatim|language> preference
  changes (<scm|notify-language> in <verbatim|texmacs/texmacs/tm-server.scm>),
  whose default is the language of the user's locale
  (<scm|get-locale-language>).

  The main functions are

  <\description>
    <item*|<cpp|translate (s)>, <scm|translate>>Translate a string from
    English into the output language.

    <item*|<cpp|translate (s, from, to)>,
    <scm|translate-from-to>>Translate between two given languages.

    <item*|<cpp|translate_as_is>, <scm|string-translate>>The same without
    guessing.

    <item*|<cpp|tree_translate (t)>, <scm|tree-translate>>Translate all
    strings of a tree which are accessible children according to the
    current <abbr|DRD>, except inside <markup|verbatim>. Two tags are
    special: <markup|localize> translates its argument from English into
    the output language, whatever the <cpp|from> and <cpp|to> arguments,
    and <verbatim|(replace <em|pattern> <em|arg1> ...)> translates the
    pattern without guessing and substitutes the translated arguments for
    <verbatim|%1>, <verbatim|%2>, .... The <scheme> function <scm|replace>
    (<verbatim|language/natural.scm>) builds and translates such a tree.

    <item*|<cpp|translate (t)> for a tree>Translate the tree and serialize
    it as a string, as needed for window titles and native menus; keyboard
    shortcuts (<markup|render-key>) are rendered in a GUI dependent way.
  </description>

  Menus and widgets call these functions themselves: every label of a menu
  or widget is a string or tree in English, translated when the widget is
  built.

  Documents use the same dictionaries through the typesetting primitive
  <markup|translate> (<cpp|edit_env_rep::exec_translate>,
  <verbatim|Typeset/Env/env_exec.cpp>). The macro <markup|localize> of
  <verbatim|std-utils.ts> is defined as <verbatim|\<less\>translate\|<em|text>\|english\|\<less\>value\|language\<gtr\>\<gtr\>>,
  so that the automatically generated texts of a document (\PChapter\Q,
  \PTheorem\Q, ...) follow the <em|document> language rather than the
  language of the interface.

  <section|Maintaining the dictionaries>

  Two <scheme> modules help translators:

  <\description>
    <item*|<verbatim|language/natural.scm>>In developer mode, the menu and
    widget macros of <verbatim|kernel/gui/gui-markup.scm> record every label
    in the table <scm|all-translations>. <scm|tr-missing> lists the labels
    seen so far that have no translation, and <scm|tr-rebuild> rewrites the
    dictionary file with the missing entries added. The commands are in the
    translations submenu of the developer menu.

    <item*|<verbatim|utils/misc/translation-list.scm>>A more systematic
    approach: <scm|update-translatable> collects all translatable strings
    from the <scheme> sources into <verbatim|english-new.scm> (minus those
    in <verbatim|english-ignore.scm>), <scm|update-missing> computes, for
    each language, the list of missing translations in
    <verbatim|src/TeXmacs/langs/natural/miss/english-<em|lan>-miss.scm>,
    and <scm|translate-begin> / <scm|translate-end> export these lists for
    an online translation service and merge the results back.
  </description>

  <section|Locales and dates>

  <verbatim|System/Language/locale.cpp> converts between <TeXmacs> language
  names and system locales:

  <\description>
    <item*|<cpp|get_locale_language ()>>The language of the user, from
    <verbatim|LC_ALL>, <verbatim|LC_MESSAGES>, <verbatim|LANG> or
    <verbatim|GDM_LANG>, from the system settings on <name|macOS>, or from
    the user interface language on <name|Windows>; English by default.

    <item*|<cpp|locale_to_language (s)>>From a locale such as
    <verbatim|fr_FR.UTF-8> to <verbatim|french>, using the first two
    letters, with special cases for <verbatim|en_GB> and
    <verbatim|zh_TW>.

    <item*|<cpp|language_to_locale (s)>>The converse, used to choose
    spell checking dictionaries, to build the language parameter sent to
    <name|LanguageTool> and to format dates.

    <item*|<cpp|get_locale_charset ()>>The character set of the system:
    <name|UTF-8> on <name|Windows>, <name|macOS>, <name|Haiku>,
    <name|Android> and with the X11 toolkit, and otherwise the value
    reported by <cpp|nl_langinfo>.

    <item*|<cpp|get_date (lan, fm)>, <cpp|pretty_time>,
    <cpp|pretty_date>>Dates in a given language and format. With <name|Qt>
    they are computed by <name|Qt> (<verbatim|Plugins/Qt/qt_utilities.cpp>);
    otherwise <verbatim|date> is run with the locale of the language.
  </description>

  <section|Pitfalls>

  <\itemize>
    <item>The dictionary loader converts from <name|UTF-8> only for a fixed
    list of target languages (<verbatim|dictionary.cpp:51-55>), which does
    not match the actual encodings of the files: for instance
    <verbatim|english-italian.scm> is in <name|UTF-8> but Italian is not in
    the list, so accented Italian translations are garbled (the translation
    of \Pcell properties\Q comes out with the two <name|UTF-8> bytes of the
    accented letter instead of one Cork character). A new dictionary must be
    written in the encoding the loader expects for its language.

    <item>Some locale codes in <verbatim|locale.cpp> are not valid
    <name|ISO> codes: Greek is mapped from <verbatim|gr> and to
    <verbatim|gr_GR> (lines 118 and 150; the language code is
    <verbatim|el>), Swedish to <verbatim|sv_SV> (line 162; the country is
    <verbatim|SE>) and Esperanto to <verbatim|eo_EO> (line 146). As a
    consequence, Greek users are not recognized from their locale, and spell
    checking dictionaries and <name|LanguageTool> may not be found for these
    languages.

    <item>Without <name|Qt>, <cpp|get_date> converts dates for Czech,
    Hungarian and Polish only if the locale is <verbatim|cz_CZ>,
    <verbatim|hu_HU> or <verbatim|pl_PL> (<verbatim|locale.cpp:330>), but
    Czech is mapped to <verbatim|cs_CZ>, so Czech dates are not converted.

    <item>The language lists are duplicated: <cpp|text_language> and
    <cpp|get_supported_languages> (<c++>), <scm|supported-languages>
    (<verbatim|kernel/texmacs/tm-modes.scm>), the locale tables, the
    hyphenation files, the dictionaries and the style packages must be kept
    consistent by hand. For instance <verbatim|american> is a text language
    but not one of the <scheme> <scm|supported-languages>.
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
