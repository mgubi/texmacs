<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Strings, characters and encodings>

  <section|Introduction>

  Inside <TeXmacs>, text is a sequence of bytes held in the <c++> class
  <cpp|string> (see <hlink|basic data types|types.en.tm>). The bytes are
  not interpreted as <name|UTF-8>: the labels of atomic trees, the strings
  manipulated by the editor and most strings passed to <scheme> are in the
  <em|<TeXmacs> universal encoding>, which combines the 8-bit <em|Cork>
  font encoding of <TeX> with symbolic names such as
  <verbatim|\<less\>alpha\<gtr\>> and numeric escapes such as
  <verbatim|\<less\>#20AC\<gtr\>>. Conversions to and from <name|UTF-8>,
  <name|HTML> entities, <LaTeX>, the <name|T2A> Cyrillic encoding or the
  local character set of the system happen at the boundaries of the
  program: keyboard input, the clipboard, file import and export, widgets,
  fonts and external programs.

  This chapter describes the encoding itself, the routines which analyze
  and convert strings, and where the conversions take place. The
  serialization of documents in <verbatim|.tm> files is described in
  <hlink|default serialization|../format/basics/tm-tm.en.tm>; how fonts
  render universal symbols is explained in <hlink|<TeXmacs>
  fonts|fonts.en.tm> and <hlink|smart fonts|smart-fonts.en.tm>.

  <section|Overview>

  <\description>
    <item*|The universal encoding>A <em|character> is either a single byte
    (an <abbr|ASCII> character or a Cork glyph such as <verbatim|\\351>
    for an e with acute accent) or a <em|symbol> between angular brackets:
    a name (<verbatim|\<less\>alpha\<gtr\>>, <verbatim|\<less\>leq\<gtr\>>,
    <verbatim|\<less\>less\<gtr\>> for the less-than sign itself) or a
    Unicode code point in hexadecimal (<verbatim|\<less\>#3B1\<gtr\>>).
    Routines whose name starts with <cpp|tm_> or <cpp|uni_> work on such
    characters rather than on bytes.

    <item*|Converters>Conversions between encodings are table driven: the
    tables in <verbatim|$TEXMACS_PATH/langs/encoding/> are loaded into
    prefix trees and applied by longest match. Encodings that <TeXmacs>
    does not know itself are handled by <name|iconv>, when it is
    available.

    <item*|Boundaries>Each subsystem that talks to the outside world is
    responsible for its own conversion: the <name|Qt> layer converts key
    presses and widget strings, the converters convert imported and
    exported documents, and fonts map universal characters to glyphs.
    File names and system commands stay in the encoding of the operating
    system (in practice <name|UTF-8>).
  </description>

  <section|Source files>

  <\description>
    <item*|<verbatim|Kernel/Types/string.hpp>,
    <verbatim|string.cpp>>The byte string class; see <hlink|basic data
    types|types.en.tm>.

    <item*|<verbatim|Data/String/analyze.hpp>,
    <verbatim|analyze.cpp>>Character tests and case changes on Cork bytes,
    the <cpp|tm_*> routines on universal characters, quoting and escaping,
    parsing helpers, search and replace, Roman numbers and hexadecimal
    numbers, completions, and a few special purpose conversions (<name|KOI8>,
    <name|ISO-8859-2>, <verbatim|ispanish>, <verbatim|igerman>).

    <item*|<verbatim|Data/String/universal.hpp>,
    <verbatim|universal.cpp>>Case changes, transliteration, removal of
    accents, letter tests and sorting for universal strings
    (<cpp|uni_*>).

    <item*|<verbatim|Data/String/converter.hpp>,
    <verbatim|converter.cpp>>The table driven <cpp|converter> class, the
    conversion functions between Cork, <name|UTF-8>, <name|HTML>, <LaTeX>,
    <name|T2A> and <verbatim|SourceCode>, the <name|iconv> wrapper and the
    <name|UTF-8> encoding and decoding primitives.

    <item*|<verbatim|Data/String/wencoding.hpp>,
    <verbatim|wencoding.cpp>>Heuristics which guess the encoding of
    western text (<cpp|guess_wencoding>, <cpp|western_to_cork>).

    <item*|<verbatim|Data/String/base64.hpp>,
    <verbatim|base64.cpp>>Base 64 encoding and decoding.

    <item*|<verbatim|Data/String/fast_search.hpp>,
    <verbatim|fast_search.cpp>>Indexed substring search and longest common
    substrings, used by the conservative <LaTeX> converters.

    <item*|<verbatim|Data/String/merge_sort.hpp>>A generic merge sort on
    arrays.

    <item*|<verbatim|$TEXMACS_PATH/langs/encoding/*.scm>>The conversion
    tables.

    <item*|<verbatim|Plugins/Qt/qt_utilities.cpp>,
    <verbatim|QTMKeyboardEvent.cpp>, <verbatim|qt_gui.cpp>>Conversions for
    widgets, key presses and the clipboard.

    <item*|<verbatim|Data/Convert/Verbatim/verbatim.cpp>>Encodings of
    plain text import and export.

    <item*|<verbatim|Scheme/Glue/build-glue-basic.scm>>The <scheme> glue
    for the routines above (<scm|utf8-\<gtr\>cork>,
    <scm|tmstring-length>, <scm|tmstring-upcase-all>, ...).
  </description>

  <section|Contents of this chapter>

  <\traverse>
    <branch|The universal encoding and universal strings|strings-cork.en.tm>

    <branch|Converters between encodings|strings-converters.en.tm>

    <branch|Where text is converted|strings-io.en.tm>

    <branch|String utilities and pitfalls|strings-utils.en.tm>
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
