<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Converters between encodings>

  <section|Table driven converters>

  <\explain>
    <cpp|struct converter_rep><explain-synopsis|a dictionary applied by
    longest match>
  <|explain>
    Declared in <source-link|Data/String/converter.hpp|src/Data/String/converter.hpp>. A converter is a
    <cpp|RESOURCE>: it is created once per pair of encodings by
    <cpp|load_converter (from, to)> and cached under the name
    <verbatim|<em|from>-<em|to>> for the rest of the session. Its fields
    are the prefix tree <cpp|ht> (a <cpp|hashtree\<less\>char,string\<gtr\>>
    mapping byte sequences to their translation), an output buffer, the
    names of the two encodings and the flag <cpp|copy_unmatched>, which is
    always true.

    <cpp|apply (c, s)> walks through <cpp|s>; at each position it follows
    the prefix tree as far as possible and replaces the <em|longest>
    matching key by its value. A byte which starts no key is copied
    unchanged. <cpp|c \<less\>\<less\> s> and <cpp|flush (c)> do the same
    incrementally.
  </explain>

  The tables live in <verbatim|$TEXMACS_PATH/langs/encoding/>. Each file
  is a list of pairs <verbatim|("<em|key>" "<em|value>")>, read by
  <cpp|hashtree_from_dictionary (dic, file, key_escape, val_escape,
  reverse)>. The two <cpp|escape_type> arguments say how to read keys and
  values:

  <\description>
    <item*|<cpp|BIT2BIT>><verbatim|#><em|hh> denotes the byte with that
    hexadecimal value (used for Cork and <name|T2A> bytes and for the
    universal symbols, which are written literally).

    <item*|<cpp|UTF8>><verbatim|#><em|hhhh> denotes the <name|UTF-8>
    encoding of that code point.

    <item*|<cpp|CHAR_ENTITY>>The string contains numeric character
    entities <verbatim|&#><em|nnn><verbatim|;> or
    <verbatim|&#x><em|hh><verbatim|;>, which are decoded to
    <name|UTF-8>.

    <item*|<cpp|ENTITY_NAME>>The string is an entity name, which is
    wrapped into <verbatim|&><em|name><verbatim|;>.
  </description>

  With <cpp|reverse> set, the second column is used as key. Several files
  are usually loaded into the same tree, and a later entry for the same key
  overwrites an earlier one; this is how, for instance,
  <source-link|tmuniversaltounicode.scm|TeXmacs/langs/encoding/tmuniversaltounicode.scm> overrides the treatment of
  <verbatim|\<less\>less\<gtr\>> and <verbatim|\<less\>gtr\<gtr\>>. Files
  with <verbatim|oneway> in their name contain mappings which are only
  valid in one direction (for instance several Unicode spaces which all
  become a plain space in Cork).

  <section|The available conversions>

  <cpp|converter_rep::load> knows the following pairs. For each, the table
  files are listed in loading order.

  <\description>
    <item*|<verbatim|Cork> to <verbatim|UTF-8>><verbatim|corktounicode>,
    <verbatim|cork-unicode-oneway>, <verbatim|tmuniversaltounicode>,
    <verbatim|symbol-unicode-oneway>, <verbatim|symbol-unicode-fallback>,
    <verbatim|symbol-unicode-math>. <verbatim|Strict-Cork> to
    <verbatim|UTF-8> is the same without the fallback table, and
    <verbatim|T2A> to <verbatim|UTF-8> adds <verbatim|t2atounicode>.

    <item*|<verbatim|UTF-8> to <verbatim|Cork>>The reverse of
    <verbatim|corktounicode>, <verbatim|tmuniversaltounicode> and
    <verbatim|unicode-symbol-oneway>, plus <verbatim|unicode-cork-oneway>;
    <verbatim|UTF-8> to <verbatim|T2A> adds <verbatim|t2atounicode>.

    <item*|<verbatim|SourceCode>>Like Cork to and from <name|UTF-8> (but
    without <verbatim|cork-unicode-oneway> in the direction of
    <name|UTF-8>), with <verbatim|cork-to-real-ascii> on top,
    which keeps the backquote and <verbatim|...> as they are instead of
    turning them into typographic characters.

    <item*|<verbatim|HTML>>Named entities from <verbatim|HTMLlat1>,
    <verbatim|HTMLspecial> and <verbatim|HTMLsymbol>, in both directions.

    <item*|<verbatim|LaTeX>><verbatim|utf8tolatex>,
    <verbatim|utf8tolatex-onedir> and <verbatim|utf8tolatex-back>.

    <item*|<verbatim|Cork> to <verbatim|ASCII>><verbatim|cork-escaped-to-ascii>,
    which replaces every byte outside printable <abbr|ASCII> by an escape
    <verbatim|\\x<em|hh>>; it is used to embed <TeXmacs> code in
    <name|SVG> images (<source-link|convert/images/tmimage.scm|TeXmacs/progs/convert/images/tmimage.scm>).

    <item*|<verbatim|T2A.CY> and <verbatim|CODEPOINT>>Conversions between
    the Cyrillic part of <name|T2A> and Unicode escapes, used when
    upgrading old documents (<source-link|Data/Convert/Texmacs/upgradetm.cpp|src/Data/Convert/Texmacs/upgradetm.cpp>)
    and by smart fonts (<source-link|Graphics/Fonts/smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>).
  </description>

  On top of the converters, <source-link|converter.cpp|src/Data/String/converter.cpp> defines the functions
  used by the rest of the program:

  <\description-paragraphs>
    <item*|<cpp|utf8_to_cork (s)>>Decodes <cpp|s> one code point at a time
    and converts each separately. A code point of at least 256 without a
    table entry becomes <verbatim|\<less\>#<em|hex>\<gtr\>>; one below 256
    without a table entry is copied unchanged. <cpp|var_utf8_to_cork> in
    addition turns the typographic quotes
    <verbatim|U+2018>--<verbatim|U+201D> into escapes;
    <cpp|sourcecode_to_cork> and <cpp|utf8_to_t2a> are the analogues for
    the other targets. <scheme>: <scm|utf8-\<gtr\>cork>,
    <scm|sourcecode-\<gtr\>cork>, <scm|utf8-\<gtr\>t2a>.

    <item*|<cpp|cork_to_utf8 (s)>>Handles <verbatim|\<less\>#<em|hex>\<gtr\>>
    escapes directly (by <cpp|encode_as_utf8>) and applies the converter to
    the text in between. <cpp|strict_cork_to_utf8>,
    <cpp|cork_to_sourcecode> and <cpp|t2a_to_utf8> are the analogues.
    <scheme>: <scm|cork-\<gtr\>utf8>, <scm|cork-\<gtr\>sourcecode>,
    <scm|t2a-\<gtr\>utf8>.

    <item*|<cpp|utf8_to_html (s)>, <cpp|html_to_utf8 (s)>>Named entities,
    plus hexadecimal entities: <cpp|utf8_to_html> turns <em|every>
    non-<abbr|ASCII> character into <verbatim|&#x<em|hhhh>;>, but
    <cpp|html_to_utf8> only decodes hexadecimal entities between
    <verbatim|0x80> and <verbatim|0xFF> (<scm|utf8-\<gtr\>html>,
    <scm|html-\<gtr\>utf8>). The <name|XML> and <name|HTML> parser
    decodes numeric entities itself, with <cpp|convert_char_entity>
    (<source-link|Data/Convert/Xml/parsexml.cpp|src/Data/Convert/Xml/parsexml.cpp>; see <hlink|the <LaTeX> and
    <name|HTML> converters|convert.en.tm>).

    <item*|<cpp|convert_utf8_to_LaTeX>,
    <cpp|convert_LaTeX_to_utf8>>Character level conversions between
    <name|UTF-8> and <LaTeX> commands. <cpp|convert_utf8_to_LaTeX> prints a
    warning for characters it cannot translate.

    <item*|<cpp|cork_to_ascii (s)>>See above (<scm|escape-to-ascii>).

    <item*|<cpp|convert (s, from, to)>, <cpp|convert_to_cork>,
    <cpp|convert_from_cork>, <cpp|check_encoding>>The general entry points.
    Conversions involving Cork, <name|UTF-8>, <verbatim|SourceCode> and
    <LaTeX> use the tables; anything else goes through <name|UTF-8> and
    <name|iconv> (<cpp|convert_using_iconv>, <cpp|check_using_iconv>). In
    builds without <verbatim|USE_ICONV>, unknown conversions return their
    input unchanged and <cpp|check_encoding> returns true.
  </description-paragraphs>

  The <name|iconv> wrapper converts in one pass with a growing output
  buffer. On an invalid or incomplete input sequence it prints an error
  and returns the <em|whole input unchanged>, so callers cannot tell a
  failed conversion from an identity conversion except by the message.

  <section|Low level <name|UTF-8> routines>

  <\description-paragraphs>
    <item*|<cpp|encode_as_utf8 (code)>>The <name|UTF-8> bytes of a code
    point (up to <verbatim|0x1FFFFF>; larger values give the empty
    string).

    <item*|<cpp|decode_from_utf8 (s, i)>>Decodes the code point at
    position <cpp|i> and advances <cpp|i>. A byte which does not start a
    valid sequence, or a sequence with a bad continuation byte, is returned
    as a single byte value; this \Pfailsafe\Q is what lets <cpp|utf8_to_cork>
    pass bytes of other encodings through unchanged.

    <item*|<cpp|utf8_to_hex_entities>,
    <cpp|hex_entities_to_utf8>>See <cpp|utf8_to_html> above.

    <item*|<cpp|utf8_to_pdf_hex_string (s)>>Converts a Cork string to the
    <name|UTF-16BE> hexadecimal form <verbatim|\<less\>FEFF...\<gtr\>> used
    for text strings in <name|PDF> files (outline entries and document
    metadata, in <source-link|Plugins/Pdf/pdf_hummus_renderer.cpp|src/Plugins/Pdf/pdf_hummus_renderer.cpp> and
    <source-link|Graphics/Renderer/printer.cpp|src/Graphics/Renderer/printer.cpp>).

    <item*|<cpp|convert_escapes>, <cpp|convert_char_entities>,
    <cpp|convert_char_entity>, <cpp|hex_digit_to_int>>Helpers for reading
    the tables.
  </description-paragraphs>

  <section|Guessing the encoding of western text>

  <source-link|Data/String/wencoding.cpp|src/Data/String/wencoding.cpp> decides how to interpret text of
  unknown origin (plain text files, clipboard contents, output of external
  programs):

  <\description>
    <item*|<cpp|guess_wencoding (s)>>Returns <verbatim|"ASCII"> if all
    bytes are printable <abbr|ASCII> or common control characters,
    <verbatim|"UTF-8-BOM"> or <verbatim|"UTF-8"> if the text decodes as
    <name|UTF-8> (with or without a byte order mark),
    <verbatim|"ISO-8859"> if it only contains bytes allowed in the
    <name|ISO-8859> family, and <verbatim|"other"> otherwise
    (<scm|guess-wencoding>).

    <item*|<cpp|western_to_cork (s)>>Converts according to the guess.
    <name|ISO-8859> text is converted from the character set associated
    with the language of the current locale
    (<cpp|language_to_local_ISO_charset>). If no conversion applies, the
    string is kept as it is if it already looks like a universal string
    (<cpp|looks_universal>), and otherwise passed through
    <cpp|tm_encode>.

    <item*|<cpp|western_to_utf8 (s)>>The analogue with <name|UTF-8> as
    target.
  </description>

  <section|Other encodings of bytes>

  <\description>
    <item*|<source-link|base64.cpp|src/Data/String/base64.cpp>><cpp|encode_base64> (with a line break
    every 80 output characters) and <cpp|decode_base64>, which ignores
    characters outside the base 64 alphabet and stops at the first
    <verbatim|=>; <scheme>: <scm|encode-base64>, <scm|decode-base64>. In
    <c++> they are used by the conservative <LaTeX> converters to store
    attributes in comments.

    <item*|Hexadecimal numbers><cpp|as_hexadecimal (i)>,
    <cpp|as_hexadecimal (i, len)> (zero padded) and <cpp|from_hexadecimal>
    in <source-link|analyze.cpp|src/Data/String/analyze.cpp> (<scm|integer-\<gtr\>hexadecimal>,
    <scm|integer-\<gtr\>padded-hexadecimal>,
    <scm|hexadecimal-\<gtr\>integer>). <cpp|from_hexadecimal> silently
    ignores characters which are not hexadecimal digits (but still shifts
    the result).

    <item*|Legacy 8-bit encodings><cpp|koi8_to_iso>, <cpp|iso_to_koi8>
    (and the Ukrainian variants), <cpp|il2_to_cork>, <cpp|cork_to_il2> in
    <source-link|analyze.cpp|src/Data/String/analyze.cpp>. <cpp|il2_to_cork> and <cpp|cork_to_il2> are
    used for Czech and other <name|ISO-8859-2> selections
    (<source-link|Edit/Replace/edit_select.cpp|src/Edit/Replace/edit_select.cpp>) and dates
    (<source-link|System/Language/locale.cpp|src/System/Language/locale.cpp>).
  </description>

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
