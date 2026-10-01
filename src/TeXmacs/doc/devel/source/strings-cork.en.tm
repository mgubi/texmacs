<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The universal encoding and universal strings>

  <section|Characters and symbols>

  A string in the <TeXmacs> universal encoding is a sequence of
  <em|characters>, each of which is

  <\itemize>
    <item>a single byte other than <verbatim|\<less\>> and
    <verbatim|\<gtr\>>, interpreted according to the Cork table below;
    or

    <item>a <em|symbol>: a sequence <verbatim|\<less\>><em|name><verbatim|\<gtr\>>,
    where <em|name> is either a symbol name such as <verbatim|alpha>,
    <verbatim|rightarrow> or <verbatim|less>, or <verbatim|#> followed by
    the hexadecimal Unicode code point of the character.
  </itemize>

  The less-than and greater-than signs themselves are written
  <verbatim|\<less\>less\<gtr\>> and <verbatim|\<less\>gtr\<gtr\>>. There
  is no escape for a lone angular bracket, and a string with unbalanced
  brackets is malformed. The routines below do not validate the names of
  symbols: whether <verbatim|\<less\>foo\<gtr\>> means anything is decided
  by the fonts which render it, the converters which translate it and the
  editing code which interprets it (for instance as a mathematical
  operator).

  Unicode escapes are normally written with upper case hexadecimal digits
  and without leading zeros (this is what <cpp|as_hexadecimal> produces,
  for instance <verbatim|\<less\>#20AC\<gtr\>>), but
  <cpp|from_hexadecimal> also accepts lower case digits, and some tables
  (such as the transliteration table) register both spellings. Code points
  below 256 which have a Cork equivalent are represented by the Cork byte
  rather than by an escape; the converters of <hlink|the next
  page|strings-converters.en.tm> take care of this.

  <section|The Cork table>

  The Cork encoding (also called <name|T1>) is the 8-bit font encoding of
  <LaTeX>'s <verbatim|fontenc>. <TeXmacs> uses it for single byte
  characters. Its correspondence with Unicode is given by
  <verbatim|langs/encoding/corktounicode.scm>; in summary:

  <\description>
    <item*|<verbatim|0x00>--<verbatim|0x0C>>Accents used for composing
    (grave, acute, circumflex, tilde, dieresis, double acute, ring, caron,
    breve, macron, dot, cedilla, ogonek).

    <item*|<verbatim|0x0D>--<verbatim|0x17>>Quotes, guillemets, the en and
    em dashes, and a compound word mark (mapped to <verbatim|U+2060>).

    <item*|<verbatim|0x18>--<verbatim|0x1F>>The per mille zero (so that
    <verbatim|%> followed by <verbatim|0x18> is the per mille sign), dotless
    i and j, and the ligatures ff, fi, fl, ffi, ffl.

    <item*|<verbatim|0x20>--<verbatim|0x7F>>Essentially <abbr|ASCII>, with
    three exceptions: <verbatim|0x60> is the typographic opening quote
    (<verbatim|U+2018>), <verbatim|0x7F> is a hyphen (<verbatim|U+2010>),
    and <verbatim|\<less\>> and <verbatim|\<gtr\>> are not used as
    characters (see above).

    <item*|<verbatim|0x80>--<verbatim|0x9F>>Upper case letters of central
    and eastern European languages (<verbatim|0x80> is A with breve,
    <verbatim|0x98> is Y with dieresis, <verbatim|0x9C> is IJ, ...), except
    <verbatim|0x9F>, which is the section sign.

    <item*|<verbatim|0xA0>--<verbatim|0xBF>>The corresponding lower case
    letters, at an offset of <verbatim|0x20>, except <verbatim|0xBD>,
    <verbatim|0xBE> and <verbatim|0xBF>, which are the inverted exclamation
    mark, the inverted question mark and the pound sign.

    <item*|<verbatim|0xC0>--<verbatim|0xFF>>As in <name|ISO-8859-1>, except
    <verbatim|0xD7> (OE instead of the multiplication sign),
    <verbatim|0xF7> (oe instead of the division sign), <verbatim|0xDF>
    (upper case SS) and <verbatim|0xFF> (sharp s instead of y with
    dieresis).
  </description>

  The Cork encoding is therefore <em|not> <name|ISO-8859-1>: only the range
  <verbatim|0xC0>--<verbatim|0xFF> almost coincides, and even there four
  positions differ. Bytes which come from outside <TeXmacs> must always be
  converted.

  The control characters <verbatim|0x09> (tab), <verbatim|0x0A> (line feed)
  and <verbatim|0x0D> (carriage return) are Cork glyphs too (macron, dot
  accent and a low quote). Document trees do not contain them, since
  paragraphs are separate children of <markup|document> nodes, but strings
  of verbatim text do; see the pitfalls in <hlink|string utilities and
  pitfalls|strings-utils.en.tm>.

  <section|Iterating over universal strings>

  The routines of <verbatim|Data/String/analyze.cpp> whose name starts with
  <cpp|tm_> treat a symbol as one character:

  <\description>
    <item*|<cpp|tm_char_forwards (s, pos)>,
    <cpp|tm_char_backwards (s, pos)>>Move <cpp|pos> over one character:
    one byte, or a whole symbol from <verbatim|\<less\>> to
    <verbatim|\<gtr\>>. <cpp|tm_char_next> and <cpp|tm_char_previous>
    return the new position.

    <item*|<cpp|tm_string_length>, <cpp|tm_forward_access (s, k)>,
    <cpp|tm_backward_access (s, k)>>The number of characters and the
    <math|k>-th character from the start or from the end (<scheme>:
    <scm|tmstring-length>, <scm|tmstring-ref>,
    <scm|tmstring-reverse-ref>).

    <item*|<cpp|tm_tokenize>, <cpp|tm_recompose>>Split a string into its
    characters and join them again.

    <item*|<cpp|tm_string_split>>Split a string into two pieces near its
    middle (<scm|tmstring-split>): preferably at the first space from the
    middle on, or at the last space if all spaces are before the middle
    (the space becomes a piece of its own); otherwise at the first
    boundary from the middle on between runs of letters, digits and other
    characters; and otherwise at the first character boundary from the
    middle on.

    <item*|<cpp|tm_search_forwards>, <cpp|tm_search_backwards>>Substring
    search which only matches at character boundaries.

    <item*|<cpp|contains_unicode_char>>Whether the string contains a
    <verbatim|\<less\>#...\<gtr\>> escape.
  </description>

  Conversions between universal strings and plain text which only touch
  the brackets:

  <\description>
    <item*|<cpp|tm_encode (s)>>Replaces <verbatim|\<less\>> and
    <verbatim|\<gtr\>> by <verbatim|\<less\>less\<gtr\>> and
    <verbatim|\<less\>gtr\<gtr\>> and leaves all other bytes unchanged
    (<scm|string-\<gtr\>tmstring>). It does not convert the bytes, so it is
    only correct for <abbr|ASCII> input.

    <item*|<cpp|tm_decode (s)>>The converse (<scm|tmstring-\<gtr\>string>).
    It turns <verbatim|\<less\>less\<gtr\>> and
    <verbatim|\<less\>gtr\<gtr\>> back into brackets, keeps Unicode escapes
    with exactly four hexadecimal digits, and <em|drops> all other
    symbols. A <verbatim|\<less\>> without matching <verbatim|\<gtr\>>
    truncates the result.

    <item*|<cpp|tm_var_encode (s)>>Like <cpp|tm_encode>, but keeps
    <verbatim|\<less\>#...\<gtr\>> escapes.

    <item*|<cpp|tm_correct (s)>>Removes stray <verbatim|\<gtr\>> signs and
    symbols which contain a nested <verbatim|\<less\>>, and drops an
    unterminated symbol at the end.
  </description>

  <section|Case, accents and letters>

  Two families of routines change the case of letters. The routines of
  <verbatim|analyze.cpp> (<cpp|upcase>, <cpp|locase>, <cpp|upcase_first>,
  <cpp|locase_all>, ...; <scheme>: <scm|upcase-all>, <scm|locase-all>, ...)
  work on bytes and only know <abbr|ASCII> and the Cork letters, which they
  recognize with <cpp|is_iso_locase> and <cpp|is_iso_upcase> and shift by
  <verbatim|0x20>.

  The routines of <verbatim|universal.cpp> (<cpp|uni_locase_char>,
  <cpp|uni_upcase_char>, <cpp|uni_locase_first>, <cpp|uni_upcase_first>,
  <cpp|uni_locase_all>, <cpp|uni_Locase_all> (all but the first
  character), <cpp|uni_upcase_all>; <scheme>: <scm|tmstring-upcase-all> and
  friends) work on universal characters. Besides the Cork letters, they
  handle the Unicode escapes for Latin Extended-A and part of Latin
  Extended-B, Greek and Cyrillic, and the symbolic names of Greek letters
  (<verbatim|\<less\>alpha\<gtr\>> and <verbatim|\<less\>Alpha\<gtr\>>;
  <verbatim|\<less\>varepsilon\<gtr\>> and similar variants are mapped to
  the upper case letter).

  The remaining routines of <verbatim|universal.cpp> are mainly used for
  sorting, indexing and bibliographies:

  <\description>
    <item*|<cpp|uni_translit (s)>>Transliteration to <abbr|ASCII>: Cork
    accented letters lose their accents (using the table
    <cpp|Cork_unaccented> of <verbatim|Data/Convert/Tex/parsetex.cpp>), and
    Cyrillic escapes are transliterated following the <name|ICAO> scheme
    (<scm|tmstring-translit>).

    <item*|<cpp|uni_unaccent_char>, <cpp|uni_unaccent_all>,
    <cpp|uni_get_accent_char>, <cpp|get_accented_list>>Remove the accent of
    an accented Latin letter, or return the accent. The unaccenting table
    covers the Cork letters <verbatim|0x80>--<verbatim|0xBF> and the letters
    of <verbatim|U+00C0>--<verbatim|U+00FF> (converted to their Cork form);
    the accent table only covers the latter.

    <item*|<cpp|uni_is_letter (s)>>Whether a character is a letter
    (<scm|tmstring-letter?>).

    <item*|<cpp|uni_before (s1, s2)>>Comparison for sorting
    (<scm|tmstring-before?>): both strings are unaccented and put in lower
    case, then compared bytewise. This is not a locale aware collation.
  </description>

  Some of these tables have errors; see <hlink|string utilities and
  pitfalls|strings-utils.en.tm>.

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
