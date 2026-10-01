<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|String utilities and pitfalls>

  <section|Byte level utilities>

  Besides the routines of the previous pages,
  <verbatim|Data/String/analyze.cpp> collects general purpose functions on
  byte strings. Unless stated otherwise they ignore the universal encoding,
  so a symbol counts as several characters and a search may match inside a
  symbol; use the <cpp|tm_*> variants when this matters.

  <\description>
    <item*|Character tests><cpp|is_alpha>, <cpp|is_digit>,
    <cpp|is_hex_digit>, <cpp|is_space>, <cpp|is_punctuation>, ... on
    <abbr|ASCII> characters (inline in <verbatim|analyze.hpp>), and
    <cpp|is_iso_alpha>, <cpp|is_iso_locase>, <cpp|is_iso_upcase> which also
    accept Cork letters.

    <item*|Reading><cpp|test (s, i, what)>, <cpp|starts>, <cpp|ends>,
    <cpp|read (s, i, what)>, <cpp|read_line>, <cpp|read_int>,
    <cpp|read_double>, <cpp|read_word>, <cpp|skip_spaces>,
    <cpp|skip_whitespace>, <cpp|skip_line>, <cpp|skip_symbol>,
    <cpp|is_whitespace>, and <cpp|parse> for binary integers. These are the
    building blocks of the hand written parsers in
    <verbatim|Data/Convert/>.

    <item*|Searching and replacing><cpp|search_forwards>,
    <cpp|search_backwards>, <cpp|occurs>, <cpp|count_occurrences> (which
    counts overlapping occurrences), <cpp|overlapping>, <cpp|replace>,
    <cpp|match_wildcard> (only <verbatim|*> is special),
    <cpp|tokenize> and <cpp|recompose>, <cpp|trim_spaces> and its left and
    right variants (also on trees and arrays), <cpp|find_non_alpha>.
    <scheme>: <scm|string-search-forwards>, <scm|string-replace>,
    <scm|string-count-occurrences>, <scm|cpp-string-tokenize>, ...

    <item*|Quoting><cpp|scm_quote> and <cpp|scm_unquote> (<scheme> string
    syntax with <verbatim|\\\\> and <verbatim|\\"> escapes;
    <scm|string-quote>), <cpp|raw_quote> and <cpp|raw_unquote> (only add or
    remove the double quotes, used to distinguish strings from symbols in
    trees), <cpp|unescape_guile> (<verbatim|\\x<em|hh>> escapes).

    <item*|Escaping for other programs><cpp|escape_sh> (shell; on
    <name|Windows> it only adds double quotes), <cpp|escape_generic> (the
    escapes of the plug-in protocol, see <hlink|the plug-in
    machinery|plugins.en.tm>), <cpp|escape_verbatim> (removes control
    characters and turns tabs and newlines into spaces),
    <cpp|escape_spaces>, <cpp|dos_to_better> (removes carriage returns),
    <cpp|convert_tabs_to_spaces>.

    <item*|Numbers><cpp|roman_nr>, <cpp|Roman_nr>, <cpp|alpha_nr>,
    <cpp|Alpha_nr>, <cpp|fnsymbol_nr> (used for numbering), and the
    hexadecimal routines of <hlink|the previous page|strings-converters.en.tm>.

    <item*|Sets of characters><cpp|string_union> and <cpp|string_minus>
    treat a string as a set of bytes.

    <item*|Completions and differences><cpp|as_completions>,
    <cpp|close_completions>, <cpp|strip_completions> (used by
    tab-completion, <verbatim|Edit/Interface/edit_complete.cpp>);
    <cpp|differences (s1, s2)>, which returns the differing ranges as
    quadruples <math|(b<rsub|1>,e<rsub|1>,b<rsub|2>,e<rsub|2>)> found by
    recursively matching common substrings, and <cpp|distance>, which sums
    the lengths of these ranges (an approximation of an edit distance;
    <scm|string-differences>).

    <item*|Mathematics><cpp|downgrade_math_letters> turns
    <verbatim|\<less\>b-x\<gtr\>>, <verbatim|\<less\>cal-x\<gtr\>> and
    similar into plain letters; <cpp|find_left_bracket> and
    <cpp|find_right_bracket> look for matching brackets in the edit tree.
  </description>

  <paragraph|Indexed search.><cpp|string_searcher>
  (<verbatim|Data/String/fast_search.hpp>) builds, for a fixed string, a
  table of hash codes of all substrings whose length is a power of two, and
  uses it to find the occurrences of a pattern quickly
  (<cpp|search_next>, <cpp|search_all>). <cpp|get_longest_common (s1, s2,
  ...)> uses two such tables to find a longest common substring. Both are
  used by the conservative <LaTeX> converters to recognize the parts of a
  document which did not change.

  <paragraph|Sorting.><verbatim|Data/String/merge_sort.hpp> provides
  <cpp|merge_sort (a)> for arrays of any type with an <cpp|\<less\>=>
  operator and <cpp|merge_sort_leq\<less\>T,LEQ\<gtr\> (a)> with an explicit
  comparison class. The sort is stable.

  <section|Pitfalls>

  <paragraph|Mixing encodings.>Nothing in the type system distinguishes a
  Cork string from a <name|UTF-8> string or a file name. The most common
  bugs are:

  <\itemize>
    <item>Raw <name|UTF-8> in trees: text from the outside (a file, the
    clipboard, an external program, a <scheme> string literal in a
    <name|UTF-8> source file) inserted without <cpp|utf8_to_cork> shows up
    as several wrong Cork characters. <verbatim|.tm> files must not contain
    bytes above 127 other than Cork characters.

    <item>Converting twice, or converting a string which is already
    <name|UTF-8>: <cpp|cork_to_utf8> applied to <name|UTF-8> produces
    mojibake. <cpp|to_qstring> tries to avoid this by guessing, which in
    turn can misinterpret a Cork string that happens to be valid
    <name|UTF-8>.

    <item>Counting bytes instead of characters: <cpp|N (s)>,
    <scm|string-length> and <scm|substring> count bytes, so they split
    symbols; use <cpp|tm_string_length>, <scm|tmstring-length> and the
    other <cpp|tm_*> routines.

    <item>Entities versus characters: the same character may be present as
    a Cork byte, as a symbol name and as a Unicode escape (for instance an
    e with acute accent is the byte <verbatim|0xE9>, not
    <verbatim|\<less\>#E9\<gtr\>>). <cpp|utf8_to_cork> always produces the
    Cork byte when there is one, but strings built by hand may not, and
    comparisons of such strings fail.
  </itemize>

  <paragraph|Line breaks and control characters.>In the Cork table the
  bytes <verbatim|0x09>, <verbatim|0x0A> and <verbatim|0x0D> are glyphs
  (macron, dot accent and a low quote). <cpp|cork_to_utf8> therefore turns
  a tab into <verbatim|U+00AF> and a newline into <verbatim|U+02D9>
  (verified: <scm|(cork-\<gtr\>utf8 "a\\nb")> gives an a, a dot accent and
  a b). Multi-line text must be converted line by line, as
  <cpp|var_cork_to_utf8> in <verbatim|Data/Convert/Verbatim/verbatim.cpp>
  does. In the other direction, <cpp|utf8_to_cork> keeps tabs and newlines
  as bytes, so the two functions are not inverse to each other on such
  strings. Code points <verbatim|U+0080>--<verbatim|U+009F> (the C1
  control characters) have no table entry and are left as raw
  <name|UTF-8> by <cpp|utf8_to_cork> (verified).

  <section|Known bugs>

  The following problems were found while writing this chapter. Items
  marked \Pverified\Q were reproduced with the <scheme> glue; the others
  are from reading the code.

  <\enumerate>
    <item>Wrong case of four Cork characters (verified).
    <cpp|uni_locase_char> lowers the whole range
    <verbatim|0x80>--<verbatim|0x9F>, including the section sign
    <verbatim|0x9F>, which becomes the pound sign;
    <cpp|uni_upcase_char> raises <verbatim|0xA0>--<verbatim|0xBF>,
    including the inverted exclamation and question marks and the pound
    sign, which become an I with dot, a D with stroke and the section sign
    (<verbatim|Data/String/universal.cpp:199>, <verbatim|:250>). This
    affects <scm|tmstring-upcase-all> and friends, and hence
    capitalization in bibliographies and titles. The byte level routines
    of <verbatim|analyze.cpp> exclude these positions correctly.

    <item>D with stroke (<verbatim|0x9E>) is unaccented to a lower case
    <verbatim|d> (verified; <verbatim|universal.cpp:400>).

    <item><cpp|uni_is_letter> considers every byte above 127 a letter,
    including the section, pound and inverted punctuation signs (verified):
    the test <verbatim|(c & 97) != 31> at <verbatim|universal.cpp:460> is
    always true.

    <item><cpp|html_to_utf8> only decodes hexadecimal entities between
    <verbatim|0x80> and <verbatim|0xFF> (<verbatim|converter.cpp:924>), so
    it does not invert <cpp|utf8_to_html>, which encodes all non-<abbr|ASCII>
    characters (verified: <verbatim|&#x2013;> is left unchanged).

    <item>An unterminated <verbatim|\<less\>#> at the end of a string makes
    <cpp|cork_to_utf8> (and its variants) output a NUL byte (verified;
    <verbatim|converter.cpp:379> and the analogous lines).

    <item><cpp|decode_from_utf8> on a sequence truncated at the end of the
    string reads the last byte again (<verbatim|converter.cpp:880>), so a
    wrong code point is produced instead of the failsafe byte.

    <item><cpp|decode_base64> drops a final group without padding
    (verified: <verbatim|"QUI"> decodes to the empty string), indexes its
    table with a negative value for bytes above 127
    (<verbatim|base64.cpp:88>), and decodes an empty group, reading past
    the end of an empty array, when <verbatim|=> follows a complete group
    (<verbatim|base64.cpp:69>).

    <item><cpp|looks_universal> only accepts decimal digits in
    <verbatim|\<less\>#...\<gtr\>> (<verbatim|wencoding.cpp:99>), so
    <cpp|western_to_cork> escapes <abbr|ASCII> text containing an escape
    such as <verbatim|\<less\>#E9\<gtr\>> with <cpp|tm_encode>.

    <item>The <verbatim|iso-8859-1> setting for plain text import and
    export applies <cpp|tm_encode> and <cpp|tm_decode> without converting
    bytes (<verbatim|Data/Convert/Verbatim/verbatim.cpp:247>,
    <verbatim|:282>), as if Cork were <name|ISO-8859-1>; characters in the
    range <verbatim|0x80>--<verbatim|0xBF> and the four differing positions
    are mangled, and on export <cpp|tm_decode> drops all symbols. Since
    export with <verbatim|auto> uses the character set of the locale, this
    also happens on systems with a <name|Latin-1> locale.

    <item><cpp|replace> and <cpp|tokenize> loop forever when the pattern
    or separator is empty (<verbatim|analyze.cpp:1263>, <verbatim|:1311>);
    both are reachable from <scheme> (<scm|string-replace>,
    <scm|cpp-string-tokenize>).

    <item><cpp|string_searcher_rep::search_sub> indexes its table out of
    range when the pattern is longer than the indexed string, or when the
    indexed string is empty (<verbatim|fast_search.cpp:90>).

    <item><cpp|from_qstring_utf8> builds the result from a C string
    (<verbatim|Plugins/Qt/qt_utilities.cpp:346>), so text containing a NUL
    character is truncated.

    <item>The combining tilde is recognized as <verbatim|U+033E> (combining
    vertical tilde) instead of <verbatim|U+0303>
    (<verbatim|Plugins/Qt/QTMKeyboardEvent.cpp:137>); probably a typo.

    <item>The <name|iconv> wrapper returns the whole input unchanged when a
    conversion fails, and <cpp|tm_decode> silently drops symbols and
    Unicode escapes which do not have exactly four digits; both lose
    information without telling the caller.
  </enumerate>

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
