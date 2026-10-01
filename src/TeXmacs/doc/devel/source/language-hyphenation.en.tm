<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Hyphenation>

  <TeXmacs> hyphenates words with Liang's algorithm, the one used by <TeX>,
  and with the same pattern files. The implementation is in
  <verbatim|System/Language/hyphenate.cpp>; the text languages of
  <verbatim|text_language.cpp> load the patterns in their constructors and
  call the algorithm from <cpp|get_hyphens> and <cpp|hyphenate>.

  <section|Pattern files>

  The patterns live in <verbatim|src/TeXmacs/langs/natural/hyphen/> as
  files <verbatim|hyphen.<em|name>>, where <em|name> is the second argument
  given to the language constructor in <cpp|text_language> (for instance
  <verbatim|us> for English and <verbatim|ukenglish> for British English).
  They are ordinary <TeX> pattern files in <name|UTF-8>, with a
  <verbatim|\\patterns{...}> block and an optional
  <verbatim|\\hyphenation{...}> block of exceptions.

  <cpp|load_hyphen_tables (name, patterns, hyphenations, toCork)> reads
  such a file into two hash tables:

  <\description>
    <item*|<cpp|patterns>>For each pattern such as <verbatim|1ba> or
    <verbatim|.ach4>, the key is the pattern without its digits
    (<verbatim|ba>, <verbatim|.ach>) and the value is the pattern itself.
    The <TeX> escapes <verbatim|^^<em|xx>> are decoded first.

    <item*|<cpp|hyphenations>>For each exception such as
    <verbatim|uni-ver-sity>, the key is the word without hyphens and the
    value is the word with hyphens.
  </description>

  The file is cut into tokens at white space; <verbatim|%> starts a comment
  up to the end of the line; a block starts at the token
  <verbatim|\\patterns{> or <verbatim|\\hyphenation{> and ends at a token
  consisting of a single <verbatim|}>. If <cpp|toCork> is set, which is
  the case for all text languages except those of
  <cpp|ucs_text_language_rep> (Bulgarian, Russian, Ukrainian), the file is
  converted from <name|UTF-8> to the Cork encoding before parsing, so that
  patterns can be compared directly with <TeXmacs> strings. For the other
  languages the patterns stay in <name|UTF-8> and words are converted to
  <name|UTF-8> before hyphenation.

  <section|The algorithm>

  <cpp|get_hyphens (s, patterns, hyphenations, utf8)> returns an array of
  penalties, one for each position between two consecutive characters of
  the word <cpp|s>: the entry with index <math|k> is the penalty for
  breaking after the first <math|k+1> characters. It proceeds as follows:

  <\enumerate>
    <item>The word is converted to lower case (and to <name|UTF-8> in the
    <cpp|utf8> case).

    <item>If the word is an exception, the penalties are read from the
    hyphenated form: <verbatim|HYPH_STD> where the exception has a hyphen,
    <verbatim|HYPH_INVALID> elsewhere.

    <item>Otherwise, the word is surrounded by dots, and for every
    substring of length 1 to 9 the pattern table is consulted. The digits
    of each matching pattern are stored in an array of the inter-letter
    positions, keeping the maximum. A position with an odd value is a
    hyphenation point (penalty <verbatim|HYPH_STD>), any other position is
    forbidden (<verbatim|HYPH_INVALID>).

    <item>Finally, breaks after the first one or two characters and before
    the last one, two or three characters are forbidden. As implemented,
    a hyphenated word therefore keeps at least three characters before the
    hyphen and four after it. This restriction does not apply to
    exceptions.
  </enumerate>

  <cpp|std_hyphenate (s, after, left, right, penalty, utf8)> performs the
  actual split: <cpp|left> receives the first <cpp|after+1> characters
  followed by a hyphen <verbatim|->, and <cpp|right> receives the rest. In
  the <cpp|utf8> case positions are counted in characters, where a
  <verbatim|\<less\>...\<gtr\>> entity counts as one character. (If the
  given penalty is <verbatim|HYPH_INVALID>, a backslash is appended instead
  of the hyphen; the line breaker never asks for such a split.)

  <section|Variants>

  <\description>
    <item*|Oriental and verbatim languages>Their <cpp|get_hyphens> forbids
    all breaks; lines are broken between the units returned by
    <cpp|advance> instead.

    <item*|<cpp|hyphenless_language>>Forbids all breaks inside words; used
    by <markup|hgroup>.

    <item*|<cpp|ad_hoc_language>>Uses an explicit hyphenation for one
    word, given by the primitive <markup|hyphenate-as>. Its break penalty is
    0 rather than <verbatim|HYPH_STD>, so these breaks are preferred to
    ordinary hyphenation points.
  </description>

  <section|Use by the line breaker>

  The concatenation typesetter stores the language in every string line
  item. When the line breaker (<verbatim|Typeset/Line/line_breaker.cpp>)
  looks for break points inside a word that does not fit, it calls
  <cpp|item-\<gtr\>lan-\<gtr\>get_hyphens (s)> and tries the positions
  with a penalty below <verbatim|HYPH_INVALID>, splitting the item with
  <cpp|item-\<gtr\>lan-\<gtr\>hyphenate>. The penalty enters the cost of
  the line break. The details, including the influence of the
  <verbatim|par-hyphen> variable, are in <hlink|line
  breaking|typesetter-lines.en.tm>.

  <section|Pitfalls>

  <\itemize>
    <item>The pattern loader only recognizes the end of a block when the
    closing brace is a token on its own. In
    <verbatim|hyphen.ukenglish> the exception list ends with
    <verbatim|some-thing}>, so the exception is stored under the key
    <verbatim|something}> and never matches; the comment in
    <verbatim|hyphenate.cpp> (\Pbug: shows the hyphenation something}
    --\<gtr\> some-thing}\Q) refers to this. Exceptions and patterns
    which come after such a token would also be misclassified.

    <item>In the Cork case, letters which have no Cork code (for instance
    Greek letters) become entities <verbatim|\<less\>...\<gtr\>>, both in
    the patterns and in the words. The algorithm does not handle this well:
    substrings are taken with a length in <em|bytes> (at most 9), while the
    array of scores is indexed by characters in one place and by bytes in
    another (<verbatim|hyphenate.cpp:224-234>). For words containing
    entities, hyphenation points are therefore missing or misplaced. The
    <name|UTF-8> branch used by <cpp|ucs_text_language_rep> does not have
    this problem.

    <item>The index returned by <cpp|get_hyphens> counts characters in the
    <cpp|utf8> case and bytes otherwise; callers must not mix the two.
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
