<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Older composite fonts: compound and math fonts>

  <section|Introduction>

  Before smart fonts were introduced (2013), the combination of several
  physical fonts was described explicitly by the rule based font selection
  (<cpp|find_font> in <source-link|Graphics/Fonts/find_font.cpp|src/Graphics/Fonts/find_font.cpp> and the
  <scm|set-font-rules> declarations in <verbatim|progs/fonts/*.scm>, see
  the <hlink|overview|fonts.en.tm>). Three kinds of composite fonts are
  used there: <em|compound fonts>, <em|math fonts> and <em|Unicode math
  fonts>. They remain in use for the <TeX> based fonts, for all fonts when
  the preference <verbatim|"new style fonts"> is off, and inside smart
  fonts for the historical families (the <verbatim|math> subfonts of
  <hlink|smart fonts|smart-fonts-smart.en.tm>). All of them are static: the
  mapping from symbols to fonts is given by encoding tables rather than
  computed from the fonts themselves.

  <section|Charmaps>

  A <cpp|charmap> (<source-link|Graphics/Fonts/charmap.hpp|src/Graphics/Fonts/charmap.hpp>) decides which of
  several fonts renders a symbol, and how the symbol is called in that
  font:

  <\cpp-code>
    struct charmap_rep: rep\<less\>charmap\<gtr\> {

    \ \ int\ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ fast_map[256];

    \ \ hashmap\<less\>string,int\<gtr\>\ \ \ \ slow_map;

    \ \ hashmap\<less\>string,string\<gtr\> slow_subst;

    \;

    \ \ inline virtual int arity () { return 1; }

    \ \ inline virtual charmap child (int i) { ... }

    \ \ inline virtual void lookup (string s, int& ch, string& r) { ch= 0; r= s; }

    \ \ void advance (string s, int& pos, string& r, int& ch);

    };
  </cpp-code>

  <cpp|lookup> returns a font number <src-arg|ch> (or <cpp|-1>) and the
  translated symbol <src-arg|r>; results are cached in <cpp|slow_map> and
  <cpp|slow_subst>, and in <cpp|fast_map> for the one-byte characters which
  are not translated. <cpp|advance> extracts the longest run of characters
  with the same font number, concatenating the translated symbols, exactly
  like <cpp|smart_font_rep::advance>. The concrete charmaps
  (<source-link|Graphics/Fonts/charmap.cpp|src/Graphics/Fonts/charmap.cpp>) are:

  <\description>
    <item*|<verbatim|any>>Every symbol, unchanged.

    <item*|<verbatim|ec>>One-byte characters and <verbatim|\<less\>less\<gtr\>>,
    <verbatim|\<less\>gtr\<gtr\>>.

    <item*|<verbatim|la>, <verbatim|hangul>, <verbatim|oriental>>The
    <name|Unicode> ranges <verbatim|U+400>\U<verbatim|U+4FF>,
    <verbatim|U+AC00>\U<verbatim|U+D7A3> and
    <verbatim|U+3000>\U<verbatim|U+FFFF> (<cpp|range_charmap>), for symbols
    written as <verbatim|\<less\>#...\<gtr\>>.

    <item*|any other name>An <cpp|explicit_charmap> based on the translator
    (encoding file <verbatim|fonts/enc/<em|name>.enc> or virtual font) of
    that name: a symbol in the dictionary is replaced by the one-byte
    string of its code.
  </description>

  <cpp|load_charmap (def)> builds a <cpp|join_charmap> from a list of such
  names: a symbol goes to the first charmap which accepts it, and the font
  numbers are offset by the arities of the preceding charmaps.

  <section|Compound fonts>

  A compound font is described by the macro
  <verbatim|(compound (<em|cm<rsub|1>> <em|font<rsub|1>>) ...)>, as in
  <source-link|progs/fonts/fonts-composite.scm|TeXmacs/progs/fonts/fonts-composite.scm>:

  <\scm-code>
    ((modern $v $a $b $s $d)

    \ (compound (ec (roman $v $a $b $s $d))

    \ \ \ \ \ \ \ \ \ \ \ (la (cyrillic $v $a $b $s $d))

    \ \ \ \ \ \ \ \ \ \ \ (cmr (tex cmr $s $d))

    \ \ \ \ \ \ \ \ \ \ \ ...

    \ \ \ \ \ \ \ \ \ \ \ (tradi-long (virtual tradi-long $s $d))

    \ \ \ \ \ \ \ \ \ \ \ ...))
  </scm-code>

  <cpp|compound_font (def, hzf, vzf)> creates a <cpp|compound_font_rep>
  with the charmap <cpp|load_charmap> of the first elements and an array of
  subfonts, of which only the first one is created immediately. The others
  are created in <cpp|compound_font_rep::advance> the first time a run
  needs them: by <cpp|virtual_font (this, name, size, dpi, ...)> for a
  <verbatim|(virtual <em|name> <em|size> <em|dpi>)> specification (the
  compound font itself is the base, as for the virtual subfonts of smart
  fonts), and by <cpp|find_magnified_font> otherwise. Measuring and
  drawing proceed run by run as in smart fonts.

  <section|Math fonts>

  <cpp|math_font (t, base_fn, error_fn, hzf, vzf)>
  (<source-link|Graphics/Fonts/math_font.cpp|src/Graphics/Fonts/math_font.cpp>) implements the
  <verbatim|(math (math <em|enc> <em|fonts>...) (rubber <em|enc>
  <em|fonts>...) <em|base> <em|error>)> macro (see
  <source-link|progs/fonts/fonts-math.scm|TeXmacs/progs/fonts/fonts-math.scm>). It holds two translators,
  <cpp|math> and <cpp|rubber>, loaded from the encodings named by the
  first elements, and the lists of fonts. A translator code <math|c>
  designates the font number <math|c/256> and the character
  <math|c&255> in it (encoding files concatenate the encodings of several
  fonts by blocks of 256). <cpp|math_font_rep::search_font> finds the font
  and rewrites the symbol:

  <\itemize>
    <item>For symbols with a numeric size suffix
    (<verbatim|\<less\>left-(-3\<gtr\>>), the rubber translator is
    consulted first; then the math translator with the entry
    <verbatim|\<less\>left-(-#\<gtr\>>, in which case the symbol is
    rewritten as the code byte followed by the size. This is the convention
    understood by <cpp|virtual_font_rep::get_char> for parameterized
    virtual glyphs.

    <item>Otherwise the math translator gives a code; the symbol becomes the
    one-byte string of the character.

    <item>Strings of letters, digits and a few punctuation characters are
    sent to the base font; anything else to the error font.
  </itemize>

  Fonts given as <verbatim|(virtual <em|name> <em|size> <em|dpi>)> are
  virtual fonts with the math font as base, as for compound fonts. This is
  how the <verbatim|tradi-*> virtual fonts originated.

  <section|Unicode math fonts>

  <cpp|unicode_math_font (up, it, bup, bit, fb)>
  (<source-link|Plugins/Freetype/unicode_math_font.cpp|src/Plugins/Freetype/unicode_math_font.cpp>) implements the
  <verbatim|unimath> macro, which combines an upright, an italic, a bold
  upright and a bold italic <name|Unicode> font with a fallback font
  (<source-link|progs/fonts/fonts-math.scm|TeXmacs/progs/fonts/fonts-math.scm>, rules
  <verbatim|unicode-math>). <cpp|search_font_sub> classifies each symbol
  once (cached in <cpp|mapper>): single letters go to the italic font,
  <verbatim|\<less\>b-...\<gtr\>> symbols to the bold fonts,
  <verbatim|-> and <verbatim|\|> are rewritten into
  <verbatim|\<less\>minus\<gtr\>> and <verbatim|\<less\>mid\<gtr\>>, big
  operators are mapped to <name|Unicode> symbols, and symbols without a
  <name|Unicode> code point go to the fallback font. Several of these rules
  were later carried over to the <cpp|REWRITE_SPECIAL> and mathematical
  rules of smart fonts.

  <section|Comparison with smart fonts>

  <\description>
    <item*|Static versus dynamic>Composite fonts route symbols with fixed
    encoding tables; smart fonts ask the fonts whether they
    <cpp|supports> a symbol and consult the font database.

    <item*|Eager versus lazy decisions>Charmaps compute the destination of
    every symbol from the tables; smart fonts resolve characters on demand
    and share the decisions among all sizes (<cpp|smart_map>).

    <item*|Common mechanisms>Both cut strings into runs with an
    <cpp|advance> routine, both use virtual fonts with the composite font
    itself as base, and both delegate slopes and corrections to the
    subfont of the first or last run.
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
