<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The character resolution algorithm>

  <section|Overview>

  When a smart font meets a character <src-arg|c> for which its smart map
  has no entry yet, it calls <cpp|smart_font_rep::resolve (string c)>. This
  routine decides once and for all (for the given family, variant, series
  and shape) which subfont will render <src-arg|c>, registers the decision
  with <cpp|sm-\<gtr\>add_char> and returns the subfont number. The
  algorithm is a long sequence of rules; the first rule which applies wins.
  It is helpful to distinguish

  <\enumerate>
    <item>special rules for mathematics, applied only if
    <cpp|math_kind != 0>;

    <item>the main search loop over the families of the family list and
    over increasing <em|attempts>, implemented by the auxiliary routines
    <cpp|resolve (c, fam, attempt)> and <cpp|resolve_rubber (c, fam,
    attempt)>;

    <item>the final fallbacks: mathematical alphabets, standard virtual
    fonts, the old <TeX> fonts and the error font.
  </enumerate>

  The number of attempts is bounded by <cpp|FONT_ATTEMPTS> (20, see
  <source-link|Graphics/Fonts/font.hpp|src/Graphics/Fonts/font.hpp>). Attempt 1 means \Pthe font
  requested by the user\Q; the subsequent attempts ask the font database
  for less and less close matches (see <hlink|font
  selection|font-database-selection.en.tm>).

  <section|Step 1: mathematical rules>

  If the smart font is in one of the mathematical shapes (<cpp|math_kind
  != 0>), the following rules are tried in this order:

  <\enumerate>
    <item>Upright letters <verbatim|\<less\>up-x\<gtr\>>: if the main font
    supports <cpp|substitute_upright (c)>, use subfont <verbatim|up>
    (the main font with <cpp|REWRITE_UPRIGHT>).

    <item>Upright Greek <verbatim|\<less\>upalpha\<gtr\>> or
    <verbatim|\<less\>up-alpha\<gtr\>>: if the main font supports the plain
    Greek letter, use <verbatim|upright-greek>.

    <item>Greek letters (<cpp|is_greek>), unless the shape is
    <verbatim|mathupright> or the family list contains more than one
    unconditional family (<cpp|use_italic_greek>, a documented hack, which
    is overridden when the main font is an untuned <name|OpenType> math
    font, <cpp|ot_math>): if
    the main font has the mathematical italic Greek letter of the
    <verbatim|U+1D6E2> block, use <verbatim|italic-greek> (main font with
    <cpp|REWRITE_ITALIC_GREEK>); otherwise use <verbatim|italic-math>, the
    italic variant of the family.

    <item><verbatim|\<less\>imath\<gtr\>>, <verbatim|\<less\>jmath\<gtr\>>,
    <verbatim|\<less\>ell\<gtr\>>: <verbatim|italic-math>.

    <item>The primes <verbatim|'> and <verbatim|`>, if no family in the list
    provides <verbatim|\<less\>#2B9\<gtr\>> resp.
    <verbatim|\<less\>backprime\<gtr\>> (<cpp|is_italic_prime>):
    <verbatim|italic-math>.

    <item>Characters with a special rewriting (<cpp|is_special>): the
    subfont <verbatim|special> with <cpp|REWRITE_SPECIAL>. Single ASCII
    characters in typewriter variants (<verbatim|-tt>) are excluded, and
    big operators <verbatim|\<less\>big-...\<gtr\>> are excluded when the
    main family is a <verbatim|TeX Gyre ... Math> font or when the family
    list contains <verbatim|mathlarge=> or <verbatim|mathbigop=>, since
    these provide true big operators.

    <item>Characters defined in <verbatim|emu-bracket.vfn>
    (<cpp|find_in_emu_bracket>), unless the main font is italic:
    <verbatim|("virtual" "emu-bracket")>.

    <item><verbatim|\<less\>langle\<gtr\>> and
    <verbatim|\<less\>rangle\<gtr\>>, if the main font is not italic and
    has a slash: the subfont <verbatim|emu-bracket> with
    <cpp|REWRITE_EMULATE>, which draws
    <verbatim|\<less\>emu-langle\<gtr\>> from two halves of a slash.
  </enumerate>

  Independently of <cpp|math_kind>, for the main family <verbatim|roman>
  in shape <verbatim|mathupright> and one of the variants <verbatim|rm>,
  <verbatim|ss>, <verbatim|tt>, single non-letter characters are sent to
  <verbatim|italic-roman>.

  <section|Step 2: the main search loop>

  <\cpp-code>
    for (int attempt= 1; attempt \<less\>= FONT_ATTEMPTS; attempt++) {

    \ \ if (attempt \<gtr\> 1 && substitute_math_letter (c, math_kind) != "") break;

    \ \ for (int i= 0; i \<less\> N(a); i++) {

    \ \ \ \ if (ot_math && is_rubber (c) &&

    \ \ \ \ \ \ \ \ (starts (c, "\<less\>wide-") \|\| starts (c, "\<less\>rubber-"))) {

    \ \ \ \ \ \ int nr= resolve_rubber (c, a[i], attempt);

    \ \ \ \ \ \ if (nr \<gtr\>= 0) return nr;

    \ \ \ \ }

    \ \ \ \ int nr= resolve (c, a[i], attempt);

    \ \ \ \ if (nr \<gtr\>= 0) return nr;

    \ \ \ \ if (is_rubber (c)) {

    \ \ \ \ \ \ nr= resolve_rubber (c, a[i], attempt);

    \ \ \ \ \ \ if (nr \<gtr\>= 0) return nr;

    \ \ \ \ }

    \ \ \ \ if (starts (c, "\<less\>wide-")) {

    \ \ \ \ \ \ if (fn[SUBFONT_MAIN]-\<gtr\>supports (c))

    \ \ \ \ \ \ \ \ return sm-\<gtr\>add_char (tuple ("main"), c);

    \ \ \ \ \ \ if (series == "bold")

    \ \ \ \ \ \ \ \ return sm-\<gtr\>add_char (tuple ("poor-bold"), c);

    \ \ \ \ }

    \ \ }

    }
  </cpp-code>

  Here <cpp|a> is the family list split at the commas. The outer loop is
  over attempts and the inner loop over families, so all families of the
  list are tried with the exact request before any of them is tried with a
  less close match. <name|Unicode> mathematical alphanumeric symbols only
  get one attempt, since they have a dedicated fallback (step 3). An
  <name|OpenType> math font stretches its own wide accents, braces and
  arrows: for such a main font, <verbatim|\<less\>wide-...\<gtr\>> and
  <verbatim|\<less\>rubber-...\<gtr\>> are tried as rubber characters
  before the emulations which <cpp|resolve> would find.

  <subsection|Trying one family>

  <cpp|resolve (c, fam, attempt)> returns a subfont number or <cpp|-1>:

  <\enumerate>
    <item>If <src-arg|fam> is a conditional entry
    <verbatim|<em|conditions>=<em|family>>, the conditions are checked as
    described in <hlink|family lists|smart-fonts-smart.en.tm>; if they fail
    the routine returns <cpp|-1>. The family is normalized with
    <cpp|tex_gyre_fix> and <cpp|kepler_fix>. In the shape
    <verbatim|mathitalic> with <cpp|math_kind != 0>, Greek letters, bold
    letters <verbatim|\<less\>b-...\<gtr\>> and dotless letters are looked
    up in a whole smart font for <em|family> (<verbatim|("subfont"
    family)>), so that the mathematical rules of step 1 apply to that font
    too.

    <item>For a single letter in italic shape, the suffix
    <verbatim| Math> of <verbatim|TeX Gyre> and <verbatim|Stix> families is
    removed: math fonts are only used for symbols.

    <item><em|Attempt 1.>

    <\enumerate>
      <item>The pseudo families <verbatim|cal>, <verbatim|cal*>,
      <verbatim|Bbb>, <verbatim|Bbb****> only accept upper case letters,
      <verbatim|cal**> and <verbatim|Bbb*> only letters.

      <item>If <src-arg|fam> is the main family and the main font supports
      <src-arg|c>, use <verbatim|main>. For another family, use
      <cpp|closest_font (fam, variant, series, rshape, sz, dpi, 1)> if it
      supports <src-arg|c>; the subfont is
      <verbatim|(fam variant series rshape "1")>.

      <item>Legacy <TeX> fonts: Greek characters for the family
      <verbatim|roman> go to <verbatim|greek>; for the families of
      <cpp|is_math_family> the subfont <verbatim|math> (with
      <cpp|REWRITE_MATH>) is tried; for <verbatim|roman> and
      <verbatim|cyrillic>, multi-byte characters are tried in
      <verbatim|cyrillic> (with <cpp|REWRITE_CYRILLIC>).

      <item>The ideographic space <verbatim|\<less\>#3000\<gtr\>> is
      ignored.

      <item>Double struck letters <verbatim|\<less\>bbb-X\<gtr\>> (strings
      of length 7), when the main family is not a <verbatim|TeX Gyre>
      font and the font has the plain letter <verbatim|X>: a
      <verbatim|poor-bbb> subfont. The pen width and height are taken from
      the database characteristics of the font (<cpp|get_up_pen_width>,
      <cpp|get_up_pen_height>), rescaled by the x-height and bounded below
      by a quarter of the line width <cpp|wline>.

      <item><verbatim|\<less\>it-...\<gtr\>>: the subfont <verbatim|it>
      with <cpp|REWRITE_ITALIC>.

      <item>If <src-arg|fam> is the main family and the main font is not
      italic, the <em|emulation> virtual fonts are tried in the order
      <verbatim|emu-fundamental>, <verbatim|emu-greek>,
      <verbatim|emu-operators>, <verbatim|emu-relations>,
      <verbatim|emu-orderings>, <verbatim|emu-setrels>,
      <verbatim|emu-arrows> (<cpp|emu_font_names>). The first one which
      defines <src-arg|c> (<cpp|virtually_defined>) and actually
      <cpp|supports> it with the glyphs of the main font is used, as a
      subfont <verbatim|("emulate" name)>. In this way a symbol missing in
      the main font is preferably constructed from glyphs <em|of the main
      font>, before looking for it in other fonts.

      There is one exception: when the main font has a <verbatim|MATH> table
      (math type <cpp|MATH_TYPE_OPENTYPE> or <cpp|MATH_TYPE_TEX_GYRE>) and
      the construction could only be exported as a bitmap
      (<cpp|virtual_font_draws_vectors> is false), the symbol is taken from
      <name|STIX Two Math>, which is shipped with <TeXmacs>, if that font has
      it (<cpp|resolve_shipped_math>, subfont <verbatim|shipped-math>). The
      document then looks the same on every system, as with an emulation,
      but exports as vectors. Only the few symbols which no font has, such
      as <verbatim|\<less\>triangleup\<gtr\>>, are still exported as
      bitmaps.
    </enumerate>

    <item><em|Attempts <math|k\<gtr\>1>.> The routine looks for a font for
    the <name|Unicode> range of <src-arg|c> (<cpp|get_unicode_range>): the
    variant becomes <verbatim|<em|variant>-<em|range>> (for instance
    <verbatim|rm-greek> or <verbatim|rm-mathsymbols>), which the font
    selection translates into a requirement on the supported scripts, and
    <cpp|closest_font (fam, v, series, rshape, sz, dpi, k-1)> is tried. If
    it supports <src-arg|c>, the subfont
    <verbatim|(fam v series rshape "<em|k-1>")> is used.
  </enumerate>

  <subsection|Extensible delimiters>

  Characters recognized by <cpp|is_rubber>, that is
  <verbatim|\<less\>left-...\<gtr\>>, <verbatim|\<less\>mid-...\<gtr\>>,
  <verbatim|\<less\>right-...\<gtr\>>,
  <verbatim|\<less\>large-...\<gtr\>>, and also the wide accents
  <verbatim|\<less\>wide-...\<gtr\>> and the stretched arrows
  <verbatim|\<less\>rubber-...\<gtr\>>, are handled by
  <cpp|resolve_rubber (c, fam, attempt)>. It extracts the delimiter name
  (for instance <verbatim|(> from <verbatim|\<less\>left-(-3\<gtr\>>). Null
  delimiters (<verbatim|.> and <verbatim|\<less\>nobracket\<gtr\>>) are
  sent to <verbatim|ignore>. When the global <cpp|has_poor_rubber>
  (<source-link|Graphics/Fonts/font.cpp|src/Graphics/Fonts/font.cpp>) is set, some delimiters are replaced
  by a simpler <em|goal> whose presence in the font is enough to build the
  delimiter: <verbatim|\<less\>mid\<gtr\>> for
  <verbatim|\<less\>sqrt\<gtr\>> and double bars, <verbatim|/> for angle
  brackets, <verbatim|[> and <verbatim|]> for floors, ceilings and double
  brackets. The goal is resolved like an ordinary character in the main
  family of the entry, giving a subfont <math|k>; the delimiter is then
  rendered by the subfont <verbatim|("rubber" k)>, which is
  <cpp|rubber_font (fn[k])>, if that font supports it. A long arrow whose
  long form the font lacks (<verbatim|\<less\>rubber-longrightarrow\<gtr\>>,
  for instance) is built on the plain arrow. Italic main fonts never
  provide rubber characters.

  The <cpp|rubber_font> wrapper (in <source-link|Graphics/Fonts/font.cpp|src/Graphics/Fonts/font.cpp>)
  caches one extensible font per base font and lets the base font choose it
  with the virtual method <cpp|make_rubber_font>. A <name|Unicode> font
  with a <verbatim|MATH> table returns a <cpp|rubber_unicode_font> which
  takes the size variants and the assemblies of the table (see
  <hlink|stretchable glyphs|opentype-stretch.en.tm>); a smart font whose
  main font is an <name|OpenType> math font hands the job to it. The
  default <cpp|font_rep::make_rubber_font> chooses
  <cpp|rubber_stix_font> for <name|Stix> (unless the hand-tuned tables are
  switched off, <cpp|hand_tuned_math_fonts>), the base font itself if its
  name mentions <verbatim|mathlarge=> or <verbatim|mathrubber=> (the font
  is then expected to handle rubber characters itself),
  <cpp|poor_rubber_font> for other <name|Unicode> fonts when
  <cpp|has_poor_rubber> holds (the default), and
  <cpp|rubber_unicode_font> otherwise. See <hlink|emulated
  fonts|smart-fonts-emulated.en.tm> for <cpp|poor_rubber_font>.

  <subsection|Wide accents>

  Wide accents <verbatim|\<less\>wide-...\<gtr\>> which are not resolved
  in a family are first tried as rubber characters (above), which only
  succeeds when the extensible font of the family supports them, as the
  <cpp|rubber_unicode_font> of a font with a <verbatim|MATH> table does;
  they are then taken from the main font if it supports them, and
  otherwise, in bold series, from a <verbatim|poor-bold> subfont (an
  emulated bold version of the medium font).

  <section|Step 3: final fallbacks>

  If the main loop failed, the following fallbacks are tried:

  <\enumerate>
    <item>Mathematical alphanumeric symbols (<verbatim|U+1D400> to
    <verbatim|U+1D7FF> and the letter-like symbols <verbatim|U+2100> to
    <verbatim|U+213F>): <cpp|substitute_math_letter (c, math_kind)> returns
    the name of an alphabet subfont (<verbatim|bold-math>,
    <verbatim|italic-math>, <verbatim|cal>, <verbatim|frak>,
    <verbatim|bbb>, <verbatim|ss>, <verbatim|tt>, ...), which renders the
    corresponding plain letter thanks to <cpp|REWRITE_LETTERS>. Script,
    fraktur and double struck letters are only substituted in
    <verbatim|mathupright> mode (<cpp|math_kind == 2>), and no substitution
    takes place outside mathematics.

    <item>If <src-arg|c> is defined in one of the traditional virtual fonts
    <verbatim|tradi-long>, <verbatim|tradi-negate>, <verbatim|tradi-misc>
    (<cpp|find_in_virtual>), the subfont <verbatim|("virtual" name)>. These
    virtual fonts are built on the smart font itself, so their components
    (arrows, bars, relations) are resolved recursively by the same
    algorithm.

    <item>In mathematics, characters which have no <name|Unicode> code point
    (<cpp|unicode_provides> is false) and are not delimiters go to
    <verbatim|other>, a smart font for the family <verbatim|roman> in shape
    <verbatim|mathitalic>, that is the old <TeX> math fonts.

    <item>Otherwise the character is drawn in red with the error font.
  </enumerate>

  <section|Example>

  Consider the family list <verbatim|mathlarge=TeX Gyre Pagella,Linux
  Libertine> in mathematical mode, and the string
  <verbatim|x\<less\>leq\<gtr\>\<less\>big-sum-2\<gtr\>\<less\>alpha\<gtr\>>.

  <\itemize>
    <item><verbatim|x> is an isolated letter, hence it is sent directly to
    the italic subfont <cpp|italic_nr> without being resolved. (For an
    untuned <name|OpenType> math font, this subfont is
    <verbatim|ot-italic>, which draws <verbatim|\<less\>#1D465\<gtr\>>
    from the main font.)

    <item><verbatim|\<less\>leq\<gtr\>>: step 1 does not apply; in the main
    loop the conditional entry <verbatim|mathlarge=TeX Gyre Pagella> fails
    (not a large symbol), and the main family <verbatim|Linux Libertine> is
    tried. If the main font has the symbol, the subfont is
    <verbatim|main>. If not, the emulation fonts are tried; then,
    with attempt 2, the closest font with variant
    <verbatim|rm-mathsymbols>.

    <item><verbatim|\<less\>big-sum-2\<gtr\>>: <cpp|is_special> would
    rewrite it into <verbatim|\<less\>sum\<gtr\>>, but this rule is
    disabled because the family list contains <verbatim|mathlarge=>. In the
    main loop, the condition <verbatim|mathlarge> holds (big operator), so
    <verbatim|TeX Gyre Pagella> is tried; <cpp|tex_gyre_fix> turns it into
    <verbatim|TeX Gyre Pagella Math> for the medium math shape, whose
    <name|Unicode> font supports the big operators natively (see
    <cpp|tex_gyre_native> in <source-link|Plugins/Freetype/unicode_font.cpp|src/Plugins/Freetype/unicode_font.cpp>).

    <item><verbatim|\<less\>alpha\<gtr\>>: step 1 sends it to
    <verbatim|italic-greek> if the main font has
    <verbatim|\<less\>#1D6FC\<gtr\>> (mathematical italic alpha), and to
    <verbatim|italic-math> (Linux Libertine Italic) otherwise.
  </itemize>

  The results are memorized in the smart map, so the next occurrence of
  these characters, in any size, costs a table lookup.

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
