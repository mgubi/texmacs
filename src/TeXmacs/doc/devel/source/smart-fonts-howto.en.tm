<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Extending, debugging and pitfalls>

  <section|Adding a new virtual glyph>

  Suppose that a symbol <verbatim|\<less\>foo\<gtr\>> is drawn in red
  although it could be built from other symbols. No recompilation is
  needed for this, only a definition in a <verbatim|.vfn> file.

  <\enumerate>
    <item><em|Choose the file.> If the symbol should be drawn in the style
    of the current text font, from the glyphs of that font, put it in the
    appropriate <verbatim|emu-*.vfn> file (relations in
    <verbatim|emu-relations.vfn>, arrows in <verbatim|emu-arrows.vfn>, and
    so on); these fonts are used in extend mode on top of the main font and
    of <verbatim|emu-fundamental>, so the definition can use all symbols
    of the main font and all the building blocks of
    <verbatim|emu-fundamental.vfn>. If the symbol is a traditional
    combination which should work with any font, put it in one of the
    <verbatim|tradi-*.vfn> files; these are built on the smart font itself
    and are only tried after all physical fonts.

    <item><em|Write the definition>, as a new entry
    <verbatim|(foo <em|expression>)>. Use names of more than one character.
    Prefer the operators which can be drawn as vector graphics (see the
    <hlink|reference|smart-fonts-virtual.en.tm>); use <verbatim|bitmap>
    explicitly when a vector rendering would be wrong. When the definition
    depends on peculiarities of particular fonts, guard the alternatives
    with <verbatim|or> and <verbatim|font>, as in
    <verbatim|emu-fundamental.vfn>:

    <\scm-code>
      (minus* (or (font (scale (hor-crop _) (hor-crop +) 1 *)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ Baskerville meyne_textur Papyrus)

      \ \ \ \ \ \ \ \ \ \ \ \ (scale - + 1 *)))
    </scm-code>

    <item><em|Test> the symbol with several main fonts, at several
    magnifications, in text and in mathematics, both on the screen and in
    a <abbr|PDF> export (the screen uses the bitmap path, printing the
    vector path). Since translators and fonts are cached, restart
    <TeXmacs> after modifying a <verbatim|.vfn> file. A personal copy of a
    file in <verbatim|$TEXMACS_HOME_PATH/fonts/virtual> overrides the
    system one, which is convenient for experiments.
  </enumerate>

  A new <verbatim|.vfn> file is not used automatically: the list of
  emulation fonts is hard-coded in <cpp|emu_font_names> and the list of
  traditional fonts in <cpp|initialize_virtual>, both in
  <source-link|Graphics/Fonts/smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>. The order of these lists is
  the order in which the files are searched.

  <section|Extending the smart font>

  <subsection|A new fallback or rewriting rule>

  To render a class of characters in a new way:

  <\enumerate>
    <item>If the subfont needs the characters in another encoding, add a
    constant <verbatim|REWRITE_<em|NAME>> and a case in the static function
    <cpp|rewrite>.

    <item>Choose a specification tree for the subfont, for instance
    <verbatim|("my-kind" <em|parameters>)>, and add a branch to
    <cpp|smart_font_rep::initialize_font> which creates the font. Use
    <cpp|smart_font_bis> for a variant of the same family,
    <cpp|closest_font> together with <cpp|adjusted_dpi> for another
    family, and wrap the result with <cpp|adjust_subfont> if it is not a
    smart font, so that the horizontal resolution is respected.

    <item>Add a rule at the right place of
    <cpp|smart_font_rep::resolve (string c)> or of
    <cpp|resolve (c, fam, attempt)>, which registers the decision with
    <cpp|sm-\<gtr\>add_font (key, rewr)> and
    <cpp|sm-\<gtr\>add_char (key, c)>. Check that the subfont
    <cpp|supports> the (rewritten) character before registering it,
    otherwise the character will be invisible rather than red.
  </enumerate>

  The decision must only depend on the family, variant, series and shape
  of the smart font and on the character, never on its size or
  resolution, since the smart map is shared by all sizes.

  <subsection|A new <name|Unicode> range>

  Ranges are defined in <cpp|get_unicode_range (int code)>. A new range
  name can then be used in conditional family entries
  (<verbatim|myrange=Some Font>) and is used automatically in the attempts
  <math|k\<gtr\>1>, which request the variant
  <verbatim|<em|variant>-myrange> from the font database; for this to be
  useful, the font database must know which fonts cover the range (see the
  <hlink|font selection|font-database-selection.en.tm>).

  <section|Adding a new emulation>

  To emulate a variant which the font database may lack (for instance a
  condensed shape), proceed as follows:

  <\enumerate>
    <item>Implement the emulated font, following the pattern of the
    <hlink|existing ones|smart-fonts-emulated.en.tm>: a
    <cpp|font_rep> subclass with a <cpp|base> font, transformed metrics in
    <cpp|get_extents> and <cpp|get_xpositions>, transformed glyph tables in
    <cpp|index_glyph> and <cpp|get_glyph>, a <cpp|draw_fixed> which draws
    through <cpp|ren-\<gtr\>draw> on the screen and, if possible, through
    renderer transformations on printers, a <cpp|magnify> and a
    constructor function with a unique resource name. Declare it in
    <source-link|Graphics/Fonts/font.hpp|src/Graphics/Fonts/font.hpp> and add the file to the build.

    <item>In <cpp|find_closest> (<source-link|Graphics/Fonts/font_translate.cpp|src/Graphics/Fonts/font_translate.cpp>),
    append a new suffix (like <verbatim|-poorbf>) to the series, shape or
    variant when the requested logical feature is not provided by the
    font which was found.

    <item>In <cpp|find_font (family, variant, series, shape, sz, dpi)>
    (<source-link|Graphics/Fonts/find_font.cpp|src/Graphics/Fonts/find_font.cpp>), recognize the suffix, find
    the font without it, wrap it with the new emulation, and store the
    result in <cpp|font::instances> under the full name.

    <item>If the emulation should also be available on demand, add an
    effect to <cpp|apply_effects> (see <hlink|font
    effects|smart-fonts-effects.en.tm>).
  </enumerate>

  <section|Debugging>

  <\description>
    <item*|Red symbols>A symbol drawn in red went through the whole
    resolution algorithm without finding a font: it is rendered by the
    error font. Check that the name of the symbol is right, that some
    installed font has it (the font database must be up to date, see
    <hlink|font database|font-database.en.tm>), or define it virtually.

    <item*|Which subfont is used?>The font inspector,
    <menu|Tools|Fonts|Font inspector>, answers this question without
    recompiling: it reports the route, the subfont, the file and the
    rewriting of the character at the cursor, read from the smart map
    (<cpp|smart_font_debug_info>), and can colour every glyph by its route
    (debug switch <verbatim|fonts>) or list all the characters of a
    document by route in a font report; see <hlink|inspecting the font
    system|opentype-tools.en.tm>. For finer questions,
    <source-link|smart_font.cpp|src/Graphics/Fonts/smart_font.cpp> contains many commented out traces, such as
    <cpp|//cout \<less\>\<less\> "Found " \<less\>\<less\> c \<less\>\<less\> " in " ...>
    in <cpp|resolve> and
    <cpp|//cout \<less\>\<less\> "Font " \<less\>\<less\> nr ...> at the end of
    <cpp|initialize_font>. Enabling them temporarily is the fastest way to
    understand a resolution. Remember that decisions are cached in the
    smart map for the whole session.

    <item*|Debug output>Font related messages are written to the
    <cpp|debug_fonts> stream (channel <verbatim|debug-fonts>, visible in
    the debugging console). The loading of encodings and virtual fonts is
    reported when the <verbatim|std> debug flag is set, for instance with
    <scm|(debug-set "std" #t)>.

    <item*|Character tables><scm|build-character-table> in
    <source-link|progs/fonts/font-sample.scm|TeXmacs/progs/fonts/font-sample.scm> builds, as <scheme> markup, a
    table with all characters between two code points, for instance
    <scm|(insert (stree-\<gtr\>tree (build-character-table #x2190
    #x21FF)))>. Inserting such tables with various values of
    <src-var|font> shows at a glance which characters are native, emulated
    or missing.

    <item*|Comparing with the old mechanism>Switching off the preference
    <verbatim|"new style fonts"> makes <cpp|smart_font> fall back on the
    rule based <cpp|find_font>. Similarly, <scm|(set-hand-tuned-math-fonts
    #f)> typesets the fonts which have both hand-tuned tables and a
    <verbatim|MATH> table (<name|TeX Gyre>, <name|Stix>) from their table
    only.

    <item*|Missing glyph bitmaps>The warning <verbatim|no bitmap available
    for ...> comes from the default <cpp|font_rep::get_glyph>: a virtual or
    emulated font was built on a font which cannot export glyphs. Setting
    <cpp|get_glyph_fatal> in <source-link|Graphics/Fonts/font.cpp|src/Graphics/Fonts/font.cpp> turns these
    warnings into failures, which gives a backtrace.

    <item*|Fatal errors><verbatim|"invalid virtual character"> (with the
    offending tree) means that <cpp|compile_bis> met an unknown construct;
    <verbatim|"bad virtual font format"> that a <verbatim|.vfn> file does
    not start with <verbatim|virtual-font>; <verbatim|"substitution font
    loop detected"> that <cpp|initialize_font> produced the smart font
    itself as a subfont.
  </description>

  <section|Pitfalls>

  <\itemize>
    <item><em|Smart fonts support everything.>
    <cpp|smart_font_rep::supports> always returns <cpp|true>. In a plain
    mode virtual font whose base is a smart font (the
    <verbatim|tradi-*> and <verbatim|emu-bracket> subfonts), all atoms are
    therefore considered supported, and <verbatim|(or <em|g<rsub|1>>
    <em|g<rsub|2>>)> always selects <math|g<rsub|1>>; only the
    <verbatim|font> test discriminates. Alternatives are meaningful in
    extend mode virtual fonts on a physical font (the <verbatim|emu-*>
    fonts).

    <item><em|Unknown constructs.> An unknown or misspelled operator makes
    <cpp|supported> return false, which silently disables the glyph in the
    <verbatim|emu-*> fonts (the emulation is only used if it is
    supported). In the <verbatim|tradi-*> fonts, which are selected by name
    only, the same mistake reaches <cpp|compile_bis> and aborts with
    <verbatim|"invalid virtual character">.

    <item><em|Variables shadow glyphs.> <verbatim|with> replaces every atom
    equal to the variable in its body, so a variable named <verbatim|w>
    hides the letter <verbatim|w> in the body.

    <item><em|One-character names.> A one-byte string is interpreted by
    <cpp|get_char> as a <em|code> (position in the file), not as a name, so
    entries with one-character names cannot be requested.

    <item><em|Codes of parameterized glyphs.> The legacy encoding \Pcode
    byte followed by a parameter\Q (used by <cpp|poor_rubber_font> and
    <cpp|math_font>) only works for entries whose position in the file is
    below 256 and different from the code of <verbatim|\<less\>> (60), since
    the string would otherwise be taken for a symbol name.

    <item><em|Shared smart maps.> Since the smart map is shared by all
    sizes, a subfont which supports a character at one size is assumed to
    support it at all sizes. Conversely, a smart font may find subfont
    numbers in the map which it has not initialized yet; always call
    <cpp|initialize_font> before using <cpp|fn[nr]>.

    <item><em|Rewritten runs.> Rewriting can change the length of a run
    (for instance <verbatim|\<less\>bbb-A\<gtr\>> becomes <verbatim|A>).
    The cursor positions inside such a run are approximated by the
    position of its start.

    <item><em|Bitmaps in PDF.> Glyphs using bitmap-only operators, and
    bold, blackboard bold, extended, distorted and blurred emulations, are
    embedded as bitmap (<name|Type 3>) fonts. Their quality depends on the
    resolution of the font. When the main font has a <verbatim|MATH>
    table, a symbol whose emulation would be such a bitmap is taken from
    <name|STIX Two Math> instead, if that font has it; the font report of
    the inspector says which emulated glyphs are still exported as
    bitmaps.

    <item><em|The magic slant.> The automatic italic emulation uses the
    slant <cpp|0.25001>, which <source-link|Typeset/Concat/concat_math.cpp|src/Typeset/Concat/concat_math.cpp>
    recognizes; do not change one without the other.

    <item><em|Accumulating effects.> <markup|add-font-effect> appends to
    <src-var|font-effects>, so nested <markup|embold> tags embolden
    repeatedly.
  </itemize>

  <section|Known issues in the code>

  The following problems were noticed in the code while writing this
  documentation:

  <\itemize>
    <item><source-link|smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>, <cpp|resolve (c, fam, attempt)>,
    attempts
    <math|k\<gtr\>1>: the test <cpp|v == "rm"> is made on the still empty
    string <cpp|v> instead of <cpp|variant>, so the variant
    <verbatim|rm-<em|range>> is always requested, never just
    <verbatim|<em|range>>.

    <item><source-link|smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>, <cpp|in_unicode_range>: for an empty
    conversion it executes <cpp|return "";> in a function returning
    <cpp|bool>, which yields <cpp|true>.

    <item><source-link|virtual_font.cpp|src/Graphics/Fonts/virtual_font.cpp>, <cpp|compile_bis> for
    <verbatim|scale>: the <verbatim|@> arguments are processed after the
    magnifications have been computed and have no effect, so
    <verbatim|(scale <em|g> <em|r> @ 1)> only scales vertically.

    <item><source-link|virtual_font.cpp|src/Graphics/Fonts/virtual_font.cpp>, <verbatim|fscale>: the vertical
    <cpp|deepen> is applied to the original glyph instead of the result of
    <cpp|widen>.

    <item><source-link|virtual_font.cpp|src/Graphics/Fonts/virtual_font.cpp>, <verbatim|rot-right>: the logical
    height is computed as <cpp|ey-\<gtr\>x2- ex-\<gtr\>x1> (with
    <cpp|ex-\<gtr\>x1> just set to zero) instead of
    <cpp|ey-\<gtr\>x2- ey-\<gtr\>x1> as for <verbatim|rot-left>.

    <item><source-link|virtual_font.cpp|src/Graphics/Fonts/virtual_font.cpp>: <verbatim|deepen> and
    <verbatim|widen> are declared vector-capable in <cpp|supported> but
    are not handled by <cpp|draw_tree>; a glyph using them on vector
    components would be missing on printers. (The only current use applies
    them to a <verbatim|circle>, which forces the bitmap path.)

    <item><source-link|poor_effected.cpp|src/Graphics/Fonts/poor_effected.cpp>, <cpp|get_extents>: the top of the
    ink box is merged with <cpp|min> instead of <cpp|max>.

    <item><source-link|poor_extended.cpp|src/Graphics/Fonts/poor_extended.cpp>, <cpp|get_xpositions>: the loops
    rescale <cpp|xpos[0..N(s)-1]> but not the final position
    <cpp|xpos[N(s)]>.

    <item><source-link|math_font.cpp|src/Graphics/Fonts/math_font.cpp>: <cpp|operator !=> on fonts returns
    <cpp|fn1.rep == fn2.rep>.

    <item><source-link|charmap.cpp|src/Graphics/Fonts/charmap.cpp>, <cpp|join_charmap_rep::child>: calls
    <cpp|child (i-sum)> instead of <cpp|child (ch-sum)>; harmless as long
    as all joined charmaps have arity one.

    <item><source-link|smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>, <cpp|register_profiled_font>: a
    profiled math font which is missing from the database is registered
    with <cpp|font_database_extend_local>, which also adds its directory to
    the preference <verbatim|"imported fonts">; merely opening a document
    in such a font therefore changes the user preferences and saves the
    local database.

    <item><source-link|smart_font.cpp|src/Graphics/Fonts/smart_font.cpp>: <cpp|is_rubber> now also accepts the
    stretched arrows <verbatim|\<less\>rubber-...\<gtr\>>, but the pseudo
    ranges <verbatim|mathrubber> and <verbatim|mathlarge> of
    <cpp|in_unicode_range> do not, so that an entry
    <verbatim|mathrubber=<em|family>> does not apply to them.

    <item><source-link|Plugins/Freetype/tt_file.cpp|src/Plugins/Freetype/tt_file.cpp>, <cpp|tt_font_path>:
    <cpp|texlive_font_dirs>, which finds the <TeX> Live font directories of
    any year, is only used on <name|macOS>; the other <name|Unix> systems
    still list <TeX> Live 2020 to 2022 by name.
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
