<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Inspecting the font system, tests and open problems>

  Emulation is how <TeXmacs> lets any font be used for mathematics: what a
  font lacks is taken from another family or constructed. The question is
  therefore less whether a glyph is emulated than being able to see what
  happened to it. This page describes the tools which show the decisions
  of the smart font (the font inspector, the colouring of glyphs by route
  and the font report), the work on mathematical symbols which came with
  the <name|OpenType> support, the tests which guard that support, and
  the problems which are still open.

  The tools are described for users in <hlink|from markup to
  glyph|../fonts/font-resolution.en.tm>; the routing they report on is the
  subject of <hlink|smart, virtual and emulated fonts|smart-fonts.en.tm>
  and in particular <hlink|resolving a character|smart-fonts-resolve.en.tm>.

  <section|What the smart font records>

  A smart font (<verbatim|Graphics/Fonts/smart_font.cpp>) routes each
  character to one of its subfonts once, and remembers the answer in its
  <cpp|smart_map>: <cpp|chv> for the single bytes, <cpp|cht> for the
  other characters, and for every subfont its specification
  <cpp|fn_spec[nr]> (a tuple such as <verbatim|("emulate"
  "emu-fundamental")> or <verbatim|(<var|family> <var|variant>
  <var|series> <var|shape> <var|attempt>)>) and its rewriting
  <cpp|fn_rewr[nr]>. The debugging tools read these tables and never call
  the resolver, so inspecting a character changes neither the routing nor
  the font caches, and a character which has not been routed yet is
  reported as <verbatim|unresolved>.

  <cpp|smart_font_rep::debug_origin (nr)> classifies a subfont, from its
  specification, into one of five <em|routes>, and caches the answer in the
  array <cpp|origins>:

  <\description>
    <item*|<verbatim|font>>The requested font.

    <item*|<verbatim|rule>>A family named by a font rule (the
    specification <verbatim|subfont>, or an attempt number of
    <verbatim|1>).

    <item*|<verbatim|fallback>>Another family found by feature distance
    (<verbatim|other>, a later attempt), or the shipped math font
    (<verbatim|shipped-math>).

    <item*|<verbatim|emulated>>A virtual font, an emulation, an assembled
    bracket or a derived (<verbatim|poor-*>) font.

    <item*|<verbatim|error>>The error font.
  </description>

  A main font extended by a virtual font (its name contains
  <verbatim|#enhance->) draws some characters itself and constructs the
  others; those are classified one by one with
  <cpp|virtual_font_constructs>.

  <section|Colouring glyphs by route>

  The debug switch <verbatim|fonts> (<cpp|DEBUG_FLAG_FONTS>, toggled from
  the <menu|Debug> menu or by <scm|toggle-font-colours-by-origin>)
  affects drawing only. <cpp|draw_fixed> tests the switch once per string;
  when it is on, each piece is drawn by <cpp|debug_draw>, which sets the
  pencil to blue for <verbatim|rule>, orange for <verbatim|fallback> and
  green for <verbatim|emulated> around the ordinary drawing, and leaves the
  requested font and the red error font as they are. Metrics, routing and
  caches are untouched, so turning the switch on or off needs a repaint,
  not a new typesetting. Glyphs which the compound fonts of the
  traditional mathematics assemble internally are not distinguished: the
  switch sees the routes of the smart font, not the insides of its
  subfonts.

  <section|The font inspector>

  <verbatim|TeXmacs/progs/fonts/font-debug.scm> implements
  <menu|Tools|Fonts|Font inspector> (<scm|open-font-inspector>). The window
  is built as <scm|top-window> builds one, from an alternative window
  handle, but with <scm|(alt-window-set-on-top win #t)>, so that it stays
  above the editor windows (see <hlink|alternative
  windows|server-windows.en.tm>). Closing it also turns the colouring off.

  The inspector is driven by two overloadings, active only while it is
  open: <scm|notify-cursor-moved> and, when it follows the mouse,
  <scm|mouse-event> on <verbatim|move>. Both remember the document being
  edited (not a <verbatim|tmfs://aux/> buffer nor a report) and schedule an
  update, which asks one of the glue routines

  <\explain>
    <scm|(font-debug-info <var|at-mouse?>)><explain-synopsis|the glyph at
    the cursor or the mouse>

    <scm|(font-debug-info-of <var|buffer> <var|at-mouse?>)><explain-synopsis|the
    same in another buffer>
  <|explain>
    Implemented in <verbatim|Texmacs/Data/new_view.cpp>. The editor
    provides its root box and the box path of the cursor or of the last
    mouse position (<cpp|get_box_root>, <cpp|get_box_path_at>, new in
    <verbatim|edit_main.cpp>); <cpp|box_font_debug_info>
    (<verbatim|Typeset/Boxes/Basic/font_debug_boxes.cpp>) finds the text box
    there, takes the character before the position (after it, under the
    mouse) and calls <cpp|smart_font_debug_info>. The result is a tuple of
    <verbatim|(key value)> pairs: the character, the font, family, variant,
    series and shape, the subfont number, its specification and name, the
    route, the math type (<verbatim|OpenType>, hand-tuned <name|TeX Gyre>
    or <name|STIX>, or traditional), whether the <name|OpenType> math path
    applies, the rewritten character if any, and, for an emulated
    character, whether a <name|PDF> export draws it as vectors or as a
    bitmap (<cpp|virtual_font_draws_vectors>). The second form looks at a
    view of <var|buffer> shown in a window and creates none; the inspector
    uses it while a report or another auxiliary buffer has the focus.
  </explain>

  The report is displayed by a <scm|texmacs-output> widget inside a
  <scm|refreshable>, with <menu|Follow the mouse>, <menu|Freeze> and
  <menu|Colour glyphs by origin> toggles and a <menu|Font report> button.

  <section|The font report>

  <menu|Font report> opens <verbatim|tmfs://fontdbg/<var|document>>, a
  buffer whose <scheme> handlers (<scm|tmfs-load-handler>,
  <scm|tmfs-master-handler> and the others, in
  <verbatim|font-debug.scm>) generate a document attached to the edited
  one: its master is that document, as for the bibliography viewer. The
  content comes from

  <\explain>
    <scm|(font-debug-report)><explain-synopsis|every character, by
    route>
  <|explain>
    <cpp|box_font_debug_report> walks the typeset boxes of the current
    editor, asks <cpp|smart_font_debug_info> about every character of
    every text box and counts the distinct answers. The result is a tuple
    of entries <verbatim|(origin char family subfont pdf variant series
    shape count)>.
  </explain>

  The report shows a summary per family, then the emulated characters,
  those taken from other families or from the families of a font rule,
  and those no font has, each drawn and counted; reopening it regenerates
  it. Only text boxes are visited, so the sized variants and assemblies of
  stretchable characters, which are drawn by other boxes, do not appear.

  <section|Mathematical symbols>

  Of the 2437 code points of the symbol list of the <LaTeX> package
  <verbatim|unicode-math>, about seven hundred had no <TeXmacs> name and
  could only be entered as <verbatim|\<less\>#<var|XXXX>\<gtr\>>, without
  palette, shortcut or <LaTeX> conversion. Two hundred of them, those with a
  free name whose glyph at least half of the shipped math fonts draw, are
  now named:

  <\itemize>
    <item><verbatim|TeXmacs/langs/encoding/tmuniversaltounicode-extra.scm>
    holds the names, generated by <verbatim|tests/opentype/missing-symbols.py>
    and reviewed; <verbatim|Data/String/converter.cpp> loads it next to
    <verbatim|tmuniversaltounicode> in both directions;

    <item><verbatim|progs/language/std-symbols.scm> gives each of them the
    class which determines its spacing;

    <item><verbatim|progs/convert/latex/latex-symbol-drd.scm> lists them in
    <verbatim|latex-unicodemath-symbol%>, so that a document using one
    exports with <verbatim|\\usepackage{unicode-math}>.
  </itemize>

  <verbatim|src/doc/math-symbol-coverage.md> counts the symbols which are
  still unnamed, block by block.

  <verbatim|TeXmacs/progs/math/math-symbol-tools.scm> arranges the symbols
  in groups declared with <scm|define-math-symbols-group>, which expands
  into one widget per width (sixteen columns for the window, four for a
  side tool), and offers them in a window (<scm|open-math-symbols>,
  <menu|All symbols...> in the mathematical insert menu) and in a side tool
  (<scm|open-math-symbols-tool>); the group shown by the side tool is an
  argument of the tool, since a refreshed widget of a dock keeps the state
  it was built with.

  Two changes make the buttons show what the typesetter can typeset. The
  symbol buttons of the palettes are drawn by <cpp|box_widget (p, s, col,
  trans, ink)> (<verbatim|Texmacs/Window/tm_button.cpp>), which for the
  mathematical font classes <verbatim|mr>, <verbatim|ms> and
  <verbatim|mt> now uses a smart font instead of <cpp|find_font>, so a
  symbol which lives only in a Unicode font is no longer blank. And in the
  <name|Qt> port (<verbatim|Plugins/Qt/qt_ui_element.cpp>) a button whose
  content is a <TeXmacs> box (a <cpp|simple_widget>) is drawn as the icon
  of its action, at the size hint of the box, as toolbar buttons are;
  before, such a button only reached the screen through a menu and stayed
  empty in a window.

  <section|Tests>

  The tests of the <name|OpenType> work need a built tree. The <c++> unit
  tests are run by the autotools harness, <verbatim|make -C tests> (see
  <hlink|building and testing|build-tests.en.tm>); two of them concern
  fonts:

  <\description-paragraphs>
    <item*|<verbatim|tests/Plugins/Freetype/tt_tools_test.cpp>>The readers
    of the <verbatim|MATH>, <verbatim|GSUB> and <verbatim|GPOS> tables.

    <item*|<verbatim|tests/Graphics/Fonts/opentype_font_test.cpp>>The
    constants, variants and assemblies of the fonts it finds, the
    corrections and kerning hooks, and the profiles: it reads
    <verbatim|fonts-opentype.scm>, checks every key and group, which profile
    first claims a shared text companion, and that each installed math
    font is an <name|OpenType> math font known to the database under its
    profile name.
  </description-paragraphs>

  These tests look for their fonts in the directory given by
  <verbatim|TM_TEST_FONT_DIR> and skip themselves without it, even for the
  fonts which are now shipped; pointing it at
  <verbatim|TeXmacs/fonts/truetype> runs all of them but those needing a
  font which is not shipped (<name|Asana Math>). The scripts of
  <verbatim|tests/opentype> do the rest:

  <\description-paragraphs>
    <item*|<verbatim|check.sh>>Runs the unit tests, renders the samples of
    <verbatim|tests/opentype/samples> with and without the hand-tuned
    tables (<verbatim|TM_HAND_TUNED=off> evaluates
    <scm|(set-hand-tuned-math-fonts #f)>), and fails if a <name|PDF> made by
    the native writer contains a <name|Type 3> (bitmap) font or a syntax
    error; with <verbatim|TM_UNICODE_MATH_TABLE> it also checks the symbol
    tables.

    <item*|<verbatim|render-samples.sh>>The renders, with a pixel diff
    against <verbatim|tests/build/ref> when that directory exists; a
    difference is reported, not a failure.

    <item*|<verbatim|compare-lualatex.sh>>Typesets the formulas of
    <verbatim|tests/opentype/compare> with <TeXmacs> and with
    <name|LuaLaTeX> and <verbatim|unicode-math> in the same font, and stacks
    the two renders.

    <item*|<verbatim|font-gallery.sh>>One specimen per installed profiled
    math font (asked from <TeXmacs> with <scm|opentype-math-font-list>), the
    images of <verbatim|src/OPENTYPEMATH.md>.

    <item*|<verbatim|missing-symbols.py>, <verbatim|survey-math-fonts.py>,
    <verbatim|assembly-report.py>>Tools which generated the symbol tables,
    surveyed the fonts and report on assemblies.
  </description-paragraphs>

  Without the native (<name|Hummus>) <name|PDF> writer every exported
  glyph is a bitmap, so the renders and the <name|Type 3> check depend on
  how the tree was configured.

  <section|Open problems>

  The list below was checked against the code; the design log
  <verbatim|src/doc/opentype-math-design.md> (sections 6 and 7) has the
  details and the history.

  <\itemize>
    <item><em|Alphabets.> No profile key declares the mathematical
    alphabets a font really provides, so an incomplete alphabet (script and
    double-struck in most fonts) is silently completed by emulated
    glyphs.

    <item><em|Constants not applied.> <verbatim|delimitedSubFormulaMinHeight>
    is left out on purpose (it would enlarge every automatic bracket), the
    device tables are parsed but never applied, the stack constants for
    <markup|above> and <markup|below> would move every such construct and
    are not used, and the italic correction of glyph assemblies is
    ignored; see <hlink|mathematics from the MATH table|opentype-math.en.tm>.

    <item><em|GPOS.> Only pair kerning of the <verbatim|kern> feature is
    read; mark attachment (<verbatim|mark>, <verbatim|mkmk>), cursive and
    contextual positioning are not, which matters for fonts placing
    combining accents by marks. Kerning is not applied across box
    boundaries, so two single letters of a formula in separate boxes stay
    unkerned (<hlink|OpenType features|opentype-features.en.tm>).

    <item><em|Profiles.> <verbatim|bold-math> is not read, and nothing
    checks that the companions a profile names exist
    (<hlink|profiles|opentype-profiles.en.tm>).

    <item><em|Bitmaps in PDF.> Six symbols only <TeXmacs> defines
    (<verbatim|triangleup>, <verbatim|blacktriangleup> and four
    <verbatim|nblacktriangle...>) and the alphabets emulated with
    <verbatim|unserif> are built with operations on pixels, which have no
    vector form, and export as small <name|Type 3> bitmaps when the font
    lacks them. Every other symbol an <name|OpenType> math font lacks is
    either constructed as vectors or, if its emulation would be a bitmap,
    taken from the shipped <name|STIX Two Math> (<cpp|resolve_shipped_math>).

    <item><em|Seams.> Assembled glyphs are glued on the measured ink of
    their parts, corrected to the advances of the table; a font whose parts
    have unusual side bearings could still show a seam
    (<hlink|stretchable glyphs|opentype-stretch.en.tm>).

    <item><em|Names.> Some choices still go by family name: the hand-tuned
    fonts (<cpp|tex_gyre_fix>, the <verbatim|stix> and <verbatim|agella>
    tests in the typesetter, which guard corrections that keep precedence),
    the traditional math families (<cpp|is_math_family>), and, for fonts
    without a <verbatim|MATH> table only, <cpp|supports_big_operators> in
    <verbatim|poor_rubber.cpp>.

    <item><em|Tests.> The renders are compared by hand; the font tests skip
    themselves unless <verbatim|TM_TEST_FONT_DIR> is set.
  </itemize>

  <section|Source files>

  <\description-paragraphs>
    <item*|<verbatim|Graphics/Fonts/smart_font.cpp>><cpp|debug_origin>,
    <cpp|debug_draw>, <cpp|debug_info>, <cpp|smart_font_debug_info>,
    <cpp|resolve_shipped_math>.

    <item*|<verbatim|Typeset/Boxes/Basic/font_debug_boxes.cpp>>From boxes
    to characters: <cpp|box_font_debug_info>,
    <cpp|box_font_debug_report>.

    <item*|<verbatim|Texmacs/Data/new_view.cpp>,
    <verbatim|Edit/Editor/edit_main.cpp>>The glue entry points and the
    access to the boxes of an editor.

    <item*|<verbatim|TeXmacs/progs/fonts/font-debug.scm>>The inspector, the
    colouring toggle and the <verbatim|fontdbg> handlers.

    <item*|<verbatim|TeXmacs/progs/math/math-symbol-tools.scm>>The symbol
    window and side tool.

    <item*|<verbatim|Texmacs/Window/tm_button.cpp>,
    <verbatim|Plugins/Qt/qt_ui_element.cpp>>Symbol buttons.

    <item*|<verbatim|tests/opentype/>, <verbatim|tests/Graphics/Fonts/>,
    <verbatim|tests/Plugins/Freetype/>>The tests.
  </description-paragraphs>

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
