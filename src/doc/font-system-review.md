# The TeXmacs font system: a review

This document describes how fonts work in TeXmacs, from the abstract `font`
interface used by the typesetter down to the FreeType and Metafont backends
that rasterize glyphs. It is meant as an orientation for people who want to
modify the font code, and as background for the OpenType MATH work described
in `opentype-math-design.md`.

Paths are relative to the repository root. Line numbers refer to the
`wip_opentype` branch and will drift.

## 1. Where the code lives

| Area | Location | Role |
|---|---|---|
| Abstract fonts | `src/Graphics/Fonts/` | `font` interface, selection, database, smart fonts, virtual fonts, "poor" derived fonts, rubber fonts, microtypography |
| Bitmap glyph layer | `src/Graphics/Bitmap_fonts/` | `glyph`, `font_metric`, `font_glyphs`, glyph algebra (`glyph_ops.cpp`, transforms, analysis) |
| FreeType backend | `src/Plugins/Freetype/` | TrueType/OpenType loading, Unicode fonts, per-family adjustment tables, rubber fonts for Unicode fonts, font analysis, MATH table parser |
| Metafont backend | `src/Plugins/Metafont/` | TFM/PK loading, TeX text and rubber fonts |
| GUI fonts | `src/Plugins/Qt/qt_font.cpp`, X11 plugin | System fonts through the toolkit (rarely used for documents) |
| Renderers | `src/Graphics/Renderer/` | `renderer::draw (int char_code, font_glyphs, x, y)` and friends |
| Typesetter | `src/Typeset/` | Consumes fonts: `env_semantics.cpp` picks the current font, `Boxes/` and `Concat/` build boxes from glyphs |
| Data | `TeXmacs/fonts/` | `font-database.scm`, `font-features.scm`, `font-characteristics.scm`, `font-substitutions.scm`, `enc/` translators, `virtual/` virtual fonts, `truetype/`, `type1/`, `tfm/` |
| Scheme side | `TeXmacs/progs/fonts/` | Font rules for the legacy TeX font scheme, menus and widgets, the math font profiles (`fonts-opentype.scm`) |

## 2. Core abstractions

### 2.1 `font` and `font_rep` (`src/Graphics/Fonts/font.hpp`)

A `font` is a reference-counted *resource*: every font has a unique string
name and `font::instances` maps names to live objects. The macro
`make (font, name, tm_new<...>)` returns the existing instance when one
exists, otherwise creates it. Names are built by concatenating the defining
parameters, so a font is fully identified by its name. This is the only
caching mechanism and it means that two different constructions that produce
the same name silently share one object.

`font_rep` is an abstract class. The key virtuals are:

- `supports (c)`: does the font have a glyph for character `c`.
- `get_extents (s, ex)`: metric of a string.
- `get_xpositions (s, xpos)`: cursor positions inside a string.
- `draw_fixed (ren, s, x, y)`: draw a string.
- `magnify (zoomx, zoomy)`: return a scaled font.
- `advance_glyph`, `get_glyph`, `index_glyph`: glyph-level access, used by
  the glyph algebra and by the PDF/PostScript exporters.
- Microtypographic hooks: `get_left_slope`, `get_right_slope`,
  `get_left_correction`, `get_right_correction`, `get_lsub_correction`,
  `get_lsup_correction`, `get_rsub_correction`, `get_rsup_correction`,
  `get_wide_correction`, `get_left_protrusion`, `get_right_protrusion`, and
  the height-aware forms `get_lsub_correction_at`, `get_lsup_correction_at`,
  `get_rsub_correction_at` and `get_rsup_correction_at`, which the OpenType
  cut-in kerning needs.
- Since the OpenType branch: `make_rubber_font (base)`, which lets a font
  decide which rubber font implementation serves it, and the queries
  `get_rubber_variant (s, height, r)`, `get_wide_variant (s, width, r)`,
  `get_top_accent (s, x)`, `is_extended_shape (s)` and
  `get_feature_variant (s, feature, alt, r)`, which let the typesetter ask
  the font instead of probing it.

Every font also carries a set of numeric parameters, computed heuristically
by each backend from a handful of glyphs:

| Field | Meaning | Typical derivation (TrueType) |
|---|---|---|
| `y1`, `y2` | descent and ascent | extents of `f`, `p` and `d` together |
| `yx` | x-height | extents of `x` |
| `yfrac` | fraction bar height (math axis) | middle of `<#2212>`, else of `+`, `-` or `x` (raised by `wline/4` without `<#2212>`), then `axisHeight` for a MATH font |
| `ysub_*`, `ysup_*`, `yshift` | script placement limits | fractions of `yx` |
| `wpt`, `hpt` | size of one point in `SI` units | from dpi |
| `wfn` | design size in `SI` | `wpt * size` |
| `wline` | rule thickness | `wfn/20`, then the height of `<#2212>`, `+` or `-` (capped by the en dash without `<#2212>`) clamped to `[wfn/48, wfn/8]`, then `fractionRuleThickness` for a MATH font |
| `wquad` | quad | width of `M` |
| `spc`, `extra`, `mspc`, `sep` | spacing | width of space, `wfn/10` |
| `slope` | italic slope | ink overhang of `f` |
| `type` | `FONT_TYPE_TEX`, `FONT_TYPE_UNICODE`, `FONT_TYPE_QT`, ... | |
| `math_type` | `MATH_TYPE_NORMAL`, `MATH_TYPE_STIX`, `MATH_TYPE_TEX_GYRE`, `MATH_TYPE_OPENTYPE` | from the font name prefix, or set by the Unicode font constructor |

Plus the microtypography tables: `lsub_correct`, `lsup_correct`,
`rsub_correct`, `rsup_correct`, `above_correct`, `below_correct`
(per-character hash maps of doubles, filled by `font_scripts.cpp` and by the
per-family `adjust_*.cpp` files), protrusion maps, spacing tables, and since
the OpenType branch a block of MATH constants for fractions, radicals and
limits.

Derived fonts inherit these values via `copy_math_pars (fn)`.

### 2.2 Metrics and units

All positions are `SI` integers, with `PIXEL` units per device pixel at the
current dpi. A `metric` has a *logical* box `(x1, y1, x2, y2)` (origin at 0,
`x2` is the advance) and an *ink* box `(x3, y3, x4, y4)`. Composite fonts
combine metrics by taking the union of both boxes.

### 2.3 Strings and character names

Fonts operate on TeXmacs strings, not on code points. A string mixes bytes in
the legacy Cork-like encoding with named entities in angle brackets:
`<alpha>`, `<big-sum-2>`, `<left-(-3>`, `<#2A0C>` (explicit Unicode),
`<b-x>` (bold letter), `<it-a>` (italic letter), and, new on this branch,
`<@1F3A>` (native glyph id). Every layer of the font stack pattern-matches
these names; there is no intermediate "glyph run" representation.

### 2.4 Bitmap glyph layer (`src/Graphics/Bitmap_fonts/`)

Below `font_rep` sits a purely bitmap model:

- `glyph`: a raster with `width`, `height`, offsets `xoff`, `yoff`, logical
  width `lwidth`, a `depth` (bits per pixel, 1 for TrueType output), and an
  `index` used by exporters to find the glyph in the original font file.
- `font_metric_rep`: `exists (code)`, `get (code)` returning a `metric`, and
  `kerning (left, right)`.
- `font_glyphs_rep`: `get (code)` returning a `glyph`.

Both are resources cached by name. `glyph_ops.cpp` and `glyph_transforms.cpp`
provide an algebra on rasters (join, intersect, clip, flip, rotate,
`hor_extend`, `ver_extend`, `hor_take`, `ver_take`, `bolden`, `slanted`,
`stretched`, `make_bbb`, `unserif`, ...). This algebra is what makes virtual
fonts and the "poor" fonts possible, and it is also why the pipeline is
fundamentally bitmap based: a renderer receives a `font_glyphs` and a
character code and draws the raster. Outline information is used only at
export time (the PDF renderer re-embeds the original font using
`glyph->index`), and by the vector drawing path in `virtual_font.cpp`.

## 3. Physical font backends

### 3.1 TeX / Metafont fonts (`src/Plugins/Metafont/`)

The historical backend. `tex_font` loads TFM metrics and PK bitmaps
(generating them with `mktexpk` when needed). Character names are mapped to
positions through *translators* (`translator.hpp`, in
`src/Graphics/Fonts/`), loaded from the `TeXmacs/fonts/enc/*.enc` files.
`tex_rubber_font` implements TeX's extensible delimiters from the `cmex`
pieces. A *math font* (`math_font.cpp`, also in `src/Graphics/Fonts/`) is a
compound of many TeX fonts (roman, italic, symbols, AMS, stmaryrd, wasy, ...)
described by Scheme rules in `TeXmacs/progs/fonts/fonts-math.scm` and looked
up through `find_font (scheme_tree)` in `find_font.cpp`, which dispatches on
tuples such as `(tex ...)`, `(cm ...)`, `(ec ...)`, `(math ...)`,
`(tex-rubber ...)`, `(compound ...)`, `(unicode ...)`, `(unimath ...)`.

This path is still what the default "roman" (Computer Modern) font uses when
`new_fonts` is off, and it defines the reference behaviour that everything
else imitates.

### 3.2 FreeType fonts (`src/Plugins/Freetype/`)

FreeType is loaded dynamically (`free_type.cpp`) or linked; only a small set
of entry points is used: new memory face, select charmap, set char size, get
char index, load glyph, render glyph, get kerning.

- `tt_file.cpp`: locates font files. `tt_font_find_sub` tries the sfnt
  formats first, `.otf`, `.ttf` and `.ttc`, then Type 1 `.pfb` and
  `.dfont`; a TeX distribution ships many families in both forms and the
  Type 1 file carries a TeX encoding rather than a Unicode cmap.
  `tt_font_path ()` concatenates
  `$TEXMACS_FONT_PATH`, the "imported fonts" preference,
  `$TEXMACS_HOME_PATH/fonts/truetype`, `$TEXMACS_PATH/fonts/truetype`, and
  platform system directories. On macOS `texlive_font_dirs` scans
  `/usr/local/texlive`, `/usr/share/texlive`, `/opt/texlive` and
  `$HOME/texlive` for any year; the Linux branch still lists 2020, 2021 and
  2022 by hand. Located files are cached persistently in `font_cache.scm`;
  the `tt_fonts` hash map caches only the existence flag.
- `tt_face.cpp`: `tt_face` reads the whole file into memory and creates an
  `FT_Face`. `tt_font_metric_rep` and `tt_font_glyphs_rep` render each glyph
  on demand in *monochrome* mode at the requested size and dpi, and cache the
  result per code. Kerning comes from the GPOS `kern` feature when the font
  has one, through the reader in `tt_tools.cpp`, and falls back to
  `FT_Get_Kerning`, that is to the legacy `kern` table.
- `tt_font.cpp`: a minimal font on top of the two caches, used for
  `(truetype ...)` tuples.
- `unicode_font.cpp`: the workhorse for modern fonts. It parses `<#XXXX>`
  and named entities via `strict_cork_to_utf8`, supports a `native` table for
  glyphs without Unicode code points (`tex_gyre_operators`) and decodes
  `<@XXXX>` glyph numbers in `get_glyphID`, implements the f-ligatures
  itself, and hosts the per-family
  microtypography: the constructor contains an `if / else if` ladder on the
  family name that installs hand-tuned correction tables for STIX, TeX Gyre
  (Termes, Pagella, Schola, Bonum), Papyrus, Libertine, Biolinum and Fira,
  with data in `adjust_*.cpp`. The OpenType MATH support is read by
  `init_ot_math`, which runs *before* that ladder; each branch of the ladder
  then overrides the fields it tunes, and the branches are skipped for a font
  with a MATH table when the user switches the hand tuning off.
- `unicode_math_font.cpp`: the older `(unimath up it bup bit rubber)`
  compound, superseded by smart fonts.
- `rubber_unicode_font.cpp`, `rubber_assemble_font.cpp`,
  `rubber_stix_font.cpp`: stretchable characters, see section 7.
- `tt_analyze.cpp`: computes the "characteristics" stored in the font
  database (serif/sans, mono, slant, x-height, stroke widths, fill rate,
  supported Unicode ranges) by analysing rendered glyph bitmaps.

### 3.3 Toolkit fonts

`qt_font.cpp` and the X11 equivalent wrap system fonts. They exist for the
GUI and for fallback; documents essentially always go through FreeType.

## 4. Font database and selection

### 4.1 The database (`font_database.cpp`)

Four Scheme files. The first three exist in a global
(`$TEXMACS_PATH/fonts/`) and a local (`$TEXMACS_HOME_PATH/fonts/`) version;
the substitutions are only global:

- `font-database.scm`: `((Family Style) ((file.ttf index size) ...))`.
- `font-features.scm`: `(Family Master Feature ...)`, mapping a family to
  its *master* family and a list of features such as `bold`, `italic`,
  `sansserif`, `mono`, `smallcaps`.
- `font-characteristics.scm`: per style, the measured attributes from
  `tt_analyze` (`mono=no sans=no slant=0 ex=67 em=243 lvw=4 ...`) plus the
  Unicode ranges covered.
- `font-substitutions.scm`: family-level substitutions
  (`((FandolSong sansserif) (FandolHei))`).

The local database is built by scanning the font path
(`font_database_build*`) and saved in full. A separate developer routine,
`font_database_save_local_delta`, writes the delta with respect to the global
database to `delta-*.scm`, which is how new fonts are contributed. Loading is
lazy, and the global database is loaded as a fallback when a family is
missing; on this branch `font_database_master` answers the same question for
one family (loading the global database still prints its one warning,
"missing font, loading global substitution list").

The local database also has to follow the shipped one, which grows with the
fonts a new version registers. Its own date says nothing, since it is saved
again on every scan, so `font_database_load` keeps a stamp in
`$TEXMACS_HOME_PATH/fonts/shipped-stamp.scm` — the date and the size of the
three shipped files and the installation (`$TEXMACS_PATH`), with the number
of entries of the local database on a second line — and merges the shipped
entries, filtered against the files really present, whenever the stamp
differs or the local database has fewer entries than it had: several
installations may share one home directory, and each must find its own
fonts. An old home directory
otherwise hides a new font for good, and the fallback search of the smart
font then finds no font at all for a character only that font draws, which
the error font shows as the name of the character in red.

### 4.2 Logical fonts and features (`font_select.cpp`, `font_translate.cpp`, `font_guess.cpp`)

TeXmacs describes a requested font as a *logical font*: an array of strings
whose first element is the family and the rest are normalized features
(`bold`, `italic`, `smallcaps`, `condensed`, `wide`, `sansserif`, `mono`,
...). `logical_font (family, variant, series, shape)` (`font_translate.cpp`)
translates the four document environment variables into such an array. `search_font` then
computes a distance between the requested features and every style of every
candidate family (with heuristics in `distance`): strictly when an exact
match is required or no feature is given, otherwise after dropping unknown
features, and finally strictly again among the families of the winning
master; `guessed_distance` between families (`font_guess.cpp`) only breaks
ties. The search stays inside the master of
the request unless the best distance there reaches `D_MASTER` (10000), and
then looks at every family; a mismatch of serif and sans serif costs
`D_SERIF` (100000), more than leaving the master. So a sans serif
typewriter face inside a serif master (KpMono under Kepler) loses to a
serif typewriter of another master, which is why the shipped database gives
KpMono a master of its own, Kepler Mono, and does the same for the sans
serif math fonts KpMathSans and NewComputerModernSansMath. The `attempt` argument selects successively worse
matches, which the smart font uses to find fallback fonts for characters the
main font lacks.

`font_translate.cpp` maps the legacy names (`roman`, `pagella`, `stix`,
`cyrillic`, ...) to the new naming scheme and back.

## 5. Smart fonts: the entry point for the typesetter

`edit_env_rep::make_current_font` (`src/Typeset/Env/env_semantics.cpp`)
creates the current font from environment variables, and `update_font`
calls it and then applies the MATH script sizes, `ssty`, the features and
the effects:

- text mode: `smart_font (font, font-family, font-series, font-shape, size, dpi)`
- math mode: `smart_font (math-font, math-font-family, math-font-series, math-font-shape, font, font-family, font-series, "mathitalic", size, dpi)`
- program mode: the `prog-*` variables, with `font` and `font-family` plus
  `-tt` as the text arguments.

The math and program overload builds the font from the *text* arguments:
`math-font` (or `prog-font`) is used only when `font` is `roman`, `ms` and
`mt` turn into the `ss` and `tt` variants, a math series other than medium
overrides the text series, and the shape `right` becomes `mathupright`.
With any other text font, `profile_fix` turns the text family into its math
companion. A text font set with another math font than its own must
therefore be written as a rule, `math=Euler Math,TeX Gyre Pagella`, which
is what `init-opentype-font` in `fonts-opentype.scm` does.

The size passed is already the script size for the current index level:
`get_script_size` divides by 1.5 per level, at most twice, unless the
document sets `math-font-sizes`. A font with a MATH table overrides that with
`scriptPercentScaleDown` and `scriptScriptPercentScaleDown` when
`math-font-sizes` has its default value, and at script levels a font of
type `MATH_TYPE_OPENTYPE` (not a hand-tuned one) is wrapped in `feature_font
(fn, "ssty", ...)` so that the script-size alternates of the font are used.

`smart_font_bis` builds the name, applies family fix-ups (`tex_gyre_fix`,
`kepler_fix`, `math_fix`, `profile_fix` and the `sys-*` CJK defaults), picks
the closest physical font as `fn[SUBFONT_MAIN]` and a sans-serif error font,
and creates a `smart_font_rep` (`smart_font.cpp`). `profile_fix` is the
OpenType math part: it swaps a text family for its math companion in math
shapes and back in text shapes, and in text as in math it sends the `ss`
and `tt` variants to the `sans` and `mono` companions declared in
`math_font_profiles.cpp`, which `TeXmacs/progs/fonts/fonts-opentype.scm`
fills at boot with `define-math-font-profile` (keys `file`, `text`,
`text-file`, `sans`, `mono`, `family`, `letters`, `bold-math`, `menu`,
`group`; the first profile naming a text family is the one that family
pulls in). A text family is swapped for its math companion only when that
font is installed, and every plain family name it produces is replaced by
its master; items with conditions, such as `math=...`, are left alone. A profiled
math font missing from the database, and the directory of its `text-file`
whenever the text companion is missing, is added to the local database once
per session (`register_profiled_font`).

### 5.1 Family syntax

The `font` environment variable is a comma separated *font sequence*. Each
item is either a family name or `conditions=family`, where conditions are
space separated and each condition is an alternative list separated by `|`.
Conditions can be Unicode ranges (`greek`, `cyrillic`, `cjk`, `mathsymbols`,
...), pseudo ranges (`mathlarge`, `mathbigop`, `mathrubber`), single
characters, character collections, code point ranges `A:Z`, logical features,
or `math`. Examples: `cjk=Songti SC,roman`,
`math=TeX Gyre Pagella Math,Linux Libertine`.

The `math` condition is special: it is not one of the conditions the
per-character resolver knows. `math_fix`, which runs before resolution,
strips it from an item in a math shape, so the item applies; in a text
shape it leaves the item alone, and the resolver never satisfies `math`. The resolver's own conditions are the logical
font's features, the Unicode ranges (`ascii`, `latin`, `greek`, `cyrillic`,
`cjk`, `hiragana`, `hangul`, `mathsymbols`, `mathextra`, `mathletters`), the
pseudo ranges `mathlarge`, `mathbigop` and `mathrubber`, the character
collections (`digit`, `latin`, `greek`, `basic-letters` and their case and
bold variants), the alphabet names that `substitute_math_letter` gives a
letter (`bold-math`, `cal`, `frak`, `bbb`, ...), a literal character and a
code point range `A:Z`. A negated collection, `!latin`, accepts the characters
outside the collection (until 27 September 2026 the resolver looked up a
collection named with the `!` itself, found none, and accepted every
character). The negation knows only the collections of `init_collections`:
`!cjk` or `!cyrillic` name no collection and are always satisfied, and a
letter such as `e` with acute accent satisfies both `latin`, by its Unicode
range, and `!latin`, since it is in no collection of that name.

### 5.2 Per-character resolution

The smart font splits every string into runs that live in the same subfont
(`advance`). Routing is cached in a `smart_map` shared by all sizes of the
same logical font: a 256-entry vector for single bytes and a hash map for
`<...>` entities. On a miss, `resolve (c)` walks the font sequence and, for
each family, tries in order:

1. the main font, if it supports the character;
2. the Greek companion font;
3. math families (`is_math_family`) through `REWRITE_MATH`;
4. the Cyrillic companion font;
5. "poor" constructions such as `poor-bbb` for blackboard bold and `it`
   for slanting;
6. emulated symbols from the `emu-*.vfn` virtual fonts;
7. on later attempts, other families supporting the Unicode range of the
   character, at a dpi adjusted so that x-heights match (`adjusted_dpi`).

`poor-bold` is not part of this ladder: the outer loop uses it for
`<wide-...>` names in a bold series.

Math shapes add many subfonts up front (`fast-italic`, `special`,
`emu-bracket`, `regular`, `bold-math`, `cal`, `frak`, `bbb`, `tt`, `ss`, ...)
and rewrite Unicode math alphanumerics (`substitute_math_letter`) into
`<b-x>`, `<cal-A>` style names or into the corresponding Unicode plane 1
code points depending on what the font offers.

Rubber characters (`<left-...>`, `<mid-...>`, `<right-...>`, `<large-...>`;
`is_rubber` does not count `<big-...>`) are routed by `resolve_rubber`, which
an OpenType math font also tries first for `<wide-...>` and `<rubber-...>`: the base delimiter is resolved
first, and then a `rubber` subfont wrapping the font that provided it is
created with `rubber_font (fn)`.

A loop detector fails hard when a substitution resolves to the smart font
itself.

### 5.3 Outside the typesetter: menus and widgets

The typesetter is not the only thing that draws text. A menu label, a
toolbar and the buttons of a palette are laid out by the GUI, and they take
their font from `find_font (family, class, series, shape, sz, dpi)` in
`find_font.cpp`, the selection that predates the smart fonts: the rules of
`TeXmacs/progs/fonts/*.scm` turn the request into a tuple such as
`(math-std ecrm cmr cmmi 10 600)` and the result is a compound of TeX
fonts. `box_widget` in `src/Texmacs/Window/tm_button.cpp` builds the box a
symbol button shows this way, with `roman`, `mr` as its default family and
class, so a palette could only ever show what the TeX fonts happen to
carry, and a symbol that lives in a Unicode font alone came out blank. On
this branch that call goes to the smart font for the mathematical classes,
which is the same font the typesetter would use for the symbol.

The font menus are Scheme (`generic/document-menu.scm`,
`fonts/fonts-opentype.scm`, `fonts/font-short-menu.scm`), and two
properties of the menu language shape them. A command's `:check-mark` is
applied by `make-menu-entry-check` (`kernel/gui/menu-widget.scm`) to the
arguments as written in the menu, recovered with `promise-source`, not to
their values: an entry built in a loop, `(init-opentype-font (cadr p))`,
hands its predicate the list `(cadr p)`, so such entries state their check
with `(check label "*" predicate)`, evaluated where the loop variable is
bound. And the contents of a submenu are built when it is opened, after
the menu that holds it, so a menu with arguments inside a submenu has lost
them by then ("widget expected"): each submenu is a menu without
arguments.

## 6. Virtual fonts (`virtual_font.cpp`, `TeXmacs/fonts/virtual/*.vfn`)

A virtual font builds glyphs from Scheme expressions over other glyphs. A
definition looks like

```scheme
(virtual-font
  (minus (align minus* + 0.5 0.5))
  (slash (or (font minus Baskerville Cuprum) /)))
```

The evaluator (`compile_bis`) supports about seventy-five primitives: glyph
references, `glue`, `glue*`, `glue-above`, `glue-below`, `join`, `intersect`,
`exclude`, `align`, `magnify`, `scale`, `hor-extend`, `ver-extend`,
`hor-take`, `ver-take`, `hor-flip`, `rotate`, `crop` variants, `bar-*`,
`curly`, `unserif`, `circle`, `reslash`, `or` (first alternative that
exists), `font` (require a family), `with` (local bindings), and pen width
queries. Names of the form `<name-#>` are templates where `#` is replaced by
a variant number at lookup time (`subst_sharp`), which is how a single
definition yields a family of rubber sizes.

Virtual fonts are used for emulated symbols missing from a font
(`emu-fundamental`, `emu-greek`, `emu-operators`, `emu-arrows`, ...) and for
assembling large delimiters (`emu-large`, `emu-alt-large`). There is also a
direct vector drawing path (`draw_tree`) for high resolution output.

The dictionary of a virtual font is a `translator` (`translator.hpp`): a
`dict` from names to indices and an array `virt_def` of definitions. The
OpenType branch creates such translators at runtime instead of loading them
from `.vfn` files.

## 7. Rubber (stretchable) characters

### 7.1 Naming

The typesetter asks a font for a named glyph with a size suffix, and, where
the font cannot answer for a height, searches for the smallest that fits:

- `get_delimiter (s, fn, height)` in `src/Typeset/Boxes/Basic/text_boxes.cpp`
  first asks the font, through `get_rubber_variant`, for the name that reaches
  the height; a font with a MATH table answers. Otherwise it tries
  `<left-(-0>`, `<left-(-1>`, ... until the extents are tall enough or a
  credit of about twenty attempts is exhausted.
- `big_operator_box` uses `<big-sum-1>` in text style and `<big-sum-2>` in
  display style.
- `wide_box` and `wide_stix_box` do the same horizontally for accents and
  braces (`<wide-hat-N>`, `<rubber-overbrace-N>`).

### 7.2 Implementations

`rubber_font (base)` (`font.cpp`) caches one rubber font per base font, built
by `base->make_rubber_font (base)`. The default `font_rep` implementation
dispatches on the font name:

| Condition | Implementation | How it stretches |
|---|---|---|
| a Unicode font with a MATH table | `rubber_unicode_font` (`unicode_font.cpp`) | the variants and assemblies of the table |
| name mentions `stix`, while the hand tuning is on | `rubber_stix_font` | STIX's own size variants and assembly pieces, hard-coded glyph names |
| `mathlarge=` or `mathrubber=` in the name | the font itself | the smart font routes rubber names to a dedicated family |
| Unicode font and `has_poor_rubber` (default true) | `poor_rubber_font` | five magnification levels built with `poor_stretched_font`, each in a normal and a narrow variant, then the `emu-large` virtual assembly for larger sizes; big operators are served by `rubber_unicode_font` |
| other Unicode fonts | `rubber_unicode_font` | magnified base glyphs (`sqrt 0.5`, `sqrt 2`, `2`) and `rubber_assemble_font`, which glues the Unicode bracket pieces U+239B..U+23B7 via `emu-alt-large.vfn` |
| anything else, TeX fonts included | the font itself | TeX rubber fonts are not built here: they come from the `(tex-rubber ...)` tuples of `find_font.cpp` |

`supports_big_operators` decides whether a font's own big operators are used
or magnified small ones; on this branch a font with a MATH table always has
them, and the family-name tests are left for the fonts that have none. The
OpenType branch adds a mode where the MATH table's variants and assemblies
drive the whole construction (see the design document).

## 8. Math typesetting parameters

The math boxes in `src/Typeset/Boxes/Composite/math_boxes.cpp` and
`script_boxes.cpp`, and the concatenation code in
`src/Typeset/Concat/concat_math.cpp`, position fractions, radicals, scripts,
limits and wide accents using the generic font parameters (`yfrac`, `sep`,
`wline`, `yx`, `ysub_*`, `ysup_*`) plus the microtypography hooks. On this
branch they read the OpenType MATH constants directly whenever `fn->ot_math`
is set: the fraction shifts and gaps, the radical gaps, the limit and stretch
stack constants, the script gaps and drops, and the bar constants. On top of
that there are explicit special cases keyed on `math_type` or on substrings
of the font name (`starts (locase_all (fn->res_name), "stix-")`, guarded by
the hand-tuning switch, and `occurs ("agella", fn->res_name)`), for example
the sqrt index position, the spacing after integrals, and which wide accents
to emulate. These special cases are the
practical reason for the OpenType MATH work: they encode, by hand, what an
OpenType math font already declares.

## 9. Assessment

Strengths:

- The abstract `font` interface decouples the typesetter from the backend and
  has allowed TeX bitmap fonts, TrueType, and toolkit fonts to coexist for
  twenty years.
- The bitmap glyph algebra plus virtual fonts give TeXmacs a robust fallback
  story: almost any text font can be used for mathematics, with missing
  symbols emulated and delimiters stretched.
- Font selection by features and distance, with analysed characteristics, is
  more forgiving than fontconfig-style matching and is portable.
- Smart fonts route per character and cache aggressively; performance in
  practice is good.

Weaknesses and technical debt:

- Glyphs are monochrome bitmaps rasterized per size and dpi. Anti-aliasing
  and vector output are handled downstream, and all glyph manipulation is
  raster manipulation.
- OpenType layout is read for single and alternate substitutions (the
  `dtls`, `flac` and `ssty` of the math work, and any feature a document names
  in `font-features`, `onum` or `ss01` for instance) and for the pair kerning
  of GPOS. There is no contextual or chained substitution,
  no mark attachment, no ligature support beyond the built-in f
  ligatures, and no complex-script shaping.
- Character addressing by string names means every layer re-parses names, and
  glyphs without Unicode code points need ad hoc escapes (`native`, `<@XXXX>`).
- Font parameters are estimated from a few glyphs (`x`, `M`, `-`, `f`) and
  per-family corrections are hard-coded C++ tables selected by family name
  prefix. Adding a font family with good math typography means adding an
  `adjust_*.cpp` file.
- Family-name string matching is scattered across `font.cpp`, `smart_font.cpp`,
  `poor_rubber.cpp`, `concat_math.cpp` and `math_boxes.cpp`, so the same font
  can be treated inconsistently depending on how its name was spelled.
- Rubber sizing is a search over integer variant numbers with heuristics for
  what each number means in each implementation. A font with a MATH table now
  answers `get_rubber_variant` and `get_wide_variant` with the variant that
  reaches a target size, but for every other font nothing tells the
  typesetter the set of available sizes.
- Caching by concatenated names is fragile: two constructors producing the
  same name share an object; the rubber Unicode fonts are kept apart only by
  their prefixes, `rubberunicode[...]` and `rubberunicode-ot[...]`.
- Font knowledge is spread over C++ tables, the Scheme font database and now
  the math font profiles; nothing checks that they agree, beyond the profile
  test.
