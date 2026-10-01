# Variable fonts

A variable font holds a continuum of designs in one file: weights, widths,
optical sizes, grades. Its `fvar` table lists *named instances* (Thin, Bold,
Condensed Black...), each a point on the axes. Before this branch TeXmacs
saw only the default instance of such a file (SF NS Regular, New York
Regular, Junicode VF Regular) and emulated bold and the other weights.

![Weights, widths and optical sizes of the macOS system font, and numeric
series on a static family](variable-fonts/specimen.png)

The specimen above is [`variable-fonts/specimen.tm`](variable-fonts/specimen.tm),
typeset by TeXmacs and exported to PDF, with SF NS, the variable system font
of macOS (weight 1 to 1000, width 30 to 150, optical size 17 to 96).

## Using it

- **Named styles.** After a scan of the fonts (`Tools > Fonts > Scan disk
  for fonts`), the named instances are styles like any other: SF NS shows
  up as the family `System Font` with Thin to Black in nine widths, and
  `font-series` `bold` picks the real Bold instead of an emulated one.
- **Any weight.** `font-series` accepts a number from 1 to 1000:
  `<with|font-series|550|...>`.
- **Any point.** `font-variations` sets the axes:
  `<with|font-variations|wght=550,wdth=87.5|...>`, and `opsz=auto` gives
  the optical size of the current font size.
- **The panel.** `Format > Font variations...` (for the selection) and
  `Document > Font > Variations...` (for the whole document) list the axes
  of the font at the cursor with their ranges and the values in force.

A home directory whose font database was made before this branch (or comes
from the database shipped with TeXmacs, built on another machine) does not
know the named instances, and may not know a variable family under its
current name at all: SF NS is `Sf ns` in the shipped database, and a
document asking for `System Font` then falls back to another font. A scan of
the fonts fixes both.

## What TeXmacs does now

Each named instance of a TrueType variable font becomes a font of its own,
the way each subfont of a `.ttc` collection already was.

- **Database.** `font_database_build` asks `tt_font_instances` for the
  instances of every file it scans, and registers each one as the subfont
  `v<k>` of the file (k counted from 1, as FreeType numbers named
  instances):

  ```scheme
  ((System\ Font Bold) ((SFNS.ttf v236 8330740)))
  ```

  The family is the one of the default instance, the style the subfamily
  name of the instance, normalized as for any other font.
- **Which instances.** Only those which differ from the default in weight,
  width or slant (`wght`, `wdth`, `ital`, `slnt`). Instances set apart by an
  optical size or a grade are left out: SF NS has 369 named instances, of
  which 81 are the nine weights in nine widths. Instances whose name
  collides with the default or another instance are left out too.
- **Static fonts.** `font_file_name` maps the entry to the file name
  `SFNS.v236.ttf`, and `tt_unpack` writes that file in
  `$TEXMACS_HOME_PATH/fonts/unpacked` the first time it is needed, from
  `tt_make_instance` (`Plugins/Freetype/tt_instance.cpp`). It is written
  again when the variable font is newer than it. The rest of TeXmacs
  (metrics, rendering, the OpenType tables, the PDF writer) handles it like
  any other TrueType font.
- **Analysis.** The characteristics of an instance are measured on its
  static font, which is then removed again unless it already existed:
  scanning the fonts of a Mac does not leave hundreds of megabytes behind.
- **Precedence.** When a static font and an instance have the same family
  and style (STIX Two Text SemiBold is both `STIXTwoText-SemiBold.otf`,
  shipped with TeXmacs, and an instance of the system's `STIXTwoText.ttf`),
  `font_database_search` lists the static font first, so that documents do
  not change.
- **Rescanning.** Files already in the database are skipped by name and
  size. A variable font which was scanned by an older TeXmacs, and so has
  only its default instance there, is recognized from its table directory
  (`tt_file_is_variable`, which reads a few hundred bytes) and scanned
  again.

## Arbitrary points: `font-variations`

The environment variable `font-variations` holds a comma separated list of
axes and values, as in `wght=550,wdth=87.5,opsz=auto`. It applies on top of
the font which `font`, `font-family`, `font-series` and `font-shape`
select.

- **Naming.** A point of the design space is a font named after the file
  and the coordinates which differ from the default:
  `SFNS.var_wdth60_wght700` (values rounded to a tenth, a minus sign
  written `m`, a decimal point `p`). `tt_unpack` writes it like a named
  instance, with `tt_make_variation`, which sets the coordinates through
  `FT_Set_Var_Design_Coordinates` instead of opening a named instance.
- **Resolution.** `tt_variation_name (name, spec, sz)` starts from the
  coordinates of the font the database found (the default, a named
  instance `v<k>`, or another point), replaces those the spec names,
  clamps them to the ranges of the axes, and gives the canonical name.
  `opsz=auto` takes the size in points. Only the `fvar` table is read,
  once per file, from the table directory. A font which is not variable,
  or a spec which changes nothing, gives the name back.
- **Where it is applied.** In `find_font`, where a file of the font
  database becomes a `unicode_font`. The value is a context,
  `get_font_variations`, set with `font_variations_scope` by
  `make_current_font`. It is part of the cache keys of `find_font`,
  `closest_font` and `smart_font_bis`, and a smart font keeps the value it
  was made with and sets it again whenever it looks up a subfont later
  (fallbacks, math alphabets, magnification).
- **Scope.** The variations reach every variable font the smart font uses,
  its fallbacks included, so that a character taken from another variable
  font gets the same weight. Static fonts are left alone.

## Weights as series

`font-series` may be a number from 1 to 1000. `make_current_font`
(`Typeset/Env/env_semantics.cpp`) turns it into the series with the nearest
name (thin, extralight, light, medium, semibold, bold, extrabold, black),
which selects the style and, for a static family, is the nearest weight
there is, and puts `wght=<number>` in front of `font-variations`, so that
a variable font takes the weight exactly and an explicit `wght` in
`font-variations` still wins. Coordinates which are those of a named
instance resolve to that instance (`SFNS.v236` rather than
`SFNS.var_wght700`), so no second copy is written.

## The panel

`Format > Font variations...` and `Document > Font > Variations...`
(`progs/fonts/font-variations.scm`) open a window, kept above the editor
windows, with the axes of the font at the cursor: for each, its name, a
field with the value in force and a few values along the axis, `-` and `+`
buttons which step by a twentieth of the range, and the range. The axes
come from `font-variation-axes` (`tt_font_axes`), which reads `fvar` and
`name` from the table directory and leaves out the axes the font hides. A
change applies at once, to the selection (the innermost `with` which sets
`font-variations` is updated rather than nested again) or to the initial
environment of the document. A value equal to the one of the style in
force is dropped from the list, and Reset removes the variable. The panel
follows the cursor.

The values are chosen with a field and buttons rather than sliders: the
widget language of TeXmacs has no slider, and adding one means a new
widget in each GUI.

## How an instance is made

`tt_make_instance (tt, k)` opens the font with FreeType at face index
`k << 16`, which selects the named instance k, and loads every glyph with
`FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING`: FreeType applies `gvar` and gives
the outline and the advance in font units, composite glyphs flattened. From
them it writes

- `glyf`: simple glyphs only, without instructions, coordinates as 16-bit
  deltas;
- `loca`: long offsets (`head.indexToLocFormat = 1`);
- `hmtx`: an advance and a left side bearing for every glyph
  (`hhea.numberOfHMetrics` = number of glyphs);

and adjusts `head` (bounding box), `hhea` (advance maximum, side bearing
extrema), `maxp` (points and contours; no composites, no instructions) and
`OS/2.usWeightClass` (the `wght` coordinate). The tables of variations
(`fvar gvar avar cvar HVAR VVAR MVAR STAT`) and of hinting
(`cvt fpgm prep hdmx LTSH VDMX`) are dropped, as is `DSIG`; the others are
copied as they are, except `GPOS`, `GSUB` and `cmap` (below). Checksums and
`checkSumAdjustment` are computed anew.

**Positioning.** The values of `GPOS` are those of the default instance; a
value which varies has a Device table in the format `VariationIndex`,
pointing to deltas in the `ItemVariationStore` of `GDEF`, one for each
region of the design space. `tt_vary_gpos` corrects every such value in
place, at the normalized coordinates FreeType gives for the instance
(`FT_Get_Var_Blend_Coordinates`, after `avar`): the value records of
single and pair adjustments (kerning; in a PairSet the Device tables are
relative to the PairSet, elsewhere to the subtable) and the anchors of
cursive attachment and of marks on bases, ligatures and marks. The tables
keep their layout, and a field shared by several records is corrected
once.

Checked against `fontTools.varLib.instancer` on SF NS Bold and Extra
Compressed Thin: outlines agree within the rounding to integer units, and
advances agree except for a few composite glyphs flagged
`USE_MY_METRICS`, where the instance takes the advance of the component
(as the default instance does) and fontTools the one of `HVAR`. The
kerning of every pair of letters, digits and punctuation (4146 pairs, of
which 897 at Black and 1277 at `wdth=60,wght=850` differ from the
default) agrees with fontTools exactly, and so do the 12002 mark anchors of
Junicode VF at weight 700 (11530 of them varying).

**Features which vary.** `GSUB` and `GPOS` 1.1 may end with a
`FeatureVariations` table: records, each a set of conditions (a range of
normalized coordinates on some axes) and the feature tables which replace
those of the `FeatureList` when the conditions hold; the first record whose
conditions hold applies. Fonts use it for glyphs which change at some points
of the design space, nearly always through the feature `rvrn` (required
variation alternates), which is empty in the `FeatureList`: the dollar and
cent of Roboto Flex lose a bar at weights from 600 and at narrow widths, and
the h, m, n and ampersand of Fraunces are "wonky" at small optical sizes or
wherever its axis `WONK` is off. `tt_vary_layout` evaluates the conditions
at the coordinates of the instance, rounded to F2DOT14 as HarfBuzz does,
writes a new `FeatureList` with the feature tables of the record in front of
the rest of the table (which moves as a whole and keeps its layout, a
feature keeping its parameters) and leaves a table of version 1.0.

TeXmacs does not shape text: a character has the glyph of the `cmap`, and
features apply only when a document names them (`font-features`). A shaper
applies `rvrn` first and without being asked, so `tt_fold_rvrn` applies its
single substitutions to the subtables of format 4 and 12 of the `cmap` of the
instance, in the order of the lookups; every place where TeXmacs maps a
character to a glyph (metrics, rendering, rubber fonts, the PDF writer) then
finds the glyph of the instance. A feature a document asks for sees the
glyphs `rvrn` chose, as it would after a shaper.

A record may hold at the default itself, as for Fraunces (whose default is
wonky): the variable font is then not its own default, and
`tt_variation_name` gives the instance `var_default` even without
variations, which `find_font` takes as for any other point.

Checked against `fontTools.varLib.instancer` (the `cmap` of its instance
followed by its `rvrn`) at 19 points of Fraunces, Roboto Flex and M+ 1 (44
records, 6335 characters): every character has the same glyph, and the
features have the same lookups, up to the numbering of lookups which
fontTools prunes. `tests/Plugins/Freetype/tt_instance_test.cpp` checks a few
of these points on the fonts of the test suite of HarfBuzz
(`test/subset/data/fonts`), found through `TM_TEST_FONT_DIR`.

**Names.** The suffix of an instance keeps its case (`instance_suffix`, as
`suffix` gives it in lower case): the axes a font defines have upper case
tags, and `Fraunces.var_opsz36_WONK0` used to be read back as a point
without `WONK`.

## Disk space

Each instance is a file of the size of the variable font (1.5 MB for SF
NS, 2.9 MB for Junicode VF). The access time of a file records when it was
last used (`tt_unpack` sets it; the modification time is left alone, since
it says which version of the variable font the file comes from), and when
the font database is loaded, `tt_clean_instances` removes the least
recently used instances beyond 200 MB. `Tools > Fonts > Clear font cache`
removes them all (`font-clean-instances 0`). Instances are not cached in
`font_cache.scm`: `tt_font_find` passes their names to `tt_unpack` every
time, which checks them against the variable font.

## Limits

- **CFF2** variable fonts (cubic outlines) are not instanced; they keep their
  default instance only.
- **MATH, kern, GDEF** are those of the default instance. No variable math
  font is known to TeXmacs yet.
- **Features which vary** other than `rvrn` are those of the point, but
  apply, as any feature, only when a document asks for them; `rvrn` itself
  is applied through the `cmap`, which works for its single substitutions,
  the only ones known in such fonts.
- No slider in the panel (see above).
- Collections of variable fonts (`.ttc`) are not instanced.
