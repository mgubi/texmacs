# Variable fonts

A variable font holds a continuum of designs in one file: weights, widths,
optical sizes, grades. Its `fvar` table lists *named instances* (Thin, Bold,
Condensed Black...), each a point on the axes. Before this branch TeXmacs
saw only the default instance of such a file (SF NS Regular, New York
Regular, Junicode VF Regular) and emulated bold and the other weights.

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
copied as they are. Checksums and `checkSumAdjustment` are computed anew.

Checked against `fontTools.varLib.instancer` on SF NS Bold and Extra
Compressed Thin: outlines agree within the rounding to integer units, and
advances agree except for a few composite glyphs flagged
`USE_MY_METRICS`, where the instance takes the advance of the component
(as the default instance does) and fontTools the one of `HVAR`.

## Limits

- **CFF2** variable fonts (cubic outlines) are not instanced; they keep their
  default instance only.
- **GPOS, GDEF, MATH, kern** are those of the default instance. Kerning
  pairs and mark positions of a Black are therefore those of the Regular;
  applying `GDEF` item variations would fix that.
- Only named instances: there is no way yet to ask for an arbitrary point on
  an axis (weight 550, optical size of the current font size).
- Collections of variable fonts (`.ttc`) are not instanced.
