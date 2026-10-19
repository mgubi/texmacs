# Audit of the OpenType work

Date: 2026-10-13. Audited: branch `wip_opentype` at `3c4f178718`, the 137
commits on top of `wip_fixes` (`f3d66a71eb`): about 20,000 lines in 115 code
files, 257 new font files, tests and documentation. The audited code files are
the same in `maxs_texmacs`, so the findings hold there too.

Method: five reviewers, one area each, read-only on the worktree, with the
Qt/Guile build of the branch. A finding marked *run* was reproduced (a
corrupted font under AddressSanitizer, a rendering, a measure against LuaLaTeX
with `unicode-math`, a comparison with a build of the base); a finding marked
*read* comes from reading the code only. The findings the summary rests on
(A1, A2, A3, A5, B1, B2, B3, C1, C2, D1) were checked a second time, in the
code or in the renderings.

Severity: **critical** = crash, memory error or wrong output in common
documents; **major** = wrong output or a broken feature in realistic cases;
**minor** = edge case or small cost; **nit** = hygiene.

## State after the fixes

The findings below were fixed on 2026-10-13, each with the reproduction of
the audit as its check and, where a test can show it, a test which fails
without the fix: seven new tests of damaged tables in `tt_tools_test`, ten
in `opentype_font_test` (which now runs on the shipped fonts without
`TM_TEST_FONT_DIR`), the new Scheme suite `opentype-typeset`, and checks in
`misc-modules` and `latex`. After the fixes the parsers agree with fontTools
on the 372 fonts for MATH, the GSUB maps and GPOS kerning, and run clean
under AddressSanitizer on the shipped and on 30 damaged fonts; the
hand-tuned fonts (Pagella, Termes, Bonum, Schola, `stix`) and `roman` are
pixel-identical to the base on the comparison sheets.

What is left:

- **E11**: the 110 faces (16.7 MB) which no menu entry used are kept and
  made reachable: a design with variants (the weights of IBM Plex, the
  condensed widths, the light weights of the Polish fonts, the typewriter
  of variable width of Latin Modern) is a submenu of the font menus and a
  second choice on the page of the fonts, each variant a master with its
  italic and its bold, and Format > Weight lists the weights of the font at
  the cursor (`progs/fonts/font-weight.scm`). E10 is settled by decision: the light weights of
  Antykwa Poltawskiego and of Latin Modern Mono Prop are also families of
  their own, "Antykwa Poltawskiego Light" and "Latin Modern Mono Prop
  Light", which hold their bold faces; the light regular and italic faces
  stay in the main family too. Every shipped style now comes back as itself
  from the variables of the font selector.
- **C5**: the boxes are wrapped as the primes are and the tree-to-box
  direction is tested; the box-to-tree direction (a click, an arrow key)
  needs a window and was not run. The rectangle of a prime changed with it:
  it is now the glyph.
- **C15** (script sizes rounded to whole points): not changed, needs
  fractional font sizes.
- **B1**: in Latin Modern Math an upright word in a script has no `ff`
  ligature, because the font has script variants of `e` and `f` and no
  ligature of these variants. Fonts without such variants keep the ligature.
- **B6**: 400 distinct delimiter heights take 20 s instead of 39 s; the
  rest is the compilation of 800 tall glyphs.
- **B7**: the switch "hand tuned math fonts" now holds for documents loaded
  after it; open documents are not typeset again.
- **C6**: a piece of an assembly other than the first is written as U+0000
  in the ToUnicode map (the PDF writer has no way to omit the entry).
- **D2**: a saved set made from a pair which goes through a style package
  (Palatino) is not shown as the current one in "Saved fonts".
- **D8**: TeXmacs has no big operator for the eleven symbols of the former
  section "More big operators"; the section is now called "Operators of the
  size of the text", and `\bigtimes` imports as the symbol `<bigtimes>`.
- **D13**: `tmuniversaltounicode-extra-candidates.scm` stays where it is: it
  is the output of `tests/opentype/missing-symbols.py`.
- **A9**: a font name which is not found still costs about 170 ms, as at the
  base.
- **E5**: with the tie between Regular, Book and Medium broken, the
  substitute of a font name which is no known master changed too (an
  arbitrary winner among equals). No test: no shipped family shows the
  defect without a TeX Live.
- **Differences from the base which stay**: by decision, a tall radical or
  a big operator which the base emulated from pieces and wrote as a bitmap
  in the PDF (the tall radicals of Pagella, Termes, Bonum, Schola, `stix`
  and Linux Libertine, their display contour integral) is drawn from the
  MATH table of the nearest font which has one, the font's own math
  companion first, at the tallest size which does not exceed the emulated
  one; everything else in these fonts is as at the base, and the samples
  have no bitmap font; Linux Libertine and Linux
  Biolinum take their radicals from Libertinus Math and New CM Sans Math,
  the nearest fonts which now have them (four shipped fonts had the pieces
  at the base, 27 now); in documents in the TeX Gyre fonts a symbol which the base
  emulated as a bitmap and which STIX Two Math has (`<triangledown>`) is
  now that glyph; the kerning of text in OpenType fonts follows all the
  lookups of the font, which moves line breaks.
- **Font data seen on the way**: GFS Neohellenic Math gives its acute and
  grave accents the attachment points of zero-width glyphs, so they sit low
  over capitals; the very wide accents of Lete Sans Math (10 em and more)
  have empty metrics and are skipped.
- **Not run**: the Linux and Windows branches of the font path code, which
  changed (TeX Live of any year on Linux); the S7 build (two checks of the
  suites `misc-modules` and `latex` failed with the S7 binary of another
  tree on an overlay of the Scheme files, not traced).
- **Seen and not traced**: a bus error at startup, twice during the audit
  and once in the fixed build, never reproduced; four checks of the suite
  `latex` (import of `eqnarray` and `align`) which fail on the first run
  with a new home directory and pass afterwards.

Also fixed on the way: `rhd`, `shortleftarrow` and `shortrightarrow` had two
LaTeX packages each and could not be exported, already at the base; the unit
test program dropped the TeX fonts from the font database of its home
directory, after which `roman` was typeset in CMU Serif.

## Summary

64 findings: 3 critical, 26 major, 29 minor, 6 nits.

What is sound: the parsing of the MATH table (no difference with fontTools on
372 fonts), the arithmetic of glyph assemblies, the placement of fractions,
of single scripts on a glyph, of limits and of the details of radicals (within
0.1 pt of LuaTeX in four fonts), the `roman` font (unchanged but for long
arrows), the symbol tables (199 code points right, no clash), the licences of
21 of the 23 new families, the font database against the shipped files, the
build lists, the documentation links.

What is not:

1. **Font files are trusted.** No offset or count read from a font is checked
   (A1 to A4). A truncated or hostile font crashes or hangs TeXmacs.
2. **The `ssty` feature is applied character by character through the smart
   font** (B1, B2, C6): words in scripts become italic, glyphs of fallback
   fonts are drawn with the numbers of another font, and script letters cannot
   be copied from the PDF. This is the most visible defect: `T_eff` in Latin
   Modern Math or STIX Two Math is wrong.
3. **Kerning and features of text fonts are read by a simplified GPOS and GSUB
   reader** (A5, A6, B3): 16 to 24% of the kerned pairs of Libertinus are
   wrong, small capitals of Libertinus change one letter only, small capitals
   of XCharter trigger f-ligatures.
4. **The placement of scripts leaves the MATH rules in several cases** (C1 to
   C4, C13, C14): display fractions on a stretched line, a subscript with a
   superscript, inline formulas on a non-glyph base.
5. **Existing documents change** (C7, C8, E5, D7): the hand-tuned Pagella,
   Termes and `stix`, long arrows everywhere, text set in a Medium face when a
   TeX Live is installed, LaTeX export of seven symbols.
6. **The euro sign** (D1, E1): a new table entry breaks the LaTeX export of
   every document with a euro sign (empty file) and a unit test, which in turn
   stops `tests/opentype/check.sh` at its first step.
7. **The font design page loses settings** (D2, D4): it resets `math-font` and
   drops the font rules it does not know.
8. **Two licence files** (E3, E4) are missing or wrong.

## A. Reading OpenType files and finding fonts

| Id | Severity | Finding | Where | Evidence |
|---|---|---|---|---|
| A1 | critical | No offset, count or index read from a font is checked against the table length (MATH, GSUB, GPOS). A truncated MATH table reads out of bounds; a 490-byte MATH table makes TeXmacs hang. The design document says a truncated table is tested; it is not. | `Plugins/Freetype/tt_tools.cpp:52-58` and all callers in 475-1026 | run (ASan, and in TeXmacs) |
| A2 | critical | GPOS PairPos format 2: `class1_count * class2_count` overflows `int`; negative allocation, wild write. Reached on the first kerning of any text in the font. | `tt_tools.cpp:982-988` | run |
| A3 | major | A GSUB extension lookup which names itself as its target loops for ever. | `tt_tools.cpp:794-798` | run |
| A4 | major | Coverage format 2 is expanded without limit: a 2 KB MATH table costs minutes and 2.9 GB. | `tt_tools.cpp:485-495` | run |
| A5 | major | Kerning returns the value of the first class subtable which covers the left glyph, over all lookups together, so later lookups are lost; pairs of value 0 are not stored. 46 of 372 fonts differ from the OpenType result: 16-24% of kerned pairs in Libertinus, 39% in Erewhon Bold, 6-7% in Fira Sans, 5% in STIX Two Math. | `tt_tools.cpp:894-904`, `994-1026` | run (against fontTools) |
| A6 | major | A GSUB feature is the union of all records with its tag, whatever the script and language, and one-to-one substitutions stored as lookup type 2 are skipped. "Small capitals" in Libertinus Sans changes only `i`, into the Turkish dotted form; `locl` and `salt` of four CMU fonts give Greek guillemets. | `tt_tools.cpp:866-880`, `804-806` | run |
| A7 | minor | The list of features proposed in the menu includes tags for which nothing can be applied: `calt` in 25 fonts, `c2sc` in Libertinus, `ss03` in EB Garamond. | `tt_tools.cpp:842-853`, `progs/fonts/font-features.scm:24-59` | run |
| A8 | minor | A MATH table with a null MathGlyphInfo or MathVariants offset is parsed as if the part were at offset 0; an unconditional message goes to stdout. | `tt_tools.cpp:702-715`, `756-764`, `497` | run |
| A9 | minor | The font path is rebuilt, with a walk of the TeX Live trees, for every file name tried: about 320 ms for each font name not in the cache. The new cache key prefix makes every existing home miss once per font. | `Plugins/Freetype/tt_file.cpp:154`, `47-93` | run |
| A10 | minor | Two installations sharing a home directory merge the font database again at every alternate start (the path is part of the stamp), with 658 lines of output. | `Graphics/Fonts/font_database.cpp:241-314` | run |
| A11 | nit | The whole font file is copied into a new string at each parse (MATH, each GSUB tag, GPOS). | `Plugins/Freetype/tt_face.cpp:83-117` | read |
| A12 | nit | With several TeX Live years installed the oldest one wins. | `tt_file.cpp:56-64` | read |

Fixes. A1-A4, A8: one checked accessor which fails outside the table, every
loop bounded by what the table can hold, an extension must not point to an
extension, a coverage is at most 65536 glyphs, a null offset skips its part.
A5: keep the subtables by lookup, add the contribution of each lookup, first
matching subtable in each. A6: read the default language system of
`latn`/`DFLT`; accept type 2 lookups with sequences of length 1. A9: compute
the path once.

## B. The font classes

| Id | Severity | Finding | Where | Evidence |
|---|---|---|---|---|
| B1 | critical | Words in subscripts and superscripts are drawn in math italic: `T_eff`, `e^{sin x}`, `x_max`. The `ssty` font asks the smart font for the variant of each character alone, which then takes each letter for a variable. Fonts with `ssty` for letters: Latin Modern Math, STIX Two Math, KpMath... | `Typeset/Env/env_semantics.cpp:592`, `Graphics/Fonts/feature_font.cpp:75-94`, `smart_font.cpp:1066-1069`, `1423-1432` | run (rendering) |
| B2 | major | The feature substitute of a character drawn by a fallback subfont is drawn with the glyph number in the main font: in Fira Math, calligraphic and fraktur letters in a script come out as other letters. The same holds for `font-features` on any character taken from a fallback. | `smart_font.cpp:1423-1432`, `feature_font.cpp:86`, `143-155` | run (rendering) |
| B3 | major | A substituted glyph whose number ends in 0x66 is taken for an `f` and starts the f-ligatures: XCharter with `c2sc` writes "ff fil fften" for "Of Oil Often". Same collision in Kurier, Linux Libertine, NewCM10, Noto Sans, STIX Two Text for `c2sc`, `smcp`, `onum`, `pnum`. | `Plugins/Freetype/unicode_font.cpp:810-930` | run (rendering) |
| B4 | major | Sans serif formulas in New Computer Modern lose every single-letter variable: the NewCM text fonts have a MATH table but no mathematical italic alphabet. | `smart_font.cpp:896-897`, `923-927`, `1066-1069` | run |
| B5 | major | A document set to IBM Plex from the font menu never uses IBM Plex Math: the features entry lacks `SansSerif`. Formulas are set in the text font with bitmaps. | `TeXmacs/fonts/font-features.scm:409` | run |
| B6 | major | Each new height of an assembled delimiter rebuilds the virtual font and leaks the old one: 400 distinct heights take 39 s (2.9 s in roman). | `Plugins/Freetype/rubber_unicode_font.cpp:566-577`, `619`, `498` | run |
| B7 | minor | The switch "hand tuned math fonts" has no effect on fonts already loaded and gives a mixture afterwards. | `Graphics/Fonts/font.cpp:589-592` | run |
| B8 | minor | The first size variant is taken for the base glyph: in GFS Neohellenic Math the text-style contour integral is drawn at display size. Other fonts list a first variant which differs from the base (read from the tables only). | `rubber_unicode_font.cpp:377-383`, `483-487`, `609-613` | run |
| B9 | minor | Every session with the default font loads the global font database and prints "missing font, loading global substitution list". | `smart_font.cpp:748`, `math_font_profiles.cpp:89`, `font_database.cpp:815-819` | run |
| B10 | minor | Explicit delimiter sizes beyond the 64 assembled ones fall back to another construction: `<left|(|80>` is shorter than `<left|(|40>`. | `rubber_unicode_font.cpp:392`, `429-440` | run |
| B11 | minor | A script on a token of several characters (`af^1`, `lim_n`, `17^2`) prints "no glyph index available" at each typesetting. | `unicode_font.cpp:1115`, `1138` | run |

Fixes. B1, B2 (and C6): apply the feature inside the subfont which draws each
run, after the smart font has split the string in context; a native glyph
number must not leave its subfont, and the source character must go with it.
B3: compare the whole code with `'f'`, not its low byte. B4: set `ot_math`
only when the font has `<#1D44E>`. B6: let the virtual font grow in place and
quantize the sizes. B11: `is_ot_integral (get_right (s))`.

## C. The typesetter

Measures are against LuaLaTeX with `unicode-math`, 43 formulas, in Latin
Modern Math, STIX Two Math, Fira Math and Libertinus Math.

| Id | Severity | Finding | Where | Evidence |
|---|---|---|---|---|
| C1 | major | A display fraction falls back on the text-style constants when its line is stretched or shrunk (kerning and glyph expansion are "auto" by default): the box is rebuilt without `disp`. Numerator at +3.9 pt instead of +6.8 pt. Limit boxes lose `stretched` in the same way (read). | `Typeset/Boxes/Composite/math_boxes.cpp:124`, `132`, `script_boxes.cpp:163`, `173` | run |
| C2 | major | The conflict between a subscript and a superscript is resolved the wrong way: the superscript is lowered, never raised, which also undoes the minimal gap. Fira `x_i^2`: 402/-354 for 527/-235. | `script_boxes.cpp:397-409` | run |
| C3 | major | In an inline formula, scripts and limits on a base which is not a glyph (big operator, bracket, radical, fraction) are placed with the text font, so by the old rules; the same formula in an equation gets the MATH rules. Differences of 1 to 2 pt. | `Typeset/Concat/concat_post.cpp:37-47`, `238-256` | run |
| C4 | minor | Subscripts inside superscripts and numerators are raised by the old level rule, which the MATH table does not have. | `Boxes/Basic/text_boxes.cpp:186-190` | run |
| C5 | major | The dotless i or j under an accent and the precomposed negations are typeset from another string than the tree holds, without `symbol_box`: box positions map to cursor paths beyond the string (`FAILED ("bad path")`). The primes in the same file use `symbol_box` for this reason. | `Typeset/Concat/concat_math.cpp:472-479`, `528-536` | read |
| C6 | major | In the PDF every glyph drawn by glyph number has U+0000 in the ToUnicode map: script letters of fonts with `ssty`, size variants, assembly pieces. Copy and search fail. | `Plugins/Pdf/pdf_hummus_renderer.cpp:1555`, `1566` | run |
| C7 | major | Documents in the hand-tuned fonts change: integrals and sums in Pagella and Termes (scripts moved by up to 1.9 pt, another glyph for the display integral), the radical in `stix`, large delimiters in `Stix`. | `concat_math.cpp:23`, `452-460`, `611`, `Graphics/Fonts/poor_rubber.cpp:77`, `unicode_font.cpp:329` | run (against the base) |
| C8 | minor | Long arrows are one size step longer in every font, the TeX fonts included; the only change in `roman`. | `text_boxes.cpp:615-638` | run (against the base) |
| C9 | minor | The underline is half a rule too close to the base and shifted to the left. | `math_boxes.cpp:494`, `750` | run |
| C10 | minor | The kern before the degree of a radical is applied as a negative `x1`: no space before the degree, and the radical is wider wherever a width is taken (the bar of a fraction sticks out). | `math_boxes.cpp:195`, `215-217` | run |
| C11 | minor | The display-style gap of a radical has no effect when the first size already covers the radicand. | `concat_math.cpp:454-462`, `text_boxes.cpp:543-553` | run |
| C12 | minor | Narrow accents (`hat`, `tilde`) come from the text font (U+02C6, U+02DC), not from the accents of the math font; `flac` can never apply, contrary to the design document. | `math_boxes.cpp:512-539` | run |
| C13 | minor | A number of several digits is treated as a box: `10^3` and `0^3` get different heights. | `text_boxes.cpp:179-181` | run |
| C14 | minor | For a base which is not a glyph, the old `sup_lo_base` is the lower bound instead of the superscript shift of the font. | `script_boxes.cpp:388-389` | run |
| C15 | nit | Script sizes from `scriptPercentScaleDown` are rounded to whole points. | `env_semantics.cpp:587` | run |

Fixes. C1: keep `disp` and `stretched` as members. C2: `ysub -= delta`, then
raise both by `sup_bottom_max_with_sub - (ysup + sup->y1)` when positive.
C3: record the math font with the line item. C5: wrap in
`symbol_box (descend (ip, 0), b, N (t[0]->label))`. C7, C8: gate the changes
on `math_type == MATH_TYPE_OPENTYPE`, not on `ot_math`.

## D. Scheme code, interface, symbols

| Id | Severity | Finding | Where | Evidence |
|---|---|---|---|---|
| D1 | major | The LaTeX export of any document with a euro sign writes an empty file: `euro` now needs both `eurosym` and `unicode-math` ("Ambiguous return values"). Also, a pasted or imported U+20AC now becomes `<euro>`. | `progs/convert/latex/latex-symbol-drd.scm:253`, `299-300`, `langs/encoding/tmuniversaltounicode-extra.scm:35` | run (against the base) |
| D2 | major | The design page ignores `math-font`, and "Use for the document" resets it: formulas in STIX go back to Computer Modern, even with nothing chosen. | `progs/fonts/font-design.scm:320-330`, `365-370`, `393-403` | run |
| D3 | major | Every button of the design page fails when the path of the document has a backslash or a double quote (Guile): all paths on Windows. | `font-design.scm:611-615`, `624-629` | run on macOS, read for Windows |
| D4 | major | The design page drops every font rule it does not know (`bold=`, `italic=`, `greek=`... written by the font selector) and resets the family `tt` to `rm`; "Saved fonts" too. | `font-design.scm:311`, `324-329` | run |
| D5 | major | The "Font features" menus look at another font file than the one which draws the text (they do not know the rules in `font` nor the companions): wrong lists, or "no feature". | `progs/fonts/font-features.scm:61-65`, `font-new-widgets.scm` | run |
| D6 | major | Saved font sets are lost at the next start when a name has `"` or `\`; with a backslash, all sets. | `progs/fonts/font-custom.scm:34-42` | run |
| D7 | minor | Seven symbols which exported for pdfLaTeX now pull in `unicode-math`: `lgroup`, `rgroup`, `dotminus`, `dottimes`, `btimes`, `mapsup`, `mapsdown`. | `latex-symbol-drd.scm:245-274` | run (against the base) |
| D8 | minor | `<bigtimes>` cannot be exported to LaTeX; the palette "More big operators" inserts plain symbols, not big operators. | `progs/math/math-symbol-tools.scm:142-147`, `progs/language/std-symbols.scm:794-800` | run / read |
| D9 | minor | The symbols window takes 6.6 s to open the first time, and the existing symbol palettes build up to 40 times slower the first time (17 ms to 719 ms). | `Texmacs/Window/tm_button.cpp:266-275`, `progs/kernel/gui/menu-widget.scm:536-538`, `math-symbol-tools.scm:264-277` | run (against the base) |
| D10 | minor | The design page fails with a Scheme error when its document is not loaded. | `font-design.scm:365-370`, `857-862` | run |
| D11 | minor | Document > Font > Features computes the new global value from the environment at the cursor. | `font-features.scm:77-107` | read |
| D12 | minor | A saved set naming a font which is not installed is applied without notice. | `font-custom.scm:96-124` | run |
| D13 | nit | Typewriter and sans serif tabs label a master by its menu name ("PT Serif" among the typewriter fonts); "Font design" is offered on the design page itself; names of sets are translated as menu labels; one help entry in three submenus; `tmuniversaltounicode-extra-candidates.scm` (754 commented lines) is shipped and never read. | various | run / read |

## E. Shipped fonts, packaging, tests, documentation

| Id | Severity | Finding | Where | Evidence |
|---|---|---|---|---|
| E1 | major | `make -C tests` fails at HEAD (the euro entry of D1 against `universal_test`), so `tests/opentype/check.sh` stops at its first step and never runs the renders. | `tests/Data/String/universal_test.cpp:177`, `tests/opentype/check.sh:10` | run |
| E2 | major | Lete Sans Math: a wide hat or tilde over a single letter is a missing-glyph box (the font has U+0302, not U+02C6; see C12). | `progs/fonts/fonts-opentype.scm:136` | run (rendering) |
| E3 | major | `noto/OFL.txt` is a stub of 43 bytes: seven Noto fonts ship without the licence text. | `TeXmacs/fonts/truetype/noto/OFL.txt` | run |
| E4 | major | `Inconsolatazi4-Bold.otf` is under the Apache License 2.0; only an OFL file is shipped and the catalogue says OFL. | `TeXmacs/fonts/truetype/inconsolata/` | run (name tables) |
| E5 | major | With a current TeX Live installed (now scanned for every year), the regular text of shipped families is set in TeX Live's Medium face: Fira, Garamond italic, Antykwa Torunska italic. | `Plugins/Freetype/tt_file.cpp:48-93` | run |
| E6 | minor | Typewriter text of the DejaVu, IBM Plex and Noto entries is PT Mono on macOS: 22 shipped mono files are not used by their own menu entries. | `fonts-opentype.scm:81`, `144`, `158` | run |
| E7 | minor | Suite `misc-modules` fails three checks on Fira styles when a TeX Live is installed; it passes on the base. | `progs/check/misc-modules-test.scm:547-549` | run |
| E8 | minor | 18 of the 24 OpenType unit tests are skipped unless `TM_TEST_FONT_DIR` is set, though the fonts are in the tree; two tests return silently without a font; the pixel comparison never changes the exit status and its reference is not in the tree; the README counts are out of date. | `tests/Graphics/Fonts/opentype_font_test.cpp`, `tests/opentype/render-samples.sh:86-90` | run |
| E9 | minor | Noto Sans Math has no sample on the design page (the only math font shipped as `.ttf`); the sample of Noto Serif is of a font which is not shipped. | `font-design.scm:564-567` | run |
| E10 | minor | Four shipped faces (light and bold at once) cannot be selected. | `poltawski/antpoltlt-bold*.otf`, `lm/lmmonoproplt10-bold*.otf` | run |
| E11 | minor | The font tree grew from 14 MB to 75 MB (in every package and in the browser bundle); about 17 MB is faces no menu entry uses (extra weights of IBM Plex, condensed DejaVu, Iwona, Kurier...). | `TeXmacs/fonts/truetype/` | run |
| E12 | nit | Documents out of date: counts and lists of profiles and shipped fonts, "`ssty` is read but not yet applied", "a dozen such fonts". | `src/OPENTYPEMATH.md`, `README.md`, `doc/opentype-math-design.md:534`, `man-math-fonts.en.tm:11` | read |
| E13 | nit | Hygiene: 38 temporary lines in `.gitignore` (among them `plugins/scilab/*`, tracked sources); `OPENTYPEMATH.md` and 24 pictures in the C++ source directory, where a test script writes by default; a section of `README.md` about this branch; a comment with a path under `/Users/mgubi`; `texlive_font_dirs` unused outside macOS; scripts which need `magick` and break with a space in the path. | `src/.gitignore`, `src/src/`, `tt_tools.cpp:767`, `tests/opentype/` | read |

## Not covered

- Cursor movement and selection inside the new boxes, with a real window (C5
  is from reading).
- Windows, Linux and the browser build: nothing was run there (D3 is inferred
  from a file name with a backslash on macOS).
- GUIs other than Qt, and Vue for the Scheme part.
- The effect of the new GPOS kerning on the line breaks of existing documents
  in OpenType text fonts.
- Math kerning (cut-ins) in a font where the kern tables matter.
- Screen rendering at other magnifications; only PDF output was measured.
- `.ttc` collections with a MATH table; fonts newer than those of TeX Live
  2025.
- Seen in passing and not traced: two bus errors at startup in Guile's
  `scm_c_random` while several TeXmacs ran at once; a crash in `latex_expand`
  for `(convert <stree> "texmacs-stree" "latex-document")`; `texmacs.bin -c`
  writes only the first page of a document (also with the base).
