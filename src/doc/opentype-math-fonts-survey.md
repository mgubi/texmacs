# OpenType math fonts for TeXmacs: survey and integration plan

The number of usable OpenType math fonts is small and stable, so it is
reasonable to know each of them individually and to hard-code what the
generic MATH-table machinery cannot infer: which text fonts go with a math
font, whether its letters should be used in formulas, which weights exist,
and the odd quirks. This document surveys the candidates and proposes how
to integrate them into TeXmacs on top of the MATH table support described
in `opentype-math-design.md`.

The numbers come from `tests/opentype/survey-math-fonts.py`, run on the
fonts in TeX Live 2025, in the `tm-fonts` collection, and in the TeXmacs
tree. Re-run it when fonts are updated:

```
tests/opentype/survey-math-fonts.py -t /path/to/unicode-math-table.scm *.otf
```

Give it math fonts only: on a font without vertical variants, such as
Concrete-Math-Bold, the script stops with an error.

## 1. What matters for TeXmacs

For a math font to work well in TeXmacs we need, in decreasing order of
importance:

1. **Text companions.** TeXmacs sets text and mathematics with the same
   `font` by default; the math font must come with upright, italic, bold
   and bold italic text faces of the same design, and ideally sans and
   mono faces.
2. **A complete and well-made MATH table**: constants, variants and
   assemblies for all delimiters, big operators in two sizes, italic
   corrections, top accent attachments, and cut-in kerning (MathKernInfo)
   for the letters.
3. **Symbol coverage.** TeXmacs's math symbols map to Unicode; the
   `unicode-math` symbol list (2435 code points) is a good yardstick. The
   TeX Gyre level (about 1650 symbols, 92 percent of the Mathematical
   Operators block) is enough for everyday mathematics; STIX level (all of them)
   covers everything TeXmacs can name.
4. **Alphabets** in the Mathematical Alphanumeric Symbols block: bold,
   italic, script, fraktur, double-struck, sans, mono. Missing alphabets can
   be emulated by TeXmacs's virtual fonts, but real ones look better.
5. **Weights**: a bold math font for bold headings and theorem titles
   (TeXmacs emulated bold mathematics by stroking; it now uses a real bold
math face where the font has one, see 5.2 step 5).
6. **License** allowing redistribution with TeXmacs (OFL, GUST, LPPL).

## 2. Summary table

Symbols is the coverage of the `unicode-math` list, alnum the coverage of
the Mathematical Alphanumeric Symbols block (996 assigned code points), ops
the Mathematical Operators block. Kern info is the number of glyphs with
MathKernInfo; vert (asm) the number of glyphs with vertical variants and how
many of them have an assembly; the GPOS column names the `kern` and `mark`
features only. Most fonts have `ssty` (script-style
alternates) and `dtls` (dotless i and j); XITS Math Bold and Libertinus Math
have `ssty` but no `dtls`, and STIX Math v1 has neither.

| Font | Version | Glyphs | KB | Symbols | Alnum | Ops | Kern info | Italics | Top acc | Vert (asm) | Horiz (asm) | GPOS |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| Latin Modern Math | 1.959 | 4802 | 716 | 65% | 94% | 92% | 0 | 1002 | 2475 | 94 (43) | 86 (71) | none |
| NewCM Math Regular | 4.0 | 7668 | 1123 | 100% | 100% | 100% | 620 | 1085 | 2908 | 162 (45) | 86 (71) | kern |
| NewCM Math Book | 4.0 | 7659 | 1247 | 100% | 100% | 100% | 621 | 1087 | 2937 | 162 (45) | 86 (71) | kern |
| NewCM Math Bold | 5.3 | 7750 | 1047 | 100% | 100% | 100% | 19 | 1085 | 2835 | 162 (45) | 86 (71) | kern |
| NewCM Sans Math | 4.0 | 7188 | 858 | 100% | 100% | 100% | 516 | 1084 | 2833 | 160 (45) | 86 (71) | kern |
| TeX Gyre Pagella Math | 1.632 | 4246 | 587 | 67% | 99% | 92% | 0 | 1253 | 1941 | 95 (47) | 86 (71) | none |
| TeX Gyre Termes Math | 1.543 | 4246 | 519 | 67% | 99% | 92% | 0 | 915 | 1941 | 95 (47) | 86 (71) | none |
| TeX Gyre Bonum Math | 1.005 | 4246 | 515 | 67% | 99% | 92% | 0 | 1024 | 1942 | 95 (47) | 86 (71) | none |
| TeX Gyre Schola Math | 1.533 | 4246 | 582 | 67% | 99% | 92% | 0 | 1026 | 1942 | 95 (47) | 86 (71) | none |
| TeX Gyre DejaVu Math | 1.106 | 4280 | 512 | 67% | 99% | 92% | 0 | 448 | 1960 | 95 (47) | 86 (71) | none |
| STIX Math (v1, shipped) | 1.1.0 | 4226 | 466 | 98% | 99% | 100% | 0 | 0 | 141 | 98 (33) | 43 (32) | kern |
| STIX Two Math | 2.12 | 6760 | 818 | 100% | 100% | 100% | 233 | 889 | 2652 | 118 (32) | 47 (37) | kern |
| XITS Math | 1.302 | 4559 | 535 | 99% | 100% | 100% | 29 | 643 | 1328 | 168 (38) | 41 (34) | kern |
| XITS Math Bold | 1.302 | 2154 | 244 | 62% | 92% | 92% | 14 | 300 | 479 | 40 (1) | 0 (0) | kern |
| Libertinus Math | 7.051 | 4463 | 575 | 67% | 98% | 85% | 0 | 423 | 981 | 59 (15) | 27 (22) | kern, mark |
| Asana Math | 0.958 | 3447 | 423 | 94% | 100% | 100% | 76 | 347 | 933 | 77 (22) | 36 (30) | kern |
| Fira Math | 0.3.4 | 2094 | 175 | 43% | 44% | 61% | 0 | 244 | 529 | 40 (18) | 6 (6) | none |
| KpMath Regular | 0.66 | 3465 | 416 | 65% | 91% | 89% | 707 | 1004 | 1271 | 105 (25) | 53 (45) | none |
| KpMath Light / Semibold / Bold / Sans / SansBold | 0.66 | 2058 to 3465 | | 61 to 65% | 91% | 81 to 89% | 105 to 708 | | | | | none |
| Erewhon Math | 0.67 | 3230 | 404 | 68% | 94% | 93% | 403 | 752 | 1337 | 80 (34) | 52 (45) | kern |
| XCharter Math | 0.65 | 3111 | 357 | 67% | 94% | 93% | 54 | 778 | 874 | 70 (26) | 52 (45) | none |
| Concrete Math | 0.65 | 3188 | 375 | 67% | 94% | 93% | 2 | 557 | 703 | 70 (26) | 52 (45) | kern |
| Euler Math | 0.62 | 3445 | 418 | 66% | 93% | 93% | 0 | 37 | 200 | 58 (24) | 52 (45) | kern |
| Neo Euler (2010, `tm-fonts`) | 0.002 | 1904 | 350 | 23% | 26% | 44% | 0 | 59 | 267 | 42 (16) | 9 (9) | kern |
| GFS Neohellenic Math | 1.02 | 3546 | 642 | 68% | 41% | 98% | 105 | 380 | 659 | 111 (42) | 85 (70) | kern |
| Garamond Math | 2022 | 5243 | 804 | 67% | 93% | 90% | 2094 | 44 | 2493 | 77 (25) | 39 (27) | kern |
| Lete Sans Math | 0.45 | 4373 | 432 | 95% | 95% | 100% | 33 | 603 | 872 | 87 (32) | 44 (38) | kern |
| IBM Plex Math | 1.000 | 7092 | 730 | 99% | 100% | 100% | 392 | 358 | 2508 | 125 (49) | 51 (43) | kern, mark |
| Old Standard Math | 1.0 | 8138 | 1042 | 100% | 100% | 100% | 0 | 1084 | 2598 | 160 (45) | 86 (71) | kern, mark |
| Noto Sans Math | 3.000 | 5130 | 991 | 100% | 100% | 100% | 0 | 528 | 1761 | 215 (57) | 29 (22) | kern, mark |

Not surveyed because unavailable here: Cambria Math (proprietary, Windows
and Office), Minion Math (commercial), Lucida Bright Math OpenType
(commercial). Noto Sans Math had no MATH table when the survey was first
run; its release 3.000 has one and was measured afterwards. The ConTeXt "companion" fonts
(`context-companion-fonts`) are patch fonts, not standalone.

The variant conventions are mostly uniform, and the exceptions are the
reason the code may assume none of them. Usually the first
MathGlyphVariantRecord of a glyph is the glyph itself, parentheses,
brackets, braces, bars, radicals and integrals have both size variants and
an assembly, and summation has two sizes without an assembly. But the first
record is not the base glyph for the parentheses, brackets and braces of
XCharter Math and Concrete Math, for the parenthesis and the radical of
KpMath Sans, nor for the bar of STIX Math v1 and XITS Math Bold; the
integrals have no assembly in Latin Modern Math, STIX Math v1, both XITS
weights, Libertinus Math, Fira Math, Garamond Math, Neo Euler and the bold
weights of KpMath;
the bar has no vertical variants in STIX Math v1 and XITS Math Bold, which
also has none for the radical and no assembly for the parentheses; and Asana
Math gives summation four sizes. STIX Math v1 is the outlier overall: no
italic corrections, few top accents, no `ssty`, which is why TeXmacs
hard-coded its behaviour.

## 3. The fonts, one by one

The tiers below were written as a plan, before anything was shipped; each
entry ends with what the branch does with the font now. Section 6 lists
what is shipped.

### Tier 1: complete families to support first

These have a full set of text companions in the same design, a good MATH
table, and a free license. They are the ones to hard-code carefully.

**Latin Modern Math** (GUST Font License). The OpenType Computer Modern.
Companions: `lmroman10-{regular,italic,bold,bolditalic}` and the whole LM
family (sans, mono, caps, several optical sizes). No MathKernInfo, so the
cut-in kerning falls back to italic corrections. Script and script-script
scales 70/50 like TeX. TeXmacs's default "roman" font is Computer Modern
through Metafont, so this is the natural OpenType default and the reference
against which to compare TeXmacs's own TeX-based layout. Quirk seen in the
samples: the radical sign is drawn so that its top edge is the rule, and its
`radicalExtraAscender` of 40 design units is the smallest of the fonts
measured (New Computer Modern Math and Euler Math have 40 too), so a gap between the sign and the rule shows immediately. This is
what the radical junction of 22 September 2026 fixed. *Now: shipped
(`lm`), with LM Roman, Sans and Mono; menu entry "Latin Modern".*

**New Computer Modern Math** (GUST FL). Regular, Book (slightly heavier,
for screens) and Bold math fonts, with matching `NewCM10-{Regular, Italic,
Bold, BoldItalic, Book, BookItalic}` text faces, sans and mono, plus a Sans
Math. Complete symbol coverage, kerning info, many stylistic sets, and a real
**bold math font**, which XITS, KpMath, Concrete, Erewhon, XCharter and Lete
Sans also have. *Now: shipped next to Latin Modern (`newcm`): regular and
bold math, four NewCM10 faces, and four faces each of NewCM Sans 10 and
NewCM Mono 10, the sans serif and typewriter companions; Book is not
shipped. New Computer Modern Sans Math is shipped too, with a profile of
its own, "Computer Modern Sans" in the sans serif section of the menu.*

**TeX Gyre Pagella, Termes, Bonum, Schola Math** (GUST FL). Already shipped
with TeXmacs together with their text faces, and already special-cased
with hand-tuned tables (`adjust_pagella.cpp` and friends). No MathKernInfo.
The hand-tuned tables stay and take precedence: they were crafted against
TeXmacs's own layout and are better than what the font declares. The MATH
table only supplies what they do not cover (variants and assemblies for
delimiters, constants for fractions, radicals and limits); the visual
sample shows where the two disagree. *Now: the menu calls them Palatino,
Times, Bookman and Schoolbook. Their typewriter companion is Inconsolata
(`inconsolata`, shipped), with TeX Gyre Cursor, a thin Courier which came
out larger than the text, as the fallback: the profiles say `(mono
"Inconsolatazi4, TeX Gyre Cursor")`.*

**TeX Gyre DejaVu Math** (Bitstream Vera / DejaVu license, free). Sans
serif math for DejaVu Sans and DejaVu Serif; TeXmacs already knows the
DejaVu text fonts (`math-dejavu`). Identical structure to the other TeX
Gyre math fonts. Companions in the `dejavu` collection. *Now: shipped
(`dejavu`) with the Serif, Sans and Sans Mono faces and the condensed
ones; menu entry "DejaVu".*

**STIX Two Math** (OFL). The most complete symbol set, MathKernInfo,
stylistic sets for alternate glyph shapes (upright integrals, calligraphic
versus script, and so on). Companions `STIXTwoText-{Regular, Italic, Bold,
BoldItalic, Medium, SemiBold}`. Ships next to STIX v1, which TeXmacs still
ships and special-cases (`rubber_stix_font.cpp`, `adjust_stix.cpp`). The font calls
itself "STIX Two Math", but `tt_font_name` normalizes every family starting
with "STIX" to "Stix", so inside TeXmacs the family is "Stix Two Math" and
documents must use that spelling (the sample document had to be fixed).
Its `displayOperatorMinHeight` is 1800, so display operators come out
large; `minConnectorOverlap` 100. *Now: shipped (`stix2`) with four STIX
Two Text faces; STIX Two has no sans serif or typewriter companion.*

**XITS Math** (OFL, a fork of STIX v1 by Khaled Hosny). Regular and Bold
math, `XITS-{Regular, Italic, Bold, BoldItalic}` text. Coverage like STIX
v1 but with italic corrections, `ssty` and kerning. The Bold math font is
partial (40 vertical variants, one assembly). Good fallback for users who
prefer Times; less compelling now that STIX Two exists. *Now: profiled,
not shipped; in the "Other" submenu when installed.*

**Libertinus Math** (OFL). Companion of Libertinus Serif and Sans, the
maintained successor of Linux Libertine, which TeXmacs already knows
(`adjust_libertine.cpp`). Fewer variants and assemblies (59 and 15) and
only 85% of the operators block, but the design is popular. TeXmacs should
map Linux Libertine users to Libertinus when both are present; that is not
done, the two are offered separately. *Now: shipped (`libertinus`), with
Libertinus Serif, Sans and Mono; menu entry "Libertinus".*

**KpMath** (OFL). Kp-Fonts in OpenType: Light, Regular, Semibold, Bold math,
plus Sans and Sans Bold math, with `KpRoman`, `KpSans` and `KpMono` text
faces in matching weights. Very rich MathKernInfo (700 glyphs) and many
stylistic sets. Alphabets partial (script 18, double-struck 20 of 52). Six math fonts: four
weights of the serif design, Light, Regular, Semibold and Bold, plus Sans and
SansBold, which are a design of their own. Valuable for presentations.
*Now: Regular, Bold, Sans and SansBold are shipped (`kp`, version 0.66)
with four faces each of KpRoman, KpSans and KpMono, as two menu entries,
"Kp Fonts" and "Kp Sans" (profile `KpMathSans`, set with the sans serif
family); Light and Semibold are not shipped.*

### Tier 2: good fonts with a narrower audience

**Asana Math** (OFL). Derived from Palatino-like pxfonts; wide coverage
(94%), MathKernInfo, but no companion text fonts of its own. Pair it with
TeX Gyre Pagella or with the pxfonts text faces. TeXmacs already lists it
(`math-asana`). *Now: profiled with Pagella text, not shipped; in the
"Other" submenu when installed.*

**Fira Math** (OFL). Sans serif math for Fira Sans, which TeXmacs supports
(`adjust_fira.cpp`, `fira-font` package). Coverage is limited (43% of the
symbol list, no script or fraktur alphabets, 61% of operators): TeXmacs's
virtual glyphs must fill the gaps, which is exactly what the smart font
does for missing symbols. Worth supporting because of Fira's popularity for
slides. Version 0.3.4, still evolving; upstream has released only the
regular weight. *Now: shipped in the `fira` directory, next to Fira Sans
and Mono; menu entry "Fira", the first of the shipped sans serif entries.*

**Erewhon Math**, **XCharter Math**, **Concrete Math**, **Euler Math**
(OFL, all by Daniel Flipo). Recent, well-made, consistent design of the
MATH tables (same 52 horizontal variants, 45 assemblies), MathKernInfo in
Erewhon. Text companions: Erewhon (Utopia), XCharter (Charter), Concrete
Roman for Concrete Math, and for Euler Math any upright text font, as with
TeX's Euler. TeXmacs already has "Concrete" and "Euler new roman" math
options through Metafont; these are their OpenType replacements. *Now: all
four are shipped and in the serif section of the menu as Utopia, Charter,
Concrete and Euler: Concrete Math with the Concrete faces of CM Unicode
(CMU Concrete), Euler Math with TeX Gyre Pagella text, as with `eulervm`,
and Heros and Inconsolata as companions.
XCharter, Concrete and Erewhon use their bold math faces; the bold Erewhon
math file names its family like the text face, and the shipped database
lists it as the Bold style of Erewhon Math.*

**Garamond Math** (OFL). For EB Garamond; huge MathKernInfo (2094 glyphs)
but almost no italic corrections (44), so kerning must come from the kern
table. Text companions are EB Garamond from Google Fonts or TeX Live.
*Now: shipped (`garamond`) with four EB Garamond faces; menu entry
"Garamond" in the serif section, Inconsolata for typewriter text.*

**Lete Sans Math** (OFL). Sans math for Lato; 95% coverage, kerning.
Companions: Lato. *Now: shipped (`letesans`), regular and bold, with the
math font itself as its text companion rather than Lato, which is not
shipped; menu entry "Lete Sans" in the sans serif section.*

**IBM Plex Math** (OFL). Complete coverage, kerning, `mark` positioning,
and the large Plex Sans, Serif and Mono families. `displayOperatorMinHeight`
is 2339, the largest of all, so display integrals will be very tall unless
capped. *Now: shipped (`plex`) with four faces each of Plex Sans, Serif
and Mono; menu entry "IBM Plex" in the sans serif section, with the text
set in Plex Sans (family `ss`); the display operators are capped at two
em.*

**Old Standard Math** (OFL). Complete coverage, no kerning info; for Old
Standard text (historical and slavistic typography). *Now: shipped
(`oldstandard`) with three Old Standard faces (there is no bold italic);
menu entry "Old Standard" in the serif section, with TeX Gyre Heros and
Inconsolata as sans serif and typewriter companions.*

**GFS Neohellenic Math** (OFL). Greek sans; 41% of the alphanumerics
block, 98% of the operators. Niche. *Now: shipped (`gfsneohellenic`) with
four GFS Neohellenic faces; menu entry "GFS Neohellenic" in the sans serif
section, Inconsolata for typewriter text.*

**Noto Sans Math** (OFL). Release 3.000 (google/fonts) has a MATH table:
all of the `unicode-math` list, the operators and alphanumerics blocks,
no MathKernInfo, `kern` and `mark` positioning, and a
`displayOperatorMinHeight` of 2300. Companions: Noto Sans and Noto Sans
Mono (master `Noto`, which also has Noto Serif). *Now: shipped (`noto`)
with four Noto Sans faces and Noto Sans Mono regular and bold; menu entry
"Noto Sans" in the sans serif section, the text set in Noto Sans (family
`ss`). Noto Sans Math is a master of its own in the database, as Fira Math
is.*

### Tier 3: do not integrate

**STIX Math v1** and the Neo Euler of 2010 in `tm-fonts` are superseded.
**Cambria Math** cannot be redistributed and TeXmacs users on Windows may
have it, so activation by name should work, but no special support beyond
the generic path is warranted. (**Noto Sans Math** used to be listed here
for want of a MATH table; it now has one, see Tier 2.)

## 4. What TeXmacs has for named fonts

When the survey was written, the per-font knowledge was spread over the
places below, which is what an integration should consolidate. They are all
still there, for the traditional fonts and the hand-tuned ones; the
profiles of section 5.1 (`math_font_profiles.cpp`, `fonts-opentype.scm`)
now carry what the OpenType math fonts need, and the menus of
`fonts-opentype.scm` list them:

- `unicode_font.cpp`: the constructor ladder that installs hand-tuned
  script corrections for STIX, TeX Gyre, Libertine, Biolinum and Fira from
  the `adjust_*.cpp` files and for Papyrus inline, and the `tex_gyre_operators` table
  of glyphs without Unicode names.
- `font.cpp`: `get_math_type` from the file name prefix, and
  `make_rubber_font`, which selects `rubber_stix_font` by name.
- `rubber_stix_font.cpp`: knows the STIX size fonts (`STIXSizeOneSym`,
  `STIXIntegralsD`, ...) by file name.
- `smart_font.cpp`: `is_math_family` (fixed list), and `tex_gyre_fix` and
  `kepler_fix`, which append " Math" to the family for math shapes, with
  `math_fix` applying them inside `math=` rules (`stix_fix` exists but every
  call is commented out);
  `supports_big_operators` in `poor_rubber.cpp`.
- `font_translate.cpp`: the map from legacy names (`math-stix`,
  `math-pagella`, `math-asana`, ...) to database family names.
- `TeXmacs/progs/generic/document-menu.scm` and `document-edit.scm`: the
  "Mathematical font" menu, gated by `font-exists-in-tt?` on a file name,
  and `init-font`, which pairs a text font with its math font
  (`(init-font "pagella" "math-pagella")`).
- `TeXmacs/progs/fonts/fonts-math.scm`: legacy `unicode-math` rules that
  combine `texgyre*-math` with the italic and bold text faces for the old
  `unimath` font.
- `TeXmacs/fonts/font-substitutions.scm`: family-level substitutions.

## 5. Proposed integration

### 5.1 A single table of math font profiles

*Implemented (first version) in `src/Graphics/Fonts/math_font_profiles.cpp`
and `TeXmacs/progs/fonts/fonts-opentype.scm`, where a profile is declared
with `define-math-font-profile`, in a reduced form of the proposal below: keys `file`, `text`,
`text-file`, `sans`, `mono`, `family`, `letters`, `bold-math`, `menu` and
`group`, with a menu label in the LaTeX style (`(menu "Palatino") (group
"Serif")`); the text-to-math mapping in `smart_font_bis`, the letter
routing and the menus. `bold-math` is recorded but not consulted: a bold
math face is found through the master of the math font in the database.
Alphabets, rubber policy and quirks are still to come; the display cap is a
constant for all fonts. `sans` and `mono` may list alternatives separated
by commas, the first one the database knows being used (`"Inconsolatazi4,
TeX Gyre Cursor"`), and a profile without `file` only gives companions to a
text font without mathematics: the Palatino of macOS takes Inconsolata for
its typewriter text instead of the closest monospaced font.*

Introduce one data structure, filled by hand, consulted by all the places
above. In C++ it can be a static table in a new
`src/Graphics/Fonts/math_font_profiles.cpp`, or it can live in Scheme
(`TeXmacs/progs/fonts/fonts-opentype.scm`) and be pushed to C++ at boot,
which is easier to edit; the C++ side needs it before any typesetting, so a
Scheme definition read once at startup is fine. A profile looks like:

```scheme
(math-font-profile
  (name "TeX Gyre Pagella Math")      ; family name in the font's name table
  (file "texgyrepagella-math")        ; file name without suffix
  (aliases "math-pagella" "pagella")  ; legacy math-font values
  (text "TeX Gyre Pagella")           ; text family (rm, it, bf, bi)
  (sans "TeX Gyre Heros")
  (mono "TeX Gyre Cursor")
  (bold-math #f)                      ; family of a bold math font, or #f
  (letters math)                      ; where math italic letters come from:
                                      ;   math  = plane 1 letters of this font
                                      ;   text  = italic text companion
  (alphabets bold italic script fraktur bbb sans mono) ; real alphabets
  (rubber math)                       ; use MATH variants for delimiters
  (big-operators math)                ; use MATH variants for big operators
  (display-operator-max 1.6)          ; cap in em for displayOperatorMinHeight
  (menu "Pagella" "TeX Gyre")         ; label and group in the font menus
  (license "GUST Font License")
  (quirks radical-gap))               ; named tweaks handled in C++
```

What each field replaces:

- `name`, `file`, `aliases`: `font_translate.cpp` entries, the
  `font-exists-in-tt?` tests in the menus, `tex_gyre_fix` / `math_fix`.
- `text`, `sans`, `mono`: `init-font` pairing, the `unicode-math` rules in
  `fonts-math.scm`, and the smart font's choice of subfonts. With a profile
  the smart font can build the sequence
  `math=<name>,<text>` itself when the user selects either the text font or
  the math font, so `<with|font|TeX Gyre Pagella>` and
  `<with|math-font|math-pagella>` behave the same.
- `letters`: replaces `is_math_family`. For `math`, letters in math mode
  are rewritten to the plane 1 code points of the math font (as
  `REWRITE_MATH` does for the TeX math fonts), which is what makes italic
  corrections and MathKernInfo available. For fonts whose italic alphabet
  is worse than the text italic, or missing (Fira Math has no script or
  fraktur), `text` keeps the current behaviour.
- `alphabets`: which of bold, script, fraktur, double-struck, sans, mono
  come from the font and which are emulated with virtual fonts (`poor_bbb`,
  `emu-*`). The survey shows script is the alphabet most often incomplete.
- `rubber`, `big-operators`: replace `supports_big_operators` and the name
  tests in `make_rubber_font` and `use_poor_rubber`.
- `display-operator-max`: some fonts declare very large display operators
  (STIX Two 1.8 em, IBM Plex 2.3 em); a cap keeps documents that mix fonts
  consistent.
- `menu`: generates the "Mathematical font" and "Font" menus from the
  profiles that are installed, instead of the hand-written lists.
- `quirks`: a closed set of C++ switches for hand tuning that is not a
  correction table, for
  example the radical gap of Latin Modern or the integral spacing that
  `concat_math.cpp` currently keys on `MATH_TYPE_STIX` and
  `MATH_TYPE_TEX_GYRE`. Each quirk has a name so that it is greppable.

Activation stays generic: any font with a MATH table gets
`MATH_TYPE_OPENTYPE` and the table-driven layout, unless a hand-tuned branch
of the constructor ladder claims it while the hand tuning is on (STIX, the
four TeX Gyre fonts, Libertine, Fira). The profile only adds
knowledge that is not in the font. A font without a profile (Cambria Math,
a new release) still works with defaults: `letters text`, alphabets
detected from the cmap, no cap.

### 5.2 Steps

1. **Generic path to parity.** Finish the items in `opentype-math-design.md`
   that affect all fonts: radical placement, direct variant selection in
   `get_delimiter`, top accent attachment, horizontal variants for wide
   accents. Use Latin Modern Math and STIX Two Math as the two references,
   compared against TeX's layout of the same formulas. *Done.*
2. **Profiles and the letter routing.** Implement the profile table and make
   `smart_font.cpp` use it for `is_math_family`, the family fix-ups and the
   text companions. This is the change that lets an OpenType math font
   supply its own italic letters. Add the profiles for Tier 1. *Done for
   the text companions and the letters; `is_math_family` is still a fixed
   list, and `tex_gyre_fix` still names the four TeX Gyre fonts.*
3. **Layer the MATH data under the hand-tuned tables.** The `adjust_*.cpp`
   corrections and the other per-font tuning keep precedence: when a
   correction table has an entry for a glyph, it wins over the italic
   correction and cut-in kern of the MATH table; when a font has no entry,
   the MATH data is used. Concretely, the constructor ladder in
   `unicode_font.cpp` must run the MATH activation *before* the per-family
   branches, so that a hand-tuned branch can override individual fields
   (`yfrac`, script shifts, corrections) while leaving the rest to the
   table. New profiles may add hand-tuned tables of their own where the
   font data is poor (the survey shows which fonts lack MathKernInfo or
   italic corrections). Ship STIX Two Math and its text faces next to STIX
   v1. *Done.*
4. **Menus and legacy names.** Generate the font menus from installed
   profiles; keep the `math-*` aliases so that old documents keep working.
   *Done: the font button of the focus bar and the end of Document > Font >
   Mathematical font list the installed profiles; the hand-written list of
   traditional math fonts stays before them.*
5. **Weights.** Use the bold math fonts (NewCM Math Bold, KpMath Bold, XITS
   Math Bold) for `math-font-series bold` instead of stroking, through the
   `bold-math` field. *Done, but through the database rather than the
   field: a bold face attached to the master of the math font is used, which
   covers New Computer Modern, Kp, XCharter and Concrete; `bold-math` is not
   read.*
6. **Tier 2 profiles**, then the remaining quirks per font as they show up
   in the samples. *Profiles done, for every font of the survey.*

### 5.3 Tests to add

- One row per profiled font in `tests/opentype/samples/math-overview.tm`,
  and a second sample with letters, scripts and kerning pairs (`V_a`, `f^2`,
  `A^T`, `W_i`, integrals with limits) where MathKernInfo makes a visible
  difference.
- A unit test that loads every installed profile: the file exists, the
  family name in the name table matches, the text companions resolve, the
  alphabets declared as real are present in the cmap.
- For each Tier 1 font, expected sizes of the display integral and of the
  first assembled parenthesis, computed from the MATH table as in
  `opentype_font_test.cpp`.

*Status: `math-variants.tm` is the second sample; `math-overview.tm` has
ten font rows, not one per profile. `test_profile_file` checks every
installed profile's file, name table and database listing, not its
companions or alphabets. The per-font sizes are not tested.*

## 6. Shipping

TeXmacs ships these math families, besides the TeX Gyre text and math
fonts and STIX v1 which were already there. The choice follows the fonts LaTeX
users know: one entry of the font menu for each of the usual pdfLaTeX
pairings (`lmodern`, `newtx`, `newpx`, `libertine`, `kpfonts`,
`erewhon`, `XCharter`, `eulervm`, `concmath`, `stix2`), and Fira, Kp
Sans and Computer Modern Sans for slides; then the Tier 2 fonts with their
text faces: Garamond and Old Standard in the serif section of the menu,
Lete Sans, Noto Sans, IBM Plex and GFS Neohellenic in the sans serif
section (the menus are sorted by label). Inconsolata is shipped as the
typewriter companion of the TeX Gyre profiles, Euler, Garamond, Old
Standard and GFS Neohellenic.

| Directory | Fonts | Size | License |
|---|---|---|---|
| `TeXmacs/fonts/truetype/lm` | Latin Modern Math, and the 10 pt Latin Modern Roman (four faces), Sans (four) and Mono (regular, italic) | 1.7 MB | GUST Font License |
| `TeXmacs/fonts/truetype/newcm` | New Computer Modern Math regular and bold, New Computer Modern Sans Math, and four faces each of NewCM10, NewCM Sans 10 and NewCM Mono 10 | 8.6 MB | GUST Font License |
| `TeXmacs/fonts/truetype/stix2` | STIX Two Math and four STIX Two Text faces | 2.0 MB | SIL OFL 1.1 |
| `TeXmacs/fonts/truetype/kp` | KpMath regular and bold, KpMath-Sans and SansBold, and four faces each of KpRoman, KpSans and KpMono, all version 0.66 | 2.1 MB | SIL OFL 1.1 |
| `TeXmacs/fonts/truetype/fira` | Fira Math 0.3.4, next to the Fira Sans and Fira Mono faces already shipped | 0.2 MB added | SIL OFL 1.1 |
| `TeXmacs/fonts/truetype/libertinus` | Libertinus Math, Serif (four faces), Sans (three: there is no bold italic) and Mono | 2.6 MB | SIL OFL 1.1 |
| `TeXmacs/fonts/truetype/erewhon` | Erewhon Math regular and bold, and four Erewhon faces | 1.5 MB | SIL OFL 1.1 |
| `TeXmacs/fonts/truetype/xcharter` | XCharter Math regular and bold, and four XCharter faces | 1.0 MB | SIL OFL 1.1 (math), Bitstream Charter license (text) |
| `TeXmacs/fonts/truetype/concrete` | Concrete Math regular (0.65) and bold (0.60), and the four Concrete faces of CM Unicode | 1.5 MB | SIL OFL 1.1 |
| `TeXmacs/fonts/truetype/euler` | Euler Math, set with TeX Gyre Pagella text | 0.4 MB | SIL OFL 1.1 |
| `TeXmacs/fonts/truetype/inconsolata` | Inconsolatazi4 regular and bold (1.015), the typewriter companion of the TeX Gyre profiles | 0.1 MB | SIL OFL 1.1 |
| `TeXmacs/fonts/truetype/letesans` | Lete Sans Math regular and bold (0.45) | 0.6 MB | SIL OFL 1.1 |
| `TeXmacs/fonts/truetype/garamond` | Garamond-Math and four EB Garamond faces | 2.3 MB | SIL OFL 1.1 |
| `TeXmacs/fonts/truetype/oldstandard` | Old Standard Math and three Old Standard faces | 1.7 MB | GUST Font License (math), SIL OFL 1.1 (text) |
| `TeXmacs/fonts/truetype/gfsneohellenic` | GFS Neohellenic Math and four GFS Neohellenic faces | 1.1 MB | SIL OFL 1.1 |
| `TeXmacs/fonts/truetype/plex` | IBM Plex Math and four faces each of IBM Plex Sans, Serif and Mono | 2.2 MB | SIL OFL 1.1 |
| `TeXmacs/fonts/truetype/noto` | Noto Sans Math 3.000, four Noto Sans faces, Noto Sans Mono regular and bold | 4.1 MB | SIL OFL 1.1 |

Each directory carries the license text and a `README.md` with the upstream
address, the version and the copyright of every file. The families are
registered in the shipped global font database
(`TeXmacs/fonts/font-database.scm`, `font-features.scm`,
`font-characteristics.scm`), so a new installation finds them with no disk
scan. Users with an existing local database get them at the next start:
the local database is merged with the shipped one whenever the shipped one
has changed or the local one has lost entries (see section 4.1 of
`doc/font-system-review.md`).

Only one weight and one optical size of each text family is shipped. A
document typeset with the whole upstream family available can therefore
differ slightly, as the extra optical sizes of Latin Modern and the Medium
and SemiBold weights of STIX Two Text are then used instead. Upstream has
released only the regular weight of Fira Math, so bold Fira mathematics is
emulated. Erewhon-Math-Bold names its family like the Erewhon text face;
the shipped database lists it as the Bold style of Erewhon Math, which a
scan of the disk alone would not. Likewise by hand: Noto Sans Math is a
master of its own, as Fira Math is, and the IBM Plex entries list the
regular file before the medium one, which a scan files as regular too.
A text font which a shipped math font sets as roman text (EB Garamond, Old
Standard, ...) is left out of the menu of text fonts, which offers it with
its mathematics instead; a sans serif text (Noto Sans, IBM Plex Sans) does
not hide its master, so Noto Serif and IBM Plex Serif stay there.

Everything else in the survey is picked up from the system or from TeX
Live through `TEXMACS_FONT_PATH` and the font database; the profiles in
`TeXmacs/progs/fonts/fonts-opentype.scm` activate when the files are
found, and the font menus list exactly the profiled fonts that are
installed. Two profiled fonts are not shipped: XITS and Asana, in the
submenu of other fonts.
