# Emoji and color fonts

TeXmacs draws the emoji in color with a font which comes with it,
**Twemoji Mozilla** (`TeXmacs/fonts/truetype/twemoji`), on every platform
and in the browser, so that a document looks the same everywhere. The
preference "emoji font" (General: "Emoji") takes those of the system
instead: Apple Color Emoji, Segoe UI Emoji or Noto Color Emoji
("system"), or a font by the name of its file. The
fonts keep one treatment on all the interfaces: the glyphs come from
FreeType and are drawn by the renderers of TeXmacs, not by the system.

![Emoji in a document](screenshots/emoji.png)

## The formats of color fonts

| Format | A glyph is | Main fonts | In TeXmacs |
|---|---|---|---|
| COLR version 0 + CPAL | layers of ordinary outlines, each in a color of the palette | Twemoji Mozilla, Segoe UI Emoji | **drawn** |
| COLR version 1 | a graph of outlines with gradients, transforms and blending | Noto Color Emoji (new builds), Nabla | one color (the union of the outlines, if any) |
| sbix | a PNG at several sizes | Apple Color Emoji | **drawn** (as pictures) |
| CBDT/CBLC | a PNG or a bitmap at several sizes | Noto Color Emoji (Android, Linux) | **drawn** (as pictures) |
| SVG in OpenType | an SVG document per glyph | some Adobe fonts | not drawn |

FreeType reads all of them: `FT_Get_Color_Glyph_Layer` and
`FT_Palette_Select` for COLR version 0 (since FreeType 2.10), with
`FT_LOAD_COLOR` the bitmaps of sbix and CBDT, `FT_Get_Color_Glyph_Paint`
the graph of COLR version 1 (which it does not draw), and the SVG through
hooks.

## How an emoji is typeset

1. **The text.** What makes a sequence is in `tm_grapheme_end`
   (`Data/String/analyze.cpp`): an emoji with the variation selectors,
   the skin tones, the tags and the keycap after it, the emoji joined by
   U+200D, the two regional indicators of a flag, and a digit, `#` or `*`
   followed by U+FE0F and U+20E3. The typesetter cuts a string into pieces
   (the `advance` of the languages, `System/Language/text_language.cpp`),
   a symbol a piece of its own, and a sequence one piece.
2. **The font.** `smart_font_rep::resolve` (`Graphics/Fonts/smart_font.cpp`)
   takes the characters which are emoji by default (`is_emoji_presentation`,
   the property Emoji_Presentation of Unicode) and the marks of the
   sequences (`is_emoji_mark`) from the emoji font, before any other font:
   some text fonts have a few emoji in black and white. A symbol which is
   text by default, like the heart U+2764, keeps the font of the text, but
   goes to the emoji font when U+FE0F follows it (`emoji_before_selector`,
   in `advance`), or when no other font has it (at the end of `resolve`).
3. **The sequences.** In the emoji font a sequence is a ligature of the
   GSUB feature `ccmp` (`parse_gsub_ligatures`, `Plugins/Freetype/tt_tools.cpp`,
   GSUB lookups of type 4); `unicode_font_rep::read_glyph` reads the
   longest ligature which starts at a character, wherever the font reads
   its glyphs (extents, positions, drawing). What is left of a sequence
   which is not a ligature of the font (a selector after a letter, a
   joiner between two emoji which make nothing together) is not drawn and
   takes no room (`is_emoji_mark` in `tt_face.cpp` and `unicode_font.cpp`).
4. **The editor.** A sequence is one character for the editor: Delete
   and Backspace remove it whole (`tm_grapheme_end`, `tm_grapheme_start`,
   in `edit_delete.cpp`), and a cursor key which would stop inside it
   (the characters of a ligature all stand at the left of its picture,
   `get_xpositions`) goes to its other side (`out_of_emoji` in
   `edit_cursor.cpp`, with `tm_grapheme_inside`).
5. **The metrics.** The base glyph of a COLR glyph is often empty: the
   metrics are those of its layers (`tt_font_metric_rep::get`), and its
   bitmap, for whatever asks for one, is their union in one color
   (`color_glyph_union`).
6. **The drawing.** `unicode_font_rep::draw_color_glyph` draws the layers
   from the bottom up, each as an ordinary glyph of the same font (a
   native glyph, `0xc000000` plus its index) with the pen set to its color
   (the color of the text where the palette says so, and the opacity of
   the text for all of them). Every renderer thus draws an emoji as it
   draws text, and a PDF keeps it as vectors. Seen so far: the screen of
   Vue (as `TEXMACS_VUE_SNAPSHOT` saves it), the screen of Cocoa (as
   `TEXMACS_NS_SNAPSHOT` saves it), drawn by the GPU and by Core Graphics
   (`TEXMACS_NS_GPU=0`) in the same colors, the screen of Qt 6 (as
   `gui-test-snapshot` saves it), the PDF of PDFHummus (the Qt builds),
   where the font is embedded as a subset, the PDF of the MuPDF writer
   (`TEXMACS_PDF_MUPDF=1` or the preference `native pdf renderer`, and
   always in the browser), where the font is embedded as a subset, and
   the PostScript export (with its PDF through Ghostscript, where the
   glyphs become Type 3 fonts), and the browser (Firefox, through
   `misc/wasm/browser-run.mjs`: the screen and the PDF of the page; its
   FreeType, the one of MuPDF, has the layers of COLR).

## The names of the emoji

An emoji has a name in the universal encoding of TeXmacs, as a Greek letter
has (`<alpha>`): `<emoji-grinning-face>` for U+1F600, and one name for a
whole sequence, `<emoji-family-man-woman-girl>` (U+1F468 U+200D U+1F469
U+200D U+1F467), `<emoji-red-heart>` (U+2764 U+FE0F), `<emoji-flag-italy>`,
`<emoji-flag-england>` (a flag of tags), `<emoji-keycap-1>`,
`<emoji-thumbs-up-medium-skin-tone>`, and the components alone
(`<emoji-medium-skin-tone>`, `<emoji-red-hair>`).

- **The names** are those of the LaTeX package `emoji`
  (`\emoji{grinning-face}`): the CLDR short names of Unicode, in small
  letters, without accents and without `: , . ' ( ) & "`, with a hyphen for
  a space, `hash` and `asterisk` for the keycaps of `#` and `*`. The 3521
  entries of the package (v0.2.2, Emoji 13.1) are all fully-qualified
  emoji of ours, and 3514 have the same name; 7 were renamed by CLDR since
  (`hugging-face` is `smiling-face-with-open-hands`, `flag-turkey` is
  `flag-turkiye`...). The shortcodes of GitHub or Slack (`:grinning:`) were
  not taken as names: each service has its own list, and many emoji have
  none; the import of LaTeX knows those of the package (below).
- **The table**, `TeXmacs/langs/encoding/emoji-names.scm` (ASCII: the code
  points as `#XXXX`), has the fully-qualified emoji of the file
  `emoji-test.txt` of Unicode and its components: 3972 names for Emoji 18.0.
  It is generated, not edited:
  `python3 misc/emoji/make-emoji-names.py emoji-test.txt > TeXmacs/langs/encoding/emoji-names.scm`
  (from <https://unicode.org/Public/emoji/latest/>), and
  `--check <emoji-table.def>` lists the names which differ from those of the
  LaTeX package. `--aliases emoji-test.txt <emoji-table.def>` writes
  `TeXmacs/langs/encoding/emoji-latex-aliases.scm`, the other names of the
  package: its 879 aliases after the shortcodes of GitHub (`smile`, `tada`,
  `+1`) and its 7 names which are not ours, 886 in all, each with the name
  of TeXmacs it stands for.
- **The conversions.** The table is in the converters from the universal
  encoding to UTF-8 and to source code (`converter_rep::load`,
  `Data/String/converter.cpp`), so that a name is written out as its
  characters (HTML, Markdown, the clipboard, a source file). The other way,
  `utf8_to_cork` takes the longest emoji of the table at each character
  (`match_emoji_name`): a text in UTF-8 (typed, pasted, imported) gets the
  names, as it gets `<alpha>`; a character which has a symbol of its own
  keeps it (U+2648 is `<aries>`, U+25FD `<square>`), and what is not a
  fully-qualified emoji stays in code points (U+2764 alone, a skin tone
  after a face, a joiner between two emoji which make nothing together).
  `cork_to_utf8 (utf8_to_cork (s))` is `s`; the table is also in the
  strict and the T2A conversions. The documents written before, with
  `<#1F600>`, are not converted on load: both forms are valid and look the
  same. `emoji_name`, `emoji_names`, `emoji_code_points` and
  `emoji_to_code_points` (`converter.hpp`; `emoji-name` in Scheme) go from
  one to the other; `is_emoji_name` is true of one symbol only.
- **Sequences partly by names.** An input method may send a family code
  point after code point (Qt split the text it commits so, before it was
  converted): its people come by their names and its joiners as code
  points, `<emoji-man><#200D><emoji-woman>...`. Everything which cuts the
  text into characters takes a name as its code points: `code_at`
  (`analyze.cpp`, behind `tm_grapheme_end` and `tm_grapheme_start`) takes a
  name as its first code point, so that such a sequence, a name with a
  skin tone or a selector after it, or names among the code points of an
  older document, is one character for the typesetter and for the editor;
  a flag by its name takes no regional indicator after it. And the editor
  writes the emoji before the cursor by the name of its whole sequence
  after each insertion (`join_emoji`, `edit_text.cpp`, with
  `emoji_names`): the family typed so is stored as
  `<emoji-family-man-woman-girl>`, a skin tone typed after a hand as
  `<emoji-thumbs-up-medium-skin-tone>`, also when the hand is a code point
  of an older document. Only the text from the start of that emoji up to
  the cursor is renamed, never what follows the cursor (a joiner typed
  between two emoji leaves the second one as it was), and the cursor stays
  right after what was typed; nothing else of a document is renamed. Qt, Qt 6
  and Qtwk now send what an input method commits one character of TeXmacs
  after the other (an emoji sequence by its name, whole).
- **Search** finds an emoji by its name and by its code points, whatever
  form the text has: when the pattern has an emoji (`has_emoji`), the
  pattern and each string are written with their emoji in code points once
  (`emoji_to_code_points`, with the positions of the string, only for a
  string which has names) and searched as text (`search_string` and
  `match` of `tree_search.cpp`; `test_sub` of `edit_search.cpp` compares
  with `emoji_match`). A hit is a whole emoji: `<emoji-man>` (or U+1F468)
  is found alone, and neither inside `<emoji-family-man-woman-girl>` nor
  inside the same family in code points (the ends of a hit must not be
  inside a sequence, `tm_grapheme_inside`). On a text of 2 MB, the search
  of an emoji by its name takes about as long as that of a word.
- **The typesetting.** The emoji font draws the names as their code points
  (its rewrite is `REWRITE_EMOJI`, which leaves code points as they are),
  and `smart_font_rep::resolve` gives a name the font its first code point
  gets: the emoji font, so that a name and the code points around it make
  one run and one ligature; or, when the emoji font has not that emoji (a
  newer one, `<emoji-face-with-bags-under-eyes>`), the font which the
  fallback finds for the code point, through a font of its own
  (`emoji-name`) which gives it the code points. Everything below works on
  the code points as before (the ligatures, the layers, the text of a PDF).
- **The footer** says which character is before the cursor: an emoji
  sequence whole (`tm_grapheme_start`, in `compute_text_footer` and in
  `footer-char-info` of the interactive footer), with its name, also for
  an emoji in code points (`<#1F600> (emoji-grinning-face)`). Vue draws
  the texts of its widgets with one font, which has no emoji (a square):
  `layout_text_emoji` (`vue_gui.cpp`) draws the emoji of a text with the
  emoji font (`get_emoji_font`, at the em of the font of the widgets). Qt
  and Cocoa give the text to the system, which finds an emoji font; Cocoa
  no longer escapes the symbols of the right footer (`tm_var_encode` made
  `<alpha>` of a footer literal).
- **LaTeX**: `\emoji{grinning-face}` is imported as `<emoji-grinning-face>`
  (`latex_command_to_tree`, `fromtex.cpp`), and so are the other names of
  the package (`\emoji{smile}`, `\emoji{+1}`, `\emoji{hugging-face}`, by
  `emoji_from_latex`). The export writes an emoji which the package has
  as `\emoji{...}` by its name there, whether the document has it by its
  name or in code points (`tmtex-string` names the sequences first,
  `emoji-names`; `tmtex-emoji` looks the name up, `emoji-latex-name`, in
  `TeXmacs/langs/encoding/emoji-latex-names.scm`, made by
  `make-emoji-names.py --latex-names` from the `emoji-table.def` of the
  package as TeX Live 2025 has it, v0.2.2: its 3521 emoji, 7 of them by
  their older names, `\emoji{hugging-face}` for
  `<emoji-smiling-face-with-open-hands>`); in a formula in a `\text`, so
  that it scales in the scripts. What is left of a sequence which is not
  one emoji is dropped (the variation selectors and the joiners after an
  emoji: `<emoji-man><#200D><emoji-woman>` is `\emoji{man}\emoji{woman}`).
  A sequence which lacks a variation selector (minimally-qualified or
  unqualified in `emoji-test.txt`: U+1F3C3 U+200D U+2640, the woman running
  without the U+FE0F after the sign) is named as the fully-qualified emoji
  by `emoji_names` and `emoji_name` (the export, the editor after an
  insertion, the footer), from `TeXmacs/langs/encoding/emoji-names-unqualified.scm`
  (`make-emoji-names.py --unqualified`, 1065 sequences of several code
  points; a character alone, U+2764, is not taken: it is the heart of the
  text). `utf8_to_cork` names such a sequence too (a text pasted), but not
  a keycap without its selector (`2026<#20E3>` stays as it is typed or
  pasted; the export, `emoji_all_names`, names it `\emoji{keycap-6}`).
  A symbol of TeXmacs which is a character of the blocks of the emoji
  (`<female>` and `<male>`, which `utf8_to_cork` makes of U+2640 and
  U+2642, the sign a key gives) counts as its code point in a sequence
  (`emoji_component_code`, used by `code_at` and by the search): a
  person running, a joiner and `<female>` typed one after the other are
  `<emoji-woman-running>`, and the sign alone stays `<female>`. The search
  compares the emoji in one form (`emoji_search_form`: names and such
  symbols as code points, without the variation selectors), so that the
  emoji typed, pasted and of an older document are found as one.
  An emoji which the package has not (a newer one) is written as
  `\tmemojichar{<characters>}{U+1FAE9}{129769}` (the last, its first code
  point, for `\iffontchar`). An `(emoji "name")` tag (an `\emoji` of an
  import which did not know the name; the import reads the name as it is
  written, `_` included, and takes the shortcodes of GitHub with `_` for
  `-`, `\emoji{white_check_mark}`) is the emoji when the package knows the
  name, else `\texttt{:name:}` with the characters special for TeX
  escaped, never an `\emoji` which the package would stop on. A document
  with these gets in its preamble, and only then (`latex-texmacs-preamble`,
  `latex-define.scm`), for `\emoji`:

  ```
  \usepackage{iftex}
  \ifLuaTeX
    \usepackage{emoji}
  \else
    \DeclareRobustCommand{\emoji}[1]{\texttt{:\detokenize{#1}:}}
    (and, for the PDF bookmarks, :name: with its hyphens kept)
  \fi
  ```

  and for `\tmemojichar`: with LuaLaTeX and XeLaTeX (`\ifLuaTeX`,
  `\ifXeTeX`, which every iftex has, the old v0.2 of TeX Live 2019 and
  before too) the characters if the current font
  has them (`\iffontchar`), else `[U+1FAE9]`; with pdfLaTeX `[U+1FAE9]`;
  in the bookmarks, the characters or the code points. The package needs
  LuaLaTeX (it stops the other engines with an error), and the exporter
  does not know which engine the document will be compiled with (it has no
  setting for it): LuaLaTeX draws the emoji in color, pdfLaTeX and XeLaTeX
  write their names as shortcodes, literally (`:flag-congo---kinshasa:`,
  also in the bookmarks). Checked with the three engines of TeX Live 2025.
  `\emoji{...}` comes back as the same emoji on import. TMML writes
  `<#1F600>` as a `<tm-sym>`, since it would come back by its name (so is
  `<#3B1>`, which comes back as `<alpha>`), and a name as its character.

## Not done yet

- **Segoe UI Emoji** (Windows, COLR) should be drawn as Twemoji is, but has
  not been tried.
- **The preference** takes effect at the next start, since the fonts are
  made once; it could be followed at once.
- **The text of the emoji of a font of bitmaps** in MuPDF has a line of its
  own for each emoji (MuPDF lays out the ActualText of an image so); the
  characters are right.
- **COLR version 1**: its graph turned into an SVG fragment per glyph and
  drawn by the SVG renderer (resvg or ThorVG), or only its solid fills.
- **MuPDF** (1.28.5) does not draw color fonts itself: it loads every glyph
  with `FT_LOAD_NO_BITMAP` and fills it in one color, and its fallback for
  emoji is the black and white Noto Emoji. It draws ours because they come
  to it as colored text. For the next steps it has what is needed: images
  for the bitmaps of sbix and CBDT (`fz_fill_image`, and images in its
  PDF), and linear and radial shadings for the gradients of COLR version 1
  (`fz_fill_shade`, vectors in its PDF); its SVG reader has no gradients,
  so the way through SVG does not work there. Cocoa and Qt would do the
  same with Core Graphics and Qt: one treatment of the layers everywhere,
  with each renderer drawing images and gradients its own way.
- **Spaces around emoji in the text of a PDF**: MuPDF leaves out the
  spaces between two emoji (a character it takes from an ActualText has
  no advance, so that the next gap seems too wide for a space), Preview
  the space before an emoji which follows a word. The characters
  themselves come out right in both.
- **Ghostscript** cannot read the text of the emoji: it ignores ActualText,
  splits the two units of UTF-16 of a character beyond U+FFFF, and writes
  the layers as their glyph numbers.
- A choice of another emoji font (a preference): the font is
  `SHIPPED_EMOJI_FONT` in `smart_font.cpp`.

## The emoji fonts of the systems

- **Bitmaps.** A font made of bitmaps only (sbix, CBDT) has sizes of its
  own: the face takes the largest (`init_strike`, `set_size` in
  `tt_face.cpp`: `FT_Select_Size` instead of `FT_Set_Char_Size`, which
  fails), and its metrics are those of that size, scaled
  (`strike_metric`). `unicode_font_rep::draw_color_bitmap` draws an emoji
  as a picture: the bitmap of the fixed size (`tt_color_bitmap`, BGRA,
  premultiplied) averaged down to the pixels of the renderer, cached, and
  drawn by `draw_picture`, which every renderer has (a PDF keeps it as an
  image). Its text in a PDF: the picture in an ActualText span, and
  invisible glyphs of the shipped Twemoji for the readers of the
  character maps; a font of bitmaps is not embedded (Apple Color Emoji has
  190 MB).
- **Apple Color Emoji** has no GSUB: its sequences are ligatures of its
  AAT `morx` table, a state machine. `aat_morx_apply` (`tt_tools.cpp`)
  runs the subtables of the default features (non contextual, contextual,
  ligatures, as Apple and HarfBuzz read them) on the glyphs of a sequence
  (`tt_face_rep::morx_sequence`, cached), and `read_glyph` takes the glyph
  when the sequence makes one. The file is read by FreeType, not into
  memory, and the tables come from `FT_Load_Sfnt_Table`.
- **Noto Color Emoji** has its sequences in GSUB, but takes the U+FE0F of
  the keycaps away with lookups which are not read (multiple and
  contextual substitutions): the ligatures pass over a variation selector
  they do not list.
- A character which the font of the system lacks comes from the shipped
  font (`resolve_emoji`).
- Seen with Apple Color Emoji: the screens of Vue, Cocoa (GPU and Core
  Graphics) and Qt, the PDF of MuPDF and of PDFHummus (copied in Preview);
  with Noto Color Emoji: Vue and the PDF of MuPDF.

## The text of a PDF

The layers are glyphs of no character, shared by many emoji (the yellow
disc of the faces), so that no character map of the font can say what
they stand for. The readers take the text in one of two ways, and each
finds it once (`draw_color_glyph` in `unicode_font.cpp`):

- those which take the ActualText of the spans (MuPDF, and the readers
  built on it) find the characters of the emoji (all of a sequence) on its
  first layer, and nothing on the others; on the first layer, because they
  leave out the glyphs with no ink;
- those which take the character map of the font alone (Preview) find
  them in invisible glyphs of the characters (text mode 3, the renderer
  hook `draw_invisible`), which the character map gives; they are narrowed
  (`Tz`) to follow each other over the width of the emoji, so that a
  sequence is one word and not a word per character.

MuPDF writes the characters beyond U+FFFF in the character map of a CID
font as `<1f600>`, one unit, which no reader takes; the writer writes the
map again, in UTF-16 (`fix_to_unicode` in `mupdf_pdf_renderer.cpp`). The
mathematical letters of the OpenType fonts (U+1D400 and beyond) copy
right since then too. PDFHummus writes them in UTF-16 itself, but gave a
glyph of no character the character U+0000, which Preview copies as
spaces: such a glyph has no entry now (`CIDFontWriter.cpp`, in the
vendored PDFWriter), and the spans are written also in the PDF 1.4 of
TeXmacs.

## Checks

The suite `emoji` (`TeXmacs/progs/check/emoji-test.scm`) checks the widths:
an emoji is one em (Twemoji draws every emoji in one em), a sequence is one
emoji, what is left of a sequence takes no room; a character which no font
has is written out as its code, several em wide.

With the preference "emoji font" at "system" (or at a font), the group
"system font" checks the sequences against one emoji of that font, and the
groups of the widths of Twemoji are left out; checked with Apple Color
Emoji and Noto Color Emoji (`TEXMACS_FONT_PATH` to its folder, the
preference at "NotoColorEmoji").

The groups "names" check the conversions to and from the names, that a
name is as wide as its code points (with every emoji font, mixed with code
points, without the emoji in the emoji font), that it is one character,
that emoji inserted code point after code point are stored by the name of
their sequence, the search across the two forms, and the import (with the
other names of the package) and export of LaTeX and HTML. The scripted
test of Vue `tests/Plugins/Vue/footer-emoji.scm` checks the footer: the
whole sequence as a text of its own (in the emoji font), then its name,
and that an emoji typed comes by its name, also a family typed code point
after code point; it runs once for each footer:
`TEXMACS_VUE_FOOTER=plain sh tests/Plugins/Vue/run.sh footer-emoji 90`, and
`TEXMACS_VUE_FOOTER=interactive` (the variable stands for the preference
"interactive footer", which a test must not set).

`src/Plugins/MuPDF/tests/pdf-compare.sh` exports `emoji.tm` with the MuPDF
writer and checks that every emoji, and every sequence whole, comes out of
it as its characters, with MuPDF and, on a Mac, with PDFKit
(`pdfkit-text.swift`, which takes the text as Preview does).
