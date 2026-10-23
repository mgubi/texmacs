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

`src/Plugins/MuPDF/tests/pdf-compare.sh` exports `emoji.tm` with the MuPDF
writer and checks that every emoji, and every sequence whole, comes out of
it as its characters, with MuPDF and, on a Mac, with PDFKit
(`pdfkit-text.swift`, which takes the text as Preview does).
