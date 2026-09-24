# Writing the PDF with MuPDF instead of PDFHummus

TeXmacs produces its PDF with the vendored **PDFHummus**
(`src/Plugins/Pdf/`, some 200 files). Since the editor already links MuPDF
for the screen, the question is whether MuPDF can write the document too,
and whether the result would be *almost the same* — in particular whether
the fonts would still be subsetted.

The answer, measured rather than guessed, is: **the page contents yes, the
subsetting of the Type 1 fonts no**. A working prototype is in
[`src/Plugins/MuPDF/mupdf_pdf_renderer.cpp`](../src/Plugins/MuPDF/mupdf_pdf_renderer.cpp);
what follows is what it took and what is still missing.

## What PDFHummus does for us

`pdf_hummus_renderer_rep` implements the `renderer_rep` interface and, on
top of the drawing, carries a good deal of PDF machinery:

| Area | What it does |
|---|---|
| Fonts, native | `tt_font_find` gives a font *file* for the TeXmacs `res_name`; Hummus loads it, embeds it and **subsets** it, and glyphs are written by index with a unicode mapping, so the text stays searchable |
| Fonts, bitmap | anything it cannot load (PK fonts, fonts it rejects) becomes a **Type 3 font**, one per 256-glyph chunk, whose glyph procedures are bitmaps |
| Ligatures | `/Span << /ActualText (ffi) >> BDC` around the ligatures, and the EC ligature slots remapped to `U+FB00…`, so copy and paste give letters |
| Images | an image pool keyed on the tree, patterns as tiling patterns, alpha as `ExtGState` |
| Links | `/Annot /Link` dictionaries built by hand, internal ones through named destinations |
| Outline | `toc_entry` collects the entries and a tree is written at the end |
| Metadata | title, author, subject |
| Other | a default `ExtGState`, encryption hooks, PDF attachments (`pdf_hummus_make_attachment`) |

## What MuPDF offers

`pdf_create_document` → per page `pdf_page_write` (which hands out an
`fz_device`) → `pdf_add_page` / `pdf_insert_page` → `pdf_save_document`.
Drawing is the ordinary fitz device interface, so a renderer written
against it looks much like the screen renderer. Everything that is not
page content (links, outline, `/Info`) is built with the `pdf_obj` API,
which is complete and pleasant.

Text is `fz_show_glyph (ctx, text, font, trm, gid, ucs, …)`: glyph *index*
and unicode, exactly the pair TeXmacs has. MuPDF writes the `ToUnicode`
CMap from the second, so the output is searchable without extra work.

Subsetting is `pdf_subset_fonts (ctx, doc, 0, NULL)`, a pass over the
finished document — it is what `mutool clean -S` calls, and the header
marks it *EXPERIMENTAL AND SUBJECT TO CHANGE*.

## The font problem

This is the one thing which does not work, and it decides the question.

Measured with a standalone program against MuPDF 1.26.9, one page of text
per font, full document versus `pdf_subset_fonts`:

| font | embedded as | Ghostscript | full | subsetted |
|---|---|---|---|---|
| Arial.ttf | `CIDFontType2` + `FontFile2` | accepts | 442 KB | **28 KB** |
| Geneva.ttf | `CIDFontType2` + `FontFile2` | accepts | 351 KB | **29 KB** |
| FiraSans-Regular.otf | `CIDFontType0` + `FontFile3` | accepts | 215 KB | **47 KB** |
| **ecrm10.pfb** (Type 1) | `CIDFontType0` + **`FontFile`** | **substitutes the font** | 91 KB | **91 KB** |

Two separate defects, both on the Type 1 fonts, which is what every TeX
font in `TeXmacs/fonts/type1` is — 275 files, 13 MB, the bulk of what a
mathematical document uses:

1. **The embedding is invalid.** MuPDF puts the raw Type 1 program in
   `/FontFile` under a `CIDFontType0` descendant; a CID font of that
   subtype must carry `/FontFile3` with a bare CFF. MuPDF reads its own
   output happily, but Ghostscript reports *"error reading a stream"*,
   falls back to NimbusSans and says the file "does not conform to Adobe's
   published PDF specification". The text then comes out in the wrong
   typeface in every consumer which is not MuPDF.
2. **The subsetter does not touch it.** `pdf_subset_fonts` handles `glyf`
   and CFF; on a Type 1 it fails (`format error: Reserved charstring byte`)
   and silently keeps the whole font.

**The first defect is fixed in the prototype**, the second is not.

MuPDF's pdf device is not the place to fix it: `pdf_dev_font` sends every
embedded font to `pdf_add_cid_font`, with no hook and no option. The
prototype therefore writes the content stream of each page itself and
chooses the font: `pdf_add_simple_font` for a Type 1, which gives a plain
`/Type1` with `/FontFile`, and `pdf_add_cid_font` for everything else,
where MuPDF is right and its subsetter works. A TeX font has at most 256
glyphs, so it fits a simple font exactly; the prototype assigns the
TeXmacs character code to each glyph and writes an `/Encoding`
`/Differences` array of glyph names (from FreeType) and a `/Widths` array
when the document is closed and the codes in use are known. This is what
PDFHummus does for its "ANSI" fonts.

Ghostscript now reads the output with no complaint and no substitution,
and its rendering of the page agrees with MuPDF's own to 97.9 % of pixels
within 32 levels — that is, the TeX font really is being used.

The second defect stands: a simple Type 1 is not subsetted either (89,761
bytes before `pdf_subset_fonts`, 89,143 after).

For comparison, PDFHummus converts Type 1 to CFF and subsets it: in a real
export the embedded font streams are **303 and 1182 bytes**
(`/Subtype /Type1C`, with the usual `QDBDRE+` subset tag). That is the gap
to close, and it is `Type1ToCFFEmbeddedFontWriter` + `CFFEmbeddedFontWriter`
— a Type 1 charstring interpreter, a CFF assembler and a subsetter — which
MuPDF has no equivalent of.

## The prototype

`mupdf_pdf_renderer.cpp` (about 800 lines against Hummus's 2550) writes
the content stream of each page itself, as Hummus does, rather than
through the `fz` device — that is what makes the choice of font ours. It
scales the stream by `72/dpi` once per page, so everything is written in
the pixels of the renderer with y upwards, exactly the coordinates `to_x`
and `to_y` give. It does:

* pages, clipping (`q … re W n` … `Q`), the graphics state, transparency
  through `ExtGState` objects;
* paths: `line`, `lines`, `fill`, `clear`, `polygon`, `arc`, `fill_arc`;
* text: `/Type1` simple fonts for the Type 1 programs and `/Type0`
  Identity-H for the rest, one `BT`…`ET` run per stretch of text, the
  positions as relative `Td`;
* glyphs of fonts which cannot be embedded, drawn as images (a stopgap:
  see below);
* pictures as `/XObject` images;
* links, a flat outline, `/Info` metadata;
* `pdf_subset_fonts` before saving.

It is reached with `TEXMACS_PDF_MUPDF=1`, which `printer ()` looks at
before choosing the Hummus renderer, and which also makes `use_pdf ()`
true so that the document is not routed through PostScript and Ghostscript
(this build has `PDF_RENDERER` undefined, so that is what normally
happens).

Exporting the first page of the TeXmacs manual:

    TEXMACS_PDF_MUPDF=1 texmacs.bin -c TeXmacs/doc/main/man-manual.en.tm out.pdf -q

gives a page which matches the reference rendering to **99.4 % of pixels
within 32 levels**, with the title, the rule, the bullets, the blue links,
the image and the small-caps TeX logo all in place, and whose text
extracts correctly (`THE GNU TEXMACS MANUAL`, `Getting started`).

Ghostscript reads it without a complaint. What is left is the size: **253
KB against the 54–60 KB** of the reference (two runs of the same export
through PostScript and Ghostscript). The three OpenType fonts in the page
are subsetted and carry the usual tags (`BWWMJR+Fira Sans Bold` and so
on); the one Type 1 is embedded whole. That single number is what is left
of the question.

Note that the reference is *not* PDFHummus: this build has `PDF_RENDERER`
undefined, so the Hummus renderer is not compiled at all and the normal
export is TeXmacs → PostScript → Ghostscript. The Hummus figures quoted
above (303 and 1182 byte font streams) come from a file which went through
that same route, where Ghostscript did the subsetting; a build configured
with the PDF renderer is needed to compare the two writers directly.

### Three bugs worth remembering

The first two belong to the earlier version, which drew through the `fz`
device; they are what makes writing the stream directly the simpler
choice, since it shares the coordinates and the sizes with Hummus.

* **The glyph size.** Hummus scales its content stream by `72/dpi` and
  passes `size * dpi_name / 72` to `Tf`; an fz device takes points, so the
  same length is `size * dpi_name / dpi`. Using Hummus's formula on the
  device made every glyph 8.3× too large.
* **The glyph orientation.** Glyph outlines are y upwards, an fz device is
  y downwards: the text matrix needs a negative y scale there. Without it
  the page is laid out correctly but every glyph is mirrored — and the
  text extractor then drops the doubled letters.
* **White is −1.** A `color` is an ARGB word, so white is `0xffffffff`,
  which as an `int` is −1. Using −1 as the "no colour selected yet"
  sentinel meant the first fill with white emitted nothing, and the page
  was cleared in PDF's default colour, black. The prototype carries
  `has_fill` / `has_stroke` flags instead.

## What is still missing

* **Type 3 fonts** for the bitmap and unloadable fonts. The prototype
  draws those glyphs as images, which is correct on the page but large and
  not searchable. Hummus builds a Type 3 font per 256-glyph chunk; MuPDF
  has no writer for them, so this is ours to write (`pdf_obj` is enough:
  a Type 3 font is a dictionary of glyph content streams).
* **Patterns** (`clear_pattern`, pattern brushes) — the prototype ignores
  them.
* **The outline is flat**: the levels are recorded but not nested.
* **Named destinations**: internal links are written with a `/Dest` name
  but the `/Dests` tree is not built yet, so `#anchor` links do not resolve.
* **Encryption and attachments** (`pdf_hummus_make_attachment`, which the
  "embed the .tm in the PDF" feature uses).
* **`draw_scalable`** falls back to rasterizing.

## What it would take to finish

1. A Type 1 → CFF converter and a CFF subsetter, or a Type 1 subsetter
   (charstring level). Without one of those the output grows by several
   hundred kilobytes per document. This is the bulk of the work, and it is
   exactly the part of PDFHummus which is not replaced by MuPDF.
2. A Type 3 font writer for the bitmap fonts.
3. Patterns, the outline tree, the destination tree, attachments.

A reasonable middle road, if the aim is to drop PDFHummus: teach MuPDF
itself to embed and subset Type 1 (upstream has the CFF subsetter already,
and the conversion is the missing half), and keep the rest of this
prototype. That way the subsetting stays in the library rather than
becoming another vendored writer inside TeXmacs.
