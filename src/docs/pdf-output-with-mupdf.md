# Writing the PDF with MuPDF instead of PDFHummus

TeXmacs produces its PDF with the vendored **PDFHummus**
(`src/Plugins/Pdf/`, some 200 files). Since the editor already links MuPDF
for the screen, the question is whether MuPDF can write the document too,
and whether the result would be *almost the same* — in particular whether
the fonts would still be subsetted.

The answer, measured rather than guessed, is **yes for the page contents
and for the fonts, once the Type 1 subsetter of pdfTeX is brought in** --
MuPDF has none, and every TeX font is a Type 1. A working prototype is
in
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

MuPDF still does not subset it: a simple Type 1 comes out of
`pdf_subset_fonts` as it went in (89,761 bytes before, 89,143 after). That
is what `mupdf_writet1.c` is for.

## The subsetter

`mupdf_writet1.c` is **pdfTeX's `writet1.c`** (`texk/web2c/pdftexdir/`),
vendored as it is there, with its copyright and under the GPL it already
carried. It is the subsetter which has been cutting these very fonts down
for twenty years: it decrypts the eexec section, reads the `Subrs` and the
`CharStrings`, interprets the charstrings to find which subroutines and
which accented components each glyph depends on (`callsubr`,
`callothersubr`, `seac`), throws the rest away and encrypts what is left
again.

Around it, `mupdf_type1.c` supplies what pdfTeX supplied: memory, the file
to read, the buffer to write, the ordered sets `writet1.c` calls AVL trees
(a sorted array does for a few hundred glyphs), and a `pdftex_fail` which
unwinds with `longjmp` instead of ending the process. The one entry point
is

    unsigned char* mupdf_t1_subset (path, names, n, &size,
                                    &len1, &len2, &len3, &psname, &err);

`mupdf_writet1.h` declares what the vendored file expects, and copies the
buffer macros of `ptexmac.h` verbatim so that it keeps behaving as it does
upstream. Keeping the file unchanged is deliberate: it stays comparable
with its original, and a fix upstream can be taken over by hand.

On `ecrm10.pfb`, 91,799 bytes for the whole font and **8,571** for a subset
of six glyphs.

The renderer calls it from `write_fonts`, once the codes in use are known,
and replaces the `/FontFile` stream `pdf_add_simple_font` had written,
together with the three lengths and the name -- which now carries the
six-letter tag that says a font is a subset.

### Two more bugs worth remembering

* **The glyph name comes from the font, not from the index.** The index
  TeXmacs carries in `gl->index` is its own: in the EC fonts FreeType
  numbers the glyphs from the `CharStrings`, one less than the code, and
  for some fonts the index falls outside the font altogether. The name of
  the glyph a code selects is therefore asked of the font's own encoding
  (`FT_ENCODING_ADOBE_CUSTOM`, `FT_Get_Char_Index`, `FT_Get_Glyph_Name`).
  With the index, half the `/Differences` array was missing and the page
  came out right only because the reader fell back to StandardEncoding.
* **One file, one entry.** Several TeXmacs fonts share a program -- the
  sizes of a TeX font, and the chunks of one font -- and
  `pdf_add_simple_font` hands the same PDF object back for it. They must
  share one entry in the renderer as well, keyed on the file: with one
  entry each, the encoding written for one of them undid the encoding of
  the next, and the letters only that one used disappeared from the text.

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

Ghostscript reads it without a complaint, and its rendering agrees with
MuPDF's own to 97.9 % of pixels within 32 levels -- that is, the TeX font
really is being used.

On six pages of `tag-help.en.tm`, with four Type 1 fonts and five
OpenType ones:

| | whole fonts | with the subsetter | reference |
|---|---|---|---|
| the file | 784 KB | **519 KB** | 194 KB |
| embedded font programs | 432 KB | **167 KB** | — |
| └ the four Type 1 | 315 KB | **49 KB** | — |
| └ the five OpenType | 118 KB | 118 KB | — |

Ghostscript reads all six pages with no complaint and no substitution, the
text extracts correctly, and the two renderings agree to 92–99 % of pixels
(the rest is Ghostscript's heavier stems, not different content).

### The reference is not the better file

The remaining difference in size is not a difference in subsetting, and it
was read the wrong way round at first. The reference has **four Type 1
fonts and thirty-seven Type 3 fonts**: the route through PostScript
rasterizes most of its text, and what it embeds are bitmaps. Its text does
not come out whole either -- the arrow of `t <- (focus-tree)` extracts as
U+FFFD, and the words break. Ours embeds nine real fonts, all outlines,
and extracts cleanly.

MuPDF's OpenType subsetting, which the 118 KB was blamed on, is in fact
doing its work:

| font | whole | in the document |
|---|---|---|
| TeX Gyre Pagella Math | 601 KB | 38.5 KB |
| Fira Sans Bold | 378 KB | 29.0 KB |
| TeX Gyre Pagella Regular | 218 KB | 21.6 KB |
| TeX Gyre Pagella Bold | 216 KB | 21.3 KB |

So 519 KB of scalable, searchable text against 194 KB of bitmaps is the
honest comparison, and the two are not the same document.

There is nothing to take from TeX for this half: pdfTeX does not subset
OpenType at all. `writeotf` in `writettf.c` stops with *"OTF fonts must be
included entirely"* and copies the whole `CFF ` table. Its `writettf.c`
does subset TrueType, in 1463 lines, but MuPDF already does that well
(Lucida Grande Bold goes in at 7 KB), so there would be nothing to gain.

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

## Checking it

`src/Plugins/MuPDF/tests/pdf-compare.sh` exports a few documents both ways
and checks, of the MuPDF one, that Ghostscript reads it without an error
and without substituting a font, that the text extracts with no U+FFFD in
it, and that the pages Ghostscript and MuPDF draw agree. The first check is
the one which earns its keep: an invalid font program still looks like a
page, only in the wrong typeface. The checks were themselves checked by
putting the old CID embedding of the Type 1 fonts back, which makes two of
them fail; note that the rendering check does *not* catch that, since both
readers then substitute.

## What is still missing

* ~~Type 3 fonts for the bitmap and unloadable fonts.~~ Done: one font per
  256 characters, the glyph an inline image mask in a little content
  stream, with a ToUnicode CMap so the text is still searchable. It also
  fixed glyphs which the image fallback drew as empty boxes.
* **Patterns** are drawn, but as tiled images: `renderer_rep::clear_pattern`
  does the tiling with `draw_picture`. Real PDF tiling patterns would be
  smaller.
* ~~The outline is flat.~~ Done: it is the tree the levels describe.
* ~~Named destinations.~~ Done: the anchors are the name tree of the
  catalogue and a link refers to one by name.
* **Encryption and attachments** (`pdf_hummus_make_attachment`, which the
  "embed the .tm in the PDF" feature uses).
* **`draw_scalable`** falls back to rasterizing, where Hummus embeds the
  PDF or EPS figure itself. At least it now rasterizes at a print
  resolution (`shadow` raises the zoom).
* **Transformations** are honoured now (`set_transformation`), which they
  were not: every frame of the graphics used to be dropped.

## What it would take to finish

1. ~~A Type 1 subsetter.~~ Done: pdfTeX's `writet1.c`, see above.
1. ~~The OpenType half.~~ There is no gap: the reference is bitmaps, and
   MuPDF subsets the OpenType fonts by a factor of ten to fifteen.
2. A Type 3 font writer for the bitmap fonts.
3. Patterns, the outline tree, the destination tree, attachments.

A reasonable middle road, if the aim is to drop PDFHummus: teach MuPDF
itself to embed and subset Type 1 (upstream has the CFF subsetter already,
and the conversion is the missing half), and keep the rest of this
prototype. That way the subsetting stays in the library rather than
becoming another vendored writer inside TeXmacs.
