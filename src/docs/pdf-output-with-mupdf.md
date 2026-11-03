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

It is chosen by the preference **`native pdf renderer`** set to `mupdf`,
or by `TEXMACS_PDF_MUPDF=1` in the environment. Either makes `use_pdf ()`
true as well, so that the document is not routed through PostScript and
Ghostscript, which is what this build does otherwise (`PDF_RENDERER` is
undefined here, so the Hummus renderer is not compiled at all).

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
| the file | 784 KB | **329 KB** | 194 KB |
| embedded font programs | 432 KB | **167 KB** | — |
| └ the four Type 1 | 315 KB | **49 KB** | — |
| └ the five OpenType | 118 KB | 118 KB | — |

Ghostscript reads all six pages with no complaint and no substitution, the
text extracts correctly, and the two renderings agree to 92–99 % of pixels
(the rest is Ghostscript's heavier stems, not different content). The 519
KB of the first measurement became 329 once the figures stopped being
rasterized.

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

Built against MuPDF 1.28.5 (Homebrew), since 26 September 2026; it was
written against 1.26.9. The move needed no change of the code, and the
checks came out the same on both: the ten documents of the harness, the
round trip of a PDF with its document embedded, and on the screen the
document area of the `pattern` and `figures` tests pixel for pixel (the
vector icons of the toolbars differ in their anti-aliasing, and nothing
else).

`src/Plugins/MuPDF/tests/pdf-compare.sh` exports a set of documents both ways
and checks, of the MuPDF one, that Ghostscript reads it without an error
and without substituting a font, that the text extracts with no U+FFFD in
it, and that the pages Ghostscript and MuPDF draw agree. The first check is
the one which earns its keep: an invalid font program still looks like a
page, only in the wrong typeface. The checks were themselves checked by
putting the old CID embedding of the Type 1 fonts back, which makes two of
them fail; note that the rendering check does *not* catch that, since both
readers then substitute.

## What it does

* pages, clipping, the graphics state, transparency through `ExtGState`;
* paths, and the transformations of the graphics (`set_transformation`);
* text in the fonts MuPDF can embed -- `/Type1` for the Type 1 programs,
  subsetted by pdfTeX's `writet1.c`, and `/Type0` Identity-H for the rest,
  subsetted by MuPDF;
* **Type 3 fonts** for what cannot be embedded at all: one font per 256
  characters, the glyph an inline image mask in a little content stream,
  with a ToUnicode CMap so the text is still searchable;
* **figures**: a PDF as a `/Form` XObject with its drawing kept as
  drawing, a raster image as an `/Image`, anything else through the
  converters first; each file embedded once however often it occurs.
  The form's `/Matrix` is the page transform of `pdf_page_obj_transform`
  with its turn upside down taken back out (`fz_scale (1, -1)`): that
  transform goes from PDF space to fitz's, y down, and taken as it is it
  drew every figure upside down -- which the rendering check cannot see,
  both readers drawing the same file. `pdf-figures.tm` in the tests has
  text in its figures which says which way is up, and the harness reads
  where it lands. The form is also a transparency group (`/Group /S
  /Transparency`), so that an alpha applies to the figure as a whole and
  not to each path and fill on its own, which would show through one
  another; the harness checks that too, with a half transparent figure
  of two overlapping squares (`overlap.pdf`);
* pictures, **tiling patterns** (below), links, named destinations, the outline as a tree, `/Info` metadata, attachments.

### Ligatures

A ligature glyph is wrapped in `/Span << /ActualText (fi) >> BDC ... EMC`,
as PDFHummus does, so that it extracts as the letters it stands for. The
letters come from the name of the glyph (`fi`, `uniFB01`, `f_f_i`), asked
of the font the way `write_fonts` asks it. Without the span, a reader which
takes the text from the glyph names -- Ghostscript is one -- gives `ﬁrst`
for *first*, and a search finds nothing.

Neither reader at hand can judge this alone, which is how it went unseen
for a while: MuPDF decomposes ligatures whatever the file says, so its
extraction was always right, and Ghostscript 10 ignores ActualText, so its
extraction is always "wrong". MuPDF does honour the spans -- rewriting one
`(fi)` as `(XY)` in a written file makes it extract *XYrst* -- and the
harness uses Ghostscript's blindness to count the ligature glyphs drawn,
each of which must have its span.

The name is asked through MuPDF (`glyph_name`): the charmap and the index
under `fz_ft_lock`, the name with `fz_get_glyph_name`, which takes the lock
itself. Asking FreeType directly worked on the Type 1 fonts and crashed on
the first OpenType one: its table of glyph names is loaded the first time a
name is asked, that load allocates, and FreeType's allocator is pointed at
a MuPDF context only while the lock is held. Every FreeType call in the
renderer goes through the lock for that reason.

### Tiling patterns

A patterned background is a `/Pattern` -- PatternType 1, colored, constant
spacing -- whose one cell draws the tile image, and one fill with it; before,
it was a `Do` per tile, five hundred on the test page. The geometry is that
of `renderer_rep::clear_pattern`, taken over as it is, and the pattern's
`/Matrix` is in the page's default space, so it carries the 72/dpi scale of
the content stream as well as the position of the first tile. Measured on
synthetic tiles, MuPDF, Ghostscript and the PostScript route put every
tile on the same pixel.

Not every tile goes into a pattern:

* a tile drawn with an alpha below 1 is drawn tile by tile, as before;
* so is a tile image **with a soft mask**. Ghostscript draws such an image
  inside a tiling pattern wrongly -- the same page drawn tile by tile and
  as a pattern agrees at 97.6% in MuPDF and at 46% in Ghostscript -- and a
  file one of the two readers gets wrong is a file to avoid.
  `opaque_tile` decides, and `pattern-photo.tm` in the tests is the case.

## Links

A reference (`<reference|…>`) and a link to a web page (`<hlink|…|url>`)
reach the renderer by two routes. A locus whose body is not text of the
document -- the number a reference prints -- becomes a `locus_box` which
calls `href` itself. A locus whose body is text of the document is left as
that text, and each of its boxes finds the link when it is drawn
(`box_rep::display_links`), in the link database the typesetter filled.

`print_doc` typeset the document with `typeset_as_document`, which drops
its typesetter before returning, and the typesetter owns the registrations:
by the time the pages were drawn, the links it had registered were gone.
An export from a session still found them, registered a second time by the
typesetter of the window; a batch export (`texmacs -c`) found none, and
every `hlink` came out as plain text. `print_doc` now keeps its typesetter
until the pages are drawn -- which applies to every printer, the PostScript
route too -- and `display_links` draws a link once however many
typesetters registered it (without that, an export from a session had each
URL four times).

That uncovered a fault of the outline: `/Outlines` was a direct dictionary,
and the `/Parent` of every entry of the first level was that same
dictionary. It is meant to be a reference -- and a direct object shared
that way is renumbered once for each place it occurs when the file is
compacted, which in a file with more objects sent `/First` to a link
annotation, and MuPDF had to repair the outline. It is now an object of its
own. The code before had it too.

What a link looks like and how it is named follows pdf_hummus_renderer:
`/Border [16 16 w [3 10]] /Color [0.75 0.5 1.0]`, the width 1 only when
the preference `locus-on-paper` is `preserve` (the loci keep their look
on paper), and `/Creator`, `/Producer` and `/CreationDate` in the metadata
as Hummus writes them. Three things differ, on purpose:

* the strings reach the renderer in UTF-8 -- the metadata, the entries of
  the outline, anchors and targets alike (checked, byte by byte) -- so they
  are not converted from Cork, as Hummus converts them;
* a place in the document keeps its own name (`#sec-first`, in a name
  tree), where Hummus numbers them (`/label7`, in a `/Dests` dictionary),
  so that `file.pdf#nameddest=...` finds it. The name is written in the
  same bytes in the link and in the tree (`pdf_text_bytes`: ASCII, or
  UTF-16BE after a byte order mark), and the tree is sorted by them: a
  reader looks a name up by its bytes, and the link used to be UTF-16 where
  the tree was UTF-8, which only MuPDF, decoding both, forgave;
* a URI is an ASCII string, so what is not ASCII is percent encoded from
  its UTF-8 (`https://fr.wikipedia.org/wiki/%C3%89t%C3%A9`); Hummus writes
  a text string, UTF-16 as soon as there is an accent.

A PDF figure keeps its layers: the objects which name them came over with
its resources, but whether a layer is seen is said in the catalogue of the
figure, and without it a layer the figure hides by default was drawn, in
every reader. `merge_layers` adds the layers of a figure to the
`/OCProperties` of the document, with the same graft map as its resources,
the hidden ones to `/OFF` (also under `/BaseState /OFF`) and its `/Order`
to the document's. On the screen nothing was needed: a figure is drawn
from its own document, which keeps its catalogue. (`/AS`, `/RBGroups` and
`/Locked` are not taken over.)

A link is one annotation on each line it runs over: its words reach
`href` a box at a time, and a box which follows the last one on its line,
to the same place, lengthens it (Hummus, and the PostScript route, have
one annotation a word).

`dest-bytes.py` in the tests checks the second and the third on the file
itself; `structure.tm` has an accented heading, label, author and address.

The outline was compared with the one a build with PDFHummus makes, on
an article three levels deep, a book (a part, chapters, an unnumbered one,
an appendix) and an article without a table of contents, with a level
skipped and mathematics in a heading: the same entries, nested and folded
the same way, on the same pages, at the same places to a tenth of a point.
The titles differ where they have accents -- Hummus converts them from
Cork, and they arrive in UTF-8, so it writes "GrÃ¶Ã§e" for "Größe". The
zoom of a destination is `null`, the reader's own, as Hummus writes it;
it was 0, which means the same in the specification but which MuPDF takes
for 100%.

The other route of the build -- PostScript through Ghostscript, when the
MuPDF renderer is not chosen -- put its outline off the pages: the entries
are written after the last page as pdfmarks, where the coordinates are
PDF's, points from the bottom left corner, but they were given as dvips
has them inside a page, pixels at the printing resolution from a margin
of an inch, y downwards -- y 2542 on a page 842 high. `printer_rep::
toc_entry` now gives points, and on a landscape page, which dvips draws a
quarter turned on portrait paper for the PDF to turn back (`/Rotate 90`),
the coordinates of the paper. Both routes now send an entry to the same
place, to a fifth of a point, portrait and landscape.

A forward reference in a batch export is "?" unless the document carries
the values of its labels, as a document saved by TeXmacs does (the
`references` part at its end): the export typesets once, and a label comes
after the reference which needs it. The same as LaTeX needing a second run,
not a fault of the export.

## The document in the PDF

*File -> Export -> Pdf with embedded document* puts the TeXmacs document
into the PDF, with the files it links to (images, included documents,
styles of its own), and *File -> Import -> Pdf with embedded document*
takes them back out. With PDFHummus that is
`pdf_hummus_{make,extract}_attachment.cpp`; without it, as here, only the
embedding had a MuPDF version (`mupdf_pdf_make_attachments`), and the rest
were stubs: the linked files were not found, so only the document went in,
and nothing came out -- the import always said "Can not extract
attachments from PDF". `Plugins/MuPDF/mupdf_attachments.cpp` has the rest:
the search for the linked files and the rewriting of their paths, taken
over from the Hummus file as they are, and the extraction with MuPDF.

It follows the format of Hummus, so that either reads what the other
writes -- all four ways were tried, with a build with PDFHummus: the
document first in `/Names/EmbeddedFiles` (the reader takes the first as
the document), and in the name tree file specifications written out in
full, since the reader of Hummus takes them as direct objects and found
nothing in a PDF made here, where MuPDF had put references (`/AF` keeps
those). Two things differ on purpose:

* the files come out into a directory of their own, not next to the PDF:
  the document of `paper.pdf` is `paper.tm`, and next to `paper.pdf` there
  is often a `paper.tm` already -- the source, perhaps newer -- which the
  Hummus extraction overwrites. The import therefore makes the linked paths
  relative to the extracted document, not to the PDF (`file-menu.scm`),
  which is where both put them;
* the name a file has in the PDF is reduced to a file name before it is
  written (a PDF naming one `../x` would write elsewhere), and decoded when
  it is UTF-16.

The export also changed the open document: `pdf-replace-linked-path`
rewrites the tree it is given in place, and was given the tree of the
buffer, so exporting turned the paths of the images of the open document
into absolute ones, behind the editor's back (unmodified, until the next
save kept them). It now works on a copy, and it is that copy which goes
into the PDF, as was meant (`attach-doc-to-exported-pdf`, `tm-print.scm`).
That was so with Hummus as well; here it only showed once the rewriting
did something. The copy names the linked files by their file names alone
(`pdf-embedded-bare-names`): it used to name them by the author's absolute
paths, which worked, since the import reduces them to file names, but
told everyone who got the PDF where the author's files were kept. A build
with PDFHummus reads the bare names as well (tried). The harness checks it
(`embed-roundtrip.sh`).

## A PDF with a password

*File -> Export -> Pdf with password* (shown only when the MuPDF renderer
writes the PDF, `pdf-encryption?`: anything else would ignore the passwords
and write a PDF which is not protected) asks for a password to open the
PDF, an owner password, which lifts the restrictions (empty: the same),
and what is allowed without it (all, none, or a list: print, print-hq,
copy, modify, annotate, form, assemble, accessibility). The PDF is
encrypted with AES, a key of 256 bits (`pdf_encryption`, which fills
`pdf_write_options`); a password longer than the 127 bytes MuPDF keeps
fails the export rather than protect the PDF with part of it.

The passwords reach the renderer through the environment
(`TEXMACS_PDF_USER_PASSWORD`, `TEXMACS_PDF_OWNER_PASSWORD`,
`TEXMACS_PDF_PERMISSIONS`), put there by the command for the one export
and taken away after it, even when it fails: a preference or the document
would keep them on the disk in clear. For the same reason the answers to
an argument of type `password` are no longer learned: `learn-interactive`
kept every answer given to an interactive command and saved them in
`interactive.scm`, in clear -- which it did with the passphrases of the
wallet too. The attachment of the document (Pdf with embedded document)
opens the PDF with the password and keeps its encryption. With the
variables set, a batch export (`texmacs -c`) is encrypted as well, which
is how `encrypt-check.sh` in the tests tries it.

## MuPDF's errors and C++

MuPDF reports an error with `fz_throw`, a `longjmp`. Two things follow,
and the writer is built around both:

* an error which no `fz_try` catches ends the process ("aborting process
  from uncaught error"), so every MuPDF call is made inside one;
* a `longjmp` skips C++ destructors, so nothing which has one may be alive
  between an `fz_try` and a call which throws -- not in the body of the
  `fz_try`, and not in a function it calls.

Hence the shape of the code. What TeXmacs has to compute -- strings,
arrays, the tree of the outline -- is computed first, and an `fz_try`
then hands MuPDF the results: C strings are TeXmacs strings with a 0
added (`&s[0]`), names are made with `snprintf`. `write_fonts`,
`write_type3`, `subset_type1`, `write_outline`, `write_dests`,
`write_metadata`, `write_links` (with labels prepared in `end_page`) and
the attachments all have that split, and each catches its own errors: an
error loses its part, as a warning, not the document. The page stream is
written through `put` and `app`, which catch the error themselves, since
they are called from everywhere with C++ objects around them; a page which
failed halfway is not a page, so the file is then not written, and an
error says so.

This was checked two ways. A script lists, for every `fz_try` body and
every `mupdf_protected` lambda of the plugin, what could create a C++
object there and the functions it calls; each of those was read, down to
`mupdf_load_image` and `mupdf_render_svg`, which catch their own errors.
And faults were injected (a `fz_throw` behind an environment variable, in a
build which was then thrown away): in the page stream the export reports
the error and writes no file, in the outline and in an encoding it warns
and writes the rest, and in each case TeXmacs goes on. The outline,
destinations, links and metadata of `structure.tm` and `tag-help` came out
the same as with the code before the change, and `structure.tm` is now in
the harness with a check of them.

The screen renderer follows the same rule where MuPDF can fail for another
reason than memory: an image is decoded when drawn (`image`, like
`draw_form`), `fz_close_device` complains of a clip left open (`end`), and
q and clips have limits of nesting (`set_clipping`, `set_transformation`);
`save_picture` catches a file which cannot be written. What it leaves
unprotected are the path and text operators of every glyph and line, which
fail only for want of memory -- a `setjmp` for each glyph is not worth it.

## What is left

* **An image with an effect** on it has to be computed, so it is
  rasterized -- at a print resolution, but rasterized.
* **A preference in the menus.** The renderer is chosen by the preference
  `native pdf renderer` set to `mupdf`, or by `TEXMACS_PDF_MUPDF=1`, not by
  anything a user can click. The menu it belongs in,
  *TeXmacs -> Pdf/Postscript*, is itself shown only when
  `supports-native-pdf?` is true, which -- now that the answer depends on
  the preference -- would mean the entry which turns the renderer on is
  hidden until it is on. Reaching it wants either a glue predicate of its
  own or an entry in the shared menu which does nothing on the builds
  without MuPDF; for a prototype the preference is interface enough.

`supports-native-pdf?` does answer for the MuPDF renderer when it is the
one chosen, which it has to: `printer-file-suffix` and
`printer-file-format` are built on it, and they were asking for PostScript
while `use_pdf ()` was writing a PDF. With the renderer off both say
PostScript, as before; with it on both say PDF.

## Checking it

`src/Plugins/MuPDF/tests/pdf-compare.sh` and its `README`: it exports a
set of documents both ways and checks, of the MuPDF one, that Ghostscript
reads it without an error and without substituting a font, that the text
extracts with no U+FFFD in it, and that the pages Ghostscript and MuPDF
draw agree. The Ghostscript check is the one which earns its keep, because
an invalid font program still looks like a page, only in the wrong
typeface; and the rendering check cannot see that fault, since both
readers then substitute. The checks were themselves checked by putting the
old CID embedding of the Type 1 fonts back, which makes two of them fail.

Twenty-four documents have been through it: twenty of the manual -- among
them Chinese, German, French and Polish for the fonts and the accents, and
the mathematics, tables, graphics, links and presentation chapters -- and
the documents of the tests, each run both ways. The rendering check
compares the pages after a blur of a pixel and a half, since the two
readers anti-alias and resample differently: the documents agree at 97.5%
and more, the photograph drawn tile by tile at 95%, and the pattern
Ghostscript draws wrongly at 42%, so the threshold is 90%.

## If this is to replace PDFHummus

The one piece which is neither ours nor MuPDF's is `writet1.c`, vendored
from pdfTeX. The better home for it is upstream: MuPDF has the CFF
subsetter already and only wants the Type 1 conversion in front of it, and
a subsetter in the library serves everyone rather than sitting in a
second copy inside TeXmacs. Until then it is here, unchanged, so that a
fix there can be taken over by hand.
