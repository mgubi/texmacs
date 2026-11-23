# Tagged PDF (PDF/UA-1)

Universities and administrations ask for accessible PDF documents: PDF/UA-1
(ISO 14289-1), a tagged PDF whose structure a screen reader follows, with
alternate descriptions for the formulas and the pictures, the language of
the text, and nothing drawn that is neither content nor marked as an
artifact. The request:
[forum.texmacs.cn/t/2191](http://forum.texmacs.cn/t/accessibility-support-for-pdfs/2191),
issue #945. A user had resorted to exporting to Typst
([texmacs-typst-exporter](https://github.com/ingolfschaefer/texmacs-typst-exporter)),
which gives up the typesetting of TeXmacs.

Both PDF writers write a tagged PDF, by default: *Preferences → Convert →
Pdf → Tagged Pdf, for accessibility*, the preference `texmacs->pdf:tagged`
(on), turns it off, and `TEXMACS_PDF_TAGGED=0` or `1` decides over the
preference -- the writer on MuPDF
(`src/Plugins/MuPDF/mupdf_pdf_renderer.cpp`, Vue since #942) and
PDFHummus (`src/Plugins/Pdf/pdf_hummus_renderer.cpp`, Qt and Cocoa). What
they share (the elements, the blocks of marked content, the order of the
children, the annotations, the XMP metadata) is `pdf_tags`
(`src/Graphics/Renderer/pdf_tags.cpp`); each writes the operators and the
objects its own way.

## How it works

**Where the drawing comes from.** Each box knows the place of its node in
the document (its inverse path, `ip`). When a renderer has a tagger,
`box_rep::redraw` sets `ren->tag_ip` to the box's `ip` around its
`pre_display`, `display`, links and `post_display` (a locus writes its
link in `post_display`); its children set their own. The renderer never
sees the document itself.

**From a place to the elements** (`src/Edit/Editor/edit_tagger.cpp`).
`print_doc` makes a tagger for the body when the PDF is to be tagged. For
an `ip` it gives the chain of elements on the way from the body to that
place: the tags of the table `pdf-tag-roles` (`tm-print.scm`, which a
style or a user may extend), and the paragraphs, which are the children of
a document (a P, or a Div when they hold paragraphs themselves).

* The text a macro writes (the number of a section, "Theorem 1.", the
  bullet of an item) has the place of the macro, after the marks of a
  decoration (`DECORATION...`), and belongs to its element.
* A box of no place in the body (the headers, footers, page numbers, the
  rule of the footnotes) has no element: an artifact.
* Headings: `H:n` in the table is a rank (`H:A` an appendix: a chapter
  in a document which has chapters, a section elsewhere). A heading is
  one level deeper than the headings above it, those of a smaller rank
  met before it in the pages and not closed by one of its rank since: no
  level is skipped (PDF/UA), and the headings of a rank under the same one
  have the same level.
* Lists: a child of the document of a list which begins with an `item`
  is an LI, with an Lbl (the `item`) and an LBody; one which does not (a
  second paragraph, an equation, a nested list, a pause) is in the LBody
  of the item before it.
* Tables: `tabular` (or a `table` which no tabular holds) is the Table
  (the document of a table in the form of a block is not a paragraph),
  `row` a TR, `cell` a TD, with `ColSpan` and `RowSpan` read from the
  `cwith` of the formats around the table, clamped to the table (no row
  spans in a table broken across pages, `table-hyphen`, as the typesetter
  does; as many columns as the shortest row). A row
  has all its cells,
  those which draw nothing included, and not those which a span covers
  (`row_cells`), so that the rows have the same number of columns.
* A formula (`math`, `equation*`, `eqnarray*`, `align*`...) or a picture
  is a whole: nothing inside it is an element of its own. The Alt of a
  formula is its LaTeX, without its environment and labels
  (`pdf-tag-alt`). The Alt of a picture is the variable `image-alt` around
  it, `<with|image-alt|A plot of the solution|<image|...>>`, which *Focus →
  Alternative text...* sets on an image (`set-current-image-alt`); without
  it, the caption of its figure, or else the name of its file, and the
  export warns of the pictures without one.
* A Sect (a bibliography, a table of contents, an index, a glossary, a
  list of figures) has the title its macro writes as a heading, of the
  rank of an appendix; the title of a slide (`tit`) is a heading.
* Included documents: the content of an `include` is typeset as a
  decoration of the include, one box of no place inside. A tagged PDF is
  therefore typeset from the body with each include replaced by the body
  of the `include` macro of the environment (std-automatic's, a style's)
  applied to its file, whose `include*` is given the document of the file
  as a second child: `<include*|f|doc>` typesets `doc` in place, as a
  child with places of its own, with what an include sets around it (the
  file, whether its scripts are trusted, the guard against cycles), in
  the three typesetters of `include*` (`bridge_rewrite`, the concater,
  `make_lazy_rewrite`). So a redefined `include`, a style without
  `part-info`, a script and a cycle give what they give without tagging.
  Only the `include*` which `print_doc` made are typeset so, found by
  their identity (`is_given_inclusion`, registered after `attach_dip`,
  which makes the nodes of no place anew): an `include*` of two children
  in a document is an error, as before, and cannot choose the trust of
  its content.
  Nothing is expanded in the preamble mode, nor when the body assigns
  `include`, nor in inactive markup or raw data, nor for a file which is
  being expanded (the typesetter reports the cycle). The `include*` is no
  element: the document holds the paragraphs.

  As the bridges read the tree of the buffer again at each place
  (`bridge_rep::my_resync`), this expanded body is the body of the buffer
  while the PDF is typeset and drawn, swapped in without telling the
  observers and put back after, also on an error (`body_swap`). The tables
  which the typesetting writes (the references, the auxiliary data, the
  attributes of the buffer or of its project) are put back too, in place:
  the labels of the expanded body lack the file they are in, which must
  not reach the document. The labels keep their numbers
  (`exec_auto_id`): the expanded body has those of the body, a tag of an
  included document numbers its labels on the `include*`, which has those
  of its include (as on the screen, where they are numbered on the
  include, the content of the include having no places of its own), and
  the numbers of the tags of the body are put back after.
  The buffer is as it was (convert-more checks the body, the auxiliary
  data and the labels after the document is typeset again).
* A link of the document (`hlink`) is a Link element; a link of no
  `hlink` (the call of a footnote and its way back) gets a Link of its
  own in the element of its text. A video is an Annot element.

**Marked content** (`tag_mark`). Before each drawing operation the writer
asks the element of the box being drawn (`tag_resolve`, cached for the
last `ip`). A change of element closes the block of marked content and
opens another: `/P <</MCID n>> BDC` for an element, `/Artifact BMC` for
none. Lines and fills outside a Formula or a Figure (rules, borders,
backgrounds) are artifacts. A block never crosses a text object (the text
object is ended first) nor an `ActualText` span (no new block inside one).

**The structure tree** (`write_structure`, when the document is closed).
The children of each element are put in the order of their places in the
document, then of their drawing: the reading order is that of the source,
whatever the pages do with the floats and the footnotes. MCR for the
marked contents, OBJR for the links (their annotations get `/Contents`
and `/StructParent`), the parent tree with the elements of each page by
MCID and of each link. Each page gets `/StructParents` and `/Tabs /S`. The
catalog gets `/MarkInfo <</Marked true>>`, `/Lang` (the language of the
document, `pdf-tag-language`), `/ViewerPreferences <</DisplayDocTitle
true>>`, and XMP metadata with the title and `pdfuaid:part 1`.

**PDFHummus.** The pages get `/StructParents` and `/Tabs` through
`OnPageWrite` of its extender, the catalog its keys through
`on_catalog_write`; the annotations are written at the end, so their
OBJR name them by the ids allocated when they were met. It writes a
`/CIDSet` for the subsets of CID fonts which does not identify exactly
their glyphs: in a tagged PDF it writes none (`hummus_write_cidset`, in
the vendored `DescendentFontWriter.cpp`), which PDF/UA allows.

**Fonts.** PDF/UA wants a Unicode value for every glyph drawn. The TeX
fonts take theirs from their encodings (`tex_math_unicode`: the math
fonts, and the CB Greek fonts, `grmn.enc`), a TeX font of no table in a
tagged PDF from the names of its glyphs; a glyph of no character in the
table, or drawn by its number (the variants of the OpenType math fonts,
#943), has the character the font maps to it, else that of its name
(uXXXX of hexadecimal digits only; uniXXXXYYYY of the ligatures ff, fi,
fl, ffi, ffl their presentation forms; the names of the AMS, stmary,
wasy... fonts which TeXmacs ships, from a table made from MuPDF's glyph
list, as the PDFHummus build has none), else, for a variant (divides.v1),
the one the font maps to the glyph of the name before the dot; never one
of a private use area; only those of none (the pieces of the horizontal
braces of cmex, the bar of mapsto of cmsy) are U+FFFD (a code of a
TeX font, by PDFHummus, is turned into its glyph of FreeType first:
the encoding of the font numbers its glyphs; code 161 is the glyph of
code 0, `requires_hack_notdef`). The vendored
PDFHummus maps the glyph 0 of a simple font too (a symbol of stmary10 or
wasy10, which it took for .notdef), and never to 0. A TrueType CID font
gets `/CIDToGIDMap /Identity`.

**Size.** The structure is tens of thousands of small objects in a long
document: the MuPDF writer puts them in compressed object streams (PDF
1.5), and a tagged PDF is about a tenth larger than an untagged one
(`TEXMACS_PDF_OBJSTMS=0`: not, which convert-more uses to read the
objects). PDFHummus writes no object streams: its tagged PDFs are up to
twice as large.

**Fonts of the pictures.** A PDF picture (a plot) often names its fonts
(Times-Roman, Helvetica, Courier, Symbol, Arial) without embedding them,
which PDF/UA forbids. In a tagged PDF both writers embed a font which
TeXmacs ships instead (`src/Plugins/Freetype/pdf_font_subst.cpp`): the
TeX Gyre clone of a standard font, by its family and style (Termes,
Heros, Cursor, Pagella, Bonum, Schola, Adventor), STIX Two Math for
Symbol (the pieces of the large delimiters included, with their Unicode
characters rather than those of the private use area), with the glyph of
each code found by its name (the encoding of the font and its
differences), the widths of the glyphs, the descriptor and a ToUnicode.
A code of no glyph in the replacement is `.notdef` (reported), never the
glyph of the code in the encoding of the program. The fonts are found in
the resources of the picture, of its forms, of its tiling patterns and of
its Type 3 fonts. The widths are those of the replacement (PDF/UA wants
them to agree with the program): a picture spaced for a font of other
widths (Calibri) changes a little. The MuPDF writer does it in the
resources it grafts, each program subset to the codes which the picture
draws with the font (a processor of its content; MuPDF's subsetter fails
on some glyphs of the TeX Gyre fonts, so a program is embedded whole
when it does). PDFHummus makes a copy of the picture with the fonts
embedded (an incremental update, subset by its CFF writer) and includes
it; it leaves a font which is not an object of its own (a direct
dictionary, rare) as it is, with a warning. A font of pictograms
(ZapfDingbats, Wingdings) has no replacement and is reported.

**Tagged pictures.** A picture which is itself a tagged PDF has marked
content (its tags, its artifacts) and a structure of its own.

* Without an alternative text of the user's (*Alternative text...*), its
  structure is grafted in place of its Figure, which becomes a Div (a
  Figure has an alternative text, which hides its children from a screen
  reader): the elements of its first page, read by the writer (types
  through its role map; a Document is a Div, a heading a P, since its
  level would not follow those of the document; another type
  NonStruct), without those of no content there, are children of the Div
  (`pdf_tags_rep::graft`), their marked content referred to in the form
  of the picture (`/MCR` with `/Stm`), which gets `/StructParents`, a key
  of the parent tree after those of the pages and the annotations. The
  marked content of the picture stays, its artifacts too; marked content
  of no element is an artifact (`pdf_graft_marked_content`); the forms
  inside the picture lose theirs. Such a picture has a form for each use:
  an element is in one place of the tree (PDFHummus copies its file once
  for all the uses, which share its fonts). PDFHummus adds
  `/StructParents` to the form through `OnFormXObjectWrite`.
* A picture is grafted only when its Figure is the image itself (not a
  drawing which holds pictures among other things), when its structure
  has no marked content in its forms (`/Stm`), and when all that its page
  paints is in marked content (`pdf_content_fully_marked`); else it is a
  Figure, its tags taken out. The structure tree is read once per object,
  with a budget (a tree may share or loop); an MCID belongs to the first
  element which has it; an artifact in kept marked content, or which
  holds some, is taken out (the picture's own irregularities).
* With an alternative text of the user's, it is a Figure: its marked
  content is taken out of its content and of its forms and patterns, but
  the optional content (`BDC /OC`), and the `/StructParents` of its forms
  (`pdf_strip_marked_content`, in `pdf_tags.cpp`).

Whether a picture is grafted is decided on the content the graft gives
(`pdf_graft_marked_content`): all it paints tagged or an artifact, its
blocks balanced; an MCID is read from the properties of a BDC or from
the resource its name gives. PDFHummus does not graft when the pictures
are distilled again (Ghostscript), and never reads an encrypted picture
(its parser damages its memory, also untagged; `hummus_pdf_plain` reads
the key Encrypt of the trailers and of the cross-reference streams, the
names decoded): Ghostscript gives its size and an unencrypted copy, or
else the placeholder is included; a picture it cannot copy is the
placeholder too, its fonts embedded. A reference of a picture to an
object which it lacks is null (it made PDFHummus hang). The MuPDF writer
reads each PDF picture once for all its uses: one copy of its resources
and of its layers.

The lexer of the content skips the data of an inline image by its length,
and the streams of a page are one content. The fonts and the forms of a
picture are each read once (a picture may make cycles), and a font which
also draws in a pattern or a Type 3 glyph, or by an ExtGState, keeps all
its codes.

## Checking it

`verapdf -f ua1 file.pdf` (Homebrew: `brew install verapdf`). On
2026-11-22 the 18 samples of `tests/documents/samples`, a paper of 21
pages, 14 documents drawn at random from the corpus
(`~/t/lab/test-documents`), a document with pictures of other programs
and one with alternative texts were compliant, from both writers. `convert-more` checks the
structure of a small document (`test-tagged-pdf-export`).

`tests/documents/tagged.sh [sample...]` does it for the samples. A dump of the
structure tree (`mutool run tests/documents/pdf-structure.js file.pdf`) is the
quickest way to see what a document gives; the PDF/UA rules are explained
in the Matterhorn Protocol.

## Left

* **Included PDF pictures**: a picture which is not tagged is a Figure
  as a whole. Of a tagged picture, only the structure of its first page,
  without its attributes (the headers of its tables) and with its
  headings as paragraphs, is grafted. A font of pictograms in a picture,
  a Type 3 font of no ToUnicode, and with PDFHummus a font which is a
  direct dictionary, are not compliant.
* **Table headers** (TH, with a Scope): TeXmacs has no markup for them.
* **The OpenType math variants** (#943): U+FFFD in the text until the
  fonts record their characters.
* **The call of a footnote** has the place of the footnote, as the number
  in front of the note, so it is in the Note rather than in its paragraph.
* **Marked content and the graphics state**: a block may open inside a
  `q` (a clipping) and close after its `Q`; veraPDF and MuPDF accept it,
  ISO 32000-1 does not forbid it, but they do not nest.
* **Headings which other macros write** (those not in `pdf-tag-roles`).
* **Languages** which `language_to_locale` does not know are `en-US`.
* **A missing glyph** (`.notdef`) drawn by the typesetter (STIX size
  fonts in a thesis of the corpus) is forbidden by PDF/UA (7.21.8).
* **PDF/UA-2** (PDF 2.0): MathML as associated files of the formulas,
  the namespaces of PDF 2.0.
