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

The PDF writer on MuPDF (`src/Plugins/MuPDF/mupdf_pdf_renderer.cpp`, the
writer of the Vue builds since #942) writes a tagged PDF when asked:
*Preferences → Convert → Pdf → Tagged Pdf, for accessibility*, the
preference `texmacs->pdf:tagged`, or `TEXMACS_PDF_TAGGED=1`. PDFHummus
(Qt, Cocoa) does not tag.

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
* Headings: `H:n` in the table is a rank; the ranks a document uses
  become H1, H2... in their order, and a heading is at most one level
  deeper than the one before it (PDF/UA asks for no skipped level).
* Lists: every child of the document of a list is an LI, with an Lbl
  (the `item`) and an LBody; a nested list or an equation in a list is in
  the LBody.
* Tables: `tabular` (or a `table` which no tabular holds) is the Table,
  `row` a TR, `cell` a TD, with `ColSpan` and `RowSpan` read from the
  `cwith` of the formats around the table. A row has all its cells,
  those which draw nothing included, and not those which a span covers
  (`row_cells`), so that the rows have the same number of columns.
* A formula (`math`, `equation*`, `eqnarray*`, `align*`...) or a picture
  is a whole: nothing inside it is an element of its own. Its Alt is its
  LaTeX (`pdf-tag-alt`), for a picture the name of its file for now.
* A link of the document (`hlink`) is a Link element; a link of no
  `hlink` (the call of a footnote and its way back) gets a Link of its
  own in the element of its text.

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

**Fonts.** PDF/UA wants a Unicode value for every glyph drawn: in a tagged
PDF the glyphs of no known character (the pieces of the horizontal braces
of cmex, some variants of the OpenType math fonts, #943) are mapped to
U+FFFD, and a TrueType CID font gets `/CIDToGIDMap /Identity`.

## Checking it

`verapdf -f ua1 file.pdf` (Homebrew: `brew install verapdf`). On
2026-11-22 the 18 samples of `tests/documents/samples`, a paper of 21
pages, and 14 documents drawn at random from the corpus
(`~/t/lab/test-documents`) were compliant, but one: a picture included
as a PDF whose own font is not embedded. `convert-more` checks the
structure of a small document (`test-tagged-pdf-export`).

`tests/documents/tagged.sh [sample...]` does it for the samples. A dump of the
structure tree (`mutool run tests/documents/pdf-structure.js file.pdf`) is the
quickest way to see what a document gives; the PDF/UA rules are explained
in the Matterhorn Protocol.

## Left

* **Alternate descriptions of the pictures**: a picture says the name of
  its file. It wants a way to write one in the document, and a check that
  each picture has one (the Typst exporter has a dialog for it).
* **Included PDF and EPS pictures**: their fonts, when not embedded, make
  the PDF non compliant; their content is not tagged either (an artifact
  inside a Figure would be right).
* **Table headers** (TH, with a Scope): TeXmacs has no markup for them.
* **The OpenType math variants** (#943): U+FFFD in the text until the
  fonts record their characters.
* **PDFHummus** does not tag; the tagger and the box hook are its for the
  taking.
* **PDF/UA-2** (PDF 2.0): MathML as associated files of the formulas,
  the namespaces of PDF 2.0.
* Tagging by default, once tried with screen readers and the viewers.
