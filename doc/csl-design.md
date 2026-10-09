# CSL bibliography styles in TeXmacs

Branch `wip_csl` (from `maxs_texmacs`). A processor of the Citation Style
Language (CSL 1.0.2) written in Scheme, so that TeXmacs can use the styles
of <https://github.com/citation-style-language/styles> (2864 independent
styles, 8001 journal aliases) for bibliographies and citations.

## Status (2026-10-09)

Working:

- A bibliography whose style is `csl-NAME` is formatted by `NAME.csl`, from
  a `.bib` file, the database tool or Zotero, through the usual
  Document -> Update -> Bibliography. Formulas and other markup of the
  fields stay TeXmacs trees.
- The style is sought next to the document, in
  `$TEXMACS_HOME_PATH/csl/styles` and in `$TEXMACS_PATH/misc/csl/styles`
  (12 styles and 26 locales come with TeXmacs, see `misc/csl/README.md`).
  The installed styles are offered in the bibliography dialog and in the
  focus bar.
- The locale is the `default-locale` of the style, or else the language of
  the document.
- Rendering elements: text, number, label, date (localized, ranges,
  seasons), names (initials, particles, et al., substitution, labels,
  editor-translator), group, choose; text case, quotes, formatting, affixes,
  punctuation; sorting with macros; page ranges; ordinals.
- Citations of numeric and label styles work with the present `cite`
  macros: the label of each entry is what the style prints for it.

Not done yet:

- Citations as CSL wants them: clusters with sorting and collapsing,
  disambiguation (year suffixes, added names and given names), locators,
  ibid and subsequent positions, footnote citations. Today an author-date
  style gives the right bibliography, but `cite` shows "[Knuth, 1984]" with
  the brackets of TeXmacs, and two works of one author in one year are not
  told apart.
- `subsequent-author-substitute`, `display` blocks, non-Latin name order.
- Downloading styles by name, attaching the style to the document.
- LaTeX export of documents with `csl-` styles keeps the rendered
  bibliography; `\bibliographystyle` cannot name a CSL style.

Conformance: `csl-run-fixtures` of `(csl csl-fixtures)` runs the official
fixtures (`processor-tests/humans` of the CSL test-suite, not distributed
with TeXmacs): 533 of 845 pass, 249 fail, 63 are skipped (they use the
citation history, abbreviations or other features outside CSL 1.0.2). Most
failures are in disambiguation (53), names (41), collapsing (18), and the
conventions of citeproc-js for markup inside fields.

## Modules (`TeXmacs/progs/csl`)

| Module | Role |
|---|---|
| `csl-utils` | nodes of styles, rich text, changes of case, a stable sort |
| `csl-style` | loading styles and locales, dependent styles, terms, ordinals |
| `csl-data` | items: from BibTeX entries (types, fields, names, dates, sentence case) and from CSL-JSON |
| `csl-names` | one name, lists of names, initials |
| `csl-render` | the rendering elements, numbers, dates, conditions, sort keys |
| `csl-process` | the processor: sorting, numbering, bibliography, citation of a list of cites |
| `csl-output` | rich text to TeXmacs trees and to the HTML of the fixtures: quotes, flip-flop, punctuation |
| `csl-bib` | the bibliography of a document; entry points `csl-bib-process`, `csl-available-styles` |
| `csl-fixtures` | runner of the official fixtures |

All strings are in the TeXmacs encoding. A rendered value is "rich text":
a string, `(cat ...)`, `(fmt alist ...)`, `(nocase ...)` or `(raw tree)`;
changes of case skip `nocase` and `raw`.

## How it plugs in

- `generate_bibliography` (`src/Edit/Process/edit_process.cpp`) treats a
  style `csl-*` like a `tm-*` style and calls `csl-bib-process`;
  `bib-compile-sub` (`progs/database/bib-manage.scm`) does the same with
  the database tool.
- The result keeps the shape of the other bibliographies,
  `(bib-list largest (document (concat (bibitem* text) (label key) ...)))`,
  inside a `with` which redefines `transform-bibitem` (the label as the
  style prints it) or `render-bibitem` (no label, hanging indentation).
  The converters and `cite` therefore work unchanged.

## Mapping of BibTeX entries

Types: article -> article-journal, book, booklet -> pamphlet, inbook and
incollection -> chapter, inproceedings and conference -> paper-conference,
manual -> book, mastersthesis and phdthesis -> thesis (with a genre),
techreport -> report, unpublished -> manuscript, misc -> document, online ->
webpage.

Fields: journal and booktitle -> container-title; school, institution,
organization -> publisher; address -> publisher-place; number -> issue
(articles), number (reports) or collection-number; series ->
collection-title; pages -> page; year, month, day or date -> issued; doi,
url, isbn, issn; howpublished -> URL or publisher; arXiv eprint -> URL.
The von part of a name is the non-dropping particle. "and others" forces
"et al.". Titles are turned into sentence case (words with only an initial
capital lose it, braces protect), as CSL styles expect.

## Next steps

1. Citation clusters: `cite` writes the whole cluster to the auxiliary
   data, the processor renders every cluster at the update and the
   document keeps the results; then year suffixes and the other
   disambiguation, collapsing, locators, textual and author-only forms.
2. Note styles: footnote citations, positions.
3. Remaining fixtures of names and dates.
4. Style management: download by name, the style attached to the document,
   CSL-JSON from Zotero.

## A bug found on the way

With `(*s7* 'cache-macro-expansions?)` on (the local s7 patch 0005, which
TeXmacs enables), a run of all fixtures in one process raises two errors
`car: argument is a free cell` (in three runs out of three; which fixtures
fail changes when the code changes); with the cache off there is none in
three runs. It looks like a cached expansion used after its pairs were
collected; not investigated further here.

Not tested yet: Guile (the code avoids S7-only constructs, but only the
S7 build was run).
