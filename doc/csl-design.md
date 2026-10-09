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
  (13 styles and 26 locales come with TeXmacs, see `misc/csl/README.md`).
  The installed styles are offered in the bibliography dialog and in the
  focus bar.
- The locale is the `default-locale` of the style, or else the language of
  the document.
- Rendering elements: text, number, label, date (localized, ranges,
  seasons), names (initials, particles, et al., substitution, labels,
  editor-translator), group, choose; text case, quotes, formatting, affixes,
  punctuation; sorting with macros; page ranges; ordinals;
  `subsequent-author-substitute` (rules complete-all and complete-each).
- Citations as the style prints them (see "Citations" below): sorted,
  grouped and collapsed (`collapse` year, year-suffix, year-suffix-ranged,
  citation-number), with locators, with the disambiguation of CSL (more
  names, initials and given names, the condition `disambiguate`, year
  suffixes), in the forms parenthetical, textual, authors only, year only,
  with the positions first, subsequent, ibid, and in footnotes for the
  styles of the class note.

Not done yet:

- `subsequent-author-substitute` with the rules partial-each and
  partial-first, `display` blocks, non-Latin name order, markup inside
  names.
- Disambiguation which looks at the subsequent form of a cite; the
  incremental use of the condition `disambiguate`.
- Downloading styles by name, attaching the style to the document,
  CSL-JSON from Zotero.
- The LaTeX export of a document with a `csl-` style keeps the rendered
  bibliography and exports the citations as `\cite{key}`: LaTeX then
  prints the labels of TeXmacs, not the citations of the style.

Conformance: `csl-run-fixtures` of `(csl csl-fixtures)` runs the official
fixtures (`processor-tests/humans` of the CSL test-suite, not distributed
with TeXmacs): 646 of 845 pass, 136 fail, 63 are skipped (they give a
history of citations, abbreviations or other things which the runner does
not feed yet). The results are the same with S7 and with Guile 1.8. The
failures are mostly conventions of citeproc-js (markup inside fields,
names in other scripts, superscript ordinals) and corners of names, sorting
and text case. On eight varied references, the bibliographies of the 13
styles which come with TeXmacs agree with `pandoc --citeproc` except for
deliberate differences: the particle of "van der Hoeven" stays with the
family name, "Second" edition becomes "2nd ed.", the month of a `misc`
entry is kept.

## Citations

A citation depends on the other ones (year suffixes, grouping, ibid), so
the citations are rendered with the bibliography and kept in the document:

1. The bibliography in a CSL style carries a binding `PREFIX-csl` (its
   value is `note` for a style which cites in footnotes).
2. When this binding exists, the citation macros (`cite-csl` in
   `packages/standard/std-automatic.ts`, used by `cite`, `cite-detail`,
   `cite-textual`, `cite-parenthesized`, `cite-author`, `cite-year`, also
   in the packages `cite-author-year` and `cite-sort`) write each citation
   to the auxiliary data `PREFIX-cites`, as `(tuple mode (tuple entry...))`
   with the modes `p`, `t`, `a`, `y`, and count the citations in `cite-nr`.
3. At the next update, `csl-bib-process` reads `PREFIX-cites`, renders each
   different citation and puts the results in the first entry of the
   bibliography, as `(set-binding "PREFIX-cite-MODE:SIGNATURE" tree)`;
   the signature is made of the keys and details. A citation which reads
   otherwise where it stands (a later citation of a work, ibid) gets one
   more binding, with `#N` after the name, N being its number.
4. The macros show the binding of the citation, or else what they showed
   before (the label of the entry between brackets).

So two updates are needed after the style of a bibliography becomes a CSL
style (Document -> Update -> All makes three). Documents without a CSL
bibliography are typeset as before, and nothing more is written to their
auxiliary data. The second argument of `cite-detail` is a locator when it
reads like one ("p. 12", "chap. 3", "12-15"), else a suffix of the cite.
In a style of the class note, the macro `cite-note` puts the citation in a
footnote; inside `footnote`, `cite-note` is `cite-inline`. A textual
citation then has two bindings: the names (mode `t`), which stay in the
text, and the rest (mode `n`), which goes to the footnote. The values of
the bindings are quoted, so that the macros inside them are expanded where
the citation stands.

Known limit: the number N of a citation is counted by the typesetter in
the order in which the macros are evaluated, and the processor counts the
entries of the auxiliary data; a citation which is evaluated but not
typeset (in a title which is also used for a header) could shift the
numbers of the later citations, which would then miss their "later
citation" forms.

## Modules (`TeXmacs/progs/csl`)

| Module | Role |
|---|---|
| `csl-utils` | nodes of styles, rich text, changes of case, a stable sort |
| `csl-style` | loading styles and locales, dependent styles, terms, ordinals |
| `csl-data` | items: from BibTeX entries (types, fields, names, dates, sentence case) and from CSL-JSON |
| `csl-names` | one name, lists of names, initials |
| `csl-render` | the rendering elements, numbers, dates, conditions, sort keys |
| `csl-process` | the processor: sorting, numbering, bibliography, one cite, disambiguation |
| `csl-cite` | citations of several cites: grouping, collapsing, textual form |
| `csl-output` | rich text to TeXmacs trees and to the HTML of the fixtures: quotes, flip-flop, punctuation |
| `csl-bib` | the bibliography and the citations of a document; entry points `csl-bib-process`, `csl-available-styles` |
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

1. Style management: download by name, the style attached to the document.
2. CSL-JSON from Zotero, for the fields which BibTeX does not have.
3. The remaining fixtures of names, sorting and text case.
4. The LaTeX export of citations in CSL styles.

## A bug found on the way

A run of all fixtures in one process raises now and then errors
`car: argument is a free cell` in S7, in fixtures which change from run to
run. They were seen with the Vue build and with a Qt build, mostly with
`(*s7* 'cache-macro-expansions?)` on (the local s7 patch 0005, which
TeXmacs enables), and once with it off. Guile runs the same fixtures
without any error. It looks like a pair which is collected while it is in
use; not investigated further here.
