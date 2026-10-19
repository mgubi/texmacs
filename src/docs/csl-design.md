# CSL bibliography styles in TeXmacs

Branch `wip_csl` (from `upsilon_texmacs`). A processor of the Citation Style
Language (CSL 1.0.2) written in Scheme, so that TeXmacs can use the styles
of <https://github.com/citation-style-language/styles> (2864 independent
styles, 8001 journal aliases) for bibliographies and citations.

## Status (2026-10-12)

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

What is open is listed in "Open points" below.

Conformance: `csl-run-fixtures` of `(csl csl-fixtures)` runs the official
fixtures (`processor-tests/humans` of the CSL test-suite, not distributed
with TeXmacs): 652 of 845 pass, 130 fail, 63 are skipped (they give a
history of citations, abbreviations or other things which the runner does
not feed yet). The results are the same with S7 and with Guile 1.8. The
failures are mostly conventions of citeproc-js (markup inside fields,
names in other scripts, superscript ordinals) and corners of names, sorting
and text case. On eight varied references, the bibliographies of the 13
styles which come with TeXmacs differ from those of `pandoc --citeproc` in
two to five entries each. In the four styles whose differences were read
(IEEE, APA, Chicago author-date, Nature), each one had one of these
causes: formulas in titles (kept here, flattened by pandoc), the particle
of "van der Hoeven" (it stays with the family name here), "Second" edition
(here "2nd ed."), and a `misc` entry (its month is kept and its title is
quoted here). The differences of the nine other styles were counted, not
read.

## Citations

A citation depends on the other ones (year suffixes, grouping, ibid), so
the citations are rendered with the bibliography and kept in the document:

1. The bibliography in a CSL style carries a binding `PREFIX-csl` (its
   value is `note` for a style which cites in footnotes).
2. When this binding exists, the citation macros (`cite-csl` in
   `packages/standard/std-automatic.ts`, used by `cite`, `cite-detail`,
   `cite-textual`, `cite-parenthesized`, `cite-author`, `cite-year`, also
   in the packages `cite-author-year` and `cite-sort`) write each citation
   to the auxiliary data `PREFIX-cites`, as
   `(tuple mode (tuple entry...) nr)` with the modes `p`, `t`, `a`, `y`,
   and count the citations of the document in `cite-nr`, which is `nr`.
3. At the next update, `csl-bib-process` reads `PREFIX-cites`, renders each
   different citation and puts the results in the first entry of the
   bibliography, as `(set-binding "PREFIX-cite-MODE:SIGNATURE" tree)`;
   the signature is made of the keys and details (`key{detail,`, and
   `key{*,` for a detail which is not a plain text). A citation which reads
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

The number N of a citation is counted by the typesetter, which is a
known limit: see "Open points".

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
  the database tool. For a CSL style it takes the entries of `.bib` files
  from the files themselves (`bib-retrieve-raw-entries`), and only the
  other sources (the document, the database, Zotero) from the database:
  the database drops the braces of the titles and lowercases the titles of
  articles, whatever their language, while a CSL style changes their case
  itself and needs the braces (`{Fourier}`) and the `language` field.
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

## Open points

As of 2026-10-12, with the processor merged into `upsilon_texmacs`.

### What gives a wrong result today

- **LaTeX export, the corners** (the export itself works since
  2026-10-18, see "LaTeX export" below). In a note style, `cite-textual`
  gives the full names of the package ("Donald E. Knuth" where TeXmacs
  writes "Knuth" after a first citation) and `cite-year` the whole note.
  `cite-year` keeps the brackets of a style without names ("[2]" where
  TeXmacs writes "2"). citeproc-lua puts a capital after a brace
  ("The {\TeX}book" becomes "The TEXBook" in title case) and, in MLA, leaves
  out the title which tells two works of an author apart.
- **The later forms of citations can be missed.** The short or ibid form
  of a repeated citation is found by the number of the citation, which the
  typesetter counts as the macros are evaluated, while the processor counts
  the entries of the auxiliary data. A citation which is evaluated but not
  typeset (in a title which is also used for a header) could shift the
  numbers of the citations after it. Foreseen, not reproduced.
- **A textual citation of a note style inside a footnote of the author.**
  The names stay in the text and the rest should stay there too
  (`cite-note` is `cite-inline` inside `footnote`); not tried.
- **`cite-textual` takes keys only**: "Knuth (1984, p. 12)" cannot be
  written.
- **Nothing tells that an update is pending.** A new or changed citation
  shows its key, or the label of its entry between brackets, until the next
  update, and a style which has just been chosen needs two updates.

### Parts of CSL which are not implemented

- `subsequent-author-substitute` with the rules which work name by name
  (partial-each, partial-first); complete-all and complete-each are done.
- The attribute `display` (block, indent, left-margin, right-inline) has
  no effect in the TeXmacs output; 3% of the styles use it.
- Names in other scripts than Latin are ordered as Western names.
- Markup inside the fields of names is not read.
- The disambiguation does not look at the subsequent form of a cite, and
  the condition `disambiguate` is set for all the ambiguous items at once
  instead of one after the other.
- Sorting compares unaccented lower case strings, not by the rules of the
  locale.
- The locators are recognized from a fixed list of English abbreviations
  ("p.", "chap.", "sec."...); any other detail becomes a suffix of the
  cite.

### What was not checked

- **The fixtures**: 652 of 845 pass, 130 fail, 63 are skipped because the
  runner does not feed histories of citations nor abbreviations.
- **Real styles**: only the 13 styles which come along were compared with
  pandoc, on bibliographies of eight references, and the differences of
  four of them were read (see the status). Citations were compared on
  three references.
- **The look.** Everything was checked on exports and on trees: the
  hanging indentations, the widths of the labels, the footnotes and the
  entries of the style chooser were never seen in a window.
- **The browser build** was neither built nor run with this code.
- **The database tool**: `doi`, `url`, `language` and `isbn` are not among
  its declared fields; the entries of `.bib` files no longer go through it
  (see above), but those of the database itself and of Zotero still do,
  and titles stored there lose their braces.

### Not started

- Style management: download a style by its name, attach the style to the
  document so that it travels with it, a chooser which copes with
  thousands of styles.
- CSL-JSON from Zotero, for the types and the fields which BibTeX does not
  have: today everything goes through BibTeX entries.
- A chapter in the developer documentation of TeXmacs (there is this file
  and the section of the manual).
- A pull request to `wip_fixes`: the work is on `wip_csl` and in
  `upsilon_texmacs` only.

## Next steps

1. A look at documents in a window: what a user meets first.
2. Style management: download by name, the style attached to the document.
3. CSL-JSON from Zotero.
4. The remaining fixtures of names, sorting and text case.

## LaTeX export

A document whose first bibliography has a style `csl-NAME` is exported for
the LaTeX package `citation-style-language` (citeproc-lua, by Zeping Lee,
in TeX Live), which formats the citations and the bibliography with the CSL
style itself (`tmtex.scm`, "Citations in a CSL style", mode `csl-package%`):

- the preamble loads the package, without natbib (both define `\citeauthor`
  and `\citep`), `\cslsetup{style = NAME}` (with the locale of the document
  when it is not English) and
  `\addbibresource[journal-abbreviation=false]{BASE-refs.bib}`;
- `cite` is `\cite`, `cite-detail` `\cite[page={12-14}]` when its details
  read as a locator (`csl-cite-details`, as for the citations of TeXmacs)
  and `\cite[suffix={, text}]` otherwise, `cite-textual` `\textcite`,
  `cite-parenthesized` `\parencite`, `cite-author` `\citeauthor`,
  `cite-year` `\citeyear`, `nocite` `\nocite`; the bibliography is
  `\printbibliography`;
- in a style whose citations show no names (numbers, labels: tested by
  `csl-citation-shows-names?`), `\textcite` and `\citeauthor` print
  `[NO_PRINTED_FORM]`, so the names come from TeXmacs
  (`csl-textual-authors`): "Knuth and van der Hoeven \cite{knuth86b}", and
  `cite-year` is `\citeyearpar`;
- next to the .tex file go the cited entries (`bib-entries-as-bibtex`: from the
  sources of `bib-compile-sub`, or the whole .bib file without the database
  tool) and a copy of the style, unless a file of its name is there.

Without a .tex file (a conversion to a string) nothing is written and the
resource is the .bib file of the bibliography. Compared on a document with
seven kinds of citations in the 13 styles which come with TeXmacs, compiled
twice with LuaLaTeX: the text is that of TeXmacs in APA, Chicago
author-date, Elsevier Harvard, Harvard, IEEE, Springer author-date and
Nature; the others differ as said in "Open points". The suite `csl` checks
the export (group "LaTeX export"), without running LaTeX.

## A bug found on the way

A run of all fixtures in one process raises now and then errors
`car: argument is a free cell` in S7, in fixtures which change from run to
run. They were seen with the Vue build and with a Qt build, mostly with
`(*s7* 'cache-macro-expansions?)` on (the local s7 patch 0005, which
TeXmacs enables), and once with it off. Guile runs the same fixtures
without any error.

It was the collector of S7 taking the cells of a list which C++ was still
building (S7 does not see the variables of C++), fixed by 0f460c3db4 on
2026-10-13: a `tmscm_builder` holds the collector while a list is built
(`src/docs/s7/01-cpp-binding.md`, 1.5). Checked on 2026-10-17: 23 runs of
all fixtures, no error; the same tree with the builder emptied, 4 errors
in each of 10 runs (in `condition_NumberIsNumeric`,
`disambiguate_DisambiguateTrueReflectedInBibliography`,
`sort_ConditionalMacroDates` and `sort_TestInheritance`), as before.
