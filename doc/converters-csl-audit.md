# Audit of the Scheme code of the converters and of the CSL processor

Made on 2026-10-12 on `maxs_texmacs` (ca9fa71343): about 14,400 lines of
Scheme in `src/TeXmacs/progs/convert/markdown`, `progs/convert/office` and
`progs/csl`, with the citation macros of `packages/standard/std-automatic.ts`.

**Method.** Four independent readings (CSL; Markdown; office import;
office export), each with a headless TeXmacs (S7) to try things. Every
finding says how far it was checked:

- **V** verified: reproduced by the reviewer and again, with another
  script, afterwards;
- **R** reproduced by the reviewer only;
- **I** inferred from reading, not run.

Nothing was fixed. No file of Microsoft Word or Google Docs was available:
what Word itself does is always inferred. Guile 1.8 and femtolisp were not
run by the reviewers; where it matters it was checked afterwards and is
said so. Paths are relative to `src/TeXmacs/progs/`.

## 0. State after the fixes

The findings were fixed on the branch `audit-fixes`, with a check for each
in the suites `csl` (182 checks), `markdown` (314) and `office` (361). The
51 suites pass on S7; `csl`, `markdown` and `office` also pass on Guile 1.8
and femtolisp (there without the two changes in C++). The official CSL
fixtures went from 646 to 654 of 845, none lost. Not run: Microsoft Word.

What is left:

- 1.4: `arrange_bib` (C++) still turns `--` into a dash in a URL once the
  bibliography is in the document.
- 1.13: a title in capitals is left as it is ("THE ART OF WAR"); the
  specification would give "The Art of War", but the fixture
  `textcase_CapitalsUntouched` wants capitals kept.
- 1.24: French has no feminine long ordinals in its locale file.
- 2.18: emphasis which starts or ends with markup next to a letter.
- 3.2: the left-right arrow accent stays `(above x "<leftrightarrow>")`.
- 3.26: the recursion as deep as the number of paragraphs.
- 4.9: the numbers of the rows of an aligned equation follow the formula,
  they are not beside their rows.
- 4.11: an empty paragraph is now written between two tables; what Word
  shows was not seen.
- Seen on the way, not fixed: the text of a text box of a .docx
  (`wps:txbx`), `||x||` in the formulas of an .odt, the heading of an
  appendix ("A App"), pages with a comma for the thousands in CSL, 500
  levels of nested lists in Markdown (quadratic).

1.5 and 3.9 were fixed in C++: `bib_field_pages` keeps pages which are not
numbers as they are written, and the XML parser limits the depth of the
elements (1000) and the expansion of declared entities (4 MB).

The rest of this document is the audit as it was made.

## 1. Summary

| | Findings | Verified again (V) | Checked and not confirmed |
|---|---:|---:|---:|
| CSL processor | 24 | 11 | 0 |
| Markdown | 25 | 9 | 0 |
| Office, import | 26 | 6 | 0 |
| Office, export | 19 | 4 | 1 |

(some findings group several small ones; the others were reproduced by
their reviewer, or inferred where the table says I)

The ones to fix first, all verified:

1. Markdown: a text which starts with `[x] ` or `[^` is exported so that
   reading it back turns the rest of the document into one formula (2.1).
2. Markdown: a backquote in imported code becomes a NUL character (2.2).
3. Markdown: a document which starts with a rule loses its text up to the
   next rule; a paragraph `[label]: words` disappears (2.3, 2.4).
4. CSL: DOIs and URLs with `_`, `~` or `&` from BibTeX are damaged or
   dropped; pages which are not numbers become "0" (1.4, 1.5).
5. CSL: undated references lose "(n.d.)" in APA; works with editors only
   are wrong in Chicago; a work with a title only prints it twice (1.1 to
   1.3).
6. Office import: `a/b+c` in a formula becomes a bracket around `b+c`;
   the accents dot and double dot become control characters (3.1, 3.2).
7. Office import: text inside `w:dir`/`w:bdo`, symbols of `w:sym` and the
   text before a field which ends in a later paragraph are dropped (3.3,
   3.5, 3.7).
8. Office export: binomials and `tfrac`/`dfrac`/`cfrac` vanish from
   formulas; a forbidden code point makes both files unreadable (4.1, 4.2).
9. Inputs of a hundred bytes which hang: nested images in Markdown, a
   footnote which cites itself (Markdown and .docx), a CSL macro which
   calls itself (2.6, 2.7, 3.8, 1.7).

## 2. CSL processor (`csl/`)

| # | | Finding | Where |
|---|---|---|---|
| 1.1 | V | A group whose only variable is an empty `year-suffix` is suppressed: APA prints "(Doe)" and no "(n.d.)" for an undated work. `year-suffix` must not count for the suppression of groups. | `csl-render.scm` `render-text`, `render-group` |
| 1.2 | V | `cs:names` inside `cs:choose` inside `cs:substitute` neither inherit `cs:name` nor suppress their variable, and the whole branch is printed: Chicago author-date gives "J. Doe, K. Roe. 1999. T. Edited by J. Doe and K. Roe." for a work with editors only (expected "Doe, J., and K. Roe, eds. 1999. T."). | `csl-render.scm` `render-substitute` |
| 1.3 | V | A substitution made by a macro does not suppress its variable: a work with a title only prints "Y. Y." in APA and Chicago. | `csl-render.scm` `render-substitute` |
| 1.4 | V | DOI and URL from BibTeX: `10.1007/978-3-540-30108-0_4` becomes `...0-04`, `~doe/my_paper` becomes `doe/mypaper`; a URL with `&` is dropped (R). The parser of BibTeX reads these fields as LaTeX text. | `csl-data.scm` `tree->text`; `csl-utils.scm` `raw->string` |
| 1.5 | V | `pages={e1234}` gives "0", `pages={12, 15--17}` gives "12-15": the parser hands over `(bib-pages "0")`. An empty field prints "0." (R). | `csl-data.scm` `bib-pages->page`; the parser |
| 1.6 | R | Disambiguation by names and given names is about cubic in the number of names: two works with the same 40 authors take 2.2 s, 100 authors about 30 s (extrapolated), with a style of rule by-cite. | `csl-process.scm` `refine!` |
| 1.7 | R | A macro of a style which calls itself never ends (killed after 120 s). | `csl-render.scm` `render-text` |
| 1.8 | R/I | With two CSL bibliographies (two prefixes) the bindings `#N` of the later citations go to the wrong citations: `cite-nr` counts for the document, the processor counts for each prefix. Bindings reproduced, display inferred. | `std-automatic.ts` `cite-csl`; `csl-bib.scm` |
| 1.9 | V | The cache of locales is keyed by the name of the style: two styles of the same name share the locale of the first ("A UND B" twice). A style next to the document and the installed one of the same name mix their terms. | `csl-style.scm` `csl-locale` |
| 1.10 | V | A range of years where the other parts are empty still prints the delimiter: "(1999–2001, –)" in APA. | `csl-render.scm` `render-date-range` |
| 1.11 | R | Title case is applied when the document is not in English (the test looks at the style, not at the locale in use): "La Vie En Rose Et Le Reste" with Chicago in a French document. The specification can be read both ways; pandoc uses the locale in use. | `csl-render.scm` `english?` |
| 1.12 | V | Sentence case from BibTeX: "What i learned about vitamin a"; after a braced first word the next one is taken for the first ("NASA Reports on mars"). The field `language` does not switch it off for German titles (R; a choice to make). | `csl-data.scm` `sentence-case` |
| 1.13 | V | Title case of a text in capitals: "THE ART of WAR". | `csl-utils.scm` `title-word` |
| 1.14 | R | With `second-field-align` the suffix of the year is printed in the label and again in the body. | `csl-process.scm` `bibliography-entry` |
| 1.15 | R | `citation-label` and `citation-number` reached through a macro are not seen: no suffix on equal labels, and the bibliography takes the layout without labels (I). | `csl-process.scm` `node-mentions?` |
| 1.16 | R | Sort keys given by a macro compare numbers as text: "[1,10–12,2,9]"; volume 10 before 9. | `csl-process.scm` `sort-key-value` |
| 1.17 | R | The affixes of `cs:names` are applied twice to a `cs:names` which substitutes: "[[E. Ed] ] T1". | `csl-render.scm` `render-names` |
| 1.18 | R | `cite-detail` whose detail is not a plain string gets no CSL citation at all. | `csl-bib.scm` `entry-signature` |
| 1.19 | R | `cite-author` of an unknown key, or of a work without names in a numeric style, shows "[CSL STYLE ERROR: reference with no printed form.]". | `csl-cite.scm` |
| 1.20 | R | A key which contains "@" collides with a `cite-detail`. | `csl-bib.scm`, `std-automatic.ts` |
| 1.21 | V | Two entries with the same key: the last one is printed twice. | `csl-process.scm` `csl-make-processor` |
| 1.22 | V | Markup in a field with a tag which is not known (`<span class="x">`) loses the text after its end tag. | `csl-data.scm` `build-markup` |
| 1.23 | R | Italic small capitals keep only the small capitals (both are `font-shape`); the no-break spaces of a locale come out as `<varspace>` (French quotes). | `csl-output.scm` |
| 1.24 | R | Smaller: the field `type` does not replace "PhD thesis"; `raw->string` flattens a `with` with its arguments; a style without `cs:layout` raises an error which nothing catches; `author={and others}` alone prints "and others"; feminine long ordinals in French; a detail "Lemma2" is taken for a page; a bibliography with no entry loses all the bindings. | several |

Found sound: linear in the number of references up to 1000; nine odd
BibTeX entries through the 13 styles without an error; unknown locales;
the formats of page ranges; no mutation of cached styles.

## 3. Markdown (`convert/markdown/`)

| # | | Finding | Where |
|---|---|---|---|
| 2.1 | V | Export then import: "[x] done" is written `\[x] done`, which is read as the start of a display formula running to the end of the document. Same for a text starting with `[^`. | `markdownout.scm` escape of `[`; `markdownin.scm` `md-math-start?`, `md-math` |
| 2.2 | V | A backquote in imported code or text becomes the character of code 0 (`utf8->cork` of a backquote). | `markdowntm.scm` `mdtm-text` |
| 2.3 | V | A document which starts with the rule `---` is taken for a header: everything up to the next `---` is dropped. | `markdownin.scm` `md-front-matter` |
| 2.4 | V | A paragraph `[Note]: see the appendix for details` is taken for the definition of a link and disappears. | `markdownin.scm` `md-ref-def` |
| 2.5 | V | A line which starts with `$$` and has no end swallows the document; text after `$$x$$` on the same line is dropped (R). | `markdownin.scm` `md-math` |
| 2.6 | V | Nested images take exponential time: 16 levels (100 bytes) 0.9 s, 18 levels 4.5 s (R). `md-link` is called twice at each level. | `markdownin.scm` `md-tokens` |
| 2.7 | R | A footnote which refers to itself recurses without bound on import. | `markdowntm.scm` `mdtm-footnote` |
| 2.8 | V | Emphasis delimiters without partners are quadratic: 6 kB 0.4 s, 12 kB 1.9 s, 24 kB 16 s (R). | `markdownin.scm` `md-emphasis` |
| 2.9 | R | Other searches to the end of the paragraph for each character: `[` without `]`, `<` without `>`, `&` without `;`, `\(` without `\)` (0.5 to 1.5 s for 20 to 50 kB). | `markdownin.scm` |
| 2.10 | V | "Wow!" followed by a link is exported `Wow![here](...)` and read back as an image. | `markdownout.scm` |
| 2.11 | R | A URL in plain text is exported with backslashes (`\&`, `\_`) which the import keeps inside the link. | `markdownout.scm`, `md-bare-link` |
| 2.12 | R | A formula, a list or code in a cell of a table is dropped on export. | `tmmarkdown.scm` `tmmd-cell` |
| 2.13 | R | `\[see Smith (2010)\]` written by other tools (pandoc escapes brackets so) becomes a display formula. A documented heuristic, but narrow. | `markdownin.scm` `md-literal-brackets?` |
| 2.14 | R | An HTML comment with a blank line inside: its end becomes visible text. | `markdownin.scm` `md-html` |
| 2.15 | R | A table right after a line of a paragraph is not recognized. | `markdownin.scm` |
| 2.16 | R | A cell with code which ends with a backslash breaks its row. | `markdownin.scm` `md-split-row` |
| 2.17 | R | The header: quotes and backslashes are escaped on export and not on import. | `markdownout.scm`, `md-yaml-value` |
| 2.18 | R | Emphasis which ends with punctuation next to a letter is exported as asterisks which are not read as emphasis. | `markdownout.scm` `mdout-spaced` |
| 2.19 | V | A single `~` is not escaped and is read as strike-through. | `markdownout.scm`, `markdownin.scm` |
| 2.20 | R | `]` in the text of a link is not escaped. | `markdownout.scm` |
| 2.21 | R | `[url](url)` gives a link inside a link. | `markdownin.scm` `md-make-link` |
| 2.22 | R | A heading which ends with `#` loses it; a line break in a heading becomes a backslash. | `markdownout.scm` |
| 2.23 | I | Nested footnotes are kept only if the arguments are evaluated from left to right. | `tmmarkdown.scm` |
| 2.24 | R | Smaller round trips: a formula followed by a digit, code with spaces at both ends, two code spans in a row, two lists of the same kind in a row, a paragraph which looks like a table, the first number of a list. | several |
| 2.25 | R | The "rule of three" of emphasis uses the remaining lengths of the runs. | `markdownin.scm` `md-match?` |

The loops rewritten on 2026-10-11 (`mdout-escape`, `md-tokens`,
`md-lines`): the reviewer found no difference of behaviour with the code
before. Its doubt about tables indexed by `char->integer` with characters
above 255 was checked afterwards: femtolisp, S7 and Guile 1.8 all give
codes below 256 and the same round trip on accented and symbol text.

Found sound: no crash on empty input, lone delimiters, unclosed fences,
CR and CRLF, NUL, invalid UTF-8, 20000 nested quotes, a line of 1 MB.

## 4. Office converters, import (`convert/office/`)

| # | | Finding | Where |
|---|---|---|---|
| 3.1 | V | A "/" in a formula becomes an opening bracket: `a/b+c` gives `a` then `(around* "/" "b+c" "<nobracket>")`. Same for linear fractions and for the formulas of LibreOffice (R). The cause is in the MathML importer. | `omml.scm`; `convert/mathml/mathtm.scm` |
| 3.2 | V | The accent dot gives `(above "x" "\n")`, a raw newline in the tree; double dot a control character, vector an arrow, ring a code (R). | `omml.scm` `omml-accents` |
| 3.3 | V | The runs inside `w:dir` and `w:bdo` (bidirectional text of Word 2010 and later) are dropped. | `docxin.scm` `docx-inline` |
| 3.4 | R | Rows or cells of a table inside a content control (`w:sdt`) are dropped. | `docxin.scm` |
| 3.5 | V | The characters of `w:sym` (arrows of the autocorrection, Symbol and Wingdings) are dropped. | `docxin.scm` `docx-run-item` |
| 3.6 | R | The title block: a second paragraph of title, subtitle or date is dropped, and a subtitle alone is dropped. | `officetm.scm` `oftm-doc-data` |
| 3.7 | V | A field which ends in a later paragraph (bibliographies of Zotero and Mendeley, tables of contents) loses the text before it in its paragraph. | `docxin.scm` |
| 3.8 | R | A footnote which refers to itself hangs the converter. | `docxin.scm` `docx-note` |
| 3.9 | R | 100,000 nested elements end TeXmacs without an error; the entities of a DOCTYPE are expanded without a limit (1 MB from a few hundred bytes). Both in the XML parser, in C++. | `parse-xml` |
| 3.10 | R | A .docx of pandoc: the second paragraph of a list item becomes a list of its own and the numbering starts again. | `docxin.scm` `docx-list-format` |
| 3.11 | R | `m:scr` is ignored: double-struck, script and fraktur letters lose their style. | `omml.scm` `omml-run` |
| 3.12 | R | .odt: bibliographies, other indexes and `text:numbered-paragraph` are dropped whole. | `odtin.scm` `odt-block` |
| 3.13 | R | .odt: bold or italic of a whole paragraph (in its automatic style) is lost. | `odtin.scm` `odt-paragraph` |
| 3.14 | R | Field instructions are split at spaces: a link with a space or with `\l` after a file, `PAGEREF` read as a reference to a number. | `docxin.scm`, `officetm.scm` |
| 3.15 | R | .odt: an embedded object which is not a formula (a chart) becomes an equation of its text. | `odtin.scm` |
| 3.16 | R | `mc:AlternateContent`: the fallback picture is lost when the choice gives nothing; a group of pictures gives the first one only. | `docxin.scm` |
| 3.17 | R | Lists with `w:numStyleLink` come out as bullets. | `docxin.scm` |
| 3.18 | V | A numeric attribute which is not a small natural number raises an error which stops the import (`text:c="-1"`), or allocates without a limit (`text:c="30000000"`, R). | `odtin.scm`, `officetm.scm` |
| 3.19 | R | No-break spaces and soft hyphens arrive as the symbols `<varspace>` and `<hyphen>`. | `officetm.scm` `oftm-text` |
| 3.20 | R | Text inside a formula loses the spaces around it ("x=1for ally"). | `omml.scm`; `mathtm.scm` |
| 3.21 | R | Tables: `w:gridBefore` is ignored, a continued vertical merge without a start loses its text, deleted rows are kept. | `docxin.scm` |
| 3.22 | R | Hidden text is imported as visible; bold and italic of right-to-left runs are ignored. | `docxin.scm` |
| 3.23 | R | Other prefixes of namespaces than the usual ones, or a file which is not an archive, give an empty document without an error. | `docxin.scm`, `odtin.scm` |
| 3.24 | R | .odt, smaller: the depth of lists leaks into cells and notes; the widths of columns inside `table:table-columns` are lost; a table in a text box is lost. | `odtin.scm` |
| 3.25 | R | The addresses of external images are kept as they are (`http://...`, `file:///...`): opening the document may fetch or read them (not tried in the editor). | `odtin.scm` |
| 3.26 | I | Guile 1.8: `quotient` and `odd?` on inexact numbers read from attributes; recursion as deep as the number of paragraphs. | several |

Found sound: no image is written to disk on import and paths with `..`
stay inside the archive in memory; cycles of styles are guarded; missing
parts and truncated XML degrade without an error; 16,000 paragraphs in
2.8 s.

## 5. Office converters, export (`convert/office/`)

| # | | Finding | Where |
|---|---|---|---|
| 4.1 | V | `binom`, `tfrac`, `dfrac`, `cfrac`, `choose`, `bmod`, `pmod` are kept unexpanded for a converter to LaTeX which is not used here, and MathML does not know them: they vanish from formulas (pandoc reads `+ +`). | `tmoffice-expand.scm` |
| 4.2 | V | A code point which XML forbids (`<#D800>`, `<#FFFE>`) makes both files unreadable for pandoc and LibreOffice. | `office-tools.scm` `ox-escape` |
| 4.3 | V | .odt: a table in a list item is lost (the .docx keeps it). | `odtout.scm` `ot-list` |
| 4.4 | R | .docx: links and images inside footnotes are dropped (no relations are written for the notes; a comment says it is intended). | `docxout.scm` |
| 4.5 | R | The names `&LeftAngleBracket;`, `&LeftFloor;`, `&LeftCeiling;` and their right ones stay as text in formulas. | `tmoffice.scm` `tmof-entities` |
| 4.6 | R | A definition by cases ends with a "." as closing delimiter; LibreOffice shows an error mark. | `omml.scm`, `odtout.scm` |
| 4.7 | R | .odt formulas which LibreOffice shows wrongly: `lim` and big operators written as words show their first letter, the hat accent, the lone "=" of aligned equations. | `odtout.scm` `ot-plain-mathml` |
| 4.8 | R | An appendix makes every section of an article a heading of level 2. | `tmoffice.scm` |
| 4.9 | R | The labels of the rows of a numbered multi-line equation are bound to the number of the equation before. | `tmoffice.scm` |
| 4.10 | R | A label with a space gives two bookmarks and a reference to neither. | `tmoffice.scm`, `office-tools.scm` |
| 4.11 | R, not confirmed | .docx: two tables in a row are shown as one table by LibreOffice. Pandoc reads two tables from the same file, so only the rendering of LibreOffice (and of Word, inferred) is concerned. | `docxout.scm` `dx-block` |
| 4.12 | R | OMML: the right scripts are lost when there are left scripts. | `omml.scm` |
| 4.13 | R | .odt: a nested list takes the kind of the first nested list of its level. | `odtout.scm` |
| 4.14 | R/I | A link with its own text to a numbered label is written as a field, which a field update replaces by the number. | `tmoffice.scm` `tmof-hlink` |
| 4.15 | V | `(tformat)` without children raises an error which stops the export. | `tmoffice.scm` |
| 4.16 | R | `tmof-paragraph` is defined twice; `paragraph` works because the dispatcher takes the value before the second definition. | `tmoffice.scm` |
| 4.17 | I | The names of sequences lose their non-ASCII letters: theorems and figures of a Cyrillic or Chinese document would share one counter. | `docxout.scm` |
| 4.18 | R | The formats of a table are quadratic: 200 rows of 10 cells with a format each take 0.7 s, 1000 rows about 16 s (extrapolated). | `tmoffice.scm` |
| 4.19 | R/I | Smaller: the table of contents is exported as an empty heading; `m:t` has no `xml:space="preserve"`; the targets of links are not percent-encoded; the same image is stored once for each use. | several |

Found sound: escaping of `&`, `<`, `>`, quotes and control characters; the
order of the children which OOXML fixes; unique ids; `mimetype` first and
stored in the .odt; empty documents, deep lists, spans, nested tables,
code with blanks; no number in exponent notation.

## 6. What the audit did not cover

- Files written by Microsoft Word and Google Docs, and what Word shows.
- Runs on Guile 1.8 and femtolisp, except the check of the Markdown
  tables.
- What a window shows (the citations of CSL, hanging indentations, the
  footnotes), the LaTeX export of CSL citations.
- The real export path through the typesetter for Markdown (numbered
  sections, theorems, figures, citations).
- Drawings and images other than PNG in the office export; headers,
  footers and section breaks in the import.
- The C++ side: the zip reader (sizes near 2 GB read from the directory),
  the XML parser beyond 3.9.
- `csl-fixtures.scm`, the database tool as a source of CSL items, and the
  CommonMark specification as a whole (about 80 cases were tried).
