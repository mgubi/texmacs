<TeXmacs|1.99.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Compiling a bibliography>

  <subsubsection*|Editing files with bibliographic entries>

  <TeXmacs> uses the <BibTeX> model for its bibliographies. Manuals about
  <BibTeX> can easily be found at various places on the web. <BibTeX> files
  can either be entered and edited using <TeXmacs> itself or using an
  external tool. Some external tools offer possibilities to search and
  retrieve bibliographic entries on the web, which can be a reason to prefer
  such tools from time to time. <TeXmacs> implements good converters for
  <BibTeX> files, so several editors can easily be used in conjunction.

  The built-in editor for <BibTeX> files is automatically used for files with
  the <verbatim|.bib> extension. New items can easily be added using
  <menu|Insert|Database entry>. When creating a new entry, required fields
  appear in dark blue, alternative fields in dark green and optional fields
  in light blue. The special field inside the header of your entry is the
  name of your entry, which will be used later for references to this entry.
  When editing a field, you may use <shortcut|(kbd-return)> to confirm it and
  jump to the next one (blank optional fields will automatically be removed
  when doing this). When the cursor is inside a bibliographic entry,
  additional fields may also be added using <menu|Focus|Insert above> and
  <menu|Focus|Insert below>.

  <BibTeX> contains a few unnatural conventions for entering names of authors
  and managing capitalization inside titles. When editing <BibTeX> files
  using <TeXmacs>, these conventions are replaced by the following more user
  friendly conventions:

  <\itemize>
    <item>When entering authors (inside ``Author'' or ``Editor'' fields), use
    the <markup|name> tag for specifying last names (using <menu|Insert|Last
    name> or <shortcut|(make 'name)>) For instance, ``Albert Einstein''
    should be entered as ``Albert <name|Einstein>'' or as ``A.
    <name|Einstein>''. Special particles such as ``von'' can be entered using
    <menu|Insert|Particle>. Title suffices such as ``Jr.'' can be entered
    similarly using <menu|Insert|Title suffix>.

    <item>When entering titles, do not capitalize, except for the first
    character and names or concepts that always must be. For instance, use
    ``Riemannian geometry'' instead of ``Riemannian Geometry'' and
    ``Differential Galois theory'' instead of ``Differential Galois Theory''.
  </itemize>

  <subsubsection*|Inserting citations and compiling bibliographies>

  Assuming that you have created a <verbatim|.bib> file with your
  bibliographic references, the mechanism to automatically compile a
  bibliography is the following:

  <\itemize>
    <item>Use <menu|Insert|Link|Citation> and <menu|Insert|Link|Invisible
    citation> to insert citations, which correspond to entries in your
    <verbatim|.bib> file.

    <item>At the place where your bibliography should be compiled, click on
    <menu|Insert|Automatic|Bibliography>. At the prompt, you should enter a
    <verbatim|bibtex> style (such as <verbatim|plain>, <verbatim|alpha>,
    <verbatim|abbrv>, etc.) and your <verbatim|.bib> file.

    <item>Use <menu|Document|Update|Bibliography> in order to compile your
    bibliography.
  </itemize>

  Notice that additional BiB<TeX> styles should be put in the directory
  <verbatim|~/.TeXmacs/system/bib>.

  <subsubsection*|Styles of the Citation Style Language>

  A style whose name starts with <verbatim|csl-> is a style of the Citation
  Style Language (CSL), the format of the styles of Zotero, Mendeley and
  <name|Pandoc>: <verbatim|csl-apa> is formatted by the file
  <verbatim|apa.csl>. <TeXmacs> processes these styles itself, from the same
  references as the other styles. A few styles come with <TeXmacs>, among
  which <verbatim|csl-apa>, <verbatim|csl-ieee>, <verbatim|csl-nature>,
  <verbatim|csl-chicago-author-date>,
  <verbatim|csl-chicago-notes-bibliography>,
  <verbatim|csl-modern-language-association> and
  <verbatim|csl-american-mathematical-society-label>; they are listed in the
  dialogue of <menu|Insert|Automatic|Bibliography>. Several thousands of
  other styles can be found at
  <hlink|https://www.zotero.org/styles|https://www.zotero.org/styles>: put
  the <verbatim|.csl> file next to your document or in the directory
  <verbatim|~/.TeXmacs/csl/styles>, and use its name without the suffix
  after <verbatim|csl->.

  With such a style, the citations are also formatted by the style: as
  numbers, as authors and years, or as footnotes. Since a citation may
  depend on the other ones (two works of one author in the same year get the
  years 2001a and 2001b, the citations of one author are grouped), the
  citations are made together with the bibliography: after adding
  citations, update the document again. A citation which is not known yet
  shows its key or a provisional text. Besides <markup|cite> and
  <markup|cite-detail>, whose second argument may be a locator such as
  <verbatim|p. 12> or <verbatim|chap. 3>, the following tags are understood:
  <markup|cite-textual> for a citation which is part of the sentence, as in
  ``Knuth (1984) shows'', <markup|cite-author> for the authors alone and
  <markup|cite-year> for the year alone. With the package
  <tmpackage|cite-author-year>, <markup|cite*> and <markup|cite-textual*>
  show all the authors, without ``et al.''. With a style which cites in
  footnotes, a citation inside a footnote of yours stays in that footnote.

  The terms (``and'', ``edited by'', the names of the months) are in the
  language of the style when it has one, and else in the language of the
  document.

  A document with such a style is exported to <LaTeX> for the package
  <verbatim|citation-style-language>, which formats the citations and the
  bibliography with the same style when <LaTeX> runs: its citations stay
  <verbatim|\\cite>, <verbatim|\\textcite> and <verbatim|\\citeauthor>
  commands, and the bibliography becomes <verbatim|\\printbibliography>.
  Next to the <verbatim|.tex> file, the export writes the cited entries, in
  a file <verbatim|<em|name>-refs.bib>, and a copy of the style. Such a
  file is compiled with <verbatim|lualatex>, twice; with another engine, the
  program <verbatim|citeproc-lua> has to be run between the two compilations,
  as <BibTeX> would be. The package is part of <TeX> Live.

  <tmdoc-copyright|2015|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>