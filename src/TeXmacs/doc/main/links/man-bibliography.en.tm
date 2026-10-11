<TeXmacs|1.99.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Compiling a bibliography>

  <TeXmacs> uses the <BibTeX> model for its bibliographies: the references
  are entries with a key (such as <verbatim|einstein1905>), a type and
  fields, and the citations of a document are generated into a list of
  references in a chosen style. The references may come from a
  <verbatim|.bib> file, from the bibliographic database of <TeXmacs> (see
  <hlink|The bibliographic database|man-bib-database.en.tm>), or from
  <hlink|Zotero|man-zotero.en.tm>.

  <subsubsection*|Inserting citations>

  <menu|Insert|Link|Citation> inserts a citation: <menu|Visible> (shown as
  a reference to the bibliography), <menu|Invisible> (the reference only
  enters the bibliography; an invisible citation of the key <verbatim|*>
  puts all the references of the file there) or <menu|Detailed> (with a precision, such as a page).
  When the document uses the package <tmpackage|cite-author-year>, the menu
  offers the citations by authors and years instead: abbreviated or full
  author lists, textual (``Einstein (1905)'') or parenthesized, and their
  parts.

  Inside a citation, type the key of the reference. <shortcut|(kbd-tab)>
  completes it, with the keys of the <verbatim|.bib> file of the
  bibliography (or of the database, when it is used) and those of Zotero.
  <shortcut|(kbd-alternate-tab)>, <menu|Focus|Search references> or the
  search icon of the focus bar open the window <with|font-shape|italic|Search
  bibliographic reference>: type words of the authors, of the title or a
  year, and click on a reference to cite it. A line at the top of the
  window names the sources which it searches, and each reference is marked
  with its source.

  To cite <TeXmacs> itself, use <menu|Cite TeXmacs> (in the focus menu of
  the citation, of the title or of the document): these references come
  with <TeXmacs>, and need no file.

  <subsubsection*|Inserting the bibliography>

  At the place where the bibliography should be, use
  <menu|Insert|Automatic|Bibliography>:

  <\itemize>
    <item>without the database tool, a file chooser asks for the
    <verbatim|.bib> file of the references; the bibliography takes the style
    <verbatim|tm-plain>;

    <item>with the database tool (<menu|Tools|Database tool>), the
    bibliography is inserted without file: its references come from the
    database (and from Zotero);

    <item>with <menu|New bibliography dialogue> in the experimental
    features of <menu|Edit|Preferences|Other>, a dialogue asks for the file
    (with a relative or an absolute path) and the style, and shows a
    preview of the bibliography; on an existing bibliography it modifies
    it.
  </itemize>

  The style and the file are the first two arguments of the bibliography
  tag, which can be changed afterwards (the focus bar proposes the styles).
  The file is written without its extension <verbatim|.bib>, relative to
  the document; it is also searched in the directories which contain the
  document. When there is no such <verbatim|.bib> file, a <verbatim|.bbl>
  file of the same name, as produced by <LaTeX>, is used.

  Then use <menu|Document|Update|Bibliography> (or
  <menu|Document|Update|All>) to generate the bibliography; do it again
  after adding citations.

  <subsubsection*|Bibliography styles>

  The styles whose names start with <verbatim|tm-> are implemented by
  <TeXmacs> itself, and need no other program: <verbatim|tm-plain>,
  <verbatim|tm-abbrv>, <verbatim|tm-alpha>, <verbatim|tm-unsrt>,
  <verbatim|tm-acm>, <verbatim|tm-ieeetr>, <verbatim|tm-siam>,
  <verbatim|tm-elsart-num> and <verbatim|tm-abstract> (which also shows
  the abstracts). Any other name is a style of <BibTeX> (a
  <verbatim|.bst> file), and the bibliography is then made by the
  <verbatim|bibtex> program, which can be chosen in
  <menu|Edit|Preferences|Convert|BibTeX>. When <verbatim|bibtex> is not
  installed, the standard styles <verbatim|plain>, <verbatim|alpha>,
  <verbatim|abbrv>... are replaced by their <verbatim|tm-> versions, and
  the other styles by <verbatim|tm-plain>.

  Additional <BibTeX> styles may be put in the directory
  <verbatim|~/.TeXmacs/system/bib>; a <verbatim|.bst> file next to the
  document is copied there when it is used. When a style of <BibTeX>
  produces citations by authors and years (<verbatim|natbib>), the package
  <tmpackage|cite-author-year> is added to the document.

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
  <markup|cite-year> for the year alone. With a style which cites in
  footnotes, a citation inside a footnote of yours stays in that footnote.

  The terms (``and'', ``edited by'', the names of the months) are in the
  language of the style when it has one, and else in the language of the
  document.

  <subsubsection*|Editing files with bibliographic entries>

  <BibTeX> files can either be entered and edited using <TeXmacs> itself or
  using an external tool. Some external tools offer possibilities to search
  and retrieve bibliographic entries on the web, which can be a reason to
  prefer such tools from time to time. <TeXmacs> implements good converters
  for <BibTeX> files, so several editors can easily be used in conjunction.

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
    name>). For instance, ``Albert Einstein'' should be entered as ``Albert
    <name|Einstein>'' or as ``A. <name|Einstein>''. Typing
    <shortcut|(kbd-return)> after ``Albert Einstein'' does it for you. The
    names are separated by typing a comma or ``<verbatim| and >'' (or with
    <menu|Insert|Extra name>). Special particles such as ``von'' can be
    entered using <menu|Insert|Particle> or <key|S-F5>. Title suffixes such
    as ``Jr.'' can be entered similarly using <menu|Insert|Title suffix> or
    <key|S-F7>.

    <item>When entering titles, do not capitalize, except for the first
    character and names or concepts that always must be. For instance, use
    ``Riemannian geometry'' instead of ``Riemannian Geometry'' and
    ``Differential Galois theory'' instead of ``Differential Galois Theory''.
  </itemize>

  <tmdoc-copyright|2015--2026|Joris van der Hoeven, Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>
