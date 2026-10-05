<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Citations from Zotero>

  <TeXmacs> can take its bibliographic references directly from the library
  of the <hlink|Zotero|https://www.zotero.org> desktop application (version
  7 or later). <TeXmacs> only reads the library: it never changes anything
  in Zotero.

  <subsubsection*|Setting up Zotero>

  Zotero answers the requests of other applications once <with|font-shape|italic|Allow
  other applications on this computer to communicate with Zotero> is
  enabled, in the advanced settings of Zotero. Zotero must be running while
  you cite or update a bibliography; when it is not, <TeXmacs> says so in
  the footer, and uses the references which it obtained before.

  The citations use the citation keys of Zotero, which Zotero (or the
  Better<nbsp>BibTeX extension) stores in the field <verbatim|citationKey>.
  An item without a citation key can still be cited, as
  <verbatim|zotero:><em|key>, where <em|key> is the key of the item in
  Zotero; you may give it a better citation key in Zotero at any time.

  The settings are in <menu|Document|Bibliography|Zotero settings...>: the
  address of Zotero (<verbatim|http://localhost:23119> by default), the
  libraries to search (your own library only, or also the libraries of your
  groups), the format of the exported references (<verbatim|bibtex> or
  <verbatim|biblatex>), and whether citation keys are completed from Zotero.
  <menu|Test the connection> tells whether Zotero answers.

  <subsubsection*|Inserting citations>

  <menu|Insert|Link|Citation|From Zotero...> opens a search window. Type
  names of authors, words of the title or a year, then select one or more
  references and press <menu|Cite>. Inside a citation, the chosen keys are
  added to it.

  The window also searches the other sources of the document, and marks
  each reference with the sources which have it: <verbatim|L> for the
  entries of the document, <verbatim|F> for the <BibTeX> file of its
  bibliography, <verbatim|D> for the database and <verbatim|Z> for Zotero.
  A reference of a group library also shows the name of the group. When two
  sources use the same key for different works, the lines are marked with
  <verbatim|(!)>, and the source which comes first in the list above wins.
  <menu|Show in Zotero> selects the chosen item in Zotero.

  While typing a key in a citation, <shortcut|(kbd-tab)> completes it,
  with the keys of the bibliography and those of Zotero. When the cursor is
  on a key which comes from Zotero, <menu|Focus|Show in Zotero> shows its
  item in Zotero.

  <subsubsection*|Generating the bibliography>

  When the bibliography of the document has no <BibTeX> file yet,
  <menu|Document|Bibliography|Update from Zotero> adds a bibliography with a
  file <verbatim|<em|name>-zotero.bib>, named after the document, and fills
  it with the references of the citations. Such a file starts with the line
  <verbatim|% Exported from Zotero by TeXmacs>, and only contains the items
  which the document cites. <TeXmacs> rewrites it at each
  <menu|Document|Update|Bibliography> (or <menu|Document|Update|All>), so
  that it follows the changes made in Zotero. A <BibTeX> file without this
  line is yours, and <TeXmacs> never changes it: its references come before
  those of Zotero.

  In a project, the citations of the master document and of all the files
  which it includes go into the file of the bibliography of the master
  document.

  <subsubsection*|With the database>

  With the bibliographic database (<menu|Tools|Database tool>), Zotero is
  one more source of references. The database of the
  user comes first, then Zotero, then the references attached to the
  document, which serve when Zotero is not running. The search window of
  the database (<shortcut|(kbd-alternate-tab)> in a citation) also lists
  the matching references of Zotero.

  References from Zotero can be copied into the database, with
  <menu|Import into database> in the search window, or automatically, when
  the database imports the references of the documents. These copies stay
  in sync with Zotero: <menu|Document|Update|Bibliography> and
  <menu|Document|Bibliography|Synchronize with Zotero> bring in the changes
  made in Zotero. When a reference was changed both in <TeXmacs> and in
  Zotero, a window shows the fields which differ, and you choose which
  value to keep for each of them. A reference deleted in Zotero is kept in
  the database.

  <subsubsection*|Keys renamed in Zotero>

  Zotero, and Better<nbsp>BibTeX in particular, may change a citation key,
  for instance when the title of an item is corrected. <TeXmacs> remembers
  which Zotero item each citation stands for, so the bibliography still
  finds the item under its old key, and the footer says
  <with|font-shape|italic|smith2020 is now smith2020gravity in Zotero>.
  <menu|Document|Bibliography|Update the citations> then gives the new keys
  to the citations of the document, or of all the files of its project. The
  changes in the open documents can be undone; the files of the project
  which were not open are saved.

  <menu|Document|Bibliography|Check against Zotero...> lists how the
  citations relate to Zotero: the keys found in Zotero or elsewhere, those
  renamed in Zotero, the items no longer in Zotero, the keys found nowhere,
  and the keys used for different works in Zotero and in another source.
  With the database, it also finds the references of the database which
  are copies of Zotero items made by hand, and offers to keep them in sync
  with Zotero from then on.

  <tmdoc-copyright|2026|Massimiliano Gubinelli>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
