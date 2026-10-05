<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Citations from Zotero>

  <TeXmacs> can take its bibliographic references directly from your
  <hlink|Zotero|https://www.zotero.org> library: from the Zotero desktop
  application (version 7 or later), or from <verbatim|zotero.org>, where
  Zotero keeps a copy of the library when it is synchronized. <TeXmacs>
  only reads the library: it never changes anything in Zotero.

  <subsubsection*|Setting up Zotero>

  <with|font-series|bold|With the Zotero application.> Zotero answers the
  requests of other applications once <with|font-shape|italic|Allow other
  applications on this computer to communicate with Zotero> is enabled, in
  the advanced settings of Zotero. Zotero must be running while you cite or
  update a bibliography; when it is not, <TeXmacs> says so in the footer,
  and uses the references which it obtained before.

  <with|font-series|bold|With zotero.org.> When your library is
  synchronized with <verbatim|zotero.org>, <TeXmacs> can read it there,
  without the application: create a key on
  <hlink|zotero.org/settings/keys|https://www.zotero.org/settings/keys>
  (reading access to your library, and to your groups if you want them, is
  enough), and give it in the settings, with <menu|Read the library
  from|zotero.org>. The key is kept in your wallet when it is open, and
  otherwise in your preferences. When <TeXmacs> needs the key and has none
  (when you open the search window of references, use a command of Zotero,
  or update a bibliography which needs Zotero), it opens your wallet if it
  is closed, since the key may be there, and otherwise asks you for it,
  with a button which opens the page of <verbatim|zotero.org> where keys
  are made. In <TeXmacs> in a web browser, the library
  is always read from <verbatim|zotero.org>: Zotero refuses the requests of
  web pages, also when the application runs on the same computer. There,
  the answers of <verbatim|zotero.org> come in the background: the search
  window, the completion of keys and the bibliography are completed when
  they arrive. Meanwhile, the footer says what is asked, such as
  <with|font-shape|italic|Asking zotero.org: searching ``gauss''...>, with
  the seconds spent after two seconds; in the search window, its line of
  sources says it, and <with|font-shape|italic|Searching zotero.org...>
  follows the results. Once
  all is answered, the footer says how long it took, or why it failed (the
  key refused, too many requests, <verbatim|zotero.org> not reached).

  The citations use the citation keys of Zotero, which Zotero (or the
  Better<nbsp>BibTeX extension) stores in the field <verbatim|citationKey>.
  An item without a citation key can still be cited, as
  <verbatim|zotero:><em|key>, where <em|key> is the key of the item in
  Zotero; you may give it a better citation key in Zotero at any time.

  Formulas written in LaTeX in the fields of Zotero, such as a title
  <verbatim|On the $\\Phi^4_3$ model>, become formulas in the
  bibliography. The references which <TeXmacs> obtains from Zotero do not
  include the paths of the files attached to the items on your computer.

  The settings are in <menu|Document|Bibliography|Zotero settings...>:

  <\description>
    <item*|Read the library from>The Zotero application,
    <verbatim|zotero.org>, or <verbatim|Automatic>: <verbatim|zotero.org>
    in a web browser, the application elsewhere.

    <item*|API key of zotero.org>The key with which <TeXmacs> reads your
    library on <verbatim|zotero.org>.

    <item*|Zotero server>The address of the Zotero application,
    <verbatim|http://localhost:23119> by default.

    <item*|Libraries>Your own library only (<verbatim|My Library>), or also
    the libraries of the groups which you belong to. When two libraries
    have the same key, your own library wins.

    <item*|Export format>The format of the references which <TeXmacs>
    obtains from Zotero: <verbatim|bibtex> or <verbatim|biblatex>.

    <item*|Complete keys from Zotero>Whether <shortcut|(kbd-tab)> also
    completes the keys of Zotero.

    <item*|Search Zotero in the search of references>Whether the search
    window of citations also lists the references of Zotero.

    <item*|Add the references of Zotero to the <BibTeX> file>Whether the
    references of Zotero which your own <BibTeX> file lacks are added to
    it (see below).
  </description>

  <menu|Test the connection> tells whether Zotero answers.

  <subsubsection*|Inserting citations>

  Insert a citation as usual, with <menu|Insert|Link|Citation>, and type
  its key. <shortcut|(kbd-tab)> completes the key, with the keys of the
  bibliography and those of Zotero.

  <shortcut|(kbd-alternate-tab)> in a citation, or <menu|Focus|Search
  references>, opens the search window of references. Type names of
  authors, words of the title or a year, and click on a reference to cite
  it. A line at the top of the window names the sources which it searches,
  and says why Zotero is left out when it is (not running, or left out in
  the settings). The window lists:

  <\itemize>
    <item>without the database, the references of the <BibTeX> file of the
    bibliography, marked with the name of the file, then those of Zotero;

    <item>with the database (see below), the references of your database,
    marked <verbatim|Database>, or <verbatim|Database, from Zotero> for
    those which come from Zotero and follow it, then those of Zotero which
    the database does not have.
  </itemize>

  The references of Zotero are marked <verbatim|Zotero>, or
  <verbatim|Zotero, <em|group>> for those of a group library.

  When you cite a reference of Zotero from this window, <TeXmacs> copies
  it at once where the bibliography reads it: into your database with the
  database (where it then follows Zotero), and otherwise into the
  <BibTeX> file of the bibliography, as <menu|Document|Update|Bibliography>
  would (see below). The document also remembers the Zotero item of the
  citation, so that the reference is asked of Zotero by this item later,
  rather than searched by its key (<verbatim|zotero.org> does not search
  the citation keys: a key typed by hand is found there by the name of the
  author and the year at its start, as in the keys of Better<nbsp>BibTeX).

  When the cursor is on a key which <TeXmacs> has already found in Zotero,
  <menu|Focus|Show in Zotero> (or the button <verbatim|Z> of the focus
  bar) selects its item in Zotero, or opens its page on
  <verbatim|zotero.org> when the library is read there.

  <subsubsection*|Generating the bibliography>

  When the bibliography of the document has no <BibTeX> file yet,
  <menu|Document|Bibliography|Update from Zotero> adds a bibliography with a
  file <verbatim|<em|name>-zotero.bib>, named after the document, and fills
  it with the references of the citations. <menu|Document|Update|All> does
  the same for a bibliography without file (as inserted by
  <menu|Insert|Automatic|Bibliography>), when Zotero has references which
  the document cites. Such a file starts with the line
  <verbatim|% Exported from Zotero by TeXmacs>, and only contains the items
  which the document cites. <TeXmacs> rewrites it at each
  <menu|Document|Update|Bibliography> (or <menu|Document|Update|All>), so
  that it follows the changes made in Zotero.

  A <BibTeX> file without this line is yours. Its references come first,
  and <TeXmacs> never changes them. When the document cites a reference of
  Zotero which the file does not have, <menu|Document|Update|Bibliography>
  (and <menu|Document|Bibliography|Update from Zotero>) adds it at the end
  of the file, after a comment line which says where it comes from:

  <\verbatim-code>
    % Added from Zotero by TeXmacs on 2026-10-05:
    zotero://select/library/items/T8WH75PX

    @article{aruCharacterisationContinuumGaussian2022,

    \ \ title = {A characterisation of the continuum ...},

    \ \ ...

    }
  </verbatim-code>

  The link of the comment shows the item in
  Zotero, and lets <TeXmacs> find the item again when Zotero changes its
  key. The reference is added once: a later change in Zotero does not
  change the file, so that you may edit it. To keep your file as it is,
  turn off <menu|Add the references of Zotero to the BibTeX file> in the
  settings.

  In a project, the citations of the master document and of all the files
  which it includes go into the file of the bibliography of the master
  document.

  <subsubsection*|With the database>

  With the bibliographic database (<menu|Tools|Database tool>), Zotero is
  one more source of references, and the bibliography needs no <BibTeX>
  file: <menu|Document|Bibliography|Update from Zotero> adds a bibliography
  without file when the document has none, and generates it. The
  references of the bibliography are then kept in the document itself. The
  database of the user comes first, then Zotero, then the references kept
  in the document, which serve when Zotero is not running.

  References from Zotero are copied into your database when you open a
  document whose bibliography uses them, as the other references of
  documents are (the database preference <with|font-shape|italic|Automatically
  import bibliographies when opening files>, on by default). These copies
  stay
  in sync with Zotero: <menu|Document|Update|Bibliography> and
  <menu|Document|Bibliography|Synchronize with Zotero> bring in the changes
  made in Zotero. When a reference was changed both in <TeXmacs> and in
  Zotero, a window shows the fields which differ, and you choose which
  value to keep for each of them. A reference deleted in Zotero is kept in
  the database.

  <menu|Document|Bibliography|Import the citations into the database>
  copies at once the references of Zotero which the document cites, and
  <menu|Focus|Import into the database> the one of the citation at the
  cursor.

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
  and the keys used for different works in Zotero and in another source
  (two references are the same work when they have the same <abbr|DOI>, or,
  when one of them has none, the same title and year).
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
