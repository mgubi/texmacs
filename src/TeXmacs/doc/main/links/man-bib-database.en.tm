<TeXmacs|2.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The bibliographic database>

  Instead of keeping your references in <verbatim|.bib> files, you may keep
  them in a database of <TeXmacs>, common to all your documents. Enable it
  with <menu|Tools|Database tool>: a menu <menu|Data> then appears, and
  <menu|View|Database toolbar> shows its icons.

  <subsubsection*|Editing the database>

  <menu|Data|Open bibliography> opens your bibliographic database, which is
  edited like a <verbatim|.bib> file (see <hlink|Compiling a
  bibliography|man-bibliography.en.tm>): <menu|Data|New entry> adds an
  entry, and <menu|Data|Confirm entry> (or <shortcut|(kbd-alternate-return)>)
  stores the changes of the entry at the cursor; <menu|Data|Remove entry>
  removes it. The database keeps the former versions of its entries.

  References enter the database in several ways:

  <\itemize>
    <item><menu|Data|Import> reads a <verbatim|.bib> file into the database;

    <item>in a <verbatim|.bib> file or a document, <menu|Data|Import
    selected entries>, <menu|Data|Import entries in buffer> and
    <menu|Data|Import entry> (at the cursor) copy entries into it;

    <item>when you open a document whose bibliography was made with the
    database, its references are imported (unless
    <with|font-shape|italic|Automatically import bibliographies when opening
    files> is turned off in <menu|Data|Preferences>);

    <item>the references of Zotero may be imported too, and then follow the
    changes made in Zotero (see <hlink|Citations from
    Zotero|man-zotero.en.tm>).
  </itemize>

  <menu|Data|Export> writes entries of the database into a <verbatim|.bib>
  file. <menu|Data|Storage> chooses the file which holds the database.

  <subsubsection*|Bibliographies made with the database>

  With the database tool, <menu|Insert|Automatic|Bibliography> inserts a
  bibliography without file. <menu|Document|Update|Bibliography> looks for
  the reference of each citation, in this order:

  <\enumerate>
    <item>the entries of the document itself (see below);

    <item>the <verbatim|.bib> file of the bibliography, when it has one;

    <item>your database;

    <item>the files exported from Zotero, then Zotero itself;

    <item>the references kept in the document.
  </enumerate>

  The completion of keys (<shortcut|(kbd-tab)>) and the search window of
  references (<shortcut|(kbd-alternate-tab)>) use the database too.

  The references of the bibliography are kept in the document: a document
  sent to someone else, without your database, still has its
  bibliography. <menu|Document|Bibliography|Local entries> opens them, so
  that they can be changed for this document only (the changes are kept
  with the document, and take precedence). <menu|Data|Export entries in
  buffer> writes the references of a document into a <verbatim|.bib> file.

  With a style of <BibTeX> (whose name does not start with
  <verbatim|tm->), the references found are gathered into one file and
  given to the <verbatim|bibtex> program.

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
