<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The style engine and the shipped styles>

  <section|The engine>

  An internal style <verbatim|tm-<em|name>> is the <scheme> module
  <verbatim|(bibtex <em|name>)> in <verbatim|bibtex/<em|name>.scm>.
  <cpp|generate_bibliography> loads it by evaluating <scm|(use-modules
  (bibtex <em|name>))> and calls <scm|(bib-process <scm-arg|prefix>
  <scm-arg|name> <scm-arg|entries>)>; with the database tool,
  <scm|bib-compile> does the same. The flow inside <scm|bib-process>
  (<source-link|bibtex/bib-utils.scm|TeXmacs/progs/bibtex/bib-utils.scm>) is described in <hlink|the database
  chapter|database-bibliography.en.tm>: set the globals
  <scm|bib-current-prefix> and <scm|bib-style>, run
  <scm|bib-preprocessing>, sort with <scm|bib-sorted-entries>, format each
  entry with <scm|bib-format-entry> and return a simplified
  <markup|bib-list>.

  <subsection|Style dispatch>

  <scm|(bib-define-style <scm-arg|name> <scm-arg|fallback>)> is a macro
  which declares the <scheme> mode <scm|bib-<scm-arg|name>%>, with
  predicate <scm|bib-<scm-arg|name>?>, defined by <scm|(bib-mode?
  <scm-arg|name>)> and, if <scm-arg|fallback> differs from
  <scm-arg|name>, implying the mode of the fallback. <scm|bib-mode?> is
  true when its argument is the current <scm|bib-style> <em|or> the global
  <scm|bib-default-style>. The macro also sets <scm|bib-default-style> to
  <scm-arg|fallback>, as a side effect of loading the module.

  A style file then redefines functions of <source-link|plain.scm|TeXmacs/progs/bibtex/plain.scm> with a
  <scm|(:mode bib-<scm-arg|name>?)> clause. Since <scm|bib-style> is set
  by <scm|bib-process>, several style modules can be loaded at the same
  time: each override only applies while its style is active, and the most
  specific mode wins as for all <scheme> overloading (see <hlink|editing
  modes on the Scheme side|modes.en.tm>). The functions of
  <source-link|plain.scm|TeXmacs/progs/bibtex/plain.scm> themselves are defined without a mode, so they are
  the defaults of every style.

  <scm|(bib-with-style <scm-arg|style> <scm-arg|f> <scm-arg|args>
  ...)> calls a function with another style temporarily active; the
  <verbatim|alpha> style uses it to append the <verbatim|plain> sort key to
  its own.

  <subsection|The protocol of a style>

  The functions which a style may override, all defined in
  <source-link|plain.scm|TeXmacs/progs/bibtex/plain.scm>, are:

  <\description>
    <item*|<scm|(bib-preprocessing <scm-arg|entries>)>>Called once with
    the list of entries before sorting; the default does nothing.

    <item*|<scm|(bib-sort-key <scm-arg|entry>)>>A string used by the
    stable sort of <scm|bib-sorted-entries>, compared with
    <scm|tmstring-before?>. The default is the upper-cased names (authors,
    or editors for books and proceedings, or the key if there are none),
    the year and the purified title. <scm|bib-sorted-entries> itself can be
    overridden to change the order completely (<verbatim|unsrt>,
    <verbatim|ieeetr> and <verbatim|elsart-num> keep the citation order).

    <item*|<scm|(bib-format-entry <scm-arg|n> <scm-arg|entry>)>>Dispatches
    on the entry type to <scm|bib-format-article>,
    <scm|bib-format-book>, ..., <scm|bib-format-unpublished>; the type
    <verbatim|conference> is an alias of <verbatim|inproceedings>, and
    unknown types are formatted by <scm|bib-format-misc>. Each of these
    returns a <verbatim|(concat (bibitem* <em|label>) (label ...) ...)>
    paragraph.

    <item*|<scm|(bib-format-bibitem <scm-arg|n> <scm-arg|entry>)>>The
    label: the running number <scm-arg|n> by default, the key for
    <verbatim|abstract>, the computed label for <verbatim|alpha>.

    <item*|Name formatting><scm|bib-format-names>,
    <scm|bib-format-name>, <scm|bib-format-first-name>,
    <scm|bib-last-name-sep>, <scm|bib-format-author>,
    <scm|bib-format-editor>.

    <item*|Parts of entries><scm|bib-format-date>,
    <scm|bib-format-pages>, <scm|bib-format-chapter-pages>,
    <scm|bib-format-vol-num-pages>, <scm|bib-format-bvolume>,
    <scm|bib-format-number-series>, <scm|bib-format-tr-number>,
    <scm|bib-format-in-ed-booktitle>.
  </description>

  The output helpers of <source-link|bib-utils.scm|TeXmacs/progs/bibtex/bib-utils.scm> (<scm|bib-new-block>,
  <scm|bib-new-sentence>, <scm|bib-new-list>, <scm|bib-format-field> and
  its variants, <scm|bib-emphasize>, <scm|bib-translate>) are documented in
  <hlink|writing <TeXmacs> bibliography
  styles|../scheme/bibliography/bibliography.en.tm>. Two details matter
  when writing styles: <scm|bib-null?> treats <verbatim|"">, the empty
  list, the empty symbol and a <markup|with> around any of these as
  empty, and the helpers silently drop empty pieces; and
  <scm|bib-translate> produces a <markup|localize> tag, so words such as
  \Pin\Q and \Pedited by\Q follow the language of the document when it is
  typeset, not when the bibliography is generated.

  <section|The shipped styles>

  All shipped styles use <verbatim|plain> as their fallback.

  <\description>
    <item*|<verbatim|plain>>The defaults, modelled on
    <verbatim|plain.bst>: numbered labels, entries sorted by names, year
    and title.

    <item*|<verbatim|abbrv>>Only redefines <scm|bib-format-first-name>, to
    abbreviate first names with <scm|bib-abbreviate>.

    <item*|<verbatim|unsrt>>Only redefines <scm|bib-sorted-entries>, as
    the identity: entries appear in the order of the citations (more
    precisely, in the order returned by <cpp|bib_entries> or the database,
    which is the citation order followed by cross-referenced entries).

    <item*|<verbatim|abstract>>Only redefines <scm|bib-format-bibitem>:
    the label is the citation key.

    <item*|<verbatim|alpha>>Labels made of name prefixes and the year, as
    in <verbatim|alpha.bst>, see below.

    <item*|<verbatim|acm>, <verbatim|siam>, <verbatim|ieeetr>,
    <verbatim|elsart-num>>Larger styles which redefine most of the
    formatting functions to follow the corresponding <verbatim|.bst>
    files. <verbatim|ieeetr> and <verbatim|elsart-num> also keep the
    citation order, and <verbatim|ieeetr> replaces the helper
    <scm|new-list-rec> of <source-link|bib-utils.scm|TeXmacs/progs/bibtex/bib-utils.scm> (the comment there says
    so) to change the separators.
  </description>

  The list returned by <scm|bib-standard-styles> (<verbatim|tm-plain>,
  <verbatim|tm-abbrv>, <verbatim|tm-abstract>, <verbatim|tm-acm>,
  <verbatim|tm-alpha>, <verbatim|tm-elsart-num>, <verbatim|tm-ieeetr>,
  <verbatim|tm-siam>, <verbatim|tm-unsrt>) must be kept in sync with these
  files, see the how-to in <hlink|the database
  chapter|database-bibliography.en.tm>. When <verbatim|bibtex> is not
  installed, <cpp|generate_bibliography> maps the standard <BibTeX> style
  names to these styles, and any other name to <verbatim|tm-plain>.

  <subsection|Labels of the <verbatim|alpha> style>

  <scm|bib-preprocessing> computes the label of every entry before
  sorting, with <scm|bib-format-label-prefix>:

  <\itemize>
    <item>The names are those of the authors, or of the editors for
    proceedings, or (for books) of the authors or else the editors. With no
    names, the first three characters of the key are used, or the entry
    number.

    <item>With one name, the label starts with the abbreviated von part
    followed by the first character of the last name, or, without von
    part, by the first three characters of the last name.

    <item>With two to four names, it is the concatenation of the von
    initials and the first character of each last name; with five or more,
    of the first three names followed by <verbatim|+>.

    <item>The last two characters of the year are appended.
  </itemize>

  Labels which occur several times get the suffixes <verbatim|a>,
  <verbatim|b>, ... in the order in which the entries are formatted, that
  is, after sorting. The sort key is the upper-cased name part of the
  label (or the label), the year, and the <verbatim|plain> sort key.

  <section|User interface>

  <subsection|Inserting a bibliography>

  <scm|open-bibliography-inserter> (<source-link|bibtex/bib-widgets.scm|TeXmacs/progs/bibtex/bib-widgets.scm>)
  opens a dialog which inserts a new <markup|bibliography> tag, or modifies
  the first one of the document (style and file). The file may be stored
  relative to the document or as an absolute path. The dialog shows a
  preview: it loads the style module, parses the whole <verbatim|.bib>
  file with <scm|parse-bib> and formats <em|all> its entries with
  <scm|bib-process> inside a <scm|texmacs-output> widget. After
  validation, the bibliography is regenerated with <scm|(update-document
  "bibliography")>.

  <subsection|Completion of citation keys>

  <source-link|bibtex/bib-complete.scm|TeXmacs/progs/bibtex/bib-complete.scm> provides the completions offered when
  typing the key of a citation. <scm|current-bib-file> finds the first
  <markup|bibliography> tag of the buffer and resolves its file name
  relative to the buffer (the result is cached per buffer);
  <scm|citekey-list> parses that file (again only when its modification
  time has changed), stores the keys in a prefix tree
  (<source-link|utils/library/ptrees.scm|TeXmacs/progs/utils/library/ptrees.scm>) and returns the keys which start
  with the typed text. <scm|citekey-completions> formats them for the
  completion mechanism.

  <tmdoc-copyright|2026|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>
