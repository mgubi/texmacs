<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Source tracking and conservative conversion>

  <section|Purpose>

  A user who collaborates with <LaTeX> users may want to import a <LaTeX>
  file, edit it in <TeXmacs> and export it again, or export a <TeXmacs>
  document, let a colleague edit the <LaTeX> file and import it back. With
  the plain converters, each conversion rewrites the whole document: the
  formatting of the source, the macros of the author and many small details
  are lost. <TeXmacs> therefore implements two complementary mechanisms,
  written in <c++> in <verbatim|Data/Convert/Tex>:

  <\description>
    <item*|Source tracking>During a conversion, <em|markers> are inserted
    into the source document. They survive the conversion and indicate
    which part of the result comes from which part of the source. The
    correspondence is stored together with the result.

    <item*|Conservative conversion>When a document which was obtained by a
    tracked conversion is converted back, the parts which were not modified
    are replaced by the corresponding parts of the original source, and
    only the modified parts are really converted.
  </description>

  Both mechanisms are controlled by preferences (menu: preferences dialog,
  tab <menu|Convert>, <menu|LaTeX>, section \PConservative conversion
  options\Q):

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|font-series|bold>|<table|<row|<cell|Preference>|<cell|Default>>|<row|<cell|<verbatim|latex-\<gtr\>texmacs:source-tracking>>|<cell|<verbatim|off>>>|<row|<cell|<verbatim|latex-\<gtr\>texmacs:conservative>>|<cell|<verbatim|off>>>|<row|<cell|<verbatim|latex-\<gtr\>texmacs:transparent-source-tracking>>|<cell|<verbatim|off>>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:source-tracking>>|<cell|<verbatim|off>>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:conservative>>|<cell|<verbatim|on>>>|<row|<cell|<verbatim|texmacs-\<gtr\>latex:transparent-source-tracking>>|<cell|<verbatim|on>>>>>>>
    Preferences for source tracking (defaults from <verbatim|init-latex.scm>).
  </big-table>

  The dialog sets the import and export variants of each preference
  together. The <c++> code reads them with <cpp|get_preference>.

  <section|Tracked import>

  <\explain>
    <cpp|tree tracked_latex_to_texmacs (string s, bool as_pic)><explain-synopsis|import
    with source tracking>
  <|explain>
    Defined in <verbatim|tracked_fromtex.cpp>. If
    <verbatim|"latex-\<gtr\>texmacs:source-tracking"> is off, this is just
    <cpp|latex_document_to_tree>. Otherwise:

    <\enumerate>
      <item><cpp|latex_mark> inserts the markers
      <verbatim|{\\blx{<em|i>}}> and <verbatim|{\\elx{<em|i>}}> around the
      paragraphs and environments of the source, where <em|i> is a
      character position in the source. Regions in which markers may not be
      inserted (verbatim environments, ...) are protected beforehand by
      <cpp|latex_protect>.

      <item>The marked source is imported with <cpp|latex_document_to_tree>.
      Since <verbatim|\\blx> and <verbatim|\\elx> are unknown commands,
      they become tags <markup|blx> and <markup|elx> with one argument.

      <item><cpp|texmacs_group_markers> transforms each concatenation which
      starts with <markup|blx> and ends with <markup|elx> into a tag
      <verbatim|(mlx "<em|b>:<em|e>" <em|body>)>, which records that
      <em|body> comes from the characters <em|b> to <em|e> of the source;
      <cpp|texmacs_correct_markers> and <cpp|texmacs_clean_markers> remove
      inconsistent and remaining markers.

      <item>If <verbatim|"latex-\<gtr\>texmacs:transparent-source-tracking">
      is on, the document is also imported without markers, and the
      results are compared (<cpp|texmacs_unmark>). Markers which changed
      the result are declared invalid (<cpp|texmacs_declare_transparent>,
      <cpp|texmacs_check_transparency>) and the process is repeated without
      them.

      <item>The result is the unmarked document, with two attachments: the
      original source (<verbatim|latex-source>) and the marked document
      (<verbatim|latex-target>).
    </enumerate>
  </explain>

  The attachments are saved with the <TeXmacs> document, so that the
  correspondence remains available in later sessions.

  <section|Conservative export>

  <\explain>
    <cpp|string conservative_texmacs_to_latex (tree doc, object
    opts)><explain-synopsis|export reusing the original source>
  <|explain>
    Defined in <verbatim|conservative_totex.cpp>. If
    <verbatim|"texmacs-\<gtr\>latex:conservative"> is off, or if the
    document has no <verbatim|latex-source> attachment, the function simply
    calls <cpp|tracked_texmacs_to_latex>. Otherwise:

    <\enumerate>
      <item>If the document is identical to the unmarked
      <verbatim|latex-target>, the original source is returned unchanged.

      <item><cpp|texmacs_invarianted> looks for subtrees of the document
      which also occur in the old target inside an <markup|mlx> marker,
      choosing the best match when a subtree occurs several times
      (<cpp|texmacs_best_match>, which uses the neighbors of the subtree).
      These subtrees are replaced by <verbatim|(ilx "<em|b>:<em|e>")>,
      adjacent invariant paragraphs are merged
      (<cpp|texmacs_invarianted_merge>), and finally
      <cpp|texmacs_invarianted_replace> substitutes the corresponding
      source text: <verbatim|(!ilx <em|source>)>.

      <item>The resulting document is exported normally; the <LaTeX>
      converter turns <markup|!ilx> into <scm|(!invariant <scm-arg|source>)>
      (<scm|tmtex-ilx>), which <scm|texout> writes verbatim. During this
      export, <scm|latex-set-virtual-packages> declares the packages used by
      the original source, so that the converter does not add them again.

      <item>The metadata, the abstract and the preamble are merged with
      those of the original source (<cpp|latex_merge_metadata>,
      <cpp|latex_merge_abstract>, <cpp|latex_merge_preamble>), so that the
      document class, the packages and the declarations of the user are
      preserved.
    </enumerate>
  </explain>

  <section|Tracked export>

  <\explain>
    <cpp|string tracked_texmacs_to_latex (tree doc, object
    opts)><explain-synopsis|export with source tracking>
  <|explain>
    Defined in <verbatim|tracked_totex.cpp>. After the macro expansion by
    <cpp|latex_expand>, the document is exported by
    <cpp|tree_to_latex_document> if
    <verbatim|"texmacs-\<gtr\>latex:source-tracking"> is off. Otherwise
    <cpp|tracked_tree_to_latex_document> is called:

    <\enumerate>
      <item><cpp|texmacs_mark> wraps each paragraph of the body into
      <verbatim|(mtm "<em|path>" <em|paragraph>)>, where the path is encoded
      as a comma-separated list of integers (<cpp|encode_as_string>).

      <item>The <LaTeX> converter translates <markup|mtm> into the markers
      <verbatim|{\\btm{<em|path>}}> and <verbatim|{\\etm{<em|path>}}>
      (<scm|tmtex-mtm> and <scm|texout-marker>).

      <item><cpp|latex_unmark> removes the markers from the generated
      <LaTeX> and records the positions at which they occurred.

      <item>If <verbatim|"texmacs-\<gtr\>latex:transparent-source-tracking">
      is on, the unmarked result is compared with an export without
      markers; markers which change the output are invalidated
      (<cpp|latex_declare_transparent>, <cpp|latex_check_transparency>)
      and the export is repeated.
    </enumerate>

    <cpp|tracked_tree_to_latex_document> returns <cpp|false> on success.
    In this case, <cpp|tracked_texmacs_to_latex> appends to the <LaTeX>
    output a comment block delimited by <verbatim|%%%%%%%%%% Begin TeXmacs
    source> and <verbatim|%%%%%%%%%% End TeXmacs source>, which contains,
    encoded in base64, the <TeXmacs> document (in <scheme> syntax, without
    the private attributes, see <cpp|purify>) followed by the marked
    <LaTeX> output. If the markers cannot be made transparent, the plain
    output is returned without this block.
  </explain>

  <section|Conservative import>

  <\explain>
    <cpp|tree conservative_latex_to_texmacs (string s, bool
    as_pic)><explain-synopsis|import reusing the original document>
  <|explain>
    Defined in <verbatim|conservative_fromtex.cpp>. If
    <verbatim|"latex-\<gtr\>texmacs:conservative"> is off, or if <verbatim|s>
    does not end with a block <verbatim|Begin TeXmacs source>
    (<cpp|get_texmacs_attachments>), this is <cpp|tracked_latex_to_texmacs>.
    Otherwise the block is decoded into the original <TeXmacs> document and
    the marked <LaTeX> which was generated from it, and:

    <\enumerate>
      <item><cpp|latex_correspondence> computes the positions of the
      paragraphs of the original document in the generated <LaTeX>. If the
      <LaTeX> file was not modified, the original document is returned.

      <item><cpp|latex_invarianted> searches the modified <LaTeX> for the
      unmodified pieces of the generated <LaTeX> and replaces them by
      <verbatim|{\\itm{<em|path>}}>.

      <item>The result is imported with <cpp|tracked_latex_to_texmacs>, and
      <cpp|latex_invarianted_replace> replaces the resulting <markup|itm>
      tags by the corresponding subtrees of the original document.

      <item>If the style, the preamble, the metadata or the abstract were
      not changed, they are restored from the original document
      (<cpp|texmacs_recover_style>, <cpp|texmacs_recover_preamble>,
      <cpp|texmacs_recover_metadata>, <cpp|texmacs_recover_abstract>);
      otherwise the preambles are merged (<cpp|texmacs_merge_preamble>).
    </enumerate>
  </explain>

  Since the encoded document is stored with paths into the body,
  <cpp|upgrade_texmacs_attachments> also upgrades documents written with
  older versions of <TeXmacs> and adapts the paths.

  <section|Error localization>

  The markers are also used by <cpp|try_latex_export>
  (<verbatim|latex_recover.cpp>), called by the <menu|Tools|LaTeX|Run>
  command. It exports the buffer with markers, runs <verbatim|pdflatex>,
  parses the log file (<cpp|get_latex_errors>) and, for each error, finds
  the position in the <LaTeX> source (<cpp|latex_error_find>) and the
  corresponding path in the <TeXmacs> document
  (<cpp|texmacs_error_find>). The widget of
  <verbatim|convert/latex/tmtex-widgets.scm> uses this information to show
  the error next to the offending markup.

  <section|Remarks>

  <\itemize>
    <item>The whole mechanism works at the granularity of paragraphs and
    environments; a small change inside a long paragraph causes the whole
    paragraph to be converted again.

    <item>The transparency checks multiply the cost of a conversion, since
    the document is converted several times.

    <item>The preference <verbatim|"texmacs-\<gtr\>latex:attach-tracking-info">,
    shown in the dialog as \PStore tracking information in <LaTeX>
    files\Q, is not consulted by <cpp|tracked_texmacs_to_latex> in this
    version: the information is appended whenever source tracking is on and
    succeeds.

    <item>A similar conservative mechanism exists for <name|BibTeX>
    (<verbatim|Data/Convert/BibTeX/conservative_bib.cpp>, preference
    <verbatim|"texmacs-\<gtr\>bibtex:conservative">).
  </itemize>

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
