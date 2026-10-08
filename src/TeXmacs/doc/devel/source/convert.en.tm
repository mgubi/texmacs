<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|The <LaTeX> and <name|HTML> converters>

  <section|Introduction>

  The general machinery for data formats and converters (the macros
  <scm|define-format> and <scm|converter>, the converter graph,
  <scm|converter-search> and <scm|convert>, converter options stored as
  preferences) is described in the chapter on <hlink|converters to other
  data formats|conversions.en.tm>. The present chapter goes one level
  deeper and explains how the two most important converters actually work:
  the converters between <TeXmacs> and <LaTeX> and between <TeXmacs> and
  <name|HTML> (including <name|MathML>), in both directions. The user-level
  description of these converters can be found in the chapters
  <hlink|converters for <LaTeX>|../../main/convert/latex/man-latex.en.tm>
  and <hlink|converters for <name|HTML>|../../main/convert/html/man-html.en.tm>.

  All <c++> file names below are relative to <source-link|src/src/|src> and all
  <scheme> file names are relative to <source-link|src/TeXmacs/progs/|TeXmacs/progs>.

  <section|Overview of the four pipelines>

  Each converter is a chain of stages, each of which works on a specific
  data representation. In <scheme>, <TeXmacs> documents are manipulated as
  <em|strees> (<scheme> trees such as <scm|(frac "1" "2")>, see
  <scm|tree-\<gtr\>stree>), <name|HTML> and <name|XML> documents as
  <em|sxml> expressions (such as <scm|(h:p (@ (class "x")) "text")>), and
  <LaTeX> documents as <em|<LaTeX> strees> with special labels starting with
  <verbatim|!> (such as <scm|(!concat (textbf "a") "b")>). The parsed
  <LaTeX> produced by the <c++> parser is yet another representation: a
  <c++> <cpp|tree> made of <markup|tuple> nodes whose first child is the
  name of the command, such as <verbatim|(tuple "\\frac" "1" "2")>.

  <\description>
    <item*|<TeXmacs> <math|\<rightarrow\>> <LaTeX>>Macro expansion of the
    unsupported markup in the editor (<cpp|edit_typeset_rep::exec_latex>),
    optional source tracking and conservative merging in <c++>
    (<source-link|Data/Convert/Tex/conservative_totex.cpp|src/Data/Convert/Tex/conservative_totex.cpp>,
    <source-link|tracked_totex.cpp|src/Data/Convert/Tex/tracked_totex.cpp>), then the <scheme> converter
    <scm|texmacs-\<gtr\>latex> (<source-link|convert/latex/tmtex.scm|TeXmacs/progs/convert/latex/tmtex.scm>) which
    maps the <TeXmacs> stree to a <LaTeX> stree, and finally the serializer
    <scm|serialize-latex> (<source-link|convert/latex/texout.scm|TeXmacs/progs/convert/latex/texout.scm>) which also
    builds the preamble with the help of <source-link|convert/latex/latex-tools.scm|TeXmacs/progs/convert/latex/latex-tools.scm>.

    <item*|<LaTeX> <math|\<rightarrow\>> <TeXmacs>>The <c++> parser
    <cpp|parse_latex> / <cpp|parse_latex_document>
    (<source-link|Data/Convert/Tex/parsetex.cpp|src/Data/Convert/Tex/parsetex.cpp>) which produces parsed <LaTeX>,
    the translation <cpp|parsed_latex_to_tree>
    (<source-link|Data/Convert/Tex/fromtex.cpp|src/Data/Convert/Tex/fromtex.cpp>) and the post-processing driver
    <cpp|latex_to_tree> (<source-link|Data/Convert/Tex/fromtex_post.cpp|src/Data/Convert/Tex/fromtex_post.cpp>). The
    knowledge about <LaTeX> commands (types and arities) lives in <scheme>
    tables in <verbatim|convert/latex/latex-*-drd.scm>, which the <c++> code
    queries through <cpp|latex_type> and <cpp|latex_arity>. Optional source
    tracking and conservative re-import are implemented in
    <source-link|tracked_fromtex.cpp|src/Data/Convert/Tex/tracked_fromtex.cpp> and <source-link|conservative_fromtex.cpp|src/Data/Convert/Tex/conservative_fromtex.cpp>.

    <item*|<TeXmacs> <math|\<rightarrow\>> <name|HTML>>Macro expansion in
    the editor (<cpp|edit_typeset_rep::exec_html>), the <scheme> converter
    <scm|texmacs-\<gtr\>html> (<source-link|convert/html/tmhtml.scm|TeXmacs/progs/convert/html/tmhtml.scm>) which
    produces sxml, possibly using <scm|texmacs-\<gtr\>mathml>
    (<source-link|convert/mathml/tmmath.scm|TeXmacs/progs/convert/mathml/tmmath.scm>) or the <LaTeX> converter (for
    <name|MathJax>) for formulas, and the serializer <scm|serialize-html>
    (<source-link|convert/html/htmlout.scm|TeXmacs/progs/convert/html/htmlout.scm>).

    <item*|<name|HTML> <math|\<rightarrow\>> <TeXmacs>>The <c++> parser
    <cpp|parse_html> (<source-link|Data/Convert/Xml/parsehtml.cpp|src/Data/Convert/Xml/parsehtml.cpp> and
    <source-link|parsexml.cpp|src/Data/Convert/Xml/parsexml.cpp>) which produces sxml, namespace normalization
    <scm|htmltm-parse> (<source-link|convert/tools/xmltm.scm|TeXmacs/progs/convert/tools/xmltm.scm>), the converter
    <scm|html-\<gtr\>texmacs> (<source-link|convert/html/htmltm.scm|TeXmacs/progs/convert/html/htmltm.scm>) with
    <name|MathML> handled by <source-link|convert/mathml/mathtm.scm|TeXmacs/progs/convert/mathml/mathtm.scm>, and the
    <c++> cleaning pass <cpp|clean_html>
    (<source-link|Data/Convert/Xml/cleanhtml.cpp|src/Data/Convert/Xml/cleanhtml.cpp>).
  </description>

  The formats are declared in <source-link|convert/latex/init-latex.scm|TeXmacs/progs/convert/latex/init-latex.scm> and
  <source-link|convert/html/init-html.scm|TeXmacs/progs/convert/html/init-html.scm>; these files are loaded lazily by
  <scm|lazy-format> in <source-link|init-texmacs.scm|TeXmacs/progs/init-texmacs.scm>. The tables below
  summarize the converters which they declare.

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|font-series|bold>|<table|<row|<cell|From>|<cell|To>|<cell|Function>>|<row|<cell|<verbatim|texmacs-stree>>|<cell|<verbatim|latex-stree>>|<cell|<scm|texmacs-\<gtr\>latex>>>|<row|<cell|<verbatim|latex-stree>>|<cell|<verbatim|latex-document>,
  <verbatim|latex-snippet>>|<cell|<scm|serialize-latex>>>|<row|<cell|<verbatim|texmacs-stree>>|<cell|<verbatim|latex-document>>|<cell|<scm|conservative-texmacs-\<gtr\>latex>>>|<row|<cell|<verbatim|latex-document>>|<cell|<verbatim|latex-tree>>|<cell|<scm|parse-latex-document>>>|<row|<cell|<verbatim|latex-snippet>>|<cell|<verbatim|latex-tree>>|<cell|<scm|parse-latex>>>|<row|<cell|<verbatim|latex-tree>>|<cell|<verbatim|texmacs-tree>>|<cell|<scm|latex-\<gtr\>texmacs>>>|<row|<cell|<verbatim|latex-document>>|<cell|<verbatim|texmacs-tree>>|<cell|<scm|latex-document-\<gtr\>texmacs>>>|<row|<cell|<verbatim|latex-class-document>>|<cell|<verbatim|texmacs-tree>>|<cell|<scm|latex-class-document-\<gtr\>texmacs>>>|<row|<cell|<verbatim|texmacs-stree>>|<cell|<verbatim|html-stree>>|<cell|<scm|texmacs-\<gtr\>html>>>|<row|<cell|<verbatim|html-stree>>|<cell|<verbatim|html-document>,
  <verbatim|html-snippet>>|<cell|<scm|serialize-html>>>|<row|<cell|<verbatim|html-document>>|<cell|<verbatim|html-stree>>|<cell|<scm|parse-html-document>>>|<row|<cell|<verbatim|html-snippet>>|<cell|<verbatim|html-stree>>|<cell|<scm|parse-html-snippet>>>|<row|<cell|<verbatim|html-stree>>|<cell|<verbatim|texmacs-stree>>|<cell|<scm|html-\<gtr\>texmacs>>>>>>>
    Converters declared in <source-link|init-latex.scm|TeXmacs/progs/convert/latex/init-latex.scm> and
    <source-link|init-html.scm|TeXmacs/progs/convert/html/init-html.scm>.
  </big-table>

  Since every edge has the same default penalty, the converter graph
  prefers the direct edges: exporting a document to <verbatim|latex-document>
  uses <scm|conservative-texmacs-\<gtr\>latex> (one step) rather than
  <scm|texmacs-\<gtr\>latex> followed by <scm|serialize-latex> (two steps),
  and importing a <verbatim|latex-document> uses
  <scm|latex-document-\<gtr\>texmacs>. Snippets (used for copy and paste)
  go through <verbatim|latex-stree> <abbr|resp.> <verbatim|latex-tree>.

  <section|Interaction with the editor>

  Exporting a buffer (<scm|export-buffer-main> in
  <source-link|texmacs/texmacs/tm-files.scm|TeXmacs/progs/texmacs/texmacs/tm-files.scm>) sets the global variables
  <scm|current-save-source> and <scm|current-save-target> and then calls
  the <c++> routine <cpp|buffer_export> in
  <source-link|Texmacs/Data/new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>. For the <name|HTML> format, this
  routine first replaces the body by <cpp|exec_html> of the body, so that
  all macros which <name|HTML> does not know are expanded <em|using the
  environment of the document> (the style, the packages and the user's
  macros). For <LaTeX>, it only attaches a <verbatim|view> attribute to the
  document: the expansion is done later, inside the conversion itself, by
  <cpp|latex_expand> which retrieves the view and calls <cpp|exec_latex>.
  The resulting document is passed to <cpp|export_tree>, which calls
  <cpp|tree_to_generic> and hence the <scheme> converters. Image file
  names produced during the export are derived from
  <scm|current-save-target>.

  Importing goes through <cpp|generic_to_tree> with the format
  <verbatim|latex-document> or <verbatim|html-document>. The same converters
  are used for copying and pasting in the corresponding formats (with the
  snippet variants).

  <section|Source map>

  <\description-paragraphs>
    <item*|<source-link|convert/latex/init-latex.scm|TeXmacs/progs/convert/latex/init-latex.scm>>Format declarations,
    converters and their options for <LaTeX>.

    <item*|<source-link|convert/latex/tmtex.scm|TeXmacs/progs/convert/latex/tmtex.scm>>The <TeXmacs> to <LaTeX>
    stree converter.

    <item*|<verbatim|convert/latex/tmtex-*.scm>>Style-specific variants for
    journal styles (<source-link|tmtex-acm.scm|TeXmacs/progs/convert/latex/tmtex-acm.scm>, <source-link|tmtex-ams.scm|TeXmacs/progs/convert/latex/tmtex-ams.scm>,
    <source-link|tmtex-beamer.scm|TeXmacs/progs/convert/latex/tmtex-beamer.scm>, <source-link|tmtex-elsevier.scm|TeXmacs/progs/convert/latex/tmtex-elsevier.scm>,
    <source-link|tmtex-ieee.scm|TeXmacs/progs/convert/latex/tmtex-ieee.scm>, <source-link|tmtex-revtex.scm|TeXmacs/progs/convert/latex/tmtex-revtex.scm>,
    <source-link|tmtex-springer.scm|TeXmacs/progs/convert/latex/tmtex-springer.scm>) and the debugging widget
    <source-link|tmtex-widgets.scm|TeXmacs/progs/convert/latex/tmtex-widgets.scm>.

    <item*|<source-link|convert/latex/texout.scm|TeXmacs/progs/convert/latex/texout.scm>>Serialization of <LaTeX>
    strees.

    <item*|<source-link|convert/latex/latex-tools.scm|TeXmacs/progs/convert/latex/latex-tools.scm>>Preamble construction:
    package management, macro definitions, catcodes, page size, colors.

    <item*|<source-link|convert/latex/latex-command-drd.scm|TeXmacs/progs/convert/latex/latex-command-drd.scm>,
    <source-link|latex-symbol-drd.scm|TeXmacs/progs/convert/latex/latex-symbol-drd.scm>, <source-link|latex-texmacs-drd.scm|TeXmacs/progs/convert/latex/latex-texmacs-drd.scm>,
    <source-link|latex-drd.scm|TeXmacs/progs/convert/latex/latex-drd.scm>>Logical tables describing <LaTeX> commands,
    symbols, the extra <LaTeX> macros introduced by <TeXmacs>, package
    dependencies and paper sizes.

    <item*|<source-link|convert/latex/latex-define.scm|TeXmacs/progs/convert/latex/latex-define.scm>,
    <source-link|latex-overload.scm|TeXmacs/progs/convert/latex/latex-overload.scm>>Definitions (as <LaTeX> strees) of the
    extra macros and environments which <TeXmacs> puts in the preamble.

    <item*|<source-link|Data/Convert/Tex/|src/Data/Convert/Tex>>The <LaTeX> parser
    (<source-link|parsetex.cpp|src/Data/Convert/Tex/parsetex.cpp>), the importer (<source-link|fromtex.cpp|src/Data/Convert/Tex/fromtex.cpp>,
    <source-link|fromtex_post.cpp|src/Data/Convert/Tex/fromtex_post.cpp>), the importer for class files
    (<source-link|fromcls.cpp|src/Data/Convert/Tex/fromcls.cpp>), the bridge to the <scheme> tables
    (<source-link|inittex.cpp|src/Data/Convert/Tex/inittex.cpp>), metadata import (<verbatim|metadata*.cpp>),
    tools on <LaTeX> sources (<source-link|latex_tools.cpp|src/Data/Convert/Tex/latex_tools.cpp>), error recovery
    and <verbatim|pdflatex> runs (<source-link|latex_recover.cpp|src/Data/Convert/Tex/latex_recover.cpp>), source
    tracking and conservative conversion (<verbatim|tracked_*.cpp>,
    <verbatim|conservative_*.cpp>).

    <item*|<source-link|Plugins/LaTeX_Preview/|src/Plugins/LaTeX_Preview>>Rendering of <LaTeX> fragments
    as pictures during the import.

    <item*|<source-link|convert/html/init-html.scm|TeXmacs/progs/convert/html/init-html.scm>>Format declaration and
    converters for <name|HTML>.

    <item*|<source-link|convert/html/tmhtml.scm|TeXmacs/progs/convert/html/tmhtml.scm>,
    <source-link|tmhtml-expand.scm|TeXmacs/progs/convert/html/tmhtml-expand.scm>, <source-link|htmlout.scm|TeXmacs/progs/convert/html/htmlout.scm>>The export to
    <name|HTML>.

    <item*|<source-link|convert/html/htmltm.scm|TeXmacs/progs/convert/html/htmltm.scm>>The import from <name|HTML>.

    <item*|<source-link|convert/mathml/|TeXmacs/progs/convert/mathml>>Export (<source-link|tmmath.scm|TeXmacs/progs/convert/mathml/tmmath.scm>) and
    import (<source-link|mathtm.scm|TeXmacs/progs/convert/mathml/mathtm.scm>, <source-link|mathml-drd.scm|TeXmacs/progs/convert/mathml/mathml-drd.scm>) of
    <name|MathML>.

    <item*|<source-link|convert/tools/|TeXmacs/progs/convert/tools>>Shared tools: sxml accessors
    (<source-link|sxml.scm|TeXmacs/progs/convert/tools/sxml.scm>, <source-link|sxhtml.scm|TeXmacs/progs/convert/tools/sxhtml.scm>), <name|XML> import helpers
    (<source-link|xmltm.scm|TeXmacs/progs/convert/tools/xmltm.scm>), construction of <TeXmacs> strees
    (<source-link|stm.scm|TeXmacs/progs/convert/tools/stm.scm>, <source-link|tmconcat.scm|TeXmacs/progs/convert/tools/tmconcat.scm>), lengths, colors and
    tables (<source-link|tmlength.scm|TeXmacs/progs/convert/tools/tmlength.scm>, <source-link|tmcolor.scm|TeXmacs/progs/convert/tools/tmcolor.scm>,
    <source-link|tmtable.scm|TeXmacs/progs/convert/tools/tmtable.scm>, <source-link|old-tmtable.scm|TeXmacs/progs/convert/tools/old-tmtable.scm>), CSS
    (<source-link|css.scm|TeXmacs/progs/convert/tools/css.scm>), the output buffer used by the serializers
    (<source-link|output.scm|TeXmacs/progs/convert/tools/output.scm>) and a pre-processor for the <LaTeX> export
    (<source-link|tmpre.scm|TeXmacs/progs/convert/tools/tmpre.scm>).

    <item*|<verbatim|Data/Convert/Xml/>>The <name|XML>/<name|HTML> parser
    (<source-link|parsexml.cpp|src/Data/Convert/Xml/parsexml.cpp>, <source-link|parsehtml.cpp|src/Data/Convert/Xml/parsehtml.cpp>) and the cleaning of
    imported <name|HTML> (<source-link|cleanhtml.cpp|src/Data/Convert/Xml/cleanhtml.cpp>).

    <item*|<source-link|doc/tmweb.scm|TeXmacs/progs/doc/tmweb.scm>>Conversion of whole directories into a
    web site.
  </description-paragraphs>

  <section|Contents of this chapter>

  <\traverse>
    <branch|Exporting to <LaTeX>|convert-latex-export.en.tm>

    <branch|Importing <LaTeX>|convert-latex-import.en.tm>

    <branch|Source tracking and conservative
    conversion|convert-latex-tracking.en.tm>

    <branch|Exporting to <name|HTML>|convert-html-export.en.tm>

    <branch|Importing <name|HTML> and <name|MathML>|convert-html-import.en.tm>

    <branch|The <name|Markdown> converters|convert-markdown.en.tm>
  </traverse>

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
