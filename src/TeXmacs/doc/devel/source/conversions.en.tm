<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Converters to other data formats>

  <TeXmacs> comes with converters between its own format and <LaTeX>,
  <name|HTML> (including <name|MathML>), <name|XML> (<abbr|e.g.> the
  <TeXmacs> <name|XML> format <verbatim|tmml>), <name|BibTeX>, <scheme>
  (the <verbatim|stm> format), plain text, several programming languages and
  <name|Coq>, as well as export to <name|PDF>, <name|PostScript> and various
  image formats. Most converters are imperfect, and there is always room for
  improvement. This chapter briefly describes how the conversion machinery
  is organized and gives some recommendations based on our experience with
  the implementation of the existing converters. For practical instructions
  on how to add a new format, we refer to the chapter
  <hlink|Adding new data formats and
  converters|../../main/convert/new/man-newconv.en.tm>.

  <section|Organization of the converters>

  <subsection|Formats and the converter graph>

  Data formats and converters are declared in <scheme>, using the macros
  <scm|define-format> and <scm|converter> from
  <source-link|progs/kernel/texmacs/tm-convert.scm|TeXmacs/progs/kernel/texmacs/tm-convert.scm>. For instance,
  <source-link|progs/convert/latex/init-latex.scm|TeXmacs/progs/convert/latex/init-latex.scm> contains declarations such
  as

  <\scm-code>
    (define-format latex

    \ \ (:name "LaTeX")

    \ \ (:suffix "tex")

    \ \ (:recognize latex-recognizes?))

    \;

    (converter latex-document latex-tree

    \ \ (:function parse-latex-document))

    \;

    (converter latex-tree texmacs-tree

    \ \ (:function latex-\<gtr\>texmacs))
  </scm-code>

  Each format <verbatim|fm> gives rise to several \Pvariants\Q: the file
  <verbatim|fm-file>, the document <verbatim|fm-document> and the snippet
  <verbatim|fm-snippet> (as strings), and the parsed forms
  <verbatim|fm-tree> (a <c++> <cpp|tree>) or <verbatim|fm-stree> (a
  <scheme> tree). The macro <scm|define-format> automatically adds
  converters between <verbatim|fm-file> and <verbatim|fm-document>. The
  converters form a directed graph with weighted edges (the weight can be
  set with the <scm|:penalty> option); the function <scm|converter-search>
  finds the cheapest path between two formats, and <scm|convert> applies the
  successive conversions along this path. Options for converters are
  declared with <scm|:option> and are stored as preferences, such as
  <verbatim|"texmacs-\<gtr\>latex:expand-macros">. A converter may also be
  implemented by an external program (option <scm|:shell>), in which case it
  is only enabled when the program is found in the path. The routines
  <cpp|generic_to_tree> and <cpp|tree_to_generic> in
  <source-link|Data/Convert/Generic/generic.cpp|src/Data/Convert/Generic/generic.cpp> allow <c++> code to call the
  converters from <scheme>.

  <subsection|Where to find the converters>

  The parsers and the more performance critical parts of the converters are
  written in <c++> and can be found in the directory
  <verbatim|src/src/Data/Convert>:

  <\description>
    <item*|<verbatim|Texmacs>>Parsing and printing the native <TeXmacs>
    format (<source-link|fromtm.cpp|src/Data/Convert/Texmacs/fromtm.cpp>, <source-link|totm.cpp|src/Data/Convert/Texmacs/totm.cpp>) and the upgrade of
    documents written with older versions of <TeXmacs>
    (<source-link|upgradetm.cpp|src/Data/Convert/Texmacs/upgradetm.cpp>).

    <item*|<verbatim|Scheme>>Conversion between trees and <scheme>
    expressions.

    <item*|<verbatim|Tex>>The <LaTeX> parser (<source-link|parsetex.cpp|src/Data/Convert/Tex/parsetex.cpp>), the
    conversion of the parsed <LaTeX> into <TeXmacs>
    (<source-link|fromtex.cpp|src/Data/Convert/Tex/fromtex.cpp>, <source-link|fromtex_post.cpp|src/Data/Convert/Tex/fromtex_post.cpp>), the importation of
    metadata for various journal styles (<verbatim|metadata*.cpp>) and the
    \Pconservative\Q and \Ptracked\Q converters, which attach source
    tracking information to the exported <LaTeX> so that it can be
    re-imported with minimal changes (<verbatim|conservative_*.cpp>,
    <verbatim|tracked_*.cpp>).

    <item*|<verbatim|Xml>>Parsers for <name|XML> and <name|HTML>
    (<source-link|parsexml.cpp|src/Data/Convert/Xml/parsexml.cpp>, <source-link|parsehtml.cpp|src/Data/Convert/Xml/parsehtml.cpp>) and some cleaning
    routines.

    <item*|<verbatim|BibTeX>>The <name|BibTeX> parser and the conservative
    import and export of bibliographies.

    <item*|<verbatim|Verbatim>>Conversion from and to plain text.

    <item*|<verbatim|Coq>>A parser for the <name|Coq> vernacular.

    <item*|<verbatim|Generic>>Glue with the <scheme> converters, indexing
    and postprocessing.
  </description>

  The remaining parts of the converters are written in <scheme> and can be
  found in <verbatim|src/TeXmacs/progs/convert>. For instance,
  <source-link|convert/latex/tmtex.scm|TeXmacs/progs/convert/latex/tmtex.scm> implements the conversion from
  <TeXmacs> to <LaTeX> and <source-link|convert/latex/texout.scm|TeXmacs/progs/convert/latex/texout.scm> the
  serialization of <LaTeX>; <source-link|convert/html/htmltm.scm|TeXmacs/progs/convert/html/htmltm.scm> and
  <source-link|convert/html/tmhtml.scm|TeXmacs/progs/convert/html/tmhtml.scm> implement the conversions from
  <abbr|resp.> to <name|HTML>, and <verbatim|convert/mathml> the conversions
  from and to <name|MathML>. Plug-ins may define additional formats (see
  for instance the files <verbatim|*-format.scm> in the plug-ins for
  programming languages).

  <section|Parsing extern data formats>

  In order to write a converter from <LaTeX>, <name|HTML>, <name|XML>,
  <abbr|etc.> to <TeXmacs>, a good first step is to write a parser for the
  extern data format. For <name|HTML>, <name|XML>, <abbr|etc.> this should
  be rather easy, but for <LaTeX>, you will probably need to be a real
  <LaTeX> guru. We recommend the result of the parsing step to be a
  <scheme> expression (as is the case for the <name|HTML> and <name|XML>
  parsers, which produce <verbatim|html-stree> <abbr|resp.>
  <verbatim|xml-stree> expressions), because this language is very well
  adapted for the implementation of the actual converter.

  This first step should be able to process any correct file having the
  extern data format; possible incompatibilities should only come into play
  during the actual conversion. In the case of <LaTeX>, one should not expand
  the macros and keep all macro definitions, because <TeXmacs> will be able
  to take advantage out of this.

  <section|The actual converter>

  We recommend the actual converter to proceed in several steps. Often it is
  convenient to start with a rough, structural, conversion step, which is
  \Ppolished\Q by a certain number of additional steps. These additional
  steps may take care of some very particular layout issues which can not be
  treated conveniently at the main step. For instance, the <c++> function
  <cpp|latex_to_tree> in <source-link|Data/Convert/Tex/fromtex_post.cpp|src/Data/Convert/Tex/fromtex_post.cpp>
  successively filters the preamble, converts the parsed <LaTeX> into a
  rough <TeXmacs> tree, finalizes the document structure (paragraphs,
  sections, algorithms, matrices), handles the preamble and matching
  environments, upgrades the result to the current <TeXmacs> conventions and
  finally corrects the tree with respect to the DRD.

  Actually, the main difficulties usually come from exceptional text, like
  verbatim, and layout issues which are handled differently in the extern
  data format and <TeXmacs>. A good example of such a difference between
  <LaTeX> and <TeXmacs> is the way equations or lists are handled. Consider
  for instance the following paragraph:

  Text before.

  <\equation*>
    a<rsup|2>+b<rsup|2>=c<rsup|2>.
  </equation*>

  Text after.

  In <LaTeX>, the equation is really seen as a part of the paragraph. Indeed,
  there will not be any blank line between \PText before\Q and the equation.
  However, for efficiency reasons, it is better to see the paragraph as three
  paragraphs in <TeXmacs>, because the lines can be typeset independently.
  Nevertheless, the equation environment will disable the indentation of
  \PText after\Q.

  As a result of this anomaly, converted texts have to be postprocessed, so
  as to insert paragraph breaks at strategic places (in the <LaTeX> importer,
  this is done by <cpp|finalize_layout> and <cpp|make_paragraphs> in
  <source-link|fromtex_post.cpp|src/Data/Convert/Tex/fromtex_post.cpp>). It should be noticed that this step may be
  independent from the format which is actually being converted and that a
  similar reverse step may be implemented for backward conversions. We also
  notice that one needs an exhaustive list of all similar exceptional
  environments for this postprocessing step. From a semantical point of
  view, one should also be able to detect that the above example logically
  forms only one and not three paragraphs.

  <section|Backward conversions>

  Conversions from <TeXmacs> to an extern data format are usually easier to
  implement, because the <TeXmacs> data format is semantically rich.
  However, conversions to an extern data format without a <TeX>-like macro
  facility give rise to the problem of macro expansion of non supported
  <TeXmacs> functions or environments. The existing converters address this
  problem in several ways: the <LaTeX> exporter can expand macros or keep
  them as <LaTeX> definitions (see the options
  <verbatim|"texmacs-\<gtr\>latex:expand-macros"> and
  <verbatim|"texmacs-\<gtr\>latex:expand-user-macros">), while the
  <name|HTML> exporter expands the document before the conversion (see
  <source-link|convert/html/tmhtml-expand.scm|TeXmacs/progs/convert/html/tmhtml-expand.scm>). Graphics and other constructs
  without a counterpart in the target format may also be exported as images
  (see for instance the option <verbatim|"texmacs-\<gtr\>html:images">).

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

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
