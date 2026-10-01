<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Importing <name|HTML> and <name|MathML>>

  <section|The stages of the import>

  An <name|HTML> document is imported along the path
  <verbatim|html-document> <math|\<rightarrow\>> <verbatim|html-stree>
  <math|\<rightarrow\>> <verbatim|texmacs-stree> of the converter graph:

  <\enumerate>
    <item><scm|parse-html-document> (<verbatim|convert/html/htmltm.scm>)
    calls <scm|htmltm-parse> (<verbatim|convert/tools/xmltm.scm>), which
    parses the string with the <c++> parser <cpp|parse_html> and normalizes
    the namespaces of the resulting sxml. The result is wrapped into
    <scm|(!file ...)>; <scm|parse-html-snippet> does the same without the
    wrapper.

    <item><scm|html-\<gtr\>texmacs> converts the sxml into a <TeXmacs>
    stree with the dispatch table <scm|htmltm-methods%>, delegating
    <name|MathML> to <verbatim|convert/mathml/mathtm.scm>, and post-processes
    the result in <c++> with <scm|clean-html> (<cpp|clean_html>).
  </enumerate>

  For a complete document, the body is wrapped into a <TeXmacs> file with
  the style <verbatim|browser>.

  <section|Parsing>

  <subsection|The <name|XML>/<name|HTML> parser>

  <cpp|parse_xml> and <cpp|parse_plain_html> in
  <verbatim|Data/Convert/Xml/parsexml.cpp> are two modes of the same parser,
  <cpp|xml_html_parser>, which aims to accept a superset of the valid
  documents and never reports errors. It proceeds in three passes:

  <\enumerate>
    <item>A flat tokenization (<cpp|xml_html_parser::parse ()>) into
    opening and closing tags, text, comments, processing instructions,
    <verbatim|CDATA> sections and the <verbatim|DOCTYPE>. Entities are
    expanded on the fly (<cpp|expand_entities>), using the entities declared
    in the doctype and the tables <verbatim|HTMLlat1.scm>,
    <verbatim|HTMLspecial.scm>, <verbatim|HTMLsymbol.scm> and
    <verbatim|XML.scm> in <verbatim|$TEXMACS_PATH/langs/encoding>.
    For <name|HTML>, the input is first transcoded to UTF-8
    (<cpp|transcode>), using the encoding of the <name|XML> prolog if
    present.

    <item>The construction of the tree (<cpp|build>), which closes elements
    implicitly according to the rules of <name|HTML>: empty elements
    (<verbatim|br>, <verbatim|img>, ...), elements with optional closing
    tags (<verbatim|p>, <verbatim|li>, <verbatim|td>, ...), and badly nested
    elements (<cpp|build_valid_child>, <cpp|build_must_close>,
    <cpp|build_can_close>).

    <item>The conversion into sxml (<cpp|finalize_sxml>), which produces
    the element names, the attribute lists <scm|(@ ...)> and the special
    nodes <verbatim|*PI*> and <verbatim|*DOCTYPE*>.
  </enumerate>

  The result is a tree of the form <verbatim|(*TOP* (html (@ ...) ...))>,
  which is converted into a <scheme> expression by the glue. Unit tests for
  this parser are in <verbatim|src/tests/Data/Convert/Xml>.

  <subsection|<name|MathJax>>

  <cpp|parse_html> (<verbatim|Data/Convert/Xml/parsehtml.cpp>) is a wrapper
  around <cpp|parse_plain_html>. If the page loads <verbatim|MathJax.js> in
  its head (<cpp|contains_mathjax>), the formulas written in <TeX> syntax
  (<verbatim|$...$>, <verbatim|\\(...\\)>, <verbatim|equation>
  environments, ...) are extracted before parsing and replaced by elements
  <verbatim|\<less\>mathjax\<gtr\><em|n>\<less\>/mathjax\<gtr\>>; the
  formula is stored under the number <em|n> and can be retrieved with
  <scm|retrieve-mathjax>. The handler <scm|htmltm-mathjax> later imports
  it with the <LaTeX> importer (<scm|parse-latex> and
  <scm|latex-\<gtr\>texmacs>).

  <subsection|Namespace normalization>

  The parser is not aware of namespaces. <scm|xmltm-parse> therefore
  rewrites all element and attribute names with normalized prefixes:
  <verbatim|h:> for <name|XHTML> (and any <name|HTML> without namespace),
  <verbatim|m:> for <name|MathML>, <verbatim|x:> for the reserved
  <verbatim|xml> namespace, and <verbatim|g:> and <verbatim|c:> for the
  <name|Coq> formats which use the same machinery. The <verbatim|xmlns>
  attributes are consumed in the process. A <verbatim|\<less\>math\<gtr\>>
  element of <name|HTML5> without namespace thus becomes
  <verbatim|h:math>; the handler <scm|htmltm-math> renames the prefixes of
  its content to <verbatim|m:> before calling the <name|MathML> importer.

  <section|The converter>

  <subsection|Dispatch>

  The generic dispatcher <scm|sxml-dispatch> (<verbatim|xmltm.scm>) splits
  the name of an element into prefix and local name and looks up the local
  name in the table of the namespace: <scm|htmltm-methods%> for
  <verbatim|h>, <scm|mathtm-methods%> for <verbatim|m>. Elements without
  entry are handled by a <em|pass> method, which converts the content and
  keeps it (<scm|htmltm-pass>); strings are converted by
  <scm|xmltm-text>. Every method has the signature
  <scm|(<scm-arg|env> <scm-arg|attributes> <scm-arg|content>)> and returns a
  list which is either empty or contains one <TeXmacs> stree.

  <subsection|Handlers>

  Most entries of <scm|htmltm-methods%> are produced by

  <\explain>
    <scm|(htmltm-handler <scm-arg|model> <scm-arg|kind> <scm-arg|method>
    <scm-arg|args-\<gtr\>serial>)><explain-synopsis|make an entry for
    <scm|htmltm-methods%>>
  <|explain>
    Defined in <verbatim|xmltm.scm>; within <verbatim|htmltm.scm> it is
    abbreviated as <scm|handler>. <scm-arg|model> describes the treatment of
    white space in the content: <scm|:empty> (empty element),
    <scm|:element> (text nodes are ignored), <scm|:mixed> (leading and
    trailing white space dropped, internal white space collapsed),
    <scm|:collapse> (collapsed but kept at the ends) or <scm|:pre>
    (preserved). <scm-arg|kind> is <scm|:block> or <scm|:inline>; block
    handlers always return a <markup|document>. <scm-arg|method> is either
    a procedure, the name of a unary <TeXmacs> macro (a string, <abbr|e.g.>
    <scm|"strong">), a list such as <scm|(with "font-series" "bold")> to
    which the converted content is appended, or a literal node list. The
    handler also turns an <verbatim|id> attribute into a <markup|label>.
  </explain>

  For instance

  <\scm-code>
    (h2 (handler :mixed :block "section*"))

    (em (handler :collapse :inline "em"))

    (b \ (handler :collapse :inline '(with "font-series" "bold")))

    (ul (handler :element :block "itemize"))

    (img (handler :empty :inline htmltm-wikipedia-image))
  </scm-code>

  Elements which have no meaning in <TeXmacs> (<verbatim|head>,
  <verbatim|script>, <verbatim|style>, forms, frames, ...) are dropped with
  <scm|htmltm-drop>. The specific handlers deal with tables
  (<scm|htmltm-table>, which computes borders, widths, alignments and cell
  spans with the helpers of <verbatim|convert/tools/old-tmtable.scm>), list
  items, anchors and links (<scm|htmltm-anchor>), images
  (<scm|htmltm-image>), the deprecated <verbatim|font> element, line
  breaks, and a few special cases: <name|TeX> formulas given as images with
  class <verbatim|tex> or as <verbatim|span> elements in <name|Wikipedia>
  pages (<scm|htmltm-wikipedia-image>, <scm|htmltm-wikipedia-span>), and
  <verbatim|pre> elements of the <name|Scilab> documentation
  (<scm|htmltm-scilab-pre>).

  <subsection|Building the <TeXmacs> tree>

  The converted children are assembled into <em|serials> with
  <scm|htmltm-serial> and the constructors of <verbatim|convert/tools/stm.scm>
  (<scm|stm-serial>, <scm|stm-concat>): inline material becomes
  <markup|concat> nodes, and block material is collected into a
  <markup|document> whose paragraphs are the blocks and lines. An invariant
  of the implementation is that block structures are always wrapped into a
  unary <markup|document>, which is why block elements must be declared
  with the kind <scm|:block>. After the conversion,
  <scm|html-postproc> replaces non-breaking spaces and straight double
  quotes, the tree is simplified, and <cpp|clean_html>
  (<verbatim|Data/Convert/Xml/cleanhtml.cpp>) removes superfluous
  documents and white space, compresses list items and converts
  <markup|above>/<markup|below> constructs produced by <name|MathML> into
  big operators with scripts.

  <section|<name|MathML>>

  <name|MathML> elements are converted by <verbatim|convert/mathml/mathtm.scm>
  with the dispatch table <scm|mathtm-methods%> (entries made by
  <scm|mathtm-handler>): <verbatim|mi>, <verbatim|mn>, <verbatim|mo> and
  <verbatim|mtext> become strings or <TeXmacs> symbols, <verbatim|mfrac>,
  <verbatim|msqrt>, <verbatim|mroot>, the script elements and
  <verbatim|munder>/<verbatim|mover> become the corresponding primitives,
  <verbatim|mtable> becomes a <markup|tabular>, and so on. Operators,
  symbols and accents are translated with the tables of
  <verbatim|convert/mathml/mathml-drd.scm> (<scm|mathml-operator-\<gtr\>tm%>,
  <scm|mathml-symbol-\<gtr\>tm%>, <scm|mathml-above-\<gtr\>tm%>,
  <abbr|etc.>). The function <scm|mathml-\<gtr\>tree> imports a
  standalone <name|MathML> string.

  If the preference <verbatim|"mathml-\<gtr\>texmacs:latex-annotations">
  is on, a <verbatim|semantics> element with a <TeX> annotation
  (<verbatim|application/x-tex>) is imported from the annotation with the
  <LaTeX> importer instead of from the presentation markup
  (<scm|mathtm-semantics>). This is often more faithful for pages generated
  from <LaTeX>, such as <name|Wikipedia>.

  The export to <name|MathML> is described in <hlink|exporting to
  <name|HTML>|convert-html-export.en.tm>.

  <section|How to add support for a new element>

  <\enumerate>
    <item>Add an entry to <scm|htmltm-methods%> in
    <verbatim|htmltm.scm>, using <scm|handler> with the appropriate white
    space model and kind. If the element maps to a unary macro, a string is
    enough; otherwise write a method <scm|(define (htmltm-my-element env a
    c) ...)> which uses <scm|htmltm-args-serial> to convert the content and
    <scm|shtml-attr-non-null> to read attributes.

    <item>For <name|MathML>, add an entry to <scm|mathtm-methods%> in
    <verbatim|mathtm.scm>, or a symbol to the tables of
    <verbatim|mathml-drd.scm>.

    <item>Add a regression test to <verbatim|convert/html/htmltm-test.scm>
    (or <verbatim|convert/mathml/mathtm-test.scm>).
  </enumerate>

  <section|Testing>

  <scm|(regtest-htmltm)>, <scm|(regtest-xmltm)> and <scm|(regtest-mathtm)>
  run the regression tests of the importer
  (<verbatim|convert/html/htmltm-test.scm>,
  <verbatim|convert/tools/xmltm-test.scm>,
  <verbatim|convert/mathml/mathtm-test.scm>); they are part of
  <scm|(run-all-tests)>. In a <scheme> session,
  <scm|(parse-html-snippet <scm-arg|s>)> shows the normalized sxml and
  <scm|(html-\<gtr\>texmacs (parse-html-snippet <scm-arg|s>))> the
  resulting stree.

  <section|Known limitations>

  <\itemize>
    <item>CSS is ignored: style sheets and most <verbatim|style> attributes
    are dropped, so that the visual formatting of modern pages is largely
    lost. The import is mainly useful for the textual and structural
    content.

    <item><name|JavaScript> is not executed, so that content generated by
    scripts is missing.

    <item>The <name|MathJax> pre-processing only recognizes pages which load
    a file named <verbatim|MathJax.js>; pages loading <name|MathJax> 3 by
    other file names (such as the pages produced by the <TeXmacs>
    exporter itself) are imported without it.

    <item>The heuristics for badly formed <name|HTML> follow the rules of
    <name|HTML> 4 and may produce unexpected structures for <name|HTML5>
    pages.
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
