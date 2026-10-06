<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Exporting to <name|HTML>>

  <section|The stages of the export>

  <\enumerate>
    <item><cpp|buffer_export> (<source-link|Texmacs/Data/new_buffer.cpp|src/Texmacs/Data/new_buffer.cpp>)
    replaces the body of the document by the result of
    <cpp|edit_typeset_rep::exec_html>, which expands all macros which the
    <name|HTML> converter does not handle. The document is then passed to
    <cpp|export_tree> and <cpp|tree_to_generic> with the format
    <verbatim|html-document>.

    <item>The converter graph goes from <verbatim|texmacs-tree> to
    <verbatim|texmacs-stree>, then uses <scm|texmacs-\<gtr\>html>
    (<source-link|convert/html/tmhtml.scm|TeXmacs/progs/convert/html/tmhtml.scm>) to produce an sxml expression
    (<verbatim|html-stree>), and finally <scm|serialize-html>
    (<source-link|convert/html/htmlout.scm|TeXmacs/progs/convert/html/htmlout.scm>) to produce the string
    (<verbatim|html-document>).
  </enumerate>

  When a selection is copied as <name|HTML> (<source-link|Edit/Replace/edit_select.cpp|src/Edit/Replace/edit_select.cpp>),
  <cpp|exec_html> is applied to the selection and the snippet variants of
  the converters are used.

  <section|Macro expansion>

  <\explain>
    <cpp|tree edit_typeset_rep::exec_html (tree t, path p)><explain-synopsis|expand
    macros for the <name|HTML> export>
  <|explain>
    Implemented in <source-link|Edit/Editor/edit_typeset.cpp|src/Edit/Editor/edit_typeset.cpp> (accessible from
    <scheme> as <scm|html-expand>). It computes the environment at the
    start of the document, joins it with the patch returned by
    <scm|(tmhtml-env-patch)>, replaces each variable
    <verbatim|<em|name>> by <verbatim|tmhtml-<em|name>> when the latter
    exists (<cpp|prefix_specific>), and evaluates the body with
    <cpp|exec>. References are expanded during this evaluation, so that the
    converter sees their text. Finally, the title of the document and the
    values of the environment variables <verbatim|html-title>,
    <verbatim|html-css>, <verbatim|html-head-javascript>,
    <verbatim|html-head-javascript-src>, <verbatim|html-head-favicon>,
    <verbatim|html-extra-css>, <verbatim|html-extra-javascript-src>,
    <verbatim|html-extra-javascript> and <verbatim|html-site-version> are
    passed to the converter by wrapping the body into a <markup|with>
    (with the additional variable <verbatim|html-doc-title> for the title).
  </explain>

  The patch of <scm|tmhtml-env-patch> (<source-link|convert/html/tmhtml-expand.scm|TeXmacs/progs/convert/html/tmhtml-expand.scm>)
  defines identity macros <scm|(xmacro "x" (eval-args "x"))> for the tags
  which the converter handles itself: sectioning titles
  (<markup|section-title>, ...), lists, the logical markup
  (<markup|strong>, <markup|em>, <markup|code*>, ...), <markup|verbatim>,
  <markup|equation*>, <markup|hlink>, the balloons and the tags for
  customized <name|HTML> generation (see below), as well as a few math
  macros. All other macros are expanded. Contrary to the <LaTeX> export,
  the list is fixed (the comment in the code notes that the <abbr|DRD>
  should be used instead). The list must be kept consistent with the
  dispatch tables of <source-link|tmhtml.scm|TeXmacs/progs/convert/html/tmhtml.scm>.

  Style files use the <verbatim|tmhtml-> prefix to adapt their macros to
  the <name|HTML> export. For instance <source-link|packages/environment/env-math.ts|TeXmacs/packages/environment/env-math.ts>
  defines <markup|tmhtml-eqnarray*> as an <markup|extern> call of the
  <scheme> function <scm|ext-tmhtml-eqnarray*>, and
  <source-link|packages/standard/std-automatic.ts|TeXmacs/packages/standard/std-automatic.ts> defines
  <markup|tmhtml-render-bibitem>. The package <verbatim|html-font-size>
  (in <source-link|packages/html|TeXmacs/packages/html>) is another example.

  <section|The converter>

  <subsection|Entry point and sxml>

  <\explain>
    <scm|(texmacs-\<gtr\>html <scm-arg|x> <scm-arg|opts>)><explain-synopsis|convert
    a <TeXmacs> stree into sxml>
  <|explain>
    For a complete file, the body, the style and the language are assembled
    into <scm|(!file <scm-arg|body> <scm-arg|style> <scm-arg|lan>
    <scm-arg|path>)> and the function recurses. Otherwise the options are
    decoded (<scm|tmhtml-initialize>), the tree is converted by
    <scm|tmhtml-root>, and the result is finalized by
    <scm|tmhtml-finalize-document> (for a <verbatim|!file>) or
    <scm|tmhtml-finalize-selection> (for a snippet).
  </explain>

  The converter produces sxml in which <name|HTML> elements have the
  prefix <verbatim|h:> and <name|MathML> elements the prefix
  <verbatim|m:>, <abbr|e.g.> <scm|(h:p "text" (h:em "x"))>; attributes
  are written as <scm|(@ (<scm-arg|name> <scm-arg|value>) ...)>. The
  finalization strips these prefixes (<scm|sxml-strip-ns-prefix>), adds the
  <name|XML> processing instruction, an <name|XHTML> 1.1 doctype (with
  <name|MathML> 2.0 when <name|MathML> is enabled) and the namespace
  declarations. Helper functions for sxml are in
  <source-link|convert/tools/sxml.scm|TeXmacs/progs/convert/tools/sxml.scm> and <source-link|sxhtml.scm|TeXmacs/progs/convert/tools/sxhtml.scm>.

  All handlers return a <em|node list> (a list of sxml nodes and strings),
  which allows a <TeXmacs> construct to produce zero or several <name|HTML>
  nodes; the main function is

  <\scm-code>
    (define (tmhtml x)

    \ \ (cond ((and tmhtml-mathjax? (ahash-ref tmhtml-env :math))

    \ \ \ \ \ \ \ \ \ (tmhtml-mathjax-formula x))

    \ \ \ \ \ \ \ \ ((and tmhtml-mathml? (ahash-ref tmhtml-env :math))

    \ \ \ \ \ \ \ \ \ `((m:math (@ (xmlns "http://www.w3.org/1998/Math/MathML"))

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ ,(texmacs-\<gtr\>mathml x tmhtml-env))))

    \ \ \ \ \ \ \ \ ((and tmhtml-images? (ahash-ref tmhtml-env :math)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ (!= tmhtml-image-root-string "image"))

    \ \ \ \ \ \ \ \ \ (tmhtml-png `(with "mode" "math" ,x)))

    \ \ \ \ \ \ \ \ ((string? x)

    \ \ \ \ \ \ \ \ \ (if (string-null? x) '() (tmhtml-text x)))

    \ \ \ \ \ \ \ \ (else (or (tmhtml-dispatch 'tmhtml-primitives% x)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (tmhtml-implicit-compound x)))))
  </scm-code>

  The converter keeps its state in the hash table <scm|tmhtml-env>
  (keys <scm|:math>, <scm|:math-display>, <scm|:preformatted>,
  <scm|:mag>, <scm|:left-margin>, ...), which handlers modify locally with
  <scm|ahash-with>.

  <subsection|Dispatch tables>

  <\description>
    <item*|<scm|tmhtml-primitives%>>A <scm|logic-dispatcher> for the
    <TeXmacs> primitives (<markup|document>, <markup|concat>,
    <markup|with>, <markup|frac>, <markup|table>, <markup|image>,
    <markup|hlink>, <markup|label>, <markup|specific>, ...). Handlers
    receive the list of arguments.

    <item*|<scm|tmhtml-stdmarkup%>>A <scm|logic-table> for the tags of the
    standard styles. An entry is either a procedure (applied to the
    arguments), or a list such as <scm|(h:strong)>: in the latter case, the
    converted arguments are appended to the list, so that
    <scm|(strong "x")> becomes <scm|(h:strong "x")>.

    <item*|<scm|tmhtml-tmdocmarkup%>>Tags of the <verbatim|tmdoc> style,
    only used when the environment variable <verbatim|html-site-version> is
    not <verbatim|"2"> (the <verbatim|tmweb2> style renders these tags
    itself).

    <item*|<scm|tmhtml-with-cmd%>>The translation of <markup|with>: an
    entry for a pair <scm|("font-series" "bold")> gives a wrapper
    <scm|(h:b)>, an entry for a variable gives either a handler
    (<abbr|e.g.> <verbatim|"color"> or <verbatim|"par-left">) or a key of
    <scm|tmhtml-env> together with CSS properties (for the ornament
    parameters).
  </description>

  Tags which are found in none of the tables are dropped (with their
  content); this is why all macros which should survive must be expanded by
  <cpp|exec_html>. The explicit form <scm|(compound "name" ...)> is
  handled like the tag <markup|name>.

  <subsection|Document structure>

  <scm|tmhtml-document> and <scm|tmhtml-paragraph> map the paragraphs of a
  <markup|document> to <verbatim|p> elements (or to blocks such as
  <verbatim|div> or <verbatim|h2> when the paragraph starts with a block
  element, see <scm|tmhtml-p> and <scm|as-blocks>), and vertical spaces to
  CSS margins. Sectioning titles (<markup|section-title>, ...) become
  <verbatim|h1>-<verbatim|h6>, lists become <verbatim|ul>, <verbatim|ol>
  and <verbatim|dl> (the items are regrouped by <scm|tmhtml-post-item>),
  <markup|label> becomes an anchor <verbatim|\<less\>a id="..."\<gtr\>>,
  and <markup|hlink> an anchor with <verbatim|href>; the suffix
  <verbatim|.tm> of local links is replaced by <verbatim|.html> (or
  <verbatim|.xhtml>) by <scm|tmhtml-suffix>, so that links between the pages
  of a converted web site keep working. Tables are converted by
  <scm|tmhtml-table> and <scm|tmhtml-tformat> with the table parser of
  <source-link|convert/tools/tmtable.scm|TeXmacs/progs/convert/tools/tmtable.scm>; cell formatting becomes
  attributes and CSS styles.

  <subsection|The <verbatim|html> head>

  <scm|tmhtml-file> builds the <verbatim|html> element. The title is taken
  from the variable <verbatim|html-title>, the first title tag of the
  document (<markup|doc-title>, <markup|tmdoc-title>, ...,
  <scm|tmhtml-find-title>) or <verbatim|html-doc-title>. Unless
  <verbatim|html-css> gives an external style sheet, a default style sheet
  produced by <scm|tmhtml-css-header> is embedded. The preference
  <verbatim|"texmacs-\<gtr\>html:css-stylesheet"> may add a link to a
  style sheet (the dialog offers the style sheets of
  <verbatim|https://www.texmacs.org/css/>), and the variables
  <verbatim|html-extra-css>, <verbatim|html-head-javascript>,
  <verbatim|html-head-javascript-src>, <verbatim|html-extra-javascript>,
  <verbatim|html-extra-javascript-src> and <verbatim|html-head-favicon> add
  further elements to the head. With <name|MathJax>, a <verbatim|script>
  element loading <name|MathJax> 3 from a CDN is added. For the
  documentation and web styles (<verbatim|tmdoc>, <verbatim|tmweb>,
  <verbatim|tmweb2>, ...), <scm|tmhtml-tmdoc-post> wraps the body in a
  <verbatim|div> of class <verbatim|tmdoc-body>.

  <section|Mathematics>

  Formulas are exported in one of four ways, according to the options (at
  most one of the last three is on; the preferences dialog enforces this,
  and by default formulas are exported as images):

  <\description>
    <item*|Plain <name|HTML>>Used when none of the options below applies
    (in particular when images are requested but the export does not
    go to a file): <scm|tmhtml-frac>, <scm|tmhtml-sub>, <scm|tmhtml-sqrt> <abbr|etc.>
    approximate the formula with <name|HTML> elements, tables and the CSS
    classes of the default style sheet; symbols are converted by
    <scm|tmhtml-math-token>.

    <item*|<name|MathJax> (<verbatim|"texmacs-\<gtr\>html:mathjax">)>Each
    formula is converted to <LaTeX> by <scm|texmacs-\<gtr\>latex> with the
    option <verbatim|"texmacs-\<gtr\>latex:mathjax"> and written as
    <verbatim|\\(...\\)> in the page (<scm|tmhtml-mathjax-formula>); the
    page loads <name|MathJax>.

    <item*|<name|MathML> (<verbatim|"texmacs-\<gtr\>html:mathml">)>Each
    formula is converted by <scm|texmacs-\<gtr\>mathml>
    (<source-link|convert/mathml/tmmath.scm|TeXmacs/progs/convert/mathml/tmmath.scm>, dispatch table
    <scm|tmmath-primitives%>); symbols and operators are translated with the
    tables of <source-link|convert/mathml/mathml-drd.scm|TeXmacs/progs/convert/mathml/mathml-drd.scm>. The output file then
    gets the suffix <verbatim|.xhtml> when a whole site is converted.

    <item*|Images (<verbatim|"texmacs-\<gtr\>html:images">)>Each formula
    is typeset by <TeXmacs> and saved as a <name|PNG> picture by
    <scm|tmhtml-png>. This only happens when the export goes to a file
    (otherwise the image root is the default <verbatim|"image"> and the
    plain conversion is used).
  </description>

  Equation arrays and numbered equations have dedicated handlers
  (<scm|tmhtml-equation*>, <scm|tmhtml-equation-lab>, and
  <scm|ext-tmhtml-eqnarray*>, called from the style through
  <markup|tmhtml-eqnarray*>).

  <section|Images and graphics>

  <scm|tmhtml-image> treats the <markup|image> primitive:

  <\itemize>
    <item>a linked image in a web format (<scm|tmhtml-web-formats>:
    <verbatim|gif>, <verbatim|jpg>, <verbatim|jpeg>, <verbatim|png>,
    <verbatim|bmp>, <verbatim|svg>) becomes an <verbatim|img> element; if
    the file is not found relative to the target, it is looked up relative
    to the buffer and copied next to the target
    (<scm|tmhtml-web-image-name>);

    <item>an embedded image in a web format is saved to a file;

    <item>other images are rendered by <scm|tmhtml-png>.
  </itemize>

  <scm|tmhtml-png> renders arbitrary markup (graphics, <markup|draw-over>,
  <markup|draw-under>, <scm|(specific "image" ...)>, formulas) with
  <scm|print-snippet> into <verbatim|<em|base>-<em|n>.png> next to the
  target file (<scm|tmhtml-image-names>) and computes CSS margins from the
  extents so that the picture is aligned with the surrounding text. Images
  are cached by content (<scm|tmhtml-image-cache>) during one export. Labels
  inside the rendered markup are kept as <verbatim|id> attributes of the
  image.

  <section|Customized <name|HTML> generation>

  The standard styles (<source-link|packages/standard/std-markup.ts|TeXmacs/packages/standard/std-markup.ts>) define
  tags which only have an effect on the <name|HTML> export:
  <markup|html-tag>, <markup|html-attr>, <markup|html-style>,
  <markup|html-class>, <markup|html-div-style>, <markup|html-div-class>,
  <markup|html-javascript>, <markup|html-javascript-src> and
  <markup|html-video>. Their handlers in <source-link|tmhtml.scm|TeXmacs/progs/convert/html/tmhtml.scm> wrap the
  converted content into an arbitrary element or add attributes to it
  (<scm|tmhtml-append-attribute>). Similarly, <scm|(specific "html"
  <scm-arg|s>)> inserts raw <name|HTML> code and <scm|(specific "html*"
  <scm-arg|x>)> converts <scm-arg|x> only for the <name|HTML> export.

  <section|Converting a web site>

  The menu <menu|Tools|Create web site> opens a dialog
  (<scm|open-website-builder> in <source-link|doc/tmweb.scm|TeXmacs/progs/doc/tmweb.scm>) which calls
  <scm|tmweb-convert-dir> or <scm|tmweb-update-dir>. These functions walk
  through the source directory, convert every <verbatim|.tm> file with
  <scm|export-buffer-main> (setting <scm|current-save-target> so that the
  images go next to each page) and copy all other files; the update
  variant only converts files which are newer than their target
  (<scm|needs-update?>). Pages get the suffix <verbatim|.xhtml> instead of
  <verbatim|.html> when <name|MathML> export is enabled. The variants
  <scm|tmweb-convert-dir-keep-texmacs> and
  <scm|tmweb-update-dir-keep-texmacs> also copy the <verbatim|.tm>
  sources.

  <section|Options>

  <\big-table|<tabular|<tformat|<cwith|1|1|1|-1|font-series|bold>|<table|<row|<cell|Option>|<cell|Default>|<cell|Effect>>|<row|<cell|<verbatim|texmacs-\<gtr\>html:css>>|<cell|<verbatim|on>>|<cell|Use
  CSS for formatting.>>|<row|<cell|<verbatim|texmacs-\<gtr\>html:mathjax>>|<cell|<verbatim|off>>|<cell|Formulas
  for <name|MathJax>.>>|<row|<cell|<verbatim|texmacs-\<gtr\>html:mathml>>|<cell|<verbatim|off>>|<cell|Formulas
  in <name|MathML>.>>|<row|<cell|<verbatim|texmacs-\<gtr\>html:images>>|<cell|<verbatim|on>>|<cell|Formulas
  as images.>>|<row|<cell|<verbatim|texmacs-\<gtr\>html:css-stylesheet>>|<cell|<verbatim|--->>|<cell|External
  style sheet.>>>>>>
    Options of the converter <verbatim|texmacs-stree> <math|\<rightarrow\>>
    <verbatim|html-stree> (<source-link|init-html.scm|TeXmacs/progs/convert/html/init-html.scm>).
  </big-table>

  The first four options are read from the option list by
  <scm|tmhtml-initialize>; <verbatim|"texmacs-\<gtr\>html:css-stylesheet">
  is read as a preference (also by <scm|magic-png-number>, which adapts the
  size of formula images to the <verbatim|web-*> style sheets).

  <section|How to add support for a new tag>

  <\enumerate>
    <item>If the tag can be expressed with existing markup, nothing needs to
    be done: <cpp|exec_html> expands it. A <verbatim|tmhtml-<em|name>>
    variant may be defined in the style file if the <name|HTML> rendering
    should differ from the typeset rendering.

    <item>To map the tag to an <name|HTML> element, add an entry to
    <scm|tmhtml-stdmarkup%>, <abbr|e.g.> <scm|(my-tag (h:span (@ (class
    "my-tag"))))>, or a procedure <scm|(my-tag ,tmhtml-my-tag)> which
    receives the list of arguments and returns a node list (use
    <scm|tmhtml> to convert the arguments).

    <item>Add the tag to the list in <scm|tmhtml-env-patch>
    (<source-link|tmhtml-expand.scm|TeXmacs/progs/convert/html/tmhtml-expand.scm>), otherwise it is expanded before the
    converter sees it.

    <item>For the reverse direction, add a rule to <scm|htmltm-methods%>,
    see <hlink|importing <name|HTML>|convert-html-import.en.tm>.
  </enumerate>

  <section|Testing>

  <\itemize>
    <item><scm|(regtest-tmhtml)> (<source-link|convert/html/tmhtml-test.scm|TeXmacs/progs/convert/html/tmhtml-test.scm>)
    runs regression tests of <scm|tmhtml-root> on small strees; it is part
    of <scm|(run-all-tests)> (<source-link|check/check-master.scm|TeXmacs/progs/check/check-master.scm>), which can
    be run with <verbatim|texmacs -x "(run-all-tests)" -q> (see
    <source-link|src/tests/README.md|tests/README.md>). The tests are written with
    <scm|regression-test-group> (<source-link|kernel/boot/debug.scm|TeXmacs/progs/kernel/boot/debug.scm>).

    <item>In a <scheme> session, <scm|(texmacs-\<gtr\>html (tree-\<gtr\>stree
    <scm-arg|t>) '())> shows the sxml for a tree, and
    <scm|serialize-html> its serialization.
  </itemize>

  <section|Known limitations>

  <\itemize>
    <item>The generated markup is <name|XHTML> 1.1 with some deprecated
    elements (<verbatim|tt>, <verbatim|font>, <verbatim|center>, ...) and
    tables for the layout of plain <name|HTML> formulas.

    <item>Tags which are neither handled nor expanded are silently dropped;
    references are replaced by their text and do not become links.

    <item>Page layout (headers, footers, page breaks, columns) is mostly
    ignored, and graphics are only exported as bitmaps.

    <item>The conversion of a whole site relies on the conventions of the
    <verbatim|tmdoc> and <verbatim|tmweb> styles.
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
