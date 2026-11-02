<TeXmacs|1.0.7.19>

<style|tmdoc>

<\body>
  <tmdoc-title|General environment variables>

  <\explain>
    <var-val|mode|text><explain-synopsis|major mode>
  <|explain>
    This very important environment variable determines the <em|current
    mode>. There are four possible values: <verbatim|text> (text mode),
    <verbatim|math> (mathematical mode), <verbatim|prog> (programming mode)
    and <verbatim|src> (source mode). The behaviour of the editor (menus,
    keystrokes, typesetting, <abbr|etc.>) depends heavily on the mode. For
    example, the following code may be used in order to include a
    mathematical formula inside text:

    <\tm-fragment>
      The formula <math|a<rsup|2>+b<rsup|2>=c<rsup|2>> is well known.
    </tm-fragment>

    <\tm-fragment>
      <inactive*|The formula <math|a<rsup|2>+b<rsup|2>=c<rsup|2>> is well
      known.>
    </tm-fragment>

    Some other environment variables (mainly the language and the font) also
    depend on the current mode (in this context, the source mode always
    behaves in a similar way as the text mode). During
    copy<space|0.2spc>&<space|0.2spc>paste and
    search<space|0.2spc>&<space|0.2spc>replace operations, <TeXmacs> tries to
    preserve the mode.
  </explain>

  <\explain>
    <var-val|language|english>

    <var-val|math-language|texmath>

    <var-val|prog-language|scheme><explain-synopsis|language>
  <|explain>
    A second major environment variable is the <em|current language>. In
    fact, there are three such environment variables: one for each mode. The
    language in which content is written is responsible for associating a
    precise semantics to the content. This semantics is used for different
    purposes:

    <\itemize>
      <item>The language may specify rules for typesetting content. For
      instance, the text language specifies punctuation and hyphenation
      rules. Similarly the mathematical language containns spacing
      information for mathematical operators.

      <item>Several editing operations depend on the current language: when
      performing a search or replace operation, <TeXmacs> is both mode and
      language sensitive. Similarly, the text language determines the
      dictionary to use when spell-checking the document.

      <item>The language controls (among other parameters like the mode and
      the document format) the way content is being converted from one
      context to another.

      Currently, no real language-dependent conversions have been implemented
      yet. But in the future one may imagine that copying a piece of English
      text to a document written in French will perform an automatic
      translation. Similarly, a mathematical document might be converted from
      infix to postfix notation.

      <item>The programming language determines the current scripting
      language in use. Other scripting languages than <scheme> are currently
      only used for interactive sessions, but primitives like <markup|extern>
      might become language-sensitive in the future.
    </itemize>

    At the moment, the current language is mainly used as a hint for
    indicating the semantics of text: it is not required that a text written
    in English contains no spelling errors, or that a formula written in a
    mathematical language is mathematically or even syntactically correct.
    Nevertheless, the editor is intended to enforce correctness more and
    more, especially for mathematics.

    The language may be specified globally for the whole document in
    <menu|Document|Language> and locally for a piece of text in
    <menu|Format|Language>.
  </explain>

  <\explain>
    <var-val|prog-session|default><explain-synopsis|name of programming
    session>
  <|explain>
    This environment variables is used in addition to the
    <src-var|prog-language> variable in order to determine a concrete
    implementation as well as a particular instance of the current
    programming language. For instance, in case of the
    <hlink|<name|Maxima>|../../../../plugins/maxima/doc/maxima-abstract.en.tm>
    language, different implementation may be used fooor the underlying
    <name|Lisp>. Similarly, one may wish to run two different instances of
    <name|Maxima> in parallel.
  </explain>

  <\explain>
    <label|magnification><var-val|magnification|1><explain-synopsis|magnification>
  <|explain>
    This variable determines the magnification which is applied to all
    content. Magnifications bigger than one are typically useful for
    presentations (from slides or from a laptop):

    <\tm-fragment>
      normal<htab|5mm><with|magnification|2|big><htab|5mm><with|magnification|3|huge>
    </tm-fragment>

    <\tm-fragment>
      <inactive*|normal<htab|5mm><with|magnification|2|big><htab|5mm><with|magnification|3|huge>>
    </tm-fragment>

    The magnification should not be confused with the <hlink|font
    size|env-font.en.tm#font-base-size>: contrary to the magnification, the
    font size may also affect the shapes of the glyphs. The magnification is
    usually specified for the entire document in
    <menu|Document|Magnification>.
  </explain>

  <\explain>
    <var-val|bg-color|white><explain-synopsis|background color>
  <|explain>
    The background color for your document, as specified in
    <menu|Document|Color|Background>. It may also be a picture or a pattern,
    made with the <markup|pattern> primitive (see below). The background of
    a single page is <src-var|page-this-bg-color>, which may be a color or
    a pattern as well.
  </explain>

  <\explain>
    <explain-macro|pattern|file|width|height|effect|base><explain-synopsis|image
    used as a color>
  <|explain>
    A color made from the image <src-arg|file>, repeated as a tile of the
    given <src-arg|width> and <src-arg|height> (lengths, or percentages of
    the box which is painted, such as <verbatim|100%>, or <verbatim|100@>
    for the size which keeps the proportions). It can be used wherever a
    color is expected: <src-var|bg-color>, <src-var|page-this-bg-color>,
    <src-var|color>, <src-var|cell-background>, <markup|decorated>...

    The optional argument <src-arg|effect> is an effect which is applied to
    the image (recoloring, skin...), as done in the pattern selector. In old
    documents it may also be a color: it then only stands for the image when
    patterns are switched off (<src-var|no-patterns>), in themes for
    instance, and it is not used otherwise.

    The optional argument <src-arg|base> is a color which is painted, opaque,
    under the image, so that the transparent parts of the image show it and
    not what lies behind. It is chosen with <samp|Base color> in the pattern
    selector. An empty <src-arg|effect> (<verbatim|""> in the source) can
    be used when there is only a base color. The base color is painted on
    the screen as in print. Without base color, a pattern which is nested in
    the document (in a cell, in <markup|decorated>...) is transparent. A
    pattern which makes the background of a page is painted on the paper:
    the background color <src-var|bg-color> of the document if it is a
    plain color, and the default of <src-var|bg-color> otherwise, so that
    a transparent image does not show what the screen held before, and looks
    as in the PDF.

    Older versions of TeXmacs ignore the <src-arg|base> argument, and the
    <src-arg|effect> too when there is one.
  </explain>

  <\explain>
    <var-val|color|black><explain-synopsis|foreground color>
  <|explain>
    The current foreground color of text and graphics, as specified in
    <menu|Document|Color|Foreground> or <menu|Format|Color>. Named colors,
    like ``<verbatim|Salmon>'', are supported for different color charts:
    <hlink|<verbatim|dvips>|https://en.wikibooks.org/wiki/LaTeX/Colors#The_68_standard_colors_known_to_dvips>,
    <hlink|<verbatim|x11>|https://en.wikipedia.org/wiki/X11_color_names#Color_name_charts>
    and <hlink|<verbatim|html>|https://www.w3schools.com/colors/colors_names.asp>.
  </explain>

  <\explain>
    <var-val|preamble|false><explain-synopsis|edit source tree?>
  <|explain>
    This flag determines whether we are editing normal text or a style-sheet.
    The source tree or preamble mode may be selected in
    <menu|Document|Source|Edit source tree>.
  </explain>

  <\explain>
    <var-val|info-flag|short><explain-synopsis|informative flags style>
  <|explain>
    This variable controls the rendering of informative flags, which are for
    instance used to indicate the locations of otherwise invisible labels or
    typesetting directives. The <src-var|info-flag> may take the values
    <verbatim|none>, <verbatim|short> and <verbatim|detailed>:

    <\tm-fragment>
      <with|info-flag|none|Label 1<label|flag-label-1>>,
      <with|info-flag|short|Label 2<label|flag-label-2>>,
      <with|info-flag|detailed|Label 3<label|flag-label-3>>.
    </tm-fragment>

    <\tm-fragment>
      <inactive*|<with|info-flag|none|Label 1<label|flag-label-1>>,
      <with|info-flag|short|Label 2<label|flag-label-2>>,
      <with|info-flag|detailed|Label 3<label|flag-label-3>>.>
    </tm-fragment>

    Usually, the rendering of informative flags is specified document-wide in
    <menu|Document|Informative flags>.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>