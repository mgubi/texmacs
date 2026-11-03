<TeXmacs|1.0.3.11>

<style|tmdoc>

<\body>
  <tmdoc-title|Miscellaneous environment variables>

  The following miscellaneous environment variables are mainly intended for
  internal use:

  <\explain>
    <var-val|save-aux|true><explain-synopsis|save auxiliary content>
  <|explain>
    This flag specifies whether auxiliary content has to be saved along with
    the document.
  </explain>

  <\explain>
    <var-val|zoom-factor|1><explain-synopsis|zoom factor>
  <|explain>
    The zoom factor which is used for displaying the document on the
    screen. It is usually not saved with the document.
  </explain>

  <\explain>
    <var-val|length-mode|magnified><explain-synopsis|are lengths
    magnified?>
  <|explain>
    When set to <verbatim|fixed>, absolute lengths are no longer multiplied
    by the current <hlink|<src-var|magnification>|env-general.en.tm#magnification>.
  </explain>

  <\explain>
    <var-val|window-bars|auto>

    <var-val|scroll-bars|true><explain-synopsis|window decorations>
  <|explain>
    When <src-var|window-bars> is set to <verbatim|false> (<abbr|resp.>
    <verbatim|true>) in the initial environment of a document, the menu bar,
    icon bars and footer of windows showing the document are hidden
    (<abbr|resp.> shown), overriding the user preferences. When
    <src-var|scroll-bars> is <verbatim|false>, no scroll bars are shown.
  </explain>

  <\explain>
    <var-val|global-title|>

    <var-val|global-author|>

    <var-val|global-subject|><explain-synopsis|document metadata>
  <|explain>
    Global metadata of the document, which can be specified in
    <menu|Document|Metadata> and which is used for instance for the metadata
    of generated <name|Pdf> files.
  </explain>

  <\explain>
    <var-val|warn-missing|true><explain-synopsis|warn about missing
    references>
  <|explain>
    Whether references to undefined labels should be recorded as missing
    during a complete typesetting of the document, so that they can be
    reported to the user.
  </explain>

  <\explain>
    <var-val|no-patterns|false><explain-synopsis|disable patterns>
  <|explain>
    When set to <verbatim|true>, pattern and gradient values of color
    variables (like <src-var|color>, <src-var|bg-color>,
    <src-var|fill-color> and the ornament colors) are replaced by plain
    colors.
  </explain>

  <\explain>
    <src-var|the-label>

    <src-var|the-tags>

    <src-var|the-modules><explain-synopsis|internal bookkeeping>
  <|explain>
    The variable <src-var|the-label> contains the text to be associated to
    the next label, <src-var|the-tags> the list of tags (like index or
    glossary tags) which currently apply, and <src-var|the-modules> the list
    of <scheme> modules and plug-ins which are needed by the style.
  </explain>

  <\explain>
    <src-var|cursor-color>, <src-var|math-cursor-color>,
    <src-var|selection-color>, <src-var|table-selection-color>,
    <src-var|focus-color>, <src-var|context-color>, <src-var|match-color>,
    <src-var|spell-error-color>, <src-var|clickable-color>,
    <src-var|correct-color>, <src-var|incorrect-color><explain-synopsis|colors
    for the user interface>
  <|explain>
    These variables determine the colors of the cursor, selections, focus
    and context rectangles, search matches, spelling errors, clickable
    regions and semantic correctness indicators, when editing the document.
    They are typically modified by themes.
  </explain>

  <\explain>
    <src-var|keyword-color>, <src-var|constant-color>,
    <src-var|number-color>, <src-var|string-color>,
    <src-var|operator-color>, <src-var|comment-color>,
    <src-var|preprocessor-color>, <src-var|modifier-color>,
    <src-var|declaration-color>, <src-var|macro-color>,
    <src-var|function-color>, <src-var|type-color>,
    <src-var|defined-color>, <src-var|misc-lexeme-color>,
    <src-var|alt-keyword-color>, <src-var|alt-constant-color><explain-synopsis|syntax
    highlighting>
  <|explain>
    Colors which are used for the syntax highlighting of source code in the
    various programming languages.
  </explain>

  <\explain>
    <src-var|canvas-type>, <src-var|canvas-color>,
    <src-var|canvas-hpadding>, <src-var|canvas-vpadding>,
    <src-var|canvas-bar-width>, <src-var|canvas-bar-padding>,
    <src-var|canvas-bar-color><explain-synopsis|canvases>
  <|explain>
    These variables control the rendering of the <markup|canvas> primitive
    (scrollable regions inside documents): the kind of scroll bars (the
    default <verbatim|plain> means none), the background color, the padding,
    and the width, distance and color of the scroll bars.
  </explain>

  <\explain>
    <src-var|ornament-shape>, <src-var|ornament-title-style>,
    <src-var|ornament-border>, <src-var|ornament-swell>,
    <src-var|ornament-corner>, <src-var|ornament-hpadding>,
    <src-var|ornament-vpadding>, <src-var|ornament-color>,
    <src-var|ornament-extra-color>, <src-var|ornament-sunny-color>,
    <src-var|ornament-shadow-color><explain-synopsis|ornaments>
  <|explain>
    These variables control the rendering of the <markup|ornament>
    primitive, which is used for framed and decorated environments: the
    shape of the frame and the style of its title, the border width and
    swell, the rounding of the corners, the padding around the body, the
    background colors of the body and the title, and the colors of the
    sunny and shadowed sides of the border.
  </explain>

  <\explain>
    <var-val|par-no-first|false><explain-synopsis|disable first indentation
    for next paragraph?>
  <|explain>
    This flag disables first indentation for the next paragraph.
  </explain>

  <\explain>
    <src-var|cell-format><explain-synopsis|current cell format>
  <|explain>
    This variable is used during the typesetting of tables in order to store
    the with-settings which apply to the current cell.
  </explain>

  <\explain>
    <src-var|atom-decorations>

    <src-var|line-decorations>

    <src-var|page-decorations>

    <src-var|xoff-decorations>

    <src-var|yoff-decorations><explain-synopsis|auxiliary variables for
    decorations>
  <|explain>
    These environment variables store auxiliary information during the
    typesetting of decorations.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>