<TeXmacs|1.0.3.11>

<style|tmdoc>

<\body>
  <tmdoc-title|Table layout>

  The environment variables for tables can be subdivided in variables
  (prefixed by <src-var|table->) which apply to the whole table and those
  (prefixed by <src-var|cell->) which apply to individual cells. Whereas
  usual environment variables are set with <markup|assign> and <markup|with>,
  the tabular environment variables are rather set with the
  <hlink|<markup|tformat> primitive|../regular/prim-table.en.tm>. This
  makes it possible to apply certain settings to any rectangular subtable of
  the entire table and in particular to rows or columns. For more details,
  see the <hlink|documentation|../regular/prim-table.en.tm#table-twith>
  of the <markup|twith> and <markup|cwith> primitives.

  <paragraph*|Layout of the table as a whole>

  <\explain>
    <var-val|table-width|>

    <var-val|table-height|><explain-synopsis|hint for table dimensions>
  <|explain>
    These parameters indicate a hint for the dimensions of the table. The
    <src-var|table-hmode> and <src-var|table-vmode> variables determine how
    to take into account these settings.
  </explain>

  <\explain>
    <var-val|table-hmode|auto>

    <var-val|table-vmode|auto><explain-synopsis|determination of table
    dimensions>
  <|explain>
    These parameters specify how to determine the dimensions of the table.
    When no <src-var|table-width> is specified, the width is determined
    automatically from the contents (mode <verbatim|auto>). When
    <src-var|table-width> is specified, the possible values of
    <src-var|table-hmode> are <verbatim|exact> (the default in that case: the
    table gets exactly the specified width), <verbatim|min> (the table is at
    most as wide as <src-var|table-width>, but no wider than needed for its
    contents) and <verbatim|max> (the table is at least as wide as
    <src-var|table-width>). In the non-automatic modes, unused space is
    distributed over the columns according to <src-var|cell-hpart> (see
    below). The height is determined similarly using
    <src-var|table-height>, <src-var|table-vmode> and <src-var|cell-vpart>.
  </explain>

  <\explain>
    <var-val|table-halign|l>

    <var-val|table-valign|f><explain-synopsis|alignment inside text>
  <|explain>
    These parameters determine how the table should be aligned in the
    surrounding text. Possible values for <src-var|table-halign> are
    <verbatim|l> (left), <verbatim|c> (center) and <verbatim|r> (right), and
    possible values for <src-var|table-valign> are <verbatim|t> (top),
    <verbatim|f> (centered at fraction bar height), <verbatim|c> (center) and
    <verbatim|b> (bottom).

    In addition to the above values, the alignment can take place with
    respect to the baselines of particular cells. Such values for
    <src-var|table-halign> are <verbatim|L> (align <abbr|w.r.t.> the left
    column), <verbatim|C> (align <abbr|w.r.t.> the middle column),
    <verbatim|R> (align <abbr|w.r.t.> the right column) and <verbatim|O>
    (align <abbr|w.r.t.> the privileged origin column
    <src-var|table-col-origin>). Similarly, <src-var|table-valign> may take
    the additional values <verbatim|T> (align <abbr|w.r.t.> the top row),
    <verbatim|C> (align <abbr|w.r.t.> the middle row), <verbatim|B> (align
    <abbr|w.r.t.> the bottom row) and <verbatim|O> (align <abbr|w.r.t.> the
    privileged origin row <src-var|table-row-origin>).
  </explain>

  <\explain>
    <var-val|table-row-origin|0>

    <var-val|table-col-origin|0><explain-synopsis|privileged cell>
  <|explain>
    Table coordinates of a privileged ``origin cell'' which may be used for
    aligning the table in the surrounding text (see above). Rows and columns
    are numbered from <verbatim|1>; negative values count from the bottom
    <abbr|resp.> right of the table.
  </explain>

  <\explain>
    <var-val|table-lsep|0fn>

    <var-val|table-rsep|0fn>

    <var-val|table-bsep|0fn>

    <var-val|table-tsep|0fn><explain-synopsis|padding around table>
  <|explain>
    Padding around the table (in addition to the padding of individual
    cells).
  </explain>

  <\explain>
    <var-val|table-lborder|0ln>

    <var-val|table-rborder|0ln>

    <var-val|table-bborder|0ln>

    <var-val|table-tborder|0ln><explain-synopsis|border around table>
  <|explain>
    Border width for the table (in addition to borders of the individual
    cells).
  </explain>

  <\explain>
    <var-val|table-hyphen|n><explain-synopsis|allow for hyphenation?>
  <|explain>
    A flag which specifies whether page breaks may occur at the middle of
    rows in the table. When <src-var|table-hyphen> is set to <verbatim|y>,
    then such page breaks may only occur when

    <\enumerate>
      <item> The table is not surrounded by other markup in the same
      paragraph.

      <item>The rows where the page break occurs have no borders.
    </enumerate>

    An example of a tabular environment which allows for page breaks is
    <markup|eqnarray*>.
  </explain>

  <\explain>
    <var-val|table-min-rows|>

    <var-val|table-min-cols|>

    <var-val|table-max-rows|>

    <var-val|table-max-cols|><explain-synopsis|constraints on the table's
    size>
  <|explain>
    It is possible to specify a minimal and maximal numbers of rows or
    columns for the table. Such settings constraint the behaviour of the
    editor for operations which may modify the size of the table (like the
    insertion and deletion of rows and columns). This is particularly useful
    for tabular macros. For instance, <src-var|table-min-cols> and
    <src-var|table-max-cols> are both set to <with|mode|math|3> for the
    <markup|eqnarray*> environment.
  </explain>

  <paragraph*|Layout of the individual cells>

  <\explain>
    <var-val|cell-background|><explain-synopsis|background color>
  <|explain>
    A background color for the cell. Besides colors, patterns and gradients
    are also allowed; the special value <verbatim|foreground> stands for the
    current foreground color.
  </explain>

  <\explain>
    <var-val|cell-width|>

    <var-val|cell-height|><explain-synopsis|hint for cell dimensions>
  <|explain>
    Hints for the width and the height of the cell. The real width and height
    also depend on the modes <src-var|cell-hmode> and <src-var|cell-vmode>,
    possible filling (see <src-var|cell-hpart> and <src-var|cell-vpart>
    below), and, of course, on the dimensions of other cells in the same row
    or column.
  </explain>

  <\explain>
    <var-val|cell-hpart|>

    <var-val|cell-vpart|><explain-synopsis|fill part of unused space>
  <|explain>
    When the sum <with|mode|math|s> of the widths of all columns in a table
    is smaller than the width <with|mode|math|w> of the table itself, then it
    should be specified what should be done with the unused space. The
    <src-var|cell-hpart> parameter specifies a part in the unusued space
    which will be taken by a particular cell. The horizontal part taken by a
    column is the maximum of the horizontal parts of its composing cells. Now
    let <with|mode|math|p<rsub|i>> the so determined part for each column
    (<with|mode|math|i\<in\>{1,\<ldots\>,n}>). Then the extra horizontal
    space which will be distributed to this column is
    <with|mode|math|p<rsub|i>*(w-s)/(p<rsub|1>+\<cdots\>+p<rsub|n>)>. A
    similar computation determines the extra vertical space which is
    distributed to each row.
  </explain>

  <\explain>
    <var-val|cell-hmode|auto>

    <var-val|cell-vmode|auto><explain-synopsis|determination of cell
    dimensions>
  <|explain>
    These parameters specify how to determine the width and the height of the
    cell. If no <src-var|cell-width> is specified, the width is determined by
    the content (mode <verbatim|auto>). Otherwise, if <src-var|cell-hmode>
    is <verbatim|exact> (the default when a width is given), then the width
    is given by <src-var|cell-width>. If <src-var|cell-hmode> is
    <verbatim|min> or <verbatim|max>, then the width is the minimum
    <abbr|resp.> maximum of <src-var|cell-width> and the width of the
    content. The height is determined similarly.
  </explain>

  <\explain>
    <var-val|cell-halign|l>

    <var-val|cell-valign|B><explain-synopsis|cell alignment>
  <|explain>
    These parameters determine the horizontal and vertical alignment of the
    cell. Possible values of <src-var|cell-halign> are <verbatim|l> (left),
    <verbatim|c> (center) and <verbatim|r> (right). The upper case variants
    <verbatim|L>, <verbatim|C> and <verbatim|R> align the cells of a column
    <abbr|w.r.t.> a common vertical axis; such a value may be followed by a
    string at whose position the alignment takes place. For instance,
    <verbatim|L.> aligns on the decimal dot and <verbatim|L,> on the decimal
    comma. Possible values of <src-var|cell-valign> are <verbatim|t> (top),
    <verbatim|c> (center), <verbatim|b> (bottom) and <verbatim|B> (baseline);
    the less common values <verbatim|T> and <verbatim|C> align the cells of a
    row <abbr|w.r.t.> common horizontal axes near the top <abbr|resp.> the
    center.
  </explain>

  <\explain>
    <var-val|cell-lsep|1spc>

    <var-val|cell-rsep|1spc>

    <var-val|cell-bsep|1sep>

    <var-val|cell-tsep|1sep><explain-synopsis|cell padding>
  <|explain>
    The amount of padding around the cell (at the left, right, bottom and
    top).
  </explain>

  <\explain>
    <var-val|cell-lborder|0ln>

    <var-val|cell-rborder|0ln>

    <var-val|cell-bborder|0ln>

    <var-val|cell-tborder|0ln><explain-synopsis|cell borders>
  <|explain>
    The borders of the cell (at the left, right, bottom and top). The
    displayed border between cells <with|mode|math|T<rsub|i,j>> and
    <with|mode|math|T<rsub|i,j+1>> at positions <with|mode|math|(i,j)> and
    <with|mode|math|(i,j+1)> is the maximum of the borders between the right
    border of <with|mode|math|T<rsub|i,j>> and the left border of
    <with|mode|math|T<rsub|i,j+1>>. Similarly, the displayed border between
    cells <with|mode|math|T<rsub|i,j>> and <with|mode|math|T<rsub|i+1,j>> is
    the maximum of the bottom border of <with|mode|math|T<rsub|i,j>> and the
    top border of <with|mode|math|T<rsub|i+1,j>>.
  </explain>

  <\explain>
    <var-val|cell-vcorrect|a><explain-synopsis|vertical correction of text>
  <|explain>
    As described above, the dimensions and the alignment of a cell may depend
    on the dimensions of its content. When cells contain text boxes, the
    vertical bounding boxes of such text may vary as a function of the text
    (the letter ``k'' <abbr|resp.> ``y'' ascends <abbr|resp.> descends
    further than ``x''). Such differences sometimes leads to unwanted,
    non-uniform results. The vertical cell correction allows for a more
    uniform treatment of text of the same font, by descending and/or
    ascending the bounding boxes to a level which only depends on the font.
    Possible values for <src-var|cell-vcorrect> are <verbatim|n> (no vertical
    correction), <verbatim|b> (vertical correction of the bottom),
    <verbatim|t> (vertical correction of the top), <verbatim|a> (vertical
    correction of bottom and the top).
  </explain>

  <\explain>
    <var-val|cell-hyphen|n><explain-synopsis|allow for hyphenation inside
    cells>
  <|explain>
    By default, the cells contain inline content which is not hyphenated.
    The <src-var|cell-hyphen> variable, which can be set through
    <menu|Cell|Line wrapping>, determines whether and how the content is
    broken into several lines. Possible values are <verbatim|n> (disable line
    breaking) and <verbatim|b>, <verbatim|c> and <verbatim|t> (enable line
    breaking and align at the bottom, center <abbr|resp.> top line). When
    line breaking is enabled, the cell content is typeset as a paragraph
    whose width is the width of the cell.
  </explain>

  <\explain>
    <var-val|cell-block|auto><explain-synopsis|block content inside cells>
  <|explain>
    This variable determines whether the cell contains block content,
    <abbr|i.e.> whether its content is wrapped into a <markup|document> tag.
    Possible values are <verbatim|no>, <verbatim|yes> and <verbatim|auto>
    (block content if and only if line wrapping is enabled through
    <src-var|cell-hyphen>). The editor uses this setting in order to insert
    or remove the <markup|document> tags around cell contents.
  </explain>

  <\explain>
    <var-val|cell-swell|0ex><explain-synopsis|extra padding around large
    cells>
  <|explain>
    When positive, cells without line wrapping whose content is
    exceptionally high or deep receive additional vertical padding of at
    most <src-var|cell-swell> at the top <abbr|resp.> bottom (except at the
    top and bottom borders of the table). As for <src-var|par-swell>, the
    amount of padding depends on the thresholds
    <src-var|math-top-swell-start>, <src-var|math-top-swell-end>,
    <src-var|math-bot-swell-start> and <src-var|math-bot-swell-end>. This
    variable is used for instance by matrices.
  </explain>

  <\explain>
    <var-val|cell-row-span|1>

    <var-val|cell-col-span|1><explain-synopsis|span of a cell>
  <|explain>
    Certain cells in a table are allowed to span over other cells at their
    right or below them. The <src-var|cell-row-span> and
    <src-var|cell-col-span> specify the row span and column span of the cell.
  </explain>

  <\explain>
    <var-val|cell-decoration|><explain-synopsis|decorating table for cell>
  <|explain>
    This environment variable may contain a decorating table for the cell.
    Such a decoration enlarges the table with extra columns and cells. The
    <markup|tmarker> primitive determines the location of the original
    decorated cell and its surroundings in the enlarged table are filled up
    with the decorations. Cell decorations are not really used at present and
    may disappear in future versions of <TeXmacs>.
  </explain>

  <\explain>
    <var-val|cell-orientation|portrait><explain-synopsis|orientation of cell>
  <|explain>
    Other orientations for cells than <verbatim|portrait> have not yet been
    implemented.
  </explain>

  <\explain>
    <var-val|cell-row-nr|1>

    <var-val|cell-col-nr|1><explain-synopsis|current cell position>
  <|explain>
    During the typesetting of a table, these environment variables contain
    the position of the cell which is currently being typeset. Notice that
    the typesetter numbers rows and columns starting from <verbatim|0> here,
    contrary to the <markup|cwith> primitive.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>