<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Typesetting tables>

  <section|Overview>

  Tables are typeset by the module <verbatim|Typeset/Table/>, with the
  classes <cpp|table_rep> and <cpp|cell_rep> declared in
  <source-link|Typeset/Table/table.hpp|src/Typeset/Table/table.hpp>. A table is not handled by the bridges:
  it is always typeset as a whole, as part of the paragraph containing it.
  There are three entry points in <source-link|Typeset/Table/table.cpp|src/Typeset/Table/table.cpp>:

  <\explain>
    <cpp|box typeset_as_table (edit_env env, tree t, path
    ip)><explain-synopsis|table as a single box>
  <|explain>
    Used by <cpp|concater_rep::typeset_table> for tables inside a line (the
    usual case for <markup|tabular>, matrices and similar environments). It
    returns one box.
  </explain>

  <\explain>
    <cpp|array\<less\>box\<gtr\> typeset_as_var_table (edit_env env, tree t,
    path ip)><explain-synopsis|table as a list of rows>
  <|explain>
    Used by <cpp|typeset_concat_or_table> when a paragraph consists of a
    table. If the table variable <verbatim|table-hyphen> is not
    <verbatim|n>, the table is returned as one box per row, so that the
    paragraph formatter can put each row on its own line and the page
    breaker can break the table across pages. Otherwise a single box is
    returned.
  </explain>

  <\explain>
    <cpp|lazy make_lazy_table (edit_env env, tree t, path
    ip)><explain-synopsis|lazy table>
  <|explain>
    Used when a table occurs in vertical material handled through lazy
    structures (floats, multi-paragraph cells, GUI containers). The
    <cpp|lazy_table_rep> answers width queries with
    <cpp|table_rep::compute_width> and produces a vertical stream of rows
    once the available width is known; a width given in <verbatim|par>
    units is resolved at that moment.
  </explain>

  When an inline table is wider than the limit
  <cpp|env-\<gtr\>table_max> (the environment variable
  <verbatim|math-table-limit>) and has no explicit width, and all its cells
  are simple, <cpp|concater_rep::typeset_table> gives up the
  two-dimensional layout and typesets the cells inline, separated by commas
  and semicolons, so that the result can be broken across lines.

  <section|The phases of table typesetting>

  All entry points perform the same sequence of steps (the lazy version
  postpones the last ones until <cpp|produce> is called):

  <\cpp-code>
    table T (env);

    T-\<gtr\>typeset (t, ip);

    T-\<gtr\>handle_decorations ();

    T-\<gtr\>handle_span ();

    T-\<gtr\>merge_borders ();

    T-\<gtr\>position_columns (true);

    T-\<gtr\>finish_horizontal ();

    T-\<gtr\>position_rows ();

    T-\<gtr\>finish ();\ \ \ \ \ \ \ \ \ \ // or var_finish ()
  </cpp-code>

  <subsection|Typesetting the cells>

  <cpp|table_rep::typeset> strips the enclosing <markup|tformat> tags and
  accumulates their <markup|twith> and <markup|cwith> formatting
  directives (together with the inherited <verbatim|cell-format> of the
  environment) into a single <markup|tformat>. <cpp|format_table> reads
  the table variables (<verbatim|table-width>, <verbatim|table-hmode>,
  paddings, borders, alignments, <verbatim|table-hyphen>, origin).
  <cpp|typeset_table> and <cpp|typeset_row> then create a <cpp|cell> for
  each entry, with the variables <verbatim|cell-row-nr> and
  <verbatim|cell-col-nr> set in the environment; <cpp|extract_format>
  selects the formatting directives which apply to each row and cell.

  <cpp|cell_rep::typeset> reads the cell variables (<cpp|format_cell>),
  applies the non-<verbatim|cell-> <markup|cwith> variables to the
  environment (<cpp|cell_local_begin>), and typesets the content:

  <\itemize>
    <item>A <markup|subtable> becomes a nested <cpp|table> stored in the
    field <cpp|T> of the cell.

    <item>If the cell variable <verbatim|cell-hyphen> is <verbatim|n> (the
    default), the content is typeset on one line with
    <cpp|typeset_as_concat>, optionally corrected vertically according to
    <verbatim|cell-vcorrect>.

    <item>Otherwise the cell may contain several lines or paragraphs. It is
    turned into a lazy structure (<cpp|make_lazy>) with a paragraph width of
    <verbatim|1par> and justified mode; the actual line breaking is done
    later, in <cpp|cell_rep::finish_horizontal>, once the column width is
    known.
  </itemize>

  A cell may also have a <verbatim|cell-decoration>, a small table with a
  <markup|tmarker> which surrounds the cell; it is typeset as a table
  <cpp|D> with status 1.

  <subsection|Decorations, spans and borders>

  <cpp|handle_decorations> expands decorated cells: the rows and columns
  of the decoration tables are inserted into the main table, so that
  decorations are aligned with the other cells. <cpp|handle_span> clears the
  cells covered by cells with <verbatim|cell-row-span> or
  <verbatim|cell-col-span> larger than one. <cpp|merge_borders> makes the
  borders of adjacent cells consistent: each border is the maximum of the
  borders declared by the cells on both sides.

  <subsection|Horizontal positioning>

  <cpp|position_columns (bool large)> determines the widths of the columns.
  <cpp|compute_widths> asks each cell for three quantities with
  <cpp|cell_rep::compute_width>: its total width <cpp|mw> and, for cells
  aligned on an anchor (upper case alignments such as <verbatim|L>,
  <verbatim|C>, <verbatim|R>, or alignment on a named position inside the
  cell), the widths <cpp|lw> and <cpp|rw> to the left and right of that
  anchor. The width of a column is the maximum over its cells; spanning
  cells enlarge their first column if necessary. For multi-paragraph cells,
  the width is obtained by querying the lazy structure with
  <cpp|make_query_vstream_width>, <abbr|i.e.> the natural width of its
  contents.

  If the table has a prescribed width (<verbatim|table-width> with a mode
  other than <verbatim|auto>), the extra space is distributed over the
  columns by the static function <cpp|blow_up>, proportionally to the
  <verbatim|cell-hpart> values of the columns, and without making a column
  wider than its natural width unless all columns have reached it. Nested
  tables are then positioned with the width of their column, and the
  horizontal alignment <verbatim|table-halign> determines the horizontal
  offset of the table.

  <cpp|finish_horizontal> then performs the line breaking of
  multi-paragraph cells (by producing a box from their lazy structure with
  a <cpp|format_cell>) and computes the horizontal position of each cell
  inside its column with <cpp|position_horizontally>.

  <subsection|Vertical positioning and final boxes>

  <cpp|position_rows> does the same for the rows: <cpp|compute_heights>
  collects heights (and depths and heights with respect to the baseline
  for anchored vertical alignments), <cpp|blow_up> distributes extra height
  according to <verbatim|cell-vpart>, and <verbatim|table-valign> fixes the
  vertical position of the table with respect to the baseline. For
  instance the alignment <verbatim|f>, used when
  <verbatim|table-valign> is not set, centers the table on the fraction
  bar height of the font.

  Finally, <cpp|finish> calls <cpp|cell_rep::finish> for each cell, which
  wraps the content in a <cpp|cell_box> drawing background and borders, and
  assembles the cells into a <cpp|table_box> (or a <cpp|composite_box> for
  exotic alignments), wrapped in further <cpp|cell_box>es for the table
  borders and paddings. <cpp|var_finish> instead produces one box per row
  for tables which may be broken across pages.

  <section|Remarks>

  <\itemize>
    <item>Since tables are not bridged, editing a cell re-typesets the whole
    paragraph containing the table, hence the whole table. For very large
    tables this is the main performance cost of the typesetter.

    <item>The variables <cpp|row_origin> and <cpp|col_origin> (from
    <verbatim|table-row-origin> and <verbatim|table-col-origin>) select the
    reference row and column for the alignments <verbatim|O>.

    <item>The cell variable <verbatim|cell-swell> adds padding to cells with
    very tall content (<cpp|cell_rep::swell_padding>), mirroring the
    <verbatim|par-swell> mechanism of the stacker.
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
