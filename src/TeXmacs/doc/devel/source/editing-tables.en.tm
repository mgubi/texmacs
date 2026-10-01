<TeXmacs|2.1.4>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Editing tables>

  <section|Representation>

  A table is a <markup|table> node whose children are <markup|row> nodes,
  whose children are <markup|cell> nodes. Formatting is attached by
  wrapping any of these nodes in a <markup|tformat> node: all children of a
  <markup|tformat> except the last one are formatting instructions, and the
  last one is the formatted table, row or cell (which may itself be a
  <markup|tformat>). Two kinds of instructions exist:

  <\description>
    <item*|<markup|twith>>(<verbatim|var>, <verbatim|val>) sets a property
    of the whole table, such as <verbatim|table-width> or
    <verbatim|table-min-rows>.

    <item*|<markup|cwith>>(<verbatim|i1>, <verbatim|i2>, <verbatim|j1>,
    <verbatim|j2>, <verbatim|var>, <verbatim|val>) sets a cell property for
    the rows <verbatim|i1> to <verbatim|i2> and the columns <verbatim|j1>
    to <verbatim|j2>. Positive indices count from 1 at the top or left,
    negative ones from -1 at the bottom or right, so that
    <verbatim|(1, -1, 2, 2)> means \Pthe whole second column\Q whatever the
    number of rows.
  </description>

  Tabular environments of the style files (<markup|tabular>,
  <markup|matrix>, <markup|eqnarray*>, ...) are macros whose body contains
  a <markup|tformat> applied to their argument; they supply default
  formats.

  Inside <verbatim|Edit/Modify/edit_table.cpp>, rows and columns are
  numbered from 0, and the <cpp|cwith> indices are converted with
  <cpp|with_decode> (positive index <math|k> becomes <math|k-1>, negative
  index <math|-k> becomes <math|n-k>). The public routines exported to
  <scheme> use the 1-based convention of the format.

  <section|Finding the table>

  All public routines start from the cursor. The protected helpers are

  <\description>
    <item*|<cpp|search_table ()>>The innermost <markup|table> around the
    cursor.

    <item*|<cpp|search_table (row, col)>>The same, and the coordinates of
    the cell containing the cursor (skipping <markup|tformat> nodes around
    rows and cells).

    <item*|<cpp|search_format ()>, <cpp|search_format (row, col)>>The
    <markup|tformat> around that table, if there is one. The second variant
    <em|inserts> an empty <markup|tformat> when there is none, so that the
    caller can add formats.

    <item*|<cpp|search_row (fp, row)>, <cpp|search_cell (fp, row,
    col)>>The paths of a row and of the content of a cell (inside the
    <markup|cell> node).
  </description>

  Most public routines call <cpp|search_format (row, col)> and silently do
  nothing when the cursor is not inside a table.

  <section|Formats>

  <\description>
    <item*|<cpp|table_get_format (fp)>>All formats which apply to the table:
    the inherited <verbatim|cell-format> of the environment followed by the
    instructions of the <markup|tformat> at <cpp|fp>.

    <item*|<cpp|table_set_format>, <cpp|table_get_format>,
    <cpp|table_del_format>>For a table property (<markup|twith>): set
    replaces an existing instruction, get returns the last one which
    applies or the environment value.

    <item*|The same with <cpp|I1>, <cpp|J1>, <cpp|I2>, <cpp|J2>>For cell
    properties (<markup|cwith>) over a rectangle. Getting returns the value
    of the last instruction whose rectangle contains the requested one;
    deleting removes the instructions whose rectangle is contained in it.

    <item*|<cpp|table_individualize (fp, var)>>Splits every
    <markup|cwith> for <cpp|var> which covers several cells into one
    instruction per cell; used before decorating.

    <item*|<cpp|table_format_center (fp, row, col)>>Rewrites all
    <markup|cwith> indices so that rows and columns before the cursor are
    counted from the start and those after it from the end
    (<scm|table-format-center>).
  </description>

  The exported routines <cpp|table_set_format (var, val)> and friends
  (<scm|table-set-format>, ...) act on the table around the cursor, and
  <cpp|cell_set_format (var, val)>, <cpp|cell_get_format> and
  <cpp|cell_del_format> (<scm|cell-set-format>, ...) on the selected cells,
  or, without a table selection, on the cell, row, column or whole table
  depending on the <em|cell mode> (<cpp|set_cell_mode>, one of
  <verbatim|cell>, <verbatim|row>, <verbatim|column>, <verbatim|table>).
  When a selection extends to the last row or column, the corresponding
  index is written as -1, so that the format also applies to rows or
  columns added later. After each change of a cell format,
  <cpp|table_correct_block_content> wraps or unwraps the cell contents in a
  <markup|document> according to the <verbatim|cell-block> and
  <verbatim|cell-hyphen> properties.

  <section|Rows, columns and extents>

  <cpp|table_insert (fp, row, col, nr_rows, nr_cols)> and
  <cpp|table_remove (fp, row, col, nr_rows, nr_cols)> are the primitives:
  they insert or remove rows and columns of empty cells and then shift the
  <markup|cwith> indices so that every format keeps applying to the same
  cells. Removing all rows or all columns destroys the table
  (<cpp|destroy_table>, which also removes an enclosing tabular macro or
  <markup|subtable>). On top of them:

  <\description>
    <item*|<cpp|table_insert_row (forward)>,
    <cpp|table_insert_column>>(<scm|table-insert-row>,
    <scm|table-insert-column>) Insert after or before the current one,
    within the limits <verbatim|table-max-rows> and
    <verbatim|table-max-cols>, and move the cursor there. These are the
    actions of <scm|structured-insert-vertical> and
    <scm|structured-insert-horizontal> inside tables
    (<verbatim|table/table-edit.scm>).

    <item*|<cpp|table_remove_row (forward, flag)>,
    <cpp|table_remove_column>>Remove the current or a neighbouring row or
    column, or destroy the table when the minimum
    (<verbatim|table-min-rows>, <verbatim|table-min-cols>) would be
    violated.

    <item*|<cpp|table_set_extents (rows, cols)>>(<scm|table-set-extents>)
    Resize to the given size, clamped to the limits.

    <item*|<cpp|table_nr_rows>, <cpp|table_nr_columns>,
    <cpp|table_which_row>, <cpp|table_which_column>,
    <cpp|table_which_cells>, <cpp|table_search_cell>,
    <cpp|table_go_to>>Queries and cursor movement, with 1-based indices
    (negative ones count from the end).
  </description>

  After a change of the shape, <cpp|table_resize_notify> calls the
  <scheme> hook <scm|table-resize-notify>, which does nothing by default and
  is overloaded by spreadsheets (<verbatim|dynamic/calc-table.scm>) to
  update their cells.

  <section|Creating tables>

  Tables are normally created through <scm|make>: when the macro
  definition of a tag contains a <markup|tformat> applied to one of its
  arguments, <cpp|make_compound> inserts the tag and then calls
  <cpp|make_table (1, 1)> inside it (see <hlink|making compound
  structures|editing-structure.en.tm>). <cpp|make_table (rows, cols)>
  inserts a <markup|tformat> around an empty table, enlarges it to the
  minimal size given by the formats, puts the table in a paragraph of its
  own when <verbatim|table-hyphen> or <verbatim|table-block> asks for it,
  and corrects the block contents. <cpp|make_subtable> (<scm|make-subtable>)
  replaces the contents of the current cell by a <markup|subtable>.

  <section|Cell spans, subtables and decorations>

  <cpp|table_bound (fp, row1, col1, row2, col2)> enlarges a rectangle of
  cells so that it contains every cell which spans into it
  (<verbatim|cell-row-span>, <verbatim|cell-col-span>); it is used when
  moving the cursor and when computing table selections.

  <cpp|table_get_subtable (fp, row1, col1, row2, col2)> extracts a
  rectangle of cells, together with the <markup|cwith> instructions which
  apply to it (renumbered relative to the rectangle); this is what is
  copied when a table selection is copied. <cpp|table_write_subtable (fp,
  row, col, subt)> writes such a subtable into an existing table at a
  given position, enlarging the table if needed and shifting the formats
  of the subtable; pasting a table selection inside a table uses it (see
  <hlink|selections and the clipboard|editing-search.en.tm>). Inside a
  <markup|calc-table>, the <scheme> function <scm|calc-table-renumber>
  adjusts the cell references of a spreadsheet before writing.

  <em|Decorations> (<cpp|table_row_decoration>,
  <cpp|table_column_decoration>, <scm|table-row-decoration>,
  <scm|table-column-decoration>) move a neighbouring row or column of
  cells into the <verbatim|cell-decoration> format of the current cells:
  the cell then displays a small table in which a <markup|tmarker> stands
  for its own content.

  <section|Deletion inside tables>

  The deletion algorithm of <hlink|inserting, deleting and making
  structure|editing-structure.en.tm> calls <cpp|back_table> when the
  cursor is just outside a table (it moves into the first or last cell)
  and <cpp|back_in_table> when it is at the border of a cell. The latter
  removes the current row if it is entirely empty, then the current column
  if it is entirely empty, then the whole table if all cells are empty
  (always within the minimum sizes); otherwise it moves to the previous or
  next cell, and finally out of the table.

  <section|Pitfalls>

  <\itemize>
    <item><cpp|search_format (row, col)> modifies the document: it inserts
    an empty <markup|tformat> when the table has none, even for pure
    queries such as <scm|table-which-row>, so a query may change the document.

    <item>In <cpp|table_get_limits> (<verbatim|Edit/Modify/edit_table.cpp:447>)
    the test for an unset maximum number of columns compares it with the
    minimum number of <em|rows>: <verbatim|if (j2\<less\>i1)> should read
    <verbatim|if (j2\<less\>j1)>. A table whose maximum number of columns
    is smaller than its minimum number of rows is treated as having no
    maximum number of columns, and conversely a maximum smaller than the
    minimum number of columns is not reset.

    <item>In <cpp|table_write_subtable> (<verbatim|edit_table.cpp:847>)
    the loop which skips <markup|tformat> nodes around a cell of the
    subtable indexes the cell with the arity of the <em|row>:
    <verbatim|subc= subc [N(subr)-1]> instead of
    <verbatim|subc [N(subc)-1]>. Pasting a subtable whose cells carry
    their own <markup|tformat> can therefore pick the wrong child or index
    out of range.

    <item><cpp|table_insert> and <cpp|table_remove> reuse their parameter
    <cpp|row> as the counter of the column loop
    (<verbatim|edit_table.cpp:467>, <verbatim|523>), and then use it again
    to shift the row indices of the formats. When rows and columns are
    inserted in the same call, which only happens through
    <cpp|table_set_extents> (<scm|table-set-extents>, used by the table
    size dialogs), the row shift is computed with the new number of rows
    instead of the insertion point. For example, a format for the
    rows 1 to -1 (a whole column) is shifted so that it
    no longer covers the added rows.

    <item>The selection branches of <cpp|cell_set_format> and
    <cpp|cell_del_format> do not check that <cpp|selection_get_subtable>
    found a table.
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
