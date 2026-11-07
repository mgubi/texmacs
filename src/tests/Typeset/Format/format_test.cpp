
/******************************************************************************
* MODULE     : format_test.cpp
* DESCRIPTION: tests of the reference counts of the formats of the typesetter
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "tm_test.hpp"
#include "Format/format.hpp"
#include "Boxes/construct.hpp"

// a format made by make_format_... is held by its handle only, and goes
// with it (the formats of the cells of tables with hyphenation, made at
// every typesetting, were never freed)
static void
test_single_handle () {
  array<line_item> none;
  format fn= make_format_none ();
  format fw= make_format_width (10);
  format fc= make_format_cell (10, 0, 0, 0);
  format fv= make_format_vstream (10, none, none);
  format fq= make_query_vstream_width (none, none);
  CHECK_EQ (fn->ref_count, 1);
  CHECK_EQ (fw->ref_count, 1);
  CHECK_EQ (fc->ref_count, 1);
  CHECK_EQ (fv->ref_count, 1);
  CHECK_EQ (fq->ref_count, 1);
}

static void
test_copies () {
  format fm= make_format_cell (10, 0, 0, 0);
  {
    format copy= fm;
    CHECK_EQ (fm->ref_count, 2);
  }
  CHECK_EQ (fm->ref_count, 1);
}

// the line items before and after a vertical stream are given back when
// its format goes
static void
test_items_released () {
  line_item item (STD_ITEM, OP_SKIP, empty_box (path ()), 0);
  array<line_item> before;
  before << item;
  array<line_item> none;
  {
    format fm= make_format_vstream (10, copy (before), none);
    format q = make_query_vstream_width (copy (before), none);
    CHECK_EQ (item->ref_count, 4);
  }
  CHECK_EQ (item->ref_count, 2);
}

int
main () {
  RUN (test_single_handle);
  RUN (test_copies);
  RUN (test_items_released);
  return test_report ();
}
