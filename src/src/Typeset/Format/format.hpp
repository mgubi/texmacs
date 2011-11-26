
/******************************************************************************
* MODULE     : format.hpp
* DESCRIPTION: standard formats for placing material
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef FORMAT_H
#define FORMAT_H
#include "formatter.hpp"
#include "Format/line_item.hpp"

struct format_none_rep: public format_rep {
  format_none_rep ():
    format_rep (FORMAT_NONE) {}
  format_none_rep (format_type ft):
    format_rep (ft) {}
  bool equal (format fm);
  operator tree ();
};

typedef tm_ext_null_ptr<format_none_rep, format> format_none;
#if 0
struct format_none : public tm_ext_null_ptr<format_none_rep, format> {
  EXTEND_NULL(format,format_none);
};
EXTEND_NULL_CODE(format,format_none);
#endif

struct format_width_rep: public format_rep {
  SI width;
  format_width_rep (SI width2): format_rep (FORMAT_WIDTH), width (width2)
  {  }
  bool equal (format fm);
  operator tree ();
};

typedef tm_ext_null_ptr<format_width_rep, format> format_width;

#if 0
struct format_width {
  EXTEND_NULL(format,format_width);
};
EXTEND_NULL_CODE(format,format_width);
#endif

struct format_cell_rep: public format_rep {
  SI  width;
  int vpos;
  SI  depth;
  SI  height;
  format_cell_rep (SI w2, int v2, SI d2, SI h2):
    format_rep (FORMAT_CELL),
    width (w2), vpos (v2), depth (d2), height (h2)
  {  }
  bool equal (format fm);
  operator tree ();
};

typedef tm_ext_null_ptr<format_cell_rep, format> format_cell;
#if 0
struct format_cell {
  EXTEND(format,format_cell);
};
EXTEND_CODE(format,format_cell);
#endif

struct format_vstream_rep: public format_rep {
  SI width;
  array<line_item> before;
  array<line_item> after;
  format_vstream_rep (SI w2, array<line_item> bef2, array<line_item> aft2):
    format_rep (FORMAT_VSTREAM), width (w2), before (bef2), after (aft2)
  {  }
  bool equal (format fm);
  operator tree ();
};

typedef tm_ext_null_ptr<format_vstream_rep, format> format_vstream;
#if 0
struct format_vstream {
  EXTEND(format,format_vstream);
};
EXTEND_CODE(format,format_vstream);
#endif

struct query_vstream_width_rep: public format_rep {
  array<line_item> before;
  array<line_item> after;
  query_vstream_width_rep (array<line_item> bef2, array<line_item> aft2):
    format_rep (QUERY_VSTREAM_WIDTH), before (bef2), after (aft2)
  {  }
  bool equal (format fm);
  operator tree ();
};

typedef tm_ext_null_ptr<query_vstream_width_rep, format> query_vstream_width;

#if 0
struct query_vstream_width {
  EXTEND(format,query_vstream_width);
};
EXTEND_CODE(format,query_vstream_width);
#endif

format make_format_vstream (SI w, array<line_item> bef, array<line_item> aft);
format make_query_vstream_width (array<line_item> bef, array<line_item> aft);

#endif // defined FORMAT_H
