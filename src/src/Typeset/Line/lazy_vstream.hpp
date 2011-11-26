
/******************************************************************************
* MODULE     : lazy_vstream.hpp
* DESCRIPTION: Make lines of a vstream from a typesetted concatenation
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LAZY_VSTREAM_H
#define LAZY_VSTREAM_H
#include "formatter.hpp"
#include "Format/page_item.hpp"
#include "Format/stack_border.hpp"

struct lazy_vstream_rep: public lazy_rep {
  tree             channel;  // "" or tuple with type&params of floating object
  array<page_item> l;        // the page items in the stream
  stack_border     sb;       // border properties

public:
  lazy_vstream_rep (path ip, tree ch, array<page_item> l, stack_border sb);
  operator tree ();
  lazy produce (lazy_type request, format fm);
  /*
  void format_vstream ();
  format query (lazy_type request, format fm);
  */
};

struct lazy_vstream : public tm_ext_null_ptr<lazy_vstream_rep, lazy> {
public:
  lazy_vstream(lazy_vstream_rep *p=NULL) : tm_ext_null_ptr<lazy_vstream_rep, lazy>(p) {}
  lazy_vstream(lazy& l) : tm_ext_null_ptr<lazy_vstream_rep, lazy>(l) {} 
  inline lazy_vstream (path ip, tree ch, array<page_item> l, stack_border sb):
    tm_ext_null_ptr<lazy_vstream_rep, lazy> (tm_new<lazy_vstream_rep> (ip, ch, l, sb)) {  }
};

#endif // defined LAZY_VSTREAM_H
