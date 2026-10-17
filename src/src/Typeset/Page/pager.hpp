
/******************************************************************************
* MODULE     : pager.cpp
* DESCRIPTION: Places typesetted paragraphs on pages
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PAGER_H
#define PAGER_H
#include "typesetter.hpp"
#include "Format/line_item.hpp"
#include "Format/page_item.hpp"
#include "Format/stack_border.hpp"
#include "Page/skeleton.hpp"

// What the pager made in the last pass, kept by the typesetter for the
// next one (make_pages.cpp, "The pages of the last pass"): the headers and
// footers, with what their typesetting read, and the texts of the pages,
// with the boxes they are made of.
struct page_part {
  bool          ok;      // what follows is known
  tree          what;    // the header or footer which was typeset
  array<string> vars;    // the variables it read, and their values
  array<tree>   values;
  array<string> keys;    // the references it looked up, and their values
  array<tree>   looked;
  array<tree>   bindings; // the bindings it wrote (a label in a title)
  box           b;       // the result
  inline page_part (): ok (false) {}
};

struct page_text {
  bool          ok;
  array<box>    bs;      // the lines
  array<double> nums;    // their spaces, the heights and stretches
  array<tree>   ts;      // the kinds of the insertions
  box           b;       // the result
  inline page_text (): ok (false) {}
};

struct page_memo {
  array<double>     pars;   // the parameters of the pager
  unsigned long long state; // the variables from which the environment
                            // derives a state (fonts, colors...)
  array<page_part>  headers;
  array<page_part>  footers;
  array<page_text>  texts;
};

class pager_rep {
public:
  path                 ip;
  edit_env             env;
  hashmap<string,tree> style;
  array<page_item>     l;

  bool         paper;
  int          quality;
  bool         show_hf;
  SI           text_width;
  SI           text_height;
  SI           width;
  SI           height;
  SI           odd;
  SI           even;
  SI           top;
  SI           bot;
  SI           dtop;
  SI           dbot;
  SI           may_extend;
  SI           may_shrink;
  SI           head_sep;
  SI           foot_sep;
  SI           col_sep;
  space        fn_sep;
  space        fnote_sep;
  SI           fnote_bl;
  space        float_sep;
  SI           mnote_sep;

  int          page_offset;
  SI           cur_top;
  array<box>   pages;
  page_memo*   memo;     // the pages of the last pass, or NULL

  array<box>   lines_bx;
  array<space> lines_ht;

protected: // making papyrus boxes
  array<page_item> pap_main;
  array<page_item> pap_fnote;

  void papyrus_fnote_float (array<page_item> from, array<page_item>& to);
  void papyrus_mcolumn (array<page_item>& l);
  void papyrus_make (array<page_item> l);

protected: // making page boxes
  box  pages_format (array<page_item> l, SI ht, SI tcor, SI bcor);
  box  pages_format (insertion ins);
  box  pages_format (pagelet pg);
  box  pages_make_page (pagelet pg);
  void memo_start ();
  box  make_part (array<page_part>* parts, tree what);
  void text_parts (pagelet pg, page_text& pt);
  void text_parts (insertion ins, page_text& pt);
  void pages_make ();
  void papyrus_make ();

public:
  pager_rep (path ip, edit_env env, array<page_item> l);

  //void start_page ();
  //void print (page_item item);
  //void end_page (bool flag);
  box   make_header (bool empty_flag);
  box   make_footer (bool empty_flag);
  brush make_background (bool empty_flag);
  void  adjust_margins (bool empty_flag);
  box   make_pages ();

  friend struct stacker_rep;
  friend box typeset_paragraph (edit_env env, tree t, path ip);
  friend box typeset_document (edit_env env, tree t, path ip);
};

typedef pager_rep* pager;

#endif // defined PAGER_H
