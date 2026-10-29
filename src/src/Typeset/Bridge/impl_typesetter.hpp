
/******************************************************************************
* MODULE     : impl_typesetter.hpp
* DESCRIPTION: Implementation of the main TeXmacs typesetting routines
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef IMPL_TYPESETTER_H
#define IMPL_TYPESETTER_H
#include "Bridge/bridge.hpp"

struct page_memo;
class typesetter_rep {
public:
  edit_env&    env;
  bridge       br;
  repaint_log  repaint;  // the areas to repaint (see boxes.hpp)
  array<box>       placed_boxes; // and, in the check mode, the boxes which
  array<rectangle> placed_rects; // logged in the last pass, with their areas
  array<brush> old_bgs;

  array<page_item> l;      // current lines
  stack_border     sb;     // border properties
  array<line_item> a;      // left surroundings
  array<line_item> b;      // right surroundings

  SI x1, y1, x2, y2;
  hashmap<string,tree> old_patch;
  bool paper;
  page_memo* kept_pages;   // the pages of the last pass (pager.hpp)
  bool collect;            // a complete pass with the bridges used again
  bool dropped;            // the last pass removed bindings which no
                           // bridge writes any more (drop_bindings)

  shove_memo shoves;        // memoized shoves

public:
  typesetter_rep (edit_env& env, tree et, path ip);
  ~typesetter_rep ();

  void insert_stack     (array<page_item> l, stack_border sb);
  void insert_parunit   (tree t, path ip);
  void insert_paragraph (tree t, path ip);
  void insert_surround  (array<line_item> a, array<line_item> b);
  void insert_marker    (tree st, path ip);

  void local_start   (array<page_item>& l, stack_border& sb);
  void local_end     (array<page_item>& l, stack_border& sb);

  void determine_page_references (box b);
  bool drop_bindings (array<tree> before, array<tree> pages);
  box  typeset ();
  box  typeset (SI& x1, SI& y1, SI& x2, SI& y2);
};

#endif // defined IMPL_TYPESETTER_H
