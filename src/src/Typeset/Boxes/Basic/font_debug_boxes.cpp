
/******************************************************************************
* MODULE     : font_debug_boxes.cpp
* DESCRIPTION: what the font system did with the text of typeset boxes
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boxes.hpp"
#include "font.hpp"
#include "analyze.hpp"
#include "iterator.hpp"

/******************************************************************************
* For the font inspector and the font report. A text box records the font
* and the string it was typeset with; the smart font answers, for a
* character, which route it took, from its routing tables. Nothing here is
* resolved or cached anew, and nothing is called while typesetting.
******************************************************************************/

// The glyph at the box path bp of the root box: the glyph before the
// position (as for a cursor), or after it (as under the mouse)
tree
box_font_debug_info (box root, path bp, bool after) {
  tree r (TUPLE);
  if (is_nil (root) || is_nil (bp)) return r;
  box leaf= root[path_up (bp)];
  int type= leaf->get_type ();
  if (type != TEXT_BOX && type != SHORTER_BOX) {
    r << tuple ("box", "not a text box") << tuple ("box-type", as_string (type));
    return r;
  }
  string s = leaf->get_leaf_string ();
  font   fn= leaf->get_leaf_font ();
  if (N(s) == 0) return r;
  int pos= min (max (last_item (bp), 0), N(s));
  if ((!after || pos == N(s)) && pos > 0) tm_char_backwards (s, pos);
  r= smart_font_debug_info (fn, s, pos);
  r << tuple ("string", s) << tuple ("position", as_string (pos));
  return r;
}

static string
info_field (tree info, string key) {
  for (int i=0; i<N(info); i++)
    if (is_tuple (info[i]) && N(info[i]) == 2 && info[i][0] == key &&
        is_atomic (info[i][1]))
      return info[i][1]->label;
  return "";
}

static void
font_debug_collect (box b, hashmap<tree,int>& h) {
  int type= b->get_type ();
  if (type == TEXT_BOX || type == SHORTER_BOX) {
    string s = b->get_leaf_string ();
    font   fn= b->get_leaf_font ();
    int   pos= 0;
    while (pos < N(s)) {
      int end= pos;
      if (s[pos] == '<') tm_char_forwards (s, end); else end= pos + 1;
      if (end <= pos) end= pos + 1;
      tree info= smart_font_debug_info (fn, s, pos);
      tree key= tuple (info_field (info, "origin"), info_field (info, "char"),
                       info_field (info, "family"),
                       info_field (info, "subfont-name"),
                       info_field (info, "pdf"));
      key << info_field (info, "variant") << info_field (info, "series")
          << info_field (info, "shape");
      h (key)= h[key] + 1;
      pos= end;
    }
    return;
  }
  int n= b->subnr ();
  for (int i=0; i<n; i++) font_debug_collect (b->subbox (i), h);
}

// Every character of the typeset boxes under root, counted by route:
// tuples (origin char family subfont pdf variant series shape count)
tree
box_font_debug_report (box root) {
  tree r (TUPLE);
  if (is_nil (root)) return r;
  hashmap<tree,int> h (0);
  font_debug_collect (root, h);
  iterator<tree> it= iterate (h);
  while (it->busy ()) {
    tree key= it->next ();
    tree e= copy (key);
    e << as_string (h[key]);
    r << e;
  }
  return r;
}
