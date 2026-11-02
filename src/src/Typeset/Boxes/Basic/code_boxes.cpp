
/******************************************************************************
* MODULE     : code_boxes.cpp
* DESCRIPTION: boxes for the lines of source code (the tag code-lines)
* COPYRIGHT  : (C) 2026  the TeXmacs contributors
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boxes.hpp"
#include "font.hpp"
#include "language.hpp"
#include "Boxes/construct.hpp"
#include "analyze.hpp"
#include "colors.hpp"

/******************************************************************************
* A code box shows the lines [i0, i1) of a document of strings (the child of
* <code-lines|<document|line|...>>) as one box, without a box per line or per
* token: the lines are at a fixed distance from each other (the pitch), the
* first baseline at 0, and nothing is made for a line until it is drawn or
* the cursor is in it. Its box paths are (k, pos): the line i0+k of the
* document, at the position pos in its string. The geometry of a line (the
* positions of the cursor in it) is the one of a text box of the line, made
* when it is needed; the colours of the syntax are asked to the language when
* the line is drawn, for the lines which are visible only.
*
* (prototype: no wrapping of long lines.)
******************************************************************************/

struct code_box_rep: public box_rep {
  tree     doc;    // the document of lines
  int      i0, i1; // the lines shown
  font     fn;
  pencil   pen;
  language lan;
  hashmap<string,color> cols; // the colours given as variables
  SI       pitch;  // the distance between two baselines

  code_box_rep (path ip, tree doc, int i0, int i1, font fn, pencil pen,
                language lan, hashmap<string,color> cols, SI pitch, SI width);
  operator tree () { return "code lines"; }

  int    nr_lines () { return i1 - i0; }
  string line (int k) {
    tree t= doc[i0+k];
    return is_atomic (t)? t->label: string (""); }
  path   line_ip (int k) { return descend (descend (ip, 0), i0+k); }
  SI     base (int k) { return -k * pitch; }
  box    line_box (int k) { return text_box (line_ip (k), 0, line (k), fn, pen); }
  int    line_at (SI y);

  // The lines are not children for what goes through all the children
  // (subnr is 0: drawing, positioning), but a box path (k, pos) leads to a
  // text box of the line k, made when it is asked for, at its place
  box    subbox (int k) {
    box lb= line_box (max (0, min (k, nr_lines () - 1)));
    lb->x0= 0; lb->y0= base (k);
    return lb; }

  void      display (renderer ren);
  path      find_box_path (SI x, SI y, SI delta, bool force, bool& found);
  path      find_lip ();
  path      find_rip ();
  path      find_left_box_path () { return path (0, 0); }
  path      find_right_box_path () {
    return path (nr_lines () - 1, N (line (nr_lines () - 1))); }
  path      find_box_path (path p, bool& found);
  path      find_tree_path (path bp);
  cursor    find_cursor (path bp);
  selection find_selection (path lbp, path rbp);
};

code_box_rep::code_box_rep (path ip2, tree doc2, int i02, int i12, font fn2,
                            pencil pen2, language lan2,
                            hashmap<string,color> cols2, SI pitch2, SI width):
  box_rep (ip2), doc (doc2), i0 (i02), i1 (max (i12, i02+1)),
  fn (fn2), pen (pen2), lan (lan2), cols (cols2), pitch (pitch2)
{
  x1= x3= 0;
  x2= x4= width;
  y1= y3= base (nr_lines () - 1) + fn->y1;
  y2= y4= fn->y2;
}

int
code_box_rep::line_at (SI y) {
  // the line whose band (half way between the baselines) contains y
  SI top= fn->y2 + (pitch - (fn->y2 - fn->y1)) / 2;
  int k= (int) ((top - (long long) y) / pitch);
  if (top < y) k= 0;
  return max (0, min (nr_lines () - 1, k));
}

/******************************************************************************
* Display: only the visible lines, coloured as the language says
******************************************************************************/

void
code_box_rep::display (renderer ren) {
  SI cx1, cy1, cx2, cy2;
  ren->get_clipping (cx1, cy1, cx2, cy2);
  int ka= line_at (cy2), kb= line_at (cy1);
  color fg= pen->get_color ();
  for (int k= ka; k <= kb; k++) {
    tree t= doc[i0+k];
    if (!is_atomic (t) || N(t->label) == 0) continue;
    string s= t->label;
    int n= N(s);
    STACK_NEW_ARRAY (xpos, SI, n+1);
    fn->get_xpositions (s, xpos);
    int pos= 0;
    while (pos < n) {
      int start= pos;
      (void) lan->advance (t, pos);
      if (pos > n) pos= n;
      if (pos <= start) pos= start + 1;
      if (pos - start == 1 && s[start] == ' ') continue;
      string col= lan->get_color (t, start, pos);
      color c= (col == ""? fg:
                 cols->contains (col)? cols[col]: named_color (col));
      ren->set_pencil (pencil (c));
      fn->draw (ren, s (start, pos), xpos[start], base (k));
    }
    STACK_DELETE_ARRAY (xpos);
  }
}

/******************************************************************************
* Cursor and selection, by the line and then by a text box of the line
******************************************************************************/

path
code_box_rep::find_box_path (SI x, SI y, SI delta, bool force, bool& found) {
  int k= line_at (y);
  box lb= line_box (k);
  path bp= lb->find_box_path (x, y - base (k), delta, force, found);
  found= true;
  return path (k, bp);
}

path
code_box_rep::find_lip () {
  return descend (line_ip (0), 0);
}

path
code_box_rep::find_rip () {
  int k= nr_lines () - 1;
  return descend (line_ip (k), N (line (k)));
}

path
code_box_rep::find_box_path (path p, bool& found) {
  // p: a path in the edit tree; the lines are at reverse (ip) * (0, i, pos)
  found= false;
  path me= reverse (ip);
  if (!is_accessible (ip)) return path (0, 0);
  if (N(p) == N(me) + 3 && path_up (p, 3) == me && p[N(me)] == 0) {
    int i= p[N(me)+1], pos= last_item (p);
    if (i >= i0 && i < i1) {
      found= true;
      return path (i - i0, max (0, min (pos, N (line (i - i0)))));
    }
  }
  if (N(p) == N(me) + 1 && path_up (p) == me) {
    found= true;
    if (last_item (p) == 0) return find_left_box_path ();
    return find_right_box_path ();
  }
  return path (0, 0);
}

path
code_box_rep::find_tree_path (path bp) {
  if (is_atom (bp)) {
    if (bp->item == 0) return reverse (find_lip ());
    return reverse (find_rip ());
  }
  int k= max (0, min (bp->item, nr_lines () - 1));
  int pos= is_nil (bp->next)? 0: bp->next->item;
  return reverse (descend (line_ip (k), max (0, min (pos, N (line (k))))));
}

cursor
code_box_rep::find_cursor (path bp) {
  if (is_atom (bp))
    bp= (bp->item == 0? find_left_box_path (): find_right_box_path ());
  int k= max (0, min (bp->item, nr_lines () - 1));
  path q= is_nil (bp->next)? path (0): bp->next;
  cursor cu= line_box (k)->find_cursor (q);
  cu->oy += base (k);
  return cu;
}

selection
code_box_rep::find_selection (path lbp, path rbp) {
  if (is_atom (lbp)) lbp= find_left_box_path ();
  if (is_atom (rbp)) rbp= find_right_box_path ();
  int k1= lbp->item, k2= rbp->item;
  if (k1 == k2) {
    selection sel= line_box (k1)->find_selection (lbp->next, rbp->next);
    return selection (translate (sel->rs, 0, base (k1)),
                      sel->start, sel->end);
  }
  if (k1 > k2) return find_selection (rbp, lbp);
  cursor c1= find_cursor (lbp), c2= find_cursor (rbp);
  SI top = fn->y2, bot= fn->y1;
  rectangles rs (rectangle (x1, base (k2) + bot, c2->ox, base (k2) + top));
  if (k2 > k1 + 1)
    rs= rectangles (rectangle (x1, base (k2 - 1) + bot, x2,
                               base (k1 + 1) + top), rs);
  rs= rectangles (rectangle (c1->ox, base (k1) + bot, x2, base (k1) + top),
                  rs);
  return selection (rs, find_tree_path (lbp), find_tree_path (rbp));
}

/******************************************************************************
* Interface
******************************************************************************/

box
code_box (path ip, tree doc, int i0, int i1, font fn, pencil pen,
          language lan, hashmap<string,color> cols, SI pitch, SI width) {
  return tm_new<code_box_rep> (ip, doc, i0, i1, fn, pen, lan, cols,
                               pitch, width);
}
