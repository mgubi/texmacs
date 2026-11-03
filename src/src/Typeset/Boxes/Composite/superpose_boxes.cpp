
/******************************************************************************
* MODULE     : superpose.cpp
* DESCRIPTION: Superpositions of arrays of boxes
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"

/******************************************************************************
* The superpose_box representation
******************************************************************************/

struct superpose_box_rep: public concrete_composite_box_rep {
  superpose_box_rep (path ip, array<box> bs, bool bfl):
    concrete_composite_box_rep (ip, bs, bfl) {}
  operator tree ();
  int reindex (int i, int item, int n);
  box adjust_kerning (int mode, double factor);
  box expand_glyphs (int mode, double factor);
  gr_selections graphical_select (SI x, SI y, SI dist);
  int find_child (SI x, SI y, SI delta, bool force);
};

superpose_box_rep::operator tree () {
  int i, n= N(bs);
  tree t (TUPLE, n+1);
  t[0]= "superpose";
  for (i=0; i<n; i++) t[i+1]= (tree) bs[i];
  return t;
}

int
superpose_box_rep::reindex (int i, int item, int n) {
  (void) item; (void) n;
  return i;
}

box
superpose_box_rep::adjust_kerning (int mode, double factor) {
  int i, n= N(bs);
  array<box> adj (n);
  for (i=0; i<n; i++)
    adj[i]= bs[i]->adjust_kerning (mode, factor);
  return superpose_box (ip, adj, border_flag);
}

box
superpose_box_rep::expand_glyphs (int mode, double factor) {
  int i, n= N(bs);
  array<box> adj (n);
  for (i=0; i<n; i++)
    adj[i]= bs[i]->expand_glyphs (mode, factor);
  return superpose_box (ip, adj, border_flag);
}

gr_selections
superpose_box_rep::graphical_select (SI x, SI y, SI dist) {
  gr_selections res;
  int i, n= subnr();
  for (i=n-1; i>=0; i--)
    res << bs[i]->graphical_select (x- sx(i), y- sy(i), dist);
  return res;
}

// the child a click goes to: the closest one, and among those as close the
// one drawn last, on top of the others (a text over an image gets the click,
// not the image under it: the labels of a TikZ picture, plugins/tikz)
int
superpose_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  if (border_flag && outside (x, delta, x1, x2) &&
      (is_accessible (ip) || force)) return -1;
  int i, n= subnr(), m= -1;
  SI d= MAX_SI;
  for (i=n-1; i>=0; i--)
    if (distance (i, x, y, delta) < d)
      if (bs[i]->accessible () || force) {
        d= distance (i, x, y, delta);
        m= i;
      }
  return m;
}

/******************************************************************************
* User interface
******************************************************************************/

box
superpose_box (path ip, array<box> bs, bool bfl) {
  return tm_new<superpose_box_rep> (ip, bs, bfl);
}
