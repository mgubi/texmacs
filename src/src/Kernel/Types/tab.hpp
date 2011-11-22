
/******************************************************************************
* MODULE     : tab.hpp
* DESCRIPTION: spacing
* COPYRIGHT  : (C) 1999  David Allouche
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TAB_H
#define TAB_H
#include "tree.hpp"

enum tab_kind { tab_all, tab_first, tab_last };

class tab_rep: public tm_obj<tab_rep> {
public:
  int pos;
  double weight;
  tab_kind kind;

  inline tab_rep () {}
  tab_rep (int pos, tree t);
};

class tab : public tm_ptr<tab_rep> {
public:
  inline tab (): tm_ptr<tab_rep> (tm_new<tab_rep> ()) {}
  inline tab (int pos, tree t): tm_ptr<tab_rep> (tm_new<tab_rep> (pos, t)) {}
};

#endif // defined TAB_H
