
/******************************************************************************
* MODULE     : typesetter.hpp
* DESCRIPTION: The result of typesetting a paragraph is
*              an instance of the paragraph class
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TYPESETTER_H
#define TYPESETTER_H
#include "boxes.hpp"
#include "env.hpp"
#include "array.hpp"

class typesetter_rep;
typedef typesetter_rep* typesetter;

typesetter new_typesetter (edit_env& env, tree et, path ip);
void       delete_typesetter (typesetter ttt);

void notify_assign      (typesetter ttt, path p, tree u);
void notify_insert      (typesetter ttt, path p, tree u);
void notify_remove      (typesetter ttt, path p, int nr);
void notify_split       (typesetter ttt, path p);
void notify_join        (typesetter ttt, path p);
void notify_assign_node (typesetter ttt, path p, tree_label op);
void notify_insert_node (typesetter ttt, path p, tree t);
void notify_remove_node (typesetter ttt, path p);
void exec_until         (typesetter ttt, path p);
box  typeset            (typesetter ttt, SI& x1, SI& y1, SI& x2, SI& y2);
box  typeset            (typesetter ttt);  // the whole document, as a box

// The work done for an edit, measured when the environment variable
// TEXMACS_EDIT_PROFILE is set: the times (ms) of the stages of the last
// typesetting pass and what they handled; the editor prints them with the
// area it invalidated and repainted (edit_interface.cpp, edit_repaint.cpp).
struct edit_profile_data {
  bool   on;
  double bridges, pages, changes;  // ms: the bridges, the pager, the change log
  int    passes;                   // typesetting passes of this edit
  int    cached, redone;           // bridges whose result was reused or not
  int    lines, moved;             // lines (and remembered boxes) logged, changed
  int    rects;                    // rectangles in the change log
  int    breaks_reused;            // searches of page breaks avoided
};
extern edit_profile_data edit_profile;
double edit_profile_now ();        // ms, from an arbitrary origin

box        typeset_as_concat (edit_env env, tree t, path ip);
box        typeset_as_box (edit_env env, tree t, path ip);
box        typeset_as_atomic (edit_env env, tree t, path ip);
box        typeset_as_stack (edit_env env, tree t, path ip);
box        typeset_as_table (edit_env env, tree t, path ip);
array<box> typeset_as_var_table (edit_env env, tree t, path ip);
box        typeset_as_paragraph (edit_env e, tree t, path ip);
box        typeset_as_document (edit_env e, tree t, path ip);
tree       box_info (edit_env env, tree t, string what);

#endif // defined TYPESETTER_H
