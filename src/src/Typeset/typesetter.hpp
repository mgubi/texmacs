
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

// The work done for an edit, measured when the environment variable
// TEXMACS_EDIT_PROFILE is set: the times (ms) of the stages of the last
// typesetting pass and what they handled; the editor prints them with the
// area it invalidated and repainted (edit_interface.cpp, edit_repaint.cpp).
struct edit_profile_data {
  bool   on;
  double bridges, pages, changes;  // ms: the bridges, the pager, the change log
  int    passes;                   // typesetting passes of this edit
  int    cached, redone;           // bridges whose result was reused or not
  int    checked;                  // of the redone: typeset again by the check
  int    lines;                    // lines (and remembered boxes) logged
  int    rects;                    // rectangles in the change log
  int    breaks_reused;            // searches of page breaks avoided
  double search;                   // ms: the search of the page breaks
  int    starts, starts_kept;      // starts of pages tried, with kept candidates
  int    pages_made;               // pages put together
  int    texts_kept, parts_kept;   // their texts, headers and footers kept
};
extern edit_profile_data edit_profile;
// the differences found by the check modes (TEXMACS_TYPESET_READS,
// TEXMACS_PAGE_BREAK_CACHE, TEXMACS_PAGE_BREAK_FAST, TEXMACS_PAGE_REUSE,
// TEXMACS_TYPESET_CACHES set to "check") since the start: a test fails
// when there are any
extern int typeset_check_mismatches;
int get_typeset_check_mismatches ();
// the bridges typeset since the start (not used again), for the tests
extern int typeset_redone_total;
int get_typeset_redone_count ();
// the switch of the caches of the typesetter which are not described by
// one of the variables above (block replay, chunks of lines, shoves, the
// prepared environment), TEXMACS_TYPESET_CACHES:
// 0 ("off"): nothing is used again, 1: used again, 2 ("check"): made
// again and compared, a difference counting in typeset_check_mismatches
int typeset_caches_mode ();
void typeset_cache_mismatch (string what); // (counts and reports it)
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
