
/******************************************************************************
* MODULE     : bridge.hpp
* DESCRIPTION: Bridge between logical and physically typesetted document
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BRIDGE_H
#define BRIDGE_H
#include "typesetter.hpp"
#include "Concat/concater.hpp"
#include "Stack/stacker.hpp"
#include "Format/page_item.hpp"
#include "Page/vpenalty.hpp"
#include "Page/skeleton.hpp"
#include "Page/pager.hpp"

#define VALID_MASK        1
#define CORRUPTED         0
#define PROCESSED         1

#define WANTED_MASK       2
#define WANTED_PARAGRAPH  0
#define WANTED_PARUNIT    2

#define MACRO_ASSIGN      0
#define MACRO_INSERT      1
#define MACRO_REMOVE      2

class bridge;
struct chunk_cache_rep;
class bridge_rep: public abstract_struct {
public:
  typesetter           ttt;      // the underlying typesetter
  edit_env&            env;      // the environment
  tree                 st;       // the present subtree
  path                 ip;       // source location of the paragraph
  int                  status;   // status among above values
  hashmap<string,tree> changes;  // changes in the environment
  array<int>           reads;    // the variables read by the typesetting
                                 // (sorted env_var_id)
  array<string>        seen_keys;   // the references and attachments it
  array<tree>          seen_values; // looked up, with the values found
  unsigned long long   seen_stamp;  // the labels_stamp when they were found
                                    // again (0: not yet; see same_lookups)
  // what it was typeset from, when the inputs are used (see "What is
  // typeset again" in bridge.cpp)
  bool                 in_known; // all its inputs are known:
  array<int>           in_vars;  // the variables read before written
  unsigned long long   in_vals;  // a hash of their values (inputs_hash)
  unsigned long long   in_state; // the state derived from the environment
  unsigned long long   in_items; // the line items it got from around it
                                 // (items_signature)
  array<box>           in_item_boxes; // (those which are not markers)
  unsigned long long   in_fp;    // the fingerprint of the environment
  bool                 in_zoom;  // and whether it read the zoom factor,
  tree                 in_zoom_at; // at this zoom (see env_table::zoom)
  array<tree>          out_bindings; // the bindings it wrote, in order

  array<page_item>     l;        // the typesetted lines of st
  stack_border         sb;       // border properties of l
  link_repository      link_env; // loci and links declared inside bridge

  int                  version;  // incremented whenever l is recomputed
  int                  serial;   // number of the bridge, in the order of
                                 // creation (never the one of another)
  chunk_cache_rep*     chunk_cache; // see chunk_lines in bridge.cpp

public:
  bridge_rep (typesetter ttt, tree st, path ip);
  virtual ~bridge_rep ();

  virtual void notify_assign (path p, tree u) = 0;
  virtual void notify_insert (path p, tree u);
  virtual void notify_remove (path p, int nr);
  virtual void notify_split  (path p);
  virtual void notify_join   (path p);
  virtual bool notify_macro  (int type, string var, int l, path p, tree u) = 0;
  virtual void notify_change () = 0;

  virtual void my_clean_links ();
  virtual void my_exec_until (path p);
  virtual bool my_typeset_will_be_complete ();
  virtual void my_typeset (int desired_status);
  virtual void exec_until (path p, bool skip_flag= false);
  // how typeset handles the lines; a block of paragraphs (bridge_document)
  // stacks them where they go instead of in a stack of its own
  virtual void my_resync ();          // st and ip as in the edit tree
  virtual bool my_reusable ();        // may what was made be used again?
  virtual bool my_check_cache ();     // typeset again what is used again?
  virtual void my_stack_start ();     // before typesetting the subtree
  virtual void my_stack_end ();       // after typesetting it
  virtual void my_insert (bool made); // into the stack of the typesetter
  void typeset (int desired_status);
  bool same_inputs ();
  void give_record ();
  bool typeset_by_inputs (int desired_status); // (made again?)
};


// do two lists of page items make the same boxes (the check modes)?
bool same_page_items (array<page_item> l1, array<page_item> l2);

// the inputs of a bridge below for the record of the bridge above it
void add_inputs (edit_env env, bool known, array<int> vars,
                 unsigned long long vals);
// do the references and attachments looked up still have these values?
// ('stamp': the labels_stamp of the tables when they last had them, or 0)
bool same_lookups (edit_env env, array<string> keys, array<tree> values,
                   unsigned long long& stamp);

class bridge {
  ABSTRACT_NULL(bridge);
  bool operator == (bridge br2);
  bool operator != (bridge br2);
  friend bridge make_bridge (typesetter ttt, tree st, path ip);
  friend void replace_bridge (bridge& br, tree st, path ip);
  friend void replace_bridge (bridge& br, path p, tree ot, tree nt, path ip);
};
ABSTRACT_NULL_CODE(bridge);

bridge make_bridge (typesetter ttt, tree t, path p);
tm_ostream& operator << (tm_ostream& out, bridge br);
extern bridge nil_bridge;
tree substitute (tree t, path p, tree u);

#include "impl_typesetter.hpp"

#endif // defined BRIDGE_H
