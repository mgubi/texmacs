
/******************************************************************************
* MODULE     : bridge_rewrite.cpp
* DESCRIPTION: Bridge between logical and physical long macro expansions
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bridge.hpp"

/******************************************************************************
* The bridge_rewrite_rep class
******************************************************************************/

class bridge_rewrite_rep: public bridge_rep {
protected:
  bridge body;
  tree   body_src;  // the rewritten tree of the body, before its ip was given
  path   body_ip;   // and the ip of the bridge then

public:
  bridge_rewrite_rep (typesetter ttt, tree st, path ip);
  void initialize (tree body_t);

  void notify_assign (path p, tree u);
  bool notify_macro  (int type, string var, int l, path p, tree u);
  void notify_change ();

  void my_typeset (int desired_status);
};

bridge_rewrite_rep::bridge_rewrite_rep (typesetter ttt, tree st, path ip):
  bridge_rep (ttt, st, ip) {}

void
bridge_rewrite_rep::initialize (tree body_t) {
  // the same rewritten tree as before (an included document, from the cache
  // of the inclusions) at the same place: the body is kept, and its bridges
  // decide by their inputs whether they are typeset again. It was made anew
  // each time (attach_right copies a detached tree), and no bridge of an
  // included file was used again. (A bridge whose path changes keeps its
  // path, shifted in place; without the inputs, the body is made again.)
  if (!is_nil (body) && typeset_inputs_mode () != 0 &&
      strong_equal (body_src, body_t) && strong_equal (body_ip, ip))
    return;
  body_src= body_t;
  body_ip = ip;
  if (is_nil (body)) body= make_bridge (ttt, attach_right (body_t, ip));
  else replace_bridge (body, attach_right (body_t, ip));
}

bridge
bridge_rewrite (typesetter ttt, tree st, path ip) {
  return tm_new<bridge_rewrite_rep> (ttt, st, ip);
}

/******************************************************************************
* Event notification
******************************************************************************/

void
bridge_rewrite_rep::notify_assign (path p, tree u) {
  // cout << "Assign " << p << ", " << u << " in " << st << "\n";
  status= CORRUPTED;
  st= substitute (st, p, u);
}

bool
bridge_rewrite_rep::notify_macro (int tp, string v, int l, path p, tree u) {
  (void) tp; (void) p; (void) u;
  bool flag= env->depends (st, v, l);
  if (flag) status= CORRUPTED;
  return flag;
}

void
bridge_rewrite_rep::notify_change () {
  status= CORRUPTED;
}

/******************************************************************************
* Typesetting
******************************************************************************/

void
bridge_rewrite_rep::my_typeset (int desired_status) {
  initialize (env->rewrite (st));
  ttt->insert_marker (st, ip);
  if (is_func (st, VAR_INCLUDE)) {
    url save_name= env->cur_file_name;
    url file_name= url_unix (env->exec_string (st[0]));
    url incl_file= relative (env->base_file_name, file_name);
    env->cur_file_name= incl_file;
    env->secure= is_secure (env->cur_file_name);
    {
      inclusion_guard guard (incl_file);
      body->typeset (desired_status);
    }
    env->cur_file_name= save_name;
    env->secure= is_secure (env->cur_file_name);
  }
  else body->typeset (desired_status);
}
