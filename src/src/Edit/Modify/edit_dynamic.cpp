
/******************************************************************************
* MODULE     : edit_dynamic.cpp
* DESCRIPTION: editing dynamic content
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "edit_dynamic.hpp"

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_dynamic_rep::edit_dynamic_rep () {}
edit_dynamic_rep::~edit_dynamic_rep () {}

/******************************************************************************
* Routines for potentially deactivated structures
******************************************************************************/

path
edit_dynamic_rep::find_dynamic (path p) {
  path parent= path_up (p);
  if (nil (parent)) return parent;
  if (drd->is_dynamic (subtree (et, parent))) return p;
  return find_dynamic (parent);
}

path
edit_dynamic_rep::find_deactivated (path p) {
  path parent= path_up (p);
  if (nil (parent)) return parent;
  if (is_func (subtree (et, parent), INACTIVE)) return parent;
  return find_deactivated (parent);
}

bool
edit_dynamic_rep::in_preamble_mode () {
  return get_env_string (PREAMBLE) == "true";
}

bool
edit_dynamic_rep::is_deactivated () {
  return !nil (find_deactivated (tp));
}

bool
edit_dynamic_rep::is_multi_paragraph_macro (tree t) {
  int n= arity (t);
  if (is_document (t) || is_func (t, PARAGRAPH) || is_func (t, SURROUND))
    return true;
  if (is_func (t, MACRO) || is_func (t, WITH))
    return is_multi_paragraph_macro (t [n-1]);
  if (is_extension (t) && (!is_compound (t, "footnote"))) {
    int i;
    for (i=1; i<n; i++)
      if (is_multi_paragraph_macro (t[i]))
	return true;
    tree f= get_env_value (t[0]->label);
    return is_multi_paragraph_macro (f);
  }
  return false;
}

static bool
contains_table_format (tree t, tree var) {
  if (is_atomic (t)) return false;
  else {
    int i, n= N(t);
    for (i=0; i<n; i++)
      if (contains_table_format (t[i], var))
	return true;
    return is_func (t, TABLE_FORMAT) && (t[N(t)-1] == tree (ARGUMENT, var));
  }
}

void
edit_dynamic_rep::activate_macro (path p, string name, tree f) {
  int n= N(f);
#ifdef WITH_EXTENSIONS
  tree r (make_tree_label (name), n-1);
#else
  tree r (EXPAND, n);
  r[0]= copy (name);
#endif
  assign (p, r);
  if (n == 1) go_to (end (et, p));
  else go_to (p * path (d_exp, 0));
  if ((n == 2) && contains_table_format (f[1], f[0]))
    make_table (1, 1);
  else if ((n == 2) && is_multi_paragraph_macro (f))
    ins_unary (p * d_exp, DOCUMENT);
  correct (path_up (p));
}

void
edit_dynamic_rep::activate () {
  path p= find_deactivated (tp);
  if (nil (p)) return;
  tree st= subtree (et, p * 0);

  if (is_func (st, SYMBOL, 1) && is_atomic (st[0])) {
    int i;
    string s= st[0]->label;
    if (is_int (s))
      assign (p, string (((char) as_int (s))));
    else {
      for (i=0; i<N(s); i++)
	if ((s[i]=='<') || (s[i]=='>'))
	  { s= ""; break; }
      assign (p, "<" * s * ">");
    }
    go_to (end (et, p));
    correct (path_up (p));
    return;
  }

  if ((is_func (st, LATEX, 1) || (is_func (st, HYBRID, 1))) &&
      is_atomic (st[0])) {
    string  s= st[0]->label;
    string  help;
    command cmd;
    if (kbd_get_command (s, help, cmd)) {
      cut (p * 0, p * 1);
      cmd ();
      return;
    }
    else if (is_func (st, HYBRID, 1)) {
      if (is_atomic (st[0])) {
	string name= st[0]->label;
	path mp= search_upwards (MACRO);
	if (!nil (mp)) {
	  tree mt= subtree (et, mp);
	  int i, n= N(mt)-1;
	  for (i=0; i<n; i++)
	    if (mt[i] == name) {
	      assign (p, tree (ARGUMENT, copy (name)));
	      go_to (end (et, p));
	      correct (path_up (p));
	      return;
	    }
	}

	tree f= get_env_value (name);
	if (is_func (f, MACRO)) {
	  activate_macro (p, name, f);
	  return;
	}
	else if (is_func (f, FUNCTION)) {
	  int n= N(f);
	  tree r (APPLY, n);
	  r[0]= copy (name);
	  st= r;
	}
	else st= tree (VALUE, copy (name));
      }
      else st= tree (APPLY, copy (st[0]));
    }
  }

  assign (p, st);
  go_to (end (et, p));
  correct (path_up (p));
}

/******************************************************************************
* Making dynamic objects
******************************************************************************/

void
edit_dynamic_rep::make_active (string op, int n) {
  int i;
  tree_label l= as_tree_label (op);
  tree t (l, n);
  for (i=0; i<n; i++) t[i]= "";

  if (n == 0) insert_tree (t);
  else if (selection_active_small ()) {
    t[0]= selection_get_cut ();
    if (n == 1) insert_tree (t, path (0, end (t[0])));
    else insert_tree (t, path (1, 0));
  }
  else insert_tree (t, path (0, 0));

  if (drd->get_arity (l) < 0)
    set_message ("tab: insert argument", op);
}

void
edit_dynamic_rep::make_deactivated (tree t, path p) {
  if (in_preamble_mode ()) insert_tree (t, p);
  else insert_tree (tree (INACTIVE, t), path (0, p));
}

void
edit_dynamic_rep::make_deactivated (string op, int n, string rf, string arg) {
  int i, k= (arg==""? 0: 1);
  tree_label l= as_tree_label (op);
  tree t (l, n);
  for (i=0; i<n; i++) t[i]= "";
  if (n>0) t[0]= arg;

  if (selection_active_small () && (n>k)) {
    path p (k+1, 0);
    t[k]= selection_get_cut ();
    if (n==k+1) p= path (k, end (t[k]));
    make_deactivated (t, p);
  }
  else make_deactivated (t, path (k, 0));

  if (drd->get_arity (l) < 0)
    set_message ("tab: insert argument, return: activate", rf);
  else set_message ("return: activate", rf);
}

void
edit_dynamic_rep::insert_argument () {
  path p= find_dynamic (tp);
  if (nil (p)) return;
  if (p==tp) {
    p= find_dynamic (path_up (tp));
    if (nil (p)) return;
  }
  tree t= subtree (et, path_up (p));
  if (is_func (t, HYBRID)) {
    if (is_atomic (t[0])) {
      tree f= get_env_value (t[0]->label);
      int n= N(f);
      if (n == 1) return;
      if (is_func (f, MACRO)) {
	p= path_up (p, 2);
	if (!is_func (subtree (et, p), INACTIVE)) return;
	activate_macro (p, t[0]->label, f);
      }
      else if (is_func (f, FUNCTION)) {
	p= path_up (p);
	tree r (APPLY, n);
	r[0]= copy (t[0]);
	assign (p, r);
	go_to (p * path (1, 0));
      }
    }
    return;
  }
  if (drd->get_arity (L(t)) >= 1) return;
  if (is_func (t, WITH) || is_func (t, ATTR)) {
    int at= ((last_item (p) >> 1) << 1) + 2;
    if (at > N(t)) at= N(t);
    insert (path_up (p) * at, tree (L (t), "", ""));
    go_to (path_up (p) * path (at, 0));
  }
  else {
    insert (path_inc (p), tree (L (t), ""));
    go_to (path_inc (p) * 0);
  }
}

/******************************************************************************
* Inserting active dynamic objects
******************************************************************************/

bool
edit_dynamic_rep::make_return_before () {
  bool flag;
  path q= tp;
  while (!is_document (subtree (et, path_up (q)))) q= path_up (q);
  flag= (N (subtree (et, path_up (q))) == (q->item+1)) || (tp != end (et, q));
  if (flag) {
    flag= insert_return ();
    go_to (end (et, q));
  }
  return flag;
}

bool
edit_dynamic_rep::make_return_after () {
  path q= tp;
  while (!is_document (subtree (et, path_up (q)))) q= path_up (q);
  if (tp == start (et, q)) return false;
  return insert_return ();
}

void
edit_dynamic_rep::make_assign (tree var, tree by) {
  insert_tree (tree (CONCAT, tree (ASSIGN, var, by)));
}

static tree
remove_changes_in (tree t, string var) {
  if (is_atomic (t)) return t;
  else if (is_func (t, WITH)) {
    int i, n=N(t), k=(n-1)>>1;
    if (k==1) {
      tree r= remove_changes_in (t[2], var);
      if (t[0] != var) r= tree (WITH, t[0], t[1], r);
      return simplify_correct (r);
    }
    tree r (WITH);
    for (i=0; i<k; i++)
      if (t[i<<1] != var) r << t[i<<1] << t[(i<<1)+1];
    r << remove_changes_in (t[i<<1], var);
    return simplify_correct (r);
  }
  else if (is_format (t) || is_func (t, SURROUND)) {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= remove_changes_in (t[i], var);
    return simplify_correct (r);
  }
  else return t;
}

void
edit_dynamic_rep::make_with (string var, string val) {
  if (selection_active_normal ()) {
    tree t= remove_changes_in (selection_get (), var);
    selection_cut ();
    insert_tree (tree (WITH, var, val, t), path (2, end (t)));
  }
  else insert_tree (tree (WITH, var, val, ""), path (2, 0));
}

bool
edit_dynamic_rep::make_big_expand (string name) {
  tree body;
  bool active= selection_active_normal ();
  if (active) {
    body= selection_get ();
    selection_cut ();
    if (!is_document (body)) body= tree (DOCUMENT, body);
  }
  else body= tree (DOCUMENT, "");

  /*** These lines will become obsolete ***/
  if (name != "footnote") {
    if (make_return_after ()) return true;
    if (make_return_before ()) return true;
  }
  /* ----------------------------------*/

  int  i= N(body)-1;
  path p (d_exp, path (i, end (body[i])));
  insert_tree (compound (name, body), p);
  return false;
}

void
edit_dynamic_rep::make_expand (string s, int n) {
#ifdef WITH_EXTENSIONS
  tree ins (make_tree_label (s), n);
#else
  tree ins (EXPAND, n+1);
  ins[0]= s;
#endif
  if (n==0) insert_tree (ins, path (d_exp));
  else if (n==1) {
    tree f= get_env_value (s);
    if ((N(f) == 2) && contains_table_format (f[1], f[0])) {
      tree sel= "";
      if (selection_active_small ()) sel= selection_get_cut ();
      insert_tree (ins, path (d_exp, 0));
      make_table (1, 1);
      if (sel != "") insert_tree (sel, end (sel));
    }
    else if (selection_active_normal ()) {
      ins[d_exp]= selection_get_cut ();
      insert_tree (ins, path (d_exp, end (ins[d_exp])));
    }
    else insert_tree (ins, path (d_exp, 0));
  }
  else insert_tree (ins, path (d_exp, 0));
}

void
edit_dynamic_rep::temp_proof_fix () {
  /* this routine should be removed as soon as possible */
  path p = search_upwards_expand ("proof");
  if (nil (p) || (N(tp) < N(p)+2)) return;
  path q = head (tp, N(p)+2);
  tree st= subtree (et, path_up (q));
  if ((!is_document (st)) || (last_item (q) != (N(st)-1))) return;
  insert (path_inc (q), tree (DOCUMENT, ""));
}

void
edit_dynamic_rep::make_apply (string s) {
  tree t (APPLY, s);
  insert_tree (t);
}

/******************************************************************************
* Deleting dynamic objects
******************************************************************************/

void
edit_dynamic_rep::back_dynamic (path p) {
  if (is_func (subtree (et, path_up (p)), INACTIVE))
    go_to (end (et, p * (N (subtree (et, p))- 1)));
  else {
    string s= get_label (subtree (et, p));
    ins_unary (p, INACTIVE);
    set_message ("return: reactivate", "delete#" * s);
  }
}

void
edit_dynamic_rep::back_expand (path p) {
  if (is_func (subtree (et, path_up (p)), INACTIVE)) back_dynamic (p);
  else {
    tree st= subtree (et, p);
    int n= N(st);
    if (n==1) {
      assign (p, "");
      correct (path_up (p));
    }
    else if ((n==2) &&
	     ((is_func (st[1], TABLE_FORMAT) || is_func (st[1], TABLE))))
      back_table (p * 1);
    else go_to (end (et, p * (n-1)));
  }
}

void
edit_dynamic_rep::back_hide_expand (path p) {
  if (is_func (subtree (et, path_up (p)), INACTIVE)) back_dynamic (p);
  else go_to (end (et, p * (N (subtree (et, p)) - 2)));
}

void
edit_dynamic_rep::back_extension (path p) {
  if (is_func (subtree (et, path_up (p)), INACTIVE)) back_dynamic (p);
  else {
    tree st= subtree (et, p);
    int n= N(st);
    if (n==0) {
      assign (p, "");
      correct (path_up (p));
    }
    else if ((n==1) &&
	     ((is_func (st[0], TABLE_FORMAT) || is_func (st[0], TABLE))))
      back_table (p * 1);
    else go_to (end (et, p * (n-1)));
  }
}

static bool
is_empty (tree t, int at, int nr) {
  int i;
  if ((at+nr > N(t)) || ((at % nr) != 0)) return false;
  for (i=at; i < at+nr; i++)
    if (t[i] != "") return false;
  return true;
}

void
edit_dynamic_rep::back_in_dynamic (tree t, path p, int min_args, int step) {
  int node= last_item (p);
  if (node>0) {
    go_to (end (et, path_up (p) * (node-1)));
    if (N(t) > min_args) {
      if (is_empty (t, node, step))
	remove (path_up (p) * node, step);
      else go_to (end (et, path_dec (p)));
    }
  }
  else {
    int i;
    for (i=0; i<N(t); i++)
      if (t[i] != tree ("")) {
	go_to (start (et, path_up (p)));
	return;
      }
    assign (path_up (p), "");
    if (subtree (et, path_up (p, 2)) == tree (INACTIVE, "")) {
      assign (path_up (p, 2), "");
      correct (path_up (p, 3));
    }
    else correct (path_up (p, 2));
  }
}

void
edit_dynamic_rep::back_in_with (tree t, path p) {
  if (is_func (subtree (et, path_up (p, 2)), INACTIVE) || in_preamble_mode ())
    back_in_dynamic (t, p, 1, 2);
  else if (t[N(t)-1] == "") {
    assign (path_up (p), "");
    correct (path_up (p, 2));
  }
  else go_to (start (et, path_up (p)));
}

void
edit_dynamic_rep::back_in_expand (tree t, path p) {
  if (is_func (subtree (et, path_up (p, 2)), INACTIVE) || in_preamble_mode ())
    back_in_dynamic (t, p, 1);
  else {
    int node= last_item (p);
    if (node>1) go_to (end (et, path_up (p) * (node-1)));
    else {
      int i;
      for (i=1; i<N(t); i++)
	if ((t[i] != "") && (t[i] != tree (DOCUMENT, ""))) {
	  go_to (start (et, path_up (p)));
	  return;
	}
      assign (path_up (p), "");
      correct (path_up (p, 2));
    }
  }
}

void
edit_dynamic_rep::back_in_extension (tree t, path p) {
  if (is_func (subtree (et, path_up (p, 2)), INACTIVE) || in_preamble_mode ())
    back_in_dynamic (t, p, 1);
  else {
    int node= last_item (p);
    if (node>0) go_to (end (et, path_up (p) * (node-1)));
    else {
      int i;
      for (i=0; i<N(t); i++)
	if ((t[i] != "") && (t[i] != tree (DOCUMENT, ""))) {
	  go_to (start (et, path_up (p)));
	  return;
	}
      assign (path_up (p), "");
      correct (path_up (p, 2));
    }
  }
}
