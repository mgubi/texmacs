
/******************************************************************************
* MODULE     : evaluate_main.cpp
* DESCRIPTION: evaluation of macro constructs
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "evaluate_main.hpp"
#include "std_environment.hpp"
#include "drd_mode.hpp"
#include "drd_std.hpp"
#include "hashset.hpp"

/******************************************************************************
* Environment changes
******************************************************************************/

static tree
protect_parts (tree t) {
  // the residuals in t unquoted, the rest as it is (inside a quasiquote)
  if (is_residual (t)) return tree (UNQUOTE, t);
  if (is_atomic (t) || !contains_residual (t)) return t;
  int i, n= N(t);
  tree r (t, n);
  // (the lengths of a resize are read by the typesetter where it typesets
  // the box: they stay as they are)
  if (is_box_with_lengths (L(t))) {
    r[0]= protect_parts (t[0]);
    for (i=1; i<n; i++) r[i]= t[i];
    return r;
  }
  for (i=0; i<n; i++) r[i]= protect_parts (t[i]);
  return r;
}

tree
protect_value (tree val) {
  // the value of an assign or a with of the result: the typesetter
  // evaluates it again, which must give the value computed here (a quoted
  // <page-the-page> must stay unexpanded, for the pager), except for the
  // residuals, which the typesetter is to compute
  if (is_atomic (val) || is_residual (val)) return val;
  if (!contains_residual (val)) return tree (QUOTE, val);
  return tree (QUASIQUOTE, protect_parts (val));
}

list<string>* residual_assigned= NULL;

int scratch_depth= 0;
int branch_depth= 0;

static void
assigned_in (tree t, list<string>& r, hashset<string>& seen) {
  // the variables which t assigns by name, also through the macros which
  // it calls (each followed once); a name computed by the macro is not
  // known (see the limits of decision 1.7)
  if (is_atomic (t)) return;
  if (is_func (t, ASSIGN) && N(t) >= 1 && is_atomic (t[0]))
    r= list<string> (t[0]->label, r);
  string tag= "";
  if (L(t) >= START_EXTENSIONS) tag= as_string (L(t));
  else if (is_func (t, COMPOUND) && N(t) >= 1 && is_atomic (t[0]))
    tag= t[0]->label;
  if (N(tag) != 0 && !seen->contains (tag)) {
    seen->insert (tag);
    if (std_env->contains (tag)) {
      tree f= std_env [tag];
      if (is_func (f, MACRO) || is_func (f, XMACRO)) assigned_in (f, r, seen);
    }
  }
  for (int i=0; i<N(t); i++) assigned_in (t[i], r, seen);
}

list<string>
assigned_in_value (tree val) {
  // the variables which a value assigns when it is executed: the calls of
  // a macro, a macro chosen by a condition, a function of for-each
  list<string> r;
  hashset<string> seen;
  assigned_in (val, r, seen);
  return unique_names (r);
}

list<string>
assigned_in_macro (tree f) {
  if (is_atomic (f) && std_env->contains (f->label)) f= std_env [f->label];
  return assigned_in_value (f);
}

tree
evaluate_assign (tree t) {
  // an assignment stays in the result, with the place of the assignment of
  // the document: the typesetter reads variables of its own
  // (page-odd-header, for the pager), a value which holds a residual is
  // computed by the typesetter at this place (evaluate_value), and the
  // assignment, typeset invisibly as in the document, keeps its place for
  // the cursor (also when it defines a macro, whose calls are expanded)
  int i, n=N(t), k=n>>1;
  array<string> vars;
  array<tree>   vals;
  tree kept (CONCAT);
  for (i=0; i<k; i++) {
    tree name= evaluate (t[i<<1]);
    tree val = evaluate (t[(i<<1)+1]);
    if (contains_residual (name) || is_compound (name)) {
      // (a variable which the typesetter alone knows: its assignment;
      // which variable changes, the rewriter does not know; or a name
      // which is not one, the call of an undefined macro, to which the
      // typesetter assigns nothing, exec_assign: <compound|counter-x> in
      // the reset of a group of std-counter, where x is not an argument)
      kept << tree (ASSIGN, name, protect_value (val));
      continue;
    }
    string var= as_string (name);
    vars << var;
    vals << val;
    kept << tree (ASSIGN, var, protect_value (val));
    note_assigned (var);
    // (in a branch which the typesetter decides, a macro: the variables
    // which its calls assign, for those which the typesetter alone expands)
    if (branch_depth > 0 && !is_atomic (val))
      note_assigned (assigned_in_value (val));
  }
  if (N(vars) > 0) {
    assoc_environment local (N(vars));
    for (i=0; i<N(vars); i++) local->raw_write (i, vars[i], vals[i]);
    assign (std_env, local);
  }
  tree r= (N(kept) == 1? kept[0]: kept);
  transfer_ip (t, r);
  return r;
}

tree
evaluate_with (tree t) {
  int i, n=N(t), k=(n-1)>>1;
  assoc_environment local (k);
  tree w (WITH);
  bool source= false;
  for (i=0; i<k; i++) {
    tree name= evaluate (t[i<<1]);
    // (a variable which the typesetter alone knows: the with is its)
    if (contains_residual (name)) { (void) evaluate_left (t); return t; }
    string var= as_string (name);
    tree   val= evaluate (t[(i<<1)+1]);
    local->raw_write (i, var, val);
    w << var << protect_value (val);
    if (var == "preamble" && val == "true") source= true;
    // (whether it is the source mode, the typesetter alone knows)
    if (var == "preamble" && is_residual (val)) {
      (void) evaluate_left (t);
      return t;
    }
    // FIXME: don't add non-typesetter variables (?)
  }
  // (in source mode, the body of show-preamble, the typesetter shows the
  // markup as it is, without executing it: see make_bridge)
  if (source) { w << t[n-1]; return w; }
  begin_with (std_env, local);
  tree r= evaluate_part (t[n-1]);
  end_with (std_env);
  w << r;
  return w;
}

tree
evaluate_provides (tree t) {
  tree r= evaluate (t[0]);
  if (is_compound (r)) return evaluate_error ("bad provides");
  if (!std_env->contains (r->label)) return "false";
  // (a variable which a branch decided by the typesetter assigns: whether
  // it is defined, the typesetter knows)
  tree v= std_env [r->label];
  if (is_func (v, VALUE, 1) && v[0] == r) return tree (PROVIDES, r);
  return "true";
}

tree
evaluate_value (tree t) {
  tree r= evaluate (t[0]);
  // (a name which the typesetter alone knows: the value is its)
  if (is_compound (r) && contains_residual (r)) return tree (VALUE, r);
  if (is_compound (r)) return evaluate_error ("bad value");
  int key= make_tree_label (r->label);
  if (!std_env->contains (key)) return evaluate_error ("undefined", r);
  // a value which holds a residual is left to the typesetter, which
  // computed it where it was bound (a with or a kept assign): a copy of the
  // residual would be computed here, in another environment (box-info
  // typesets its argument again, lengths are converted with other fonts)
  if (contains_residual (std_env[key])) return tree (VALUE, r);
  return evaluate_result (std_env[key]);
}

tree
evaluate_quote_value (tree t) {
  tree r= evaluate (t[0]);
  if (is_compound (r)) return evaluate_error ("bad quoted value");
  int key= make_tree_label (r->label);
  if (!std_env->contains (key)) return evaluate_error ("undefined", r);
  // (a value which the typesetter alone knows: the quoted value is its)
  if (contains_residual (std_env[key])) return t;
  return std_env[key];
}

/******************************************************************************
* DRD properties
******************************************************************************/

tree
evaluate_drd_props (tree t) {
  (void) t; return "";
  // FIXME: not yet implemented
}

/******************************************************************************
* Syntactic decomposition
******************************************************************************/

tree
evaluate_get_label (tree t) {
  tree r= evaluate (t[0]);
  return copy (as_string (L(r)));  
}

tree
evaluate_get_arity (tree t) {
  tree r= evaluate (t[0]);
  return as_string (arity (r));
}

/******************************************************************************
* Classical macro expansion
******************************************************************************/

#ifdef CLASSICAL_MACRO_EXPANSION
tree
evaluate_compound (tree t) {
  int d; tree f;
  if (L(t) == COMPOUND) {
    d= 1;
    f= t[0];
    if (is_compound (f)) f= evaluate (f);
    if (is_atomic (f)) {
      string var= f->label;
      if (!std_env->contains (var)) return evaluate_error ("undefined", var);
      f= std_env [var];
    }
  }
  else {
    string var= as_string (L(t));
    if (!std_env->contains (var)) return evaluate_error ("undefined", var);
    d= 0;
    f= std_env [var];
  }

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(t)-d;
    assoc_environment local (L(f)==XMACRO? 1: n);
    if (L(f) == XMACRO)
      local->raw_write (0, as_string (f[0]), t);
    else {
      static tree uninit (UNINIT);
      for (i=0; i<n; i++)
	local->raw_write (i, as_string (f[i]), i<m? t[i+d]: uninit);
      //local->print ("");
    }
    macro_down (std_env, local);
    tree r= evaluate (f[n]);
    macro_up (std_env);
    return r;
  }
  else return evaluate (f);
}

tree
evaluate_arg (tree t) {
  tree r= t[0];
  if (is_compound (r)) return evaluate_error ("bad arg");
  int key= make_tree_label (r->label);
  if (macro_top_level (std_env)) return evaluate_error ("undefined", r);
  basic_environment local= macro_arguments (std_env);
  //local->print ("");
  if (!local->contains (key)) return evaluate_error ("undefined", r);
  macro_up (std_env);
  r= local[key];
  // (an index which does not apply gives the error of the typesetter
  // (edit_env_rep::exec_arg), not the argument: author-affiliation of
  // amsart.ts with an affiliation which is not a tuple)
  bool err= false;
  if (N(t) > 1) {
    int i, n= N(t);
    for (i=1; i<n; i++) {
      tree u= evaluate (t[i]);
      if (!is_int (u)) { err= true; break; }
      int nr= as_int (u);
      if ((!is_compound (r)) || (nr<0) || (nr>=N(r))) { err= true; break; }
      r= r[nr];
    }
  }
  if (err) r= evaluate_error ("arg " * t[0]->label);
  else r= evaluate (r);
  macro_redown (std_env, local);
  return r;
}

tree
evaluate_quote_arg (tree t) {
  tree r= t[0];
  if (is_compound (r)) return evaluate_error ("bad quoted arg");
  int key= make_tree_label (r->label);
  if (macro_top_level (std_env)) return evaluate_error ("undefined", r);
  basic_environment local= macro_arguments (std_env);
  if (!local->contains (key)) return evaluate_error ("undefined", r);
  r= local[key];
  if (N(t) > 1) {
    int i, n= N(t);
    for (i=1; i<n; i++) {
      tree u= evaluate (t[i]);
      if (!is_int (u)) break;
      int nr= as_int (u);
      if ((!is_compound (r)) || (nr<0) || (nr>=N(r))) break;
      r= r[nr];
    }
  }
  return r;
}
#endif // CLASSICAL_MACRO_EXPANSION

/******************************************************************************
* Alternative macro expansion
******************************************************************************/

#ifdef ALTERNATIVE_MACRO_EXPANSION

tree
expand (tree t, assoc_environment env) {
  if (is_atomic (t)) return t;
  else {
    int i, n= N(t);
    switch (L(t)) {
    case MACRO:
      {
	assoc_environment local= copy (env);
	// (all the children but the body are parameters, which shadow the
	// arguments of the same names: amsart's subsection-title binds a
	// macro with a parameter name inside its own macro with one)
	for (i=0; i+1<n; i++)
	  if (is_atomic (t[i])) {
	    int key= make_tree_label (t[i]->label);
	    local->remove (key);
	  }
	bool flag= true;
	tree r (t, n);
	for (i=0; i<n; i++) {
	  r[i]= expand (t[i], i==n-1? local: env);
	  flag= flag && weak_equal (r[i], t[i]);
	}
	if (flag) return t;
	return r;
      }
    case XMACRO:
      {
	assoc_environment local= copy (env);
	if (is_atomic (t[0])) {
	  int key= make_tree_label (t[0]->label);
	  local->remove (key);
	}
	tree body= expand (t[1], local);
	if (weak_equal (body, t[1])) return t;
	return tree (XMACRO, t[0], body);
      }
    case ARG:
      {
	tree r= t[0];
	if (is_compound (r)) return evaluate_error ("bad arg");
	int key= make_tree_label (r->label);
	if (!env->contains (key)) return t;
	r= env[key];
	if (N(t) > 1) {
	  int i, n= N(t);
	  for (i=1; i<n; i++) {
	    tree u= evaluate (expand (t[i], env));
	    if (!is_int (u)) return evaluate_error ("arg " * t[0]->label);
	    int nr= as_int (u);
	    if ((!is_compound (r)) || (nr<0) || (nr>=N(r)))
	      return evaluate_error ("arg " * t[0]->label);
	    r= r[nr];
	  }
	}
	return r;
      }
    case QUOTE_ARG:
      return tree (QUOTE, expand (tree (ARG, A(t)), env));
    case MAP_ARGS:
      {
	if (!(is_atomic (t[0]) && is_atomic (t[1]) && is_atomic (t[2])))
	  return evaluate_error ("invalid map arguments");
	int key= make_tree_label (t[2]->label);
	if (!env->contains (key))
	  return evaluate_error ("undefined", t[2]);
	tree val= env [key];
	if (is_atomic (val))
	  return evaluate_error ("map arguments " * t[2]->label);
	
	int start= 0, end= N(val);
	if (N(t)>=4) start= as_int (evaluate (expand (t[3], env)));
	if (N(t)>=5) end  = as_int (evaluate (expand (t[4], env)));
	int i, n= max (0, end-start);
	tree r (make_tree_label (t[1]->label), n);
	for (i=0; i<n; i++)
	  r[i]= tree (make_tree_label (t[0]->label),
		      val[start+i],
		      as_string (start+i));
	return r;
      }
    case EVAL_ARGS:
      return tree (EVAL_ARGS, expand (tree (ARG, t[0]), env));
    case OCCURS_INSIDE:
      // (the second child is the name of an argument, as for eval-args:
      // render-proof looks for a <qed> in its body)
      if (n == 2 && is_atomic (t[1])) {
	int key= make_tree_label (t[1]->label);
	if (env->contains (key))
	  return intern_tree (tree (OCCURS_INSIDE, expand (t[0], env),
				    env[key]));
      }
      // fall through
    default:
      {
	bool flag= true;
	tree r (t, n);
	for (i=0; i<n; i++) {
	  r[i]= expand (t[i], env);
	  flag= flag && weak_equal (r[i], t[i]);
	}
	if (flag) return t;
	// (the same expansion as before when the arguments it holds did not
	// change: see evaluate_compound)
	return intern_tree (r);
      }
    }
  }
}

tree
evaluate_compound (tree t) {
  int d; tree f; string var;
  if (L(t) == COMPOUND) {
    d= 1;
    f= t[0];
    if (is_compound (f)) f= evaluate (f);
    if (is_atomic (f)) {
      var= f->label;
      // (an undefined macro is the typesetter's: it shows the call, with
      // its arguments, whose labels it makes; see evaluate_left)
      if (!std_env->contains (var)) { (void) evaluate_left (t, 1); return t; }
      f= std_env [var];
    }
  }
  else {
    var= as_string (L(t));
    if (!std_env->contains (var)) { (void) evaluate_left (t); return t; }
    d= 0;
    f= std_env [var];
  }

  if (is_applicable (f)) {
    int i, n=N(f)-1, m=N(t)-d;
    assoc_environment local (L(f)==XMACRO? 1: n);
    if (L(f) == XMACRO)
      local->raw_write (0, as_string (f[0]), t);
    else {
      static tree uninit (UNINIT);
      for (i=0; i<n; i++)
	local->raw_write (i, as_string (f[i]), i<m? t[i+d]: uninit);
      //local->print ("");
    }
    // the parts of the body which hold arguments are made again at each
    // expansion; they are interned (expand), so that a part whose
    // arguments did not change is the same tree as at the last expansion,
    // and its evaluation is found in its memorizer (Joris's "FIXME: should
    // we remember partial expansions?")
    tree e= expand (f[n], local);
    decorate_ip (t, e);
    if (is_func (f[n], ARG, 1)) return evaluate_argument_result (e);
    return evaluate_result (e);
  }
  // (a layout value, like the pattern of a theme's title-color, is no
  // macro: the call gives the value which the typesetter computed where it
  // was bound, as evaluate_value)
  else if (is_layout_value (L(f)) && var != "") return tree (VALUE, var);
  // (a macro known to the typesetter only, defined in a branch which it
  // decides: the call is its, with its arguments)
  else if (is_residual (f)) {
    (void) evaluate_left (t, d);
    return t;
  }
  else return evaluate_result (f);
}

#endif // ALTERNATIVE_MACRO_EXPANSION

/******************************************************************************
* Argument expansion
******************************************************************************/

#define is_accessible(p) ((is_nil (p)) || ((p)->item >= 0))

tree
expand (tree t, bool search_accessible) {
  if (is_atomic (t)) return t;
#ifdef CLASSICAL_MACRO_EXPANSION
  else if (macro_top_level (std_env)) return t;
  else if (is_func (t, ARG) || is_func (t, QUOTE_ARG)) {
    tree r= t[0];
    if (is_compound (r)) return evaluate_error ("bad arg");
    int key= make_tree_label (r->label);
    basic_environment local= macro_arguments (std_env);
    if (!local->contains (key)) return evaluate_error ("undefined", r);
    macro_up (std_env);
    r= local[key];
    if (N(t) > 1) {
      int i, n= N(t);
      for (i=1; i<n; i++) {
	tree u= evaluate (t[i]);
	if (!is_int (u)) break;
	int nr= as_int (u);
	if ((!is_compound (r)) || (nr<0) || (nr>=N(r))) break;
	r= r[nr];
      }
    }
    if (is_func (t, ARG))
      r= expand (r, search_accessible);
    macro_redown (std_env, local);
    return r;
  }
#endif // CLASSICAL_MACRO_EXPANSION
  else if (is_func (t, EXPAND_AS, 2))
    return expand (t[0], search_accessible);
  else if (search_accessible && is_accessible (obtain_ip (t)))
    return t;
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++) {
      r[i]= expand (t[i], search_accessible);
      if (search_accessible &&
	  is_accessible (obtain_ip (r[i])) &&
	  the_drd->is_accessible_child (t, i)
	  // FIXME: should be drd->is_accessible_child (t, i)
	  )
	return r[i];
    }
    if (search_accessible) return t;
    return r;
  }
}
