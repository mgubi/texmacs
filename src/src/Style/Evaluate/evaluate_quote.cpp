
/******************************************************************************
* MODULE     : evaluate_quote.cpp
* DESCRIPTION: quoting and evaluation primitives
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "evaluate_main.hpp"
#include "std_environment.hpp"

/******************************************************************************
* The eval-args primitive
******************************************************************************/

tree
evaluate_eval_args (tree t) {
#ifdef CLASSICAL_MACRO_EXPANSION
  if (macro_top_level (std_env) || !is_atomic (t[0]))
    return evaluate_error ("undefined", t[0]);
  basic_environment local= macro_arguments (std_env);
  int key= make_tree_label (t[0]->label);
  if (!local->contains (key))
    return evaluate_error ("undefined", t[0]);
  tree u= local [key];
#else
  tree u= t[0];
#endif
  if (is_atomic (u)) return evaluate_error ("bad eval-args");

#ifdef CLASSICAL_MACRO_EXPANSION
  macro_up (std_env);
#endif

  int i, n= N(u);
  tree r (u, n);
  for (i=0; i<n; i++)
    r[i]= evaluate (u[i]);

#ifdef CLASSICAL_MACRO_EXPANSION
  macro_redown (std_env, local);
#endif

  return r;
}

/******************************************************************************
* Quasiquote
******************************************************************************/

static bool quasi_left= false;

tree
evaluate_quasiquote (tree t) {
  if (is_atomic (t)) return t;
  else if (is_func (t, UNQUOTE, 1)) return evaluate (t[0]);
  else {
    int i, n= N(t);
    tree r (L(t));
    for (i=0; i<n; i++) {
      if (is_func (t[i], VAR_UNQUOTE, 1)) {
	tree ins= evaluate (t[i]);
	// (what only the typesetter computes, an extern left to it: it
	// splices the result, see evaluate_quasi)
	if (is_residual (ins) || is_left_call (ins)) {
	  quasi_left= true;
	  r << t[i];
	}
	else if (is_compound (ins)) r << A(ins);
	else r << evaluate_error ("bad unquote*");
      }
      else r << evaluate_quasiquote (t[i]);
    }
    return r;
  }
}

tree
evaluate_quasi (tree t) {
  // quasi and quasiquote: the typesetter's when a part to splice is its
  // (the map of the title of an article: an extern whose arguments hold
  // labels); its children would be spliced instead of its result
  bool old= quasi_left;
  quasi_left= false;
  tree r= evaluate_quasiquote (t[0]);
  bool left= quasi_left;
  quasi_left= old;
  if (left) { (void) evaluate_left (t); return t; }
  if (is_func (t, QUASI)) return evaluate_result (r);
  return r;
}
