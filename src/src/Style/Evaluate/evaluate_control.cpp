
/******************************************************************************
* MODULE     : evaluate_control.cpp
* DESCRIPTION: control structures
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "evaluate_main.hpp"
#include "std_environment.hpp"
#include "vars.hpp"
#include "file.hpp"
#include "convert.hpp"
#include "scheme.hpp"

tree load_inclusion (url u); // implemented in tm_file.cpp

/******************************************************************************
* Classical control structures
******************************************************************************/

void
make_references (list<string> names) {
  // the variables which only the typesetter knows from now on: references
  // to them (one tree per name, so that the same environment before gives
  // the same environment after, and what follows finds its memorizers)
  static tree uninit (UNINIT);
  static hashmap<string,tree> refs (uninit);
  names= unique_names (names);
  if (is_nil (names)) return;
  assoc_environment local (N(names));
  int k= 0;
  for (list<string> l= names; !is_nil (l); l= l->next, k++) {
    if (!refs->contains (l->item)) refs (l->item)= tree (VALUE, l->item);
    local->raw_write (k, l->item, refs [l->item]);
  }
  assign (std_env, local);
  note_assigned (names);
}

list<string>
evaluate_branches (tree t, tree& r, array<int> branches, bool markup) {
  // the branches of a residual if or case: the typesetter takes one of
  // them, so each is evaluated from the environment before them, which
  // stays the one after them; a variable which one of them assigns (a
  // counter incremented in one branch) is known to the typesetter only,
  // and becomes a reference to it (its assignments stay in the branches)
  environment before= std_env;
  list<string> names;
  {
    assigned_collector collect;
    depth_guard guard (branch_depth);
    for (int k=0; k<N(branches); k++) {
      std_env= before;
      tree u= t[branches[k]];
      r[branches[k]]= (markup? evaluate_result (u): evaluate (u));
    }
    names= collect.names;
  }
  std_env= before;
  // (a macro which a branch redefines: the typesetter may expand the
  // former one, whose assignments become references too)
  list<string> more;
  for (list<string> l= names; !is_nil (l); l= l->next)
    if (std_env->contains (l->item)) {
      tree f= std_env [l->item];
      if (is_func (f, MACRO) || is_func (f, XMACRO))
        more= assigned_in_value (f) * more;
    }
  names= unique_names (names * more);
  make_references (names);
  return names;
}

list<string>
evaluate_left (tree t, int from) {
  // a tree left as it is to the typesetter, which evaluates it (a call of
  // an undefined macro or of one which it alone knows, a script with such
  // an argument): the variables which its parts assign become references,
  // as in a branch which it decides
  if (is_atomic (t) || N(t) <= from) return list<string> ();
  tree scratch (t, N(t));
  array<int> parts;
  for (int i=from; i<N(t); i++) parts << i;
  // (a scratch evaluation: its results, kept apart in the memorizers, are
  // not used, and the scripts in it are not run, see evaluate_rewrite)
  depth_guard guard (scratch_depth);
  return evaluate_branches (t, scratch, parts);
}

tree
evaluate_if (tree t) {
  if (N(t) != 2 && N(t) != 3) return evaluate_error ("bad if");
  tree u= evaluate (t[0]);
  if (is_residual (u) || is_left_call (u)) {
    // the condition is left to the typesetter, with both branches
    tree r (t, N(t));
    r[0]= u;
    array<int> branches;
    for (int i=1; i<N(t); i++) branches << i;
    (void) evaluate_branches (t, r, branches);
    return r;
  }
  if (!is_bool (u)) return evaluate_error ("bad if");
  if (as_bool (u)) return evaluate_result (t[1]);
  if (N(t)==3) return evaluate_result (t[2]);
  return "";
}

tree
evaluate_case (tree t) {
  if (N(t) < 2) return evaluate_error ("bad case");
  int i, n= N(t);
  for (i=0; i<(n-1); i+=2) {
    tree u= evaluate (t[i]);
    if (is_residual (u) || is_left_call (u)) {
      // the rest of the cases is left to the typesetter
      tree r (CASE);
      r << u;
      // (the branches, and the last tree when it is the default branch,
      // in the mode of the case; the conditions as values)
      array<int> branches;
      for (int j=i+1; j<n; j++) {
        bool branch= (j-i) % 2 == 1 || j == n-1;
        if (branch) { r << tree (""); branches << N(r)-1; }
        else r << evaluate (t[j]);
      }
      tree src (CASE);
      src << u;
      for (int j=i+1; j<n; j++) src << t[j];
      (void) evaluate_branches (src, r, branches);
      return r;
    }
    if (!is_bool (u)) return evaluate_error ("bad case");
    if (as_bool (u)) return evaluate_result (t[i+1]);
  }
  if (i<n) return evaluate_result (t[i]);
  return "";
}

tree
evaluate_while (tree t) {
  if (N(t) != 2) return evaluate_error ("bad while");
  tree r (CONCAT);
  while (true) {
    tree u= evaluate (t[0]);
    if (is_residual (u) || is_left_call (u)) {
      // the rest of the loop is the typesetter's (a counter which it alone
      // knows); the variables which the body assigns become references,
      // as in a branch which it decides, until the body assigns no other
      // (a pass with the references may take other branches of it)
      tree scratch (t, N(t));
      array<int> body;
      body << 1;
      int known= -1;
      list<string> names;
      depth_guard guard (scratch_depth);
      for (int pass=0; pass<4 && N(names) != known; pass++) {
        known= N(names);
        names= unique_names (evaluate_branches (t, scratch, body, false) *
                             names);
      }
      if (N(r) == 0) return t;
      r << t;
      break;
    }
    if (!is_bool (u)) return evaluate_error ("bad while");
    if (!as_bool (u)) break;
    r << evaluate (t[1]);
  }
  if (N(r) == 0) return "";
  if (N(r) == 1) return r[0];
  return r;
}

tree
evaluate_for_each (tree t) {
  if (N(t) != 2) return evaluate_error ("bad for-each");
  tree fun = evaluate (t[0]);
  tree args= evaluate (t[1]);
  // (a list or a function which the typesetter alone knows: its loop, and
  // the variables which the function assigns become references)
  if (is_residual (fun) || is_residual (args) || is_left_call (args)) {
    make_references (assigned_in_macro (fun));
    return t;
  }
  if (!is_tuple (args)) return evaluate_error ("bad for-each");
  int i, n= N(args);
  for (i=0; i<n; i++)
    evaluate (tree (COMPOUND, fun, args[i]));
  return "";
}

/******************************************************************************
* External dependencies
******************************************************************************/

tree
evaluate_include (tree t) {
  url base_file_name (as_string (std_env["base-file-name"]));
  url incl_file_name=
    relative (base_file_name, url_system (as_string (evaluate (t[0]))));
  tree incl= load_inclusion (incl_file_name);

  assoc_environment local (2);
  local->raw_write (0, string ("cur-file-name"),
		    as_string (incl_file_name));
  local->raw_write (1, string ("secure"),
		    bool_as_tree (is_secure (incl_file_name)));

  begin_with (std_env, local);
  tree r= evaluate (incl);
  end_with (std_env);
  return r;
}

static tree
filter_style (tree t) {
  if (is_atomic (t)) return t;
  else switch (L(t)) {
  case STYLE_WITH:
  case VAR_STYLE_WITH:
    return filter_style (t[N(t)-1]);
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
    if (is_atomic (t[0]) || N(t[0]) == 0) return "";
    else return filter_style (t[0][N(t[0])-1]);
  case ACTIVE:
  case VAR_ACTIVE:
  case INACTIVE:
  case VAR_INACTIVE:
    return filter_style (t[0]);
  default:
    {
      int i, n= N(t);
      tree r (t, n);
      for (i=0; i<n; i++)
	r[i]= filter_style (t[i]);
      // (the assignments of the preamble keep their places)
      transfer_ip (t, r);
      return r;
    }
  }
}

tree
evaluate_filter_style (tree t) {
  // (as exec: the style definitions of a document, in hide-preamble,
  // are its value without the style markup, evaluated in its turn)
  if (N(t) < 1) return evaluate_error ("invalid filter-style");
  return evaluate_result (filter_style (evaluate (t[0])));
}

tree
evaluate_use_package (tree t) {
  int i, n= N(t);
  for (i=0; i<n; i++) {
    url base_file_name (as_string (std_env["base-file-name"]));
    url styp= "$TEXMACS_STYLE_PATH";
    url name= as_string (t[i]) * string (".ts");
    //cout << "Package " << name << "\n";
    if (is_rooted_web (base_file_name))
      styp= styp | head (base_file_name);
    else styp= head (base_file_name) | styp;
    string doc_s;
    if (!load_string (styp * name, doc_s, false)) {
      tree doc= texmacs_document_to_tree (doc_s);
      if (is_compound (doc))
	evaluate (filter_style (extract (doc, "body")));
    }
  }
  return "";
}

tree
evaluate_use_module (tree t) {
  int i, n= N(t);
  for (i=0; i<n; i++) {
    string s= evaluate_string (t[i]);
    if (!use_module (s)) continue;
    tree t= std_env[THE_MODULES] * tuple (s);
    assoc_environment local (1);
    local->raw_write (0, THE_MODULES, t);
    assign (std_env, local);
  }
  return "";
}
