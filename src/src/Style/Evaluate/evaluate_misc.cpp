
/******************************************************************************
* MODULE     : evaluate_misc.cpp
* DESCRIPTION: various other primitives for evaluation
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "evaluate_main.hpp"
#include "std_environment.hpp"
#include "vars.hpp"
#include "analyze.hpp"
#include "url.hpp"
#include "../../Graphics/Types/frame.hpp"
#include "image_files.hpp"
#include "renderer.hpp"

static hashmap<string,tree> local_ref ("?");
static hashmap<string,tree> global_ref ("?");

tree
evaluate_formatting (tree t, string v) {
  int i, n= N(t);
  tree r (t, n-1);
  // (the values of a cell format are executed by the typesetter in each
  // cell, with the variables which it sets there: evaluate_locals.cpp)
  for (i=0; i<n-1; i++) {
    bool local= begin_typesetter_locals (t, i);
    r[i]= evaluate (t[i]);
    if (local) end_with (std_env);
  }
  tree oldv= std_env [v];
  tree newv= intern_tree (oldv * r);
  assoc_environment local (1);
  local->raw_write (0, v, newv);
  begin_with (std_env, local);
  tree b= evaluate_part (t[n-1]);
  end_with (std_env);
  return r * tree (TFORMAT, b);
}

tree
evaluate_table (tree t) {
  // FIXME: we should execute values in old cell format
  // (the same empty format each time: a new one would be a new key for
  // begin_with, and a new environment inside the table)
  static tree empty_format (TFORMAT);
  // (in the cells, the variables which the typesetter sets itself there,
  // evaluate_locals.cpp: the margins of a cell whose lines are broken, at
  // zero, which placed a list in a cell of a box with negative margins with
  // the margins outside the table; the row and the column)
  assoc_environment local (1);
  local->raw_write (0, CELL_FORMAT, empty_format);
  begin_with (std_env, local);
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++) r[i]= evaluate_typesetter_child (t, i);
  end_with (std_env);
  return r;
}

tree
evaluate_hard_id (tree t) {
  if (N(t) == 0) {
    pointer ptr= (pointer) std_env.operator -> ();
    return "%" * as_hexadecimal (ptr);
  }
  else {
    t= expand (t[0], true);
    pointer ptr1= (pointer) std_env.operator -> ();
    pointer ptr2= (pointer) t.operator -> ();
    return "%" * as_hexadecimal (ptr1) * "-" * as_hexadecimal (ptr2);
  }
}

tree
evaluate_script (tree t) {
  if (N(t) != 1 && N(t) != 2) return tree (_ERROR, "bad script");
  if (N(t) == 1) return tree (SCRIPT, evaluate (t[0]));
  else return tree (SCRIPT, evaluate (t[0]), expand (t[1], true));
}

tree
evaluate_set_binding (tree t) {
  tree keys, value;
  if (N(t) == 1) {
    keys= std_env ["the-tags"];
    // (tags known to the typesetter only: it makes the bindings)
    if (contains_residual (keys)) keys= tuple ();
    if (!is_tuple (keys))
      return tree (_ERROR, "bad set binding");
    for (int i=0; i<N(keys); i++)
      if (!is_atomic (keys[i]))
	return tree (_ERROR, "bad set binding");
    value= evaluate (t[0]);
    // (the same trees when nothing changed: the assignment then finds its
    // memorizer and gives the same environment, and the evaluations which
    // follow are found in theirs; a copy, or a new empty tuple, would give
    // a new environment at each evaluation)
    static tree no_tags (TUPLE);
    assoc_environment local (2);
    local->raw_write (0, string ("the-tags"), no_tags);
    local->raw_write (1, string ("the-label"), value);
    assign (std_env, local);
    note_assigned (string ("the-tags"));
    note_assigned (string ("the-label"));
  }
  else if (N(t) >= 2) {
    tree key= evaluate (t[0]);
    if (contains_residual (key)) {
      // (a name known to the typesetter only, a counter which a residual
      // branch assigned: the typesetter makes the binding)
      tree r (t, N(t));
      r[0]= key;
      for (int i=1; i<N(t); i++) r[i]= evaluate (t[i]);
      transfer_ip (t, r);
      return r;
    }
    if (!is_atomic (key)) 
      return tree (_ERROR, "bad set binding");
    keys= tuple (key);
    value= evaluate (t[1]);
  }
  else return tree (_ERROR, "bad set binding");

  for (int i=0; i<N(keys); i++) {
    string key= keys[i]->label;
    tree old_value= local_ref[key];
    string part= as_string (std_env ["current-part"]);
    url base_file_name (as_string (std_env ["base-file-name"]));
    url cur_file_name (as_string (std_env ["cur-file-name"]));
    if (is_func (old_value, TUPLE) && (N(old_value) >= 2))
      local_ref (key)= tuple (copy (value), old_value[1]);
    else local_ref (key)= tuple (copy (value), "?");
    if (cur_file_name != base_file_name || N(part) != 0) {
      string extra;
      if (cur_file_name != base_file_name)
	extra << as_string (delta (base_file_name, cur_file_name));
      if (N(part) != 0)
	extra << "#" << part (1, N(part));
      local_ref (key) << extra;
    }
    /* FIXME:
    if (complete && is_tuple (old_value) && N(old_value) >= 1) {
      string old_s= tree_as_string (old_value[0]);
      string new_s= tree_as_string (value);
      if (new_s != old_s && !starts (key, "auto-")) {
        if (new_s == "") typeset_warning << "Redefined " << key << LF;
	else typeset_warning << "Redefined " << key << " as " << new_s << LF;
      }
    }
    */
  }

  // the binding is made by the typesetter (concater_rep::typeset_set_binding,
  // which also makes the locus of the label): it stays, evaluated
  tree r (t, N(t));
  if (N(t) == 1) r[0]= value;
  else {
    r[0]= keys[0];
    r[1]= value;
    for (int i=2; i<N(t); i++) r[i]= evaluate (t[i]);
  }
  return r;
}

tree
evaluate_get_binding (tree t) {
  if (N(t) != 1 && N(t) != 2) return tree (_ERROR, "bad get binding");
  string key= evaluate_string (t[0]);
  tree value= local_ref->contains (key)? local_ref [key]: global_ref [key];
  int type= (N(t) == 1? 0: as_int (evaluate_string (t[1])));
  if (type != 0 && type != 1) type= 0;
  if (is_func (value, TUPLE) && (N(value) >= 2)) value= value[type];
  else if (type == 1) value= tree (UNINIT);
  /* FIXME:
  if (complete && value == tree (UNINIT))
    typeset_warning << "Undefined reference " << key << LF;
  */
  return value;
}

tree
evaluate_has_binding (tree t) {
  if (N(t) != 1 && N(t) != 2) return tree (_ERROR, "bad get binding");
  string key= evaluate_string (t[0]);
  if (!local_ref->contains (key) && !global_ref->contains (key))
    return "false";
  tree value= local_ref->contains (key)? local_ref [key]: global_ref [key];
  int type= (N(t) == 1? 0: as_int (evaluate_string (t[1])));
  if (type != 0 && type != 1) type= 0;
  if (is_func (value, TUPLE) && (N(value) >= 2)) value= value[type];
  else if (type == 1) value= tree (UNINIT);
  return as_string_bool (value != tree (UNINIT));
}

tree
evaluate_pattern (tree t) {
  url base_file_name (as_string (std_env ["base-file-name"]));
  url im= evaluate_string (t[0]);
  url image= resolve_pattern (relative (base_file_name, im));
  if (is_none (image)) return "white";
  int imw_pt, imh_pt;
  int dpi= as_int (as_string (std_env ["dpi"]));
  image_size (image, imw_pt, imh_pt);
  double pt= ((double) dpi*PIXEL) / 72.0;
  SI imw= (SI) (((double) imw_pt) * pt);
  SI imh= (SI) (((double) imh_pt) * pt);
  if (imw <= 0 || imh <= 0) return "white";
  string w= evaluate_string (t[1]);
  string h= evaluate_string (t[2]);
  if (is_length (w))
    w= as_string (as_length (w));
  else if (is_magnification (w))
    w= as_string ((SI) (get_magnification (w) * ((double) imw)));
  if (is_length (h))
    h= as_string (as_length (h));
  else if (is_magnification (h))
    h= as_string ((SI) (get_magnification (h) * ((double) imh)));
  if (w == "" && h != "") {
    if (is_int (h)) w= as_string ((SI) ((as_double (h) * imw) / imh));
    else if (is_percentage (h))
      w= as_string (100.0 * (as_percentage (h) * imw) / imh) * "@";
    else return "white";
  }
  else if (h == "" && w != "") {
    if (is_int (w)) h= as_string ((SI) ((as_double (w) * imh) / imw));
    else if (is_percentage (w))
      h= as_string (100.0 * (as_percentage (w) * imh) / imw) * "@";
    else return "white";
  }
  else if (w == "" && h == "") {
    w= as_string (imw);
    h= as_string (imh);
  }
  else if ((!is_int (w) && !is_percentage (w)) ||
	   (!is_int (h) && !is_percentage (h)))
    return "white";
  tree r (_PATTERN, as_string (image), w, h);
  if (N(t) == 4) r << evaluate (t[3]);
  return r;
}

tree
evaluate_point (tree t) {
  int i, n= N(t);
  tree u (_POINT, n);
  for (i=0; i<n; i++)
    u[i]= evaluate (t[i]);
  if (n==0 || is_double (u[0])) return u;
  return as_tree (as_point (u));
}

/*
tree
evaluate_box_info (tree t) {
  tree t1= t[0];
  tree t2= t[1];
  if (!is_string (t2))
    return tree (_ERROR, "bad box info");
  return box_info (edit_env (this), t1, as_string (t2));
}

tree
evaluate_frame_direct (tree t) {
  tree t1= evaluate (t[0]);
  return as_tree (!nil (fr) ? fr (::as_point (t1)) : point ());
}

tree
evaluate_frame_inverse (tree t) {
  tree t1= evaluate (t[0]);
  return as_tree (!nil (fr) ? fr [::as_point (t1)] : point ());
}
*/
