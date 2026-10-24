
/******************************************************************************
* MODULE     : evaluate_locals.cpp
* DESCRIPTION: the variables which the typesetter sets itself in a part
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "evaluate_main.hpp"
#include "std_environment.hpp"
#include "vars.hpp"

/******************************************************************************
* The typesetter sets some variables itself in some children of a primitive:
* the row of a cell, the math level of a script, the colour of a link, the
* margins in an ornament... A macro which reads one of them there must find
* what the typesetter finds, and the rewriter evaluates the style before the
* typesetting: in these children the variable gets the value which the
* typesetter always gives it, or else is left to it, as a reference
* (decision 1.1 of docs/pure-style-decisions.md).
*
* Each entry gives the primitive, its children from from to to (a negative
* or zero bound counts from the end: 0, 0 is all of them; -1, 0 the last),
* the variable, its value (NULL: a reference), and the functions of the
* typesetter which set it there. An entry with an unless variable does not
* apply when that variable has that value. tests/style/typesetter-vars.sh
* checks that every place where the typesetter sets a variable is in this
* table, or reviewed in its own list of exceptions: a new one fails there.
******************************************************************************/

struct typesetter_local {
  tree_label label;
  int from, to;
  const char* var;
  const char* val;
  const char* sites;
  const char* unless_var;
  const char* unless_val;
};

static const typesetter_local typesetter_locals[]= {
  // scripts (the math level: local_begin_script)
  { LSUB, 0, 0, "math-display", "false", "concater_rep::typeset_script" },
  { LSUB, 0, 0, "math-condensed", "true", "concater_rep::typeset_script" },
  { LSUB, 0, 0, "math-vpos", "-1", "concater_rep::typeset_script" },
  { LSUB, 0, 0, "math-level", NULL, "concater_rep::typeset_script" },
  { LSUP, 0, 0, "math-display", "false", "concater_rep::typeset_script" },
  { LSUP, 0, 0, "math-condensed", "true", "concater_rep::typeset_script" },
  { LSUP, 0, 0, "math-vpos", "1", "concater_rep::typeset_script" },
  { LSUP, 0, 0, "math-level", NULL, "concater_rep::typeset_script" },
  { RSUB, 0, 0, "math-display", "false", "concater_rep::typeset_script" },
  { RSUB, 0, 0, "math-condensed", "true", "concater_rep::typeset_script" },
  { RSUB, 0, 0, "math-vpos", "-1", "concater_rep::typeset_script" },
  { RSUB, 0, 0, "math-level", NULL, "concater_rep::typeset_script" },
  { RSUP, 0, 0, "math-display", "false", "concater_rep::typeset_script" },
  { RSUP, 0, 0, "math-condensed", "true", "concater_rep::typeset_script" },
  { RSUP, 0, 0, "math-vpos", "1", "concater_rep::typeset_script" },
  { RSUP, 0, 0, "math-level", NULL, "concater_rep::typeset_script" },
  // fractions (in a display, the display style only is changed; otherwise
  // the level, the display style being already false)
  { FRAC, 0, 1, "math-display", "false", "concater_rep::typeset_frac" },
  { FRAC, 0, 1, "math-vpos", "1", "concater_rep::typeset_frac" },
  { FRAC, 1, 2, "math-display", "false", "concater_rep::typeset_frac" },
  { FRAC, 1, 2, "math-vpos", "-1", "concater_rep::typeset_frac" },
  { FRAC, 0, 0, "math-level", NULL, "concater_rep::typeset_frac" },
  // the index of a root
  { SQRT, 1, 2, "math-display", "false",
    "concater_rep::typeset_sqrt concater_rep::typeset_wide_sqrt" },
  { SQRT, 1, 2, "math-level", NULL,
    "concater_rep::typeset_sqrt concater_rep::typeset_wide_sqrt" },
  // what is above or below, the scripts of a long arrow
  { ABOVE, 1, 2, "math-display", "false", "concater_rep::typeset_above" },
  { ABOVE, 1, 2, "math-condensed", "true", "concater_rep::typeset_above" },
  { ABOVE, 1, 2, "math-level", NULL, "concater_rep::typeset_above" },
  { BELOW, 1, 2, "math-display", "false", "concater_rep::typeset_below" },
  { BELOW, 1, 2, "math-condensed", "true", "concater_rep::typeset_below" },
  { BELOW, 1, 2, "math-level", NULL, "concater_rep::typeset_below" },
  { LONG_ARROW, 1, 0, "math-display", "false",
    "concater_rep::typeset_long_arrow" },
  { LONG_ARROW, 1, 0, "math-condensed", "true",
    "concater_rep::typeset_long_arrow" },
  { LONG_ARROW, 1, 2, "math-vpos", "-1", "concater_rep::typeset_long_arrow" },
  { LONG_ARROW, 2, 3, "math-vpos", "1", "concater_rep::typeset_long_arrow" },
  { LONG_ARROW, 1, 0, "math-level", NULL, "concater_rep::typeset_long_arrow" },
  // brackets: their nesting, and their colour when they are coloured
  { AROUND, 0, 0, "math-nesting-level", NULL, "concater_rep::typeset_around" },
  { AROUND, 0, 0, "color", NULL, "concater_rep::typeset_around",
    "math-nesting-mode", "off" },
  { VAR_AROUND, 0, 0, "math-nesting-level", NULL,
    "concater_rep::typeset_around" },
  { VAR_AROUND, 0, 0, "color", NULL, "concater_rep::typeset_around",
    "math-nesting-mode", "off" },
  { BIG_AROUND, 0, 0, "math-nesting-level", NULL,
    "concater_rep::typeset_around" },
  { BIG_AROUND, 0, 0, "color", NULL, "concater_rep::typeset_around",
    "math-nesting-mode", "off" },
  // the body of a link, in the colour of the links on the screen
  { LOCUS, -1, 0, "color", NULL,
    "concater_rep::typeset_locus bridge_locus_rep::my_typeset "
    "make_lazy_locus typeset_as_atomic" },
  // graphics and the text in them (BEGIN_MAGNIFY)
  { GRAPHICS, 0, 0, "magnification", NULL, "BEGIN_MAGNIFY" },
  { GRAPHICS, 0, 0, "magnify", "1", "BEGIN_MAGNIFY" },
  { GR_GROUP, 0, 0, "magnification", NULL, "BEGIN_MAGNIFY" },
  { GR_GROUP, 0, 0, "magnify", "1", "BEGIN_MAGNIFY" },
  { TEXT_AT, 0, 0, "magnification", NULL, "BEGIN_MAGNIFY" },
  { TEXT_AT, 0, 0, "magnify", "1", "BEGIN_MAGNIFY" },
  { MATH_AT, 0, 0, "magnification", NULL, "BEGIN_MAGNIFY" },
  { MATH_AT, 0, 0, "magnify", "1", "BEGIN_MAGNIFY" },
  { DOCUMENT_AT, 0, 0, "magnification", NULL, "BEGIN_MAGNIFY" },
  { DOCUMENT_AT, 0, 0, "magnify", "1", "BEGIN_MAGNIFY" },
  // the body of an ornament, an art box, a canvas: margins of their own
  { ORNAMENT, 0, 1, "par-left", NULL, "bridge_ornament_rep::my_typeset" },
  { ORNAMENT, 0, 1, "par-right", NULL, "bridge_ornament_rep::my_typeset" },
  { ART_BOX, 0, 1, "par-left", NULL, "bridge_art_box_rep::my_typeset" },
  { ART_BOX, 0, 1, "par-right", NULL, "bridge_art_box_rep::my_typeset" },
  { CANVAS, -1, 0, "par-left", NULL,
    "bridge_canvas_rep::my_typeset lazy_canvas_rep::query "
    "lazy_canvas_rep::produce" },
  { CANVAS, -1, 0, "par-right", NULL,
    "bridge_canvas_rep::my_typeset lazy_canvas_rep::query "
    "lazy_canvas_rep::produce" },
  { CANVAS, -1, 0, "page-medium", NULL,
    "lazy_canvas_rep::query lazy_canvas_rep::produce" },
  { CANVAS, -1, 0, "par-mode", NULL,
    "lazy_canvas_rep::query lazy_canvas_rep::produce" },
  { CANVAS, -1, 0, "par-no-first", NULL,
    "lazy_canvas_rep::query lazy_canvas_rep::produce" },
  { CANVAS, -1, 0, "par-width", NULL,
    "lazy_canvas_rep::query lazy_canvas_rep::produce" },
  // the cells of a table (the paragraph variables in a cell whose lines
  // are broken), and the formats, which are executed in each cell
  { TABLE, 0, 0, "cell-row-nr", NULL, "table_rep::typeset_table" },
  { TABLE, 0, 0, "cell-col-nr", NULL, "table_rep::typeset_row" },
  { TABLE, 0, 0, "par-left", NULL, "cell_rep::typeset" },
  { TABLE, 0, 0, "par-right", NULL, "cell_rep::typeset" },
  { TABLE, 0, 0, "page-medium", NULL, "cell_rep::typeset" },
  { TABLE, 0, 0, "par-mode", NULL, "cell_rep::typeset" },
  { TABLE, 0, 0, "par-no-first", NULL, "cell_rep::typeset" },
  { TABLE, 0, 0, "par-width", NULL, "cell_rep::typeset" },
  { TFORMAT, 0, -1, "cell-row-nr", NULL, "table_rep::typeset_table" },
  { TFORMAT, 0, -1, "cell-col-nr", NULL, "table_rep::typeset_row" },
  // the sizes of an image, and of the body of a box, for its lengths
  { IMAGE, 1, 5, "w-length", NULL, "concater_rep::typeset_image" },
  { IMAGE, 1, 5, "h-length", NULL, "concater_rep::typeset_image" },
  { MOVE, 1, 0, "w-length", NULL, "concater_rep::typeset_move" },
  { MOVE, 1, 0, "h-length", NULL, "concater_rep::typeset_move" },
  { MOVE, 1, 0, "l-length", NULL, "concater_rep::typeset_move" },
  { MOVE, 1, 0, "b-length", NULL, "concater_rep::typeset_move" },
  { MOVE, 1, 0, "r-length", NULL, "concater_rep::typeset_move" },
  { MOVE, 1, 0, "t-length", NULL, "concater_rep::typeset_move" },
  { SHIFT, 1, 0, "w-length", NULL, "concater_rep::typeset_shift" },
  { SHIFT, 1, 0, "h-length", NULL, "concater_rep::typeset_shift" },
  { SHIFT, 1, 0, "l-length", NULL, "concater_rep::typeset_shift" },
  { SHIFT, 1, 0, "b-length", NULL, "concater_rep::typeset_shift" },
  { SHIFT, 1, 0, "r-length", NULL, "concater_rep::typeset_shift" },
  { SHIFT, 1, 0, "t-length", NULL, "concater_rep::typeset_shift" },
  { RESIZE, 1, 0, "w-length", NULL, "concater_rep::typeset_resize" },
  { RESIZE, 1, 0, "h-length", NULL, "concater_rep::typeset_resize" },
  { RESIZE, 1, 0, "l-length", NULL, "concater_rep::typeset_resize" },
  { RESIZE, 1, 0, "b-length", NULL, "concater_rep::typeset_resize" },
  { RESIZE, 1, 0, "r-length", NULL, "concater_rep::typeset_resize" },
  { RESIZE, 1, 0, "t-length", NULL, "concater_rep::typeset_resize" },
  { CLIPPED, 1, 0, "w-length", NULL, "concater_rep::typeset_clipped" },
  { CLIPPED, 1, 0, "h-length", NULL, "concater_rep::typeset_clipped" },
  { CLIPPED, 1, 0, "l-length", NULL, "concater_rep::typeset_clipped" },
  { CLIPPED, 1, 0, "b-length", NULL, "concater_rep::typeset_clipped" },
  { CLIPPED, 1, 0, "r-length", NULL, "concater_rep::typeset_clipped" },
  { CLIPPED, 1, 0, "t-length", NULL, "concater_rep::typeset_clipped" }
};

/******************************************************************************
* Lookup
******************************************************************************/

static int
typesetter_locals_number () {
  return (int) (sizeof (typesetter_locals) / sizeof (typesetter_local));
}

static array<int>
typesetter_locals_of (tree_label l) {
  // the entries of a primitive (made once)
  static hashmap<int,array<int> > by_label;
  static bool made= false;
  if (!made) {
    for (int k=0; k<typesetter_locals_number (); k++) {
      int key= (int) typesetter_locals[k].label;
      array<int> a;
      if (by_label->contains (key)) a= by_label [key];
      a << k;
      by_label (key)= a;
    }
    made= true;
  }
  if (!by_label->contains ((int) l)) return array<int> ();
  return by_label [(int) l];
}

static tree
typesetter_local_value (int k) {
  // the value of an entry: the same tree each time, since the values of
  // an environment are keys of begin_with
  static array<tree> values;
  if (N(values) == 0) {
    for (int i=0; i<typesetter_locals_number (); i++) {
      const typesetter_local& e= typesetter_locals[i];
      if (e.val == NULL) values << tree (VALUE, string (e.var));
      else values << tree (string (e.val));
    }
  }
  return values[k];
}

bool
has_typesetter_locals (tree t) {
  return is_compound (t) && N(typesetter_locals_of (L(t))) != 0;
}

bool
begin_typesetter_locals (tree t, int i) {
  // begin the environment of the child i of t, with the variables which
  // the typesetter sets itself there; false (and nothing begun) when there
  // are none
  if (is_atomic (t)) return false;
  array<int> ks= typesetter_locals_of (L(t));
  if (N(ks) == 0) return false;
  int n= N(t);
  array<string> vars;
  array<tree> vals;
  for (int j=0; j<N(ks); j++) {
    const typesetter_local& e= typesetter_locals[ks[j]];
    int from= (e.from < 0? n + e.from: e.from);
    int to  = (e.to  <= 0? n + e.to  : e.to);
    if (i < from || i >= to) continue;
    if (e.unless_var != NULL) {
      tree u= std_env [string (e.unless_var)];
      if (is_atomic (u) && u->label == string (e.unless_val)) continue;
    }
    string var (e.var);
    if (contains (var, vars)) continue;
    vars << var;
    vals << typesetter_local_value (ks[j]);
  }
  if (N(vars) == 0) return false;
  assoc_environment local (N(vars));
  for (int j=0; j<N(vars); j++) local->raw_write (j, vars[j], vals[j]);
  begin_with (std_env, local);
  return true;
}

tree
evaluate_typesetter_child (tree t, int i) {
  // the child i of t, evaluated as a part, with the variables which the
  // typesetter sets itself there
  bool local= begin_typesetter_locals (t, i);
  tree r= evaluate_part (t[i]);
  if (local) end_with (std_env);
  return r;
}
