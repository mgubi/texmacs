
/******************************************************************************
* MODULE     : evaluate_numeric.cpp
* DESCRIPTION: numeric operations
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "evaluate_main.hpp"
#include "colors.hpp"

/******************************************************************************
* Arithmetic operations
******************************************************************************/

tree
evaluate_plus_minus (tree t) {
  int i, n= N(t);
  if (n==0) return evaluate_error ("bad plus/minus");
  tree inc= evaluate (t[0]);
  if (is_double (inc)) {
    double acc= as_double (inc);
    if ((n==1) && is_func (t, MINUS))
      acc= -acc;
    for (i=1; i<n; i++) {
      tree inc= evaluate (t[i]);
      if (!is_double (inc))
	return evaluate_error ("bad plus/minus");
      if ((i == n-1) && is_func (t, MINUS))
	acc -= as_double (inc);
      else acc += as_double (inc);
    }
    return as_string (acc);
  }
  else if (is_anylen (inc)) {
    tree acc= as_tmlen (inc);
    if ((n==1) && is_func (t, MINUS))
      acc= tmlen_times (-1, acc);
    for (i=1; i<n; i++) {
      tree inc= evaluate (t[i]);
      if (!is_anylen (inc))
	return evaluate_error ("bad plus/minus");
      inc= as_tmlen (inc);
      if ((i == n-1) && is_func (t, MINUS))
	inc= tmlen_times (-1, inc);
      acc= tmlen_plus (acc, inc);
    }
    return acc;
  }
  else return evaluate_error ("bad plus/minus");
}

tree
evaluate_times_over (tree t) {
  int i, n= N(t);
  if (n==0) return evaluate_error ("bad times/over");
  tree prod= evaluate (t[0]);
  if (is_double (prod));
  else if (is_anylen (prod)) prod= as_tmlen (prod);
  else if (is_percentage (prod)) prod= as_tree (as_percentage (prod));
  else return evaluate_error ("bad times/over");
  if ((n==1) && is_func (t, OVER)) {
    if (is_double (prod)) return as_string (1 / as_double (prod));
    else return evaluate_error ("bad times/over");
  }
  // cout << t << "\n";
  // cout << "  0\t" << prod << "\n";
  for (i=1; i<n; i++) {
    tree mul= evaluate (t[i]);
    if (is_double (mul)) {
      double _mul= as_double (mul);
      if ((i == n-1) && is_func (t, OVER))
	_mul= 1 / _mul;
      if (is_double (prod))
	prod= as_string (_mul * as_double (prod));
      else prod= tmlen_times (_mul, prod);
    }
    else if (is_anylen (mul)) {
      mul= as_tmlen (mul);
      if ((i == n-1) && is_func (t, OVER)) {
	if (!is_func (prod, TMLEN))
	  return evaluate_error ("bad times/over");
	return tmlen_over (prod, mul);
      }
      if (is_double (prod))
	prod= tmlen_times (as_double (prod), mul);
      else return evaluate_error ("bad times/over");
    }
    else if (is_percentage (mul)) {
      double _mul= as_percentage (mul);
      if (is_double (prod))
	prod= as_string (_mul * as_double (prod));
      else prod= tmlen_times (_mul, prod);
    }
    else return evaluate_error ("bad times/over");
    // cout << "  " << i << "\t" << prod << "\n";
  }
  return prod;
}

tree
evaluate_divide (tree t) {
  if (N(t) != 2) return evaluate_error ("bad divide");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return evaluate_error ("bad divide");
  if (is_int (t1->label) && (is_int (t2->label))) {
    int den= as_int (t2->label);
    if (den == 0) return evaluate_error ("division by zero");
    return as_string (as_int (t1->label) / den);
  }
  return evaluate_error ("bad divide");
}

tree
evaluate_modulo (tree t) {
  if (N(t)!=2) return evaluate_error ("bad modulo");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return evaluate_error ("bad modulo");
  if (is_int (t1->label) && (is_int (t2->label))) {
    int den= as_int (t2->label);
    if (den == 0) return evaluate_error ("modulo zero");
    return as_string (as_int (t1->label) % den);
  }
  return evaluate_error ("bad modulo");
}

tree
evaluate_min_max (tree t) {
  int i, n= N(t);
  if (n==0) return evaluate_error ("bad min/max");
  tree first= evaluate (t[0]);
  if (is_double (first)) {
    double ret= as_double (first);
    for (i=1; i<n; i++) {
      tree next= evaluate (t[i]);
      if (!is_double (next))
	return evaluate_error ("bad min/max");
      if (is_func (t, MINIMUM))
	ret= min (ret, as_double (next));
      else
	ret= max (ret, as_double (next));
    }
    return as_string (ret);
  }
  else if (is_anylen (first)) {
    tree ret= as_tmlen (first);
    for (i=1; i<n; i++) {
      tree next= evaluate (t[i]);
      if (!is_anylen (next))
	return evaluate_error ("bad min/max");
      next= as_tmlen (next);
      if (is_func (t, MINIMUM))
	ret= tmlen_min (ret, next);
      else
	ret= tmlen_max (ret, next);
    }
    return ret;
  }
  else return evaluate_error ("bad min/max");
}

/******************************************************************************
* Elementary functions
******************************************************************************/

tree
evaluate_math_sqrt (tree t) {
  if (N(t)!=1) return evaluate_error ("bad sqrt");
  tree t1= evaluate (t[0]);
  if (is_double (t1))
    return as_tree (sqrt (as_double (t1)));
  return evaluate_error ("bad sqrt");
}

tree
evaluate_exp (tree t) {
  if (N(t)!=1) return evaluate_error ("bad exp");
  tree t1= evaluate (t[0]);
  if (is_double (t1))
    return as_tree (exp (as_double (t1)));
  return evaluate_error ("bad exp");
}

tree
evaluate_log (tree t) {
  if (N(t)!=1) return evaluate_error ("bad log");
  tree t1= evaluate (t[0]);
  if (is_double (t1))
    return as_tree (log (as_double (t1)));
  return evaluate_error ("bad log");
}

tree
evaluate_pow (tree t) {
  if (N(t)!=2) return evaluate_error ("bad pow");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (is_double (t1) && is_double (t2))
    return as_tree (pow (as_double (t1), as_double (t2)));
  return evaluate_error ("bad pow");
}

tree
evaluate_cos (tree t) {
  if (N(t)!=1) return evaluate_error ("bad cos");
  tree t1= evaluate (t[0]);
  if (is_double (t1))
    return as_tree (cos (as_double (t1)));
  return evaluate_error ("bad cos");
}

tree
evaluate_sin (tree t) {
  if (N(t)!=1) return evaluate_error ("bad sin");
  tree t1= evaluate (t[0]);
  if (is_double (t1))
    return as_tree (sin (as_double (t1)));
  return evaluate_error ("bad sin");
}

tree
evaluate_tan (tree t) {
  if (N(t)!=1) return evaluate_error ("bad tan");
  tree t1= evaluate (t[0]);
  if (is_double (t1))
    return as_tree (tan (as_double (t1)));
  return evaluate_error ("bad tan");
}

/******************************************************************************
* Colors
******************************************************************************/

tree
evaluate_blend (tree t) {
  if (N(t)!=2) return evaluate_error ("bad blend");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return evaluate_error ("bad blend");
  string s1= t1->label;
  string s2= t2->label;
  if (is_color_name (s1) && (is_color_name (s2))) {
    color c1= named_color (s1);
    color c2= named_color (s2);
    return get_hex_color (blend_colors (c1, c2));
  }
  return evaluate_error ("bad blend");
}

tree
evaluate_rgb_color (tree t) {
  if (N(t)!=3 && N(t)!=4) return evaluate_error ("bad rgb-color");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  tree t3= evaluate (t[2]);
  tree t4= (N(t)==4? evaluate (t[3]): tree ("255"));
  if (!(is_int (t1) && is_int (t2) && is_int (t3) && is_int (t4)))
    return evaluate_error ("bad rgb-color");
  color c= rgb_color (as_int (t1), as_int (t2), as_int (t3), as_int (t4));
  return get_hex_color (c);
}

tree
evaluate_rgb_access (tree t) {
  if (N(t)!=2) return evaluate_error ("bad rgb-access");
  tree t1= evaluate (t[0]);
  tree t2= evaluate (t[1]);
  if (is_compound (t1) || is_compound (t2))
    return evaluate_error ("bad rgb-access");
  string s1= t1->label;
  if (!(is_color_name (s1))) return evaluate_error ("bad rgb-access");
  color c= named_color (s1);
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  if (starts (t2->label, "r") || t2->label == "0") return as_string (r);
  if (starts (t2->label, "g") || t2->label == "1") return as_string (g);
  if (starts (t2->label, "b") || t2->label == "2") return as_string (b);
  if (starts (t2->label, "a") || t2->label == "3") return as_string (a);
  return evaluate_error ("bad rgb-access");
}
