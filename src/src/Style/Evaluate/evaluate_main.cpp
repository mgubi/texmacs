
/******************************************************************************
* MODULE     : evaluate_main.cpp
* DESCRIPTION: standard style evaluation
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "evaluate_main.hpp"
#include "std_environment.hpp"
#include "memorizer.hpp"
#include "vars.hpp"
#include "path.hpp"
#include "analyze.hpp"

environment std_env;
static bool markup_mode= false; // see evaluate_markup
static int evaluate_calls= 0; // evaluations of compound trees
static int evaluate_hits= 0;  // of which found in a memorizer

/******************************************************************************
* Memorizing evaluations
******************************************************************************/

static tree no_tree (UNINIT);

class memorizer;
class evaluate_memorizer_rep: public compound_memorizer_rep {
  environment env_in;
  tree t_in;
  bool markup; // evaluated as markup or as a value (see evaluate_markup)
  environment env_out;
  tree t_out;

public:
  inline evaluate_memorizer_rep (environment env, tree t, bool m):
    env_in (env), t_in (t), markup (m), env_out (), t_out (no_tree) {}
  void print (tm_ostream& out) {
    out << "evaluate_memorizer (" << t_in << ")"; }

  int type () { return MEMORIZE_EVALUATE; }
  int hash () { return weak_hash (env_in) ^ weak_hash (t_in) ^ (int) markup; }
  bool equal (memorizer_rep* mem) {
    evaluate_memorizer_rep* rep= (evaluate_memorizer_rep*) mem;
    return weak_equal (env_in, rep->env_in) && weak_equal (t_in, rep->t_in) &&
           markup == rep->markup; }

  void set_environment (environment env) { env_out= env; }
  environment get_environment () { return env_out; }
  void set_tree (tree t) { t_out= t; }
  tree get_tree () { return t_out; }
};

inline memorizer
evaluate_memorizer (environment env, tree t, bool markup) {
  return (memorizer_rep*) tm_new<evaluate_memorizer_rep> (env, t, markup);
}

/******************************************************************************
* Ip handling
******************************************************************************/

// FIXME: from boxes.hpp
#define DECORATION_RIGHT  (-4)
#define DETACHED          (-5)
inline path decorate_right (path ip) {
  return (is_nil (ip) || (ip->item >= 0))? path (DECORATION_RIGHT, ip): ip; }

void
transfer_ip (tree src, tree dest) {
  ASSERT (obtain_ip (dest) == path (DETACHED), "already has an ip");
  dest->obs= list_observer (ip_observer (obtain_ip (src)), dest->obs);
}

void
decorate_ip (tree ref, path ip) {
  if (obtain_ip (ref) == path (DETACHED)) {
    ref->obs= list_observer (ip_observer (ip), ref->obs);
    if (is_compound (ref)) {
      int i, n= N(ref);
      for (i=0; i<n; i++)
	decorate_ip (ref[i], ip);
    }
  }
}

void
decorate_ip (tree src, tree dest) {
  decorate_ip (dest, decorate_right (obtain_ip (src)));
}

/******************************************************************************
* Interned trees
******************************************************************************/

static inline bool
same_child (tree t1, tree t2) {
  // (an atom of the document, with its place, is only the same as itself:
  // a macro argument substituted in another call, with the same text,
  // would carry the place of the first)
  return weak_equal (t1, t2) ||
         (is_atomic (t1) && is_atomic (t2) && t1->label == t2->label &&
          obtain_ip (t1) == path (DETACHED) &&
          obtain_ip (t2) == path (DETACHED));
}

class intern_memorizer_rep: public memorizer_rep {
  tree t;
  int  h;

public:
  inline intern_memorizer_rep (tree t2): t (t2) {
    // (as same_child: an atom of the document by its address; mixed,
    // since the table of the memorizers uses the low bits)
    unsigned int u= (unsigned int) L(t2);
    for (int i=0; i<N(t); i++) {
      bool text= is_atomic (t[i]) && obtain_ip (t[i]) == path (DETACHED);
      u= u * 1000003u ^ (unsigned int)
        (text? ::hash (t[i]->label): weak_hash (t[i]));
    }
    h= (int) (u ^ (u >> 16)); }
  void print (tm_ostream& out) { out << "intern_memorizer (" << t << ")"; }

  int type () { return MEMORIZE_INTERN; }
  int hash () { return h; }
  bool equal (memorizer_rep* mem) {
    intern_memorizer_rep* rep= (intern_memorizer_rep*) mem;
    if (L(t) != L(rep->t) || N(t) != N(rep->t)) return false;
    for (int i=0; i<N(t); i++)
      if (!same_child (t[i], rep->t[i])) return false;
    return true; }
  tree get_tree () { return t; }
};

tree
intern_tree (tree t) {
  // the tree made before with the same label and the same children (atoms
  // compared by their strings, the others by address), if there is one,
  // else t: a value which is made again although its parts did not change
  // (the cell format of a tformat, the parts of a macro body in which the
  // arguments are substituted, for instance) is then the same tree, and
  // the memorizers keyed by it (begin_with, evaluate) find their results
  if (is_atomic (t)) return t;
  memorizer mem= (memorizer_rep*) tm_new<intern_memorizer_rep> (t);
  return mem->get_tree ();
}

/******************************************************************************
* Partial evaluation
*
* Some values are only known to the typesetter: the size of a typeset box
* (box-info, frame-direct, frame-inverse), what depends on the order of the
* document and on data kept with the buffer (auto-id, which numbers the
* automatic labels, get-attachment, and the label tables of get-binding and
* has-binding, which set-binding fills while typesetting), and the lengths. A length in a unit
* (3fn, 1cm, 1r) is converted by the typesetter at the place where it is
* used: with the scale at which it lays out the document (a typesetting at
* twice the size doubles every unit, but not an absolute tmlen computed
* outside), and for some units with the font, the paragraph, the box (w, h,
* l, b, r, t, which move, shift, resize and clipped bind) or the ornament
* being typeset. So the rewriter does not convert lengths: in a computation
* on lengths, a length with a unit is a residual, and so are the units
* themselves (fn-length and the like). Such a value is a
* residual, and a computation on a residual is a residual too: the computation
* with its evaluated arguments, which the typesetter can finish since the
* macro arguments, values and macros in it have been substituted.
******************************************************************************/

static bool
is_unit_length (string s) {
  // a number followed by a unit, such as 3fn, -0.5w or 1r
  int start= 0, i, n= N(s);
  while ((start+1<n) && (s[start]=='-') && (s[start+1]=='-')) start += 2;
  for (i=start; i<n && !is_locase (s[i]); i++) {}
  if (i == start || i == n || !is_double (s (start, i))) return false;
  for (int j=i; j<n; j++)
    if (!is_locase (s[j])) return false;
  return true;
}

static bool
is_length_unit (tree_label l) {
  return l >= CM_LENGTH && l <= HR_LENGTH;
}

static bool
is_layout_value (tree_label l) {
  if (is_length_unit (l)) return true;
  switch (l) {
  case BOX_INFO:
  case FRAME_DIRECT:
  case FRAME_INVERSE:
  case AUTO_ID:
  case GET_ATTACHMENT:
  case GET_BINDING:
  case HAS_BINDING:
  case _PATTERN:
  case HARD_ID:
    return true;
  default:
    return false;
  }
}

static bool
is_length_computation (tree_label l) {
  // computations whose atomic arguments are read as lengths
  switch (l) {
  case PLUS: case MINUS: case TIMES: case OVER: case DIV: case MOD:
  case MINIMUM: case MAXIMUM:
  case EQUAL: case UNEQUAL: case LESS: case LESSEQ:
  case GREATER: case GREATEREQ:
    return true;
  default:
    return false;
  }
}

static bool
is_strict_computation (tree_label l) {
  // computations which evaluate all their arguments
  if (is_length_computation (l)) return true;
  switch (l) {
  case MATH_SQRT: case EXP: case LOG: case POW: case COS: case SIN: case TAN:
  case MERGE: case LENGTH: case RANGE: case NUMBER: case TRANSLATE:
  case CHANGE_CASE: case IS_TUPLE: case LOOK_UP: case NOT: case XOR:
  case BLEND: case RGB_COLOR: case RGB_ACCESS:
    return true;
  default:
    return false;
  }
}

bool
is_residual (tree t, bool as_length) {
  if (is_atomic (t)) return as_length && is_unit_length (t->label);
  tree_label l= L(t);
  return is_layout_value (l) || is_strict_computation (l) ||
         l == IF || l == VAR_IF || l == CASE || l == OR || l == AND ||
         l == VALUE;
}

bool
contains_residual (tree t) {
  // (a macro is code, not a value which was computed)
  if (is_residual (t)) return true;
  if (is_atomic (t) || L(t) == MACRO || L(t) == XMACRO) return false;
  for (int i=0; i<N(t); i++)
    if (contains_residual (t[i])) return true;
  return false;
}

tree
evaluate_markup (tree t) {
  // the evaluation of t as a part of the markup of the result (a child of
  // a typesetting primitive, the body of a with): the expansion of a macro
  // call of the document is kept in an expand-as with the place of the
  // call, whose boundaries the concater marks (typeset_expand_as), where the
  // cursor is put before and after the tag, as when it typesets the call
  // (typeset_compound). (Not in the values: the computations of the
  // rewriter would see the expand-as; exec of an expand-as gives its body.)
  // It is evaluated as markup: the typesetter typesets some tags otherwise
  // than it executes them (exec), quote and quote-value, which are shown
  // as their source; a macro call, a value, an extern, an eval, the
  // branches of an if, give their result in the same mode (evaluate_result)
  tree r= evaluate_in (t, true);
  if (is_atomic (t) || (L(t) != COMPOUND && L(t) < START_EXTENSIONS)) return r;
  path ip= obtain_ip (t);
  if (ip == path (DETACHED) || is_nil (ip) || ip->item < 0) return r;
  tree w (EXPAND_AS, as_string (L(t)), r);
  transfer_ip (t, w);
  return w;
}

tree
evaluate_part (tree t) {
  // a part of a typesetting primitive, or the body of a with: markup when
  // the primitive is evaluated as markup, a value otherwise (exec of a
  // primitive executes its parts)
  return markup_mode? evaluate_markup (t): evaluate (t);
}

static tree
evaluate_ornamented (tree t, int body) {
  // the typesetter typesets the body of an ornament, a canvas or an art
  // box with margins of its own (bridge_ornamented_rep: par-left and
  // par-right, from the margins outside and the paddings): in the body
  // they are left to it, as references (evaluate_value)
  static tree left (VALUE, PAR_LEFT), right (VALUE, PAR_RIGHT);
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++)
    if (i != body) r[i]= evaluate_part (t[i]);
  if (body >= 0 && body < n) {
    assoc_environment local (2);
    local->raw_write (0, PAR_LEFT, left);
    local->raw_write (1, PAR_RIGHT, right);
    begin_with (std_env, local);
    r[body]= evaluate_part (t[body]);
    end_with (std_env);
  }
  transfer_ip (t, r);
  return r;
}

static tree
evaluate_compound_children (tree t) {
  int i, n= N(t);
  tree r (t, n);
  for (i=0; i<n; i++) r[i]= evaluate_part (t[i]);
  transfer_ip (t, r);
  return r;
}

static tree
evaluate_rewrite_inactive (tree t) {
  // inactive markup (the source shown by inactive, inactive*, error...) is
  // rendered by the typesetter, which reads the types of the arguments in
  // the DRD of the document and the src-* macros of the style
  // (edit_env_rep::rewrite_inactive); its rewrite-inactive only accepts a
  // macro argument, so the source is given to it through a macro applied on
  // the spot, unevaluated (the alternative expansion has substituted it
  // for the arg); evaluate_inactive.cpp, the rewriter's own rendering of
  // 2007, is not used
  if (N(t) < 2) return evaluate_error ("invalid rewrite-inactive");
  tree inner (REWRITE_INACTIVE, tree (ARG, "src-body"), copy (t[1]));
  tree fun (MACRO, "src-body", inner);
  return tree (COMPOUND, fun, t[0]);
}

static tree
evaluate_partially (tree t) {
  // the arguments of a strict computation, evaluated; the computation
  // itself when one of them is a residual, else UNINIT
  static tree uninit (UNINIT);
  int i, n= N(t);
  bool len= is_length_computation (L(t)), residual= false;
  tree r (t, n);
  for (i=0; i<n; i++) {
    r[i]= evaluate (t[i]);
    // (also an argument which holds one: merge "footnote-" with the
    // concat of a counter which a residual branch assigned)
    if (is_residual (r[i], len) ||
        (is_compound (r[i]) && contains_residual (r[i]))) residual= true;
  }
  if (!residual) return uninit;
  transfer_ip (t, r);
  return r;
}

/******************************************************************************
* Main dispatching
******************************************************************************/

tree
evaluate_impl (tree t) {
  //cout << "Really evaluate " << t << LF;
  if (is_length_unit (L(t))) return copy (t);
  if (is_strict_computation (L(t))) {
    // (when no argument is a residual, the computation below evaluates the
    // arguments again, which the memorizers give at once)
    tree r= evaluate_partially (t);
    if (L(r) != UNINIT) return r;
  }
  switch (L(t)) {
  /* Typesetting primitives with side effects */
  case DATOMS:
    return evaluate_formatting (t, ATOM_DECORATIONS);
  case DLINES:
    return evaluate_formatting (t, LINE_DECORATIONS);
  case DPAGES:
    return evaluate_formatting (t, PAGE_DECORATIONS);
  case TFORMAT:
    return evaluate_formatting (t, CELL_FORMAT);
  case TABLE:
    return evaluate_table (t);
  case ORNAMENT:
  case ART_BOX:
    return evaluate_ornamented (t, 0);
  case CANVAS:
    return evaluate_ornamented (t, N(t) - 1);
  /* Primitives for macro expansion */
  case ASSIGN:
    return evaluate_assign (t);
  case PROVIDE:
    return evaluate_assign (t); // FIXME: provisory
  case WITH:
    return evaluate_with (t);
  case PROVIDES:
    return evaluate_provides (t);
  case VALUE:
    return evaluate_value (t);
  case QUOTE_VALUE:
    if (markup_mode) return t;
    return evaluate_quote_value (t);
  case OR_VALUE:
    return evaluate_value (t); // FIXME: provisory
  case MACRO:
    return copy (t);
  case DRD_PROPS:
    return evaluate_drd_props (t);
#ifdef CLASSICAL_MACRO_EXPANSION
  case ARG:
    return evaluate_arg (t);
  case QUOTE_ARG:
    return evaluate_quote_arg (t);
#endif
  case COMPOUND:
    return evaluate_compound (t);
  case XMACRO:
    return copy (t);
  case GET_LABEL:
    return evaluate_get_label (t);
  case GET_ARITY:
    return evaluate_get_arity (t);

  /* Primitives for quoting and evaluation */
  case MAP_ARGS:
    return evaluate_rewrite (t);
  case EVAL_ARGS:
    return evaluate_eval_args (t);
  case MARK:
    return tree (MARK, copy (t[0]), evaluate_part (t[1]));
  case VAR_MARK:
    return tree (VAR_MARK, copy (t[0]), evaluate_part (t[1]));
  case EXPAND_AS:
    return evaluate_result (t[1]);
  case EVAL:
    return evaluate_result (evaluate (t[0]));
  case QUOTE:
    if (markup_mode) return t;
    return t[0];
  case QUASI:
    return evaluate_result (evaluate_quasiquote (t[0]));
  case QUASIQUOTE:
    return evaluate_quasiquote (t[0]);
  case UNQUOTE:
  case VAR_UNQUOTE:
    return evaluate (t[0]);
  case COPY:
    return copy (evaluate (t[0]));    

  /* Control structures */
  case IF:
    return evaluate_if (t);
  case VAR_IF:
    // (if* is typeset as its body, invisible when the condition is false:
    // a phantom, concater_rep::typeset_var_if; it stays, evaluated)
    return evaluate_compound_children (t);
  case CASE:
    return evaluate_case (t);
  case WHILE:
    return evaluate_while (t);
  case FOR_EACH:
    return evaluate_for_each (t);
  case EXTERN:
    return evaluate_rewrite (t);
  case VAR_INCLUDE:
    return evaluate_include (t);
  case WITH_PACKAGE:
    return evaluate_rewrite (t);
  case USE_PACKAGE:
    return evaluate_use_package (t);
  case USE_MODULE:
    return evaluate_use_module (t);

  /* Computational markup */
  case OR:
    return evaluate_or (t);
  case XOR:
    return evaluate_xor (t);
  case AND:
    return evaluate_and (t);
  case NOT:
    return evaluate_not (t);
  case PLUS:
  case MINUS:
    return evaluate_plus_minus (t);
  case TIMES:
  case OVER:
    return evaluate_times_over (t);
  case DIV:
    return evaluate_divide (t);
  case MOD:
    return evaluate_modulo (t);
  case MINIMUM:
  case MAXIMUM:
    return evaluate_min_max (t);
  case MATH_SQRT:
    return evaluate_math_sqrt (t);
  case EXP:
    return evaluate_exp (t);
  case LOG:
    return evaluate_log (t);
  case POW:
    return evaluate_pow (t);
  case COS:
    return evaluate_cos (t);
  case SIN:
    return evaluate_sin (t);
  case TAN:
    return evaluate_tan (t);
  case MERGE:
    return evaluate_merge (t);
  case LENGTH:
    return evaluate_length (t);
  case RANGE:
    return evaluate_range (t);
  case NUMBER:
    return evaluate_number (t);
  case _DATE:
    return evaluate_date (t);
  case TRANSLATE:
    return evaluate_translate (t);
  case CHANGE_CASE:
    return evaluate_change_case (t);
  case FIND_FILE:
    return evaluate_find_file (t);
  case FIND_FILE_UPWARDS:
    return evaluate_find_file_upwards (t);
  case IS_TUPLE:
    return evaluate_is_tuple (t);
  case LOOK_UP:
    return evaluate_lookup (t);
  case OCCURS_INSIDE:
    return evaluate_occurs_inside (t);
  case EQUAL:
    return evaluate_equal (t);
  case UNEQUAL:
    return evaluate_unequal (t);
  case LESS:
    return evaluate_less (t);
  case LESSEQ:
    return evaluate_lesseq (t);
  case GREATER:
    return evaluate_greater (t);
  case GREATEREQ:
    return evaluate_greatereq (t);
  case BLEND:
    return evaluate_blend (t);
  case RGB_COLOR:
    return evaluate_rgb_color (t);
  case RGB_ACCESS:
    return evaluate_rgb_access (t);

  /* Length units */
  case CM_LENGTH:
    return evaluate_cm_length ();
  case MM_LENGTH:
    return evaluate_mm_length ();
  case IN_LENGTH:
    return evaluate_in_length ();
  case PT_LENGTH:
    return evaluate_pt_length ();
  case BP_LENGTH:
    return evaluate_bp_length ();
  case DD_LENGTH:
    return evaluate_dd_length ();
  case PC_LENGTH:
    return evaluate_pc_length ();
  case CC_LENGTH:
    return evaluate_cc_length ();
  case FS_LENGTH:
    return evaluate_fs_length ();
  case FBS_LENGTH:
    return evaluate_fbs_length ();
  case EM_LENGTH:
    return evaluate_em_length ();
  case LN_LENGTH:
    return evaluate_ln_length ();
  case SEP_LENGTH:
    return evaluate_sep_length ();
  case YFRAC_LENGTH:
    return evaluate_yfrac_length ();
  case EX_LENGTH:
    return evaluate_ex_length ();
  case FN_LENGTH:
    return evaluate_fn_length ();
  case FNS_LENGTH:
    return evaluate_fns_length ();
  case BLS_LENGTH:
    return evaluate_bls_length ();
  case FNBOT_LENGTH:
    return evaluate_fnbot_length ();
  case FNTOP_LENGTH:
    return evaluate_fntop_length ();
  case SPC_LENGTH:
    return evaluate_spc_length ();
  case XSPC_LENGTH:
    return evaluate_xspc_length ();
  case PAR_LENGTH:
    return evaluate_par_length ();
  case PAW_LENGTH:
    return evaluate_paw_length ();
  case PAG_LENGTH:
    return evaluate_pag_length ();
  case GW_LENGTH:
    return evaluate_gw_length ();
  case GH_LENGTH:
    return evaluate_gh_length ();
  case GU_LENGTH:
    return evaluate_gu_length ();
  case TMPT_LENGTH:
    return evaluate_tmpt_length ();
  case PX_LENGTH:
    return evaluate_px_length ();
  case MS_LENGTH:
    return evaluate_ms_length ();
  case S_LENGTH:
    return evaluate_s_length ();
  case MSEC_LENGTH:
    return evaluate_msec_length ();
  case SEC_LENGTH:
    return evaluate_sec_length ();
  case MIN_LENGTH:
    return evaluate_min_length ();
  case HR_LENGTH:
    return evaluate_hr_length ();

  /* Primitives for stylesheet editing */
  case FILTER_STYLE:
    return evaluate_filter_style (t);
  case STYLE_WITH:
  case VAR_STYLE_WITH:
    return evaluate_result (t[N(t)-1]);
  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
  case ACTIVE:
  case VAR_ACTIVE:
    return evaluate_compound (t);
  case INACTIVE:
  case VAR_INACTIVE:
    // (source shown by the typesetter, with the layout which the DRD gives
    // to these tags; see evaluate_rewrite_inactive)
    return copy (t);
  case REWRITE_INACTIVE:
    return evaluate_rewrite_inactive (t);

  /* Linking primitives */
  case HARD_ID:
    // (the identifier is made by the typesetter, from its environment and
    // the address of the tree: the rewriter's would change with the address
    // of its environment, at each change before it)
    return evaluate_compound_children (t);
  case SCRIPT:
    return evaluate_script (t);
  case HLINK:
  case ACTION:
    return evaluate_compound (t);
  case SET_BINDING:
    return evaluate_set_binding (t);
  case GET_BINDING:
  case HAS_BINDING:
    // (the label tables are filled by the typesetter, see set-binding)
    return evaluate_compound_children (t);

  /* Graphical primitives */
  case _PATTERN:
    // (the size of a pattern is converted by the typesetter, at its scale,
    // like the lengths; evaluate_pattern computed it here)
    return evaluate_compound_children (t);
  case _POINT:
    return evaluate_point (t);
    /*
  case BOX_INFO:
    return evaluate_box_info (t);
  case FRAME_DIRECT:
    return evaluate_frame_direct (t);
  case FRAME_INVERSE:
    return evaluate_frame_inverse (t);
    */

  /* User extensions */
  default:
    if (L(t) < START_EXTENSIONS) {
      int i, n= N(t);
      tree r (t, n);
      // (data, on which the rewriter computes, holds no expand-as)
      bool data= is_func (t, TUPLE) || is_func (t, ATTR) ||
                 is_func (t, COLLECTION) || is_func (t, ASSOCIATE) ||
                 is_func (t, TMLEN);
      for (i=0; i<n; i++)
	r[i]= data? evaluate (t[i]): evaluate_part (t[i]);
      transfer_ip (t, r);
      return r;
    }
    else {
      tree r= evaluate_compound (t);
      return r;
    }      
  }
}

/******************************************************************************
* Error handling
******************************************************************************/

tree
evaluate_error (string error) {
  return tree (_ERROR, error);
}

tree
evaluate_error (string error, tree arg) {
  return tree (_ERROR, error, arg);
}

tree
evaluate_error (string error, array<tree> args) {
  tree r= tree (_ERROR, error);
  r << args;
  return r;
}

/******************************************************************************
* Main evaluation routines
******************************************************************************/

string
evaluate_string (tree t) {
  tree r= evaluate (t);
  if (is_atomic (r)) return r->label;
  else return "";
}

tree
evaluate_in (tree t, bool markup) {
  if (is_atomic (t)) return t;
  if (style_trace) cout << "Evaluate "
    // << obtain_ip (t) << " "
       << "[" << as_hexadecimal ((pointer) t.operator -> ())
       << ", " << as_hexadecimal ((pointer) std_env.operator -> ()) << "] "
       << t << INDENT << LF;
  evaluate_calls++;
  memorizer mem= evaluate_memorizer (std_env, t, markup);
  if (is_memorized (mem)) {
    evaluate_hits++;
    std_env= mem->get_environment ();
    if (style_trace) cout << UNINDENT << "Memorized "
         << "[" << as_hexadecimal ((pointer) mem->get_tree ().operator -> ())
         << ", " << as_hexadecimal ((pointer) std_env.operator -> ()) << "] "
         << mem->get_tree () << LF;
    return mem->get_tree ();
  }
  memorize_start ();
  bool old_mode= markup_mode;
  markup_mode= markup;
  tree r= evaluate_impl (t);
  markup_mode= old_mode;
  decorate_ip (t, r);
  mem->set_tree (r);
  mem->set_environment (std_env);
  memorize_end ();
  // (the trace gives the addresses of the tree and of the environment after
  // the evaluation, as those before it in "Evaluate")
  if (style_trace) cout << UNINDENT << "Computed "
       << "[" << as_hexadecimal ((pointer) mem->get_tree ().operator -> ())
       << ", " << as_hexadecimal ((pointer) std_env.operator -> ()) << "] "
       << mem->get_tree ()
    // << " at " << obtain_ip (r);
       << LF;
  return mem->get_tree ();
}

tree
evaluate (tree t) {
  return evaluate_in (t, false);
}

tree
evaluate_result (tree t) {
  // the tree whose result is the result of the tree being evaluated (the
  // body of a macro call, a value...), in the same mode
  return evaluate_in (t, markup_mode);
}

memorizer
evaluate (environment env, tree t) {
  environment old_env= std_env;
  evaluate_calls= evaluate_hits= 0;
  memorize_initialize ();
  std_env= env;
  (void) evaluate_in (t, true); // (the document is markup)
  std_env= old_env;
  return memorize_finalize ();
}

array<int>
evaluate_statistics () {
  // of the last evaluation of a document: evaluations, memorized ones,
  // size of the table of memorizers
  array<int> r;
  r << evaluate_calls << evaluate_hits << memorizer_table_size ();
  return r;
}
