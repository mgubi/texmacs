/******************************************************************************
 * MODULE     : chez_tm.hpp
 * DESCRIPTION: Interface to Chez Scheme
 * COPYRIGHT  : (C) 2020 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef CHEZ_TM_H
#define CHEZ_TM_H

#include "tm_configure.hpp"
#include "blackbox.hpp"
#include "array.hpp"

#include "scheme.h"

// the type of scheme objects
typedef ptr tmscm;

bool tmscm_is_blackbox (tmscm obj);
tmscm blackbox_to_tmscm (blackbox b);
blackbox tmscm_to_blackbox (tmscm obj);

inline tmscm tmscm_null () { return Snil; }
inline tmscm tmscm_true () { return Strue; }
inline tmscm tmscm_false () { return Sfalse; }
inline void tmscm_set_car (tmscm a, tmscm b) { Sset_car (a, b); }
inline void tmscm_set_cdr (tmscm a, tmscm b) { Sset_cdr (a, b); }
	
inline bool tmscm_is_equal (tmscm o1, tmscm o2) {
  return Scall2 (Stop_level_value (Sstring_to_symbol ("equal?")), o1, o2); }

inline bool tmscm_is_null (tmscm obj) { return Snullp (obj); }
inline bool tmscm_is_pair (tmscm obj) { return Spairp (obj); }
inline bool tmscm_is_list (tmscm obj) {
   return Scall1 (Stop_level_value (Sstring_to_symbol ("list?")), obj); }
inline bool tmscm_is_bool (tmscm obj) { return Sbooleanp (obj); }
inline bool tmscm_is_int (tmscm obj) { return Sfixnump (obj); }
inline bool tmscm_is_double (tmscm obj) { return Sflonump (obj); }
inline bool tmscm_is_string (tmscm obj) { return Sstringp (obj); }
inline bool tmscm_is_symbol (tmscm obj) { return Ssymbolp (obj); }

inline tmscm tmscm_cons (tmscm obj1, tmscm obj2) { return Scons (obj1, obj2); }
inline tmscm tmscm_car (tmscm obj) { return Scar (obj); }
inline tmscm tmscm_cdr (tmscm obj) { return Scdr (obj); }
inline tmscm tmscm_caar (tmscm obj) { return Scar (Scar (obj)); }
inline tmscm tmscm_cadr (tmscm obj) { return Scar (Scdr (obj)); }
inline tmscm tmscm_cdar (tmscm obj) { return Scdr (Scar (obj)); }
inline tmscm tmscm_cddr (tmscm obj) { return Scdr (Scdr (obj)); }
inline tmscm tmscm_caddr (tmscm obj) { return Scar (Scdr (Scdr (obj))); }
inline tmscm tmscm_cadddr (tmscm obj) { return Scar (Scdr (Scdr (Scdr (obj)))); }

inline tmscm bool_to_tmscm (bool b) { return Sboolean (b); }
inline tmscm int_to_tmscm (int i) { return Sinteger32 (i); }
inline tmscm long_to_tmscm (long l) { return Sinteger64 (l);  }
inline tmscm double_to_tmscm (double r) { return Sflonum (r); }
tmscm string_to_tmscm (string s);
tmscm symbol_to_tmscm (string s);

inline bool tmscm_to_bool (tmscm obj) { return Sboolean_value (obj); }
inline int tmscm_to_int (tmscm obj) { return Sfixnum_value (obj); }
inline double tmscm_to_double (tmscm obj) { return Sflonum_value (obj); }
string tmscm_to_string (tmscm obj);
string tmscm_to_symbol (tmscm obj);

tmscm eval_scheme_file (string name);
tmscm eval_scheme (string s);
tmscm call_scheme (tmscm fun);
tmscm call_scheme (tmscm fun, tmscm a1);
tmscm call_scheme (tmscm fun, tmscm a1, tmscm a2);
tmscm call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3);
tmscm call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3, tmscm a4);
tmscm call_scheme (tmscm fun, array<tmscm> a);


/******************************************************************************
 * Gluing
 ******************************************************************************/

#define tmscm_install_procedure(name, func, args, p0, p1) \
   Sforeign_symbol (name, (void*) func)

/* The SCM_EXPECT macros provide branch prediction hints to the
   compiler.  To use only in places where the result of the expression
   under "normal" circumstances is known.  */
#ifdef __GNUC__
# define TMSCM_EXPECT    __builtin_expect
#else
# define TMSCM_EXPECT(_expr, _value) (_expr)
#endif

#define TMSCM_LIKELY(_expr)    TMSCM_EXPECT ((_expr), 1)
#define TMSCM_UNLIKELY(_expr)  TMSCM_EXPECT ((_expr), 0)

#define TMSCM_ASSERT(_cond, _arg, _pos, _subr)                    \
  do { if (TMSCM_UNLIKELY (!(_cond)))  {                           \
cout << "error in argumet type " _subr << " " << arg << "\n"; ASSERT(false, ""); } } while (0)


#define TMSCM_ARG1 1
#define TMSCM_ARG2 2
#define TMSCM_ARG3 3
#define TMSCM_ARG4 4
#define TMSCM_ARG5 5
#define TMSCM_ARG6 6
#define TMSCM_ARG7 7
#define TMSCM_ARG8 8
#define TMSCM_ARG9 9
#define TMSCM_ARG10 10

#define TMSCM_UNSPECIFIED ( Svoid ) //FIMXE: is this correct?

string scheme_dialect ();

#endif // defined S7_TM_H


