/******************************************************************************
 * MODULE     : guile_cpp_tm.hpp
 * DESCRIPTION: Interface to guile_cpp (portable C++20 rewrite of Guile 1.8)
 * COPYRIGHT  : (C) 2026  TeXmacs contributors
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef GUILE_CPP_TM_H
#define GUILE_CPP_TM_H

// Include guile_cpp before TeXmacs headers to avoid macro/symbol collisions
#include "guile_cpp/scm.hpp"
#include "guile_cpp/vm.hpp"

#include "tm_configure.hpp"
#include "blackbox.hpp"
#include "array.hpp"

// The type of Scheme objects: 64-bit tagged value passed by value
typedef guile::SCM tmscm;

bool tmscm_is_blackbox (tmscm obj);
tmscm blackbox_to_tmscm (blackbox b);
blackbox tmscm_to_blackbox (tmscm obj);

inline tmscm tmscm_null () { return guile::SCM_EOL; }
inline tmscm tmscm_true () { return guile::SCM_BOOL_T; }
inline tmscm tmscm_false () { return guile::SCM_BOOL_F; }
inline void tmscm_set_car (tmscm a, tmscm b) {
  guile::as_obj<guile::PairObj> (a)->car= b;
}
inline void tmscm_set_cdr (tmscm a, tmscm b) {
  guile::as_obj<guile::PairObj> (a)->cdr= b;
}

inline bool tmscm_is_equal (tmscm o1, tmscm o2) {
  return guile::scm_equal (o1, o2);
}

inline bool tmscm_is_null (tmscm obj) { return obj.is_null (); }
inline bool tmscm_is_pair (tmscm obj) { return guile::is_pair (obj); }
inline bool tmscm_is_list (tmscm obj) { return guile::is_proper_list (obj); }
inline bool tmscm_is_bool (tmscm obj) { return obj.is_bool (); }
bool tmscm_is_int (tmscm obj);
inline bool tmscm_is_double (tmscm obj) { return guile::is_real_number (obj); }
inline bool tmscm_is_string (tmscm obj) { return guile::is_string (obj); }
inline bool tmscm_is_symbol (tmscm obj) { return guile::is_symbol (obj); }

// guile_cpp scans the C++ stack conservatively (as Guile 1.8 does),
// so tmscm_builder is a lightweight counter matching the glue interface.
extern int tmscm_builders;
struct tmscm_builder {
  inline tmscm_builder () { ++tmscm_builders; }
  inline ~tmscm_builder () { --tmscm_builders; }
};

inline tmscm tmscm_cons (tmscm o1, tmscm o2) { return guile::vm ().cons (o1, o2); }
inline tmscm tmscm_car (tmscm obj) { return guile::as_obj<guile::PairObj> (obj)->car; }
inline tmscm tmscm_cdr (tmscm obj) { return guile::as_obj<guile::PairObj> (obj)->cdr; }
inline tmscm tmscm_caar (tmscm obj) { return tmscm_car (tmscm_car (obj)); }
inline tmscm tmscm_cadr (tmscm obj) { return tmscm_car (tmscm_cdr (obj)); }
inline tmscm tmscm_cdar (tmscm obj) { return tmscm_cdr (tmscm_car (obj)); }
inline tmscm tmscm_cddr (tmscm obj) { return tmscm_cdr (tmscm_cdr (obj)); }
inline tmscm tmscm_caddr (tmscm obj) { return tmscm_car (tmscm_cddr (obj)); }
inline tmscm tmscm_cadddr (tmscm obj) { return tmscm_car (tmscm_cddr (tmscm_cdr (obj))); }

inline tmscm bool_to_tmscm (bool b) { return guile::SCM::from_bool (b); }
inline tmscm int_to_tmscm (int i) { return guile::SCM::from_fixnum (i); }
inline tmscm long_to_tmscm (long l) { return guile::vm ().make_integer (static_cast<int64_t> (l)); }
inline tmscm double_to_tmscm (double r) { return guile::vm ().make_real (r); }
tmscm string_to_tmscm (string s);
tmscm symbol_to_tmscm (string s);

inline bool tmscm_to_bool (tmscm obj) { return obj.is_true (); }
int tmscm_to_int (tmscm obj);
unsigned int tmscm_to_uint (tmscm obj);
double tmscm_to_double (tmscm obj);
string tmscm_to_string (tmscm obj);
string tmscm_to_symbol (tmscm obj);

string scheme_dialect ();
string scheme_init_file ();
tmscm eval_scheme_file (string name);
tmscm eval_scheme (string s);
tmscm call_scheme (tmscm fun);
tmscm call_scheme (tmscm fun, tmscm a1);
tmscm call_scheme (tmscm fun, tmscm a1, tmscm a2);
tmscm call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3);
tmscm call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3, tmscm a4);
tmscm call_scheme (tmscm fun, array<tmscm> a);

/******************************************************************************
 * Gluing (direct raw_fn dispatch, zero std::function overhead on calls)
 ******************************************************************************/

template <tmscm (*PROC) ()>
static tmscm gcpp_proc (const tmscm*, size_t) {
  return PROC ();
}
template <tmscm (*PROC) (tmscm)>
static tmscm gcpp_proc (const tmscm* a, size_t) {
  return PROC (a[0]);
}
template <tmscm (*PROC) (tmscm, tmscm)>
static tmscm gcpp_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1]);
}
template <tmscm (*PROC) (tmscm, tmscm, tmscm)>
static tmscm gcpp_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2]);
}
template <tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm)>
static tmscm gcpp_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3]);
}
template <tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm, tmscm)>
static tmscm gcpp_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3], a[4]);
}
template <tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)>
static tmscm gcpp_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3], a[4], a[5]);
}
template <tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)>
static tmscm gcpp_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
}
template <tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)>
static tmscm gcpp_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
}
template <tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)>
static tmscm gcpp_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
}
template <tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)>
static tmscm gcpp_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);
}

inline void gcpp_install_raw_proc (const char* name, int req, int opt, int rst,
                                   guile::RawSubrFn fn) {
  guile::SCM p= guile::vm ().define_proc (name, req, opt, rst != 0, fn);
  guile::as_obj<guile::SubrObj> (p)->raw_fn= fn;
}

#define tmscm_install_procedure(name, func, args, p0, p1)                     \
  gcpp_install_raw_proc (name, args, p0, p1, +gcpp_proc<func>)

#ifdef __GNUC__
# define TMSCM_EXPECT __builtin_expect
#else
# define TMSCM_EXPECT(_expr, _value) (_expr)
#endif

#define TMSCM_LIKELY(_expr)   TMSCM_EXPECT ((_expr), 1)
#define TMSCM_UNLIKELY(_expr) TMSCM_EXPECT ((_expr), 0)

#define TMSCM_ASSERT(_cond, _arg, _pos, _subr)                                \
  do { if (TMSCM_UNLIKELY (!(_cond)))                                         \
    guile::vm ().wrong_type_arg (_subr, _pos, _arg, "valid argument");        \
  } while (0)

#define TMSCM_ASSERT_LIST(t, pos, subr)                                       \
  TMSCM_ASSERT (tmscm_is_list (t), t, pos, subr)

#define TMSCM_UNSPECIFIED guile::SCM_UNSPECIFIED

#define TMSCM_ARG1  1
#define TMSCM_ARG2  2
#define TMSCM_ARG3  3
#define TMSCM_ARG4  4
#define TMSCM_ARG5  5
#define TMSCM_ARG6  6
#define TMSCM_ARG7  7
#define TMSCM_ARG8  8
#define TMSCM_ARG9  9
#define TMSCM_ARG10 10

#define TMSCM_DEFER_INTS ((void) 0)
#define TMSCM_ALLOW_INTS ((void) 0)

#endif // GUILE_CPP_TM_H
