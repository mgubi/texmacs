/******************************************************************************
* MODULE     : guile_cpp_tm.hpp
* DESCRIPTION: Interface to guile_cpp (Guile 1.8 rewritten in C++)
* COPYRIGHT  : (C) 2026  PikachuHy and Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GUILE_CPP_TM_H
#define GUILE_CPP_TM_H

#include "tm_configure.hpp"
#include "blackbox.hpp"
#include "array.hpp"

// only the tagged values and the pairs of guile_cpp (included through the
// path of this file: the rest of TeXmacs needs no include flag for it)
#include "guile_cpp/include/guile_cpp/value.hpp"

// the type of Scheme objects: a 64-bit tagged value, passed by value
typedef guile::SCM tmscm;

bool tmscm_is_blackbox (tmscm obj);
tmscm blackbox_to_tmscm (blackbox b);
blackbox tmscm_to_blackbox (tmscm obj);

inline tmscm tmscm_null () { return guile::SCM_EOL; }
inline tmscm tmscm_true () { return guile::SCM_BOOL_T; }
inline tmscm tmscm_false () { return guile::SCM_BOOL_F; }
inline void
tmscm_set_car (tmscm a, tmscm b) {
  guile::as_obj<guile::PairObj> (a)->car= b;
}
inline void
tmscm_set_cdr (tmscm a, tmscm b) {
  guile::as_obj<guile::PairObj> (a)->cdr= b;
}

bool tmscm_is_equal (tmscm o1, tmscm o2);

inline bool tmscm_is_null (tmscm obj) { return obj.is_null (); }
inline bool tmscm_is_pair (tmscm obj) { return guile::is_pair (obj); }
bool tmscm_is_list (tmscm obj);
inline bool tmscm_is_bool (tmscm obj) { return obj.is_bool (); }
bool tmscm_is_int (tmscm obj);
bool tmscm_is_double (tmscm obj);
inline bool tmscm_is_string (tmscm obj) { return guile::is_string (obj); }
inline bool tmscm_is_symbol (tmscm obj) { return guile::is_symbol (obj); }

// guile_cpp scans the C stack conservatively (as Guile 1.8), so that the
// lists which C++ code builds are safe: tmscm_builder only counts
extern int tmscm_builders;
struct tmscm_builder {
  inline tmscm_builder () { tmscm_builders++; }
  inline ~tmscm_builder () { tmscm_builders--; }
};

tmscm tmscm_cons (tmscm o1, tmscm o2);
inline tmscm
tmscm_car (tmscm obj) {
  return guile::as_obj<guile::PairObj> (obj)->car;
}
inline tmscm
tmscm_cdr (tmscm obj) {
  return guile::as_obj<guile::PairObj> (obj)->cdr;
}
inline tmscm tmscm_caar (tmscm obj) { return tmscm_car (tmscm_car (obj)); }
inline tmscm tmscm_cadr (tmscm obj) { return tmscm_car (tmscm_cdr (obj)); }
inline tmscm tmscm_cdar (tmscm obj) { return tmscm_cdr (tmscm_car (obj)); }
inline tmscm tmscm_cddr (tmscm obj) { return tmscm_cdr (tmscm_cdr (obj)); }
inline tmscm tmscm_caddr (tmscm obj) { return tmscm_car (tmscm_cddr (obj)); }
inline tmscm
tmscm_cadddr (tmscm obj) {
  return tmscm_car (tmscm_cddr (tmscm_cdr (obj)));
}

inline tmscm bool_to_tmscm (bool b) { return guile::SCM::from_bool (b); }
inline tmscm int_to_tmscm (int i) { return guile::SCM::from_fixnum (i); }
tmscm long_to_tmscm (long l);
tmscm double_to_tmscm (double r);
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
* Gluing: the glue routines are called directly, with the array of their
* arguments, as the raw builtins of guile_cpp
******************************************************************************/

typedef tmscm (*tmscm_raw_proc) (const tmscm* args, size_t nargs);
void tmscm_install_raw_procedure (const char* name, int req, int opt, int rst,
                                  tmscm_raw_proc fn);

template<tmscm (*PROC) ()> tmscm
tmscm_proc (const tmscm*, size_t) {
  return PROC ();
}
template<tmscm (*PROC) (tmscm)> tmscm
tmscm_proc (const tmscm* a, size_t) {
  return PROC (a[0]);
}
template<tmscm (*PROC) (tmscm, tmscm)> tmscm
tmscm_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1]);
}
template<tmscm (*PROC) (tmscm, tmscm, tmscm)> tmscm
tmscm_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2]);
}
template<tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm)> tmscm
tmscm_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3]);
}
template<tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm, tmscm)> tmscm
tmscm_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3], a[4]);
}
template<tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)> tmscm
tmscm_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3], a[4], a[5]);
}
template<tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm, tmscm, tmscm,
                        tmscm)> tmscm
tmscm_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
}
template<tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm, tmscm, tmscm,
                        tmscm, tmscm)> tmscm
tmscm_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
}
template<tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm, tmscm, tmscm,
                        tmscm, tmscm, tmscm)> tmscm
tmscm_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
}
template<tmscm (*PROC) (tmscm, tmscm, tmscm, tmscm, tmscm, tmscm,
                        tmscm, tmscm, tmscm, tmscm)> tmscm
tmscm_proc (const tmscm* a, size_t) {
  return PROC (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);
}

#define tmscm_install_procedure(name, func, args, p0, p1) \
  tmscm_install_raw_procedure (name, args, p0, p1, tmscm_proc<func>)

#ifdef __GNUC__
#define TMSCM_EXPECT __builtin_expect
#else
#define TMSCM_EXPECT(_expr, _value) (_expr)
#endif

#define TMSCM_LIKELY(_expr)   TMSCM_EXPECT ((_expr), 1)
#define TMSCM_UNLIKELY(_expr) TMSCM_EXPECT ((_expr), 0)

void tmscm_wrong_type_arg (const char* subr, int pos, tmscm arg);

#define TMSCM_ASSERT(_cond, _arg, _pos, _subr) \
  do { if (TMSCM_UNLIKELY (!(_cond))) \
         tmscm_wrong_type_arg (_subr, _pos, _arg); } while (0)

#define TMSCM_ASSERT_LIST(t, pos, subr) \
  TMSCM_ASSERT (tmscm_is_list (t), t, pos, subr)

#define TMSCM_UNSPECIFIED guile::SCM_UNSPECIFIED

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

#define TMSCM_DEFER_INTS
#define TMSCM_ALLOW_INTS

#endif // GUILE_CPP_TM_H
