
/******************************************************************************
 * MODULE     : chibi_tm.cpp
 * DESCRIPTION: Interface to Chibi
 * COPYRIGHT  : (C) 2018 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef CHIBI_TM_HPP
#define CHIBI_TM_HPP

#define SEXP_USE_DEBUG_GC 1

#include "string.hpp"
#include "array.hpp"
#include "blackbox.hpp"

#include <chibi/eval.h>

typedef sexp tmscm;


//inline bool is_blackbox(tmscm p)    { return is_blackbox(p); }
//inline blackbox blackboxvalue(tmscm p) { return static_cast<cell_tm*>(p)->blackboxvalue(); }
//inline void set_blackbox(tmscm p, blackbox b) { return static_cast<cell_tm*>(p)->set_blackbox(b); }



/**** Interfacing to TeXmacs *****/


extern tmscm scheme_context;


inline tmscm tmscm_null () { return SEXP_NULL; }
inline tmscm tmscm_true () { return SEXP_TRUE; }
inline tmscm tmscm_false () { return SEXP_FALSE; }

inline bool tmscm_is_equal(tmscm o1, tmscm o2) { return (o1 == o2); }

inline bool tmscm_is_null (tmscm obj) { return (obj == tmscm_null()); }
inline bool tmscm_is_pair (tmscm obj) { return sexp_pairp (obj); }
inline bool tmscm_is_list (tmscm obj) { return tmscm_is_pair (obj) || tmscm_is_null (obj); }
inline bool tmscm_is_bool (tmscm obj) { return ((obj == tmscm_true ()) || (obj == tmscm_false ())); }
inline bool tmscm_is_int (tmscm obj) { return sexp_fixnump (obj); }
inline bool tmscm_is_double (tmscm obj) { return sexp_flonump (obj); }
inline bool tmscm_is_string (tmscm obj) { return sexp_stringp (obj); }
inline bool tmscm_is_symbol (tmscm obj) { return sexp_symbolp (obj); }

inline tmscm tmscm_cons (tmscm obj1, tmscm obj2) { return sexp_cons (scheme_context,obj1,obj2); }
inline tmscm tmscm_car (tmscm obj) { return sexp_car (obj); }
inline tmscm tmscm_cdr (tmscm obj) { return sexp_cdr (obj); }
inline tmscm tmscm_caar (tmscm obj) { return tmscm_car (tmscm_car (obj)); }
inline tmscm tmscm_cadr (tmscm obj) { return tmscm_car (tmscm_cdr (obj)); }
inline tmscm tmscm_cdar (tmscm obj) { return tmscm_cdr (tmscm_car (obj)); }
inline tmscm tmscm_cddr (tmscm obj) { return tmscm_cdr (tmscm_cdr (obj)); }
inline tmscm tmscm_caddr (tmscm obj) { return tmscm_cadr (tmscm_cdr (obj)); }
inline tmscm tmscm_cadddr (tmscm obj) { return tmscm_caddr (tmscm_cdr (obj)); }

void print_result (sexp ctx, sexp env, sexp res);

inline void tmscm_set_car (tmscm obj, tmscm obj2) {
    if (sexp_immutablep(obj)) {
        cout << "set-car!: immutable pair\n";
        print_result(scheme_context, sexp_context_env(scheme_context), obj);
        return;
    }
    sexp_car(obj) = obj2;
}

inline void tmscm_set_cdr (tmscm obj, tmscm obj2) {
    if (sexp_immutablep(obj)) {
        cout << "set-cdr!: immutable pair\n";
        return;
    }
   sexp_cdr(obj) = obj2;
}



inline tmscm bool_to_tmscm (bool b) { return b ? tmscm_true () : tmscm_false (); }
inline tmscm int_to_tmscm (int i) {   return sexp_make_integer (scheme_context, i); }
inline tmscm long_to_tmscm (long i) {   return sexp_make_integer (scheme_context, i); }
inline tmscm double_to_tmscm (double i) { return sexp_make_flonum (scheme_context, i); }
tmscm string_to_tmscm (string s);
tmscm symbol_to_tmscm (string s);

inline bool tmscm_to_bool (tmscm obj) { return (obj != tmscm_false()); }
inline int tmscm_to_int (tmscm obj) { return sexp_unbox_fixnum (obj); }
inline double tmscm_to_double (tmscm obj) { return sexp_flonum_value (obj); }
inline string tmscm_to_string (tmscm obj) { return string ( sexp_string_data (obj),  sexp_string_size (obj)); }
inline string tmscm_to_symbol (tmscm obj) {
    sexp_gc_var1(out);
    sexp_gc_preserve1(scheme_context, out);
    out = sexp_symbol_to_string_op(scheme_context, SEXP_NULL, 0, obj);
    string res = tmscm_to_string(out);
    sexp_gc_release1(scheme_context);
    return res;
}


bool tmscm_is_blackbox (tmscm obj);
tmscm blackbox_to_tmscm (blackbox b);
blackbox tmscm_to_blackbox (tmscm obj);


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


template<tmscm (*PROC)()>
static tmscm proc (tmscm cxt, tmscm env, sexp_sint_t n) {
    tmscm res = PROC();
    return (res);
}
template<tmscm (*PROC)(tmscm)>
static tmscm proc (tmscm cxt, tmscm env, sexp_sint_t n, tmscm a1) {
    tmscm res = PROC(a1);
    return (res);
}


template<tmscm (*PROC)(tmscm, tmscm)>
static tmscm proc (tmscm cxt, tmscm env, sexp_sint_t n, tmscm a1, tmscm a2) {
    tmscm res = PROC(a1,a2);
    return (res);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm)>
static tmscm proc (tmscm cxt, tmscm env, sexp_sint_t n, tmscm a1, tmscm a2, tmscm a3) {
    tmscm res = PROC(a1,a2,a3);
    return (res);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm)>
static tmscm proc (tmscm cxt, tmscm env, sexp_sint_t n, tmscm a1, tmscm a2, tmscm a3, tmscm a4) {
    tmscm res = PROC(a1,a2,a3,a4);
    return (res);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm, tmscm)>
static tmscm proc (tmscm cxt, tmscm env, sexp_sint_t n, tmscm a1, tmscm a2, tmscm a3, tmscm a4, tmscm a5) {
    tmscm res = PROC(a1,a2,a3,a4,a5);
    return (res);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)>
static tmscm proc (tmscm cxt, tmscm env, sexp_sint_t n, tmscm a1, tmscm a2, tmscm a3, tmscm a4, tmscm a5, tmscm a6) {
    tmscm res = PROC(a1,a2,a3,a4,a5,a6);
    return (res);
}


template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)>
static tmscm proc (tmscm cxt, tmscm env, sexp_sint_t n, tmscm a1, tmscm a2, tmscm a3, tmscm a4, tmscm a5, tmscm a6, tmscm a7) {
    tmscm res = PROC(a1,a2,a3,a4,a5,a6,a7);
    return (res);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)>
static tmscm proc (tmscm cxt, tmscm env, sexp_sint_t n, tmscm a1, tmscm a2, tmscm a3, tmscm a4, tmscm a5, tmscm a6, tmscm a7, tmscm a8) {
    tmscm res = PROC(a1,a2,a3,a4,a5,a6,a7,a8);
    return (res);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)>
static tmscm proc (tmscm cxt, tmscm env, sexp_sint_t n, tmscm a1, tmscm a2, tmscm a3, tmscm a4, tmscm a5, tmscm a6, tmscm a7, tmscm a8, tmscm a9) {
    tmscm res = PROC(a1,a2,a3,a4,a5,a6,a7,a8,a9);
    return (res);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)>
static tmscm proc (tmscm cxt, tmscm env, sexp_sint_t n, tmscm a1, tmscm a2, tmscm a3, tmscm a4, tmscm a5, tmscm a6, tmscm a7, tmscm a8, tmscm a9, tmscm a10) {
    tmscm res = PROC(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
    return (res);
}


typedef sexp_proc1 tmscm_foreign_func;
string scheme_dialect ();
void tmscm_define_glue(const char *name, int args, tmscm_foreign_func f);

#define tmscm_install_procedure(name, func, args, p0, p1) tmscm_define_glue( name, args, (tmscm_foreign_func)proc<func> )





#define SCM_ARG1
#define SCM_ARG2
#define SCM_ARG3
#define SCM_ARG4
#define SCM_ARG5
#define SCM_ARG6
#define SCM_ARG7
#define SCM_ARG8

#define TMSCM_ASSERT(_cond, _arg, _pos, _subr)

#define TMSCM_UNSPECIFIED SEXP_VOID


#endif /* CHIBI_TM_HPP */
