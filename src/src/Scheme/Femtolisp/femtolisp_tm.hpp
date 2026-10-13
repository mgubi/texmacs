/******************************************************************************
* MODULE     : femtolisp_tm.hpp
* DESCRIPTION: Interface to femtolisp
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef FEMTOLISP_TM_H
#define FEMTOLISP_TM_H

#include "tm_configure.hpp"
#include "blackbox.hpp"
#include "array.hpp"

#include "fl_tm.h"

/******************************************************************************
* Scheme objects held by C++
*******************************************************************************
* The garbage collector of femtolisp moves the objects. A tmscm registers
* itself in a list of roots, which the collector updates (see the function
* tmscm_relocate_roots): any tmscm, local variable, temporary, argument or
* member of a C++ object, stays valid across allocations. The errors of
* femtolisp (longjmps) never cross the C++ code: fltm_apply catches them and
* the builtins of the glue raise them only after their C++ objects are gone,
* so that the destructors, which unlink the roots, always run.
******************************************************************************/

struct tmscm_node {
  fltm_value v;
  tmscm_node* prev;
  tmscm_node* next;
};

extern tmscm_node tmscm_roots;

class tmscm: public tmscm_node {
  inline void link () {
    prev= &tmscm_roots; next= tmscm_roots.next;
    next->prev= this; tmscm_roots.next= this; }
public:
  inline tmscm () { v= 0; link (); }
  inline explicit tmscm (fltm_value x) { v= x; link (); }
  inline tmscm (const tmscm& o) { v= o.v; link (); }
  inline ~tmscm () { prev->next= next; next->prev= prev; }
  inline tmscm& operator = (const tmscm& o) { v= o.v; return *this; }
};

bool tmscm_is_blackbox (tmscm obj);
tmscm blackbox_to_tmscm (blackbox b);
blackbox tmscm_to_blackbox (tmscm obj);

inline tmscm tmscm_null () { return tmscm (fltm_nil); }
inline tmscm tmscm_true () { return tmscm (fltm_true); }
inline tmscm tmscm_false () { return tmscm (fltm_false); }

inline fltm_value& fltm_car (fltm_value x) { return FLTM_PTR (x) [0]; }
inline fltm_value& fltm_cdr (fltm_value x) { return FLTM_PTR (x) [1]; }

inline void tmscm_set_car (tmscm a, tmscm b) { fltm_car (a.v)= b.v; }
inline void tmscm_set_cdr (tmscm a, tmscm b) { fltm_cdr (a.v)= b.v; }

inline bool tmscm_is_equal (tmscm o1, tmscm o2) {
  return o1.v == o2.v || fltm_is_equal (o1.v, o2.v); }

inline bool tmscm_is_null (tmscm obj) { return obj.v == fltm_nil; }
inline bool tmscm_is_pair (tmscm obj) {
  return FLTM_TAG (obj.v) == FLTM_TAG_CONS; }
inline bool tmscm_is_list (tmscm obj) { return fltm_is_list (obj.v); }
inline bool tmscm_is_bool (tmscm obj) {
  return obj.v == fltm_true || obj.v == fltm_false; }
inline bool tmscm_is_int (tmscm obj) { return fltm_is_integer (obj.v); }
inline bool tmscm_is_double (tmscm obj) { return fltm_is_number (obj.v); }
inline bool tmscm_is_string (tmscm obj) { return fltm_is_string (obj.v); }
inline bool tmscm_is_symbol (tmscm obj) {
  return FLTM_TAG (obj.v) == FLTM_TAG_SYM; }

inline tmscm tmscm_cons (tmscm obj1, tmscm obj2) {
  return tmscm (fltm_cons (obj1.v, obj2.v)); }
inline tmscm tmscm_car (tmscm obj) { return tmscm (fltm_car (obj.v)); }
inline tmscm tmscm_cdr (tmscm obj) { return tmscm (fltm_cdr (obj.v)); }
inline tmscm tmscm_caar (tmscm obj) {
  return tmscm (fltm_car (fltm_car (obj.v))); }
inline tmscm tmscm_cadr (tmscm obj) {
  return tmscm (fltm_car (fltm_cdr (obj.v))); }
inline tmscm tmscm_cdar (tmscm obj) {
  return tmscm (fltm_cdr (fltm_car (obj.v))); }
inline tmscm tmscm_cddr (tmscm obj) {
  return tmscm (fltm_cdr (fltm_cdr (obj.v))); }
inline tmscm tmscm_caddr (tmscm obj) {
  return tmscm (fltm_car (fltm_cdr (fltm_cdr (obj.v)))); }
inline tmscm tmscm_cadddr (tmscm obj) {
  return tmscm (fltm_car (fltm_cdr (fltm_cdr (fltm_cdr (obj.v))))); }

inline tmscm bool_to_tmscm (bool b) {
  return tmscm (b? fltm_true: fltm_false); }
inline tmscm int_to_tmscm (int i) { return tmscm (fltm_integer (i)); }
inline tmscm long_to_tmscm (long l) { return tmscm (fltm_integer (l)); }
inline tmscm double_to_tmscm (double r) { return tmscm (fltm_double (r)); }
tmscm string_to_tmscm (string s);
tmscm symbol_to_tmscm (string s);

inline bool tmscm_to_bool (tmscm obj) { return obj.v != fltm_false; }
int tmscm_to_int (tmscm obj);
unsigned int tmscm_to_uint (tmscm obj);
inline double tmscm_to_double (tmscm obj) { return fltm_to_double (obj.v); }
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
*******************************************************************************
* A glue function is called as a femtolisp builtin, through tmscm_proc: the
* arguments are copied into tmscm's (the stack of femtolisp may move during
* the call), and an error of the glue code (TMSCM_ASSERT, a C++ exception)
* is raised as a femtolisp error once the C++ objects of the call are gone.
******************************************************************************/

struct tmscm_error {
  const char* key;
  const char* subr;
  const char* message;
  fltm_value obj; // the faulty argument (not moved: no allocation follows)
  bool has_obj;
  tmscm_error (const char* k, const char* s, const char* m):
    key (k), subr (s), message (m), obj (0), has_obj (false) {}
  tmscm_error (const char* k, const char* s, const char* m, fltm_value o):
    key (k), subr (s), message (m), obj (o), has_obj (true) {}
};

void tmscm_check_arity (const char* name, int expected, uint32_t nargs);
void tmscm_raise (const tmscm_error& e)
#ifdef __GNUC__
  __attribute__ ((__noreturn__))
#endif
  ;

// tmscm_error is trivially destructible: the longjmp of tmscm_raise may
// leave this frame with it alive
#define TMSCM_CALL_GLUE(call)                                   \
  fltm_value res= 0;                                            \
  bool failed= false;                                           \
  tmscm_error err ("misc-error", "glue", "C++ exception");      \
  try {                                                         \
    call;                                                       \
  }                                                             \
  catch (const tmscm_error& e) {                                \
    err= e; failed= true;                                       \
  }                                                             \
  catch (...) {                                                 \
    failed= true;                                               \
  }                                                             \
  if (failed) tmscm_raise (err);                                \
  return res;

template<tmscm (*PROC)()>
static fltm_value tmscm_proc (fltm_value* args, uint32_t nargs) {
  (void) args;
  TMSCM_CALL_GLUE (tmscm_check_arity ("glue", 0, nargs);
                   res= PROC ().v);
}

template<tmscm (*PROC)(tmscm)>
static fltm_value tmscm_proc (fltm_value* args, uint32_t nargs) {
  TMSCM_CALL_GLUE (tmscm_check_arity ("glue", 1, nargs);
                   tmscm a1 (args[0]);
                   res= PROC (a1).v);
}

template<tmscm (*PROC)(tmscm, tmscm)>
static fltm_value tmscm_proc (fltm_value* args, uint32_t nargs) {
  TMSCM_CALL_GLUE (tmscm_check_arity ("glue", 2, nargs);
                   tmscm a1 (args[0]); tmscm a2 (args[1]);
                   res= PROC (a1, a2).v);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm)>
static fltm_value tmscm_proc (fltm_value* args, uint32_t nargs) {
  TMSCM_CALL_GLUE (tmscm_check_arity ("glue", 3, nargs);
                   tmscm a1 (args[0]); tmscm a2 (args[1]);
                   tmscm a3 (args[2]);
                   res= PROC (a1, a2, a3).v);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm)>
static fltm_value tmscm_proc (fltm_value* args, uint32_t nargs) {
  TMSCM_CALL_GLUE (tmscm_check_arity ("glue", 4, nargs);
                   tmscm a1 (args[0]); tmscm a2 (args[1]);
                   tmscm a3 (args[2]); tmscm a4 (args[3]);
                   res= PROC (a1, a2, a3, a4).v);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm, tmscm)>
static fltm_value tmscm_proc (fltm_value* args, uint32_t nargs) {
  TMSCM_CALL_GLUE (tmscm_check_arity ("glue", 5, nargs);
                   tmscm a1 (args[0]); tmscm a2 (args[1]);
                   tmscm a3 (args[2]); tmscm a4 (args[3]);
                   tmscm a5 (args[4]);
                   res= PROC (a1, a2, a3, a4, a5).v);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)>
static fltm_value tmscm_proc (fltm_value* args, uint32_t nargs) {
  TMSCM_CALL_GLUE (tmscm_check_arity ("glue", 6, nargs);
                   tmscm a1 (args[0]); tmscm a2 (args[1]);
                   tmscm a3 (args[2]); tmscm a4 (args[3]);
                   tmscm a5 (args[4]); tmscm a6 (args[5]);
                   res= PROC (a1, a2, a3, a4, a5, a6).v);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm)>
static fltm_value tmscm_proc (fltm_value* args, uint32_t nargs) {
  TMSCM_CALL_GLUE (tmscm_check_arity ("glue", 7, nargs);
                   tmscm a1 (args[0]); tmscm a2 (args[1]);
                   tmscm a3 (args[2]); tmscm a4 (args[3]);
                   tmscm a5 (args[4]); tmscm a6 (args[5]);
                   tmscm a7 (args[6]);
                   res= PROC (a1, a2, a3, a4, a5, a6, a7).v);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm,
                       tmscm)>
static fltm_value tmscm_proc (fltm_value* args, uint32_t nargs) {
  TMSCM_CALL_GLUE (tmscm_check_arity ("glue", 8, nargs);
                   tmscm a1 (args[0]); tmscm a2 (args[1]);
                   tmscm a3 (args[2]); tmscm a4 (args[3]);
                   tmscm a5 (args[4]); tmscm a6 (args[5]);
                   tmscm a7 (args[6]); tmscm a8 (args[7]);
                   res= PROC (a1, a2, a3, a4, a5, a6, a7, a8).v);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm,
                       tmscm, tmscm)>
static fltm_value tmscm_proc (fltm_value* args, uint32_t nargs) {
  TMSCM_CALL_GLUE (tmscm_check_arity ("glue", 9, nargs);
                   tmscm a1 (args[0]); tmscm a2 (args[1]);
                   tmscm a3 (args[2]); tmscm a4 (args[3]);
                   tmscm a5 (args[4]); tmscm a6 (args[5]);
                   tmscm a7 (args[6]); tmscm a8 (args[7]);
                   tmscm a9 (args[8]);
                   res= PROC (a1, a2, a3, a4, a5, a6, a7, a8, a9).v);
}

template<tmscm (*PROC)(tmscm, tmscm, tmscm, tmscm, tmscm, tmscm, tmscm,
                       tmscm, tmscm, tmscm)>
static fltm_value tmscm_proc (fltm_value* args, uint32_t nargs) {
  TMSCM_CALL_GLUE (tmscm_check_arity ("glue", 10, nargs);
                   tmscm a1 (args[0]); tmscm a2 (args[1]);
                   tmscm a3 (args[2]); tmscm a4 (args[3]);
                   tmscm a5 (args[4]); tmscm a6 (args[5]);
                   tmscm a7 (args[6]); tmscm a8 (args[7]);
                   tmscm a9 (args[8]); tmscm a10 (args[9]);
                   res= PROC (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10).v);
}

void tmscm_define_builtin (const char* name, fltm_builtin_t f);

#define tmscm_install_procedure(name, func, args, p0, p1) \
  tmscm_define_builtin (name, tmscm_proc<func>)

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
  do { if (TMSCM_UNLIKELY (!(_cond)))                             \
      throw tmscm_error ("wrong-type-arg", _subr,                 \
                         "Wrong type argument", \
                         (_arg).v); } while (0)

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

#define TMSCM_UNSPECIFIED (tmscm (fltm_unspecified))

string scheme_dialect ();

#endif // defined FEMTOLISP_TM_H
