/******************************************************************************
* MODULE     : fl_tm.h
* DESCRIPTION: C interface of femtolisp for TeXmacs
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

/* The C++ code of TeXmacs does not include flisp.h, whose macros (car, ptr,
   symbol, ...) would clash with TeXmacs names: it uses this interface, which
   is implemented in fl_core.c, together with femtolisp itself.

   Femtolisp has a copying garbage collector: an allocation may move every
   Lisp object. The functions marked "allocates" below may collect garbage;
   they protect their own arguments, but other values which the caller holds
   in C variables must be registered with fltm_set_gc_roots (the C++ class
   tmscm does it, see femtolisp_tm.hpp). Errors of femtolisp are longjmps:
   they never leave the functions of this interface, except fltm_raise. */

#ifndef FL_TM_H
#define FL_TM_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef uintptr_t fltm_value;
typedef fltm_value (*fltm_builtin_t) (fltm_value* args, uint32_t nargs);

/* the tags of femtolisp values (see flisp.h) */
#define FLTM_TAG(x)      ((x) & 0x7)
#define FLTM_PTR(x)      ((fltm_value*) ((x) & ~(fltm_value) 0x7))
#define FLTM_TAG_SYM     0x6
#define FLTM_TAG_CONS    0x7
#define FLTM_IS_FIXNUM(x) (((x) & 3) == 0)

extern fltm_value fltm_nil, fltm_true, fltm_false, fltm_unspecified;

/* initialization, with the text of the boot image; returns 0 on success */
int fltm_init (size_t heap_size, const char* boot, size_t boot_length);

/* called by each garbage collection, with the relocation function, to update
   the values held outside femtolisp */
void fltm_set_gc_roots (void (*roots) (fltm_value (*reloc) (fltm_value)));
void fltm_gc (void);
int fltm_is_valid (fltm_value v); /* for debugging */
void fltm_heap_bounds (void** from, void** to, size_t* size);

/* predicates */
int fltm_is_list (fltm_value x);    /* a proper list */
int fltm_is_integer (fltm_value x); /* an exact integer */
int fltm_is_number (fltm_value x);
int fltm_is_string (fltm_value x);
int fltm_is_procedure (fltm_value x);
int fltm_is_equal (fltm_value a, fltm_value b);

/* constructors (allocate) */
fltm_value fltm_cons (fltm_value a, fltm_value b);
fltm_value fltm_integer (int64_t i);
fltm_value fltm_double (double x);
fltm_value fltm_string (const char* s, size_t n);
fltm_value fltm_symbol (const char* s); /* NUL-terminated */

/* accessors */
int64_t fltm_to_integer (fltm_value x);
double fltm_to_double (fltm_value x);
const char* fltm_string_data (fltm_value x);
size_t fltm_string_length (fltm_value x);
const char* fltm_symbol_name (fltm_value x);

/* opaque values, which hold a pointer of the embedding program; to_string
   returns a malloc'ed NUL-terminated string, equal is nonzero when the two
   values are equal, finalize is called when the value is collected */
typedef struct {
  char* (*to_string) (void* p);
  void (*finalize) (void* p);
  int (*equal) (void* p, void* q);
  uintptr_t (*hash) (void* p);
} fltm_opaque_ops;
void* fltm_define_opaque_type (const char* name, fltm_opaque_ops* ops);
fltm_value fltm_make_opaque (void* type, void* p); /* allocates */
int fltm_is_opaque (fltm_value x, void* type);
void* fltm_opaque_pointer (fltm_value x);

/* global variables */
void fltm_define (const char* name, fltm_value v); /* not constant */
fltm_value fltm_lookup (const char* name); /* fltm_false when unbound */
fltm_value fltm_builtin (const char* name, fltm_builtin_t f); /* allocates */

/* applies f to the list args and catches the errors: returns 0 and the result
   in *res, or reports the error and returns 1 and the error object in *res
   (allocates; *res must be a GC root or a C variable) */
int fltm_apply (fltm_value f, fltm_value args, fltm_value* res);

/* reads and evaluates (with the global eval) the expressions of s, as
   fltm_apply; the result is the value of the last expression */
int fltm_eval_string (const char* s, size_t n, fltm_value* res);

/* raises the error (key subr message . args), as Guile's errors; to be
   called only from a builtin, with no C++ object alive in the frames between
   the builtin and the call */
void fltm_raise (const char* key, const char* subr, const char* message,
                 fltm_value args)
#ifdef __GNUC__
  __attribute__ ((__noreturn__))
#endif
  ;

#ifdef __cplusplus
}
#endif

#endif /* FL_TM_H */
