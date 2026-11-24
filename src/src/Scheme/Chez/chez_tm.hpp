/******************************************************************************
 * MODULE     : chez_tm.hpp
 * DESCRIPTION: Interface to the Chez Scheme kernel (C API)
 * COPYRIGHT  : (C) 2026
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
#include "string.hpp"

/* The Chez Scheme kernel API header ("scheme.h").  The include directory
   must be supplied by the build system (e.g. -I/usr/lib/csv<version>/<mach>).
   "scheme.h" defines the kernel types (ptr, iptr, uptr), the accessor
   macros (Scar, Scdr, Sstring_ref, ...) and the exported functions
   (Sscheme_init, Sregister_boot_file, Sbuild_heap, Scall0..Scall3, ...).
   See examples/crepl.c in the Chez Scheme distribution for the standard
   embedding pattern that we follow here. */
#include "scheme.h"

/* ------------------------------------------------------------------------
 * The engine-specific type: a Chez Scheme object is a kernel "ptr".
 * Everything which depends on the Scheme engine should be in this file
 * and in chez_tm.cpp, mirroring the structure of guile_tm.hpp/cpp.
 * ---------------------------------------------------------------------- */

typedef ptr tmscm;

/* -- global engine state (defined in chez_tm.cpp) ------------------------ */

extern tmscm tm_null;              /* the value of '()                        */
extern tmscm tm_blackbox_rtd;       /* RTD for tm-blackbox records            */
extern tmscm tm_make_blackbox;      /* (tm-make-blackbox addr)                */
extern "C" void chez_cache_runtime_procedures ();
extern tmscm tm_eval_string_proc;  /* (tm-eval-string s)                      */
extern tmscm tm_load_file_proc;    /* (tm-load-file file)                     */
extern tmscm tm_call_0_proc;       /* (tm-call-0 fun)                         */
extern tmscm tm_call_1_proc;       /* (tm-call-1 fun a1)                      */
extern tmscm tm_call_2_proc;       /* (tm-call-2 fun a1 a2)                   */
extern tmscm tm_call_3_proc;       /* (tm-call-3 fun a1 a2 a3)                */
extern tmscm tm_call_4_proc;       /* (tm-call-4 fun a1 a2 a3 a4)             */
extern tmscm tm_call_n_proc;       /* (tm-call-n fun args)                    */
extern tmscm tm_equal_pred;        /* equal?                                   */
extern tmscm tm_string_to_symbol;  /* string->symbol                          */
tmscm chez_bytes_to_string (const char* s, iptr n);

/* -- shadow root stack for GC-safe C++ -> Scheme conversions -------------- */

extern tmscm chez_root_stack_vec;  /* permanently locked Scheme vector      */
extern iptr  chez_root_stack_top;  /* next free slot index (0-based)        */
extern iptr  chez_root_stack_cap;  /* current vector length                 */
void chez_root_stack_grow ();      /* cold path: lazy-init or double size   */

#if defined(__GNUC__) || defined(__clang__)
#define CHEZ_UNLIKELY(x) __builtin_expect (!!(x), 0)
#else
#define CHEZ_UNLIKELY(x) (x)
#endif

/* RAII handle reserving a LIFO slot in chez_root_stack_vec.
   Must be stack-allocated with strict LIFO lifetimes; do not heap-allocate
   or destroy out of order (deleting a lower slot sweeps all slots above it).
   Slots are addressed by integer index so vector growth is transparent to
   active caller frames (including recursive and re-entrant conversions).
   Unused and popped slots always hold Sfixnum(0): GC skips fixnums and
   S_dirty_set skips its SegInfo/alloc_mutex write barrier for fixnums. */
class ChezRoot {
  iptr slot;
public:
  inline ChezRoot () {
    if (CHEZ_UNLIKELY (chez_root_stack_top >= chez_root_stack_cap))
      chez_root_stack_grow ();
    slot= chez_root_stack_top++;
  }
  inline explicit ChezRoot (tmscm val) {
    if (CHEZ_UNLIKELY (chez_root_stack_top >= chez_root_stack_cap))
      chez_root_stack_grow ();
    slot= chez_root_stack_top++;
    if (val != Sfixnum (0))
      Svector_set (chez_root_stack_vec, slot, val);
  }
  inline ~ChezRoot () {
    if (CHEZ_UNLIKELY (chez_root_stack_top != slot + 1)) {
      for (iptr i= slot + 1; i < chez_root_stack_top; i++)
        Svector_set (chez_root_stack_vec, i, Sfixnum (0));
    }
    Svector_set (chez_root_stack_vec, slot, Sfixnum (0));
    chez_root_stack_top= slot;
  }
  inline tmscm get () const { return Svector_ref (chez_root_stack_vec, slot); }
  inline void set (tmscm val) { Svector_set (chez_root_stack_vec, slot, val); }
  ChezRoot (const ChezRoot&) = delete;
  ChezRoot& operator= (const ChezRoot&) = delete;
  static void* operator new (size_t) = delete;
  static void* operator new[] (size_t) = delete;
};

/* -- basic predicates and constructors (all inline, kernel macros) -------- */

inline tmscm tmscm_null () { return tm_null; }
inline tmscm tmscm_true () { return Sboolean (1); }
inline tmscm tmscm_false () { return Sboolean (0); }

inline bool tmscm_is_null (tmscm obj) { return Snullp (obj); }
inline bool tmscm_is_pair (tmscm obj) { return Spairp (obj); }
inline bool tmscm_is_bool (tmscm obj) { return Sbooleanp (obj); }
inline bool tmscm_is_int (tmscm obj)   { return Sfixnump (obj) || Sbignump (obj); }
// an exact integer is accepted where a double is expected, as by s7
inline bool tmscm_is_double (tmscm obj) {
  return Sflonump (obj) || Sfixnump (obj) || Sbignump (obj); }
inline bool tmscm_is_string (tmscm obj) { return Sstringp (obj); }
inline bool tmscm_is_symbol (tmscm obj) { return Ssymbolp (obj); }
bool tmscm_is_list (tmscm obj);   /* needs a call to list?; defined in cpp  */
bool tmscm_is_equal (tmscm o1, tmscm o2); /* needs equal?; defined in cpp   */

/* The conversions of glue.cpp root their partial results with ChezRoot,
   there is no collector to switch off as for S7 */
struct tmscm_builder { inline tmscm_builder () {} inline ~tmscm_builder () {} };
inline tmscm tmscm_cons (tmscm obj1, tmscm obj2) { return Scons (obj1, obj2); }
inline tmscm tmscm_car (tmscm obj) { return Scar (obj); }
inline tmscm tmscm_cdr (tmscm obj) { return Scdr (obj); }
inline tmscm tmscm_caar (tmscm obj) { return Scar (Scar (obj)); }
inline tmscm tmscm_cadr (tmscm obj) { return Scar (Scdr (obj)); }
inline tmscm tmscm_cdar (tmscm obj) { return Scdr (Scar (obj)); }
inline tmscm tmscm_cddr (tmscm obj) { return Scdr (Scdr (obj)); }
inline tmscm tmscm_caddr (tmscm obj) { return Scar (Scdr (Scdr (obj))); }
inline tmscm tmscm_cadddr (tmscm obj) { return Scar (Scdr (Scdr (Scdr (obj)))); }

inline void tmscm_set_car (tmscm a, tmscm b) { Sset_car (a, b); }
inline void tmscm_set_cdr (tmscm a, tmscm b) { Sset_cdr (a, b); }

/* -- conversions between C++ and Scheme values ---------------------------- */

inline tmscm bool_to_tmscm (bool b) { return Sboolean (b); }
inline tmscm int_to_tmscm (int i) { return Sinteger (i); }
inline tmscm long_to_tmscm (long l) { return Sinteger ((iptr) l); }
inline tmscm double_to_tmscm (double d) { return Sflonum (d); }

tmscm string_to_tmscm (string s);
tmscm symbol_to_tmscm (string s);
extern const char* current_glue_proc;
inline bool tmscm_to_bool (tmscm obj) { return Sboolean_value (obj); }
// as Guile's scm_to_int, out-of-range rather than truncate an integer which
// does not fit (the glue shims report the exception, tm-glue-raise gives the
// key out-of-range)
inline int  tmscm_to_int (tmscm obj)  {
  if (Sfixnump (obj)) {
    iptr i= Sfixnum_value (obj);
    if (i >= -2147483647 - 1 && i <= 2147483647) return (int) i;
  }
  throw string ("Value out of range in ") *
        string (current_glue_proc ? current_glue_proc : "tmscm_to_int");
}
inline unsigned int tmscm_to_uint (tmscm obj) {
  if (Sfixnump (obj)) {
    iptr i= Sfixnum_value (obj);
    if (i >= 0 && i <= 4294967295LL) return (unsigned int) i;
  }
  throw string ("Value out of range in ") *
        string (current_glue_proc ? current_glue_proc : "tmscm_to_uint");
}
inline double tmscm_to_double (tmscm obj) {
  if (Sflonump (obj)) return Sflonum_value (obj);
  if (Sfixnump (obj)) return (double) Sfixnum_value (obj);
  return (double) Sinteger64_value (obj); }
string tmscm_to_string (tmscm obj);
string tmscm_to_symbol (tmscm obj);

/* -- blackboxes (opaque C++ values kept inside Scheme objects) ------------ */
/* We represent a blackbox as a Chez Scheme record (tm-blackbox) containing an
   integer address token (ptr) pointing to a heap-allocated copy of the C++
   blackbox.
   Because a record is a first-class, non-pair, non-list Chez Scheme datatype,
   polymorphic Scheme functions (e.g. tm-car, tm-cdr, pair?, list?) correctly
   distinguish blackboxes from lists and pairs, and Chez's native equal? /
   equal-hash dispatch to C++ equality/hashing via record-type-equal-procedure
   and record-type-hash-procedure.
   Lifetime is managed via a Chez guardian (tm-blackbox-guardian): upon
   creation, every blackbox is registered with the guardian, and reclaimed
   when unreachable by reap-blackboxes! (boot-chez.scm), which calls
   tmscm_delete_blackbox. */

bool tmscm_is_blackbox (tmscm obj);
tmscm blackbox_to_tmscm (blackbox b);
blackbox tmscm_to_blackbox (tmscm obj);
extern "C" void tmscm_delete_blackbox (void* ptr);

/* -- evaluation and calls -------------------------------------------------- */

tmscm eval_scheme_file (string name);
tmscm eval_scheme (string s);
tmscm call_scheme (tmscm fun);
tmscm call_scheme (tmscm fun, tmscm a1);
tmscm call_scheme (tmscm fun, tmscm a1, tmscm a2);
tmscm call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3);
tmscm call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3, tmscm a4);
tmscm call_scheme (tmscm fun, array<tmscm> a);

/* -- native glue registration (src/Scheme/Chez/glue_chez.cpp) ------------- */

/* Register the foreign symbol tm_glue_table, the table of the glue routines,
   with Sforeign_symbol().  That table is plain C memory, so this runs from
   custom_init() inside Sbuild_heap(), before any boot file is evaluated;
   boot-chez.scm then defines the routines from it.  Idempotent. */
void initialize_glue_chez ();

/* Number of registered entry points, for the start-up diagnostic. */
int chez_glue_procedure_count ();

// initialize_glue() (glue.cpp) installs nothing: the routines come from
// the table of glue_chez.cpp
#define tmscm_install_procedure(name, func, args, p0, p1) (void) 0

void tmscm_wrong_type_arg (tmscm arg, int pos, const char* subr);
#define TMSCM_ASSERT(_cond, _arg, _pos, _subr) \
  do { if (!(_cond)) tmscm_wrong_type_arg ((_arg), (_pos), (_subr)); } while (0)

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

#define TMSCM_UNSPECIFIED Svoid

string scheme_dialect ();

#endif /* defined CHEZ_TM_H */
