/******************************************************************************
* MODULE     : femtolisp_tm.cpp
* DESCRIPTION: Interface to femtolisp
* COPYRIGHT  : (C) 2026  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "femtolisp_tm.hpp"
#include "blackbox.hpp"
#include "file.hpp"
#include "../Scheme/glue.hpp"
#include "convert.hpp" // tree_to_texmacs (should not belong here)
#include "analyze.hpp"
#include "fl_boot.h"

#include <stdlib.h>
#include <string.h>
#include <unistd.h> // for getpid
#ifdef HAVE_GETTIMEOFDAY
#include <sys/time.h>
#else
#include <sys/timeb.h>
#endif

/******************************************************************************
* The roots: the Scheme objects held by C++
******************************************************************************/

tmscm_node tmscm_roots= { 0, &tmscm_roots, &tmscm_roots };

static bool tmscm_check_roots= (getenv ("TEXMACS_FL_CHECK_ROOTS") != NULL);

static int64_t tmscm_gc_count= 0;

static void
tmscm_relocate_roots (fltm_value (*reloc) (fltm_value)) {
  tmscm_gc_count++;
  if (tmscm_check_roots) {
    int i= 0;
    for (tmscm_node* n= tmscm_roots.next; n != &tmscm_roots; n= n->next, i++)
      if (!fltm_is_valid (n->v)) {
        void *from, *to; size_t size;
        fltm_heap_bounds (&from, &to, &size);
        fprintf (stderr, "femtolisp: invalid root #%d at %p: %lx "
                 "(from %p to %p size %lx)\n", i, (void*) n,
                 (unsigned long) n->v, from, to, (unsigned long) size);
        abort ();
      }
  }
  for (tmscm_node* n= tmscm_roots.next; n != &tmscm_roots; n= n->next)
    n->v= reloc (n->v);
}

/******************************************************************************
* Initialization of femtolisp
******************************************************************************/

int tm_femtolisp_argc;
char **tm_femtolisp_argv;

void
start_scheme (int argc, char** argv, void (*call_back) (int, char**)) {
  tm_femtolisp_argc= argc;
  tm_femtolisp_argv= argv;
  // a heap of 48 Mb at start (it grows anyway), each of the two halves of
  // the copying collector: about the heap of S7 in TeXmacs (1M cells of 48
  // bytes); with 8 Mb, a boot collected the garbage 23 times instead of 6,
  // and 8 exports to LaTeX take 13% less time than with 32 Mb (38 Mb more
  // at the peak); TEXMACS_FL_HEAP sets it (in Mb)
  size_t heap_mb= 48;
  if (getenv ("TEXMACS_FL_HEAP")) heap_mb= atoi (getenv ("TEXMACS_FL_HEAP"));
  if (fltm_init (heap_mb * 1024 * 1024, (const char*) fl_boot_image,
                 fl_boot_image_length)) {
    cerr << "TeXmacs] femtolisp could not be initialized\n";
    exit (1);
  }
  fltm_set_gc_roots (tmscm_relocate_roots);
  call_back (argc, argv);
}

/******************************************************************************
* Evaluation of files and strings
******************************************************************************/

tmscm
eval_scheme_file (string file) {
  if (DEBUG_STD) debug_std << "Evaluating " << file << "...\n";
  // the current load: the one of femtolisp, then the module-aware one of
  // init-femtolisp.scm
  tmscm load (fltm_lookup ("load"));
  return call_scheme (load, string_to_tmscm (file));
}

tmscm
eval_scheme (string s) {
  tmscm r;
  c_string _s (s);
  fltm_eval_string (_s, N(s), &r.v);
  return r;
}

/******************************************************************************
* Using scheme objects as functions
******************************************************************************/

static tmscm
apply_scheme (tmscm fun, tmscm args) {
  tmscm r;
  fltm_apply (fun.v, args.v, &r.v);
  return r;
}

tmscm
call_scheme (tmscm fun) {
  return apply_scheme (fun, tmscm_null ());
}

tmscm
call_scheme (tmscm fun, tmscm a1) {
  return apply_scheme (fun, tmscm_cons (a1, tmscm_null ()));
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2) {
  tmscm l= tmscm_cons (a2, tmscm_null ());
  l= tmscm_cons (a1, l);
  return apply_scheme (fun, l);
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3) {
  tmscm l= tmscm_cons (a3, tmscm_null ());
  l= tmscm_cons (a2, l);
  l= tmscm_cons (a1, l);
  return apply_scheme (fun, l);
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3, tmscm a4) {
  tmscm l= tmscm_cons (a4, tmscm_null ());
  l= tmscm_cons (a3, l);
  l= tmscm_cons (a2, l);
  l= tmscm_cons (a1, l);
  return apply_scheme (fun, l);
}

tmscm
call_scheme (tmscm fun, array<tmscm> a) {
  tmscm l= tmscm_null ();
  for (int i=N(a)-1; i>=0; i--)
    l= tmscm_cons (a[i], l);
  return apply_scheme (fun, l);
}

/******************************************************************************
* Gluing
******************************************************************************/

void
tmscm_define_builtin (const char* name, fltm_builtin_t f) {
  fltm_define (name, fltm_builtin (name, f));
}

void
tmscm_check_arity (const char* name, int expected, uint32_t nargs) {
  (void) name;
  if (nargs != (uint32_t) expected)
    throw tmscm_error ("wrong-number-of-args", "glue",
                       nargs < (uint32_t) expected?
                       "too few arguments": "too many arguments");
}

void
tmscm_raise (const tmscm_error& e) {
  // no C++ object may be alive here: fltm_raise longjmps
  fltm_value args= e.has_obj? fltm_cons (e.obj, fltm_nil): fltm_nil;
  fltm_raise (e.key, e.subr, e.message, args);
}

/******************************************************************************
* Miscellaneous routines for use by glue only
******************************************************************************/

string
scheme_dialect () {
  return "femtolisp";
}

string
scheme_init_file () {
  // sets up femtolisp, then loads the common init-kernel.scm and
  // init-texmacs.scm
  return "$TEXMACS_PATH/progs/init-femtolisp.scm";
}

/******************************************************************************
* Numbers
******************************************************************************/

int
tmscm_to_int (tmscm obj) {
  // as Guile's scm_to_int: an error rather than a truncation
  int64_t i= fltm_to_integer (obj.v);
  if (i < -2147483647LL - 1 || i > 2147483647LL)
    throw tmscm_error ("out-of-range", "tmscm_to_int",
                       "Value out of range", obj.v);
  return (int) i;
}

unsigned int
tmscm_to_uint (tmscm obj) {
  int64_t i= fltm_to_integer (obj.v);
  if (i < 0 || i > 4294967295LL)
    throw tmscm_error ("out-of-range", "tmscm_to_uint",
                       "Value out of range", obj.v);
  return (unsigned int) i;
}

/******************************************************************************
* Strings and symbols
******************************************************************************/

tmscm
string_to_tmscm (string s) {
  return tmscm (fltm_string (&s[0], N(s)));
}

string
tmscm_to_string (tmscm s) {
  return string (fltm_string_data (s.v), (int) fltm_string_length (s.v));
}

tmscm
symbol_to_tmscm (string s) {
  c_string _s (s);
  return tmscm (fltm_symbol (_s));
}

string
tmscm_to_symbol (tmscm s) {
  return string (fltm_symbol_name (s.v));
}

/******************************************************************************
* Blackbox
******************************************************************************/

static void* blackbox_type= NULL;

static char*
blackbox_to_cstring (void* p) {
  blackbox b= *((blackbox*) p);
  string s= "<blackbox>";
  int type_= type_box (b);
  if (type_ == type_helper<tree>::id) {
    tree t= open_box<tree> (b);
    s= "<tree " * tree_to_texmacs (t) * ">";
  }
  else if (type_ == type_helper<observer>::id) s= "<observer>";
  else if (type_ == type_helper<widget>::id) s= "<widget>";
  else if (type_ == type_helper<promise<widget> >::id) s= "<promise-widget>";
  else if (type_ == type_helper<command>::id) s= "<command>";
  else if (type_ == type_helper<url>::id) {
    url u= open_box<url> (b);
    s= "<url " * as_string (u) * ">";
  }
  else if (type_ == type_helper<modification>::id) s= "<modification>";
  else if (type_ == type_helper<patch>::id) s= "<patch>";
  char* r= (char*) malloc (N(s) + 1);
  memcpy (r, &s[0], N(s));
  r[N(s)]= '\0';
  return r;
}

static void
blackbox_finalize (void* p) {
  tm_delete ((blackbox*) p);
}

static int
blackbox_equal (void* p, void* q) {
  return *((blackbox*) p) == *((blackbox*) q);
}

static uintptr_t
blackbox_hash (void* p) {
  blackbox b= *((blackbox*) p);
  if (type_box (b) == type_helper<tree>::id)
    return (uintptr_t) hash (open_box<tree> (b));
  return (uintptr_t) type_box (b);
}

bool
tmscm_is_blackbox (tmscm t) {
  return fltm_is_opaque (t.v, blackbox_type);
}

tmscm
blackbox_to_tmscm (blackbox b) {
  return tmscm (fltm_make_opaque (blackbox_type, tm_new<blackbox> (b)));
}

blackbox
tmscm_to_blackbox (tmscm t) {
  return *((blackbox*) fltm_opaque_pointer (t.v));
}

/******************************************************************************
* Compatibility
******************************************************************************/

static fltm_value
g_current_time (fltm_value* args, uint32_t nargs) {
  (void) args; (void) nargs;
#ifdef HAVE_GETTIMEOFDAY
  struct timeval tp;
  gettimeofday (&tp, NULL);
  return fltm_integer (tp.tv_sec);
#else
  timeb tb;
  ftime (&tb);
  return fltm_integer (tb.time);
#endif
}

static fltm_value
g_getpid (fltm_value* args, uint32_t nargs) {
  (void) args; (void) nargs;
  return fltm_integer ((int64_t) getpid ());
}

// (%gc-count): the number of garbage collections so far
static fltm_value
g_gc_count (fltm_value* args, uint32_t nargs) {
  (void) args; (void) nargs;
  return fltm_integer (tmscm_gc_count);
}

static void
initialize_compat () {
  tmscm_define_builtin ("%gc-count", g_gc_count);
  tmscm_define_builtin ("current-time", g_current_time);
  tmscm_define_builtin ("getpid", g_getpid);
}

/******************************************************************************
* Initialization
******************************************************************************/

static void
initialize_smobs () {
  static fltm_opaque_ops ops= {
    blackbox_to_cstring, blackbox_finalize, blackbox_equal, blackbox_hash };
  blackbox_type= fltm_define_opaque_type ("blackbox", &ops);
}

void
initialize_scheme () {
  const char* init_prg =
    "(define (texmacs-version) \"" TEXMACS_VERSION "\")\n"
    "(define object-stack '(()))\n";
  initialize_compat ();
  initialize_smobs ();
  initialize_glue ();
  // the identity of the compiler (the boot image), for the cache of the
  // compiled files (boot-femtolisp.scm)
  unsigned long sum= 5381;
  for (unsigned long i=0; i<fl_boot_image_length; i++)
    sum= (sum * 33) ^ fl_boot_image[i];
  string id= "fl-" * as_string ((int) fl_boot_image_length) * "-" *
             as_hexadecimal ((int) (sum & 0x7fffffff));
  fltm_define ("*fl-boot-id*", string_to_tmscm (id).v);
  tmscm r;
  fltm_eval_string (init_prg, strlen (init_prg), &r.v);
}
