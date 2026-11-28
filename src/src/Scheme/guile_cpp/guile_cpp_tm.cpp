/******************************************************************************
* MODULE     : guile_cpp_tm.cpp
* DESCRIPTION: Interface to guile_cpp (Guile 1.8 rewritten in C++)
* COPYRIGHT  : (C) 2026  PikachuHy and Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

// the headers of guile_cpp first: they use names which TeXmacs defines
#include "guile_cpp/scm.hpp"
#include "guile_cpp/vm.hpp"

#include "guile_cpp_tm.hpp"

#include <limits.h>
#include <math.h>
#include <pthread.h>
#include <stdlib.h>

#include "blackbox.hpp"
#include "file.hpp"
#include "sys_utils.hpp"
#include "stack_guard.hpp"
#include "../Scheme/glue.hpp"
#include "convert.hpp" // tree_to_texmacs

using guile::SCM;
using guile::VM;
using guile::PortObj;
using guile::SmobObj;

/******************************************************************************
* Strings between TeXmacs and guile_cpp
******************************************************************************/

static void
port_write (SCM port, string s) {
  guile::port_puts (guile::as_obj<PortObj> (port), s);
}

/******************************************************************************
* State and errors
******************************************************************************/

int tmscm_builders= 0;
tmscm object_stack= guile::SCM_EOL;

static int scheme_errors= 0;

// a root of the collector for two values, popped however the scope is left
// (an exception left a span pushed, which the collector then read)
struct root_pair {
  SCM roots[2];
  guile::GcRootSpan span;
  root_pair (SCM a, SCM b) {
    roots[0]= a; roots[1]= b;
    span.data= roots; span.count= 2; span.prev= NULL;
    guile::vm ().heap ().push_root_span (&span);
  }
  ~root_pair () { guile::vm ().heap ().pop_root_span (); }
};
static bool scheme_started= false;
static size_t blackbox_smob_type= (size_t) -1;

int
scheme_caught_errors () {
  return scheme_errors;
}

static tmscm
handle_scheme_throw (SCM key, SCM args) {
  VM& v= guile::vm ();
  if (key == v.sym_quit) {
    int code= 0;
    if (guile::is_pair (args)) {
      SCM arg= guile::car (args);
      if (arg.is_false ()) code= 1;
      else if (arg == guile::SCM_BOOL_T) code= 0;
      else if (guile::is_exact_integer (arg))
        code= (int) guile::scm_to_int64 (arg, "quit", 1);
    }
    guile::flush_all_ports ();
    exit (code);
  }
  scheme_errors++;
  // the key and the arguments were only referenced by the exception object,
  // which the collector does not see, and printing them allocates
  root_pair guard (key, args);
  try {
  SCM errp= v.current_error_port ();
  port_write (errp, "ERROR: ");
  if (guile::is_pair (args) &&
      guile::is_pair (guile::cdr (args)) &&
      guile::is_pair (guile::cdr (guile::cdr (args)))) {
    SCM subr = guile::car (args);
    SCM msg  = guile::car (guile::cdr (args));
    SCM margs= guile::car (guile::cdr (guile::cdr (args)));
    SCM tail = guile::cdr (guile::cdr (guile::cdr (args)));
    SCM rest = guile::is_pair (tail)? guile::car (tail): guile::SCM_BOOL_F;
    guile::display_error_impl (guile::SCM_BOOL_F, errp, subr, msg, margs, rest);
  }
  else {
    port_write (errp, "Throw to key `");
    v.write_to_port (key, errp, true);
    port_write (errp, "' with args `");
    v.write_to_port (args, errp, false);
    port_write (errp, "'.\n");
  }
  guile::port_flush (guile::as_obj<PortObj> (errp));
  }
  catch (...) {
    // (an error while printing the error: it is not printed)
  }
  return v.cons (key, args);
}

// An exception of C++ which crossed Scheme (a continuation invoked out of
// its extent, an exception of the C++ library): the frames of dynamic-wind,
// with-fluids and catch which it left are unwound, and it is reported as
// an error of Scheme, (misc-error #f message () #f)
// Unwind to the frame dynwind; an error in an after-thunk is reported and
// the unwinding goes on from the frame where it happened (switch_dynwind
// leaves a frame before running its after-thunk), so that the outer frames
// are unwound too
static tmscm handle_scheme_throw (SCM key, SCM args);

static void
unwind_to (SCM dynwind) {
  VM& v= guile::vm ();
  for (int tries= 0; tries < 10000; tries++) {
    try {
      if (v.current_dynwind != dynwind) guile::switch_dynwind_pub (dynwind);
      return;
    }
    catch (const guile::SchemeThrow& ex) {
      SCM key= ex.key, args= ex.args;
      root_pair guard (key, args);
      try { handle_scheme_throw (key, args); }
      catch (...) {}
    }
    catch (...) {
    }
  }
  v.current_dynwind= dynwind;
}

static tmscm
handle_cpp_exception (SCM dynwind, const char* what) {
  VM& v= guile::vm ();
  unwind_to (dynwind);
  try {
    SCM msg = v.make_string (what);
    SCM args= v.list4 (guile::SCM_BOOL_F, msg, guile::SCM_EOL,
                       guile::SCM_BOOL_F);
    return handle_scheme_throw (v.make_symbol ("misc-error"), args);
  }
  catch (...) {
    scheme_errors++;
    return guile::SCM_BOOL_F;
  }
}

// Every call from C++ into Scheme catches every exception (guile_cpp's
// errors and escapes are exceptions of C++), so that none of them reaches
// the event loop of the interface; called in a handler of catch (...)
static tmscm
handle_exception (SCM dynwind) {
  try {
    throw;
  }
  catch (const guile::SchemeThrow& ex) {
    // the frames of dynamic-wind, with-fluids... which the error left are
    // unwound (their after-thunks run), as for the other exceptions
    SCM key= ex.key, args= ex.args;
    root_pair guard (key, args);
    unwind_to (dynwind);
    return handle_scheme_throw (key, args);
  }
  catch (const guile::ContinuationEscape&) {
    return handle_cpp_exception (dynwind,
                                 "continuation invoked out of its extent");
  }
  catch (const string&) {
    return handle_cpp_exception (dynwind, "exception of TeXmacs");
  }
  catch (const std::exception& e) {
    return handle_cpp_exception (dynwind, e.what ());
  }
  catch (...) {
    return handle_cpp_exception (dynwind, "exception of C++");
  }
}

static inline SCM
current_dynwind () {
  return guile::vm ().current_dynwind;
}

// An exception of TeXmacs (tm_throw) in a routine of the glue becomes an
// error of Scheme, as with the other interpreters: guile_cpp unwinds its
// own frames for it
void
tmscm_raise_error (string msg) {
  // (the message is an argument: a ~ in it is not a directive)
  guile::vm ().misc_error ("glue", "~A",
                           guile::vm ().list1 (guile::vm ().make_string (msg)));
}

void
tmscm_wrong_type_arg (const char* subr, int pos, tmscm arg) {
  guile::vm ().wrong_type_arg (subr, pos, arg, "valid argument");
}

/******************************************************************************
* The blackboxes of TeXmacs as smobs
******************************************************************************/

static void
free_blackbox_smob (SCM obj) {
  SmobObj* sm= guile::as_obj<SmobObj> (obj);
  blackbox* ptr= (blackbox*) sm->user_data;
  if (ptr != NULL) {
    sm->user_data= NULL;
    tm_delete (ptr);
  }
}

static void
print_blackbox_smob (SCM obj, SCM port) {
  string s= "<blackbox>";
  int type_= type_box (tmscm_to_blackbox (obj));
  if (type_ == type_helper<tree>::id) {
    tree t= tmscm_to_tree (obj);
    s= "<tree " * tree_to_texmacs (t) * ">";
  }
  else if (type_ == type_helper<observer>::id) s= "<observer>";
  else if (type_ == type_helper<widget>::id) s= "<widget>";
  else if (type_ == type_helper<promise<widget> >::id) s= "<promise-widget>";
  else if (type_ == type_helper<command>::id) {
    command cmd= tmscm_to_command (obj);
    s= print_to_string<command> (cmd);
  }
  else if (type_ == type_helper<url>::id) {
    url u= tmscm_to_url (obj);
    s= "<url " * as_string (u) * ">";
  }
  else if (type_ == type_helper<modification>::id) s= "<modification>";
  else if (type_ == type_helper<patch>::id) s= "<patch>";
  port_write (port, s);
}

static SCM
equalp_blackbox_smob (SCM a, SCM b) {
  if (a == b) return guile::SCM_BOOL_T;
  if (!tmscm_is_blackbox (b)) return guile::SCM_BOOL_F;
  return SCM::from_bool (tmscm_to_blackbox (a) == tmscm_to_blackbox (b));
}

static uint64_t
hash_tree_bounded (tree t, int& budget) {
  if (budget <= 0) return 0;
  budget--;
  if (is_atomic (t))
    return ((uint64_t) (uint32_t) hash (t->label)) ^ 0x9e3779b97f4a7c15ULL;
  uint64_t h= ((uint64_t) (uint32_t) L (t)) * 2654435761ULL;
  int i, n= N (t);
  h ^= ((uint64_t) n) * 2246822519ULL;
  for (i= 0; i < min (n, 4) && budget > 0; i++)
    h= (h * 1315423911ULL) ^ hash_tree_bounded (t[i], budget);
  return h;
}

static uint64_t
hash_blackbox_smob (SCM obj) {
  blackbox bb= tmscm_to_blackbox (obj);
  int type_= type_box (bb);
  if (type_ == type_helper<tree>::id) {
    int budget= 12;
    return hash_tree_bounded (tmscm_to_tree (obj), budget);
  }
  else if (type_ == type_helper<url>::id)
    return (uint64_t) (uint32_t) hash (as_string (tmscm_to_url (obj)));
  else if (type_ == type_helper<path>::id) {
    path p= open_box<path> (bb);
    uint64_t h= 0x517cc1b727220a95ULL;
    for (int k= 0; !is_nil (p) && k < 16; p= p->next, k++)
      h= (h * 1315423911ULL) ^ ((uint64_t) (uint32_t) p->item);
    return h;
  }
  return ((uint64_t) (uint32_t) type_) * 2654435761ULL;
}

static void
initialize_smobs () {
  guile::SmobTypeInfo info;
  info.name   = "blackbox";
  info.free_fn= free_blackbox_smob;
  info.print  = print_blackbox_smob;
  info.equalp = equalp_blackbox_smob;
  info.hash_fn= hash_blackbox_smob;
  blackbox_smob_type= guile::vm ().make_smob_type (info);
}

bool
tmscm_is_blackbox (tmscm obj) {
  return guile::is_smob (obj) &&
         guile::as_obj<SmobObj> (obj)->smob_type == blackbox_smob_type;
}

tmscm
blackbox_to_tmscm (blackbox b) {
  SmobObj* sm= guile::vm ().heap ().alloc<SmobObj> (blackbox_smob_type);
  sm->user_data= (void*) tm_new<blackbox> (b);
  return SCM::from_obj (sm);
}

blackbox
tmscm_to_blackbox (tmscm obj) {
  SmobObj* sm= guile::as_obj<SmobObj> (obj);
  return *((blackbox*) sm->user_data);
}

/******************************************************************************
* Startup and initialization
******************************************************************************/

// guile_cpp stops with "Stack overflow" when the C stack is deeper than
// c_stack_limit_bytes from where it started (start_scheme): the guards of
// TeXmacs (stack_guard.hpp) stop the recursions of C++ before that point,
// so that a deep document gives the error "nesting too deep" of TeXmacs
static pthread_t main_thread;

static char*
guile_cpp_stack_low () {
  if (!pthread_equal (pthread_self (), main_thread)) return NULL;
  VM& v= guile::vm ();
  char* base= (char*) v.heap ().stack_base ();
  size_t limit= v.c_stack_limit_bytes ();
  if (base == NULL || limit >= (size_t) (uintptr_t) base) return NULL;
  return base - limit;
}

static bool
env_is (const char* val, const char* a, const char* b,
        const char* c, const char* d) {
  return strcmp (val, a) == 0 || strcmp (val, b) == 0 ||
         strcmp (val, c) == 0 || strcmp (val, d) == 0;
}

void
start_scheme (int argc, char** argv, void (*call_back) (int, char**)) {
  if (scheme_started) return;
  scheme_started= true;

  volatile int stack_anchor= 0;
  VM& v= guile::vm ();

  // the Scheme library of Guile 1.8 (ice-9, oop, srfi), which the build
  // copies into TeXmacs/boot/guile_cpp (see src/Makefile.in)
  c_string dir (get_env ("TEXMACS_PATH") * "/boot/guile_cpp");
  guile::set_data_dirs (dir, dir, NULL);

  string jit= get_env ("GUILE_JIT");
  if (jit != "") {
    c_string val (jit);
    if (env_is (val, "0", "off", "false", "no")) v.jit_enabled= false;
    else if (env_is (val, "1", "on", "true", "yes")) v.jit_enabled= true;
  }

  // a large first threshold, so that the boot is done without collections,
  // then collections from 1.5 million cells on
  v.heap ().set_min_gc_threshold (1500000);
  v.heap ().set_initial_gc_threshold (4500000);

  try {
    v.init ((int*) &stack_anchor, true);
  }
  catch (...) {
    handle_exception (guile::SCM_EOL);
  }
  main_thread= pthread_self ();
  scheme_stack_low= guile_cpp_stack_low;
  v.set_program_arguments (argc, argv);

  call_back (argc, argv);
}

void
initialize_scheme () {
  const char* init_prg=
    "(read-set! keywords 'prefix)\n"
#ifdef DEBUG_ON
    "(read-enable 'positions)\n"
    "(debug-enable 'debug)\n"
    "(debug-enable 'backtrace)\n"
#endif
    "\n"
    "(define (display-to-string obj)\n"
    "  (call-with-output-string\n"
    "    (lambda (port) (display obj port))))\n"
    "(define (object->string obj)\n"
    "  (call-with-output-string\n"
    "    (lambda (port) (write obj port))))\n"
    "\n"
    "(define (texmacs-version) \"" TEXMACS_VERSION "\")\n"
    "(define object-stack '(()))\n";

  VM& v= guile::vm ();
  SCM dw= current_dynwind ();
  try {
    v.eval_string (init_prg);
  }
  catch (...) {
    handle_exception (dw);
  }

  initialize_smobs ();
  initialize_glue ();

  SCM ovar= v.lookup ("object-stack");
  if (guile::is_variable (ovar))
    object_stack= guile::as_obj<guile::VariableObj> (ovar)->value;
  else object_stack= v.list1 (guile::SCM_EOL);
}

string
scheme_dialect () {
  return "guile-c";
}

string
scheme_init_file () {
  return "$TEXMACS_PATH/progs/init-guile.scm";
}

/******************************************************************************
* Evaluation and calls
******************************************************************************/

tmscm
eval_scheme_file (string file) {
  if (DEBUG_STD) debug_std << "Evaluating " << file << "...\n";
  SCM dw= current_dynwind ();
  try {
    return guile::vm ().primitive_load (file);
  }
  catch (...) {
    return handle_exception (dw);
  }
}

tmscm
eval_scheme (string s) {
  SCM dw= current_dynwind ();
  try {
    return guile::vm ().eval_string (s);
  }
  catch (...) {
    return handle_exception (dw);
  }
}

tmscm
call_scheme (tmscm fun) {
  SCM dw= current_dynwind ();
  try {
    return guile::vm ().call0 (fun);
  }
  catch (...) {
    return handle_exception (dw);
  }
}

tmscm
call_scheme (tmscm fun, tmscm a1) {
  SCM dw= current_dynwind ();
  try {
    return guile::vm ().call1 (fun, a1);
  }
  catch (...) {
    return handle_exception (dw);
  }
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2) {
  SCM dw= current_dynwind ();
  try {
    return guile::vm ().call2 (fun, a1, a2);
  }
  catch (...) {
    return handle_exception (dw);
  }
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3) {
  SCM dw= current_dynwind ();
  try {
    return guile::vm ().call3 (fun, a1, a2, a3);
  }
  catch (...) {
    return handle_exception (dw);
  }
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3, tmscm a4) {
  SCM dw= current_dynwind ();
  try {
    return guile::vm ().call4 (fun, a1, a2, a3, a4);
  }
  catch (...) {
    return handle_exception (dw);
  }
}

tmscm
call_scheme (tmscm fun, array<tmscm> a) {
  int i, n= N (a);
  STACK_NEW_ARRAY (args, tmscm, n);
  for (i= 0; i < n; i++) args[i]= a[i];
  tmscm r;
  SCM dw= current_dynwind ();
  try {
    r= guile::vm ().apply (fun, args, (size_t) n);
  }
  catch (...) {
    r= handle_exception (dw);
  }
  STACK_DELETE_ARRAY (args);
  return r;
}

/******************************************************************************
* Gluing
******************************************************************************/

void
tmscm_install_raw_procedure (const char* name, int req, int opt, int rst,
                             tmscm_raw_proc fn) {
  SCM p= guile::vm ().define_proc (name, req, opt, rst != 0, fn);
  guile::as_obj<guile::SubrObj> (p)->raw_fn= fn;
}

/******************************************************************************
* Predicates and constructors
******************************************************************************/

bool
tmscm_is_equal (tmscm o1, tmscm o2) {
  return guile::scm_equal (o1, o2);
}

bool
tmscm_is_list (tmscm obj) {
  return guile::is_proper_list (obj);
}

bool
tmscm_is_double (tmscm obj) {
  return guile::is_real_number (obj);
}

tmscm
tmscm_cons (tmscm o1, tmscm o2) {
  return guile::vm ().cons (o1, o2);
}

tmscm
long_to_tmscm (long l) {
  return guile::vm ().make_integer ((int64_t) l);
}

tmscm
double_to_tmscm (double r) {
  return guile::vm ().make_real (r);
}

/******************************************************************************
* Numbers, strings and symbols
******************************************************************************/

bool
tmscm_is_int (tmscm obj) {
  if (guile::is_exact_integer (obj)) return true;
  if (guile::is_real_obj (obj)) {
    double d= guile::as_obj<guile::RealObj> (obj)->val;
    return isfinite (d) && trunc (d) == d;
  }
  return false;
}

int
tmscm_to_int (tmscm obj) {
  int64_t v= 0;
  if (obj.is_fixnum ()) v= obj.to_fixnum ();
  else if (guile::is_real_obj (obj)) {
    double d= guile::as_obj<guile::RealObj> (obj)->val;
    if (!isfinite (d) || d < -2147483648.0 || d > 2147483647.0)
      guile::vm ().out_of_range ("tmscm_to_int", obj);
    return (int) llround (d);
  }
  else v= guile::scm_to_int64 (obj, "tmscm_to_int", 1);
  if (v < (int64_t) INT_MIN || v > (int64_t) INT_MAX)
    guile::vm ().out_of_range ("tmscm_to_int", obj);
  return (int) v;
}

unsigned int
tmscm_to_uint (tmscm obj) {
  int64_t v= 0;
  if (obj.is_fixnum ()) v= obj.to_fixnum ();
  else if (guile::is_real_obj (obj)) {
    double d= guile::as_obj<guile::RealObj> (obj)->val;
    if (!isfinite (d) || d < 0.0 || d > 4294967295.0)
      guile::vm ().out_of_range ("tmscm_to_uint", obj);
    return (unsigned int) llround (d);
  }
  else v= guile::scm_to_int64 (obj, "tmscm_to_uint", 1);
  if (v < 0 || v > (int64_t) UINT_MAX)
    guile::vm ().out_of_range ("tmscm_to_uint", obj);
  return (unsigned int) v;
}

double
tmscm_to_double (tmscm obj) {
  return guile::scm_to_double (obj, "tmscm_to_double");
}

tmscm
string_to_tmscm (string s) {
  return guile::vm ().make_string (s);  // (a copy: Scheme may change it)
}

string
tmscm_to_string (tmscm obj) {
  if (!guile::is_string (obj))
    guile::vm ().wrong_type_arg ("tmscm_to_string", 1, obj, "string");
  return guile::as_obj<guile::StringObj> (obj)->str ();  // (a copy)
}

tmscm
symbol_to_tmscm (string s) {
  return guile::vm ().make_symbol (s);
}

string
tmscm_to_symbol (tmscm obj) {
  if (!guile::is_symbol (obj))
    guile::vm ().wrong_type_arg ("tmscm_to_symbol", 1, obj, "symbol");
  // (a copy: the name of a symbol must never change, the caller may)
  return copy (guile::as_obj<guile::SymbolObj> (obj)->name);
}
