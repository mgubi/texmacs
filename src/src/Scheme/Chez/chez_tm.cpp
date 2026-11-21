
/******************************************************************************
 * MODULE     : chez_tm.cpp
 * DESCRIPTION: Interface to the Chez Scheme kernel (C API)
 * COPYRIGHT  : (C) 2026
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "chez_tm.hpp"
#include "blackbox.hpp"
#include "file.hpp"
#include "converter.hpp"
#include "basic.hpp"
#include "../Scheme/glue.hpp"

#include <map>
#include <mutex>
#include <string.h>
#include <cstdio>
#include <cstdlib>  /* getenv, _fullpath/_MAX_PATH on Windows */

/* ------------------------------------------------------------------------
 * Global engine state
 * ---------------------------------------------------------------------- */

tmscm tm_null= 0;
tmscm tm_blackbox_guardian= 0;
tmscm tm_blackbox_rtd= 0;
tmscm tm_make_blackbox= 0;
tmscm tm_eval_string_proc= 0;
tmscm tm_load_file_proc= 0;
tmscm tm_apply_proc= 0;
tmscm tm_call_0_proc= 0;
tmscm tm_call_1_proc= 0;
tmscm tm_call_2_proc= 0;
tmscm tm_call_3_proc= 0;
tmscm tm_call_4_proc= 0;
tmscm tm_call_n_proc= 0;
tmscm tm_list_pred= 0;
tmscm tm_equal_pred= 0;
tmscm object_stack= 0;

/* ------------------------------------------------------------------------
 * Shadow root stack for GC-safe C++ -> Scheme conversions
 * ---------------------------------------------------------------------- */

tmscm chez_root_stack_vec= 0;
iptr  chez_root_stack_top= 0;
iptr  chez_root_stack_cap= 0;

static const iptr CHEZ_ROOT_STACK_INIT_CAP= 512;

void
chez_root_stack_grow () {
  iptr new_cap= (chez_root_stack_cap == 0)
                ? CHEZ_ROOT_STACK_INIT_CAP
                : (chez_root_stack_cap * 2);
  tmscm old_vec= chez_root_stack_vec;
  tmscm new_vec= Smake_vector (new_cap, Sfixnum (0));
  /* Smake_vector is a pure C allocation and never synchronously runs GC.
     Lock new_vec before swapping so every live root stays pinned. */
  Slock_object (new_vec);
  for (iptr i= 0; i < chez_root_stack_top; i++)
    Svector_set (new_vec, i, Svector_ref (old_vec, i));
  chez_root_stack_vec= new_vec;
  chez_root_stack_cap= new_cap;
  if (old_vec != 0)
    Sunlock_object (old_vec);
}

extern "C" iptr
chez_root_stack_depth () {
  return chez_root_stack_top;
}

extern "C" iptr
chez_root_stack_capacity () {
  return chez_root_stack_cap;
}

/* ------------------------------------------------------------------------
 * Chez Scheme Threading & Concurrency Model
 *
 * TeXmacs runs single-threaded on the main GUI thread. The Scheme engine
 * is initialized and activated once at boot time by Sscheme_init() in
 * start_scheme(). It remains permanently active for the lifetime of the
 * process, avoiding per-call TLS activation/deactivation churn.
 *
 * TeXmacs C++ document trees (tree_rep, buffer) are strictly single-threaded
 * and non-reentrant; synchronization is handled at the Qt event loop level.
 * Therefore, leaf FFI dispatchers (tm_fast_call_N) and call_scheme do not
 * pay the overhead of an atomic recursive mutex.
 * ---------------------------------------------------------------------- */

/* ------------------------------------------------------------------------
 * Basic list/equality predicates (need cached Scheme procedures)
 * ---------------------------------------------------------------------- */

bool
tmscm_is_list (tmscm obj) {
  /* Fast C-level cycle-detecting walk using Spairp/Scdr.
     Do NOT call Scheme (Scall1 tm_list_pred) here: C++ traversal functions
     like tmscm_to_scheme_tree call tmscm_is_list while holding unpinned tmscm
     pointers on the C++ stack; entering Scheme can trigger a copying GC which
     relocates the tree and causes a crash. */
  tmscm slow= obj, fast= obj;
  for (int i= 0; i < 1000000; i++) {
    if (Snullp (fast)) return true;
    if (!Spairp (fast)) return false;
    fast= Scdr (fast);
    if (Snullp (fast)) return true;
    if (!Spairp (fast)) return false;
    fast= Scdr (fast);
    slow= Scdr (slow);
    if (fast == slow) return false; /* cycle */
  }
  return false;
}

void
tmscm_wrong_type_arg (tmscm arg, int pos, const char* subr) {
  (void) arg;
  string msg= "Wrong type argument in " * string (subr ? subr : "unknown")
            * " at position " * as_string (pos);
  tm_throw ((char*) c_string (msg));
}

bool
tmscm_is_equal (tmscm o1, tmscm o2) {
  /* Fast path: when comparing two top-level blackboxes from C++, unwrap and
     compare directly without crossing into Scheme.  Nested blackboxes inside
     pairs, vectors, or records are handled by Chez's native equal? via
     record-type-equal-procedure on tm-blackbox-rtd. */
  if (tmscm_is_blackbox (o1) && tmscm_is_blackbox (o2))
    return tmscm_to_blackbox (o1) == tmscm_to_blackbox (o2);
  return Sboolean_value (Scall2 (tm_equal_pred, o1, o2));
}

/* ------------------------------------------------------------------------
 * String and symbol conversions
 *
 * NOTE [String Encoding Duality & Raw Byte Invariant]:
 * TeXmacs C++ assumes the Scheme <-> C++ string bridge operates on raw 8-bit
 * bytes (1 Scheme character = 1 C++ byte), preserving exact byte fidelity for
 * binary assets (e.g. PNG images loaded via string-load / tmfs-load) as well
 * as Cork and raw UTF-8 byte streams.
 *
 * Do NOT use decode_from_utf8 / encode_as_utf8 here: treating raw binary bytes
 * >= 128 as UTF-8 expands them into multi-byte sequences (e.g. PNG magic 0x89
 * -> 0xC2 0x89), inflating and corrupting image data (see TODO_CHEZ.md).
 *
 * KNOWN CONSEQUENCE, deliberately kept: a Scheme character above 0xFF cannot
 * cross this bridge.  tmscm_to_string truncates it to its low byte, so
 * U+03BC mu reaches C++ as 0xBC and regtest-xmltm reports one mismatch in the
 * Bx row of latin1-chart.  Fixing that by encoding UTF-8 here breaks binary
 * round-trips through string-load / tmfs-load, so the fix has to distinguish
 * text from byte buffers at the boundary that knows which one it is holding --
 * not inside these two functions.
 * ---------------------------------------------------------------------- */

tmscm
string_to_tmscm (string s) {
  iptr n= (iptr) N (s);
  tmscm res= Smake_bytevector (n, 0);
  memcpy (Sbytevector_data (res), &s[0], n);
  return res;
}

string
tmscm_to_string (tmscm s) {
  if (Sbytevectorp (s)) {
    iptr n= Sbytevector_length (s);
    string r ((int) n);
    memcpy (&r[0], Sbytevector_data (s), n);
    return r;
  }
  FAILED ("bytevector expected in tmscm_to_string");
  return "";
}

tmscm
symbol_to_tmscm (string s) {
  c_string _s (s);
  return Sstring_to_symbol (_s);
}

string
tmscm_to_symbol (tmscm s) {
  tmscm str= Ssymbol_to_string (s);
  iptr n= Sstring_length (str);
  string r;
  for (iptr i= 0; i < n; i++)
    r << encode_as_utf8 (Sstring_ref (str, i));
  return r;
}

/* ------------------------------------------------------------------------
 * Blackboxes
 * ---------------------------------------------------------------------- */

bool
tmscm_is_blackbox (tmscm t) {
  return Srecordp (t)
      && (Srecord_type (t) == tm_blackbox_rtd)
      && tmscm_is_int (Srecord_uniform_ref (t, 0))
      /* ptr is zeroed when reaped by the guardian; reject dead wrappers. */
      && (Sinteger_value (Srecord_uniform_ref (t, 0)) != 0);
}

tmscm
blackbox_to_tmscm (blackbox b) {
  blackbox* p= tm_new<blackbox> (b);
  return Scall1 (tm_make_blackbox, Sinteger ((iptr) p));
}

blackbox
tmscm_to_blackbox (tmscm t) {
  void* p= (void*) Sinteger_value (Srecord_uniform_ref (t, 0));
  if (!p) return blackbox ();
  return *((blackbox*) p);
}

extern "C" void
tmscm_delete_blackbox (void* p) {
  if (p) tm_delete ((blackbox*) p);
}

extern "C" bool
tmscm_blackbox_equal (uptr p1, uptr p2) {
  if (p1 == p2) return true;
  if (!p1 || !p2) return false;
  return *((blackbox*) p1) == *((blackbox*) p2);
}

extern "C" iptr
tmscm_blackbox_hash (uptr p) {
  if (!p) return 0;
  blackbox* bb= (blackbox*) p;
  if (type_box (*bb) == type_helper<tree>::id) {
    return (iptr) hash (open_box<tree> (*bb));
  }
  return (iptr) type_box (*bb);
}

void
reap_blackboxes () {
  ASSERT (tm_blackbox_guardian != 0,
          "reap_blackboxes called before tm_blackbox_guardian is initialized");
  while (true) {
    tmscm b= Scall0 (tm_blackbox_guardian);
    if (b == Sboolean (0)) break;
    if (Srecordp (b) && Srecord_type (b) == tm_blackbox_rtd) {
      void* p= (void*) Sinteger_value (Srecord_uniform_ref (b, 0));
      if (p) {
        tm_delete ((blackbox*) p);
        Srecord_uniform_ref (b, 0)= Sfixnum (0);
      }
    }
  }
}

const char* current_glue_proc = NULL;

/* ------------------------------------------------------------------------
 * Scheme bootstrap
 *
 * No Scheme source lives in this file.  Everything the kernel needs on the
 * Scheme side -- tm-apply, tm-load-file, tm-eval-string, the blackbox
 * guardian, the glue installer, texmacs-version -- is defined in
 * TeXmacs/progs/kernel/boot/boot-chez.scm, which initialize_scheme() loads
 * with Chez's primitive `load' before initialize_glue() runs.
 *
 * The only Scheme knowledge retained here is the *names* of the bindings the
 * kernel caches, because Stop_level_value() has to look them up by symbol.
 * ---------------------------------------------------------------------- */

/* FAILED() takes a C string; the messages here are assembled at run time. */
static void
boot_failed (string msg) {
  c_string _msg (msg);
  fprintf (stderr, "ERROR: %s\n", (const char*) _msg);
  fflush (stderr);
  tm_throw ((const char*) _msg);
}

/* Path of the Scheme bootstrap.  init_texmacs() has already exported
   TEXMACS_PATH by the time the kernel starts, and a packaged .app sets it
   relative to the executable, so getenv is authoritative; the url() fallback
   covers a tree booted without the variable, where the path system still
   knows where the data directory is. */
static string
boot_scheme_path () {
  const char* env= getenv ("TEXMACS_PATH");
  if (env != NULL && env[0] != '\0')
    return string (env) * "/progs/kernel/boot/boot-chez.scm";
  return as_string (url ("$TEXMACS_PATH/progs/kernel/boot/boot-chez.scm"));
}

/* Evaluate a file in the interaction environment.  Chez's `load' is what we
   need here: the engine's guarded per-form loader (tm-load-file) is itself
   defined by the file being loaded. */
static void
load_scheme_file (string file) {
  ptr load_proc= Stop_level_value (Sstring_to_symbol ("load"));
  if (!Sprocedurep (load_proc))
    boot_failed ("Chez Scheme: 'load' is not bound, cannot load " * file);
  c_string _file (file);
  ptr str= Sstring_of_length (_file, (iptr) N (file));
  Slock_object (str);
  Scall1 (load_proc, str);
  Sunlock_object (str);
}

/* ------------------------------------------------------------------------
 * Evaluation and calls
 * ---------------------------------------------------------------------- */

tmscm
eval_scheme_file (string file) {
  c_string _file (file);
  return Scall1 (tm_load_file_proc, Sstring_utf8 (_file, (iptr) N (file)));
}

tmscm
eval_scheme (string s) {
  c_string _s (s);
  return Scall1 (tm_eval_string_proc, Sstring_utf8 (_s, (iptr) N (s)));
}

/* ------------------------------------------------------------------------
 * Procedure calls
 *
 * UTF-8 transcoding between C++ bytevectors and Scheme UCS-4 strings is
 * handled on the Scheme side by tm-call-0..4 / tm-call-n (boot-chez.scm)
 * via recur-utf8->string and recur-string->utf8.
 * ---------------------------------------------------------------------- */

static inline ptr
scall4 (ptr proc, ptr a1, ptr a2, ptr a3, ptr a4) {
  Sinitframe (4);
  Sput_arg (1, a1);
  Sput_arg (2, a2);
  Sput_arg (3, a3);
  Sput_arg (4, a4);
  return Scall (proc, 4);
}

static inline ptr
scall5 (ptr proc, ptr a1, ptr a2, ptr a3, ptr a4, ptr a5) {
  Sinitframe (5);
  Sput_arg (1, a1);
  Sput_arg (2, a2);
  Sput_arg (3, a3);
  Sput_arg (4, a4);
  Sput_arg (5, a5);
  return Scall (proc, 5);
}

tmscm
call_scheme (tmscm fun) {
  return Scall1 (tm_call_0_proc, fun);
}

tmscm
call_scheme (tmscm fun, tmscm a1) {
  return Scall2 (tm_call_1_proc, fun, a1);
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2) {
  return Scall3 (tm_call_2_proc, fun, a1, a2);
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3) {
  return scall4 (tm_call_3_proc, fun, a1, a2, a3);
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3, tmscm a4) {
  return scall5 (tm_call_4_proc, fun, a1, a2, a3, a4);
}

tmscm
call_scheme (tmscm fun, array<tmscm> a) {
  int n= N (a);
  if (n == 0)
    return Scall2 (tm_call_n_proc, fun, tmscm_null ());
  ChezRoot r_fun (fun);
  ChezRoot r_list (tmscm_null ());
  for (int i= n - 1; i >= 0; i--)
    r_list.set (Scons (a[i], r_list.get ()));
  return Scall2 (tm_call_n_proc, r_fun.get (), r_list.get ());
}

extern "C" tmscm
chez_call_scheme_1 (tmscm fun, tmscm a1) {
  return call_scheme (fun, a1);
}

/* ------------------------------------------------------------------------
 * Dialect and initialization
 * ---------------------------------------------------------------------- */

string
scheme_dialect () {
  return "chez";
}

static bool s_chez_heap_built = false;

static bool
chez_heap_built () {
  return s_chez_heap_built;
}

static void
register_foreign_symbols () {
  Sforeign_symbol ("chez_heap_built", (void*) chez_heap_built);
  Sforeign_symbol ("chez_cache_runtime_procedures", (void*) chez_cache_runtime_procedures);
  Sforeign_symbol ("tmscm_delete_blackbox", (void*) tmscm_delete_blackbox);
  Sforeign_symbol ("tmscm_blackbox_equal", (void*) tmscm_blackbox_equal);
  Sforeign_symbol ("tmscm_blackbox_hash", (void*) tmscm_blackbox_hash);
  Sforeign_symbol ("chez_root_stack_depth", (void*) chez_root_stack_depth);
  Sforeign_symbol ("chez_root_stack_capacity", (void*) chez_root_stack_capacity);
  Sforeign_symbol ("chez_call_scheme_1", (void*) chez_call_scheme_1);
}

static void
custom_init (void) {
  /* called by Sbuild_heap before loading the boot files */
  s_chez_heap_built = false;
  register_foreign_symbols ();
  /* The glue entry points are registered here, not in initialize_scheme(),
     because Sforeign_symbol() only writes to a C table: it needs neither the
     heap nor a Scheme call, so it is legal this early.  Doing it here is what
     lets boot-chez.scm -- and therefore any boot file evaluated during heap
     construction -- bind the glue with foreign-procedure.  See
     docs/chez/GLUE_CHEZ_PLAN.md. */
  initialize_glue_chez ();
}

#ifndef CHEZ_BOOT_DIR
#define CHEZ_BOOT_DIR ""
#endif

static string
boot_dir () {
  const char* env= getenv ("SCHEMEHEAPDIRS");
  if (env != NULL && env[0] != '\0') return env;
  return CHEZ_BOOT_DIR;
}

extern bool is_server_started ();

static bool s_has_texmacs_boot = false;
static string s_texmacs_boot_file = "";
static int s_argc = 0;
static char** s_argv = NULL;
static bool s_scheme_initialized = false;

bool
chez_has_boot () {
  return s_has_texmacs_boot;
}

string
chez_boot_file () {
  return s_texmacs_boot_file;
}

static void
build_chez_heap () {
  if (s_scheme_initialized) return;
  s_scheme_initialized = true;

  if (is_server_started ()) {
    fprintf (stderr, "Chez Scheme: initializing heap with active TeXmacs server\n");
  } else {
    fprintf (stderr, "Chez Scheme: WARNING - initializing heap before TeXmacs server started\n");
  }
  fflush (stderr);

  Sscheme_init (NULL);
  string dir= boot_dir ();
  if (N (dir) > 0 && dir[N (dir) - 1] != '/') dir= dir * "/";
  Sregister_boot_file (c_string (dir * "petite.boot"));
  Sregister_boot_file (c_string (dir * "scheme.boot"));

  bool is_compile_modules = false;
  for (int i = 1; i < s_argc; i++) {
    if (strcmp (s_argv[i], "-compile-modules") == 0 ||
        strcmp (s_argv[i], "--compile-modules") == 0 ||
        strcmp (s_argv[i], "-recompile-modules") == 0 ||
        strcmp (s_argv[i], "--recompile-modules") == 0) {
      is_compile_modules = true;
      break;
    }
  }

  if (!is_compile_modules) {
    string texmacs_boot = "";
    const char* no_boot = getenv ("TEXMACS_NO_BOOT");
    bool skip_boot = (no_boot != NULL && (strcmp (no_boot, "1") == 0 || strcmp (no_boot, "true") == 0));
    if (!skip_boot) {
      const char* env_boot = getenv ("TEXMACS_BOOT_FILE");
      if (env_boot != NULL && env_boot[0] != '\0' && exists (url_system (env_boot))) {
        texmacs_boot = env_boot;
      } else {
        const char* boot_env = getenv ("TEXMACS_BOOT_PATH");
        string cand_dir = (boot_env != NULL && boot_env[0] != '\0') ? string (boot_env) : "";
        if (N (cand_dir) == 0) {
          string tm_p = get_env ("TEXMACS_PATH");
          if (N (tm_p) > 0 && exists (url_system (tm_p * "/boot"))) cand_dir = tm_p * "/boot";
        }
        if (N (cand_dir) > 0) {
          string cand_boot = cand_dir * "/texmacs.boot";
          if (exists (url_system (cand_boot))) texmacs_boot = cand_boot;
        }
      }

      if (N (texmacs_boot) > 0) {
#if defined(_WIN32)
        char resolved[_MAX_PATH];
        if (_fullpath (resolved, c_string (texmacs_boot), _MAX_PATH) != NULL) {
          texmacs_boot = string (resolved);
        }
#else
        char resolved[PATH_MAX];
        if (realpath (c_string (texmacs_boot), resolved) != NULL) {
          texmacs_boot = string (resolved);
        }
#endif
        s_texmacs_boot_file = texmacs_boot;
        s_has_texmacs_boot = true;
        fprintf (stderr, "Chez Scheme: booting from boot file %s\n", (char*) c_string (texmacs_boot));
        Sregister_boot_file (c_string (texmacs_boot));
      }
    }
  }

  const char* execpath = (s_argv != NULL && s_argv[0] != NULL) ? s_argv[0] : "texmacs";
  Sbuild_heap (execpath, custom_init);
  s_chez_heap_built = true;
}

void
start_scheme (int argc, char** argv, void (*call_back) (int, char**)) {
  s_argc = argc;
  s_argv = argv;
  if (call_back != NULL) call_back (argc, argv);
  if (s_scheme_initialized) Sscheme_deinit ();
}

/* ------------------------------------------------------------------------
 * Initialization
 * ---------------------------------------------------------------------- */

extern "C" void
chez_cache_runtime_procedures () {
  // Only run once, can be invoked from scheme or C++.
  if (object_stack != 0) return;
  tm_null= Stop_level_value (Sstring_to_symbol ("tmscm-null"));
  Slock_object (tm_null);
  tm_blackbox_guardian= Stop_level_value (Sstring_to_symbol ("tm-blackbox-guardian"));
  ASSERT (tm_blackbox_guardian != 0 && Sprocedurep (tm_blackbox_guardian),
          "tm-blackbox-guardian must be a procedure");
  Slock_object (tm_blackbox_guardian);
  tm_blackbox_rtd= Stop_level_value (Sstring_to_symbol ("tm-blackbox-rtd"));
  ASSERT (tm_blackbox_rtd != 0 && Srecordp (tm_blackbox_rtd),
          "tm-blackbox-rtd must be a record type descriptor");
  Slock_object (tm_blackbox_rtd);
  tm_make_blackbox= Stop_level_value (Sstring_to_symbol ("tm-make-blackbox"));
  ASSERT (tm_make_blackbox != 0 && Sprocedurep (tm_make_blackbox),
          "tm-make-blackbox must be a procedure");
  Slock_object (tm_make_blackbox);
  tm_eval_string_proc= Stop_level_value (Sstring_to_symbol ("tm-eval-string"));
  ASSERT (tm_eval_string_proc != 0 && Sprocedurep (tm_eval_string_proc),
          "tm-eval-string must be a procedure");
  Slock_object (tm_eval_string_proc);
  tm_load_file_proc= Stop_level_value (Sstring_to_symbol ("tm-load-file"));
  ASSERT (tm_load_file_proc != 0 && Sprocedurep (tm_load_file_proc),
          "tm-load-file must be a procedure");
  Slock_object (tm_load_file_proc);
  tm_apply_proc= Stop_level_value (Sstring_to_symbol ("tm-apply"));
  ASSERT (tm_apply_proc != 0 && Sprocedurep (tm_apply_proc),
          "tm-apply must be a procedure");
  Slock_object (tm_apply_proc);
  tm_call_0_proc= Stop_level_value (Sstring_to_symbol ("tm-call-0"));
  Slock_object (tm_call_0_proc);
  tm_call_1_proc= Stop_level_value (Sstring_to_symbol ("tm-call-1"));
  Slock_object (tm_call_1_proc);
  tm_call_2_proc= Stop_level_value (Sstring_to_symbol ("tm-call-2"));
  Slock_object (tm_call_2_proc);
  tm_call_3_proc= Stop_level_value (Sstring_to_symbol ("tm-call-3"));
  Slock_object (tm_call_3_proc);
  tm_call_4_proc= Stop_level_value (Sstring_to_symbol ("tm-call-4"));
  Slock_object (tm_call_4_proc);
  tm_call_n_proc= Stop_level_value (Sstring_to_symbol ("tm-call-n"));
  Slock_object (tm_call_n_proc);
  tm_list_pred= Stop_level_value (Sstring_to_symbol ("list?"));
  ASSERT (tm_list_pred != 0 && Sprocedurep (tm_list_pred),
          "list? must be a procedure");
  Slock_object (tm_list_pred);
  tm_equal_pred= Stop_level_value (Sstring_to_symbol ("equal?"));
  ASSERT (tm_equal_pred != 0 && Sprocedurep (tm_equal_pred),
          "equal? must be a procedure");
  Slock_object (tm_equal_pred);
  object_stack= Stop_level_value (Sstring_to_symbol ("object-stack"));
  Slock_object (object_stack);
  if (chez_root_stack_vec == 0)
    chez_root_stack_grow ();
}

void
initialize_scheme () {
  /* Build the Scheme heap now that the TeXmacs server (the_server) has started.
     This ensures custom_init() and any registered boot files (e.g. texmacs.boot)
     can safely interact with the server and glue procedures without hitting
     uninitialized server crashes. */
  build_chez_heap ();

  if (!s_has_texmacs_boot) {
    string boot= boot_scheme_path ();
    if (!exists (url_system (boot)))
      boot_failed ("Chez Scheme: bootstrap file not found: " * boot);
    fprintf (stderr, "Chez Scheme: loading %s\n", (char*) c_string (boot));
    fflush (stderr);
    load_scheme_file (boot);
  }

  // probably doesn't run, since boot-chez.scm will call this also at the end.
  chez_cache_runtime_procedures ();

  if (s_has_texmacs_boot) {
    eval_scheme ("(define *texmacs-boot-mode* 'boot)");
  }

  fprintf (stderr, "Chez Scheme: %d native glue procedures registered\n",
           chez_glue_procedure_count ());
  fflush (stderr);
}
