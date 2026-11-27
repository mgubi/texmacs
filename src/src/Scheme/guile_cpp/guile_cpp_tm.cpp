/******************************************************************************
 * MODULE     : guile_cpp_tm.cpp
 * DESCRIPTION: Interface to guile_cpp (portable C++20 rewrite of Guile 1.8)
 * COPYRIGHT  : (C) 2026  TeXmacs contributors
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "guile_cpp_tm.hpp"

#include <cmath>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <string>
#include <string_view>
#include <vector>

#include "blackbox.hpp"
#include "file.hpp"
#include "../Scheme/glue.hpp"
#include "convert.hpp" // tree_to_texmacs

/******************************************************************************
 * State & error handling
 ******************************************************************************/

int tmscm_builders= 0;
tmscm object_stack= guile::SCM_EOL;

namespace {

int g_caught_errors= 0;
bool g_scheme_started= false;
size_t g_blackbox_smob_type= static_cast<size_t> (-1);

tmscm handle_scheme_throw (const guile::SchemeThrow& ex) {
  auto& v= guile::vm ();
  if (ex.key == v.sym_quit) {
    int code= 0;
    if (guile::is_pair (ex.args)) {
      guile::SCM arg= guile::car (ex.args);
      if (arg.is_false ()) code= 1;
      else if (arg == guile::SCM_BOOL_T) code= 0;
      else if (guile::is_exact_integer (arg))
        code= static_cast<int> (guile::scm_to_int64 (arg, "quit", 1));
    }
    guile::flush_all_ports ();
    std::exit (code);
  }
  ++g_caught_errors;
  guile::SCM errp= v.current_error_port ();
  guile::port_puts (guile::as_obj<guile::PortObj> (errp), "ERROR: ");
  if (guile::is_pair (ex.args) &&
      guile::is_pair (guile::cdr (ex.args)) &&
      guile::is_pair (guile::cdr (guile::cdr (ex.args)))) {
    guile::SCM subr= guile::car (ex.args);
    guile::SCM msg= guile::car (guile::cdr (ex.args));
    guile::SCM margs= guile::car (guile::cdr (guile::cdr (ex.args)));
    guile::SCM rest= guile::is_pair (guile::cdr (guile::cdr (guile::cdr (ex.args))))
                       ? guile::car (guile::cdr (guile::cdr (guile::cdr (ex.args))))
                       : guile::SCM_BOOL_F;
    guile::display_error_impl (guile::SCM_BOOL_F, errp, subr, msg, margs, rest);
  } else {
    guile::port_puts (guile::as_obj<guile::PortObj> (errp), "Throw to key `");
    v.write_to_port (ex.key, errp, true);
    guile::port_puts (guile::as_obj<guile::PortObj> (errp), "' with args `");
    v.write_to_port (ex.args, errp, false);
    guile::port_puts (guile::as_obj<guile::PortObj> (errp), "'.\n");
  }
  guile::port_flush (guile::as_obj<guile::PortObj> (errp));
  return v.cons (ex.key, ex.args);
}

void free_blackbox_smob (guile::SCM obj) {
  auto* sm= guile::as_obj<guile::SmobObj> (obj);
  auto* ptr= static_cast<blackbox*> (sm->user_data);
  if (ptr != nullptr) {
    sm->user_data= nullptr;
    tm_delete (ptr);
  }
}

void print_blackbox_smob (guile::SCM obj, guile::SCM port) {
  string s= "<blackbox>";
  int type_= type_box (tmscm_to_blackbox (obj));
  if (type_ == type_helper<tree>::id) {
    tree t= tmscm_to_tree (obj);
    s= "<tree " * tree_to_texmacs (t) * ">";
  } else if (type_ == type_helper<observer>::id) {
    s= "<observer>";
  } else if (type_ == type_helper<widget>::id) {
    s= "<widget>";
  } else if (type_ == type_helper<promise<widget> >::id) {
    s= "<promise-widget>";
  } else if (type_ == type_helper<command>::id) {
    command cmd= tmscm_to_command (obj);
    s= print_to_string<command> (cmd);
  } else if (type_ == type_helper<url>::id) {
    url u= tmscm_to_url (obj);
    s= "<url " * as_string (u) * ">";
  } else if (type_ == type_helper<modification>::id) {
    s= "<modification>";
  } else if (type_ == type_helper<patch>::id) {
    s= "<patch>";
  }
  int n= N (s);
  guile::port_puts (guile::as_obj<guile::PortObj> (port),
                    n > 0 ? std::string_view (&s[0], static_cast<size_t> (n))
                          : std::string_view ());
}

guile::SCM equalp_blackbox_smob (guile::SCM a, guile::SCM b) {
  if (a == b) return guile::SCM_BOOL_T;
  if (!tmscm_is_blackbox (b)) return guile::SCM_BOOL_F;
  return guile::SCM::from_bool (tmscm_to_blackbox (a) == tmscm_to_blackbox (b));
}

uint64_t hash_tree_bounded (tree t, int& budget) {
  if (budget <= 0) return 0;
  --budget;
  if (is_atomic (t)) {
    return static_cast<uint64_t> (static_cast<uint32_t> (hash (t->label))) ^ 0x9e3779b97f4a7c15ULL;
  }
  uint64_t h= static_cast<uint64_t> (static_cast<uint32_t> (L (t))) * 2654435761ULL;
  int n= N (t);
  h^= static_cast<uint64_t> (n) * 2246822519ULL;
  int lim= n < 4 ? n : 4;
  for (int i= 0; i < lim && budget > 0; ++i) {
    h= (h * 1315423911ULL) ^ hash_tree_bounded (t[i], budget);
  }
  return h;
}

uint64_t hash_blackbox_smob (guile::SCM obj) {
  blackbox bb= tmscm_to_blackbox (obj);
  int type_= type_box (bb);
  if (type_ == type_helper<tree>::id) {
    int budget= 12;
    return hash_tree_bounded (tmscm_to_tree (obj), budget);
  } else if (type_ == type_helper<url>::id) {
    return static_cast<uint64_t> (static_cast<uint32_t> (hash (as_string (tmscm_to_url (obj)))));
  } else if (type_ == type_helper<path>::id) {
    path p= open_box<path> (bb);
    uint64_t h= 0x517cc1b727220a95ULL;
    for (int k= 0; !is_nil (p) && k < 16; p= p->next, ++k) {
      h= (h * 1315423911ULL) ^ static_cast<uint64_t> (static_cast<uint32_t> (p->item));
    }
    return h;
  }
  return static_cast<uint64_t> (static_cast<uint32_t> (type_)) * 2654435761ULL;
}

void initialize_smobs () {
  guile::SmobTypeInfo info;
  info.name= "blackbox";
  info.free_fn= free_blackbox_smob;
  info.print= print_blackbox_smob;
  info.equalp= equalp_blackbox_smob;
  info.hash_fn= hash_blackbox_smob;
  g_blackbox_smob_type= guile::vm ().make_smob_type (info);
}

} // namespace

int
scheme_caught_errors () {
  return g_caught_errors;
}

/******************************************************************************
 * Startup & initialization
 ******************************************************************************/

void
start_scheme (int argc, char** argv, void (*call_back) (int, char**)) {
  if (g_scheme_started) return;
  g_scheme_started= true;

  volatile int stack_anchor= 0;
  auto& v= guile::vm ();

  if (const char* env_jit= std::getenv ("GUILE_JIT")) {
    if (std::strcmp (env_jit, "0") == 0 || std::strcmp (env_jit, "off") == 0 ||
        std::strcmp (env_jit, "false") == 0 || std::strcmp (env_jit, "no") == 0) {
      v.jit_enabled= false;
    } else if (std::strcmp (env_jit, "1") == 0 || std::strcmp (env_jit, "on") == 0 ||
               std::strcmp (env_jit, "true") == 0 || std::strcmp (env_jit, "yes") == 0) {
      v.jit_enabled= true;
    }
  }

  // Pre-size initial GC threshold so startup finishes without mid-boot GC pauses,
  // then settle to a cache-friendly 1.5M cell threshold for steady-state editing.
  v.heap ().set_min_gc_threshold (1500000);
  v.heap ().set_initial_gc_threshold (4500000);

  try {
    v.init (const_cast<int*> (&stack_anchor), true);
  } catch (const guile::SchemeThrow& ex) {
    handle_scheme_throw (ex);
  }
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

  auto& v= guile::vm ();
  try {
    v.eval_string (init_prg);
  } catch (const guile::SchemeThrow& ex) {
    handle_scheme_throw (ex);
  }

  initialize_smobs ();
  initialize_glue ();

  guile::SCM ovar= v.lookup ("object-stack");
  if (guile::is_variable (ovar)) {
    object_stack= guile::as_obj<guile::VariableObj> (ovar)->value;
  } else {
    object_stack= v.list1 (guile::SCM_EOL);
  }
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
 * Evaluation & calls
 ******************************************************************************/

tmscm
eval_scheme_file (string file) {
  if (DEBUG_STD) debug_std << "Evaluating " << file << "...\n";
  int n= N (file);
  std::string path (n > 0 ? &file[0] : "", static_cast<size_t> (n));
  try {
    return guile::vm ().primitive_load (path);
  } catch (const guile::SchemeThrow& ex) {
    return handle_scheme_throw (ex);
  }
}

tmscm
eval_scheme (string s) {
  int n= N (s);
  try {
    return guile::vm ().eval_string (
        n > 0 ? std::string_view (&s[0], static_cast<size_t> (n))
              : std::string_view ());
  } catch (const guile::SchemeThrow& ex) {
    return handle_scheme_throw (ex);
  }
}

tmscm
call_scheme (tmscm fun) {
  try {
    return guile::vm ().call0 (fun);
  } catch (const guile::SchemeThrow& ex) {
    return handle_scheme_throw (ex);
  }
}

tmscm
call_scheme (tmscm fun, tmscm a1) {
  try {
    return guile::vm ().call1 (fun, a1);
  } catch (const guile::SchemeThrow& ex) {
    return handle_scheme_throw (ex);
  }
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2) {
  try {
    return guile::vm ().call2 (fun, a1, a2);
  } catch (const guile::SchemeThrow& ex) {
    return handle_scheme_throw (ex);
  }
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3) {
  try {
    return guile::vm ().call3 (fun, a1, a2, a3);
  } catch (const guile::SchemeThrow& ex) {
    return handle_scheme_throw (ex);
  }
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3, tmscm a4) {
  try {
    return guile::vm ().call4 (fun, a1, a2, a3, a4);
  } catch (const guile::SchemeThrow& ex) {
    return handle_scheme_throw (ex);
  }
}

tmscm
call_scheme (tmscm fun, array<tmscm> a) {
  const int n= N (a);
  if (n <= 8) {
    tmscm buf[8];
    for (int i= 0; i < n; ++i) buf[i]= a[i];
    try {
      return guile::vm ().apply (fun, buf, static_cast<size_t> (n));
    } catch (const guile::SchemeThrow& ex) {
      return handle_scheme_throw (ex);
    }
  }
  std::vector<tmscm> v (static_cast<size_t> (n));
  for (int i= 0; i < n; ++i) v[static_cast<size_t> (i)]= a[i];
  try {
    return guile::vm ().apply (fun, v.data (), static_cast<size_t> (n));
  } catch (const guile::SchemeThrow& ex) {
    return handle_scheme_throw (ex);
  }
}

/******************************************************************************
 * Numeric, string & symbol conversions
 ******************************************************************************/

bool
tmscm_is_int (tmscm obj) {
  if (guile::is_exact_integer (obj)) return true;
  if (guile::is_real_obj (obj)) {
    double d= guile::as_obj<guile::RealObj> (obj)->val;
    return std::isfinite (d) && std::trunc (d) == d;
  }
  return false;
}

int
tmscm_to_int (tmscm obj) {
  int64_t v= 0;
  if (obj.is_fixnum ()) {
    v= obj.to_fixnum ();
  } else if (guile::is_real_obj (obj)) {
    double d= guile::as_obj<guile::RealObj> (obj)->val;
    if (!std::isfinite (d) || d < -2147483648.0 || d > 2147483647.0) {
      guile::vm ().out_of_range ("tmscm_to_int", obj);
    }
    return static_cast<int> (std::llround (d));
  } else {
    v= guile::scm_to_int64 (obj, "tmscm_to_int", 1);
  }
  if (v < std::numeric_limits<int>::min () || v > std::numeric_limits<int>::max ()) {
    guile::vm ().out_of_range ("tmscm_to_int", obj);
  }
  return static_cast<int> (v);
}

unsigned int
tmscm_to_uint (tmscm obj) {
  int64_t v= 0;
  if (obj.is_fixnum ()) {
    v= obj.to_fixnum ();
  } else if (guile::is_real_obj (obj)) {
    double d= guile::as_obj<guile::RealObj> (obj)->val;
    if (!std::isfinite (d) || d < 0.0 || d > 4294967295.0) {
      guile::vm ().out_of_range ("tmscm_to_uint", obj);
    }
    return static_cast<unsigned int> (std::llround (d));
  } else {
    v= guile::scm_to_int64 (obj, "tmscm_to_uint", 1);
  }
  if (v < 0 || v > static_cast<int64_t> (std::numeric_limits<unsigned int>::max ())) {
    guile::vm ().out_of_range ("tmscm_to_uint", obj);
  }
  return static_cast<unsigned int> (v);
}

double
tmscm_to_double (tmscm obj) {
  return guile::scm_to_double (obj, "tmscm_to_double");
}

tmscm
string_to_tmscm (string s) {
  int n= N (s);
  return guile::vm ().make_string (
      n > 0 ? std::string_view (&s[0], static_cast<size_t> (n))
            : std::string_view ());
}

string
tmscm_to_string (tmscm obj) {
  if (!guile::is_string (obj)) {
    guile::vm ().wrong_type_arg ("tmscm_to_string", 1, obj, "string");
  }
  auto sv= guile::as_obj<guile::StringObj> (obj)->view ();
  return string (sv.data (), static_cast<int> (sv.size ()));
}

tmscm
symbol_to_tmscm (string s) {
  int n= N (s);
  return guile::vm ().make_symbol (
      n > 0 ? std::string_view (&s[0], static_cast<size_t> (n))
            : std::string_view ());
}

string
tmscm_to_symbol (tmscm obj) {
  if (!guile::is_symbol (obj)) {
    guile::vm ().wrong_type_arg ("tmscm_to_symbol", 1, obj, "symbol");
  }
  const auto& nm= guile::as_obj<guile::SymbolObj> (obj)->name;
  return string (nm.data (), static_cast<int> (nm.size ()));
}

/******************************************************************************
 * Blackbox SMOB
 ******************************************************************************/

bool
tmscm_is_blackbox (tmscm obj) {
  return guile::is_smob (obj) &&
         guile::as_obj<guile::SmobObj> (obj)->smob_type == g_blackbox_smob_type;
}

tmscm
blackbox_to_tmscm (blackbox b) {
  auto* sm= guile::vm ().heap ().alloc<guile::SmobObj> (g_blackbox_smob_type);
  sm->user_data= static_cast<void*> (tm_new<blackbox> (b));
  return guile::SCM::from_obj (sm);
}

blackbox
tmscm_to_blackbox (tmscm obj) {
  auto* sm= guile::as_obj<guile::SmobObj> (obj);
  return *static_cast<blackbox*> (sm->user_data);
}
