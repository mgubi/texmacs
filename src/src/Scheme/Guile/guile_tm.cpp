
/******************************************************************************
* MODULE     : guile_tm.cpp
* DESCRIPTION: Interface to Guile
* COPYRIGHT  : (C) 1999-2019  Joris van der Hoeven and Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifdef OS_MINGW
  //FIXME: if this include is not here we have compilation problems on mingw32
  //       (probably name clashes with Windows headers)
  //#include "tree.hpp"
#endif
  //#include "Glue/glue.hpp"

#include "guile_tm.hpp"
#include "blackbox.hpp"
#include "file.hpp"
#include "../Scheme/glue.hpp"
#include "convert.hpp" // tree_to_texmacs (should not belong here)

/******************************************************************************
 * Installation of guile and initialization of guile
 ******************************************************************************/
bool scm_busy= false;

#if (defined(GUILE_C) || defined(GUILE_D))
static void (*old_call_back) (int, char**)= NULL;
static void
new_call_back (void *closure, int argc, char** argv) {
  (void) closure;
  
  old_call_back (argc, argv);
}
#endif


int guile_argc;
char **guile_argv;

void
start_scheme (int argc, char** argv, void (*call_back) (int, char**)) {
  guile_argc = argc;
  guile_argv = argv;
#if (defined(GUILE_C) || defined(GUILE_D))
  old_call_back= call_back;
#if (defined(GUILE_D))
  // Guile 2/3 compiles every module it loads through use-modules and
  // caches the result. TeXmacs code does not compile: many of its macros
  // call helpers defined in the same file, which only exist when the file
  // is evaluated form by form, and the compiler silently falls back to
  // the interpreter after a costly failed attempt. We therefore run all
  // TeXmacs Scheme code interpreted. The variable must be set before
  // Guile is initialised, since Guile reads it while booting.
  setenv ("GUILE_AUTO_COMPILE", "0", 1);
#endif
  scm_boot_guile (argc, argv, new_call_back, 0);
#else
#ifdef DOTS_OK
  gh_enter (argc, argv, (void (*)(...)) ((void*) call_back));
#else
  gh_enter (argc, argv, call_back);
#endif
#endif
}



/******************************************************************************
 * Catching errors (with thanks to Dale P. Smith)
 ******************************************************************************/

SCM
TeXmacs_lazy_catcher (void *data, SCM tag, SCM throw_args) {
  SCM eport= scm_current_error_port();
  scm_handle_by_message_noexit (data, tag, throw_args);
  scm_force_output (eport);
  scm_ithrow (tag, throw_args, 1);
  return SCM_UNSPECIFIED; /* never returns */
}

SCM
TeXmacs_catcher (void *data, SCM tag, SCM args) {
  (void) data;
  return scm_cons (tag, args);
}

/******************************************************************************
 * Evaluation of files
 ******************************************************************************/

#if (defined(GUILE_D))
static SCM
TeXmacs_primitive_load (char *file) {
  // primitive-load is redefined in initialize_scheme, so that TeXmacs
  // files are read as Latin-1, as Guile 1.8 did
  SCM load= scm_variable_ref (scm_c_lookup ("primitive-load"));
  return scm_call_1 (load, scm_from_locale_string (file));
}
#define TM_PRIMITIVE_LOAD TeXmacs_primitive_load
#else
#define TM_PRIMITIVE_LOAD scm_c_primitive_load
#endif

#ifndef DEBUG_ON
static SCM
TeXmacs_lazy_eval_file (char *file) {
#if (defined(GUILE_D))
  return scm_c_with_throw_handler (SCM_BOOL_T,
                                  (scm_t_catch_body) TM_PRIMITIVE_LOAD, file,
                                  (scm_t_catch_handler) TeXmacs_lazy_catcher, file, 0);
#else
  return scm_internal_lazy_catch (SCM_BOOL_T,
                                  (scm_t_catch_body) scm_c_primitive_load, file,
                                  (scm_t_catch_handler) TeXmacs_lazy_catcher, file);
#endif
}
#endif

static SCM
TeXmacs_eval_file (char *file) {
#ifndef DEBUG_ON
  return scm_internal_catch (SCM_BOOL_T,
                             (scm_t_catch_body) TeXmacs_lazy_eval_file, file,
                             (scm_t_catch_handler) TeXmacs_catcher, file);
#else
  return 	TM_PRIMITIVE_LOAD (file);
#endif
}

SCM
eval_scheme_file (string file) {
    //static int cumul= 0;
    //timer tm;
  if (DEBUG_STD) debug_std << "Evaluating " << file << "...\n";
  c_string _file (file);
  SCM result= TeXmacs_eval_file (_file);
    //int extra= tm->watch (); cumul += extra;
    //cout << extra << "\t" << cumul << "\t" << file << "\n";
  return result;
}

/******************************************************************************
 * Evaluation of strings
 ******************************************************************************/

#ifndef DEBUG_ON
static SCM
TeXmacs_lazy_eval_string (char *s) {
#if (defined(GUILE_D))
  return scm_c_with_throw_handler (SCM_BOOL_T,
                                  (scm_t_catch_body) scm_c_eval_string, s,
                                  (scm_t_catch_handler) TeXmacs_lazy_catcher, s, 0);
#else
  return scm_internal_lazy_catch (SCM_BOOL_T,
                                  (scm_t_catch_body) scm_c_eval_string, s,
                                  (scm_t_catch_handler) TeXmacs_lazy_catcher, s);
#endif
}
#endif

static SCM
TeXmacs_eval_string (char *s) {
#ifndef DEBUG_ON
  return scm_internal_catch (SCM_BOOL_T,
                             (scm_t_catch_body) TeXmacs_lazy_eval_string, s,
                             (scm_t_catch_handler) TeXmacs_catcher, s);
#else
  return  scm_c_eval_string(s);
#endif
}

SCM
eval_scheme (string s) {
    // cout << "Eval] " << s << "\n";
#ifdef DEBUG_ON
if ( ! scm_busy) {
#endif
  c_string _s (s);
  SCM result= TeXmacs_eval_string (_s);
  return result;
#ifdef DEBUG_ON
  } else return SCM_BOOL_F;
#endif
}

/******************************************************************************
 * Using scheme objects as functions
 ******************************************************************************/

struct arg_list { int  n; SCM* a; };

static SCM
TeXmacs_call (arg_list* args) {
  switch (args->n) {
    case 0: return scm_call_0 (args->a[0]); break;
    case 1: return scm_call_1 (args->a[0], args->a[1]); break;
    case 2: return scm_call_2 (args->a[0], args->a[1], args->a[2]); break;
    case 3:
      return scm_call_3 (args->a[0], args->a[1], args->a[2], args->a[3]); break;
    default:
    {
      int i;
      SCM l= SCM_NULL;
      for (i=args->n; i>=1; i--)
        l= scm_cons (args->a[i], l);
      return scm_apply_0 (args->a[0], l);
    }
  }
}

#ifndef DEBUG_ON
static SCM
TeXmacs_lazy_call_scm (arg_list* args) {
#if (defined(GUILE_D))
  return scm_c_with_throw_handler (SCM_BOOL_T,
                                  (scm_t_catch_body) TeXmacs_call, (void*) args,
                                  (scm_t_catch_handler) TeXmacs_lazy_catcher, (void*) args, 0);
#else
  return scm_internal_lazy_catch (SCM_BOOL_T,
                                  (scm_t_catch_body) TeXmacs_call, (void*) args,
                                  (scm_t_catch_handler) TeXmacs_lazy_catcher, (void*) args);
#endif
}
#endif

static SCM
TeXmacs_call_scm (arg_list *args) {
#ifndef DEBUG_ON
  return scm_internal_catch (SCM_BOOL_T,
                             (scm_t_catch_body) TeXmacs_lazy_call_scm, (void*) args,
                             (scm_t_catch_handler) TeXmacs_catcher, (void*) args);
#else
  return TeXmacs_call(args);
#endif
}

SCM
call_scheme (SCM fun) {
// uncomment block to display scheme call
/*
  SCM ENDLscm= scm_from_locale_string ("\n");
  SCM source=scm_procedure_source(fun);
  scm_call_2(scm_c_eval_string("display*"), source, ENDLscm);
  scm_call_2(scm_c_eval_string("display*"),  scm_procedure_environment(fun), ENDLscm);
  scm_call_2(scm_c_eval_string("display*"),  scm_procedure_properties(fun), ENDLscm);
  //DBGFMT1(debug_tmwidgets, source);
*/
  SCM a[]= { fun }; arg_list args= { 0, a };
  return TeXmacs_call_scm (&args);
}

SCM
call_scheme (SCM fun, SCM a1) {
  SCM a[]= { fun, a1 }; arg_list args= { 1, a };
  return TeXmacs_call_scm (&args);
}

SCM
call_scheme (SCM fun, SCM a1, SCM a2) {
  SCM a[]= { fun, a1, a2 }; arg_list args= { 2, a };
  return TeXmacs_call_scm (&args);
}

SCM
call_scheme (SCM fun, SCM a1, SCM a2, SCM a3) {
  SCM a[]= { fun, a1, a2, a3 }; arg_list args= { 3, a };
  return TeXmacs_call_scm (&args);
}

SCM
call_scheme (SCM fun, SCM a1, SCM a2, SCM a3, SCM a4) {
  SCM a[]= { fun, a1, a2, a3, a4 }; arg_list args= { 4, a };
  return TeXmacs_call_scm (&args);
}

SCM
call_scheme (SCM fun, array<SCM> a) {
  const int n= N(a);
  STACK_NEW_ARRAY(scm, SCM, n+1);
  int i;
  scm[0]= fun;
  for (i=0; i<n; i++) scm[i+1]= a[i];
  arg_list args= { n, scm };
  SCM ret= TeXmacs_call_scm (&args);
  STACK_DELETE_ARRAY(scm);
  return ret;
}


/******************************************************************************
 * Miscellaneous routines for use by glue only
 ******************************************************************************/

string
scheme_dialect () {
#ifdef GUILE_A
  return "guile-a";
#else
#ifdef GUILE_B
  return "guile-b";
#else
#ifdef GUILE_C
  return "guile-c";
#else
#ifdef GUILE_D
  return "guile-d";
#else
  return "unknown";
#endif
#endif
#endif
#endif
}

#if (defined(GUILE_C) || defined(GUILE_D))
#define SET_SMOB(smob,data,type)   \
SCM_NEWSMOB (smob, SCM_UNPACK (type), data);
#else
#define SET_SMOB(smob,data,type)   \
SCM_NEWCELL (smob);              \
SCM_SETCAR (smob, (SCM) (type)); \
SCM_SETCDR (smob, (SCM) (data));
#endif


/******************************************************************************
 * Booleans
 ******************************************************************************/


SCM
bool_to_scm (bool flag) {
  return scm_bool2scm (flag);
}

#if (defined(GUILE_A) || defined(GUILE_B))
int
scm_to_bool (SCM flag) {
  return scm_scm2bool (flag);
}
#endif

/******************************************************************************
 * Integers
 ******************************************************************************/

SCM
int_to_scm (int i) {
#if (defined(GUILE_D))
  return scm_from_int (i);
#else
  return scm_long2scm ((long) i);
#endif
}

SCM
long_to_scm (long l) {
#if (defined(GUILE_D))
  return scm_from_long (l);
#else
  return scm_long2scm (l);
#endif
}

#if (defined(GUILE_A) || defined(GUILE_B))
int
scm_to_int (SCM i) {
  return (int) scm_scm2long (i);
}

long
scm_to_long (SCM l) {
  return scm_scm2long (l);
}
#endif

/******************************************************************************
 * Floating point numbers
 ******************************************************************************/
#if 0
bool scm_is_double (scm o) {
  return SCM_REALP(o);
}
#endif

SCM
double_to_scm (double i) {
  return scm_double2scm (i);
}

#if (defined(GUILE_A) || defined(GUILE_B))
double
scm_to_double (SCM i) {
  return scm_scm2double (i);
}
#endif

/******************************************************************************
 * Strings
 ******************************************************************************/


tmscm
string_to_tmscm (string s) {
  c_string _s (s);
#ifdef DEBUG_ON
  if (! scm_busy) {
#endif
  SCM r= scm_str2scm (_s, N(s));
  return r;
#ifdef DEBUG_ON
  } else return SCM_BOOL_F;
#endif
}

#ifdef GUILE_D

// Guile 2/3 strings are sequences of Unicode characters, while TeXmacs
// strings are sequences of bytes in its own (Cork based) encoding.
// string_to_tmscm smuggles the bytes into Scheme as Latin-1 characters,
// so that every TeXmacs string survives the round trip unchanged.
// Strings which do not come from TeXmacs (literals in Scheme files,
// results of Guile library functions) may also contain characters beyond
// Latin-1; such strings are taken to be Unicode text and converted to
// Cork. We only use the public API: the string is read as UTF-8, which
// is pure ASCII in the most frequent case.

static string
guile_utf8_to_tm (const char* s, size_t n) {
  size_t i;
  for (i=0; i<n; i++)
    if (((unsigned char) s[i]) >= 0x80) break;
  if (i == n) return string (s, (int) n);  // ASCII
  string r;
  for (i=0; i<n; ) {
    unsigned char c= (unsigned char) s[i];
    if (c < 0x80) { r << ((char) c); i++; }
    else if ((c & 0xe0) == 0xc0 && c <= 0xc3 && i+1 < n) {
      // a character of Latin-1, that is a byte of TeXmacs
      r << ((char) (((c & 0x03) << 6) | (((unsigned char) s[i+1]) & 0x3f)));
      i += 2;
    }
    else return utf8_to_cork (string (s, (int) n));
  }
  return r;
}

string
tmscm_to_string (tmscm s) {
  size_t len_r;
  char* _r= scm_to_utf8_stringn (s, &len_r);
  string r= guile_utf8_to_tm (_r, len_r);
  free (_r);
  return r;
}
#else
string
tmscm_to_string (tmscm s) {
  guile_str_size_t len_r;
  char* _r= scm_scm2str (s, &len_r);
  string r (_r, len_r);
  #ifdef OS_WIN32
    scm_must_free(_r);
  #else
    free (_r);
  #endif
  return r;
}
#endif // #ifdef GUILE_D


/******************************************************************************
 * Symbols
 ******************************************************************************/

#if 0
bool tmscm_is_symbol (tmscm s) {
  return SCM_NFALSEP (scm_symbol_p (s));
}
#endif

tmscm
symbol_to_tmscm (string s) {
#ifdef GUILE_D
  // as for strings: the bytes of TeXmacs are Latin-1 characters
  c_string _s (s);
  return scm_from_latin1_symboln (_s, N(s));
#else
  c_string _s (s);
  SCM r= scm_symbol2scm (_s);
  return r;
#endif
}

string
tmscm_to_symbol (tmscm s) {
#ifdef GUILE_D
  return tmscm_to_string (scm_symbol_to_string (s));
#else
  guile_str_size_t len_r;
  char* _r= scm_scm2symbol (s, &len_r);
  string r (_r, len_r);
#ifdef OS_WIN32
  scm_must_free(_r);
#else
  free (_r);
#endif
  return r;
#endif
}

/******************************************************************************
 * Blackbox
 ******************************************************************************/

#if defined(SIZEOF_ENT) && SIZEOF_ENT == SCM_SIZEOF_LONG_LONG
static long long blackbox_tag;
#define SCM_BLACKBOXP(t) (SCM_NIMP (t) && (((long long) SCM_CAR (t)) == blackbox_tag))
#else
static long blackbox_tag;
#define SCM_BLACKBOXP(t) (SCM_NIMP (t) && (((long) SCM_CAR (t)) == blackbox_tag))
#endif

bool
tmscm_is_blackbox (tmscm t) {
  return SCM_BLACKBOXP (t);
}

tmscm
blackbox_to_tmscm (blackbox b) {
  SCM blackbox_smob;
#if (defined(GUILE_D))
  // we run finalizers on the main thread periodically since our memory allocation scheme
  // is not thread safe.
  scm_run_finalizers ();
#endif
  SET_SMOB (blackbox_smob, (void*) (tm_new<blackbox> (b)), (SCM) blackbox_tag);
  return blackbox_smob;
}

blackbox
tmscm_to_blackbox (tmscm blackbox_smob) {
  return *((blackbox*) SCM_CDR (blackbox_smob));
}

static SCM
mark_blackbox (SCM blackbox_smob) {
  (void) blackbox_smob;
  return SCM_BOOL_F;
}

static scm_sizet
free_blackbox (SCM blackbox_smob) {
  blackbox *ptr = (blackbox *) SCM_CDR (blackbox_smob);
#ifdef DEBUG_ON
  scm_busy= true;
#endif
  tm_delete (ptr);
#ifdef DEBUG_ON
  scm_busy= false;
#endif
  return 0;
}

int
print_blackbox (SCM blackbox_smob, SCM port, scm_print_state *pstate) {
  (void) pstate;
  string s = "<blackbox>";
  int type_ = type_box (tmscm_to_blackbox(blackbox_smob)) ;
  if (type_ == type_helper<tree>::id) {
    tree t= tmscm_to_tree (blackbox_smob);
    s= "<tree " * tree_to_texmacs (t) * ">";
  }
  else if (type_ == type_helper<observer>::id) {
    s= "<observer>";
  }
  else if (type_ == type_helper<widget>::id) {
    s= "<widget>";
  }
  else if (type_ == type_helper<promise<widget> >::id) {
    s= "<promise-widget>";
  }
  else if (type_ == type_helper<command>::id) {
    command cmd= tmscm_to_command (blackbox_smob);
    s= print_to_string<command> (cmd);
  }
  else if (type_ == type_helper<url>::id) {
    url u= tmscm_to_url (blackbox_smob);
    s= "<url " * as_string (u) * ">";
  }
  else if (type_ == type_helper<modification>::id) {
    s= "<modification>";
  }
  else if (type_ == type_helper<patch>::id) {
    s= "<patch>";
  }
  
  scm_display (string_to_tmscm (s), port);
  return 1;
}

static SCM
cmp_blackbox (SCM t1, SCM t2) {
  return scm_bool2scm (tmscm_to_blackbox (t1) == tmscm_to_blackbox (t2));
}



/******************************************************************************
 * Initialization
 ******************************************************************************/


#ifdef SCM_NEWSMOB
void
initialize_smobs () {
  blackbox_tag= scm_make_smob_type (const_cast<char*> ("blackbox"), 0);
  scm_set_smob_mark (blackbox_tag, mark_blackbox);
  scm_set_smob_free (blackbox_tag, free_blackbox);
  scm_set_smob_print (blackbox_tag, print_blackbox);
  scm_set_smob_equalp (blackbox_tag, cmp_blackbox);
}

#else

scm_smobfuns blackbox_smob_funcs = {
  mark_blackbox, free_blackbox, print_blackbox, cmp_blackbox
};


void
initialize_smobs () {
  blackbox_tag= scm_newsmob (&blackbox_smob_funcs);
}

#endif

tmscm object_stack;

void
initialize_scheme () {
  
#if (defined(GUILE_D))
  // we do not want finalizers to be called in concurrent threads...
  scm_set_automatic_finalization_enabled (0);
#endif
  
  const char* init_prg =
//  "(display (current-module)) (display \"\\n\")\n"
//  "(set-current-module the-root-module)\n"
//  "(display (current-module)) (display \"\\n\")\n"
  "(read-set! keywords 'prefix)\n"
  "(read-enable 'positions)\n"
#if (!defined(GUILE_D))
  "(debug-enable 'debug)\n"
#endif
#ifdef DEBUG_ON
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
  "(define object-stack '(()))";
  
  scm_c_eval_string (init_prg);
#if (defined(GUILE_D))
  // TeXmacs strings are sequences of bytes, which we pass to Guile as
  // Latin-1 characters (see string_to_tmscm). Guile 2/3 reads source
  // files as UTF-8 by default, so that a string literal with non ASCII
  // characters would not hold the bytes of the file, as it did with
  // Guile 1.8 and as the rest of TeXmacs expects. We therefore read the
  // Scheme files of TeXmacs (but not those of Guile itself) as Latin-1,
  // by redefining primitive-load and primitive-load-path, which are used
  // by load and by the module system. The standard ports also use
  // Latin-1, so that displayed strings come out byte for byte.
  const char* load_prg =
  "(define (texmacs-guile-file? f)\n"
  "  (or (string-prefix? (%package-data-dir) f)\n"
  "      (string-prefix? (%global-site-dir) f)\n"
  "      (string-prefix? (%site-dir) f)))\n"
  "(define (texmacs-load-latin1 file)\n"
  "  (if %load-hook (%load-hook file))\n"
  "  (call-with-port\n"
  "    (open-input-file file #:encoding \"ISO-8859-1\")\n"
  "    (lambda (port)\n"
  "      (let loop ()\n"
  "        (let ((form ((or (fluid-ref current-reader) read) port)))\n"
  "          (if (not (eof-object? form))\n"
  "              (begin (primitive-eval form) (loop))))))))\n"
  "(define texmacs-guile-primitive-load primitive-load)\n"
  "(define texmacs-guile-primitive-load-path primitive-load-path)\n"
  "(set! primitive-load\n"
  "  (lambda (file)\n"
  "    (if (texmacs-guile-file? file)\n"
  "        (texmacs-guile-primitive-load file)\n"
  "        (texmacs-load-latin1 file))))\n"
  "(set! primitive-load-path\n"
  "  (lambda (name . opt)\n"
  "    (let ((file (%search-load-path name)))\n"
  "      (if (and file (not (texmacs-guile-file? file)))\n"
  "          (texmacs-load-latin1 file)\n"
  "          (apply texmacs-guile-primitive-load-path name opt)))))\n"
  "(set-port-encoding! (current-output-port) \"ISO-8859-1\")\n"
  "(set-port-encoding! (current-error-port) \"ISO-8859-1\")\n";
  scm_c_eval_string (load_prg);
#endif
  initialize_smobs ();
  initialize_glue ();
  object_stack= scm_lookup_string ("object-stack");
  
    // uncomment to have a guile repl available at startup
    //	gh_repl(guile_argc, guile_argv);
    //scm_shell (guile_argc, guile_argv);
  
  
}

