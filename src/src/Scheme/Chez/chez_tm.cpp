
/******************************************************************************
* MODULE     : chez_tm.cpp
* DESCRIPTION: Interface to Chez Scheme
* COPYRIGHT  : (C) 2020 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "chez_tm.hpp"
#include "blackbox.hpp"
#include "file.hpp"
#include "../Scheme/glue.hpp"
#include "convert.hpp" // tree_to_texmacs (should not belong here)

#include <unistd.h> // for getpid

/******************************************************************************
 * Initialization of Chez
 ******************************************************************************/

//s7_scheme *tm_s7;
//s7_pointer user_env;

int tm_chez_argc;
char **tm_chez_argv;

void
start_scheme (int argc, char** argv, void (*call_back) (int, char**)) {
  tm_chez_argc = argc;
  tm_chez_argv = argv;
  
  Sscheme_init (NULL);
  Sregister_boot_file ("/Users/mgubi/t/ChezScheme/usr/lib/csv9.5.5/a6osx/petite.boot");
  Sregister_boot_file ("/Users/mgubi/t/ChezScheme/usr/lib/csv9.5.5/a6osx/scheme.boot");
  Sbuild_heap (NULL, NULL);
    
  call_back (argc, argv);
}

/******************************************************************************
 * Evaluation of files
 ******************************************************************************/

tmscm
eval_scheme_file (string file) {
    //static int cumul= 0;
    //timer tm;
  if (DEBUG_STD) debug_std << "Evaluating " << file << "...\n";
//  tmscm result= Scall2 (Stop_level_value (Sstring_to_symbol ("load")),
//                        string_to_tmscm (file),
//                        Stop_level_value (Sstring_to_symbol ("*texmacs-user-module*")));
//  tmscm result= eval_scheme (s);
  tmscm result= Scall1 (Stop_level_value (Sstring_to_symbol ("tm-load")),
                        string_to_tmscm (file));

    //int extra= tm->watch (); cumul += extra;
    //cout << extra << "\t" << cumul << "\t" << file << "\n";
  return result;
}

/******************************************************************************
 * Evaluation of strings
 ******************************************************************************/

tmscm
eval_scheme (string s) {
   cout << "Eval] " << s << "\n";
  tmscm result= Scall1 (Stop_level_value (Sstring_to_symbol ("tm-eval-string")),
                        string_to_tmscm (s));
  return result;
}

/******************************************************************************
 * Using scheme objects as functions
 ******************************************************************************/

struct arg_list { int  n; tmscm* a; };

static tmscm
TeXmacs_call_scm (arg_list *args) {
  tmscm r;
  if (args->n == 0)
    r = Scall0 (args->a[0]);
  else if (args->n == 1)
    r = Scall1 (args->a[0], args->a[1]);
  else if (args->n == 2)
    r = Scall2 (args->a[0], args->a[1] , args->a[2]);
  else if (args->n == 3)
    r = Scall3 (args->a[0], args->a[1] , args->a[2], args->a[3]);
  else if (args->n > 3) {
    Sinitframe (args->n);
    for (int i=1; i <= args->n; i++)
      Sput_arg (i, args->a[i]);
    r= Scall(args->a[0], args->n);
  } else r= tmscm_null();
  return r;
}

tmscm
call_scheme (tmscm fun) {
  tmscm a[]= { fun }; arg_list args= { 0, a };
  return TeXmacs_call_scm (&args);
}

tmscm
call_scheme (tmscm fun, tmscm a1) {
  tmscm a[]= { fun, a1 }; arg_list args= { 1, a };
  return TeXmacs_call_scm (&args);
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2) {
  tmscm a[]= { fun, a1, a2 }; arg_list args= { 2, a };
  return TeXmacs_call_scm (&args);
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3) {
  tmscm a[]= { fun, a1, a2, a3 }; arg_list args= { 3, a };
  return TeXmacs_call_scm (&args);
}

tmscm
call_scheme (tmscm fun, tmscm a1, tmscm a2, tmscm a3, tmscm a4) {
  tmscm a[]= { fun, a1, a2, a3, a4 }; arg_list args= { 4, a };
  return TeXmacs_call_scm (&args);
}

tmscm
call_scheme (tmscm fun, array<tmscm> a) {
  const int n= N(a);
  STACK_NEW_ARRAY(scm, tmscm, n+1);
  int i;
  scm[0]= fun;
  for (i=0; i<n; i++) scm[i+1]= a[i];
  arg_list args= { n, scm };
  tmscm ret= TeXmacs_call_scm (&args);
  STACK_DELETE_ARRAY(scm);
  return ret;
}


/******************************************************************************
 * Miscellaneous routines for use by glue only
 ******************************************************************************/

string
scheme_dialect () {
  return "chez";
}

/******************************************************************************
 * Strings
 ******************************************************************************/


tmscm
string_to_tmscm (string s) {
  c_string _s (s);
  tmscm r= Sstring_of_length (_s, N(s));
  return r;
}

string
tmscm_to_string (tmscm s) {
  string r;
  tmscm_lock (s);
  tmscm bv = Scall1 (Stop_level_value (Sstring_to_symbol("tm-string-decode")), s);
  tmscm_lock (bv);   tmscm_unlock (s);
  if (!Sbytevectorp (bv)) {
    if (Spairp (bv) && Sbytevectorp (Scar (bv))) {
      int len_r = Sbytevector_length (Scar (bv));
      const char* _r= (const char*) Sbytevector_data (Scar (bv));
      string r1 (_r, len_r);
      string rr= utf8_to_cork (r);
      r = rr;
    } else r = "<tm string conversion error>";
  } else {
    int len_r = Sbytevector_length (bv);
    const char* _r= (const char*) Sbytevector_data (bv);
    string r1 (_r, len_r);
    r = r1;
  }
  tmscm_unlock (bv);
  return r;
}

/******************************************************************************
 * Symbols
 ******************************************************************************/

tmscm
symbol_to_tmscm (string s) {
  c_string _s (s);
  tmscm r= Sstring_to_symbol (_s);
  return r;
}

string
tmscm_to_symbol (tmscm s) {
  return tmscm_to_string ( Ssymbol_to_string (s));
}

/******************************************************************************
 * Blackbox
 ******************************************************************************/

bool
tmscm_is_blackbox (tmscm t) {
  return (tmscm_is_pair (t) && (tmscm_car (t) == Sstring_to_symbol("blackbox")));
}

tmscm
blackbox_to_tmscm (blackbox b) {
  return Scons (Sstring_to_symbol ("blackbox"),
                Sfixnum ((uptr)(tm_new<blackbox> (b))));
}

blackbox
tmscm_to_blackbox (tmscm blackbox_smob) {
  return *((blackbox *) Sfixnum_value (tmscm_cdr (blackbox_smob)));
}


static tmscm blackbox_to_string (tmscm o)
{
  string s = "<not a blackbox!>";

  if (tmscm_is_blackbox (o)) {
  tmscm blackbox_smob = tmscm_cdr (o);
  int type_ = type_box (tmscm_to_blackbox (blackbox_smob)) ;
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
    s= "<command>";
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
  }
  return string_to_tmscm (s);
}
  
// FIXME: What about this ?????
// Remember to free the blackboxes!!!!
static tmscm free_blackbox (tmscm obj)
{
  ASSERT (tmscm_is_blackbox (obj),  "free_blackbox : wrong type");
  blackbox *ptr = (blackbox *)  Sfixnum_value (tmscm_cdr (obj));
  tm_delete (ptr);
}






/******************************************************************************
 * Initialization
 ******************************************************************************/

tmscm object_stack;

void
scheme_install_procedure (const char *name, void * func, int args ) {
  string s;
  s << "(define " << name << " (foreign-procedure \"" << name << "\" (";
  for (int i=0; i<args; i++) s << "ptr ";
  s << ") ptr))";
  //cout << s << "\n";
  Sforeign_symbol (name, func);
  Scall1 (Stop_level_value (Sstring_to_symbol ("tm-eval-string")), string_to_tmscm (s));
}

void
initialize_scheme () {
  
  // setup basic infrastructure from within Scheme
  
  const char* init_prg = "(begin "
//  "(current-eval interpret)"
//  "(display \"Interpreting code\") (newline)"
  "(define (display-to-string obj) (call-with-string-output-port (lambda (port) (display obj port))))"
  "(define (texmacs-version) \"" TEXMACS_VERSION "\")"
  "(define object-stack '(()))"
  "(define *texmacs-user-module* #f)"
  "(define eval-string (lambda (s . env) \
      (apply eval (cons (with-input-from-string s read)  env))))"
  "(define tm-eval-string (lambda (s) (eval-string s *texmacs-user-module*)))"
  "(define tm-load (lambda (file) (load file (lambda (e) (eval e *texmacs-user-module*)))))"
  "(define *latin1-transcoder* (make-transcoder \
      (latin-1-codec) (eol-style none) (error-handling-mode raise)))"
  "(define *utf8-transcoder* (make-transcoder \
      (utf-8-codec) (eol-style none) (error-handling-mode raise)))"
  "(define tm-string-decode (lambda (str) (call/cc (lambda (k) (with-exception-handler \
     (lambda (err) (k (cons 'utf8 (string->bytevector str *utf8-transcoder*))))\
     (lambda () (string->bytevector str *latin1-transcoder*)))))))"
  "(set! *texmacs-user-module* (interaction-environment))"
  ")";

  // eval in the top level environment the initialization code
  Scall1(Stop_level_value (Sstring_to_symbol ("eval")),
      Scall2 (Stop_level_value (Sstring_to_symbol ("with-input-from-string")),
                              string_to_tmscm (init_prg),
              Stop_level_value (Sstring_to_symbol ("read"))));
  
  // now the glue
  initialize_glue ();

  // we have to lock object_stack otherwise the GC will move it
  // alternatively we will have to change the code in object.cpp
  object_stack= Stop_level_value (Sstring_to_symbol ("object-stack"));
  Slock_object (object_stack);

  cout << "Init Scheme complete\n";
}

