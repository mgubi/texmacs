
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
  
  Sscheme_init(NULL);
  Sregister_boot_file("/Users/mgubi/t/ChezScheme/usr/lib/csv9.5.5/a6osx/petite.boot");
  Sregister_boot_file("/Users/mgubi/t/ChezScheme/usr/lib/csv9.5.5/a6osx/scheme.boot");
  Sbuild_heap(NULL, NULL);
    
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
  tmscm result= Scall2 (Stop_level_value (Sstring_to_symbol ("load")),
                        string_to_tmscm (file),
                        Stop_level_value (Sstring_to_symbol ("*texmacs-user-module*")));
    //int extra= tm->watch (); cumul += extra;
    //cout << extra << "\t" << cumul << "\t" << file << "\n";
  return result;
}

/******************************************************************************
 * Evaluation of strings
 ******************************************************************************/

tmscm
eval_scheme (string s) {
  // cout << "Eval] " << s << "\n";
  tmscm result= Scall2 (Stop_level_value (Sstring_to_symbol ("eval")),
                        string_to_tmscm (s),
                        Stop_level_value (Sstring_to_symbol ("*texmacs-user-module*")));
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
  }
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
  tmscm bv = Scall1 (Stop_level_value (Sstring_to_symbol("tm_string_decode")), s);
  if (!Sbytevectorp (bv)) return "";
  int len_r = Sbytevector_length (bv);
  const char* _r= (const char*) Sbytevector_data (bv);
  string r (_r, len_r);
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
initialize_scheme () {
  const char* init_prg = "(begin \n"
  "(define (display-to-string obj)\n"
  "  (call-with-output-string\n"
  "    (lambda (port) (display obj port))))\n"
  "\n"
  "(define (texmacs-version) \"" TEXMACS_VERSION "\")\n"
  "(define object-stack '(()))\n"
  ")";

  // eval in the root environment
  Scall1 (Stop_level_value (Sstring_to_symbol ("eval")),
                            string_to_tmscm (init_prg));

  initialize_glue ();

  // we have to lock object_stack otherwise the GC will move it
  // alternatively we will have to change the code in object.cpp
  
  object_stack= Stop_level_value (Sstring_to_symbol ("object-stack"));
  Slock_object(object_stack);
  
    // uncomment to have a guile repl available at startup	
    //	gh_repl(guile_argc, guile_argv);
    //scm_shell (guile_argc, guile_argv);
  
  
}

