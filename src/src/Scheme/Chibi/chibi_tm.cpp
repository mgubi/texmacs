
/******************************************************************************
 * MODULE     : chibi_tm.cpp
 * DESCRIPTION: Interface to Chibi
 * COPYRIGHT  : (C) 2018 Massimiliano Gubinelli
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "chibi_tm.hpp"

#include "blackbox.hpp"
#include "file.hpp"
#include "../Scheme/glue.hpp"
#include "convert.hpp" // tree_to_texmacs (should not belong here)

tmscm scheme_context;

sexp_uint_t blackbox_type_tag = 0;

void finalize_blackbox (void *p) {
    tm_delete((blackbox*)p);
}

sexp sexp_finalize_blackbox (sexp ctx, sexp self, sexp_sint_t n, sexp obj) {
    if (sexp_cpointer_freep(obj))
        finalize_blackbox(sexp_cpointer_value(obj));
    return SEXP_VOID;
}


bool tmscm_is_blackbox (tmscm obj) {
    return sexp_pointerp(obj) && (sexp_pointer_tag(obj) == blackbox_type_tag);
}

blackbox tmscm_to_blackbox (tmscm obj) {
    ASSERT(tmscm_is_blackbox(obj), "this is not a blackbox!")
    return *(blackbox*)sexp_cpointer_value(obj);
}


tmscm blackbox_to_tmscm (blackbox b) {
    sexp_gc_var1(res);
    sexp_gc_preserve1(scheme_context, res);
    res = sexp_alloc_tagged(scheme_context, sexp_sizeof(cpointer), blackbox_type_tag);
    sexp_cpointer_value(res) = (void*)(tm_new<blackbox>(b));
    sexp_freep(res) = 1;
    sexp_gc_release1(scheme_context);
    return res;
}


sexp sexp_init_blackbox (sexp ctx, sexp env) {
    sexp sexp_blackbox_type_obj;
    sexp_gc_var3(name, tmp, op);
    sexp_gc_preserve3(ctx, name, tmp, op);
    name = sexp_c_string(ctx, "texmacs-blackbox", -1);
    sexp_blackbox_type_obj = sexp_register_c_type(ctx, name, sexp_finalize_blackbox);
    tmp = sexp_string_to_symbol(ctx, name);
    sexp_env_define(ctx, env, tmp, sexp_blackbox_type_obj);
    tmp = sexp_make_type_predicate(ctx, name, sexp_blackbox_type_obj);
    name = sexp_intern(ctx, "blackbox?", 9);
    sexp_env_define(ctx, env, name, tmp);
    blackbox_type_tag = sexp_type_tag(sexp_blackbox_type_obj);
    sexp_gc_release3(ctx);
    return SEXP_VOID;
}



/******************************************************************************
 * Entry points to Scheme
 ******************************************************************************/


tmscm tmscm_eval_string (const char *str)
{
    sexp_gc_var1(res);
    sexp_gc_preserve1(scheme_context, res);
    cout << "Eval: " << str << "\n";
    res = sexp_eval_string(scheme_context, str, -1,  NULL);
    print_result(scheme_context, sexp_context_env(scheme_context), res);
    sexp_gc_release1(scheme_context);
    return res;
}


tmscm tmscm_lookup_string(const char *name)
{
    return sexp_eval(scheme_context, sexp_intern(scheme_context, name, -1), sexp_context_env(scheme_context));
}

tmscm tmscm_apply (tmscm func, tmscm args)
{
    sexp_gc_var3(res, tmp1, tmp2);
    sexp_gc_preserve3(scheme_context, res, tmp1, tmp2);
    tmp1 = func;
    tmp2 = args;
    res =  sexp_apply(scheme_context, func, args);
    //cout << "@@@@ apply result :";
    //print_result(scheme_context, sexp_context_env(scheme_context), res);
    sexp_gc_release3(scheme_context);
    return res;
}


void tmscm_define(tmscm symbol, tmscm value)
{
    sexp_env_define(scheme_context, sexp_context_env(scheme_context), symbol, value);
}

tmscm object_stack;

/******************************************************************************
 * Installation of guile and initialization of guile
 ******************************************************************************/

void repl (sexp ctx, sexp env);
void print_result (sexp ctx, sexp env, sexp res);

void
start_scheme (int argc, char** argv, void (*call_back) (int, char**)) {
    // gh_enter (argc, argv, call_back);
    call_back(argc, argv);
}



static void sexp_add_path (sexp ctx, const char *str) {
    const char *colon;
    if (str && *str) {
        colon = strchr(str, ':');
        if (colon)
            sexp_add_path(ctx, colon+1);
        else
            colon = str + strlen(str);
        sexp_push(ctx, sexp_global(ctx, SEXP_G_MODULE_PATH), SEXP_VOID);
        sexp_car(sexp_global(ctx, SEXP_G_MODULE_PATH))
        = sexp_c_string(ctx, str, colon-str);
        sexp_immutablep(sexp_global(ctx, SEXP_G_MODULE_PATH)) = 1;
    }
}

void
initialize_scheme () {
    
    sexp_gc_var2(res, env);

    sexp_init();
    
    scheme_context = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
    //sexp_add_path(scheme_context, "/Users/mgubi/t/chibi-scheme/lib");

    sexp_gc_preserve2(scheme_context, res, env);

    sexp_load_standard_env(scheme_context, NULL, SEXP_SEVEN);
    sexp_load_standard_ports(scheme_context, NULL, stdin, stdout, stderr, 1);

    env = sexp_context_env(scheme_context);
    sexp_init_blackbox(scheme_context, env);
    
    const char* init_prg =
    "(begin \n"
#if 0
    "(read-set! keywords 'prefix)\n"
    "(read-enable 'positions)\n"
    "(debug-enable 'debug)\n"
    ";(debug-enable 'backtrace)\n"
    "\n"
    "(define (display-to-string obj)\n"
    "  (call-with-output-string\n"
    "    (lambda (port) (display obj port))))\n"
    "(define (object->string obj)\n"
    "  (call-with-output-string\n"
    "    (lambda (port) (write obj port))))\n"
#endif
//    "(define (texmacs-version) \"" TEXMACS_VERSION "\")\n"
    "(define (%%texmacs-version%%) \"" TEXMACS_VERSION "\")\n"
    "(define object-stack (cons '() '()))\n"
    ")";
    
    res = tmscm_eval_string (init_prg);
    print_result(scheme_context, sexp_context_env(scheme_context), res);

    initialize_glue ();
    object_stack= tmscm_lookup_string ("object-stack");

    res = tmscm_eval_string ("(define *texmacs-primitives* (env-exports (current-environment)))");
    print_result(scheme_context, env, res);

    res = tmscm_eval_string("(load (url-concretize \"$TEXMACS_PATH/progs/chibi/booter.scm\"))");
    print_result(scheme_context, env, res);

    
    //REPL
    repl(scheme_context, env);
    
    // abort();
    sexp_gc_release2(scheme_context);

    //FIXME: when we finalize chibi?
    //sexp_destroy_context(ctx);
}

#if 0
/******************************************************************************
 * Catching errors (with thanks to Dale P. Smith)
 ******************************************************************************/

tmscm
TeXmacs_lazy_catcher (void *data, tmscm tag, tmscm throw_args) {
    tmscm eport= tmscm_current_error_port();
    tmscm_handle_by_message_noexit (data, tag, throw_args);
    tmscm_force_output (eport);
    tmscm_ithrow (tag, throw_args, 1);
    return tmscm_UNSPECIFIED; /* never returns */
}

tmscm
TeXmacs_catcher (void *data, tmscm tag, tmscm args) {
    (void) data;
    return tmscm_cons (tag, args);
}
#endif

/******************************************************************************
 * Evaluation of files
 ******************************************************************************/

tmscm
eval_scheme_file (string file) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(scheme_context, result);

    //static int cumul= 0;
    //timer tm;
    //if (DEBUG_STD)
        debug_std << "Evaluating " << file << "...\n";
    c_string _file (file);
    result= sexp_load(scheme_context, sexp_c_string(scheme_context, _file, -1), NULL);
    //print_result(scheme_context, sexp_context_env(scheme_context), result);
    //FILE *f = fopen(_file, "r");
  //  tmscm result= tmscm_eval_file (f);
   // fclose(f);
    //int extra= tm->watch (); cumul += extra;
    //cout << extra << "\t" << cumul << "\t" << file << "\n";
    sexp_gc_release1(scheme_context);
    return result;
}

/******************************************************************************
 * Evaluation of strings
 ******************************************************************************/

tmscm
eval_scheme (string s) {
    // cout << "Eval] " << s << "\n";
    sexp_gc_var1(result);
    sexp_gc_preserve1(scheme_context, result);
    c_string _s (s);
    result= tmscm_eval_string (_s);
    sexp_gc_release1(scheme_context);
    return result;
}

/******************************************************************************
 * Using scheme objects as functions
 ******************************************************************************/

struct arg_list { int  n; tmscm* a; };

tmscm
TeXmacs_call_scm (arg_list* args) {
    sexp_gc_var3(tmp, l, res);
    sexp_gc_preserve3(scheme_context, tmp, l, res);
    switch (args->n) {
        default:
        {
            int i;
            l= tmscm_null ();
            for (i=args->n; i>=1; i--)
                l= tmscm_cons (args->a[i], l);
            tmp = args->a[0];
            res = tmscm_apply (tmp, l);
            cout << "Scheme call with result :";
            print_result(scheme_context, sexp_context_env(scheme_context), res);
        }
    }
    sexp_gc_release3(scheme_context);
    return res;
}
#if 0
static tmscm
TeXmacs_lazy_call_scm (arg_list* args) {
    return tmscm_internal_lazy_catch (tmscm_BOOL_T,
                                    (tmscm_t_catch_body) TeXmacs_call, (void*) args,
                                    (tmscm_t_catch_handler) TeXmacs_lazy_catcher, (void*) args);
}

static tmscm
TeXmacs_call_scm (arg_list *args) {
    return tmscm_internal_catch (tmscm_BOOL_T,
                               (tmscm_t_catch_body) TeXmacs_lazy_call_scm, (void*) args,
                               (tmscm_t_catch_handler) TeXmacs_catcher, (void*) args);
}
#endif
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
    STACK_NEW_ARRAY(v, tmscm, n+1);
    int i;
    v[0]= fun;
    for (i=0; i<n; i++) v[i+1]= a[i];
    arg_list args= { n, v };
    tmscm ret= TeXmacs_call_scm (&args);
    STACK_DELETE_ARRAY(v);
    return ret;
}


/******************************************************************************
 * Gluing
 ******************************************************************************/


string
scheme_dialect () {
    return "chibi-scheme";
}

void tmscm_define_glue(const char *name, int args, tmscm_foreign_func f)
{
    //  cout << "Define glue: " << name << LF;
    sexp_define_foreign(scheme_context, sexp_context_env(scheme_context), name, args, f);
}




/******************************************************************************
 * Strings
 ******************************************************************************/

tmscm
string_to_tmscm (string s) {
    sexp_gc_var1(r);
    sexp_gc_preserve1(scheme_context, r);
    c_string _s (s);
    r= sexp_c_string (scheme_context, _s, N(s));
    sexp_gc_release1(scheme_context);
    return r;
}

/******************************************************************************
 * Symbols
 ******************************************************************************/

tmscm
symbol_to_tmscm (string s) {
    sexp_gc_var1(r);
    sexp_gc_preserve1(scheme_context, r);
    c_string _s (s);
    r= sexp_intern (scheme_context,_s, N(s));
    sexp_gc_release1(scheme_context);
    return r;
}

/******************************************************************************
 * REPL (from chibi's sources)
 ******************************************************************************/


static sexp sexp_param_ref (sexp ctx, sexp env, sexp name) {
    sexp res = sexp_env_ref(ctx, env, name, SEXP_FALSE);
    return sexp_opcodep(res) ? sexp_parameter_ref(ctx, res) : NULL;
}


void print_result (sexp ctx, sexp env, sexp res) {
    sexp_gc_var2(out, err);
    sexp_gc_preserve2(ctx, out, err);
    out = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_OUT_SYMBOL));
    err = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_ERR_SYMBOL));
    if (out == NULL) {
        fprintf(stderr, "Standard I/O ports not found\n");
        return;
    }
    if (err == NULL) err = out;
    if (res && sexp_exceptionp(res)) {
        sexp_print_exception(ctx, res, err);
        if (res != sexp_global(ctx, SEXP_G_OOS_ERROR))
            sexp_stack_trace(ctx, err);
    } else if (res != SEXP_VOID) {
        sexp_write(ctx, res, out);
        sexp_write_char(ctx, '\n', out);
    }
    sexp_gc_release2(ctx);
}

void repl (sexp ctx, sexp env) {
    sexp_gc_var6(obj, tmp, res, in, out, err);
    sexp_gc_preserve6(ctx, obj, tmp, res, in, out, err);
    sexp_context_tracep(ctx) = 1;
    in  = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_IN_SYMBOL));
    out = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_OUT_SYMBOL));
    err = sexp_param_ref(ctx, env, sexp_global(ctx, SEXP_G_CUR_ERR_SYMBOL));
    if (in == NULL || out == NULL) {
        fprintf(stderr, "Standard I/O ports not found, aborting.  Maybe a bad -x language?\n");
        exit(70);
    }
    if (err == NULL) err = out;
    sexp_port_sourcep(in) = 1;
    while (1) {
        sexp_write_string(ctx, "> ", out);
        sexp_flush(ctx, out);
//        sexp_maybe_block_port(ctx, in, 1);
        obj = sexp_read(ctx, in);
 //       sexp_maybe_unblock_port(ctx, in);
        if (obj == SEXP_EOF)
            break;
        if (sexp_exceptionp(obj)) {
            sexp_print_exception(ctx, obj, err);
        } else {
            sexp_context_top(ctx) = 0;
            if (!(sexp_idp(obj)||sexp_pairp(obj)||sexp_nullp(obj)))
                obj = sexp_make_lit(ctx, obj);
            tmp = sexp_env_bindings(env);
            res = sexp_eval(ctx, obj, env);
            sexp_warn_undefs(ctx, sexp_env_bindings(env), tmp, res);
            if (res && sexp_exceptionp(res)) {
                sexp_print_exception(ctx, res, err);
                if (res != sexp_global(ctx, SEXP_G_OOS_ERROR))
                    sexp_stack_trace(ctx, err);
            } else if (res != SEXP_VOID) {
                sexp_write(ctx, res, out);
                sexp_write_char(ctx, '\n', out);
            }
        }
    }
    sexp_gc_release6(ctx);
}


