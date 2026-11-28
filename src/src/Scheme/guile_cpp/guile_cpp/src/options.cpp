#include "guile_cpp/vm.hpp"

namespace guile {

void VM::set_program_arguments(int argc, char** argv) {
    SCM lst = SCM_EOL;
    for (int i = argc - 1; i >= 0; --i) {
        lst = cons(make_string(argv[i] ? argv[i] : ""), lst);
    }
    program_args_ = lst;
}

void VM::set_program_arguments_list(SCM args) {
    program_args_ = args;
}

static SCM options_interface_read(VM& v, const SCM* a, size_t nargs) {
    auto get_list = [&]() -> SCM {
        SCM r = SCM_EOL;
        if (v.read_opts.copy_source) r = v.cons(v.make_symbol("copy"), r);
        if (v.read_opts.positions) r = v.cons(v.make_symbol("positions"), r);
        if (v.read_opts.case_insensitive) r = v.cons(v.make_symbol("case-insensitive"), r);
        r = v.cons(v.make_symbol("keywords"), v.cons(v.read_opts.keywords, r));
        if (v.read_opts.r6rs_hex_escapes) r = v.cons(v.make_symbol("r6rs-hex-escapes"), r);
        return r;
    };
    if (nargs == 0 || a[0].is_undefined() || a[0].is_false()) return get_list();
    if (a[0] == SCM_BOOL_T) {
        // Full list with descriptions
        return v.list4(
            v.list3(v.make_symbol("copy"), SCM::from_bool(v.read_opts.copy_source), v.make_string("Copy source code expressions.")),
            v.list3(v.make_symbol("positions"), SCM::from_bool(v.read_opts.positions), v.make_string("Record positions of source code expressions.")),
            v.list3(v.make_symbol("case-insensitive"), SCM::from_bool(v.read_opts.case_insensitive), v.make_string("Convert symbols to lower case.")),
            v.list3(v.make_symbol("keywords"), v.read_opts.keywords, v.make_string("Style of keyword recognition: #f, 'prefix or 'postfix.")));
    }
    if (is_pair(a[0]) || a[0].is_null()) {
        v.read_opts.copy_source = false;
        v.read_opts.positions = false;
        v.read_opts.case_insensitive = false;
        v.read_opts.keywords = SCM_BOOL_F;
        v.read_opts.r6rs_hex_escapes = false;
        SCM cur = a[0];
        while (is_pair(cur)) {
            SCM k = car(cur);
            cur = cdr(cur);
            if (k == v.make_symbol("copy")) v.read_opts.copy_source = true;
            else if (k == v.make_symbol("positions")) v.read_opts.positions = true;
            else if (k == v.make_symbol("case-insensitive")) v.read_opts.case_insensitive = true;
            else if (k == v.make_symbol("r6rs-hex-escapes")) v.read_opts.r6rs_hex_escapes = true;
            else if (k == v.make_symbol("keywords")) {
                if (is_pair(cur)) { v.read_opts.keywords = car(cur); cur = cdr(cur); }
            }
        }
    }
    return get_list();
}

static SCM options_interface_print(VM& v, const SCM* a, size_t nargs) {
    auto get_list = [&]() -> SCM {
        SCM r = SCM_EOL;
        if (v.print_opts.closures) r = v.cons(v.make_symbol("closures"), r);
        if (v.print_opts.source) r = v.cons(v.make_symbol("source"), r);
        if (v.print_opts.highlight_objects) r = v.cons(v.make_symbol("highlight-objects"), r);
        if (v.print_opts.quote_keywordish_symbols) r = v.cons(v.make_symbol("quote-keywordish-symbols"), r);
        if (v.print_opts.escape_newlines) r = v.cons(v.make_symbol("escape-newlines"), r);
        return r;
    };
    if (nargs == 0 || a[0].is_undefined() || a[0].is_false()) return get_list();
    if (a[0] == SCM_BOOL_T) {
        return v.list2(
            v.list3(v.make_symbol("closures"), SCM::from_bool(v.print_opts.closures), v.make_string("Print closures.")),
            v.list3(v.make_symbol("source"), SCM::from_bool(v.print_opts.source), v.make_string("Print closure source.")));
    }
    if (is_pair(a[0]) || a[0].is_null()) {
        v.print_opts.closures = false;
        v.print_opts.source = false;
        v.print_opts.highlight_objects = false;
        v.print_opts.quote_keywordish_symbols = false;
        v.print_opts.escape_newlines = false;
        SCM cur = a[0];
        while (is_pair(cur)) {
            SCM k = car(cur); cur = cdr(cur);
            if (k == v.make_symbol("closures")) v.print_opts.closures = true;
            else if (k == v.make_symbol("source")) v.print_opts.source = true;
            else if (k == v.make_symbol("highlight-objects")) v.print_opts.highlight_objects = true;
            else if (k == v.make_symbol("quote-keywordish-symbols")) v.print_opts.quote_keywordish_symbols = true;
            else if (k == v.make_symbol("escape-newlines")) v.print_opts.escape_newlines = true;
        }
    }
    return get_list();
}

static SCM options_interface_eval(VM& v, const SCM* a, size_t nargs) {
    auto get_list = [&]() -> SCM {
        return v.list2(v.make_symbol("stack"), v.make_integer(v.eval_opts.stack_limit));
    };
    if (nargs == 0 || a[0].is_undefined() || a[0].is_false()) return get_list();
    if (a[0] == SCM_BOOL_T) {
        return v.list1(v.list3(v.make_symbol("stack"), v.make_integer(v.eval_opts.stack_limit), v.make_string("Size of stack.")));
    }
    if (is_pair(a[0])) {
        SCM cur = a[0];
        while (is_pair(cur)) {
            if (car(cur) == v.make_symbol("stack") && is_pair(cdr(cur))) {
                v.eval_opts.stack_limit = static_cast<int>(scm_to_int64(car(cdr(cur)), "eval-options-interface", 1));
                cur = cdr(cdr(cur));
            } else {
                cur = cdr(cur);
            }
        }
    }
    return get_list();
}

static SCM options_interface_debug(VM& v, const SCM* a, size_t nargs) {
    auto get_list = [&]() -> SCM {
        SCM r = SCM_EOL;
        if (v.debug_opts.debug) r = v.cons(v.make_symbol("debug"), r);
        if (v.debug_opts.backtrace) r = v.cons(v.make_symbol("backtrace"), r);
        if (v.debug_opts.backwards) r = v.cons(v.make_symbol("backwards"), r);
        if (v.debug_opts.show_file_name) r = v.cons(v.make_symbol("show-file-name"), r);
        if (v.debug_opts.procnames) r = v.cons(v.make_symbol("procnames"), r);
        if (v.debug_opts.cheap) r = v.cons(v.make_symbol("cheap"), r);
        if (v.debug_opts.trace) r = v.cons(v.make_symbol("trace"), r);
        r = v.cons(v.make_symbol("depth"), v.cons(v.make_integer(v.debug_opts.depth), r));
        r = v.cons(v.make_symbol("maxdepth"), v.cons(v.make_integer(v.debug_opts.maxdepth), r));
        r = v.cons(v.make_symbol("frames"), v.cons(v.make_integer(v.debug_opts.frames), r));
        r = v.cons(v.make_symbol("indent"), v.cons(v.make_integer(v.debug_opts.indent), r));
        r = v.cons(v.make_symbol("width"), v.cons(v.make_integer(v.debug_opts.width), r));
        r = v.cons(v.make_symbol("stack"), v.cons(v.make_uinteger(v.debug_opts.stack), r));
        return r;
    };
    if (nargs == 0 || a[0].is_undefined() || a[0].is_false()) return get_list();
    if (a[0] == SCM_BOOL_T) {
        return v.list3(
            v.list3(v.make_symbol("debug"), SCM::from_bool(v.debug_opts.debug), v.make_string("Use the debugging evaluator.")),
            v.list3(v.make_symbol("backtrace"), SCM::from_bool(v.debug_opts.backtrace), v.make_string("Show backtrace on error.")),
            v.list3(v.make_symbol("depth"), v.make_integer(v.debug_opts.depth), v.make_string("Maximal length of printed backtrace.")));
    }
    if (is_pair(a[0]) || a[0].is_null()) {
        v.debug_opts.debug = false;
        v.debug_opts.backtrace = false;
        v.debug_opts.backwards = false;
        v.debug_opts.show_file_name = false;
        v.debug_opts.procnames = false;
        v.debug_opts.cheap = false;
        v.debug_opts.trace = false;
        SCM cur = a[0];
        while (is_pair(cur)) {
            SCM k = car(cur); cur = cdr(cur);
            if (k == v.make_symbol("debug")) v.debug_opts.debug = true;
            else if (k == v.make_symbol("backtrace")) v.debug_opts.backtrace = true;
            else if (k == v.make_symbol("backwards")) v.debug_opts.backwards = true;
            else if (k == v.make_symbol("show-file-name")) v.debug_opts.show_file_name = true;
            else if (k == v.make_symbol("procnames")) v.debug_opts.procnames = true;
            else if (k == v.make_symbol("cheap")) v.debug_opts.cheap = true;
            else if (k == v.make_symbol("trace")) v.debug_opts.trace = true;
            else if (k == v.make_symbol("depth") && is_pair(cur)) { v.debug_opts.depth = static_cast<int>(scm_to_int64(car(cur), "debug-options", 1)); cur = cdr(cur); }
            else if (k == v.make_symbol("maxdepth") && is_pair(cur)) { v.debug_opts.maxdepth = static_cast<int>(scm_to_int64(car(cur), "debug-options", 1)); cur = cdr(cur); }
            else if (k == v.make_symbol("frames") && is_pair(cur)) { v.debug_opts.frames = static_cast<int>(scm_to_int64(car(cur), "debug-options", 1)); cur = cdr(cur); }
            else if (k == v.make_symbol("indent") && is_pair(cur)) { v.debug_opts.indent = static_cast<int>(scm_to_int64(car(cur), "debug-options", 1)); cur = cdr(cur); }
            else if (k == v.make_symbol("width") && is_pair(cur)) { v.debug_opts.width = static_cast<int>(scm_to_int64(car(cur), "debug-options", 1)); cur = cdr(cur); }
            else if (k == v.make_symbol("stack") && is_pair(cur)) { int64_t s = scm_to_int64(car(cur), "debug-options", 1); v.debug_opts.stack = s > 0 ? static_cast<size_t>(s) : 0; cur = cdr(cur); }
        }
    }
    return get_list();
}

void VM::init_options() {
    // Version primitives
    define_proc("version", 0, 0, false, [this](const SCM*, size_t) -> SCM { return make_string("1.8.8"); });
    define_proc("major-version", 0, 0, false, [this](const SCM*, size_t) -> SCM { return make_string("1"); });
    define_proc("minor-version", 0, 0, false, [this](const SCM*, size_t) -> SCM { return make_string("8"); });
    define_proc("micro-version", 0, 0, false, [this](const SCM*, size_t) -> SCM { return make_string("8"); });
    define_proc("effective-version", 0, 0, false, [this](const SCM*, size_t) -> SCM { return make_string("1.8"); });

    // Program arguments
    auto prog_args_fn = [this](const SCM*, size_t) -> SCM { return program_args_; };
    define_proc("program-arguments", 0, 0, false, prog_args_fn);
    define_proc("command-line", 0, 0, false, prog_args_fn);
    define_proc("set-program-arguments", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        program_args_ = a[0];
        return SCM_UNSPECIFIED;
    });

    // Features
    SCM feats = SCM_EOL;
    for (const char* f : {"guile", "r5rs", "inexact", "complex", "delay", "posix", "regex", "socket", "net-db", "random", "array", "sort", "values", "debug-extensions", "current-time", "srfi-4", "srfi-13", "srfi-14", "srfi-61"}) {
        feats = cons(make_symbol(f), feats);
    }
    features_var = define("*features*", feats);
    define_proc("add-feature!", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM cur = as_obj<VariableObj>(features_var)->value;
        if (scm_memq(a[0], cur).is_false()) {
            as_obj<VariableObj>(features_var)->value = cons(a[0], cur);
        }
        return SCM_UNSPECIFIED;
    });

    // Options interfaces
    define_proc("read-options-interface", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        return options_interface_read(*this, a, nargs);
    });
    define_proc("print-options-interface", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        return options_interface_print(*this, a, nargs);
    });
    define_proc("eval-options-interface", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        return options_interface_eval(*this, a, nargs);
    });
    define_proc("debug-options-interface", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        return options_interface_debug(*this, a, nargs);
    });
    define_proc("evaluator-traps-interface", 0, 1, false, [](const SCM*, size_t) -> SCM {
        return SCM_EOL;
    });

    // Deprecation
    define_proc("include-deprecated-features", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return SCM::from_bool(include_deprecated);
    });
    define_proc("issue-deprecation-warning", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (warn_deprecated) {
            SCM lst = nargs > 0 ? a[0] : SCM_EOL;
            SCM errp = current_error_port();
            while (is_pair(lst)) {
                write_to_port(car(lst), errp, true);
                lst = cdr(lst);
            }
            port_putc(as_obj<PortObj>(errp), '\n');
        }
        return SCM_UNSPECIFIED;
    });

    // GC primitives
    define_proc("gc", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        heap_.collect();
        return SCM_UNSPECIFIED;
    });
    define_proc("gc-stats", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        SCM r = SCM_EOL;
        r = cons(cons(make_symbol("gc-time-taken"), make_integer(static_cast<int64_t>(heap_.gc_time_sec() * 1000000.0))), r);
        r = cons(cons(make_symbol("cells-allocated"), make_uinteger(heap_.live_count())), r);
        r = cons(cons(make_symbol("heap-size"), make_uinteger(heap_.gc_threshold())), r);
        r = cons(cons(make_symbol("gc-times"), make_uinteger(heap_.gc_times())), r);
        r = cons(cons(make_symbol("total-cells-allocated"), make_uinteger(heap_.total_allocated())), r);
        return r;
    });
    define_proc("gc-live-object-stats", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        hashmap<string, int> counts(0);
        for (Obj* cur = heap_.all_objects(); cur; cur = cur->gc_next) {
            const char* nm = "object";
            switch (cur->tag) {
            case ObjTag::Pair: nm = "pair"; break;
            case ObjTag::Symbol: nm = "symbol"; break;
            case ObjTag::String: nm = "string"; break;
            case ObjTag::Vector: nm = "vector"; break;
            case ObjTag::Closure: nm = "closure"; break;
            case ObjTag::Subr: nm = "subr"; break;
            case ObjTag::Struct: nm = "struct"; break;
            case ObjTag::HashTable: nm = "hashtable"; break;
            case ObjTag::Port: nm = "port"; break;
            case ObjTag::Variable: nm = "variable"; break;
            case ObjTag::EvalClosure: nm = "eval-closure"; break;
            default: nm = "other"; break;
            }
            counts(nm)++;
        }
        if (heap_.pair_live() > 0) counts("pair") += static_cast<int>(heap_.pair_live());
        if (heap_.closure_live() > 0) counts("closure") += static_cast<int>(heap_.closure_live());
        if (heap_.var_live() > 0) counts("variable") += static_cast<int>(heap_.var_live());
        if (heap_.eval_closure_live() > 0) counts("eval-closure") += static_cast<int>(heap_.eval_closure_live());
        SCM r = SCM_EOL;
        iterator<string> it = iterate(counts);
        while (it->busy()) {
            string k = it->next();
            r = cons(cons(make_string(string(&k[0], N(k))), make_uinteger(counts[k])), r);
        }
        return r;
    });
    define_proc("gc-set-debug-check-freelist!", 1, 0, false, [](const SCM*, size_t) -> SCM {
        return SCM_UNSPECIFIED;
    });

    // Built-in noop / misc
    define_proc("noop", 0, 0, true, [](const SCM* a, size_t nargs) -> SCM {
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        return is_pair(lst) ? car(lst) : SCM_BOOL_F;
    });
    define_proc("quit", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM arg = (nargs >= 1 && !a[0].is_undefined()) ? list1(a[0]) : SCM_EOL;
        throw_scm(sym_quit, arg);
    });
    define_proc("exit", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM arg = (nargs >= 1 && !a[0].is_undefined()) ? list1(a[0]) : SCM_EOL;
        throw_scm(sym_quit, arg);
    });
    define_proc("primitive-exit", 0, 1, false, [](const SCM* a, size_t nargs) -> SCM {
        int code = 0;
        if (nargs >= 1 && !a[0].is_undefined()) {
            if (a[0].is_false()) code = 1;
            else if (a[0] == SCM_BOOL_T) code = 0;
            else if (is_exact_integer(a[0])) code = static_cast<int>(scm_to_int64(a[0], "primitive-exit", 1));
        }
        flush_all_ports();
        std::_Exit(code);
    });
}

} // namespace guile
