#include "guile_cpp/vm.hpp"

namespace guile {

// (TeXmacs: the lookup shares the name, without allocation; only a new
// symbol copies it, as the key of the table and its name, never changed)
SCM VM::make_symbol(string name) {
    SCM* found = symbol_table->find(name);
    if (found != nullptr) return *found;
    string key = copy(name);
    SCM sym = heap_.alloc_scm<SymbolObj>(key, scm_hash_string(key), true);
    symbol_table(key) = sym;
    interned_symbols << sym;
    return sym;
}

SCM VM::make_uninterned_symbol(string name) {
    string key = copy(name);
    return heap_.alloc_scm<SymbolObj>(key, scm_hash_string(key), false);
}

SCM VM::make_keyword(SCM sym) {
    if (!is_symbol(sym)) wrong_type_arg("symbol->keyword", 1, sym, "symbol");
    auto* so = as_obj<SymbolObj>(sym);
    if (so->keyword.is_false()) {
        so->keyword = heap_.alloc_scm<KeywordObj>(sym);
    }
    return so->keyword;
}

SCM VM::make_keyword(string name) {
    return make_keyword(make_symbol(name));
}

void VM::init_symbols() {
    define_proc("symbol?", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return SCM::from_bool(is_symbol(args[0]));
    });

    define_proc("symbol-interned?", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_symbol(args[0])) wrong_type_arg("symbol-interned?", 1, args[0], "symbol");
        return SCM::from_bool(as_obj<SymbolObj>(args[0])->interned);
    });

    define_proc("make-symbol", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("make-symbol", 1, args[0], "string");
        return make_uninterned_symbol(as_obj<StringObj>(args[0])->str());
    });

    define_proc("symbol->string", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_symbol(args[0])) wrong_type_arg("symbol->string", 1, args[0], "symbol");
        return make_string(as_obj<SymbolObj>(args[0])->name, true);
    });

    define_proc("string->symbol", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("string->symbol", 1, args[0], "string");
        return make_symbol(as_obj<StringObj>(args[0])->str());
    });

    define_proc("string-ci->symbol", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("string-ci->symbol", 1, args[0], "string");
        string s = as_obj<StringObj>(args[0])->str();  // (a copy, changed)
        if (read_opts.case_insensitive) {
            for (int i = 0; i < N(s); i++) s[i] = static_cast<char>(std::tolower(static_cast<unsigned char>(s[i])));
        }
        return make_symbol(s);
    });

    define_proc("symbol-hash", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_symbol(args[0])) wrong_type_arg("symbol-hash", 1, args[0], "symbol");
        return make_uinteger(as_obj<SymbolObj>(args[0])->hash);
    });

    define_proc("gensym", 0, 1, false, [this](const SCM* args, size_t nargs) -> SCM {
        string prefix = " g";
        if (nargs >= 1 && !args[0].is_undefined()) {
            if (!is_string(args[0])) wrong_type_arg("gensym", 1, args[0], "string");
            prefix = as_obj<StringObj>(args[0])->str();
        }
        string name = prefix * as_string(static_cast<long long>(++gensym_counter));
        return make_symbol(name);
    });

    define_proc("symbol-append", 0, 0, true, [this](const SCM* args, size_t nargs) -> SCM {
        string res;  // (fresh)
        SCM lst = nargs > 0 ? args[0] : SCM_EOL;
        while (is_pair(lst)) {
            SCM s = car(lst);
            if (!is_symbol(s)) wrong_type_arg("symbol-append", 1, s, "symbol");
            res << as_obj<SymbolObj>(s)->name;
            lst = cdr(lst);
        }
        return make_symbol(res);
    });

    // Deprecated Guile symbol property slots (used by boot-9!)
    define_proc("symbol-fref", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_symbol(args[0])) wrong_type_arg("symbol-fref", 1, args[0], "symbol");
        return as_obj<SymbolObj>(args[0])->fref;
    });
    define_proc("symbol-pref", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_symbol(args[0])) wrong_type_arg("symbol-pref", 1, args[0], "symbol");
        return as_obj<SymbolObj>(args[0])->pref;
    });
    define_proc("symbol-fset!", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_symbol(args[0])) wrong_type_arg("symbol-fset!", 1, args[0], "symbol");
        as_obj<SymbolObj>(args[0])->fref = args[1];
        return SCM_UNSPECIFIED;
    });
    define_proc("symbol-pset!", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_symbol(args[0])) wrong_type_arg("symbol-pset!", 1, args[0], "symbol");
        as_obj<SymbolObj>(args[0])->pref = args[1];
        return SCM_UNSPECIFIED;
    });

    // Keywords
    define_proc("keyword?", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return SCM::from_bool(is_keyword(args[0]));
    });
    define_proc("symbol->keyword", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        return make_keyword(args[0]);
    });
    define_proc("keyword->symbol", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_keyword(args[0])) wrong_type_arg("keyword->symbol", 1, args[0], "keyword");
        return as_obj<KeywordObj>(args[0])->symbol;
    });
    define_proc("make-keyword-from-dash-symbol", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_symbol(args[0])) wrong_type_arg("make-keyword-from-dash-symbol", 1, args[0], "symbol");
        string nm = as_obj<SymbolObj>(args[0])->name;
        if (N(nm) == 0 || nm[0] != '-') {
            misc_error("make-keyword-from-dash-symbol", "symbol does not start with '-': ~S", list1(args[0]));
        }
        return make_keyword(make_symbol(str_sub(nm, 1)));
    });
}

} // namespace guile
