#include "guile_cpp/vm.hpp"

namespace guile {

SCM VM::make_symbol(std::string_view name) {
    string key(name.data(), static_cast<int>(name.size()));
    SCM* found = symbol_table->find(key);
    if (found != nullptr) return *found;
    uint64_t h = scm_hash_string(name);
    SCM sym = heap_.alloc_scm<SymbolObj>(std::string(name), h, true);
    symbol_table(key) = sym;
    return sym;
}

SCM VM::make_uninterned_symbol(std::string_view name) {
    uint64_t h = scm_hash_string(name);
    return heap_.alloc_scm<SymbolObj>(std::string(name), h, false);
}

SCM VM::make_keyword(SCM sym) {
    if (!is_symbol(sym)) wrong_type_arg("symbol->keyword", 1, sym, "symbol");
    auto* so = as_obj<SymbolObj>(sym);
    if (so->keyword.is_false()) {
        so->keyword = heap_.alloc_scm<KeywordObj>(sym);
    }
    return so->keyword;
}

SCM VM::make_keyword(std::string_view name) {
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
        return make_uninterned_symbol(as_obj<StringObj>(args[0])->view());
    });

    define_proc("symbol->string", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_symbol(args[0])) wrong_type_arg("symbol->string", 1, args[0], "symbol");
        return make_string(as_obj<SymbolObj>(args[0])->name, true);
    });

    define_proc("string->symbol", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("string->symbol", 1, args[0], "string");
        return make_symbol(as_obj<StringObj>(args[0])->view());
    });

    define_proc("string-ci->symbol", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("string-ci->symbol", 1, args[0], "string");
        std::string s = as_obj<StringObj>(args[0])->to_std_string();
        if (read_opts.case_insensitive) {
            for (char& c : s) c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
        }
        return make_symbol(s);
    });

    define_proc("symbol-hash", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_symbol(args[0])) wrong_type_arg("symbol-hash", 1, args[0], "symbol");
        return make_uinteger(as_obj<SymbolObj>(args[0])->hash);
    });

    define_proc("gensym", 0, 1, false, [this](const SCM* args, size_t nargs) -> SCM {
        std::string prefix = " g";
        if (nargs >= 1 && !args[0].is_undefined()) {
            if (!is_string(args[0])) wrong_type_arg("gensym", 1, args[0], "string");
            prefix = as_obj<StringObj>(args[0])->to_std_string();
        }
        std::string name = prefix + std::to_string(++gensym_counter);
        return make_symbol(name);
    });

    define_proc("symbol-append", 0, 0, true, [this](const SCM* args, size_t nargs) -> SCM {
        std::string res;
        SCM lst = nargs > 0 ? args[0] : SCM_EOL;
        while (is_pair(lst)) {
            SCM s = car(lst);
            if (!is_symbol(s)) wrong_type_arg("symbol-append", 1, s, "symbol");
            res += as_obj<SymbolObj>(s)->name;
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
        const std::string& nm = as_obj<SymbolObj>(args[0])->name;
        if (nm.empty() || nm[0] != '-') {
            misc_error("make-keyword-from-dash-symbol", "symbol does not start with '-': ~S", list1(args[0]));
        }
        return make_keyword(make_symbol(nm.substr(1)));
    });
}

} // namespace guile
