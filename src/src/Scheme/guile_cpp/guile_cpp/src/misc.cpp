#include "guile_cpp/vm.hpp"
#include <clocale>
#include <cstring>
#include <dlfcn.h>

namespace guile {

// the message of dlerror, read once (a second call returns NULL)
static const char* dl_error_message(const char* dflt) {
    const char* e = dlerror();
    return e ? e : dflt;
}

void VM::register_extension(string lib, string init_fn, void (*fn)()) {
    string key = lib * ":" * init_fn;
    extensions(string(gcpp_data(key), static_cast<int>(static_cast<size_t>(N(key))))) = (pointer) fn;
}

void VM::load_extension(string lib, string init_fn) {
    string key = lib * ":" * init_fn;
    pointer fn = extensions[string(gcpp_data(key), static_cast<int>(static_cast<size_t>(N(key))))];
    if (fn != nullptr) {
        reinterpret_cast<void (*)()>(fn)();
        return;
    }
    // Fallback to dynamic linking via dlopen
    void* h = dlopen(((char*) c_string(lib)), RTLD_LAZY | RTLD_GLOBAL);
    if (!h) {
        misc_error("load-extension", "file: ~S, message: ~S",
                   list2(make_string(lib), make_string(dl_error_message("not found"))));
    }
    void* sym = dlsym(h, ((char*) c_string(init_fn)));
    if (!sym) {
        misc_error("load-extension", "symbol not found: ~S", list1(make_string(init_fn)));
    }
    using InitFn = void(*)();
    reinterpret_cast<InitFn>(sym)();
}

size_t VM::make_smob_type(const SmobTypeInfo& info) {
    (smob_types_ << info);
    return static_cast<size_t>(N(smob_types_)) - 1;
}

void VM::init_misc() {
    // Extensions
    define_proc("load-extension", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("load-extension", 1, a[0], "string");
        if (!is_string(a[1])) wrong_type_arg("load-extension", 2, a[1], "string");
        load_extension(as_obj<StringObj>(a[0])->str(), as_obj<StringObj>(a[1])->str());
        return SCM_UNSPECIFIED;
    });

    // Dynamic linking (dynl.c)
    define_proc("dynamic-link", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        string fn;
        c_string fn_c;  // (kept alive for dlopen)
        const char* p = nullptr;
        if (nargs >= 1 && !a[0].is_undefined() && a[0].is_true()) {
            if (!is_string(a[0])) wrong_type_arg("dynamic-link", 1, a[0], "string");
            fn = as_obj<StringObj>(a[0])->str();
            fn_c = c_string(fn);
            p = fn_c;
        }
        void* h = dlopen(p, RTLD_LAZY | RTLD_GLOBAL);
        if (!h) misc_error("dynamic-link", "file: ~S, message: ~S",
                           list2(nargs >= 1 ? a[0] : SCM_BOOL_F, make_string(dl_error_message("dlopen failed"))));
        return heap_.alloc_scm<DynamicLibObj>(h, fn);
    });
    define_proc("dynamic-object?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_a<DynamicLibObj>(a[0], ObjTag::DynamicLib));
    });
    define_proc("dynamic-unlink", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_a<DynamicLibObj>(a[0], ObjTag::DynamicLib)) wrong_type_arg("dynamic-unlink", 1, a[0], "dynamic-object");
        auto* d = as_obj<DynamicLibObj>(a[0]);
        if (d->handle) { dlclose(d->handle); d->handle = nullptr; }
        return SCM_UNSPECIFIED;
    });

    // i18n (i18n.c)
    define("LC_ALL",      make_integer(LC_ALL));
    define("LC_COLLATE",  make_integer(LC_COLLATE));
    define("LC_CTYPE",    make_integer(LC_CTYPE));
    define("LC_MONETARY", make_integer(LC_MONETARY));
    define("LC_NUMERIC",  make_integer(LC_NUMERIC));
    define("LC_TIME",     make_integer(LC_TIME));
    define_proc("setlocale", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        int cat = static_cast<int>(scm_to_int64(a[0], "setlocale", 1));
        const char* loc = nullptr;
        c_string loc_c;  // (kept alive for setlocale)
        if (nargs >= 2 && !a[1].is_undefined()) {
            if (!is_string(a[1])) wrong_type_arg("setlocale", 2, a[1], "string");
            loc_c = c_string(as_obj<StringObj>(a[1])->str());
            loc = loc_c;
        }
        const char* r = std::setlocale(cat, loc);
        if (!r) misc_error("setlocale", "invalid locale");
        return make_string(r);
    });

    // Asyncs (async.c)
    define_proc("async", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return heap_.alloc_scm<AsyncObj>(a[0]);
    });
    define_proc("system-async", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return heap_.alloc_scm<AsyncObj>(a[0]);
    });
    define_proc("async-mark", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_a<AsyncObj>(a[0], ObjTag::Async)) wrong_type_arg("async-mark", 1, a[0], "async");
        as_obj<AsyncObj>(a[0])->marked = true;
        return SCM_UNSPECIFIED;
    });
    define_proc("system-async-mark", 1, 1, false, [](const SCM* a, size_t) -> SCM {
        if (is_a<AsyncObj>(a[0], ObjTag::Async)) as_obj<AsyncObj>(a[0])->marked = true;
        return SCM_UNSPECIFIED;
    });
    define_proc("run-asyncs", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM cur = a[0];
        while (is_pair(cur)) {
            if (is_a<AsyncObj>(car(cur), ObjTag::Async)) {
                auto* as = as_obj<AsyncObj>(car(cur));
                if (as->marked) { as->marked = false; call0(as->thunk); }
            }
            cur = cdr(cur);
        }
        return SCM_UNSPECIFIED;
    });
    define_proc("call-with-blocked-asyncs", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return call0(a[0]);
    });
    define_proc("call-with-unblocked-asyncs", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return call0(a[0]);
    });
    define_proc("noop", 0, 0, true, [](const SCM* a, size_t nargs) -> SCM {
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        return is_pair(lst) ? car(lst) : SCM_BOOL_F;
    });

    // Threads & Mutexes (single-threaded / null-threads compatible core)
    SCM main_thread = heap_.alloc_scm<ThreadObj>();
    heap_.add_permanent(main_thread);
    define_proc("current-thread", 0, 0, false, [main_thread](const SCM*, size_t) -> SCM { return main_thread; });
    define_proc("all-threads", 0, 0, false, [this, main_thread](const SCM*, size_t) -> SCM { return list1(main_thread); });
    define_proc("thread?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_a<ThreadObj>(a[0], ObjTag::Thread)); });
    define_proc("thread-exited?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_a<ThreadObj>(a[0], ObjTag::Thread)) wrong_type_arg("thread-exited?", 1, a[0], "thread");
        return SCM::from_bool(as_obj<ThreadObj>(a[0])->exited);
    });
    define_proc("yield", 0, 0, false, [](const SCM*, size_t) -> SCM { return SCM_BOOL_T; });
    define_proc("make-mutex", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        bool rec = false;
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        while (is_pair(lst)) {
            if (car(lst) == make_symbol("recursive")) rec = true;
            lst = cdr(lst);
        }
        return heap_.alloc_scm<MutexObj>(rec);
    });
    define_proc("make-recursive-mutex", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return heap_.alloc_scm<MutexObj>(true);
    });
    define_proc("mutex?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_a<MutexObj>(a[0], ObjTag::Mutex)); });
    define_proc("lock-mutex", 1, 1, false, [this, main_thread](const SCM* a, size_t) -> SCM {
        if (!is_a<MutexObj>(a[0], ObjTag::Mutex)) wrong_type_arg("lock-mutex", 1, a[0], "mutex");
        auto* m = as_obj<MutexObj>(a[0]);
        if (m->locked && !m->recursive) misc_error("lock-mutex", "mutex already locked by thread");
        m->locked = true; m->level++; m->owner = main_thread;
        return SCM_BOOL_T;
    });
    define_proc("try-mutex", 1, 0, false, [this, main_thread](const SCM* a, size_t) -> SCM {
        if (!is_a<MutexObj>(a[0], ObjTag::Mutex)) wrong_type_arg("try-mutex", 1, a[0], "mutex");
        auto* m = as_obj<MutexObj>(a[0]);
        if (m->locked && !m->recursive) return SCM_BOOL_F;
        m->locked = true; m->level++; m->owner = main_thread;
        return SCM_BOOL_T;
    });
    define_proc("unlock-mutex", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_a<MutexObj>(a[0], ObjTag::Mutex)) wrong_type_arg("unlock-mutex", 1, a[0], "mutex");
        auto* m = as_obj<MutexObj>(a[0]);
        if (!m->locked) misc_error("unlock-mutex", "mutex not locked");
        if (--m->level <= 0) { m->locked = false; m->level = 0; m->owner = SCM_BOOL_F; }
        return SCM_BOOL_T;
    });
    define_proc("mutex-owner", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_a<MutexObj>(a[0], ObjTag::Mutex)) wrong_type_arg("mutex-owner", 1, a[0], "mutex");
        return as_obj<MutexObj>(a[0])->owner;
    });
    define_proc("mutex-level", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_a<MutexObj>(a[0], ObjTag::Mutex)) wrong_type_arg("mutex-level", 1, a[0], "mutex");
        return make_integer(as_obj<MutexObj>(a[0])->level);
    });
    define_proc("mutex-locked?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_a<MutexObj>(a[0], ObjTag::Mutex)) wrong_type_arg("mutex-locked?", 1, a[0], "mutex");
        return SCM::from_bool(as_obj<MutexObj>(a[0])->locked);
    });
    define_proc("make-condition-variable", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return heap_.alloc_scm<CondVarObj>();
    });
    define_proc("condition-variable?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_a<CondVarObj>(a[0], ObjTag::CondVar));
    });
    define_proc("signal-condition-variable", 1, 0, false, [](const SCM*, size_t) -> SCM { return SCM_UNSPECIFIED; });
    define_proc("broadcast-condition-variable", 1, 0, false, [](const SCM*, size_t) -> SCM { return SCM_UNSPECIFIED; });

    // Misc Guile 1.8 helpers used by boot-9 / scripts
    define_proc("unmemoize", 1, 0, false, [](const SCM* a, size_t) -> SCM { return a[0]; });
    define_proc("memoized?", 1, 0, false, [](const SCM*, size_t) -> SCM { return SCM_BOOL_F; });
    define_proc("local-ref", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        // Used in boot-9.scm macroexpand-1: (local-ref (list sym))
        SCM sym = is_pair(a[0]) ? car(a[0]) : a[0];
        SCM var = lookup_sym(sym);
        if (is_variable(var) && !as_obj<VariableObj>(var)->value.is_undefined()) {
            return as_obj<VariableObj>(var)->value;
        }
        return SCM_BOOL_F;
    });
    define_proc("c-registered-modules", 0, 0, false, [](const SCM*, size_t) -> SCM { return SCM_EOL; });
    define_proc("c-clear-registered-modules", 0, 0, false, [](const SCM*, size_t) -> SCM { return SCM_UNSPECIFIED; });
    define_proc("builtin-bindings", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return pre_modules_obarray;
    });

    // Attach standard Guile 1.8.8 procedure docstrings
    auto add_doc = [this](const char* name, const char* doc) {
        SCM var = lookup(name);
        if (is_variable(var) && !as_obj<VariableObj>(var)->value.is_undefined()) {
            SCM proc = as_obj<VariableObj>(var)->value;
            if (is_procedure(proc)) {
                set_procedure_property(proc, sym_documentation, make_string(doc));
            }
        }
    };
    add_doc("bit-extract", "Return the integer composed of the START (inclusive) through END (exclusive) bits of N.");
    add_doc("make-guardian", "Create a new guardian procedure.");
    add_doc("symbol?", "Return #t if OBJ is a symbol, otherwise return #f.");
    add_doc("gensym", "Create a new symbol with a name constructed from a prefix and a counter value.");
    add_doc("hash", "Return a hash value for OBJ in the range [0, SIZE).");
    add_doc("hashv", "Return a hash value for OBJ using eqv? in the range [0, SIZE).");
    add_doc("hashq", "Return a hash value for OBJ using eq? in the range [0, SIZE).");
    add_doc("hashx-remove!", "Remove the entry for KEY from TABLE using HASH and ASSOC.");
    add_doc("list", "Return a list containing the arguments.");
    add_doc("make-list", "Create a list containing N elements, optionally initialized to INIT.");
    add_doc("cons*", "Like list, but the last argument provides the tail of the constructed list.");
    add_doc("append!", "Destructively append lists.");
    add_doc("list-ref", "Return the K-th element from LIST.");
    add_doc("list-set!", "Set the K-th element of LIST to VAL.");
    add_doc("list-cdr-set!", "Set the K-th cdr of LIST to VAL.");
    add_doc("1+", "Return N + 1.");
    add_doc("1-", "Return N - 1.");
    add_doc("ash", "Return N arithmetically shifted left by CNT bits.");
    add_doc("exact?", "Return #t if X is an exact number.");
    add_doc("inexact?", "Return #t if X is an inexact number.");
    add_doc("exp", "Return e raised to the power Z.");
    add_doc("log", "Return the natural logarithm of Z.");
    add_doc("log10", "Return the base-10 logarithm of Z.");
    add_doc("sqrt", "Return the square root of Z.");
    add_doc("odd?", "Return #t if N is odd.");
    add_doc("even?", "Return #t if N is even.");
    add_doc("inf?", "Return #t if X is positive or negative infinity.");
    add_doc("nan?", "Return #t if X is NaN.");
    add_doc("abs", "Return the absolute value of X.");
    add_doc("number->string", "Convert number N to a string in optional RADIX.");
    add_doc("string->number", "Parse string S as a number in optional RADIX.");
    add_doc("number?", "Return #t if X is a number.");
    add_doc("complex?", "Return #t if X is a complex number.");
    add_doc("real?", "Return #t if X is a real number.");
    add_doc("rational?", "Return #t if X is a rational number.");
    add_doc("integer?", "Return #t if X is an integer.");
    add_doc("equal?", "Return #t if X and Y are recursively equal.");
}

} // namespace guile
