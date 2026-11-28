#include "guile_cpp/vm.hpp"

namespace guile {

// Module struct field indices (must match module-type in ice-9/boot-9.scm!)
static constexpr size_t MODULE_INDEX_OBARRAY      = 0;
static constexpr size_t MODULE_INDEX_USES         = 1;
static constexpr size_t MODULE_INDEX_BINDER       = 2;
static constexpr size_t MODULE_INDEX_EVAL_CLOSURE = 3;
static constexpr size_t MODULE_INDEX_TRANSFORMER  = 4;

SCM VM::make_variable(SCM val) {
    return heap_.alloc_scm<VariableObj>(val);
}

bool VM::is_module(SCM obj) const {
    if (!module_system_booted || !is_struct(obj)) return false;
    return as_obj<StructObj>(obj)->vtable == module_type;
}

static SCM mod_field(SCM mod, size_t idx) {
    auto* st = as_obj<StructObj>(mod);
    if (idx >= static_cast<size_t>(N(st->slots))) return SCM_BOOL_F;
    return SCM(st->slots[idx]);
}

void VM::post_boot_init_modules() {
    if (module_system_booted) return;
    auto fetch = [this](const char* name) -> SCM {
        SCM v = hash_ref(pre_modules_obarray, make_symbol(name), SCM_BOOL_F, 1);
        return v;
    };
    SCM mt_var = fetch("module-type");
    if (is_variable(mt_var)) module_type = as_obj<VariableObj>(mt_var)->value;
    resolve_module_var = fetch("resolve-module");
    process_define_module_var = fetch("process-define-module");
    process_use_modules_var = fetch("process-use-modules");
    module_export_var = fetch("module-export!");
    the_root_module_var = fetch("the-root-module");
    module_system_booted = true;
}

SCM VM::current_module() {
    SCM curr = fluid_ref(the_module_fluid);
    if (curr.is_true()) return curr;
    if (module_system_booted && is_variable(the_root_module_var)) {
        return as_obj<VariableObj>(the_root_module_var)->value;
    }
    return SCM_BOOL_F;
}

SCM VM::set_current_module(SCM mod) {
    if (!module_system_booted) post_boot_init_modules();
    if (!is_module(mod)) wrong_type_arg("set-current-module", 1, mod, "module");
    SCM old = current_module();
    fluid_set(the_module_fluid, mod);
    return old;
}

SCM VM::module_lookup_closure(SCM mod) {
    if (mod.is_false() || !is_struct(mod)) return SCM_BOOL_F;
    return mod_field(mod, MODULE_INDEX_EVAL_CLOSURE);
}

SCM VM::current_module_lookup_closure() {
    if (!module_system_booted) return SCM_BOOL_F;
    return module_lookup_closure(current_module());
}

SCM VM::current_module_transformer() {
    if (!module_system_booted) return SCM_BOOL_F;
    SCM mod = current_module();
    if (mod.is_false() || !is_struct(mod)) return SCM_BOOL_F;
    return mod_field(mod, MODULE_INDEX_TRANSFORMER);
}

// Compact open-addressed visited set: tiny 256B stack table (safe for C-stack limit),
// spills to heap open-addressed table only when a module graph exceeds 24 modules.
struct ModVisitedSet {
    static constexpr size_t SMALL_CAP = 32; // power of 2 -> 256 bytes on stack
    uint64_t small_slots[SMALL_CAP];
    size_t count = 0;
    array<uint64_t> big_slots;

    ModVisitedSet() noexcept { std::memset(small_slots, 0, sizeof(small_slots)); }

    bool insert(uint64_t bits) {
        if ((N(big_slots) == 0)) {
            if (count < 24) {
                size_t idx = ((bits >> 4) * 2654435761ULL) & (SMALL_CAP - 1);
                while (true) {
                    uint64_t cur = small_slots[idx];
                    if (cur == bits) return false;
                    if (cur == 0) {
                        small_slots[idx] = bits;
                        ++count;
                        return true;
                    }
                    idx = (idx + 1) & (SMALL_CAP - 1);
                }
            }
            gcpp_assign(big_slots, 512, 0);
            for (size_t i = 0; i < SMALL_CAP; ++i) {
                uint64_t b = small_slots[i];
                if (b != 0) {
                    size_t idx = ((b >> 4) * 2654435761ULL) & (static_cast<size_t>(N(big_slots)) - 1);
                    while (big_slots[idx] != 0) idx = (idx + 1) & (static_cast<size_t>(N(big_slots)) - 1);
                    big_slots[idx] = b;
                }
            }
        }
        if ((count + 1) * 4 >= static_cast<size_t>(N(big_slots)) * 3) {
            array<uint64_t> next = array_fill<uint64_t>(static_cast<size_t>(N(big_slots)) * 2, 0);
            for (uint64_t b : big_slots) {
                if (b != 0) {
                    size_t idx = ((b >> 4) * 2654435761ULL) & (static_cast<size_t>(N(next)) - 1);
                    while (next[idx] != 0) idx = (idx + 1) & (static_cast<size_t>(N(next)) - 1);
                    next[idx] = b;
                }
            }
            big_slots = next;
        }
        size_t mask = static_cast<size_t>(N(big_slots)) - 1;
        size_t idx = ((bits >> 4) * 2654435761ULL) & mask;
        while (true) {
            uint64_t cur = big_slots[idx];
            if (cur == bits) return false;
            if (cur == 0) {
                big_slots[idx] = bits;
                ++count;
                return true;
            }
            idx = (idx + 1) & mask;
        }
    }
};

static SCM module_variable_uncached(VM& v, SCM mod, SCM sym,
                                    ModVisitedSet& visited, bool& saw_binder) {
    if (!is_struct(mod)) return SCM_BOOL_F;
    if (!visited.insert(mod.bits)) return SCM_BOOL_F;

    SCM obarray = mod_field(mod, MODULE_INDEX_OBARRAY);
    if (is_hashtable(obarray)) {
        auto* ht = as_obj<HashTableObj>(obarray);
        if (!ht->is_module_obarray) ht->is_module_obarray = true;
        SCM b = hash_ref(obarray, sym, SCM_BOOL_F, 1);
        if (b.is_true()) return b;
    }
    SCM binder = mod_field(mod, MODULE_INDEX_BINDER);
    if (binder.is_true()) {
        saw_binder = true;
        SCM b = v.call3(binder, mod, sym, SCM_BOOL_F);
        if (b.is_true()) return b;
    }
    SCM uses = mod_field(mod, MODULE_INDEX_USES);
    while (is_pair(uses)) {
        SCM b = module_variable_uncached(v, car(uses), sym, visited, saw_binder);
        if (b.is_true()) return b;
        uses = cdr(uses);
    }
    return SCM_BOOL_F;
}

static inline uint64_t mix_sym_hash(SCM sym) noexcept {
    uint64_t h = is_symbol(sym) ? as_obj<SymbolObj>(sym)->hash
               : is_keyword(sym) ? (as_obj<KeywordObj>(sym)->symbol.bits ^ 0x9e3779b97f4a7c15ULL)
               : sym.bits;
    h ^= (h >> 33);
    h *= 0xff51afd7ed558ccdULL;
    h ^= (h >> 33);
    return h;
}

static inline SCM fast_obarray_ref(HashTableObj* ht, SCM sym, uint64_t mixed_h) noexcept {
    if (ht->count == 0) return SCM_BOOL_F;
    size_t sz = static_cast<size_t>(N(ht->buckets));
    if (sz == 0) return SCM_BOOL_F;
    size_t idx = ((sz & (sz - 1)) == 0) ? static_cast<size_t>(mixed_h & (sz - 1)) : static_cast<size_t>(mixed_h % sz);
    SCM cur = ht->buckets[idx];
    while (is_pair(cur)) {
        SCM entry = car(cur);
        if (is_pair(entry) && car(entry) == sym) return cdr(entry);
        cur = cdr(cur);
    }
    return SCM_BOOL_F;
}

static SCM module_variable_stamped(VM& v, SCM mod, SCM sym, uint64_t mixed_h,
                                   uint32_t stamp, bool& saw_binder) {
    if (!is_struct(mod)) return SCM_BOOL_F;
    auto* st = as_obj<StructObj>(mod);
    if (st->visit_stamp == stamp) return SCM_BOOL_F;
    st->visit_stamp = stamp;

    size_t nslots = static_cast<size_t>(N(st->slots));
    if (nslots > MODULE_INDEX_OBARRAY) {
        SCM obarray(st->slots[MODULE_INDEX_OBARRAY]);
        if (is_hashtable(obarray)) {
            auto* ht = as_obj<HashTableObj>(obarray);
            if (!ht->is_module_obarray) ht->is_module_obarray = true;
            SCM b = fast_obarray_ref(ht, sym, mixed_h);
            if (b.is_true()) return b;
        }
    }
    if (nslots > MODULE_INDEX_BINDER) {
        SCM binder(st->slots[MODULE_INDEX_BINDER]);
        if (binder.is_true()) {
            saw_binder = true;
            SCM res = v.call3(binder, mod, sym, SCM_BOOL_F);
            if (res.is_true()) return res;
        }
    }
    if (nslots > MODULE_INDEX_USES) {
        SCM uses(st->slots[MODULE_INDEX_USES]);
        while (is_pair(uses)) {
            SCM b = module_variable_stamped(v, car(uses), sym, mixed_h, stamp, saw_binder);
            if (b.is_true()) return b;
            uses = cdr(uses);
        }
    }
    return SCM_BOOL_F;
}

SCM VM::module_variable(SCM mod, SCM sym) {
    if (!is_struct(mod)) return SCM_BOOL_F;
    uint32_t sym_ep = is_symbol(sym) ? as_obj<SymbolObj>(sym)->def_epoch : 0;
    size_t cidx = (((mod.bits >> 4) * 2654435761ULL) ^ ((sym.bits >> 4) * 2246822519ULL)) & (MOD_VAR_CACHE_SIZE - 1);
    auto& ce = mod_var_cache[cidx];
    if (GCPP_LIKELY(ce.mod_epoch == module_epoch && ce.gc_epoch == mod_cache_epoch &&
        ce.sym_epoch == sym_ep &&
        ce.mod_bits == mod.bits && ce.sym_bits == sym.bits)) {
        return SCM(ce.var_bits);
    }
    uint64_t ep = module_epoch;
    uint64_t gep = mod_cache_epoch;
    uint64_t mh = mix_sym_hash(sym);
    // Fast-path: check mod's own obarray first before walking uses DAG
    auto* st = as_obj<StructObj>(mod);
    if (!(N(st->slots) == 0)) {
        SCM ob(st->slots[MODULE_INDEX_OBARRAY]);
        if (is_hashtable(ob)) {
            auto* ht = as_obj<HashTableObj>(ob);
            if (!ht->is_module_obarray) ht->is_module_obarray = true;
            SCM local = fast_obarray_ref(ht, sym, mh);
            if (local.is_true()) {
                ce.mod_bits = mod.bits;
                ce.sym_bits = sym.bits;
                ce.var_bits = local.bits;
                ce.mod_epoch = ep;
                ce.gc_epoch = gep;
                ce.sym_epoch = sym_ep;
                return local;
            }
        }
    }
    bool saw_binder = false;
    SCM res = SCM_BOOL_F;
    if (GCPP_LIKELY(mod_search_depth == 0)) {
        uint32_t stamp = ++mod_visit_stamp;
        if (stamp == 0) {
            for (Obj* cur = heap_.all_objects(); cur; cur = cur->gc_next) {
                if (cur->tag == ObjTag::Struct) static_cast<StructObj*>(cur)->visit_stamp = 0;
            }
            stamp = ++mod_visit_stamp;
        }
        struct DepthGuard { int& d; ~DepthGuard() { --d; } } dg{++mod_search_depth};
        res = module_variable_stamped(*this, mod, sym, mh, stamp, saw_binder);
    } else {
        ModVisitedSet visited;
        res = module_variable_uncached(*this, mod, sym, visited, saw_binder);
    }
    uint32_t sym_ep_after = is_symbol(sym) ? as_obj<SymbolObj>(sym)->def_epoch : 0;
    if (!saw_binder && ep == module_epoch && gep == mod_cache_epoch && sym_ep == sym_ep_after) {
        ce.mod_bits = mod.bits;
        ce.sym_bits = sym.bits;
        ce.var_bits = res.bits;
        ce.mod_epoch = ep;
        ce.gc_epoch = gep;
        ce.sym_epoch = sym_ep;
    }
    return res;
}

SCM VM::eval_closure_lookup(SCM eclo, SCM sym, bool definep) {
    auto* ec = as_obj<EvalClosureObj>(eclo);
    if (definep) {
        if (ec->is_interface) return SCM_BOOL_F;
        if (is_symbol(sym)) ++as_obj<SymbolObj>(sym)->def_epoch;
        else ++module_epoch;
        if (is_variable(module_make_local_var) && !as_obj<VariableObj>(module_make_local_var)->value.is_undefined()) {
            return call2(as_obj<VariableObj>(module_make_local_var)->value, ec->module, sym);
        }
        // Fallback if called before module-make-local-var! is defined
        SCM obarray = mod_field(ec->module, MODULE_INDEX_OBARRAY);
        if (is_hashtable(obarray)) {
            as_obj<HashTableObj>(obarray)->is_module_obarray = true;
            SCM h = hash_create_handle(obarray, sym, SCM_BOOL_F, 1);
            if (cdr(h).is_false()) set_cdr(h, make_variable(SCM_UNDEFINED));
            return cdr(h);
        }
        return SCM_BOOL_F;
    }
    return module_variable(ec->module, sym);
}

SCM VM::sym2var(SCM sym, SCM proc, bool definep) {
    SCM var = SCM_BOOL_F;
    if (proc.is_true()) {
        if (is_eval_closure(proc)) {
            var = eval_closure_lookup(proc, sym, definep);
        } else {
            var = call2(proc, sym, SCM::from_bool(definep));
        }
    } else {
        if (!definep) {
            var = hash_ref(pre_modules_obarray, sym, SCM_BOOL_F, 1);
        } else {
            SCM h = hash_create_handle(pre_modules_obarray, sym, SCM_BOOL_F, 1);
            var = cdr(h);
            if (var.is_false()) {
                var = make_variable(SCM_UNDEFINED);
                set_cdr(h, var);
            }
        }
    }
    if (var.is_true() && !is_variable(var)) {
        misc_error("scm_sym2var", "~S is not bound to a variable", list1(sym));
    }
    return var;
}

SCM VM::define_sym(SCM sym, SCM val) {
    SCM var = sym2var(sym, current_module_lookup_closure(), true);
    as_obj<VariableObj>(var)->value = val;
    return var;
}

SCM VM::define(string name, SCM val) {
    return define_sym(make_symbol(name), val);
}

SCM VM::define_proc(string name, int req, int opt, bool rst, SubrFn fn, SpecialSubr sp) {
    SCM proc = make_subr(name, req, opt, rst, std::move(fn), sp);
    define(name, proc);
    return proc;
}

SCM VM::lookup_sym(SCM sym) {
    return sym2var(sym, current_module_lookup_closure(), false);
}

SCM VM::lookup(string name) {
    return lookup_sym(make_symbol(name));
}

static SCM env_top_level(SCM env) {
    while (is_pair(env)) {
        SCM a = car(env);
        if (!is_pair(a) && vm().is_procedure(a)) return a;
        env = cdr(env);
    }
    return SCM_BOOL_F;
}

void VM::init_modules() {
    if (!is_hashtable(pre_modules_obarray)) {
        pre_modules_obarray = make_hashtable(1533, WeakKind::None);
        as_obj<HashTableObj>(pre_modules_obarray)->is_module_obarray = true;
    }
    if (!is_fluid(the_module_fluid)) {
        the_module_fluid = make_fluid(SCM_BOOL_F);
    }
    module_make_local_var = define("module-make-local-var!", SCM_UNDEFINED);

    // Variable primitives
    define_proc("make-variable", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return make_variable(a[0]);
    });
    define_proc("make-undefined-variable", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return make_variable(SCM_UNDEFINED);
    });
    define_proc("variable?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_variable(a[0]));
    });
    define_proc("variable-bound?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_variable(a[0])) wrong_type_arg("variable-bound?", 1, a[0], "variable");
        return SCM::from_bool(!as_obj<VariableObj>(a[0])->value.is_undefined());
    });
    define_proc("variable-ref", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_variable(a[0])) wrong_type_arg("variable-ref", 1, a[0], "variable");
        SCM v = as_obj<VariableObj>(a[0])->value;
        if (v.is_undefined()) misc_error("variable-ref", "variable is unbound: ~S", list1(a[0]));
        return v;
    });
    define_proc("variable-set!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_variable(a[0])) wrong_type_arg("variable-set!", 1, a[0], "variable");
        auto* var = as_obj<VariableObj>(a[0]);
        var->value = a[1];
        notify_var_mutated(var);
        return SCM_UNSPECIFIED;
    });

    // Module primitives
    define_proc("%get-pre-modules-obarray", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return pre_modules_obarray;
    });
    define_proc("current-module", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return current_module();
    });
    define_proc("set-current-module", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return set_current_module(a[0]);
    });
    define_proc("interaction-environment", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return current_module();
    });
    define_proc("standard-eval-closure", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return heap_.alloc_scm<EvalClosureObj>(a[0], false);
    });
    define_proc("standard-interface-eval-closure", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return heap_.alloc_scm<EvalClosureObj>(a[0], true);
    });
    define_proc("env-module", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM proc = env_top_level(a[0]);
        if (proc.is_false()) {
            return (module_system_booted && is_variable(the_root_module_var))
                   ? as_obj<VariableObj>(the_root_module_var)->value : SCM_BOOL_F;
        }
        if (is_eval_closure(proc)) return as_obj<EvalClosureObj>(proc)->module;
        SCM mod = procedure_property(proc, sym_module);
        if (mod.is_false() && module_system_booted && is_variable(the_root_module_var)) {
            mod = as_obj<VariableObj>(the_root_module_var)->value;
        }
        return mod;
    });

    define_proc("module-import-interface", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_module(a[0])) wrong_type_arg("module-import-interface", 1, a[0], "module");
        auto find_iface = [&](auto& find_iface, SCM mod, SCM sym) -> SCM {
            SCM uses = mod_field(mod, MODULE_INDEX_USES);
            while (is_pair(uses)) {
                SCM iface = car(uses);
                SCM obarray = mod_field(iface, MODULE_INDEX_OBARRAY);
                if (is_hashtable(obarray) && hash_ref(obarray, sym, SCM_BOOL_F, 1).is_true()) {
                    return iface;
                }
                SCM binder = mod_field(iface, MODULE_INDEX_BINDER);
                if (binder.is_true() && call3(binder, iface, sym, SCM_BOOL_F).is_true()) {
                    return iface;
                }
                SCM sub = find_iface(find_iface, iface, sym);
                if (sub.is_true()) return sub;
                uses = cdr(uses);
            }
            return SCM_BOOL_F;
        };
        return find_iface(find_iface, a[0], a[1]);
    });
}

} // namespace guile
