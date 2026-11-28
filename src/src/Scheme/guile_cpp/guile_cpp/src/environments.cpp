#include "guile_cpp/vm.hpp"
#include <algorithm>
#include <vector>

namespace guile {

namespace {

void env_notify(SCM env_scm);

SCM env_add_observer(SCM env_scm, SCM proc_or_parent, bool weak) {
    auto* en = as_obj<EnvironmentObj>(env_scm);
    SCM tok = vm().heap().alloc_scm<ObserverObj>(env_scm, proc_or_parent, weak);
    if (weak) en->weak_observers.push_back(tok);
    else en->strong_observers.push_back(tok);
    return tok;
}

void env_remove_observer(SCM tok_scm) {
    if (!is_observer(tok_scm)) return;
    auto* obs = as_obj<ObserverObj>(tok_scm);
    if (!obs->active) return;
    obs->active = false;
    if (is_environment(obs->env)) {
        auto* en = as_obj<EnvironmentObj>(obs->env);
        auto& vec = obs->weak ? en->weak_observers : en->strong_observers;
        vec.erase(std::remove(vec.begin(), vec.end(), tok_scm), vec.end());
    }
    obs->env = SCM_BOOL_F;
    obs->proc = SCM_BOOL_F;
}

void env_notify(SCM env_scm) {
    if (!is_environment(env_scm)) return;
    auto* en = as_obj<EnvironmentObj>(env_scm);
    // Snapshot active observers so mutations during callbacks don't invalidate iteration
    ScmRootVec snapshot(vm().heap());
    for (SCM o : en->strong_observers) snapshot.push_back(o);
    for (SCM o : en->weak_observers) snapshot.push_back(o);

    bool has_err = false;
    SCM err_key = SCM_BOOL_F;
    SCM err_args = SCM_EOL;

    for (size_t i = 0; i < snapshot.size(); ++i) {
        SCM o = snapshot[i];
        if (!is_observer(o)) continue;
        auto* obs = as_obj<ObserverObj>(o);
        if (!obs->active) continue;
        try {
            if (is_environment(obs->proc)) {
                env_notify(obs->proc);
            } else if (vm().is_procedure(obs->proc)) {
                vm().call1(obs->proc, env_scm);
            }
        } catch (const SchemeThrow& ex) {
            if (!has_err) {
                has_err = true;
                err_key = ex.key;
                err_args = ex.args;
            }
        }
    }

    if (has_err) {
        throw SchemeThrow(err_key, err_args);
    }
}

[[noreturn]] void error_env_unbound(const char* subr, SCM env, SCM sym) {
    vm().misc_error(subr, "Symbol ~S not bound in environment ~S", vm().list2(sym, env));
}

[[noreturn]] void error_env_immutable_binding(const char* subr, SCM env, SCM sym) {
    vm().misc_error(subr, "Binding for symbol ~S is immutable in environment ~S", vm().list2(sym, env));
}

// Forward declarations for recursive dispatch across environment types
SCM env_ref_opt(SCM env_scm, SCM sym); // returns SCM_UNDEFINED if unbound
SCM env_cell_opt(SCM env_scm, SCM sym, bool for_write); // returns SCM_UNDEFINED if unbound
void env_define_op(SCM env_scm, SCM sym, SCM val);
void env_undefine_op(SCM env_scm, SCM sym);
void env_set_op(SCM env_scm, SCM sym, SCM val);
SCM env_fold_op(SCM env_scm, SCM proc, SCM init);

// Helper for export-environment signature matching:
// signature is either a list of symbols, or a procedure (sym -> #t/#f)
bool export_allows(SCM sig, SCM sym) {
    if (vm().is_procedure(sig)) {
        return vm().call1(sig, sym).is_true();
    }
    SCM cur = sig;
    while (is_pair(cur)) {
        if (car(cur) == sym) return true;
        cur = cdr(cur);
    }
    return false;
}

SCM env_cell_opt(SCM env_scm, SCM sym, bool for_write) {
    auto* en = as_obj<EnvironmentObj>(env_scm);
    switch (en->env_kind) {
    case EnvKind::Leaf: {
        SCM handle = hash_get_handle(en->table, sym, 0);
        if (handle.is_false()) return SCM_UNDEFINED;
        return cdr(handle); // pair (sym . val)
    }
    case EnvKind::Eval: {
        SCM c = env_cell_opt(en->local, sym, for_write);
        if (!c.is_undefined()) return c;
        return env_cell_opt(en->imported, sym, for_write);
    }
    case EnvKind::Import: {
        SCM found = SCM_UNDEFINED;
        SCM cur = en->local; // list of imported envs
        while (is_pair(cur)) {
            SCM c = env_cell_opt(car(cur), sym, for_write);
            if (!c.is_undefined()) {
                if (found.is_undefined()) found = c;
                else if (vm().is_procedure(en->imported)) {
                    // Conflict resolution
                    SCM resolved = vm().call3(en->imported, env_scm, sym, cur);
                    return is_pair(resolved) ? resolved : vm().cons(sym, resolved);
                }
            }
            cur = cdr(cur);
        }
        return found;
    }
    case EnvKind::Export: {
        if (!export_allows(en->imported, sym)) return SCM_UNDEFINED;
        return env_cell_opt(en->local, sym, for_write);
    }
    }
    return SCM_UNDEFINED;
}

SCM env_ref_opt(SCM env_scm, SCM sym) {
    SCM cell = env_cell_opt(env_scm, sym, false);
    if (cell.is_undefined()) return SCM_UNDEFINED;
    return cdr(cell);
}

void env_define_op(SCM env_scm, SCM sym, SCM val) {
    auto* en = as_obj<EnvironmentObj>(env_scm);
    switch (en->env_kind) {
    case EnvKind::Leaf: {
        // Always allocate a FRESH cell pair on define!
        SCM cell = vm().cons(sym, val);
        hash_set(en->table, sym, cell, 0);
        env_notify(env_scm);
        return;
    }
    case EnvKind::Eval:
        env_define_op(en->local, sym, val);
        return;
    case EnvKind::Import:
    case EnvKind::Export:
        error_env_immutable_binding("environment-define", env_scm, sym);
    }
}

void env_undefine_op(SCM env_scm, SCM sym) {
    auto* en = as_obj<EnvironmentObj>(env_scm);
    switch (en->env_kind) {
    case EnvKind::Leaf: {
        SCM handle = hash_get_handle(en->table, sym, 0);
        if (handle.is_false()) return; // already undefined: do NOT notify!
        hash_remove(en->table, sym, 0);
        env_notify(env_scm);
        return;
    }
    case EnvKind::Eval:
        env_undefine_op(en->local, sym);
        return;
    case EnvKind::Import:
    case EnvKind::Export:
        error_env_immutable_binding("environment-undefine", env_scm, sym);
    }
}

void env_set_op(SCM env_scm, SCM sym, SCM val) {
    SCM cell = env_cell_opt(env_scm, sym, true);
    if (cell.is_undefined()) error_env_unbound("environment-set!", env_scm, sym);
    set_cdr(cell, val);
}

// Collect all (sym . val) bindings of an environment without duplicates (preserving shadowing)
void env_collect_bindings(SCM env_scm, std::vector<std::pair<SCM, SCM>>& out, std::vector<SCM>& seen) {
    auto* en = as_obj<EnvironmentObj>(env_scm);
    auto has_seen = [&](SCM sym) {
        for (SCM s : seen) if (s == sym) return true;
        return false;
    };
    switch (en->env_kind) {
    case EnvKind::Leaf: {
        auto* ht = as_obj<HashTableObj>(en->table);
        for (SCM b : ht->buckets) {
            SCM p = b;
            while (is_pair(p)) {
                SCM entry = car(p); // (sym . cell)
                SCM sym = car(entry);
                SCM cell = cdr(entry);
                if (!has_seen(sym)) {
                    seen.push_back(sym);
                    out.emplace_back(sym, cdr(cell));
                }
                p = cdr(p);
            }
        }
        break;
    }
    case EnvKind::Eval:
        env_collect_bindings(en->local, out, seen);
        env_collect_bindings(en->imported, out, seen);
        break;
    case EnvKind::Import: {
        SCM cur = en->local;
        while (is_pair(cur)) {
            env_collect_bindings(car(cur), out, seen);
            cur = cdr(cur);
        }
        break;
    }
    case EnvKind::Export: {
        std::vector<std::pair<SCM, SCM>> sub_out;
        std::vector<SCM> sub_seen;
        env_collect_bindings(en->local, sub_out, sub_seen);
        for (auto& [sym, val] : sub_out) {
            if (!has_seen(sym) && export_allows(en->imported, sym)) {
                seen.push_back(sym);
                out.emplace_back(sym, val);
            }
        }
        break;
    }
    }
}

SCM env_fold_op(SCM env_scm, SCM proc, SCM init) {
    std::vector<std::pair<SCM, SCM>> bindings;
    std::vector<SCM> seen;
    env_collect_bindings(env_scm, bindings, seen);
    SCM acc = init;
    for (auto& [sym, val] : bindings) {
        acc = vm().call3(proc, sym, val, acc);
    }
    return acc;
}

} // namespace

void VM::init_environments() {
    auto def_doc = [this](const char* name, int req, int opt, bool rst, SubrFn fn, const char* doc) {
        SCM p = define_proc(name, req, opt, rst, std::move(fn));
        set_procedure_property(p, sym_documentation, make_string(doc));
        return p;
    };

    def_doc("environment?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_environment(a[0]));
    }, "Return @code{#t} if @var{obj} is an environment, or @code{#f} otherwise.");

    def_doc("environment-bound?", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0])) wrong_type_arg("environment-bound?", 1, a[0], "environment");
        if (!is_symbol(a[1])) wrong_type_arg("environment-bound?", 2, a[1], "symbol");
        return SCM::from_bool(!env_ref_opt(a[0], a[1]).is_undefined());
    }, "Return @code{#t} if @var{sym} is bound in @var{env}, or @code{#f} otherwise.");

    def_doc("environment-ref", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0])) wrong_type_arg("environment-ref", 1, a[0], "environment");
        if (!is_symbol(a[1])) wrong_type_arg("environment-ref", 2, a[1], "symbol");
        SCM val = env_ref_opt(a[0], a[1]);
        if (val.is_undefined()) error_env_unbound("environment-ref", a[0], a[1]);
        return val;
    }, "Return the value of the location bound to @var{sym} in @var{env}.");

    def_doc("environment-fold", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0])) wrong_type_arg("environment-fold", 1, a[0], "environment");
        if (!is_procedure(a[1])) wrong_type_arg("environment-fold", 2, a[1], "procedure");
        return env_fold_op(a[0], a[1], a[2]);
    }, "Iterate over all the bindings in @var{env}, accumulating a result.");

    def_doc("environment-define", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0])) wrong_type_arg("environment-define", 1, a[0], "environment");
        if (!is_symbol(a[1])) wrong_type_arg("environment-define", 2, a[1], "symbol");
        env_define_op(a[0], a[1], a[2]);
        return SCM_UNSPECIFIED;
    }, "Bind @var{sym} to a new location containing @var{val} in @var{env}.");

    def_doc("environment-undefine", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0])) wrong_type_arg("environment-undefine", 1, a[0], "environment");
        if (!is_symbol(a[1])) wrong_type_arg("environment-undefine", 2, a[1], "symbol");
        env_undefine_op(a[0], a[1]);
        return SCM_UNSPECIFIED;
    }, "Remove any binding for @var{sym} from @var{env}.");

    def_doc("environment-set!", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0])) wrong_type_arg("environment-set!", 1, a[0], "environment");
        if (!is_symbol(a[1])) wrong_type_arg("environment-set!", 2, a[1], "symbol");
        env_set_op(a[0], a[1], a[2]);
        return SCM_UNSPECIFIED;
    }, "If @var{env} binds @var{sym} to some location, change that location's value to @var{val}.");

    def_doc("environment-cell", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0])) wrong_type_arg("environment-cell", 1, a[0], "environment");
        if (!is_symbol(a[1])) wrong_type_arg("environment-cell", 2, a[1], "symbol");
        SCM cell = env_cell_opt(a[0], a[1], a[2].is_true());
        if (cell.is_undefined()) error_env_unbound("environment-cell", a[0], a[1]);
        return cell;
    }, "Return the value cell which @var{env} binds to @var{sym}, or @code{#f} if unavailable.");

    def_doc("environment-observe", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0])) wrong_type_arg("environment-observe", 1, a[0], "environment");
        if (!is_procedure(a[1])) wrong_type_arg("environment-observe", 2, a[1], "procedure");
        return env_add_observer(a[0], a[1], false);
    }, "Whenever @var{env}'s bindings change, call @var{proc} with @var{env}.");

    def_doc("environment-observe-weak", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0])) wrong_type_arg("environment-observe-weak", 1, a[0], "environment");
        if (!is_procedure(a[1])) wrong_type_arg("environment-observe-weak", 2, a[1], "procedure");
        return env_add_observer(a[0], a[1], true);
    }, "Like @code{environment-observe}, but holds the observer token weakly.");

    def_doc("environment-unobserve", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_observer(a[0])) wrong_type_arg("environment-unobserve", 1, a[0], "observer");
        env_remove_observer(a[0]);
        return SCM_UNSPECIFIED;
    }, "Cancel the observation request which returned the value @var{token}.");

    // Leaf environments
    def_doc("make-leaf-environment", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        auto* en = heap_.alloc<EnvironmentObj>(EnvKind::Leaf);
        en->table = make_hashtable(31);
        return SCM::from_obj(en);
    }, "Create a new leaf environment, containing no bindings.");

    def_doc("leaf-environment?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_environment(a[0]) && as_obj<EnvironmentObj>(a[0])->env_kind == EnvKind::Leaf);
    }, "Return @code{#t} if @var{object} is a leaf environment, or @code{#f} otherwise.");

    // Eval environments
    def_doc("make-eval-environment", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0])) wrong_type_arg("make-eval-environment", 1, a[0], "environment");
        if (!is_environment(a[1])) wrong_type_arg("make-eval-environment", 2, a[1], "environment");
        auto* en = heap_.alloc<EnvironmentObj>(EnvKind::Eval);
        en->local = a[0];
        en->imported = a[1];
        SCM self = SCM::from_obj(en);
        en->child_obs_1 = env_add_observer(a[0], self, true);
        en->child_obs_2 = env_add_observer(a[1], self, true);
        return self;
    }, "Return a new environment object whose bindings are the union of @var{local} and @var{imported}.");

    def_doc("eval-environment?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_environment(a[0]) && as_obj<EnvironmentObj>(a[0])->env_kind == EnvKind::Eval);
    }, "Return @code{#t} if @var{object} is an eval environment, or @code{#f} otherwise.");

    def_doc("eval-environment-local", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0]) || as_obj<EnvironmentObj>(a[0])->env_kind != EnvKind::Eval) {
            wrong_type_arg("eval-environment-local", 1, a[0], "eval-environment");
        }
        return as_obj<EnvironmentObj>(a[0])->local;
    }, "Return the local environment of eval environment @var{env}.");

    def_doc("eval-environment-set-local!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0]) || as_obj<EnvironmentObj>(a[0])->env_kind != EnvKind::Eval) {
            wrong_type_arg("eval-environment-set-local!", 1, a[0], "eval-environment");
        }
        if (!is_environment(a[1])) wrong_type_arg("eval-environment-set-local!", 2, a[1], "environment");
        auto* en = as_obj<EnvironmentObj>(a[0]);
        env_remove_observer(en->child_obs_1);
        en->local = a[1];
        en->child_obs_1 = env_add_observer(a[1], a[0], true);
        env_notify(a[0]);
        return SCM_UNSPECIFIED;
    }, "Change @var{env}'s local environment to @var{local}.");

    def_doc("eval-environment-imported", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0]) || as_obj<EnvironmentObj>(a[0])->env_kind != EnvKind::Eval) {
            wrong_type_arg("eval-environment-imported", 1, a[0], "eval-environment");
        }
        return as_obj<EnvironmentObj>(a[0])->imported;
    }, "Return the imported environment of eval environment @var{env}.");

    def_doc("eval-environment-set-imported!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0]) || as_obj<EnvironmentObj>(a[0])->env_kind != EnvKind::Eval) {
            wrong_type_arg("eval-environment-set-imported!", 1, a[0], "eval-environment");
        }
        if (!is_environment(a[1])) wrong_type_arg("eval-environment-set-imported!", 2, a[1], "environment");
        auto* en = as_obj<EnvironmentObj>(a[0]);
        env_remove_observer(en->child_obs_2);
        en->imported = a[1];
        en->child_obs_2 = env_add_observer(a[1], a[0], true);
        env_notify(a[0]);
        return SCM_UNSPECIFIED;
    }, "Change @var{env}'s imported environment to @var{imported}.");

    // Import environments
    def_doc("make-import-environment", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM cur = a[0];
        while (is_pair(cur)) {
            if (!is_environment(car(cur))) wrong_type_arg("make-import-environment", 1, a[0], "list of environments");
            cur = cdr(cur);
        }
        if (!cur.is_null()) wrong_type_arg("make-import-environment", 1, a[0], "list of environments");
        auto* en = heap_.alloc<EnvironmentObj>(EnvKind::Import);
        en->local = scm_list_copy(a[0]);
        en->imported = a[1];
        SCM self = SCM::from_obj(en);
        SCM obs_list = SCM_EOL;
        cur = en->local;
        while (is_pair(cur)) {
            obs_list = cons(env_add_observer(car(cur), self, true), obs_list);
            cur = cdr(cur);
        }
        en->child_obs_2 = obs_list;
        return self;
    }, "Return a new environment importing bindings from @var{imports}, resolving conflicts with @var{conflict-proc}.");

    def_doc("import-environment?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_environment(a[0]) && as_obj<EnvironmentObj>(a[0])->env_kind == EnvKind::Import);
    }, "Return @code{#t} if @var{object} is an import environment, or @code{#f} otherwise.");

    def_doc("import-environment-imports", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0]) || as_obj<EnvironmentObj>(a[0])->env_kind != EnvKind::Import) {
            wrong_type_arg("import-environment-imports", 1, a[0], "import-environment");
        }
        return as_obj<EnvironmentObj>(a[0])->local;
    }, "Return the list of environments imported by the import environment @var{env}.");

    def_doc("import-environment-set-imports!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0]) || as_obj<EnvironmentObj>(a[0])->env_kind != EnvKind::Import) {
            wrong_type_arg("import-environment-set-imports!", 1, a[0], "import-environment");
        }
        auto* en = as_obj<EnvironmentObj>(a[0]);
        SCM old = en->child_obs_2;
        while (is_pair(old)) { env_remove_observer(car(old)); old = cdr(old); }
        en->local = scm_list_copy(a[1]);
        SCM obs_list = SCM_EOL;
        SCM cur = en->local;
        while (is_pair(cur)) {
            obs_list = cons(env_add_observer(car(cur), a[0], true), obs_list);
            cur = cdr(cur);
        }
        en->child_obs_2 = obs_list;
        env_notify(a[0]);
        return SCM_UNSPECIFIED;
    }, "Change @var{env}'s list of imported environments to @var{imports}.");

    // Export environments
    def_doc("make-export-environment", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0])) wrong_type_arg("make-export-environment", 1, a[0], "environment");
        auto* en = heap_.alloc<EnvironmentObj>(EnvKind::Export);
        en->local = a[0];
        en->imported = a[1];
        SCM self = SCM::from_obj(en);
        en->child_obs_1 = env_add_observer(a[0], self, true);
        return self;
    }, "Return a new environment restricting @var{private} to bindings matching @var{signature}.");

    def_doc("export-environment?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_environment(a[0]) && as_obj<EnvironmentObj>(a[0])->env_kind == EnvKind::Export);
    }, "Return @code{#t} if @var{object} is an export environment, or @code{#f} otherwise.");

    def_doc("export-environment-private", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0]) || as_obj<EnvironmentObj>(a[0])->env_kind != EnvKind::Export) {
            wrong_type_arg("export-environment-private", 1, a[0], "export-environment");
        }
        return as_obj<EnvironmentObj>(a[0])->local;
    }, "Return the private environment of export environment @var{env}.");

    def_doc("export-environment-set-private!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0]) || as_obj<EnvironmentObj>(a[0])->env_kind != EnvKind::Export) {
            wrong_type_arg("export-environment-set-private!", 1, a[0], "export-environment");
        }
        if (!is_environment(a[1])) wrong_type_arg("export-environment-set-private!", 2, a[1], "environment");
        auto* en = as_obj<EnvironmentObj>(a[0]);
        env_remove_observer(en->child_obs_1);
        en->local = a[1];
        en->child_obs_1 = env_add_observer(a[1], a[0], true);
        env_notify(a[0]);
        return SCM_UNSPECIFIED;
    }, "Change the private environment of export environment @var{env}.");

    def_doc("export-environment-signature", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0]) || as_obj<EnvironmentObj>(a[0])->env_kind != EnvKind::Export) {
            wrong_type_arg("export-environment-signature", 1, a[0], "export-environment");
        }
        return as_obj<EnvironmentObj>(a[0])->imported;
    }, "Return the signature of export environment @var{env}.");

    def_doc("export-environment-set-signature!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_environment(a[0]) || as_obj<EnvironmentObj>(a[0])->env_kind != EnvKind::Export) {
            wrong_type_arg("export-environment-set-signature!", 1, a[0], "export-environment");
        }
        as_obj<EnvironmentObj>(a[0])->imported = a[1];
        env_notify(a[0]);
        return SCM_UNSPECIFIED;
    }, "Change the signature of export environment @var{env}.");
}

} // namespace guile
