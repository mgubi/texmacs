#include "guile_cpp/vm.hpp"

namespace guile {

SCM VM::object_properties(SCM obj) {
    return hash_ref(obj_props_table, obj, SCM_EOL, 1);
}

void VM::set_object_properties(SCM obj, SCM props) {
    hash_set(obj_props_table, obj, props, 1);
}

SCM VM::source_properties(SCM obj) {
    if (!is_pair(obj)) return SCM_EOL;
    SCM h = hash_get_handle(src_props_table, obj, 1);
    if (is_pair(h)) return cdr(h);
    SrcPosEntry* found = src_pos_map->find(obj);
    if (found != nullptr) {
        const SrcPosEntry& e = *found;
        return list3(
            cons(sym_filename, e.filename),
            cons(sym_line, make_integer(e.line)),
            cons(sym_column, make_integer(e.col)));
    }
    return SCM_EOL;
}

void VM::set_source_properties(SCM obj, SCM props) {
    if (!is_pair(obj)) return;
    src_pos_map->reset(obj);
    hash_set(src_props_table, obj, props, 1);
}

void VM::set_source_position(SCM pair_obj, SCM filename, int32_t line, int32_t col) {
    if (!is_pair(pair_obj)) return;
    src_pos_map(pair_obj) = {filename, line, col};
}

SCM VM::source_property(SCM obj, SCM key) {
    if (!is_pair(obj)) return SCM_BOOL_F;
    SCM h = hash_get_handle(src_props_table, obj, 1);
    if (is_pair(h)) {
        SCM pair = scm_assq(key, cdr(h));
        return is_pair(pair) ? cdr(pair) : SCM_BOOL_F;
    }
    SrcPosEntry* found = src_pos_map->find(obj);
    if (found != nullptr) {
        if (key == sym_line) return make_integer(found->line);
        if (key == sym_column) return make_integer(found->col);
        if (key == sym_filename) return found->filename;
    }
    return SCM_BOOL_F;
}

void VM::set_source_property(SCM obj, SCM key, SCM val) {
    if (!is_pair(obj)) return;
    SCM props = source_properties(obj);
    SCM pair = scm_assq(key, props);
    if (is_pair(pair)) {
        set_cdr(pair, val);
    } else {
        set_source_properties(obj, cons(cons(key, val), props));
    }
}

SCM VM::procedure_properties(SCM proc) {
    if (is_proc_with_setter(proc)) {
        SCM own = object_properties(proc);
        SCM gprops = procedure_properties(as_obj<ProcWithSetterObj>(proc)->getter);
        if (own.is_null()) return gprops;
        // Prepend own properties before getter properties so own 'name overrides getter's 'name
        SCM head = SCM_EOL, tail = SCM_EOL;
        for (SCM p = own; is_pair(p); p = cdr(p)) {
            SCM c = cons(car(p), SCM_EOL);
            if (head.is_null()) head = tail = c;
            else { set_cdr(tail, c); tail = c; }
        }
        set_cdr(tail, gprops);
        return head;
    }
    if (is_subr(proc)) {
        auto* s = as_obj<SubrObj>(proc);
        SCM p = s->props;
        // Ensure arity and name are present if queried
        if (scm_assq(sym_name, p).is_false() && !(N(s->name) == 0)) {
            p = cons(cons(sym_name, make_symbol(s->name)), p);
        }
        SCM sym_arity = make_symbol("arity");
        if (scm_assq(sym_arity, p).is_false()) {
            p = cons(cons(sym_arity, list3(make_integer(s->req), make_integer(s->opt), SCM::from_bool(s->rst))), p);
        }
        return p;
    }
    if (is_closure(proc)) {
        auto* c = as_obj<ClosureObj>(proc);
        SCM p = c->props;
        SCM sym_arity = make_symbol("arity");
        if (scm_assq(sym_arity, p).is_false()) {
            int req = 0;
            bool rst = false;
            SCM f = c->formals;
            while (is_pair(f)) { ++req; f = cdr(f); }
            if (!f.is_null()) rst = true;
            p = cons(cons(sym_arity, list3(make_integer(req), make_integer(0), SCM::from_bool(rst))), p);
        }
        return p;
    }
    return object_properties(proc);
}

void VM::set_procedure_properties(SCM proc, SCM props) {
    if (is_subr(proc)) as_obj<SubrObj>(proc)->props = props;
    else if (is_closure(proc)) as_obj<ClosureObj>(proc)->props = props;
    else set_object_properties(proc, props);
}

SCM VM::procedure_property(SCM proc, SCM key) {
    SCM props = procedure_properties(proc);
    SCM pair = scm_assq(key, props);
    return is_pair(pair) ? cdr(pair) : SCM_BOOL_F;
}

void VM::set_procedure_property(SCM proc, SCM key, SCM val) {
    SCM props = is_subr(proc) ? as_obj<SubrObj>(proc)->props
              : is_closure(proc) ? as_obj<ClosureObj>(proc)->props
              : object_properties(proc);
    SCM pair = scm_assq(key, props);
    if (is_pair(pair)) {
        set_cdr(pair, val);
    } else {
        set_procedure_properties(proc, cons(cons(key, val), props));
    }
}

void VM::init_properties() {
    obj_props_table = make_hashtable(61, WeakKind::KeyAlist);
    src_props_table = make_hashtable(257, WeakKind::KeyAlist);

    // Object properties
    define_proc("object-properties", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return object_properties(a[0]);
    });
    define_proc("set-object-properties!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        set_object_properties(a[0], a[1]);
        return a[1];
    });
    define_proc("object-property", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM pair = scm_assq(a[1], object_properties(a[0]));
        return is_pair(pair) ? cdr(pair) : SCM_BOOL_F;
    });
    define_proc("set-object-property!", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM props = object_properties(a[0]);
        SCM pair = scm_assq(a[1], props);
        if (is_pair(pair)) set_cdr(pair, a[2]);
        else set_object_properties(a[0], cons(cons(a[1], a[2]), props));
        return a[2];
    });

    // Source properties
    define_proc("supports-source-properties?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_pair(a[0]));
    });
    define_proc("source-properties", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return source_properties(a[0]);
    });
    define_proc("set-source-properties!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        set_source_properties(a[0], a[1]);
        return a[1];
    });
    define_proc("source-property", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        return source_property(a[0], a[1]);
    });
    define_proc("set-source-property!", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        set_source_property(a[0], a[1], a[2]);
        return a[2];
    });

    // Procedure properties
    define_proc("procedure-properties", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("procedure-properties", 1, a[0], "procedure");
        return procedure_properties(a[0]);
    });
    define_proc("set-procedure-properties!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("set-procedure-properties!", 1, a[0], "procedure");
        set_procedure_properties(a[0], a[1]);
        return SCM_UNSPECIFIED;
    });
    define_proc("procedure-property", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("procedure-property", 1, a[0], "procedure");
        return procedure_property(a[0], a[1]);
    });
    define_proc("set-procedure-property!", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("set-procedure-property!", 1, a[0], "procedure");
        set_procedure_property(a[0], a[1], a[2]);
        return SCM_UNSPECIFIED;
    });
    define_proc("procedure-name", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("procedure-name", 1, a[0], "procedure");
        return procedure_property(a[0], sym_name);
    });
    define_proc("procedure-documentation", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("procedure-documentation", 1, a[0], "procedure");
        SCM d = procedure_property(a[0], sym_documentation);
        if (d.is_true()) return d;
        if (is_closure(a[0])) {
            SCM body = as_obj<ClosureObj>(a[0])->body;
            if (is_pair(body) && is_string(car(body)) && is_pair(cdr(body))) {
                return car(body);
            }
        }
        return SCM_BOOL_F;
    });
    define_proc("procedure-source", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("procedure-source", 1, a[0], "procedure");
        if (is_closure(a[0])) {
            auto* c = as_obj<ClosureObj>(a[0]);
            return cons(sym_lambda, cons(c->formals, c->body));
        }
        return SCM_BOOL_F;
    });

    // Primitive properties (deprecated Guile 1.8 API used by boot-9!)
    define_proc("primitive-make-property", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return cons(a[0], make_hashtable(31, WeakKind::KeyAlist));
    });
    define_proc("primitive-property-ref", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_pair(a[0])) wrong_type_arg("primitive-property-ref", 1, a[0], "property");
        SCM h = hash_get_handle(cdr(a[0]), a[1], 1);
        if (is_pair(h)) return cdr(h);
        SCM not_found = car(a[0]);
        if (is_procedure(not_found)) return call2(not_found, a[0], a[1]);
        return not_found;
    });
    define_proc("primitive-property-set!", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_pair(a[0])) wrong_type_arg("primitive-property-set!", 1, a[0], "property");
        hash_set(cdr(a[0]), a[1], a[2], 1);
        return a[2];
    });
    define_proc("primitive-property-del!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_pair(a[0])) wrong_type_arg("primitive-property-del!", 1, a[0], "property");
        hash_remove(cdr(a[0]), a[1], 1);
        return SCM_UNSPECIFIED;
    });

    // Hooks
    define_proc("make-hook", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        int arity = 0;
        if (nargs >= 1 && !a[0].is_undefined()) {
            int64_t n = scm_to_int64(a[0], "make-hook", 1);
            if (n < 0 || n > 16) out_of_range("make-hook", a[0]);
            arity = static_cast<int>(n);
        }
        return heap_.alloc_scm<HookObj>(arity);
    });
    define_proc("hook?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_hook(a[0]));
    });
    define_proc("hook-empty?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_hook(a[0])) wrong_type_arg("hook-empty?", 1, a[0], "hook");
        return SCM::from_bool(as_obj<HookObj>(a[0])->procs.is_null());
    });
    define_proc("add-hook!", 2, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_hook(a[0])) wrong_type_arg("add-hook!", 1, a[0], "hook");
        if (!is_procedure(a[1])) wrong_type_arg("add-hook!", 2, a[1], "procedure");
        auto* hk = as_obj<HookObj>(a[0]);
        // Check procedure arity compatibility with hook arity
        if (is_closure(a[1])) {
            int req = 0; bool rst = false;
            SCM f = as_obj<ClosureObj>(a[1])->formals;
            while (is_pair(f)) { ++req; f = cdr(f); }
            if (!f.is_null()) rst = true;
            if (hk->arity < req || (!rst && hk->arity > req)) {
                wrong_type_arg("add-hook!", 2, a[1], "procedure with matching arity");
            }
        } else if (is_subr(a[1])) {
            auto* s = as_obj<SubrObj>(a[1]);
            if (hk->arity < s->req || (!s->rst && hk->arity > s->req + s->opt)) {
                wrong_type_arg("add-hook!", 2, a[1], "procedure with matching arity");
            }
        }
        bool append_p = (nargs >= 3 && !a[2].is_undefined() && a[2].is_true());
        // Remove existing occurrence first
        SCM cur = hk->procs, head = SCM_EOL, tail = SCM_EOL;
        while (is_pair(cur)) {
            if (car(cur) != a[1]) {
                SCM n = cons(car(cur), SCM_EOL);
                if (head.is_null()) head = tail = n;
                else { set_cdr(tail, n); tail = n; }
            }
            cur = cdr(cur);
        }
        if (append_p) {
            SCM n = cons(a[1], SCM_EOL);
            if (head.is_null()) hk->procs = n;
            else { set_cdr(tail, n); hk->procs = head; }
        } else {
            hk->procs = cons(a[1], head);
        }
        return SCM_UNSPECIFIED;
    });
    define_proc("remove-hook!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_hook(a[0])) wrong_type_arg("remove-hook!", 1, a[0], "hook");
        auto* hk = as_obj<HookObj>(a[0]);
        SCM cur = hk->procs, head = SCM_EOL, tail = SCM_EOL;
        while (is_pair(cur)) {
            if (car(cur) != a[1]) {
                SCM n = cons(car(cur), SCM_EOL);
                if (head.is_null()) head = tail = n;
                else { set_cdr(tail, n); tail = n; }
            }
            cur = cdr(cur);
        }
        hk->procs = head;
        return SCM_UNSPECIFIED;
    });
    define_proc("reset-hook!", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_hook(a[0])) wrong_type_arg("reset-hook!", 1, a[0], "hook");
        as_obj<HookObj>(a[0])->procs = SCM_EOL;
        return SCM_UNSPECIFIED;
    });
    define_proc("hook->list", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_hook(a[0])) wrong_type_arg("hook->list", 1, a[0], "hook");
        return scm_list_copy(as_obj<HookObj>(a[0])->procs);
    });
    define_proc("run-hook", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_hook(a[0])) wrong_type_arg("run-hook", 1, a[0], "hook");
        auto* hk = as_obj<HookObj>(a[0]);
        SCM arg_list = nargs > 1 ? a[1] : SCM_EOL;
        if (scm_ilength(arg_list) != hk->arity) {
            misc_error("run-hook", "Hook ~S requires ~A arguments", list2(a[0], make_integer(hk->arity)));
        }
        SCM procs = scm_list_copy(hk->procs);
        while (is_pair(procs)) {
            apply_list(car(procs), arg_list);
            procs = cdr(procs);
        }
        return SCM_UNSPECIFIED;
    });

    // Standard GC & exit hooks
    after_gc_hook = heap_.alloc_scm<HookObj>(0);
    before_gc_hook = heap_.alloc_scm<HookObj>(0);
    exit_hook = heap_.alloc_scm<HookObj>(0);
    define("after-gc-hook", after_gc_hook);
    define("before-gc-hook", before_gc_hook);
    define("exit-hook", exit_hook);

    // Guardians
    define_proc("make-guardian", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        bool greedy = (nargs >= 1 && !a[0].is_undefined() && a[0].is_true());
        return heap_.alloc_scm<GuardianObj>(greedy);
    });
    define_proc("guardian?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_guardian(a[0]));
    });
    define_proc("guardian-greedy?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_guardian(a[0])) wrong_type_arg("guardian-greedy?", 1, a[0], "guardian");
        return SCM::from_bool(as_obj<GuardianObj>(a[0])->greedy);
    });
    define_proc("guardian-destroyed?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_guardian(a[0])) wrong_type_arg("guardian-destroyed?", 1, a[0], "guardian");
        return SCM::from_bool(as_obj<GuardianObj>(a[0])->destroyed);
    });
    define_proc("destroy-guardian!", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_guardian(a[0])) wrong_type_arg("destroy-guardian!", 1, a[0], "guardian");
        auto* g = as_obj<GuardianObj>(a[0]);
        g->destroyed = true;
        gcpp_clear(g->live);
        return SCM_UNSPECIFIED;
    });

    // Arbiters
    define_proc("make-arbiter", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return heap_.alloc_scm<ArbiterObj>(a[0]);
    });
    define_proc("try-arbiter", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_arbiter(a[0])) wrong_type_arg("try-arbiter", 1, a[0], "arbiter");
        auto* ar = as_obj<ArbiterObj>(a[0]);
        if (ar->locked) return SCM_BOOL_F;
        ar->locked = true;
        return SCM_BOOL_T;
    });
    define_proc("release-arbiter", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_arbiter(a[0])) wrong_type_arg("release-arbiter", 1, a[0], "arbiter");
        auto* ar = as_obj<ArbiterObj>(a[0]);
        if (!ar->locked) return SCM_BOOL_F;
        ar->locked = false;
        return SCM_BOOL_T;
    });
}

} // namespace guile
