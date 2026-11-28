#include "guile_cpp/vm.hpp"
#include <algorithm>
#include <cstring>

namespace guile {

// ============================================================================
// Subr / Closure / Macro / Values Constructors
// ============================================================================

SCM VM::make_subr(const std::string& name, int req, int opt, bool rst, SubrFn fn, SpecialSubr sp) {
    return heap_.alloc_scm<SubrObj>(name, req, opt, rst, std::move(fn), sp);
}

SCM VM::make_closure(SCM formals, SCM body, SCM env) {
    return heap_.alloc_scm<ClosureObj>(formals, body, env);
}

SCM VM::make_macro(MacroKind kind, SCM transformer, SCM name) {
    return heap_.alloc_scm<MacroObj>(kind, transformer, name);
}

SCM VM::make_values(SCM list_of_vals) {
    if (is_pair(list_of_vals) && cdr(list_of_vals).is_null()) {
        return car(list_of_vals);
    }
    return heap_.alloc_scm<ValuesObj>(list_of_vals);
}

bool VM::is_procedure(SCM x) const {
    if (is_subr(x) || is_closure(x) || is_continuation(x) || is_proc_with_setter(x) ||
        is_eval_closure(x) || is_guardian(x)) {
        return true;
    }
    if (is_struct(x)) {
        auto* st = as_obj<StructObj>(x);
        if (st->is_entity || st->is_operator) return true;
        uint32_t of = obj_class_flags(x);
        if ((of & (CLASSF_ENTITY | CLASSF_OPERATOR | CLASSF_PURE_GENERIC)) != 0) return true;
    }
    if (is_smob(x)) {
        auto* sm = as_obj<SmobObj>(x);
        return sm->smob_type < smob_types_.size() && static_cast<bool>(smob_types_[sm->smob_type].apply);
    }
    return false;
}

// ============================================================================
// Lexical Environment Lookup & Binding (Guile 1.8.8 exact format)
// ============================================================================

namespace {

enum class BindingKind { NotFound, LocalCar, LocalCdr, GlobalVar };

struct BindingLoc {
    BindingKind kind = BindingKind::NotFound;
    SCM cell = SCM_BOOL_F; // PairObj (for LocalCar/LocalCdr) or VariableObj (for GlobalVar)
};

BindingLoc lookup_binding(SCM sym, SCM env, bool definep) {
    SCM e = env;
    while (is_pair(e)) {
        SCM frame = car(e);
        if (is_pair(frame)) {
            SCM vars = car(frame);
            SCM vals = cdr(frame);
            SCM prev_val_pair = frame;
            bool is_first = true;
            while (is_pair(vars)) {
                if (car(vars) == sym) {
                    return {BindingKind::LocalCar, vals};
                }
                vars = cdr(vars);
                prev_val_pair = vals;
                vals = cdr(vals);
                is_first = false;
            }
            if (vars == sym) {
                return {BindingKind::LocalCdr, is_first ? frame : prev_val_pair};
            }
            e = cdr(e);
        } else {
            // Top-level eval-closure frame
            SCM var = vm().sym2var(sym, frame, definep);
            if (var.is_true()) return {BindingKind::GlobalVar, var};
            return {BindingKind::NotFound, SCM_BOOL_F};
        }
    }
    SCM var = vm().sym2var(sym, SCM_BOOL_F, definep);
    if (var.is_true()) return {BindingKind::GlobalVar, var};
    return {BindingKind::NotFound, SCM_BOOL_F};
}

SCM get_binding_value(const BindingLoc& loc, SCM sym) {
    switch (loc.kind) {
    case BindingKind::LocalCar: {
        SCM v = car(loc.cell);
        if (v.is_undefined()) {
            vm().error(vm().sym_unbound_variable, nullptr, "Variable used before given a value: ~S", vm().list1(sym), SCM_BOOL_F);
        }
        return v;
    }
    case BindingKind::LocalCdr: {
        SCM v = cdr(loc.cell);
        if (v.is_undefined()) {
            vm().error(vm().sym_unbound_variable, nullptr, "Variable used before given a value: ~S", vm().list1(sym), SCM_BOOL_F);
        }
        return v;
    }
    case BindingKind::GlobalVar: {
        SCM v = as_obj<VariableObj>(loc.cell)->value;
        if (v.is_undefined()) {
            vm().error(vm().sym_unbound_variable, nullptr, "Unbound variable: ~S", vm().list1(sym), SCM_BOOL_F);
        }
        return v;
    }
    case BindingKind::NotFound:
        vm().error(vm().sym_unbound_variable, nullptr, "Unbound variable: ~S", vm().list1(sym), SCM_BOOL_F);
    }
    return SCM_UNDEFINED;
}

SCM lookup_env_opt(SCM sym, SCM env) {
    auto loc = lookup_binding(sym, env, false);
    if (loc.kind == BindingKind::NotFound) return SCM_UNDEFINED;
    if (loc.kind == BindingKind::LocalCar) return car(loc.cell);
    if (loc.kind == BindingKind::LocalCdr) return cdr(loc.cell);
    return as_obj<VariableObj>(loc.cell)->value;
}

SCM env_top_level_closure(SCM env) {
    while (is_pair(env)) {
        SCM a = car(env);
        if (!is_pair(a)) return a;
        env = cdr(env);
    }
    return SCM_BOOL_F;
}

SCM extend_env_formals(SCM formals, const SCM* args, size_t nargs, SCM env, SCM proc_for_err) {
    if (formals.is_null()) {
        if (nargs != 0) vm().wrong_num_args(nullptr, proc_for_err);
        return env;
    }
    if (is_symbol(formals)) {
        SCM vals = vm().list_n(args, nargs);
        return vm().cons(vm().cons(formals, vals), env);
    }
    if (!is_pair(formals)) {
        vm().misc_error("eval", "Bad formals list: ~S", vm().list1(formals));
    }
    SCM f = formals;
    size_t i = 0;
    SCM head = SCM_EOL, tail = SCM_EOL;
    while (is_pair(f)) {
        if (i >= nargs) vm().wrong_num_args(nullptr, proc_for_err);
        SCM cell = vm().cons(args[i++], SCM_EOL);
        if (head.is_null()) head = tail = cell;
        else { set_cdr(tail, cell); tail = cell; }
        f = cdr(f);
    }
    if (f.is_null()) {
        if (i != nargs) vm().wrong_num_args(nullptr, proc_for_err);
    } else if (is_symbol(f)) {
        SCM rest = vm().list_n(args + i, nargs - i);
        set_cdr(tail, rest);
    } else {
        vm().misc_error("eval", "Bad formals list: ~S", vm().list1(formals));
    }
    return vm().cons(vm().cons(formals, head), env);
}

// Built-in syntax IDs stored in MacroObj::extra
enum class BuiltinSyntax : uint32_t {
    None = 0,
    Quote,
    Quasiquote,
    Lambda,
    If,
    SetX,
    Define,
    Begin,
    Let,
    LetStar,
    Letrec,
    Cond,
    Case,
    Do,
    And,
    Or,
    Delay,
    At,
    AtAt,
    TheEnvironment,
    DefinedP,
    EvalCase,
};

BuiltinSyntax get_builtin_syntax(SCM op_val) {
    if (is_macro(op_val) && as_obj<MacroObj>(op_val)->kind == MacroKind::BuiltinMacro) {
        return static_cast<BuiltinSyntax>(as_obj<MacroObj>(op_val)->extra);
    }
    return BuiltinSyntax::None;
}

bool is_syntax_sym(SCM sym, BuiltinSyntax expected, SCM env) {
    if (is_macro(sym)) return get_builtin_syntax(sym) == expected;
    if (!is_symbol(sym)) return false;
    SCM val = lookup_env_opt(sym, env);
    return get_builtin_syntax(val) == expected;
}

// Desugar (define (f a b) body...) -> (define f (lambda (a b) body...))
// Also handles curried (define ((f a) b) body...)
void desugar_define(SCM form, SCM& out_name, SCM& out_expr) {
    SCM rest = cdr(form);
    if (!is_pair(rest)) vm().error(vm().sym_syntax_error, "define", "Missing expression in ~S", vm().list1(form));
    SCM target = car(rest);
    SCM body = cdr(rest);
    if (is_symbol(target)) {
        out_name = target;
        if (body.is_null()) {
            out_expr = SCM_UNSPECIFIED;
        } else if (is_pair(body) && cdr(body).is_null()) {
            out_expr = car(body);
        } else {
            vm().error(vm().sym_syntax_error, "define", "Missing or extra expression in ~S", vm().list1(form));
        }
        return;
    }
    if (is_pair(target)) {
        if (body.is_null()) vm().error(vm().sym_syntax_error, "define", "Missing expression in ~S", vm().list1(form));
        SCM expr = vm().cons(vm().sym_lambda, vm().cons(cdr(target), body));
        target = car(target);
        while (is_pair(target)) {
            expr = vm().list3(vm().sym_lambda, cdr(target), expr);
            target = car(target);
        }
        if (!is_symbol(target)) vm().error(vm().sym_syntax_error, "define", "Bad variable in ~S", vm().list1(form));
        out_name = target;
        out_expr = expr;
        return;
    }
    vm().error(vm().sym_syntax_error, "define", "Bad variable in ~S", vm().list1(form));
}

// Check for internal definitions at the start of a body; if present, wrap in (letrec (...) rest...)
// Returns true if transformed into a single letrec expression `out_expr`.
bool transform_internal_defines(SCM body, SCM env, SCM& out_expr) {
    if (!is_pair(body)) return false;
    auto maybe_expand_macro = [&](SCM form) -> SCM {
        int limit = 16;
        while (limit-- > 0 && is_pair(form) && (is_symbol(car(form)) || is_macro(car(form)))) {
            SCM op = is_symbol(car(form)) ? lookup_env_opt(car(form), env) : car(form);
            if (is_macro(op) && as_obj<MacroObj>(op)->kind != MacroKind::BuiltinMacro &&
                as_obj<MacroObj>(op)->kind != MacroKind::Syntax) {
                form = vm().call2(as_obj<MacroObj>(op)->transformer, form, env);
            } else {
                break;
            }
        }
        return form;
    };
    auto starts_with_def = [&](SCM e) -> bool {
        e = maybe_expand_macro(e);
        if (!is_pair(e)) return false;
        SCM h = car(e);
        if (is_syntax_sym(h, BuiltinSyntax::Define, env)) return true;
        if (is_syntax_sym(h, BuiltinSyntax::Begin, env)) {
            SCM b = cdr(e);
            while (is_pair(b)) {
                SCM sub = maybe_expand_macro(car(b));
                if (is_pair(sub) && is_syntax_sym(car(sub), BuiltinSyntax::Define, env)) return true;
                b = cdr(b);
            }
        }
        return false;
    };
    if (!starts_with_def(car(body))) return false;

    SCM bindings_rev = SCM_EOL;
    SCM cur = body;
    // Flatten leading (define ...), (begin (define ...) ...), and macros expanding to them
    std::vector<SCM> queue;
    while (is_pair(cur)) {
        queue.push_back(car(cur));
        cur = cdr(cur);
    }
    size_t idx = 0;
    while (idx < queue.size()) {
        SCM form = maybe_expand_macro(queue[idx]);
        if (is_pair(form) && is_syntax_sym(car(form), BuiltinSyntax::Define, env)) {
            SCM nm = SCM_BOOL_F, ex = SCM_BOOL_F;
            desugar_define(form, nm, ex);
            bindings_rev = vm().cons(vm().list2(nm, ex), bindings_rev);
            ++idx;
        } else if (is_pair(form) && is_syntax_sym(car(form), BuiltinSyntax::Begin, env)) {
            SCM sub = cdr(form);
            if (sub.is_null()) { ++idx; continue; }
            std::vector<SCM> spliced;
            while (is_pair(sub)) { spliced.push_back(car(sub)); sub = cdr(sub); }
            if (!spliced.empty() && starts_with_def(spliced[0])) {
                queue.erase(queue.begin() + static_cast<ptrdiff_t>(idx));
                queue.insert(queue.begin() + static_cast<ptrdiff_t>(idx), spliced.begin(), spliced.end());
            } else {
                break;
            }
        } else {
            break;
        }
    }
    if (bindings_rev.is_null()) return false;
    SCM rem_body = SCM_EOL;
    for (size_t i = queue.size(); i > idx; --i) {
        rem_body = vm().cons(queue[i - 1], rem_body);
    }
    if (rem_body.is_null()) {
        vm().error(vm().sym_syntax_error, "define", "Missing body expression in ~S", vm().list1(body));
    }
    SCM bindings = scm_reverse(bindings_rev);
    out_expr = vm().cons(vm().sym_letrec, vm().cons(bindings, rem_body));
    return true;
}

// ============================================================================
// Dynamic-Wind Switching (Full Multi-Shot Call/CC + Fluids + Dynamic State)
// ============================================================================

void swap_fluid_node(DynWindObj* dw, bool unwinding) {
    if (dw->kind == DynWindKind::Fluid) {
        SCM fl = dw->a;
        SCM cur_val = vm().fluid_ref(fl);
        if (unwinding) {
            // Exiting extent: save current value in b, restore old value from c
            dw->b = cur_val;
            vm().fluid_set(fl, dw->c);
        } else {
            // Re-entering extent: save current value in c, restore extent value from b
            dw->c = cur_val;
            vm().fluid_set(fl, dw->b);
        }
    } else if (dw->kind == DynWindKind::DynState) {
        SCM cur_ds = vm().current_dynamic_state();
        if (unwinding) {
            dw->a = cur_ds;
            vm().set_current_dynamic_state(dw->b);
        } else {
            dw->b = cur_ds;
            vm().set_current_dynamic_state(dw->a);
        }
    }
}

void switch_dynwind(SCM target_dw) {
    SCM cur = vm().current_dynwind;
    if (cur == target_dw) return;

    // Fast path: unwinding a single dynwind frame (99.9% of call/cc escapes inside dynamic-wind!)
    if (GCPP_LIKELY(is_a<DynWindObj>(cur, ObjTag::DynWind) && as_obj<DynWindObj>(cur)->parent == target_dw)) {
        auto* dw = as_obj<DynWindObj>(cur);
        vm().current_dynwind = target_dw;
        if (dw->kind == DynWindKind::Wind) {
            SCM after = dw->b;
            if (GCPP_LIKELY(is_closure(after))) {
                auto* acl = as_obj<ClosureObj>(after);
                if (GCPP_LIKELY(is_bytecode(acl->bytecode))) {
                    bytecode_run_closure(acl, nullptr, 0);
                    return;
                }
                if (acl->bytecode.is_false() && bytecode_should_compile(acl) && bytecode_compile_closure(acl)) {
                    bytecode_run_closure(acl, nullptr, 0);
                    return;
                }
            }
            vm().call0(after);
        } else if (dw->kind == DynWindKind::Fluid || dw->kind == DynWindKind::DynState) {
            swap_fluid_node(dw, true);
        } else if (dw->kind == DynWindKind::FlagReset) {
            if (is_a<DynWindObj>(dw->a, ObjTag::DynWind)) {
                as_obj<DynWindObj>(dw->a)->flags &= ~1U;
            }
        }
        return;
    }

    // Collect ancestors of target_dw
    std::vector<SCM> target_path;
    SCM t = target_dw;
    while (is_a<DynWindObj>(t, ObjTag::DynWind)) {
        target_path.push_back(t);
        t = as_obj<DynWindObj>(t)->parent;
    }

    // Find LCA by walking cur up
    SCM lca = SCM_EOL;
    SCM c = cur;
    std::vector<SCM> unwind_list;
    while (is_a<DynWindObj>(c, ObjTag::DynWind)) {
        auto it = std::find(target_path.begin(), target_path.end(), c);
        if (it != target_path.end()) {
            lca = c;
            target_path.erase(it, target_path.end());
            break;
        }
        unwind_list.push_back(c);
        c = as_obj<DynWindObj>(c)->parent;
    }
    if (lca.is_null()) {
        // target_path has full path from target down to root
    }

    // 1. Unwind from cur to lca
    for (SCM node : unwind_list) {
        auto* dw = as_obj<DynWindObj>(node);
        vm().current_dynwind = dw->parent;
        if (dw->kind == DynWindKind::Wind) {
            vm().call0(dw->b); // after thunk
        } else if (dw->kind == DynWindKind::Fluid || dw->kind == DynWindKind::DynState) {
            swap_fluid_node(dw, true);
        } else if (dw->kind == DynWindKind::FlagReset) {
            if (is_a<DynWindObj>(dw->a, ObjTag::DynWind)) {
                as_obj<DynWindObj>(dw->a)->flags &= ~1U;
            }
        }
    }

    // 2. Rewind from lca down to target_dw (reverse order of target_path)
    for (auto it = target_path.rbegin(); it != target_path.rend(); ++it) {
        SCM node = *it;
        auto* dw = as_obj<DynWindObj>(node);
        if (dw->kind == DynWindKind::Wind) {
            vm().call0(dw->a); // before thunk (runs in outer dynwind!)
            vm().current_dynwind = node;
        } else if (dw->kind == DynWindKind::Fluid || dw->kind == DynWindKind::DynState) {
            swap_fluid_node(dw, false);
            vm().current_dynwind = node;
        } else if (dw->kind == DynWindKind::FlagReset) {
            if (is_a<DynWindObj>(dw->a, ObjTag::DynWind)) {
                as_obj<DynWindObj>(dw->a)->flags |= 1U;
            }
            vm().current_dynwind = node;
        } else {
            vm().current_dynwind = node;
        }
    }
}

// ============================================================================
// Quasiquote Recursive Evaluator (R5RS + nested quasiquotes + vectors)
// ============================================================================

SCM eval_quasiquote(SCM form, SCM env, int depth) {
    if (is_pair(form)) {
        SCM head = car(form);
        if (head == vm().sym_unquote) {
            if (!is_pair(cdr(form)) || !cdr(cdr(form)).is_null()) {
                vm().error(vm().sym_syntax_error, "quasiquote", "Missing or extra expression in ~S", vm().list1(form));
            }
            if (depth == 0) {
                return vm().eval(car(cdr(form)), env);
            }
            return vm().list2(vm().sym_unquote, eval_quasiquote(car(cdr(form)), env, depth - 1));
        }
        if (head == vm().sym_quasiquote && is_pair(cdr(form)) && cdr(cdr(form)).is_null()) {
            return vm().list2(vm().sym_quasiquote, eval_quasiquote(car(cdr(form)), env, depth + 1));
        }
        // Check for unquote-splicing in list position
        if (is_pair(head) && car(head) == vm().sym_unquote_splicing) {
            if (!is_pair(cdr(head)) || !cdr(cdr(head)).is_null()) {
                vm().error(vm().sym_syntax_error, "quasiquote", "Missing or extra expression in ~S", vm().list1(head));
            }
            if (depth == 0) {
                SCM spliced = vm().eval(car(cdr(head)), env);
                SCM tail = eval_quasiquote(cdr(form), env, 0);
                if (tail.is_null()) return spliced;
                return scm_append2(spliced, tail);
            }
            SCM a = vm().list2(vm().sym_unquote_splicing, eval_quasiquote(car(cdr(head)), env, depth - 1));
            return vm().cons(a, eval_quasiquote(cdr(form), env, depth));
        }
        SCM a = eval_quasiquote(head, env, depth);
        SCM d = eval_quasiquote(cdr(form), env, depth);
        return vm().cons(a, d);
    }
    if (is_vector(form)) {
        auto* v = as_obj<VectorObj>(form);
        ScmRootVec out(vm().heap());
        for (size_t i = 0; i < v->elts.size(); ++i) {
            SCM e = v->elts[i];
            if (is_pair(e) && car(e) == vm().sym_unquote_splicing) {
                if (!is_pair(cdr(e)) || !cdr(cdr(e)).is_null()) {
                    vm().error(vm().sym_syntax_error, "quasiquote", "Missing or extra expression in ~S", vm().list1(e));
                }
                if (depth == 0) {
                    SCM spliced = vm().eval(car(cdr(e)), env);
                    while (is_pair(spliced)) {
                        out.push_back(car(spliced));
                        spliced = cdr(spliced);
                    }
                    if (!spliced.is_null()) vm().wrong_type_arg("quasiquote", 1, spliced, "proper list");
                } else {
                    out.push_back(vm().list2(vm().sym_unquote_splicing, eval_quasiquote(car(cdr(e)), env, depth - 1)));
                }
            } else {
                out.push_back(eval_quasiquote(e, env, depth));
            }
        }
        return vm().heap().alloc_scm<VectorObj>(out.vec);
    }
    return form;
}

// ============================================================================
// CEK Machine Core Loop
// ============================================================================

enum class StepMode { Eval, ApplyVal, ApplyProc };

struct CekState {
    StepMode mode = StepMode::Eval;
    SCM expr = SCM_UNSPECIFIED;
    SCM env = SCM_EOL;
    SCM val = SCM_UNSPECIFIED;
    SCM proc = SCM_BOOL_F;
    std::vector<SCM> args;
    SCM cont = SCM_EOL;
};

void dispatch_throw(VM& v, CekState& st, SCM key, SCM throw_args, SCM base_dw, int my_depth) {
    // Search current_dynwind for matching Catch, LazyCatch, or ThrowHandler.
    // Note: ThrowHandlers run WITHOUT unwinding even if they live above base_dw,
    // so if a ThrowHandler throws to a catch lexically inside it, that inner catch is still active!
    while (true) {
        SCM dw_cur = v.current_dynwind;
        SCM found = SCM_BOOL_F;
        bool passed_base = false;
        bool restarted = false;
        while (is_a<DynWindObj>(dw_cur, ObjTag::DynWind)) {
            if (dw_cur == base_dw) passed_base = true;
            auto* dw = as_obj<DynWindObj>(dw_cur);
            if ((dw->kind == DynWindKind::Catch || dw->kind == DynWindKind::LazyCatch || dw->kind == DynWindKind::ThrowHandler) &&
                (dw->a == SCM_BOOL_T || dw->a == key)) {
                if ((dw->kind == DynWindKind::ThrowHandler || dw->kind == DynWindKind::LazyCatch) && (dw->flags & 1U)) {
                    dw_cur = dw->parent;
                    continue;
                }
                if (dw->kind == DynWindKind::ThrowHandler && passed_base) {
                    // Run outer ThrowHandler immediately without unwinding current C++ stack!
                    dw->flags |= 1U;
                    SCM fr = v.heap().alloc_scm<DynWindObj>(DynWindKind::FlagReset, dw_cur, SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F, v.current_dynwind, v.next_dw_id++);
                    v.current_dynwind = fr;
                    SCM stk = build_stack_from_cont(st.cont);
                    v.fluid_set(v.the_last_stack_fluid, stk);
                    std::vector<SCM> hargs;
                    hargs.push_back(key);
                    SCM a = throw_args;
                    while (is_pair(a)) { hargs.push_back(car(a)); a = cdr(a); }
                    try {
                        v.apply(dw->b, hargs.data(), hargs.size());
                    } catch (const SchemeThrow& inner_ex) {
                        key = inner_ex.key;
                        throw_args = inner_ex.args;
                        restarted = true;
                        break;
                    }
                    // Returned normally: keep FlagReset & running bit set until an outer catch unwinds!
                    dw_cur = dw->parent;
                    continue;
                }
                if (passed_base) {
                    // Matching Catch/LazyCatch lives in an outer run_cek frame
                    break;
                }
                found = dw_cur;
                break;
            }
            dw_cur = dw->parent;
        }
        if (restarted) continue;
        if (found.is_false()) {
            throw SchemeThrow(key, throw_args);
        }
        auto* dw = as_obj<DynWindObj>(found);
        if (dw->kind == DynWindKind::LazyCatch) {
            // Unwind up to dw, unlink dw, run handler in CEK with RethrowAfter!
            switch_dynwind(found);
            v.current_dynwind = dw->parent;
            SCM stk = build_stack_from_cont(st.cont);
            v.fluid_set(v.the_last_stack_fluid, stk);
            auto* rcf = v.heap().alloc<ContFrameObj>(ContKind::RethrowAfter, st.cont);
            rcf->aux = key;
            rcf->aux2 = throw_args;
            st.cont = SCM::from_obj(rcf);
            st.mode = StepMode::ApplyProc;
            st.proc = dw->b;
            st.args.clear();
            st.args.push_back(key);
            SCM a = throw_args;
            while (is_pair(a)) { st.args.push_back(car(a)); a = cdr(a); }
            return;
        }
        if (dw->kind == DynWindKind::ThrowHandler) {
            // Run handler WITHOUT unwinding; mark running and push FlagReset dynwind guard (stays until non-local exit!)
            dw->flags |= 1U;
            SCM fr = v.heap().alloc_scm<DynWindObj>(DynWindKind::FlagReset, found, SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F, v.current_dynwind, v.next_dw_id++);
            v.current_dynwind = fr;
            SCM stk = build_stack_from_cont(st.cont);
            v.fluid_set(v.the_last_stack_fluid, stk);
            auto* rcf = v.heap().alloc<ContFrameObj>(ContKind::RethrowAfter, st.cont);
            rcf->aux = key;
            rcf->aux2 = throw_args;
            st.cont = SCM::from_obj(rcf);
            st.mode = StepMode::ApplyProc;
            st.proc = dw->b;
            st.args.clear();
            st.args.push_back(key);
            SCM a = throw_args;
            while (is_pair(a)) { st.args.push_back(car(a)); a = cdr(a); }
            return;
        }
        // Regular catch:
        // 1. Optional pre-unwind handler (runs BEFORE unwinding in CEK with running flag!)
        if (dw->c.is_true() && v.is_procedure(dw->c) && !(dw->flags & 1U)) {
            dw->flags |= 1U;
            SCM fr = v.heap().alloc_scm<DynWindObj>(DynWindKind::FlagReset, found, SCM_BOOL_F, SCM_BOOL_F, SCM_BOOL_F, v.current_dynwind, v.next_dw_id++);
            v.current_dynwind = fr;
            SCM stk = build_stack_from_cont(st.cont);
            v.fluid_set(v.the_last_stack_fluid, stk);
            auto* pcf = v.heap().alloc<ContFrameObj>(ContKind::PreUnwindAfter, st.cont);
            pcf->aux = found;
            pcf->aux2 = key;
            pcf->aux3 = throw_args;
            st.cont = SCM::from_obj(pcf);
            st.mode = StepMode::ApplyProc;
            st.proc = dw->c;
            st.args.clear();
            st.args.push_back(key);
            SCM a = throw_args;
            while (is_pair(a)) { st.args.push_back(car(a)); a = cdr(a); }
            return;
        }
        // 2. Unwind dynamic-wind chain to dw->parent (also resets any FlagReset nodes!)
        switch_dynwind(dw->parent);
        // 3. Jump to catch's saved continuation and tail-apply handler to (cons key throw_args)
        st.cont = dw->d;
        st.mode = StepMode::ApplyProc;
        st.proc = dw->b;
        st.args.clear();
        st.args.push_back(key);
        SCM a = throw_args;
        while (is_pair(a)) {
            st.args.push_back(car(a));
            a = cdr(a);
        }
        return;
    }
}

SCM run_cek(VM& v, CekState& st) {
    v.check_c_stack();
    SCM base_dw = v.current_dynwind;
    int my_depth = ++v.trampoline_depth;
    // Root st.args across GC!
    GcRootSpan args_span{st.args.data(), st.args.size(), nullptr};
    v.heap().push_root_span(&args_span);
    auto sync_args = [&]() {
        args_span.data = st.args.data();
        args_span.count = st.args.size();
    };

    struct DepthGuard {
        VM& vm_ref;
        SCM saved_cont;
        ~DepthGuard() {
            vm_ref.current_cont = saved_cont;
            vm_ref.heap().pop_root_span();
            --vm_ref.trampoline_depth;
        }
    } dguard{v, v.current_cont};

    while (true) {
        v.current_cont = st.cont;
        sync_args();
        try {
            if (st.mode == StepMode::Eval) {
                SCM expr = st.expr;
                SCM env = st.env;

                if (is_symbol(expr)) {
                    auto loc = lookup_binding(expr, env, false);
                    st.val = get_binding_value(loc, expr);
                    st.mode = StepMode::ApplyVal;
                    continue;
                }
                if (is_variable(expr)) {
                    SCM val = as_obj<VariableObj>(expr)->value;
                    if (val.is_undefined()) {
                        v.error(v.sym_unbound_variable, nullptr, "Unbound variable: ~S", v.list1(expr), SCM_BOOL_F);
                    }
                    st.val = val;
                    st.mode = StepMode::ApplyVal;
                    continue;
                }
                if (expr.is_null()) {
                    v.error(v.sym_syntax_error, nullptr, "Illegal empty combination: ~S", v.list1(expr));
                }
                if (!is_pair(expr)) {
                    // Self-evaluating (numbers, strings, chars, booleans, vectors, keywords, etc.)
                    st.val = expr;
                    st.mode = StepMode::ApplyVal;
                    continue;
                }

                // Pair expression: (op . args)
                SCM op_expr = car(expr);
                SCM arg_exprs = cdr(expr);

                if (is_symbol(op_expr) || is_macro(op_expr)) {
                    SCM op_val = is_symbol(op_expr) ? lookup_env_opt(op_expr, env) : op_expr;
                    if (is_macro(op_val)) {
                        auto* mac = as_obj<MacroObj>(op_val);
                        if (mac->kind == MacroKind::BuiltinMacro) {
                            auto bs = static_cast<BuiltinSyntax>(mac->extra);
                            switch (bs) {
                            case BuiltinSyntax::Quote: {
                                if (!is_pair(arg_exprs) || !cdr(arg_exprs).is_null()) {
                                    v.error(v.sym_syntax_error, "quote", "Missing or extra expression in ~S", v.list1(expr));
                                }
                                st.val = car(arg_exprs);
                                st.mode = StepMode::ApplyVal;
                                continue;
                            }
                            case BuiltinSyntax::Quasiquote: {
                                if (!is_pair(arg_exprs) || !cdr(arg_exprs).is_null()) {
                                    v.error(v.sym_syntax_error, "quasiquote", "Missing or extra expression in ~S", v.list1(expr));
                                }
                                st.val = eval_quasiquote(car(arg_exprs), env, 0);
                                st.mode = StepMode::ApplyVal;
                                continue;
                            }
                            case BuiltinSyntax::Lambda: {
                                if (arg_exprs.is_null()) {
                                    v.error(v.sym_syntax_error, "lambda", "Missing expression in ~S", v.list1(expr));
                                }
                                if (!is_pair(arg_exprs)) {
                                    v.error(v.sym_syntax_error, "lambda", "Bad expression ~S", v.list1(expr));
                                }
                                SCM formals = car(arg_exprs);
                                SCM body = cdr(arg_exprs);
                                if (body.is_null()) {
                                    v.error(v.sym_syntax_error, "lambda", "Missing expression in ~S", v.list1(expr));
                                }
                                if (!is_pair(body)) {
                                    v.error(v.sym_syntax_error, "lambda", "Bad expression ~S", v.list1(expr));
                                }
                                if (!formals.is_null() && !is_symbol(formals)) {
                                    if (!is_pair(formals)) {
                                        v.error(v.sym_syntax_error, "lambda", "Bad formals in ~S", v.list1(expr));
                                    }
                                    std::vector<SCM> seen;
                                    SCM f = formals;
                                    while (is_pair(f)) {
                                        SCM sym = car(f);
                                        if (!is_symbol(sym)) {
                                            v.error(v.sym_syntax_error, "lambda", "Bad formal ~S in ~S", v.list2(sym, expr));
                                        }
                                        for (SCM s : seen) {
                                            if (s == sym) v.error(v.sym_syntax_error, "lambda", "Duplicate formal ~S in ~S", v.list2(sym, expr));
                                        }
                                        seen.push_back(sym);
                                        f = cdr(f);
                                    }
                                    if (!f.is_null()) {
                                        if (!is_symbol(f)) {
                                            v.error(v.sym_syntax_error, "lambda", "Bad formal ~S in ~S", v.list2(f, expr));
                                        }
                                        for (SCM s : seen) {
                                            if (s == f) v.error(v.sym_syntax_error, "lambda", "Duplicate formal ~S in ~S", v.list2(f, expr));
                                        }
                                    }
                                }
                                st.val = v.make_closure(formals, body, env);
                                st.mode = StepMode::ApplyVal;
                                continue;
                            }
                            case BuiltinSyntax::If: {
                                if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs))) {
                                    v.error(v.sym_syntax_error, "if", "Missing or extra expression in ~S", v.list1(expr));
                                }
                                SCM test_e = car(arg_exprs);
                                SCM conseq = car(cdr(arg_exprs));
                                SCM rest2 = cdr(cdr(arg_exprs));
                                SCM alt = SCM_UNSPECIFIED;
                                bool has_alt = false;
                                if (is_pair(rest2)) {
                                    if (!cdr(rest2).is_null()) v.error(v.sym_syntax_error, "if", "Missing or extra expression in ~S", v.list1(expr));
                                    alt = car(rest2);
                                    has_alt = true;
                                } else if (!rest2.is_null()) {
                                    v.error(v.sym_syntax_error, "if", "Bad expression ~S", v.list1(expr));
                                }
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::If, st.cont);
                                cf->env = env;
                                cf->aux = conseq;
                                cf->expr = alt;
                                cf->aux2 = SCM::from_bool(has_alt);
                                st.cont = SCM::from_obj(cf);
                                st.expr = test_e;
                                continue;
                            }
                            case BuiltinSyntax::Begin: {
                                if (arg_exprs.is_null()) {
                                    st.val = SCM_UNSPECIFIED;
                                    st.mode = StepMode::ApplyVal;
                                    continue;
                                }
                                if (!is_pair(arg_exprs)) v.error(v.sym_syntax_error, "begin", "Bad expression ~S", v.list1(expr));
                                if (cdr(arg_exprs).is_null()) {
                                    st.expr = car(arg_exprs);
                                    continue;
                                }
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                                cf->env = env;
                                cf->expr = cdr(arg_exprs);
                                // Only track current-module changes for top-level `begin`
                                // whose env is the active current-module eval-closure
                                // (e.g. `(begin (define-module ...) ...)`), never for closure
                                // bodies or explicit cross-module `(eval '(begin ...) mod)`.
                                if (v.module_system_booted && is_pair(env) &&
                                    is_eval_closure(car(env)) && cdr(env).is_null() &&
                                    car(env) == v.current_module_lookup_closure()) {
                                    cf->aux = car(env);
                                }
                                st.cont = SCM::from_obj(cf);
                                st.expr = car(arg_exprs);
                                continue;
                            }
                            case BuiltinSyntax::SetX: {
                                if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs)) || !cdr(cdr(arg_exprs)).is_null()) {
                                    v.error(v.sym_syntax_error, "set!", "Missing or extra expression in ~S", v.list1(expr));
                                }
                                SCM target = car(arg_exprs);
                                SCM val_expr = car(cdr(arg_exprs));
                                // If target is a macro call (e.g. (@ (mod) var) or (@@ (mod) var)), expand it first!
                                if (is_pair(target) && (is_symbol(car(target)) || is_macro(car(target)))) {
                                    SCM mv = is_symbol(car(target)) ? lookup_env_opt(car(target), env) : car(target);
                                    if (is_macro(mv)) {
                                        if (as_obj<MacroObj>(mv)->kind == MacroKind::BuiltinMacro) {
                                            v.error(v.sym_syntax_error, "set!", "Bad variable in set!: ~S", v.list1(target));
                                        }
                                        SCM expanded = v.call2(as_obj<MacroObj>(mv)->transformer, target, env);
                                        if (is_variable(expanded) || is_symbol(expanded)) {
                                            target = expanded;
                                        } else {
                                            v.error(v.sym_syntax_error, "set!", "Bad variable in set!: ~S", v.list1(target));
                                        }
                                    }
                                }
                                // SRFI-17 generalized set!: (set! (proc arg...) val) => ((setter proc) arg... val)
                                if (is_pair(target)) {
                                    SCM setter_call = v.cons(v.list2(v.make_symbol("setter"), car(target)),
                                                             scm_append2(cdr(target), v.list1(val_expr)));
                                    st.expr = setter_call;
                                    continue;
                                }
                                if (!is_symbol(target) && !is_variable(target)) {
                                    v.error(v.sym_syntax_error, "set!", "Bad variable in set!: ~S", v.list1(target));
                                }
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::SetVar, st.cont);
                                cf->env = env;
                                cf->expr = target;
                                st.cont = SCM::from_obj(cf);
                                st.expr = val_expr;
                                continue;
                            }
                            case BuiltinSyntax::Define: {
                                SCM nm = SCM_BOOL_F, val_expr = SCM_BOOL_F;
                                desugar_define(expr, nm, val_expr);
                                // Evaluate val_expr BEFORE creating the local binding so (define x x) sees imported x!
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::Define, st.cont);
                                cf->env = env;
                                cf->expr = nm;
                                cf->aux = env_top_level_closure(env);
                                st.cont = SCM::from_obj(cf);
                                st.expr = val_expr;
                                continue;
                            }
                            case BuiltinSyntax::Let: {
                                if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs))) {
                                    v.error(v.sym_syntax_error, "let", "Missing expression in ~S", v.list1(expr));
                                }
                                SCM loop_name = SCM_BOOL_F;
                                SCM bindings = car(arg_exprs);
                                SCM body = cdr(arg_exprs);
                                if (is_symbol(bindings)) {
                                    loop_name = bindings;
                                    if (!is_pair(body) || !is_pair(cdr(body))) {
                                        v.error(v.sym_syntax_error, "let", "Missing expression in ~S", v.list1(expr));
                                    }
                                    bindings = car(body);
                                    body = cdr(body);
                                }
                                if (!bindings.is_null() && !is_pair(bindings)) {
                                    v.error(v.sym_syntax_error, "let", "Bad bindings in ~S", v.list1(expr));
                                }
                                // Validate all bindings & check duplicates up front
                                {
                                    std::vector<SCM> seen;
                                    SCM b = bindings;
                                    while (is_pair(b)) {
                                        SCM entry = car(b);
                                        if (!is_pair(entry) || !is_pair(cdr(entry)) || !cdr(cdr(entry)).is_null()) {
                                            v.error(v.sym_syntax_error, "let", "Bad binding ~S in ~S", v.list2(entry, expr));
                                        }
                                        if (!is_symbol(car(entry))) {
                                            v.error(v.sym_syntax_error, "let", "Bad variable ~S in ~S", v.list2(car(entry), expr));
                                        }
                                        for (SCM s : seen) {
                                            if (s == car(entry)) v.error(v.sym_syntax_error, "let", "Duplicate binding ~S in ~S", v.list2(car(entry), expr));
                                        }
                                        seen.push_back(car(entry));
                                        b = cdr(b);
                                    }
                                    if (!b.is_null()) v.error(v.sym_syntax_error, "let", "Bad bindings in ~S", v.list1(expr));
                                }
                                if (bindings.is_null()) {
                                    if (loop_name.is_true()) {
                                        SCM cl = v.make_closure(SCM_EOL, body, env);
                                        SCM new_env = v.cons(v.cons(v.list1(loop_name), v.list1(cl)), env);
                                        as_obj<ClosureObj>(cl)->env = new_env;
                                        st.mode = StepMode::ApplyProc;
                                        st.proc = cl;
                                        st.args.clear();
                                        continue;
                                    }
                                    SCM transformed;
                                    if (transform_internal_defines(body, env, transformed)) {
                                        st.expr = transformed;
                                    } else if (cdr(body).is_null()) {
                                        st.expr = car(body);
                                    } else {
                                        auto* cf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                                        cf->env = env;
                                        cf->expr = cdr(body);
                                        st.cont = SCM::from_obj(cf);
                                        st.expr = car(body);
                                    }
                                    continue;
                                }
                                SCM first = car(bindings);
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::LetInit, st.cont);
                                cf->env = env;
                                cf->expr = cdr(bindings);
                                cf->aux = body;
                                cf->aux2 = v.list1(car(first)); // names_rev
                                cf->aux3 = loop_name;
                                st.cont = SCM::from_obj(cf);
                                st.expr = car(cdr(first));
                                continue;
                            }
                            case BuiltinSyntax::LetStar: {
                                if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs))) {
                                    v.error(v.sym_syntax_error, "let*", "Missing expression in ~S", v.list1(expr));
                                }
                                SCM bindings = car(arg_exprs);
                                SCM body = cdr(arg_exprs);
                                if (!bindings.is_null() && !is_pair(bindings)) {
                                    v.error(v.sym_syntax_error, "let*", "Bad bindings in ~S", v.list1(expr));
                                }
                                // Validate all bindings up front (duplicates allowed in let*)
                                {
                                    SCM b = bindings;
                                    while (is_pair(b)) {
                                        SCM entry = car(b);
                                        if (!is_pair(entry) || !is_pair(cdr(entry)) || !cdr(cdr(entry)).is_null()) {
                                            v.error(v.sym_syntax_error, "let*", "Bad binding ~S in ~S", v.list2(entry, expr));
                                        }
                                        if (!is_symbol(car(entry))) {
                                            v.error(v.sym_syntax_error, "let*", "Bad variable ~S in ~S", v.list2(car(entry), expr));
                                        }
                                        b = cdr(b);
                                    }
                                    if (!b.is_null()) v.error(v.sym_syntax_error, "let*", "Bad bindings in ~S", v.list1(expr));
                                }
                                if (bindings.is_null()) {
                                    SCM transformed;
                                    if (transform_internal_defines(body, env, transformed)) {
                                        st.expr = transformed;
                                    } else if (cdr(body).is_null()) {
                                        st.expr = car(body);
                                    } else {
                                        auto* cf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                                        cf->env = env;
                                        cf->expr = cdr(body);
                                        st.cont = SCM::from_obj(cf);
                                        st.expr = car(body);
                                    }
                                    continue;
                                }
                                SCM first = car(bindings);
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::LetStarInit, st.cont);
                                cf->env = env;
                                cf->expr = cdr(bindings);
                                cf->aux = body;
                                cf->aux2 = car(first);
                                st.cont = SCM::from_obj(cf);
                                st.expr = car(cdr(first));
                                continue;
                            }
                            case BuiltinSyntax::Letrec: {
                                if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs))) {
                                    v.error(v.sym_syntax_error, "letrec", "Missing expression in ~S", v.list1(expr));
                                }
                                SCM bindings = car(arg_exprs);
                                SCM body = cdr(arg_exprs);
                                if (!bindings.is_null() && !is_pair(bindings)) {
                                    v.error(v.sym_syntax_error, "letrec", "Bad bindings in ~S", v.list1(expr));
                                }
                                if (bindings.is_null()) {
                                    SCM transformed;
                                    if (transform_internal_defines(body, env, transformed)) {
                                        st.expr = transformed;
                                    } else if (cdr(body).is_null()) {
                                        st.expr = car(body);
                                    } else {
                                        auto* cf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                                        cf->env = env;
                                        cf->expr = cdr(body);
                                        st.cont = SCM::from_obj(cf);
                                        st.expr = car(body);
                                    }
                                    continue;
                                }
                                // Pre-allocate frame with SCM_UNDEFINED slots & validate bindings
                                SCM names_h = SCM_EOL, names_t = SCM_EOL;
                                SCM vals_h = SCM_EOL, vals_t = SCM_EOL;
                                std::vector<SCM> seen;
                                SCM b = bindings;
                                while (is_pair(b)) {
                                    SCM entry = car(b);
                                    if (!is_pair(entry) || !is_pair(cdr(entry)) || !cdr(cdr(entry)).is_null()) {
                                        v.error(v.sym_syntax_error, "letrec", "Bad binding ~S in ~S", v.list2(entry, expr));
                                    }
                                    if (!is_symbol(car(entry))) {
                                        v.error(v.sym_syntax_error, "letrec", "Bad variable ~S in ~S", v.list2(car(entry), expr));
                                    }
                                    for (SCM s : seen) {
                                        if (s == car(entry)) v.error(v.sym_syntax_error, "letrec", "Duplicate binding ~S in ~S", v.list2(car(entry), expr));
                                    }
                                    seen.push_back(car(entry));
                                    SCM nc = v.cons(car(entry), SCM_EOL);
                                    SCM vc = v.cons(SCM_UNDEFINED, SCM_EOL);
                                    if (names_h.is_null()) { names_h = names_t = nc; vals_h = vals_t = vc; }
                                    else { set_cdr(names_t, nc); names_t = nc; set_cdr(vals_t, vc); vals_t = vc; }
                                    b = cdr(b);
                                }
                                if (!b.is_null()) v.error(v.sym_syntax_error, "letrec", "Bad bindings in ~S", v.list1(expr));
                                SCM ext_env = v.cons(v.cons(names_h, vals_h), env);
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::LetrecInit, st.cont);
                                cf->env = ext_env;
                                cf->expr = cdr(bindings);
                                cf->aux = body;
                                cf->aux2 = vals_h; // target cell for current binding
                                st.cont = SCM::from_obj(cf);
                                st.env = ext_env;
                                st.expr = car(cdr(car(bindings)));
                                continue;
                            }
                            case BuiltinSyntax::And: {
                                if (arg_exprs.is_null()) {
                                    st.val = SCM_BOOL_T;
                                    st.mode = StepMode::ApplyVal;
                                    continue;
                                }
                                if (cdr(arg_exprs).is_null()) {
                                    st.expr = car(arg_exprs);
                                    continue;
                                }
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::And, st.cont);
                                cf->env = env;
                                cf->expr = cdr(arg_exprs);
                                st.cont = SCM::from_obj(cf);
                                st.expr = car(arg_exprs);
                                continue;
                            }
                            case BuiltinSyntax::Or: {
                                if (arg_exprs.is_null()) {
                                    st.val = SCM_BOOL_F;
                                    st.mode = StepMode::ApplyVal;
                                    continue;
                                }
                                if (cdr(arg_exprs).is_null()) {
                                    st.expr = car(arg_exprs);
                                    continue;
                                }
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::Or, st.cont);
                                cf->env = env;
                                cf->expr = cdr(arg_exprs);
                                st.cont = SCM::from_obj(cf);
                                st.expr = car(arg_exprs);
                                continue;
                            }
                            case BuiltinSyntax::Cond: {
                                if (arg_exprs.is_null()) {
                                    v.error(v.sym_syntax_error, "cond", "Missing clauses in ~S", v.list1(expr));
                                }
                                // Validate all clauses up front
                                {
                                    bool arrow_unbound = lookup_env_opt(v.sym_arrow, env).is_undefined();
                                    bool else_unbound = lookup_env_opt(v.sym_else, env).is_undefined();
                                    SCM cl_it = arg_exprs;
                                    while (is_pair(cl_it)) {
                                        SCM cl = car(cl_it);
                                        if (!is_pair(cl)) {
                                            v.error(v.sym_syntax_error, "cond", "Bad cond clause ~S in ~S", v.list2(cl, expr));
                                        }
                                        if (else_unbound && car(cl) == v.sym_else) {
                                            if (cdr(cl).is_null()) v.error(v.sym_syntax_error, "cond", "Bad cond clause ~S in ~S", v.list2(cl, expr));
                                            if (!cdr(cl_it).is_null()) {
                                                if (is_pair(cdr(cl_it))) v.error(v.sym_syntax_error, "cond", "Misplaced else clause in ~S", v.list1(expr));
                                                v.error(v.sym_syntax_error, "cond", "Bad expression ~S", v.list1(expr));
                                            }
                                        } else if (arrow_unbound && is_pair(cdr(cl))) {
                                            if (car(cdr(cl)) == v.sym_arrow) {
                                                SCM r = cdr(cdr(cl));
                                                if (r.is_null()) v.error(v.sym_syntax_error, "cond", "Missing recipient in ~S", v.list1(cl));
                                                if (!is_pair(r)) v.error(v.sym_syntax_error, "cond", "Bad cond clause ~S", v.list1(cl));
                                                if (!cdr(r).is_null()) v.error(v.sym_syntax_error, "cond", "Extra expression in ~S", v.list1(cl));
                                            } else if (is_pair(cdr(cdr(cl))) && car(cdr(cdr(cl))) == v.sym_arrow) {
                                                // SRFI-61: (generator guard => receiver)
                                                SCM r = cdr(cdr(cdr(cl)));
                                                if (r.is_null()) v.error(v.sym_syntax_error, "cond", "Missing recipient in ~S", v.list1(cl));
                                                if (!is_pair(r)) v.error(v.sym_syntax_error, "cond", "Bad cond clause ~S", v.list1(cl));
                                                if (!cdr(r).is_null()) v.error(v.sym_syntax_error, "cond", "Extra expression in ~S", v.list1(cl));
                                            }
                                        }
                                        cl_it = cdr(cl_it);
                                    }
                                    if (!cl_it.is_null()) v.error(v.sym_syntax_error, "cond", "Bad expression ~S", v.list1(expr));
                                }
                                SCM clause = car(arg_exprs);
                                if (car(clause) == v.sym_else && lookup_env_opt(v.sym_else, env).is_undefined()) {
                                    SCM body = cdr(clause);
                                    if (cdr(body).is_null()) { st.expr = car(body); continue; }
                                    auto* cf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                                    cf->env = env;
                                    cf->expr = cdr(body);
                                    st.cont = SCM::from_obj(cf);
                                    st.expr = car(body);
                                    continue;
                                }
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::CondClause, st.cont);
                                cf->env = env;
                                cf->expr = clause;
                                cf->aux = cdr(arg_exprs);
                                st.cont = SCM::from_obj(cf);
                                st.expr = car(clause);
                                continue;
                            }
                            case BuiltinSyntax::Case: {
                                if (arg_exprs.is_null()) v.error(v.sym_syntax_error, "case", "Missing clauses in ~S", v.list1(expr));
                                if (!is_pair(arg_exprs)) v.error(v.sym_syntax_error, "case", "Bad expression ~S", v.list1(expr));
                                SCM clauses = cdr(arg_exprs);
                                if (clauses.is_null()) v.error(v.sym_syntax_error, "case", "Missing clauses in ~S", v.list1(expr));
                                if (!is_pair(clauses)) v.error(v.sym_syntax_error, "case", "Bad expression ~S", v.list1(expr));
                                {
                                    bool else_unbound = lookup_env_opt(v.sym_else, env).is_undefined();
                                    SCM c = clauses;
                                    while (is_pair(c)) {
                                        SCM cl = car(c);
                                        if (!is_pair(cl) || !is_pair(cdr(cl))) {
                                            v.error(v.sym_syntax_error, "case", "Bad case clause ~S in ~S", v.list2(cl, expr));
                                        }
                                        SCM labels = car(cl);
                                        if (else_unbound && labels == v.sym_else) {
                                            if (!cdr(c).is_null()) {
                                                if (is_pair(cdr(c))) v.error(v.sym_syntax_error, "case", "Misplaced else clause in ~S", v.list1(expr));
                                                v.error(v.sym_syntax_error, "case", "Bad expression ~S", v.list1(expr));
                                            }
                                        } else {
                                            SCM l = labels;
                                            while (is_pair(l)) l = cdr(l);
                                            if (!l.is_null()) {
                                                v.error(v.sym_syntax_error, "case", "Bad case labels ~S in ~S", v.list2(labels, expr));
                                            }
                                        }
                                        c = cdr(c);
                                    }
                                    if (!c.is_null()) v.error(v.sym_syntax_error, "case", "Bad expression ~S", v.list1(expr));
                                }
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::CaseKey, st.cont);
                                cf->env = env;
                                cf->expr = clauses;
                                st.cont = SCM::from_obj(cf);
                                st.expr = car(arg_exprs);
                                continue;
                            }
                            case BuiltinSyntax::Do: {
                                // (do ((var init [step]) ...) (test expr ...) command ...)
                                if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs))) {
                                    v.error(v.sym_syntax_error, "do", "Bad do form: ~S", v.list1(expr));
                                }
                                SCM var_specs = car(arg_exprs);
                                SCM test_and_cmds = cdr(arg_exprs);
                                // Reduce redundant (var init var) step to (var init) for Guile 1.8 unmemoization compatibility
                                {
                                    SCM vs = var_specs;
                                    while (is_pair(vs)) {
                                        SCM sp = car(vs);
                                        if (is_pair(sp) && is_pair(cdr(sp)) && is_pair(cdr(cdr(sp))) &&
                                            cdr(cdr(cdr(sp))).is_null() && car(cdr(cdr(sp))) == car(sp)) {
                                            set_cdr(cdr(sp), SCM_EOL);
                                        }
                                        vs = cdr(vs);
                                    }
                                }
                                if (var_specs.is_null()) {
                                    auto* cf = v.heap().alloc<ContFrameObj>(ContKind::DoTest, st.cont);
                                    cf->env = env;
                                    cf->expr = arg_exprs;
                                    st.cont = SCM::from_obj(cf);
                                    st.expr = car(car(test_and_cmds));
                                    continue;
                                }
                                SCM first = car(var_specs);
                                if (!is_pair(first) || !is_pair(cdr(first))) {
                                    v.error(v.sym_syntax_error, "do", "Bad do variable spec: ~S", v.list1(first));
                                }
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::DoInit, st.cont);
                                cf->env = env;
                                cf->expr = cdr(var_specs);
                                cf->aux = arg_exprs;
                                st.cont = SCM::from_obj(cf);
                                st.expr = car(cdr(first));
                                continue;
                            }
                            case BuiltinSyntax::Delay: {
                                if (!is_pair(arg_exprs) || !cdr(arg_exprs).is_null()) {
                                    v.error(v.sym_syntax_error, "delay", "Bad delay form");
                                }
                                st.val = v.heap().alloc_scm<PromiseObj>(car(arg_exprs), env);
                                st.mode = StepMode::ApplyVal;
                                continue;
                            }
                            case BuiltinSyntax::At:
                            case BuiltinSyntax::AtAt: {
                                // (@ (mod name) var) or (@@ (mod name) var)
                                if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs)) || !cdr(cdr(arg_exprs)).is_null()) {
                                    v.error(v.sym_syntax_error, "@", "Bad @ form: ~S", v.list1(expr));
                                }
                                SCM mod_name = car(arg_exprs);
                                SCM sym = car(cdr(arg_exprs));
                                SCM mod = SCM_BOOL_F;
                                if (bs == BuiltinSyntax::At) {
                                    SCM resolve_iface = v.lookup("resolve-interface");
                                    if (is_variable(resolve_iface) && !as_obj<VariableObj>(resolve_iface)->value.is_undefined()) {
                                        mod = v.call1(as_obj<VariableObj>(resolve_iface)->value, mod_name);
                                    }
                                } else {
                                    if (is_variable(v.resolve_module_var) && !as_obj<VariableObj>(v.resolve_module_var)->value.is_undefined()) {
                                        mod = v.call1(as_obj<VariableObj>(v.resolve_module_var)->value, mod_name);
                                    }
                                }
                                SCM var = v.module_variable(mod, sym);
                                if (var.is_false() || !is_variable(var) || as_obj<VariableObj>(var)->value.is_undefined()) {
                                    v.error(v.sym_unbound_variable, "@", "Unbound variable: ~S", v.list1(sym));
                                }
                                st.val = as_obj<VariableObj>(var)->value;
                                st.mode = StepMode::ApplyVal;
                                continue;
                            }
                            case BuiltinSyntax::TheEnvironment: {
                                st.val = env;
                                st.mode = StepMode::ApplyVal;
                                continue;
                            }
                            case BuiltinSyntax::DefinedP: {
                                if (!is_pair(arg_exprs)) v.error(v.sym_syntax_error, "defined?", "Bad defined? form");
                                SCM sym = car(arg_exprs);
                                SCM check_env = env;
                                if (is_pair(cdr(arg_exprs))) {
                                    SCM m = v.eval(car(cdr(arg_exprs)), env);
                                    if (v.is_module(m)) {
                                        SCM var = v.module_variable(m, sym);
                                        st.val = SCM::from_bool(is_variable(var) && !as_obj<VariableObj>(var)->value.is_undefined());
                                        st.mode = StepMode::ApplyVal;
                                        continue;
                                    }
                                    check_env = m;
                                }
                                SCM val = lookup_env_opt(sym, check_env);
                                st.val = SCM::from_bool(!val.is_undefined());
                                st.mode = StepMode::ApplyVal;
                                continue;
                            }
                            default:
                                break;
                            }
                        } else {
                            // User macro: Syntax (0), Macro (1), or MemoizingMacro (2)
                            if (mac->kind != MacroKind::Syntax) {
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::MacroExpand, st.cont);
                                cf->env = env;
                                cf->expr = expr;
                                cf->aux = op_val;
                                st.cont = SCM::from_obj(cf);
                            }
                            st.mode = StepMode::ApplyProc;
                            st.proc = mac->transformer;
                            st.args = {expr, env};
                            continue;
                        }
                    } else if (!op_val.is_undefined()) {
                        // Fast path: op_expr already resolved to a value (non-macro)!
                        if (arg_exprs.is_null()) {
                            st.mode = StepMode::ApplyProc;
                            st.proc = op_val;
                            st.args.clear();
                            continue;
                        }
                        if (!is_pair(arg_exprs)) {
                            v.wrong_num_args(nullptr, op_val);
                        }
                        auto* cf = v.heap().alloc<ContFrameObj>(ContKind::AppArgs, st.cont);
                        cf->env = env;
                        cf->aux = op_val;
                        cf->expr = cdr(arg_exprs);
                        st.cont = SCM::from_obj(cf);
                        st.expr = car(arg_exprs);
                        continue;
                    }
                    // If op_val is undefined, fall through to trigger unbound variable error
                }

                // General procedure application: evaluate op_expr first
                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::AppOp, st.cont);
                cf->env = env;
                cf->expr = arg_exprs;
                cf->aux = expr;
                st.cont = SCM::from_obj(cf);
                st.expr = op_expr;
                continue;
            }

            if (st.mode == StepMode::ApplyProc) {
                SCM proc = st.proc;
                size_t nargs = st.args.size();

                if (is_closure(proc)) {
                    auto* cl = as_obj<ClosureObj>(proc);
                    // Avoid unbounded C-stack ping-pong between CEK and bytecode VM
                    // on deep mutual recursion: when C stack is already past half the
                    // limit and the closure is CEK-compatible (no bytecode-only upvals),
                    // execute it directly on the heap-allocated CEK continuation stack.
                    bool prefer_cek = (cl->inline_upval_cnt == 0 && cl->upvals.is_false() &&
                                       is_pair(cl->body) &&
                                       v.c_stack_used() > (v.c_stack_limit_bytes() >> 1));
                    if (!prefer_cek &&
                        (is_bytecode(cl->bytecode) || (cl->bytecode.is_false() && bytecode_should_compile(cl) && bytecode_compile_closure(cl)))) {
                        st.val = bytecode_run_closure(cl, st.args.data(), nargs);
                        st.mode = StepMode::ApplyVal;
                        continue;
                    }
                    SCM new_env = extend_env_formals(cl->formals, st.args.data(), nargs, cl->env, proc);
                    SCM body = cl->body;
                    SCM transformed;
                    if (transform_internal_defines(body, new_env, transformed)) {
                        cl->body = v.list1(transformed);
                        st.mode = StepMode::Eval;
                        st.env = new_env;
                        st.expr = transformed;
                    } else if (cdr(body).is_null()) {
                        st.mode = StepMode::Eval;
                        st.env = new_env;
                        st.expr = car(body);
                    } else {
                        auto* cf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                        cf->env = new_env;
                        cf->expr = cdr(body);
                        st.cont = SCM::from_obj(cf);
                        st.mode = StepMode::Eval;
                        st.env = new_env;
                        st.expr = car(body);
                    }
                    continue;
                }

                if (is_subr(proc)) {
                    auto* sub = as_obj<SubrObj>(proc);
                    if (sub->special != SpecialSubr::None) {
                        switch (sub->special) {
                        case SpecialSubr::Apply: {
                            if (nargs < 2) v.wrong_num_args("apply", proc);
                            SCM target = st.args[0];
                            SCM last = st.args[nargs - 1];
                            if (scm_ilength(last) < 0) v.wrong_type_arg("apply", static_cast<int>(nargs), last, "proper list");
                            std::vector<SCM> flat;
                            for (size_t i = 1; i + 1 < nargs; ++i) flat.push_back(st.args[i]);
                            while (is_pair(last)) { flat.push_back(car(last)); last = cdr(last); }
                            st.proc = target;
                            st.args = std::move(flat);
                            continue;
                        }
                        case SpecialSubr::CallCC: {
                            if (nargs != 1) v.wrong_num_args("call-with-current-continuation", proc);
                            SCM k = v.heap().alloc_scm<ContinuationObj>(st.cont, v.current_dynwind, my_depth);
                            st.proc = st.args[0];
                            st.args = {k};
                            continue;
                        }
                        case SpecialSubr::CallWithValues: {
                            if (nargs != 2) v.wrong_num_args("call-with-values", proc);
                            auto* cf = v.heap().alloc<ContFrameObj>(ContKind::CallWithValues, st.cont);
                            cf->expr = st.args[1]; // consumer
                            st.cont = SCM::from_obj(cf);
                            st.proc = st.args[0]; // producer
                            st.args.clear();
                            continue;
                        }
                        case SpecialSubr::DynamicWind: {
                            if (nargs != 3) v.wrong_num_args("dynamic-wind", proc);
                            SCM before = st.args[0], thunk = st.args[1], after = st.args[2];
                            SCM dw = v.heap().alloc_scm<DynWindObj>(DynWindKind::Wind, before, after, SCM_BOOL_F, SCM_BOOL_F, v.current_dynwind, v.next_dw_id++);
                            auto* cf = v.heap().alloc<ContFrameObj>(ContKind::DynWindEnter, st.cont);
                            cf->expr = thunk;
                            cf->aux = after;
                            cf->aux2 = before;
                            cf->aux3 = dw;
                            st.cont = SCM::from_obj(cf);
                            st.proc = before;
                            st.args.clear();
                            continue;
                        }
                        case SpecialSubr::Catch: {
                            if (nargs < 3 || nargs > 4) v.wrong_num_args("catch", proc);
                            SCM key = st.args[0], thunk = st.args[1], handler = st.args[2];
                            SCM pre_unwind = (nargs == 4) ? st.args[3] : SCM_BOOL_F;
                            SCM dw = v.heap().alloc_scm<DynWindObj>(DynWindKind::Catch, key, handler, pre_unwind, st.cont, v.current_dynwind, v.next_dw_id++);
                            v.current_dynwind = dw;
                            auto* cf = v.heap().alloc<ContFrameObj>(ContKind::CatchPop, st.cont);
                            cf->aux = dw;
                            st.cont = SCM::from_obj(cf);
                            st.proc = thunk;
                            st.args.clear();
                            continue;
                        }
                        case SpecialSubr::LazyCatch: {
                            if (nargs != 3) v.wrong_num_args("lazy-catch", proc);
                            SCM key = st.args[0], thunk = st.args[1], handler = st.args[2];
                            SCM dw = v.heap().alloc_scm<DynWindObj>(DynWindKind::LazyCatch, key, handler, SCM_BOOL_F, st.cont, v.current_dynwind, v.next_dw_id++);
                            v.current_dynwind = dw;
                            auto* cf = v.heap().alloc<ContFrameObj>(ContKind::CatchPop, st.cont);
                            cf->aux = dw;
                            st.cont = SCM::from_obj(cf);
                            st.proc = thunk;
                            st.args.clear();
                            continue;
                        }
                        case SpecialSubr::WithThrowHandler: {
                            if (nargs != 3) v.wrong_num_args("with-throw-handler", proc);
                            SCM key = st.args[0], thunk = st.args[1], handler = st.args[2];
                            SCM dw = v.heap().alloc_scm<DynWindObj>(DynWindKind::ThrowHandler, key, handler, SCM_BOOL_F, st.cont, v.current_dynwind, v.next_dw_id++);
                            v.current_dynwind = dw;
                            auto* cf = v.heap().alloc<ContFrameObj>(ContKind::CatchPop, st.cont);
                            cf->aux = dw;
                            st.cont = SCM::from_obj(cf);
                            st.proc = thunk;
                            st.args.clear();
                            continue;
                        }
                        case SpecialSubr::WithFluidStar: {
                            if (nargs != 3) v.wrong_num_args("with-fluid*", proc);
                            SCM fl = st.args[0], val = st.args[1], thunk = st.args[2];
                            if (!is_fluid(fl)) v.wrong_type_arg("with-fluid*", 1, fl, "fluid");
                            SCM old_val = v.fluid_ref(fl);
                            v.fluid_set(fl, val);
                            SCM dw = v.heap().alloc_scm<DynWindObj>(DynWindKind::Fluid, fl, val, old_val, SCM_BOOL_F, v.current_dynwind, v.next_dw_id++);
                            v.current_dynwind = dw;
                            auto* cf = v.heap().alloc<ContFrameObj>(ContKind::FluidPop, st.cont);
                            cf->aux = dw;
                            st.cont = SCM::from_obj(cf);
                            st.proc = thunk;
                            st.args.clear();
                            continue;
                        }
                        case SpecialSubr::WithFluidsStar: {
                            if (nargs != 3) v.wrong_num_args("with-fluids*", proc);
                            SCM fls = st.args[0], vals = st.args[1], thunk = st.args[2];
                            SCM fcur = fls, vcur = vals;
                            while (is_pair(fcur) && is_pair(vcur)) {
                                SCM fl = car(fcur), val = car(vcur);
                                if (!is_fluid(fl)) v.wrong_type_arg("with-fluids*", 1, fl, "fluid");
                                SCM old_val = v.fluid_ref(fl);
                                v.fluid_set(fl, val);
                                SCM dw = v.heap().alloc_scm<DynWindObj>(DynWindKind::Fluid, fl, val, old_val, SCM_BOOL_F, v.current_dynwind, v.next_dw_id++);
                                v.current_dynwind = dw;
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::FluidPop, st.cont);
                                cf->aux = dw;
                                st.cont = SCM::from_obj(cf);
                                fcur = cdr(fcur);
                                vcur = cdr(vcur);
                            }
                            st.proc = thunk;
                            st.args.clear();
                            continue;
                        }
                        case SpecialSubr::WithDynamicState: {
                            if (nargs != 2) v.wrong_num_args("with-dynamic-state", proc);
                            SCM ds = st.args[0], thunk = st.args[1];
                            if (!is_dynamic_state(ds)) v.wrong_type_arg("with-dynamic-state", 1, ds, "dynamic-state");
                            SCM old_ds = v.set_current_dynamic_state(ds);
                            SCM dw = v.heap().alloc_scm<DynWindObj>(DynWindKind::DynState, ds, old_ds, SCM_BOOL_F, SCM_BOOL_F, v.current_dynwind, v.next_dw_id++);
                            v.current_dynwind = dw;
                            auto* cf = v.heap().alloc<ContFrameObj>(ContKind::DynStatePop, st.cont);
                            cf->aux = dw;
                            st.cont = SCM::from_obj(cf);
                            st.proc = thunk;
                            st.args.clear();
                            continue;
                        }
                        case SpecialSubr::PrimitiveEval: {
                            if (nargs != 1) v.wrong_num_args("primitive-eval", proc);
                            SCM form = st.args[0];
                            SCM xform = v.current_module_transformer();
                            if (xform.is_true() && v.is_procedure(xform)) {
                                form = v.call1(xform, form);
                            }
                            SCM tl = v.current_module_lookup_closure();
                            st.mode = StepMode::Eval;
                            st.expr = form;
                            st.env = tl.is_true() ? v.list1(tl) : SCM_EOL;
                            continue;
                        }
                        case SpecialSubr::Eval: {
                            if (nargs != 2) v.wrong_num_args("eval", proc);
                            SCM form = is_pair(st.args[0]) ? scm_copy_tree(st.args[0]) : st.args[0];
                            SCM mod = st.args[1];
                            if (v.is_module(mod)) {
                                SCM old_mod = v.set_current_module(mod);
                                // Run inside with-fluid* of the_module_fluid so dynamic-wind / errors restore it!
                                SCM dw = v.heap().alloc_scm<DynWindObj>(DynWindKind::Fluid, v.the_module_fluid, mod, old_mod, SCM_BOOL_F, v.current_dynwind, v.next_dw_id++);
                                v.current_dynwind = dw;
                                auto* cf = v.heap().alloc<ContFrameObj>(ContKind::FluidPop, st.cont);
                                cf->aux = dw;
                                st.cont = SCM::from_obj(cf);
                                SCM xform = v.current_module_transformer();
                                if (xform.is_true() && v.is_procedure(xform)) {
                                    form = v.call1(xform, form);
                                }
                                SCM tl = v.module_lookup_closure(mod);
                                st.mode = StepMode::Eval;
                                st.expr = form;
                                st.env = tl.is_true() ? v.list1(tl) : SCM_EOL;
                                continue;
                            }
                            // Allow evaluating in a lexical environment list too
                            st.mode = StepMode::Eval;
                            st.expr = form;
                            st.env = mod;
                            continue;
                        }
                        case SpecialSubr::LocalEval: {
                            if (nargs < 1 || nargs > 2) v.wrong_num_args("local-eval", proc);
                            st.mode = StepMode::Eval;
                            st.expr = is_pair(st.args[0]) ? scm_copy_tree(st.args[0]) : st.args[0];
                            st.env = (nargs == 2) ? st.args[1] : (v.current_module_lookup_closure().is_true() ? v.list1(v.current_module_lookup_closure()) : SCM_EOL);
                            continue;
                        }
                        case SpecialSubr::Force: {
                            if (nargs != 1) v.wrong_num_args("force", proc);
                            SCM p = st.args[0];
                            if (!is_promise(p)) {
                                v.wrong_type_arg("force", 1, p, "promise");
                            }
                            auto* pr = as_obj<PromiseObj>(p);
                            if (pr->forced) {
                                st.val = pr->expr_or_val;
                                st.mode = StepMode::ApplyVal;
                                continue;
                            }
                            auto* cf = v.heap().alloc<ContFrameObj>(ContKind::ForceFinish, st.cont);
                            cf->aux = p;
                            st.cont = SCM::from_obj(cf);
                            st.mode = StepMode::Eval;
                            st.expr = pr->expr_or_val;
                            st.env = pr->env;
                            continue;
                        }
                        case SpecialSubr::Map:
                        case SpecialSubr::ForEach:
                        case SpecialSubr::AndMap:
                        case SpecialSubr::OrMap: {
                            if (nargs < 2) v.wrong_num_args(sub->name.c_str(), proc);
                            SCM fn = st.args[0];
                            int mode = (sub->special == SpecialSubr::Map) ? 0
                                     : (sub->special == SpecialSubr::ForEach) ? 1
                                     : (sub->special == SpecialSubr::AndMap) ? 2 : 3;
                            // Validate all list args: for core map/for-each (mode 0/1), all must have equal length!
                            bool any_empty = false;
                            bool any_nonempty = false;
                            std::vector<SCM> call_args;
                            SCM next_lists_h = SCM_EOL, next_lists_t = SCM_EOL;
                            for (size_t i = 1; i < nargs; ++i) {
                                SCM l = st.args[i];
                                if (l.is_null()) {
                                    any_empty = true;
                                    continue;
                                }
                                if (!is_pair(l)) v.wrong_type_arg(sub->name.c_str(), static_cast<int>(i + 1), l, "list");
                                any_nonempty = true;
                                if (!any_empty) {
                                    call_args.push_back(car(l));
                                    SCM c = v.cons(cdr(l), SCM_EOL);
                                    if (next_lists_h.is_null()) next_lists_h = next_lists_t = c;
                                    else { set_cdr(next_lists_t, c); next_lists_t = c; }
                                }
                            }
                            if (any_empty) {
                                if (any_nonempty && (mode == 0 || mode == 1)) {
                                    v.out_of_range(sub->name.c_str(), st.args[1]);
                                }
                                st.val = (mode == 0) ? SCM_EOL : (mode == 1 ? SCM_UNSPECIFIED : (mode == 2 ? SCM_BOOL_T : SCM_BOOL_F));
                                st.mode = StepMode::ApplyVal;
                                continue;
                            }
                            auto* cf = v.heap().alloc<ContFrameObj>(ContKind::MapLoop, st.cont);
                            cf->expr = SCM_EOL;
                            cf->aux = fn;
                            cf->aux2 = next_lists_h;
                            cf->aux3 = SCM::from_fixnum(mode);
                            st.cont = SCM::from_obj(cf);
                            st.proc = fn;
                            st.args = std::move(call_args);
                            continue;
                        }
                        default:
                            break;
                        }
                    }

                    // Normal C++ Subr invocation
                    int req = sub->req, opt = sub->opt;
                    if (static_cast<int>(nargs) < req || (!sub->rst && static_cast<int>(nargs) > req + opt)) {
                        v.wrong_num_args(sub->name.c_str(), proc);
                    }
                    // Record active subr frame for debugging stack traces if it throws
                    SCM saved_cont_for_subr = st.cont;
                    if (v.debug_opts.debug) {
                        auto* scf = v.heap().alloc<ContFrameObj>(ContKind::AppArgs, st.cont);
                        scf->aux = proc;
                        scf->vals.assign_from(st.args.data(), nargs);
                        st.cont = SCM::from_obj(scf);
                        v.current_cont = st.cont;
                    }
                    if (sub->raw_varargs) {
                        st.val = sub->raw_fn ? sub->raw_fn(st.args.data(), nargs) : sub->fn(st.args.data(), nargs);
                    } else if (!sub->rst) {
                        // Pad missing optional args with SCM_UNDEFINED
                        if (static_cast<int>(nargs) < req + opt) {
                            st.args.resize(static_cast<size_t>(req + opt), SCM_UNDEFINED);
                            sync_args();
                        }
                        st.val = sub->raw_fn ? sub->raw_fn(st.args.data(), nargs) : sub->fn(st.args.data(), nargs);
                    } else {
                        // Pack rest args into a list at index req + opt
                        SCM rest_list = SCM_EOL;
                        for (size_t i = nargs; i > static_cast<size_t>(req + opt); --i) {
                            rest_list = v.cons(st.args[i - 1], rest_list);
                        }
                        std::vector<SCM> packed(static_cast<size_t>(req + opt + 1), SCM_UNDEFINED);
                        for (size_t i = 0; i < std::min<size_t>(nargs, static_cast<size_t>(req + opt)); ++i) {
                            packed[i] = st.args[i];
                        }
                        packed[static_cast<size_t>(req + opt)] = rest_list;
                        st.val = sub->fn(packed.data(), packed.size());
                    }
                    st.cont = saved_cont_for_subr;
                    st.mode = StepMode::ApplyVal;
                    continue;
                }

                if (is_continuation(proc)) {
                    auto* k = as_obj<ContinuationObj>(proc);
                    SCM ret_val = (nargs == 0) ? v.make_values(SCM_EOL)
                                : (nargs == 1 ? st.args[0] : v.make_values(v.list_n(st.args.data(), nargs)));
                    if (k->has_vm_snap) {
                        if (k->vm_depth > 0 && v.is_bc_inv_active(k->vm_depth)) {
                            ContinuationEscape esc;
                            esc.target_cont = k->cont_frame;
                            esc.target_dw = k->dynwind_chain;
                            esc.return_val = ret_val;
                            esc.target_depth = 0;
                            esc.target_vm_depth = k->vm_depth;
                            esc.target_k_obj = proc;
                            throw esc;
                        }
                        // Upward multi-shot continuation whose original bytecode run already returned!
                        switch_dynwind(k->dynwind_chain);
                        SCM r = bytecode_resume_continuation(k, ret_val);
                        if (k->cek_depth > 0 && k->cek_depth < my_depth) {
                            ContinuationEscape esc;
                            esc.target_cont = k->cont_frame;
                            esc.target_dw = k->dynwind_chain;
                            esc.return_val = r;
                            esc.target_depth = k->cek_depth;
                            esc.target_vm_depth = -1;
                            esc.target_k_obj = proc;
                            throw esc;
                        }
                        st.cont = k->cont_frame;
                        st.val = r;
                        st.mode = StepMode::ApplyVal;
                        continue;
                    }
                    if (k->cek_depth > 0 && k->cek_depth < my_depth) {
                        ContinuationEscape esc;
                        esc.target_cont = k->cont_frame;
                        esc.target_dw = k->dynwind_chain;
                        esc.return_val = ret_val;
                        esc.target_depth = k->cek_depth;
                        esc.target_vm_depth = -1;
                        esc.target_k_obj = proc;
                        throw esc;
                    }
                    switch_dynwind(k->dynwind_chain);
                    st.cont = k->cont_frame;
                    st.val = ret_val;
                    st.mode = StepMode::ApplyVal;
                    continue;
                }

                if (is_proc_with_setter(proc)) {
                    st.proc = as_obj<ProcWithSetterObj>(proc)->getter;
                    continue;
                }

                if (is_eval_closure(proc)) {
                    if (nargs != 2) v.wrong_num_args("eval-closure", proc);
                    st.val = v.eval_closure_lookup(proc, st.args[0], st.args[1].is_true());
                    st.mode = StepMode::ApplyVal;
                    continue;
                }

                if (is_guardian(proc)) {
                    auto* g = as_obj<GuardianObj>(proc);
                    if (g->destroyed) v.misc_error("guardian", "Guardian is destroyed");
                    if (nargs == 0) {
                        int nz = N(g->zombies);
                        if (nz == 0) {
                            st.val = SCM_BOOL_F;
                        } else {
                            st.val = g->zombies[0];
                            for (int i = 1; i < nz; i++) g->zombies[i - 1] = g->zombies[i];
                            g->zombies->resize(nz - 1);
                        }
                    } else if (nargs == 1) {
                        if (st.args[0].is_heap()) {
                            g->live.push_back(st.args[0]);
                        }
                        st.val = SCM_UNSPECIFIED;
                    } else {
                        v.wrong_num_args("guardian", proc);
                    }
                    st.mode = StepMode::ApplyVal;
                    continue;
                }

                if (is_struct(proc)) {
                    auto* so = as_obj<StructObj>(proc);
                    uint32_t of = obj_class_flags(proc);
                    if ((of & CLASSF_PURE_GENERIC) != 0) {
                        st.val = goops_apply_generic(proc, st.args.data(), nargs);
                        st.mode = StepMode::ApplyVal;
                        continue;
                    }
                    if ((of & CLASSF_OPERATOR) != 0 || (of & CLASSF_ENTITY) != 0 || so->is_entity || so->is_operator) {
                        SCM target = SCM_BOOL_F;
                        if ((of & CLASSF_ENTITY) != 0 || so->is_entity) {
                            target = so->entity_proc;
                        } else if (is_struct(so->vtable) && as_obj<StructObj>(so->vtable)->slots.size() > 3) {
                            target = SCM(as_obj<StructObj>(so->vtable)->slots[3]);
                        }
                        if (target.is_true() && v.is_procedure(target)) {
                            // Guile 1.8: entity/operator procedure receives (self arg1 ...)
                            std::vector<SCM> new_args;
                            new_args.reserve(nargs + 1);
                            new_args.push_back(proc);
                            new_args.insert(new_args.end(), st.args.begin(), st.args.end());
                            st.args.swap(new_args);
                            st.proc = target;
                            continue;
                        }
                    }
                    v.misc_error(nullptr, "Wrong type to apply: ~S", v.list1(proc));
                }

                if (is_smob(proc)) {
                    auto* sm = as_obj<SmobObj>(proc);
                    auto& info = v.smob_info(sm->smob_type);
                    if (info.apply) {
                        std::vector<SCM> full;
                        full.push_back(proc);
                        full.insert(full.end(), st.args.begin(), st.args.end());
                        st.val = info.apply(full.data(), full.size());
                        st.mode = StepMode::ApplyVal;
                        continue;
                    }
                }

                v.misc_error(nullptr, "Wrong type to apply: ~S", v.list1(proc));
            }

            // StepMode::ApplyVal — deliver st.val to top continuation frame st.cont
            if (st.cont.is_null()) {
                return st.val;
            }
            auto* cf = as_obj<ContFrameObj>(st.cont);
            st.cont = cf->next;

            switch (cf->kind) {
            case ContKind::Halt:
                return st.val;

            case ContKind::If:
                if (st.val.is_true()) {
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = cf->aux;
                } else if (cf->aux2.is_true()) {
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = cf->expr;
                } else {
                    st.val = SCM_UNSPECIFIED;
                    st.mode = StepMode::ApplyVal;
                }
                continue;

            case ContKind::Begin: {
                st.val = SCM_UNSPECIFIED;
                SCM rem = cf->expr;
                SCM next_env = cf->env;
                SCM track_ec = cf->aux;
                if (v.module_system_booted && track_ec.is_true()) {
                    SCM tl = v.current_module_lookup_closure();
                    if (tl.is_true() && tl != track_ec) {
                        next_env = v.list1(tl);
                        track_ec = tl;
                    }
                }
                if (cdr(rem).is_null()) {
                    st.mode = StepMode::Eval;
                    st.env = next_env;
                    st.expr = car(rem);
                } else {
                    auto* next_cf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                    next_cf->env = next_env;
                    next_cf->expr = cdr(rem);
                    next_cf->aux = track_ec;
                    st.cont = SCM::from_obj(next_cf);
                    st.mode = StepMode::Eval;
                    st.env = next_env;
                    st.expr = car(rem);
                }
                continue;
            }

            case ContKind::SetVar: {
                if (is_variable(cf->expr)) {
                    auto* vo = as_obj<VariableObj>(cf->expr);
                    vo->value = st.val;
                    v.notify_var_mutated(vo);
                } else {
                    auto loc = lookup_binding(cf->expr, cf->env, false);
                    if (loc.kind == BindingKind::LocalCar) set_car(loc.cell, st.val);
                    else if (loc.kind == BindingKind::LocalCdr) set_cdr(loc.cell, st.val);
                    else if (loc.kind == BindingKind::GlobalVar) {
                        auto* vo = as_obj<VariableObj>(loc.cell);
                        vo->value = st.val;
                        v.notify_var_mutated(vo);
                    }
                    else v.error(v.sym_unbound_variable, "set!", "Unbound variable: ~S", v.list1(cf->expr));
                }
                st.val = SCM_UNSPECIFIED;
                st.mode = StepMode::ApplyVal;
                continue;
            }

            case ContKind::Define: {
                SCM var = v.sym2var(cf->expr, cf->aux, true);
                auto* vo = as_obj<VariableObj>(var);
                vo->value = st.val;
                v.notify_var_mutated(vo);
                if (v.debug_opts.procnames) {
                    if (is_closure(st.val) && v.procedure_property(st.val, v.sym_name).is_false()) {
                        v.set_procedure_property(st.val, v.sym_name, cf->expr);
                    } else if (is_proc_with_setter(st.val) && scm_assq(v.sym_name, v.object_properties(st.val)).is_false()) {
                        v.set_procedure_property(st.val, v.sym_name, cf->expr);
                    }
                }
                st.val = SCM_UNSPECIFIED;
                st.mode = StepMode::ApplyVal;
                continue;
            }

            case ContKind::AppOp: {
                SCM proc = st.val;
                SCM rem_args = cf->expr;
                if (rem_args.is_null()) {
                    st.mode = StepMode::ApplyProc;
                    st.proc = proc;
                    st.args.clear();
                    continue;
                }
                if (!is_pair(rem_args)) v.wrong_num_args(nullptr, proc);
                auto* ncf = v.heap().alloc<ContFrameObj>(ContKind::AppArgs, st.cont);
                ncf->env = cf->env;
                ncf->aux = proc;
                ncf->expr = cdr(rem_args);
                st.cont = SCM::from_obj(ncf);
                st.mode = StepMode::Eval;
                st.env = cf->env;
                st.expr = car(rem_args);
                continue;
            }

            case ContKind::AppArgs: {
                // Note: allocate a fresh frame rather than mutating cf in place, so multi-shot call/cc never corrupts saved frames!
                if (cf->expr.is_null()) {
                    st.mode = StepMode::ApplyProc;
                    st.proc = cf->aux;
                    st.args.assign(cf->vals.data(), cf->vals.data() + cf->vals.size());
                    st.args.push_back(st.val);
                    continue;
                }
                if (!is_pair(cf->expr)) v.wrong_num_args(nullptr, cf->aux);
                auto* ncf = v.heap().alloc<ContFrameObj>(ContKind::AppArgs, st.cont);
                ncf->env = cf->env;
                ncf->aux = cf->aux;
                ncf->expr = cdr(cf->expr);
                ncf->vals = cf->vals;
                ncf->vals.push_back(st.val);
                st.cont = SCM::from_obj(ncf);
                st.mode = StepMode::Eval;
                st.env = cf->env;
                st.expr = car(cf->expr);
                continue;
            }

            case ContKind::LetInit: {
                SmallScmVec evaled = cf->vals;
                evaled.push_back(st.val);
                if (is_pair(cf->expr)) {
                    SCM entry = car(cf->expr);
                    if (!is_pair(entry) || !is_symbol(car(entry)) || !is_pair(cdr(entry)) || !cdr(cdr(entry)).is_null()) {
                        v.error(v.sym_syntax_error, "let", "Bad binding in let: ~S", v.list1(entry));
                    }
                    auto* ncf = v.heap().alloc<ContFrameObj>(ContKind::LetInit, st.cont);
                    ncf->env = cf->env;
                    ncf->expr = cdr(cf->expr);
                    ncf->aux = cf->aux;
                    ncf->aux2 = v.cons(car(entry), cf->aux2);
                    ncf->aux3 = cf->aux3;
                    ncf->vals = std::move(evaled);
                    st.cont = SCM::from_obj(ncf);
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(cdr(entry));
                    continue;
                }
                SCM names = scm_reverse(cf->aux2);
                if (cf->aux3.is_true()) {
                    // Named let!
                    SCM cl = v.make_closure(names, cf->aux, cf->env);
                    SCM loop_env = v.cons(v.cons(v.list1(cf->aux3), v.list1(cl)), cf->env);
                    as_obj<ClosureObj>(cl)->env = loop_env;
                    st.mode = StepMode::ApplyProc;
                    st.proc = cl;
                    st.args.assign(evaled.data(), evaled.data() + evaled.size());
                    continue;
                }
                SCM vals_list = v.list_n(evaled.data(), evaled.size());
                SCM new_env = v.cons(v.cons(names, vals_list), cf->env);
                SCM body = cf->aux;
                SCM transformed;
                if (transform_internal_defines(body, new_env, transformed)) {
                    st.mode = StepMode::Eval;
                    st.env = new_env;
                    st.expr = transformed;
                } else if (cdr(body).is_null()) {
                    st.mode = StepMode::Eval;
                    st.env = new_env;
                    st.expr = car(body);
                } else {
                    auto* bcf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                    bcf->env = new_env;
                    bcf->expr = cdr(body);
                    st.cont = SCM::from_obj(bcf);
                    st.mode = StepMode::Eval;
                    st.env = new_env;
                    st.expr = car(body);
                }
                continue;
            }

            case ContKind::LetStarInit: {
                SCM new_env = v.cons(v.cons(v.list1(cf->aux2), v.list1(st.val)), cf->env);
                if (is_pair(cf->expr)) {
                    SCM entry = car(cf->expr);
                    if (!is_pair(entry) || !is_symbol(car(entry)) || !is_pair(cdr(entry)) || !cdr(cdr(entry)).is_null()) {
                        v.error(v.sym_syntax_error, "let*", "Bad binding in let*: ~S", v.list1(entry));
                    }
                    auto* ncf = v.heap().alloc<ContFrameObj>(ContKind::LetStarInit, st.cont);
                    ncf->env = new_env;
                    ncf->expr = cdr(cf->expr);
                    ncf->aux = cf->aux;
                    ncf->aux2 = car(entry);
                    st.cont = SCM::from_obj(ncf);
                    st.mode = StepMode::Eval;
                    st.env = new_env;
                    st.expr = car(cdr(entry));
                    continue;
                }
                SCM body = cf->aux;
                SCM transformed;
                if (transform_internal_defines(body, new_env, transformed)) {
                    st.mode = StepMode::Eval;
                    st.env = new_env;
                    st.expr = transformed;
                } else if (cdr(body).is_null()) {
                    st.mode = StepMode::Eval;
                    st.env = new_env;
                    st.expr = car(body);
                } else {
                    auto* bcf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                    bcf->env = new_env;
                    bcf->expr = cdr(body);
                    st.cont = SCM::from_obj(bcf);
                    st.mode = StepMode::Eval;
                    st.env = new_env;
                    st.expr = car(body);
                }
                continue;
            }

            case ContKind::LetrecInit: {
                // R5RS / Petrofsky-safe letrec: collect ALL init values into a fresh vector BEFORE assigning to frame!
                SmallScmVec evaled = cf->vals;
                evaled.push_back(st.val);
                if (is_pair(cf->expr)) {
                    SCM entry = car(cf->expr);
                    auto* ncf = v.heap().alloc<ContFrameObj>(ContKind::LetrecInit, st.cont);
                    ncf->env = cf->env;
                    ncf->expr = cdr(cf->expr);
                    ncf->aux = cf->aux;
                    ncf->aux2 = cf->aux2; // head of vals list
                    ncf->vals = std::move(evaled);
                    st.cont = SCM::from_obj(ncf);
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(cdr(entry));
                    continue;
                }
                SCM cell = cf->aux2;
                for (size_t i = 0; i < evaled.size() && is_pair(cell); ++i) {
                    set_car(cell, evaled[i]);
                    cell = cdr(cell);
                }
                SCM body = cf->aux;
                SCM transformed;
                if (transform_internal_defines(body, cf->env, transformed)) {
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = transformed;
                } else if (cdr(body).is_null()) {
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(body);
                } else {
                    auto* bcf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                    bcf->env = cf->env;
                    bcf->expr = cdr(body);
                    st.cont = SCM::from_obj(bcf);
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(body);
                }
                continue;
            }

            case ContKind::And: {
                if (st.val.is_false()) {
                    st.mode = StepMode::ApplyVal;
                    continue;
                }
                SCM rem = cf->expr;
                if (cdr(rem).is_null()) {
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(rem);
                } else {
                    auto* ncf = v.heap().alloc<ContFrameObj>(ContKind::And, st.cont);
                    ncf->env = cf->env;
                    ncf->expr = cdr(rem);
                    st.cont = SCM::from_obj(ncf);
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(rem);
                }
                continue;
            }

            case ContKind::Or: {
                if (st.val.is_true()) {
                    st.mode = StepMode::ApplyVal;
                    continue;
                }
                SCM rem = cf->expr;
                if (cdr(rem).is_null()) {
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(rem);
                } else {
                    auto* ncf = v.heap().alloc<ContFrameObj>(ContKind::Or, st.cont);
                    ncf->env = cf->env;
                    ncf->expr = cdr(rem);
                    st.cont = SCM::from_obj(ncf);
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(rem);
                }
                continue;
            }

            case ContKind::CondClause: {
                SCM clause = cf->expr;
                SCM rem_clauses = cf->aux;
                SCM body = cdr(clause);
                bool arrow_unbound = lookup_env_opt(v.sym_arrow, cf->env).is_undefined();
                // Check SRFI-61: (generator guard => receiver)
                if (arrow_unbound && is_pair(body) && is_pair(cdr(body)) &&
                    car(cdr(body)) == v.sym_arrow && is_pair(cdr(cdr(body)))) {
                    std::vector<SCM> gvals;
                    if (is_values(st.val)) {
                        SCM cur = as_obj<ValuesObj>(st.val)->values;
                        while (is_pair(cur)) { gvals.push_back(car(cur)); cur = cdr(cur); }
                    } else {
                        gvals.push_back(st.val);
                    }
                    SCM guard_fn = v.eval(car(body), cf->env);
                    SCM guard_res = v.apply(guard_fn, gvals.data(), gvals.size());
                    if (guard_res.is_true()) {
                        SCM recv_fn = v.eval(car(cdr(cdr(body))), cf->env);
                        st.mode = StepMode::ApplyProc;
                        st.proc = recv_fn;
                        st.args = std::move(gvals);
                        continue;
                    }
                    // Guard failed: fall through to rem_clauses
                } else if (st.val.is_true()) {
                    if (body.is_null()) {
                        st.mode = StepMode::ApplyVal;
                        continue;
                    }
                    if (arrow_unbound && is_pair(body) && car(body) == v.sym_arrow) {
                        auto* acf = v.heap().alloc<ContFrameObj>(ContKind::CondArrow, st.cont);
                        acf->expr = st.val;
                        st.cont = SCM::from_obj(acf);
                        st.mode = StepMode::Eval;
                        st.env = cf->env;
                        st.expr = car(cdr(body));
                        continue;
                    }
                    if (cdr(body).is_null()) {
                        st.mode = StepMode::Eval;
                        st.env = cf->env;
                        st.expr = car(body);
                    } else {
                        auto* bcf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                        bcf->env = cf->env;
                        bcf->expr = cdr(body);
                        st.cont = SCM::from_obj(bcf);
                        st.mode = StepMode::Eval;
                        st.env = cf->env;
                        st.expr = car(body);
                    }
                    continue;
                }
                if (rem_clauses.is_null()) {
                    st.val = SCM_UNSPECIFIED;
                    st.mode = StepMode::ApplyVal;
                    continue;
                }
                SCM next_cl = car(rem_clauses);
                if (!is_pair(next_cl)) v.error(v.sym_syntax_error, "cond", "Bad cond clause");
                if (car(next_cl) == v.sym_else && lookup_env_opt(v.sym_else, cf->env).is_undefined()) {
                    SCM nbody = cdr(next_cl);
                    if (nbody.is_null()) v.error(v.sym_syntax_error, "cond", "Empty else clause");
                    if (cdr(nbody).is_null()) { st.mode = StepMode::Eval; st.env = cf->env; st.expr = car(nbody); continue; }
                    auto* bcf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                    bcf->env = cf->env;
                    bcf->expr = cdr(nbody);
                    st.cont = SCM::from_obj(bcf);
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(nbody);
                    continue;
                }
                auto* ncf = v.heap().alloc<ContFrameObj>(ContKind::CondClause, st.cont);
                ncf->env = cf->env;
                ncf->expr = next_cl;
                ncf->aux = cdr(rem_clauses);
                st.cont = SCM::from_obj(ncf);
                st.mode = StepMode::Eval;
                st.env = cf->env;
                st.expr = car(next_cl);
                continue;
            }

            case ContKind::CondArrow: {
                st.mode = StepMode::ApplyProc;
                st.proc = st.val;
                st.args = {cf->expr};
                continue;
            }

            case ContKind::CaseKey: {
                SCM key = st.val;
                SCM clauses = cf->expr;
                SCM matched_body = SCM_BOOL_F;
                bool else_unbound = lookup_env_opt(v.sym_else, cf->env).is_undefined();
                while (is_pair(clauses)) {
                    SCM cl = car(clauses);
                    if (!is_pair(cl)) v.error(v.sym_syntax_error, "case", "Bad case clause");
                    SCM datums = car(cl);
                    if (else_unbound && datums == v.sym_else) {
                        matched_body = cdr(cl);
                        break;
                    }
                    bool hit = false;
                    while (is_pair(datums)) {
                        if (scm_eqv(key, car(datums))) { hit = true; break; }
                        datums = cdr(datums);
                    }
                    if (hit) { matched_body = cdr(cl); break; }
                    clauses = cdr(clauses);
                }
                if (matched_body.is_false() || matched_body.is_null()) {
                    st.val = SCM_UNSPECIFIED;
                    st.mode = StepMode::ApplyVal;
                    continue;
                }
                if (cdr(matched_body).is_null()) {
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(matched_body);
                } else {
                    auto* bcf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                    bcf->env = cf->env;
                    bcf->expr = cdr(matched_body);
                    st.cont = SCM::from_obj(bcf);
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(matched_body);
                }
                continue;
            }

            case ContKind::DoInit: {
                SmallScmVec inits = cf->vals;
                inits.push_back(st.val);
                if (is_pair(cf->expr)) {
                    SCM spec = car(cf->expr);
                    auto* ncf = v.heap().alloc<ContFrameObj>(ContKind::DoInit, st.cont);
                    ncf->env = cf->env;
                    ncf->expr = cdr(cf->expr);
                    ncf->aux = cf->aux;
                    ncf->vals = std::move(inits);
                    st.cont = SCM::from_obj(ncf);
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(cdr(spec));
                    continue;
                }
                // Build initial loop frame
                SCM do_args = cf->aux;
                SCM var_specs = car(do_args);
                SCM names_h = SCM_EOL, names_t = SCM_EOL;
                SCM vs = var_specs;
                while (is_pair(vs)) {
                    SCM nc = v.cons(car(car(vs)), SCM_EOL);
                    if (names_h.is_null()) names_h = names_t = nc;
                    else { set_cdr(names_t, nc); names_t = nc; }
                    vs = cdr(vs);
                }
                SCM vals_list = v.list_n(inits.data(), inits.size());
                SCM loop_env = v.cons(v.cons(names_h, vals_list), cf->env);
                auto* tcf = v.heap().alloc<ContFrameObj>(ContKind::DoTest, st.cont);
                tcf->env = loop_env;
                tcf->expr = do_args;
                st.cont = SCM::from_obj(tcf);
                st.mode = StepMode::Eval;
                st.env = loop_env;
                st.expr = car(car(cdr(do_args)));
                continue;
            }

            case ContKind::DoTest: {
                SCM do_args = cf->expr;
                SCM test_clause = car(cdr(do_args));
                if (st.val.is_true()) {
                    SCM res_exprs = cdr(test_clause);
                    if (res_exprs.is_null()) {
                        st.val = SCM_UNSPECIFIED;
                        st.mode = StepMode::ApplyVal;
                    } else if (cdr(res_exprs).is_null()) {
                        st.mode = StepMode::Eval;
                        st.env = cf->env;
                        st.expr = car(res_exprs);
                    } else {
                        auto* bcf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                        bcf->env = cf->env;
                        bcf->expr = cdr(res_exprs);
                        st.cont = SCM::from_obj(bcf);
                        st.mode = StepMode::Eval;
                        st.env = cf->env;
                        st.expr = car(res_exprs);
                    }
                    continue;
                }
                // Run commands, then evaluate step expressions!
                SCM cmds = cdr(cdr(do_args));
                SCM var_specs = car(do_args);
                // Build step expression list: for each (var init [step]), use step if present else var
                SCM steps_h = SCM_EOL, steps_t = SCM_EOL;
                SCM vs = var_specs;
                while (is_pair(vs)) {
                    SCM sp = car(vs);
                    SCM step_e = (is_pair(cdr(cdr(sp)))) ? car(cdr(cdr(sp))) : car(sp);
                    SCM sc = v.cons(step_e, SCM_EOL);
                    if (steps_h.is_null()) steps_h = steps_t = sc;
                    else { set_cdr(steps_t, sc); steps_t = sc; }
                    vs = cdr(vs);
                }
                // Push DoStep frame (if steps exist) or directly loop DoTest
                if (steps_h.is_null()) {
                    auto* tcf = v.heap().alloc<ContFrameObj>(ContKind::DoTest, st.cont);
                    tcf->env = cf->env;
                    tcf->expr = do_args;
                    st.cont = SCM::from_obj(tcf);
                    if (cmds.is_null()) {
                        st.mode = StepMode::Eval;
                        st.env = cf->env;
                        st.expr = car(test_clause);
                    } else {
                        // Run commands then re-eval test
                        SCM all_cmds = scm_append2(cmds, v.list1(car(test_clause)));
                        auto* bcf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                        bcf->env = cf->env;
                        bcf->expr = cdr(all_cmds);
                        st.cont = SCM::from_obj(bcf);
                        st.mode = StepMode::Eval;
                        st.env = cf->env;
                        st.expr = car(all_cmds);
                    }
                    continue;
                }
                auto* scf = v.heap().alloc<ContFrameObj>(ContKind::DoStep, st.cont);
                scf->env = cf->env;
                scf->expr = cdr(steps_h);
                scf->aux = do_args;
                st.cont = SCM::from_obj(scf);
                if (cmds.is_null()) {
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(steps_h);
                } else {
                    SCM all_cmds = scm_append2(cmds, v.list1(car(steps_h)));
                    auto* bcf = v.heap().alloc<ContFrameObj>(ContKind::Begin, st.cont);
                    bcf->env = cf->env;
                    bcf->expr = cdr(all_cmds);
                    st.cont = SCM::from_obj(bcf);
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(all_cmds);
                }
                continue;
            }

            case ContKind::DoStep: {
                SmallScmVec new_vals = cf->vals;
                new_vals.push_back(st.val);
                if (is_pair(cf->expr)) {
                    auto* ncf = v.heap().alloc<ContFrameObj>(ContKind::DoStep, st.cont);
                    ncf->env = cf->env;
                    ncf->expr = cdr(cf->expr);
                    ncf->aux = cf->aux;
                    ncf->vals = std::move(new_vals);
                    st.cont = SCM::from_obj(ncf);
                    st.mode = StepMode::Eval;
                    st.env = cf->env;
                    st.expr = car(cf->expr);
                    continue;
                }
                // Bind a FRESH frame (per R5RS so closures captured in loop iterations get distinct bindings!)
                SCM do_args = cf->aux;
                SCM outer_env = cdr(cf->env);
                SCM names = car(car(cf->env));
                SCM vals_list = v.list_n(new_vals.data(), new_vals.size());
                SCM next_env = v.cons(v.cons(names, vals_list), outer_env);
                auto* tcf = v.heap().alloc<ContFrameObj>(ContKind::DoTest, st.cont);
                tcf->env = next_env;
                tcf->expr = do_args;
                st.cont = SCM::from_obj(tcf);
                st.mode = StepMode::Eval;
                st.env = next_env;
                st.expr = car(car(cdr(do_args)));
                continue;
            }

            case ContKind::MacroExpand: {
                // Guile 1.8 semantics: MemoizingMacro (defmacro / define-macro / procedure->memoizing-macro)
                // overwrites the macro call pair in-place so the macro is expanded ONCE, never again!
                if (is_macro(cf->aux) && as_obj<MacroObj>(cf->aux)->kind == MacroKind::MemoizingMacro &&
                    is_pair(cf->expr) && st.val != cf->expr) {
                    // Check target pair doesn't appear inside expanded form (prevent cycles)
                    auto contains_cell = [](auto& self, SCM val, SCM target, int depth) noexcept -> bool {
                        if (!is_pair(val)) return false;
                        if (val == target) return true;
                        if (depth > 24) return true;
                        SCM cur = val;
                        int steps = 0;
                        while (is_pair(cur)) {
                            if (cur == target || ++steps > 256) return true;
                            if (self(self, car(cur), target, depth + 1)) return true;
                            cur = cdr(cur);
                        }
                        return cur == target;
                    };
                    if (!contains_cell(contains_cell, st.val, cf->expr, 0)) {
                        auto* p = as_obj<PairObj>(cf->expr);
                        if (is_pair(st.val)) {
                            p->car = car(st.val);
                            p->cdr = cdr(st.val);
                        } else if (is_macro(v.builtin_begin_macro)) {
                            p->car = v.builtin_begin_macro;
                            p->cdr = v.list1(st.val);
                        }
                    }
                }
                // Evaluate the expanded form in the original env!
                st.mode = StepMode::Eval;
                st.env = cf->env;
                st.expr = st.val;
                continue;
            }

            case ContKind::CallWithValues: {
                st.mode = StepMode::ApplyProc;
                st.proc = cf->expr;
                st.args.clear();
                if (is_values(st.val)) {
                    SCM vl = as_obj<ValuesObj>(st.val)->values;
                    while (is_pair(vl)) { st.args.push_back(car(vl)); vl = cdr(vl); }
                } else {
                    st.args.push_back(st.val);
                }
                continue;
            }

            case ContKind::DynWindEnter: {
                // before thunk finished; install dw and run thunk
                v.current_dynwind = cf->aux3;
                auto* ecf = v.heap().alloc<ContFrameObj>(ContKind::DynWindExit, st.cont);
                ecf->expr = cf->aux; // after thunk
                ecf->aux = cf->aux3; // dw node
                st.cont = SCM::from_obj(ecf);
                st.mode = StepMode::ApplyProc;
                st.proc = cf->expr; // main thunk
                st.args.clear();
                continue;
            }

            case ContKind::DynWindExit: {
                // thunk finished with st.val; unlink dw, run after thunk, then return st.val
                auto* dw = as_obj<DynWindObj>(cf->aux);
                v.current_dynwind = dw->parent;
                // Push a tiny frame to restore saved result after `after` returns
                auto* rcf = v.heap().alloc<ContFrameObj>(ContKind::DynWindRewind, st.cont);
                rcf->aux = st.val;
                st.cont = SCM::from_obj(rcf);
                st.mode = StepMode::ApplyProc;
                st.proc = cf->expr; // after thunk
                st.args.clear();
                continue;
            }

            case ContKind::DynWindRewind: {
                st.val = cf->aux;
                st.mode = StepMode::ApplyVal;
                continue;
            }

            case ContKind::CatchPop: {
                auto* dw = as_obj<DynWindObj>(cf->aux);
                v.current_dynwind = dw->parent;
                st.mode = StepMode::ApplyVal;
                continue;
            }

            case ContKind::RethrowAfter: {
                v.throw_scm(cf->aux, cf->aux2);
            }

            case ContKind::PreUnwindAfter: {
                auto* dw = as_obj<DynWindObj>(cf->aux);
                switch_dynwind(dw->parent);
                st.cont = dw->d;
                st.mode = StepMode::ApplyProc;
                st.proc = dw->b;
                st.args.clear();
                st.args.push_back(cf->aux2);
                SCM a = cf->aux3;
                while (is_pair(a)) { st.args.push_back(car(a)); a = cdr(a); }
                continue;
            }

            case ContKind::FluidPop: {
                auto* dw = as_obj<DynWindObj>(cf->aux);
                v.current_dynwind = dw->parent;
                swap_fluid_node(dw, true);
                st.mode = StepMode::ApplyVal;
                continue;
            }

            case ContKind::DynStatePop: {
                auto* dw = as_obj<DynWindObj>(cf->aux);
                v.current_dynwind = dw->parent;
                swap_fluid_node(dw, true);
                st.mode = StepMode::ApplyVal;
                continue;
            }

            case ContKind::ForceFinish: {
                auto* pr = as_obj<PromiseObj>(cf->aux);
                if (!pr->forced) {
                    pr->forced = true;
                    pr->expr_or_val = st.val;
                    pr->env = SCM_EOL;
                } else {
                    st.val = pr->expr_or_val;
                }
                st.mode = StepMode::ApplyVal;
                continue;
            }

            case ContKind::MapLoop: {
                int mode = static_cast<int>(cf->aux3.to_fixnum());
                if (mode == 2 && st.val.is_false()) {
                    st.mode = StepMode::ApplyVal;
                    continue;
                }
                if (mode == 3 && st.val.is_true()) {
                    st.mode = StepMode::ApplyVal;
                    continue;
                }
                SCM rev_results = (mode == 0) ? v.cons(st.val, cf->expr) : SCM_EOL;

                // Extract next elements from cf->aux2 (list of remaining lists)
                SCM rem = cf->aux2;
                bool any_empty = false;
                bool any_nonempty = false;
                std::vector<SCM> call_args;
                SCM next_h = SCM_EOL, next_t = SCM_EOL;
                while (is_pair(rem)) {
                    SCM l = car(rem);
                    if (l.is_null()) {
                        any_empty = true;
                        rem = cdr(rem);
                        continue;
                    }
                    if (!is_pair(l)) v.wrong_type_arg("map", 2, l, "list");
                    any_nonempty = true;
                    if (!any_empty) {
                        call_args.push_back(car(l));
                        SCM c = v.cons(cdr(l), SCM_EOL);
                        if (next_h.is_null()) next_h = next_t = c;
                        else { set_cdr(next_t, c); next_t = c; }
                    }
                    rem = cdr(rem);
                }
                if (any_empty) {
                    if (any_nonempty && (mode == 0 || mode == 1)) {
                        v.out_of_range(mode == 0 ? "map" : "for-each", cf->aux2);
                    }
                    if (mode == 0) {
                        SCM out = SCM_EOL;
                        SCM cur = rev_results;
                        while (is_pair(cur)) {
                            out = v.cons(car(cur), out);
                            cur = cdr(cur);
                        }
                        st.val = out;
                    }
                    else if (mode == 1) st.val = SCM_UNSPECIFIED;
                    // For and-map / or-map, st.val already holds the last result!
                    st.mode = StepMode::ApplyVal;
                    continue;
                }
                auto* ncf = v.heap().alloc<ContFrameObj>(ContKind::MapLoop, st.cont);
                ncf->expr = rev_results;
                ncf->aux = cf->aux;
                ncf->aux2 = next_h;
                ncf->aux3 = cf->aux3;
                st.cont = SCM::from_obj(ncf);
                st.mode = StepMode::ApplyProc;
                st.proc = cf->aux;
                st.args = std::move(call_args);
                continue;
            }

            default:
                return st.val;
            }
        } catch (const SchemeThrow& ex) {
            dispatch_throw(v, st, ex.key, ex.args, base_dw, my_depth);
        } catch (const ContinuationEscape& esc) {
            if (esc.target_vm_depth == -1 && esc.target_depth == my_depth) {
                switch_dynwind(esc.target_dw);
                st.cont = esc.target_cont;
                st.val = esc.return_val;
                st.mode = StepMode::ApplyVal;
                continue;
            }
            throw;
        }
    }
}

} // namespace

// ============================================================================
// Public VM eval / apply / call methods
// ============================================================================

SCM VM::eval(SCM expr, SCM env) {
    CekState st;
    st.mode = StepMode::Eval;
    st.expr = expr;
    st.env = env;
    st.cont = SCM_EOL;
    return run_cek(*this, st);
}

SCM VM::primitive_eval(SCM expr) {
    SCM xform = current_module_transformer();
    if (xform.is_true() && is_procedure(xform)) {
        expr = call1(xform, expr);
    }
    SCM tl = current_module_lookup_closure();
    SCM env = tl.is_true() ? list1(tl) : SCM_EOL;
    return eval(expr, env);
}

SCM VM::eval_string(std::string_view code) {
    SCM port = make_input_string_port(std::string(code));
    SCM res = SCM_UNSPECIFIED;
    while (true) {
        SCM form = read_from_port(port);
        if (form.is_eof()) break;
        res = primitive_eval(form);
    }
    return res;
}

void switch_dynwind_pub(SCM target_dw) {
    switch_dynwind(target_dw);
}

SCM VM::apply(SCM proc, const SCM* args, size_t nargs) {
    if (is_closure(proc)) {
        auto* cl = as_obj<ClosureObj>(proc);
        if (is_bytecode(cl->bytecode) || (cl->bytecode.is_false() && bytecode_compile_closure(cl))) {
            return bytecode_run_closure(cl, args, nargs);
        }
    } else if (is_subr(proc) && !debug_opts.debug) {
        auto* sub = as_obj<SubrObj>(proc);
        if (sub->special == SpecialSubr::None && !sub->rst &&
            nargs >= static_cast<size_t>(sub->req) &&
            nargs <= static_cast<size_t>(sub->req) + static_cast<size_t>(sub->opt)) {
            if (nargs == static_cast<size_t>(sub->req) && sub->opt == 0) {
                return sub->raw_fn ? sub->raw_fn(args, nargs) : sub->fn(args, nargs);
            }
            SCM full_args[16];
            size_t total = static_cast<size_t>(sub->req) + static_cast<size_t>(sub->opt);
            if (total <= 16) {
                for (size_t i = 0; i < nargs; ++i) full_args[i] = args[i];
                for (size_t i = nargs; i < total; ++i) full_args[i] = SCM_UNDEFINED;
                return sub->raw_fn ? sub->raw_fn(full_args, nargs) : sub->fn(full_args, nargs);
            }
        }
    }
    CekState st;
    st.mode = StepMode::ApplyProc;
    st.proc = proc;
    st.args.assign(args, args + nargs);
    st.cont = SCM_EOL;
    return run_cek(*this, st);
}

SCM VM::apply_list(SCM proc, SCM arg_list) {
    std::vector<SCM> v;
    SCM cur = arg_list;
    while (is_pair(cur)) { v.push_back(car(cur)); cur = cdr(cur); }
    return apply(proc, v.data(), v.size());
}

SCM VM::call0(SCM proc) { return apply(proc, nullptr, 0); }
SCM VM::call1(SCM proc, SCM a) { SCM args[1] = {a}; return apply(proc, args, 1); }
SCM VM::call2(SCM proc, SCM a, SCM b) { SCM args[2] = {a, b}; return apply(proc, args, 2); }
SCM VM::call3(SCM proc, SCM a, SCM b, SCM c) { SCM args[3] = {a, b, c}; return apply(proc, args, 3); }
SCM VM::call4(SCM proc, SCM a, SCM b, SCM c, SCM d) { SCM args[4] = {a, b, c, d}; return apply(proc, args, 4); }

// ============================================================================
// Stack & Backtrace Support
// ============================================================================

SCM build_stack_from_cont(SCM cont, SCM id) {
    auto* st = vm().heap().alloc<StackObj>();
    st->id = id;
    SCM cur = cont;
    int limit = 200;
    while (is_a<ContFrameObj>(cur, ObjTag::ContFrame) && limit-- > 0) {
        auto* cf = as_obj<ContFrameObj>(cur);
        if (cf->kind == ContKind::AppArgs || cf->kind == ContKind::AppOp) {
            auto* fr = vm().heap().alloc<FrameObj>();
            fr->is_proc = (cf->kind == ContKind::AppArgs);
            fr->proc = (cf->kind == ContKind::AppArgs) ? cf->aux : SCM_BOOL_F;
            fr->args = vm().list_n(cf->vals.data(), cf->vals.size());
            fr->source = cf->expr;
            st->frames.push_back(SCM::from_obj(fr));
        }
        cur = cf->next;
    }
    for (size_t i = 0; i < st->frames.size(); ++i) {
        auto* fr = as_obj<FrameObj>(st->frames[i]);
        if (i + 1 < st->frames.size()) fr->prev = st->frames[i + 1];
        if (i > 0) fr->next = st->frames[i - 1];
    }
    return SCM::from_obj(st);
}

void display_backtrace_impl(SCM stack, SCM port, int first, int depth) {
    auto* p = as_obj<PortObj>(port);
    if (!is_stack(stack)) return;
    auto* st = as_obj<StackObj>(stack);
    port_puts(p, "Backtrace:\n");
    int count = static_cast<int>(st->frames.size());
    int end = (depth > 0 && first + depth < count) ? (first + depth) : count;
    for (int i = first; i < end; ++i) {
        auto* fr = as_obj<FrameObj>(st->frames[static_cast<size_t>(i)]);
        port_puts(p, "  ");
        port_puts(p, std::to_string(i));
        port_puts(p, ": ");
        if (fr->proc.is_true()) {
            vm().write_to_port(fr->proc, port, false);
        } else {
            port_puts(p, "<eval>");
        }
        port_putc(p, '\n');
    }
}

void display_error_impl(SCM stack, SCM port, SCM subr, SCM message, SCM args, SCM rest) {
    auto* p = as_obj<PortObj>(port);
    if (is_string(subr)) {
        port_puts(p, as_obj<StringObj>(subr)->view());
        port_puts(p, ": ");
    } else if (is_symbol(subr)) {
        port_puts(p, as_obj<SymbolObj>(subr)->name);
        port_puts(p, ": ");
    }
    if (is_string(message)) {
        simple_format_to_port(port, as_obj<StringObj>(message)->view(), is_pair(args) ? args : SCM_EOL);
    } else {
        vm().write_to_port(message, port, true);
    }
    port_putc(p, '\n');
}

// ============================================================================
// Eval / Macro / Control / Stack Primitives Registration
// ============================================================================

void VM::init_eval() {
    the_last_stack_fluid = make_fluid(SCM_BOOL_F);
    define("the-last-stack", the_last_stack_fluid);

    // Register built-in syntax macros
    auto reg_syntax = [this](const char* name, BuiltinSyntax bs) -> SCM {
        SCM sym = make_symbol(name);
        auto* m = heap_.alloc<MacroObj>(MacroKind::BuiltinMacro, SCM_BOOL_F, sym);
        m->extra = static_cast<uint32_t>(bs);
        SCM ms = SCM::from_obj(m);
        define_sym(sym, ms);
        return ms;
    };
    reg_syntax("quote", BuiltinSyntax::Quote);
    reg_syntax("quasiquote", BuiltinSyntax::Quasiquote);
    reg_syntax("lambda", BuiltinSyntax::Lambda);
    reg_syntax("if", BuiltinSyntax::If);
    reg_syntax("set!", BuiltinSyntax::SetX);
    reg_syntax("define", BuiltinSyntax::Define);
    builtin_begin_macro = reg_syntax("begin", BuiltinSyntax::Begin);
    reg_syntax("let", BuiltinSyntax::Let);
    reg_syntax("let*", BuiltinSyntax::LetStar);
    reg_syntax("letrec", BuiltinSyntax::Letrec);
    reg_syntax("cond", BuiltinSyntax::Cond);
    reg_syntax("case", BuiltinSyntax::Case);
    reg_syntax("do", BuiltinSyntax::Do);
    reg_syntax("and", BuiltinSyntax::And);
    reg_syntax("or", BuiltinSyntax::Or);
    reg_syntax("delay", BuiltinSyntax::Delay);
    reg_syntax("@", BuiltinSyntax::At);
    reg_syntax("@@", BuiltinSyntax::AtAt);
    reg_syntax("the-environment", BuiltinSyntax::TheEnvironment);

    // defined? is a regular procedure in Guile 1.8.8: (defined? sym [module])
    define_proc("defined?", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_symbol(a[0])) wrong_type_arg("defined?", 1, a[0], "symbol");
        SCM mod = (nargs >= 2 && !a[1].is_undefined() && a[1].is_true()) ? a[1] : current_module();
        if (is_module(mod)) {
            SCM var = module_variable(mod, a[0]);
            return SCM::from_bool(is_variable(var) && !as_obj<VariableObj>(var)->value.is_undefined());
        }
        if (mod.is_false()) {
            SCM var = lookup_sym(a[0]);
            return SCM::from_bool(is_variable(var) && !as_obj<VariableObj>(var)->value.is_undefined());
        }
        // Lexical env
        SCM val = lookup_env_opt(a[0], mod);
        return SCM::from_bool(!val.is_undefined());
    });

    // CEK Special Subrs
    auto noop_fn = [](const SCM*, size_t) -> SCM { return SCM_UNSPECIFIED; };
    define_proc("apply", 2, 0, true, noop_fn, SpecialSubr::Apply);
    define_proc("@apply", 2, 0, true, noop_fn, SpecialSubr::Apply);
    define_proc("call-with-current-continuation", 1, 0, false, noop_fn, SpecialSubr::CallCC);
    define_proc("@call-with-current-continuation", 1, 0, false, noop_fn, SpecialSubr::CallCC);
    define_proc("call-with-values", 2, 0, false, noop_fn, SpecialSubr::CallWithValues);
    define_proc("@call-with-values", 2, 0, false, noop_fn, SpecialSubr::CallWithValues);
    define_proc("apply:nconc2last", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM lst = a[0];
        if (!is_pair(lst)) wrong_type_arg("apply", 2, lst, "list");
        if (cdr(lst).is_null()) {
            SCM last = car(lst);
            if (scm_ilength(last) < 0) wrong_type_arg("apply", 2, last, "proper list");
            return last;
        }
        SCM head = cons(car(lst), SCM_EOL);
        SCM tail = head;
        SCM cur = cdr(lst);
        while (is_pair(cdr(cur))) {
            SCM n = cons(car(cur), SCM_EOL);
            set_cdr(tail, n);
            tail = n;
            cur = cdr(cur);
        }
        SCM last = car(cur);
        if (scm_ilength(last) < 0) wrong_type_arg("apply", 2, last, "proper list");
        set_cdr(tail, last);
        return head;
    });
    define_proc("dynamic-wind", 3, 0, false, noop_fn, SpecialSubr::DynamicWind);
    define_proc("catch", 3, 1, false, noop_fn, SpecialSubr::Catch);
    define_proc("lazy-catch", 3, 0, false, noop_fn, SpecialSubr::LazyCatch);
    define_proc("with-throw-handler", 3, 0, false, noop_fn, SpecialSubr::WithThrowHandler);
    define_proc("with-fluid*", 3, 0, false, noop_fn, SpecialSubr::WithFluidStar);
    define_proc("with-fluids*", 3, 0, false, noop_fn, SpecialSubr::WithFluidsStar);
    define_proc("with-dynamic-state", 2, 0, false, noop_fn, SpecialSubr::WithDynamicState);
    define_proc("eval", 2, 0, false, noop_fn, SpecialSubr::Eval);
    define_proc("primitive-eval", 1, 0, false, noop_fn, SpecialSubr::PrimitiveEval);
    define_proc("local-eval", 1, 1, false, noop_fn, SpecialSubr::LocalEval);
    define_proc("force", 1, 0, false, noop_fn, SpecialSubr::Force);
    define_proc("map", 2, 0, true, noop_fn, SpecialSubr::Map);
    define_proc("map-in-order", 2, 0, true, noop_fn, SpecialSubr::Map);
    define_proc("for-each", 2, 0, true, noop_fn, SpecialSubr::ForEach);
    define_proc("and-map", 2, 0, true, noop_fn, SpecialSubr::AndMap);
    define_proc("or-map", 2, 0, true, noop_fn, SpecialSubr::OrMap);

    // Throw
    define_proc("throw", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_symbol(a[0])) wrong_type_arg("throw", 1, a[0], "symbol");
        SCM rest = nargs > 1 ? a[1] : SCM_EOL;
        throw_scm(a[0], rest);
    });

    // Values
    define_proc("values", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        return make_values(lst);
    });

    // Promises
    define_proc("promise?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_promise(a[0]));
    });

    // Procedures & Procedures with setter
    define_proc("procedure?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_procedure(a[0]));
    });
    define_proc("closure?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_closure(a[0]));
    });
    define_proc("thunk?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) return SCM_BOOL_F;
        if (is_subr(a[0])) return SCM::from_bool(as_obj<SubrObj>(a[0])->req == 0);
        if (is_closure(a[0])) {
            SCM f = as_obj<ClosureObj>(a[0])->formals;
            return SCM::from_bool(f.is_null() || is_symbol(f));
        }
        return SCM_BOOL_T;
    });
    define_proc("procedure-environment", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_closure(a[0])) {
            auto* cl = as_obj<ClosureObj>(a[0]);
            if (is_bytecode(cl->bytecode) && (cl->inline_upval_cnt > 0 || is_vector(cl->upvals))) {
                auto* bc = as_obj<BytecodeObj>(cl->bytecode);
                const SCM* uv_data = cl->inline_upval_cnt > 0 ? cl->inline_upvals : as_obj<VectorObj>(cl->upvals)->elts.data();
                size_t uv_len = cl->inline_upval_cnt > 0 ? static_cast<size_t>(cl->inline_upval_cnt) : as_obj<VectorObj>(cl->upvals)->elts.size();
                if (!bc->upval_syms.empty() && bc->upval_syms.size() == uv_len) {
                    SCM names_h = SCM_EOL, names_t = SCM_EOL;
                    SCM vals_h = SCM_EOL, vals_t = SCM_EOL;
                    for (size_t i = 0; i < bc->upval_syms.size(); ++i) {
                        SCM val = uv_data[i];
                        uint8_t k = i < bc->upval_kinds.size() ? bc->upval_kinds[i] : 0;
                        if (k == 1 && is_box(val)) val = as_obj<BoxObj>(val)->val;
                        else if (k == 2 && is_pair(val)) val = car(val);
                        else if (k == 3 && is_pair(val)) val = cdr(val);
                        SCM nc = cons(bc->upval_syms[i], SCM_EOL);
                        SCM vc = cons(val, SCM_EOL);
                        if (names_h.is_null()) { names_h = names_t = nc; vals_h = vals_t = vc; }
                        else { set_cdr(names_t, nc); names_t = nc; set_cdr(vals_t, vc); vals_t = vc; }
                    }
                    return cons(cons(names_h, vals_h), cl->env);
                }
            }
            return cl->env;
        }
        if (is_procedure(a[0])) return SCM_BOOL_F;
        wrong_type_arg("procedure-environment", 1, a[0], "procedure");
    });
    define_proc("make-procedure-with-setter", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("make-procedure-with-setter", 1, a[0], "procedure");
        if (!is_procedure(a[1])) wrong_type_arg("make-procedure-with-setter", 2, a[1], "procedure");
        return heap_.alloc_scm<ProcWithSetterObj>(a[0], a[1]);
    });
    define_proc("procedure-with-setter?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_proc_with_setter(a[0]));
    });
    define_proc("procedure", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_proc_with_setter(a[0])) return as_obj<ProcWithSetterObj>(a[0])->getter;
        wrong_type_arg("procedure", 1, a[0], "procedure-with-setter");
    });
    define_proc("setter", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_proc_with_setter(a[0])) return as_obj<ProcWithSetterObj>(a[0])->setter;
        if (is_struct(a[0])) {
            auto* so = as_obj<StructObj>(a[0]);
            uint32_t of = obj_class_flags(a[0]);
            if (((of & CLASSF_ENTITY) != 0 || so->is_entity) && so->entity_setter.is_true()) {
                return so->entity_setter;
            }
            if ((of & CLASSF_OPERATOR) != 0 && is_struct(so->vtable)) {
                auto* vt = as_obj<StructObj>(so->vtable);
                if (vt->slots.size() > 4 && SCM(vt->slots[4]).is_true()) {
                    return SCM(vt->slots[4]);
                }
            }
        }
        // Standard R5RS / Guile setters
        if (is_subr(a[0])) {
            const std::string& nm = as_obj<SubrObj>(a[0])->name;
            auto get_var = [this](const char* s) -> SCM {
                SCM v = lookup(s);
                return is_variable(v) ? as_obj<VariableObj>(v)->value : SCM_BOOL_F;
            };
            if (nm == "car") return get_var("set-car!");
            if (nm == "cdr") return get_var("set-cdr!");
            if (nm == "vector-ref") return get_var("vector-set!");
            if (nm == "string-ref") return get_var("string-set!");
            if (nm == "struct-ref") return get_var("struct-set!");
            if (nm == "fluid-ref") return get_var("fluid-set!");
            if (nm == "variable-ref") return get_var("variable-set!");
        }
        wrong_type_arg("setter", 1, a[0], "procedure-with-setter");
    });

    // Macros
    define_proc("procedure->syntax", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("procedure->syntax", 1, a[0], "procedure");
        return make_macro(MacroKind::Syntax, a[0]);
    });
    define_proc("procedure->macro", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("procedure->macro", 1, a[0], "procedure");
        return make_macro(MacroKind::Macro, a[0]);
    });
    define_proc("procedure->memoizing-macro", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("procedure->memoizing-macro", 1, a[0], "procedure");
        return make_macro(MacroKind::MemoizingMacro, a[0]);
    });
    define_proc("macro?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_macro(a[0]));
    });
    define_proc("macro-type", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_macro(a[0])) return SCM_BOOL_F;
        switch (as_obj<MacroObj>(a[0])->kind) {
        case MacroKind::Syntax: return make_symbol("syntax");
        case MacroKind::Macro: return make_symbol("macro");
        case MacroKind::MemoizingMacro: return make_symbol("macro!");
        case MacroKind::BuiltinMacro: return make_symbol("builtin-macro!");
        }
        return SCM_BOOL_F;
    });
    define_proc("macro-name", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_macro(a[0])) wrong_type_arg("macro-name", 1, a[0], "macro");
        auto* m = as_obj<MacroObj>(a[0]);
        if (m->name.is_true()) return m->name;
        if (is_procedure(m->transformer)) return procedure_property(m->transformer, sym_name);
        return SCM_BOOL_F;
    });
    define_proc("macro-transformer", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_macro(a[0])) wrong_type_arg("macro-transformer", 1, a[0], "macro");
        return as_obj<MacroObj>(a[0])->transformer;
    });

    // evalext
    define_proc("self-evaluating?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(!is_pair(a[0]) && !is_symbol(a[0]) && !a[0].is_null());
    });
    define_proc("eval-string", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("eval-string", 1, a[0], "string");
        return eval_string(as_obj<StringObj>(a[0])->view());
    });

    // Stack & Backtrace primitives
    define_proc("make-stack", 1, 0, true, [this](const SCM* a, size_t) -> SCM {
        if (is_stack(a[0])) return a[0];
        if (is_continuation(a[0])) {
            auto* k = as_obj<ContinuationObj>(a[0]);
            SCM stk_scm = build_stack_from_cont(k->cont_frame, a[0]);
            if (k->has_vm_snap && k->snap_frames_size() > 0) {
                auto* st = as_obj<StackObj>(stk_scm);
                const VmFrameSnap* fd = k->snap_frames_data();
                size_t fn = k->snap_frames_size();
                std::vector<SCM> combined;
                combined.reserve(fn + st->frames.size());
                for (size_t i = fn; i > 0; --i) {
                    auto* fr = heap_.alloc<FrameObj>();
                    fr->proc = fd[i - 1].closure;
                    fr->is_proc = true;
                    combined.push_back(SCM::from_obj(fr));
                }
                for (SCM f : st->frames) combined.push_back(f);
                st->frames.swap(combined);
                for (size_t i = 0; i < st->frames.size(); ++i) {
                    auto* fr = as_obj<FrameObj>(st->frames[i]);
                    fr->prev = (i + 1 < st->frames.size()) ? st->frames[i + 1] : SCM_BOOL_F;
                    fr->next = (i > 0) ? st->frames[i - 1] : SCM_BOOL_F;
                }
            }
            return stk_scm;
        }
        return build_stack_from_cont(current_cont, a[0]);
    });
    define_proc("stack?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_stack(a[0])); });
    define_proc("stack-id", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_stack(a[0])) return as_obj<StackObj>(a[0])->id;
        if (is_continuation(a[0])) return make_symbol("continuation");
        if (a[0] == SCM_BOOL_T) return SCM_BOOL_T;
        wrong_type_arg("stack-id", 1, a[0], "stack");
    });
    define_proc("stack-length", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_stack(a[0])) wrong_type_arg("stack-length", 1, a[0], "stack");
        return make_uinteger(as_obj<StackObj>(a[0])->frames.size());
    });
    define_proc("stack-ref", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_stack(a[0])) wrong_type_arg("stack-ref", 1, a[0], "stack");
        int64_t idx = scm_to_int64(a[1], "stack-ref", 2);
        auto* st = as_obj<StackObj>(a[0]);
        if (idx < 0 || static_cast<size_t>(idx) >= st->frames.size()) out_of_range("stack-ref", a[1]);
        return st->frames[static_cast<size_t>(idx)];
    });
    define_proc("frame?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_frame(a[0])); });
    define_proc("frame-procedure?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_frame(a[0])) wrong_type_arg("frame-procedure?", 1, a[0], "frame");
        return SCM::from_bool(as_obj<FrameObj>(a[0])->is_proc);
    });
    define_proc("frame-procedure", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_frame(a[0])) wrong_type_arg("frame-procedure", 1, a[0], "frame");
        return as_obj<FrameObj>(a[0])->proc;
    });
    define_proc("frame-arguments", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_frame(a[0])) wrong_type_arg("frame-arguments", 1, a[0], "frame");
        return as_obj<FrameObj>(a[0])->args;
    });
    define_proc("frame-source", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_frame(a[0])) wrong_type_arg("frame-source", 1, a[0], "frame");
        return as_obj<FrameObj>(a[0])->source;
    });
    define_proc("frame-next", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_frame(a[0])) wrong_type_arg("frame-next", 1, a[0], "frame");
        return as_obj<FrameObj>(a[0])->next;
    });
    define_proc("frame-previous", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_frame(a[0])) wrong_type_arg("frame-previous", 1, a[0], "frame");
        return as_obj<FrameObj>(a[0])->prev;
    });
    define_proc("last-stack-frame", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM cont = is_continuation(a[0]) ? as_obj<ContinuationObj>(a[0])->cont_frame : current_cont;
        SCM stk = build_stack_from_cont(cont, a[0]);
        auto* s = as_obj<StackObj>(stk);
        if (s->frames.empty()) return SCM_BOOL_F;
        return cons(s->frames.front(), SCM_EOL);
    });
    define_proc("display-backtrace", 2, 2, false, [](const SCM* a, size_t nargs) -> SCM {
        int first = (nargs >= 3 && !a[2].is_undefined() && a[2].is_true()) ? static_cast<int>(scm_to_int64(a[2], "display-backtrace", 3)) : 0;
        int depth = (nargs >= 4 && !a[3].is_undefined() && a[3].is_true()) ? static_cast<int>(scm_to_int64(a[3], "display-backtrace", 4)) : -1;
        display_backtrace_impl(a[0], a[1], first, depth);
        return SCM_UNSPECIFIED;
    });
    define_proc("display-error", 6, 0, false, [](const SCM* a, size_t) -> SCM {
        display_error_impl(a[0], a[1], a[2], a[3], a[4], a[5]);
        return SCM_UNSPECIFIED;
    });
    define_proc("display-application", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM port = (nargs >= 2 && is_port(a[1])) ? a[1] : current_output_port();
        write_to_port(a[0], port, false);
        return SCM_UNSPECIFIED;
    });
    // start-stack syntax: (start-stack id exp) => exp
    define("start-stack", make_macro(MacroKind::MemoizingMacro,
        make_subr("start-stack", 2, 0, false, [](const SCM* a, size_t) -> SCM {
            SCM form = a[0];
            return (is_pair(form) && is_pair(cdr(form)) && is_pair(cdr(cdr(form)))) ? car(cdr(cdr(form))) : SCM_UNSPECIFIED;
        }), make_symbol("start-stack")));

    // @bind dynamic scoping syntax: (@bind ((var val) ...) body ...)
    define("@bind", make_macro(MacroKind::MemoizingMacro,
        make_subr("@bind", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
            SCM form = a[0];
            if (!is_pair(form) || !is_pair(cdr(form)) || !is_pair(cdr(cdr(form)))) {
                error(sym_syntax_error, "@bind", "Missing expression");
            }
            SCM bindings = car(cdr(form));
            SCM body = cdr(cdr(form));
            std::vector<SCM> vars, vals, tmps, olds;
            SCM cur = bindings;
            while (is_pair(cur)) {
                SCM b = car(cur);
                if (!is_pair(b) || !is_symbol(car(b)) || !is_pair(cdr(b)) || !cdr(cdr(b)).is_null()) {
                    error(sym_syntax_error, "@bind", "Bad binding: ~S", list1(b));
                }
                SCM v = car(b);
                for (SCM prev : vars) {
                    if (prev == v) error(sym_syntax_error, "@bind", "Duplicate binding: ~S", list1(v));
                }
                vars.push_back(v);
                vals.push_back(car(cdr(b)));
                tmps.push_back(make_uninterned_symbol("bind-tmp"));
                olds.push_back(make_uninterned_symbol("bind-old"));
                cur = cdr(cur);
            }
            if (!cur.is_null()) error(sym_syntax_error, "@bind", "Bad binding: ~S", list1(bindings));
            // Build (let ((tmp val) ...) (dynamic-wind swap (lambda () body ...) swap))
            SCM let_binds = SCM_EOL;
            for (size_t i = vars.size(); i > 0; --i) {
                let_binds = cons(list2(tmps[i - 1], vals[i - 1]), let_binds);
            }
            SCM swap_binds = SCM_EOL;
            SCM swap_body = SCM_EOL;
            for (size_t i = vars.size(); i > 0; --i) {
                swap_binds = cons(list2(olds[i - 1], vars[i - 1]), swap_binds);
                swap_body = cons(list3(sym_set_x, tmps[i - 1], olds[i - 1]), swap_body);
                swap_body = cons(list3(sym_set_x, vars[i - 1], tmps[i - 1]), swap_body);
            }
            SCM swap_thunk = list3(sym_lambda, SCM_EOL,
                vars.empty() ? SCM_BOOL_F : cons(sym_let, cons(swap_binds, swap_body)));
            SCM body_thunk = cons(sym_lambda, cons(SCM_EOL, body));
            SCM dw_call = list4(make_symbol("dynamic-wind"), swap_thunk, body_thunk, swap_thunk);
            return list3(sym_let, let_binds, dw_call);
        }), make_symbol("@bind")));
}

} // namespace guile
