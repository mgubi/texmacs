#include "guile_cpp/vm.hpp"
#include "guile_cpp/bytecode_ops.hpp"
#include <algorithm>
#include <cmath>
#include <cstring>
#include <unordered_map>
#include <unordered_set>

namespace guile {

// ============================================================================
// Fast Builtin IDs (registered once in VM::init_bytecode_builtins)
// ============================================================================

enum FastBuiltinId : uint8_t {
    FB_NONE = 0,
    // Arith group (guarded by VM::fast_arith_ok)
    FB_ADD,
    FB_SUB,
    FB_MUL,
    FB_DIV,
    FB_ABS,
    FB_ADD1,
    FB_SUB1,
    FB_NUMEQ,
    FB_LT,
    FB_LE,
    FB_GT,
    FB_GE,
    FB_EVENP,
    FB_ODDP,
    // Core group (guarded by VM::fast_core_ok)
    FB_EQ,
    FB_NULLP,
    FB_PAIRP,
    FB_NOT,
    FB_CAR,
    FB_CDR,
    FB_CONS,
    FB_REVERSE,
    FB_VEC_REF,
    FB_VEC_SET,
    FB_MAKE_VEC,
    FB_STRUCT_REF,
    FB_STRUCT_SET,
    FB_HASHV_REF,
    FB_HASHV_SET,
    FB_HASHQ_REF,
    FB_HASHQ_SET,
    FB_CALLCC,
};

void VM::init_bytecode_builtins() {
    auto reg = [&](const char* name, FastBuiltinId id, uint8_t flag) {
        SCM var = lookup(name);
        if (is_variable(var)) {
            as_obj<VariableObj>(var)->flags |= flag;
            fast_builtin_vars[var] = static_cast<uint8_t>(id);
        }
    };
    reg("+", FB_ADD, VAR_FLAG_FAST_ARITH);
    reg("-", FB_SUB, VAR_FLAG_FAST_ARITH);
    reg("*", FB_MUL, VAR_FLAG_FAST_ARITH);
    reg("/", FB_DIV, VAR_FLAG_FAST_ARITH);
    reg("abs", FB_ABS, VAR_FLAG_FAST_ARITH);
    reg("1+", FB_ADD1, VAR_FLAG_FAST_ARITH);
    reg("1-", FB_SUB1, VAR_FLAG_FAST_ARITH);
    reg("=", FB_NUMEQ, VAR_FLAG_FAST_ARITH);
    reg("<", FB_LT, VAR_FLAG_FAST_ARITH);
    reg("<=", FB_LE, VAR_FLAG_FAST_ARITH);
    reg(">", FB_GT, VAR_FLAG_FAST_ARITH);
    reg(">=", FB_GE, VAR_FLAG_FAST_ARITH);
    reg("even?", FB_EVENP, VAR_FLAG_FAST_ARITH);
    reg("odd?", FB_ODDP, VAR_FLAG_FAST_ARITH);

    reg("eq?", FB_EQ, VAR_FLAG_FAST_CORE);
    reg("null?", FB_NULLP, VAR_FLAG_FAST_CORE);
    reg("pair?", FB_PAIRP, VAR_FLAG_FAST_CORE);
    reg("not", FB_NOT, VAR_FLAG_FAST_CORE);
    reg("car", FB_CAR, VAR_FLAG_FAST_CORE);
    reg("cdr", FB_CDR, VAR_FLAG_FAST_CORE);
    reg("cons", FB_CONS, VAR_FLAG_FAST_CORE);
    reg("reverse", FB_REVERSE, VAR_FLAG_FAST_CORE);
    reg("vector-ref", FB_VEC_REF, VAR_FLAG_FAST_CORE);
    reg("vector-set!", FB_VEC_SET, VAR_FLAG_FAST_CORE);
    reg("make-vector", FB_MAKE_VEC, VAR_FLAG_FAST_CORE);
    reg("struct-ref", FB_STRUCT_REF, VAR_FLAG_FAST_CORE);
    reg("struct-set!", FB_STRUCT_SET, VAR_FLAG_FAST_CORE);
    reg("hashv-ref", FB_HASHV_REF, VAR_FLAG_FAST_CORE);
    reg("hashv-set!", FB_HASHV_SET, VAR_FLAG_FAST_CORE);
    reg("hashq-ref", FB_HASHQ_REF, VAR_FLAG_FAST_CORE);
    reg("hashq-set!", FB_HASHQ_SET, VAR_FLAG_FAST_CORE);
    reg("call-with-current-continuation", FB_CALLCC, VAR_FLAG_FAST_CALLCC);
    reg("@call-with-current-continuation", FB_CALLCC, VAR_FLAG_FAST_CALLCC);
    reg("call/cc", FB_CALLCC, VAR_FLAG_FAST_CALLCC);
}

// ============================================================================
// Builtin Syntax IDs (must match eval.cpp BuiltinSyntax enum!)
// ============================================================================
namespace {

enum class BSyn : uint32_t {
    None = 0,
    Quote, Quasiquote, Lambda, If, SetX, Define, Begin,
    Let, LetStar, Letrec, Cond, Case, Do, And, Or, Delay,
    At, AtAt, TheEnvironment, DefinedP, EvalCase,
};

BSyn get_bsyn(SCM val) noexcept {
    if (is_macro(val) && as_obj<MacroObj>(val)->kind == MacroKind::BuiltinMacro) {
        return static_cast<BSyn>(as_obj<MacroObj>(val)->extra);
    }
    return BSyn::None;
}

SCM top_level_eval_closure(SCM env) noexcept {
    while (is_pair(env)) {
        SCM a = car(env);
        if (!is_pair(a)) return a;
        env = cdr(env);
    }
    return SCM_BOOL_F;
}

// ============================================================================
// Compiler
// ============================================================================

struct OuterCekBinding {
    SCM sym;
    SCM cell;       // PairObj*
    bool is_cdr;    // false => car(cell), true => cdr(cell)
};

enum class UpvalKind : uint8_t {
    Val = 0,       // immutable value
    Box = 1,       // BoxObj*
    CellCar = 2,   // CEK PairObj* (car)
    CellCdr = 3,   // CEK PairObj* (cdr)
};

struct LocalVar {
    SCM sym;
    uint16_t slot;
    bool boxed = false;
    bool check_undef = false;
    bool is_loop = false;
    int loop_id = -1;
    SCM known_bc = SCM_BOOL_F; // BytecodeObj* if immutable all-lambda letrec binding
};

struct UpvalInfo {
    SCM sym;
    UpvalKind kind;
    bool from_parent_local; // true => parent local slot; false => parent upval idx
    uint16_t parent_index;
    bool check_undef = false;
    SCM known_bc = SCM_BOOL_F;
};

struct NamedLoopInfo {
    SCM name;
    uint32_t start_pc;
    std::vector<uint16_t> param_slots;
};

struct FnCompiler {
    FnCompiler* parent = nullptr;
    BytecodeObj* bc = nullptr;
    SCM eval_closure = SCM_BOOL_F;
    const std::vector<OuterCekBinding>* cek_outer = nullptr;

    std::vector<LocalVar> locals;
    std::vector<UpvalInfo> upvals;
    std::vector<NamedLoopInfo> loops;
    std::vector<SCM> mutated_syms;  // any sym targeted by set! anywhere in subtree
    std::vector<SCM> captured_syms; // any sym referenced inside an inner lambda
    uint16_t next_slot = 0;
    uint16_t max_slot = 0;
    bool failed = false;

    static bool vec_has(const std::vector<SCM>& vec, SCM s) noexcept {
        for (SCM x : vec) if (x == s) return true;
        return false;
    }
    static void vec_add(std::vector<SCM>& vec, SCM s) {
        for (SCM x : vec) if (x == s) return;
        vec.push_back(s);
    }

    uint16_t alloc_slot() {
        uint16_t s = next_slot++;
        if (next_slot > max_slot) max_slot = next_slot;
        return s;
    }

    void emit(uint32_t insn) { bc->code.push_back(insn); }

    uint32_t emit_jump(Op op) {
        uint32_t idx = static_cast<uint32_t>(bc->code.size());
        emit(enc_s16(op, 0));
        return idx;
    }

    bool patch_jump(uint32_t idx) {
        int32_t delta = static_cast<int32_t>(bc->code.size()) - static_cast<int32_t>(idx) - 1;
        if (delta < -32768 || delta > 32767) { failed = true; return false; }
        Op op = static_cast<Op>(dec_op(bc->code[idx]));
        bc->code[idx] = enc_s16(op, static_cast<int16_t>(delta));
        return true;
    }

    uint32_t add_const(SCM v) {
        for (size_t i = 0; i < bc->consts.size(); ++i) {
            if (bc->consts[i] == v) return static_cast<uint32_t>(i);
        }
        uint32_t idx = static_cast<uint32_t>(bc->consts.size());
        bc->consts.push_back(v);
        return idx;
    }

    uint16_t add_global(SCM sym) {
        for (size_t i = 0; i < bc->global_syms.size(); ++i) {
            if (bc->global_syms[i] == sym) return static_cast<uint16_t>(i);
        }
        uint16_t idx = static_cast<uint16_t>(bc->global_syms.size());
        bc->global_syms.push_back(sym);
        SCM var = vm().sym2var(sym, eval_closure, false);
        bc->globals.push_back(is_variable(var) ? var : SCM_BOOL_F);
        return idx;
    }

    // Lookup symbol in current locals (returns index in locals, or -1)
    int find_local(SCM sym) const {
        for (int i = static_cast<int>(locals.size()) - 1; i >= 0; --i) {
            if (locals[static_cast<size_t>(i)].sym == sym) return i;
        }
        return -1;
    }

    bool is_lexically_bound(SCM sym) const {
        if (find_local(sym) >= 0) return true;
        if (parent) return parent->is_lexically_bound(sym);
        if (cek_outer) {
            for (const auto& b : *cek_outer) if (b.sym == sym) return true;
        }
        return false;
    }

    // Resolve as upvalue (recursively capturing through parent compilers or CEK outer bindings)
    // Returns upval index in `upvals`, or -1 if global
    int resolve_upval(SCM sym, UpvalKind& out_kind, bool& out_chk, SCM* out_known_bc = nullptr) {
        for (size_t i = 0; i < upvals.size(); ++i) {
            if (upvals[i].sym == sym) {
                out_kind = upvals[i].kind;
                out_chk = upvals[i].check_undef;
                if (out_known_bc) *out_known_bc = upvals[i].known_bc;
                return static_cast<int>(i);
            }
        }
        if (parent) {
            int pl = parent->find_local(sym);
            if (pl >= 0) {
                const auto& lv = parent->locals[static_cast<size_t>(pl)];
                UpvalKind k = lv.boxed ? UpvalKind::Box : UpvalKind::Val;
                int idx = static_cast<int>(upvals.size());
                upvals.push_back({sym, k, true, lv.slot, lv.check_undef, lv.known_bc});
                bc->upval_descs.push_back((1u << 16) | lv.slot);
                bc->upval_syms.push_back(sym);
                bc->upval_kinds.push_back(static_cast<uint8_t>(k));
                bc->upval_known_bc.push_back(lv.known_bc);
                out_kind = k;
                out_chk = lv.check_undef;
                if (out_known_bc) *out_known_bc = lv.known_bc;
                return idx;
            }
            UpvalKind pk;
            bool pchk = false;
            SCM pknown = SCM_BOOL_F;
            int pu = parent->resolve_upval(sym, pk, pchk, &pknown);
            if (pu >= 0) {
                int idx = static_cast<int>(upvals.size());
                upvals.push_back({sym, pk, false, static_cast<uint16_t>(pu), pchk, pknown});
                bc->upval_descs.push_back(static_cast<uint32_t>(pu));
                bc->upval_syms.push_back(sym);
                bc->upval_kinds.push_back(static_cast<uint8_t>(pk));
                bc->upval_known_bc.push_back(pknown);
                out_kind = pk;
                out_chk = pchk;
                if (out_known_bc) *out_known_bc = pknown;
                return idx;
            }
            return -1;
        }
        if (cek_outer) {
            for (size_t i = 0; i < cek_outer->size(); ++i) {
                if ((*cek_outer)[i].sym == sym) {
                    UpvalKind k = (*cek_outer)[i].is_cdr ? UpvalKind::CellCdr : UpvalKind::CellCar;
                    int idx = static_cast<int>(upvals.size());
                    upvals.push_back({sym, k, false, static_cast<uint16_t>(i), false, SCM_BOOL_F});
                    bc->upval_descs.push_back((2u << 16) | static_cast<uint32_t>(i));
                    bc->upval_syms.push_back(sym);
                    bc->upval_kinds.push_back(static_cast<uint8_t>(k));
                    bc->upval_known_bc.push_back(SCM_BOOL_F);
                    out_kind = k;
                    out_chk = false;
                    if (out_known_bc) *out_known_bc = SCM_BOOL_F;
                    return idx;
                }
            }
        }
        return -1;
    }

    // Lookup global syntax (returns BSyn::None if lexically shadowed or not a builtin macro)
    BSyn resolve_bsyn(SCM sym) const {
        if (is_macro(sym)) return get_bsyn(sym);
        if (!is_symbol(sym)) return BSyn::None;
        if (is_lexically_bound(sym)) return BSyn::None;
        SCM var = vm().sym2var(sym, eval_closure, false);
        if (!is_variable(var)) return BSyn::None;
        return get_bsyn(as_obj<VariableObj>(var)->value);
    }

    // Lookup fast builtin op for a global call (only if not shadowed & root var matches!)
    FastBuiltinId resolve_fast_builtin(SCM sym) const {
        if (!is_symbol(sym) || is_lexically_bound(sym)) return FB_NONE;
        SCM var = vm().sym2var(sym, eval_closure, false);
        if (!is_variable(var)) return FB_NONE;
        auto it = vm().fast_builtin_vars.find(var);
        if (it == vm().fast_builtin_vars.end()) return FB_NONE;
        return static_cast<FastBuiltinId>(it->second);
    }

    // Pre-scan AST to find all mutated symbols and symbols captured by inner lambdas.
    // Uses direct symbol equality (100% sound over-approximation, zero sym2var calls!).
    void prescan(SCM expr, bool in_inner_lambda) {
        if (is_symbol(expr)) {
            if (in_inner_lambda) vec_add(captured_syms, expr);
            return;
        }
        if (!is_pair(expr)) return;
        SCM head = car(expr);
        SCM rest = cdr(expr);
        if (is_symbol(head)) {
            auto& v = vm();
            if (head == v.sym_quote) return;
            if (head == v.sym_set_x && is_pair(rest)) {
                if (is_symbol(car(rest))) {
                    vec_add(mutated_syms, car(rest));
                    if (in_inner_lambda) vec_add(captured_syms, car(rest));
                }
                if (is_pair(cdr(rest))) prescan(car(cdr(rest)), in_inner_lambda);
                return;
            }
            if (head == v.sym_lambda && is_pair(rest)) {
                SCM b = cdr(rest);
                while (is_pair(b)) { prescan(car(b), true); b = cdr(b); }
                return;
            }
            if (head == v.sym_define && is_pair(rest)) {
                SCM target = car(rest);
                SCM b = cdr(rest);
                bool fn_def = is_pair(target);
                while (is_pair(b)) { prescan(car(b), in_inner_lambda || fn_def); b = cdr(b); }
                return;
            }
            if (head == v.sym_delay && is_pair(rest)) {
                prescan(car(rest), true);
                return;
            }
            if ((head == v.sym_let || head == v.sym_let_star || head == v.sym_letrec) && is_pair(rest)) {
                SCM b = car(rest);
                SCM body = cdr(rest);
                if (head == v.sym_let && is_symbol(b) && is_pair(body)) {
                    b = car(body);
                    body = cdr(body);
                }
                while (is_pair(b)) {
                    if (is_pair(car(b)) && is_pair(cdr(car(b)))) {
                        prescan(car(cdr(car(b))), in_inner_lambda);
                    }
                    b = cdr(b);
                }
                while (is_pair(body)) { prescan(car(body), in_inner_lambda); body = cdr(body); }
                return;
            }
        }
        SCM cur = expr;
        while (is_pair(cur)) {
            prescan(car(cur), in_inner_lambda);
            cur = cdr(cur);
        }
    }

    // Check if a named-let loop variable is ONLY called in tail position (never captured/escaped/non-tail/set!)
    bool can_inline_named_let(SCM loop_sym, size_t arity, SCM body, bool in_tail) const {
        if (vec_has(mutated_syms, loop_sym)) return false;
        // Walk a sequence of body expressions
        SCM cur = body;
        while (is_pair(cur)) {
            bool is_last = cdr(cur).is_null();
            if (!check_loop_expr(loop_sym, arity, car(cur), in_tail && is_last)) return false;
            cur = cdr(cur);
        }
        return cur.is_null();
    }

    bool check_loop_expr(SCM loop_sym, size_t arity, SCM expr, bool in_tail) const {
        if (expr == loop_sym) return false; // escaped as value!
        if (!is_pair(expr)) return true;
        SCM head = car(expr);
        SCM args = cdr(expr);
        if (head == loop_sym) {
            if (!in_tail) return false;
            size_t cnt = 0;
            SCM a = args;
            while (is_pair(a)) {
                if (!check_loop_expr(loop_sym, arity, car(a), false)) return false;
                ++cnt;
                a = cdr(a);
            }
            return a.is_null() && cnt == arity;
        }
        if (is_symbol(head)) {
            BSyn bs = resolve_bsyn(head);
            switch (bs) {
            case BSyn::Quote:
                return true;
            case BSyn::If: {
                if (!is_pair(args) || !is_pair(cdr(args))) return false;
                if (!check_loop_expr(loop_sym, arity, car(args), false)) return false;
                if (!check_loop_expr(loop_sym, arity, car(cdr(args)), in_tail)) return false;
                SCM r2 = cdr(cdr(args));
                if (is_pair(r2)) return check_loop_expr(loop_sym, arity, car(r2), in_tail);
                return r2.is_null();
            }
            case BSyn::Begin:
                return can_inline_named_let(loop_sym, arity, args, in_tail);
            case BSyn::And:
            case BSyn::Or: {
                SCM cur = args;
                while (is_pair(cur)) {
                    bool is_last = cdr(cur).is_null();
                    if (!check_loop_expr(loop_sym, arity, car(cur), in_tail && is_last)) return false;
                    cur = cdr(cur);
                }
                return cur.is_null();
            }
            case BSyn::Let:
            case BSyn::LetStar:
            case BSyn::Letrec: {
                if (!is_pair(args) || !is_pair(cdr(args))) return false;
                SCM b = car(args);
                SCM body = cdr(args);
                if (bs == BSyn::Let && is_symbol(b)) {
                    if (b == loop_sym) return true; // shadowed!
                    if (!is_pair(body) || !is_pair(cdr(body))) return false;
                    b = car(body);
                    body = cdr(body);
                }
                bool shadowed = false;
                while (is_pair(b)) {
                    SCM entry = car(b);
                    if (!is_pair(entry) || !is_pair(cdr(entry))) return false;
                    if (bs == BSyn::LetStar && shadowed) {
                        // already shadowed for subsequent inits
                    } else if (!check_loop_expr(loop_sym, arity, car(cdr(entry)), false)) {
                        return false;
                    }
                    if (car(entry) == loop_sym) shadowed = true;
                    b = cdr(b);
                }
                if (shadowed) return true;
                return can_inline_named_let(loop_sym, arity, body, in_tail);
            }
            case BSyn::Cond: {
                SCM c = args;
                while (is_pair(c)) {
                    SCM cl = car(c);
                    if (!is_pair(cl)) return false;
                    if (car(cl) == vm().sym_else && !is_lexically_bound(vm().sym_else)) {
                        if (!can_inline_named_let(loop_sym, arity, cdr(cl), in_tail)) return false;
                    } else {
                        if (!check_loop_expr(loop_sym, arity, car(cl), false)) return false;
                        SCM cb = cdr(cl);
                        if (is_pair(cb) && car(cb) == vm().sym_arrow && !is_lexically_bound(vm().sym_arrow)) {
                            return false; // conservative for =>
                        }
                        if (!can_inline_named_let(loop_sym, arity, cb, in_tail)) return false;
                    }
                    c = cdr(c);
                }
                return c.is_null();
            }
            case BSyn::Lambda:
                return !expr_mentions(loop_sym, args);
            default:
                break;
            }
        }
        // General call (not tail for arguments!)
        SCM cur = expr;
        while (is_pair(cur)) {
            if (!check_loop_expr(loop_sym, arity, car(cur), false)) return false;
            cur = cdr(cur);
        }
        return cur.is_null();
    }

    bool expr_mentions(SCM sym, SCM e) const {
        if (e == sym) return true;
        if (!is_pair(e)) return false;
        while (is_pair(e)) {
            if (expr_mentions(sym, car(e))) return true;
            e = cdr(e);
        }
        return e == sym;
    }

    // Check for leading internal defines in a body (if any, bail to CEK if complex, or desugar simple defines to letrec!)
    bool compile_body(SCM body, bool tail) {
        if (!is_pair(body)) { failed = true; return false; }
        // Check if body starts with internal define (or begin containing define)
        SCM first = car(body);
        if (is_pair(first) && is_symbol(car(first))) {
            BSyn bs = resolve_bsyn(car(first));
            if (bs == BSyn::Define) {
                // Desugar leading (define ...) forms into a letrec
                SCM bindings_rev = SCM_EOL;
                SCM cur = body;
                while (is_pair(cur) && is_pair(car(cur)) && is_symbol(car(car(cur))) &&
                       resolve_bsyn(car(car(cur))) == BSyn::Define) {
                    SCM form = car(cur);
                    SCM rest = cdr(form);
                    if (!is_pair(rest)) { failed = true; return false; }
                    SCM target = car(rest), dbody = cdr(rest);
                    SCM nm = SCM_BOOL_F, ex = SCM_BOOL_F;
                    if (is_symbol(target)) {
                        nm = target;
                        if (dbody.is_null()) ex = SCM_UNSPECIFIED;
                        else if (is_pair(dbody) && cdr(dbody).is_null()) ex = car(dbody);
                        else { failed = true; return false; }
                    } else if (is_pair(target)) {
                        if (dbody.is_null()) { failed = true; return false; }
                        ex = vm().cons(vm().sym_lambda, vm().cons(cdr(target), dbody));
                        target = car(target);
                        while (is_pair(target)) {
                            ex = vm().list3(vm().sym_lambda, cdr(target), ex);
                            target = car(target);
                        }
                        if (!is_symbol(target)) { failed = true; return false; }
                        nm = target;
                    } else {
                        failed = true; return false;
                    }
                    bindings_rev = vm().cons(vm().list2(nm, ex), bindings_rev);
                    cur = cdr(cur);
                }
                if (!is_pair(cur)) { failed = true; return false; } // missing body expr -> let CEK throw exact error
                // Check if remaining body still has a define or begin-define -> fallback to CEK
                if (is_pair(car(cur)) && is_symbol(car(car(cur))) &&
                    resolve_bsyn(car(car(cur))) == BSyn::Begin) {
                    failed = true; return false;
                }
                SCM letrec_form = vm().cons(vm().sym_letrec, vm().cons(scm_reverse(bindings_rev), cur));
                if (body == bc->source_body) {
                    bc->source_body = vm().list1(letrec_form);
                }
                return compile_expr(letrec_form, tail);
            }
            if (bs == BSyn::Begin) {
                // If leading begin contains define, let CEK handle internal define flattening
                SCM b = cdr(first);
                if (b.is_null() || (is_pair(b) && is_pair(car(b)) && is_symbol(car(car(b))) &&
                                    resolve_bsyn(car(car(b))) == BSyn::Define)) {
                    failed = true; return false;
                }
            }
        }
        SCM cur = body;
        while (is_pair(cur)) {
            bool is_last = cdr(cur).is_null();
            if (!compile_expr(car(cur), tail && is_last)) return false;
            if (!is_last) emit(enc0(OP_POP));
            cur = cdr(cur);
        }
        if (!cur.is_null()) { failed = true; return false; }
        return true;
    }

    bool compile_var_ref(SCM sym) {
        int li = find_local(sym);
        if (li >= 0) {
            const auto& lv = locals[static_cast<size_t>(li)];
            if (lv.is_loop) { failed = true; return false; } // shouldn't happen if can_inline_named_let passed
            if (lv.boxed) emit(enc_u24(lv.check_undef ? OP_BOX_REF_CHK : OP_BOX_REF, lv.slot));
            else emit(enc_u24(lv.check_undef ? OP_LOCAL_REF_CHK : OP_LOCAL_REF, lv.slot));
            return true;
        }
        UpvalKind uk;
        bool uchk = false;
        int ui = resolve_upval(sym, uk, uchk);
        if (ui >= 0) {
            switch (uk) {
            case UpvalKind::Val: emit(enc_u24(OP_UPVAL_REF, static_cast<uint32_t>(ui))); break;
            case UpvalKind::Box: emit(enc_u24(uchk ? OP_UPVAL_BOX_REF_CHK : OP_UPVAL_BOX_REF, static_cast<uint32_t>(ui))); break;
            case UpvalKind::CellCar: emit(enc_u24(OP_CELL_CAR_REF, static_cast<uint32_t>(ui))); break;
            case UpvalKind::CellCdr: emit(enc_u24(OP_CELL_CDR_REF, static_cast<uint32_t>(ui))); break;
            }
            return true;
        }
        uint16_t gi = add_global(sym);
        if (is_variable(bc->globals[gi]) && is_macro(as_obj<VariableObj>(bc->globals[gi])->value)) {
            failed = true;
            return false;
        }
        emit(enc_u24(OP_GLOBAL_REF, gi));
        return true;
    }

    bool compile_var_set(SCM sym) {
        // Value is on top of stack; leaves SCM_UNSPECIFIED on stack
        int li = find_local(sym);
        if (li >= 0) {
            const auto& lv = locals[static_cast<size_t>(li)];
            if (lv.boxed) emit(enc_u24(OP_BOX_SET, lv.slot));
            else emit(enc_u24(OP_LOCAL_SET, lv.slot));
            emit(enc0(OP_UNSPEC));
            return true;
        }
        UpvalKind uk;
        bool uchk = false;
        int ui = resolve_upval(sym, uk, uchk);
        if (ui >= 0) {
            switch (uk) {
            case UpvalKind::Box: emit(enc_u24(OP_UPVAL_BOX_SET, static_cast<uint32_t>(ui))); break;
            case UpvalKind::CellCar: emit(enc_u24(OP_CELL_CAR_SET, static_cast<uint32_t>(ui))); break;
            case UpvalKind::CellCdr: emit(enc_u24(OP_CELL_CDR_SET, static_cast<uint32_t>(ui))); break;
            case UpvalKind::Val: failed = true; return false; // shouldn't happen since prescan boxes mutated+captured
            }
            emit(enc0(OP_UNSPEC));
            return true;
        }
        uint16_t gi = add_global(sym);
        emit(enc_u24(OP_GLOBAL_SET, gi));
        emit(enc0(OP_UNSPEC));
        return true;
    }

    // Try compiling `(if (op local imm) conseq alt)` with fused compare+branch!
    bool try_compile_fast_if_test(SCM test_e, uint32_t& out_jf_patch) {
        if (!is_pair(test_e) || !is_symbol(car(test_e))) return false;
        SCM op_sym = car(test_e);
        SCM args = cdr(test_e);
        if (!is_pair(args) || !is_pair(cdr(args)) || !cdr(cdr(args)).is_null()) return false;
        FastBuiltinId fb = resolve_fast_builtin(op_sym);
        if (fb != FB_LT && fb != FB_NUMEQ) return false;
        SCM a0 = car(args), a1 = car(cdr(args));
        if (!is_symbol(a0) || !a1.is_fixnum()) return false;
        int64_t imm = a1.to_fixnum();
        if (imm < -32768 || imm > 32767) return false;
        int li = find_local(a0);
        if (li < 0) return false;
        const auto& lv = locals[static_cast<size_t>(li)];
        if (lv.boxed || lv.check_undef || lv.slot > 255) return false;
        uint16_t gi = add_global(op_sym);
        (void)gi; // we store global idx only if needed; wait: fused op needs fallback global idx!
        // Encode fused op: word 0 = enc_ab(OP, slot, imm16), word 1 = enc_ab(OP_NOP_AUX, 0, jump_off) + word 2 = gi?
        // Actually even simpler: only fuse when `fast_arith_ok` is checked; if slow, we re-push local & imm and call `globals[gi]`!
        // Encode 2 words:
        //   word 0: enc_ab(OP_LOCAL_LT_IMM_JF / OP_LOCAL_EQ_IMM_JF, lv.slot, static_cast<uint16_t>(static_cast<int16_t>(imm)))
        //   word 1: (static_cast<uint32_t>(gi) << 16) | jump_u16 -> we patch lower 16 bits!
        // Keep it even cleaner: only emit OP_LOCAL_REF + OP_FIXNUM + OP_LT2/OP_NUMEQ2 + OP_JUMP_IF_FALSE — wait: let's check!
        // A 2-word fused op cuts 4 dispatches down to 1 dispatch (4x faster branch in fib & tail-loop!!)!
        emit(enc_ab(fb == FB_LT ? OP_LOCAL_LT_IMM_JF : OP_LOCAL_EQ_IMM_JF,
                    static_cast<uint8_t>(lv.slot),
                    static_cast<uint16_t>(static_cast<int16_t>(imm))));
        out_jf_patch = static_cast<uint32_t>(bc->code.size());
        emit(static_cast<uint32_t>(gi) << 16); // low 16 bits = signed jump delta (patched below!)
        return true;
    }

    bool compile_expr(SCM expr, bool tail) {
        if (failed) return false;
        if (expr.is_null()) { failed = true; return false; } // illegal empty combination -> CEK
        if (is_symbol(expr)) {
            if (!compile_var_ref(expr)) return false;
            if (tail) emit(enc0(OP_RETURN));
            return true;
        }
        if (is_variable(expr)) { failed = true; return false; }
        if (!is_pair(expr)) {
            if (expr.is_fixnum()) {
                int64_t n = expr.to_fixnum();
                if (n >= -32768 && n <= 32767) emit(enc_s16(OP_FIXNUM, static_cast<int16_t>(n)));
                else emit(enc_u24(OP_CONST, add_const(expr)));
            } else if (expr == SCM_BOOL_T) emit(enc0(OP_TRUE));
            else if (expr == SCM_BOOL_F) emit(enc0(OP_FALSE));
            else if (expr == SCM_UNSPECIFIED) emit(enc0(OP_UNSPEC));
            else emit(enc_u24(OP_CONST, add_const(expr)));
            if (tail) emit(enc0(OP_RETURN));
            return true;
        }

        SCM op_expr = car(expr);
        SCM arg_exprs = cdr(expr);

        // Check builtin syntax / user macros (including memoized BuiltinMacro objects in car)
        if (is_symbol(op_expr) || is_macro(op_expr)) {
            SCM mac_val = SCM_BOOL_F;
            if (is_macro(op_expr)) {
                mac_val = op_expr;
            } else if (!is_lexically_bound(op_expr)) {
                SCM var = vm().sym2var(op_expr, eval_closure, false);
                if (is_variable(var) && is_macro(as_obj<VariableObj>(var)->value)) {
                    mac_val = as_obj<VariableObj>(var)->value;
                }
            }
            if (is_macro(mac_val)) {
                auto* mac = as_obj<MacroObj>(mac_val);
                if (mac->kind != MacroKind::BuiltinMacro) {
                    // User macro (defmacro/syncase/etc.) -> bail to CEK for exact semantics
                    failed = true;
                    return false;
                }
                BSyn bs = static_cast<BSyn>(mac->extra);
                switch (bs) {
                    case BSyn::Quote: {
                        if (!is_pair(arg_exprs) || !cdr(arg_exprs).is_null()) { failed = true; return false; }
                        SCM q = car(arg_exprs);
                        if (q.is_null()) emit(enc0(OP_NIL));
                        else if (q == SCM_BOOL_T) emit(enc0(OP_TRUE));
                        else if (q == SCM_BOOL_F) emit(enc0(OP_FALSE));
                        else emit(enc_u24(OP_CONST, add_const(q)));
                        if (tail) emit(enc0(OP_RETURN));
                        return true;
                    }
                    case BSyn::Quasiquote: {
                        if (!is_pair(arg_exprs) || !cdr(arg_exprs).is_null()) { failed = true; return false; }
                        auto& v = vm();
                        if (is_lexically_bound(v.sym_unquote) || is_lexically_bound(v.sym_unquote_splicing) ||
                            is_lexically_bound(v.sym_quasiquote) || !v.fast_core_ok) {
                            failed = true; return false;
                        }
                        auto is_const_qq = [&](auto& is_const_qq, SCM x) -> bool {
                            if (is_vector(x)) return false;
                            if (!is_pair(x)) return true;
                            SCM a = car(x);
                            if (a == v.sym_unquote || a == v.sym_unquote_splicing || a == v.sym_quasiquote) return false;
                            return is_const_qq(is_const_qq, a) && is_const_qq(is_const_qq, cdr(x));
                        };
                        auto emit_qq = [&](auto& emit_qq, SCM x) -> bool {
                            if (is_vector(x)) { failed = true; return false; }
                            if (!is_pair(x)) {
                                if (x.is_null()) emit(enc0(OP_NIL));
                                else if (x == SCM_BOOL_T) emit(enc0(OP_TRUE));
                                else if (x == SCM_BOOL_F) emit(enc0(OP_FALSE));
                                else if (x.is_fixnum() && x.to_fixnum() >= -32768 && x.to_fixnum() <= 32767)
                                    emit(enc_s16(OP_FIXNUM, static_cast<int16_t>(x.to_fixnum())));
                                else emit(enc_u24(OP_CONST, add_const(x)));
                                return true;
                            }
                            SCM a = car(x);
                            SCM d = cdr(x);
                            if (a == v.sym_unquote) {
                                if (!is_pair(d) || !cdr(d).is_null()) { failed = true; return false; }
                                return compile_expr(car(d), false);
                            }
                            if (a == v.sym_quasiquote || a == v.sym_unquote_splicing) {
                                failed = true; return false;
                            }
                            if (is_pair(a) && car(a) == v.sym_unquote_splicing) {
                                if (!is_pair(cdr(a)) || !cdr(cdr(a)).is_null()) { failed = true; return false; }
                                if (d.is_null()) {
                                    return compile_expr(car(cdr(a)), false);
                                }
                                uint16_t app_gi = add_global(v.make_symbol("append"));
                                emit(enc_u24(OP_GLOBAL_REF, app_gi));
                                if (!compile_expr(car(cdr(a)), false)) return false;
                                if (!emit_qq(emit_qq, d)) return false;
                                emit(enc_u24(OP_CALL, 2));
                                return true;
                            }
                            if (is_const_qq(is_const_qq, x)) {
                                emit(enc_u24(OP_CONST, add_const(x)));
                                return true;
                            }
                            if (!emit_qq(emit_qq, a)) return false;
                            if (!emit_qq(emit_qq, d)) return false;
                            uint16_t cons_gi = add_global(v.make_symbol("cons"));
                            emit(enc_ab(OP_CONS, 0, cons_gi));
                            return true;
                        };
                        if (!emit_qq(emit_qq, car(arg_exprs))) return false;
                        if (tail) emit(enc0(OP_RETURN));
                        return true;
                    }
                    case BSyn::If: {
                        if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs))) { failed = true; return false; }
                        SCM test_e = car(arg_exprs);
                        SCM conseq = car(cdr(arg_exprs));
                        SCM rest2 = cdr(cdr(arg_exprs));
                        bool has_alt = false;
                        SCM alt = SCM_UNSPECIFIED;
                        if (is_pair(rest2)) {
                            if (!cdr(rest2).is_null()) { failed = true; return false; }
                            alt = car(rest2);
                            has_alt = true;
                        } else if (!rest2.is_null()) { failed = true; return false; }

                        uint32_t jf_idx = 0;
                        bool fused = try_compile_fast_if_test(test_e, jf_idx);
                        if (!fused) {
                            if (!compile_expr(test_e, false)) return false;
                            jf_idx = emit_jump(OP_JUMP_IF_FALSE);
                        }
                        if (!compile_expr(conseq, tail)) return false;
                        uint32_t jend = 0;
                        if (!tail) jend = emit_jump(OP_JUMP);
                        if (fused) {
                            int32_t delta = static_cast<int32_t>(bc->code.size()) - static_cast<int32_t>(jf_idx) - 1;
                            if (delta < -32768 || delta > 32767) { failed = true; return false; }
                            bc->code[jf_idx] = (bc->code[jf_idx] & 0xFFFF0000u) | static_cast<uint16_t>(static_cast<int16_t>(delta));
                        } else {
                            if (!patch_jump(jf_idx)) return false;
                        }
                        if (has_alt) {
                            if (!compile_expr(alt, tail)) return false;
                        } else {
                            emit(enc0(OP_UNSPEC));
                            if (tail) emit(enc0(OP_RETURN));
                        }
                        if (!tail && !patch_jump(jend)) return false;
                        return true;
                    }
                    case BSyn::Begin: {
                        if (arg_exprs.is_null()) {
                            emit(enc0(OP_UNSPEC));
                            if (tail) emit(enc0(OP_RETURN));
                            return true;
                        }
                        return compile_body(arg_exprs, tail);
                    }
                    case BSyn::SetX: {
                        if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs)) || !cdr(cdr(arg_exprs)).is_null()) {
                            failed = true; return false;
                        }
                        SCM target = car(arg_exprs);
                        if (!is_symbol(target)) { failed = true; return false; } // generalized set! / @ -> CEK
                        if (!compile_expr(car(cdr(arg_exprs)), false)) return false;
                        if (!compile_var_set(target)) return false;
                        if (tail) emit(enc0(OP_RETURN));
                        return true;
                    }
                    case BSyn::And: {
                        if (arg_exprs.is_null()) {
                            emit(enc0(OP_TRUE));
                            if (tail) emit(enc0(OP_RETURN));
                            return true;
                        }
                        std::vector<uint32_t> exits;
                        SCM cur = arg_exprs;
                        while (is_pair(cur)) {
                            bool is_last = cdr(cur).is_null();
                            if (!compile_expr(car(cur), tail && is_last)) return false;
                            if (!is_last) exits.push_back(emit_jump(OP_JUMP_IF_FALSE_KEEP));
                            cur = cdr(cur);
                        }
                        if (!cur.is_null()) { failed = true; return false; }
                        if (tail) {
                            if (!exits.empty()) {
                                for (uint32_t j : exits) if (!patch_jump(j)) return false;
                                emit(enc0(OP_RETURN));
                            }
                        } else {
                            for (uint32_t j : exits) if (!patch_jump(j)) return false;
                        }
                        return true;
                    }
                    case BSyn::Or: {
                        if (arg_exprs.is_null()) {
                            emit(enc0(OP_FALSE));
                            if (tail) emit(enc0(OP_RETURN));
                            return true;
                        }
                        std::vector<uint32_t> exits;
                        SCM cur = arg_exprs;
                        while (is_pair(cur)) {
                            bool is_last = cdr(cur).is_null();
                            if (!compile_expr(car(cur), tail && is_last)) return false;
                            if (!is_last) exits.push_back(emit_jump(OP_JUMP_IF_TRUE_KEEP));
                            cur = cdr(cur);
                        }
                        if (!cur.is_null()) { failed = true; return false; }
                        if (tail) {
                            if (!exits.empty()) {
                                for (uint32_t j : exits) if (!patch_jump(j)) return false;
                                emit(enc0(OP_RETURN));
                            }
                        } else {
                            for (uint32_t j : exits) if (!patch_jump(j)) return false;
                        }
                        return true;
                    }
                    case BSyn::Cond: {
                        if (arg_exprs.is_null()) { failed = true; return false; }
                        bool arrow_unbound = !is_lexically_bound(vm().sym_arrow) &&
                                             !is_variable(vm().sym2var(vm().sym_arrow, eval_closure, false));
                        bool else_unbound = !is_lexically_bound(vm().sym_else) &&
                                            !is_variable(vm().sym2var(vm().sym_else, eval_closure, false));
                        std::vector<uint32_t> end_jumps;
                        SCM cur = arg_exprs;
                        bool saw_else = false;
                        while (is_pair(cur)) {
                            SCM cl = car(cur);
                            if (!is_pair(cl)) { failed = true; return false; }
                            SCM test_e = car(cl);
                            SCM cbody = cdr(cl);
                            if (else_unbound && test_e == vm().sym_else) {
                                if (!cdr(cur).is_null() || cbody.is_null()) { failed = true; return false; }
                                if (!compile_body(cbody, tail)) return false;
                                saw_else = true;
                                break;
                            }
                            if (arrow_unbound && is_pair(cbody)) {
                                if (car(cbody) == vm().sym_arrow) {
                                    if (!is_pair(cdr(cbody)) || !cdr(cdr(cbody)).is_null()) {
                                        failed = true; return false;
                                    }
                                    if (!compile_expr(test_e, false)) return false;
                                    uint16_t tmp = alloc_slot();
                                    emit(enc_u24(OP_LOCAL_SET, tmp));
                                    emit(enc_u24(OP_LOCAL_REF, tmp));
                                    uint32_t jf = emit_jump(OP_JUMP_IF_FALSE);
                                    if (!compile_expr(car(cdr(cbody)), false)) return false;
                                    emit(enc_u24(OP_LOCAL_REF, tmp));
                                    emit(enc_u24(tail ? OP_TAIL_CALL : OP_CALL, 1));
                                    if (!tail) end_jumps.push_back(emit_jump(OP_JUMP));
                                    if (!patch_jump(jf)) return false;
                                    cur = cdr(cur);
                                    continue;
                                }
                                if (is_pair(cdr(cbody)) && car(cdr(cbody)) == vm().sym_arrow) {
                                    failed = true; return false; // let CEK handle SRFI-61
                                }
                            }
                            if (cbody.is_null()) {
                                if (!compile_expr(test_e, false)) return false;
                                end_jumps.push_back(emit_jump(OP_JUMP_IF_TRUE_KEEP));
                            } else {
                                if (!compile_expr(test_e, false)) return false;
                                uint32_t jf = emit_jump(OP_JUMP_IF_FALSE);
                                if (!compile_body(cbody, tail)) return false;
                                if (!tail) end_jumps.push_back(emit_jump(OP_JUMP));
                                if (!patch_jump(jf)) return false;
                            }
                            cur = cdr(cur);
                        }
                        if (!saw_else) {
                            if (!cur.is_null()) { failed = true; return false; }
                            emit(enc0(OP_UNSPEC));
                            if (tail) emit(enc0(OP_RETURN));
                        }
                        for (uint32_t j : end_jumps) {
                            if (!patch_jump(j)) return false;
                        }
                        if (tail && !end_jumps.empty()) emit(enc0(OP_RETURN));
                        return true;
                    }
                    case BSyn::Lambda: {
                        if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs))) { failed = true; return false; }
                        SCM formals = car(arg_exprs);
                        SCM lbody = cdr(arg_exprs);
                        auto* child_bc = compile_lambda_proto(formals, lbody, this);
                        if (!child_bc) { failed = true; return false; }
                        uint32_t cidx = add_const(SCM::from_obj(child_bc));
                        emit(enc_u24(OP_MAKE_CLOSURE, cidx));
                        if (tail) emit(enc0(OP_RETURN));
                        return true;
                    }
                    case BSyn::Let: {
                        if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs))) { failed = true; return false; }
                        SCM first = car(arg_exprs);
                        SCM body = cdr(arg_exprs);
                        if (is_symbol(first)) {
                            // Named let!
                            SCM loop_sym = first;
                            if (!is_pair(body) || !is_pair(cdr(body))) { failed = true; return false; }
                            SCM bindings = car(body);
                            body = cdr(body);
                            std::vector<SCM> names;
                            std::vector<SCM> inits;
                            SCM b = bindings;
                            while (is_pair(b)) {
                                SCM e = car(b);
                                if (!is_pair(e) || !is_symbol(car(e)) || !is_pair(cdr(e)) || !cdr(cdr(e)).is_null()) {
                                    failed = true; return false;
                                }
                                for (SCM s : names) if (s == car(e)) { failed = true; return false; }
                                names.push_back(car(e));
                                inits.push_back(car(cdr(e)));
                                b = cdr(b);
                            }
                            if (!b.is_null()) { failed = true; return false; }

                            // Check if we can compile as ZERO-OVERHEAD INLINE LOOP!
                            if (can_inline_named_let(loop_sym, names.size(), body, true)) {
                                bc->has_loop = true;
                                // Evaluate all inits in outer scope
                                for (SCM init_e : inits) {
                                    if (!compile_expr(init_e, false)) return false;
                                }
                                size_t saved_locals = locals.size();
                                uint16_t saved_slot = next_slot;
                                std::vector<uint16_t> pslots;
                                pslots.reserve(names.size());
                                for (SCM nm : names) {
                                    uint16_t sl = alloc_slot();
                                    pslots.push_back(sl);
                                    bool boxed = vec_has(mutated_syms, nm);
                                    locals.push_back({nm, sl, boxed, false, false, -1});
                                }
                                // Pop init values into slots in reverse order
                                for (size_t i = names.size(); i > 0; --i) {
                                    emit(enc_u24(OP_LOCAL_SET, pslots[i - 1]));
                                    if (locals[saved_locals + i - 1].boxed) {
                                        emit(enc_u24(OP_BOX_INIT, pslots[i - 1]));
                                    }
                                }
                                uint32_t loop_start = static_cast<uint32_t>(bc->code.size());
                                int lid = static_cast<int>(loops.size());
                                loops.push_back({loop_sym, loop_start, pslots});
                                locals.push_back({loop_sym, 0, false, false, true, lid});

                                bool ok = compile_body(body, tail);

                                loops.pop_back();
                                locals.resize(saved_locals);
                                next_slot = saved_slot;
                                return ok;
                            }

                            // General named let: desugar to ((letrec ((loop (lambda names . body))) loop) . inits)
                            SCM formals = vm().list_n(names.data(), names.size());
                            SCM lam = vm().cons(vm().sym_lambda, vm().cons(formals, body));
                            SCM lr = vm().list3(vm().sym_letrec, vm().list1(vm().list2(loop_sym, lam)), loop_sym);
                            SCM call_form = vm().cons(lr, vm().list_n(inits.data(), inits.size()));
                            return compile_expr(call_form, tail);
                        }
                        // Regular let
                        std::vector<SCM> names, inits;
                        SCM b = first;
                        while (is_pair(b)) {
                            SCM e = car(b);
                            if (!is_pair(e) || !is_symbol(car(e)) || !is_pair(cdr(e)) || !cdr(cdr(e)).is_null()) {
                                failed = true; return false;
                            }
                            for (SCM s : names) if (s == car(e)) { failed = true; return false; }
                            names.push_back(car(e));
                            inits.push_back(car(cdr(e)));
                            b = cdr(b);
                        }
                        if (!b.is_null()) { failed = true; return false; }
                        for (SCM init_e : inits) {
                            if (!compile_expr(init_e, false)) return false;
                        }
                        size_t saved_locals = locals.size();
                        uint16_t saved_slot = next_slot;
                        std::vector<uint16_t> slots;
                        for (SCM nm : names) {
                            uint16_t sl = alloc_slot();
                            slots.push_back(sl);
                            bool boxed = vec_has(mutated_syms, nm);
                            locals.push_back({nm, sl, boxed, false, false, -1});
                        }
                        for (size_t i = names.size(); i > 0; --i) {
                            emit(enc_u24(OP_LOCAL_SET, slots[i - 1]));
                            if (locals[saved_locals + i - 1].boxed) {
                                emit(enc_u24(OP_BOX_INIT, slots[i - 1]));
                            }
                        }
                        bool ok = compile_body(body, tail);
                        if (ok && !tail && !slots.empty() && saved_slot <= 255) {
                            emit(enc_ab(OP_CLEAR_LOCALS, static_cast<uint8_t>(saved_slot), static_cast<uint16_t>(slots.size())));
                        }
                        locals.resize(saved_locals);
                        next_slot = saved_slot;
                        return ok;
                    }
                    case BSyn::LetStar: {
                        if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs))) { failed = true; return false; }
                        SCM bindings = car(arg_exprs);
                        SCM body = cdr(arg_exprs);
                        size_t saved_locals = locals.size();
                        uint16_t saved_slot = next_slot;
                        SCM b = bindings;
                        while (is_pair(b)) {
                            SCM e = car(b);
                            if (!is_pair(e) || !is_symbol(car(e)) || !is_pair(cdr(e)) || !cdr(cdr(e)).is_null()) {
                                failed = true; return false;
                            }
                            if (!compile_expr(car(cdr(e)), false)) return false;
                            SCM nm = car(e);
                            uint16_t sl = alloc_slot();
                            bool boxed = vec_has(mutated_syms, nm);
                            locals.push_back({nm, sl, boxed, false, false, -1});
                            emit(enc_u24(OP_LOCAL_SET, sl));
                            if (boxed) emit(enc_u24(OP_BOX_INIT, sl));
                            b = cdr(b);
                        }
                        if (!b.is_null()) { failed = true; return false; }
                        bool ok = compile_body(body, tail);
                        if (ok && !tail && next_slot > saved_slot && saved_slot <= 255) {
                            emit(enc_ab(OP_CLEAR_LOCALS, static_cast<uint8_t>(saved_slot), static_cast<uint16_t>(next_slot - saved_slot)));
                        }
                        locals.resize(saved_locals);
                        next_slot = saved_slot;
                        return ok;
                    }
                    case BSyn::Letrec: {
                        if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs))) { failed = true; return false; }
                        SCM bindings = car(arg_exprs);
                        SCM body = cdr(arg_exprs);
                        std::vector<SCM> names, inits;
                        SCM b = bindings;
                        while (is_pair(b)) {
                            SCM e = car(b);
                            if (!is_pair(e) || !is_symbol(car(e)) || !is_pair(cdr(e)) || !cdr(cdr(e)).is_null()) {
                                failed = true; return false;
                            }
                            for (SCM s : names) if (s == car(e)) { failed = true; return false; }
                            names.push_back(car(e));
                            inits.push_back(car(cdr(e)));
                            b = cdr(b);
                        }
                        if (!b.is_null()) { failed = true; return false; }
                        // Check if all inits are syntactic (lambda ...) forms -> immutable bindings can record known_bc!
                        bool all_lambdas = !names.empty();
                        for (SCM init_e : inits) {
                            if (!is_pair(init_e) || !is_symbol(car(init_e)) ||
                                resolve_bsyn(car(init_e)) != BSyn::Lambda ||
                                !is_pair(cdr(init_e)) || !is_pair(cdr(cdr(init_e)))) {
                                all_lambdas = false;
                                break;
                            }
                        }
                        size_t saved_locals = locals.size();
                        uint16_t saved_slot = next_slot;
                        std::vector<uint16_t> slots;
                        std::vector<BytecodeObj*> prealloc_bcs(names.size(), nullptr);
                        for (size_t i = 0; i < names.size(); ++i) {
                            SCM nm = names[i];
                            uint16_t sl = alloc_slot();
                            slots.push_back(sl);
                            // Always box letrec bindings so mutual recursion & internal defines always share live cells!
                            bool boxed = true;
                            SCM kbc = SCM_BOOL_F;
                            if (all_lambdas && !vec_has(mutated_syms, nm)) {
                                prealloc_bcs[i] = vm().heap().alloc<BytecodeObj>();
                                kbc = SCM::from_obj(prealloc_bcs[i]);
                            }
                            locals.push_back({nm, sl, boxed, true, false, -1, kbc});
                            emit(enc_u24(OP_BOX_NEW_UNDEF, sl));
                        }
                        // Evaluate ALL inits before assigning (R5RS / Petrofsky safe!)
                        for (size_t i = 0; i < inits.size(); ++i) {
                            if (prealloc_bcs[i]) {
                                SCM arg_e = cdr(inits[i]);
                                auto* child_bc = compile_lambda_proto(car(arg_e), cdr(arg_e), this,
                                                                      SCM_BOOL_F, nullptr, prealloc_bcs[i]);
                                if (!child_bc) { failed = true; return false; }
                                uint32_t cidx = add_const(SCM::from_obj(child_bc));
                                emit(enc_u24(OP_MAKE_CLOSURE, cidx));
                            } else {
                                if (!compile_expr(inits[i], false)) return false;
                            }
                        }
                        for (size_t i = names.size(); i > 0; --i) {
                            if (locals[saved_locals + i - 1].boxed) emit(enc_u24(OP_BOX_SET, slots[i - 1]));
                            else emit(enc_u24(OP_LOCAL_SET, slots[i - 1]));
                        }
                        // After init, check_undef is no longer needed in body
                        for (size_t i = 0; i < names.size(); ++i) {
                            locals[saved_locals + i].check_undef = false;
                        }
                        bool ok = compile_body(body, tail);
                        locals.resize(saved_locals);
                        next_slot = saved_slot;
                        return ok;
                    }
                    case BSyn::Case: {
                        if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs))) { failed = true; return false; }
                        auto& v = vm();
                        bool else_unbound = !is_lexically_bound(v.sym_else) &&
                                            !is_variable(v.sym2var(v.sym_else, eval_closure, false));
                        if (!compile_expr(car(arg_exprs), false)) return false;
                        uint16_t saved_slot = next_slot;
                        uint16_t kslot = alloc_slot();
                        emit(enc_u24(OP_LOCAL_SET, kslot));
                        std::vector<uint32_t> end_jumps;
                        SCM cur = cdr(arg_exprs);
                        bool saw_else = false;
                        while (is_pair(cur)) {
                            SCM cl = car(cur);
                            if (!is_pair(cl) || !is_pair(cdr(cl))) { failed = true; return false; }
                            SCM datums = car(cl);
                            SCM cbody = cdr(cl);
                            if (else_unbound && datums == v.sym_else) {
                                if (!cdr(cur).is_null()) { failed = true; return false; }
                                if (!compile_body(cbody, tail)) return false;
                                saw_else = true;
                                break;
                            }
                            // Verify datums is a proper list
                            int64_t dlen = scm_ilength(datums);
                            if (dlen < 0) { failed = true; return false; }
                            if (dlen == 0) {
                                cur = cdr(cur);
                                continue;
                            }
                            if (dlen == 1 && (is_symbol(car(datums)) || is_keyword(car(datums)) || !car(datums).is_heap()) && v.fast_core_ok) {
                                emit(enc_u24(OP_LOCAL_REF, kslot));
                                emit(enc_u24(OP_CONST, add_const(car(datums))));
                                emit(enc_ab(OP_EQ, 0, add_global(v.make_symbol("eq?"))));
                            } else {
                                uint16_t mv_gi = add_global(v.make_symbol("memv"));
                                emit(enc_u24(OP_GLOBAL_REF, mv_gi));
                                emit(enc_u24(OP_LOCAL_REF, kslot));
                                emit(enc_u24(OP_CONST, add_const(datums)));
                                emit(enc_u24(OP_CALL, 2));
                            }
                            uint32_t jf = emit_jump(OP_JUMP_IF_FALSE);
                            if (!compile_body(cbody, tail)) return false;
                            if (!tail) end_jumps.push_back(emit_jump(OP_JUMP));
                            if (!patch_jump(jf)) return false;
                            cur = cdr(cur);
                        }
                        if (!saw_else) {
                            if (!cur.is_null()) { failed = true; return false; }
                            emit(enc0(OP_UNSPEC));
                            if (tail) emit(enc0(OP_RETURN));
                        }
                        for (uint32_t j : end_jumps) {
                            if (!patch_jump(j)) return false;
                        }
                        if (!tail && kslot <= 255) {
                            emit(enc_ab(OP_CLEAR_LOCALS, static_cast<uint8_t>(kslot), 1));
                        }
                        next_slot = saved_slot;
                        return true;
                    }
                    case BSyn::Do: {
                        if (!is_pair(arg_exprs) || !is_pair(cdr(arg_exprs))) { failed = true; return false; }
                        SCM var_specs = car(arg_exprs);
                        SCM test_clause = car(cdr(arg_exprs));
                        SCM cmds = cdr(cdr(arg_exprs));
                        if (!is_pair(test_clause)) { failed = true; return false; }
                        std::vector<SCM> names, inits, steps;
                        SCM vs = var_specs;
                        while (is_pair(vs)) {
                            SCM sp = car(vs);
                            if (!is_pair(sp) || !is_symbol(car(sp)) || !is_pair(cdr(sp))) { failed = true; return false; }
                            SCM nm = car(sp);
                            for (SCM s : names) if (s == nm) { failed = true; return false; }
                            SCM init_e = car(cdr(sp));
                            SCM step_rest = cdr(cdr(sp));
                            SCM step_e = nm;
                            if (is_pair(step_rest)) {
                                if (!cdr(step_rest).is_null()) { failed = true; return false; }
                                step_e = car(step_rest);
                                if (step_e == nm) {
                                    // Match Guile 1.8.8 unmemoization: reduce redundant (var init var) -> (var init)
                                    set_cdr(cdr(sp), SCM_EOL);
                                }
                            } else if (!step_rest.is_null()) { failed = true; return false; }
                            names.push_back(nm);
                            inits.push_back(init_e);
                            steps.push_back(step_e);
                            vs = cdr(vs);
                        }
                        if (!vs.is_null()) { failed = true; return false; }
                        // Evaluate all inits in outer scope
                        for (SCM ie : inits) {
                            if (!compile_expr(ie, false)) return false;
                        }
                        size_t saved_locals = locals.size();
                        uint16_t saved_slot = next_slot;
                        std::vector<uint16_t> pslots;
                        pslots.reserve(names.size());
                        for (SCM nm : names) {
                            uint16_t sl = alloc_slot();
                            pslots.push_back(sl);
                            bool boxed = vec_has(mutated_syms, nm) || vec_has(captured_syms, nm);
                            locals.push_back({nm, sl, boxed, false, false, -1});
                        }
                        for (size_t i = names.size(); i > 0; --i) {
                            emit(enc_u24(OP_LOCAL_SET, pslots[i - 1]));
                            if (locals[saved_locals + i - 1].boxed) emit(enc_u24(OP_BOX_INIT, pslots[i - 1]));
                        }
                        uint32_t loop_start = static_cast<uint32_t>(bc->code.size());
                        if (!compile_expr(car(test_clause), false)) return false;
                        uint32_t jf = emit_jump(OP_JUMP_IF_FALSE);
                        // Test succeeded -> evaluate result exprs (or *unspecified*)
                        SCM res_exprs = cdr(test_clause);
                        if (res_exprs.is_null()) {
                            emit(enc0(OP_UNSPEC));
                            if (tail) emit(enc0(OP_RETURN));
                        } else {
                            if (!compile_body(res_exprs, tail)) return false;
                        }
                        uint32_t jend = 0;
                        if (!tail) jend = emit_jump(OP_JUMP);
                        if (!patch_jump(jf)) return false;
                        // Test failed -> execute commands, then evaluate steps and jump back
                        SCM c = cmds;
                        while (is_pair(c)) {
                            if (!compile_expr(car(c), false)) return false;
                            emit(enc0(OP_POP));
                            c = cdr(c);
                        }
                        if (!c.is_null()) { failed = true; return false; }
                        for (SCM se : steps) {
                            if (!compile_expr(se, false)) return false;
                        }
                        for (size_t i = names.size(); i > 0; --i) {
                            emit(enc_u24(OP_LOCAL_SET, pslots[i - 1]));
                            if (locals[saved_locals + i - 1].boxed) emit(enc_u24(OP_BOX_INIT, pslots[i - 1]));
                        }
                        int32_t delta = static_cast<int32_t>(loop_start) - static_cast<int32_t>(bc->code.size()) - 1;
                        if (delta < -32768 || delta > 32767) { failed = true; return false; }
                        emit(enc_s16(OP_JUMP, static_cast<int16_t>(delta)));
                        if (!tail && !patch_jump(jend)) return false;
                        if (!tail && !pslots.empty() && saved_slot <= 255) {
                            emit(enc_ab(OP_CLEAR_LOCALS, static_cast<uint8_t>(saved_slot), static_cast<uint16_t>(pslots.size())));
                        }
                        locals.resize(saved_locals);
                        next_slot = saved_slot;
                        return true;
                    }
                    default:
                        // Delay, Define, @, @@, TheEnvironment, DefinedP -> CEK
                        failed = true;
                        return false;
                    }
            }
        }

        if (is_symbol(op_expr)) {
            // Check if calling an inlined named-let loop in tail position!
            int li = find_local(op_expr);
            if (li >= 0 && locals[static_cast<size_t>(li)].is_loop) {
                if (!tail) { failed = true; return false; }
                size_t loop_id = static_cast<size_t>(locals[static_cast<size_t>(li)].loop_id);
                // Collect args
                std::vector<SCM> cargs;
                SCM a = arg_exprs;
                while (is_pair(a)) { cargs.push_back(car(a)); a = cdr(a); }
                if (!a.is_null() || cargs.size() != loops[loop_id].param_slots.size()) { failed = true; return false; }
                for (SCM ae : cargs) {
                    if (!compile_expr(ae, false)) return false;
                }
                // Re-index loops[loop_id] AFTER compiling args because nested
                // named-lets inside argument expressions can reallocate `loops`.
                const auto& lp = loops[loop_id];
                for (size_t i = cargs.size(); i > 0; --i) {
                    uint16_t sl = lp.param_slots[i - 1];
                    // If boxed (captured by inner lambda inside loop), allocate fresh box for next iteration!
                    // (Look up local entry by slot)
                    bool is_boxed = false;
                    for (int k = static_cast<int>(locals.size()) - 1; k >= 0; --k) {
                        if (!locals[static_cast<size_t>(k)].is_loop && locals[static_cast<size_t>(k)].slot == sl) {
                            is_boxed = locals[static_cast<size_t>(k)].boxed;
                            break;
                        }
                    }
                    emit(enc_u24(OP_LOCAL_SET, sl));
                    if (is_boxed) emit(enc_u24(OP_BOX_INIT, sl));
                }
                int32_t delta = static_cast<int32_t>(lp.start_pc) - static_cast<int32_t>(bc->code.size()) - 1;
                if (delta < -32768 || delta > 32767) { failed = true; return false; }
                emit(enc_s16(OP_JUMP, static_cast<int16_t>(delta)));
                return true;
            }

            // Check fast inlined builtin calls!
            FastBuiltinId fb = resolve_fast_builtin(op_expr);
            if (fb != FB_NONE) {
                std::vector<SCM> cargs;
                SCM a = arg_exprs;
                while (is_pair(a)) { cargs.push_back(car(a)); a = cdr(a); }
                if (!a.is_null()) { failed = true; return false; }
                size_t na = cargs.size();

                // Fused `(+ local imm)` / `(- local imm)`!
                if ((fb == FB_ADD || fb == FB_SUB) && na == 2 && is_symbol(cargs[0]) && cargs[1].is_fixnum()) {
                    int64_t imm = fb == FB_ADD ? cargs[1].to_fixnum() : -cargs[1].to_fixnum();
                    int lidx = find_local(cargs[0]);
                    if (lidx >= 0 && imm >= -32768 && imm <= 32767) {
                        const auto& lv = locals[static_cast<size_t>(lidx)];
                        if (!lv.boxed && !lv.check_undef && lv.slot <= 255) {
                            uint16_t gi = add_global(op_expr);
                            // 2-word instruction: word 0 = enc_ab(OP_LOCAL_ADD_IMM, slot, imm16), word 1 = (fb == FB_SUB ? 1 : 0) | (gi << 16)
                            emit(enc_ab(OP_LOCAL_ADD_IMM, static_cast<uint8_t>(lv.slot), static_cast<uint16_t>(static_cast<int16_t>(imm))));
                            emit((static_cast<uint32_t>(gi) << 16) | (fb == FB_SUB ? 1u : 0u));
                            if (tail) emit(enc0(OP_RETURN));
                            return true;
                        }
                    }
                }

                // Special native call/cc!
                if (fb == FB_CALLCC && na == 1) {
                    uint16_t gi = add_global(op_expr);
                    if (!compile_expr(cargs[0], false)) return false;
                    emit(enc_ab(OP_CALLCC, 0, gi));
                    if (tail) emit(enc0(OP_RETURN));
                    return true;
                }

                auto emit_fast = [&](Op op, uint8_t a_field = 0) -> bool {
                    uint16_t gi = add_global(op_expr);
                    for (SCM ae : cargs) if (!compile_expr(ae, false)) return false;
                    emit(enc_ab(op, a_field, gi));
                    if (tail) emit(enc0(OP_RETURN));
                    return true;
                };

                if (fb == FB_ADD && na == 2) return emit_fast(OP_ADD2);
                if (fb == FB_ADD && (na == 1 || (na >= 3 && na <= 16))) return emit_fast(OP_ADDN, static_cast<uint8_t>(na));
                if (fb == FB_SUB && na == 2) return emit_fast(OP_SUB2);
                if (fb == FB_MUL && na == 2) return emit_fast(OP_MUL2);
                if (fb == FB_DIV && na == 2) return emit_fast(OP_DIV2);
                if (fb == FB_ABS && na == 1) return emit_fast(OP_ABS);
                if (fb == FB_ADD1 && na == 1) return emit_fast(OP_ADD1);
                if (fb == FB_SUB1 && na == 1) return emit_fast(OP_SUB1);
                if (fb == FB_NUMEQ && na == 2) return emit_fast(OP_NUMEQ2);
                if (fb == FB_LT && na == 2) return emit_fast(OP_LT2);
                if (fb == FB_LE && na == 2) return emit_fast(OP_LE2);
                if (fb == FB_GT && na == 2) return emit_fast(OP_GT2);
                if (fb == FB_GE && na == 2) return emit_fast(OP_GE2);
                if (fb == FB_EVENP && na == 1) return emit_fast(OP_EVENP);
                if (fb == FB_ODDP && na == 1) return emit_fast(OP_ODDP);
                if (fb == FB_EQ && na == 2) return emit_fast(OP_EQ);
                if (fb == FB_NULLP && na == 1) return emit_fast(OP_NULLP);
                if (fb == FB_PAIRP && na == 1) return emit_fast(OP_PAIRP);
                if (fb == FB_NOT && na == 1) return emit_fast(OP_NOT);
                if (fb == FB_CAR && na == 1) return emit_fast(OP_CAR);
                if (fb == FB_CDR && na == 1) return emit_fast(OP_CDR);
                if (fb == FB_CONS && na == 2) return emit_fast(OP_CONS);
                if (fb == FB_REVERSE && na == 1) return emit_fast(OP_REVERSE);
                if (fb == FB_VEC_REF && na == 2) return emit_fast(OP_VEC_REF);
                if (fb == FB_VEC_SET && na == 3) return emit_fast(OP_VEC_SET);
                if (fb == FB_MAKE_VEC && (na == 1 || na == 2)) return emit_fast(OP_MAKE_VEC, static_cast<uint8_t>(na));
                if (fb == FB_STRUCT_REF && na == 2) return emit_fast(OP_STRUCT_REF);
                if (fb == FB_STRUCT_SET && na == 3) return emit_fast(OP_STRUCT_SET);
                if (fb == FB_HASHV_REF && (na == 2 || na == 3)) return emit_fast(OP_HASHV_REF, static_cast<uint8_t>(na));
                if (fb == FB_HASHV_SET && na == 3) return emit_fast(OP_HASHV_SET);
                if (fb == FB_HASHQ_REF && (na == 2 || na == 3)) return emit_fast(OP_HASHQ_REF, static_cast<uint8_t>(na));
                if (fb == FB_HASHQ_SET && na == 3) return emit_fast(OP_HASHQ_SET);
            }
        }

        // General procedure call
        std::vector<SCM> cargs;
        SCM a = arg_exprs;
        while (is_pair(a)) { cargs.push_back(car(a)); a = cdr(a); }
        if (!a.is_null() || cargs.size() > 250) { failed = true; return false; }

        if (!compile_expr(op_expr, false)) return false;
        for (SCM ae : cargs) {
            if (!compile_expr(ae, false)) return false;
        }
        emit(enc_u24(tail ? OP_TAIL_CALL : OP_CALL, static_cast<uint32_t>(cargs.size())));
        return true;
    }

    static BytecodeObj* compile_lambda_proto(SCM formals, SCM body, FnCompiler* parent_c,
                                             SCM eval_clo = SCM_BOOL_F,
                                             const std::vector<OuterCekBinding>* cek_outer = nullptr,
                                             BytecodeObj* prealloc_bc = nullptr) {
        auto* bc = prealloc_bc ? prealloc_bc : vm().heap().alloc<BytecodeObj>();
        bc->eval_closure = parent_c ? parent_c->eval_closure : eval_clo;
        bc->cached_env = bc->eval_closure.is_true() ? vm().list1(bc->eval_closure) : SCM_EOL;
        bc->source_formals = formals;
        bc->source_body = body;

        FnCompiler fc;
        fc.parent = parent_c;
        fc.bc = bc;
        fc.eval_closure = bc->eval_closure;
        fc.cek_outer = parent_c ? parent_c->cek_outer : cek_outer;

        // Parse formals
        std::vector<SCM> params;
        SCM f = formals;
        while (is_pair(f)) {
            if (!is_symbol(car(f))) return nullptr;
            params.push_back(car(f));
            f = cdr(f);
        }
        bool has_rest = false;
        if (!f.is_null()) {
            if (!is_symbol(f)) return nullptr;
            has_rest = true;
            params.push_back(f);
        }
        bc->arity_req = static_cast<uint16_t>(has_rest ? params.size() - 1 : params.size());
        bc->has_rest = has_rest;

        // Prescan body for mutations & captures
        {
            SCM b = body;
            while (is_pair(b)) { fc.prescan(car(b), false); b = cdr(b); }
        }

        // Bind parameter slots
        for (SCM p : params) {
            uint16_t sl = fc.alloc_slot();
            bool boxed = vec_has(fc.mutated_syms, p);
            fc.locals.push_back({p, sl, boxed, false, false, -1});
            if (boxed) fc.emit(enc_u24(OP_BOX_INIT, sl));
        }

        if (!fc.compile_body(body, true) || fc.failed) {
            return nullptr;
        }

        bc->max_locals = fc.max_slot;
        bc->num_upvals = static_cast<uint16_t>(fc.upvals.size());
        return bc;
    }
};

// ============================================================================
// Bytecode VM Execution Engine (Flat Stacks + Computed Goto)
// ============================================================================

struct VmFrame {
    SCM closure;
    SCM bytecode;
    const uint32_t* pc;
    uint32_t base_off;
    uint32_t ret_slot;
};

struct BytecodeEngine {
    std::vector<SCM> stack;
    std::vector<VmFrame> frames;
    size_t active_sp = 0;
    size_t active_fp = 0;
    SCM foreach1_helper = SCM_BOOL_F;
    SCM map1_helper = SCM_BOOL_F;
    SCM dynwind_helper = SCM_BOOL_F;

    BytecodeEngine() {
        stack.resize(131072, SCM_UNSPECIFIED);
        frames.resize(16384);
    }

    SCM get_dynwind_helper() {
        if (is_closure(dynwind_helper)) return dynwind_helper;
        auto& v = vm();
        auto* dbc = v.heap().alloc<BytecodeObj>();
        dbc->arity_req = 3;
        dbc->has_rest = false;
        dbc->max_locals = 5; // 0=before, 1=thunk, 2=after, 3=dw, 4=res
        dbc->num_upvals = 0;
        // (begin (before) (push-dw) (let ((res (thunk))) (pop-dw) (after) res))
        dbc->code = {
            enc_u24(OP_LOCAL_REF, 0),
            enc_u24(OP_CALL, 0),
            enc0(OP_POP),
            enc0(OP_DYNWIND_PUSH),
            enc_u24(OP_LOCAL_REF, 1),
            enc_u24(OP_CALL, 0),
            enc_u24(OP_LOCAL_SET, 4),
            enc0(OP_DYNWIND_POP),
            enc_u24(OP_LOCAL_REF, 2),
            enc_u24(OP_CALL, 0),
            enc0(OP_POP),
            enc_u24(OP_LOCAL_REF, 4),
            enc0(OP_RETURN),
        };
        auto* dcl = v.heap().alloc<ClosureObj>(SCM_EOL, SCM_EOL, SCM_EOL);
        dcl->bytecode = SCM::from_obj(dbc);
        dynwind_helper = SCM::from_obj(dcl);
        return dynwind_helper;
    }

    SCM get_foreach1_helper() {
        if (is_closure(foreach1_helper)) return foreach1_helper;
        auto& v = vm();
        if (!v.module_system_booted || !v.fast_core_ok) return SCM_BOOL_F;
        SCM root_env = SCM_EOL;
        if (is_variable(v.the_root_module_var)) {
            SCM rm = as_obj<VariableObj>(v.the_root_module_var)->value;
            if (is_struct(rm) && as_obj<StructObj>(rm)->slots.size() > 3) {
                SCM ec{as_obj<StructObj>(rm)->slots[3]};
                if (ec.is_true()) root_env = v.list1(ec);
            }
        }
        SCM port = make_input_string_port(
            "(lambda (proc lst) "
            "  (let loop ((l lst)) "
            "    (if (pair? l) "
            "        (begin (proc (car l)) (loop (cdr l))) "
            "        (if (null? l) (if #f #f) (for-each proc l)))))");
        SCM form = v.read_from_port(port);
        SCM fn = v.eval(form, root_env);
        if (is_closure(fn) && bytecode_compile_closure(as_obj<ClosureObj>(fn))) {
            foreach1_helper = fn;
            return fn;
        }
        return SCM_BOOL_F;
    }

    SCM get_map1_helper() {
        if (is_closure(map1_helper)) return map1_helper;
        auto& v = vm();
        if (!v.module_system_booted || !v.fast_core_ok) return SCM_BOOL_F;
        SCM root_env = SCM_EOL;
        if (is_variable(v.the_root_module_var)) {
            SCM rm = as_obj<VariableObj>(v.the_root_module_var)->value;
            if (is_struct(rm) && as_obj<StructObj>(rm)->slots.size() > 3) {
                SCM ec{as_obj<StructObj>(rm)->slots[3]};
                if (ec.is_true()) root_env = v.list1(ec);
            }
        }
        SCM port = make_input_string_port(
            "(lambda (proc lst) "
            "  (let loop ((l lst) (acc '())) "
            "    (if (pair? l) "
            "        (loop (cdr l) (cons (proc (car l)) acc)) "
            "        (if (null? l) (reverse acc) (map proc l)))))");
        SCM form = v.read_from_port(port);
        SCM fn = v.eval(form, root_env);
        if (is_closure(fn) && bytecode_compile_closure(as_obj<ClosureObj>(fn))) {
            map1_helper = fn;
            return fn;
        }
        return SCM_BOOL_F;
    }

    void ensure_stack(size_t needed) {
        if (needed >= stack.size()) {
            size_t new_sz = stack.size() * 2;
            while (new_sz <= needed) new_sz *= 2;
            stack.resize(new_sz, SCM_UNSPECIFIED);
        }
    }

    void ensure_frames(size_t needed) {
        if (needed >= frames.size()) {
            frames.resize(frames.size() * 2);
        }
    }
};

static BytecodeEngine& bc_engine() {
    static BytecodeEngine eng;
    return eng;
}

// Fast inline hashv/hashq helpers for VM opcodes (MUST match scm_ihashq/scm_ihashv in hashtab.cpp!)
static inline size_t fast_hashq_val(SCM key, size_t sz) noexcept {
    uint64_t h = key.bits;
    if (key.is_heap()) {
        ObjTag t = key.to_obj()->tag;
        if (t == ObjTag::Symbol) h = as_obj<SymbolObj>(key)->hash;
        else if (t == ObjTag::Keyword) h = as_obj<KeywordObj>(key)->symbol.bits ^ 0x9e3779b97f4a7c15ULL;
    }
    h ^= (h >> 33);
    h *= 0xff51afd7ed558ccdULL;
    h ^= (h >> 33);
    return ((sz & (sz - 1)) == 0) ? static_cast<size_t>(h & (sz - 1)) : static_cast<size_t>(h % sz);
}

static inline size_t fast_hashv_val(SCM key, size_t sz) noexcept {
    if (!key.is_heap()) {
        uint64_t h = key.bits;
        h ^= (h >> 33);
        h *= 0xff51afd7ed558ccdULL;
        h ^= (h >> 33);
        return ((sz & (sz - 1)) == 0) ? static_cast<size_t>(h & (sz - 1)) : static_cast<size_t>(h % sz);
    }
    ObjTag t = key.to_obj()->tag;
    if (t == ObjTag::Real || t == ObjTag::Bignum || t == ObjTag::Fraction || t == ObjTag::Complex) {
        return static_cast<size_t>(scm_ihashv(key, sz));
    }
    return fast_hashq_val(key, sz);
}

static inline SCM fast_hash_lookup(HashTableObj* ht, SCM key, bool is_hashq, SCM def_val) noexcept {
    size_t sz = ht->buckets.size();
    if (sz == 0) return def_val;
    size_t idx = is_hashq ? fast_hashq_val(key, sz) : fast_hashv_val(key, sz);
    SCM cur = ht->buckets[idx];
    if (is_hashq || !key.is_heap()) {
        while (is_pair(cur)) {
            SCM entry = car(cur);
            if (is_pair(entry) && car(entry) == key) return cdr(entry);
            cur = cdr(cur);
        }
        return def_val;
    }
    while (is_pair(cur)) {
        SCM entry = car(cur);
        if (is_pair(entry) && scm_eqv(car(entry), key)) return cdr(entry);
        cur = cdr(cur);
    }
    return def_val;
}

static inline void fast_hash_store(SCM ht_scm, HashTableObj* ht, SCM key, SCM val, bool is_hashq) {
    size_t sz = ht->buckets.size();
    if (sz == 0 || ht->weak != WeakKind::None) { hash_set(ht_scm, key, val, is_hashq ? 1 : 2); return; }
    size_t idx = is_hashq ? fast_hashq_val(key, sz) : fast_hashv_val(key, sz);
    SCM cur = ht->buckets[idx];
    if (is_hashq || !key.is_heap()) {
        while (is_pair(cur)) {
            SCM entry = car(cur);
            if (is_pair(entry) && car(entry) == key) { set_cdr(entry, val); return; }
            cur = cdr(cur);
        }
        if (ht->count + 1 > sz * 9 / 10) {
            hash_set(ht_scm, key, val, is_hashq ? 1 : 2);
            return;
        }
        ht->buckets[idx] = vm().cons(vm().cons(key, val), ht->buckets[idx]);
        ++ht->count;
        return;
    }
    hash_set(ht_scm, key, val, 2);
}

// Out-of-line slow global call so SCM args[3] never lingers on run_bytecode_loop's C++ stack frame across GC!
[[gnu::noinline]] static SCM bc_slow_global_call(VM& v, BytecodeObj* bc, uint16_t gi,
                                                 SCM a0, SCM a1, SCM a2, size_t na) {
    SCM gvar = bc->globals[gi];
    if (!is_variable(gvar)) {
        gvar = v.sym2var(bc->global_syms[gi], bc->eval_closure, false);
        if (is_variable(gvar)) bc->globals[gi] = gvar;
        else v.error(v.sym_unbound_variable, nullptr, "Unbound variable: ~S", v.list1(bc->global_syms[gi]), SCM_BOOL_F);
    }
    SCM proc = as_obj<VariableObj>(gvar)->value;
    if (proc.is_undefined()) v.error(v.sym_unbound_variable, nullptr, "Unbound variable: ~S", v.list1(bc->global_syms[gi]), SCM_BOOL_F);
    SCM args[3] = {a0, a1, a2};
    return v.apply(proc, args, na);
}

[[gnu::noinline]] static SCM bc_slow_global_call_n(VM& v, BytecodeObj* bc, uint16_t gi,
                                                   const SCM* args_p, size_t na) {
    SCM gvar = bc->globals[gi];
    if (!is_variable(gvar)) {
        gvar = v.sym2var(bc->global_syms[gi], bc->eval_closure, false);
        if (is_variable(gvar)) bc->globals[gi] = gvar;
        else v.error(v.sym_unbound_variable, nullptr, "Unbound variable: ~S", v.list1(bc->global_syms[gi]), SCM_BOOL_F);
    }
    SCM proc = as_obj<VariableObj>(gvar)->value;
    if (proc.is_undefined()) v.error(v.sym_unbound_variable, nullptr, "Unbound variable: ~S", v.list1(bc->global_syms[gi]), SCM_BOOL_F);
    SCM tmp[16];
    for (size_t i = 0; i < na; ++i) tmp[i] = args_p[i];
    return v.apply(proc, tmp, na);
}

// Out-of-line helper so temporary SCM arrays (tmp[16]) don't linger on run_bytecode_loop's C++ stack across GC!
[[gnu::noinline]] static SCM call_subr_padded(VM& v, SubrObj* sb, const SCM* args, size_t nargs) {
    int req = sb->req, opt = sb->opt;
    if (!sb->rst) {
        SCM tmp[16];
        for (size_t i = 0; i < nargs; ++i) tmp[i] = args[i];
        for (int i = static_cast<int>(nargs); i < req + opt; ++i) tmp[i] = SCM_UNDEFINED;
        return sb->fn(tmp, nargs);
    }
    SCM rest_l = v.list_n(args + req + opt,
                          nargs > static_cast<size_t>(req + opt) ? nargs - static_cast<size_t>(req + opt) : 0);
    SCM tmp[16];
    size_t keep = std::min<size_t>(nargs, static_cast<size_t>(req + opt));
    for (size_t i = 0; i < keep; ++i) tmp[i] = args[i];
    for (size_t i = keep; i < static_cast<size_t>(req + opt); ++i) tmp[i] = SCM_UNDEFINED;
    tmp[req + opt] = rest_l;
    return sb->fn(tmp, static_cast<size_t>(req + opt + 1));
}

// Execute a compiled closure or resume a continuation inside the computed-goto loop!
static SCM run_bytecode_loop(ClosureObj* entry_cl, const SCM* entry_args, size_t entry_nargs,
                             ContinuationObj* resume_k = nullptr, SCM resume_val = SCM_UNSPECIFIED) {
    auto& v = vm();
    v.check_c_stack();
    auto& eng = bc_engine();

    const size_t entry_sp = eng.active_sp;
    const size_t entry_fp = eng.active_fp;
    const int my_vm_depth = ++v.next_bc_inv_id;
    ++v.bc_vm_depth;
    v.active_bc_invs.push_back(my_vm_depth);

    struct DepthGuard {
        VM& vm_ref;
        BytecodeEngine& eng_ref;
        size_t esp, efp;
        ~DepthGuard() {
            eng_ref.active_sp = esp;
            eng_ref.active_fp = efp;
            vm_ref.active_bc_invs.pop_back();
            --vm_ref.bc_vm_depth;
        }
    } dguard{v, eng, entry_sp, entry_fp};

    // Active registers
    ClosureObj* cl = nullptr;
    BytecodeObj* bc = nullptr;
    SCM* upvals_ptr = nullptr;
    const uint32_t* pc = nullptr;
    uint32_t base_off = static_cast<uint32_t>(entry_sp);
    size_t sp_idx = entry_sp;
    size_t fp_idx = entry_fp;

    auto sync_gc = [&]() noexcept {
        eng.active_sp = sp_idx;
        eng.active_fp = fp_idx;
    };

    auto load_frame_regs = [&](ClosureObj* new_cl, BytecodeObj* new_bc) noexcept {
        cl = new_cl;
        bc = new_bc;
        upvals_ptr = cl->inline_upval_cnt > 0
            ? cl->inline_upvals
            : (is_vector(cl->upvals) ? as_obj<VectorObj>(cl->upvals)->elts.data() : nullptr);
    };

    auto setup_closure_args = [&](ClosureObj* target_cl, BytecodeObj* target_bc,
                                  uint32_t target_base, const SCM* a, size_t na) {
        size_t req = target_bc->arity_req;
        if (!target_bc->has_rest) {
            if (na != req) { sync_gc(); v.wrong_num_args(nullptr, SCM::from_obj(target_cl)); }
            eng.ensure_stack(target_base + target_bc->max_locals + 64);
            SCM* dst = eng.stack.data() + target_base;
            for (size_t i = 0; i < na; ++i) dst[i] = a[i];
            for (size_t i = na; i < target_bc->max_locals; ++i) dst[i] = SCM_UNSPECIFIED;
        } else {
            if (na < req) { sync_gc(); v.wrong_num_args(nullptr, SCM::from_obj(target_cl)); }
            sync_gc();
            SCM rest_lst = v.list_n(a + req, na - req);
            eng.ensure_stack(target_base + target_bc->max_locals + 64);
            SCM* dst = eng.stack.data() + target_base;
            for (size_t i = 0; i < req; ++i) dst[i] = a[i];
            dst[req] = rest_lst;
            for (size_t i = req + 1; i < target_bc->max_locals; ++i) dst[i] = SCM_UNSPECIFIED;
        }
    };

    if (resume_k) {
        size_t sn = resume_k->snap_stack_size();
        size_t fn = resume_k->snap_frames_size();
        if (fn == 0) {
            return resume_val;
        }
        eng.ensure_stack(entry_sp + sn + 64);
        eng.ensure_frames(entry_fp + fn + 16);
        std::memcpy(eng.stack.data() + entry_sp, resume_k->snap_stack_data(), sn * sizeof(SCM));
        const VmFrameSnap* fd = resume_k->snap_frames_data();
        for (size_t i = 0; i < fn; ++i) {
            const auto& sf = fd[i];
            auto* fbc = as_obj<BytecodeObj>(sf.bytecode);
            eng.frames[entry_fp + i] = {
                sf.closure, sf.bytecode, fbc->code.data() + sf.pc_off,
                static_cast<uint32_t>(entry_sp + sf.base_off),
                static_cast<uint32_t>(entry_sp + sf.ret_slot)
            };
        }
        fp_idx = entry_fp + fn - 1;
        const auto& topf = eng.frames[fp_idx];
        load_frame_regs(as_obj<ClosureObj>(topf.closure), as_obj<BytecodeObj>(topf.bytecode));
        pc = topf.pc;
        base_off = topf.base_off;
        sp_idx = entry_sp + sn;
        eng.stack[sp_idx++] = resume_val;
    } else {
        auto* entry_bc = as_obj<BytecodeObj>(entry_cl->bytecode);
        setup_closure_args(entry_cl, entry_bc, base_off, entry_args, entry_nargs);
        load_frame_regs(entry_cl, entry_bc);
        pc = bc->code.data();
        sp_idx = base_off + bc->max_locals;
    }

    // Computed-goto jump table
    static const void* dispatch_table[OP_NUM_OPS] = {
        &&L_CONST, &&L_FIXNUM, &&L_TRUE, &&L_FALSE, &&L_NIL, &&L_UNSPEC, &&L_UNDEF, &&L_POP, &&L_DUP,
        &&L_LOCAL_REF, &&L_LOCAL_REF_CHK, &&L_LOCAL_SET,
        &&L_BOX_INIT, &&L_BOX_NEW_UNDEF, &&L_BOX_REF, &&L_BOX_REF_CHK, &&L_BOX_SET,
        &&L_UPVAL_REF, &&L_UPVAL_BOX_REF, &&L_UPVAL_BOX_REF_CHK, &&L_UPVAL_BOX_SET,
        &&L_CELL_CAR_REF, &&L_CELL_CAR_SET, &&L_CELL_CDR_REF, &&L_CELL_CDR_SET,
        &&L_GLOBAL_REF, &&L_GLOBAL_SET,
        &&L_JUMP, &&L_JUMP_IF_FALSE, &&L_JUMP_IF_TRUE, &&L_JUMP_IF_FALSE_KEEP, &&L_JUMP_IF_TRUE_KEEP,
        &&L_MAKE_CLOSURE, &&L_CALL, &&L_TAIL_CALL, &&L_RETURN,
        &&L_ADD2, &&L_SUB2, &&L_MUL2, &&L_DIV2, &&L_ABS, &&L_ADDN, &&L_ADD1, &&L_SUB1,
        &&L_NUMEQ2, &&L_LT2, &&L_LE2, &&L_GT2, &&L_GE2, &&L_EVENP, &&L_ODDP,
        &&L_EQ, &&L_NULLP, &&L_PAIRP, &&L_NOT, &&L_CAR, &&L_CDR, &&L_CONS, &&L_REVERSE,
        &&L_VEC_REF, &&L_VEC_SET, &&L_MAKE_VEC, &&L_STRUCT_REF, &&L_STRUCT_SET,
        &&L_HASHV_REF, &&L_HASHV_SET, &&L_HASHQ_REF, &&L_HASHQ_SET, &&L_CALLCC,
        &&L_LOCAL_ADD_IMM, &&L_LOCAL_LT_IMM_JF, &&L_LOCAL_EQ_IMM_JF, &&L_CLEAR_LOCALS,
        &&L_DYNWIND_PUSH, &&L_DYNWIND_POP,
    };

    auto slow_global_call = [&](uint16_t gi, const SCM* args_p, size_t na) -> SCM {
        sync_gc();
        return bc_slow_global_call(v, bc, gi,
                                   na > 0 ? args_p[0] : SCM_UNSPECIFIED,
                                   na > 1 ? args_p[1] : SCM_UNSPECIFIED,
                                   na > 2 ? args_p[2] : SCM_UNSPECIFIED, na);
    };

    SCM* stk = eng.stack.data();
    uint32_t insn = 0;

    #define DISPATCH() do { insn = *pc++; goto *dispatch_table[dec_op(insn)]; } while (0)
    #define RELOAD_STK() do { stk = eng.stack.data(); } while (0)

    while (true) {
        try {
            DISPATCH();

        L_CONST:
            stk[sp_idx++] = bc->consts[dec_u24(insn)];
            DISPATCH();

        L_FIXNUM:
            stk[sp_idx++] = SCM::from_fixnum(dec_s16(insn));
            DISPATCH();

        L_TRUE:
            stk[sp_idx++] = SCM_BOOL_T;
            DISPATCH();

        L_FALSE:
            stk[sp_idx++] = SCM_BOOL_F;
            DISPATCH();

        L_NIL:
            stk[sp_idx++] = SCM_EOL;
            DISPATCH();

        L_UNSPEC:
            stk[sp_idx++] = SCM_UNSPECIFIED;
            DISPATCH();

        L_UNDEF:
            stk[sp_idx++] = SCM_UNDEFINED;
            DISPATCH();

        L_POP:
            --sp_idx;
            DISPATCH();

        L_DUP:
            stk[sp_idx] = stk[sp_idx - 1];
            ++sp_idx;
            DISPATCH();

        L_LOCAL_REF:
            stk[sp_idx++] = stk[base_off + dec_u24(insn)];
            DISPATCH();

        L_LOCAL_REF_CHK: {
            SCM val = stk[base_off + dec_u24(insn)];
            if (GCPP_UNLIKELY(val.is_undefined())) {
                sync_gc();
                v.error(v.sym_unbound_variable, nullptr, "Variable used before given a value: ~S", v.list1(SCM_BOOL_F), SCM_BOOL_F);
            }
            stk[sp_idx++] = val;
            DISPATCH();
        }

        L_LOCAL_SET:
            stk[base_off + dec_u24(insn)] = stk[--sp_idx];
            DISPATCH();

        L_BOX_INIT: {
            sync_gc();
            uint32_t sl = dec_u24(insn);
            stk[base_off + sl] = v.heap().alloc_scm<BoxObj>(stk[base_off + sl]);
            DISPATCH();
        }

        L_BOX_NEW_UNDEF: {
            sync_gc();
            uint32_t sl = dec_u24(insn);
            stk[base_off + sl] = v.heap().alloc_scm<BoxObj>(SCM_UNDEFINED);
            DISPATCH();
        }

        L_BOX_REF:
            stk[sp_idx++] = as_obj<BoxObj>(stk[base_off + dec_u24(insn)])->val;
            DISPATCH();

        L_BOX_REF_CHK: {
            SCM val = as_obj<BoxObj>(stk[base_off + dec_u24(insn)])->val;
            if (GCPP_UNLIKELY(val.is_undefined())) {
                sync_gc();
                v.error(v.sym_unbound_variable, nullptr, "Variable used before given a value: ~S", v.list1(SCM_BOOL_F), SCM_BOOL_F);
            }
            stk[sp_idx++] = val;
            DISPATCH();
        }

        L_BOX_SET:
            as_obj<BoxObj>(stk[base_off + dec_u24(insn)])->val = stk[--sp_idx];
            DISPATCH();

        L_UPVAL_REF:
            stk[sp_idx++] = upvals_ptr[dec_u24(insn)];
            DISPATCH();

        L_UPVAL_BOX_REF:
            stk[sp_idx++] = as_obj<BoxObj>(upvals_ptr[dec_u24(insn)])->val;
            DISPATCH();

        L_UPVAL_BOX_REF_CHK: {
            SCM val = as_obj<BoxObj>(upvals_ptr[dec_u24(insn)])->val;
            if (GCPP_UNLIKELY(val.is_undefined())) {
                sync_gc();
                v.error(v.sym_unbound_variable, nullptr, "Variable used before given a value: ~S", v.list1(SCM_BOOL_F), SCM_BOOL_F);
            }
            stk[sp_idx++] = val;
            DISPATCH();
        }

        L_UPVAL_BOX_SET:
            as_obj<BoxObj>(upvals_ptr[dec_u24(insn)])->val = stk[--sp_idx];
            DISPATCH();

        L_CELL_CAR_REF:
            stk[sp_idx++] = car(upvals_ptr[dec_u24(insn)]);
            DISPATCH();

        L_CELL_CAR_SET:
            set_car(upvals_ptr[dec_u24(insn)], stk[--sp_idx]);
            DISPATCH();

        L_CELL_CDR_REF:
            stk[sp_idx++] = cdr(upvals_ptr[dec_u24(insn)]);
            DISPATCH();

        L_CELL_CDR_SET:
            set_cdr(upvals_ptr[dec_u24(insn)], stk[--sp_idx]);
            DISPATCH();

        L_GLOBAL_REF: {
            uint32_t gi = dec_u24(insn);
            SCM gv = bc->globals[gi];
            if (GCPP_LIKELY(is_variable(gv))) {
                SCM val = as_obj<VariableObj>(gv)->value;
                if (GCPP_LIKELY(!val.is_undefined())) {
                    stk[sp_idx++] = val;
                    DISPATCH();
                }
            }
            sync_gc();
            gv = v.sym2var(bc->global_syms[gi], bc->eval_closure, false);
            if (!is_variable(gv) || as_obj<VariableObj>(gv)->value.is_undefined()) {
                v.error(v.sym_unbound_variable, nullptr, "Unbound variable: ~S", v.list1(bc->global_syms[gi]), SCM_BOOL_F);
            }
            bc->globals[gi] = gv;
            stk[sp_idx++] = as_obj<VariableObj>(gv)->value;
            DISPATCH();
        }

        L_GLOBAL_SET: {
            uint32_t gi = dec_u24(insn);
            SCM val = stk[--sp_idx];
            SCM gv = bc->globals[gi];
            if (!is_variable(gv)) {
                sync_gc();
                gv = v.sym2var(bc->global_syms[gi], bc->eval_closure, false);
                if (!is_variable(gv)) {
                    v.error(v.sym_unbound_variable, "set!", "Unbound variable: ~S", v.list1(bc->global_syms[gi]));
                }
                bc->globals[gi] = gv;
            }
            auto* vo = as_obj<VariableObj>(gv);
            vo->value = val;
            v.notify_var_mutated(vo);
            DISPATCH();
        }

        L_JUMP:
            pc += dec_s16(insn);
            DISPATCH();

        L_JUMP_IF_FALSE:
            if (stk[--sp_idx].is_false()) pc += dec_s16(insn);
            DISPATCH();

        L_JUMP_IF_TRUE:
            if (stk[--sp_idx].is_true()) pc += dec_s16(insn);
            DISPATCH();

        L_JUMP_IF_FALSE_KEEP:
            if (stk[sp_idx - 1].is_false()) pc += dec_s16(insn);
            else --sp_idx;
            DISPATCH();

        L_JUMP_IF_TRUE_KEEP:
            if (stk[sp_idx - 1].is_true()) pc += dec_s16(insn);
            else --sp_idx;
            DISPATCH();

        L_MAKE_CLOSURE: {
            sync_gc();
            SCM proto_scm = bc->consts[dec_u24(insn)];
            auto* proto = as_obj<BytecodeObj>(proto_scm);
            auto* ncl = v.heap().alloc<ClosureObj>(proto->source_formals, proto->source_body, proto->cached_env);
            ncl->bytecode = proto_scm;
            uint16_t nuv = proto->num_upvals;
            if (nuv > 0) {
                if (GCPP_LIKELY(nuv <= ClosureObj::INLINE_UPVAL_CAP)) {
                    ncl->inline_upval_cnt = static_cast<uint8_t>(nuv);
                    for (uint16_t i = 0; i < nuv; ++i) {
                        uint32_t desc = proto->upval_descs[i];
                        uint32_t kind = desc >> 16;
                        uint32_t idx = desc & 0xFFFFu;
                        ncl->inline_upvals[i] = (kind == 1) ? stk[base_off + idx] : upvals_ptr[idx];
                    }
                } else {
                    auto* uv = v.heap().alloc<VectorObj>(nuv, SCM_BOOL_F);
                    for (uint16_t i = 0; i < nuv; ++i) {
                        uint32_t desc = proto->upval_descs[i];
                        uint32_t kind = desc >> 16;
                        uint32_t idx = desc & 0xFFFFu;
                        uv->elts[i] = (kind == 1) ? stk[base_off + idx] : upvals_ptr[idx];
                    }
                    ncl->upvals = SCM::from_obj(uv);
                }
            }
            stk[sp_idx++] = SCM::from_obj(ncl);
            DISPATCH();
        }

        L_CALL: {
            size_t nargs = dec_u24(insn);
            size_t fn_slot = sp_idx - 1 - nargs;
            SCM callee = stk[fn_slot];
            if (GCPP_LIKELY(is_closure(callee))) {
                auto* tcl = as_obj<ClosureObj>(callee);
                if (GCPP_LIKELY(is_bytecode(tcl->bytecode))) {
                    auto* tbc = as_obj<BytecodeObj>(tcl->bytecode);
                    if (GCPP_LIKELY(!tbc->has_rest && nargs == tbc->arity_req)) {
#ifdef GUILE_CPP_JIT
                        if (v.jit_enabled && tbc->num_upvals <= ClosureObj::INLINE_UPVAL_CAP && v.fast_arith_ok && v.fast_core_ok && !v.debug_opts.debug) {
                            if (tbc->jit_code && tbc->jit_epoch == v.jit_epoch) {
                                sync_gc();
                                uint64_t jr = jit_invoke(tcl, tbc, stk + fn_slot + 1, nargs);
                                RELOAD_STK();
                                if (GCPP_LIKELY((jr & 1ULL) == 0)) {
                                    stk[fn_slot] = SCM(jr);
                                    sp_idx = fn_slot + 1;
                                    DISPATCH();
                                }
                            } else if (tbc->jit_calls != 0xFFFFFFFFu && (++tbc->jit_calls >= 16 || tbc->has_loop)) {
                                sync_gc();
                                bool compiled = jit_try_compile(tcl, tbc);
                                RELOAD_STK();
                                if (compiled) {
                                    uint64_t jr = jit_invoke(tcl, tbc, stk + fn_slot + 1, nargs);
                                    RELOAD_STK();
                                    if (GCPP_LIKELY((jr & 1ULL) == 0)) {
                                        stk[fn_slot] = SCM(jr);
                                        sp_idx = fn_slot + 1;
                                        DISPATCH();
                                    }
                                }
                            }
                        }
#endif
                        eng.ensure_frames(fp_idx + 2);
                        eng.frames[fp_idx++] = {SCM::from_obj(cl), SCM::from_obj(bc), pc, base_off, static_cast<uint32_t>(fn_slot)};
                        uint32_t new_base = static_cast<uint32_t>(fn_slot + 1);
                        eng.ensure_stack(new_base + tbc->max_locals + 64);
                        RELOAD_STK();
                        for (size_t i = nargs; i < tbc->max_locals; ++i) stk[new_base + i] = SCM_UNSPECIFIED;
                        load_frame_regs(tcl, tbc);
                        base_off = new_base;
                        sp_idx = new_base + tbc->max_locals;
                        pc = tbc->code.data();
                        DISPATCH();
                    }
                    eng.ensure_frames(fp_idx + 2);
                    eng.frames[fp_idx++] = {SCM::from_obj(cl), SCM::from_obj(bc), pc, base_off, static_cast<uint32_t>(fn_slot)};
                    uint32_t new_base = static_cast<uint32_t>(fn_slot + 1);
                    setup_closure_args(tcl, tbc, new_base, stk + fn_slot + 1, nargs);
                    RELOAD_STK();
                    load_frame_regs(tcl, tbc);
                    base_off = new_base;
                    sp_idx = new_base + tbc->max_locals;
                    pc = tbc->code.data();
                    DISPATCH();
                }
                if (tcl->bytecode.is_false() && bytecode_should_compile(tcl)) {
                    sync_gc();
                    if (bytecode_compile_closure(tcl)) {
                        auto* tbc = as_obj<BytecodeObj>(tcl->bytecode);
                        eng.ensure_frames(fp_idx + 2);
                        eng.frames[fp_idx++] = {SCM::from_obj(cl), SCM::from_obj(bc), pc, base_off, static_cast<uint32_t>(fn_slot)};
                        uint32_t new_base = static_cast<uint32_t>(fn_slot + 1);
                        setup_closure_args(tcl, tbc, new_base, stk + fn_slot + 1, nargs);
                        RELOAD_STK();
                        load_frame_regs(tcl, tbc);
                        base_off = new_base;
                        sp_idx = new_base + tbc->max_locals;
                        pc = tbc->code.data();
                        DISPATCH();
                    }
                }
            } else if (is_subr(callee)) {
                auto* sb = as_obj<SubrObj>(callee);
                if (sb->special == SpecialSubr::None) {
                    int req = sb->req, opt = sb->opt;
                    if (static_cast<int>(nargs) >= req && (sb->rst || static_cast<int>(nargs) <= req + opt)) {
                        sync_gc();
                        SCM r = (sb->raw_varargs || (!sb->rst && static_cast<int>(nargs) == req + opt))
                            ? (sb->raw_fn ? sb->raw_fn(stk + fn_slot + 1, nargs) : sb->fn(stk + fn_slot + 1, nargs))
                            : call_subr_padded(v, sb, stk + fn_slot + 1, nargs);
                        RELOAD_STK();
                        stk[fn_slot] = r;
                        sp_idx = fn_slot + 1;
                        DISPATCH();
                    }
                } else if (sb->special == SpecialSubr::CallCC) {
                    if (nargs != 1) { sync_gc(); v.wrong_num_args("call-with-current-continuation", callee); }
                    SCM proc = stk[fn_slot + 1];
                    sync_gc();
                    auto* k = v.heap().alloc<ContinuationObj>(v.current_cont, v.current_dynwind, v.trampoline_depth);
                    k->vm_depth = my_vm_depth;
                    k->has_vm_snap = true;
                    k->set_stack_snap(stk + entry_sp, fn_slot - entry_sp);
                    size_t n_frames = fp_idx - entry_fp;
                    k->reserve_frames_snap(n_frames + 1);
                    for (size_t i = 0; i < n_frames; ++i) {
                        const auto& fr = eng.frames[entry_fp + i];
                        auto* fbc = as_obj<BytecodeObj>(fr.bytecode);
                        k->push_frame_snap({
                            fr.closure, fr.bytecode,
                            static_cast<uint32_t>(fr.pc - fbc->code.data()),
                            static_cast<uint32_t>(fr.base_off - entry_sp),
                            static_cast<uint32_t>(fr.ret_slot - entry_sp)
                        });
                    }
                    k->push_frame_snap({
                        SCM::from_obj(cl), SCM::from_obj(bc),
                        static_cast<uint32_t>(pc - bc->code.data()),
                        static_cast<uint32_t>(base_off - entry_sp),
                        0
                    });
                    SCM k_scm = SCM::from_obj(k);
                    stk[fn_slot] = proc;
                    stk[fn_slot + 1] = k_scm;
                    sp_idx = fn_slot + 2;
                    insn = enc_u24(OP_CALL, 1);
                    goto L_CALL;
                } else if (sb->special == SpecialSubr::DynamicWind) {
                    if (nargs != 3) { sync_gc(); v.wrong_num_args("dynamic-wind", callee); }
                    sync_gc();
                    SCM h = eng.get_dynwind_helper();
                    RELOAD_STK();
                    stk[fn_slot] = h;
                    goto L_CALL;
                } else if (sb->special == SpecialSubr::Apply) {
                    if (nargs < 2) { sync_gc(); v.wrong_num_args("apply", callee); }
                    SCM target = stk[fn_slot + 1];
                    SCM last = stk[fn_slot + nargs];
                    int64_t llen = scm_ilength(last);
                    if (llen < 0) { sync_gc(); v.wrong_type_arg("apply", static_cast<int>(nargs), last, "proper list"); }
                    size_t fixed = nargs - 2;
                    size_t total = fixed + static_cast<size_t>(llen);
                    eng.ensure_stack(fn_slot + 1 + total + 64);
                    RELOAD_STK();
                    stk[fn_slot] = target;
                    if (fixed > 0) std::memmove(stk + fn_slot + 1, stk + fn_slot + 2, fixed * sizeof(SCM));
                    size_t pos = fn_slot + 1 + fixed;
                    while (is_pair(last)) { stk[pos++] = car(last); last = cdr(last); }
                    sp_idx = pos;
                    insn = enc_u24(OP_CALL, static_cast<uint32_t>(total));
                    goto L_CALL;
                } else if (sb->special == SpecialSubr::ForEach && nargs == 2 && v.fast_core_ok) {
                    SCM lst = stk[fn_slot + 2];
                    if (lst.is_null()) {
                        stk[fn_slot] = SCM_UNSPECIFIED;
                        sp_idx = fn_slot + 1;
                        DISPATCH();
                    }
                    if (is_pair(lst)) {
                        sync_gc();
                        SCM h = eng.get_foreach1_helper();
                        RELOAD_STK();
                        if (is_closure(h)) {
                            stk[fn_slot] = h;
                            goto L_CALL;
                        }
                    }
                } else if (sb->special == SpecialSubr::Map && nargs == 2 && v.fast_core_ok) {
                    SCM lst = stk[fn_slot + 2];
                    if (lst.is_null()) {
                        stk[fn_slot] = SCM_EOL;
                        sp_idx = fn_slot + 1;
                        DISPATCH();
                    }
                    if (is_pair(lst)) {
                        sync_gc();
                        SCM h = eng.get_map1_helper();
                        RELOAD_STK();
                        if (is_closure(h)) {
                            stk[fn_slot] = h;
                            goto L_CALL;
                        }
                    }
                }
            } else if (is_struct(callee)) {
                auto* gso = as_obj<StructObj>(callee);
                if (GCPP_LIKELY(gso->gf_cache_tag == gso->entity_proc && is_closure(gso->gf_cache_clo))) {
                    if (GCPP_LIKELY(nargs == 1 && gso->gf_cache_cls1.is_undefined() && is_struct(stk[fn_slot + 1]))) {
                        SCM c0 = as_obj<StructObj>(stk[fn_slot + 1])->vtable;
                        if (GCPP_LIKELY(c0 == gso->gf_cache_cls0 && (as_obj<StructObj>(c0)->vtable_flags & CLASSF_GOOPS_VALID))) {
                            stk[fn_slot] = gso->gf_cache_clo;
                            goto L_CALL;
                        }
                    } else if (nargs == 2 && is_struct(stk[fn_slot + 1]) && is_struct(stk[fn_slot + 2])) {
                        SCM c0 = as_obj<StructObj>(stk[fn_slot + 1])->vtable;
                        SCM c1 = as_obj<StructObj>(stk[fn_slot + 2])->vtable;
                        if (GCPP_LIKELY(c0 == gso->gf_cache_cls0 && c1 == gso->gf_cache_cls1 &&
                            (as_obj<StructObj>(c0)->vtable_flags & CLASSF_GOOPS_VALID) &&
                            (as_obj<StructObj>(c1)->vtable_flags & CLASSF_GOOPS_VALID))) {
                            stk[fn_slot] = gso->gf_cache_clo;
                            goto L_CALL;
                        }
                    }
                }
            } else if (is_continuation(callee)) {
                auto* k = as_obj<ContinuationObj>(callee);
                SCM rval = (nargs == 0) ? v.make_values(SCM_EOL)
                         : (nargs == 1 ? stk[fn_slot + 1] : v.make_values(v.list_n(stk + fn_slot + 1, nargs)));
                if (GCPP_LIKELY(k->has_vm_snap && k->vm_depth == my_vm_depth)) {
                    sync_gc();
                    switch_dynwind_pub(k->dynwind_chain);
                    size_t sn = k->snap_stack_size();
                    size_t fn = k->snap_frames_size();
                    if (fn == 0) {
                        return rval;
                    }
                    eng.ensure_stack(entry_sp + sn + 64);
                    eng.ensure_frames(entry_fp + fn + 16);
                    RELOAD_STK();
                    std::memcpy(stk + entry_sp, k->snap_stack_data(), sn * sizeof(SCM));
                    const VmFrameSnap* fd = k->snap_frames_data();
                    for (size_t i = 0; i < fn; ++i) {
                        const auto& sf = fd[i];
                        auto* fbc = as_obj<BytecodeObj>(sf.bytecode);
                        eng.frames[entry_fp + i] = {
                            sf.closure, sf.bytecode, fbc->code.data() + sf.pc_off,
                            static_cast<uint32_t>(entry_sp + sf.base_off),
                            static_cast<uint32_t>(entry_sp + sf.ret_slot)
                        };
                    }
                    fp_idx = entry_fp + fn - 1;
                    const auto& topf = eng.frames[fp_idx];
                    load_frame_regs(as_obj<ClosureObj>(topf.closure), as_obj<BytecodeObj>(topf.bytecode));
                    pc = topf.pc;
                    base_off = topf.base_off;
                    sp_idx = entry_sp + sn;
                    stk[sp_idx++] = rval;
                    DISPATCH();
                } else if (k->has_vm_snap && k->vm_depth > 0 && v.is_bc_inv_active(k->vm_depth)) {
                    ContinuationEscape esc;
                    esc.target_cont = k->cont_frame;
                    esc.target_dw = k->dynwind_chain;
                    esc.return_val = rval;
                    esc.target_depth = 0;
                    esc.target_vm_depth = k->vm_depth;
                    esc.target_k_obj = callee;
                    throw esc;
                } else if (!k->has_vm_snap && k->cek_depth > 0 && k->cek_depth <= v.trampoline_depth) {
                    ContinuationEscape esc;
                    esc.target_cont = k->cont_frame;
                    esc.target_dw = k->dynwind_chain;
                    esc.return_val = rval;
                    esc.target_depth = k->cek_depth;
                    esc.target_vm_depth = -1;
                    esc.target_k_obj = callee;
                    throw esc;
                }
            }
            sync_gc();
            SCM res = is_macro(callee)
                ? v.eval(v.cons(callee, v.list_n(stk + fn_slot + 1, nargs)), cl->env)
                : v.apply(callee, stk + fn_slot + 1, nargs);
            RELOAD_STK();
            stk[fn_slot] = res;
            sp_idx = fn_slot + 1;
            DISPATCH();
        }

        L_TAIL_CALL: {
            size_t nargs = dec_u24(insn);
            size_t fn_slot = sp_idx - 1 - nargs;
            SCM callee = stk[fn_slot];
            if (GCPP_LIKELY(is_closure(callee))) {
                auto* tcl = as_obj<ClosureObj>(callee);
                if (GCPP_LIKELY(is_bytecode(tcl->bytecode))) {
                    auto* tbc = as_obj<BytecodeObj>(tcl->bytecode);
                    if (GCPP_LIKELY(!tbc->has_rest && nargs == tbc->arity_req)) {
#ifdef GUILE_CPP_JIT
                        if (v.jit_enabled && tbc->num_upvals <= ClosureObj::INLINE_UPVAL_CAP && v.fast_arith_ok && v.fast_core_ok && !v.debug_opts.debug) {
                            if (tbc->jit_code && tbc->jit_epoch == v.jit_epoch) {
                                sync_gc();
                                uint64_t jr = jit_invoke(tcl, tbc, stk + fn_slot + 1, nargs);
                                RELOAD_STK();
                                if (GCPP_LIKELY((jr & 1ULL) == 0)) {
                                    stk[sp_idx++] = SCM(jr);
                                    goto L_RETURN;
                                }
                            } else if (tbc->jit_calls != 0xFFFFFFFFu && (++tbc->jit_calls >= 16 || tbc->has_loop)) {
                                sync_gc();
                                bool compiled = jit_try_compile(tcl, tbc);
                                RELOAD_STK();
                                if (compiled) {
                                    uint64_t jr = jit_invoke(tcl, tbc, stk + fn_slot + 1, nargs);
                                    RELOAD_STK();
                                    if (GCPP_LIKELY((jr & 1ULL) == 0)) {
                                        stk[sp_idx++] = SCM(jr);
                                        goto L_RETURN;
                                    }
                                }
                            }
                        }
#endif
                        eng.ensure_stack(base_off + tbc->max_locals + 64);
                        RELOAD_STK();
                        std::memmove(stk + base_off, stk + fn_slot + 1, nargs * sizeof(SCM));
                        for (size_t i = nargs; i < tbc->max_locals; ++i) stk[base_off + i] = SCM_UNSPECIFIED;
                        load_frame_regs(tcl, tbc);
                        sp_idx = base_off + tbc->max_locals;
                        pc = tbc->code.data();
                        DISPATCH();
                    }
                    {
                        std::vector<SCM> tmp_args(stk + fn_slot + 1, stk + fn_slot + 1 + nargs);
                        setup_closure_args(tcl, tbc, base_off, tmp_args.data(), nargs);
                    }
                    RELOAD_STK();
                    load_frame_regs(tcl, tbc);
                    sp_idx = base_off + tbc->max_locals;
                    pc = tbc->code.data();
                    DISPATCH();
                }
                if (tcl->bytecode.is_false() && bytecode_should_compile(tcl)) {
                    sync_gc();
                    if (bytecode_compile_closure(tcl)) {
                        auto* tbc = as_obj<BytecodeObj>(tcl->bytecode);
                        {
                            std::vector<SCM> tmp_args(stk + fn_slot + 1, stk + fn_slot + 1 + nargs);
                            setup_closure_args(tcl, tbc, base_off, tmp_args.data(), nargs);
                        }
                        RELOAD_STK();
                        load_frame_regs(tcl, tbc);
                        sp_idx = base_off + tbc->max_locals;
                        pc = tbc->code.data();
                        DISPATCH();
                    }
                }
            } else if (is_subr(callee)) {
                auto* sb = as_obj<SubrObj>(callee);
                if (sb->special == SpecialSubr::None) {
                    int req = sb->req, opt = sb->opt;
                    if (static_cast<int>(nargs) >= req && (sb->rst || static_cast<int>(nargs) <= req + opt)) {
                        sync_gc();
                        SCM res;
                        if (sb->raw_varargs || (!sb->rst && static_cast<int>(nargs) == req + opt)) {
                            res = sb->raw_fn ? sb->raw_fn(stk + fn_slot + 1, nargs) : sb->fn(stk + fn_slot + 1, nargs);
                        } else {
                            res = call_subr_padded(v, sb, stk + fn_slot + 1, nargs);
                        }
                        RELOAD_STK();
                        stk[sp_idx++] = res;
                        goto L_RETURN;
                    }
                } else if (sb->special == SpecialSubr::CallCC) {
                    if (nargs != 1) { sync_gc(); v.wrong_num_args("call-with-current-continuation", callee); }
                    SCM proc = stk[fn_slot + 1];
                    sync_gc();
                    auto* k = v.heap().alloc<ContinuationObj>(v.current_cont, v.current_dynwind, v.trampoline_depth);
                    k->vm_depth = my_vm_depth;
                    k->has_vm_snap = true;
                    size_t n_frames = fp_idx - entry_fp;
                    if (n_frames > 0) {
                        const auto& caller = eng.frames[fp_idx - 1];
                        k->set_stack_snap(stk + entry_sp, caller.ret_slot - entry_sp);
                        k->reserve_frames_snap(n_frames);
                        for (size_t i = 0; i < n_frames; ++i) {
                            const auto& fr = eng.frames[entry_fp + i];
                            auto* fbc = as_obj<BytecodeObj>(fr.bytecode);
                            k->push_frame_snap({
                                fr.closure, fr.bytecode,
                                static_cast<uint32_t>(fr.pc - fbc->code.data()),
                                static_cast<uint32_t>(fr.base_off - entry_sp),
                                static_cast<uint32_t>(fr.ret_slot - entry_sp)
                            });
                        }
                    }
                    // Tail-call (proc k) reusing current frame!
                    SCM k_scm = SCM::from_obj(k);
                    stk[fn_slot] = proc;
                    stk[fn_slot + 1] = k_scm;
                    sp_idx = fn_slot + 2;
                    insn = enc_u24(OP_TAIL_CALL, 1);
                    goto L_TAIL_CALL;
                } else if (sb->special == SpecialSubr::DynamicWind) {
                    if (nargs != 3) { sync_gc(); v.wrong_num_args("dynamic-wind", callee); }
                    sync_gc();
                    SCM h = eng.get_dynwind_helper();
                    RELOAD_STK();
                    stk[fn_slot] = h;
                    goto L_TAIL_CALL;
                } else if (sb->special == SpecialSubr::Apply) {
                    if (nargs < 2) { sync_gc(); v.wrong_num_args("apply", callee); }
                    SCM target = stk[fn_slot + 1];
                    SCM last = stk[fn_slot + nargs];
                    int64_t llen = scm_ilength(last);
                    if (llen < 0) { sync_gc(); v.wrong_type_arg("apply", static_cast<int>(nargs), last, "proper list"); }
                    size_t fixed = nargs - 2;
                    size_t total = fixed + static_cast<size_t>(llen);
                    eng.ensure_stack(fn_slot + 1 + total + 64);
                    RELOAD_STK();
                    stk[fn_slot] = target;
                    if (fixed > 0) std::memmove(stk + fn_slot + 1, stk + fn_slot + 2, fixed * sizeof(SCM));
                    size_t pos = fn_slot + 1 + fixed;
                    while (is_pair(last)) { stk[pos++] = car(last); last = cdr(last); }
                    sp_idx = pos;
                    insn = enc_u24(OP_TAIL_CALL, static_cast<uint32_t>(total));
                    goto L_TAIL_CALL;
                } else if (sb->special == SpecialSubr::ForEach && nargs == 2 && v.fast_core_ok) {
                    SCM lst = stk[fn_slot + 2];
                    if (lst.is_null()) {
                        stk[sp_idx++] = SCM_UNSPECIFIED;
                        goto L_RETURN;
                    }
                    if (is_pair(lst)) {
                        sync_gc();
                        SCM h = eng.get_foreach1_helper();
                        RELOAD_STK();
                        if (is_closure(h)) {
                            stk[fn_slot] = h;
                            goto L_TAIL_CALL;
                        }
                    }
                } else if (sb->special == SpecialSubr::Map && nargs == 2 && v.fast_core_ok) {
                    SCM lst = stk[fn_slot + 2];
                    if (lst.is_null()) {
                        stk[sp_idx++] = SCM_EOL;
                        goto L_RETURN;
                    }
                    if (is_pair(lst)) {
                        sync_gc();
                        SCM h = eng.get_map1_helper();
                        RELOAD_STK();
                        if (is_closure(h)) {
                            stk[fn_slot] = h;
                            goto L_TAIL_CALL;
                        }
                    }
                }
            } else if (is_struct(callee)) {
                auto* gso = as_obj<StructObj>(callee);
                if (GCPP_LIKELY(gso->gf_cache_tag == gso->entity_proc && is_closure(gso->gf_cache_clo))) {
                    if (GCPP_LIKELY(nargs == 1 && gso->gf_cache_cls1.is_undefined() && is_struct(stk[fn_slot + 1]))) {
                        SCM c0 = as_obj<StructObj>(stk[fn_slot + 1])->vtable;
                        if (GCPP_LIKELY(c0 == gso->gf_cache_cls0 && (as_obj<StructObj>(c0)->vtable_flags & CLASSF_GOOPS_VALID))) {
                            stk[fn_slot] = gso->gf_cache_clo;
                            goto L_TAIL_CALL;
                        }
                    } else if (nargs == 2 && is_struct(stk[fn_slot + 1]) && is_struct(stk[fn_slot + 2])) {
                        SCM c0 = as_obj<StructObj>(stk[fn_slot + 1])->vtable;
                        SCM c1 = as_obj<StructObj>(stk[fn_slot + 2])->vtable;
                        if (GCPP_LIKELY(c0 == gso->gf_cache_cls0 && c1 == gso->gf_cache_cls1 &&
                            (as_obj<StructObj>(c0)->vtable_flags & CLASSF_GOOPS_VALID) &&
                            (as_obj<StructObj>(c1)->vtable_flags & CLASSF_GOOPS_VALID))) {
                            stk[fn_slot] = gso->gf_cache_clo;
                            goto L_TAIL_CALL;
                        }
                    }
                }
            } else if (is_continuation(callee)) {
                auto* k = as_obj<ContinuationObj>(callee);
                SCM rval = (nargs == 0) ? v.make_values(SCM_EOL)
                         : (nargs == 1 ? stk[fn_slot + 1] : v.make_values(v.list_n(stk + fn_slot + 1, nargs)));
                if (GCPP_LIKELY(k->has_vm_snap && k->vm_depth == my_vm_depth)) {
                    sync_gc();
                    switch_dynwind_pub(k->dynwind_chain);
                    size_t sn = k->snap_stack_size();
                    size_t fn = k->snap_frames_size();
                    if (fn == 0) {
                        return rval;
                    }
                    eng.ensure_stack(entry_sp + sn + 64);
                    eng.ensure_frames(entry_fp + fn + 16);
                    RELOAD_STK();
                    std::memcpy(stk + entry_sp, k->snap_stack_data(), sn * sizeof(SCM));
                    const VmFrameSnap* fd = k->snap_frames_data();
                    for (size_t i = 0; i < fn; ++i) {
                        const auto& sf = fd[i];
                        auto* fbc = as_obj<BytecodeObj>(sf.bytecode);
                        eng.frames[entry_fp + i] = {
                            sf.closure, sf.bytecode, fbc->code.data() + sf.pc_off,
                            static_cast<uint32_t>(entry_sp + sf.base_off),
                            static_cast<uint32_t>(entry_sp + sf.ret_slot)
                        };
                    }
                    fp_idx = entry_fp + fn - 1;
                    const auto& topf = eng.frames[fp_idx];
                    load_frame_regs(as_obj<ClosureObj>(topf.closure), as_obj<BytecodeObj>(topf.bytecode));
                    pc = topf.pc;
                    base_off = topf.base_off;
                    sp_idx = entry_sp + sn;
                    stk[sp_idx++] = rval;
                    DISPATCH();
                } else if (k->has_vm_snap && k->vm_depth > 0 && v.is_bc_inv_active(k->vm_depth)) {
                    ContinuationEscape esc;
                    esc.target_cont = k->cont_frame;
                    esc.target_dw = k->dynwind_chain;
                    esc.return_val = rval;
                    esc.target_depth = 0;
                    esc.target_vm_depth = k->vm_depth;
                    esc.target_k_obj = callee;
                    throw esc;
                } else if (!k->has_vm_snap && k->cek_depth > 0 && k->cek_depth <= v.trampoline_depth) {
                    ContinuationEscape esc;
                    esc.target_cont = k->cont_frame;
                    esc.target_dw = k->dynwind_chain;
                    esc.return_val = rval;
                    esc.target_depth = k->cek_depth;
                    esc.target_vm_depth = -1;
                    esc.target_k_obj = callee;
                    throw esc;
                }
            }
            sync_gc();
            SCM res = is_macro(callee)
                ? v.eval(v.cons(callee, v.list_n(stk + fn_slot + 1, nargs)), cl->env)
                : v.apply(callee, stk + fn_slot + 1, nargs);
            RELOAD_STK();
            stk[sp_idx++] = res;
            goto L_RETURN;
        }

        L_RETURN: {
            if (fp_idx == entry_fp) {
                return stk[sp_idx - 1];
            }
            const auto& caller = eng.frames[--fp_idx];
            load_frame_regs(as_obj<ClosureObj>(caller.closure), as_obj<BytecodeObj>(caller.bytecode));
            pc = caller.pc;
            base_off = caller.base_off;
            stk[caller.ret_slot] = stk[sp_idx - 1];
            sp_idx = caller.ret_slot + 1;
            DISPATCH();
        }

        // ================================================================
        // Fast Inlined Arithmetic & Comparisons
        // ================================================================
        L_ADD2: {
            SCM b = stk[sp_idx - 1], a = stk[sp_idx - 2];
            if (GCPP_LIKELY(a.is_fixnum() && b.is_fixnum() && v.fast_arith_ok)) {
                int64_t r;
                if (GCPP_LIKELY(!__builtin_add_overflow(a.to_fixnum(), b.to_fixnum(), &r) && SCM::fits_fixnum(r))) {
                    stk[sp_idx - 2] = SCM::from_fixnum(r);
                    --sp_idx;
                    DISPATCH();
                }
            } else if (v.fast_arith_ok && (is_real_obj(a) || is_real_obj(b)) &&
                       (is_real_obj(a) || a.is_fixnum()) && (is_real_obj(b) || b.is_fixnum())) {
                double da = is_real_obj(a) ? as_obj<RealObj>(a)->val : static_cast<double>(a.to_fixnum());
                double db = is_real_obj(b) ? as_obj<RealObj>(b)->val : static_cast<double>(b.to_fixnum());
                sync_gc();
                stk[sp_idx - 2] = v.heap().alloc_scm<RealObj>(da + db);
                --sp_idx;
                DISPATCH();
            }
            SCM args[2] = {a, b};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_SUB2: {
            SCM b = stk[sp_idx - 1], a = stk[sp_idx - 2];
            if (GCPP_LIKELY(a.is_fixnum() && b.is_fixnum() && v.fast_arith_ok)) {
                int64_t r;
                if (GCPP_LIKELY(!__builtin_sub_overflow(a.to_fixnum(), b.to_fixnum(), &r) && SCM::fits_fixnum(r))) {
                    stk[sp_idx - 2] = SCM::from_fixnum(r);
                    --sp_idx;
                    DISPATCH();
                }
            } else if (v.fast_arith_ok && (is_real_obj(a) || is_real_obj(b)) &&
                       (is_real_obj(a) || a.is_fixnum()) && (is_real_obj(b) || b.is_fixnum())) {
                double da = is_real_obj(a) ? as_obj<RealObj>(a)->val : static_cast<double>(a.to_fixnum());
                double db = is_real_obj(b) ? as_obj<RealObj>(b)->val : static_cast<double>(b.to_fixnum());
                sync_gc();
                stk[sp_idx - 2] = v.heap().alloc_scm<RealObj>(da - db);
                --sp_idx;
                DISPATCH();
            }
            SCM args[2] = {a, b};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_MUL2: {
            SCM b = stk[sp_idx - 1], a = stk[sp_idx - 2];
            if (GCPP_LIKELY(v.fast_arith_ok)) {
                if (GCPP_LIKELY(a.is_fixnum() && b.is_fixnum())) {
                    int64_t r;
                    if (GCPP_LIKELY(!__builtin_mul_overflow(a.to_fixnum(), b.to_fixnum(), &r) && SCM::fits_fixnum(r))) {
                        stk[sp_idx - 2] = SCM::from_fixnum(r);
                        --sp_idx;
                        DISPATCH();
                    }
                }
                if ((is_real_obj(a) || is_real_obj(b)) &&
                    (is_real_obj(a) || (a.is_fixnum() && a != SCM::from_fixnum(0))) &&
                    (is_real_obj(b) || (b.is_fixnum() && b != SCM::from_fixnum(0)))) {
                    double da = is_real_obj(a) ? as_obj<RealObj>(a)->val : static_cast<double>(a.to_fixnum());
                    double db = is_real_obj(b) ? as_obj<RealObj>(b)->val : static_cast<double>(b.to_fixnum());
                    sync_gc();
                    stk[sp_idx - 2] = v.heap().alloc_scm<RealObj>(da * db);
                    --sp_idx;
                    DISPATCH();
                }
                if (is_number(a) && is_number(b)) {
                    sync_gc();
                    stk[sp_idx - 2] = scm_num_mul(a, b);
                    RELOAD_STK();
                    --sp_idx;
                    DISPATCH();
                }
            }
            SCM args[2] = {a, b};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_DIV2: {
            SCM b = stk[sp_idx - 1], a = stk[sp_idx - 2];
            if (GCPP_LIKELY(v.fast_arith_ok && (is_real_obj(a) || is_real_obj(b)) &&
                (is_real_obj(a) || a.is_fixnum()) &&
                (is_real_obj(b) || (b.is_fixnum() && b.to_fixnum() != 0)))) {
                double da = is_real_obj(a) ? as_obj<RealObj>(a)->val : static_cast<double>(a.to_fixnum());
                double db = is_real_obj(b) ? as_obj<RealObj>(b)->val : static_cast<double>(b.to_fixnum());
                sync_gc();
                stk[sp_idx - 2] = v.heap().alloc_scm<RealObj>(da / db);
                --sp_idx;
                DISPATCH();
            }
            SCM args[2] = {a, b};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_ABS: {
            SCM a = stk[sp_idx - 1];
            if (GCPP_LIKELY(a.is_fixnum() && v.fast_arith_ok)) {
                int64_t x = a.to_fixnum();
                if (x >= 0) { DISPATCH(); }
                int64_t r = -x;
                if (GCPP_LIKELY(SCM::fits_fixnum(r))) {
                    stk[sp_idx - 1] = SCM::from_fixnum(r);
                    DISPATCH();
                }
            } else if (is_real_obj(a) && v.fast_arith_ok) {
                double d = as_obj<RealObj>(a)->val;
                if (d >= 0.0 && !std::signbit(d)) { DISPATCH(); }
                sync_gc();
                stk[sp_idx - 1] = v.heap().alloc_scm<RealObj>(std::fabs(d));
                DISPATCH();
            }
            stk[sp_idx - 1] = slow_global_call(dec_b(insn), &a, 1);
            RELOAD_STK();
            DISPATCH();
        }

        L_ADDN: {
            uint8_t na = dec_a(insn);
            size_t base_a = sp_idx - na;
            if (GCPP_LIKELY(v.fast_arith_ok)) {
                if (na == 1) {
                    SCM a = stk[base_a];
                    if (GCPP_LIKELY(a.is_fixnum() || is_real_obj(a))) { DISPATCH(); }
                } else {
                    int64_t acc_i = 0;
                    double acc_d = 0.0;
                    bool is_fl = false;
                    bool ok = true;
                    for (uint8_t i = 0; i < na; ++i) {
                        SCM x = stk[base_a + i];
                        if (x.is_fixnum()) {
                            int64_t xi = x.to_fixnum();
                            if (is_fl) {
                                acc_d += static_cast<double>(xi);
                            } else {
                                int64_t r;
                                if (__builtin_add_overflow(acc_i, xi, &r) || !SCM::fits_fixnum(r)) {
                                    ok = false; break;
                                }
                                acc_i = r;
                            }
                        } else if (is_real_obj(x)) {
                            if (!is_fl) { acc_d = static_cast<double>(acc_i); is_fl = true; }
                            acc_d += as_obj<RealObj>(x)->val;
                        } else {
                            ok = false; break;
                        }
                    }
                    if (GCPP_LIKELY(ok)) {
                        if (is_fl) {
                            sync_gc();
                            stk[base_a] = v.heap().alloc_scm<RealObj>(acc_d);
                        } else {
                            stk[base_a] = SCM::from_fixnum(acc_i);
                        }
                        sp_idx = base_a + 1;
                        DISPATCH();
                    }
                }
            }
            sync_gc();
            SCM r = bc_slow_global_call_n(v, bc, dec_b(insn), stk + base_a, na);
            RELOAD_STK();
            stk[base_a] = r;
            sp_idx = base_a + 1;
            DISPATCH();
        }

        L_ADD1: {
            SCM a = stk[sp_idx - 1];
            if (GCPP_LIKELY(a.is_fixnum() && v.fast_arith_ok)) {
                int64_t r = a.to_fixnum() + 1;
                if (GCPP_LIKELY(SCM::fits_fixnum(r))) { stk[sp_idx - 1] = SCM::from_fixnum(r); DISPATCH(); }
            }
            stk[sp_idx - 1] = slow_global_call(dec_b(insn), &a, 1);
            RELOAD_STK();
            DISPATCH();
        }

        L_SUB1: {
            SCM a = stk[sp_idx - 1];
            if (GCPP_LIKELY(a.is_fixnum() && v.fast_arith_ok)) {
                int64_t r = a.to_fixnum() - 1;
                if (GCPP_LIKELY(SCM::fits_fixnum(r))) { stk[sp_idx - 1] = SCM::from_fixnum(r); DISPATCH(); }
            }
            stk[sp_idx - 1] = slow_global_call(dec_b(insn), &a, 1);
            RELOAD_STK();
            DISPATCH();
        }

        L_NUMEQ2: {
            SCM b = stk[sp_idx - 1], a = stk[sp_idx - 2];
            if (GCPP_LIKELY(a.is_fixnum() && b.is_fixnum() && v.fast_arith_ok)) {
                stk[sp_idx - 2] = SCM::from_bool(a == b);
                --sp_idx;
                DISPATCH();
            }
            SCM args[2] = {a, b};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_LT2: {
            SCM b = stk[sp_idx - 1], a = stk[sp_idx - 2];
            if (GCPP_LIKELY(a.is_fixnum() && b.is_fixnum() && v.fast_arith_ok)) {
                stk[sp_idx - 2] = SCM::from_bool(a.to_fixnum() < b.to_fixnum());
                --sp_idx;
                DISPATCH();
            }
            SCM args[2] = {a, b};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_LE2: {
            SCM b = stk[sp_idx - 1], a = stk[sp_idx - 2];
            if (GCPP_LIKELY(a.is_fixnum() && b.is_fixnum() && v.fast_arith_ok)) {
                stk[sp_idx - 2] = SCM::from_bool(a.to_fixnum() <= b.to_fixnum());
                --sp_idx;
                DISPATCH();
            }
            SCM args[2] = {a, b};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_GT2: {
            SCM b = stk[sp_idx - 1], a = stk[sp_idx - 2];
            if (GCPP_LIKELY(a.is_fixnum() && b.is_fixnum() && v.fast_arith_ok)) {
                stk[sp_idx - 2] = SCM::from_bool(a.to_fixnum() > b.to_fixnum());
                --sp_idx;
                DISPATCH();
            }
            SCM args[2] = {a, b};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_GE2: {
            SCM b = stk[sp_idx - 1], a = stk[sp_idx - 2];
            if (GCPP_LIKELY(a.is_fixnum() && b.is_fixnum() && v.fast_arith_ok)) {
                stk[sp_idx - 2] = SCM::from_bool(a.to_fixnum() >= b.to_fixnum());
                --sp_idx;
                DISPATCH();
            }
            SCM args[2] = {a, b};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_EVENP: {
            SCM a = stk[sp_idx - 1];
            if (GCPP_LIKELY(a.is_fixnum() && v.fast_arith_ok)) {
                stk[sp_idx - 1] = SCM::from_bool((a.to_fixnum() & 1) == 0);
                DISPATCH();
            }
            stk[sp_idx - 1] = slow_global_call(dec_b(insn), &a, 1);
            RELOAD_STK();
            DISPATCH();
        }

        L_ODDP: {
            SCM a = stk[sp_idx - 1];
            if (GCPP_LIKELY(a.is_fixnum() && v.fast_arith_ok)) {
                stk[sp_idx - 1] = SCM::from_bool((a.to_fixnum() & 1) != 0);
                DISPATCH();
            }
            stk[sp_idx - 1] = slow_global_call(dec_b(insn), &a, 1);
            RELOAD_STK();
            DISPATCH();
        }

        // ================================================================
        // Fast Inlined Core Operations
        // ================================================================
        L_EQ: {
            SCM b = stk[sp_idx - 1], a = stk[sp_idx - 2];
            if (GCPP_LIKELY(v.fast_core_ok)) {
                bool eq = (a == b) || (is_keyword(a) && is_keyword(b) && as_obj<KeywordObj>(a)->symbol == as_obj<KeywordObj>(b)->symbol);
                stk[sp_idx - 2] = SCM::from_bool(eq);
                --sp_idx;
                DISPATCH();
            }
            SCM args[2] = {a, b};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_NULLP:
            if (GCPP_LIKELY(v.fast_core_ok)) {
                stk[sp_idx - 1] = SCM::from_bool(stk[sp_idx - 1].is_null());
                DISPATCH();
            }
            stk[sp_idx - 1] = slow_global_call(dec_b(insn), &stk[sp_idx - 1], 1);
            RELOAD_STK();
            DISPATCH();

        L_PAIRP:
            if (GCPP_LIKELY(v.fast_core_ok)) {
                stk[sp_idx - 1] = SCM::from_bool(is_pair(stk[sp_idx - 1]));
                DISPATCH();
            }
            stk[sp_idx - 1] = slow_global_call(dec_b(insn), &stk[sp_idx - 1], 1);
            RELOAD_STK();
            DISPATCH();

        L_NOT:
            if (GCPP_LIKELY(v.fast_core_ok)) {
                stk[sp_idx - 1] = SCM::from_bool(stk[sp_idx - 1].is_false());
                DISPATCH();
            }
            stk[sp_idx - 1] = slow_global_call(dec_b(insn), &stk[sp_idx - 1], 1);
            RELOAD_STK();
            DISPATCH();

        L_CAR: {
            SCM x = stk[sp_idx - 1];
            if (GCPP_LIKELY(is_pair(x) && v.fast_core_ok)) {
                stk[sp_idx - 1] = car(x);
                DISPATCH();
            }
            stk[sp_idx - 1] = slow_global_call(dec_b(insn), &x, 1);
            RELOAD_STK();
            DISPATCH();
        }

        L_CDR: {
            SCM x = stk[sp_idx - 1];
            if (GCPP_LIKELY(is_pair(x) && v.fast_core_ok)) {
                stk[sp_idx - 1] = cdr(x);
                DISPATCH();
            }
            stk[sp_idx - 1] = slow_global_call(dec_b(insn), &x, 1);
            RELOAD_STK();
            DISPATCH();
        }

        L_CONS: {
            if (GCPP_LIKELY(v.fast_core_ok)) {
                sync_gc();
                SCM b = stk[sp_idx - 1], a = stk[sp_idx - 2];
                stk[sp_idx - 2] = v.heap().alloc_scm<PairObj>(a, b);
                --sp_idx;
                DISPATCH();
            }
            SCM args[2] = {stk[sp_idx - 2], stk[sp_idx - 1]};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_REVERSE: {
            if (GCPP_LIKELY(v.fast_core_ok)) {
                sync_gc();
                stk[sp_idx - 1] = scm_reverse(stk[sp_idx - 1]);
                DISPATCH();
            }
            stk[sp_idx - 1] = slow_global_call(dec_b(insn), &stk[sp_idx - 1], 1);
            RELOAD_STK();
            DISPATCH();
        }

        L_VEC_REF: {
            SCM idx = stk[sp_idx - 1], vec = stk[sp_idx - 2];
            if (GCPP_LIKELY(is_vector(vec) && idx.is_fixnum() && v.fast_core_ok)) {
                int64_t k = idx.to_fixnum();
                auto* vo = as_obj<VectorObj>(vec);
                if (GCPP_LIKELY(k >= 0 && static_cast<size_t>(k) < vo->elts.size())) {
                    stk[sp_idx - 2] = vo->elts[static_cast<size_t>(k)];
                    --sp_idx;
                    DISPATCH();
                }
            }
            SCM args[2] = {vec, idx};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_VEC_SET: {
            SCM val = stk[sp_idx - 1], idx = stk[sp_idx - 2], vec = stk[sp_idx - 3];
            if (GCPP_LIKELY(is_vector(vec) && idx.is_fixnum() && v.fast_core_ok)) {
                int64_t k = idx.to_fixnum();
                auto* vo = as_obj<VectorObj>(vec);
                if (GCPP_LIKELY(k >= 0 && static_cast<size_t>(k) < vo->elts.size())) {
                    vo->elts[static_cast<size_t>(k)] = val;
                    stk[sp_idx - 3] = SCM_UNSPECIFIED;
                    sp_idx -= 2;
                    DISPATCH();
                }
            }
            SCM args[3] = {vec, idx, val};
            stk[sp_idx - 3] = slow_global_call(dec_b(insn), args, 3);
            RELOAD_STK();
            sp_idx -= 2;
            DISPATCH();
        }

        L_MAKE_VEC: {
            uint8_t na = dec_a(insn);
            SCM k_val = stk[sp_idx - na];
            SCM fill = (na == 2) ? stk[sp_idx - 1] : SCM_UNSPECIFIED;
            if (GCPP_LIKELY(k_val.is_fixnum() && v.fast_core_ok)) {
                int64_t k = k_val.to_fixnum();
                if (GCPP_LIKELY(k >= 0)) {
                    sync_gc();
                    stk[sp_idx - na] = v.make_vector(static_cast<size_t>(k), fill);
                    RELOAD_STK();
                    sp_idx -= (na - 1);
                    DISPATCH();
                }
            }
            SCM args[2] = {k_val, fill};
            stk[sp_idx - na] = slow_global_call(dec_b(insn), args, na);
            RELOAD_STK();
            sp_idx -= (na - 1);
            DISPATCH();
        }

        L_STRUCT_REF: {
            SCM idx = stk[sp_idx - 1], st = stk[sp_idx - 2];
            if (GCPP_LIKELY(is_struct(st) && idx.is_fixnum() && v.fast_core_ok)) {
                int64_t k = idx.to_fixnum();
                auto* so = as_obj<StructObj>(st);
                if (GCPP_LIKELY(k >= 0 && static_cast<size_t>(k) < so->slots.size() && !so->has_tail)) {
                    // Check layout slot type is 'p' or 's' (fast check via common GOOPS slots)
                    stk[sp_idx - 2] = struct_ref(st, static_cast<size_t>(k));
                    --sp_idx;
                    DISPATCH();
                }
            }
            SCM args[2] = {st, idx};
            stk[sp_idx - 2] = slow_global_call(dec_b(insn), args, 2);
            RELOAD_STK();
            --sp_idx;
            DISPATCH();
        }

        L_STRUCT_SET: {
            SCM val = stk[sp_idx - 1], idx = stk[sp_idx - 2], st = stk[sp_idx - 3];
            if (GCPP_LIKELY(is_struct(st) && idx.is_fixnum() && v.fast_core_ok)) {
                int64_t k = idx.to_fixnum();
                if (GCPP_LIKELY(k >= 0 && static_cast<size_t>(k) < as_obj<StructObj>(st)->slots.size())) {
                    sync_gc();
                    struct_set(st, static_cast<size_t>(k), val);
                    stk[sp_idx - 3] = val;
                    sp_idx -= 2;
                    DISPATCH();
                }
            }
            SCM args[3] = {st, idx, val};
            stk[sp_idx - 3] = slow_global_call(dec_b(insn), args, 3);
            RELOAD_STK();
            sp_idx -= 2;
            DISPATCH();
        }

        L_HASHV_REF: {
            uint8_t na = dec_a(insn);
            SCM defv = (na == 3) ? stk[sp_idx - 1] : SCM_BOOL_F;
            SCM key = stk[sp_idx - (na == 3 ? 2 : 1)];
            SCM ht = stk[sp_idx - na];
            if (GCPP_LIKELY(is_hashtable(ht) && v.fast_core_ok)) {
                stk[sp_idx - na] = fast_hash_lookup(as_obj<HashTableObj>(ht), key, false, defv);
                sp_idx -= (na - 1);
                DISPATCH();
            }
            SCM args[3] = {ht, key, defv};
            stk[sp_idx - na] = slow_global_call(dec_b(insn), args, na);
            RELOAD_STK();
            sp_idx -= (na - 1);
            DISPATCH();
        }

        L_HASHV_SET: {
            SCM val = stk[sp_idx - 1], key = stk[sp_idx - 2], ht = stk[sp_idx - 3];
            if (GCPP_LIKELY(is_hashtable(ht) && v.fast_core_ok)) {
                sync_gc();
                fast_hash_store(ht, as_obj<HashTableObj>(ht), key, val, false);
                stk[sp_idx - 3] = val;
                sp_idx -= 2;
                DISPATCH();
            }
            SCM args[3] = {ht, key, val};
            stk[sp_idx - 3] = slow_global_call(dec_b(insn), args, 3);
            RELOAD_STK();
            sp_idx -= 2;
            DISPATCH();
        }

        L_HASHQ_REF: {
            uint8_t na = dec_a(insn);
            SCM defv = (na == 3) ? stk[sp_idx - 1] : SCM_BOOL_F;
            SCM key = stk[sp_idx - (na == 3 ? 2 : 1)];
            SCM ht = stk[sp_idx - na];
            if (GCPP_LIKELY(is_hashtable(ht) && v.fast_core_ok)) {
                stk[sp_idx - na] = fast_hash_lookup(as_obj<HashTableObj>(ht), key, true, defv);
                sp_idx -= (na - 1);
                DISPATCH();
            }
            SCM args[3] = {ht, key, defv};
            stk[sp_idx - na] = slow_global_call(dec_b(insn), args, na);
            RELOAD_STK();
            sp_idx -= (na - 1);
            DISPATCH();
        }

        L_HASHQ_SET: {
            SCM val = stk[sp_idx - 1], key = stk[sp_idx - 2], ht = stk[sp_idx - 3];
            if (GCPP_LIKELY(is_hashtable(ht) && v.fast_core_ok)) {
                sync_gc();
                fast_hash_store(ht, as_obj<HashTableObj>(ht), key, val, true);
                stk[sp_idx - 3] = val;
                sp_idx -= 2;
                DISPATCH();
            }
            SCM args[3] = {ht, key, val};
            stk[sp_idx - 3] = slow_global_call(dec_b(insn), args, 3);
            RELOAD_STK();
            sp_idx -= 2;
            DISPATCH();
        }

        L_CALLCC: {
            bool is_tail = dec_a(insn) != 0;
            SCM proc = stk[sp_idx - 1];
            if (!v.fast_callcc_ok) {
                SCM res = slow_global_call(dec_b(insn), &proc, 1);
                RELOAD_STK();
                stk[sp_idx - 1] = res;
                if (is_tail) goto L_RETURN;
                DISPATCH();
            }
            sync_gc();
            auto* k = v.heap().alloc<ContinuationObj>(v.current_cont, v.current_dynwind, v.trampoline_depth);
            k->vm_depth = my_vm_depth;
            k->has_vm_snap = true;
            // Snapshot stack below `proc` (caller expects 1 value pushed onto stack at sp_idx - 1!)
            size_t snap_sp = sp_idx - 1;
            k->set_stack_snap(stk + entry_sp, snap_sp - entry_sp);
            // Snapshot active frames plus current frame!
            size_t n_frames = fp_idx - entry_fp;
            k->reserve_frames_snap(n_frames + 1);
            for (size_t i = 0; i < n_frames; ++i) {
                const auto& fr = eng.frames[entry_fp + i];
                auto* fbc = as_obj<BytecodeObj>(fr.bytecode);
                k->push_frame_snap({
                    fr.closure, fr.bytecode,
                    static_cast<uint32_t>(fr.pc - fbc->code.data()),
                    static_cast<uint32_t>(fr.base_off - entry_sp),
                    static_cast<uint32_t>(fr.ret_slot - entry_sp)
                });
            }
            // Current frame resumes right after OP_CALLCC (or at OP_RETURN if tail!)
            uint32_t cur_pc_off = static_cast<uint32_t>(pc - bc->code.data());
            k->push_frame_snap({
                SCM::from_obj(cl), SCM::from_obj(bc),
                cur_pc_off,
                static_cast<uint32_t>(base_off - entry_sp),
                0
            });

            // Now call (proc k)!
            SCM k_scm = SCM::from_obj(k);
            stk[sp_idx++] = k_scm;
            if (GCPP_LIKELY(is_closure(proc))) {
                auto* tcl = as_obj<ClosureObj>(proc);
                if (tcl->bytecode.is_false()) { sync_gc(); bytecode_compile_closure(tcl); }
                if (GCPP_LIKELY(is_bytecode(tcl->bytecode))) {
                    auto* tbc = as_obj<BytecodeObj>(tcl->bytecode);
                    if (GCPP_LIKELY(!tbc->has_rest && tbc->arity_req == 1)) {
                        size_t fn_slot = sp_idx - 2;
                        eng.ensure_frames(fp_idx + 2);
                        eng.frames[fp_idx++] = {SCM::from_obj(cl), SCM::from_obj(bc), pc, base_off, static_cast<uint32_t>(fn_slot)};
                        uint32_t new_base = static_cast<uint32_t>(fn_slot + 1);
                        eng.ensure_stack(new_base + tbc->max_locals + 64);
                        RELOAD_STK();
                        stk[new_base] = k_scm;
                        for (size_t i = 1; i < tbc->max_locals; ++i) stk[new_base + i] = SCM_UNSPECIFIED;
                        load_frame_regs(tcl, tbc);
                        base_off = new_base;
                        sp_idx = new_base + tbc->max_locals;
                        pc = tbc->code.data();
                        DISPATCH();
                    }
                }
            }
            sync_gc();
            SCM res = v.apply(proc, &k_scm, 1);
            RELOAD_STK();
            stk[sp_idx - 2] = res;
            sp_idx -= 1;
            DISPATCH();
        }

        // ================================================================
        // Fused Loop Super-Instructions!
        // ================================================================
        L_LOCAL_ADD_IMM: {
            uint8_t sl = dec_a(insn);
            int16_t imm = dec_s16(insn);
            uint32_t aux = *pc++;
            SCM a = stk[base_off + sl];
            if (GCPP_LIKELY(a.is_fixnum() && v.fast_arith_ok)) {
                int64_t r;
                if (GCPP_LIKELY(!__builtin_add_overflow(a.to_fixnum(), static_cast<int64_t>(imm), &r) && SCM::fits_fixnum(r))) {
                    stk[sp_idx++] = SCM::from_fixnum(r);
                    DISPATCH();
                }
            }
            uint16_t gi = static_cast<uint16_t>(aux >> 16);
            bool is_sub = (aux & 1u) != 0;
            SCM args[2] = {a, SCM::from_fixnum(is_sub ? -static_cast<int64_t>(imm) : static_cast<int64_t>(imm))};
            stk[sp_idx++] = slow_global_call(gi, args, 2);
            RELOAD_STK();
            DISPATCH();
        }

        L_LOCAL_LT_IMM_JF: {
            uint8_t sl = dec_a(insn);
            int16_t imm = dec_s16(insn);
            uint32_t aux = *pc++;
            int16_t delta = static_cast<int16_t>(aux & 0xFFFFu);
            SCM a = stk[base_off + sl];
            if (GCPP_LIKELY(a.is_fixnum() && v.fast_arith_ok)) {
                if (!(a.to_fixnum() < static_cast<int64_t>(imm))) pc += delta;
                DISPATCH();
            }
            uint16_t gi = static_cast<uint16_t>(aux >> 16);
            SCM args[2] = {a, SCM::from_fixnum(imm)};
            SCM res = slow_global_call(gi, args, 2);
            RELOAD_STK();
            if (res.is_false()) pc += delta;
            DISPATCH();
        }

        L_LOCAL_EQ_IMM_JF: {
            uint8_t sl = dec_a(insn);
            int16_t imm = dec_s16(insn);
            uint32_t aux = *pc++;
            int16_t delta = static_cast<int16_t>(aux & 0xFFFFu);
            SCM a = stk[base_off + sl];
            if (GCPP_LIKELY(a.is_fixnum() && v.fast_arith_ok)) {
                if (a.to_fixnum() != static_cast<int64_t>(imm)) pc += delta;
                DISPATCH();
            }
            uint16_t gi = static_cast<uint16_t>(aux >> 16);
            SCM args[2] = {a, SCM::from_fixnum(imm)};
            SCM res = slow_global_call(gi, args, 2);
            RELOAD_STK();
            if (res.is_false()) pc += delta;
            DISPATCH();
        }

        L_CLEAR_LOCALS: {
            uint8_t start = dec_a(insn);
            uint16_t cnt = dec_b(insn);
            for (uint16_t i = 0; i < cnt; ++i) stk[base_off + start + i] = SCM_UNSPECIFIED;
            DISPATCH();
        }

        L_DYNWIND_PUSH: {
            sync_gc();
            SCM dw = v.heap().alloc_scm<DynWindObj>(DynWindKind::Wind, stk[base_off + 0], stk[base_off + 2],
                                                    SCM_BOOL_F, SCM_BOOL_F, v.current_dynwind, v.next_dw_id++);
            v.current_dynwind = dw;
            stk[base_off + 3] = dw;
            DISPATCH();
        }

        L_DYNWIND_POP: {
            v.current_dynwind = as_obj<DynWindObj>(stk[base_off + 3])->parent;
            DISPATCH();
        }

        } catch (const ContinuationEscape& esc) {
            if (esc.target_vm_depth == my_vm_depth && is_continuation(esc.target_k_obj)) {
                auto* k = as_obj<ContinuationObj>(esc.target_k_obj);
                sync_gc();
                switch_dynwind_pub(k->dynwind_chain);
                size_t sn = k->snap_stack_size();
                size_t fn = k->snap_frames_size();
                if (fn == 0) {
                    return esc.return_val;
                }
                eng.ensure_stack(entry_sp + sn + 64);
                eng.ensure_frames(entry_fp + fn + 16);
                RELOAD_STK();
                std::memcpy(stk + entry_sp, k->snap_stack_data(), sn * sizeof(SCM));
                const VmFrameSnap* fd = k->snap_frames_data();
                for (size_t i = 0; i < fn; ++i) {
                    const auto& sf = fd[i];
                    auto* fbc = as_obj<BytecodeObj>(sf.bytecode);
                    eng.frames[entry_fp + i] = {
                        sf.closure, sf.bytecode, fbc->code.data() + sf.pc_off,
                        static_cast<uint32_t>(entry_sp + sf.base_off),
                        static_cast<uint32_t>(entry_sp + sf.ret_slot)
                    };
                }
                fp_idx = entry_fp + fn - 1;
                const auto& topf = eng.frames[fp_idx];
                load_frame_regs(as_obj<ClosureObj>(topf.closure), as_obj<BytecodeObj>(topf.bytecode));
                pc = topf.pc;
                base_off = topf.base_off;
                sp_idx = entry_sp + sn;
                stk[sp_idx++] = esc.return_val;
                continue;
            }
            throw;
        }
    }

    #undef DISPATCH
    #undef RELOAD_STK
}

} // namespace

// ============================================================================
// Public Entry Points
// ============================================================================

static bool expr_has_named_let(SCM expr, int depth) noexcept {
    if (depth <= 0 || !is_pair(expr)) return false;
    SCM head = car(expr);
    auto& v = vm();
    if (head == v.sym_let) {
        SCM rest = cdr(expr);
        if (is_pair(rest) && is_symbol(car(rest)) && is_pair(cdr(rest)) && is_pair(cdr(cdr(rest)))) {
            // Only eagerly compile 1-shot thunks for true tail-loop named lets (body ends in if/cond)
            SCM body = cdr(cdr(rest));
            while (is_pair(body) && is_pair(cdr(body))) body = cdr(body);
            if (is_pair(body) && is_pair(car(body)) && (car(car(body)) == v.sym_if || car(car(body)) == v.sym_cond)) {
                return true;
            }
            return false;
        }
        if (is_pair(rest)) {
            SCM body = cdr(rest);
            while (is_pair(body)) {
                if (expr_has_named_let(car(body), depth - 1)) return true;
                body = cdr(body);
            }
        }
        return false;
    }
    if (head == v.sym_let_star || head == v.sym_letrec || head == v.sym_begin || head == v.sym_if) {
        SCM cur = (head == v.sym_let_star || head == v.sym_letrec) && is_pair(cdr(expr)) ? cdr(cdr(expr)) : cdr(expr);
        int budget = 8;
        while (is_pair(cur) && budget-- > 0) {
            if (expr_has_named_let(car(cur), depth - 1)) return true;
            cur = cdr(cur);
        }
    }
    return false;
}

bool bytecode_should_compile(ClosureObj* cl) noexcept {
    if (cl->extra >= 1) return true;
    ++cl->extra;
    // Compile 1-shot thunks immediately if they contain a named-let loop (e.g. benchmark thunks!)
    SCM b = cl->body;
    int budget = 6;
    while (is_pair(b) && budget-- > 0) {
        if (expr_has_named_let(car(b), 4)) return true;
        b = cdr(b);
    }
    return false;
}

// Pre-compilation expander for MemoizingMacro (defmacro / define-macro / procedure->memoizing-macro).
// Expands memoizing macros across a closure body BEFORE prescan & bytecode emission so that
// macro-heavy code (like TeXmacs's with / when / for / and-with / receive / ahash-with) compiles
// cleanly to fast bytecode + ARM64 JIT instead of falling back to the CEK interpreter.
struct MemoMacroExpander {
    SCM eval_clo;
    SCM macro_env;
    std::vector<SCM> bound; // only SymbolObjs (always rooted in intern table)
    int budget = 512;
    bool ok = true;

    bool is_bound(SCM sym) const noexcept {
        for (SCM s : bound) if (s == sym) return true;
        return false;
    }

    void push_formals(SCM formals) {
        SCM f = formals;
        while (is_pair(f)) {
            if (is_symbol(car(f))) bound.push_back(car(f));
            f = cdr(f);
        }
        if (is_symbol(f)) bound.push_back(f);
    }

    SCM expand_list(SCM lst, int depth) {
        if (!is_pair(lst) || !ok) return lst;
        // Walk and only allocate a new spine if any element actually changes
        SCM head = SCM_EOL;
        SCM tail = SCM_EOL;
        bool changed = false;
        SCM cur = lst;
        while (is_pair(cur)) {
            SCM orig = car(cur);
            SCM ne = expand_expr(orig, depth);
            if (!ok) return lst;
            if (!changed && ne != orig) {
                changed = true;
                // Copy prefix up to cur
                SCM p = lst;
                while (p != cur) {
                    SCM cell = vm().cons(car(p), SCM_EOL);
                    if (head.is_null()) head = tail = cell;
                    else { set_cdr(tail, cell); tail = cell; }
                    p = cdr(p);
                }
            }
            if (changed) {
                SCM cell = vm().cons(ne, SCM_EOL);
                if (head.is_null()) head = tail = cell;
                else { set_cdr(tail, cell); tail = cell; }
            }
            cur = cdr(cur);
        }
        if (!changed) return lst;
        if (!cur.is_null()) set_cdr(tail, cur);
        return head;
    }

    SCM expand_qq(SCM x, int qq_depth, int depth) {
        if (!is_pair(x) || !ok) return x;
        if (depth > 64) { ok = false; return x; }
        auto& v = vm();
        SCM a = car(x);
        SCM d = cdr(x);
        if (a == v.sym_quasiquote) {
            SCM nd = expand_qq(d, qq_depth + 1, depth + 1);
            return (nd == d) ? x : v.cons(a, nd);
        }
        if (a == v.sym_unquote || a == v.sym_unquote_splicing) {
            if (qq_depth == 1) {
                if (is_pair(d) && cdr(d).is_null()) {
                    SCM ne = expand_expr(car(d), depth + 1);
                    return (ne == car(d)) ? x : v.list2(a, ne);
                }
                return x;
            }
            SCM nd = expand_qq(d, qq_depth - 1, depth + 1);
            return (nd == d) ? x : v.cons(a, nd);
        }
        SCM na = expand_qq(a, qq_depth, depth + 1);
        SCM nd = expand_qq(d, qq_depth, depth + 1);
        return (na == a && nd == d) ? x : v.cons(na, nd);
    }

    SCM expand_expr(SCM expr, int depth) {
        if (!ok || !is_pair(expr)) return expr;
        if (depth > 48 || budget <= 0) { ok = false; return expr; }

        SCM op = car(expr);
        SCM args = cdr(expr);
        SCM mac_val = SCM_BOOL_F;
        if (is_macro(op)) {
            mac_val = op;
        } else if (is_symbol(op) && !is_bound(op)) {
            SCM var = vm().sym2var(op, eval_clo, false);
            if (is_variable(var) && is_macro(as_obj<VariableObj>(var)->value)) {
                mac_val = as_obj<VariableObj>(var)->value;
            }
        }

        if (is_macro(mac_val)) {
            auto* mac = as_obj<MacroObj>(mac_val);
            if (mac->kind == MacroKind::MemoizingMacro) {
                --budget;
                SCM expanded = SCM_BOOL_F;
                try {
                    expanded = vm().call2(mac->transformer, expr, macro_env);
                } catch (const SchemeThrow&) {
                    ok = false;
                    return expr;
                }
                if (expanded == expr) { ok = false; return expr; }
                return expand_expr(expanded, depth + 1);
            }
            if (mac->kind != MacroKind::BuiltinMacro) {
                // Non-memoizing Macro or Syntax -> cannot expand ahead of time
                ok = false;
                return expr;
            }
            BSyn bs = static_cast<BSyn>(mac->extra);
            switch (bs) {
            case BSyn::Quote:
            case BSyn::At:
            case BSyn::AtAt:
            case BSyn::TheEnvironment:
                return expr;
            case BSyn::Quasiquote: {
                if (!is_pair(args) || !cdr(args).is_null()) return expr;
                SCM nq = expand_qq(car(args), 1, depth + 1);
                return (nq == car(args)) ? expr : vm().list2(op, nq);
            }
            case BSyn::Lambda: {
                if (!is_pair(args) || !is_pair(cdr(args))) return expr;
                SCM formals = car(args);
                SCM body = cdr(args);
                size_t saved = bound.size();
                push_formals(formals);
                SCM nbody = expand_list(body, depth + 1);
                bound.resize(saved);
                return (nbody == body) ? expr : vm().cons(op, vm().cons(formals, nbody));
            }
            case BSyn::Let: {
                if (!is_pair(args) || !is_pair(cdr(args))) return expr;
                SCM first = car(args);
                SCM rest = cdr(args);
                SCM loop_sym = SCM_BOOL_F;
                SCM bindings = first;
                SCM body = rest;
                if (is_symbol(first) && is_pair(rest) && is_pair(cdr(rest))) {
                    loop_sym = first;
                    bindings = car(rest);
                    body = cdr(rest);
                }
                // Expand all binding init expressions in the OUTER scope
                SCM nb_head = SCM_EOL, nb_tail = SCM_EOL;
                bool b_changed = false;
                size_t saved = bound.size();
                std::vector<SCM> new_vars;
                SCM b = bindings;
                while (is_pair(b)) {
                    SCM e = car(b);
                    SCM ne = e;
                    if (is_pair(e) && is_symbol(car(e)) && is_pair(cdr(e)) && cdr(cdr(e)).is_null()) {
                        new_vars.push_back(car(e));
                        SCM nv = expand_expr(car(cdr(e)), depth + 1);
                        if (!ok) return expr;
                        if (nv != car(cdr(e))) ne = vm().list2(car(e), nv);
                    }
                    if (!b_changed && ne != e) {
                        b_changed = true;
                        SCM p = bindings;
                        while (p != b) {
                            SCM cell = vm().cons(car(p), SCM_EOL);
                            if (nb_head.is_null()) nb_head = nb_tail = cell;
                            else { set_cdr(nb_tail, cell); nb_tail = cell; }
                            p = cdr(p);
                        }
                    }
                    if (b_changed) {
                        SCM cell = vm().cons(ne, SCM_EOL);
                        if (nb_head.is_null()) nb_head = nb_tail = cell;
                        else { set_cdr(nb_tail, cell); nb_tail = cell; }
                    }
                    b = cdr(b);
                }
                SCM nbindings = b_changed ? nb_head : bindings;
                if (is_symbol(loop_sym)) bound.push_back(loop_sym);
                for (SCM s : new_vars) bound.push_back(s);
                SCM nbody = expand_list(body, depth + 1);
                bound.resize(saved);
                if (!b_changed && nbody == body) return expr;
                if (is_symbol(loop_sym)) return vm().cons(op, vm().cons(loop_sym, vm().cons(nbindings, nbody)));
                return vm().cons(op, vm().cons(nbindings, nbody));
            }
            case BSyn::LetStar: {
                if (!is_pair(args) || !is_pair(cdr(args))) return expr;
                SCM bindings = car(args);
                SCM body = cdr(args);
                SCM nb_head = SCM_EOL, nb_tail = SCM_EOL;
                bool b_changed = false;
                size_t saved = bound.size();
                SCM b = bindings;
                while (is_pair(b)) {
                    SCM e = car(b);
                    SCM ne = e;
                    if (is_pair(e) && is_symbol(car(e)) && is_pair(cdr(e)) && cdr(cdr(e)).is_null()) {
                        SCM nv = expand_expr(car(cdr(e)), depth + 1);
                        if (!ok) { bound.resize(saved); return expr; }
                        if (nv != car(cdr(e))) ne = vm().list2(car(e), nv);
                        bound.push_back(car(e));
                    }
                    if (!b_changed && ne != e) {
                        b_changed = true;
                        SCM p = bindings;
                        while (p != b) {
                            SCM cell = vm().cons(car(p), SCM_EOL);
                            if (nb_head.is_null()) nb_head = nb_tail = cell;
                            else { set_cdr(nb_tail, cell); nb_tail = cell; }
                            p = cdr(p);
                        }
                    }
                    if (b_changed) {
                        SCM cell = vm().cons(ne, SCM_EOL);
                        if (nb_head.is_null()) nb_head = nb_tail = cell;
                        else { set_cdr(nb_tail, cell); nb_tail = cell; }
                    }
                    b = cdr(b);
                }
                SCM nbindings = b_changed ? nb_head : bindings;
                SCM nbody = expand_list(body, depth + 1);
                bound.resize(saved);
                if (!b_changed && nbody == body) return expr;
                return vm().cons(op, vm().cons(nbindings, nbody));
            }
            case BSyn::Letrec: {
                if (!is_pair(args) || !is_pair(cdr(args))) return expr;
                SCM bindings = car(args);
                SCM body = cdr(args);
                size_t saved = bound.size();
                SCM b = bindings;
                while (is_pair(b)) {
                    if (is_pair(car(b)) && is_symbol(car(car(b)))) bound.push_back(car(car(b)));
                    b = cdr(b);
                }
                SCM nb_head = SCM_EOL, nb_tail = SCM_EOL;
                bool b_changed = false;
                b = bindings;
                while (is_pair(b)) {
                    SCM e = car(b);
                    SCM ne = e;
                    if (is_pair(e) && is_symbol(car(e)) && is_pair(cdr(e)) && cdr(cdr(e)).is_null()) {
                        SCM nv = expand_expr(car(cdr(e)), depth + 1);
                        if (!ok) { bound.resize(saved); return expr; }
                        if (nv != car(cdr(e))) ne = vm().list2(car(e), nv);
                    }
                    if (!b_changed && ne != e) {
                        b_changed = true;
                        SCM p = bindings;
                        while (p != b) {
                            SCM cell = vm().cons(car(p), SCM_EOL);
                            if (nb_head.is_null()) nb_head = nb_tail = cell;
                            else { set_cdr(nb_tail, cell); nb_tail = cell; }
                            p = cdr(p);
                        }
                    }
                    if (b_changed) {
                        SCM cell = vm().cons(ne, SCM_EOL);
                        if (nb_head.is_null()) nb_head = nb_tail = cell;
                        else { set_cdr(nb_tail, cell); nb_tail = cell; }
                    }
                    b = cdr(b);
                }
                SCM nbindings = b_changed ? nb_head : bindings;
                SCM nbody = expand_list(body, depth + 1);
                bound.resize(saved);
                if (!b_changed && nbody == body) return expr;
                return vm().cons(op, vm().cons(nbindings, nbody));
            }
            case BSyn::Cond: {
                SCM head = SCM_EOL, tail = SCM_EOL;
                bool changed = false;
                SCM cur = args;
                while (is_pair(cur)) {
                    SCM cl = car(cur);
                    SCM ncl = cl;
                    if (is_pair(cl)) {
                        SCM t = car(cl);
                        SCM cb = cdr(cl);
                        SCM nt = (t == vm().sym_else && !is_bound(t)) ? t : expand_expr(t, depth + 1);
                        if (!ok) return expr;
                        SCM ncb = cb;
                        if (is_pair(cb) && car(cb) == vm().sym_arrow && !is_bound(vm().sym_arrow) && is_pair(cdr(cb))) {
                            SCM nr = expand_list(cdr(cb), depth + 1);
                            if (nr != cdr(cb)) ncb = vm().cons(car(cb), nr);
                        } else {
                            ncb = expand_list(cb, depth + 1);
                        }
                        if (!ok) return expr;
                        if (nt != t || ncb != cb) ncl = vm().cons(nt, ncb);
                    }
                    if (!changed && ncl != cl) {
                        changed = true;
                        SCM p = args;
                        while (p != cur) {
                            SCM cell = vm().cons(car(p), SCM_EOL);
                            if (head.is_null()) head = tail = cell;
                            else { set_cdr(tail, cell); tail = cell; }
                            p = cdr(p);
                        }
                    }
                    if (changed) {
                        SCM cell = vm().cons(ncl, SCM_EOL);
                        if (head.is_null()) head = tail = cell;
                        else { set_cdr(tail, cell); tail = cell; }
                    }
                    cur = cdr(cur);
                }
                return changed ? vm().cons(op, head) : expr;
            }
            case BSyn::Case: {
                if (!is_pair(args) || !is_pair(cdr(args))) return expr;
                SCM key = car(args);
                SCM clauses = cdr(args);
                SCM nkey = expand_expr(key, depth + 1);
                if (!ok) return expr;
                SCM head = SCM_EOL, tail = SCM_EOL;
                bool changed = false;
                SCM cur = clauses;
                while (is_pair(cur)) {
                    SCM cl = car(cur);
                    SCM ncl = cl;
                    if (is_pair(cl)) {
                        SCM ncb = expand_list(cdr(cl), depth + 1);
                        if (!ok) return expr;
                        if (ncb != cdr(cl)) ncl = vm().cons(car(cl), ncb);
                    }
                    if (!changed && ncl != cl) {
                        changed = true;
                        SCM p = clauses;
                        while (p != cur) {
                            SCM cell = vm().cons(car(p), SCM_EOL);
                            if (head.is_null()) head = tail = cell;
                            else { set_cdr(tail, cell); tail = cell; }
                            p = cdr(p);
                        }
                    }
                    if (changed) {
                        SCM cell = vm().cons(ncl, SCM_EOL);
                        if (head.is_null()) head = tail = cell;
                        else { set_cdr(tail, cell); tail = cell; }
                    }
                    cur = cdr(cur);
                }
                if (!changed && nkey == key) return expr;
                return vm().cons(op, vm().cons(nkey, changed ? head : clauses));
            }
            case BSyn::Do: {
                if (!is_pair(args) || !is_pair(cdr(args))) return expr;
                SCM vars = car(args);
                SCM test_clause = car(cdr(args));
                SCM body = cdr(cdr(args));
                // First expand inits in outer scope, collect var names
                size_t saved = bound.size();
                std::vector<SCM> names;
                SCM vcur = vars;
                while (is_pair(vcur)) {
                    if (is_pair(car(vcur)) && is_symbol(car(car(vcur)))) names.push_back(car(car(vcur)));
                    vcur = cdr(vcur);
                }
                SCM nv_head = SCM_EOL, nv_tail = SCM_EOL;
                bool v_changed = false;
                vcur = vars;
                while (is_pair(vcur)) {
                    SCM sp = car(vcur);
                    SCM nsp = sp;
                    if (is_pair(sp) && is_symbol(car(sp)) && is_pair(cdr(sp))) {
                        SCM init_e = car(cdr(sp));
                        SCM step_rest = cdr(cdr(sp));
                        SCM ninit = expand_expr(init_e, depth + 1);
                        if (!ok) return expr;
                        SCM nstep_rest = step_rest;
                        if (is_pair(step_rest)) {
                            size_t s2 = bound.size();
                            for (SCM nm : names) bound.push_back(nm);
                            nstep_rest = expand_list(step_rest, depth + 1);
                            bound.resize(s2);
                            if (!ok) return expr;
                        }
                        if (ninit != init_e || nstep_rest != step_rest) {
                            nsp = vm().cons(car(sp), vm().cons(ninit, nstep_rest));
                        }
                    }
                    if (!v_changed && nsp != sp) {
                        v_changed = true;
                        SCM p = vars;
                        while (p != vcur) {
                            SCM cell = vm().cons(car(p), SCM_EOL);
                            if (nv_head.is_null()) nv_head = nv_tail = cell;
                            else { set_cdr(nv_tail, cell); nv_tail = cell; }
                            p = cdr(p);
                        }
                    }
                    if (v_changed) {
                        SCM cell = vm().cons(nsp, SCM_EOL);
                        if (nv_head.is_null()) nv_head = nv_tail = cell;
                        else { set_cdr(nv_tail, cell); nv_tail = cell; }
                    }
                    vcur = cdr(vcur);
                }
                for (SCM nm : names) bound.push_back(nm);
                SCM ntest = expand_list(test_clause, depth + 1);
                SCM nbody = expand_list(body, depth + 1);
                bound.resize(saved);
                if (!ok) return expr;
                if (!v_changed && ntest == test_clause && nbody == body) return expr;
                return vm().cons(op, vm().cons(v_changed ? nv_head : vars, vm().cons(ntest, nbody)));
            }
            case BSyn::SetX: {
                if (!is_pair(args) || !is_pair(cdr(args))) return expr;
                SCM target = car(args);
                SCM nrest = expand_list(cdr(args), depth + 1);
                if (!ok) return expr;
                return (nrest == cdr(args)) ? expr : vm().cons(op, vm().cons(target, nrest));
            }
            case BSyn::Define: {
                if (!is_pair(args)) return expr;
                SCM target = car(args);
                SCM dbody = cdr(args);
                if (is_pair(target)) {
                    size_t saved = bound.size();
                    SCM t = target;
                    while (is_pair(t)) {
                        push_formals(cdr(t));
                        t = car(t);
                    }
                    if (is_symbol(t)) bound.push_back(t);
                    SCM ndbody = expand_list(dbody, depth + 1);
                    bound.resize(saved);
                    if (!ok) return expr;
                    return (ndbody == dbody) ? expr : vm().cons(op, vm().cons(target, ndbody));
                }
                SCM ndbody = expand_list(dbody, depth + 1);
                if (!ok) return expr;
                return (ndbody == dbody) ? expr : vm().cons(op, vm().cons(target, ndbody));
            }
            default: {
                // If, Begin, And, Or, Delay: expand all sub-expressions
                SCM nargs = expand_list(args, depth + 1);
                return (nargs == args) ? expr : vm().cons(op, nargs);
            }
            }
        }

        // General procedure call: expand operator and arguments
        return expand_list(expr, depth + 1);
    }
};

bool bytecode_compile_closure(ClosureObj* cl) {
    if (is_bytecode(cl->bytecode)) return true;
    if (cl->bytecode == SCM_BOOL_T) return false;
    // Don't compile during very early pre-module boot or when debug backtraces are requested
    if (!vm().module_system_booted || vm().debug_opts.debug) return false;

    SCM eval_clo = top_level_eval_closure(cl->env);

    // Pre-expand any MemoizingMacro calls (with/when/for/and-with/receive/defmacro) BEFORE prescan!
    // Temporarily guard cl->bytecode against re-entrant compilation if a macro invokes cl.
    cl->bytecode = SCM_BOOL_T;
    SCM expanded_body = cl->body;
    {
        MemoMacroExpander exp;
        exp.eval_clo = eval_clo;
        exp.macro_env = cl->env;
        // Record formal parameters and outer lexical bindings so shadowed macros aren't expanded
        exp.push_formals(cl->formals);
        SCM ee = cl->env;
        while (is_pair(ee)) {
            SCM frame = car(ee);
            if (!is_pair(frame)) break;
            exp.push_formals(car(frame));
            ee = cdr(ee);
        }
        expanded_body = exp.expand_list(cl->body, 0);
        if (!exp.ok) {
            return false; // cl->bytecode stays SCM_BOOL_T -> CEK fallback
        }
    }

    // Extract any outer CEK lexical bindings from cl->env (after macro expansion, so GC-safe!)
    std::vector<OuterCekBinding> cek_outer;
    SCM e = cl->env;
    while (is_pair(e)) {
        SCM frame = car(e);
        if (!is_pair(frame)) break;
        SCM vars = car(frame);
        SCM vals = cdr(frame);
        SCM prev_val_pair = frame;
        bool is_first = true;
        while (is_pair(vars)) {
            if (is_symbol(car(vars)) && is_pair(vals)) {
                cek_outer.push_back({car(vars), vals, false});
            }
            vars = cdr(vars);
            prev_val_pair = vals;
            vals = is_pair(vals) ? cdr(vals) : SCM_EOL;
            is_first = false;
        }
        if (is_symbol(vars)) {
            cek_outer.push_back({vars, is_first ? frame : prev_val_pair, true});
        }
        e = cdr(e);
    }

    auto* bc = FnCompiler::compile_lambda_proto(cl->formals, expanded_body, nullptr, eval_clo, &cek_outer);
    if (!bc) {
        cl->bytecode = SCM_BOOL_T; // mark uncompilable -> CEK fallback
        return false;
    }

    cl->bytecode = SCM::from_obj(bc);
    cl->body = bc->source_body;
    if (bc->num_upvals > 0) {
        if (bc->num_upvals <= ClosureObj::INLINE_UPVAL_CAP) {
            cl->inline_upval_cnt = static_cast<uint8_t>(bc->num_upvals);
            for (size_t i = 0; i < bc->num_upvals; ++i) {
                uint32_t idx = bc->upval_descs[i] & 0xFFFFu;
                cl->inline_upvals[i] = cek_outer[idx].cell;
            }
        } else {
            auto* uv = vm().heap().alloc<VectorObj>(bc->num_upvals, SCM_BOOL_F);
            for (size_t i = 0; i < bc->num_upvals; ++i) {
                uint32_t idx = bc->upval_descs[i] & 0xFFFFu;
                uv->elts[i] = cek_outer[idx].cell;
            }
            cl->upvals = SCM::from_obj(uv);
        }
    }
    return true;
}

SCM bytecode_run_closure(ClosureObj* cl, const SCM* args, size_t nargs) {
#ifdef GUILE_CPP_JIT
    auto& v = vm();
    auto* bc = as_obj<BytecodeObj>(cl->bytecode);
    if (v.jit_enabled && !bc->has_rest && nargs == bc->arity_req && bc->num_upvals <= ClosureObj::INLINE_UPVAL_CAP &&
        v.fast_arith_ok && v.fast_core_ok && !v.debug_opts.debug) {
        if (bc->jit_code && bc->jit_epoch == v.jit_epoch) {
            uint64_t jr = jit_invoke(cl, bc, args, nargs);
            if (GCPP_LIKELY((jr & 1ULL) == 0)) return SCM(jr);
        } else if (bc->jit_calls != 0xFFFFFFFFu && (++bc->jit_calls >= 16 || bc->has_loop)) {
            if (jit_try_compile(cl, bc)) {
                uint64_t jr = jit_invoke(cl, bc, args, nargs);
                if (GCPP_LIKELY((jr & 1ULL) == 0)) return SCM(jr);
            }
        }
    }
#endif
    return run_bytecode_loop(cl, args, nargs);
}

SCM bytecode_resume_continuation(ContinuationObj* k, SCM ret_val) {
    return run_bytecode_loop(nullptr, nullptr, 0, k, ret_val);
}

void bytecode_mark_roots(const Marker& mark) {
    auto& eng = bc_engine();
    mark(eng.foreach1_helper);
    mark(eng.map1_helper);
    mark(eng.dynwind_helper);
    for (size_t i = 0; i < eng.active_sp; ++i) {
        mark(eng.stack[i]);
    }
    for (size_t i = 0; i < eng.active_fp; ++i) {
        mark(eng.frames[i].closure);
        mark(eng.frames[i].bytecode);
    }
}

} // namespace guile
