#include "guile_cpp/vm.hpp"
#include <cmath>

namespace guile {

static bool real_eqv_bits(double a, double b) {
    if (std::isnan(a) && std::isnan(b)) return true;
    return std::memcmp(&a, &b, sizeof(double)) == 0;
}

static bool scm_eqv_builtin(SCM a, SCM b) {
    if (a == b) return true;
    if (a.is_imp() || b.is_imp()) return false;

    ObjTag ta = a.to_obj()->tag;
    ObjTag tb = b.to_obj()->tag;

    // Keywords with same symbol are eqv? in Guile 1.8.8!
    if (ta == ObjTag::Keyword && tb == ObjTag::Keyword) {
        return as_obj<KeywordObj>(a)->symbol == as_obj<KeywordObj>(b)->symbol;
    }

    // Numbers: same exactness and equal value
    if (is_number(a) && is_number(b)) {
        if (is_exact(a) != is_exact(b)) return false;
        if (ta == ObjTag::Real && tb == ObjTag::Real) {
            return real_eqv_bits(as_obj<RealObj>(a)->val, as_obj<RealObj>(b)->val);
        }
        if (ta == ObjTag::Real && tb == ObjTag::Complex) {
            auto* cb = as_obj<ComplexObj>(b);
            return real_eqv_bits(as_obj<RealObj>(a)->val, cb->real) && cb->imag == 0.0;
        }
        if (ta == ObjTag::Complex && tb == ObjTag::Real) {
            auto* ca = as_obj<ComplexObj>(a);
            return real_eqv_bits(ca->real, as_obj<RealObj>(b)->val) && ca->imag == 0.0;
        }
        if (ta == ObjTag::Complex && tb == ObjTag::Complex) {
            auto* ca = as_obj<ComplexObj>(a);
            auto* cb = as_obj<ComplexObj>(b);
            return real_eqv_bits(ca->real, cb->real) && real_eqv_bits(ca->imag, cb->imag);
        }
        if (ta == ObjTag::Bignum && tb == ObjTag::Bignum) {
            return BigInt::cmp(as_obj<BignumObj>(a)->val, as_obj<BignumObj>(b)->val) == 0;
        }
        if (ta == ObjTag::Fraction && tb == ObjTag::Fraction) {
            auto* fa = as_obj<FractionObj>(a);
            auto* fb = as_obj<FractionObj>(b);
            return scm_eqv_builtin(fa->num, fb->num) && scm_eqv_builtin(fa->den, fb->den);
        }
        return false;
    }

    return false;
}

bool scm_eqv(SCM a, SCM b) {
    if (scm_eqv_builtin(a, b)) return true;
    auto& v = vm();
    if (is_subr(v.subr_eqv_p)) {
        SCM gf = as_obj<SubrObj>(v.subr_eqv_p)->generic;
        if (gf.is_true() && (is_goops_instance(a) || is_goops_instance(b))) {
            SCM args[2] = {a, b};
            return goops_apply_generic(gf, args, 2).is_true();
        }
    }
    return false;
}

static bool equal_rec(SCM a, SCM b, int depth) {
    if (a == b) return true;
    if (a.is_imp() || b.is_imp()) return false;
    if (depth > 10000) return true; // Cycle guard

    ObjTag ta = a.to_obj()->tag;
    ObjTag tb = b.to_obj()->tag;

    if (is_number(a) && is_number(b)) {
        if (is_exact(a) != is_exact(b)) return false;
        if (ta == ObjTag::Real && tb == ObjTag::Real) {
            return as_obj<RealObj>(a)->val == as_obj<RealObj>(b)->val;
        }
        if (ta == ObjTag::Real && tb == ObjTag::Complex) {
            auto* cb = as_obj<ComplexObj>(b);
            return as_obj<RealObj>(a)->val == cb->real && cb->imag == 0.0;
        }
        if (ta == ObjTag::Complex && tb == ObjTag::Real) {
            auto* ca = as_obj<ComplexObj>(a);
            return ca->real == as_obj<RealObj>(b)->val && ca->imag == 0.0;
        }
        if (ta == ObjTag::Complex && tb == ObjTag::Complex) {
            auto* ca = as_obj<ComplexObj>(a);
            auto* cb = as_obj<ComplexObj>(b);
            return ca->real == cb->real && ca->imag == cb->imag;
        }
        return scm_eqv_builtin(a, b);
    }

    if (ta == ObjTag::Keyword && tb == ObjTag::Keyword) {
        return as_obj<KeywordObj>(a)->symbol == as_obj<KeywordObj>(b)->symbol;
    }

    if (ta == ObjTag::Pair && tb == ObjTag::Pair) {
        // Iterative cdr walk to avoid deep recursion on long lists
        while (is_pair(a) && is_pair(b)) {
            if (a == b) return true;
            if (!equal_rec(car(a), car(b), depth + 1)) return false;
            a = cdr(a);
            b = cdr(b);
        }
        return equal_rec(a, b, depth + 1);
    }

    if (ta == ObjTag::String && tb == ObjTag::String) {
        return as_obj<StringObj>(a)->view() == as_obj<StringObj>(b)->view();
    }

    if (ta == ObjTag::Vector && tb == ObjTag::Vector) {
        auto* va = as_obj<VectorObj>(a);
        auto* vb = as_obj<VectorObj>(b);
        if (va->elts.size() != vb->elts.size()) return false;
        for (size_t i = 0; i < va->elts.size(); ++i) {
            if (!equal_rec(va->elts[i], vb->elts[i], depth + 1)) return false;
        }
        return true;
    }

    if (ta == ObjTag::WeakVector && tb == ObjTag::WeakVector) {
        auto* va = as_obj<WeakVectorObj>(a);
        auto* vb = as_obj<WeakVectorObj>(b);
        if (va->elts.size() != vb->elts.size()) return false;
        for (size_t i = 0; i < va->elts.size(); ++i) {
            if (!equal_rec(va->elts[i], vb->elts[i], depth + 1)) return false;
        }
        return true;
    }

    if (ta == ObjTag::CharSet && tb == ObjTag::CharSet) {
        auto* ca = as_obj<CharSetObj>(a);
        auto* cb = as_obj<CharSetObj>(b);
        return std::memcmp(ca->bits, cb->bits, sizeof(ca->bits)) == 0;
    }

    if (ta == ObjTag::Values && tb == ObjTag::Values) {
        return equal_rec(as_obj<ValuesObj>(a)->values, as_obj<ValuesObj>(b)->values, depth + 1);
    }

    if (ta == ObjTag::Struct && tb == ObjTag::Struct && !is_goops_instance(a) && !is_goops_instance(b)) {
        return struct_equal(a, b);
    }

    if (is_any_array(a) && is_any_array(b)) {
        return array_equal(a, b);
    }

    if (ta == ObjTag::Smob && tb == ObjTag::Smob) {
        auto* sa = as_obj<SmobObj>(a);
        auto* sb = as_obj<SmobObj>(b);
        if (sa->smob_type == sb->smob_type) {
            auto& info = vm().smob_info(sa->smob_type);
            if (info.equalp) return info.equalp(a, b).is_true();
        }
    }

    // GOOPS generic equal? dispatch
    auto& v = vm();
    if (is_subr(v.subr_equal_p)) {
        SCM gf = as_obj<SubrObj>(v.subr_equal_p)->generic;
        if (gf.is_true() && (is_goops_instance(a) || is_goops_instance(b))) {
            SCM args[2] = {a, b};
            return goops_apply_generic(gf, args, 2).is_true();
        }
    }

    return false;
}

bool scm_equal(SCM a, SCM b) {
    return equal_rec(a, b, 0);
}

void VM::init_eq() {
    auto eq_pair = [](SCM a, SCM b) -> bool {
        if (a == b) return true;
        if (is_keyword(a) && is_keyword(b)) {
            return as_obj<KeywordObj>(a)->symbol == as_obj<KeywordObj>(b)->symbol;
        }
        return false;
    };
    SCM p_eq = define_proc("eq?", 0, 0, true, [eq_pair](const SCM* args, size_t nargs) -> SCM {
        if (nargs < 2) return SCM_BOOL_T;
        if (nargs == 2) return SCM::from_bool(eq_pair(args[0], args[1]));
        for (size_t i = 1; i < nargs; ++i) {
            if (!eq_pair(args[i - 1], args[i])) return SCM_BOOL_F;
        }
        return SCM_BOOL_T;
    });
    as_obj<SubrObj>(p_eq)->raw_varargs = true;

    subr_eqv_p = define_proc("eqv?", 0, 0, true, [](const SCM* args, size_t nargs) -> SCM {
        if (nargs < 2) return SCM_BOOL_T;
        if (nargs == 2) return SCM::from_bool(scm_eqv(args[0], args[1]));
        for (size_t i = 1; i < nargs; ++i) {
            if (!scm_eqv(args[i - 1], args[i])) return SCM_BOOL_F;
        }
        return SCM_BOOL_T;
    });
    as_obj<SubrObj>(subr_eqv_p)->generic_capable = true;
    as_obj<SubrObj>(subr_eqv_p)->raw_varargs = true;

    subr_equal_p = define_proc("equal?", 0, 0, true, [](const SCM* args, size_t nargs) -> SCM {
        if (nargs < 2) return SCM_BOOL_T;
        if (nargs == 2) return SCM::from_bool(scm_equal(args[0], args[1]));
        for (size_t i = 1; i < nargs; ++i) {
            if (!scm_equal(args[i - 1], args[i])) return SCM_BOOL_F;
        }
        return SCM_BOOL_T;
    });
    as_obj<SubrObj>(subr_equal_p)->generic_capable = true;
    as_obj<SubrObj>(subr_equal_p)->raw_varargs = true;
    define_proc("not", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return SCM::from_bool(args[0].is_false());
    });
    define_proc("boolean?", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return SCM::from_bool(args[0].is_bool());
    });
}

} // namespace guile
