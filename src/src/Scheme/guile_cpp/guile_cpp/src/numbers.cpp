#include "guile_cpp/vm.hpp"
#include <cmath>
#include <complex>
#include <limits>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <cctype>

namespace guile {

// ============================================================================
// Integer / Bignum / Fraction / Real / Complex Constructors & Conversions
// ============================================================================

SCM VM::make_integer(int64_t v) {
    if (SCM::fits_fixnum(v)) return SCM::from_fixnum(v);
    return heap_.alloc_scm<BignumObj>(v);
}

SCM VM::make_uinteger(uint64_t v) {
    if (v <= static_cast<uint64_t>(SCM::FIXNUM_MAX)) {
        return SCM::from_fixnum(static_cast<int64_t>(v));
    }
    return heap_.alloc_scm<BignumObj>(v);
}

SCM scm_from_bigint(BigInt&& z) {
    if (z.fits_i64()) {
        int64_t v = z.to_i64();
        if (SCM::fits_fixnum(v)) return SCM::from_fixnum(v);
    }
    return vm().heap().alloc_scm<BignumObj>(std::move(z));
}

SCM scm_from_bigint(const BigInt& z) {
    if (z.fits_i64()) {
        int64_t v = z.to_i64();
        if (SCM::fits_fixnum(v)) return SCM::from_fixnum(v);
    }
    return vm().heap().alloc_scm<BignumObj>(z);
}

SCM VM::make_bignum(BigInt v) {
    return scm_from_bigint(std::move(v));
}

SCM VM::make_real(double v) {
    return heap_.alloc_scm<RealObj>(v);
}

SCM VM::make_complex(double r, double i) {
    if (i == 0.0 && !std::isnan(i)) {
        return make_real(r);
    }
    return heap_.alloc_scm<ComplexObj>(r, i);
}

SCM VM::make_fraction(SCM num, SCM den) {
    // Fast fixnum/fixnum path
    if (num.is_fixnum() && den.is_fixnum()) {
        int64_t n = num.to_fixnum();
        int64_t d = den.to_fixnum();
        if (d == 0) num_overflow("/");
        if (n == 0) return SCM::from_fixnum(0);
        if (d < 0) { n = -n; d = -d; } // note: FIXNUM_MIN is -2^61, -FIXNUM_MIN fits in int64_t!
        uint64_t un = (n >= 0) ? static_cast<uint64_t>(n) : static_cast<uint64_t>(-n);
        uint64_t ud = static_cast<uint64_t>(d);
        // Binary GCD on uint64_t
        unsigned sa = static_cast<unsigned>(gcpp_countr_zero(un));
        unsigned sb = static_cast<unsigned>(gcpp_countr_zero(ud));
        unsigned sh = std::min(sa, sb);
        uint64_t a = un >> sa, b = ud >> sb;
        while (a != b) {
            if (a > b) { a -= b; a >>= gcpp_countr_zero(a); }
            else       { b -= a; b >>= gcpp_countr_zero(b); }
        }
        uint64_t g = a << sh;
        if (g > 1) {
            n /= static_cast<int64_t>(g);
            d /= static_cast<int64_t>(g);
        }
        if (d == 1) return make_integer(n);
        return heap_.alloc_scm<FractionObj>(make_integer(n), make_integer(d));
    }

    uint64_t sn_buf = 0, sd_buf = 0;
    BigIntView nv = scm_to_bigint_view(num, sn_buf, "/", 1);
    BigIntView dv = scm_to_bigint_view(den, sd_buf, "/", 2);
    if (dv.is_zero()) num_overflow("/");
    if (nv.is_zero()) return SCM::from_fixnum(0);
    BigInt n(nv), d(dv);
    if (d.is_neg()) {
        n.negate();
        d.abs_inplace();
    }
    BigInt g = BigInt::gcd(n, d);
    if (g.size() > 1 || g.low_u64() > 1) {
        n = BigInt::divexact(n, g);
        d = BigInt::divexact(d, g);
    }
    if (d.size() == 1 && d.data()[0] == 1) {
        return scm_from_bigint(std::move(n));
    }
    SCM sn = scm_from_bigint(std::move(n));
    SCM sd = scm_from_bigint(std::move(d));
    return heap_.alloc_scm<FractionObj>(sn, sd);
}

bool is_exact(SCM x) {
    return x.is_fixnum() || is_bignum(x) || is_fraction(x);
}

bool is_inexact(SCM x) {
    return is_real_obj(x) || is_complex_obj(x);
}

bool is_real_number(SCM x) {
    if (x.is_fixnum() || is_bignum(x) || is_fraction(x) || is_real_obj(x)) return true;
    if (is_complex_obj(x)) return as_obj<ComplexObj>(x)->imag == 0.0;
    return false;
}

bool is_rational_number(SCM x) {
    if (x.is_fixnum() || is_bignum(x) || is_fraction(x)) return true;
    if (is_real_obj(x)) return std::isfinite(as_obj<RealObj>(x)->val);
    if (is_complex_obj(x)) {
        auto* c = as_obj<ComplexObj>(x);
        return c->imag == 0.0 && std::isfinite(c->real);
    }
    return false;
}

bool is_integer_number(SCM x) {
    if (x.is_fixnum() || is_bignum(x)) return true;
    if (is_real_obj(x)) {
        double d = as_obj<RealObj>(x)->val;
        if (std::isnan(d)) return false;
        if (std::isinf(d)) return true; // Guile 1.8: (integer? +inf.0) => #t
        return std::trunc(d) == d;
    }
    if (is_complex_obj(x)) {
        auto* c = as_obj<ComplexObj>(x);
        if (c->imag != 0.0 || std::isnan(c->real)) return false;
        if (std::isinf(c->real)) return true;
        return std::trunc(c->real) == c->real;
    }
    return false;
}

BigInt scm_to_bigint(SCM x, const char* subr, int argpos) {
    if (x.is_fixnum()) return BigInt(x.to_fixnum());
    if (is_bignum(x)) return as_obj<BignumObj>(x)->val;
    vm().wrong_type_arg(subr, argpos, x, "exact integer");
}

BigIntView scm_to_bigint_view(SCM x, uint64_t& scratch_limb, const char* subr, int argpos) {
    if (x.is_fixnum()) {
        int64_t v = x.to_fixnum();
        if (v == 0) return {&scratch_limb, 0, false};
        if (v > 0) { scratch_limb = static_cast<uint64_t>(v); return {&scratch_limb, 1, false}; }
        scratch_limb = 0ULL - static_cast<uint64_t>(v);
        return {&scratch_limb, 1, true};
    }
    if (is_bignum(x)) return as_obj<BignumObj>(x)->val.view();
    vm().wrong_type_arg(subr, argpos, x, "exact integer");
}

int64_t scm_to_int64(SCM x, const char* subr, int argpos) {
    if (x.is_fixnum()) return x.to_fixnum();
    if (is_bignum(x)) {
        auto* b = as_obj<BignumObj>(x);
        if (b->val.fits_i64()) return b->val.to_i64();
        vm().out_of_range(subr, x);
    }
    vm().wrong_type_arg(subr, argpos, x, "exact integer");
}

uint64_t scm_to_uint64(SCM x, const char* subr, int argpos) {
    if (x.is_fixnum()) {
        int64_t v = x.to_fixnum();
        if (v < 0) vm().out_of_range(subr, x);
        return static_cast<uint64_t>(v);
    }
    if (is_bignum(x)) {
        auto* b = as_obj<BignumObj>(x);
        if (!b->val.fits_u64()) vm().out_of_range(subr, x);
        return b->val.to_u64();
    }
    vm().wrong_type_arg(subr, argpos, x, "exact non-negative integer");
}

double scm_to_double(SCM x, const char* subr) {
    if (x.is_fixnum()) return static_cast<double>(x.to_fixnum());
    if (is_real_obj(x)) return as_obj<RealObj>(x)->val;
    if (is_bignum(x)) return as_obj<BignumObj>(x)->val.to_double_exact();
    if (is_fraction(x)) {
        auto* f = as_obj<FractionObj>(x);
        uint64_t sn = 0, sd = 0;
        BigIntView zn = scm_to_bigint_view(f->num, sn, subr, 1);
        BigIntView zd = scm_to_bigint_view(f->den, sd, subr, 1);
        return BigRat::to_double_exact(zn, zd);
    }
    if (is_complex_obj(x) && as_obj<ComplexObj>(x)->imag == 0.0) {
        return as_obj<ComplexObj>(x)->real;
    }
    vm().wrong_type_arg(subr, 1, x, "real");
}

static std::complex<double> scm_to_complex(SCM x, const char* subr) {
    if (is_complex_obj(x)) {
        auto* c = as_obj<ComplexObj>(x);
        return {c->real, c->imag};
    }
    return {scm_to_double(x, subr), 0.0};
}

struct RatViews {
    uint64_t sn = 0;
    uint64_t sd = 1;
    BigIntView num;
    BigIntView den;

    void init(SCM x, const char* subr) {
        if (x.is_fixnum()) {
            num = scm_to_bigint_view(x, sn, subr, 1);
            sd = 1;
            den = {&sd, 1, false};
        } else if (is_bignum(x)) {
            num = as_obj<BignumObj>(x)->val.view();
            sd = 1;
            den = {&sd, 1, false};
        } else if (is_fraction(x)) {
            auto* f = as_obj<FractionObj>(x);
            num = scm_to_bigint_view(f->num, sn, subr, 1);
            den = scm_to_bigint_view(f->den, sd, subr, 1);
        } else {
            vm().wrong_type_arg(subr, 1, x, "exact rational");
        }
    }
};

static BigRat scm_to_bigrat(SCM x, const char* subr) {
    if (x.is_fixnum()) {
        return BigRat(BigInt(x.to_fixnum()), BigInt(int64_t(1)));
    }
    if (is_bignum(x)) {
        return BigRat(as_obj<BignumObj>(x)->val, BigInt(int64_t(1)));
    }
    if (is_fraction(x)) {
        auto* f = as_obj<FractionObj>(x);
        return BigRat(scm_to_bigint(f->num, subr, 1), scm_to_bigint(f->den, subr, 1));
    }
    vm().wrong_type_arg(subr, 1, x, "exact rational");
}

// Convert already-canonical BigRat (den > 0, gcd(|num|, den) == 1) to SCM without redundant GCD
static SCM scm_from_bigrat_reduced(BigRat&& q) {
    if (q.num.is_zero()) return SCM::from_fixnum(0);
    if (q.den.size() == 1 && q.den.data()[0] == 1) {
        return scm_from_bigint(std::move(q.num));
    }
    SCM n = scm_from_bigint(std::move(q.num));
    SCM d = scm_from_bigint(std::move(q.den));
    return vm().heap().alloc_scm<FractionObj>(n, d);
}

SCM scm_exact_to_inexact(SCM n) {
    if (!is_number(n)) vm().wrong_type_arg("exact->inexact", 1, n, "number");
    if (is_inexact(n)) return n;
    return vm().make_real(scm_to_double(n, "exact->inexact"));
}

SCM scm_inexact_to_exact(SCM n) {
    if (!is_number(n)) vm().wrong_type_arg("inexact->exact", 1, n, "number");
    if (is_exact(n)) return n;
    if (is_complex_obj(n)) {
        auto* c = as_obj<ComplexObj>(n);
        if (c->imag != 0.0) vm().wrong_type_arg("inexact->exact", 1, n, "real");
        n = vm().make_real(c->real);
    }
    double d = as_obj<RealObj>(n)->val;
    if (!std::isfinite(d)) vm().out_of_range("inexact->exact", n);
    if (d == 0.0) return SCM::from_fixnum(0);
    return scm_from_bigrat_reduced(BigRat::from_double(d));
}

// ============================================================================
// Arithmetic Operations
// ============================================================================

SCM scm_num_add(SCM a, SCM b) {
    if (a.is_fixnum() && b.is_fixnum()) {
        int64_t r;
        if (!__builtin_add_overflow(a.to_fixnum(), b.to_fixnum(), &r) && SCM::fits_fixnum(r)) {
            return SCM::from_fixnum(r);
        }
    }
    if (!is_number(a)) vm().wrong_type_arg("+", 1, a, "number");
    if (!is_number(b)) vm().wrong_type_arg("+", 2, b, "number");

    if (is_complex_obj(a) || is_complex_obj(b)) {
        auto ca = scm_to_complex(a, "+");
        auto cb = scm_to_complex(b, "+");
        auto r = ca + cb;
        return vm().make_complex(r.real(), r.imag());
    }
    if (is_real_obj(a) || is_real_obj(b)) {
        return vm().make_real(scm_to_double(a, "+") + scm_to_double(b, "+"));
    }
    if (is_fraction(a) || is_fraction(b)) {
        RatViews ra, rb;
        ra.init(a, "+"); rb.init(b, "+");
        return scm_from_bigrat_reduced(BigRat::add_views(ra.num, ra.den, rb.num, rb.den));
    }
    uint64_t sa = 0, sb = 0;
    BigIntView za = scm_to_bigint_view(a, sa, "+", 1);
    BigIntView zb = scm_to_bigint_view(b, sb, "+", 2);
    return scm_from_bigint(BigInt::add(za, zb));
}

SCM scm_num_sub(SCM a, SCM b) {
    if (a.is_fixnum() && b.is_fixnum()) {
        int64_t r;
        if (!__builtin_sub_overflow(a.to_fixnum(), b.to_fixnum(), &r) && SCM::fits_fixnum(r)) {
            return SCM::from_fixnum(r);
        }
    }
    if (!is_number(a)) vm().wrong_type_arg("-", 1, a, "number");
    if (!is_number(b)) vm().wrong_type_arg("-", 2, b, "number");

    if (is_complex_obj(a) || is_complex_obj(b)) {
        auto ca = scm_to_complex(a, "-");
        auto cb = scm_to_complex(b, "-");
        auto r = ca - cb;
        return vm().make_complex(r.real(), r.imag());
    }
    if (is_real_obj(a) || is_real_obj(b)) {
        return vm().make_real(scm_to_double(a, "-") - scm_to_double(b, "-"));
    }
    if (is_fraction(a) || is_fraction(b)) {
        RatViews ra, rb;
        ra.init(a, "-"); rb.init(b, "-");
        return scm_from_bigrat_reduced(BigRat::sub_views(ra.num, ra.den, rb.num, rb.den));
    }
    uint64_t sa = 0, sb = 0;
    BigIntView za = scm_to_bigint_view(a, sa, "-", 1);
    BigIntView zb = scm_to_bigint_view(b, sb, "-", 2);
    return scm_from_bigint(BigInt::sub(za, zb));
}

static SCM scm_num_neg(SCM a) {
    if (a.is_fixnum()) {
        int64_t v = a.to_fixnum();
        if (v != SCM::FIXNUM_MIN) return SCM::from_fixnum(-v);
    }
    if (is_bignum(a)) return scm_from_bigint(BigInt::neg(as_obj<BignumObj>(a)->val));
    if (is_real_obj(a)) return vm().make_real(-as_obj<RealObj>(a)->val);
    if (is_complex_obj(a)) {
        auto* c = as_obj<ComplexObj>(a);
        return vm().make_complex(-c->real, -c->imag);
    }
    return scm_num_sub(SCM::from_fixnum(0), a);
}

SCM scm_num_mul(SCM a, SCM b) {
    if (a.is_fixnum() && b.is_fixnum()) {
        int64_t r;
        if (!__builtin_mul_overflow(a.to_fixnum(), b.to_fixnum(), &r) && SCM::fits_fixnum(r)) {
            return SCM::from_fixnum(r);
        }
    }
    if (!is_number(a)) vm().wrong_type_arg("*", 1, a, "number");
    if (!is_number(b)) vm().wrong_type_arg("*", 2, b, "number");

    if (is_complex_obj(a) || is_complex_obj(b)) {
        // Guile 1.8: exact 0 * complex => exact 0 (if finite)
        if ((a.is_fixnum() && a.to_fixnum() == 0) || (b.is_fixnum() && b.to_fixnum() == 0)) {
            auto other = (a.is_fixnum() && a.to_fixnum() == 0) ? scm_to_complex(b, "*") : scm_to_complex(a, "*");
            if (std::isfinite(other.real()) && std::isfinite(other.imag())) return SCM::from_fixnum(0);
        }
        auto ca = scm_to_complex(a, "*");
        auto cb = scm_to_complex(b, "*");
        auto r = ca * cb;
        return vm().make_complex(r.real(), r.imag());
    }
    if (is_real_obj(a) || is_real_obj(b)) {
        // Guile 1.8: exact 0 * finite flonum => exact 0
        if (a.is_fixnum() && a.to_fixnum() == 0 && std::isfinite(as_obj<RealObj>(b)->val)) return SCM::from_fixnum(0);
        if (b.is_fixnum() && b.to_fixnum() == 0 && std::isfinite(as_obj<RealObj>(a)->val)) return SCM::from_fixnum(0);
        return vm().make_real(scm_to_double(a, "*") * scm_to_double(b, "*"));
    }
    if (is_fraction(a) || is_fraction(b)) {
        RatViews ra, rb;
        ra.init(a, "*"); rb.init(b, "*");
        return scm_from_bigrat_reduced(BigRat::mul_views(ra.num, ra.den, rb.num, rb.den));
    }
    if (is_bignum(a) && b.is_fixnum()) {
        int64_t bv = b.to_fixnum();
        if (bv == 0) return SCM::from_fixnum(0);
        if (bv == 1) return a;
        return scm_from_bigint(BigInt::mul_si(as_obj<BignumObj>(a)->val, bv));
    }
    if (a.is_fixnum() && is_bignum(b)) {
        int64_t av = a.to_fixnum();
        if (av == 0) return SCM::from_fixnum(0);
        if (av == 1) return b;
        return scm_from_bigint(BigInt::mul_si(as_obj<BignumObj>(b)->val, av));
    }
    if (is_bignum(a) && is_bignum(b)) {
        return scm_from_bigint(BigInt::mul(as_obj<BignumObj>(a)->val, as_obj<BignumObj>(b)->val));
    }
    uint64_t sa = 0, sb = 0;
    BigIntView za = scm_to_bigint_view(a, sa, "*", 1);
    BigIntView zb = scm_to_bigint_view(b, sb, "*", 2);
    return scm_from_bigint(BigInt::mul(za, zb));
}

SCM scm_num_div(SCM a, SCM b) {
    if (!is_number(a)) vm().wrong_type_arg("/", 1, a, "number");
    if (!is_number(b)) vm().wrong_type_arg("/", 2, b, "number");

    if (is_complex_obj(a) || is_complex_obj(b)) {
        if (b.is_fixnum() && b.to_fixnum() == 0) vm().num_overflow("/");
        auto ca = scm_to_complex(a, "/");
        auto cb = scm_to_complex(b, "/");
        auto r = ca / cb;
        return vm().make_complex(r.real(), r.imag());
    }
    if (is_real_obj(a) || is_real_obj(b)) {
        if (b.is_fixnum() && b.to_fixnum() == 0) vm().num_overflow("/");
        double da = scm_to_double(a, "/");
        double db = scm_to_double(b, "/");
        return vm().make_real(da / db);
    }
    if (is_exact_integer(a) && is_exact_integer(b)) {
        return vm().make_fraction(a, b);
    }
    RatViews ra, rb;
    ra.init(a, "/"); rb.init(b, "/");
    if (rb.num.is_zero()) vm().num_overflow("/");
    return scm_from_bigrat_reduced(BigRat::div_views(ra.num, ra.den, rb.num, rb.den));
}

// ============================================================================
// Comparison
// ============================================================================

static int cmp_exact_vs_double(SCM ex, double d) {
    if (std::isnan(d)) return 2; // unordered
    if (std::isinf(d)) return d > 0 ? -1 : 1;
    RatViews ra;
    ra.init(ex, "<");
    BigRat qb = BigRat::from_double(d);
    return BigRat::cmp_views(ra.num, ra.den, qb.num, qb.den);
}

// Returns -1, 0, 1, or 2 if unordered (NaN)
int scm_num_cmp(SCM a, SCM b) {
    if (a.is_fixnum() && b.is_fixnum()) {
        int64_t va = a.to_fixnum(), vb = b.to_fixnum();
        return (va < vb) ? -1 : (va > vb ? 1 : 0);
    }
    if (!is_real_number(a)) vm().wrong_type_arg("<", 1, a, "real");
    if (!is_real_number(b)) vm().wrong_type_arg("<", 2, b, "real");

    if (is_exact(a) && is_exact(b)) {
        if (is_exact_integer(a) && is_exact_integer(b)) {
            uint64_t sa = 0, sb = 0;
            BigIntView za = scm_to_bigint_view(a, sa, "<", 1);
            BigIntView zb = scm_to_bigint_view(b, sb, "<", 2);
            return BigInt::cmp(za, zb);
        }
        RatViews ra, rb;
        ra.init(a, "<"); rb.init(b, "<");
        return BigRat::cmp_views(ra.num, ra.den, rb.num, rb.den);
    }
    if (is_exact(a) && is_inexact(b)) {
        return cmp_exact_vs_double(a, scm_to_double(b, "<"));
    }
    if (is_inexact(a) && is_exact(b)) {
        int c = cmp_exact_vs_double(b, scm_to_double(a, "<"));
        return (c == 2) ? 2 : -c;
    }
    double da = scm_to_double(a, "<");
    double db = scm_to_double(b, "<");
    if (std::isnan(da) || std::isnan(db)) return 2;
    return (da < db) ? -1 : (da > db ? 1 : 0);
}

bool scm_num_eq(SCM a, SCM b) {
    if (a == b) {
        if (is_real_obj(a) && std::isnan(as_obj<RealObj>(a)->val)) return false;
        if (is_complex_obj(a)) {
            auto* c = as_obj<ComplexObj>(a);
            if (std::isnan(c->real) || std::isnan(c->imag)) return false;
        }
        return is_number(a);
    }
    if (!is_number(a)) vm().wrong_type_arg("=", 1, a, "number");
    if (!is_number(b)) vm().wrong_type_arg("=", 2, b, "number");
    if (is_complex_obj(a) || is_complex_obj(b)) {
        auto ca = scm_to_complex(a, "=");
        auto cb = scm_to_complex(b, "=");
        if (ca.imag() != cb.imag()) return false;
        if (ca.imag() == 0.0 && cb.imag() == 0.0) {
            return scm_num_cmp(is_complex_obj(a) ? vm().make_real(ca.real()) : a,
                               is_complex_obj(b) ? vm().make_real(cb.real()) : b) == 0;
        }
        return ca.real() == cb.real();
    }
    return scm_num_cmp(a, b) == 0;
}

// ============================================================================
// Integer Division, GCD, LCM, Bitwise
// ============================================================================

static SCM extract_integer_as_exact(SCM x, const char* subr, int pos, bool& was_inexact) {
    if (is_exact_integer(x)) {
        was_inexact = false;
        return x;
    }
    if (is_real_obj(x)) {
        double d = as_obj<RealObj>(x)->val;
        if (std::isfinite(d) && std::trunc(d) == d) {
            was_inexact = true;
            return scm_inexact_to_exact(x);
        }
    }
    vm().wrong_type_arg(subr, pos, x, "integer");
}

SCM scm_quotient(SCM a, SCM b) {
    if (a.is_fixnum() && b.is_fixnum()) {
        int64_t vb = b.to_fixnum();
        if (vb == 0) vm().num_overflow("quotient");
        return vm().make_integer(a.to_fixnum() / vb);
    }
    bool ia = false, ib = false;
    SCM ea = extract_integer_as_exact(a, "quotient", 1, ia);
    SCM eb = extract_integer_as_exact(b, "quotient", 2, ib);
    uint64_t sa = 0, sb = 0;
    BigIntView za = scm_to_bigint_view(ea, sa, "quotient", 1);
    BigIntView zb = scm_to_bigint_view(eb, sb, "quotient", 2);
    if (zb.is_zero()) vm().num_overflow("quotient");
    SCM res = scm_from_bigint(BigInt::tdiv_q(za, zb));
    return (ia || ib) ? scm_exact_to_inexact(res) : res;
}

SCM scm_remainder(SCM a, SCM b) {
    if (a.is_fixnum() && b.is_fixnum()) {
        int64_t vb = b.to_fixnum();
        if (vb == 0) vm().num_overflow("remainder");
        return SCM::from_fixnum(a.to_fixnum() % vb);
    }
    if (is_bignum(a) && b.is_fixnum()) {
        int64_t vb = b.to_fixnum();
        if (vb == 0) vm().num_overflow("remainder");
        return vm().make_integer(BigInt::tdiv_r_si(as_obj<BignumObj>(a)->val, vb));
    }
    bool ia = false, ib = false;
    SCM ea = extract_integer_as_exact(a, "remainder", 1, ia);
    SCM eb = extract_integer_as_exact(b, "remainder", 2, ib);
    uint64_t sa = 0, sb = 0;
    BigIntView za = scm_to_bigint_view(ea, sa, "remainder", 1);
    BigIntView zb = scm_to_bigint_view(eb, sb, "remainder", 2);
    if (zb.is_zero()) vm().num_overflow("remainder");
    SCM res = scm_from_bigint(BigInt::tdiv_r(za, zb));
    return (ia || ib) ? scm_exact_to_inexact(res) : res;
}

SCM scm_modulo(SCM a, SCM b) {
    if (a.is_fixnum() && b.is_fixnum()) {
        int64_t va = a.to_fixnum(), vb = b.to_fixnum();
        if (vb == 0) vm().num_overflow("modulo");
        int64_t r = va % vb;
        if (r != 0 && ((va < 0) != (vb < 0))) r += vb;
        return SCM::from_fixnum(r);
    }
    if (is_bignum(a) && b.is_fixnum()) {
        int64_t vb = b.to_fixnum();
        if (vb == 0) vm().num_overflow("modulo");
        int64_t r = BigInt::tdiv_r_si(as_obj<BignumObj>(a)->val, vb);
        if (r != 0 && ((r < 0) != (vb < 0))) r += vb;
        return vm().make_integer(r);
    }
    bool ia = false, ib = false;
    SCM ea = extract_integer_as_exact(a, "modulo", 1, ia);
    SCM eb = extract_integer_as_exact(b, "modulo", 2, ib);
    uint64_t sa = 0, sb = 0;
    BigIntView za = scm_to_bigint_view(ea, sa, "modulo", 1);
    BigIntView zb = scm_to_bigint_view(eb, sb, "modulo", 2);
    if (zb.is_zero()) vm().num_overflow("modulo");
    SCM res = scm_from_bigint(BigInt::fdiv_r(za, zb));
    return (ia || ib) ? scm_exact_to_inexact(res) : res;
}

SCM scm_gcd(SCM a, SCM b) {
    if (a.is_fixnum() && b.is_fixnum()) {
        uint64_t ua = a.to_fixnum() >= 0 ? static_cast<uint64_t>(a.to_fixnum()) : static_cast<uint64_t>(-a.to_fixnum());
        uint64_t ub = b.to_fixnum() >= 0 ? static_cast<uint64_t>(b.to_fixnum()) : static_cast<uint64_t>(-b.to_fixnum());
        if (ua == 0) return vm().make_uinteger(ub);
        if (ub == 0) return vm().make_uinteger(ua);
        unsigned sa = static_cast<unsigned>(gcpp_countr_zero(ua));
        unsigned sb = static_cast<unsigned>(gcpp_countr_zero(ub));
        unsigned sh = std::min(sa, sb);
        ua >>= sa; ub >>= sb;
        while (ua != ub) {
            if (ua > ub) { ua -= ub; ua >>= gcpp_countr_zero(ua); }
            else         { ub -= ua; ub >>= gcpp_countr_zero(ub); }
        }
        return vm().make_uinteger(ua << sh);
    }
    bool ia = false, ib = false;
    SCM ea = extract_integer_as_exact(a, "gcd", 1, ia);
    SCM eb = extract_integer_as_exact(b, "gcd", 2, ib);
    uint64_t sa = 0, sb = 0;
    BigIntView za = scm_to_bigint_view(ea, sa, "gcd", 1);
    BigIntView zb = scm_to_bigint_view(eb, sb, "gcd", 2);
    SCM res = scm_from_bigint(BigInt::gcd(za, zb));
    return (ia || ib) ? scm_exact_to_inexact(res) : res;
}

SCM scm_lcm(SCM a, SCM b) {
    bool ia = false, ib = false;
    SCM ea = extract_integer_as_exact(a, "lcm", 1, ia);
    SCM eb = extract_integer_as_exact(b, "lcm", 2, ib);
    uint64_t sa = 0, sb = 0;
    BigIntView za = scm_to_bigint_view(ea, sa, "lcm", 1);
    BigIntView zb = scm_to_bigint_view(eb, sb, "lcm", 2);
    SCM res = scm_from_bigint(BigInt::lcm(za, zb));
    return (ia || ib) ? scm_exact_to_inexact(res) : res;
}

SCM scm_logand(SCM a, SCM b) {
    if (a.is_fixnum() && b.is_fixnum()) {
        return SCM::from_fixnum(a.to_fixnum() & b.to_fixnum());
    }
    uint64_t sa = 0, sb = 0;
    BigIntView za = scm_to_bigint_view(a, sa, "logand", 1);
    BigIntView zb = scm_to_bigint_view(b, sb, "logand", 2);
    return scm_from_bigint(BigInt::bit_and(za, zb));
}

SCM scm_logior(SCM a, SCM b) {
    if (a.is_fixnum() && b.is_fixnum()) {
        return SCM::from_fixnum(a.to_fixnum() | b.to_fixnum());
    }
    uint64_t sa = 0, sb = 0;
    BigIntView za = scm_to_bigint_view(a, sa, "logior", 1);
    BigIntView zb = scm_to_bigint_view(b, sb, "logior", 2);
    return scm_from_bigint(BigInt::bit_ior(za, zb));
}

SCM scm_logxor(SCM a, SCM b) {
    if (a.is_fixnum() && b.is_fixnum()) {
        return SCM::from_fixnum(a.to_fixnum() ^ b.to_fixnum());
    }
    uint64_t sa = 0, sb = 0;
    BigIntView za = scm_to_bigint_view(a, sa, "logxor", 1);
    BigIntView zb = scm_to_bigint_view(b, sb, "logxor", 2);
    return scm_from_bigint(BigInt::bit_xor(za, zb));
}

SCM scm_lognot(SCM a) {
    if (a.is_fixnum()) return SCM::from_fixnum(~a.to_fixnum());
    uint64_t sa = 0;
    BigIntView za = scm_to_bigint_view(a, sa, "lognot", 1);
    return scm_from_bigint(BigInt::bit_not(za));
}

SCM scm_ash(SCM n, SCM count) {
    int64_t cnt = scm_to_int64(count, "ash", 2);
    if (n.is_fixnum()) {
        int64_t vn = n.to_fixnum();
        if (cnt == 0 || vn == 0) return n;
        if (cnt < 0) {
            if (-cnt >= 63) return SCM::from_fixnum(vn < 0 ? -1 : 0);
            return SCM::from_fixnum(vn >> (-cnt));
        }
        if (cnt < 61) {
            int64_t r = vn * (int64_t(1) << cnt);
            // Check no overflow by shifting back
            if ((r >> cnt) == vn && SCM::fits_fixnum(r)) return SCM::from_fixnum(r);
        }
    }
    uint64_t sn = 0;
    BigIntView zn = scm_to_bigint_view(n, sn, "ash", 1);
    if (cnt >= 0) {
        if (cnt > 100000000LL) vm().num_overflow("ash");
        return scm_from_bigint(BigInt::mul_2exp(zn, static_cast<size_t>(cnt)));
    } else {
        uint64_t rsh = static_cast<uint64_t>(-cnt);
        return scm_from_bigint(BigInt::fdiv_q_2exp(zn, static_cast<size_t>(rsh)));
    }
}

// ============================================================================
// String <-> Number Parsing & Formatting (Full R5RS + Guile 1.8.8)
// ============================================================================

static SCM parse_ureal(std::string_view s, int radix, int exactness, bool negative) {
    if (s.empty()) return SCM_BOOL_F;

    // Special IEEE values (+inf.0, -inf.0, +nan.0, -nan.0)
    auto lower_eq = [](std::string_view a, const char* b) {
        if (a.size() != std::strlen(b)) return false;
        for (size_t i = 0; i < a.size(); ++i)
            if (std::tolower(static_cast<unsigned char>(a[i])) != b[i]) return false;
        return true;
    };
    if (lower_eq(s, "inf.0")) {
        if (exactness == 1) return SCM_BOOL_F;
        return vm().make_real(negative ? -std::numeric_limits<double>::infinity()
                                       :  std::numeric_limits<double>::infinity());
    }
    if (lower_eq(s, "nan.0")) {
        if (exactness == 1) return SCM_BOOL_F;
        return vm().make_real(std::numeric_limits<double>::quiet_NaN());
    }

    // Helper: parse unsigned integer with optional trailing '#' wildcards (using fast BigInt::from_string_unsigned)
    auto parse_uint_fast = [&](std::string_view p, BigInt& out, bool& local_hash) -> bool {
        if (p.empty()) return false;
        size_t hash_pos = p.find('#');
        if (hash_pos == std::string_view::npos) {
            return BigInt::from_string_unsigned(p, radix, out);
        }
        if (hash_pos == 0) return false;
        // All characters from hash_pos onward must be '#'
        for (size_t k = hash_pos; k < p.size(); ++k) {
            if (p[k] != '#') return false;
        }
        local_hash = true;
        std::string replaced(p);
        for (size_t k = hash_pos; k < replaced.size(); ++k) replaced[k] = '0';
        return BigInt::from_string_unsigned(replaced, radix, out);
    };

    // Check for fraction: uint / uint
    auto slash = s.find('/');
    if (slash != std::string_view::npos) {
        auto lhs = s.substr(0, slash);
        auto rhs = s.substr(slash + 1);
        if (lhs.empty() || rhs.empty()) return SCM_BOOL_F;
        bool has_hash = false;
        BigInt zn, zd;
        if (!parse_uint_fast(lhs, zn, has_hash) || !parse_uint_fast(rhs, zd, has_hash) || zd.is_zero()) {
            return SCM_BOOL_F;
        }
        if (negative) zn.negate();
        SCM res = vm().make_fraction(scm_from_bigint(std::move(zn)), scm_from_bigint(std::move(zd)));
        if (exactness == 2 || (exactness == 0 && has_hash)) {
            res = scm_exact_to_inexact(res);
        }
        return res;
    }

    // Check if decimal/exponent float (only in radix 10)
    bool is_dec = false;
    bool has_hash = false;
    if (radix == 10) {
        for (char c : s) {
            if (c == '.' || c == 'e' || c == 'E' || c == 's' || c == 'S' ||
                c == 'f' || c == 'F' || c == 'd' || c == 'D' || c == 'l' || c == 'L') {
                is_dec = true;
                break;
            }
        }
    }

    if (is_dec) {
        // Parse R5RS decimal: [digits][.digits][exp_marker[+-]digits]
        size_t i = 0;
        bool seen_digit = false;
        bool seen_dot = false;
        bool seen_hash = false;
        std::string norm;
        if (negative) norm.push_back('-');

        while (i < s.size()) {
            char c = s[i];
            if (c >= '0' && c <= '9') {
                if (seen_hash) return SCM_BOOL_F;
                seen_digit = true;
                norm.push_back(c);
                ++i;
            } else if (c == '#') {
                if (!seen_digit) return SCM_BOOL_F;
                seen_hash = true;
                has_hash = true;
                norm.push_back('0');
                ++i;
            } else if (c == '.') {
                if (seen_dot) return SCM_BOOL_F;
                seen_dot = true;
                norm.push_back('.');
                ++i;
            } else if (c == 'e' || c == 'E' || c == 's' || c == 'S' ||
                       c == 'f' || c == 'F' || c == 'd' || c == 'D' ||
                       c == 'l' || c == 'L') {
                if (!seen_digit) return SCM_BOOL_F;
                norm.push_back('e');
                ++i;
                if (i < s.size() && (s[i] == '+' || s[i] == '-')) {
                    norm.push_back(s[i++]);
                }
                bool exp_digits = false;
                while (i < s.size() && s[i] >= '0' && s[i] <= '9') {
                    exp_digits = true;
                    norm.push_back(s[i++]);
                }
                if (!exp_digits || i != s.size()) return SCM_BOOL_F;
                break;
            } else {
                return SCM_BOOL_F;
            }
        }
        if (!seen_digit || i != s.size()) return SCM_BOOL_F;

        if (exactness == 1) {
            // Exact decimal: parse exact rational m * 10^e!
            std::string_view nv = norm;
            bool neg2 = false;
            if (!nv.empty() && nv[0] == '-') { neg2 = true; nv.remove_prefix(1); }
            int64_t exp10 = 0;
            auto epos = nv.find('e');
            std::string_view mant_sv = (epos == std::string_view::npos) ? nv : nv.substr(0, epos);
            if (epos != std::string_view::npos) {
                try { exp10 = std::stoll(std::string(nv.substr(epos + 1))); } catch (...) { return SCM_BOOL_F; }
            }
            std::string digits_only;
            digits_only.reserve(mant_sv.size());
            bool after_dot = false;
            for (char c : mant_sv) {
                if (c == '.') { after_dot = true; continue; }
                digits_only.push_back(c);
                if (after_dot) --exp10;
            }
            BigInt num, den(int64_t(1));
            if (!BigInt::from_string_unsigned(digits_only, 10, num)) return SCM_BOOL_F;
            if (neg2) num.negate();
            if (exp10 > 0) {
                num = BigInt::mul(num, BigInt::ui_pow_ui(10, static_cast<uint64_t>(exp10)));
            } else if (exp10 < 0) {
                den = BigInt::ui_pow_ui(10, static_cast<uint64_t>(-exp10));
            }
            return vm().make_fraction(scm_from_bigint(std::move(num)), scm_from_bigint(std::move(den)));
        }
        errno = 0;
        char* endp = nullptr;
        double d = std::strtod(norm.c_str(), &endp);
        if (endp != norm.c_str() + norm.size()) return SCM_BOOL_F;
        if (errno == ERANGE && std::isinf(d)) {
            vm().out_of_range("string->number", vm().make_string(s));
        }
        return vm().make_real(d);
    }

    // Integer in given radix (uses O(n) power-of-2 or subquadratic D&C decimal parsing!)
    BigInt z;
    if (!parse_uint_fast(s, z, has_hash)) return SCM_BOOL_F;
    if (negative) z.negate();
    SCM res = scm_from_bigint(std::move(z));
    if (exactness == 2 || (exactness == 0 && has_hash)) {
        return scm_exact_to_inexact(res);
    }
    return res;
}

static SCM parse_real(std::string_view s, int radix, int exactness) {
    if (s.empty()) return SCM_BOOL_F;
    bool neg = false;
    bool has_sign = false;
    if (s[0] == '+') { has_sign = true; s.remove_prefix(1); }
    else if (s[0] == '-') { has_sign = true; neg = true; s.remove_prefix(1); }
    // inf.0 and nan.0 require explicit sign in R5RS/Guile!
    if (!has_sign && s.size() == 5 &&
        (std::tolower(static_cast<unsigned char>(s[0])) == 'i' ||
         std::tolower(static_cast<unsigned char>(s[0])) == 'n')) {
        return SCM_BOOL_F;
    }
    return parse_ureal(s, radix, exactness, neg);
}

SCM scm_string_to_number(std::string_view s, int radix) {
    if (s.empty()) return SCM_BOOL_F;
    int exactness = 0; // 0 = unspecified, 1 = exact (#e), 2 = inexact (#i)
    bool seen_radix = false;
    bool seen_exact = false;

    while (s.size() >= 2 && s[0] == '#') {
        char c = static_cast<char>(std::tolower(static_cast<unsigned char>(s[1])));
        s.remove_prefix(2);
        if (c == 'b') { if (seen_radix) return SCM_BOOL_F; seen_radix = true; radix = 2; }
        else if (c == 'o') { if (seen_radix) return SCM_BOOL_F; seen_radix = true; radix = 8; }
        else if (c == 'd') { if (seen_radix) return SCM_BOOL_F; seen_radix = true; radix = 10; }
        else if (c == 'x') { if (seen_radix) return SCM_BOOL_F; seen_radix = true; radix = 16; }
        else if (c == 'e') { if (seen_exact) return SCM_BOOL_F; seen_exact = true; exactness = 1; }
        else if (c == 'i') { if (seen_exact) return SCM_BOOL_F; seen_exact = true; exactness = 2; }
        else return SCM_BOOL_F;
    }
    if (s.empty()) return SCM_BOOL_F;

    // Polar complex: real@real
    auto at = s.find('@');
    if (at != std::string_view::npos) {
        SCM r = parse_real(s.substr(0, at), radix, exactness == 1 ? 0 : exactness);
        SCM th = parse_real(s.substr(at + 1), radix, exactness == 1 ? 0 : exactness);
        if (r.is_false() || th.is_false()) return SCM_BOOL_F;
        double mag = scm_to_double(r, "string->number");
        double ang = scm_to_double(th, "string->number");
        if (exactness == 1 && ang == 0.0) return scm_inexact_to_exact(vm().make_real(mag));
        return vm().make_complex(mag * std::cos(ang), mag * std::sin(ang));
    }

    // Rectangular complex ending in 'i' or 'I'
    if (s.back() == 'i' || s.back() == 'I') {
        std::string_view body = s.substr(0, s.size() - 1);
        if (body == "+") return vm().make_complex(0.0, 1.0);
        if (body == "-") return vm().make_complex(0.0, -1.0);
        // Find last '+' or '-' not preceded by exponent marker (only in radix 10)
        size_t split = std::string_view::npos;
        for (size_t i = body.size(); i > 0; --i) {
            char c = body[i - 1];
            if (c == '+' || c == '-') {
                if (i - 1 == 0) {
                    split = 0;
                    break;
                }
                char prev = body[i - 2];
                if (radix == 10 && (prev == 'e' || prev == 'E' || prev == 's' || prev == 'S' ||
                                    prev == 'f' || prev == 'F' || prev == 'd' || prev == 'D' ||
                                    prev == 'l' || prev == 'L')) {
                    continue;
                }
                split = i - 1;
                break;
            }
        }
        if (split == std::string_view::npos) return SCM_BOOL_F;
        SCM rp = SCM::from_fixnum(0);
        if (split > 0) {
            rp = parse_real(body.substr(0, split), radix, exactness == 1 ? 0 : exactness);
            if (rp.is_false()) return SCM_BOOL_F;
        }
        std::string_view imag_sv = body.substr(split);
        double iv = 0.0;
        if (imag_sv == "+") iv = 1.0;
        else if (imag_sv == "-") iv = -1.0;
        else {
            SCM ip = parse_real(imag_sv, radix, exactness == 1 ? 0 : exactness);
            if (ip.is_false()) return SCM_BOOL_F;
            iv = scm_to_double(ip, "string->number");
        }
        if (iv == 0.0) {
            if (exactness == 1) return scm_inexact_to_exact(rp);
            if (exactness == 2 || is_inexact(rp)) return scm_exact_to_inexact(rp);
            return vm().make_real(scm_to_double(rp, "string->number"));
        }
        if (exactness == 1) return SCM_BOOL_F; // Guile 1.8 has no exact non-real complex
        return vm().make_complex(scm_to_double(rp, "string->number"), iv);
    }

    return parse_real(s, radix, exactness);
}

static std::string double_to_string(double d) {
    if (std::isnan(d)) return "+nan.0";
    if (std::isinf(d)) return d > 0 ? "+inf.0" : "-inf.0";
    if (d == 0.0) return std::signbit(d) ? "-0.0" : "0.0";
    char buf[64];
    std::snprintf(buf, sizeof(buf), "%.15g", d);
    std::string s(buf);
    if (s.find('.') == std::string::npos && s.find('e') == std::string::npos && s.find('E') == std::string::npos) {
        s += ".0";
    }
    return s;
}

static std::string double_to_string_radix(double d, int radix) {
    if (radix == 10) return double_to_string(d);
    if (std::isnan(d)) return "+nan.0";
    if (std::isinf(d)) return d > 0 ? "+inf.0" : "-inf.0";
    bool neg = std::signbit(d);
    double ad = std::fabs(d);
    static const char* digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    BigRat q = BigRat::from_double(ad);
    BigInt ip, rem;
    BigInt::tdiv_qr(q.num, q.den, ip, rem);
    std::string res = neg ? "-" : "";
    res += BigInt::to_string(ip, -radix); // negative radix => uppercase
    res.push_back('.');
    if (rem.is_zero()) {
        res.push_back('0');
    } else {
        int max_frac = 54;
        for (int i = 0; i < max_frac && !rem.is_zero(); ++i) {
            rem = BigInt::mul_ui(rem, static_cast<uint64_t>(radix));
            BigInt dig;
            BigInt::tdiv_qr(rem, q.den, dig, rem);
            res.push_back(digits[dig.low_u64()]);
        }
    }
    return res;
}

std::string scm_number_to_string(SCM n, int radix) {
    if (n.is_fixnum()) {
        int64_t v = n.to_fixnum();
        if (radix == 10) return std::to_string(v);
        SmallIntView sv(v);
        return BigInt::to_string(sv, radix);
    }
    if (is_bignum(n)) {
        return BigInt::to_string(as_obj<BignumObj>(n)->val, radix);
    }
    if (is_fraction(n)) {
        auto* f = as_obj<FractionObj>(n);
        return scm_number_to_string(f->num, radix) + "/" + scm_number_to_string(f->den, radix);
    }
    if (is_real_obj(n)) {
        return double_to_string_radix(as_obj<RealObj>(n)->val, radix);
    }
    if (is_complex_obj(n)) {
        auto* c = as_obj<ComplexObj>(n);
        std::string rs = double_to_string_radix(c->real, radix);
        std::string is = double_to_string_radix(c->imag, radix);
        if (!is.empty() && is[0] != '+' && is[0] != '-') rs += '+';
        return rs + is + 'i';
    }
    vm().wrong_type_arg("number->string", 1, n, "number");
}

// ============================================================================
// Primitive Registration
// ============================================================================

void VM::init_numbers() {
    define("most-positive-fixnum", SCM::from_fixnum(SCM::FIXNUM_MAX));
    define("most-negative-fixnum", SCM::from_fixnum(SCM::FIXNUM_MIN));

    define_proc("number?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_number(a[0])); });
    define_proc("complex?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_number(a[0])); });
    define_proc("real?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_real_number(a[0])); });
    define_proc("rational?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_rational_number(a[0])); });
    define_proc("integer?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_integer_number(a[0])); });
    define_proc("exact?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_number(a[0])) wrong_type_arg("exact?", 1, a[0], "number");
        return SCM::from_bool(is_exact(a[0]));
    });
    define_proc("inexact?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_number(a[0])) wrong_type_arg("inexact?", 1, a[0], "number");
        return SCM::from_bool(is_inexact(a[0]));
    });
    define_proc("inf?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        if (is_real_obj(a[0])) return SCM::from_bool(std::isinf(as_obj<RealObj>(a[0])->val));
        if (is_complex_obj(a[0])) {
            auto* c = as_obj<ComplexObj>(a[0]);
            return SCM::from_bool(std::isinf(c->real) || std::isinf(c->imag));
        }
        return SCM_BOOL_F;
    });
    define_proc("nan?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        if (is_real_obj(a[0])) return SCM::from_bool(std::isnan(as_obj<RealObj>(a[0])->val));
        if (is_complex_obj(a[0])) {
            auto* c = as_obj<ComplexObj>(a[0]);
            return SCM::from_bool(std::isnan(c->real) || std::isnan(c->imag));
        }
        return SCM_BOOL_F;
    });
    define_proc("finite?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_number(a[0])) wrong_type_arg("finite?", 1, a[0], "number");
        if (is_real_obj(a[0])) return SCM::from_bool(std::isfinite(as_obj<RealObj>(a[0])->val));
        if (is_complex_obj(a[0])) {
            auto* c = as_obj<ComplexObj>(a[0]);
            return SCM::from_bool(std::isfinite(c->real) && std::isfinite(c->imag));
        }
        return SCM_BOOL_T;
    });

    define_proc("zero?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_number(a[0])) wrong_type_arg("zero?", 1, a[0], "number");
        return SCM::from_bool(scm_num_eq(a[0], SCM::from_fixnum(0)));
    });
    define_proc("positive?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(scm_num_cmp(a[0], SCM::from_fixnum(0)) == 1);
    });
    define_proc("negative?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(scm_num_cmp(a[0], SCM::from_fixnum(0)) == -1);
    });
    define_proc("odd?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        bool inx = false;
        SCM ex = extract_integer_as_exact(a[0], "odd?", 1, inx);
        if (ex.is_fixnum()) return SCM::from_bool((ex.to_fixnum() & 1) != 0);
        return SCM::from_bool(as_obj<BignumObj>(ex)->val.is_odd());
    });
    define_proc("even?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        bool inx = false;
        SCM ex = extract_integer_as_exact(a[0], "even?", 1, inx);
        if (ex.is_fixnum()) return SCM::from_bool((ex.to_fixnum() & 1) == 0);
        return SCM::from_bool(as_obj<BignumObj>(ex)->val.is_even());
    });

    // Variadic arithmetic (primitive-generic capable, raw_varargs = zero rest-list boxing!)
    subr_add = define_proc("+", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (nargs == 2 && a[0].is_fixnum() && a[1].is_fixnum()) {
            int64_t r;
            if (!__builtin_add_overflow(a[0].to_fixnum(), a[1].to_fixnum(), &r) && SCM::fits_fixnum(r)) {
                return SCM::from_fixnum(r);
            }
            return scm_num_add(a[0], a[1]);
        }
        if (nargs == 0) return SCM::from_fixnum(0);
        SCM acc = a[0];
        if (!is_number(acc)) {
            if (as_obj<SubrObj>(subr_add)->generic.is_true()) {
                if (nargs == 1) return goops_apply_generic(as_obj<SubrObj>(subr_add)->generic, &acc, 1);
                SCM args[2] = {acc, a[1]};
                acc = goops_apply_generic(as_obj<SubrObj>(subr_add)->generic, args, 2);
                for (size_t i = 2; i < nargs; ++i) {
                    SCM a2[2] = {acc, a[i]};
                    acc = goops_apply_generic(as_obj<SubrObj>(subr_add)->generic, a2, 2);
                }
                return acc;
            }
            wrong_type_arg("+", 1, acc, "number");
        }
        for (size_t i = 1; i < nargs; ++i) {
            SCM cur = a[i];
            if (!is_number(cur) && as_obj<SubrObj>(subr_add)->generic.is_true()) {
                SCM args[2] = {acc, cur};
                acc = goops_apply_generic(as_obj<SubrObj>(subr_add)->generic, args, 2);
            } else {
                acc = scm_num_add(acc, cur);
            }
        }
        return acc;
    });
    as_obj<SubrObj>(subr_add)->generic_capable = true;
    as_obj<SubrObj>(subr_add)->raw_varargs = true;

    subr_sub = define_proc("-", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (nargs == 2 && a[0].is_fixnum() && a[1].is_fixnum()) {
            int64_t r;
            if (!__builtin_sub_overflow(a[0].to_fixnum(), a[1].to_fixnum(), &r) && SCM::fits_fixnum(r)) {
                return SCM::from_fixnum(r);
            }
            return scm_num_sub(a[0], a[1]);
        }
        if (nargs == 1) {
            if (!is_number(a[0]) && as_obj<SubrObj>(subr_sub)->generic.is_true()) {
                return goops_apply_generic(as_obj<SubrObj>(subr_sub)->generic, a, 1);
            }
            return scm_num_neg(a[0]);
        }
        SCM acc = a[0];
        for (size_t i = 1; i < nargs; ++i) {
            SCM cur = a[i];
            if ((!is_number(acc) || !is_number(cur)) && as_obj<SubrObj>(subr_sub)->generic.is_true()) {
                SCM args[2] = {acc, cur};
                acc = goops_apply_generic(as_obj<SubrObj>(subr_sub)->generic, args, 2);
            } else {
                acc = scm_num_sub(acc, cur);
            }
        }
        return acc;
    });
    as_obj<SubrObj>(subr_sub)->generic_capable = true;
    as_obj<SubrObj>(subr_sub)->raw_varargs = true;

    subr_mul = define_proc("*", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (nargs == 2 && a[0].is_fixnum() && a[1].is_fixnum()) {
            int64_t r;
            if (!__builtin_mul_overflow(a[0].to_fixnum(), a[1].to_fixnum(), &r) && SCM::fits_fixnum(r)) {
                return SCM::from_fixnum(r);
            }
            return scm_num_mul(a[0], a[1]);
        }
        if (nargs == 0) return SCM::from_fixnum(1);
        SCM acc = a[0];
        if (!is_number(acc)) {
            if (as_obj<SubrObj>(subr_mul)->generic.is_true()) {
                if (nargs == 1) return goops_apply_generic(as_obj<SubrObj>(subr_mul)->generic, &acc, 1);
                SCM args[2] = {acc, a[1]};
                acc = goops_apply_generic(as_obj<SubrObj>(subr_mul)->generic, args, 2);
                for (size_t i = 2; i < nargs; ++i) {
                    SCM a2[2] = {acc, a[i]};
                    acc = goops_apply_generic(as_obj<SubrObj>(subr_mul)->generic, a2, 2);
                }
                return acc;
            }
            wrong_type_arg("*", 1, acc, "number");
        }
        for (size_t i = 1; i < nargs; ++i) {
            SCM cur = a[i];
            if (!is_number(cur) && as_obj<SubrObj>(subr_mul)->generic.is_true()) {
                SCM args[2] = {acc, cur};
                acc = goops_apply_generic(as_obj<SubrObj>(subr_mul)->generic, args, 2);
            } else {
                acc = scm_num_mul(acc, cur);
            }
        }
        return acc;
    });
    as_obj<SubrObj>(subr_mul)->generic_capable = true;
    as_obj<SubrObj>(subr_mul)->raw_varargs = true;

    subr_div = define_proc("/", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (nargs == 1) {
            if (!is_number(a[0]) && as_obj<SubrObj>(subr_div)->generic.is_true()) {
                return goops_apply_generic(as_obj<SubrObj>(subr_div)->generic, a, 1);
            }
            return scm_num_div(SCM::from_fixnum(1), a[0]);
        }
        SCM acc = a[0];
        for (size_t i = 1; i < nargs; ++i) {
            SCM cur = a[i];
            if ((!is_number(acc) || !is_number(cur)) && as_obj<SubrObj>(subr_div)->generic.is_true()) {
                SCM args[2] = {acc, cur};
                acc = goops_apply_generic(as_obj<SubrObj>(subr_div)->generic, args, 2);
            } else {
                acc = scm_num_div(acc, cur);
            }
        }
        return acc;
    });
    as_obj<SubrObj>(subr_div)->generic_capable = true;
    as_obj<SubrObj>(subr_div)->raw_varargs = true;

    define_proc("1+", 1, 0, false, [](const SCM* a, size_t) -> SCM { return scm_num_add(a[0], SCM::from_fixnum(1)); });
    define_proc("1-", 1, 0, false, [](const SCM* a, size_t) -> SCM { return scm_num_sub(a[0], SCM::from_fixnum(1)); });

    // Comparisons (primitive-generic capable, raw_varargs)
    subr_num_eq = define_proc("=", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (nargs == 2 && a[0].is_fixnum() && a[1].is_fixnum()) {
            return SCM::from_bool(a[0] == a[1]);
        }
        if (nargs == 0) return SCM_BOOL_T;
        SCM prev = a[0];
        if (nargs == 1) {
            if (!is_number(prev) && !as_obj<SubrObj>(subr_num_eq)->generic.is_true()) {
                wrong_type_arg("=", 1, prev, "number");
            }
            return SCM_BOOL_T;
        }
        for (size_t i = 1; i < nargs; ++i) {
            SCM cur = a[i];
            if ((!is_number(prev) || !is_number(cur)) && as_obj<SubrObj>(subr_num_eq)->generic.is_true()) {
                SCM args[2] = {prev, cur};
                if (goops_apply_generic(as_obj<SubrObj>(subr_num_eq)->generic, args, 2).is_false()) return SCM_BOOL_F;
            } else {
                if (!scm_num_eq(prev, cur)) return SCM_BOOL_F;
            }
            prev = cur;
        }
        return SCM_BOOL_T;
    });
    as_obj<SubrObj>(subr_num_eq)->generic_capable = true;
    as_obj<SubrObj>(subr_num_eq)->raw_varargs = true;

    auto reg_ord = [this](const char* name, SCM& slot, auto pred) {
        slot = define_proc(name, 0, 0, true, [this, name, &slot, pred](const SCM* a, size_t nargs) -> SCM {
            if (nargs == 2 && a[0].is_fixnum() && a[1].is_fixnum()) {
                int64_t x = a[0].to_fixnum(), y = a[1].to_fixnum();
                return SCM::from_bool(pred(x < y ? -1 : (x > y ? 1 : 0)));
            }
            if (nargs == 0) return SCM_BOOL_T;
            SCM prev = a[0];
            if (nargs == 1) {
                if (!is_real_number(prev) && !as_obj<SubrObj>(slot)->generic.is_true()) {
                    wrong_type_arg(name, 1, prev, "real");
                }
                return SCM_BOOL_T;
            }
            for (size_t i = 1; i < nargs; ++i) {
                SCM cur = a[i];
                if ((!is_real_number(prev) || !is_real_number(cur)) && as_obj<SubrObj>(slot)->generic.is_true()) {
                    SCM args[2] = {prev, cur};
                    if (goops_apply_generic(as_obj<SubrObj>(slot)->generic, args, 2).is_false()) return SCM_BOOL_F;
                } else {
                    int c = scm_num_cmp(prev, cur);
                    if (c == 2 || !pred(c)) return SCM_BOOL_F;
                }
                prev = cur;
            }
            return SCM_BOOL_T;
        });
        as_obj<SubrObj>(slot)->generic_capable = true;
        as_obj<SubrObj>(slot)->raw_varargs = true;
    };
    reg_ord("<",  subr_num_lt, [](int c) { return c < 0; });
    reg_ord("<=", subr_num_le, [](int c) { return c <= 0; });
    reg_ord(">",  subr_num_gt, [](int c) { return c > 0; });
    reg_ord(">=", subr_num_ge, [](int c) { return c >= 0; });

    define_proc("abs", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_real_number(a[0])) wrong_type_arg("abs", 1, a[0], "real");
        if (is_real_obj(a[0])) return make_real(std::fabs(as_obj<RealObj>(a[0])->val));
        return (scm_num_cmp(a[0], SCM::from_fixnum(0)) < 0) ? scm_num_neg(a[0]) : a[0];
    });

    auto reg_minmax = [this](const char* name, bool is_max) {
        define_proc(name, 1, 0, true, [this, name, is_max](const SCM* a, size_t nargs) -> SCM {
            SCM best = a[0];
            if (!is_real_number(best)) wrong_type_arg(name, 1, best, "real");
            bool inexact_seen = is_inexact(best);
            SCM rest = nargs > 1 ? a[1] : SCM_EOL;
            while (is_pair(rest)) {
                SCM cur = car(rest);
                if (!is_real_number(cur)) wrong_type_arg(name, 2, cur, "real");
                if (is_inexact(cur)) inexact_seen = true;
                int c = scm_num_cmp(cur, best);
                if (c == 2) return make_real(std::numeric_limits<double>::quiet_NaN());
                if ((is_max && c > 0) || (!is_max && c < 0)) best = cur;
                rest = cdr(rest);
            }
            return (inexact_seen && is_exact(best)) ? scm_exact_to_inexact(best) : best;
        });
    };
    reg_minmax("max", true);
    reg_minmax("min", false);

    define_proc("quotient", 2, 0, false, [](const SCM* a, size_t) -> SCM { return scm_quotient(a[0], a[1]); });
    define_proc("remainder", 2, 0, false, [](const SCM* a, size_t) -> SCM { return scm_remainder(a[0], a[1]); });
    define_proc("modulo", 2, 0, false, [](const SCM* a, size_t) -> SCM { return scm_modulo(a[0], a[1]); });

    {
        SCM s_gcd = define_proc("gcd", 0, 0, true, [](const SCM* a, size_t nargs) -> SCM {
            if (nargs == 2) return scm_gcd(a[0], a[1]);
            SCM acc = SCM::from_fixnum(0);
            for (size_t i = 0; i < nargs; ++i) acc = scm_gcd(acc, a[i]);
            return acc;
        });
        as_obj<SubrObj>(s_gcd)->raw_varargs = true;
    }
    {
        SCM s_lcm = define_proc("lcm", 0, 0, true, [](const SCM* a, size_t nargs) -> SCM {
            if (nargs == 2) return scm_lcm(a[0], a[1]);
            SCM acc = SCM::from_fixnum(1);
            for (size_t i = 0; i < nargs; ++i) acc = scm_lcm(acc, a[i]);
            return acc;
        });
        as_obj<SubrObj>(s_lcm)->raw_varargs = true;
    }

    define_proc("numerator", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM x = a[0];
        if (is_exact_integer(x)) return x;
        if (is_fraction(x)) return as_obj<FractionObj>(x)->num;
        if (is_real_obj(x)) {
            double d = as_obj<RealObj>(x)->val;
            if (!std::isfinite(d)) out_of_range("numerator", x);
            SCM ex = scm_inexact_to_exact(x);
            return scm_exact_to_inexact(is_fraction(ex) ? as_obj<FractionObj>(ex)->num : ex);
        }
        wrong_type_arg("numerator", 1, x, "rational");
    });
    define_proc("denominator", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM x = a[0];
        if (is_exact_integer(x)) return SCM::from_fixnum(1);
        if (is_fraction(x)) return as_obj<FractionObj>(x)->den;
        if (is_real_obj(x)) {
            double d = as_obj<RealObj>(x)->val;
            if (!std::isfinite(d)) out_of_range("denominator", x);
            SCM ex = scm_inexact_to_exact(x);
            return scm_exact_to_inexact(is_fraction(ex) ? as_obj<FractionObj>(ex)->den : SCM::from_fixnum(1));
        }
        wrong_type_arg("denominator", 1, x, "rational");
    });

    auto reg_round_op = [this](const char* name, int mode) {
        // mode: 0=floor, 1=ceiling, 2=truncate, 3=round
        define_proc(name, 1, 0, false, [this, name, mode](const SCM* a, size_t) -> SCM {
            SCM x = a[0];
            if (is_exact_integer(x)) return x;
            if (is_fraction(x)) {
                auto* f = as_obj<FractionObj>(x);
                uint64_t sn = 0, sd = 0;
                BigIntView n = scm_to_bigint_view(f->num, sn, name, 1);
                BigIntView d = scm_to_bigint_view(f->den, sd, name, 1);
                BigInt q;
                if (mode == 0) q = BigInt::fdiv_q(n, d);
                else if (mode == 1) q = BigInt::cdiv_q(n, d);
                else if (mode == 2) q = BigInt::tdiv_q(n, d);
                else {
                    BigInt r;
                    BigInt::fdiv_qr(n, d, q, r);
                    r = BigInt::mul_2exp(r, 1);
                    int cmp = BigInt::cmp(r, d);
                    if (cmp > 0 || (cmp == 0 && q.is_odd())) {
                        q = BigInt::add_ui(q, 1);
                    }
                }
                return scm_from_bigint(std::move(q));
            }
            if (is_real_obj(x)) {
                double d = as_obj<RealObj>(x)->val;
                if (!std::isfinite(d)) return x;
                if (mode == 0) return make_real(std::floor(d));
                if (mode == 1) return make_real(std::ceil(d));
                if (mode == 2) return make_real(std::trunc(d));
                return make_real(std::nearbyint(d)); // round ties to even
            }
            wrong_type_arg(name, 1, x, "real");
        });
    };
    reg_round_op("floor", 0);
    reg_round_op("ceiling", 1);
    reg_round_op("truncate", 2);
    reg_round_op("round", 3);

    // Transcendentals
    define_proc("exp", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::exp(scm_to_complex(a[0], "exp")); return make_complex(r.real(), r.imag()); }
        return make_real(std::exp(scm_to_double(a[0], "exp")));
    });
    define_proc("log", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::log(scm_to_complex(a[0], "log")); return make_complex(r.real(), r.imag()); }
        double d = scm_to_double(a[0], "log");
        if (d < 0.0) { auto r = std::log(std::complex<double>(d, 0.0)); return make_complex(r.real(), r.imag()); }
        return make_real(std::log(d));
    });
    define_proc("log10", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::log10(scm_to_complex(a[0], "log10")); return make_complex(r.real(), r.imag()); }
        double d = scm_to_double(a[0], "log10");
        if (d < 0.0) { auto r = std::log10(std::complex<double>(d, 0.0)); return make_complex(r.real(), r.imag()); }
        return make_real(std::log10(d));
    });
    define_proc("sin", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::sin(scm_to_complex(a[0], "sin")); return make_complex(r.real(), r.imag()); }
        return make_real(std::sin(scm_to_double(a[0], "sin")));
    });
    define_proc("cos", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::cos(scm_to_complex(a[0], "cos")); return make_complex(r.real(), r.imag()); }
        return make_real(std::cos(scm_to_double(a[0], "cos")));
    });
    define_proc("tan", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::tan(scm_to_complex(a[0], "tan")); return make_complex(r.real(), r.imag()); }
        return make_real(std::tan(scm_to_double(a[0], "tan")));
    });
    define_proc("asin", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::asin(scm_to_complex(a[0], "asin")); return make_complex(r.real(), r.imag()); }
        double d = scm_to_double(a[0], "asin");
        if (d < -1.0 || d > 1.0) { auto r = std::asin(std::complex<double>(d, 0.0)); return make_complex(r.real(), r.imag()); }
        return make_real(std::asin(d));
    });
    define_proc("acos", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::acos(scm_to_complex(a[0], "acos")); return make_complex(r.real(), r.imag()); }
        double d = scm_to_double(a[0], "acos");
        if (d < -1.0 || d > 1.0) { auto r = std::acos(std::complex<double>(d, 0.0)); return make_complex(r.real(), r.imag()); }
        return make_real(std::acos(d));
    });
    define_proc("atan", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (nargs >= 2 && !a[1].is_undefined()) {
            return make_real(std::atan2(scm_to_double(a[0], "atan"), scm_to_double(a[1], "atan")));
        }
        if (is_complex_obj(a[0])) { auto r = std::atan(scm_to_complex(a[0], "atan")); return make_complex(r.real(), r.imag()); }
        return make_real(std::atan(scm_to_double(a[0], "atan")));
    });
    define_proc("sinh", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::sinh(scm_to_complex(a[0], "sinh")); return make_complex(r.real(), r.imag()); }
        return make_real(std::sinh(scm_to_double(a[0], "sinh")));
    });
    define_proc("cosh", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::cosh(scm_to_complex(a[0], "cosh")); return make_complex(r.real(), r.imag()); }
        return make_real(std::cosh(scm_to_double(a[0], "cosh")));
    });
    define_proc("tanh", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::tanh(scm_to_complex(a[0], "tanh")); return make_complex(r.real(), r.imag()); }
        return make_real(std::tanh(scm_to_double(a[0], "tanh")));
    });
    define_proc("asinh", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::asinh(scm_to_complex(a[0], "asinh")); return make_complex(r.real(), r.imag()); }
        return make_real(std::asinh(scm_to_double(a[0], "asinh")));
    });
    define_proc("acosh", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::acosh(scm_to_complex(a[0], "acosh")); return make_complex(r.real(), r.imag()); }
        double d = scm_to_double(a[0], "acosh");
        if (d < 1.0) { auto r = std::acosh(std::complex<double>(d, 0.0)); return make_complex(r.real(), r.imag()); }
        return make_real(std::acosh(d));
    });
    define_proc("atanh", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) { auto r = std::atanh(scm_to_complex(a[0], "atanh")); return make_complex(r.real(), r.imag()); }
        double d = scm_to_double(a[0], "atanh");
        if (d <= -1.0 || d >= 1.0) { auto r = std::atanh(std::complex<double>(d, 0.0)); return make_complex(r.real(), r.imag()); }
        return make_real(std::atanh(d));
    });

    define_proc("sqrt", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) {
            auto r = std::sqrt(scm_to_complex(a[0], "sqrt"));
            return make_complex(r.real(), r.imag());
        }
        double d = scm_to_double(a[0], "sqrt");
        if (d < 0.0) return make_complex(0.0, std::sqrt(-d));
        return make_real(std::sqrt(d));
    });

    define_proc("inf", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return make_real(std::numeric_limits<double>::infinity());
    });
    define_proc("nan", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return make_real(std::numeric_limits<double>::quiet_NaN());
    });
    define_proc("modulo-expt", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        uint64_t sn = 0, sk = 0, sm = 0;
        BigIntView nv = scm_to_bigint_view(a[0], sn, "modulo-expt", 1);
        BigIntView kv = scm_to_bigint_view(a[1], sk, "modulo-expt", 2);
        BigIntView mv = scm_to_bigint_view(a[2], sm, "modulo-expt", 3);
        if (mv.is_zero()) num_overflow("modulo-expt");
        bool neg_m = mv.sgn() < 0;
        BigIntView abs_m = mv.abs_view();
        BigInt base_owned, exp_owned;
        BigIntView base_v = nv, exp_v = kv;
        if (kv.sgn() < 0) {
            if (!BigInt::modinv(nv, abs_m, base_owned)) {
                num_overflow("modulo-expt");
            }
            base_v = base_owned.view();
            exp_owned = BigInt::neg(kv);
            exp_v = exp_owned.view();
        }
        BigInt r = BigInt::powm(base_v, exp_v, abs_m);
        if (neg_m && !r.is_zero()) r = BigInt::sub(r, abs_m);
        return scm_from_bigint(std::move(r));
    });

    auto int_expt_impl = [this](SCM base, SCM exp_scm) -> SCM {
        int64_t k = scm_to_int64(exp_scm, "integer-expt", 2);
        if (k == 0) return is_exact(base) ? SCM::from_fixnum(1) : make_real(1.0);
        if (base.is_fixnum() && base.to_fixnum() == 0) {
            if (k < 0) num_overflow("expt");
            return SCM::from_fixnum(0);
        }
        if (base.is_fixnum() && base.to_fixnum() == 1) return SCM::from_fixnum(1);
        if (base.is_fixnum() && base.to_fixnum() == -1) return SCM::from_fixnum((k & 1) ? -1 : 1);

        bool neg = k < 0;
        uint64_t e = neg ? static_cast<uint64_t>(-k) : static_cast<uint64_t>(k);

        if (is_exact_integer(base)) {
            uint64_t sb = 0;
            BigIntView zb = scm_to_bigint_view(base, sb, "expt", 1);
            SCM res = scm_from_bigint(BigInt::pow_ui(zb, e));
            return neg ? scm_num_div(SCM::from_fixnum(1), res) : res;
        }
        if (is_fraction(base)) {
            auto* f = as_obj<FractionObj>(base);
            uint64_t sn = 0, sd = 0;
            BigIntView zn = scm_to_bigint_view(f->num, sn, "expt", 1);
            BigIntView zd = scm_to_bigint_view(f->den, sd, "expt", 1);
            SCM s_num = scm_from_bigint(BigInt::pow_ui(zn, e));
            SCM s_den = scm_from_bigint(BigInt::pow_ui(zd, e));
            return neg ? make_fraction(s_den, s_num) : make_fraction(s_num, s_den);
        }
        // Binary exponentiation for inexact/complex
        SCM acc = make_real(1.0);
        SCM b = base;
        while (e > 0) {
            if (e & 1) acc = scm_num_mul(acc, b);
            e >>= 1;
            if (e > 0) b = scm_num_mul(b, b);
        }
        return neg ? scm_num_div(make_real(1.0), acc) : acc;
    };

    define_proc("integer-expt", 2, 0, false, [int_expt_impl](const SCM* a, size_t) -> SCM {
        return int_expt_impl(a[0], a[1]);
    });

    define_proc("expt", 2, 0, false, [this, int_expt_impl](const SCM* a, size_t) -> SCM {
        if (is_exact_integer(a[1])) return int_expt_impl(a[0], a[1]);
        if (is_complex_obj(a[0]) || is_complex_obj(a[1])) {
            auto r = std::pow(scm_to_complex(a[0], "expt"), scm_to_complex(a[1], "expt"));
            return make_complex(r.real(), r.imag());
        }
        double da = scm_to_double(a[0], "expt");
        double db = scm_to_double(a[1], "expt");
        if (da < 0.0 && std::trunc(db) != db) {
            auto r = std::pow(std::complex<double>(da, 0.0), std::complex<double>(db, 0.0));
            return make_complex(r.real(), r.imag());
        }
        return make_real(std::pow(da, db));
    });

    // Complex accessors
    define_proc("make-rectangular", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        return make_complex(scm_to_double(a[0], "make-rectangular"), scm_to_double(a[1], "make-rectangular"));
    });
    define_proc("make-polar", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        double r = scm_to_double(a[0], "make-polar");
        double th = scm_to_double(a[1], "make-polar");
        return make_complex(r * std::cos(th), r * std::sin(th));
    });
    define_proc("real-part", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) return make_real(as_obj<ComplexObj>(a[0])->real);
        if (!is_number(a[0])) wrong_type_arg("real-part", 1, a[0], "number");
        return a[0];
    });
    define_proc("imag-part", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) return make_real(as_obj<ComplexObj>(a[0])->imag);
        if (!is_number(a[0])) wrong_type_arg("imag-part", 1, a[0], "number");
        return SCM::from_fixnum(0);
    });
    define_proc("magnitude", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_complex_obj(a[0])) {
            auto* c = as_obj<ComplexObj>(a[0]);
            return make_real(std::hypot(c->real, c->imag));
        }
        if (!is_real_number(a[0])) wrong_type_arg("magnitude", 1, a[0], "number");
        return (scm_num_cmp(a[0], SCM::from_fixnum(0)) < 0) ? scm_num_neg(a[0]) : a[0];
    });
    define_proc("angle", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        auto c = scm_to_complex(a[0], "angle");
        return make_real(std::atan2(c.imag(), c.real()));
    });

    define_proc("exact->inexact", 1, 0, false, [](const SCM* a, size_t) -> SCM { return scm_exact_to_inexact(a[0]); });
    define_proc("inexact->exact", 1, 0, false, [](const SCM* a, size_t) -> SCM { return scm_inexact_to_exact(a[0]); });

    // Guile 1.8 flonum math subrs ($sqrt, $sin, ...)
    auto reg_dsubr = [this](const char* name, double(*fn)(double)) {
        define_proc(name, 1, 0, false, [this, name, fn](const SCM* a, size_t) -> SCM {
            return make_real(fn(scm_to_double(a[0], name)));
        });
    };
    reg_dsubr("$sqrt",  std::sqrt);
    reg_dsubr("$abs",   std::fabs);
    reg_dsubr("$exp",   std::exp);
    reg_dsubr("$log",   std::log);
    reg_dsubr("$log10", std::log10);
    reg_dsubr("$sin",   std::sin);
    reg_dsubr("$cos",   std::cos);
    reg_dsubr("$tan",   std::tan);
    reg_dsubr("$asin",  std::asin);
    reg_dsubr("$acos",  std::acos);
    reg_dsubr("$atan",  std::atan);
    reg_dsubr("$sinh",  std::sinh);
    reg_dsubr("$cosh",  std::cosh);
    reg_dsubr("$tanh",  std::tanh);
    reg_dsubr("$asinh", std::asinh);
    reg_dsubr("$acosh", std::acosh);
    reg_dsubr("$atanh", std::atanh);
    define_proc("$expt", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        return make_real(std::pow(scm_to_double(a[0], "$expt"), scm_to_double(a[1], "$expt")));
    });
    define_proc("$atan2", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        return make_real(std::atan2(scm_to_double(a[0], "$atan2"), scm_to_double(a[1], "$atan2")));
    });

    // Bitwise ops (raw_varargs = zero rest-list boxing!)
    {
        SCM s_and = define_proc("logand", 0, 0, true, [](const SCM* a, size_t nargs) -> SCM {
            if (nargs == 2) return scm_logand(a[0], a[1]);
            SCM acc = SCM::from_fixnum(-1);
            for (size_t i = 0; i < nargs; ++i) acc = scm_logand(acc, a[i]);
            return acc;
        });
        as_obj<SubrObj>(s_and)->raw_varargs = true;
    }
    {
        SCM s_ior = define_proc("logior", 0, 0, true, [](const SCM* a, size_t nargs) -> SCM {
            if (nargs == 2) return scm_logior(a[0], a[1]);
            SCM acc = SCM::from_fixnum(0);
            for (size_t i = 0; i < nargs; ++i) acc = scm_logior(acc, a[i]);
            return acc;
        });
        as_obj<SubrObj>(s_ior)->raw_varargs = true;
    }
    {
        SCM s_xor = define_proc("logxor", 0, 0, true, [](const SCM* a, size_t nargs) -> SCM {
            if (nargs == 2) return scm_logxor(a[0], a[1]);
            SCM acc = SCM::from_fixnum(0);
            for (size_t i = 0; i < nargs; ++i) acc = scm_logxor(acc, a[i]);
            return acc;
        });
        as_obj<SubrObj>(s_xor)->raw_varargs = true;
    }
    define_proc("lognot", 1, 0, false, [](const SCM* a, size_t) -> SCM { return scm_lognot(a[0]); });
    define_proc("logtest", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        SCM r = scm_logand(a[0], a[1]);
        return SCM::from_bool(!(r.is_fixnum() && r.to_fixnum() == 0));
    });
    define_proc("logbit?", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t idx = scm_to_int64(a[0], "logbit?", 1);
        if (idx < 0) out_of_range("logbit?", a[0]);
        uint64_t scratch = 0;
        BigIntView z = scm_to_bigint_view(a[1], scratch, "logbit?", 2);
        return SCM::from_bool(BigInt::tstbit(z, static_cast<size_t>(idx)));
    });
    define_proc("ash", 2, 0, false, [](const SCM* a, size_t) -> SCM { return scm_ash(a[0], a[1]); });
    define_proc("logcount", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (a[0].is_fixnum()) {
            int64_t v = a[0].to_fixnum();
            uint64_t u = (v >= 0) ? static_cast<uint64_t>(v) : static_cast<uint64_t>(~v);
            return SCM::from_fixnum(static_cast<int64_t>(gcpp_popcount(u)));
        }
        uint64_t scratch = 0;
        BigIntView z = scm_to_bigint_view(a[0], scratch, "logcount", 1);
        return make_uinteger(BigInt::logcount(z));
    });
    define_proc("integer-length", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (a[0].is_fixnum()) {
            int64_t v = a[0].to_fixnum();
            uint64_t u = (v >= 0) ? static_cast<uint64_t>(v) : static_cast<uint64_t>(~v);
            return SCM::from_fixnum(u == 0 ? 0 : static_cast<int64_t>(64 - gcpp_countl_zero(u)));
        }
        uint64_t scratch = 0;
        BigIntView z = scm_to_bigint_view(a[0], scratch, "integer-length", 1);
        return make_uinteger(BigInt::integer_length(z));
    });
    define_proc("bit-extract", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t start = scm_to_int64(a[1], "bit-extract", 2);
        int64_t end = scm_to_int64(a[2], "bit-extract", 3);
        if (start < 0 || end < start) out_of_range("bit-extract", a[1]);
        uint64_t scratch = 0;
        BigIntView z = scm_to_bigint_view(a[0], scratch, "bit-extract", 1);
        BigInt shifted = BigInt::fdiv_q_2exp(z, static_cast<size_t>(start));
        BigInt mask = BigInt::sub_ui(BigInt::mul_2exp(BigInt(int64_t(1)), static_cast<size_t>(end - start)), 1);
        return scm_from_bigint(BigInt::bit_and(shifted, mask));
    });
    define_proc("integer-expt", 2, 0, false, [int_expt_impl](const SCM* a, size_t) -> SCM {
        return int_expt_impl(a[0], a[1]);
    });
    define_proc("divide", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        return scm_num_div(a[0], a[1]);
    });
    define_proc("rationalize", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_real_number(a[0])) wrong_type_arg("rationalize", 1, a[0], "real");
        if (!is_real_number(a[1])) wrong_type_arg("rationalize", 2, a[1], "real");
        if (is_exact_integer(a[0])) return is_inexact(a[1]) ? scm_exact_to_inexact(a[0]) : a[0];
        bool inx = is_inexact(a[0]) || is_inexact(a[1]);
        double dx = scm_to_double(a[0], "rationalize");
        double de = std::fabs(scm_to_double(a[1], "rationalize"));
        if (!std::isfinite(dx) || !std::isfinite(de)) {
            if (std::isnan(dx) || std::isnan(de)) return make_real(std::numeric_limits<double>::quiet_NaN());
            return make_real(dx);
        }
        SCM ex = is_exact(a[0]) ? a[0] : scm_inexact_to_exact(a[0]);
        SCM eps = is_exact(a[1]) ? a[1] : scm_inexact_to_exact(a[1]);
        if (scm_num_cmp(eps, SCM::from_fixnum(0)) < 0) eps = scm_num_neg(eps);

        auto exact_floor = [](SCM q) -> SCM {
            BigRat mq = scm_to_bigrat(q, "rationalize");
            return scm_from_bigint(BigInt::fdiv_q(mq.num, mq.den));
        };

        SCM int_part = exact_floor(ex);
        if (scm_num_cmp(ex, int_part) == 0) {
            return inx ? scm_exact_to_inexact(ex) : ex;
        }
        // Guile 1.8.8 exact continued-fraction convergent algorithm (libguile/numbers.c)
        ex = scm_num_sub(ex, int_part);
        SCM rx = scm_num_div(SCM::from_fixnum(1), ex);
        SCM tt = SCM::from_fixnum(1);
        SCM a1 = SCM::from_fixnum(0), a2 = SCM::from_fixnum(1);
        SCM b1 = SCM::from_fixnum(1), b2 = SCM::from_fixnum(0);
        for (int i = 0; i < 1000000; ++i) {
            SCM num = scm_num_add(scm_num_mul(a1, tt), a2);
            SCM den = scm_num_add(scm_num_mul(b1, tt), b2);
            if (scm_num_cmp(den, SCM::from_fixnum(0)) != 0) {
                SCM diff = scm_num_sub(ex, scm_num_div(num, den));
                if (scm_num_cmp(diff, SCM::from_fixnum(0)) < 0) diff = scm_num_neg(diff);
                if (scm_num_cmp(diff, eps) <= 0) {
                    SCM res = scm_num_add(int_part, scm_num_div(num, den));
                    return inx ? scm_exact_to_inexact(res) : res;
                }
            }
            rx = scm_num_div(SCM::from_fixnum(1), scm_num_sub(rx, tt));
            tt = exact_floor(rx);
            a2 = a1; b2 = b1;
            a1 = num; b1 = den;
        }
        num_overflow("rationalize");
    });
}

} // namespace guile
