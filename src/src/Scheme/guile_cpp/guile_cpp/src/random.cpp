#include "guile_cpp/vm.hpp"
#include <cmath>
#include <random>

namespace guile {

namespace {

inline uint64_t rotl(uint64_t x, int k) { return (x << k) | (x >> (64 - k)); }

uint64_t rstate_next_u64(RandomStateObj* st) {
    uint64_t result = rotl(st->s[1] * 5, 7) * 9;
    uint64_t t = st->s[1] << 17;
    st->s[2] ^= st->s[0];
    st->s[3] ^= st->s[1];
    st->s[1] ^= st->s[2];
    st->s[0] ^= st->s[3];
    st->s[2] ^= t;
    st->s[3] = rotl(st->s[3], 45);
    return result;
}

double rstate_uniform(RandomStateObj* st) {
    return (rstate_next_u64(st) >> 11) * (1.0 / 9007199254740992.0);
}

double rstate_normal(RandomStateObj* st) {
    if (st->has_normal) {
        st->has_normal = false;
        return st->cached_normal;
    }
    double u1, u2, s;
    do {
        u1 = 2.0 * rstate_uniform(st) - 1.0;
        u2 = 2.0 * rstate_uniform(st) - 1.0;
        s = u1 * u1 + u2 * u2;
    } while (s >= 1.0 || s == 0.0);
    double m = std::sqrt(-2.0 * std::log(s) / s);
    st->cached_normal = u2 * m;
    st->has_normal = true;
    return u1 * m;
}

void seed_rstate(RandomStateObj* st, uint64_t seed) {
    // SplitMix64 seeding
    for (int i = 0; i < 4; ++i) {
        uint64_t z = (seed += 0x9e3779b97f4a7c15ULL);
        z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
        z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
        st->s[i] = z ^ (z >> 31);
    }
    st->has_normal = false;
}

RandomStateObj* get_rstate(const SCM* a, size_t nargs, size_t idx, const char* subr) {
    SCM rs = (nargs > idx && !a[idx].is_undefined()) ? a[idx] : as_obj<VariableObj>(vm().random_state_var)->value;
    if (!is_random_state(rs)) vm().wrong_type_arg(subr, static_cast<int>(idx + 1), rs, "random-state");
    return as_obj<RandomStateObj>(rs);
}

} // namespace

void VM::init_random() {
    auto* def_rs = heap_.alloc<RandomStateObj>();
    seed_rstate(def_rs, 0x123456789abcdef0ULL);
    random_state_var = define("*random-state*", SCM::from_obj(def_rs));

    define_proc("random-state?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_random_state(a[0])); });
    define_proc("seed->random-state", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        auto* rs = heap_.alloc<RandomStateObj>();
        uint64_t h = scm_ihash(a[0], UINT64_MAX);
        seed_rstate(rs, h);
        return SCM::from_obj(rs);
    });
    define_proc("copy-random-state", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        auto* src = get_rstate(a, nargs, 0, "copy-random-state");
        auto* dst = heap_.alloc<RandomStateObj>();
        *dst = *src;
        dst->gc_next = nullptr;
        return SCM::from_obj(dst);
    });
    define_proc("random-state-from-platform", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        std::random_device rd;
        uint64_t seed = (static_cast<uint64_t>(rd()) << 32) ^ rd();
        auto* rs = heap_.alloc<RandomStateObj>();
        seed_rstate(rs, seed);
        return SCM::from_obj(rs);
    });
    define_proc("datum->random-state", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        auto* rs = heap_.alloc<RandomStateObj>();
        seed_rstate(rs, scm_ihash(a[0], UINT64_MAX));
        return SCM::from_obj(rs);
    });
    define_proc("random-state->datum", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_random_state(a[0])) wrong_type_arg("random-state->datum", 1, a[0], "random-state");
        auto* rs = as_obj<RandomStateObj>(a[0]);
        return list4(make_uinteger(rs->s[0]), make_uinteger(rs->s[1]), make_uinteger(rs->s[2]), make_uinteger(rs->s[3]));
    });

    define_proc("random", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        auto* rs = get_rstate(a, nargs, 1, "random");
        if (is_exact_integer(a[0])) {
            if (a[0].is_fixnum()) {
                int64_t n = a[0].to_fixnum();
                if (n <= 0) out_of_range("random", a[0]);
                return make_uinteger(rstate_next_u64(rs) % static_cast<uint64_t>(n));
            }
            uint64_t scratch = 0;
            BigIntView zn = scm_to_bigint_view(a[0], scratch, "random", 1);
            if (zn.sgn() <= 0) out_of_range("random", a[0]);
            size_t nlimbs = (zn.bit_length_abs() + 63) >> 6;
            BigInt zr;
            uint64_t* rp = zr.alloc_uninit(nlimbs);
            for (size_t i = 0; i < nlimbs; ++i) rp[i] = rstate_next_u64(rs);
            zr.set_size_and_normalize(nlimbs, false);
            return scm_from_bigint(BigInt::mod(zr, zn));
        }
        if (is_real_obj(a[0])) {
            double n = as_obj<RealObj>(a[0])->val;
            if (n <= 0.0) out_of_range("random", a[0]);
            return make_real(rstate_uniform(rs) * n);
        }
        wrong_type_arg("random", 1, a[0], "positive real");
    });

    define_proc("random:uniform", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        return make_real(rstate_uniform(get_rstate(a, nargs, 0, "random:uniform")));
    });
    define_proc("random:normal", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        return make_real(rstate_normal(get_rstate(a, nargs, 0, "random:normal")));
    });
    define_proc("random:exp", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        double u = rstate_uniform(get_rstate(a, nargs, 0, "random:exp"));
        return make_real(-std::log(1.0 - u));
    });
    define_proc("random:normal-vector!", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        auto* rs = get_rstate(a, nargs, 1, "random:normal-vector!");
        if (is_vector(a[0])) {
            auto* v = as_obj<VectorObj>(a[0]);
            for (SCM& e : v->elts) e = make_real(rstate_normal(rs));
            return SCM_UNSPECIFIED;
        }
        wrong_type_arg("random:normal-vector!", 1, a[0], "vector");
    });
}

} // namespace guile
