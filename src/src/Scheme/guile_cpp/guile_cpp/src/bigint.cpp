#include "guile_cpp/bigint.hpp"
#include "basic.hpp"
#include "guile_cpp/compat.hpp"
#include <cassert>

#if defined(_MSC_VER) && defined(_M_X64)
#include <intrin.h>
#endif

namespace guile {

namespace {

// ============================================================================
// Section 1: Portable 64x64->128 & 128/64 Primitives
// ============================================================================

#if defined(__SIZEOF_INT128__)
using u128 = unsigned __int128;
using i128 = __int128;
#endif

static inline uint64_t mul64(uint64_t a, uint64_t b, uint64_t& hi) noexcept {
#if defined(__SIZEOF_INT128__)
    u128 p = static_cast<u128>(a) * static_cast<u128>(b);
    hi = static_cast<uint64_t>(p >> 64);
    return static_cast<uint64_t>(p);
#elif defined(_MSC_VER) && defined(_M_X64)
    return _umul128(a, b, &hi);
#else
    uint64_t a0 = a & 0xFFFFFFFFULL, a1 = a >> 32;
    uint64_t b0 = b & 0xFFFFFFFFULL, b1 = b >> 32;
    uint64_t p00 = a0 * b0;
    uint64_t p01 = a0 * b1;
    uint64_t p10 = a1 * b0;
    uint64_t p11 = a1 * b1;
    uint64_t mid = (p00 >> 32) + (p01 & 0xFFFFFFFFULL) + (p10 & 0xFFFFFFFFULL);
    hi = p11 + (p01 >> 32) + (p10 >> 32) + (mid >> 32);
    return (mid << 32) | (p00 & 0xFFFFFFFFULL);
#endif
}

static inline uint64_t add_carry(uint64_t a, uint64_t b, uint64_t cin, uint64_t& cout) noexcept {
#if defined(__SIZEOF_INT128__)
    u128 s = static_cast<u128>(a) + static_cast<u128>(b) + static_cast<u128>(cin);
    cout = static_cast<uint64_t>(s >> 64);
    return static_cast<uint64_t>(s);
#elif defined(_MSC_VER) && defined(_M_X64)
    unsigned long long r = 0;
    cout = _addcarry_u64(static_cast<unsigned char>(cin), a, b, &r);
    return static_cast<uint64_t>(r);
#else
    uint64_t s1 = a + b;
    uint64_t c1 = (s1 < a) ? 1ULL : 0ULL;
    uint64_t s2 = s1 + cin;
    uint64_t c2 = (s2 < s1) ? 1ULL : 0ULL;
    cout = c1 + c2;
    return s2;
#endif
}

static inline uint64_t sub_borrow(uint64_t a, uint64_t b, uint64_t bin, uint64_t& bout) noexcept {
#if defined(__SIZEOF_INT128__)
    u128 d = static_cast<u128>(a) - static_cast<u128>(b) - static_cast<u128>(bin);
    bout = static_cast<uint64_t>((d >> 64) & 1ULL);
    return static_cast<uint64_t>(d);
#elif defined(_MSC_VER) && defined(_M_X64)
    unsigned long long r = 0;
    bout = _subborrow_u64(static_cast<unsigned char>(bin), a, b, &r);
    return static_cast<uint64_t>(r);
#else
    uint64_t d1 = a - b;
    uint64_t b1 = (a < b) ? 1ULL : 0ULL;
    uint64_t d2 = d1 - bin;
    uint64_t b2 = (d1 < bin) ? 1ULL : 0ULL;
    bout = b1 + b2;
    return d2;
#endif
}

// Compute v = floor((2^128 - 1) / d) - 2^64 for normalized d >= 2^63.
static inline uint64_t invert_limb(uint64_t d) noexcept {
    // 2-step 32-bit exact division refinement + exact adjustment
    uint64_t d_hi = (d >> 32) + 1ULL;
    uint64_t v = 0;
    if (d_hi < (1ULL << 32)) {
        uint64_t q0 = (~0ULL) / d_hi; // in [2^32, 2^33 - 1]
        v = (q0 - (1ULL << 32)) << 32;
    }
    // Compute R = (2^128 - 1) - (2^64 + v) * d
    uint64_t p_hi, p_lo = mul64(v, d, p_hi);
    uint64_t r_lo = ~p_lo;
    uint64_t r_hi = (~d) - p_hi;
    // Refine by 32 more bits
    while (r_hi >= d_hi) {
        v += (1ULL << 32);
        p_lo = mul64(v, d, p_hi);
        r_lo = ~p_lo;
        r_hi = (~d) - p_hi;
    }
    // Now r_hi < d_hi <= 2^32, so (r_hi << 32) | (r_lo >> 32) does not overflow
    uint64_t r_top = (r_hi << 32) | (r_lo >> 32);
    uint64_t delta = r_top / d_hi;
    v += delta;
    p_lo = mul64(v, d, p_hi);
    r_lo = ~p_lo;
    r_hi = (~d) - p_hi;
    // At most 2 exact corrections
    while (r_hi > 0 || r_lo >= d) {
        uint64_t b = 0;
        r_lo = sub_borrow(r_lo, d, 0, b);
        r_hi -= b;
        ++v;
    }
    return v;
}

// Granlund-Montgomery Algorithm 4: exact (u1 * 2^64 + u0) / d for u1 < d, d >= 2^63.
static inline uint64_t div_2by1(uint64_t u1, uint64_t u0, uint64_t d, uint64_t v, uint64_t& rem) noexcept {
    uint64_t q1, q0 = mul64(v, u1, q1);
    uint64_t c = 0;
    q0 = add_carry(q0, u0, 0, c);
    q1 = q1 + u1 + c + 1ULL;
    uint64_t r = u0 - q1 * d;
    if (r > q0) {
        --q1;
        r += d;
    }
    if (r >= d) {
        ++q1;
        r -= d;
    }
    rem = r;
    return q1;
}

// Granlund-Montgomery Algorithm 3: reciprocal for 2-limb normalized divisor (d1, d0), d1 >= 2^63.
static inline uint64_t invert_2limbs(uint64_t d1, uint64_t d0) noexcept {
    uint64_t v = invert_limb(d1);
    uint64_t p = d1 * v + d0;
    if (p < d0) {
        --v;
        if (p >= d1) {
            --v;
            p -= d1;
        }
        p -= d1;
    }
    uint64_t t1, t0 = mul64(v, d0, t1);
    p += t1;
    if (p < t1) {
        --v;
        if (p > d1 || (p == d1 && t0 >= d0)) --v;
    }
    return v;
}

// Granlund-Montgomery Algorithm 5: exact 3-by-2 division for (u2, u1) < (d1, d0), d1 >= 2^63.
static inline uint64_t div_3by2(uint64_t u2, uint64_t u1, uint64_t u0,
                                uint64_t d1, uint64_t d0, uint64_t v,
                                uint64_t& r1_out, uint64_t& r0_out) noexcept {
    uint64_t q1, q0 = mul64(v, u2, q1);
    uint64_t c = 0;
    q0 = add_carry(q0, u1, 0, c);
    q1 = q1 + u2 + c;

    uint64_t r1 = u1 - q1 * d1;
    uint64_t t1, t0 = mul64(d0, q1, t1);
    uint64_t b = 0;
    uint64_t r0 = sub_borrow(u0, t0, 0, b);
    r1 = r1 - t1 - b;

    ++q1;
    b = 0;
    r0 = sub_borrow(r0, d0, 0, b);
    r1 = r1 - d1 - b;

    if (r1 >= q0) {
        --q1;
        c = 0;
        r0 = add_carry(r0, d0, 0, c);
        r1 = r1 + d1 + c;
    }
    if (r1 > d1 || (r1 == d1 && r0 >= d0)) {
        ++q1;
        b = 0;
        r0 = sub_borrow(r0, d0, 0, b);
        r1 = r1 - d1 - b;
    }
    r1_out = r1;
    r0_out = r0;
    return q1;
}

// Exact odd inverse modulo 2^64 (Newton's method, 5 steps: 3->6->12->24->48->64 bits)
static inline uint64_t modinv_u64(uint64_t a) noexcept {
    uint64_t x = a; // valid mod 2^3 for odd a
    x *= 2ULL - a * x;
    x *= 2ULL - a * x;
    x *= 2ULL - a * x;
    x *= 2ULL - a * x;
    x *= 2ULL - a * x;
    return x;
}

// ============================================================================
// Section 2: Fast Limb Buffer Pool & Non-Moving Scratch Arena
// ============================================================================

struct LimbPool {
    static constexpr size_t NUM_BUCKETS = 6; // 8, 16, 32, 64, 128, 256 limbs
    static constexpr size_t MAX_PER_BUCKET = 32;
    struct Bucket {
        uint64_t* ptrs[MAX_PER_BUCKET];
        size_t count = 0;
    };
    Bucket buckets[NUM_BUCKETS];

    ~LimbPool() {
        for (size_t b = 0; b < NUM_BUCKETS; ++b) {
            for (size_t i = 0; i < buckets[b].count; ++i) {
                delete[] buckets[b].ptrs[i];
            }
        }
    }

    static inline size_t bucket_for(size_t n, uint32_t& cap_out) noexcept {
        if (n <= 8) { cap_out = 8; return 0; }
        if (n > 256) { cap_out = static_cast<uint32_t>(n); return NUM_BUCKETS; }
        size_t p = gcpp_bit_ceil(n);
        cap_out = static_cast<uint32_t>(p);
        return static_cast<size_t>(gcpp_countr_zero(p) - 3);
    }

    uint64_t* alloc(size_t n, uint32_t& cap_out) {
        size_t b = bucket_for(n, cap_out);
        if (b < NUM_BUCKETS && buckets[b].count > 0) {
            return buckets[b].ptrs[--buckets[b].count];
        }
        return new uint64_t[cap_out];
    }

    void free(uint64_t* p, uint32_t cap) noexcept {
        if (cap >= 8 && cap <= 256 && (cap & (cap - 1)) == 0) {
            size_t b = static_cast<size_t>(gcpp_countr_zero(cap) - 3);
            if (buckets[b].count < MAX_PER_BUCKET) {
                buckets[b].ptrs[buckets[b].count++] = p;
                return;
            }
        }
        delete[] p;
    }
};

static LimbPool g_limb_pool;

struct ScratchArena {
    struct Chunk {
        uint64_t* data;  // TeXmacs: owned, tm_new_array / tm_delete_array
        size_t cap = 0;
        size_t used = 0;
    };
    array<Chunk> chunks;

    ScratchArena() {
        (chunks << Chunk{tm_new_array<uint64_t>(131072), 131072, 0});
    }
    ~ScratchArena() {
        for (Chunk& ch : chunks) tm_delete_array(ch.data);
    }

    struct Mark {
        size_t chunk_idx;
        size_t used;
    };

    Mark mark() const noexcept {
        size_t idx = static_cast<size_t>(N(chunks)) - 1;
        return {idx, A(chunks)[idx].used};
    }

    void restore(Mark m) noexcept {
        for (size_t i = m.chunk_idx + 1; i < static_cast<size_t>(N(chunks)); ++i) {
            chunks[i].used = 0;
        }
        chunks[m.chunk_idx].used = m.used;
        while (static_cast<size_t>(N(chunks)) > m.chunk_idx + 1 && chunks[N(chunks) - 1].cap > 131072) {
            tm_delete_array(chunks[N(chunks) - 1].data);
            chunks->resize(N(chunks) - 1);
        }
    }

    uint64_t* alloc(size_t n) {
        if (n == 0) n = 1;
        for (size_t i = static_cast<size_t>(N(chunks)); i > 0; --i) {
            auto& ch = chunks[i - 1];
            if (ch.used + n <= ch.cap) {
                uint64_t* p = ch.data + ch.used;
                ch.used += n;
                return p;
            }
            if (ch.used > 0) break;
        }
        size_t new_cap = std::max(n * 2, size_t(131072));
        (chunks << Chunk{tm_new_array<uint64_t>(new_cap), new_cap, n});
        return chunks[N(chunks) - 1].data;
    }
};

static ScratchArena tl_scratch;

struct ScratchMark {
    ScratchArena::Mark saved;
    ScratchMark() noexcept : saved(tl_scratch.mark()) {}
    ~ScratchMark() noexcept { tl_scratch.restore(saved); }
};

// ============================================================================
// Section 3: Low-Level mpn Kernels (Little-Endian uint64_t Limbs)
// ============================================================================

static inline size_t mpn_norm(const uint64_t* a, size_t n) noexcept {
    while (n > 0 && a[n - 1] == 0) --n;
    return n;
}

static inline int mpn_cmp(const uint64_t* a, const uint64_t* b, size_t n) noexcept {
    for (size_t i = n; i > 0; --i) {
        if (a[i - 1] != b[i - 1]) return (a[i - 1] < b[i - 1]) ? -1 : 1;
    }
    return 0;
}

static inline int mpn_cmp_var(const uint64_t* a, size_t na, const uint64_t* b, size_t nb) noexcept {
    if (na != nb) return (na < nb) ? -1 : 1;
    return mpn_cmp(a, b, na);
}

static inline uint64_t mpn_add_n(uint64_t* r, const uint64_t* a, const uint64_t* b, size_t n) noexcept {
    uint64_t c = 0;
    for (size_t i = 0; i < n; ++i) {
        r[i] = add_carry(a[i], b[i], c, c);
    }
    return c;
}

static inline uint64_t mpn_sub_n(uint64_t* r, const uint64_t* a, const uint64_t* b, size_t n) noexcept {
    uint64_t br = 0;
    for (size_t i = 0; i < n; ++i) {
        r[i] = sub_borrow(a[i], b[i], br, br);
    }
    return br;
}

static inline uint64_t mpn_add_1(uint64_t* r, const uint64_t* a, size_t n, uint64_t w) noexcept {
    uint64_t c = w;
    size_t i = 0;
    for (; i < n && c != 0; ++i) {
        uint64_t s = a[i] + c;
        c = (s < a[i]) ? 1ULL : 0ULL;
        r[i] = s;
    }
    if (r != a && i < n) {
        std::memcpy(r + i, a + i, (n - i) * sizeof(uint64_t));
    }
    return c;
}

static inline uint64_t mpn_sub_1(uint64_t* r, const uint64_t* a, size_t n, uint64_t w) noexcept {
    uint64_t br = w;
    size_t i = 0;
    for (; i < n && br != 0; ++i) {
        uint64_t ai = a[i];
        r[i] = ai - br;
        br = (ai < br) ? 1ULL : 0ULL;
    }
    if (r != a && i < n) {
        std::memcpy(r + i, a + i, (n - i) * sizeof(uint64_t));
    }
    return br;
}

// Requires na >= nb
static inline uint64_t mpn_add(uint64_t* r, const uint64_t* a, size_t na, const uint64_t* b, size_t nb) noexcept {
    uint64_t c = mpn_add_n(r, a, b, nb);
    if (na > nb) return mpn_add_1(r + nb, a + nb, na - nb, c);
    return c;
}

// Requires na >= nb
static inline uint64_t mpn_sub(uint64_t* r, const uint64_t* a, size_t na, const uint64_t* b, size_t nb) noexcept {
    uint64_t br = mpn_sub_n(r, a, b, nb);
    if (na > nb) return mpn_sub_1(r + nb, a + nb, na - nb, br);
    return br;
}

// Shift left by 0 < cnt < 64 bits; returns shifted-out high bits. Allows r == a or r > a.
static inline uint64_t mpn_lshift(uint64_t* r, const uint64_t* a, size_t n, unsigned cnt) noexcept {
    if (n == 0) return 0;
    unsigned rcnt = 64 - cnt;
    uint64_t retval = a[n - 1] >> rcnt;
    for (size_t i = n - 1; i > 0; --i) {
        r[i] = (a[i] << cnt) | (a[i - 1] >> rcnt);
    }
    r[0] = a[0] << cnt;
    return retval;
}

// Shift right by 0 < cnt < 64 bits; returns shifted-out low bits. Allows r == a or r < a.
static inline uint64_t mpn_rshift(uint64_t* r, const uint64_t* a, size_t n, unsigned cnt) noexcept {
    if (n == 0) return 0;
    unsigned lcnt = 64 - cnt;
    uint64_t retval = a[0] << lcnt;
    for (size_t i = 0; i + 1 < n; ++i) {
        r[i] = (a[i] >> cnt) | (a[i + 1] << lcnt);
    }
    r[n - 1] = a[n - 1] >> cnt;
    return retval;
}

#if defined(__clang__) || defined(__GNUC__)
#define GUILE_ALWAYS_INLINE __attribute__((always_inline)) inline
#elif defined(_MSC_VER)
#define GUILE_ALWAYS_INLINE __forceinline
#else
#define GUILE_ALWAYS_INLINE inline
#endif

// r[0..n-1] = a[0..n-1] * w, returns carry out (r and a must not overlap)
static GUILE_ALWAYS_INLINE uint64_t mpn_mul_1(uint64_t* __restrict__ r, const uint64_t* __restrict__ a, size_t n, uint64_t w) noexcept {
#if defined(__SIZEOF_INT128__) && (defined(__clang__) || defined(__GNUC__))
    uint64_t c = 0;
    size_t i = 0;
#if defined(__clang__)
    for (; i + 8 <= n; i += 8) {
        u128 p0 = static_cast<u128>(a[i])     * w;
        u128 p1 = static_cast<u128>(a[i + 1]) * w;
        u128 p2 = static_cast<u128>(a[i + 2]) * w;
        u128 p3 = static_cast<u128>(a[i + 3]) * w;
        u128 p4 = static_cast<u128>(a[i + 4]) * w;
        u128 p5 = static_cast<u128>(a[i + 5]) * w;
        u128 p6 = static_cast<u128>(a[i + 6]) * w;
        u128 p7 = static_cast<u128>(a[i + 7]) * w;
        unsigned long long cf = 0;
        r[i]     = __builtin_addcll(static_cast<unsigned long long>(p0), c, 0, &cf);
        r[i + 1] = __builtin_addcll(static_cast<unsigned long long>(p1), static_cast<unsigned long long>(p0 >> 64), cf, &cf);
        r[i + 2] = __builtin_addcll(static_cast<unsigned long long>(p2), static_cast<unsigned long long>(p1 >> 64), cf, &cf);
        r[i + 3] = __builtin_addcll(static_cast<unsigned long long>(p3), static_cast<unsigned long long>(p2 >> 64), cf, &cf);
        r[i + 4] = __builtin_addcll(static_cast<unsigned long long>(p4), static_cast<unsigned long long>(p3 >> 64), cf, &cf);
        r[i + 5] = __builtin_addcll(static_cast<unsigned long long>(p5), static_cast<unsigned long long>(p4 >> 64), cf, &cf);
        r[i + 6] = __builtin_addcll(static_cast<unsigned long long>(p6), static_cast<unsigned long long>(p5 >> 64), cf, &cf);
        r[i + 7] = __builtin_addcll(static_cast<unsigned long long>(p7), static_cast<unsigned long long>(p6 >> 64), cf, &cf);
        c = static_cast<uint64_t>(p7 >> 64) + static_cast<uint64_t>(cf);
    }
#endif
    for (; i + 4 <= n; i += 4) {
        u128 p0 = static_cast<u128>(a[i])     * w;
        u128 p1 = static_cast<u128>(a[i + 1]) * w;
        u128 p2 = static_cast<u128>(a[i + 2]) * w;
        u128 p3 = static_cast<u128>(a[i + 3]) * w;
#if defined(__clang__)
        unsigned long long cf = 0;
        r[i]     = __builtin_addcll(static_cast<unsigned long long>(p0), c, 0, &cf);
        r[i + 1] = __builtin_addcll(static_cast<unsigned long long>(p1), static_cast<unsigned long long>(p0 >> 64), cf, &cf);
        r[i + 2] = __builtin_addcll(static_cast<unsigned long long>(p2), static_cast<unsigned long long>(p1 >> 64), cf, &cf);
        r[i + 3] = __builtin_addcll(static_cast<unsigned long long>(p3), static_cast<unsigned long long>(p2 >> 64), cf, &cf);
        c = static_cast<uint64_t>(p3 >> 64) + static_cast<uint64_t>(cf);
#else
        uint64_t cf = 0;
        r[i]     = add_carry(static_cast<uint64_t>(p0), c, 0, cf);
        r[i + 1] = add_carry(static_cast<uint64_t>(p1), static_cast<uint64_t>(p0 >> 64), cf, cf);
        r[i + 2] = add_carry(static_cast<uint64_t>(p2), static_cast<uint64_t>(p1 >> 64), cf, cf);
        r[i + 3] = add_carry(static_cast<uint64_t>(p3), static_cast<uint64_t>(p2 >> 64), cf, cf);
        c = static_cast<uint64_t>(p3 >> 64) + cf;
#endif
    }
    for (; i < n; ++i) {
        u128 p = static_cast<u128>(a[i]) * w + c;
        r[i] = static_cast<uint64_t>(p);
        c = static_cast<uint64_t>(p >> 64);
    }
    return c;
#else
    uint64_t c = 0;
    for (size_t i = 0; i < n; ++i) {
        uint64_t hi, lo = mul64(a[i], w, hi);
        uint64_t s = lo + c;
        c = hi + (s < lo ? 1ULL : 0ULL);
        r[i] = s;
    }
    return c;
#endif
}

// r[0..n-1] += a[0..n-1] * w, returns carry out (r and a must not overlap)
static GUILE_ALWAYS_INLINE uint64_t mpn_addmul_1(uint64_t* __restrict__ r, const uint64_t* __restrict__ a, size_t n, uint64_t w) noexcept {
#if defined(__SIZEOF_INT128__) && defined(__clang__)
    uint64_t c = 0;
    size_t i = 0;
    // 8-way zero-dep Chain-1 (r + lo(p)) + Chain-2 (u + [c, hi0..hi6]):
    // ONLY 9 cycles loop-carried latency per 8 limbs = 1.125 cycles/limb on ARM64!
    for (; i + 8 <= n; i += 8) {
        u128 p0 = static_cast<u128>(a[i])     * w;
        u128 p1 = static_cast<u128>(a[i + 1]) * w;
        u128 p2 = static_cast<u128>(a[i + 2]) * w;
        u128 p3 = static_cast<u128>(a[i + 3]) * w;
        u128 p4 = static_cast<u128>(a[i + 4]) * w;
        u128 p5 = static_cast<u128>(a[i + 5]) * w;
        u128 p6 = static_cast<u128>(a[i + 6]) * w;
        u128 p7 = static_cast<u128>(a[i + 7]) * w;
        unsigned long long c1 = 0, c2 = 0;
        uint64_t u0 = __builtin_addcll(r[i],     static_cast<unsigned long long>(p0), 0,  &c1);
        uint64_t u1 = __builtin_addcll(r[i + 1], static_cast<unsigned long long>(p1), c1, &c1);
        uint64_t u2 = __builtin_addcll(r[i + 2], static_cast<unsigned long long>(p2), c1, &c1);
        uint64_t u3 = __builtin_addcll(r[i + 3], static_cast<unsigned long long>(p3), c1, &c1);
        uint64_t u4 = __builtin_addcll(r[i + 4], static_cast<unsigned long long>(p4), c1, &c1);
        uint64_t u5 = __builtin_addcll(r[i + 5], static_cast<unsigned long long>(p5), c1, &c1);
        uint64_t u6 = __builtin_addcll(r[i + 6], static_cast<unsigned long long>(p6), c1, &c1);
        uint64_t u7 = __builtin_addcll(r[i + 7], static_cast<unsigned long long>(p7), c1, &c1);
        uint64_t hi7_c1 = static_cast<uint64_t>(p7 >> 64) + static_cast<uint64_t>(c1);
        r[i]     = __builtin_addcll(u0, c, 0, &c2);
        r[i + 1] = __builtin_addcll(u1, static_cast<unsigned long long>(p0 >> 64), c2, &c2);
        r[i + 2] = __builtin_addcll(u2, static_cast<unsigned long long>(p1 >> 64), c2, &c2);
        r[i + 3] = __builtin_addcll(u3, static_cast<unsigned long long>(p2 >> 64), c2, &c2);
        r[i + 4] = __builtin_addcll(u4, static_cast<unsigned long long>(p3 >> 64), c2, &c2);
        r[i + 5] = __builtin_addcll(u5, static_cast<unsigned long long>(p4 >> 64), c2, &c2);
        r[i + 6] = __builtin_addcll(u6, static_cast<unsigned long long>(p5 >> 64), c2, &c2);
        r[i + 7] = __builtin_addcll(u7, static_cast<unsigned long long>(p6 >> 64), c2, &c2);
        c = hi7_c1 + static_cast<uint64_t>(c2);
    }
    if (i + 4 <= n) {
        u128 p0 = static_cast<u128>(a[i])     * w;
        u128 p1 = static_cast<u128>(a[i + 1]) * w;
        u128 p2 = static_cast<u128>(a[i + 2]) * w;
        u128 p3 = static_cast<u128>(a[i + 3]) * w;
        unsigned long long c1 = 0, c2 = 0;
        uint64_t u0 = __builtin_addcll(r[i],     static_cast<unsigned long long>(p0), 0,  &c1);
        uint64_t u1 = __builtin_addcll(r[i + 1], static_cast<unsigned long long>(p1), c1, &c1);
        uint64_t u2 = __builtin_addcll(r[i + 2], static_cast<unsigned long long>(p2), c1, &c1);
        uint64_t u3 = __builtin_addcll(r[i + 3], static_cast<unsigned long long>(p3), c1, &c1);
        uint64_t hi3_c1 = static_cast<uint64_t>(p3 >> 64) + static_cast<uint64_t>(c1);
        r[i]     = __builtin_addcll(u0, c, 0, &c2);
        r[i + 1] = __builtin_addcll(u1, static_cast<unsigned long long>(p0 >> 64), c2, &c2);
        r[i + 2] = __builtin_addcll(u2, static_cast<unsigned long long>(p1 >> 64), c2, &c2);
        r[i + 3] = __builtin_addcll(u3, static_cast<unsigned long long>(p2 >> 64), c2, &c2);
        c = hi3_c1 + static_cast<uint64_t>(c2);
        i += 4;
    }
    for (; i < n; ++i) {
        u128 p = static_cast<u128>(a[i]) * w + r[i] + c;
        r[i] = static_cast<uint64_t>(p);
        c = static_cast<uint64_t>(p >> 64);
    }
    return c;
#elif defined(__SIZEOF_INT128__)
    uint64_t c = 0;
    for (size_t i = 0; i < n; ++i) {
        u128 p = static_cast<u128>(a[i]) * w + r[i] + c;
        r[i] = static_cast<uint64_t>(p);
        c = static_cast<uint64_t>(p >> 64);
    }
    return c;
#else
    uint64_t c = 0;
    for (size_t i = 0; i < n; ++i) {
        uint64_t hi, lo = mul64(a[i], w, hi);
        uint64_t c1 = 0, c2 = 0;
        lo = add_carry(lo, c, 0, c1);
        uint64_t s = add_carry(r[i], lo, 0, c2);
        r[i] = s;
        c = hi + c1 + c2;
    }
    return c;
#endif
}

// r[0..n-1] -= a[0..n-1] * w, returns borrow out (r and a must not overlap)
static inline uint64_t mpn_submul_1(uint64_t* __restrict__ r, const uint64_t* __restrict__ a, size_t n, uint64_t w) noexcept {
#if defined(__SIZEOF_INT128__) && defined(__clang__)
    uint64_t br = 0;
    size_t i = 0;
    for (; i + 4 <= n; i += 4) {
        u128 p0 = static_cast<u128>(a[i])     * w;
        u128 p1 = static_cast<u128>(a[i + 1]) * w;
        u128 p2 = static_cast<u128>(a[i + 2]) * w;
        u128 p3 = static_cast<u128>(a[i + 3]) * w;
        unsigned long long cf = 0;
        uint64_t s0 = __builtin_addcll(static_cast<unsigned long long>(p0), br, 0, &cf);
        uint64_t s1 = __builtin_addcll(static_cast<unsigned long long>(p1), static_cast<unsigned long long>(p0 >> 64), cf, &cf);
        uint64_t s2 = __builtin_addcll(static_cast<unsigned long long>(p2), static_cast<unsigned long long>(p1 >> 64), cf, &cf);
        uint64_t s3 = __builtin_addcll(static_cast<unsigned long long>(p3), static_cast<unsigned long long>(p2 >> 64), cf, &cf);
        unsigned long long bf = 0;
        r[i]     = __builtin_subcll(r[i],     s0, 0,  &bf);
        r[i + 1] = __builtin_subcll(r[i + 1], s1, bf, &bf);
        r[i + 2] = __builtin_subcll(r[i + 2], s2, bf, &bf);
        r[i + 3] = __builtin_subcll(r[i + 3], s3, bf, &bf);
        br = static_cast<uint64_t>(p3 >> 64) + static_cast<uint64_t>(cf) + static_cast<uint64_t>(bf);
    }
    for (; i < n; ++i) {
        u128 p = static_cast<u128>(a[i]) * w + br;
        uint64_t lo = static_cast<uint64_t>(p);
        br = static_cast<uint64_t>(p >> 64) + (r[i] < lo ? 1ULL : 0ULL);
        r[i] -= lo;
    }
    return br;
#elif defined(__SIZEOF_INT128__)
    uint64_t br = 0;
    for (size_t i = 0; i < n; ++i) {
        u128 p = static_cast<u128>(a[i]) * w + br;
        uint64_t lo = static_cast<uint64_t>(p);
        br = static_cast<uint64_t>(p >> 64) + (r[i] < lo ? 1ULL : 0ULL);
        r[i] -= lo;
    }
    return br;
#else
    uint64_t br = 0;
    for (size_t i = 0; i < n; ++i) {
        uint64_t hi, lo = mul64(a[i], w, hi);
        uint64_t c1 = 0, b2 = 0;
        lo = add_carry(lo, br, 0, c1);
        uint64_t d = sub_borrow(r[i], lo, 0, b2);
        r[i] = d;
        br = hi + c1 + b2;
    }
    return br;
#endif
}

// Exact division by 3: r[0..n-1] = a[0..n-1] / 3 (for exact multiples of 3; returns carry/remainder)
static inline uint64_t mpn_divexact_by3(uint64_t* r, const uint64_t* a, size_t n) noexcept {
    constexpr uint64_t INV3 = 0xAAAAAAAAAAAAAAABULL; // 3^-1 mod 2^64
    uint64_t borrow = 0;
    for (size_t i = 0; i < n; ++i) {
        uint64_t ai = a[i];
        uint64_t s = ai - borrow;
        uint64_t b1 = (ai < borrow) ? 1ULL : 0ULL;
        uint64_t q = s * INV3;
        r[i] = q;
        // High word of q * 3:
        // If q >= ceil(2^64/3) = 0x5555555555555556ULL, hi >= 1; if q >= ceil(2*2^64/3) = 0xAAAAAAAAAAAAAAABULL, hi = 2
        uint64_t hi = (q >= 0x5555555555555556ULL ? 1ULL : 0ULL) + (q >= 0xAAAAAAAAAAAAAAABULL ? 1ULL : 0ULL);
        borrow = hi + b1;
    }
    return borrow;
}

// ============================================================================
// Section 4: Multi-Tier Multiplication & Squaring
//   Basecase -> Karatsuba -> Toom-Cook 3 -> Radix-4 Double FFT -> 3-Prime NTT
// ============================================================================

static constexpr size_t KARATSUBA_MUL_THRESH = 28;
static constexpr size_t KARATSUBA_SQR_THRESH = 38;
static constexpr size_t TOOM3_MUL_THRESH     = 110;
static constexpr size_t TOOM3_SQR_THRESH     = 140;
static constexpr size_t FFT_MUL_THRESH       = 192;
static constexpr size_t FFT_SQR_THRESH       = 200;

static void mpn_mul_dispatch(uint64_t* r, const uint64_t* a, size_t na, const uint64_t* b, size_t nb);
static void mpn_sqr_dispatch(uint64_t* r, const uint64_t* a, size_t n);

static void mpn_mul_basecase(uint64_t* r, const uint64_t* a, size_t na, const uint64_t* b, size_t nb) noexcept {
    // Assumes na >= nb >= 1
    r[na] = mpn_mul_1(r, a, na, b[0]);
    for (size_t j = 1; j < nb; ++j) {
        r[na + j] = mpn_addmul_1(r + j, a, na, b[j]);
    }
}

static void mpn_sqr_basecase(uint64_t* __restrict__ r, const uint64_t* __restrict__ a, size_t n) noexcept {
    if (n == 1) {
        uint64_t hi;
        r[0] = mul64(a[0], a[0], hi);
        r[1] = hi;
        return;
    }
    // 1. Off-diagonal upper triangle: sum_{0 <= i < j < n} a[i]*a[j] * B^(i+j)
    r[0] = 0;
    r[n] = mpn_mul_1(r + 1, a + 1, n - 1, a[0]);
    for (size_t i = 1; i + 1 < n; ++i) {
        r[i + n] = mpn_addmul_1(r + 2 * i + 1, a + i + 1, n - 1 - i, a[i]);
    }
    // 2. Double the off-diagonal sum
    r[2 * n - 1] = mpn_lshift(r + 1, r + 1, 2 * n - 2, 1);
    // 3. Add diagonal squares a[i]^2 at positions 2*i, 2*i+1
    uint64_t c = 0;
    for (size_t i = 0; i < n; ++i) {
        uint64_t hi, lo = mul64(a[i], a[i], hi);
        uint64_t c1 = 0, c2 = 0;
        r[2 * i]     = add_carry(r[2 * i], lo, c, c1);
        r[2 * i + 1] = add_carry(r[2 * i + 1], hi, c1, c2);
        c = c2;
    }
}

// Karatsuba balanced/near-balanced multiply: na >= nb >= KARATSUBA_MUL_THRESH, na < 2*nb
static void mpn_mul_karatsuba(uint64_t* r, const uint64_t* a, size_t na, const uint64_t* b, size_t nb) {
    ScratchMark mark;
    size_t half = (na + 1) >> 1;
    // Note: half < nb because na < 2*nb (except if na == 2*nb - 1, half == nb -> fall back to slicing)
    if (half >= nb) {
        // Slice a into [0..nb-1] and [nb..na-1]
        mpn_mul_dispatch(r, a, nb, b, nb);
        size_t rem = na - nb;
        uint64_t* tmp = tl_scratch.alloc(rem + nb);
        if (rem >= nb) mpn_mul_dispatch(tmp, a + nb, rem, b, nb);
        else mpn_mul_dispatch(tmp, b, nb, a + nb, rem);
        uint64_t c = mpn_add_n(r + nb, r + nb, tmp, nb);
        if (rem > 0) {
            std::memcpy(r + 2 * nb, tmp + nb, rem * sizeof(uint64_t));
            mpn_add_1(r + 2 * nb, r + 2 * nb, rem, c);
        }
        return;
    }

    size_t na1 = na - half;
    size_t nb1 = nb - half;
    const uint64_t* a0 = a;
    const uint64_t* a1 = a + half;
    const uint64_t* b0 = b;
    const uint64_t* b1 = b + half;

    // z0 = a0 * b0 (2*half limbs) -> stored in r[0..2*half-1]
    mpn_mul_dispatch(r, a0, half, b0, half);
    // z2 = a1 * b1 (na1 + nb1 limbs) -> stored in r[2*half..na+nb-1]
    mpn_mul_dispatch(r + 2 * half, a1, na1, b1, nb1);

    // sa = a0 + a1 (half + 1 limbs), sb = b0 + b1 (half + 1 limbs)
    size_t s_len = half + 1;
    uint64_t* sa = tl_scratch.alloc(s_len);
    uint64_t* sb = tl_scratch.alloc(s_len);
    uint64_t* zm = tl_scratch.alloc(2 * s_len);

    sa[half] = mpn_add(sa, a0, half, a1, na1);
    sb[half] = mpn_add(sb, b0, half, b1, nb1);
    size_t sal = (sa[half] != 0) ? s_len : half;
    size_t sbl = (sb[half] != 0) ? s_len : half;
    size_t zml = sal + sbl;
    if (sal >= sbl) mpn_mul_dispatch(zm, sa, sal, sb, sbl);
    else mpn_mul_dispatch(zm, sb, sbl, sa, sal);

    // zm -= z0
    uint64_t br = mpn_sub(zm, zm, zml, r, 2 * half);
    (void)br;
    // zm -= z2
    mpn_sub(zm, zm, zml, r + 2 * half, na1 + nb1);

    // Add zm into r + half
    size_t zml_eff = mpn_norm(zm, zml);
    if (zml_eff > 0) {
        mpn_add(r + half, r + half, na + nb - half, zm, zml_eff);
    }
}

static void mpn_sqr_karatsuba(uint64_t* r, const uint64_t* a, size_t n) {
    ScratchMark mark;
    size_t half = (n + 1) >> 1;
    size_t n1 = n - half;
    const uint64_t* a0 = a;
    const uint64_t* a1 = a + half;

    // z0 = a0^2 in r[0..2*half-1]
    mpn_sqr_dispatch(r, a0, half);
    // z2 = a1^2 in r[2*half..2*n-1]
    mpn_sqr_dispatch(r + 2 * half, a1, n1);

    // sa = a0 + a1
    size_t s_len = half + 1;
    uint64_t* sa = tl_scratch.alloc(s_len);
    uint64_t* zm = tl_scratch.alloc(2 * s_len);
    sa[half] = mpn_add(sa, a0, half, a1, n1);
    size_t sal = (sa[half] != 0) ? s_len : half;
    size_t zml = 2 * sal;
    mpn_sqr_dispatch(zm, sa, sal);

    mpn_sub(zm, zm, zml, r, 2 * half);
    mpn_sub(zm, zm, zml, r + 2 * half, 2 * n1);

    size_t zml_eff = mpn_norm(zm, zml);
    if (zml_eff > 0) {
        mpn_add(r + half, r + half, 2 * n - half, zm, zml_eff);
    }
}

// Signed limb helper for Toom-3 interpolation
struct SignedSlice {
    uint64_t* p;
    size_t len;
    bool neg;
};

static inline void signed_add(uint64_t* dst, size_t& dlen, bool& dneg,
                              const uint64_t* a, size_t al, bool aneg,
                              const uint64_t* b, size_t bl, bool bneg) noexcept {
    al = mpn_norm(a, al);
    bl = mpn_norm(b, bl);
    if (al == 0) {
        if (bl > 0 && dst != b) std::memcpy(dst, b, bl * sizeof(uint64_t));
        dlen = bl;
        dneg = (bl > 0) && bneg;
        return;
    }
    if (bl == 0) {
        if (dst != a) std::memcpy(dst, a, al * sizeof(uint64_t));
        dlen = al;
        dneg = aneg;
        return;
    }
    if (aneg == bneg) {
        uint64_t c;
        size_t m = std::max(al, bl);
        if (al >= bl) c = mpn_add(dst, a, al, b, bl);
        else c = mpn_add(dst, b, bl, a, al);
        if (c) dst[m++] = c;
        dlen = m;
        dneg = aneg;
    } else {
        int cmp = mpn_cmp_var(a, al, b, bl);
        if (cmp == 0) {
            dlen = 0;
            dneg = false;
        } else if (cmp > 0) {
            mpn_sub(dst, a, al, b, bl);
            dlen = mpn_norm(dst, al);
            dneg = aneg;
        } else {
            mpn_sub(dst, b, bl, a, al);
            dlen = mpn_norm(dst, bl);
            dneg = bneg;
        }
    }
}

// Toom-Cook 3-way multiply (Bodrato interpolation): na >= nb, na < 2*nb, nb >= TOOM3_MUL_THRESH
static void mpn_mul_toom3(uint64_t* r, const uint64_t* a, size_t na, const uint64_t* b, size_t nb) {
    ScratchMark mark;
    size_t k = (na + 2) / 3;
    if (2 * k >= nb) {
        mpn_mul_karatsuba(r, a, na, b, nb);
        return;
    }
    // Split a = a0 + a1*B^k + a2*B^(2k), b = b0 + b1*B^k + b2*B^(2k)
    const uint64_t* a0 = a;          size_t na0 = k;
    const uint64_t* a1 = a + k;      size_t na1 = k;
    const uint64_t* a2 = a + 2 * k;  size_t na2 = na - 2 * k;
    const uint64_t* b0 = b;          size_t nb0 = k;
    const uint64_t* b1 = b + k;      size_t nb1 = k;
    const uint64_t* b2 = b + 2 * k;  size_t nb2 = nb - 2 * k;

    size_t el = k + 2; // evaluation limb capacity
    size_t pl = 2 * el; // product limb capacity

    uint64_t* ap0 = tl_scratch.alloc(el); // a0 + a2
    uint64_t* bp0 = tl_scratch.alloc(el); // b0 + b2
    uint64_t* am1 = tl_scratch.alloc(el); // a0 - a1 + a2
    uint64_t* bm1 = tl_scratch.alloc(el); // b0 - b1 + b2
    uint64_t* ap1 = tl_scratch.alloc(el); // a0 + a1 + a2
    uint64_t* bp1 = tl_scratch.alloc(el); // b0 + b1 + b2
    uint64_t* am2 = tl_scratch.alloc(el); // a0 - 2*a1 + 4*a2  (actually: (am1 + a2)*2 - a0 = a0 - 2a1 + 4a2? Wait: 2*(a0 - a1 + a2 + a2) - a0 = a0 - 2a1 + 4a2! YES!)
    uint64_t* bm2 = tl_scratch.alloc(el);

    // Evaluate A at 1, -1, -2
    size_t ap0_l = 0; bool ap0_n = false;
    signed_add(ap0, ap0_l, ap0_n, a0, na0, false, a2, na2, false);
    size_t ap1_l = 0; bool ap1_n = false;
    signed_add(ap1, ap1_l, ap1_n, ap0, ap0_l, false, a1, na1, false);
    size_t am1_l = 0; bool am1_n = false;
    signed_add(am1, am1_l, am1_n, ap0, ap0_l, false, a1, na1, true);
    // a(-2) = 2*(a(-1) + a2) - a0 = a0 - 2*a1 + 4*a2
    size_t am2_l = 0; bool am2_n = false;
    signed_add(am2, am2_l, am2_n, am1, am1_l, am1_n, a2, na2, false);
    if (am2_l > 0) {
        uint64_t c = mpn_lshift(am2, am2, am2_l, 1);
        if (c) am2[am2_l++] = c;
    }
    signed_add(am2, am2_l, am2_n, am2, am2_l, am2_n, a0, na0, true);

    // Evaluate B at 1, -1, -2
    size_t bp0_l = 0; bool bp0_n = false;
    signed_add(bp0, bp0_l, bp0_n, b0, nb0, false, b2, nb2, false);
    size_t bp1_l = 0; bool bp1_n = false;
    signed_add(bp1, bp1_l, bp1_n, bp0, bp0_l, false, b1, nb1, false);
    size_t bm1_l = 0; bool bm1_n = false;
    signed_add(bm1, bm1_l, bm1_n, bp0, bp0_l, false, b1, nb1, true);
    size_t bm2_l = 0; bool bm2_n = false;
    signed_add(bm2, bm2_l, bm2_n, bm1, bm1_l, bm1_n, b2, nb2, false);
    if (bm2_l > 0) {
        uint64_t c = mpn_lshift(bm2, bm2, bm2_l, 1);
        if (c) bm2[bm2_l++] = c;
    }
    signed_add(bm2, bm2_l, bm2_n, bm2, bm2_l, bm2_n, b0, nb0, true);

    auto mul_signed = [](uint64_t* dst, const uint64_t* x, size_t xl, bool xn,
                         const uint64_t* y, size_t yl, bool yn, size_t& ol, bool& on) {
        if (xl == 0 || yl == 0) { ol = 0; on = false; return; }
        if (xl >= yl) mpn_mul_dispatch(dst, x, xl, y, yl);
        else mpn_mul_dispatch(dst, y, yl, x, xl);
        ol = mpn_norm(dst, xl + yl);
        on = (ol > 0) && (xn != yn);
    };

    // 5 pointwise products: r0 = P(0), r1 = P(1), rm1 = P(-1), rm2 = P(-2), rinf = P(inf)
    uint64_t* r0   = tl_scratch.alloc(pl); size_t r0_l = 0;   bool r0_n = false;
    uint64_t* r1   = tl_scratch.alloc(pl); size_t r1_l = 0;   bool r1_n = false;
    uint64_t* rm1  = tl_scratch.alloc(pl); size_t rm1_l = 0;  bool rm1_n = false;
    uint64_t* rm2  = tl_scratch.alloc(pl); size_t rm2_l = 0;  bool rm2_n = false;
    uint64_t* rinf = tl_scratch.alloc(pl); size_t rinf_l = 0; bool rinf_n = false;

    mul_signed(r0,   a0,  mpn_norm(a0, na0), false, b0,  mpn_norm(b0, nb0), false, r0_l,   r0_n);
    mul_signed(r1,   ap1, ap1_l, false,             bp1, bp1_l, false,             r1_l,   r1_n);
    mul_signed(rm1,  am1, am1_l, am1_n,             bm1, bm1_l, bm1_n,             rm1_l,  rm1_n);
    mul_signed(rm2,  am2, am2_l, am2_n,             bm2, bm2_l, bm2_n,             rm2_l,  rm2_n);
    mul_signed(rinf, a2,  mpn_norm(a2, na2), false, b2,  mpn_norm(b2, nb2), false, rinf_l, rinf_n);

    // Bodrato's exact interpolation sequence:
    // Let c0 = r0, c4 = rinf
    // v3 = (rm2 - r1) / 3
    uint64_t* c3 = tl_scratch.alloc(pl); size_t c3_l = 0; bool c3_n = false;
    signed_add(c3, c3_l, c3_n, rm2, rm2_l, rm2_n, r1, r1_l, !r1_n);
    if (c3_l > 0) { mpn_divexact_by3(c3, c3, c3_l); c3_l = mpn_norm(c3, c3_l); }

    // c1 = (r1 - rm1) / 2
    uint64_t* c1 = tl_scratch.alloc(pl); size_t c1_l = 0; bool c1_n = false;
    signed_add(c1, c1_l, c1_n, r1, r1_l, r1_n, rm1, rm1_l, !rm1_n);
    if (c1_l > 0) { mpn_rshift(c1, c1, c1_l, 1); c1_l = mpn_norm(c1, c1_l); }

    // c2 = rm1 - r0
    uint64_t* c2 = tl_scratch.alloc(pl); size_t c2_l = 0; bool c2_n = false;
    signed_add(c2, c2_l, c2_n, rm1, rm1_l, rm1_n, r0, r0_l, !r0_n);

    // c3 = (c2 - c3) / 2 + 2 * rinf
    signed_add(c3, c3_l, c3_n, c2, c2_l, c2_n, c3, c3_l, !c3_n);
    if (c3_l > 0) { mpn_rshift(c3, c3, c3_l, 1); c3_l = mpn_norm(c3, c3_l); }
    if (rinf_l > 0) {
        uint64_t* tmp = tl_scratch.alloc(rinf_l + 1);
        uint64_t cy = mpn_lshift(tmp, rinf, rinf_l, 1);
        size_t tl = rinf_l;
        if (cy) tmp[tl++] = cy;
        signed_add(c3, c3_l, c3_n, c3, c3_l, c3_n, tmp, tl, false);
    }

    // c2 = c2 + c1 - rinf
    signed_add(c2, c2_l, c2_n, c2, c2_l, c2_n, c1, c1_l, c1_n);
    signed_add(c2, c2_l, c2_n, c2, c2_l, c2_n, rinf, rinf_l, true);

    // c1 = c1 - c3
    signed_add(c1, c1_l, c1_n, c1, c1_l, c1_n, c3, c3_l, !c3_n);

    // Recompose: r = r0 + c1*B^k + c2*B^(2k) + c3*B^(3k) + rinf*B^(4k)
    // All final coefficients c0, c1, c2, c3, c4 are guaranteed non-negative!
    size_t total = na + nb;
    std::memset(r, 0, total * sizeof(uint64_t));
    if (r0_l > 0) std::memcpy(r, r0, r0_l * sizeof(uint64_t));
    if (c1_l > 0) mpn_add(r + k, r + k, total - k, c1, c1_l);
    if (c2_l > 0) mpn_add(r + 2 * k, r + 2 * k, total - 2 * k, c2, c2_l);
    if (c3_l > 0 && 3 * k < total) mpn_add(r + 3 * k, r + 3 * k, total - 3 * k, c3, c3_l);
    if (rinf_l > 0 && 4 * k < total) mpn_add(r + 4 * k, r + 4 * k, total - 4 * k, rinf, rinf_l);
}

static void mpn_sqr_toom3(uint64_t* r, const uint64_t* a, size_t n) {
    ScratchMark mark;
    size_t k = (n + 2) / 3;
    if (2 * k >= n) {
        mpn_sqr_karatsuba(r, a, n);
        return;
    }
    const uint64_t* a0 = a;          size_t na0 = k;
    const uint64_t* a1 = a + k;      size_t na1 = k;
    const uint64_t* a2 = a + 2 * k;  size_t na2 = n - 2 * k;

    size_t el = k + 2;
    size_t pl = 2 * el;

    uint64_t* ap0 = tl_scratch.alloc(el);
    uint64_t* ap1 = tl_scratch.alloc(el);
    uint64_t* am1 = tl_scratch.alloc(el);
    uint64_t* am2 = tl_scratch.alloc(el);

    size_t ap0_l = 0; bool ap0_n = false;
    signed_add(ap0, ap0_l, ap0_n, a0, na0, false, a2, na2, false);
    size_t ap1_l = 0; bool ap1_n = false;
    signed_add(ap1, ap1_l, ap1_n, ap0, ap0_l, false, a1, na1, false);
    size_t am1_l = 0; bool am1_n = false;
    signed_add(am1, am1_l, am1_n, ap0, ap0_l, false, a1, na1, true);
    size_t am2_l = 0; bool am2_n = false;
    signed_add(am2, am2_l, am2_n, am1, am1_l, am1_n, a2, na2, false);
    if (am2_l > 0) {
        uint64_t c = mpn_lshift(am2, am2, am2_l, 1);
        if (c) am2[am2_l++] = c;
    }
    signed_add(am2, am2_l, am2_n, am2, am2_l, am2_n, a0, na0, true);

    auto sqr_u = [](uint64_t* dst, const uint64_t* x, size_t xl, size_t& ol) {
        if (xl == 0) { ol = 0; return; }
        mpn_sqr_dispatch(dst, x, xl);
        ol = mpn_norm(dst, 2 * xl);
    };

    uint64_t* r0   = tl_scratch.alloc(pl); size_t r0_l = 0;
    uint64_t* r1   = tl_scratch.alloc(pl); size_t r1_l = 0;
    uint64_t* rm1  = tl_scratch.alloc(pl); size_t rm1_l = 0;
    uint64_t* rm2  = tl_scratch.alloc(pl); size_t rm2_l = 0;
    uint64_t* rinf = tl_scratch.alloc(pl); size_t rinf_l = 0;

    sqr_u(r0,   a0,  mpn_norm(a0, na0), r0_l);
    sqr_u(r1,   ap1, ap1_l,             r1_l);
    sqr_u(rm1,  am1, am1_l,             rm1_l);
    sqr_u(rm2,  am2, am2_l,             rm2_l);
    sqr_u(rinf, a2,  mpn_norm(a2, na2), rinf_l);

    // Exact same Bodrato interpolation (all r* are non-negative!)
    uint64_t* c3 = tl_scratch.alloc(pl); size_t c3_l = 0; bool c3_n = false;
    signed_add(c3, c3_l, c3_n, rm2, rm2_l, false, r1, r1_l, true);
    if (c3_l > 0) { mpn_divexact_by3(c3, c3, c3_l); c3_l = mpn_norm(c3, c3_l); }

    uint64_t* c1 = tl_scratch.alloc(pl); size_t c1_l = 0; bool c1_n = false;
    signed_add(c1, c1_l, c1_n, r1, r1_l, false, rm1, rm1_l, true);
    if (c1_l > 0) { mpn_rshift(c1, c1, c1_l, 1); c1_l = mpn_norm(c1, c1_l); }

    uint64_t* c2 = tl_scratch.alloc(pl); size_t c2_l = 0; bool c2_n = false;
    signed_add(c2, c2_l, c2_n, rm1, rm1_l, false, r0, r0_l, true);

    signed_add(c3, c3_l, c3_n, c2, c2_l, c2_n, c3, c3_l, !c3_n);
    if (c3_l > 0) { mpn_rshift(c3, c3, c3_l, 1); c3_l = mpn_norm(c3, c3_l); }
    if (rinf_l > 0) {
        uint64_t* tmp = tl_scratch.alloc(rinf_l + 1);
        uint64_t cy = mpn_lshift(tmp, rinf, rinf_l, 1);
        size_t tl = rinf_l;
        if (cy) tmp[tl++] = cy;
        signed_add(c3, c3_l, c3_n, c3, c3_l, c3_n, tmp, tl, false);
    }

    signed_add(c2, c2_l, c2_n, c2, c2_l, c2_n, c1, c1_l, c1_n);
    signed_add(c2, c2_l, c2_n, c2, c2_l, c2_n, rinf, rinf_l, true);
    signed_add(c1, c1_l, c1_n, c1, c1_l, c1_n, c3, c3_l, !c3_n);

    size_t total = 2 * n;
    std::memset(r, 0, total * sizeof(uint64_t));
    if (r0_l > 0) std::memcpy(r, r0, r0_l * sizeof(uint64_t));
    if (c1_l > 0) mpn_add(r + k, r + k, total - k, c1, c1_l);
    if (c2_l > 0) mpn_add(r + 2 * k, r + 2 * k, total - 2 * k, c2, c2_l);
    if (c3_l > 0 && 3 * k < total) mpn_add(r + 3 * k, r + 3 * k, total - 3 * k, c3, c3_l);
    if (rinf_l > 0 && 4 * k < total) mpn_add(r + 4 * k, r + 4 * k, total - 4 * k, rinf, rinf_l);
}

// ============================================================================
// Section 4b: In-Place L1-Cache-Friendly Radix-4 DIF/DIT Real-to-Complex FFT
//   Zero bit-reversal, zero ping-pong buffers, L1-cache recursive depth-first,
//   unit-stride NEON/AVX2 vectorized butterflies, fused 0.25/M scaling.
//   Adaptive 16/15/14-bit balanced base: proven worst-case error < 0.16 << 0.5.
//   3-prime 64-bit NTT fallback for ultra-massive (> 7M bits) operands.
// ============================================================================

struct FFTPlan {
    size_t M = 0; // complex FFT size (= N / 2)
    array<double> w1r, w1i;
    // Bit-reversed real-to-complex unpack twiddles: rw_br[p] = exp(-pi*i * bitrev(p) / M)
    array<double> rwr_br, rwi_br;

    void init(size_t m) {
        M = m;
        constexpr double PI = 3.141592653589793238462643383279502884;
        size_t tsz = std::max(M, size_t(4));
        gcpp_assign(w1r, tsz, 1.0);
        gcpp_assign(w1i, tsz, 0.0);
        for (size_t q = 1; q * 4 <= M; q <<= 1) {
            double ang = -PI / static_cast<double>(2 * q); // -2*pi / (4*q)
            for (size_t j = 0; j < q; ++j) {
                double a = ang * static_cast<double>(j);
                w1r[q + j] = std::cos(a);
                w1i[q + j] = std::sin(a);
            }
        }
        gcpp_resize(rwr_br, M);
        gcpp_resize(rwi_br, M);
        unsigned logm = static_cast<unsigned>(gcpp_countr_zero(M));
        double rang = -PI / static_cast<double>(M);
        for (size_t p = 0; p < M; ++p) {
            size_t k = 0;
            for (unsigned b = 0; b < logm; ++b) {
                k = (k << 1) | ((p >> b) & 1);
            }
            double a = rang * static_cast<double>(k);
            rwr_br[p] = std::cos(a);
            rwi_br[p] = std::sin(a);
        }
    }
};

struct FFTCache {
    FFTPlan plans[22];
    array<double> ar, ai;
    array<double> br, bi;

    const FFTPlan& get(size_t M) {
        unsigned k = static_cast<unsigned>(gcpp_countr_zero(M));
        if (plans[k].M != M) plans[k].init(M);
        return plans[k];
    }

    void ensure(size_t M, bool need_b) {
        if (static_cast<size_t>(N(ar)) < M) { gcpp_resize(ar, M); gcpp_resize(ai, M); }
        if (need_b && static_cast<size_t>(N(br)) < M) { gcpp_resize(br, M); gcpp_resize(bi, M); }
    }
};

static FFTCache g_fft;

static void fft_dif_inplace(double* __restrict__ ar, double* __restrict__ ai,
                            size_t len, const FFTPlan& plan) noexcept {
    if (len == 1) return;
    if (len == 2) {
        double r0 = ar[0], i0 = ai[0], r1 = ar[1], i1 = ai[1];
        ar[0] = r0 + r1; ai[0] = i0 + i1;
        ar[1] = r0 - r1; ai[1] = i0 - i1;
        return;
    }
    if (len == 4) {
        double r0 = ar[0], i0 = ai[0], r1 = ar[1], i1 = ai[1];
        double r2 = ar[2], i2 = ai[2], r3 = ar[3], i3 = ai[3];
        double t0r = r0 + r2, t0i = i0 + i2;
        double t1r = r0 - r2, t1i = i0 - i2;
        double t2r = r1 + r3, t2i = i1 + i3;
        double t3r = r1 - r3, t3i = i1 - i3;
        ar[0] = t0r + t2r; ai[0] = t0i + t2i;
        ar[1] = t0r - t2r; ai[1] = t0i - t2i;
        ar[2] = t1r + t3i; ai[2] = t1i - t3r;
        ar[3] = t1r - t3i; ai[3] = t1i + t3r;
        return;
    }

    const double* __restrict__ w1r_all = gcpp_data(plan.w1r);
    const double* __restrict__ w1i_all = gcpp_data(plan.w1i);

    if (len > 2048) {
        size_t q = len >> 2;
        double* __restrict__ x0r = ar;
        double* __restrict__ x1r = ar + q;
        double* __restrict__ x2r = ar + 2 * q;
        double* __restrict__ x3r = ar + 3 * q;
        double* __restrict__ x0i = ai;
        double* __restrict__ x1i = ai + q;
        double* __restrict__ x2i = ai + 2 * q;
        double* __restrict__ x3i = ai + 3 * q;
        const double* __restrict__ wr = w1r_all + q;
        const double* __restrict__ wi = w1i_all + q;

        for (size_t j = 0; j < q; ++j) {
            double r0 = x0r[j], i0 = x0i[j];
            double r1 = x1r[j], i1 = x1i[j];
            double r2 = x2r[j], i2 = x2i[j];
            double r3 = x3r[j], i3 = x3i[j];

            double t0r = r0 + r2, t0i = i0 + i2;
            double t1r = r0 - r2, t1i = i0 - i2;
            double t2r = r1 + r3, t2i = i1 + i3;
            double t3r = r1 - r3, t3i = i1 - i3;

            double u0r = t0r + t2r, u0i = t0i + t2i;
            double u2r = t0r - t2r, u2i = t0i - t2i;
            double u1r = t1r + t3i, u1i = t1i - t3r;
            double u3r = t1r - t3i, u3i = t1i + t3r;

            double c1 = wr[j], s1 = wi[j];
            double c2 = c1 * c1 - s1 * s1, s2 = 2.0 * c1 * s1;
            double c3 = c2 * c1 - s2 * s1, s3 = c2 * s1 + s2 * c1;

            x0r[j] = u0r;                    x0i[j] = u0i;
            x1r[j] = u2r * c2 - u2i * s2;    x1i[j] = u2r * s2 + u2i * c2;
            x2r[j] = u1r * c1 - u1i * s1;    x2i[j] = u1r * s1 + u1i * c1;
            x3r[j] = u3r * c3 - u3i * s3;    x3i[j] = u3r * s3 + u3i * c3;
        }
        fft_dif_inplace(x0r, x0i, q, plan);
        fft_dif_inplace(x1r, x1i, q, plan);
        fft_dif_inplace(x2r, x2i, q, plan);
        fft_dif_inplace(x3r, x3i, q, plan);
        return;
    }

    // L1-cache-resident iterative Radix-4 DIF loop (len <= 2048, computes w2/w3 in FMA registers!)
    size_t m = len;
    for (; m >= 8; m >>= 2) {
        size_t q = m >> 2;
        const double* __restrict__ wr = w1r_all + q;
        const double* __restrict__ wi = w1i_all + q;
        for (size_t base = 0; base < len; base += m) {
            double* __restrict__ x0r = ar + base;
            double* __restrict__ x1r = x0r + q;
            double* __restrict__ x2r = x0r + 2 * q;
            double* __restrict__ x3r = x0r + 3 * q;
            double* __restrict__ x0i = ai + base;
            double* __restrict__ x1i = x0i + q;
            double* __restrict__ x2i = x0i + 2 * q;
            double* __restrict__ x3i = x0i + 3 * q;

            for (size_t j = 0; j < q; ++j) {
                double r0 = x0r[j], i0 = x0i[j];
                double r1 = x1r[j], i1 = x1i[j];
                double r2 = x2r[j], i2 = x2i[j];
                double r3 = x3r[j], i3 = x3i[j];

                double t0r = r0 + r2, t0i = i0 + i2;
                double t1r = r0 - r2, t1i = i0 - i2;
                double t2r = r1 + r3, t2i = i1 + i3;
                double t3r = r1 - r3, t3i = i1 - i3;

                double u0r = t0r + t2r, u0i = t0i + t2i;
                double u2r = t0r - t2r, u2i = t0i - t2i;
                double u1r = t1r + t3i, u1i = t1i - t3r;
                double u3r = t1r - t3i, u3i = t1i + t3r;

                double c1 = wr[j], s1 = wi[j];
                double c2 = c1 * c1 - s1 * s1, s2 = 2.0 * c1 * s1;
                double c3 = c2 * c1 - s2 * s1, s3 = c2 * s1 + s2 * c1;

                x0r[j] = u0r;                    x0i[j] = u0i;
                x1r[j] = u2r * c2 - u2i * s2;    x1i[j] = u2r * s2 + u2i * c2;
                x2r[j] = u1r * c1 - u1i * s1;    x2i[j] = u1r * s1 + u1i * c1;
                x3r[j] = u3r * c3 - u3i * s3;    x3i[j] = u3r * s3 + u3i * c3;
            }
        }
    }
    if (m == 4) {
        for (size_t base = 0; base < len; base += 4) {
            double r0 = ar[base],     i0 = ai[base];
            double r1 = ar[base + 1], i1 = ai[base + 1];
            double r2 = ar[base + 2], i2 = ai[base + 2];
            double r3 = ar[base + 3], i3 = ai[base + 3];
            double t0r = r0 + r2, t0i = i0 + i2;
            double t1r = r0 - r2, t1i = i0 - i2;
            double t2r = r1 + r3, t2i = i1 + i3;
            double t3r = r1 - r3, t3i = i1 - i3;
            ar[base]     = t0r + t2r; ai[base]     = t0i + t2i;
            ar[base + 1] = t0r - t2r; ai[base + 1] = t0i - t2i;
            ar[base + 2] = t1r + t3i; ai[base + 2] = t1i - t3r;
            ar[base + 3] = t1r - t3i; ai[base + 3] = t1i + t3r;
        }
    } else if (m == 2) {
        for (size_t base = 0; base < len; base += 2) {
            double r0 = ar[base],     i0 = ai[base];
            double r1 = ar[base + 1], i1 = ai[base + 1];
            ar[base]     = r0 + r1; ai[base]     = i0 + i1;
            ar[base + 1] = r0 - r1; ai[base + 1] = i0 - i1;
        }
    }
}

// In-place Radix-4 DIT inverse FFT (exact inverse of fft_dif_inplace, unscaled!)
static void fft_dit_inv_inplace(double* __restrict__ ar, double* __restrict__ ai,
                                size_t len, const FFTPlan& plan) noexcept {
    if (len == 1) return;
    if (len == 2) {
        double r0 = ar[0], i0 = ai[0], r1 = ar[1], i1 = ai[1];
        ar[0] = r0 + r1; ai[0] = i0 + i1;
        ar[1] = r0 - r1; ai[1] = i0 - i1;
        return;
    }
    if (len == 4) {
        double r0 = ar[0], i0 = ai[0], r2 = ar[1], i2 = ai[1];
        double r1 = ar[2], i1 = ai[2], r3 = ar[3], i3 = ai[3];
        double t0r = r0 + r2, t0i = i0 + i2;
        double t1r = r0 - r2, t1i = i0 - i2;
        double t2r = r1 + r3, t2i = i1 + i3;
        double t3r = r1 - r3, t3i = i1 - i3;
        ar[0] = t0r + t2r; ai[0] = t0i + t2i;
        ar[2] = t0r - t2r; ai[2] = t0i - t2i;
        ar[1] = t1r - t3i; ai[1] = t1i + t3r;
        ar[3] = t1r + t3i; ai[3] = t1i - t3r;
        return;
    }

    const double* __restrict__ w1r_all = gcpp_data(plan.w1r);
    const double* __restrict__ w1i_all = gcpp_data(plan.w1i);

    if (len > 2048) {
        size_t q = len >> 2;
        double* __restrict__ x0r = ar;
        double* __restrict__ x1r = ar + q;
        double* __restrict__ x2r = ar + 2 * q;
        double* __restrict__ x3r = ar + 3 * q;
        double* __restrict__ x0i = ai;
        double* __restrict__ x1i = ai + q;
        double* __restrict__ x2i = ai + 2 * q;
        double* __restrict__ x3i = ai + 3 * q;

        fft_dit_inv_inplace(x0r, x0i, q, plan);
        fft_dit_inv_inplace(x1r, x1i, q, plan);
        fft_dit_inv_inplace(x2r, x2i, q, plan);
        fft_dit_inv_inplace(x3r, x3i, q, plan);

        const double* __restrict__ wr = w1r_all + q;
        const double* __restrict__ wi = w1i_all + q;
        for (size_t j = 0; j < q; ++j) {
            double c1 = wr[j], s1 = -wi[j];
            double c2 = c1 * c1 - s1 * s1, s2 = 2.0 * c1 * s1;
            double c3 = c2 * c1 - s2 * s1, s3 = c2 * s1 + s2 * c1;

            double b0r = x0r[j], b0i = x0i[j];
            double a1r = x1r[j], a1i = x1i[j];
            double a2r = x2r[j], a2i = x2i[j];
            double a3r = x3r[j], a3i = x3i[j];

            double b2r = a1r * c2 - a1i * s2, b2i = a1r * s2 + a1i * c2;
            double b1r = a2r * c1 - a2i * s1, b1i = a2r * s1 + a2i * c1;
            double b3r = a3r * c3 - a3i * s3, b3i = a3r * s3 + a3i * c3;

            double t0r = b0r + b2r, t0i = b0i + b2i;
            double t1r = b0r - b2r, t1i = b0i - b2i;
            double t2r = b1r + b3r, t2i = b1i + b3i;
            double t3r = b1r - b3r, t3i = b1i - b3i;

            x0r[j] = t0r + t2r; x0i[j] = t0i + t2i;
            x2r[j] = t0r - t2r; x2i[j] = t0i - t2i;
            x1r[j] = t1r - t3i; x1i[j] = t1i + t3r;
            x3r[j] = t1r + t3i; x3i[j] = t1i - t3r;
        }
        return;
    }

    unsigned loglen = static_cast<unsigned>(gcpp_countr_zero(len));
    size_t m;
    if (loglen & 1) {
        for (size_t base = 0; base < len; base += 2) {
            double r0 = ar[base],     i0 = ai[base];
            double r1 = ar[base + 1], i1 = ai[base + 1];
            ar[base]     = r0 + r1; ai[base]     = i0 + i1;
            ar[base + 1] = r0 - r1; ai[base + 1] = i0 - i1;
        }
        m = 8;
    } else {
        for (size_t base = 0; base < len; base += 4) {
            double r0 = ar[base],     i0 = ai[base];
            double r2 = ar[base + 1], i2 = ai[base + 1];
            double r1 = ar[base + 2], i1 = ai[base + 2];
            double r3 = ar[base + 3], i3 = ai[base + 3];
            double t0r = r0 + r2, t0i = i0 + i2;
            double t1r = r0 - r2, t1i = i0 - i2;
            double t2r = r1 + r3, t2i = i1 + i3;
            double t3r = r1 - r3, t3i = i1 - i3;
            ar[base]     = t0r + t2r; ai[base]     = t0i + t2i;
            ar[base + 2] = t0r - t2r; ai[base + 2] = t0i - t2i;
            ar[base + 1] = t1r - t3i; ai[base + 1] = t1i + t3r;
            ar[base + 3] = t1r + t3i; ai[base + 3] = t1i - t3r;
        }
        m = 16;
    }
    for (; m <= len; m <<= 2) {
        size_t q = m >> 2;
        const double* __restrict__ wr = w1r_all + q;
        const double* __restrict__ wi = w1i_all + q;
        for (size_t base = 0; base < len; base += m) {
            double* __restrict__ x0r = ar + base;
            double* __restrict__ x1r = x0r + q;
            double* __restrict__ x2r = x0r + 2 * q;
            double* __restrict__ x3r = x0r + 3 * q;
            double* __restrict__ x0i = ai + base;
            double* __restrict__ x1i = x0i + q;
            double* __restrict__ x2i = x0i + 2 * q;
            double* __restrict__ x3i = x0i + 3 * q;

            for (size_t j = 0; j < q; ++j) {
                double c1 = wr[j], s1 = -wi[j];
                double c2 = c1 * c1 - s1 * s1, s2 = 2.0 * c1 * s1;
                double c3 = c2 * c1 - s2 * s1, s3 = c2 * s1 + s2 * c1;

                double b0r = x0r[j], b0i = x0i[j];
                double a1r = x1r[j], a1i = x1i[j];
                double a2r = x2r[j], a2i = x2i[j];
                double a3r = x3r[j], a3i = x3i[j];

                double b2r = a1r * c2 - a1i * s2, b2i = a1r * s2 + a1i * c2;
                double b1r = a2r * c1 - a2i * s1, b1i = a2r * s1 + a2i * c1;
                double b3r = a3r * c3 - a3i * s3, b3i = a3r * s3 + a3i * c3;

                double t0r = b0r + b2r, t0i = b0i + b2i;
                double t1r = b0r - b2r, t1i = b0i - b2i;
                double t2r = b1r + b3r, t2i = b1i + b3i;
                double t3r = b1r - b3r, t3i = b1i - b3i;

                x0r[j] = t0r + t2r; x0i[j] = t0i + t2i;
                x2r[j] = t0r - t2r; x2i[j] = t0i - t2i;
                x1r[j] = t1r - t3i; x1i[j] = t1i + t3r;
                x3r[j] = t1r + t3i; x3i[j] = t1i - t3r;
            }
        }
    }
}

// Decompose limbs[0..na-1] into balanced signed b-bit chunks packed into SoA (or_[j], oi_[j]) = (real[2j], real[2j+1]).
// Only zero-fills the unused tail [written .. M] (cuts memset in half!).
static void limbs_to_fft_soa(const uint64_t* a, size_t na, int bits,
                             double* __restrict__ or_, double* __restrict__ oi_, size_t M) noexcept {
    const int64_t half = 1LL << (bits - 1);
    const int64_t base = 1LL << bits;
    const uint64_t mask = static_cast<uint64_t>(base - 1);

    if (bits == 16) {
        int64_t carry = 0;
        size_t idx = 0;
        // Branchless balanced 16-bit decomposition: (d + 32768) >> 16 is 1 iff d >= 32768!
        for (size_t i = 0; i + 1 < na; ++i) {
            uint64_t w = a[i];
            int64_t d0 = static_cast<int64_t>(w & 0xFFFFULL) + carry;
            carry = (d0 + 32768) >> 16;
            d0 -= carry << 16;

            int64_t d1 = static_cast<int64_t>((w >> 16) & 0xFFFFULL) + carry;
            carry = (d1 + 32768) >> 16;
            d1 -= carry << 16;

            or_[idx] = static_cast<double>(d0);
            oi_[idx] = static_cast<double>(d1);
            ++idx;

            int64_t d2 = static_cast<int64_t>((w >> 32) & 0xFFFFULL) + carry;
            carry = (d2 + 32768) >> 16;
            d2 -= carry << 16;

            int64_t d3 = static_cast<int64_t>(w >> 48) + carry;
            carry = (d3 + 32768) >> 16;
            d3 -= carry << 16;

            or_[idx] = static_cast<double>(d2);
            oi_[idx] = static_cast<double>(d3);
            ++idx;
        }
        if (na > 0) {
            uint64_t w = a[na - 1];
            int64_t d0 = static_cast<int64_t>(w & 0xFFFFULL) + carry;
            carry = (d0 + 32768) >> 16;
            d0 -= carry << 16;

            int64_t d1 = static_cast<int64_t>((w >> 16) & 0xFFFFULL) + carry;
            carry = (d1 + 32768) >> 16;
            d1 -= carry << 16;

            or_[idx] = static_cast<double>(d0);
            oi_[idx] = static_cast<double>(d1);
            ++idx;

            int64_t d2 = static_cast<int64_t>((w >> 32) & 0xFFFFULL) + carry;
            carry = (d2 + 32768) >> 16;
            d2 -= carry << 16;

            int64_t d3 = static_cast<int64_t>(w >> 48) + carry;
            if (idx + 1 < M) {
                carry = (d3 + 32768) >> 16;
                d3 -= carry << 16;
            } else {
                carry = 0;
            }
            or_[idx] = static_cast<double>(d2);
            oi_[idx] = static_cast<double>(d3);
            ++idx;
        }
        if (carry != 0) {
            or_[idx] = static_cast<double>(carry);
            oi_[idx] = 0.0;
            ++idx;
        }
        if (idx < M) {
            std::memset(or_ + idx, 0, (M - idx) * sizeof(double));
            std::memset(oi_ + idx, 0, (M - idx) * sizeof(double));
        }
        return;
    }

    // General b-bit extraction (b = 14 or 15)
    std::memset(or_, 0, M * sizeof(double));
    std::memset(oi_, 0, M * sizeof(double));
    uint64_t bitbuf = 0;
    int avail = 0;
    size_t limb_i = 0;
    int64_t carry = 0;
    size_t pos = 0;
    size_t max_pos = 2 * M;
    auto emit = [&](int64_t v) {
        if ((pos & 1) == 0) or_[pos >> 1] = static_cast<double>(v);
        else oi_[pos >> 1] = static_cast<double>(v);
        ++pos;
    };
    while (limb_i < na || avail > 0 || carry != 0) {
        uint64_t chunk;
        if (avail < bits && limb_i < na) {
            uint64_t w = a[limb_i++];
            chunk = (bitbuf | (w << avail)) & mask;
            int used = bits - avail;
            bitbuf = (used < 64) ? (w >> used) : 0ULL;
            avail = 64 - used;
        } else {
            chunk = bitbuf & mask;
            bitbuf >>= bits;
            avail = std::max(0, avail - bits);
        }
        int64_t d = static_cast<int64_t>(chunk) + carry;
        if (pos + 1 < max_pos && d >= half) { d -= base; carry = 1; } else carry = 0;
        emit(d);
    }
}

static void fft_soa_to_limbs(const double* __restrict__ in_r, const double* __restrict__ in_i,
                             size_t M, int bits, uint64_t* __restrict__ r, size_t rn) noexcept {
    const int64_t mask = (1LL << bits) - 1;
    if (bits == 16) {
        int64_t carry = 0;
        for (size_t i = 0; i < rn; ++i) {
            size_t j = 2 * i;
            int64_t v0 = std::llround(in_r[j]) + carry;
            uint64_t d0 = static_cast<uint64_t>(v0 & 0xFFFFLL);
            carry = v0 >> 16;

            int64_t v1 = std::llround(in_i[j]) + carry;
            uint64_t d1 = static_cast<uint64_t>(v1 & 0xFFFFLL);
            carry = v1 >> 16;

            int64_t v2 = std::llround(in_r[j + 1]) + carry;
            uint64_t d2 = static_cast<uint64_t>(v2 & 0xFFFFLL);
            carry = v2 >> 16;

            int64_t v3 = std::llround(in_i[j + 1]) + carry;
            uint64_t d3 = static_cast<uint64_t>(v3 & 0xFFFFLL);
            carry = v3 >> 16;

            r[i] = d0 | (d1 << 16) | (d2 << 32) | (d3 << 48);
        }
        return;
    }

    int64_t carry = 0;
    uint64_t bitbuf = 0;
    int fill = 0;
    size_t out_i = 0;
    size_t total_chunks = 2 * M;
    for (size_t k = 0; k < total_chunks && out_i < rn; ++k) {
        double raw = ((k & 1) == 0) ? in_r[k >> 1] : in_i[k >> 1];
        int64_t v = std::llround(raw) + carry;
        uint64_t d = static_cast<uint64_t>(v & mask);
        carry = v >> bits;
        bitbuf |= (d << fill);
        fill += bits;
        if (fill >= 64) {
            r[out_i++] = bitbuf;
            fill -= 64;
            bitbuf = (fill > 0) ? (d >> (bits - fill)) : 0ULL;
        }
    }
    if (out_i < rn) {
        r[out_i++] = bitbuf;
        while (out_i < rn) r[out_i++] = 0;
    }
}

// Disjoint __restrict__ helpers so compiler auto-vectorizes real-FFT unpack/convolve/repack into 2-wide SIMD!
static void real_fft_sqr_blk(double* __restrict__ arp, double* __restrict__ aip,
                             double* __restrict__ arq, double* __restrict__ aiq,
                             const double* __restrict__ rwrp, const double* __restrict__ rwip,
                             size_t half_blk, double scale_pair) noexcept {
    for (size_t j = 0; j < half_blk; ++j) {
        size_t qi = half_blk - 1 - j;
        double apr = arp[j], api = aip[j];
        double aqr = arq[qi], aqi = aiq[qi];
        double twr = rwrp[j], twi = rwip[j];

        double er = apr + aqr, ei = api - aqi;
        double dr = apr - aqr, di = api + aqi;
        double or_ = di * twr + dr * twi;
        double oi_ = di * twi - dr * twr;

        double xkr  = er + or_, xki  = ei + oi_;
        double xmkr = er - or_, xmki = -(ei - oi_);

        double ckr  = xkr * xkr - xki * xki;
        double cki  = 2.0 * xkr * xki;
        double cmkr = xmkr * xmkr - xmki * xmki;
        double cmki = 2.0 * xmkr * xmki;

        double per = scale_pair * (ckr + cmkr);
        double pei = scale_pair * (cki - cmki);
        double pdr = scale_pair * (ckr - cmkr);
        double pdi = scale_pair * (cki + cmki);
        double por = -pdi * twr + pdr * twi;
        double poi =  pdr * twr + pdi * twi;

        arp[j]  = per + por; aip[j]  = pei + poi;
        arq[qi] = per - por; aiq[qi] = -(pei - poi);
    }
}

static void real_fft_mul_blk(double* __restrict__ arp, double* __restrict__ aip,
                             double* __restrict__ arq, double* __restrict__ aiq,
                             const double* __restrict__ brp, const double* __restrict__ bip,
                             const double* __restrict__ brq, const double* __restrict__ biq,
                             const double* __restrict__ rwrp, const double* __restrict__ rwip,
                             size_t half_blk, double scale_pair) noexcept {
    for (size_t j = 0; j < half_blk; ++j) {
        size_t qi = half_blk - 1 - j;
        double twr = rwrp[j], twi = rwip[j];

        double apr = arp[j], api = aip[j], aqr = arq[qi], aqi = aiq[qi];
        double aer = apr + aqr, aei = api - aqi;
        double adr = apr - aqr, adi = api + aqi;
        double aor = adi * twr + adr * twi, aoi = adi * twi - adr * twr;
        double xkr = aer + aor, xki = aei + aoi;
        double xmkr = aer - aor, xmki = -(aei - aoi);

        double bpr = brp[j], bpi = bip[j], bqr = brq[qi], bqi = biq[qi];
        double ber = bpr + bqr, bei = bpi - bqi;
        double bdr = bpr - bqr, bdi = bpi + bqi;
        double bor = bdi * twr + bdr * twi, boi = bdi * twi - bdr * twr;
        double ykr = ber + bor, yki = bei + boi;
        double ymkr = ber - bor, ymki = -(bei - boi);

        double ckr  = xkr * ykr - xki * yki;
        double cki  = xkr * yki + xki * ykr;
        double cmkr = xmkr * ymkr - xmki * ymki;
        double cmki = xmkr * ymki + xmki * ymkr;

        double per = scale_pair * (ckr + cmkr);
        double pei = scale_pair * (cki - cmki);
        double pdr = scale_pair * (ckr - cmkr);
        double pdi = scale_pair * (cki + cmki);
        double por = -pdi * twr + pdr * twi;
        double poi =  pdr * twr + pdi * twi;

        arp[j]  = per + por; aip[j]  = pei + poi;
        arq[qi] = per - por; aiq[qi] = -(pei - poi);
    }
}

// 3-prime 64-bit NTT fallback for ultra-massive integers (> 7M bits)
static void mpn_mul_ntt3(uint64_t* r, const uint64_t* a, size_t na, const uint64_t* b, size_t nb);

static bool mpn_mul_fft(uint64_t* r, const uint64_t* a, size_t na, const uint64_t* b, size_t nb, bool is_sqr) {
    if constexpr (std::numeric_limits<double>::digits != 53) {
        mpn_mul_ntt3(r, a, na, b, nb);
        return true;
    }
    size_t na_eff = mpn_norm(a, na);
    size_t nb_eff = is_sqr ? na_eff : mpn_norm(b, nb);
    if (na_eff == 0 || nb_eff == 0) {
        std::memset(r, 0, (na + nb) * sizeof(uint64_t));
        return true;
    }
    size_t bits_a = (na_eff - 1) * 64 + static_cast<size_t>(64 - gcpp_countl_zero(a[na_eff - 1]));
    size_t bits_b = is_sqr ? bits_a : ((nb_eff - 1) * 64 + static_cast<size_t>(64 - gcpp_countl_zero(b[nb_eff - 1])));

    int bits = 0;
    size_t N = 0;
    auto try_bits = [&](int b_width, size_t max_N) -> bool {
        size_t ca = (bits_a + static_cast<size_t>(b_width) - 1) / static_cast<size_t>(b_width);
        size_t cb = (bits_b + static_cast<size_t>(b_width) - 1) / static_cast<size_t>(b_width);
        size_t out_chunks = (64 * (na + nb) + static_cast<size_t>(b_width) - 1) / static_cast<size_t>(b_width);
        size_t need = std::max(ca + cb, out_chunks);
        size_t n_fft = 1;
        while (n_fft < need) n_fft <<= 1;
        if (n_fft <= max_N) {
            bits = b_width;
            N = n_fft;
            return true;
        }
        return false;
    };
    if (!try_bits(16, 1ULL << 17)) {
        if (!try_bits(15, 1ULL << 18)) {
            if (!try_bits(14, 1ULL << 20)) {
                mpn_mul_ntt3(r, a, na, b, nb);
                return true;
            }
        }
    }

    size_t M = N >> 1;
    const FFTPlan& plan = g_fft.get(M);
    g_fft.ensure(M, !is_sqr);
    double* __restrict__ ar = gcpp_data(g_fft.ar);
    double* __restrict__ ai = gcpp_data(g_fft.ai);

    limbs_to_fft_soa(a, na_eff, bits, ar, ai, M);
    if (bits == 16 && 4 * (na_eff + nb_eff) >= N) {
        size_t last_idx = 2 * na_eff;
        if (last_idx < M && ar[last_idx] != 0.0) {
            ai[last_idx - 1] += ar[last_idx] * 65536.0;
            ar[last_idx] = 0.0;
        }
    }
    fft_dif_inplace(ar, ai, M, plan);

    const double* __restrict__ rwr = gcpp_data(plan.rwr_br);
    const double* __restrict__ rwi = gcpp_data(plan.rwi_br);
    const double inv_m = 1.0 / static_cast<double>(M);
    const double half_inv_m = 0.5 * inv_m;
    // Note: factoring 0.5 out of E, D doubles X & Y -> C is 4x larger -> multiply by 0.125 * inv_m!
    const double scale_pair = 0.125 * inv_m;

    if (is_sqr) {
        // Index 0 = br(0): DC & Nyquist
        double dc = ar[0] + ai[0];
        double nyq = ar[0] - ai[0];
        double c0 = dc * dc, cm = nyq * nyq;
        ar[0] = half_inv_m * (c0 + cm);
        ai[0] = half_inv_m * (c0 - cm);
        // Index 1 = br(M/2): self-paired! W[1] = A[1]^2 * inv_m directly!
        if (M >= 2) {
            double r1 = ar[1], i1 = ai[1];
            ar[1] = inv_m * (r1 * r1 - i1 * i1);
            ai[1] = inv_m * (2.0 * r1 * i1);
        }
        // Bit-reversed conjugate pairs within each power-of-2 block [base .. 2*base - 1]!
        for (size_t base = 2; base < M; base <<= 1) {
            size_t half_blk = base >> 1;
            real_fft_sqr_blk(ar + base, ai + base, ar + base + half_blk, ai + base + half_blk,
                             rwr + base, rwi + base, half_blk, scale_pair);
        }
    } else {
        double* __restrict__ br = gcpp_data(g_fft.br);
        double* __restrict__ bi = gcpp_data(g_fft.bi);
        limbs_to_fft_soa(b, nb_eff, bits, br, bi, M);
        if (bits == 16 && 4 * (na_eff + nb_eff) >= N) {
            size_t last_idx = 2 * nb_eff;
            if (last_idx < M && br[last_idx] != 0.0) {
                bi[last_idx - 1] += br[last_idx] * 65536.0;
                br[last_idx] = 0.0;
            }
        }
        fft_dif_inplace(br, bi, M, plan);

        double dcA = ar[0] + ai[0], nyqA = ar[0] - ai[0];
        double dcB = br[0] + bi[0], nyqB = br[0] - bi[0];
        double c0 = dcA * dcB, cm = nyqA * nyqB;
        ar[0] = half_inv_m * (c0 + cm);
        ai[0] = half_inv_m * (c0 - cm);
        if (M >= 2) {
            double a1r = ar[1], a1i = ai[1], b1r = br[1], b1i = bi[1];
            ar[1] = inv_m * (a1r * b1r - a1i * b1i);
            ai[1] = inv_m * (a1r * b1i + a1i * b1r);
        }
        for (size_t base = 2; base < M; base <<= 1) {
            size_t half_blk = base >> 1;
            real_fft_mul_blk(ar + base, ai + base, ar + base + half_blk, ai + base + half_blk,
                             br + base, bi + base, br + base + half_blk, bi + base + half_blk,
                             rwr + base, rwi + base, half_blk, scale_pair);
        }
    }

    fft_dit_inv_inplace(ar, ai, M, plan);
    fft_soa_to_limbs(ar, ai, M, bits, r, na + nb);
    return true;
}

// Ultra-massive 3-prime 64-bit NTT fallback (> 7M bits, exact for up to 2^26 * 32 = 2B bits!)
namespace ntt3 {

static inline uint64_t mulmod(uint64_t a, uint64_t b, uint64_t p) noexcept {
#if defined(__SIZEOF_INT128__)
    return static_cast<uint64_t>((static_cast<u128>(a) * b) % p);
#else
    uint64_t hi, lo = mul64(a, b, hi);
    uint64_t s = gcpp_countl_zero(p);
    uint64_t dp = p << s;
    uint64_t v = invert_limb(dp);
    uint64_t u1 = (s == 0) ? hi : ((hi << s) | (lo >> (64 - s)));
    uint64_t u0 = lo << s;
    if (u1 >= dp) u1 %= dp;
    uint64_t rem = 0;
    div_2by1(u1, u0, dp, v, rem);
    return rem >> s;
#endif
}

static uint64_t powmod(uint64_t a, uint64_t e, uint64_t p) noexcept {
    uint64_t r = 1;
    while (e > 0) {
        if (e & 1) r = mulmod(r, a, p);
        a = mulmod(a, a, p);
        e >>= 1;
    }
    return r;
}

static void ntt(array<uint64_t>& a, uint64_t p, uint64_t g, bool inv) {
    size_t n = static_cast<size_t>(N(a));
    for (size_t i = 1, j = 0; i < n; ++i) {
        size_t bit = n >> 1;
        for (; j & bit; bit >>= 1) j ^= bit;
        j ^= bit;
        if (i < j) std::swap(a[i], a[j]);
    }
    for (size_t len = 2; len <= n; len <<= 1) {
        uint64_t wlen = powmod(g, (p - 1) / len, p);
        if (inv) wlen = powmod(wlen, p - 2, p);
        for (size_t i = 0; i < n; i += len) {
            uint64_t w = 1;
            for (size_t j = 0; j < len / 2; ++j) {
                uint64_t u = a[i + j];
                uint64_t v = mulmod(a[i + j + len / 2], w, p);
                a[i + j] = (u + v >= p) ? (u + v - p) : (u + v);
                a[i + j + len / 2] = (u >= v) ? (u - v) : (u + p - v);
                w = mulmod(w, wlen, p);
            }
        }
    }
    if (inv) {
        uint64_t inv_n = powmod(n, p - 2, p);
        for (auto& x : a) x = mulmod(x, inv_n, p);
    }
}

} // namespace ntt3

static void mpn_mul_ntt3(uint64_t* r, const uint64_t* a, size_t na, const uint64_t* b, size_t nb) {
    // 3 verified NTT-friendly primes (~59-62 bits each, 2^55 | (p-1)), product > 2^177
    // P1 = 4179340454199820289 = 29 * 2^57 + 1 (primitive root 3)
    // P2 = 1945555039024054273 = 27 * 2^56 + 1 (primitive root 5)
    // P3 = 180143985094819841  =  5 * 2^55 + 1 (primitive root 6)
    constexpr uint64_t P1 = 4179340454199820289ULL, G1 = 3;
    constexpr uint64_t P2 = 1945555039024054273ULL, G2 = 5;
    constexpr uint64_t P3 = 180143985094819841ULL,  G3 = 6;

    size_t la = na * 2, lb = nb * 2; // 32-bit chunks
    size_t n = 1;
    while (n < la + lb) n <<= 1;

    auto conv = [&](uint64_t p, uint64_t g) {
        array<uint64_t> fa = array_fill<uint64_t>(n, 0), fb = array_fill<uint64_t>(n, 0);
        for (size_t i = 0; i < na; ++i) {
            fa[2 * i] = a[i] & 0xFFFFFFFFULL;
            fa[2 * i + 1] = a[i] >> 32;
        }
        for (size_t i = 0; i < nb; ++i) {
            fb[2 * i] = b[i] & 0xFFFFFFFFULL;
            fb[2 * i + 1] = b[i] >> 32;
        }
        ntt3::ntt(fa, p, g, false);
        ntt3::ntt(fb, p, g, false);
        for (size_t i = 0; i < n; ++i) fa[i] = ntt3::mulmod(fa[i], fb[i], p);
        ntt3::ntt(fa, p, g, true);
        return fa;
    };

    auto c1 = conv(P1, G1);
    auto c2 = conv(P2, G2);
    auto c3 = conv(P3, G3);

    uint64_t inv_p1_p2 = ntt3::powmod(P1 % P2, P2 - 2, P2);
    uint64_t p1p2_mod_p3 = ntt3::mulmod(P1 % P3, P2 % P3, P3);
    uint64_t inv_p1p2_p3 = ntt3::powmod(p1p2_mod_p3, P3 - 2, P3);

    // Garner CRT for 3 primes: x = x1 + P1 * x2 + (P1*P2) * x3, where each coeff < 2^120
    uint64_t p1p2_hi, p1p2_lo = mul64(P1, P2, p1p2_hi);
    uint64_t carry0 = 0, carry1 = 0, carry2 = 0; // 192-bit running carry (shifted right by 32 each step)
    size_t out_chunks = (na + nb) * 2;
    std::memset(r, 0, (na + nb) * sizeof(uint64_t));

    for (size_t i = 0; i < out_chunks; ++i) {
        uint64_t r1 = c1[i];
        uint64_t r2 = c2[i];
        uint64_t r3 = c3[i];
        uint64_t t1 = (r2 >= r1 % P2) ? (r2 - r1 % P2) : (r2 + P2 - r1 % P2);
        uint64_t k2 = ntt3::mulmod(t1, inv_p1_p2, P2);
        uint64_t cur_mod_p3 = (r1 % P3 + ntt3::mulmod(k2 % P3, P1 % P3, P3)) % P3;
        uint64_t t2 = (r3 >= cur_mod_p3) ? (r3 - cur_mod_p3) : (r3 + P3 - cur_mod_p3);
        uint64_t k3 = ntt3::mulmod(t2, inv_p1p2_p3, P3);

        // Compute val = r1 + k2 * P1 + k3 * (P1 * P2) into 3 limbs (v0, v1, v2)
        uint64_t m1_hi, m1_lo = mul64(k2, P1, m1_hi);
        uint64_t c = 0;
        uint64_t v0 = add_carry(r1, m1_lo, 0, c);
        uint64_t v1 = m1_hi + c;
        uint64_t v2 = 0;

        uint64_t m2_hi, m2_lo = mul64(k3, p1p2_lo, m2_hi);
        uint64_t m3_hi, m3_lo = mul64(k3, p1p2_hi, m3_hi);
        c = 0;
        v0 = add_carry(v0, m2_lo, 0, c);
        uint64_t c2a = 0, c2b = 0;
        v1 = add_carry(v1, m2_hi, c, c2a);
        v1 = add_carry(v1, m3_lo, 0, c2b);
        v2 = m3_hi + c2a + c2b;

        // Add running carry
        c = 0;
        v0 = add_carry(v0, carry0, 0, c);
        v1 = add_carry(v1, carry1, c, c);
        v2 = v2 + carry2 + c;

        uint32_t digit = static_cast<uint32_t>(v0 & 0xFFFFFFFFULL);
        if ((i & 1) == 0) r[i >> 1] = digit;
        else r[i >> 1] |= (static_cast<uint64_t>(digit) << 32);

        // Shift (v2, v1, v0) right by 32 into (carry2, carry1, carry0)
        carry0 = (v0 >> 32) | (v1 << 32);
        carry1 = (v1 >> 32) | (v2 << 32);
        carry2 = (v2 >> 32);
    }
}

static void mpn_mul_dispatch(uint64_t* r, const uint64_t* a, size_t na, const uint64_t* b, size_t nb) {
    if (na < nb) { std::swap(a, b); std::swap(na, nb); }
    if (nb == 0) return;
    if (nb < KARATSUBA_MUL_THRESH) {
        mpn_mul_basecase(r, a, na, b, nb);
        return;
    }
    // Unbalanced slicing when na >= 2 * nb (keep chunks balanced unless both huge for FFT)
    if (na >= 2 * nb && nb < FFT_MUL_THRESH) {
        ScratchMark mark;
        uint64_t* tmp = tl_scratch.alloc(2 * nb);
        mpn_mul_dispatch(r, a, nb, b, nb);
        size_t pos = nb;
        while (pos < na) {
            size_t chunk = std::min(nb, na - pos);
            mpn_mul_dispatch(tmp, a + pos, chunk, b, nb);
            uint64_t c = mpn_add_n(r + pos, r + pos, tmp, nb);
            std::memcpy(r + pos + nb, tmp + nb, chunk * sizeof(uint64_t));
            if (c) mpn_add_1(r + pos + nb, r + pos + nb, chunk, c);
            pos += chunk;
        }
        return;
    }
    if (nb >= FFT_MUL_THRESH && (na + nb) >= 2 * FFT_MUL_THRESH) {
        mpn_mul_fft(r, a, na, b, nb, false);
        return;
    }
    if (nb >= TOOM3_MUL_THRESH && na < 2 * nb) {
        mpn_mul_toom3(r, a, na, b, nb);
        return;
    }
    mpn_mul_karatsuba(r, a, na, b, nb);
}

static void mpn_sqr_dispatch(uint64_t* r, const uint64_t* a, size_t n) {
    if (n == 0) return;
    if (n < KARATSUBA_SQR_THRESH) {
        mpn_sqr_basecase(r, a, n);
        return;
    }
    if (n >= FFT_SQR_THRESH) {
        mpn_mul_fft(r, a, n, a, n, true);
        return;
    }
    if (n >= TOOM3_SQR_THRESH) {
        mpn_sqr_toom3(r, a, n);
        return;
    }
    mpn_sqr_karatsuba(r, a, n);
}

// ============================================================================
// Section 5: Multi-Tier Division (1-limb GM -> Knuth D 3by2 -> Burnikel-Ziegler)
// ============================================================================

static constexpr size_t BZ_DIV_THRESH = 64;

// Single-limb division: q[0..n-1] = a[0..n-1] / d, returns remainder. Allows q == a.
static uint64_t mpn_divrem_1(uint64_t* q, const uint64_t* a, size_t n, uint64_t d) noexcept {
    if (n == 0) return 0;
    if (d == 1) {
        if (q != a) std::memcpy(q, a, n * sizeof(uint64_t));
        return 0;
    }
    unsigned s = static_cast<unsigned>(gcpp_countl_zero(d));
    uint64_t dn = d << s;
    uint64_t inv = invert_limb(dn);
    uint64_t r = 0;
    if (s == 0) {
        for (size_t i = n; i > 0; --i) {
            uint64_t ai = a[i - 1];
            if (r == 0 && ai < dn) {
                q[i - 1] = 0;
                r = ai;
            } else {
                q[i - 1] = div_2by1(r, ai, dn, inv, r);
            }
        }
        return r;
    } else {
        uint64_t prev = a[n - 1];
        r = prev >> (64 - s);
        for (size_t i = n - 1; i > 0; --i) {
            uint64_t cur = a[i - 1];
            uint64_t u0 = (prev << s) | (cur >> (64 - s));
            q[i] = div_2by1(r, u0, dn, inv, r);
            prev = cur;
        }
        q[0] = div_2by1(r, prev << s, dn, inv, r);
        return r >> s;
    }
}

static uint64_t mpn_mod_1(const uint64_t* a, size_t n, uint64_t d) noexcept {
    if (n == 0 || d == 1) return 0;
    if ((d & (d - 1)) == 0) return a[0] & (d - 1);
    unsigned s = static_cast<unsigned>(gcpp_countl_zero(d));
    uint64_t dn = d << s;
    uint64_t inv = invert_limb(dn);
    uint64_t r = 0;
    if (s == 0) {
        for (size_t i = n; i > 0; --i) {
            uint64_t ai = a[i - 1];
            if (r == 0 && ai < dn) r = ai;
            else div_2by1(r, ai, dn, inv, r);
        }
        return r;
    } else {
        uint64_t prev = a[n - 1];
        r = prev >> (64 - s);
        for (size_t i = n - 1; i > 0; --i) {
            uint64_t cur = a[i - 1];
            uint64_t u0 = (prev << s) | (cur >> (64 - s));
            div_2by1(r, u0, dn, inv, r);
            prev = cur;
        }
        div_2by1(r, prev << s, dn, inv, r);
        return r >> s;
    }
}

// Knuth Algorithm D with Granlund-Montgomery 3-by-2 normalized reciprocal.
// Assumes b is ALREADY normalized (b[nb-1] >= 2^63), nb >= 2, na >= nb.
// u[0..na] is overwritten in place with remainder in u[0..nb-1]; q[0..na-nb] receives quotient (na-nb+1 limbs).
static void mpn_divrem_knuth_norm(uint64_t* q, uint64_t* u, size_t na, const uint64_t* b, size_t nb) noexcept {
    uint64_t d1 = b[nb - 1];
    uint64_t d0 = b[nb - 2];
    uint64_t inv = invert_2limbs(d1, d0);

    size_t qlen = na - nb;
    // Top limb: check if u[qlen..na-1] >= b[0..nb-1]
    if (mpn_cmp(u + qlen, b, nb) >= 0) {
        mpn_sub_n(u + qlen, u + qlen, b, nb);
        q[qlen] = 1;
    } else {
        q[qlen] = 0;
    }

    for (size_t j = qlen; j > 0; --j) {
        size_t idx = j - 1;
        uint64_t u2 = u[idx + nb];
        uint64_t u1 = u[idx + nb - 1];
        uint64_t u0 = u[idx + nb - 2];
        uint64_t qhat;
        if (u2 == d1 && u1 == d0) {
            qhat = ~0ULL;
            // Subtract qhat * b from u[idx..idx+nb]
            uint64_t br = mpn_submul_1(u + idx, b, nb, qhat);
            if (u[idx + nb] < br) {
                --qhat;
                uint64_t c = mpn_add_n(u + idx, u + idx, b, nb);
                u[idx + nb] = u[idx + nb] - br + c;
            } else {
                u[idx + nb] -= br;
            }
        } else {
            uint64_t r1, r0;
            qhat = div_3by2(u2, u1, u0, d1, d0, inv, r1, r0);
            if (nb > 2) {
                uint64_t br = mpn_submul_1(u + idx, b, nb - 2, qhat);
                uint64_t b2 = 0;
                uint64_t nr0 = sub_borrow(r0, br, 0, b2);
                uint64_t nr1 = r1 - b2;
                u[idx + nb - 2] = nr0;
                u[idx + nb - 1] = nr1;
                if (r1 < b2) { // borrow out -> qhat was 1 too large (extremely rare!)
                    --qhat;
                    uint64_t c = mpn_add_n(u + idx, u + idx, b, nb);
                    (void)c;
                }
            } else {
                u[idx] = r0;
                u[idx + 1] = r1;
            }
            u[idx + nb] = 0;
        }
        q[idx] = qhat;
    }
}

// Forward declaration for Burnikel-Ziegler mutual recursion
static void mpn_divrem_norm(uint64_t* q, uint64_t* u, size_t na, const uint64_t* b, size_t nb);

// Burnikel-Ziegler Div3n2n:
// Input: normalized b of length 2*n (b[2n-1] >= 2^63), u of length 3*n with u[n..3n-1] < b[0..2n-1].
// Output: q of length n, remainder written into u[0..2n-1].
static void bz_div_3n2n(uint64_t* q, uint64_t* u, const uint64_t* b, size_t n);

// Burnikel-Ziegler Div2n1n:
// Input: normalized b of length 2*m (or odd length), u of length 4*m with u[2m..4m-1] < b[0..2m-1].
// Output: q of length 2*m, remainder in u[0..2m-1].
static void bz_div_2n1n(uint64_t* q, uint64_t* u, const uint64_t* b, size_t nb) {
    if (nb < BZ_DIV_THRESH || (nb & 1) != 0) {
        // Use Knuth D directly on 2*nb by nb (u has 2*nb limbs, top nb < b so q[nb]==0)
        // Note: mpn_divrem_knuth_norm writes q[0..nb] (nb+1 limbs) and reads u[0..2*nb-1],
        // so pass a temp q_top or handle directly:
        ScratchMark mark;
        uint64_t* qt = tl_scratch.alloc(nb + 1);
        mpn_divrem_knuth_norm(qt, u, 2 * nb, b, nb);
        std::memcpy(q, qt, nb * sizeof(uint64_t));
        return;
    }
    size_t half = nb >> 1;
    // Upper half: (u[half..4*half-1]) / b -> q[half..2*half-1], rem in u[half..3*half-1]
    bz_div_3n2n(q + half, u + half, b, half);
    // Lower half: (u[0..3*half-1]) / b -> q[0..half-1], rem in u[0..2*half-1]
    bz_div_3n2n(q, u, b, half);
}

static void bz_div_3n2n(uint64_t* q, uint64_t* u, const uint64_t* b, size_t n) {
    // Fast skip when entire 3n-limb dividend u[0..3n-1] < b[0..2n-1] (q == 0!)
    if (mpn_norm(u + 2 * n, n) == 0 && mpn_cmp(u, b, 2 * n) < 0) {
        std::memset(q, 0, n * sizeof(uint64_t));
        return;
    }
    ScratchMark mark;
    const uint64_t* b1 = b + n;
    const uint64_t* b0 = b;
    // Compare top n limbs u[2n..3n-1] against b1[0..n-1]
    if (mpn_cmp(u + 2 * n, b1, n) < 0) {
        // Normal case: divide [u[n..3n-1]] (2n limbs) by b1 (n limbs) -> q (n limbs), rem in u[n..2n-1]
        bz_div_2n1n(q, u + n, b1, n);
    } else {
        // Rare case: u[2n..3n-1] == b1 -> q = B^n - 1
        std::memset(q, 0xFF, n * sizeof(uint64_t));
        // Subtract (B^n - 1) * b1 from u[n..3n-1] = u[n..3n-1] - b1*B^n + b1
        mpn_sub_n(u + 2 * n, u + 2 * n, b1, n);
        uint64_t c = mpn_add_n(u + n, u + n, b1, n);
        u[2 * n] += c;
    }
    // Compute d = q * b0 (2n limbs)
    uint64_t* d = tl_scratch.alloc(2 * n);
    mpn_mul_dispatch(d, q, n, b0, n);

    // Subtract d from u[0..2n-1] (note u[2n] may be 0 or 1 from rare case above, or 0 in normal case)
    uint64_t top = u[2 * n];
    uint64_t br = mpn_sub_n(u, u, d, 2 * n);
    // While remainder is negative (top < br): decrement q and add b to u[0..2n-1] (at most 2 times!)
    int64_t signed_top = static_cast<int64_t>(top) - static_cast<int64_t>(br);
    while (signed_top < 0) {
        mpn_sub_1(q, q, n, 1);
        uint64_t c = mpn_add_n(u, u, b, 2 * n);
        signed_top += static_cast<int64_t>(c);
    }
    u[2 * n] = 0;
}

static constexpr size_t BARRETT_DIV_THRESH = 1536;

// Newton-Raphson approximate normalized reciprocal:
// Given normalized d[0..n-1] (d[n-1] >= 2^63), computes inv[0..n-1] such that
//   V = B^n + inv  satisfies  0 <= floor((B^(2n) - 1) / d) - V <= 3.
static void mpn_invert_appr(uint64_t* inv, const uint64_t* d, size_t n) {
    ScratchMark mark;
    if (n <= 48) {
        // Exact basecase via Knuth D: (B^(2n) - 1 - B^n * d) / d = floor((B^(2n)-1)/d) - B^n
        uint64_t* u = tl_scratch.alloc(2 * n);
        for (size_t i = 0; i < n; ++i) u[i] = ~0ULL;
        for (size_t i = 0; i < n; ++i) u[n + i] = ~d[i];
        uint64_t* qt = tl_scratch.alloc(n + 1);
        mpn_divrem_knuth_norm(qt, u, 2 * n, d, n);
        std::memcpy(inv, qt, n * sizeof(uint64_t));
        return;
    }

    size_t h = (n >> 1) + 1;
    size_t l = n - h; // 1 <= l < h, 2*h - n in {1, 2}

    uint64_t* ih = tl_scratch.alloc(h);
    mpn_invert_appr(ih, d + l, h);

    // T = (B^h + ih) * d (n + h + 1 limbs)
    uint64_t* T = tl_scratch.alloc(n + h + 1);
    mpn_mul_dispatch(T, d, n, ih, h);
    T[n + h] = mpn_add_n(T + h, T + h, d, n);

    uint64_t vh_top = 1;
    while (T[n + h] > 0) {
        uint64_t br = mpn_sub(T, T, n + h, d, n);
        T[n + h] -= br;
        uint64_t vb = mpn_sub_1(ih, ih, h, 1);
        vh_top -= vb;
    }

    // E = B^(n+h) - T < 5 * B^n; extract E_hi = floor(E / B^h) (l + 1 limbs: E[h..n])
    bool low_nz = false;
    for (size_t i = 0; i < h; ++i) {
        if (T[i] != 0) { low_nz = true; break; }
    }
    uint64_t* eh = tl_scratch.alloc(l + 1);
    for (size_t i = 0; i <= l; ++i) eh[i] = ~T[h + i];
    if (!low_nz) mpn_add_1(eh, eh, l + 1, 1);

    // V_{h,hi} = floor(V_h / B^(h - l)) (l + 1 limbs)
    uint64_t* vh_hi = tl_scratch.alloc(l + 1);
    std::memcpy(vh_hi, ih + (h - l), l * sizeof(uint64_t));
    vh_hi[l] = vh_top;

    // P = eh * vh_hi (2*l + 2 limbs); corr = floor(P / B^l) = P[l .. 2*l + 1] (l + 2 limbs)
    uint64_t* P = tl_scratch.alloc(2 * l + 2);
    mpn_mul_dispatch(P, eh, l + 1, vh_hi, l + 1);
    const uint64_t* corr = P + l;

    // V = V_h * B^l + corr -> inv = (ih * B^l + corr) mod B^n
    std::memcpy(inv, corr, l * sizeof(uint64_t));
    uint64_t c = mpn_add(inv + l, ih, h, corr + l, 2);
    if (vh_top + c > 1) {
        // Clamp defensively (mathematically unreachable for normalized d)
        for (size_t i = 0; i < n; ++i) inv[i] = ~0ULL;
    }
}

// Barrett 2n-by-n division using precomputed approximate reciprocal inv[0..n-1] of normalized b[0..n-1].
// Input: u[0..2*n-1] with u[n..2*n-1] < b[0..n-1].
// Output: exact q[0..n-1], exact remainder in u[0..n-1].
static void mpn_divrem_barrett_2n1n(uint64_t* q, uint64_t* u, const uint64_t* b, const uint64_t* inv, size_t n) {
    ScratchMark mark;
    const uint64_t* u_hi = u + n;
    uint64_t* prod = tl_scratch.alloc(2 * n);

    // q_est = floor(u_hi * (B^n + inv) / B^n) = u_hi + floor(u_hi * inv / B^n) <= q_true
    mpn_mul_dispatch(prod, u_hi, n, inv, n);
    uint64_t qc = mpn_add_n(q, u_hi, prod + n, n);
    if (qc != 0) {
        for (size_t i = 0; i < n; ++i) q[i] = ~0ULL;
    }

    // prod = q_est * b (2*n limbs); r = u - prod < 6 * b < 6 * B^n
    mpn_mul_dispatch(prod, q, n, b, n);
    mpn_sub_n(u, u, prod, n + 1);

    // Adjust q upward (at most 5 times, typically 0-1)
    while (u[n] > 0 || mpn_cmp(u, b, n) >= 0) {
        uint64_t sb = mpn_sub_n(u, u, b, n);
        u[n] -= sb;
        mpn_add_1(q, q, n, 1);
    }
}

// General normalized division: b[nb-1] >= 2^63, na >= nb >= 2.
// q gets na - nb + 1 limbs, u[0..nb-1] gets remainder.
static void mpn_divrem_norm(uint64_t* q, uint64_t* u, size_t na, const uint64_t* b, size_t nb) {
    size_t qlen = na - nb;
    if (nb < BZ_DIV_THRESH || qlen < BZ_DIV_THRESH / 2) {
        mpn_divrem_knuth_norm(q, u, na, b, nb);
        return;
    }

    if (nb >= BARRETT_DIV_THRESH) {
        ScratchMark mark;
        uint64_t* inv = tl_scratch.alloc(nb);
        mpn_invert_appr(inv, b, nb);

        size_t t = (na + nb - 1) / nb;
        if (t < 2) t = 2;
        size_t work_len = t * nb;
        uint64_t* up = tl_scratch.alloc(work_len);
        std::memcpy(up, u, na * sizeof(uint64_t));
        if (work_len > na) std::memset(up + na, 0, (work_len - na) * sizeof(uint64_t));

        uint64_t top_q = 0;
        if (mpn_cmp(up + (t - 1) * nb, b, nb) >= 0) {
            mpn_sub_n(up + (t - 1) * nb, up + (t - 1) * nb, b, nb);
            top_q = 1;
        }

        uint64_t* qfull = tl_scratch.alloc((t - 1) * nb);
        for (size_t i = t - 1; i > 0; --i) {
            size_t pos = (i - 1) * nb;
            mpn_divrem_barrett_2n1n(qfull + pos, up + pos, b, inv, nb);
        }

        size_t q_total = na - nb + 1;
        size_t qcopy = std::min(q_total, (t - 1) * nb);
        std::memcpy(q, qfull, qcopy * sizeof(uint64_t));
        if (qcopy < q_total) {
            std::memset(q + qcopy, 0, (q_total - qcopy) * sizeof(uint64_t));
            if (top_q && (t - 1) * nb < q_total) q[(t - 1) * nb] = 1;
        }
        std::memcpy(u, up, nb * sizeof(uint64_t));
        return;
    }

    // Pad b up to an even/power-friendly block size m = ceil(nb / 2^k) * 2^k so bz_div_2n1n can recurse deeply!
    ScratchMark mark;
    size_t k = 0;
    while ((nb >> k) >= BZ_DIV_THRESH) ++k;
    size_t step = size_t(1) << k;
    size_t m = ((nb + step - 1) / step) * step; // multiple of 2^k, m >= nb
    size_t pad = m - nb;

    // Shift both b and u up by `pad` limbs (equivalent to multiplying both by B^pad!)
    uint64_t* bp = tl_scratch.alloc(m);
    if (pad > 0) std::memset(bp, 0, pad * sizeof(uint64_t));
    std::memcpy(bp + pad, b, nb * sizeof(uint64_t));

    // Break padded u into blocks of size m
    size_t up_len = na + pad;
    // Number of blocks of size m needed so top block < bp
    size_t t = (up_len + m - 1) / m;
    if (t < 2) t = 2;
    size_t work_len = t * m;
    uint64_t* up = tl_scratch.alloc(work_len);
    std::memset(up, 0, work_len * sizeof(uint64_t));
    std::memcpy(up + pad, u, na * sizeof(uint64_t));

    // Because bp is normalized (bp[m-1] >= 2^63), top block < B^m < 2*bp:
    // if top block >= bp, a single O(m) subtraction reduces it < bp (avoiding an extra BZ call!)
    uint64_t top_q = 0;
    if (mpn_cmp(up + (t - 1) * m, bp, m) >= 0) {
        mpn_sub_n(up + (t - 1) * m, up + (t - 1) * m, bp, m);
        top_q = 1;
    }

    // Process blocks from top down: at each step divide 2m limbs by m limbs
    uint64_t* qfull = tl_scratch.alloc((t - 1) * m);
    for (size_t i = t - 1; i > 0; --i) {
        size_t pos = (i - 1) * m;
        bz_div_2n1n(qfull + pos, up + pos, bp, m);
    }

    // Extract exact quotient (na - nb + 1 limbs) and remainder (nb limbs at up[pad..pad+nb-1])
    size_t q_total = na - nb + 1;
    size_t qcopy = std::min(q_total, (t - 1) * m);
    std::memcpy(q, qfull, qcopy * sizeof(uint64_t));
    if (qcopy < q_total) {
        std::memset(q + qcopy, 0, (q_total - qcopy) * sizeof(uint64_t));
        if (top_q && (t - 1) * m < q_total) q[(t - 1) * m] = 1;
    }
    std::memcpy(u, up + pad, nb * sizeof(uint64_t));
}

// General unnormalized mpn division: a[0..na-1] / b[0..nb-1], na >= nb >= 1, b[nb-1] != 0.
// q has na - nb + 1 limbs (may be nullptr if only remainder needed), r has nb limbs (may be nullptr).
static void mpn_divrem(uint64_t* q, uint64_t* r, const uint64_t* a, size_t na, const uint64_t* b, size_t nb) {
    if (nb == 1) {
        ScratchMark mark;
        uint64_t* qbuf = q ? q : tl_scratch.alloc(na);
        uint64_t rem = mpn_divrem_1(qbuf, a, na, b[0]);
        if (r) r[0] = rem;
        return;
    }
    ScratchMark mark;
    unsigned s = static_cast<unsigned>(gcpp_countl_zero(b[nb - 1]));
    uint64_t* u = tl_scratch.alloc(na + 1);
    const uint64_t* bn = b;
    uint64_t* b_buf = nullptr;
    if (s == 0) {
        std::memcpy(u, a, na * sizeof(uint64_t));
        u[na] = 0;
    } else {
        b_buf = tl_scratch.alloc(nb);
        mpn_lshift(b_buf, b, nb, s);
        bn = b_buf;
        u[na] = mpn_lshift(u, a, na, s);
    }
    size_t qlen = na - nb + 1;
    uint64_t* qbuf = q ? q : tl_scratch.alloc(qlen);
    if (u[na] != 0) {
        // u has na + 1 limbs, top limb is < bn[nb-1]
        // Note: mpn_divrem_norm writes na + 1 - nb + 1 limbs to q, top one is guaranteed 0!
        uint64_t* qt = tl_scratch.alloc(qlen + 1);
        mpn_divrem_norm(qt, u, na + 1, bn, nb);
        std::memcpy(qbuf, qt, qlen * sizeof(uint64_t));
    } else {
        mpn_divrem_norm(qbuf, u, na, bn, nb);
    }
    if (r) {
        if (s == 0) std::memcpy(r, u, nb * sizeof(uint64_t));
        else mpn_rshift(r, u, nb, s);
    }
}

// ============================================================================
// Section 6: Fast GCD (1-limb Binary -> 64-bit Lehmer -> Half-GCD) & ModInv
// ============================================================================

static inline uint64_t gcd_u64(uint64_t a, uint64_t b) noexcept {
    if (a == 0) return b;
    if (b == 0) return a;
    unsigned sa = static_cast<unsigned>(gcpp_countr_zero(a));
    unsigned sb = static_cast<unsigned>(gcpp_countr_zero(b));
    unsigned shift = std::min(sa, sb);
    a >>= sa;
    b >>= sb;
    while (a != b) {
        if (a > b) {
            a -= b;
            a >>= gcpp_countr_zero(a);
        } else {
            b -= a;
            b >>= gcpp_countr_zero(b);
        }
    }
    return a << shift;
}

// Lehmer 64-bit inner step: given top 64-bit approximations (ah, bh) with ah >= bh,
// compute unimodular 2x2 matrix [[u00, -u01], [-u10, u11]] (or opposite parity) such that
// [a', b'] = M * [a, b] reduces a, b by ~30-60 bits without division on full bignums!
struct LehmerMat {
    uint64_t u00, u01, u10, u11;
    bool odd_steps;
};

// Fast 64-bit Lehmer step with zero-division q=1/q=2 fast paths!
static inline bool lehmer_step64(uint64_t ah, uint64_t bh, LehmerMat& m) noexcept {
    uint64_t u00 = 1, u01 = 0, u10 = 0, u11 = 1;
    bool odd = false;
    while (bh > 0) {
        uint64_t r, n00, n01;
        uint64_t diff = ah - bh;
        if (diff < bh) {
            r = diff;
            n00 = u00 + u10;
            n01 = u01 + u11;
        } else {
            uint64_t diff2 = diff - bh;
            if (diff2 < bh) {
                r = diff2;
                n00 = u00 + (u10 << 1);
                n01 = u01 + (u11 << 1);
            } else {
                uint64_t q = ah / bh;
                if (q > 0xFFFFFFFFULL) break;
                r = ah - q * bh;
                n00 = u00 + q * u10;
                n01 = u01 + q * u11;
            }
        }
        if (!odd) {
            if (r < n01 || bh - r < n00 + u10) break;
        } else {
            if (r < n00 || bh - r < n01 + u11) break;
        }
        ah = bh; bh = r;
        u00 = u10; u10 = n00;
        u01 = u11; u11 = n01;
        odd = !odd;
    }
    if (u10 == 0) return false;
    m = {u00, u01, u10, u11, odd};
    return true;
}

// Helper: extract top 64 bits of (a, b) aligned to bit-length of a (na >= nb >= 1, a[na-1] != 0)
static inline void extract_top64(const uint64_t* a, size_t na, const uint64_t* b, size_t nb,
                                 uint64_t& ah, uint64_t& bh) noexcept {
    unsigned lz = static_cast<unsigned>(gcpp_countl_zero(a[na - 1]));
    if (lz == 0) {
        ah = a[na - 1];
        bh = (nb == na) ? b[nb - 1] : 0ULL;
    } else {
        ah = (a[na - 1] << lz) | (na >= 2 ? (a[na - 2] >> (64 - lz)) : 0ULL);
        uint64_t b_hi = (nb == na) ? b[nb - 1] : 0ULL;
        uint64_t b_lo = (nb >= na - 1 && na >= 2) ? b[na - 2] : 0ULL;
        bh = (b_hi << lz) | (b_lo >> (64 - lz));
    }
}

// Dispatch Lehmer: 64-bit Lehmer for <= 48 limbs (fastest per step), 128-bit double-limb for > 48 limbs!
static inline bool lehmer_step_auto(const uint64_t* a, size_t na, const uint64_t* b, size_t nb, LehmerMat& m) noexcept {
#if defined(__SIZEOF_INT128__)
    if (na > 48) {
        unsigned lz = static_cast<unsigned>(gcpp_countl_zero(a[na - 1]));
        auto get_b = [&](size_t idx) -> uint64_t { return idx < nb ? b[idx] : 0ULL; };
        u128 ah, bh;
        if (lz == 0) {
            ah = (static_cast<u128>(a[na - 1]) << 64) | a[na - 2];
            bh = (static_cast<u128>(get_b(na - 1)) << 64) | get_b(na - 2);
        } else {
            uint64_t a2 = (a[na - 1] << lz) | (a[na - 2] >> (64 - lz));
            uint64_t a1 = (a[na - 2] << lz) | (a[na - 3] >> (64 - lz));
            ah = (static_cast<u128>(a2) << 64) | a1;
            uint64_t bv2 = get_b(na - 1), bv1 = get_b(na - 2), bv0 = get_b(na - 3);
            uint64_t b2 = (bv2 << lz) | (bv1 >> (64 - lz));
            uint64_t b1 = (bv1 << lz) | (bv0 >> (64 - lz));
            bh = (static_cast<u128>(b2) << 64) | b1;
        }
        uint64_t u00 = 1, u01 = 0, u10 = 0, u11 = 1;
        bool odd = false;
        constexpr uint64_t MAX_U = 1ULL << 62;
        while (bh > 0) {
            uint64_t n00, n01;
            u128 r;
            u128 diff = ah - bh;
            if (diff < bh) {
                // Fast q = 1 (zero division!)
                r = diff;
                n00 = u00 + u10;
                n01 = u01 + u11;
                if ((n00 | n01) >= MAX_U) break;
            } else {
                u128 diff2 = diff - bh;
                if (diff2 < bh) {
                    // Fast q = 2 (zero division!)
                    r = diff2;
                    n00 = u00 + (u10 << 1);
                    n01 = u01 + (u11 << 1);
                    if ((n00 | n01) >= MAX_U) break;
                } else {
                    uint64_t q;
                    uint64_t bhi = static_cast<uint64_t>(bh >> 64);
                    if (bhi >= (1ULL << 32)) {
                        uint64_t ahi = static_cast<uint64_t>(ah >> 64);
                        q = ahi / (bhi + 1ULL);
                        r = ah - static_cast<u128>(q) * bh;
                        while (r >= bh) { r -= bh; ++q; }
                    } else {
                        u128 q128 = ah / bh;
                        if (q128 > 0xFFFFFFFFULL) break;
                        q = static_cast<uint64_t>(q128);
                        r = ah - q128 * bh;
                    }
                    if ((u10 >= (1ULL << 30) && q > (MAX_U - u00) / u10) ||
                        (u11 >= (1ULL << 30) && q > (MAX_U - u01) / u11)) break;
                    n00 = u00 + q * u10;
                    n01 = u01 + q * u11;
                    if ((n00 | n01) >= MAX_U) break;
                }
            }
            if (!odd) {
                if (r < n01 || bh - r < static_cast<u128>(n00) + u10) break;
            } else {
                if (r < n00 || bh - r < static_cast<u128>(n01) + u11) break;
            }
            ah = bh; bh = r;
            u00 = u10; u10 = n00;
            u01 = u11; u11 = n01;
            odd = !odd;
        }
        if (u10 > 0) {
            m = {u00, u01, u10, u11, odd};
            return true;
        }
        return false;
    }
#endif
    uint64_t ah, bh;
    extract_top64(a, na, b, nb, ah, bh);
    return lehmer_step64(ah, bh, m);
}

// Fused single-pass apply_lehmer: reads a[i], b[i] once instead of 4 passes!
static void apply_lehmer(uint64_t* a, size_t& na, uint64_t* b, size_t& nb, const LehmerMat& m) {
#if defined(__SIZEOF_INT128__)
    ScratchMark mark;
    size_t n = na;
    uint64_t* na_buf = tl_scratch.alloc(n + 1);
    uint64_t* nb_buf = tl_scratch.alloc(n + 1);
    // Compute a' = (-1)^odd * (u00*a - u01*b), b' = (-1)^odd * (-u10*a + u11*b)
    // Both a' and b' are guaranteed non-negative!
    i128 ca = 0, cb = 0;
    if (!m.odd_steps) {
        for (size_t i = 0; i < n; ++i) {
            uint64_t ai = a[i];
            uint64_t bi = (i < nb) ? b[i] : 0ULL;
            // u00*ai - u01*bi + ca
            i128 pa = static_cast<i128>(static_cast<u128>(ai) * m.u00) - static_cast<i128>(static_cast<u128>(bi) * m.u01) + ca;
            // Note: u00, u01 < 2^62, so ai*u00 < 2^126 fits in signed i128 without overflow!
            na_buf[i] = static_cast<uint64_t>(pa);
            ca = pa >> 64;

            i128 pb = static_cast<i128>(static_cast<u128>(bi) * m.u11) - static_cast<i128>(static_cast<u128>(ai) * m.u10) + cb;
            nb_buf[i] = static_cast<uint64_t>(pb);
            cb = pb >> 64;
        }
    } else {
        for (size_t i = 0; i < n; ++i) {
            uint64_t ai = a[i];
            uint64_t bi = (i < nb) ? b[i] : 0ULL;
            i128 pa = static_cast<i128>(static_cast<u128>(bi) * m.u01) - static_cast<i128>(static_cast<u128>(ai) * m.u00) + ca;
            na_buf[i] = static_cast<uint64_t>(pa);
            ca = pa >> 64;

            i128 pb = static_cast<i128>(static_cast<u128>(ai) * m.u10) - static_cast<i128>(static_cast<u128>(bi) * m.u11) + cb;
            nb_buf[i] = static_cast<uint64_t>(pb);
            cb = pb >> 64;
        }
    }
    na_buf[n] = static_cast<uint64_t>(ca);
    nb_buf[n] = static_cast<uint64_t>(cb);
    size_t nna = mpn_norm(na_buf, n + 1);
    size_t nnb = mpn_norm(nb_buf, n + 1);
    if (nna > 0) std::memcpy(a, na_buf, nna * sizeof(uint64_t));
    na = nna;
    if (nnb > 0) std::memcpy(b, nb_buf, nnb * sizeof(uint64_t));
    nb = nnb;
#else
    ScratchMark mark;
    size_t sz = na + 1;
    uint64_t* t1 = tl_scratch.alloc(sz);
    uint64_t* t2 = tl_scratch.alloc(sz);
    uint64_t* na_buf = tl_scratch.alloc(sz);
    uint64_t* nb_buf = tl_scratch.alloc(sz);

    t1[na] = mpn_mul_1(t1, a, na, m.u00);
    size_t l1 = mpn_norm(t1, na + 1);
    t2[nb] = mpn_mul_1(t2, b, nb, m.u01);
    size_t l2 = mpn_norm(t2, nb + 1);
    size_t nna;
    if (!m.odd_steps) { mpn_sub(na_buf, t1, l1, t2, l2); nna = mpn_norm(na_buf, l1); }
    else              { mpn_sub(na_buf, t2, l2, t1, l1); nna = mpn_norm(na_buf, l2); }

    t1[na] = mpn_mul_1(t1, a, na, m.u10);
    l1 = mpn_norm(t1, na + 1);
    t2[nb] = mpn_mul_1(t2, b, nb, m.u11);
    l2 = mpn_norm(t2, nb + 1);
    size_t nnb;
    if (!m.odd_steps) { mpn_sub(nb_buf, t2, l2, t1, l1); nnb = mpn_norm(nb_buf, l2); }
    else              { mpn_sub(nb_buf, t1, l1, t2, l2); nnb = mpn_norm(nb_buf, l1); }

    if (nna > 0) std::memcpy(a, na_buf, nna * sizeof(uint64_t));
    na = nna;
    if (nnb > 0) std::memcpy(b, nb_buf, nnb * sizeof(uint64_t));
    nb = nnb;
#endif
}

struct HgcdMat {
    BigInt u00, u01, u10, u11;
    bool odd = false;

    static HgcdMat identity() {
        HgcdMat m;
        m.u00.set_ui(1); m.u11.set_ui(1);
        return m;
    }

    static HgcdMat compose(const HgcdMat& m2, const HgcdMat& m1) {
        HgcdMat r;
        r.odd = (m2.odd != m1.odd);
        r.u00 = BigInt::add(BigInt::mul(m2.u00, m1.u00), BigInt::mul(m2.u01, m1.u10));
        r.u01 = BigInt::add(BigInt::mul(m2.u00, m1.u01), BigInt::mul(m2.u01, m1.u11));
        r.u10 = BigInt::add(BigInt::mul(m2.u10, m1.u00), BigInt::mul(m2.u11, m1.u10));
        r.u11 = BigInt::add(BigInt::mul(m2.u10, m1.u01), BigInt::mul(m2.u11, m1.u11));
        return r;
    }

    // Fast O(n) left-multiply by single-limb LehmerMat: M = L * M (zero BigInt::mul!)
    void left_mul_lehmer(const LehmerMat& L) {
        odd = (odd != L.odd_steps);
        BigInt n00 = BigInt::add(BigInt::mul_ui(u00, L.u00), BigInt::mul_ui(u10, L.u01));
        BigInt n01 = BigInt::add(BigInt::mul_ui(u01, L.u00), BigInt::mul_ui(u11, L.u01));
        BigInt n10 = BigInt::add(BigInt::mul_ui(u00, L.u10), BigInt::mul_ui(u10, L.u11));
        BigInt n11 = BigInt::add(BigInt::mul_ui(u01, L.u10), BigInt::mul_ui(u11, L.u11));
        u00 = std::move(n00); u01 = std::move(n01);
        u10 = std::move(n10); u11 = std::move(n11);
    }

    // Fast left-multiply by single Euclidean step [[0, -1], [-1, q]] with odd=true
    void left_mul_quotient(BigIntView q) {
        odd = !odd;
        BigInt n00 = std::move(u10);
        BigInt n01 = std::move(u11);
        BigInt n10 = BigInt::add(u00, BigInt::mul(q, n00));
        BigInt n11 = BigInt::add(u01, BigInt::mul(q, n01));
        u00 = std::move(n00); u01 = std::move(n01);
        u10 = std::move(n10); u11 = std::move(n11);
    }

    // Apply this matrix to (a, b) -> (a', b')
    void apply(BigInt& a, BigInt& b) const {
        BigInt t1 = BigInt::mul(u00, a);
        BigInt t2 = BigInt::mul(u01, b);
        BigInt t3 = BigInt::mul(u10, a);
        BigInt t4 = BigInt::mul(u11, b);
        if (!odd) {
            a = BigInt::sub(t1, t2);
            b = BigInt::sub(t4, t3);
        } else {
            a = BigInt::sub(t2, t1);
            b = BigInt::sub(t3, t4);
        }
    }
    // Apply this matrix to (a, b) where a_hi = a >> (s*64), b_hi = b >> (s*64) have ALREADY been
    // reduced to (a_hi_red, b_hi_red) = M * (a_hi, b_hi):
    //   M * (a_hi*B^s + a_lo, b_hi*B^s + b_lo) = (a_hi_red*B^s, b_hi_red*B^s) + M * (a_lo, b_lo)!!
    // Cuts the matrix-vector multiplication size in HALF!
    void apply_split(BigInt& a, BigInt& b, size_t s, const BigInt& a_hi_red, const BigInt& b_hi_red) const {
        auto low_slice = [&](const BigInt& x) -> BigIntView {
            size_t l = std::min(static_cast<size_t>(N(x)), s);
            l = mpn_norm(gcpp_data(x), l);
            return {gcpp_data(x), l, false};
        };
        BigIntView alo = low_slice(a);
        BigIntView blo = low_slice(b);
        BigInt t1 = BigInt::mul(u00, alo);
        BigInt t2 = BigInt::mul(u01, blo);
        BigInt t3 = BigInt::mul(u10, alo);
        BigInt t4 = BigInt::mul(u11, blo);
        BigInt da = !odd ? BigInt::sub(t1, t2) : BigInt::sub(t2, t1);
        BigInt db = !odd ? BigInt::sub(t4, t3) : BigInt::sub(t3, t4);
        a = BigInt::add(BigInt::mul_2exp(a_hi_red, s * 64), da);
        b = BigInt::add(BigInt::mul_2exp(b_hi_red, s * 64), db);
    }
};

static constexpr size_t HGCD_THRESH = 64; // limbs

// Raw 64-bit Lehmer step on unshifted top limbs (ah, bh) above target_limbs.
// Requires r > n01 (even) / r > n00 (odd) so b_new is GUARANTEED >= B^target_limbs (never overshoots target_limbs!).
static inline bool lehmer_step64_raw(uint64_t ah, uint64_t bh, LehmerMat& m) noexcept {
    uint64_t u00 = 1, u01 = 0, u10 = 0, u11 = 1;
    bool odd = false;
    while (bh > 0) {
        uint64_t r, n00, n01;
        uint64_t diff = ah - bh;
        if (diff < bh) {
            r = diff;
            n00 = u00 + u10;
            n01 = u01 + u11;
        } else {
            uint64_t diff2 = diff - bh;
            if (diff2 < bh) {
                r = diff2;
                n00 = u00 + (u10 << 1);
                n01 = u01 + (u11 << 1);
            } else {
                uint64_t q = ah / bh;
                r = ah - q * bh;
                if (q > 0xFFFFFFFFULL) break;
                n00 = u00 + q * u10;
                n01 = u01 + q * u11;
            }
        }
        if (!odd) {
            if (r <= n01 || bh - r < n00 + u10) break;
        } else {
            if (r <= n00 || bh - r < n01 + u11) break;
        }
        ah = bh; bh = r;
        u00 = u10; u10 = n00;
        u01 = u11; u11 = n01;
        odd = !odd;
    }
    if (u10 == 0) return false;
    m = {u00, u01, u10, u11, odd};
    return true;
}

static HgcdMat hgcd_rec(BigInt& a, BigInt& b);

// Flat-array zero-allocation hgcd_basecase: keeps M = (u00, u01, u10, u11) on ScratchArena!
static HgcdMat hgcd_basecase(BigInt& a, BigInt& b, size_t target_limbs) {
    ScratchMark mark;
    size_t cap = static_cast<size_t>(N(a)) + 4;
    uint64_t* u00 = tl_scratch.alloc(cap); size_t l00 = 1; u00[0] = 1;
    uint64_t* u01 = tl_scratch.alloc(cap); size_t l01 = 0;
    uint64_t* u10 = tl_scratch.alloc(cap); size_t l10 = 0;
    uint64_t* u11 = tl_scratch.alloc(cap); size_t l11 = 1; u11[0] = 1;
    uint64_t* t0  = tl_scratch.alloc(cap);
    uint64_t* t1  = tl_scratch.alloc(cap);
    bool odd = false;

    auto mul_add_col = [&](uint64_t* x, size_t& lx, uint64_t* y, size_t& ly, const LehmerMat& L) {
        size_t n = std::max(lx, ly);
        if (n == 0) return;
        // nx = L.u00*x + L.u01*y, ny = L.u10*x + L.u11*y
#if defined(__SIZEOF_INT128__)
        u128 cx = 0, cy = 0;
        for (size_t i = 0; i < n; ++i) {
            uint64_t xi = (i < lx) ? x[i] : 0ULL;
            uint64_t yi = (i < ly) ? y[i] : 0ULL;
            u128 px = static_cast<u128>(xi) * L.u00 + static_cast<u128>(yi) * L.u01 + cx;
            t0[i] = static_cast<uint64_t>(px);
            cx = px >> 64;
            u128 py = static_cast<u128>(xi) * L.u10 + static_cast<u128>(yi) * L.u11 + cy;
            t1[i] = static_cast<uint64_t>(py);
            cy = py >> 64;
        }
        size_t nlx = n, nly = n;
        while (cx > 0) { t0[nlx++] = static_cast<uint64_t>(cx); cx >>= 64; }
        while (cy > 0) { t1[nly++] = static_cast<uint64_t>(cy); cy >>= 64; }
        nlx = mpn_norm(t0, nlx);
        nly = mpn_norm(t1, nly);
        if (nlx > 0) std::memcpy(x, t0, nlx * sizeof(uint64_t));
        if (nly > 0) std::memcpy(y, t1, nly * sizeof(uint64_t));
        lx = nlx; ly = nly;
#else
        BigInt bx(BigIntView{x, lx, false}), by(BigIntView{y, ly, false});
        BigInt nx = BigInt::add(BigInt::mul_ui(bx, L.u00), BigInt::mul_ui(by, L.u01));
        BigInt ny = BigInt::add(BigInt::mul_ui(bx, L.u10), BigInt::mul_ui(by, L.u11));
        lx = static_cast<size_t>(N(nx)); ly = static_cast<size_t>(N(ny));
        if (lx > 0) std::memcpy(x, gcpp_data(nx), lx * sizeof(uint64_t));
        if (ly > 0) std::memcpy(y, gcpp_data(ny), ly * sizeof(uint64_t));
#endif
    };

    auto apply_q = [&](const BigInt& q) {
        odd = !odd;
        // (u00, u10) -> (u10, u00 + q*u10); (u01, u11) -> (u11, u01 + q*u11)
        auto step_col = [&](uint64_t* x, size_t& lx, uint64_t* y, size_t& ly) {
            if (ly == 0) {
                if (lx > 0) std::memcpy(y, x, lx * sizeof(uint64_t));
                ly = lx; lx = 0;
                return;
            }
            size_t ql = static_cast<size_t>(N(q));
            size_t pl = ql + ly;
            if (ql >= ly) mpn_mul_dispatch(t0, gcpp_data(q), ql, y, ly);
            else mpn_mul_dispatch(t0, y, ly, gcpp_data(q), ql);
            pl = mpn_norm(t0, pl);
            // t0 += x
            if (lx > 0) {
                if (pl >= lx) {
                    uint64_t c = mpn_add(t0, t0, pl, x, lx);
                    if (c) t0[pl++] = c;
                } else {
                    uint64_t c = mpn_add(t0, x, lx, t0, pl);
                    pl = lx;
                    if (c) t0[pl++] = c;
                }
            }
            // x = old y; y = t0
            std::memcpy(x, y, ly * sizeof(uint64_t));
            lx = ly;
            if (pl > 0) std::memcpy(y, t0, pl * sizeof(uint64_t));
            ly = pl;
        };
        step_col(u00, l00, u10, l10);
        step_col(u01, l01, u11, l11);
    };

    auto apply_q1 = [&]() {
        odd = !odd;
        auto step_col1 = [&](uint64_t* x, size_t& lx, uint64_t* y, size_t& ly) {
            // (x, y) -> (y, x + y)
            if (lx == 0) {
                std::memcpy(x, y, ly * sizeof(uint64_t));
                lx = ly;
                return;
            }
            if (ly == 0) {
                std::memcpy(y, x, lx * sizeof(uint64_t));
                ly = lx; lx = 0;
                return;
            }
            size_t pl = std::max(lx, ly);
            uint64_t c = (lx >= ly) ? mpn_add(t0, x, lx, y, ly) : mpn_add(t0, y, ly, x, lx);
            if (c) t0[pl++] = c;
            std::memcpy(x, y, ly * sizeof(uint64_t));
            lx = ly;
            std::memcpy(y, t0, pl * sizeof(uint64_t));
            ly = pl;
        };
        step_col1(u00, l00, u10, l10);
        step_col1(u01, l01, u11, l11);
    };

    while (static_cast<size_t>(N(b)) > target_limbs) {
        if (BigInt::cmp_abs(a, b) < 0) break;
        LehmerMat lm;
        if (static_cast<size_t>(N(a)) >= 2 && static_cast<size_t>(N(a)) <= static_cast<size_t>(N(b)) + 1 && static_cast<size_t>(N(b)) >= target_limbs + 2 && lehmer_step_auto(gcpp_data(a), static_cast<size_t>(N(a)), gcpp_data(b), static_cast<size_t>(N(b)), lm)) {
            size_t na = static_cast<size_t>(N(a)), nb = static_cast<size_t>(N(b));
            apply_lehmer(gcpp_data(a), na, gcpp_data(b), nb, lm);
            a.set_size_and_normalize(na, false);
            b.set_size_and_normalize(nb, false);
            odd = (odd != lm.odd_steps);
            mul_add_col(u00, l00, u10, l10, lm);
            mul_add_col(u01, l01, u11, l11, lm);
            continue;
        }
        // Near target_limbs: use exact unshifted top-limb Lehmer so we NEVER overshoot target_limbs!
        if (static_cast<size_t>(N(a)) == target_limbs + 1 && static_cast<size_t>(N(b)) == target_limbs + 1 &&
            lehmer_step64_raw(gcpp_data(a)[target_limbs], gcpp_data(b)[target_limbs], lm)) {
            size_t na = static_cast<size_t>(N(a)), nb = static_cast<size_t>(N(b));
            apply_lehmer(gcpp_data(a), na, gcpp_data(b), nb, lm);
            a.set_size_and_normalize(na, false);
            b.set_size_and_normalize(nb, false);
            odd = (odd != lm.odd_steps);
            mul_add_col(u00, l00, u10, l10, lm);
            mul_add_col(u01, l01, u11, l11, lm);
            continue;
        }
        // Fast q = 1 check (avoids Knuth D division completely on Fibonacci-like steps!)
        BigInt diff = BigInt::sub(a, b);
        if (BigInt::cmp_abs(diff, b) < 0) {
            if (static_cast<size_t>(N(diff)) <= target_limbs) break;
            apply_q1();
            a = std::move(b);
            b = std::move(diff);
            continue;
        }
        BigInt q, r;
        BigInt::tdiv_qr(a, b, q, r);
        if (static_cast<size_t>(N(r)) <= target_limbs) break;
        apply_q(q);
        a = std::move(b);
        b = std::move(r);
    }

    HgcdMat M;
    M.odd = odd;
    M.u00 = BigInt(BigIntView{u00, l00, false});
    M.u01 = BigInt(BigIntView{u01, l01, false});
    M.u10 = BigInt(BigIntView{u10, l10, false});
    M.u11 = BigInt(BigIntView{u11, l11, false});
    return M;
}

static HgcdMat hgcd_rec(BigInt& a, BigInt& b) {
    size_t n = static_cast<size_t>(N(a));
    size_t s = (n + 1) >> 1;
    if (static_cast<size_t>(N(b)) <= s) return HgcdMat::identity();
    if (n < HGCD_THRESH) {
        return hgcd_basecase(a, b, s);
    }

    // 1. Recursive HGCD on top half (a >> s limbs, b >> s limbs), returning BOTH M1 and reduced (a_hi, b_hi)!
    BigInt a_hi = BigInt::tdiv_q_2exp(a, s * 64);
    BigInt b_hi = BigInt::tdiv_q_2exp(b, s * 64);
    HgcdMat M1 = hgcd_rec(a_hi, b_hi);
    M1.apply_split(a, b, s, a_hi, b_hi);
    if (a.is_neg() || b.is_neg() || static_cast<size_t>(N(b)) <= s) return M1;
    if (BigInt::cmp_abs(a, b) < 0) return M1;

    // 2. One Euclidean quotient step on full (a, b)
    BigInt q, r;
    BigInt::tdiv_qr(a, b, q, r);
    if (static_cast<size_t>(N(r)) <= s) return M1;
    M1.left_mul_quotient(q);
    a = std::move(b);
    b = std::move(r);
    if (static_cast<size_t>(N(b)) <= s) return M1;

    // 3. Second recursive HGCD on remaining excess above s
    size_t shift = (2 * s > static_cast<size_t>(N(a))) ? (2 * s - static_cast<size_t>(N(a))) : 0;
    if (shift >= static_cast<size_t>(N(a)) || static_cast<size_t>(N(b)) <= shift) return M1;
    BigInt a2 = BigInt::tdiv_q_2exp(a, shift * 64);
    BigInt b2 = BigInt::tdiv_q_2exp(b, shift * 64);
    if (BigInt::cmp_abs(a2, b2) < 0) return M1;
    HgcdMat M2 = hgcd_rec(a2, b2);
    M2.apply_split(a, b, shift, a2, b2);
    return HgcdMat::compose(M2, M1);
}

// Top-level Half-GCD reduction for BigInt::gcd: reduces (a, b) from n limbs down to ~n/2 limbs
// WITHOUT computing the expensive top-level HgcdMat::compose(M2, M1) (8 large multiplications saved!).
static void hgcd_reduce_inplace(BigInt& a, BigInt& b) {
    size_t n = static_cast<size_t>(N(a));
    size_t s = (n + 1) >> 1;
    if (static_cast<size_t>(N(b)) <= s) return;
    if (n < HGCD_THRESH) {
        (void)hgcd_basecase(a, b, s);
        a.abs_inplace();
        b.abs_inplace();
        if (BigInt::cmp_abs(a, b) < 0) std::swap(a, b);
        return;
    }

    // 1. Reduce top half and apply M1 only to low half (no compose!)
    BigInt a_hi = BigInt::tdiv_q_2exp(a, s * 64);
    BigInt b_hi = BigInt::tdiv_q_2exp(b, s * 64);
    HgcdMat M1 = hgcd_rec(a_hi, b_hi);
    M1.apply_split(a, b, s, a_hi, b_hi);
    a.abs_inplace();
    b.abs_inplace();
    if (BigInt::cmp_abs(a, b) < 0) std::swap(a, b);
    if (b.is_zero() || static_cast<size_t>(N(b)) <= s) return;

    // 2. One Euclidean remainder step (no matrix update needed!)
    BigInt r = BigInt::tdiv_r(a, b);
    a = std::move(b);
    b = std::move(r);
    if (b.is_zero() || static_cast<size_t>(N(b)) <= s) return;

    // 3. Second HGCD on remaining excess above s (no compose!)
    size_t shift = (2 * s > static_cast<size_t>(N(a))) ? (2 * s - static_cast<size_t>(N(a))) : 0;
    if (shift >= static_cast<size_t>(N(a)) || static_cast<size_t>(N(b)) <= shift) return;
    BigInt a2 = BigInt::tdiv_q_2exp(a, shift * 64);
    BigInt b2 = BigInt::tdiv_q_2exp(b, shift * 64);
    if (BigInt::cmp_abs(a2, b2) < 0) return;
    HgcdMat M2 = hgcd_rec(a2, b2);
    M2.apply_split(a, b, shift, a2, b2);
    a.abs_inplace();
    b.abs_inplace();
    if (BigInt::cmp_abs(a, b) < 0) std::swap(a, b);
}

// ============================================================================
// Section 7: Fast Base Conversion Tables
// ============================================================================

struct RadixInfo {
    uint64_t big_base; // largest power of radix fitting in uint64_t
    unsigned digits_per_limb;
    unsigned bits_per_digit; // non-zero only for power-of-2 radices (2,4,8,16,32)
};

static inline RadixInfo get_radix_info(int radix) noexcept {
    static constexpr RadixInfo table[37] = {
        {0, 0, 0}, {0, 0, 0},
        {0, 64, 1},                          // 2
        {12157665459056928801ULL, 40, 0},    // 3
        {0, 32, 2},                          // 4
        {7450580596923828125ULL, 27, 0},     // 5
        {4738381338321616896ULL, 24, 0},     // 6
        {3909821048582988049ULL, 22, 0},     // 7
        {0, 21, 3},                          // 8
        {12157665459056928801ULL, 20, 0},    // 9
        {10000000000000000000ULL, 19, 0},    // 10
        {5559917313492231481ULL, 18, 0},     // 11
        {2218611106740436992ULL, 17, 0},     // 12
        {8650415919381337933ULL, 17, 0},     // 13
        {2177953337809371136ULL, 16, 0},     // 14
        {6568408355712890625ULL, 16, 0},     // 15
        {0, 16, 4},                          // 16
        {2862423051509815793ULL, 15, 0},     // 17
        {6746640616477458432ULL, 15, 0},     // 18
        {15181127029874798299ULL, 15, 0},    // 19
        {1638400000000000000ULL, 14, 0},     // 20
        {3243919932521508681ULL, 14, 0},     // 21
        {6221821273427820544ULL, 14, 0},     // 22
        {11592836324538749809ULL, 14, 0},    // 23
        {876488338465357824ULL, 13, 0},      // 24
        {1490116119384765625ULL, 13, 0},     // 25
        {2481152873203736576ULL, 13, 0},     // 26
        {4052555153018976267ULL, 13, 0},     // 27
        {6502111422497947648ULL, 13, 0},     // 28
        {10260628712958602189ULL, 13, 0},    // 29
        {15943230000000000000ULL, 13, 0},    // 30
        {787662783788549761ULL, 12, 0},      // 31
        {0, 12, 5},                          // 32
        {1667889514952984961ULL, 12, 0},     // 33
        {2386420683693101056ULL, 12, 0},     // 34
        {3379220508056640625ULL, 12, 0},     // 35
        {4738381338321616896ULL, 12, 0},     // 36
    };
    return table[radix];
}

// Fast 2-digit decimal table
static constexpr char DEC_PAIRS[201] =
    "0001020304050607080910111213141516171819"
    "2021222324252627282930313233343536373839"
    "4041424344454647484950515253545556575859"
    "6061626364656667686970717273747576777879"
    "8081828384858687888990919293949596979899";

static inline void format_u64_dec_padded(uint64_t v, char* out, unsigned width) noexcept {
    unsigned pos = width;
    while (pos >= 2) {
        pos -= 2;
        uint64_t q = v / 100;
        unsigned rem = static_cast<unsigned>(v - q * 100);
        v = q;
        out[pos]     = DEC_PAIRS[2 * rem];
        out[pos + 1] = DEC_PAIRS[2 * rem + 1];
    }
    if (pos == 1) out[0] = static_cast<char>('0' + v);
}

static inline void format_u64_radix_padded(uint64_t v, int radix, const char* digs, char* out, unsigned width) noexcept {
    if (radix == 10) {
        format_u64_dec_padded(v, out, width);
        return;
    }
    for (unsigned i = width; i > 0; --i) {
        uint64_t q = v / static_cast<uint64_t>(radix);
        out[i - 1] = digs[v - q * static_cast<uint64_t>(radix)];
        v = q;
    }
}

// Fast pre-inverted single-limb divrem for repeated base conversion (avoids recomputing invert_limb!)
static inline uint64_t mpn_divrem_1_preinv(uint64_t* a, size_t n, uint64_t dn, uint64_t inv, unsigned s) noexcept {
    uint64_t r = 0;
    if (s == 0) {
        for (size_t i = n; i > 0; --i) {
            uint64_t ai = a[i - 1];
            if (r == 0 && ai < dn) { a[i - 1] = 0; r = ai; }
            else a[i - 1] = div_2by1(r, ai, dn, inv, r);
        }
        return r;
    } else {
        uint64_t prev = a[n - 1];
        r = prev >> (64 - s);
        for (size_t i = n - 1; i > 0; --i) {
            uint64_t cur = a[i - 1];
            uint64_t u0 = (prev << s) | (cur >> (64 - s));
            a[i] = div_2by1(r, u0, dn, inv, r);
            prev = cur;
        }
        a[0] = div_2by1(r, prev << s, dn, inv, r);
        return r >> s;
    }
}

struct DecPow10Entry {
    BigInt val;
    array<uint64_t> b_norm;
    array<uint64_t> inv;
    size_t zero_limbs = 0; // trailing zero limbs in b_norm (30% of limbs for 10^D!)
    unsigned shift = 0;
    bool has_inv = false;
    uint32_t uses = 0;
};

static void tdiv_qr_dec_pow10(const BigInt& a, DecPow10Entry& d, BigInt& q, BigInt& r) {
    size_t na = static_cast<size_t>(N(a));
    size_t nb = static_cast<size_t>(N(d.val));
    if (nb < 96 || na < nb || (na - nb) < 16) {
        BigInt::tdiv_qr(a, d.val, q, r);
        return;
    }
    ++d.uses;
    // Build & cache Barrett reciprocal for all reused levels (and large levels on 2nd call)
    if (!d.has_inv && (d.uses >= 2 || nb <= 1200)) {
        d.shift = static_cast<unsigned>(gcpp_countl_zero(gcpp_data(d.val)[nb - 1]));
        gcpp_resize(d.b_norm, nb);
        if (d.shift == 0) std::memcpy(gcpp_data(d.b_norm), gcpp_data(d.val), nb * sizeof(uint64_t));
        else mpn_lshift(gcpp_data(d.b_norm), gcpp_data(d.val), nb, d.shift);
        size_t zl = 0;
        while (zl < nb && d.b_norm[zl] == 0) ++zl;
        d.zero_limbs = zl;
        gcpp_resize(d.inv, nb);
        mpn_invert_appr(gcpp_data(d.inv), gcpp_data(d.b_norm), nb);
        d.has_inv = true;
    }
    if (!d.has_inv) {
        // 1-use root on cold call: use Burnikel-Ziegler (with q==0 skip) instead of throwing away a huge invert_appr!
        ScratchMark mark;
        unsigned s = static_cast<unsigned>(gcpp_countl_zero(gcpp_data(d.val)[nb - 1]));
        uint64_t* u = tl_scratch.alloc(na + 1);
        const uint64_t* bn = gcpp_data(d.val);
        size_t una = na;
        if (s == 0) {
            std::memcpy(u, gcpp_data(a), na * sizeof(uint64_t));
            u[na] = 0;
        } else {
            uint64_t* bbuf = tl_scratch.alloc(nb);
            mpn_lshift(bbuf, gcpp_data(d.val), nb, s);
            bn = bbuf;
            uint64_t hi = mpn_lshift(u, gcpp_data(a), na, s);
            u[na] = hi;
            if (hi) ++una;
        }
        size_t k = 0;
        while ((nb >> k) >= BZ_DIV_THRESH) ++k;
        size_t step = size_t(1) << k;
        size_t m = ((nb + step - 1) / step) * step;
        size_t pad = m - nb;
        uint64_t* bp = tl_scratch.alloc(m);
        if (pad > 0) std::memset(bp, 0, pad * sizeof(uint64_t));
        std::memcpy(bp + pad, bn, nb * sizeof(uint64_t));
        size_t up_len = una + pad;
        size_t t = (up_len + m - 1) / m;
        if (t < 2) t = 2;
        uint64_t* up = tl_scratch.alloc(t * m);
        std::memset(up, 0, t * m * sizeof(uint64_t));
        std::memcpy(up + pad, u, una * sizeof(uint64_t));
        uint64_t top_q = 0;
        if (mpn_cmp(up + (t - 1) * m, bp, m) >= 0) {
            mpn_sub_n(up + (t - 1) * m, up + (t - 1) * m, bp, m);
            top_q = 1;
        }
        uint64_t* qfull = tl_scratch.alloc((t - 1) * m);
        for (size_t i = t - 1; i > 0; --i) {
            bz_div_2n1n(qfull + (i - 1) * m, up + (i - 1) * m, bp, m);
        }
        size_t q_total = una - nb + 1;
        uint64_t* qp = q.alloc_uninit(q_total);
        size_t qcopy = std::min(q_total, (t - 1) * m);
        std::memcpy(qp, qfull, qcopy * sizeof(uint64_t));
        if (qcopy < q_total) {
            std::memset(qp + qcopy, 0, (q_total - qcopy) * sizeof(uint64_t));
            if (top_q && (t - 1) * m < q_total) qp[(t - 1) * m] = 1;
        }
        q.set_size_and_normalize(q_total, false);
        uint64_t* rp = r.alloc_uninit(nb);
        if (s == 0) std::memcpy(rp, up + pad, nb * sizeof(uint64_t));
        else mpn_rshift(rp, up + pad, nb, s);
        r.set_size_and_normalize(nb, false);
        return;
    }
    ScratchMark mark;
    size_t una = na;
    uint64_t* u = tl_scratch.alloc(na + 1);
    if (d.shift == 0) {
        std::memcpy(u, gcpp_data(a), na * sizeof(uint64_t));
    } else {
        uint64_t hi = mpn_lshift(u, gcpp_data(a), na, d.shift);
        if (hi) u[una++] = hi;
    }
    size_t t = (una + nb - 1) / nb;
    if (t < 2) t = 2;
    size_t work_len = t * nb;
    uint64_t* up = tl_scratch.alloc(work_len);
    std::memcpy(up, u, una * sizeof(uint64_t));
    if (work_len > una) std::memset(up + una, 0, (work_len - una) * sizeof(uint64_t));

    uint64_t top_q = 0;
    if (mpn_cmp(up + (t - 1) * nb, gcpp_data(d.b_norm), nb) >= 0) {
        mpn_sub_n(up + (t - 1) * nb, up + (t - 1) * nb, gcpp_data(d.b_norm), nb);
        top_q = 1;
    }
    uint64_t* qfull = tl_scratch.alloc((t - 1) * nb);
    uint64_t* prod = tl_scratch.alloc(2 * nb);
    size_t zl = d.zero_limbs;
    size_t bnz = nb - zl;
    for (size_t i = t - 1; i > 0; --i) {
        size_t pos = (i - 1) * nb;
        uint64_t* ub = up + pos;
        uint64_t* qb = qfull + pos;
        const uint64_t* u_hi = ub + nb;
        // 1. q_est = u_hi + floor(u_hi * inv / B^nb)
        mpn_mul_dispatch(prod, u_hi, nb, gcpp_data(d.inv), nb);
        uint64_t qc = mpn_add_n(qb, u_hi, prod + nb, nb);
        if (qc != 0) for (size_t j = 0; j < nb; ++j) qb[j] = ~0ULL;
        // 2. Skip trailing zero limbs of 10^D when computing q * b_norm!
        if (zl > 0) std::memset(prod, 0, zl * sizeof(uint64_t));
        mpn_mul_dispatch(prod + zl, qb, nb, gcpp_data(d.b_norm) + zl, bnz);
        mpn_sub_n(ub, ub, prod, nb + 1);
        while (ub[nb] > 0 || mpn_cmp(ub, gcpp_data(d.b_norm), nb) >= 0) {
            uint64_t sb = mpn_sub_n(ub, ub, gcpp_data(d.b_norm), nb);
            ub[nb] -= sb;
            mpn_add_1(qb, qb, nb, 1);
        }
    }
    size_t q_total = una - nb + 1;
    uint64_t* qp = q.alloc_uninit(q_total);
    size_t qcopy = std::min(q_total, (t - 1) * nb);
    std::memcpy(qp, qfull, qcopy * sizeof(uint64_t));
    if (qcopy < q_total) {
        std::memset(qp + qcopy, 0, (q_total - qcopy) * sizeof(uint64_t));
        if (top_q && (t - 1) * nb < q_total) qp[(t - 1) * nb] = 1;
    }
    q.set_size_and_normalize(q_total, false);

    uint64_t* rp = r.alloc_uninit(nb);
    if (d.shift == 0) std::memcpy(rp, up, nb * sizeof(uint64_t));
    else mpn_rshift(rp, up, nb, d.shift);
    r.set_size_and_normalize(nb, false);
}

// Subquadratic base-conversion helper for to_string
static void to_string_dc(const BigInt& x, array<DecPow10Entry>* dec_pows,
                         array<BigInt>* gen_pows, size_t level,
                         unsigned dpl, int radix, const char* digs,
                         uint64_t dn, uint64_t inv, unsigned s_shift,
                         string& out, bool pad_zeros, size_t expected_digits) {
    if (static_cast<size_t>(N(x)) <= 16 || level == 0) {
        ScratchMark mark;
        size_t n = static_cast<size_t>(N(x));
        if (n == 0) {
            if (pad_zeros) str_append_n(out, expected_digits, '0');
            else out << '0';
            return;
        }
        uint64_t* tmp = tl_scratch.alloc(n);
        uint64_t* chunks = tl_scratch.alloc(n * 2 + 4);
        std::memcpy(tmp, gcpp_data(x), n * sizeof(uint64_t));
        size_t total_chunks = 0;
        while (n > 0) {
            chunks[total_chunks++] = mpn_divrem_1_preinv(tmp, n, dn, inv, s_shift);
            n = mpn_norm(tmp, n);
        }
        char buf[64];
        if (pad_zeros) {
            size_t total_out_digits = total_chunks * dpl;
            if (total_out_digits < expected_digits) {
                str_append_n(out, expected_digits - total_out_digits, '0');
            }
            size_t top_idx = total_chunks - 1;
            size_t rem_digits = expected_digits - (total_chunks - 1) * dpl;
            if (rem_digits > dpl) {
                str_append_n(out, rem_digits - dpl, '0');
                rem_digits = dpl;
            }
            format_u64_radix_padded(chunks[top_idx], radix, digs, buf, static_cast<unsigned>(rem_digits));
            str_append(out, buf, rem_digits);
            for (size_t i = top_idx; i > 0; --i) {
                format_u64_radix_padded(chunks[i - 1], radix, digs, buf, dpl);
                str_append(out, buf, dpl);
            }
        } else {
            uint64_t top = chunks[total_chunks - 1];
            int len = 0;
            do {
                buf[len++] = digs[top % static_cast<uint64_t>(radix)];
                top /= static_cast<uint64_t>(radix);
            } while (top > 0);
            for (int i = len - 1; i >= 0; --i) out << buf[i];
            for (size_t i = total_chunks - 1; i > 0; --i) {
                format_u64_radix_padded(chunks[i - 1], radix, digs, buf, dpl);
                str_append(out, buf, dpl);
            }
        }
        return;
    }

    size_t low_digits = static_cast<size_t>(dpl) << (level - 1);
    const BigInt& dv = (radix == 10) ? (*dec_pows)[level - 1].val : (*gen_pows)[level - 1];
    if (BigInt::cmp_abs(x, dv) < 0) {
        if (pad_zeros && expected_digits > low_digits) {
            str_append_n(out, expected_digits - low_digits, '0');
        }
        to_string_dc(x, dec_pows, gen_pows, level - 1, dpl, radix, digs, dn, inv, s_shift, out, pad_zeros, low_digits);
        return;
    }
    BigInt q, r;
    if (radix == 10) tdiv_qr_dec_pow10(x, (*dec_pows)[level - 1], q, r);
    else BigInt::tdiv_qr(x, dv, q, r);
    to_string_dc(q, dec_pows, gen_pows, level - 1, dpl, radix, digs, dn, inv, s_shift, out, pad_zeros,
                 pad_zeros ? (expected_digits - low_digits) : 0);
    to_string_dc(r, dec_pows, gen_pows, level - 1, dpl, radix, digs, dn, inv, s_shift, out, true, low_digits);
}

// ============================================================================
// Section 8: Montgomery Multiplication & Sliding-Window Modular Exponentiation
// ============================================================================

// CIOS Montgomery multiplication: r[0..n-1] = a * b * R^-1 mod m (a, b < m, m odd, n limbs)
// n0inv = -m[0]^-1 mod 2^64. r must not alias a or b.
static void mont_mul_cios(uint64_t* r, const uint64_t* a, const uint64_t* b,
                          const uint64_t* m, size_t n, uint64_t n0inv) noexcept {
    ScratchMark mark;
    uint64_t* t = tl_scratch.alloc(n + 2);
    std::memset(t, 0, (n + 2) * sizeof(uint64_t));

    for (size_t i = 0; i < n; ++i) {
        // t += a * b[i]
        uint64_t c1 = mpn_addmul_1(t, a, n, b[i]);
        uint64_t s1 = t[n] + c1;
        uint64_t c2 = (s1 < c1) ? 1ULL : 0ULL;
        t[n] = s1;
        t[n + 1] = c2;

        // Reduction step: u = t[0] * n0inv mod 2^64; t = (t + u * m) / 2^64
        uint64_t u = t[0] * n0inv;
#if defined(__SIZEOF_INT128__)
        u128 p0 = static_cast<u128>(m[0]) * u + t[0];
        uint64_t cy = static_cast<uint64_t>(p0 >> 64);
        for (size_t j = 1; j < n; ++j) {
            u128 p = static_cast<u128>(m[j]) * u + t[j] + cy;
            t[j - 1] = static_cast<uint64_t>(p);
            cy = static_cast<uint64_t>(p >> 64);
        }
        u128 last = static_cast<u128>(t[n]) + cy;
        t[n - 1] = static_cast<uint64_t>(last);
        t[n] = t[n + 1] + static_cast<uint64_t>(last >> 64);
#else
        uint64_t cy = mpn_addmul_1(t, m, n, u);
        uint64_t s2 = t[n] + cy;
        uint64_t cy2 = (s2 < cy) ? 1ULL : 0ULL;
        for (size_t j = 0; j < n - 1; ++j) t[j] = t[j + 1];
        t[n - 1] = s2;
        t[n] = t[n + 1] + cy2;
#endif
    }
    if (t[n] != 0 || mpn_cmp(t, m, n) >= 0) {
        mpn_sub_n(r, t, m, n);
    } else {
        std::memcpy(r, t, n * sizeof(uint64_t));
    }
}

// Separate fast multiply/square + carry-saving Montgomery REDC (zero-dep Chain-1 mpn_addmul_1)!
static void mont_redc(uint64_t* __restrict__ r, uint64_t* __restrict__ prod,
                      const uint64_t* __restrict__ m, size_t n, uint64_t n0inv) noexcept {
    uint64_t cy = 0;
    for (size_t i = 0; i < n; ++i) {
        uint64_t u = prod[i] * n0inv;
        uint64_t c = mpn_addmul_1(prod + i, m, n, u);
        uint64_t c2 = 0;
        prod[i + n] = add_carry(prod[i + n], c, cy, c2);
        cy = c2;
    }
    if (cy != 0 || mpn_cmp(prod + n, m, n) >= 0) {
        mpn_sub_n(r, prod + n, m, n);
    } else {
        std::memcpy(r, prod + n, n * sizeof(uint64_t));
    }
}

// Powm for odd modulus m > 1 using sliding-window Montgomery exponentiation
static BigInt powm_odd(BigIntView base, BigIntView exp, BigIntView m) {
    size_t n = m.len;
    uint64_t n0inv = 0ULL - modinv_u64(m.limbs[0]);

    // Reduce base mod m into [0, m-1]
    BigInt bmod = BigInt::mod(base, m);
    if (bmod.is_zero()) return BigInt(int64_t(0));
    if (exp.is_zero()) return BigInt(int64_t(1));

    // Compute R^2 mod m (where R = 2^(64*n))
    BigInt r2_num;
    uint64_t* rp = r2_num.alloc_uninit(2 * n + 1);
    std::memset(rp, 0, 2 * n * sizeof(uint64_t));
    rp[2 * n] = 1;
    BigInt r2 = BigInt::mod(r2_num, m);

    // Choose optimal sliding window size w based on exponent bit length
    size_t ebits = exp.bit_length_abs();
    unsigned w = 1;
    if (ebits >= 1536) w = 6;
    else if (ebits >= 512) w = 5;
    else if (ebits >= 128) w = 4;
    else if (ebits >= 32) w = 3;
    else if (ebits >= 8) w = 2;

    size_t table_cnt = size_t(1) << (w - 1); // odd powers: 1, 3, 5, ..., 2^w - 1

    ScratchMark mark;
    uint64_t* m_limbs  = tl_scratch.alloc(n);
    uint64_t* r2_limbs = tl_scratch.alloc(n);
    uint64_t* b_pad    = tl_scratch.alloc(n);
    uint64_t* tbl      = tl_scratch.alloc(table_cnt * n);
    uint64_t* acc      = tl_scratch.alloc(n);
    uint64_t* tmp      = tl_scratch.alloc(n);
    uint64_t* prod     = tl_scratch.alloc(2 * n);

    std::memcpy(m_limbs, m.limbs, n * sizeof(uint64_t));
    std::memset(r2_limbs, 0, n * sizeof(uint64_t));
    if (static_cast<size_t>(N(r2)) > 0) std::memcpy(r2_limbs, gcpp_data(r2), static_cast<size_t>(N(r2)) * sizeof(uint64_t));
    std::memset(b_pad, 0, n * sizeof(uint64_t));
    std::memcpy(b_pad, gcpp_data(bmod), static_cast<size_t>(N(bmod)) * sizeof(uint64_t));

    auto do_mont_sqr = [&](uint64_t* out_r, const uint64_t* a) {
        if (n < KARATSUBA_SQR_THRESH) mpn_sqr_basecase(prod, a, n);
        else mpn_sqr_dispatch(prod, a, n);
        mont_redc(out_r, prod, m_limbs, n, n0inv);
    };
    auto do_mont_mul = [&](uint64_t* out_r, const uint64_t* a, const uint64_t* b) {
        if (n <= 12) {
            mont_mul_cios(out_r, a, b, m_limbs, n, n0inv);
        } else {
            if (n < KARATSUBA_MUL_THRESH) mpn_mul_basecase(prod, a, n, b, n);
            else mpn_mul_dispatch(prod, a, n, b, n);
            mont_redc(out_r, prod, m_limbs, n, n0inv);
        }
    };

    do_mont_mul(tbl, b_pad, r2_limbs);

    if (table_cnt > 1) {
        uint64_t* b2 = tl_scratch.alloc(n);
        do_mont_sqr(b2, tbl);
        for (size_t i = 1; i < table_cnt; ++i) {
            do_mont_mul(tbl + i * n, tbl + (i - 1) * n, b2);
        }
    }

    auto get_bit = [&](size_t idx) -> bool {
        return ((exp.limbs[idx >> 6] >> (idx & 63)) & 1ULL) != 0;
    };

    // Initialize acc from top window (avoiding initial squaring of 1!)
    size_t bit = ebits;
    bool started = false;
    while (bit > 0) {
        if (!get_bit(bit - 1)) {
            --bit;
            if (started) {
                do_mont_sqr(tmp, acc);
                std::swap(acc, tmp);
            }
            continue;
        }
        // Extract sliding window of length <= w ending in a 1-bit
        size_t wlen = std::min(static_cast<size_t>(w), bit);
        uint32_t win = 0;
        for (size_t k = 0; k < wlen; ++k) {
            win = (win << 1) | (get_bit(bit - 1 - k) ? 1U : 0U);
        }
        while ((win & 1U) == 0) {
            win >>= 1;
            --wlen;
        }
        if (!started) {
            std::memcpy(acc, tbl + (win >> 1) * n, n * sizeof(uint64_t));
            started = true;
        } else {
            for (size_t k = 0; k < wlen; ++k) {
                do_mont_sqr(tmp, acc);
                std::swap(acc, tmp);
            }
            do_mont_mul(tmp, acc, tbl + (win >> 1) * n);
            std::swap(acc, tmp);
        }
        bit -= wlen;
    }

    // Convert out of Montgomery form: mont_redc(acc) directly (no multiply by 1!)
    std::memcpy(prod, acc, n * sizeof(uint64_t));
    std::memset(prod + n, 0, n * sizeof(uint64_t));
    mont_redc(tmp, prod, m_limbs, n, n0inv);

    BigInt res;
    uint64_t* out = res.alloc_uninit(n);
    std::memcpy(out, tmp, n * sizeof(uint64_t));
    res.set_size_and_normalize(n, false);
    return res;
}

// Powm modulo 2^k (k >= 1)
static BigInt powm_2exp(BigIntView base, BigIntView exp, size_t k) {
    size_t nlimbs = (k + 63) >> 6;
    uint64_t top_mask = ((k & 63) == 0) ? ~0ULL : ((1ULL << (k & 63)) - 1ULL);

    // Reduce base mod 2^k (two's complement if negative!)
    BigInt b;
    if (base.sgn() >= 0) {
        uint64_t* bp = b.alloc_uninit(nlimbs);
        size_t copy = std::min(base.len, nlimbs);
        if (copy > 0) std::memcpy(bp, base.limbs, copy * sizeof(uint64_t));
        if (copy < nlimbs) std::memset(bp + copy, 0, (nlimbs - copy) * sizeof(uint64_t));
        bp[nlimbs - 1] &= top_mask;
        b.set_size_and_normalize(nlimbs, false);
    } else {
        BigInt mod2k = BigInt::mul_2exp(BigInt(int64_t(1)), k);
        b = BigInt::mod(base, mod2k);
    }

    BigInt acc(int64_t(1));
    size_t ebits = exp.bit_length_abs();
    auto mask_to_k = [&](BigInt& x) {
        if (static_cast<size_t>(N(x)) > nlimbs) x.set_size_and_normalize(nlimbs, false);
        if (static_cast<size_t>(N(x)) == nlimbs) {
            gcpp_data(x)[nlimbs - 1] &= top_mask;
            x.normalize();
        }
    };
    for (size_t i = ebits; i > 0; --i) {
        acc = BigInt::sqr(acc);
        mask_to_k(acc);
        if (((exp.limbs[(i - 1) >> 6] >> ((i - 1) & 63)) & 1ULL) != 0) {
            acc = BigInt::mul(acc, b);
            mask_to_k(acc);
        }
    }
    return acc;
}

// ============================================================================
// Section 9: Two's-Complement Bitwise Helper
// ============================================================================

// Helper for reading limb i of two's complement representation of a signed BigIntView.
// For a >= 0: limb(i) = (i < len ? limbs[i] : 0).
// For a < 0:  twos(-M) = ~(M - 1). Precompute index of first non-zero limb of M:
//   - For i < first_nz:  M_minus_1[i] = ~0ULL -> ~M_minus_1[i] = 0ULL
//   - For i == first_nz: ~((M[i]) - 1) = -M[i] (mod 2^64)
//   - For i > first_nz:  ~(i < len ? M[i] : 0ULL)
struct TwosReader {
    const uint64_t* limbs;
    size_t len;
    bool neg;
    size_t first_nz;

    explicit TwosReader(BigIntView v) noexcept
        : limbs(v.limbs), len(v.len), neg(v.neg && v.len > 0), first_nz(0) {
        if (neg) {
            while (first_nz < len && limbs[first_nz] == 0) ++first_nz;
        }
    }

    uint64_t limb(size_t i) const noexcept {
        if (!neg) return (i < len) ? limbs[i] : 0ULL;
        if (i < first_nz) return 0ULL;
        if (i == first_nz) return 0ULL - limbs[i];
        return (i < len) ? ~limbs[i] : ~0ULL;
    }

    uint64_t sign_ext() const noexcept { return neg ? ~0ULL : 0ULL; }
};

} // anonymous namespace

// ============================================================================
// Section 10: Public BigInt Implementation
// ============================================================================

uint64_t* BigInt::pool_alloc(size_t n, uint32_t& out_cap) {
    return g_limb_pool.alloc(n, out_cap);
}

void BigInt::pool_free(uint64_t* p, uint32_t cap) noexcept {
    g_limb_pool.free(p, cap);
}

double BigInt::to_double_exact(BigIntView a) noexcept {
    if (a.len == 0) return 0.0;
    size_t bits = a.bit_length_abs();
    double res;
    if (bits <= 53) {
        res = static_cast<double>(a.limbs[0]);
    } else if (bits > 1024) {
        res = std::numeric_limits<double>::infinity();
    } else {
        // Extract top 53 bits, half bit (bit shift-1), and sticky bit (any bit < shift-1)
        size_t shift = bits - 53;
        size_t top_idx = a.len - 1;
        unsigned top_bits = static_cast<unsigned>(64 - gcpp_countl_zero(a.limbs[top_idx]));
        uint64_t mant53;
        if (top_bits >= 53) {
            mant53 = a.limbs[top_idx] >> (top_bits - 53);
        } else {
            unsigned need = 53 - top_bits;
            mant53 = (a.limbs[top_idx] << need) | (a.limbs[top_idx - 1] >> (64 - need));
        }
        // Half bit is at bit position (shift - 1)
        size_t h_idx = (shift - 1) >> 6;
        unsigned h_bit = static_cast<unsigned>((shift - 1) & 63);
        bool half = ((a.limbs[h_idx] >> h_bit) & 1ULL) != 0;
        bool sticky = false;
        if (h_bit > 0 && (a.limbs[h_idx] & ((1ULL << h_bit) - 1ULL)) != 0) {
            sticky = true;
        } else {
            for (size_t i = 0; i < h_idx; ++i) {
                if (a.limbs[i] != 0) { sticky = true; break; }
            }
        }
        if (half && (sticky || (mant53 & 1ULL) != 0)) {
            ++mant53;
        }
        res = std::ldexp(static_cast<double>(mant53), static_cast<int>(shift));
    }
    return a.neg ? -res : res;
}

int BigInt::cmp_abs(BigIntView a, BigIntView b) noexcept {
    return mpn_cmp_var(a.limbs, a.len, b.limbs, b.len);
}

int BigInt::cmp(BigIntView a, BigIntView b) noexcept {
    int sa = a.sgn(), sb = b.sgn();
    if (sa != sb) return (sa < sb) ? -1 : 1;
    if (sa == 0) return 0;
    int c = mpn_cmp_var(a.limbs, a.len, b.limbs, b.len);
    return (sa > 0) ? c : -c;
}

int BigInt::cmp_ui(BigIntView a, uint64_t b) noexcept {
    SmallIntView sb(b);
    return cmp(a, sb);
}

int BigInt::cmp_si(BigIntView a, int64_t b) noexcept {
    SmallIntView sb(b);
    return cmp(a, sb);
}

BigInt BigInt::neg(BigIntView a) {
    BigInt r(a);
    r.negate();
    return r;
}

BigInt BigInt::abs(BigIntView a) {
    BigInt r(a);
    r.abs_inplace();
    return r;
}

BigInt BigInt::add(BigIntView a, BigIntView b) {
    if (a.is_zero()) return BigInt(b);
    if (b.is_zero()) return BigInt(a);
    BigInt res;
    if (a.neg == b.neg) {
        size_t m = std::max(a.len, b.len);
        uint64_t* rp = res.alloc_uninit(m + 1);
        uint64_t c = (a.len >= b.len) ? mpn_add(rp, a.limbs, a.len, b.limbs, b.len)
                                      : mpn_add(rp, b.limbs, b.len, a.limbs, a.len);
        rp[m] = c;
        res.set_size_and_normalize(m + (c ? 1 : 0), a.neg);
    } else {
        int c = mpn_cmp_var(a.limbs, a.len, b.limbs, b.len);
        if (c == 0) return res;
        if (c > 0) {
            uint64_t* rp = res.alloc_uninit(a.len);
            mpn_sub(rp, a.limbs, a.len, b.limbs, b.len);
            res.set_size_and_normalize(a.len, a.neg);
        } else {
            uint64_t* rp = res.alloc_uninit(b.len);
            mpn_sub(rp, b.limbs, b.len, a.limbs, a.len);
            res.set_size_and_normalize(b.len, b.neg);
        }
    }
    return res;
}

BigInt BigInt::sub(BigIntView a, BigIntView b) {
    return add(a, b.neg_view());
}

BigInt BigInt::add_ui(BigIntView a, uint64_t b) {
    SmallIntView sb(b);
    return add(a, sb);
}

BigInt BigInt::sub_ui(BigIntView a, uint64_t b) {
    SmallIntView sb(b);
    return sub(a, sb);
}

BigInt BigInt::mul_ui(BigIntView a, uint64_t b) {
    if (a.is_zero() || b == 0) return BigInt();
    if (b == 1) return BigInt(a);
    BigInt res;
    uint64_t* rp = res.alloc_uninit(a.len + 1);
    rp[a.len] = mpn_mul_1(rp, a.limbs, a.len, b);
    res.set_size_and_normalize(a.len + 1, a.neg);
    return res;
}

BigInt BigInt::mul_si(BigIntView a, int64_t b) {
    if (a.is_zero() || b == 0) return BigInt();
    uint64_t ub = (b > 0) ? static_cast<uint64_t>(b) : (0ULL - static_cast<uint64_t>(b));
    bool neg = a.neg != (b < 0);
    if (ub == 1) {
        BigInt res(a);
        if (neg != res.is_neg()) res.negate();
        return res;
    }
    BigInt res;
    uint64_t* rp = res.alloc_uninit(a.len + 1);
    rp[a.len] = mpn_mul_1(rp, a.limbs, a.len, ub);
    res.set_size_and_normalize(a.len + 1, neg);
    return res;
}

BigInt BigInt::mul(BigIntView a, BigIntView b) {
    if (a.is_zero() || b.is_zero()) return BigInt();
    bool neg = (a.neg != b.neg);
    if (a.limbs == b.limbs && a.len == b.len) {
        BigInt res = sqr(a);
        if (neg) res.negate();
        return res;
    }
    // Fast 1x1 path
    if (a.len == 1 && b.len == 1) {
        BigInt res;
        uint64_t hi, lo = mul64(a.limbs[0], b.limbs[0], hi);
        uint64_t* rp = res.alloc_uninit(hi ? 2 : 1);
        rp[0] = lo;
        if (hi) rp[1] = hi;
        res.set_size_and_normalize(hi ? 2 : 1, neg);
        return res;
    }
    BigInt res;
    size_t rn = a.len + b.len;
    uint64_t* rp = res.alloc_uninit(rn);
    mpn_mul_dispatch(rp, a.limbs, a.len, b.limbs, b.len);
    res.set_size_and_normalize(rn, neg);
    return res;
}

BigInt BigInt::sqr(BigIntView a) {
    if (a.is_zero()) return BigInt();
    BigInt res;
    size_t rn = 2 * a.len;
    uint64_t* rp = res.alloc_uninit(rn);
    mpn_sqr_dispatch(rp, a.limbs, a.len);
    res.set_size_and_normalize(rn, false);
    return res;
}

void BigInt::tdiv_qr(BigIntView a, BigIntView b, BigInt& q, BigInt& r) {
    assert(!b.is_zero());
    if (a.is_zero()) {
        q.set_zero();
        r.set_zero();
        return;
    }
    int cmp = mpn_cmp_var(a.limbs, a.len, b.limbs, b.len);
    if (cmp < 0) {
        q.set_zero();
        r = BigInt(a);
        return;
    }
    if (cmp == 0) {
        q.set_si((a.neg == b.neg) ? 1 : -1);
        r.set_zero();
        return;
    }
    bool qneg = (a.neg != b.neg);
    bool rneg = a.neg;
    size_t qlen = a.len - b.len + 1;
    size_t rlen = b.len;
    BigInt q_out, r_out;
    uint64_t* qp = q_out.alloc_uninit(qlen);
    uint64_t* rp = r_out.alloc_uninit(rlen);
    mpn_divrem(qp, rp, a.limbs, a.len, b.limbs, b.len);
    q_out.set_size_and_normalize(qlen, qneg);
    r_out.set_size_and_normalize(rlen, rneg);
    q = std::move(q_out);
    r = std::move(r_out);
}

BigInt BigInt::tdiv_q(BigIntView a, BigIntView b) {
    assert(!b.is_zero());
    if (a.is_zero()) return BigInt();
    int cmp = mpn_cmp_var(a.limbs, a.len, b.limbs, b.len);
    if (cmp < 0) return BigInt();
    if (cmp == 0) return BigInt((a.neg == b.neg) ? int64_t(1) : int64_t(-1));
    size_t qlen = a.len - b.len + 1;
    BigInt q;
    uint64_t* qp = q.alloc_uninit(qlen);
    mpn_divrem(qp, nullptr, a.limbs, a.len, b.limbs, b.len);
    q.set_size_and_normalize(qlen, a.neg != b.neg);
    return q;
}

BigInt BigInt::tdiv_r(BigIntView a, BigIntView b) {
    assert(!b.is_zero());
    if (a.is_zero()) return BigInt();
    int cmp = mpn_cmp_var(a.limbs, a.len, b.limbs, b.len);
    if (cmp < 0) return BigInt(a);
    if (cmp == 0) return BigInt();
    if (b.len == 1) {
        uint64_t rem = mpn_mod_1(a.limbs, a.len, b.limbs[0]);
        BigInt r(rem);
        if (a.neg) r.negate();
        return r;
    }
    BigInt r;
    uint64_t* rp = r.alloc_uninit(b.len);
    mpn_divrem(nullptr, rp, a.limbs, a.len, b.limbs, b.len);
    r.set_size_and_normalize(b.len, a.neg);
    return r;
}

void BigInt::fdiv_qr(BigIntView a, BigIntView b, BigInt& q, BigInt& r) {
    tdiv_qr(a, b, q, r);
    if (!r.is_zero() && (a.neg != b.neg)) {
        q = sub_ui(q, 1);
        r = add(r, b);
    }
}

BigInt BigInt::fdiv_q(BigIntView a, BigIntView b) {
    BigInt q, r;
    fdiv_qr(a, b, q, r);
    return q;
}

BigInt BigInt::fdiv_r(BigIntView a, BigIntView b) {
    BigInt r = tdiv_r(a, b);
    if (!r.is_zero() && (a.neg != b.neg)) {
        r = add(r, b);
    }
    return r;
}

BigInt BigInt::cdiv_q(BigIntView a, BigIntView b) {
    BigInt q, r;
    tdiv_qr(a, b, q, r);
    if (!r.is_zero() && (a.neg == b.neg)) {
        q = add_ui(q, 1);
    }
    return q;
}

BigInt BigInt::divexact(BigIntView a, BigIntView b) {
    if (b.len == 1) {
        uint64_t d = b.limbs[0];
        if (d == 1) return b.neg ? neg(a) : BigInt(a);
        if (a.is_zero()) return BigInt();
        // Hensel odd exact division mod 2^64 (super fast for 1-limb divisors!)
        unsigned tz = static_cast<unsigned>(gcpp_countr_zero(d));
        uint64_t odd_d = d >> tz;
        uint64_t inv = modinv_u64(odd_d);
        BigInt res;
        uint64_t* rp = res.alloc_uninit(a.len);
        const uint64_t* src = a.limbs;
        size_t n = a.len;
        if (tz > 0) {
            mpn_rshift(rp, src, n, tz);
            src = rp;
            n = mpn_norm(rp, n);
        }
        uint64_t borrow = 0;
        for (size_t i = 0; i < n; ++i) {
            uint64_t ai = src[i];
            uint64_t s = ai - borrow;
            uint64_t b1 = (ai < borrow) ? 1ULL : 0ULL;
            uint64_t qi = s * inv;
            rp[i] = qi;
            uint64_t hi;
            mul64(qi, odd_d, hi);
            borrow = hi + b1;
        }
        res.set_size_and_normalize(n, a.neg != b.neg);
        return res;
    }
    return tdiv_q(a, b);
}

BigInt BigInt::mod(BigIntView a, BigIntView b) {
    BigInt r = tdiv_r(a, b);
    if (r.is_neg()) r = add(r, b.abs_view());
    return r;
}

int64_t BigInt::tdiv_r_si(BigIntView a, int64_t d) noexcept {
    assert(d != 0);
    if (a.is_zero()) return 0;
    uint64_t ud = (d > 0) ? static_cast<uint64_t>(d) : (0ULL - static_cast<uint64_t>(d));
    uint64_t rem = mpn_mod_1(a.limbs, a.len, ud);
    return a.neg ? -static_cast<int64_t>(rem) : static_cast<int64_t>(rem);
}

uint64_t BigInt::mod_ui(BigIntView a, uint64_t d) noexcept {
    assert(d != 0);
    if (a.is_zero()) return 0;
    uint64_t rem = mpn_mod_1(a.limbs, a.len, d);
    if (a.neg && rem != 0) rem = d - rem;
    return rem;
}

BigInt BigInt::gcd(BigIntView a_in, BigIntView b_in) {
    if (a_in.is_zero()) return abs(b_in);
    if (b_in.is_zero()) return abs(a_in);
    if (a_in.len == 1 && b_in.len == 1) {
        return BigInt(gcd_u64(a_in.limbs[0], b_in.limbs[0]));
    }
    // Single-limb fast path
    if (a_in.len == 1) {
        uint64_t r = mpn_mod_1(b_in.limbs, b_in.len, a_in.limbs[0]);
        return BigInt(gcd_u64(a_in.limbs[0], r));
    }
    if (b_in.len == 1) {
        uint64_t r = mpn_mod_1(a_in.limbs, a_in.len, b_in.limbs[0]);
        return BigInt(gcd_u64(b_in.limbs[0], r));
    }

    BigInt a = abs(a_in);
    BigInt b = abs(b_in);
    if (cmp_abs(a, b) < 0) std::swap(a, b);

    // Strip common factors of 2
    size_t ta = scan1(a, 0);
    size_t tb = scan1(b, 0);
    size_t common2 = std::min(ta, tb);
    if (ta > 0) a = tdiv_q_2exp(a, ta);
    if (tb > 0) b = tdiv_q_2exp(b, tb);
    if (cmp_abs(a, b) < 0) std::swap(a, b);

    while (!b.is_zero()) {
        if (static_cast<size_t>(N(a)) == 1) {
            a.set_ui(gcd_u64(gcpp_data(a)[0], gcpp_data(b)[0]));
            break;
        }
        if (static_cast<size_t>(N(b)) == 1) {
            uint64_t r = mpn_mod_1(gcpp_data(a), static_cast<size_t>(N(a)), gcpp_data(b)[0]);
            a.set_ui(gcd_u64(gcpp_data(b)[0], r));
            break;
        }
        // Use Half-GCD reduce for large inputs (skips top-level matrix compose!)
        if (static_cast<size_t>(N(a)) >= HGCD_THRESH && static_cast<size_t>(N(b)) > (static_cast<size_t>(N(a)) >> 1)) {
            size_t old_sum = static_cast<size_t>(N(a)) + static_cast<size_t>(N(b));
            hgcd_reduce_inplace(a, b);
            if (b.is_zero()) break;
            if (static_cast<size_t>(N(a)) + static_cast<size_t>(N(b)) < old_sum) {
                if (static_cast<size_t>(N(a)) > static_cast<size_t>(N(b)) + 1) {
                    a = tdiv_r(a, b);
                    std::swap(a, b);
                }
                continue;
            }
            // Fall through to Lehmer / division if HGCD made no progress
        }
        // Double-limb Lehmer step when a and b have similar sizes
        if (static_cast<size_t>(N(a)) <= static_cast<size_t>(N(b)) + 1) {
            LehmerMat lm;
            if (lehmer_step_auto(gcpp_data(a), static_cast<size_t>(N(a)), gcpp_data(b), static_cast<size_t>(N(b)), lm)) {
                size_t na = static_cast<size_t>(N(a)), nb = static_cast<size_t>(N(b));
                apply_lehmer(gcpp_data(a), na, gcpp_data(b), nb, lm);
                a.set_size_and_normalize(na, false);
                b.set_size_and_normalize(nb, false);
                if (cmp_abs(a, b) < 0) std::swap(a, b);
                continue;
            }
        }
        // Fallback single division step
        BigInt r = tdiv_r(a, b);
        a = std::move(b);
        b = std::move(r);
    }
    return common2 > 0 ? mul_2exp(a, common2) : a;
}

BigInt BigInt::lcm(BigIntView a, BigIntView b) {
    if (a.is_zero() || b.is_zero()) return BigInt();
    BigInt g = gcd(a, b);
    BigInt q = divexact(a.abs_view(), g);
    return mul(q, b.abs_view());
}

bool BigInt::modinv(BigIntView a_in, BigIntView m_in, BigInt& out) {
    if (m_in.sgn() <= 0) return false;
    if (m_in.len == 1 && m_in.limbs[0] == 1) {
        out.set_zero();
        return true;
    }
    BigInt r0(m_in);
    BigInt r1 = mod(a_in, m_in);
    if (r1.is_zero()) return false;
    BigInt t0(int64_t(0)), t1(int64_t(1));
    while (!r1.is_zero()) {
        BigInt q, r;
        tdiv_qr(r0, r1, q, r);
        BigInt t2 = sub(t0, mul(q, t1));
        r0 = std::move(r1);
        r1 = std::move(r);
        t0 = std::move(t1);
        t1 = std::move(t2);
    }
    if (static_cast<size_t>(N(r0)) != 1 || gcpp_data(r0)[0] != 1) return false;
    if (t0.is_neg()) t0 = add(t0, m_in);
    out = std::move(t0);
    return true;
}

BigInt BigInt::pow_ui(BigIntView base, uint64_t exp) {
    if (exp == 0) return BigInt(int64_t(1));
    if (base.is_zero()) return BigInt();
    if (exp == 1) return BigInt(base);
    if (base.len == 1 && base.limbs[0] == 1) {
        return BigInt((base.neg && (exp & 1)) ? int64_t(-1) : int64_t(1));
    }
    // Power-of-2 fast path: (+/- 2^k)^exp = (+/-1)^exp * 2^(k*exp)
    if (base.len == 1 && (base.limbs[0] & (base.limbs[0] - 1)) == 0) {
        size_t tz = static_cast<size_t>(gcpp_countr_zero(base.limbs[0]));
        BigInt r = mul_2exp(BigInt(int64_t(1)), tz * static_cast<size_t>(exp));
        if (base.neg && (exp & 1)) r.negate();
        return r;
    }
    // Strip trailing zeros from base (factor out 2^tz)
    size_t tz = scan1(base, 0);
    BigInt b = (tz > 0) ? tdiv_q_2exp(base.abs_view(), tz) : abs(base);
    bool res_neg = base.neg && ((exp & 1) != 0);

    // Left-to-right binary exponentiation with dedicated squaring
    unsigned top_bit = static_cast<unsigned>(63 - gcpp_countl_zero(exp));
    BigInt acc = b;
    for (unsigned i = top_bit; i > 0; --i) {
        acc = sqr(acc);
        if ((exp >> (i - 1)) & 1ULL) {
            acc = mul(acc, b);
        }
    }
    if (tz > 0) acc = mul_2exp(acc, tz * static_cast<size_t>(exp));
    if (res_neg) acc.negate();
    return acc;
}

BigInt BigInt::ui_pow_ui(uint64_t base, uint64_t exp) {
    SmallIntView sb(base);
    return pow_ui(sb, exp);
}

BigInt BigInt::powm(BigIntView base, BigIntView exp, BigIntView m) {
    assert(m.sgn() > 0 && exp.sgn() >= 0);
    if (m.len == 1 && m.limbs[0] == 1) return BigInt();
    if (exp.is_zero()) return BigInt(int64_t(1));
    if (m.is_odd()) {
        return powm_odd(base, exp, m);
    }
    // Even modulus: split m = 2^k * m_odd and combine via CRT
    size_t k = scan1(m, 0);
    BigInt m_odd = tdiv_q_2exp(m, k);
    if (static_cast<size_t>(N(m_odd)) == 1 && gcpp_data(m_odd)[0] == 1) {
        return powm_2exp(base, exp, k);
    }
    BigInt r_odd = powm_odd(base, exp, m_odd);
    BigInt r_even = powm_2exp(base, exp, k);
    // CRT: x = r_odd + m_odd * ((r_even - r_odd) * m_odd^{-1} mod 2^k)
    BigInt m_odd_inv;
    BigInt mod2k = mul_2exp(BigInt(int64_t(1)), k);
    modinv(m_odd, mod2k, m_odd_inv);
    BigInt diff = mod(sub(r_even, r_odd), mod2k);
    BigInt h = mod(mul(diff, m_odd_inv), mod2k);
    return add(r_odd, mul(m_odd, h));
}

// ============================================================================
// Section 11: Bitwise & Shift Operations
// ============================================================================

BigInt BigInt::mul_2exp(BigIntView a, size_t bits) {
    if (a.is_zero() || bits == 0) return BigInt(a);
    size_t word_shift = bits >> 6;
    unsigned bit_shift = static_cast<unsigned>(bits & 63);
    size_t rn = a.len + word_shift + (bit_shift ? 1 : 0);
    BigInt res;
    uint64_t* rp = res.alloc_uninit(rn);
    if (word_shift > 0) std::memset(rp, 0, word_shift * sizeof(uint64_t));
    if (bit_shift == 0) {
        std::memcpy(rp + word_shift, a.limbs, a.len * sizeof(uint64_t));
    } else {
        rp[word_shift + a.len] = mpn_lshift(rp + word_shift, a.limbs, a.len, bit_shift);
    }
    res.set_size_and_normalize(rn, a.neg);
    return res;
}

BigInt BigInt::tdiv_q_2exp(BigIntView a, size_t bits) {
    if (a.is_zero() || bits == 0) return BigInt(a);
    size_t word_shift = bits >> 6;
    if (word_shift >= a.len) return BigInt();
    unsigned bit_shift = static_cast<unsigned>(bits & 63);
    size_t rn = a.len - word_shift;
    BigInt res;
    uint64_t* rp = res.alloc_uninit(rn);
    if (bit_shift == 0) {
        std::memcpy(rp, a.limbs + word_shift, rn * sizeof(uint64_t));
    } else {
        mpn_rshift(rp, a.limbs + word_shift, rn, bit_shift);
    }
    res.set_size_and_normalize(rn, a.neg);
    return res;
}

BigInt BigInt::fdiv_q_2exp(BigIntView a, size_t bits) {
    if (a.is_zero() || bits == 0) return BigInt(a);
    if (!a.neg) return tdiv_q_2exp(a, bits);
    // Negative: floor(a / 2^bits) = trunc(a / 2^bits) - (any shifted-out bit != 0 ? 1 : 0)
    size_t word_shift = bits >> 6;
    unsigned bit_shift = static_cast<unsigned>(bits & 63);
    bool lost_bits = false;
    size_t check_words = std::min(word_shift, a.len);
    for (size_t i = 0; i < check_words; ++i) {
        if (a.limbs[i] != 0) { lost_bits = true; break; }
    }
    if (!lost_bits && word_shift < a.len && bit_shift > 0) {
        if ((a.limbs[word_shift] & ((1ULL << bit_shift) - 1ULL)) != 0) lost_bits = true;
    }
    if (word_shift >= a.len) {
        return BigInt(int64_t(-1));
    }
    BigInt q = tdiv_q_2exp(a, bits);
    if (lost_bits) q = sub_ui(q, 1);
    return q;
}

BigInt BigInt::bit_not(BigIntView a) {
    // ~a = -(a + 1)
    BigInt r = add_ui(a, 1);
    r.negate();
    return r;
}

template <typename Op>
static BigInt bitwise_binary(BigIntView a, BigIntView b, Op op) {
    TwosReader ra(a), rb(b);
    uint64_t res_sign = op(ra.sign_ext(), rb.sign_ext());
    bool res_neg = (res_sign != 0);
    size_t n = std::max(a.len, b.len);
    BigInt res;
    if (!res_neg) {
        uint64_t* rp = res.alloc_uninit(n);
        for (size_t i = 0; i < n; ++i) {
            rp[i] = op(ra.limb(i), rb.limb(i));
        }
        res.set_size_and_normalize(n, false);
    } else {
        // Result in two's complement is negative -> magnitude = (~R) + 1
        uint64_t* rp = res.alloc_uninit(n + 1);
        uint64_t c = 1;
        for (size_t i = 0; i < n; ++i) {
            uint64_t w = ~op(ra.limb(i), rb.limb(i));
            uint64_t s = w + c;
            c = (s < w) ? 1ULL : 0ULL;
            rp[i] = s;
        }
        rp[n] = c;
        res.set_size_and_normalize(n + 1, true);
    }
    return res;
}

BigInt BigInt::bit_and(BigIntView a, BigIntView b) {
    return bitwise_binary(a, b, [](uint64_t x, uint64_t y) { return x & y; });
}

BigInt BigInt::bit_ior(BigIntView a, BigIntView b) {
    return bitwise_binary(a, b, [](uint64_t x, uint64_t y) { return x | y; });
}

BigInt BigInt::bit_xor(BigIntView a, BigIntView b) {
    return bitwise_binary(a, b, [](uint64_t x, uint64_t y) { return x ^ y; });
}

bool BigInt::tstbit(BigIntView a, size_t bit) noexcept {
    size_t w = bit >> 6;
    unsigned b = static_cast<unsigned>(bit & 63);
    if (!a.neg) {
        if (w >= a.len) return false;
        return ((a.limbs[w] >> b) & 1ULL) != 0;
    }
    TwosReader r(a);
    return ((r.limb(w) >> b) & 1ULL) != 0;
}

BigInt BigInt::setbit(BigIntView a, size_t bit) {
    if (tstbit(a, bit)) return BigInt(a);
    BigInt mask = mul_2exp(BigInt(int64_t(1)), bit);
    return bit_ior(a, mask);
}

BigInt BigInt::clrbit(BigIntView a, size_t bit) {
    if (!tstbit(a, bit)) return BigInt(a);
    BigInt mask = bit_not(mul_2exp(BigInt(int64_t(1)), bit));
    return bit_and(a, mask);
}

void BigInt::setbit_inplace(size_t bit) {
    if (!neg_) {
        size_t w = bit >> 6;
        unsigned b = static_cast<unsigned>(bit & 63);
        if (w >= size_) {
            reserve(w + 1);
            std::memset(ptr_ + size_, 0, (w + 1 - size_) * sizeof(uint64_t));
            size_ = static_cast<uint32_t>(w + 1);
        }
        ptr_[w] |= (1ULL << b);
        return;
    }
    *this = setbit(view(), bit);
}

void BigInt::clrbit_inplace(size_t bit) {
    if (!neg_) {
        size_t w = bit >> 6;
        if (w >= size_) return;
        ptr_[w] &= ~(1ULL << (bit & 63));
        normalize();
        return;
    }
    *this = clrbit(view(), bit);
}

size_t BigInt::scan1(BigIntView a, size_t start) noexcept {
    if (a.is_zero()) return ~size_t(0);
    size_t w = start >> 6;
    unsigned b = static_cast<unsigned>(start & 63);
    if (!a.neg) {
        if (w >= a.len) return ~size_t(0);
        uint64_t first = a.limbs[w] & (~0ULL << b);
        if (first != 0) return (w << 6) + static_cast<size_t>(gcpp_countr_zero(first));
        for (size_t i = w + 1; i < a.len; ++i) {
            if (a.limbs[i] != 0) return (i << 6) + static_cast<size_t>(gcpp_countr_zero(a.limbs[i]));
        }
        return ~size_t(0);
    }
    TwosReader r(a);
    for (size_t i = w; ; ++i) {
        uint64_t v = r.limb(i);
        if (i == w) v &= (~0ULL << b);
        if (v != 0) return (i << 6) + static_cast<size_t>(gcpp_countr_zero(v));
        if (i >= a.len) return (i << 6); // infinite leading 1-bits
    }
}

size_t BigInt::logcount(BigIntView a) noexcept {
    if (a.is_zero()) return 0;
    if (!a.neg) {
        size_t total = 0;
        for (size_t i = 0; i < a.len; ++i) total += static_cast<size_t>(gcpp_popcount(a.limbs[i]));
        return total;
    }
    // For a < 0, logcount(a) = popcount(~a) = popcount(|a| - 1)!
    size_t total = 0;
    uint64_t borrow = 1;
    for (size_t i = 0; i < a.len; ++i) {
        uint64_t ai = a.limbs[i];
        uint64_t v = ai - borrow;
        borrow = (ai < borrow) ? 1ULL : 0ULL;
        total += static_cast<size_t>(gcpp_popcount(v));
    }
    return total;
}

size_t BigInt::integer_length(BigIntView a) noexcept {
    if (a.is_zero()) return 0;
    if (!a.neg) return a.bit_length_abs();
    // For a < 0: integer-length(a) = bit_length_abs(~a) = bit_length_abs(|a| - 1)
    // Only differs from bit_length_abs(|a|) when |a| is an exact power of 2!
    size_t bl = a.bit_length_abs();
    bool is_pow2 = ((a.limbs[a.len - 1] & (a.limbs[a.len - 1] - 1ULL)) == 0);
    if (is_pow2) {
        for (size_t i = 0; i + 1 < a.len; ++i) {
            if (a.limbs[i] != 0) { is_pow2 = false; break; }
        }
    }
    return is_pow2 ? (bl - 1) : bl;
}

// ============================================================================
// Section 12: Big-Endian Byte Import / Export & Base Conversion
// ============================================================================

BigInt BigInt::from_be_bytes(const uint8_t* data, size_t len) {
    while (len > 0 && *data == 0) { ++data; --len; }
    if (len == 0) return BigInt();
    size_t nlimbs = (len + 7) >> 3;
    BigInt res;
    uint64_t* rp = res.alloc_uninit(nlimbs);
    std::memset(rp, 0, nlimbs * sizeof(uint64_t));
    for (size_t i = 0; i < len; ++i) {
        size_t pos = len - 1 - i;
        rp[pos >> 3] |= static_cast<uint64_t>(data[i]) << ((pos & 7) * 8);
    }
    res.set_size_and_normalize(nlimbs, false);
    return res;
}

bool BigInt::to_be_bytes(BigIntView a, uint8_t* out, size_t len) noexcept {
    if (a.neg) return false;
    if (a.bit_length_abs() > len * 8) return false;
    std::memset(out, 0, len);
    if (a.is_zero()) return true;
    size_t nbytes = (a.bit_length_abs() + 7) >> 3;
    for (size_t i = 0; i < nbytes; ++i) {
        uint8_t byte = static_cast<uint8_t>((a.limbs[i >> 3] >> ((i & 7) * 8)) & 0xFFU);
        out[len - 1 - i] = byte;
    }
    return true;
}

string BigInt::to_string(BigIntView a, int radix) {
    bool upper = false;
    if (radix < 0) { upper = true; radix = -radix; }
    assert(radix >= 2 && radix <= 36);
    if (a.is_zero()) return "0";
    const char* digs = upper ? "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                             : "0123456789abcdefghijklmnopqrstuvwxyz";

    RadixInfo ri = get_radix_info(radix);

    // Fast O(n) path for power-of-2 radices (2, 4, 8, 16, 32)
    if (ri.bits_per_digit > 0) {
        unsigned bpd = ri.bits_per_digit;
        size_t bits = a.bit_length_abs();
        size_t ndigits = (bits + bpd - 1) / bpd;
        string out;
        out->resize(static_cast<int>(ndigits + (a.neg ? 1 : 0)));
        char* p = str_mut(out);
        if (a.neg) *p++ = '-';

        if (bpd == 4) {
            // Ultra-fast hex extraction
            size_t pos = ndigits;
            for (size_t i = 0; i < a.len; ++i) {
                uint64_t w = a.limbs[i];
                size_t cnt = (i + 1 < a.len) ? 16 : pos;
                for (size_t k = 0; k < cnt; ++k) {
                    p[--pos] = digs[w & 0xFULL];
                    w >>= 4;
                }
            }
            return out;
        }
        uint64_t mask = (1ULL << bpd) - 1ULL;
        for (size_t d = 0; d < ndigits; ++d) {
            size_t bitpos = (ndigits - 1 - d) * bpd;
            size_t wi = bitpos >> 6;
            unsigned bi = static_cast<unsigned>(bitpos & 63);
            uint64_t val = a.limbs[wi] >> bi;
            if (bi + bpd > 64 && wi + 1 < a.len) {
                val |= a.limbs[wi + 1] << (64 - bi);
            }
            p[d] = digs[val & mask];
        }
        return out;
    }

    // Single-limb fast path
    if (a.len == 1) {
        char buf[66];
        int len = 0;
        uint64_t v = a.limbs[0];
        if (radix == 10) {
            while (v >= 100) {
                uint64_t q = v / 100;
                unsigned rem = static_cast<unsigned>(v - q * 100);
                v = q;
                buf[len++] = DEC_PAIRS[2 * rem + 1];
                buf[len++] = DEC_PAIRS[2 * rem];
            }
            if (v >= 10) {
                buf[len++] = DEC_PAIRS[2 * v + 1];
                buf[len++] = DEC_PAIRS[2 * v];
            } else {
                buf[len++] = static_cast<char>('0' + v);
            }
        } else {
            while (v > 0) {
                uint64_t q = v / static_cast<uint64_t>(radix);
                buf[len++] = digs[v - q * static_cast<uint64_t>(radix)];
                v = q;
            }
        }
        string out;
        if (a.neg) out << '-';
        for (int i = len - 1; i >= 0; --i) out << buf[i];
        return out;
    }

    // Small/medium basecase (<= 32 limbs)
    unsigned s_shift = static_cast<unsigned>(gcpp_countl_zero(ri.big_base));
    uint64_t dn = ri.big_base << s_shift;
    uint64_t inv = invert_limb(dn);
    if (a.len <= 32) {
        ScratchMark mark;
        uint64_t* tmp = tl_scratch.alloc(a.len);
        uint64_t* chunks = tl_scratch.alloc(a.len * 2 + 4);
        std::memcpy(tmp, a.limbs, a.len * sizeof(uint64_t));
        size_t n = a.len;
        size_t total_chunks = 0;
        while (n > 0) {
            chunks[total_chunks++] = mpn_divrem_1_preinv(tmp, n, dn, inv, s_shift);
            n = mpn_norm(tmp, n);
        }
        string out;
        if (a.neg) out << '-';
        char buf[64];
        uint64_t top = chunks[total_chunks - 1];
        int len = 0;
        do {
            buf[len++] = digs[top % static_cast<uint64_t>(radix)];
            top /= static_cast<uint64_t>(radix);
        } while (top > 0);
        for (int i = len - 1; i >= 0; --i) out << buf[i];
        for (size_t i = total_chunks - 1; i > 0; --i) {
            format_u64_radix_padded(chunks[i - 1], radix, digs, buf, ri.digits_per_limb);
            str_append(out, buf, ri.digits_per_limb);
        }
        return out;
    }

    // Large numbers: subquadratic Divide-and-Conquer base conversion
    // For radix 10: cache pow10[k] = (10^19)^(2^k) + Barrett reciprocal (t == 2 ALWAYS!)
    static array<DecPow10Entry> dec_pow10;
    array<BigInt> local_pows;
    size_t start_level = 1;
    if (radix == 10) {
        if ((N(dec_pow10) == 0)) {
            DecPow10Entry e;
            e.val = BigInt(ri.big_base); // 10^19
            (dec_pow10 << (std::move(e)));
        }
        // Only build up to largest k such that pow10[k] <= a (never build unused k+1!)
        while (2 * static_cast<size_t>(N(dec_pow10[N(dec_pow10) - 1].val)) <= a.len + 1) {
            DecPow10Entry e;
            e.val = sqr(dec_pow10[N(dec_pow10) - 1].val);
            (dec_pow10 << (std::move(e)));
        }
        while (start_level < static_cast<size_t>(N(dec_pow10)) && static_cast<size_t>(N(dec_pow10[start_level - 1].val)) <= a.len) ++start_level;
    } else {
        (local_pows << BigInt(ri.big_base));
        while (2 * static_cast<size_t>(N(local_pows[N(local_pows) - 1])) <= a.len + 1) {
            (local_pows << (sqr(local_pows[N(local_pows) - 1])));
        }
        while (start_level < static_cast<size_t>(N(local_pows)) && static_cast<size_t>(N(local_pows[start_level - 1])) <= a.len) ++start_level;
    }

    string out;
    
    if (a.neg) out << '-';
    BigInt abs_a(a.abs_view());
    to_string_dc(abs_a, radix == 10 ? &dec_pow10 : nullptr, radix == 10 ? nullptr : &local_pows,
                 start_level, ri.digits_per_limb, radix, digs, dn, inv, s_shift, out, false, 0);
    return out;
}

static inline int parse_digit(char c, int radix) noexcept {
    int d = -1;
    if (c >= '0' && c <= '9') d = c - '0';
    else if (c >= 'a' && c <= 'z') d = c - 'a' + 10;
    else if (c >= 'A' && c <= 'Z') d = c - 'A' + 10;
    return (d >= 0 && d < radix) ? d : -1;
}

bool BigInt::from_string_unsigned(const char* s, size_t sn, int radix, BigInt& out) {
    if ((sn == 0) || radix < 2 || radix > 36) return false;
    // Validate & strip leading zeros
    size_t first_nz = 0;
    while (first_nz < sn && s[first_nz] == '0') ++first_nz;
    if (first_nz == sn) {
        out.set_zero();
        return true;
    }
    s += first_nz; sn -= first_nz;

    RadixInfo ri = get_radix_info(radix);

    // Fast O(n) path for power-of-2 radices (2, 4, 8, 16, 32)
    if (ri.bits_per_digit > 0) {
        unsigned bpd = ri.bits_per_digit;
        size_t total_bits = sn * bpd;
        size_t nlimbs = (total_bits + 63) >> 6;
        BigInt res;
        uint64_t* rp = res.alloc_uninit(nlimbs);
        std::memset(rp, 0, nlimbs * sizeof(uint64_t));

        if (bpd == 4) {
            size_t len = sn;
            for (size_t i = 0; i < len; ++i) {
                int d = parse_digit(s[len - 1 - i], 16);
                if (d < 0) return false;
                rp[i >> 4] |= static_cast<uint64_t>(d) << ((i & 15) * 4);
            }
        } else {
            size_t len = sn;
            for (size_t i = 0; i < len; ++i) {
                int d = parse_digit(s[len - 1 - i], radix);
                if (d < 0) return false;
                size_t bitpos = i * bpd;
                size_t wi = bitpos >> 6;
                unsigned bi = static_cast<unsigned>(bitpos & 63);
                rp[wi] |= static_cast<uint64_t>(d) << bi;
                if (bi + bpd > 64) {
                    rp[wi + 1] |= static_cast<uint64_t>(d) >> (64 - bi);
                }
            }
        }
        res.set_size_and_normalize(nlimbs, false);
        out = std::move(res);
        return true;
    }

    // Fast chunked path for small strings
    unsigned dpl = ri.digits_per_limb;
    if (sn <= dpl) {
        uint64_t v = 0;
        for (size_t si = 0; si < sn; si++) { char c = s[si];
            int d = parse_digit(c, radix);
            if (d < 0) return false;
            v = v * static_cast<uint64_t>(radix) + static_cast<uint64_t>(d);
        }
        out.set_ui(v);
        return true;
    }

    // Parse into chunks of size dpl (aligned to the right so every chunk except top has exact width dpl)
    size_t n = sn;
    size_t num_chunks = (n + dpl - 1) / dpl;
    array<uint64_t> chunks(num_chunks);
    size_t top_len = n - (num_chunks - 1) * dpl;
    size_t pos = 0;
    for (size_t ci = 0; ci < num_chunks; ++ci) {
        size_t w = (ci == 0) ? top_len : static_cast<size_t>(dpl);
        uint64_t v = 0;
        for (size_t k = 0; k < w; ++k) {
            int d = parse_digit(s[pos++], radix);
            if (d < 0) return false;
            v = v * static_cast<uint64_t>(radix) + static_cast<uint64_t>(d);
        }
        chunks[num_chunks - 1 - ci] = v; // little-endian chunk order!
    }

    // Small/medium: simple Horner accumulation in uint64_t chunks
    if (num_chunks <= 32) {
        BigInt acc(chunks[N(chunks) - 1]);
        for (size_t i = num_chunks - 1; i > 0; --i) {
            acc = mul_ui(acc, ri.big_base);
            if (chunks[i - 1] != 0) acc = add_ui(acc, chunks[i - 1]);
        }
        out = std::move(acc);
        return true;
    }

    // Large: subquadratic Divide-and-Conquer bottom-up binary tree!
    array<BigInt> level;
    (void) 0;
    for (uint64_t c : chunks) (level << BigInt(c));

    BigInt base_pow(ri.big_base);
    while (static_cast<size_t>(N(level)) > 1) {
        size_t pairs = static_cast<size_t>(N(level)) >> 1;
        bool has_odd = (static_cast<size_t>(N(level)) & 1) != 0;
        array<BigInt> next;
        (void) 0;
        for (size_t i = 0; i < pairs; ++i) {
            BigInt hi = mul(level[2 * i + 1], base_pow);
            (next << (add(level[2 * i], hi)));
        }
        if (has_odd) (next << (std::move(level[N(level) - 1])));
        level = std::move(next);
        if (static_cast<size_t>(N(level)) > 1) base_pow = sqr(base_pow);
    }
    out = std::move(level[0]);
    return true;
}

// ============================================================================
// Section 13: BigRat Exact Rational Implementation
// ============================================================================

void BigRat::canonicalize() {
    if (num.is_zero()) {
        den.set_ui(1);
        return;
    }
    if (den.is_neg()) {
        num.negate();
        den.abs_inplace();
    }
    if (static_cast<size_t>(N(den)) == 1 && gcpp_data(den)[0] == 1) return;
    BigInt g = BigInt::gcd(num, den);
    if (static_cast<size_t>(N(g)) > 1 || (static_cast<size_t>(N(g)) == 1 && gcpp_data(g)[0] > 1)) {
        num = BigInt::divexact(num, g);
        den = BigInt::divexact(den, g);
    }
}

BigRat BigRat::from_double(double d) {
    if (d == 0.0) return BigRat();
    int exp = 0;
    double frac = std::frexp(d, &exp);
    int64_t mant = static_cast<int64_t>(std::ldexp(frac, 53));
    int shift = exp - 53;
    // Strip trailing zeros from mantissa to keep denominator minimal immediately!
    uint64_t um = (mant >= 0) ? static_cast<uint64_t>(mant) : (0ULL - static_cast<uint64_t>(mant));
    int tz = gcpp_countr_zero(um);
    mant >>= tz;
    shift += tz;
    if (shift >= 0) {
        return BigRat(BigInt::mul_2exp(BigInt(mant), static_cast<size_t>(shift)), BigInt(int64_t(1)));
    } else {
        return BigRat(BigInt(mant), BigInt::mul_2exp(BigInt(int64_t(1)), static_cast<size_t>(-shift)));
    }
}

BigRat BigRat::add_views(BigIntView an, BigIntView ad, BigIntView bn, BigIntView bd) {
    if (an.is_zero()) return BigRat(BigInt(bn), BigInt(bd));
    if (bn.is_zero()) return BigRat(BigInt(an), BigInt(ad));
    if (BigInt::cmp(ad, bd) == 0) {
        BigRat r(BigInt::add(an, bn), BigInt(ad));
        r.canonicalize();
        return r;
    }
    BigInt g = BigInt::gcd(ad, bd);
    if (static_cast<size_t>(N(g)) == 1 && gcpp_data(g)[0] == 1) {
        BigInt n = BigInt::add(BigInt::mul(an, bd), BigInt::mul(bn, ad));
        BigInt d = BigInt::mul(ad, bd);
        return BigRat(std::move(n), std::move(d)); // already coprime!
    }
    BigInt ad_q = BigInt::divexact(ad, g);
    BigInt bd_q = BigInt::divexact(bd, g);
    BigInt t = BigInt::add(BigInt::mul(an, bd_q), BigInt::mul(bn, ad_q));
    BigInt g2 = BigInt::gcd(t, g);
    if (static_cast<size_t>(N(g2)) > 1 || (static_cast<size_t>(N(g2)) == 1 && gcpp_data(g2)[0] > 1)) {
        t = BigInt::divexact(t, g2);
        BigInt ad_r = BigInt::divexact(ad, g2);
        return BigRat(std::move(t), BigInt::mul(ad_r, bd_q));
    }
    return BigRat(std::move(t), BigInt::mul(ad, bd_q));
}

BigRat BigRat::add(const BigRat& a, const BigRat& b) {
    return add_views(a.num, a.den, b.num, b.den);
}

BigRat BigRat::sub_views(BigIntView an, BigIntView ad, BigIntView bn, BigIntView bd) {
    return add_views(an, ad, bn.neg_view(), bd);
}

BigRat BigRat::sub(const BigRat& a, const BigRat& b) {
    return add_views(a.num, a.den, b.num.view().neg_view(), b.den);
}

BigRat BigRat::mul_views(BigIntView an, BigIntView ad, BigIntView bn, BigIntView bd) {
    if (an.is_zero() || bn.is_zero()) return BigRat();
    BigInt g1 = BigInt::gcd(an, bd);
    BigInt g2 = BigInt::gcd(bn, ad);
    bool r1 = (static_cast<size_t>(N(g1)) > 1 || g1.low_u64() > 1);
    bool r2 = (static_cast<size_t>(N(g2)) > 1 || g2.low_u64() > 1);
    BigInt an_o, bd_o, bn_o, ad_o;
    BigIntView an_v = r1 ? (an_o = BigInt::divexact(an, g1)).view() : an;
    BigIntView bd_v = r1 ? (bd_o = BigInt::divexact(bd, g1)).view() : bd;
    BigIntView bn_v = r2 ? (bn_o = BigInt::divexact(bn, g2)).view() : bn;
    BigIntView ad_v = r2 ? (ad_o = BigInt::divexact(ad, g2)).view() : ad;
    return BigRat(BigInt::mul(an_v, bn_v), BigInt::mul(ad_v, bd_v));
}

BigRat BigRat::mul(const BigRat& a, const BigRat& b) {
    return mul_views(a.num, a.den, b.num, b.den);
}

BigRat BigRat::div_views(BigIntView an, BigIntView ad, BigIntView bn, BigIntView bd) {
    if (bn.neg) {
        return mul_views(an, ad, bd.neg_view(), bn.abs_view());
    }
    return mul_views(an, ad, bd, bn);
}

BigRat BigRat::div(const BigRat& a, const BigRat& b) {
    return div_views(a.num, a.den, b.num, b.den);
}

int BigRat::cmp_views(BigIntView an, BigIntView ad, BigIntView bn, BigIntView bd) {
    int sa = an.sgn(), sb = bn.sgn();
    if (sa != sb) return (sa < sb) ? -1 : 1;
    if (sa == 0) return 0;
    if (BigInt::cmp(ad, bd) == 0) return BigInt::cmp(an, bn);
    return BigInt::cmp(BigInt::mul(an, bd), BigInt::mul(bn, ad));
}

int BigRat::cmp(const BigRat& a, const BigRat& b) {
    return cmp_views(a.num, a.den, b.num, b.den);
}

double BigRat::to_double_exact(BigIntView num, BigIntView den) {
    int ns = num.sgn();
    if (ns == 0) return 0.0;
    bool neg = (ns < 0) != (den.sgn() < 0);
    BigIntView n = num.abs_view();
    BigIntView d = den.abs_view();
    size_t nb = n.bit_length_abs();
    size_t db = d.bit_length_abs();
    double res = 0.0;
    if (nb <= 53 && db <= 53) {
        res = static_cast<double>(n.limbs[0]) / static_cast<double>(d.limbs[0]);
    } else {
        int64_t exp = static_cast<int64_t>(nb) - static_cast<int64_t>(db);
        if (exp > 1025) {
            res = std::numeric_limits<double>::infinity();
        } else if (exp < -1076) {
            res = 0.0;
        } else {
            int64_t shift = 53 - exp;
            if (exp < -1022) shift = 1075;
            BigInt sn, sd, q, r;
            if (shift >= 0) {
                sn = BigInt::mul_2exp(n, static_cast<size_t>(shift));
                sd = BigInt(d);
            } else {
                sn = BigInt(n);
                sd = BigInt::mul_2exp(d, static_cast<size_t>(-shift));
            }
            BigInt::tdiv_qr(sn, sd, q, r);
            if (q.bit_length_abs() > 53 && exp >= -1022) {
                bool lsb = q.is_odd();
                q = BigInt::tdiv_q_2exp(q, 1);
                if (lsb) r = BigInt::add(r, sd);
                sd = BigInt::mul_2exp(sd, 1);
                --shift;
            }
            r = BigInt::mul_2exp(r, 1);
            int cmp = BigInt::cmp(r, sd);
            if (cmp > 0 || (cmp == 0 && q.is_odd())) {
                q = BigInt::add_ui(q, 1);
            }
            res = std::ldexp(q.to_double_exact(), static_cast<int>(-shift));
        }
    }
    return neg ? -res : res;
}

} // namespace guile
