#pragma once

#include <cstdint>
#include "guile_cpp/compat.hpp"
#include <cstddef>
#include <cstring>
#include <string>
#include <string_view>
#include <utility>
#include <algorithm>
#include <cmath>
#include <limits>
#if defined(_MSC_VER)
#include <intrin.h>
#endif

namespace guile {

namespace detail {
inline unsigned clz64(uint64_t x) noexcept {
    if (x == 0) return 64;
#if defined(__clang__) || defined(__GNUC__)
    return static_cast<unsigned>(__builtin_clzll(x));
#elif defined(_MSC_VER) && defined(_M_X64)
    unsigned long idx = 0;
    _BitScanReverse64(&idx, x);
    return static_cast<unsigned>(63 - idx);
#else
    unsigned n = 0;
    if ((x >> 32) == 0) { n += 32; x <<= 32; }
    if ((x >> 48) == 0) { n += 16; x <<= 16; }
    if ((x >> 56) == 0) { n += 8;  x <<= 8; }
    if ((x >> 60) == 0) { n += 4;  x <<= 4; }
    if ((x >> 62) == 0) { n += 2;  x <<= 2; }
    if ((x >> 63) == 0) { n += 1; }
    return n;
#endif
}
} // namespace detail

// Non-owning read-only view of a canonical sign-magnitude arbitrary-precision integer.
// Invariants:
//   - Zero has len == 0 and neg == false.
//   - Non-zero has len >= 1 and limbs[len - 1] != 0.
struct BigIntView {
    const uint64_t* limbs = nullptr;
    size_t len = 0;
    bool neg = false;

    constexpr bool is_zero() const noexcept { return len == 0; }
    constexpr int sgn() const noexcept { return len == 0 ? 0 : (neg ? -1 : 1); }
    constexpr bool is_odd() const noexcept { return len > 0 && (limbs[0] & 1ULL) != 0; }
    constexpr bool is_even() const noexcept { return !is_odd(); }

    size_t bit_length_abs() const noexcept {
        if (len == 0) return 0;
        return (len - 1) * 64 + static_cast<size_t>(64 - detail::clz64(limbs[len - 1]));
    }

    uint64_t low_u64() const noexcept {
        return len > 0 ? limbs[0] : 0ULL;
    }

    bool fits_i64() const noexcept {
        if (len == 0) return true;
        if (len > 1) return false;
        if (!neg) return limbs[0] <= static_cast<uint64_t>(std::numeric_limits<int64_t>::max());
        return limbs[0] <= (static_cast<uint64_t>(std::numeric_limits<int64_t>::max()) + 1ULL);
    }

    int64_t to_i64() const noexcept {
        if (len == 0) return 0;
        if (!neg) return static_cast<int64_t>(limbs[0]);
        return static_cast<int64_t>(0ULL - limbs[0]);
    }

    bool fits_u64() const noexcept {
        return !neg && len <= 1;
    }

    uint64_t to_u64() const noexcept {
        return len > 0 ? limbs[0] : 0ULL;
    }

    BigIntView abs_view() const noexcept {
        return {limbs, len, false};
    }

    BigIntView neg_view() const noexcept {
        return {limbs, len, len > 0 ? !neg : false};
    }
};

// Stack-backed helper for wrapping an int64_t or uint64_t as a BigIntView with zero allocation.
struct SmallIntView {
    uint64_t buf = 0;
    BigIntView view;

    explicit SmallIntView(int64_t v) noexcept {
        if (v == 0) {
            view = {&buf, 0, false};
        } else if (v > 0) {
            buf = static_cast<uint64_t>(v);
            view = {&buf, 1, false};
        } else {
            buf = 0ULL - static_cast<uint64_t>(v);
            view = {&buf, 1, true};
        }
    }

    explicit SmallIntView(uint64_t v) noexcept {
        if (v == 0) {
            view = {&buf, 0, false};
        } else {
            buf = v;
            view = {&buf, 1, false};
        }
    }

    operator BigIntView() const noexcept { return view; }
};

// Arbitrary-precision signed integer with Small Buffer Optimization (up to 256 bits inline).
// Pure portable C++20, zero external dependencies.
class BigInt {
public:
    static constexpr uint32_t INLINE_CAP = 4; // 4 x 64-bit = 256 bits inline

private:
    uint64_t* ptr_ = inline_;
    uint32_t size_ = 0;
    uint32_t cap_ = INLINE_CAP;
    bool neg_ = false;
    uint64_t inline_[INLINE_CAP];

    static uint64_t* pool_alloc(size_t n, uint32_t& out_cap);
    static void pool_free(uint64_t* p, uint32_t cap) noexcept;

    bool is_heap() const noexcept { return cap_ > INLINE_CAP; }

    void free_heap() noexcept {
        if (is_heap()) {
            pool_free(ptr_, cap_);
            ptr_ = inline_;
            cap_ = INLINE_CAP;
        }
    }

public:
    BigInt() noexcept = default;

    explicit BigInt(int64_t v) noexcept { set_si(v); }
    explicit BigInt(uint64_t v) noexcept { set_ui(v); }

    BigInt(const BigInt& o) {
        size_ = o.size_;
        neg_ = o.neg_;
        if (size_ > INLINE_CAP) {
            ptr_ = pool_alloc(size_, cap_);
        } else {
            ptr_ = inline_;
            cap_ = INLINE_CAP;
        }
        if (size_ > 0) std::memcpy(ptr_, o.ptr_, size_ * sizeof(uint64_t));
    }

    BigInt(BigInt&& o) noexcept {
        size_ = o.size_;
        neg_ = o.neg_;
        if (o.is_heap()) {
            ptr_ = o.ptr_;
            cap_ = o.cap_;
            o.ptr_ = o.inline_;
            o.size_ = 0;
            o.cap_ = INLINE_CAP;
            o.neg_ = false;
        } else {
            ptr_ = inline_;
            cap_ = INLINE_CAP;
            for (uint32_t i = 0; i < size_; ++i) inline_[i] = o.inline_[i];
            o.size_ = 0;
            o.neg_ = false;
        }
    }

    explicit BigInt(BigIntView v) {
        size_ = static_cast<uint32_t>(v.len);
        neg_ = v.neg && (v.len > 0);
        if (size_ > INLINE_CAP) {
            ptr_ = pool_alloc(size_, cap_);
        } else {
            ptr_ = inline_;
            cap_ = INLINE_CAP;
        }
        if (size_ > 0) std::memcpy(ptr_, v.limbs, size_ * sizeof(uint64_t));
    }

    ~BigInt() noexcept {
        if (cap_ > INLINE_CAP) pool_free(ptr_, cap_);
    }

    BigInt& operator=(const BigInt& o) {
        if (this == &o) return *this;
        if (o.size_ > cap_) {
            if (is_heap()) pool_free(ptr_, cap_);
            ptr_ = pool_alloc(o.size_, cap_);
        }
        size_ = o.size_;
        neg_ = o.neg_;
        if (size_ > 0) std::memcpy(ptr_, o.ptr_, size_ * sizeof(uint64_t));
        return *this;
    }

    BigInt& operator=(BigInt&& o) noexcept {
        if (this == &o) return *this;
        free_heap();
        size_ = o.size_;
        neg_ = o.neg_;
        if (o.is_heap()) {
            ptr_ = o.ptr_;
            cap_ = o.cap_;
            o.ptr_ = o.inline_;
            o.size_ = 0;
            o.cap_ = INLINE_CAP;
            o.neg_ = false;
        } else {
            ptr_ = inline_;
            cap_ = INLINE_CAP;
            for (uint32_t i = 0; i < size_; ++i) inline_[i] = o.inline_[i];
            o.size_ = 0;
            o.neg_ = false;
        }
        return *this;
    }

    // Accessors
    uint64_t* data() noexcept { return ptr_; }
    const uint64_t* data() const noexcept { return ptr_; }
    size_t size() const noexcept { return size_; }
    size_t capacity() const noexcept { return cap_; }
    bool is_neg() const noexcept { return neg_; }
    bool is_zero() const noexcept { return size_ == 0; }
    int sgn() const noexcept { return size_ == 0 ? 0 : (neg_ ? -1 : 1); }
    bool is_odd() const noexcept { return size_ > 0 && (ptr_[0] & 1ULL) != 0; }
    bool is_even() const noexcept { return !is_odd(); }

    BigIntView view() const noexcept { return {ptr_, size_, neg_}; }
    operator BigIntView() const noexcept { return view(); }

    void reserve(size_t n) {
        if (n <= cap_) return;
        uint32_t new_cap = 0;
        uint64_t* np = pool_alloc(std::max(n, static_cast<size_t>(cap_) * 2), new_cap);
        if (size_ > 0) std::memcpy(np, ptr_, size_ * sizeof(uint64_t));
        if (is_heap()) pool_free(ptr_, cap_);
        ptr_ = np;
        cap_ = new_cap;
    }

    // Allocate exact uninitialized space (discarding old contents)
    uint64_t* alloc_uninit(size_t n) {
        if (n > cap_) {
            if (is_heap()) pool_free(ptr_, cap_);
            ptr_ = pool_alloc(n, cap_);
        }
        size_ = static_cast<uint32_t>(n);
        return ptr_;
    }

    void set_size_and_normalize(size_t n, bool neg) noexcept {
        while (n > 0 && ptr_[n - 1] == 0) --n;
        size_ = static_cast<uint32_t>(n);
        neg_ = (n > 0) ? neg : false;
    }

    void normalize() noexcept {
        while (size_ > 0 && ptr_[size_ - 1] == 0) --size_;
        if (size_ == 0) neg_ = false;
    }

    void set_zero() noexcept {
        size_ = 0;
        neg_ = false;
    }

    void set_si(int64_t v) noexcept {
        if (v == 0) {
            size_ = 0;
            neg_ = false;
        } else if (v > 0) {
            ptr_[0] = static_cast<uint64_t>(v);
            size_ = 1;
            neg_ = false;
        } else {
            ptr_[0] = 0ULL - static_cast<uint64_t>(v);
            size_ = 1;
            neg_ = true;
        }
    }

    void set_ui(uint64_t v) noexcept {
        if (v == 0) {
            size_ = 0;
            neg_ = false;
        } else {
            ptr_[0] = v;
            size_ = 1;
            neg_ = false;
        }
    }

    void negate() noexcept {
        if (size_ > 0) neg_ = !neg_;
    }

    void abs_inplace() noexcept {
        neg_ = false;
    }

    bool fits_i64() const noexcept { return view().fits_i64(); }
    int64_t to_i64() const noexcept { return view().to_i64(); }
    bool fits_u64() const noexcept { return view().fits_u64(); }
    uint64_t to_u64() const noexcept { return view().to_u64(); }
    uint64_t low_u64() const noexcept { return view().low_u64(); }
    size_t bit_length_abs() const noexcept { return view().bit_length_abs(); }

    // Exact IEEE-754 round-to-nearest-even conversion to double
    static double to_double_exact(BigIntView a) noexcept;
    double to_double_exact() const noexcept { return to_double_exact(view()); }

    // Comparisons
    static int cmp_abs(BigIntView a, BigIntView b) noexcept;
    static int cmp(BigIntView a, BigIntView b) noexcept;
    static int cmp_ui(BigIntView a, uint64_t b) noexcept;
    static int cmp_si(BigIntView a, int64_t b) noexcept;

    // Core arithmetic
    static BigInt add(BigIntView a, BigIntView b);
    static BigInt sub(BigIntView a, BigIntView b);
    static BigInt neg(BigIntView a);
    static BigInt abs(BigIntView a);
    static BigInt add_ui(BigIntView a, uint64_t b);
    static BigInt sub_ui(BigIntView a, uint64_t b);
    static BigInt mul_ui(BigIntView a, uint64_t b);
    static BigInt mul_si(BigIntView a, int64_t b);
    static BigInt mul(BigIntView a, BigIntView b);
    static BigInt sqr(BigIntView a);

    // Division & modulo
    // Truncated (towards zero): a = q * b + r, sign(r) == sign(a)
    static void tdiv_qr(BigIntView a, BigIntView b, BigInt& q, BigInt& r);
    static BigInt tdiv_q(BigIntView a, BigIntView b);
    static BigInt tdiv_r(BigIntView a, BigIntView b);
    // Floored (towards -inf): sign(r) == sign(b)
    static void fdiv_qr(BigIntView a, BigIntView b, BigInt& q, BigInt& r);
    static BigInt fdiv_q(BigIntView a, BigIntView b);
    static BigInt fdiv_r(BigIntView a, BigIntView b);
    // Ceiling (towards +inf)
    static BigInt cdiv_q(BigIntView a, BigIntView b);
    // Exact division (assumes b divides a exactly)
    static BigInt divexact(BigIntView a, BigIntView b);
    // Non-negative modulo (result in [0, |b| - 1], matching mpz_mod)
    static BigInt mod(BigIntView a, BigIntView b);
    // Fast single-limb remainder (truncated): returns a % d (signed as a)
    static int64_t tdiv_r_si(BigIntView a, int64_t d) noexcept;
    static uint64_t mod_ui(BigIntView a, uint64_t d) noexcept;

    // GCD / LCM / Modular inverse
    static BigInt gcd(BigIntView a, BigIntView b);
    static BigInt lcm(BigIntView a, BigIntView b);
    // Returns true if inverse exists (gcd(|a|, m) == 1), setting out in [0, m-1] (m > 0)
    static bool modinv(BigIntView a, BigIntView m, BigInt& out);

    // Exponentiation & modular exponentiation
    static BigInt pow_ui(BigIntView base, uint64_t exp);
    static BigInt ui_pow_ui(uint64_t base, uint64_t exp);
    // Computes base^exp mod m where exp >= 0 and m > 0, result in [0, m-1]
    static BigInt powm(BigIntView base, BigIntView exp, BigIntView m);

    // Bitwise & shift operations (two's complement semantics matching GMP / R5RS / SRFI-60)
    static BigInt mul_2exp(BigIntView a, size_t bits);
    static BigInt tdiv_q_2exp(BigIntView a, size_t bits);
    static BigInt fdiv_q_2exp(BigIntView a, size_t bits);
    static BigInt bit_and(BigIntView a, BigIntView b);
    static BigInt bit_ior(BigIntView a, BigIntView b);
    static BigInt bit_xor(BigIntView a, BigIntView b);
    static BigInt bit_not(BigIntView a); // -(a + 1)
    static bool tstbit(BigIntView a, size_t bit) noexcept;
    static BigInt setbit(BigIntView a, size_t bit);
    static BigInt clrbit(BigIntView a, size_t bit);
    void setbit_inplace(size_t bit);
    void clrbit_inplace(size_t bit);
    // Index of lowest 1-bit >= start (for a != 0); returns ~size_t(0) if none
    static size_t scan1(BigIntView a, size_t start = 0) noexcept;
    // Population count: for a >= 0 counts 1-bits; for a < 0 counts 0-bits (i.e. popcount(~a))
    static size_t logcount(BigIntView a) noexcept;
    // Integer length: bit_length_abs(a >= 0 ? a : ~a)
    static size_t integer_length(BigIntView a) noexcept;

    // Big-endian byte import/export (unsigned magnitude)
    static BigInt from_be_bytes(const uint8_t* data, size_t len);
    // Writes big-endian unsigned bytes into out[0..len-1], right-aligned (zero-padded on left).
    // Returns false if negative or does not fit in len bytes.
    static bool to_be_bytes(BigIntView a, uint8_t* out, size_t len) noexcept;

    // Base conversion (radix 2..36; negative radix => uppercase digits, matching GMP)
    static std::string to_string(BigIntView a, int radix = 10);
    // Parses unsigned digits in radix 2..36 (returns false on invalid char or empty string)
    static bool from_string_unsigned(std::string_view s, int radix, BigInt& out);
};

// Exact arbitrary-precision rational (replaces mpq_t).
struct BigRat {
    BigInt num;
    BigInt den;

    BigRat() noexcept : num(int64_t(0)), den(int64_t(1)) {}
    BigRat(BigInt n, BigInt d) : num(std::move(n)), den(std::move(d)) {}

    int sgn() const noexcept { return num.sgn(); }
    void canonicalize();

    static BigRat from_double(double d);
    static BigRat add(const BigRat& a, const BigRat& b);
    static BigRat add_views(BigIntView an, BigIntView ad, BigIntView bn, BigIntView bd);
    static BigRat sub(const BigRat& a, const BigRat& b);
    static BigRat sub_views(BigIntView an, BigIntView ad, BigIntView bn, BigIntView bd);
    static BigRat mul(const BigRat& a, const BigRat& b);
    static BigRat mul_views(BigIntView an, BigIntView ad, BigIntView bn, BigIntView bd);
    static BigRat div(const BigRat& a, const BigRat& b);
    static BigRat div_views(BigIntView an, BigIntView ad, BigIntView bn, BigIntView bd);
    static int cmp(const BigRat& a, const BigRat& b);
    static int cmp_views(BigIntView an, BigIntView ad, BigIntView bn, BigIntView bd);
    static double to_double_exact(BigIntView num, BigIntView den);
};

} // namespace guile
