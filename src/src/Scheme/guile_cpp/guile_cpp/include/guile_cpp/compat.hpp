#pragma once

// TeXmacs: C++17 replacements for the C++20 features guile_cpp used: the
// bit functions of <bit> (countr_zero, countl_zero, popcount, bit_ceil),
// with the same results for 0, and GCPP_LIKELY / GCPP_UNLIKELY in place of
// the attributes [[likely]] / [[unlikely]].

#include <cstdint>
#include <cstddef>
#include <type_traits>

#if defined(__GNUC__) || defined(__clang__)
#define GCPP_LIKELY(x)   __builtin_expect(!!(x), 1)
#define GCPP_UNLIKELY(x) __builtin_expect(!!(x), 0)
#else
#define GCPP_LIKELY(x)   (x)
#define GCPP_UNLIKELY(x) (x)
#endif

namespace guile {

template <typename T>
inline int gcpp_countr_zero(T x) noexcept {
    static_assert(std::is_unsigned<T>::value, "unsigned type expected");
    if (x == 0) return int(sizeof(T) * 8);
    if (sizeof(T) <= sizeof(unsigned int)) return __builtin_ctz((unsigned int) x);
    if (sizeof(T) <= sizeof(unsigned long)) return __builtin_ctzl((unsigned long) x);
    return __builtin_ctzll((unsigned long long) x);
}

template <typename T>
inline int gcpp_countl_zero(T x) noexcept {
    static_assert(std::is_unsigned<T>::value, "unsigned type expected");
    constexpr int bits = int(sizeof(T) * 8);
    if (x == 0) return bits;
    if (sizeof(T) <= sizeof(unsigned int))
        return __builtin_clz((unsigned int) x) - (int(sizeof(unsigned int) * 8) - bits);
    if (sizeof(T) <= sizeof(unsigned long))
        return __builtin_clzl((unsigned long) x) - (int(sizeof(unsigned long) * 8) - bits);
    return __builtin_clzll((unsigned long long) x) - (int(sizeof(unsigned long long) * 8) - bits);
}

template <typename T>
inline int gcpp_popcount(T x) noexcept {
    static_assert(std::is_unsigned<T>::value, "unsigned type expected");
    if (sizeof(T) <= sizeof(unsigned int)) return __builtin_popcount((unsigned int) x);
    if (sizeof(T) <= sizeof(unsigned long)) return __builtin_popcountl((unsigned long) x);
    return __builtin_popcountll((unsigned long long) x);
}

template <typename T>
inline T gcpp_bit_ceil(T x) noexcept {
    static_assert(std::is_unsigned<T>::value, "unsigned type expected");
    if (x <= 1) return T(1);
    return T(T(1) << (int(sizeof(T) * 8) - gcpp_countl_zero(T(x - 1))));
}

} // namespace guile
