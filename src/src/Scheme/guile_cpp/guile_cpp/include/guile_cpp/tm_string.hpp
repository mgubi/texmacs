#pragma once

// TeXmacs: the strings of guile_cpp are TeXmacs strings (string.hpp). A
// copy of a TeXmacs string shares its characters (there is no copy on
// write): a string which is changed (<<, resize, [i]=) must be owned by the
// code which changes it, a fresh string or a copy (). These helpers give
// the operations of std::string which TeXmacs writes otherwise.

#include <cstring>
#include <cstdlib>
#include <cerrno>
#include <cstddef>
#include <initializer_list>
#include "string.hpp"
#include "array.hpp"

namespace guile {

// the characters of s ("" when s is empty), not 0-terminated
inline const char* str_data(string s) { return N(s) == 0 ? "" : &s[0]; }
inline char* str_mut(string& s) { return &s[0]; }

inline void str_append(string& s, const char* p, size_t n) {
    if (n == 0) return;
    int k = N(s);
    s->resize(k + static_cast<int>(n));
    std::memcpy(&s[k], p, n);
}
inline void str_append(string& s, const char* p) { str_append(s, p, std::strlen(p)); }
inline void str_append(string& s, string t) { s << t; }
inline void str_append_n(string& s, size_t count, char c) {
    if (count == 0) return;
    int k = N(s);
    s->resize(k + static_cast<int>(count));
    std::memset(&s[k], c, count);
}

// substring of n characters from pos (to the end by default), a copy
inline string str_sub(string s, size_t pos, size_t n = static_cast<size_t>(-1)) {
    size_t len = static_cast<size_t>(N(s));
    if (pos >= len) return string();
    if (n > len - pos) n = len - pos;
    return s(static_cast<int>(pos), static_cast<int>(pos + n));
}

// the position of c or of t from pos, or -1
inline int str_find(string s, char c, int pos = 0) {
    for (int i = pos < 0 ? 0 : pos; i < N(s); i++) if (s[i] == c) return i;
    return -1;
}
inline int str_find(string s, const char* t, int pos = 0) {
    int m = static_cast<int>(std::strlen(t)), n = N(s);
    for (int i = pos < 0 ? 0 : pos; i + m <= n; i++)
        if (std::memcmp(&s[i], t, m) == 0) return i;
    return -1;
}
inline int str_rfind(string s, char c, int pos = -1) {
    if (pos < 0 || pos >= N(s)) pos = N(s) - 1;
    for (int i = pos; i >= 0; i--) if (s[i] == c) return i;
    return -1;
}

inline bool str_starts_with(string s, const char* t) {
    int m = static_cast<int>(std::strlen(t));
    return N(s) >= m && std::memcmp(str_data(s), t, m) == 0;
}

// std::stoll and std::stoi: an error of Scheme (gcpp_number_error, in
// numbers.cpp) if s does not start with a number or if it is out of range
[[noreturn]] void gcpp_number_error(const char* subr, string s);
inline long long gcpp_stoll(string s, int base = 10) {
    c_string cs(s);
    char* endp = nullptr;
    errno = 0;
    long long r = std::strtoll(cs, &endp, base);
    if (endp == static_cast<char*>(cs)) gcpp_number_error("stoll", s);
    if (errno == ERANGE) gcpp_number_error("stoll", s);
    return r;
}
inline int gcpp_stoi(string s, int base = 10) {
    long long r = gcpp_stoll(s, base);
    if (r < -2147483647LL - 1 || r > 2147483647LL) gcpp_number_error("stoi", s);
    return static_cast<int>(r);
}
inline double gcpp_stod(string s) {
    c_string cs(s);
    char* endp = nullptr;
    double r = std::strtod(cs, &endp);
    if (endp == static_cast<char*>(cs)) gcpp_number_error("stod", s);
    return r;
}
// the first position from pos of a character of set, or -1
inline int str_find_first_of(string s, const char* set, int pos = 0) {
    for (int i = pos < 0 ? 0 : pos; i < N(s); i++) if (std::strchr(set, s[i]) && s[i] != 0) return i;
    return -1;
}

// on a view (the characters p and their number n): a copy of the
// substring, the position of c or t from pos (or -1)
inline string view_sub(const char* p, size_t n, size_t pos, size_t m = static_cast<size_t>(-1)) {
    if (pos >= n) return string();
    if (m > n - pos) m = n - pos;
    return string(p + pos, static_cast<int>(m));
}
inline int view_find(const char* p, size_t n, char c, int pos = 0) {
    for (int i = pos < 0 ? 0 : pos; i < static_cast<int>(n); i++) if (p[i] == c) return i;
    return -1;
}
inline int view_find(const char* p, size_t n, const char* t, int pos = 0) {
    int m = static_cast<int>(std::strlen(t));
    for (int i = pos < 0 ? 0 : pos; i + m <= static_cast<int>(n); i++)
        if (std::memcmp(p + i, t, m) == 0) return i;
    return -1;
}

// strcmp-like comparison of two strings of bytes
inline int str_compare(const char* a, size_t na, const char* b, size_t nb) {
    int c = std::memcmp(a, b, na < nb ? na : nb);
    if (c != 0) return c;
    return na < nb ? -1 : (na > nb ? 1 : 0);
}

// the operations of std::string and std::vector which have no TeXmacs
// form: the data, clearing (a new empty value: the old one may be shared),
// resizing (the new characters are 0, the new elements T ())
inline char* gcpp_data(string& s) { return N(s) == 0 ? nullptr : &s[0]; }
// (by reference, and without A or N, which take the array by value: the
// count of references of the array would change at each call)
template <typename T> inline T* gcpp_data(array<T>& a) { return &a[0]; }
template <typename T> inline T* gcpp_data(const array<T>& a) { return &const_cast<array<T>&>(a)[0]; }
inline void gcpp_clear(string& s) { s = string(); }
template <typename T> inline void gcpp_clear(array<T>& a) { a = array<T>(); }
inline void gcpp_resize(string& s, size_t n, char c = 0) {
    int k = N(s);
    s->resize(static_cast<int>(n));
    for (int i = k; i < N(s); i++) s[i] = c;
}
template <typename T> inline void gcpp_resize(array<T>& a, size_t n) {
    int k = N(a);
    a->resize(static_cast<int>(n));
    for (int i = k; i < N(a); i++) a[i] = T();
}
template <typename T, typename U> inline void gcpp_resize(array<T>& a, size_t n, U v) {
    int k = N(a);
    a->resize(static_cast<int>(n));
    for (int i = k; i < N(a); i++) a[i] = static_cast<T>(v);
}
template <typename T, typename U> inline void gcpp_assign(array<T>& a, size_t n, U v) {
    a = array<T>(static_cast<int>(n));
    for (int i = 0; i < N(a); i++) a[i] = static_cast<T>(v);
}
template <typename T> inline array<T> array_fill(size_t n, T v) {
    array<T> a(static_cast<int>(n));
    for (int i = 0; i < N(a); i++) a[i] = v;
    return a;
}
// append the n elements of p to a (owned by the caller)
template <typename T> inline void array_append(array<T>& a, const T* p, size_t n) {
    int k = N(a);
    a->resize(k + static_cast<int>(n));
    for (int i = 0; i < static_cast<int>(n); i++) a[k + i] = p[i];
}
// a new array: a with its elements [pos, pos + nrem) replaced by those of ins
template <typename T> inline array<T> array_splice(array<T> a, size_t pos, size_t nrem, array<T> ins) {
    int n = N(a), m = N(ins), p = static_cast<int>(pos), r = static_cast<int>(nrem);
    array<T> b(n - r + m);
    for (int i = 0; i < p; i++) b[i] = a[i];
    for (int i = 0; i < m; i++) b[p + i] = ins[i];
    for (int i = p + r; i < n; i++) b[i - r + m] = a[i];
    return b;
}
// a new array: a without the elements equal to x
template <typename T> inline array<T> array_remove(array<T> a, T x) {
    array<T> b;
    for (int i = 0; i < N(a); i++) if (!(a[i] == x)) b << a[i];
    return b;
}
template <typename T> inline array<T> array_list(std::initializer_list<T> l) {
    array<T> a;
    for (const T& x : l) a << x;
    return a;
}
template <typename T> inline array<T> array_from(const T* p, size_t n) {
    array<T> a(static_cast<int>(n));
    for (int i = 0; i < N(a); i++) a[i] = p[i];
    return a;
}

} // namespace guile

// range-for over TeXmacs strings and arrays (found by ADL)
inline char* begin(string& s) { return N(s) == 0 ? nullptr : &s[0]; }
inline char* end(string& s) { return N(s) == 0 ? nullptr : &s[0] + N(s); }
template <typename T> inline T* begin(array<T> a) { return A(a); }
template <typename T> inline T* end(array<T> a) { return A(a) + N(a); }
