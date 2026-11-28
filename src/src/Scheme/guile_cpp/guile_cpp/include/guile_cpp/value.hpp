#pragma once

// TeXmacs: the part of scm.hpp which the interface of TeXmacs
// (../../guile_cpp_tm.hpp, included by most of TeXmacs) needs: the tagged
// values, the header of the heap objects and the pairs; it includes no other
// header of guile_cpp and nothing of the standard library but <cstdint>.

#include <cstdint>
#include <cstddef>

namespace guile {

struct Obj;

// ============================================================================
// 64-bit Tagged Value Representation (SCM)
// ============================================================================
// Low bits:
//   bits & 7 == 0 (non-zero) : Pointer to 8-byte aligned heap Obj*
//   bits & 3 == 2            : 62-bit signed fixnum (int64_t(bits) >> 2)
//   bits & 0xFF == 0x04      : Immediate special constants (#f, #t, (), eof, undef, unspec)
//   bits & 0xFF == 0x0C      : Character (uint32_t(bits >> 8))
// ============================================================================

struct SCM {
    uint64_t bits = 0;

    constexpr SCM() noexcept = default;
    explicit constexpr SCM(uint64_t b) noexcept : bits(b) {}

    constexpr bool operator==(const SCM& o) const noexcept { return bits == o.bits; }
    constexpr bool operator!=(const SCM& o) const noexcept { return bits != o.bits; }

    static constexpr uint64_t TAG_BOOL_F      = 0x0004ULL;
    static constexpr uint64_t TAG_BOOL_T      = 0x0404ULL;
    static constexpr uint64_t TAG_EOL         = 0x0304ULL;
    static constexpr uint64_t TAG_EOF         = 0x0A04ULL;
    static constexpr uint64_t TAG_UNDEFINED   = 0x0904ULL;
    static constexpr uint64_t TAG_UNSPECIFIED = 0x0804ULL;
    static constexpr uint64_t TAG_UNBOUND     = 0x0B04ULL;
    static constexpr uint64_t TAG_ELISP_NIL   = 0x0104ULL;
    static constexpr uint64_t TAG_CHAR_MASK   = 0x00FFULL;
    static constexpr uint64_t TAG_CHAR_VAL    = 0x000CULL;

    static constexpr int64_t FIXNUM_MIN = -(int64_t{1} << 61);
    static constexpr int64_t FIXNUM_MAX =  (int64_t{1} << 61) - 1;

    static constexpr SCM from_bool(bool b) noexcept {
        return SCM(b ? TAG_BOOL_T : TAG_BOOL_F);
    }
    static constexpr SCM from_fixnum(int64_t v) noexcept {
        return SCM((static_cast<uint64_t>(v) << 2) | 2ULL);
    }
    static constexpr SCM from_char(uint32_t ch) noexcept {
        return SCM((static_cast<uint64_t>(ch) << 8) | TAG_CHAR_VAL);
    }
    static inline SCM from_obj(Obj* p) noexcept {
        return SCM(reinterpret_cast<uint64_t>(p));
    }

    static constexpr bool fits_fixnum(int64_t v) noexcept {
        return v >= FIXNUM_MIN && v <= FIXNUM_MAX;
    }

    constexpr bool is_heap() const noexcept { return bits != 0 && (bits & 7ULL) == 0; }
    constexpr bool is_imp() const noexcept { return !is_heap(); }
    constexpr bool is_fixnum() const noexcept { return (bits & 3ULL) == 2ULL; }
    constexpr bool is_char() const noexcept { return (bits & TAG_CHAR_MASK) == TAG_CHAR_VAL; }
    constexpr bool is_false() const noexcept { return bits == TAG_BOOL_F; }
    constexpr bool is_true() const noexcept { return bits != TAG_BOOL_F; }
    constexpr bool is_bool() const noexcept { return bits == TAG_BOOL_F || bits == TAG_BOOL_T; }
    constexpr bool is_null() const noexcept { return bits == TAG_EOL; }
    constexpr bool is_eof() const noexcept { return bits == TAG_EOF; }
    constexpr bool is_undefined() const noexcept { return bits == TAG_UNDEFINED; }
    constexpr bool is_unspecified() const noexcept { return bits == TAG_UNSPECIFIED; }
    constexpr bool is_unbound() const noexcept { return bits == TAG_UNBOUND; }

    constexpr int64_t to_fixnum() const noexcept {
        return static_cast<int64_t>(bits) >> 2;
    }
    constexpr uint32_t to_char() const noexcept {
        return static_cast<uint32_t>(bits >> 8);
    }
    inline Obj* to_obj() const noexcept {
        return reinterpret_cast<Obj*>(bits);
    }
};

constexpr SCM SCM_BOOL_F      = SCM(SCM::TAG_BOOL_F);
constexpr SCM SCM_BOOL_T      = SCM(SCM::TAG_BOOL_T);
constexpr SCM SCM_EOL         = SCM(SCM::TAG_EOL);
constexpr SCM SCM_EOF         = SCM(SCM::TAG_EOF);
constexpr SCM SCM_UNDEFINED   = SCM(SCM::TAG_UNDEFINED);
constexpr SCM SCM_UNSPECIFIED = SCM(SCM::TAG_UNSPECIFIED);
constexpr SCM SCM_UNBOUND     = SCM(SCM::TAG_UNBOUND);
constexpr SCM SCM_ELISP_NIL   = SCM(SCM::TAG_ELISP_NIL);

// ============================================================================
// Heap Object Tags
// ============================================================================

enum class ObjTag : uint16_t {
    Pair = 1,
    Symbol,
    Keyword,
    String,
    Vector,
    WeakVector,
    Bignum,
    Fraction,
    Real,
    Complex,
    Subr,
    Closure,
    Macro,
    Variable,
    Port,
    Fluid,
    DynamicState,
    HashTable,
    Struct,
    Continuation,
    ContFrame,
    DynWind,
    Promise,
    Values,
    Hook,
    Guardian,
    Arbiter,
    RandomState,
    Regexp,
    Dir,
    UniformVector,
    BitVector,
    Array,
    CharSet,
    ProcWithSetter,
    EvalClosure,
    Smob,
    Stack,
    Frame,
    Environment,
    Observer,
    Mutex,
    CondVar,
    Thread,
    DynamicLib,
    Async,
    Box,
    Bytecode,
};

struct alignas(8) Obj {
    Obj* gc_next = nullptr;
    ObjTag tag;
    uint8_t gc_mark = 0;
    uint8_t flags = 0;
    uint32_t extra = 0;

    explicit Obj(ObjTag t) noexcept : tag(t) {}
};

template <typename T>
inline bool is_a(SCM x, ObjTag t) noexcept {
    return x.is_heap() && x.to_obj()->tag == t;
}

template <typename T>
inline T* as_obj(SCM x) noexcept {
    return static_cast<T*>(x.to_obj());
}

// ============================================================================
// Core Heap Objects
// ============================================================================

struct PairObj : Obj {
    SCM car;
    SCM cdr;
    PairObj(SCM a, SCM d) noexcept : Obj(ObjTag::Pair), car(a), cdr(d) {}
};

struct SymbolObj;
struct StringObj;

using RawSubrFn = SCM(*)(const SCM* args, size_t nargs);

inline bool is_pair(SCM x) noexcept { return is_a<PairObj>(x, ObjTag::Pair); }
inline bool is_symbol(SCM x) noexcept { return is_a<SymbolObj>(x, ObjTag::Symbol); }
inline bool is_string(SCM x) noexcept { return is_a<StringObj>(x, ObjTag::String); }

} // namespace guile
