#include "guile_cpp/vm.hpp"
#include <cstring>
#include <cmath>
#include <limits>
#include <algorithm>

namespace guile {

// ============================================================================
// UniformVectorObj & BitVectorObj methods
// ============================================================================

size_t UniformVectorObj::elem_size() const noexcept {
    switch (utype) {
    case UniformType::U8:  case UniformType::S8:  return 1;
    case UniformType::U16: case UniformType::S16: return 2;
    case UniformType::U32: case UniformType::S32: case UniformType::F32: return 4;
    case UniformType::U64: case UniformType::S64: case UniformType::F64: case UniformType::C32: return 8;
    case UniformType::C64: return 16;
    }
    return 1;
}

UniformVectorObj::UniformVectorObj(UniformType ut, size_t n)
    : Obj(ObjTag::UniformVector), utype(ut), length(n) {
    bytes.resize(n * elem_size(), 0);
}

BitVectorObj::BitVectorObj(size_t n, bool fill)
    : Obj(ObjTag::BitVector), words((n + 63) / 64, fill ? ~0ULL : 0ULL), length(n) {
    if (fill && (n % 64) != 0) {
        words.back() &= (1ULL << (n % 64)) - 1ULL;
    }
}

bool BitVectorObj::get(size_t i) const noexcept {
    return (words[i >> 6] >> (i & 63)) & 1ULL;
}

void BitVectorObj::set(size_t i, bool v) noexcept {
    if (v) words[i >> 6] |= (1ULL << (i & 63));
    else words[i >> 6] &= ~(1ULL << (i & 63));
}

const char* uniform_type_tag(UniformType ut) {
    switch (ut) {
    case UniformType::U8:  return "u8";
    case UniformType::S8:  return "s8";
    case UniformType::U16: return "u16";
    case UniformType::S16: return "s16";
    case UniformType::U32: return "u32";
    case UniformType::S32: return "s32";
    case UniformType::U64: return "u64";
    case UniformType::S64: return "s64";
    case UniformType::F32: return "f32";
    case UniformType::F64: return "f64";
    case UniformType::C32: return "c32";
    case UniformType::C64: return "c64";
    }
    return "u8";
}

SCM uniform_vector_ref(UniformVectorObj* uv, size_t i) {
    const uint8_t* p = uv->bytes.data() + i * uv->elem_size();
    switch (uv->utype) {
    case UniformType::U8:  { uint8_t  v; std::memcpy(&v, p, 1); return vm().make_uinteger(v); }
    case UniformType::S8:  { int8_t   v; std::memcpy(&v, p, 1); return vm().make_integer(v); }
    case UniformType::U16: { uint16_t v; std::memcpy(&v, p, 2); return vm().make_uinteger(v); }
    case UniformType::S16: { int16_t  v; std::memcpy(&v, p, 2); return vm().make_integer(v); }
    case UniformType::U32: { uint32_t v; std::memcpy(&v, p, 4); return vm().make_uinteger(v); }
    case UniformType::S32: { int32_t  v; std::memcpy(&v, p, 4); return vm().make_integer(v); }
    case UniformType::U64: { uint64_t v; std::memcpy(&v, p, 8); return vm().make_uinteger(v); }
    case UniformType::S64: { int64_t  v; std::memcpy(&v, p, 8); return vm().make_integer(v); }
    case UniformType::F32: { float    v; std::memcpy(&v, p, 4); return vm().make_real(v); }
    case UniformType::F64: { double   v; std::memcpy(&v, p, 8); return vm().make_real(v); }
    case UniformType::C32: {
        float r, im; std::memcpy(&r, p, 4); std::memcpy(&im, p + 4, 4);
        return vm().make_complex(r, im);
    }
    case UniformType::C64: {
        double r, im; std::memcpy(&r, p, 8); std::memcpy(&im, p + 8, 8);
        return vm().make_complex(r, im);
    }
    }
    return SCM::from_fixnum(0);
}

void uniform_vector_set(UniformVectorObj* uv, size_t i, SCM val) {
    uint8_t* p = uv->bytes.data() + i * uv->elem_size();
    switch (uv->utype) {
    case UniformType::U8: {
        uint64_t x = scm_to_uint64(val, "u8vector-set!", 3);
        if (x > 255) vm().out_of_range("u8vector-set!", val);
        uint8_t v = static_cast<uint8_t>(x); std::memcpy(p, &v, 1); break;
    }
    case UniformType::S8: {
        int64_t x = scm_to_int64(val, "s8vector-set!", 3);
        if (x < -128 || x > 127) vm().out_of_range("s8vector-set!", val);
        int8_t v = static_cast<int8_t>(x); std::memcpy(p, &v, 1); break;
    }
    case UniformType::U16: {
        uint64_t x = scm_to_uint64(val, "u16vector-set!", 3);
        if (x > 65535) vm().out_of_range("u16vector-set!", val);
        uint16_t v = static_cast<uint16_t>(x); std::memcpy(p, &v, 2); break;
    }
    case UniformType::S16: {
        int64_t x = scm_to_int64(val, "s16vector-set!", 3);
        if (x < -32768 || x > 32767) vm().out_of_range("s16vector-set!", val);
        int16_t v = static_cast<int16_t>(x); std::memcpy(p, &v, 2); break;
    }
    case UniformType::U32: {
        uint64_t x = scm_to_uint64(val, "u32vector-set!", 3);
        if (x > 0xFFFFFFFFULL) vm().out_of_range("u32vector-set!", val);
        uint32_t v = static_cast<uint32_t>(x); std::memcpy(p, &v, 4); break;
    }
    case UniformType::S32: {
        int64_t x = scm_to_int64(val, "s32vector-set!", 3);
        if (x < INT32_MIN || x > INT32_MAX) vm().out_of_range("s32vector-set!", val);
        int32_t v = static_cast<int32_t>(x); std::memcpy(p, &v, 4); break;
    }
    case UniformType::U64: {
        uint64_t v = scm_to_uint64(val, "u64vector-set!", 3);
        std::memcpy(p, &v, 8); break;
    }
    case UniformType::S64: {
        int64_t v = scm_to_int64(val, "s64vector-set!", 3);
        std::memcpy(p, &v, 8); break;
    }
    case UniformType::F32: {
        float v = static_cast<float>(scm_to_double(val, "f32vector-set!"));
        std::memcpy(p, &v, 4); break;
    }
    case UniformType::F64: {
        double v = scm_to_double(val, "f64vector-set!");
        std::memcpy(p, &v, 8); break;
    }
    case UniformType::C32: {
        float r = 0, im = 0;
        if (is_complex_obj(val)) { r = static_cast<float>(as_obj<ComplexObj>(val)->real); im = static_cast<float>(as_obj<ComplexObj>(val)->imag); }
        else { r = static_cast<float>(scm_to_double(val, "c32vector-set!")); }
        std::memcpy(p, &r, 4); std::memcpy(p + 4, &im, 4); break;
    }
    case UniformType::C64: {
        double r = 0, im = 0;
        if (is_complex_obj(val)) { r = as_obj<ComplexObj>(val)->real; im = as_obj<ComplexObj>(val)->imag; }
        else { r = scm_to_double(val, "c64vector-set!"); }
        std::memcpy(p, &r, 8); std::memcpy(p + 8, &im, 8); break;
    }
    }
}

SCM read_uniform_vector_body(UniformType ut, SCM list_val) {
    int64_t n = scm_ilength(list_val);
    if (n < 0) vm().wrong_type_arg("list->uvec", 1, list_val, "proper list");
    auto* uv = vm().heap().alloc<UniformVectorObj>(ut, static_cast<size_t>(n));
    SCM cur = list_val;
    size_t i = 0;
    while (is_pair(cur)) {
        uniform_vector_set(uv, i++, car(cur));
        cur = cdr(cur);
    }
    return SCM::from_obj(uv);
}

// ============================================================================
// Generalized Array Helpers
// ============================================================================

bool is_any_array(SCM x) {
    return is_array(x) || is_vector(x) || is_string(x) || is_bitvector(x) || is_uniform_vector(x);
}

static size_t base_vec_length(SCM bv) {
    if (is_vector(bv)) return as_obj<VectorObj>(bv)->elts.size();
    if (is_string(bv)) return as_obj<StringObj>(bv)->len;
    if (is_bitvector(bv)) return as_obj<BitVectorObj>(bv)->length;
    if (is_uniform_vector(bv)) return as_obj<UniformVectorObj>(bv)->length;
    return 0;
}

static SCM base_vec_ref(SCM bv, size_t idx) {
    if (is_vector(bv)) return as_obj<VectorObj>(bv)->elts[idx];
    if (is_string(bv)) return SCM::from_char(static_cast<unsigned char>(as_obj<StringObj>(bv)->view()[idx]));
    if (is_bitvector(bv)) return SCM::from_bool(as_obj<BitVectorObj>(bv)->get(idx));
    if (is_uniform_vector(bv)) return uniform_vector_ref(as_obj<UniformVectorObj>(bv), idx);
    return SCM_UNSPECIFIED;
}

static void base_vec_set(SCM bv, size_t idx, SCM val) {
    if (is_vector(bv)) { as_obj<VectorObj>(bv)->elts[idx] = val; return; }
    if (is_string(bv)) {
        if (!val.is_char()) vm().wrong_type_arg("array-set!", 2, val, "char");
        as_obj<StringObj>(bv)->mutable_data()[idx] = static_cast<char>(val.to_char());
        return;
    }
    if (is_bitvector(bv)) {
        if (!val.is_bool()) vm().wrong_type_arg("array-set!", 2, val, "boolean");
        as_obj<BitVectorObj>(bv)->set(idx, val.is_true());
        return;
    }
    if (is_uniform_vector(bv)) { uniform_vector_set(as_obj<UniformVectorObj>(bv), idx, val); return; }
}

static void get_array_info(SCM arr, SCM& base, int64_t& offset, std::vector<ArrayDim>& dims) {
    if (is_array(arr)) {
        auto* a = as_obj<ArrayObj>(arr);
        base = a->base_vec;
        offset = a->base_offset;
        dims = a->dims;
    } else if (is_vector(arr) || is_string(arr) || is_bitvector(arr) || is_uniform_vector(arr)) {
        base = arr;
        offset = 0;
        size_t len = base_vec_length(arr);
        dims = {{0, static_cast<int64_t>(len) - 1, 1}};
    } else {
        vm().wrong_type_arg("array", 1, arr, "array");
    }
}

static SCM array_type_sym(SCM arr) {
    SCM base = is_array(arr) ? as_obj<ArrayObj>(arr)->base_vec : arr;
    if (is_vector(base)) return SCM_BOOL_T;
    if (is_string(base)) return vm().make_symbol("a");
    if (is_bitvector(base)) return vm().make_symbol("b");
    if (is_uniform_vector(base)) return vm().make_symbol(uniform_type_tag(as_obj<UniformVectorObj>(base)->utype));
    return SCM_BOOL_F;
}

static bool parse_uniform_type_sym(SCM sym, UniformType& out) {
    if (!is_symbol(sym)) return false;
    const std::string& s = as_obj<SymbolObj>(sym)->name;
    if (s == "u8")  { out = UniformType::U8; return true; }
    if (s == "s8")  { out = UniformType::S8; return true; }
    if (s == "u16") { out = UniformType::U16; return true; }
    if (s == "s16") { out = UniformType::S16; return true; }
    if (s == "u32") { out = UniformType::U32; return true; }
    if (s == "s32") { out = UniformType::S32; return true; }
    if (s == "u64") { out = UniformType::U64; return true; }
    if (s == "s64") { out = UniformType::S64; return true; }
    if (s == "f32") { out = UniformType::F32; return true; }
    if (s == "f64") { out = UniformType::F64; return true; }
    if (s == "c32") { out = UniformType::C32; return true; }
    if (s == "c64") { out = UniformType::C64; return true; }
    return false;
}

static SCM make_base_vec_for_type(SCM type_sym, size_t total_len, SCM fill) {
    if (type_sym == SCM_BOOL_T) {
        return vm().make_vector(total_len, fill.is_undefined() ? SCM_UNSPECIFIED : fill);
    }
    if (is_symbol(type_sym)) {
        const std::string& nm = as_obj<SymbolObj>(type_sym)->name;
        if (nm == "a") {
            char ch = (!fill.is_undefined() && !fill.is_unspecified() && fill.is_char()) ? static_cast<char>(fill.to_char()) : '\0';
            return vm().make_string(std::string(total_len, ch));
        }
        if (nm == "b") {
            bool b = (!fill.is_undefined() && !fill.is_unspecified() && fill.is_true());
            return vm().heap().alloc_scm<BitVectorObj>(total_len, b);
        }
        UniformType ut;
        if (parse_uniform_type_sym(type_sym, ut)) {
            auto* uv = vm().heap().alloc<UniformVectorObj>(ut, total_len);
            if (!fill.is_undefined() && !fill.is_unspecified()) {
                for (size_t i = 0; i < total_len; ++i) uniform_vector_set(uv, i, fill);
            }
            return SCM::from_obj(uv);
        }
    }
    vm().misc_error("make-typed-array", "unknown array type: ~S", vm().list1(type_sym));
}

bool array_equal(SCM a, SCM b) {
    SCM ba, bb;
    int64_t oa = 0, ob = 0;
    std::vector<ArrayDim> da, db;
    get_array_info(a, ba, oa, da);
    get_array_info(b, bb, ob, db);
    if (da.size() != db.size()) return false;
    for (size_t i = 0; i < da.size(); ++i) {
        if (da[i].lbnd != db[i].lbnd || da[i].ubnd != db[i].ubnd) return false;
    }
    std::function<bool(size_t, int64_t, int64_t)> cmp = [&](size_t d, int64_t pa, int64_t pb) -> bool {
        if (d == da.size()) return scm_equal(base_vec_ref(ba, static_cast<size_t>(pa)), base_vec_ref(bb, static_cast<size_t>(pb)));
        for (int64_t i = da[d].lbnd; i <= da[d].ubnd; ++i) {
            int64_t off = i - da[d].lbnd;
            if (!cmp(d + 1, pa + off * da[d].inc, pb + off * db[d].inc)) return false;
        }
        return true;
    };
    return cmp(0, oa, ob);
}

void VM::init_arrays() {
    // ========================================================================
    // SRFI-4 Uniform Numeric Vectors
    // ========================================================================
    auto reg_uvec = [this](UniformType ut, const std::string& pfx) {
        define_proc(pfx + "vector?", 1, 0, false, [ut](const SCM* a, size_t) -> SCM {
            return SCM::from_bool(is_uniform_vector(a[0]) && as_obj<UniformVectorObj>(a[0])->utype == ut);
        });
        define_proc("make-" + pfx + "vector", 1, 1, false, [this, ut, pfx](const SCM* a, size_t nargs) -> SCM {
            int64_t n = scm_to_int64(a[0], ("make-" + pfx + "vector").c_str(), 1);
            if (n < 0) out_of_range(("make-" + pfx + "vector").c_str(), a[0]);
            auto* uv = heap_.alloc<UniformVectorObj>(ut, static_cast<size_t>(n));
            if (nargs >= 2 && !a[1].is_undefined()) {
                for (size_t i = 0; i < static_cast<size_t>(n); ++i) uniform_vector_set(uv, i, a[1]);
            }
            return SCM::from_obj(uv);
        });
        define_proc(pfx + "vector", 0, 0, true, [ut](const SCM* a, size_t nargs) -> SCM {
            return read_uniform_vector_body(ut, nargs > 0 ? a[0] : SCM_EOL);
        });
        define_proc(pfx + "vector-length", 1, 0, false, [this, ut, pfx](const SCM* a, size_t) -> SCM {
            if (!is_uniform_vector(a[0]) || as_obj<UniformVectorObj>(a[0])->utype != ut)
                wrong_type_arg((pfx + "vector-length").c_str(), 1, a[0], (pfx + "vector").c_str());
            return make_uinteger(as_obj<UniformVectorObj>(a[0])->length);
        });
        define_proc(pfx + "vector-ref", 2, 0, false, [this, ut, pfx](const SCM* a, size_t) -> SCM {
            if (!is_uniform_vector(a[0]) || as_obj<UniformVectorObj>(a[0])->utype != ut)
                wrong_type_arg((pfx + "vector-ref").c_str(), 1, a[0], (pfx + "vector").c_str());
            auto* uv = as_obj<UniformVectorObj>(a[0]);
            int64_t k = scm_to_int64(a[1], (pfx + "vector-ref").c_str(), 2);
            if (k < 0 || static_cast<size_t>(k) >= uv->length) out_of_range((pfx + "vector-ref").c_str(), a[1]);
            return uniform_vector_ref(uv, static_cast<size_t>(k));
        });
        define_proc(pfx + "vector-set!", 3, 0, false, [this, ut, pfx](const SCM* a, size_t) -> SCM {
            if (!is_uniform_vector(a[0]) || as_obj<UniformVectorObj>(a[0])->utype != ut)
                wrong_type_arg((pfx + "vector-set!").c_str(), 1, a[0], (pfx + "vector").c_str());
            auto* uv = as_obj<UniformVectorObj>(a[0]);
            int64_t k = scm_to_int64(a[1], (pfx + "vector-set!").c_str(), 2);
            if (k < 0 || static_cast<size_t>(k) >= uv->length) out_of_range((pfx + "vector-set!").c_str(), a[1]);
            uniform_vector_set(uv, static_cast<size_t>(k), a[2]);
            return SCM_UNSPECIFIED;
        });
        define_proc(pfx + "vector->list", 1, 0, false, [this, ut, pfx](const SCM* a, size_t) -> SCM {
            if (!is_uniform_vector(a[0]) || as_obj<UniformVectorObj>(a[0])->utype != ut)
                wrong_type_arg((pfx + "vector->list").c_str(), 1, a[0], (pfx + "vector").c_str());
            auto* uv = as_obj<UniformVectorObj>(a[0]);
            SCM res = SCM_EOL;
            for (size_t i = uv->length; i > 0; --i) res = cons(uniform_vector_ref(uv, i - 1), res);
            return res;
        });
        define_proc("list->" + pfx + "vector", 1, 0, false, [ut](const SCM* a, size_t) -> SCM {
            return read_uniform_vector_body(ut, a[0]);
        });
    };
    reg_uvec(UniformType::U8, "u8");   reg_uvec(UniformType::S8, "s8");
    reg_uvec(UniformType::U16, "u16"); reg_uvec(UniformType::S16, "s16");
    reg_uvec(UniformType::U32, "u32"); reg_uvec(UniformType::S32, "s32");
    reg_uvec(UniformType::U64, "u64"); reg_uvec(UniformType::S64, "s64");
    reg_uvec(UniformType::F32, "f32"); reg_uvec(UniformType::F64, "f64");
    reg_uvec(UniformType::C32, "c32"); reg_uvec(UniformType::C64, "c64");

    define_proc("uniform-vector?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_uniform_vector(a[0]) || is_bitvector(a[0]));
    });
    define_proc("uniform-vector-length", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_uniform_vector(a[0])) return make_uinteger(as_obj<UniformVectorObj>(a[0])->length);
        if (is_bitvector(a[0])) return make_uinteger(as_obj<BitVectorObj>(a[0])->length);
        wrong_type_arg("uniform-vector-length", 1, a[0], "uniform vector");
    });
    define_proc("uniform-vector-ref", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t k = scm_to_int64(a[1], "uniform-vector-ref", 2);
        if (is_uniform_vector(a[0])) {
            auto* uv = as_obj<UniformVectorObj>(a[0]);
            if (k < 0 || static_cast<size_t>(k) >= uv->length) out_of_range("uniform-vector-ref", a[1]);
            return uniform_vector_ref(uv, static_cast<size_t>(k));
        }
        if (is_bitvector(a[0])) {
            auto* bv = as_obj<BitVectorObj>(a[0]);
            if (k < 0 || static_cast<size_t>(k) >= bv->length) out_of_range("uniform-vector-ref", a[1]);
            return SCM::from_bool(bv->get(static_cast<size_t>(k)));
        }
        wrong_type_arg("uniform-vector-ref", 1, a[0], "uniform vector");
    });
    define_proc("uniform-vector-set!", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t k = scm_to_int64(a[1], "uniform-vector-set!", 2);
        if (is_uniform_vector(a[0])) {
            auto* uv = as_obj<UniformVectorObj>(a[0]);
            if (k < 0 || static_cast<size_t>(k) >= uv->length) out_of_range("uniform-vector-set!", a[1]);
            uniform_vector_set(uv, static_cast<size_t>(k), a[2]);
            return SCM_UNSPECIFIED;
        }
        if (is_bitvector(a[0])) {
            auto* bv = as_obj<BitVectorObj>(a[0]);
            if (k < 0 || static_cast<size_t>(k) >= bv->length) out_of_range("uniform-vector-set!", a[1]);
            bv->set(static_cast<size_t>(k), a[2].is_true());
            return SCM_UNSPECIFIED;
        }
        wrong_type_arg("uniform-vector-set!", 1, a[0], "uniform vector");
    });
    define_proc("uniform-vector-element-type", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_uniform_vector(a[0])) return make_symbol(uniform_type_tag(as_obj<UniformVectorObj>(a[0])->utype));
        if (is_bitvector(a[0])) return make_symbol("b");
        wrong_type_arg("uniform-vector-element-type", 1, a[0], "uniform vector");
    });
    define_proc("uniform-vector-element-size", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_uniform_vector(a[0])) return make_uinteger(as_obj<UniformVectorObj>(a[0])->elem_size());
        wrong_type_arg("uniform-vector-element-size", 1, a[0], "uniform vector");
    });

    // ========================================================================
    // BitVectors
    // ========================================================================
    define_proc("bitvector?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_bitvector(a[0])); });
    define_proc("make-bitvector", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        int64_t n = scm_to_int64(a[0], "make-bitvector", 1);
        if (n < 0) out_of_range("make-bitvector", a[0]);
        bool fill = (nargs >= 2 && !a[1].is_undefined() && a[1].is_true());
        return heap_.alloc_scm<BitVectorObj>(static_cast<size_t>(n), fill);
    });
    define_proc("bitvector", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        int64_t n = scm_ilength(lst);
        auto* bv = heap_.alloc<BitVectorObj>(static_cast<size_t>(n), false);
        size_t i = 0;
        while (is_pair(lst)) { bv->set(i++, car(lst).is_true()); lst = cdr(lst); }
        return SCM::from_obj(bv);
    });
    define_proc("bitvector-length", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_bitvector(a[0])) wrong_type_arg("bitvector-length", 1, a[0], "bitvector");
        return make_uinteger(as_obj<BitVectorObj>(a[0])->length);
    });
    define_proc("bitvector-ref", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_bitvector(a[0])) wrong_type_arg("bitvector-ref", 1, a[0], "bitvector");
        auto* bv = as_obj<BitVectorObj>(a[0]);
        int64_t k = scm_to_int64(a[1], "bitvector-ref", 2);
        if (k < 0 || static_cast<size_t>(k) >= bv->length) out_of_range("bitvector-ref", a[1]);
        return SCM::from_bool(bv->get(static_cast<size_t>(k)));
    });
    define_proc("bitvector-set!", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_bitvector(a[0])) wrong_type_arg("bitvector-set!", 1, a[0], "bitvector");
        auto* bv = as_obj<BitVectorObj>(a[0]);
        int64_t k = scm_to_int64(a[1], "bitvector-set!", 2);
        if (k < 0 || static_cast<size_t>(k) >= bv->length) out_of_range("bitvector-set!", a[1]);
        bv->set(static_cast<size_t>(k), a[2].is_true());
        return SCM_UNSPECIFIED;
    });
    define_proc("bitvector->list", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_bitvector(a[0])) wrong_type_arg("bitvector->list", 1, a[0], "bitvector");
        auto* bv = as_obj<BitVectorObj>(a[0]);
        SCM res = SCM_EOL;
        for (size_t i = bv->length; i > 0; --i) res = cons(SCM::from_bool(bv->get(i - 1)), res);
        return res;
    });
    define_proc("list->bitvector", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t n = scm_ilength(a[0]);
        if (n < 0) wrong_type_arg("list->bitvector", 1, a[0], "list");
        auto* bv = heap_.alloc<BitVectorObj>(static_cast<size_t>(n), false);
        SCM cur = a[0]; size_t i = 0;
        while (is_pair(cur)) { bv->set(i++, car(cur).is_true()); cur = cdr(cur); }
        return SCM::from_obj(bv);
    });
    define_proc("bitvector-fill!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_bitvector(a[0])) wrong_type_arg("bitvector-fill!", 1, a[0], "bitvector");
        auto* bv = as_obj<BitVectorObj>(a[0]);
        bool v = a[1].is_true();
        for (size_t i = 0; i < bv->length; ++i) bv->set(i, v);
        return SCM_UNSPECIFIED;
    });
    define_proc("bit-count", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_bitvector(a[1])) wrong_type_arg("bit-count", 2, a[1], "bitvector");
        auto* bv = as_obj<BitVectorObj>(a[1]);
        bool target = a[0].is_true();
        size_t cnt = 0;
        for (size_t i = 0; i < bv->length; ++i) if (bv->get(i) == target) ++cnt;
        return make_uinteger(cnt);
    });
    define_proc("bit-position", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_bitvector(a[1])) wrong_type_arg("bit-position", 2, a[1], "bitvector");
        auto* bv = as_obj<BitVectorObj>(a[1]);
        bool target = a[0].is_true();
        int64_t k = scm_to_int64(a[2], "bit-position", 3);
        if (k < 0) out_of_range("bit-position", a[2]);
        for (size_t i = static_cast<size_t>(k); i < bv->length; ++i) {
            if (bv->get(i) == target) return make_uinteger(i);
        }
        return SCM_BOOL_F;
    });
    define_proc("bit-invert!", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_bitvector(a[0])) wrong_type_arg("bit-invert!", 1, a[0], "bitvector");
        auto* bv = as_obj<BitVectorObj>(a[0]);
        for (size_t i = 0; i < bv->length; ++i) bv->set(i, !bv->get(i));
        return SCM_UNSPECIFIED;
    });

    // ========================================================================
    // Multi-Dimensional Arrays (unif.c, ramap.c)
    // ========================================================================
    define_proc("array?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_any_array(a[0])); });
    define_proc("typed-array?", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        if (!is_any_array(a[0])) return SCM_BOOL_F;
        return SCM::from_bool(array_type_sym(a[0]) == a[1]);
    });
    define_proc("array-type", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_any_array(a[0])) wrong_type_arg("array-type", 1, a[0], "array");
        return array_type_sym(a[0]);
    });
    define_proc("array-rank", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_any_array(a[0])) wrong_type_arg("array-rank", 1, a[0], "array");
        if (is_array(a[0])) return make_uinteger(as_obj<ArrayObj>(a[0])->dims.size());
        return make_integer(1);
    });
    define_proc("array-dimensions", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM base; int64_t off; std::vector<ArrayDim> dims;
        get_array_info(a[0], base, off, dims);
        SCM res = SCM_EOL;
        for (size_t i = dims.size(); i > 0; --i) {
            auto& d = dims[i - 1];
            SCM spec = (d.lbnd == 0) ? make_integer(d.ubnd + 1) : list2(make_integer(d.lbnd), make_integer(d.ubnd));
            res = cons(spec, res);
        }
        return res;
    });

    auto make_typed_array_impl = [this](SCM type_sym, SCM fill, SCM bounds_list) -> SCM {
        std::vector<ArrayDim> dims;
        SCM cur = bounds_list;
        while (is_pair(cur)) {
            SCM b = car(cur);
            ArrayDim d;
            if (is_pair(b)) {
                d.lbnd = scm_to_int64(car(b), "make-typed-array", 3);
                d.ubnd = scm_to_int64(car(cdr(b)), "make-typed-array", 3);
            } else {
                d.lbnd = 0;
                d.ubnd = scm_to_int64(b, "make-typed-array", 3) - 1;
            }
            dims.push_back(d);
            cur = cdr(cur);
        }
        int64_t total = 1;
        for (size_t i = dims.size(); i > 0; --i) {
            dims[i - 1].inc = total;
            int64_t sz = dims[i - 1].ubnd - dims[i - 1].lbnd + 1;
            if (sz < 0) sz = 0;
            total *= sz;
        }
        SCM base = make_base_vec_for_type(type_sym, static_cast<size_t>(total), fill);
        if (dims.size() == 1 && dims[0].lbnd == 0) return base;
        auto* ar = heap_.alloc<ArrayObj>();
        ar->base_vec = base;
        ar->base_offset = 0;
        ar->dims = std::move(dims);
        ar->contiguous = true;
        return SCM::from_obj(ar);
    };

    define_proc("make-typed-array", 2, 0, true, [make_typed_array_impl](const SCM* a, size_t nargs) -> SCM {
        SCM bounds = nargs > 2 ? a[2] : SCM_EOL;
        return make_typed_array_impl(a[0], a[1], bounds);
    });
    define_proc("make-array", 1, 0, true, [make_typed_array_impl](const SCM* a, size_t nargs) -> SCM {
        SCM bounds = nargs > 1 ? a[1] : SCM_EOL;
        return make_typed_array_impl(SCM_BOOL_T, a[0], bounds);
    });

    define_proc("array-in-bounds?", 1, 0, true, [](const SCM* a, size_t nargs) -> SCM {
        SCM base; int64_t off; std::vector<ArrayDim> dims;
        get_array_info(a[0], base, off, dims);
        SCM idxs = nargs > 1 ? a[1] : SCM_EOL;
        size_t d = 0;
        while (is_pair(idxs)) {
            if (d >= dims.size()) return SCM_BOOL_F;
            int64_t idx = scm_to_int64(car(idxs), "array-in-bounds?", 2);
            if (idx < dims[d].lbnd || idx > dims[d].ubnd) return SCM_BOOL_F;
            ++d; idxs = cdr(idxs);
        }
        return SCM::from_bool(d == dims.size());
    });

    define_proc("array-ref", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        SCM base; int64_t off; std::vector<ArrayDim> dims;
        get_array_info(a[0], base, off, dims);
        SCM idxs = nargs > 1 ? a[1] : SCM_EOL;
        size_t d = 0;
        int64_t pos = off;
        while (is_pair(idxs)) {
            if (d >= dims.size()) wrong_num_args("array-ref");
            int64_t idx = scm_to_int64(car(idxs), "array-ref", 2);
            if (idx < dims[d].lbnd || idx > dims[d].ubnd) out_of_range("array-ref", car(idxs));
            pos += (idx - dims[d].lbnd) * dims[d].inc;
            ++d; idxs = cdr(idxs);
        }
        if (d != dims.size()) wrong_num_args("array-ref");
        return base_vec_ref(base, static_cast<size_t>(pos));
    });

    define_proc("array-set!", 2, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        SCM base; int64_t off; std::vector<ArrayDim> dims;
        get_array_info(a[0], base, off, dims);
        SCM val = a[1];
        SCM idxs = nargs > 2 ? a[2] : SCM_EOL;
        size_t d = 0;
        int64_t pos = off;
        while (is_pair(idxs)) {
            if (d >= dims.size()) misc_error("array-set!", "wrong number of indices");
            int64_t idx = scm_to_int64(car(idxs), "array-set!", 3);
            if (idx < dims[d].lbnd || idx > dims[d].ubnd) out_of_range("array-set!", car(idxs));
            pos += (idx - dims[d].lbnd) * dims[d].inc;
            ++d; idxs = cdr(idxs);
        }
        if (d != dims.size()) misc_error("array-set!", "wrong number of indices");
        base_vec_set(base, static_cast<size_t>(pos), val);
        return SCM_UNSPECIFIED;
    });

    define_proc("array->list", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM base; int64_t off; std::vector<ArrayDim> dims;
        get_array_info(a[0], base, off, dims);
        std::function<SCM(size_t, int64_t)> build = [&](size_t d, int64_t pos) -> SCM {
            if (d == dims.size()) return base_vec_ref(base, static_cast<size_t>(pos));
            SCM res = SCM_EOL;
            for (int64_t i = dims[d].ubnd; i >= dims[d].lbnd; --i) {
                res = cons(build(d + 1, pos + (i - dims[d].lbnd) * dims[d].inc), res);
            }
            return res;
        };
        return build(0, off);
    });

    define_proc("list->typed-array", 3, 0, false, [this, make_typed_array_impl](const SCM* a, size_t) -> SCM {
        SCM type_sym = a[0];
        SCM shape = a[1];
        SCM lst = a[2];
        SCM bounds = SCM_EOL;
        if (is_exact_integer(shape)) {
            int64_t rank = scm_to_int64(shape, "list->typed-array", 2);
            SCM cur = lst;
            SCM rev_b = SCM_EOL;
            for (int64_t r = 0; r < rank; ++r) {
                int64_t len = scm_ilength(cur);
                if (len < 0) wrong_type_arg("list->typed-array", 3, lst, "list");
                rev_b = cons(make_integer(len), rev_b);
                cur = (len > 0 && is_pair(cur)) ? car(cur) : SCM_EOL;
            }
            bounds = scm_reverse_x(rev_b);
        } else {
            bounds = shape;
        }
        SCM arr = make_typed_array_impl(type_sym, SCM_UNDEFINED, bounds);
        SCM base; int64_t off; std::vector<ArrayDim> dims;
        get_array_info(arr, base, off, dims);
        std::function<void(size_t, int64_t, SCM)> fill = [&](size_t d, int64_t pos, SCM cur) {
            if (d == dims.size()) { base_vec_set(base, static_cast<size_t>(pos), cur); return; }
            SCM p = cur;
            for (int64_t i = dims[d].lbnd; i <= dims[d].ubnd; ++i) {
                if (!is_pair(p)) wrong_type_arg("list->typed-array", 3, lst, "nested list");
                fill(d + 1, pos + (i - dims[d].lbnd) * dims[d].inc, car(p));
                p = cdr(p);
            }
        };
        fill(0, off, lst);
        return arr;
    });
    define_proc("list->array", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        return call3(as_obj<VariableObj>(lookup("list->typed-array"))->value, SCM_BOOL_T, a[0], a[1]);
    });

    auto prot_to_type = [this](SCM prot) -> SCM {
        if (prot == SCM_BOOL_T || prot == SCM_EOL) return SCM_BOOL_T;
        if (prot == SCM_BOOL_F) return make_symbol("b");
        if (prot.is_char()) return make_symbol("a");
        if (prot.is_fixnum()) {
            int64_t v = prot.to_fixnum();
            if (v == 1) return make_symbol("u32");
            if (v == -1) return make_symbol("s32");
            return make_symbol("s32");
        }
        if (is_real_obj(prot)) return make_symbol("f64");
        if (is_complex_obj(prot)) return make_symbol("c64");
        return SCM_BOOL_T;
    };
    define_proc("dimensions->uniform-array", 2, 1, false, [this, make_typed_array_impl, prot_to_type](const SCM* a, size_t nargs) -> SCM {
        SCM dims = is_exact_integer(a[0]) ? list1(a[0]) : a[0];
        SCM type_sym = prot_to_type(a[1]);
        SCM fill = (nargs >= 3 && !a[2].is_undefined()) ? a[2] : SCM_UNDEFINED;
        return make_typed_array_impl(type_sym, fill, dims);
    });
    define_proc("list->uniform-array", 3, 0, false, [this, prot_to_type](const SCM* a, size_t) -> SCM {
        SCM type_sym = prot_to_type(a[1]);
        return call3(as_obj<VariableObj>(lookup("list->typed-array"))->value, type_sym, a[0], a[2]);
    });

    define_proc("array-equal?", 0, 0, true, [](const SCM* a, size_t nargs) -> SCM {
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        if (!is_pair(lst)) return SCM_BOOL_T;
        SCM prev = car(lst); lst = cdr(lst);
        while (is_pair(lst)) {
            if (!scm_equal(prev, car(lst))) return SCM_BOOL_F;
            prev = car(lst); lst = cdr(lst);
        }
        return SCM_BOOL_T;
    });

    define_proc("array-fill!", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        SCM base; int64_t off; std::vector<ArrayDim> dims;
        get_array_info(a[0], base, off, dims);
        std::function<void(size_t, int64_t)> go = [&](size_t d, int64_t pos) {
            if (d == dims.size()) { base_vec_set(base, static_cast<size_t>(pos), a[1]); return; }
            for (int64_t i = dims[d].lbnd; i <= dims[d].ubnd; ++i) go(d + 1, pos + (i - dims[d].lbnd) * dims[d].inc);
        };
        go(0, off);
        return SCM_UNSPECIFIED;
    });

    define_proc("array-copy!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM b1, b2; int64_t o1, o2; std::vector<ArrayDim> d1, d2;
        get_array_info(a[0], b1, o1, d1);
        get_array_info(a[1], b2, o2, d2);
        if (d1.size() != d2.size()) misc_error("array-copy!", "arrays have different ranks");
        std::function<void(size_t, int64_t, int64_t)> go = [&](size_t d, int64_t p1, int64_t p2) {
            if (d == d1.size()) { base_vec_set(b2, static_cast<size_t>(p2), base_vec_ref(b1, static_cast<size_t>(p1))); return; }
            int64_t len = d1[d].ubnd - d1[d].lbnd + 1;
            for (int64_t i = 0; i < len; ++i) go(d + 1, p1 + i * d1[d].inc, p2 + i * d2[d].inc);
        };
        go(0, o1, o2);
        return SCM_UNSPECIFIED;
    });

    define_proc("array-index-map!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM base; int64_t off; std::vector<ArrayDim> dims;
        get_array_info(a[0], base, off, dims);
        std::vector<SCM> idxs(dims.size());
        std::function<void(size_t, int64_t)> go = [&](size_t d, int64_t pos) {
            if (d == dims.size()) {
                SCM v = apply(a[1], idxs.data(), idxs.size());
                base_vec_set(base, static_cast<size_t>(pos), v);
                return;
            }
            for (int64_t i = dims[d].lbnd; i <= dims[d].ubnd; ++i) {
                idxs[d] = make_integer(i);
                go(d + 1, pos + (i - dims[d].lbnd) * dims[d].inc);
            }
        };
        go(0, off);
        return SCM_UNSPECIFIED;
    });

    define_proc("array-map!", 2, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        SCM dst = a[0], proc = a[1];
        SCM srcs_lst = nargs > 2 ? a[2] : SCM_EOL;
        SCM db; int64_t doff; std::vector<ArrayDim> dd;
        get_array_info(dst, db, doff, dd);
        struct SrcInfo { SCM base; int64_t off; std::vector<ArrayDim> dims; };
        std::vector<SrcInfo> srcs;
        while (is_pair(srcs_lst)) {
            SrcInfo si;
            get_array_info(car(srcs_lst), si.base, si.off, si.dims);
            srcs.push_back(std::move(si));
            srcs_lst = cdr(srcs_lst);
        }
        bool dsubr_1arg = false;
        if (is_subr(proc)) {
            auto* sub = as_obj<SubrObj>(proc);
            if (srcs.size() >= 2 && sub->req == 1 && sub->opt == 0 && !sub->rst) {
                if (!sub->name.empty() && sub->name[0] == '$') {
                    dsubr_1arg = true; // Guile 1.8 dsubr accepts extra sources and uses 1st
                } else {
                    wrong_type_arg("array-map!", 2, proc, "procedure");
                }
            }
        }
        std::vector<int64_t> offsets(srcs.size());
        for (size_t s = 0; s < srcs.size(); ++s) offsets[s] = srcs[s].off;
        std::function<void(size_t, int64_t)> go = [&](size_t d, int64_t dpos) {
            if (d == dd.size()) {
                std::vector<SCM> cargs(srcs.size());
                for (size_t s = 0; s < srcs.size(); ++s) cargs[s] = base_vec_ref(srcs[s].base, static_cast<size_t>(offsets[s]));
                SCM v = dsubr_1arg ? call1(proc, cargs[0]) : apply(proc, cargs.data(), cargs.size());
                base_vec_set(db, static_cast<size_t>(dpos), v);
                return;
            }
            int64_t len = dd[d].ubnd - dd[d].lbnd + 1;
            for (int64_t i = 0; i < len; ++i) {
                std::vector<int64_t> saved = offsets;
                for (size_t s = 0; s < srcs.size(); ++s) offsets[s] += i * srcs[s].dims[d].inc;
                go(d + 1, dpos + i * dd[d].inc);
                offsets = saved;
            }
        };
        go(0, doff);
        return SCM_UNSPECIFIED;
    });

    define_proc("array-for-each", 2, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        SCM proc = a[0];
        SCM first = a[1];
        SCM rest = nargs > 2 ? a[2] : SCM_EOL;
        struct SrcInfo { SCM base; int64_t off; std::vector<ArrayDim> dims; };
        std::vector<SrcInfo> srcs;
        { SrcInfo si; get_array_info(first, si.base, si.off, si.dims); srcs.push_back(std::move(si)); }
        while (is_pair(rest)) {
            SrcInfo si; get_array_info(car(rest), si.base, si.off, si.dims); srcs.push_back(std::move(si));
            rest = cdr(rest);
        }
        std::vector<int64_t> offsets(srcs.size());
        for (size_t s = 0; s < srcs.size(); ++s) offsets[s] = srcs[s].off;
        const auto& dd = srcs[0].dims;
        std::function<void(size_t)> go = [&](size_t d) {
            if (d == dd.size()) {
                std::vector<SCM> cargs(srcs.size());
                for (size_t s = 0; s < srcs.size(); ++s) cargs[s] = base_vec_ref(srcs[s].base, static_cast<size_t>(offsets[s]));
                apply(proc, cargs.data(), cargs.size());
                return;
            }
            int64_t len = dd[d].ubnd - dd[d].lbnd + 1;
            for (int64_t i = 0; i < len; ++i) {
                std::vector<int64_t> saved = offsets;
                for (size_t s = 0; s < srcs.size(); ++s) offsets[s] += i * srcs[s].dims[d].inc;
                go(d + 1);
                offsets = saved;
            }
        };
        go(0);
        return SCM_UNSPECIFIED;
    });

    define_proc("make-shared-array", 2, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        SCM oldarr = a[0], mapfunc = a[1];
        SCM bounds = nargs > 2 ? a[2] : SCM_EOL;
        SCM base; int64_t old_off; std::vector<ArrayDim> old_dims;
        get_array_info(oldarr, base, old_off, old_dims);

        std::vector<ArrayDim> new_dims;
        SCM cur = bounds;
        while (is_pair(cur)) {
            ArrayDim d;
            if (is_pair(car(cur))) {
                d.lbnd = scm_to_int64(car(car(cur)), "make-shared-array", 3);
                d.ubnd = scm_to_int64(car(cdr(car(cur))), "make-shared-array", 3);
            } else {
                d.lbnd = 0;
                d.ubnd = scm_to_int64(car(cur), "make-shared-array", 3) - 1;
            }
            new_dims.push_back(d);
            cur = cdr(cur);
        }

        auto eval_pos = [&](const std::vector<SCM>& idxs, bool check_low, bool check_high) -> int64_t {
            SCM mapped = apply(mapfunc, idxs.data(), idxs.size());
            int64_t pos = old_off;
            size_t d = 0;
            while (is_pair(mapped)) {
                if (d >= old_dims.size()) misc_error("make-shared-array", "mapping out of range");
                int64_t i = scm_to_int64(car(mapped), "make-shared-array", 2);
                if (check_low && (i < old_dims[d].lbnd || i > old_dims[d].ubnd)) {
                    out_of_range("make-shared-array", car(mapped));
                }
                if (check_high && (i < old_dims[d].lbnd || i > old_dims[d].ubnd)) {
                    misc_error("make-shared-array", "mapping out of range");
                }
                pos += (i - old_dims[d].lbnd) * old_dims[d].inc;
                ++d; mapped = cdr(mapped);
            }
            if (d != old_dims.size()) misc_error("make-shared-array", "mapping out of range");
            return pos;
        };

        bool non_empty = true;
        for (const auto& nd : new_dims) {
            if (nd.ubnd < nd.lbnd) { non_empty = false; break; }
        }
        std::vector<SCM> zero_idx(new_dims.size());
        for (size_t d = 0; d < new_dims.size(); ++d) zero_idx[d] = make_integer(new_dims[d].lbnd);
        int64_t new_off = non_empty ? eval_pos(zero_idx, true, false) : 0;
        if (non_empty) {
            std::vector<SCM> hi_idx(new_dims.size());
            for (size_t d = 0; d < new_dims.size(); ++d) hi_idx[d] = make_integer(new_dims[d].ubnd);
            (void)eval_pos(hi_idx, false, true);
        }
        for (size_t d = 0; d < new_dims.size(); ++d) {
            if (non_empty && new_dims[d].ubnd > new_dims[d].lbnd) {
                auto probe = zero_idx;
                probe[d] = make_integer(new_dims[d].lbnd + 1);
                new_dims[d].inc = eval_pos(probe, false, true) - new_off;
            } else {
                new_dims[d].inc = 0;
            }
        }
        auto* ar = heap_.alloc<ArrayObj>();
        ar->base_vec = base;
        ar->base_offset = new_off;
        ar->dims = std::move(new_dims);
        ar->contiguous = false;
        return SCM::from_obj(ar);
    });

    define_proc("shared-array-root", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return is_array(a[0]) ? as_obj<ArrayObj>(a[0])->base_vec : a[0];
    });
    define_proc("shared-array-offset", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return make_integer(is_array(a[0]) ? as_obj<ArrayObj>(a[0])->base_offset : 0);
    });
    define_proc("shared-array-increments", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM base; int64_t off; std::vector<ArrayDim> dims;
        get_array_info(a[0], base, off, dims);
        SCM res = SCM_EOL;
        for (size_t i = dims.size(); i > 0; --i) res = cons(make_integer(dims[i - 1].inc), res);
        return res;
    });
    define_proc("transpose-array", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        SCM base; int64_t off; std::vector<ArrayDim> dims;
        get_array_info(a[0], base, off, dims);
        SCM args = nargs > 1 ? a[1] : SCM_EOL;
        std::vector<int64_t> perm;
        int64_t max_d = -1;
        while (is_pair(args)) {
            int64_t p = scm_to_int64(car(args), "transpose-array", 2);
            perm.push_back(p);
            if (p > max_d) max_d = p;
            args = cdr(args);
        }
        std::vector<ArrayDim> ndims(static_cast<size_t>(max_d + 1));
        std::vector<bool> seen(ndims.size(), false);
        for (size_t i = 0; i < perm.size() && i < dims.size(); ++i) {
            size_t t = static_cast<size_t>(perm[i]);
            if (!seen[t]) { ndims[t] = dims[i]; seen[t] = true; }
            else { ndims[t].inc += dims[i].inc; }
        }
        auto* ar = heap_.alloc<ArrayObj>();
        ar->base_vec = base;
        ar->base_offset = off;
        ar->dims = std::move(ndims);
        ar->contiguous = false;
        return SCM::from_obj(ar);
    });
}

} // namespace guile
