#include "guile_cpp/vm.hpp"
#include <cctype>
#include <algorithm>
#include <cstring>

namespace guile {

namespace {

void parse_str_bounds(SCM str, const SCM* args, size_t nargs, size_t start_idx,
                      const char* subr, size_t& start, size_t& end) {
    if (!is_string(str)) vm().wrong_type_arg(subr, 1, str, "string");
    size_t len = as_obj<StringObj>(str)->len;
    int64_t s = (nargs > start_idx && !args[start_idx].is_undefined())
                ? scm_to_int64(args[start_idx], subr, static_cast<int>(start_idx + 1)) : 0;
    int64_t e = (nargs > start_idx + 1 && !args[start_idx + 1].is_undefined())
                ? scm_to_int64(args[start_idx + 1], subr, static_cast<int>(start_idx + 2)) : static_cast<int64_t>(len);
    if (s < 0 || e < s || static_cast<size_t>(e) > len) vm().out_of_range(subr, args[start_idx]);
    start = static_cast<size_t>(s);
    end = static_cast<size_t>(e);
}

bool match_char_pred(SCM pred, unsigned char c) {
    if (pred.is_char()) return pred.to_char() == c;
    if (is_charset(pred)) return as_obj<CharSetObj>(pred)->contains(c);
    if (vm().is_procedure(pred)) return vm().call1(pred, SCM::from_char(c)).is_true();
    vm().wrong_type_arg("char-pred", 1, pred, "char, char-set, or procedure");
}

SCM make_cs_from_pred(bool (*p)(unsigned char)) {
    auto* cs = vm().heap().alloc<CharSetObj>();
    for (int i = 0; i < 256; ++i) {
        if (p(static_cast<unsigned char>(i))) cs->add(static_cast<uint8_t>(i));
    }
    return SCM::from_obj(cs);
}

} // namespace

void VM::init_srfi13_14() {
    // ========================================================================
    // SRFI-14: Character Sets
    // ========================================================================
    define_proc("char-set?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_charset(a[0]));
    });
    define_proc("char-set=", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        if (!is_pair(lst)) return SCM_BOOL_T;
        SCM first = car(lst);
        if (!is_charset(first)) wrong_type_arg("char-set=", 1, first, "char-set");
        auto* c0 = as_obj<CharSetObj>(first);
        lst = cdr(lst);
        while (is_pair(lst)) {
            if (!is_charset(car(lst))) wrong_type_arg("char-set=", 2, car(lst), "char-set");
            auto* ci = as_obj<CharSetObj>(car(lst));
            if (std::memcmp(c0->bits, ci->bits, sizeof(c0->bits)) != 0) return SCM_BOOL_F;
            lst = cdr(lst);
        }
        return SCM_BOOL_T;
    });
    define_proc("char-set<=", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        if (!is_pair(lst)) return SCM_BOOL_T;
        SCM prev = car(lst);
        if (!is_charset(prev)) wrong_type_arg("char-set<=", 1, prev, "char-set");
        lst = cdr(lst);
        while (is_pair(lst)) {
            SCM cur = car(lst);
            if (!is_charset(cur)) wrong_type_arg("char-set<=", 2, cur, "char-set");
            auto* a0 = as_obj<CharSetObj>(prev);
            auto* b0 = as_obj<CharSetObj>(cur);
            for (int i = 0; i < 4; ++i) {
                if ((a0->bits[i] & ~b0->bits[i]) != 0) return SCM_BOOL_F;
            }
            prev = cur;
            lst = cdr(lst);
        }
        return SCM_BOOL_T;
    });
    define_proc("char-set-hash", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-hash", 1, a[0], "char-set");
        uint64_t bound = (nargs >= 2 && !a[1].is_undefined() && scm_to_int64(a[1], "char-set-hash", 2) > 0)
                         ? scm_to_uint64(a[1], "char-set-hash", 2) : 871ULL;
        auto* cs = as_obj<CharSetObj>(a[0]);
        uint64_t h = cs->bits[0] ^ (cs->bits[1] * 31) ^ (cs->bits[2] * 131) ^ (cs->bits[3] * 1313);
        return make_uinteger(h % bound);
    });
    define_proc("char-set-copy", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-copy", 1, a[0], "char-set");
        auto* n = heap_.alloc<CharSetObj>();
        std::memcpy(n->bits, as_obj<CharSetObj>(a[0])->bits, sizeof(n->bits));
        return SCM::from_obj(n);
    });
    define_proc("char-set", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        auto* cs = heap_.alloc<CharSetObj>();
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        while (is_pair(lst)) {
            if (!car(lst).is_char()) wrong_type_arg("char-set", 1, car(lst), "char");
            cs->add(static_cast<uint8_t>(car(lst).to_char()));
            lst = cdr(lst);
        }
        return SCM::from_obj(cs);
    });
    define_proc("list->char-set", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        auto* cs = heap_.alloc<CharSetObj>();
        if (nargs >= 2 && !a[1].is_undefined()) {
            if (!is_charset(a[1])) wrong_type_arg("list->char-set", 2, a[1], "char-set");
            std::memcpy(cs->bits, as_obj<CharSetObj>(a[1])->bits, sizeof(cs->bits));
        }
        SCM lst = a[0];
        while (is_pair(lst)) {
            if (!car(lst).is_char()) wrong_type_arg("list->char-set", 1, car(lst), "char");
            cs->add(static_cast<uint8_t>(car(lst).to_char()));
            lst = cdr(lst);
        }
        return SCM::from_obj(cs);
    });
    define_proc("list->char-set!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[1])) wrong_type_arg("list->char-set!", 2, a[1], "char-set");
        auto* cs = as_obj<CharSetObj>(a[1]);
        SCM lst = a[0];
        while (is_pair(lst)) {
            if (!car(lst).is_char()) wrong_type_arg("list->char-set!", 1, car(lst), "char");
            cs->add(static_cast<uint8_t>(car(lst).to_char()));
            lst = cdr(lst);
        }
        return a[1];
    });
    define_proc("string->char-set", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("string->char-set", 1, a[0], "string");
        auto* cs = heap_.alloc<CharSetObj>();
        if (nargs >= 2 && !a[1].is_undefined()) {
            if (!is_charset(a[1])) wrong_type_arg("string->char-set", 2, a[1], "char-set");
            std::memcpy(cs->bits, as_obj<CharSetObj>(a[1])->bits, sizeof(cs->bits));
        }
        for (unsigned char c : as_obj<StringObj>(a[0])->str()) cs->add(c);
        return SCM::from_obj(cs);
    });
    define_proc("string->char-set!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("string->char-set!", 1, a[0], "string");
        if (!is_charset(a[1])) wrong_type_arg("string->char-set!", 2, a[1], "char-set");
        auto* cs = as_obj<CharSetObj>(a[1]);
        for (unsigned char c : as_obj<StringObj>(a[0])->str()) cs->add(c);
        return a[1];
    });
    define_proc("char-set->list", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set->list", 1, a[0], "char-set");
        auto* cs = as_obj<CharSetObj>(a[0]);
        SCM res = SCM_EOL;
        for (int i = 255; i >= 0; --i) {
            if (cs->contains(static_cast<uint8_t>(i))) res = cons(SCM::from_char(static_cast<uint32_t>(i)), res);
        }
        return res;
    });
    define_proc("char-set->string", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set->string", 1, a[0], "char-set");
        auto* cs = as_obj<CharSetObj>(a[0]);
        string res;
        for (int i = 0; i < 256; ++i) {
            if (cs->contains(static_cast<uint8_t>(i))) (res << static_cast<char>(i));
        }
        return make_string_owned(res);
    });
    define_proc("char-set-size", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-size", 1, a[0], "char-set");
        auto* cs = as_obj<CharSetObj>(a[0]);
        int cnt = 0;
        for (int i = 0; i < 4; ++i) cnt += __builtin_popcountll(cs->bits[i]);
        return make_integer(cnt);
    });
    define_proc("char-set-contains?", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-contains?", 1, a[0], "char-set");
        if (!a[1].is_char()) wrong_type_arg("char-set-contains?", 2, a[1], "char");
        return SCM::from_bool(as_obj<CharSetObj>(a[0])->contains(static_cast<uint8_t>(a[1].to_char())));
    });
    define_proc("char-set-count", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[1])) wrong_type_arg("char-set-count", 2, a[1], "char-set");
        auto* cs = as_obj<CharSetObj>(a[1]);
        int cnt = 0;
        for (int i = 0; i < 256; ++i) {
            if (cs->contains(static_cast<uint8_t>(i)) && call1(a[0], SCM::from_char(static_cast<uint32_t>(i))).is_true()) ++cnt;
        }
        return make_integer(cnt);
    });
    define_proc("char-set-every", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[1])) wrong_type_arg("char-set-every", 2, a[1], "char-set");
        auto* cs = as_obj<CharSetObj>(a[1]);
        SCM last = SCM_BOOL_T;
        for (int i = 0; i < 256; ++i) {
            if (cs->contains(static_cast<uint8_t>(i))) {
                last = call1(a[0], SCM::from_char(static_cast<uint32_t>(i)));
                if (last.is_false()) return SCM_BOOL_F;
            }
        }
        return last;
    });
    define_proc("char-set-any", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[1])) wrong_type_arg("char-set-any", 2, a[1], "char-set");
        auto* cs = as_obj<CharSetObj>(a[1]);
        for (int i = 0; i < 256; ++i) {
            if (cs->contains(static_cast<uint8_t>(i))) {
                SCM v = call1(a[0], SCM::from_char(static_cast<uint32_t>(i)));
                if (v.is_true()) return v;
            }
        }
        return SCM_BOOL_F;
    });
    define_proc("char-set-fold", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[2])) wrong_type_arg("char-set-fold", 3, a[2], "char-set");
        auto* cs = as_obj<CharSetObj>(a[2]);
        SCM acc = a[1];
        for (int i = 0; i < 256; ++i) {
            if (cs->contains(static_cast<uint8_t>(i))) acc = call2(a[0], SCM::from_char(static_cast<uint32_t>(i)), acc);
        }
        return acc;
    });
    define_proc("char-set-for-each", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[1])) wrong_type_arg("char-set-for-each", 2, a[1], "char-set");
        auto* cs = as_obj<CharSetObj>(a[1]);
        for (int i = 0; i < 256; ++i) {
            if (cs->contains(static_cast<uint8_t>(i))) call1(a[0], SCM::from_char(static_cast<uint32_t>(i)));
        }
        return SCM_UNSPECIFIED;
    });
    define_proc("char-set-map", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[1])) wrong_type_arg("char-set-map", 2, a[1], "char-set");
        auto* cs = as_obj<CharSetObj>(a[1]);
        auto* out = heap_.alloc<CharSetObj>();
        for (int i = 0; i < 256; ++i) {
            if (cs->contains(static_cast<uint8_t>(i))) {
                SCM c = call1(a[0], SCM::from_char(static_cast<uint32_t>(i)));
                if (!c.is_char()) wrong_type_arg("char-set-map", 1, c, "char");
                out->add(static_cast<uint8_t>(c.to_char()));
            }
        }
        return SCM::from_obj(out);
    });
    define_proc("char-set-filter", 2, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[1])) wrong_type_arg("char-set-filter", 2, a[1], "char-set");
        auto* src = as_obj<CharSetObj>(a[1]);
        auto* out = heap_.alloc<CharSetObj>();
        if (nargs >= 3 && !a[2].is_undefined()) {
            if (!is_charset(a[2])) wrong_type_arg("char-set-filter", 3, a[2], "char-set");
            std::memcpy(out->bits, as_obj<CharSetObj>(a[2])->bits, sizeof(out->bits));
        }
        for (int i = 0; i < 256; ++i) {
            if (src->contains(static_cast<uint8_t>(i)) && call1(a[0], SCM::from_char(static_cast<uint32_t>(i))).is_true()) {
                out->add(static_cast<uint8_t>(i));
            }
        }
        return SCM::from_obj(out);
    });
    define_proc("char-set-filter!", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[1])) wrong_type_arg("char-set-filter!", 2, a[1], "char-set");
        if (!is_charset(a[2])) wrong_type_arg("char-set-filter!", 3, a[2], "char-set");
        auto* src = as_obj<CharSetObj>(a[1]);
        auto* out = as_obj<CharSetObj>(a[2]);
        for (int i = 0; i < 256; ++i) {
            if (src->contains(static_cast<uint8_t>(i)) && call1(a[0], SCM::from_char(static_cast<uint32_t>(i))).is_true()) {
                out->add(static_cast<uint8_t>(i));
            }
        }
        return a[2];
    });
    define_proc("ucs-range->char-set", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        int64_t lo = scm_to_int64(a[0], "ucs-range->char-set", 1);
        int64_t hi = scm_to_int64(a[1], "ucs-range->char-set", 2);
        if (lo < 0 || hi < lo) out_of_range("ucs-range->char-set", a[0]);
        auto* cs = heap_.alloc<CharSetObj>();
        if (nargs >= 4 && !a[3].is_undefined()) {
            if (!is_charset(a[3])) wrong_type_arg("ucs-range->char-set", 4, a[3], "char-set");
            std::memcpy(cs->bits, as_obj<CharSetObj>(a[3])->bits, sizeof(cs->bits));
        }
        for (int64_t i = lo; i < hi && i < 256; ++i) cs->add(static_cast<uint8_t>(i));
        return SCM::from_obj(cs);
    });
    define_proc("ucs-range->char-set!", 4, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t lo = scm_to_int64(a[0], "ucs-range->char-set!", 1);
        int64_t hi = scm_to_int64(a[1], "ucs-range->char-set!", 2);
        if (!is_charset(a[3])) wrong_type_arg("ucs-range->char-set!", 4, a[3], "char-set");
        auto* cs = as_obj<CharSetObj>(a[3]);
        for (int64_t i = lo; i < hi && i < 256; ++i) cs->add(static_cast<uint8_t>(i));
        return a[3];
    });
    define_proc("char-set-cursor", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-cursor", 1, a[0], "char-set");
        auto* cs = as_obj<CharSetObj>(a[0]);
        for (int i = 0; i < 256; ++i) if (cs->contains(static_cast<uint8_t>(i))) return make_integer(i);
        return make_integer(-1);
    });
    define_proc("char-set-ref", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-ref", 1, a[0], "char-set");
        auto* cs = as_obj<CharSetObj>(a[0]);
        int64_t c = scm_to_int64(a[1], "char-set-ref", 2);
        if (c < 0 || c > 255 || !cs->contains(static_cast<unsigned char>(c))) {
            misc_error("char-set-ref", "invalid character set cursor: ~S", list1(a[1]));
        }
        return SCM::from_char(static_cast<uint32_t>(c));
    });
    define_proc("char-set-cursor-next", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-cursor-next", 1, a[0], "char-set");
        int64_t cur = scm_to_int64(a[1], "char-set-cursor-next", 2);
        auto* cs = as_obj<CharSetObj>(a[0]);
        for (int64_t i = cur + 1; i < 256; ++i) if (cs->contains(static_cast<uint8_t>(i))) return make_integer(i);
        return make_integer(-1);
    });
    define_proc("end-of-char-set?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(a[0].is_fixnum() && a[0].to_fixnum() < 0);
    });

    // Char-set algebra
    define_proc("char-set-complement", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-complement", 1, a[0], "char-set");
        auto* out = heap_.alloc<CharSetObj>();
        auto* in = as_obj<CharSetObj>(a[0]);
        for (int i = 0; i < 4; ++i) out->bits[i] = ~in->bits[i];
        return SCM::from_obj(out);
    });
    define_proc("char-set-complement!", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-complement!", 1, a[0], "char-set");
        auto* cs = as_obj<CharSetObj>(a[0]);
        for (int i = 0; i < 4; ++i) cs->bits[i] = ~cs->bits[i];
        return a[0];
    });
    define_proc("char-set-union", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        auto* out = heap_.alloc<CharSetObj>();
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        while (is_pair(lst)) {
            if (!is_charset(car(lst))) wrong_type_arg("char-set-union", 1, car(lst), "char-set");
            auto* c = as_obj<CharSetObj>(car(lst));
            for (int i = 0; i < 4; ++i) out->bits[i] |= c->bits[i];
            lst = cdr(lst);
        }
        return SCM::from_obj(out);
    });
    define_proc("char-set-union!", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-union!", 1, a[0], "char-set");
        auto* out = as_obj<CharSetObj>(a[0]);
        SCM lst = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(lst)) {
            if (!is_charset(car(lst))) wrong_type_arg("char-set-union!", 2, car(lst), "char-set");
            auto* c = as_obj<CharSetObj>(car(lst));
            for (int i = 0; i < 4; ++i) out->bits[i] |= c->bits[i];
            lst = cdr(lst);
        }
        return a[0];
    });
    define_proc("char-set-intersection", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        auto* out = heap_.alloc<CharSetObj>();
        for (int i = 0; i < 4; ++i) out->bits[i] = ~0ULL;
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        while (is_pair(lst)) {
            if (!is_charset(car(lst))) wrong_type_arg("char-set-intersection", 1, car(lst), "char-set");
            auto* c = as_obj<CharSetObj>(car(lst));
            for (int i = 0; i < 4; ++i) out->bits[i] &= c->bits[i];
            lst = cdr(lst);
        }
        return SCM::from_obj(out);
    });
    define_proc("char-set-intersection!", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-intersection!", 1, a[0], "char-set");
        auto* out = as_obj<CharSetObj>(a[0]);
        SCM lst = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(lst)) {
            if (!is_charset(car(lst))) wrong_type_arg("char-set-intersection!", 2, car(lst), "char-set");
            auto* c = as_obj<CharSetObj>(car(lst));
            for (int i = 0; i < 4; ++i) out->bits[i] &= c->bits[i];
            lst = cdr(lst);
        }
        return a[0];
    });
    define_proc("char-set-difference", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-difference", 1, a[0], "char-set");
        auto* out = heap_.alloc<CharSetObj>();
        std::memcpy(out->bits, as_obj<CharSetObj>(a[0])->bits, sizeof(out->bits));
        SCM lst = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(lst)) {
            if (!is_charset(car(lst))) wrong_type_arg("char-set-difference", 2, car(lst), "char-set");
            auto* c = as_obj<CharSetObj>(car(lst));
            for (int i = 0; i < 4; ++i) out->bits[i] &= ~c->bits[i];
            lst = cdr(lst);
        }
        return SCM::from_obj(out);
    });
    define_proc("char-set-difference!", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-difference!", 1, a[0], "char-set");
        auto* out = as_obj<CharSetObj>(a[0]);
        SCM lst = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(lst)) {
            if (!is_charset(car(lst))) wrong_type_arg("char-set-difference!", 2, car(lst), "char-set");
            auto* c = as_obj<CharSetObj>(car(lst));
            for (int i = 0; i < 4; ++i) out->bits[i] &= ~c->bits[i];
            lst = cdr(lst);
        }
        return a[0];
    });
    define_proc("char-set-xor", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        auto* out = heap_.alloc<CharSetObj>();
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        while (is_pair(lst)) {
            if (!is_charset(car(lst))) wrong_type_arg("char-set-xor", 1, car(lst), "char-set");
            auto* c = as_obj<CharSetObj>(car(lst));
            for (int i = 0; i < 4; ++i) out->bits[i] ^= c->bits[i];
            lst = cdr(lst);
        }
        return SCM::from_obj(out);
    });
    define_proc("char-set-xor!", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-xor!", 1, a[0], "char-set");
        auto* out = as_obj<CharSetObj>(a[0]);
        SCM lst = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(lst)) {
            if (!is_charset(car(lst))) wrong_type_arg("char-set-xor!", 2, car(lst), "char-set");
            auto* c = as_obj<CharSetObj>(car(lst));
            for (int i = 0; i < 4; ++i) out->bits[i] ^= c->bits[i];
            lst = cdr(lst);
        }
        return a[0];
    });
    define_proc("char-set-adjoin", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-adjoin", 1, a[0], "char-set");
        auto* out = heap_.alloc<CharSetObj>();
        std::memcpy(out->bits, as_obj<CharSetObj>(a[0])->bits, sizeof(out->bits));
        SCM lst = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(lst)) {
            if (!car(lst).is_char()) wrong_type_arg("char-set-adjoin", 2, car(lst), "char");
            out->add(static_cast<uint8_t>(car(lst).to_char()));
            lst = cdr(lst);
        }
        return SCM::from_obj(out);
    });
    define_proc("char-set-adjoin!", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-adjoin!", 1, a[0], "char-set");
        auto* out = as_obj<CharSetObj>(a[0]);
        SCM lst = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(lst)) {
            if (!car(lst).is_char()) wrong_type_arg("char-set-adjoin!", 2, car(lst), "char");
            out->add(static_cast<uint8_t>(car(lst).to_char()));
            lst = cdr(lst);
        }
        return a[0];
    });
    define_proc("char-set-delete", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-delete", 1, a[0], "char-set");
        auto* out = heap_.alloc<CharSetObj>();
        std::memcpy(out->bits, as_obj<CharSetObj>(a[0])->bits, sizeof(out->bits));
        SCM lst = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(lst)) {
            if (!car(lst).is_char()) wrong_type_arg("char-set-delete", 2, car(lst), "char");
            out->remove(static_cast<uint8_t>(car(lst).to_char()));
            lst = cdr(lst);
        }
        return SCM::from_obj(out);
    });
    define_proc("char-set-delete!", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-delete!", 1, a[0], "char-set");
        auto* out = as_obj<CharSetObj>(a[0]);
        SCM lst = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(lst)) {
            if (!car(lst).is_char()) wrong_type_arg("char-set-delete!", 2, car(lst), "char");
            out->remove(static_cast<uint8_t>(car(lst).to_char()));
            lst = cdr(lst);
        }
        return a[0];
    });
    define_proc("char-set-diff+intersection", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[0])) wrong_type_arg("char-set-diff+intersection", 1, a[0], "char-set");
        auto* diff = heap_.alloc<CharSetObj>();
        auto* inter = heap_.alloc<CharSetObj>();
        std::memcpy(diff->bits, as_obj<CharSetObj>(a[0])->bits, sizeof(diff->bits));
        SCM lst = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(lst)) {
            if (!is_charset(car(lst))) wrong_type_arg("char-set-diff+intersection", 2, car(lst), "char-set");
            auto* c = as_obj<CharSetObj>(car(lst));
            for (int i = 0; i < 4; ++i) {
                inter->bits[i] |= (diff->bits[i] & c->bits[i]);
                diff->bits[i] &= ~c->bits[i];
            }
            lst = cdr(lst);
        }
        return make_values(list2(SCM::from_obj(diff), SCM::from_obj(inter)));
    });
    define_proc("char-set-diff+intersection!", 2, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_charset(a[0]) || !is_charset(a[1])) wrong_type_arg("char-set-diff+intersection!", 1, a[0], "char-set");
        auto* cs1 = as_obj<CharSetObj>(a[0]);
        auto* cs2 = as_obj<CharSetObj>(a[1]);
        uint64_t inter_bits[4];
        for (int i = 0; i < 4; ++i) {
            inter_bits[i] = cs1->bits[i] & cs2->bits[i];
            cs1->bits[i] &= ~cs2->bits[i];
        }
        SCM lst = nargs > 2 ? a[2] : SCM_EOL;
        while (is_pair(lst)) {
            auto* c = as_obj<CharSetObj>(car(lst));
            for (int i = 0; i < 4; ++i) {
                inter_bits[i] |= (cs1->bits[i] & c->bits[i]);
                cs1->bits[i] &= ~c->bits[i];
            }
            lst = cdr(lst);
        }
        std::memcpy(cs2->bits, inter_bits, sizeof(inter_bits));
        return make_values(list2(a[0], a[1]));
    });
    define_proc("char-set-unfold", 4, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        auto* cs = heap_.alloc<CharSetObj>();
        if (nargs >= 5 && !a[4].is_undefined()) std::memcpy(cs->bits, as_obj<CharSetObj>(a[4])->bits, sizeof(cs->bits));
        SCM seed = a[3];
        while (call1(a[0], seed).is_false()) {
            SCM c = call1(a[1], seed);
            if (!c.is_char()) wrong_type_arg("char-set-unfold", 2, c, "char");
            cs->add(static_cast<uint8_t>(c.to_char()));
            seed = call1(a[2], seed);
        }
        return SCM::from_obj(cs);
    });
    define_proc("char-set-unfold!", 5, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_charset(a[4])) wrong_type_arg("char-set-unfold!", 5, a[4], "char-set");
        auto* cs = as_obj<CharSetObj>(a[4]);
        SCM seed = a[3];
        while (call1(a[0], seed).is_false()) {
            SCM c = call1(a[1], seed);
            if (!c.is_char()) wrong_type_arg("char-set-unfold!", 2, c, "char");
            cs->add(static_cast<uint8_t>(c.to_char()));
            seed = call1(a[2], seed);
        }
        return a[4];
    });
    define_proc("->char-set", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_charset(a[0])) return a[0];
        auto* cs = heap_.alloc<CharSetObj>();
        if (a[0].is_char()) { cs->add(static_cast<uint8_t>(a[0].to_char())); return SCM::from_obj(cs); }
        if (is_string(a[0])) {
            for (unsigned char c : as_obj<StringObj>(a[0])->str()) cs->add(c);
            return SCM::from_obj(cs);
        }
        wrong_type_arg("->char-set", 1, a[0], "char, string, or char-set");
    });

    // Standard SRFI-14 char-sets
    define("char-set:lower-case",   make_cs_from_pred([](unsigned char c) { return std::islower(c) != 0; }));
    define("char-set:upper-case",   make_cs_from_pred([](unsigned char c) { return std::isupper(c) != 0; }));
    define("char-set:title-case",   make_cs_from_pred([](unsigned char) { return false; }));
    define("char-set:letter",       make_cs_from_pred([](unsigned char c) { return std::isalpha(c) != 0; }));
    define("char-set:digit",        make_cs_from_pred([](unsigned char c) { return std::isdigit(c) != 0; }));
    define("char-set:letter+digit", make_cs_from_pred([](unsigned char c) { return std::isalnum(c) != 0; }));
    define("char-set:graphic",      make_cs_from_pred([](unsigned char c) { return c >= 0x21 && c <= 0x7E; }));
    define("char-set:printing",     make_cs_from_pred([](unsigned char c) { return (c >= 0x20 && c <= 0x7E) || c == '\t' || c == '\n' || c == '\v' || c == '\f' || c == '\r'; }));
    define("char-set:whitespace",   make_cs_from_pred([](unsigned char c) { return std::isspace(c) != 0; }));
    define("char-set:blank",        make_cs_from_pred([](unsigned char c) { return c == ' ' || c == '\t'; }));
    define("char-set:iso-control",  make_cs_from_pred([](unsigned char c) { return c <= 0x1F || (c >= 0x7F && c <= 0x9F); }));
    define("char-set:punctuation",  make_cs_from_pred([](unsigned char c) {
        return c == '!' || c == '"' || c == '#' || c == '%' || c == '&' || c == '\'' ||
               c == '(' || c == ')' || c == '*' || c == ',' || c == '-' || c == '.' ||
               c == '/' || c == ':' || c == ';' || c == '?' || c == '@' || c == '[' ||
               c == '\\' || c == ']' || c == '_' || c == '{' || c == '}';
    }));
    define("char-set:symbol",       make_cs_from_pred([](unsigned char c) {
        return c == '$' || c == '+' || c == '<' || c == '=' || c == '>' || c == '^' || c == '`' || c == '|' || c == '~';
    }));
    define("char-set:hex-digit",    make_cs_from_pred([](unsigned char c) { return std::isxdigit(c) != 0; }));
    define("char-set:ascii",        make_cs_from_pred([](unsigned char c) { return c < 128; }));
    define("char-set:empty",        make_cs_from_pred([](unsigned char) { return false; }));
    define("char-set:full",         make_cs_from_pred([](unsigned char) { return true; }));

    // ========================================================================
    // SRFI-13: String Library (Guile 1.8.8 core)
    // ========================================================================
    define_proc("string-any-c-code", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[1], a, nargs, 2, "string-any", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[1]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        for (size_t i = s; i < e; ++i) {
            if (is_procedure(a[0])) {
                SCM r = call1(a[0], SCM::from_char(static_cast<unsigned char>(sv[i])));
                if (r.is_true()) return r;
            } else if (match_char_pred(a[0], static_cast<unsigned char>(sv[i]))) {
                return SCM_BOOL_T;
            }
        }
        return SCM_BOOL_F;
    });
    define_proc("string-every-c-code", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[1], a, nargs, 2, "string-every", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[1]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        SCM last = SCM_BOOL_T;
        for (size_t i = s; i < e; ++i) {
            if (is_procedure(a[0])) {
                last = call1(a[0], SCM::from_char(static_cast<unsigned char>(sv[i])));
                if (last.is_false()) return SCM_BOOL_F;
            } else if (!match_char_pred(a[0], static_cast<unsigned char>(sv[i]))) {
                return SCM_BOOL_F;
            }
        }
        return last;
    });

    define_proc("string-tabulate", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("string-tabulate", 1, a[0], "procedure");
        int64_t len = scm_to_int64(a[1], "string-tabulate", 2);
        if (len < 0) out_of_range("string-tabulate", a[1]);
        string res;
        (void) 0;
        for (int64_t i = 0; i < len; ++i) {
            SCM c = call1(a[0], make_integer(i));
            if (!c.is_char()) wrong_type_arg("string-tabulate", 1, c, "char");
            (res << static_cast<char>(c.to_char()));
        }
        return make_string_owned(res);
    });

    define_proc("reverse-list->string", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        string s;
        SCM cur = a[0];
        while (is_pair(cur)) {
            if (!car(cur).is_char()) wrong_type_arg("reverse-list->string", 1, car(cur), "char");
            (s << static_cast<char>(car(cur).to_char()));
            cur = cdr(cur);
        }
        std::reverse(begin(s), end(s));
        return make_string_owned(s);
    });

    define_proc("string-join", 1, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        string delim = (nargs >= 2 && !a[1].is_undefined()) ? as_obj<StringObj>(a[1])->str() : " ";
        SCM grammar = (nargs >= 3 && !a[2].is_undefined()) ? a[2] : make_symbol("infix");
        string res;
        bool first = true;
        SCM cur = a[0];
        if (cur.is_null() && grammar == make_symbol("strict-infix")) {
            misc_error("string-join", "strict-infix grammar requires non-empty list");
        }
        while (is_pair(cur)) {
            if (!is_string(car(cur))) wrong_type_arg("string-join", 1, car(cur), "string");
            if (grammar == make_symbol("prefix")) res << delim;
            else if (!first && (grammar == make_symbol("infix") || grammar == make_symbol("strict-infix"))) res << delim;
            str_append(res, as_obj<StringObj>(car(cur))->chars(), as_obj<StringObj>(car(cur))->len);
            if (grammar == make_symbol("suffix")) res << delim;
            first = false;
            cur = cdr(cur);
        }
        return make_string_owned(res);
    });

    define_proc("string-copy!", 3, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        // (string-copy! target tstart s [start [end]])
        if (!is_string(a[0])) wrong_type_arg("string-copy!", 1, a[0], "string");
        int64_t tstart = scm_to_int64(a[1], "string-copy!", 2);
        size_t s = 0, e = 0;
        parse_str_bounds(a[2], a, nargs, 3, "string-copy!", s, e);
        auto* dst = as_obj<StringObj>(a[0]);
        auto src = as_obj<StringObj>(a[2])->str();
        if (tstart < 0 || static_cast<size_t>(tstart) + (e - s) > dst->len) out_of_range("string-copy!", a[1]);
        std::memmove(dst->mutable_data() + tstart, gcpp_data(src) + s, e - s);
        return SCM_UNSPECIFIED;
    });

    define_proc("string-take", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("string-take", 1, a[0], "string");
        int64_t n = scm_to_int64(a[1], "string-take", 2);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len;
        if (n < 0 || static_cast<size_t>(n) > sv_n) out_of_range("string-take", a[1]);
        return make_string(view_sub(sv, sv_n, 0, static_cast<size_t>(n)));
    });
    define_proc("string-take-right", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("string-take-right", 1, a[0], "string");
        int64_t n = scm_to_int64(a[1], "string-take-right", 2);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len;
        if (n < 0 || static_cast<size_t>(n) > sv_n) out_of_range("string-take-right", a[1]);
        return make_string(view_sub(sv, sv_n, sv_n - static_cast<size_t>(n)));
    });
    define_proc("string-drop", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("string-drop", 1, a[0], "string");
        int64_t n = scm_to_int64(a[1], "string-drop", 2);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len;
        if (n < 0 || static_cast<size_t>(n) > sv_n) out_of_range("string-drop", a[1]);
        return make_string(view_sub(sv, sv_n, static_cast<size_t>(n)));
    });
    define_proc("string-drop-right", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("string-drop-right", 1, a[0], "string");
        int64_t n = scm_to_int64(a[1], "string-drop-right", 2);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len;
        if (n < 0 || static_cast<size_t>(n) > sv_n) out_of_range("string-drop-right", a[1]);
        return make_string(view_sub(sv, sv_n, 0, sv_n - static_cast<size_t>(n)));
    });

    define_proc("string-pad", 2, 3, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("string-pad", 1, a[0], "string");
        int64_t len = scm_to_int64(a[1], "string-pad", 2);
        if (len < 0) out_of_range("string-pad", a[1]);
        char pad = (nargs >= 3 && !a[2].is_undefined()) ? static_cast<char>(a[2].to_char()) : ' ';
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 3, "string-pad", s, e);
        auto sv = as_obj<StringObj>(a[0])->sub(s, e - s);
        if (static_cast<size_t>(len) <= static_cast<size_t>(N(sv))) return make_string(str_sub(sv, static_cast<size_t>(N(sv)) - static_cast<size_t>(len)));
        return make_string_owned(string(pad, static_cast<int>(len) - N(sv)) * sv);
    });
    define_proc("string-pad-right", 2, 3, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("string-pad-right", 1, a[0], "string");
        int64_t len = scm_to_int64(a[1], "string-pad-right", 2);
        if (len < 0) out_of_range("string-pad-right", a[1]);
        char pad = (nargs >= 3 && !a[2].is_undefined()) ? static_cast<char>(a[2].to_char()) : ' ';
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 3, "string-pad-right", s, e);
        auto sv = as_obj<StringObj>(a[0])->sub(s, e - s);
        if (static_cast<size_t>(len) <= static_cast<size_t>(N(sv))) return make_string(str_sub(sv, 0, static_cast<size_t>(len)));
        return make_string_owned(sv * string(pad, static_cast<int>(len) - N(sv)));
    });

    auto reg_trim = [this](const char* name, bool left, bool right) {
        define_proc(name, 1, 3, false, [this, name, left, right](const SCM* a, size_t nargs) -> SCM {
            size_t s = 0, e = 0;
            parse_str_bounds(a[0], a, nargs, 2, name, s, e);
            SCM pred = (nargs >= 2 && !a[1].is_undefined()) ? a[1] : SCM_UNDEFINED;
            StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len;
            auto is_trim = [&](unsigned char c) {
                return pred.is_undefined() ? (std::isspace(c) != 0) : match_char_pred(pred, c);
            };
            if (left) while (s < e && is_trim(static_cast<unsigned char>(sv[s]))) ++s;
            if (right) while (e > s && is_trim(static_cast<unsigned char>(sv[e - 1]))) --e;
            return make_string(view_sub(sv, sv_n, s, e - s));
        });
    };
    reg_trim("string-trim", true, false);
    reg_trim("string-trim-right", false, true);
    reg_trim("string-trim-both", true, true);

    define_proc("string-index", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 2, "string-index", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        for (size_t i = s; i < e; ++i) {
            if (match_char_pred(a[1], static_cast<unsigned char>(sv[i]))) return make_uinteger(i);
        }
        return SCM_BOOL_F;
    });
    define_proc("string-rindex", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 2, "string-rindex", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        for (size_t i = e; i > s; --i) {
            if (match_char_pred(a[1], static_cast<unsigned char>(sv[i - 1]))) return make_uinteger(i - 1);
        }
        return SCM_BOOL_F;
    });
    define_proc("string-index-right", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 2, "string-index-right", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        for (size_t i = e; i > s; --i) {
            if (match_char_pred(a[1], static_cast<unsigned char>(sv[i - 1]))) return make_uinteger(i - 1);
        }
        return SCM_BOOL_F;
    });
    define_proc("string-skip", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 2, "string-skip", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        for (size_t i = s; i < e; ++i) {
            if (!match_char_pred(a[1], static_cast<unsigned char>(sv[i]))) return make_uinteger(i);
        }
        return SCM_BOOL_F;
    });
    define_proc("string-skip-right", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 2, "string-skip-right", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        for (size_t i = e; i > s; --i) {
            if (!match_char_pred(a[1], static_cast<unsigned char>(sv[i - 1]))) return make_uinteger(i - 1);
        }
        return SCM_BOOL_F;
    });
    define_proc("string-count", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 2, "string-count", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        size_t cnt = 0;
        for (size_t i = s; i < e; ++i) {
            if (match_char_pred(a[1], static_cast<unsigned char>(sv[i]))) ++cnt;
        }
        return make_uinteger(cnt);
    });

    auto reg_contains = [this](const char* name, bool ci) {
        define_proc(name, 2, 4, false, [this, name, ci](const SCM* a, size_t nargs) -> SCM {
            size_t s1 = 0, e1 = 0, s2 = 0, e2 = 0;
            parse_str_bounds(a[0], a, nargs, 2, name, s1, e1);
            parse_str_bounds(a[1], a, nargs, 4, name, s2, e2);
            StringObj* v1_o = as_obj<StringObj>(a[0]); const char* v1 = v1_o->chars(); size_t v1_n = v1_o->len; (void) v1_n;
            StringObj* v2_o = as_obj<StringObj>(a[1]); const char* v2 = v2_o->chars(); size_t v2_n = v2_o->len; (void) v2_n;
            size_t n2 = e2 - s2;
            if (n2 == 0) return make_uinteger(s1);
            if (n2 > e1 - s1) return SCM_BOOL_F;
            for (size_t i = s1; i + n2 <= e1; ++i) {
                bool ok = true;
                for (size_t j = 0; j < n2; ++j) {
                    unsigned char c1 = static_cast<unsigned char>(v1[i + j]);
                    unsigned char c2 = static_cast<unsigned char>(v2[s2 + j]);
                    if (ci) { c1 = std::tolower(c1); c2 = std::tolower(c2); }
                    if (c1 != c2) { ok = false; break; }
                }
                if (ok) return make_uinteger(i);
            }
            return SCM_BOOL_F;
        });
    };
    reg_contains("string-contains", false);
    reg_contains("string-contains-ci", true);

    // Case mapping
    auto reg_case = [this](const char* name, int mode, bool destructive) {
        // mode: 0=up, 1=down, 2=title/capitalize
        define_proc(name, 1, 2, false, [this, name, mode, destructive](const SCM* a, size_t nargs) -> SCM {
            size_t s = 0, e = 0;
            parse_str_bounds(a[0], a, nargs, 1, name, s, e);
            SCM target = destructive ? a[0] : make_string(as_obj<StringObj>(a[0])->chars(), as_obj<StringObj>(a[0])->len);
            char* d = as_obj<StringObj>(target)->mutable_data();
            bool in_word = false;
            for (size_t i = s; i < e; ++i) {
                unsigned char c = static_cast<unsigned char>(d[i]);
                if (mode == 0) d[i] = static_cast<char>(std::toupper(c));
                else if (mode == 1) d[i] = static_cast<char>(std::tolower(c));
                else {
                    if (std::isalpha(c)) {
                        d[i] = static_cast<char>(in_word ? std::tolower(c) : std::toupper(c));
                        in_word = true;
                    } else {
                        in_word = false;
                    }
                }
            }
            return target;
        });
    };
    reg_case("string-upcase", 0, false);
    reg_case("string-upcase!", 0, true);
    reg_case("string-downcase", 1, false);
    reg_case("string-downcase!", 1, true);
    reg_case("string-titlecase", 2, false);
    reg_case("string-titlecase!", 2, true);
    reg_case("string-capitalize", 2, false);
    reg_case("string-capitalize!", 2, true);

    define_proc("string-reverse", 1, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 1, "string-reverse", s, e);
        string res(as_obj<StringObj>(a[0])->sub(s, e - s));
        std::reverse(begin(res), end(res));
        return make_string_owned(res);
    });
    define_proc("string-reverse!", 1, 2, false, [](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 1, "string-reverse!", s, e);
        char* d = as_obj<StringObj>(a[0])->mutable_data();
        std::reverse(d + s, d + e);
        return a[0];
    });

    define_proc("string-concatenate", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        string res;
        SCM cur = a[0];
        while (is_pair(cur)) {
            if (!is_string(car(cur))) wrong_type_arg("string-concatenate", 1, car(cur), "string");
            str_append(res, as_obj<StringObj>(car(cur))->chars(), as_obj<StringObj>(car(cur))->len);
            cur = cdr(cur);
        }
        if (!cur.is_null()) wrong_type_arg("string-concatenate", 1, a[0], "proper list");
        return make_string_owned(res);
    });
    define_proc("string-concatenate-reverse", 1, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        array<string> parts;
        SCM cur = a[0];
        while (is_pair(cur)) {
            if (!is_string(car(cur))) wrong_type_arg("string-concatenate-reverse", 1, car(cur), "string");
            (parts << as_obj<StringObj>(car(cur))->str());
            cur = cdr(cur);
        }
        string res;
        for (size_t i = static_cast<size_t>(N(parts)); i > 0; --i) str_append(res, parts[i - 1]);
        if (nargs >= 2 && !a[1].is_undefined()) {
            if (!is_string(a[1])) wrong_type_arg("string-concatenate-reverse", 2, a[1], "string");
            StringObj* tail_o = as_obj<StringObj>(a[1]); const char* tail = tail_o->chars(); size_t tail_n = tail_o->len;
            size_t end = (nargs >= 3 && !a[2].is_undefined()) ? static_cast<size_t>(scm_to_int64(a[2], "string-concatenate-reverse", 3)) : tail_n;
            if (end > tail_n) out_of_range("string-concatenate-reverse", a[2]);
            str_append(res, view_sub(tail, tail_n, 0, end));
        }
        return make_string_owned(res);
    });
    define_proc("string-concatenate/shared", 1, 0, false, [this](const SCM* a, size_t nargs) -> SCM {
        return call1(as_obj<VariableObj>(lookup("string-concatenate"))->value, a[0]);
    });
    define_proc("string-concatenate-reverse/shared", 1, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        array<SCM> v = array_from<SCM>(a, nargs);
        return apply(as_obj<VariableObj>(lookup("string-concatenate-reverse"))->value, gcpp_data(v), static_cast<size_t>(N(v)));
    });

    define_proc("string-map", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("string-map", 1, a[0], "procedure");
        size_t s = 0, e = 0;
        parse_str_bounds(a[1], a, nargs, 2, "string-map", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[1]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        string res;
        (void) 0;
        for (size_t i = s; i < e; ++i) {
            SCM c = call1(a[0], SCM::from_char(static_cast<unsigned char>(sv[i])));
            if (!c.is_char()) wrong_type_arg("string-map", 1, c, "char");
            (res << static_cast<char>(c.to_char()));
        }
        return make_string_owned(res);
    });
    define_proc("string-map!", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("string-map!", 1, a[0], "procedure");
        size_t s = 0, e = 0;
        parse_str_bounds(a[1], a, nargs, 2, "string-map!", s, e);
        auto* so = as_obj<StringObj>(a[1]);
        for (size_t i = s; i < e; ++i) {
            SCM c = call1(a[0], SCM::from_char(static_cast<unsigned char>(so->chars()[i])));
            if (!c.is_char()) wrong_type_arg("string-map!", 1, c, "char");
            so->mutable_data()[i] = static_cast<char>(c.to_char());
        }
        return SCM_UNSPECIFIED;
    });
    define_proc("string-fold", 3, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[2], a, nargs, 3, "string-fold", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[2]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        SCM acc = a[1];
        for (size_t i = s; i < e; ++i) acc = call2(a[0], SCM::from_char(static_cast<unsigned char>(sv[i])), acc);
        return acc;
    });
    define_proc("string-fold-right", 3, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[2], a, nargs, 3, "string-fold-right", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[2]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        SCM acc = a[1];
        for (size_t i = e; i > s; --i) acc = call2(a[0], SCM::from_char(static_cast<unsigned char>(sv[i - 1])), acc);
        return acc;
    });
    define_proc("string-for-each", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("string-for-each", 1, a[0], "procedure");
        size_t s = 0, e = 0;
        parse_str_bounds(a[1], a, nargs, 2, "string-for-each", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[1]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        for (size_t i = s; i < e; ++i) call1(a[0], SCM::from_char(static_cast<unsigned char>(sv[i])));
        return SCM_UNSPECIFIED;
    });
    define_proc("string-for-each-index", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_procedure(a[0])) wrong_type_arg("string-for-each-index", 1, a[0], "procedure");
        size_t s = 0, e = 0;
        parse_str_bounds(a[1], a, nargs, 2, "string-for-each-index", s, e);
        for (size_t i = s; i < e; ++i) call1(a[0], make_uinteger(i));
        return SCM_UNSPECIFIED;
    });
    define_proc("string-filter", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 2, "string-filter", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        string res;
        for (size_t i = s; i < e; ++i) {
            if (match_char_pred(a[1], static_cast<unsigned char>(sv[i]))) (res << sv[i]);
        }
        return make_string_owned(res);
    });
    define_proc("string-delete", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 2, "string-delete", s, e);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len; (void) sv_n;
        string res;
        for (size_t i = s; i < e; ++i) {
            if (!match_char_pred(a[1], static_cast<unsigned char>(sv[i]))) (res << sv[i]);
        }
        return make_string_owned(res);
    });
    define_proc("string-tokenize", 1, 3, false, [this](const SCM* a, size_t nargs) -> SCM {
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 2, "string-tokenize", s, e);
        SCM cs = (nargs >= 2 && !a[1].is_undefined()) ? a[1] : SCM_UNDEFINED;
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len;
        auto in_tok = [&](unsigned char c) {
            return cs.is_undefined() ? (std::isgraph(c) != 0) : match_char_pred(cs, c);
        };
        SCM res = SCM_EOL;
        size_t i = s;
        while (i < e) {
            while (i < e && !in_tok(static_cast<unsigned char>(sv[i]))) ++i;
            size_t start = i;
            while (i < e && in_tok(static_cast<unsigned char>(sv[i]))) ++i;
            if (i > start) res = cons(make_string(view_sub(sv, sv_n, start, i - start)), res);
        }
        return scm_reverse_x(res);
    });
    define_proc("string-replace", 2, 4, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("string-replace", 1, a[0], "string");
        if (!is_string(a[1])) wrong_type_arg("string-replace", 2, a[1], "string");
        StringObj* v1_o = as_obj<StringObj>(a[0]); const char* v1 = v1_o->chars(); size_t v1_n = v1_o->len;
        StringObj* v2_o = as_obj<StringObj>(a[1]); const char* v2 = v2_o->chars(); size_t v2_n = v2_o->len;
        int64_t st1 = (nargs >= 3 && !a[2].is_undefined()) ? scm_to_int64(a[2], "string-replace", 3) : 0;
        int64_t en1 = (nargs >= 4 && !a[3].is_undefined()) ? scm_to_int64(a[3], "string-replace", 4) : static_cast<int64_t>(v1_n);
        int64_t st2 = (nargs >= 5 && !a[4].is_undefined()) ? scm_to_int64(a[4], "string-replace", 5) : 0;
        int64_t en2 = (nargs >= 6 && !a[5].is_undefined()) ? scm_to_int64(a[5], "string-replace", 6) : static_cast<int64_t>(v2_n);
        if (st1 < 0 || en1 < st1 || static_cast<size_t>(en1) > v1_n ||
            st2 < 0 || en2 < st2 || static_cast<size_t>(en2) > v2_n) out_of_range("string-replace", a[2]);
        string res;
        str_append(res, view_sub(v1, v1_n, 0, static_cast<size_t>(st1)));
        str_append(res, view_sub(v2, v2_n, static_cast<size_t>(st2), static_cast<size_t>(en2 - st2)));
        str_append(res, view_sub(v1, v1_n, static_cast<size_t>(en1)));
        return make_string_owned(res);
    });
    define_proc("xsubstring", 2, 3, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("xsubstring", 1, a[0], "string");
        int64_t from = scm_to_int64(a[1], "xsubstring", 2);
        StringObj* sv_o = as_obj<StringObj>(a[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len;
        size_t s = (nargs >= 4 && !a[3].is_undefined()) ? static_cast<size_t>(scm_to_int64(a[3], "xsubstring", 4)) : 0;
        size_t e = (nargs >= 5 && !a[4].is_undefined()) ? static_cast<size_t>(scm_to_int64(a[4], "xsubstring", 5)) : sv_n;
        auto sub = view_sub(sv, sv_n, s, e - s);
        int64_t to = (nargs >= 3 && !a[2].is_undefined()) ? scm_to_int64(a[2], "xsubstring", 3) : (from + static_cast<int64_t>(static_cast<size_t>(N(sub))));
        if (to < from) out_of_range("xsubstring", a[2]);
        if (to == from) return make_string("");
        if ((N(sub) == 0)) misc_error("xsubstring", "cannot replicate empty string");
        string res;
        (void) 0;
        int64_t n = static_cast<int64_t>(static_cast<size_t>(N(sub)));
        for (int64_t i = from; i < to; ++i) {
            int64_t idx = ((i % n) + n) % n;
            (res << sub[static_cast<size_t>(idx)]);
        }
        return make_string_owned(res);
    });
    define_proc("string-xcopy!", 4, 3, false, [this](const SCM* a, size_t nargs) -> SCM {
        // (string-xcopy! target tstart s sfrom [sto [start [end]]])
        if (!is_string(a[0]) || !is_string(a[2])) wrong_type_arg("string-xcopy!", 1, a[0], "string");
        int64_t tstart = scm_to_int64(a[1], "string-xcopy!", 2);
        array<SCM> xs_args;
        (xs_args << a[2]);
        (xs_args << a[3]);
        for (size_t i = 4; i < nargs; ++i) (xs_args << a[i]);
        SCM rep = apply(as_obj<VariableObj>(lookup("xsubstring"))->value, gcpp_data(xs_args), static_cast<size_t>(N(xs_args)));
        auto rv = as_obj<StringObj>(rep)->str();
        auto* dst = as_obj<StringObj>(a[0]);
        if (tstart < 0 || static_cast<size_t>(tstart) + static_cast<size_t>(N(rv)) > dst->len) out_of_range("string-xcopy!", a[1]);
        std::memcpy(dst->mutable_data() + tstart, gcpp_data(rv), static_cast<size_t>(N(rv)));
        return SCM_UNSPECIFIED;
    });
    define_proc("string-unfold", 4, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        string res;
        if (nargs >= 5 && !a[4].is_undefined()) res = as_obj<StringObj>(a[4])->str();
        SCM seed = a[3];
        while (call1(a[0], seed).is_false()) {
            SCM c = call1(a[1], seed);
            if (!c.is_char()) wrong_type_arg("string-unfold", 2, c, "char");
            (res << static_cast<char>(c.to_char()));
            seed = call1(a[2], seed);
        }
        if (nargs >= 6 && !a[5].is_undefined()) {
            SCM tail = call1(a[5], seed);
            str_append(res, as_obj<StringObj>(tail)->chars(), as_obj<StringObj>(tail)->len);
        }
        return make_string_owned(res);
    });
    define_proc("string-unfold-right", 4, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        string chars;
        SCM seed = a[3];
        while (call1(a[0], seed).is_false()) {
            SCM c = call1(a[1], seed);
            if (!c.is_char()) wrong_type_arg("string-unfold-right", 2, c, "char");
            (chars << static_cast<char>(c.to_char()));
            seed = call1(a[2], seed);
        }
        std::reverse(begin(chars), end(chars));
        string prefix;
        if (nargs >= 6 && !a[5].is_undefined()) {
            SCM hd = call1(a[5], seed);
            prefix = as_obj<StringObj>(hd)->str();
        }
        string suffix;
        if (nargs >= 5 && !a[4].is_undefined()) suffix = as_obj<StringObj>(a[4])->str();
        return make_string_owned(prefix * chars * suffix);
    });

    // Prefix/Suffix/Compare/Hash
    auto reg_affix = [this](const char* name, bool is_prefix, bool ci, bool bool_ret) {
        define_proc(name, 2, 4, false, [this, name, is_prefix, ci, bool_ret](const SCM* a, size_t nargs) -> SCM {
            size_t s1 = 0, e1 = 0, s2 = 0, e2 = 0;
            parse_str_bounds(a[0], a, nargs, 2, name, s1, e1);
            parse_str_bounds(a[1], a, nargs, 4, name, s2, e2);
            auto v1 = as_obj<StringObj>(a[0])->sub(s1, e1 - s1);
            auto v2 = as_obj<StringObj>(a[1])->sub(s2, e2 - s2);
            size_t n = std::min(static_cast<size_t>(N(v1)), static_cast<size_t>(N(v2)));
            size_t match = 0;
            for (size_t i = 0; i < n; ++i) {
                unsigned char c1 = static_cast<unsigned char>(is_prefix ? v1[i] : v1[static_cast<size_t>(N(v1)) - 1 - i]);
                unsigned char c2 = static_cast<unsigned char>(is_prefix ? v2[i] : v2[static_cast<size_t>(N(v2)) - 1 - i]);
                if (ci) { c1 = std::tolower(c1); c2 = std::tolower(c2); }
                if (c1 != c2) break;
                ++match;
            }
            if (bool_ret) return SCM::from_bool(match == static_cast<size_t>(N(v1)));
            return make_uinteger(match);
        });
    };
    reg_affix("string-prefix-length", true, false, false);
    reg_affix("string-prefix-length-ci", true, true, false);
    reg_affix("string-suffix-length", false, false, false);
    reg_affix("string-suffix-length-ci", false, true, false);
    reg_affix("string-prefix?", true, false, true);
    reg_affix("string-prefix-ci?", true, true, true);
    reg_affix("string-suffix?", false, false, true);
    reg_affix("string-suffix-ci?", false, true, true);

    auto reg_scmp = [this](const char* name, bool ci) {
        define_proc(name, 5, 4, false, [this, name, ci](const SCM* a, size_t nargs) -> SCM {
            size_t s1 = 0, e1 = 0, s2 = 0, e2 = 0;
            parse_str_bounds(a[0], a, nargs, 5, name, s1, e1);
            parse_str_bounds(a[1], a, nargs, 7, name, s2, e2);
            StringObj* v1_o = as_obj<StringObj>(a[0]); const char* v1 = v1_o->chars(); size_t v1_n = v1_o->len; (void) v1_n;
            StringObj* v2_o = as_obj<StringObj>(a[1]); const char* v2 = v2_o->chars(); size_t v2_n = v2_o->len; (void) v2_n;
            size_t i = s1, j = s2;
            while (i < e1 && j < e2) {
                unsigned char c1 = static_cast<unsigned char>(v1[i]);
                unsigned char c2 = static_cast<unsigned char>(v2[j]);
                if (ci) { c1 = std::tolower(c1); c2 = std::tolower(c2); }
                if (c1 < c2) return call1(a[2], make_uinteger(i));
                if (c1 > c2) return call1(a[4], make_uinteger(i));
                ++i; ++j;
            }
            if (i < e1) return call1(a[4], make_uinteger(i));
            if (j < e2) return call1(a[2], make_uinteger(i));
            return call1(a[3], make_uinteger(i));
        });
    };
    reg_scmp("string-compare", false);
    reg_scmp("string-compare-ci", true);

    auto reg_sord = [this](const char* name, bool ci, int op) {
        // op: 0==, 1=<>, 2=<, 3=>, 4=<=, 5=>=
        define_proc(name, 2, 4, false, [this, name, ci, op](const SCM* a, size_t nargs) -> SCM {
            size_t s1 = 0, e1 = 0, s2 = 0, e2 = 0;
            parse_str_bounds(a[0], a, nargs, 2, name, s1, e1);
            parse_str_bounds(a[1], a, nargs, 4, name, s2, e2);
            StringObj* v1_o = as_obj<StringObj>(a[0]); const char* v1 = v1_o->chars(); size_t v1_n = v1_o->len; (void) v1_n;
            StringObj* v2_o = as_obj<StringObj>(a[1]); const char* v2 = v2_o->chars(); size_t v2_n = v2_o->len; (void) v2_n;
            size_t i = s1, j = s2;
            int cmp = 0;
            while (i < e1 && j < e2) {
                unsigned char c1 = static_cast<unsigned char>(v1[i]);
                unsigned char c2 = static_cast<unsigned char>(v2[j]);
                if (ci) { c1 = std::tolower(c1); c2 = std::tolower(c2); }
                if (c1 != c2) { cmp = (c1 < c2) ? -1 : 1; break; }
                ++i; ++j;
            }
            if (cmp == 0) {
                if (i < e1) cmp = 1;
                else if (j < e2) cmp = -1;
            }
            bool res = (op == 0) ? (cmp == 0) : (op == 1) ? (cmp != 0)
                     : (op == 2) ? (cmp < 0)  : (op == 3) ? (cmp > 0)
                     : (op == 4) ? (cmp <= 0) : (cmp >= 0);
            return res ? make_uinteger(i) : SCM_BOOL_F;
        });
    };
    reg_sord("string=", false, 0);    reg_sord("string<>", false, 1);
    reg_sord("string<", false, 2);    reg_sord("string>", false, 3);
    reg_sord("string<=", false, 4);   reg_sord("string>=", false, 5);
    reg_sord("string-ci=", true, 0);  reg_sord("string-ci<>", true, 1);
    reg_sord("string-ci<", true, 2);  reg_sord("string-ci>", true, 3);
    reg_sord("string-ci<=", true, 4); reg_sord("string-ci>=", true, 5);

    define_proc("string-hash", 1, 3, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("string-hash", 1, a[0], "string");
        uint64_t bound = (nargs >= 2 && !a[1].is_undefined() && scm_to_int64(a[1], "string-hash", 2) > 0)
                         ? scm_to_uint64(a[1], "string-hash", 2) : 871ULL;
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 2, "string-hash", s, e);
        return make_uinteger(scm_hash_string(as_obj<StringObj>(a[0])->sub(s, e - s)) % bound);
    });
    define_proc("string-hash-ci", 1, 3, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("string-hash-ci", 1, a[0], "string");
        uint64_t bound = (nargs >= 2 && !a[1].is_undefined() && scm_to_int64(a[1], "string-hash-ci", 2) > 0)
                         ? scm_to_uint64(a[1], "string-hash-ci", 2) : 871ULL;
        size_t s = 0, e = 0;
        parse_str_bounds(a[0], a, nargs, 2, "string-hash-ci", s, e);
        string tmp(as_obj<StringObj>(a[0])->sub(s, e - s));
        for (char& c : tmp) c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
        return make_uinteger(scm_hash_string(tmp) % bound);
    });
}

} // namespace guile
