#include "guile_cpp/vm.hpp"
#include <cctype>
#include <algorithm>

namespace guile {

// (TeXmacs: make_string copies the characters, as the std::string of
// guile_cpp did; make_string_owned takes a string which nothing else keeps)
SCM VM::make_string_owned(string s, bool read_only) {
    if ((N(s) == 0) && !read_only) {
        static SCM s_nullstr = SCM_BOOL_F;
        if (s_nullstr.is_false()) {
            s_nullstr = permanent_object(heap_.alloc_scm<StringObj>(string(), false));
        }
        return s_nullstr;
    }
    return heap_.alloc_scm<StringObj>(s, read_only);
}

SCM VM::make_string(const char* p, size_t n, bool read_only) {
    return make_string_owned(string(p, static_cast<int>(n)), read_only);
}

SCM VM::make_string(string s, bool read_only) {
    return make_string_owned(copy(s), read_only);
}

SCM make_shared_substring(SCM str, size_t start, size_t end, bool read_only) {
    auto* s = as_obj<StringObj>(str);
    return vm().heap().alloc_scm<StringObj>(s->buf, s->buf_read_only, s->start + start, end - start, read_only || s->read_only);
}

static void check_mutable_string(SCM s, const char* subr, int pos) {
    if (!is_string(s)) vm().wrong_type_arg(subr, pos, s, "string");
    auto* so = as_obj<StringObj>(s);
    if (so->read_only || so->buf_read_only) {
        vm().misc_error(subr, "string is read-only: ~S", vm().list1(s));
    }
}

static int compare_strings(const StringObj* sa, const StringObj* sb, bool ci) {
    const char* a = sa->chars(); const char* b = sb->chars();
    size_t na = sa->len, nb = sb->len;
    size_t n = std::min(na, nb);
    for (size_t i = 0; i < n; ++i) {
        unsigned char ca = static_cast<unsigned char>(a[i]);
        unsigned char cb = static_cast<unsigned char>(b[i]);
        if (ci) {
            ca = static_cast<unsigned char>(std::tolower(ca));
            cb = static_cast<unsigned char>(std::tolower(cb));
        }
        if (ca != cb) return (ca < cb) ? -1 : 1;
    }
    if (na != nb) return (na < nb) ? -1 : 1;
    return 0;
}

void VM::init_strings() {
    define_proc("string?", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return SCM::from_bool(is_string(args[0]));
    });

    define_proc("string-null?", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("string-null?", 1, args[0], "string");
        return SCM::from_bool(as_obj<StringObj>(args[0])->len == 0);
    });

    define_proc("make-string", 1, 1, false, [this](const SCM* args, size_t nargs) -> SCM {
        int64_t k = scm_to_int64(args[0], "make-string", 1);
        if (k < 0) out_of_range("make-string", args[0]);
        char fill = ' ';
        if (nargs >= 2 && !args[1].is_undefined()) {
            if (!args[1].is_char()) wrong_type_arg("make-string", 2, args[1], "char");
            fill = static_cast<char>(args[1].to_char());
        }
        return make_string_owned(string(fill, static_cast<int>(k)));
    });

    define_proc("string", 0, 0, true, [this](const SCM* args, size_t nargs) -> SCM {
        string res;
        SCM lst = nargs > 0 ? args[0] : SCM_EOL;
        while (is_pair(lst)) {
            SCM c = car(lst);
            if (!c.is_char()) wrong_type_arg("string", 1, c, "char");
            (res << static_cast<char>(c.to_char()));
            lst = cdr(lst);
        }
        return make_string_owned(res);
    });

    define_proc("list->string", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (scm_ilength(args[0]) < 0) wrong_type_arg("list->string", 1, args[0], "proper list");
        string res;
        SCM lst = args[0];
        while (is_pair(lst)) {
            SCM c = car(lst);
            if (!c.is_char()) wrong_type_arg("list->string", 1, c, "char");
            (res << static_cast<char>(c.to_char()));
            lst = cdr(lst);
        }
        return make_string_owned(res);
    });

    define_proc("string->list", 1, 2, false, [this](const SCM* args, size_t nargs) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("string->list", 1, args[0], "string");
        StringObj* sv_o = as_obj<StringObj>(args[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len;
        int64_t start = (nargs >= 2 && !args[1].is_undefined()) ? scm_to_int64(args[1], "string->list", 2) : 0;
        int64_t end = (nargs >= 3 && !args[2].is_undefined()) ? scm_to_int64(args[2], "string->list", 3) : static_cast<int64_t>(sv_n);
        if (start < 0 || end < start || static_cast<size_t>(end) > sv_n) out_of_range("string->list", args[1]);
        SCM res = SCM_EOL;
        for (int64_t i = end; i > start; --i) {
            res = cons(SCM::from_char(static_cast<unsigned char>(sv[static_cast<size_t>(i - 1)])), res);
        }
        return res;
    });

    define_proc("string-length", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("string-length", 1, args[0], "string");
        return make_integer(static_cast<int64_t>(as_obj<StringObj>(args[0])->len));
    });

    define_proc("string-ref", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("string-ref", 1, args[0], "string");
        int64_t k = scm_to_int64(args[1], "string-ref", 2);
        StringObj* sv_o = as_obj<StringObj>(args[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len;
        if (k < 0 || static_cast<size_t>(k) >= sv_n) out_of_range("string-ref", args[1]);
        return SCM::from_char(static_cast<unsigned char>(sv[static_cast<size_t>(k)]));
    });

    define_proc("string-set!", 3, 0, false, [this](const SCM* args, size_t) -> SCM {
        check_mutable_string(args[0], "string-set!", 1);
        int64_t k = scm_to_int64(args[1], "string-set!", 2);
        if (!args[2].is_char()) wrong_type_arg("string-set!", 3, args[2], "char");
        auto* so = as_obj<StringObj>(args[0]);
        if (k < 0 || static_cast<size_t>(k) >= so->len) out_of_range("string-set!", args[1]);
        so->mutable_data()[k] = static_cast<char>(args[2].to_char());
        return SCM_UNSPECIFIED;
    });

    auto substring_impl = [this](const char* name, bool shared, bool ro) {
        define_proc(name, 2, 1, false, [this, name, shared, ro](const SCM* args, size_t nargs) -> SCM {
            if (!is_string(args[0])) wrong_type_arg(name, 1, args[0], "string");
            auto* so = as_obj<StringObj>(args[0]);
            int64_t start = scm_to_int64(args[1], name, 2);
            int64_t end = (nargs >= 3 && !args[2].is_undefined()) ? scm_to_int64(args[2], name, 3) : static_cast<int64_t>(so->len);
            if (start < 0 || end < start || static_cast<size_t>(end) > so->len) out_of_range(name, args[1]);
            if (shared) {
                if (!ro && start == 0 && static_cast<size_t>(end) == so->len) return args[0];
                return make_shared_substring(args[0], static_cast<size_t>(start), static_cast<size_t>(end), ro);
            }
            return make_string_owned(so->sub(static_cast<size_t>(start), static_cast<size_t>(end - start)), ro);
        });
    };
    substring_impl("substring", false, false);
    substring_impl("substring/copy", false, false);
    substring_impl("substring/shared", true, false);
    substring_impl("substring/read-only", true, true);
    substring_impl("make-shared-substring", true, false);

    define_proc("string-copy", 1, 2, false, [this](const SCM* args, size_t nargs) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("string-copy", 1, args[0], "string");
        StringObj* sv_o = as_obj<StringObj>(args[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len;
        int64_t start = (nargs >= 2 && !args[1].is_undefined()) ? scm_to_int64(args[1], "string-copy", 2) : 0;
        int64_t end = (nargs >= 3 && !args[2].is_undefined()) ? scm_to_int64(args[2], "string-copy", 3) : static_cast<int64_t>(sv_n);
        if (start < 0 || end < start || static_cast<size_t>(end) > sv_n) out_of_range("string-copy", args[1]);
        return make_string(string(view_sub(sv, sv_n, static_cast<size_t>(start), static_cast<size_t>(end - start))));
    });

    define_proc("string-append", 0, 0, true, [this](const SCM* args, size_t nargs) -> SCM {
        string res;
        SCM lst = nargs > 0 ? args[0] : SCM_EOL;
        while (is_pair(lst)) {
            SCM s = car(lst);
            if (!is_string(s)) wrong_type_arg("string-append", 1, s, "string");
            str_append(res, as_obj<StringObj>(s)->chars(), as_obj<StringObj>(s)->len);
            lst = cdr(lst);
        }
        return make_string_owned(res);
    });

    define_proc("string-append/shared", 0, 0, true, [this](const SCM* args, size_t nargs) -> SCM {
        string res;
        SCM lst = nargs > 0 ? args[0] : SCM_EOL;
        while (is_pair(lst)) {
            SCM s = car(lst);
            if (!is_string(s)) wrong_type_arg("string-append/shared", 1, s, "string");
            str_append(res, as_obj<StringObj>(s)->chars(), as_obj<StringObj>(s)->len);
            lst = cdr(lst);
        }
        return make_string_owned(res);
    });

    define_proc("string-fill!", 2, 2, false, [this](const SCM* args, size_t nargs) -> SCM {
        check_mutable_string(args[0], "string-fill!", 1);
        if (!args[1].is_char()) wrong_type_arg("string-fill!", 2, args[1], "char");
        auto* so = as_obj<StringObj>(args[0]);
        int64_t start = (nargs >= 3 && !args[2].is_undefined()) ? scm_to_int64(args[2], "string-fill!", 3) : 0;
        int64_t end = (nargs >= 4 && !args[3].is_undefined()) ? scm_to_int64(args[3], "string-fill!", 4) : static_cast<int64_t>(so->len);
        if (start < 0 || end < start || static_cast<size_t>(end) > so->len) out_of_range("string-fill!", args[2]);
        std::memset(so->mutable_data() + start, static_cast<unsigned char>(args[1].to_char()), static_cast<size_t>(end - start));
        return SCM_UNSPECIFIED;
    });

    define_proc("substring-move!", 5, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("substring-move!", 1, args[0], "string");
        check_mutable_string(args[3], "substring-move!", 4);
        auto* s1 = as_obj<StringObj>(args[0]);
        auto* s2 = as_obj<StringObj>(args[3]);
        int64_t st1 = scm_to_int64(args[1], "substring-move!", 2);
        int64_t en1 = scm_to_int64(args[2], "substring-move!", 3);
        int64_t st2 = scm_to_int64(args[4], "substring-move!", 5);
        if (st1 < 0 || en1 < st1 || static_cast<size_t>(en1) > s1->len ||
            st2 < 0 || static_cast<size_t>(st2 + (en1 - st1)) > s2->len) {
            out_of_range("substring-move!", args[1]);
        }
        std::memmove(s2->mutable_data() + st2, s1->chars() + st1, static_cast<size_t>(en1 - st1));
        return SCM_UNSPECIFIED;
    });

    // Comparisons
    auto make_str_cmp = [this](const char* name, bool ci, auto pred) {
        define_proc(name, 2, 0, false, [this, name, ci, pred](const SCM* args, size_t) -> SCM {
            if (!is_string(args[0])) wrong_type_arg(name, 1, args[0], "string");
            if (!is_string(args[1])) wrong_type_arg(name, 2, args[1], "string");
            int c = compare_strings(as_obj<StringObj>(args[0]), as_obj<StringObj>(args[1]), ci);
            return SCM::from_bool(pred(c));
        });
    };
    make_str_cmp("string=?",     false, [](int c) { return c == 0; });
    make_str_cmp("string<?",     false, [](int c) { return c < 0; });
    make_str_cmp("string<=?",    false, [](int c) { return c <= 0; });
    make_str_cmp("string>?",     false, [](int c) { return c > 0; });
    make_str_cmp("string>=?",    false, [](int c) { return c >= 0; });
    make_str_cmp("string-ci=?",  true,  [](int c) { return c == 0; });
    make_str_cmp("string-ci<?",  true,  [](int c) { return c < 0; });
    make_str_cmp("string-ci<=?", true,  [](int c) { return c <= 0; });
    make_str_cmp("string-ci>?",  true,  [](int c) { return c > 0; });
    make_str_cmp("string-ci>=?", true,  [](int c) { return c >= 0; });

    define_proc("string-split", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("string-split", 1, args[0], "string");
        if (!args[1].is_char()) wrong_type_arg("string-split", 2, args[1], "char");
        StringObj* sv_o = as_obj<StringObj>(args[0]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len;
        char sep = static_cast<char>(args[1].to_char());
        SCM res = SCM_EOL;
        size_t end = sv_n;
        for (size_t i = sv_n; i > 0; --i) {
            if (sv[i - 1] == sep) {
                res = cons(make_string(view_sub(sv, sv_n, i, end - i)), res);
                end = i - 1;
            }
        }
        res = cons(make_string(view_sub(sv, sv_n, 0, end)), res);
        return res;
    });

    define_proc("string->number", 1, 1, false, [this](const SCM* args, size_t nargs) -> SCM {
        if (!is_string(args[0])) wrong_type_arg("string->number", 1, args[0], "string");
        int radix = 10;
        if (nargs >= 2 && !args[1].is_undefined()) {
            int64_t r = scm_to_int64(args[1], "string->number", 2);
            if (r < 2 || r > 36) out_of_range("string->number", args[1]);
            radix = static_cast<int>(r);
        }
        return scm_string_to_number(as_obj<StringObj>(args[0])->str(), radix);
    });

    define_proc("number->string", 1, 1, false, [this](const SCM* args, size_t nargs) -> SCM {
        if (!is_number(args[0])) wrong_type_arg("number->string", 1, args[0], "number");
        int radix = 10;
        if (nargs >= 2 && !args[1].is_undefined()) {
            int64_t r = scm_to_int64(args[1], "number->string", 2);
            if (r < 2 || r > 36) out_of_range("number->string", args[1]);
            radix = static_cast<int>(r);
        }
        return make_string(scm_number_to_string(args[0], radix));
    });
}

} // namespace guile
