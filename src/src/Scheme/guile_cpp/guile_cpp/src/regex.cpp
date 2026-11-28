#include "guile_cpp/vm.hpp"
#include <regex.h>

namespace guile {

void VM::init_regex() {
    define("regexp/basic",    make_integer(REG_BASIC));
    define("regexp/extended", make_integer(REG_EXTENDED));
    define("regexp/icase",    make_integer(REG_ICASE));
    define("regexp/newline",  make_integer(REG_NEWLINE));
    define("regexp/notbol",   make_integer(REG_NOTBOL));
    define("regexp/noteol",   make_integer(REG_NOTEOL));

    define_proc("regexp?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_regexp(a[0]));
    });

    define_proc("make-regexp", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("make-regexp", 1, a[0], "string");
        int flags = REG_EXTENDED;
        SCM rest = nargs > 1 ? a[1] : SCM_EOL;
        if (is_pair(rest)) {
            flags = 0;
            while (is_pair(rest)) {
                flags |= static_cast<int>(scm_to_int64(car(rest), "make-regexp", 2));
                rest = cdr(rest);
            }
        }
        auto* rx = heap_.alloc<RegexpObj>();
        StringObj* psv_o = as_obj<StringObj>(a[0]); const char* psv = psv_o->chars(); size_t psv_n = psv_o->len;
        if (view_find(psv, psv_n, '\0') != static_cast<size_t>(-1)) {
            misc_error("make-regexp", "string contains #\\nul character: ~S", list1(a[0]));
        }
        rx->pattern = string(psv);
        rx->flags = flags;
        int rc = ::regcomp(&rx->rx, ((char*) c_string(rx->pattern)), flags);
        if (rc != 0) {
            char errbuf[256];
            ::regerror(rc, &rx->rx, errbuf, sizeof(errbuf));
            error(make_symbol("regular-expression-syntax"), "make-regexp", errbuf, SCM_EOL);
        }
        rx->compiled = true;
        return SCM::from_obj(rx);
    });

    define_proc("regexp-exec", 2, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_regexp(a[0])) wrong_type_arg("regexp-exec", 1, a[0], "regexp");
        if (!is_string(a[1])) wrong_type_arg("regexp-exec", 2, a[1], "string");
        auto* rx = as_obj<RegexpObj>(a[0]);
        StringObj* sv_o = as_obj<StringObj>(a[1]); const char* sv = sv_o->chars(); size_t sv_n = sv_o->len;
        if (view_find(sv, sv_n, '\0') != static_cast<size_t>(-1)) {
            misc_error("regexp-exec", "string contains #\\nul character: ~S", list1(a[1]));
        }
        int64_t start = (nargs >= 3 && !a[2].is_undefined()) ? scm_to_int64(a[2], "regexp-exec", 3) : 0;
        int eflags = (nargs >= 4 && !a[3].is_undefined()) ? static_cast<int>(scm_to_int64(a[3], "regexp-exec", 4)) : 0;
        if (start < 0 || static_cast<size_t>(start) > sv_n) out_of_range("regexp-exec", a[2]);

        string sub(view_sub(sv, sv_n, static_cast<size_t>(start)));
        size_t nmatch = rx->rx.re_nsub + 1;
        array<regmatch_t> matches(nmatch);
        int rc = ::regexec(&rx->rx, ((char*) c_string(sub)), nmatch, gcpp_data(matches), eflags);
        if (rc == REG_NOMATCH) return SCM_BOOL_F;
        if (rc != 0) {
            char errbuf[256];
            ::regerror(rc, &rx->rx, errbuf, sizeof(errbuf));
            error(make_symbol("regular-expression-syntax"), "regexp-exec", errbuf, SCM_EOL);
        }
        // Guile 1.8.8 match vector: #(original-string (start0 . end0) (start1 . end1) ...)
        auto* vec = heap_.alloc<VectorObj>(nmatch + 1, SCM_BOOL_F);
        vec->elts[0] = a[1];
        for (size_t i = 0; i < nmatch; ++i) {
            if (matches[i].rm_so >= 0) {
                vec->elts[i + 1] = cons(make_integer(start + matches[i].rm_so),
                                        make_integer(start + matches[i].rm_eo));
            } else {
                vec->elts[i + 1] = cons(make_integer(-1), make_integer(-1));
            }
        }
        return SCM::from_obj(vec);
    });
}

} // namespace guile
