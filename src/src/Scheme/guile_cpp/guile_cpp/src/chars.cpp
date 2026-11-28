#include "guile_cpp/vm.hpp"
#include <cctype>
#include <cstring>

namespace guile {

static const char* const kAsciiNames[33] = {
    "nul", "soh", "stx", "etx", "eot", "enq", "ack", "bel",
    "bs",  "ht",  "nl",  "vt",  "np",  "cr",  "so",  "si",
    "dle", "dc1", "dc2", "dc3", "dc4", "nak", "syn", "etb",
    "can", "em",  "sub", "esc", "fs",  "gs",  "rs",  "us",
    "sp"
};

const char* char_to_name(uint32_t c) {
    if (c == '\n') return "newline";
    if (c == ' ')  return "space";
    if (c == '\t') return "tab";
    if (c == '\b') return "backspace";
    if (c == '\r') return "return";
    if (c == '\f') return "page";
    if (c == '\a') return "alarm";
    if (c == '\v') return "vtab";
    if (c == 0)    return "nul";
    if (c == 127)  return "del";
    if (c < 33)    return kAsciiNames[c];
    return nullptr;
}

static bool iequals(std::string_view a, std::string_view b) {
    if (a.size() != b.size()) return false;
    for (size_t i = 0; i < a.size(); ++i) {
        if (std::tolower(static_cast<unsigned char>(a[i])) !=
            std::tolower(static_cast<unsigned char>(b[i]))) return false;
    }
    return true;
}

int name_to_char(std::string_view name) {
    if (iequals(name, "space") || iequals(name, "sp")) return ' ';
    if (iequals(name, "newline") || iequals(name, "nl") || iequals(name, "lf")) return '\n';
    if (iequals(name, "tab") || iequals(name, "ht")) return '\t';
    if (iequals(name, "backspace") || iequals(name, "bs")) return '\b';
    if (iequals(name, "return") || iequals(name, "cr")) return '\r';
    if (iequals(name, "page") || iequals(name, "np") || iequals(name, "ff")) return '\f';
    if (iequals(name, "alarm") || iequals(name, "bel")) return '\a';
    if (iequals(name, "vtab") || iequals(name, "vt")) return '\v';
    if (iequals(name, "null") || iequals(name, "nul")) return 0;
    if (iequals(name, "del") || iequals(name, "delete")) return 127;
    if (iequals(name, "esc") || iequals(name, "escape")) return 27;
    for (int i = 0; i < 33; ++i) {
        if (iequals(name, kAsciiNames[i])) return i;
    }
    // Check octal digits (Guile 1.8 supports #\000 .. #\377)
    if (!name.empty()) {
        bool all_oct = true;
        int val = 0;
        for (char c : name) {
            if (c < '0' || c > '7') { all_oct = false; break; }
            val = val * 8 + (c - '0');
        }
        if (all_oct && val <= 255) return val;
    }
    return -1;
}

void VM::init_chars() {
    define("char-code-limit", make_integer(256));

    define_proc("char?", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return SCM::from_bool(args[0].is_char());
    });

    auto make_char_cmp = [this](const char* name, bool ci, auto cmp) {
        define_proc(name, 2, 0, true, [this, name, ci, cmp](const SCM* args, size_t nargs) -> SCM {
            if (!args[0].is_char()) wrong_type_arg(name, 1, args[0], "char");
            if (!args[1].is_char()) wrong_type_arg(name, 2, args[1], "char");
            auto norm = [ci](uint32_t c) -> uint32_t {
                return ci ? static_cast<uint32_t>(std::tolower(static_cast<unsigned char>(c))) : c;
            };
            uint32_t prev = norm(args[0].to_char());
            uint32_t cur = norm(args[1].to_char());
            if (!cmp(prev, cur)) return SCM_BOOL_F;
            prev = cur;
            SCM rest = nargs > 2 ? args[2] : SCM_EOL;
            int pos = 3;
            while (is_pair(rest)) {
                SCM x = car(rest);
                if (!x.is_char()) wrong_type_arg(name, pos, x, "char");
                cur = norm(x.to_char());
                if (!cmp(prev, cur)) return SCM_BOOL_F;
                prev = cur;
                rest = cdr(rest);
                ++pos;
            }
            return SCM_BOOL_T;
        });
    };

    make_char_cmp("char=?",    false, [](uint32_t a, uint32_t b) { return a == b; });
    make_char_cmp("char<?",    false, [](uint32_t a, uint32_t b) { return a < b; });
    make_char_cmp("char<=?",   false, [](uint32_t a, uint32_t b) { return a <= b; });
    make_char_cmp("char>?",    false, [](uint32_t a, uint32_t b) { return a > b; });
    make_char_cmp("char>=?",   false, [](uint32_t a, uint32_t b) { return a >= b; });
    make_char_cmp("char-ci=?", true,  [](uint32_t a, uint32_t b) { return a == b; });
    make_char_cmp("char-ci<?", true,  [](uint32_t a, uint32_t b) { return a < b; });
    make_char_cmp("char-ci<=?",true,  [](uint32_t a, uint32_t b) { return a <= b; });
    make_char_cmp("char-ci>?", true,  [](uint32_t a, uint32_t b) { return a > b; });
    make_char_cmp("char-ci>=?",true,  [](uint32_t a, uint32_t b) { return a >= b; });

    auto make_char_pred = [this](const char* name, auto pred) {
        define_proc(name, 1, 0, false, [this, name, pred](const SCM* args, size_t) -> SCM {
            if (!args[0].is_char()) wrong_type_arg(name, 1, args[0], "char");
            return SCM::from_bool(pred(static_cast<unsigned char>(args[0].to_char())));
        });
    };
    make_char_pred("char-alphabetic?", [](unsigned char c) { return std::isalpha(c) != 0; });
    make_char_pred("char-numeric?",    [](unsigned char c) { return std::isdigit(c) != 0; });
    make_char_pred("char-whitespace?", [](unsigned char c) { return std::isspace(c) != 0; });
    make_char_pred("char-upper-case?", [](unsigned char c) { return std::isupper(c) != 0; });
    make_char_pred("char-lower-case?", [](unsigned char c) { return std::islower(c) != 0; });
    make_char_pred("char-is-both?",    [](unsigned char c) { return std::isalpha(c) != 0; });

    define_proc("char->integer", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!args[0].is_char()) wrong_type_arg("char->integer", 1, args[0], "char");
        return make_integer(static_cast<int64_t>(args[0].to_char()));
    });
    define_proc("integer->char", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        int64_t n = scm_to_int64(args[0], "integer->char", 1);
        if (n < 0 || n > 255) out_of_range("integer->char", args[0]);
        return SCM::from_char(static_cast<uint32_t>(n));
    });
    define_proc("char-upcase", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!args[0].is_char()) wrong_type_arg("char-upcase", 1, args[0], "char");
        return SCM::from_char(static_cast<unsigned char>(std::toupper(static_cast<unsigned char>(args[0].to_char()))));
    });
    define_proc("char-downcase", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!args[0].is_char()) wrong_type_arg("char-downcase", 1, args[0], "char");
        return SCM::from_char(static_cast<unsigned char>(std::tolower(static_cast<unsigned char>(args[0].to_char()))));
    });
}

} // namespace guile
