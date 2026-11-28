#include "guile_cpp/vm.hpp"
#include <cctype>
#include <cstring>

namespace guile {

namespace {

bool is_delimiter(int c) {
    return c == EOF || std::isspace(static_cast<unsigned char>(c)) ||
           c == '(' || c == ')' ||
           c == '"' || c == ';';
}

[[noreturn]] void read_error(PortObj* p, const std::string& msg, SCM args = SCM_EOL) {
    std::string loc = p->filename.empty() ? "unknown file" : p->filename;
    std::string full = loc + ":" + std::to_string(p->line + 1) + ":" + std::to_string(p->column + 1) + ": " + msg;
    vm().error(vm().sym_read_error, "scm_lreadr", full, args, SCM_BOOL_F);
}

inline SCM port_filename_scm(PortObj* p) {
    if (p->filename.empty()) return SCM_BOOL_F;
    if (p->filename_scm.is_false()) {
        p->filename_scm = vm().make_string(p->filename);
    }
    return p->filename_scm;
}

void skip_whitespace_and_comments(PortObj* p) {
    while (true) {
        int c = port_getc_fast(p);
        if (c == EOF) return;
        if (c == ' ' || c == '\n' || c == '\t' || c == '\r' || c == '\f' || c == '\v') continue;
        if (c == ';') {
            while ((c = port_getc_fast(p)) != EOF && c != '\n') {}
            continue;
        }
        if (c == '#') {
            int next = port_peekc(p);
            if (next == '!') {
                port_getc(p); // consume '!'
                // Guile #! ... !# block comment
                while (true) {
                    int b = port_getc(p);
                    if (b == EOF) read_error(p, "unterminated `#! ... !#' comment");
                    if (b == '!' && port_peekc(p) == '#') {
                        port_getc(p);
                        break;
                    }
                }
                continue;
            }
            if (next == '|') {
                // R6RS/SRFI-30 nested #| ... |# block comment
                port_getc(p);
                int depth = 1;
                while (depth > 0) {
                    int b = port_getc(p);
                    if (b == EOF) read_error(p, "unterminated `#| ... |#' comment");
                    if (b == '#' && port_peekc(p) == '|') { port_getc(p); ++depth; }
                    else if (b == '|' && port_peekc(p) == '#') { port_getc(p); --depth; }
                }
                continue;
            }
            port_ungetc_fast(p, c);
            return;
        }
        port_ungetc_fast(p, c);
        return;
    }
}

std::string read_token(PortObj* p) {
    std::string tok;
    tok.reserve(24);
    while (true) {
        int c = port_getc_fast(p);
        if (c == EOF) break;
        if (is_delimiter(c)) {
            port_ungetc_fast(p, c);
            break;
        }
        tok.push_back(static_cast<char>(c));
    }
    return tok;
}

SCM read_expr(PortObj* p, SCM port_scm);

SCM read_string(PortObj* p) {
    std::string s;
    s.reserve(32);
    while (true) {
        int c = port_getc_fast(p);
        if (c == EOF) read_error(p, "end of file in string constant");
        if (c == '"') break;
        if (c == '\\') {
            int esc = port_getc_fast(p);
            if (esc == EOF) read_error(p, "end of file in string escape");
            switch (esc) {
            case 'n': s.push_back('\n'); break;
            case 't': s.push_back('\t'); break;
            case 'r': s.push_back('\r'); break;
            case 'f': s.push_back('\f'); break;
            case 'b': s.push_back('\b'); break;
            case 'a': s.push_back('\a'); break;
            case 'v': s.push_back('\v'); break;
            case '\\': s.push_back('\\'); break;
            case '"': s.push_back('"'); break;
            case '\n': break; // line continuation
            case '0': case '1': case '2': case '3':
            case '4': case '5': case '6': case '7': {
                int val = esc - '0';
                for (int k = 0; k < 2; ++k) {
                    int nx = port_peekc(p);
                    if (nx >= '0' && nx <= '7') {
                        port_getc(p);
                        val = (val << 3) | (nx - '0');
                    } else break;
                }
                s.push_back(static_cast<char>(val & 0xFF));
                break;
            }
            case 'x': {
                int h1 = port_getc(p);
                int h2 = port_getc(p);
                auto hex = [](int ch) -> int {
                    if (ch >= '0' && ch <= '9') return ch - '0';
                    if (ch >= 'a' && ch <= 'f') return ch - 'a' + 10;
                    if (ch >= 'A' && ch <= 'F') return ch - 'A' + 10;
                    return -1;
                };
                int d1 = hex(h1), d2 = hex(h2);
                if (d1 < 0 || d2 < 0) read_error(p, "illegal character in escape sequence: ~S", vm().list1(SCM::from_char(static_cast<uint32_t>(d1 < 0 ? h1 : h2))));
                s.push_back(static_cast<char>((d1 << 4) | d2));
                break;
            }
            default:
                read_error(p, "illegal character in escape sequence: ~S", vm().list1(SCM::from_char(static_cast<uint32_t>(esc))));
            }
        } else {
            s.push_back(static_cast<char>(c));
        }
    }
    return vm().make_string(std::move(s));
}

SCM read_list(PortObj* p, SCM port_scm, char close_ch, long start_line, long start_col) {
    skip_whitespace_and_comments(p);
    int c = port_peekc_fast(p);
    if (c == EOF) read_error(p, "end of file");
    if (c == close_ch) {
        port_getc_fast(p);
        return SCM_EOL;
    }

    SCM head = SCM_EOL;
    SCM tail = SCM_EOL;

    while (true) {
        skip_whitespace_and_comments(p);
        c = port_peekc_fast(p);
        if (c == EOF) read_error(p, "end of file");
        if (c == close_ch) {
            port_getc_fast(p);
            break;
        }
        // Check for dotted pair '.'
        if (c == '.') {
            port_getc_fast(p);
            int next = port_peekc_fast(p);
            if (is_delimiter(next)) {
                skip_whitespace_and_comments(p);
                SCM last = read_expr(p, port_scm);
                if (last.is_eof()) read_error(p, "end of file");
                skip_whitespace_and_comments(p);
                int endc = port_getc_fast(p);
                if (endc != close_ch) read_error(p, "missing close paren after dotted cdr");
                if (head.is_null()) {
                    // Guile 1.8 extension: (. x) reads as x!
                    return last;
                }
                set_cdr(tail, last);
                break;
            } else {
                port_ungetc_fast(p, '.');
            }
        }

        SCM elem = read_expr(p, port_scm);
        if (elem.is_eof()) read_error(p, "end of file");
        SCM cell = vm().cons(elem, SCM_EOL);
        if (head.is_null()) {
            head = tail = cell;
            if (vm().read_opts.positions) {
                vm().set_source_position(head, port_filename_scm(p),
                                         static_cast<int32_t>(start_line),
                                         static_cast<int32_t>(start_col));
            }
        } else {
            set_cdr(tail, cell);
            tail = cell;
        }
    }
    return head;
}

SCM read_char_literal(PortObj* p) {
    int c = port_getc(p);
    if (c == EOF) read_error(p, "end of file in character literal");
    int next = port_peekc(p);
    if (is_delimiter(next)) {
        return SCM::from_char(static_cast<uint32_t>(c));
    }
    std::string name;
    name.push_back(static_cast<char>(c));
    name += read_token(p);
    int ch = name_to_char(name);
    if (ch < 0) read_error(p, "unknown character name ~A", vm().list1(vm().make_string(name)));
    return SCM::from_char(static_cast<uint32_t>(ch));
}

SCM read_sharp(PortObj* p, SCM port_scm) {
    int c = port_getc(p);
    if (c == EOF) read_error(p, "end of file");

    // Check user-installed read-hash-procedures first!
    SCM rhp = vm().read_hash_procedures;
    while (is_pair(rhp)) {
        SCM entry = car(rhp);
        if (is_pair(entry) && car(entry).is_char() && static_cast<int>(car(entry).to_char()) == c) {
            return vm().call2(cdr(entry), SCM::from_char(static_cast<uint32_t>(c)), port_scm);
        }
        rhp = cdr(rhp);
    }

    switch (c) {
    case 't': case 'T': {
        if (is_delimiter(port_peekc(p))) return SCM_BOOL_T;
        std::string rest = read_token(p);
        if (rest == "rue") return SCM_BOOL_T;
        read_error(p, "Unknown # object: ~S", vm().list1(SCM::from_char(static_cast<uint32_t>(c))));
    }
    case 'f': case 'F': {
        if (is_delimiter(port_peekc(p))) return SCM_BOOL_F;
        // Could be #f32(...) or #f64(...) SRFI-4 uniform vector!
        std::string rest = read_token(p);
        if (rest == "alse") return SCM_BOOL_F;
        if (rest == "32" && port_peekc(p) == '(') {
            long sl = p->line, sc = p->column;
            port_getc(p);
            SCM lst = read_list(p, port_scm, ')', sl, sc);
            return read_uniform_vector_body(UniformType::F32, lst);
        }
        if (rest == "64" && port_peekc(p) == '(') {
            long sl = p->line, sc = p->column;
            port_getc(p);
            SCM lst = read_list(p, port_scm, ')', sl, sc);
            return read_uniform_vector_body(UniformType::F64, lst);
        }
        read_error(p, "Unknown # object: ~S", vm().list1(SCM::from_char(static_cast<uint32_t>(c))));
    }
    case '\\':
        return read_char_literal(p);
    case '(': {
        long sl = p->line, sc = p->column;
        SCM lst = read_list(p, port_scm, ')', sl, sc);
        std::vector<SCM> elts;
        while (is_pair(lst)) { elts.push_back(car(lst)); lst = cdr(lst); }
        return vm().heap().alloc_scm<VectorObj>(std::move(elts));
    }
    case ':': {
        // Keyword #:name
        std::string tok = read_token(p);
        if (vm().read_opts.case_insensitive) {
            for (char& ch : tok) ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
        }
        return vm().make_keyword(vm().make_symbol(tok));
    }
    case ';': {
        // Datum comment: skip next datum and read the one after!
        (void)read_expr(p, port_scm);
        return read_expr(p, port_scm);
    }
    case '*': {
        // Bitvector #*010110
        std::string bits = read_token(p);
        auto* bv = vm().heap().alloc<BitVectorObj>(bits.size(), false);
        for (size_t i = 0; i < bits.size(); ++i) {
            if (bits[i] == '1') bv->set(i, true);
            else if (bits[i] != '0') read_error(p, "invalid bitvector digit");
        }
        return SCM::from_obj(bv);
    }
    case '{': {
        // Guile extended symbol syntax #{foo bar}#
        std::string sym;
        while (true) {
            int ch = port_getc(p);
            if (ch == EOF) read_error(p, "unterminated #{...}# symbol");
            if (ch == '\\') {
                int esc = port_getc(p);
                if (esc == EOF) read_error(p, "unterminated escape in #{...}#");
                sym.push_back(static_cast<char>(esc));
            } else if (ch == '}' && port_peekc(p) == '#') {
                port_getc(p);
                break;
            } else {
                sym.push_back(static_cast<char>(ch));
            }
        }
        return vm().make_symbol(sym);
    }
    case '\'':
        return vm().list2(vm().sym_syntax, read_expr(p, port_scm));
    case '`':
        return vm().list2(vm().sym_quasisyntax, read_expr(p, port_scm));
    case ',': {
        if (port_peekc(p) == '@') {
            port_getc(p);
            return vm().list2(vm().sym_unsyntax_splicing, read_expr(p, port_scm));
        }
        return vm().list2(vm().sym_unsyntax, read_expr(p, port_scm));
    }
    case 'b': case 'B':
    case 'o': case 'O':
    case 'd': case 'D':
    case 'x': case 'X':
    case 'e': case 'E':
    case 'i': case 'I': {
        std::string tok = "#";
        tok.push_back(static_cast<char>(c));
        tok += read_token(p);
        SCM num = scm_string_to_number(tok, 10);
        if (num.is_false()) read_error(p, "invalid number: " + tok);
        return num;
    }
    case 'u': case 's': case 'c': {
        // SRFI-4 uniform vector: #u8(...), #s16(...), #c32(...), etc.
        std::string tok;
        tok.push_back(static_cast<char>(c));
        tok += read_token(p);
        if (port_peekc(p) == '(') {
            long sl = p->line, sc = p->column;
            port_getc(p);
            SCM lst = read_list(p, port_scm, ')', sl, sc);
            if (tok == "u8")  return read_uniform_vector_body(UniformType::U8, lst);
            if (tok == "s8")  return read_uniform_vector_body(UniformType::S8, lst);
            if (tok == "u16") return read_uniform_vector_body(UniformType::U16, lst);
            if (tok == "s16") return read_uniform_vector_body(UniformType::S16, lst);
            if (tok == "u32") return read_uniform_vector_body(UniformType::U32, lst);
            if (tok == "s32") return read_uniform_vector_body(UniformType::S32, lst);
            if (tok == "u64") return read_uniform_vector_body(UniformType::U64, lst);
            if (tok == "s64") return read_uniform_vector_body(UniformType::S64, lst);
            if (tok == "c32") return read_uniform_vector_body(UniformType::C32, lst);
            if (tok == "c64") return read_uniform_vector_body(UniformType::C64, lst);
        }
        read_error(p, "Unknown # object: ~S", vm().list1(SCM::from_char(static_cast<uint32_t>(c))));
    }
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9': {
        // Array syntax #2((1 2) (3 4)) or #2u8(...) or #2u32@2@7((1 2) (3 4)) or #0(x)
        std::string tok;
        tok.push_back(static_cast<char>(c));
        tok += read_token(p);
        size_t pos = 0;
        while (pos < tok.size() && std::isdigit(static_cast<unsigned char>(tok[pos]))) ++pos;
        int rank = std::stoi(tok.substr(0, pos));
        std::string rest = tok.substr(pos);
        // Split optional @lbnd and :len suffixes
        std::vector<int64_t> lbnds;
        size_t spec_pos = rest.find_first_of("@:");
        std::string tag = (spec_pos == std::string::npos) ? rest : rest.substr(0, spec_pos);
        if (spec_pos != std::string::npos) {
            size_t i = spec_pos;
            while (i < rest.size() && (rest[i] == '@' || rest[i] == ':')) {
                int64_t lb = 0;
                if (rest[i] == '@') {
                    ++i;
                    size_t j = i;
                    if (j < rest.size() && (rest[j] == '+' || rest[j] == '-')) ++j;
                    while (j < rest.size() && std::isdigit(static_cast<unsigned char>(rest[j]))) ++j;
                    if (j == i) read_error(p, "invalid array lower bound");
                    lb = std::stoll(rest.substr(i, j - i));
                    i = j;
                }
                if (i < rest.size() && rest[i] == ':') {
                    ++i;
                    size_t j = i;
                    if (j < rest.size() && (rest[j] == '+' || rest[j] == '-')) ++j;
                    while (j < rest.size() && std::isdigit(static_cast<unsigned char>(rest[j]))) ++j;
                    if (j == i) read_error(p, "invalid array length");
                    int64_t len = std::stoll(rest.substr(i, j - i));
                    if (len < 0) read_error(p, "array length must be non-negative");
                    i = j;
                }
                lbnds.push_back(lb);
            }
        }
        SCM body = read_expr(p, port_scm);
        SCM type_sym = tag.empty() ? SCM_BOOL_T : vm().make_symbol(tag);
        // Delegate to list->typed-array
        SCM var = vm().lookup("list->typed-array");
        if (is_variable(var) && !as_obj<VariableObj>(var)->value.is_undefined()) {
            SCM arr = vm().call3(as_obj<VariableObj>(var)->value, type_sym, vm().make_integer(rank), body);
            if (!lbnds.empty()) {
                if (is_array(arr)) {
                    auto* ar = as_obj<ArrayObj>(arr);
                    for (size_t d = 0; d < lbnds.size() && d < ar->dims.size(); ++d) {
                        int64_t len = ar->dims[d].ubnd - ar->dims[d].lbnd + 1;
                        ar->dims[d].lbnd = lbnds[d];
                        ar->dims[d].ubnd = lbnds[d] + len - 1;
                    }
                } else if (rank == 1 && (is_vector(arr) || is_uniform_vector(arr) || is_bitvector(arr) || is_string(arr))) {
                    auto* ar = vm().heap().alloc<ArrayObj>();
                    ar->base_vec = arr;
                    ar->base_offset = 0;
                    size_t len = is_vector(arr) ? as_obj<VectorObj>(arr)->elts.size()
                               : is_uniform_vector(arr) ? as_obj<UniformVectorObj>(arr)->length
                               : is_bitvector(arr) ? as_obj<BitVectorObj>(arr)->length
                               : as_obj<StringObj>(arr)->len;
                    ar->dims.push_back({lbnds[0], lbnds[0] + static_cast<int64_t>(len) - 1, 1});
                    arr = SCM::from_obj(ar);
                }
            }
            return arr;
        }
        read_error(p, "array reader not initialized");
    }
    default:
        read_error(p, "Unknown # object: ~S", vm().list1(SCM::from_char(static_cast<uint32_t>(c))));
    }
}

SCM read_expr(PortObj* p, SCM port_scm) {
    skip_whitespace_and_comments(p);
    long start_line = p->line;
    long start_col = p->column;
    int c = port_getc_fast(p);
    if (c == EOF) return SCM_EOF;

    auto attach_pos = [&](SCM pair_obj) -> SCM {
        if (vm().read_opts.positions && is_pair(pair_obj)) {
            vm().set_source_position(pair_obj, port_filename_scm(p),
                                     static_cast<int32_t>(start_line),
                                     static_cast<int32_t>(start_col));
        }
        return pair_obj;
    };

    switch (c) {
    case '(':
        return read_list(p, port_scm, ')', start_line, start_col);
    case ')':
        read_error(p, "unexpected \")\"");
    case '"':
        return read_string(p);
    case '\'': {
        SCM d = read_expr(p, port_scm);
        if (d.is_eof()) read_error(p, "end of file");
        return attach_pos(vm().list2(vm().sym_quote, d));
    }
    case '`': {
        SCM d = read_expr(p, port_scm);
        if (d.is_eof()) read_error(p, "end of file");
        return attach_pos(vm().list2(vm().sym_quasiquote, d));
    }
    case ',': {
        if (port_peekc(p) == '@') {
            port_getc(p);
            SCM d = read_expr(p, port_scm);
            if (d.is_eof()) read_error(p, "end of file");
            return attach_pos(vm().list2(vm().sym_unquote_splicing, d));
        }
        SCM d = read_expr(p, port_scm);
        if (d.is_eof()) read_error(p, "end of file");
        return attach_pos(vm().list2(vm().sym_unquote, d));
    }
    case '#':
        return read_sharp(p, port_scm);
    default: {
        std::string tok;
        tok.push_back(static_cast<char>(c));
        tok += read_token(p);

        // Try parsing as number first
        SCM num = scm_string_to_number(tok, 10);
        if (num.is_true()) return num;

        // Check keyword read option ('prefix or 'postfix)
        if (vm().read_opts.keywords == vm().sym_prefix && tok.size() > 1 && tok[0] == ':') {
            std::string kw = tok.substr(1);
            if (vm().read_opts.case_insensitive) {
                for (char& ch : kw) ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
            }
            return vm().make_keyword(vm().make_symbol(kw));
        }
        if (vm().read_opts.keywords == vm().sym_postfix && tok.size() > 1 && tok.back() == ':') {
            std::string kw = tok.substr(0, tok.size() - 1);
            if (vm().read_opts.case_insensitive) {
                for (char& ch : kw) ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
            }
            return vm().make_keyword(vm().make_symbol(kw));
        }

        // Symbol
        if (vm().read_opts.case_insensitive) {
            for (char& ch : tok) ch = static_cast<char>(std::tolower(static_cast<unsigned char>(ch)));
        }
        return vm().make_symbol(tok);
    }
    }
}

} // namespace

SCM VM::read_from_port(SCM port) {
    if (!is_port(port)) wrong_type_arg("read", 1, port, "input port");
    return read_expr(as_obj<PortObj>(port), port);
}

SCM VM::read_from_string(std::string_view s) {
    SCM p = make_input_string_port(std::string(s));
    return read_from_port(p);
}

void VM::init_read() {
    // Enable prefix keywords (:foo) by default? Wait: in Guile 1.8.8, let's check:
    // Actually in Guile 1.8.8, `read-options` default for `keywords` is `#f`, EXCEPT
    // `ice-9/boot-9.scm` uses `#:foo` AND `:use-module`! Wait: does boot-9 enable prefix keywords?
    // Let's check! `test-suite/lib.scm` uses `(define-module (test-suite lib) :use-module ...)`!
    // In Guile 1.8.8, `:use-module` is a SYMBOL in `define-module` unless keywords='prefix is set!
    // boot-9.scm handles BOTH `#:use-module` (keyword) AND `:use-module` (symbol)!
    // So keeping `read_opts.keywords = SCM_BOOL_F` matches Guile 1.8.8's exact default!

    define_proc("read", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM port = (nargs >= 1 && !a[0].is_undefined()) ? a[0] : current_input_port();
        return read_from_port(port);
    });

    define_proc("read-hash-extend", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!a[0].is_char()) wrong_type_arg("read-hash-extend", 1, a[0], "char");
        // Remove existing entry for char a[0]
        SCM cur = read_hash_procedures, prev = SCM_EOL;
        while (is_pair(cur)) {
            if (car(car(cur)) == a[0]) {
                if (prev.is_null()) read_hash_procedures = cdr(cur);
                else set_cdr(prev, cdr(cur));
                break;
            }
            prev = cur;
            cur = cdr(cur);
        }
        if (a[1].is_true()) {
            read_hash_procedures = cons(cons(a[0], a[1]), read_hash_procedures);
        }
        return SCM_UNSPECIFIED;
    });

    define_proc("read-hash-procedure", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!a[0].is_char()) wrong_type_arg("read-hash-procedure", 1, a[0], "char");
        SCM cur = read_hash_procedures;
        while (is_pair(cur)) {
            if (car(car(cur)) == a[0]) return cdr(car(cur));
            cur = cdr(cur);
        }
        return SCM_BOOL_F;
    });
}

} // namespace guile
