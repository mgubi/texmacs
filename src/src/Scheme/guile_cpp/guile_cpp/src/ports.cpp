#include "guile_cpp/vm.hpp"
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <cerrno>
#include <cstring>
#include <cstdio>
#include <algorithm>

namespace guile {

PortObj::~PortObj() {
    if (!closed) {
        if (kind == PortKind::Pipe && pipe_fp) {
            pclose(pipe_fp);
            pipe_fp = nullptr;
        } else if (kind == PortKind::File && owns_fd && fd >= 3) {
            if (is_output && !write_buf.empty()) {
                (void)::write(fd, write_buf.data(), write_buf.size());
                write_buf.clear();
            }
            ::close(fd);
            fd = -1;
        }
        closed = true;
    }
}

void port_flush(PortObj* p) {
    if (!p || p->closed || !p->is_output) return;
    if (p->kind == PortKind::File) {
        if (!p->write_buf.empty() && p->fd >= 0) {
            size_t off = 0;
            while (off < p->write_buf.size()) {
                ssize_t n = ::write(p->fd, p->write_buf.data() + off, p->write_buf.size() - off);
                if (n < 0) {
                    if (errno == EINTR) continue;
                    vm().syserror("force-output", errno);
                }
                off += static_cast<size_t>(n);
            }
            p->write_buf.clear();
        }
    } else if (p->kind == PortKind::Pipe) {
        if (p->pipe_fp) {
            if (!p->write_buf.empty()) {
                std::fwrite(p->write_buf.data(), 1, p->write_buf.size(), p->pipe_fp);
                p->write_buf.clear();
            }
            std::fflush(p->pipe_fp);
        }
    } else if (p->kind == PortKind::Soft) {
        if (is_vector(p->soft_vec)) {
            auto* v = as_obj<VectorObj>(p->soft_vec);
            if (v->elts.size() >= 3 && v->elts[2].is_true()) {
                vm().call0(v->elts[2]);
            }
        }
    }
}

void port_close(PortObj* p) {
    if (!p || p->closed) return;
    if (p->is_output) {
        try { port_flush(p); } catch (...) {}
    }
    if (p->kind == PortKind::File) {
        if (p->owns_fd && p->fd >= 0) {
            ::close(p->fd);
        }
        p->fd = -1;
    } else if (p->kind == PortKind::Pipe) {
        if (p->pipe_fp) {
            pclose(p->pipe_fp);
            p->pipe_fp = nullptr;
        }
    } else if (p->kind == PortKind::Soft) {
        if (is_vector(p->soft_vec)) {
            auto* v = as_obj<VectorObj>(p->soft_vec);
            if (v->elts.size() >= 5 && v->elts[4].is_true()) {
                vm().call0(v->elts[4]);
            }
        }
    }
    p->closed = true;
}

static void advance_port_pos(PortObj* p, int c) {
    if (c == '\n') { ++p->line; p->column = 0; }
    else if (c == '\r') { p->column = 0; }
    else if (c == '\a') { /* bell does not change column */ }
    else if (c == '\b') { if (p->column > 0) --p->column; }
    else if (c == '\t') { p->column = ((p->column / 8) + 1) * 8; }
    else { ++p->column; }
}

void port_putc(PortObj* p, char c) {
    if (!p) return;
    if (p->closed) vm().wrong_type_arg("write-char", 1, SCM::from_obj(p), "open output port");
    if (!p->is_output) vm().wrong_type_arg("write-char", 1, SCM::from_obj(p), "output port");
    advance_port_pos(p, static_cast<unsigned char>(c));

    if (p->kind == PortKind::String) {
        if (p->str_pos > p->str_buf.size()) {
            p->str_buf.resize(p->str_pos, '\0');
        }
        if (p->str_pos == p->str_buf.size()) {
            p->str_buf.push_back(c);
        } else {
            p->str_buf[p->str_pos] = c;
        }
        ++p->str_pos;
    } else if (p->kind == PortKind::File) {
        p->write_buf.push_back(c);
        if (p->buf_mode == _IONBF || (p->buf_mode == _IOLBF && c == '\n') || p->write_buf.size() >= 4096) {
            port_flush(p);
        }
    } else if (p->kind == PortKind::Pipe) {
        p->write_buf.push_back(c);
        if (p->buf_mode == _IONBF || (p->buf_mode == _IOLBF && c == '\n') || p->write_buf.size() >= 4096) {
            port_flush(p);
        }
    } else if (p->kind == PortKind::Soft) {
        if (is_vector(p->soft_vec)) {
            auto* v = as_obj<VectorObj>(p->soft_vec);
            if (v->elts.size() >= 1 && v->elts[0].is_true()) {
                vm().call1(v->elts[0], SCM::from_char(static_cast<unsigned char>(c)));
            }
        }
    }
}

void port_puts(PortObj* p, std::string_view s) {
    if (!p) return;
    if (p->closed) vm().wrong_type_arg("display", 1, SCM::from_obj(p), "open output port");
    if (!p->is_output) vm().wrong_type_arg("display", 1, SCM::from_obj(p), "output port");
    if (p->kind == PortKind::String) {
        if (p->str_pos == p->str_buf.size()) {
            p->str_buf.append(s);
            p->str_pos += s.size();
        } else {
            if (p->str_pos + s.size() > p->str_buf.size()) {
                p->str_buf.resize(p->str_pos + s.size(), '\0');
            }
            std::memcpy(p->str_buf.data() + p->str_pos, s.data(), s.size());
            p->str_pos += s.size();
        }
        for (char c : s) advance_port_pos(p, static_cast<unsigned char>(c));
        return;
    }
    if (p->kind == PortKind::Soft && is_vector(p->soft_vec)) {
        auto* v = as_obj<VectorObj>(p->soft_vec);
        if (v->elts.size() >= 2 && v->elts[1].is_true()) {
            vm().call1(v->elts[1], vm().make_string(s));
            for (char c : s) advance_port_pos(p, static_cast<unsigned char>(c));
            return;
        }
    }
    for (char c : s) port_putc(p, c);
}

int port_getc(PortObj* p) {
    if (!p) return EOF;
    if (p->closed) vm().wrong_type_arg("read-char", 1, SCM::from_obj(p), "open input port");
    if (!p->is_input) vm().wrong_type_arg("read-char", 1, SCM::from_obj(p), "input port");

    int c = EOF;
    if (!p->unread_buf.empty()) {
        c = static_cast<unsigned char>(p->unread_buf.back());
        p->unread_buf.pop_back();
    } else if (p->kind == PortKind::String) {
        if (p->str_pos < p->str_buf.size()) {
            c = static_cast<unsigned char>(p->str_buf[p->str_pos++]);
        }
    } else if (p->kind == PortKind::File) {
        if (p->read_pos >= p->read_buf.size()) {
            p->read_buf.resize(16384);
            ssize_t n;
            do {
                n = ::read(p->fd, p->read_buf.data(), p->read_buf.size());
            } while (n < 0 && errno == EINTR);
            if (n < 0) { p->read_buf.clear(); vm().syserror("read-char", errno); }
            if (n == 0) { p->read_buf.clear(); return EOF; }
            p->read_buf.resize(static_cast<size_t>(n));
            p->read_pos = 0;
        }
        c = static_cast<unsigned char>(p->read_buf[p->read_pos++]);
    } else if (p->kind == PortKind::Pipe) {
        if (p->pipe_fp) {
            c = std::fgetc(p->pipe_fp);
        }
    } else if (p->kind == PortKind::Soft) {
        if (is_vector(p->soft_vec)) {
            auto* v = as_obj<VectorObj>(p->soft_vec);
            if (v->elts.size() >= 4 && v->elts[3].is_true()) {
                SCM res = vm().call0(v->elts[3]);
                if (res.is_char()) c = static_cast<int>(res.to_char());
                else if (res.is_eof() || res.is_false()) c = EOF;
            }
        }
    }

    if (c != EOF) {
        advance_port_pos(p, c);
    }
    return c;
}

void port_ungetc(PortObj* p, int c) {
    if (!p || c == EOF) return;
    p->unread_buf.push_back(static_cast<char>(c));
    if (c == '\n') {
        if (p->line > 0) --p->line;
    } else {
        if (p->column > 0) --p->column;
    }
}

void port_ungets(PortObj* p, std::string_view s) {
    for (size_t i = s.size(); i > 0; --i) {
        port_ungetc(p, static_cast<unsigned char>(s[i - 1]));
    }
}

int port_peekc(PortObj* p) {
    int c = port_getc(p);
    if (c != EOF) port_ungetc(p, c);
    return c;
}

SCM make_file_port(int fd, std::string_view filename, std::string_view mode, bool owns_fd) {
    auto* p = vm().heap().alloc<PortObj>();
    p->kind = PortKind::File;
    p->fd = fd;
    p->owns_fd = owns_fd;
    p->filename = std::string(filename);
    p->mode_str = std::string(mode);
    for (char c : mode) {
        if (c == 'r') p->is_input = true;
        else if (c == 'w' || c == 'a') p->is_output = true;
        else if (c == '+') { p->is_input = true; p->is_output = true; }
        else if (c == '0') p->buf_mode = _IONBF;
        else if (c == 'l') p->buf_mode = _IOLBF;
    }
    if (!p->is_input && !p->is_output) p->is_input = true;
    return SCM::from_obj(p);
}

SCM make_input_string_port(std::string s) {
    auto* p = vm().heap().alloc<PortObj>();
    p->kind = PortKind::String;
    p->is_input = true;
    p->is_output = false;
    p->mode_str = "r";
    p->str_buf = std::move(s);
    p->str_pos = 0;
    return SCM::from_obj(p);
}

SCM make_output_string_port() {
    auto* p = vm().heap().alloc<PortObj>();
    p->kind = PortKind::String;
    p->is_input = false;
    p->is_output = true;
    p->mode_str = "w";
    return SCM::from_obj(p);
}

void flush_all_ports() {
    for (PortObj* p : vm().heap().open_ports()) {
        if (!p->closed && p->is_output) {
            try { port_flush(p); } catch (...) {}
        }
    }
}

void simple_format_to_port(SCM port, std::string_view fmt, SCM args) {
    auto* p = as_obj<PortObj>(port);
    SCM cur = args;
    for (size_t i = 0; i < fmt.size(); ++i) {
        if (fmt[i] == '~' && i + 1 < fmt.size()) {
            char spec = fmt[++i];
            if (spec == '~') {
                port_putc(p, '~');
            } else if (spec == '%') {
                port_putc(p, '\n');
            } else if (spec == 'A' || spec == 'a') {
                if (!is_pair(cur)) vm().misc_error("simple-format", "FORMAT: Missing argument");
                vm().write_to_port(car(cur), port, true);
                cur = cdr(cur);
            } else if (spec == 'S' || spec == 's') {
                if (!is_pair(cur)) vm().misc_error("simple-format", "FORMAT: Missing argument");
                vm().write_to_port(car(cur), port, false);
                cur = cdr(cur);
            } else {
                port_putc(p, '~');
                port_putc(p, spec);
            }
        } else {
            port_putc(p, fmt[i]);
        }
    }
}

SCM VM::current_input_port() { return fluid_ref(cur_inport_fluid); }
SCM VM::current_output_port() { return fluid_ref(cur_outport_fluid); }
SCM VM::current_error_port() { return fluid_ref(cur_errport_fluid); }
SCM VM::current_load_port() { return fluid_ref(cur_loadport_fluid); }
SCM VM::set_current_input_port(SCM p) {
    if (!is_port(p) || !as_obj<PortObj>(p)->is_input) wrong_type_arg("set-current-input-port", 1, p, "input port");
    SCM old = current_input_port();
    fluid_set(cur_inport_fluid, p);
    return old;
}
SCM VM::set_current_output_port(SCM p) {
    if (!is_port(p) || !as_obj<PortObj>(p)->is_output) wrong_type_arg("set-current-output-port", 1, p, "output port");
    SCM old = current_output_port();
    fluid_set(cur_outport_fluid, p);
    return old;
}
SCM VM::set_current_error_port(SCM p) {
    if (!is_port(p) || !as_obj<PortObj>(p)->is_output) wrong_type_arg("set-current-error-port", 1, p, "output port");
    SCM old = current_error_port();
    fluid_set(cur_errport_fluid, p);
    return old;
}

static int parse_open_flags(std::string_view mode) {
    bool r = false, w = false, a = false, plus = false;
    for (char c : mode) {
        if (c == 'r') r = true;
        else if (c == 'w') w = true;
        else if (c == 'a') a = true;
        else if (c == '+') plus = true;
    }
    if (w) return (plus ? O_RDWR : O_WRONLY) | O_CREAT | O_TRUNC;
    if (a) return (plus ? O_RDWR : O_WRONLY) | O_CREAT | O_APPEND;
    if (r) return plus ? O_RDWR : O_RDONLY;
    return O_RDONLY;
}

void VM::init_ports() {
    cur_inport_fluid = make_fluid(make_file_port(STDIN_FILENO, "standard input", "r0", false));
    cur_outport_fluid = make_fluid(make_file_port(STDOUT_FILENO, "standard output", "wl", false));
    cur_errport_fluid = make_fluid(make_file_port(STDERR_FILENO, "standard error", "w0", false));
    cur_loadport_fluid = make_fluid(SCM_BOOL_F);

    define_proc("port?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_port(a[0])); });
    define_proc("input-port?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_port(a[0]) && as_obj<PortObj>(a[0])->is_input);
    });
    define_proc("output-port?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_port(a[0]) && as_obj<PortObj>(a[0])->is_output);
    });
    define_proc("file-port?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_port(a[0]) && (as_obj<PortObj>(a[0])->kind == PortKind::File || as_obj<PortObj>(a[0])->kind == PortKind::Pipe));
    });
    define_proc("port-closed?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("port-closed?", 1, a[0], "port");
        return SCM::from_bool(as_obj<PortObj>(a[0])->closed);
    });
    define_proc("eof-object?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(a[0].is_eof()); });

    define_proc("current-input-port", 0, 0, false, [this](const SCM*, size_t) -> SCM { return current_input_port(); });
    define_proc("current-output-port", 0, 0, false, [this](const SCM*, size_t) -> SCM { return current_output_port(); });
    define_proc("current-error-port", 0, 0, false, [this](const SCM*, size_t) -> SCM { return current_error_port(); });
    define_proc("current-load-port", 0, 0, false, [this](const SCM*, size_t) -> SCM { return current_load_port(); });
    define_proc("set-current-input-port", 1, 0, false, [this](const SCM* a, size_t) -> SCM { return set_current_input_port(a[0]); });
    define_proc("set-current-output-port", 1, 0, false, [this](const SCM* a, size_t) -> SCM { return set_current_output_port(a[0]); });
    define_proc("set-current-error-port", 1, 0, false, [this](const SCM* a, size_t) -> SCM { return set_current_error_port(a[0]); });

    // File ports
    define_proc("open-file", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("open-file", 1, a[0], "string");
        if (!is_string(a[1])) wrong_type_arg("open-file", 2, a[1], "string");
        std::string fname = as_obj<StringObj>(a[0])->to_std_string();
        std::string mode = as_obj<StringObj>(a[1])->to_std_string();
        int flags = parse_open_flags(mode);
        int fd = ::open(fname.c_str(), flags, 0666);
        if (fd < 0) syserror("open-file", errno, fname);
        return make_file_port(fd, fname, mode, true);
    });
    define_proc("open-input-file", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("open-input-file", 1, a[0], "string");
        std::string fname = as_obj<StringObj>(a[0])->to_std_string();
        int fd = ::open(fname.c_str(), O_RDONLY);
        if (fd < 0) syserror("open-input-file", errno, fname);
        return make_file_port(fd, fname, "r", true);
    });
    define_proc("open-output-file", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("open-output-file", 1, a[0], "string");
        std::string fname = as_obj<StringObj>(a[0])->to_std_string();
        int fd = ::open(fname.c_str(), O_WRONLY | O_CREAT | O_TRUNC, 0666);
        if (fd < 0) syserror("open-output-file", errno, fname);
        return make_file_port(fd, fname, "w", true);
    });
    define_proc("fdopen", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        int fd = static_cast<int>(scm_to_int64(a[0], "fdopen", 1));
        if (!is_string(a[1])) wrong_type_arg("fdopen", 2, a[1], "string");
        return make_file_port(fd, "", as_obj<StringObj>(a[1])->view(), true);
    });
    define_proc("fdes->inport", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        int fd = static_cast<int>(scm_to_int64(a[0], "fdes->inport", 1));
        SCM p = make_file_port(fd, "", "r", false);
        as_obj<PortObj>(p)->revealed = 1;
        return p;
    });
    define_proc("fdes->outport", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        int fd = static_cast<int>(scm_to_int64(a[0], "fdes->outport", 1));
        SCM p = make_file_port(fd, "", "w", false);
        as_obj<PortObj>(p)->revealed = 1;
        return p;
    });
    define_proc("port->fdes", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("port->fdes", 1, a[0], "port");
        auto* p = as_obj<PortObj>(a[0]);
        if (p->closed) misc_error("port->fdes", "Port is closed");
        int fd = (p->kind == PortKind::Pipe && p->pipe_fp) ? fileno(p->pipe_fp) : p->fd;
        if (fd < 0) wrong_type_arg("port->fdes", 1, a[0], "file port");
        ++p->revealed;
        p->owns_fd = false;
        return make_integer(fd);
    });
    define_proc("fileno", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("fileno", 1, a[0], "port");
        auto* p = as_obj<PortObj>(a[0]);
        if (p->closed) misc_error("fileno", "Port is closed");
        int fd = (p->kind == PortKind::Pipe && p->pipe_fp) ? fileno(p->pipe_fp) : p->fd;
        if (fd < 0) wrong_type_arg("fileno", 1, a[0], "file port");
        return make_integer(fd);
    });
    define_proc("port-revealed", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("port-revealed", 1, a[0], "port");
        return make_integer(as_obj<PortObj>(a[0])->revealed);
    });
    define_proc("set-port-revealed!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("set-port-revealed!", 1, a[0], "port");
        as_obj<PortObj>(a[0])->revealed = static_cast<int>(scm_to_int64(a[1], "set-port-revealed!", 2));
        return SCM_UNSPECIFIED;
    });

    auto close_fn = [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("close-port", 1, a[0], "port");
        bool was_open = !as_obj<PortObj>(a[0])->closed;
        port_close(as_obj<PortObj>(a[0]));
        return SCM::from_bool(was_open);
    };
    define_proc("close-port", 1, 0, false, close_fn);
    define_proc("close-input-port", 1, 0, false, close_fn);
    define_proc("close-output-port", 1, 0, false, close_fn);
    define_proc("close", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_port(a[0])) {
            bool was_open = !as_obj<PortObj>(a[0])->closed;
            port_close(as_obj<PortObj>(a[0]));
            return SCM::from_bool(was_open);
        }
        int fd = static_cast<int>(scm_to_int64(a[0], "close", 1));
        if (::close(fd) < 0) syserror("close", errno);
        return SCM_BOOL_T;
    });

    // String ports
    define_proc("open-input-string", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("open-input-string", 1, a[0], "string");
        return make_input_string_port(as_obj<StringObj>(a[0])->to_std_string());
    });
    define_proc("open-output-string", 0, 0, false, [](const SCM*, size_t) -> SCM {
        return make_output_string_port();
    });
    define_proc("get-output-string", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0]) || as_obj<PortObj>(a[0])->closed ||
            as_obj<PortObj>(a[0])->kind != PortKind::String || !as_obj<PortObj>(a[0])->is_output) {
            wrong_type_arg("get-output-string", 1, a[0], "open output string port");
        }
        return make_string(as_obj<PortObj>(a[0])->str_buf);
    });
    define_proc("call-with-input-string", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("call-with-input-string", 1, a[0], "string");
        SCM p = make_input_string_port(as_obj<StringObj>(a[0])->to_std_string());
        return call1(a[1], p);
    });
    define_proc("call-with-output-string", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM p = make_output_string_port();
        call1(a[0], p);
        auto* po = as_obj<PortObj>(p);
        if (po->closed) wrong_type_arg("get-output-string", 1, p, "open output string port");
        return make_string(po->str_buf);
    });

    // Soft ports
    define_proc("make-soft-port", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_vector(a[0]) || as_obj<VectorObj>(a[0])->elts.size() < 5) {
            wrong_type_arg("make-soft-port", 1, a[0], "vector of length 5");
        }
        if (!is_string(a[1])) wrong_type_arg("make-soft-port", 2, a[1], "string");
        auto* p = heap_.alloc<PortObj>();
        p->kind = PortKind::Soft;
        p->soft_vec = a[0];
        std::string mode = as_obj<StringObj>(a[1])->to_std_string();
        p->mode_str = mode;
        for (char c : mode) {
            if (c == 'r') p->is_input = true;
            else if (c == 'w') p->is_output = true;
            else if (c == '+') { p->is_input = true; p->is_output = true; }
        }
        return SCM::from_obj(p);
    });

    define_proc("%make-void-port", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("%make-void-port", 1, a[0], "string");
        SCM noop1 = make_subr("void-write", 1, 0, false, [](const SCM*, size_t) -> SCM { return SCM_UNSPECIFIED; });
        SCM noop0 = make_subr("void-flush", 0, 0, false, [](const SCM*, size_t) -> SCM { return SCM_UNSPECIFIED; });
        SCM eof0  = make_subr("void-read", 0, 0, false, [](const SCM*, size_t) -> SCM { return SCM_BOOL_F; });
        SCM vec = make_vector(5, SCM_BOOL_F);
        auto* v = as_obj<VectorObj>(vec);
        v->elts[0] = noop1;
        v->elts[1] = noop1;
        v->elts[2] = noop0;
        v->elts[3] = eof0;
        v->elts[4] = noop0;
        auto* p = heap_.alloc<PortObj>();
        p->kind = PortKind::Soft;
        p->soft_vec = vec;
        std::string mode = as_obj<StringObj>(a[0])->to_std_string();
        p->mode_str = mode;
        for (char c : mode) {
            if (c == 'r') p->is_input = true;
            else if (c == 'w' || c == 'a') p->is_output = true;
            else if (c == '+') { p->is_input = true; p->is_output = true; }
        }
        return SCM::from_obj(p);
    });

    // Character I/O
    define_proc("read-char", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM port = (nargs >= 1 && !a[0].is_undefined()) ? a[0] : current_input_port();
        if (!is_port(port)) wrong_type_arg("read-char", 1, port, "input port");
        int c = port_getc(as_obj<PortObj>(port));
        return (c == EOF) ? SCM_EOF : SCM::from_char(static_cast<uint32_t>(c));
    });
    define_proc("peek-char", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM port = (nargs >= 1 && !a[0].is_undefined()) ? a[0] : current_input_port();
        if (!is_port(port)) wrong_type_arg("peek-char", 1, port, "input port");
        int c = port_peekc(as_obj<PortObj>(port));
        return (c == EOF) ? SCM_EOF : SCM::from_char(static_cast<uint32_t>(c));
    });
    define_proc("unread-char", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!a[0].is_char()) wrong_type_arg("unread-char", 1, a[0], "char");
        SCM port = (nargs >= 2 && !a[1].is_undefined()) ? a[1] : current_input_port();
        if (!is_port(port)) wrong_type_arg("unread-char", 2, port, "input port");
        port_ungetc(as_obj<PortObj>(port), static_cast<int>(a[0].to_char()));
        return a[0];
    });
    define_proc("unread-string", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("unread-string", 1, a[0], "string");
        if (!is_port(a[1])) wrong_type_arg("unread-string", 2, a[1], "input port");
        port_ungets(as_obj<PortObj>(a[1]), as_obj<StringObj>(a[0])->view());
        return a[0];
    });
    define_proc("write-char", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!a[0].is_char()) wrong_type_arg("write-char", 1, a[0], "char");
        SCM port = (nargs >= 2 && !a[1].is_undefined()) ? a[1] : current_output_port();
        if (!is_port(port)) wrong_type_arg("write-char", 2, port, "output port");
        port_putc(as_obj<PortObj>(port), static_cast<char>(a[0].to_char()));
        return SCM_UNSPECIFIED;
    });
    define_proc("force-output", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM port = (nargs >= 1 && !a[0].is_undefined()) ? a[0] : current_output_port();
        if (!is_port(port)) wrong_type_arg("force-output", 1, port, "output port");
        port_flush(as_obj<PortObj>(port));
        return SCM_UNSPECIFIED;
    });
    define_proc("flush-all-ports", 0, 0, false, [](const SCM*, size_t) -> SCM {
        flush_all_ports();
        return SCM_UNSPECIFIED;
    });
    define_proc("drain-input", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("drain-input", 1, a[0], "input port");
        auto* p = as_obj<PortObj>(a[0]);
        std::string out;
        while (!p->unread_buf.empty()) {
            out.push_back(p->unread_buf.back());
            p->unread_buf.pop_back();
        }
        if (p->read_pos < p->read_buf.size()) {
            out.append(p->read_buf.substr(p->read_pos));
            p->read_pos = p->read_buf.size();
        }
        return make_string(std::move(out));
    });
    define_proc("char-ready?", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM port = (nargs >= 1 && !a[0].is_undefined()) ? a[0] : current_input_port();
        if (!is_port(port)) wrong_type_arg("char-ready?", 1, port, "input port");
        auto* p = as_obj<PortObj>(port);
        if (!p->unread_buf.empty() || p->read_pos < p->read_buf.size()) return SCM_BOOL_T;
        if (p->kind == PortKind::String) return SCM_BOOL_T;
        if (p->kind == PortKind::File && p->fd >= 0) {
            fd_set rfds;
            FD_ZERO(&rfds);
            FD_SET(p->fd, &rfds);
            struct timeval tv{0, 0};
            return SCM::from_bool(select(p->fd + 1, &rfds, nullptr, nullptr, &tv) > 0);
        }
        return SCM_BOOL_T;
    });

    // Port metadata
    define_proc("port-filename", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("port-filename", 1, a[0], "port");
        auto* p = as_obj<PortObj>(a[0]);
        return p->filename.empty() ? SCM_BOOL_F : make_string(p->filename);
    });
    define_proc("set-port-filename!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("set-port-filename!", 1, a[0], "port");
        auto* p = as_obj<PortObj>(a[0]);
        p->filename = is_string(a[1]) ? as_obj<StringObj>(a[1])->to_std_string() : "";
        p->filename_scm = is_string(a[1]) ? a[1] : SCM_BOOL_F;
        return SCM_UNSPECIFIED;
    });
    define_proc("port-line", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("port-line", 1, a[0], "port");
        return make_integer(as_obj<PortObj>(a[0])->line);
    });
    define_proc("set-port-line!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("set-port-line!", 1, a[0], "port");
        as_obj<PortObj>(a[0])->line = static_cast<long>(scm_to_int64(a[1], "set-port-line!", 2));
        return SCM_UNSPECIFIED;
    });
    define_proc("port-column", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("port-column", 1, a[0], "port");
        return make_integer(as_obj<PortObj>(a[0])->column);
    });
    define_proc("set-port-column!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("set-port-column!", 1, a[0], "port");
        as_obj<PortObj>(a[0])->column = static_cast<long>(scm_to_int64(a[1], "set-port-column!", 2));
        return SCM_UNSPECIFIED;
    });
    define_proc("port-mode", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("port-mode", 1, a[0], "port");
        return make_string(as_obj<PortObj>(a[0])->mode_str);
    });

    // Seek / ftell / truncate-file / setvbuf
    define_proc("seek", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t offset = scm_to_int64(a[1], "seek", 2);
        int whence = static_cast<int>(scm_to_int64(a[2], "seek", 3));
        if (is_port(a[0])) {
            auto* p = as_obj<PortObj>(a[0]);
            if (p->closed) misc_error("seek", "Port is closed");
            // Special case: (seek port 0 SEEK_CUR) is ftell — MUST NOT discard unread_buf or read_buf!
            if (whence == SEEK_CUR && offset == 0) {
                if (p->kind == PortKind::String) {
                    int64_t pos = static_cast<int64_t>(p->str_pos) - static_cast<int64_t>(p->unread_buf.size());
                    return make_integer(pos);
                }
                if (p->is_output) port_flush(p);
                off_t r = ::lseek(p->fd, 0, SEEK_CUR);
                if (r < 0) syserror("seek", errno);
                r -= static_cast<off_t>((p->read_buf.size() - p->read_pos) + p->unread_buf.size());
                return make_integer(static_cast<int64_t>(r));
            }
            if (p->kind == PortKind::String) {
                int64_t cur_pos = static_cast<int64_t>(p->str_pos) - static_cast<int64_t>(p->unread_buf.size());
                p->unread_buf.clear();
                int64_t base = (whence == SEEK_SET) ? 0 : (whence == SEEK_CUR ? cur_pos : static_cast<int64_t>(p->str_buf.size()));
                int64_t np = base + offset;
                if (np < 0) out_of_range("seek", a[1]);
                p->str_pos = static_cast<size_t>(np);
                return make_integer(np);
            }
            if (p->is_output) port_flush(p);
            if (whence == SEEK_CUR) {
                offset -= static_cast<int64_t>((p->read_buf.size() - p->read_pos) + p->unread_buf.size());
            }
            p->unread_buf.clear();
            p->read_buf.clear(); p->read_pos = 0;
            off_t r = ::lseek(p->fd, static_cast<off_t>(offset), whence);
            if (r < 0) syserror("seek", errno);
            return make_integer(static_cast<int64_t>(r));
        }
        int fd = static_cast<int>(scm_to_int64(a[0], "seek", 1));
        off_t r = ::lseek(fd, static_cast<off_t>(offset), whence);
        if (r < 0) syserror("seek", errno);
        return make_integer(static_cast<int64_t>(r));
    });
    define_proc("ftell", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (is_port(a[0])) {
            auto* p = as_obj<PortObj>(a[0]);
            if (p->kind == PortKind::String) {
                int64_t pos = static_cast<int64_t>(p->str_pos) - static_cast<int64_t>(p->unread_buf.size());
                return make_integer(pos);
            }
            if (p->is_output) port_flush(p);
            off_t r = ::lseek(p->fd, 0, SEEK_CUR);
            if (r < 0) syserror("ftell", errno);
            r -= static_cast<off_t>(p->read_buf.size() - p->read_pos + p->unread_buf.size());
            return make_integer(static_cast<int64_t>(r));
        }
        int fd = static_cast<int>(scm_to_int64(a[0], "ftell", 1));
        off_t r = ::lseek(fd, 0, SEEK_CUR);
        if (r < 0) syserror("ftell", errno);
        return make_integer(static_cast<int64_t>(r));
    });
    define_proc("truncate-file", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        bool has_len = (nargs >= 2 && !a[1].is_undefined());
        if (is_string(a[0])) {
            if (!has_len) misc_error("truncate-file", "must supply length for filename");
            off_t len = static_cast<off_t>(scm_to_int64(a[1], "truncate-file", 2));
            if (::truncate(as_obj<StringObj>(a[0])->to_std_string().c_str(), len) < 0) syserror("truncate-file", errno);
            return SCM_UNSPECIFIED;
        }
        int fd = -1;
        off_t len = 0;
        if (is_port(a[0])) {
            auto* p = as_obj<PortObj>(a[0]);
            if (p->closed) wrong_type_arg("truncate-file", 1, a[0], "open file port");
            if (p->is_output) port_flush(p);
            fd = p->fd;
            if (has_len) {
                len = static_cast<off_t>(scm_to_int64(a[1], "truncate-file", 2));
            } else {
                off_t cur = ::lseek(fd, 0, SEEK_CUR);
                if (cur < 0) syserror("truncate-file", errno);
                len = cur - static_cast<off_t>((p->read_buf.size() - p->read_pos) + p->unread_buf.size());
                if (len < 0) len = 0;
                p->read_buf.clear(); p->read_pos = 0; p->unread_buf.clear();
                ::lseek(fd, len, SEEK_SET);
            }
        } else {
            fd = static_cast<int>(scm_to_int64(a[0], "truncate-file", 1));
            len = has_len ? static_cast<off_t>(scm_to_int64(a[1], "truncate-file", 2)) : ::lseek(fd, 0, SEEK_CUR);
        }
        if (::ftruncate(fd, len) < 0) syserror("truncate-file", errno);
        return SCM_UNSPECIFIED;
    });
    define_proc("setvbuf", 2, 1, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("setvbuf", 1, a[0], "port");
        auto* p = as_obj<PortObj>(a[0]);
        p->buf_mode = static_cast<int>(scm_to_int64(a[1], "setvbuf", 2));
        return SCM_UNSPECIFIED;
    });
    define_proc("isatty?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("isatty?", 1, a[0], "port");
        auto* p = as_obj<PortObj>(a[0]);
        if (p->closed || p->fd < 0) return SCM_BOOL_F;
        return SCM::from_bool(::isatty(p->fd) != 0);
    });
    define_proc("ttyname", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_port(a[0])) wrong_type_arg("ttyname", 1, a[0], "port");
        auto* p = as_obj<PortObj>(a[0]);
        char* name = (p->fd >= 0) ? ::ttyname(p->fd) : nullptr;
        if (!name) syserror("ttyname", errno);
        return make_string(name);
    });
    define_proc("port-for-each", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        ScmRootVec ports(heap_);
        for (Obj* cur = heap_.all_objects(); cur; cur = cur->gc_next) {
            if (cur->tag == ObjTag::Port && !static_cast<PortObj*>(cur)->closed) {
                ports.push_back(SCM::from_obj(cur));
            }
        }
        for (size_t i = 0; i < ports.size(); ++i) call1(a[0], ports[i]);
        return SCM_UNSPECIFIED;
    });
    define_proc("close-all-ports-except", 0, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        SCM keep = nargs > 0 ? a[0] : SCM_EOL;
        for (Obj* cur = heap_.all_objects(); cur; cur = cur->gc_next) {
            if (cur->tag == ObjTag::Port) {
                SCM ps = SCM::from_obj(cur);
                if (scm_memq(ps, keep).is_false()) {
                    port_close(static_cast<PortObj*>(cur));
                }
            }
        }
        return SCM_UNSPECIFIED;
    });

    // simple-format
    define_proc("simple-format", 2, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[1])) wrong_type_arg("simple-format", 2, a[1], "string");
        auto fmt = as_obj<StringObj>(a[1])->view();
        SCM rest = nargs > 2 ? a[2] : SCM_EOL;
        if (a[0].is_false()) {
            SCM sp = make_output_string_port();
            simple_format_to_port(sp, fmt, rest);
            return make_string(as_obj<PortObj>(sp)->str_buf);
        }
        SCM port = (a[0] == SCM_BOOL_T) ? current_output_port() : a[0];
        if (!is_port(port)) wrong_type_arg("simple-format", 1, a[0], "port or boolean");
        simple_format_to_port(port, fmt, rest);
        return SCM_BOOL_T;
    });

    // rdelim & rw builtins
    auto define_rdelim = [this]() {
        define_proc("%read-delimited!", 3, 3, false, [this](const SCM* a, size_t nargs) -> SCM {
            // (%read-delimited! delims buf gobble [port [start [end]]])
            if (!is_string(a[0])) wrong_type_arg("%read-delimited!", 1, a[0], "string");
            if (!is_string(a[1])) wrong_type_arg("%read-delimited!", 2, a[1], "string");
            auto delims = as_obj<StringObj>(a[0])->view();
            auto* buf = as_obj<StringObj>(a[1]);
            bool gobble = a[2].is_true();
            SCM port = (nargs >= 4 && !a[3].is_undefined()) ? a[3] : current_input_port();
            if (!is_port(port)) wrong_type_arg("%read-delimited!", 4, port, "input port");
            int64_t start = (nargs >= 5 && !a[4].is_undefined()) ? scm_to_int64(a[4], "%read-delimited!", 5) : 0;
            int64_t end = (nargs >= 6 && !a[5].is_undefined()) ? scm_to_int64(a[5], "%read-delimited!", 6) : static_cast<int64_t>(buf->len);
            if (start < 0 || end < start || static_cast<size_t>(end) > buf->len) out_of_range("%read-delimited!", a[4]);
            auto* p = as_obj<PortObj>(port);
            int64_t count = 0;
            SCM term = SCM_BOOL_F;
            while (start + count < end) {
                int c = port_getc(p);
                if (c == EOF) { term = SCM_EOF; break; }
                if (delims.find(static_cast<char>(c)) != std::string_view::npos) {
                    term = SCM::from_char(static_cast<uint32_t>(c));
                    if (!gobble) port_ungetc(p, c);
                    break;
                }
                buf->mutable_data()[start + count] = static_cast<char>(c);
                ++count;
            }
            return cons(term, make_integer(count));
        });

        define_proc("%read-line", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
            SCM port = (nargs >= 1 && !a[0].is_undefined()) ? a[0] : current_input_port();
            if (!is_port(port)) wrong_type_arg("%read-line", 1, port, "input port");
            auto* p = as_obj<PortObj>(port);
            std::string line;
            int c;
            while ((c = port_getc(p)) != EOF) {
                if (c == '\n') {
                    return cons(make_string(std::move(line)), SCM::from_char('\n'));
                }
                line.push_back(static_cast<char>(c));
            }
            if (line.empty()) return cons(SCM_EOF, SCM_EOF);
            return cons(make_string(std::move(line)), SCM_EOF);
        });

        define_proc("write-line", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
            SCM port = (nargs >= 2 && !a[1].is_undefined()) ? a[1] : current_output_port();
            if (!is_port(port)) wrong_type_arg("write-line", 2, port, "output port");
            write_to_port(a[0], port, true);
            port_putc(as_obj<PortObj>(port), '\n');
            return SCM_UNSPECIFIED;
        });
    };
    define_proc("%init-rdelim-builtins", 0, 0, false, [define_rdelim](const SCM*, size_t) -> SCM {
        define_rdelim();
        return SCM_UNSPECIFIED;
    });

    auto define_rw = [this]() {
        define_proc("read-string!/partial", 1, 3, false, [this](const SCM* a, size_t nargs) -> SCM {
            if (!is_string(a[0])) wrong_type_arg("read-string!/partial", 1, a[0], "string");
            auto* buf = as_obj<StringObj>(a[0]);
            SCM port = (nargs >= 2 && !a[1].is_undefined()) ? a[1] : current_input_port();
            int64_t start = (nargs >= 3 && !a[2].is_undefined()) ? scm_to_int64(a[2], "read-string!/partial", 3) : 0;
            int64_t end = (nargs >= 4 && !a[3].is_undefined()) ? scm_to_int64(a[3], "read-string!/partial", 4) : static_cast<int64_t>(buf->len);
            if (!is_port(port)) wrong_type_arg("read-string!/partial", 2, port, "input port");
            if (start < 0 || end < start || static_cast<size_t>(end) > buf->len) out_of_range("read-string!/partial", a[2]);
            auto* p = as_obj<PortObj>(port);
            int64_t count = 0;
            while (start + count < end) {
                int c = port_getc(p);
                if (c == EOF) break;
                buf->mutable_data()[start + count] = static_cast<char>(c);
                ++count;
            }
            return (count == 0 && start < end) ? SCM_EOF : make_integer(count);
        });
        define_proc("write-string/partial", 1, 3, false, [this](const SCM* a, size_t nargs) -> SCM {
            if (!is_string(a[0])) wrong_type_arg("write-string/partial", 1, a[0], "string");
            auto sv = as_obj<StringObj>(a[0])->view();
            SCM port = (nargs >= 2 && !a[1].is_undefined()) ? a[1] : current_output_port();
            int64_t start = (nargs >= 3 && !a[2].is_undefined()) ? scm_to_int64(a[2], "write-string/partial", 3) : 0;
            int64_t end = (nargs >= 4 && !a[3].is_undefined()) ? scm_to_int64(a[3], "write-string/partial", 4) : static_cast<int64_t>(sv.size());
            if (!is_port(port)) wrong_type_arg("write-string/partial", 2, port, "output port");
            if (start < 0 || end < start || static_cast<size_t>(end) > sv.size()) out_of_range("write-string/partial", a[2]);
            port_puts(as_obj<PortObj>(port), sv.substr(static_cast<size_t>(start), static_cast<size_t>(end - start)));
            return make_integer(end - start);
        });
    };
    define_proc("%init-rw-builtins", 0, 0, false, [define_rw](const SCM*, size_t) -> SCM {
        define_rw();
        return SCM_UNSPECIFIED;
    });

    // Constants for buffering and seek
    define("_IONBF", make_integer(_IONBF));
    define("_IOLBF", make_integer(_IOLBF));
    define("_IOFBF", make_integer(_IOFBF));
    define("SEEK_SET", make_integer(SEEK_SET));
    define("SEEK_CUR", make_integer(SEEK_CUR));
    define("SEEK_END", make_integer(SEEK_END));
}

} // namespace guile
