#include "guile_cpp/vm.hpp"
#include <sys/stat.h>
#include <unistd.h>
#include <cstdlib>
#include <fcntl.h>
#include <cerrno>

namespace guile {

// TeXmacs: the directories of the Scheme library are given at run time by
// the embedder (set_data_dirs, before VM::init), not compiled in, so that
// the packages are relocatable; the compiled-in paths remain the defaults
#ifndef GUILE_CPP_PKGDATA_DIR
#define GUILE_CPP_PKGDATA_DIR ""
#endif
#ifndef GUILE_CPP_LIBRARY_DIR
#define GUILE_CPP_LIBRARY_DIR ""
#endif
#ifndef GUILE_CPP_SITE_DIR
#define GUILE_CPP_SITE_DIR ""
#endif
#ifndef GUILE_CPP_SOURCE_DIR
#define GUILE_CPP_SOURCE_DIR ""
#endif

static std::string& pkgdata_dir() {
    static std::string d = GUILE_CPP_PKGDATA_DIR; return d;
}
static std::string& library_dir() {
    static std::string d = GUILE_CPP_LIBRARY_DIR; return d;
}
static std::string& site_dir() {
    static std::string d = GUILE_CPP_SITE_DIR; return d;
}

void set_data_dirs(const char* library, const char* pkgdata, const char* site) {
    library_dir() = library ? library : "";
    pkgdata_dir() = pkgdata ? pkgdata : "";
    site_dir() = site ? site : "";
}

static bool file_is_regular_readable(const std::string& path) {
    struct stat st{};
    if (::stat(path.c_str(), &st) != 0) return false;
    if (!S_ISREG(st.st_mode)) return false;
    return ::access(path.c_str(), R_OK) == 0;
}

SCM VM::search_load_path(const std::string& filename) {
    if (filename.empty()) return SCM_BOOL_F;

    SCM exts = is_variable(load_extensions_var) ? as_obj<VariableObj>(load_extensions_var)->value : SCM_EOL;

    auto try_with_exts = [&](const std::string& base) -> SCM {
        // Check exact filename first if it already matches or try extensions
        if (file_is_regular_readable(base)) return make_string(base);
        SCM cur = exts;
        while (is_pair(cur)) {
            if (is_string(car(cur))) {
                std::string candidate = base + as_obj<StringObj>(car(cur))->to_std_string();
                if (file_is_regular_readable(candidate)) return make_string(candidate);
            }
            cur = cdr(cur);
        }
        return SCM_BOOL_F;
    };

    if (filename[0] == '/') {
        return try_with_exts(filename);
    }

    SCM path_list = is_variable(load_path_var) ? as_obj<VariableObj>(load_path_var)->value : SCM_EOL;
    SCM cur = path_list;
    while (is_pair(cur)) {
        if (is_string(car(cur))) {
            std::string dir = as_obj<StringObj>(car(cur))->to_std_string();
            std::string full = dir.empty() ? filename
                             : (dir.back() == '/' ? (dir + filename) : (dir + "/" + filename));
            SCM found = try_with_exts(full);
            if (found.is_true()) return found;
        }
        cur = cdr(cur);
    }
    return SCM_BOOL_F;
}

SCM VM::primitive_load(const std::string& filename) {
    if (is_variable(load_hook_var)) {
        SCM hook = as_obj<VariableObj>(load_hook_var)->value;
        if (hook.is_true()) {
            if (!is_procedure(hook)) misc_error("primitive-load", "value of %load-hook is neither a procedure nor #f");
            call1(hook, make_string(filename));
        }
    }

    int fd = ::open(filename.c_str(), O_RDONLY);
    if (fd < 0) syserror("primitive-load", errno, filename);
    SCM port = make_file_port(fd, filename, "r", true);
    SCM old_load_port = fluid_ref(cur_loadport_fluid);
    fluid_set(cur_loadport_fluid, port);

    try {
        while (true) {
            SCM reader = fluid_ref(cur_reader_fluid);
            SCM form = reader.is_false() ? read_from_port(port) : call1(reader, port);
            if (form.is_eof()) break;
            primitive_eval(form);
        }
    } catch (...) {
        fluid_set(cur_loadport_fluid, old_load_port);
        port_close(as_obj<PortObj>(port));
        throw;
    }

    fluid_set(cur_loadport_fluid, old_load_port);
    port_close(as_obj<PortObj>(port));
    return SCM_UNSPECIFIED;
}

SCM VM::primitive_load_path(const std::string& filename) {
    SCM full = search_load_path(filename);
    if (full.is_false()) {
        misc_error("primitive-load-path", "Unable to find file ~S in load path", list1(make_string(filename)));
    }
    return primitive_load(as_obj<StringObj>(full)->to_std_string());
}

void VM::init_load_path() {
    SCM path = SCM_EOL;
    // Standard directories: site-dir, pkgdata-dir, library-dir
    if (!site_dir().empty()) path = cons(make_string(site_dir()), path);
    if (!pkgdata_dir().empty() && pkgdata_dir() != library_dir())
        path = cons(make_string(pkgdata_dir()), path);
    if (!library_dir().empty()) path = cons(make_string(library_dir()), path);

    const char* env_lp = std::getenv("GUILE_LOAD_PATH");
    if (env_lp && *env_lp) {
        std::string s(env_lp);
        std::vector<std::string> parts;
        size_t pos = 0;
        while (true) {
            auto colon = s.find(':', pos);
            if (colon == std::string::npos) {
                parts.push_back(s.substr(pos));
                break;
            }
            parts.push_back(s.substr(pos, colon - pos));
            pos = colon + 1;
        }
        for (size_t i = parts.size(); i > 0; --i) {
            path = cons(make_string(parts[i - 1]), path);
        }
    }
    as_obj<VariableObj>(load_path_var)->value = path;
}

void VM::load_startup_files() {
    primitive_load_path("ice-9/boot-9.scm");
}

void VM::init_load() {
    cur_reader_fluid = make_fluid(SCM_BOOL_F);
    define("current-reader", cur_reader_fluid);

    load_path_var = define("%load-path", SCM_EOL);
    load_extensions_var = define("%load-extensions", list2(make_string(""), make_string(".scm")));
    load_hook_var = define("%load-hook", SCM_BOOL_F);
    load_verbosely_var = define("%load-verbosely", SCM_BOOL_F);

    define_proc("%package-data-dir", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return make_string(pkgdata_dir());
    });
    define_proc("%library-dir", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return make_string(library_dir());
    });
    define_proc("%site-dir", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return make_string(site_dir());
    });

    define_proc("parse-path", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM tail = (nargs >= 2 && !a[1].is_undefined()) ? a[1] : SCM_EOL;
        if (a[0].is_false()) return tail;
        if (!is_string(a[0])) wrong_type_arg("parse-path", 1, a[0], "string or #f");
        auto sv = as_obj<StringObj>(a[0])->view();
        SCM res = tail;
        size_t end = sv.size();
        for (size_t i = sv.size(); i > 0; --i) {
            if (sv[i - 1] == ':') {
                res = cons(make_string(sv.substr(i, end - i)), res);
                end = i - 1;
            }
        }
        res = cons(make_string(sv.substr(0, end)), res);
        return res;
    });

    define_proc("search-path", 2, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[1])) wrong_type_arg("search-path", 2, a[1], "string");
        std::string fn = as_obj<StringObj>(a[1])->to_std_string();
        if (fn.find('\0') != std::string::npos) {
            misc_error("search-path", "string contains #\\nul character: ~S", list1(a[1]));
        }
        if (!fn.empty() && fn[0] == '/') return a[1];
        SCM exts = (nargs >= 3 && !a[2].is_undefined()) ? a[2] : SCM_EOL;
        // If filename already has an extension after last '/', ignore exts
        auto slash = fn.rfind('/');
        auto dot = fn.rfind('.');
        if (dot != std::string::npos && (slash == std::string::npos || dot > slash)) {
            exts = SCM_EOL;
        }
        SCM cur = a[0];
        while (is_pair(cur)) {
            if (!is_string(car(cur))) wrong_type_arg("search-path", 1, a[0], "list of strings");
            std::string dir = as_obj<StringObj>(car(cur))->to_std_string();
            std::string base = dir.empty() ? fn : (dir.back() == '/' ? (dir + fn) : (dir + "/" + fn));
            if (exts.is_null()) {
                if (file_is_regular_readable(base)) return make_string(base);
            } else {
                SCM e = exts;
                while (is_pair(e)) {
                    if (!is_string(car(e))) wrong_type_arg("search-path", 3, exts, "list of strings");
                    std::string cand = base + as_obj<StringObj>(car(e))->to_std_string();
                    if (file_is_regular_readable(cand)) return make_string(cand);
                    e = cdr(e);
                }
            }
            cur = cdr(cur);
        }
        return SCM_BOOL_F;
    });

    define_proc("%search-load-path", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("%search-load-path", 1, a[0], "string");
        return search_load_path(as_obj<StringObj>(a[0])->to_std_string());
    });

    define_proc("primitive-load", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("primitive-load", 1, a[0], "string");
        return primitive_load(as_obj<StringObj>(a[0])->to_std_string());
    });

    define_proc("primitive-load-path", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("primitive-load-path", 1, a[0], "string");
        return primitive_load_path(as_obj<StringObj>(a[0])->to_std_string());
    });

    // %guile-build-info alist
    SCM bi = SCM_EOL;
    auto add_bi = [&](const char* k, const char* v) {
        bi = cons(cons(make_symbol(k), make_string(v)), bi);
    };
    add_bi("srcdir", GUILE_CPP_SOURCE_DIR);
    add_bi("top_srcdir", GUILE_CPP_SOURCE_DIR);
    add_bi("pkgdatadir", pkgdata_dir().c_str());
    add_bi("guileversion", "1.8.8");
    add_bi("libguileinterface", "21:0:4");
    define("%guile-build-info", bi);
}

} // namespace guile
