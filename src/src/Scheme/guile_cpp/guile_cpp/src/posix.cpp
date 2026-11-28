#include "guile_cpp/vm.hpp"
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/utsname.h>
#include <sys/select.h>
#include <signal.h>
#include <pwd.h>
#include <grp.h>
#include <utime.h>
#include <dirent.h>
#include <cerrno>
#include <cstring>
#include <cstdlib>
#include <climits>

extern char** environ;

namespace guile {

static SCM stat_to_vector(const struct stat& st) {
    // Guile 1.8.8 stat vector has 15 elements:
    // 0:dev 1:ino 2:mode 3:nlink 4:uid 5:gid 6:rdev 7:size 8:atime 9:mtime 10:ctime 11:blksize 12:blocks 13:atime_nsec 14:mtime_nsec
    auto* v = vm().heap().alloc<VectorObj>(15, SCM::from_fixnum(0));
    v->elts[0] = vm().make_integer(static_cast<int64_t>(st.st_dev));
    v->elts[1] = vm().make_uinteger(static_cast<uint64_t>(st.st_ino));
    v->elts[2] = vm().make_integer(static_cast<int64_t>(st.st_mode));
    v->elts[3] = vm().make_integer(static_cast<int64_t>(st.st_nlink));
    v->elts[4] = vm().make_integer(static_cast<int64_t>(st.st_uid));
    v->elts[5] = vm().make_integer(static_cast<int64_t>(st.st_gid));
    v->elts[6] = vm().make_integer(static_cast<int64_t>(st.st_rdev));
    v->elts[7] = vm().make_integer(static_cast<int64_t>(st.st_size));
    v->elts[8] = vm().make_integer(static_cast<int64_t>(st.st_atime));
    v->elts[9] = vm().make_integer(static_cast<int64_t>(st.st_mtime));
    v->elts[10] = vm().make_integer(static_cast<int64_t>(st.st_ctime));
    v->elts[11] = vm().make_integer(static_cast<int64_t>(st.st_blksize));
    v->elts[12] = vm().make_integer(static_cast<int64_t>(st.st_blocks));
    const char* tname = "unknown";
    if (S_ISREG(st.st_mode))       tname = "regular";
    else if (S_ISDIR(st.st_mode))  tname = "directory";
    else if (S_ISLNK(st.st_mode))  tname = "symlink";
    else if (S_ISBLK(st.st_mode))  tname = "block-special";
    else if (S_ISCHR(st.st_mode))  tname = "char-special";
    else if (S_ISFIFO(st.st_mode)) tname = "fifo";
    else if (S_ISSOCK(st.st_mode)) tname = "socket";
    v->elts[13] = vm().make_symbol(tname);
    v->elts[14] = vm().make_integer(static_cast<int64_t>(st.st_mode & 07777));
    return SCM::from_obj(v);
}

void VM::init_posix() {
    // ========================================================================
    // Filesystem (filesys.c)
    // ========================================================================
    define_proc("stat", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        struct stat st{};
        if (is_string(a[0])) {
            string p = as_obj<StringObj>(a[0])->str();
            if (::stat(((char*) c_string(p)), &st) != 0) syserror("stat", errno, p);
        } else if (is_port(a[0])) {
            auto* pt = as_obj<PortObj>(a[0]);
            if (::fstat(pt->fd, &st) != 0) syserror("stat", errno);
        } else {
            int fd = static_cast<int>(scm_to_int64(a[0], "stat", 1));
            if (::fstat(fd, &st) != 0) syserror("stat", errno);
        }
        return stat_to_vector(st);
    });
    define_proc("lstat", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("lstat", 1, a[0], "string");
        string p = as_obj<StringObj>(a[0])->str();
        struct stat st{};
        if (::lstat(((char*) c_string(p)), &st) != 0) syserror("lstat", errno, p);
        return stat_to_vector(st);
    });
    define_proc("stat:dev",     1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[0]; });
    define_proc("stat:ino",     1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[1]; });
    define_proc("stat:mode",    1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[2]; });
    define_proc("stat:nlink",   1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[3]; });
    define_proc("stat:uid",     1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[4]; });
    define_proc("stat:gid",     1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[5]; });
    define_proc("stat:rdev",    1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[6]; });
    define_proc("stat:size",    1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[7]; });
    define_proc("stat:atime",   1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[8]; });
    define_proc("stat:mtime",   1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[9]; });
    define_proc("stat:ctime",   1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[10]; });
    define_proc("stat:blksize", 1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[11]; });
    define_proc("stat:blocks",  1, 0, false, [](const SCM* a, size_t) -> SCM { return as_obj<VectorObj>(a[0])->elts[12]; });
    define_proc("stat:type",    1, 0, false, [this](const SCM* a, size_t) -> SCM {
        mode_t m = static_cast<mode_t>(scm_to_int64(as_obj<VectorObj>(a[0])->elts[2], "stat:type", 1));
        if (S_ISREG(m))  return make_symbol("regular");
        if (S_ISDIR(m))  return make_symbol("directory");
        if (S_ISLNK(m))  return make_symbol("symlink");
        if (S_ISBLK(m))  return make_symbol("block-special");
        if (S_ISCHR(m))  return make_symbol("char-special");
        if (S_ISFIFO(m)) return make_symbol("fifo");
        if (S_ISSOCK(m)) return make_symbol("socket");
        return make_symbol("unknown");
    });
    define_proc("stat:perms", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t m = scm_to_int64(as_obj<VectorObj>(a[0])->elts[2], "stat:perms", 1);
        return make_integer(m & 07777);
    });

    define_proc("chmod", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        mode_t m = static_cast<mode_t>(scm_to_int64(a[1], "chmod", 2));
        if (is_string(a[0])) {
            string p = as_obj<StringObj>(a[0])->str();
            if (::chmod(((char*) c_string(p)), m) != 0) syserror("chmod", errno, p);
        } else if (is_port(a[0])) {
            if (::fchmod(as_obj<PortObj>(a[0])->fd, m) != 0) syserror("chmod", errno);
        } else {
            if (::fchmod(static_cast<int>(scm_to_int64(a[0], "chmod", 1)), m) != 0) syserror("chmod", errno);
        }
        return SCM_UNSPECIFIED;
    });
    define_proc("chown", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        uid_t u = static_cast<uid_t>(scm_to_int64(a[1], "chown", 2));
        gid_t g = static_cast<gid_t>(scm_to_int64(a[2], "chown", 3));
        if (is_string(a[0])) {
            string p = as_obj<StringObj>(a[0])->str();
            if (::chown(((char*) c_string(p)), u, g) != 0) syserror("chown", errno, p);
        } else {
            int fd = is_port(a[0]) ? as_obj<PortObj>(a[0])->fd : static_cast<int>(scm_to_int64(a[0], "chown", 1));
            if (::fchown(fd, u, g) != 0) syserror("chown", errno);
        }
        return SCM_UNSPECIFIED;
    });
    define_proc("umask", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (nargs >= 1 && !a[0].is_undefined()) {
            mode_t m = static_cast<mode_t>(scm_to_int64(a[0], "umask", 1));
            return make_integer(::umask(m));
        }
        mode_t old = ::umask(0);
        ::umask(old);
        return make_integer(old);
    });
    define_proc("mkdir", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("mkdir", 1, a[0], "string");
        string p = as_obj<StringObj>(a[0])->str();
        mode_t m = (nargs >= 2 && !a[1].is_undefined()) ? static_cast<mode_t>(scm_to_int64(a[1], "mkdir", 2)) : 0777;
        if (::mkdir(((char*) c_string(p)), m) != 0) syserror("mkdir", errno, p);
        return SCM_UNSPECIFIED;
    });
    define_proc("rmdir", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("rmdir", 1, a[0], "string");
        string p = as_obj<StringObj>(a[0])->str();
        if (::rmdir(((char*) c_string(p))) != 0) syserror("rmdir", errno, p);
        return SCM_UNSPECIFIED;
    });
    define_proc("rename-file", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0]) || !is_string(a[1])) wrong_type_arg("rename-file", 1, a[0], "string");
        string p1 = as_obj<StringObj>(a[0])->str();
        string p2 = as_obj<StringObj>(a[1])->str();
        if (::rename(((char*) c_string(p1)), ((char*) c_string(p2))) != 0) syserror("rename-file", errno, p1);
        return SCM_UNSPECIFIED;
    });
    define_proc("delete-file", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("delete-file", 1, a[0], "string");
        string p = as_obj<StringObj>(a[0])->str();
        if (::unlink(((char*) c_string(p))) != 0) syserror("delete-file", errno, p);
        return SCM_UNSPECIFIED;
    });
    define_proc("link", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        string p1 = as_obj<StringObj>(a[0])->str();
        string p2 = as_obj<StringObj>(a[1])->str();
        if (::link(((char*) c_string(p1)), ((char*) c_string(p2))) != 0) syserror("link", errno, p1);
        return SCM_UNSPECIFIED;
    });
    define_proc("symlink", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        string p1 = as_obj<StringObj>(a[0])->str();
        string p2 = as_obj<StringObj>(a[1])->str();
        if (::symlink(((char*) c_string(p1)), ((char*) c_string(p2))) != 0) syserror("symlink", errno, p1);
        return SCM_UNSPECIFIED;
    });
    define_proc("readlink", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("readlink", 1, a[0], "string");
        string p = as_obj<StringObj>(a[0])->str();
        char buf[PATH_MAX];
        ssize_t n = ::readlink(((char*) c_string(p)), buf, sizeof(buf));
        if (n < 0) syserror("readlink", errno, p);
        return make_string(string(buf, static_cast<size_t>(n)));
    });
    define_proc("copy-file", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        string src = as_obj<StringObj>(a[0])->str();
        string dst = as_obj<StringObj>(a[1])->str();
        int in = ::open(((char*) c_string(src)), O_RDONLY);
        if (in < 0) syserror("copy-file", errno, src);
        struct stat st{};
        ::fstat(in, &st);
        int out = ::open(((char*) c_string(dst)), O_WRONLY | O_CREAT | O_TRUNC, st.st_mode & 0777);
        if (out < 0) { int e = errno; ::close(in); syserror("copy-file", e, dst); }
        char buf[8192];
        ssize_t n;
        while ((n = ::read(in, buf, sizeof(buf))) > 0) {
            if (::write(out, buf, static_cast<size_t>(n)) < 0) {
                int e = errno; ::close(in); ::close(out); syserror("copy-file", e, dst);
            }
        }
        ::close(in); ::close(out);
        return SCM_UNSPECIFIED;
    });
    define_proc("getcwd", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        char buf[PATH_MAX];
        if (!::getcwd(buf, sizeof(buf))) syserror("getcwd", errno);
        return make_string(buf);
    });
    define_proc("chdir", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("chdir", 1, a[0], "string");
        string p = as_obj<StringObj>(a[0])->str();
        if (::chdir(((char*) c_string(p))) != 0) syserror("chdir", errno, p);
        return SCM_UNSPECIFIED;
    });
    define_proc("access?", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("access?", 1, a[0], "string");
        string p = as_obj<StringObj>(a[0])->str();
        int mode = static_cast<int>(scm_to_int64(a[1], "access?", 2));
        return SCM::from_bool(::access(((char*) c_string(p)), mode) == 0);
    });
    define_proc("utime", 1, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("utime", 1, a[0], "string");
        string p = as_obj<StringObj>(a[0])->str();
        if (nargs < 2 || a[1].is_undefined()) {
            if (::utime(((char*) c_string(p)), nullptr) != 0) syserror("utime", errno, p);
        } else {
            struct utimbuf ub{};
            ub.actime = static_cast<time_t>(scm_to_int64(a[1], "utime", 2));
            ub.modtime = (nargs >= 3 && !a[2].is_undefined()) ? static_cast<time_t>(scm_to_int64(a[2], "utime", 3)) : ub.actime;
            if (::utime(((char*) c_string(p)), &ub) != 0) syserror("utime", errno, p);
        }
        return SCM_UNSPECIFIED;
    });
    define_proc("mkstemp!", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("mkstemp!", 1, a[0], "string");
        auto* so = as_obj<StringObj>(a[0]);
        string tmpl = so->str();
        array<char> buf = array_from<char>(str_data(tmpl), N(tmpl));
        (buf << '\0');
        int fd = ::mkstemp(gcpp_data(buf));
        if (fd < 0) syserror("mkstemp!", errno, tmpl);
        std::memcpy(so->mutable_data(), gcpp_data(buf), so->len);
        return make_file_port(fd, string(gcpp_data(buf)), "w+", true);
    });
    define_proc("tmpnam", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        char tmpl[] = "/tmp/guileXXXXXX";
        int fd = ::mkstemp(tmpl);
        if (fd >= 0) { ::close(fd); ::unlink(tmpl); }
        return make_string(tmpl);
    });
    define_proc("dirname", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("dirname", 1, a[0], "string");
        auto sv = as_obj<StringObj>(a[0])->str();
        size_t end = static_cast<size_t>(N(sv));
        while (end > 1 && sv[end - 1] == '/') --end;
        auto pos = str_rfind(sv, '/', end - 1);
        if (pos == static_cast<size_t>(-1)) return make_string(".");
        while (pos > 0 && sv[pos - 1] == '/') --pos;
        return make_string(pos == 0 ? "/" : str_sub(sv, 0, pos));
    });
    define_proc("basename", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("basename", 1, a[0], "string");
        auto sv = as_obj<StringObj>(a[0])->str();
        size_t end = static_cast<size_t>(N(sv));
        while (end > 1 && sv[end - 1] == '/') --end;
        auto pos = str_rfind(sv, '/', end - 1);
        auto base = (pos == static_cast<size_t>(-1)) ? str_sub(sv, 0, end) : (end == 1 && sv[0] == '/' ? str_sub(sv, 0, 1) : str_sub(sv, pos + 1, end - pos - 1));
        if (nargs >= 2 && !a[1].is_undefined() && is_string(a[1])) {
            auto suf = as_obj<StringObj>(a[1])->str();
            if (static_cast<size_t>(N(base)) >= static_cast<size_t>(N(suf)) && str_sub(base, static_cast<size_t>(N(base)) - static_cast<size_t>(N(suf))) == suf) {
                base = str_sub(base, 0, static_cast<size_t>(N(base)) - static_cast<size_t>(N(suf)));
            }
        }
        return make_string(base);
    });
    define_proc("canonicalize-path", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("canonicalize-path", 1, a[0], "string");
        string p = as_obj<StringObj>(a[0])->str();
        char buf[PATH_MAX];
        if (!::realpath(((char*) c_string(p)), buf)) syserror("canonicalize-path", errno, p);
        return make_string(buf);
    });

    // Directory streams
    define_proc("directory-stream?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(is_dir(a[0])); });
    define_proc("opendir", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("opendir", 1, a[0], "string");
        string p = as_obj<StringObj>(a[0])->str();
        DIR* d = ::opendir(((char*) c_string(p)));
        if (!d) syserror("opendir", errno, p);
        return heap_.alloc_scm<DirObj>(d);
    });
    define_proc("readdir", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_dir(a[0])) wrong_type_arg("readdir", 1, a[0], "directory stream");
        auto* d = as_obj<DirObj>(a[0]);
        if (d->closed || !d->dir) misc_error("readdir", "Directory stream is closed");
        struct dirent* ent = ::readdir(d->dir);
        if (!ent) return SCM_EOF;
        return make_string(ent->d_name);
    });
    define_proc("rewinddir", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_dir(a[0])) wrong_type_arg("rewinddir", 1, a[0], "directory stream");
        auto* d = as_obj<DirObj>(a[0]);
        if (d->closed || !d->dir) misc_error("rewinddir", "Directory stream is closed");
        ::rewinddir(d->dir);
        return SCM_UNSPECIFIED;
    });
    define_proc("closedir", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_dir(a[0])) wrong_type_arg("closedir", 1, a[0], "directory stream");
        auto* d = as_obj<DirObj>(a[0]);
        if (!d->closed && d->dir) { ::closedir(d->dir); d->dir = nullptr; d->closed = true; }
        return SCM_UNSPECIFIED;
    });

    // File descriptor operations
    define_proc("open-fdes", 2, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("open-fdes", 1, a[0], "string");
        string p = as_obj<StringObj>(a[0])->str();
        int flags = static_cast<int>(scm_to_int64(a[1], "open-fdes", 2));
        mode_t mode = (nargs >= 3 && !a[2].is_undefined()) ? static_cast<mode_t>(scm_to_int64(a[2], "open-fdes", 3)) : 0666;
        int fd = ::open(((char*) c_string(p)), flags, mode);
        if (fd < 0) syserror("open-fdes", errno, p);
        return make_integer(fd);
    });
    define_proc("close-fdes", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        int fd = static_cast<int>(scm_to_int64(a[0], "close-fdes", 1));
        if (::close(fd) != 0) syserror("close-fdes", errno);
        return SCM_UNSPECIFIED;
    });
    define_proc("dup->fdes", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        int oldfd = is_port(a[0]) ? as_obj<PortObj>(a[0])->fd : static_cast<int>(scm_to_int64(a[0], "dup->fdes", 1));
        int newfd = (nargs >= 2 && !a[1].is_undefined())
                    ? ::dup2(oldfd, static_cast<int>(scm_to_int64(a[1], "dup->fdes", 2)))
                    : ::dup(oldfd);
        if (newfd < 0) syserror("dup->fdes", errno);
        return make_integer(newfd);
    });
    define_proc("dup2", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        int fd1 = is_port(a[0]) ? as_obj<PortObj>(a[0])->fd : static_cast<int>(scm_to_int64(a[0], "dup2", 1));
        int fd2 = static_cast<int>(scm_to_int64(a[1], "dup2", 2));
        if (::dup2(fd1, fd2) < 0) syserror("dup2", errno);
        return SCM_UNSPECIFIED;
    });
    define_proc("fcntl", 2, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        int fd = is_port(a[0]) ? as_obj<PortObj>(a[0])->fd : static_cast<int>(scm_to_int64(a[0], "fcntl", 1));
        int cmd = static_cast<int>(scm_to_int64(a[1], "fcntl", 2));
        int r = (nargs >= 3 && !a[2].is_undefined())
                ? ::fcntl(fd, cmd, static_cast<int>(scm_to_int64(a[2], "fcntl", 3)))
                : ::fcntl(fd, cmd);
        if (r < 0) syserror("fcntl", errno);
        return make_integer(r);
    });
    define_proc("fsync", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        int fd = is_port(a[0]) ? as_obj<PortObj>(a[0])->fd : static_cast<int>(scm_to_int64(a[0], "fsync", 1));
        if (is_port(a[0])) port_flush(as_obj<PortObj>(a[0]));
        if (::fsync(fd) != 0) syserror("fsync", errno);
        return SCM_UNSPECIFIED;
    });
    define_proc("pipe", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        int fds[2];
        if (::pipe(fds) != 0) syserror("pipe", errno);
        return cons(make_file_port(fds[0], "pipe", "r", true),
                    make_file_port(fds[1], "pipe", "w", true));
    });

    // ========================================================================
    // Process / Environment / Signals (posix.c, simpos.c, scmsigs.c)
    // ========================================================================
    define_proc("getenv", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("getenv", 1, a[0], "string");
        const char* val = std::getenv(((char*) c_string(as_obj<StringObj>(a[0])->str())));
        return val ? make_string(val) : SCM_BOOL_F;
    });
    define_proc("putenv", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("putenv", 1, a[0], "string");
        string s = as_obj<StringObj>(a[0])->str();
        auto eq = str_find(s, '=');
        if (eq == static_cast<size_t>(-1)) {
            ::unsetenv(((char*) c_string(s)));
        } else {
            ::setenv(((char*) c_string(str_sub(s, 0, eq))), ((char*) c_string(str_sub(s, eq + 1))), 1);
        }
        return SCM_UNSPECIFIED;
    });
    define_proc("setenv", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("setenv", 1, a[0], "string");
        string k = as_obj<StringObj>(a[0])->str();
        if (a[1].is_false()) { ::unsetenv(((char*) c_string(k))); }
        else {
            if (!is_string(a[1])) wrong_type_arg("setenv", 2, a[1], "string or #f");
            ::setenv(((char*) c_string(k)), ((char*) c_string(as_obj<StringObj>(a[1])->str())), 1);
        }
        return SCM_UNSPECIFIED;
    });
    define_proc("environ", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (nargs >= 1 && !a[0].is_undefined()) {
            // Update environment from list of "KEY=VAL" strings
            SCM cur = a[0];
            while (is_pair(cur)) {
                if (is_string(car(cur))) {
                    string s = as_obj<StringObj>(car(cur))->str();
                    auto eq = str_find(s, '=');
                    if (eq != static_cast<size_t>(-1)) ::setenv(((char*) c_string(str_sub(s, 0, eq))), ((char*) c_string(str_sub(s, eq + 1))), 1);
                }
                cur = cdr(cur);
            }
            return SCM_UNSPECIFIED;
        }
        SCM res = SCM_EOL;
        if (environ) {
            for (char** p = environ; *p; ++p) res = cons(make_string(*p), res);
        }
        return scm_reverse_x(res);
    });
    define_proc("system", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (nargs == 0 || a[0].is_undefined()) return SCM::from_bool(std::system(nullptr) != 0);
        if (!is_string(a[0])) wrong_type_arg("system", 1, a[0], "string");
        flush_all_ports();
        return make_integer(std::system(((char*) c_string(as_obj<StringObj>(a[0])->str()))));
    });
    define_proc("primitive-fork", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        flush_all_ports();
        pid_t pid = ::fork();
        if (pid < 0) syserror("primitive-fork", errno);
        return make_integer(pid);
    });
    define_proc("waitpid", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        pid_t pid = static_cast<pid_t>(scm_to_int64(a[0], "waitpid", 1));
        int opts = (nargs >= 2 && !a[1].is_undefined()) ? static_cast<int>(scm_to_int64(a[1], "waitpid", 2)) : 0;
        int status = 0;
        pid_t r = ::waitpid(pid, &status, opts);
        if (r < 0) syserror("waitpid", errno);
        return cons(make_integer(r), make_integer(status));
    });
    define_proc("status:exit-val", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        int s = static_cast<int>(scm_to_int64(a[0], "status:exit-val", 1));
        return WIFEXITED(s) ? make_integer(WEXITSTATUS(s)) : SCM_BOOL_F;
    });
    define_proc("status:term-sig", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        int s = static_cast<int>(scm_to_int64(a[0], "status:term-sig", 1));
        return WIFSIGNALED(s) ? make_integer(WTERMSIG(s)) : SCM_BOOL_F;
    });
    define_proc("status:stop-sig", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        int s = static_cast<int>(scm_to_int64(a[0], "status:stop-sig", 1));
        return WIFSTOPPED(s) ? make_integer(WSTOPSIG(s)) : SCM_BOOL_F;
    });
    define_proc("kill", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        pid_t pid = static_cast<pid_t>(scm_to_int64(a[0], "kill", 1));
        int sig = static_cast<int>(scm_to_int64(a[1], "kill", 2));
        if (::kill(pid, sig) != 0) syserror("kill", errno);
        return SCM_UNSPECIFIED;
    });
    define_proc("getpid", 0, 0, false, [this](const SCM*, size_t) -> SCM { return make_integer(::getpid()); });
    define_proc("getppid", 0, 0, false, [this](const SCM*, size_t) -> SCM { return make_integer(::getppid()); });
    define_proc("getuid", 0, 0, false, [this](const SCM*, size_t) -> SCM { return make_integer(::getuid()); });
    define_proc("geteuid", 0, 0, false, [this](const SCM*, size_t) -> SCM { return make_integer(::geteuid()); });
    define_proc("getgid", 0, 0, false, [this](const SCM*, size_t) -> SCM { return make_integer(::getgid()); });
    define_proc("getegid", 0, 0, false, [this](const SCM*, size_t) -> SCM { return make_integer(::getegid()); });
    define_proc("getpgrp", 0, 0, false, [this](const SCM*, size_t) -> SCM { return make_integer(::getpgrp()); });
    define_proc("getgroups", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        gid_t gids[128];
        int n = ::getgroups(128, gids);
        if (n < 0) syserror("getgroups", errno);
        auto* v = heap_.alloc<VectorObj>(static_cast<size_t>(n), SCM::from_fixnum(0));
        for (int i = 0; i < n; ++i) v->elts[static_cast<size_t>(i)] = make_integer(gids[i]);
        return SCM::from_obj(v);
    });
    define_proc("getlogin", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        const char* l = ::getlogin();
        return l ? make_string(l) : SCM_BOOL_F;
    });
    define_proc("getpw", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        struct passwd* pw = nullptr;
        if (nargs == 0 || a[0].is_undefined()) { pw = ::getpwent(); }
        else if (is_string(a[0])) { pw = ::getpwnam(((char*) c_string(as_obj<StringObj>(a[0])->str()))); }
        else { pw = ::getpwuid(static_cast<uid_t>(scm_to_int64(a[0], "getpw", 1))); }
        if (!pw) misc_error("getpw", "entry not found");
        auto* v = heap_.alloc<VectorObj>(7, SCM_BOOL_F);
        v->elts[0] = make_string(pw->pw_name ? pw->pw_name : "");
        v->elts[1] = make_string(pw->pw_passwd ? pw->pw_passwd : "");
        v->elts[2] = make_integer(pw->pw_uid);
        v->elts[3] = make_integer(pw->pw_gid);
        v->elts[4] = make_string(pw->pw_gecos ? pw->pw_gecos : "");
        v->elts[5] = make_string(pw->pw_dir ? pw->pw_dir : "");
        v->elts[6] = make_string(pw->pw_shell ? pw->pw_shell : "");
        return SCM::from_obj(v);
    });
    define_proc("getgr", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        struct group* gr = nullptr;
        if (nargs == 0 || a[0].is_undefined()) { gr = ::getgrent(); }
        else if (is_string(a[0])) { gr = ::getgrnam(((char*) c_string(as_obj<StringObj>(a[0])->str()))); }
        else { gr = ::getgrgid(static_cast<gid_t>(scm_to_int64(a[0], "getgr", 1))); }
        if (!gr) misc_error("getgr", "entry not found");
        auto* v = heap_.alloc<VectorObj>(4, SCM_BOOL_F);
        v->elts[0] = make_string(gr->gr_name ? gr->gr_name : "");
        v->elts[1] = make_string(gr->gr_passwd ? gr->gr_passwd : "");
        v->elts[2] = make_integer(gr->gr_gid);
        SCM mems = SCM_EOL;
        if (gr->gr_mem) {
            for (char** p = gr->gr_mem; *p; ++p) mems = cons(make_string(*p), mems);
        }
        v->elts[3] = scm_reverse_x(mems);
        return SCM::from_obj(v);
    });
    define_proc("uname", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        struct utsname u{};
        if (::uname(&u) != 0) syserror("uname", errno);
        auto* v = heap_.alloc<VectorObj>(5, SCM_BOOL_F);
        v->elts[0] = make_string(u.sysname);
        v->elts[1] = make_string(u.nodename);
        v->elts[2] = make_string(u.release);
        v->elts[3] = make_string(u.version);
        v->elts[4] = make_string(u.machine);
        return SCM::from_obj(v);
    });
    define_proc("gethostname", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        char buf[256];
        if (::gethostname(buf, sizeof(buf)) != 0) syserror("gethostname", errno);
        buf[sizeof(buf) - 1] = '\0';
        return make_string(buf);
    });
    define_proc("sleep", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        unsigned int s = static_cast<unsigned int>(scm_to_uint64(a[0], "sleep", 1));
        return make_uinteger(::sleep(s));
    });
    define_proc("usleep", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        useconds_t us = static_cast<useconds_t>(scm_to_uint64(a[0], "usleep", 1));
        ::usleep(us);
        return make_integer(0);
    });
    define_proc("alarm", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return make_uinteger(::alarm(static_cast<unsigned int>(scm_to_uint64(a[0], "alarm", 1))));
    });
    define_proc("raise", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        int sig = static_cast<int>(scm_to_int64(a[0], "raise", 1));
        if (::raise(sig) != 0) syserror("raise", errno);
        return SCM_UNSPECIFIED;
    });
    define_proc("sigaction", 1, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        int sig = static_cast<int>(scm_to_int64(a[0], "sigaction", 1));
        struct sigaction old{};
        if (nargs == 1 || a[1].is_undefined()) {
            if (::sigaction(sig, nullptr, &old) != 0) syserror("sigaction", errno);
            return cons(make_integer(reinterpret_cast<intptr_t>(old.sa_handler)), make_integer(old.sa_flags));
        }
        struct sigaction sa{};
        if (is_exact_integer(a[1])) {
            int64_t h = scm_to_int64(a[1], "sigaction", 2);
            if (h != reinterpret_cast<intptr_t>(SIG_DFL) && h != reinterpret_cast<intptr_t>(SIG_IGN)) {
                out_of_range("sigaction", a[1]);
            }
            sa.sa_handler = reinterpret_cast<void(*)(int)>(static_cast<intptr_t>(h));
        } else if (a[1].is_false()) {
            sa.sa_handler = SIG_DFL;
        } else if (is_procedure(a[1])) {
            sa.sa_handler = SIG_DFL;
        } else {
            wrong_type_arg("sigaction", 2, a[1], "procedure or integer");
        }
        sa.sa_flags = (nargs >= 3 && !a[2].is_undefined()) ? static_cast<int>(scm_to_int64(a[2], "sigaction", 3)) : 0;
        sigemptyset(&sa.sa_mask);
        if (::sigaction(sig, &sa, &old) != 0) syserror("sigaction", errno);
        return cons(make_integer(reinterpret_cast<intptr_t>(old.sa_handler)), make_integer(old.sa_flags));
    });
    define_proc("restore-signals", 0, 0, false, [](const SCM*, size_t) -> SCM { return SCM_UNSPECIFIED; });

    // POSIX constants
    define("O_RDONLY", make_integer(O_RDONLY));
    define("O_WRONLY", make_integer(O_WRONLY));
    // exec family: execl, execlp, execle
    define_proc("execl", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("execl", 1, a[0], "string");
        string path = as_obj<StringObj>(a[0])->str();
        array<string> storage;
        SCM rest = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(rest)) {
            if (!is_string(car(rest))) wrong_type_arg("execl", 2, car(rest), "string");
            (storage << as_obj<StringObj>(car(rest))->str());
            rest = cdr(rest);
        }
        array<char*> argv;
        for (auto& s : storage) (argv << gcpp_data(s));
        (argv << static_cast<char*>(nullptr));
        ::execv(((char*) c_string(path)), gcpp_data(argv));
        syserror("execl", errno);
    });
    define_proc("execlp", 1, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("execlp", 1, a[0], "string");
        string file = as_obj<StringObj>(a[0])->str();
        array<string> storage;
        SCM rest = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(rest)) {
            if (!is_string(car(rest))) wrong_type_arg("execlp", 2, car(rest), "string");
            (storage << as_obj<StringObj>(car(rest))->str());
            rest = cdr(rest);
        }
        array<char*> argv;
        for (auto& s : storage) (argv << gcpp_data(s));
        (argv << static_cast<char*>(nullptr));
        ::execvp(((char*) c_string(file)), gcpp_data(argv));
        syserror("execlp", errno);
    });
    define_proc("execle", 2, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("execle", 1, a[0], "string");
        string path = as_obj<StringObj>(a[0])->str();
        array<string> env_storage;
        for (SCM e = a[1]; is_pair(e); e = cdr(e)) {
            if (!is_string(car(e))) wrong_type_arg("execle", 2, car(e), "list of strings");
            (env_storage << as_obj<StringObj>(car(e))->str());
        }
        array<string> arg_storage;
        SCM rest = nargs > 2 ? a[2] : SCM_EOL;
        while (is_pair(rest)) {
            if (!is_string(car(rest))) wrong_type_arg("execle", 3, car(rest), "string");
            (arg_storage << as_obj<StringObj>(car(rest))->str());
            rest = cdr(rest);
        }
        array<char*> argv, envp;
        for (auto& s : arg_storage) (argv << gcpp_data(s));
        (argv << static_cast<char*>(nullptr));
        for (auto& s : env_storage) (envp << gcpp_data(s));
        (envp << static_cast<char*>(nullptr));
        ::execve(((char*) c_string(path)), gcpp_data(argv), gcpp_data(envp));
        syserror("execle", errno);
    });

    define("O_RDWR",   make_integer(O_RDWR));
    define("O_CREAT",  make_integer(O_CREAT));
    define("O_EXCL",   make_integer(O_EXCL));
    define("O_NOCTTY", make_integer(O_NOCTTY));
    define("O_TRUNC",  make_integer(O_TRUNC));
    define("O_APPEND", make_integer(O_APPEND));
    define("O_NONBLOCK", make_integer(O_NONBLOCK));
    define("F_DUPFD",  make_integer(F_DUPFD));
    define("F_GETFD",  make_integer(F_GETFD));
    define("F_SETFD",  make_integer(F_SETFD));
    define("F_GETFL",  make_integer(F_GETFL));
    define("F_SETFL",  make_integer(F_SETFL));
    define("FD_CLOEXEC", make_integer(FD_CLOEXEC));
    define("R_OK", make_integer(R_OK));
    define("W_OK", make_integer(W_OK));
    define("X_OK", make_integer(X_OK));
    define("F_OK", make_integer(F_OK));
    define("WNOHANG", make_integer(WNOHANG));
    define("WUNTRACED", make_integer(WUNTRACED));

    define("SIG_DFL", make_integer(reinterpret_cast<intptr_t>(SIG_DFL)));
    define("SIG_IGN", make_integer(reinterpret_cast<intptr_t>(SIG_IGN)));
    define("SIGHUP",  make_integer(SIGHUP));
    define("SIGINT",  make_integer(SIGINT));
    define("SIGQUIT", make_integer(SIGQUIT));
    define("SIGILL",  make_integer(SIGILL));
    define("SIGABRT", make_integer(SIGABRT));
    define("SIGFPE",  make_integer(SIGFPE));
    define("SIGKILL", make_integer(SIGKILL));
    define("SIGSEGV", make_integer(SIGSEGV));
    define("SIGPIPE", make_integer(SIGPIPE));
    define("SIGALRM", make_integer(SIGALRM));
    define("SIGTERM", make_integer(SIGTERM));
    define("SIGUSR1", make_integer(SIGUSR1));
    define("SIGUSR2", make_integer(SIGUSR2));
    define("SIGCHLD", make_integer(SIGCHLD));
    define("SIGCONT", make_integer(SIGCONT));
    define("SIGSTOP", make_integer(SIGSTOP));
    define("SIGTSTP", make_integer(SIGTSTP));

    // Standard errno symbols
    define("EPERM", make_integer(EPERM));
    define("ENOENT", make_integer(ENOENT));
    define("ESRCH", make_integer(ESRCH));
    define("EINTR", make_integer(EINTR));
    define("EIO", make_integer(EIO));
    define("ENXIO", make_integer(ENXIO));
    define("E2BIG", make_integer(E2BIG));
    define("ENOEXEC", make_integer(ENOEXEC));
    define("EBADF", make_integer(EBADF));
    define("ECHILD", make_integer(ECHILD));
    define("EAGAIN", make_integer(EAGAIN));
    define("ENOMEM", make_integer(ENOMEM));
    define("EACCES", make_integer(EACCES));
    define("EFAULT", make_integer(EFAULT));
    define("EBUSY", make_integer(EBUSY));
    define("EEXIST", make_integer(EEXIST));
    define("EXDEV", make_integer(EXDEV));
    define("ENODEV", make_integer(ENODEV));
    define("ENOTDIR", make_integer(ENOTDIR));
    define("EISDIR", make_integer(EISDIR));
    define("EINVAL", make_integer(EINVAL));
    define("ENFILE", make_integer(ENFILE));
    define("EMFILE", make_integer(EMFILE));
    define("ENOTTY", make_integer(ENOTTY));
    define("EFBIG", make_integer(EFBIG));
    define("ENOSPC", make_integer(ENOSPC));
    define("ESPIPE", make_integer(ESPIPE));
    define("EROFS", make_integer(EROFS));
    define("EMLINK", make_integer(EMLINK));
    define("EPIPE", make_integer(EPIPE));
    define("EDOM", make_integer(EDOM));
    define("ERANGE", make_integer(ERANGE));
    define("ENAMETOOLONG", make_integer(ENAMETOOLONG));
    define("ENOSYS", make_integer(ENOSYS));
    define("ENOTEMPTY", make_integer(ENOTEMPTY));
    define("ELOOP", make_integer(ELOOP));
    define("EWOULDBLOCK", make_integer(EWOULDBLOCK));

    // POSIX select (reads writes excepts [secs [usecs]]) -> (ready-reads ready-writes ready-excepts)
    define_proc("select", 3, 2, false, [this](const SCM* a, size_t nargs) -> SCM {
        fd_set rfds, wfds, efds;
        FD_ZERO(&rfds);
        FD_ZERO(&wfds);
        FD_ZERO(&efds);
        int max_fd = -1;

        auto item_fd = [this](SCM x, int pos) -> int {
            if (is_port(x)) {
                auto* pt = as_obj<PortObj>(x);
                if (pt->closed || pt->fd < 0) wrong_type_arg("select", pos, x, "open file port");
                return pt->fd;
            }
            return static_cast<int>(scm_to_int64(x, "select", pos));
        };

        auto fill_set = [&](SCM col, fd_set* fds, int pos) {
            if (is_vector(col)) {
                for (SCM item : as_obj<VectorObj>(col)->elts) {
                    int fd = item_fd(item, pos);
                    if (fd >= 0 && fd < FD_SETSIZE) {
                        FD_SET(fd, fds);
                        if (fd > max_fd) max_fd = fd;
                    }
                }
            } else {
                SCM cur = col;
                while (is_pair(cur)) {
                    int fd = item_fd(car(cur), pos);
                    if (fd >= 0 && fd < FD_SETSIZE) {
                        FD_SET(fd, fds);
                        if (fd > max_fd) max_fd = fd;
                    }
                    cur = cdr(cur);
                }
                if (!cur.is_null()) wrong_type_arg("select", pos, col, "list or vector");
            }
        };

        fill_set(a[0], &rfds, 1);
        fill_set(a[1], &wfds, 2);
        fill_set(a[2], &efds, 3);

        struct timeval tv{};
        struct timeval* tvp = nullptr;
        if (nargs >= 4 && !a[3].is_undefined() && !a[3].is_false()) {
            tv.tv_sec = static_cast<time_t>(scm_to_int64(a[3], "select", 4));
            if (nargs >= 5 && !a[4].is_undefined() && !a[4].is_false()) {
                tv.tv_usec = static_cast<suseconds_t>(scm_to_int64(a[4], "select", 5));
            }
            tvp = &tv;
        }

        int rc = ::select(max_fd + 1, &rfds, &wfds, &efds, tvp);
        if (rc < 0) syserror("select", errno);

        auto collect = [&](SCM col, const fd_set* fds, int pos) -> SCM {
            array<SCM> out;
            if (is_vector(col)) {
                for (SCM item : as_obj<VectorObj>(col)->elts) {
                    int fd = item_fd(item, pos);
                    if (fd >= 0 && fd < FD_SETSIZE && FD_ISSET(fd, fds)) (out << item);
                }
            } else {
                SCM cur = col;
                while (is_pair(cur)) {
                    SCM item = car(cur);
                    int fd = item_fd(item, pos);
                    if (fd >= 0 && fd < FD_SETSIZE && FD_ISSET(fd, fds)) (out << item);
                    cur = cdr(cur);
                }
            }
            return list_n(gcpp_data(out), static_cast<size_t>(N(out)));
        };

        return list3(collect(a[0], &rfds, 1), collect(a[1], &wfds, 2), collect(a[2], &efds, 3));
    });
}

} // namespace guile
