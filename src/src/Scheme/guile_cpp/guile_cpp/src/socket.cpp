#include "guile_cpp/vm.hpp"
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <cerrno>
#include <cstring>

namespace guile {

namespace {

int port_or_fd(SCM x, const char* subr, int pos) {
    if (is_port(x)) return as_obj<PortObj>(x)->fd;
    return static_cast<int>(scm_to_int64(x, subr, pos));
}

void bigint_to_ipv6_bytes(SCM num, uint8_t out[16], const char* subr, int pos) {
    uint64_t scratch = 0;
    BigIntView z = scm_to_bigint_view(num, scratch, subr, pos);
    if (!BigInt::to_be_bytes(z, out, 16)) {
        vm().out_of_range(subr, num);
    }
}

SCM ipv6_bytes_to_scm(const uint8_t in[16]) {
    return scm_from_bigint(BigInt::from_be_bytes(in, 16));
}

// Convert either a socket-address vector OR (fam arg1 [arg2 ...]) into sockaddr_storage
socklen_t fill_sockaddr(const SCM* args, size_t nargs, struct sockaddr_storage* ss, const char* subr) {
    std::memset(ss, 0, sizeof(*ss));
    std::vector<SCM> fields;
    if (nargs == 1 && is_vector(args[0])) {
        auto* vec = as_obj<VectorObj>(args[0]);
        fields = vec->elts;
    } else {
        fields.assign(args, args + nargs);
    }
    if (fields.empty()) vm().wrong_num_args(subr);
    int fam = static_cast<int>(scm_to_int64(fields[0], subr, 1));
    if (fam == AF_INET) {
        if (fields.size() < 3) vm().wrong_num_args(subr);
        auto* sin = reinterpret_cast<struct sockaddr_in*>(ss);
        sin->sin_family = AF_INET;
        uint64_t addr = scm_to_uint64(fields[1], subr, 2);
        if (addr > 0xFFFFFFFFULL) vm().out_of_range(subr, fields[1]);
        uint64_t port = scm_to_uint64(fields[2], subr, 3);
        if (port > 0xFFFFULL) vm().out_of_range(subr, fields[2]);
        sin->sin_addr.s_addr = htonl(static_cast<uint32_t>(addr));
        sin->sin_port = htons(static_cast<uint16_t>(port));
        return sizeof(struct sockaddr_in);
    }
    if (fam == AF_INET6) {
        if (fields.size() < 3) vm().wrong_num_args(subr);
        auto* sin6 = reinterpret_cast<struct sockaddr_in6*>(ss);
        sin6->sin6_family = AF_INET6;
        bigint_to_ipv6_bytes(fields[1], sin6->sin6_addr.s6_addr, subr, 2);
        uint64_t port = scm_to_uint64(fields[2], subr, 3);
        if (port > 0xFFFFULL) vm().out_of_range(subr, fields[2]);
        sin6->sin6_port = htons(static_cast<uint16_t>(port));
        if (fields.size() >= 4) sin6->sin6_flowinfo = static_cast<uint32_t>(scm_to_uint64(fields[3], subr, 4));
        if (fields.size() >= 5) sin6->sin6_scope_id = static_cast<uint32_t>(scm_to_uint64(fields[4], subr, 5));
        return sizeof(struct sockaddr_in6);
    }
    if (fam == AF_UNIX) {
        if (fields.size() < 2) vm().wrong_num_args(subr);
        if (!is_string(fields[1])) vm().wrong_type_arg(subr, 2, fields[1], "string");
        auto* sun = reinterpret_cast<struct sockaddr_un*>(ss);
        sun->sun_family = AF_UNIX;
        std::string path = as_obj<StringObj>(fields[1])->to_std_string();
        if (path.size() >= sizeof(sun->sun_path)) {
            vm().misc_error(subr, "unix address path too long: ~A", vm().list1(fields[1]));
        }
        std::memcpy(sun->sun_path, path.c_str(), path.size() + 1);
        return static_cast<socklen_t>(offsetof(struct sockaddr_un, sun_path) + path.size() + 1);
    }
    vm().misc_error(subr, "unsupported address family");
}

SCM sockaddr_to_scm(const struct sockaddr* sa, socklen_t len) {
    auto& v = vm();
    if (!sa || len == 0) return v.make_vector(1, v.make_integer(AF_UNSPEC));
    if (sa->sa_family == AF_INET) {
        const auto* sin = reinterpret_cast<const struct sockaddr_in*>(sa);
        auto* vec = v.heap().alloc<VectorObj>(3, SCM_BOOL_F);
        vec->elts[0] = v.make_integer(AF_INET);
        vec->elts[1] = v.make_uinteger(ntohl(sin->sin_addr.s_addr));
        vec->elts[2] = v.make_uinteger(ntohs(sin->sin_port));
        return SCM::from_obj(vec);
    }
    if (sa->sa_family == AF_INET6) {
        const auto* sin6 = reinterpret_cast<const struct sockaddr_in6*>(sa);
        auto* vec = v.heap().alloc<VectorObj>(5, SCM_BOOL_F);
        vec->elts[0] = v.make_integer(AF_INET6);
        vec->elts[1] = ipv6_bytes_to_scm(sin6->sin6_addr.s6_addr);
        vec->elts[2] = v.make_uinteger(ntohs(sin6->sin6_port));
        vec->elts[3] = v.make_uinteger(sin6->sin6_flowinfo);
        vec->elts[4] = v.make_uinteger(sin6->sin6_scope_id);
        return SCM::from_obj(vec);
    }
    if (sa->sa_family == AF_UNIX) {
        const auto* sun = reinterpret_cast<const struct sockaddr_un*>(sa);
        auto* vec = v.heap().alloc<VectorObj>(2, SCM_BOOL_F);
        vec->elts[0] = v.make_integer(AF_UNIX);
        size_t max_len = (len > offsetof(struct sockaddr_un, sun_path)) ? (len - offsetof(struct sockaddr_un, sun_path)) : 0;
        size_t slen = ::strnlen(sun->sun_path, max_len);
        vec->elts[1] = v.make_string(std::string_view(sun->sun_path, slen));
        return SCM::from_obj(vec);
    }
    return v.make_vector(1, v.make_integer(sa->sa_family));
}

} // namespace

void VM::init_socket() {
    define("AF_UNSPEC", make_integer(AF_UNSPEC));
    define("AF_UNIX",   make_integer(AF_UNIX));
    define("AF_INET",   make_integer(AF_INET));
    define("AF_INET6",  make_integer(AF_INET6));
    define("PF_UNSPEC", make_integer(PF_UNSPEC));
    define("PF_UNIX",   make_integer(PF_UNIX));
    define("PF_INET",   make_integer(PF_INET));
    define("PF_INET6",  make_integer(PF_INET6));
    define("SOCK_STREAM", make_integer(SOCK_STREAM));
    define("SOCK_DGRAM",  make_integer(SOCK_DGRAM));
    define("SOCK_RAW",    make_integer(SOCK_RAW));
    define("SOL_SOCKET",  make_integer(SOL_SOCKET));
    define("SO_REUSEADDR", make_integer(SO_REUSEADDR));
    define("SO_KEEPALIVE", make_integer(SO_KEEPALIVE));
    define("SO_BROADCAST", make_integer(SO_BROADCAST));
    define("SO_LINGER",    make_integer(SO_LINGER));
    define("SO_SNDBUF",    make_integer(SO_SNDBUF));
    define("SO_RCVBUF",    make_integer(SO_RCVBUF));
    define("SO_ERROR",     make_integer(SO_ERROR));
    define("SO_TYPE",      make_integer(SO_TYPE));
    define("INADDR_ANY",       make_uinteger(INADDR_ANY));
    define("INADDR_LOOPBACK",  make_uinteger(INADDR_LOOPBACK));
    define("INADDR_BROADCAST", make_uinteger(INADDR_BROADCAST));
    define("INADDR_NONE",      make_uinteger(INADDR_NONE));
    define("MSG_OOB",      make_integer(MSG_OOB));
    define("MSG_PEEK",     make_integer(MSG_PEEK));
    define("MSG_DONTROUTE", make_integer(MSG_DONTROUTE));
    define("SHUT_RD",   make_integer(SHUT_RD));
    define("SHUT_WR",   make_integer(SHUT_WR));
    define("SHUT_RDWR", make_integer(SHUT_RDWR));

    define_proc("htons", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        uint64_t v = scm_to_uint64(a[0], "htons", 1);
        if (v > 0xFFFFULL) out_of_range("htons", a[0]);
        return make_uinteger(htons(static_cast<uint16_t>(v)));
    });
    define_proc("ntohs", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        uint64_t v = scm_to_uint64(a[0], "ntohs", 1);
        if (v > 0xFFFFULL) out_of_range("ntohs", a[0]);
        return make_uinteger(ntohs(static_cast<uint16_t>(v)));
    });
    define_proc("htonl", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        uint64_t v = scm_to_uint64(a[0], "htonl", 1);
        if (v > 0xFFFFFFFFULL) out_of_range("htonl", a[0]);
        return make_uinteger(htonl(static_cast<uint32_t>(v)));
    });
    define_proc("ntohl", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        uint64_t v = scm_to_uint64(a[0], "ntohl", 1);
        if (v > 0xFFFFFFFFULL) out_of_range("ntohl", a[0]);
        return make_uinteger(ntohl(static_cast<uint32_t>(v)));
    });
    define_proc("inet-aton", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("inet-aton", 1, a[0], "string");
        struct in_addr addr{};
        if (::inet_aton(as_obj<StringObj>(a[0])->to_std_string().c_str(), &addr) == 0) return SCM_BOOL_F;
        return make_uinteger(ntohl(addr.s_addr));
    });
    define_proc("inet-ntoa", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        uint64_t v = scm_to_uint64(a[0], "inet-ntoa", 1);
        if (v > 0xFFFFFFFFULL) out_of_range("inet-ntoa", a[0]);
        struct in_addr addr{};
        addr.s_addr = htonl(static_cast<uint32_t>(v));
        return make_string(::inet_ntoa(addr));
    });
    define_proc("inet-pton", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        int af = static_cast<int>(scm_to_int64(a[0], "inet-pton", 1));
        if (!is_string(a[1])) wrong_type_arg("inet-pton", 2, a[1], "string");
        std::string s = as_obj<StringObj>(a[1])->to_std_string();
        if (af == AF_INET) {
            struct in_addr addr{};
            if (::inet_pton(af, s.c_str(), &addr) != 1) misc_error("inet-pton", "invalid IPv4 address");
            return make_uinteger(ntohl(addr.s_addr));
        }
        if (af == AF_INET6) {
            uint8_t buf[16];
            if (::inet_pton(af, s.c_str(), buf) != 1) misc_error("inet-pton", "invalid IPv6 address");
            return ipv6_bytes_to_scm(buf);
        }
        misc_error("inet-pton", "unsupported address family");
    });
    define_proc("inet-ntop", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        int af = static_cast<int>(scm_to_int64(a[0], "inet-ntop", 1));
        char buf[INET6_ADDRSTRLEN];
        if (af == AF_INET) {
            uint64_t v = scm_to_uint64(a[1], "inet-ntop", 2);
            if (v > 0xFFFFFFFFULL) out_of_range("inet-ntop", a[1]);
            struct in_addr addr{};
            addr.s_addr = htonl(static_cast<uint32_t>(v));
            if (!::inet_ntop(af, &addr, buf, sizeof(buf))) syserror("inet-ntop", errno);
            return make_string(buf);
        }
        if (af == AF_INET6) {
            uint8_t raw[16]{};
            bigint_to_ipv6_bytes(a[1], raw, "inet-ntop", 2);
            if (!::inet_ntop(af, raw, buf, sizeof(buf))) syserror("inet-ntop", errno);
            return make_string(buf);
        }
        misc_error("inet-ntop", "unsupported address family");
    });

    // Socket address constructors & accessors
    define_proc("make-socket-address", 1, 0, true, [](const SCM* a, size_t nargs) -> SCM {
        std::vector<SCM> all;
        all.push_back(a[0]);
        SCM rest = nargs > 1 ? a[1] : SCM_EOL;
        while (is_pair(rest)) { all.push_back(car(rest)); rest = cdr(rest); }
        struct sockaddr_storage ss{};
        socklen_t len = fill_sockaddr(all.data(), all.size(), &ss, "make-socket-address");
        return sockaddr_to_scm(reinterpret_cast<const struct sockaddr*>(&ss), len);
    });
    define_proc("sockaddr:fam", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_vector(a[0]) || as_obj<VectorObj>(a[0])->elts.empty()) wrong_type_arg("sockaddr:fam", 1, a[0], "socket address");
        return as_obj<VectorObj>(a[0])->elts[0];
    });
    define_proc("sockaddr:addr", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_vector(a[0]) || as_obj<VectorObj>(a[0])->elts.size() < 2) wrong_type_arg("sockaddr:addr", 1, a[0], "socket address");
        return as_obj<VectorObj>(a[0])->elts[1];
    });
    define_proc("sockaddr:port", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_vector(a[0]) || as_obj<VectorObj>(a[0])->elts.size() < 3) wrong_type_arg("sockaddr:port", 1, a[0], "socket address");
        return as_obj<VectorObj>(a[0])->elts[2];
    });
    define_proc("sockaddr:flowinfo", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_vector(a[0]) || as_obj<VectorObj>(a[0])->elts.size() < 4) wrong_type_arg("sockaddr:flowinfo", 1, a[0], "socket address");
        return as_obj<VectorObj>(a[0])->elts[3];
    });
    define_proc("sockaddr:scopeid", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_vector(a[0]) || as_obj<VectorObj>(a[0])->elts.size() < 5) wrong_type_arg("sockaddr:scopeid", 1, a[0], "socket address");
        return as_obj<VectorObj>(a[0])->elts[4];
    });
    define_proc("sockaddr:path", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_vector(a[0]) || as_obj<VectorObj>(a[0])->elts.size() < 2) wrong_type_arg("sockaddr:path", 1, a[0], "socket address");
        return as_obj<VectorObj>(a[0])->elts[1];
    });

    define_proc("socket", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        int dom = static_cast<int>(scm_to_int64(a[0], "socket", 1));
        int type = static_cast<int>(scm_to_int64(a[1], "socket", 2));
        int proto = static_cast<int>(scm_to_int64(a[2], "socket", 3));
        int fd = ::socket(dom, type, proto);
        if (fd < 0) syserror("socket", errno);
        return make_file_port(fd, "socket", "r+0", true);
    });
    define_proc("socketpair", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        int dom = static_cast<int>(scm_to_int64(a[0], "socketpair", 1));
        int type = static_cast<int>(scm_to_int64(a[1], "socketpair", 2));
        int proto = static_cast<int>(scm_to_int64(a[2], "socketpair", 3));
        int fds[2];
        if (::socketpair(dom, type, proto, fds) != 0) syserror("socketpair", errno);
        return cons(make_file_port(fds[0], "socket", "r+0", true),
                    make_file_port(fds[1], "socket", "r+0", true));
    });
    define_proc("bind", 2, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        int fd = port_or_fd(a[0], "bind", 1);
        std::vector<SCM> addr_args;
        addr_args.push_back(a[1]);
        SCM rest = nargs > 2 ? a[2] : SCM_EOL;
        while (is_pair(rest)) { addr_args.push_back(car(rest)); rest = cdr(rest); }
        struct sockaddr_storage ss{};
        socklen_t len = fill_sockaddr(addr_args.data(), addr_args.size(), &ss, "bind");
        if (::bind(fd, reinterpret_cast<const struct sockaddr*>(&ss), len) != 0) syserror("bind", errno);
        return SCM_UNSPECIFIED;
    });
    define_proc("listen", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        int fd = port_or_fd(a[0], "listen", 1);
        int backlog = static_cast<int>(scm_to_int64(a[1], "listen", 2));
        if (::listen(fd, backlog) != 0) syserror("listen", errno);
        return SCM_UNSPECIFIED;
    });
    define_proc("accept", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        int fd = port_or_fd(a[0], "accept", 1);
        struct sockaddr_storage ss{};
        socklen_t len = sizeof(ss);
        int nfd = ::accept(fd, reinterpret_cast<struct sockaddr*>(&ss), &len);
        if (nfd < 0) syserror("accept", errno);
        return cons(make_file_port(nfd, "socket", "r+0", true),
                    sockaddr_to_scm(reinterpret_cast<const struct sockaddr*>(&ss), len));
    });
    define_proc("connect", 2, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        int fd = port_or_fd(a[0], "connect", 1);
        std::vector<SCM> addr_args;
        addr_args.push_back(a[1]);
        SCM rest = nargs > 2 ? a[2] : SCM_EOL;
        while (is_pair(rest)) { addr_args.push_back(car(rest)); rest = cdr(rest); }
        struct sockaddr_storage ss{};
        socklen_t len = fill_sockaddr(addr_args.data(), addr_args.size(), &ss, "connect");
        if (::connect(fd, reinterpret_cast<const struct sockaddr*>(&ss), len) != 0) syserror("connect", errno);
        return SCM_UNSPECIFIED;
    });
    define_proc("sendto", 3, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        int fd = port_or_fd(a[0], "sendto", 1);
        if (!is_string(a[1])) wrong_type_arg("sendto", 2, a[1], "string");
        auto sv = as_obj<StringObj>(a[1])->view();
        std::vector<SCM> addr_args;
        addr_args.push_back(a[2]);
        SCM rest = nargs > 3 ? a[3] : SCM_EOL;
        while (is_pair(rest)) { addr_args.push_back(car(rest)); rest = cdr(rest); }
        int flags = 0;
        // Optional flags if sockaddr vector followed by int: (sendto sock msg sockaddr [flags])
        if (addr_args.size() >= 2 && is_vector(addr_args[0])) {
            flags = static_cast<int>(scm_to_int64(addr_args[1], "sendto", 4));
            addr_args.resize(1);
        }
        struct sockaddr_storage ss{};
        socklen_t len = fill_sockaddr(addr_args.data(), addr_args.size(), &ss, "sendto");
        ssize_t n = ::sendto(fd, sv.data(), sv.size(), flags, reinterpret_cast<const struct sockaddr*>(&ss), len);
        if (n < 0) syserror("sendto", errno);
        return make_integer(n);
    });
    define_proc("getsockname", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        int fd = port_or_fd(a[0], "getsockname", 1);
        struct sockaddr_storage ss{};
        socklen_t len = sizeof(ss);
        if (::getsockname(fd, reinterpret_cast<struct sockaddr*>(&ss), &len) != 0) syserror("getsockname", errno);
        return sockaddr_to_scm(reinterpret_cast<const struct sockaddr*>(&ss), len);
    });
    define_proc("getpeername", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        int fd = port_or_fd(a[0], "getpeername", 1);
        struct sockaddr_storage ss{};
        socklen_t len = sizeof(ss);
        if (::getpeername(fd, reinterpret_cast<struct sockaddr*>(&ss), &len) != 0) syserror("getpeername", errno);
        return sockaddr_to_scm(reinterpret_cast<const struct sockaddr*>(&ss), len);
    });
    define_proc("shutdown", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        int fd = port_or_fd(a[0], "shutdown", 1);
        int how = static_cast<int>(scm_to_int64(a[1], "shutdown", 2));
        if (::shutdown(fd, how) != 0) syserror("shutdown", errno);
        return SCM_UNSPECIFIED;
    });
    define_proc("recv!", 2, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        int fd = port_or_fd(a[0], "recv!", 1);
        if (!is_string(a[1])) wrong_type_arg("recv!", 2, a[1], "string");
        int flags = (nargs >= 3 && !a[2].is_undefined()) ? static_cast<int>(scm_to_int64(a[2], "recv!", 3)) : 0;
        auto* so = as_obj<StringObj>(a[1]);
        ssize_t n = ::recv(fd, so->mutable_data(), so->len, flags);
        if (n < 0) syserror("recv!", errno);
        return make_integer(n);
    });
    define_proc("send", 2, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        int fd = port_or_fd(a[0], "send", 1);
        if (!is_string(a[1])) wrong_type_arg("send", 2, a[1], "string");
        int flags = (nargs >= 3 && !a[2].is_undefined()) ? static_cast<int>(scm_to_int64(a[2], "send", 3)) : 0;
        auto sv = as_obj<StringObj>(a[1])->view();
        ssize_t n = ::send(fd, sv.data(), sv.size(), flags);
        if (n < 0) syserror("send", errno);
        return make_integer(n);
    });

    // Net DB
    define_proc("getproto", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        struct protoent* pe = nullptr;
        if (nargs == 0 || a[0].is_undefined()) pe = ::getprotoent();
        else if (is_string(a[0])) pe = ::getprotobyname(as_obj<StringObj>(a[0])->to_std_string().c_str());
        else pe = ::getprotobynumber(static_cast<int>(scm_to_int64(a[0], "getproto", 1)));
        if (!pe) misc_error("getproto", "protocol not found");
        auto* v = heap_.alloc<VectorObj>(3, SCM_BOOL_F);
        v->elts[0] = make_string(pe->p_name ? pe->p_name : "");
        SCM al = SCM_EOL;
        if (pe->p_aliases) for (char** p = pe->p_aliases; *p; ++p) al = cons(make_string(*p), al);
        v->elts[1] = scm_reverse_x(al);
        v->elts[2] = make_integer(pe->p_proto);
        return SCM::from_obj(v);
    });
    define_proc("gethost", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        struct hostent* he = nullptr;
        if (nargs == 0 || a[0].is_undefined()) he = ::gethostent();
        else if (is_string(a[0])) he = ::gethostbyname(as_obj<StringObj>(a[0])->to_std_string().c_str());
        else {
            uint32_t addr = htonl(static_cast<uint32_t>(scm_to_uint64(a[0], "gethost", 1)));
            he = ::gethostbyaddr(&addr, sizeof(addr), AF_INET);
        }
        if (!he) misc_error("gethost", "host not found");
        auto* v = heap_.alloc<VectorObj>(5, SCM_BOOL_F);
        v->elts[0] = make_string(he->h_name ? he->h_name : "");
        SCM al = SCM_EOL;
        if (he->h_aliases) for (char** p = he->h_aliases; *p; ++p) al = cons(make_string(*p), al);
        v->elts[1] = scm_reverse_x(al);
        v->elts[2] = make_integer(he->h_addrtype);
        v->elts[3] = make_integer(he->h_length);
        SCM addrs = SCM_EOL;
        if (he->h_addr_list && he->h_addrtype == AF_INET) {
            for (char** p = he->h_addr_list; *p; ++p) {
                uint32_t a4 = 0;
                std::memcpy(&a4, *p, 4);
                addrs = cons(make_uinteger(ntohl(a4)), addrs);
            }
        }
        v->elts[4] = scm_reverse_x(addrs);
        return SCM::from_obj(v);
    });
}

} // namespace guile
