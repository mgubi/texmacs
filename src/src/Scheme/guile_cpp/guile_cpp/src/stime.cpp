#include "guile_cpp/vm.hpp"
#include <ctime>
#include <sys/time.h>
#include <sys/times.h>
#include <unistd.h>
#include <cerrno>

#include <cstdlib>

namespace guile {

struct ScopedTZ {
    bool changed = false;
    bool had_old = false;
    string old_tz;
    ScopedTZ(SCM zone_arg) {
        if (!zone_arg.is_undefined() && is_string(zone_arg)) {
            const char* cur = std::getenv("TZ");
            if (cur) { had_old = true; old_tz = cur; }
            ::setenv("TZ", ((char*) c_string(as_obj<StringObj>(zone_arg)->str())), 1);
            ::tzset();
            changed = true;
        }
    }
    ~ScopedTZ() {
        if (changed) {
            if (had_old) ::setenv("TZ", ((char*) c_string(old_tz)), 1);
            else ::unsetenv("TZ");
            ::tzset();
        }
    }
};

static SCM tm_to_vector(const struct tm& t, long gmtoff, const char* zone) {
    // Guile 1.8.8 tm vector (11 elements):
    // 0:sec 1:min 2:hour 3:mday 4:mon 5:year 6:wday 7:yday 8:isdst 9:gmtoff 10:zone
    auto* v = vm().heap().alloc<VectorObj>(11, SCM_BOOL_F);
    v->elts[0] = vm().make_integer(t.tm_sec);
    v->elts[1] = vm().make_integer(t.tm_min);
    v->elts[2] = vm().make_integer(t.tm_hour);
    v->elts[3] = vm().make_integer(t.tm_mday);
    v->elts[4] = vm().make_integer(t.tm_mon);
    v->elts[5] = vm().make_integer(t.tm_year);
    v->elts[6] = vm().make_integer(t.tm_wday);
    v->elts[7] = vm().make_integer(t.tm_yday);
    v->elts[8] = vm().make_integer(t.tm_isdst);
    v->elts[9] = vm().make_integer(-gmtoff); // Guile 1.8 tm:gmtoff is seconds WEST of UTC!
    v->elts[10] = zone ? vm().make_string(zone) : SCM_BOOL_F;
    return SCM::from_obj(v);
}

static struct tm vector_to_tm(SCM vec, const char* subr) {
    if (!is_vector(vec) || static_cast<size_t>(N(as_obj<VectorObj>(vec)->elts)) < 9) {
        vm().wrong_type_arg(subr, 1, vec, "tm vector");
    }
    auto* v = as_obj<VectorObj>(vec);
    struct tm t{};
    t.tm_sec   = static_cast<int>(scm_to_int64(v->elts[0], subr, 1));
    t.tm_min   = static_cast<int>(scm_to_int64(v->elts[1], subr, 1));
    t.tm_hour  = static_cast<int>(scm_to_int64(v->elts[2], subr, 1));
    t.tm_mday  = static_cast<int>(scm_to_int64(v->elts[3], subr, 1));
    t.tm_mon   = static_cast<int>(scm_to_int64(v->elts[4], subr, 1));
    t.tm_year  = static_cast<int>(scm_to_int64(v->elts[5], subr, 1));
    t.tm_wday  = static_cast<int>(scm_to_int64(v->elts[6], subr, 1));
    t.tm_yday  = static_cast<int>(scm_to_int64(v->elts[7], subr, 1));
    t.tm_isdst = static_cast<int>(scm_to_int64(v->elts[8], subr, 1));
    return t;
}

static const int64_t kStartRealUs = []() {
    struct timeval tv{};
    gettimeofday(&tv, nullptr);
    return static_cast<int64_t>(tv.tv_sec) * 1000000LL + tv.tv_usec;
}();

void VM::init_stime() {
    long clk_tck = ::sysconf(_SC_CLK_TCK);
    if (clk_tck <= 0) clk_tck = 100;
    define("internal-time-units-per-second", make_integer(clk_tck));

    define_proc("current-time", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return make_integer(static_cast<int64_t>(std::time(nullptr)));
    });
    define_proc("gettimeofday", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        struct timeval tv{};
        ::gettimeofday(&tv, nullptr);
        return cons(make_integer(tv.tv_sec), make_integer(tv.tv_usec));
    });
    define_proc("get-internal-real-time", 0, 0, false, [this, clk_tck](const SCM*, size_t) -> SCM {
        struct timeval tv{};
        ::gettimeofday(&tv, nullptr);
        int64_t now = static_cast<int64_t>(tv.tv_sec) * 1000000LL + tv.tv_usec;
        return make_integer((now - kStartRealUs) * clk_tck / 1000000LL);
    });
    define_proc("get-internal-run-time", 0, 0, false, [this, clk_tck](const SCM*, size_t) -> SCM {
        return make_integer(static_cast<int64_t>(std::clock()) * clk_tck / CLOCKS_PER_SEC);
    });
    define_proc("times", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        struct tms t{};
        clock_t real = ::times(&t);
        auto* v = heap_.alloc<VectorObj>(5, SCM::from_fixnum(0));
        v->elts[0] = make_integer(real);
        v->elts[1] = make_integer(t.tms_utime);
        v->elts[2] = make_integer(t.tms_stime);
        v->elts[3] = make_integer(t.tms_cutime);
        v->elts[4] = make_integer(t.tms_cstime);
        return SCM::from_obj(v);
    });

    define_proc("localtime", 1, 1, false, [](const SCM* a, size_t nargs) -> SCM {
        time_t t = static_cast<time_t>(scm_to_int64(a[0], "localtime", 1));
        ScopedTZ stz(nargs >= 2 ? a[1] : SCM_UNDEFINED);
        struct tm res{};
        ::localtime_r(&t, &res);
        return tm_to_vector(res, res.tm_gmtoff, res.tm_zone);
    });
    define_proc("gmtime", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        time_t t = static_cast<time_t>(scm_to_int64(a[0], "gmtime", 1));
        struct tm res{};
        ::gmtime_r(&t, &res);
        return tm_to_vector(res, 0, "GMT");
    });
    define_proc("mktime", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        struct tm t = vector_to_tm(a[0], "mktime");
        ScopedTZ stz(nargs >= 2 ? a[1] : SCM_UNDEFINED);
        time_t res = ::mktime(&t);
        return cons(make_integer(res), tm_to_vector(t, t.tm_gmtoff, t.tm_zone));
    });
    define_proc("strftime", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("strftime", 1, a[0], "string");
        struct tm t = vector_to_tm(a[1], "strftime");
        string fmt = as_obj<StringObj>(a[0])->str();
        char buf[1024];
        size_t n = std::strftime(buf, sizeof(buf), ((char*) c_string(fmt)), &t);
        return make_string(string(buf, n));
    });
    define_proc("strptime", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("strptime", 1, a[0], "string");
        if (!is_string(a[1])) wrong_type_arg("strptime", 2, a[1], "string");
        string fmt = as_obj<StringObj>(a[0])->str();
        string str = as_obj<StringObj>(a[1])->str();
        struct tm t{};
        t.tm_isdst = -1;
        ::tzset();
        c_string str_c(str), fmt_c(fmt);  // (kept alive: end points into str_c)
        char* end = ::strptime(str_c, fmt_c, &t);
        if (!end) misc_error("strptime", "failed to parse time string");
        long gmtoff = t.tm_gmtoff;
        const char* zone = t.tm_zone;
        if (str_find(fmt, "%s") != static_cast<size_t>(-1)) {
            // Ensure localtime fields & gmtoff reflect current TZ for GNU %s
            time_t tt = ::mktime(&t);
            struct tm lt{};
            ::localtime_r(&tt, &lt);
            t = lt;
            gmtoff = lt.tm_gmtoff;
            zone = lt.tm_zone;
        }
        return cons(tm_to_vector(t, gmtoff, zone), make_integer(end - static_cast<char*>(str_c)));
    });
    define_proc("tzset", 0, 0, false, [](const SCM*, size_t) -> SCM {
        ::tzset();
        return SCM_UNSPECIFIED;
    });

    // tm:* accessors/mutators (also defined in ice-9/boot-9.scm, safe to have here too)
    const char* fields[] = {"sec", "min", "hour", "mday", "mon", "year", "wday", "yday", "isdst", "gmtoff", "zone"};
    for (int i = 0; i < 11; ++i) {
        define_proc(string("tm:") * fields[i], 1, 0, false, [i](const SCM* a, size_t) -> SCM {
            return as_obj<VectorObj>(a[0])->elts[static_cast<size_t>(i)];
        });
        define_proc(string("set-tm:") * fields[i], 2, 0, false, [i](const SCM* a, size_t) -> SCM {
            as_obj<VectorObj>(a[0])->elts[static_cast<size_t>(i)] = a[1];
            return SCM_UNSPECIFIED;
        });
    }
}

} // namespace guile
