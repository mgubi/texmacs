#include "guile_cpp/vm.hpp"
#include <cerrno>
#include <cstring>

namespace guile {

[[noreturn]] void VM::throw_scm(SCM key, SCM args) {
    throw SchemeThrow(key, args);
}

[[noreturn]] void VM::error(SCM key, const char* subr, string msg, SCM args, SCM rest) {
    SCM s_subr = subr ? make_string(subr) : SCM_BOOL_F;
    SCM s_msg = make_string(msg);
    SCM s_args = args;
    SCM s_rest = rest;
    throw_scm(key, list4(s_subr, s_msg, s_args, s_rest));
}

[[noreturn]] void VM::misc_error(const char* subr, string msg, SCM args) {
    error(sym_misc_error, subr, msg, args, SCM_BOOL_F);
}

[[noreturn]] void VM::wrong_type_arg(const char* subr, int pos, SCM bad_val, const char* expected) {
    string msg;
    SCM margs;
    if (expected) {
        msg = "Wrong type argument in position ~A (expecting ~A): ~S";
        margs = list3(make_integer(pos), make_string(expected), bad_val);
    } else if (pos > 0) {
        msg = "Wrong type argument in position ~A: ~S";
        margs = list2(make_integer(pos), bad_val);
    } else {
        msg = "Wrong type argument: ~S";
        margs = list1(bad_val);
    }
    error(sym_wrong_type_arg, subr, msg, margs, list1(bad_val));
}

[[noreturn]] void VM::wrong_num_args(const char* subr, SCM proc) {
    string msg = "Wrong number of arguments to ~A";
    SCM arg = proc.is_true() ? proc : (subr ? make_string(subr) : SCM_BOOL_F);
    error(sym_wrong_num_args, nullptr, msg, list1(arg), SCM_BOOL_F);
}

[[noreturn]] void VM::out_of_range(const char* subr, SCM bad_val) {
    error(sym_out_of_range, subr, "Value out of range: ~S", list1(bad_val), list1(bad_val));
}

[[noreturn]] void VM::syserror(const char* subr, int errnum, string extra) {
    string emsg = std::strerror(errnum);
    if (!(N(extra) == 0)) {
        error(sym_system_error, subr, "~A: ~S", list2(make_string(emsg), make_string(extra)),
              list1(make_integer(errnum)));
    } else {
        error(sym_system_error, subr, "~A", list1(make_string(emsg)),
              list1(make_integer(errnum)));
    }
}

[[noreturn]] void VM::num_overflow(const char* subr) {
    error(sym_numerical_overflow, subr, "Numerical overflow", SCM_BOOL_F, SCM_BOOL_F);
}

void VM::init_error() {
    define_proc("scm-error", 5, 0, false, [this](const SCM* args, size_t) -> SCM {
        // (scm-error key subr message args data)
        throw_scm(args[0], list4(args[1], args[2], args[3], args[4]));
    });

    define_proc("strerror", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        int64_t e = scm_to_int64(args[0], "strerror", 1);
        return make_string(std::strerror(static_cast<int>(e)));
    });

    define_proc("system-error-errno", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        // args[0] is exception args: (key subr msg args (errno))
        SCM a = args[0];
        if (is_pair(a) && car(a) == sym_system_error) {
            SCM rest = cdr(a);
            // skip subr, msg, msg-args -> data
            for (int i = 0; i < 3 && is_pair(rest); ++i) rest = cdr(rest);
            if (is_pair(rest) && is_pair(car(rest))) {
                return car(car(rest));
            }
        }
        return SCM_BOOL_F;
    });
}

} // namespace guile
