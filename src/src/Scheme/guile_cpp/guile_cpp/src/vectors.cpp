#include "guile_cpp/vm.hpp"

namespace guile {

SCM VM::make_vector(size_t n, SCM fill) {
    return heap_.alloc_scm<VectorObj>(n, fill);
}

void VM::init_vectors() {
    define_proc("vector?", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return SCM::from_bool(is_vector(args[0]) || is_weak_vector(args[0]));
    });

    define_proc("make-vector", 1, 1, false, [this](const SCM* args, size_t nargs) -> SCM {
        int64_t n = scm_to_int64(args[0], "make-vector", 1);
        if (n < 0) out_of_range("make-vector", args[0]);
        SCM fill = (nargs >= 2 && !args[1].is_undefined()) ? args[1] : SCM_UNSPECIFIED;
        return make_vector(static_cast<size_t>(n), fill);
    });

    define_proc("vector", 0, 0, true, [this](const SCM* args, size_t nargs) -> SCM {
        SCM lst = nargs > 0 ? args[0] : SCM_EOL;
        std::vector<SCM> elts;
        while (is_pair(lst)) {
            elts.push_back(car(lst));
            lst = cdr(lst);
        }
        return heap_.alloc_scm<VectorObj>(std::move(elts));
    });

    define_proc("vector-length", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (is_vector(args[0])) return make_integer(static_cast<int64_t>(as_obj<VectorObj>(args[0])->elts.size()));
        if (is_weak_vector(args[0])) return make_integer(static_cast<int64_t>(as_obj<WeakVectorObj>(args[0])->elts.size()));
        wrong_type_arg("vector-length", 1, args[0], "vector");
    });

    define_proc("vector-ref", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        int64_t k = scm_to_int64(args[1], "vector-ref", 2);
        if (is_vector(args[0])) {
            auto* v = as_obj<VectorObj>(args[0]);
            if (k < 0 || static_cast<size_t>(k) >= v->elts.size()) out_of_range("vector-ref", args[1]);
            return v->elts[static_cast<size_t>(k)];
        }
        if (is_weak_vector(args[0])) {
            auto* v = as_obj<WeakVectorObj>(args[0]);
            if (k < 0 || static_cast<size_t>(k) >= v->elts.size()) out_of_range("vector-ref", args[1]);
            return v->elts[static_cast<size_t>(k)];
        }
        wrong_type_arg("vector-ref", 1, args[0], "vector");
    });

    define_proc("vector-set!", 3, 0, false, [this](const SCM* args, size_t) -> SCM {
        int64_t k = scm_to_int64(args[1], "vector-set!", 2);
        if (is_vector(args[0])) {
            auto* v = as_obj<VectorObj>(args[0]);
            if (k < 0 || static_cast<size_t>(k) >= v->elts.size()) out_of_range("vector-set!", args[1]);
            v->elts[static_cast<size_t>(k)] = args[2];
            return SCM_UNSPECIFIED;
        }
        if (is_weak_vector(args[0])) {
            auto* v = as_obj<WeakVectorObj>(args[0]);
            if (k < 0 || static_cast<size_t>(k) >= v->elts.size()) out_of_range("vector-set!", args[1]);
            v->elts[static_cast<size_t>(k)] = args[2];
            return SCM_UNSPECIFIED;
        }
        wrong_type_arg("vector-set!", 1, args[0], "vector");
    });

    define_proc("vector->list", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (is_vector(args[0])) {
            auto* v = as_obj<VectorObj>(args[0]);
            SCM res = SCM_EOL;
            for (size_t i = v->elts.size(); i > 0; --i) res = cons(v->elts[i - 1], res);
            return res;
        }
        if (is_weak_vector(args[0])) {
            auto* v = as_obj<WeakVectorObj>(args[0]);
            SCM res = SCM_EOL;
            for (size_t i = v->elts.size(); i > 0; --i) res = cons(v->elts[i - 1], res);
            return res;
        }
        if (is_array(args[0])) {
            auto* ar = as_obj<ArrayObj>(args[0]);
            if (ar->dims.size() == 1 && ar->dims[0].lbnd == 0 && is_vector(ar->base_vec)) {
                auto* v = as_obj<VectorObj>(ar->base_vec);
                SCM res = SCM_EOL;
                for (int64_t i = ar->dims[0].ubnd; i >= 0; --i) {
                    int64_t idx = ar->base_offset + i * ar->dims[0].inc;
                    res = cons(v->elts[static_cast<size_t>(idx)], res);
                }
                return res;
            }
        }
        wrong_type_arg("vector->list", 1, args[0], "vector");
    });

    define_proc("list->vector", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        int64_t n = scm_ilength(args[0]);
        if (n < 0) wrong_type_arg("list->vector", 1, args[0], "proper list");
        std::vector<SCM> elts;
        elts.reserve(static_cast<size_t>(n));
        SCM cur = args[0];
        while (is_pair(cur)) {
            elts.push_back(car(cur));
            cur = cdr(cur);
        }
        return heap_.alloc_scm<VectorObj>(std::move(elts));
    });

    define_proc("vector-fill!", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (is_vector(args[0])) {
            auto* v = as_obj<VectorObj>(args[0]);
            for (SCM& e : v->elts) e = args[1];
            return SCM_UNSPECIFIED;
        }
        if (is_weak_vector(args[0])) {
            auto* v = as_obj<WeakVectorObj>(args[0]);
            for (SCM& e : v->elts) e = args[1];
            return SCM_UNSPECIFIED;
        }
        wrong_type_arg("vector-fill!", 1, args[0], "vector");
    });

    define_proc("vector-copy", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_vector(args[0])) wrong_type_arg("vector-copy", 1, args[0], "vector");
        auto* v = as_obj<VectorObj>(args[0]);
        return heap_.alloc_scm<VectorObj>(v->elts);
    });

    define_proc("vector-move-left!", 5, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_vector(args[0])) wrong_type_arg("vector-move-left!", 1, args[0], "vector");
        if (!is_vector(args[3])) wrong_type_arg("vector-move-left!", 4, args[3], "vector");
        auto* v1 = as_obj<VectorObj>(args[0]);
        auto* v2 = as_obj<VectorObj>(args[3]);
        int64_t s1 = scm_to_int64(args[1], "vector-move-left!", 2);
        int64_t e1 = scm_to_int64(args[2], "vector-move-left!", 3);
        int64_t s2 = scm_to_int64(args[4], "vector-move-left!", 5);
        if (s1 < 0 || e1 < s1 || static_cast<size_t>(e1) > v1->elts.size() ||
            s2 < 0 || static_cast<size_t>(s2 + (e1 - s1)) > v2->elts.size()) {
            out_of_range("vector-move-left!", args[1]);
        }
        for (int64_t i = s1, j = s2; i < e1; ++i, ++j) {
            v2->elts[static_cast<size_t>(j)] = v1->elts[static_cast<size_t>(i)];
        }
        return SCM_UNSPECIFIED;
    });

    define_proc("vector-move-right!", 5, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_vector(args[0])) wrong_type_arg("vector-move-right!", 1, args[0], "vector");
        if (!is_vector(args[3])) wrong_type_arg("vector-move-right!", 4, args[3], "vector");
        auto* v1 = as_obj<VectorObj>(args[0]);
        auto* v2 = as_obj<VectorObj>(args[3]);
        int64_t s1 = scm_to_int64(args[1], "vector-move-right!", 2);
        int64_t e1 = scm_to_int64(args[2], "vector-move-right!", 3);
        int64_t s2 = scm_to_int64(args[4], "vector-move-right!", 5);
        int64_t len = e1 - s1;
        if (s1 < 0 || len < 0 || static_cast<size_t>(e1) > v1->elts.size() ||
            s2 < 0 || static_cast<size_t>(s2 + len) > v2->elts.size()) {
            out_of_range("vector-move-right!", args[1]);
        }
        for (int64_t i = e1 - 1, j = s2 + len - 1; i >= s1; --i, --j) {
            v2->elts[static_cast<size_t>(j)] = v1->elts[static_cast<size_t>(i)];
        }
        return SCM_UNSPECIFIED;
    });

    // Weak vectors & weak alist vectors
    auto define_weaks = [this]() {
        define_proc("make-weak-vector", 1, 1, false, [this](const SCM* args, size_t nargs) -> SCM {
            int64_t n = scm_to_int64(args[0], "make-weak-vector", 1);
            if (n < 0) out_of_range("make-weak-vector", args[0]);
            SCM fill = (nargs >= 2 && !args[1].is_undefined()) ? args[1] : SCM_BOOL_F;
            return heap_.alloc_scm<WeakVectorObj>(static_cast<size_t>(n), fill, WeakKind::Vector);
        });
        define_proc("weak-vector", 0, 0, true, [this](const SCM* args, size_t nargs) -> SCM {
            SCM lst = nargs > 0 ? args[0] : SCM_EOL;
            int64_t n = scm_ilength(lst);
            if (n < 0) wrong_type_arg("weak-vector", 1, lst, "list");
            SCM wv = heap_.alloc_scm<WeakVectorObj>(static_cast<size_t>(n), SCM_BOOL_F, WeakKind::Vector);
            auto* o = as_obj<WeakVectorObj>(wv);
            size_t i = 0;
            while (is_pair(lst)) { o->elts[i++] = car(lst); lst = cdr(lst); }
            return wv;
        });
        define_proc("list->weak-vector", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
            int64_t n = scm_ilength(args[0]);
            if (n < 0) wrong_type_arg("list->weak-vector", 1, args[0], "proper list");
            SCM wv = heap_.alloc_scm<WeakVectorObj>(static_cast<size_t>(n), SCM_BOOL_F, WeakKind::Vector);
            auto* o = as_obj<WeakVectorObj>(wv);
            SCM lst = args[0];
            size_t i = 0;
            while (is_pair(lst)) { o->elts[i++] = car(lst); lst = cdr(lst); }
            return wv;
        });
        define_proc("weak-vector?", 1, 0, false, [](const SCM* args, size_t) -> SCM {
            return SCM::from_bool(is_weak_vector(args[0]) && as_obj<WeakVectorObj>(args[0])->kind == WeakKind::Vector);
        });
        auto reg_alist_vec = [this](const std::string& name, WeakKind wk) {
            define_proc("make-" + name, 1, 0, false, [this, name, wk](const SCM* args, size_t) -> SCM {
                int64_t n = scm_to_int64(args[0], ("make-" + name).c_str(), 1);
                if (n < 0) out_of_range(("make-" + name).c_str(), args[0]);
                return heap_.alloc_scm<WeakVectorObj>(static_cast<size_t>(n), SCM_EOL, wk);
            });
            define_proc(name + "?", 1, 0, false, [wk](const SCM* args, size_t) -> SCM {
                return SCM::from_bool(is_weak_vector(args[0]) && as_obj<WeakVectorObj>(args[0])->kind == wk);
            });
        };
        reg_alist_vec("weak-key-alist-vector", WeakKind::KeyAlist);
        reg_alist_vec("weak-value-alist-vector", WeakKind::ValueAlist);
        reg_alist_vec("doubly-weak-alist-vector", WeakKind::DoublyWeakAlist);
    };
    define_proc("%init-weaks-builtins", 0, 0, false, [define_weaks](const SCM*, size_t) -> SCM {
        define_weaks();
        return SCM_UNSPECIFIED;
    });
}

} // namespace guile
