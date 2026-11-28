#include "guile_cpp/vm.hpp"
#include <algorithm>

namespace guile {

void VM::init_sort() {
    auto validate_less = [this](SCM less, const char* subr) {
        if (!is_procedure(less)) wrong_type_arg(subr, 2, less, "procedure");
        if (is_closure(less)) {
            int req = 0;
            bool rst = false;
            SCM f = as_obj<ClosureObj>(less)->formals;
            while (is_pair(f)) { ++req; f = cdr(f); }
            if (!f.is_null()) rst = true;
            if (req > 2 || (!rst && req < 2)) wrong_type_arg(subr, 2, less, "2-argument procedure");
        } else if (is_subr(less)) {
            auto* s = as_obj<SubrObj>(less);
            if (s->req > 2 || (!s->rst && s->req + s->opt < 2)) wrong_type_arg(subr, 2, less, "2-argument procedure");
        }
    };

    auto sort_impl = [this, validate_less](SCM seq, SCM less, bool stable, bool destructive) -> SCM {
        validate_less(less, destructive ? "sort!" : "sort");
        if (seq.is_null()) return SCM_EOL;
        if (is_pair(seq)) {
            ScmRootVec items(heap_);
            SCM cur = seq;
            while (is_pair(cur)) { (items << car(cur)); cur = cdr(cur); }
            if (!cur.is_null()) wrong_type_arg("sort", 1, seq, "proper list");
            auto cmp = [&](SCM a, SCM b) { return call2(less, a, b).is_true(); };
            if (stable) std::stable_sort(begin(items.vec), end(items.vec), cmp);
            else std::sort(begin(items.vec), end(items.vec), cmp);
            if (destructive) {
                cur = seq;
                for (size_t i = 0; i < static_cast<size_t>(N(items)); ++i) { set_car(cur, items[i]); cur = cdr(cur); }
                return seq;
            }
            return list_n(gcpp_data(items), static_cast<size_t>(N(items)));
        }
        if (is_vector(seq)) {
            auto* v = as_obj<VectorObj>(seq);
            ScmRootVec items(heap_);
            items.vec = copy(v->elts);  // (sorted in place: a copy)
            items.sync();
            auto cmp = [&](SCM a, SCM b) { return call2(less, a, b).is_true(); };
            if (stable) std::stable_sort(begin(items.vec), end(items.vec), cmp);
            else std::sort(begin(items.vec), end(items.vec), cmp);
            if (destructive) {
                v->elts = items.vec;
                return seq;
            }
            return heap_.alloc_scm<VectorObj>(items.vec);
        }
        if (is_array(seq)) {
            auto* ar = as_obj<ArrayObj>(seq);
            if (static_cast<size_t>(N(ar->dims)) == 1 && is_vector(ar->base_vec)) {
                auto* v = as_obj<VectorObj>(ar->base_vec);
                int64_t len = ar->dims[0].ubnd - ar->dims[0].lbnd + 1;
                ScmRootVec items(heap_);
                for (int64_t i = 0; i < len; ++i) {
                    (items << (v->elts[static_cast<size_t>(ar->base_offset + i * ar->dims[0].inc)]));
                }
                auto cmp = [&](SCM a, SCM b) { return call2(less, a, b).is_true(); };
                if (stable) std::stable_sort(begin(items.vec), end(items.vec), cmp);
                else std::sort(begin(items.vec), end(items.vec), cmp);
                if (destructive) {
                    for (int64_t i = 0; i < len; ++i) {
                        v->elts[static_cast<size_t>(ar->base_offset + i * ar->dims[0].inc)] = items[static_cast<size_t>(i)];
                    }
                    return seq;
                }
                return heap_.alloc_scm<VectorObj>(items.vec);
            }
        }
        wrong_type_arg("sort", 1, seq, "list or vector");
    };

    define_proc("sort", 2, 0, false, [sort_impl](const SCM* a, size_t) -> SCM { return sort_impl(a[0], a[1], false, false); });
    define_proc("sort!", 2, 0, false, [sort_impl](const SCM* a, size_t) -> SCM { return sort_impl(a[0], a[1], false, true); });
    define_proc("stable-sort", 2, 0, false, [sort_impl](const SCM* a, size_t) -> SCM { return sort_impl(a[0], a[1], true, false); });
    define_proc("stable-sort!", 2, 0, false, [sort_impl](const SCM* a, size_t) -> SCM { return sort_impl(a[0], a[1], true, true); });

    define_proc("restricted-vector-sort!", 4, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_vector(a[0])) wrong_type_arg("restricted-vector-sort!", 1, a[0], "vector");
        auto* v = as_obj<VectorObj>(a[0]);
        int64_t start = scm_to_int64(a[2], "restricted-vector-sort!", 3);
        int64_t end = scm_to_int64(a[3], "restricted-vector-sort!", 4);
        if (start < 0 || end < start || static_cast<size_t>(end) > static_cast<size_t>(N(v->elts))) out_of_range("restricted-vector-sort!", a[2]);
        std::stable_sort(begin(v->elts) + start, begin(v->elts) + end,
                         [&](SCM x, SCM y) { return call2(a[1], x, y).is_true(); });
        return a[0];
    });

    define_proc("sorted?", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (a[0].is_null()) return SCM_BOOL_T;
        if (is_pair(a[0])) {
            SCM cur = a[0];
            while (is_pair(cdr(cur))) {
                if (call2(a[1], car(cdr(cur)), car(cur)).is_true()) return SCM_BOOL_F;
                cur = cdr(cur);
            }
            return SCM_BOOL_T;
        }
        if (is_vector(a[0])) {
            auto* v = as_obj<VectorObj>(a[0]);
            for (size_t i = 1; i < static_cast<size_t>(N(v->elts)); ++i) {
                if (call2(a[1], v->elts[i], v->elts[i - 1]).is_true()) return SCM_BOOL_F;
            }
            return SCM_BOOL_T;
        }
        if (is_array(a[0])) {
            auto* ar = as_obj<ArrayObj>(a[0]);
            if (static_cast<size_t>(N(ar->dims)) == 1 && is_vector(ar->base_vec)) {
                auto* v = as_obj<VectorObj>(ar->base_vec);
                int64_t len = ar->dims[0].ubnd - ar->dims[0].lbnd + 1;
                for (int64_t i = 1; i < len; ++i) {
                    SCM cur_e = v->elts[static_cast<size_t>(ar->base_offset + i * ar->dims[0].inc)];
                    SCM prev_e = v->elts[static_cast<size_t>(ar->base_offset + (i - 1) * ar->dims[0].inc)];
                    if (call2(a[1], cur_e, prev_e).is_true()) return SCM_BOOL_F;
                }
                return SCM_BOOL_T;
            }
        }
        wrong_type_arg("sorted?", 1, a[0], "list or vector");
    });

    auto merge_impl = [this](SCM l1, SCM l2, SCM less) -> SCM {
        SCM head = SCM_EOL, tail = SCM_EOL;
        auto push = [&](SCM x) {
            SCM n = cons(x, SCM_EOL);
            if (head.is_null()) head = tail = n;
            else { set_cdr(tail, n); tail = n; }
        };
        while (is_pair(l1) && is_pair(l2)) {
            if (call2(less, car(l2), car(l1)).is_true()) {
                push(car(l2)); l2 = cdr(l2);
            } else {
                push(car(l1)); l1 = cdr(l1);
            }
        }
        while (is_pair(l1)) { push(car(l1)); l1 = cdr(l1); }
        while (is_pair(l2)) { push(car(l2)); l2 = cdr(l2); }
        return head;
    };
    define_proc("merge", 3, 0, false, [merge_impl](const SCM* a, size_t) -> SCM { return merge_impl(a[0], a[1], a[2]); });
    define_proc("merge!", 3, 0, false, [merge_impl](const SCM* a, size_t) -> SCM { return merge_impl(a[0], a[1], a[2]); });
}

} // namespace guile
