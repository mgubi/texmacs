#include "guile_cpp/vm.hpp"
#include <algorithm>
#include <vector>

namespace guile {

static void init_srfi_60_extension() {
    auto& v = vm();
    v.define_proc("log2-binary-factors", 1, 0, false, [&v](const SCM* a, size_t) -> SCM {
        uint64_t scratch = 0;
        BigIntView z = scm_to_bigint_view(a[0], scratch, "log2-binary-factors", 1);
        if (z.is_zero()) return v.make_integer(-1);
        return v.make_uinteger(BigInt::scan1(z, 0));
    });
    v.define_proc("copy-bit", 3, 0, false, [](const SCM* a, size_t) -> SCM {
        uint64_t idx = scm_to_uint64(a[0], "copy-bit", 1);
        bool bit = a[2].is_true();
        uint64_t scratch = 0;
        BigIntView z = scm_to_bigint_view(a[1], scratch, "copy-bit", 2);
        return scm_from_bigint(bit ? BigInt::setbit(z, static_cast<size_t>(idx))
                                   : BigInt::clrbit(z, static_cast<size_t>(idx)));
    });
    v.define_proc("rotate-bit-field", 4, 0, false, [&v](const SCM* a, size_t) -> SCM {
        int64_t count = scm_to_int64(a[1], "rotate-bit-field", 2);
        int64_t start = scm_to_int64(a[2], "rotate-bit-field", 3);
        int64_t end = scm_to_int64(a[3], "rotate-bit-field", 4);
        if (start < 0 || end < start) v.out_of_range("rotate-bit-field", a[2]);
        int64_t width = end - start;
        if (width <= 1) return a[0];
        int64_t r = ((count % width) + width) % width;
        if (r == 0) return a[0];
        BigInt z = scm_to_bigint(a[0], "rotate-bit-field", 1);
        std::vector<bool> bits(static_cast<size_t>(width));
        for (int64_t i = 0; i < width; ++i) {
            bits[static_cast<size_t>(i)] = BigInt::tstbit(z, static_cast<size_t>(start + i));
        }
        for (int64_t i = 0; i < width; ++i) {
            int64_t src = (i - r + width) % width;
            if (bits[static_cast<size_t>(src)]) z.setbit_inplace(static_cast<size_t>(start + i));
            else z.clrbit_inplace(static_cast<size_t>(start + i));
        }
        return scm_from_bigint(std::move(z));
    });
    v.define_proc("reverse-bit-field", 3, 0, false, [&v](const SCM* a, size_t) -> SCM {
        int64_t start = scm_to_int64(a[1], "reverse-bit-field", 2);
        int64_t end = scm_to_int64(a[2], "reverse-bit-field", 3);
        if (start < 0 || end < start) v.out_of_range("reverse-bit-field", a[1]);
        BigInt z = scm_to_bigint(a[0], "reverse-bit-field", 1);
        for (int64_t l = start, r = end - 1; l < r; ++l, --r) {
            bool bl = BigInt::tstbit(z, static_cast<size_t>(l));
            bool br = BigInt::tstbit(z, static_cast<size_t>(r));
            if (br) z.setbit_inplace(static_cast<size_t>(l)); else z.clrbit_inplace(static_cast<size_t>(l));
            if (bl) z.setbit_inplace(static_cast<size_t>(r)); else z.clrbit_inplace(static_cast<size_t>(r));
        }
        return scm_from_bigint(std::move(z));
    });
    v.define_proc("integer->list", 1, 1, false, [&v](const SCM* a, size_t nargs) -> SCM {
        uint64_t scratch = 0;
        BigIntView z = scm_to_bigint_view(a[0], scratch, "integer->list", 1);
        int64_t len = 0;
        if (nargs >= 2 && !a[1].is_undefined()) {
            len = scm_to_int64(a[1], "integer->list", 2);
            if (len < 0) v.out_of_range("integer->list", a[1]);
        } else {
            len = static_cast<int64_t>(BigInt::integer_length(z));
        }
        SCM res = SCM_EOL;
        for (int64_t i = 0; i < len; ++i) {
            res = v.cons(SCM::from_bool(BigInt::tstbit(z, static_cast<size_t>(i))), res);
        }
        return res;
    });
    auto list_to_int = [&v](const SCM* a, size_t) -> SCM {
        BigInt z(int64_t(0));
        SCM lst = a[0];
        while (is_pair(lst)) {
            z = BigInt::mul_2exp(z, 1);
            if (car(lst).is_true()) z = BigInt::add_ui(z, 1);
            lst = cdr(lst);
        }
        if (!lst.is_null()) v.wrong_type_arg("list->integer", 1, a[0], "proper list");
        return scm_from_bigint(std::move(z));
    };
    v.define_proc("list->integer", 1, 0, false, list_to_int);
    v.define_proc("booleans->integer", 0, 0, true, [list_to_int](const SCM* a, size_t nargs) -> SCM {
        SCM lst = nargs > 0 ? a[0] : SCM_EOL;
        return list_to_int(&lst, 1);
    });
}

static void init_srfi_1_extension() {
    auto& v = vm();

    // Constructors & Predicates & Selectors
    v.define_proc("xcons", 2, 0, false, [&v](const SCM* a, size_t) -> SCM { return v.cons(a[1], a[0]); });
    v.define_proc("not-pair?", 1, 0, false, [](const SCM* a, size_t) -> SCM { return SCM::from_bool(!is_pair(a[0])); });
    v.define_proc("car+cdr", 1, 0, false, [&v](const SCM* a, size_t) -> SCM {
        if (!is_pair(a[0])) v.wrong_type_arg("car+cdr", 1, a[0], "pair");
        return v.make_values(v.list2(car(a[0]), cdr(a[0])));
    });
    auto is_proc = [](SCM x) -> bool {
        return is_subr(x) || is_closure(x) || is_continuation(x) || is_proc_with_setter(x) ||
               is_eval_closure(x) || (is_struct(x) && as_obj<StructObj>(x)->is_entity);
    };
    auto check_proc_arity = [&v, is_proc](SCM proc, int arity, const char* name, int pos) {
        if (!is_proc(proc)) v.wrong_type_arg(name, pos, proc, "procedure");
        if (is_closure(proc)) {
            SCM f = as_obj<ClosureObj>(proc)->formals;
            int req = 0;
            while (is_pair(f)) { ++req; f = cdr(f); }
            bool has_rest = !f.is_null();
            if (arity < req || (!has_rest && arity > req)) {
                v.wrong_type_arg(name, pos, proc, "procedure");
            }
        } else if (is_subr(proc)) {
            auto* sb = as_obj<SubrObj>(proc);
            if (arity < sb->req || (!sb->rst && arity > sb->req + sb->opt)) {
                v.wrong_type_arg(name, pos, proc, "procedure");
            }
        }
    };

    auto nth_fn = [&v](const char* name, int idx) {
        v.define_proc(name, 1, 0, false, [&v, name, idx](const SCM* a, size_t) -> SCM {
            SCM cur = a[0];
            for (int i = 0; i < idx; ++i) {
                if (!is_pair(cur)) v.out_of_range(name, a[0]);
                cur = cdr(cur);
            }
            if (!is_pair(cur)) v.out_of_range(name, a[0]);
            return car(cur);
        });
    };
    nth_fn("fifth", 4); nth_fn("sixth", 5); nth_fn("seventh", 6);
    nth_fn("eighth", 7); nth_fn("ninth", 8); nth_fn("tenth", 9);

    v.define_proc("last", 1, 0, false, [&v](const SCM* a, size_t) -> SCM {
        if (!is_pair(a[0])) v.wrong_type_arg("last", 1, a[0], "pair");
        SCM cur = a[0];
        while (is_pair(cdr(cur))) cur = cdr(cur);
        return car(cur);
    });

    v.define_proc("take!", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        int64_t k = scm_to_int64(a[1], "take!", 2);
        if (k < 0) v.out_of_range("take!", a[1]);
        if (k == 0) return SCM_EOL;
        SCM cur = a[0];
        for (int64_t i = 1; i < k; ++i) {
            if (!is_pair(cur)) v.wrong_type_arg("take!", 1, a[0], "list");
            cur = cdr(cur);
        }
        if (!is_pair(cur)) v.wrong_type_arg("take!", 1, a[0], "list");
        set_cdr(cur, SCM_EOL);
        return a[0];
    });

    v.define_proc("take-right", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        int64_t k = scm_to_int64(a[1], "take-right", 2);
        if (k < 0) v.out_of_range("take-right", a[1]);
        SCM lag = a[0], lead = a[0];
        for (int64_t i = 0; i < k; ++i) {
            if (!is_pair(lead)) v.wrong_type_arg("take-right", 1, a[0], "list");
            lead = cdr(lead);
        }
        while (is_pair(lead)) { lead = cdr(lead); lag = cdr(lag); }
        return lag;
    });

    v.define_proc("drop-right", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        int64_t k = scm_to_int64(a[1], "drop-right", 2);
        if (k < 0) v.out_of_range("drop-right", a[1]);
        SCM lag = a[0], lead = a[0];
        for (int64_t i = 0; i < k; ++i) {
            if (!is_pair(lead)) v.wrong_type_arg("drop-right", 1, a[0], "list");
            lead = cdr(lead);
        }
        SCM head = SCM_EOL, tail = SCM_EOL;
        while (is_pair(lead)) {
            SCM n = v.cons(car(lag), SCM_EOL);
            if (head.is_null()) head = tail = n; else { set_cdr(tail, n); tail = n; }
            lead = cdr(lead); lag = cdr(lag);
        }
        return head;
    });

    v.define_proc("drop-right!", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        int64_t k = scm_to_int64(a[1], "drop-right!", 2);
        if (k < 0) v.out_of_range("drop-right!", a[1]);
        SCM lag = a[0], lead = a[0];
        for (int64_t i = 0; i < k; ++i) {
            if (!is_pair(lead)) v.wrong_type_arg("drop-right!", 1, a[0], "list");
            lead = cdr(lead);
        }
        if (!is_pair(lead)) return SCM_EOL;
        while (is_pair(cdr(lead))) { lead = cdr(lead); lag = cdr(lag); }
        set_cdr(lag, SCM_EOL);
        return a[0];
    });

    v.define_proc("split-at", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        int64_t k = scm_to_int64(a[1], "split-at", 2);
        if (k < 0) v.out_of_range("split-at", a[1]);
        SCM head = SCM_EOL, tail = SCM_EOL, cur = a[0];
        for (int64_t i = 0; i < k; ++i) {
            if (!is_pair(cur)) v.wrong_type_arg("split-at", 1, a[0], "list");
            SCM n = v.cons(car(cur), SCM_EOL);
            if (head.is_null()) head = tail = n; else { set_cdr(tail, n); tail = n; }
            cur = cdr(cur);
        }
        return v.make_values(v.list2(head, cur));
    });

    v.define_proc("split-at!", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        int64_t k = scm_to_int64(a[1], "split-at!", 2);
        if (k < 0) v.out_of_range("split-at!", a[1]);
        if (k == 0) return v.make_values(v.list2(SCM_EOL, a[0]));
        SCM cur = a[0];
        for (int64_t i = 1; i < k; ++i) {
            if (!is_pair(cur)) v.wrong_type_arg("split-at!", 1, a[0], "list");
            cur = cdr(cur);
        }
        if (!is_pair(cur)) v.wrong_type_arg("split-at!", 1, a[0], "list");
        SCM rest = cdr(cur);
        set_cdr(cur, SCM_EOL);
        return v.make_values(v.list2(a[0], rest));
    });

    // Fold / Reduce / Map / Filter-map
    v.define_proc("fold", 3, 0, true, [&v, check_proc_arity](const SCM* a, size_t nargs) -> SCM {
        SCM kons = a[0], acc = a[1];
        std::vector<SCM> ls;
        ls.push_back(a[2]);
        SCM rest = nargs > 3 ? a[3] : SCM_EOL;
        while (is_pair(rest)) { ls.push_back(car(rest)); rest = cdr(rest); }
        if (ls.size() == 1) check_proc_arity(kons, 2, "fold", 1);
        while (true) {
            bool done = false;
            for (size_t i = 0; i < ls.size(); ++i) {
                if (ls[i].is_null()) done = true;
                else if (!is_pair(ls[i])) v.wrong_type_arg("fold", static_cast<int>(3 + i), ls[i], "proper list");
            }
            if (done) return acc;
            std::vector<SCM> cargs;
            for (auto& l : ls) {
                cargs.push_back(car(l));
                l = cdr(l);
            }
            cargs.push_back(acc);
            acc = v.apply(kons, cargs.data(), cargs.size());
        }
    });

    v.define_proc("reduce", 3, 0, false, [&v](const SCM* a, size_t) -> SCM {
        if (a[2].is_null()) return a[1];
        if (!is_pair(a[2])) v.wrong_type_arg("reduce", 3, a[2], "list");
        SCM acc = car(a[2]);
        SCM cur = cdr(a[2]);
        while (is_pair(cur)) {
            acc = v.call2(a[0], car(cur), acc);
            cur = cdr(cur);
        }
        return acc;
    });

    v.define_proc("reduce-right", 3, 0, false, [&v](const SCM* a, size_t) -> SCM {
        if (a[2].is_null()) return a[1];
        if (!is_pair(a[2])) v.wrong_type_arg("reduce-right", 3, a[2], "list");
        ScmRootVec items(v.heap());
        SCM cur = a[2];
        while (is_pair(cur)) { items.push_back(car(cur)); cur = cdr(cur); }
        SCM acc = items[items.size() - 1];
        for (size_t i = items.size() - 1; i > 0; --i) {
            acc = v.call2(a[0], items[i - 1], acc);
        }
        return acc;
    });

    v.define_proc("filter-map", 2, 0, true, [&v](const SCM* a, size_t nargs) -> SCM {
        SCM proc = a[0];
        std::vector<SCM> ls;
        ls.push_back(a[1]);
        SCM rest = nargs > 2 ? a[2] : SCM_EOL;
        while (is_pair(rest)) { ls.push_back(car(rest)); rest = cdr(rest); }
        SCM head = SCM_EOL, tail = SCM_EOL;
        while (true) {
            bool done = false;
            for (size_t i = 0; i < ls.size(); ++i) {
                if (ls[i].is_null()) done = true;
                else if (!is_pair(ls[i])) v.wrong_type_arg("filter-map", static_cast<int>(2 + i), ls[i], "proper list");
            }
            if (done) return head;
            std::vector<SCM> cargs;
            for (auto& l : ls) {
                cargs.push_back(car(l));
                l = cdr(l);
            }
            SCM r = v.apply(proc, cargs.data(), cargs.size());
            if (r.is_true()) {
                SCM n = v.cons(r, SCM_EOL);
                if (head.is_null()) head = tail = n; else { set_cdr(tail, n); tail = n; }
            }
        }
    });

    // Searching: find, find-tail, take-while, drop-while, span, break, list-index
    v.define_proc("find", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM cur = a[1];
        while (is_pair(cur)) {
            if (v.call1(a[0], car(cur)).is_true()) return car(cur);
            cur = cdr(cur);
        }
        return SCM_BOOL_F;
    });
    v.define_proc("find-tail", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM cur = a[1];
        while (is_pair(cur)) {
            if (v.call1(a[0], car(cur)).is_true()) return cur;
            cur = cdr(cur);
        }
        return SCM_BOOL_F;
    });
    v.define_proc("take-while", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM head = SCM_EOL, tail = SCM_EOL, cur = a[1];
        while (is_pair(cur) && v.call1(a[0], car(cur)).is_true()) {
            SCM n = v.cons(car(cur), SCM_EOL);
            if (head.is_null()) head = tail = n; else { set_cdr(tail, n); tail = n; }
            cur = cdr(cur);
        }
        return head;
    });
    v.define_proc("take-while!", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM cur = a[1], prev = SCM_EOL;
        while (is_pair(cur) && v.call1(a[0], car(cur)).is_true()) {
            prev = cur; cur = cdr(cur);
        }
        if (prev.is_null()) return SCM_EOL;
        set_cdr(prev, SCM_EOL);
        return a[1];
    });
    v.define_proc("drop-while", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM cur = a[1];
        while (is_pair(cur) && v.call1(a[0], car(cur)).is_true()) cur = cdr(cur);
        return cur;
    });
    v.define_proc("span", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM head = SCM_EOL, tail = SCM_EOL, cur = a[1];
        while (is_pair(cur) && v.call1(a[0], car(cur)).is_true()) {
            SCM n = v.cons(car(cur), SCM_EOL);
            if (head.is_null()) head = tail = n; else { set_cdr(tail, n); tail = n; }
            cur = cdr(cur);
        }
        return v.make_values(v.list2(head, cur));
    });
    v.define_proc("span!", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM cur = a[1], prev = SCM_EOL;
        while (is_pair(cur) && v.call1(a[0], car(cur)).is_true()) { prev = cur; cur = cdr(cur); }
        if (prev.is_null()) return v.make_values(v.list2(SCM_EOL, a[1]));
        set_cdr(prev, SCM_EOL);
        return v.make_values(v.list2(a[1], cur));
    });
    v.define_proc("break", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM head = SCM_EOL, tail = SCM_EOL, cur = a[1];
        while (is_pair(cur) && v.call1(a[0], car(cur)).is_false()) {
            SCM n = v.cons(car(cur), SCM_EOL);
            if (head.is_null()) head = tail = n; else { set_cdr(tail, n); tail = n; }
            cur = cdr(cur);
        }
        return v.make_values(v.list2(head, cur));
    });
    v.define_proc("break!", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM cur = a[1], prev = SCM_EOL;
        while (is_pair(cur) && v.call1(a[0], car(cur)).is_false()) { prev = cur; cur = cdr(cur); }
        if (prev.is_null()) return v.make_values(v.list2(SCM_EOL, a[1]));
        set_cdr(prev, SCM_EOL);
        return v.make_values(v.list2(a[1], cur));
    });
    v.define_proc("list-index", 2, 0, true, [&v, check_proc_arity](const SCM* a, size_t nargs) -> SCM {
        SCM pred = a[0];
        std::vector<SCM> ls;
        ls.push_back(a[1]);
        SCM rest = nargs > 2 ? a[2] : SCM_EOL;
        while (is_pair(rest)) { ls.push_back(car(rest)); rest = cdr(rest); }
        if (ls.size() <= 2) check_proc_arity(pred, static_cast<int>(ls.size()), "list-index", 1);
        int64_t idx = 0;
        while (true) {
            bool done = false;
            for (size_t i = 0; i < ls.size(); ++i) {
                if (ls[i].is_null()) done = true;
                else if (!is_pair(ls[i])) v.wrong_type_arg("list-index", static_cast<int>(2 + i), ls[i], "proper list");
            }
            if (done) return SCM_BOOL_F;
            std::vector<SCM> cargs;
            for (auto& l : ls) {
                cargs.push_back(car(l)); l = cdr(l);
            }
            if (v.apply(pred, cargs.data(), cargs.size()).is_true()) return v.make_integer(idx);
            ++idx;
        }
    });

    // Alist / Copy / Append-reverse / Concatenate / Count / Delete / Remove / Partition / Lset / Member / Assoc / Map / For-each
    v.define_proc("alist-copy", 1, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM head = SCM_EOL, tail = SCM_EOL, cur = a[0];
        while (is_pair(cur)) {
            SCM e = car(cur);
            if (!is_pair(e)) v.wrong_type_arg("alist-copy", 1, a[0], "association list");
            SCM n = v.cons(v.cons(car(e), cdr(e)), SCM_EOL);
            if (head.is_null()) head = tail = n;
            else { set_cdr(tail, n); tail = n; }
            cur = cdr(cur);
        }
        if (!cur.is_null()) v.wrong_type_arg("alist-copy", 1, a[0], "proper list");
        return head;
    });
    v.define_proc("append-reverse", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM cur = a[0], res = a[1];
        while (is_pair(cur)) { res = v.cons(car(cur), res); cur = cdr(cur); }
        if (!cur.is_null()) v.wrong_type_arg("append-reverse", 1, a[0], "proper list");
        return res;
    });
    v.define_proc("append-reverse!", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        return scm_reverse_x(a[0], a[1]);
    });
    v.define_proc("concatenate", 1, 0, false, [&v](const SCM* a, size_t) -> SCM {
        ScmRootVec lists(v.heap());
        SCM cur = a[0];
        while (is_pair(cur)) { lists.push_back(car(cur)); cur = cdr(cur); }
        if (!cur.is_null()) v.wrong_type_arg("concatenate", 1, a[0], "proper list");
        if (lists.empty()) return SCM_EOL;
        SCM res = lists[lists.size() - 1];
        for (size_t i = lists.size() - 1; i > 0; --i) res = scm_append2(lists[i - 1], res);
        return res;
    });
    v.define_proc("concatenate!", 1, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM cur = a[0];
        while (is_pair(cur)) cur = cdr(cur);
        if (!cur.is_null()) v.wrong_type_arg("concatenate!", 1, a[0], "proper list");
        return v.apply_list(as_obj<VariableObj>(v.lookup("append!"))->value, a[0]);
    });
    v.define_proc("count", 2, 0, true, [&v, check_proc_arity](const SCM* a, size_t nargs) -> SCM {
        SCM pred = a[0];
        std::vector<SCM> ls;
        ls.push_back(a[1]);
        SCM rest = nargs > 2 ? a[2] : SCM_EOL;
        while (is_pair(rest)) { ls.push_back(car(rest)); rest = cdr(rest); }
        if (ls.size() <= 2) check_proc_arity(pred, static_cast<int>(ls.size()), "count", 1);
        int64_t cnt = 0;
        while (true) {
            bool done = false;
            for (size_t i = 0; i < ls.size(); ++i) {
                if (ls[i].is_null()) done = true;
                else if (!is_pair(ls[i])) v.wrong_type_arg("count", static_cast<int>(2 + i), ls[i], "proper list");
            }
            if (done) return v.make_integer(cnt);
            std::vector<SCM> cargs;
            for (auto& l : ls) {
                cargs.push_back(car(l));
                l = cdr(l);
            }
            if (v.apply(pred, cargs.data(), cargs.size()).is_true()) ++cnt;
        }
    });
    v.define_proc("delete", 2, 1, false, [&v](const SCM* a, size_t nargs) -> SCM {
        SCM eq = (nargs >= 3 && !a[2].is_undefined()) ? a[2] : SCM_UNDEFINED;
        SCM head = SCM_EOL, tail = SCM_EOL, cur = a[1];
        while (is_pair(cur)) {
            bool match = eq.is_undefined() ? scm_equal(a[0], car(cur)) : v.call2(eq, a[0], car(cur)).is_true();
            if (!match) {
                SCM n = v.cons(car(cur), SCM_EOL);
                if (head.is_null()) head = tail = n;
                else { set_cdr(tail, n); tail = n; }
            }
            cur = cdr(cur);
        }
        return head;
    });
    v.define_proc("delete!", 2, 1, false, [&v](const SCM* a, size_t nargs) -> SCM {
        SCM eq = (nargs >= 3 && !a[2].is_undefined()) ? a[2] : SCM_UNDEFINED;
        SCM head = SCM_EOL, prev = SCM_EOL, cur = a[1];
        while (is_pair(cur)) {
            SCM nxt = cdr(cur);
            bool match = eq.is_undefined() ? scm_equal(a[0], car(cur)) : v.call2(eq, a[0], car(cur)).is_true();
            if (match) {
                if (!prev.is_null()) set_cdr(prev, nxt);
            } else {
                if (head.is_null()) head = cur;
                prev = cur;
            }
            cur = nxt;
        }
        return head;
    });
    v.define_proc("delete-duplicates", 1, 1, false, [&v](const SCM* a, size_t nargs) -> SCM {
        SCM eq = (nargs >= 2 && !a[1].is_undefined()) ? a[1] : SCM_UNDEFINED;
        ScmRootVec kept(v.heap());
        SCM cur = a[0];
        while (is_pair(cur)) {
            SCM x = car(cur);
            bool dup = false;
            for (size_t i = 0; i < kept.size(); ++i) {
                if (eq.is_undefined() ? scm_equal(kept[i], x) : v.call2(eq, kept[i], x).is_true()) {
                    dup = true; break;
                }
            }
            if (!dup) kept.push_back(x);
            cur = cdr(cur);
        }
        return v.list_n(kept.data(), kept.size());
    });
    v.define_proc("delete-duplicates!", 1, 1, false, [&v](const SCM* a, size_t nargs) -> SCM {
        return v.apply(as_obj<VariableObj>(v.lookup("delete-duplicates"))->value, a, nargs);
    });
    v.define_proc("remove", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM head = SCM_EOL, tail = SCM_EOL, cur = a[1];
        while (is_pair(cur)) {
            if (v.call1(a[0], car(cur)).is_false()) {
                SCM n = v.cons(car(cur), SCM_EOL);
                if (head.is_null()) head = tail = n;
                else { set_cdr(tail, n); tail = n; }
            }
            cur = cdr(cur);
        }
        return head;
    });
    v.define_proc("remove!", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM head = SCM_EOL, prev = SCM_EOL, cur = a[1];
        while (is_pair(cur)) {
            SCM nxt = cdr(cur);
            if (v.call1(a[0], car(cur)).is_false()) {
                if (head.is_null()) head = cur;
                prev = cur;
            } else if (!prev.is_null()) {
                set_cdr(prev, nxt);
            }
            cur = nxt;
        }
        return head;
    });
    v.define_proc("partition", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        SCM ih = SCM_EOL, it = SCM_EOL, oh = SCM_EOL, ot = SCM_EOL, cur = a[1];
        while (is_pair(cur)) {
            SCM n = v.cons(car(cur), SCM_EOL);
            if (v.call1(a[0], car(cur)).is_true()) {
                if (ih.is_null()) ih = it = n; else { set_cdr(it, n); it = n; }
            } else {
                if (oh.is_null()) oh = ot = n; else { set_cdr(ot, n); ot = n; }
            }
            cur = cdr(cur);
        }
        if (!cur.is_null()) v.wrong_type_arg("partition", 2, a[1], "proper list");
        return v.make_values(v.list2(ih, oh));
    });
    v.define_proc("partition!", 2, 0, false, [&v](const SCM* a, size_t nargs) -> SCM {
        return v.apply(as_obj<VariableObj>(v.lookup("partition"))->value, a, nargs);
    });
    v.define_proc("length+", 1, 0, false, [&v](const SCM* a, size_t) -> SCM {
        int64_t n = scm_ilength(a[0]);
        return (n < 0) ? SCM_BOOL_F : v.make_integer(n);
    });
    v.define_proc("list-copy", 1, 0, false, [&v](const SCM* a, size_t) -> SCM {
        if (!is_pair(a[0])) return a[0];
        SCM head = SCM_EOL, tail = SCM_EOL, cur = a[0];
        while (is_pair(cur)) {
            SCM n = v.cons(car(cur), SCM_EOL);
            if (head.is_null()) head = tail = n; else { set_cdr(tail, n); tail = n; }
            cur = cdr(cur);
        }
        set_cdr(tail, cur);
        return head;
    });
    v.define_proc("list-tabulate", 2, 0, false, [&v](const SCM* a, size_t) -> SCM {
        int64_t n = scm_to_int64(a[0], "list-tabulate", 1);
        if (n < 0) v.out_of_range("list-tabulate", a[0]);
        SCM res = SCM_EOL;
        for (int64_t i = n - 1; i >= 0; --i) res = v.cons(v.call1(a[1], v.make_integer(i)), res);
        return res;
    });
    v.define_proc("lset-adjoin", 2, 0, true, [&v](const SCM* a, size_t nargs) -> SCM {
        SCM eq = a[0], lst = a[1], elts = nargs > 2 ? a[2] : SCM_EOL;
        while (is_pair(elts)) {
            SCM x = car(elts);
            bool found = false;
            SCM c = lst;
            while (is_pair(c)) {
                if (v.call2(eq, car(c), x).is_true()) { found = true; break; }
                c = cdr(c);
            }
            if (!found) lst = v.cons(x, lst);
            elts = cdr(elts);
        }
        return lst;
    });
    v.define_proc("lset-difference!", 2, 0, true, [&v, is_proc](const SCM* a, size_t nargs) -> SCM {
        SCM eq = a[0], lst1 = a[1], rest = nargs > 2 ? a[2] : SCM_EOL;
        if (!is_proc(eq)) v.wrong_type_arg("lset-difference!", 1, eq, "procedure");
        SCM head = SCM_EOL, tail = SCM_EOL, cur = lst1;
        while (is_pair(cur)) {
            SCM x = car(cur);
            bool found = false;
            SCM r = rest;
            while (is_pair(r) && !found) {
                SCM l = car(r);
                while (is_pair(l)) {
                    if (v.call2(eq, x, car(l)).is_true()) { found = true; break; }
                    l = cdr(l);
                }
                r = cdr(r);
            }
            if (!found) {
                SCM n = v.cons(x, SCM_EOL);
                if (head.is_null()) head = tail = n; else { set_cdr(tail, n); tail = n; }
            }
            cur = cdr(cur);
        }
        return head;
    });
    v.define_proc("member", 2, 1, false, [&v](const SCM* a, size_t nargs) -> SCM {
        SCM eq = (nargs >= 3 && !a[2].is_undefined()) ? a[2] : SCM_UNDEFINED;
        SCM cur = a[1];
        while (is_pair(cur)) {
            bool match = eq.is_undefined() ? scm_equal(a[0], car(cur)) : v.call2(eq, a[0], car(cur)).is_true();
            if (match) return cur;
            cur = cdr(cur);
        }
        return SCM_BOOL_F;
    });
    v.define_proc("assoc", 2, 1, false, [&v](const SCM* a, size_t nargs) -> SCM {
        SCM eq = (nargs >= 3 && !a[2].is_undefined()) ? a[2] : SCM_UNDEFINED;
        SCM cur = a[1];
        while (is_pair(cur)) {
            SCM e = car(cur);
            if (!is_pair(e)) v.wrong_type_arg("assoc", 2, a[1], "alist");
            bool match = eq.is_undefined() ? scm_equal(a[0], car(e)) : v.call2(eq, a[0], car(e)).is_true();
            if (match) return e;
            cur = cdr(cur);
        }
        return SCM_BOOL_F;
    });
    auto srfi1_map = [&v](const SCM* a, size_t nargs) -> SCM {
        SCM proc = a[0];
        std::vector<SCM> ls;
        ls.push_back(a[1]);
        SCM rest = nargs > 2 ? a[2] : SCM_EOL;
        while (is_pair(rest)) { ls.push_back(car(rest)); rest = cdr(rest); }
        SCM head = SCM_EOL, tail = SCM_EOL;
        while (true) {
            std::vector<SCM> cargs;
            for (auto& l : ls) {
                if (!is_pair(l)) return head;
                cargs.push_back(car(l)); l = cdr(l);
            }
            SCM n = v.cons(v.apply(proc, cargs.data(), cargs.size()), SCM_EOL);
            if (head.is_null()) head = tail = n; else { set_cdr(tail, n); tail = n; }
        }
    };
    v.define_proc("map", 2, 0, true, srfi1_map);
    v.define_proc("map-in-order", 2, 0, true, srfi1_map);
    v.define_proc("for-each", 2, 0, true, [&v](const SCM* a, size_t nargs) -> SCM {
        SCM proc = a[0];
        std::vector<SCM> ls;
        ls.push_back(a[1]);
        SCM rest = nargs > 2 ? a[2] : SCM_EOL;
        while (is_pair(rest)) { ls.push_back(car(rest)); rest = cdr(rest); }
        while (true) {
            std::vector<SCM> cargs;
            for (auto& l : ls) {
                if (!is_pair(l)) return SCM_UNSPECIFIED;
                cargs.push_back(car(l)); l = cdr(l);
            }
            v.apply(proc, cargs.data(), cargs.size());
        }
    });
}

void VM::init_srfi1_60() {
    register_extension("libguile-srfi-srfi-1-v-3", "scm_init_srfi_1", init_srfi_1_extension);
    register_extension("libguile-srfi-srfi-60-v-2", "scm_init_srfi_60", init_srfi_60_extension);
}

} // namespace guile
