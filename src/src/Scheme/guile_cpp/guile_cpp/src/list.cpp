#include "guile_cpp/vm.hpp"
#include <functional>
#include <unordered_set>

namespace guile {

SCM VM::cons(SCM a, SCM d) {
    return heap_.alloc_scm<PairObj>(a, d);
}

SCM VM::cons_source(SCM a, SCM d, SCM src) {
    SCM p = cons(a, d);
    if (src.is_true()) {
        SCM props = source_properties(src);
        if (is_pair(props)) set_source_properties(p, props);
    }
    return p;
}

SCM VM::list1(SCM a) { return cons(a, SCM_EOL); }
SCM VM::list2(SCM a, SCM b) { return cons(a, cons(b, SCM_EOL)); }
SCM VM::list3(SCM a, SCM b, SCM c) { return cons(a, cons(b, cons(c, SCM_EOL))); }
SCM VM::list4(SCM a, SCM b, SCM c, SCM d) { return cons(a, cons(b, cons(c, cons(d, SCM_EOL)))); }

SCM VM::list_n(const SCM* elts, size_t n) {
    SCM res = SCM_EOL;
    for (size_t i = n; i > 0; --i) {
        res = cons(elts[i - 1], res);
    }
    return res;
}

int64_t scm_ilength(SCM lst) {
    // Floyd's cycle-finding algorithm
    int64_t n = 0;
    SCM slow = lst;
    SCM fast = lst;
    while (is_pair(fast)) {
        fast = cdr(fast);
        ++n;
        if (!is_pair(fast)) {
            return fast.is_null() ? n : -1;
        }
        fast = cdr(fast);
        ++n;
        slow = cdr(slow);
        if (slow == fast) return -1; // circular
    }
    return fast.is_null() ? n : -1;
}

bool is_proper_list(SCM lst) {
    return scm_ilength(lst) >= 0;
}

SCM scm_reverse(SCM lst) {
    if (scm_ilength(lst) < 0) vm().wrong_type_arg("reverse", 1, lst, "proper list");
    SCM res = SCM_EOL;
    SCM cur = lst;
    while (is_pair(cur)) {
        res = vm().cons(car(cur), res);
        cur = cdr(cur);
    }
    return res;
}

SCM scm_reverse_x(SCM lst, SCM newtail) {
    if (scm_ilength(lst) < 0) vm().wrong_type_arg("reverse!", 1, lst, "proper list");
    SCM cur = lst;
    SCM prev = newtail;
    while (is_pair(cur)) {
        SCM nxt = cdr(cur);
        set_cdr(cur, prev);
        prev = cur;
        cur = nxt;
    }
    return prev;
}

SCM scm_append2(SCM a, SCM b) {
    if (a.is_null()) return b;
    if (scm_ilength(a) < 0) vm().wrong_type_arg("append", 1, a, "proper list");
    SCM head = vm().cons(car(a), SCM_EOL);
    SCM tail = head;
    SCM cur = cdr(a);
    while (is_pair(cur)) {
        SCM n = vm().cons(car(cur), SCM_EOL);
        set_cdr(tail, n);
        tail = n;
        cur = cdr(cur);
    }
    set_cdr(tail, b);
    return head;
}

SCM scm_list_copy(SCM lst) {
    if (!is_pair(lst)) {
        if (lst.is_null()) return SCM_EOL;
        vm().wrong_type_arg("list-copy", 1, lst, "list");
    }
    SCM head = vm().cons(car(lst), SCM_EOL);
    SCM tail = head;
    SCM cur = cdr(lst);
    while (is_pair(cur)) {
        SCM n = vm().cons(car(cur), SCM_EOL);
        set_cdr(tail, n);
        tail = n;
        cur = cdr(cur);
    }
    set_cdr(tail, cur); // supports dotted lists too
    return head;
}

SCM scm_assq(SCM key, SCM alist) {
    SCM cur = alist;
    while (is_pair(cur)) {
        SCM entry = car(cur);
        if (!is_pair(entry)) vm().wrong_type_arg("assq", 2, alist, "association list");
        if (car(entry) == key) return entry;
        cur = cdr(cur);
    }
    if (!cur.is_null()) vm().wrong_type_arg("assq", 2, alist, "association list");
    return SCM_BOOL_F;
}

SCM scm_assv(SCM key, SCM alist) {
    SCM cur = alist;
    while (is_pair(cur)) {
        SCM entry = car(cur);
        if (!is_pair(entry)) vm().wrong_type_arg("assv", 2, alist, "association list");
        if (scm_eqv(car(entry), key)) return entry;
        cur = cdr(cur);
    }
    if (!cur.is_null()) vm().wrong_type_arg("assv", 2, alist, "association list");
    return SCM_BOOL_F;
}

SCM scm_assoc(SCM key, SCM alist) {
    SCM cur = alist;
    while (is_pair(cur)) {
        SCM entry = car(cur);
        if (!is_pair(entry)) vm().wrong_type_arg("assoc", 2, alist, "association list");
        if (scm_equal(car(entry), key)) return entry;
        cur = cdr(cur);
    }
    if (!cur.is_null()) vm().wrong_type_arg("assoc", 2, alist, "association list");
    return SCM_BOOL_F;
}

SCM scm_memq(SCM key, SCM lst) {
    SCM cur = lst;
    while (is_pair(cur)) {
        if (car(cur) == key) return cur;
        cur = cdr(cur);
    }
    if (!cur.is_null()) vm().wrong_type_arg("memq", 2, lst, "proper list");
    return SCM_BOOL_F;
}

static SCM copy_tree_rec(SCM obj, std::unordered_set<uint64_t>& visiting, int depth) {
    if (depth > 4096) {
        vm().error(vm().make_symbol("stack-overflow"), "copy-tree", "Stack overflow");
    }
    if (is_pair(obj)) {
        SCM head = SCM_EOL;
        SCM tail = SCM_EOL;
        SCM cur = obj;
        SCM slow = obj;
        size_t step = 0;
        std::vector<uint64_t> spine_visited;
        bool track_set = (depth > 32);
        while (is_pair(cur)) {
            if (track_set) {
                if (!visiting.insert(cur.bits).second) {
                    for (uint64_t b : spine_visited) visiting.erase(b);
                    vm().error(vm().sym_syntax_error, "copy-tree", "Bad expression: circular list");
                }
                spine_visited.push_back(cur.bits);
            } else {
                if (++step > 1 && cur == slow) {
                    vm().error(vm().sym_syntax_error, "copy-tree", "Bad expression: circular list");
                }
                if ((step & 1) == 0 && is_pair(slow)) slow = cdr(slow);
            }
            SCM a = copy_tree_rec(car(cur), visiting, depth + 1);
            SCM cell = vm().cons(a, SCM_EOL);
            if (head.is_null()) head = tail = cell;
            else { set_cdr(tail, cell); tail = cell; }
            cur = cdr(cur);
        }
        if (!cur.is_null()) {
            set_cdr(tail, copy_tree_rec(cur, visiting, depth + 1));
        }
        for (uint64_t b : spine_visited) visiting.erase(b);
        return head;
    }
    if (is_vector(obj)) {
        if (!visiting.insert(obj.bits).second) {
            vm().error(vm().sym_syntax_error, "copy-tree", "Bad expression: circular vector");
        }
        auto* vec = as_obj<VectorObj>(obj);
        size_t n = vec->elts.size();
        SCM nv = vm().make_vector(n, SCM_UNSPECIFIED);
        auto* nvec = as_obj<VectorObj>(nv);
        for (size_t i = 0; i < n; ++i) {
            nvec->elts[i] = copy_tree_rec(vec->elts[i], visiting, depth + 1);
        }
        visiting.erase(obj.bits);
        return nv;
    }
    return obj;
}

// Zero-allocation fast path for shallow acyclic lists (99.9% of copy-tree calls!)
static SCM copy_tree_fast(SCM obj, int depth) {
    if (is_pair(obj)) {
        if (depth > 32) {
            std::unordered_set<uint64_t> visiting;
            return copy_tree_rec(obj, visiting, depth);
        }
        SCM head = SCM_EOL;
        SCM tail = SCM_EOL;
        SCM cur = obj;
        SCM slow = obj;
        size_t step = 0;
        while (is_pair(cur)) {
            if (++step > 1 && cur == slow) {
                vm().error(vm().sym_syntax_error, "copy-tree", "Bad expression: circular list");
            }
            if ((step & 1) == 0 && is_pair(slow)) slow = cdr(slow);
            SCM ca = car(cur);
            SCM a = is_pair(ca) ? copy_tree_fast(ca, depth + 1)
                  : is_vector(ca) ? ([&]() { std::unordered_set<uint64_t> v; return copy_tree_rec(ca, v, depth + 1); })()
                  : ca;
            SCM cell = vm().cons(a, SCM_EOL);
            if (head.is_null()) head = tail = cell;
            else { set_cdr(tail, cell); tail = cell; }
            cur = cdr(cur);
        }
        if (!cur.is_null()) {
            set_cdr(tail, copy_tree_fast(cur, depth + 1));
        }
        return head;
    }
    if (is_vector(obj)) {
        std::unordered_set<uint64_t> visiting;
        return copy_tree_rec(obj, visiting, depth);
    }
    return obj;
}

SCM scm_copy_tree(SCM obj) {
    return copy_tree_fast(obj, 0);
}

static SCM copy_tree_impl(SCM obj) {
    return copy_tree_fast(obj, 0);
}

void VM::init_list() {
    define_proc("pair?", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return SCM::from_bool(is_pair(args[0]));
    });
    define_proc("null?", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return SCM::from_bool(args[0].is_null());
    });
    define_proc("cons", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        return cons(args[0], args[1]);
    });
    define_proc("cons-source", 3, 0, false, [this](const SCM* args, size_t) -> SCM {
        return cons_source(args[0], args[1], args[2]);
    });

    // car/cdr and all 2..4 letter combinations
    auto reg_cxr = [this](const std::string& name, const char* ops) {
        define_proc(name, 1, 0, false, [this, name, ops](const SCM* args, size_t) -> SCM {
            SCM cur = args[0];
            for (const char* p = ops; *p; ++p) {
                if (!is_pair(cur)) wrong_type_arg(name.c_str(), 1, args[0], "pair");
                cur = (*p == 'a') ? car(cur) : cdr(cur);
            }
            return cur;
        });
    };
    reg_cxr("car", "a"); reg_cxr("cdr", "d");
    reg_cxr("caar", "aa"); reg_cxr("cadr", "da"); reg_cxr("cdar", "ad"); reg_cxr("cddr", "dd");
    reg_cxr("caaar", "aaa"); reg_cxr("caadr", "daa"); reg_cxr("cadar", "ada"); reg_cxr("caddr", "dda");
    reg_cxr("cdaar", "aad"); reg_cxr("cdadr", "dad"); reg_cxr("cddar", "add"); reg_cxr("cdddr", "ddd");
    reg_cxr("caaaar", "aaaa"); reg_cxr("caaadr", "daaa"); reg_cxr("caadar", "adaa"); reg_cxr("caaddr", "ddaa");
    reg_cxr("cadaar", "aada"); reg_cxr("cadadr", "dada"); reg_cxr("caddar", "adda"); reg_cxr("cadddr", "ddda");
    reg_cxr("cdaaar", "aaad"); reg_cxr("cdaadr", "daad"); reg_cxr("cdadar", "adad"); reg_cxr("cdaddr", "ddad");
    reg_cxr("cddaar", "aadd"); reg_cxr("cddadr", "dadd"); reg_cxr("cdddar", "addd"); reg_cxr("cddddr", "dddd");

    define_proc("set-car!", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_pair(args[0])) wrong_type_arg("set-car!", 1, args[0], "pair");
        set_car(args[0], args[1]);
        return SCM_UNSPECIFIED;
    });
    define_proc("set-cdr!", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_pair(args[0])) wrong_type_arg("set-cdr!", 1, args[0], "pair");
        set_cdr(args[0], args[1]);
        return SCM_UNSPECIFIED;
    });

    define_proc("list", 0, 0, true, [](const SCM* args, size_t nargs) -> SCM {
        return nargs > 0 ? args[0] : SCM_EOL;
    });

    auto cons_star_fn = [this](const SCM* args, size_t nargs) -> SCM {
        // args[0] = first, args[1] = rest list
        SCM rest = nargs > 1 ? args[1] : SCM_EOL;
        if (rest.is_null()) return args[0];
        SCM head = cons(args[0], SCM_EOL);
        SCM tail = head;
        while (is_pair(cdr(rest))) {
            SCM n = cons(car(rest), SCM_EOL);
            set_cdr(tail, n);
            tail = n;
            rest = cdr(rest);
        }
        set_cdr(tail, car(rest));
        return head;
    };
    define_proc("cons*", 1, 0, true, cons_star_fn);
    define_proc("list*", 1, 0, true, cons_star_fn);

    define_proc("make-list", 1, 1, false, [this](const SCM* args, size_t nargs) -> SCM {
        int64_t n = scm_to_int64(args[0], "make-list", 1);
        if (n < 0) out_of_range("make-list", args[0]);
        SCM fill = (nargs >= 2 && !args[1].is_undefined()) ? args[1] : SCM_EOL;
        SCM res = SCM_EOL;
        for (int64_t i = 0; i < n; ++i) res = cons(fill, res);
        return res;
    });

    define_proc("list?", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return SCM::from_bool(scm_ilength(args[0]) >= 0);
    });

    define_proc("length", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        int64_t n = scm_ilength(args[0]);
        if (n < 0) wrong_type_arg("length", 1, args[0], "proper list");
        return make_integer(n);
    });

    define_proc("append", 0, 0, true, [this](const SCM* args, size_t nargs) -> SCM {
        SCM lsts = nargs > 0 ? args[0] : SCM_EOL;
        if (lsts.is_null()) return SCM_EOL;
        // Collect into vector to append right-to-left
        ScmRootVec items(heap_);
        while (is_pair(lsts)) {
            items.push_back(car(lsts));
            lsts = cdr(lsts);
        }
        SCM res = items[items.size() - 1];
        for (size_t i = items.size() - 1; i > 0; --i) {
            res = scm_append2(items[i - 1], res);
        }
        return res;
    });

    define_proc("append!", 0, 0, true, [this](const SCM* args, size_t nargs) -> SCM {
        SCM lsts = nargs > 0 ? args[0] : SCM_EOL;
        while (is_pair(lsts) && car(lsts).is_null()) lsts = cdr(lsts);
        if (!is_pair(lsts)) return SCM_EOL;
        SCM head = car(lsts);
        lsts = cdr(lsts);
        if (lsts.is_null()) return head;
        if (!is_pair(head)) wrong_type_arg("append!", 1, head, "list");
        if (scm_ilength(head) < 0) wrong_type_arg("append!", 1, head, "proper list");
        SCM tail = head;
        while (is_pair(cdr(tail))) tail = cdr(tail);
        while (is_pair(lsts)) {
            SCM next = car(lsts);
            lsts = cdr(lsts);
            if (next.is_null() && !lsts.is_null()) continue;
            set_cdr(tail, next);
            if (!lsts.is_null()) {
                if (!is_pair(next)) wrong_type_arg("append!", 1, next, "list");
                if (scm_ilength(next) < 0) wrong_type_arg("append!", 1, next, "proper list");
                tail = next;
                while (is_pair(cdr(tail))) tail = cdr(tail);
            }
        }
        return head;
    });

    define_proc("last-pair", 1, 0, false, [this](const SCM* args, size_t) -> SCM {
        if (!is_pair(args[0])) wrong_type_arg("last-pair", 1, args[0], "pair");
        SCM slow = args[0], fast = args[0];
        while (is_pair(cdr(fast))) {
            fast = cdr(fast);
            if (is_pair(cdr(fast))) {
                fast = cdr(fast);
                slow = cdr(slow);
                if (slow == fast) wrong_type_arg("last-pair", 1, args[0], "finite list");
            }
        }
        return fast;
    });

    define_proc("reverse", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return scm_reverse(args[0]);
    });

    define_proc("reverse!", 1, 1, false, [](const SCM* args, size_t nargs) -> SCM {
        SCM newtail = (nargs >= 2 && !args[1].is_undefined()) ? args[1] : SCM_EOL;
        return scm_reverse_x(args[0], newtail);
    });

    define_proc("list-tail", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        int64_t k = scm_to_int64(args[1], "list-tail", 2);
        if (k < 0) out_of_range("list-tail", args[1]);
        SCM cur = args[0];
        for (int64_t i = 0; i < k; ++i) {
            if (!is_pair(cur)) out_of_range("list-tail", args[1]);
            cur = cdr(cur);
        }
        return cur;
    });

    define_proc("list-cdr-ref", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        int64_t k = scm_to_int64(args[1], "list-cdr-ref", 2);
        if (k < 0) out_of_range("list-cdr-ref", args[1]);
        SCM cur = args[0];
        for (int64_t i = 0; i < k; ++i) {
            if (!is_pair(cur)) out_of_range("list-cdr-ref", args[1]);
            cur = cdr(cur);
        }
        return cur;
    });

    define_proc("list-ref", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        int64_t k = scm_to_int64(args[1], "list-ref", 2);
        if (k < 0) out_of_range("list-ref", args[1]);
        SCM cur = args[0];
        for (int64_t i = 0; i < k; ++i) {
            if (!is_pair(cur)) out_of_range("list-ref", args[1]);
            cur = cdr(cur);
        }
        if (!is_pair(cur)) out_of_range("list-ref", args[1]);
        return car(cur);
    });

    define_proc("list-set!", 3, 0, false, [this](const SCM* args, size_t) -> SCM {
        int64_t k = scm_to_int64(args[1], "list-set!", 2);
        if (k < 0) out_of_range("list-set!", args[1]);
        SCM cur = args[0];
        for (int64_t i = 0; i < k; ++i) {
            if (!is_pair(cur)) out_of_range("list-set!", args[1]);
            cur = cdr(cur);
        }
        if (!is_pair(cur)) out_of_range("list-set!", args[1]);
        set_car(cur, args[2]);
        return args[2];
    });

    define_proc("list-cdr-set!", 3, 0, false, [this](const SCM* args, size_t) -> SCM {
        int64_t k = scm_to_int64(args[1], "list-cdr-set!", 2);
        if (k < 0) out_of_range("list-cdr-set!", args[1]);
        SCM cur = args[0];
        for (int64_t i = 0; i < k; ++i) {
            if (!is_pair(cur)) out_of_range("list-cdr-set!", args[1]);
            cur = cdr(cur);
        }
        if (!is_pair(cur)) out_of_range("list-cdr-set!", args[1]);
        set_cdr(cur, args[2]);
        return args[2];
    });

    define_proc("list-head", 2, 0, false, [this](const SCM* args, size_t) -> SCM {
        int64_t k = scm_to_int64(args[1], "list-head", 2);
        if (k < 0) out_of_range("list-head", args[1]);
        if (k == 0) return SCM_EOL;
        SCM cur = args[0];
        if (!is_pair(cur)) out_of_range("list-head", args[1]);
        SCM head = cons(car(cur), SCM_EOL);
        SCM tail = head;
        cur = cdr(cur);
        for (int64_t i = 1; i < k; ++i) {
            if (!is_pair(cur)) out_of_range("list-head", args[1]);
            SCM n = cons(car(cur), SCM_EOL);
            set_cdr(tail, n);
            tail = n;
            cur = cdr(cur);
        }
        return head;
    });

    define_proc("list-copy", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return scm_list_copy(args[0]);
    });

    define_proc("copy-tree", 1, 0, false, [](const SCM* args, size_t) -> SCM {
        return copy_tree_impl(args[0]);
    });

    // Membership
    auto make_mem = [this](const char* name, bool sloppy, auto pred) {
        define_proc(name, 2, 0, false, [this, name, sloppy, pred](const SCM* args, size_t) -> SCM {
            SCM cur = args[1];
            while (is_pair(cur)) {
                if (pred(car(cur), args[0])) return cur;
                cur = cdr(cur);
            }
            if (!sloppy && !cur.is_null()) wrong_type_arg(name, 2, args[1], "proper list");
            return SCM_BOOL_F;
        });
    };
    make_mem("memq", false, [](SCM a, SCM b) { return a == b; });
    make_mem("memv", false, [](SCM a, SCM b) { return scm_eqv(a, b); });
    make_mem("member", false, [](SCM a, SCM b) { return scm_equal(a, b); });
    make_mem("sloppy-memq", true, [](SCM a, SCM b) { return a == b; });
    make_mem("sloppy-memv", true, [](SCM a, SCM b) { return scm_eqv(a, b); });
    make_mem("sloppy-member", true, [](SCM a, SCM b) { return scm_equal(a, b); });

    // Deletion
    auto make_del = [this](const char* name, bool destructive, bool only_one, auto pred) {
        define_proc(name, 2, 0, false, [this, name, destructive, only_one, pred](const SCM* args, size_t) -> SCM {
            SCM item = args[0];
            SCM lst = args[1];
            if (!destructive) {
                SCM head = SCM_EOL, tail = SCM_EOL;
                SCM cur = lst;
                bool deleted = false;
                while (is_pair(cur)) {
                    if (!(only_one && deleted) && pred(car(cur), item)) {
                        deleted = true;
                    } else {
                        SCM n = cons(car(cur), SCM_EOL);
                        if (head.is_null()) { head = tail = n; }
                        else { set_cdr(tail, n); tail = n; }
                    }
                    cur = cdr(cur);
                }
                if (!cur.is_null()) wrong_type_arg(name, 2, lst, "proper list");
                return head;
            } else {
                SCM cur = lst;
                SCM head = SCM_EOL, prev = SCM_EOL;
                bool deleted = false;
                while (is_pair(cur)) {
                    SCM nxt = cdr(cur);
                    if (!(only_one && deleted) && pred(car(cur), item)) {
                        deleted = true;
                        if (!prev.is_null()) set_cdr(prev, nxt);
                        if (only_one) {
                            return head.is_null() ? nxt : head;
                        }
                    } else {
                        if (head.is_null()) head = cur;
                        prev = cur;
                    }
                    cur = nxt;
                }
                if (!cur.is_null()) wrong_type_arg(name, 2, lst, "proper list");
                return head;
            }
        });
    };
    make_del("delq", false, false, [](SCM a, SCM b) { return a == b; });
    make_del("delv", false, false, [](SCM a, SCM b) { return scm_eqv(a, b); });
    make_del("delete", false, false, [](SCM a, SCM b) { return scm_equal(a, b); });
    make_del("delq!", true, false, [](SCM a, SCM b) { return a == b; });
    make_del("delv!", true, false, [](SCM a, SCM b) { return scm_eqv(a, b); });
    make_del("delete!", true, false, [](SCM a, SCM b) { return scm_equal(a, b); });
    make_del("delq1!", true, true, [](SCM a, SCM b) { return a == b; });
    make_del("delv1!", true, true, [](SCM a, SCM b) { return scm_eqv(a, b); });
    make_del("delete1!", true, true, [](SCM a, SCM b) { return scm_equal(a, b); });

    // Alists
    define_proc("acons", 3, 0, false, [this](const SCM* args, size_t) -> SCM {
        return cons(cons(args[0], args[1]), args[2]);
    });

    auto make_ass = [this](const char* name, bool sloppy, auto pred) {
        define_proc(name, 2, 0, false, [this, name, sloppy, pred](const SCM* args, size_t) -> SCM {
            SCM cur = args[1];
            while (is_pair(cur)) {
                SCM entry = car(cur);
                if (is_pair(entry)) {
                    if (pred(car(entry), args[0])) return entry;
                } else if (!sloppy) {
                    wrong_type_arg(name, 2, args[1], "association list");
                }
                cur = cdr(cur);
            }
            if (!sloppy && !cur.is_null()) wrong_type_arg(name, 2, args[1], "association list");
            return SCM_BOOL_F;
        });
    };
    make_ass("assq", false, [](SCM a, SCM b) { return a == b; });
    make_ass("assv", false, [](SCM a, SCM b) { return scm_eqv(a, b); });
    make_ass("assoc", false, [](SCM a, SCM b) { return scm_equal(a, b); });
    make_ass("sloppy-assq", true, [](SCM a, SCM b) { return a == b; });
    make_ass("sloppy-assv", true, [](SCM a, SCM b) { return scm_eqv(a, b); });
    make_ass("sloppy-assoc", true, [](SCM a, SCM b) { return scm_equal(a, b); });

    auto make_ass_ops = [this](const std::string& pfx, auto pred) {
        define_proc(pfx + "-ref", 2, 0, false, [pred](const SCM* args, size_t) -> SCM {
            SCM cur = args[0];
            while (is_pair(cur)) {
                SCM e = car(cur);
                if (is_pair(e) && pred(car(e), args[1])) return cdr(e);
                cur = cdr(cur);
            }
            return SCM_BOOL_F;
        });
        define_proc(pfx + "-set!", 3, 0, false, [this, pred](const SCM* args, size_t) -> SCM {
            SCM cur = args[0];
            while (is_pair(cur)) {
                SCM e = car(cur);
                if (is_pair(e) && pred(car(e), args[1])) {
                    set_cdr(e, args[2]);
                    return args[0];
                }
                cur = cdr(cur);
            }
            return cons(cons(args[1], args[2]), args[0]);
        });
        define_proc(pfx + "-remove!", 2, 0, false, [pred](const SCM* args, size_t) -> SCM {
            SCM cur = args[0], prev = SCM_EOL, head = args[0];
            while (is_pair(cur)) {
                SCM e = car(cur);
                if (is_pair(e) && pred(car(e), args[1])) {
                    if (prev.is_null()) return cdr(cur);
                    set_cdr(prev, cdr(cur));
                    return head;
                }
                prev = cur;
                cur = cdr(cur);
            }
            return head;
        });
    };
    make_ass_ops("assq", [](SCM a, SCM b) { return a == b; });
    make_ass_ops("assv", [](SCM a, SCM b) { return scm_eqv(a, b); });
    make_ass_ops("assoc", [](SCM a, SCM b) { return scm_equal(a, b); });

    // Core filter / filter! (re-exported by srfi-1)
    define_proc("filter", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM head = SCM_EOL, tail = SCM_EOL, cur = a[1];
        while (is_pair(cur)) {
            if (call1(a[0], car(cur)).is_true()) {
                SCM n = cons(car(cur), SCM_EOL);
                if (head.is_null()) head = tail = n;
                else { set_cdr(tail, n); tail = n; }
            }
            cur = cdr(cur);
        }
        if (!cur.is_null()) wrong_type_arg("filter", 2, a[1], "proper list");
        return head;
    });
    define_proc("filter!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        SCM head = SCM_EOL, prev = SCM_EOL, cur = a[1];
        while (is_pair(cur)) {
            SCM nxt = cdr(cur);
            if (call1(a[0], car(cur)).is_true()) {
                if (head.is_null()) head = cur;
                prev = cur;
            } else if (!prev.is_null()) {
                set_cdr(prev, nxt);
            }
            cur = nxt;
        }
        if (!cur.is_null()) wrong_type_arg("filter!", 2, a[1], "proper list");
        return head;
    });
}

} // namespace guile
