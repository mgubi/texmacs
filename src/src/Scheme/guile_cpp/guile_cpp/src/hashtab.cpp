#include "guile_cpp/vm.hpp"
#include <cstring>

namespace guile {

uint64_t scm_hash_string(const char* s, size_t n) {
    uint64_t h = 14695981039346656037ULL;
    for (size_t i = 0; i < n; i++) {
        h ^= static_cast<unsigned char>(s[i]);
        h *= 1099511628211ULL;
    }
    return h;
}

static inline uint64_t mod_bucket_size(uint64_t h, uint64_t n) noexcept {
    if (GCPP_LIKELY((n & (n - 1)) == 0)) return h & (n - 1);
    return h % n;
}

uint64_t scm_ihashq(SCM obj, uint64_t n) {
    if (n == 0) return 0;
    uint64_t h = obj.bits;
    if (is_keyword(obj)) h = as_obj<KeywordObj>(obj)->symbol.bits ^ 0x9e3779b97f4a7c15ULL;
    else if (is_symbol(obj)) h = as_obj<SymbolObj>(obj)->hash;
    h ^= (h >> 33);
    h *= 0xff51afd7ed558ccdULL;
    h ^= (h >> 33);
    return mod_bucket_size(h, n);
}

uint64_t scm_ihashv(SCM obj, uint64_t n) {
    if (n == 0) return 0;
    if (is_real_obj(obj)) {
        double d = as_obj<RealObj>(obj)->val;
        if (d == 0.0) d = 0.0;
        uint64_t bits = 0;
        std::memcpy(&bits, &d, sizeof(bits));
        return (bits ^ (bits >> 32)) % n;
    }
    if (is_bignum(obj)) {
        return as_obj<BignumObj>(obj)->val.low_u64() % n;
    }
    if (is_fraction(obj)) {
        auto* f = as_obj<FractionObj>(obj);
        return (scm_ihashv(f->num, n) * 31 + scm_ihashv(f->den, n)) % n;
    }
    if (is_complex_obj(obj)) {
        auto* c = as_obj<ComplexObj>(obj);
        if (c->imag == 0.0) {
            double d = (c->real == 0.0) ? 0.0 : c->real;
            uint64_t bits = 0;
            std::memcpy(&bits, &d, sizeof(bits));
            return (bits ^ (bits >> 32)) % n;
        }
        uint64_t b1 = 0, b2 = 0;
        std::memcpy(&b1, &c->real, sizeof(b1));
        std::memcpy(&b2, &c->imag, sizeof(b2));
        return (b1 ^ (b2 * 31)) % n;
    }
    return scm_ihashq(obj, n);
}

static uint64_t hash_equal_rec(SCM obj, int depth) {
    if (depth > 16) return 0x55555555ULL;
    if (!obj.is_heap()) {
        uint64_t h = obj.bits;
        h ^= (h >> 33);
        h *= 0xff51afd7ed558ccdULL;
        h ^= (h >> 33);
        return h;
    }
    ObjTag t = obj.to_obj()->tag;
    switch (t) {
    case ObjTag::Symbol:
        return as_obj<SymbolObj>(obj)->hash;
    case ObjTag::String:
        return scm_hash_string(as_obj<StringObj>(obj)->chars(), as_obj<StringObj>(obj)->len);
    case ObjTag::Pair: {
        // Iterative cdr walk (bounded to 16 elements) for O(1) stack & fast list hashing
        uint64_t h = 0x9e3779b97f4a7c15ULL;
        SCM cur = obj;
        for (int i = 0; i < 16 && is_pair(cur); ++i) {
            h = h * 31ULL + hash_equal_rec(car(cur), depth + 1) + 7ULL;
            cur = cdr(cur);
        }
        if (!cur.is_null() && !is_pair(cur)) {
            h = h * 31ULL + hash_equal_rec(cur, depth + 1);
        }
        return h;
    }
    case ObjTag::Vector: {
        auto* v = as_obj<VectorObj>(obj);
        uint64_t h = static_cast<size_t>(N(v->elts));
        size_t lim = std::min<size_t>(static_cast<size_t>(N(v->elts)), 16);
        for (size_t i = 0; i < lim; ++i) {
            h = h * 31ULL + hash_equal_rec(v->elts[i], depth + 1);
        }
        return h;
    }
    case ObjTag::WeakVector: {
        auto* v = as_obj<WeakVectorObj>(obj);
        uint64_t h = static_cast<size_t>(N(v->elts));
        size_t lim = std::min<size_t>(static_cast<size_t>(N(v->elts)), 16);
        for (size_t i = 0; i < lim; ++i) {
            h = h * 31ULL + hash_equal_rec(v->elts[i], depth + 1);
        }
        return h;
    }
    case ObjTag::CharSet: {
        auto* cs = as_obj<CharSetObj>(obj);
        return scm_hash_string(string(reinterpret_cast<const char*>(cs->bits), sizeof(cs->bits)));
    }
    case ObjTag::Values:
        return hash_equal_rec(as_obj<ValuesObj>(obj)->values, depth + 1) ^ 0x12345678ULL;
    case ObjTag::Real:
    case ObjTag::Bignum:
    case ObjTag::Fraction:
    case ObjTag::Complex:
    case ObjTag::Keyword:
        return scm_ihashv(obj, UINT64_MAX);
    case ObjTag::Smob: {
        auto* sm = as_obj<SmobObj>(obj);
        auto& info = vm().smob_info(sm->smob_type);
        if (info.hash_fn) return info.hash_fn(obj);
        return 263ULL;
    }
    case ObjTag::Struct:
    case ObjTag::Array:
    case ObjTag::BitVector:
    case ObjTag::UniformVector:
        // Structural or GOOPS-extensible equal? -> constant hash preserves (equal? a b) => same bucket
        return 263ULL;
    default:
        // Closure, Subr, Macro, Variable, Port, Fluid, DynamicState, HashTable, Promise, etc.:
        // equal? is strictly pointer identity (a == b), so hash by pointer!
        return scm_ihashq(obj, UINT64_MAX);
    }
}

uint64_t scm_ihash(SCM obj, uint64_t n) {
    if (n == 0) return 0;
    uint64_t h = hash_equal_rec(obj, 0);
    h ^= (h >> 33);
    h *= 0xff51afd7ed558ccdULL;
    h ^= (h >> 33);
    return mod_bucket_size(h, n);
}

static size_t next_prime_bucket_size(size_t n) {
    static const size_t primes[] = {
        31, 61, 113, 223, 431, 883, 1759, 3517, 7039, 14083,
        28181, 56377, 112757, 225527, 451057, 902119, 1804247, 3608503
    };
    for (size_t p : primes) {
        if (p >= n) return p;
    }
    return n | 1;
}

SCM VM::make_hashtable(size_t nbuckets, WeakKind weak) {
    return heap_.alloc_scm<HashTableObj>(next_prime_bucket_size(nbuckets), weak);
}

static void rehash_table(HashTableObj* ht, int mode) {
    size_t new_n = next_prime_bucket_size(static_cast<size_t>(N(ht->buckets)) + 1);
    array<SCM> nb = array_fill<SCM>(new_n, SCM_EOL);
    for (SCM b : ht->buckets) {
        SCM cur = b;
        while (is_pair(cur)) {
            SCM nxt = cdr(cur);
            SCM entry = car(cur);
            SCM k = car(entry);
            uint64_t idx = (mode == 1) ? scm_ihashq(k, new_n)
                         : (mode == 2) ? scm_ihashv(k, new_n)
                                       : scm_ihash(k, new_n);
            set_cdr(cur, nb[idx]);
            nb[idx] = cur;
            cur = nxt;
        }
    }
    ht->buckets = nb;
}

static inline bool key_match(SCM a, SCM b, int mode) {
    if (a == b) return true;
    if (mode == 1) {
        if (is_keyword(a) && is_keyword(b)) return as_obj<KeywordObj>(a)->symbol == as_obj<KeywordObj>(b)->symbol;
        return false;
    }
    if (mode == 2) return scm_eqv(a, b);
    // Fast-path for mode == 3 (equal?): symbols & immediates only equal themselves (already failed a == b)!
    if (!b.is_heap() || is_symbol(b)) return false;
    if (is_string(b)) {
        return is_string(a) && string_obj_equal(as_obj<StringObj>(a), as_obj<StringObj>(b));
    }
    return scm_equal(a, b);
}

static uint64_t key_hash(SCM k, size_t n, int mode) {
    if (mode == 1) return scm_ihashq(k, n);
    if (mode == 2) return scm_ihashv(k, n);
    return scm_ihash(k, n);
}

static array<SCM>* get_table_buckets(SCM table, const char* subr) {
    if (is_hashtable(table)) return &as_obj<HashTableObj>(table)->buckets;
    if (is_weak_vector(table)) return &as_obj<WeakVectorObj>(table)->elts;
    if (is_vector(table)) return &as_obj<VectorObj>(table)->elts;
    vm().wrong_type_arg(subr, 1, table, "hash table");
}

SCM hash_get_handle(SCM table, SCM key, int mode) {
    auto* buckets = get_table_buckets(table, "hash-get-handle");
    if ((N(*buckets) == 0)) return SCM_BOOL_F;
    uint64_t idx = key_hash(key, static_cast<size_t>(N(*buckets)), mode);
    SCM cur = (*buckets)[idx];
    while (is_pair(cur)) {
        SCM entry = car(cur);
        if (is_pair(entry) && key_match(car(entry), key, mode)) return entry;
        cur = cdr(cur);
    }
    return SCM_BOOL_F;
}

SCM hash_create_handle(SCM table, SCM key, SCM init, int mode) {
    auto* buckets = get_table_buckets(table, "hash-create-handle!");
    if ((N(*buckets) == 0)) vm().out_of_range("hash-create-handle!", table);
    uint64_t idx = key_hash(key, static_cast<size_t>(N(*buckets)), mode);
    SCM cur = (*buckets)[idx];
    while (is_pair(cur)) {
        SCM entry = car(cur);
        if (is_pair(entry) && key_match(car(entry), key, mode)) return entry;
        cur = cdr(cur);
    }
    SCM entry = vm().cons(key, init);
    (*buckets)[idx] = vm().cons(entry, (*buckets)[idx]);
    if (is_hashtable(table)) {
        auto* ht = as_obj<HashTableObj>(table);
        ++ht->count;
        if (ht->count > static_cast<size_t>(N(ht->buckets)) * 9 / 10) {
            rehash_table(ht, mode);
        }
        if (mode == 1 && ht->is_module_obarray) {
            if (is_symbol(key)) ++as_obj<SymbolObj>(key)->def_epoch;
            else ++vm().module_epoch;
        }
    }
    return entry;
}

SCM hash_ref(SCM table, SCM key, SCM dflt, int mode) {
    SCM h = hash_get_handle(table, key, mode);
    return is_pair(h) ? cdr(h) : dflt;
}

SCM hash_set(SCM table, SCM key, SCM val, int mode) {
    SCM h = hash_create_handle(table, key, val, mode);
    if (cdr(h) != val) {
        set_cdr(h, val);
        if (mode == 1 && is_hashtable(table) && as_obj<HashTableObj>(table)->is_module_obarray) {
            if (is_symbol(key)) ++as_obj<SymbolObj>(key)->def_epoch;
            else ++vm().module_epoch;
        }
    }
    return val;
}

SCM hash_remove(SCM table, SCM key, int mode) {
    auto* buckets = get_table_buckets(table, "hash-remove!");
    if ((N(*buckets) == 0)) return SCM_BOOL_F;
    uint64_t idx = key_hash(key, static_cast<size_t>(N(*buckets)), mode);
    SCM prev = SCM_EOL;
    SCM cur = (*buckets)[idx];
    while (is_pair(cur)) {
        SCM entry = car(cur);
        if (is_pair(entry) && key_match(car(entry), key, mode)) {
            if (prev.is_null()) (*buckets)[idx] = cdr(cur);
            else set_cdr(prev, cdr(cur));
            if (is_hashtable(table)) {
                auto* ht = as_obj<HashTableObj>(table);
                if (ht->count > 0) --ht->count;
                if (mode == 1 && ht->is_module_obarray) {
                    if (is_symbol(key)) ++as_obj<SymbolObj>(key)->def_epoch;
                    else ++vm().module_epoch;
                }
            }
            return entry;
        }
        prev = cur;
        cur = cdr(cur);
    }
    return SCM_BOOL_F;
}

void VM::init_hashtab() {
    define_proc("hash", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t sz = scm_to_int64(a[1], "hash", 2);
        if (sz <= 0) out_of_range("hash", a[1]);
        return make_uinteger(scm_ihash(a[0], static_cast<uint64_t>(sz)));
    });
    define_proc("hashq", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t sz = scm_to_int64(a[1], "hashq", 2);
        if (sz <= 0) out_of_range("hashq", a[1]);
        return make_uinteger(scm_ihashq(a[0], static_cast<uint64_t>(sz)));
    });
    define_proc("hashv", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t sz = scm_to_int64(a[1], "hashv", 2);
        if (sz <= 0) out_of_range("hashv", a[1]);
        return make_uinteger(scm_ihashv(a[0], static_cast<uint64_t>(sz)));
    });

    auto reg_make_ht = [this](const char* name, WeakKind wk) {
        define_proc(name, 0, 1, false, [this, name, wk](const SCM* a, size_t nargs) -> SCM {
            int64_t n = 31;
            if (nargs >= 1 && !a[0].is_undefined()) {
                n = scm_to_int64(a[0], name, 1);
                if (n < 0) out_of_range(name, a[0]);
            }
            return make_hashtable(static_cast<size_t>(n), wk);
        });
    };
    reg_make_ht("make-hash-table", WeakKind::None);
    reg_make_ht("make-weak-key-hash-table", WeakKind::KeyAlist);
    reg_make_ht("make-weak-value-hash-table", WeakKind::ValueAlist);
    reg_make_ht("make-doubly-weak-hash-table", WeakKind::DoublyWeakAlist);

    define_proc("hash-table?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_hashtable(a[0]));
    });
    define_proc("weak-key-hash-table?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_hashtable(a[0]) && as_obj<HashTableObj>(a[0])->weak == WeakKind::KeyAlist);
    });
    define_proc("weak-value-hash-table?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_hashtable(a[0]) && as_obj<HashTableObj>(a[0])->weak == WeakKind::ValueAlist);
    });
    define_proc("doubly-weak-hash-table?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_hashtable(a[0]) && as_obj<HashTableObj>(a[0])->weak == WeakKind::DoublyWeakAlist);
    });

    auto reg_ht_ops = [this](string pfx, int mode) {
        define_proc(pfx * "-get-handle", 2, 0, false, [mode](const SCM* a, size_t) -> SCM {
            return hash_get_handle(a[0], a[1], mode);
        });
        define_proc(pfx * "-create-handle!", 3, 0, false, [mode](const SCM* a, size_t) -> SCM {
            return hash_create_handle(a[0], a[1], a[2], mode);
        });
        define_proc(pfx * "-ref", 2, 1, false, [mode](const SCM* a, size_t nargs) -> SCM {
            SCM dflt = (nargs >= 3 && !a[2].is_undefined()) ? a[2] : SCM_BOOL_F;
            return hash_ref(a[0], a[1], dflt, mode);
        });
        define_proc(pfx * "-set!", 3, 0, false, [mode](const SCM* a, size_t) -> SCM {
            return hash_set(a[0], a[1], a[2], mode);
        });
        define_proc(pfx * "-remove!", 2, 0, false, [mode](const SCM* a, size_t) -> SCM {
            return hash_remove(a[0], a[1], mode);
        });
    };
    reg_ht_ops("hash",  0);
    reg_ht_ops("hashq", 1);
    reg_ht_ops("hashv", 2);

    // Custom hashx-* operations
    define_proc("hashx-get-handle", 4, 0, false, [this](const SCM* a, size_t) -> SCM {
        // (hashx-get-handle hash assoc table key)
        if (!is_hashtable(a[2])) wrong_type_arg("hashx-get-handle", 3, a[2], "hash table");
        auto* ht = as_obj<HashTableObj>(a[2]);
        SCM idx_scm = call2(a[0], a[3], make_uinteger(static_cast<size_t>(N(ht->buckets))));
        size_t idx = static_cast<size_t>(scm_to_uint64(idx_scm, "hashx-get-handle", 1) % static_cast<size_t>(N(ht->buckets)));
        return call2(a[1], a[3], ht->buckets[idx]);
    });
    define_proc("hashx-create-handle!", 5, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_hashtable(a[2])) wrong_type_arg("hashx-create-handle!", 3, a[2], "hash table");
        auto* ht = as_obj<HashTableObj>(a[2]);
        SCM idx_scm = call2(a[0], a[3], make_uinteger(static_cast<size_t>(N(ht->buckets))));
        size_t idx = static_cast<size_t>(scm_to_uint64(idx_scm, "hashx-create-handle!", 1) % static_cast<size_t>(N(ht->buckets)));
        SCM h = call2(a[1], a[3], ht->buckets[idx]);
        if (h.is_true()) return h;
        SCM entry = cons(a[3], a[4]);
        ht->buckets[idx] = cons(entry, ht->buckets[idx]);
        ++ht->count;
        return entry;
    });
    define_proc("hashx-ref", 4, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM dflt = (nargs >= 5 && !a[4].is_undefined()) ? a[4] : SCM_BOOL_F;
        if (!is_hashtable(a[2])) wrong_type_arg("hashx-ref", 3, a[2], "hash table");
        auto* ht = as_obj<HashTableObj>(a[2]);
        SCM idx_scm = call2(a[0], a[3], make_uinteger(static_cast<size_t>(N(ht->buckets))));
        size_t idx = static_cast<size_t>(scm_to_uint64(idx_scm, "hashx-ref", 1) % static_cast<size_t>(N(ht->buckets)));
        SCM h = call2(a[1], a[3], ht->buckets[idx]);
        return is_pair(h) ? cdr(h) : dflt;
    });
    define_proc("hashx-set!", 5, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_hashtable(a[2])) wrong_type_arg("hashx-set!", 3, a[2], "hash table");
        auto* ht = as_obj<HashTableObj>(a[2]);
        SCM idx_scm = call2(a[0], a[3], make_uinteger(static_cast<size_t>(N(ht->buckets))));
        size_t idx = static_cast<size_t>(scm_to_uint64(idx_scm, "hashx-set!", 1) % static_cast<size_t>(N(ht->buckets)));
        SCM h = call2(a[1], a[3], ht->buckets[idx]);
        if (is_pair(h)) { set_cdr(h, a[4]); return a[4]; }
        if (!h.is_false()) wrong_type_arg("hashx-set!", 2, h, "pair or #f");
        SCM entry = cons(a[3], a[4]);
        ht->buckets[idx] = cons(entry, ht->buckets[idx]);
        ++ht->count;
        if (ht->count > static_cast<size_t>(N(ht->buckets)) * 9 / 10) {
            size_t new_n = next_prime_bucket_size(static_cast<size_t>(N(ht->buckets)) + 1);
            array<SCM> old_buckets;
            old_buckets = ht->buckets;
            gcpp_assign(ht->buckets, new_n, SCM_EOL);
            for (SCM b : old_buckets) {
                for (SCM p = b; is_pair(p); p = cdr(p)) {
                    SCM e = car(p);
                    if (!is_pair(e)) continue;
                    SCM ni = call2(a[0], car(e), make_uinteger(new_n));
                    size_t nidx = static_cast<size_t>(scm_to_uint64(ni, "hashx-set!", 1) % new_n);
                    ht->buckets[nidx] = cons(e, ht->buckets[nidx]);
                }
            }
        }
        return a[4];
    });
    define_proc("hashx-remove!", 4, 0, false, [this](const SCM* a, size_t) -> SCM {
        // (hashx-remove! hash assoc table key)
        if (!is_hashtable(a[2])) wrong_type_arg("hashx-remove!", 3, a[2], "hash table");
        auto* ht = as_obj<HashTableObj>(a[2]);
        SCM idx_scm = call2(a[0], a[3], make_uinteger(static_cast<size_t>(N(ht->buckets))));
        size_t idx = static_cast<size_t>(scm_to_uint64(idx_scm, "hashx-remove!", 1) % static_cast<size_t>(N(ht->buckets)));
        SCM h = call2(a[1], a[3], ht->buckets[idx]);
        if (h.is_false()) return SCM_BOOL_F;
        SCM prev = SCM_EOL, cur = ht->buckets[idx];
        while (is_pair(cur)) {
            if (car(cur) == h) {
                if (prev.is_null()) ht->buckets[idx] = cdr(cur);
                else set_cdr(prev, cdr(cur));
                if (ht->count > 0) --ht->count;
                return h;
            }
            prev = cur;
            cur = cdr(cur);
        }
        return SCM_BOOL_F;
    });

    define_proc("hash-fold", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        // (hash-fold proc init table)
        if (!is_hashtable(a[2])) wrong_type_arg("hash-fold", 3, a[2], "hash table");
        auto* ht = as_obj<HashTableObj>(a[2]);
        SCM acc = a[1];
        // Snapshot entries so mutation during fold doesn't crash
        ScmRootVec entries(heap_);
        for (SCM b : ht->buckets) {
            SCM cur = b;
            while (is_pair(cur)) {
                (entries << car(cur));
                cur = cdr(cur);
            }
        }
        for (size_t i = 0; i < static_cast<size_t>(N(entries)); ++i) {
            SCM e = entries[i];
            if (is_pair(e)) acc = call3(a[0], car(e), cdr(e), acc);
        }
        return acc;
    });

    define_proc("hash-for-each", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_hashtable(a[1])) wrong_type_arg("hash-for-each", 2, a[1], "hash table");
        auto* ht = as_obj<HashTableObj>(a[1]);
        ScmRootVec entries(heap_);
        for (SCM b : ht->buckets) {
            SCM cur = b;
            while (is_pair(cur)) { (entries << car(cur)); cur = cdr(cur); }
        }
        for (size_t i = 0; i < static_cast<size_t>(N(entries)); ++i) {
            SCM e = entries[i];
            if (is_pair(e)) call2(a[0], car(e), cdr(e));
        }
        return SCM_UNSPECIFIED;
    });

    define_proc("hash-for-each-handle", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_hashtable(a[1])) wrong_type_arg("hash-for-each-handle", 2, a[1], "hash table");
        auto* ht = as_obj<HashTableObj>(a[1]);
        ScmRootVec entries(heap_);
        for (SCM b : ht->buckets) {
            SCM cur = b;
            while (is_pair(cur)) { (entries << car(cur)); cur = cdr(cur); }
        }
        for (size_t i = 0; i < static_cast<size_t>(N(entries)); ++i) {
            call1(a[0], entries[i]);
        }
        return SCM_UNSPECIFIED;
    });

    define_proc("hash-map->list", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_hashtable(a[1])) wrong_type_arg("hash-map->list", 2, a[1], "hash table");
        auto* ht = as_obj<HashTableObj>(a[1]);
        ScmRootVec entries(heap_);
        for (SCM b : ht->buckets) {
            SCM cur = b;
            while (is_pair(cur)) { (entries << car(cur)); cur = cdr(cur); }
        }
        SCM res = SCM_EOL;
        for (size_t i = 0; i < static_cast<size_t>(N(entries)); ++i) {
            SCM e = entries[i];
            if (is_pair(e)) res = cons(call2(a[0], car(e), cdr(e)), res);
        }
        return res;
    });

    define_proc("hash-clear!", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_hashtable(a[0])) wrong_type_arg("hash-clear!", 1, a[0], "hash table");
        auto* ht = as_obj<HashTableObj>(a[0]);
        for (SCM& b : ht->buckets) b = SCM_EOL;
        ht->count = 0;
        ++module_epoch;
        return SCM_UNSPECIFIED;
    });

    define_proc("hash-count", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        // (hash-count pred table)
        if (!is_hashtable(a[1])) wrong_type_arg("hash-count", 2, a[1], "hash table");
        auto* ht = as_obj<HashTableObj>(a[1]);
        int64_t cnt = 0;
        ScmRootVec entries(heap_);
        for (SCM b : ht->buckets) {
            SCM cur = b;
            while (is_pair(cur)) { (entries << car(cur)); cur = cdr(cur); }
        }
        for (size_t i = 0; i < static_cast<size_t>(N(entries)); ++i) {
            SCM e = entries[i];
            if (is_pair(e) && call2(a[0], car(e), cdr(e)).is_true()) ++cnt;
        }
        return make_integer(cnt);
    });
}

} // namespace guile
