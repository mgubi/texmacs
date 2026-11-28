#include "guile_cpp/vm.hpp"
#include <algorithm>
#include <random>
#include <unordered_set>

namespace guile {

namespace {

// Exact Guile 1.8.8 class layout and slot indices (libguile/goops.h)
constexpr const char* CLASS_CLASS_LAYOUT =
    "prsrpwpopopwururururururururpwpwpwpwpwpwpwpwpwpwpwpw";

constexpr size_t SI_LAYOUT            = 0;
constexpr size_t SI_VTABLE            = 1;
constexpr size_t SI_PRINT             = 2;
constexpr size_t SI_SETTER            = 4;
constexpr size_t SI_GOOPS_FIELDS      = 5;
constexpr size_t SI_REDEFINED         = 5;
constexpr size_t SI_HASHSETS          = 6;  // 8 slots: 6..13
constexpr size_t SI_NAME              = 14;
constexpr size_t SI_DIRECT_SUPERS     = 15;
constexpr size_t SI_DIRECT_SLOTS      = 16;
constexpr size_t SI_DIRECT_SUBCLASSES = 17;
constexpr size_t SI_DIRECT_METHODS    = 18;
constexpr size_t SI_CPL               = 19;
constexpr size_t SI_SLOTS             = 21;
constexpr size_t SI_GETTERS_N_SETTERS = 22;
constexpr size_t SI_NFIELDS           = 24;
constexpr size_t SI_ENVIRONMENT       = 25;
constexpr size_t N_CLASS_SLOTS        = 26;

// Generic function slots
constexpr size_t SI_GF_METHODS       = 0;
constexpr size_t SI_GF_N_SPECIALIZED = 1;
constexpr size_t SI_GF_USED_BY       = 2;

// Method slots
constexpr size_t SI_M_GENERIC_FUNCTION = 0;
constexpr size_t SI_M_SPECIALIZERS     = 1;
constexpr size_t SI_M_PROCEDURE        = 2;
constexpr size_t SI_M_CODE_TABLE       = 3;

constexpr size_t INITIAL_MCACHE_SIZE = 1;

// PRNG for class hashsets
std::mt19937 g_goops_rng(0x474F4F50u); // "GOOP"

// Hell set for reentrancy guard during class redefinition / change-class (keyed by data_id)
std::vector<uint64_t> g_hell;
uint64_t g_next_data_id = 1;

uint64_t get_data_id(StructObj* st) {
    if (st->data_id == 0) st->data_id = g_next_data_id++;
    return st->data_id;
}

// Port class table: [port_kind][dir] where dir: 0=in, 1=out, 2=inout
SCM g_port_classes[4][3];

inline SCM raw_slot(SCM obj, size_t idx) {
    return SCM(as_obj<StructObj>(obj)->slots[idx]);
}

inline void set_raw_slot(SCM obj, size_t idx, SCM val) {
    auto* st = as_obj<StructObj>(obj);
    if (idx < st->slots.size()) st->slots[idx] = val.bits;
}

inline void set_raw_uword(SCM obj, size_t idx, uint64_t val) {
    auto* st = as_obj<StructObj>(obj);
    if (idx < st->slots.size()) st->slots[idx] = val;
}

inline void set_class_flags(SCM cls, uint32_t f) {
    as_obj<StructObj>(cls)->vtable_flags |= f;
}

inline void clear_class_flags(SCM cls, uint32_t f) {
    as_obj<StructObj>(cls)->vtable_flags &= ~f;
}

inline bool is_subclass(SCM c1, SCM c2) {
    if (!is_struct(c1)) return false;
    auto* st = as_obj<StructObj>(c1);
    if (st->slots.size() <= SI_CPL) return false;
    return scm_memq(c2, SCM(st->slots[SI_CPL])).is_true();
}

inline bool is_a_class(SCM x, SCM cls) {
    return is_goops_instance(x) && is_subclass(struct_vtable(x), cls);
}

inline bool is_generic_obj(SCM x) {
    return is_a_class(x, vm().goops_class_generic);
}

inline bool is_pure_generic(SCM x) {
    return is_struct(x) && ((obj_class_flags(x) & CLASSF_PURE_GENERIC) != 0);
}

inline bool is_method_obj(SCM x) {
    return is_a_class(x, vm().goops_class_method);
}

inline bool is_accessor_method(SCM x) {
    return is_struct(x) && ((obj_class_flags(x) & CLASSF_ACCESSOR_METHOD) != 0);
}

void validate_class(const char* subr, int pos, SCM x) {
    if (!is_goops_class(x)) vm().wrong_type_arg(subr, pos, x, "class");
}

void validate_instance(const char* subr, int pos, SCM x) {
    if (!is_goops_instance(x)) vm().wrong_type_arg(subr, pos, x, "instance");
}

void validate_generic(const char* subr, int pos, SCM x) {
    if (!is_generic_obj(x)) vm().wrong_type_arg(subr, pos, x, "generic function");
}

void validate_method(const char* subr, int pos, SCM x) {
    if (!is_method_obj(x)) vm().wrong_type_arg(subr, pos, x, "method");
}

// Look up a variable in (oop goops) module
SCM goops_getvar(const char* name) {
    auto& v = vm();
    SCM sym = v.make_symbol(name);
    SCM var = v.eval_closure_lookup(v.goops_lookup_closure, sym, false);
    if (is_variable(var) && !as_obj<VariableObj>(var)->value.is_undefined()) {
        return as_obj<VariableObj>(var)->value;
    }
    v.error(v.sym_unbound_variable, nullptr, "Unbound variable: ~S", v.list1(sym));
}

SCM goops_call_gf1(const char* name, SCM a) { return vm().call1(goops_getvar(name), a); }
SCM goops_call_gf2(const char* name, SCM a, SCM b) { return vm().call2(goops_getvar(name), a, b); }
SCM goops_call_gf3(const char* name, SCM a, SCM b, SCM c) { return vm().call3(goops_getvar(name), a, b, c); }
SCM goops_call_gf4(const char* name, SCM a, SCM b, SCM c, SCM d) { return vm().call4(goops_getvar(name), a, b, c, d); }

void goops_defvar(SCM sym, SCM val) {
    auto& v = vm();
    SCM form = v.list3(v.make_symbol("define-public"), sym, v.list2(v.sym_quote, val));
    v.eval(form, v.cons(v.module_lookup_closure(v.goops_module), SCM_EOL));
}

// Forward declarations
SCM goops_slot_ref(SCM obj, SCM slot_name);
SCM goops_slot_set(SCM obj, SCM slot_name, SCM val);
SCM goops_allocate_instance(SCM cls, SCM initargs);
SCM goops_make(SCM args);
SCM goops_basic_make_class(SCM meta, SCM name, SCM dsupers, SCM dslots);
SCM goops_make_extended_class(const char* type_name, bool applicablep);
void goops_change_object_class(SCM obj, SCM old_cls, SCM new_cls);

SCM test_change_class(SCM obj) {
    SCM cls = struct_vtable(obj);
    auto* cso = as_obj<StructObj>(cls);
    if (cso->slots.size() > SI_REDEFINED) {
        SCM redef = SCM(cso->slots[SI_REDEFINED]);
        if (redef.is_true() && !redef.is_unbound()) {
            goops_change_object_class(obj, cls, redef);
            cls = struct_vtable(obj);
        }
    }
    return cls;
}

void goops_change_object_class(SCM obj, SCM /*old_cls*/, SCM new_cls) {
    uint64_t did = get_data_id(as_obj<StructObj>(obj));
    for (uint64_t id : g_hell) {
        if (id == did) return; // already undergoing class change
    }
    g_hell.push_back(did);
    struct Guard {
        uint64_t id;
        ~Guard() {
            auto it = std::find(g_hell.begin(), g_hell.end(), id);
            if (it != g_hell.end()) g_hell.erase(it);
        }
    } guard{did};
    goops_call_gf2("change-class", obj, new_cls);
}

void prep_hashsets(SCM cls) {
    for (size_t i = 0; i < 8; ++i) {
        set_raw_uword(cls, SI_HASHSETS + i, static_cast<uint64_t>(g_goops_rng()));
    }
}

SCM goops_make_method_cache(SCM gf) {
    auto& v = vm();
    SCM dispatch_sym = v.make_symbol("#@dispatch");
    SCM vec = v.make_vector(INITIAL_MCACHE_SIZE, v.goops_list_of_no_method);
    return v.cons(dispatch_sym,
           v.list4(v.make_symbol("args"),
                   SCM::from_fixnum(1),
                   vec,
                   gf));
}

void clear_method_cache(SCM gf) {
    SCM cache = goops_make_method_cache(gf);
    auto* so = as_obj<StructObj>(gf);
    so->entity_proc = cache;
    so->gf_cache_tag = SCM_BOOL_F;
    so->gf_cache_cls0 = SCM_BOOL_F;
    so->gf_cache_cls1 = SCM_UNDEFINED;
    so->gf_cache_clo = SCM_BOOL_F;
    set_raw_slot(gf, SI_GF_USED_BY, SCM_BOOL_F);
}

// ============================================================================
// Keyword lookup (scm_i_get_keyword / scm_get_keyword)
// ============================================================================

SCM i_get_keyword(SCM key, SCM l, int64_t len, SCM default_val, const char* subr) {
    for (int64_t i = 0; i != len; i += 2) {
        SCM obj = car(l);
        if (!is_keyword(obj)) {
            vm().misc_error(subr, "bad keyword: ~S", vm().list1(obj));
        } else if (obj == key) {
            return car(cdr(l));
        } else {
            l = cdr(cdr(l));
        }
    }
    return default_val;
}

SCM goops_get_keyword(SCM key, SCM l, SCM default_val) {
    if (!is_keyword(key)) vm().wrong_type_arg("get-keyword", 1, key, "keyword");
    int64_t len = scm_ilength(l);
    if (len < 0 || (len & 1) != 0) {
        vm().misc_error("get-keyword", "Bad keyword-value list: ~S", vm().list1(l));
    }
    return i_get_keyword(key, l, len, default_val, "get-keyword");
}

// ============================================================================
// Boot-time CPL, slots, getters-n-setters
// ============================================================================

SCM filter_cpl(SCM ls) {
    SCM res = SCM_EOL;
    while (is_pair(ls)) {
        SCM el = car(ls);
        if (scm_memq(el, res).is_false()) res = vm().cons(el, res);
        ls = cdr(ls);
    }
    return res;
}

SCM compute_cpl(SCM cls) {
    auto& v = vm();
    if (v.goops_loaded) {
        return goops_call_gf1("compute-cpl", cls);
    }
    SCM supers = raw_slot(cls, SI_DIRECT_SUPERS);
    // map compute_cpl over supers
    SCM mapped = SCM_EOL, tail = SCM_EOL;
    for (SCM p = supers; is_pair(p); p = cdr(p)) {
        SCM cell = v.cons(compute_cpl(car(p)), SCM_EOL);
        if (mapped.is_null()) mapped = tail = cell;
        else { set_cdr(tail, cell); tail = cell; }
    }
    // append (cons (cons cls supers) mapped)
    SCM all_lists = v.cons(v.cons(cls, supers), mapped);
    SCM flat = SCM_EOL, ftail = SCM_EOL;
    for (SCM lp = all_lists; is_pair(lp); lp = cdr(lp)) {
        for (SCM ep = car(lp); is_pair(ep); ep = cdr(ep)) {
            SCM cell = v.cons(car(ep), SCM_EOL);
            if (flat.is_null()) flat = ftail = cell;
            else { set_cdr(ftail, cell); ftail = cell; }
        }
    }
    return scm_reverse_x(filter_cpl(flat), SCM_EOL);
}

SCM remove_duplicate_slots(SCM l, SCM res, SCM seen) {
    if (l.is_null()) return res;
    SCM tmp = car(car(l));
    if (!is_symbol(tmp)) vm().misc_error("%compute-slots", "bad slot name ~S", vm().list1(tmp));
    if (scm_memq(tmp, seen).is_false()) {
        res = vm().cons(car(l), res);
        seen = vm().cons(tmp, seen);
    }
    return remove_duplicate_slots(cdr(l), res, seen);
}

SCM append2(SCM a, SCM b) {
    if (a.is_null()) return b;
    SCM head = SCM_EOL, tail = SCM_EOL;
    for (SCM p = a; is_pair(p); p = cdr(p)) {
        SCM cell = vm().cons(car(p), SCM_EOL);
        if (head.is_null()) head = tail = cell;
        else { set_cdr(tail, cell); tail = cell; }
    }
    set_cdr(tail, b);
    return head;
}

SCM build_slots_list(SCM dslots, SCM cpl) {
    SCM res = dslots;
    for (SCM p = cdr(cpl); is_pair(p); p = cdr(p)) {
        res = append2(raw_slot(car(p), SI_DIRECT_SLOTS), res);
    }
    return remove_duplicate_slots(scm_reverse(res), SCM_EOL, SCM_EOL);
}

SCM maplist_slots(SCM ls) {
    SCM orig = ls;
    while (is_pair(ls)) {
        if (!is_pair(car(ls))) {
            set_car(ls, vm().cons(car(ls), SCM_EOL));
        }
        ls = cdr(ls);
    }
    return orig;
}

SCM compute_getters_n_setters(SCM slots) {
    auto& v = vm();
    SCM kw_init_value = v.make_keyword("init-value");
    SCM kw_init_thunk = v.make_keyword("init-thunk");
    SCM res = SCM_EOL, tail = SCM_EOL;
    int64_t i = 0;
    for (; is_pair(slots); slots = cdr(slots)) {
        SCM init = SCM_BOOL_F;
        SCM options = cdr(car(slots));
        if (is_pair(options)) {
            int64_t olen = scm_ilength(options);
            SCM iv = i_get_keyword(kw_init_value, options, olen, SCM_UNDEFINED, "compute-getters-n-setters");
            if (!iv.is_undefined()) {
                // Build a zero-arg closure returning iv
                init = v.make_closure(SCM_EOL, v.list1(v.list2(v.sym_quote, iv)), SCM_EOL);
            } else {
                init = i_get_keyword(kw_init_thunk, options, olen, SCM_BOOL_F, "compute-getters-n-setters");
            }
        }
        SCM entry = v.cons(car(car(slots)), v.cons(init, SCM::from_fixnum(i++)));
        SCM cell = v.cons(entry, SCM_EOL);
        if (res.is_null()) res = tail = cell;
        else { set_cdr(tail, cell); tail = cell; }
    }
    return res;
}

// ============================================================================
// Slot access helpers
// ============================================================================

SCM slot_definition_using_name(SCM cls, SCM slot_name) {
    SCM slots = raw_slot(cls, SI_GETTERS_N_SETTERS);
    for (; is_pair(slots); slots = cdr(slots)) {
        if (car(car(slots)) == slot_name) return car(slots);
    }
    return SCM_BOOL_F;
}

SCM get_slot_value(SCM /*cls*/, SCM obj, SCM slotdef) {
    SCM access = cdr(cdr(slotdef));
    if (access.is_fixnum()) {
        return struct_ref(obj, static_cast<size_t>(access.to_fixnum()));
    }
    SCM getter = car(access);
    return vm().call1(getter, obj);
}

SCM get_slot_value_using_name(SCM cls, SCM obj, SCM slot_name) {
    SCM slotdef = slot_definition_using_name(cls, slot_name);
    if (slotdef.is_true()) return get_slot_value(cls, obj, slotdef);
    return goops_call_gf3("slot-missing", cls, obj, slot_name);
}

SCM set_slot_value(SCM /*cls*/, SCM obj, SCM slotdef, SCM value) {
    SCM access = cdr(cdr(slotdef));
    if (access.is_fixnum()) {
        struct_set(obj, static_cast<size_t>(access.to_fixnum()), value);
    } else {
        SCM setter = car(cdr(access));
        vm().call2(setter, obj, value);
    }
    return SCM_UNSPECIFIED;
}

SCM set_slot_value_using_name(SCM cls, SCM obj, SCM slot_name, SCM value) {
    SCM slotdef = slot_definition_using_name(cls, slot_name);
    if (slotdef.is_true()) return set_slot_value(cls, obj, slotdef, value);
    return goops_call_gf4("slot-missing", cls, obj, slot_name, value);
}

SCM test_slot_existence(SCM /*cls*/, SCM obj, SCM slot_name) {
    SCM cls = struct_vtable(obj);
    for (SCM l = raw_slot(cls, SI_GETTERS_N_SETTERS); is_pair(l); l = cdr(l)) {
        if (car(car(l)) == slot_name) return SCM_BOOL_T;
    }
    return SCM_BOOL_F;
}

SCM goops_slot_ref(SCM obj, SCM slot_name) {
    validate_instance("slot-ref", 1, obj);
    SCM cls = test_change_class(obj);
    SCM res = get_slot_value_using_name(cls, obj, slot_name);
    if (res.is_unbound()) return goops_call_gf3("slot-unbound", cls, obj, slot_name);
    return res;
}

SCM goops_slot_set(SCM obj, SCM slot_name, SCM value) {
    validate_instance("slot-set!", 1, obj);
    SCM cls = test_change_class(obj);
    return set_slot_value_using_name(cls, obj, slot_name, value);
}

// ============================================================================
// %prep-layout! & %inherit-magic!
// ============================================================================

bool gns_instance_allocated_p(SCM gns) {
    SCM cddr_g = cdr(cdr(gns));
    if (cddr_g.is_fixnum()) return true;
    return is_pair(cddr_g) && is_pair(cdr(cddr_g)) && is_pair(cdr(cdr(cddr_g)));
}

int64_t gns_index(SCM gns) {
    SCM cddr_g = cdr(cdr(gns));
    if (cddr_g.is_fixnum()) return cddr_g.to_fixnum();
    return scm_to_int64(car(cdr(cdr(cddr_g))), "%prep-layout!", 1);
}

int64_t gns_size(SCM gns) {
    SCM cddr_g = cdr(cdr(gns));
    if (cddr_g.is_fixnum()) return 1;
    return scm_to_int64(car(cdr(cdr(cdr(cddr_g)))), "%prep-layout!", 1);
}

SCM goops_prep_layout(SCM cls) {
    auto& v = vm();
    validate_instance("%prep-layout!", 1, cls);
    SCM slots = raw_slot(cls, SI_SLOTS);
    SCM gns = raw_slot(cls, SI_GETTERS_N_SETTERS);
    SCM nfields_scm = raw_slot(cls, SI_NFIELDS);
    if (!nfields_scm.is_fixnum() || nfields_scm.to_fixnum() < 0) {
        v.misc_error("%prep-layout!", "bad value in nfields slot: ~S", v.list1(nfields_scm));
    }
    size_t n = static_cast<size_t>(2 * nfields_scm.to_fixnum());
    if (n < std::strlen(CLASS_CLASS_LAYOUT) && is_subclass(cls, v.goops_class_class)) {
        v.misc_error("%prep-layout!", "class object doesn't have enough fields: ~S", v.list1(nfields_scm));
    }

    SCM kw_class = v.make_keyword("class");
    std::string layout(n, '\0');
    size_t i = 0;
    while (is_pair(gns)) {
        if (gns_instance_allocated_p(car(gns))) {
            if (i >= n || !is_pair(slots)) {
                v.misc_error("%prep-layout!", "inconsistent getters-n-setters");
            }
            int64_t len = scm_ilength(cdr(car(slots)));
            SCM type = i_get_keyword(kw_class, cdr(car(slots)), len, SCM_BOOL_F, "%prep-layout!");
            char p = 'p', a = 'w';
            if (type.is_true()) {
                if (!is_goops_class(type)) {
                    v.misc_error("%prep-layout!", "bad slot class");
                } else if (is_subclass(type, v.goops_class_foreign_slot)) {
                    if (is_subclass(type, v.goops_class_self)) p = 's';
                    else if (is_subclass(type, v.goops_class_protected)) p = 'p';
                    else p = 'u';

                    if (is_subclass(type, v.goops_class_opaque)) a = 'o';
                    else if (is_subclass(type, v.goops_class_read_only)) a = 'r';
                    else a = 'w';
                }
            }
            int64_t idx = gns_index(car(gns));
            if (idx != static_cast<int64_t>(i >> 1)) {
                v.misc_error("%prep-layout!", "inconsistent getters-n-setters");
            }
            int64_t sz = gns_size(car(gns));
            while (sz > 0) {
                if (i + 1 >= n) v.misc_error("%prep-layout!", "inconsistent getters-n-setters");
                layout[i++] = p;
                layout[i++] = a;
                --sz;
            }
        }
        slots = cdr(slots);
        gns = cdr(gns);
    }
    if (!slots.is_null()) {
        v.misc_error("%prep-layout!", "inconsistent getters-n-setters");
    }
    set_raw_slot(cls, SI_LAYOUT, v.make_symbol(layout));
    return SCM_UNSPECIFIED;
}

SCM goops_inherit_magic(SCM cls, SCM dsupers) {
    validate_instance("%inherit-magic!", 1, cls);
    uint32_t flags = 0;
    for (SCM ls = dsupers; !ls.is_null(); ls = cdr(ls)) {
        if (!is_pair(ls) || !is_goops_instance(car(ls))) {
            vm().wrong_type_arg("%inherit-magic!", 2, dsupers, "list of classes");
        }
        flags |= struct_class_flags(car(ls));
    }
    flags &= CLASSF_INHERIT;
    set_class_flags(cls, flags);
    prep_hashsets(cls);
    return SCM_UNSPECIFIED;
}

// ============================================================================
// %allocate-instance
// ============================================================================

SCM wrap_init_instance(SCM cls, size_t n, bool is_ent) {
    auto* st = vm().heap().alloc<StructObj>();
    SCM handle = SCM::from_obj(st);
    st->vtable = cls;
    st->is_entity = is_ent;
    st->slots.assign(n, 0);
    SCM slayout = raw_slot(cls, SI_LAYOUT);
    const std::string& desc = as_obj<SymbolObj>(slayout)->name;
    for (size_t i = 0; i < n; ++i) {
        if (i * 2 < desc.size() && desc[i * 2] == 'p') {
            st->slots[i] = SCM_UNBOUND.bits;
        } else if (i * 2 < desc.size() && desc[i * 2] == 's') {
            st->slots[i] = handle.bits;
        } else {
            st->slots[i] = 0;
        }
    }
    return handle;
}

SCM goops_allocate_instance(SCM cls, SCM /*initargs*/) {
    auto& v = vm();
    validate_class("%allocate-instance", 1, cls);
    uint32_t cf = struct_class_flags(cls);

    if (cf & CLASSF_FOREIGN) {
        v.misc_error("%allocate-instance", "Can't make instances of class ~S", v.list1(cls));
    }

    size_t n = static_cast<size_t>(raw_slot(cls, SI_NFIELDS).to_fixnum());

    // Entities (including generic functions / accessors)
    if (cf & CLASSF_ENTITY) {
        SCM obj = wrap_init_instance(cls, n, true);
        as_obj<StructObj>(obj)->entity_proc = SCM_BOOL_F;
        as_obj<StructObj>(obj)->entity_setter = SCM_BOOL_F;
        if (cf & CLASSF_PURE_GENERIC) {
            clear_method_cache(obj);
        }
        return obj;
    }

    // Class objects (instances of a metaclass)
    if (cf & CLASSF_METACLASS) {
        SCM z = make_struct_impl(cls, 0, SCM_EOL);
        set_raw_slot(z, SI_PRINT, SCM_UNBOUND);
        for (size_t i = SI_GOOPS_FIELDS; i < n; ++i) {
            set_raw_slot(z, i, SCM_UNBOUND);
        }
        // Mark newly allocated class as valid GOOPS class (redefined will be initialized later)
        set_class_flags(z, CLASSF_GOOPS_OR_VALID);
        if (is_subclass(cls, v.goops_class_entity_class)) {
            set_class_flags(z, CLASSF_OPERATOR | CLASSF_ENTITY);
        } else if (is_subclass(cls, v.goops_class_operator_class)) {
            set_class_flags(z, CLASSF_OPERATOR);
        }
        return z;
    }

    // Regular instances (including operator instances)
    SCM obj = wrap_init_instance(cls, n, false);
    if (cf & CLASSF_OPERATOR) {
        as_obj<StructObj>(obj)->is_operator = true;
    }
    return obj;
}

// ============================================================================
// Class construction helpers
// ============================================================================

SCM goops_basic_basic_make_class(SCM meta, SCM name, SCM dsupers, SCM dslots) {
    auto& v = vm();
    SCM z = make_struct_impl(meta, 0, SCM_EOL);

    set_raw_slot(z, SI_DIRECT_SUPERS, dsupers);
    SCM cpl = compute_cpl(z);
    SCM slots = build_slots_list(maplist_slots(dslots), cpl);
    SCM nfields = SCM::from_fixnum(scm_ilength(slots));
    SCM gns = compute_getters_n_setters(slots);

    set_raw_slot(z, SI_NAME, name);
    set_raw_slot(z, SI_DIRECT_SLOTS, dslots);
    set_raw_slot(z, SI_DIRECT_SUBCLASSES, SCM_EOL);
    set_raw_slot(z, SI_DIRECT_METHODS, SCM_EOL);
    set_raw_slot(z, SI_CPL, cpl);
    set_raw_slot(z, SI_SLOTS, slots);
    set_raw_slot(z, SI_NFIELDS, nfields);
    set_raw_slot(z, SI_GETTERS_N_SETTERS, gns);
    set_raw_slot(z, SI_REDEFINED, SCM_BOOL_F);
    set_raw_slot(z, SI_ENVIRONMENT, v.cons(v.current_module_lookup_closure(), SCM_EOL));

    for (SCM tmp = dsupers; is_pair(tmp); tmp = cdr(tmp)) {
        SCM sup = car(tmp);
        set_raw_slot(sup, SI_DIRECT_SUBCLASSES, v.cons(z, raw_slot(sup, SI_DIRECT_SUBCLASSES)));
    }

    uint32_t f = CLASSF_GOOPS_OR_VALID;
    if (meta == v.goops_class_entity_class) f |= CLASSF_OPERATOR | CLASSF_ENTITY;
    else if (meta == v.goops_class_operator_class) f |= CLASSF_OPERATOR;
    set_class_flags(z, f);
    return z;
}

SCM goops_basic_make_class(SCM meta, SCM name, SCM dsupers, SCM dslots) {
    SCM z = goops_basic_basic_make_class(meta, name, dsupers, dslots);
    goops_inherit_magic(z, dsupers);
    goops_prep_layout(z);
    return z;
}

SCM make_class_from_template(const char* tmpl, const char* type_name, SCM supers, bool applicablep) {
    auto& v = vm();
    SCM name = SCM_UNBOUND;
    if (type_name) {
        char buf[256];
        std::snprintf(buf, sizeof(buf), tmpl, type_name);
        name = v.make_symbol(buf);
    }
    SCM cls = v.permanent_object(goops_basic_make_class(
        applicablep ? v.goops_class_procedure_class : v.goops_class_class,
        name, supers, SCM_EOL));
    if (!name.is_unbound() && v.eval_closure_lookup(v.goops_lookup_closure, name, false).is_false()) {
        goops_defvar(name, cls);
    }
    return cls;
}

SCM goops_make_extended_class(const char* type_name, bool applicablep) {
    auto& v = vm();
    return make_class_from_template(
        "<%s>", type_name,
        v.list1(applicablep ? v.goops_class_applicable : v.goops_class_top),
        applicablep);
}

// ============================================================================
// Method dispatch, sorting, applicability, method cache
// ============================================================================

SCM fold_downward_gf_methods(SCM method_lists, SCM gf) {
    auto& v = vm();
    SCM gfs = goops_slot_ref(gf, v.make_symbol("extended-by"));
    method_lists = v.cons(goops_slot_ref(gf, v.make_symbol("methods")), method_lists);
    while (is_pair(gfs)) {
        method_lists = fold_downward_gf_methods(method_lists, car(gfs));
        gfs = cdr(gfs);
    }
    return method_lists;
}

SCM fold_upward_gf_methods(SCM method_lists, SCM gf) {
    auto& v = vm();
    if (is_a_class(gf, v.goops_class_extended_generic)) {
        SCM gfs = goops_slot_ref(gf, v.make_symbol("extends"));
        while (is_pair(gfs)) {
            SCM methods = goops_slot_ref(car(gfs), v.make_symbol("methods"));
            method_lists = fold_upward_gf_methods(v.cons(methods, method_lists), car(gfs));
            gfs = cdr(gfs);
        }
    }
    return method_lists;
}

SCM goops_gf_methods(SCM gf) {
    validate_generic("generic-function-methods", 1, gf);
    SCM lists = fold_upward_gf_methods(SCM_EOL, gf);
    lists = fold_downward_gf_methods(lists, gf);
    // Append all lists in `lists`
    SCM res = SCM_EOL, tail = SCM_EOL;
    for (SCM lp = lists; is_pair(lp); lp = cdr(lp)) {
        for (SCM ep = car(lp); is_pair(ep); ep = cdr(ep)) {
            SCM cell = vm().cons(car(ep), SCM_EOL);
            if (res.is_null()) res = tail = cell;
            else { set_cdr(tail, cell); tail = cell; }
        }
    }
    return res;
}

bool applicable_p(SCM actual, SCM formal) {
    return scm_memq(formal, raw_slot(actual, SI_CPL)).is_true();
}

bool more_specific_p(SCM m1, SCM m2, const SCM* targs) {
    SCM s1 = raw_slot(m1, SI_M_SPECIALIZERS);
    SCM s2 = raw_slot(m2, SI_M_SPECIALIZERS);
    for (size_t i = 0; ; ++i) {
        if (s1.is_null()) return true;
        if (s2.is_null()) return false;
        if (!is_pair(s1)) return false; // dotted rest arg is less specific
        if (!is_pair(s2)) return true;
        if (car(s1) != car(s2)) {
            SCM cs1 = car(s1), cs2 = car(s2);
            for (SCM l = raw_slot(targs[i], SI_CPL); is_pair(l); l = cdr(l)) {
                if (cs1 == car(l)) return true;
                if (cs2 == car(l)) return false;
            }
            return false;
        }
        s1 = cdr(s1);
        s2 = cdr(s2);
    }
}

SCM compute_applicable_methods_impl(SCM gf, SCM args, int64_t len, bool find_method_p) {
    std::vector<SCM> types;
    types.reserve(static_cast<size_t>(len));
    for (SCM p = args; is_pair(p); p = cdr(p)) {
        types.push_back(goops_class_of(car(p)));
    }

    std::vector<SCM> applicable;
    for (SCM l = goops_gf_methods(gf); is_pair(l); l = cdr(l)) {
        SCM m = car(l);
        SCM fl = raw_slot(m, SI_M_SPECIALIZERS);
        // Only accept accessors which match exactly in first arg
        if (is_accessor_method(m) && (!is_pair(fl) || types.empty() || types[0] != car(fl))) {
            continue;
        }
        for (int64_t i = 0; ; ++i, fl = cdr(fl)) {
            if (is_goops_instance(fl) || (i >= len && fl.is_null())) {
                applicable.push_back(m);
                break;
            }
            if (i >= len || fl.is_null() || !is_pair(fl) || !applicable_p(types[static_cast<size_t>(i)], car(fl))) {
                break;
            }
        }
    }

    if (applicable.empty()) {
        if (find_method_p) return SCM_BOOL_F;
        goops_call_gf2("no-applicable-method", gf, args);
        return SCM_BOOL_F;
    }

    if (applicable.size() > 1) {
        // Shell sort matching goops.c
        size_t sz = applicable.size();
        for (size_t incr = sz / 2; incr > 0; incr /= 2) {
            for (size_t i = incr; i < sz; ++i) {
                for (int64_t j = static_cast<int64_t>(i - incr); j >= 0; j -= static_cast<int64_t>(incr)) {
                    if (more_specific_p(applicable[static_cast<size_t>(j)],
                                        applicable[static_cast<size_t>(j) + incr],
                                        types.data())) {
                        break;
                    }
                    std::swap(applicable[static_cast<size_t>(j)], applicable[static_cast<size_t>(j) + incr]);
                }
            }
        }
    }

    SCM res = SCM_EOL;
    for (size_t i = applicable.size(); i > 0; --i) {
        res = vm().cons(applicable[i - 1], res);
    }
    return res;
}

// Method cache lookup matching libguile/objects.c:scm_mcache_lookup_cmethod
SCM mcache_lookup_cmethod(SCM cache, SCM args) {
    if (!is_pair(cache) || !is_pair(cdr(cache)) || !is_pair(cdr(cdr(cache)))) return SCM_BOOL_F;
    SCM z = cdr(cdr(cache)); // (N-SPEC METHODS_OR_HASHSET ...)
    int64_t n = scm_to_int64(car(z), "apply-generic", 1);
    SCM methods = car(cdr(z));

    uint64_t mask = 0;
    uint64_t i = 0;
    uint64_t end = 0;
    if (is_vector(methods)) {
        // Format #1: linear search
        mask = ~uint64_t{0};
        i = 0;
        end = as_obj<VectorObj>(methods)->elts.size();
        if (end == 0) return SCM_BOOL_F;
    } else {
        // Format #2: hashed search
        uint64_t hashset = scm_to_uint64(methods, "apply-generic", 1);
        z = cdr(cdr(z));
        mask = scm_to_uint64(car(z), "apply-generic", 1);
        methods = car(cdr(z));
        if (!is_vector(methods) || as_obj<VectorObj>(methods)->elts.empty()) return SCM_BOOL_F;
        int64_t j = n;
        SCM ls = args;
        i = 0;
        if (is_pair(ls)) {
            do {
                SCM cls = goops_class_of(car(ls));
                i += as_obj<StructObj>(cls)->slots[SI_HASHSETS + hashset];
                ls = cdr(ls);
            } while (j-- && is_pair(ls));
        }
        i &= mask;
        end = i;
    }

    auto* vec = as_obj<VectorObj>(methods);
    do {
        int64_t j = n;
        SCM entry = vec->elts[static_cast<size_t>(i)];
        SCM ls = args;
        bool matched = true;
        if (is_pair(ls)) {
            do {
                if (!is_pair(entry) || goops_class_of(car(ls)) != car(entry)) {
                    matched = false;
                    break;
                }
                ls = cdr(ls);
                entry = cdr(entry);
            } while (j-- && is_pair(ls));
        }
        if (matched && is_pair(entry) && (car(entry).is_null() || is_pair(car(entry)))) {
            return entry;
        }
        i = (i + 1) & mask;
    } while (i != end);
    return SCM_BOOL_F;
}

SCM mcache_compute_cmethod(SCM gf, SCM cache, SCM args) {
    SCM cm = mcache_lookup_cmethod(cache, args);
    if (cm.is_true()) return cm;
    return goops_call_gf3("memoize-method!", gf, args, cache);
}

// ============================================================================
// Bootstrap make (replaced later by GOOPS Scheme generic make)
// ============================================================================

SCM goops_make(SCM args) {
    auto& v = vm();
    int64_t len = scm_ilength(args);
    if (len <= 0 || (len & 1) == 0) v.wrong_num_args("make");

    SCM cls = car(args);
    SCM rest = cdr(args);
    SCM kw_name = v.make_keyword("name");
    SCM kw_setter = v.make_keyword("setter");
    SCM kw_gf = v.make_keyword("generic-function");
    SCM kw_spec = v.make_keyword("specializers");
    SCM kw_proc = v.make_keyword("procedure");
    SCM kw_dsupers = v.make_keyword("dsupers");
    SCM kw_slots = v.make_keyword("slots");

    if (cls == v.goops_class_generic || cls == v.goops_class_accessor) {
        SCM mutex_proc = v.lookup("make-mutex");
        SCM mutex = is_variable(mutex_proc) ? v.call0(as_obj<VariableObj>(mutex_proc)->value) : SCM_BOOL_F;
        SCM inits = v.cons(SCM_EOL, v.list4(SCM::from_fixnum(0), SCM_BOOL_F, mutex, SCM_EOL));
        SCM z = make_struct_impl(cls, 0, inits);
        as_obj<StructObj>(z)->is_entity = true;
        SCM nm = i_get_keyword(kw_name, rest, len - 1, SCM_BOOL_F, "make");
        v.set_procedure_property(z, v.sym_name, nm);
        clear_method_cache(z);
        if (cls == v.goops_class_accessor) {
            SCM setter = i_get_keyword(kw_setter, rest, len - 1, SCM_BOOL_F, "make");
            if (setter.is_true()) as_obj<StructObj>(z)->entity_setter = setter;
        }
        return z;
    }

    SCM z = goops_allocate_instance(cls, rest);
    if (cls == v.goops_class_method ||
        cls == v.goops_class_simple_method ||
        cls == v.goops_class_accessor_method) {
        set_raw_slot(z, SI_M_GENERIC_FUNCTION, i_get_keyword(kw_gf, rest, len - 1, SCM_BOOL_F, "make"));
        set_raw_slot(z, SI_M_SPECIALIZERS, i_get_keyword(kw_spec, rest, len - 1, SCM_EOL, "make"));
        set_raw_slot(z, SI_M_PROCEDURE, i_get_keyword(kw_proc, rest, len - 1, SCM_EOL, "make"));
        set_raw_slot(z, SI_M_CODE_TABLE, SCM_EOL);
    } else {
        set_raw_slot(z, SI_NAME, i_get_keyword(kw_name, rest, len - 1, v.make_symbol("???"), "make"));
        set_raw_slot(z, SI_DIRECT_SUPERS, i_get_keyword(kw_dsupers, rest, len - 1, SCM_EOL, "make"));
        set_raw_slot(z, SI_DIRECT_SLOTS, i_get_keyword(kw_slots, rest, len - 1, SCM_EOL, "make"));
    }
    return z;
}

// ============================================================================
// Build basic, standard, port, and struct classes
// ============================================================================

SCM build_class_class_slots() {
    auto& v = vm();
    SCM kw_class = v.make_keyword("class");
    auto s1 = [&](const char* nm) { return v.list1(v.make_symbol(nm)); };
    auto s3 = [&](const char* nm, SCM c) { return v.list3(v.make_symbol(nm), kw_class, c); };
    std::vector<SCM> items = {
        s3("layout", v.goops_class_protected_read_only),
        s3("vtable", v.goops_class_self),
        s1("print"),
        s3("procedure", v.goops_class_protected_opaque),
        s3("setter", v.goops_class_protected_opaque),
        s1("redefined"),
        s3("h0", v.goops_class_int),
        s3("h1", v.goops_class_int),
        s3("h2", v.goops_class_int),
        s3("h3", v.goops_class_int),
        s3("h4", v.goops_class_int),
        s3("h5", v.goops_class_int),
        s3("h6", v.goops_class_int),
        s3("h7", v.goops_class_int),
        s1("name"),
        s1("direct-supers"),
        s1("direct-slots"),
        s1("direct-subclasses"),
        s1("direct-methods"),
        s1("cpl"),
        s1("default-slot-definition-class"),
        s1("slots"),
        s1("getters-n-setters"),
        s1("keyword-access"),
        s1("nfields"),
        s1("environment"),
    };
    return v.list_n(items.data(), items.size());
}

void fix_cpl(SCM c, SCM before, SCM after) {
    auto& v = vm();
    SCM cpl = raw_slot(c, SI_CPL);
    SCM ls = scm_memq(after, cpl);
    if (ls.is_false()) return;
    // delq1! before from (cdr ls)
    SCM prev = SCM_BOOL_F, cur = cdr(ls), tail = cdr(ls);
    while (is_pair(cur)) {
        if (car(cur) == before) {
            if (prev.is_false()) tail = cdr(cur);
            else set_cdr(prev, cdr(cur));
            break;
        }
        prev = cur;
        cur = cdr(cur);
    }
    set_car(ls, before);
    set_cdr(ls, v.cons(after, tail));
    SCM dslots = raw_slot(c, SI_DIRECT_SLOTS);
    SCM slots = build_slots_list(maplist_slots(dslots), cpl);
    SCM gns = compute_getters_n_setters(slots);
    set_raw_slot(c, SI_SLOTS, slots);
    set_raw_slot(c, SI_GETTERS_N_SETTERS, gns);
}

void make_stdcls(SCM& var, const char* name, SCM meta, SCM super, SCM slots) {
    auto& v = vm();
    SCM sym = v.make_symbol(name);
    SCM sups = is_pair(super) ? super : v.list1(super);
    var = v.permanent_object(goops_basic_make_class(meta, sym, sups, slots));
    goops_defvar(sym, var);
}

void create_basic_classes() {
    auto& v = vm();
    SCM extra = v.make_string(CLASS_CLASS_LAYOUT + 2 * VTABLE_OFFSET_USER);
    SCM name = v.make_symbol("<class>");
    v.goops_class_class = v.permanent_object(make_vtable_vtable_impl(extra, 0, SCM_EOL));
    set_class_flags(v.goops_class_class, CLASSF_GOOPS_OR_VALID | CLASSF_METACLASS);

    set_raw_slot(v.goops_class_class, SI_NAME, name);
    set_raw_slot(v.goops_class_class, SI_DIRECT_SUPERS, SCM_EOL);
    set_raw_slot(v.goops_class_class, SI_DIRECT_SUBCLASSES, SCM_EOL);
    set_raw_slot(v.goops_class_class, SI_DIRECT_METHODS, SCM_EOL);
    set_raw_slot(v.goops_class_class, SI_CPL, SCM_EOL);
    set_raw_slot(v.goops_class_class, SI_NFIELDS, SCM::from_fixnum(N_CLASS_SLOTS));
    set_raw_slot(v.goops_class_class, SI_REDEFINED, SCM_BOOL_F);
    set_raw_slot(v.goops_class_class, SI_ENVIRONMENT, v.cons(v.current_module_lookup_closure(), SCM_EOL));
    prep_hashsets(v.goops_class_class);
    goops_defvar(name, v.goops_class_class);

    // <top>
    SCM top_name = v.make_symbol("<top>");
    v.goops_class_top = v.permanent_object(goops_basic_make_class(v.goops_class_class, top_name, SCM_EOL, SCM_EOL));
    goops_defvar(top_name, v.goops_class_top);

    // <object>
    SCM obj_name = v.make_symbol("<object>");
    v.goops_class_object = v.permanent_object(
        goops_basic_make_class(v.goops_class_class, obj_name, v.list1(v.goops_class_top), SCM_EOL));
    goops_defvar(obj_name, v.goops_class_object);

    // Patch <object> and <class> inheritance
    set_raw_slot(v.goops_class_object, SI_DIRECT_SUBCLASSES, v.list1(v.goops_class_class));
    set_raw_slot(v.goops_class_class, SI_DIRECT_SUPERS, v.list1(v.goops_class_object));
    set_raw_slot(v.goops_class_class, SI_CPL,
                 v.list3(v.goops_class_class, v.goops_class_object, v.goops_class_top));
}

void create_standard_classes() {
    auto& v = vm();
    SCM kw_init_value = v.make_keyword("init-value");
    SCM kw_init_thunk = v.make_keyword("init-thunk");
    SCM kw_init_keyword = v.make_keyword("init-keyword");
    SCM kw_slot_def = v.make_keyword("slot-definition");
    SCM kw_class = v.make_keyword("class");

    SCM method_slots = v.list4(
        v.make_symbol("generic-function"),
        v.make_symbol("specializers"),
        v.make_symbol("procedure"),
        v.make_symbol("code-table"));
    SCM amethod_slots = v.list1(
        v.list3(v.make_symbol("slot-definition"), kw_init_keyword, kw_slot_def));
    SCM mutex_closure = v.make_closure(
        SCM_EOL,
        v.list1(v.list1(v.make_symbol("make-mutex"))),
        v.cons(v.current_module_lookup_closure(), SCM_EOL));
    SCM gf_slots = v.cons(
        v.make_symbol("methods"),
        v.list4(
            v.list3(v.make_symbol("n-specialized"), kw_init_value, SCM::from_fixnum(0)),
            v.list3(v.make_symbol("used-by"), kw_init_value, SCM_BOOL_F),
            v.list3(v.make_symbol("cache-mutex"), kw_init_thunk, mutex_closure),
            v.list3(v.make_symbol("extended-by"), kw_init_value, SCM_EOL)));
    SCM egf_slots = v.list1(
        v.list3(v.make_symbol("extends"), kw_init_value, SCM_EOL));

    // Foreign slot classes
    make_stdcls(v.goops_class_foreign_slot, "<foreign-slot>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_protected, "<protected-slot>", v.goops_class_class, v.goops_class_foreign_slot, SCM_EOL);
    make_stdcls(v.goops_class_opaque, "<opaque-slot>", v.goops_class_class, v.goops_class_foreign_slot, SCM_EOL);
    make_stdcls(v.goops_class_read_only, "<read-only-slot>", v.goops_class_class, v.goops_class_foreign_slot, SCM_EOL);
    make_stdcls(v.goops_class_self, "<self-slot>", v.goops_class_class, v.goops_class_read_only, SCM_EOL);
    make_stdcls(v.goops_class_protected_opaque, "<protected-opaque-slot>", v.goops_class_class,
                v.list2(v.goops_class_protected, v.goops_class_opaque), SCM_EOL);
    make_stdcls(v.goops_class_protected_read_only, "<protected-read-only-slot>", v.goops_class_class,
                v.list2(v.goops_class_protected, v.goops_class_read_only), SCM_EOL);
    make_stdcls(v.goops_class_scm, "<scm-slot>", v.goops_class_class, v.goops_class_protected, SCM_EOL);
    make_stdcls(v.goops_class_int, "<int-slot>", v.goops_class_class, v.goops_class_foreign_slot, SCM_EOL);
    make_stdcls(v.goops_class_float, "<float-slot>", v.goops_class_class, v.goops_class_foreign_slot, SCM_EOL);
    make_stdcls(v.goops_class_double, "<double-slot>", v.goops_class_class, v.goops_class_foreign_slot, SCM_EOL);

    // Finish <class> slots now that foreign slot classes exist
    SCM class_slots = build_class_class_slots();
    set_raw_slot(v.goops_class_class, SI_DIRECT_SLOTS, class_slots);
    set_raw_slot(v.goops_class_class, SI_SLOTS, class_slots);
    set_raw_slot(v.goops_class_class, SI_GETTERS_N_SETTERS, compute_getters_n_setters(class_slots));

    make_stdcls(v.goops_class_foreign_class, "<foreign-class>", v.goops_class_class, v.goops_class_class,
                v.list2(v.list3(v.make_symbol("constructor"), kw_class, v.goops_class_opaque),
                        v.list3(v.make_symbol("destructor"), kw_class, v.goops_class_opaque)));
    make_stdcls(v.goops_class_foreign_object, "<foreign-object>", v.goops_class_foreign_class, v.goops_class_object, SCM_EOL);
    set_class_flags(v.goops_class_foreign_object, CLASSF_FOREIGN);

    // Generic / method / procedure metaclasses & classes
    make_stdcls(v.goops_class_procedure_class, "<procedure-class>", v.goops_class_class, v.goops_class_class, SCM_EOL);
    make_stdcls(v.goops_class_entity_class, "<entity-class>", v.goops_class_class, v.goops_class_procedure_class, SCM_EOL);
    make_stdcls(v.goops_class_operator_class, "<operator-class>", v.goops_class_class, v.goops_class_procedure_class, SCM_EOL);
    make_stdcls(v.goops_class_operator_with_setter_class, "<operator-with-setter-class>",
                v.goops_class_class, v.goops_class_operator_class, SCM_EOL);
    make_stdcls(v.goops_class_method, "<method>", v.goops_class_class, v.goops_class_object, method_slots);
    make_stdcls(v.goops_class_simple_method, "<simple-method>", v.goops_class_class, v.goops_class_method, SCM_EOL);
    set_class_flags(v.goops_class_simple_method, CLASSF_SIMPLE_METHOD);
    make_stdcls(v.goops_class_accessor_method, "<accessor-method>", v.goops_class_class, v.goops_class_simple_method, amethod_slots);
    set_class_flags(v.goops_class_accessor_method, CLASSF_ACCESSOR_METHOD);

    make_stdcls(v.goops_class_applicable, "<applicable>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_entity, "<entity>", v.goops_class_entity_class,
                v.list2(v.goops_class_object, v.goops_class_applicable), SCM_EOL);
    make_stdcls(v.goops_class_entity_with_setter, "<entity-with-setter>", v.goops_class_entity_class,
                v.goops_class_entity, SCM_EOL);
    make_stdcls(v.goops_class_generic, "<generic>", v.goops_class_entity_class, v.goops_class_entity, gf_slots);
    set_class_flags(v.goops_class_generic, CLASSF_PURE_GENERIC);
    make_stdcls(v.goops_class_extended_generic, "<extended-generic>", v.goops_class_entity_class,
                v.goops_class_generic, egf_slots);
    set_class_flags(v.goops_class_extended_generic, CLASSF_PURE_GENERIC);
    make_stdcls(v.goops_class_generic_with_setter, "<generic-with-setter>", v.goops_class_entity_class,
                v.list2(v.goops_class_generic, v.goops_class_entity_with_setter), SCM_EOL);
    set_class_flags(v.goops_class_generic_with_setter, CLASSF_PURE_GENERIC);
    make_stdcls(v.goops_class_accessor, "<accessor>", v.goops_class_entity_class,
                v.goops_class_generic_with_setter, SCM_EOL);
    set_class_flags(v.goops_class_accessor, CLASSF_PURE_GENERIC);
    make_stdcls(v.goops_class_extended_generic_with_setter, "<extended-generic-with-setter>",
                v.goops_class_entity_class,
                v.list2(v.goops_class_generic_with_setter, v.goops_class_extended_generic), SCM_EOL);
    set_class_flags(v.goops_class_extended_generic_with_setter, CLASSF_PURE_GENERIC);
    make_stdcls(v.goops_class_extended_accessor, "<extended-accessor>",
                v.goops_class_entity_class,
                v.list2(v.goops_class_accessor, v.goops_class_extended_generic_with_setter), SCM_EOL);
    fix_cpl(v.goops_class_extended_accessor, v.goops_class_extended_generic, v.goops_class_generic);
    set_class_flags(v.goops_class_extended_accessor, CLASSF_PURE_GENERIC);

    // Primitive type classes
    make_stdcls(v.goops_class_boolean, "<boolean>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_char, "<char>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_list, "<list>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_pair, "<pair>", v.goops_class_class, v.goops_class_list, SCM_EOL);
    make_stdcls(v.goops_class_null, "<null>", v.goops_class_class, v.goops_class_list, SCM_EOL);
    make_stdcls(v.goops_class_string, "<string>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_symbol, "<symbol>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_vector, "<vector>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_number, "<number>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_complex, "<complex>", v.goops_class_class, v.goops_class_number, SCM_EOL);
    make_stdcls(v.goops_class_real, "<real>", v.goops_class_class, v.goops_class_complex, SCM_EOL);
    make_stdcls(v.goops_class_integer, "<integer>", v.goops_class_class, v.goops_class_real, SCM_EOL);
    make_stdcls(v.goops_class_fraction, "<fraction>", v.goops_class_class, v.goops_class_real, SCM_EOL);
    make_stdcls(v.goops_class_keyword, "<keyword>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_unknown, "<unknown>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_procedure, "<procedure>", v.goops_class_procedure_class, v.goops_class_applicable, SCM_EOL);
    make_stdcls(v.goops_class_procedure_with_setter, "<procedure-with-setter>", v.goops_class_procedure_class,
                v.goops_class_procedure, SCM_EOL);
    make_stdcls(v.goops_class_primitive_generic, "<primitive-generic>", v.goops_class_procedure_class,
                v.goops_class_procedure, SCM_EOL);
    make_stdcls(v.goops_class_port, "<port>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_input_port, "<input-port>", v.goops_class_class, v.goops_class_port, SCM_EOL);
    make_stdcls(v.goops_class_output_port, "<output-port>", v.goops_class_class, v.goops_class_port, SCM_EOL);
    make_stdcls(v.goops_class_input_output_port, "<input-output-port>", v.goops_class_class,
                v.list2(v.goops_class_input_port, v.goops_class_output_port), SCM_EOL);

    // Additional built-in types (tables, fluids, arrays, uniform vectors)
    make_stdcls(v.goops_class_hashtable, "<hashtable>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_fluid, "<fluid>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_array, "<array>", v.goops_class_class, v.goops_class_top, SCM_EOL);
    make_stdcls(v.goops_class_uvec, "<uvec>", v.goops_class_class, v.goops_class_top, SCM_EOL);
}

void create_port_classes() {
    auto& v = vm();
    const char* names[4] = {"file", "string", "soft", "pipe"};
    for (int k = 0; k < 4; ++k) {
        SCM base = make_class_from_template("<%s-port>", names[k], v.list1(v.goops_class_port), false);
        g_port_classes[k][0] = make_class_from_template(
            "<%s-input-port>", names[k], v.list2(base, v.goops_class_input_port), false);
        g_port_classes[k][1] = make_class_from_template(
            "<%s-output-port>", names[k], v.list2(base, v.goops_class_output_port), false);
        SCM io = make_class_from_template(
            "<%s-input-output-port>", names[k], v.list2(base, v.goops_class_input_output_port), false);
        // Patch CPL as in goops.c:2733
        set_raw_slot(io, SI_CPL, v.cons(io, v.cons(base, raw_slot(v.goops_class_input_output_port, SI_CPL))));
        g_port_classes[k][2] = io;
    }
}

void create_struct_classes() {
    auto& v = vm();
    if (!is_hashtable(v.struct_table)) return;
    auto* ht = as_obj<HashTableObj>(v.struct_table);
    for (SCM bucket : ht->buckets) {
        for (SCM p = bucket; is_pair(p); p = cdr(p)) {
            SCM entry = car(p);
            if (!is_pair(entry)) continue;
            SCM vt = car(entry);
            SCM data = cdr(entry);
            if (is_pair(data) && is_symbol(car(data))) {
                const char* nm = as_obj<SymbolObj>(car(data))->name.c_str();
                bool app = (struct_class_flags(vt) & CLASSF_OPERATOR) != 0;
                set_cdr(data, goops_make_extended_class(nm, app));
            }
        }
    }
}

void define_goops_proc(const char* name, int req, int opt, bool rst, SubrFn fn) {
    auto& v = vm();
    SCM subr = v.make_subr(name, req, opt, rst, std::move(fn));
    SCM sym = v.make_symbol(name);
    SCM var = v.eval_closure_lookup(v.goops_lookup_closure, sym, true);
    if (is_variable(var)) {
        as_obj<VariableObj>(var)->value = subr;
    }
}

} // namespace

// ============================================================================
// Public runtime hooks (called by eval.cpp, eq.cpp, print.cpp, numbers.cpp)
// ============================================================================

SCM goops_class_of(SCM obj) {
    auto& v = vm();
    if (obj.is_fixnum()) return v.goops_class_integer;
    if (obj.is_char()) return v.goops_class_char;
    if (obj.is_bool()) return v.goops_class_boolean;
    if (obj.is_null()) return v.goops_class_null;
    if (obj.is_imp()) return v.goops_class_unknown;

    switch (obj.to_obj()->tag) {
    case ObjTag::Pair: return v.goops_class_pair;
    case ObjTag::Closure: return v.goops_class_procedure;
    case ObjTag::Symbol: return v.goops_class_symbol;
    case ObjTag::Vector:
    case ObjTag::WeakVector: return v.goops_class_vector;
    case ObjTag::String: return v.goops_class_string;
    case ObjTag::Bignum: return v.goops_class_integer;
    case ObjTag::Real: return v.goops_class_real;
    case ObjTag::Complex: return v.goops_class_complex;
    case ObjTag::Fraction: return v.goops_class_fraction;
    case ObjTag::Subr:
        if (as_obj<SubrObj>(obj)->generic.is_true()) return v.goops_class_primitive_generic;
        return v.goops_class_procedure;
    case ObjTag::ProcWithSetter: return v.goops_class_procedure_with_setter;
    case ObjTag::Continuation:
    case ObjTag::EvalClosure:
    case ObjTag::Guardian: return v.goops_class_procedure;
    case ObjTag::Keyword: return v.goops_class_keyword;
    case ObjTag::HashTable: return v.goops_class_hashtable;
    case ObjTag::Fluid: return v.goops_class_fluid;
    case ObjTag::Array: return v.goops_class_array;
    case ObjTag::UniformVector:
    case ObjTag::BitVector: return v.goops_class_uvec;
    case ObjTag::Port: {
        auto* pt = as_obj<PortObj>(obj);
        int k = static_cast<int>(pt->kind);
        if (k < 0 || k > 3) return v.goops_class_port;
        int d = pt->is_output ? (pt->is_input ? 2 : 1) : 0;
        SCM pc = g_port_classes[k][d];
        return pc.is_true() ? pc : v.goops_class_port;
    }
    case ObjTag::Struct: {
        uint32_t of = obj_class_flags(obj);
        if (of & CLASSF_GOOPS_VALID) {
            return struct_vtable(obj);
        }
        if (of & CLASSF_GOOPS) {
            return test_change_class(obj);
        }
        // Ordinary struct: lazily associate a GOOPS class in struct_table
        SCM vt = struct_vtable(obj);
        SCM h = struct_create_handle(vt);
        SCM data = cdr(h);
        if (is_pair(data) && cdr(data).is_true()) {
            return cdr(data);
        }
        SCM nm = is_pair(data) ? car(data) : SCM_BOOL_F;
        const char* cname = is_symbol(nm) ? as_obj<SymbolObj>(nm)->name.c_str() : nullptr;
        bool app = (obj_class_flags(obj) & CLASSF_OPERATOR) != 0;
        SCM cls = goops_make_extended_class(cname, app);
        if (is_pair(data)) set_cdr(data, cls);
        return cls;
    }
    default:
        return v.goops_class_unknown;
    }
}

SCM goops_apply_generic(SCM gf, const SCM* args, size_t nargs) {
    auto& v = vm();
    if (!is_struct(gf)) v.wrong_type_arg("apply", 1, gf, "procedure");
    auto* gso = as_obj<StructObj>(gf);
    if (GCPP_LIKELY(gso->gf_cache_tag == gso->entity_proc && is_closure(gso->gf_cache_clo))) {
        if (nargs == 1 && gso->gf_cache_cls1.is_undefined() && is_struct(args[0])) {
            SCM c0 = as_obj<StructObj>(args[0])->vtable;
            if (GCPP_LIKELY(c0 == gso->gf_cache_cls0 && (as_obj<StructObj>(c0)->vtable_flags & CLASSF_GOOPS_VALID))) {
                return v.apply(gso->gf_cache_clo, args, 1);
            }
        } else if (nargs == 2 && is_struct(args[0]) && is_struct(args[1])) {
            SCM c0 = as_obj<StructObj>(args[0])->vtable;
            SCM c1 = as_obj<StructObj>(args[1])->vtable;
            if (GCPP_LIKELY(c0 == gso->gf_cache_cls0 && c1 == gso->gf_cache_cls1 &&
                (as_obj<StructObj>(c0)->vtable_flags & CLASSF_GOOPS_VALID) &&
                (as_obj<StructObj>(c1)->vtable_flags & CLASSF_GOOPS_VALID))) {
                return v.apply(gso->gf_cache_clo, args, 2);
            }
        }
    }
    SCM arg_list = v.list_n(args, nargs);
    SCM cache = gso->entity_proc;
    SCM cmethod = mcache_compute_cmethod(gf, cache, arg_list);
    if (!is_pair(cmethod) || !is_pair(cdr(cmethod))) {
        return cmethod;
    }
    // cmethod is (ENV FORMALS . BODY)
    SCM clo = SCM_BOOL_F;
    auto it = v.goops_cmethod_closures.find(cmethod);
    if (it != v.goops_cmethod_closures.end()) {
        clo = it->second;
    } else {
        SCM env = car(cmethod);
        SCM code = cdr(cmethod);
        SCM formals = car(code);
        SCM body = cdr(code);
        clo = v.make_closure(formals, body, env);
        v.goops_cmethod_closures[cmethod] = clo;
    }
    // Populate monomorphic inline cache for 1-arg / 2-arg GOOPS instance dispatches!
    if (is_pair(gso->entity_proc)) {
        if (nargs == 1 && is_struct(args[0])) {
            SCM c0 = as_obj<StructObj>(args[0])->vtable;
            if (is_struct(c0) && (as_obj<StructObj>(c0)->vtable_flags & CLASSF_GOOPS_VALID)) {
                gso->gf_cache_tag = gso->entity_proc;
                gso->gf_cache_cls0 = c0;
                gso->gf_cache_cls1 = SCM_UNDEFINED;
                gso->gf_cache_clo = clo;
            }
        } else if (nargs == 2 && is_struct(args[0]) && is_struct(args[1])) {
            SCM c0 = as_obj<StructObj>(args[0])->vtable;
            SCM c1 = as_obj<StructObj>(args[1])->vtable;
            if (is_struct(c0) && (as_obj<StructObj>(c0)->vtable_flags & CLASSF_GOOPS_VALID) &&
                is_struct(c1) && (as_obj<StructObj>(c1)->vtable_flags & CLASSF_GOOPS_VALID)) {
                gso->gf_cache_tag = gso->entity_proc;
                gso->gf_cache_cls0 = c0;
                gso->gf_cache_cls1 = c1;
                gso->gf_cache_clo = clo;
            }
        }
    }
    return v.apply_list(clo, arg_list);
}

// ============================================================================
// %init-goops-builtins & VM::init_goops()
// ============================================================================

static SCM init_goops_builtins_impl() {
    auto& v = vm();
    v.goops_module = v.current_module();
    v.goops_lookup_closure = v.module_lookup_closure(v.goops_module);
    v.goops_components = v.make_hashtable(37, WeakKind::KeyAlist);
    v.goops_list_of_no_method = v.list1(v.make_symbol("no-method"));
    SCM mm = v.lookup("make-mutex");
    v.goops_hell_mutex = is_variable(mm) ? v.call0(as_obj<VariableObj>(mm)->value) : SCM_BOOL_F;

    // Register all GOOPS C primitives inside (oop goops) module
    define_goops_proc("class-of", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return goops_class_of(a[0]);
    });
    define_goops_proc("%compute-slots", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("%compute-slots", 1, a[0]);
        return build_slots_list(raw_slot(a[0], SI_DIRECT_SLOTS), raw_slot(a[0], SI_CPL));
    });
    define_goops_proc("get-keyword", 3, 0, false, [](const SCM* a, size_t) -> SCM {
        return goops_get_keyword(a[0], a[1], a[2]);
    });
    define_goops_proc("%initialize-object", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        auto& vm_ = vm();
        SCM obj = a[0], initargs = a[1];
        validate_instance("%initialize-object", 1, obj);
        int64_t n_initargs = scm_ilength(initargs);
        if (n_initargs < 0 || (n_initargs & 1) != 0) {
            vm_.wrong_type_arg("%initialize-object", 2, initargs, "even-length keyword list");
        }
        SCM cls = struct_vtable(obj);
        SCM gns = raw_slot(cls, SI_GETTERS_N_SETTERS);
        SCM slots = raw_slot(cls, SI_SLOTS);
        SCM kw_init_kw = vm_.make_keyword("init-keyword");

        for (; is_pair(slots); gns = cdr(gns), slots = cdr(slots)) {
            SCM slot_spec = car(slots);
            SCM slot_value = SCM_UNDEFINED;
            if (is_pair(cdr(slot_spec))) {
                int64_t n = scm_ilength(cdr(slot_spec));
                if (n < 0 || (n & 1) != 0) {
                    vm_.misc_error("%initialize-object", "class contains bogus slot definition: ~S",
                                   vm_.list1(slot_spec));
                }
                SCM tmp = i_get_keyword(kw_init_kw, cdr(slot_spec), n, SCM_UNDEFINED, "%initialize-object");
                if (!tmp.is_undefined()) {
                    if (!is_keyword(tmp)) {
                        vm_.misc_error("%initialize-object", "initarg must be a keyword. It was ~S",
                                       vm_.list1(tmp));
                    }
                    slot_value = i_get_keyword(tmp, initargs, n_initargs, SCM_UNDEFINED, "%initialize-object");
                }
            }
            if (!slot_value.is_undefined()) {
                set_slot_value(cls, obj, car(gns), slot_value);
            } else {
                SCM init_thunk = car(cdr(car(gns)));
                if (init_thunk.is_true()) {
                    SCM cur = get_slot_value(cls, obj, car(gns));
                    if (cur.is_unbound()) {
                        set_slot_value(cls, obj, car(gns), vm_.call0(init_thunk));
                    }
                }
            }
        }
        return obj;
    });
    define_goops_proc("%prep-layout!", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return goops_prep_layout(a[0]);
    });
    define_goops_proc("%inherit-magic!", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        return goops_inherit_magic(a[0], a[1]);
    });
    define_goops_proc("instance?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_goops_instance(a[0]));
    });
    define_goops_proc("class-name", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("class-name", 1, a[0]);
        return goops_slot_ref(a[0], vm().make_symbol("name"));
    });
    define_goops_proc("class-direct-supers", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("class-direct-supers", 1, a[0]);
        return goops_slot_ref(a[0], vm().make_symbol("direct-supers"));
    });
    define_goops_proc("class-direct-slots", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("class-direct-slots", 1, a[0]);
        return goops_slot_ref(a[0], vm().make_symbol("direct-slots"));
    });
    define_goops_proc("class-direct-subclasses", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("class-direct-subclasses", 1, a[0]);
        return goops_slot_ref(a[0], vm().make_symbol("direct-subclasses"));
    });
    define_goops_proc("class-direct-methods", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("class-direct-methods", 1, a[0]);
        return goops_slot_ref(a[0], vm().make_symbol("direct-methods"));
    });
    define_goops_proc("class-precedence-list", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("class-precedence-list", 1, a[0]);
        return goops_slot_ref(a[0], vm().make_symbol("cpl"));
    });
    define_goops_proc("class-slots", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("class-slots", 1, a[0]);
        return goops_slot_ref(a[0], vm().make_symbol("slots"));
    });
    define_goops_proc("class-environment", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("class-environment", 1, a[0]);
        return goops_slot_ref(a[0], vm().make_symbol("environment"));
    });
    define_goops_proc("generic-function-name", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_generic("generic-function-name", 1, a[0]);
        return vm().procedure_property(a[0], vm().sym_name);
    });
    define_goops_proc("generic-function-methods", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return goops_gf_methods(a[0]);
    });
    define_goops_proc("method-generic-function", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_method("method-generic-function", 1, a[0]);
        return goops_slot_ref(a[0], vm().make_symbol("generic-function"));
    });
    define_goops_proc("method-specializers", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_method("method-specializers", 1, a[0]);
        return goops_slot_ref(a[0], vm().make_symbol("specializers"));
    });
    define_goops_proc("method-procedure", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_method("method-procedure", 1, a[0]);
        return goops_slot_ref(a[0], vm().make_symbol("procedure"));
    });
    define_goops_proc("accessor-method-slot-definition", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        if (!is_accessor_method(a[0])) {
            vm().wrong_type_arg("accessor-method-slot-definition", 1, a[0], "accessor");
        }
        return goops_slot_ref(a[0], vm().make_symbol("slot-definition"));
    });
    define_goops_proc("%tag-body", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        // Our AST evaluator executes unmemoized body directly (works in both apply_generic and local-eval)
        return a[0];
    });
    define_goops_proc("make-unbound", 0, 0, false, [](const SCM*, size_t) -> SCM {
        return SCM_UNBOUND;
    });
    define_goops_proc("unbound?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(a[0].is_unbound());
    });
    define_goops_proc("assert-bound", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        if (a[0].is_unbound()) return goops_call_gf1("slot-unbound", a[1]);
        return a[0];
    });
    define_goops_proc("@assert-bound-ref", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        int64_t idx = scm_to_int64(a[1], "@assert-bound-ref", 2);
        SCM val = raw_slot(a[0], static_cast<size_t>(idx));
        if (val.is_unbound()) return goops_call_gf1("slot-unbound", a[0]);
        return val;
    });
    define_goops_proc("@slot-ref", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        int64_t idx = scm_to_int64(a[1], "@slot-ref", 2);
        return raw_slot(a[0], static_cast<size_t>(idx));
    });
    define_goops_proc("@slot-set!", 3, 0, false, [](const SCM* a, size_t) -> SCM {
        int64_t idx = scm_to_int64(a[1], "@slot-set!", 2);
        set_raw_slot(a[0], static_cast<size_t>(idx), a[2]);
        return SCM_UNSPECIFIED;
    });
    define_goops_proc("%fast-slot-ref", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_instance("%fast-slot-ref", 1, a[0]);
        int64_t n = raw_slot(struct_vtable(a[0]), SI_NFIELDS).to_fixnum();
        int64_t idx = scm_to_int64(a[1], "%fast-slot-ref", 2);
        if (idx < 0 || idx >= n) vm().out_of_range("%fast-slot-ref", a[1]);
        return raw_slot(a[0], static_cast<size_t>(idx));
    });
    define_goops_proc("%fast-slot-set!", 3, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_instance("%fast-slot-set!", 1, a[0]);
        int64_t n = raw_slot(struct_vtable(a[0]), SI_NFIELDS).to_fixnum();
        int64_t idx = scm_to_int64(a[1], "%fast-slot-set!", 2);
        if (idx < 0 || idx >= n) vm().out_of_range("%fast-slot-set!", a[1]);
        set_raw_slot(a[0], static_cast<size_t>(idx), a[2]);
        return SCM_UNSPECIFIED;
    });
    define_goops_proc("slot-ref-using-class", 3, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("slot-ref-using-class", 1, a[0]);
        validate_instance("slot-ref-using-class", 2, a[1]);
        if (!is_symbol(a[2])) vm().wrong_type_arg("slot-ref-using-class", 3, a[2], "symbol");
        SCM res = get_slot_value_using_name(a[0], a[1], a[2]);
        if (res.is_unbound()) return goops_call_gf3("slot-unbound", a[0], a[1], a[2]);
        return res;
    });
    define_goops_proc("slot-set-using-class!", 4, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("slot-set-using-class!", 1, a[0]);
        validate_instance("slot-set-using-class!", 2, a[1]);
        if (!is_symbol(a[2])) vm().wrong_type_arg("slot-set-using-class!", 3, a[2], "symbol");
        return set_slot_value_using_name(a[0], a[1], a[2], a[3]);
    });
    define_goops_proc("slot-bound-using-class?", 3, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("slot-bound-using-class?", 1, a[0]);
        validate_instance("slot-bound-using-class?", 2, a[1]);
        if (!is_symbol(a[2])) vm().wrong_type_arg("slot-bound-using-class?", 3, a[2], "symbol");
        return SCM::from_bool(!get_slot_value_using_name(a[0], a[1], a[2]).is_unbound());
    });
    define_goops_proc("slot-exists-using-class?", 3, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("slot-exists-using-class?", 1, a[0]);
        validate_instance("slot-exists-using-class?", 2, a[1]);
        if (!is_symbol(a[2])) vm().wrong_type_arg("slot-exists-using-class?", 3, a[2], "symbol");
        return test_slot_existence(a[0], a[1], a[2]);
    });
    define_goops_proc("slot-ref", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        return goops_slot_ref(a[0], a[1]);
    });
    define_goops_proc("slot-set!", 3, 0, false, [](const SCM* a, size_t) -> SCM {
        return goops_slot_set(a[0], a[1], a[2]);
    });
    define_goops_proc("slot-bound?", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_instance("slot-bound?", 1, a[0]);
        SCM cls = test_change_class(a[0]);
        return SCM::from_bool(!get_slot_value_using_name(cls, a[0], a[1]).is_unbound());
    });
    define_goops_proc("slot-exists?", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_instance("slot-exists?", 1, a[0]);
        if (!is_symbol(a[1])) vm().wrong_type_arg("slot-exists?", 2, a[1], "symbol");
        SCM cls = test_change_class(a[0]);
        return test_slot_existence(cls, a[0], a[1]);
    });
    define_goops_proc("%allocate-instance", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        return goops_allocate_instance(a[0], a[1]);
    });
    define_goops_proc("%set-object-setter!", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        if (!is_struct(a[0])) vm().wrong_type_arg("%set-object-setter!", 1, a[0], "entity or operator class");
        auto* st = as_obj<StructObj>(a[0]);
        uint32_t of = obj_class_flags(a[0]);
        uint32_t cf = struct_class_flags(a[0]);
        if ((of & CLASSF_ENTITY) != 0 || st->is_entity) {
            st->entity_setter = a[1];
        } else if ((cf & CLASSF_OPERATOR) != 0) {
            if (st->slots.size() > SI_SETTER) st->slots[SI_SETTER] = a[1].bits;
            st->entity_setter = a[1];
        } else {
            vm().wrong_type_arg("%set-object-setter!", 1, a[0], "entity or operator class");
        }
        return SCM_UNSPECIFIED;
    });
    define_goops_proc("%modify-instance", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_instance("%modify-instance", 1, a[0]);
        validate_instance("%modify-instance", 2, a[1]);
        auto* o = as_obj<StructObj>(a[0]);
        auto* n = as_obj<StructObj>(a[1]);
        std::swap(o->vtable, n->vtable);
        std::swap(o->slots, n->slots);
        std::swap(o->tail_len, n->tail_len);
        std::swap(o->tail_start, n->tail_start);
        std::swap(o->has_tail, n->has_tail);
        std::swap(o->is_entity, n->is_entity);
        std::swap(o->is_operator, n->is_operator);
        std::swap(o->entity_proc, n->entity_proc);
        std::swap(o->entity_setter, n->entity_setter);
        std::swap(o->vtable_flags, n->vtable_flags);
        std::swap(o->data_id, n->data_id);
        return SCM_UNSPECIFIED;
    });
    define_goops_proc("%modify-class", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("%modify-class", 1, a[0]);
        validate_class("%modify-class", 2, a[1]);
        auto* o = as_obj<StructObj>(a[0]);
        auto* n = as_obj<StructObj>(a[1]);
        std::swap(o->vtable, n->vtable);
        std::swap(o->slots, n->slots);
        std::swap(o->tail_len, n->tail_len);
        std::swap(o->tail_start, n->tail_start);
        std::swap(o->has_tail, n->has_tail);
        std::swap(o->is_entity, n->is_entity);
        std::swap(o->is_operator, n->is_operator);
        std::swap(o->entity_proc, n->entity_proc);
        std::swap(o->entity_setter, n->entity_setter);
        std::swap(o->vtable_flags, n->vtable_flags);
        if (o->slots.size() > SI_VTABLE) o->slots[SI_VTABLE] = a[0].bits;
        if (n->slots.size() > SI_VTABLE) n->slots[SI_VTABLE] = a[1].bits;
        // Redirect existing instances of `old` to point to the old-class snapshot `new`
        // (mirrors Guile 1.8 where instances hold a pointer to the malloc'd vtable data block).
        for (Obj* cur = vm().heap().all_objects(); cur; cur = cur->gc_next) {
            if (cur->tag == ObjTag::Struct && cur != o && cur != n) {
                auto* st = static_cast<StructObj*>(cur);
                if (st->vtable == a[0]) st->vtable = a[1];
                else if (st->vtable == a[1]) st->vtable = a[0];
            }
        }
        return SCM_UNSPECIFIED;
    });
    define_goops_proc("%invalidate-class", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_class("%invalidate-class", 1, a[0]);
        clear_class_flags(a[0], CLASSF_GOOPS_VALID);
        return SCM_UNSPECIFIED;
    });
    define_goops_proc("%invalidate-method-cache!", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        if (!is_pure_generic(a[0])) {
            vm().wrong_type_arg("%invalidate-method-cache!", 1, a[0], "pure generic function");
        }
        std::function<void(SCM)> invalidate = [&](SCM gf) {
            SCM used_by = raw_slot(gf, SI_GF_USED_BY);
            if (used_by.is_true()) {
                SCM methods = raw_slot(gf, SI_GF_METHODS);
                for (SCM u = used_by; is_pair(u); u = cdr(u)) {
                    invalidate(car(u));
                }
                clear_method_cache(gf);
                for (SCM m = methods; is_pair(m); m = cdr(m)) {
                    set_raw_slot(car(m), SI_M_CODE_TABLE, SCM_EOL);
                }
            }
            SCM n = raw_slot(gf, SI_GF_N_SPECIALIZED);
            SCM cache = as_obj<StructObj>(gf)->entity_proc;
            if (is_pair(cache) && is_pair(cdr(cache)) && is_pair(cdr(cdr(cache)))) {
                set_car(cdr(cdr(cache)), n);
            }
        };
        invalidate(a[0]);
        return SCM_UNSPECIFIED;
    });
    define_goops_proc("generic-capability?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        if (!vm().is_procedure(a[0])) vm().wrong_type_arg("generic-capability?", 1, a[0], "procedure");
        return SCM::from_bool(is_subr(a[0]) && as_obj<SubrObj>(a[0])->generic_capable);
    });
    define_goops_proc("enable-primitive-generic!", 0, 0, true, [](const SCM* a, size_t nargs) -> SCM {
        auto& vm_ = vm();
        SCM subrs = nargs > 0 ? a[0] : SCM_EOL;
        SCM kw_name = vm_.make_keyword("name");
        while (is_pair(subrs)) {
            SCM subr = car(subrs);
            if (!is_subr(subr) || !as_obj<SubrObj>(subr)->generic_capable) {
                vm_.wrong_type_arg("enable-primitive-generic!", 1, subr, "generic-capable primitive");
            }
            auto* so = as_obj<SubrObj>(subr);
            so->generic = goops_make(vm_.list3(vm_.goops_class_generic, kw_name, vm_.make_symbol(so->name)));
            subrs = cdr(subrs);
        }
        return SCM_UNSPECIFIED;
    });
    define_goops_proc("primitive-generic-generic", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        auto& vm_ = vm();
        if (is_subr(a[0]) && as_obj<SubrObj>(a[0])->generic_capable) {
            auto* so = as_obj<SubrObj>(a[0]);
            if (so->generic.is_false()) {
                SCM kw_name = vm_.make_keyword("name");
                so->generic = goops_make(vm_.list3(vm_.goops_class_generic, kw_name, vm_.make_symbol(so->name)));
            }
            return so->generic;
        }
        vm_.wrong_type_arg("primitive-generic-generic", 1, a[0], "primitive generic");
    });
    auto cam_fn = [](const SCM* a, size_t) -> SCM {
        validate_generic("%compute-applicable-methods", 1, a[0]);
        int64_t n = scm_ilength(a[1]);
        if (n < 0) vm().wrong_type_arg("%compute-applicable-methods", 2, a[1], "list");
        return compute_applicable_methods_impl(a[0], a[1], n, true);
    };
    define_goops_proc("%compute-applicable-methods", 2, 0, false, cam_fn);
    define_goops_proc("compute-applicable-methods", 2, 0, false, cam_fn);
    define_goops_proc("make", 0, 0, true, [](const SCM* a, size_t nargs) -> SCM {
        return goops_make(nargs > 0 ? a[0] : SCM_EOL);
    });
    define_goops_proc("find-method", 0, 0, true, [](const SCM* a, size_t nargs) -> SCM {
        SCM l = nargs > 0 ? a[0] : SCM_EOL;
        int64_t len = scm_ilength(l);
        if (len <= 0) vm().wrong_num_args("find-method");
        SCM gf = car(l);
        l = cdr(l);
        validate_generic("find-method", 1, gf);
        if (raw_slot(gf, SI_GF_METHODS).is_null()) {
            vm().misc_error("find-method", "no methods for generic ~S", vm().list1(gf));
        }
        return compute_applicable_methods_impl(gf, l, len - 1, true);
    });
    define_goops_proc("%method-more-specific?", 3, 0, false, [](const SCM* a, size_t) -> SCM {
        validate_method("%method-more-specific?", 1, a[0]);
        validate_method("%method-more-specific?", 2, a[1]);
        int64_t len = scm_ilength(a[2]);
        int64_t m1_s = scm_ilength(raw_slot(a[0], SI_M_SPECIALIZERS));
        int64_t m2_s = scm_ilength(raw_slot(a[1], SI_M_SPECIALIZERS));
        if (len < 0 || (len < m1_s && len < m2_s)) {
            vm().wrong_type_arg("%method-more-specific?", 3, a[2], "list of classes");
        }
        std::vector<SCM> targs;
        targs.reserve(static_cast<size_t>(len));
        for (SCM p = a[2]; is_pair(p); p = cdr(p)) {
            if (!is_goops_class(car(p))) {
                vm().wrong_type_arg("%method-more-specific?", 3, a[2], "list of classes");
            }
            targs.push_back(car(p));
        }
        return SCM::from_bool(more_specific_p(a[0], a[1], targs.data()));
    });
    define_goops_proc("pure-generic?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_pure_generic(a[0]));
    });
    define_goops_proc("%goops-loaded", 0, 0, false, [](const SCM*, size_t) -> SCM {
        vm().goops_loaded = true;
        return SCM_UNSPECIFIED;
    });

    // Build all GOOPS built-in classes
    create_basic_classes();
    create_standard_classes();
    create_struct_classes();
    create_port_classes();

    // Create no-applicable-method generic
    SCM nam_sym = v.make_symbol("no-applicable-method");
    v.goops_no_applicable_method = v.permanent_object(
        goops_make(v.list3(v.goops_class_generic, v.make_keyword("name"), nam_sym)));
    goops_defvar(nam_sym, v.goops_no_applicable_method);

    return SCM_UNSPECIFIED;
}

void VM::init_goops() {
    define_proc("%init-goops-builtins", 0, 0, false, [](const SCM*, size_t) -> SCM {
        return init_goops_builtins_impl();
    });
    define_proc("primitive-generic?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_subr(a[0]) && as_obj<SubrObj>(a[0])->generic.is_true());
    });
}

} // namespace guile
