#include "guile_cpp/vm.hpp"
#include <cstring>

namespace guile {

static bool is_tail_prot(char c) {
    return c == 'R' || c == 'W' || c == 'O';
}

static void validate_layout_string(string fields, SCM orig) {
    if (static_cast<size_t>(N(fields)) % 2 == 1) {
        vm().misc_error("make-struct-layout", "odd length field specification: ~S", vm().list1(orig));
    }
    for (size_t x = 0; x < static_cast<size_t>(N(fields)); x += 2) {
        char ftype = fields[x];
        char fprot = fields[x + 1];
        if (ftype != 'u' && ftype != 'p' && ftype != 's') {
            vm().misc_error("make-struct-layout", "unrecognized field type: ~S",
                            vm().list1(SCM::from_char(static_cast<unsigned char>(ftype))));
        }
        if (fprot == 'w') {
            if (ftype == 's') vm().misc_error("make-struct-layout", "self fields not writable");
        } else if (fprot == 'r' || fprot == 'o') {
            // ok
        } else if (is_tail_prot(fprot)) {
            if (ftype == 's') vm().misc_error("make-struct-layout", "self fields not allowed in tail array");
            if (x != static_cast<size_t>(N(fields)) - 2) vm().misc_error("make-struct-layout", "tail array field must be last field in layout");
        } else {
            vm().misc_error("make-struct-layout", "unrecognized ref specification: ~S",
                            vm().list1(SCM::from_char(static_cast<unsigned char>(fprot))));
        }
    }
}

SCM struct_vtable(SCM s) {
    if (!is_struct(s)) vm().wrong_type_arg("struct-vtable", 1, s, "struct");
    return as_obj<StructObj>(s)->vtable;
}

SCM struct_layout(SCM s) {
    SCM vt = struct_vtable(s);
    if (!is_struct(vt)) return SCM_BOOL_F;
    auto* vto = as_obj<StructObj>(vt);
    if ((N(vto->slots) == 0)) return SCM_BOOL_F;
    return SCM(vto->slots[VTABLE_INDEX_LAYOUT]);
}

bool is_struct_vtable(SCM x) {
    if (!is_struct(x)) return false;
    auto* st = as_obj<StructObj>(x);
    // A struct is a vtable if its own vtable's layout starts with "prsrpw"
    // AND slot 0 (layout) is a symbol and slot 1 is self!
    SCM my_layout = struct_layout(x);
    if (!is_symbol(my_layout)) return false;
    string desc = as_obj<SymbolObj>(my_layout)->name;
    auto req = as_obj<StringObj>(vm().required_vtable_fields)->str();
    if (static_cast<size_t>(N(desc)) < static_cast<size_t>(N(req))) return false;
    if (std::memcmp(str_data(desc), str_data(req), N(req)) != 0) return false;
    if (static_cast<size_t>(N(st->slots)) < 2) return false;
    return is_symbol(SCM(st->slots[VTABLE_INDEX_LAYOUT]));
}

static void init_struct_fields(StructObj* obj, SCM handle, SCM layout_sym, size_t tail_elts, SCM inits) {
    string desc = as_obj<SymbolObj>(layout_sym)->name;
    size_t nfields = static_cast<size_t>(N(desc)) / 2;
    bool has_tail = (nfields > 0 && is_tail_prot(desc[static_cast<size_t>(N(desc)) - 1]));
    size_t total_slots = has_tail ? (nfields - 1 + 1 + tail_elts) : nfields;
    gcpp_assign(obj->slots, total_slots, 0);
    obj->has_tail = has_tail;
    obj->tail_len = has_tail ? tail_elts : 0;
    obj->tail_start = has_tail ? (nfields - 1) : 0;

    size_t slot_idx = 0;
    SCM cur_init = inits;
    for (size_t f = 0; f < nfields; ++f) {
        char ftype = desc[f * 2];
        char fprot = desc[f * 2 + 1];
        if (is_tail_prot(fprot)) {
            // Store tail length word first, just like Guile 1.8!
            obj->slots[slot_idx++] = static_cast<uint64_t>(tail_elts);
            char norm_prot = (fprot == 'R') ? 'r' : (fprot == 'W' ? 'w' : 'o');
            for (size_t t = 0; t < tail_elts; ++t) {
                if (ftype == 'u') {
                    if ((norm_prot == 'r' || norm_prot == 'w') && is_pair(cur_init)) {
                        obj->slots[slot_idx++] = scm_to_uint64(car(cur_init), "make-struct", 3);
                        cur_init = cdr(cur_init);
                    } else {
                        obj->slots[slot_idx++] = 0;
                    }
                } else if (ftype == 'p') {
                    if ((norm_prot == 'r' || norm_prot == 'w') && is_pair(cur_init)) {
                        obj->slots[slot_idx++] = car(cur_init).bits;
                        cur_init = cdr(cur_init);
                    } else {
                        obj->slots[slot_idx++] = SCM_BOOL_F.bits;
                    }
                }
            }
            break;
        }
        if (ftype == 'u') {
            if ((fprot == 'r' || fprot == 'w') && is_pair(cur_init)) {
                obj->slots[slot_idx++] = scm_to_uint64(car(cur_init), "make-struct", 3);
                cur_init = cdr(cur_init);
            } else {
                obj->slots[slot_idx++] = 0;
            }
        } else if (ftype == 'p') {
            if ((fprot == 'r' || fprot == 'w') && is_pair(cur_init)) {
                obj->slots[slot_idx++] = car(cur_init).bits;
                cur_init = cdr(cur_init);
            } else {
                obj->slots[slot_idx++] = SCM_BOOL_F.bits;
            }
        } else if (ftype == 's') {
            obj->slots[slot_idx++] = handle.bits;
        }
    }
}

SCM make_struct_impl(SCM vtable, size_t tail_len, SCM init_list) {
    if (!is_struct_vtable(vtable)) {
        vm().wrong_type_arg("make-struct", 1, vtable, "vtable");
    }
    auto* vto = as_obj<StructObj>(vtable);
    SCM layout = SCM(vto->slots[VTABLE_INDEX_LAYOUT]);
    if (!is_symbol(layout)) {
        vm().misc_error("make-struct", "invalid vtable layout");
    }
    string desc = as_obj<SymbolObj>(layout)->name;
    bool has_tail = (!(N(desc) == 0) && is_tail_prot(desc[N(desc) - 1]));
    if (!has_tail && tail_len != 0) {
        vm().misc_error("make-struct", "tail array not allowed unless layout ends with R, W, or O");
    }

    auto* st = vm().heap().alloc<StructObj>();
    SCM handle = SCM::from_obj(st);
    st->vtable = vtable;
    if (vto->vtable_flags & 1U) {
        st->is_entity = true;
    }
    init_struct_fields(st, handle, layout, tail_len, init_list);
    if (vtable == vm().module_type) {
        if (!(N(st->slots) == 0) && is_hashtable(SCM(st->slots[0]))) {
            as_obj<HashTableObj>(SCM(st->slots[0]))->is_module_obarray = true;
        }
    }
    return handle;
}

SCM make_vtable_vtable_impl(SCM extra_fields, size_t tail_len, SCM init_list) {
    if (!is_string(extra_fields)) {
        vm().wrong_type_arg("make-vtable-vtable", 1, extra_fields, "string");
    }
    string full_desc = as_obj<StringObj>(vm().required_vtable_fields)->str() *
                            as_obj<StringObj>(extra_fields)->str();
    SCM full_str = vm().make_string(full_desc);
    validate_layout_string(full_desc, full_str);
    SCM layout_sym = vm().make_symbol(full_desc);

    auto* vt = vm().heap().alloc<StructObj>();
    SCM handle = SCM::from_obj(vt);
    vt->vtable = handle; // A vtable-vtable is its own vtable!
    SCM all_inits = vm().cons(layout_sym, init_list);
    init_struct_fields(vt, handle, layout_sym, tail_len, all_inits);
    return handle;
}

SCM struct_ref(SCM handle, size_t p) {
    if (!is_struct(handle)) vm().wrong_type_arg("struct-ref", 1, handle, "struct");
    auto* st = as_obj<StructObj>(handle);
    SCM layout = struct_layout(handle);
    string desc = as_obj<SymbolObj>(layout)->name;
    size_t layout_len = static_cast<size_t>(N(desc));
    size_t n_base = st->has_tail ? st->tail_start : (layout_len / 2);

    if (p >= static_cast<size_t>(N(st->slots))) {
        vm().out_of_range("struct-ref", vm().make_uinteger(p));
    }
    char ftype = 'p';
    char fprot = 'r';
    if (p < n_base) {
        ftype = desc[p * 2];
        fprot = desc[p * 2 + 1];
        if (fprot != 'r' && fprot != 'w') {
            vm().misc_error("struct-ref", "ref denied for field ~A", vm().list1(vm().make_uinteger(p)));
        }
    } else if (st->has_tail && p == st->tail_start) {
        // Reading the tail length word returns the length integer
        return vm().make_uinteger(st->tail_len);
    } else if (st->has_tail) {
        ftype = desc[layout_len - 2];
        fprot = desc[layout_len - 1];
        if (fprot != 'R' && fprot != 'W') {
            vm().misc_error("struct-ref", "ref denied for field ~A", vm().list1(vm().make_uinteger(p)));
        }
    } else {
        vm().out_of_range("struct-ref", vm().make_uinteger(p));
    }

    if (ftype == 'u') return vm().make_uinteger(st->slots[p]);
    return SCM(st->slots[p]);
}

void struct_set(SCM handle, size_t p, SCM val) {
    if (!is_struct(handle)) vm().wrong_type_arg("struct-set!", 1, handle, "struct");
    auto* st = as_obj<StructObj>(handle);
    SCM layout = struct_layout(handle);
    string desc = as_obj<SymbolObj>(layout)->name;
    size_t layout_len = static_cast<size_t>(N(desc));
    size_t n_base = st->has_tail ? st->tail_start : (layout_len / 2);

    if (p >= static_cast<size_t>(N(st->slots))) {
        vm().out_of_range("struct-set!", vm().make_uinteger(p));
    }
    char ftype = 'p';
    if (p < n_base) {
        ftype = desc[p * 2];
        char fprot = desc[p * 2 + 1];
        if (fprot != 'w') {
            vm().misc_error("struct-set!", "set! denied for field ~A", vm().list1(vm().make_uinteger(p)));
        }
    } else if (st->has_tail && p == st->tail_start) {
        vm().misc_error("struct-set!", "set! denied for field ~A", vm().list1(vm().make_uinteger(p)));
    } else if (st->has_tail && desc[layout_len - 1] == 'W') {
        ftype = desc[layout_len - 2];
    } else {
        vm().misc_error("struct-set!", "set! denied for field ~A", vm().list1(vm().make_uinteger(p)));
    }

    if (ftype == 'u') {
        st->slots[p] = scm_to_uint64(val, "struct-set!", 3);
    } else if (ftype == 'p') {
        st->slots[p] = val.bits;
        if (st->vtable == vm().module_type) {
            if (p == 0 && is_hashtable(val)) {
                as_obj<HashTableObj>(val)->is_module_obarray = true;
            }
            if (p <= 2) ++vm().module_epoch;
        }
    } else {
        vm().misc_error("struct-set!", "self fields immutable");
    }
}

SCM struct_create_handle(SCM vt) {
    SCM h = hash_create_handle(vm().struct_table, vt, SCM_BOOL_F, 1);
    if (cdr(h).is_false()) {
        set_cdr(h, vm().cons(SCM_BOOL_F, SCM_BOOL_F));
    }
    return h;
}

SCM struct_vtable_name(SCM vt) {
    if (!is_struct_vtable(vt)) vm().wrong_type_arg("struct-vtable-name", 1, vt, "vtable");
    SCM h = struct_create_handle(vt);
    return car(cdr(h));
}

void set_struct_vtable_name(SCM vt, SCM name) {
    if (!is_struct_vtable(vt)) vm().wrong_type_arg("set-struct-vtable-name!", 1, vt, "vtable");
    if (!is_symbol(name)) vm().wrong_type_arg("set-struct-vtable-name!", 2, name, "symbol");
    SCM h = struct_create_handle(vt);
    set_car(cdr(h), name);
}

bool struct_equal(SCM a, SCM b) {
    if (a == b) return true;
    auto* sa = as_obj<StructObj>(a);
    auto* sb = as_obj<StructObj>(b);
    if (sa->vtable != sb->vtable) return false;
    if (static_cast<size_t>(N(sa->slots)) != static_cast<size_t>(N(sb->slots))) return false;
    SCM layout = struct_layout(a);
    string desc = as_obj<SymbolObj>(layout)->name;
    size_t n_base = sa->has_tail ? sa->tail_start : (static_cast<size_t>(N(desc)) / 2);
    for (size_t i = 0; i < static_cast<size_t>(N(sa->slots)); ++i) {
        if (sa->has_tail && i == sa->tail_start) {
            if (sa->slots[i] != sb->slots[i]) return false;
            continue;
        }
        char ftype = (i < n_base) ? desc[i * 2] : desc[static_cast<size_t>(N(desc)) - 2];
        if (ftype == 'u') {
            if (sa->slots[i] != sb->slots[i]) return false;
        } else if (ftype == 's') {
            continue; // self reference
        } else {
            if (!scm_equal(SCM(sa->slots[i]), SCM(sb->slots[i]))) return false;
        }
    }
    return true;
}

void VM::init_struct() {
    struct_table = make_hashtable(31, WeakKind::KeyAlist);
    required_vtable_fields = make_string("prsrpw");
    vtable_vtable_no_extra = make_vtable_vtable_impl(make_string(""), 0, SCM_EOL);

    define("vtable-index-layout",  make_integer(VTABLE_INDEX_LAYOUT));
    define("vtable-index-vtable",  make_integer(VTABLE_INDEX_VTABLE));
    define("vtable-index-printer", make_integer(VTABLE_INDEX_PRINTER));
    define("vtable-offset-user",   make_integer(VTABLE_OFFSET_USER));

    define_proc("make-struct-layout", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("make-struct-layout", 1, a[0], "string");
        auto sv = as_obj<StringObj>(a[0])->str();
        validate_layout_string(sv, a[0]);
        return make_symbol(sv);
    });

    define_proc("struct?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_struct(a[0]));
    });

    define_proc("struct-vtable?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_struct_vtable(a[0]));
    });

    define_proc("make-struct", 2, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        int64_t tlen = scm_to_int64(a[1], "make-struct", 2);
        if (tlen < 0) out_of_range("make-struct", a[1]);
        SCM inits = nargs > 2 ? a[2] : SCM_EOL;
        return make_struct_impl(a[0], static_cast<size_t>(tlen), inits);
    });

    define_proc("make-vtable-vtable", 2, 0, true, [this](const SCM* a, size_t nargs) -> SCM {
        int64_t tlen = scm_to_int64(a[1], "make-vtable-vtable", 2);
        if (tlen < 0) out_of_range("make-vtable-vtable", a[1]);
        SCM inits = nargs > 2 ? a[2] : SCM_EOL;
        return make_vtable_vtable_impl(a[0], static_cast<size_t>(tlen), inits);
    });

    define_proc("make-vtable", 1, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        if (!is_string(a[0])) wrong_type_arg("make-vtable", 1, a[0], "string");
        SCM layout = make_symbol(as_obj<StringObj>(a[0])->str());
        validate_layout_string(as_obj<StringObj>(a[0])->str(), a[0]);
        SCM printer = (nargs >= 2 && !a[1].is_undefined()) ? a[1] : SCM_BOOL_F;
        return make_struct_impl(vtable_vtable_no_extra, 0, list2(layout, printer));
    });

    define_proc("struct-ref", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t pos = scm_to_int64(a[1], "struct-ref", 2);
        if (pos < 0) out_of_range("struct-ref", a[1]);
        return struct_ref(a[0], static_cast<size_t>(pos));
    });

    define_proc("struct-set!", 3, 0, false, [this](const SCM* a, size_t) -> SCM {
        int64_t pos = scm_to_int64(a[1], "struct-set!", 2);
        if (pos < 0) out_of_range("struct-set!", a[1]);
        struct_set(a[0], static_cast<size_t>(pos), a[2]);
        return a[2];
    });

    define_proc("struct-vtable", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return struct_vtable(a[0]);
    });

    define_proc("struct-vtable-tag", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_struct_vtable(a[0])) wrong_type_arg("struct-vtable-tag", 1, a[0], "vtable");
        return make_uinteger(a[0].bits >> 3);
    });

    define_proc("struct-vtable-name", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return struct_vtable_name(a[0]);
    });

    define_proc("set-struct-vtable-name!", 2, 0, false, [](const SCM* a, size_t) -> SCM {
        set_struct_vtable_name(a[0], a[1]);
        return SCM_UNSPECIFIED;
    });

    // Entities & Operators (objects.c)
    metaclass_standard = make_vtable_vtable_impl(make_string(""), 0, list3(SCM_BOOL_F, SCM_EOL, SCM_EOL));
    metaclass_operator = make_vtable_vtable_impl(make_string("popo"), 0, list3(SCM_BOOL_F, SCM_EOL, SCM_EOL));
    SCM entity_layout = make_symbol("");
    SCM entity_cls = make_struct_impl(metaclass_standard, 0, list4(entity_layout, SCM_BOOL_F, SCM_EOL, SCM_EOL));
    as_obj<StructObj>(entity_cls)->vtable_flags |= (CLASSF_OPERATOR | CLASSF_ENTITY);
    define("<class>", metaclass_standard);
    define("<operator-class>", metaclass_operator);
    define("<entity>", entity_cls);

    define_proc("entity?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_struct(a[0]) &&
            ((obj_class_flags(a[0]) & CLASSF_ENTITY) != 0 || as_obj<StructObj>(a[0])->is_entity));
    });
    define_proc("operator?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        if (!is_struct(a[0])) return SCM_BOOL_F;
        uint32_t f = obj_class_flags(a[0]);
        return SCM::from_bool(((f & CLASSF_OPERATOR) != 0 && (f & CLASSF_ENTITY) == 0) ||
                              (as_obj<StructObj>(a[0])->is_operator && !as_obj<StructObj>(a[0])->is_entity));
    });
    define_proc("valid-object-procedure?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_closure(a[0]) || is_subr(a[0]));
    });
    define_proc("set-object-procedure!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_struct(a[0])) wrong_type_arg("set-object-procedure!", 1, a[0], "entity");
        auto* st = as_obj<StructObj>(a[0]);
        uint32_t cf = struct_class_flags(a[0]);
        uint32_t of = obj_class_flags(a[0]);
        bool is_ent = (of & CLASSF_ENTITY) != 0 || st->is_entity;
        bool is_op_cls = (cf & CLASSF_OPERATOR) != 0;
        if (!is_ent && !is_op_cls) {
            wrong_type_arg("set-object-procedure!", 1, a[0], "entity or operator class");
        }
        if (is_ent && (of & CLASSF_PURE_GENERIC) != 0) {
            wrong_type_arg("set-object-procedure!", 1, a[0], "non-generic entity");
        }
        if (!is_closure(a[1]) && !is_subr(a[1])) {
            wrong_type_arg("set-object-procedure!", 2, a[1], "procedure");
        }
        if (is_ent) {
            st->is_entity = true;
            st->entity_proc = a[1];
        } else {
            if (static_cast<size_t>(N(st->slots)) > 3) st->slots[3] = a[1].bits;
            st->entity_proc = a[1];
        }
        return SCM_UNSPECIFIED;
    });
    define_proc("object-procedure", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_struct(a[0])) wrong_type_arg("object-procedure", 1, a[0], "entity");
        auto* st = as_obj<StructObj>(a[0]);
        uint32_t of = obj_class_flags(a[0]);
        uint32_t cf = struct_class_flags(a[0]);
        if ((of & CLASSF_ENTITY) != 0 || st->is_entity) return st->entity_proc;
        if ((cf & CLASSF_OPERATOR) != 0 && static_cast<size_t>(N(st->slots)) > 3) return SCM(st->slots[3]);
        wrong_type_arg("object-procedure", 1, a[0], "entity or operator class");
    });
    define_proc("make-class-object", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_struct(a[0])) wrong_type_arg("make-class-object", 1, a[0], "struct");
        if (!is_string(a[1])) wrong_type_arg("make-class-object", 2, a[1], "string");
        auto sv = as_obj<StringObj>(a[1])->str();
        validate_layout_string(sv, a[1]);
        SCM layout = make_symbol(sv);
        SCM c = make_struct_impl(a[0], 0, list4(layout, SCM_BOOL_F, SCM_EOL, SCM_EOL));
        if (a[0] == metaclass_operator) as_obj<StructObj>(c)->vtable_flags |= CLASSF_OPERATOR;
        return c;
    });
    define_proc("make-subclass-object", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_struct(a[0])) wrong_type_arg("make-subclass-object", 1, a[0], "struct");
        if (!is_string(a[1])) wrong_type_arg("make-subclass-object", 2, a[1], "string");
        auto* st = as_obj<StructObj>(a[0]);
        SCM pl = SCM(st->slots[VTABLE_INDEX_LAYOUT]);
        string combined = as_obj<SymbolObj>(pl)->name * as_obj<StringObj>(a[1])->str();
        SCM combined_str = make_string(combined);
        validate_layout_string(combined, combined_str);
        SCM layout = make_symbol(combined);
        SCM c = make_struct_impl(st->vtable, 0, list4(layout, SCM_BOOL_F, SCM_EOL, SCM_EOL));
        as_obj<StructObj>(c)->vtable_flags |= st->vtable_flags;
        return c;
    });
}

} // namespace guile
