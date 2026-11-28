#include "guile_cpp/vm.hpp"

namespace guile {

SCM VM::make_fluid(SCM def) {
    size_t id = next_fluid_id++;
    SCM fl = heap_.alloc_scm<FluidObj>(id, def);
    if (is_dynamic_state(default_dynamic_state)) {
        auto* ds = as_obj<DynamicStateObj>(default_dynamic_state);
        if (static_cast<size_t>(N(ds->values)) <= id) gcpp_resize(ds->values, id + 1, SCM_UNDEFINED);
        ds->values[id] = def;
    }
    if (is_dynamic_state(current_dyn_state) && current_dyn_state != default_dynamic_state) {
        auto* ds = as_obj<DynamicStateObj>(current_dyn_state);
        if (static_cast<size_t>(N(ds->values)) <= id) gcpp_resize(ds->values, id + 1, SCM_UNDEFINED);
        ds->values[id] = def;
    }
    return fl;
}

SCM VM::fluid_ref(SCM fluid) {
    if (!is_fluid(fluid)) wrong_type_arg("fluid-ref", 1, fluid, "fluid");
    auto* f = as_obj<FluidObj>(fluid);
    if (is_dynamic_state(current_dyn_state)) {
        auto* ds = as_obj<DynamicStateObj>(current_dyn_state);
        if (f->id < static_cast<size_t>(N(ds->values)) && !ds->values[f->id].is_undefined()) {
            return ds->values[f->id];
        }
    }
    return f->default_val;
}

void VM::fluid_set(SCM fluid, SCM val) {
    if (!is_fluid(fluid)) wrong_type_arg("fluid-set!", 1, fluid, "fluid");
    auto* f = as_obj<FluidObj>(fluid);
    if (is_dynamic_state(current_dyn_state)) {
        auto* ds = as_obj<DynamicStateObj>(current_dyn_state);
        if (static_cast<size_t>(N(ds->values)) <= f->id) gcpp_resize(ds->values, f->id + 1, SCM_UNDEFINED);
        ds->values[f->id] = val;
    }
}

SCM VM::make_dynamic_state(SCM parent) {
    auto* ds = heap_.alloc<DynamicStateObj>();
    SCM src = parent.is_true() ? parent : default_dynamic_state;
    if (is_dynamic_state(src)) {
        ds->values = copy(as_obj<DynamicStateObj>(src)->values);  // (owned: fluid-set! changes it)
    }
    return SCM::from_obj(ds);
}

SCM VM::current_dynamic_state() {
    return current_dyn_state;
}

SCM VM::set_current_dynamic_state(SCM ds) {
    if (!is_dynamic_state(ds)) wrong_type_arg("set-current-dynamic-state", 1, ds, "dynamic-state");
    SCM old = current_dyn_state;
    current_dyn_state = ds;
    return old;
}

void VM::init_fluids() {
    if (!is_dynamic_state(default_dynamic_state)) {
        default_dynamic_state = heap_.alloc_scm<DynamicStateObj>();
        current_dyn_state = default_dynamic_state;
    }

    define_proc("make-fluid", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM def = (nargs >= 1 && !a[0].is_undefined()) ? a[0] : SCM_BOOL_F;
        return make_fluid(def);
    });
    define_proc("fluid?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_fluid(a[0]));
    });
    define_proc("fluid-ref", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return fluid_ref(a[0]);
    });
    define_proc("fluid-set!", 2, 0, false, [this](const SCM* a, size_t) -> SCM {
        fluid_set(a[0], a[1]);
        return SCM_UNSPECIFIED;
    });
    define_proc("fluid-bound?", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        if (!is_fluid(a[0])) wrong_type_arg("fluid-bound?", 1, a[0], "fluid");
        return SCM_BOOL_T;
    });

    define_proc("make-dynamic-state", 0, 1, false, [this](const SCM* a, size_t nargs) -> SCM {
        SCM par = (nargs >= 1 && !a[0].is_undefined()) ? a[0] : SCM_BOOL_F;
        if (par.is_true() && !is_dynamic_state(par)) wrong_type_arg("make-dynamic-state", 1, par, "dynamic-state");
        return make_dynamic_state(par);
    });
    define_proc("dynamic-state?", 1, 0, false, [](const SCM* a, size_t) -> SCM {
        return SCM::from_bool(is_dynamic_state(a[0]));
    });
    define_proc("current-dynamic-state", 0, 0, false, [this](const SCM*, size_t) -> SCM {
        return current_dynamic_state();
    });
    define_proc("set-current-dynamic-state", 1, 0, false, [this](const SCM* a, size_t) -> SCM {
        return set_current_dynamic_state(a[0]);
    });

    // Note: with-fluids*, with-fluid*, with-dynamic-state are registered as SpecialSubr in eval.cpp
    // so they participate directly in the CEK continuation & dynamic-wind chain!
}

} // namespace guile
