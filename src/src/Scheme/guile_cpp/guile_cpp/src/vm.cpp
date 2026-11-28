#include "guile_cpp/vm.hpp"

namespace guile {

static VM* g_vm = nullptr;

VM& vm() {
    if (!g_vm) g_vm = new VM();
    return *g_vm;
}

VM::VM() = default;
VM::~VM() = default;

void VM::init(void* stack_base, bool load_boot) {
    if (initialized_) return;
    g_vm = this;
    heap_.set_stack_base(stack_base);

    // Disable GC during initial bootstrap before root marker is wired up
    heap_.set_enabled(false);

    // Wire up VM root marker for GC!
    heap_.callback_data = this;
    heap_.vm_marker = [](void* data, const Marker& mark) {
        static_cast<VM*>(data)->mark_roots(mark);
    };

    // Intern cached symbols
    sym_quote = make_symbol("quote");
    sym_quasiquote = make_symbol("quasiquote");
    sym_unquote = make_symbol("unquote");
    sym_unquote_splicing = make_symbol("unquote-splicing");
    sym_syntax = make_symbol("syntax");
    sym_quasisyntax = make_symbol("quasisyntax");
    sym_unsyntax = make_symbol("unsyntax");
    sym_unsyntax_splicing = make_symbol("unsyntax-splicing");
    sym_lambda = make_symbol("lambda");
    sym_if = make_symbol("if");
    sym_set_x = make_symbol("set!");
    sym_define = make_symbol("define");
    sym_begin = make_symbol("begin");
    sym_let = make_symbol("let");
    sym_let_star = make_symbol("let*");
    sym_letrec = make_symbol("letrec");
    sym_cond = make_symbol("cond");
    sym_else = make_symbol("else");
    sym_arrow = make_symbol("=>");
    sym_case = make_symbol("case");
    sym_do = make_symbol("do");
    sym_and = make_symbol("and");
    sym_or = make_symbol("or");
    sym_delay = make_symbol("delay");
    sym_at = make_symbol("@");
    sym_atat = make_symbol("@@");
    sym_the_environment = make_symbol("the-environment");
    sym_eval_when = make_symbol("eval-when");
    sym_misc_error = make_symbol("misc-error");
    sym_wrong_type_arg = make_symbol("wrong-type-arg");
    sym_wrong_num_args = make_symbol("wrong-number-of-args");
    sym_out_of_range = make_symbol("out-of-range");
    sym_system_error = make_symbol("system-error");
    sym_numerical_overflow = make_symbol("numerical-overflow");
    sym_read_error = make_symbol("read-error");
    sym_unbound_variable = make_symbol("unbound-variable");
    sym_syntax_error = make_symbol("syntax-error");
    sym_goops_error = make_symbol("goops-error");
    sym_quit = make_symbol("quit");
    sym_name = make_symbol("name");
    sym_documentation = make_symbol("documentation");
    sym_source = make_symbol("source");
    sym_filename = make_symbol("filename");
    sym_line = make_symbol("line");
    sym_column = make_symbol("column");
    sym_module = make_symbol("module");
    sym_system_module = make_symbol("system-module");
    sym_prefix = make_symbol("prefix");
    sym_postfix = make_symbol("postfix");

    // Core bootstrap storage needed before any define_proc() calls!
    default_dynamic_state = heap_.alloc_scm<DynamicStateObj>();
    current_dyn_state = default_dynamic_state;
    pre_modules_obarray = make_hashtable(1533, WeakKind::None);
    as_obj<HashTableObj>(pre_modules_obarray)->is_module_obarray = true;
    the_module_fluid = make_fluid(SCM_BOOL_F);

    // Initialize subsystems in dependency order (mirrors libguile/init.c!)
    init_fluids();
    init_modules();
    init_error();
    init_symbols();
    init_eq();
    init_list();
    init_vectors();
    init_chars();
    init_strings();
    init_numbers();
    init_hashtab();
    init_properties();
    init_struct();
    init_ports();
    init_read();
    init_print();
    init_srfi13_14();
    init_eval();
    init_load();
    init_options();
    init_posix();
    init_stime();
    init_regex();
    init_socket();
    init_arrays();
    init_sort();
    init_random();
    init_srfi1_60();
    init_goops();
    init_environments();
    init_misc();

    init_load_path();

    // Enable GC now that all core roots are initialized
    heap_.after_gc_callback = [](void* data) {
        static_cast<VM*>(data)->run_after_gc_hook();
    };
    heap_.set_enabled(true);
    initialized_ = true;

    if (load_boot) {
        load_startup_files();
    }
    init_bytecode_builtins();
}

void VM::mark_roots(const Marker& mark) {
    mark(read_opts.keywords);
    mark(module_type);
    mark(pre_modules_obarray);
    mark(the_module_fluid);
    mark(the_root_module_var);
    mark(resolve_module_var);
    mark(process_define_module_var);
    mark(process_use_modules_var);
    mark(module_export_var);
    mark(module_make_local_var);
    mark(cur_inport_fluid);
    mark(cur_outport_fluid);
    mark(cur_errport_fluid);
    mark(cur_loadport_fluid);
    mark(cur_reader_fluid);
    mark(the_last_stack_fluid);
    mark(builtin_begin_macro);
    mark(default_dynamic_state);
    mark(current_dyn_state);
    mark(current_dynwind);
    mark(current_cont);
    mark(obj_props_table);
    mark(src_props_table);
    mark(struct_table);
    mark(required_vtable_fields);
    mark(vtable_vtable_no_extra);
    mark(read_hash_procedures);
    mark(after_gc_hook);
    mark(before_gc_hook);
    mark(exit_hook);
    mark(features_var);
    mark(load_path_var);
    mark(load_extensions_var);
    mark(load_hook_var);
    mark(load_verbosely_var);
    mark(random_state_var);
    mark(program_args_);

    // Cached symbols
    mark(sym_quote); mark(sym_quasiquote); mark(sym_unquote); mark(sym_unquote_splicing);
    mark(sym_syntax); mark(sym_quasisyntax); mark(sym_unsyntax); mark(sym_unsyntax_splicing);
    mark(sym_lambda); mark(sym_if); mark(sym_set_x); mark(sym_define);
    mark(sym_begin); mark(sym_let); mark(sym_let_star); mark(sym_letrec);
    mark(sym_cond); mark(sym_else); mark(sym_arrow); mark(sym_case);
    mark(sym_do); mark(sym_and); mark(sym_or); mark(sym_delay);
    mark(sym_at); mark(sym_atat); mark(sym_the_environment); mark(sym_eval_when);
    mark(sym_misc_error); mark(sym_wrong_type_arg); mark(sym_wrong_num_args);
    mark(sym_out_of_range); mark(sym_system_error); mark(sym_numerical_overflow);
    mark(sym_read_error); mark(sym_unbound_variable); mark(sym_syntax_error);
    mark(sym_goops_error); mark(sym_quit);
    mark(sym_name); mark(sym_documentation); mark(sym_source);
    mark(sym_filename); mark(sym_line); mark(sym_column);
    mark(sym_module); mark(sym_system_module); mark(sym_prefix); mark(sym_postfix);

    // GOOPS classes & runtime roots
    mark(metaclass_standard); mark(metaclass_operator);
    mark(goops_class_top); mark(goops_class_object); mark(goops_class_class);
    mark(goops_class_applicable); mark(goops_class_entity_class);
    mark(goops_class_entity); mark(goops_class_entity_with_setter);
    mark(goops_class_generic); mark(goops_class_generic_with_setter);
    mark(goops_class_method); mark(goops_class_simple_method);
    mark(goops_class_accessor_method); mark(goops_class_accessor);
    mark(goops_class_procedure_class); mark(goops_class_operator_class);
    mark(goops_class_operator_with_setter_class);
    mark(goops_class_extended_generic); mark(goops_class_extended_generic_with_setter);
    mark(goops_class_extended_accessor);
    mark(goops_class_foreign_class); mark(goops_class_foreign_object);
    mark(goops_class_foreign_slot); mark(goops_class_self);
    mark(goops_class_protected); mark(goops_class_opaque);
    mark(goops_class_read_only); mark(goops_class_protected_opaque);
    mark(goops_class_protected_read_only); mark(goops_class_scm);
    mark(goops_class_int); mark(goops_class_float); mark(goops_class_double);
    mark(goops_class_boolean); mark(goops_class_char); mark(goops_class_list);
    mark(goops_class_pair); mark(goops_class_null); mark(goops_class_string);
    mark(goops_class_symbol); mark(goops_class_vector); mark(goops_class_number);
    mark(goops_class_integer); mark(goops_class_fraction);
    mark(goops_class_real); mark(goops_class_complex); mark(goops_class_keyword);
    mark(goops_class_procedure); mark(goops_class_procedure_with_setter);
    mark(goops_class_primitive_generic);
    mark(goops_class_port); mark(goops_class_input_port);
    mark(goops_class_output_port); mark(goops_class_input_output_port);
    mark(goops_class_hashtable); mark(goops_class_fluid);
    mark(goops_class_uvec); mark(goops_class_array);
    mark(goops_class_unknown);
    mark(goops_no_applicable_method);
    mark(goops_module); mark(goops_lookup_closure);
    mark(goops_components); mark(goops_list_of_no_method);
    mark(goops_hell_mutex);

    // Primitive-generic subr handles
    mark(subr_eqv_p); mark(subr_equal_p);
    mark(subr_write); mark(subr_display);
    mark(subr_num_eq); mark(subr_num_lt); mark(subr_num_le);
    mark(subr_num_gt); mark(subr_num_ge);
    mark(subr_add); mark(subr_sub); mark(subr_mul); mark(subr_div);

    for (SCM r : permanent_roots) mark(r);
    bytecode_mark_roots(mark);

    // Symbol intern table
    for (auto& [k, v] : symbol_table) {
        mark(v);
    }
}

void VM::run_after_gc_hook() {
    if (initialized_ && is_hook(after_gc_hook) && !as_obj<HookObj>(after_gc_hook)->procs.is_null()) {
        SCM procs = scm_list_copy(as_obj<HookObj>(after_gc_hook)->procs);
        while (is_pair(procs)) {
            call0(car(procs));
            procs = cdr(procs);
        }
    }
}

} // namespace guile
