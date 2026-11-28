#pragma once

#include "guile_cpp/scm.hpp"
#include <algorithm>
#include <new>
#include <type_traits>
#include <vector>
#include <string>
#include <string_view>

namespace guile {

class VM;

// ============================================================================
// Garbage Collector / Heap (Slab Pools + Mark-Sweep)
// ============================================================================

inline constexpr uint8_t OBJ_FLAG_FREE = 0x80;
inline constexpr uint8_t OBJ_FLAG_JIT_CLOSURE = 0x02;
inline constexpr uint8_t OBJ_FLAG_JIT_NURSERY = 0x04;

template <typename T, size_t SlabCount>
class SlabPool {
public:
    struct Slab {
        alignas(T) uint8_t storage[sizeof(T) * SlabCount];
        Slab* next = nullptr;
    };
    struct Range {
        uintptr_t lo;
        uintptr_t hi;
    };

    SlabPool() = default;
    ~SlabPool() {
        for (Slab* s = slabs_; s; ) {
            if constexpr (!std::is_trivially_destructible_v<T>) {
                for (size_t i = 0; i < SlabCount; ++i) {
                    T* slot = reinterpret_cast<T*>(s->storage + i * sizeof(T));
                    if ((slot->flags & OBJ_FLAG_FREE) == 0) slot->~T();
                }
            }
            Slab* nxt = s->next;
            delete s;
            s = nxt;
        }
    }

    SlabPool(const SlabPool&) = delete;
    SlabPool& operator=(const SlabPool&) = delete;

    void grow() {
        auto* s = new Slab();
        s->next = slabs_;
        slabs_ = s;
        uintptr_t lo = reinterpret_cast<uintptr_t>(s->storage);
        uintptr_t hi = lo + sizeof(T) * SlabCount;
        if (lo < min_addr_) min_addr_ = lo;
        if (hi > max_addr_) max_addr_ = hi;
        ranges_.push_back({lo, hi});
        std::sort(ranges_.begin(), ranges_.end(), [](const Range& a, const Range& b) { return a.lo < b.lo; });
        for (size_t i = SlabCount; i > 0; --i) {
            T* slot = reinterpret_cast<T*>(s->storage + (i - 1) * sizeof(T));
            slot->gc_mark = 0;
            slot->flags = OBJ_FLAG_FREE;
            slot->gc_next = free_list_;
            free_list_ = slot;
        }
    }

    template <typename... Args>
    inline T* alloc(Args&&... args) {
        if (GCPP_UNLIKELY(!free_list_)) grow();
        T* slot = free_list_;
        free_list_ = static_cast<T*>(slot->gc_next);
        new (slot) T(std::forward<Args>(args)...);
        ++live_;
        return slot;
    }

    inline bool contains_live(uintptr_t ptr) const noexcept {
        if (ptr < min_addr_ || ptr >= max_addr_) return false;
        size_t n = ranges_.size();
        if (n <= 8) {
            for (size_t i = 0; i < n; ++i) {
                const auto& r = ranges_[i];
                if (ptr < r.lo) return false;
                if (ptr < r.hi) {
                    if ((ptr - r.lo) % sizeof(T) != 0) return false;
                    auto* o = reinterpret_cast<const Obj*>(ptr);
                    return (o->flags & OBJ_FLAG_FREE) == 0;
                }
            }
            return false;
        }
        size_t lo = 0, hi = n;
        while (lo < hi) {
            size_t mid = lo + ((hi - lo) >> 1);
            if (ptr < ranges_[mid].lo) {
                hi = mid;
            } else if (ptr >= ranges_[mid].hi) {
                lo = mid + 1;
            } else {
                if ((ptr - ranges_[mid].lo) % sizeof(T) != 0) return false;
                auto* o = reinterpret_cast<const Obj*>(ptr);
                return (o->flags & OBJ_FLAG_FREE) == 0;
            }
        }
        return false;
    }

    size_t sweep() noexcept {
        size_t freed = 0;
        for (Slab* s = slabs_; s; s = s->next) {
            uint8_t* base = s->storage;
            for (size_t i = 0; i < SlabCount; ++i) {
                T* slot = reinterpret_cast<T*>(base + i * sizeof(T));
                if (slot->flags & OBJ_FLAG_FREE) continue;
                if (slot->gc_mark) {
                    slot->gc_mark = 0;
                } else {
                    if constexpr (!std::is_trivially_destructible_v<T>) {
                        slot->~T();
                    }
                    slot->flags = OBJ_FLAG_FREE;
                    slot->gc_mark = 0;
                    slot->gc_next = free_list_;
                    free_list_ = slot;
                    --live_;
                    ++freed;
                }
            }
        }
        return freed;
    }

    size_t live() const noexcept { return live_; }

private:
    Slab* slabs_ = nullptr;
    T* free_list_ = nullptr;
    size_t live_ = 0;
    uintptr_t min_addr_ = UINTPTR_MAX;
    uintptr_t max_addr_ = 0;
    std::vector<Range> ranges_;
};

struct GcRootSpan {
    const SCM* data;
    size_t count;
    GcRootSpan* prev;
};

class Heap {
public:
    Heap() = default;
    ~Heap();

    void set_stack_base(void* base) noexcept { stack_base_ = base; }
    void* stack_base() const noexcept { return stack_base_; }

    template <typename T, typename... Args>
    inline T* alloc(Args&&... args) {
        if (GCPP_UNLIKELY(gc_enabled_ && alloc_since_gc_ >= gc_threshold_)) {
            collect();
        }
        ++live_count_;
        ++alloc_since_gc_;
        ++total_allocated_;
        if constexpr (std::is_same_v<T, PairObj>) {
            return pair_pool_.alloc(std::forward<Args>(args)...);
        } else if constexpr (std::is_same_v<T, ContFrameObj>) {
            return cont_pool_.alloc(std::forward<Args>(args)...);
        } else if constexpr (std::is_same_v<T, ClosureObj>) {
            return closure_pool_.alloc(std::forward<Args>(args)...);
        } else if constexpr (std::is_same_v<T, VariableObj>) {
            return var_pool_.alloc(std::forward<Args>(args)...);
        } else if constexpr (std::is_same_v<T, RealObj>) {
            return real_pool_.alloc(std::forward<Args>(args)...);
        } else if constexpr (std::is_same_v<T, FractionObj>) {
            return frac_pool_.alloc(std::forward<Args>(args)...);
        } else if constexpr (std::is_same_v<T, BignumObj>) {
            return bignum_pool_.alloc(std::forward<Args>(args)...);
        } else if constexpr (std::is_same_v<T, BoxObj>) {
            return box_pool_.alloc(std::forward<Args>(args)...);
        } else if constexpr (std::is_same_v<T, EvalClosureObj>) {
            return eval_closure_pool_.alloc(std::forward<Args>(args)...);
        } else if constexpr (std::is_same_v<T, DynWindObj>) {
            return dynwind_pool_.alloc(std::forward<Args>(args)...);
        } else if constexpr (std::is_same_v<T, ContinuationObj>) {
            return continuation_pool_.alloc(std::forward<Args>(args)...);
        } else if constexpr (std::is_same_v<T, StringObj>) {
            return string_pool_.alloc(std::forward<Args>(args)...);
        } else if constexpr (std::is_same_v<T, SymbolObj>) {
            return symbol_pool_.alloc(std::forward<Args>(args)...);
        } else {
            T* obj = new T(std::forward<Args>(args)...);
            obj->gc_next = head_;
            head_ = obj;
            if constexpr (std::is_same_v<T, WeakVectorObj> || std::is_same_v<T, EnvironmentObj>) {
                weak_containers_.push_back(obj);
            } else if constexpr (std::is_same_v<T, HashTableObj>) {
                if (obj->weak != WeakKind::None) weak_containers_.push_back(obj);
            } else if constexpr (std::is_same_v<T, GuardianObj>) {
                guardians_.push_back(obj);
            } else if constexpr (std::is_same_v<T, PortObj>) {
                ports_.push_back(obj);
            }
            return obj;
        }
    }

    template <typename T, typename... Args>
    inline SCM alloc_scm(Args&&... args) {
        return SCM::from_obj(alloc<T>(std::forward<Args>(args)...));
    }

    void add_root(SCM* slot);
    void remove_root(SCM* slot);
    void add_permanent(SCM val);

    void push_root_span(GcRootSpan* span) noexcept {
        span->prev = root_spans_;
        root_spans_ = span;
    }
    void pop_root_span() noexcept {
        if (root_spans_) root_spans_ = root_spans_->prev;
    }

    void collect();
    void set_enabled(bool e) noexcept { gc_enabled_ = e; }
    bool is_enabled() const noexcept { return gc_enabled_; }

    size_t live_count() const noexcept { return live_count_; }
    size_t total_allocated() const noexcept { return total_allocated_; }
    size_t gc_times() const noexcept { return gc_times_; }
    double gc_time_sec() const noexcept { return gc_time_sec_; }
    size_t gc_threshold() const noexcept { return gc_threshold_; }
    void set_min_gc_threshold(size_t n) noexcept {
        min_gc_threshold_ = n;
        if (gc_threshold_ < n) gc_threshold_ = n;
    }
    void set_initial_gc_threshold(size_t n) noexcept {
        if (gc_times_ == 0 && gc_threshold_ < n) gc_threshold_ = n;
    }

    size_t pair_live() const noexcept { return pair_pool_.live(); }
    size_t closure_live() const noexcept { return closure_pool_.live(); }
    size_t var_live() const noexcept { return var_pool_.live(); }
    size_t eval_closure_live() const noexcept { return eval_closure_pool_.live(); }

    // For iterating non-slab live objects (ports, structs, hash tables, etc.)
    Obj* all_objects() const noexcept { return head_; }
    const std::vector<PortObj*>& open_ports() const noexcept { return ports_; }

    // Custom mark callback for VM internal tables
    // (TeXmacs: function pointers, called with the VM)
    void (*vm_marker)(void* data, const Marker& mark) = nullptr;
    void (*after_gc_callback)(void* data) = nullptr;
    void* callback_data = nullptr;

    // (public for the callbacks of the collector, see gc.cpp)
    void mark_scm(SCM x, std::vector<Obj*>& worklist);
private:
    void trace_obj(Obj* obj, std::vector<Obj*>& worklist);
    void drain_worklist(std::vector<Obj*>& worklist);
    void process_guardians(std::vector<Obj*>& worklist);
    void clear_weak_refs();
    void sweep();
    static void destroy_obj(Obj* cur);

    Obj* head_ = nullptr;
    SlabPool<PairObj, 4096> pair_pool_;
    SlabPool<ContFrameObj, 2048> cont_pool_;
    SlabPool<ClosureObj, 1024> closure_pool_;
    SlabPool<VariableObj, 1024> var_pool_;
    SlabPool<RealObj, 1024> real_pool_;
    SlabPool<FractionObj, 1024> frac_pool_;
    SlabPool<BignumObj, 1024> bignum_pool_;
    SlabPool<BoxObj, 2048> box_pool_;
    SlabPool<EvalClosureObj, 256> eval_closure_pool_;
    SlabPool<DynWindObj, 2048> dynwind_pool_;
    SlabPool<ContinuationObj, 1024> continuation_pool_;
    SlabPool<StringObj, 2048> string_pool_;
    SlabPool<SymbolObj, 1024> symbol_pool_;

    std::vector<uintptr_t> non_slab_ptrs_;
    std::vector<Obj*> weak_containers_;
    std::vector<GuardianObj*> guardians_;
    std::vector<PortObj*> ports_;
    std::vector<SCM*> explicit_roots_;
    std::vector<SCM> permanent_roots_;
    GcRootSpan* root_spans_ = nullptr;
    void* stack_base_ = nullptr;

    size_t live_count_ = 0;
    size_t alloc_since_gc_ = 0;
    size_t total_allocated_ = 0;
    size_t min_gc_threshold_ = 262144;
    size_t gc_threshold_ = 262144;
    size_t gc_times_ = 0;
    double gc_time_sec_ = 0.0;
    bool gc_enabled_ = true;
    bool in_gc_ = false;
};

// RAII guard to root a std::vector<SCM> across GC
struct ScmRootVec {
    std::vector<SCM> vec;
    GcRootSpan span;
    Heap* heap;

    explicit ScmRootVec(Heap& h, size_t reserve_n = 0) : heap(&h) {
        if (reserve_n) vec.reserve(reserve_n);
        span.data = vec.data();
        span.count = vec.size();
        heap->push_root_span(&span);
    }
    ~ScmRootVec() {
        heap->pop_root_span();
    }
    void push_back(SCM x) {
        vec.push_back(x);
        span.data = vec.data();
        span.count = vec.size();
    }
    void sync() {
        span.data = vec.data();
        span.count = vec.size();
    }
    SCM& operator[](size_t i) { return vec[i]; }
    const SCM& operator[](size_t i) const { return vec[i]; }
    size_t size() const noexcept { return vec.size(); }
    bool empty() const noexcept { return vec.empty(); }
    const SCM* data() const noexcept { return vec.data(); }
};

// ============================================================================
// Options structs
// ============================================================================

struct ReadOptions {
    bool copy_source = false;
    bool positions = false;
    bool case_insensitive = false;
    SCM keywords = SCM_BOOL_F; // #f, 'prefix, 'postfix
    bool r6rs_hex_escapes = false;
};

struct PrintOptions {
    bool closures = false;
    bool source = false;
    bool highlight_objects = false;
    bool quote_keywordish_symbols = false;
    bool escape_newlines = true;
};

struct EvalOptions {
    int stack_limit = 20000;
};

struct DebugOptions {
    bool debug = false;
    bool backtrace = false;
    bool backwards = false;
    int depth = 20;
    int maxdepth = 1000;
    int frames = 3;
    int indent = 10;
    int width = 79;
    bool show_file_name = true;
    bool procnames = true;
    bool cheap = true;
    bool trace = false;
    size_t stack = 0; // 0 => default C-stack safety cap (5 MB)
};

// ============================================================================
// Guile 1.8.8 C++ Runtime / VM
// ============================================================================

class VM {
public:
    VM();
    ~VM();

    void init(void* stack_base, bool load_boot = true);
    void mark_roots(const Marker& mark);
    void run_after_gc_hook();
    bool is_initialized() const noexcept { return initialized_; }

    Heap& heap() noexcept { return heap_; }

    // Constructors
    SCM cons(SCM a, SCM d);
    SCM cons_source(SCM a, SCM d, SCM src);
    SCM list1(SCM a);
    SCM list2(SCM a, SCM b);
    SCM list3(SCM a, SCM b, SCM c);
    SCM list4(SCM a, SCM b, SCM c, SCM d);
    SCM list_n(const SCM* elts, size_t n);
    SCM make_symbol(std::string_view name);
    SCM make_symbol(string name);
    SCM make_symbol(const char* name) { return make_symbol(std::string_view(name)); }
    SCM make_uninterned_symbol(std::string_view name);
    SCM make_keyword(SCM sym);
    SCM make_keyword(std::string_view name);
    SCM make_string(std::string_view sv, bool read_only = false);
    SCM make_vector(size_t n, SCM fill = SCM_UNSPECIFIED);
    SCM make_real(double v);
    SCM make_complex(double r, double i);
    SCM make_integer(int64_t v);
    SCM make_uinteger(uint64_t v);
    SCM make_bignum(BigInt v);
    SCM make_fraction(SCM num, SCM den);
    SCM make_variable(SCM val = SCM_UNDEFINED);
    SCM make_subr(const std::string& name, int req, int opt, bool rst, SubrFn fn, SpecialSubr sp = SpecialSubr::None);
    SCM make_closure(SCM formals, SCM body, SCM env);
    SCM make_macro(MacroKind kind, SCM transformer, SCM name = SCM_BOOL_F);
    SCM make_hashtable(size_t nbuckets = 31, WeakKind weak = WeakKind::None);
    SCM make_fluid(SCM def = SCM_BOOL_F);
    SCM make_values(SCM list_of_vals);

    // Defining / Looking up bindings
    SCM define(const std::string& name, SCM val);
    SCM define_sym(SCM sym, SCM val);
    SCM define_proc(const std::string& name, int req, int opt, bool rst, SubrFn fn, SpecialSubr sp = SpecialSubr::None);
    SCM lookup(const std::string& name);
    SCM lookup_sym(SCM sym);
    SCM sym2var(SCM sym, SCM eval_closure, bool definep);

    // Module helpers
    SCM current_module();
    SCM set_current_module(SCM mod);
    SCM current_module_lookup_closure();
    SCM current_module_transformer();
    SCM module_lookup_closure(SCM mod);
    SCM module_variable(SCM mod, SCM sym);
    SCM eval_closure_lookup(SCM eclo, SCM sym, bool definep);
    bool is_module(SCM obj) const;
    void post_boot_init_modules();

    // Fluid / Dynamic State helpers
    SCM fluid_ref(SCM fluid);
    void fluid_set(SCM fluid, SCM val);
    SCM current_dynamic_state();
    SCM set_current_dynamic_state(SCM ds);
    SCM make_dynamic_state(SCM parent = SCM_BOOL_F);

    // Standard ports
    SCM current_input_port();
    SCM current_output_port();
    SCM current_error_port();
    SCM current_load_port();
    SCM set_current_input_port(SCM p);
    SCM set_current_output_port(SCM p);
    SCM set_current_error_port(SCM p);

    // Properties
    SCM object_properties(SCM obj);
    void set_object_properties(SCM obj, SCM props);
    SCM source_properties(SCM obj);
    void set_source_properties(SCM obj, SCM props);
    void set_source_position(SCM pair_obj, SCM filename, int32_t line, int32_t col);
    SCM source_property(SCM obj, SCM key);
    void set_source_property(SCM obj, SCM key, SCM val);
    SCM procedure_properties(SCM proc);
    void set_procedure_properties(SCM proc, SCM props);
    SCM procedure_property(SCM proc, SCM key);
    void set_procedure_property(SCM proc, SCM key, SCM val);

    // Evaluation & Application (CEK Machine)
    SCM eval(SCM expr, SCM env);
    SCM primitive_eval(SCM expr);
    SCM eval_string(std::string_view code);
    SCM apply(SCM proc, const SCM* args, size_t nargs);
    SCM apply_list(SCM proc, SCM arg_list);
    SCM call0(SCM proc);
    SCM call1(SCM proc, SCM a);
    SCM call2(SCM proc, SCM a, SCM b);
    SCM call3(SCM proc, SCM a, SCM b, SCM c);
    SCM call4(SCM proc, SCM a, SCM b, SCM c, SCM d);
    bool is_procedure(SCM x) const;

    // Reader & Printer
    SCM read_from_port(SCM port);
    SCM read_from_string(std::string_view s);
    void write_to_port(SCM obj, SCM port, bool display_mode = false);
    std::string write_to_string(SCM obj, bool display_mode = false);

    // Error throwing
    [[noreturn]] void throw_scm(SCM key, SCM args);
    [[noreturn]] void error(SCM key, const char* subr, const std::string& msg, SCM args = SCM_EOL, SCM rest = SCM_BOOL_F);
    inline size_t c_stack_used() const {
        volatile char probe = 0;
        void* base = heap_.stack_base();
        if (!base) return 0;
        ptrdiff_t used = static_cast<const char*>(base) - const_cast<const char*>(&probe);
        if (used < 0) used = -used;
        return static_cast<size_t>(used);
    }
    inline size_t c_stack_limit_bytes() const {
        size_t limit_bytes = 5UL * 1024UL * 1024UL;
        if (debug_opts.stack > 0) {
            size_t opt_bytes = debug_opts.stack * sizeof(void*);
            if (opt_bytes < limit_bytes) limit_bytes = opt_bytes;
        }
        return limit_bytes;
    }
    inline void check_c_stack() {
        if (GCPP_UNLIKELY(c_stack_used() > c_stack_limit_bytes())) {
            error(make_symbol("stack-overflow"), nullptr, "Stack overflow", SCM_EOL, SCM_BOOL_F);
        }
    }
    [[noreturn]] void misc_error(const char* subr, const std::string& msg, SCM args = SCM_EOL);
    [[noreturn]] void wrong_type_arg(const char* subr, int pos, SCM bad_val, const char* expected = nullptr);
    [[noreturn]] void wrong_num_args(const char* subr, SCM proc = SCM_BOOL_F);
    [[noreturn]] void out_of_range(const char* subr, SCM bad_val);
    [[noreturn]] void syserror(const char* subr, int errnum, const std::string& extra = "");
    [[noreturn]] void num_overflow(const char* subr);

    // Extensions & SMOBs
    void register_extension(const std::string& lib, const std::string& init_fn, void (*fn)());
    void load_extension(const std::string& lib, const std::string& init_fn);
    size_t make_smob_type(const SmobTypeInfo& info);
    SmobTypeInfo& smob_info(size_t idx) { return smob_types_.at(idx); }

    // Load path & startup
    void init_load_path();
    SCM search_load_path(const std::string& filename);
    SCM primitive_load(const std::string& filename);
    SCM primitive_load_path(const std::string& filename);
    void load_startup_files();
    void set_program_arguments(int argc, char** argv);
    void set_program_arguments_list(SCM args);
    SCM program_arguments() const noexcept { return program_args_; }

    // Public state fields accessed by subsystem files
    ReadOptions read_opts;
    PrintOptions print_opts;
    EvalOptions eval_opts;
    DebugOptions debug_opts;

    bool module_system_booted = false;
    SCM module_type = SCM_BOOL_F;
    SCM pre_modules_obarray = SCM_BOOL_F;
    SCM the_module_fluid = SCM_BOOL_F;
    SCM the_root_module_var = SCM_BOOL_F;
    SCM resolve_module_var = SCM_BOOL_F;
    SCM process_define_module_var = SCM_BOOL_F;
    SCM process_use_modules_var = SCM_BOOL_F;
    SCM module_export_var = SCM_BOOL_F;
    SCM module_make_local_var = SCM_BOOL_F;

    // Standard fluids
    SCM cur_inport_fluid = SCM_BOOL_F;
    SCM cur_outport_fluid = SCM_BOOL_F;
    SCM cur_errport_fluid = SCM_BOOL_F;
    SCM cur_loadport_fluid = SCM_BOOL_F;
    SCM cur_reader_fluid = SCM_BOOL_F;
    SCM the_last_stack_fluid = SCM_BOOL_F;
    SCM builtin_begin_macro = SCM_BOOL_F;
    SCM default_dynamic_state = SCM_BOOL_F;
    SCM current_dyn_state = SCM_BOOL_F;
    size_t next_fluid_id = 0;

    // CEK active state (per active trampoline)
    SCM current_dynwind = SCM_EOL;
    SCM current_cont = SCM_EOL;
    uint64_t next_dw_id = 1;
    int trampoline_depth = 0;

    // Tables & hooks
    SCM obj_props_table = SCM_BOOL_F;
    SCM src_props_table = SCM_BOOL_F;
    struct SrcPosEntry {
        SCM filename;
        int32_t line;
        int32_t col;
    };
    hashmap<SCM, SrcPosEntry> src_pos_map;  // (key: the pair)
    SCM struct_table = SCM_BOOL_F;
    SCM required_vtable_fields = SCM_BOOL_F;
    SCM vtable_vtable_no_extra = SCM_BOOL_F;
    SCM read_hash_procedures = SCM_EOL;
    SCM after_gc_hook = SCM_BOOL_F;
    SCM before_gc_hook = SCM_BOOL_F;
    SCM exit_hook = SCM_BOOL_F;
    SCM features_var = SCM_BOOL_F;
    SCM load_path_var = SCM_BOOL_F;
    SCM load_extensions_var = SCM_BOOL_F;
    SCM load_hook_var = SCM_BOOL_F;
    SCM load_verbosely_var = SCM_BOOL_F;
    SCM random_state_var = SCM_BOOL_F;
    SCM program_args_ = SCM_EOL;
    SCM gensym_counter_val = SCM::from_fixnum(0);
    uint64_t gensym_counter = 0;
    bool include_deprecated = true;
    bool warn_deprecated = false;

    // Cached common symbols
    SCM sym_quote = SCM_BOOL_F;
    SCM sym_quasiquote = SCM_BOOL_F;
    SCM sym_unquote = SCM_BOOL_F;
    SCM sym_unquote_splicing = SCM_BOOL_F;
    SCM sym_syntax = SCM_BOOL_F;
    SCM sym_quasisyntax = SCM_BOOL_F;
    SCM sym_unsyntax = SCM_BOOL_F;
    SCM sym_unsyntax_splicing = SCM_BOOL_F;
    SCM sym_lambda = SCM_BOOL_F;
    SCM sym_if = SCM_BOOL_F;
    SCM sym_set_x = SCM_BOOL_F;
    SCM sym_define = SCM_BOOL_F;
    SCM sym_begin = SCM_BOOL_F;
    SCM sym_let = SCM_BOOL_F;
    SCM sym_let_star = SCM_BOOL_F;
    SCM sym_letrec = SCM_BOOL_F;
    SCM sym_cond = SCM_BOOL_F;
    SCM sym_else = SCM_BOOL_F;
    SCM sym_arrow = SCM_BOOL_F;
    SCM sym_case = SCM_BOOL_F;
    SCM sym_do = SCM_BOOL_F;
    SCM sym_and = SCM_BOOL_F;
    SCM sym_or = SCM_BOOL_F;
    SCM sym_delay = SCM_BOOL_F;
    SCM sym_at = SCM_BOOL_F;
    SCM sym_atat = SCM_BOOL_F;
    SCM sym_the_environment = SCM_BOOL_F;
    SCM sym_eval_when = SCM_BOOL_F;
    SCM sym_misc_error = SCM_BOOL_F;
    SCM sym_wrong_type_arg = SCM_BOOL_F;
    SCM sym_wrong_num_args = SCM_BOOL_F;
    SCM sym_out_of_range = SCM_BOOL_F;
    SCM sym_system_error = SCM_BOOL_F;
    SCM sym_numerical_overflow = SCM_BOOL_F;
    SCM sym_read_error = SCM_BOOL_F;
    SCM sym_unbound_variable = SCM_BOOL_F;
    SCM sym_syntax_error = SCM_BOOL_F;
    SCM sym_goops_error = SCM_BOOL_F;
    SCM sym_quit = SCM_BOOL_F;
    SCM sym_name = SCM_BOOL_F;
    SCM sym_documentation = SCM_BOOL_F;
    SCM sym_source = SCM_BOOL_F;
    SCM sym_filename = SCM_BOOL_F;
    SCM sym_line = SCM_BOOL_F;
    SCM sym_column = SCM_BOOL_F;
    SCM sym_module = SCM_BOOL_F;
    SCM sym_system_module = SCM_BOOL_F;
    SCM sym_prefix = SCM_BOOL_F;
    SCM sym_postfix = SCM_BOOL_F;

    // GOOPS core cached classes/symbols
    bool goops_loaded = false;
    SCM metaclass_standard = SCM_BOOL_F;
    SCM metaclass_operator = SCM_BOOL_F;
    SCM goops_class_top = SCM_BOOL_F;
    SCM goops_class_object = SCM_BOOL_F;
    SCM goops_class_class = SCM_BOOL_F;
    SCM goops_class_applicable = SCM_BOOL_F;
    SCM goops_class_entity_class = SCM_BOOL_F;
    SCM goops_class_entity = SCM_BOOL_F;
    SCM goops_class_entity_with_setter = SCM_BOOL_F;
    SCM goops_class_generic = SCM_BOOL_F;
    SCM goops_class_generic_with_setter = SCM_BOOL_F;
    SCM goops_class_method = SCM_BOOL_F;
    SCM goops_class_simple_method = SCM_BOOL_F;
    SCM goops_class_accessor_method = SCM_BOOL_F;
    SCM goops_class_accessor = SCM_BOOL_F;
    SCM goops_class_procedure_class = SCM_BOOL_F;
    SCM goops_class_operator_class = SCM_BOOL_F;
    SCM goops_class_operator_with_setter_class = SCM_BOOL_F;
    SCM goops_class_extended_generic = SCM_BOOL_F;
    SCM goops_class_extended_generic_with_setter = SCM_BOOL_F;
    SCM goops_class_extended_accessor = SCM_BOOL_F;
    SCM goops_class_foreign_class = SCM_BOOL_F;
    SCM goops_class_foreign_object = SCM_BOOL_F;
    SCM goops_class_foreign_slot = SCM_BOOL_F;
    SCM goops_class_self = SCM_BOOL_F;
    SCM goops_class_protected = SCM_BOOL_F;
    SCM goops_class_opaque = SCM_BOOL_F;
    SCM goops_class_read_only = SCM_BOOL_F;
    SCM goops_class_protected_opaque = SCM_BOOL_F;
    SCM goops_class_protected_read_only = SCM_BOOL_F;
    SCM goops_class_scm = SCM_BOOL_F;
    SCM goops_class_int = SCM_BOOL_F;
    SCM goops_class_float = SCM_BOOL_F;
    SCM goops_class_double = SCM_BOOL_F;
    SCM goops_class_boolean = SCM_BOOL_F;
    SCM goops_class_char = SCM_BOOL_F;
    SCM goops_class_list = SCM_BOOL_F;
    SCM goops_class_pair = SCM_BOOL_F;
    SCM goops_class_null = SCM_BOOL_F;
    SCM goops_class_string = SCM_BOOL_F;
    SCM goops_class_symbol = SCM_BOOL_F;
    SCM goops_class_vector = SCM_BOOL_F;
    SCM goops_class_number = SCM_BOOL_F;
    SCM goops_class_complex = SCM_BOOL_F;
    SCM goops_class_real = SCM_BOOL_F;
    SCM goops_class_integer = SCM_BOOL_F;
    SCM goops_class_fraction = SCM_BOOL_F;
    SCM goops_class_keyword = SCM_BOOL_F;
    SCM goops_class_procedure = SCM_BOOL_F;
    SCM goops_class_procedure_with_setter = SCM_BOOL_F;
    SCM goops_class_primitive_generic = SCM_BOOL_F;
    SCM goops_class_port = SCM_BOOL_F;
    SCM goops_class_input_port = SCM_BOOL_F;
    SCM goops_class_output_port = SCM_BOOL_F;
    SCM goops_class_input_output_port = SCM_BOOL_F;
    SCM goops_class_hashtable = SCM_BOOL_F;
    SCM goops_class_fluid = SCM_BOOL_F;
    SCM goops_class_unknown = SCM_BOOL_F;
    SCM goops_class_uvec = SCM_BOOL_F;
    SCM goops_class_array = SCM_BOOL_F;
    SCM goops_no_applicable_method = SCM_BOOL_F;
    SCM goops_module = SCM_BOOL_F;
    SCM goops_lookup_closure = SCM_BOOL_F;
    SCM goops_components = SCM_BOOL_F;
    SCM goops_list_of_no_method = SCM_BOOL_F;
    SCM goops_hell_mutex = SCM_BOOL_F;

    // Primitive-generic subr handles
    SCM subr_eqv_p = SCM_BOOL_F;
    SCM subr_equal_p = SCM_BOOL_F;
    SCM subr_write = SCM_BOOL_F;
    SCM subr_display = SCM_BOOL_F;
    SCM subr_num_eq = SCM_BOOL_F;
    SCM subr_num_lt = SCM_BOOL_F;
    SCM subr_num_le = SCM_BOOL_F;
    SCM subr_num_gt = SCM_BOOL_F;
    SCM subr_num_ge = SCM_BOOL_F;
    SCM subr_add = SCM_BOOL_F;
    SCM subr_sub = SCM_BOOL_F;
    SCM subr_mul = SCM_BOOL_F;
    SCM subr_div = SCM_BOOL_F;

    // Fast built-in flags for bytecode inline opcodes
    static constexpr uint8_t VAR_FLAG_FAST_ARITH  = 0x01;
    static constexpr uint8_t VAR_FLAG_FAST_CORE   = 0x02;
    static constexpr uint8_t VAR_FLAG_FAST_CALLCC = 0x04;
    static constexpr uint8_t VAR_FLAG_JIT_GUARD   = 0x08;
    bool fast_arith_ok = true;
    bool fast_core_ok = true;
    bool fast_callcc_ok = true;
    bool jit_enabled = false; // TeXmacs: no JIT (see ../README)
    uint64_t jit_epoch = 1;
    uint64_t module_epoch = 1;
    uint64_t mod_cache_epoch = 1;
    uint32_t mod_visit_stamp = 0;
    int mod_search_depth = 0;
    struct ModVarCacheEntry {
        uint64_t mod_bits = 0;
        uint64_t sym_bits = 0;
        uint64_t var_bits = 0;
        uint64_t mod_epoch = 0;
        uint64_t gc_epoch = 0;
        uint32_t sym_epoch = 0;
    };
    static constexpr size_t MOD_VAR_CACHE_SIZE = 16384;
    ModVarCacheEntry mod_var_cache[MOD_VAR_CACHE_SIZE];
    int bc_vm_depth = 0;
    int next_bc_inv_id = 0;
    std::vector<int> active_bc_invs;

    bool is_bc_inv_active(int inv_id) const noexcept {
        for (int id : active_bc_invs) {
            if (id == inv_id) return true;
        }
        return false;
    }

    inline void notify_var_mutated(VariableObj* var) noexcept {
        if (var->flags & VAR_FLAG_FAST_ARITH)  fast_arith_ok = false;
        if (var->flags & VAR_FLAG_FAST_CORE)   fast_core_ok = false;
        if (var->flags & VAR_FLAG_FAST_CALLCC) fast_callcc_ok = false;
        if (var->flags & VAR_FLAG_JIT_GUARD)   ++jit_epoch;
    }

    // Fast builtin VariableObj handles (populated after subsystem init)
    hashmap<SCM, int> fast_builtin_vars{-1}; // VariableObj -> FastOp tag (-1: none)
    hashmap<SCM, SCM> goops_cmethod_closures{SCM_BOOL_F}; // cmethod -> ClosureObj (cleared on GC)
    void init_bytecode_builtins();

    std::vector<SCM> permanent_roots;
    SCM permanent_object(SCM x) { permanent_roots.push_back(x); return x; }

    hashmap<string, SCM> symbol_table{SCM_BOOL_F};
    array<SCM> interned_symbols;  // the values of symbol_table, for the collector
    hashmap<string, pointer> extensions{(pointer) nullptr};  // (void (*)())
    std::vector<SmobTypeInfo> smob_types_;

    // Subsystem initializers
    void init_symbols();
    void init_eq();
    void init_list();
    void init_vectors();
    void init_chars();
    void init_strings();
    void init_srfi13_14();
    void init_numbers();
    void init_hashtab();
    void init_properties();
    void init_ports();
    void init_read();
    void init_print();
    void init_struct();
    void init_fluids();
    void init_modules();
    void init_eval();
    void init_load();
    void init_options();
    void init_posix();
    void init_stime();
    void init_regex();
    void init_socket();
    void init_arrays();
    void init_sort();
    void init_random();
    void init_srfi1_60();
    void init_goops();
    void init_environments();
    void init_misc();
    void init_error();

private:
    Heap heap_;
    bool initialized_ = false;
};

// Global thread-local / singleton VM accessor (TeXmacs: inline, as it is
// called everywhere; vm_create makes the VM at the first call)
extern VM* g_vm;
VM& vm_create();
inline VM& vm() {
    if (GCPP_UNLIKELY(g_vm == nullptr)) return vm_create();
    return *g_vm;
}

// ============================================================================
// Shared cross-module helper declarations
// ============================================================================

// Equality (eq.cpp)
bool scm_eqv(SCM a, SCM b);
bool scm_equal(SCM a, SCM b);

// Hashing (hashtab.cpp)
uint64_t scm_hash_string(std::string_view s);
uint64_t scm_ihashq(SCM obj, uint64_t n);
uint64_t scm_ihashv(SCM obj, uint64_t n);
uint64_t scm_ihash(SCM obj, uint64_t n);
SCM hash_ref(SCM table, SCM key, SCM dflt, int mode); // mode: 0=equal, 1=eq, 2=eqv
SCM hash_set(SCM table, SCM key, SCM val, int mode);
SCM hash_remove(SCM table, SCM key, int mode);
SCM hash_create_handle(SCM table, SCM key, SCM init, int mode);
SCM hash_get_handle(SCM table, SCM key, int mode);

// Numbers (numbers.cpp)
bool is_real_number(SCM x);
bool is_rational_number(SCM x);
bool is_integer_number(SCM x);
bool is_exact(SCM x);
bool is_inexact(SCM x);
double scm_to_double(SCM x, const char* subr = "number");
int64_t scm_to_int64(SCM x, const char* subr = "integer", int argpos = 1);
uint64_t scm_to_uint64(SCM x, const char* subr = "integer", int argpos = 1);
BigInt scm_to_bigint(SCM x, const char* subr = "integer", int argpos = 1);
BigIntView scm_to_bigint_view(SCM x, uint64_t& scratch_limb, const char* subr = "integer", int argpos = 1);
SCM scm_from_bigint(BigInt&& z);
SCM scm_from_bigint(const BigInt& z);
SCM scm_num_add(SCM a, SCM b);
SCM scm_num_sub(SCM a, SCM b);
SCM scm_num_mul(SCM a, SCM b);
SCM scm_num_div(SCM a, SCM b);
int scm_num_cmp(SCM a, SCM b); // <0, 0, >0 (for real numbers)
bool scm_num_eq(SCM a, SCM b);
SCM scm_string_to_number(std::string_view s, int radix = 10);
std::string scm_number_to_string(SCM n, int radix = 10);
SCM scm_exact_to_inexact(SCM n);
SCM scm_inexact_to_exact(SCM n);
SCM scm_quotient(SCM a, SCM b);
SCM scm_remainder(SCM a, SCM b);
SCM scm_modulo(SCM a, SCM b);
SCM scm_gcd(SCM a, SCM b);
SCM scm_lcm(SCM a, SCM b);
SCM scm_ash(SCM n, SCM count);
SCM scm_logand(SCM a, SCM b);
SCM scm_logior(SCM a, SCM b);
SCM scm_logxor(SCM a, SCM b);
SCM scm_lognot(SCM a);

// Lists (list.cpp)
int64_t scm_ilength(SCM lst); // -1 if improper or circular
bool is_proper_list(SCM lst);
SCM scm_reverse(SCM lst);
SCM scm_copy_tree(SCM obj);
SCM scm_reverse_x(SCM lst, SCM newtail = SCM_EOL);
SCM scm_append2(SCM a, SCM b);
SCM scm_list_copy(SCM lst);
SCM scm_assq(SCM key, SCM alist);
SCM scm_assv(SCM key, SCM alist);
SCM scm_assoc(SCM key, SCM alist);
SCM scm_memq(SCM key, SCM lst);

// Ports (ports.cpp)
int port_getc(PortObj* p);
inline int port_getc_fast(PortObj* p) {
    if (GCPP_LIKELY(p && !p->closed && p->is_input && p->unread_buf.empty())) {
        if (p->kind == PortKind::File) {
            if (GCPP_LIKELY(p->read_pos < p->read_buf.size())) {
                int c = static_cast<unsigned char>(p->read_buf[p->read_pos++]);
                if (c == '\n') { ++p->line; p->column = 0; }
                else if (c == '\t') { p->column = (p->column + 8) & ~7L; }
                else { ++p->column; }
                return c;
            }
        } else if (p->kind == PortKind::String) {
            if (GCPP_LIKELY(p->str_pos < p->str_buf.size())) {
                int c = static_cast<unsigned char>(p->str_buf[p->str_pos++]);
                if (c == '\n') { ++p->line; p->column = 0; }
                else if (c == '\t') { p->column = (p->column + 8) & ~7L; }
                else { ++p->column; }
                return c;
            }
        }
    }
    return port_getc(p);
}
int port_peekc(PortObj* p);
void port_ungetc(PortObj* p, int c);
inline int port_peekc_fast(PortObj* p) {
    if (GCPP_LIKELY(p && !p->closed && p->is_input)) {
        if (!p->unread_buf.empty()) return static_cast<unsigned char>(p->unread_buf.back());
        if (GCPP_LIKELY(p->kind == PortKind::File && p->read_pos < p->read_buf.size())) {
            return static_cast<unsigned char>(p->read_buf[p->read_pos]);
        }
        if (GCPP_LIKELY(p->kind == PortKind::String && p->str_pos < p->str_buf.size())) {
            return static_cast<unsigned char>(p->str_buf[p->str_pos]);
        }
    }
    return port_peekc(p);
}
inline void port_ungetc_fast(PortObj* p, int c) {
    if (c == EOF || !p) return;
    if (GCPP_LIKELY(p->unread_buf.empty() && c != '\n' && c != '\t' && p->column > 0)) {
        if (GCPP_LIKELY(p->kind == PortKind::File && p->read_pos > 0 &&
            static_cast<unsigned char>(p->read_buf[p->read_pos - 1]) == static_cast<unsigned char>(c))) {
            --p->read_pos;
            --p->column;
            return;
        }
        if (GCPP_LIKELY(p->kind == PortKind::String && p->str_pos > 0 &&
            static_cast<unsigned char>(p->str_buf[p->str_pos - 1]) == static_cast<unsigned char>(c))) {
            --p->str_pos;
            --p->column;
            return;
        }
    }
    port_ungetc(p, c);
}
void port_ungets(PortObj* p, std::string_view s);
void port_putc(PortObj* p, char c);
void port_puts(PortObj* p, std::string_view s);
void port_flush(PortObj* p);
void port_close(PortObj* p);
SCM make_file_port(int fd, std::string_view filename, std::string_view mode, bool owns_fd = true);
SCM make_input_string_port(std::string s);
SCM make_output_string_port();
void flush_all_ports();
void simple_format_to_port(SCM port, std::string_view fmt, SCM args);

// Structs (struct.cpp)
bool is_struct_vtable(SCM x);
SCM struct_layout(SCM s);
SCM struct_vtable(SCM s);
SCM struct_ref(SCM s, size_t idx);
void struct_set(SCM s, size_t idx, SCM val);
SCM make_struct_impl(SCM vtable, size_t tail_len, SCM init_list);
SCM make_vtable_vtable_impl(SCM extra_fields, size_t tail_len, SCM init_list);
SCM struct_vtable_name(SCM vt);
void set_struct_vtable_name(SCM vt, SCM name);
SCM struct_create_handle(SCM vt);
bool struct_equal(SCM a, SCM b);

// Arrays & Uniform Vectors (arrays.cpp)
bool is_any_array(SCM x);
bool array_equal(SCM a, SCM b);
SCM uniform_vector_ref(UniformVectorObj* uv, size_t i);
void uniform_vector_set(UniformVectorObj* uv, size_t i, SCM val);
const char* uniform_type_tag(UniformType ut);
SCM read_uniform_vector_body(UniformType ut, SCM list_val);

// Chars (chars.cpp)
const char* char_to_name(uint32_t c);
int name_to_char(std::string_view name);

// Strings (strings.cpp)
SCM make_shared_substring(SCM str, size_t start, size_t end, bool read_only = false);

// GOOPS (goops.cpp)
SCM goops_class_of(SCM obj);
SCM goops_apply_generic(SCM gf, const SCM* args, size_t nargs);

// Eval/Stack helpers (eval.cpp)
SCM build_stack_from_cont(SCM cont, SCM id = SCM_BOOL_F);
void display_backtrace_impl(SCM stack, SCM port, int first = 0, int depth = -1);
void display_error_impl(SCM stack, SCM port, SCM subr, SCM message, SCM args, SCM rest);
void switch_dynwind_pub(SCM target_dw);

// Bytecode Compiler & VM (bytecode.cpp)
bool bytecode_should_compile(ClosureObj* cl) noexcept;
// TeXmacs: the directories of the Scheme library (ice-9...), before VM::init
void set_data_dirs(const char* library, const char* pkgdata, const char* site);

bool bytecode_compile_closure(ClosureObj* cl);
SCM bytecode_run_closure(ClosureObj* cl, const SCM* args, size_t nargs);
SCM bytecode_resume_continuation(ContinuationObj* k, SCM ret_val);
void bytecode_mark_roots(const Marker& mark);

// Native ARM64 JIT Compiler (jit_arm64.cpp, not part of the TeXmacs build:
// GUILE_CPP_JIT is never defined there, see ../README)
#ifdef GUILE_CPP_JIT
inline constexpr uint64_t JIT_BAILOUT = 1ULL;
bool jit_try_compile(ClosureObj* cl, BytecodeObj* bc);
uint64_t jit_invoke(ClosureObj* cl, BytecodeObj* bc, const SCM* args, size_t nargs);
#endif

} // namespace guile
