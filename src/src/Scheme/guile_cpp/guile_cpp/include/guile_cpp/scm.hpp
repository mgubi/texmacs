#pragma once

#include <cstdint>
#include <cstddef>
#include <cstdio>
#include <cstring>
#include "hashmap.hpp"
#include "hashset.hpp"
#include "iterator.hpp"
#include <type_traits>
#include "basic.hpp"
#include "guile_cpp/tm_string.hpp"
#include <regex.h>
#include <dirent.h>
#include "guile_cpp/bigint.hpp"
#include "guile_cpp/value.hpp"

namespace guile {

// the tagged values, the objects header and the pairs are in value.hpp

struct SymbolObj : Obj {
    string name;  // (never changed: shared with the key of the symbol table)
    uint64_t hash = 0;
    uint32_t def_epoch = 0;
    bool interned = true;
    SCM props = SCM_EOL;
    SCM fref = SCM_BOOL_F;
    SCM pref = SCM_EOL;
    SCM keyword = SCM_BOOL_F;
    SymbolObj(string n, uint64_t h, bool intern = true)
        : Obj(ObjTag::Symbol), name(n), hash(h), interned(intern) {}
};

struct KeywordObj : Obj {
    SCM symbol;
    explicit KeywordObj(SCM sym) noexcept : Obj(ObjTag::Keyword), symbol(sym) {}
};

// TeXmacs: the characters of a string are a TeXmacs string, which the
// shared substrings (make-shared-substring) share with their string: a
// change of one (string-set!...) is seen by the other, as in Guile. The
// string given to the constructor must be owned by the new object (a fresh
// string, or a copy): VM::make_string copies, VM::make_string_owned not.
struct StringObj : Obj {
    string buf;
    bool buf_read_only = false;  // (a literal: also for its substrings)
    size_t start = 0;
    size_t len = 0;
    bool read_only = false;

    explicit StringObj(string s, bool ro = false)
        : Obj(ObjTag::String), buf(s), buf_read_only(ro),
          start(0), len(static_cast<size_t>(N(s))), read_only(ro) {}

    StringObj(string b, bool b_ro, size_t st, size_t l, bool ro = false)
        : Obj(ObjTag::String), buf(b), buf_read_only(b_ro), start(st), len(l), read_only(ro) {}

    // the characters, not 0-terminated ("" for the empty string)
    const char* chars() const noexcept {
        return len == 0 ? "" : &const_cast<string&>(buf)[static_cast<int>(start)];
    }
    // a fresh copy of the characters
    string str() const {
        return const_cast<string&>(buf)(static_cast<int>(start), static_cast<int>(start + len));
    }
    // a copy of the n characters from pos
    string sub(size_t pos, size_t n = static_cast<size_t>(-1)) const {
        return view_sub(chars(), len, pos, n);
    }
    // the same characters
    friend bool string_obj_equal(const StringObj* a, const StringObj* b) {
        return a->len == b->len && std::memcmp(a->chars(), b->chars(), a->len) == 0;
    }
    char* mutable_data() {
        return &buf[static_cast<int>(start)];
    }
};

struct VectorObj : Obj {
    array<SCM> elts;
    explicit VectorObj(size_t n, SCM fill = SCM_UNSPECIFIED)
        : Obj(ObjTag::Vector), elts(array_fill(n, fill)) {}
    explicit VectorObj(array<SCM> v)
        : Obj(ObjTag::Vector), elts(std::move(v)) {}
};

enum class WeakKind : uint8_t {
    None = 0,
    Vector = 1,
    KeyAlist = 2,
    ValueAlist = 3,
    DoublyWeakAlist = 4,
};

struct WeakVectorObj : Obj {
    array<SCM> elts;
    WeakKind kind = WeakKind::Vector;
    WeakVectorObj(size_t n, SCM fill, WeakKind k = WeakKind::Vector)
        : Obj(ObjTag::WeakVector), elts(array_fill(n, fill)), kind(k) {}
};

struct BignumObj : Obj {
    BigInt val;
    BignumObj() : Obj(ObjTag::Bignum) {}
    explicit BignumObj(int64_t v) : Obj(ObjTag::Bignum), val(v) {}
    explicit BignumObj(uint64_t v) : Obj(ObjTag::Bignum), val(v) {}
    explicit BignumObj(const BigInt& v) : Obj(ObjTag::Bignum), val(v) {}
    explicit BignumObj(BigInt&& v) noexcept : Obj(ObjTag::Bignum), val(std::move(v)) {}
};

struct FractionObj : Obj {
    SCM num;
    SCM den;
    FractionObj(SCM n, SCM d) noexcept : Obj(ObjTag::Fraction), num(n), den(d) {}
};

struct RealObj : Obj {
    double val;
    explicit RealObj(double v) noexcept : Obj(ObjTag::Real), val(v) {}
};

struct ComplexObj : Obj {
    double real;
    double imag;
    ComplexObj(double r, double i) noexcept : Obj(ObjTag::Complex), real(r), imag(i) {}
};

// C++ primitive procedure. TeXmacs: in place of std::function, a plain
// function pointer, or for a lambda which captures (most capture the VM),
// a copy of it on the heap, shared by reference counting, called through a
// function pointer
struct SubrClosure {
    int ref_count;
    SCM (*call)(SubrClosure* c, const SCM* args, size_t nargs);
    void (*destroy)(SubrClosure* c);
};

template <typename F>
struct SubrClosureOf : SubrClosure {
    F f;
    explicit SubrClosureOf(const F& g) : f(g) {
        ref_count = 1;
        call = &SubrClosureOf::call_it;
        destroy = &SubrClosureOf::destroy_it;
    }
    static SCM call_it(SubrClosure* c, const SCM* args, size_t nargs) {
        return static_cast<SubrClosureOf*>(c)->f(args, nargs);
    }
    static void destroy_it(SubrClosure* c) {
        tm_delete(static_cast<SubrClosureOf*>(c));
    }
};

class SubrFn {
    RawSubrFn raw_ = nullptr;
    SubrClosure* clo_ = nullptr;
    void release() noexcept {
        if (clo_ && --clo_->ref_count == 0) clo_->destroy(clo_);
        clo_ = nullptr;
    }
public:
    SubrFn() noexcept {}
    SubrFn(RawSubrFn f) noexcept : raw_(f) {}
    template <typename F,
              typename = typename std::enable_if<
                  !std::is_same<typename std::decay<F>::type, SubrFn>::value>::type>
    SubrFn(const F& f) {
        if constexpr (std::is_convertible<F, RawSubrFn>::value) raw_ = f;
        else clo_ = tm_new<SubrClosureOf<F>>(f);
    }
    SubrFn(const SubrFn& o) noexcept : raw_(o.raw_), clo_(o.clo_) {
        if (clo_) clo_->ref_count++;
    }
    SubrFn& operator=(const SubrFn& o) noexcept {
        if (o.clo_) o.clo_->ref_count++;
        release();
        raw_ = o.raw_; clo_ = o.clo_;
        return *this;
    }
    ~SubrFn() { release(); }
    explicit operator bool() const noexcept { return raw_ != nullptr || clo_ != nullptr; }
    SCM operator()(const SCM* args, size_t nargs) const {
        return raw_ ? raw_(args, nargs) : clo_->call(clo_, args, nargs);
    }
};

// TeXmacs: the callback of the collector (in place of
// std::function<void(SCM)>): a function pointer and its data
struct Marker {
    void (*fn)(void* data, SCM x);
    void* data;
    void operator()(SCM x) const { fn(data, x); }
};

enum class SpecialSubr : uint8_t {
    None = 0,
    Apply,
    CallCC,
    CallWithValues,
    DynamicWind,
    Catch,
    LazyCatch,
    WithThrowHandler,
    WithFluidsStar,
    WithFluidStar,
    WithDynamicState,
    Eval,
    PrimitiveEval,
    LocalEval,
    Force,
    Map,
    ForEach,
    AndMap,
    OrMap,
};

struct SubrObj : Obj {
    string name;
    int req = 0;
    int opt = 0;
    bool rst = false;
    bool raw_varargs = false;
    RawSubrFn raw_fn = nullptr;
    SubrFn fn;
    SpecialSubr special = SpecialSubr::None;
    bool generic_capable = false;
    SCM generic = SCM_BOOL_F;
    SCM props = SCM_EOL;

    SubrObj(string n, int r, int o, bool rest, SubrFn f, SpecialSubr sp = SpecialSubr::None)
        : Obj(ObjTag::Subr), name(n), req(r), opt(o), rst(rest), fn(f), special(sp) {}
};

struct BoxObj : Obj {
    SCM val;
    explicit BoxObj(SCM v = SCM_UNDEFINED) noexcept : Obj(ObjTag::Box), val(v) {}
};

struct BytecodeObj : Obj {
    uint16_t arity_req = 0;
    bool has_rest = false;
    bool has_loop = false;
    uint16_t max_locals = 0;
    uint16_t num_upvals = 0;
    SCM eval_closure = SCM_BOOL_F;
    SCM cached_env = SCM_EOL;     // pre-consed (eval_closure) or EOL to avoid PairObj alloc per OP_MAKE_CLOSURE
    SCM source_formals = SCM_EOL;
    SCM source_body = SCM_EOL;
    array<uint32_t> code;
    uint32_t* code_ptr = nullptr;  // (TeXmacs: &code[0], kept by emit, read by the VM at each call)
    array<SCM> consts;
    array<SCM> globals;     // VariableObj* inline cache (or SCM_BOOL_F)
    array<SCM> global_syms; // SymbolObj*
    array<uint32_t> upval_descs; // (is_local << 16) | slot_index
    array<SCM> upval_syms;  // SymbolObj* for procedure-environment reconstruction
    array<uint8_t> upval_kinds; // 0=Val, 1=Box, 2=CellCar, 3=CellCdr
    array<SCM> upval_known_bc;  // BytecodeObj* for immutable all-lambda letrec boxes, else SCM_BOOL_F
    void* jit_code = nullptr;     // Compiled native ARM64 ext_entry pointer (or nullptr)
    void* jit_code_int = nullptr; // Compiled native ARM64 int_entry pointer (or nullptr)
    uint64_t jit_epoch = 0;       // VM::jit_epoch when compiled
    uint32_t jit_calls = 0;       // Hotness counter (0xFFFFFFFFu = declined)
    uint16_t jit_bailouts = 0;    // Consecutive/total non-exception bailouts (deopt if >= 4)
    BytecodeObj() : Obj(ObjTag::Bytecode) {}
};

struct ClosureObj : Obj {
    static constexpr size_t INLINE_UPVAL_CAP = 8;
    // Shared prefix layout with JIT nursery JitClosure:
    //   +0:  Obj::gc_next
    //   +8:  Obj::tag, gc_mark, flags, extra
    //   +16: bytecode
    //   +24: inline_upvals[0..INLINE_UPVAL_CAP-1]
    SCM bytecode = SCM_BOOL_F; // BytecodeObj*, SCM_BOOL_F = uncompiled, SCM_BOOL_T = CEK fallback
    SCM inline_upvals[INLINE_UPVAL_CAP];
    SCM formals;   // e.g. (a b . rest) or sym or ()
    SCM body;      // list of body expressions
    SCM env;       // lexical environment: ((vars . vals) ... . (eval-closure))
    SCM props = SCM_EOL;
    SCM upvals = SCM_BOOL_F;   // VectorObj of captured values/boxes when > INLINE_UPVAL_CAP, else SCM_BOOL_F
    uint8_t inline_upval_cnt = 0;

    ClosureObj(SCM f, SCM b, SCM e) noexcept
        : Obj(ObjTag::Closure), formals(f), body(b), env(e) {}
};

enum class MacroKind : uint8_t {
    Syntax = 0,
    Macro = 1,
    MemoizingMacro = 2,
    BuiltinMacro = 3,
};

struct MacroObj : Obj {
    MacroKind kind;
    SCM transformer;
    SCM name = SCM_BOOL_F;

    MacroObj(MacroKind k, SCM t, SCM n = SCM_BOOL_F) noexcept
        : Obj(ObjTag::Macro), kind(k), transformer(t), name(n) {}
};

struct VariableObj : Obj {
    SCM value;
    explicit VariableObj(SCM v = SCM_UNDEFINED) noexcept
        : Obj(ObjTag::Variable), value(v) {}
};

enum class PortKind : uint8_t {
    File = 0,
    String = 1,
    Soft = 2,
    Pipe = 3,
};

struct PortObj : Obj {
    PortKind kind = PortKind::File;
    bool is_input = false;
    bool is_output = false;
    bool closed = false;
    bool owns_fd = true;
    int fd = -1;
    FILE* pipe_fp = nullptr;
    pid_t pipe_pid = -1;
    string filename;
    SCM filename_scm = SCM_BOOL_F;
    string mode_str;
    long line = 0;
    long column = 0;
    int revealed = 0;
    // String port buffer
    string str_buf;
    size_t str_pos = 0;
    // Soft port vector #(write-char write-string flush read-char close)
    SCM soft_vec = SCM_BOOL_F;
    // Unread buffer (LIFO stack)
    string unread_buf;
    // Read buffer for file/pipe ports
    string read_buf;
    size_t read_pos = 0;
    // Write buffer for file/pipe ports
    string write_buf;
    int buf_mode = _IOFBF; // _IONBF, _IOLBF, _IOFBF

    PortObj() : Obj(ObjTag::Port) {}
    ~PortObj();
};

struct FluidObj : Obj {
    size_t id = 0;
    SCM default_val = SCM_BOOL_F;
    explicit FluidObj(size_t i, SCM def = SCM_BOOL_F) noexcept
        : Obj(ObjTag::Fluid), id(i), default_val(def) {}
};

struct DynamicStateObj : Obj {
    array<SCM> values;
    DynamicStateObj() : Obj(ObjTag::DynamicState) {}
};

struct HashTableObj : Obj {
    array<SCM> buckets; // each bucket is an alist ((k . v) ...)
    size_t count = 0;
    WeakKind weak = WeakKind::None;
    bool is_module_obarray = false;

    HashTableObj(size_t nbuckets, WeakKind w = WeakKind::None)
        : Obj(ObjTag::HashTable), buckets(array_fill<SCM>(nbuckets ? nbuckets : 31, SCM_EOL)), count(0), weak(w) {}
};

// Guile 1.8.8 Structs & Vtables
constexpr int VTABLE_INDEX_LAYOUT  = 0;
constexpr int VTABLE_INDEX_VTABLE  = 1;
constexpr int VTABLE_INDEX_PRINTER = 2;
constexpr int VTABLE_OFFSET_USER   = 3;

// GOOPS / objects.h class flags stored in StructObj::vtable_flags
constexpr uint32_t CLASSF_ENTITY          = 1u << 0;
constexpr uint32_t CLASSF_OPERATOR        = 1u << 1;
constexpr uint32_t CLASSF_SIMPLE_METHOD   = 1u << 2;
constexpr uint32_t CLASSF_ACCESSOR_METHOD = 1u << 3;
constexpr uint32_t CLASSF_PURE_GENERIC    = 1u << 4;
constexpr uint32_t CLASSF_FOREIGN         = 1u << 5;
constexpr uint32_t CLASSF_METACLASS       = 1u << 6;
constexpr uint32_t CLASSF_GOOPS_VALID     = 1u << 7;
constexpr uint32_t CLASSF_GOOPS           = 1u << 8;
constexpr uint32_t CLASSF_GOOPS_OR_VALID  = CLASSF_GOOPS | CLASSF_GOOPS_VALID;
constexpr uint32_t CLASSF_INHERIT         = ~(CLASSF_PURE_GENERIC | CLASSF_SIMPLE_METHOD | CLASSF_ACCESSOR_METHOD);

struct StructObj : Obj {
    SCM vtable = SCM_BOOL_F;      // StructObj* (or self for vtable-vtable)
    array<uint64_t> slots;  // raw words ('p'/'s' store SCM::bits, 'u' stores uint64_t)
    size_t tail_len = 0;          // length of tail array if layout has tail
    size_t tail_start = 0;        // index of tail length word (if any)
    bool has_tail = false;
    bool is_entity = false;
    bool is_operator = false;
    SCM entity_proc = SCM_BOOL_F;
    SCM entity_setter = SCM_BOOL_F;
    uint32_t vtable_flags = 0;
    uint32_t visit_stamp = 0;
    uint64_t data_id = 0;         // identity of the underlying slot block (swapped by %modify-instance)
    // Monomorphic inline cache for GOOPS generic functions (auto-invalidates when entity_proc changes)
    SCM gf_cache_tag  = SCM_BOOL_F;
    SCM gf_cache_cls0 = SCM_BOOL_F;
    SCM gf_cache_cls1 = SCM_UNDEFINED;
    SCM gf_cache_clo  = SCM_BOOL_F;

    StructObj() : Obj(ObjTag::Struct) {}
};

// Dynamic-wind / fluid / catch chain node (persistent singly-linked list for O(1) call/cc!)
enum class DynWindKind : uint8_t {
    Wind = 0,       // (before . after) thunks
    Fluid = 1,      // fluid binding save/restore
    DynState = 2,   // whole dynamic state save/restore
    Catch = 3,      // catch handler (key, handler, pre_unwind)
    LazyCatch = 4,  // lazy-catch handler (key, handler)
    ThrowHandler = 5, // with-throw-handler (key, handler)
    FlagReset = 6,  // resets running flag bit on a (DynWindObj) when unwound
};

struct DynWindObj : Obj {
    DynWindKind kind;
    SCM a = SCM_BOOL_F; // before / fluid / dynstate / key
    SCM b = SCM_BOOL_F; // after  / saved_val / saved_state / handler
    SCM c = SCM_BOOL_F; // pre_unwind_handler / target_cont
    SCM d = SCM_BOOL_F; // extra (for catch: saved cont frame!)
    SCM parent = SCM_EOL;
    uint64_t id = 0;

    DynWindObj(DynWindKind k, SCM a_, SCM b_, SCM c_, SCM d_, SCM par, uint64_t id_) noexcept
        : Obj(ObjTag::DynWind), kind(k), a(a_), b(b_), c(c_), d(d_), parent(par), id(id_) {}
};

// CEK Machine Continuation Frame
enum class ContKind : uint16_t {
    Halt = 0,
    If,              // expr=alt, env, aux=conseq
    Begin,           // expr=rest_forms, env
    SetVar,          // expr=sym, env, aux=var_or_false
    Define,          // expr=sym, env, aux=var_or_false
    AppOp,           // expr=arg_exprs, env, aux=orig_form
    AppArgs,         // expr=rem_arg_exprs, env, aux=proc, vals=evaled_args
    LetInit,         // expr=rem_bindings, env, aux=body, aux2=names_rev, vals=evaled_vals, aux3=loop_name_or_false
    LetStarInit,     // expr=rem_bindings, env, aux=body, aux2=curr_var
    LetrecInit,      // expr=rem_bindings, env, aux=body, aux2=frame_pair, aux3=curr_cell
    And,             // expr=rem_exprs, env
    Or,              // expr=rem_exprs, env
    CondClause,      // expr=curr_clause, env, aux=rem_clauses
    CondArrow,       // expr=test_val, env
    CaseKey,         // expr=clauses, env
    DoInit,          // expr=rem_vars, env, aux=do_spec, vals=evaled_inits
    DoTest,          // expr=do_spec, env
    DoStep,          // expr=rem_step_exprs, env, aux=do_spec, vals=evaled_steps
    Quasiquote,      // expr=rem_qq, env, aux=mode, aux2=depth
    MacroExpand,     // expr=orig_form, env, aux=macro_obj
    CallWithValues,  // expr=consumer, env
    DynWindEnter,    // expr=thunk, aux=after, aux2=before, aux3=dw_node
    DynWindExit,     // expr=after, aux=saved_result
    DynWindRewind,   // vals=rewind_list, aux=target_cont, aux2=target_dw, aux3=return_val
    CatchPop,        // aux=dw_node
    RethrowAfter,    // aux=key, aux2=throw_args
    PreUnwindAfter,  // aux=dw_node, aux2=key, aux3=throw_args
    FluidPop,        // aux=fluid, aux2=old_val
    DynStatePop,     // aux=old_state
    ForceFinish,     // aux=promise
    MapLoop,         // aux=proc, aux2=rem_lists, vals=results_rev, aux3=mode(0=map,1=foreach,2=andmap,3=ormap)
    LoadLoop,        // aux=port, aux2=old_load_port
};

struct SmallScmVec {
    static constexpr size_t INLINE_CAP = 6;
    SCM inline_buf[INLINE_CAP];
    uint32_t inline_sz = 0;
    array<SCM> heap_buf;

    // (TeXmacs: a copy owns its elements, as with std::vector: the frames
    // of the evaluator copy their values, cf->vals, and then add to them)
    SmallScmVec() noexcept {}
    SmallScmVec(const SmallScmVec& o) : inline_sz(o.inline_sz), heap_buf(copy(o.heap_buf)) {
        for (uint32_t i = 0; i < inline_sz; i++) inline_buf[i] = o.inline_buf[i];
    }
    SmallScmVec& operator=(const SmallScmVec& o) {
        if (this == &o) return *this;
        inline_sz = o.inline_sz;
        for (uint32_t i = 0; i < inline_sz; i++) inline_buf[i] = o.inline_buf[i];
        heap_buf = copy(o.heap_buf);
        return *this;
    }

    size_t size() const noexcept { return (N(heap_buf) == 0) ? inline_sz : static_cast<size_t>(N(heap_buf)); }
    bool empty() const noexcept { return size() == 0; }
    const SCM* data() const noexcept { return (N(heap_buf) == 0) ? inline_buf : gcpp_data(heap_buf); }
    SCM* data() noexcept { return (N(heap_buf) == 0) ? inline_buf : gcpp_data(heap_buf); }
    SCM& operator[](size_t i) noexcept { return data()[i]; }
    SCM operator[](size_t i) const noexcept { return data()[i]; }
    const SCM* begin() const noexcept { return data(); }
    const SCM* end() const noexcept { return data() + size(); }

    void push_back(SCM v) {
        if ((N(heap_buf) == 0)) {
            if (inline_sz < INLINE_CAP) {
                inline_buf[inline_sz++] = v;
                return;
            }
            (void) 0;
            array_append(heap_buf, inline_buf, inline_sz);
            inline_sz = 0;
        }
        (heap_buf << v);
    }

    void assign_from(const SCM* p, size_t n) {
        gcpp_clear(heap_buf);
        if (n <= INLINE_CAP) {
            for (size_t i = 0; i < n; ++i) inline_buf[i] = p[i];
            inline_sz = static_cast<uint32_t>(n);
        } else {
            inline_sz = 0;
            heap_buf = array_from(p, n);
        }
    }
};
// (TeXmacs: SmallScmVec as the arrays, for the code converted from
// std::vector)
inline SmallScmVec& operator<<(SmallScmVec& v, SCM x) { v.push_back(x); return v; }
inline int N(const SmallScmVec& v) { return static_cast<int>(v.size()); }
inline SCM* gcpp_data(SmallScmVec& v) { return v.data(); }
inline const SCM* gcpp_data(const SmallScmVec& v) { return v.data(); }

struct ContFrameObj : Obj {
    ContKind kind;
    SCM next = SCM_EOL;     // parent ContFrameObj or EOL
    SCM expr = SCM_EOL;
    SCM env = SCM_EOL;
    SCM aux = SCM_BOOL_F;
    SCM aux2 = SCM_BOOL_F;
    SCM aux3 = SCM_BOOL_F;
    SmallScmVec vals;

    explicit ContFrameObj(ContKind k, SCM nxt = SCM_EOL) noexcept
        : Obj(ObjTag::ContFrame), kind(k), next(nxt) {}
};

struct VmFrameSnap {
    SCM closure = SCM_BOOL_F;
    SCM bytecode = SCM_BOOL_F;
    uint32_t pc_off = 0;
    uint32_t base_off = 0;
    uint32_t ret_slot = 0;
};

struct ContinuationObj : Obj {
    static constexpr size_t INLINE_STACK_CAP = 8;
    static constexpr size_t INLINE_FRAME_CAP = 4;

    SCM cont_frame;    // saved ContFrameObj
    SCM dynwind_chain; // saved DynWindObj chain
    int cek_depth = -1;
    int vm_depth = -1;
    bool has_vm_snap = false;
    uint16_t inline_stack_len = 0;
    uint16_t inline_frame_len = 0;
    SCM inline_stack[INLINE_STACK_CAP];
    VmFrameSnap inline_frames[INLINE_FRAME_CAP];
    array<SCM> vm_stack_snap;
    array<VmFrameSnap> vm_frames_snap;

    ContinuationObj(SCM cf, SCM dw, int cd = -1) noexcept
        : Obj(ObjTag::Continuation), cont_frame(cf), dynwind_chain(dw), cek_depth(cd) {}

    size_t snap_stack_size() const noexcept {
        return (N(vm_stack_snap) == 0) ? inline_stack_len : static_cast<size_t>(N(vm_stack_snap));
    }
    const SCM* snap_stack_data() const noexcept {
        return (N(vm_stack_snap) == 0) ? inline_stack : gcpp_data(vm_stack_snap);
    }
    size_t snap_frames_size() const noexcept {
        return (N(vm_frames_snap) == 0) ? inline_frame_len : static_cast<size_t>(N(vm_frames_snap));
    }
    const VmFrameSnap* snap_frames_data() const noexcept {
        return (N(vm_frames_snap) == 0) ? inline_frames : gcpp_data(vm_frames_snap);
    }
    void set_stack_snap(const SCM* src, size_t n) {
        if (n <= INLINE_STACK_CAP) {
            for (size_t i = 0; i < n; ++i) inline_stack[i] = src[i];
            inline_stack_len = static_cast<uint16_t>(n);
        } else {
            inline_stack_len = 0;
            vm_stack_snap = array_from(src, n);
        }
    }
    void reserve_frames_snap(size_t n) {
        if (n > INLINE_FRAME_CAP) (void) 0;
    }
    void push_frame_snap(const VmFrameSnap& f) {
        if ((N(vm_frames_snap) == 0)) {
            if (inline_frame_len < INLINE_FRAME_CAP) {
                inline_frames[inline_frame_len++] = f;
                return;
            }
            (void) 0;
            array_append(vm_frames_snap, inline_frames, inline_frame_len);
            inline_frame_len = 0;
        }
        (vm_frames_snap << f);
    }
};

struct PromiseObj : Obj {
    bool forced = false;
    SCM expr_or_val;
    SCM env;
    PromiseObj(SCM e, SCM en) noexcept
        : Obj(ObjTag::Promise), forced(false), expr_or_val(e), env(en) {}
};

struct ValuesObj : Obj {
    SCM values; // proper list of values
    explicit ValuesObj(SCM v) noexcept : Obj(ObjTag::Values), values(v) {}
};

struct HookObj : Obj {
    int arity = 0;
    SCM procs = SCM_EOL;
    explicit HookObj(int a = 0) noexcept : Obj(ObjTag::Hook), arity(a) {}
};

struct GuardianObj : Obj {
    array<SCM> live;
    array<SCM> zombies;  // (TeXmacs: a queue, small)
    bool greedy = false;
    bool destroyed = false;
    explicit GuardianObj(bool g = false) noexcept : Obj(ObjTag::Guardian), greedy(g) {}
};

struct ArbiterObj : Obj {
    SCM name;
    bool locked = false;
    explicit ArbiterObj(SCM n) noexcept : Obj(ObjTag::Arbiter), name(n) {}
};

struct RandomStateObj : Obj {
    uint64_t s[4] = {0, 0, 0, 0}; // xoshiro256** state
    bool has_normal = false;
    double cached_normal = 0.0;
    RandomStateObj() : Obj(ObjTag::RandomState) {}
};

struct RegexpObj : Obj {
    regex_t rx{};
    bool compiled = false;
    string pattern;
    int flags = 0;
    RegexpObj() : Obj(ObjTag::Regexp) {}
    ~RegexpObj() {
        if (compiled) regfree(&rx);
    }
};

struct DirObj : Obj {
    DIR* dir = nullptr;
    bool closed = false;
    DirObj(DIR* d) noexcept : Obj(ObjTag::Dir), dir(d) {}
    ~DirObj() {
        if (dir && !closed) closedir(dir);
    }
};

enum class UniformType : uint8_t {
    U8, S8, U16, S16, U32, S32, U64, S64, F32, F64, C32, C64
};

struct UniformVectorObj : Obj {
    UniformType utype;
    array<uint8_t> bytes;
    size_t length = 0;
    UniformVectorObj(UniformType ut, size_t n);
    size_t elem_size() const noexcept;
};

struct BitVectorObj : Obj {
    array<uint64_t> words;
    size_t length = 0;
    BitVectorObj(size_t n, bool fill = false);
    bool get(size_t i) const noexcept;
    void set(size_t i, bool v) noexcept;
};

struct ArrayDim {
    int64_t lbnd = 0;
    int64_t ubnd = -1;
    int64_t inc = 1;
};

struct ArrayObj : Obj {
    SCM base_vec; // VectorObj, StringObj, BitVectorObj, UniformVectorObj
    int64_t base_offset = 0;
    array<ArrayDim> dims;
    bool contiguous = true;
    ArrayObj() : Obj(ObjTag::Array) {}
};

struct CharSetObj : Obj {
    uint64_t bits[4] = {0, 0, 0, 0}; // 256 bits for 8-bit chars
    CharSetObj() : Obj(ObjTag::CharSet) {}
    bool contains(uint8_t c) const noexcept { return (bits[c >> 6] >> (c & 63)) & 1ULL; }
    void add(uint8_t c) noexcept { bits[c >> 6] |= (1ULL << (c & 63)); }
    void remove(uint8_t c) noexcept { bits[c >> 6] &= ~(1ULL << (c & 63)); }
};

struct ProcWithSetterObj : Obj {
    SCM getter;
    SCM setter;
    ProcWithSetterObj(SCM g, SCM s) noexcept
        : Obj(ObjTag::ProcWithSetter), getter(g), setter(s) {}
};

struct EvalClosureObj : Obj {
    SCM module;
    bool is_interface = false;
    EvalClosureObj(SCM m, bool iface = false) noexcept
        : Obj(ObjTag::EvalClosure), module(m), is_interface(iface) {}
};

struct SmobTypeInfo {
    string name;
    void (*mark)(SCM, const Marker&) = nullptr;
    void (*free_fn)(SCM) = nullptr;
    void (*print)(SCM, SCM) = nullptr;
    SCM (*equalp)(SCM, SCM) = nullptr;
    SubrFn apply;
    uint64_t (*hash_fn)(SCM) = nullptr;
};

struct SmobObj : Obj {
    size_t smob_type = 0;
    uint64_t word1 = 0;
    uint64_t word2 = 0;
    uint64_t word3 = 0;
    void* user_data = nullptr;
    SmobObj(size_t st) noexcept : Obj(ObjTag::Smob), smob_type(st) {}
};

struct FrameObj : Obj {
    SCM proc = SCM_BOOL_F;
    SCM args = SCM_EOL;
    SCM source = SCM_BOOL_F;
    SCM next = SCM_BOOL_F;
    SCM prev = SCM_BOOL_F;
    bool is_proc = false;
    bool is_real = true;
    FrameObj() : Obj(ObjTag::Frame) {}
};

struct StackObj : Obj {
    SCM id = SCM_BOOL_F;
    array<SCM> frames; // vector of FrameObj
    StackObj() : Obj(ObjTag::Stack) {}
};

enum class EnvKind : uint8_t { Leaf = 0, Eval = 1, Import = 2, Export = 3 };

struct ObserverObj : Obj {
    SCM env = SCM_BOOL_F;
    SCM proc = SCM_BOOL_F;
    bool weak = false;
    bool active = true;
    ObserverObj(SCM e, SCM p, bool w) noexcept
        : Obj(ObjTag::Observer), env(e), proc(p), weak(w) {}
};

struct EnvironmentObj : Obj {
    EnvKind env_kind = EnvKind::Leaf;
    SCM table = SCM_BOOL_F;
    SCM local = SCM_BOOL_F;
    SCM imported = SCM_BOOL_F;
    SCM child_obs_1 = SCM_BOOL_F;
    SCM child_obs_2 = SCM_BOOL_F;
    array<SCM> strong_observers;
    array<SCM> weak_observers;
    explicit EnvironmentObj(EnvKind k = EnvKind::Leaf) : Obj(ObjTag::Environment), env_kind(k) {}
};

struct MutexObj : Obj {
    bool locked = false;
    int level = 0;
    bool recursive = false;
    SCM owner = SCM_BOOL_F;
    MutexObj(bool rec = false) : Obj(ObjTag::Mutex), recursive(rec) {}
};

struct CondVarObj : Obj {
    CondVarObj() : Obj(ObjTag::CondVar) {}
};

struct ThreadObj : Obj {
    bool exited = false;
    SCM result = SCM_UNSPECIFIED;
    ThreadObj() : Obj(ObjTag::Thread) {}
};

struct DynamicLibObj : Obj {
    void* handle = nullptr;
    string filename;
    DynamicLibObj(void* h, string fn) : Obj(ObjTag::DynamicLib), handle(h), filename(fn) {}
};

struct AsyncObj : Obj {
    SCM thunk;
    bool marked = false;
    explicit AsyncObj(SCM t) noexcept : Obj(ObjTag::Async), thunk(t) {}
};

// ============================================================================
// Inline Predicates & Accessors
// ============================================================================

inline bool is_keyword(SCM x) noexcept { return is_a<KeywordObj>(x, ObjTag::Keyword); }
inline bool is_vector(SCM x) noexcept { return is_a<VectorObj>(x, ObjTag::Vector); }
inline bool is_weak_vector(SCM x) noexcept { return is_a<WeakVectorObj>(x, ObjTag::WeakVector); }
inline bool is_bignum(SCM x) noexcept { return is_a<BignumObj>(x, ObjTag::Bignum); }
inline bool is_fraction(SCM x) noexcept { return is_a<FractionObj>(x, ObjTag::Fraction); }
inline bool is_real_obj(SCM x) noexcept { return is_a<RealObj>(x, ObjTag::Real); }
inline bool is_complex_obj(SCM x) noexcept { return is_a<ComplexObj>(x, ObjTag::Complex); }
inline bool is_subr(SCM x) noexcept { return is_a<SubrObj>(x, ObjTag::Subr); }
inline bool is_closure(SCM x) noexcept { return is_a<ClosureObj>(x, ObjTag::Closure); }
inline bool is_macro(SCM x) noexcept { return is_a<MacroObj>(x, ObjTag::Macro); }
inline bool is_variable(SCM x) noexcept { return is_a<VariableObj>(x, ObjTag::Variable); }
inline bool is_port(SCM x) noexcept { return is_a<PortObj>(x, ObjTag::Port); }
inline bool is_fluid(SCM x) noexcept { return is_a<FluidObj>(x, ObjTag::Fluid); }
inline bool is_dynamic_state(SCM x) noexcept { return is_a<DynamicStateObj>(x, ObjTag::DynamicState); }
inline bool is_hashtable(SCM x) noexcept { return is_a<HashTableObj>(x, ObjTag::HashTable); }
inline bool is_struct(SCM x) noexcept { return is_a<StructObj>(x, ObjTag::Struct); }
inline uint32_t struct_class_flags(SCM cls) noexcept {
    return is_struct(cls) ? as_obj<StructObj>(cls)->vtable_flags : 0u;
}
inline uint32_t obj_class_flags(SCM obj) noexcept {
    if (!is_struct(obj)) return 0u;
    SCM vt = as_obj<StructObj>(obj)->vtable;
    return is_struct(vt) ? as_obj<StructObj>(vt)->vtable_flags : 0u;
}
inline bool is_goops_instance(SCM x) noexcept {
    return is_struct(x) && ((obj_class_flags(x) & CLASSF_GOOPS) != 0);
}
inline bool is_goops_class(SCM x) noexcept {
    return is_struct(x) && ((obj_class_flags(x) & CLASSF_METACLASS) != 0);
}
inline bool is_continuation(SCM x) noexcept { return is_a<ContinuationObj>(x, ObjTag::Continuation); }
inline bool is_promise(SCM x) noexcept { return is_a<PromiseObj>(x, ObjTag::Promise); }
inline bool is_values(SCM x) noexcept { return is_a<ValuesObj>(x, ObjTag::Values); }
inline bool is_hook(SCM x) noexcept { return is_a<HookObj>(x, ObjTag::Hook); }
inline bool is_guardian(SCM x) noexcept { return is_a<GuardianObj>(x, ObjTag::Guardian); }
inline bool is_arbiter(SCM x) noexcept { return is_a<ArbiterObj>(x, ObjTag::Arbiter); }
inline bool is_random_state(SCM x) noexcept { return is_a<RandomStateObj>(x, ObjTag::RandomState); }
inline bool is_regexp(SCM x) noexcept { return is_a<RegexpObj>(x, ObjTag::Regexp); }
inline bool is_dir(SCM x) noexcept { return is_a<DirObj>(x, ObjTag::Dir); }
inline bool is_uniform_vector(SCM x) noexcept { return is_a<UniformVectorObj>(x, ObjTag::UniformVector); }
inline bool is_bitvector(SCM x) noexcept { return is_a<BitVectorObj>(x, ObjTag::BitVector); }
inline bool is_array(SCM x) noexcept { return is_a<ArrayObj>(x, ObjTag::Array); }
inline bool is_charset(SCM x) noexcept { return is_a<CharSetObj>(x, ObjTag::CharSet); }
inline bool is_proc_with_setter(SCM x) noexcept { return is_a<ProcWithSetterObj>(x, ObjTag::ProcWithSetter); }
inline bool is_eval_closure(SCM x) noexcept { return is_a<EvalClosureObj>(x, ObjTag::EvalClosure); }
inline bool is_smob(SCM x) noexcept { return is_a<SmobObj>(x, ObjTag::Smob); }
inline bool is_stack(SCM x) noexcept { return is_a<StackObj>(x, ObjTag::Stack); }
inline bool is_frame(SCM x) noexcept { return is_a<FrameObj>(x, ObjTag::Frame); }
inline bool is_environment(SCM x) noexcept { return is_a<EnvironmentObj>(x, ObjTag::Environment); }
inline bool is_observer(SCM x) noexcept { return is_a<ObserverObj>(x, ObjTag::Observer); }
inline bool is_box(SCM x) noexcept { return is_a<BoxObj>(x, ObjTag::Box); }
inline bool is_bytecode(SCM x) noexcept { return is_a<BytecodeObj>(x, ObjTag::Bytecode); }

inline bool is_exact_integer(SCM x) noexcept { return x.is_fixnum() || is_bignum(x); }
inline bool is_number(SCM x) noexcept {
    return x.is_fixnum() || is_bignum(x) || is_fraction(x) || is_real_obj(x) || is_complex_obj(x);
}

inline SCM car(SCM x) noexcept { return as_obj<PairObj>(x)->car; }
inline SCM cdr(SCM x) noexcept { return as_obj<PairObj>(x)->cdr; }
inline void set_car(SCM x, SCM v) noexcept { as_obj<PairObj>(x)->car = v; }
inline void set_cdr(SCM x, SCM v) noexcept { as_obj<PairObj>(x)->cdr = v; }

// C++ exception used for Scheme throw & error propagation
struct SchemeThrow {
    SCM key;
    SCM args;
    SchemeThrow(SCM k, SCM a) noexcept : key(k), args(a) {}
};

// C++ exception used when a continuation escapes across a nested C++ trampoline
struct ContinuationEscape {
    SCM target_cont;
    SCM target_dw;
    SCM return_val;
    int target_depth;
    int target_vm_depth = -1;
    SCM target_k_obj = SCM_BOOL_F;
};

} // namespace guile

namespace guile {
// TeXmacs: the hash of the keys of hashmap and hashset (found by ADL)
inline int hash(SCM x) {
    uint64_t h = x.bits;
    h ^= h >> 31; h *= 0x7fb5d329728ea185ULL; h ^= h >> 27;
    return static_cast<int>(h);
}
} // namespace guile
