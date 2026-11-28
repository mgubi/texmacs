#pragma once
#include <cstdint>

namespace guile {

// ============================================================================
// Bytecode Instruction Encoding (32-bit fixed-width)
//   bits  0..7 : opcode (uint8_t)
//   bits  8..15: arg A  (uint8_t)
//   bits 16..31: arg B  (uint16_t / int16_t)
//   bits  8..31: arg U24 (uint32_t)
// ============================================================================

enum Op : uint8_t {
    OP_CONST = 0,       // u24: push consts[u24]
    OP_FIXNUM,          // s16: push SCM::from_fixnum(s16)
    OP_TRUE,
    OP_FALSE,
    OP_NIL,
    OP_UNSPEC,
    OP_UNDEF,
    OP_POP,
    OP_DUP,

    OP_LOCAL_REF,       // u24: push base[u24]
    OP_LOCAL_REF_CHK,   // u24: push base[u24], error if SCM_UNDEFINED
    OP_LOCAL_SET,       // u24: base[u24] = pop()
    OP_BOX_INIT,        // u24: base[u24] = BoxObj(base[u24])
    OP_BOX_NEW_UNDEF,   // u24: base[u24] = BoxObj(SCM_UNDEFINED)
    OP_BOX_REF,         // u24: push BoxObj(base[u24])->val
    OP_BOX_REF_CHK,     // u24: push BoxObj(base[u24])->val, error if SCM_UNDEFINED
    OP_BOX_SET,         // u24: BoxObj(base[u24])->val = pop()

    OP_UPVAL_REF,       // u24: push upvals[u24]
    OP_UPVAL_BOX_REF,   // u24: push BoxObj(upvals[u24])->val
    OP_UPVAL_BOX_REF_CHK,
    OP_UPVAL_BOX_SET,   // u24: BoxObj(upvals[u24])->val = pop()
    OP_CELL_CAR_REF,    // u24: push car(upvals[u24])
    OP_CELL_CAR_SET,    // u24: set_car(upvals[u24], pop())
    OP_CELL_CDR_REF,    // u24: push cdr(upvals[u24])
    OP_CELL_CDR_SET,    // u24: set_cdr(upvals[u24], pop())

    OP_GLOBAL_REF,      // u24: push global[u24]
    OP_GLOBAL_SET,      // u24: global[u24] = pop()

    OP_JUMP,            // s16: pc += s16
    OP_JUMP_IF_FALSE,   // s16: if (pop().is_false()) pc += s16
    OP_JUMP_IF_TRUE,    // s16: if (pop().is_true()) pc += s16
    OP_JUMP_IF_FALSE_KEEP, // s16: if (top.is_false()) pc += s16; else pop()
    OP_JUMP_IF_TRUE_KEEP,  // s16: if (top.is_true()) pc += s16; else pop()

    OP_MAKE_CLOSURE,    // u24: push ClosureObj from consts[u24]
    OP_CALL,            // u24: nargs
    OP_TAIL_CALL,       // u24: nargs
    OP_RETURN,

    // Fast inlined arithmetic & comparisons (arg B = global cache idx for slow fallback)
    OP_ADD2,
    OP_SUB2,
    OP_MUL2,
    OP_DIV2,
    OP_ABS,
    OP_ADDN,            // A = nargs (1 or 3..16), B = global idx
    OP_ADD1,
    OP_SUB1,
    OP_NUMEQ2,
    OP_LT2,
    OP_LE2,
    OP_GT2,
    OP_GE2,
    OP_EVENP,
    OP_ODDP,

    // Fast inlined core ops (arg B = global cache idx for slow fallback)
    OP_EQ,
    OP_NULLP,
    OP_PAIRP,
    OP_NOT,
    OP_CAR,
    OP_CDR,
    OP_CONS,
    OP_REVERSE,
    OP_VEC_REF,
    OP_VEC_SET,
    OP_MAKE_VEC,        // A = nargs (1 or 2), B = global idx
    OP_STRUCT_REF,
    OP_STRUCT_SET,
    OP_HASHV_REF,       // A = nargs (2 or 3), B = global idx
    OP_HASHV_SET,
    OP_HASHQ_REF,       // A = nargs (2 or 3), B = global idx
    OP_HASHQ_SET,
    OP_CALLCC,          // A = is_tail (0/1), B = global idx

    // Special fused fast ops for tight loops!
    OP_LOCAL_ADD_IMM,   // A = local_slot, B = int16_t imm (2-word op)
    OP_LOCAL_LT_IMM_JF, // A = local_slot, B = int16_t imm; next word = (gi<<16)|delta (2-word op)
    OP_LOCAL_EQ_IMM_JF, // A = local_slot, B = int16_t imm; next word = (gi<<16)|delta (2-word op)
    OP_CLEAR_LOCALS,    // A = start_slot, B = count
    OP_DYNWIND_PUSH,    // internal helper op for dynamic-wind
    OP_DYNWIND_POP,     // internal helper op for dynamic-wind
    OP_NUM_OPS
};

static inline uint32_t enc0(Op op) noexcept { return static_cast<uint32_t>(op); }
static inline uint32_t enc_u24(Op op, uint32_t u) noexcept { return static_cast<uint32_t>(op) | (u << 8); }
static inline uint32_t enc_s16(Op op, int16_t s) noexcept { return static_cast<uint32_t>(op) | (static_cast<uint32_t>(static_cast<uint16_t>(s)) << 16); }
static inline uint32_t enc_ab(Op op, uint8_t a, uint16_t b) noexcept {
    return static_cast<uint32_t>(op) | (static_cast<uint32_t>(a) << 8) | (static_cast<uint32_t>(b) << 16);
}
static inline uint8_t dec_op(uint32_t insn) noexcept { return static_cast<uint8_t>(insn & 0xFFu); }
static inline uint8_t dec_a(uint32_t insn) noexcept { return static_cast<uint8_t>((insn >> 8) & 0xFFu); }
static inline uint16_t dec_b(uint32_t insn) noexcept { return static_cast<uint16_t>(insn >> 16); }
static inline int16_t dec_s16(uint32_t insn) noexcept { return static_cast<int16_t>(insn >> 16); }
static inline uint32_t dec_u24(uint32_t insn) noexcept { return insn >> 8; }

} // namespace guile
