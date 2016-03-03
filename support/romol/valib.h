#ifndef VALIB_H
#define VALIB_H

#include <stdint.h>
#include <stdbool.h>

#define MAX_VECTOR_LENGTH 64

#if VALIB_HIDE_DECLS
#define MCC_HIDDEN __attribute__((mcc_hidden))
#else
#define MCC_HIDDEN
#endif

// Vector registers
typedef int MCC_HIDDEN valib_vector_t;
enum MCC_HIDDEN {
    VR0,  VR1,  VR2,  VR3,  VR4,  VR5,  VR6,  VR7,
    VR8,  VR9,  VR10, VR11, VR12, VR13, VR14, VR15,
    VR16, VR17, VR18, VR19, VR20, VR21, VR22, VR23,
    VR24, VR25, VR26, VR27, VR28, VR29, VR30, VR31
};

// Mask registers
typedef int MCC_HIDDEN valib_mask_t;
enum MCC_HIDDEN {
    MR0, MR1, MR2, MR3, MR4, MR5, MR6, MR7
};

// Aliases to document the directionality
// This is used by Mercurium, so make sure to
// mark the builtins with the apropiate typedef
typedef valib_vector_t MCC_HIDDEN VDestRegId;
typedef valib_vector_t MCC_HIDDEN VSrcRegId;

typedef valib_mask_t MCC_HIDDEN MaskDestRegId;
typedef valib_mask_t MCC_HIDDEN MaskSrcRegId;

// -------------------------------------------------------
// Vector arithmetic (unmasked)
// -------------------------------------------------------

#define ARITH_BINOP(op) \
void MCC_HIDDEN valib_##op##_db_db_db (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_fl_fl_fl (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
\
void MCC_HIDDEN valib_##op##_int64_int64_int64 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_uint64_uint64_uint64 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
\
void MCC_HIDDEN valib_##op##_int32_int32_int32 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_uint32_uint32_uint32 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
\
void MCC_HIDDEN valib_##op##_int16_int16_int16 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_uint16_uint16_uint16 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
\
void MCC_HIDDEN valib_##op##_int8_int8_int8 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_uint8_uint8_uint8 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2);

// VDEST <- VSRC1 (op) VSRC2
ARITH_BINOP(add)
ARITH_BINOP(sub)
ARITH_BINOP(mul)
ARITH_BINOP(div)
ARITH_BINOP(mod)

#define ARITH_UNOP(op) \
void MCC_HIDDEN valib_##op##_db_db (VDestRegId dest, VSrcRegId src); \
void MCC_HIDDEN valib_##op##_fl_fl (VDestRegId dest, VSrcRegId src); \
\
void MCC_HIDDEN valib_##op##_int64_int64 (VDestRegId dest, VSrcRegId src); \
void MCC_HIDDEN valib_##op##_uint64_uint64 (VDestRegId dest, VSrcRegId src); \
\
void MCC_HIDDEN valib_##op##_int32_int32 (VDestRegId dest, VSrcRegId src); \
void MCC_HIDDEN valib_##op##_uint32_uint32 (VDestRegId dest, VSrcRegId src); \
\
void MCC_HIDDEN valib_##op##_int16_int16 (VDestRegId dest, VSrcRegId src); \
void MCC_HIDDEN valib_##op##_uint16_uint16 (VDestRegId dest, VSrcRegId src); \
\
void MCC_HIDDEN valib_##op##_int8_int8 (VDestRegId dest, VSrcRegId src); \
void MCC_HIDDEN valib_##op##_uint8_uint8 (VDestRegId dest, VSrcRegId src);

// VDEST <- (op) VSRC
ARITH_UNOP(neg)

// -------------------------------------------------------
// Vector arithmetic (masked)
// -------------------------------------------------------

#define ARITH_BINOP_MASK(op) \
void MCC_HIDDEN valib_##op##m_db_db_db (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_fl_fl_fl (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int64_int64_int64 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint64_uint64_uint64 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int32_int32_int32 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint32_uint32_uint32 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int16_int16_int16 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint16_uint16_uint16 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int8_int8_int8 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint8_uint8_uint8 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr);

// VDEST <- VSRC1 (op[mask]) VSRC2
// (FIXME: what are the values of VDEST that correspond to unmasked elements?)
ARITH_BINOP_MASK(add)
ARITH_BINOP_MASK(sub)
ARITH_BINOP_MASK(mul)
ARITH_BINOP_MASK(div)
ARITH_BINOP_MASK(mod)

#define ARITH_UNOP_MASK(op) \
void MCC_HIDDEN valib_##op##m_db_db (VDestRegId dest, VSrcRegId src, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_fl_fl (VDestRegId dest, VSrcRegId src, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int64_int64 (VDestRegId dest, VSrcRegId src, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint64_uint64 (VDestRegId dest, VSrcRegId src, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int32_int32 (VDestRegId dest, VSrcRegId src, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint32_uint32 (VDestRegId dest, VSrcRegId src, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int16_int16 (VDestRegId dest, VSrcRegId src, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint16_uint16 (VDestRegId dest, VSrcRegId src, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int8_int8 (VDestRegId dest, VSrcRegId src, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint8_uint8 (VDestRegId dest, VSrcRegId src, MaskSrcRegId mr);

ARITH_UNOP_MASK(neg)

// -------------------------------------------------------
// Mixed vector/scalar arithmetic (unmasked)
// -------------------------------------------------------

#define ARITH_BINOP_SCALAR(op) \
void MCC_HIDDEN valib_##op##s_db_db_db (VDestRegId dest, VSrcRegId src1, double src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##s_fl_fl_fl (VDestRegId dest, VSrcRegId src1, float src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##s_int64_int64_int64 (VDestRegId dest, VSrcRegId src1, int64_t src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##s_uint64_uint64_uint64 (VDestRegId dest, VSrcRegId src1, uint64_t src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##s_int32_int32_int32 (VDestRegId dest, VSrcRegId src1, int32_t src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##s_uint32_uint32_uint32 (VDestRegId dest, VSrcRegId src1, uint32_t src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##s_int16_int16_int16 (VDestRegId dest, VSrcRegId src1, int16_t src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##s_uint16_uint16_uint16 (VDestRegId dest, VSrcRegId src1, uint16_t src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##s_int8_int8_int8 (VDestRegId dest, VSrcRegId src1, int8_t src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##s_uint8_uint8_uint8 (VDestRegId dest, VSrcRegId src1, int8_t src2, MaskSrcRegId mr);

// VDEST <- VSRC1 (op) scalar_SRC2
ARITH_BINOP_SCALAR(add)
ARITH_BINOP_SCALAR(sub)
ARITH_BINOP_SCALAR(mul)
ARITH_BINOP_SCALAR(div)
ARITH_BINOP_SCALAR(mod)

// -------------------------------------------------------
// Mixed vector/scalar arithmetic (masked)
// -------------------------------------------------------

#define ARITH_BINOP_SCALAR_MASK(op) \
void MCC_HIDDEN valib_##op##sm_db_db_db (VDestRegId dest, VSrcRegId src1, double src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##sm_fl_fl_fl (VDestRegId dest, VSrcRegId src1, float src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##sm_int64_int64_int64 (VDestRegId dest, VSrcRegId src1, int64_t src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##sm_uint64_uint64_uint64 (VDestRegId dest, VSrcRegId src1, uint64_t src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##sm_int32_int32_int32 (VDestRegId dest, VSrcRegId src1, int32_t src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##sm_uint32_uint32_uint32 (VDestRegId dest, VSrcRegId src1, uint32_t src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##sm_int16_int16_int16 (VDestRegId dest, VSrcRegId src1, int16_t src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##sm_uint16_uint16_uint16 (VDestRegId dest, VSrcRegId src1, uint16_t src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##sm_int8_int8_int8 (VDestRegId dest, VSrcRegId src1, int8_t src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##sm_uint8_uint8_uint8 (VDestRegId dest, VSrcRegId src1, int8_t src2, MaskSrcRegId mr);

// VDEST <- VSRC1 (op[mask]) scalar_SRC2
// (FIXME: what are the values of VDEST that correspond to unmasked elements?)
ARITH_BINOP_SCALAR_MASK(add)
ARITH_BINOP_SCALAR_MASK(sub)
ARITH_BINOP_SCALAR_MASK(mul)
ARITH_BINOP_SCALAR_MASK(div)
ARITH_BINOP_SCALAR_MASK(mod)

// -------------------------------------------------------
// Vector bitwise operations
// -------------------------------------------------------
//
// The whole vector is handled like a big integer

#define BITWISE_BINOP(op) \
void MCC_HIDDEN valib_##op(VDestRegId dest, VSrcRegId src1, VSrcRegId src2);

BITWISE_BINOP(or)
BITWISE_BINOP(and)
BITWISE_BINOP(xor)
BITWISE_BINOP(lsl)
BITWISE_BINOP(lsr)
BITWISE_BINOP(asr)

#define BITWISE_UNOP(op) \
void valib_##op##b (VDestRegId dest, VSrcRegId src1, VSrcRegId src2);

BITWISE_UNOP(not)

// -------------------------------------------------------
// Element bitwise operations
// -------------------------------------------------------
//
// Note that for "or, and, xor, not" these operations are functionally
// equivalent to the BITWISE_BINOP and BITWISE_UNOP defined above

#define ELEMENTWISE_BIT_BINOP(op) \
void MCC_HIDDEN valib_##op##_int64_int64_int64 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_uint64_uint64_uint64 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
\
void MCC_HIDDEN valib_##op##_int32_int32_int32 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_uint32_uint32_uint32 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
\
void MCC_HIDDEN valib_##op##_int16_int16_int16 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_uint16_uint16_uint16 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
\
void MCC_HIDDEN valib_##op##_int8_int8_int8 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_uint8_uint8_uint8 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2);

ELEMENTWISE_BIT_BINOP(or)
ELEMENTWISE_BIT_BINOP(and)
ELEMENTWISE_BIT_BINOP(xor)
ELEMENTWISE_BIT_BINOP(lsl)
ELEMENTWISE_BIT_BINOP(lsr)
ELEMENTWISE_BIT_BINOP(asr)

#define ELEMENTWISE_BIT_UNOP(op) \
void MCC_HIDDEN valib_##op##_int64_int64_int64 (VDestRegId dest, VSrcRegId src1); \
void MCC_HIDDEN valib_##op##_uint64_uint64_uint64 (VDestRegId dest, VSrcRegId src1); \
\
void MCC_HIDDEN valib_##op##_int32_int32_int32 (VDestRegId dest, VSrcRegId src1); \
void MCC_HIDDEN valib_##op##_uint32_uint32_uint32 (VDestRegId dest, VSrcRegId src1); \
\
void MCC_HIDDEN valib_##op##_int16_int16_int16 (VDestRegId dest, VSrcRegId src1); \
void MCC_HIDDEN valib_##op##_uint16_uint16_uint16 (VDestRegId dest, VSrcRegId src1); \
\
void MCC_HIDDEN valib_##op##_int8_int8_int8 (VDestRegId dest, VSrcRegId src1); \
void MCC_HIDDEN valib_##op##_uint8_uint8_uint8 (VDestRegId dest, VSrcRegId src1);

ELEMENTWISE_BIT_UNOP(not)

// -------------------------------------------------------
// Element bitwise operations (masked)
// -------------------------------------------------------

#define ELEMENTWISE_BIT_BINOP_MASK(op) \
void MCC_HIDDEN valib_##op##m_int64_int64_int64 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint64_uint64_uint64 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int32_int32_int32 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint32_uint32_uint32 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int16_int16_int16 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint16_uint16_uint16 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int8_int8_int8 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint8_uint8_uint8 (VDestRegId dest, VSrcRegId src1, VSrcRegId src2, MaskSrcRegId mr);

ELEMENTWISE_BIT_BINOP_MASK(or)
ELEMENTWISE_BIT_BINOP_MASK(and)
ELEMENTWISE_BIT_BINOP_MASK(xor)
ELEMENTWISE_BIT_BINOP_MASK(lsl)
ELEMENTWISE_BIT_BINOP_MASK(lsr)
ELEMENTWISE_BIT_BINOP_MASK(asr)

#define ELEMENTWISE_BIT_UNOP_MASK(op) \
void MCC_HIDDEN valib_##op##m_int64_int64_int64 (VDestRegId dest, VSrcRegId src1, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint64_uint64_uint64 (VDestRegId dest, VSrcRegId src1, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int32_int32_int32 (VDestRegId dest, VSrcRegId src1, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint32_uint32_uint32 (VDestRegId dest, VSrcRegId src1, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int16_int16_int16 (VDestRegId dest, VSrcRegId src1, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint16_uint16_uint16 (VDestRegId dest, VSrcRegId src1, MaskSrcRegId mr); \
\
void MCC_HIDDEN valib_##op##m_int8_int8_int8 (VDestRegId dest, VSrcRegId src1, MaskSrcRegId mr); \
void MCC_HIDDEN valib_##op##m_uint8_uint8_uint8 (VDestRegId dest, VSrcRegId src1, MaskSrcRegId mr);

ELEMENTWISE_BIT_UNOP_MASK(not)

// -------------------------------------------------------
// Relational operators
// -------------------------------------------------------
#define RELATIONAL_OP(op) \
void MCC_HIDDEN valib_##op##_db_db (MaskSrcRegId mrdest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_fl_fl (MaskSrcRegId mrdest, VSrcRegId src1, VSrcRegId src2); \
\
void MCC_HIDDEN valib_##op##_int64_int64 (MaskSrcRegId mrdest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_uint64_uint64 (MaskSrcRegId mrdest, VSrcRegId src1, VSrcRegId src2); \
\
void MCC_HIDDEN valib_##op##_int32_int32 (MaskSrcRegId mrdest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_uint32_uint32 (MaskSrcRegId mrdest, VSrcRegId src1, VSrcRegId src2); \
\
void MCC_HIDDEN valib_##op##_int16_int16 (MaskSrcRegId mrdest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_uint16_uint16 (MaskSrcRegId mrdest, VSrcRegId src1, VSrcRegId src2); \
\
void MCC_HIDDEN valib_##op##_int8_int8 (MaskSrcRegId mrdest, VSrcRegId src1, VSrcRegId src2); \
void MCC_HIDDEN valib_##op##_uint8_uint8 (MaskSrcRegId mrdest, VSrcRegId src1, VSrcRegId src2);

RELATIONAL_OP(eq)
RELATIONAL_OP(ne)
RELATIONAL_OP(lt)
RELATIONAL_OP(le)
RELATIONAL_OP(gt)
RELATIONAL_OP(ge)

// -------------------------------------------------------
// Load/Store
// -------------------------------------------------------

// DEST <- *ADDRESS
void MCC_HIDDEN valib_ld_db ( VDestRegId dest, double * address);
void MCC_HIDDEN valib_ld_fl ( VDestRegId dest, float * address);
void MCC_HIDDEN valib_ld_int64 ( VDestRegId dest, int64_t * address);
void MCC_HIDDEN valib_ld_int32 ( VDestRegId dest, int32_t * address);
void MCC_HIDDEN valib_ld_int16 ( VDestRegId dest, int16_t * address);
void MCC_HIDDEN valib_ld_int8 ( VDestRegId dest, int8_t * address);

void MCC_HIDDEN valib_ldm_db ( VDestRegId dest, double * address, MaskSrcRegId msrc);
void MCC_HIDDEN valib_ldm_fl ( VDestRegId dest, float * address, MaskSrcRegId msrc);
void MCC_HIDDEN valib_ldm_int64 ( VDestRegId dest, int64_t * address, MaskSrcRegId msrc);
void MCC_HIDDEN valib_ldm_int32 ( VDestRegId dest, int32_t * address, MaskSrcRegId msrc);
void MCC_HIDDEN valib_ldm_int16 ( VDestRegId dest, int16_t * address, MaskSrcRegId msrc);
void MCC_HIDDEN valib_ldm_int8 ( VDestRegId dest, int8_t * address, MaskSrcRegId msrc);

// *ADDRESS <- SRC
void MCC_HIDDEN valib_st_db ( VSrcRegId src, double * address);
void MCC_HIDDEN valib_st_fl ( VSrcRegId src, float * address);
void MCC_HIDDEN valib_st_int64 ( VSrcRegId src, int64_t * address);
void MCC_HIDDEN valib_st_int32 ( VSrcRegId src, int32_t * address);
void MCC_HIDDEN valib_st_int16 ( VSrcRegId src, int16_t * address);
void MCC_HIDDEN valib_st_int8 ( VSrcRegId src, int8_t * address);

void MCC_HIDDEN valib_stm_db ( VSrcRegId src, double * address, MaskSrcRegId msrc);
void MCC_HIDDEN valib_stm_fl ( VSrcRegId src, float * address, MaskSrcRegId msrc);
void MCC_HIDDEN valib_stm_int64 ( VSrcRegId src, int64_t * address, MaskSrcRegId msrc);
void MCC_HIDDEN valib_stm_int32 ( VSrcRegId src, int32_t * address, MaskSrcRegId msrc);
void MCC_HIDDEN valib_stm_int16 ( VSrcRegId src, int16_t * address, MaskSrcRegId msrc);
void MCC_HIDDEN valib_stm_int8 ( VSrcRegId src, int8_t * address, MaskSrcRegId msrc);

// -------------------------------------------------------
// Gather/Scatter
// -------------------------------------------------------

// src[:] = base[ offset[:] ]
void MCC_HIDDEN valib_gather_offset_db(VDestRegId dest, void *base, VSrcRegId offset);
void MCC_HIDDEN valib_gather_offset_fl(VDestRegId dest, void *base, VSrcRegId offset);
void MCC_HIDDEN valib_gather_offset_int64(VDestRegId dest, void *base, VSrcRegId offset);
void MCC_HIDDEN valib_gather_offset_int32(VDestRegId dest, void *base, VSrcRegId offset);
void MCC_HIDDEN valib_gather_offset_int16(VDestRegId dest, void *base, VSrcRegId offset);
void MCC_HIDDEN valib_gather_offset_int8 (VDestRegId dest, void *base, VSrcRegId offset);

void MCC_HIDDEN valib_gather_offset_mask_db(VDestRegId dest, void *base, VSrcRegId offset, MaskSrcRegId mr);
void MCC_HIDDEN valib_gather_offset_mask_fl(VDestRegId dest, void *base, VSrcRegId offset, MaskSrcRegId mr);
void MCC_HIDDEN valib_gather_offset_mask_int64(VDestRegId dest, void *base, VSrcRegId offset, MaskSrcRegId mr);
void MCC_HIDDEN valib_gather_offset_mask_int32(VDestRegId dest, void *base, VSrcRegId offset, MaskSrcRegId mr);
void MCC_HIDDEN valib_gather_offset_mask_int16(VDestRegId dest, void *base, VSrcRegId offset, MaskSrcRegId mr);
void MCC_HIDDEN valib_gather_offset_mask_int8 (VDestRegId dest, void *base, VSrcRegId offset, MaskSrcRegId mr);

// base[ offset[:] ] = src[:]
void MCC_HIDDEN valib_scatter_offset_db(VSrcRegId src, void *base, VSrcRegId offset);
void MCC_HIDDEN valib_scatter_offset_fl(VSrcRegId src, void *base, VSrcRegId offset);
void MCC_HIDDEN valib_scatter_offset_int64(VSrcRegId src, void *base, VSrcRegId offset);
void MCC_HIDDEN valib_scatter_offset_int32(VSrcRegId src, void *base, VSrcRegId offset);
void MCC_HIDDEN valib_scatter_offset_int16(VSrcRegId src, void *base, VSrcRegId offset);
void MCC_HIDDEN valib_scatter_offset_int8 (VSrcRegId src, void *base, VSrcRegId offset);

void MCC_HIDDEN valib_scatter_offset_mask_db(VSrcRegId src, void *base, VSrcRegId offset, MaskSrcRegId mr);
void MCC_HIDDEN valib_scatter_offset_mask_fl(VSrcRegId src, void *base, VSrcRegId offset, MaskSrcRegId mr);
void MCC_HIDDEN valib_scatter_offset_mask_int64(VSrcRegId src, void *base, VSrcRegId offset, MaskSrcRegId mr);
void MCC_HIDDEN valib_scatter_offset_mask_int32(VSrcRegId src, void *base, VSrcRegId offset, MaskSrcRegId mr);
void MCC_HIDDEN valib_scatter_offset_mask_int16(VSrcRegId src, void *base, VSrcRegId offset, MaskSrcRegId mr);
void MCC_HIDDEN valib_scatter_offset_mask_int8 (VSrcRegId src, void *base, VSrcRegId offset, MaskSrcRegId mr);

// src[:] = *(addr[:])
void MCC_HIDDEN valib_gather_db(VDestRegId dest, VSrcRegId addr);
void MCC_HIDDEN valib_gather_fl(VDestRegId dest, VSrcRegId addr);
void MCC_HIDDEN valib_gather_int64(VDestRegId dest, VSrcRegId addr);
void MCC_HIDDEN valib_gather_int32(VDestRegId dest, VSrcRegId addr);
void MCC_HIDDEN valib_gather_int16(VDestRegId dest, VSrcRegId addr);
void MCC_HIDDEN valib_gather_int8 (VDestRegId dest, VSrcRegId addr);

void MCC_HIDDEN valib_gather_mask_db(VDestRegId dest, VSrcRegId addr, MaskSrcRegId mr);
void MCC_HIDDEN valib_gather_mask_fl(VDestRegId dest, VSrcRegId addr, MaskSrcRegId mr);
void MCC_HIDDEN valib_gather_mask_int64(VDestRegId dest, VSrcRegId addr, MaskSrcRegId mr);
void MCC_HIDDEN valib_gather_mask_int32(VDestRegId dest, VSrcRegId addr, MaskSrcRegId mr);
void MCC_HIDDEN valib_gather_mask_int16(VDestRegId dest, VSrcRegId addr, MaskSrcRegId mr);
void MCC_HIDDEN valib_gather_mask_int8 (VDestRegId dest, VSrcRegId addr, MaskSrcRegId mr);

// *(addr[:]) = src[:]
void MCC_HIDDEN valib_scatter_db(VDestRegId dest, VSrcRegId addr);
void MCC_HIDDEN valib_scatter_fl(VDestRegId dest, VSrcRegId addr);
void MCC_HIDDEN valib_scatter_int64(VDestRegId dest, VSrcRegId addr);
void MCC_HIDDEN valib_scatter_int32(VDestRegId dest, VSrcRegId addr);
void MCC_HIDDEN valib_scatter_int16(VDestRegId dest, VSrcRegId addr);
void MCC_HIDDEN valib_scatter_int8 (VDestRegId dest, VSrcRegId addr);

void MCC_HIDDEN valib_scatter_mask_db(VDestRegId dest, VSrcRegId addr, MaskSrcRegId mr);
void MCC_HIDDEN valib_scatter_mask_fl(VDestRegId dest, VSrcRegId addr, MaskSrcRegId mr);
void MCC_HIDDEN valib_scatter_mask_int64(VDestRegId dest, VSrcRegId addr, MaskSrcRegId mr);
void MCC_HIDDEN valib_scatter_mask_int32(VDestRegId dest, VSrcRegId addr, MaskSrcRegId mr);
void MCC_HIDDEN valib_scatter_mask_int16(VDestRegId dest, VSrcRegId addr, MaskSrcRegId mr);
void MCC_HIDDEN valib_scatter_mask_int8 (VDestRegId dest, VSrcRegId addr, MaskSrcRegId mr);

// -------------------------------------------------------
// Vector reductions
// -------------------------------------------------------

#define VECTOR_RED(op) \
double MCC_HIDDEN valib_red_##op##_db (VSrcRegId src); \
float MCC_HIDDEN valib_red_##op##_fl (VSrcRegId src); \
\
int64_t MCC_HIDDEN valib_red_##op##_int64 (VSrcRegId src); \
uint64_t MCC_HIDDEN valib_red_##op##_uint64 (VSrcRegId src); \
\
int32_t MCC_HIDDEN valib_red_##op##_int32 (VSrcRegId src); \
uint32_t MCC_HIDDEN valib_red_##op##_uint32 (VSrcRegId src); \
\
int16_t MCC_HIDDEN valib_red_##op##_int16 (VSrcRegId src); \
uint16_t MCC_HIDDEN valib_red_##op##_uint16 (VSrcRegId src); \
\
int8_t MCC_HIDDEN valib_red_##op##_int8 (VSrcRegId src); \
uint8_t MCC_HIDDEN valib_red_##op##_uint8 (VSrcRegId src);

VECTOR_RED(add)
VECTOR_RED(sub)
VECTOR_RED(max)
VECTOR_RED(min)

// -------------------------------------------------------
// Vector architectural state
// -------------------------------------------------------

uint32_t MCC_HIDDEN valib_get_vector_length(void);
void     MCC_HIDDEN valib_set_vector_length(uint32_t);

uint32_t MCC_HIDDEN valib_get_max_vector_length(void);

// -------------------------------------------------------
// Vector value movement
// -------------------------------------------------------

void MCC_HIDDEN valib_mov(VDestRegId dest, VSrcRegId src);
void MCC_HIDDEN valib_movm(VDestRegId dest, VSrcRegId src, MaskSrcRegId mr);

// -------------------------------------------------------
// Scalar to vector promotion
// -------------------------------------------------------

void MCC_HIDDEN valib_set_db (VDestRegId dest, double src);
void MCC_HIDDEN valib_set_fl (VDestRegId dest, float src);
void MCC_HIDDEN valib_set_int64 (VDestRegId dest, int64_t src);
void MCC_HIDDEN valib_set_uint64 (VDestRegId dest, uint64_t src);
void MCC_HIDDEN valib_set_int32 (VDestRegId dest, int32_t src);
void MCC_HIDDEN valib_set_uint32 (VDestRegId dest, uint32_t src);
void MCC_HIDDEN valib_set_int16 (VDestRegId dest, int16_t src);
void MCC_HIDDEN valib_set_uint16 (VDestRegId dest, uint16_t src);
void MCC_HIDDEN valib_set_int8 (VDestRegId dest, int8_t src);
void MCC_HIDDEN valib_set_uint8 (VDestRegId dest, uint8_t src);

// -------------------------------------------------------
// Vector conversions (unmasked)
// -------------------------------------------------------
// valib_cvm_SRC_DEST

// Integer conversions/promotions

void MCC_HIDDEN valib_cv_int8_int16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int8_int32(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int8_int64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_int16_int8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int16_int32(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int16_int64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_int32_int8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int32_int16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int32_int64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_int64_int8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int64_int16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int64_int32(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_uint8_uint16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint8_uint32(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint8_uint64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_uint16_uint8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint16_uint32(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint16_uint64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_uint32_uint8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint32_uint16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint32_uint64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_uint64_uint8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint64_uint16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint64_uint32(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_uint8_int16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint8_int32(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint8_int64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_uint16_int8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint16_int32(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint16_int64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_uint32_int8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint32_int16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint32_int64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_uint64_int8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint64_int16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint64_int32(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_int8_uint16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int8_uint32(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int8_uint64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_int16_uint8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int16_uint32(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int16_uint64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_int32_uint8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int32_uint16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int32_uint64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_int64_uint8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int64_uint16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int64_uint32(VDestRegId dest, VDestRegId src);

// Floating conversions/promotions

void MCC_HIDDEN valib_cv_int8_fl(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int16_fl(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int32_fl(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int64_fl(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_int8_db(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int16_db(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int32_db(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_int64_db(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_fl_int8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_fl_int16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_fl_int32(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_fl_int64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_db_int8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_db_int16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_db_int32(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_db_int64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_uint8_fl(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint16_fl(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint32_fl(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint64_fl(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_uint8_db(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint16_db(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint32_db(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_uint64_db(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_fl_uint8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_fl_uint16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_fl_uint32(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_fl_uint64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_db_uint8(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_db_uint16(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_db_uint32(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_db_uint64(VDestRegId dest, VDestRegId src);

void MCC_HIDDEN valib_cv_db_fl(VDestRegId dest, VDestRegId src);
void MCC_HIDDEN valib_cv_fl_db(VDestRegId dest, VDestRegId src);

// -------------------------------------------------------
// Vector conversions (masked)
// -------------------------------------------------------
// valib_cvm_SRC_DEST

// Integer conversions/promotions

void MCC_HIDDEN valib_cvm_int8_int16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int8_int32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int8_int64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_int16_int8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int16_int32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int16_int64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_int32_int8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int32_int16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int32_int64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_int64_int8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int64_int16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int64_int32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_uint8_uint16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint8_uint32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint8_uint64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_uint16_uint8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint16_uint32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint16_uint64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_uint32_uint8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint32_uint16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint32_uint64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_uint64_uint8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint64_uint16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint64_uint32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_uint8_int16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint8_int32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint8_int64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_uint16_int8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint16_int32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint16_int64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_uint32_int8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint32_int16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint32_int64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_uint64_int8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint64_int16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint64_int32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_int8_uint16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int8_uint32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int8_uint64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_int16_uint8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int16_uint32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int16_uint64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_int32_uint8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int32_uint16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int32_uint64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_int64_uint8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int64_uint16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int64_uint32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

// Floating conversions/promotions

void MCC_HIDDEN valib_cvm_int8_fl(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int16_fl(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int32_fl(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int64_fl(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_int8_db(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int16_db(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int32_db(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_int64_db(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_fl_int8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_fl_int16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_fl_int32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_fl_int64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_db_int8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_db_int16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_db_int32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_db_int64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_uint8_fl(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint16_fl(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint32_fl(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint64_fl(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_uint8_db(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint16_db(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint32_db(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_uint64_db(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_fl_uint8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_fl_uint16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_fl_uint32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_fl_uint64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_db_uint8(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_db_uint16(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_db_uint32(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_db_uint64(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

void MCC_HIDDEN valib_cvm_db_fl(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);
void MCC_HIDDEN valib_cvm_fl_db(VDestRegId dest, VDestRegId src, MaskSrcRegId mask);

// -------------------------------------------------------
// Mask operations
// -------------------------------------------------------

void MCC_HIDDEN valib_mask_ld(MaskDestRegId dest, void*);
void MCC_HIDDEN valib_mask_st(MaskSrcRegId dest, void*);
// Note: if the mask fits in a GPR we may want to have
// GPRs <-> mask movements

void MCC_HIDDEN valib_mask_mov(MaskDestRegId dest, MaskSrcRegId src);

void MCC_HIDDEN valib_mask_all(MaskDestRegId dest);
void MCC_HIDDEN valib_mask_none(MaskDestRegId dest);

bool MCC_HIDDEN valib_mask_is_zero(MaskSrcRegId src);

void MCC_HIDDEN valib_mask_or(MaskDestRegId dest, MaskSrcRegId src1, MaskSrcRegId src2);
void MCC_HIDDEN valib_mask_and(MaskDestRegId dest, MaskSrcRegId src1, MaskSrcRegId src2);
void MCC_HIDDEN valib_mask_xor(MaskDestRegId dest, MaskSrcRegId src1, MaskSrcRegId src2);
void MCC_HIDDEN valib_mask_not(MaskDestRegId dest, MaskSrcRegId src);

int32_t MCC_HIDDEN valib_mask_clz(MaskSrcRegId src); // count leading zeros
int32_t MCC_HIDDEN valib_mask_ctz(MaskSrcRegId src); // count trailing zeros
int32_t MCC_HIDDEN valib_mask_popcount(MaskSrcRegId src);

// -------------------------------------------------------
// Selection
// -------------------------------------------------------
// dest is formed by elements of src2 where src1 is 1, src3 otherwise

void MCC_HIDDEN valib_select_int8 (VDestRegId dest, MaskSrcRegId src1, VSrcRegId src2, VSrcRegId src3);
void MCC_HIDDEN valib_select_int16(VDestRegId dest, MaskSrcRegId src1, VSrcRegId src2, VSrcRegId src3);
void MCC_HIDDEN valib_select_int32(VDestRegId dest, MaskSrcRegId src1, VSrcRegId src2, VSrcRegId src3);
void MCC_HIDDEN valib_select_int64(VDestRegId dest, MaskSrcRegId src1, VSrcRegId src2, VSrcRegId src3);

void MCC_HIDDEN valib_select_int8 (VDestRegId dest, MaskSrcRegId src1, VSrcRegId src2, VSrcRegId src3);
void MCC_HIDDEN valib_select_int16(VDestRegId dest, MaskSrcRegId src1, VSrcRegId src2, VSrcRegId src3);
void MCC_HIDDEN valib_select_int32(VDestRegId dest, MaskSrcRegId src1, VSrcRegId src2, VSrcRegId src3);
void MCC_HIDDEN valib_select_int64(VDestRegId dest, MaskSrcRegId src1, VSrcRegId src2, VSrcRegId src3);

void MCC_HIDDEN valib_select_fl(VDestRegId dest, MaskSrcRegId src1, VSrcRegId src2, VSrcRegId src3);
void MCC_HIDDEN valib_select_db(VDestRegId dest, MaskSrcRegId src1, VSrcRegId src2, VSrcRegId src3);

// -------------------------------------------------------
// Insert/extract
// -------------------------------------------------------

/* TBD. Probably not needed */

#endif // VALIB_H
