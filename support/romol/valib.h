#ifndef VALIB_H
#define VALIB_H

#include <stdint.h>

// Vector registers
typedef
enum VRegId_tag
{
    V_0,  V_1,  V_2,  V_3,  V_4,  V_5,  V_6,  V_7,
    V_8,  V_9,  V_10, V_11, V_12, V_13, V_14, V_15,
    V_16, V_17, V_18, V_19, V_20, V_21, V_22, V_23,
    V_24, V_25, V_26, V_27, V_28, V_29, V_30, V_31
} VRegId;

// Mask registers
typedef
enum MaskRegId_tag
{
    M_0, M_1, M_2, M_3, M_4, M_5, M_6, M_7
} MaskRegId;

// -------------------------------------------------------
// Vector arithmetic (unmasked)
// -------------------------------------------------------

#define ARITH_BINOP(op) \
void valib_##op##_db_db_db (VRegId dest, VRegId src1, VRegId src2); \
void valib_##op##_fl_fl_fl (VRegId dest, VRegId src1, VRegId src2); \
\
void valib_##op##_int64_int64_int64 (VRegId dest, VRegId src1, VRegId src2); \
void valib_##op##_uint64_uint64_uint64 (VRegId dest, VRegId src1, VRegId src2); \
\
void valib_##op##_int32_int32_int32 (VRegId dest, VRegId src1, VRegId src2); \
void valib_##op##_uint32_uint32_uint32 (VRegId dest, VRegId src1, VRegId src2); \
\
void valib_##op##_int16_int16_int16 (VRegId dest, VRegId src1, VRegId src2); \
void valib_##op##_uint16_uint16_uint16 (VRegId dest, VRegId src1, VRegId src2); \
\
void valib_##op##_int8_int8_int8 (VRegId dest, VRegId src1, VRegId src2); \
void valib_##op##_uint8_uint8_uint8 (VRegId dest, VRegId src1, VRegId src2);

// VDEST <- VSRC1 (op) VSRC2
ARITH_BINOP(add)
ARITH_BINOP(sub)
ARITH_BINOP(mul)

#define ARITH_UNOP(op) \
void valib_##op##_db_db (VRegId dest, VRegId src); \
void valib_##op##_fl_fl (VRegId dest, VRegId src); \
\
void valib_##op##_int64_int64 (VRegId dest, VRegId src); \
void valib_##op##_uint64_uint64 (VRegId dest, VRegId src); \
\
void valib_##op##_int32_int32 (VRegId dest, VRegId src); \
void valib_##op##_uint32_uint32 (VRegId dest, VRegId src); \
\
void valib_##op##_int16_int16 (VRegId dest, VRegId src); \
void valib_##op##_uint16_uint16 (VRegId dest, VRegId src); \
\
void valib_##op##_int8_int8 (VRegId dest, VRegId src); \
void valib_##op##_uint8_uint8 (VRegId dest, VRegId src);

// VDEST <- (op) VSRC
ARITH_UNOP(neg)

// -------------------------------------------------------
// Vector arithmetic (masked)
// -------------------------------------------------------

#define ARITH_BINOP_MASK(op) \
void valib_##op##m_db_db_db (VRegId dest, VRegId src1, VRegId src2, MaskRegId mr); \
void valib_##op##m_fl_fl_fl (VRegId dest, VRegId src1, VRegId src2, MaskRegId mr); \
\
void valib_##op##m_int64_int64_int64 (VRegId dest, VRegId src1, VRegId src2, MaskRegId mr); \
void valib_##op##m_uint64_uint64_uint64 (VRegId dest, VRegId src1, VRegId src2, MaskRegId mr); \
\
void valib_##op##m_int32_int32_int32 (VRegId dest, VRegId src1, VRegId src2, MaskRegId mr); \
void valib_##op##m_uint32_uint32_uint32 (VRegId dest, VRegId src1, VRegId src2, MaskRegId mr); \
\
void valib_##op##m_int16_int16_int16 (VRegId dest, VRegId src1, VRegId src2, MaskRegId mr); \
void valib_##op##m_uint16_uint16_uint16 (VRegId dest, VRegId src1, VRegId src2, MaskRegId mr); \
\
void valib_##op##m_int8_int8_int8 (VRegId dest, VRegId src1, VRegId src2, MaskRegId mr); \
void valib_##op##m_uint8_uint8_uint8 (VRegId dest, VRegId src1, VRegId src2, MaskRegId mr);

// VDEST <- VSRC1 (op[mask]) VSRC2
// (FIXME: what are the values of VDEST that correspond to unmasked elements?)
ARITH_BINOP_MASK(add)
ARITH_BINOP_MASK(sub)
ARITH_BINOP_MASK(mul)

#define ARITH_UNOP_MASK(op) \
void valib_##op##m_db_db (VRegId dest, VRegId src, MaskRegId mr); \
void valib_##op##m_fl_fl (VRegId dest, VRegId src, MaskRegId mr); \
\
void valib_##op##m_int64_int64 (VRegId dest, VRegId src, MaskRegId mr); \
void valib_##op##m_uint64_uint64 (VRegId dest, VRegId src, MaskRegId mr); \
\
void valib_##op##m_int32_int32 (VRegId dest, VRegId src, MaskRegId mr); \
void valib_##op##m_uint32_uint32 (VRegId dest, VRegId src, MaskRegId mr); \
\
void valib_##op##m_int16_int16 (VRegId dest, VRegId src, MaskRegId mr); \
void valib_##op##m_uint16_uint16 (VRegId dest, VRegId src, MaskRegId mr); \
\
void valib_##op##m_int8_int8 (VRegId dest, VRegId src, MaskRegId mr); \
void valib_##op##m_uint8_uint8 (VRegId dest, VRegId src, MaskRegId mr);

ARITH_UNOP_MASK(neg)

// -------------------------------------------------------
// Mixed vector/scalar arithmetic (unmasked)
// -------------------------------------------------------

#define ARITH_BINOP_SCALAR(op) \
void valib_##op##s_db_db_db (VRegId dest, VRegId src1, double src2, MaskRegId mr); \
void valib_##op##s_fl_fl_fl (VRegId dest, VRegId src1, float src2, MaskRegId mr); \
\
void valib_##op##s_int64_int64_int64 (VRegId dest, VRegId src1, int64_t src2, MaskRegId mr); \
void valib_##op##s_uint64_uint64_uint64 (VRegId dest, VRegId src1, uint64_t src2, MaskRegId mr); \
\
void valib_##op##s_int32_int32_int32 (VRegId dest, VRegId src1, int32_t src2, MaskRegId mr); \
void valib_##op##s_uint32_uint32_uint32 (VRegId dest, VRegId src1, uint32_t src2, MaskRegId mr); \
\
void valib_##op##s_int16_int16_int16 (VRegId dest, VRegId src1, int16_t src2, MaskRegId mr); \
void valib_##op##s_uint16_uint16_uint16 (VRegId dest, VRegId src1, uint16_t src2, MaskRegId mr); \
\
void valib_##op##s_int8_int8_int8 (VRegId dest, VRegId src1, int8_t src2, MaskRegId mr); \
void valib_##op##s_uint8_uint8_uint8 (VRegId dest, VRegId src1, int8_t src2, MaskRegId mr);

// VDEST <- VSRC1 (op) scalar_SRC2
ARITH_BINOP_SCALAR(add)
ARITH_BINOP_SCALAR(sub)
ARITH_BINOP_SCALAR(mul)

// -------------------------------------------------------
// Mixed vector/scalar arithmetic (masked)
// -------------------------------------------------------

#define ARITH_BINOP_SCALAR_MASK(op) \
void valib_##op##sm_db_db_db (VRegId dest, VRegId src1, double src2, MaskRegId mr); \
void valib_##op##sm_fl_fl_fl (VRegId dest, VRegId src1, float src2, MaskRegId mr); \
\
void valib_##op##sm_int64_int64_int64 (VRegId dest, VRegId src1, int64_t src2, MaskRegId mr); \
void valib_##op##sm_uint64_uint64_uint64 (VRegId dest, VRegId src1, uint64_t src2, MaskRegId mr); \
\
void valib_##op##sm_int32_int32_int32 (VRegId dest, VRegId src1, int32_t src2, MaskRegId mr); \
void valib_##op##sm_uint32_uint32_uint32 (VRegId dest, VRegId src1, uint32_t src2, MaskRegId mr); \
\
void valib_##op##sm_int16_int16_int16 (VRegId dest, VRegId src1, int16_t src2, MaskRegId mr); \
void valib_##op##sm_uint16_uint16_uint16 (VRegId dest, VRegId src1, uint16_t src2, MaskRegId mr); \
\
void valib_##op##sm_int8_int8_int8 (VRegId dest, VRegId src1, int8_t src2, MaskRegId mr); \
void valib_##op##sm_uint8_uint8_uint8 (VRegId dest, VRegId src1, int8_t src2, MaskRegId mr);

// VDEST <- VSRC1 (op[mask]) scalar_SRC2
// (FIXME: what are the values of VDEST that correspond to unmasked elements?)
ARITH_BINOP_SCALAR_MASK(add)
ARITH_BINOP_SCALAR_MASK(sub)
ARITH_BINOP_SCALAR_MASK(mul)

// -------------------------------------------------------
// Bitwise operations (vector)
// -------------------------------------------------------

#define ELEMENTWISE_BIT_BINOP(op) \
void valib_##op##_int64_int64_int64 (VRegId dest, VRegId src1, VRegId src2); \
void valib_##op##_uint64_uint64_uint64 (VRegId dest, VRegId src1, VRegId src2); \
\
void valib_##op##_int32_int32_int32 (VRegId dest, VRegId src1, VRegId src2); \
void valib_##op##_uint32_uint32_uint32 (VRegId dest, VRegId src1, VRegId src2); \
\
void valib_##op##_int16_int16_int16 (VRegId dest, VRegId src1, VRegId src2); \
void valib_##op##_uint16_uint16_uint16 (VRegId dest, VRegId src1, VRegId src2); \
\
void valib_##op##_int8_int8_int8 (VRegId dest, VRegId src1, VRegId src2); \
void valib_##op##_uint8_uint8_uint8 (VRegId dest, VRegId src1, VRegId src2);

ELEMENTWISE_BIT_BINOP(lsl)
ELEMENTWISE_BIT_BINOP(lsr)
ELEMENTWISE_BIT_BINOP(asr)

#define BITWISE_BINOP(op) \
void valib_##op(VRegId dest, VRegId src1, VRegId src2);

BITWISE_BINOP(or)
BITWISE_BINOP(and)
BITWISE_BINOP(xor)
BITWISE_BINOP(lsl)
BITWISE_BINOP(lsr)
BITWISE_BINOP(asr)

#define BITWISE_UNOP(op) \
void valib_##op##b (VRegId dest, VRegId src1, VRegId src2);

BITWISE_UNOP(not)

// -------------------------------------------------------
// Relational operators
// -------------------------------------------------------
#define RELATIONAL_OP(op) \
void valib_##op##_db_db (MaskRegId mrdest, VRegId src1, VRegId src2); \
void valib_##op##_fl_fl (MaskRegId mrdest, VRegId src1, VRegId src2); \
\
void valib_##op##_int64_int64 (MaskRegId mrdest, VRegId src1, VRegId src2); \
void valib_##op##_uint64_uint64 (MaskRegId mrdest, VRegId src1, VRegId src2); \
\
void valib_##op##_int32_int32 (MaskRegId mrdest, VRegId src1, VRegId src2); \
void valib_##op##_uint32_uint32 (MaskRegId mrdest, VRegId src1, VRegId src2); \
\
void valib_##op##_int16_int16 (MaskRegId mrdest, VRegId src1, VRegId src2); \
void valib_##op##_uint16_uint16 (MaskRegId mrdest, VRegId src1, VRegId src2); \
\
void valib_##op##_int8_int8 (MaskRegId mrdest, VRegId src1, VRegId src2); \
void valib_##op##_uint8_uint8 (MaskRegId mrdest, VRegId src1, VRegId src2);

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
void valib_ld_db ( VRegId dest, double * address);
void valib_ld_fl ( VRegId dest, float * address);
void valib_ld_int64 ( VRegId dest, int64_t * address);
void valib_ld_int32 ( VRegId dest, int32_t * address);
void valib_ld_int16 ( VRegId dest, int16_t * address);
void valib_ld_int8 ( VRegId dest, int8_t * address);

// *ADDRESS <- SRC
void valib_st_db ( VRegId src, double * address);
void valib_st_fl ( VRegId src, float * address);
void valib_st_int64 ( VRegId src, int64_t * address);
void valib_st_int32 ( VRegId src, int32_t * address);
void valib_st_int16 ( VRegId src, int16_t * address);
void valib_st_int8 ( VRegId src, int8_t * address);

// -------------------------------------------------------
// Gather/Scatter
// -------------------------------------------------------

/* TBD */

// -------------------------------------------------------
// Vector reductions
// -------------------------------------------------------

#define VECTOR_RED(op) \
double valib_##op##_db (VRegId src); \
float valib_##op##_fl (VRegId src); \
\
int64_t valib_##op##_int64 (VRegId src); \
uint64_t valib_##op##_uint64 (VRegId src); \
\
int32_t valib_##op##_int32 (VRegId src); \
uint32_t valib_##op##_uint32 (VRegId src); \
\
int16_t valib_##op##_int16 (VRegId src); \
uint16_t valib_##op##_uint16 (VRegId src); \
\
int8_t valib_##op##_int8 (VRegId src); \
uint8_t valib_##op##_uint8 (VRegId src);

VECTOR_RED(add)
VECTOR_RED(sub)
VECTOR_RED(max)
VECTOR_RED(min)

// -------------------------------------------------------
// Vector architectural state
// -------------------------------------------------------

uint32_t valib_get_vector_length(void);
void     valib_set_vector_length(uint32_t);

uint32_t valib_get_max_vector_length(void);

// -------------------------------------------------------
// Vector value movement
// -------------------------------------------------------

void valib_mov(VRegId dest, VRegId src);
void valib_movm(VRegId dest, VRegId src, MaskRegId mr);

// -------------------------------------------------------
// Scalar to vector promotion
// -------------------------------------------------------

void valib_set_db (VRegId dest, double src);
void valib_set_fl (VRegId dest, float src);
void valib_set_int64 (VRegId dest, int64_t src);
void valib_set_uint64 (VRegId dest, uint64_t src);
void valib_set_int32 (VRegId dest, int32_t src);
void valib_set_uint32 (VRegId dest, uint32_t src);
void valib_set_int16 (VRegId dest, int16_t src);
void valib_set_uint16 (VRegId dest, uint16_t src);
void valib_set_int8 (VRegId dest, int8_t src);
void valib_set_uint8 (VRegId dest, uint8_t src);

// -------------------------------------------------------
// Mask operations
// -------------------------------------------------------

void valib_mask_all(MaskRegId dest);
void valib_mask_none(MaskRegId dest);

void valib_mask_or(MaskRegId dest, MaskRegId src1, MaskRegId src2);
void valib_mask_and(MaskRegId dest, MaskRegId src1, MaskRegId src2);
void valib_mask_xor(MaskRegId dest, MaskRegId src1, MaskRegId src2);

int32_t valib_mask_clz(MaskRegId src); // count leading zeros
int32_t valib_mask_ctz(MaskRegId src); // count trailing zeros
int32_t valib_mask_popcount(MaskRegId src);

// -------------------------------------------------------
// Insert/extract
// -------------------------------------------------------

/* TBD. Probably not needed */

#endif // VALIB_H
