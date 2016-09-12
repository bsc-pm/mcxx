/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

#include "fortran03-intrinsics.h"
#include "cxx-ast.h"
#include "cxx-utils.h"
#include "cxx-scope-decls.h"
#include "cxx-entrylist.h"
#include "cxx-typeutils.h"
#include "cxx-exprtype.h"
#include "cxx-ambiguity.h"
#include "cxx-limits.h"
#include "fortran03-typeutils.h"
#include "fortran03-buildscope.h"
#include "fortran03-exprtype.h"
#include "fortran03-scope.h"
#include "fortran03-modules.h"
#include <string.h>
#include "red_black_tree.h"
#include "fortran03-intrinsics-simplify.h"

typedef
enum intrinsic_kind_tag
{
    INTRINSIC_KIND_NONE = 0,
    INTRINSIC_KIND_ATOMIC_SUBROUTINE,
    INTRINSIC_KIND_ELEMENTAL_FUNCTION,
    INTRINSIC_KIND_ELEMENTAL_SUBROUTINE,
    INTRINSIC_KIND_INQUIRY_FUNCTION,
    INTRINSIC_KIND_PURE_SUBROUTINE,
    INTRINSIC_KIND_IMPURE_SUBROUTINE,
    INTRINSIC_KIND_TRANSFORMATIONAL_FUNCTION,

    INTRINSIC_KIND_MIXED,
} intrinsic_kind_t;

// Internal state variable which is only set in fortran_solve_generic_intrinsic_call
static char solving_call_statement = 0;

// We do not use the I in <complex.h>
#undef I

#define A  INTRINSIC_KIND_ATOMIC_SUBROUTINE
#define E  INTRINSIC_KIND_ELEMENTAL_FUNCTION
#define ES INTRINSIC_KIND_ELEMENTAL_SUBROUTINE
#define I  INTRINSIC_KIND_INQUIRY_FUNCTION
#define PS INTRINSIC_KIND_PURE_SUBROUTINE
#define S  INTRINSIC_KIND_IMPURE_SUBROUTINE
#define T  INTRINSIC_KIND_TRANSFORMATIONAL_FUNCTION

// Mixed is for intrinsics that can be both subroutine or function
#define M  INTRINSIC_KIND_MIXED

/* 
 * Syntax
 *
 * FORTRAN_GENERIC_INTRINSIC(module-name, id, argument-keywords, kind-of-intrinsic, constant-evaluation)
 * FORTRAN_GENERIC_INTRINSIC_2(module-name, id, argument-keywords0, kind-of-intrinsic0, constant-evaluation0, 
 *                                 argument-keywords1, kind-of-intrinsic1, constant-evaluation1)
 *
 * argument-keywords: string literal of comma-separated (with no blanks!) of uppercase names optionally preceded by ? when they are optional. 
 *                    Example:   "A,B,?C,?D" means four keywords A, B, C and D where C and D are optional
 *
 * kind-of-intrinsic: can be A, E, ES, I, PS, S or T. See above for its meaning
 *
 * constant-evaluation: pointer to a function implementing constant evaluation of this intrinsic. 
 *
 * FORTRAN_GENERIC_INTRINSIC_2 is used for those intrinsics sharing the same name but callable with very different argument keywords
 *
 * Some intrinsics (like MAX) allow an unbounded length of arguments. In these
 * intrinsics, argument-keywords should be NULL
 */

#define FORTRAN_INTRINSIC_GENERIC_LIST \
FORTRAN_GENERIC_INTRINSIC(NULL, abs, "A", E, simplify_abs) \
FORTRAN_GENERIC_INTRINSIC(NULL, achar, "I,?KIND", E, simplify_achar) \
FORTRAN_GENERIC_INTRINSIC(NULL, acos, "X", E, simplify_acos) \
FORTRAN_GENERIC_INTRINSIC(NULL, acosh, "X", E, simplify_acosh) \
FORTRAN_GENERIC_INTRINSIC(NULL, adjustl, "STRING", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, adjustr, "STRING", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, aimag, "Z", E, simplify_aimag) \
FORTRAN_GENERIC_INTRINSIC(NULL, aint, "A,?KIND", E, simplify_aint) \
FORTRAN_GENERIC_INTRINSIC(NULL, all, "MASK,?DIM", T, NULL) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, allocated, "ARRAY", I, NULL, "SCALAR", I, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, anint, "A,?KIND", E, simplify_anint) \
FORTRAN_GENERIC_INTRINSIC(NULL, any, "MASK,?DIM", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, asin, "X", E, simplify_asin) \
FORTRAN_GENERIC_INTRINSIC(NULL, asinh, "X", E, simplify_asinh) \
FORTRAN_GENERIC_INTRINSIC(NULL, associated, "POINTER,?TARGET", I, NULL) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, atan, "X", E, simplify_atan, "Y,X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, atan2, "Y,X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, atanh, "X", E, simplify_atanh) \
FORTRAN_GENERIC_INTRINSIC(NULL, atomic_define, "ATOM,VALUE", A, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, atomic_ref, "VALUE,ATOM", A, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, bessel_j0, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, bessel_j1, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, bessel_jn, "N,X", E, NULL, "N1,N2,X", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, bessel_y0, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, bessel_y1, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, bessel_yn, "N,X", E, NULL, "N1,N2,X", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, bge, "I,J", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, bgt, "I,J", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, ble, "I,J", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, blt, "I,J", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, bit_size, "I", I, simplify_bit_size) \
FORTRAN_GENERIC_INTRINSIC(NULL, btest, "I,POS", E, simplify_btest) \
FORTRAN_GENERIC_INTRINSIC(NULL, ceiling, "A,?KIND", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, char, "I,?KIND", E, simplify_char) \
FORTRAN_GENERIC_INTRINSIC(NULL, cmplx, "X,?Y,?KIND", E, simplify_cmplx) \
FORTRAN_GENERIC_INTRINSIC(NULL, dcmplx, "X,?Y", E, simplify_dcmplx) \
FORTRAN_GENERIC_INTRINSIC(NULL, command_argument_count, "", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, conjg, "Z", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, cos, "X", E, simplify_cos) \
FORTRAN_GENERIC_INTRINSIC(NULL, cosh, "X", E, simplify_cosh) \
FORTRAN_GENERIC_INTRINSIC(NULL, count, "MASK,?DIM,?KIND", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, cpu_time, "TIME", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, cshift, "ARRAY,SHIFT,?DIM", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, date_and_time, "?DATE,?TIME,?ZONE,?VALUES", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, dble, "A", E, simplify_dble) \
FORTRAN_GENERIC_INTRINSIC(NULL, digits, "X", I, simplify_digits) \
FORTRAN_GENERIC_INTRINSIC(NULL, dim, "X,Y", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, dot_product, "VECTOR_A,VECTOR_B", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, dprod, "X,Y", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, dshiftl, "I,J,SHIFT", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, dshiftr, "I,J,SHIFT", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, eoshift, "ARRAY,SHIFT,?BOUNDARY,?DIM", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, epsilon, "X", I, simplify_epsilon) \
FORTRAN_GENERIC_INTRINSIC(NULL, erf, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, erfc, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, erfc_scaled, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, execute_command_line, "COMMAND,?WAIT,?EXITSTAT,?CMDSTAT,?CMDMSG", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, exp, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, exponent, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, extends_type_of, "A,MOLD", I, NULL) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, findloc, "ARRAY,VALUE,DIM,?MASK,?KIND,?BACK", T, NULL, "ARRAY,VALUE,?MASK,?KIND,?BACK", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, floor, "A,?KIND", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, flush, "?UNIT", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, fraction, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, gamma, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, get_command, "?COMMAND,?LENGTH,?STATUS", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, get_command_argument, "NUMBER,?VALUE,?LENGTH,?STATUS", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, get_environment_variable, "NAME,?VALUE,?LENGTH,?STATUS,?TRIM_NAME", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, huge, "X", I, simplify_huge) \
FORTRAN_GENERIC_INTRINSIC(NULL, hypot, "X,Y", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, iachar, "C,?KIND", E, simplify_iachar) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, iall, "ARRAY,DIM,?MASK", E, NULL, "ARRAY,?MASK", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, iand, "I,J", E, simplify_iand) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, iany, "ARRAY,DIM,?MASK", E, NULL, "ARRAY,?MASK", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, ibclr, "I,POS", E, simplify_ibclr) \
FORTRAN_GENERIC_INTRINSIC(NULL, ibits, "I,POS,LEN", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, ibset, "I,POS", E, simplify_ibset) \
FORTRAN_GENERIC_INTRINSIC(NULL, ichar, "C,?KIND", E, simplify_ichar) \
FORTRAN_GENERIC_INTRINSIC(NULL, ieor, "I,J", E, simplify_ieor) \
FORTRAN_GENERIC_INTRINSIC(NULL, image_index, "COARRAY,SUB", I, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, index, "STRING,SUBSTRING,?BACK,?KIND", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, int, "A,?KIND", E, simplify_int) \
FORTRAN_GENERIC_INTRINSIC(NULL, ior, "I,J", E, simplify_ior) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, iparity, "ARRAY,DIM,?MASK", T, NULL, "ARRAY,?MASK", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, ishft, "I,SHIFT", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, ishftc, "I,SHIFT,?SIZE", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, is_contiguous, "ARRAY", I, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, is_iostat_end, "I", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, is_iostat_eor, "I", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, kind, "X", I, simplify_kind) \
FORTRAN_GENERIC_INTRINSIC(NULL, lbound, "ARRAY,?DIM,?KIND", I, simplify_lbound) \
FORTRAN_GENERIC_INTRINSIC(NULL, lcobound, "COARRAY,?DIM,?KIND", I, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, leadz, "I", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, len, "STRING,?KIND", I, simplify_len) \
FORTRAN_GENERIC_INTRINSIC(NULL, len_trim, "STRING,?KIND", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, lge, "STRING_A,STRING_B", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, lgt, "STRING_A,STRING_B", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, lle, "STRING_A,STRING_B", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, llt, "STRING_A,STRING_B", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, log, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, log_gamma, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, log10, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, logical, "L,?KIND", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, malloc, "SIZE", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, maskl, "I,?KIND", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, maskr, "I,?KIND", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, matmul, "MATRIX_A,MATRIX_B", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, max, NULL, E, simplify_max) \
FORTRAN_GENERIC_INTRINSIC(NULL, maxexponent, "X", I, simplify_maxexponent) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, maxloc, "ARRAY,DIM,?MASK,?KIND,?BACK", T, NULL, "ARRAY,?MASK,?KIND,?BACK", T, NULL) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, maxval, "ARRAY,DIM,?MASK", T, simplify_maxval, "ARRAY,?MASK", T, simplify_maxval) \
FORTRAN_GENERIC_INTRINSIC(NULL, merge, "TSOURCE,FSOURCE,MASK", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, merge_bits, "I,J,MASK", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, min, NULL, E, simplify_min) \
FORTRAN_GENERIC_INTRINSIC(NULL, minexponent, "X", I, simplify_minexponent) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, minloc, "ARRAY,DIM,?MASK,?KIND,?BACK", E, NULL, "ARRAY,?MASK,?KIND,?BACK", T, NULL) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, minval, "ARRAY,DIM,?MASK", T, simplify_minval, "ARRAY,?MASK", T, simplify_minval) \
FORTRAN_GENERIC_INTRINSIC(NULL, mod, "A,P", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, modulo, "A,P", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, move_alloc, "FROM,TO", PS, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, mvbits, "FROM,FROMPOS,LEN,TO,TOPOS", ES, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, nearest, "X,S", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, new_line, "A", I, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, nint, "A,?KIND", E, simplify_nint) \
FORTRAN_GENERIC_INTRINSIC(NULL, not, "I", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, norm2, "X,?DIM", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, null, "?MOLD", T, simplify_null) \
FORTRAN_GENERIC_INTRINSIC(NULL, num_images, "", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, pack, "ARRAY,MASK,?VECTOR", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, parity, "ARRAY,?MASK", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, popcnt, "I", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, poppar, "I", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, precision, "X", I, simplify_precision) \
FORTRAN_GENERIC_INTRINSIC(NULL, present, "A", I, NULL) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, product, "ARRAY,DIM,?MASK", T, NULL, "ARRAY,?MASK", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, radix, "X", I, simplify_radix) \
FORTRAN_GENERIC_INTRINSIC(NULL, random_number, "HARVEST", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, random_seed, "?SIZE,?PUT,?GET", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, range, "X", I, simplify_range) \
FORTRAN_GENERIC_INTRINSIC(NULL, real, "A,?KIND", E, simplify_real) \
FORTRAN_GENERIC_INTRINSIC(NULL, float, "A", E, simplify_float) \
FORTRAN_GENERIC_INTRINSIC(NULL, repeat, "STRING,NCOPIES", T, simplify_repeat) \
FORTRAN_GENERIC_INTRINSIC(NULL, reshape, "SOURCE,SHAPE,?PAD,?ORDER", T, simplify_reshape) \
FORTRAN_GENERIC_INTRINSIC(NULL, rrspacing, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, same_type_as, "A,B", I, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, scale, "X,I", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, scan, "STRING,SET,?BACK,?KIND", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, selected_char_kind, "NAME", T, simplify_selected_char_kind) \
FORTRAN_GENERIC_INTRINSIC(NULL, selected_int_kind, "R", T, simplify_selected_int_kind) \
FORTRAN_GENERIC_INTRINSIC(NULL, selected_real_kind, "?P,?R,?RADIX", T, simplify_selected_real_kind) \
FORTRAN_GENERIC_INTRINSIC(NULL, set_exponent, "X,I", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, shape, "SOURCE,?KIND", I, simplify_shape) \
FORTRAN_GENERIC_INTRINSIC(NULL, shifta, "I,SHIFT", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, shiftl, "I,SHIFT", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, shiftr, "I,SHIFT", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, sign, "A,B", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, sin, "X", E, simplify_sin) \
FORTRAN_GENERIC_INTRINSIC(NULL, sinh, "X", E, simplify_sinh) \
FORTRAN_GENERIC_INTRINSIC(NULL, size, "ARRAY,?DIM,?KIND", I, simplify_size) \
FORTRAN_GENERIC_INTRINSIC(NULL, sizeof, NULL, I, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, spacing, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, spread, "SOURCE,DIM,NCOPIES", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, sqrt, "X", E, simplify_sqrt) \
FORTRAN_GENERIC_INTRINSIC(NULL, storage_size, "A,?KIND", I, NULL) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, sum, "ARRAY,DIM,?MASK", T, NULL, "ARRAY,?MASK", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, system_clock, "?COUNT,?COUNT_RATE,?COUNT_MAX", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, tan, "X", E, simplify_tan) \
FORTRAN_GENERIC_INTRINSIC(NULL, tanh, "X", E, simplify_tanh) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, this_image, "", T, NULL, "COARRAY,?DIM", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, tiny, "X", I, simplify_tiny) \
FORTRAN_GENERIC_INTRINSIC(NULL, trailz, "I", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, transfer, "SOURCE,MOLD,?SIZE", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, transpose, "MATRIX", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, trim, "STRING", T, simplify_trim) \
FORTRAN_GENERIC_INTRINSIC(NULL, ubound, "ARRAY,?DIM,?KIND", I, simplify_ubound) \
FORTRAN_GENERIC_INTRINSIC(NULL, ucobound, "COARRAY,?DIM,?KIND", I, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, unpack, "VECTOR,MASK,FIELD", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, verify, "STRING,SET,?BACK,?KIND", E, NULL)  \
FORTRAN_GENERIC_INTRINSIC(NULL, max0, NULL, E, simplify_max0) \
FORTRAN_GENERIC_INTRINSIC(NULL, max1, NULL, E, simplify_max1) \
FORTRAN_GENERIC_INTRINSIC(NULL, min0, NULL, E, simplify_min0) \
FORTRAN_GENERIC_INTRINSIC(NULL, min1, NULL, E, simplify_min1) \
FORTRAN_GENERIC_INTRINSIC(NULL, amax0, NULL, E, simplify_amax0) \
FORTRAN_GENERIC_INTRINSIC(NULL, amax1, NULL, E, simplify_amax1) \
FORTRAN_GENERIC_INTRINSIC(NULL, amin0, NULL, E, simplify_amin0) \
FORTRAN_GENERIC_INTRINSIC(NULL, amin1, NULL, E, simplify_amin1) \
FORTRAN_GENERIC_INTRINSIC(NULL, dmax1, NULL, E, simplify_dmax1) \
FORTRAN_GENERIC_INTRINSIC(NULL, dmin1, NULL, E, simplify_dmin1) \
\
FORTRAN_GENERIC_INTRINSIC(NULL, abort, "", S, NULL)  \
FORTRAN_GENERIC_INTRINSIC(NULL, access, "NAME,MODE", I, NULL)  \
FORTRAN_GENERIC_INTRINSIC(NULL, and, "I,J", E, NULL)  \
FORTRAN_GENERIC_INTRINSIC(NULL, besj0, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, besj1, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, besjn, "N,X", E, NULL, "N1,N2,X", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, besy0, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, besy1, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC_2(NULL, besyn, "N,X", E, NULL, "N1,N2,X", T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, chdir, NULL, M, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, dfloat, "A", E, simplify_float) \
FORTRAN_GENERIC_INTRINSIC(NULL, etime, NULL, M, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, exit,  "?STATUS", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, fdate, NULL, M, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, fgetc, NULL, M, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, free, "PTR", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, fseek, "UNIT,OFFSET,WHENCE,?STATUS", S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, ftell, NULL, M, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, getarg, NULL, S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, getcwd, NULL, M, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, getlog, NULL, S, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, hostnm, NULL, M, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, isnan, "X", E, NULL)  \
FORTRAN_GENERIC_INTRINSIC(NULL, loc, NULL, E, NULL)  \
FORTRAN_GENERIC_INTRINSIC(NULL, lshift, "I,SHIFT", E, NULL)  \
FORTRAN_GENERIC_INTRINSIC(NULL, or, "I,J", E, NULL)  \
FORTRAN_GENERIC_INTRINSIC(NULL, rshift, "I,SHIFT", E, NULL)  \
FORTRAN_GENERIC_INTRINSIC(NULL, sleep, "SECONDS", S, NULL)  \
FORTRAN_GENERIC_INTRINSIC(NULL, system, NULL, M, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, time, NULL, T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, time8, NULL, T, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, xor, "I,J", E, NULL)  \
\
FORTRAN_GENERIC_INTRINSIC(NULL, sind, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, cosd, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, tand, "X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, atan2d, "Y,X", E, NULL) \
FORTRAN_GENERIC_INTRINSIC(NULL, imag, "Z", E, simplify_aimag) \
FORTRAN_GENERIC_INTRINSIC(NULL, imagpart, "Z", E, simplify_aimag) \
ISO_C_BINDING_INTRINSICS \
IEEE_EXCEPTIONS_INTRINSICS \
MERCURIUM_SPECIFIC_INTRINSICS

#define ISO_C_BINDING_INTRINSICS \
  FORTRAN_GENERIC_INTRINSIC("iso_c_binding", c_associated, "C_PTR_1,?C_PTR_2", S, NULL) \
  FORTRAN_GENERIC_INTRINSIC("iso_c_binding", c_f_pointer, "CPTR,FPTR,?SHAPE", S, NULL) \
  FORTRAN_GENERIC_INTRINSIC("iso_c_binding", c_funloc, NULL, S, NULL) \
  FORTRAN_GENERIC_INTRINSIC("iso_c_binding", c_loc, NULL, S, NULL) \
  FORTRAN_GENERIC_INTRINSIC("iso_c_binding", c_sizeof, "X", S, NULL)

#define MERCURIUM_SPECIFIC_INTRINSICS \
  FORTRAN_GENERIC_INTRINSIC(NULL, mercurium_loc, "X", I, simplify_mcc_loc) \
  FORTRAN_GENERIC_INTRINSIC(NULL, mercurium_null, "", I, simplify_mcc_null) \
  FORTRAN_GENERIC_INTRINSIC(NULL, ompss_opencl_allocate, "ARR", S, NULL)\
  FORTRAN_GENERIC_INTRINSIC(NULL, ompss_opencl_deallocate, "ARR", S, NULL)

#define IEEE_EXCEPTIONS_INTRINSICS \
  FORTRAN_GENERIC_INTRINSIC("ieee_exceptions", ieee_support_flag, "FLAG,?X", I, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_exceptions", ieee_support_halting, "FLAG", I, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_support_datatype, "?X", I, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_support_denormal, "?X", I, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_support_divide, "?X", I, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_support_inf, "?X", I, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_support_nan, "?X", I, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_support_rounding, "ROUND_VALUE,?X", I, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_support_sqrt, "?X", I, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_support_standard, "?X", I, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_class, "X", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_copy_sign, "X,Y", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_is_finite, "X", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_is_nan, "X", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_is_negative, "X", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_is_normal, "X", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_logb, "X", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_is_next_after, "X,Y", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_rem, "X,Y", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_rint, "X", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_scalb, "X,I", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_unordered, "X,Y", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_value, "X,CLASS", E, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_exceptions", ieee_get_flag, "FLAG,FLAG_VALUE", ES, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_exceptions", ieee_get_halting_mode, "FLAG,HALTING", ES, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_exceptions", ieee_set_flag, "FLAG,FLAG_VALUE", ES, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_exceptions", ieee_set_halting_mode, "FLAG,HALTING", ES, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_exceptions", ieee_get_status, "STATUS_VALUE", S, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_exceptions", ieee_set_status, "STATUS_VALUE", S, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_get_rounding_mode, "ROUND_VALUE", S, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_set_rounding_mode, "ROUND_VALUE", S, NULL) \
  FORTRAN_GENERIC_INTRINSIC("ieee_arithmetic", ieee_selected_real_kind, "?P,?R", T, NULL) \

// Well, for some reason C_LOC and C_FUNLOC do not have a known interface in gfortran
#if 0
FORTRAN_GENERIC_INTRINSIC("iso_c_binding", c_funloc, "X", S, NULL)
FORTRAN_GENERIC_INTRINSIC("iso_c_binding", c_loc, "X", S, NULL)
#endif


#define MAX_KEYWORDS_INTRINSICS 10

typedef struct intrinsic_variant_info_tag
{
    int num_keywords;
    const char* keyword_names[MAX_KEYWORDS_INTRINSICS];
    char is_optional[MAX_KEYWORDS_INTRINSICS];
} intrinsic_variant_info_t;

static intrinsic_variant_info_t get_variant(const char* keywords)
{
    int keyword_index = 0;
    intrinsic_variant_info_t result;
    memset(&result, 0, sizeof(result));
    if (keywords != NULL)
    {
        char *c = xstrdup(keywords);
        char *p = strtok(c, ",");
        while (p != NULL)
        {
            ERROR_CONDITION(keyword_index == MAX_KEYWORDS_INTRINSICS, 
                    "Too many keywords for intrinsic!\n", 0);

            char is_optional = (*p == '?');
            if (is_optional)
                p++;

            result.keyword_names[keyword_index] = uniquestr(strtolower(p));
            result.is_optional[keyword_index] = is_optional;

            p = strtok(NULL, ",");
            keyword_index++;
        }
        result.num_keywords = keyword_index;
        DELETE(c);
    }
    else
    {
        // Special case, we allow anything
        result.num_keywords = -1;
    }

    return result;
}

static void update_keywords_of_intrinsic(scope_entry_t* entry, const char* keywords, int num_actual_arguments)
{
    intrinsic_variant_info_t current_variant = get_variant(keywords);

    if (symbol_entity_specs_get_num_parameters(entry) != 0)
    {
        // Already updated
        return;
    }

    if (current_variant.num_keywords == 0)
    {
        // No arguments at all
    }
    else if (current_variant.num_keywords < 0)
    {
        // Unbounded arguments (like MAX and MIN)
        int i;
        for (i = 0; i < num_actual_arguments; i++)
        {
            scope_entry_t* new_keyword_sym = NEW0(scope_entry_t);
            new_keyword_sym->kind = SK_VARIABLE;
            uniquestr_sprintf(&new_keyword_sym->symbol_name, "A%d", (i+1));
            new_keyword_sym->decl_context = entry->decl_context;
            new_keyword_sym->type_information = function_type_get_parameter_type_num(entry->type_information, i);

            symbol_entity_specs_set_is_optional(new_keyword_sym, current_variant.is_optional[i]);
            new_keyword_sym->do_not_print = 1;

            symbol_set_as_parameter_of_function(new_keyword_sym, 
                    entry,
                    /* nesting */ 0,
                    /* position */ symbol_entity_specs_get_num_related_symbols(entry));

            symbol_entity_specs_add_related_symbols(entry, new_keyword_sym);
        }
    }
    else // current_variant.num_keywords > 0
    {
        // Usual case with a bounded set of keywords
        int i;
        for (i = 0; i < current_variant.num_keywords; i++)
        {
            scope_entry_t* new_keyword_sym = NEW0(scope_entry_t);
            new_keyword_sym->kind = SK_VARIABLE;
            new_keyword_sym->symbol_name = uniquestr(current_variant.keyword_names[i]);
            new_keyword_sym->decl_context = entry->decl_context;
            new_keyword_sym->type_information = function_type_get_parameter_type_num(entry->type_information, i);

            symbol_set_as_parameter_of_function(new_keyword_sym, entry,
                    /* nesting */ 0,
                    /* position */ symbol_entity_specs_get_num_related_symbols(entry));

            symbol_entity_specs_set_is_optional(new_keyword_sym, current_variant.is_optional[i]);

            symbol_entity_specs_add_related_symbols(entry,
                    new_keyword_sym);
        }
    }
}

static char generic_keyword_check(
        scope_entry_t* symbol,
        int *num_arguments,
        nodecl_t *argument_expressions,
        const char* keywords,
        type_t** reordered_types,
        nodecl_t* reordered_exprs)
{
    intrinsic_variant_info_t current_variant = get_variant(keywords);
    DEBUG_CODE()
    {
        fprintf(stderr, "INTRINSIC: Checking intrinsic '%s'\n",
                symbol->symbol_name);
        fprintf(stderr, "INTRINSICS: Keywords of this intrinsic\n");
        int i;
        for (i = 0; i < current_variant.num_keywords; i++)
        {
            fprintf(stderr, "INTRINSICS:     %-10s   %s\n",
                    strtoupper(current_variant.keyword_names[i]),
                    current_variant.is_optional[i] ? "[OPTIONAL]" : "");
        }
        if (current_variant.num_keywords < 0)
        {
            fprintf(stderr, "INTRINSICS:     unlimited keyword list\n");
        }
    }

    if (*num_arguments == 0)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "INTRINSICS: Invocation to intrinsic '%s' succeeds trivially because there are no expressions given\n",
                    symbol->symbol_name);
        }
        return 1;
    }

    if (current_variant.num_keywords < 0)
    {
        int i;
        for (i = 0; i < (*num_arguments); i++)
        {
            ERROR_CONDITION(!nodecl_is_null(argument_expressions[i])
                    && nodecl_get_kind(argument_expressions[i]) != NODECL_FORTRAN_ACTUAL_ARGUMENT,
                    "This node must be a NODECL_FORTRAN_ACTUAL_ARGUMENT", 0);

            nodecl_t expr = nodecl_get_child(argument_expressions[i], 0);

            reordered_types[i] = nodecl_get_type(expr);
            reordered_exprs[i] = expr;
        }
        DEBUG_CODE()
        {
            fprintf(stderr, "INTRINSICS: Invocation to intrinsic '%s' succeeds trivially because it is an unbounded parameter function\n",
                    symbol->symbol_name);
        }
        return 1;
    }

    char ok = 1;
    int i;
    int position = 0;
    char seen_keywords = 0;
    for (i = 0; i < (*num_arguments) && ok; i++)
    {
        ERROR_CONDITION(!nodecl_is_null(argument_expressions[i])
                && nodecl_get_kind(argument_expressions[i]) != NODECL_FORTRAN_ACTUAL_ARGUMENT,
                "This node must be a NODECL_FORTRAN_ACTUAL_ARGUMENT", 0);

        const char* keyword = NULL;
        nodecl_t expr = nodecl_null();

        if (!nodecl_is_null(argument_expressions[i]))
        {
            keyword = nodecl_get_text(argument_expressions[i]);
            expr = nodecl_get_child(argument_expressions[i], 0);
        }

        if (keyword != NULL)
        {
            char found = 0;
            int j;
            for (j = 0; j < current_variant.num_keywords && !found; j++)
            {
                if (strcasecmp(current_variant.keyword_names[j], keyword) == 0)
                {
                    position = j;
                    found = 1;
                }
            }

            if (!found)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "INTRINSICS: Intrinsic '%s' does not have any keyword '%s'\n",
                            symbol->symbol_name,
                            keyword);
                }
                ok = 0;
                break;
            }
            seen_keywords = 1;
        }
        else
        {
            if (seen_keywords)
            {
                ok = 0;
                break;
            }

            if (position > current_variant.num_keywords)
            {
                ok = 0;
                DEBUG_CODE()
                {
                    fprintf(stderr, "INTRINSICS: Too many parameters (%d) for intrinsic '%s' (maximum is %d)\n",
                            position,
                            symbol->symbol_name,
                            current_variant.num_keywords);
                }
                break;
            }
        }
        if (nodecl_is_null(reordered_exprs[position]))
        {
            if (!nodecl_is_null(expr))
            {
                if (nodecl_is_err_expr(expr))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "INTRINSICS: Dummy argument '%s' of intrinsic '%s' is associated to an invalid expression\n",
                                current_variant.keyword_names[position],
                                symbol->symbol_name);
                    }
                    ok = 0;
                    break;
                };
                reordered_exprs[position] = expr;
                reordered_types[position] = nodecl_get_type(expr);
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "INTRINSICS: Dummy argument '%s' (position %d) of intrinsic '%s' already got an actual argument\n",
                        current_variant.keyword_names[position],
                        position,
                        symbol->symbol_name);
            }
            ok = 0;
            break;
        }

        position++;
    }

    // Now check every nonoptional dummy argument has a real argument
    int j;
    for (j = 0; j < current_variant.num_keywords && ok; j++)
    {
        if (nodecl_is_null(reordered_exprs[j])
                && !current_variant.is_optional[j])
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "INTRINSICS: No real argument given for nonoptional dummy argument '%s' of intrinsic '%s'\n",
                        current_variant.keyword_names[j],
                        symbol->symbol_name);
            }
            ok = 0;
            break;
        }
    }

    if (ok)
    {
        *num_arguments = current_variant.num_keywords;
        DEBUG_CODE()
        {
            fprintf(stderr, "INTRINSICS: Invocation to intrinsic '%s' seems fine\n",
                    symbol->symbol_name);
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "INTRINSICS: Invocation to intrinsic '%s' has failed\n",
                    symbol->symbol_name);
        }
    }

    return ok;
}


#define FORTRAN_GENERIC_INTRINSIC(module, name, keywords0, kind0, compute_code) \
static scope_entry_t* compute_intrinsic_##name(scope_entry_t* symbol, \
        type_t** argument_types, \
        nodecl_t *argument_expressions, \
        int num_arguments, \
        const_value_t** const_value); \
static scope_entry_t* keyword_compute_intrinsic_##name(scope_entry_t* symbol, \
            type_t** argument_types, \
            nodecl_t *argument_expressions, \
            int num_arguments, \
            const_value_t** const_value) \
{ \
    ERROR_CONDITION(argument_types != NULL, "This must be NULL as it is unused", 0); \
    type_t* reordered_types[MCXX_MAX_FUNCTION_CALL_ARGUMENTS] = { 0 }; \
    nodecl_t reordered_exprs[MCXX_MAX_FUNCTION_CALL_ARGUMENTS] = { nodecl_null() }; \
    int result = generic_keyword_check( \
            symbol, \
            &num_arguments, \
            argument_expressions, \
            keywords0, \
            reordered_types, \
            reordered_exprs); \
    if (result) \
    { \
        scope_entry_t* sym = compute_intrinsic_##name(symbol, reordered_types, reordered_exprs, num_arguments, const_value); \
        if (sym != NULL) \
        { \
            update_keywords_of_intrinsic(sym, keywords0, num_arguments); \
        } \
        return sym; \
    } \
    else \
    { \
        return NULL; \
    } \
}

#define FORTRAN_GENERIC_INTRINSIC_2(module, name, keywords0, kind0, compute_code0, keywords1, kind1, compute_code1) \
static scope_entry_t* compute_intrinsic_##name##_0(scope_entry_t* symbol,  \
        type_t** argument_types, \
        nodecl_t *argument_expressions, \
        int num_arguments, \
        const_value_t** const_value); \
static scope_entry_t* compute_intrinsic_##name##_1(scope_entry_t* symbol,  \
        type_t** argument_types, \
        nodecl_t *argument_expressions, \
        int num_arguments, \
        const_value_t** const_value); \
static scope_entry_t* keyword_compute_intrinsic_##name(scope_entry_t* symbol, \
            type_t** argument_types, \
            nodecl_t *argument_expressions, \
            int num_arguments, \
            const_value_t** const_value) \
{ \
    ERROR_CONDITION(argument_types != NULL, "This must be NULL as it is unused", 0); \
    type_t* reordered_types[MCXX_MAX_FUNCTION_CALL_ARGUMENTS] = { 0 }; \
    nodecl_t reordered_exprs[MCXX_MAX_FUNCTION_CALL_ARGUMENTS] = { nodecl_null() }; \
    int keep_nargs = num_arguments; \
    int result = generic_keyword_check( \
            symbol, \
            &num_arguments, \
            argument_expressions, \
            keywords0, \
            reordered_types, \
            reordered_exprs); \
    if (result) \
    { \
        scope_entry_t* sym = compute_intrinsic_##name##_0(symbol, reordered_types, reordered_exprs, num_arguments, const_value); \
        if (sym != NULL) \
        { \
            update_keywords_of_intrinsic(sym, keywords0, num_arguments); \
            return sym; \
        } \
    } \
    { \
        num_arguments = keep_nargs; \
        memset(reordered_types, 0, sizeof(reordered_types)); \
        memset(reordered_exprs, 0, sizeof(reordered_exprs)); \
        result = generic_keyword_check( \
                symbol, \
                &num_arguments, \
                argument_expressions, \
                keywords1, \
                reordered_types, \
                reordered_exprs); \
        if (result) \
        { \
            scope_entry_t* sym = compute_intrinsic_##name##_1(symbol, reordered_types, reordered_exprs, num_arguments, const_value); \
            if (sym != NULL) \
            { \
                update_keywords_of_intrinsic(sym, keywords1, num_arguments); \
            } \
            return sym; \
        } \
        else \
        { \
            return NULL; \
        } \
    }\
}

FORTRAN_INTRINSIC_GENERIC_LIST
#undef FORTRAN_GENERIC_INTRINSIC
#undef FORTRAN_GENERIC_INTRINSIC_2

// This is used when serializing
static struct intrinsic_name_pair_t
{
    computed_function_type_t fun;
} _intrinsic_names[] = {
    { NULL },
#define FORTRAN_GENERIC_INTRINSIC(module, name, keywords0, kind0, compute_code) \
    { keyword_compute_intrinsic_##name },
#define FORTRAN_GENERIC_INTRINSIC_2(module, name, keywords0, kind0, compute_code0, keywords1, kind1, compute_code1) \
    { keyword_compute_intrinsic_##name },
FORTRAN_INTRINSIC_GENERIC_LIST
#undef FORTRAN_GENERIC_INTRINSIC
#undef FORTRAN_GENERIC_INTRINSIC_2
};

int fortran_intrinsic_get_id(computed_function_type_t fun)
{
    int num_items = sizeof(_intrinsic_names) / sizeof(_intrinsic_names[0]);
    for (int i = 0; i < num_items; i++)
    {
        if (_intrinsic_names[i].fun == fun)
            return i;
    }

    return -1;
}

computed_function_type_t fortran_intrinsic_get_ptr(int id)
{
    int num_items = sizeof(_intrinsic_names) / sizeof(_intrinsic_names[0]);
    if (id >= num_items)
    {
        return NULL;
    }
    else
    {
        return _intrinsic_names[id].fun;
    }
}

// This is used when serializing
static struct intrinsic_simplify_fun_t
{
    simplify_function_t fun;
} _simplify_functions[] = {
    { NULL }, /* Reserve the first one for 0 */
#define FORTRAN_GENERIC_INTRINSIC(module, name, keywords0, kind0, compute_code) \
    { compute_code },
#define FORTRAN_GENERIC_INTRINSIC_2(module, name, keywords0, kind0, compute_code0, keywords1, kind1, compute_code1) \
    { compute_code0 },
FORTRAN_INTRINSIC_GENERIC_LIST
#undef FORTRAN_GENERIC_INTRINSIC
#undef FORTRAN_GENERIC_INTRINSIC_2
};

int fortran_simplify_function_get_id(simplify_function_t fun)
{
    int num_items = sizeof(_simplify_functions) / sizeof(_simplify_functions[0]);
    for (int i = 0; i < num_items; i++)
    {
        if (_simplify_functions[i].fun == fun)
            return i;
    }

    return -1;
}

simplify_function_t fortran_simplify_function_get_ptr(int id)
{
    int num_items = sizeof(_simplify_functions) / sizeof(_simplify_functions[0]);
    if (id >= num_items)
    {
        return NULL;
    }
    else
    {
        return _simplify_functions[id].fun;
    }
}

typedef
struct intrinsic_descr_tag
{
    scope_entry_t* generic_symbol;
    const char* name;
    type_t* result_type;
    int num_types;
    type_t** parameter_types;
} intrinsic_descr_t;

static int compare_types(type_t* t1, type_t* t2)
{
    if (equivalent_types(get_unqualified_type(t1), get_unqualified_type(t2)))
        return 0;
    else if (t1 < t2)
        return -1;
    else
        return 1;
}

static int intrinsic_descr_cmp(const void* i1, const void* i2)
{
    const intrinsic_descr_t* d1 = (const intrinsic_descr_t*)i1;
    const intrinsic_descr_t* d2 = (const intrinsic_descr_t*)i2;

    if (d1->generic_symbol != d2->generic_symbol)
    {
        if (d1->generic_symbol < d2->generic_symbol)
            return -1;
        else
            return 1;
    }

    int c;
    if ((c = strcasecmp(d1->name, d2->name)) != 0)
        return c;

    if (d1->result_type  == NULL
            && d1->result_type != d2->result_type)
        return -1;

    if (d2->result_type  == NULL
            && d1->result_type != d2->result_type)
        return 1;

    if (d1->result_type != NULL
            && d2->result_type != NULL
            && (c = compare_types(d1->result_type, d2->result_type)) != 0)
        return c;

    if (d1->num_types != d2->num_types)
    {
        if (d1->num_types < d2->num_types)
            return -1;
        else
            return 1;
    }

    int i;
    for (i = 0; i < d1->num_types; i++)
    {
        if ((c = compare_types(d1->parameter_types[i], d2->parameter_types[i])) != 0)
            return c;
    }

    return 0;
}

static char specific_keyword_check(
        scope_entry_t* symbol,
        int *num_arguments,
        nodecl_t *argument_expressions,
        nodecl_t* reordered_exprs)
{
    char ok = 1;
    int i;
    int position = 0;
    char seen_keywords = 0;
    for (i = 0; i < (*num_arguments) && ok; i++)
    {
        const char* keyword = NULL;
        nodecl_t expr = nodecl_null();

        if (!nodecl_is_null(argument_expressions[i]))
        {
            keyword = nodecl_get_text(argument_expressions[i]);
            expr = nodecl_get_child(argument_expressions[i], 0);
        }

        if (keyword != NULL)
        {
            char found = 0;
            int j;
            for (j = 0; j < symbol_entity_specs_get_num_related_symbols(symbol) && !found; j++)
            {
                if (strcasecmp(symbol_entity_specs_get_related_symbols_num(symbol, j)->symbol_name, keyword) == 0)
                {
                    position = j;
                    found = 1;
                }
            }

            if (!found)
            {
                // fprintf(stderr, "%s: warning: no keyword '%s' for intrinsic '%s'\n",
                //         ast_location(keyword),
                //         ASTText(keyword),
                //         intrinsic_info->intrinsic_name);
                DEBUG_CODE()
                {
                    fprintf(stderr, "INTRINSICS: Intrinsic '%s' does not have any keyword '%s'\n",
                            symbol->symbol_name,
                            keyword);
                }
                ok = 0;
                break;
            }
            seen_keywords = 1;
        }
        else
        {
            if (seen_keywords)
            {
                ok = 0;
                break;
            }

            if (position > symbol_entity_specs_get_num_related_symbols(symbol))
            {
                ok = 0;
                DEBUG_CODE()
                {
                    fprintf(stderr, "INTRINSICS: Too many parameters (%d) for intrinsic '%s' (maximum is %d)\n",
                            position,
                            symbol->symbol_name,
                            symbol_entity_specs_get_num_related_symbols(symbol));
                }
                break;
            }
        }
        if (nodecl_is_null(reordered_exprs[position]))
        {
            if (!nodecl_is_null(expr))
            {
                if (nodecl_is_err_expr(expr))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "INTRINSICS: Dummy argument '%s' of intrinsic '%s' is associated to an invalid expression\n",
                                symbol_entity_specs_get_related_symbols_num(symbol, position)->symbol_name,
                                symbol->symbol_name);
                    }
                    ok = 0;
                    break;
                }
                reordered_exprs[position] = expr;
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "INTRINSICS: Dummy argument '%s' (position %d) of intrinsic '%s' already got an actual argument\n",
                        symbol_entity_specs_get_related_symbols_num(symbol, position)->symbol_name,
                        position,
                        symbol->symbol_name);
            }
            ok = 0;
            break;
        }

        position++;
    }

    // Now check every nonoptional dummy argument has a real argument
    int j;
    for (j = 0; j < symbol_entity_specs_get_num_related_symbols(symbol) && ok; j++)
    {
        if (nodecl_is_null(reordered_exprs[j])
                && !symbol_entity_specs_get_is_optional(symbol_entity_specs_get_related_symbols_num(symbol, j)))
        {
            // fprintf(stderr, "%s: warning: no real argument given for dummy argument '%s' of intrinsic '%s'\n",
            //         ast_location(argument),
            //         current_variant->keyword_names[j],
            //         intrinsic_info->intrinsic_name);
            DEBUG_CODE()
            {
                fprintf(stderr, "INTRINSICS: No real argument given for nonoptional dummy argument '%s' of intrinsic '%s'\n",
                        symbol_entity_specs_get_related_symbols_num(symbol, position)->symbol_name,
                        symbol->symbol_name);
            }
            ok = 0;
            break;
        }
    }

    if (ok)
    {
        *num_arguments = symbol_entity_specs_get_num_related_symbols(symbol);
        DEBUG_CODE()
        {
            fprintf(stderr, "INTRINSICS: Invocation to intrinsic '%s' seems fine\n",
                    symbol->symbol_name);
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "INTRINSICS: Invocation to intrinsic '%s' has failed\n",
                    symbol->symbol_name);
        }
    }

    return ok;
}

// This function ensures that arrays are dimensionless
static type_t* fix_type_for_intrinsic_parameters(type_t* t)
{
    if (is_lvalue_reference_type(t))
    {
        return get_lvalue_reference_type(
                fix_type_for_intrinsic_parameters(reference_type_get_referenced_type(t))
                );
    }
    else if (fortran_is_array_type(t))
    {
        int n = fortran_get_rank_of_type(t);
        type_t* rank0 = fortran_get_rank0_type(t);

        return fortran_get_n_ranked_type(rank0, n, CURRENT_COMPILED_FILE->global_decl_context);
    }
    else
    {
        return t;
    }
}

static rb_red_blk_tree* intrinsic_map = NULL;

// Creates an intrinsic procedure.
//
// If generic_symbol is not NULL the newly created intrinsic is a specific
// intrinsic of a generic intrinsic name
//
// If generic_symbol is NULL it is just a specific intrinsic name (for those
// cases where there is only one specialization and it would be overkill to
// define it as a generic one)
//
// If generic_symbol is not NULL the name of the newly created symbol is taken
// from the name of generic_symbol, otherwise it is taken from the parameter 'name'.
//
//    This is needed for this case
//
//       MODULE MOO
//       END MODULE MOO
//
static scope_entry_t* get_intrinsic_symbol_(
        scope_entry_t* generic_symbol,
        const char* name,
        type_t* result_type,
        int num_types, type_t** types,
        const decl_context_t* decl_context,
        char is_elemental,
        char is_pure,
        char is_transformational UNUSED_PARAMETER,
        char is_inquiry UNUSED_PARAMETER)
{
    ERROR_CONDITION(result_type == NULL, "This should be void", 0);

    if (generic_symbol != NULL
            && generic_symbol->symbol_name != NULL)
        name = generic_symbol->symbol_name;

    int i;
    for (i = 0; i < num_types; i++)
    {
        types[i] = fix_type_for_intrinsic_parameters(types[i]);
    }

    intrinsic_descr_t descr;
    descr.generic_symbol = generic_symbol;
    descr.name = name;
    descr.result_type = result_type;
    descr.num_types = num_types;
    descr.parameter_types = types;

    rb_red_blk_node* n = rb_tree_query(intrinsic_map, &descr);
    if (n != NULL)
    {
        scope_entry_t* entry = (scope_entry_t*)rb_node_get_info(n);
        DEBUG_CODE()
        {
            fprintf(stderr, "INTRINSICS: Returning existing intrinsic '%s' of type '%s'\n",
                    entry->symbol_name,
                    print_type_str(entry->type_information, decl_context));
        }
        return entry;
    }
    else
    {
        // Create a new descriptor
        intrinsic_descr_t *p = NEW0(intrinsic_descr_t);
        p->name = name;
        p->result_type = result_type;
        p->num_types = num_types;
        if (num_types > 0)
        {
            p->parameter_types = NEW_VEC(type_t*, num_types);
            memcpy(p->parameter_types, types, num_types * sizeof(*p->parameter_types));
        }

        parameter_info_t param_info[num_types + 1];
        memset(param_info, 0, sizeof(param_info));
        for (i = 0; i < num_types; i++)
        {
            ERROR_CONDITION((types[i] == NULL), "Invalid description of builtin", 0);
            param_info[i].type_info = types[i];
        }
        type_t* function_type = get_new_function_type(result_type, param_info, num_types, REF_QUALIFIER_NONE);

        // We do not want it be signed in the scope
        scope_entry_t* new_entry = NEW0(scope_entry_t);
        if (generic_symbol != NULL)
        {
            new_entry->locus = generic_symbol->locus;
        }
        else
        {
            new_entry->locus = make_locus("(fortran-intrinsic)", 0, 0);
        }
        new_entry->symbol_name = uniquestr(name);
        new_entry->decl_context = new_program_unit_context(decl_context);
        new_entry->kind = SK_FUNCTION;
        new_entry->do_not_print = 1;
        new_entry->type_information = function_type;
        symbol_entity_specs_set_emission_template(new_entry, generic_symbol);

        if (generic_symbol != NULL)
            symbol_entity_specs_set_simplify_function(new_entry, symbol_entity_specs_get_simplify_function(generic_symbol));

        symbol_entity_specs_set_is_elemental(new_entry, is_elemental);
        symbol_entity_specs_set_is_pure(new_entry, (is_pure || is_elemental));
        symbol_entity_specs_set_is_global_hidden(new_entry, 1);
        symbol_entity_specs_set_is_builtin(new_entry, 1);

        // A specific symbol can't have both bits enabled. Only the generic one
        symbol_entity_specs_set_is_intrinsic_subroutine(new_entry, is_void_type(result_type));
        symbol_entity_specs_set_is_intrinsic_function(new_entry, !is_void_type(result_type));

        if (decl_context->current_scope->related_entry != NULL
                && decl_context->current_scope->related_entry->kind == SK_MODULE)
        {
            new_entry->decl_context->current_scope->related_entry =
                decl_context->current_scope->related_entry;
        }

        rb_tree_insert(intrinsic_map, p, new_entry);

        DEBUG_CODE()
        {
            fprintf(stderr, "INTRINSICS: Creating new intrinsic '%s' of type '%s'\n",
                    name,
                    print_type_str(function_type, new_entry->decl_context));
        }

        return new_entry;
    }
}

#define GET_INTRINSIC_ELEMENTAL(symbol, name, result, ...) \
    ({ \
     type_t* _types[] = { __VA_ARGS__ }; \
     const int _size = sizeof(_types) / sizeof(*_types) ; \
     get_intrinsic_symbol_(symbol, name, result, _size, _types, symbol->decl_context, \
         /* is_elemental */ 1, \
         /* is_pure */ 1, \
         /* is_transformational */ 0, \
         /* is_inquiry */ 0); \
     })

#define GET_INTRINSIC_INQUIRY(symbol, name, result, ...) \
    ({ \
     type_t* _types[] = { __VA_ARGS__ }; \
     const int _size = sizeof(_types) / sizeof(*_types) ; \
     get_intrinsic_symbol_(symbol, name, result, _size, _types, symbol->decl_context, \
         /* is_elemental */ 0, \
         /* is_pure */ 0, \
         /* is_transformational */ 0, \
         /* is_inquiry */ 1); \
     })

#define GET_INTRINSIC_PURE(symbol, name, result, ...) \
    ({ \
     type_t* _types[] = { __VA_ARGS__ }; \
     const int _size = sizeof(_types) / sizeof(*_types) ; \
     get_intrinsic_symbol_(symbol, name, result, _size, _types, symbol->decl_context, \
         /* is_elemental */ 0, \
         /* is_pure */ 1, \
         /* is_transformational */ 0, \
         /* is_inquiry */ 0); \
     })

#define GET_INTRINSIC_IMPURE(symbol, name, result, ...) \
    ({ \
     type_t* _types[] = { __VA_ARGS__ }; \
     const int _size = sizeof(_types) / sizeof(*_types) ; \
     get_intrinsic_symbol_(symbol, name, result, _size, _types, symbol->decl_context, \
         /* is_elemental */ 0, \
         /* is_pure */ 0, \
         /* is_transformational */ 0, \
         /* is_inquiry */ 0); \
     })

#define GET_INTRINSIC_TRANSFORMATIONAL(symbol, name, result, ...) \
    ({ \
     type_t* _types[] = { __VA_ARGS__ }; \
     const int _size = sizeof(_types) / sizeof(*_types) ; \
     get_intrinsic_symbol_(symbol, name, result, _size, _types, symbol->decl_context, \
         /* is_elemental */ 0, \
         /* is_pure */ 0, \
         /* is_transformational */ 1, \
         /* is_inquiry */ 0); \
     })

static void null_dtor_func(const void *v UNUSED_PARAMETER) { }

static void fortran_init_specific_names(const decl_context_t* decl_context);

static void fortran_create_scope_for_intrinsics(const decl_context_t* decl_context);
static void fortran_init_intrinsic_modules(const decl_context_t* decl_context);
static void fortran_finish_intrinsic_modules(const decl_context_t* decl_context);

static int pstrcasecmp(const char** a, const char** b)
{
    return strcasecmp(*a, *b);
}

static char intrinsic_has_been_disabled(const char* c)
{
    if (CURRENT_CONFIGURATION->num_disabled_intrinsics > 0) {
        return (bsearch(&c,
                    CURRENT_CONFIGURATION->disabled_intrinsics_list,
                    CURRENT_CONFIGURATION->num_disabled_intrinsics,
                    sizeof(const char*),
                    (int (*)(const void*, const void*))pstrcasecmp) != NULL);
    }
    return 0;
}

void fortran_init_intrinsics(const decl_context_t* decl_context)
{
    fortran_create_scope_for_intrinsics(decl_context);
    fortran_init_intrinsic_modules(decl_context);

    const decl_context_t* fortran_intrinsic_context = fortran_get_context_of_intrinsics(decl_context);

    if (CURRENT_CONFIGURATION->num_disabled_intrinsics > 0)
    {
        // Sort the list of disabled intrinsics lists
        qsort(CURRENT_CONFIGURATION->disabled_intrinsics_list,
                CURRENT_CONFIGURATION->num_disabled_intrinsics,
                sizeof(const char*),
                (int (*)(const void*, const void*))pstrcasecmp);
    }

#define FORTRAN_GENERIC_INTRINSIC(module_name, name, keywords0, kind0, compute_code) \
    { \
        const decl_context_t* relevant_decl_context = fortran_intrinsic_context; \
        scope_entry_t* module_sym = NULL; \
        char disabled = 0; \
        if (module_name != NULL) \
        { \
            rb_red_blk_node* query = rb_tree_query(CURRENT_COMPILED_FILE->module_file_cache, module_name); \
            ERROR_CONDITION(query == NULL, "Module '%s' has not been registered", module_name); \
            module_sym = (scope_entry_t*)rb_node_get_info(query); \
            relevant_decl_context = module_sym->related_decl_context; \
        } \
        else { \
            disabled = intrinsic_has_been_disabled(#name); \
        } \
        if (!disabled) \
        { \
        scope_entry_t* new_intrinsic = new_symbol(relevant_decl_context, relevant_decl_context->current_scope, uniquestr(#name)); \
        new_intrinsic->locus = make_locus("(fortran-intrinsic)", 0, 0); \
        new_intrinsic->kind = SK_FUNCTION; \
        new_intrinsic->do_not_print = 1; \
        new_intrinsic->type_information = get_computed_function_type(keyword_compute_intrinsic_##name); \
        symbol_entity_specs_set_is_global_hidden(new_intrinsic, (module_sym == NULL)); \
        symbol_entity_specs_set_is_builtin(new_intrinsic, 1); \
        symbol_entity_specs_set_is_intrinsic_function(new_intrinsic, 1); \
        if (kind0 == ES || kind0 == PS || kind0 == S) \
        { \
            symbol_entity_specs_set_is_intrinsic_function(new_intrinsic, 0); \
            symbol_entity_specs_set_is_intrinsic_subroutine(new_intrinsic, 1); \
        } \
        else if (kind0 == M) \
        { \
            symbol_entity_specs_set_is_intrinsic_function(new_intrinsic, 1); \
            symbol_entity_specs_set_is_intrinsic_subroutine(new_intrinsic, 1); \
        } \
        symbol_entity_specs_set_simplify_function(new_intrinsic, compute_code); \
        if (module_sym != NULL) \
        { \
            new_intrinsic->locus = module_sym->locus; \
            symbol_entity_specs_set_in_module(new_intrinsic, module_sym); \
            symbol_entity_specs_set_is_module_procedure(new_intrinsic, 1); \
            symbol_entity_specs_add_related_symbols(module_sym, \
                    new_intrinsic); \
        } \
        } \
    }

#define FORTRAN_GENERIC_INTRINSIC_2(module_name, name, keywords0, kind0, compute_code0, keywords1, kind1, compute_code1) \
    FORTRAN_GENERIC_INTRINSIC(module_name, name, keywords0, kind0, compute_code0)

    FORTRAN_INTRINSIC_GENERIC_LIST
#undef FORTRAN_GENERIC_INTRINSIC
#undef FORTRAN_GENERIC_INTRINSIC_2

    intrinsic_map = rb_tree_create(intrinsic_descr_cmp, null_dtor_func, null_dtor_func);

    // Sign in specific names for intrinsics
    fortran_init_specific_names(fortran_intrinsic_context);

    fortran_finish_intrinsic_modules(decl_context);
}

void copy_intrinsic_function_info(scope_entry_t* entry, scope_entry_t* intrinsic)
{
    entry->kind = intrinsic->kind;
    entry->type_information = intrinsic->type_information;

#define COPY_SPEC(x) \
    symbol_entity_specs_set_##x(entry, symbol_entity_specs_get_##x(intrinsic))

    COPY_SPEC(is_global_hidden);
    COPY_SPEC(is_builtin);
    COPY_SPEC(is_intrinsic_function);
    COPY_SPEC(is_intrinsic_subroutine);
    COPY_SPEC(simplify_function);

    symbol_entity_specs_free_related_symbols(entry);

    // Update the keywords
    int i;
    for (i = 0; i < symbol_entity_specs_get_num_related_symbols(intrinsic); i++)
    {
        scope_entry_t* dummy_arg = symbol_entity_specs_get_related_symbols_num(intrinsic, i);

        scope_entry_t* new_keyword_sym = NEW0(scope_entry_t);
        new_keyword_sym->kind = SK_VARIABLE;
        new_keyword_sym->symbol_name = uniquestr(dummy_arg->symbol_name);
        new_keyword_sym->decl_context = entry->decl_context;
        new_keyword_sym->type_information = dummy_arg->type_information;

        symbol_set_as_parameter_of_function(new_keyword_sym, entry,
                /* nesting */ 0,
                /* position */ symbol_entity_specs_get_num_related_symbols(entry));

        symbol_entity_specs_set_is_optional(new_keyword_sym, symbol_entity_specs_get_is_optional(dummy_arg));

        symbol_entity_specs_add_related_symbols(entry,
                new_keyword_sym);
    }
}

// Only called when registering specific names of generic intrinsics
// i.e. ccos as a specific name of cos
static scope_entry_t* register_specific_intrinsic_name(
        const decl_context_t* decl_context,
        const char *generic_name,
        const char *specific_name,
        int num_args,
        type_t* t0, type_t* t1, type_t* t2, type_t* t3, type_t* t4, type_t* t5, type_t* t6)
{
    if (intrinsic_has_been_disabled(generic_name)
            || intrinsic_has_been_disabled(specific_name))
        return NULL;

#define MAX_SPECIFIC_PARAMETERS 7
    scope_entry_t* generic_entry = fortran_query_intrinsic_name_str(decl_context, generic_name);
    ERROR_CONDITION(generic_entry == NULL
            || !symbol_entity_specs_get_is_builtin(generic_entry), "Invalid symbol when registering specific intrinsic name\n", 0);

    ERROR_CONDITION(num_args > MAX_SPECIFIC_PARAMETERS, "Too many arguments", 0);

    // Sanity check with the input types
    //
    // We want them to be a (possibly empty) sequence of non-null types
    // followed by a (possibly empty) sequence of null types
    type_t* max_spec_param_types[MAX_SPECIFIC_PARAMETERS] = {t0, t1, t2, t3, t4, t5, t6};
    int i;
    char seen_null = 0;
    for (i = 0; i < MAX_SPECIFIC_PARAMETERS; i++)
    {
        ERROR_CONDITION(seen_null && max_spec_param_types[i] != NULL, "Invalid inputs to the function", 0);
        seen_null = (max_spec_param_types[i] == NULL);
    }

    nodecl_t nodecl_actual_arguments[MAX_SPECIFIC_PARAMETERS] = { 
        t0 != NULL ? nodecl_make_fortran_actual_argument(nodecl_make_dummy(t0, make_locus("", 0, 0)), make_locus("", 0, 0)) : nodecl_null(),
        t1 != NULL ? nodecl_make_fortran_actual_argument(nodecl_make_dummy(t1, make_locus("", 0, 0)), make_locus("", 0, 0)) : nodecl_null(),
        t2 != NULL ? nodecl_make_fortran_actual_argument(nodecl_make_dummy(t2, make_locus("", 0, 0)), make_locus("", 0, 0)) : nodecl_null(),
        t3 != NULL ? nodecl_make_fortran_actual_argument(nodecl_make_dummy(t3, make_locus("", 0, 0)), make_locus("", 0, 0)) : nodecl_null(),
        t4 != NULL ? nodecl_make_fortran_actual_argument(nodecl_make_dummy(t4, make_locus("", 0, 0)), make_locus("", 0, 0)) : nodecl_null(),
        t5 != NULL ? nodecl_make_fortran_actual_argument(nodecl_make_dummy(t5, make_locus("", 0, 0)), make_locus("", 0, 0)) : nodecl_null(),
        t6 != NULL ? nodecl_make_fortran_actual_argument(nodecl_make_dummy(t6, make_locus("", 0, 0)), make_locus("", 0, 0)) : nodecl_null() 
    };

    scope_entry_t* specific_entry = fortran_solve_generic_intrinsic_call(generic_entry, 
            nodecl_actual_arguments,
            num_args, /* is call */ 0);
    
    ERROR_CONDITION(specific_entry == NULL, "No specific symbol is possible when registering specific intrinsic name '%s' of generic intrinsic '%s'\n", 
            specific_name,
            generic_name);

    if (strcasecmp(generic_name, specific_name) == 0)
    {
        // If the name is the same, mark it as the specific interface of this intrinsic
        symbol_entity_specs_set_specific_intrinsic(generic_entry, specific_entry);
    }
    else
    {
        // If the name is different, create a new symbol with the type of the specific one
        type_t* specific_type = specific_entry->type_information;

        type_t* result_type = function_type_get_return_type(specific_type);

        type_t* param_types[MAX_SPECIFIC_PARAMETERS];
        int N = function_type_get_num_parameters(specific_type);
        ERROR_CONDITION(N > MAX_SPECIFIC_PARAMETERS, "Too many parameters", 0);
        for (i = 0; i < MAX_SPECIFIC_PARAMETERS && i < N; i++)
        {
            param_types[i] = function_type_get_parameter_type_num(specific_type, i);
        }

        int real_num_parameters = 0;
        // Add the keywords that are non null
        for (i = 0; i < MAX_SPECIFIC_PARAMETERS; i++)
        {
            if (nodecl_is_null(nodecl_actual_arguments[i]))
                break;
            real_num_parameters++;
        }

        scope_entry_t* new_specific_entry = get_intrinsic_symbol_(
                generic_entry,
                specific_name,
                result_type,
                real_num_parameters, param_types,
                generic_entry->decl_context,
                symbol_entity_specs_get_is_elemental(generic_entry),
                symbol_entity_specs_get_is_pure(generic_entry),
                /* is_transformational */ 0,
                /* is_inquiry */ 0);

        // Note that get_intrinsic_symbol_ uses the name in generic_entry
        // if it is not NULL, and here it will never be null, so overwrite
        // the name now
        new_specific_entry->symbol_name = uniquestr(specific_name);

        // Add the keywords that are non null
        for (i = 0; i < MAX_SPECIFIC_PARAMETERS; i++)
        {
            if (nodecl_is_null(nodecl_actual_arguments[i]))
                break;

            symbol_set_as_parameter_of_function(
                    symbol_entity_specs_get_related_symbols_num(specific_entry, i),
                    new_specific_entry,
                    /* nesting */ 0,
                    /* position */ symbol_entity_specs_get_num_related_symbols(new_specific_entry));

            symbol_entity_specs_add_related_symbols(new_specific_entry,
                    symbol_entity_specs_get_related_symbols_num(specific_entry, i));
        }

        insert_entry(generic_entry->decl_context->current_scope, new_specific_entry);
    }

    return specific_entry;
#undef MAX_SPECIFIC_PARAMETERS
}

// Only called when we register intrinsic names that are not strictly generic
// (so they do not have a generic symbol)
static scope_entry_t* register_custom_intrinsic(
        const decl_context_t* decl_context,
        const char* specific_name,
        type_t* result_type,
        int num_types,
        type_t* t0, type_t* t1, type_t* t2)
{
    if (intrinsic_has_been_disabled(specific_name))
        return NULL;

    type_t* types[3] = { t0, t1, t2 };

    scope_entry_t* entry = get_intrinsic_symbol_(NULL,
            specific_name,
            result_type,
            num_types, types,
            decl_context,
            0, 0, 0, 0);

    int i;
    for (i = 0; i < 3 && types[i] != NULL; i++)
    {
        scope_entry_t* new_keyword_sym = NEW0(scope_entry_t);
        new_keyword_sym->kind = SK_VARIABLE;
        uniquestr_sprintf(&new_keyword_sym->symbol_name, "A%d", (i+1));
        new_keyword_sym->decl_context = entry->decl_context;
        new_keyword_sym->type_information = types[i];

        symbol_set_as_parameter_of_function(new_keyword_sym, entry,
                /* nesting */ 0,
                /* position */ symbol_entity_specs_get_num_related_symbols(entry));

        symbol_entity_specs_add_related_symbols(entry,
                new_keyword_sym);
    }

    insert_alias(decl_context->current_scope, entry, specific_name);

    return entry;
}

#define REGISTER_SPECIFIC_INTRINSIC_0(_specific_name, _generic_name) \
    register_specific_intrinsic_name(decl_context, (_generic_name), (_specific_name), 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
#define REGISTER_SPECIFIC_INTRINSIC_1(_specific_name, _generic_name, t_0) \
    register_specific_intrinsic_name(decl_context, (_generic_name), (_specific_name), 1, (t_0), NULL, NULL, NULL, NULL, NULL, NULL)
#define REGISTER_SPECIFIC_INTRINSIC_2(_specific_name, _generic_name, t_0, t_1) \
    register_specific_intrinsic_name(decl_context, (_generic_name), (_specific_name), 2, (t_0), (t_1), NULL, NULL, NULL, NULL, NULL)

#define REGISTER_CUSTOM_INTRINSIC_0(_specific_name, result_type) \
    register_custom_intrinsic(decl_context, (_specific_name), result_type, 0, NULL, NULL, NULL)
#define REGISTER_CUSTOM_INTRINSIC_1(_specific_name, result_type, type_0) \
    register_custom_intrinsic(decl_context, (_specific_name), result_type, 1, type_0, NULL, NULL)
#define REGISTER_CUSTOM_INTRINSIC_2(_specific_name, result_type, type_0, type_1) \
    register_custom_intrinsic(decl_context, (_specific_name), result_type, 2, type_0, type_1, NULL)
#define REGISTER_CUSTOM_INTRINSIC_3(_specific_name, result_type, type_0, type_1, type_2) \
    register_custom_intrinsic(decl_context, (_specific_name), result_type, 3, type_0, type_1, type_2)

static void fortran_init_specific_names(const decl_context_t* decl_context)
{
    type_t* default_char = get_array_type(fortran_get_default_character_type(), nodecl_null(), decl_context);

    REGISTER_SPECIFIC_INTRINSIC_1("abs", "abs", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("acos", "acos", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("aimag", "aimag", get_complex_type(fortran_get_default_real_type()));
    REGISTER_SPECIFIC_INTRINSIC_2("aint", "aint", fortran_get_default_real_type(), NULL);
    REGISTER_SPECIFIC_INTRINSIC_1("alog", "log", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("alog10", "log10", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_2("amod", "mod", fortran_get_default_real_type(), fortran_get_default_real_type());
    // 'amax0' 'amax1' 'amin0' 'amin1' are defined as generic intrinsics due to their non-fortranish nature of unbounded number of parameters
    REGISTER_SPECIFIC_INTRINSIC_2("anint", "anint", fortran_get_default_real_type(), NULL);
    REGISTER_SPECIFIC_INTRINSIC_1("asin", "asin", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("atan", "atan", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_2("atan2", "atan2", fortran_get_default_real_type(), fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("cabs", "abs", get_complex_type(fortran_get_default_real_type()));
    REGISTER_SPECIFIC_INTRINSIC_1("ccos", "cos", get_complex_type(fortran_get_default_real_type()));
    REGISTER_SPECIFIC_INTRINSIC_1("cdcos", "cos", get_complex_type(fortran_get_doubleprecision_type()));
    REGISTER_SPECIFIC_INTRINSIC_1("cexp", "exp", get_complex_type(fortran_get_default_real_type()));
    REGISTER_SPECIFIC_INTRINSIC_2("char", "char", fortran_get_default_integer_type(), NULL);
    REGISTER_SPECIFIC_INTRINSIC_1("clog", "log", get_complex_type(fortran_get_default_real_type()));
    REGISTER_SPECIFIC_INTRINSIC_1("conjg", "conjg", get_complex_type(fortran_get_default_real_type()));
    REGISTER_SPECIFIC_INTRINSIC_1("cos", "cos", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("cosh", "cosh", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("csin", "sin", get_complex_type(fortran_get_default_real_type()));
    REGISTER_SPECIFIC_INTRINSIC_1("csqrt", "sqrt", get_complex_type(fortran_get_default_real_type()));
    REGISTER_SPECIFIC_INTRINSIC_1("dabs", "abs", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dacos", "cos", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dasin", "asin", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("datan", "atan", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_2("datan2", "atan2", fortran_get_doubleprecision_type(), fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dcos", "cos", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dcosh", "cosh", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_2("ddim", "dim", fortran_get_doubleprecision_type(), fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dexp", "exp", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_2("dim", "dim", fortran_get_default_real_type(), fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_2("dint", "aint", fortran_get_doubleprecision_type(), NULL);
    REGISTER_SPECIFIC_INTRINSIC_1("dlog", "log", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dlog10", "log10", fortran_get_doubleprecision_type());
    // dmax1 dmin1 are defined as generic intrinsics
    REGISTER_SPECIFIC_INTRINSIC_2("dmod", "mod", fortran_get_doubleprecision_type(), fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_2("dnint", "anint", fortran_get_doubleprecision_type(), NULL);
    REGISTER_SPECIFIC_INTRINSIC_2("dprod", "dprod", fortran_get_default_real_type(), fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_2("dreal", "real", get_complex_type(fortran_get_doubleprecision_type()), NULL);
    REGISTER_SPECIFIC_INTRINSIC_2("dsign", "sign", fortran_get_doubleprecision_type(), fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dsin", "sin", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dsinh", "sinh", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dsqrt", "sqrt", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dtan", "tan", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dtanh", "tanh", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("exp", "exp", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("iabs", "abs", fortran_get_default_integer_type());
    REGISTER_SPECIFIC_INTRINSIC_2("ichar", "ichar", default_char, NULL);
    REGISTER_SPECIFIC_INTRINSIC_2("idim", "dim", fortran_get_default_integer_type(), fortran_get_default_integer_type());
    REGISTER_SPECIFIC_INTRINSIC_2("idint", "int", fortran_get_doubleprecision_type(), NULL);
    REGISTER_SPECIFIC_INTRINSIC_2("idnint", "nint", fortran_get_doubleprecision_type(), NULL);
    REGISTER_SPECIFIC_INTRINSIC_2("ifix", "int", fortran_get_default_real_type(), NULL);
    REGISTER_SPECIFIC_INTRINSIC_2("index", "index", default_char, default_char);
    REGISTER_SPECIFIC_INTRINSIC_2("int", "int", fortran_get_default_integer_type(), NULL);
    REGISTER_SPECIFIC_INTRINSIC_2("isign", "sign", fortran_get_default_integer_type(), fortran_get_default_integer_type());
    REGISTER_SPECIFIC_INTRINSIC_2("len", "len", default_char, NULL);
    REGISTER_SPECIFIC_INTRINSIC_2("lge", "lge", default_char, default_char);
    REGISTER_SPECIFIC_INTRINSIC_2("lgt", "lgt", default_char, default_char);
    REGISTER_SPECIFIC_INTRINSIC_2("lle", "lle", default_char, default_char);
    REGISTER_SPECIFIC_INTRINSIC_2("llt", "llt", default_char, default_char);
    REGISTER_SPECIFIC_INTRINSIC_2("mod", "mod", fortran_get_default_integer_type(), fortran_get_default_integer_type());
    REGISTER_SPECIFIC_INTRINSIC_2("nint", "nint", fortran_get_default_real_type(), NULL);
    REGISTER_SPECIFIC_INTRINSIC_2("real", "real", fortran_get_default_integer_type(), NULL);
    REGISTER_SPECIFIC_INTRINSIC_2("sign", "sign", fortran_get_default_real_type(), fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("sin", "sin", fortran_get_default_real_type());
    //REGISTER_SPECIFIC_INTRINSIC_2("sngl", "real", fortran_get_doubleprecision_type(), NULL);
    REGISTER_SPECIFIC_INTRINSIC_1("sqrt", "sqrt", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("tan", "tan", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("tanh", "tanh", fortran_get_default_real_type());

    REGISTER_SPECIFIC_INTRINSIC_1("sind", "sind", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dsind", "sind", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("csind", "sind", get_complex_type(fortran_get_default_real_type()));

    REGISTER_SPECIFIC_INTRINSIC_1("cosd", "cosd", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dcosd", "cosd", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("ccosd", "cosd", get_complex_type(fortran_get_default_real_type()));

    REGISTER_SPECIFIC_INTRINSIC_1("tand", "tand", fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_1("dtand", "tand", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("ctand", "tand", get_complex_type(fortran_get_default_real_type()));

    REGISTER_SPECIFIC_INTRINSIC_2("atan2d", "atan2d", fortran_get_default_real_type(), fortran_get_default_real_type());
    REGISTER_SPECIFIC_INTRINSIC_2("datan2d", "atan2d", fortran_get_doubleprecision_type(), fortran_get_doubleprecision_type());

    // Non standard stuff
    // Very old (normally from g77) intrinsics
    REGISTER_SPECIFIC_INTRINSIC_1("cdabs", "abs", get_complex_type(fortran_get_doubleprecision_type()));
    REGISTER_SPECIFIC_INTRINSIC_1("zabs", "abs", get_complex_type(fortran_get_doubleprecision_type()));
    REGISTER_SPECIFIC_INTRINSIC_1("dconjg", "conjg", get_complex_type(fortran_get_doubleprecision_type()));
    REGISTER_SPECIFIC_INTRINSIC_1("dimag", "aimag", get_complex_type(fortran_get_doubleprecision_type()));
    REGISTER_SPECIFIC_INTRINSIC_1("derf", "erf", fortran_get_doubleprecision_type());
    REGISTER_SPECIFIC_INTRINSIC_1("derfc", "erfc", fortran_get_doubleprecision_type());

    REGISTER_CUSTOM_INTRINSIC_2("getenv", get_void_type(), fortran_get_default_character_type(), 
            fortran_get_default_character_type());
    REGISTER_CUSTOM_INTRINSIC_1("sngl", fortran_get_default_real_type(), fortran_get_doubleprecision_type());
    REGISTER_CUSTOM_INTRINSIC_0("iargc", fortran_get_default_integer_type());
}

static type_t* no_ptr(type_t* t)
{
    if (t == NULL)
        return NULL;
    if (is_pointer_type(t))
        return pointer_type_get_pointee_type(t);
    return t;
}

scope_entry_t* compute_intrinsic_abs(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_integer_type(t0)
            || is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "abs", t0, lvalue_ref(t0));
    }
    else if (is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "abs", complex_type_get_base_type(t0), lvalue_ref(t0));
    }

    return NULL;
}

static char opt_valid_kind_expr(nodecl_t expr, int *val)
{
    if (nodecl_is_null(expr))
        return 1;

    if (!nodecl_is_constant(expr))
        return 0;

    int k = const_value_cast_to_4(nodecl_get_constant(expr));

    if (val != NULL)
        *val = k;

    return 1;
}

static scope_entry_t* compute_intrinsic_achar(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    int dc = 1;
    if (is_integer_type(t0)
            && opt_valid_kind_expr(argument_expressions[1], &dc))
    {
        // We ignore the character kind here
        return GET_INTRINSIC_ELEMENTAL(symbol, "achar", 
                get_array_type(get_char_type(), nodecl_null(), symbol->decl_context), 
                t0,
                fortran_get_default_integer_type());
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_acos(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "acos", t0, lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_acosh(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "acosh", t0, lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_adjustl(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (fortran_is_character_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "adjustl", t0, lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_adjustr(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (fortran_is_character_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "adjustr", t0, lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_aimag(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "aimag", complex_type_get_base_type(t0), lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_imag(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_aimag(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_imagpart(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_aimag(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_aint(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    int dr = fortran_get_default_real_type_kind();
    if (is_floating_type(t0)
            && (dr = type_get_size(t0))
            && opt_valid_kind_expr(argument_expressions[1], &dr))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "aint", 
                choose_float_type_from_kind(argument_expressions[1], dr), 
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_all(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    if (fortran_is_array_type(t0)
            && is_bool_type(fortran_get_rank0_type(t0))
            && (t1 == NULL || is_integer_type(t1)))
    {
        type_t* return_type = fortran_get_rank0_type(t0);
        if (t1 != NULL)
        {
            return_type = fortran_get_n_ranked_type(fortran_get_rank0_type(t0), fortran_get_rank_of_type(t0) - 1, 
                    symbol->decl_context);
        }

        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "all", return_type, lvalue_ref(t0),
                (t1 == NULL ? lvalue_ref(fortran_get_default_integer_type()) : lvalue_ref(t1)));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_allocated_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    scope_entry_t* entry = NULL;

    if (!nodecl_is_null(argument_expressions[0])
            && (entry = fortran_data_ref_get_symbol(argument_expressions[0])))
    {
        if (entry != NULL
                && fortran_is_array_type(entry->type_information) && symbol_entity_specs_get_is_allocatable(entry))
        {
            return GET_INTRINSIC_INQUIRY(symbol, "allocated", fortran_get_default_logical_type(), t0);
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_allocated_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    scope_entry_t* entry = NULL;

    if (!nodecl_is_null(argument_expressions[0])
            && (entry = nodecl_get_symbol(argument_expressions[0])))
    {
        if (entry != NULL
                && !fortran_is_array_type(entry->type_information)
                && symbol_entity_specs_get_is_allocatable(entry))
        {
            return GET_INTRINSIC_INQUIRY(symbol, "allocated", fortran_get_default_logical_type(), t0);
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_anint(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    int dr = fortran_get_default_real_type_kind();
    if (is_floating_type(t0)
            && (dr = type_get_size(t0))
            && opt_valid_kind_expr(argument_expressions[1], &dr))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "anint", 
                choose_float_type_from_kind(argument_expressions[1], dr), 
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_any(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    if (fortran_is_array_type(t0)
            && is_bool_type(fortran_get_rank0_type(t0))
            && (t1 == NULL || is_integer_type(t1)))
    {
        type_t* return_type = fortran_get_rank0_type(t0);
        if (t1 != NULL)
        {
            return_type = fortran_get_n_ranked_type(fortran_get_rank0_type(t0), fortran_get_rank_of_type(t0) - 1, 
                    symbol->decl_context);
        }

        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "any", return_type, lvalue_ref(t0),
                lvalue_ref(t1 == NULL ? fortran_get_default_integer_type() : t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_asin(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "asin", t0, lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_asinh(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "asinh", t0, lvalue_ref(t0));
    }
    return NULL;
}

static char entity_is_target(nodecl_t n)
{
    if (nodecl_get_kind(n) == NODECL_SYMBOL)
    {
        return symbol_entity_specs_get_is_target(nodecl_get_symbol(n));
    }
    else if (nodecl_get_kind(n) == NODECL_ARRAY_SUBSCRIPT)
    {
        return entity_is_target(nodecl_get_child(n, 0));
    }
    else if (nodecl_get_kind(n) == NODECL_CLASS_MEMBER_ACCESS)
    {
        return entity_is_target(nodecl_get_child(n, 1))
            || entity_is_target(nodecl_get_child(n, 0));
    }
    return 0;
}

scope_entry_t* compute_intrinsic_associated(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* ptr_type = NULL;
    if (!nodecl_is_null(argument_expressions[0])
            && is_pointer_type(ptr_type = no_ref(nodecl_get_type(argument_expressions[0]))))
    {
        if (nodecl_is_null(argument_expressions[1]))
        {
            return GET_INTRINSIC_INQUIRY(symbol, "associated",
                    fortran_get_default_logical_type(),
                    lvalue_ref(ptr_type),
                    lvalue_ref(ptr_type));
        }
        else
        {
            type_t* target_type = NULL;
            if ((target_type = no_ref(nodecl_get_type(argument_expressions[1])))
                    && is_pointer_type(target_type))
            {
                if (fortran_equivalent_tkr_types(pointer_type_get_pointee_type(ptr_type), pointer_type_get_pointee_type(target_type)))
                {
                    return GET_INTRINSIC_INQUIRY(symbol, "associated", 
                            fortran_get_default_logical_type(),
                            lvalue_ref(ptr_type),
                            lvalue_ref(target_type));
                }
            }
            // Associated to some target, this should be a lvalue to a target entity
            else if (entity_is_target(argument_expressions[1])
                    && (target_type = no_ref(nodecl_get_type(argument_expressions[1]))))
            {
                if (fortran_equivalent_tkr_types(pointer_type_get_pointee_type(ptr_type), target_type))
                {
                    return GET_INTRINSIC_INQUIRY(symbol, "associated",
                            fortran_get_default_logical_type(),
                            lvalue_ref(ptr_type),
                            lvalue_ref(target_type));
                }
            }
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_atan_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (num_arguments == 1)
    {
        if (is_floating_type(t0)
                || is_complex_type(t0))
        {
            return GET_INTRINSIC_ELEMENTAL(symbol, "atan", t0, t0);
        }
    }
    else if (num_arguments == 2)
    {
        // t0 == Y
        // t1 == X
        type_t* t1 = fortran_get_rank0_type(argument_types[1]);
        if (is_floating_type(t0)
                && equivalent_types(get_unqualified_type(t0), get_unqualified_type(t1)))
        {
            return GET_INTRINSIC_ELEMENTAL(symbol, "atan2", t1, t1, t0);
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_atan_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_atan_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_atan2(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_atan_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_atan2d(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    // t0 == Y
    // t1 == X
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);
    if (is_floating_type(t0)
            && equivalent_types(get_unqualified_type(t0), get_unqualified_type(t1)))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "atan2d", t1, t1, t0);
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_atanh(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "atanh", t0, lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_atomic_define(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    // Not supported
    return NULL;
}

scope_entry_t* compute_intrinsic_atomic_ref(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    // Not supported
    return NULL;
}

scope_entry_t* compute_intrinsic_bessel_j0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "bessel_j0", t0, lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_bessel_j1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "bessel_j1", t0, lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_bessel_jn_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments == 2)
    {
        type_t* t0 = fortran_get_rank0_type(argument_types[0]);
        type_t* t1 = fortran_get_rank0_type(argument_types[1]);
        if (is_integer_type(t0)
                && is_floating_type(t1))
        {
            return GET_INTRINSIC_ELEMENTAL(symbol, "bessel_jn", t1, lvalue_ref(t0), lvalue_ref(t1));
        }
    }
    else if (num_arguments == 3)
    {
        type_t* t0 = no_ptr(no_ref(argument_types[0]));
        type_t* t1 = no_ptr(no_ref(argument_types[1]));
        type_t* t2 = no_ptr(no_ref(argument_types[2]));

        if (is_integer_type(t0)
                && is_integer_type(t1)
                && is_floating_type(t2))
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "bessel_jn", t2, lvalue_ref(t0), lvalue_ref(t1), lvalue_ref(t2));
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_bessel_jn_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_bessel_jn_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_bessel_y0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "bessel_y0", t0, t0);
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_bessel_y1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "bessel_y1", t0, t0);
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_bessel_yn_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments == 2)
    {
        type_t* t0 = fortran_get_rank0_type(argument_types[0]);
        type_t* t1 = fortran_get_rank0_type(argument_types[1]);
        if (is_integer_type(t0)
                && is_floating_type(t1))
        {
            return GET_INTRINSIC_ELEMENTAL(symbol, "bessel_yn", t1, t0, t1);
        }
    }
    else if (num_arguments == 3)
    {
        type_t* t0 = no_ptr(no_ref(argument_types[0]));
        type_t* t1 = no_ptr(no_ref(argument_types[1]));
        type_t* t2 = no_ptr(no_ref(argument_types[2]));

        if (is_integer_type(t0)
                && is_integer_type(t1)
                && is_floating_type(t2))
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "bessel_yn", t2, t0, t1, t2);
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_bessel_yn_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_bessel_yn_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_bge(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "bge", fortran_get_default_logical_type(), lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_bgt(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "bgt", fortran_get_default_logical_type(), lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_ble(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ble", fortran_get_default_logical_type(), lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_blt(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "blt", fortran_get_default_logical_type(), lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_bit_size(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    if (is_integer_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "bit_size", fortran_get_default_logical_type(), lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_btest(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);
    if (is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "btest", fortran_get_default_logical_type(), lvalue_ref(t0), lvalue_ref(t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_ceiling(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    int dr = fortran_get_default_real_type_kind();
    if (is_floating_type(t0)
            && opt_valid_kind_expr(argument_expressions[1], &dr))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ceiling", 
                choose_int_type_from_kind(argument_expressions[1], dr), 
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_char(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    int di = fortran_get_default_integer_type_kind();
    if (is_integer_type(t0)
            && opt_valid_kind_expr(argument_expressions[1], &di))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "char", 
                get_array_type(get_char_type(), 
                    const_value_to_nodecl(
                        const_value_get_one(fortran_get_default_integer_type_kind(), /* signed */ 1)),
                    symbol->decl_context), 
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_cmplx(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = no_ref(argument_types[1] != NULL ? fortran_get_rank0_type(argument_types[1]) : NULL);
    int dr = fortran_get_default_real_type_kind();
    if (opt_valid_kind_expr(argument_expressions[2], &dr))
    {
        if ((is_floating_type(t0)
                    || is_complex_type(t0)
                    || is_integer_type(t0))
                && (t1 == NULL
                    || is_floating_type(t1)
                    || is_complex_type(t1)
                    || is_integer_type(t1)))
        {
            return GET_INTRINSIC_ELEMENTAL(symbol, "cmplx", 
                    get_complex_type(choose_float_type_from_kind(argument_expressions[2], dr)), 
                    lvalue_ref(t0),
                    lvalue_ref(t1 == NULL ? t0 : t1),
                    lvalue_ref(fortran_get_default_integer_type()));
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_dcmplx(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = no_ref(argument_types[1] != NULL ? fortran_get_rank0_type(argument_types[1]) : NULL);
    if ((is_floating_type(t0)
                || is_complex_type(t0)
                || is_integer_type(t0))
            && (t1 == NULL
                || is_floating_type(t1)
                || is_complex_type(t1)
                || is_integer_type(t1)))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "dcmplx", 
                get_complex_type(fortran_get_doubleprecision_type()),
                lvalue_ref(t0),
                lvalue_ref(t1 == NULL ? t0 : t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_command_argument_count(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return get_intrinsic_symbol_(symbol,
            "command_argument_count", fortran_get_default_integer_type(), 0, NULL, symbol->decl_context, 
            /* is_elemental */ 0, 
            /* is_pure */ 0, 
            /* is_transformational */ 1, 
            /* is_inquiry */ 0); 
}

scope_entry_t* compute_intrinsic_conjg(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "conjg", t0, lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_cos(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "cos", t0, lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_cosd(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "cosd", t0, lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_cosh(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "cosh", t0, lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_count(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    int di = fortran_get_default_integer_type_kind();
    if (fortran_is_array_type_or_pointer_to(t0)
            && is_bool_type(fortran_get_rank0_type(t0))
            && (t1 == NULL || is_integer_type(t1))
            && opt_valid_kind_expr(argument_expressions[2], &di))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "count", 
                choose_int_type_from_kind(argument_expressions[2], di),
                lvalue_ref(t0),
                lvalue_ref((t1 == NULL ? fortran_get_default_integer_type() : t1)),
                lvalue_ref(fortran_get_default_integer_type()));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_cpu_time(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (is_floating_type(t0))
    {
        // void because this is a subroutine
        return GET_INTRINSIC_IMPURE(symbol, "cpu_time", get_void_type(), lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_cshift(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    type_t* t1 = no_ref(argument_types[1]);
    type_t* t2 = no_ptr(no_ref(argument_types[2]));

    if (fortran_is_array_type(t0)
            && is_integer_type(fortran_get_rank0_type(t1))
            && (fortran_get_rank_of_type(t1) == 0 ||
                ((fortran_get_rank_of_type(t0) - 1) == fortran_get_rank_of_type(t1)))
            && (t2 == NULL || is_integer_type(t2)))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "cshift", t0,
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2 == NULL ? fortran_get_default_integer_type() : t2));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_date_and_time(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));
    type_t* t3 = no_ptr(no_ref(argument_types[3]));

    if ((t0 == NULL || fortran_is_character_type(t0))
            && (t1 == NULL || fortran_is_character_type(t1))
            && (t2 == NULL || fortran_is_character_type(t2))
            && (t3 == NULL || 
                (is_integer_type(fortran_get_rank0_type(t3)) &&
                 fortran_get_rank_of_type(t3) == 1)))
    {
        type_t* char_type = get_array_type(get_char_type(), nodecl_null(), symbol->decl_context);
        type_t* int_array = fortran_get_n_ranked_type(fortran_get_default_integer_type(), 1, symbol->decl_context);
        return GET_INTRINSIC_IMPURE(symbol, "date_and_time", get_void_type(),
                lvalue_ref(char_type),
                lvalue_ref(char_type),
                lvalue_ref(char_type),
                lvalue_ref(int_array));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_dble(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_integer_type(t0)
            || is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "dble", fortran_get_doubleprecision_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_digits(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (is_integer_type(fortran_get_rank0_type(t0))
            || is_floating_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "digits", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_dim(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if ((is_integer_type(t0)
                || is_floating_type(t0))
            && equivalent_types(get_unqualified_type(t0), get_unqualified_type(t1)))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "dim", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_dot_product(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    type_t* t1 = no_ref(argument_types[1]);

    char is_numeric_0 = is_integer_type(fortran_get_rank0_type(t0))
        || is_floating_type(fortran_get_rank0_type(t0))
        || is_complex_type(fortran_get_rank0_type(t0));

    char is_logical_0 = is_bool_type(fortran_get_rank0_type(t0));

    char is_numeric_1 = is_integer_type(fortran_get_rank0_type(t1))
        || is_floating_type(fortran_get_rank0_type(t1))
        || is_complex_type(fortran_get_rank0_type(t1));

    char is_logical_1 = is_bool_type(fortran_get_rank0_type(t1));

    if ((is_numeric_0 && is_numeric_1)
            || ((is_logical_0 && is_logical_1)
                && (fortran_get_rank_of_type(t0) == 1)
                && (fortran_get_rank_of_type(t1) == 1)))
    {
        type_t* res = common_type_of_binary_operation(fortran_get_rank0_type(t0), fortran_get_rank0_type(t1));
        if (res == NULL)
            return NULL;
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "dot_product", res,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_dprod(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (equivalent_types(t0, fortran_get_default_real_type())
            && equivalent_types(t1, fortran_get_default_real_type()))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "dprod", fortran_get_doubleprecision_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_dshiftl(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);
    type_t* t2 = fortran_get_rank0_type(argument_types[2]);

    if (is_integer_type(t0)
            && is_integer_type(t1)
            && is_integer_type(t2))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "dshiftl", t0,
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_dshiftr(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);
    type_t* t2 = fortran_get_rank0_type(argument_types[2]);

    if (is_integer_type(t0)
            && is_integer_type(t1)
            && is_integer_type(t2))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "dshiftr", t0,
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_eoshift(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    type_t* t1 = no_ref(argument_types[1]);
    type_t* t2 = no_ref(argument_types[2]);
    type_t* t3 = no_ref(argument_types[3]);

    if (fortran_is_array_type_or_pointer_to(t0)
            && is_integer_type(fortran_get_rank0_type(t1))
            // t1 must have rank(t0)-1 or rank 0
            && (((fortran_get_rank_of_type(t0) - 1) == fortran_get_rank_of_type(t1))
                || (fortran_get_rank_of_type(t1) == 0))
            && (t2 == NULL
                || (equivalent_types(get_unqualified_type(fortran_get_rank0_type(t0)), 
                        get_unqualified_type(fortran_get_rank0_type(t2)))
                    // t2 must have rank(t0)-1 or rank 0
                    && (((fortran_get_rank_of_type(t0) - 1) == fortran_get_rank_of_type(t2))
                        || (fortran_get_rank_of_type(t2) == 0))))
            && (t3 == NULL
                || (is_integer_type(t3))))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "eoshift", t0, 
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2 == NULL ? fortran_get_rank0_type(t0) : t2),
                lvalue_ref(t3 == NULL ? fortran_get_default_integer_type() : t3));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_epsilon(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (is_integer_type(fortran_get_rank0_type(t0))
            || is_floating_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "epsilon", fortran_get_rank0_type(t0),
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_erf(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "erf", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_erfc(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "erfc", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_erfc_scaled(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "erfc_scaled", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_execute_command_line(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));
    type_t* t3 = no_ptr(no_ref(argument_types[3]));
    type_t* t4 = no_ptr(no_ref(argument_types[4]));

    if (fortran_is_character_type(t0)
            && (t1 == NULL || is_bool_type(t1))
            && (t2 == NULL || is_integer_type(t2))
            && (t3 == NULL || is_integer_type(t3))
            && (t4 == NULL || fortran_is_character_type(t4)))
    {
        return GET_INTRINSIC_IMPURE(symbol, "execute_command_line", 
                get_void_type(), // It is a subroutine
                lvalue_ref(t1),
                lvalue_ref(t2 == NULL ? fortran_get_default_integer_type() : t2),
                lvalue_ref(t3 == NULL ? fortran_get_default_integer_type() : t3),
                lvalue_ref(t4 == NULL ? fortran_get_n_ranked_type(get_char_type(), 1, symbol->decl_context) : t3));

    }

    return NULL;
}

scope_entry_t* compute_intrinsic_exp(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "exp", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_exponent(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "exponent", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_extends_type_of(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    // Not supported
    return NULL;
}

scope_entry_t* compute_intrinsic_findloc_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(num_arguments == 6 ? argument_types[2] : NULL));
    type_t* t3 = no_ptr(no_ref(num_arguments == 6 ? argument_types[3] : argument_types[2]));
    nodecl_t kind = num_arguments == 6 ? argument_expressions[4] : argument_expressions[3];
    type_t* t5 = no_ptr(no_ref(num_arguments == 6 ? argument_types[5] : argument_types[4]));

    int di = fortran_get_default_integer_type_kind();

    if (fortran_is_array_type(t0)
            && fortran_is_intrinsic_type(fortran_get_rank0_type(t0))
            && (fortran_get_rank_of_type(t1) == 0)
            && (common_type_of_equality_operation(fortran_get_rank0_type(t0), t1) != NULL)
            && (t2 == NULL || is_integer_type(t2))
            && (t3 == NULL || (is_bool_type(fortran_get_rank0_type(t3)) && fortran_type_is_conformable_to(t3, t0)))
            && opt_valid_kind_expr(kind, &di)
            && (t5 == NULL || is_bool_type(t5)))
    {
        if (t2 == NULL)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "findloc",
                    fortran_get_n_ranked_type(choose_int_type_from_kind(kind, di), fortran_get_rank_of_type(t1) - 1, symbol->decl_context),
                    lvalue_ref(t0),
                    lvalue_ref(t1),
                    lvalue_ref(t3 == NULL ? fortran_get_default_logical_type() : t3),
                    lvalue_ref(fortran_get_default_integer_type()),
                    lvalue_ref(t5 == NULL ? fortran_get_default_logical_type() : t5)
                    );
        }
        else
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "findloc", 
                    fortran_get_n_ranked_type(choose_int_type_from_kind(kind, di), fortran_get_rank_of_type(t1) - 1, symbol->decl_context),
                    lvalue_ref(t0),
                    lvalue_ref(t1),
                    lvalue_ref(t2),
                    lvalue_ref(t3 == NULL ? fortran_get_default_logical_type() : t3),
                    lvalue_ref(fortran_get_default_integer_type()),
                    lvalue_ref(t5 == NULL ? fortran_get_default_logical_type() : t5)
                    );
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_findloc_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_findloc_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_floor(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    nodecl_t kind = argument_expressions[1];

    int dr = fortran_get_default_real_type_kind();
    if (is_floating_type(t0)
            && opt_valid_kind_expr(kind, &dr))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "floor", 
                choose_int_type_from_kind(kind, dr),
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_flush(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 =
        (argument_types[0] != NULL) ?
            fortran_get_rank0_type(argument_types[0]) : fortran_get_default_integer_type();

    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_IMPURE(symbol, "flush", get_void_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_fraction(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "fraction", t0,
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_free(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments == 1)
    {
        type_t* t0 = no_ref(argument_types[0]);

        if(is_integer_type(t0))
        {
            return GET_INTRINSIC_IMPURE(symbol, "free",
                    /* subroutine */ get_void_type(),
                    lvalue_ref(t0));
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_fseek(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));
    type_t* t3 = no_ptr(no_ref(argument_types[3]));

    if(is_integer_type(t0)
            && is_integer_type(t1)
            && is_integer_type(t2)
            && (t3 == NULL ||
                // The type of t3 should be INTEGER(4)
                (equivalent_types(t3, fortran_choose_int_type_from_kind(4)))))
    {
        return GET_INTRINSIC_IMPURE(symbol, "fseek",
                /* subroutine */ get_void_type(),
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2),
                lvalue_ref(t3));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_ftell(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    if(num_arguments == 2
            && is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_IMPURE(symbol, "ftell",
                /* subroutine */ get_void_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    else if (num_arguments == 1
            && is_integer_type(t0))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "ftell",
                fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_gamma(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "gamma", t0,
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_hostnm(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    if (fortran_is_character_type(t0))
    {
        if (num_arguments == 2
                && is_integer_type(t1))
        {
            return GET_INTRINSIC_IMPURE(symbol, "hostnm",
                    /* subroutine */ get_void_type(),
                    lvalue_ref(t0),
                    lvalue_ref(t1));
        }
        else if (num_arguments == 1)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "hostnm",
                    solving_call_statement ?
                    /* subroutine */ get_void_type() :
                    /* function */ fortran_get_default_integer_type(),
                    lvalue_ref(t0));
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_isnan(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments != 1)
        return NULL;

    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "isnan", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_getcwd(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    if (fortran_is_character_type(t0))
    {
        if (num_arguments == 2
                && is_integer_type(t1))
        {
            return GET_INTRINSIC_IMPURE(symbol, "getcwd",
                    /* subroutine */ get_void_type(),
                    lvalue_ref(t0),
                    lvalue_ref(t1));
        }
        else if (num_arguments == 1)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "getcwd",
                    fortran_get_default_integer_type(),
                    lvalue_ref(t0));
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_getlog(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments == 1)
    {
        type_t* t0 = no_ptr(no_ref(argument_types[0]));

        if(fortran_is_character_type(t0))
        {
            return GET_INTRINSIC_IMPURE(symbol, "getlog",
                    /* subroutine */ get_void_type(),
                    lvalue_ref(t0));
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_get_command(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));

    if ((t0 == NULL || fortran_is_character_type(t0))
            && (t1 == NULL || is_integer_type(t1))
            && (t2 == NULL || is_integer_type(t2)))
    {
        return GET_INTRINSIC_IMPURE(symbol, "get_command",
                get_void_type(), // Is a subroutine
                lvalue_ref(t0 == NULL ? fortran_get_n_ranked_type(get_char_type(), 1, symbol->decl_context) : t0),
                lvalue_ref(t1 == NULL ? fortran_get_default_integer_type() : t1),
                lvalue_ref(t2 == NULL ? fortran_get_default_integer_type() : t2));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_get_command_argument(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));
    type_t* t3 = no_ptr(no_ref(argument_types[3]));

    if (is_integer_type(t0)
            && (t1 == NULL || fortran_is_character_type(t1))
            && (t2 == NULL || is_integer_type(t2))
            && (t3 == NULL || is_integer_type(t3)))
    {
        return GET_INTRINSIC_IMPURE(symbol, "get_command_argument",
                get_void_type(), // Is a subroutine
                lvalue_ref(t0),
                lvalue_ref(t1 == NULL ? fortran_get_n_ranked_type(get_char_type(), 1, symbol->decl_context) : t1),
                lvalue_ref(t2 == NULL ? fortran_get_default_integer_type() : t2),
                lvalue_ref(t3 == NULL ? fortran_get_default_integer_type() : t3));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_get_environment_variable(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));
    type_t* t3 = no_ptr(no_ref(argument_types[3]));
    type_t* t4 = no_ptr(no_ref(argument_types[4]));

    if (fortran_is_character_type(t0)
            && (t1 == NULL || fortran_is_character_type(t1))
            && (t2 == NULL || is_integer_type(t2))
            && (t3 == NULL || is_integer_type(t3))
            && (t4 == NULL || is_bool_type(t4)))
    {
        return GET_INTRINSIC_IMPURE(symbol, "get_environment_variable",
                get_void_type(), // is a subroutine
                lvalue_ref(argument_types[0]),
                lvalue_ref(t1 == NULL ? fortran_get_n_ranked_type(get_char_type(), 1, symbol->decl_context) : t1),
                lvalue_ref(t2 == NULL ? fortran_get_default_integer_type() : argument_types[2]),
                lvalue_ref(t3 == NULL ? fortran_get_default_integer_type() : argument_types[3]),
                lvalue_ref(t4 == NULL ? fortran_get_default_logical_type() : argument_types[4]));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_huge(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    if (is_integer_type(fortran_get_rank0_type(t0))
            || is_floating_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "huge", fortran_get_rank0_type(t0),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_hypot(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_floating_type(t0)
            && equivalent_types(get_unqualified_type(t0), get_unqualified_type(t1)))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "hypot", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_iachar(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    int di = fortran_get_default_integer_type_kind();
    if (fortran_is_character_type(t0)
            && opt_valid_kind_expr(argument_expressions[1], &di))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "iachar", 
                choose_int_type_from_kind(argument_expressions[1], di), 
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_iall_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(num_arguments == 3 ? argument_types[1] : NULL));
    type_t* t2 = no_ptr(no_ref(num_arguments == 3 ? argument_types[2] : argument_types[1]));
    if (fortran_is_array_type(t0)
            && is_integer_type(fortran_get_rank0_type(t0))
            && is_integer_type(t1)
            && (t2 == NULL
                || (is_bool_type(fortran_get_rank0_type(t2)) && fortran_type_is_conformable_to(t2, t0))))
    {
        type_t* return_type = fortran_get_rank0_type(t0);
        if (t1 != NULL)
        {
            return_type = fortran_get_n_ranked_type(fortran_get_rank0_type(t0), fortran_get_rank_of_type(t0) - 1, symbol->decl_context);
        }
        if (num_arguments == 3)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "iall",
                    return_type,
                    lvalue_ref(t0),
                    lvalue_ref(t1 == NULL ? fortran_get_default_integer_type() : t1),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2));
        }
        else if (num_arguments == 2)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "iall",
                    return_type,
                    lvalue_ref(t0),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2));
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_iall_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_iall_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_iand(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "iand", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_iany_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(num_arguments == 3 ? argument_types[1] : NULL));
    type_t* t2 = no_ptr(no_ref(num_arguments == 3 ? argument_types[2] : argument_types[1]));
    if (fortran_is_array_type(t0)
            && is_integer_type(fortran_get_rank0_type(t0))
            && is_integer_type(t1)
            && (t2 == NULL
                || (is_bool_type(fortran_get_rank0_type(t2)) && fortran_type_is_conformable_to(t2, t0))))
    {
        type_t* return_type = fortran_get_rank0_type(t0);
        if (t1 != NULL)
        {
            return_type = fortran_get_n_ranked_type(fortran_get_rank0_type(t0), fortran_get_rank_of_type(t0) - 1, symbol->decl_context);
        }
        if (num_arguments == 3)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "iany",
                    return_type,
                    lvalue_ref(t0),
                    lvalue_ref(t1 == NULL ? fortran_get_default_integer_type() : t1),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2));
        }
        else if (num_arguments == 2)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "iany",
                    return_type,
                    lvalue_ref(t0),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2));
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_iany_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_iany_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_ibclr(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ibclr", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_ibits(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);
    type_t* t2 = fortran_get_rank0_type(argument_types[2]);

    if (is_integer_type(t0)
            && is_integer_type(t1)
            && is_integer_type(t2))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ibits", t0,
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_ibset(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ibset", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_ichar(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    int di = fortran_get_default_integer_type_kind();
    if (fortran_is_character_type(t0)
            && opt_valid_kind_expr(argument_expressions[1], &di))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ichar",
                choose_int_type_from_kind(argument_expressions[1], di), 
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_ieor(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieor", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_image_index(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    // Not supported
    return NULL;
}

scope_entry_t* compute_intrinsic_index(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);
    type_t* t2 = fortran_get_rank0_type(argument_types[2]);

    int di = fortran_get_default_integer_type_kind();
    if (fortran_is_character_type(t0)
            && fortran_is_character_type(t1)
            && equivalent_types(get_unqualified_type(array_type_get_element_type(t0)), 
                get_unqualified_type(array_type_get_element_type(t1)))
            && (t2 == NULL || is_bool_type(t2))
            && opt_valid_kind_expr(argument_expressions[3], &di))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "index", 
                choose_int_type_from_kind(argument_expressions[3], di),
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2),
                lvalue_ref(fortran_get_default_integer_type()));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_int(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    int di = fortran_get_default_integer_type_kind();

    if ((is_integer_type(t0)
                || is_floating_type(t0)
                || is_complex_type(t0))
            && opt_valid_kind_expr(argument_expressions[1], &di))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "int", 
                choose_int_type_from_kind(argument_expressions[1], di),
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_ior(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ior", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_iparity_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(num_arguments == 3 ? argument_types[1] : NULL));
    type_t* t2 = no_ptr(no_ref(num_arguments == 3 ? argument_types[2] : argument_types[1]));

    if (fortran_is_array_type(t0)
            && is_integer_type(fortran_get_rank0_type(t0))
            && is_integer_type(t1)
            && (t2 == NULL
                || (is_bool_type(fortran_get_rank0_type(t2)) && fortran_type_is_conformable_to(t2, t0))))
    {
        type_t* return_type = fortran_get_rank0_type(t0);
        if (t1 != NULL)
        {
            return_type = fortran_get_n_ranked_type(fortran_get_rank0_type(t0), fortran_get_rank_of_type(t0) - 1, symbol->decl_context);
        }
        if (num_arguments == 3)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "iparity",
                    return_type,
                    lvalue_ref(t0),
                    lvalue_ref(t1 == NULL ? fortran_get_default_integer_type() : t1),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2));
        }
        else if (num_arguments == 2)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "iparity",
                    return_type,
                    lvalue_ref(t0),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2));
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_iparity_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_iparity_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_ishft(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ishft", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_ishftc(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);
    type_t* t2 = fortran_get_rank0_type(argument_types[2]);

    if (is_integer_type(t0)
            && is_integer_type(t1)
            && (t2 == NULL || is_integer_type(t2)))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ishftc", t0,
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2 == NULL ? fortran_get_default_integer_type() : t2));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_is_contiguous(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (fortran_is_array_type(t0))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "is_contiguous", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_is_iostat_end(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "is_iostat_end", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_is_iostat_eor(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "is_iostat_eor", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_kind(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (fortran_is_intrinsic_type(t0))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "kind", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_lbound(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    int di = fortran_get_default_integer_type_kind();

    if (fortran_is_array_type(t0)
            && (t1 == NULL || is_integer_type(t1))
            && (opt_valid_kind_expr(argument_expressions[2], &di)))
    {
        if (t1 != NULL)
        {
            return GET_INTRINSIC_INQUIRY(symbol, "lbound",
                    choose_int_type_from_kind(argument_expressions[2], di),
                    lvalue_ref(t0),
                    lvalue_ref(t1),
                    lvalue_ref(fortran_get_default_integer_type()));
        }
        else
        {
            return GET_INTRINSIC_INQUIRY(symbol, "lbound",
                    fortran_get_n_ranked_type(choose_int_type_from_kind(argument_expressions[2], di), 
                        1,
                        symbol->decl_context),
                    lvalue_ref(t0),
                    lvalue_ref(fortran_get_default_integer_type()),
                    lvalue_ref(fortran_get_default_integer_type()));
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_lcobound(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    // Not supported
    return NULL;
}

scope_entry_t* compute_intrinsic_leadz(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "leadz", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_len(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    int di = fortran_get_default_integer_type_kind();
    if (fortran_is_character_type(fortran_get_rank0_type(t0))
            && opt_valid_kind_expr(argument_expressions[1], &di))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "len",
                choose_int_type_from_kind(argument_expressions[1], di),
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_len_trim(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    int di = fortran_get_default_integer_type_kind();
    if (fortran_is_character_type(t0)
            && opt_valid_kind_expr(argument_expressions[1], &di))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "len_trim",
                choose_int_type_from_kind(argument_expressions[1], di),
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_lge(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (fortran_is_character_type(t0)
            && fortran_is_character_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "lge", fortran_get_default_logical_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_lgt(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (fortran_is_character_type(t0)
            && fortran_is_character_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "lgt", fortran_get_default_logical_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_lle(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (fortran_is_character_type(t0)
            && fortran_is_character_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "lle", fortran_get_default_logical_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_llt(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (fortran_is_character_type(t0)
            && fortran_is_character_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "llt", fortran_get_default_logical_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_log(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "log", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_log_gamma(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "log_gamma", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_log10(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "log10", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_logical(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    int dl = fortran_get_default_logical_type_kind();
    if (is_bool_type(t0)
            && opt_valid_kind_expr(argument_expressions[1], &dl))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "logical",
                choose_logical_type_from_kind(argument_expressions[1], dl),
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }
    return NULL;
}

// GNU Extension
scope_entry_t* compute_intrinsic_malloc(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "malloc",
                fortran_get_default_integer_type(),
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_maskl(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    int di = fortran_get_default_integer_type_kind();

    if (is_integer_type(t0)
            && opt_valid_kind_expr(argument_expressions[1], &di))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "maskl", 
                choose_int_type_from_kind(argument_expressions[1], di), 
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type())); 
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_maskr(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    int di = fortran_get_default_integer_type_kind();

    if (is_integer_type(t0)
            && opt_valid_kind_expr(argument_expressions[1], &di))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "maskr", 
                choose_int_type_from_kind(argument_expressions[1], di), 
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_matmul(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    char is_numeric_0 = is_integer_type(fortran_get_rank0_type(t0))
        || is_floating_type(fortran_get_rank0_type(t0))
        || is_complex_type(fortran_get_rank0_type(t0));

    char is_logical_0 = is_bool_type(fortran_get_rank0_type(t0));

    char is_numeric_1 = is_integer_type(fortran_get_rank0_type(t1))
        || is_floating_type(fortran_get_rank0_type(t1))
        || is_complex_type(fortran_get_rank0_type(t1));

    char is_logical_1 = is_bool_type(fortran_get_rank0_type(t1));

    char is_rank1_0 = fortran_get_rank_of_type(t0) == 1;
    char is_rank1_1 = fortran_get_rank_of_type(t1) == 1;
    char is_rank2_0 = fortran_get_rank_of_type(t0) == 2;
    char is_rank2_1 = fortran_get_rank_of_type(t1) == 2;

    if (((is_rank2_0 && (is_rank1_1 || is_rank2_1))
                || (is_rank2_1 && (is_rank1_0 || is_rank2_0)))
            && ((is_logical_0 && is_logical_1)
                || (is_numeric_0 && is_numeric_1)))
    {
        type_t* result_type = common_type_of_binary_operation(fortran_get_rank0_type(t0), fortran_get_rank0_type(t1));

        if (is_rank2_0 && is_rank2_1)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "matmul",
                    fortran_get_n_ranked_type(result_type, 2, symbol->decl_context),
                    lvalue_ref(t0),
                    lvalue_ref(t1));
        }
        else
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "matmul",
                    fortran_get_n_ranked_type(result_type, 1, symbol->decl_context),
                    lvalue_ref(t0),
                    lvalue_ref(t1));
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_max_min_aux(
        const char* name,
        type_t* output_type,
        type_t* input_type,
        scope_entry_t* symbol,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments == 0)
        return NULL;

    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (input_type == NULL
            && (!is_integer_type(t0)
                && !is_floating_type(t0)
                && !fortran_is_character_type(t0)))
    {
        return NULL;
    }
    else if (input_type != NULL
            && !equivalent_types(get_unqualified_type(input_type), get_unqualified_type(t0)))
    {
        if ((is_integer_type(input_type)
                    && is_integer_type(t0))
                || (is_floating_type(input_type)
                    && is_floating_type(t0))
                || (fortran_is_character_type(input_type)
                    && fortran_is_character_type(t0)))
        {
            // Well, the input type does not match, but it is a common extension to
            // allow different kinds for the same type
        }
        // Neither the type nor the kind match, this is wrong
        else return NULL;
    }

    type_t* ranked_0[num_arguments + 1];
    memset(ranked_0, 0, sizeof(ranked_0));

    ranked_0[0] = (input_type == NULL) ? t0 : input_type;

    int i;
    for (i = 1; i < num_arguments; i++)
    {
        // -- Due to a GNU extension we do not check this
        // if (!equivalent_types(
        //             get_unqualified_type(fortran_get_rank0_type(argument_types[i])), 
        //             get_unqualified_type(t0)))
        //     return NULL;

        if (input_type == NULL)
        {
            ranked_0[i] = lvalue_ref(fortran_get_rank0_type(argument_types[i]));
        }
        else
        {
            ranked_0[i] = lvalue_ref(input_type);
        }
    }

    return get_intrinsic_symbol_(symbol, name, 
            output_type == NULL ? t0 : output_type, 
            num_arguments, ranked_0, symbol->decl_context, 
            /* is_elemental */ 1, 
            /* is_pure */ 1, 
            /* is_transformational */ 0, 
            /* is_inquiry */ 0); 
}

scope_entry_t* compute_intrinsic_max(
        scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_max_min_aux("max",
            /* output_type */ NULL,
            /* input_type */ NULL,
            symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_max0(
        scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_max_min_aux("max0",
            /* output_type */ fortran_get_default_integer_type(),
            /* input_type */ fortran_get_default_integer_type(),
            symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_max1(
        scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_max_min_aux("max1",
            /* output_type */ fortran_get_default_integer_type(),
            /* input_type */ fortran_get_default_real_type(),
            symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_amax0(
        scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_max_min_aux("amax0",
            /* output_type */ fortran_get_default_real_type(),
            /* input_type */ fortran_get_default_integer_type(),
            symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_amax1(
        scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_max_min_aux("amax1",
            /* output_type */ fortran_get_default_real_type(),
            /* input_type */ fortran_get_default_real_type(),
            symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_dmax1(
        scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_max_min_aux("dmax1",
            /* output_type */ fortran_get_doubleprecision_type(),
            /* input_type */ fortran_get_doubleprecision_type(),
            symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_maxexponent(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (is_floating_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "maxexponent", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_maxloc_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(num_arguments == 5 ? argument_types[1] : NULL));
    type_t* t2 = no_ptr(no_ref(num_arguments == 5 ? argument_types[2] : argument_types[1]));
    // type_t* t3 = num_arguments == 5 ? argument_types[3] : argument_types[2];
    nodecl_t kind = num_arguments == 5 ? argument_expressions[3] : argument_expressions[2];
    type_t* t4 = no_ptr(no_ref(num_arguments == 5 ? argument_types[4] : argument_types[3]));

    int di = fortran_get_default_integer_type_kind();

    if (fortran_is_array_type(t0)
            && (is_floating_type(fortran_get_rank0_type(t0))
                || is_integer_type(fortran_get_rank0_type(t0))
                || fortran_is_character_type(fortran_get_rank0_type(t0)))
            && (t1 == NULL || is_integer_type(t1))
            && (t2 == NULL || (is_bool_type(fortran_get_rank0_type(t2)) && fortran_type_is_conformable_to(t2, t0)))
            && opt_valid_kind_expr(kind, &di)
            && (t4 == NULL || is_bool_type(t4)))
    {
        int rank = 1;
        if (t1 != NULL)
        {
            rank = fortran_get_rank_of_type(t0) - 1;
        }

        if (t1 == NULL)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "maxloc",
                    fortran_get_n_ranked_type(choose_int_type_from_kind(kind, di), rank, symbol->decl_context),
                    lvalue_ref(t0),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2),
                    lvalue_ref(fortran_get_default_integer_type()),
                    lvalue_ref(t4 == NULL ? fortran_get_default_logical_type() : t4));
        }
        else
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "maxloc",
                    fortran_get_n_ranked_type(choose_int_type_from_kind(kind, di), rank, symbol->decl_context),
                    lvalue_ref(t0),
                    lvalue_ref(t1),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2),
                    lvalue_ref(fortran_get_default_integer_type()),
                    lvalue_ref(t4 == NULL ? fortran_get_default_logical_type() : t4));
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_maxloc_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_maxloc_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_maxval_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(num_arguments == 3 ? argument_types[1] : NULL));
    type_t* t2 = no_ptr(no_ref(num_arguments == 3 ? argument_types[2] : argument_types[1]));

    if (fortran_is_array_type(t0)
            && (is_integer_type(fortran_get_rank0_type(t0))
                || is_floating_type(fortran_get_rank0_type(t0))
                || fortran_is_character_type(fortran_get_rank0_type(t0)))
            && (t1 == NULL || is_integer_type(t1))
            && (t2 == NULL || (is_bool_type(fortran_get_rank0_type(t2)) && fortran_type_is_conformable_to(t2, t0))))
    {
        if (t1 == NULL)
        {
            // If DIM is absent this is always a scalar
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "maxval", 
                    fortran_get_rank0_type(t0),
                    lvalue_ref(t0),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2));
        }
        else
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "maxval",
                    fortran_get_n_ranked_type(fortran_get_rank0_type(t0), fortran_get_rank_of_type(t0) - 1, symbol->decl_context),
                    lvalue_ref(t0),
                    lvalue_ref(t1),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2));
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_maxval_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_maxval_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_merge(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);
    type_t* t2 = fortran_get_rank0_type(argument_types[2]);

    if (equivalent_types(get_unqualified_type(t0), 
                get_unqualified_type(t1))
            && is_bool_type(t2))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "merge", t0,
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_merge_bits(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);
    type_t* t2 = fortran_get_rank0_type(argument_types[2]);

    if (is_integer_type(t0)
            && is_integer_type(t1)
            && is_integer_type(t2))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "merge_bits", t0,
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_min(
        scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_max_min_aux("min", /* output_type */ NULL, /* input_type */ NULL, 
            symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_min0(
        scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_max_min_aux("min0", 
            /* output_type */ fortran_get_default_integer_type(), 
            /* input_type */ fortran_get_default_integer_type(), 
            symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_min1(
        scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_max_min_aux("min1", 
            /* output_type */ fortran_get_default_integer_type(), 
            /* input_type */ fortran_get_default_real_type(), 
            symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_amin0(
        scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_max_min_aux("amin0",
            /* output_type */ fortran_get_default_real_type(),
            /* input_type */ fortran_get_default_integer_type(),
            symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_amin1(
        scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_max_min_aux("amin1",
            /* output_type */ fortran_get_default_real_type(),
            /* input_type */ fortran_get_default_real_type(),
            symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_dmin1(
        scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_max_min_aux("dmin1",
            /* output_type */ fortran_get_doubleprecision_type(),
            /* input_type */ fortran_get_doubleprecision_type(),
            symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_minexponent(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (is_floating_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "minexponent", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_minloc_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(num_arguments == 5 ? argument_types[1] : NULL));
    type_t* t2 = no_ptr(no_ref(num_arguments == 5 ? argument_types[2] : argument_types[1]));
    // type_t* t3 = num_arguments == 5 ? argument_types[3] : argument_types[2];
    nodecl_t kind = num_arguments == 5 ? argument_expressions[3] : argument_expressions[2];
    type_t* t4 = no_ptr(no_ref(num_arguments == 5 ? argument_types[4] : argument_types[3]));

    int di = fortran_get_default_integer_type_kind();

    if (fortran_is_array_type(t0)
            && (is_floating_type(fortran_get_rank0_type(t0))
                || is_integer_type(fortran_get_rank0_type(t0))
                || fortran_is_character_type(fortran_get_rank0_type(t0)))
            && (t1 == NULL || is_integer_type(t1))
            && (t2 == NULL || (is_bool_type(fortran_get_rank0_type(t2)) && fortran_type_is_conformable_to(t2, t0)))
            && opt_valid_kind_expr(kind, &di)
            && (t4 == NULL || is_bool_type(t4)))
    {
        int rank = 1;
        if (t1 != NULL)
        {
            rank = fortran_get_rank_of_type(t0) - 1;
        }

        if (t1 == NULL)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "minloc", 
                    fortran_get_n_ranked_type(choose_int_type_from_kind(kind, di), rank, symbol->decl_context),
                    lvalue_ref(t0), 
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2), 
                    lvalue_ref(fortran_get_default_integer_type()),
                    lvalue_ref(t4 == NULL ? fortran_get_default_logical_type() : t4));
        }
        else
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "minloc", 
                    fortran_get_n_ranked_type(choose_int_type_from_kind(kind, di), rank, symbol->decl_context),
                    lvalue_ref(t0), 
                    lvalue_ref(t1),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2), 
                    lvalue_ref(fortran_get_default_integer_type()),
                    lvalue_ref(t4 == NULL ? fortran_get_default_logical_type() : t4));
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_minloc_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_minloc_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_minval_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(num_arguments == 3 ? argument_types[1] : NULL));
    type_t* t2 = no_ptr(no_ref(num_arguments == 3 ? argument_types[2] : argument_types[1]));

    if (fortran_is_array_type(t0)
            && (is_integer_type(fortran_get_rank0_type(t0))
                || is_floating_type(fortran_get_rank0_type(t0))
                || fortran_is_character_type(fortran_get_rank0_type(t0)))
            && (t1 == NULL || is_integer_type(t1))
            && (t2 == NULL || (is_bool_type(fortran_get_rank0_type(t2)) && fortran_type_is_conformable_to(t2, t0))))
    {
        if (t1 == NULL)
        {
            // If DIM is absent this is always a scalar
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "minval",
                    fortran_get_rank0_type(t0),
                    lvalue_ref(t0),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2));
        }
        else
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "minval",
                    fortran_get_n_ranked_type(fortran_get_rank0_type(t0), fortran_get_rank_of_type(t0) - 1, symbol->decl_context),
                    lvalue_ref(t0),
                    lvalue_ref(t1),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2));
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_minval_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_minval_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_mod(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if ((is_integer_type(t0) && is_integer_type(t1))
                || (is_floating_type(t0) && is_floating_type(t1)))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "mod", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_modulo(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if ((is_integer_type(t0) && is_integer_type(t1))
                || (is_floating_type(t0) && is_floating_type(t1)))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "modulo", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_move_alloc(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    if (equivalent_types(get_unqualified_type(fortran_get_rank0_type(t0)), 
                get_unqualified_type(fortran_get_rank0_type(t1)))
            && (fortran_get_rank_of_type(t0) == fortran_get_rank_of_type(t1)))
    {
        return GET_INTRINSIC_PURE(symbol, "move_alloc", /* subroutine */ get_void_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_mvbits(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);
    type_t* t2 = fortran_get_rank0_type(argument_types[2]);
    type_t* t3 = fortran_get_rank0_type(argument_types[3]);
    type_t* t4 = fortran_get_rank0_type(argument_types[4]);

    if (is_integer_type(t0)
            && is_integer_type(t1)
            && is_integer_type(t2)
            && is_integer_type(t3)
            && is_integer_type(t4))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "mvbits", /* subroutine */ get_void_type(),
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2),
                lvalue_ref(t3),
                lvalue_ref(t4));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_nearest(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_floating_type(t0)
            && is_floating_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "nearest", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_new_line(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (fortran_is_character_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "new_line", fortran_get_rank0_type(t0),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_nint(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    int di = fortran_get_default_integer_type_kind();

    if (is_floating_type(t0)
            && opt_valid_kind_expr(argument_expressions[1], &di))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "nint",
                choose_int_type_from_kind(argument_expressions[1], di),
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_not(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "not", t0,
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_norm2(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    if (fortran_is_array_type(t0)
            && is_floating_type(fortran_get_rank0_type(t0))
            && (t1 == NULL || is_integer_type(t1)))
    {
        if (t1 == NULL)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "norm2",
                    fortran_get_rank0_type(t0),
                    lvalue_ref(t0),
                    fortran_get_default_integer_type());
        }
        else
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "norm2", 
                    fortran_get_n_ranked_type(fortran_get_rank0_type(t0), fortran_get_rank_of_type(t0) - 1, symbol->decl_context), 
                    lvalue_ref(t0),
                    lvalue_ref(t1));
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_null(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (nodecl_is_null(argument_expressions[0]))
    {
        type_t* p = get_pointer_type(get_void_type());
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "null", p, lvalue_ref(p));
    }
    else
    {
        type_t* relevant_type = NULL;
        scope_entry_t* sym = NULL;
        if (is_pointer_type(no_ref(nodecl_get_type(argument_expressions[0]))))
        {
            relevant_type = no_ref(nodecl_get_type(nodecl_get_child(argument_expressions[0], 0)));
        }
        else if ((sym = fortran_data_ref_get_symbol(argument_expressions[0])) != NULL)
        {
            if (!symbol_entity_specs_get_is_allocatable(sym))
            {
                return NULL;
            }
            relevant_type = no_ref(sym->type_information);
        }
        else
        {
            return NULL;
        }

        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "null", relevant_type, lvalue_ref(relevant_type));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_num_images(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    // Not supported
    return NULL;
}

scope_entry_t* compute_intrinsic_pack(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));

    if (fortran_is_array_type(t0)
                    && is_bool_type(fortran_get_rank0_type(t1))
                    && fortran_type_is_conformable_to(t1, t0)
            && (t2 == NULL ||
                (equivalent_types(get_unqualified_type(fortran_get_rank0_type(t0)),
                                  get_unqualified_type(fortran_get_rank0_type(t2)))
                 && fortran_get_rank_of_type(t2) == 1)))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "pack",
                t2 == NULL ? fortran_get_n_ranked_type(fortran_get_rank0_type(t0), 1, symbol->decl_context) : t2,
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2 == NULL ? fortran_get_n_ranked_type(fortran_get_rank0_type(t0), 1, symbol->decl_context) : t2));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_parity(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    if (fortran_is_array_type(t0)
            && is_bool_type(fortran_get_rank0_type(t0))
            && (t1 == NULL || is_integer_type(t1)))
    {
        if (t1 == NULL)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "parity", fortran_get_rank0_type(t0),
                    lvalue_ref(t0));
        }
        else
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "parity", 
                    fortran_get_n_ranked_type(fortran_get_rank0_type(t0), fortran_get_rank_of_type(t0) - 1, symbol->decl_context), 
                    lvalue_ref(t0),
                    lvalue_ref(t1));
        }
    }


    return NULL;
}

scope_entry_t* compute_intrinsic_popcnt(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "popcnt", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    } 

    return NULL;
}

scope_entry_t* compute_intrinsic_poppar(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "poppar", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    } 

    return NULL;
}

scope_entry_t* compute_intrinsic_precision(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (is_complex_type(fortran_get_rank0_type(t0))
            || is_floating_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "precision", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_present(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    return GET_INTRINSIC_INQUIRY(symbol, "present", fortran_get_default_logical_type(),
            lvalue_ref(t0));
}

scope_entry_t* compute_intrinsic_product_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(num_arguments == 3 ? argument_types[1] : NULL));
    type_t* t2 = no_ptr(no_ref(num_arguments == 3 ? argument_types[2] : argument_types[1]));

    if (fortran_is_array_type(t0)
            && (is_integer_type(fortran_get_rank0_type(t0)) 
                || is_floating_type(fortran_get_rank0_type(t0)) 
                || is_complex_type(fortran_get_rank0_type(t0)))
            && (t1 == NULL || is_integer_type(t1))
            && (t2 == NULL || (is_bool_type(fortran_get_rank0_type(t2)) && fortran_type_is_conformable_to(t2, t0))))
    {
        type_t* return_type = fortran_get_rank0_type(t0);
        if (t1 != NULL)
        {
            return_type = fortran_get_n_ranked_type(fortran_get_rank0_type(t0), fortran_get_rank_of_type(t0) - 1, symbol->decl_context);
        }
        if (num_arguments == 3)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "product",
                    return_type,
                    lvalue_ref(t0), 
                    lvalue_ref(t1 == NULL ? fortran_get_default_integer_type() : t1),
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2));
        }
        else if (num_arguments == 2)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "product",
                    return_type,
                    lvalue_ref(t0), 
                    lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2));
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_product_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_product_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_radix(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (is_integer_type(fortran_get_rank0_type(t0))
            || is_floating_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "radix", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_random_number(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (is_floating_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_IMPURE(symbol, "random_number", /* subroutine */ get_void_type(),
                lvalue_ref(argument_types[0]));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_random_seed(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));

    int num_args = (t0 != NULL) + (t1 != NULL) + (t2 != NULL);

    if ((num_args == 0 || num_args == 1)
            && (t0 == NULL || is_integer_type(t0))
            && (t1 == NULL || (is_integer_type(fortran_get_rank0_type(t1)) && (fortran_get_rank_of_type(t1) == 1)))
            && (t2 == NULL || (is_integer_type(fortran_get_rank0_type(t2)) && (fortran_get_rank_of_type(t2) == 1))))
    {
        return GET_INTRINSIC_IMPURE(symbol, "random_seed",
                /* subroutine */ get_void_type(),
                lvalue_ref(fortran_get_default_integer_type()),
                lvalue_ref(fortran_get_n_ranked_type(fortran_get_default_integer_type(), 1, symbol->decl_context)),
                lvalue_ref(fortran_get_n_ranked_type(fortran_get_default_integer_type(), 1, symbol->decl_context)));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_range(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (is_integer_type(fortran_get_rank0_type(t0))
            || is_floating_type(fortran_get_rank0_type(t0))
            || is_complex_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "range", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_real(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    int dr;
    if (is_complex_type(t0))
    {
        // Get as a default kind, that of the complex
        dr = type_get_size(complex_type_get_base_type(t0));
    }
    else
    {
        dr = fortran_get_default_real_type_kind();
    }

    if ((is_integer_type(t0)
                || is_floating_type(t0)
                || is_complex_type(t0))
            && opt_valid_kind_expr(argument_expressions[1], &dr))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "real",
                choose_float_type_from_kind(argument_expressions[1], dr),
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_float(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "float", 
                fortran_get_default_real_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_access(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{

    if (num_arguments != 2)
        return NULL;

    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    if(fortran_is_character_type(t0)
            && fortran_is_character_type(t1))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "access",
                fortran_get_default_integer_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_abort(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments != 0)
        return NULL;

    return GET_INTRINSIC_IMPURE(symbol, "abort",
            /* subroutine */ get_void_type());
}

scope_entry_t* compute_intrinsic_and(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{

    if (num_arguments != 2)
        return NULL;

    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    if (equivalent_types(t0, t1)
            && (is_integer_type(t0) || is_bool_type(t0)))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "and", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_besj0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_bessel_j0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_besj1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_bessel_j1(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_besjn_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_bessel_jn_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_besjn_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_bessel_jn_1(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_besy0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_bessel_y0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_besy1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_bessel_y1(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_besyn_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_bessel_yn_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_besyn_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_bessel_yn_1(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_chdir(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    if (fortran_is_character_type(t0))
    {
        if (num_arguments == 2
                && is_integer_type(t1))
        {
            return GET_INTRINSIC_IMPURE(symbol, "chdir",
                    /* subroutine */ get_void_type(),
                    lvalue_ref(t0),
                    lvalue_ref(t1));
        }
        else if (num_arguments == 1)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "chdir",
                    fortran_get_default_integer_type(),
                    lvalue_ref(t0));
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_dfloat(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "dfloat", 
                fortran_get_doubleprecision_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_repeat(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    if (fortran_is_character_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "repeat", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_reshape(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));
    type_t* t3 = no_ptr(no_ref(argument_types[3]));

    // SOURCE
    // t0 may be of any type. It shall not be scalar
    if (fortran_get_rank_of_type(t0) == 0)
        return NULL;

    // SHAPE
    // t1 shall be of type integer rank one 
    if (fortran_get_rank_of_type(t1) != 1)
        return NULL;

    // t1 shall be of constant size
    nodecl_t arr_shape_size;

    if (array_type_has_region(t1))
        arr_shape_size = array_type_get_region_size_expr(t1);
    else
        arr_shape_size = array_type_get_array_size_expr(t1);

    if (nodecl_is_null(arr_shape_size)
            || !nodecl_is_constant(arr_shape_size))
        return NULL;

    const_value_t* shape_size_cval = nodecl_get_constant(arr_shape_size);

    int shape_size = const_value_cast_to_signed_int(shape_size_cval);

    if (shape_size > 8)
        return NULL;

    // PAD
    if (t2 != NULL)
    {
        // Shall be of the same type of SOURCE
        if (!equivalent_types(
                    get_unqualified_type(fortran_get_rank0_type(t0)), 
                    get_unqualified_type(fortran_get_rank0_type(t2))))
            return NULL;

        if (fortran_get_rank_of_type(t2) == 0)
            return NULL;
    }
    else
    {
        t2 = fortran_get_n_ranked_type(fortran_get_rank0_type(t1), 1, symbol->decl_context);
    }

    // ORDER
    if (t3 != NULL)
    {
        nodecl_t order_size = array_type_get_array_size_expr(t3);
        if (!nodecl_is_constant(order_size))
            return NULL;
    }
    else
    {
        t3 = t1;
    }

    type_t* r = fortran_get_n_ranked_type(fortran_get_rank0_type(t0), shape_size, symbol->decl_context);

    return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "reshape", r,
            lvalue_ref(t0),
            lvalue_ref(t1),
            lvalue_ref(t2),
            lvalue_ref(t3));
}

scope_entry_t* compute_intrinsic_rrspacing(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "rrspacing", t0,
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_same_type_as(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    // Not supported
    return NULL;
}

scope_entry_t* compute_intrinsic_scale(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_floating_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "scale", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_scan(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);
    type_t* t2 = fortran_get_rank0_type(argument_types[2]);
    // type_t* t3 = fortran_get_rank0_type(argument_types[3]);

    int di = 1;

    if (fortran_is_character_type(t0)
            && fortran_is_character_type(t1)
            && (t2 == NULL || is_bool_type(t2))
            && opt_valid_kind_expr(argument_expressions[3], &di))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "scan",
                choose_int_type_from_kind(argument_expressions[3], di),
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2),
                lvalue_ref(fortran_get_default_integer_type()));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_selected_char_kind(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    if (fortran_is_character_type(t0))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "selected_char_kind", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_selected_int_kind(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "selected_int_kind", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_selected_real_kind(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));

    int num_args = (t0 != NULL) + (t1 != NULL) + (t2 != NULL);

    if (num_args != 0
            && (t0 == NULL || is_integer_type(t0))
            && (t1 == NULL || is_integer_type(t1))
            && (t2 == NULL || is_integer_type(t2)))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "selected_real_kind", fortran_get_default_integer_type(), 
                lvalue_ref(t0 == NULL ? fortran_get_default_integer_type() : t0),
                lvalue_ref(t1 == NULL ? fortran_get_default_integer_type() : t1),
                lvalue_ref(t2 == NULL ? fortran_get_default_integer_type() : t2));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_set_exponent(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_floating_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "set_exponent", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_shape(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    int di = fortran_get_default_integer_type_kind();
    if (opt_valid_kind_expr(argument_expressions[1], &di))
    {
        type_t* result_array_type = 
            get_array_type_bounds(
                    choose_int_type_from_kind(argument_expressions[1], di),
                    nodecl_make_one(),
                    nodecl_make_int_literal(fortran_get_rank_of_type(t0)),
                    symbol->decl_context);
        return GET_INTRINSIC_INQUIRY(symbol, "shape", result_array_type,
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_shifta(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "shifta", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_shiftl(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "shiftl", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_shiftr(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "shiftr", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_sign(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if ((is_integer_type(t0)
                || is_floating_type(t0))
            && (equivalent_types(get_unqualified_type(t0), 
                    get_unqualified_type(t1))))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "sign", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_sin(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "sin", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_sind(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "sind", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_sinh(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "sinh", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_size(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    int di = fortran_get_default_integer_type_kind();

    if (fortran_is_array_type(t0)
            && (t1 == NULL || is_integer_type(t1))
            && (opt_valid_kind_expr(argument_expressions[2], &di)))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "size", 
                choose_int_type_from_kind(argument_expressions[2], di),
                lvalue_ref(t0),
                lvalue_ref(t1 == NULL ? fortran_get_default_integer_type() : t1),
                lvalue_ref(fortran_get_default_integer_type()));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_spacing(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "spacing", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_spread(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));

    if( is_integer_type(t1)
            && is_integer_type(t2))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "spread", 
                fortran_get_n_ranked_type(fortran_get_rank0_type(t0), fortran_get_rank_of_type(t0) + 1, symbol->decl_context),
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_sqrt(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "sqrt", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_storage_size(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    int di = fortran_get_default_integer_type_kind();
    if (opt_valid_kind_expr(argument_expressions[1], &di))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "storage_size", 
                choose_int_type_from_kind(argument_expressions[1], di),
                lvalue_ref(t0),
                lvalue_ref(fortran_get_default_integer_type()));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_sizeof(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    return GET_INTRINSIC_INQUIRY(symbol, "sizeof",
            get_ptrdiff_t_type(),
            lvalue_ref(t0));

    return NULL;
}

scope_entry_t* compute_intrinsic_sum_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(num_arguments == 3 ? argument_types[1] : NULL));
    type_t* t2 = no_ptr(no_ref(num_arguments == 3 ? argument_types[2] : argument_types[1]));

    if (fortran_is_array_type(t0)
            && (is_integer_type(fortran_get_rank0_type(t0)) 
                || is_floating_type(fortran_get_rank0_type(t0)) 
                || is_complex_type(fortran_get_rank0_type(t0)))
            && (t1 == NULL || is_integer_type(t1))
            && (t2 == NULL || (is_bool_type(fortran_get_rank0_type(t2)) && fortran_type_is_conformable_to(t2, t0))))
    {
        type_t* return_type = fortran_get_rank0_type(t0);
        if (t1 != NULL)
        {
            return_type = fortran_get_n_ranked_type(fortran_get_rank0_type(t0), fortran_get_rank_of_type(t0) - 1, symbol->decl_context);
        }
        if (num_arguments == 3)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "sum",
                    return_type,
                    t0, 
                    t1 == NULL ? fortran_get_default_integer_type() : t1,
                    t2 == NULL ? fortran_get_default_logical_type() : t2);
        }
        else if (num_arguments == 2)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "sum",
                    return_type,
                    t0, 
                    t2 == NULL ? fortran_get_default_logical_type() : t2);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_sum_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return compute_intrinsic_sum_0(symbol, argument_types, argument_expressions, num_arguments, const_value);
}

scope_entry_t* compute_intrinsic_system_clock(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));

    if ((t0 == NULL || is_integer_type(t0))
            && (t1 == NULL || is_integer_type(t1) || is_floating_type(t1))
            && (t2 == NULL || is_integer_type(t2)))
    {
        return GET_INTRINSIC_IMPURE(symbol, "system_clock", /*subroutine*/ get_void_type(),
                lvalue_ref(t0 == NULL ? fortran_get_default_integer_type() : argument_types[0]),
                lvalue_ref(t1 == NULL ? fortran_get_default_integer_type() : argument_types[1]),
                lvalue_ref(t2 == NULL ? fortran_get_default_integer_type() : argument_types[2]));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_tan(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "tan", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_tand(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "tand", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_tanh(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);

    if (is_floating_type(t0)
            || is_complex_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "tanh", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_this_image_0(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    // Not supported
    return NULL;
}

scope_entry_t* compute_intrinsic_this_image_1(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    // Not supported
    return NULL;
}

scope_entry_t* compute_intrinsic_tiny(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    if (is_floating_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "tiny", fortran_get_rank0_type(t0),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_trailz(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    if (is_integer_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "trailz", fortran_get_default_integer_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_transfer(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));

    if (t2 == NULL || (is_integer_type(t2)))
    {
        if (!fortran_is_array_type(t1) 
                && t2 == NULL)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "transfer", t1,
                    lvalue_ref(t0),
                    lvalue_ref(t1),
                    lvalue_ref(fortran_get_default_integer_type()));
        }
        else if (fortran_is_array_type(t1)
                && t2 == NULL)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "transfer", 
                    fortran_get_n_ranked_type(fortran_get_rank0_type(t1), 1, symbol->decl_context),
                    lvalue_ref(t0),
                    lvalue_ref(t1),
                    lvalue_ref(fortran_get_default_integer_type()));
        }
        else
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "transfer", 
                    fortran_get_n_ranked_type(fortran_get_rank0_type(t1), 1, symbol->decl_context),
                    lvalue_ref(t0),
                    lvalue_ref(t1),
                    lvalue_ref(t2));
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_transpose(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (fortran_is_array_type(t0)
            && fortran_get_rank_of_type(t0) == 2)
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "transpose", t0,
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_trim(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    if (fortran_is_character_type(t0))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "trim",
                get_array_type(fortran_get_default_character_type(),
                    /* whole size */ nodecl_null(),
                    symbol->decl_context),
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_ubound(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    int di = fortran_get_default_integer_type_kind();

    if (fortran_is_array_type(t0)
            && (t1 == NULL || is_integer_type(t1))
            && (opt_valid_kind_expr(argument_expressions[2], &di)))
    {
        if (t1 != NULL)
        {
            return GET_INTRINSIC_INQUIRY(symbol, "ubound",
                    choose_int_type_from_kind(argument_expressions[2], di),
                    lvalue_ref(t0),
                    lvalue_ref(t1),
                    lvalue_ref(fortran_get_default_integer_type()));
        }
        else
        {
            return GET_INTRINSIC_INQUIRY(symbol, "ubound",
                    fortran_get_n_ranked_type(choose_int_type_from_kind(argument_expressions[2], di), 
                        1,
                        symbol->decl_context),
                    lvalue_ref(t0),
                    lvalue_ref(fortran_get_default_integer_type()),
                    lvalue_ref(fortran_get_default_integer_type()));
        }
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_ucobound(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    // Not supported
    return NULL;
}

scope_entry_t* compute_intrinsic_unpack(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));

    if (fortran_is_array_type(t0)
            && (fortran_get_rank_of_type(t0) == 1)
            && fortran_is_array_type(t1)
            && is_bool_type(fortran_get_rank0_type(t1))
            && equivalent_types(
                get_unqualified_type(fortran_get_rank0_type(t2)),
                get_unqualified_type(fortran_get_rank0_type(t0)))
            && fortran_type_is_conformable_to(t2, t0))
    {
        type_t* result = fortran_get_n_ranked_type(fortran_get_rank0_type(t0),
                fortran_get_rank_of_type(t1), CURRENT_COMPILED_FILE->global_decl_context);
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "unpack", result,
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_verify(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);
    type_t* t2 = fortran_get_rank0_type(argument_types[2]);

    int di = fortran_get_default_integer_type_kind();
    if (fortran_is_character_type(t0)
            && fortran_is_character_type(t1)
            && (t2 == NULL || is_bool_type(t2))
            && opt_valid_kind_expr(argument_expressions[3], &di))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "verify",
                choose_int_type_from_kind(argument_expressions[3], di),
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref(t2 == NULL ? fortran_get_default_logical_type() : t2),
                lvalue_ref(fortran_get_default_integer_type()));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_loc(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments != 1)
        return NULL;

    type_t* t0 = argument_types[0];

    // if (!is_lvalue_reference_type(t0))
    //     return NULL;

    return GET_INTRINSIC_INQUIRY(symbol, "loc",
            choose_int_type_from_kind(argument_expressions[0], CURRENT_CONFIGURATION->type_environment->sizeof_pointer),
            lvalue_ref(t0));
}

scope_entry_t* compute_intrinsic_lshift(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{

    if (num_arguments != 2)
        return NULL;

    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if(is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "lshift", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_or(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{

    if (num_arguments != 2)
        return NULL;

    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (equivalent_types(t0, t1)
            && (is_integer_type(t0) || is_bool_type(t0)))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "or", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_rshift(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments != 2)
        return NULL;

    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if(is_integer_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "rshift", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_xor(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments != 2)
        return NULL;

    type_t* t0 = fortran_get_rank0_type(argument_types[0]);
    type_t* t1 = fortran_get_rank0_type(argument_types[1]);

    if (equivalent_types(t0, t1)
            && (is_integer_type(t0) || is_bool_type(t0)))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "xor", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_system(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments != 1
            && num_arguments != 2)
        return NULL;

    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (!fortran_is_character_type(t0))
        return NULL;

    if (solving_call_statement)
    {
        return GET_INTRINSIC_IMPURE(symbol, "system", get_void_type(),
                lvalue_ref(t0),
                lvalue_ref(get_signed_int_type()));
    }
    else
    {
        if (num_arguments != 1)
            return NULL;
        return GET_INTRINSIC_IMPURE(symbol, "system", get_signed_int_type(),
                lvalue_ref(t0));
    }

    return NULL;
}


scope_entry_t* compute_intrinsic_time(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "time", fortran_choose_int_type_from_kind(4));
}

scope_entry_t* compute_intrinsic_time8(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "time8", fortran_choose_int_type_from_kind(8));
}

scope_entry_t* compute_intrinsic_sleep(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (!is_integer_type(t0))
        return NULL;

    return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "sleep", get_void_type(),
            lvalue_ref(t0));
}

scope_entry_t* compute_intrinsic_fdate(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

    if (num_arguments == 1
            && fortran_is_character_type(t0))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "fdate",
                /* subroutine */get_void_type(),
                lvalue_ref(t0));
    }
    else if (num_arguments == 0)
    {
        return GET_INTRINSIC_INQUIRY(symbol, "fdate",
                get_array_type(get_char_type(), nodecl_null(), symbol->decl_context));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_fgetc(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));
    type_t* t2 = no_ptr(no_ref(argument_types[2]));

    if (is_integer_type(t0)
            && fortran_is_character_type(t1))
    {
        if (num_arguments == 3
                && is_integer_type(t2))
        {
            return GET_INTRINSIC_IMPURE(symbol, "fgetc",
                    /* subroutine */ get_void_type(),
                    lvalue_ref(t0),
                    lvalue_ref(t1),
                    lvalue_ref(t2));
        }
        else if (num_arguments == 2)
        {
            return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "fgetc",
                    fortran_get_default_integer_type(),
                    lvalue_ref(t0),
                    lvalue_ref(t1));
        }
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_etime(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));
    type_t* t1 = no_ptr(no_ref(argument_types[1]));

    if (num_arguments == 2
            && fortran_get_rank_of_type(t0) == 1
            && is_float_type(fortran_get_rank0_type(t0))
            && is_floating_type(t1))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "etime",
                get_void_type(), // subroutine 
                lvalue_ref(t0),
                lvalue_ref(t1));
    }
    else if (num_arguments == 1
            && fortran_get_rank_of_type(t0) == 1
            && is_float_type(fortran_get_rank0_type(t0)))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "etime",
                fortran_get_default_real_type(),
                lvalue_ref(t0));
    }
    return NULL;
}

scope_entry_t* compute_intrinsic_exit(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ptr(no_ref(argument_types[0]));

	if ((num_arguments == 0)
			|| (num_arguments == 1 && is_integer_type(t0)))
	{
		return GET_INTRINSIC_IMPURE(symbol, "exit",
				get_void_type(), // It is a subroutine
				lvalue_ref(t0 == NULL ? fortran_get_default_integer_type() : t0));
	}

    return NULL;
}

scope_entry_t* compute_intrinsic_getarg(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments == 2)
    {
        type_t* t0 = no_ptr(no_ref(argument_types[0]));
        type_t* t1 = no_ptr(no_ref(argument_types[1]));

        if (is_integer_type(t0)
                && fortran_is_character_type(t1))
        {
            return GET_INTRINSIC_IMPURE(symbol, "getarg",
                    /* subroutine */ get_void_type(),
                    lvalue_ref(t0),
                    lvalue_ref(t1));
        }
    }

    return NULL;
}

static scope_entry_t* get_c_ptr(scope_entry_t* module)
{
    scope_entry_list_t* res = fortran_query_module_for_name(module, "c_ptr");
    scope_entry_t* c_ptr = (res != NULL) ? entry_list_head(res) : NULL;
    entry_list_free(res);

    return c_ptr;
}

static scope_entry_t* get_c_funptr(scope_entry_t* module)
{
    scope_entry_list_t* res = fortran_query_module_for_name(module, "c_funptr");
    scope_entry_t* c_funptr = (res != NULL) ? entry_list_head(res) : NULL;
    entry_list_free(res);

    return c_funptr;
}

scope_entry_t* compute_intrinsic_c_associated(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    type_t* t1 = argument_types[1] != NULL ? no_ref(argument_types[1]) : t0;

    ERROR_CONDITION(symbol_entity_specs_get_from_module(symbol) == NULL, "Invalid symbol", 0);
    scope_entry_t* module = symbol_entity_specs_get_from_module(symbol);

    scope_entry_t* c_ptr = get_c_ptr(module);
    scope_entry_t* c_funptr = get_c_funptr(module);

    ERROR_CONDITION(c_ptr == NULL || c_funptr == NULL, "c_ptr, c_funptr not found!\n", 0);

    if (equivalent_types(t1, t0)
            && (equivalent_types(t0, get_user_defined_type(c_ptr))
                || equivalent_types(t0, get_user_defined_type(c_funptr))))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "c_associated", 
                fortran_get_default_logical_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_c_f_pointer(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments != 2
            && num_arguments != 3)
        return NULL;

    type_t* t0 = no_ref(argument_types[0]);
    type_t* t1 = no_ref(argument_types[1]);
    type_t* t2 = (argument_types[2] != NULL) ? no_ref(argument_types[2]) : NULL;

    ERROR_CONDITION(symbol_entity_specs_get_from_module(symbol) == NULL, "Invalid symbol", 0);
    scope_entry_t* module = symbol_entity_specs_get_from_module(symbol);

    scope_entry_t* c_ptr = get_c_ptr(module);

    ERROR_CONDITION(c_ptr == NULL, "c_ptr not found!\n", 0);

    if (equivalent_types(t0, get_user_defined_type(c_ptr))
            && is_pointer_type(t1)
            && (fortran_is_pointer_to_array_type(t1) == (t2 != NULL))
            && (t2 == NULL ||
                (fortran_is_array_type(t2)
                 && fortran_get_rank_of_type(t2) == 1
                 && is_integer_type(fortran_get_rank0_type(t2)))))
    {
        return GET_INTRINSIC_IMPURE(symbol, "c_f_pointer",
                /* subroutine */ get_void_type(),
                lvalue_ref(t0),
                lvalue_ref(t1),
                lvalue_ref((t2 != NULL) ? t2 :
                    fortran_get_n_ranked_type(fortran_get_default_integer_type(),
                        1,
                        CURRENT_COMPILED_FILE->global_decl_context)));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_c_funloc(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments != 1)
        return NULL;

    ERROR_CONDITION(symbol_entity_specs_get_from_module(symbol) == NULL, "Invalid symbol", 0);
    scope_entry_t* module = symbol_entity_specs_get_from_module(symbol);

    scope_entry_t* c_funptr = get_c_funptr(module);
    ERROR_CONDITION(c_funptr == NULL, "c_funptr not found!\n", 0);

    type_t* t0 = no_ref(argument_types[0]);
    scope_entry_t* sym = nodecl_get_symbol(argument_expressions[0]);

    if ((sym->kind == SK_FUNCTION
                // a procedure dummy argument
                || (sym->kind == SK_VARIABLE
                    && symbol_is_parameter_of_function(sym,
                        sym->decl_context->current_scope->related_entry)))
            && is_function_type(t0) // sanity check
            && !nodecl_is_null(symbol_entity_specs_get_bind_info(sym))
            && nodecl_get_kind(symbol_entity_specs_get_bind_info(sym)) == NODECL_FORTRAN_BIND_C)
    {
        return GET_INTRINSIC_INQUIRY(symbol, "c_funloc", get_user_defined_type(c_funptr),
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_c_loc(scope_entry_t* symbol,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments != 1)
        return NULL;

    ERROR_CONDITION(symbol_entity_specs_get_from_module(symbol) == NULL, "Invalid symbol", 0);
    scope_entry_t* module = symbol_entity_specs_get_from_module(symbol);

    scope_entry_t* c_ptr = get_c_ptr(module);
    ERROR_CONDITION(c_ptr == NULL, "c_ptr not found!\n", 0);

    type_t* t0 = no_ref(argument_types[0]);

    return GET_INTRINSIC_INQUIRY(symbol, "c_loc", get_user_defined_type(c_ptr),
            lvalue_ref(t0));
}

scope_entry_t* compute_intrinsic_c_sizeof(scope_entry_t* symbol,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);

    return GET_INTRINSIC_INQUIRY(symbol, "c_sizeof", get_ptrdiff_t_type(),
            lvalue_ref(t0));
}

scope_entry_t* compute_intrinsic_mercurium_loc(scope_entry_t* symbol,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments != 1)
        return NULL;

    type_t* t0 = no_ref(argument_types[0]);

    return GET_INTRINSIC_INQUIRY(symbol, "mercurium_loc", get_pointer_type(get_void_type()),
            lvalue_ref(t0));
}

scope_entry_t* compute_intrinsic_mercurium_null(scope_entry_t* symbol,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    return GET_INTRINSIC_INQUIRY(symbol, "mercurium_null", get_pointer_type(get_void_type()));
}

scope_entry_t* compute_intrinsic_ompss_opencl_allocate(scope_entry_t* symbol,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions,
        int num_arguments,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments != 1)
        return NULL;

    nodecl_t arg = argument_expressions[0];
    ERROR_CONDITION(nodecl_get_kind(arg) != NODECL_ARRAY_SUBSCRIPT,
            "The argument of 'ompss_opencl_allocate' intrinsic must be "
            "an allocatable array or a pointer to an array with all its bounds specified\n", 0);

    scope_entry_t* sym = fortran_data_ref_get_symbol(arg);
    ERROR_CONDITION(sym == NULL, "Invalid symbol\n", 0);

    ERROR_CONDITION(
            !(symbol_entity_specs_get_is_allocatable(sym)
                && fortran_is_array_type(sym->type_information))
            &&
            !(is_pointer_type(sym->type_information)
                && fortran_is_pointer_to_array_type(sym->type_information)),
            "The argument of 'ompss_opencl_allocate' intrinsic must be "
            "an allocatable array or a pointer to an array with all its bounds specified\n", 0);

    type_t* t0 = argument_types[0];
    return GET_INTRINSIC_IMPURE(symbol, "ompss_opencl_allocate", get_void_type(), t0);
}

scope_entry_t* compute_intrinsic_ompss_opencl_deallocate(scope_entry_t* symbol,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    if (num_arguments != 1)
        return NULL;

    nodecl_t arg = argument_expressions[0];

    scope_entry_t* sym = fortran_data_ref_get_symbol(arg);
    ERROR_CONDITION(sym == NULL, "Unreachable code\n", 0);

    ERROR_CONDITION(
            !(symbol_entity_specs_get_is_allocatable(sym)
                && fortran_is_array_type(sym->type_information))
            &&
            !(is_pointer_type(sym->type_information)
                && fortran_is_pointer_to_array_type(sym->type_information)),
            "The argument of 'ompss_opencl_deallocate' intrinsic must be "
            "an allocatable array or a pointer to an array\n", 0);

    type_t* t0 = argument_types[0];
    return GET_INTRINSIC_IMPURE(symbol, "ompss_opencl_deallocate", get_void_type(), t0);
}

scope_entry_t* fortran_solve_generic_intrinsic_call(scope_entry_t* symbol,
        nodecl_t* nodecl_actual_arguments,
        int num_actual_arguments,
        char is_call)
{
    type_t* t = symbol->type_information;
    computed_function_type_t fun = computed_function_type_get_computing_function(t);

    solving_call_statement = is_call;

    return fun(symbol,
            /* argument_types */ NULL,
            nodecl_actual_arguments,
            num_actual_arguments,
            /* const value */ NULL);
}

void fortran_simplify_specific_intrinsic_call(scope_entry_t* symbol,
        nodecl_t* nodecl_actual_arguments,
        int num_actual_arguments,
        nodecl_t* nodecl_simplified,
        const locus_t* locus)
{
    nodecl_t reordered_exprs[MCXX_MAX_FUNCTION_CALL_ARGUMENTS] = { nodecl_null() };

    // We need to do this to reorder the arguments
    // since the simplification function expects them already ordered
    if (specific_keyword_check(symbol,
                &num_actual_arguments,
                nodecl_actual_arguments,
                reordered_exprs))
    {
        if (symbol_entity_specs_get_simplify_function(symbol) != NULL)
        {
            nodecl_t nodecl_arguments[MCXX_MAX_FUNCTION_CALL_ARGUMENTS] = { nodecl_null() };

            int j;
            for (j = 0; j < num_actual_arguments; j++)
            {
                if (!nodecl_is_null(reordered_exprs[j]))
                    nodecl_arguments[j] = reordered_exprs[j];
                else
                    nodecl_arguments[j] = nodecl_null();
            }

            *nodecl_simplified = (symbol_entity_specs_get_simplify_function(symbol))(symbol, num_actual_arguments, nodecl_arguments);
            if (!nodecl_is_null(*nodecl_simplified))
            {
                nodecl_set_locus(*nodecl_simplified, locus);
            }
        }
    }
}

static void fortran_init_intrinsic_module_iso_c_binding(const decl_context_t* decl_context)
{
    // Initialize ISO_C_BINDING
    const decl_context_t* module_context = new_program_unit_context(decl_context);
    const locus_t* locus = make_locus("(iso_c_binding)", 0, 0);

    const char* iso_c_binding_name = UNIQUESTR_LITERAL("iso_c_binding");

    scope_entry_t* iso_c_binding = new_symbol(decl_context, decl_context->current_scope, iso_c_binding_name);
    iso_c_binding->kind = SK_MODULE;
    iso_c_binding->locus = locus;
    symbol_entity_specs_set_is_builtin(iso_c_binding, 1);
    iso_c_binding->related_decl_context = module_context;
    iso_c_binding->defined = 1;

    rb_tree_insert(CURRENT_COMPILED_FILE->module_file_cache, iso_c_binding_name, iso_c_binding);

    type_t* int_type = fortran_get_default_integer_type();

    nodecl_t one = const_value_to_nodecl(const_value_get_signed_int(1));
    type_t* character_type = 
                    get_array_type_bounds(
                            fortran_choose_character_type_from_kind(1), 
                            one, one, decl_context);

    module_context->current_scope->related_entry = iso_c_binding;

    struct named_constants_t {
        const char* name;
        type_t* const_type;
        int value;
    } named_constants[] =
    {
        // Kind names
        // We are missing 128 items
        { "c_signed_char", int_type, 1 },
        { "c_short", int_type, 2 },
        { "c_int", int_type, 4 },
        { "c_long", int_type, type_get_size(get_signed_long_int_type()) },
        { "c_long_long", int_type, 8 },
        { "c_size_t", int_type, type_get_size(get_size_t_type()) },
        { "c_intptr_t", int_type, type_get_size(get_pointer_type(get_void_type())) },
        { "c_intmax_t", int_type, type_get_size(get_signed_long_long_int_type()) },
        { "c_int8_t", int_type, 1 },
        { "c_int16_t", int_type, 2 }, 
        { "c_int32_t", int_type, 4 },
        { "c_int64_t", int_type, 8 },
        { "c_int_least8_t", int_type, 1 },
        { "c_int_least16_t", int_type, 2 },
        { "c_int_least32_t", int_type, 4 },
        { "c_int_least64_t", int_type, 8 },
        { "c_int_fast8_t", int_type, 1 },
        { "c_int_fast16_t", int_type, 4 },
        { "c_int_fast32_t", int_type, 4 },
        { "c_int_fast64_t", int_type, 8 },
        { "c_float", int_type, 4 },
        { "c_double", int_type, 8 },
        { "c_long_double", int_type, type_get_size(get_long_double_type()) },
        { "c_float_complex", int_type, type_get_size(get_complex_type(get_float_type())) },
        { "c_double_complex", int_type, type_get_size(get_complex_type(get_double_type())) },
        { "c_long_double_complex", int_type, type_get_size(get_complex_type(get_long_double_type())) },
        { "c_bool", int_type, 1 },
        { "c_char", int_type, 1 },

        // Character names
        { "c_null_char", character_type, '\0' },
        { "c_alert", character_type, '\a' },
        { "c_backspace", character_type, '\b' },
        { "c_form_feed", character_type, '\f' },
        { "c_new_line", character_type, '\n' },
        { "c_carriage_return", character_type, '\r' },
        { "c_horizontal_tab", character_type, '\t' },
        { "c_vertical_tab", character_type, '\v' },

        // Sentinel
        { NULL, NULL, 0 }
    };

    int i;
    for (i = 0; named_constants[i].name != NULL; i++)
    {
        scope_entry_t* symbol = new_symbol(module_context, module_context->current_scope,
                uniquestr(named_constants[i].name));
        symbol->locus = locus;
        symbol->kind = SK_VARIABLE;
        symbol->type_information = get_const_qualified_type(named_constants[i].const_type);
        symbol_entity_specs_set_in_module(symbol, iso_c_binding);
        symbol_entity_specs_set_access(symbol, AS_PUBLIC);
        symbol_entity_specs_add_related_symbols(iso_c_binding,
                symbol);

        if (fortran_is_character_type(symbol->type_information))
        {
            char c = named_constants[i].value;
            symbol->value = const_value_to_nodecl(const_value_make_string(&c, 1));
        }
        else
        {
            symbol->value = const_value_to_nodecl(const_value_get_signed_int(named_constants[i].value));
        }
    }

    {
    scope_entry_t* c_ptr = new_symbol(module_context, module_context->current_scope, UNIQUESTR_LITERAL("c_ptr"));
    c_ptr->locus = locus;
    c_ptr->kind = SK_CLASS;
    c_ptr->type_information = get_new_class_type(module_context, TT_STRUCT);
    symbol_entity_specs_set_in_module(c_ptr, iso_c_binding);
    symbol_entity_specs_set_access(c_ptr, AS_PUBLIC);
    symbol_entity_specs_add_related_symbols(iso_c_binding,
            c_ptr);

    scope_entry_t* c_null_ptr = new_symbol(module_context, module_context->current_scope,
            UNIQUESTR_LITERAL("c_null_ptr"));
    c_null_ptr->locus = locus;
    c_null_ptr->kind = SK_VARIABLE;
    c_null_ptr->type_information = get_user_defined_type(c_ptr);
    symbol_entity_specs_set_in_module(c_null_ptr, iso_c_binding);
    symbol_entity_specs_set_access(c_null_ptr, AS_PUBLIC);
    symbol_entity_specs_add_related_symbols(iso_c_binding,
            c_null_ptr);
    }

    {
    scope_entry_t* c_funptr = new_symbol(module_context, module_context->current_scope,
            UNIQUESTR_LITERAL("c_funptr"));
    c_funptr->locus = locus;
    c_funptr->type_information = get_new_class_type(module_context, TT_STRUCT);
    c_funptr->kind = SK_CLASS;
    symbol_entity_specs_set_in_module(c_funptr, iso_c_binding);
    symbol_entity_specs_set_access(c_funptr, AS_PUBLIC);
    c_funptr->type_information = get_new_class_type(module_context, TT_STRUCT);
    symbol_entity_specs_add_related_symbols(iso_c_binding, c_funptr);

    scope_entry_t* c_null_funptr = new_symbol(module_context, module_context->current_scope,
            UNIQUESTR_LITERAL("c_null_funptr"));
    c_null_funptr->locus = locus;
    c_null_funptr->kind = SK_VARIABLE;
    c_null_funptr->type_information = get_user_defined_type(c_funptr);
    symbol_entity_specs_set_in_module(c_null_funptr, iso_c_binding);
    symbol_entity_specs_set_access(c_null_funptr, AS_PUBLIC);
    symbol_entity_specs_add_related_symbols(iso_c_binding, c_null_funptr);
    }
}

static void fortran_init_intrinsic_module_ieee_exceptions(const decl_context_t* decl_context)
{
    // Initialize IEEE_EXCEPTIONS
    const decl_context_t* module_context = new_program_unit_context(decl_context);

    const locus_t* locus = make_locus("(ieee_exceptions)", 0, 0);

    const char* ieee_exceptions_name = UNIQUESTR_LITERAL("ieee_exceptions");

    scope_entry_t* ieee_exceptions = new_symbol(decl_context, decl_context->current_scope, ieee_exceptions_name);
    ieee_exceptions->locus = locus;
    ieee_exceptions->kind = SK_MODULE;
    symbol_entity_specs_set_is_builtin(ieee_exceptions, 1);
    ieee_exceptions->related_decl_context = module_context;
    ieee_exceptions->defined = 1;

    rb_tree_insert(CURRENT_COMPILED_FILE->module_file_cache, ieee_exceptions_name, ieee_exceptions);

    module_context->current_scope->related_entry = ieee_exceptions;

    int i;
    // Private types
    scope_entry_t *ieee_flag = NULL, *ieee_status = NULL;
    struct private_types_tag
    {
        const char* name;
        scope_entry_t** p;
    } private_types[] = {
        { "ieee_flag_type", &ieee_flag },
        { "ieee_status_type", &ieee_status },
        { NULL, NULL }
    };

    for (i = 0; private_types[i].name != NULL; i++)
    {
        scope_entry_t* new_type = NULL;
        new_type = *(private_types[i].p) = new_symbol(module_context, module_context->current_scope,
                uniquestr(private_types[i].name));
        new_type->locus = locus;
        new_type->kind = SK_CLASS;
        symbol_entity_specs_set_in_module(new_type, ieee_exceptions);
        new_type->type_information = get_new_class_type(module_context, TT_STRUCT);
        symbol_entity_specs_set_access(new_type, AS_PUBLIC);

        symbol_entity_specs_add_related_symbols(ieee_exceptions, new_type);
    }

    // Global names
    type_t* ieee_flag_type = get_const_qualified_type(get_user_defined_type(ieee_flag));
    nodecl_t ieee_flag_type_value_tree = const_value_to_nodecl(
            const_value_make_struct(0, NULL,
                get_user_defined_type(ieee_flag)));

    // type_t* ieee_status_type = get_user_defined_type(ieee_status);

    type_t* ieee_flag_type_3 = get_array_type_bounds(ieee_flag_type,
            const_value_to_nodecl(const_value_get_signed_int(1)),
            const_value_to_nodecl(const_value_get_signed_int(3)),
            decl_context);

    type_t* ieee_flag_type_5 = get_array_type_bounds(ieee_flag_type,
            const_value_to_nodecl(const_value_get_signed_int(1)),
            const_value_to_nodecl(const_value_get_signed_int(5)),
            decl_context);

    struct global_vars_tag
    {
        const char* name;
        type_t* type;
        nodecl_t value;
    } global_names[] = {
        { "ieee_invalid", ieee_flag_type, ieee_flag_type_value_tree },
        { "ieee_overflow", ieee_flag_type, ieee_flag_type_value_tree },
        { "ieee_divided_by_zero", ieee_flag_type, ieee_flag_type_value_tree },
        { "ieee_underflow", ieee_flag_type, ieee_flag_type_value_tree },
        { "ieee_inexact", ieee_flag_type, ieee_flag_type_value_tree },
        { "ieee_usual", ieee_flag_type_3, nodecl_null() },
        { "ieee_all", ieee_flag_type_5, nodecl_null() },
        { NULL, NULL, nodecl_null() }
    };

    for (i = 0; global_names[i].name != NULL; i++)
    {
        scope_entry_t* new_var = NULL;
        new_var = new_symbol(module_context, module_context->current_scope, uniquestr(global_names[i].name));
        new_var->locus = locus;
        new_var->kind = SK_VARIABLE;
        symbol_entity_specs_set_in_module(new_var, ieee_exceptions);
        new_var->type_information = global_names[i].type;
        new_var->value = global_names[i].value;
        symbol_entity_specs_set_access(new_var, AS_PUBLIC);

        symbol_entity_specs_add_related_symbols(ieee_exceptions, new_var);
    }
}

static void fortran_init_intrinsic_module_ieee_arithmetic(const decl_context_t* decl_context)
{
    // Initialize IEEE_ARITHMETIC
    const decl_context_t* module_context = new_program_unit_context(decl_context);

    const locus_t* locus = make_locus("(ieee_arithmetic)", 0, 0);

    const char* ieee_arithmetic_name = UNIQUESTR_LITERAL("ieee_arithmetic");

    scope_entry_t* ieee_arithmetic = new_symbol(decl_context, decl_context->current_scope, ieee_arithmetic_name);
    ieee_arithmetic->kind = SK_MODULE;
    symbol_entity_specs_set_is_builtin(ieee_arithmetic, 1);
    ieee_arithmetic->related_decl_context = module_context;
    ieee_arithmetic->defined = 1;

    rb_tree_insert(CURRENT_COMPILED_FILE->module_file_cache, ieee_arithmetic_name, ieee_arithmetic);

    module_context->current_scope->related_entry = ieee_arithmetic;

    int i;
    // Private types
    scope_entry_t *ieee_class = NULL, *ieee_round = NULL;
    struct private_types_tag
    {
        const char* name;
        scope_entry_t** p;
    } private_types[] = {
        { "ieee_class_type", &ieee_class },
        { "ieee_round_type", &ieee_round },
        { NULL, NULL }
    };

    for (i = 0; private_types[i].name != NULL; i++)
    {
        scope_entry_t* new_type = NULL;
        new_type = *(private_types[i].p) = new_symbol(module_context, module_context->current_scope,
                uniquestr(private_types[i].name));
        new_type->locus = locus;
        new_type->kind = SK_CLASS;
        symbol_entity_specs_set_in_module(new_type, ieee_arithmetic);
        new_type->type_information = get_new_class_type(module_context, TT_STRUCT);
        symbol_entity_specs_set_access(new_type, AS_PUBLIC);

        symbol_entity_specs_add_related_symbols(ieee_arithmetic,
                new_type);
    }

    // Global names
    type_t* ieee_class_type = get_const_qualified_type(get_user_defined_type(ieee_class));
    nodecl_t ieee_class_type_value = const_value_to_nodecl(
            const_value_make_struct(0, NULL,
                get_user_defined_type(ieee_class)));

    type_t* ieee_round_type = get_const_qualified_type(get_user_defined_type(ieee_round));
    nodecl_t ieee_round_type_value = const_value_to_nodecl(
            const_value_make_struct(0, NULL,
                get_user_defined_type(ieee_round)));

    struct global_vars_tag
    {
        const char* name;
        type_t* type;
        nodecl_t value;
    } global_names[] = {
        {"ieee_signaling_nan", ieee_class_type, ieee_class_type_value },
        {"ieee_quiet_nan", ieee_class_type, ieee_class_type_value },
        {"ieee_negative_inf", ieee_class_type, ieee_class_type_value },
        {"ieee_negative_normal", ieee_class_type, ieee_class_type_value },
        {"ieee_negative_denormal", ieee_class_type, ieee_class_type_value },
        {"ieee_negative_zero", ieee_class_type, ieee_class_type_value },
        {"ieee_positive_zero", ieee_class_type, ieee_class_type_value },
        {"ieee_positive_denormal", ieee_class_type, ieee_class_type_value },
        {"ieee_positive_normal", ieee_class_type, ieee_class_type_value },
        {"ieee_positive_inf", ieee_class_type, ieee_class_type_value },

        {"ieee_nearest", ieee_round_type, ieee_round_type_value },
        {"ieee_to_zero", ieee_round_type, ieee_round_type_value },
        {"ieee_up", ieee_round_type, ieee_round_type_value },
        {"ieee_down", ieee_round_type, ieee_round_type_value },
        {"ieee_other", ieee_round_type, ieee_round_type_value },

        { NULL, NULL, nodecl_null() }
    };

    for (i = 0; global_names[i].name != NULL; i++)
    {
        scope_entry_t* new_var = NULL;
        new_var = new_symbol(module_context, module_context->current_scope, uniquestr(global_names[i].name));
        new_var->locus = locus;
        new_var->kind = SK_VARIABLE;
        symbol_entity_specs_set_in_module(new_var, ieee_arithmetic);
        new_var->type_information = global_names[i].type;
        new_var->value = global_names[i].value;
        symbol_entity_specs_set_access(new_var, AS_PUBLIC);

        symbol_entity_specs_add_related_symbols(ieee_arithmetic,
                new_var);
    }

    // OPERATOR(==) for IEEE_CLASS_TYPE and for IEEE_ROUND_TYPE
    //
    // OPERATOR(==) IEEE_CLASS_TYPE
    struct operator_eq_neq_tag
    {
        const char* operator_class_type;
        const char* operator_round_type;
        const char* operator_generic;
    }  operator_eq_neq[] =
    {
        { "operator_eq_class_type",  "operator_eq_round_type",  ".operator.==" },
        { "operator_neq_class_type", "operator_neq_round_type", ".operator./=" },
        { NULL, NULL, NULL }
    };

    for (i = 0; operator_eq_neq[i].operator_class_type != NULL; i++)
    {
        scope_entry_t* new_operator_eq_class_type = NULL;

        new_operator_eq_class_type = new_symbol(module_context, module_context->current_scope,
                uniquestr(operator_eq_neq[i].operator_class_type));
        new_operator_eq_class_type->kind = SK_FUNCTION;
        new_operator_eq_class_type->locus = locus;
        parameter_info_t eq_class_type_parameter_info[2] =
        {
            { 0, lvalue_ref(ieee_class_type), NULL },
            { 0, lvalue_ref(ieee_class_type), NULL }
        };
        new_operator_eq_class_type->type_information =
            get_new_function_type(
                    fortran_get_default_logical_type(),
                    eq_class_type_parameter_info, 2, REF_QUALIFIER_NONE);
        symbol_entity_specs_set_in_module(new_operator_eq_class_type, ieee_arithmetic);
        symbol_entity_specs_set_access(new_operator_eq_class_type, AS_PRIVATE);

        // Keywords
        int j;
        for (j = 0; j < 2; j++)
        {
            scope_entry_t* new_keyword_sym = NEW0(scope_entry_t);
            new_keyword_sym->kind = SK_VARIABLE;
            uniquestr_sprintf(&new_keyword_sym->symbol_name, "A%d", (j+1));
            new_keyword_sym->decl_context = new_operator_eq_class_type->decl_context;
            new_keyword_sym->type_information = lvalue_ref(ieee_class_type);

            new_keyword_sym->do_not_print = 1;

            symbol_set_as_parameter_of_function(new_keyword_sym, 
                    new_operator_eq_class_type,
                    /* nesting */ 0,
                    /* position */ symbol_entity_specs_get_num_related_symbols(new_operator_eq_class_type));

            symbol_entity_specs_add_related_symbols(new_operator_eq_class_type, new_keyword_sym);
        }

        // OPERATOR IEEE_ROUND_TYPE
        scope_entry_t* new_operator_eq_round_type = NULL;
        new_operator_eq_round_type = new_symbol(module_context, module_context->current_scope,
                uniquestr(operator_eq_neq[i].operator_round_type));
        new_operator_eq_round_type->kind = SK_FUNCTION;
        new_operator_eq_round_type->locus = locus;
        parameter_info_t eq_round_type_parameter_info[2] =
        {
            { 0, lvalue_ref(ieee_round_type), NULL },
            { 0, lvalue_ref(ieee_round_type), NULL }
        };
        new_operator_eq_round_type->type_information =
            get_new_function_type(
                    fortran_get_default_logical_type(),
                    eq_round_type_parameter_info, 2, REF_QUALIFIER_NONE);
        symbol_entity_specs_set_in_module(new_operator_eq_round_type, ieee_arithmetic);
        symbol_entity_specs_set_access(new_operator_eq_round_type, AS_PRIVATE);

        for (j = 0; j < 2; j++)
        {
            scope_entry_t* new_keyword_sym = NEW0(scope_entry_t);
            new_keyword_sym->kind = SK_VARIABLE;
            uniquestr_sprintf(&new_keyword_sym->symbol_name, "A%d", (j+1));
            new_keyword_sym->decl_context = new_operator_eq_round_type->decl_context;
            new_keyword_sym->type_information = lvalue_ref(ieee_round_type);

            new_keyword_sym->do_not_print = 1;

            symbol_set_as_parameter_of_function(new_keyword_sym, 
                    new_operator_eq_round_type,
                    /* nesting */ 0,
                    /* position */ symbol_entity_specs_get_num_related_symbols(new_operator_eq_round_type));

            symbol_entity_specs_add_related_symbols(new_operator_eq_round_type, new_keyword_sym);
        }

        // OPERATOR Generic
        scope_entry_t* new_operator_eq = NULL;

        new_operator_eq = new_symbol(module_context,
                module_context->current_scope,
                uniquestr(operator_eq_neq[i].operator_generic));
        new_operator_eq->kind = SK_GENERIC_NAME;
        new_operator_eq->locus = locus;
        new_operator_eq->type_information = get_void_type();
        symbol_entity_specs_set_in_module(new_operator_eq, ieee_arithmetic);
        symbol_entity_specs_set_is_implicit_basic_type(new_operator_eq, 0);

        symbol_entity_specs_add_related_symbols(new_operator_eq,
                new_operator_eq_class_type);
        symbol_entity_specs_add_related_symbols(new_operator_eq,
                new_operator_eq_round_type);
        symbol_entity_specs_add_related_symbols(ieee_arithmetic,
                new_operator_eq);
    }
}

static void fortran_init_intrinsic_module_ieee_features(const decl_context_t* decl_context)
{
    // Initialize IEEE_FEATURES
    const decl_context_t* module_context = new_program_unit_context(decl_context);

    const locus_t* locus = make_locus("(ieee_features)", 0, 0);

    const char* ieee_features_name = UNIQUESTR_LITERAL("ieee_features");

    scope_entry_t* ieee_features = new_symbol(decl_context, decl_context->current_scope, ieee_features_name);
    ieee_features->locus = locus;
    ieee_features->kind = SK_MODULE;
    symbol_entity_specs_set_is_builtin(ieee_features, 1);
    ieee_features->related_decl_context = module_context;
    ieee_features->defined = 1;

    rb_tree_insert(CURRENT_COMPILED_FILE->module_file_cache, ieee_features_name, ieee_features);

    module_context->current_scope->related_entry = ieee_features;

    int i;
    // Private types
    scope_entry_t* ieee_features_sym = NULL;
    struct private_types_tag
    {
        const char* name;
        scope_entry_t** p;
    } private_types[] = {
        { "ieee_features_type", &ieee_features_sym },
        { NULL, NULL }
    };

    for (i = 0; private_types[i].name != NULL; i++)
    {
        scope_entry_t* new_type = NULL;
        new_type = *(private_types[i].p) = new_symbol(module_context, module_context->current_scope,
                uniquestr(private_types[i].name));
        new_type->locus = locus;
        new_type->kind = SK_CLASS;
        symbol_entity_specs_set_in_module(new_type, ieee_features);
        new_type->type_information = get_new_class_type(module_context, TT_STRUCT);
        symbol_entity_specs_set_access(new_type, AS_PUBLIC);

        symbol_entity_specs_add_related_symbols(ieee_features, new_type);
    }

    // Global names
    type_t* ieee_features_type = get_const_qualified_type(get_user_defined_type(ieee_features_sym));
    nodecl_t ieee_features_type_value = const_value_to_nodecl(
            const_value_make_struct(0, NULL,
                get_user_defined_type(ieee_features_sym)));

    struct global_vars_tag
    {
        const char* name;
        type_t* type;
        nodecl_t value;
    } global_names[] = {
        {"ieee_datatype", ieee_features_type, ieee_features_type_value },
        {"ieee_denormal", ieee_features_type, ieee_features_type_value },
        {"ieee_divide", ieee_features_type, ieee_features_type_value },
        {"ieee_halting", ieee_features_type, ieee_features_type_value },
        {"ieee_inexact_flag", ieee_features_type, ieee_features_type_value },
        {"ieee_inf", ieee_features_type, ieee_features_type_value },
        {"ieee_invalid_flag", ieee_features_type, ieee_features_type_value },
        {"ieee_nan", ieee_features_type, ieee_features_type_value },
        {"ieee_rounding", ieee_features_type, ieee_features_type_value },
        {"ieee_sqrt", ieee_features_type, ieee_features_type_value },
        {"ieee_underflow_flag", ieee_features_type, ieee_features_type_value },
        { NULL, NULL, nodecl_null() }
    };

    for (i = 0; global_names[i].name != NULL; i++)
    {
        scope_entry_t* new_var = NULL;
        new_var = new_symbol(module_context, module_context->current_scope, uniquestr(global_names[i].name));
        new_var->locus = locus;
        new_var->kind = SK_VARIABLE;
        symbol_entity_specs_set_in_module(new_var, ieee_features);
        new_var->type_information = global_names[i].type;
        new_var->value = global_names[i].value;
        symbol_entity_specs_set_access(new_var, AS_PUBLIC);

        symbol_entity_specs_add_related_symbols(ieee_features, new_var);
    }
}

static void fortran_init_intrinsic_module_ieee(const decl_context_t* decl_context)
{
    fortran_init_intrinsic_module_ieee_exceptions(decl_context);
    fortran_init_intrinsic_module_ieee_arithmetic(decl_context);
    fortran_init_intrinsic_module_ieee_features(decl_context);
}

static type_t* get_type_from_module(const char* module_name, const char* type_name)
{
    scope_entry_t* module = get_module_in_cache(module_name);
    scope_entry_list_t* res = fortran_query_module_for_name(module, type_name);
    scope_entry_t* sym = (res != NULL) ? entry_list_head(res) : NULL;
    entry_list_free(res);

    ERROR_CONDITION(res == NULL, "Type %s.%s does not exist\n", module_name, type_name);

    return get_user_defined_type(sym);
}


static type_t* get_ieee_flag_type(void)
{
    return get_type_from_module("ieee_exceptions", "ieee_flag_type");
}

static type_t* get_ieee_status_type(void)
{
    return get_type_from_module("ieee_exceptions", "ieee_status_type");
}

static type_t* get_ieee_round_type(void)
{
    return get_type_from_module("ieee_arithmetic", "ieee_round_type");
}

static type_t* get_ieee_class_type(void)
{
    return get_type_from_module("ieee_arithmetic", "ieee_class_type");
}

static scope_entry_t* compute_intrinsic_ieee_get_halting_mode(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));
    type_t* t1 = no_ref(fortran_get_rank0_type(argument_types[1]));

    if (equivalent_types(get_unqualified_type(t0), get_ieee_flag_type())
            && is_bool_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_get_halting_mode", get_void_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_copy_sign(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));
    type_t* t1 = no_ref(fortran_get_rank0_type(argument_types[1]));

    if (is_floating_type(t0)
            && is_floating_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_copy_sign", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_set_halting_mode(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t *t0 = no_ref(fortran_get_rank0_type(argument_types[0]));
    type_t *t1 = no_ref(fortran_get_rank0_type(argument_types[1]));

    if (equivalent_types(get_unqualified_type(t0), get_ieee_flag_type())
            && equivalent_types(get_unqualified_type(t1), fortran_get_default_logical_type()))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_set_halting_mode", get_void_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_rem(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));
    type_t* t1 = no_ref(fortran_get_rank0_type(argument_types[1]));

    if (is_floating_type(t0)
            && is_floating_type(t1))
    {
        // FIXME - We should choose the most precise of the both types. Now we
        // return the first one
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_rem", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_set_status(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);

    if (equivalent_types(get_unqualified_type(t0), get_ieee_status_type()))
    {
        return GET_INTRINSIC_PURE(symbol, "ieee_set_status", get_void_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_support_flag(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    type_t* t1 = no_ref(argument_types[1] != NULL ? fortran_get_rank0_type(argument_types[1]) : fortran_get_default_real_type());

    if (equivalent_types(get_unqualified_type(t0), get_ieee_flag_type())
            && is_floating_type(t1))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "ieee_support_flag", fortran_get_default_logical_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_support_inf(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0] != NULL ? fortran_get_rank0_type(argument_types[0]) : fortran_get_default_real_type());

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "ieee_support_inf", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_value(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    type_t* t1 = no_ref(argument_types[1]);

    if (is_floating_type(t0)
            && equivalent_types(get_unqualified_type(t1), get_ieee_class_type()))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_value", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_logb(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_logb", t0,
                lvalue_ref(t0));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_ieee_selected_real_kind(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0] == NULL ? fortran_get_default_integer_type() : argument_types[0]);
    type_t* t1 = no_ref(argument_types[1] == NULL ? fortran_get_default_integer_type() : argument_types[1]);

    if (is_integer_type(t0) &&
            is_integer_type(t1))
    {
        return GET_INTRINSIC_TRANSFORMATIONAL(symbol, "ieee_selected_real_kind", fortran_get_default_integer_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_is_negative(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_is_negative", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_get_rounding_mode(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t *t0 = no_ref(argument_types[0]);

    if (equivalent_types(get_unqualified_type(t0), get_ieee_round_type()))
    {
        return GET_INTRINSIC_PURE(symbol, "ieee_get_rounding_mode", get_void_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_is_finite(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_is_finite", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_is_normal(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_is_normal", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_support_halting(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));

    if (equivalent_types(get_unqualified_type(t0), get_ieee_flag_type()))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "ieee_support_halting", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_get_status(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t *t0 = no_ref(argument_types[0]);

    if (equivalent_types(get_unqualified_type(t0), get_ieee_status_type()))
    {
        return GET_INTRINSIC_PURE(symbol, "ieee_get_status", get_void_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_set_flag(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t *t0 = no_ref(fortran_get_rank0_type(argument_types[0]));
    type_t *t1 = no_ref(fortran_get_rank0_type(argument_types[1]));

    if (equivalent_types(get_unqualified_type(t0), get_ieee_flag_type())
            && equivalent_types(get_unqualified_type(t1), fortran_get_default_logical_type()))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_set_flag", get_void_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

scope_entry_t* compute_intrinsic_ieee_get_flag(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));
    type_t* t1 = no_ref(fortran_get_rank0_type(argument_types[1]));

    if (equivalent_types(get_unqualified_type(t0), get_ieee_flag_type())
            && is_bool_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_get_flag", get_void_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_unordered(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));
    type_t* t1 = no_ref(fortran_get_rank0_type(argument_types[1]));

    if (is_floating_type(t0)
            && is_floating_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_unordered", fortran_get_default_logical_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_support_denormal(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0] != NULL ? fortran_get_rank0_type(argument_types[0]) : fortran_get_default_real_type());

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "ieee_support_denormal", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_support_sqrt(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0] != NULL ? fortran_get_rank0_type(argument_types[0]) : fortran_get_default_real_type());

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "ieee_support_sqrt", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_is_next_after(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));
    type_t* t1 = no_ref(fortran_get_rank0_type(argument_types[1]));

    if (is_floating_type(t0)
            && is_floating_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_next_after", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_support_datatype(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0] != NULL ? argument_types[0] : fortran_get_default_real_type());

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "ieee_support_datatype", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_class(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0] != NULL ? fortran_get_rank0_type(argument_types[0]) : fortran_get_default_real_type());

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_class", get_ieee_class_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_rint(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_rint", t0,
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_set_rounding_mode(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);

    if (equivalent_types(get_unqualified_type(t0), get_ieee_round_type()))
    {
        return GET_INTRINSIC_PURE(symbol, "ieee_set_rounding_mode", get_void_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_support_standard(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0] != NULL ? fortran_get_rank0_type(argument_types[0]) : fortran_get_default_real_type());

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "ieee_support_standard", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_support_divide(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0] != NULL ? fortran_get_rank0_type(argument_types[0]) : fortran_get_default_real_type());

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "ieee_support_divide", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_support_rounding(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0]);
    type_t* t1 = no_ref(argument_types[1] != NULL ? fortran_get_rank0_type(argument_types[1]) : fortran_get_default_real_type());

    if (equivalent_types(get_unqualified_type(t0), get_ieee_round_type())
            && is_floating_type(t1))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "ieee_support_rounding", fortran_get_default_logical_type(),
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_scalb(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));
    type_t* t1 = no_ref(fortran_get_rank0_type(argument_types[1]));

    if (is_floating_type(t0)
            && is_integer_type(t1))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_scalb", t0,
                lvalue_ref(t0),
                lvalue_ref(t1));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_support_nan(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(argument_types[0] != NULL ? fortran_get_rank0_type(argument_types[0]) : fortran_get_default_real_type());

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_INQUIRY(symbol, "ieee_support_nan", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static scope_entry_t* compute_intrinsic_ieee_is_nan(scope_entry_t* symbol UNUSED_PARAMETER,
        type_t** argument_types UNUSED_PARAMETER,
        nodecl_t* argument_expressions UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        const_value_t** const_value UNUSED_PARAMETER)
{
    type_t* t0 = no_ref(fortran_get_rank0_type(argument_types[0]));

    if (is_floating_type(t0))
    {
        return GET_INTRINSIC_ELEMENTAL(symbol, "ieee_is_nan", fortran_get_default_logical_type(),
                lvalue_ref(t0));
    }

    return NULL;
}

static void fortran_init_intrinsic_modules(const decl_context_t* decl_context)
{
    fortran_init_intrinsic_module_iso_c_binding(decl_context);
    fortran_init_intrinsic_module_ieee(decl_context);
}

static void fortran_finish_intrinsic_modules(const decl_context_t* decl_context UNUSED_PARAMETER)
{
    // Finish modules
    //
    // IEEE_ARITHMETICS modules has a USE IEEE_EXCEPTIONS
    scope_entry_t* ieee_arithmetic = get_module_in_cache("ieee_arithmetic");
    scope_entry_t* ieee_exceptions = get_module_in_cache("ieee_exceptions");

    int i;
    for (i = 0; i < symbol_entity_specs_get_num_related_symbols(ieee_exceptions); i++)
    {
        scope_entry_t* sym_in_module = symbol_entity_specs_get_related_symbols_num(ieee_exceptions, i);

        if (symbol_entity_specs_get_access(sym_in_module) == AS_PRIVATE)
            continue;

        insert_symbol_from_module(sym_in_module,
                ieee_arithmetic->related_decl_context,
                sym_in_module->symbol_name,
                ieee_exceptions,
                make_locus("", 0, 0));
    }
}

static void fortran_create_scope_for_intrinsics(const decl_context_t* decl_context)
{
    scope_entry_t* fortran_intrinsics = new_symbol(decl_context, decl_context->current_scope,
            UNIQUESTR_LITERAL(".fortran_intrinsics"));
    fortran_intrinsics->kind = SK_NAMESPACE;
    fortran_intrinsics->related_decl_context = new_namespace_context(decl_context, fortran_intrinsics);
}

const decl_context_t* fortran_get_context_of_intrinsics(const decl_context_t* decl_context)
{
    decl_context_t* global_context = decl_context_clone(decl_context);
    global_context->current_scope = decl_context->global_scope;

    scope_entry_list_t* global_list = query_in_scope_str(global_context, UNIQUESTR_LITERAL(".fortran_intrinsics"), NULL);

    ERROR_CONDITION(global_list == NULL, "Fortran intrinsics context was requested but it is missing", 0);

    scope_entry_t* symbol = entry_list_head(global_list);
    entry_list_free(global_list);

    ERROR_CONDITION(symbol->kind != SK_NAMESPACE, "Invalid symbol .fortran_intrinsics", 0);

    return symbol->related_decl_context;
}


