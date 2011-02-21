#include "fortran03-intrinsics.h"

#define A  ATOMIC_SUBROUTINE
#define E  ELEMENTAL_FUNCTION
#define ES ELEMENTAL_SUBROUTINE
#define I  INQUIRY_FUNCTION
#define PS PURE_SUBROUTINE
#define S  IMPURE_SUBROUTINE
#define T  TRANSFORMATIONAL_FUNCTION
#define ET (E | T)

#define FORTRAN_INTRINSIC_GENERIC_LIST \
FORTRAN_GENERIC_INTRINSIC(abs, E) \
FORTRAN_GENERIC_INTRINSIC(achar, E) \
FORTRAN_GENERIC_INTRINSIC(acos, E) \
FORTRAN_GENERIC_INTRINSIC(acosh, E) \
FORTRAN_GENERIC_INTRINSIC(adjustl, E) \
FORTRAN_GENERIC_INTRINSIC(adjustr, E) \
FORTRAN_GENERIC_INTRINSIC(aimag, E) \
FORTRAN_GENERIC_INTRINSIC(aint, E) \
FORTRAN_GENERIC_INTRINSIC(all, T) \
FORTRAN_GENERIC_INTRINSIC(allocated, I) \
FORTRAN_GENERIC_INTRINSIC(anint, E) \
FORTRAN_GENERIC_INTRINSIC(any, T) \
FORTRAN_GENERIC_INTRINSIC(asin, E) \
FORTRAN_GENERIC_INTRINSIC(asinh, E) \
FORTRAN_GENERIC_INTRINSIC(associated, I) \
FORTRAN_GENERIC_INTRINSIC(atan, E) \
FORTRAN_GENERIC_INTRINSIC(atan2, E) \
FORTRAN_GENERIC_INTRINSIC(atanh, E) \
FORTRAN_GENERIC_INTRINSIC(atomic_define, A) \
FORTRAN_GENERIC_INTRINSIC(atomic_ref, A) \
FORTRAN_GENERIC_INTRINSIC(bessel_j0, E) \
FORTRAN_GENERIC_INTRINSIC(bessel_j1, E) \
FORTRAN_GENERIC_INTRINSIC(bessel_jn, ET) \
FORTRAN_GENERIC_INTRINSIC(bessel_y0, E) \
FORTRAN_GENERIC_INTRINSIC(bessel_y1, E) \
FORTRAN_GENERIC_INTRINSIC(bessel_yn, ET) \
FORTRAN_GENERIC_INTRINSIC(bge, E) \
FORTRAN_GENERIC_INTRINSIC(bgt, E) \
FORTRAN_GENERIC_INTRINSIC(ble, E) \
FORTRAN_GENERIC_INTRINSIC(blt, E) \
FORTRAN_GENERIC_INTRINSIC(bit_size, I) \
FORTRAN_GENERIC_INTRINSIC(btest, E) \
FORTRAN_GENERIC_INTRINSIC(ceiling, E) \
FORTRAN_GENERIC_INTRINSIC(char, E) \
FORTRAN_GENERIC_INTRINSIC(cmplx, E) \
FORTRAN_GENERIC_INTRINSIC(command_argument_count, T) \
FORTRAN_GENERIC_INTRINSIC(conjg, E) \
FORTRAN_GENERIC_INTRINSIC(cos, E) \
FORTRAN_GENERIC_INTRINSIC(cosh, E) \
FORTRAN_GENERIC_INTRINSIC(count, T) \
FORTRAN_GENERIC_INTRINSIC(cpu_time, S) \
FORTRAN_GENERIC_INTRINSIC(cshift, T) \
FORTRAN_GENERIC_INTRINSIC(date_and_time, S) \
FORTRAN_GENERIC_INTRINSIC(dble, E) \
FORTRAN_GENERIC_INTRINSIC(digits, I) \
FORTRAN_GENERIC_INTRINSIC(dim, E) \
FORTRAN_GENERIC_INTRINSIC(dot_product, T) \
FORTRAN_GENERIC_INTRINSIC(dprod, E) \
FORTRAN_GENERIC_INTRINSIC(dshiftl, E) \
FORTRAN_GENERIC_INTRINSIC(dshiftr, E) \
FORTRAN_GENERIC_INTRINSIC(eoshift, T) \
FORTRAN_GENERIC_INTRINSIC(epsilon, I) \
FORTRAN_GENERIC_INTRINSIC(erf, E) \
FORTRAN_GENERIC_INTRINSIC(erfc, E) \
FORTRAN_GENERIC_INTRINSIC(erfc_scaled, E) \
FORTRAN_GENERIC_INTRINSIC(execute_command_line, S) \
FORTRAN_GENERIC_INTRINSIC(exp, E) \
FORTRAN_GENERIC_INTRINSIC(exponent, E) \
FORTRAN_GENERIC_INTRINSIC(extends_type_of, I) \
FORTRAN_GENERIC_INTRINSIC(findloc, T) \
FORTRAN_GENERIC_INTRINSIC(floor, E) \
FORTRAN_GENERIC_INTRINSIC(fraction, E) \
FORTRAN_GENERIC_INTRINSIC(gamma, E) \
FORTRAN_GENERIC_INTRINSIC(get_command, S) \
FORTRAN_GENERIC_INTRINSIC(get_command_argument, S) \
FORTRAN_GENERIC_INTRINSIC(get_environment_variable, S) \
FORTRAN_GENERIC_INTRINSIC(huge, I) \
FORTRAN_GENERIC_INTRINSIC(hypot, E) \
FORTRAN_GENERIC_INTRINSIC(iachar, E) \
FORTRAN_GENERIC_INTRINSIC(iall, T) \
FORTRAN_GENERIC_INTRINSIC(iand, E) \
FORTRAN_GENERIC_INTRINSIC(iany, T) \
FORTRAN_GENERIC_INTRINSIC(ibclr, E) \
FORTRAN_GENERIC_INTRINSIC(ibits, E) \
FORTRAN_GENERIC_INTRINSIC(ibset, E) \
FORTRAN_GENERIC_INTRINSIC(ichar, E) \
FORTRAN_GENERIC_INTRINSIC(ieor, E) \
FORTRAN_GENERIC_INTRINSIC(image_index, I) \
FORTRAN_GENERIC_INTRINSIC(index, E) \
FORTRAN_GENERIC_INTRINSIC(int, E) \
FORTRAN_GENERIC_INTRINSIC(ior, E) \
FORTRAN_GENERIC_INTRINSIC(iparity, T) \
FORTRAN_GENERIC_INTRINSIC(ishft, E) \
FORTRAN_GENERIC_INTRINSIC(ishftc, E) \
FORTRAN_GENERIC_INTRINSIC(is_contiguous, I) \
FORTRAN_GENERIC_INTRINSIC(is_iostat_end, E) \
FORTRAN_GENERIC_INTRINSIC(is_iostat_eor, E) \
FORTRAN_GENERIC_INTRINSIC(kind, I) \
FORTRAN_GENERIC_INTRINSIC(lbound, I) \
FORTRAN_GENERIC_INTRINSIC(lcobound, I) \
FORTRAN_GENERIC_INTRINSIC(leadz, E) \
FORTRAN_GENERIC_INTRINSIC(len, I) \
FORTRAN_GENERIC_INTRINSIC(len_trim, E) \
FORTRAN_GENERIC_INTRINSIC(lge, E) \
FORTRAN_GENERIC_INTRINSIC(lgt, E) \
FORTRAN_GENERIC_INTRINSIC(lle, E) \
FORTRAN_GENERIC_INTRINSIC(llt, E) \
FORTRAN_GENERIC_INTRINSIC(log, E) \
FORTRAN_GENERIC_INTRINSIC(log_gamma, E) \
FORTRAN_GENERIC_INTRINSIC(logical, E) \
FORTRAN_GENERIC_INTRINSIC(maskl, E) \
FORTRAN_GENERIC_INTRINSIC(maskr, E) \
FORTRAN_GENERIC_INTRINSIC(matmul, T) \
FORTRAN_GENERIC_INTRINSIC(max, E) \
FORTRAN_GENERIC_INTRINSIC(maxexponent, I) \
FORTRAN_GENERIC_INTRINSIC(maxloc, T) \
FORTRAN_GENERIC_INTRINSIC(maxval, T) \
FORTRAN_GENERIC_INTRINSIC(merge, E) \
FORTRAN_GENERIC_INTRINSIC(merge_bits, E) \
FORTRAN_GENERIC_INTRINSIC(min, E) \
FORTRAN_GENERIC_INTRINSIC(minexponent, I) \
FORTRAN_GENERIC_INTRINSIC(minloc, T) \
FORTRAN_GENERIC_INTRINSIC(minval, T) \
FORTRAN_GENERIC_INTRINSIC(mod, E) \
FORTRAN_GENERIC_INTRINSIC(modulo, E) \
FORTRAN_GENERIC_INTRINSIC(move_alloc, PS) \
FORTRAN_GENERIC_INTRINSIC(mvbits, E) \
FORTRAN_GENERIC_INTRINSIC(nearest, E) \
FORTRAN_GENERIC_INTRINSIC(new_line, I) \
FORTRAN_GENERIC_INTRINSIC(nint, E) \
FORTRAN_GENERIC_INTRINSIC(not, E) \
FORTRAN_GENERIC_INTRINSIC(norm2, T) \
FORTRAN_GENERIC_INTRINSIC(null, T) \
FORTRAN_GENERIC_INTRINSIC(num_images, T) \
FORTRAN_GENERIC_INTRINSIC(parity, T) \
FORTRAN_GENERIC_INTRINSIC(popcnt, E) \
FORTRAN_GENERIC_INTRINSIC(poppar, E) \
FORTRAN_GENERIC_INTRINSIC(precision, I) \
FORTRAN_GENERIC_INTRINSIC(present, I) \
FORTRAN_GENERIC_INTRINSIC(product, T) \
FORTRAN_GENERIC_INTRINSIC(radix, I) \
FORTRAN_GENERIC_INTRINSIC(random_number, S) \
FORTRAN_GENERIC_INTRINSIC(random_seed, S) \
FORTRAN_GENERIC_INTRINSIC(range, I) \
FORTRAN_GENERIC_INTRINSIC(real, E) \
FORTRAN_GENERIC_INTRINSIC(repeat, T) \
FORTRAN_GENERIC_INTRINSIC(reshape, T) \
FORTRAN_GENERIC_INTRINSIC(rrspacing, E) \
FORTRAN_GENERIC_INTRINSIC(same_type_as, I) \
FORTRAN_GENERIC_INTRINSIC(scale, E) \
FORTRAN_GENERIC_INTRINSIC(scan, E) \
FORTRAN_GENERIC_INTRINSIC(selected_char_kind, T) \
FORTRAN_GENERIC_INTRINSIC(selected_int_kind, T) \
FORTRAN_GENERIC_INTRINSIC(selected_real_kind, T) \
FORTRAN_GENERIC_INTRINSIC(set_exponent, E) \
FORTRAN_GENERIC_INTRINSIC(shape, I) \
FORTRAN_GENERIC_INTRINSIC(shifta, E) \
FORTRAN_GENERIC_INTRINSIC(shiftl, E) \
FORTRAN_GENERIC_INTRINSIC(shiftr, E) \
FORTRAN_GENERIC_INTRINSIC(sign, E) \
FORTRAN_GENERIC_INTRINSIC(sin, E) \
FORTRAN_GENERIC_INTRINSIC(sinh, E) \
FORTRAN_GENERIC_INTRINSIC(size, I) \
FORTRAN_GENERIC_INTRINSIC(spacing, E) \
FORTRAN_GENERIC_INTRINSIC(spread, T) \
FORTRAN_GENERIC_INTRINSIC(sqrt, E) \
FORTRAN_GENERIC_INTRINSIC(storage_size, I) \
FORTRAN_GENERIC_INTRINSIC(sum, T) \
FORTRAN_GENERIC_INTRINSIC(system_clock, S) \
FORTRAN_GENERIC_INTRINSIC(tan, E) \
FORTRAN_GENERIC_INTRINSIC(tanh, E) \
FORTRAN_GENERIC_INTRINSIC(this_image, T) \
FORTRAN_GENERIC_INTRINSIC(tiny, I) \
FORTRAN_GENERIC_INTRINSIC(trailz, E) \
FORTRAN_GENERIC_INTRINSIC(transfer, T) \
FORTRAN_GENERIC_INTRINSIC(transpose, T) \
FORTRAN_GENERIC_INTRINSIC(trim, T) \
FORTRAN_GENERIC_INTRINSIC(ubound, I) \
FORTRAN_GENERIC_INTRINSIC(ucobound, I) \
FORTRAN_GENERIC_INTRINSIC(unpack, T) \
FORTRAN_GENERIC_INTRINSIC(verify, E) 

// 1 means not valid as an actual argument
#define FORTRAN_INTRINSIC_SPECIFIC_LIST \
FORTRAN_SPECIFIC_INTRINSIC(abs, 0) \
FORTRAN_SPECIFIC_INTRINSIC(acos, 0) \
FORTRAN_SPECIFIC_INTRINSIC(aimag, 0) \
FORTRAN_SPECIFIC_INTRINSIC(aint, 0) \
FORTRAN_SPECIFIC_INTRINSIC(alog, 0) \
FORTRAN_SPECIFIC_INTRINSIC(alog10, 0) \
FORTRAN_SPECIFIC_INTRINSIC(amax0, 1) \
FORTRAN_SPECIFIC_INTRINSIC(amax1, 1) \
FORTRAN_SPECIFIC_INTRINSIC(amin0, 1) \
FORTRAN_SPECIFIC_INTRINSIC(amin1, 1) \
FORTRAN_SPECIFIC_INTRINSIC(amod, 0) \
FORTRAN_SPECIFIC_INTRINSIC(anint, 0) \
FORTRAN_SPECIFIC_INTRINSIC(asin, 0) \
FORTRAN_SPECIFIC_INTRINSIC(atan, 0) \
FORTRAN_SPECIFIC_INTRINSIC(atan2, 0) \
FORTRAN_SPECIFIC_INTRINSIC(cabs, 0) \
FORTRAN_SPECIFIC_INTRINSIC(ccos, 0) \
FORTRAN_SPECIFIC_INTRINSIC(cexp, 0) \
FORTRAN_SPECIFIC_INTRINSIC(char, 1) \
FORTRAN_SPECIFIC_INTRINSIC(clog, 0) \
FORTRAN_SPECIFIC_INTRINSIC(conjg, 0) \
FORTRAN_SPECIFIC_INTRINSIC(cos, 0) \
FORTRAN_SPECIFIC_INTRINSIC(cosh, 0) \
FORTRAN_SPECIFIC_INTRINSIC(csin, 0) \
FORTRAN_SPECIFIC_INTRINSIC(csqrt, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dabs, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dacos, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dasin, 0) \
FORTRAN_SPECIFIC_INTRINSIC(datan, 0) \
FORTRAN_SPECIFIC_INTRINSIC(datan2, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dcos, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dcosh, 0) \
FORTRAN_SPECIFIC_INTRINSIC(ddim, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dexp, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dim, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dint, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dlog, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dlog10, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dmax1, 1) \
FORTRAN_SPECIFIC_INTRINSIC(dmin1, 1) \
FORTRAN_SPECIFIC_INTRINSIC(dmod, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dnint, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dprod, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dsign, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dsin, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dsinh, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dsqrt, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dtan, 0) \
FORTRAN_SPECIFIC_INTRINSIC(dtanh, 0) \
FORTRAN_SPECIFIC_INTRINSIC(exp, 0) \
FORTRAN_SPECIFIC_INTRINSIC(float, 1) \
FORTRAN_SPECIFIC_INTRINSIC(iabs, 0) \
FORTRAN_SPECIFIC_INTRINSIC(ichar, 1) \
FORTRAN_SPECIFIC_INTRINSIC(idim, 0) \
FORTRAN_SPECIFIC_INTRINSIC(idint, 1) \
FORTRAN_SPECIFIC_INTRINSIC(idnint, 0) \
FORTRAN_SPECIFIC_INTRINSIC(ifix, 1) \
FORTRAN_SPECIFIC_INTRINSIC(index, 0) \
FORTRAN_SPECIFIC_INTRINSIC(int, 1) \
FORTRAN_SPECIFIC_INTRINSIC(isign, 0) \
FORTRAN_SPECIFIC_INTRINSIC(len, 0) \
FORTRAN_SPECIFIC_INTRINSIC(lge, 1) \
FORTRAN_SPECIFIC_INTRINSIC(lgt, 1) \
FORTRAN_SPECIFIC_INTRINSIC(lle, 1) \
FORTRAN_SPECIFIC_INTRINSIC(llt, 1) \
FORTRAN_SPECIFIC_INTRINSIC(max0, 1) \
FORTRAN_SPECIFIC_INTRINSIC(max1, 1) \
FORTRAN_SPECIFIC_INTRINSIC(min0, 1) \
FORTRAN_SPECIFIC_INTRINSIC(min1, 1) \
FORTRAN_SPECIFIC_INTRINSIC(mod, 0) \
FORTRAN_SPECIFIC_INTRINSIC(nint, 0) \
FORTRAN_SPECIFIC_INTRINSIC(real, 1) \
FORTRAN_SPECIFIC_INTRINSIC(sign, 0) \
FORTRAN_SPECIFIC_INTRINSIC(sin, 0) \
FORTRAN_SPECIFIC_INTRINSIC(sinh, 0) \
FORTRAN_SPECIFIC_INTRINSIC(sngl, 1) \
FORTRAN_SPECIFIC_INTRINSIC(sqrt, 0) \
FORTRAN_SPECIFIC_INTRINSIC(tan, 0) \
FORTRAN_SPECIFIC_INTRINSIC(tanh, 0) 


#undef A  
#undef E  
#undef ES 
#undef I  
#undef PS 
#undef S  
#undef T  

#define FORTRAN_GENERIC_INTRINSIC(name, _) \
    static void register_generic_intrinsic_##name(decl_context_t);
FORTRAN_INTRINSIC_GENERIC_LIST
#undef FORTRAN_GENERIC_INTRINSIC

static void register_generic_intrinsic_abs(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_achar(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_acos(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_acosh(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_adjustl(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_adjustr(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_aimag(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_aint(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_all(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_allocated(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_anint(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_any(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_asin(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_asinh(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_associated(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_atan(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_atan2(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_atanh(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_atomic_define(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_atomic_ref(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_bessel_j0(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_bessel_j1(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_bessel_jn(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_bessel_y0(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_bessel_y1(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_bessel_yn(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_bge(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_bgt(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_ble(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_blt(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_bit_size(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_btest(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_ceiling(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_char(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_cmplx(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_command_argument_count(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_conjg(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_cos(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_cosh(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_count(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_cpu_time(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_cshift(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_date_and_time(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_dble(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_digits(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_dim(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_dot_product(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_dprod(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_dshiftl(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_dshiftr(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_eoshift(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_epsilon(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_erf(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_erfc(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_erfc_scaled(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_execute_command_line(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_exp(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_exponent(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_extends_type_of(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_findloc(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_floor(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_fraction(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_gamma(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_get_command(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_get_command_argument(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_get_environment_variable(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_huge(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_hypot(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_iachar(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_iall(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_iand(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_iany(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_ibclr(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_ibits(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_ibset(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_ichar(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_ieor(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_image_index(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_index(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_int(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_ior(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_iparity(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_ishft(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_ishftc(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_is_contiguous(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_is_iostat_end(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_is_iostat_eor(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_kind(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_lbound(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_lcobound(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_leadz(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_len(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_len_trim(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_lge(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_lgt(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_lle(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_llt(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_log(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_log_gamma(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_logical(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_maskl(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_maskr(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_matmul(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_max(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_maxexponent(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_maxloc(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_maxval(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_merge(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_merge_bits(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_min(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_minexponent(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_minloc(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_minval(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_mod(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_modulo(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_move_alloc(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_mvbits(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_nearest(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_new_line(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_nint(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_not(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_norm2(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_null(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_num_images(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_parity(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_popcnt(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_poppar(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_precision(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_present(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_product(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_radix(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_random_number(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_random_seed(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_range(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_real(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_repeat(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_reshape(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_rrspacing(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_same_type_as(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_scale(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_scan(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_selected_char_kind(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_selected_int_kind(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_selected_real_kind(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_set_exponent(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_shape(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_shifta(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_shiftl(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_shiftr(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_sign(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_sin(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_sinh(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_size(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_spacing(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_spread(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_sqrt(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_storage_size(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_sum(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_system_clock(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_tan(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_tanh(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_this_image(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_tiny(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_trailz(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_transfer(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_transpose(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_trim(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_ubound(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_ucobound(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_unpack(decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void register_generic_intrinsic_verify(decl_context_t decl_context UNUSED_PARAMETER)
{
}

void register_all_fortran_intrinsics(decl_context_t decl_context)
{
#define FORTRAN_GENERIC_INTRINSIC(name, _) \
    register_generic_intrinsic_##name(decl_context);
FORTRAN_INTRINSIC_GENERIC_LIST
#undef FORTRAN_GENERIC_INTRINSIC
}
