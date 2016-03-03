
//
// Generator of src/frontend/cxx-gccbuiltin-sse.h for gcc
//
// Use make generate_builtins_ia32 (at the tp level) to compile this file
//

#include <iostream>
#include <sstream>
#include "builtins-common.hpp"

// --------------------------------------------
// Specific generators for NEON
// --------------------------------------------

#include <stdint.h>

#define GENERATE_NEON_VECTOR(BITS, ELEMENT, TYPENAME) \
template <> \
struct generate_type<TYPENAME> \
{ \
    static const int size = BITS / 8;\
    typedef ELEMENT element_type;\
    static std::string g() \
    {\
        std::stringstream ss;\
\
        ss << "get_vector_type_by_bytes(" << generate_type<element_type>::g() << ", " << (size) << ")";\
\
        return ss.str();\
    }\
};

GENERATE_NEON_VECTOR(64, int8_t,  __Int8x8_t)
GENERATE_NEON_VECTOR(64, int16_t, __Int16x4_t)
GENERATE_NEON_VECTOR(64, int32_t, __Int32x2_t)
GENERATE_NEON_VECTOR(64, int64_t, __Int64x1_t)

GENERATE_NEON_VECTOR(64, uint8_t,  __Uint8x8_t)
GENERATE_NEON_VECTOR(64, uint16_t, __Uint16x4_t)
GENERATE_NEON_VECTOR(64, uint32_t, __Uint32x2_t)
GENERATE_NEON_VECTOR(64, uint64_t, __Uint64x1_t)


GENERATE_NEON_VECTOR(128, int8_t,  __Int8x16_t)
GENERATE_NEON_VECTOR(128, int16_t, __Int16x8_t)
GENERATE_NEON_VECTOR(128, int32_t, __Int32x4_t)
GENERATE_NEON_VECTOR(128, int64_t, __Int64x2_t)

GENERATE_NEON_VECTOR(128, uint8_t,  __Uint8x16_t)
GENERATE_NEON_VECTOR(128, uint16_t, __Uint16x8_t)
GENERATE_NEON_VECTOR(128, uint32_t, __Uint32x4_t)
GENERATE_NEON_VECTOR(128, uint64_t, __Uint64x2_t)

typedef float float32_t;
typedef double float64_t;

GENERATE_NEON_VECTOR(64,  float32_t, __Float32x2_t)
GENERATE_NEON_VECTOR(64,  float64_t, __Float64x1_t)

GENERATE_NEON_VECTOR(128, float32_t, __Float32x4_t)
GENERATE_NEON_VECTOR(128, float64_t, __Float64x2_t)

GENERATE_NEON_VECTOR(64, int16_t, __Poly16x4_t)
GENERATE_NEON_VECTOR(64, int8_t, __Poly8x8_t)

GENERATE_NEON_VECTOR(128, int16_t, __Poly16x8_t)
GENERATE_NEON_VECTOR(128, int8_t, __Poly8x16_t)

template <>
struct generate_type<__builtin_aarch64_simd_ti> // 2 double registers
{
    static const int size = 2;
    typedef long int element_type;

    static std::string g()
    {
        std::stringstream ss;
        ss << "get_vector_type_by_elements(" << generate_type<element_type>::g() << ", " << (size) << ")";\
        return ss.str();
    }
};

template <>
struct generate_type<__builtin_aarch64_simd_ei> // 3 double registers
{
    static const int size = 3;
    typedef long int element_type;

    static std::string g()
    {
        std::stringstream ss;
        ss << "get_vector_type_by_elements(" << generate_type<element_type>::g() << ", " << (size) << ")";\
        return ss.str();
    }
};

template <>
struct generate_type<__builtin_aarch64_simd_oi> // 4 double registers
{
    static const int size = 4;
    typedef long int element_type;

    static std::string g()
    {
        std::stringstream ss;
        ss << "get_vector_type_by_elements(" << generate_type<element_type>::g() << ", " << (size) << ")";\
        return ss.str();
    }
};

template <>
struct generate_type<__builtin_aarch64_simd_ci> // 6 double registers
{
    static const int size = 6;
    typedef long int element_type;

    static std::string g()
    {
        std::stringstream ss;
        ss << "get_vector_type_by_elements(" << generate_type<element_type>::g() << ", " << (size) << ")";\
        return ss.str();
    }
};

template <>
struct generate_type<__builtin_aarch64_simd_xi> // 8 double registers
{
    static const int size = 8;
    typedef long int element_type;

    static std::string g()
    {
        std::stringstream ss;
        ss << "get_vector_type_by_elements(" << generate_type<element_type>::g() << ", " << (size) << ")";\
        return ss.str();
    }
};


// --------------------------------------------
// End of specific generators for NEON
// --------------------------------------------


#define END

#define VECTOR_INTRINSICS_LIST \
VECTOR_INTRIN(__builtin_aarch64_absdi) \
VECTOR_INTRIN(__builtin_aarch64_absv16qi) \
VECTOR_INTRIN(__builtin_aarch64_absv2df) \
VECTOR_INTRIN(__builtin_aarch64_absv2di) \
VECTOR_INTRIN(__builtin_aarch64_absv2sf) \
VECTOR_INTRIN(__builtin_aarch64_absv2si) \
VECTOR_INTRIN(__builtin_aarch64_absv4hi) \
VECTOR_INTRIN(__builtin_aarch64_absv4sf) \
VECTOR_INTRIN(__builtin_aarch64_absv4si) \
VECTOR_INTRIN(__builtin_aarch64_absv8hi) \
VECTOR_INTRIN(__builtin_aarch64_absv8qi) \
VECTOR_INTRIN(__builtin_aarch64_addhn2v2di) \
VECTOR_INTRIN(__builtin_aarch64_addhn2v4si) \
VECTOR_INTRIN(__builtin_aarch64_addhn2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_addhnv2di) \
VECTOR_INTRIN(__builtin_aarch64_addhnv4si) \
VECTOR_INTRIN(__builtin_aarch64_addhnv8hi) \
VECTOR_INTRIN(__builtin_aarch64_addpdi) \
VECTOR_INTRIN(__builtin_aarch64_addpv2si) \
VECTOR_INTRIN(__builtin_aarch64_addpv4hi) \
VECTOR_INTRIN(__builtin_aarch64_addpv8qi) \
VECTOR_INTRIN(__builtin_aarch64_ashldi) \
VECTOR_INTRIN(__builtin_aarch64_ashlv16qi) \
VECTOR_INTRIN(__builtin_aarch64_ashlv2di) \
VECTOR_INTRIN(__builtin_aarch64_ashlv2si) \
VECTOR_INTRIN(__builtin_aarch64_ashlv4hi) \
VECTOR_INTRIN(__builtin_aarch64_ashlv4si) \
VECTOR_INTRIN(__builtin_aarch64_ashlv8hi) \
VECTOR_INTRIN(__builtin_aarch64_ashlv8qi) \
VECTOR_INTRIN(__builtin_aarch64_ashr_simddi) \
VECTOR_INTRIN(__builtin_aarch64_ashrv16qi) \
VECTOR_INTRIN(__builtin_aarch64_ashrv2di) \
VECTOR_INTRIN(__builtin_aarch64_ashrv2si) \
VECTOR_INTRIN(__builtin_aarch64_ashrv4hi) \
VECTOR_INTRIN(__builtin_aarch64_ashrv4si) \
VECTOR_INTRIN(__builtin_aarch64_ashrv8hi) \
VECTOR_INTRIN(__builtin_aarch64_ashrv8qi) \
VECTOR_INTRIN(__builtin_aarch64_btruncv2df) \
VECTOR_INTRIN(__builtin_aarch64_btruncv2sf) \
VECTOR_INTRIN(__builtin_aarch64_btruncv4sf) \
VECTOR_INTRIN(__builtin_aarch64_ceilv2df) \
VECTOR_INTRIN(__builtin_aarch64_ceilv2sf) \
VECTOR_INTRIN(__builtin_aarch64_ceilv4sf) \
VECTOR_INTRIN(__builtin_aarch64_clrsbv16qi) \
VECTOR_INTRIN(__builtin_aarch64_clrsbv2si) \
VECTOR_INTRIN(__builtin_aarch64_clrsbv4hi) \
VECTOR_INTRIN(__builtin_aarch64_clrsbv4si) \
VECTOR_INTRIN(__builtin_aarch64_clrsbv8hi) \
VECTOR_INTRIN(__builtin_aarch64_clrsbv8qi) \
VECTOR_INTRIN(__builtin_aarch64_clzv16qi) \
VECTOR_INTRIN(__builtin_aarch64_clzv2si) \
VECTOR_INTRIN(__builtin_aarch64_clzv4hi) \
VECTOR_INTRIN(__builtin_aarch64_clzv4si) \
VECTOR_INTRIN(__builtin_aarch64_clzv8hi) \
VECTOR_INTRIN(__builtin_aarch64_clzv8qi) \
VECTOR_INTRIN(__builtin_aarch64_combinedf) \
VECTOR_INTRIN(__builtin_aarch64_combinedi) \
VECTOR_INTRIN(__builtin_aarch64_combinev2sf) \
VECTOR_INTRIN(__builtin_aarch64_combinev2si) \
VECTOR_INTRIN(__builtin_aarch64_combinev4hi) \
VECTOR_INTRIN(__builtin_aarch64_combinev8qi) \
VECTOR_INTRIN(__builtin_aarch64_float_extend_lo_v2df) \
VECTOR_INTRIN(__builtin_aarch64_float_truncate_hi_v4sf) \
VECTOR_INTRIN(__builtin_aarch64_float_truncate_lo_v2sf) \
VECTOR_INTRIN(__builtin_aarch64_floatunsv2div2df) \
VECTOR_INTRIN(__builtin_aarch64_floatunsv2siv2sf) \
VECTOR_INTRIN(__builtin_aarch64_floatunsv4siv4sf) \
VECTOR_INTRIN(__builtin_aarch64_floatv2div2df) \
VECTOR_INTRIN(__builtin_aarch64_floatv2siv2sf) \
VECTOR_INTRIN(__builtin_aarch64_floatv4siv4sf) \
VECTOR_INTRIN(__builtin_aarch64_floorv2df) \
VECTOR_INTRIN(__builtin_aarch64_floorv2sf) \
VECTOR_INTRIN(__builtin_aarch64_floorv4sf) \
VECTOR_INTRIN(__builtin_aarch64_fmav2df) \
VECTOR_INTRIN(__builtin_aarch64_fmav2sf) \
VECTOR_INTRIN(__builtin_aarch64_fmav4sf) \
VECTOR_INTRIN(__builtin_aarch64_frecpedf) \
VECTOR_INTRIN(__builtin_aarch64_frecpesf) \
VECTOR_INTRIN(__builtin_aarch64_frecpev2df) \
VECTOR_INTRIN(__builtin_aarch64_frecpev2sf) \
VECTOR_INTRIN(__builtin_aarch64_frecpev4sf) \
VECTOR_INTRIN(__builtin_aarch64_frecpsdf) \
VECTOR_INTRIN(__builtin_aarch64_frecpssf) \
VECTOR_INTRIN(__builtin_aarch64_frecpsv2df) \
VECTOR_INTRIN(__builtin_aarch64_frecpsv2sf) \
VECTOR_INTRIN(__builtin_aarch64_frecpsv4sf) \
VECTOR_INTRIN(__builtin_aarch64_frecpxdf) \
VECTOR_INTRIN(__builtin_aarch64_frecpxsf) \
VECTOR_INTRIN(__builtin_aarch64_frintndf) \
VECTOR_INTRIN(__builtin_aarch64_frintnv2df) \
VECTOR_INTRIN(__builtin_aarch64_frintnv2sf) \
VECTOR_INTRIN(__builtin_aarch64_frintnv4sf) \
VECTOR_INTRIN(__builtin_aarch64_get_dregcidf) \
VECTOR_INTRIN(__builtin_aarch64_get_dregcidi) \
VECTOR_INTRIN(__builtin_aarch64_get_dregciv2sf) \
VECTOR_INTRIN(__builtin_aarch64_get_dregciv2si) \
VECTOR_INTRIN(__builtin_aarch64_get_dregciv4hi) \
VECTOR_INTRIN(__builtin_aarch64_get_dregciv8qi) \
VECTOR_INTRIN(__builtin_aarch64_get_dregoidf) \
VECTOR_INTRIN(__builtin_aarch64_get_dregoidi) \
VECTOR_INTRIN(__builtin_aarch64_get_dregoiv2sf) \
VECTOR_INTRIN(__builtin_aarch64_get_dregoiv2si) \
VECTOR_INTRIN(__builtin_aarch64_get_dregoiv4hi) \
VECTOR_INTRIN(__builtin_aarch64_get_dregoiv8qi) \
VECTOR_INTRIN(__builtin_aarch64_get_dregxidf) \
VECTOR_INTRIN(__builtin_aarch64_get_dregxidi) \
VECTOR_INTRIN(__builtin_aarch64_get_dregxiv2sf) \
VECTOR_INTRIN(__builtin_aarch64_get_dregxiv2si) \
VECTOR_INTRIN(__builtin_aarch64_get_dregxiv4hi) \
VECTOR_INTRIN(__builtin_aarch64_get_dregxiv8qi) \
VECTOR_INTRIN(__builtin_aarch64_get_qregciv16qi) \
VECTOR_INTRIN(__builtin_aarch64_get_qregciv2df) \
VECTOR_INTRIN(__builtin_aarch64_get_qregciv2di) \
VECTOR_INTRIN(__builtin_aarch64_get_qregciv4sf) \
VECTOR_INTRIN(__builtin_aarch64_get_qregciv4si) \
VECTOR_INTRIN(__builtin_aarch64_get_qregciv8hi) \
VECTOR_INTRIN(__builtin_aarch64_get_qregoiv16qi) \
VECTOR_INTRIN(__builtin_aarch64_get_qregoiv2df) \
VECTOR_INTRIN(__builtin_aarch64_get_qregoiv2di) \
VECTOR_INTRIN(__builtin_aarch64_get_qregoiv4sf) \
VECTOR_INTRIN(__builtin_aarch64_get_qregoiv4si) \
VECTOR_INTRIN(__builtin_aarch64_get_qregoiv8hi) \
VECTOR_INTRIN(__builtin_aarch64_get_qregxiv16qi) \
VECTOR_INTRIN(__builtin_aarch64_get_qregxiv2df) \
VECTOR_INTRIN(__builtin_aarch64_get_qregxiv2di) \
VECTOR_INTRIN(__builtin_aarch64_get_qregxiv4sf) \
VECTOR_INTRIN(__builtin_aarch64_get_qregxiv4si) \
VECTOR_INTRIN(__builtin_aarch64_get_qregxiv8hi) \
VECTOR_INTRIN(__builtin_aarch64_im_lane_boundsi) \
VECTOR_INTRIN(__builtin_aarch64_lbtruncuv2dfv2di) \
VECTOR_INTRIN(__builtin_aarch64_lbtruncuv2sfv2si) \
VECTOR_INTRIN(__builtin_aarch64_lbtruncuv4sfv4si) \
VECTOR_INTRIN(__builtin_aarch64_lbtruncv2dfv2di) \
VECTOR_INTRIN(__builtin_aarch64_lbtruncv2sfv2si) \
VECTOR_INTRIN(__builtin_aarch64_lbtruncv4sfv4si) \
VECTOR_INTRIN(__builtin_aarch64_lceiludfdi) \
VECTOR_INTRIN(__builtin_aarch64_lceilusfsi) \
VECTOR_INTRIN(__builtin_aarch64_lceiluv2dfv2di) \
VECTOR_INTRIN(__builtin_aarch64_lceiluv2sfv2si) \
VECTOR_INTRIN(__builtin_aarch64_lceiluv4sfv4si) \
VECTOR_INTRIN(__builtin_aarch64_lceilv2dfv2di) \
VECTOR_INTRIN(__builtin_aarch64_lceilv2sfv2si) \
VECTOR_INTRIN(__builtin_aarch64_lceilv4sfv4si) \
VECTOR_INTRIN(__builtin_aarch64_ld1v16qi) \
VECTOR_INTRIN(__builtin_aarch64_ld1v2df) \
VECTOR_INTRIN(__builtin_aarch64_ld1v2di) \
VECTOR_INTRIN(__builtin_aarch64_ld1v2sf) \
VECTOR_INTRIN(__builtin_aarch64_ld1v2si) \
VECTOR_INTRIN(__builtin_aarch64_ld1v4hi) \
VECTOR_INTRIN(__builtin_aarch64_ld1v4sf) \
VECTOR_INTRIN(__builtin_aarch64_ld1v4si) \
VECTOR_INTRIN(__builtin_aarch64_ld1v8hi) \
VECTOR_INTRIN(__builtin_aarch64_ld1v8qi) \
VECTOR_INTRIN(__builtin_aarch64_ld2df) \
VECTOR_INTRIN(__builtin_aarch64_ld2di) \
VECTOR_INTRIN(__builtin_aarch64_ld2_lanev16qi) \
VECTOR_INTRIN(__builtin_aarch64_ld2_lanev2df) \
VECTOR_INTRIN(__builtin_aarch64_ld2_lanev2di) \
VECTOR_INTRIN(__builtin_aarch64_ld2_lanev4sf) \
VECTOR_INTRIN(__builtin_aarch64_ld2_lanev4si) \
VECTOR_INTRIN(__builtin_aarch64_ld2_lanev8hi) \
VECTOR_INTRIN(__builtin_aarch64_ld2rdf) \
VECTOR_INTRIN(__builtin_aarch64_ld2rdi) \
VECTOR_INTRIN(__builtin_aarch64_ld2rv16qi) \
VECTOR_INTRIN(__builtin_aarch64_ld2rv2df) \
VECTOR_INTRIN(__builtin_aarch64_ld2rv2di) \
VECTOR_INTRIN(__builtin_aarch64_ld2rv2sf) \
VECTOR_INTRIN(__builtin_aarch64_ld2rv2si) \
VECTOR_INTRIN(__builtin_aarch64_ld2rv4hi) \
VECTOR_INTRIN(__builtin_aarch64_ld2rv4sf) \
VECTOR_INTRIN(__builtin_aarch64_ld2rv4si) \
VECTOR_INTRIN(__builtin_aarch64_ld2rv8hi) \
VECTOR_INTRIN(__builtin_aarch64_ld2rv8qi) \
VECTOR_INTRIN(__builtin_aarch64_ld2v16qi) \
VECTOR_INTRIN(__builtin_aarch64_ld2v2df) \
VECTOR_INTRIN(__builtin_aarch64_ld2v2di) \
VECTOR_INTRIN(__builtin_aarch64_ld2v2sf) \
VECTOR_INTRIN(__builtin_aarch64_ld2v2si) \
VECTOR_INTRIN(__builtin_aarch64_ld2v4hi) \
VECTOR_INTRIN(__builtin_aarch64_ld2v4sf) \
VECTOR_INTRIN(__builtin_aarch64_ld2v4si) \
VECTOR_INTRIN(__builtin_aarch64_ld2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_ld2v8qi) \
VECTOR_INTRIN(__builtin_aarch64_ld3df) \
VECTOR_INTRIN(__builtin_aarch64_ld3di) \
VECTOR_INTRIN(__builtin_aarch64_ld3_lanev16qi) \
VECTOR_INTRIN(__builtin_aarch64_ld3_lanev2df) \
VECTOR_INTRIN(__builtin_aarch64_ld3_lanev2di) \
VECTOR_INTRIN(__builtin_aarch64_ld3_lanev4sf) \
VECTOR_INTRIN(__builtin_aarch64_ld3_lanev4si) \
VECTOR_INTRIN(__builtin_aarch64_ld3_lanev8hi) \
VECTOR_INTRIN(__builtin_aarch64_ld3rdf) \
VECTOR_INTRIN(__builtin_aarch64_ld3rdi) \
VECTOR_INTRIN(__builtin_aarch64_ld3rv16qi) \
VECTOR_INTRIN(__builtin_aarch64_ld3rv2df) \
VECTOR_INTRIN(__builtin_aarch64_ld3rv2di) \
VECTOR_INTRIN(__builtin_aarch64_ld3rv2sf) \
VECTOR_INTRIN(__builtin_aarch64_ld3rv2si) \
VECTOR_INTRIN(__builtin_aarch64_ld3rv4hi) \
VECTOR_INTRIN(__builtin_aarch64_ld3rv4sf) \
VECTOR_INTRIN(__builtin_aarch64_ld3rv4si) \
VECTOR_INTRIN(__builtin_aarch64_ld3rv8hi) \
VECTOR_INTRIN(__builtin_aarch64_ld3rv8qi) \
VECTOR_INTRIN(__builtin_aarch64_ld3v16qi) \
VECTOR_INTRIN(__builtin_aarch64_ld3v2df) \
VECTOR_INTRIN(__builtin_aarch64_ld3v2di) \
VECTOR_INTRIN(__builtin_aarch64_ld3v2sf) \
VECTOR_INTRIN(__builtin_aarch64_ld3v2si) \
VECTOR_INTRIN(__builtin_aarch64_ld3v4hi) \
VECTOR_INTRIN(__builtin_aarch64_ld3v4sf) \
VECTOR_INTRIN(__builtin_aarch64_ld3v4si) \
VECTOR_INTRIN(__builtin_aarch64_ld3v8hi) \
VECTOR_INTRIN(__builtin_aarch64_ld3v8qi) \
VECTOR_INTRIN(__builtin_aarch64_ld4df) \
VECTOR_INTRIN(__builtin_aarch64_ld4di) \
VECTOR_INTRIN(__builtin_aarch64_ld4_lanev16qi) \
VECTOR_INTRIN(__builtin_aarch64_ld4_lanev2df) \
VECTOR_INTRIN(__builtin_aarch64_ld4_lanev2di) \
VECTOR_INTRIN(__builtin_aarch64_ld4_lanev4sf) \
VECTOR_INTRIN(__builtin_aarch64_ld4_lanev4si) \
VECTOR_INTRIN(__builtin_aarch64_ld4_lanev8hi) \
VECTOR_INTRIN(__builtin_aarch64_ld4rdf) \
VECTOR_INTRIN(__builtin_aarch64_ld4rdi) \
VECTOR_INTRIN(__builtin_aarch64_ld4rv16qi) \
VECTOR_INTRIN(__builtin_aarch64_ld4rv2df) \
VECTOR_INTRIN(__builtin_aarch64_ld4rv2di) \
VECTOR_INTRIN(__builtin_aarch64_ld4rv2sf) \
VECTOR_INTRIN(__builtin_aarch64_ld4rv2si) \
VECTOR_INTRIN(__builtin_aarch64_ld4rv4hi) \
VECTOR_INTRIN(__builtin_aarch64_ld4rv4sf) \
VECTOR_INTRIN(__builtin_aarch64_ld4rv4si) \
VECTOR_INTRIN(__builtin_aarch64_ld4rv8hi) \
VECTOR_INTRIN(__builtin_aarch64_ld4rv8qi) \
VECTOR_INTRIN(__builtin_aarch64_ld4v16qi) \
VECTOR_INTRIN(__builtin_aarch64_ld4v2df) \
VECTOR_INTRIN(__builtin_aarch64_ld4v2di) \
VECTOR_INTRIN(__builtin_aarch64_ld4v2sf) \
VECTOR_INTRIN(__builtin_aarch64_ld4v2si) \
VECTOR_INTRIN(__builtin_aarch64_ld4v4hi) \
VECTOR_INTRIN(__builtin_aarch64_ld4v4sf) \
VECTOR_INTRIN(__builtin_aarch64_ld4v4si) \
VECTOR_INTRIN(__builtin_aarch64_ld4v8hi) \
VECTOR_INTRIN(__builtin_aarch64_ld4v8qi) \
VECTOR_INTRIN(__builtin_aarch64_lfloorudfdi) \
VECTOR_INTRIN(__builtin_aarch64_lfloorusfsi) \
VECTOR_INTRIN(__builtin_aarch64_lflooruv2dfv2di) \
VECTOR_INTRIN(__builtin_aarch64_lflooruv2sfv2si) \
VECTOR_INTRIN(__builtin_aarch64_lflooruv4sfv4si) \
VECTOR_INTRIN(__builtin_aarch64_lfloorv2dfv2di) \
VECTOR_INTRIN(__builtin_aarch64_lfloorv2sfv2si) \
VECTOR_INTRIN(__builtin_aarch64_lfloorv4sfv4si) \
VECTOR_INTRIN(__builtin_aarch64_lfrintndfdi) \
VECTOR_INTRIN(__builtin_aarch64_lfrintnsfsi) \
VECTOR_INTRIN(__builtin_aarch64_lfrintnudfdi) \
VECTOR_INTRIN(__builtin_aarch64_lfrintnusfsi) \
VECTOR_INTRIN(__builtin_aarch64_lfrintnuv2dfv2di) \
VECTOR_INTRIN(__builtin_aarch64_lfrintnuv2sfv2si) \
VECTOR_INTRIN(__builtin_aarch64_lfrintnuv4sfv4si) \
VECTOR_INTRIN(__builtin_aarch64_lfrintnv2dfv2di) \
VECTOR_INTRIN(__builtin_aarch64_lfrintnv2sfv2si) \
VECTOR_INTRIN(__builtin_aarch64_lfrintnv4sfv4si) \
VECTOR_INTRIN(__builtin_aarch64_lrounddfdi) \
VECTOR_INTRIN(__builtin_aarch64_lroundsfsi) \
VECTOR_INTRIN(__builtin_aarch64_lroundudfdi) \
VECTOR_INTRIN(__builtin_aarch64_lroundusfsi) \
VECTOR_INTRIN(__builtin_aarch64_lrounduv2dfv2di) \
VECTOR_INTRIN(__builtin_aarch64_lrounduv2sfv2si) \
VECTOR_INTRIN(__builtin_aarch64_lrounduv4sfv4si) \
VECTOR_INTRIN(__builtin_aarch64_lroundv2dfv2di) \
VECTOR_INTRIN(__builtin_aarch64_lroundv2sfv2si) \
VECTOR_INTRIN(__builtin_aarch64_lroundv4sfv4si) \
VECTOR_INTRIN(__builtin_aarch64_lshr_simddi_uus) \
VECTOR_INTRIN(__builtin_aarch64_lshrv16qi) \
VECTOR_INTRIN(__builtin_aarch64_lshrv2di) \
VECTOR_INTRIN(__builtin_aarch64_lshrv2si) \
VECTOR_INTRIN(__builtin_aarch64_lshrv4hi) \
VECTOR_INTRIN(__builtin_aarch64_lshrv4si) \
VECTOR_INTRIN(__builtin_aarch64_lshrv8hi) \
VECTOR_INTRIN(__builtin_aarch64_lshrv8qi) \
VECTOR_INTRIN(__builtin_aarch64_nearbyintv2df) \
VECTOR_INTRIN(__builtin_aarch64_nearbyintv2sf) \
VECTOR_INTRIN(__builtin_aarch64_nearbyintv4sf) \
VECTOR_INTRIN(__builtin_aarch64_pmulv16qi) \
VECTOR_INTRIN(__builtin_aarch64_pmulv8qi) \
VECTOR_INTRIN(__builtin_aarch64_popcountv16qi) \
VECTOR_INTRIN(__builtin_aarch64_popcountv8qi) \
VECTOR_INTRIN(__builtin_aarch64_raddhn2v2di) \
VECTOR_INTRIN(__builtin_aarch64_raddhn2v4si) \
VECTOR_INTRIN(__builtin_aarch64_raddhn2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_raddhnv2di) \
VECTOR_INTRIN(__builtin_aarch64_raddhnv4si) \
VECTOR_INTRIN(__builtin_aarch64_raddhnv8hi) \
VECTOR_INTRIN(__builtin_aarch64_rbitv16qi) \
VECTOR_INTRIN(__builtin_aarch64_rbitv8qi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_plus_scal_v16qi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_plus_scal_v2df) \
VECTOR_INTRIN(__builtin_aarch64_reduc_plus_scal_v2di) \
VECTOR_INTRIN(__builtin_aarch64_reduc_plus_scal_v2sf) \
VECTOR_INTRIN(__builtin_aarch64_reduc_plus_scal_v2si) \
VECTOR_INTRIN(__builtin_aarch64_reduc_plus_scal_v4hi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_plus_scal_v4sf) \
VECTOR_INTRIN(__builtin_aarch64_reduc_plus_scal_v4si) \
VECTOR_INTRIN(__builtin_aarch64_reduc_plus_scal_v8hi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_plus_scal_v8qi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smax_nan_scal_v2df) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smax_nan_scal_v2sf) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smax_nan_scal_v4sf) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smax_scal_v16qi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smax_scal_v2df) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smax_scal_v2sf) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smax_scal_v2si) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smax_scal_v4hi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smax_scal_v4sf) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smax_scal_v4si) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smax_scal_v8hi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smax_scal_v8qi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smin_nan_scal_v2df) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smin_nan_scal_v2sf) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smin_nan_scal_v4sf) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smin_scal_v16qi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smin_scal_v2df) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smin_scal_v2sf) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smin_scal_v2si) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smin_scal_v4hi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smin_scal_v4sf) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smin_scal_v4si) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smin_scal_v8hi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_smin_scal_v8qi) \
VECTOR_INTRIN(__builtin_aarch64_reduc_umax_scal_v16qi_uu) \
VECTOR_INTRIN(__builtin_aarch64_reduc_umax_scal_v2si_uu) \
VECTOR_INTRIN(__builtin_aarch64_reduc_umax_scal_v4hi_uu) \
VECTOR_INTRIN(__builtin_aarch64_reduc_umax_scal_v4si_uu) \
VECTOR_INTRIN(__builtin_aarch64_reduc_umax_scal_v8hi_uu) \
VECTOR_INTRIN(__builtin_aarch64_reduc_umax_scal_v8qi_uu) \
VECTOR_INTRIN(__builtin_aarch64_reduc_umin_scal_v16qi_uu) \
VECTOR_INTRIN(__builtin_aarch64_reduc_umin_scal_v2si_uu) \
VECTOR_INTRIN(__builtin_aarch64_reduc_umin_scal_v4hi_uu) \
VECTOR_INTRIN(__builtin_aarch64_reduc_umin_scal_v4si_uu) \
VECTOR_INTRIN(__builtin_aarch64_reduc_umin_scal_v8hi_uu) \
VECTOR_INTRIN(__builtin_aarch64_reduc_umin_scal_v8qi_uu) \
VECTOR_INTRIN(__builtin_aarch64_rintv2df) \
VECTOR_INTRIN(__builtin_aarch64_rintv2sf) \
VECTOR_INTRIN(__builtin_aarch64_rintv4sf) \
VECTOR_INTRIN(__builtin_aarch64_roundv2df) \
VECTOR_INTRIN(__builtin_aarch64_roundv2sf) \
VECTOR_INTRIN(__builtin_aarch64_roundv4sf) \
VECTOR_INTRIN(__builtin_aarch64_rsubhn2v2di) \
VECTOR_INTRIN(__builtin_aarch64_rsubhn2v4si) \
VECTOR_INTRIN(__builtin_aarch64_rsubhn2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_rsubhnv2di) \
VECTOR_INTRIN(__builtin_aarch64_rsubhnv4si) \
VECTOR_INTRIN(__builtin_aarch64_rsubhnv8hi) \
VECTOR_INTRIN(__builtin_aarch64_saddl2v16qi) \
VECTOR_INTRIN(__builtin_aarch64_saddl2v4si) \
VECTOR_INTRIN(__builtin_aarch64_saddl2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_saddlv2si) \
VECTOR_INTRIN(__builtin_aarch64_saddlv4hi) \
VECTOR_INTRIN(__builtin_aarch64_saddlv8qi) \
VECTOR_INTRIN(__builtin_aarch64_saddw2v16qi) \
VECTOR_INTRIN(__builtin_aarch64_saddw2v4si) \
VECTOR_INTRIN(__builtin_aarch64_saddw2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_saddwv2si) \
VECTOR_INTRIN(__builtin_aarch64_saddwv4hi) \
VECTOR_INTRIN(__builtin_aarch64_saddwv8qi) \
VECTOR_INTRIN(__builtin_aarch64_set_qregciv16qi) \
VECTOR_INTRIN(__builtin_aarch64_set_qregciv2df) \
VECTOR_INTRIN(__builtin_aarch64_set_qregciv2di) \
VECTOR_INTRIN(__builtin_aarch64_set_qregciv4sf) \
VECTOR_INTRIN(__builtin_aarch64_set_qregciv4si) \
VECTOR_INTRIN(__builtin_aarch64_set_qregciv8hi) \
VECTOR_INTRIN(__builtin_aarch64_set_qregoiv16qi) \
VECTOR_INTRIN(__builtin_aarch64_set_qregoiv2df) \
VECTOR_INTRIN(__builtin_aarch64_set_qregoiv2di) \
VECTOR_INTRIN(__builtin_aarch64_set_qregoiv4sf) \
VECTOR_INTRIN(__builtin_aarch64_set_qregoiv4si) \
VECTOR_INTRIN(__builtin_aarch64_set_qregoiv8hi) \
VECTOR_INTRIN(__builtin_aarch64_set_qregxiv16qi) \
VECTOR_INTRIN(__builtin_aarch64_set_qregxiv2df) \
VECTOR_INTRIN(__builtin_aarch64_set_qregxiv2di) \
VECTOR_INTRIN(__builtin_aarch64_set_qregxiv4sf) \
VECTOR_INTRIN(__builtin_aarch64_set_qregxiv4si) \
VECTOR_INTRIN(__builtin_aarch64_set_qregxiv8hi) \
VECTOR_INTRIN(__builtin_aarch64_shaddv16qi) \
VECTOR_INTRIN(__builtin_aarch64_shaddv2si) \
VECTOR_INTRIN(__builtin_aarch64_shaddv4hi) \
VECTOR_INTRIN(__builtin_aarch64_shaddv4si) \
VECTOR_INTRIN(__builtin_aarch64_shaddv8hi) \
VECTOR_INTRIN(__builtin_aarch64_shaddv8qi) \
VECTOR_INTRIN(__builtin_aarch64_shsubv16qi) \
VECTOR_INTRIN(__builtin_aarch64_shsubv2si) \
VECTOR_INTRIN(__builtin_aarch64_shsubv4hi) \
VECTOR_INTRIN(__builtin_aarch64_shsubv4si) \
VECTOR_INTRIN(__builtin_aarch64_shsubv8hi) \
VECTOR_INTRIN(__builtin_aarch64_shsubv8qi) \
VECTOR_INTRIN(__builtin_aarch64_simd_bsldf_suss) \
VECTOR_INTRIN(__builtin_aarch64_simd_bsldi_suss) \
VECTOR_INTRIN(__builtin_aarch64_simd_bsldi_uuuu) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv16qi_pupp) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv16qi_suss) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv16qi_uuuu) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv2df_suss) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv2di_suss) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv2di_uuuu) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv2sf_suss) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv2si_suss) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv2si_uuuu) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv4hi_pupp) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv4hi_suss) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv4hi_uuuu) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv4sf_suss) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv4si_suss) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv4si_uuuu) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv8hi_pupp) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv8hi_suss) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv8hi_uuuu) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv8qi_pupp) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv8qi_suss) \
VECTOR_INTRIN(__builtin_aarch64_simd_bslv8qi_uuuu) \
VECTOR_INTRIN(__builtin_aarch64_smax_nanpv2df) \
VECTOR_INTRIN(__builtin_aarch64_smax_nanpv2sf) \
VECTOR_INTRIN(__builtin_aarch64_smax_nanpv4sf) \
VECTOR_INTRIN(__builtin_aarch64_smax_nanv2df) \
VECTOR_INTRIN(__builtin_aarch64_smax_nanv2sf) \
VECTOR_INTRIN(__builtin_aarch64_smax_nanv4sf) \
VECTOR_INTRIN(__builtin_aarch64_smaxpv16qi) \
VECTOR_INTRIN(__builtin_aarch64_smaxpv2df) \
VECTOR_INTRIN(__builtin_aarch64_smaxpv2sf) \
VECTOR_INTRIN(__builtin_aarch64_smaxpv2si) \
VECTOR_INTRIN(__builtin_aarch64_smaxpv4hi) \
VECTOR_INTRIN(__builtin_aarch64_smaxpv4sf) \
VECTOR_INTRIN(__builtin_aarch64_smaxpv4si) \
VECTOR_INTRIN(__builtin_aarch64_smaxpv8hi) \
VECTOR_INTRIN(__builtin_aarch64_smaxpv8qi) \
VECTOR_INTRIN(__builtin_aarch64_smaxv16qi) \
VECTOR_INTRIN(__builtin_aarch64_smaxv2df) \
VECTOR_INTRIN(__builtin_aarch64_smaxv2sf) \
VECTOR_INTRIN(__builtin_aarch64_smaxv2si) \
VECTOR_INTRIN(__builtin_aarch64_smaxv4hi) \
VECTOR_INTRIN(__builtin_aarch64_smaxv4sf) \
VECTOR_INTRIN(__builtin_aarch64_smaxv4si) \
VECTOR_INTRIN(__builtin_aarch64_smaxv8hi) \
VECTOR_INTRIN(__builtin_aarch64_smaxv8qi) \
VECTOR_INTRIN(__builtin_aarch64_smin_nanpv2df) \
VECTOR_INTRIN(__builtin_aarch64_smin_nanpv2sf) \
VECTOR_INTRIN(__builtin_aarch64_smin_nanpv4sf) \
VECTOR_INTRIN(__builtin_aarch64_smin_nanv2df) \
VECTOR_INTRIN(__builtin_aarch64_smin_nanv2sf) \
VECTOR_INTRIN(__builtin_aarch64_smin_nanv4sf) \
VECTOR_INTRIN(__builtin_aarch64_sminpv16qi) \
VECTOR_INTRIN(__builtin_aarch64_sminpv2df) \
VECTOR_INTRIN(__builtin_aarch64_sminpv2sf) \
VECTOR_INTRIN(__builtin_aarch64_sminpv2si) \
VECTOR_INTRIN(__builtin_aarch64_sminpv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sminpv4sf) \
VECTOR_INTRIN(__builtin_aarch64_sminpv4si) \
VECTOR_INTRIN(__builtin_aarch64_sminpv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sminpv8qi) \
VECTOR_INTRIN(__builtin_aarch64_sminv16qi) \
VECTOR_INTRIN(__builtin_aarch64_sminv2df) \
VECTOR_INTRIN(__builtin_aarch64_sminv2sf) \
VECTOR_INTRIN(__builtin_aarch64_sminv2si) \
VECTOR_INTRIN(__builtin_aarch64_sminv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sminv4sf) \
VECTOR_INTRIN(__builtin_aarch64_sminv4si) \
VECTOR_INTRIN(__builtin_aarch64_sminv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sminv8qi) \
VECTOR_INTRIN(__builtin_aarch64_sqabsdi) \
VECTOR_INTRIN(__builtin_aarch64_sqabshi) \
VECTOR_INTRIN(__builtin_aarch64_sqabsqi) \
VECTOR_INTRIN(__builtin_aarch64_sqabssi) \
VECTOR_INTRIN(__builtin_aarch64_sqabsv16qi) \
VECTOR_INTRIN(__builtin_aarch64_sqabsv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqabsv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqabsv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqabsv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqabsv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqabsv8qi) \
VECTOR_INTRIN(__builtin_aarch64_sqadddi) \
VECTOR_INTRIN(__builtin_aarch64_sqaddhi) \
VECTOR_INTRIN(__builtin_aarch64_sqaddqi) \
VECTOR_INTRIN(__builtin_aarch64_sqaddsi) \
VECTOR_INTRIN(__builtin_aarch64_sqaddv16qi) \
VECTOR_INTRIN(__builtin_aarch64_sqaddv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqaddv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqaddv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqaddv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqaddv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqaddv8qi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal2_laneqv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal2_laneqv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal2_lanev4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal2_lanev8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal2_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal2_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal2v4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlalhi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal_lanehi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal_laneqhi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal_laneqsi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal_laneqv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal_laneqv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal_lanesi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal_lanev2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal_lanev4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal_nv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlal_nv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlalsi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlalv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlalv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl2_laneqv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl2_laneqv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl2_lanev4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl2_lanev8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl2_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl2_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl2v4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlslhi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl_lanehi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl_laneqhi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl_laneqsi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl_laneqv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl_laneqv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl_lanesi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl_lanev2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl_lanev4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl_nv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlsl_nv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlslsi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlslv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmlslv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulhhi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulh_lanehi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulh_laneqhi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulh_laneqsi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulh_laneqv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulh_laneqv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulh_laneqv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulh_laneqv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulh_lanesi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulh_lanev2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulh_lanev4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulh_lanev4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulh_lanev8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulhsi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulhv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulhv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulhv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmulhv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull2_laneqv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull2_laneqv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull2_lanev4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull2_lanev8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull2_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull2_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull2v4si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmullhi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull_lanehi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull_laneqhi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull_laneqsi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull_laneqv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull_laneqv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull_lanesi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull_lanev2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull_lanev4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull_nv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmull_nv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmullsi) \
VECTOR_INTRIN(__builtin_aarch64_sqdmullv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqdmullv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqmovndi) \
VECTOR_INTRIN(__builtin_aarch64_sqmovnhi) \
VECTOR_INTRIN(__builtin_aarch64_sqmovnsi) \
VECTOR_INTRIN(__builtin_aarch64_sqmovnv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqmovnv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqmovnv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqmovundi) \
VECTOR_INTRIN(__builtin_aarch64_sqmovunhi) \
VECTOR_INTRIN(__builtin_aarch64_sqmovunsi) \
VECTOR_INTRIN(__builtin_aarch64_sqmovunv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqmovunv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqmovunv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqnegdi) \
VECTOR_INTRIN(__builtin_aarch64_sqneghi) \
VECTOR_INTRIN(__builtin_aarch64_sqnegqi) \
VECTOR_INTRIN(__builtin_aarch64_sqnegsi) \
VECTOR_INTRIN(__builtin_aarch64_sqnegv16qi) \
VECTOR_INTRIN(__builtin_aarch64_sqnegv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqnegv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqnegv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqnegv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqnegv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqnegv8qi) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulhhi) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulh_lanehi) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulh_laneqhi) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulh_laneqsi) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulh_laneqv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulh_laneqv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulh_laneqv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulh_laneqv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulh_lanesi) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulh_lanev2si) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulh_lanev4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulh_lanev4si) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulh_lanev8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulhsi) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulhv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulhv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulhv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqrdmulhv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshldi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshlhi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshlqi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshlsi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshlv16qi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshlv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqrshlv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqrshlv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshlv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqrshlv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshlv8qi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshrn_ndi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshrn_nhi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshrn_nsi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshrn_nv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqrshrn_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqrshrn_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshrun_ndi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshrun_nhi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshrun_nsi) \
VECTOR_INTRIN(__builtin_aarch64_sqrshrun_nv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqrshrun_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqrshrun_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqrtdf) \
VECTOR_INTRIN(__builtin_aarch64_sqrtv2df) \
VECTOR_INTRIN(__builtin_aarch64_sqrtv2sf) \
VECTOR_INTRIN(__builtin_aarch64_sqrtv4sf) \
VECTOR_INTRIN(__builtin_aarch64_sqshldi) \
VECTOR_INTRIN(__builtin_aarch64_sqshlhi) \
VECTOR_INTRIN(__builtin_aarch64_sqshl_ndi) \
VECTOR_INTRIN(__builtin_aarch64_sqshl_nhi) \
VECTOR_INTRIN(__builtin_aarch64_sqshl_nqi) \
VECTOR_INTRIN(__builtin_aarch64_sqshl_nsi) \
VECTOR_INTRIN(__builtin_aarch64_sqshl_nv16qi) \
VECTOR_INTRIN(__builtin_aarch64_sqshl_nv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqshl_nv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqshl_nv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqshl_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqshl_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqshl_nv8qi) \
VECTOR_INTRIN(__builtin_aarch64_sqshlqi) \
VECTOR_INTRIN(__builtin_aarch64_sqshlsi) \
VECTOR_INTRIN(__builtin_aarch64_sqshlu_ndi_uss) \
VECTOR_INTRIN(__builtin_aarch64_sqshlu_nhi_uss) \
VECTOR_INTRIN(__builtin_aarch64_sqshlu_nqi_uss) \
VECTOR_INTRIN(__builtin_aarch64_sqshlu_nsi_uss) \
VECTOR_INTRIN(__builtin_aarch64_sqshlu_nv16qi_uss) \
VECTOR_INTRIN(__builtin_aarch64_sqshlu_nv2di_uss) \
VECTOR_INTRIN(__builtin_aarch64_sqshlu_nv2si_uss) \
VECTOR_INTRIN(__builtin_aarch64_sqshlu_nv4hi_uss) \
VECTOR_INTRIN(__builtin_aarch64_sqshlu_nv4si_uss) \
VECTOR_INTRIN(__builtin_aarch64_sqshlu_nv8hi_uss) \
VECTOR_INTRIN(__builtin_aarch64_sqshlu_nv8qi_uss) \
VECTOR_INTRIN(__builtin_aarch64_sqshlv16qi) \
VECTOR_INTRIN(__builtin_aarch64_sqshlv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqshlv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqshlv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqshlv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqshlv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqshlv8qi) \
VECTOR_INTRIN(__builtin_aarch64_sqshrn_ndi) \
VECTOR_INTRIN(__builtin_aarch64_sqshrn_nhi) \
VECTOR_INTRIN(__builtin_aarch64_sqshrn_nsi) \
VECTOR_INTRIN(__builtin_aarch64_sqshrn_nv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqshrn_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqshrn_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqshrun_ndi) \
VECTOR_INTRIN(__builtin_aarch64_sqshrun_nhi) \
VECTOR_INTRIN(__builtin_aarch64_sqshrun_nsi) \
VECTOR_INTRIN(__builtin_aarch64_sqshrun_nv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqshrun_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqshrun_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqsubdi) \
VECTOR_INTRIN(__builtin_aarch64_sqsubhi) \
VECTOR_INTRIN(__builtin_aarch64_sqsubqi) \
VECTOR_INTRIN(__builtin_aarch64_sqsubsi) \
VECTOR_INTRIN(__builtin_aarch64_sqsubv16qi) \
VECTOR_INTRIN(__builtin_aarch64_sqsubv2di) \
VECTOR_INTRIN(__builtin_aarch64_sqsubv2si) \
VECTOR_INTRIN(__builtin_aarch64_sqsubv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sqsubv4si) \
VECTOR_INTRIN(__builtin_aarch64_sqsubv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sqsubv8qi) \
VECTOR_INTRIN(__builtin_aarch64_srhaddv16qi) \
VECTOR_INTRIN(__builtin_aarch64_srhaddv2si) \
VECTOR_INTRIN(__builtin_aarch64_srhaddv4hi) \
VECTOR_INTRIN(__builtin_aarch64_srhaddv4si) \
VECTOR_INTRIN(__builtin_aarch64_srhaddv8hi) \
VECTOR_INTRIN(__builtin_aarch64_srhaddv8qi) \
VECTOR_INTRIN(__builtin_aarch64_srshldi) \
VECTOR_INTRIN(__builtin_aarch64_srshlv16qi) \
VECTOR_INTRIN(__builtin_aarch64_srshlv2di) \
VECTOR_INTRIN(__builtin_aarch64_srshlv2si) \
VECTOR_INTRIN(__builtin_aarch64_srshlv4hi) \
VECTOR_INTRIN(__builtin_aarch64_srshlv4si) \
VECTOR_INTRIN(__builtin_aarch64_srshlv8hi) \
VECTOR_INTRIN(__builtin_aarch64_srshlv8qi) \
VECTOR_INTRIN(__builtin_aarch64_srshr_ndi) \
VECTOR_INTRIN(__builtin_aarch64_srshr_nv16qi) \
VECTOR_INTRIN(__builtin_aarch64_srshr_nv2di) \
VECTOR_INTRIN(__builtin_aarch64_srshr_nv2si) \
VECTOR_INTRIN(__builtin_aarch64_srshr_nv4hi) \
VECTOR_INTRIN(__builtin_aarch64_srshr_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_srshr_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_srshr_nv8qi) \
VECTOR_INTRIN(__builtin_aarch64_srsra_ndi) \
VECTOR_INTRIN(__builtin_aarch64_srsra_nv16qi) \
VECTOR_INTRIN(__builtin_aarch64_srsra_nv2di) \
VECTOR_INTRIN(__builtin_aarch64_srsra_nv2si) \
VECTOR_INTRIN(__builtin_aarch64_srsra_nv4hi) \
VECTOR_INTRIN(__builtin_aarch64_srsra_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_srsra_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_srsra_nv8qi) \
VECTOR_INTRIN(__builtin_aarch64_sshldi) \
VECTOR_INTRIN(__builtin_aarch64_sshll2_nv16qi) \
VECTOR_INTRIN(__builtin_aarch64_sshll2_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_sshll2_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sshll_nv2si) \
VECTOR_INTRIN(__builtin_aarch64_sshll_nv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sshll_nv8qi) \
VECTOR_INTRIN(__builtin_aarch64_sshlv16qi) \
VECTOR_INTRIN(__builtin_aarch64_sshlv2di) \
VECTOR_INTRIN(__builtin_aarch64_sshlv2si) \
VECTOR_INTRIN(__builtin_aarch64_sshlv4hi) \
VECTOR_INTRIN(__builtin_aarch64_sshlv4si) \
VECTOR_INTRIN(__builtin_aarch64_sshlv8hi) \
VECTOR_INTRIN(__builtin_aarch64_sshlv8qi) \
VECTOR_INTRIN(__builtin_aarch64_ssli_ndi) \
VECTOR_INTRIN(__builtin_aarch64_ssli_nv16qi) \
VECTOR_INTRIN(__builtin_aarch64_ssli_nv2di) \
VECTOR_INTRIN(__builtin_aarch64_ssli_nv2si) \
VECTOR_INTRIN(__builtin_aarch64_ssli_nv4hi) \
VECTOR_INTRIN(__builtin_aarch64_ssli_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_ssli_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_ssli_nv8qi) \
VECTOR_INTRIN(__builtin_aarch64_ssra_ndi) \
VECTOR_INTRIN(__builtin_aarch64_ssra_nv16qi) \
VECTOR_INTRIN(__builtin_aarch64_ssra_nv2di) \
VECTOR_INTRIN(__builtin_aarch64_ssra_nv2si) \
VECTOR_INTRIN(__builtin_aarch64_ssra_nv4hi) \
VECTOR_INTRIN(__builtin_aarch64_ssra_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_ssra_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_ssra_nv8qi) \
VECTOR_INTRIN(__builtin_aarch64_ssri_ndi) \
VECTOR_INTRIN(__builtin_aarch64_ssri_nv16qi) \
VECTOR_INTRIN(__builtin_aarch64_ssri_nv2di) \
VECTOR_INTRIN(__builtin_aarch64_ssri_nv2si) \
VECTOR_INTRIN(__builtin_aarch64_ssri_nv4hi) \
VECTOR_INTRIN(__builtin_aarch64_ssri_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_ssri_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_ssri_nv8qi) \
VECTOR_INTRIN(__builtin_aarch64_ssubl2v16qi) \
VECTOR_INTRIN(__builtin_aarch64_ssubl2v4si) \
VECTOR_INTRIN(__builtin_aarch64_ssubl2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_ssublv2si) \
VECTOR_INTRIN(__builtin_aarch64_ssublv4hi) \
VECTOR_INTRIN(__builtin_aarch64_ssublv8qi) \
VECTOR_INTRIN(__builtin_aarch64_ssubw2v16qi) \
VECTOR_INTRIN(__builtin_aarch64_ssubw2v4si) \
VECTOR_INTRIN(__builtin_aarch64_ssubw2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_ssubwv2si) \
VECTOR_INTRIN(__builtin_aarch64_ssubwv4hi) \
VECTOR_INTRIN(__builtin_aarch64_ssubwv8qi) \
VECTOR_INTRIN(__builtin_aarch64_st1v16qi) \
VECTOR_INTRIN(__builtin_aarch64_st1v2df) \
VECTOR_INTRIN(__builtin_aarch64_st1v2di) \
VECTOR_INTRIN(__builtin_aarch64_st1v2sf) \
VECTOR_INTRIN(__builtin_aarch64_st1v2si) \
VECTOR_INTRIN(__builtin_aarch64_st1v4hi) \
VECTOR_INTRIN(__builtin_aarch64_st1v4sf) \
VECTOR_INTRIN(__builtin_aarch64_st1v4si) \
VECTOR_INTRIN(__builtin_aarch64_st1v8hi) \
VECTOR_INTRIN(__builtin_aarch64_st1v8qi) \
VECTOR_INTRIN(__builtin_aarch64_st2df) \
VECTOR_INTRIN(__builtin_aarch64_st2di) \
VECTOR_INTRIN(__builtin_aarch64_st2_lanev16qi) \
VECTOR_INTRIN(__builtin_aarch64_st2_lanev2df) \
VECTOR_INTRIN(__builtin_aarch64_st2_lanev2di) \
VECTOR_INTRIN(__builtin_aarch64_st2_lanev4sf) \
VECTOR_INTRIN(__builtin_aarch64_st2_lanev4si) \
VECTOR_INTRIN(__builtin_aarch64_st2_lanev8hi) \
VECTOR_INTRIN(__builtin_aarch64_st2v16qi) \
VECTOR_INTRIN(__builtin_aarch64_st2v2df) \
VECTOR_INTRIN(__builtin_aarch64_st2v2di) \
VECTOR_INTRIN(__builtin_aarch64_st2v2sf) \
VECTOR_INTRIN(__builtin_aarch64_st2v2si) \
VECTOR_INTRIN(__builtin_aarch64_st2v4hi) \
VECTOR_INTRIN(__builtin_aarch64_st2v4sf) \
VECTOR_INTRIN(__builtin_aarch64_st2v4si) \
VECTOR_INTRIN(__builtin_aarch64_st2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_st2v8qi) \
VECTOR_INTRIN(__builtin_aarch64_st3df) \
VECTOR_INTRIN(__builtin_aarch64_st3di) \
VECTOR_INTRIN(__builtin_aarch64_st3_lanev16qi) \
VECTOR_INTRIN(__builtin_aarch64_st3_lanev2df) \
VECTOR_INTRIN(__builtin_aarch64_st3_lanev2di) \
VECTOR_INTRIN(__builtin_aarch64_st3_lanev4sf) \
VECTOR_INTRIN(__builtin_aarch64_st3_lanev4si) \
VECTOR_INTRIN(__builtin_aarch64_st3_lanev8hi) \
VECTOR_INTRIN(__builtin_aarch64_st3v16qi) \
VECTOR_INTRIN(__builtin_aarch64_st3v2df) \
VECTOR_INTRIN(__builtin_aarch64_st3v2di) \
VECTOR_INTRIN(__builtin_aarch64_st3v2sf) \
VECTOR_INTRIN(__builtin_aarch64_st3v2si) \
VECTOR_INTRIN(__builtin_aarch64_st3v4hi) \
VECTOR_INTRIN(__builtin_aarch64_st3v4sf) \
VECTOR_INTRIN(__builtin_aarch64_st3v4si) \
VECTOR_INTRIN(__builtin_aarch64_st3v8hi) \
VECTOR_INTRIN(__builtin_aarch64_st3v8qi) \
VECTOR_INTRIN(__builtin_aarch64_st4df) \
VECTOR_INTRIN(__builtin_aarch64_st4di) \
VECTOR_INTRIN(__builtin_aarch64_st4_lanev16qi) \
VECTOR_INTRIN(__builtin_aarch64_st4_lanev2df) \
VECTOR_INTRIN(__builtin_aarch64_st4_lanev2di) \
VECTOR_INTRIN(__builtin_aarch64_st4_lanev4sf) \
VECTOR_INTRIN(__builtin_aarch64_st4_lanev4si) \
VECTOR_INTRIN(__builtin_aarch64_st4_lanev8hi) \
VECTOR_INTRIN(__builtin_aarch64_st4v16qi) \
VECTOR_INTRIN(__builtin_aarch64_st4v2df) \
VECTOR_INTRIN(__builtin_aarch64_st4v2di) \
VECTOR_INTRIN(__builtin_aarch64_st4v2sf) \
VECTOR_INTRIN(__builtin_aarch64_st4v2si) \
VECTOR_INTRIN(__builtin_aarch64_st4v4hi) \
VECTOR_INTRIN(__builtin_aarch64_st4v4sf) \
VECTOR_INTRIN(__builtin_aarch64_st4v4si) \
VECTOR_INTRIN(__builtin_aarch64_st4v8hi) \
VECTOR_INTRIN(__builtin_aarch64_st4v8qi) \
VECTOR_INTRIN(__builtin_aarch64_subhn2v2di) \
VECTOR_INTRIN(__builtin_aarch64_subhn2v4si) \
VECTOR_INTRIN(__builtin_aarch64_subhn2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_subhnv2di) \
VECTOR_INTRIN(__builtin_aarch64_subhnv4si) \
VECTOR_INTRIN(__builtin_aarch64_subhnv8hi) \
VECTOR_INTRIN(__builtin_aarch64_suqadddi_ssu) \
VECTOR_INTRIN(__builtin_aarch64_suqaddhi_ssu) \
VECTOR_INTRIN(__builtin_aarch64_suqaddqi_ssu) \
VECTOR_INTRIN(__builtin_aarch64_suqaddsi_ssu) \
VECTOR_INTRIN(__builtin_aarch64_suqaddv16qi_ssu) \
VECTOR_INTRIN(__builtin_aarch64_suqaddv2di_ssu) \
VECTOR_INTRIN(__builtin_aarch64_suqaddv2si_ssu) \
VECTOR_INTRIN(__builtin_aarch64_suqaddv4hi_ssu) \
VECTOR_INTRIN(__builtin_aarch64_suqaddv4si_ssu) \
VECTOR_INTRIN(__builtin_aarch64_suqaddv8hi_ssu) \
VECTOR_INTRIN(__builtin_aarch64_suqaddv8qi_ssu) \
VECTOR_INTRIN(__builtin_aarch64_uaddl2v16qi) \
VECTOR_INTRIN(__builtin_aarch64_uaddl2v4si) \
VECTOR_INTRIN(__builtin_aarch64_uaddl2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_uaddlv2si) \
VECTOR_INTRIN(__builtin_aarch64_uaddlv4hi) \
VECTOR_INTRIN(__builtin_aarch64_uaddlv8qi) \
VECTOR_INTRIN(__builtin_aarch64_uaddw2v16qi) \
VECTOR_INTRIN(__builtin_aarch64_uaddw2v4si) \
VECTOR_INTRIN(__builtin_aarch64_uaddw2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_uaddwv2si) \
VECTOR_INTRIN(__builtin_aarch64_uaddwv4hi) \
VECTOR_INTRIN(__builtin_aarch64_uaddwv8qi) \
VECTOR_INTRIN(__builtin_aarch64_uhaddv16qi) \
VECTOR_INTRIN(__builtin_aarch64_uhaddv2si) \
VECTOR_INTRIN(__builtin_aarch64_uhaddv4hi) \
VECTOR_INTRIN(__builtin_aarch64_uhaddv4si) \
VECTOR_INTRIN(__builtin_aarch64_uhaddv8hi) \
VECTOR_INTRIN(__builtin_aarch64_uhaddv8qi) \
VECTOR_INTRIN(__builtin_aarch64_uhsubv16qi) \
VECTOR_INTRIN(__builtin_aarch64_uhsubv2si) \
VECTOR_INTRIN(__builtin_aarch64_uhsubv4hi) \
VECTOR_INTRIN(__builtin_aarch64_uhsubv4si) \
VECTOR_INTRIN(__builtin_aarch64_uhsubv8hi) \
VECTOR_INTRIN(__builtin_aarch64_uhsubv8qi) \
VECTOR_INTRIN(__builtin_aarch64_umaxpv16qi) \
VECTOR_INTRIN(__builtin_aarch64_umaxpv2si) \
VECTOR_INTRIN(__builtin_aarch64_umaxpv4hi) \
VECTOR_INTRIN(__builtin_aarch64_umaxpv4si) \
VECTOR_INTRIN(__builtin_aarch64_umaxpv8hi) \
VECTOR_INTRIN(__builtin_aarch64_umaxpv8qi) \
VECTOR_INTRIN(__builtin_aarch64_umaxv16qi) \
VECTOR_INTRIN(__builtin_aarch64_umaxv2si) \
VECTOR_INTRIN(__builtin_aarch64_umaxv4hi) \
VECTOR_INTRIN(__builtin_aarch64_umaxv4si) \
VECTOR_INTRIN(__builtin_aarch64_umaxv8hi) \
VECTOR_INTRIN(__builtin_aarch64_umaxv8qi) \
VECTOR_INTRIN(__builtin_aarch64_uminpv16qi) \
VECTOR_INTRIN(__builtin_aarch64_uminpv2si) \
VECTOR_INTRIN(__builtin_aarch64_uminpv4hi) \
VECTOR_INTRIN(__builtin_aarch64_uminpv4si) \
VECTOR_INTRIN(__builtin_aarch64_uminpv8hi) \
VECTOR_INTRIN(__builtin_aarch64_uminpv8qi) \
VECTOR_INTRIN(__builtin_aarch64_uminv16qi) \
VECTOR_INTRIN(__builtin_aarch64_uminv2si) \
VECTOR_INTRIN(__builtin_aarch64_uminv4hi) \
VECTOR_INTRIN(__builtin_aarch64_uminv4si) \
VECTOR_INTRIN(__builtin_aarch64_uminv8hi) \
VECTOR_INTRIN(__builtin_aarch64_uminv8qi) \
VECTOR_INTRIN(__builtin_aarch64_uqadddi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqaddhi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqaddqi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqaddsi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqaddv16qi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqaddv2di_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqaddv2si_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqaddv4hi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqaddv4si_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqaddv8hi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqaddv8qi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqmovndi) \
VECTOR_INTRIN(__builtin_aarch64_uqmovnhi) \
VECTOR_INTRIN(__builtin_aarch64_uqmovnsi) \
VECTOR_INTRIN(__builtin_aarch64_uqmovnv2di) \
VECTOR_INTRIN(__builtin_aarch64_uqmovnv4si) \
VECTOR_INTRIN(__builtin_aarch64_uqmovnv8hi) \
VECTOR_INTRIN(__builtin_aarch64_uqrshldi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshlhi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshlqi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshlsi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshlv16qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshlv2di_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshlv2si_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshlv4hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshlv4si_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshlv8hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshlv8qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshrn_ndi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshrn_nhi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshrn_nsi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshrn_nv2di_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshrn_nv4si_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqrshrn_nv8hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshldi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshlhi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshl_ndi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshl_nhi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshl_nqi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshl_nsi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshl_nv16qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshl_nv2di_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshl_nv2si_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshl_nv4hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshl_nv4si_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshl_nv8hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshl_nv8qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshlqi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshlsi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshlv16qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshlv2di_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshlv2si_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshlv4hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshlv4si_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshlv8hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshlv8qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshrn_ndi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshrn_nhi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshrn_nsi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshrn_nv2di_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshrn_nv4si_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqshrn_nv8hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_uqsubdi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqsubhi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqsubqi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqsubsi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqsubv16qi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqsubv2di_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqsubv2si_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqsubv4hi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqsubv4si_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqsubv8hi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_uqsubv8qi_uuu) \
VECTOR_INTRIN(__builtin_aarch64_urecpev2si) \
VECTOR_INTRIN(__builtin_aarch64_urecpev4si) \
VECTOR_INTRIN(__builtin_aarch64_urhaddv16qi) \
VECTOR_INTRIN(__builtin_aarch64_urhaddv2si) \
VECTOR_INTRIN(__builtin_aarch64_urhaddv4hi) \
VECTOR_INTRIN(__builtin_aarch64_urhaddv4si) \
VECTOR_INTRIN(__builtin_aarch64_urhaddv8hi) \
VECTOR_INTRIN(__builtin_aarch64_urhaddv8qi) \
VECTOR_INTRIN(__builtin_aarch64_urshldi_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshlv16qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshlv2di_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshlv2si_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshlv4hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshlv4si_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshlv8hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshlv8qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshr_ndi_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshr_nv16qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshr_nv2di_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshr_nv2si_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshr_nv4hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshr_nv4si_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshr_nv8hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_urshr_nv8qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_ursra_ndi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_ursra_nv16qi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_ursra_nv2di_uuus) \
VECTOR_INTRIN(__builtin_aarch64_ursra_nv2si_uuus) \
VECTOR_INTRIN(__builtin_aarch64_ursra_nv4hi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_ursra_nv4si_uuus) \
VECTOR_INTRIN(__builtin_aarch64_ursra_nv8hi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_ursra_nv8qi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_ushldi_uus) \
VECTOR_INTRIN(__builtin_aarch64_ushll2_nv16qi) \
VECTOR_INTRIN(__builtin_aarch64_ushll2_nv4si) \
VECTOR_INTRIN(__builtin_aarch64_ushll2_nv8hi) \
VECTOR_INTRIN(__builtin_aarch64_ushll_nv2si_uus) \
VECTOR_INTRIN(__builtin_aarch64_ushll_nv4hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_ushll_nv8qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_ushlv16qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_ushlv2di_uus) \
VECTOR_INTRIN(__builtin_aarch64_ushlv2si_uus) \
VECTOR_INTRIN(__builtin_aarch64_ushlv4hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_ushlv4si_uus) \
VECTOR_INTRIN(__builtin_aarch64_ushlv8hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_ushlv8qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_usli_ndi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usli_nv16qi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usli_nv2di_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usli_nv2si_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usli_nv4hi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usli_nv4si_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usli_nv8hi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usli_nv8qi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usqadddi_uus) \
VECTOR_INTRIN(__builtin_aarch64_usqaddhi_uus) \
VECTOR_INTRIN(__builtin_aarch64_usqaddqi_uus) \
VECTOR_INTRIN(__builtin_aarch64_usqaddsi_uus) \
VECTOR_INTRIN(__builtin_aarch64_usqaddv16qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_usqaddv2di_uus) \
VECTOR_INTRIN(__builtin_aarch64_usqaddv2si_uus) \
VECTOR_INTRIN(__builtin_aarch64_usqaddv4hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_usqaddv4si_uus) \
VECTOR_INTRIN(__builtin_aarch64_usqaddv8hi_uus) \
VECTOR_INTRIN(__builtin_aarch64_usqaddv8qi_uus) \
VECTOR_INTRIN(__builtin_aarch64_usra_ndi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usra_nv16qi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usra_nv2di_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usra_nv2si_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usra_nv4hi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usra_nv4si_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usra_nv8hi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usra_nv8qi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usri_ndi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usri_nv16qi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usri_nv2di_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usri_nv2si_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usri_nv4hi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usri_nv4si_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usri_nv8hi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usri_nv8qi_uuus) \
VECTOR_INTRIN(__builtin_aarch64_usubl2v16qi) \
VECTOR_INTRIN(__builtin_aarch64_usubl2v4si) \
VECTOR_INTRIN(__builtin_aarch64_usubl2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_usublv2si) \
VECTOR_INTRIN(__builtin_aarch64_usublv4hi) \
VECTOR_INTRIN(__builtin_aarch64_usublv8qi) \
VECTOR_INTRIN(__builtin_aarch64_usubw2v16qi) \
VECTOR_INTRIN(__builtin_aarch64_usubw2v4si) \
VECTOR_INTRIN(__builtin_aarch64_usubw2v8hi) \
VECTOR_INTRIN(__builtin_aarch64_usubwv2si) \
VECTOR_INTRIN(__builtin_aarch64_usubwv4hi) \
VECTOR_INTRIN(__builtin_aarch64_usubwv8qi) \
VECTOR_INTRIN(__builtin_aarch64_vec_unpacks_hi_v4sf) \
VECTOR_INTRIN(__builtin_iceilf) \
VECTOR_INTRIN(__builtin_ifloorf) \
END

static void do_alias(const char* newname, const char* existing)
{
    std::cout << "{\n"
        << "scope_entry_list_t *entry_list = query_in_scope_str(decl_context, uniquestr(\"" << existing << "\"), /* field_path */ NULL);\n"
        << "ERROR_CONDITION(entry_list == NULL, \"Symbol '" << existing << "' should have been declared\",0);\n"
        << "scope_entry_t* orig_sym = entry_list_head(entry_list);\n"
        << "entry_list_free(entry_list);\n"
        << "scope_entry_t* new_sym = new_symbol(decl_context, decl_context->current_scope, uniquestr(\"" << newname << "\"));\n"
        << "new_sym->kind = SK_FUNCTION;"
        << "new_sym->do_not_print = 1;\n"
        << "new_sym->type_information = orig_sym->type_information;\n"
        << "symbol_entity_specs_set_is_builtin(new_sym, 1);\n"
        << "}\n";
}

int main(int, char**)
{
#define VECTOR_INTRIN(X) \
    f<__typeof__(X)>(#X);
#define VECTOR_ALIAS(newname, existing) \
    do_alias(#newname, #existing);
    VECTOR_INTRINSICS_LIST
#undef VECTOR_INTRIN
}
