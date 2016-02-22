#ifndef CXX_INTELSUPPORT_H
#define CXX_INTELSUPPORT_H

#include "libmcxx-common.h"
#include "cxx-nodecl.h"
#include "cxx-scope-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN void intel_check_assume(
        AST expression,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_out);
LIBMCXX_EXTERN void intel_check_assume_nodecl(
        nodecl_t assumed_expr,
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_out);

LIBMCXX_EXTERN void intel_check_assume_aligned(
        AST expression,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_out);
LIBMCXX_EXTERN void intel_check_assume_aligned_nodecl(
        nodecl_t pointer_arg,
        nodecl_t alignment,
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_out);

LIBMCXX_EXTERN type_t* get_m64_struct_type(void);

LIBMCXX_EXTERN type_t* get_m128_struct_type(void);
LIBMCXX_EXTERN type_t* get_m128d_struct_type(void);
LIBMCXX_EXTERN type_t* get_m128i_struct_type(void);

LIBMCXX_EXTERN type_t* get_m256_struct_type(void);
LIBMCXX_EXTERN type_t* get_m256d_struct_type(void);
LIBMCXX_EXTERN type_t* get_m256i_struct_type(void);

LIBMCXX_EXTERN type_t* get_m512_struct_type(void);
LIBMCXX_EXTERN type_t* get_m512d_struct_type(void);
LIBMCXX_EXTERN type_t* get_m512i_struct_type(void);

LIBMCXX_EXTERN scope_entry_t* get_m64_typedef(void);

LIBMCXX_EXTERN scope_entry_t* get_m128_typedef(void);
LIBMCXX_EXTERN scope_entry_t* get_m128d_typedef(void);
LIBMCXX_EXTERN scope_entry_t* get_m128i_typedef(void);

LIBMCXX_EXTERN scope_entry_t* get_m256_typedef(void);
LIBMCXX_EXTERN scope_entry_t* get_m256d_typedef(void);
LIBMCXX_EXTERN scope_entry_t* get_m256i_typedef(void);

LIBMCXX_EXTERN scope_entry_t* get_m512_typedef(void);
LIBMCXX_EXTERN scope_entry_t* get_m512d_typedef(void);
LIBMCXX_EXTERN scope_entry_t* get_m512i_typedef(void);


LIBMCXX_EXTERN char is_intel_vector_struct_type(type_t* t, int *size);
LIBMCXX_EXTERN char vector_type_to_intel_vector_struct_reinterpret_type(type_t* orig, type_t* dest);
LIBMCXX_EXTERN char intel_vector_struct_to_intel_vector_struct_reinterpret_type(type_t* orig, type_t* dest);

LIBMCXX_EXTERN type_t* vector_type_get_intel_vector_struct_type(type_t* vector_type);
LIBMCXX_EXTERN scope_entry_t* vector_type_get_intel_vector_typedef(type_t* vector_type);
LIBMCXX_EXTERN type_t* intel_vector_struct_type_get_vector_type(type_t* vector_type);

LIBMCXX_EXTERN void sign_in_icc_intrinsics(const decl_context_t* decl_context);
LIBMCXX_EXTERN void sign_in_intel_simd_types(const decl_context_t* decl_context);

MCXX_END_DECLS

#endif // CXX_INTELSUPPORT_H
