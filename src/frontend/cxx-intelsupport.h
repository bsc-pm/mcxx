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



MCXX_END_DECLS

#endif // CXX_INTELSUPPORT_H
