#ifndef CXX_MSSUPPORT_H
#define CXX_MSSUPPORT_H

#include "libmcxx-common.h"
#include "cxx-ast-decls.h"
#include "cxx-buildscope-decls.h"

LIBMCXX_EXTERN void gather_ms_declspec(AST attribute, 
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context);

LIBMCXX_EXTERN void gather_ms_declspec_list(AST attribute,
        gather_decl_spec_t* gather_info,
        decl_context_t decl_context);

LIBMCXX_EXTERN void keep_ms_declspecs_in_symbol(
        scope_entry_t* entry,
        gather_decl_spec_t* gather_info);

LIBMCXX_EXTERN void apply_ms_attribute_to_type(AST a, type_t** type,
        decl_context_t decl_context);

#endif // CXX_MSSUPPORT_H
