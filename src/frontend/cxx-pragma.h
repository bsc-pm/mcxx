#ifndef CXX_PRAGMA_H
#define CXX_PRAGMA_H

#include "cxx-ast-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-nodecl.h"

void common_build_scope_pragma_custom_construct(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output,
        nodecl_t* nodecl_pragma_line,
        void (*function_for_child)(AST, decl_context_t, nodecl_t*, void* info), 
        void *info);

void common_build_scope_pragma_custom_directive(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output);

void common_build_scope_pragma_custom_line(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output);

#endif // CXX_PRAGMA_H
