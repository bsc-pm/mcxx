#ifndef CXX_KOENIG_H
#define CXX_KOENIG_H

#include "cxx-scope.h"
#include "cxx-buildscope.h"

MCXX_BEGIN_DECLS

char koenig_can_be_used(AST called_expression, decl_context_t decl_context);

scope_entry_list_t* koenig_lookup(
        int num_arguments,
        argument_type_info_t** argument_type_list, 
        decl_context_t decl_context);

MCXX_END_DECLS

#endif // CXX_KOENIG_H
