#ifndef CXX_OVERLOAD_H
#define CXX_OVERLOAD_H

#include "cxx-type-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"

struct scope_entry_tag* solve_overload(struct scope_entry_list_tag* candidate_functions, 
        struct type_tag **argument_types, int num_arguments,
        decl_context_t decl_context,
        const char* filename, int line);

char type_can_be_implicitly_converted_to(struct type_tag* orig, struct type_tag* dest, decl_context_t decl_context, 
        char *ambiguous_conversion);

struct scope_entry_tag* address_of_overloaded_function(struct scope_entry_list_tag* overload_set, 
        template_argument_list_t* explicit_template_arguments,
        struct type_tag* target_type,
        decl_context_t decl_context,
        const char *filename,
        int line);

#endif // CXX_OVERLOAD_H
