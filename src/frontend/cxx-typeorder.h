#ifndef CXX_TYPEORDER_H
#define CXX_TYPEORDER_H

#include "cxx-scope-decls.h"
#include "cxx-typeunif.h"
#include "cxx-buildscope-decls.h"

char is_less_or_equal_specialized_template_class(struct type_tag* c1, struct type_tag* c2, 
        decl_context_t decl_context, deduction_set_t** deduction_set, 
        const char *filename, int line);

char is_less_or_equal_specialized_template_function(struct type_tag* f1, struct type_tag* f2,
        decl_context_t decl_context, deduction_set_t** deduction_set,
        template_argument_list_t* explicit_template_arguments,
        const char *filename, int line, char is_conversion);

char is_sound_type(struct type_tag* t, decl_context_t decl_context);

#endif // CXX_TYPEORDER_H
