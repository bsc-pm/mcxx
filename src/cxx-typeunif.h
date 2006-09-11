#ifndef CXX_TYPEUNIF_H
#define CXX_TYPEUNIF_H

#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"

typedef struct unification_item_tag
{
    // parameter type <- value
    int parameter_num;
    int parameter_nesting;
    char* parameter_name;
    
    type_t* value;
    AST expression;
} unification_item_t;

typedef struct 
{
    int num_elems;
    unification_item_t** unif_list;
} unification_set_t;

char unificate_two_types(type_t* t1, type_t* t2, scope_t* st, 
        unification_set_t** unif_set, decl_context_t decl_context);

#endif
