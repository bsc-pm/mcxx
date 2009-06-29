#ifndef CXX_OMP_SUPPORT_H
#define CXX_OMP_SUPPORT_H

#include "cxx-typeutils.h"
#include "cxx-scope-decls.h"

typedef struct omp_reductor_tag omp_reductor_t;

typedef
enum omp_udr_associativity_tag
{
    OMP_UDR_ORDER_INVALID = 0,
    OMP_UDR_ORDER_LEFT,
    OMP_UDR_ORDER_RIGHT
} omp_udr_associativity_t;

// OMP User defined reductions

void omp_udr_register_reduction_builtin(type_t*, 
        const char* operator_name, 
        AST identity,
        omp_udr_associativity_t);
void omp_udr_register_reduction_function(type_t*,
        scope_entry_t* entry, 
        AST identity,
        omp_udr_associativity_t);

char omp_udr_lookup_function(type_t* t, 
        scope_entry_t* entry, 
        AST* identity, 
        omp_udr_associativity_t* assoc);

char omp_udr_lookup_builtin(type_t* t, 
        const char* operator_name, 
        AST* identity, 
        omp_udr_associativity_t* assoc);

#endif // CXX_OMP_SUPPORT_H
