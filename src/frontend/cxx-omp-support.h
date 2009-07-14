#ifndef CXX_OMP_SUPPORT_H
#define CXX_OMP_SUPPORT_H

#include "cxx-macros.h"
#include "cxx-typeutils.h"
#include "cxx-scope-decls.h"

MCXX_BEGIN_DECLS

typedef struct omp_reductor_tag omp_reductor_t;

typedef
enum omp_udr_associativity_tag
{
    OMP_UDR_ORDER_INVALID = 0,
    OMP_UDR_ORDER_LEFT,
    OMP_UDR_ORDER_RIGHT
} omp_udr_associativity_t;

// OMP User defined reductions

void omp_udr_register_reduction(type_t* type, 
        const char* reductor_name,
        AST identity,
        omp_udr_associativity_t assoc);

void omp_udr_register_reduction_builtin(type_t* type, 
        const char* reductor_name,
        AST identity,
        omp_udr_associativity_t assoc);

char omp_udr_lookup_reduction(type_t* t, 
        const char* reductor_name,
        AST* identity, 
        omp_udr_associativity_t* assoc,
        char *is_builtin);

void omp_udr_initialize_basic_types(decl_context_t decl_context);

MCXX_END_DECLS

#endif // CXX_OMP_SUPPORT_H
