#ifndef CXX_TYPECALC_H
#define CXX_TYPECALC_H

#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

typedef struct {
    int num_types;
    type_t** types;
    value_type_t value_type;

    // These fields below are kludgy and not directly related to the strict
    // type calculus
    //
    // If the invocation is done to an object, the object
    type_t* object_type;
    // The list of overloaded functions entries
    scope_entry_list_t* overloaded_functions;
} calculated_type_t;

MCXX_END_DECLS

#endif // CXX_TYPECALC_H
