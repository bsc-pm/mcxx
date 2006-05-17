#ifndef CXX_TYPEUTILS_H
#define CXX_TYPEUTILS_H

#include "cxx-ast.h"
#include "cxx-scope.h"

char equivalent_types(type_t* t1, type_t* t2, scope_t* st);
char overloaded_function(function_info_t* f1, function_info_t* f2, scope_t* st);

/* Copy functions */
class_info_t* copy_class_info(class_info_t* class_info);
simple_type_t* copy_simple_type(simple_type_t* type_info);
type_t* copy_type(type_t* type);
function_info_t* copy_function_info(function_info_t* function_info);
array_info_t* copy_array_info(array_info_t* array_info);
pointer_info_t* copy_pointer_info(pointer_info_t* pointer_info);
enum_info_t* copy_enum_info(enum_info_t* enum_info);

// Equality functions
char equivalent_builtin_type(simple_type_t *t1, simple_type_t *t2);

// Conversion functions
type_t* simple_type_to_type(simple_type_t* simple_type_info);
char equivalent_simple_types(simple_type_t *t1, simple_type_t *t2, scope_t* st);

// Query functions
const char* get_builtin_type_name(simple_type_t* simple_type_info, scope_t* st);

// Debug purpose functions
void print_declarator(type_t* printed_declarator, scope_t* st);

#endif // CXX_TYPEUTILS_H
