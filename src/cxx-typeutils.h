#ifndef CXX_TYPEUTILS_H
#define CXX_TYPEUTILS_H

#include "cxx-ast.h"
#include "cxx-symtab.h"

char equivalent_types(type_t* t1, type_t* t2, symtab_t* st);

/* Copy functions */
class_info_t* copy_class_info(class_info_t* class_info);
simple_type_t* copy_simple_type(simple_type_t* type_info);
type_t* copy_type(type_t* type);
function_info_t* copy_function_info(function_info_t* function_info);
array_info_t* copy_array_info(array_info_t* array_info);
pointer_info_t* copy_pointer_info(pointer_info_t* pointer_info);
enum_info_t* copy_enum_info(enum_info_t* enum_info);

// Conversion functions
type_t* simple_type_to_type(simple_type_t* simple_type_info);

// Query functions
const char* get_builtin_type_name(simple_type_t* simple_type_info, symtab_t* st);

// Debug purpose functions
void print_declarator(type_t* printed_declarator, symtab_t* st);

#endif // CXX_TYPEUTILS_H
