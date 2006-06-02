#ifndef CXX_TYPECALC_H
#define CXX_TYPECALC_H

#include "cxx-ast.h"
#include "cxx-scope.h"

typedef struct {
	int num_types;
	type_t** types;
	value_type_t value_type;
} calculated_type_t;

calculated_type_t* calculate_expression_type(AST a, scope_t* st);

type_t* new_fundamental_type(void);
type_t* new_bool_type(void);
type_t* new_float_type(void);
type_t* new_char_type(void);
type_t* new_wchar_type(void);
type_t* new_const_char_pointer_type(void);
type_t* new_const_wchar_pointer_type(void);
type_t* new_int_type(void);


#endif // CXX_TYPECALC_H
