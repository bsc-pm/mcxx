#ifndef CXX_TYPECALC_H
#define CXX_TYPECALC_H

#include "cxx-ast.h"
#include "cxx-scope.h"

type_t* calculate_expression_type(AST a, scope_t* st);

type_t* new_fundamental_type(temporary_status_t temporary_status);
type_t* new_boolean_type(temporary_status_t temporary_status);
type_t* new_float_type(temporary_status_t temporary_status);
type_t* new_char_type(temporary_status_t temporary_status);
type_t* new_wchar_type(temporary_status_t temporary_status);
type_t* new_const_char_pointer_type(temporary_status_t temporary_status);
type_t* new_const_wchar_pointer_type(temporary_status_t temporary_status);
type_t* new_int_type(temporary_status_t temporary_status);

char is_fundamental_type(type_t* t);

#endif // CXX_TYPECALC_H
