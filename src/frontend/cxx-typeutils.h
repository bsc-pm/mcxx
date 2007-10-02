/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef CXX_TYPEUTILS_H
#define CXX_TYPEUTILS_H

#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-macros.h"
#include "cxx-solvetemplate.h"

MCXX_BEGIN_DECLS

/* Type constructors: Builtins */
type_t* get_char_type(void);
type_t* get_signed_char_type(void);
type_t* get_unsigned_char_type(void);
type_t* get_wchar_t_type(void);
type_t* get_bool_type(void);
type_t* get_signed_int_type(void);
type_t* get_signed_short_int_type(void);
type_t* get_signed_long_int_type(void);
type_t* get_signed_long_long_int_type(void);
type_t* get_unsigned_int_type(void);
type_t* get_unsigned_short_int_type(void);
type_t* get_unsigned_int_type(void);
type_t* get_unsigned_long_int_type(void);
type_t* get_unsigned_long_long_int_type(void);
type_t* get_float_type(void);
type_t* get_void_type(void);
type_t* get_double_type(void);
type_t* get_long_double_type(void);

type_t* get_gcc_typeof_type(AST type_tree, decl_context_t decl_context);
type_t* get_gcc_typeof_expr_type(AST type_expr, decl_context_t decl_context);

type_t* get_gcc_builtin_va_list_type(void);

type_t* get_user_defined_type(scope_entry_t* entry);

type_t* get_template_dependent_type(AST tree, decl_context_t decl_context);

type_t* get_new_enum_type(decl_context_t decl_context);
type_t* get_new_class_type(decl_context_t decl_context);

type_t* get_complex_type(type_t* t);

type_t* get_new_typedef(type_t* t);

/* Type constructors: cv-qualification */
// The given cv_qualifier is strictly the one will have the returning type
type_t* get_cv_qualified_type(type_t* t, cv_qualifier_t cv_qualifier);

// These add 'const', 'volatile' or 'restrict' to the current qualification of t
type_t* get_const_qualified_type(type_t* t);
type_t* get_volatile_qualified_type(type_t* t);
type_t* get_restrict_qualified_type(type_t* t);

/* Type constructors: derived types */
type_t* get_pointer_type(type_t*);

type_t* get_pointer_to_member_type(type_t*, scope_entry_t* class_entry);

type_t* get_reference_type(type_t* t);

type_t* get_array_type(type_t*, AST expression, decl_context_t decl_context);

type_t* get_function_type(type_t* t);
type_t* get_conversion_type(type_t* t);

/* Type comparison functions */
enum cv_equivalence_t
{
    CVE_UNKNOWN = 0,
    CVE_IGNORE_OUTERMOST,
    CVE_CONSIDER
};

char equivalent_types(type_t* t1, type_t* t2,
        enum cv_equivalence_t cv_equiv, decl_context_t decl_context);
char overloaded_function(type_t* f1, type_t* f2,
        decl_context_t decl_context);
char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2);

/* Modifiers used when the type is still being built */
void function_type_set_lacking_prototype(type_t* function_type, char lacks_prototype);
void function_type_set_has_ellipsis(type_t* function_type);
void function_type_add_parameter(type_t* function_type, 
        type_t* prototype_type, 
        type_t* parameter_type, 
        AST default_argument, 
        decl_context_t default_arg_context);
void function_type_set_no_parameters(type_t* function_type);
void function_type_set_static(type_t* function_type, char is_static);
void function_type_set_inline(type_t* function_type, char is_inline);
void function_type_set_virtual(type_t* function_type, char is_virtual);
void function_type_set_explicit(type_t* function_type, char is_explicit);
void function_type_set_exception_spec(type_t* function_type);
void function_type_add_exception_spec(type_t* function_type, type_t* exception_type);
void function_type_set_template_information(type_t* function_type,
        int template_nesting,
        int num_template_parameters,
        template_parameter_t** template_parameters);
void function_type_set_template_body(type_t* type, AST function_body);
void function_type_set_is_constructor(type_t* entry, char is_constructor);

void class_type_add_base_class(type_t* class_type, scope_entry_t* base_class, char is_virtual);
void class_type_set_inner_context(type_t* class_type, decl_context_t decl_context);
void class_type_add_constructor(type_t* class_type, scope_entry_t* entry);
void class_type_set_destructor(type_t* class_type, scope_entry_t* entry);
void class_type_add_operator_function(type_t* class_type, scope_entry_t* entry);
void class_type_add_conversion_function(type_t* class_type, scope_entry_t* entry);
void class_type_add_nonstatic_data_member(type_t* class_type, scope_entry_t* entry);
void class_type_add_static_data_member(type_t* class_type, scope_entry_t* entry);
void class_type_set_incomplete_dependent(type_t* t);
void class_type_set_complete_dependent(type_t* t);
void class_type_set_incomplete_independent(type_t* t);
void class_type_set_complete_independent(type_t* t);
void class_type_set_instantiation_trees(type_t* t, AST body, AST base_clause);
void class_type_add_constructor(type_t* t, scope_entry_t* entry);
void class_type_set_destructor(type_t* t, scope_entry_t* entry);

void template_type_set_template_arguments(type_t* t, template_argument_list_t* list);
void template_type_set_template_match_pair(type_t* t, matching_pair_t* match_pair);

void enum_type_add_enumerator(type_t* t, scope_entry_t* entry);

/* Query functions: is-a-kind-of-type functions */
char is_builtin_type(type_t* t);
char is_fundamental_type(type_t* t);

char is_integral_type(type_t* t); 
char is_enumerated_type(type_t* t);

char is_signed_int_type(type_t *t);
char is_unsigned_int_type(type_t *t);
char is_signed_short_int_type(type_t *t);
char is_unsigned_short_int_type(type_t *t);
char is_signed_long_int_type(type_t *t);
char is_unsigned_long_int_type(type_t *t);
char is_signed_long_long_int_type(type_t *t);
char is_unsigned_long_long_int_type(type_t *t);

char is_char_type(type_t* t);
char is_signed_char_type(type_t* t);
char is_unsigned_char_type(type_t* t);

char is_wchar_t_type(type_t* t);

char is_floating_type(type_t* t);
char is_double_type(type_t* t);
char is_long_double_type(type_t* t);
char is_float_type(type_t* t);

char is_pointer_type(type_t* t1);

char is_array_type(type_t* t1);

char is_function_type(type_t* t);
char is_conversion_type(type_t* t);

char is_reference_type(type_t* t1);

char is_class_type(type_t* possible_class);
char is_unnamed_class_type(type_t* possible_class);
char is_named_class_type(type_t* possible_class);

char is_named_type(type_t* t);

char is_void_type(type_t* t);
char is_void_pointer_type(type_t* t1);

char is_bool_type(type_t* t1);

char is_non_derived_type(type_t* t);

char is_dependent_type(type_t* type, decl_context_t decl_context);

char is_template_dependent_type(type_t* t);

char is_complex_type(type_t* t);

/* Query functions: cv-qualification */
cv_qualifier_t* get_outermost_cv_qualifier(type_t* t);
type_t* get_unqualified_type(type_t* t);
cv_qualifier_t get_cv_qualifier(type_t* type_info);

/* Query functions: specific ones */
char function_type_get_static(type_t* function_type);
char function_type_get_inline(type_t* function_type);
char function_type_get_virtual(type_t* function_type);
char function_type_get_explicit(type_t* function_type);
char function_type_get_is_constructor(type_t* function_type);
int function_type_get_num_parameters(type_t* function_type);
type_t* function_type_get_parameter_type_num(type_t* function_type, int num_param);
char function_type_get_lacking_prototype(type_t* function_type);
char function_type_get_has_ellipsis(type_t* function_type);
char function_type_is_conversion(type_t* t);
type_t* function_type_get_return_type(type_t* t);

type_t* pointer_type_get_pointee_type(type_t *t);
scope_entry_t* pointer_to_member_type_get_class(type_t *t);
type_t* pointer_to_member_type_get_class_type(type_t *t);

type_t* array_type_get_element_type(type_t* t);
AST array_type_get_array_size_expr(type_t* t);
decl_context_t array_type_get_array_size_expr_context(type_t* t);

template_argument_list_t* template_type_get_template_arguments(type_t* t);
matching_pair_t* template_type_get_template_match_pair(type_t* t);

int class_type_get_num_bases(type_t* class_type);
scope_entry_t* class_type_get_base_num(type_t* class_type, int num, char *is_virtual);
int class_type_num_constructors(type_t* t);
scope_entry_t* class_type_get_constructors_num(type_t* t, int num);
scope_entry_t* class_type_get_destructor(type_t* t);
decl_context_t class_type_get_context(type_t* t);
void class_type_get_instantiation_trees(type_t* t, AST *body, AST *base_clause);
decl_context_t class_type_get_inner_context(type_t* class_type);

decl_context_t enum_type_get_context(type_t* t);

scope_entry_t* named_type_get_symbol(type_t* t);

type_t* get_foundational_type(type_t* t);

/* Query functions: Miscelaneous stuff not classified otherwise */
char is_base_class_of(type_t* possible_base, type_t* possible_derived);

type_t* advance_over_typedefs(type_t* t);
type_t* advance_over_typedefs_with_cv_qualif(type_t* t1, cv_qualifier_t* cv_qualif);

char can_be_promoted_to_dest(type_t* orig, type_t* dest);
char can_be_converted_to_dest(type_t* orig, type_t* dest);

type_t* reference_type_get_referenced_type(type_t* t1);
char is_reference_related(type_t* rt1, type_t* rt2, 
        decl_context_t decl_context);
char is_reference_compatible(type_t* t1, type_t* t2, 
        decl_context_t decl_context);

char pointer_can_be_converted_to_dest(type_t* orig, type_t* dest, 
        char* to_void, char* derived_to_base, char* cv_adjust,
        decl_context_t decl_context);

type_t* get_actual_class_type(type_t* class_type);

char is_dependent_expression(AST expr, decl_context_t decl_context);

char is_specialized_class_type(type_t* type);


char is_pointer_to_member_type(type_t* t);

char is_pointer_to_class_type(type_t* t1);
char is_reference_to_class_type(type_t* t1);
char is_typedef_type(type_t* t1);

type_t* typedef_type_get_aliased_type(type_t* t);

scope_entry_t* give_real_entry(scope_entry_t* entry);

cv_qualifier_t* get_innermost_cv_qualifier(type_t* t);

char class_type_is_incomplete_dependent(type_t* t);
char class_type_is_complete_dependent(type_t* t);
char class_type_is_incomplete_independent(type_t* t);
char class_type_is_complete_independent(type_t* t);

/* Naming types functions */
char* get_declaration_string_internal(type_t* type_info, 
        decl_context_t decl_context,
        const char* symbol_name, 
        const char* initializer, 
        char semicolon,
        int *num_parameter_names,
        char ***parameter_names,
        char is_parameter);
char* get_simple_type_name_string(decl_context_t decl_context, type_t* type_info);
char* get_named_type_name(scope_entry_t* entry);

/* Debug purpose functions */
char* print_declarator(type_t* printed_declarator, decl_context_t decl_context);

MCXX_END_DECLS

#endif // CXX_TYPEUTILS_H
