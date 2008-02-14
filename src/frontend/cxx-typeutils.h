/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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

#include "cxx-type-decls.h"
#include "cxx-ast-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-macros.h"
#include "cxx-solvetemplate.h"

MCXX_BEGIN_DECLS

standard_conversion_t identity_scs(struct type_tag* orig, struct type_tag* dest);

char standard_conversion_is_identity(standard_conversion_t);
char standard_conversion_is_invalid(standard_conversion_t);
struct type_tag* standard_conversion_get_orig_type(standard_conversion_t scs);
struct type_tag* standard_conversion_get_dest_type(standard_conversion_t scs);

char standard_conversion_between_types(standard_conversion_t *result, 
        struct type_tag* orig, struct type_tag* dest, 
        decl_context_t decl_context);

// Type environment 
// - This is yet very EXPERIMENTAL. Ignore it for now -
typedef struct type_environment_tag type_environment_t;
// An hypothetical type environment for Linux IA32
extern type_environment_t* type_environment_linux_ia32;
// End of the type environment related stuff

/* Type constructors: Builtins */
struct type_tag* get_char_type(void);
struct type_tag* get_signed_char_type(void);
struct type_tag* get_unsigned_char_type(void);
struct type_tag* get_wchar_t_type(void);
struct type_tag* get_bool_type(void);
struct type_tag* get_signed_int_type(void);
struct type_tag* get_signed_short_int_type(void);
struct type_tag* get_signed_long_int_type(void);
struct type_tag* get_signed_long_long_int_type(void);
struct type_tag* get_unsigned_int_type(void);
struct type_tag* get_unsigned_short_int_type(void);
struct type_tag* get_unsigned_int_type(void);
struct type_tag* get_size_t_type(void);
struct type_tag* get_unsigned_long_int_type(void);
struct type_tag* get_unsigned_long_long_int_type(void);
struct type_tag* get_float_type(void);
struct type_tag* get_void_type(void);
struct type_tag* get_double_type(void);
struct type_tag* get_long_double_type(void);

struct type_tag* get_gcc_typeof_type(struct AST_tag* type_tree, decl_context_t decl_context);
struct type_tag* get_gcc_typeof_expr_type(struct AST_tag* type_expr, decl_context_t decl_context);

struct type_tag* get_gcc_builtin_va_list_type(void);

struct type_tag* get_user_defined_type(struct scope_entry_tag* entry);

struct type_tag* get_dependent_typename_type(struct scope_entry_tag* dependent_entity, 
        struct AST_tag* nested_name, struct AST_tag* unqualified_part);

struct type_tag* get_new_enum_type(decl_context_t decl_context);
struct type_tag* get_new_class_type(decl_context_t decl_context);

struct type_tag* get_new_template_type(template_parameter_list_t* template_parameter_list, struct type_tag* primary_type,
        const char* template_name, decl_context_t decl_context, int line, const char* filename);

struct type_tag* get_complex_type(struct type_tag* t);

struct type_tag* get_new_typedef(struct type_tag* t);

struct type_tag* get_unresolved_overloaded_type(struct scope_entry_list_tag* overload_set,
        template_argument_list_t* explicit_template_arguments);
template_argument_list_t* unresolved_overloaded_type_get_explicit_template_arguments(struct type_tag* t);

struct type_tag* get_dependent_expr_type(void);

struct type_tag* get_zero_type(void);

struct type_tag* get_pseudo_destructor_call_type(void);

type_t* get_literal_string_type(int length, char is_wchar);

struct type_tag* get_throw_expr_type(void);

/* Type constructors: cv-qualification */
// The given cv_qualifier is strictly the one will have the returning type
struct type_tag* get_cv_qualified_type(struct type_tag* t, cv_qualifier_t cv_qualifier);

// These add 'const', 'volatile' or 'restrict' to the current qualification of t
struct type_tag* get_const_qualified_type(struct type_tag* t);
struct type_tag* get_volatile_qualified_type(struct type_tag* t);
struct type_tag* get_restrict_qualified_type(struct type_tag* t);

/* Type constructors: derived types */
struct type_tag* get_pointer_type(struct type_tag*);

struct type_tag* get_pointer_to_member_type(struct type_tag*, struct scope_entry_tag* class_entry);

struct type_tag* get_reference_type(struct type_tag* t);

struct type_tag* get_array_type(struct type_tag*, struct AST_tag* expression, decl_context_t decl_context);

struct type_tag* get_function_type(struct type_tag* t, parameter_info_t* parameter_info, int num_parameters) DEPRECATED;
struct type_tag* get_new_function_type(struct type_tag* t, parameter_info_t* parameter_info, int num_parameters);
struct type_tag* get_nonproto_function_type(struct type_tag* t, int num_parameters);

struct type_tag* get_vector_type(struct type_tag* element_type, unsigned int vector_size);


/* Type comparison functions */
char equivalent_types(struct type_tag* t1, struct type_tag* t2, decl_context_t decl_context);
char overloaded_function(struct type_tag* f1, struct type_tag* f2,
        decl_context_t decl_context) DEPRECATED;
char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2);

/* Modifiers used when the type is still being built */

void class_type_add_base_class(struct type_tag* class_type, struct scope_entry_tag* base_class, char is_virtual);
void class_type_set_inner_context(struct type_tag* class_type, decl_context_t decl_context);
void class_type_add_constructor(struct type_tag* class_type, struct scope_entry_tag* entry);
void class_type_set_destructor(struct type_tag* class_type, struct scope_entry_tag* entry);
void class_type_add_copy_assignment_operator(struct type_tag* class_type, struct scope_entry_tag* entry);
void class_type_add_copy_constructor(struct type_tag* class_type, struct scope_entry_tag* entry);
void class_type_add_conversion_function(struct type_tag* class_type, struct scope_entry_tag* entry);
void class_type_add_nonstatic_data_member(struct type_tag* class_type, struct scope_entry_tag* entry);
void class_type_add_static_data_member(struct type_tag* class_type, struct scope_entry_tag* entry);
void class_type_set_incomplete_dependent(struct type_tag* t);
void class_type_set_complete_dependent(struct type_tag* t);
void class_type_set_incomplete_independent(struct type_tag* t);
void class_type_set_complete_independent(struct type_tag* t);
void class_type_set_instantiation_trees(struct type_tag* t, struct AST_tag* body, struct AST_tag* base_clause);
void class_type_add_constructor(struct type_tag* t, struct scope_entry_tag* entry);
void class_type_set_destructor(struct type_tag* t, struct scope_entry_tag* entry);
void class_type_set_is_dependent(struct type_tag* t, char is_dependent);

void enum_type_add_enumerator(struct type_tag* t, struct scope_entry_tag* entry);

struct type_tag* unnamed_class_enum_type_set_name(struct type_tag* t, struct scope_entry_tag* entry);

void template_type_set_related_symbol(struct type_tag* t, struct scope_entry_tag*);

/* Query functions: is-a-kind-of-type functions */
char is_builtin_type(struct type_tag* t);
char is_fundamental_type(struct type_tag* t);

// Any type of 'int' nature regardless of being signed or not 
// (int, short, long, long long)
char is_any_int_type(struct type_tag* t);
// Like the previous but only for unsigned
char is_any_unsigned_int_type(struct type_tag* t);
// Like the previous but only for signed
char is_any_signed_int_type(struct type_tag* t);

// char, wchar_t, bool and any integer
char is_integral_type(struct type_tag* t); 
// A synonim in the standard
char is_integer_type(struct type_tag* t); 
char is_enumerated_type(struct type_tag* t);

char is_signed_int_type(type_t *t);
char is_unsigned_int_type(type_t *t);
char is_signed_short_int_type(type_t *t);
char is_unsigned_short_int_type(type_t *t);
char is_signed_long_int_type(type_t *t);
char is_unsigned_long_int_type(type_t *t);
char is_signed_long_long_int_type(type_t *t);
char is_unsigned_long_long_int_type(type_t *t);

char is_character_type(struct type_tag* t);
char is_char_type(struct type_tag* t);
char is_signed_char_type(struct type_tag* t);
char is_unsigned_char_type(struct type_tag* t);

char is_wchar_t_type(struct type_tag* t);

char is_floating_type(struct type_tag* t);
char is_double_type(struct type_tag* t);
char is_long_double_type(struct type_tag* t);
char is_float_type(struct type_tag* t);

// Either floating type or integral type (note that integral types include
// char, wchar_t, bool and all sorts of 'int')
char is_arithmetic_type(struct type_tag* t);

// Either floating or any int (this is like 'is_arithmetic_type' but
// excluding char, wchar_t and bool)
char is_int_or_floating_type(struct type_tag* t);

char is_pointer_type(struct type_tag* t1);

char is_array_type(struct type_tag* t1);

char is_function_type(struct type_tag* t);

char is_reference_type(struct type_tag* t1);

char is_vector_type(struct type_tag* t);

char is_class_type(struct type_tag* possible_class);
char is_unnamed_class_type(struct type_tag* possible_class);
char is_named_class_type(struct type_tag* possible_class);

char is_named_type(struct type_tag* t);

char is_void_type(struct type_tag* t);
char is_void_pointer_type(struct type_tag* t1);

char is_gcc_builtin_va_list(type_t *t);

char is_bool_type(struct type_tag* t1);

char is_non_derived_type(struct type_tag* t);

char is_dependent_type(struct type_tag* type, decl_context_t decl_context);

char is_dependent_typename_type(struct type_tag* t);

char is_complex_type(struct type_tag* t);

char is_unresolved_overloaded_type(struct type_tag* t);
char is_dependent_expr_type(struct type_tag* t);

char is_zero_type(struct type_tag* t);

char is_throw_expr_type(struct type_tag* t);

char is_pseudo_destructor_call_type(type_t *t);

char is_literal_string_type(struct type_tag* t);

char is_template_type(struct type_tag* t);

// A type returned by template_type_get_primary_type or template_type_get_specialized_type
char is_template_specialized_type(struct type_tag* t);

/* Query functions: cv-qualification */
struct type_tag* get_unqualified_type(struct type_tag* t);
cv_qualifier_t get_cv_qualifier(struct type_tag* type_info);

char is_less_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2);
char is_equal_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2);
char is_less_or_equal_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2);
char is_more_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2);
char is_more_or_equal_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2);

char is_less_cv_qualified_type(struct type_tag* t1, struct type_tag* t2);
char is_equally_cv_qualified_type(struct type_tag* t1, struct type_tag* t2);
char is_less_or_equal_cv_qualified_type(struct type_tag* t1, struct type_tag* t2);
char is_more_cv_qualified_type(struct type_tag* t1, struct type_tag* t2);
char is_more_or_equal_cv_qualified_type(struct type_tag* t1, struct type_tag* t2);

char is_const_qualified_type(struct type_tag* t1);
char is_volatile_qualified_type(struct type_tag* t1);
char is_restrict_qualified_type(struct type_tag* t1);

char is_const_qualified(cv_qualifier_t cv);
char is_volatile_qualified(cv_qualifier_t cv);
char is_restrict_qualified(cv_qualifier_t cv);

int get_sizeof_type(struct type_tag* t);

/* Query functions: specific ones */
int function_type_get_num_parameters(struct type_tag* function_type);
struct type_tag* function_type_get_parameter_type_num(struct type_tag* function_type, int num_param);
char function_type_get_lacking_prototype(struct type_tag* function_type);
char function_type_get_has_ellipsis(struct type_tag* function_type);
struct type_tag* function_type_get_return_type(struct type_tag* t);

struct type_tag* pointer_type_get_pointee_type(type_t *t);
struct scope_entry_tag* pointer_to_member_type_get_class(type_t *t);
struct type_tag* pointer_to_member_type_get_class_type(type_t *t);

scope_entry_list_t *unresolved_overloaded_type_get_overload_set(struct type_tag* t);

struct type_tag* array_type_get_element_type(struct type_tag* t);
struct AST_tag* array_type_get_array_size_expr(struct type_tag* t);
decl_context_t array_type_get_array_size_expr_context(struct type_tag* t);

int class_type_get_num_bases(struct type_tag* class_type);
struct scope_entry_tag* class_type_get_base_num(struct type_tag* class_type, int num, char *is_virtual);
int class_type_get_num_constructors(struct type_tag* t);
struct scope_entry_tag* class_type_get_constructors_num(struct type_tag* t, int num);

int class_type_get_num_nonstatic_data_members(struct type_tag* class_type);
struct scope_entry_tag* class_type_get_nonstatic_data_member_num(struct type_tag* class_type, int i);

int class_type_get_num_static_data_members(struct type_tag* class_type);
struct scope_entry_tag* class_type_get_static_data_member_num(struct type_tag* class_type, int i);

int class_type_get_num_conversions(struct type_tag* t);
struct scope_entry_tag* class_type_get_conversion_num(struct type_tag* t, int num);

char class_type_get_is_dependent(struct type_tag* t);

// Gives all the conversions related to a class
struct scope_entry_list_tag* class_type_get_all_conversions(struct type_tag* class_type, 
        decl_context_t decl_context);

int class_type_get_num_copy_assignment_operators(struct type_tag* t);
struct scope_entry_tag* class_type_get_copy_assignment_operator_num(struct type_tag* t, int num);
int class_type_get_num_copy_constructors(struct type_tag* t);
struct scope_entry_tag* class_type_get_copy_constructor_num(struct type_tag* t, int num);

struct scope_entry_tag* class_type_get_destructor(struct type_tag* t);
decl_context_t class_type_get_context(struct type_tag* t);
void class_type_get_instantiation_trees(struct type_tag* t, struct AST_tag* *body, struct AST_tag* *base_clause);
decl_context_t class_type_get_inner_context(struct type_tag* class_type);

decl_context_t enum_type_get_context(struct type_tag* t);

struct scope_entry_tag* named_type_get_symbol(struct type_tag* t);

char pointer_types_are_similar(struct type_tag* t_orig, struct type_tag* t_dest, decl_context_t decl_context);

struct type_tag* template_type_get_primary_type(struct type_tag* t);
struct type_tag* template_type_get_specialized_type(struct type_tag* t, 
        template_argument_list_t* template_argument_list,
        template_parameter_list_t *template_parameters, 
        decl_context_t decl_context, 
        int line, const char* filename);
template_parameter_list_t* template_type_get_template_parameters(struct type_tag* t);

int template_type_get_num_specializations(struct type_tag* t);
struct type_tag* template_type_get_specialization_num(struct type_tag* t, int i);

int template_type_get_nesting_level(struct type_tag* t);

scope_entry_t* template_type_get_related_symbol(struct type_tag* t);

void template_type_update_template_parameters(struct type_tag* t, template_parameter_list_t*);

template_argument_list_t* template_specialized_type_get_template_arguments(struct type_tag* t);

struct type_tag* template_specialized_type_get_related_template_type(struct type_tag* t);

template_parameter_list_t* template_specialized_type_get_template_parameters(struct type_tag* t);
void template_specialized_type_update_template_parameters(struct type_tag* t, template_parameter_list_t* template_parameters);

void dependent_typename_get_components(struct type_tag* t, struct scope_entry_tag** dependent_entry, 
        struct AST_tag* *nested_name, struct AST_tag* *unqualified_part);

int vector_type_get_vector_size(struct type_tag*);
struct type_tag* vector_type_get_element_type(struct type_tag*);

/* Query functions: Miscelaneous stuff not classified otherwise */
char is_base_class_of(struct type_tag* possible_base, struct type_tag* possible_derived) DEPRECATED;
char class_type_is_base(struct type_tag* possible_base, struct type_tag* possible_derived);
char class_type_is_derived(struct type_tag* possible_base, struct type_tag* possible_derived);
// char class_type_is_directly_derived(struct type_tag* possible_base, struct type_tag* possible_derived);
// char class_type_is_indirectly_derived(struct type_tag* possible_base, struct type_tag* possible_derived);
char is_pointer_to_void_type(struct type_tag* t);
char is_pointer_to_function_type(struct type_tag* t1);

char pointer_to_class_type_is_base(struct type_tag* possible_pclass_base,
        struct type_tag* possible_pclass_derived);
char pointer_to_class_type_is_derived(struct type_tag* possible_pclass_derived,
        struct type_tag* possible_pclass_base);

struct type_tag* advance_over_typedefs(struct type_tag* t);
struct type_tag* advance_over_typedefs_with_cv_qualif(struct type_tag* t1, cv_qualifier_t* cv_qualif);

struct type_tag* reference_type_get_referenced_type(struct type_tag* t1);

struct type_tag* no_ref(struct type_tag* t);

struct type_tag* get_actual_class_type(struct type_tag* class_type);

char is_dependent_expression(struct AST_tag* expr, decl_context_t decl_context);

char is_pointer_to_member_type(struct type_tag* t);

char is_pointer_to_class_type(struct type_tag* t1);
char is_reference_to_class_type(struct type_tag* t1);
char is_typedef_type(struct type_tag* t1);

struct type_tag* typedef_type_get_aliased_type(struct type_tag* t);

struct scope_entry_tag* give_real_entry(struct scope_entry_tag* entry);

cv_qualifier_t* get_innermost_cv_qualifier(struct type_tag* t);

char class_type_is_incomplete_dependent(struct type_tag* t);
char class_type_is_complete_dependent(struct type_tag* t);
char class_type_is_incomplete_independent(struct type_tag* t);
char class_type_is_complete_independent(struct type_tag* t);

char pointer_types_can_be_converted(struct type_tag* orig, struct type_tag* dest, decl_context_t decl_context);

char vector_types_can_be_converted(struct type_tag* t1, struct type_tag* t2, decl_context_t decl_context);

void set_as_template_specialized_type(struct type_tag* type_to_specialize, 
        template_argument_list_t * template_arguments, 
        template_parameter_list_t* template_parameters,
        struct type_tag* template_type);

/* Naming types functions */
char* get_declaration_string_internal(struct type_tag* type_info, 
        decl_context_t decl_context,
        const char* symbol_name, 
        const char* initializer, 
        char semicolon,
        int *num_parameter_names,
        char ***parameter_names,
        char is_parameter);
char* get_simple_type_name_string(decl_context_t decl_context, struct type_tag* type_info);
char* get_named_type_name(struct scope_entry_tag* entry);

struct type_tag* get_ellipsis_type(void);
char is_ellipsis_type(struct type_tag* t);

/* Debug purpose functions */
char* print_declarator(struct type_tag* printed_declarator, decl_context_t decl_context);

char has_dependent_template_arguments(template_argument_list_t* template_arguments,
        decl_context_t decl_context);

char syntactic_comparison_of_nested_names(struct AST_tag* nested_name_1, struct AST_tag* nested_name_2, 
        struct AST_tag* unqualified_part_1, struct AST_tag* unqualified_part_2, decl_context_t decl_context);

MCXX_END_DECLS

#endif // CXX_TYPEUTILS_H
