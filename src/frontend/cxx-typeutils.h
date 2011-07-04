/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information 
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/



#ifndef CXX_TYPEUTILS_H
#define CXX_TYPEUTILS_H

#include "libmcxx-common.h"

#include "cxx-type-decls.h"
#include "cxx-ast-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-macros.h"
#include "cxx-solvetemplate.h"
#include "cxx-nodecl-output.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN char standard_conversion_is_identity(standard_conversion_t);
LIBMCXX_EXTERN char standard_conversion_is_invalid(standard_conversion_t);
LIBMCXX_EXTERN type_t* standard_conversion_get_orig_type(standard_conversion_t scs);
LIBMCXX_EXTERN type_t* standard_conversion_get_dest_type(standard_conversion_t scs);

LIBMCXX_EXTERN char standard_conversion_between_types(standard_conversion_t *result, 
        type_t* orig, type_t* dest);

typedef scope_entry_t* (*computed_function_type_t)(scope_entry_t* symbol, 
        type_t** argument_types, 
        AST *argument_expressions,
        int num_arguments);

// Get environmental information for the type
LIBMCXX_EXTERN char type_is_runtime_sized(type_t* t);
LIBMCXX_EXTERN _size_t type_get_size(type_t*);
LIBMCXX_EXTERN _size_t type_get_alignment(type_t*);

/* Type constructors: Builtins */
LIBMCXX_EXTERN type_t* get_char_type(void);
LIBMCXX_EXTERN type_t* get_signed_char_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_char_type(void);
LIBMCXX_EXTERN type_t* get_wchar_t_type(void);
LIBMCXX_EXTERN type_t* get_bool_type(void);
LIBMCXX_EXTERN type_t* get_signed_int_type(void);
LIBMCXX_EXTERN type_t* get_signed_short_int_type(void);
LIBMCXX_EXTERN type_t* get_signed_long_int_type(void);
LIBMCXX_EXTERN type_t* get_signed_long_long_int_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_int_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_short_int_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_int_type(void);
LIBMCXX_EXTERN type_t* get_size_t_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_long_int_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_long_long_int_type(void);
LIBMCXX_EXTERN type_t* get_float_type(void);
LIBMCXX_EXTERN type_t* get_void_type(void);
LIBMCXX_EXTERN type_t* get_double_type(void);
LIBMCXX_EXTERN type_t* get_long_double_type(void);

/* Fortran specialities */
LIBMCXX_EXTERN type_t* get_bool_of_integer_type(type_t* t);

LIBMCXX_EXTERN type_t* get_gcc_typeof_type(AST type_tree, decl_context_t decl_context);
LIBMCXX_EXTERN type_t* get_gcc_typeof_expr_type(AST type_expr, decl_context_t decl_context);

LIBMCXX_EXTERN type_t* get_gcc_builtin_va_list_type(void);

LIBMCXX_EXTERN type_t* get_user_defined_type(scope_entry_t* entry);
LIBMCXX_EXTERN type_t* get_indirect_type(scope_entry_t* entry);

LIBMCXX_EXTERN type_t* get_dependent_typename_type(scope_entry_t* dependent_entity, 
        decl_context_t decl_context,
        AST nested_name, AST unqualified_part);

LIBMCXX_EXTERN type_t* get_dependent_typename_type_from_parts(scope_entry_t* dependent_entity, 
        dependent_name_part_t* dependent_parts);

LIBMCXX_EXTERN type_t* get_new_enum_type(decl_context_t decl_context);
LIBMCXX_EXTERN type_t* get_new_class_type(decl_context_t decl_context, enum class_kind_t class_kind);

LIBMCXX_EXTERN type_t* get_new_template_type(template_parameter_list_t* template_parameter_list, type_t* primary_type,
        const char* template_name, decl_context_t decl_context, int line, const char* filename);

LIBMCXX_EXTERN type_t* get_complex_type(type_t* t);

LIBMCXX_EXTERN type_t* get_unresolved_overloaded_type(const scope_entry_list_t* overload_set,
        template_parameter_list_t* explicit_template_parameters);
LIBMCXX_EXTERN template_parameter_list_t* unresolved_overloaded_type_get_explicit_template_arguments(type_t* t);

LIBMCXX_EXTERN scope_entry_t* unresolved_overloaded_type_simplify(type_t* t, 
        decl_context_t decl_context, int line, const char* filename);

LIBMCXX_EXTERN type_t* get_dependent_expr_type(void);

LIBMCXX_EXTERN type_t* canonical_type(type_t* type);

// States that this is a variably modified type
LIBMCXX_EXTERN char is_variably_modified_type(type_t* t);

// This is a plain '0'
LIBMCXX_EXTERN type_t* get_zero_type(void);
// This is for g++'s '__null'
LIBMCXX_EXTERN type_t* get_null_type(void);

// Error type
LIBMCXX_EXTERN type_t* get_error_type(void);

LIBMCXX_EXTERN type_t* get_pseudo_destructor_call_type(void);

LIBMCXX_EXTERN type_t* get_literal_string_type(int length, char is_wchar);

LIBMCXX_EXTERN type_t* get_throw_expr_type(void);

/* Type constructors: cv-qualification */
// The given cv_qualifier is strictly the one will have the returning type
LIBMCXX_EXTERN type_t* get_cv_qualified_type(type_t* t, cv_qualifier_t cv_qualifier);

// These add 'const', 'volatile' or 'restrict' to the current qualification of t
LIBMCXX_EXTERN type_t* get_const_qualified_type(type_t* t);
LIBMCXX_EXTERN type_t* get_volatile_qualified_type(type_t* t);
LIBMCXX_EXTERN type_t* get_restrict_qualified_type(type_t* t);

/* Type constructors: derived types */
LIBMCXX_EXTERN type_t* get_pointer_type(type_t*);

LIBMCXX_EXTERN type_t* get_pointer_to_member_type(type_t*, scope_entry_t* class_entry);

LIBMCXX_EXTERN type_t* get_lvalue_reference_type(type_t* t);
LIBMCXX_EXTERN type_t* get_rvalue_reference_type(type_t* t);

LIBMCXX_EXTERN type_t* get_array_type(type_t* element_type, 
        nodecl_t size_of_array, 
        decl_context_t decl_context);

LIBMCXX_EXTERN type_t* get_array_type_bounds(type_t*, 
        nodecl_t lower_bound, 
        nodecl_t upper_bound, 
        decl_context_t decl_context);

LIBMCXX_EXTERN type_t* get_array_type_str(type_t*, const char* dim);

LIBMCXX_EXTERN type_t* get_new_function_type(type_t* t, parameter_info_t* parameter_info, int num_parameters);
LIBMCXX_EXTERN type_t* get_nonproto_function_type(type_t* t, int num_parameters);

LIBMCXX_EXTERN type_t* get_vector_type(type_t* element_type, unsigned int vector_size);

LIBMCXX_EXTERN type_t* get_generic_vector_type(struct type_tag* element_type);

LIBMCXX_EXTERN type_t* get_computed_function_type(computed_function_type_t compute_type_function);
LIBMCXX_EXTERN type_t* get_computed_function_type(computed_function_type_t compute_type_function);

/* Type comparison functions */
LIBMCXX_EXTERN char equivalent_types(type_t* t1, type_t* t2);
LIBMCXX_EXTERN char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2);

/* Modifiers used when the type is still being built */

LIBMCXX_EXTERN void class_type_add_base_class(type_t* class_type, 
        scope_entry_t* base_class, 
        char is_virtual, 
        char is_dependent,
        access_specifier_t access_spec);
LIBMCXX_EXTERN void class_type_set_inner_context(type_t* class_type, decl_context_t decl_context);
LIBMCXX_EXTERN void class_type_add_constructor(type_t* class_type, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_set_destructor(type_t* class_type, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_add_copy_assignment_operator(type_t* class_type, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_add_move_assignment_operator(type_t* class_type, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_add_copy_constructor(type_t* class_type, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_add_move_constructor(type_t* class_type, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_add_conversion_function(type_t* class_type, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_add_nonstatic_data_member(type_t* class_type, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_add_static_data_member(type_t* class_type, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_set_instantiation_trees(type_t* t, AST body, AST base_clause);
LIBMCXX_EXTERN void class_type_set_default_constructor(type_t* t, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_add_member_function(type_t* t, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_set_enclosing_class_type(type_t* t, type_t* class_type);

LIBMCXX_EXTERN void class_type_add_friend_symbol(type_t* t, scope_entry_t* entry);

LIBMCXX_EXTERN void class_type_add_typename(type_t* t, scope_entry_t* class_type);

LIBMCXX_EXTERN void class_type_add_member(type_t* t, scope_entry_t* member);

LIBMCXX_EXTERN void enum_type_add_enumerator(type_t* t, scope_entry_t* entry);
LIBMCXX_EXTERN void enum_type_set_underlying_type(type_t* t, type_t* underlying_type);

LIBMCXX_EXTERN void set_is_incomplete_type(type_t* t, char is_incomplete);
LIBMCXX_EXTERN void set_is_complete_type(type_t* t, char is_complete);
LIBMCXX_EXTERN void set_is_dependent_type(type_t* t, char is_dependent);

LIBMCXX_EXTERN void template_type_set_related_symbol(type_t* t, scope_entry_t*);

/* Query functions: is-a-kind-of-type functions */
LIBMCXX_EXTERN char is_builtin_type(type_t* t);
LIBMCXX_EXTERN char is_fundamental_type(type_t* t);

LIBMCXX_EXTERN char is_pod_type(type_t* t);

LIBMCXX_EXTERN char is_trivially_copiable_type(type_t* t);
LIBMCXX_EXTERN char is_standard_layout_type(type_t* t);

// States whether a type is faulty
LIBMCXX_EXTERN char is_faulty_type(type_t*);

// Any type of 'int' nature regardless of being signed or not 
// (int, short, long, long long)
LIBMCXX_EXTERN char is_any_int_type(type_t* t);
// Like the previous but only for unsigned
LIBMCXX_EXTERN char is_any_unsigned_int_type(type_t* t);
// Like the previous but only for signed
LIBMCXX_EXTERN char is_any_signed_int_type(type_t* t);

// Returns the type on which this complex is based
LIBMCXX_EXTERN type_t* complex_type_get_base_type(type_t* t);

// char, wchar_t, bool and any integer
LIBMCXX_EXTERN char is_integral_type(type_t* t); 
LIBMCXX_EXTERN char is_signed_integral_type(type_t* t);
// A synonim in the standard
LIBMCXX_EXTERN char is_integer_type(type_t* t); 
LIBMCXX_EXTERN char is_enum_type(type_t* t);
LIBMCXX_EXTERN char is_named_enumerated_type(type_t* t);
LIBMCXX_EXTERN char is_unnamed_enumerated_type(type_t* t);

LIBMCXX_EXTERN char is_signed_int_type(type_t *t);
LIBMCXX_EXTERN char is_unsigned_int_type(type_t *t);
LIBMCXX_EXTERN char is_signed_short_int_type(type_t *t);
LIBMCXX_EXTERN char is_unsigned_short_int_type(type_t *t);
LIBMCXX_EXTERN char is_signed_long_int_type(type_t *t);
LIBMCXX_EXTERN char is_unsigned_long_int_type(type_t *t);
LIBMCXX_EXTERN char is_signed_long_long_int_type(type_t *t);
LIBMCXX_EXTERN char is_unsigned_long_long_int_type(type_t *t);

LIBMCXX_EXTERN char is_character_type(type_t* t);
LIBMCXX_EXTERN char is_char_type(type_t* t);
LIBMCXX_EXTERN char is_signed_char_type(type_t* t);
LIBMCXX_EXTERN char is_unsigned_char_type(type_t* t);

LIBMCXX_EXTERN char is_wchar_t_type(type_t* t);

LIBMCXX_EXTERN char is_floating_type(type_t* t);
LIBMCXX_EXTERN char is_double_type(type_t* t);
LIBMCXX_EXTERN char is_long_double_type(type_t* t);
LIBMCXX_EXTERN char is_float_type(type_t* t);

// Either floating type or integral type (note that integral types include
// char, wchar_t, bool and all sorts of 'int')
LIBMCXX_EXTERN char is_arithmetic_type(type_t* t);

// Either floating or any int (this is like 'is_arithmetic_type' but
// excluding char, wchar_t and bool)
LIBMCXX_EXTERN char is_int_or_floating_type(type_t* t);

LIBMCXX_EXTERN char is_pointer_type(type_t* t1);

LIBMCXX_EXTERN char is_array_type(type_t* t1);

LIBMCXX_EXTERN char is_function_type(type_t* t);

LIBMCXX_EXTERN char is_lvalue_reference_type(type_t* t1);
LIBMCXX_EXTERN char is_rvalue_reference_type(type_t* t1);

LIBMCXX_EXTERN char is_vector_type(type_t* t);

LIBMCXX_EXTERN char is_generic_vector_type(type_t* t);

LIBMCXX_EXTERN char is_class_type(type_t* possible_class);
LIBMCXX_EXTERN char is_unnamed_class_type(type_t* possible_class);
LIBMCXX_EXTERN char is_named_class_type(type_t* possible_class);

LIBMCXX_EXTERN char is_union_type(type_t* possible_union);

LIBMCXX_EXTERN char is_named_type(type_t* t);

LIBMCXX_EXTERN char is_void_type(type_t* t);
LIBMCXX_EXTERN char is_void_pointer_type(type_t* t1);

LIBMCXX_EXTERN char is_gcc_builtin_va_list(type_t *t);

LIBMCXX_EXTERN char is_bool_type(type_t* t1);

LIBMCXX_EXTERN char is_non_derived_type(type_t* t);

LIBMCXX_EXTERN char is_dependent_type(type_t* type);

LIBMCXX_EXTERN char is_aggregate_type(type_t* t);

LIBMCXX_EXTERN char is_dependent_typename_type(type_t* t);

LIBMCXX_EXTERN char is_complex_type(type_t* t);

LIBMCXX_EXTERN char is_unresolved_overloaded_type(type_t* t);
LIBMCXX_EXTERN char is_dependent_expr_type(type_t* t);

LIBMCXX_EXTERN char is_zero_type(type_t* t);

LIBMCXX_EXTERN char is_error_type(type_t* t);

LIBMCXX_EXTERN char is_throw_expr_type(type_t* t);

LIBMCXX_EXTERN char is_pseudo_destructor_call_type(type_t *t);

LIBMCXX_EXTERN char is_literal_string_type(type_t* t);

LIBMCXX_EXTERN char is_template_type(type_t* t);

LIBMCXX_EXTERN char is_scalar_type(type_t* t);

LIBMCXX_EXTERN char is_incomplete_type(type_t* t);
LIBMCXX_EXTERN char is_complete_type(type_t* t);

// A type returned by template_type_get_primary_type or template_type_get_specialized_type
LIBMCXX_EXTERN char is_template_specialized_type(type_t* t);

/* Query functions: cv-qualification */
LIBMCXX_EXTERN type_t* get_unqualified_type(type_t* t);
LIBMCXX_EXTERN cv_qualifier_t get_cv_qualifier(type_t* type_info);

LIBMCXX_EXTERN char is_less_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2);
LIBMCXX_EXTERN char is_equal_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2);
LIBMCXX_EXTERN char is_less_or_equal_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2);
LIBMCXX_EXTERN char is_more_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2);
LIBMCXX_EXTERN char is_more_or_equal_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2);

LIBMCXX_EXTERN char is_less_cv_qualified_type(type_t* t1, type_t* t2);
LIBMCXX_EXTERN char is_equally_cv_qualified_type(type_t* t1, type_t* t2);
LIBMCXX_EXTERN char is_less_or_equal_cv_qualified_type(type_t* t1, type_t* t2);
LIBMCXX_EXTERN char is_more_cv_qualified_type(type_t* t1, type_t* t2);
LIBMCXX_EXTERN char is_more_or_equal_cv_qualified_type(type_t* t1, type_t* t2);

LIBMCXX_EXTERN char is_const_qualified_type(type_t* t1);
LIBMCXX_EXTERN char is_volatile_qualified_type(type_t* t1);
LIBMCXX_EXTERN char is_restrict_qualified_type(type_t* t1);

LIBMCXX_EXTERN char is_const_qualified(cv_qualifier_t cv);
LIBMCXX_EXTERN char is_volatile_qualified(cv_qualifier_t cv);
LIBMCXX_EXTERN char is_restrict_qualified(cv_qualifier_t cv);

LIBMCXX_EXTERN char is_computed_function_type(type_t* t);

LIBMCXX_EXTERN int get_sizeof_type(type_t* t);

/* Query functions: specific ones */
LIBMCXX_EXTERN int function_type_get_num_parameters(type_t* function_type);
LIBMCXX_EXTERN type_t* function_type_get_parameter_type_num(type_t* function_type, int num_param);
LIBMCXX_EXTERN type_t* function_type_get_nonadjusted_parameter_type_num(type_t* function_type, int num_param);
LIBMCXX_EXTERN char function_type_get_lacking_prototype(type_t* function_type);
LIBMCXX_EXTERN char function_type_get_has_ellipsis(type_t* function_type);
LIBMCXX_EXTERN type_t* function_type_get_return_type(type_t* t);

LIBMCXX_EXTERN char function_type_can_override(type_t* potential_overrider, type_t* function_type);

LIBMCXX_EXTERN type_t* pointer_type_get_pointee_type(type_t *t);
LIBMCXX_EXTERN scope_entry_t* pointer_to_member_type_get_class(type_t *t);
LIBMCXX_EXTERN type_t* pointer_to_member_type_get_class_type(type_t *t);

LIBMCXX_EXTERN scope_entry_list_t *unresolved_overloaded_type_get_overload_set(type_t* t);

LIBMCXX_EXTERN type_t* array_type_get_element_type(type_t* t);
LIBMCXX_EXTERN nodecl_t array_type_get_array_size_expr(type_t* t);
LIBMCXX_EXTERN decl_context_t array_type_get_array_size_expr_context(type_t* t);


LIBMCXX_EXTERN char array_type_is_unknown_size(type_t* t);
LIBMCXX_EXTERN nodecl_t array_type_get_array_lower_bound(type_t* t);
LIBMCXX_EXTERN nodecl_t array_type_get_array_upper_bound(type_t* t);

LIBMCXX_EXTERN char array_type_is_vla(type_t* t);

LIBMCXX_EXTERN int enum_type_get_num_enumerators(type_t* t);
LIBMCXX_EXTERN scope_entry_t* enum_type_get_enumerator_num(type_t* t, int n);
LIBMCXX_EXTERN type_t* enum_type_get_underlying_type(type_t* t);

LIBMCXX_EXTERN enum class_kind_t class_type_get_class_kind(type_t* t);
LIBMCXX_EXTERN int class_type_get_num_bases(type_t* class_type);
LIBMCXX_EXTERN scope_entry_t* class_type_get_base_num(type_t* class_type, int num, 
        char *is_virtual, 
        char *is_dependent,
        access_specifier_t *access_specifier);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_all_bases(type_t *t, char include_dependent);
LIBMCXX_EXTERN int class_type_get_num_constructors(type_t* t);
LIBMCXX_EXTERN scope_entry_t* class_type_get_constructors_num(type_t* t, int num);

LIBMCXX_EXTERN type_t* class_type_get_enclosing_class_type(type_t* t);

LIBMCXX_EXTERN int class_type_get_num_nonstatic_data_members(type_t* class_type);
LIBMCXX_EXTERN scope_entry_t* class_type_get_nonstatic_data_member_num(type_t* class_type, int i);

LIBMCXX_EXTERN int class_type_get_num_static_data_members(type_t* class_type);
LIBMCXX_EXTERN scope_entry_t* class_type_get_static_data_member_num(type_t* class_type, int i);

LIBMCXX_EXTERN int class_type_get_num_member_functions(type_t* class_type);
LIBMCXX_EXTERN scope_entry_t* class_type_get_member_function_num(type_t* class_type, int i);

LIBMCXX_EXTERN int class_type_get_num_conversions(type_t* t);
LIBMCXX_EXTERN scope_entry_t* class_type_get_conversion_num(type_t* t, int num);

LIBMCXX_EXTERN int class_type_get_num_typenames(type_t* t);
LIBMCXX_EXTERN scope_entry_t* class_type_get_typename_num(type_t* t, int num);

LIBMCXX_EXTERN int class_type_get_num_members(type_t* t);
LIBMCXX_EXTERN scope_entry_t* class_type_get_member_num(type_t* t, int num);

LIBMCXX_EXTERN computed_function_type_t computed_function_type_get_computing_function(type_t* t);

// Gives all the conversions related to a class
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_all_conversions(type_t* class_type, 
        decl_context_t decl_context);

LIBMCXX_EXTERN int class_type_get_num_copy_assignment_operators(type_t* t);
LIBMCXX_EXTERN scope_entry_t* class_type_get_copy_assignment_operator_num(type_t* t, int num);
LIBMCXX_EXTERN int class_type_get_num_move_assignment_operators(type_t* t);
LIBMCXX_EXTERN scope_entry_t* class_type_get_move_assignment_operator_num(type_t* t, int num);
LIBMCXX_EXTERN int class_type_get_num_copy_constructors(type_t* t);
LIBMCXX_EXTERN scope_entry_t* class_type_get_copy_constructor_num(type_t* t, int num);
LIBMCXX_EXTERN int class_type_get_num_move_constructors(type_t* t);
LIBMCXX_EXTERN scope_entry_t* class_type_get_move_constructor_num(type_t* t, int num);
LIBMCXX_EXTERN scope_entry_t* class_type_get_default_constructor(type_t* t);

LIBMCXX_EXTERN scope_entry_t* class_type_get_destructor(type_t* t);
LIBMCXX_EXTERN decl_context_t class_type_get_context(type_t* t);
LIBMCXX_EXTERN void class_type_get_instantiation_trees(type_t* t, AST *body, AST *base_clause);
LIBMCXX_EXTERN decl_context_t class_type_get_inner_context(type_t* class_type);

LIBMCXX_EXTERN void class_type_add_virtual_function(type_t* class_type, scope_entry_t* entry);
LIBMCXX_EXTERN scope_entry_t* class_type_get_virtual_function_num(type_t* class_type, int i);
LIBMCXX_EXTERN int class_type_get_num_virtual_functions(type_t* class_type);

LIBMCXX_EXTERN int class_type_get_num_friends(type_t* class_type);
LIBMCXX_EXTERN scope_entry_t* class_type_get_friend_num(type_t* class_type, int);

LIBMCXX_EXTERN decl_context_t enum_type_get_context(type_t* t);

LIBMCXX_EXTERN scope_entry_t* named_type_get_symbol(type_t* t);

LIBMCXX_EXTERN char pointer_types_are_similar(type_t* t_orig, type_t* t_dest);

LIBMCXX_EXTERN type_t* template_type_get_primary_type(type_t* t);
LIBMCXX_EXTERN type_t* template_type_get_matching_specialized_type(type_t* t,
        template_parameter_list_t* template_parameter_list,
        decl_context_t decl_context);
LIBMCXX_EXTERN type_t* template_type_get_specialized_type(type_t* t, 
        template_parameter_list_t * template_parameters,
        decl_context_t decl_context, 
        int line, const char* filename);
LIBMCXX_EXTERN type_t* template_type_get_specialized_type_after_type(type_t* t, 
        template_parameter_list_t *template_parameters, 
        type_t* after_type,
        decl_context_t decl_context, 
        int line, const char* filename);
LIBMCXX_EXTERN template_parameter_list_t* template_type_get_template_parameters(type_t* t);

LIBMCXX_EXTERN int template_type_get_num_specializations(type_t* t);
LIBMCXX_EXTERN type_t* template_type_get_specialization_num(type_t* t, int i);

LIBMCXX_EXTERN int template_type_get_nesting_level(type_t* t);

LIBMCXX_EXTERN scope_entry_t* template_type_get_related_symbol(type_t* t);

LIBMCXX_EXTERN void template_type_update_template_parameters(type_t* t, template_parameter_list_t*);

LIBMCXX_EXTERN template_parameter_list_t* template_specialized_type_get_template_parameters(type_t* t);
LIBMCXX_EXTERN template_parameter_list_t* template_specialized_type_get_template_arguments(type_t* t);

LIBMCXX_EXTERN type_t* template_specialized_type_get_related_template_type(type_t* t);

LIBMCXX_EXTERN void template_specialized_type_update_template_parameters(type_t* t, template_parameter_list_t* template_parameters);

LIBMCXX_EXTERN void dependent_typename_get_components(type_t* t, 
        scope_entry_t** dependent_entry, 
        dependent_name_part_t** dependent_parts);

LIBMCXX_EXTERN int vector_type_get_vector_size(type_t*);
LIBMCXX_EXTERN type_t* vector_type_get_element_type(type_t*);

LIBMCXX_EXTERN type_t* braced_list_type_get_type_num(type_t* t, int num);
LIBMCXX_EXTERN int braced_list_type_get_num_types(type_t* t);
LIBMCXX_EXTERN type_t** braced_list_type_get_types(type_t* t);

/* Query functions: Miscelaneous stuff not classified otherwise */
LIBMCXX_EXTERN char class_type_is_trivial(type_t* t);
LIBMCXX_EXTERN char class_type_is_trivially_copiable(type_t* t);
LIBMCXX_EXTERN char class_type_is_standard_layout(type_t* t);

LIBMCXX_EXTERN char class_type_is_base(type_t* possible_base, type_t* possible_derived);
LIBMCXX_EXTERN char class_type_is_derived(type_t* possible_derived, type_t* possible_base);

LIBMCXX_EXTERN char is_pointer_to_void_type(type_t* t);
LIBMCXX_EXTERN char is_pointer_to_function_type(type_t* t1);

LIBMCXX_EXTERN char pointer_to_class_type_is_base(type_t* possible_pclass_base,
        type_t* possible_pclass_derived);
LIBMCXX_EXTERN char pointer_to_class_type_is_derived(type_t* possible_pclass_derived,
        type_t* possible_pclass_base);

LIBMCXX_EXTERN char class_type_is_empty(type_t* t);
LIBMCXX_EXTERN char class_type_is_nearly_empty(type_t* t);
LIBMCXX_EXTERN char class_type_is_dynamic(type_t* t);

LIBMCXX_EXTERN char class_type_is_abstract(type_t* class_type);
LIBMCXX_EXTERN void class_type_set_is_abstract(type_t* class_type, char is_abstract);

LIBMCXX_EXTERN type_t* advance_over_typedefs(type_t* t);
LIBMCXX_EXTERN type_t* advance_over_typedefs_with_cv_qualif(type_t* t1, cv_qualifier_t* cv_qualif);

LIBMCXX_EXTERN type_t* reference_type_get_referenced_type(type_t* t1);

LIBMCXX_EXTERN type_t* no_ref(type_t* t);

LIBMCXX_EXTERN type_t* get_actual_class_type(type_t* class_type);

LIBMCXX_EXTERN type_t* get_actual_enum_type(type_t* enum_type);

LIBMCXX_EXTERN char is_pointer_to_member_type(type_t* t);

LIBMCXX_EXTERN char is_pointer_to_class_type(type_t* t1);
LIBMCXX_EXTERN char is_lvalue_reference_to_class_type(type_t* t1);
LIBMCXX_EXTERN char is_rvalue_reference_to_class_type(type_t* t1);
LIBMCXX_EXTERN char is_reference_to_class_type(type_t* t1);

LIBMCXX_EXTERN char class_type_is_incomplete_dependent(type_t* t);
LIBMCXX_EXTERN char class_type_is_complete_dependent(type_t* t);
LIBMCXX_EXTERN char class_type_is_incomplete_independent(type_t* t);
LIBMCXX_EXTERN char class_type_is_complete_independent(type_t* t);

LIBMCXX_EXTERN char pointer_types_can_be_converted(type_t* orig, type_t* dest);

LIBMCXX_EXTERN void set_as_template_specialized_type(type_t* type_to_specialize, 
        template_parameter_list_t * template_parameters, 
        type_t* template_type);

LIBMCXX_EXTERN type_t* get_foundation_type(type_t* t);

/* Naming types functions */
LIBMCXX_EXTERN const char* get_declaration_string_internal(type_t* type_info, 
        decl_context_t decl_context,
        const char* symbol_name, 
        const char* initializer, 
        char semicolon,
        int num_parameter_names,
        const char **parameter_names,
        char is_parameter);
LIBMCXX_EXTERN const char* get_simple_type_name_string(decl_context_t decl_context, type_t* type_info);
LIBMCXX_EXTERN const char* get_named_type_name(scope_entry_t* entry);

LIBMCXX_EXTERN type_t* get_ellipsis_type(void);
LIBMCXX_EXTERN char is_ellipsis_type(type_t* t);

LIBMCXX_EXTERN type_t* get_braced_list_type(int num_types, type_t** arg_list);
LIBMCXX_EXTERN char is_braced_list_type(type_t* t);

LIBMCXX_EXTERN char has_dependent_template_parameters(template_parameter_list_t* template_parameters);

LIBMCXX_EXTERN char syntactic_comparison_of_nested_names(
        dependent_name_part_t* dependent_parts_1,
        dependent_name_part_t* dependent_parts_2);

LIBMCXX_EXTERN dependent_name_part_t* copy_dependent_parts(
        dependent_name_part_t* dependent_part);

/* Debug purpose functions */
LIBMCXX_EXTERN const char* print_declarator(type_t* printed_declarator);
LIBMCXX_EXTERN long long unsigned int type_system_used_memory(void);

/* Only for type environment routines in cxx-typeenviron.c. 
   Do not use them anywhere else */
LIBMCXX_EXTERN void type_set_size(type_t* t, _size_t size);
LIBMCXX_EXTERN void type_set_alignment(type_t* t, _size_t alignment);
LIBMCXX_EXTERN void type_set_valid_size(type_t* t, char valid);

LIBMCXX_EXTERN _size_t type_get_data_size(type_t* t);
LIBMCXX_EXTERN void type_set_data_size(type_t* t, _size_t data_size);

LIBMCXX_EXTERN _size_t class_type_get_non_virtual_size(type_t* t);
LIBMCXX_EXTERN void class_type_set_non_virtual_size(type_t* t, _size_t non_virtual_size);

LIBMCXX_EXTERN _size_t class_type_get_non_virtual_align(type_t* t);
LIBMCXX_EXTERN void class_type_set_non_virtual_align(type_t* t, _size_t non_virtual_align);

LIBMCXX_EXTERN _size_t class_type_get_offset_direct_base(type_t* class_type, scope_entry_t* direct_base);
LIBMCXX_EXTERN void class_type_set_offset_direct_base(type_t* class_type, scope_entry_t* direct_base, _size_t base_offset);

LIBMCXX_EXTERN _size_t class_type_get_offset_virtual_base(type_t* class_type, scope_entry_t* virtual_base);
LIBMCXX_EXTERN void class_type_set_offset_virtual_base(type_t* class_type, scope_entry_t* virtual_base, _size_t offset);

LIBMCXX_EXTERN int class_type_get_num_virtual_bases_with_offset(type_t* t);
LIBMCXX_EXTERN void class_type_get_virtual_base_with_offset_num(type_t* t, int num, 
        scope_entry_t** symbol, 
        _size_t* offset);

LIBMCXX_EXTERN unsigned int get_array_type_counter(void);
LIBMCXX_EXTERN unsigned int get_class_type_counter(void);
LIBMCXX_EXTERN unsigned int get_function_type_counter(void);
LIBMCXX_EXTERN unsigned int get_function_type_reused(void);
LIBMCXX_EXTERN unsigned int get_pointer_type_counter(void);
LIBMCXX_EXTERN unsigned int get_pointer_to_member_type_counter(void);
LIBMCXX_EXTERN unsigned int get_qualified_type_counter(void);
LIBMCXX_EXTERN unsigned int get_reference_type_counter(void);
LIBMCXX_EXTERN unsigned int get_vector_type_counter(void);
LIBMCXX_EXTERN unsigned int get_enum_type_counter(void);
LIBMCXX_EXTERN unsigned int get_template_type_counter(void);
LIBMCXX_EXTERN size_t get_type_t_size(void);

LIBMCXX_EXTERN const char* print_decl_type_str(type_t* t, decl_context_t decl_context, const char* name);
LIBMCXX_EXTERN const char* print_type_str(type_t* t, decl_context_t decl_context);

MCXX_END_DECLS

#endif // CXX_TYPEUTILS_H
