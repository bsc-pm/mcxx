/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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
#include "cxx-cexpr-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-macros.h"
#include "cxx-solvetemplate.h"
#include "cxx-nodecl-output.h"
#include "cxx-locus.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN standard_conversion_t get_identity_scs(type_t* t_orig, type_t* t_dest);
LIBMCXX_EXTERN standard_conversion_t get_invalid_scs(void);
LIBMCXX_EXTERN char standard_conversion_is_identity(standard_conversion_t);
LIBMCXX_EXTERN char standard_conversion_is_invalid(standard_conversion_t);
LIBMCXX_EXTERN type_t* standard_conversion_get_orig_type(standard_conversion_t scs);
LIBMCXX_EXTERN type_t* standard_conversion_get_dest_type(standard_conversion_t scs);

LIBMCXX_EXTERN char standard_conversion_between_types(standard_conversion_t *result, 
        type_t* orig, type_t* dest, const locus_t* locus);

// Get environmental information for the type
LIBMCXX_EXTERN char type_is_runtime_sized(type_t* t);
LIBMCXX_EXTERN _size_t type_get_size(type_t*);
LIBMCXX_EXTERN _size_t type_get_alignment(type_t*);
LIBMCXX_EXTERN char type_depends_on_nonconstant_values(type_t* t);

/* Type constructors: Builtins */
LIBMCXX_EXTERN type_t* get_signed_byte_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_byte_type(void);

LIBMCXX_EXTERN type_t* get_char_type(void);
LIBMCXX_EXTERN type_t* get_signed_char_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_char_type(void);
LIBMCXX_EXTERN type_t* get_wchar_t_type(void);
LIBMCXX_EXTERN type_t* get_char16_t_type(void);
LIBMCXX_EXTERN type_t* get_char32_t_type(void);
LIBMCXX_EXTERN type_t* get_bool_type(void);
LIBMCXX_EXTERN type_t* get_signed_int_type(void);
LIBMCXX_EXTERN type_t* get_signed_short_int_type(void);
LIBMCXX_EXTERN type_t* get_signed_long_int_type(void);
LIBMCXX_EXTERN type_t* get_signed_long_long_int_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_int_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_short_int_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_int_type(void);
LIBMCXX_EXTERN type_t* get_size_t_type(void);
LIBMCXX_EXTERN type_t* get_ptrdiff_t_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_long_int_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_long_long_int_type(void);
LIBMCXX_EXTERN type_t* get_floating_type_from_descriptor(floating_type_info_t* info);
LIBMCXX_EXTERN type_t* get_float_type(void);
LIBMCXX_EXTERN type_t* get_void_type(void);
LIBMCXX_EXTERN type_t* get_double_type(void);
LIBMCXX_EXTERN type_t* get_long_double_type(void);

// __int128
LIBMCXX_EXTERN type_t* get_signed_int128_type(void);
LIBMCXX_EXTERN type_t* get_unsigned_int128_type(void);

// __float128
LIBMCXX_EXTERN type_t* get_float128_type(void);

// "short" float / half float
LIBMCXX_EXTERN type_t* get_float16_type(void);

LIBMCXX_EXTERN type_t* get_unknown_dependent_type(void);

/* Fortran specialities */
LIBMCXX_EXTERN type_t* get_bool_of_integer_type(type_t* t);
LIBMCXX_EXTERN type_t* get_hollerith_type(void);
LIBMCXX_EXTERN char is_hollerith_type(type_t* t);

LIBMCXX_EXTERN type_t* get_typeof_expr_dependent_type(nodecl_t nodecl_expr,
        const decl_context_t* decl_context,
        char is_decltype);

LIBMCXX_EXTERN nodecl_t typeof_expr_type_get_expression(type_t* t);
LIBMCXX_EXTERN const decl_context_t* typeof_expr_type_get_expression_context(type_t* t);

LIBMCXX_EXTERN char typeof_expr_type_is_decltype(type_t* t);

LIBMCXX_EXTERN char is_typeof_expr(type_t* t);

LIBMCXX_EXTERN type_t* get_gcc_builtin_va_list_type(void);

LIBMCXX_EXTERN char is_gxx_underlying_type(type_t* t);
LIBMCXX_EXTERN type_t* get_gxx_underlying_type(type_t* t);
LIBMCXX_EXTERN type_t* gxx_underlying_type_get_underlying_type(type_t* t);

LIBMCXX_EXTERN type_t* get_user_defined_type(scope_entry_t* entry);
LIBMCXX_EXTERN type_t* get_immutable_indirect_type(scope_entry_t* entry);
LIBMCXX_EXTERN type_t* get_mutable_indirect_type(scope_entry_t* entry);

LIBMCXX_EXTERN type_t* get_dependent_typename_type_from_parts(scope_entry_t* dependent_entity, 
        nodecl_t dependent_parts);
LIBMCXX_EXTERN char is_valid_symbol_for_dependent_typename(scope_entry_t* entry);
LIBMCXX_EXTERN enum type_tag_t get_dependent_entry_kind(type_t* t);
LIBMCXX_EXTERN type_t* set_dependent_entry_kind(type_t* t, enum type_tag_t kind);

LIBMCXX_EXTERN char is_transparent_union(type_t* t);
LIBMCXX_EXTERN void set_is_transparent_union(type_t* t, char is_transparent_union);
#if 0
LIBMCXX_EXTERN void dependent_typename_set_is_artificial(type_t* t, char is_artificial);
LIBMCXX_EXTERN char dependent_typename_is_artificial(type_t* t);
#endif

LIBMCXX_EXTERN type_t* get_new_enum_type(const decl_context_t* decl_context, char is_scoped);
LIBMCXX_EXTERN type_t* get_new_class_type(const decl_context_t* decl_context, enum type_tag_t class_kind);

LIBMCXX_EXTERN type_t* get_new_template_type(template_parameter_list_t* template_parameter_list, type_t* primary_type,
        const char* template_name, const decl_context_t* decl_context, const locus_t* locus);

LIBMCXX_EXTERN type_t* get_new_template_alias_type(template_parameter_list_t* template_parameter_list, type_t* primary_type,
        const char* template_name, const decl_context_t* decl_context, const locus_t* locus);

LIBMCXX_EXTERN type_t* get_complex_type(type_t* t);

LIBMCXX_EXTERN type_t* get_unresolved_overloaded_type(const scope_entry_list_t* overload_set,
        template_parameter_list_t* explicit_template_arguments);
LIBMCXX_EXTERN template_parameter_list_t* unresolved_overloaded_type_get_explicit_template_arguments(type_t* t);

LIBMCXX_EXTERN template_parameter_list_t* compute_template_parameter_values_of_primary(template_parameter_list_t* template_parameter_list);

LIBMCXX_EXTERN scope_entry_t* unresolved_overloaded_type_simplify(type_t* t, 
        const decl_context_t* decl_context, const locus_t* locus);
LIBMCXX_EXTERN scope_entry_t* unresolved_overloaded_type_simplify_unpacked(
        scope_entry_list_t* overload_set,
        template_parameter_list_t* explicit_template_arguments,
        const decl_context_t* decl_context,
        const locus_t* locus);

LIBMCXX_EXTERN type_t* deduce_auto_initializer(
        nodecl_t nodecl_initializer,
        type_t* type_to_deduce,
        const decl_context_t* decl_context);
LIBMCXX_EXTERN type_t* deduce_decltype_auto_initializer(
        nodecl_t nodecl_initializer,
        type_t* type_to_deduce,
        const decl_context_t* decl_context);
LIBMCXX_EXTERN type_t* compute_type_of_decltype_nodecl(
        nodecl_t nodecl_expr,
        const decl_context_t* decl_context);

LIBMCXX_EXTERN type_t* canonical_type(type_t* type);

// States that this is a variably modified type
LIBMCXX_EXTERN char is_variably_modified_type(type_t* t);

// This is a type for a bool 'false'
LIBMCXX_EXTERN type_t* get_bool_false_type(void);

// States that this type is based on another but some attributes or properties
// have been changed
LIBMCXX_EXTERN char is_variant_type(type_t* t);
// Returns the type from which 't' was created
LIBMCXX_EXTERN type_t* variant_type_get_nonvariant(type_t* t);

// This is a zero type based on any integer/boolean type
LIBMCXX_EXTERN type_t* get_zero_type(type_t* t); // A synonim of get_variant_type_zero
LIBMCXX_EXTERN type_t* get_variant_type_zero(type_t* t);

// nullptr_t
LIBMCXX_EXTERN type_t* get_nullptr_type(void);

// Error type
LIBMCXX_EXTERN type_t* get_error_type(void);

LIBMCXX_EXTERN type_t* get_pseudo_destructor_call_type(void);

LIBMCXX_EXTERN type_t* get_literal_string_type(int length, type_t* base_type);

LIBMCXX_EXTERN type_t* get_throw_expr_type(void);

LIBMCXX_EXTERN type_t* get_implicit_none_type(void);

// Used for parameter packs
LIBMCXX_EXTERN type_t* get_pack_type(type_t* t);
LIBMCXX_EXTERN char is_pack_type(type_t* t);
LIBMCXX_EXTERN type_t* pack_type_get_packed_type(type_t* t);

LIBMCXX_EXTERN void get_packs_in_type(type_t* pack_type,
        scope_entry_t*** packs_to_expand,
        int *num_packs_to_expand);

// Used for parameter packs when they are expanded but cannot be flattened
LIBMCXX_EXTERN type_t* get_sequence_of_types(int num_types, type_t** types);
LIBMCXX_EXTERN int sequence_of_types_get_num_types(type_t* seq_type);
LIBMCXX_EXTERN type_t* sequence_of_types_get_type_num(type_t* seq_type, int num);
LIBMCXX_EXTERN char is_sequence_of_types(type_t* seq_type);
LIBMCXX_EXTERN type_t* get_sequence_of_types_append_type(type_t* seq_type, type_t* type);
LIBMCXX_EXTERN type_t* get_sequence_of_types_flattened(int num_types, type_t** types);

/* Type constructors: cv-qualification */
// The given cv_qualifier is strictly the one will have the returning type
LIBMCXX_EXTERN type_t* get_cv_qualified_type(type_t* t, cv_qualifier_t cv_qualifier);

// These add 'const', 'volatile' or 'restrict' to the current qualification of t
LIBMCXX_EXTERN type_t* get_const_qualified_type(type_t* t);
LIBMCXX_EXTERN type_t* get_volatile_qualified_type(type_t* t);
LIBMCXX_EXTERN type_t* get_restrict_qualified_type(type_t* t);

/* Type constructors: derived types */
LIBMCXX_EXTERN type_t* get_pointer_type(type_t*);

LIBMCXX_EXTERN type_t* get_pointer_to_member_type(type_t*, type_t* class_entry);

LIBMCXX_EXTERN type_t* get_lvalue_reference_type(type_t* t);
LIBMCXX_EXTERN type_t* get_rvalue_reference_type(type_t* t);
LIBMCXX_EXTERN type_t* get_rebindable_reference_type(type_t* t);

LIBMCXX_EXTERN type_t* get_array_type(type_t* element_type, 
        nodecl_t size_of_array, 
        const decl_context_t* decl_context);

LIBMCXX_EXTERN type_t* get_array_type_bounds(type_t*, 
        nodecl_t lower_bound, 
        nodecl_t upper_bound, 
        const decl_context_t* decl_context);

LIBMCXX_EXTERN type_t* get_array_type_bounds_with_descriptor(type_t*, 
        nodecl_t lower_bound, 
        nodecl_t upper_bound, 
        const decl_context_t* decl_context);

LIBMCXX_EXTERN type_t* get_array_type_bounds_with_regions(type_t*, 
        nodecl_t lower_bound, 
        nodecl_t upper_bound, 
        const decl_context_t* decl_context,
        nodecl_t region, 
        const decl_context_t* decl_context_region);

LIBMCXX_EXTERN type_t* get_array_type_unknown_size_dependent(
        type_t* element_type);

LIBMCXX_EXTERN type_t* array_type_rebase(type_t* array_type,
        type_t* new_element_type);

// This is used only for C99 in parameter declarations
LIBMCXX_EXTERN type_t *get_cv_qualified_array_type(type_t *array_type,
                                                   cv_qualifier_t);
LIBMCXX_EXTERN cv_qualifier_t array_type_get_cv_qualifier(type_t *array_type);

LIBMCXX_EXTERN type_t* get_new_function_type(type_t* t,
        parameter_info_t* parameter_info, int num_parameters,
        ref_qualifier_t ref_qualifier);
type_t* get_new_function_type_trailing_type(type_t* t,
        parameter_info_t* parameter_info, int num_parameters,
        ref_qualifier_t ref_qualifier);
LIBMCXX_EXTERN type_t* get_nonproto_function_type(type_t* t, int num_parameters);

LIBMCXX_EXTERN type_t* get_vector_type_by_bytes(type_t* element_type, unsigned int vector_size);
LIBMCXX_EXTERN type_t* get_vector_type_by_elements(type_t* element_type, unsigned int num_elements);

LIBMCXX_EXTERN type_t* get_generic_vector_type(struct type_tag* element_type);

LIBMCXX_EXTERN type_t* get_mask_type(unsigned int mask_size_bits);

LIBMCXX_EXTERN type_t* get_computed_function_type(computed_function_type_t compute_type_function);

/* Fixes dependent typenames in types so they can be compared without context */
LIBMCXX_EXTERN type_t* fix_dependent_typenames_in_context(type_t* t, const decl_context_t* decl_context, const locus_t* locus);

/* Type comparison functions */
LIBMCXX_EXTERN char equivalent_types(type_t* t1, type_t* t2);
LIBMCXX_EXTERN char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2);

// Compares two function types ignoring ref qualifiers
LIBMCXX_EXTERN char equivalent_function_types_may_differ_ref_qualifier(
        type_t* ft1, type_t* ft2);

/* Modifiers used when the type is still being built */

LIBMCXX_EXTERN void class_type_add_base_class(type_t* class_type, 
        scope_entry_t* base_class, 
        char is_virtual, 
        char is_dependent,
        char is_expansion,
        access_specifier_t access_spec);

LIBMCXX_EXTERN void class_type_set_inner_context(type_t* class_type, const decl_context_t* decl_context);
LIBMCXX_EXTERN void class_type_set_destructor(type_t* class_type, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_set_default_constructor(type_t* t, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_set_enclosing_class_type(type_t* t, type_t* class_type);

LIBMCXX_EXTERN void class_type_add_friend_symbol(type_t* t, scope_entry_t* entry);
LIBMCXX_EXTERN void class_type_add_inherited_constructor(type_t* t, scope_entry_t* entry);

LIBMCXX_EXTERN void class_type_add_member(type_t* t,
        scope_entry_t* member,
        const decl_context_t* decl_context,
        char is_definition);
LIBMCXX_EXTERN void class_type_add_member_after(type_t* class_type,
        scope_entry_t* position,
        scope_entry_t* entry,
        const decl_context_t* decl_context,
        char is_definition);
LIBMCXX_EXTERN void class_type_add_member_before(type_t* class_type,
        scope_entry_t* position,
        scope_entry_t* entry,
        const decl_context_t* decl_context,
        char is_definition);

LIBMCXX_EXTERN void class_type_complete_if_needed(scope_entry_t* entry, const decl_context_t* decl_context, const locus_t* locus);
LIBMCXX_EXTERN char class_type_complete_if_possible(scope_entry_t* entry, const decl_context_t* decl_context, const locus_t* locus);

LIBMCXX_EXTERN void enum_type_add_enumerator(type_t* t, scope_entry_t* entry);
LIBMCXX_EXTERN void enum_type_set_underlying_type(type_t* t, type_t* underlying_type);
LIBMCXX_EXTERN void enum_type_set_underlying_type_is_fixed(type_t* t, char is_fixed);

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
LIBMCXX_EXTERN char is_unsigned_integral_type(type_t* t);
// A synonim in the standard
LIBMCXX_EXTERN char is_integer_type(type_t* t); 
LIBMCXX_EXTERN char is_enum_type(type_t* t);

LIBMCXX_EXTERN char is_unscoped_enum_type(type_t* t);
LIBMCXX_EXTERN char is_scoped_enum_type(type_t* t);

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

LIBMCXX_EXTERN char is_signed_byte_type(type_t* t);
LIBMCXX_EXTERN char is_unsigned_byte_type(type_t* t);

LIBMCXX_EXTERN char is_character_type(type_t* t);
LIBMCXX_EXTERN char is_char_type(type_t* t);
LIBMCXX_EXTERN char is_signed_char_type(type_t* t);
LIBMCXX_EXTERN char is_unsigned_char_type(type_t* t);

LIBMCXX_EXTERN char is_signed_int128_type(type_t*);
LIBMCXX_EXTERN char is_unsigned_int128_type(type_t*);

LIBMCXX_EXTERN char is_wchar_t_type(type_t* t);
LIBMCXX_EXTERN char is_char16_t_type(type_t* t);
LIBMCXX_EXTERN char is_char32_t_type(type_t* t);

LIBMCXX_EXTERN char is_floating_type(type_t* t);
LIBMCXX_EXTERN char is_double_type(type_t* t);
LIBMCXX_EXTERN char is_long_double_type(type_t* t);
LIBMCXX_EXTERN char is_float_type(type_t* t);
LIBMCXX_EXTERN char is_other_float_type(type_t* t);
LIBMCXX_EXTERN char is_float128_type(type_t* t);

LIBMCXX_EXTERN const floating_type_info_t* floating_type_get_info(type_t* t);

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
LIBMCXX_EXTERN char is_rebindable_reference_type(type_t* t1);
LIBMCXX_EXTERN char is_any_reference_type(type_t* t1);

LIBMCXX_EXTERN char is_vector_type(type_t* t);

LIBMCXX_EXTERN char is_generic_vector_type(type_t* t);

LIBMCXX_EXTERN char is_class_type(type_t* possible_class);
LIBMCXX_EXTERN char is_unnamed_class_type(type_t* possible_class);
LIBMCXX_EXTERN char is_named_class_type(type_t* possible_class);

// Convenience
LIBMCXX_EXTERN char is_class_type_or_array_thereof(type_t* t);

LIBMCXX_EXTERN char is_literal_type(type_t* possible_union);

LIBMCXX_EXTERN char is_trivial_type(type_t* possible_union);

LIBMCXX_EXTERN char is_union_type(type_t* possible_union);

LIBMCXX_EXTERN char is_named_type(type_t* t);

// All indirect types are named types (the converse is not always true)
LIBMCXX_EXTERN char is_indirect_type(type_t* t);

// Some indirect types are marked as mutable to avoid caching issues
LIBMCXX_EXTERN char is_mutable_indirect_type(type_t* t);

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

LIBMCXX_EXTERN char is_zero_type(type_t* t); // Alias of variant_type_is_zero
LIBMCXX_EXTERN char variant_type_is_zero(type_t* t);

LIBMCXX_EXTERN char is_zero_type_or_nullptr_type(type_t* t);

LIBMCXX_EXTERN char is_nullptr_type(type_t* t);

LIBMCXX_EXTERN char is_error_type(type_t* t);

LIBMCXX_EXTERN char is_throw_expr_type(type_t* t);

LIBMCXX_EXTERN char is_implicit_none_type(type_t *t);

LIBMCXX_EXTERN char is_pseudo_destructor_call_type(type_t *t);

LIBMCXX_EXTERN char is_string_literal_type(type_t* t);

LIBMCXX_EXTERN char is_template_type(type_t* t);

LIBMCXX_EXTERN char is_literal_type(type_t* t);

LIBMCXX_EXTERN char is_trivial_type(type_t* t);

LIBMCXX_EXTERN char is_scalar_type(type_t* t);

LIBMCXX_EXTERN char is_incomplete_type(type_t* t);
LIBMCXX_EXTERN char is_complete_type(type_t* t);

// A type returned by template_type_get_primary_type or template_type_get_specialized_type
LIBMCXX_EXTERN char is_template_specialized_type(type_t* t);

/* Query functions: cv-qualification */
LIBMCXX_EXTERN type_t* get_unqualified_type(type_t* t);
LIBMCXX_EXTERN cv_qualifier_t get_cv_qualifier(type_t* type_info);

LIBMCXX_EXTERN char type_is_reference_related_to(type_t* t1, type_t* t2);
LIBMCXX_EXTERN char type_is_reference_compatible_to(type_t* t1, type_t* t2);

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

LIBMCXX_EXTERN char is_unqualified_type(type_t* t1);
LIBMCXX_EXTERN char is_const_qualified_type(type_t* t1);
LIBMCXX_EXTERN char is_volatile_qualified_type(type_t* t1);
LIBMCXX_EXTERN char is_restrict_qualified_type(type_t* t1);

LIBMCXX_EXTERN char is_const_qualified(cv_qualifier_t cv);
LIBMCXX_EXTERN char is_volatile_qualified(cv_qualifier_t cv);
LIBMCXX_EXTERN char is_restrict_qualified(cv_qualifier_t cv);

LIBMCXX_EXTERN char is_computed_function_type(type_t* t);

LIBMCXX_EXTERN char is_mask_type(type_t* t);
LIBMCXX_EXTERN unsigned int mask_type_get_num_bits(type_t* t);
LIBMCXX_EXTERN type_t* mask_type_get_underlying_type(type_t* t);

LIBMCXX_EXTERN int get_sizeof_type(type_t* t);

/* Query functions: specific ones */
LIBMCXX_EXTERN int function_type_get_num_parameters(type_t* function_type);
LIBMCXX_EXTERN type_t* function_type_get_parameter_type_num(type_t* function_type, int num_param);
LIBMCXX_EXTERN type_t* function_type_get_nonadjusted_parameter_type_num(type_t* function_type, int num_param);
LIBMCXX_EXTERN char function_type_get_lacking_prototype(type_t* function_type);
LIBMCXX_EXTERN char function_type_get_has_ellipsis(type_t* function_type);
LIBMCXX_EXTERN char function_type_get_has_trailing_return(type_t *t);
LIBMCXX_EXTERN type_t* function_type_get_return_type(type_t* t);

LIBMCXX_EXTERN ref_qualifier_t function_type_get_ref_qualifier(type_t* t);

LIBMCXX_EXTERN char function_type_can_override(type_t* potential_overrider, type_t* function_type);

LIBMCXX_EXTERN char function_type_same_parameter_types_and_cv_qualif(type_t* t1, type_t* t2);

LIBMCXX_EXTERN type_t* function_type_replace_return_type(type_t* t, type_t* new_return);
LIBMCXX_EXTERN type_t* function_type_replace_return_type_with_trailing_return(type_t* t, type_t* new_return);

LIBMCXX_EXTERN type_t* pointer_type_get_pointee_type(type_t *t);
LIBMCXX_EXTERN type_t* pointer_to_member_type_get_class_type(type_t *t);

LIBMCXX_EXTERN scope_entry_list_t *unresolved_overloaded_type_get_overload_set(type_t* t);

LIBMCXX_EXTERN type_t* array_type_get_element_type(type_t* t);
LIBMCXX_EXTERN nodecl_t array_type_get_array_size_expr(type_t* t);
LIBMCXX_EXTERN const decl_context_t* array_type_get_array_size_expr_context(type_t* t);


LIBMCXX_EXTERN char array_type_is_unknown_size(type_t* t);
LIBMCXX_EXTERN nodecl_t array_type_get_array_lower_bound(type_t* t);
LIBMCXX_EXTERN nodecl_t array_type_get_array_upper_bound(type_t* t);

// This function returns the total number of elements of an array if
// all dimensions of this array are constant. Otherwise returns -1.
LIBMCXX_EXTERN int  array_type_get_total_number_of_elements(type_t* t);

LIBMCXX_EXTERN char array_type_is_vla(type_t* t);

LIBMCXX_EXTERN char array_type_is_string_literal(type_t* t);

LIBMCXX_EXTERN char array_type_with_descriptor(type_t* t);

LIBMCXX_EXTERN char array_type_has_region(type_t* t);
LIBMCXX_EXTERN const decl_context_t* array_type_get_region_size_expr_context(type_t* t);
LIBMCXX_EXTERN nodecl_t array_type_get_region_size_expr(type_t* t);
LIBMCXX_EXTERN nodecl_t array_type_get_region_lower_bound(type_t* t);
LIBMCXX_EXTERN nodecl_t array_type_get_region_upper_bound(type_t* t);
LIBMCXX_EXTERN nodecl_t array_type_get_region_stride(type_t* t);

LIBMCXX_EXTERN int enum_type_get_num_enumerators(type_t* t);
LIBMCXX_EXTERN scope_entry_t* enum_type_get_enumerator_num(type_t* t, int n);
LIBMCXX_EXTERN type_t* enum_type_get_underlying_type(type_t* t);
LIBMCXX_EXTERN char enum_type_get_underlying_type_is_fixed(type_t* t);

LIBMCXX_EXTERN enum type_tag_t class_type_get_class_kind(type_t* t);
LIBMCXX_EXTERN void class_type_set_class_kind(type_t* t, enum type_tag_t class_kind);
LIBMCXX_EXTERN int class_type_get_num_bases(type_t* class_type);
LIBMCXX_EXTERN scope_entry_t* class_type_get_base_num(type_t* class_type, int num, 
        char *is_virtual, 
        char *is_dependent,
        char *is_expansion,
        access_specifier_t *access_specifier);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_all_bases(type_t *t, char include_dependent);

LIBMCXX_EXTERN scope_entry_list_t* class_type_get_members(type_t* t);
LIBMCXX_EXTERN member_declaration_info_t* class_type_get_member_declarations(type_t* t, int *num_declarations);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_nonstatic_data_members(type_t* t);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_static_data_members(type_t* t);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_member_functions(type_t* t);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_constructors(type_t* t);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_conversions(type_t* t);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_virtual_base_classes(type_t* t);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_direct_base_classes(type_t* t);

LIBMCXX_EXTERN scope_entry_list_t* class_type_get_virtual_base_classes_canonical(type_t* t);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_direct_base_classes_canonical(type_t* t);

LIBMCXX_EXTERN type_t* class_type_get_enclosing_class_type(type_t* t);

LIBMCXX_EXTERN computed_function_type_t computed_function_type_get_computing_function(type_t* t);

// Gives all the conversions related to a class
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_all_conversions(type_t* class_type, 
        const decl_context_t* decl_context);

LIBMCXX_EXTERN scope_entry_list_t* class_type_get_copy_assignment_operators(type_t* t);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_move_assignment_operators(type_t* t);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_copy_constructors(type_t* t);
LIBMCXX_EXTERN scope_entry_list_t* class_type_get_move_constructors(type_t* t);
LIBMCXX_EXTERN scope_entry_t* class_type_get_default_constructor(type_t* t);

LIBMCXX_EXTERN scope_entry_t* class_type_get_destructor(type_t* t);
LIBMCXX_EXTERN const decl_context_t* class_type_get_context(type_t* t);
LIBMCXX_EXTERN const decl_context_t* class_type_get_inner_context(type_t* class_type);

LIBMCXX_EXTERN const decl_context_t* class_or_enum_type_get_inner_context(type_t* class_or_enum_type);

LIBMCXX_EXTERN scope_entry_list_t* class_type_get_virtual_functions(type_t* class_type);

LIBMCXX_EXTERN scope_entry_list_t* class_type_get_friends(type_t* class_type);

LIBMCXX_EXTERN scope_entry_list_t* class_type_get_inherited_constructors(type_t* t);

LIBMCXX_EXTERN const decl_context_t* enum_type_get_context(type_t* t);

LIBMCXX_EXTERN scope_entry_t* named_type_get_symbol(type_t* t);

LIBMCXX_EXTERN char pointer_types_are_similar(type_t* t_orig, type_t* t_dest);

LIBMCXX_EXTERN type_t* template_type_get_primary_type(type_t* t);

LIBMCXX_EXTERN type_t* template_type_get_specialized_type(type_t* t,
        template_parameter_list_t * template_parameters,
        const decl_context_t* decl_context,
        const locus_t* locus);
LIBMCXX_EXTERN type_t* template_type_get_specialized_type_for_instantiation(type_t* t,
        template_parameter_list_t* template_parameters,
        type_t* type_used_as_template,
        const decl_context_t* decl_context, 
        const locus_t* locus);

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
        nodecl_t* dependent_parts);

LIBMCXX_EXTERN int vector_type_get_vector_size_in_bytes(type_t*);
LIBMCXX_EXTERN type_t* vector_type_get_element_type(type_t*);
LIBMCXX_EXTERN int vector_type_get_num_elements(type_t*);

LIBMCXX_EXTERN type_t* braced_list_type_get_type_num(type_t* t, int num);
LIBMCXX_EXTERN int braced_list_type_get_num_types(type_t* t);
LIBMCXX_EXTERN type_t** braced_list_type_get_types(type_t* t);

/* Query functions: Miscelaneous stuff not classified otherwise */
LIBMCXX_EXTERN char class_type_is_trivial(type_t* t);
LIBMCXX_EXTERN char class_type_is_trivially_copiable(type_t* t);
LIBMCXX_EXTERN char class_type_is_standard_layout(type_t* t);

// Avoid using these from now and prioritize the _instantiating versions
LIBMCXX_EXTERN char class_type_is_base(type_t* possible_base, type_t* possible_derived);
LIBMCXX_EXTERN char class_type_is_base_strict(type_t* possible_base, type_t* possible_derived);
LIBMCXX_EXTERN char class_type_is_derived(type_t* possible_derived, type_t* possible_base);
LIBMCXX_EXTERN char class_type_is_derived_strict(type_t* possible_derived, type_t* possible_base);

LIBMCXX_EXTERN char class_type_is_base_instantiating(type_t* possible_base, type_t* possible_derived, const locus_t* locus);
LIBMCXX_EXTERN char class_type_is_base_strict_instantiating(type_t* possible_base, type_t* possible_derived, const locus_t* locus);
LIBMCXX_EXTERN char class_type_is_derived_instantiating(type_t* possible_derived, type_t* possible_base, const locus_t* locus);
LIBMCXX_EXTERN char class_type_is_derived_strict_instantiating(type_t* possible_derived, type_t* possible_base, const locus_t* locus);

LIBMCXX_EXTERN char class_type_is_ambiguous_base_of_derived_class(type_t* base_class, type_t* derived_class);
LIBMCXX_EXTERN char class_type_is_virtual_base_or_base_of_virtual_base(type_t* base_class, type_t* derived_class);

LIBMCXX_EXTERN char is_pointer_to_void_type(type_t* t);
LIBMCXX_EXTERN char is_pointer_to_function_type(type_t* t1);

LIBMCXX_EXTERN char class_type_is_empty(type_t* t);
LIBMCXX_EXTERN char class_type_is_nearly_empty(type_t* t);
LIBMCXX_EXTERN char class_type_is_dynamic(type_t* t);
LIBMCXX_EXTERN char class_type_is_polymorphic(type_t* t);

LIBMCXX_EXTERN char class_type_is_abstract(type_t* class_type);
LIBMCXX_EXTERN void class_type_set_is_abstract(type_t* class_type, char is_abstract);

LIBMCXX_EXTERN char class_type_is_lambda(type_t* class_type);
LIBMCXX_EXTERN void class_type_set_is_lambda(type_t* class_type, char is_lambda);

LIBMCXX_EXTERN void class_type_set_is_packed(type_t* t, char is_packed);
LIBMCXX_EXTERN char class_type_is_packed(type_t* class_type);

LIBMCXX_EXTERN type_t* advance_over_typedefs(type_t* t);
LIBMCXX_EXTERN type_t* advance_over_typedefs_with_cv_qualif(type_t* t1, cv_qualifier_t* cv_qualif);

LIBMCXX_EXTERN type_t* reference_type_get_referenced_type(type_t* t1);

// Remove reference type
LIBMCXX_EXTERN type_t* no_ref(type_t* t);

// Only returns a reference in C++. In C it does nothing
LIBMCXX_EXTERN type_t* lvalue_ref(type_t* t);

LIBMCXX_EXTERN type_t* get_actual_class_type(type_t* class_type);

LIBMCXX_EXTERN type_t* get_actual_enum_type(type_t* enum_type);

LIBMCXX_EXTERN char is_pointer_to_member_type(type_t* t);

LIBMCXX_EXTERN char is_pointer_to_class_type(type_t* t1);
LIBMCXX_EXTERN char is_lvalue_reference_to_class_type(type_t* t1);
LIBMCXX_EXTERN char is_rvalue_reference_to_class_type(type_t* t1);
LIBMCXX_EXTERN char is_rebindable_reference_to_class_type(type_t* t1);
LIBMCXX_EXTERN char is_any_reference_to_class_type(type_t* t1);

LIBMCXX_EXTERN char class_type_is_incomplete_dependent(type_t* t);
LIBMCXX_EXTERN char class_type_is_complete_dependent(type_t* t);
LIBMCXX_EXTERN char class_type_is_incomplete_independent(type_t* t);
LIBMCXX_EXTERN char class_type_is_complete_independent(type_t* t);

LIBMCXX_EXTERN char pointer_types_can_be_converted(type_t* orig, type_t* dest);

LIBMCXX_EXTERN void set_as_template_specialized_type(type_t* type_to_specialize,
        template_parameter_list_t * template_parameters,
        type_t* template_type);

/* Naming types functions */
LIBMCXX_EXTERN const char* get_declaration_string(type_t* type_info,
        const decl_context_t* decl_context,
        const char* symbol_name,
        const char* initializer,
        char semicolon,
        int num_parameter_names,
        const char** parameter_names,
        const char** parameter_attributes,
        char is_parameter);

LIBMCXX_EXTERN const char* get_declaration_string_ex(type_t* type_info,
        const decl_context_t* decl_context,
        const char* symbol_name, const char* initializer,
        char semicolon,
        int num_parameter_names,
        const char** parameter_names,
        const char** parameter_attributes,
        char is_parameter,
        char unparenthesize_ptr_operator,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data);

// This function is extern because it's used in cxx-scope.c
LIBMCXX_EXTERN const char* get_simple_type_name_string_internal_common(
        scope_entry_t* entry,
        const decl_context_t* decl_context,
        void* data UNUSED_PARAMETER);

// Like get_declarator_name_string_ex but only returns the declarator part, ignoring the type specifier
LIBMCXX_EXTERN const char* get_declarator_name_string_ex(const decl_context_t* decl_context,
        type_t* type_info,
        const char* symbol_name,
        int num_parameter_names,
        const char** parameter_names,
        const char** parameter_attributes,
        char is_parameter,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data);

LIBMCXX_EXTERN type_t* get_ellipsis_type(void);
LIBMCXX_EXTERN char is_ellipsis_type(type_t* t);

LIBMCXX_EXTERN type_t* get_braced_list_type(int num_types, type_t** arg_list);
LIBMCXX_EXTERN char is_braced_list_type(type_t* t);

LIBMCXX_EXTERN char has_dependent_template_parameters(template_parameter_list_t* template_parameters);

LIBMCXX_EXTERN char is_template_explicit_specialization(template_parameter_list_t* template_parameters);

LIBMCXX_EXTERN char syntactic_comparison_of_nested_names(
        nodecl_t dependent_parts_1,
        nodecl_t dependent_parts_2);

/* Debug purpose functions */
LIBMCXX_EXTERN const char* print_declarator(type_t* printed_declarator);

LIBMCXX_EXTERN const char* sci_conversion_to_str(standard_conversion_item_t e);

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

LIBMCXX_EXTERN size_t get_type_t_size(void);

LIBMCXX_EXTERN const char* print_decl_type_str(type_t* t, const decl_context_t* decl_context, const char* name);
LIBMCXX_EXTERN const char* print_type_str(type_t* t, const decl_context_t* decl_context);

LIBMCXX_EXTERN type_t* type_deep_copy(type_t* orig,
        const decl_context_t* new_decl_context,
        symbol_map_t* symbol_map);

LIBMCXX_EXTERN type_t* type_deep_copy_compute_maps(
        type_t* orig_type,
        scope_entry_t* dest,
        const decl_context_t* new_decl_context,
        symbol_map_t* symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map);

LIBMCXX_EXTERN type_t* get_variant_type_interoperable(type_t* t);
LIBMCXX_EXTERN char variant_type_is_interoperable(type_t* t);

LIBMCXX_EXTERN type_t* get_variant_type_add_gcc_attribute(type_t* t, gcc_attribute_t attr);
LIBMCXX_EXTERN type_t* get_variant_type_add_ms_attribute(type_t* t, gcc_attribute_t attr);

LIBMCXX_EXTERN void variant_type_get_gcc_attributes(type_t* t, int* num_attrs, gcc_attribute_t** attrs);
LIBMCXX_EXTERN void variant_type_get_ms_attributes(type_t* t, int* num_attrs, gcc_attribute_t** attrs);

// C11 atomics
LIBMCXX_EXTERN type_t* get_variant_type_atomic(type_t* t);
LIBMCXX_EXTERN char variant_type_is_atomic(type_t* t);
LIBMCXX_EXTERN char is_atomic_type(type_t* t);

// Vector flavor (NULL-ended array of vector flavors)
LIBMCXX_EXTERN const char* vector_flavors[];
LIBMCXX_EXTERN void vector_types_set_flavor(const char* c);
LIBMCXX_EXTERN const char* vector_types_get_vector_flavor(void);

// TL::Source stuff
LIBMCXX_EXTERN const char* type_to_source(type_t* t);
LIBMCXX_EXTERN char is_function_or_template_function_name_or_extern_variable(scope_entry_t* entry, void* p UNUSED_PARAMETER);

// C++ auto
LIBMCXX_EXTERN type_t* get_auto_type(void);
LIBMCXX_EXTERN type_t* get_decltype_auto_type(void);
LIBMCXX_EXTERN char is_auto_type(type_t* t);
LIBMCXX_EXTERN char type_is_derived_from_auto(type_t* t);

LIBMCXX_EXTERN char is_decltype_auto_type(type_t* t);

// C genericity stuff. 
// Used only to implement gcc builtins. Not to be used elsewhere!
LIBMCXX_EXTERN type_t* get_generic_type(int num);
LIBMCXX_EXTERN char is_generic_type(type_t*);
LIBMCXX_EXTERN int generic_type_get_num(type_t*);

LIBMCXX_EXTERN const char* print_gnu_vector_type(
        const decl_context_t* decl_context,
        type_t* t,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data);

LIBMCXX_EXTERN const char* print_intel_sse_avx_vector_type(
        const decl_context_t* decl_context,
        type_t* t,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data);

LIBMCXX_EXTERN const char* print_altivec_vector_type(
        const decl_context_t* decl_context,
        type_t* t,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data);

LIBMCXX_EXTERN const char* print_opencl_vector_type(
        const decl_context_t* decl_context,
        type_t* t,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data);

LIBMCXX_EXTERN parameter_info_t get_parameter_info_for_type(type_t* t);

// Used by cxx-typeorder.c
void free_temporary_template_type(type_t* t);

MCXX_END_DECLS
#endif // CXX_TYPEUTILS_H
