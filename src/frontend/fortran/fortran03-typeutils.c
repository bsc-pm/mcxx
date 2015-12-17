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

#include "fortran03-typeutils.h"
#include "fortran03-codegen.h"
#include "cxx-nodecl-decls.h"
#include "cxx-limits.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include <string.h>

const char* fortran_print_type_str(type_t* t)
{
    t = no_ref(t);

    if (is_error_type(t))
    {
        return "<error-type>";
    }

    if (is_hollerith_type(t))
    {
        return "HOLLERITH";
    }

    const char* result = "";
    char is_pointer = 0;
    if (is_pointer_type(t))
    {
        is_pointer = 1;
        t = pointer_type_get_pointee_type(t);
    }

    struct array_spec_tag {
        nodecl_t lower;
        nodecl_t upper;
        char is_undefined;
    } array_spec_list[MCXX_MAX_ARRAY_SPECIFIER] = { { nodecl_null(), nodecl_null(), 0 }  };

    int array_spec_idx;
    for (array_spec_idx = MCXX_MAX_ARRAY_SPECIFIER - 1; 
            fortran_is_array_type(t);
            array_spec_idx--)
    {
        if (array_spec_idx < 0)
        {
            internal_error("too many array dimensions %d\n", MCXX_MAX_ARRAY_SPECIFIER);
        }

        if (!array_type_is_unknown_size(t))
        {
            array_spec_list[array_spec_idx].lower = array_type_get_array_lower_bound(t);
            array_spec_list[array_spec_idx].upper = array_type_get_array_upper_bound(t);
        }
        else
        {
            array_spec_list[array_spec_idx].is_undefined = 1;
        }

        t = array_type_get_element_type(t);
    }

    char is_array = (array_spec_idx != (MCXX_MAX_ARRAY_SPECIFIER - 1));

    if (is_bool_type(t)
            || is_integer_type(t)
            || is_floating_type(t)
            || is_double_type(t)
            || is_complex_type(t))
    {
        const char* type_name = NULL;
        char c[128] = { 0 };

        if (is_bool_type(t))
        {
            type_name = "LOGICAL";
        }
        else if (is_integer_type(t))
        {
            type_name = "INTEGER";
        }
        else if (is_floating_type(t))
        {
            type_name = "REAL";
        }
        else if (is_complex_type(t))
        {
            type_name = "COMPLEX";
        }
        else
        {
            internal_error("unreachable code", 0);
        }

        size_t size = type_get_size(t);
        if (is_floating_type(t))
        {
            // KIND of floats is their size in byes (using the bits as in IEEE754) 
            size = (floating_type_get_info(t)->bits) / 8;
        }
        else if (is_complex_type(t))
        {
            // KIND of a complex is the KIND of its component type
            type_t* f = complex_type_get_base_type(t);
            size = (floating_type_get_info(f)->bits) / 8;
        }

        snprintf(c, 127, "%s(%zd)", type_name, size);
        c[127] = '\0';

        result = uniquestr(c);
    }
    else if (is_class_type(t))
    {
        scope_entry_t* entry = named_type_get_symbol(t);
        char c[128] = { 0 };
        snprintf(c, 127, "TYPE(%s)", 
                entry->symbol_name);
        c[127] = '\0';

        result = uniquestr(c);
    }
    else if (fortran_is_character_type(t))
    {
        nodecl_t length = array_type_get_array_size_expr(t);
        char c[128] = { 0 };
        snprintf(c, 127, "CHARACTER(LEN=%s)",
                nodecl_is_null(length) ? "*" : codegen_to_str(length, nodecl_retrieve_context(length)));
        c[127] = '\0';
        result = uniquestr(c);
    }
    else if (is_function_type(t))
    {
        result = "PROCEDURE";
    }
    else
    {
        const char* non_printable = NULL;
        uniquestr_sprintf(&non_printable, "non-fortran type '%s'", print_declarator(t));
        return non_printable;
    }

    if (is_pointer)
    {
        result = strappend(result, ", POINTER");
    }

    if (is_array)
    {
        array_spec_idx++;
        result = strappend(result, ", DIMENSION(");

        while (array_spec_idx <= (MCXX_MAX_ARRAY_SPECIFIER - 1))
        {
            if (!array_spec_list[array_spec_idx].is_undefined)
            {
                result = strappend(result, codegen_to_str(array_spec_list[array_spec_idx].lower, 
                            nodecl_retrieve_context(array_spec_list[array_spec_idx].lower)));
                result = strappend(result, ":");
                result = strappend(result, codegen_to_str(array_spec_list[array_spec_idx].upper, 
                            nodecl_retrieve_context(array_spec_list[array_spec_idx].upper)));
            }
            else
            {
                result = strappend(result, ":");
            }
            if ((array_spec_idx + 1) <= (MCXX_MAX_ARRAY_SPECIFIER - 1))
            {
                result = strappend(result, ", ");
            }
            array_spec_idx++;
        }

        result = strappend(result, ")");
    }

    return result;
}

int fortran_get_rank_of_type(type_t* t)
{
    t = no_ref(t);

    if (!fortran_is_array_type(t)
            && !fortran_is_pointer_to_array_type(t))
        return 0;

    if (fortran_is_pointer_to_array_type(t))
    {
        t = pointer_type_get_pointee_type(t);
    }

    int result = 0;
    while (fortran_is_array_type(t))
    {
        result++;
        t = array_type_get_element_type(t);
    }

    return result;
}

type_t* fortran_get_rank0_type(type_t* t)
{
    t = no_ref(t);

    if (is_pointer_type(t))
        t = pointer_type_get_pointee_type(t);

    while (fortran_is_array_type(t))
    {
        t = array_type_get_element_type(t);
    }
    return t;
}

char fortran_is_character_type(type_t* t)
{
    t = no_ref(t);

    return (is_array_type(t)
            && is_character_type(array_type_get_element_type(t)));
}

char fortran_is_character_type_or_pointer_to(type_t* t)
{
    t = no_ref(t);
    return fortran_is_pointer_to_character_type(t)
        || fortran_is_character_type(t);
}

char fortran_is_pointer_to_character_type(type_t* t)
{
    t = no_ref(t);

    if (is_pointer_type(t))
    {
        return fortran_is_character_type(pointer_type_get_pointee_type(t));
    }
    return 0;
}

type_t* fortran_replace_return_type_of_function_type(type_t* function_type, type_t* new_return_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "Must be a function type", 0);

    cv_qualifier_t cv = get_cv_qualifier(function_type_get_return_type(function_type));

    int num_parameters = function_type_get_num_parameters(function_type);
    if (!function_type_get_lacking_prototype(function_type))
    {
        parameter_info_t parameter_info[1 + num_parameters];
        memset(&parameter_info, 0, sizeof(parameter_info));
        int i;
        for (i = 0; i < num_parameters; i++)
        {
            parameter_info[i].type_info = function_type_get_parameter_type_num(function_type, i);
        }

        return get_new_function_type(
                get_cv_qualified_type(new_return_type, cv),
                parameter_info, num_parameters, REF_QUALIFIER_NONE);
    }
    else
    {
        return get_nonproto_function_type(
                get_cv_qualified_type(new_return_type, cv),
                num_parameters);
    }
}

char fortran_equivalent_tk_types(type_t* t1, type_t* t2)
{
    type_t* r1 = t1;
    if (is_function_type(r1))
    {
        r1 = function_type_get_return_type(r1);
    }
    r1 = fortran_get_rank0_type(r1);

    type_t* r2 = t2;
    if (is_function_type(r2))
    {
        r2 = function_type_get_return_type(r2);
    }
    r2 = fortran_get_rank0_type(r2);

    // Preprocess for character types
    if (fortran_is_character_type(r1))
    {
        r1 = get_unqualified_type(array_type_get_element_type(r1));
    }
    if (fortran_is_character_type(r2))
    {
        r2 = get_unqualified_type(array_type_get_element_type(r2));
    }

    return equivalent_types(get_unqualified_type(r1), get_unqualified_type(r2));
}

char fortran_equivalent_tkr_types(type_t* t1, type_t* t2)
{
    if (!fortran_equivalent_tk_types(t1, t2))
        return 0;

    int rank1 = fortran_get_rank_of_type(t1);
    int rank2 = fortran_get_rank_of_type(t2);

    if (rank1 != rank2)
        return 0;

    return 1;
}

type_t* fortran_get_basic_type(type_t* type_info)
{
    // Many functions drop the reference type, so chek it the first
    if (is_lvalue_reference_type(type_info))
    {
        return fortran_get_basic_type(no_ref(type_info));
    }
    else if (is_pointer_type(type_info))
    {
        return fortran_get_basic_type(pointer_type_get_pointee_type(type_info));
    }
    else if (fortran_is_array_type(type_info))
    {
        return fortran_get_basic_type(array_type_get_element_type(type_info));
    }
    else if (is_function_type(type_info))
    {
        return fortran_get_basic_type(function_type_get_return_type(type_info));
    }
    else
    {
        return type_info;
    }
}

type_t* fortran_update_basic_type_with_type(type_t* type_info, type_t* basic_type)
{
    if (is_error_type(basic_type))
        return basic_type;

    // Many functions drop the reference type, so chek it the first
    if (is_lvalue_reference_type(type_info))
    {
        return get_lvalue_reference_type(
                fortran_update_basic_type_with_type(reference_type_get_referenced_type(type_info), basic_type));
    }
    else if (is_pointer_type(type_info))
    {
        cv_qualifier_t cv = get_cv_qualifier(type_info);
        return get_cv_qualified_type(
                get_pointer_type(
                    fortran_update_basic_type_with_type(pointer_type_get_pointee_type(type_info), basic_type)
                    ),
                cv);
    }
    else if (fortran_is_array_type(type_info))
    {
        return get_array_type_bounds(
                fortran_update_basic_type_with_type(array_type_get_element_type(type_info), basic_type),
                array_type_get_array_lower_bound(type_info),
                array_type_get_array_upper_bound(type_info),
                array_type_get_array_size_expr_context(type_info));
    }
    else if (is_function_type(type_info))
    {
        return fortran_replace_return_type_of_function_type(type_info, basic_type);
    }
    else
    {
        cv_qualifier_t cv = get_cv_qualifier(type_info);
        return get_cv_qualified_type(basic_type, cv);
    }
}

char fortran_basic_type_is_implicit_none(type_t* t)
{
    if (t == NULL)
    {
        return 0;
    }
    else if (is_implicit_none_type(t))
    {
        return 1;
    }
    else if (is_array_type(t))
    {
        return fortran_basic_type_is_implicit_none(array_type_get_element_type(t));
    }
    else if (is_function_type(t))
    {
        return fortran_basic_type_is_implicit_none(function_type_get_return_type(t));
    }
    else if (is_lvalue_reference_type(t))
    {
        return fortran_basic_type_is_implicit_none(reference_type_get_referenced_type(t));
    }
    else if (is_pointer_type(t))
    {
        return fortran_basic_type_is_implicit_none(pointer_type_get_pointee_type(t));
    }
    else
        return 0;
}

char fortran_is_array_type(type_t* t)
{
    t = no_ref(t);

    return is_array_type(t)
        && !fortran_is_character_type(t);
}

char fortran_is_pointer_to_array_type(type_t* t)
{
    t = no_ref(t);

    return is_pointer_type(t)
        && fortran_is_array_type(pointer_type_get_pointee_type(t));
}

char fortran_is_array_type_or_pointer_to(type_t* t)
{
    return fortran_is_array_type(t)
        || fortran_is_pointer_to_array_type(t);
}

char fortran_is_scalar_type(type_t* t)
{
    return (!is_pointer_type(t)
            && !is_pointer_to_member_type(t)
            && !is_array_type(t)
            && !is_lvalue_reference_type(t)
            && !is_rvalue_reference_type(t)
            && !is_function_type(t)
            && !is_vector_type(t));
}

type_t* fortran_rebuild_array_type(type_t* rank0_type, type_t* array_type)
{
    rank0_type = no_ref(rank0_type);

    // ERROR_CONDITION(!fortran_is_scalar_type(rank0_type)
    //         && !fortran_is_character_type(rank0_type), "Invalid rank0 type", 0);

    if (!fortran_is_array_type(array_type))
    {
        return rank0_type;
    }
    else
    {
        type_t* t = fortran_rebuild_array_type(rank0_type, array_type_get_element_type(array_type));

        if (array_type_has_region(array_type))
        {
            return get_array_type_bounds_with_regions(t, 
                    array_type_get_array_lower_bound(array_type),
                    array_type_get_array_upper_bound(array_type),
                    array_type_get_array_size_expr_context(array_type),
                    // Why did we do this so difficult?
                    nodecl_make_range(
                        nodecl_shallow_copy(array_type_get_region_lower_bound(array_type)),
                        nodecl_shallow_copy(array_type_get_region_upper_bound(array_type)),
                        nodecl_shallow_copy(array_type_get_region_stride(array_type)),
                        fortran_get_default_integer_type(),
                        make_locus("", 0, 0)),
                    array_type_get_region_size_expr_context(array_type)
                    );
        }
        else if (array_type_with_descriptor(array_type))
        {
            return get_array_type_bounds_with_descriptor(t, 
                    array_type_get_array_lower_bound(array_type),
                    array_type_get_array_upper_bound(array_type),
                    array_type_get_array_size_expr_context(array_type));
        }
        else 
        {
            return get_array_type_bounds(t, 
                    array_type_get_array_lower_bound(array_type),
                    array_type_get_array_upper_bound(array_type),
                    array_type_get_array_size_expr_context(array_type));
        }
    }
}

type_t* fortran_get_n_ranked_type(type_t* scalar_type, int rank, const decl_context_t* decl_context)
{
    scalar_type = no_ref(scalar_type);

    ERROR_CONDITION(fortran_is_array_type(scalar_type), "This is not a scalar type!", 0);

    if (rank == 0)
    {
        return scalar_type;
    }
    else if (rank > 0)
    {
        return get_array_type(fortran_get_n_ranked_type(scalar_type, rank-1, decl_context), nodecl_null(), decl_context);
    }
    else
    {
        internal_error("Invalid rank %d\n", rank);
    }
}

type_t* fortran_get_n_ranked_type_with_descriptor(type_t* scalar_type, int rank, const decl_context_t* decl_context)
{
    scalar_type = no_ref(scalar_type);

    ERROR_CONDITION(fortran_is_array_type(scalar_type), "This is not a scalar type!", 0);

    if (rank == 0)
    {
        return scalar_type;
    }
    else if (rank > 0)
    {
        return get_array_type_bounds_with_descriptor(
                fortran_get_n_ranked_type_with_descriptor(scalar_type, rank-1, decl_context), nodecl_null(), nodecl_null(), decl_context);
    }
    else
    {
        internal_error("Invalid rank %d\n", rank);
    }
}

char fortran_is_intrinsic_type(type_t* t)
{
    t = no_ref(t);

    if (is_pointer_type(t))
        t = pointer_type_get_pointee_type(t);

    return (is_integer_type(t)
            || is_floating_type(t)
            || is_complex_type(t)
            || is_bool_type(t)
            || fortran_is_character_type(t));
}

char fortran_type_is_conformable_to(type_t* t1, type_t* t2)
{
    t1 = no_ref(t1);
    t2 = no_ref(t2);

    if (fortran_get_rank_of_type(t1) == fortran_get_rank_of_type(t2))
        return 1;
    else if (fortran_get_rank_of_type(t1) == 0)
        return 1;
    else
        return 0;
}

static type_t* _default_integer_type = NULL;
static type_t* _default_real_type = NULL;
static type_t* _doubleprecision_type = NULL;
static type_t* _default_logical_type = NULL;
static type_t* _default_character_type = NULL;

static void wrong_init_kind(const char* typename, int kind_size, type_t* (*kind_fun)(int))
{
    int number_of_valids = 0;
    const char* valid_set = "";

    int test = 1;
    while (test <= 16)
    {
        if (kind_fun(test) != NULL)
        {
            if (number_of_valids > 0)
            {
                uniquestr_sprintf(&valid_set, "%s, %d", valid_set, test);
            }
            else
            {
                uniquestr_sprintf(&valid_set, "%d", test);
            }
            number_of_valids++;
        }
        test = test * 2;
    }

    const char* valid_values_message = "";

    if (number_of_valids == 1)
    {
        uniquestr_sprintf(&valid_values_message, ". Valid value is %s", valid_set);
    }
    else if (number_of_valids > 1)
    {
        uniquestr_sprintf(&valid_values_message, ". Valid values are %s", valid_set);
    }

    fatal_error("Error: KIND %d is not valid for type %s%s\n", kind_size,
            typename, valid_values_message);
}

void fortran_init_kinds(void)
{
    static char already_init = 0;
    if (already_init)
        return;

    already_init = 1;

    // INTEGER
    if (CURRENT_CONFIGURATION->default_integer_kind <= 0)
    {
        CURRENT_CONFIGURATION->default_integer_kind = 4;
    }
    _default_integer_type = fortran_choose_int_type_from_kind(CURRENT_CONFIGURATION->default_integer_kind);
    if (_default_integer_type == NULL)
        wrong_init_kind("INTEGER", CURRENT_CONFIGURATION->default_integer_kind, fortran_choose_int_type_from_kind);

    // REAL
    if (CURRENT_CONFIGURATION->default_real_kind <= 0)
    {
        CURRENT_CONFIGURATION->default_real_kind = 4;
    }
    _default_real_type = fortran_choose_float_type_from_kind(CURRENT_CONFIGURATION->default_real_kind);
    if (_default_real_type == NULL)
        wrong_init_kind("REAL", CURRENT_CONFIGURATION->default_real_kind, fortran_choose_float_type_from_kind);

    // DOUBLE PRECISION
    if (CURRENT_CONFIGURATION->doubleprecision_kind <= 0)
    {
        CURRENT_CONFIGURATION->doubleprecision_kind = 8;
    }
    _doubleprecision_type = fortran_choose_float_type_from_kind(CURRENT_CONFIGURATION->doubleprecision_kind);
    if (_doubleprecision_type == NULL)
        wrong_init_kind("DOUBLE PRECISION", CURRENT_CONFIGURATION->doubleprecision_kind, fortran_choose_float_type_from_kind);

    if (CURRENT_CONFIGURATION->doubleprecision_kind < CURRENT_CONFIGURATION->default_real_kind)
        // This is weird
        fprintf(stderr, "Warning: Setting a KIND for DOUBLE PRECISION smaller than the default REAL\n");

    // LOGICAL
    if (CURRENT_CONFIGURATION->default_logical_kind <= 0)
    {
        CURRENT_CONFIGURATION->default_logical_kind = 4;
    }
    _default_logical_type = fortran_choose_logical_type_from_kind(CURRENT_CONFIGURATION->default_logical_kind);
    if (_default_logical_type == NULL)
        wrong_init_kind("LOGICAL", CURRENT_CONFIGURATION->default_logical_kind, fortran_choose_logical_type_from_kind);

    // CHARACTER
    if (CURRENT_CONFIGURATION->default_character_kind <= 0)
    {
        CURRENT_CONFIGURATION->default_character_kind = 1;
    }
    _default_character_type = fortran_choose_character_type_from_kind(CURRENT_CONFIGURATION->default_character_kind);
    if (_default_character_type == NULL)
        wrong_init_kind("CHARACTER", CURRENT_CONFIGURATION->default_character_kind, fortran_choose_character_type_from_kind);
}


type_t* fortran_get_default_integer_type(void)
{
    return _default_integer_type;
}

type_t* fortran_get_default_real_type(void)
{
    return _default_real_type;
}

type_t* fortran_get_doubleprecision_type(void)
{
    return _doubleprecision_type;
}

type_t* fortran_get_default_logical_type(void)
{
    return _default_logical_type;
}

type_t* fortran_get_default_character_type(void)
{
    return _default_character_type;
}

int fortran_get_default_integer_type_kind(void)
{
    return type_get_size(fortran_get_default_integer_type());
}

int fortran_get_default_real_type_kind(void)
{
    return type_get_size(fortran_get_default_real_type());
}

int fortran_get_doubleprecision_type_kind(void)
{
    return type_get_size(fortran_get_doubleprecision_type());
}

int fortran_get_default_logical_type_kind(void)
{
    return type_get_size(fortran_get_default_logical_type());
}

int fortran_get_default_character_type_kind(void)
{
    return type_get_size(fortran_get_default_character_type());
}

static type_t* choose_type_from_kind_table(
        type_t** type_table, 
        int num_types, 
        int kind_size)
{
    type_t* result = NULL;
    if ((0 < kind_size)
            && (kind_size <= num_types))
    {
        result = type_table[kind_size];
    }

    return result;
}

#define MAX_INT_KIND MCXX_MAX_BYTES_INTEGER
type_t* fortran_choose_int_type_from_kind(int kind_size)
{
    static char int_types_init = 0;
    static type_t* int_types[MAX_INT_KIND + 1] = { 0 };
    if (!int_types_init)
    {
#ifdef HAVE_FORTRAN_KIND16
        int_types[type_get_size(get_signed_int128_type())] = get_signed_int128_type();
#endif
        int_types[type_get_size(get_signed_long_long_int_type())] = get_signed_long_long_int_type();
        int_types[type_get_size(get_signed_long_int_type())] = get_signed_long_int_type();
        int_types[type_get_size(get_signed_int_type())] = get_signed_int_type();
        int_types[type_get_size(get_signed_short_int_type())] = get_signed_short_int_type();
        int_types[type_get_size(get_signed_byte_type())] = get_signed_byte_type();
        int_types_init = 1;
    }
    return choose_type_from_kind_table(int_types, MAX_INT_KIND, kind_size);
}

#define MAX_FLOAT_KIND 16
type_t* fortran_choose_float_type_from_kind(int kind_size)
{
    static char float_types_init = 0;
    static type_t* float_types[MAX_FLOAT_KIND + 1] = { 0 };
    if (!float_types_init)
    {
        int i;
        for (i = 0; i < CURRENT_CONFIGURATION->type_environment->num_float_types; i++)
        {
            float_types[CURRENT_CONFIGURATION->type_environment->all_floats[i]->bits / 8] 
                = get_floating_type_from_descriptor(CURRENT_CONFIGURATION->type_environment->all_floats[i]);
        }
        float_types_init = 1;
    }
    return choose_type_from_kind_table(float_types, MAX_FLOAT_KIND, kind_size);
}

#define MAX_LOGICAL_KIND MCXX_MAX_BYTES_INTEGER
type_t* fortran_choose_logical_type_from_kind(int kind_size)
{
    static char logical_types_init = 0;
    static type_t* logical_types[MAX_LOGICAL_KIND + 1] = { 0 };
    if (!logical_types_init)
    {
#ifdef HAVE_FORTRAN_KIND16
        logical_types[type_get_size(get_signed_int128_type())] = get_bool_of_integer_type(get_signed_int128_type());
#endif
        logical_types[type_get_size(get_signed_long_long_int_type())] = get_bool_of_integer_type(get_signed_long_long_int_type());
        logical_types[type_get_size(get_signed_long_int_type())] = get_bool_of_integer_type(get_signed_long_int_type());
        logical_types[type_get_size(get_signed_int_type())] = get_bool_of_integer_type(get_signed_int_type());
        logical_types[type_get_size(get_signed_short_int_type())] = get_bool_of_integer_type(get_signed_short_int_type());
        logical_types[type_get_size(get_signed_byte_type())] = get_bool_of_integer_type(get_signed_byte_type());
        logical_types_init = 1;
    }
    return choose_type_from_kind_table(logical_types, MAX_LOGICAL_KIND, kind_size);
}

#define MAX_CHARACTER_KIND MCXX_MAX_BYTES_INTEGER
type_t* fortran_choose_character_type_from_kind(int kind_size)
{
    static char character_types_init = 0;
    static type_t* character_types[MAX_CHARACTER_KIND + 1] = { 0 };
    if (!character_types_init)
    {
        character_types[type_get_size(get_char_type())] = get_char_type();
        character_types_init = 1;
    }
    return choose_type_from_kind_table(character_types, MAX_CHARACTER_KIND, kind_size);
}

int fortran_array_type_get_total_number_of_elements(type_t* t)
{
    ERROR_CONDITION(!fortran_is_array_type(t), "This is not a Fortran array type", 0);
    t = advance_over_typedefs(t);

    int number_of_elements = 1;
    while (fortran_is_array_type(t))
    {
        if (array_type_is_unknown_size(t))
        {
            return -1;
        }

        nodecl_t whole_size = array_type_get_array_size_expr(t);
        if (!nodecl_is_constant(whole_size))
        {
            return -1;
        }

        const_value_t * size = nodecl_get_constant(whole_size);
        number_of_elements *= const_value_cast_to_signed_int(size);
        t = array_type_get_element_type(t);
    }

    if (number_of_elements < 0)
        number_of_elements = 0;

    return number_of_elements;
}

char fortran_array_has_zero_size(type_t* array_type)
{
    while (fortran_is_array_type(array_type))
    {
        nodecl_t nodecl_size = array_type_get_array_size_expr(array_type);
        if (nodecl_is_constant(nodecl_size))
        {
            int size = const_value_cast_to_signed_int(nodecl_get_constant(nodecl_size));

            if (size <= 0)
            {
                return 1;
            }

        }

        array_type = array_type_get_element_type(array_type);
    }

    return 0;
}
