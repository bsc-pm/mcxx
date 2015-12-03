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

#ifndef FORTRAN03_INTRINSICS_SIMPLIFY_H
#define FORTRAN03_INTRINSICS_SIMPLIFY_H

#include <math.h>
#include <limits.h>
#include <float.h>
#include <complex.h>

#include "fortran03-cexpr.h"
#include "cxx-typeutils.h"

static const_value_t* compute_binary_elemental(
        const_value_t* cval_a,
        const_value_t* cval_b,
        const_value_t* (*compute)(const_value_t*, const_value_t*))
{
    if (const_value_is_array(cval_a)
            || const_value_is_array(cval_b))
    {
        if (const_value_is_array(cval_a)
                && const_value_is_array(cval_b)
                && const_value_get_num_elements(cval_a) != const_value_get_num_elements(cval_b))
            return NULL;

        int num_elements;
        if (const_value_is_array(cval_a))
            num_elements = const_value_get_num_elements(cval_a);
        else
            num_elements = const_value_get_num_elements(cval_b);

        if (num_elements == 0)
            return const_value_make_array(0, NULL);

        const_value_t* const_vals[num_elements];

        int k;
        for (k = 0; k < num_elements; k++)
        {
            const_value_t* current_val = NULL;
            if (const_value_is_array(cval_a)
                    && const_value_is_array(cval_b))
            {
                current_val = compute_binary_elemental(
                        const_value_get_element_num(cval_a, k),
                        const_value_get_element_num(cval_b, k),
                        compute);
            }
            else if (const_value_is_array(cval_a))
            {
                current_val = compute_binary_elemental(
                        const_value_get_element_num(cval_a, k),
                        cval_b,
                        compute);
            }
            else
            {
                current_val = compute_binary_elemental(
                        cval_a,
                        const_value_get_element_num(cval_b, k),
                        compute);
            }

            if (current_val == NULL)
                return NULL;

            const_vals[k] = current_val;
        }

        return const_value_make_array(num_elements, const_vals);
    }
    else
    {
        return compute(cval_a, cval_b);
    }
}

static nodecl_t nodecl_make_int_literal(int n)
{
    return nodecl_make_integer_literal(fortran_get_default_integer_type(), 
            const_value_get_integer(n, type_get_size(fortran_get_default_integer_type()), 1), 
            make_locus("", 0, 0));
}

static nodecl_t nodecl_make_zero(void)
{
    return nodecl_make_int_literal(0);
}

static nodecl_t nodecl_make_one(void)
{
    return nodecl_make_int_literal(1);
}

static nodecl_t simplify_precision(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = no_ref(nodecl_get_type(x));

    const floating_type_info_t * model = floating_type_get_info(t);

    // In mercurium radix is always 2
    int k = 0;

    int precision = (((model->p - 1) * log10(model->base)) + k);

    return nodecl_make_int_literal(precision);
}

static const_value_t* get_huge_value(type_t* t)
{
    t = fortran_get_rank0_type(t);

    if (is_floating_type(t))
    {
        if (is_float_type(t))
        {
            return const_value_get_float(FLT_MAX);
        }
        else if (is_double_type(t))
        {
            return const_value_get_double(DBL_MAX);
        }
        else if (is_long_double_type(t))
        {
            return const_value_get_long_double(LDBL_MAX);
        }
        else 
        {
#ifdef HAVE_QUADMATH_H
            const floating_type_info_t* floating_info = floating_type_get_info(t);
            if (floating_info->bits == 128)
            {
                return const_value_get_float128(FLT128_MAX);
            }
#endif
        }
    }
    else if (is_integer_type(t))
    {
        return integer_type_get_maximum(t);
    }

    return NULL;
}


static nodecl_t simplify_huge(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = nodecl_get_type(x);

    const_value_t* val = get_huge_value(t);

    if (val != NULL)
    {
        return const_value_to_nodecl(val);
    }

    return nodecl_null();
}

static const_value_t* get_tiny_value(type_t* t)
{
    t = fortran_get_rank0_type(t);

    if (is_floating_type(t))
    {
        if (is_float_type(t))
        {
            return const_value_get_float(FLT_MIN);
        }
        else if (is_double_type(t))
        {
            return const_value_get_double(DBL_MIN);
        }
        else if (is_long_double_type(t))
        {
            return const_value_get_long_double(LDBL_MIN);
        }
        else 
        {
#ifdef HAVE_QUADMATH_H
            const floating_type_info_t* floating_info = floating_type_get_info(t);
            if (floating_info->bits == 128)
            {
                return const_value_get_float128(FLT128_MIN);
            }
#endif
        }
    }
    else if (is_integer_type(t))
    {
        return const_value_get_one(type_get_size(t), 1);
    }

    return NULL;
}

static nodecl_t simplify_tiny(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = nodecl_get_type(x);

    const_value_t* val = get_tiny_value(t);

    if (val != NULL)
    {
        return const_value_to_nodecl(val);
    }

    return nodecl_null();
}

#define MIN(a, b) ((a) < (b) ? (a) : (b))

static nodecl_t simplify_range(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];
    type_t* t = no_ref(nodecl_get_type(x));

    int value = 0;
    if (is_integer_type(t))
    {
        const_value_t* val = get_huge_value(t);
        value = log10( const_value_cast_to_8(val) );
    }
    else if (is_floating_type(t))
    {
        double huge_val = const_value_cast_to_double(get_huge_value(t));
        double tiny_val = const_value_cast_to_double(get_tiny_value(t));

        value = MIN(log10(huge_val), -log10(tiny_val));
    }
    else if (is_complex_type(t))
    {
        // Not yet implemented
        return nodecl_null();
    }

    return nodecl_make_int_literal(value);
}

static nodecl_t simplify_radix(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments UNUSED_PARAMETER)
{
    // Radix is always 2 in our compiler
    return nodecl_make_int_literal(2);
}

static nodecl_t simplify_selected_real_kind(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t p = arguments[0];
    nodecl_t r = arguments[1];
    nodecl_t radix = arguments[2];

    if (nodecl_is_null(p))
        p = nodecl_make_zero();
    if (nodecl_is_null(r))
        r = nodecl_make_zero();
    if (nodecl_is_null(radix))
        radix = nodecl_make_zero();

    if (!nodecl_is_constant(p)
            || !nodecl_is_constant(r)
            || !nodecl_is_constant(radix))
        return nodecl_null();


    uint64_t p_ = const_value_cast_to_8(nodecl_get_constant(p));
    uint64_t r_ = const_value_cast_to_8(nodecl_get_constant(r));
    uint64_t radix_ = const_value_cast_to_8(nodecl_get_constant(radix));

    int num_reals = CURRENT_CONFIGURATION->type_environment->num_float_types;

    int i;
    for (i = 0; i < num_reals; i++)
    {
        type_t* real_type = get_floating_type_from_descriptor(CURRENT_CONFIGURATION->type_environment->all_floats[i]);

        // Reuse other simplification routines. We build a convenience node here
        nodecl_t nodecl_type = nodecl_make_type(real_type, make_locus("", 0, 0));

        nodecl_t precision = simplify_precision(entry, 1, &nodecl_type);
        nodecl_t range = simplify_range(entry, 1, &nodecl_type);
        nodecl_t current_radix = simplify_radix(entry, 1, &nodecl_type);

        uint64_t precision_ = const_value_cast_to_8(nodecl_get_constant(precision));
        uint64_t range_ = const_value_cast_to_8(nodecl_get_constant(range));
        uint64_t current_radix_ = const_value_cast_to_8(nodecl_get_constant(current_radix));

        nodecl_free(nodecl_type);

        if (p_ <= precision_
                && r_ <= range_
                && (radix_ == 0 || radix_ == current_radix_))
        {
            return nodecl_make_int_literal(type_get_size(real_type));
        }
    }

    return nodecl_make_int_literal(-1);
}

static nodecl_t simplify_selected_int_kind(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t r = arguments[0];

    if (!nodecl_is_constant(r))
        return nodecl_null();

    int r_ = const_value_cast_to_4(nodecl_get_constant(r));

    int64_t range = 1;
    int i;
    for (i = 0; i < r_; i++)
    {
        range *= 10;
    }

    const_value_t* c1 = const_value_get_signed_long_long_int(range);
    const_value_t* c2 = const_value_get_signed_long_long_int(-range);

    type_t* t1 = const_value_get_minimal_integer_type(c1);
    type_t* t2 = const_value_get_minimal_integer_type(c2);

    int kind_1 = type_get_size(t1);
    int kind_2 = type_get_size(t2);

    int kind = kind_1 > kind_2 ? kind_1 : kind_2;

    return nodecl_make_int_literal(kind);
}

static nodecl_t simplify_selected_char_kind(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t name = arguments[0];

    if (nodecl_get_kind(name) == NODECL_STRING_LITERAL)
    {
        const char* t = nodecl_get_text(name);

        if ((strcmp(t, "\"ASCII\"") == 0)
                || (strcmp(t, "'ASCII'") == 0)
                // gfortran
                || (strcmp(t, "\"DEFAULT\"") == 0)
                || (strcmp(t, "'DEFAULT'") == 0))
        {
            return nodecl_make_int_literal(1);
        }
        else
        {
            // We do not support anything else
            return nodecl_make_int_literal(-1);
        }
    }

    return nodecl_null();
}

static nodecl_t simplify_bit_size(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t i = arguments[0];

    return nodecl_make_int_literal(type_get_size(no_ref(nodecl_get_type(i))) * 8);
}

static nodecl_t simplify_len(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t str = arguments[0];
    nodecl_t kind = arguments[1];

    type_t* t = fortran_get_rank0_type(no_ref(nodecl_get_type(str)));

    if (array_type_is_unknown_size(t))
        return nodecl_null();

    int kind_ = type_get_size(fortran_get_default_integer_type());

    nodecl_t n = array_type_get_array_size_expr(t);
    if (nodecl_is_constant(n))
    {
        if (!nodecl_is_null(kind))
        {
            if (!nodecl_is_constant(kind))
                return nodecl_null();

            kind_ = const_value_cast_to_4(nodecl_get_constant(kind));
        }

        t = choose_int_type_from_kind(kind, kind_);
        return const_value_to_nodecl_with_basic_type(
                const_value_cast_to_bytes(nodecl_get_constant(n),
                    type_get_size(t), /* sign */ 1),
                t);
    }

    return nodecl_null();
}

static nodecl_t simplify_kind(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];
     
    type_t* t = no_ref(nodecl_get_type(x));
    t = fortran_get_rank0_type(t);

    if (is_complex_type(t))
    {
        t = complex_type_get_base_type(t);
    }
    else if (fortran_is_character_type(t))
    {
        t = array_type_get_element_type(t);
    }

    return nodecl_make_int_literal(type_get_size(t));
}

static nodecl_t simplify_digits(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = no_ref(nodecl_get_type(x));
    t = fortran_get_rank0_type(t);

    if (is_integer_type(t))
    {
        return nodecl_make_int_literal(type_get_size(t) * 8 - 1);
    }
    else if (is_floating_type(t))
    {
        const floating_type_info_t* model = floating_type_get_info(t);

        return nodecl_make_int_literal(model->p + 1);
    }

    return nodecl_null();
}

static nodecl_t simplify_epsilon(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = no_ref(nodecl_get_type(x));
    t = fortran_get_rank0_type(t);

    if (is_float_type(t))
    {
        return nodecl_make_floating_literal(
                get_float_type(),
                const_value_get_float(FLT_EPSILON),
                make_locus("", 0, 0));
    }
    else if (is_double_type(t))
    {
        return nodecl_make_floating_literal(
                get_double_type(),
                const_value_get_double(DBL_EPSILON),
                make_locus("", 0, 0));
    }
    else if (is_long_double_type(t))
    {
        return nodecl_make_floating_literal(
                get_long_double_type(),
                const_value_get_long_double(LDBL_EPSILON),
                make_locus("", 0, 0));
    }

    return nodecl_null();
}

static nodecl_t simplify_maxexponent(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = no_ref(nodecl_get_type(x));
    t = fortran_get_rank0_type(t);

    const floating_type_info_t* model = floating_type_get_info(t);

    return nodecl_make_int_literal(model->emax);
}

static nodecl_t simplify_minexponent(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = no_ref(nodecl_get_type(x));
    t = fortran_get_rank0_type(t);

    const floating_type_info_t* model = floating_type_get_info(t);

    return nodecl_make_int_literal(model->emin);
}

static nodecl_t simplify_xbound(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments,
        nodecl_t (*bound_fun)(type_t*))
{
    nodecl_t array = arguments[0];
    nodecl_t dim = arguments[1];
    nodecl_t kind = arguments[2];

    int kind_ = type_get_size(fortran_get_default_integer_type());
    if (!nodecl_is_null(kind))
    {
        if (!nodecl_is_constant(kind))
            return nodecl_null();

        kind_ = const_value_cast_to_4(nodecl_get_constant(kind));
    }

    type_t* t = no_ref(nodecl_get_type(array));
    if (fortran_is_pointer_to_array_type(t))
        return nodecl_null();

    if (nodecl_is_null(dim))
    {
        int i, rank = fortran_get_rank_of_type(t);
        nodecl_t nodecl_list = nodecl_null();
        for (i = 0; i < rank; i++)
        {
            if (array_type_is_unknown_size(t))
            {
                return nodecl_null();
            }

            nodecl_list = nodecl_concat_lists(
                    nodecl_make_list_1(nodecl_shallow_copy(bound_fun(t))),
                    nodecl_list);

            t = array_type_get_element_type(t);
        }

        nodecl_t result = nodecl_make_structured_value(
                nodecl_list,
                nodecl_null(),
                get_array_type_bounds(choose_int_type_from_kind(kind, kind_),
                    nodecl_make_one(),
                    nodecl_make_int_literal(kind_),
                    CURRENT_COMPILED_FILE->global_decl_context),
                make_locus("", 0, 0));

        if (rank > 0)
        {
            const_value_t* const_vals[rank];

            t = no_ref(nodecl_get_type(array));
            for (i = 0; i < rank; i++)
            {
                nodecl_t bound = nodecl_shallow_copy(bound_fun(t)); 
                if (!nodecl_is_constant(bound))
                    return nodecl_null();

                const_vals[rank - i - 1] = const_value_cast_to_bytes(nodecl_get_constant(bound), kind_, /* signed */ 1);

                t = array_type_get_element_type(t);
            }

            nodecl_set_constant(result, 
                    const_value_make_array(rank, const_vals));
        }

        return result;
    }
    else
    {
        if (nodecl_is_constant(dim))
        {
            int dim_ = const_value_cast_to_4(nodecl_get_constant(dim));

            int rank = fortran_get_rank_of_type(t);

            if ((rank - dim_) < 0)
                return nodecl_null();

            int i;
            for (i = 0; i < (rank - dim_); i++)
            {
                t = array_type_get_element_type(t);
            }

            if (!array_type_is_unknown_size(t))
            {
                nodecl_t bound = bound_fun(t);
                if (!nodecl_is_constant(bound))
                    return nodecl_null();

                t = choose_int_type_from_kind(kind, kind_);
                return const_value_to_nodecl_with_basic_type(
                        const_value_cast_to_bytes(
                            nodecl_get_constant(bound),
                            type_get_size(t),
                            /* sign */ 1),
                        t);
            }
        }
    }

    return nodecl_null();
}

static nodecl_t simplify_lbound(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_xbound(entry, num_arguments, arguments, array_type_get_array_lower_bound);
}

static nodecl_t simplify_ubound(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_xbound(entry, num_arguments, arguments, array_type_get_array_upper_bound);
}

static nodecl_t simplify_size(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t array = arguments[0];
    nodecl_t dim = arguments[1];
    nodecl_t kind = arguments[2];

    int kind_ = type_get_size(fortran_get_default_integer_type());
    if (!nodecl_is_null(kind))
    {
        if (!nodecl_is_constant(kind))
            return nodecl_null();

        kind_ = const_value_cast_to_4(nodecl_get_constant(kind));
    }

    type_t* t = no_ref(nodecl_get_type(array));
    if (fortran_is_pointer_to_array_type(t))
        return nodecl_null();

    if (nodecl_is_null(dim))
    {
        int value = fortran_array_type_get_total_number_of_elements(t);
        if (value == -1)
        {
            return nodecl_null();
        }
        else
        {
            return nodecl_make_integer_literal(
                    choose_int_type_from_kind(kind, kind_),
                    const_value_get_signed_int(value),
                    make_locus("", 0, 0));
        }
    }
    else
    {
        if (nodecl_is_constant(dim))
        {
            int dim_ = const_value_cast_to_4(nodecl_get_constant(dim));

            int rank = fortran_get_rank_of_type(t);

            if ((rank - dim_) < 0)
                return nodecl_null();

            int i;
            for (i = 0; i < (rank - dim_); i++)
            {
                t = array_type_get_element_type(t);
            }

            if (!array_type_is_unknown_size(t))
            {
                nodecl_t n = array_type_get_array_size_expr(t);
                if (nodecl_is_constant(n))
                    return nodecl_shallow_copy(n);
            }
        }
    }

    return nodecl_null();
}

static nodecl_t simplify_shape(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t array = arguments[0];
    nodecl_t kind = arguments[1];

    int kind_ = type_get_size(fortran_get_default_integer_type());
    if (!nodecl_is_null(kind))
    {
        if (!nodecl_is_constant(kind))
            return nodecl_null();

        kind_ = const_value_cast_to_4(nodecl_get_constant(kind));
    }

    nodecl_t nodecl_list = nodecl_null();

    type_t* t = no_ref(nodecl_get_type(array));
    if (fortran_is_pointer_to_array_type(t))
        return nodecl_null();

    int i, rank = fortran_get_rank_of_type(t);
    for (i = 0; i < rank; i++)
    {
        if (array_type_is_unknown_size(t))
        {
            return nodecl_null();
        }

        nodecl_t size = array_type_get_array_size_expr(t);

        // We could do a bit more here
        if (!nodecl_is_constant(size))
            return nodecl_null();

        nodecl_list = nodecl_concat_lists(
                nodecl_make_list_1(nodecl_shallow_copy(size)),
                nodecl_list);

        t = array_type_get_element_type(t);
    }

    nodecl_t result;
    if (rank > 0)
    {
        result = nodecl_make_structured_value(
                nodecl_list,
                nodecl_null(),
                get_array_type_bounds(
                    choose_int_type_from_kind(kind, kind_),
                    nodecl_make_one(),
                    nodecl_make_int_literal(rank),
                    CURRENT_COMPILED_FILE->global_decl_context),
                make_locus("", 0, 0));

        const_value_t* const_vals[rank];

        t = no_ref(nodecl_get_type(array));
        for (i = 0; i < rank; i++)
        {
            nodecl_t size = array_type_get_array_size_expr(t);

            const_vals[rank - i - 1] = nodecl_get_constant(size);

            t = array_type_get_element_type(t);
        }

        nodecl_set_constant(
                result,
                const_value_make_array(rank, const_vals));
    }
    else
    {
        result = nodecl_make_structured_value(
                nodecl_null(),
                nodecl_null(),
                get_array_type_bounds(
                    choose_int_type_from_kind(kind, kind_),
                    nodecl_make_one(),
                    nodecl_make_zero(), 
                    CURRENT_COMPILED_FILE->global_decl_context),
                make_locus("", 0, 0));
    }

    return result;
}

static nodecl_t simplify_max_min(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments,
        const_value_t* (combine)(const_value_t*, const_value_t*))
{
    nodecl_t result = nodecl_null();
    int i;
    for (i = 0; i < num_arguments; i++)
    {
        nodecl_t current_arg = arguments[i];
        if (i == 0)
        {
            if (nodecl_is_constant(current_arg))
            {
                result = current_arg;
            }
            else
            {
                result = nodecl_null();
                break;
            }
        }
        else
        {
            if (nodecl_is_constant(current_arg))
            {
                const_value_t *current_val = nodecl_get_constant(result);

                const_value_t *new_val = nodecl_get_constant(current_arg);

                const_value_t* t = combine(new_val, current_val);

                if (const_value_is_nonzero(t))
                {
                    result = const_value_to_nodecl(const_value_cast_as_another( new_val, current_val ));
                }
            }
            else
            {
                result = nodecl_null();
                break;
            }
        }
    }

    return result;
}

static nodecl_t simplify_max_min_plus_conv(scope_entry_t* entry UNUSED_PARAMETER,
        int num_arguments, nodecl_t* arguments,
        const_value_t* (combine)(const_value_t*, const_value_t*),
        const_value_t* (convert)(const_value_t*))
{
    nodecl_t result = simplify_max_min(entry, num_arguments, arguments, combine);

    if (!nodecl_is_null(result))
    {
        result = const_value_to_nodecl(convert(nodecl_get_constant(result)));
    }

    return result;
}

static nodecl_t simplify_max(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_max_min(entry, num_arguments, arguments, const_value_gt);
}

static nodecl_t simplify_min(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_max_min(entry, num_arguments, arguments, const_value_lt);
}

static nodecl_t simplify_max1(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_max_min_plus_conv(entry, num_arguments, arguments, const_value_gt, const_value_round_to_zero);
}

static nodecl_t simplify_min1(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_max_min_plus_conv(entry, num_arguments, arguments, const_value_lt, const_value_round_to_zero);
}

static nodecl_t simplify_amax0(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_max_min_plus_conv(entry, num_arguments, arguments, const_value_gt, const_value_cast_to_float_value);
}

static nodecl_t simplify_amin0(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_max_min_plus_conv(entry, num_arguments, arguments, const_value_lt, const_value_cast_to_float_value);
}

static nodecl_t simplify_amax1(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_max_min_plus_conv(entry, num_arguments, arguments, const_value_gt, const_value_cast_to_float_value);
}

static nodecl_t simplify_amin1(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_max_min_plus_conv(entry, num_arguments, arguments, const_value_lt, const_value_cast_to_float_value);
}

static nodecl_t simplify_dmax1(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_max_min_plus_conv(entry, num_arguments, arguments, const_value_gt, const_value_cast_to_double_value);
}

static nodecl_t simplify_dmin1(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_max_min_plus_conv(entry, num_arguments, arguments, const_value_lt, const_value_cast_to_double_value);
}

static nodecl_t simplify_max0(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_max_min_plus_conv(entry, num_arguments, arguments, const_value_gt, const_value_cast_to_signed_int_value);
}

static nodecl_t simplify_min0(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_max_min_plus_conv(entry, num_arguments, arguments, const_value_lt, const_value_cast_to_signed_int_value);
}

static nodecl_t simplify_int(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t arg = arguments[0];
    nodecl_t arg_kind = arguments[1];

    int kind = fortran_get_default_integer_type_kind();
    if (!nodecl_is_null(arg_kind))
    {
        kind = const_value_cast_to_4(nodecl_get_constant(arg_kind));
    }

    if (!nodecl_is_constant(arg))
        return nodecl_null();

    const_value_t* v = nodecl_get_constant(arg);

    if (const_value_is_integer(v))
    {
        return nodecl_make_integer_literal(
                choose_int_type_from_kind(arg, kind),
                const_value_cast_to_bytes(v, kind, 1),
                make_locus("", 0, 0));
    }
    else if (const_value_is_floating(v))
    {
        return nodecl_make_integer_literal(
                choose_int_type_from_kind(arg, kind),
                const_value_round_to_zero_bytes(v, kind),
                make_locus("", 0, 0));
    }

    return nodecl_null();
}

static nodecl_t simplify_real(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t arg = arguments[0];
    nodecl_t arg_kind = arguments[1];

    if (nodecl_is_constant(arg))
    {
        const_value_t* value = nodecl_get_constant(arg);

        int kind = 0;
        if (!nodecl_is_null(arg_kind))
        {
            ERROR_CONDITION(!nodecl_is_constant(arg_kind), "Kind must be constant here", 0);
            const_value_t* kind_value = nodecl_get_constant(arg_kind);
            kind = const_value_cast_to_4(kind_value);
        }
        else
        {
            kind = fortran_get_default_real_type_kind();
        }

        type_t* float_type = choose_float_type_from_kind(arg_kind, kind);

        if (const_value_is_complex(value))
        {
            value = const_value_complex_get_real_part(value);
        }

        if (is_float_type(float_type))
        {
            return const_value_to_nodecl(const_value_cast_to_float_value(value));
        }
        else if (is_double_type(float_type))
        {
            return const_value_to_nodecl(const_value_cast_to_double_value(value));
        }
        else if (is_long_double_type(float_type))
        {
            return const_value_to_nodecl(const_value_cast_to_long_double_value(value));
        }
        else
        {
            fatal_error("Invalid floating type");
        }
    }

    return nodecl_null();
}

static nodecl_t simplify_float(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t argument_list[2] = { arguments[0], 
        const_value_to_nodecl(const_value_get_signed_int(fortran_get_default_real_type_kind())) }; 
    return simplify_real(entry, 2, argument_list);
}

static nodecl_t simplify_dble(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t argument_list[2] = { arguments[0], 
        const_value_to_nodecl(const_value_get_signed_int(fortran_get_doubleprecision_type_kind())) }; 
    return simplify_real(entry, 2, argument_list);
}

static nodecl_t simplify_cmplx(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t arg_real = arguments[0];
    nodecl_t arg_imag = arguments[1];
    nodecl_t arg_kind = arguments[2];

    if (nodecl_is_constant(arg_real)
            && nodecl_is_constant(arg_imag))
    {
        const_value_t* value_real = nodecl_get_constant(arg_real);
        const_value_t* value_imag = nodecl_get_constant(arg_imag);

        int kind = 0;
        if (!nodecl_is_null(arg_kind))
        {
            ERROR_CONDITION(!nodecl_is_constant(arg_kind), "Kind must be constant here", 0);
            const_value_t* kind_value = nodecl_get_constant(arg_kind);
            kind = const_value_cast_to_4(kind_value);
        }
        else
        {
            kind = fortran_get_default_real_type_kind();
        }

        type_t* float_type = choose_float_type_from_kind(arg_kind, kind);

        if (is_float_type(float_type))
        {
            return const_value_to_nodecl(
                    const_value_make_complex(
                        const_value_cast_to_float_value(value_real),
                        const_value_cast_to_float_value(value_imag))
                    );
        }
        else if (is_double_type(float_type))
        {
            return const_value_to_nodecl(
                    const_value_make_complex(
                        const_value_cast_to_double_value(value_real),
                        const_value_cast_to_double_value(value_imag))
                    );
        }
        else if (is_long_double_type(float_type))
        {
            return const_value_to_nodecl(
                    const_value_make_complex(
                        const_value_cast_to_long_double_value(value_real),
                        const_value_cast_to_long_double_value(value_imag))
                    );
        }
        else
        {
            fatal_error("Invalid floating type");
        }
    }

    return nodecl_null();
}

static nodecl_t simplify_dcmplx(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t argument_list[3] = { arguments[0], arguments[1],
        const_value_to_nodecl(const_value_get_signed_int(fortran_get_doubleprecision_type_kind())) }; 

    return simplify_cmplx(entry, 3, argument_list);
}

static nodecl_t simplify_char(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    if (nodecl_is_constant(arguments[0]))
    {
        const_value_t* cval = nodecl_get_constant(arguments[0]);
        const_value_t* result = NULL;

        if (const_value_is_array(cval))
        {
            int N = const_value_get_num_elements(cval);
            const_value_t* values[N + 1];

            int i;
            for (i = 0; i < N; i++)
            {
                char c = const_value_cast_to_1(const_value_get_element_num(cval, i));
                values[i] = const_value_make_string(&c, 1);
            }

            result = const_value_make_array(N, values);
        }
        else
        {
            char c = const_value_cast_to_1(nodecl_get_constant(arguments[0]));
            result = const_value_make_string(&c, 1);
        }
        return const_value_to_nodecl(result);
    }

    return nodecl_null();
}

static nodecl_t simplify_achar(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    return simplify_char(entry, num_arguments, arguments);
}

static void compute_factors_of_array_indexing(
        int N,
        int* shape, 
        int* factors)
{
    int i;
    for (i = 0; i < N; i++)
    {
        factors[i] = 1;
        int j;
        for (j = 0; j < i; j++)
        {
            factors[i] = factors[i] * shape[j];
        }
    }
}

#if 0
static void determine_array_subscript_of_lineal_index(
        int N,
        int* factors,

        int index_,

        int* subscript
        )
{
    int i;
    int val = index_;
    for (i = N - 1; i > 1; i--)
    {
        subscript[i] = val / factors[i];
        val = val % factors[i];
    }

    subscript[0] = val;
}
#endif

static void permute_subscript(
        int N,
        int *subscript,
        int *permutation,

        int *out
        )
{
    int i;
    for (i = 0; i < N; i++)
    {
        out[permutation[i]] = subscript[i];
    }
}

static void determine_lineal_index_of_array_subscript(
        int N,
        int *subscript,

        int *factors,

        int *index_)
{
    (*index_) = 0;

    int i;
    for (i = 0; i < N; i++)
    {
        (*index_) = (*index_) + factors[i] * subscript[i];
    }
}

static const_value_t* reshape_array_from_flattened_rec(
        int N,
        int rank,
        int* shape,
        int* factors,
        int* subscript,
        
        const_value_t* flattened_array,
        const_value_t* flattened_pad
        )
{
    if (rank == N)
    {
        int index_ = 0;

        determine_lineal_index_of_array_subscript(N, subscript, factors, &index_);

        // int i;
        // fprintf(stderr, "(");
        // for (i = 0; i < N; i++)
        // {
        //     if (i > 0)
        //         fprintf(stderr, ", ");
        //     fprintf(stderr, "%d", subscript[i]);
        // }
        // fprintf(stderr, ") -> %d\n", index_);

        if (index_ < 0)
            return NULL;

        if (index_ >= const_value_get_num_elements(flattened_array))
        {
            if (flattened_pad == NULL)
                return NULL;
            else
            {
                int start = index_ - const_value_get_num_elements(flattened_array);
                const_value_t* result = const_value_get_element_num(flattened_pad, start % const_value_get_num_elements(flattened_pad));
                return result;
            }
        }

        const_value_t* value = const_value_get_element_num(flattened_array, index_);

        return value;
    }
    else
    {
        int i;
        int current_rank = N - rank - 1;
        subscript[current_rank] = 0;
        int size = shape[current_rank];
        const_value_t* result[size];
        for (i = 0; i < size; i++)
        {
            result[i] = reshape_array_from_flattened_rec(
                    N,
                    rank + 1,
                    shape,
                    factors,
                    subscript,
                    
                    flattened_array,
                    flattened_pad
                    );

            subscript[current_rank]++;
        }

        return const_value_make_array(size, result);
    }
}

static const_value_t* reshape_array_from_flattened(
         const_value_t* flattened_array, 
         const_value_t* const_val_shape,
         const_value_t* flattened_pad,
         const_value_t* order
         )
{
    int N = const_value_get_num_elements(const_val_shape);
    int shape[N];

    int i;
    for (i = 0; i < N; i++)
    {
        shape[i] = const_value_cast_to_signed_int(const_value_get_element_num(const_val_shape, i));
    }


    int permutation[N];
    if (order == NULL)
    {
        for (i = 0; i < N; i++)
        {
            permutation[i] = i;
        }
    }
    else
    {
        for (i = 0; i < N; i++)
        {
            permutation[i] = const_value_cast_to_signed_int(const_value_get_element_num(order, i)) - 1;
        }
    }

    // We first have to permute the shape to get the proper factors
    int temp_shape[N];
    permute_subscript(N, shape, permutation, temp_shape);

    int temp_factors[N];
    compute_factors_of_array_indexing(N, temp_shape, temp_factors);

    // But factors appear in the given shape order, so we have to permute them as well
    int factors[N];
    permute_subscript(N, temp_factors, permutation, factors);

    int subscript[N];
    memset(subscript, 0, sizeof(subscript));

#if 0
    fprintf(stderr, "RESHAPE!!!\n");
#define PRINT_ARRAY(x) \
        fprintf(stderr, "%s = (", #x); \
        for (i = 0; i < N; i++) \
        { \
            if (i > 0) \
                fprintf(stderr, ", "); \
            fprintf(stderr, "%d", x[i]); \
        } \
        fprintf(stderr, ")\n"); 
    PRINT_ARRAY(shape)
    PRINT_ARRAY(factors)
    PRINT_ARRAY(permutation)
#endif

    return reshape_array_from_flattened_rec(
            N,
            /* rank */ 0,
            shape,
            factors,
            subscript,

            flattened_array,
            flattened_pad
            );
}

static nodecl_t simplify_reshape(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    if (nodecl_is_constant(arguments[0])
            && nodecl_is_constant(arguments[1])
            && (nodecl_is_null(arguments[2]) || nodecl_is_constant(arguments[2]))
            && (nodecl_is_null(arguments[3]) || nodecl_is_constant(arguments[3])))
    {
        const_value_t* shape = nodecl_get_constant(arguments[1]);

        const_value_t* pad = NULL;
        if (!nodecl_is_null(arguments[2]))
            pad = nodecl_get_constant(arguments[2]);
        const_value_t* order = NULL;
        if (!nodecl_is_null(arguments[3]))
            order = nodecl_get_constant(arguments[3]);

        type_t *base_type = fortran_get_rank0_type(nodecl_get_type(arguments[0]));

        const_value_t* flattened_source = fortran_flatten_array(nodecl_get_constant(arguments[0]));
        const_value_t* flattened_pad = NULL;
        if (pad != NULL)
        {
            flattened_pad = fortran_flatten_array(pad);
        }

        const_value_t* val = reshape_array_from_flattened(
                flattened_source,
                shape,
                flattened_pad,
                order
                );
        if (val == NULL)
            return nodecl_null();

        nodecl_t result = const_value_to_nodecl_with_basic_type(val, base_type);

        return result;
    }
    return nodecl_null();
}

static nodecl_t simplify_repeat(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    if (!nodecl_is_constant(arguments[0])
            || !nodecl_is_constant(arguments[1]))
        return nodecl_null();

    const_value_t* string_cval = nodecl_get_constant(arguments[0]);
    const_value_t* repeat_cval = nodecl_get_constant(arguments[1]);

    if (const_value_is_negative(repeat_cval))
        return nodecl_null();

    const_value_t* result = const_value_make_string("", 0);
    int repeat = const_value_cast_to_signed_int(repeat_cval);
    int i;
    for (i = 1; i <= repeat; i++)
    {
        result = const_value_string_concat(result, string_cval);
    }

    return const_value_to_nodecl(result);
}

typedef int index_info_t[MCXX_MAX_ARRAY_SPECIFIER];

static const_value_t* reduce_for_a_given_dimension(
        const_value_t* original_array, 
        const_value_t* mask, 
        int num_dimensions,
        int reduced_dimension,
        index_info_t index_info,
        const_value_t* (*combine)(const_value_t* a, const_value_t* b),
        const_value_t* neuter)
{
    const_value_t* result = NULL;

    const_value_t* considered_array = original_array;
    const_value_t* considered_mask_array = mask;

    int i = 0;
    for (i = 0; i < reduced_dimension - 1; i++)
    {
        considered_array = const_value_get_element_num(considered_array, 
                index_info[i]);

        if (considered_mask_array != NULL)
        {
            considered_mask_array = const_value_get_element_num(considered_mask_array, 
                index_info[i]);
        }
    }

    // Now we are in the reduced dimension
    int N = const_value_get_num_elements(considered_array);
    for (i = 0; i < N; i++)
    {
        const_value_t* current_value = const_value_get_element_num(considered_array, i);

        const_value_t* current_mask = NULL;
        if (considered_mask_array != NULL)
        {
            current_mask = const_value_get_element_num(considered_mask_array, i);
        }

        int j;
        for (j = reduced_dimension; j < num_dimensions; j++)
        {
            current_value = const_value_get_element_num(current_value, index_info[j]);

            if (current_mask != NULL)
            {
                current_mask = const_value_get_element_num(current_mask, index_info[j]);
            }
        }

        if (current_mask == NULL
                || const_value_is_nonzero(current_mask))
        {
            if (result == NULL)
            {
                result = current_value;
            }
            else
            {
                result = combine(result, current_value);
            }
        }
    }

    if (result == NULL)
    {
        result = neuter;
    }

    return result;
}


static const_value_t* reduce_recursively(
        const_value_t* original_array, 
        const_value_t* mask, 
        int reduced_dimension,
        int num_dimensions,
        index_info_t index_info,
        const_value_t* (*combine)(const_value_t* a, const_value_t* b),
        const_value_t* neuter,

        const_value_t* current_array, 
        int current_dimension)
{
    if ((current_dimension + 1) < num_dimensions)
    {
        int i, N = const_value_get_num_elements(current_array);

        const_value_t* tmp[N];

        for (i = 0; i < N; i++)
        {
            index_info[current_dimension] = i;

            tmp[i] = reduce_recursively(original_array,
                    mask,
                    reduced_dimension,
                    num_dimensions,
                    index_info,
                    combine,
                    neuter,

                    const_value_get_element_num(current_array, i),
                    current_dimension + 1);

            // Early return if this is the reduced dimension
            if ((current_dimension + 1) == reduced_dimension)
            {
                return tmp[i];
            }
        }

        return const_value_make_array(N, tmp);
    }
    else if ((current_dimension + 1) == num_dimensions)
    {
        int i, N = const_value_get_num_elements(current_array);
        const_value_t* tmp[N];

        for (i = 0; i < N; i++)
        {
            index_info[current_dimension] = i;

            tmp[i] = reduce_for_a_given_dimension(original_array, mask, num_dimensions, reduced_dimension, index_info, combine, neuter);

            // Early return if this is the reduced dimension
            if ((current_dimension + 1) == reduced_dimension)
            {
                return tmp[i];
            }
        }

        return const_value_make_array(N, tmp);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

static const_value_t* simplify_maxminval_aux(scope_entry_t* entry UNUSED_PARAMETER, 
        const_value_t* array_constant,
        const_value_t* dim_constant,
        const_value_t* mask_constant,
        int num_dimensions,
        const_value_t* (*combine)(const_value_t* a, const_value_t* b),
        const_value_t* neuter
        )
{
    // no DIM=
    if ((dim_constant == NULL)
            // or rank 1
            || (num_dimensions == 1))
    {
        // Case 1) Reduce all values into a scalar
        const_value_t* values = fortran_flatten_array_with_mask(array_constant, mask_constant);
        int num_values = const_value_get_num_elements(values);
        if (num_values > 0)
        {
            const_value_t* reduced_val = const_value_get_element_num(values, 0);
            int i;
            for (i = 1; i < num_values; i++)
            {
                const_value_t* current_val = const_value_get_element_num(values, i);
                reduced_val = combine(reduced_val, current_val);
            }

            return reduced_val;
        }
        else
        {
            // Degenerated case
            return neuter;
        }
    }
    else
    {
        // Case 2) Multidimensional reduction
        // Recursively traverse all elements
        //
        index_info_t index_info;
        memset(index_info, 0, sizeof(index_info));

        int reduced_dim = const_value_cast_to_signed_int(dim_constant);

        // This is in fortran order, but we internally use C order +1
        reduced_dim = num_dimensions - reduced_dim + 1;

        return reduce_recursively(
                array_constant,
                mask_constant,
                reduced_dim,
                num_dimensions,
                index_info,
                combine,
                neuter,

                array_constant,
                0);
    }
}

static nodecl_t simplify_maxminval(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, 
        nodecl_t* arguments,
        int num_dimensions,
        const_value_t* (*combine)(const_value_t* a, const_value_t* b),
        const_value_t* neuter
        )
{
    nodecl_t array;
    nodecl_t dim;
    nodecl_t mask;
    if (num_arguments == 2)
    {
        array = arguments[0];
        dim = nodecl_null();
        mask = arguments[1];
    }
    else if (num_arguments == 3)
    {
        array = arguments[0];
        dim = arguments[1];
        mask = arguments[2];
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    if (!nodecl_is_constant(array)
            || (!nodecl_is_null(dim) && !nodecl_is_constant(dim))
            || (!nodecl_is_null(mask) && !nodecl_is_constant(mask)))
        return nodecl_null();

    const_value_t* v = simplify_maxminval_aux(
            entry,
            nodecl_get_constant(array),
            nodecl_is_null(dim) ? NULL : nodecl_get_constant(dim),
            nodecl_is_null(mask) ? NULL : nodecl_get_constant(mask),
            num_dimensions,
            combine,
            neuter);

    if (v == NULL)
        return nodecl_null();

    return const_value_to_nodecl(v);
}

static const_value_t* const_value_compute_max(const_value_t* a, const_value_t* b)
{
    // a > b
    if (const_value_is_nonzero(const_value_gt(a, b)))
    {
        return a;
    }
    else
    {
        return b;
    }
}

static const_value_t* get_max_neuter_for_type(type_t* t)
{
    if (is_integer_type(t))
    {
        return integer_type_get_minimum(t);
    }
    else if (is_floating_type(t))
    {
        return get_huge_value(t);
    }
    else if (fortran_is_character_type(t))
    {
        nodecl_t nodecl_size = array_type_get_array_size_expr(t);
        const_value_t* size_constant = nodecl_get_constant(nodecl_size);
        ERROR_CONDITION(size_constant == NULL, "This should not happen", 0);

        int size = const_value_cast_to_signed_int(size_constant);
        ERROR_CONDITION(size <= 0, "This should not happen", 0);

        const_value_t* values[size];

        const_value_t* zero = const_value_get_zero(type_get_size(array_type_get_element_type(t)), 1);

        int i;
        for (i = 0; i < size; i++)
        {
            values[i] = zero;
        }

        return const_value_make_string_from_values(size, values);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

static nodecl_t simplify_maxval(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    nodecl_t array = arguments[0];

    type_t* array_type = no_ref(nodecl_get_type(array));
    type_t* element_type = fortran_get_rank0_type(array_type);
    int num_dimensions = fortran_get_rank_of_type(array_type);

    return simplify_maxminval(
            entry,
            num_arguments, arguments,
            num_dimensions,
            const_value_compute_max,
            get_max_neuter_for_type(element_type));
}

static const_value_t* const_value_compute_min(const_value_t* a, const_value_t* b)
{
    // a < b
    if (const_value_is_nonzero(const_value_lt(a, b)))
    {
        return a;
    }
    else
    {
        return b;
    }
}

static const_value_t* get_min_neuter_for_type(type_t* t)
{
    // We do not use integer_type_get_minimum because fortran does not use two's complement
    if (is_integer_type(t))
    {
        return integer_type_get_maximum(t);
    }
    else if (is_floating_type(t))
    {
        return get_huge_value(t);
    }
    else if (fortran_is_character_type(t))
    {
        nodecl_t nodecl_size = array_type_get_array_size_expr(t);
        const_value_t* size_constant = nodecl_get_constant(nodecl_size);
        ERROR_CONDITION(size_constant == NULL, "This should not happen", 0);

        int size = const_value_cast_to_signed_int(size_constant);
        ERROR_CONDITION(size <= 0, "This should not happen", 0);

        const_value_t* values[size];

        const_value_t* zero = integer_type_get_maximum(array_type_get_element_type(t));

        int i;
        for (i = 0; i < size; i++)
        {
            values[i] = zero;
        }

        return const_value_make_string_from_values(size, values);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

static nodecl_t simplify_minval(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    nodecl_t array = arguments[0];

    type_t* array_type = no_ref(nodecl_get_type(array));
    type_t* element_type = fortran_get_rank0_type(array_type);
    int num_dimensions = fortran_get_rank_of_type(array_type);

    return simplify_maxminval(
            entry,
            num_arguments, arguments,
            num_dimensions,
            const_value_compute_min,
            get_min_neuter_for_type(element_type));
}

static const_value_t* compute_abs(const_value_t* cval)
{
    // Array case
    if (const_value_is_array(cval))
    {
        int i, N = const_value_get_num_elements(cval);
        const_value_t* array[N];

        for (i = 0; i < N; i++)
        {
            array[i] = compute_abs ( const_value_get_element_num(cval, i) );
            if (array[i] == NULL)
                return NULL;
        }

        return const_value_make_array(N, array);
    }

    if (const_value_is_integer(cval)
            || const_value_is_floating(cval))
    {
        if (const_value_is_negative(cval))
        {
            cval = const_value_neg(cval);
        }
        return cval;
    }
    else if (const_value_is_complex(cval))
    {
        const_value_t* real_part = const_value_complex_get_real_part(cval);
        const_value_t* imag_part = const_value_complex_get_imag_part(cval);

        const_value_t* result = 
            const_value_sqrt(
                    const_value_add(
                        const_value_square(real_part),
                        const_value_square(imag_part)));

        return result;
    }
    else 
        return NULL;
}

static nodecl_t simplify_abs(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    const_value_t* abs_value = compute_abs(cval);
    if (abs_value == NULL)
        return nodecl_null();

    return const_value_to_nodecl(abs_value);
}

static const_value_t* common_real_function_1(
        const_value_t* cval, 
        char domain_check(const_value_t*),
        float funf(float),
        double fun(double),
        long double funl(long double),
#ifdef HAVE_QUADMATH_H
        __float128 funq(__float128),
#else
        void *funq UNUSED_PARAMETER,
#endif
        _Complex float cfunf(_Complex float),
        _Complex double cfun(_Complex double),
        _Complex long double cfunl(_Complex long double),
#ifdef HAVE_QUADMATH_H
        __complex128 cfunq(__complex128)
#else
        void *cfunq UNUSED_PARAMETER
#endif
         )
{
    // Array case
    if (const_value_is_array(cval))
    {
        int i, N = const_value_get_num_elements(cval);
        const_value_t* array[N];

        for (i = 0; i < N; i++)
        {
            array[i] = common_real_function_1(
                    const_value_get_element_num(cval, i),
                    domain_check,
                    funf, fun, funl, funq,
                    cfunf, cfun, cfunl, cfunq);

            if (array[i] == NULL)
                return NULL;
        }

        return const_value_make_array(N, array);
    }

    if (domain_check != NULL
            && !domain_check(cval))
        return NULL;

    if (funf != NULL && const_value_is_float(cval))
    {
        return const_value_get_float( funf(const_value_cast_to_float(cval)) );
    }
    else if (fun != NULL && const_value_is_double(cval))
    {
        return const_value_get_double( fun(const_value_cast_to_double(cval)) );
    }
    else if (funl != NULL && const_value_is_long_double(cval))
    {
        return const_value_get_long_double( funl(const_value_cast_to_long_double(cval)) );
    }
#ifdef HAVE_QUADMATH_H
    else if (funq != NULL && const_value_is_float128(cval))
    {
        return const_value_get_float128( funq(const_value_cast_to_float128(cval)) );
    }
#endif
    else if (const_value_is_complex(cval))
    {
        const_value_t* rval = const_value_complex_get_real_part(cval);
        if (cfunf != NULL && const_value_is_float(rval))
        {
            return const_value_get_complex_float( cfunf(const_value_cast_to_complex_float(cval)) );
        }
        else if (cfun != NULL && const_value_is_double(rval))
        {
            return const_value_get_complex_double( cfun(const_value_cast_to_complex_double(cval)) );
        }
        else if (cfunl != NULL && const_value_is_long_double(rval))
        {
            return const_value_get_complex_long_double( cfunl(const_value_cast_to_complex_long_double(cval)) );
        }
#ifdef HAVE_QUADMATH_H
        else if (cfunq != NULL && const_value_is_float128(rval))
        {
            return const_value_get_complex_float128( cfunq(const_value_cast_to_complex_float128(cval)) );
        }
#endif
    }

    return NULL;
}

static nodecl_t common_real_function_1_to_nodecl(
        const_value_t* cval, 
        char domain_check(const_value_t*),
        float funf(float),
        double fun(double),
        long double funl(long double),
#ifdef HAVE_QUADMATH_H
        __float128 funq(__float128),
#else
        void *funq,
#endif
        _Complex float cfunf(_Complex float),
        _Complex double cfun(_Complex double),
        _Complex long double cfunl(_Complex long double),
#ifdef HAVE_QUADMATH_H
        __complex128 cfunq(__complex128)
#else
        void *cfunq
#endif
         )
{
    const_value_t* result = common_real_function_1(
            cval,
            domain_check,
            funf,
            fun,
            funl,
            funq,

            cfunf,
            cfun,
            cfunl,
            cfunq);

    if (result == NULL)
        return nodecl_null();

    return const_value_to_nodecl(result);
}

static char abs_value_is_lte_1(const_value_t* cval)
{
    const_value_t* cval_abs = cval;
    if (!const_value_is_complex(cval))
    {
        if (const_value_is_negative(cval))
        {
            cval_abs = const_value_neg(cval);
        }
    }
    else
    {
        const_value_t* real_part = const_value_complex_get_real_part(cval);
        const_value_t* imag_part = const_value_complex_get_imag_part(cval);

        cval_abs = const_value_sqrt(
                const_value_add(
                    const_value_square(real_part),
                    const_value_square(imag_part)));
    }

    return !const_value_is_zero(
            const_value_lte(cval_abs, const_value_get_float(1.0f))
            );
}

static nodecl_t simplify_acos(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            abs_value_is_lte_1,
            acosf,
            acos,
            acosl,
#ifdef HAVE_QUADMATH_H
            acosq,
#else
            NULL,
#endif
            cacosf,
            cacos,
#ifdef HAVE_CACOSL
            cacosl,
#else
			NULL,
#endif
#ifdef HAVE_QUADMATH_H
            cacosq
#else
            NULL
#endif
            );

}

static nodecl_t simplify_acosh(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            abs_value_is_lte_1,
            acoshf,
            acosh,
            acoshl,
#ifdef HAVE_QUADMATH_H
            acoshq,
#else
            NULL,
#endif
            cacoshf,
            cacosh,
#ifdef HAVE_CACOSHL
            cacoshl,
#else
			NULL,
#endif
#ifdef HAVE_QUADMATH_H
            cacoshq
#else
            NULL
#endif
            );
}

static nodecl_t simplify_aimag(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return const_value_to_nodecl(const_value_complex_get_imag_part(cval));
}

static const_value_t* compute_aint(const_value_t* cval, type_t* floating_type)
{
    // Array case
    if (const_value_is_array(cval))
    {
        int i, N = const_value_get_num_elements(cval);
        const_value_t* array[N];

        for (i = 0; i < N; i++)
        {
            array[i] = compute_aint ( const_value_get_element_num(cval, i), floating_type );
            if (array[i] == NULL)
                return NULL;
        }

        return const_value_make_array(N, array);
    }

    return const_value_cast_to_floating_type_value(const_value_round_to_zero( cval ), floating_type);
}

static nodecl_t simplify_aint(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    nodecl_t arg = arguments[0];
    nodecl_t kind = nodecl_null();
    if (num_arguments == 2)
    {
        kind = arguments[1];
    }

    if (!nodecl_is_constant(arg)
            || (!nodecl_is_null(kind) && !nodecl_is_constant(kind)))
        return nodecl_null();

    int kind_ = type_get_size(no_ref(nodecl_get_type(arg)));
    if (!nodecl_is_null(kind))
    {
        kind_ = const_value_cast_to_signed_int(nodecl_get_constant(kind));
    }

    type_t* floating_type = choose_float_type_from_kind(kind, kind_);
    const_value_t* float_value = compute_aint(nodecl_get_constant(arg), floating_type);

    return const_value_to_nodecl_with_basic_type(
            float_value,
            floating_type);
}

static const_value_t* compute_anint(const_value_t* cval, type_t* floating_type)
{
    // Array case
    if (const_value_is_array(cval))
    {
        int i, N = const_value_get_num_elements(cval);
        const_value_t* array[N];

        for (i = 0; i < N; i++)
        {
            array[i] = compute_anint ( const_value_get_element_num(cval, i), floating_type );
            if (array[i] == NULL)
                return NULL;
        }

        return const_value_make_array(N, array);
    }

    return const_value_cast_to_floating_type_value(const_value_round_to_nearest( cval ), 
            floating_type);
}

static nodecl_t simplify_anint(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    nodecl_t arg = arguments[0];
    nodecl_t kind = nodecl_null();
    if (num_arguments == 2)
    {
        kind = arguments[1];
    }

    if (!nodecl_is_constant(arg)
            || (!nodecl_is_null(kind) && !nodecl_is_constant(kind)))
        return nodecl_null();

    int kind_ = type_get_size(no_ref(nodecl_get_type(arg)));
    if (!nodecl_is_null(kind))
    {
        kind_ = const_value_cast_to_signed_int(nodecl_get_constant(kind));
    }

    type_t* floating_type = choose_float_type_from_kind(kind, kind_);
    const_value_t* float_value = compute_anint(nodecl_get_constant(arg), floating_type);

    return const_value_to_nodecl_with_basic_type(
            float_value,
            floating_type);
}

static nodecl_t simplify_asin(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            abs_value_is_lte_1,
            asinf,
            asin,
            asinl,
#ifdef HAVE_QUADMATH_H
            asinq,
#else
            NULL,
#endif
            casinf,
            casin,
#ifdef HAVE_CASINL
            casinl,
#else
			NULL,
#endif
#ifdef HAVE_QUADMATH_H
            casinq
#else
            NULL
#endif
            );

}

static nodecl_t simplify_asinh(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            abs_value_is_lte_1,
            asinhf,
            asinh,
            asinhl,
#ifdef HAVE_QUADMATH_H
            asinhq,
#else
            NULL,
#endif
            casinhf,
            casinh,
#ifdef HAVE_CASINHL
            casinhl,
#else
            NULL,
#endif
#ifdef HAVE_QUADMATH_H
            casinhq
#else
            NULL
#endif
            );
}

static nodecl_t simplify_atan(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            /* domain_check */ NULL,
            atanf,
            atan,
            atanl,
#ifdef HAVE_QUADMATH_H
            atanq,
#else
            NULL,
#endif
            catanf,
            catan,
#ifdef HAVE_CATANL
            catanl,
#else
			NULL,
#endif
#ifdef HAVE_QUADMATH_H
            catanq
#else
            NULL
#endif
            );
}

static nodecl_t simplify_atanh(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            /* domain_check */ NULL,
            atanhf,
            atanh,
            atanhl,
#ifdef HAVE_QUADMATH_H
            atanhq,
#else
            NULL,
#endif
            catanhf,
            catanh,
#ifdef HAVE_CATANHL
            catanhl,
#else
			NULL,
#endif
#ifdef HAVE_QUADMATH_H
            catanhq
#else
            NULL
#endif
            );
}

static nodecl_t simplify_cos(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            /* domain_check */ NULL,
            cosf,
            cos,
            cosl,
#ifdef HAVE_QUADMATH_H
            cosq,
#else
            NULL,
#endif
            ccosf,
            ccos,
#ifdef HAVE_CCOSL
            ccosl,
#else
			NULL,
#endif
#ifdef HAVE_QUADMATH_H
            ccosq
#else
            NULL
#endif
            );
}

static nodecl_t simplify_cosh(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            /* domain_check */ NULL,
            coshf,
            cosh,
            coshl,
#ifdef HAVE_QUADMATH_H
            coshq,
#else
            NULL,
#endif
            ccoshf,
            ccosh,
#ifdef HAVE_CCOSHL
            ccoshl,
#else
			NULL,
#endif
#ifdef HAVE_QUADMATH_H
            ccoshq
#else
            NULL
#endif
            );
}

static nodecl_t simplify_sin(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            /* domain_check */ NULL,
            sinf,
            sin,
            sinl,
#ifdef HAVE_QUADMATH_H
            sinq,
#else
            NULL,
#endif
            csinf,
            csin,
#ifdef HAVE_CSINL
            csinl,
#else
			NULL,
#endif
#ifdef HAVE_QUADMATH_H
            csinq
#else
            NULL
#endif
            );
}

static nodecl_t simplify_sinh(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            /* domain_check */ NULL,
            sinhf,
            sinh,
            sinhl,
#ifdef HAVE_QUADMATH_H
            sinhq,
#else
            NULL,
#endif
            csinhf,
            csinh,
#ifdef HAVE_CSINHL
            csinhl,
#else
			NULL,
#endif
#ifdef HAVE_QUADMATH_H
            csinhq
#else
            NULL
#endif
            );
}

static nodecl_t simplify_tan(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            /* domain_check */ NULL,
            tanf,
            tan,
            tanl,
#ifdef HAVE_QUADMATH_H
            tanq,
#else
            NULL,
#endif
            ctanf,
            ctan,
#ifdef HAVE_CTANL
            ctanl,
#else
			NULL,
#endif
#ifdef HAVE_QUADMATH_H
            ctanq
#else
            NULL
#endif
            );
}

static nodecl_t simplify_tanh(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            /* domain_check */ NULL,
            tanhf,
            tanh,
            tanhl,
#ifdef HAVE_QUADMATH_H
            tanhq,
#else
            NULL,
#endif
            ctanhf,
            ctanh,
#ifdef HAVE_CTAHNL
            ctanhl,
#else
			NULL,
#endif
#ifdef HAVE_QUADMATH_H
            ctanhq
#else
            NULL
#endif
            );
}

static nodecl_t simplify_trim(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t string = arguments[0];

    if (!nodecl_is_constant(string))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(string);

    // Should not happen
    if (!const_value_is_string(cval))
        return nodecl_null();

    char is_null_ended = 0;
    const char* str = const_value_string_unpack_to_string(cval, &is_null_ended);

    int length = strlen(str) + !!is_null_ended;
    if (length == 0)
        return nodecl_null();

    char* new_str = xstrdup(str);
    char* right = &(new_str[length - 1]);

    while (*right == ' ')
        right--;

    right++; *right = '\0';

    const_value_t* trimmed_string = const_value_make_string(new_str, strlen(new_str));

    DELETE(new_str);

    return const_value_to_nodecl(trimmed_string);
}

static char value_is_positive(const_value_t* cval)
{
    if (const_value_is_complex(cval))
        return 1;

    return !const_value_is_negative(cval);
}

static nodecl_t simplify_sqrt(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    if (!nodecl_is_constant(x))
        return nodecl_null();

    const_value_t* cval = nodecl_get_constant(x);

    return common_real_function_1_to_nodecl(
            cval,
            value_is_positive,
            sqrtf,
            sqrt,
            sqrtl,
#ifdef HAVE_QUADMATH_H
            sqrtq,
#else
            NULL,
#endif
            csqrtf,
            csqrt,
            csqrtl,
#ifdef HAVE_QUADMATH_H
            csqrtq
#else
            NULL
#endif
            );
}

#endif

static nodecl_t simplify_iachar(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t c = arguments[0];

    nodecl_t kind_arg = arguments[1];

    if (nodecl_is_null(c))
        return nodecl_null();

    // We cannot simplify anything if the nodecl is non-constant
    if (!nodecl_is_constant(c))
        return nodecl_null();

    const_value_t* str = nodecl_get_constant(c);

    int num_elements = 0;
    int *values = NULL;

    char is_null_ended = 0;
    const_value_string_unpack_to_int(str, &values, &num_elements, &is_null_ended);

    num_elements += !!is_null_ended;

    if (num_elements == 0)
        return nodecl_null();

    int val = values[0];
    DELETE(values);

    int kind = fortran_get_default_integer_type_kind();
    if (!nodecl_is_null(kind_arg))
        kind = const_value_cast_to_signed_int(nodecl_get_constant(kind_arg));

    type_t* t = choose_int_type_from_kind(kind_arg, kind);

    return const_value_to_nodecl_with_basic_type(
            const_value_get_integer(val, type_get_size(t), /* sign */ 1),
            t);
}

static nodecl_t simplify_ichar(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    // Mercurium only supports ASCII thus ichar is the same as iachar
    return simplify_iachar(entry, num_arguments, arguments);
}

static const_value_t* compute_ibclr(const_value_t* cval_i, const_value_t* cval_pos)
{
    if (!const_value_is_integer(cval_i)
            || !const_value_is_integer(cval_pos))
        return NULL;

    cvalue_int_t i = const_value_cast_to_cvalue_int(cval_i);
    cvalue_int_t pos = const_value_cast_to_cvalue_int(cval_pos);

    int bytes = const_value_get_bytes(cval_i);
    int bits = bytes * 8;

    if (pos < 0
            || pos >= bits)
        return NULL;

    cvalue_uint_t new_i = i & ~(1 << pos);
    return const_value_get_integer(new_i, bytes, /* signed */ 1);
}

static nodecl_t simplify_ibclr(scope_entry_t* entry UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        nodecl_t* arguments)
{
    const_value_t *cval_i = nodecl_get_constant(arguments[0]);
    const_value_t *cval_pos = nodecl_get_constant(arguments[1]);

    if (cval_i == NULL
            || cval_pos == NULL)
        return nodecl_null();

    const_value_t* cval = compute_binary_elemental(
            cval_i,
            cval_pos,
            compute_ibclr);

    if (cval == NULL)
        return nodecl_null();

    return const_value_to_nodecl(cval);
}

static const_value_t* compute_ibset(const_value_t* cval_i, const_value_t* cval_pos)
{
    if (!const_value_is_integer(cval_i)
            || !const_value_is_integer(cval_pos))
        return NULL;

    cvalue_int_t i = const_value_cast_to_cvalue_int(cval_i);
    cvalue_int_t pos = const_value_cast_to_cvalue_int(cval_pos);

    int bytes = const_value_get_bytes(cval_i);
    int bits = bytes * 8;

    if (pos < 0
            || pos >= bits)
        return NULL;

    cvalue_uint_t new_i = i | (1 << pos);
    return const_value_get_integer(new_i, bytes, /* signed */ 1);
}


static nodecl_t simplify_ibset(scope_entry_t* entry UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        nodecl_t* arguments)
{
    const_value_t *cval_i = nodecl_get_constant(arguments[0]);
    const_value_t *cval_pos = nodecl_get_constant(arguments[1]);

    if (cval_i == NULL
            || cval_pos == NULL)
        return nodecl_null();

    const_value_t* cval = compute_binary_elemental(
            cval_i,
            cval_pos,
            compute_ibset);

    if (cval == NULL)
        return nodecl_null();

    return const_value_to_nodecl(cval);
}

static const_value_t* compute_btest(const_value_t* cval_i, const_value_t* cval_pos)
{
    if (!const_value_is_integer(cval_i)
            || !const_value_is_integer(cval_pos))
        return NULL;

    cvalue_int_t i = const_value_cast_to_cvalue_int(cval_i);
    cvalue_int_t pos = const_value_cast_to_cvalue_int(cval_pos);

    int bytes = const_value_get_bytes(cval_i);
    int bits = bytes * 8;

    if (pos < 0
            || pos >= bits)
        return NULL;

    cvalue_uint_t test_bit = (i & (1 << pos));

    if (test_bit)
    {
        return const_value_get_one(fortran_get_default_logical_type_kind(), 1);
    }
    else
    {
        return const_value_get_zero(fortran_get_default_logical_type_kind(), 1);
    }
}

static nodecl_t simplify_btest(scope_entry_t* entry UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        nodecl_t* arguments)
{
    const_value_t *cval_i = nodecl_get_constant(arguments[0]);
    const_value_t *cval_pos = nodecl_get_constant(arguments[1]);

    if (cval_i == NULL
            || cval_pos == NULL)
        return nodecl_null();

    const_value_t* cval = compute_binary_elemental(
            cval_i,
            cval_pos,
            compute_btest);

    if (cval == NULL)
        return nodecl_null();

    return const_value_to_nodecl_with_basic_type(cval,
            fortran_get_default_logical_type());
}

static const_value_t* compute_nint(const_value_t* cval)
{
    // Array case
    if (const_value_is_array(cval))
    {
        int i, N = const_value_get_num_elements(cval);
        const_value_t* array[N];

        for (i = 0; i < N; i++)
        {
            array[i] = compute_nint(const_value_get_element_num(cval, i));
            if (array[i] == NULL)
                return NULL;
        }

        return const_value_make_array(N, array);
    }

    return const_value_cast_to_signed_int_value(const_value_round_to_zero(cval));
}

static nodecl_t simplify_nint(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments, nodecl_t* arguments)
{
    nodecl_t arg = arguments[0];
    nodecl_t kind = nodecl_null();
    if (num_arguments == 2)
    {
        kind = arguments[1];
    }

    if (!nodecl_is_constant(arg)
            || (!nodecl_is_null(kind) && !nodecl_is_constant(kind)))
        return nodecl_null();


    int kind_ = type_get_size(no_ref(nodecl_get_type(arg)));
    if (!nodecl_is_null(kind))
    {
        kind_ = const_value_cast_to_signed_int(nodecl_get_constant(kind));
    }

    type_t* integer_type = choose_int_type_from_kind(kind, kind_);
    const_value_t* integer_value = 
        const_value_cast_to_bytes(
                compute_nint(nodecl_get_constant(arg)),
                type_get_size(integer_type),
                /* sign */ 1);

    return const_value_to_nodecl_with_basic_type(
            integer_value,
            integer_type);
}

static nodecl_t simplify_null(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    const_value_t* zero = const_value_get_zero(fortran_get_default_integer_type_kind(), 1);

    ERROR_CONDITION(entry == NULL, "Invalid symbol", 0);

    nodecl_t called = nodecl_make_symbol(entry, make_locus("", 0, 0));

    nodecl_t result;
    if (nodecl_is_null(arguments[0]))
    {
        type_t* pointer_type = get_pointer_type(get_void_type());

        result = nodecl_make_function_call(called,
                nodecl_null(),
                nodecl_null(),
                nodecl_null(),
                pointer_type,
                make_locus("", 0, 0));
        nodecl_set_constant(result, zero);
    }
    else
    {
        type_t* pointer_type = nodecl_get_type(arguments[0]);

        result = nodecl_make_function_call(called,
                nodecl_make_list_1(nodecl_shallow_copy(arguments[0])),
                nodecl_null(),
                nodecl_null(),
                pointer_type,
                make_locus("", 0, 0));
        nodecl_set_constant(result, zero);
    }

    return result;
}

static nodecl_t simplify_mcc_loc(scope_entry_t* entry UNUSED_PARAMETER, int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t arg = arguments[0];

    arg = fortran_expression_as_variable(nodecl_shallow_copy(arg));

    return nodecl_make_reference(
                arg,
                get_pointer_type(get_void_type()),
                nodecl_get_locus(arguments[0]));
}

static nodecl_t simplify_mcc_null(scope_entry_t* entry UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        nodecl_t* arguments UNUSED_PARAMETER)
{
    const_value_t* zero = const_value_get_zero(fortran_get_default_integer_type_kind(), 1);

    nodecl_t zero_pointer = const_value_to_nodecl(zero);

    nodecl_set_type(zero_pointer, get_variant_type_zero(fortran_get_default_integer_type()));
    return zero_pointer;
}

static const_value_t* compute_ieor(const_value_t* val_i, const_value_t* val_j)
{
    if (!const_value_is_integer(val_i)
            || !const_value_is_integer(val_j))
        return NULL;

    return const_value_get_integer(
            const_value_cast_to_cvalue_uint(val_i) ^ const_value_cast_to_cvalue_uint(val_j),
            const_value_get_bytes(val_i),
            /* signed */ 1);
}

static nodecl_t simplify_ieor(scope_entry_t* entry UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        nodecl_t* arguments)
{
    const_value_t *cval_i = nodecl_get_constant(arguments[0]);
    const_value_t *cval_j = nodecl_get_constant(arguments[1]);

    if (cval_i == NULL
            || cval_j == NULL)
        return nodecl_null();

    const_value_t* cval = compute_binary_elemental(
            cval_i,
            cval_j,
            compute_ieor);

    if (cval == NULL)
        return nodecl_null();

    return const_value_to_nodecl(cval);
}

static const_value_t* compute_ior(const_value_t* val_i, const_value_t* val_j)
{
    if (!const_value_is_integer(val_i)
            || !const_value_is_integer(val_j))
        return NULL;

    return const_value_get_integer(
            const_value_cast_to_cvalue_uint(val_i) | const_value_cast_to_cvalue_uint(val_j),
            const_value_get_bytes(val_i),
            /* signed */ 1);
}

static nodecl_t simplify_ior(scope_entry_t* entry UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        nodecl_t* arguments)
{
    const_value_t *cval_i = nodecl_get_constant(arguments[0]);
    const_value_t *cval_j = nodecl_get_constant(arguments[1]);

    if (cval_i == NULL
            || cval_j == NULL)
        return nodecl_null();

    const_value_t* cval = compute_binary_elemental(
            cval_i,
            cval_j,
            compute_ior);

    if (cval == NULL)
        return nodecl_null();

    return const_value_to_nodecl(cval);
}

static const_value_t* compute_iand(const_value_t* val_i, const_value_t* val_j)
{
    if (!const_value_is_integer(val_i)
            || !const_value_is_integer(val_j))
        return NULL;

    return const_value_get_integer(
            const_value_cast_to_cvalue_uint(val_i) & const_value_cast_to_cvalue_uint(val_j),
            const_value_get_bytes(val_i),
            /* signed */ 1);
}

static nodecl_t simplify_iand(scope_entry_t* entry UNUSED_PARAMETER,
        int num_arguments UNUSED_PARAMETER,
        nodecl_t* arguments)
{
    const_value_t *cval_i = nodecl_get_constant(arguments[0]);
    const_value_t *cval_j = nodecl_get_constant(arguments[1]);

    if (cval_i == NULL
            || cval_j == NULL)
        return nodecl_null();

    const_value_t* cval = compute_binary_elemental(
            cval_i,
            cval_j,
            compute_iand);

    if (cval == NULL)
        return nodecl_null();

    return const_value_to_nodecl(cval);
}
