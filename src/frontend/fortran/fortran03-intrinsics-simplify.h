#ifndef FORTRAN03_INTRINSICS_SIMPLIFY_H
#define FORTRAN03_INTRINSICS_SIMPLIFY_H

#include <math.h>
#include <limits.h>
#include <float.h>

static nodecl_t nodecl_make_int_literal(int n)
{
    return nodecl_make_integer_literal(get_signed_int_type(), const_value_get_signed_int(n), NULL, 0);
}

static nodecl_t nodecl_make_zero(void)
{
    return nodecl_make_int_literal(0);
}

static nodecl_t nodecl_make_one(void)
{
    return nodecl_make_int_literal(1);
}

static nodecl_t simplify_precision(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = nodecl_get_type(x);

    real_model_t model = real_type_get_model(t);

    // In mercurium radix is always 2
    int k = 0;

    int precision = (((model.p - 1) * log10(model.base)) + k);

    return nodecl_make_integer_literal(get_signed_int_type(), const_value_get_signed_int(precision), NULL, 0);
}


static nodecl_t simplify_huge(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = nodecl_get_type(x);
    t = get_rank0_type(t);

    if (is_float_type(t))
    {
        return nodecl_make_floating_literal(t, const_value_get_float(FLT_MAX), NULL, 0);
    }
    else if (is_double_type(t))
    {
        return nodecl_make_floating_literal(t, const_value_get_double(DBL_MAX), NULL, 0);
    }
    else if (is_long_double_type(t))
    {
        return nodecl_make_floating_literal(t, const_value_get_long_double(LDBL_MAX), NULL, 0);
    }
    else if (is_integer_type(t))
    {
        return nodecl_make_integer_literal(t, integer_type_get_maximum(t), NULL, 0);
    }

    return nodecl_null();
}

static nodecl_t simplify_tiny(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = nodecl_get_type(x);
    t = get_rank0_type(t);

    if (is_float_type(t))
    {
        return nodecl_make_floating_literal(t, const_value_get_float(FLT_MIN), NULL, 0);
    }
    else if (is_double_type(t))
    {
        return nodecl_make_floating_literal(t, const_value_get_double(DBL_MIN), NULL, 0);
    }
    else if (is_long_double_type(t))
    {
        return nodecl_make_floating_literal(t, const_value_get_long_double(LDBL_MIN), NULL, 0);
    }
    else if (is_integer_type(t))
    {
        return nodecl_make_integer_literal(t, const_value_get_one(type_get_size(t), 1), NULL, 0);
    }

    return nodecl_null();
}

#define MIN(a, b) ((a) < (b) ? (a) : (b))

static nodecl_t simplify_range(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];
    type_t* t = nodecl_get_type(x);

    int value = 0;
    if (is_integer_type(t))
    {
        nodecl_t huge = simplify_huge(1, arguments);
        const_value_t* val = nodecl_get_constant(huge);
        value = log10( const_value_cast_to_8(val) );
    }
    else if (is_floating_type(t))
    {
        nodecl_t huge = simplify_huge(1, arguments);
        nodecl_t tiny = simplify_tiny(1, arguments);
        double huge_val = const_value_cast_to_double(nodecl_get_constant(huge));
        double tiny_val = const_value_cast_to_double(nodecl_get_constant(tiny));

        value = MIN(log10(huge_val), -log10(tiny_val));
    }
    else if (is_complex_type(t))
    {
        // Not yet implemented
        return nodecl_null();
    }

    return nodecl_make_integer_literal(get_signed_int_type(),
            const_value_get_signed_int(value),
            NULL, 0);
}

static nodecl_t simplify_radix(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments UNUSED_PARAMETER)
{
    // Radix is always 2 in our compiler
    return nodecl_make_integer_literal(get_signed_int_type(), 
            const_value_get_signed_int(2), 
            NULL, 0);
}

static nodecl_t simplify_selected_real_kind(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
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

    // Get our three reals: float, double, long double
    type_t* real_types[] = { get_float_type(), get_double_type(), get_long_double_type() };

    int num_reals = sizeof(real_types) / sizeof(real_types[0]);

    uint64_t p_ = const_value_cast_to_8(nodecl_get_constant(p));
    uint64_t r_ = const_value_cast_to_8(nodecl_get_constant(r));
    uint64_t radix_ = const_value_cast_to_8(nodecl_get_constant(radix));

    int i;
    for (i = 0; i < num_reals; i++)
    {
        nodecl_t nodecl_type = nodecl_make_type(real_types[i], NULL, 0);

        nodecl_t precision = simplify_precision(1, &nodecl_type);
        nodecl_t range = simplify_range(1, &nodecl_type);
        nodecl_t current_radix = simplify_radix(1, &nodecl_type);

        uint64_t precision_ = const_value_cast_to_8(nodecl_get_constant(precision));
        uint64_t range_ = const_value_cast_to_8(nodecl_get_constant(range));
        uint64_t current_radix_ = const_value_cast_to_8(nodecl_get_constant(current_radix));

        if (p_ <= precision_
                && r_ <= range_
                && (radix_ == 0 || radix_ == current_radix_))
        {
            return nodecl_make_integer_literal(get_signed_int_type(),
                    const_value_get_signed_int(type_get_size(real_types[i])), NULL, 0);
        }
    }

    return nodecl_null();
}

static nodecl_t simplify_selected_int_kind(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
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

    return nodecl_make_integer_literal(
            get_signed_int_type(),
            const_value_get_signed_int(kind), 
            NULL, 0);
}

static nodecl_t simplify_selected_char_kind(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t name = arguments[0];

    if (nodecl_get_kind(name) == NODECL_STRING_LITERAL)
    {
        const char* t = nodecl_get_text(name);

        if (strcmp(t, "\"ASCII\"") == 0
                || strcmp(t, "'ASCII'") == 0)
        {
            return nodecl_make_integer_literal(
                    get_signed_int_type(),
                    const_value_get_signed_int(1), 
                    NULL, 0);
        }
        else
        {
            // We do not support anything else
            return nodecl_make_integer_literal(
                    get_signed_int_type(),
                    const_value_get_signed_int(-1), 
                    NULL, 0);
        }
    }

    return nodecl_null();
}

static nodecl_t simplify_bit_size(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t i = arguments[0];

    return nodecl_make_integer_literal(
            get_signed_int_type(),
            const_value_get_signed_int(type_get_size(no_ref(nodecl_get_type(i))) * 8), 
            NULL, 0);
}

static nodecl_t simplify_len(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t str = arguments[0];

    type_t* t = no_ref(nodecl_get_type(str));

    if (array_type_is_unknown_size(t))
        return nodecl_null();

    return nodecl_copy(array_type_get_array_size_expr(t));
}

static nodecl_t simplify_kind(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];
     
    type_t* t = no_ref(nodecl_get_type(x));
    t = get_rank0_type(t);

    return nodecl_make_integer_literal(
            get_signed_int_type(),
            const_value_get_signed_int(type_get_size(t)), 
            NULL, 0);
}

static nodecl_t simplify_digits(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = no_ref(nodecl_get_type(x));
    t = get_rank0_type(t);

    if (is_integer_type(t))
    {
        return nodecl_make_integer_literal(
                get_signed_int_type(),
                // -1 because of the sign
                const_value_get_signed_int(type_get_size(t) * 8 - 1),
                NULL, 0);
    }
    else if (is_floating_type(t))
    {
        real_model_t model = real_type_get_model(t);

        return nodecl_make_integer_literal(
                get_signed_int_type(),
                // +1 because of the implicit integer part not actually represented
                const_value_get_signed_int(model.p + 1),
                NULL, 0);
    }

    return nodecl_null();
}

static nodecl_t simplify_epsilon(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = no_ref(nodecl_get_type(x));
    t = get_rank0_type(t);

    if (is_float_type(t))
    {
        return nodecl_make_floating_literal(
                get_float_type(),
                const_value_get_float(FLT_EPSILON),
                NULL, 0);
    }
    else if (is_double_type(t))
    {
        return nodecl_make_floating_literal(
                get_float_type(),
                const_value_get_double(DBL_EPSILON),
                NULL, 0);
    }
    else if (is_long_double_type(t))
    {
        return nodecl_make_floating_literal(
                get_float_type(),
                const_value_get_long_double(LDBL_EPSILON),
                NULL, 0);
    }

    return nodecl_null();
}

static nodecl_t simplify_maxexponent(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = no_ref(nodecl_get_type(x));
    t = get_rank0_type(t);

    real_model_t model = real_type_get_model(t);

    return nodecl_make_integer_literal(
            get_signed_int_type(),
            const_value_get_signed_int(model.emax),
            NULL, 0);
}

static nodecl_t simplify_minexponent(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = no_ref(nodecl_get_type(x));
    t = get_rank0_type(t);

    real_model_t model = real_type_get_model(t);

    return nodecl_make_integer_literal(
            get_signed_int_type(),
            const_value_get_signed_int(model.emin),
            NULL, 0);
}

static nodecl_t simplify_xbound(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments,
        nodecl_t (*bound_fun)(type_t*))
{
    nodecl_t array = arguments[0];
    nodecl_t dim = arguments[1];
    nodecl_t kind = arguments[2];

    int kind_ = type_get_size(get_signed_int_type());
    if (!nodecl_is_null(kind))
    {
        if (!nodecl_is_constant(kind))
            return nodecl_null();

        kind_ = const_value_cast_to_4(nodecl_get_constant(kind));
    }

    if (nodecl_is_null(dim))
    {
        type_t* t = no_ref(nodecl_get_type(array));
        int i, rank = get_rank_of_type(t);
        nodecl_t nodecl_list = nodecl_null();
        for (i = 0; i < rank; i++)
        {
            if (array_type_is_unknown_size(t))
            {
                return nodecl_null();
            }

            nodecl_list = nodecl_concat_lists(
                    nodecl_make_list_1(nodecl_copy(bound_fun(t))),
                    nodecl_list);

            t = array_type_get_element_type(t);
        }

        return nodecl_make_structured_literal(
                nodecl_list,
                get_array_type_bounds(choose_int_type_from_kind(nodecl_get_ast(kind), kind_),
                    nodecl_make_one(),
                    nodecl_make_integer_literal(get_signed_int_type(), const_value_get_signed_int(kind_), NULL, 0),
                    CURRENT_COMPILED_FILE->global_decl_context),
                NULL, 0);
    }
    else
    {
        if (nodecl_is_constant(dim))
        {
            type_t* t = no_ref(nodecl_get_type(array));
            int dim_ = const_value_cast_to_4(nodecl_get_constant(dim));

            int rank = get_rank_of_type(t);

            if ((rank - dim_) < 0)
                return nodecl_null();

            int i;
            for (i = 0; i < (rank - dim_); i++)
            {
                t = array_type_get_element_type(t);
            }

            if (!array_type_is_unknown_size(t))
            {
                return nodecl_copy(bound_fun(t));
            }
        }
    }

    return nodecl_null();
}

static nodecl_t simplify_lbound(int num_arguments, nodecl_t* arguments)
{
    return simplify_xbound(num_arguments, arguments, array_type_get_array_lower_bound);
}

static nodecl_t simplify_ubound(int num_arguments, nodecl_t* arguments)
{
    return simplify_xbound(num_arguments, arguments, array_type_get_array_upper_bound);
}

static nodecl_t simplify_size(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t array = arguments[0];
    nodecl_t dim = arguments[1];
    nodecl_t kind = arguments[2];

    int kind_ = type_get_size(get_signed_int_type());
    if (!nodecl_is_null(kind))
    {
        if (!nodecl_is_constant(kind))
            return nodecl_null();

        kind_ = const_value_cast_to_4(nodecl_get_constant(kind));
    }

    if (nodecl_is_null(dim))
    {
        type_t* t = no_ref(nodecl_get_type(array));
        int i, rank = get_rank_of_type(t);
        int value = 1;
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

            value = value * const_value_cast_to_4(nodecl_get_constant(size));
            
            t = array_type_get_element_type(t);
        }

        return nodecl_make_integer_literal(
                choose_int_type_from_kind(nodecl_get_ast(kind), kind_),
                const_value_get_signed_int(value),
                NULL, 0);
    }
    else
    {
        if (nodecl_is_constant(dim))
        {
            type_t* t = no_ref(nodecl_get_type(array));
            int dim_ = const_value_cast_to_4(nodecl_get_constant(dim));

            int rank = get_rank_of_type(t);

            if ((rank - dim_) < 0)
                return nodecl_null();

            int i;
            for (i = 0; i < (rank - dim_); i++)
            {
                t = array_type_get_element_type(t);
            }

            if (!array_type_is_unknown_size(t))
            {
                return nodecl_copy(array_type_get_array_size_expr(t));
            }
        }
    }

    return nodecl_null();
}

static nodecl_t simplify_shape(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t array = arguments[0];
    nodecl_t kind = arguments[1];

    int kind_ = type_get_size(get_signed_int_type());
    if (!nodecl_is_null(kind))
    {
        if (!nodecl_is_constant(kind))
            return nodecl_null();

        kind_ = const_value_cast_to_4(nodecl_get_constant(kind));
    }

    nodecl_t nodecl_list = nodecl_null();

    type_t* t = no_ref(nodecl_get_type(array));
    int i, rank = get_rank_of_type(t);
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
                nodecl_make_list_1(nodecl_copy(size)),
                nodecl_list);

        t = array_type_get_element_type(t);
    }

    if (rank > 0)
    {
        return nodecl_make_structured_literal(
                nodecl_list,
                get_array_type_bounds(choose_int_type_from_kind(nodecl_get_ast(kind), kind_),
                    nodecl_make_one(),
                    nodecl_make_integer_literal(get_signed_int_type(), const_value_get_signed_int(kind_), NULL, 0),
                    CURRENT_COMPILED_FILE->global_decl_context),
                NULL, 0);
    }
    else
    {
        return nodecl_make_structured_literal(
                nodecl_null(),
                get_array_type_bounds(choose_int_type_from_kind(nodecl_get_ast(kind), kind_),
                    nodecl_make_one(),
                    nodecl_make_zero(), 
                    CURRENT_COMPILED_FILE->global_decl_context),
                NULL, 0);
    }
}

#endif
