#ifndef FORTRAN03_INTRINSICS_SIMPLIFY_H
#define FORTRAN03_INTRINSICS_SIMPLIFY_H

#include <math.h>
#include <limits.h>
#include <float.h>

static nodecl_t nodecl_make_zero(void)
{
    return nodecl_make_integer_literal(get_signed_int_type(), const_value_get_zero(type_get_size(get_signed_int_type()), 1), NULL, 0);
}

static nodecl_t simplify_precision(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = nodecl_get_type(x);

    real_model_t model = real_type_get_model(t);

    // In mercurium radix is always 2
    int k = 0;

    int precision = (((model.p - 1) * log10(model.base)) + k);

    return nodecl_make_integer_literal(get_signed_int_type(), const_value_get(precision, type_get_size(get_signed_int_type()), 1), NULL, 0);
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
            const_value_get(value, type_get_size(get_signed_int_type()), 1),
            NULL, 0);
}

static nodecl_t simplify_radix(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments UNUSED_PARAMETER)
{
    // Radix is always 2 in our compiler
    return nodecl_make_integer_literal(get_signed_int_type(), 
            const_value_get(2, type_get_size(get_signed_int_type()), 1), 
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

    if (nodecl_is_constant(p)
            && nodecl_is_constant(r)
            && nodecl_is_constant(radix))
    {
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
                        const_value_get(type_get_size(real_types[i]), type_get_size(get_signed_int_type()), 1), NULL, 0);
            }
        }
    }

    return nodecl_null();
}

#endif
