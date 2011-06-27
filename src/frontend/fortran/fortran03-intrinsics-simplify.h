#ifndef FORTRAN03_INTRINSICS_SIMPLIFY_H
#define FORTRAN03_INTRINSICS_SIMPLIFY_H

#include <math.h>

static nodecl_t make_nodecl_zero(void)
{
    return nodecl_make_integer_literal(get_signed_int_type(), const_value_get_zero(type_get_size(get_signed_int_type()), 1), NULL, 0);
}

static nodecl_t simplify_precision(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    nodecl_t x = arguments[0];

    type_t* t = nodecl_get_type(x);

    real_model_t model = real_type_get_model(t);

    // FIXME - We should check if model.r is a power of 10, but it is not in Mercurium
    int k = 0;

    int precision = (((model.p - 1) * log10(model.base)) + k);

    return nodecl_make_integer_literal(get_signed_int_type(), const_value_get(precision, type_get_size(get_signed_int_type()), 1), NULL, 0);
}

static nodecl_t simplify_range(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
#if 0
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
    }
    else if (is_complex_type(t))
    {
    }
#endif

    return nodecl_null();
}

static nodecl_t simplify_radix(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
    // Radix is always 2 in our compiler
    return nodecl_make_integer_literal(get_signed_int_type(), 
            const_value_get(2, type_get_size(get_signed_int_type()), 1), 
            NULL, 0);
}

static nodecl_t simplify_selected_real_kind(int num_arguments UNUSED_PARAMETER, nodecl_t* arguments)
{
#if 0
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
        }
    }
#endif

    return nodecl_null();
}

#endif
