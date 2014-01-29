/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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



#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <float.h>
#include <math.h>
#include <complex.h>
#include <fenv.h>
#ifdef HAVE_QUADMATH_H
#include <quadmath.h>
#endif
#include "cxx-buildscope.h"
#include "cxx-exprtype.h"
#include "cxx-cexpr.h"
#include "cxx-ast.h"
#include "cxx-utils.h"
#include "cxx-prettyprint.h"
#include "cxx-ambiguity.h"
#include "cxx-typeutils.h"
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"
#include "cxx-overload.h"
#include "cxx-instantiation.h"
#include "cxx-typeenviron.h"
#include "cxx-limits.h"
#include "cxx-nodecl-output.h"

#define CVAL_HASH_SIZE (37)

/*
IMPORTANT: incompatible changes to enum const_value_kind_tag requires
increasing the value of CURRENT_MODULE_VERSION in fortran03-modules.c.

It is safe to add new enumerator values after the last.

Any other change is an incompatible one. In particular, removing
or reordering the enumerator values are incompatible changes.
*/
typedef enum const_value_kind_tag
{
    CVK_NONE = 0,
    CVK_INTEGER,
    CVK_FLOAT,
    CVK_DOUBLE,
    CVK_LONG_DOUBLE,
    CVK_FLOAT128,
    CVK_COMPLEX,
    CVK_ARRAY,
    CVK_STRUCT,
    CVK_VECTOR,
    CVK_STRING,
    CVK_RANGE,
    CVK_MASK,
} const_value_kind_t;

typedef struct const_multi_value_tag
{
    type_t* struct_type;
    int num_elements;
    const_value_t* elements[];
} const_multi_value_t;

/*
IMPORTANT: incompatible changes to memory layout of struct const_value_tag
requires increasing the value of CURRENT_MODULE_VERSION in fortran03-modules.c.

It is safe to add new fields inside the union value.

Any other change should be assumed to be incompatible. In particular, adding
fields before or after the union are incompatible changes.
*/
struct const_value_tag
{
    const_value_kind_t kind;
    char sign : 1;
    int num_bytes;

    union
    {
        // CVK_INTEGER
        cvalue_uint_t  i;
        cvalue_int_t si;
        // CVK_FLOAT
        float f;
        // CVK_DOUBLE
        double d;
        // CVK_LONG_DOUBLE
        long double ld;
#ifdef HAVE_QUADMATH_H
        // CVK_FLOAT128
        __float128 f128;
#endif
        // CVK_COMPLEX
        // CVK_ARRAY
        // CVK_STRUCT
        // CVK_VECTOR
        // CVK_RANGE
        const_multi_value_t* m;
    } value;
};

typedef
struct const_value_hash_bucket_tag
{
    const_value_t* constant_value;
    struct const_value_hash_bucket_tag *next;
} const_value_hash_bucket_t;

typedef const_value_hash_bucket_t* const_value_hash_t[CVAL_HASH_SIZE];

static const_value_hash_t _hash_pool[(MCXX_MAX_BYTES_INTEGER + 1) * 2] = { { (const_value_hash_bucket_t*)0 } };

const_value_t* const_value_get_integer(cvalue_uint_t value, int num_bytes, char sign)
{
    ERROR_CONDITION(num_bytes > MCXX_MAX_BYTES_INTEGER
            || num_bytes < 0, "Invalid num_bytes = %d\n", num_bytes);

    int bucket_index = value % CVAL_HASH_SIZE;

    int pool = 2 * num_bytes + !!sign;

    const_value_hash_bucket_t* bucket = _hash_pool[pool][bucket_index];

    while (bucket != NULL)
    {
        if (bucket->constant_value->value.i == value)
        {
            break;
        }
        bucket = bucket->next;
    }

    if (bucket == NULL)
    {
        bucket = xcalloc(1, sizeof(*bucket));
        
        bucket->constant_value = xcalloc(1, sizeof(*bucket->constant_value));
        bucket->constant_value->kind = CVK_INTEGER;
        bucket->constant_value->value.i = value;
        bucket->constant_value->num_bytes = num_bytes;
        bucket->constant_value->sign = sign;

        bucket->next = _hash_pool[pool][bucket_index];

        _hash_pool[pool][bucket_index] = bucket;
    }

    return bucket->constant_value;
}

#define GET_SIGNED_INTEGER(type)  \
const_value_t* const_value_get_signed_##type ( cvalue_uint_t value ) \
{ \
    return const_value_get_integer(value, type_get_size(get_signed_##type##_type ( ) ), 1); \
}

#define GET_UNSIGNED_INTEGER(type)  \
const_value_t* const_value_get_unsigned_##type ( cvalue_uint_t value ) \
{ \
    return const_value_get_integer(value, type_get_size(get_unsigned_##type##_type ( ) ), 0); \
} \

#define GET_INTEGER(type) \
    GET_SIGNED_INTEGER(type) \
    GET_UNSIGNED_INTEGER(type)
    

GET_INTEGER(int)
GET_INTEGER(long_int)
GET_INTEGER(long_long_int)

const_value_t* const_value_get_float(float f)
{
    const_value_t* v = xcalloc(1, sizeof(*v));
    v->kind = CVK_FLOAT;
    v->value.f = f;
    v->sign = 1;

    return v;
}

const_value_t* const_value_get_double(double d)
{
    const_value_t* v = xcalloc(1, sizeof(*v));
    v->kind = CVK_DOUBLE;
    v->value.d = d;
    v->sign = 1;

    return v;
}

const_value_t* const_value_get_long_double(long double ld)
{
    const_value_t* v = xcalloc(1, sizeof(*v));
    v->kind = CVK_LONG_DOUBLE;
    v->value.ld = ld;
    v->sign = 1;

    return v;
}

#ifdef HAVE_QUADMATH_H
const_value_t* const_value_get_float128(__float128 ld)
{
    const_value_t* v = xcalloc(1, sizeof(*v));
    v->kind = CVK_FLOAT128;
    v->value.f128 = ld;
    v->sign = 1;

    return v;
}
#endif

#define OTHER_KIND default : { internal_error("Unexpected literal kind", 0); }

#define CAST_TO_INTX(_bytes, _field) \
case _bytes:  \
    { \
        if (sign) \
        { \
            return const_value_get_integer((int## _bytes ##_t)val->value._field, bytes, 1); \
        } \
        else \
        { \
            return const_value_get_integer((uint## _bytes ##_t)val->value._field, bytes, 0); \
        } \
        break; \
    }

#ifdef HAVE_INT128
#define CAST_TO_INT128(_field) \
case 128: \
    { \
        if (sign) \
        { \
            return const_value_get_integer((signed __int128)val->value._field, 16, 1); \
        } \
        else \
        { \
            return const_value_get_integer((unsigned __int128)val->value._field, 16, 0); \
        } \
        break; \
    }
#else
#define CAST_TO_INT128(_field)
#endif

#define CAST_FLOAT_POINT_TO_INT(_field) \
{ \
    int bits = bytes * 8; \
    switch (bits) \
    { \
        CAST_TO_INTX(8, _field) \
        CAST_TO_INTX(16, _field) \
        CAST_TO_INTX(32, _field) \
        CAST_TO_INTX(64, _field) \
        CAST_TO_INT128(_field) \
        default: { internal_error("Cannot perform conversion of floating point to integer of %d bytes\n", bytes); } \
    } \
}

const_value_t* const_value_cast_to_bytes(const_value_t* val, int bytes, char sign)
{
    switch (val->kind)
    {
        case CVK_INTEGER:
            return const_value_get_integer(val->value.i, bytes, sign);
        case CVK_FLOAT:
            CAST_FLOAT_POINT_TO_INT(f);
            break;
        case CVK_DOUBLE:
            CAST_FLOAT_POINT_TO_INT(d);
            break;
        case CVK_LONG_DOUBLE:
            CAST_FLOAT_POINT_TO_INT(ld);
            break;
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            CAST_FLOAT_POINT_TO_INT(f128);
            break;
#endif
        OTHER_KIND;
    }
    return NULL;
}

static const_value_t* make_multival(int num_elements, const_value_t **elements)
{
    const_value_t* result = xcalloc(1, sizeof(*result));

    result->value.m = xcalloc(1, sizeof(const_multi_value_t) + sizeof(const_value_t) * num_elements);
    result->value.m->num_elements = num_elements;

    int i;
    for (i = 0; i < num_elements; i++)
    {
        ERROR_CONDITION(elements[i] == NULL, "Invalid NULL constant in component %d of multi-value constant", i);

        result->value.m->elements[i] = elements[i];
    }

    return result;
}

static const_value_t* multival_get_element_num(const_value_t* v, int element)
{
    ERROR_CONDITION(element >= v->value.m->num_elements, "Invalid index %d in a multi-value constant with up to %d components", 
            element, v->value.m->num_elements);
    return v->value.m->elements[element];
}

static int multival_get_num_elements(const_value_t* v)
{
    return v->value.m->num_elements;
}

#define IS_MULTIVALUE(x) \
    (x == CVK_COMPLEX \
    || x == CVK_ARRAY \
    || x == CVK_STRUCT \
    || x == CVK_VECTOR \
    || x == CVK_STRING \
    || x == CVK_RANGE)

// Use this to apply a unary function to a multival
static const_value_t* map_unary_to_structured_value(const_value_t* (*fun)(const_value_t*),
        const_value_t* m1)
{
    ERROR_CONDITION(!IS_MULTIVALUE(m1->kind), "The value is not a multiple-value constant", 0);

    int i, num_elements = multival_get_num_elements(m1);
    const_value_t* result_arr[num_elements];
    for (i = 0; i < num_elements; i++)
    {
        result_arr[i] = fun(multival_get_element_num(m1, i));
    }

    const_value_t* mval = make_multival(num_elements, result_arr);
    mval->kind = m1->kind;

    return mval;
}

// Use this to apply a binary function to a couple of multivals
static const_value_t* map_binary_to_structured_value(const_value_t* (*fun)(const_value_t*, const_value_t*),
        const_value_t* m1,
        const_value_t* m2)
{
    ERROR_CONDITION(!IS_MULTIVALUE(m1->kind) || !IS_MULTIVALUE(m2->kind), "One of the values is not a multiple-value constant", 0);
    ERROR_CONDITION(multival_get_num_elements(m1) != multival_get_num_elements(m2), 
            "Cannot apply a binary map to multiple-values with different number of elements %d != %d", 
            multival_get_num_elements(m1),
            multival_get_num_elements(m2));

    int i, num_elements = multival_get_num_elements(m1);
    const_value_t* result_arr[num_elements];
    for (i = 0; i < num_elements; i++)
    {
        result_arr[i] = fun(multival_get_element_num(m1, i), multival_get_element_num(m2, i));
    }

    const_value_t* mval = make_multival(num_elements, result_arr);
    mval->kind = m1->kind;

    return mval;
}

const_value_t* const_value_cast_to_signed_int_value(const_value_t* val)
{
    if (IS_MULTIVALUE(val->kind))
    {
        return map_unary_to_structured_value(const_value_cast_to_signed_int_value, val);
    }
    return const_value_cast_to_bytes(val, type_get_size(get_signed_int_type()), 1);
}

const_value_t* const_value_round(const_value_t* val, int num_bytes, int rounding_mode)
{
    switch (val->kind)
    {
        case CVK_INTEGER:
            {
                return val;
            }
        case CVK_FLOAT:
            {
                int old_round_mode = fegetround();
                fesetround(rounding_mode);

                long long int l = llrintf(val->value.f);

                fesetround(old_round_mode);

                return const_value_get_integer(l, num_bytes, 1);
            }
        case CVK_DOUBLE:
            {
                int old_round_mode = fegetround();
                fesetround(rounding_mode);

                long long int l = llrint(val->value.d);

                fesetround(old_round_mode);

                return const_value_get_integer(l, num_bytes, 1);
            }
        case CVK_LONG_DOUBLE:
            {
                int old_round_mode = fegetround();
                fesetround(rounding_mode);

                long long int l = llrintl(val->value.ld);

                fesetround(old_round_mode);

                return const_value_get_integer(l, num_bytes, 1);
            }
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            {
                int old_round_mode = fegetround();
                fesetround(rounding_mode);

                long long int l = llrintq(val->value.ld);

                fesetround(old_round_mode);

                return const_value_get_integer(l, num_bytes, 1);
            }
#endif
        OTHER_KIND;
    }
    return NULL;
}

const_value_t* const_value_round_to_zero_bytes(const_value_t* val, int num_bytes)
{
    return const_value_round(val, num_bytes, FE_TOWARDZERO);
}

const_value_t* const_value_round_to_zero(const_value_t* val)
{
    return const_value_round_to_zero_bytes(val, type_get_size(get_signed_int_type()));
}

const_value_t* const_value_round_to_nearest_bytes(const_value_t* val, int num_bytes)
{
    return const_value_round(val, num_bytes, FE_TONEAREST);
}

const_value_t* const_value_round_to_nearest(const_value_t* val)
{
    return const_value_round_to_nearest_bytes(val, type_get_size(get_signed_int_type()));
}

const_value_t* const_value_get_zero(int num_bytes, char sign)
{
    return const_value_get_integer(0, num_bytes, sign);
}

const_value_t* const_value_get_one(int num_bytes, char sign)
{
    return const_value_get_integer(1, num_bytes, sign);
}

const_value_t* const_value_get_minus_one(int num_bytes, char sign)
{
    return const_value_get_integer(-1, num_bytes, sign);
}

static void common_bytes(const_value_t* v1, const_value_t* v2, int *num_bytes, char *sign)
{
    if (v1->num_bytes == v2->num_bytes
            && v1->sign == v2->sign)
    {
        *num_bytes = v1->num_bytes;
        *sign = v1->sign;
    }
    else 
    {
        *num_bytes = (v1->num_bytes > v2->num_bytes) ? v1->num_bytes : v2->num_bytes;
        if (v1->sign != v2->sign)
        {
            if (v1->num_bytes == v2->num_bytes)
                *sign = 0;
            else if (v1->num_bytes > v2->num_bytes)
                *sign = v1->sign;
            else if (v1->num_bytes < v2->num_bytes)
                *sign = v2->sign;
        }
        else
        {
            *sign = v1->sign;
        }
    }
}

char const_value_is_nonzero(const_value_t* v)
{
    if (IS_MULTIVALUE(v->kind))
    {
        int num_elements = v->value.m->num_elements;
        int i;
        for (i=0; i<num_elements; i++)
        {
            if (const_value_is_nonzero(v->value.m->elements[i]))
                return 1;
        }

        return 0;
    }

    switch (v->kind)
    {
        case CVK_INTEGER:
            return !!v->value.i;
        case CVK_FLOAT:
            return v->value.f != 0.0f;
        case CVK_DOUBLE:
            return v->value.d != 0.0;
        case CVK_LONG_DOUBLE:
            return v->value.ld != 0.0L;
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            return v->value.f128 != 0.0Q;
#endif
        case CVK_COMPLEX:
            {
                return const_value_is_nonzero(const_value_complex_get_real_part(v))
                    || const_value_is_nonzero(const_value_complex_get_imag_part(v));

            }
        OTHER_KIND;
    }

    return 0;
}

char const_value_is_zero(const_value_t* v)
{
    return !const_value_is_nonzero(v);
}

char const_value_is_one(const_value_t* v)
{
    if (IS_MULTIVALUE(v->kind))
    {
        int num_elements = v->value.m->num_elements;
        int i;
        for (i=0; i<num_elements; i++)
        {
            if (!const_value_is_one(v->value.m->elements[i]))
                return 0;
        }

        return 1;
    }

    switch (v->kind)
    {
        case CVK_INTEGER:
            return v->value.i == 1;
        case CVK_FLOAT:
            return v->value.f == 1.0f;
        case CVK_DOUBLE:
            return v->value.d == 1.0;
        case CVK_LONG_DOUBLE:
            return v->value.ld == 1.0L;
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            return v->value.f128 == 1.0Q;
#endif
        case CVK_COMPLEX:
            {
                return const_value_is_one(const_value_complex_get_real_part(v))
                    && const_value_is_zero(const_value_complex_get_imag_part(v));

            }
        OTHER_KIND;
    }

    return 0;   
}

char const_value_is_minus_one(const_value_t* v)
{
    switch (v->kind)
    {
        case CVK_INTEGER:
            return v->value.si == -1;
        case CVK_FLOAT:
            return v->value.f == -1.0f;
        case CVK_DOUBLE:
            return v->value.d == -1.0;
        case CVK_LONG_DOUBLE:
            return v->value.ld == -1.0L;
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            return v->value.f128 == -1.0Q;
#endif
        case CVK_COMPLEX:
            {
                return const_value_is_minus_one(const_value_complex_get_real_part(v))
                    && const_value_is_zero(const_value_complex_get_imag_part(v));

            }
        OTHER_KIND;
    }

    return 0;   
}

char const_value_is_positive(const_value_t* v)
{
    
    switch (v->kind)
    {
        case CVK_INTEGER:
            if (!v->sign)
                return 1;
            else
                return v->value.si > 0;
        case CVK_FLOAT:
            return v->value.f > 0.0f;
        case CVK_DOUBLE:
            return v->value.d > 0.0;
        case CVK_LONG_DOUBLE:
            return v->value.ld > 0.0L;
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            return v->value.f128 > 0.0Q;
#endif
        OTHER_KIND;        
    }

    return 0;    
}

char const_value_is_negative(const_value_t* v)
{
    return !const_value_is_positive(v);
}

cvalue_int_t const_value_cast_to_cvalue_int(const_value_t* value)
{
    return (cvalue_int_t)const_value_cast_to_cvalue_uint(value);
}

cvalue_uint_t const_value_cast_to_cvalue_uint(const_value_t* value)
{
#ifdef HAVE_INT128
    return const_value_cast_to_16(value);
#else
    return const_value_cast_to_8(value);
#endif
}

uint64_t const_value_cast_to_8(const_value_t* val)
{
    switch (val->kind)
    {
        case CVK_INTEGER:
            return val->value.i;
        case CVK_FLOAT:
            return val->value.f;
        case CVK_DOUBLE:
            return val->value.d;
        case CVK_LONG_DOUBLE:
            return val->value.ld;
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            return val->value.f128;
#endif
        OTHER_KIND;
    }
}

#ifdef HAVE_INT128
unsigned __int128 const_value_cast_to_16(const_value_t* val)
{
    switch (val->kind)
    {
        case CVK_INTEGER:
            return val->value.i;
        case CVK_FLOAT:
            return val->value.f;
        case CVK_DOUBLE:
            return val->value.d;
        case CVK_LONG_DOUBLE:
            return val->value.ld;
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            return val->value.f128;
#endif
        OTHER_KIND;
    }
}
#endif

uint32_t const_value_cast_to_4(const_value_t* val)
{
    switch (val->kind)
    {
        case CVK_INTEGER:
            return val->value.i;
        case CVK_FLOAT:
            return val->value.f;
        case CVK_DOUBLE:
            return val->value.d;
        case CVK_LONG_DOUBLE:
            return val->value.ld;
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            return val->value.f128;
#endif
        OTHER_KIND;
    }
}

uint16_t const_value_cast_to_2(const_value_t* val)
{
    switch (val->kind)
    {
        case CVK_INTEGER:
            return val->value.i;
        case CVK_FLOAT:
            return val->value.f;
        case CVK_DOUBLE:
            return val->value.d;
        case CVK_LONG_DOUBLE:
            return val->value.ld;
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            return val->value.f128;
#endif
        OTHER_KIND;
    }
}

uint8_t const_value_cast_to_1(const_value_t* val)
{
    switch (val->kind)
    {
        case CVK_INTEGER:
            return val->value.i;
        case CVK_FLOAT:
            return val->value.f;
        case CVK_DOUBLE:
            return val->value.d;
        case CVK_LONG_DOUBLE:
            return val->value.ld;
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            return val->value.f128;
#endif
        OTHER_KIND;
    }
}

int const_value_cast_to_signed_int(const_value_t* val)
{
    switch (val->kind)
    {
        case CVK_INTEGER:
            return val->value.i;
        case CVK_FLOAT:
            return val->value.f;
        case CVK_DOUBLE:
            return val->value.d;
        case CVK_LONG_DOUBLE:
            return val->value.ld;
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            return val->value.f128;
#endif
        OTHER_KIND;
    }
}

#ifdef HAVE_QUADMATH_H
  #define IS_FLOAT(kind) (kind == CVK_FLOAT || kind == CVK_DOUBLE || kind == CVK_LONG_DOUBLE || kind == CVK_FLOAT128)
#else
  #define IS_FLOAT(kind) (kind == CVK_FLOAT || kind == CVK_DOUBLE || kind == CVK_LONG_DOUBLE)
#endif

char const_value_is_signed(const_value_t* val)
{
    return val->sign;
}

struct type_mask_tag
{
    type_t* type;
    cvalue_uint_t mask;
};

static cvalue_uint_t safe_compute_bitmask(
        char is_signed,
        size_t size_in_bytes)
{
    size_t shift = 8 * size_in_bytes;

    if (is_signed)
        shift--;

#ifdef HAVE_INT128
    if (shift == 128)
#else
    if (shift == 64)
#endif
    {
        return 0;
    }

    cvalue_uint_t bitmask = 0;

    if (shift < (sizeof(cvalue_uint_t) * 8))
    {
        bitmask = (~(cvalue_uint_t)0) << shift;
    }

    return bitmask;
}

static cvalue_uint_t safe_compute_bitmask_of_type(type_t* t)
{
    return safe_compute_bitmask(is_signed_integral_type(t), type_get_size(t));
}

static type_t* get_minimal_integer_for_value_(
        char is_signed, 
        cvalue_uint_t value,
        struct type_mask_tag* unsigned_type_mask,
        struct type_mask_tag* signed_type_mask)
{
    if (!is_signed)
    {
        struct type_mask_tag* type_mask = unsigned_type_mask;
        if (type_mask == NULL)
            return NULL;

        int i;
        for (i = 0; type_mask[i].type != NULL; i++)
        {
            if ((value & type_mask[i].mask) == 0)
            {
                return type_mask[i].type;
            }
        }
        return NULL;
    }
    else
    {
        const int NBITS_1 = (sizeof(cvalue_uint_t)*8 - 1);
        char is_negative = (value & ((cvalue_uint_t)1 << NBITS_1)) >> NBITS_1;

        struct type_mask_tag *type_mask= signed_type_mask;

        if (!is_negative)
        {
            if (type_mask == NULL)
                return NULL;

            int i;
            for (i = 0; type_mask[i].type != NULL; i++)
            {
                if ((value & type_mask[i].mask) == 0)
                {
                    return type_mask[i].type;
                }
            }
            return NULL;
        }
        else
        {
            if (type_mask == NULL)
                return NULL;

            int i;
            for (i = 0; type_mask[i].type != NULL; i++)
            {
                // Note that we count the sign bit as a valid bit, so we shift left the mask 1 bit
                if (((~value) & (type_mask[i].mask << 1)) == 0)
                {
                    return type_mask[i].type;
                }
            }
            return NULL;
        }
    }

    internal_error("Code unreachable", 0);
    return NULL;
}

static type_t* get_minimal_integer_for_value_at_least_signed_int(char is_signed, cvalue_uint_t value)
{
    struct type_mask_tag unsigned_type_mask[] =
    {
        { get_unsigned_int_type(),           safe_compute_bitmask_of_type(get_unsigned_int_type())      },
        { get_unsigned_long_int_type(),      safe_compute_bitmask_of_type(get_unsigned_long_int_type()) },
        { get_unsigned_long_long_int_type(), safe_compute_bitmask_of_type(get_unsigned_long_long_int_type()) },
#ifdef HAVE_INT128
        { get_unsigned_int128_type(),        safe_compute_bitmask_of_type(get_unsigned_int128_type()) },
#endif
        // Sentinel
        { NULL, 0 },
    };

    struct type_mask_tag signed_type_mask[] =
    {
        { get_signed_int_type(),           safe_compute_bitmask_of_type(get_signed_int_type())           },
        { get_signed_long_int_type(),      safe_compute_bitmask_of_type(get_signed_long_int_type())      },
        { get_signed_long_long_int_type(), safe_compute_bitmask_of_type(get_signed_long_long_int_type()) },
#ifdef HAVE_INT128
        { get_signed_int128_type(),        safe_compute_bitmask_of_type(get_signed_int128_type())        },
#endif
        // Sentinel
        { NULL, 0 },
    };

    return get_minimal_integer_for_value_(
            is_signed, value,
            unsigned_type_mask,
            signed_type_mask);
}

static type_t* get_minimal_integer_for_value(char is_signed, cvalue_uint_t value)
{
    struct type_mask_tag unsigned_type_mask[] =
    {
        { get_unsigned_char_type(),          safe_compute_bitmask_of_type(get_unsigned_char_type())      },
        { get_unsigned_short_int_type(),     safe_compute_bitmask_of_type(get_unsigned_short_int_type()) },
        { get_unsigned_int_type(),           safe_compute_bitmask_of_type(get_unsigned_int_type())       },
        { get_unsigned_long_int_type(),      safe_compute_bitmask_of_type(get_unsigned_long_int_type())  },
        { get_unsigned_long_long_int_type(), safe_compute_bitmask_of_type(get_unsigned_long_long_int_type()) },
#ifdef HAVE_INT128
        { get_unsigned_int128_type(),        safe_compute_bitmask_of_type(get_unsigned_int128_type())    },
#endif
        // Sentinel
        { NULL, 0 },
    };

    struct type_mask_tag signed_type_mask[] =
    {
        // Like above but one bit less now
        { get_signed_char_type(),          safe_compute_bitmask_of_type(get_signed_char_type())          },
        { get_signed_short_int_type(),     safe_compute_bitmask_of_type(get_signed_short_int_type())     },
        { get_signed_int_type(),           safe_compute_bitmask_of_type(get_signed_int_type())           },
        { get_signed_long_int_type(),      safe_compute_bitmask_of_type(get_signed_long_int_type())      },
        { get_signed_long_long_int_type(), safe_compute_bitmask_of_type(get_signed_long_long_int_type()) },
#ifdef HAVE_INT128
        { get_signed_int128_type(),        safe_compute_bitmask_of_type(get_signed_int128_type())        },
#endif
        // Sentinel
        { NULL, 0 },
    };

    return get_minimal_integer_for_value_(
            is_signed, value,
            unsigned_type_mask,
            signed_type_mask);
}

type_t* const_value_get_minimal_integer_type_from_list_of_types(
        cvalue_uint_t value,
        int num_types,
        type_t** types)
{
    if (num_types <= 0)
        internal_error("Invalid number of types", 0);

    struct type_mask_tag unsigned_type_mask[2] = { { NULL, 0 }, { NULL, 0 } };
    struct type_mask_tag signed_type_mask[2] = { { NULL, 0 }, { NULL, 0 } };

    type_t* result = NULL;
    int i;
    for (i = 0; i < num_types && result == NULL; i++)
    {
        // We try one by one because we allow types with differing signs
        char is_signed = is_signed_integral_type(types[i]);

        if (!is_signed)
        {
            unsigned_type_mask[0].type = types[i];
            unsigned_type_mask[0].mask = safe_compute_bitmask_of_type(types[i]);
        }
        else
        {
            signed_type_mask[0].type = types[i];
            signed_type_mask[0].mask = safe_compute_bitmask_of_type(types[i]);
        }

        result = get_minimal_integer_for_value_(
                is_signed,
                value,
                unsigned_type_mask,
                signed_type_mask);
    }

    return result;
}

static type_t* get_suitable_floating_type(const_value_t* val)
{
    if (val->kind == CVK_FLOAT)
    {
        return get_float_type();
    }
    else if (val->kind == CVK_DOUBLE)
    {
        return get_double_type();
    }
    else if (val->kind == CVK_LONG_DOUBLE)
    {
        return get_long_double_type();
    }
    internal_error("Invalid constant kind", 0);
}

type_t* const_value_get_minimal_integer_type(const_value_t* val)
{
    return get_minimal_integer_for_value(val->sign, val->value.i);
}

type_t* const_value_get_minimal_integer_for_value_at_least_signed_int(const_value_t* val)
{
    return get_minimal_integer_for_value_at_least_signed_int(val->sign, val->value.i);
}

nodecl_t const_value_to_nodecl_with_basic_type(const_value_t* v, 
        type_t* basic_type)
{
    switch (v->kind)
    {
        case CVK_INTEGER:
            {
                // Zero is special
                if (basic_type == NULL && v->value.i == 0)
                    return nodecl_make_integer_literal(get_zero_type(get_signed_int_type()), v, make_locus("", 0, 0));

                type_t* t = basic_type;
                if (t == NULL)
                    t = get_minimal_integer_for_value_at_least_signed_int(v->sign, v->value.i);

                ERROR_CONDITION(t == NULL, "Invalid type for integer constant", 0);

                if (is_bool_type(t))
                {
                    return nodecl_make_boolean_literal(t, v, make_locus("", 0, 0));
                }
                else
                {
                    return nodecl_make_integer_literal(t, v, make_locus("", 0, 0));
                }
                break;
            }
        case CVK_FLOAT:
        case CVK_DOUBLE:
        case CVK_LONG_DOUBLE:
            {
                type_t* t = basic_type;
                if (t == NULL)
                    t = get_suitable_floating_type(v);
                return nodecl_make_floating_literal(t, v, make_locus("", 0, 0));
                break;
            }
        case CVK_STRING:
            {
                return nodecl_make_string_literal(
                        get_array_type_bounds(
                            get_char_type(),
                            nodecl_make_integer_literal(get_signed_int_type(), const_value_get_one(4, 1), make_locus("", 0, 0)),
                            nodecl_make_integer_literal(get_signed_int_type(), const_value_get_signed_int(v->value.m->num_elements), make_locus("", 0, 0)),
                            CURRENT_COMPILED_FILE->global_decl_context),
                        v, make_locus("", 0, 0));
                break;
            }
        case CVK_ARRAY:
            {
                nodecl_t list = nodecl_null();
                int i;
                for (i = 0; i < v->value.m->num_elements; i++)
                {
                    list = nodecl_append_to_list(list, const_value_to_nodecl_with_basic_type(v->value.m->elements[i], basic_type));
                }

                // Get the type from the first element
                type_t* t = basic_type;
                if (v->value.m->num_elements > 0)
                {
                    t = nodecl_get_type(nodecl_list_head(list));
                }

                // Fortran boundaries!
                t = get_array_type_bounds(
                        t,
                        nodecl_make_integer_literal(get_signed_int_type(), const_value_get_one(4, 1), make_locus("", 0, 0)),
                        nodecl_make_integer_literal(get_signed_int_type(), const_value_get_signed_int(v->value.m->num_elements), make_locus("", 0, 0)),
                        CURRENT_COMPILED_FILE->global_decl_context);

                nodecl_t result = nodecl_make_structured_value(
                        list, t,
                        make_locus("", 0, 0));

                nodecl_set_constant(result, v);
                return result;
                break;
            }
        case CVK_STRUCT:
            {
                nodecl_t list = nodecl_null();

                scope_entry_list_t* data_members = class_type_get_nonstatic_data_members(basic_type);

                int i;
                scope_entry_list_iterator_t* it_member = NULL;
                for (i = 0, it_member = entry_list_iterator_begin(data_members);
                        (i < v->value.m->num_elements) && !entry_list_iterator_end(it_member);
                        i++, entry_list_iterator_next(it_member))
                {
                    scope_entry_t* member = entry_list_iterator_current(it_member);

                    list = nodecl_append_to_list(list,
                            const_value_to_nodecl_with_basic_type(v->value.m->elements[i],
                                member->type_information));
                }
                entry_list_iterator_free(it_member);

                entry_list_free(data_members);

                type_t* t = v->value.m->struct_type;

                nodecl_t result = nodecl_make_structured_value(
                        list, t,
                        make_locus("", 0, 0));

                nodecl_set_constant(result, v);
                return result;
                break;
            }
        case CVK_COMPLEX:
            {
                const_value_t* real = const_value_complex_get_real_part(v);

                type_t* t = NULL;
                if (const_value_is_floating(real))
                {
                    t = basic_type;
                    if (t == NULL)
                        t = get_suitable_floating_type(const_value_complex_get_real_part(v));
                }
                else if (const_value_is_integer(real))
                {
                    // This is a GCC extension
                    t = basic_type;
                    if (t == NULL)
                        t = get_minimal_integer_for_value_at_least_signed_int(real->sign, real->value.i);
                }

                t = get_complex_type(t);
                nodecl_t result = nodecl_make_complex_literal(t, v, make_locus("", 0, 0));

                return result;
                break;
            }
        case CVK_VECTOR:
            {
                ERROR_CONDITION(v->value.m->num_elements == 0, "Zero length vector constant", 0);

                char promote_from_scalar = 1;

                // Check if all values are the same so we can use a vector promotion instead
                int i;
                for (i = 1; i < v->value.m->num_elements && promote_from_scalar; i++)
                {
                    promote_from_scalar =
                        const_value_is_nonzero(
                                const_value_eq(
                                    v->value.m->elements[i-1],
                                    v->value.m->elements[i]));
                }

                nodecl_t result = nodecl_null();
                if (promote_from_scalar)
                {
                    nodecl_t scalar_node = const_value_to_nodecl_with_basic_type(
                            v->value.m->elements[0], basic_type);
                    type_t* vector_type = get_vector_type_by_elements(
                            nodecl_get_type(scalar_node),
                            v->value.m->num_elements);

                    result = nodecl_make_vector_promotion(
                            scalar_node,
                            /* mask */ nodecl_null(),
                            vector_type,
                            make_locus("", 0, 0));
                }
                else
                {
                    nodecl_t list = nodecl_null();
                    for (i = 0; i < v->value.m->num_elements; i++)
                    {
                        list = nodecl_append_to_list(list,
                                const_value_to_nodecl_with_basic_type(v->value.m->elements[i], basic_type));
                    }

                    type_t* vector_type = get_vector_type_by_elements(
                            nodecl_get_type(nodecl_list_head(list)),
                            v->value.m->num_elements);

                    result = nodecl_make_vector_literal(list,
                            /* mask */ nodecl_null(),
                            vector_type,
                            make_locus("", 0, 0));
                }

                nodecl_set_constant(result, v);

                return result;
                break;
            }
        default:
            {
                // The caller should check this case
                return nodecl_null();
                break;
            }
    }
}

nodecl_t const_value_to_nodecl(const_value_t* v)
{
    return const_value_to_nodecl_with_basic_type(v, /* basic_type */ NULL);
}

char const_value_is_integer(const_value_t* v)
{
    return v->kind == CVK_INTEGER;
}

char const_value_is_floating(const_value_t* v)
{
    return const_value_is_float(v)
        || const_value_is_double(v)
        || const_value_is_long_double(v)
#ifdef HAVE_QUADMATH_H
        || const_value_is_float128(v)
#endif
        ;
}

char const_value_is_float(const_value_t* v)
{
    return v->kind == CVK_FLOAT;
}

char const_value_is_double(const_value_t* v)
{
    return v->kind == CVK_DOUBLE;
}

char const_value_is_long_double(const_value_t* v)
{
    return v->kind == CVK_LONG_DOUBLE;
}

#ifdef HAVE_QUADMATH_H
char const_value_is_float128(const_value_t* v)
{
    return v->kind == CVK_FLOAT128;
}
#endif

char const_value_is_complex(const_value_t* v)
{
    return v->kind == CVK_COMPLEX;
}

char const_value_is_structured(const_value_t* v)
{
    return v->kind == CVK_STRUCT;
}

char const_value_is_array(const_value_t* v)
{
    return v->kind == CVK_ARRAY;
}

char const_value_is_vector(const_value_t* v)
{
    return v->kind == CVK_VECTOR;
}

char const_value_is_string(const_value_t* v)
{
    return v->kind == CVK_STRING;
}

char const_value_is_range(const_value_t* v)
{
    return v->kind == CVK_RANGE;
}

float const_value_cast_to_float(const_value_t* v)
{
    if (v->kind == CVK_FLOAT)
        return v->value.f;
    else if (v->kind == CVK_INTEGER)
    {
        if (v->sign)
        {
            return (float)v->value.si;
        }
        else
        {
            return (float)v->value.i;
        }
    }
    else if (v->kind == CVK_DOUBLE)
        return (float)v->value.d;
    else if (v->kind == CVK_LONG_DOUBLE)
        return (float)v->value.ld;
#ifdef HAVE_QUADMATH_H
    else if (v->kind == CVK_FLOAT128)
        return (float)v->value.f128;
#endif
    else if (IS_MULTIVALUE(v->kind))
    {
    }

    internal_error("Code unreachable", 0);
}

double const_value_cast_to_double(const_value_t* v)
{
    if (v->kind == CVK_DOUBLE)
        return v->value.d;
    else if (v->kind == CVK_INTEGER)
    {
        if (v->sign)
        {
            return (double)(v->value.si);
        }
        else
        {
            return (double)v->value.i;
        }
    }
    else if (v->kind == CVK_FLOAT)
        return (double)v->value.f;
    else if (v->kind == CVK_LONG_DOUBLE)
        return (double)v->value.ld;
#ifdef HAVE_QUADMATH_H
    else if (v->kind == CVK_FLOAT128)
        return (double)v->value.f128;
#endif

    internal_error("Code unreachable", 0);
}

long double const_value_cast_to_long_double(const_value_t* v)
{
    if (v->kind == CVK_LONG_DOUBLE)
        return v->value.ld;
    else if (v->kind == CVK_INTEGER)
    {
        if (v->sign)
        {
            return (long double)(v->value.si);
        }
        else
        {
            return (long double)v->value.i;
        }
    }
    else if (v->kind == CVK_FLOAT)
        return (long double)v->value.f;
    else if (v->kind == CVK_DOUBLE)
        return (long double)v->value.d;
#ifdef HAVE_QUADMATH_H
    else if (v->kind == CVK_FLOAT128)
        return (long double)v->value.f128;
#endif

    internal_error("Code unreachable", 0);
}

#ifdef HAVE_QUADMATH_H
__float128 const_value_cast_to_float128(const_value_t* v)
{
     if (v->kind == CVK_FLOAT128)
        return v->value.f128;
    else if (v->kind == CVK_INTEGER)
    {
        if (v->sign)
        {
            return (__float128)v->value.si;
        }
        else
        {
            return (__float128)v->value.i;
        }
    }
    else if (v->kind == CVK_FLOAT)
        return (__float128)v->value.f;
    else if (v->kind == CVK_DOUBLE)
        return (__float128)v->value.d;
    else if (v->kind == CVK_LONG_DOUBLE)
        return (__float128)v->value.ld;

    internal_error("Code unreachable", 0);
}
#endif

_Complex float const_value_cast_to_complex_float(const_value_t* val)
{
    return const_value_cast_to_float(const_value_complex_get_real_part(val)) + 
        const_value_cast_to_float(const_value_complex_get_imag_part(val)) * _Complex_I;
}

_Complex double const_value_cast_to_complex_double(const_value_t* val)
{
    return const_value_cast_to_double(const_value_complex_get_real_part(val)) + 
        const_value_cast_to_double(const_value_complex_get_imag_part(val)) * _Complex_I;
}

_Complex long double const_value_cast_to_complex_long_double(const_value_t* val)
{
    return const_value_cast_to_long_double(const_value_complex_get_real_part(val)) + 
        const_value_cast_to_long_double(const_value_complex_get_imag_part(val)) * _Complex_I;
}

#ifdef HAVE_QUADMATH_H
__complex128 const_value_cast_to_complex_float128(const_value_t* val)
{
    return const_value_cast_to_float128(const_value_complex_get_real_part(val)) + 
        const_value_cast_to_float128(const_value_complex_get_imag_part(val)) * _Complex_I;
}
#endif

const_value_t* const_value_cast_to_float_value(const_value_t* val)
{
    if (IS_MULTIVALUE(val->kind))
    {
        return map_unary_to_structured_value(const_value_cast_to_float_value, val);
    }
    return const_value_get_float(const_value_cast_to_float(val));
}

const_value_t* const_value_cast_to_double_value(const_value_t* val)
{
    if (IS_MULTIVALUE(val->kind))
    {
        return map_unary_to_structured_value(const_value_cast_to_double_value, val);
    }
    return const_value_get_double(const_value_cast_to_double(val));
}

const_value_t* const_value_cast_to_long_double_value(const_value_t* val)
{
    if (IS_MULTIVALUE(val->kind))
    {
        return map_unary_to_structured_value(const_value_cast_to_long_double_value, val);
    }
    return const_value_get_long_double(const_value_cast_to_long_double(val));
}

#ifdef HAVE_QUADMATH_H
const_value_t* const_value_cast_to_float128_value(const_value_t* val)
{
    if (IS_MULTIVALUE(val->kind))
    {
        return map_unary_to_structured_value(const_value_cast_to_float128_value, val);
    }
    return const_value_get_float128(const_value_cast_to_float128(val));
}
#endif 

const_value_t* const_value_cast_to_floating_type_value(const_value_t* val, type_t* floating_type)
{
    if (is_float_type(floating_type))
    {
        return const_value_cast_to_float_value(val);
    }
    else if (is_double_type(floating_type))
    {
        return const_value_cast_to_double_value(val);
    }
    else if (is_long_double_type(floating_type))
    {
        return const_value_cast_to_long_double_value(val);
    }
    else
    {
#ifdef HAVE_QUADMATH_H
        const floating_type_info_t* floating_info = floating_type_get_info(floating_type);
        if (floating_info->bits == 128)
        {
            return const_value_cast_to_float128_value(val);
        }
#endif
        internal_error("Invalid floating type '%s'\n", print_declarator(floating_type));
    }
}

const_value_t* integer_type_get_minimum(type_t* t)
{
    if (is_wchar_t_type(t))
    {
        t = (CURRENT_CONFIGURATION->type_environment->int_type_of_wchar_t)();
    }

    if (is_unsigned_char_type(t)
            || is_unsigned_byte_type(t)
            || is_unsigned_short_int_type(t)
            || is_unsigned_long_int_type(t)
            || is_unsigned_long_long_int_type(t)
            || is_unsigned_int_type(t))
    {
        return const_value_get_zero(type_get_size(t), /* sign */ 0);
    }
    else if (is_signed_char_type(t)
            || is_signed_byte_type(t)
            || is_signed_short_int_type(t)
            || is_signed_long_int_type(t)
            || is_signed_long_long_int_type(t)
            || is_signed_int_type(t))
    {
        cvalue_uint_t mask = ~(cvalue_uint_t)0;
        mask >>= 8*(sizeof(cvalue_uint_t) - type_get_size(t)) + 1;
        return const_value_get_integer(~mask, type_get_size(t), /* sign */ 1);
    }

    internal_error("Invalid type", 0);
    return NULL;
}

const_value_t* integer_type_get_maximum(type_t* t)
{
    if (is_wchar_t_type(t))
    {
        t = (CURRENT_CONFIGURATION->type_environment->int_type_of_wchar_t)();
    }

    if (is_unsigned_integral_type(t))
    {
        cvalue_uint_t mask = ~(cvalue_uint_t)0;

        if (type_get_size(t) < 8)
        {
            mask &= ~(~(cvalue_uint_t)0 << (type_get_size(t) * 8));
        }

        return const_value_get_integer(mask, type_get_size(t), /* sign */ 0);
    }
    else if (is_signed_integral_type(t))
    {
        cvalue_uint_t mask = ~(cvalue_uint_t)0;
        mask >>= 8*(sizeof(cvalue_uint_t) - type_get_size(t)) + 1;
        return const_value_get_integer(mask, type_get_size(t), /* sign */ 1);
    }

    internal_error("Invalid type", 0);
    return NULL;
}

const_value_t* floating_type_get_maximum(type_t* t)
{
    // FIXME We should make this more independent! 
    // Although it will work on most IEEE 754 environments
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
        else
#endif
        {
            internal_error("Invalid floating type", 0);
        }
    }
}

const_value_t* floating_type_get_minimum(type_t* t)
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
        else
#endif
        {
            internal_error("Invalid floating type", 0);
        }
    }
}

int const_value_get_bytes(const_value_t* val)
{
    return val->num_bytes;
}


const_value_t* const_value_make_array(int num_elements, const_value_t **elements)
{
    const_value_t* result = make_multival(num_elements, elements);
    result->kind = CVK_ARRAY;
    return result;
}

const_value_t* const_value_make_vector(int num_elements, const_value_t **elements)
{
    const_value_t* result = make_multival(num_elements, elements);
    result->kind = CVK_VECTOR;
    return result;
}

static const_value_t* const_value_make_multival_from_scalar(
        int num_elements, 
        const_value_t* value,
        const_value_t* (*const_value_make)(int num_elements, const_value_t** elements)
        )
{
    ERROR_CONDITION(value == NULL, "Invalid constant", 0);
    const_value_t** value_set = xcalloc(num_elements, sizeof(*value_set));

    for (int i = 0; i < num_elements; i++)
    {
        value_set[i] = value;
    }

    const_value_t* result = const_value_make(num_elements, value_set);

    xfree(value_set);

    return result;
}

const_value_t* const_value_make_vector_from_scalar(int num_elements, const_value_t* value)
{
    return const_value_make_multival_from_scalar(num_elements, value,
            const_value_make_vector);
}

const_value_t* const_value_make_array_from_scalar(int num_elements, const_value_t* value)
{
    return const_value_make_multival_from_scalar(num_elements, value,
            const_value_make_array);
}

const_value_t* const_value_make_struct(int num_elements, const_value_t **elements, type_t* struct_type)
{
    const_value_t* result = make_multival(num_elements, elements);
    result->kind = CVK_STRUCT;
    result->value.m->struct_type = struct_type;
    return result;
}

type_t* const_value_get_struct_type(const_value_t* v)
{
    ERROR_CONDITION(!const_value_is_structured(v), "Invalid constant value", 0);

    return v->value.m->struct_type;
}

const_value_t* const_value_make_string_from_values(int num_elements, const_value_t **elements)
{
    const_value_t* result = make_multival(num_elements, elements);
    result->kind = CVK_STRING;

    return result;
}

const_value_t* const_value_make_string(const char* literal, int num_elements)
{
    const_value_t* elements[num_elements + 1];
    memset(elements, 0, sizeof(elements));
    int i;
    for (i = 0; i < num_elements; i++)
    {
        elements[i] = const_value_get_integer(literal[i], 1, 0);
    }

    return const_value_make_string_from_values(num_elements, elements);
}

const_value_t* const_value_make_wstring(int* literal, int num_elements)
{
    const_value_t* elements[num_elements + 1];
    memset(elements, 0, sizeof(elements));
    int i;
    for (i = 0; i < num_elements; i++)
    {
        elements[i] = const_value_get_integer(literal[i], 4, 0);
    }

    return const_value_make_string_from_values(num_elements, elements);
}

const_value_t* const_value_make_complex(const_value_t* real_part, const_value_t* imag_part)
{
    const_value_t* complex_[] = { real_part, imag_part };

    ERROR_CONDITION((real_part->kind != imag_part->kind),
            "Real part and imag part must be the same constant kind", 0);

    const_value_t* result = make_multival(2, complex_);
    result->kind = CVK_COMPLEX;
    return result;
}

const_value_t* const_value_get_complex_float(_Complex float f)
{
    return const_value_make_complex(
            const_value_get_float(__real__ f),
            const_value_get_float(__imag__ f));
}

const_value_t* const_value_get_complex_double(_Complex double d)
{
    return const_value_make_complex(
            const_value_get_double(__real__ d),
            const_value_get_double(__imag__ d));
}

const_value_t* const_value_get_complex_long_double(_Complex long double ld)
{
    return const_value_make_complex(
            const_value_get_long_double(__real__ ld),
            const_value_get_long_double(__imag__ ld));
}

#ifdef HAVE_QUADMATH_H
const_value_t* const_value_get_complex_float128(__complex128 f128)
{
    return const_value_make_complex(
            const_value_get_float128(__real__ f128),
            const_value_get_float128(__imag__ f128));
}
#endif

const_value_t* const_value_make_range(const_value_t* lower, const_value_t* upper, const_value_t* stride)
{
    const_value_t* range[] = { lower, upper, stride };
    const_value_t* result = make_multival(3, range);
    result->kind = CVK_RANGE;
    return result;
}

const_value_t* const_value_complex_get_real_part(const_value_t* value)
{
    ERROR_CONDITION(!const_value_is_complex(value), "This is not a complex constant", 0);
    return multival_get_element_num(value, 0);
}

const_value_t* const_value_complex_get_imag_part(const_value_t* value)
{
    ERROR_CONDITION(!const_value_is_complex(value), "This is not a complex constant", 0);
    return multival_get_element_num(value, 1);
}

int const_value_get_num_elements(const_value_t* value)
{
    ERROR_CONDITION(!IS_MULTIVALUE(value->kind), "This is not a multiple-value constant", 0);
    return multival_get_num_elements(value);
}

const_value_t* const_value_get_element_num(const_value_t* value, int num)
{
    ERROR_CONDITION(!IS_MULTIVALUE(value->kind), "This is not a multiple-value constant", 0);
    return multival_get_element_num(value, num);
}

const_value_t* const_value_convert_to_vector(const_value_t* value, int num_elems)
{
    const_value_t* array[num_elems];
    int i;
    for (i = 0; i < num_elems; i++)
    {
        array[i] = value;
    }
    return const_value_make_vector(num_elems, array);
}

const_value_t* const_value_convert_to_array(const_value_t* value, int num_elems)
{
    const_value_t* array[num_elems];
    int i;
    for (i = 0; i < num_elems; i++)
    {
        array[i] = value;
    }
    return const_value_make_array(num_elems, array);
}

const_value_t* const_value_real_to_complex(const_value_t* value)
{
    if (const_value_is_integer(value))
    {
        return const_value_make_complex(
                value,
                const_value_get_zero(const_value_get_bytes(value), 
                    const_value_is_signed(value)));
    }
    else if (const_value_is_float(value))
    {
        return const_value_make_complex(
                value,
                const_value_get_float(0));
    }
    else if (const_value_is_double(value))
    {
        return const_value_make_complex(
                value,
                const_value_get_double(0));
    }
    else if (const_value_is_long_double(value))
    {
        return const_value_make_complex(
                value,
                const_value_get_long_double(0));
    }
    else
    {
        internal_error("Invalid constant value", 0);
    }
}

static const_value_t* extend_first_operand_to_structured_value(const_value_t* (*fun)(const_value_t*, const_value_t*),
        const_value_t* m1,
        const_value_t* m2)
{
    ERROR_CONDITION(!IS_MULTIVALUE(m1->kind), "The first operand must be a multiple-value constant", 0);
    ERROR_CONDITION(IS_MULTIVALUE(m2->kind), "The second operand must not be a multiple-value constant", 0);

    int i, num_elements = multival_get_num_elements(m1);
    const_value_t* result_arr[num_elements];
    for (i = 0; i < num_elements; i++)
    {
        result_arr[i] = fun(multival_get_element_num(m1, i), m2);
    }

    const_value_t* mval = make_multival(num_elements, result_arr);
    mval->kind = m1->kind;

    return mval;
}

static const_value_t* extend_second_operand_to_structured_value(const_value_t* (*fun)(const_value_t*, const_value_t*),
        const_value_t* m1,
        const_value_t* m2)
{
    ERROR_CONDITION(IS_MULTIVALUE(m1->kind), "The first operand must not be a multiple-value constant", 0);
    ERROR_CONDITION(!IS_MULTIVALUE(m2->kind), "The second operand must be a multiple-value constant", 0);

    int i, num_elements = multival_get_num_elements(m2);
    const_value_t* result_arr[num_elements];
    for (i = 0; i < num_elements; i++)
    {
        result_arr[i] = fun(m1, multival_get_element_num(m2, i));
    }

    const_value_t* mval = make_multival(num_elements, result_arr);
    mval->kind = m2->kind;

    return mval;
}


#define OP(_opname) const_value_##opname

#define BINOP_FUN_I(_opname, _binop) \
const_value_t* const_value_##_opname(const_value_t* v1, const_value_t* v2) \
{ \
    ERROR_CONDITION(v1 == NULL || v2 == NULL, "Either of the parameters is NULL", 0); \
    if ((v1->kind == CVK_INTEGER) \
            && (v2->kind == CVK_INTEGER)) \
    { \
       int bytes = 0; char sign = 0; \
       common_bytes(v1, v2, &bytes, &sign); \
       cvalue_uint_t value = 0; \
       if (sign) \
       { \
           (*((cvalue_int_t*)&value)) = v1->value.si _binop v2->value.si; \
       } \
       else \
       { \
           value = v1->value.i _binop v2->value.i; \
       } \
       return const_value_get_integer(value, bytes, sign); \
    } \
    return NULL; \
}

#ifdef HAVE_QUADMATH_H

#define CAST_TO_FLOAT_FIRST_IS_FLOAT128(a, b, _binop) \
   else if (a->kind == CVK_FLOAT128) \
   { \
       if (!b->sign) \
        return const_value_get_float128(a->value.f128 _binop (__float128) b->value.i); \
       else \
        return const_value_get_float128(a->value.f128 _binop (__float128) b->value.si); \
   } 

#define CAST_TO_FLOAT_FIRST_IS_FLOAT128_FUN(a, b, _func) \
    else if (a->kind == CVK_FLOAT128) \
    { \
       if (!b->sign) \
        return const_value_get_float128(_func ## q(a->value.f128, (__float128) b->value.i)); \
       else \
        return const_value_get_float128(_func ## q(a->value.f128, (__float128) b->value.si)); \
    } 

#define CAST_TO_FLOAT_SECOND_IS_FLOAT128(a, b, _binop) \
   else if (b->kind == CVK_FLOAT128) \
   { \
       if (!a->sign) \
           return const_value_get_float128((__float128) a->value.i _binop b->value.f128); \
       else \
           return const_value_get_float128((__float128) a->value.si _binop b->value.f128); \
   } 

#define CAST_TO_FLOAT_SECOND_IS_FLOAT128_FUN(a, b, _func) \
   else if (b->kind == CVK_FLOAT128) \
   { \
       if (!a->sign) \
           return const_value_get_float128( _func ## q( (__float128) a->value.i, b->value.f128)); \
       else \
           return const_value_get_float128( _func ## q( (__float128) a->value.si, b->value.f128)); \
   } 

#define BOTH_ARE_SAME_FLOAT128(a, b, _binop) \
    else if (a->kind == CVK_FLOAT128) \
        return const_value_get_float128(a->value.f128 _binop b->value.f128); 

#define BOTH_ARE_SAME_FLOAT128_FUN(a, b, _func) \
    else if (a->kind == CVK_FLOAT128) \
        return const_value_get_float128(_func ## q(a->value.f128, b->value.f128)); \

#define BOTH_ARE_FLOAT_FIRST_FLOAT128(a, b, _binop) \
    else if (a->kind == CVK_FLOAT128) \
    { \
        if (b->kind == CVK_FLOAT) \
        { \
            return const_value_get_float128(a->value.f128 _binop (__float128) b->value.f); \
        } \
        else if (b->kind == CVK_DOUBLE) \
        { \
            return const_value_get_float128(a->value.f128 _binop (__float128) b->value.d); \
        } \
        else if (b->kind == CVK_LONG_DOUBLE) \
        { \
            return const_value_get_float128(a->value.f128 _binop (__float128) b->value.ld); \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } 

#define BOTH_ARE_FLOAT_FIRST_FLOAT128_FUN(a, b, _func) \
    else if (a->kind == CVK_LONG_DOUBLE) \
    { \
        if (b->kind == CVK_FLOAT) \
        { \
            return const_value_get_float128(_func ## q (a->value.f128, (__float128) b->value.f)); \
        } \
        else if (b->kind == CVK_DOUBLE) \
        { \
            return const_value_get_float128(_func ## q(a->value.f128, (__float128) b->value.d)); \
        } \
        else if (b->kind == CVK_LONG_DOUBLE) \
        { \
            return const_value_get_float128(_func ## q(a->value.f128, (__float128) b->value.ld)); \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } 

#define BOTH_ARE_FLOAT_SECOND_FLOAT128(a, b, _binop) \
    else if (b->kind == CVK_FLOAT128) \
    { \
        if (a->kind == CVK_FLOAT) \
        { \
            return const_value_get_float128((__float128) a->value.f _binop b->value.f128); \
        } \
        else if (a->kind == CVK_DOUBLE) \
        { \
            return const_value_get_float128((__float128) a->value.d _binop b->value.f128); \
        } \
        else if (a->kind == CVK_LONG_DOUBLE) \
        { \
            return const_value_get_float128((__float128) a->value.ld _binop b->value.f128); \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } 

#define BOTH_ARE_FLOAT_SECOND_FLOAT128_FUN(a, b, _func) \
    else if (b->kind == CVK_FLOAT128) \
    { \
        if (a->kind == CVK_FLOAT) \
        { \
            return const_value_get_float128(_func ## q((__float128) a->value.f, b->value.f128)); \
        } \
        else if (a->kind == CVK_DOUBLE) \
        { \
            return const_value_get_float128(_func ## q ((__float128) a->value.d, b->value.f128)); \
        } \
        else if (a->kind == CVK_LONG_DOUBLE) \
        { \
            return const_value_get_float128(_func ## q ((__float128) a->value.ld, b->value.f128)); \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } 

// Relational

#define CAST_TO_FLOAT_FIRST_IS_FLOAT128_REL(a, b, _binop) \
   else if (a->kind == CVK_FLOAT128) \
   { \
       if (!b->sign) \
        return const_value_get_signed_int(a->value.f128 _binop (__float128) b->value.i); \
       else \
        return const_value_get_signed_int(a->value.f128 _binop (__float128) b->value.si); \
   } 

#define CAST_TO_FLOAT_FIRST_IS_FLOAT128_FUN_REL(a, b, _func) \
    else if (a->kind == CVK_FLOAT128) \
    { \
       if (!b->sign) \
        return const_value_get_signed_int(_func ## q(a->value.f128, (__float128) b->value.i)); \
       else \
        return const_value_get_signed_int(_func ## q(a->value.f128, (__float128) b->value.si)); \
    } 

#define CAST_TO_FLOAT_SECOND_IS_FLOAT128_REL(a, b, _binop) \
   else if (b->kind == CVK_FLOAT128) \
   { \
       if (!a->sign) \
           return const_value_get_signed_int((__float128) a->value.i _binop b->value.f128); \
       else \
           return const_value_get_signed_int((__float128) a->value.si _binop b->value.f128); \
   } 

#define CAST_TO_FLOAT_SECOND_IS_FLOAT128_FUN_REL(a, b, _func) \
   else if (b->kind == CVK_FLOAT128) \
   { \
       if (!a->sign) \
           return const_value_get_signed_int( _func ## q( (__float128) a->value.i, b->value.f128)); \
       else \
           return const_value_get_signed_int( _func ## q( (__float128) a->value.si, b->value.f128)); \
   } 

#define BOTH_ARE_SAME_FLOAT128_REL(a, b, _binop) \
    else if (a->kind == CVK_FLOAT128) \
        return const_value_get_signed_int(a->value.f128 _binop b->value.f128); 

#define BOTH_ARE_SAME_FLOAT128_FUN_REL(a, b, _func) \
    else if (a->kind == CVK_FLOAT128) \
        return const_value_get_signed_int(_func ## q(a->value.f128, b->value.f128)); \

#define BOTH_ARE_FLOAT_FIRST_FLOAT128_REL(a, b, _binop) \
    else if (a->kind == CVK_FLOAT128) \
    { \
        if (b->kind == CVK_FLOAT) \
        { \
            return const_value_get_signed_int(a->value.f128 _binop (__float128) b->value.f); \
        } \
        else if (b->kind == CVK_DOUBLE) \
        { \
            return const_value_get_signed_int(a->value.f128 _binop (__float128) b->value.d); \
        } \
        else if (b->kind == CVK_LONG_DOUBLE) \
        { \
            return const_value_get_signed_int(a->value.f128 _binop (__float128) b->value.ld); \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } 

#define BOTH_ARE_FLOAT_FIRST_FLOAT128_FUN_REL(a, b, _func) \
    else if (a->kind == CVK_LONG_DOUBLE) \
    { \
        if (b->kind == CVK_FLOAT) \
        { \
            return const_value_get_signed_int(_func ## q (a->value.f128, (__float128) b->value.f)); \
        } \
        else if (b->kind == CVK_DOUBLE) \
        { \
            return const_value_get_signed_int(_func ## q(a->value.f128, (__float128) b->value.d)); \
        } \
        else if (b->kind == CVK_LONG_DOUBLE) \
        { \
            return const_value_get_signed_int(_func ## q(a->value.f128, (__float128) b->value.ld)); \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } 

#define BOTH_ARE_FLOAT_SECOND_FLOAT128_REL(a, b, _binop) \
    else if (b->kind == CVK_FLOAT128) \
    { \
        if (a->kind == CVK_FLOAT) \
        { \
            return const_value_get_signed_int((__float128) a->value.f _binop b->value.f128); \
        } \
        else if (a->kind == CVK_DOUBLE) \
        { \
            return const_value_get_signed_int((__float128) a->value.d _binop b->value.f128); \
        } \
        else if (a->kind == CVK_LONG_DOUBLE) \
        { \
            return const_value_get_signed_int((__float128) a->value.ld _binop b->value.f128); \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } 

#define BOTH_ARE_FLOAT_SECOND_FLOAT128_FUN_REL(a, b, _func) \
    else if (b->kind == CVK_FLOAT128) \
    { \
        if (a->kind == CVK_FLOAT) \
        { \
            return const_value_get_signed_int(_func ## q((__float128) a->value.f, b->value.f128)); \
        } \
        else if (a->kind == CVK_DOUBLE) \
        { \
            return const_value_get_signed_int(_func ## q ((__float128) a->value.d, b->value.f128)); \
        } \
        else if (a->kind == CVK_LONG_DOUBLE) \
        { \
            return const_value_get_signed_int(_func ## q ((__float128) a->value.ld, b->value.f128)); \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } 

#else
   #define CAST_TO_FLOAT_FIRST_IS_FLOAT128(a, b, _binop)
   #define CAST_TO_FLOAT_FIRST_IS_FLOAT128_FUN(a, b, _func)

   #define CAST_TO_FLOAT_SECOND_IS_FLOAT128(a, b, _binop)
   #define CAST_TO_FLOAT_SECOND_IS_FLOAT128_FUN(a, b, _func)

   #define BOTH_ARE_SAME_FLOAT128(a, b, _binop)
   #define BOTH_ARE_SAME_FLOAT128_FUN(a, b, _func)

   #define BOTH_ARE_FLOAT_FIRST_FLOAT128(a, b, _binop)
   #define BOTH_ARE_FLOAT_SECOND_FLOAT128(a, b, _binop)

   #define BOTH_ARE_FLOAT_FIRST_FLOAT128_FUN(a, b, _func) 
   #define BOTH_ARE_FLOAT_SECOND_FLOAT128_FUN(a, b, _func) 

   #define CAST_TO_FLOAT_FIRST_IS_FLOAT128_REL(a, b, _binop)
   #define CAST_TO_FLOAT_FIRST_IS_FLOAT128_FUN_REL(a, b, _func)

   #define CAST_TO_FLOAT_SECOND_IS_FLOAT128_REL(a, b, _binop)
   #define CAST_TO_FLOAT_SECOND_IS_FLOAT128_FUN_REL(a, b, _func)

   #define BOTH_ARE_SAME_FLOAT128_REL(a, b, _binop)
   #define BOTH_ARE_SAME_FLOAT128_FUN_REL(a, b, _func)

   #define BOTH_ARE_FLOAT_FIRST_FLOAT128_REL(a, b, _binop)
   #define BOTH_ARE_FLOAT_SECOND_FLOAT128_REL(a, b, _binop)

   #define BOTH_ARE_FLOAT_FIRST_FLOAT128_FUN_REL(a, b, _func) 
   #define BOTH_ARE_FLOAT_SECOND_FLOAT128_FUN_REL(a, b, _func) 
#endif

#define CAST_TO_FLOAT_FIRST_IS_FLOAT(a, b, _binop) \
    if (a->kind == CVK_FLOAT) \
    { \
        if (!b->sign) \
            return const_value_get_float(a->value.f _binop (float) b->value.i); \
        else \
            return const_value_get_float(a->value.f _binop (float) b->value.si); \
    } \
    else if (a->kind == CVK_DOUBLE) \
    { \
        if (!b->sign) \
            return const_value_get_double(a->value.d _binop (double) b->value.i); \
        else \
            return const_value_get_double(a->value.d _binop (double) b->value.si); \
    } \
    else if (a->kind == CVK_LONG_DOUBLE) \
    { \
        if (!b->sign) \
            return const_value_get_long_double(a->value.ld _binop (long double) b->value.i); \
        else \
            return const_value_get_long_double(a->value.ld _binop (long double) b->value.si); \
    }  \
    CAST_TO_FLOAT_FIRST_IS_FLOAT128(a, b, _binop)

#define CAST_TO_FLOAT_FIRST_IS_FLOAT_FUN(a, b, _func) \
    if (a->kind == CVK_FLOAT) \
    { \
        if (!b->sign) \
            return const_value_get_float(_func ## f(a->value.f, (float) b->value.i)); \
        else \
            return const_value_get_float(_func ## f(a->value.f, (float) b->value.si)); \
    } \
    else if (a->kind == CVK_DOUBLE) \
    { \
        if (!b->sign) \
            return const_value_get_double(_func ## d(a->value.d, (double) b->value.i)); \
        else \
            return const_value_get_double(_func ## d(a->value.d, (double) b->value.si)); \
    } \
    else if (a->kind == CVK_LONG_DOUBLE) \
    { \
        if (!b->sign) \
            return const_value_get_long_double(_func ## ld(a->value.ld, (long double) b->value.i)); \
        else \
            return const_value_get_long_double(_func ## ld(a->value.ld, (long double) b->value.si)); \
    }  \
    CAST_TO_FLOAT_FIRST_IS_FLOAT128_FUN(a, b, _func)

#define CAST_TO_FLOAT_SECOND_IS_FLOAT(a, b, _binop) \
    if (b->kind == CVK_FLOAT) \
    { \
        if (!a->sign)\
            return const_value_get_float((float) a->value.i _binop b->value.f); \
        else \
            return const_value_get_float((float) a->value.si _binop b->value.f); \
    } \
    else if (b->kind == CVK_DOUBLE) \
    { \
        if (!a->sign)\
            return const_value_get_double((double) a->value.i _binop b->value.d); \
        else \
            return const_value_get_double((double) a->value.si _binop b->value.d); \
    } \
    else if (b->kind == CVK_LONG_DOUBLE) \
    { \
        if (!a->sign)\
            return const_value_get_long_double((long double) a->value.i _binop b->value.ld); \
        else \
            return const_value_get_long_double((long double) a->value.si _binop b->value.ld); \
    }  \
    CAST_TO_FLOAT_SECOND_IS_FLOAT128(a, b, _binop)

#define CAST_TO_FLOAT_SECOND_IS_FLOAT_FUN(a, b, _func) \
    if (b->kind == CVK_FLOAT) \
    { \
        if (!a->sign) \
            return const_value_get_float(_func##f((float) a->value.i, b->value.f)); \
        else \
            return const_value_get_float(_func##f((float) a->value.si, b->value.f)); \
    } \
    else if (b->kind == CVK_DOUBLE) \
    { \
        if (!a->sign) \
            return const_value_get_double(_func##d((double) a->value.i, b->value.d)); \
        else \
            return const_value_get_double(_func##d((double) a->value.si, b->value.d)); \
    } \
    else if (b->kind == CVK_LONG_DOUBLE) \
    { \
        if (!a->sign) \
            return const_value_get_long_double(_func##ld((long double) a->value.i, b->value.ld)); \
        else \
            return const_value_get_long_double(_func##ld((long double) a->value.si, b->value.ld)); \
    } \
    CAST_TO_FLOAT_SECOND_IS_FLOAT128_FUN(a, b, _func)

#define BOTH_ARE_SAME_FLOAT(a, b, _binop) \
    if (a->kind == CVK_FLOAT) \
        return const_value_get_float(a->value.f _binop b->value.f); \
    else if (a->kind == CVK_DOUBLE) \
        return const_value_get_double(a->value.d _binop b->value.d); \
    else if (a->kind == CVK_LONG_DOUBLE) \
        return const_value_get_long_double(a->value.ld _binop b->value.ld); \
    BOTH_ARE_SAME_FLOAT128(a, b, _binop)

#define BOTH_ARE_SAME_FLOAT_FUN(a, b, _func) \
    if (a->kind == CVK_FLOAT) \
        return const_value_get_float(_func ## f ( a->value.f, b->value.f)); \
    else if (a->kind == CVK_DOUBLE) \
        return const_value_get_double(_func ## d(a->value.d, b->value.d)); \
    else if (a->kind == CVK_LONG_DOUBLE) \
        return const_value_get_long_double(_func ## ld(a->value.ld, b->value.ld)); \
    BOTH_ARE_SAME_FLOAT128_FUN(a, b, _func)

#define BOTH_ARE_FLOAT_FIRST_LARGER(a, b, _binop) \
    if (a->kind == CVK_DOUBLE) \
    { \
        return const_value_get_double(a->value.d _binop (double) b->value.f); \
    } \
    else if (a->kind == CVK_LONG_DOUBLE) \
    { \
        if (b->kind == CVK_FLOAT) \
        { \
            return const_value_get_long_double(a->value.ld _binop (long double) b->value.f); \
        } \
        else if (b->kind == CVK_DOUBLE) \
        { \
            return const_value_get_long_double(a->value.ld _binop (long double) b->value.d); \
        } \
    } \
    BOTH_ARE_FLOAT_FIRST_FLOAT128(a, b, _binop)

#define BOTH_ARE_FLOAT_FIRST_LARGER_FUN(a, b, _func) \
    if (a->kind == CVK_DOUBLE) \
    { \
        return const_value_get_double(_func ## d(a->value.d, (double) b->value.f)); \
    } \
    else if (a->kind == CVK_LONG_DOUBLE) \
    { \
        if (b->kind == CVK_FLOAT) \
        { \
            return const_value_get_long_double(_func ## ld (a->value.ld, (long double) b->value.f)); \
        } \
        else if (b->kind == CVK_DOUBLE) \
        { \
            return const_value_get_long_double(_func ## ld(a->value.ld, (long double) b->value.d)); \
        } \
    } \
    BOTH_ARE_FLOAT_FIRST_FLOAT128_FUN(a, b, _func)

#define BOTH_ARE_FLOAT_SECOND_LARGER(a, b, _binop) \
    if (b->kind == CVK_DOUBLE) \
    { \
        return const_value_get_double((double)a->value.f _binop b->value.d); \
    } \
    else if (b->kind == CVK_LONG_DOUBLE) \
    { \
        if (a->kind == CVK_FLOAT) \
        { \
            return const_value_get_long_double((long double) a->value.f _binop b->value.ld); \
        } \
        else if (a->kind == CVK_DOUBLE) \
        { \
            return const_value_get_long_double((long double) a->value.d _binop b->value.ld); \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } \
    BOTH_ARE_FLOAT_SECOND_FLOAT128(a, b, _binop)

#define BOTH_ARE_FLOAT_SECOND_LARGER_FUN(a, b, _func) \
    if (b->kind == CVK_DOUBLE) \
    { \
        return const_value_get_double(_func ## d ((double)a->value.f, b->value.d)); \
    } \
    else if (b->kind == CVK_LONG_DOUBLE) \
    { \
        if (a->kind == CVK_FLOAT) \
        { \
            return const_value_get_long_double(_func ## ld((long double) a->value.f, b->value.ld)); \
        } \
        else if (a->kind == CVK_DOUBLE) \
        { \
            return const_value_get_long_double(_func ## ld ((long double) a->value.d, b->value.ld)); \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } \
    BOTH_ARE_FLOAT_SECOND_FLOAT128_FUN(a, b, _func)

#define BINOP_FUN(_opname, _binop) \
const_value_t* const_value_##_opname(const_value_t* v1, const_value_t* v2) \
{ \
    ERROR_CONDITION(v1 == NULL || v2 == NULL, "Either of the parameters is NULL", 0); \
    if ((v1->kind == CVK_INTEGER) \
            && (v2->kind == CVK_INTEGER)) \
    { \
       int bytes = 0; char sign = 0; \
       common_bytes(v1, v2, &bytes, &sign); \
       cvalue_uint_t value = 0; \
       if (sign) \
       { \
           (*((cvalue_int_t*)&value)) = v1->value.si _binop v2->value.si; \
       } \
       else \
       { \
           value = v1->value.i _binop v2->value.i; \
       } \
       return const_value_get_integer(value, bytes, sign); \
    } \
    else if (IS_FLOAT(v1->kind) \
            && (v2->kind == CVK_INTEGER)) \
    { \
        CAST_TO_FLOAT_FIRST_IS_FLOAT(v1, v2, _binop) \
    } \
    else if (v1->kind == CVK_INTEGER \
            && IS_FLOAT(v2->kind)) \
    { \
        CAST_TO_FLOAT_SECOND_IS_FLOAT(v1, v2, _binop) \
    } \
    else if (IS_FLOAT(v1->kind) && IS_FLOAT(v2->kind)) \
    { \
        if (v1->kind == v2->kind) \
        { \
            BOTH_ARE_SAME_FLOAT(v1, v2, _binop) \
            else { internal_error("Code unreachable", 0) }; \
        } \
        else if (v1->kind > v2->kind) \
        { \
            BOTH_ARE_FLOAT_FIRST_LARGER(v1, v2, _binop) \
            else { internal_error("Code unreachable", 0) }; \
        } \
        else if (v1->kind < v2->kind) \
        { \
            BOTH_ARE_FLOAT_SECOND_LARGER(v1, v2, _binop) \
            else { internal_error("Code unreachable", 0) }; \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } \
    else if (v1->kind == CVK_COMPLEX && v2->kind == CVK_COMPLEX) \
    { \
        return complex_##_opname (v1, v2); \
    } \
    else if (v1->kind == CVK_COMPLEX && v2->kind != CVK_COMPLEX) \
    { \
        return const_value_##_opname ( v1, const_value_real_to_complex(v2) ); \
    } \
    else if (v1->kind != CVK_COMPLEX && v2->kind == CVK_COMPLEX) \
    { \
        return const_value_##_opname ( const_value_real_to_complex(v1), v2 ); \
    } \
    else if (IS_MULTIVALUE(v1->kind) \
            && IS_MULTIVALUE(v2->kind) \
            && (multival_get_num_elements(v1) == multival_get_num_elements(v2))) \
    { \
        return map_binary_to_structured_value( const_value_##_opname, v1, v2); \
    } \
    else if (IS_MULTIVALUE(v1->kind)) \
    { \
        return extend_first_operand_to_structured_value( const_value_##_opname, v1, v2); \
    } \
    else if (IS_MULTIVALUE(v2->kind)) \
    { \
        return extend_second_operand_to_structured_value( const_value_##_opname, v1, v2); \
    } \
    internal_error("Code unreachable", 0); \
}

#define BINOP_FUN_CALL(_opname, _func) \
const_value_t* const_value_##_opname(const_value_t* v1, const_value_t* v2) \
{ \
    ERROR_CONDITION(v1 == NULL || v2 == NULL, "Either of the parameters is NULL", 0); \
    if (v1->kind == CVK_INTEGER \
            && v2->kind == CVK_INTEGER) \
    { \
       int bytes = 0; char sign = 0; \
       common_bytes(v1, v2, &bytes, &sign); \
       cvalue_uint_t value = 0; \
       if (sign) \
       { \
           *(cvalue_int_t*)&value = _func ## s ( v1->value.si, v2->value.si); \
       } \
       else \
       { \
        \
           value = _func ## u( v1->value.i, v2->value.i ); \
       } \
       return const_value_get_integer(value, bytes, sign); \
    } \
    else if (v2->kind == CVK_INTEGER \
            && IS_FLOAT(v1->kind)) \
    { \
        CAST_TO_FLOAT_FIRST_IS_FLOAT_FUN(v1, v2, _func) \
    } \
    else if (v1->kind == CVK_INTEGER \
            && IS_FLOAT(v2->kind)) \
    { \
        CAST_TO_FLOAT_SECOND_IS_FLOAT_FUN(v1, v2, _func) \
    } \
    else if (IS_FLOAT(v1->kind) && IS_FLOAT(v2->kind)) \
    { \
        if (v1->kind == v2->kind) \
        { \
            BOTH_ARE_SAME_FLOAT_FUN(v1, v2, _func) \
            else { internal_error("Code unreachable", 0) }; \
        } \
        else if (v1->kind > v2->kind) \
        { \
            BOTH_ARE_FLOAT_FIRST_LARGER_FUN(v1, v2, _func) \
            else { internal_error("Code unreachable", 0) }; \
        } \
        else if (v1->kind < v2->kind) \
        { \
            BOTH_ARE_FLOAT_SECOND_LARGER_FUN(v1, v2, _func) \
            else { internal_error("Code unreachable", 0) }; \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } \
    else if (v1->kind == CVK_COMPLEX && v2->kind == CVK_COMPLEX) \
    { \
        return _func ## z (v1, v2); \
    } \
    else if (v1->kind == CVK_COMPLEX && v2->kind != CVK_COMPLEX) \
    { \
        return const_value_##_opname ( v1, const_value_real_to_complex(v2) ); \
    } \
    else if (v1->kind != CVK_COMPLEX && v2->kind == CVK_COMPLEX) \
    { \
        return const_value_##_opname ( const_value_real_to_complex(v1), v2 ); \
    } \
    else if (IS_MULTIVALUE(v1->kind) \
            && IS_MULTIVALUE(v2->kind) \
            && (multival_get_num_elements(v1) == multival_get_num_elements(v2))) \
    { \
        return map_binary_to_structured_value( const_value_##_opname, v1, v2); \
    } \
    else if (IS_MULTIVALUE(v1->kind)) \
    { \
        return extend_first_operand_to_structured_value( const_value_##_opname, v1, v2); \
    } \
    else if (IS_MULTIVALUE(v2->kind)) \
    { \
        return extend_second_operand_to_structured_value( const_value_##_opname, v1, v2); \
    } \
    internal_error("Code unreachable", 0); \
}

// Relational

#define CAST_TO_FLOAT_FIRST_IS_FLOAT_REL(a, b, _binop) \
    if (a->kind == CVK_FLOAT) \
    { \
        if (!b->sign) \
            return const_value_get_signed_int(a->value.f _binop (float) b->value.i); \
        else \
            return const_value_get_signed_int(a->value.f _binop (float) b->value.si); \
    } \
    else if (a->kind == CVK_DOUBLE) \
    { \
        if (!b->sign) \
            return const_value_get_signed_int(a->value.d _binop (double) b->value.i); \
        else \
            return const_value_get_signed_int(a->value.d _binop (double) b->value.si); \
    } \
    else if (a->kind == CVK_LONG_DOUBLE) \
    { \
        if (!b->sign) \
            return const_value_get_signed_int(a->value.ld _binop (long double) b->value.i); \
        else \
            return const_value_get_signed_int(a->value.ld _binop (long double) b->value.si); \
    }  \
    CAST_TO_FLOAT_FIRST_IS_FLOAT128_REL(a, b, _binop)

#define CAST_TO_FLOAT_FIRST_IS_FLOAT_FUN_REL(a, b, _func) \
    if (a->kind == CVK_FLOAT) \
    { \
        if (!b->sign) \
            return const_value_get_signed_int(_func ## f(a->value.f, (float) b->value.i)); \
        else \
            return const_value_get_signed_int(_func ## f(a->value.f, (float) b->value.si)); \
    } \
    else if (a->kind == CVK_DOUBLE) \
    { \
        if (!b->sign) \
            return const_value_get_signed_int(_func ## d(a->value.d, (double) b->value.i)); \
        else \
            return const_value_get_signed_int(_func ## d(a->value.d, (double) b->value.si)); \
    } \
    else if (a->kind == CVK_LONG_DOUBLE) \
    { \
        if (!b->sign) \
            return const_value_get_signed_int(_func ## ld(a->value.ld, (long double) b->value.i)); \
        else \
            return const_value_get_signed_int(_func ## ld(a->value.ld, (long double) b->value.si)); \
    }  \
    CAST_TO_FLOAT_FIRST_IS_FLOAT128_FUN_REL(a, b, _func)

#define CAST_TO_FLOAT_SECOND_IS_FLOAT_REL(a, b, _binop) \
    if (b->kind == CVK_FLOAT) \
    { \
        if (!a->sign)\
            return const_value_get_signed_int((float) a->value.i _binop b->value.f); \
        else \
            return const_value_get_signed_int((float) a->value.si _binop b->value.f); \
    } \
    else if (b->kind == CVK_DOUBLE) \
    { \
        if (!a->sign)\
            return const_value_get_signed_int((double) a->value.i _binop b->value.d); \
        else \
            return const_value_get_signed_int((double) a->value.si _binop b->value.d); \
    } \
    else if (b->kind == CVK_LONG_DOUBLE) \
    { \
        if (!a->sign)\
            return const_value_get_signed_int((long double) a->value.i _binop b->value.ld); \
        else \
            return const_value_get_signed_int((long double) a->value.si _binop b->value.ld); \
    }  \
    CAST_TO_FLOAT_SECOND_IS_FLOAT128_REL(a, b, _binop)

#define CAST_TO_FLOAT_SECOND_IS_FLOAT_FUN_REL(a, b, _func) \
    if (b->kind == CVK_FLOAT) \
    { \
        if (!a->sign) \
            return const_value_get_signed_int(_func##f((float) a->value.i, b->value.f)); \
        else \
            return const_value_get_signed_int(_func##f((float) a->value.si, b->value.f)); \
    } \
    else if (b->kind == CVK_DOUBLE) \
    { \
        if (!a->sign) \
            return const_value_get_signed_int(_func##d((double) a->value.i, b->value.d)); \
        else \
            return const_value_get_signed_int(_func##d((double) a->value.si, b->value.d)); \
    } \
    else if (b->kind == CVK_LONG_DOUBLE) \
    { \
        if (!a->sign) \
            return const_value_get_signed_int(_func##ld((long double) a->value.i, b->value.ld)); \
        else \
            return const_value_get_signed_int(_func##ld((long double) a->value.si, b->value.ld)); \
    } \
    CAST_TO_FLOAT_SECOND_IS_FLOAT128_FUN_REL(a, b, _func)

#define BOTH_ARE_SAME_FLOAT_REL(a, b, _binop) \
    if (a->kind == CVK_FLOAT) \
        return const_value_get_signed_int(a->value.f _binop b->value.f); \
    else if (a->kind == CVK_DOUBLE) \
        return const_value_get_signed_int(a->value.d _binop b->value.d); \
    else if (a->kind == CVK_LONG_DOUBLE) \
        return const_value_get_signed_int(a->value.ld _binop b->value.ld); \
    BOTH_ARE_SAME_FLOAT128(a, b, _binop)

#define BOTH_ARE_SAME_FLOAT_FUN_REL(a, b, _func) \
    if (a->kind == CVK_FLOAT) \
        return const_value_get_signed_int(_func ## f ( a->value.f, b->value.f)); \
    else if (a->kind == CVK_DOUBLE) \
        return const_value_get_signed_int(_func ## d(a->value.d, b->value.d)); \
    else if (a->kind == CVK_LONG_DOUBLE) \
        return const_value_get_signed_int(_func ## ld(a->value.ld, b->value.ld)); \
    BOTH_ARE_SAME_FLOAT128_FUN(a, b, _func)

#define BOTH_ARE_FLOAT_FIRST_LARGER_REL(a, b, _binop) \
    if (a->kind == CVK_DOUBLE) \
    { \
        return const_value_get_signed_int(a->value.d _binop (double) b->value.f); \
    } \
    else if (a->kind == CVK_LONG_DOUBLE) \
    { \
        if (b->kind == CVK_FLOAT) \
        { \
            return const_value_get_signed_int(a->value.ld _binop (long double) b->value.f); \
        } \
        else if (b->kind == CVK_DOUBLE) \
        { \
            return const_value_get_signed_int(a->value.ld _binop (long double) b->value.d); \
        } \
    } \
    BOTH_ARE_FLOAT_FIRST_FLOAT128(a, b, _binop)

#define BOTH_ARE_FLOAT_FIRST_LARGER_FUN_REL(a, b, _func) \
    if (a->kind == CVK_DOUBLE) \
    { \
        return const_value_get_signed_int(_func ## d(a->value.d, (double) b->value.f)); \
    } \
    else if (a->kind == CVK_LONG_DOUBLE) \
    { \
        if (b->kind == CVK_FLOAT) \
        { \
            return const_value_get_signed_int(_func ## ld (a->value.ld, (long double) b->value.f)); \
        } \
        else if (b->kind == CVK_DOUBLE) \
        { \
            return const_value_get_signed_int(_func ## ld(a->value.ld, (long double) b->value.d)); \
        } \
    } \
    BOTH_ARE_FLOAT_FIRST_FLOAT128_FUN(a, b, _func)

#define BOTH_ARE_FLOAT_SECOND_LARGER_REL(a, b, _binop) \
    if (b->kind == CVK_DOUBLE) \
    { \
        return const_value_get_signed_int((double)a->value.f _binop b->value.d); \
    } \
    else if (b->kind == CVK_LONG_DOUBLE) \
    { \
        if (a->kind == CVK_FLOAT) \
        { \
            return const_value_get_signed_int((long double) a->value.f _binop b->value.ld); \
        } \
        else if (a->kind == CVK_DOUBLE) \
        { \
            return const_value_get_signed_int((long double) a->value.d _binop b->value.ld); \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } \
    BOTH_ARE_FLOAT_SECOND_FLOAT128(a, b, _binop)

#define BOTH_ARE_FLOAT_SECOND_LARGER_FUN_REL(a, b, _func) \
    if (b->kind == CVK_DOUBLE) \
    { \
        return const_value_get_signed_int(_func ## d ((double)a->value.f, b->value.d)); \
    } \
    else if (b->kind == CVK_LONG_DOUBLE) \
    { \
        if (a->kind == CVK_FLOAT) \
        { \
            return const_value_get_signed_int(_func ## ld((long double) a->value.f, b->value.ld)); \
        } \
        else if (a->kind == CVK_DOUBLE) \
        { \
            return const_value_get_signed_int(_func ## ld ((long double) a->value.d, b->value.ld)); \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } \
    BOTH_ARE_FLOAT_SECOND_FLOAT128_FUN(a, b, _func)

#define BINOP_FUN_REL(_opname, _binop, _reduce_multival) \
const_value_t* const_value_##_opname(const_value_t* v1, const_value_t* v2) \
{ \
    ERROR_CONDITION(v1 == NULL || v2 == NULL, "Either of the parameters is NULL", 0); \
    if ((v1->kind == CVK_INTEGER) \
            && (v2->kind == CVK_INTEGER)) \
    { \
       int bytes = 0; char sign = 0; \
       common_bytes(v1, v2, &bytes, &sign); \
       cvalue_uint_t value = 0; \
       if (sign) \
       { \
           (*((cvalue_int_t*)&value)) = v1->value.si _binop v2->value.si; \
       } \
       else \
       { \
           value = v1->value.i _binop v2->value.i; \
       } \
       return const_value_get_signed_int(value); \
    } \
    else if (IS_FLOAT(v1->kind) \
            && (v2->kind == CVK_INTEGER)) \
    { \
        CAST_TO_FLOAT_FIRST_IS_FLOAT_REL(v1, v2, _binop) \
    } \
    else if (v1->kind == CVK_INTEGER \
            && IS_FLOAT(v2->kind)) \
    { \
        CAST_TO_FLOAT_SECOND_IS_FLOAT_REL(v1, v2, _binop) \
    } \
    else if (IS_FLOAT(v1->kind) && IS_FLOAT(v2->kind)) \
    { \
        if (v1->kind == v2->kind) \
        { \
            BOTH_ARE_SAME_FLOAT_REL(v1, v2, _binop) \
            else { internal_error("Code unreachable", 0) }; \
        } \
        else if (v1->kind > v2->kind) \
        { \
            BOTH_ARE_FLOAT_FIRST_LARGER_REL(v1, v2, _binop) \
            else { internal_error("Code unreachable", 0) }; \
        } \
        else if (v1->kind < v2->kind) \
        { \
            BOTH_ARE_FLOAT_SECOND_LARGER_REL(v1, v2, _binop) \
            else { internal_error("Code unreachable", 0) }; \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } \
    else if (v1->kind == CVK_COMPLEX && v2->kind == CVK_COMPLEX) \
    { \
        return complex_##_opname (v1, v2); \
    } \
    else if (v1->kind == CVK_COMPLEX && v2->kind != CVK_COMPLEX) \
    { \
        return const_value_##_opname ( v1, const_value_real_to_complex(v2) ); \
    } \
    else if (v1->kind != CVK_COMPLEX && v2->kind == CVK_COMPLEX) \
    { \
        return const_value_##_opname ( const_value_real_to_complex(v1), v2 ); \
    } \
    else if (IS_MULTIVALUE(v1->kind) \
            && IS_MULTIVALUE(v2->kind) \
            && (multival_get_num_elements(v1) == multival_get_num_elements(v2))) \
    { \
        return _reduce_multival( const_value_##_opname, v1, v2); \
    } \
    else if (IS_MULTIVALUE(v1->kind)) \
    { \
        return extend_first_operand_to_structured_value( const_value_##_opname, v1, v2); \
    } \
    else if (IS_MULTIVALUE(v2->kind)) \
    { \
        return extend_second_operand_to_structured_value( const_value_##_opname, v1, v2); \
    } \
    internal_error("Code unreachable", 0); \
}

#define BINOP_FUN_CALL_REL(_opname, _func, _reduce_multival) \
const_value_t* const_value_##_opname(const_value_t* v1, const_value_t* v2) \
{ \
    ERROR_CONDITION(v1 == NULL || v2 == NULL, "Either of the parameters is NULL", 0); \
    if (v1->kind == CVK_INTEGER \
            && v2->kind == CVK_INTEGER) \
    { \
       int bytes = 0; char sign = 0; \
       common_bytes(v1, v2, &bytes, &sign); \
       cvalue_uint_t value = 0; \
       if (sign) \
       { \
           *(cvalue_int_t*)&value = _func ## s ( v1->value.si, v2->value.si); \
       } \
       else \
       { \
        \
           value = _func ## u( v1->value.i, v2->value.i ); \
       } \
       return const_value_get_signed_int(value); \
    } \
    else if (v2->kind == CVK_INTEGER \
            && IS_FLOAT(v1->kind)) \
    { \
        CAST_TO_FLOAT_FIRST_IS_FLOAT_FUN_REL(v1, v2, _func) \
    } \
    else if (v1->kind == CVK_INTEGER \
            && IS_FLOAT(v2->kind)) \
    { \
        CAST_TO_FLOAT_SECOND_IS_FLOAT_FUN_REL(v1, v2, _func) \
    } \
    else if (IS_FLOAT(v1->kind) && IS_FLOAT(v2->kind)) \
    { \
        if (v1->kind == v2->kind) \
        { \
            BOTH_ARE_SAME_FLOAT_FUN(v1, v2, _func) \
            else { internal_error("Code unreachable", 0) }; \
        } \
        else if (v1->kind > v2->kind) \
        { \
            BOTH_ARE_FLOAT_FIRST_LARGER_FUN_REL(v1, v2, _func) \
            else { internal_error("Code unreachable", 0) }; \
        } \
        else if (v1->kind < v2->kind) \
        { \
            BOTH_ARE_FLOAT_SECOND_LARGER_FUN_REL(v1, v2, _func) \
            else { internal_error("Code unreachable", 0) }; \
        } \
        else { internal_error("Code unreachable", 0) }; \
    } \
    else if (v1->kind == CVK_COMPLEX && v2->kind == CVK_COMPLEX) \
    { \
        return _func ## z (v1, v2); \
    } \
    else if (v1->kind == CVK_COMPLEX && v2->kind != CVK_COMPLEX) \
    { \
        return const_value_##_opname ( v1, const_value_real_to_complex(v2) ); \
    } \
    else if (v1->kind != CVK_COMPLEX && v2->kind == CVK_COMPLEX) \
    { \
        return const_value_##_opname ( const_value_real_to_complex(v1), v2 ); \
    } \
    else if (IS_MULTIVALUE(v1->kind) \
            && IS_MULTIVALUE(v2->kind)) \
    { \
        return _reduce_multival ( const_value_##_opname, v1, v2 ); \
    } \
    else if (IS_MULTIVALUE(v1->kind)) \
    { \
        return extend_first_operand_to_structured_value( const_value_##_opname, v1, v2); \
    } \
    else if (IS_MULTIVALUE(v2->kind)) \
    { \
        return extend_second_operand_to_structured_value( const_value_##_opname, v1, v2); \
    } \
    internal_error("Code unreachable", 0); \
}

static const_value_t* complex_add(const_value_t*, const_value_t*);
static const_value_t* complex_sub(const_value_t*, const_value_t*);
static const_value_t* complex_mul(const_value_t*, const_value_t*);
static const_value_t* complex_div(const_value_t*, const_value_t*);
static const_value_t* complex_and(const_value_t*, const_value_t*);
static const_value_t* complex_or(const_value_t*, const_value_t*);
static const_value_t* complex_lt(const_value_t*, const_value_t*);
static const_value_t* complex_lte(const_value_t*, const_value_t*);
static const_value_t* complex_gt(const_value_t*, const_value_t*);
static const_value_t* complex_gte(const_value_t*, const_value_t*);
static const_value_t* complex_eq(const_value_t*, const_value_t*);
static const_value_t* complex_neq(const_value_t*, const_value_t*);
static const_value_t* arith_powz(const_value_t*, const_value_t*);

static const_value_t* reduce_lexicographic_lt(
        const_value_t* (*)(const_value_t*, const_value_t*),
        const_value_t*,
        const_value_t*);
static const_value_t* reduce_lexicographic_lte(
        const_value_t* (*)(const_value_t*, const_value_t*),
        const_value_t*,
        const_value_t*);
static const_value_t* reduce_lexicographic_gt(
        const_value_t* (*)(const_value_t*, const_value_t*),
        const_value_t*,
        const_value_t*);
static const_value_t* reduce_lexicographic_gte(
        const_value_t* (*)(const_value_t*, const_value_t*),
        const_value_t*,
        const_value_t*);
static const_value_t* reduce_equal_values_and_length(
        const_value_t* (*)(const_value_t*, const_value_t*),
        const_value_t*,
        const_value_t*);
static const_value_t* reduce_different_values_or_length(
        const_value_t* (*)(const_value_t*, const_value_t*),
        const_value_t*,
        const_value_t*);

BINOP_FUN(add, +)
BINOP_FUN(sub, -)
BINOP_FUN(mul, *)
BINOP_FUN(div, /)
BINOP_FUN_I(mod, %)
BINOP_FUN_I(shr, >>)
BINOP_FUN_I(bitshl, <<)
BINOP_FUN_I(bitand, &)
BINOP_FUN_I(bitor, |)
BINOP_FUN_I(bitxor, ^)
BINOP_FUN(and, &&)
BINOP_FUN(or, ||)
BINOP_FUN_REL(lt, <, reduce_lexicographic_lt)
BINOP_FUN_REL(lte, <=, reduce_lexicographic_lte)
BINOP_FUN_REL(gt, >, reduce_lexicographic_gt)
BINOP_FUN_REL(gte, >=, reduce_lexicographic_gte)
BINOP_FUN_REL(eq, ==, reduce_equal_values_and_length)
BINOP_FUN_REL(neq, !=, reduce_different_values_or_length)

static cvalue_uint_t arith_powu(cvalue_uint_t a, cvalue_uint_t b)
{
    if (b == 0)
    {
        return 1;
    }
    else if ((b & (cvalue_uint_t)1) == (cvalue_uint_t)1) // odd
    {
        cvalue_uint_t k = arith_powu(a, (b-1) >> 1);
        return a * k * k;
    }
    else // even
    {
        cvalue_uint_t k = arith_powu(a, b >> 1);
        return k * k;
    }
}

static cvalue_int_t arith_pows(cvalue_int_t a, cvalue_int_t b)
{
    if (b == 0)
    {
        return 1;
    }
    else if (b < 0)
    {
        return 1 / arith_pows(a, -b);
    }
    else if ((b & (cvalue_int_t)1) == (cvalue_int_t)1) // odd
    {
        cvalue_int_t k = arith_powu(a, (b-1) >> 1);
        return a * k * k;
    }
    else // even
    {
        cvalue_int_t k = arith_powu(a, b >> 1);
        return k * k;
    }
}

static float arith_powf(float a, float b)
{
    return powf(a, b);
}

static double arith_powd(double a, double b)
{
    return pow(a, b);
}

static long double arith_powld(long double a, long double b)
{
    return powl(a, b);
}

#ifdef HAVE_QUADMATH_H
static long double arith_powq(__float128 a, __float128 b)
{
    return powq(a, b);
}
#endif

BINOP_FUN_CALL(pow, arith_pow)

#ifndef HAVE_QUADMATH_H
#define UNOP_FUN_FLOAT128(_unop)
#else
#define UNOP_FUN_FLOAT128(_unop) \
    else if (v1->kind == CVK_FLOAT128) \
    { \
        return const_value_get_float128(_unop v1->value.f128); \
    }
#endif

#define UNOP_FUN(_opname, _unop) \
const_value_t* const_value_##_opname(const_value_t* v1) \
{ \
    ERROR_CONDITION(v1 == NULL, "Parameter cannot be NULL", 0); \
    if (v1->kind == CVK_INTEGER) \
    { \
        cvalue_uint_t value = 0; \
        if (v1->sign) \
        { \
            value = _unop v1->value.si; \
        } \
        else \
        { \
            value = _unop v1->value.i; \
        } \
        return const_value_get_integer(value, v1->num_bytes, v1->sign); \
    } \
    else if (v1->kind == CVK_FLOAT) \
    { \
        return const_value_get_float(_unop v1->value.f); \
    } \
    else if (v1->kind == CVK_DOUBLE) \
    { \
        return const_value_get_double(_unop v1->value.d); \
    } \
    else if (v1->kind == CVK_LONG_DOUBLE) \
    { \
        return const_value_get_long_double(_unop v1->value.ld); \
    } \
    UNOP_FUN_FLOAT128(_unop); \
    return NULL; \
}

#ifndef HAVE_QUADMATH_H
#define UNOP_FUN_I_OR_F_FLOAT128(_unop)
#else
#define UNOP_FUN_I_OR_F_FLOAT128(_unop) \
    else if (v1->kind == CVK_FLOAT128) \
    { \
        return const_value_get_signed_int(_unop v1->value.f128); \
    }
#endif

#define UNOP_FUN_I_OR_F(_opname, _unop) \
const_value_t* const_value_##_opname(const_value_t* v1) \
{ \
    ERROR_CONDITION(v1 == NULL, "Parameter cannot be NULL", 0); \
    if (v1->kind == CVK_INTEGER) \
    { \
        cvalue_uint_t value = 0; \
        if (v1->sign) \
        { \
            value = _unop v1->value.si; \
        } \
        else \
        { \
            value = _unop v1->value.i; \
        } \
        return const_value_get_integer(value, v1->num_bytes, v1->sign); \
    } \
    else if (v1->kind == CVK_FLOAT) \
    { \
        return const_value_get_signed_int(_unop v1->value.f); \
    } \
    else if (v1->kind == CVK_DOUBLE) \
    { \
        return const_value_get_signed_int(_unop v1->value.d); \
    } \
    else if (v1->kind == CVK_LONG_DOUBLE) \
    { \
        return const_value_get_signed_int(_unop v1->value.ld); \
    } \
    UNOP_FUN_I_OR_F_FLOAT128(_unop); \
    return NULL; \
}

#define UNOP_FUN_I(_opname, _unop) \
const_value_t* const_value_##_opname(const_value_t* v1) \
{ \
    ERROR_CONDITION(v1 == NULL, "Parameter cannot be NULL", 0); \
    if (v1->kind == CVK_INTEGER) \
    { \
        cvalue_uint_t value = 0; \
        if (v1->sign) \
        { \
            value = _unop v1->value.si; \
        } \
        else \
        { \
            value = _unop v1->value.i; \
        } \
        return const_value_get_integer(value, v1->num_bytes, v1->sign); \
    } \
    return NULL; \
}

UNOP_FUN(plus, +)
UNOP_FUN(neg, -)
UNOP_FUN_I(bitnot, ~)
UNOP_FUN_I_OR_F(not, !)

#define MEANINGLESS_IN_COMPLEX(name) \
static const_value_t* name(const_value_t* v1 UNUSED_PARAMETER, const_value_t* v2 UNUSED_PARAMETER) \
{ \
    internal_error("Current operator makes no sense for complex numbers", 0); \
}

MEANINGLESS_IN_COMPLEX(complex_or)
MEANINGLESS_IN_COMPLEX(complex_and)
MEANINGLESS_IN_COMPLEX(complex_lt)
MEANINGLESS_IN_COMPLEX(complex_lte)
MEANINGLESS_IN_COMPLEX(complex_gt)
MEANINGLESS_IN_COMPLEX(complex_gte)

static const_value_t* complex_add(const_value_t* v1, const_value_t* v2)
{
    return const_value_make_complex(
            const_value_add(
                const_value_complex_get_real_part(v1),
                const_value_complex_get_real_part(v2)),
            const_value_add(
                const_value_complex_get_imag_part(v1),
                const_value_complex_get_imag_part(v2)));
}

static const_value_t* complex_sub(const_value_t* v1, const_value_t* v2)
{
    return const_value_make_complex(
            const_value_sub(
                const_value_complex_get_real_part(v1),
                const_value_complex_get_real_part(v2)),
            const_value_sub(
                const_value_complex_get_imag_part(v1),
                const_value_complex_get_imag_part(v2)));
}

static const_value_t* complex_mul(const_value_t* v1, const_value_t* v2)
{
    // Mathematically this can be done with just three multiplications
    // but maybe is not the numerically the same
    //
    // (a, b) * (c, d) = (a*c - b*d , a*d + b*c)
    const_value_t* a = const_value_complex_get_real_part(v1);
    const_value_t* b = const_value_complex_get_imag_part(v1);
    const_value_t* c = const_value_complex_get_real_part(v2);
    const_value_t* d = const_value_complex_get_imag_part(v2);

    return const_value_make_complex(
            const_value_sub(
                const_value_mul(a, c),
                const_value_mul(b, d)),
            const_value_add(
                const_value_mul(a, d),
                const_value_mul(b, c)));
}

static const_value_t* complex_div(const_value_t* v1, const_value_t* v2)
{
    // (a, b) / (c, d) = (T1 / K, T2 / K)
    //   where 
    //       T1 = a*c + b*d
    //       T2 = b*c - a*d
    //       K = c**2 + d**2
    const_value_t* a = const_value_complex_get_real_part(v1);
    const_value_t* b = const_value_complex_get_imag_part(v1);
    const_value_t* c = const_value_complex_get_real_part(v2);
    const_value_t* d = const_value_complex_get_imag_part(v2);

    const_value_t* K = const_value_add(const_value_mul(c, c), const_value_mul(d, d));

    const_value_t* T1 = const_value_add(const_value_mul(a, c), const_value_mul(b,d));
    const_value_t* T2 = const_value_sub(const_value_mul(b, c), const_value_mul(a,d));

    return const_value_make_complex(
            const_value_div(T1, K),
            const_value_div(T2, K));
}

static const_value_t* complex_eq(const_value_t* v1, const_value_t* v2)
{
    const_value_t* eq_real = 
        const_value_eq(
                const_value_complex_get_real_part(v1),
                const_value_complex_get_real_part(v2));
    const_value_t* eq_imag = 
        const_value_eq(
                const_value_complex_get_imag_part(v1),
                const_value_complex_get_imag_part(v2));

    return const_value_and(eq_real, eq_imag);
}

static const_value_t* complex_neq(const_value_t* v1, const_value_t* v2)
{
    return const_value_not(complex_eq(v1, v2));
}

static const_value_kind_t get_common_complex_kind(const_value_t *v1, const_value_t *v2)
{
    const_value_kind_t kind1 = v1->value.m->elements[0]->kind; 
    const_value_kind_t kind2 = v2->value.m->elements[0]->kind; 

    if (kind1 > kind2)
        return kind1;
    else
        return kind2;
}

static const_value_t* arith_powz(const_value_t* v1, const_value_t* v2)
{
    switch (get_common_complex_kind(v1, v2))
    {
        case CVK_FLOAT:
            {
                return const_value_get_complex_float(
                        cpowf(const_value_cast_to_complex_float(v1),
                            const_value_cast_to_complex_float(v2)));
                break;
            }
        case CVK_DOUBLE:
            {
                return const_value_get_complex_double(
                        cpow(const_value_cast_to_complex_double(v1),
                            const_value_cast_to_complex_double(v2)));
                break;
            }
        case CVK_LONG_DOUBLE:
            {
                return const_value_get_complex_double(
                        cpowl(const_value_cast_to_complex_double(v1),
                            const_value_cast_to_complex_double(v2)));
                break;
            }
#ifdef HAVE_QUADMATH_H
        case CVK_FLOAT128:
            {
                return const_value_get_complex_float128(
                        cpowq(const_value_cast_to_complex_float128(v1),
                            const_value_cast_to_complex_float128(v2)));
                break;
            }
#endif
        default:
            {
                internal_error("Code unreachable", 0);
                break;
            }
    }
}

void const_value_string_unpack_to_int(const_value_t* v, int **values, int *num_elements)
{
    ERROR_CONDITION(v->kind != CVK_STRING, "Invalid data type", 0);

    int *result = xcalloc(const_value_get_num_elements(v), sizeof(*result));

    int i, nels = const_value_get_num_elements(v);
    for (i = 0; i < nels; i++)
    {
        result[i] = const_value_cast_to_4(v->value.m->elements[i]);
    }

    *num_elements = nels;
    *values = result;
}

const char *const_value_string_unpack_to_string(const_value_t* v)
{
    int *values = NULL, num_elements = 0;
    const_value_string_unpack_to_int(v, &values, &num_elements);

    char str[num_elements + 1];
    int i;
    for (i = 0; i < num_elements; i++)
    {
        str[i] = (char)values[i];
    }
    str[num_elements] = '\0';

    return uniquestr(str);
}

const_value_t* const_value_string_concat(const_value_t* v1, const_value_t* v2)
{
    ERROR_CONDITION(v1->kind != v2->kind || v1->kind != CVK_STRING, "Invalid data types for concatenation", 0);

    // Rebuild the string
    int nelems1 = const_value_get_num_elements(v1);
    int nelems2 = const_value_get_num_elements(v2);
    int num_elements = nelems1 + nelems2 ;
    char new_string[num_elements + 1];

    int p = 0;
    int i;
    for (i = 0; i < nelems1; i++, p++)
    {
        new_string[p] = const_value_cast_to_1(const_value_get_element_num(v1, i));
    }
    for (i = 0; i < nelems2; i++, p++)
    {
        new_string[p] = const_value_cast_to_1(const_value_get_element_num(v2, i));
    }

    new_string[num_elements] = '\0';

    const char* str = uniquestr(new_string);

    return const_value_make_string(str, num_elements);
}

const_value_t* const_value_cast_as_another(const_value_t* val, const_value_t* mold)
{
    if (const_value_is_integer(mold))
    {
        return const_value_cast_to_bytes(val, const_value_get_bytes(mold), const_value_is_signed(mold));
    }
    else if (const_value_is_floating(mold))
    {
        if (const_value_is_float(mold))
        {
            return const_value_cast_to_float_value(val);
        }
        else if (const_value_is_double(mold))
        {
            return const_value_cast_to_double_value(val);
        }
        else if (const_value_is_long_double(mold))
        {
            return const_value_cast_to_long_double_value(val);
        }
#ifdef HAVE_QUADMATH_H
        else if (const_value_is_float128(mold))
        {
            return const_value_cast_to_float128_value(val);
        }
#endif
    }
    else
    {
        internal_error("Mold is not valid for this cast", 0);
    }
    return NULL;
}

const_value_t* const_value_square(const_value_t* val)
{
    const_value_t* result = const_value_mul(val, val);

    return result;
}

const_value_t* const_value_sqrt(const_value_t* val)
{
    if (const_value_is_float(val))
    {
        return const_value_get_float(sqrtf(val->value.f));
    }
    else if (const_value_is_double(val))
    {
        return const_value_get_double(sqrt(val->value.d));
    }
    else if (const_value_is_long_double(val))
    {
        return const_value_get_long_double(sqrtl(val->value.ld));
    }
#ifdef HAVE_QUADMATH_H
    else if (const_value_is_float128(val))
    {
        return const_value_get_float128(sqrtq(val->value.ld));
    }
#endif
    else
    {
        internal_error("Not implemented yet", 0);
    }
}

// This function is for supporting Fortran modules
size_t const_value_get_raw_data_size(void)
{
    return sizeof(const_value_t);
}

const_value_t* const_value_get_mask(cvalue_uint_t value, unsigned int num_bits)
{
    const_value_t* result = xcalloc(1, sizeof(*result));

    result->kind = CVK_MASK;
    result->num_bytes = num_bits / 8;
    result->value.i = value;

    return result;
}

char const_value_is_mask(const_value_t* v)
{
    return (v != NULL && v->kind == CVK_MASK);
}

unsigned int const_value_mask_get_num_bits(const_value_t* v)
{
    ERROR_CONDITION(!const_value_is_mask(v), "This const value is not a mask", 0);

    return v->num_bytes * 8;
}

unsigned int const_value_mask_get_num_bytes(const_value_t* v)
{
    ERROR_CONDITION(!const_value_is_mask(v), "This const value is not a mask", 0);

    return v->num_bytes;
}

cvalue_uint_t const_value_mask_get_value(const_value_t* v)
{
    ERROR_CONDITION(!const_value_is_mask(v), "This const value is not a mask", 0);

    return v->value.i;
}

// Only build simple types using this routine
// This function is for supporting Fortran modules
const_value_t* const_value_build_from_raw_data(const char* raw_buffer)
{
    const_value_t* result = xcalloc(1, sizeof(*result));

    // memcpy
    memcpy(result, raw_buffer, sizeof(const_value_t));

    return result;
}

static const_value_t* reduce_lexicographic_lt(
        const_value_t* (*fun)(const_value_t*, const_value_t*),
        const_value_t* lhs,
        const_value_t* rhs)
{
    int n_lhs = const_value_get_num_elements(lhs);
    int n_rhs = const_value_get_num_elements(rhs);

    int n_min = n_lhs < n_rhs ? n_lhs : n_rhs;

    int i;
    for (i = 0; i < n_min; i++)
    {
        if (const_value_is_zero(
                    fun( const_value_get_element_num(lhs, i),
                        const_value_get_element_num(rhs, i) )))
        {
            // lhs[i] < rhs[i]
            return const_value_get_signed_int(1);
        }
        else if (const_value_is_zero(
                    fun( const_value_get_element_num(rhs, i),
                        const_value_get_element_num(lhs, i) )))
        {
            // !(lhs[i] < rhs[i])
            // rhs[i] < lhs[i]
            return const_value_get_signed_int(0);
        }
        else
        {
            // !(lhs[i] < rhs[i])
            // !(rhs[i] < lhs[i])
            // They are the same, this is a common prefix, continue
        }
    }

    // All elements were the same
    return const_value_get_signed_int(n_lhs < n_rhs);
}

static const_value_t* reduce_lexicographic_gt(
        const_value_t* (*fun)(const_value_t*, const_value_t*),
        const_value_t* lhs,
        const_value_t* rhs)
{
    int n_lhs = const_value_get_num_elements(lhs);
    int n_rhs = const_value_get_num_elements(rhs);

    int n_min = n_lhs < n_rhs ? n_lhs : n_rhs;

    int i;
    for (i = 0; i < n_min; i++)
    {
        if (const_value_is_zero(
                    fun( const_value_get_element_num(lhs, i),
                        const_value_get_element_num(rhs, i) )))
        {
            // lhs[i] > rhs[i]
            return const_value_get_signed_int(1);
        }
        else if (const_value_is_zero(
                    fun( const_value_get_element_num(rhs, i),
                        const_value_get_element_num(lhs, i) )))
        {
            // !(lhs[i] > rhs[i])
            // rhs[i] > lhs[i]
            return const_value_get_signed_int(0);
        }
        else
        {
            // !(lhs[i] > rhs[i])
            // !(rhs[i] > lhs[i])
            // They are the same, this is a common prefix, continue
        }
    }

    // All elements were the same
    return const_value_get_signed_int(n_lhs > n_rhs);
}

static const_value_t* reduce_lexicographic_lte(
        const_value_t* (*fun)(const_value_t*, const_value_t*),
        const_value_t* lhs,
        const_value_t* rhs)
{
    int n_lhs = const_value_get_num_elements(lhs);
    int n_rhs = const_value_get_num_elements(rhs);

    int n_min = n_lhs < n_rhs ? n_lhs : n_rhs;

    int i;
    for (i = 0; i < n_min; i++)
    {
        if (const_value_is_zero(
                    fun( const_value_get_element_num(lhs, i),
                        const_value_get_element_num(rhs, i) )))
        {
            // lhs[i] < rhs[i]
            return const_value_get_signed_int(1);
        }
        else if (const_value_is_zero(
                    fun( const_value_get_element_num(rhs, i),
                        const_value_get_element_num(lhs, i) )))
        {
            // !(lhs[i] < rhs[i])
            // rhs[i] < lhs[i]
            return const_value_get_signed_int(0);
        }
        else
        {
            // !(lhs[i] < rhs[i])
            // !(rhs[i] < lhs[i])
            // They are the same, this is a common prefix, continue
        }
    }

    // All elements were the same
    return const_value_get_signed_int(n_lhs <= n_rhs);
}

static const_value_t* reduce_lexicographic_gte(
        const_value_t* (*fun)(const_value_t*, const_value_t*),
        const_value_t* lhs,
        const_value_t* rhs)
{
    int n_lhs = const_value_get_num_elements(lhs);
    int n_rhs = const_value_get_num_elements(rhs);

    int n_min = n_lhs < n_rhs ? n_lhs : n_rhs;

    int i;
    for (i = 0; i < n_min; i++)
    {
        if (const_value_is_zero(
                    fun( const_value_get_element_num(lhs, i),
                        const_value_get_element_num(rhs, i) )))
        {
            // lhs[i] > rhs[i]
            return const_value_get_signed_int(1);
        }
        else if (const_value_is_zero(
                    fun( const_value_get_element_num(rhs, i),
                        const_value_get_element_num(lhs, i) )))
        {
            // !(lhs[i] > rhs[i])
            // rhs[i] > lhs[i]
            return const_value_get_signed_int(0);
        }
        else
        {
            // !(lhs[i] > rhs[i])
            // !(rhs[i] > lhs[i])
            // They are the same, this is a common prefix, continue
        }
    }

    // All elements were the same
    return const_value_get_signed_int(n_lhs >= n_rhs);
}

static const_value_t* reduce_equal_values_and_length(
        const_value_t* (*fun)(const_value_t*, const_value_t*),
        const_value_t* lhs,
        const_value_t* rhs)
{
    int n_lhs = const_value_get_num_elements(lhs);
    int n_rhs = const_value_get_num_elements(rhs);

    if (n_lhs != n_rhs)
        return const_value_get_signed_int(0);

    int i;
    for (i = 0; i < n_lhs; i++)
    {
        if (const_value_is_zero(
                    fun( const_value_get_element_num(lhs, i),
                         const_value_get_element_num(rhs, i) )))
            return const_value_get_signed_int(0);
    }

    return const_value_get_signed_int(1);
}

static const_value_t* reduce_different_values_or_length(
        const_value_t* (*fun)(const_value_t*, const_value_t*),
        const_value_t* lhs,
        const_value_t* rhs)
{
    int n_lhs = const_value_get_num_elements(lhs);
    int n_rhs = const_value_get_num_elements(rhs);

    if (n_lhs != n_rhs)
        return const_value_get_signed_int(1);

    int i;
    for (i = 0; i < n_lhs; i++)
    {
        if (const_value_is_nonzero(
                    fun( const_value_get_element_num(lhs, i),
                         const_value_get_element_num(rhs, i) )))
            return const_value_get_signed_int(1);
    }

    return const_value_get_signed_int(0);
}
