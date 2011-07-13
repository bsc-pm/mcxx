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


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <fenv.h>
#include "cxx-buildscope.h"
#include "cxx-exprtype.h"
#include "cxx-cexpr.h"
#include "cxx-ast.h"
#include "cxx-utils.h"
#include "cxx-prettyprint.h"
#include "cxx-ambiguity.h"
#include "cxx-typeutils.h"
#include "cxx-exprtype.h"
#include "cxx-overload.h"
#include "cxx-instantiation.h"
#include "cxx-typeenviron.h"
#include "cxx-limits.h"
#include "cxx-nodecl-output.h"

#define CVAL_HASH_SIZE (37)

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
    CVK_STRING
} const_value_kind_t;

typedef struct const_multi_value_tag
{
    int num_elements;
    const_value_t* elements[];
} const_multi_value_t;

struct const_value_tag
{
    const_value_kind_t kind;
    char sign : 1;
    int num_bytes;
    AST tree;

    union
    {
        // CVK_INTEGER
        uint64_t i;
        // CVK_FLOAT
        float f;
        // CVK_DOUBLE
        double d;
        // CVK_LONG_DOUBLE
        long double ld;
#ifdef HAVE_FLOAT128
        // CVK_FLOAT128
        __float128 f128;
#endif
        // CVK_COMPLEX
        // CVK_ARRAY
        // CVK_STRUCT
        // CVK_VECTOR
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

static const_value_hash_t _hash_pool[MCXX_MAX_BYTES_INTEGER * 2] = { { (const_value_hash_bucket_t*)0 } };

const_value_t* const_value_get_integer(uint64_t value, int num_bytes, char sign)
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
        bucket = calloc(1, sizeof(*bucket));
        
        bucket->constant_value = calloc(1, sizeof(*bucket->constant_value));
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
const_value_t* const_value_get_signed_##type ( uint64_t value ) \
{ \
    return const_value_get_integer(value, type_get_size(get_signed_##type##_type ( ) ), 1); \
}

#define GET_UNSIGNED_INTEGER(type)  \
const_value_t* const_value_get_unsigned_##type ( uint64_t value ) \
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
    const_value_t* v = calloc(1, sizeof(*v));
    v->kind = CVK_FLOAT;
    v->value.f = f;
    v->sign = 1;

    return v;
}

const_value_t* const_value_get_double(double d)
{
    const_value_t* v = calloc(1, sizeof(*v));
    v->kind = CVK_DOUBLE;
    v->value.d = d;
    v->sign = 1;

    return v;
}

const_value_t* const_value_get_long_double(long double ld)
{
    const_value_t* v = calloc(1, sizeof(*v));
    v->kind = CVK_LONG_DOUBLE;
    v->value.ld = ld;
    v->sign = 1;

    return v;
}

#define OTHER_KIND default : { internal_error("Unexpected literal kind", 0); }

const_value_t* const_value_cast_to_bytes(const_value_t* val, int bytes, char sign)
{
    switch (val->kind)
    {
        case CVK_INTEGER:
            return const_value_get_integer(val->value.i, bytes, sign);
        OTHER_KIND;
    }
    return NULL;
}

const_value_t* const_value_cast_to_signed_int_value(const_value_t* val)
{
    switch (val->kind)
    {
        case CVK_INTEGER:
            return const_value_get_integer(val->value.i, type_get_size(get_signed_int_type()), 1);
        OTHER_KIND;
    }
    return NULL;
}

const_value_t* const_value_round_to_zero_bytes(const_value_t* val, int num_bytes)
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
                fesetround(FE_TOWARDZERO);

                long long int l = llrintf(val->value.f);

                fesetround(old_round_mode);

                return const_value_get_integer(l, num_bytes, 1);
            }
        case CVK_DOUBLE:
            {
                int old_round_mode = fegetround();
                fesetround(FE_TOWARDZERO);

                long long int l = llrint(val->value.d);

                fesetround(old_round_mode);

                return const_value_get_integer(l, num_bytes, 1);
            }
        case CVK_LONG_DOUBLE:
            {
                int old_round_mode = fegetround();
                fesetround(FE_TOWARDZERO);

                long long int l = llrintl(val->value.ld);

                fesetround(old_round_mode);

                return const_value_get_integer(l, num_bytes, 1);
            }
        OTHER_KIND;
    }
    return NULL;
}

const_value_t* const_value_round_to_zero(const_value_t* val)
{
    return const_value_round_to_zero_bytes(val, type_get_size(get_signed_int_type()));
}

const_value_t* const_value_get_zero(int num_bytes, char sign)
{
    return const_value_get_integer(0, num_bytes, sign);
}

const_value_t* const_value_get_one(int num_bytes, char sign)
{
    return const_value_get_integer(1, num_bytes, sign);
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
        OTHER_KIND;
    }

    return 0;
}

char const_value_is_zero(const_value_t* v)
{
    return !const_value_is_nonzero(v);
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
        OTHER_KIND;
    }
}

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
        OTHER_KIND;
    }
}

#define IS_FLOAT(kind) (kind == CVK_FLOAT || kind == CVK_DOUBLE || kind == CVK_LONG_DOUBLE)

char const_value_is_signed(const_value_t* val)
{
    return val->sign;
}

static type_t* get_minimal_integer_for_value(char is_signed, uint64_t value)
{
    if (!is_signed)
    {
        uint64_t bitmask = ~(uint64_t)0;

        struct type_mask_tag
        {
            type_t* type;
            uint64_t mask;
        } type_mask[] =
        {
            { get_unsigned_char_type(),          bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_unsigned_char_type())         ) },
            { get_unsigned_short_int_type(),     bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_unsigned_short_int_type())    ) },
            { get_unsigned_int_type(),           bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_unsigned_int_type())          ) },
            { get_unsigned_long_int_type(),      bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_unsigned_long_int_type())     ) },
            { get_unsigned_long_long_int_type(), 0 },
            // Sentinel
            { NULL, 0 },
        };

        int i;
        for (i = 0; type_mask[i].type != NULL; i++)
        {
            if ((value & type_mask[i].mask) == 0)
            {
                return type_mask[i].type;
            }
        }
        internal_error("Value '%llu' does not have any suitable integer type", value);
    }
    else
    {
        char is_negative = (value & ((uint64_t)1 << 63)) >> 63;

        if (!is_negative)
        {
            uint64_t bitmask = ~(uint64_t)0;

            struct type_mask_tag
            {
                type_t* type;
                uint64_t mask;
            } type_mask[] =
            {
                // Like above but one bit less now
                { get_signed_char_type(),          bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_signed_char_type())          - 1) },
                { get_signed_short_int_type(),     bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_signed_short_int_type())     - 1) },
                { get_signed_int_type(),           bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_signed_int_type())           - 1) },
                { get_signed_long_int_type(),      bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_signed_long_int_type())      - 1) },
                { get_signed_long_long_int_type(), bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_signed_long_long_int_type()) - 1) },
                // Sentinel
                { NULL, 0 },
            };

            int i;
            for (i = 0; type_mask[i].type != NULL; i++)
            {
                if ((value & type_mask[i].mask) == 0)
                {
                    return type_mask[i].type;
                }
            }
            internal_error("Value '%lld' does not have any suitable integer type", (int64_t)value);
        }
        else
        {
            uint64_t bitmask = ~(uint64_t)0;
            uint64_t remove_sign = (~(uint64_t)0) >> 1;

            struct type_mask_tag
            {
                type_t* type;
                uint64_t mask;
            } type_mask[] =
            {
                { get_signed_char_type(),          remove_sign & (bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_signed_char_type())     )) },
                { get_signed_short_int_type(),     remove_sign & (bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_signed_short_int_type()))) },
                { get_signed_int_type(),           remove_sign & (bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_signed_int_type())      )) },
                { get_signed_long_int_type(),      remove_sign & (bitmask << ((uint64_t)8 * (uint64_t)type_get_size(get_signed_long_int_type()) )) },
                { get_signed_long_long_int_type(), 0 },
                // Sentinel
                { NULL, 0 },
            };

            int i;
            for (i = 0; type_mask[i].type != NULL; i++)
            {
                // Like above but we check for the existence of a 0 instead of 1
                if (((~value) & type_mask[i].mask) == 0)
                {
                    return type_mask[i].type;
                }
            }
            internal_error("Value '%lld' does not have any suitable integer type", (int64_t)value);
        }
    }

    internal_error("Code unreachable", 0);
    return NULL;
}

static type_t* get_minimal_floating_type(const_value_t* val)
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


static void get_proper_suffix_integer(char is_signed, uint64_t value, const char** suffix, type_t** t)
{
    *t = get_minimal_integer_for_value(is_signed, value);

    if (is_signed_char_type(*t)
            || is_signed_short_int_type(*t)
            || is_signed_int_type(*t))
    {
        *suffix = uniquestr("");
    }
    else if (is_unsigned_char_type(*t)
            || is_unsigned_short_int_type(*t)
            || is_unsigned_int_type(*t))
    {
        *suffix = uniquestr("u");
    }
    else if (is_signed_long_int_type(*t))
    {
        *suffix = uniquestr("l");
    }
    else if (is_unsigned_long_int_type(*t))
    {
        *suffix = uniquestr("ul");
    }
    else if (is_signed_long_long_int_type(*t))
    {
        *suffix = uniquestr("ll");
    }
    else if (is_unsigned_long_long_int_type(*t))
    {
        *suffix = uniquestr("ull");
    }
    else
    {
        internal_error("Invalid type", 0);
    }
}

nodecl_t const_value_to_nodecl(const_value_t* v)
{
    if (v->kind == CVK_INTEGER 
            && v->value.i == 0)
    {
        // Zero is special
        return nodecl_make_integer_literal(get_zero_type(), v, NULL, 0);
    }
    else
    {
        if (v->kind == CVK_INTEGER)
        {
            type_t* t = get_minimal_integer_for_value(v->sign, v->value.i);
            return nodecl_make_integer_literal(t, v, NULL, 0);
        }
        else if (v->kind == CVK_FLOAT
                || v->kind == CVK_DOUBLE
                || v->kind == CVK_LONG_DOUBLE)
        {
            type_t* t = get_minimal_floating_type(v);
            return nodecl_make_floating_literal(t, v, NULL, 0);
        }
        else if (v->kind == CVK_STRING)
        {
            return nodecl_make_string_literal(
                    get_array_type_bounds(
                        get_char_type(),
                        nodecl_make_integer_literal(get_signed_int_type(), const_value_get_one(4, 1), NULL, 0),
                        nodecl_make_integer_literal(get_signed_int_type(), const_value_get_signed_int(v->value.m->num_elements), NULL, 0),
                        CURRENT_COMPILED_FILE->global_decl_context),
                    v, NULL, 0);
        }
        else
        {
            return nodecl_null();
        }
    }
}

char const_value_is_integer(const_value_t* v)
{
    return v->kind == CVK_INTEGER;
}

char const_value_is_floating(const_value_t* v)
{
    return const_value_is_float(v)
        || const_value_is_double(v)
        || const_value_is_long_double(v);
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

float const_value_cast_to_float(const_value_t* v)
{
    if (v->kind == CVK_FLOAT)
        return v->value.f;
    else if (v->kind == CVK_INTEGER)
    {
        if (v->sign)
        {
            return (float)*(int64_t*)(&v->value.i);
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
            return (double)*(int64_t*)(&v->value.i);
        }
        else
        {
            return (double)v->value.i;
        }
    }
    else if (v->kind == CVK_FLOAT)
        return (float)v->value.f;
    else if (v->kind == CVK_LONG_DOUBLE)
        return (float)v->value.ld;

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
            return (long double)*(int64_t*)(&v->value.i);
        }
        else
        {
            return (long double)v->value.i;
        }
    }
    else if (v->kind == CVK_FLOAT)
        return (float)v->value.f;
    else if (v->kind == CVK_DOUBLE)
        return (float)v->value.d;

    internal_error("Code unreachable", 0);
}

const_value_t* const_value_cast_to_float_value(const_value_t* val)
{
    return const_value_get_float(const_value_cast_to_float(val));
}

const_value_t* const_value_cast_to_double_value(const_value_t* val)
{
    return const_value_get_double(const_value_cast_to_double(val));
}

const_value_t* const_value_cast_to_long_double_value(const_value_t* val)
{
    return const_value_get_long_double(const_value_cast_to_long_double(val));
}

AST const_value_to_tree(const_value_t* v)
{
    if (v->tree == NULL)
    {
        if (v->kind == CVK_INTEGER
                && v->value.i == 0)
        {
            // 0 is special as it is an octal
            v->tree = ASTLeaf(AST_OCTAL_LITERAL, NULL, 0, uniquestr("0"));
            CXX_LANGUAGE()
            {
                expression_set_type(v->tree, get_zero_type());
            }
            C_LANGUAGE()
            {
                expression_set_type(v->tree, get_signed_int_type());
            }
        }
        else
        {
            if (v->kind == CVK_INTEGER)
            {
                char c[64] = { 0 };
                type_t* t = NULL;
                const char* suffix = NULL;

                get_proper_suffix_integer(v->sign, v->value.i, &suffix, &t);

                if (v->sign)
                {
                    signed long long *sll = (signed long long*)&v->value.i;
                    snprintf(c, 63, "%lld%s", *sll, suffix);
                }
                else
                {
                    snprintf(c, 63, "%llu%s", (unsigned long long)v->value.i, suffix);
                }

                c[63] = '\0';

                v->tree = ASTLeaf(AST_DECIMAL_LITERAL, NULL, 0, uniquestr(c));
                expression_set_type(v->tree, t);
            }
            else if (v->kind == CVK_FLOAT
                    || v->kind == CVK_DOUBLE
                    || v->kind == CVK_LONG_DOUBLE)
            {
                char c[64];
                type_t* t = NULL;

                if (v->kind == CVK_FLOAT)
                {
                    snprintf(c, 63, "%.6f", v->value.f);
                    t = get_float_type();
                }
                else if (v->kind == CVK_DOUBLE)
                {
                    snprintf(c, 63, "%.15f", v->value.d);
                    t = get_double_type();
                }
                else if (v->kind == CVK_LONG_DOUBLE)
                {
                    snprintf(c, 63, "%.33Lf", v->value.ld);
                    t = get_long_double_type();
                }
                else
                {
                    internal_error("Unreachable code", 0);
                }
                c[63] = '\0';

                v->tree = ASTLeaf(AST_FLOATING_LITERAL, NULL, 0, uniquestr(c));
                expression_set_type(v->tree, t);
            }
            else if (v->kind == CVK_STRING)
            {
                int *bytes = NULL;
                int num_elements = 0;
                const_value_string_unpack(v, &bytes, &num_elements);

                if (num_elements > 0)
                {
                    if (const_value_get_bytes(v->value.m->elements[0]) == 1)
                    {
                        int length = num_elements + 2 + 1;
                        char c[length];
                        c[0] = '"';
                        int i, p = 1;
                        for (i = 0; i < num_elements; i++, p++)
                        {
                            c[p] = const_value_cast_to_1(v->value.m->elements[i]);
                        }
                        c[p] = '"'; p++;
                        c[p] = '\0'; p++;

                        v->tree = ASTLeaf(AST_STRING_LITERAL, NULL, 0, uniquestr(c));
                    }
                    else
                    {
                        char c[num_elements + 3 + 1];

                        wchar_t t_elements[num_elements + 1];
                        memcpy(t_elements, bytes, sizeof(int)*num_elements);
                        t_elements[num_elements] = 0;

                        snprintf(c, num_elements + 2 + 1, "L\"%ls\"", t_elements);

                        v->tree = ASTLeaf(AST_STRING_LITERAL, NULL, 0, uniquestr(c));
                    }
                }
                else
                {
                    v->tree = ASTLeaf(AST_STRING_LITERAL, NULL, 0, uniquestr("\"\""));
                }

                free(bytes);
            }
            else
            {
                return NULL;
            }
        }

        // Set ourselves as a constant
        expression_set_constant(v->tree, v);
        expression_set_nodecl(v->tree, const_value_to_nodecl(v));
    }

    return v->tree;
}

const_value_t* integer_type_get_minimum(type_t* t)
{
    if (is_unsigned_char_type(t)
            || is_unsigned_short_int_type(t)
            || is_unsigned_long_int_type(t)
            || is_unsigned_long_long_int_type(t)
            || is_unsigned_int_type(t))
    {
        return const_value_get_zero(type_get_size(t), /* sign */ 0);
    }
    else if (is_signed_char_type(t)
            || is_signed_short_int_type(t)
            || is_signed_long_int_type(t)
            || is_signed_long_long_int_type(t)
            || is_signed_int_type(t))
    {
        uint64_t mask = ~(uint64_t)0;
        mask >>= (64 - type_get_size(t)*8 + 1);
        return const_value_get_integer(~mask, type_get_size(t), /* sign */ 1);
    }

    internal_error("Invalid type", 0);
    return NULL;
}

const_value_t* integer_type_get_maximum(type_t* t)
{
    if (is_unsigned_char_type(t)
            || is_unsigned_short_int_type(t)
            || is_unsigned_long_int_type(t)
            || is_unsigned_long_long_int_type(t)
            || is_unsigned_int_type(t))
    {
        uint64_t mask = ~(uint64_t)0;

        if (type_get_size(t) < 8)
        {
            mask &= ~(~(uint64_t)0 << (type_get_size(t) * 8));
        }

        return const_value_get_integer(mask, type_get_size(t), /* sign */ 0);
    }
    else if (is_signed_char_type(t)
            || is_signed_short_int_type(t)
            || is_signed_long_int_type(t)
            || is_signed_long_long_int_type(t)
            || is_signed_int_type(t))
    {
        uint64_t mask = ~(uint64_t)0;
        mask >>= (64 - type_get_size(t)*8 + 1);
        return const_value_get_integer(mask, type_get_size(t), /* sign */ 1);
    }

    internal_error("Invalid type", 0);
    return NULL;
}

int const_value_get_bytes(const_value_t* val)
{
    return val->num_bytes;
}

static const_value_t* make_multival(int num_elements, const_value_t **elements)
{
    const_value_t* result = calloc(1, sizeof(*result));

    result->value.m = calloc(1, sizeof(const_multi_value_t) + sizeof(const_value_t) * num_elements);
    result->value.m->num_elements = num_elements;

    int i;
    for (i = 0; i < num_elements; i++)
    {
        result->value.m->elements[i] = elements[i];
    }

    return result;
}

static const_value_t* multival_get_element_num(const_value_t* v, int element)
{
    return v->value.m->elements[element];
}

static int multival_get_num_elements(const_value_t* v)
{
    return v->value.m->num_elements;
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

const_value_t* const_value_make_struct(int num_elements, const_value_t **elements)
{
    const_value_t* result = make_multival(num_elements, elements);
    result->kind = CVK_STRUCT;
    return result;
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
    const_value_t* result = make_multival(2, complex_);
    result->kind = CVK_COMPLEX;
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

#define IS_STRUCTURED(x) \
    (x == CVK_COMPLEX \
    || x == CVK_ARRAY \
    || x == CVK_STRUCT \
    || x == CVK_VECTOR \
    || x == CVK_STRING)

int const_value_get_num_elements(const_value_t* value)
{
    ERROR_CONDITION(!IS_STRUCTURED(value->kind), "This is not a multiple-value constant", 0);
    return multival_get_num_elements(value);
}

const_value_t* const_value_get_element_num(const_value_t* value, int num)
{
    ERROR_CONDITION(!IS_STRUCTURED(value->kind), "This is not a multiple-value constant", 0);
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

// Use this to apply a binary function to a couple of multivals
static const_value_t* map_binary_to_structured_value(const_value_t* (*fun)(const_value_t*, const_value_t*),
        const_value_t* m1,
        const_value_t* m2)
{
    ERROR_CONDITION(!IS_STRUCTURED(m1->kind) || !IS_STRUCTURED(m2->kind), "One of the values is not a multiple-value constant", 0);
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

#if 0
static const_value_t* map_unary_to_structured_value(const_value_t* (*fun)(const_value_t*),
        const_value_t* m1)
{
    ERROR_CONDITION(!IS_STRUCTURED(m1->kind), "The value is not a multiple-value constant", 0);

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
#endif

#define OP(_opname) const_value_##opname

#define BINOP_FUN_I(_opname, _binop) \
const_value_t* const_value_##_opname(const_value_t* v1, const_value_t* v2) \
{ \
    ERROR_CONDITION(v1 == NULL || v2 == NULL, "Either of the parameters is NULL", 0); \
    if (v1->kind == CVK_INTEGER \
            && v2->kind == CVK_INTEGER) \
    { \
       int bytes = 0; char sign = 0; \
       common_bytes(v1, v2, &bytes, &sign); \
       uint64_t value = 0; \
       if (sign) \
       { \
           *(int64_t*)&value = *(int64_t*)&(v1->value.i) _binop *(int64_t*)&(v2->value.i); \
       } \
       else \
       { \
           value = v1->value.i _binop v2->value.i; \
       } \
       return const_value_get_integer(value, bytes, sign); \
    } \
    return NULL; \
}

#define BINOP_FUN(_opname, _binop) \
const_value_t* const_value_##_opname(const_value_t* v1, const_value_t* v2) \
{ \
    ERROR_CONDITION(v1 == NULL || v2 == NULL, "Either of the parameters is NULL", 0); \
    if (v1->kind == CVK_INTEGER \
            && v2->kind == CVK_INTEGER) \
    { \
       int bytes = 0; char sign = 0; \
       common_bytes(v1, v2, &bytes, &sign); \
       uint64_t value = 0; \
       if (sign) \
       { \
           *(int64_t*)&value = *(int64_t*)&(v1->value.i) _binop *(int64_t*)&(v2->value.i); \
       } \
       else \
       { \
           value = v1->value.i _binop v2->value.i; \
       } \
       return const_value_get_integer(value, bytes, sign); \
    } \
    else if (v1->kind == CVK_INTEGER \
            && IS_FLOAT(v2->kind)) \
    { \
        if (v2->kind == CVK_FLOAT) \
        { \
            return const_value_get_float((float) v1->value.i _binop v2->value.f); \
        } \
        else if (v2->kind == CVK_DOUBLE) \
        { \
            return const_value_get_double((double) v1->value.i _binop v2->value.d); \
        } \
        else if (v2->kind == CVK_LONG_DOUBLE) \
        { \
            return const_value_get_long_double((long double) v1->value.i _binop v2->value.ld); \
        } \
    } \
    else if (v2->kind == CVK_INTEGER \
            && IS_FLOAT(v1->kind)) \
    { \
        if (v1->kind == CVK_FLOAT) \
        { \
            return const_value_get_float(v1->value.f _binop (float) v2->value.i); \
        } \
        else if (v1->kind == CVK_DOUBLE) \
        { \
            return const_value_get_double(v1->value.d _binop (double) v2->value.i); \
        } \
        else if (v1->kind == CVK_LONG_DOUBLE) \
        { \
            return const_value_get_long_double(v1->value.d _binop (long double) v2->value.i); \
        } \
    } \
    else if (IS_FLOAT(v1->kind) && IS_FLOAT(v2->kind)) \
    { \
        if (v1->kind == v2->kind) \
        { \
            if (v1->kind == CVK_FLOAT) \
                return const_value_get_float(v1->value.f _binop v2->value.f); \
            else if (v1->kind == CVK_DOUBLE) \
                return const_value_get_double(v1->value.d _binop v2->value.d); \
            else if (v1->kind == CVK_LONG_DOUBLE) \
                return const_value_get_long_double(v1->value.ld _binop v2->value.ld); \
        } \
        else if (v1->kind > v2->kind) \
        { \
            if (v1->kind == CVK_DOUBLE) \
            { \
                return const_value_get_double(v1->value.d _binop (double) v2->value.f); \
            } \
            else if (v1->kind == CVK_LONG_DOUBLE) \
            { \
                if (v2->kind == CVK_FLOAT) \
                { \
                    return const_value_get_long_double(v1->value.ld _binop (long double) v2->value.f); \
                } \
                else if (v2->kind == CVK_DOUBLE) \
                { \
                    return const_value_get_long_double(v1->value.ld _binop (long double) v2->value.d); \
                } \
            } \
        } \
        else if (v1->kind < v2->kind) \
        { \
            if (v2->kind == CVK_DOUBLE) \
            { \
                return const_value_get_double((double)v1->value.f _binop v2->value.d); \
            } \
            else if (v2->kind == CVK_LONG_DOUBLE) \
            { \
                if (v1->kind == CVK_FLOAT) \
                { \
                    return const_value_get_long_double((long double) v1->value.f _binop v2->value.ld); \
                } \
                else if (v1->kind == CVK_DOUBLE) \
                { \
                    return const_value_get_long_double((long double) v1->value.d _binop v2->value.ld); \
                } \
            } \
        } \
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
    else if (IS_STRUCTURED(v1->kind) \
            && IS_STRUCTURED(v2->kind) \
            && (multival_get_num_elements(v1) == multival_get_num_elements(v2))) \
    { \
        return map_binary_to_structured_value( const_value_##_opname, v1, v2); \
    } \
 \
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
       uint64_t value = 0; \
       if (sign) \
       { \
           *(int64_t*)&value = _func ## s ( *(int64_t*)&(v1->value.i), *(int64_t*)&(v2->value.i) ); \
       } \
       else \
       { \
        \
           value = _func ## u( v1->value.i, v2->value.i ); \
       } \
       return const_value_get_integer(value, bytes, sign); \
    } \
    else if (v1->kind == CVK_INTEGER \
            && IS_FLOAT(v2->kind)) \
    { \
        if (v2->kind == CVK_FLOAT) \
        { \
            return const_value_get_float(_func##f((float) v1->value.i, v2->value.f)); \
        } \
        else if (v2->kind == CVK_DOUBLE) \
        { \
            return const_value_get_double(_func ## d( (double) v1->value.i, v2->value.d)); \
        } \
        else if (v2->kind == CVK_LONG_DOUBLE) \
        { \
            return const_value_get_long_double( _func ## ld( v1->value.i, v2->value.ld)); \
        } \
    } \
    else if (v2->kind == CVK_INTEGER \
            && IS_FLOAT(v1->kind)) \
    { \
        if (v1->kind == CVK_FLOAT) \
        { \
            return const_value_get_float(_func ## f(v1->value.f, (float) v2->value.i)); \
        } \
        else if (v1->kind == CVK_DOUBLE) \
        { \
            return const_value_get_double(_func ## d (v1->value.d, (double) v2->value.i)); \
        } \
        else if (v1->kind == CVK_LONG_DOUBLE) \
        { \
            return const_value_get_long_double(_func ## ld (v1->value.d, (long double) v2->value.i)); \
        } \
    } \
    else if (IS_FLOAT(v1->kind) && IS_FLOAT(v2->kind)) \
    { \
        if (v1->kind == v2->kind) \
        { \
            if (v1->kind == CVK_FLOAT) \
                return const_value_get_float(_func ## f ( v1->value.f, v2->value.f)); \
            else if (v1->kind == CVK_DOUBLE) \
                return const_value_get_double(_func ## d(v1->value.d, v2->value.d)); \
            else return const_value_get_long_double(_func ## ld(v1->value.ld, v2->value.ld)); \
        } \
        else if (v1->kind > v2->kind) \
        { \
            if (v1->kind == CVK_DOUBLE) \
            { \
                return const_value_get_double(_func ## d(v1->value.d, (double) v2->value.f)); \
            } \
            else if (v1->kind == CVK_LONG_DOUBLE) \
            { \
                if (v2->kind == CVK_FLOAT) \
                { \
                    return const_value_get_long_double(_func ## ld (v1->value.ld, (long double) v2->value.f)); \
                } \
                else if (v2->kind == CVK_DOUBLE) \
                { \
                    return const_value_get_long_double(_func ## d(v1->value.ld, (long double) v2->value.d)); \
                } \
            } \
        } \
        else if (v1->kind < v2->kind) \
        { \
            if (v2->kind == CVK_DOUBLE) \
            { \
                return const_value_get_double(_func ## d ((double)v1->value.f, v2->value.d)); \
            } \
            else if (v2->kind == CVK_LONG_DOUBLE) \
            { \
                if (v1->kind == CVK_FLOAT) \
                { \
                    return const_value_get_long_double(_func ## ld((long double) v1->value.f, v2->value.ld)); \
                } \
                else if (v1->kind == CVK_DOUBLE) \
                { \
                    return const_value_get_long_double(_func ## ld ((long double) v1->value.d, v2->value.ld)); \
                } \
            } \
        } \
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
    else if (IS_STRUCTURED(v1->kind) \
            && IS_STRUCTURED(v2->kind) \
            && (multival_get_num_elements(v1) == multival_get_num_elements(v2))) \
    { \
        return map_binary_to_structured_value( const_value_##_opname, v1, v2); \
    } \
 return NULL; \
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

BINOP_FUN(add, +)
BINOP_FUN(sub, -)
BINOP_FUN(mul, *)
BINOP_FUN(div, /)
BINOP_FUN_I(mod, %)
BINOP_FUN_I(shr, >>)
BINOP_FUN_I(shl, <<)
BINOP_FUN_I(bitand, &)
BINOP_FUN_I(bitor, |)
BINOP_FUN_I(bitxor, ^)
BINOP_FUN(and, &&)
BINOP_FUN(or, ||)
BINOP_FUN(lt, <)
BINOP_FUN(lte, <=)
BINOP_FUN(gt, >)
BINOP_FUN(gte, >=)
BINOP_FUN(eq, ==)
BINOP_FUN(neq, !=)

static uint64_t arith_powu(uint64_t a, uint64_t b)
{
    if (b == 0)
    {
        return 1;
    }
    else if ((b & (uint64_t)1) == (uint64_t)1) // odd
    {
        uint64_t k = arith_powu(a, (b-1) >> 1);
        return a * k * k;
    }
    else // even
    {
        uint64_t k = arith_powu(a, b >> 1);
        return k * k;
    }
}

static int64_t arith_pows(int64_t a, int64_t b)
{
    if (b == 0)
    {
        return 1;
    }
    else if (b < 0)
    {
        return 1 / arith_pows(a, -b);
    }
    else if ((b & (int64_t)1) == (int64_t)1) // odd
    {
        int64_t k = arith_powu(a, (b-1) >> 1);
        return a * k * k;
    }
    else // even
    {
        int64_t k = arith_powu(a, b >> 1);
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

BINOP_FUN_CALL(pow, arith_pow)

#define UNOP_FUN(_opname, _unop) \
const_value_t* const_value_##_opname(const_value_t* v1) \
{ \
    ERROR_CONDITION(v1 == NULL, "Parameter cannot be NULL", 0); \
    if (v1->kind == CVK_INTEGER) \
    { \
        uint64_t value = 0; \
        if (v1->sign) \
        { \
            value = _unop (int64_t)v1->value.i; \
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
    return NULL; \
}

#define UNOP_FUN_I(_opname, _unop) \
const_value_t* const_value_##_opname(const_value_t* v1) \
{ \
    ERROR_CONDITION(v1 == NULL, "Parameter cannot be NULL", 0); \
    if (v1->kind == CVK_INTEGER) \
    { \
        uint64_t value = 0; \
        if (v1->sign) \
        { \
            value = _unop (int64_t)v1->value.i; \
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
UNOP_FUN_I(not, !)

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

static const_value_t* arith_powz(const_value_t* v1 UNUSED_PARAMETER, const_value_t* v2 UNUSED_PARAMETER)
{
    internal_error("Not yet implemented", 0);
}

void const_value_string_unpack(const_value_t* v, int **values, int *num_elements)
{
    ERROR_CONDITION(v->kind != CVK_STRING, "Invalid data type", 0);

    int *result = calloc(const_value_get_num_elements(v), sizeof(*result));

    int i, nels = const_value_get_num_elements(v);
    for (i = 0; i < nels; i++)
    {
        result[i] = const_value_cast_to_4(v->value.m->elements[i]);
    }

    *num_elements = nels;
    *values = result;
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
