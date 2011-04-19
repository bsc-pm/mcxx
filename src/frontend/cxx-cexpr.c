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



#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
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

// We allow up to 16 bytes per integer
#define MAX_NUM_BYTES (16)

#define CVAL_HASH_SIZE (37)

struct const_value_tag
{
    char sign : 1;
    int num_bytes;
    AST tree;
    uint64_t value;
};

typedef
struct const_value_hash_bucket_tag
{
    const_value_t* constant_value;
    struct const_value_hash_bucket_tag *next;
} const_value_hash_bucket_t;

typedef const_value_hash_bucket_t* const_value_hash_t[CVAL_HASH_SIZE];

static const_value_hash_t _hash_pool[MAX_NUM_BYTES * 2] = { { (const_value_hash_bucket_t*)0 } };

const_value_t* const_value_get(uint64_t value, int num_bytes, char sign)
{
    ERROR_CONDITION(num_bytes > MAX_NUM_BYTES
            || num_bytes < 0, "Invalid num_bytes = %d\n", num_bytes);

    int bucket_index = value % CVAL_HASH_SIZE;

    int pool = 2 * num_bytes + !!sign;

    const_value_hash_bucket_t* bucket = _hash_pool[pool][bucket_index];

    while (bucket != NULL)
    {
        if (bucket->constant_value->value == value)
        {
            break;
        }
        bucket = bucket->next;
    }

    if (bucket == NULL)
    {
        bucket = calloc(1, sizeof(*bucket));
        
        bucket->constant_value = calloc(1, sizeof(*bucket->constant_value));
        bucket->constant_value->value = value;
        bucket->constant_value->num_bytes = num_bytes;
        bucket->constant_value->sign = sign;

        bucket->next = _hash_pool[pool][bucket_index];

        _hash_pool[pool][bucket_index] = bucket;
    }

    return bucket->constant_value;
}

const_value_t* const_value_cast_to_bytes(const_value_t* val, int bytes, char sign)
{
    return const_value_get(val->value, bytes, sign);
}

const_value_t* const_value_get_zero(int num_bytes, char sign)
{
    return const_value_get(0, num_bytes, sign);
}

const_value_t* const_value_get_one(int num_bytes, char sign)
{
    return const_value_get(1, num_bytes, sign);
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
            *sign = 1;
        }
        else
        {
            *sign = v1->sign;
        }
    }
}

char const_value_is_nonzero(const_value_t* v)
{
    return !!v->value;
}

char const_value_is_zero(const_value_t* v)
{
    return !const_value_is_nonzero(v);
}

uint64_t const_value_cast_to_8(const_value_t* val)
{
    return val->value;
}

uint32_t const_value_cast_to_4(const_value_t* val)
{
    return (uint32_t)(val->value & 0xffffffff);
}

uint16_t const_value_cast_to_2(const_value_t* val)
{
    return (uint16_t)(val->value & 0xffff);
}

uint8_t const_value_cast_to_1(const_value_t* val)
{
    return (uint8_t)(val->value & 0xff);
}

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

type_t* const_value_get_minimal_integer_type(const_value_t* val)
{
    return get_minimal_integer_for_value(val->sign, val->value);
}


static void get_proper_suffix(char is_signed, uint64_t value, const char** suffix, type_t** t)
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

AST const_value_to_tree(const_value_t* v)
{
    if (v->tree == NULL)
    {
        if (v->value == 0)
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
            char c[64] = { 0 };
            type_t* t = NULL;
            const char* suffix = NULL;

            get_proper_suffix(v->sign, v->value, &suffix, &t);

            if (v->sign)
            {
                signed long long *sll = (signed long long*)&v->value;
                snprintf(c, 63, "%lld%s", *sll, suffix);
            }
            else
            {
                snprintf(c, 63, "%llu%s", (unsigned long long)v->value, suffix);
            }

            c[63] = '\0';

            v->tree = ASTLeaf(AST_DECIMAL_LITERAL, NULL, 0, uniquestr(c));
            expression_set_type(v->tree, t);
        }

        // Set ourselves as a constant
        expression_set_constant(v->tree, v);
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
        return const_value_get(~mask, type_get_size(t), /* sign */ 1);
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

        return const_value_get(mask, type_get_size(t), /* sign */ 0);
    }
    else if (is_signed_char_type(t)
            || is_signed_short_int_type(t)
            || is_signed_long_int_type(t)
            || is_signed_long_long_int_type(t)
            || is_signed_int_type(t))
    {
        uint64_t mask = ~(uint64_t)0;
        mask >>= (64 - type_get_size(t)*8 + 1);
        return const_value_get(mask, type_get_size(t), /* sign */ 1);
    }

    internal_error("Invalid type", 0);
    return NULL;
}

#define OP(_opname) const_value_##opname

#define BINOP_FUN(_opname, _binop) \
const_value_t* const_value_##_opname(const_value_t* v1, const_value_t* v2) \
{ \
    ERROR_CONDITION(v1 == NULL || v2 == NULL, "Either of the parameters is NULL", 0); \
    int bytes = 0; char sign = 0; \
    common_bytes(v1, v2, &bytes, &sign); \
    uint64_t value = 0; \
    if (sign) \
    { \
        *(int64_t*)&value = *(int64_t*)&(v1->value) _binop *(int64_t*)&(v2->value); \
    } \
    else \
    { \
        value = v1->value _binop v2->value; \
    } \
    return const_value_get(value, bytes, sign); \
}

#define BINOP_FUN_CALL(_opname, _func) \
const_value_t* const_value_##_opname(const_value_t* v1, const_value_t* v2) \
{ \
    ERROR_CONDITION(v1 == NULL || v2 == NULL, "Either of the parameters is NULL", 0); \
    int bytes = 0; char sign = 0; \
    common_bytes(v1, v2, &bytes, &sign); \
    uint64_t value = 0; \
    if (sign) \
    { \
        *(int64_t*)&value = _func##s(*(int64_t*)&(v1->value), *(int64_t*)&(v2->value)); \
    } \
    else \
    { \
        value = _func##u(v1->value, v2->value); \
    } \
    return const_value_get(value, bytes, sign); \
}

BINOP_FUN(add, +)
BINOP_FUN(sub, -)
BINOP_FUN(mul, *)
BINOP_FUN(div, /)
BINOP_FUN(mod, %)
BINOP_FUN(shr, >>)
BINOP_FUN(shl, <<)
BINOP_FUN(bitand, &)
BINOP_FUN(bitor, |)
BINOP_FUN(bitxor, ^)
BINOP_FUN(and, &&)
BINOP_FUN(or, ||)
BINOP_FUN(lt, <)
BINOP_FUN(lte, <=)
BINOP_FUN(gt, >)
BINOP_FUN(gte, >=)
BINOP_FUN(eq, ==)
BINOP_FUN(neq, !=)

static uint64_t int_powu(uint64_t a, uint64_t b)
{
    if (b == 0)
    {
        return 1;
    }
    else if ((b & (uint64_t)1) == (uint64_t)1) // odd
    {
        uint64_t k = int_powu(a, (b-1) >> 1);
        return a * k * k;
    }
    else // even
    {
        uint64_t k = int_powu(a, b >> 1);
        return k * k;
    }
}

static int64_t int_pows(int64_t a, int64_t b)
{
    if (b == 0)
    {
        return 1;
    }
    else if (b < 0)
    {
        return 1 / int_pows(a, -b);
    }
    else if ((b & (int64_t)1) == (int64_t)1) // odd
    {
        int64_t k = int_powu(a, (b-1) >> 1);
        return a * k * k;
    }
    else // even
    {
        int64_t k = int_powu(a, b >> 1);
        return k * k;
    }
}

BINOP_FUN_CALL(pow, int_pow)

#define UNOP_FUN(_opname, _unop) \
const_value_t* const_value_##_opname(const_value_t* v1) \
{ \
    ERROR_CONDITION(v1 == NULL, "Parameter cannot be NULL", 0); \
    uint64_t value = 0; \
    if (v1->sign) \
    { \
        value = _unop (int64_t)v1->value; \
    } \
    else \
    { \
        value = _unop v1->value; \
    } \
    return const_value_get(value, v1->num_bytes, v1->sign); \
}

#define UNOP_FUN_CALL(_opname, _func) \
const_value_t* const_value_##_opname(const_value_t* v1) \
{ \
    ERROR_CONDITION(v1 == NULL, "Parameter cannot be NULL", 0); \
    uint64_t value = 0; \
    if (v1->sign) \
    { \
        value = _func##s((int64_t)v1->value); \
    } \
    else \
    { \
        value = _func##u(v1->value); \
    } \
    return const_value_get(value, v1->num_bytes, v1->sign); \
}

UNOP_FUN(plus, +)
UNOP_FUN(neg, -)
UNOP_FUN(bitnot, ~)
UNOP_FUN(not, !)
