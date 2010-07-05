/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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
            if (v1->num_bytes == v2->num_bytes)
            {
                *sign = 1;
            }
            else if (v1->num_bytes > v2->num_bytes)
            {
                *sign = v1->sign;
            }
            else
            {
                *sign = v2->sign;
            }
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

static void get_proper_prefix(char is_signed, uint64_t value, const char** prefix, type_t** t)
{
    if (is_signed)
    {
        int int_bits = 8 * type_get_size(get_signed_int_type());
        int long_int_bits = 8 * type_get_size(get_signed_long_int_type());
        
        uint64_t bitmask = ~(uint64_t)0;

        if ((*(int64_t*)&value) < 0)
        {
            *((int64_t*)&value) = -(int64_t)value;
        }

        if (((bitmask << int_bits) & value) == 0)
        {
            *t = get_signed_int_type();
            *prefix = uniquestr("");
        }
        else if (((bitmask << long_int_bits) & value) == 0)
        {
            *t = get_signed_long_int_type();
            *prefix = uniquestr("l");
        }
        else
        {
            *t = get_signed_long_long_int_type();
            *prefix = uniquestr("ll");
        }
    }
    else
    {
        int int_bits = 8 * type_get_size(get_unsigned_int_type());
        int long_int_bits = 8 * type_get_size(get_unsigned_long_int_type());

        uint64_t bitmask = ~(uint64_t)0;

        if (((bitmask << int_bits) & value) == 0)
        {
            *t = get_unsigned_int_type();
            *prefix = uniquestr("u");
        }
        else if (((bitmask << long_int_bits) & value) == 0)
        {
            *t = get_unsigned_long_int_type();
            *prefix = uniquestr("lu");
        }
        else
        {
            *t = get_unsigned_long_long_int_type();
            *prefix = uniquestr("llu");
        }
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
            const char* prefix = NULL;

            get_proper_prefix(v->sign, v->value, &prefix, &t);

            if (v->sign)
            {
                signed long long *sll = (signed long long*)&v->value;
                snprintf(c, 63, "%lld%s", *sll, prefix);
            }
            else
            {
                snprintf(c, 63, "%llu%s", (unsigned long long)v->value, prefix);
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

UNOP_FUN(plus, +)
UNOP_FUN(neg, -)
UNOP_FUN(bitnot, ~)
UNOP_FUN(not, !)
