/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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



#include "cxx-intelsupport.h"

#include "cxx-typeutils.h"
#include "cxx-nodecl.h"
#include "cxx-nodecl-output.h"
#include "cxx-exprtype.h"
#include "cxx-diagnostic.h"
#include "cxx-utils.h"

void intel_check_assume(
        AST expression,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_out)
{
    AST assumed_expr = ast_get_child(expression, 0);

    nodecl_t nodecl_assumed_expr = nodecl_null();
    check_expression_non_executable(assumed_expr, decl_context, &nodecl_assumed_expr);

    intel_check_assume_nodecl(nodecl_assumed_expr,
            decl_context, 
            ast_get_locus(expression),
            nodecl_out);
}

void intel_check_assume_nodecl(
        nodecl_t assumed_expr,
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_out)
{
    if (nodecl_is_err_expr(assumed_expr))
    {
        *nodecl_out = assumed_expr;
        return;
    }

    type_t* t = nodecl_get_type(assumed_expr);

    standard_conversion_t scs;
    C_LANGUAGE()
    {
        if (!standard_conversion_between_types(&scs, t, get_signed_int_type(), locus))
        {
            error_printf_at(locus, "argument of '__assume' is of type '%s' not convertible to int\n",
                    print_type_str(t, decl_context));
            nodecl_free(assumed_expr);
            *nodecl_out = nodecl_make_err_expr(locus);
            return;
        }
    }
    CXX_LANGUAGE()
    {
        if (!standard_conversion_between_types(&scs, t, get_bool_type(), locus))
        {
            error_printf_at(nodecl_get_locus(assumed_expr), "argument of '__assume' is of type '%s' not convertible to bool\n",
                    print_type_str(t, decl_context));
            nodecl_free(assumed_expr);
            *nodecl_out = nodecl_make_err_expr(locus);
            return;
        }
    }

    *nodecl_out = nodecl_make_intel_assume(assumed_expr, get_void_type(), locus);
}

void intel_check_assume_aligned(
        AST expression,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_out)
{
    AST pointer_expr = ast_get_child(expression, 0);

    nodecl_t nodecl_pointer_expr = nodecl_null();
    check_expression_non_executable(pointer_expr, decl_context, &nodecl_pointer_expr);

    AST alignment = ast_get_child(expression, 1);

    nodecl_t nodecl_alignment_expr = nodecl_null();
    check_expression_non_executable_must_be_constant(alignment, decl_context, &nodecl_alignment_expr);

    intel_check_assume_aligned_nodecl(
            nodecl_pointer_expr,
            nodecl_alignment_expr,
            decl_context,
            ast_get_locus(expression),
            nodecl_out);
}

void intel_check_assume_aligned_nodecl(
        nodecl_t pointer_arg,
        nodecl_t alignment,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_out)
{
    if (nodecl_is_err_expr(pointer_arg))
    {
        *nodecl_out = pointer_arg;
        nodecl_free(alignment);
        return;
    }

    if (nodecl_is_err_expr(alignment))
    {
        *nodecl_out = alignment;
        nodecl_free(pointer_arg);
        return;
    }

    if (!is_pointer_type(no_ref(nodecl_get_type(pointer_arg))))
    {
        error_printf_at(nodecl_get_locus(pointer_arg), "first argument of __assume_aligned must be a pointer\n");

        *nodecl_out = nodecl_make_err_expr(locus);
        nodecl_free(pointer_arg);
        nodecl_free(alignment);
        return;
    }

    if (!nodecl_is_constant(alignment) || !const_value_is_integer(nodecl_get_constant(alignment)))
    {
        error_printf_at(nodecl_get_locus(alignment), "second argument of __assume_aligned argument must be an integer constant\n");
        *nodecl_out = nodecl_make_err_expr(locus);
        nodecl_free(pointer_arg);
        nodecl_free(alignment);
        return;
    }
    else
    {
        int v = const_value_cast_to_signed_int(nodecl_get_constant(alignment));
        char is_power_of_two = (v && !(v & (v - 1))); // Bithack
        if (!is_power_of_two)
        {
            error_printf_at(nodecl_get_locus(alignment), "second argument of __assume_aligned argument must be a power of two constant\n");
            *nodecl_out = nodecl_make_err_expr(locus);
            nodecl_free(pointer_arg);
            nodecl_free(alignment);
            return;
        }
    }

    *nodecl_out = nodecl_make_intel_assume_aligned(pointer_arg, alignment, get_void_type(), locus);
}

char is_intel_vector_struct_type(type_t* t, int *size)
{
    if (!CURRENT_CONFIGURATION->enable_intel_vector_types)
        return 0;

    if (t == NULL)
        return 0;

#define VECTOR_SIZE(n)  \
    VECTOR_SIZE_(_, n) \
    VECTOR_SIZE_(d_, n) \
    VECTOR_SIZE_(i_, n)

#define VECTOR_TESTS \
            VECTOR_SIZE(128) \
            VECTOR_SIZE(256) \
            VECTOR_SIZE(512) \

#define VECTOR_SIZE_(p, n) \
    if (equivalent_types(t, get_m##n##p##struct_type())) \
    { \
        if (size != NULL) *size = n / 8; \
        return 1; \
    } else

    VECTOR_TESTS
    return 0;

#undef VECTOR_SIZE_
#undef VECTOR_SIZE
#undef VECTOR_TESTS

    return 0;
}

// MMX registers. Completely untyped
static type_t* __m64_struct_type = NULL;
type_t* get_m64_struct_type(void)
{
    return __m64_struct_type;
}
static scope_entry_t* __m64_typedef = NULL;
scope_entry_t* get_m64_typedef(void)
{
    return __m64_typedef;
}

#define GET_MXX_STRUCT_TYPE(n) \
    static type_t* __m##n##_struct_type = NULL; \
    type_t* get_m##n##_struct_type(void) \
    { \
        return __m##n##_struct_type; \
    } \
    static scope_entry_t* __m##n##_typedef = NULL; \
    scope_entry_t* get_m##n##_typedef(void) \
    { \
        return __m##n##_typedef; \
    } \
    \
    static type_t* __m##n##d_struct_type = NULL; \
    type_t* get_m##n##d_struct_type(void) \
    { \
        return __m##n##d_struct_type; \
    } \
    static scope_entry_t* __m##n##d_typedef = NULL; \
    scope_entry_t* get_m##n##d_typedef(void) \
    { \
        return __m##n##d_typedef; \
    } \
    \
    static type_t* __m##n##i_struct_type = NULL; \
    type_t* get_m##n##i_struct_type(void) \
    { \
        return __m##n##i_struct_type; \
    } \
    static scope_entry_t* __m##n##i_typedef = NULL; \
    scope_entry_t* get_m##n##i_typedef(void) \
    { \
        return __m##n##i_typedef; \
    }

GET_MXX_STRUCT_TYPE(128)
GET_MXX_STRUCT_TYPE(256)
GET_MXX_STRUCT_TYPE(512)

void sign_in_intel_simd_types(const decl_context_t* decl_context)
{
    struct {
        const char* name;
        type_t** field;
        scope_entry_t** typedef_name;
        enum type_tag_t type_tag;
    } vector_names[] = {
        { "union __m64", &__m64_struct_type, &__m64_typedef, TT_UNION },

        { "struct __m128",  &__m128_struct_type,  &__m128_typedef,  TT_STRUCT },
        { "struct __m128d", &__m128d_struct_type, &__m128d_typedef, TT_STRUCT },
        { "union __m128i",  &__m128i_struct_type, &__m128i_typedef, TT_UNION  },

        { "union __m256",   &__m256_struct_type,  &__m256_typedef,  TT_UNION  },
        { "struct __m256d", &__m256d_struct_type, &__m256d_typedef, TT_STRUCT },
        { "union __m256i",  &__m256i_struct_type, &__m256i_typedef, TT_UNION  },

        { "union __m512",   &__m512_struct_type,  &__m512_typedef,  TT_UNION  },
        { "union __m512d",  &__m512d_struct_type, &__m512d_typedef, TT_UNION  },
        { "union __m512i",  &__m512i_struct_type, &__m512i_typedef, TT_UNION  },
        { NULL, NULL, NULL, TT_INVALID }
    };

    if (CURRENT_CONFIGURATION->enable_intel_vector_types)
    {
        int i;
        for (i = 0; vector_names[i].name != NULL; i++)
        {
            const char* name = vector_names[i].name;
            const char* typedef_name = name;
            // Skip "struct "
            if (vector_names[i].type_tag == TT_STRUCT)
            {
                typedef_name += strlen("struct ");
            }
            else if (vector_names[i].type_tag == TT_UNION)
            {
                typedef_name += strlen("union ");
            }
            else
            {
                internal_error("Invalid type tag", 0);
            }

            CXX_LANGUAGE()
            {
                name = typedef_name;
            }

            // class-name
            {
                const char* class_name = name;
                scope_entry_t* sym = new_symbol(decl_context, decl_context->current_scope, uniquestr(class_name));
                sym->locus = make_locus("(builtin-simd-type)", 0, 0);
                sym->kind = SK_CLASS;
                sym->type_information = get_new_class_type(decl_context, vector_names[i].type_tag);

                *(vector_names[i].field) = get_user_defined_type(sym);
            }

            // typedef-name
            {
                scope_entry_t* sym = new_symbol(decl_context, decl_context->current_scope, uniquestr(typedef_name));
                sym->locus = make_locus("(builtin-simd-type)", 0, 0);
                sym->kind = SK_TYPEDEF;
                sym->type_information = *(vector_names[i].field);
                sym->defined = 1;
                symbol_entity_specs_set_is_user_declared(sym, 1);

                *(vector_names[i].typedef_name) = sym;
            }
        }
    }
    else
    {
        int i;
        for (i = 0; vector_names[i].name != NULL; i++)
        {
            *(vector_names[i].field) = NULL;
            *(vector_names[i].typedef_name) = NULL;
        }
    }
}

type_t* intel_vector_struct_type_get_vector_type(type_t* vector_type)
{
    if (0);
    else if (equivalent_types(vector_type, get_m128i_struct_type()))
        return get_vector_type_by_bytes(get_signed_int_type(), 16);
    else if (equivalent_types(vector_type, get_m128_struct_type()))
        return get_vector_type_by_bytes(get_float_type(), 16);
    else if (equivalent_types(vector_type, get_m128d_struct_type()))
        return get_vector_type_by_bytes(get_double_type(), 16);

    else if (equivalent_types(vector_type, get_m256i_struct_type()))
        return get_vector_type_by_bytes(get_signed_int_type(), 32);
    else if (equivalent_types(vector_type, get_m256_struct_type()))
        return get_vector_type_by_bytes(get_float_type(), 32);
    else if (equivalent_types(vector_type, get_m256d_struct_type()))
        return get_vector_type_by_bytes(get_double_type(), 32);

    else if (equivalent_types(vector_type, get_m512i_struct_type()))
        return get_vector_type_by_bytes(get_signed_int_type(), 64);
    else if (equivalent_types(vector_type, get_m512_struct_type()))
        return get_vector_type_by_bytes(get_float_type(), 64);
    else if (equivalent_types(vector_type, get_m512d_struct_type()))
        return get_vector_type_by_bytes(get_double_type(), 64);

    return NULL;
}

type_t* vector_type_get_intel_vector_struct_type(type_t* vector_type)
{
    ERROR_CONDITION(!is_vector_type(vector_type), "Invalid type", 0);

    type_t* element_type = vector_type_get_element_type(vector_type);
    int vector_size = vector_type_get_num_elements(vector_type) * type_get_size(element_type);

    ERROR_CONDITION(vector_size > 64, "Vector too large", 0);

    enum  { VEC_FLOAT = 1 << 10, VEC_DOUBLE = 1 << 11, VEC_INTEGER = 1 << 12 };

    if (is_float_type(element_type))
    {
        vector_size |= VEC_FLOAT;
    }
    else if (is_double_type(element_type))
    {
        vector_size |= VEC_DOUBLE;
    }
    else if (is_float_type(element_type))
    {
        vector_size |= VEC_INTEGER;
    }
    else
    {
        return NULL;
    }

#define VECTOR_KIND(TYPE, VEC_SIZE) (TYPE | VEC_SIZE)

    switch (vector_size)
    {
        case VECTOR_KIND(VEC_INTEGER, 16):
            return get_m128i_struct_type();
        case VECTOR_KIND(VEC_INTEGER, 32):
            return get_m256i_struct_type();
        case VECTOR_KIND(VEC_INTEGER, 64):
            return get_m512i_struct_type();

        case VECTOR_KIND(VEC_FLOAT, 16):
            return get_m128_struct_type();
        case VECTOR_KIND(VEC_FLOAT, 32):
            return get_m256_struct_type();
        case VECTOR_KIND(VEC_FLOAT, 64):
            return get_m512_struct_type();

        case VECTOR_KIND(VEC_DOUBLE, 16):
            return get_m128d_struct_type();
        case VECTOR_KIND(VEC_DOUBLE, 32):
            return get_m256d_struct_type();
        case VECTOR_KIND(VEC_DOUBLE, 64):
            return get_m512d_struct_type();

        default:
            return NULL;
    }

#undef VECTOR_KIND
}

scope_entry_t* vector_type_get_intel_vector_typedef(type_t* vector_type)
{
    ERROR_CONDITION(!is_vector_type(vector_type), "Invalid type", 0);

    type_t* element_type = vector_type_get_element_type(vector_type);
    int vector_size = vector_type_get_num_elements(vector_type) * type_get_size(element_type);

    if (vector_size > 64)
        return NULL;

    enum  { VEC_FLOAT = 1 << 10, VEC_DOUBLE = 1 << 11, VEC_INTEGER = 1 << 12 };

    if (is_float_type(element_type))
    {
        vector_size |= VEC_FLOAT;
    }
    else if (is_double_type(element_type))
    {
        vector_size |= VEC_DOUBLE;
    }
    else if (is_integral_type(element_type))
    {
        vector_size |= VEC_INTEGER;
    }
    else
    {
        return NULL;
    }

#define VECTOR_KIND(TYPE, VEC_SIZE) (TYPE | VEC_SIZE)

    switch (vector_size)
    {
        case VECTOR_KIND(VEC_INTEGER, 16):
            return get_m128i_typedef();
        case VECTOR_KIND(VEC_INTEGER, 32):
            return get_m256i_typedef();
        case VECTOR_KIND(VEC_INTEGER, 64):
            return get_m512i_typedef();

        case VECTOR_KIND(VEC_FLOAT, 16):
            return get_m128_typedef();
        case VECTOR_KIND(VEC_FLOAT, 32):
            return get_m256_typedef();
        case VECTOR_KIND(VEC_FLOAT, 64):
            return get_m512_typedef();

        case VECTOR_KIND(VEC_DOUBLE, 16):
            return get_m128d_typedef();
        case VECTOR_KIND(VEC_DOUBLE, 32):
            return get_m256d_typedef();
        case VECTOR_KIND(VEC_DOUBLE, 64):
            return get_m512d_typedef();

        default:
            return NULL;
    }

#undef VECTOR_KIND
}

// This function allows conversion between vector types of the same size as an Intel struct
char vector_type_to_intel_vector_struct_reinterpret_type(type_t* orig, type_t* dest)
{
    if (!CURRENT_CONFIGURATION->enable_intel_vector_types)
        return 0;

    if (!is_vector_type(orig)
            || !is_intel_vector_struct_type(dest, NULL))
        return 0;

    int vector_size = vector_type_get_vector_size_in_bytes(no_ref(orig));
    // type_t* element_type = vector_type_get_element_type(no_ref(orig));
    type_t* dest_struct = get_unqualified_type(no_ref(dest));

    switch (vector_size)
    {
        case 16:
            return (equivalent_types(dest_struct, get_m128_struct_type())
                    || equivalent_types(dest_struct, get_m128d_struct_type())
                    || equivalent_types(dest_struct, get_m128i_struct_type()));
        case 32:
            return (equivalent_types(dest_struct, get_m256_struct_type())
                    || equivalent_types(dest_struct, get_m256d_struct_type())
                    || equivalent_types(dest_struct, get_m256i_struct_type()));
        case 64:
            return (equivalent_types(dest_struct, get_m512_struct_type())
                    || equivalent_types(dest_struct, get_m512d_struct_type())
                    || equivalent_types(dest_struct, get_m512i_struct_type()));
    }

    return 0;
}

char intel_vector_struct_to_intel_vector_struct_reinterpret_type(type_t* orig, type_t* dest)
{
    if (!CURRENT_CONFIGURATION->enable_intel_vector_types)
        return 0;

    struct vector_kinds_tag {
        type_t* (*v[3])(void); 
    } vector_list[] = {
        { { get_m128_struct_type, get_m128d_struct_type, get_m128i_struct_type } },
        { { get_m256_struct_type, get_m256d_struct_type, get_m256i_struct_type } } ,
        { { get_m512_struct_type, get_m512d_struct_type, get_m512i_struct_type } },
        { { NULL, NULL, NULL } },
    };

    int i;
    for (i = 0; vector_list[i].v[0] != NULL; i++)
    {
        int j;
        for (j = 0; j < 3; j++)
        {
            if (equivalent_types((vector_list[i].v[j])(), orig))
            {
                int k;
                for (k = 0; k < 3; k++)
                {
                    // Note that when k == j this function should not have been used
                    if (equivalent_types((vector_list[i].v[k])(), dest))
                    {
                        return 1;
                    }
                }
                return 0;
            }
        }
    }

    return 0;
}

void sign_in_icc_intrinsics(const decl_context_t* decl_context)
{
    const locus_t* builtins_locus = make_locus("(intel-builtins)", 0, 0);
    // Xeon
#include "cxx-iccbuiltins.h"

    // Knights Corner (aka MIC)
    builtins_locus = make_locus("(intel-builtins-knc)", 0, 0);
#include "cxx-iccbuiltins-knc.h"
}

