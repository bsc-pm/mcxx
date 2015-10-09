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

#include "cxx-cuda.h"
#include "cxx-exprtype.h"
#include "cxx-overload.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"
#include "cxx-utils.h"
#include "cxx-ambiguity.h"
#include "cxx-prettyprint.h"
#include "cxx-entrylist.h"
#include "cxx-tltype.h"
#include "cxx-diagnostic.h"
#include "cxx-codegen.h"


static type_t* cuda_get_named_type(const char* name, const decl_context_t* decl_context)
{
    scope_entry_list_t* entry_list = query_name_str(decl_context, uniquestr(name), NULL);
    ERROR_CONDITION(entry_list == NULL, "Invalid '%s' lookup", name);

    scope_entry_t* entry = entry_list_head(entry_list);
    entry_list_free(entry_list);

    if (entry->kind != SK_CLASS
            && entry->kind != SK_TYPEDEF)
    {
        ERROR_CONDITION(entry_list == NULL, "'%s' is not a valid typename", name);
    }


    return get_user_defined_type(entry);
}

static type_t* cuda_get_dim3_type(void)
{
    const decl_context_t* global_decl_context = CURRENT_COMPILED_FILE->global_decl_context;
    return cuda_get_named_type("dim3", global_decl_context);
}

static type_t* cuda_get_uint3_type(void)
{
    const decl_context_t* global_decl_context = CURRENT_COMPILED_FILE->global_decl_context;
    return cuda_get_named_type("uint3", global_decl_context);
}

static type_t* cuda_get_cudaStream_t_type(void)
{
    const decl_context_t* global_decl_context = CURRENT_COMPILED_FILE->global_decl_context;
    return cuda_get_named_type("cudaStream_t", global_decl_context);
}

void cuda_kernel_symbols_for_function_body(
        AST function_body,
        gather_decl_spec_t* gather_info, 
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const decl_context_t* block_context)
{
    // FIXME - We should keep an attribute for this in the symbol otherwise
    // we force the user to specify __global__ and __device__ in the
    // function definition again (regardless of the function declarator)
    if (gather_info->cuda.is_global
            || gather_info->cuda.is_device)
    {
        type_t* uint3_type = cuda_get_uint3_type();
        type_t* dim3_type = cuda_get_dim3_type();
        struct symbol_builtin_info_tag
        {
            const char* name;
            type_t* type;
        } cuda_builtins[] =
        {
            { "gridDim", dim3_type },
            { "blockIdx", uint3_type },
            { "blockDim", dim3_type },
            { "threadIdx", uint3_type },
            { "warpSize", get_signed_int_type() },
            // Sentinel
            { NULL, NULL },
        };

        int i = 0;
        while (cuda_builtins[i].name != NULL)
        {
            scope_entry_t* cuda_sym = new_symbol(block_context, 
                    block_context->current_scope, 
                    uniquestr(cuda_builtins[i].name));

            cuda_sym->locus = ast_get_locus(function_body);

            cuda_sym->kind = SK_VARIABLE;
            cuda_sym->type_information = cuda_builtins[i].type;
            cuda_sym->defined = 1;

            i++;
        }
    }
}

void check_nodecl_cuda_kernel_call(nodecl_t nodecl_postfix, nodecl_t nodecl_cuda_kernel_args, 
        nodecl_t nodecl_call_args, 
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t *nodecl_output)
{
    type_t* dim3_type = cuda_get_dim3_type();
    type_t* cudaStream_t_type = cuda_get_cudaStream_t_type();

    struct kernel_arg_item
    {
        const char* position;
        type_t* expected_type;
    } kernel_args[] = 
    {
        { "first", dim3_type },
        { "second", dim3_type },
        { "third", get_size_t_type() },
        { "fourth", cudaStream_t_type },
    };

    int num_items = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_cuda_kernel_args, &num_items);

    nodecl_t nodecl_actual_cuda_kernel_args = nodecl_null();

    int i = 0;

    for (i = 0; i < num_items; i++)
    {
        char is_convertible = 1;

        nodecl_t nodecl_arg = list[i];
        type_t* dest_type = kernel_args[i].expected_type;

        type_t* orig_type = nodecl_get_type(nodecl_arg);

        C_LANGUAGE()
        {
            standard_conversion_t result;
            is_convertible = standard_conversion_between_types(&result,
                    orig_type, dest_type, locus);

            if (!is_convertible)
            {
                // we mimick C++ behavior
                is_convertible =
                    (standard_conversion_between_types(&result, orig_type, get_signed_int_type(), locus)
                     && equivalent_types(no_ref(get_unqualified_type(dest_type)), dim3_type));
            }

            nodecl_arg = nodecl_shallow_copy(nodecl_arg);
        }

        CXX_LANGUAGE()
        {
            check_nodecl_function_argument_initialization(
                    nodecl_arg,
                    decl_context,
                    dest_type,
                    /* disallow_narrowing */ 0,
                    &nodecl_arg);

            is_convertible = !(nodecl_is_err_expr(nodecl_arg));
        }

        if (!is_convertible)
        {
            error_printf_at(nodecl_get_locus(nodecl_arg), "%s argument '%s' for kernel call cannot be converted to type '%s'\n",
                    kernel_args[i].position,
                    codegen_to_str(nodecl_arg, nodecl_retrieve_context(nodecl_arg)),
                    print_type_str(dest_type, decl_context));
            *nodecl_output = nodecl_make_err_expr(locus);
            return;
        }

        nodecl_actual_cuda_kernel_args = nodecl_append_to_list(nodecl_actual_cuda_kernel_args,
                nodecl_arg);
    }

    nodecl_t nodecl_plain_call = nodecl_null();
    check_nodecl_function_call(nodecl_postfix, nodecl_call_args, decl_context, &nodecl_plain_call);

    *nodecl_output = nodecl_make_cuda_kernel_call(nodecl_actual_cuda_kernel_args,
            nodecl_plain_call,
            get_void_type(),
            locus);
}

void check_cuda_kernel_call(AST expression, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    AST postfix_expr = ASTSon0(expression);
    AST cuda_kernel_args = ASTSon1(expression);
    AST call_args = ASTSon2(expression);

    const locus_t* locus = ast_get_locus(expression);

    nodecl_t nodecl_postfix = nodecl_null();
    check_expression(postfix_expr, decl_context, &nodecl_postfix);

    if (nodecl_is_err_expr(nodecl_postfix))
    {
        *nodecl_output = nodecl_make_err_expr(locus);
        return;
    }

    nodecl_t nodecl_kernel_arg_0 = nodecl_null();
    AST arg_0 = ASTSon0(cuda_kernel_args);
    if (!check_expression(arg_0, decl_context, &nodecl_kernel_arg_0))
    {
        *nodecl_output = nodecl_make_err_expr(locus);
        return;
    }

    nodecl_t nodecl_kernel_arg_1 = nodecl_null();
    AST arg_1 = ASTSon1(cuda_kernel_args);
    if (!check_expression(arg_1, decl_context, &nodecl_kernel_arg_1))
    {
        *nodecl_output = nodecl_make_err_expr(locus);
        return;
    }

    nodecl_t nodecl_kernel_arg_2 = nodecl_null();
    AST arg_2 = ASTSon2(cuda_kernel_args);
    if (arg_2 != NULL
            && !check_expression(arg_2, decl_context, &nodecl_kernel_arg_2))
    {
        *nodecl_output = nodecl_make_err_expr(locus);
        return;
    }

    nodecl_t nodecl_kernel_arg_3 = nodecl_null();
    AST arg_3 = ASTSon3(cuda_kernel_args);
    if (arg_3 != NULL
            && !check_expression(arg_3, decl_context, &nodecl_kernel_arg_3))
    {
        *nodecl_output = nodecl_make_err_expr(locus);
        return;
    }

    nodecl_t nodecl_cuda_kernel_args = nodecl_make_list_2(nodecl_kernel_arg_0, nodecl_kernel_arg_1);

    if (!nodecl_is_null(nodecl_kernel_arg_2))
    {
        nodecl_cuda_kernel_args = nodecl_append_to_list(nodecl_cuda_kernel_args, nodecl_kernel_arg_2);
    }
    if (!nodecl_is_null(nodecl_kernel_arg_3))
    {
        nodecl_cuda_kernel_args = nodecl_append_to_list(nodecl_cuda_kernel_args, nodecl_kernel_arg_3);
    }

    nodecl_t nodecl_argument_list = nodecl_null();
    check_function_arguments(call_args, decl_context, &nodecl_argument_list);

    if (!nodecl_is_null(nodecl_argument_list) 
            && nodecl_is_err_expr(nodecl_argument_list))
    {
        *nodecl_output = nodecl_make_err_expr(locus);
        return;
    }

    char is_dependent = 0;

    // Check dependent expressions
    if (nodecl_expr_is_type_dependent(nodecl_postfix))
    {
        is_dependent = 1;
    }
    else if (nodecl_expr_is_type_dependent(nodecl_kernel_arg_0)
            || nodecl_expr_is_type_dependent(nodecl_kernel_arg_1)
            || (!nodecl_is_null(nodecl_kernel_arg_2) && nodecl_expr_is_type_dependent(nodecl_kernel_arg_2))
            || (!nodecl_is_null(nodecl_kernel_arg_3) && nodecl_expr_is_type_dependent(nodecl_kernel_arg_3)))
    {
        is_dependent = 1;
    }
    else 
    {
        int num_items = 0;
        nodecl_t* list = nodecl_unpack_list(nodecl_argument_list, &num_items);

        int i;
        for (i = 0; i < num_items && !is_dependent; i++)
        {
            if (nodecl_expr_is_type_dependent(list[i]))
            {
                is_dependent = 1;
            }
        }
        DELETE(list);
    }

    nodecl_t function_form = nodecl_null();
    scope_entry_t* called_symbol = nodecl_get_symbol(nodecl_postfix);
    if (called_symbol != NULL
            && is_template_specialized_type(called_symbol->type_information))
    {
        function_form =
            nodecl_make_cxx_function_form_template_id(
                    nodecl_get_locus(nodecl_postfix));

        template_parameter_list_t* template_args =
            nodecl_get_template_parameters(nodecl_postfix);
        nodecl_set_template_parameters(function_form, template_args);
    }

    if (is_dependent)
    {
        *nodecl_output =
            nodecl_make_cuda_kernel_call(
                    nodecl_cuda_kernel_args,
                    nodecl_make_function_call(
                        nodecl_postfix,
                        nodecl_argument_list,
                        /* alternate_name */ nodecl_null(),
                        function_form,
                        get_unknown_dependent_type(),
                        locus),
                    get_unknown_dependent_type(),
                    locus);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }

    check_nodecl_cuda_kernel_call(nodecl_postfix,
            nodecl_cuda_kernel_args,
            nodecl_argument_list,
            decl_context,
            locus,
            nodecl_output);
}
