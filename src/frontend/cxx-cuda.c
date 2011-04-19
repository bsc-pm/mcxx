#include "cxx-cuda.h"
#include "cxx-exprtype.h"
#include "cxx-overload.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"
#include "cxx-utils.h"
#include "cxx-ambiguity.h"
#include "cxx-prettyprint.h"
#include "cxx-entrylist.h"

static type_t* cuda_get_named_type(const char* name, decl_context_t decl_context)
{
    scope_entry_list_t* entry_list = query_unqualified_name_str(decl_context, name);
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

static type_t* cuda_get_dim3_type(decl_context_t decl_context)
{
    static type_t* dim3_type = NULL;
    if (dim3_type == NULL)
    {
        dim3_type = cuda_get_named_type("dim3", decl_context);
    }
    return dim3_type;
}

static type_t* cuda_get_uint3_type(decl_context_t decl_context)
{
    static type_t* uint3_type = NULL;
    if (uint3_type == NULL)
    {
        uint3_type = cuda_get_named_type("uint3", decl_context);
    }
    return uint3_type;
}

static type_t* cuda_get_cudaStream_t_type(decl_context_t decl_context)
{
    static type_t* cudaStream_t_type = NULL;
    if (cudaStream_t_type == NULL)
    {
        cudaStream_t_type = cuda_get_named_type("cudaStream_t", decl_context);
    }
    return cudaStream_t_type;
}

void cuda_kernel_symbols_for_function_body(
        AST function_body,
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context,
        decl_context_t block_context)
{
    // FIXME - We should keep an attribute for this in the symbol otherwise
    // we force the user to specify __global__ and __device__ in the
    // function definition again (regardless of the function declarator)
    if (gather_info->cuda.is_global
            || gather_info->cuda.is_device)
    {
        type_t* uint3_type = cuda_get_uint3_type(decl_context);
        type_t* dim3_type = cuda_get_dim3_type(decl_context);
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
                    block_context.current_scope, 
                    cuda_builtins[i].name);

            cuda_sym->line = ASTLine(function_body);
            cuda_sym->file = ASTFileName(function_body);

            cuda_sym->point_of_declaration = function_body;
            cuda_sym->kind = SK_VARIABLE;
            cuda_sym->type_information = cuda_builtins[i].type;
            cuda_sym->defined = 1;

            i++;
        }
    }
}

void cuda_kernel_call_check(AST expression, decl_context_t decl_context)
{
    AST postfix_expr = ASTSon0(expression);
    AST cuda_kernel_args = ASTSon1(expression);
    AST call_args = ASTSon2(expression);

    AST arg_0 = ASTSon0(cuda_kernel_args);
    if (!check_for_expression(arg_0, decl_context))
    {
        expression_set_error(expression);
        return;
    }

    AST arg_1 = ASTSon1(cuda_kernel_args);
    if (!check_for_expression(arg_1, decl_context))
    {
        expression_set_error(expression);
        return;
    }

    AST arg_2 = ASTSon2(cuda_kernel_args);
    if (arg_2 != NULL
            && !check_for_expression(arg_2, decl_context))
    {
        expression_set_error(expression);
        return;
    }

    AST arg_3 = ASTSon3(cuda_kernel_args);
    if (arg_3 != NULL
            && !check_for_expression(arg_3, decl_context))
    {
        expression_set_error(expression);
        return;
    }

    type_t* dim3_type = cuda_get_dim3_type(decl_context);
    type_t* cudaStream_t_type = cuda_get_cudaStream_t_type(decl_context);

    struct kernel_arg_item
    {
        AST tree;
        const char* position;
        type_t* expected_type;
    } kernel_args[] = 
    {
        { arg_0, "first", dim3_type },
        { arg_1, "second", dim3_type },
        { arg_2, "third", get_size_t_type() },
        { arg_3, "fourth", cudaStream_t_type },
    };

    int i = 0;
    while (kernel_args[i].tree != NULL)
    {
        char is_convertible = 1;

        AST tree = kernel_args[i].tree;
        type_t* dest_type = kernel_args[i].expected_type;

        C_LANGUAGE()
        {
            standard_conversion_t result;
            is_convertible = standard_conversion_between_types(&result, 
                    expression_get_type(tree), dest_type);

        }

        CXX_LANGUAGE()
        {
            char ambiguous_conversion = 0;
            scope_entry_t* conversor = NULL;
            is_convertible = (type_can_be_implicitly_converted_to(
                        expression_get_type(tree), 
                        get_lvalue_reference_type(get_const_qualified_type(dest_type)), 
                        decl_context, 
                        &ambiguous_conversion, &conversor)
                    && !ambiguous_conversion);
        }

        if (!is_convertible)
        {
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: warning: %s argument '%s' for kernel call cannot be converted to type '%s'\n",
                        ast_location(tree),
                        kernel_args[i].position,
                        prettyprint_in_buffer(tree),
                        print_type_str(dest_type, decl_context));
            }
            expression_set_error(expression);
            return;
        }
        i++;
    }

    // 1. Check if we might require Koenig lookup
    char might_require_koenig = 0;
    CXX_LANGUAGE()
    {
        might_require_koenig = (ASTType(postfix_expr) == AST_SYMBOL
                || ASTType(postfix_expr) == AST_CONVERSION_FUNCTION_ID
                || ASTType(postfix_expr) == AST_OPERATOR_FUNCTION_ID);
    }

    if (!_check_for_functional_expression(expression, postfix_expr, call_args, decl_context, might_require_koenig))
    {
        expression_set_error(expression);
        return;
    }

    // A CUDA kernel should return void
    expression_set_type(expression, get_void_type());
}
