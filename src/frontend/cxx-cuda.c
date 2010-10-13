#include "cxx-cuda.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"
#include "cxx-utils.h"

static type_t* cuda_get_named_type(const char* name, decl_context_t decl_context)
{
    scope_entry_list_t* entry_list = query_unqualified_name_str(decl_context, name);
    ERROR_CONDITION(entry_list == NULL, "Invalid '%s' lookup", name);

    scope_entry_t* entry = entry_list->entry;

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
