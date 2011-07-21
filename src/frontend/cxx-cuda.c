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
#include "cxx-attrnames.h"

#if 0
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
#endif

static type_t* cuda_get_dim3_type(void)
{
    static type_t* dim3_type = NULL;
    if (dim3_type == NULL)
    {
        decl_context_t global_decl_context = scope_link_get_global_decl_context(CURRENT_COMPILED_FILE->scope_link);

        scope_entry_t* new_class_sym = NULL; 
        C_LANGUAGE()
        {
            new_class_sym = new_symbol(global_decl_context, global_decl_context.current_scope, "struct dim3");
            insert_alias(global_decl_context.current_scope, new_class_sym, "dim3");
        }
        CXX_LANGUAGE()
        {
            new_class_sym = new_symbol(global_decl_context, global_decl_context.current_scope, "dim3");
        }
        new_class_sym->kind = SK_CLASS;
        new_class_sym->type_information = get_new_class_type(global_decl_context, CK_STRUCT);
        decl_context_t class_context = new_class_context(global_decl_context, new_class_sym);

        class_type_set_inner_context(new_class_sym->type_information, class_context);

        // struct dim3
        // {
        //     unsigned int x, y, z;

        //     __attribute__((host)) __attribute__((device)) dim3(unsigned int vx = 1, unsigned int vy = 1, unsigned int vz = 1) : x(vx), y(vy), z(vz) {}
        //     __attribute__((host)) __attribute__((device)) dim3(uint3 v) : x(v.x), y(v.y), z(v.z) {}
        //     __attribute__((host)) __attribute__((device)) operator uint3(void) { uint3 t; t.x = x; t.y = y; t.z = z; return t; }
        // };

        const char *names[] = { "x", "y", "z", /* sentinel */ NULL };

        int i;
        for (i = 0; names[i] != NULL; i++)
        {
            scope_entry_t* member_sym = new_symbol(class_context, class_context.current_scope, names[i]);
            member_sym->kind = SK_VARIABLE;
            member_sym->type_information = get_unsigned_int_type();
            member_sym->entity_specs.is_member = 1;
            member_sym->entity_specs.class_type = get_user_defined_type(new_class_sym);

            class_type_add_nonstatic_data_member(new_class_sym->type_information, member_sym);
        }

        // FIXME - We should register the constructors and conversion
        dim3_type = get_user_defined_type(new_class_sym);
    }
    return dim3_type;
}

static type_t* cuda_get_uint3_type(void)
{
    static type_t* uint3_type = NULL;
    if (uint3_type == NULL)
    {
        decl_context_t global_decl_context = scope_link_get_global_decl_context(CURRENT_COMPILED_FILE->scope_link);

        scope_entry_t* new_class_sym = NULL; 
        C_LANGUAGE()
        {
            new_class_sym = new_symbol(global_decl_context, global_decl_context.current_scope, "struct uint3");
            insert_alias(global_decl_context.current_scope, new_class_sym, "uint3");
        }
        CXX_LANGUAGE()
        {
            new_class_sym = new_symbol(global_decl_context, global_decl_context.current_scope, "uint3");
        }

        new_class_sym->kind = SK_CLASS;
        new_class_sym->type_information = get_new_class_type(global_decl_context, CK_STRUCT);
        decl_context_t class_context = new_class_context(global_decl_context, new_class_sym);

        class_type_set_inner_context(new_class_sym->type_information, class_context);

        // struct uint3
        // {
        //     unsigned int x, y, z;
        // };

        const char *names[] = { "x", "y", "z", /* sentinel */ NULL };

        int i;
        for (i = 0; names[i] != NULL; i++)
        {
            scope_entry_t* member_sym = new_symbol(class_context, class_context.current_scope, names[i]);
            member_sym->kind = SK_VARIABLE;
            member_sym->type_information = get_unsigned_int_type();
            member_sym->entity_specs.is_member = 1;
            member_sym->entity_specs.class_type = get_user_defined_type(new_class_sym);

            class_type_add_nonstatic_data_member(new_class_sym->type_information, member_sym);
        }


        uint3_type = get_user_defined_type(new_class_sym);
    }
    return uint3_type;
}

static type_t* cuda_get_cudaStream_t_type(void)
{
    static type_t* cudaStream_t_type = NULL;
    if (cudaStream_t_type == NULL)
    {
        decl_context_t global_decl_context = scope_link_get_global_decl_context(CURRENT_COMPILED_FILE->scope_link);

        // typedef struct CUstream_st *cudaStream_t;
        scope_entry_t* new_class_sym = new_symbol(global_decl_context, global_decl_context.current_scope, "struct CUstream_st");
        new_class_sym->kind = SK_CLASS;
        new_class_sym->type_information = get_new_class_type(global_decl_context, CK_STRUCT);

        scope_entry_t* new_typedef_sym = new_symbol(global_decl_context, global_decl_context.current_scope, "cudaStream_t");
        new_typedef_sym->kind = SK_TYPEDEF;
        new_typedef_sym->type_information = get_pointer_type(get_user_defined_type(new_class_sym));

        cudaStream_t_type = get_user_defined_type(new_typedef_sym);
    }
    return cudaStream_t_type;
}

void cuda_kernel_symbols_for_function_body(
        AST function_body,
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context UNUSED_PARAMETER,
        decl_context_t block_context)
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

    type_t* dim3_type = cuda_get_dim3_type();
    type_t* cudaStream_t_type = cuda_get_cudaStream_t_type();

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
	// Sentinel
	{ NULL, NULL, NULL }
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
            // FIXME - CUDA and C do not mix
            // if (!checking_ambiguity())
            // {
            //     fprintf(stderr, "%s: warning: %s argument '%s' for kernel call cannot be converted to type '%s'\n",
            //             ast_location(tree),
            //             kernel_args[i].position,
            //             prettyprint_in_buffer(tree),
            //             print_type_str(dest_type, decl_context));
            // }
            // expression_set_error(expression);
            // return;
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

    ASTAttrSetValueType(expression, LANG_IS_CUDA_KERNEL_CALL, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expression, LANG_KERNEL_CONFIGURATION, tl_type_t, tl_ast(cuda_kernel_args));
    ASTAttrSetValueType(expression, LANG_CALLED_EXPRESSION, tl_type_t, tl_ast(postfix_expr));
    ASTAttrSetValueType(expression, LANG_FUNCTION_ARGUMENTS, tl_type_t, tl_ast(call_args));
}

void init_cuda_builtins(decl_context_t decl_context UNUSED_PARAMETER)
{
    cuda_get_dim3_type();
    cuda_get_uint3_type();
    // cuda_get_cudaStream_t_type();
}
