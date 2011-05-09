#include "cxx-codegen.h"
#include "cxx-utils.h"
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"

typedef struct codegen_context_tag
{
    int indent_level;
    FILE *file;
} codegen_context_t;

static scope_entry_t* get_symbol_of_name(AST a)
{
    ERROR_CONDITION(a == NULL, "Invalid tree", 0);
    scope_entry_t* result = expression_get_symbol(a);

    ERROR_CONDITION(result == NULL, "Invalid symbol", 0);
    return result;
}

static void declare_symbol(codegen_context_t *ctx, scope_entry_t* symbol);
static void define_symbol(codegen_context_t *ctx, scope_entry_t* symbol);

static void indent(codegen_context_t *ctx)
{
    int k = ctx->indent_level;

    int i;
    for (i = 0; i < k; i++)
    {
        // FIXME - Make this configurable
        fprintf(ctx->file, "  ");
    }
}

static void codegen_type_of_symbol_rec(
        codegen_context_t *ctx,
        scope_entry_t* entry, 
        type_t* t,
        char needs_def, 
        char is_function_def)
{
    if (t == NULL)
        return;

    if (is_pointer_type(t))
    {
        codegen_type_of_symbol_rec(ctx, entry, pointer_type_get_pointee_type(t), /* needs_def */ 0, is_function_def);
    }
    else if (is_array_type(t))
    {
        codegen_type_of_symbol_rec(ctx, entry, array_type_get_element_type(t), /* needs_def */ 1, is_function_def);
    }
    else if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        codegen_type_of_symbol_rec(ctx, entry, reference_type_get_referenced_type(t), /* needs_def */ 0, is_function_def);
    }
    else if (is_pointer_to_class_type(t))
    {
        codegen_type_of_symbol_rec(ctx, entry, pointer_type_get_pointee_type(t), /* needs_def */ 0, is_function_def);
        codegen_type_of_symbol_rec(ctx, entry, pointer_to_member_type_get_class_type(t), /* needs_def */ 0, is_function_def);
    }
    else if (is_function_type(t))
    {
        codegen_type_of_symbol_rec(ctx, entry, function_type_get_return_type(t), 
                /* needs_def */ is_function_def, is_function_def);
        int i;
        for (i = 0; i < function_type_get_num_parameters(t); i++)
        {
            codegen_type_of_symbol_rec(ctx, entry, function_type_get_parameter_type_num(t, i), 
                    /* needs_def */ is_function_def, is_function_def);
        }
    }
    else if (is_vector_type(t))
    {
        codegen_type_of_symbol_rec(ctx, entry, vector_type_get_element_type(t), /* needs_def */ 1, is_function_def);
    }
    else if (is_class_type(t))
    {
        scope_entry_t* class_entry = named_type_get_symbol(t);
        if (needs_def)
        {
            define_symbol(ctx, class_entry);
        }
        else
        {
            declare_symbol(ctx, class_entry);
        }
    }
    else if (is_enum_type(t))
    {
        scope_entry_t* enum_entry = named_type_get_symbol(t);
        if (needs_def)
        {
            define_symbol(ctx, enum_entry);
        }
        else
        {
            declare_symbol(ctx, enum_entry);
        }
    }
    else
    {
        // Do nothing as it will be a builtin type
    }
}

scope_entry_list_t *clear_list = NULL;

static void add_to_clear_list(scope_entry_t* entry)
{
    clear_list = entry_list_add(clear_list, entry);
}

static void run_clear_list(void)
{
    if (clear_list != NULL)
    {
        scope_entry_list_iterator_t* it = entry_list_iterator_begin(clear_list);
        while (!entry_list_iterator_end(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            entry->entity_specs.codegen_status = CODEGEN_STATUS_NONE;

            entry_list_iterator_next(it);
        }
        entry_list_iterator_free(it);
    }
}

static void define_symbol(codegen_context_t *ctx, scope_entry_t* symbol)
{
    ERROR_CONDITION(symbol == NULL, "Invalid symbol", 0);

    // Do nothing if already defined
    if (symbol->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED)
        return;

    add_to_clear_list(symbol);

    symbol->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
    switch (symbol->kind)
    {
        case SK_CLASS:
            {
                break;
            }
        case SK_ENUM:
            {
                break;
            }
        case SK_TYPEDEF:
            {
                break;
            }
        default:
            {
                internal_error("Unhandled symbol kind %d\n", symbol->kind);
            }
    }
}

static void declare_symbol(codegen_context_t *ctx, scope_entry_t* symbol)
{
    // Do nothing if already defined or declared
    if (symbol->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED
            || symbol->entity_specs.codegen_status == CODEGEN_STATUS_DECLARED)
        return;

    add_to_clear_list(symbol);
    symbol->entity_specs.codegen_status = CODEGEN_STATUS_DECLARED;

    switch (symbol->kind)
    {
        case SK_VARIABLE:
            {
                // ???
                break;
            }
        case SK_CLASS:
            {
                indent(ctx);
                // TODO - Namespaces
                fprintf(ctx->file, "class %s;\n", symbol->symbol_name);
                break;
            }
        case SK_ENUM:
            {
                indent(ctx);
                C_LANGUAGE()
                {
                    // the symbol is already called 'enum X' in C
                    fprintf(ctx->file, "%s;\n", symbol->symbol_name);
                }
                CXX_LANGUAGE()
                {
                    // TODO - Namespaces
                    fprintf(ctx->file, "enum %s;\n", symbol->symbol_name);
                }
                break;
            }
        case SK_ENUMERATOR:
            {
                // This requires defining its enum
                break;
            }
        case SK_TYPEDEF:
            {
                codegen_type_of_symbol_rec(ctx,
                        symbol,
                        symbol->type_information,
                        /* needs_def */ 0,
                        /* is_function_def */ 0);
                indent(ctx);
                fprintf(stderr, "typedef %s;\n", print_decl_type_str(symbol->type_information, 
                        symbol->decl_context, 
                        get_qualified_symbol_name(symbol, symbol->decl_context)));
                break;
            }
        default:
            {
                internal_error("Unhandled symbol kind %d\n", symbol->kind);
                break;
            }
    }
}

static void codegen_function_def_top_level(codegen_context_t *ctx, scope_entry_t* symbol, AST statement_seq)
{
    ERROR_CONDITION(symbol->kind != SK_FUNCTION, "Invalid symbol", 0);

    codegen_type_of_symbol_rec(ctx, symbol, symbol->type_information, /* needs_def */ 0, /* is_function_def */ 1);

    indent(ctx);

    if (statement_seq != NULL)
    {
    }
}

void c_cxx_codegen_translation_unit(FILE* f, AST a, scope_link_t* sl UNUSED_PARAMETER)
{
    if (a == NULL)
        return;

    codegen_context_t ctx;
    ctx.file = f;
    ctx.indent_level = 0;

    AST it;
    for_each_element(a, it)
    {
        AST top_level = ASTSon1(it);

        if (ASTType(top_level) == AST_OBJECT_INIT)
        {
            // scope_entry_t* symbol = get_symbol_of_name(ASTSon0(top_level));
            // AST init = ASTSon1(a);

            // codegen_object_init_top_level(ctx, symbol, init);
        }
        else if (ASTType(top_level) == AST_FUNCTION_CODE)
        {
            scope_entry_t* symbol = get_symbol_of_name(ASTSon0(top_level));
            AST statement_seq = ASTSon1(top_level);

            codegen_function_def_top_level(&ctx, symbol, statement_seq);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    run_clear_list();
}

