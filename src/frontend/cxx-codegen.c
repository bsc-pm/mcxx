#include "cxx-codegen.h"
#include "cxx-utils.h"
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"
#include "cxx-prettyprint.h"
#include "cxx-nodecl-visitor.h"
#include <string.h>

typedef
struct nodecl_codegen_visitor_tag
{
    // Base visitor
    nodecl_external_visitor_t _base_visitor;

    // Codegen
    FILE *file;
    int indent_level;
    scope_entry_t* current_sym;
} nodecl_codegen_visitor_t;

static void indent(nodecl_codegen_visitor_t* v)
{
    int i;
    for (i = 0; i < v->indent_level; i++)
    {
        // FIXME - Make this configurable
        fprintf(v->file, "  ");
    }
}

static void walk_type_for_symbols(
        nodecl_codegen_visitor_t *ctx,
        type_t* t,
        char needs_def, 
        void symbol_to_declare(nodecl_codegen_visitor_t*, scope_entry_t*),
        void symbol_to_define(nodecl_codegen_visitor_t*, scope_entry_t*))
{
    if (t == NULL)
        return;

    if (is_pointer_type(t))
    {
        walk_type_for_symbols(ctx, pointer_type_get_pointee_type(t), /* needs_def */ 0, symbol_to_declare, symbol_to_define);
    }
    else if (is_array_type(t))
    {
        walk_type_for_symbols(ctx, array_type_get_element_type(t), /* needs_def */ 1, symbol_to_declare, symbol_to_define);
    }
    else if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        walk_type_for_symbols(ctx, reference_type_get_referenced_type(t), /* needs_def */ 0, symbol_to_declare, symbol_to_define);
    }
    else if (is_pointer_to_class_type(t))
    {
        walk_type_for_symbols(ctx, pointer_type_get_pointee_type(t), /* needs_def */ 0, symbol_to_declare, symbol_to_define);
        walk_type_for_symbols(ctx, pointer_to_member_type_get_class_type(t), /* needs_def */ 0, symbol_to_declare, symbol_to_define);
    }
    else if (is_function_type(t))
    {
        walk_type_for_symbols(ctx, function_type_get_return_type(t), 
                /* needs_def */ 0, symbol_to_declare, symbol_to_define);
        int i;
        for (i = 0; i < function_type_get_num_parameters(t); i++)
        {
            walk_type_for_symbols(ctx, function_type_get_parameter_type_num(t, i), 
                    /* needs_def */ 0, symbol_to_declare, symbol_to_define);
        }
    }
    else if (is_vector_type(t))
    {
        walk_type_for_symbols(ctx, vector_type_get_element_type(t), /* needs_def */ 1, symbol_to_declare, symbol_to_define);
    }
    else if (is_class_type(t))
    {
        scope_entry_t* class_entry = named_type_get_symbol(t);
        if (needs_def)
        {
            symbol_to_define(ctx, class_entry);
        }
        else
        {
            symbol_to_declare(ctx, class_entry);
        }
    }
    else if (is_enum_type(t))
    {
        scope_entry_t* enum_entry = named_type_get_symbol(t);
        if (needs_def)
        {
            symbol_to_define(ctx, enum_entry);
        }
        else
        {
            symbol_to_declare(ctx, enum_entry);
        }
    }
    else
    {
        // Do nothing as it will be a builtin type
    }
}

static void declare_symbol(nodecl_codegen_visitor_t *ctx, scope_entry_t* symbol);
static void define_symbol(nodecl_codegen_visitor_t *ctx, scope_entry_t* symbol);
static void declare_symbol_if_nonlocal(nodecl_codegen_visitor_t *ctx, scope_entry_t* symbol);
static void define_symbol_if_nonlocal(nodecl_codegen_visitor_t *ctx, scope_entry_t* symbol);

static void codegen_type_of_symbol(
        nodecl_codegen_visitor_t *ctx,
        type_t* t,
        char needs_def)
{
    walk_type_for_symbols(ctx, t, needs_def, declare_symbol, define_symbol);
}

static void codegen_type_of_symbol_only_nonlocal(
        nodecl_codegen_visitor_t *ctx,
        type_t* t,
        char needs_def)
{
    walk_type_for_symbols(ctx, t, needs_def, declare_symbol_if_nonlocal, define_symbol_if_nonlocal);
}

static void define_local_entities_in_trees(nodecl_codegen_visitor_t* ctx, nodecl_t node, scope_t* current_scope)
{
    if (nodecl_is_null(node))
        return;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        define_local_entities_in_trees(ctx, nodecl_get_children(node, i), current_scope);
    }

    scope_entry_t* entry = nodecl_get_symbol(node);
    if (entry != NULL
            && entry->type_information != NULL)
    {
        codegen_type_of_symbol(ctx, entry->type_information, /* needs def */ 1);
        if (current_scope == entry->decl_context.current_scope)
        {
            define_symbol(ctx, entry);
        }
    }

    type_t* type = nodecl_get_type(node);
    if (type != NULL)
    {
        codegen_type_of_symbol(ctx, type, /* needs def */ 1);
    }
}

static void define_nonlocal_entities_in_trees(nodecl_codegen_visitor_t* ctx, nodecl_t node)
{
    if (nodecl_is_null(node))
        return;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        define_nonlocal_entities_in_trees(ctx, nodecl_get_children(node, i));
    }

    scope_entry_t* entry = nodecl_get_symbol(node);
    if (entry != NULL
            && entry->type_information != NULL)
    {
        codegen_type_of_symbol_only_nonlocal(ctx, entry->type_information, /* needs def */ 1);
        define_symbol_if_nonlocal(ctx, entry);
    }

    type_t* type = nodecl_get_type(node);
    if (type != NULL)
    {
        codegen_type_of_symbol_only_nonlocal(ctx, type, /* needs def */ 1);
    }
}

static scope_entry_list_t *_clear_list = NULL;

static void add_to_clear_list(scope_entry_t* entry)
{
    _clear_list = entry_list_add(_clear_list, entry);
}

static void run_clear_list(void)
{
    if (_clear_list != NULL)
    {
        scope_entry_list_iterator_t* it = entry_list_iterator_begin(_clear_list);
        while (!entry_list_iterator_end(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            entry->entity_specs.codegen_status = CODEGEN_STATUS_NONE;

            entry_list_iterator_next(it);
        }
        entry_list_iterator_free(it);
    }
    entry_list_free(_clear_list);
    _clear_list = NULL;
}

static void define_symbol(nodecl_codegen_visitor_t *ctx, scope_entry_t* symbol)
{
    ERROR_CONDITION(symbol == NULL, "Invalid symbol", 0);

    if (symbol->do_not_print)
        return;

    add_to_clear_list(symbol);

    switch (symbol->kind)
    {
        case SK_VARIABLE:
            {
                declare_symbol(ctx, symbol);
                break;
            }
        case SK_TYPEDEF:
            {
                codegen_type_of_symbol(ctx,
                        symbol->type_information,
                        /* needs_def */ 1);
                // Do nothing if already defined
                if (symbol->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED)
                    return;
                indent(ctx);
                fprintf(ctx->file, "typedef %s;\n", print_decl_type_str(symbol->type_information, 
                            symbol->decl_context, symbol->symbol_name));
                break;

                ctx->indent_level++;

                int i;
                for (i = 0; i < enum_type_get_num_enumerators(symbol->type_information); i++)
                {
                    scope_entry_t* enumerator = enum_type_get_enumerator_num(symbol->type_information, i);
                    if (i != 0)
                    {
                        fprintf(ctx->file, ",\n");
                    }
                    indent(ctx);
                    fprintf(ctx->file, "%s", enumerator->symbol_name);
                    enumerator->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                }

                ctx->indent_level--;

                fprintf(ctx->file, "\n");
                indent(ctx);
                fprintf(ctx->file, "};\n");
                break;
            }
        case SK_CLASS:
            {
                // Do nothing if already defined
                if (symbol->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED)
                    return;

                int i = 0;
                C_LANGUAGE()
                {
                    // the symbol will be already called 'struct/union X' in C
                    indent(ctx);
                    fprintf(ctx->file, "%s\n", symbol->symbol_name);
                    indent(ctx);
                    fprintf(ctx->file, "{\n");
                }
                access_specifier_t default_access_spec = AS_UNKNOWN;
                CXX_LANGUAGE()
                {
                    // TODO - Namespaces
                    const char *class_key = "";
                    switch (class_type_get_class_kind(symbol->type_information))
                    {
                        case CK_CLASS:
                            class_key = "class";
                            default_access_spec = AS_PRIVATE;
                            break;
                        case CK_STRUCT:
                            class_key = "struct";
                            default_access_spec = AS_PUBLIC;
                            break;
                        case CK_UNION:
                            class_key = "union";
                            default_access_spec = AS_PUBLIC;
                            break;
                        default:
                            internal_error("Invalid class kind", 0);
                    }

                    if (class_type_get_num_bases(symbol->type_information) != 0)
                    {
                        // We need to define all the bases first
                        for (i = 0; i < class_type_get_num_bases(symbol->type_information); i++)
                        {
                            scope_entry_t* entry = class_type_get_base_num(symbol->type_information, i, NULL, NULL, NULL);
                            define_symbol(ctx, entry);
                        }
                    }

                    indent(ctx);
                    fprintf(ctx->file, "%s %s", class_key, symbol->symbol_name);

                    if (class_type_get_num_bases(symbol->type_information) != 0)
                    {
                        fprintf(ctx->file, " : ");
                        for (i = 0; i < class_type_get_num_bases(symbol->type_information); i++)
                        {
                            if (i != 0)
                            {
                                fprintf(ctx->file, ", ");
                            }

                            access_specifier_t current_access_spec = AS_UNKNOWN;
                            char is_virtual = 0;
                            scope_entry_t* base = class_type_get_base_num(symbol->type_information, i, 
                                    &is_virtual, 
                                    /* is_dependent */ NULL, 
                                    &current_access_spec);

                            if (is_virtual)
                            {
                                fprintf(ctx->file, "virtual ");
                            }

                            if (current_access_spec != default_access_spec)
                            {
                                if (current_access_spec == AS_PUBLIC)
                                {
                                    fprintf(ctx->file, "public ");
                                }
                                else if (current_access_spec == AS_PRIVATE)
                                {
                                    fprintf(ctx->file, "private ");
                                }
                                else if (current_access_spec == AS_PROTECTED)
                                {
                                    fprintf(ctx->file, "protected ");
                                }
                                else
                                {
                                    internal_error("Unreachable code", 0);
                                }
                            }

                            fprintf(ctx->file, "%s", get_qualified_symbol_name(base, symbol->decl_context));
                        }
                    }

                    fprintf(ctx->file, "{\n");
                }


                access_specifier_t current_access_spec = default_access_spec;
                int num_members = class_type_get_num_members(symbol->type_information);
                for (i = 0; i < num_members; i++)
                {
                    scope_entry_t* entry = class_type_get_member_num(symbol->type_information, i);
                    access_specifier_t access_spec = entry->entity_specs.access;

                    CXX_LANGUAGE()
                    {
                        ctx->indent_level += 1;
                        if (current_access_spec != access_spec)
                        {
                            current_access_spec = access_spec;

                            indent(ctx);
                            if (current_access_spec == AS_PUBLIC)
                            {
                                fprintf(ctx->file, "public:\n");
                            }
                            else if (current_access_spec == AS_PRIVATE)
                            {
                                fprintf(ctx->file, "private:\n");
                            }
                            else if (current_access_spec == AS_PROTECTED)
                            {
                                fprintf(ctx->file, "protected:\n");
                            }
                            else
                            {
                                internal_error("Unreachable code", 0);
                            }
                        }
                    }

                    ctx->indent_level += 1;
                    declare_symbol(ctx, entry);
                    ctx->indent_level--;

                    CXX_LANGUAGE()
                    {
                        ctx->indent_level--;
                    }
                }

                indent(ctx);
                fprintf(ctx->file, "};\n");

                break;
            }
        default:
            {
                internal_error("I do not know how to define a %s\n", symbol_kind_name(symbol));
            }
    }

    symbol->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
}

static void declare_symbol(nodecl_codegen_visitor_t *ctx, scope_entry_t* symbol)
{
    // Do nothing if already defined or declared
    if (symbol->entity_specs.codegen_status == CODEGEN_STATUS_DEFINED
            || symbol->entity_specs.codegen_status == CODEGEN_STATUS_DECLARED)
        return;

    add_to_clear_list(symbol);
    symbol->entity_specs.codegen_status = CODEGEN_STATUS_DECLARED;

    if (symbol->do_not_print)
        return;

    switch (symbol->kind)
    {
        case SK_VARIABLE:
            {
                const char* decl_specifiers = "";
                const char* gcc_attributes = "";
                const char* declarator = "";
                const char* initializer = "";

                if (symbol->entity_specs.is_static)
                {
                    decl_specifiers = strappend(decl_specifiers, "static ");
                }
                if (symbol->entity_specs.is_extern)
                {
                    decl_specifiers = strappend(decl_specifiers, "extern ");
                }
                else 
                {
                    // If this not a member, or if it is, is nonstatic, this has already been defined
                    if (!symbol->entity_specs.is_member
                            || !symbol->entity_specs.is_static)
                    {
                        symbol->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                    }
                }
                if (symbol->entity_specs.is_thread)
                {
                    decl_specifiers = strappend(decl_specifiers, "__thread ");
                }
                if (symbol->entity_specs.is_mutable)
                {
                    decl_specifiers = strappend(decl_specifiers, "mutable ");
                }
                if (symbol->entity_specs.is_register)
                {
                    decl_specifiers = strappend(decl_specifiers, "register ");
                }

                declarator = print_decl_type_str(symbol->type_information, symbol->decl_context, 
                        symbol->symbol_name);

                indent(ctx);
                fprintf(ctx->file, "%s%s%s%s;\n",
                        decl_specifiers, gcc_attributes, declarator, initializer);
                break;
            }
        case SK_CLASS:
            {
                indent(ctx);
                C_LANGUAGE()
                {
                    // the symbol will be already called 'struct/union X' in C
                    fprintf(ctx->file, "%s;\n", symbol->symbol_name);
                }
                CXX_LANGUAGE()
                {
                    // TODO - Namespaces
                    const char *class_key = "";
                    switch (class_type_get_class_kind(symbol->type_information))
                    {
                        case CK_CLASS:
                            class_key = "class";
                            break;
                        case CK_STRUCT:
                            class_key = "struct";
                            break;
                        case CK_UNION:
                            class_key = "union";
                            break;
                        default:
                            internal_error("Invalid class kind", 0);
                    }

                    fprintf(ctx->file, "%s %s;\n", class_key, symbol->symbol_name);
                }
                break;
            }
        case SK_ENUM:
            {
                // Enums cannot be only declared but defined
                define_symbol(ctx, symbol);
                break;
            }
        case SK_TYPEDEF:
            {
                // Get a declaration to the aliased type
                codegen_type_of_symbol(ctx,
                        symbol->type_information,
                        /* needs_def */ 0);

                // A typedef cannot be only declared, it is always defined
                symbol->entity_specs.codegen_status = CODEGEN_STATUS_DEFINED;
                indent(ctx);
                fprintf(ctx->file, "typedef %s;\n", print_decl_type_str(symbol->type_information, 
                        symbol->decl_context, symbol->symbol_name));
                break;
            }
        default:
            {
                internal_error("Do not know how to declare a %s\n", symbol_kind_name(symbol));
                break;
            }
    }
}

static char is_local_symbol(scope_entry_t* entry)
{
    return entry != NULL
        && (entry->decl_context.current_scope->kind == BLOCK_SCOPE
                || entry->decl_context.current_scope->kind == FUNCTION_SCOPE);
}

static void declare_symbol_if_nonlocal(nodecl_codegen_visitor_t *ctx, scope_entry_t* symbol)
{
    if (!is_local_symbol(symbol))
    {
        declare_symbol(ctx, symbol);
    }
}

static void define_symbol_if_nonlocal(nodecl_codegen_visitor_t *ctx, scope_entry_t* symbol)
{
    if (!is_local_symbol(symbol))
    {
        define_symbol(ctx, symbol);
    }
}

UNUSED_PARAMETER static const char* codegen_expression(nodecl_codegen_visitor_t *ctx UNUSED_PARAMETER, AST expression, decl_context_t decl_context UNUSED_PARAMETER)
{
    return prettyprint_in_buffer(expression);
}

#if 0
static const char* codegen_expression_list(nodecl_codegen_visitor_t *ctx UNUSED_PARAMETER, AST expression, decl_context_t decl_context UNUSED_PARAMETER)
{
    return cxx_list_handler_in_buffer(expression);
}
#endif

static void not_implemented_yet(nodecl_external_visitor_t* visitor UNUSED_PARAMETER, nodecl_t node)
{
    fprintf(stderr, "WARNING -> Uninmplemented node! '%s'\n", ast_print_node_type(ASTType(nodecl_get_ast(node))));
}

static void codegen_function_code(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t name = nodecl_get_children(node, 0);
    nodecl_t statement_seq = nodecl_get_children(node, 1);
    nodecl_t internal_functions = nodecl_get_children(node, 2);

    if (!nodecl_is_null(internal_functions))
    {
        internal_error("C/C++ does not have internal functions", 0);
    }

    if (!nodecl_is_null(nodecl_get_children(statement_seq, 0)))
    {
        internal_error("C/C++ functions only have one statement", 0);
    }

    nodecl_t statement = nodecl_get_children(node, 1);

    scope_entry_t* symbol = nodecl_get_symbol(name);
    ERROR_CONDITION(symbol == NULL || symbol->kind != SK_FUNCTION, "Invalid symbol", 0);

    visitor->current_sym = symbol;

    codegen_type_of_symbol(visitor, function_type_get_return_type(symbol->type_information), /* needs_def */ 1);

    int i;
    int num_params = function_type_get_num_parameters(symbol->type_information);
    if (function_type_get_has_ellipsis(symbol->type_information))
        num_params--;

    for (i = 0; i < num_params; i++)
    {
        codegen_type_of_symbol(visitor, function_type_get_parameter_type_num(symbol->type_information, i), /* needs_def */ 1);
    }

    define_nonlocal_entities_in_trees(visitor, statement);

    const char* argument_names[symbol->entity_specs.num_related_symbols + 1];
    memset(argument_names, 0, sizeof(argument_names));

    for (i = 0; i < symbol->entity_specs.num_related_symbols; i++)
    {
        if (symbol->entity_specs.related_symbols[i] != NULL)
        {
            argument_names[i] = symbol->entity_specs.related_symbols[i]->symbol_name;
        }
    }

    const char* decl_spec_seq = "";
    if (symbol->entity_specs.is_static
            && !symbol->entity_specs.is_member)
    {
        decl_spec_seq = strappend(decl_spec_seq, "static ");
    }
    if (symbol->entity_specs.is_extern)
    {
        decl_spec_seq = strappend(decl_spec_seq, "extern ");
    }
    if (symbol->entity_specs.is_inline)
    {
        decl_spec_seq = strappend(decl_spec_seq, "inline ");
    }

    const char* gcc_attributes = "";
    for (i = 0; i < MCXX_MAX_GCC_ATTRIBUTES_PER_SYMBOL; i++)
    {
        if (symbol->entity_specs.gcc_attributes[i].attribute_name != NULL)
        {
            if (symbol->entity_specs.gcc_attributes[i].expression_list == NULL)
            {
                char c[256];
                snprintf(c, 255, "__attribute__((%s)) ", symbol->entity_specs.gcc_attributes[i].attribute_name);
                c[255] = '\0';

                gcc_attributes = strappend(gcc_attributes, c);
            }
            else
            {
                internal_error("Attribute Not yet implemented", 0);
                // char c[256];
                // snprintf(c, 255, "__attribute__((%s(%s))) ", 
                //         symbol->entity_specs.gcc_attributes[i].attribute_name,
                //         codegen_expression_list(ctx, symbol->entity_specs.gcc_attributes[i].expression_list, symbol->decl_context)
                //             );
                // c[255] = '\0';

                // gcc_attributes = strappend(gcc_attributes, c);
            }
        }
    }

    const char* declarator = get_declaration_string_internal(symbol->type_information,
            symbol->decl_context,
            symbol->symbol_name,
            /* initializer */ "",
            /* semicolon */ 0,
            /* num_parameter_names */ symbol->entity_specs.num_related_symbols,
            /* parameter_names */ argument_names,
            /* is_parameter */ 0);

    const char* exception_spec = "";
    CXX_LANGUAGE()
    {
        if (!symbol->entity_specs.any_exception)
        {
            exception_spec = " throw (";
            for (i = 0; i < symbol->entity_specs.num_exceptions; i++)
            {
                if (i != 0)
                {
                    exception_spec = strappend(exception_spec, ", ");
                }
                exception_spec = strappend(exception_spec, print_type_str(symbol->entity_specs.exceptions[i], symbol->decl_context));
            }
            exception_spec = strappend(exception_spec, " )");
        }
    }

    indent(visitor);
    fprintf(visitor->file, "%s%s%s%s\n", decl_spec_seq, gcc_attributes, declarator, exception_spec);

    NODECL_WALK(visitor, statement);
}

static void codegen_compound_statement(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    indent(visitor);
    fprintf(visitor->file, "{\n");
    visitor->indent_level++;
    nodecl_t statement_seq = nodecl_get_children(node, 0);

    scope_entry_t* scope_symbol = nodecl_get_symbol(node);
    ERROR_CONDITION(scope_symbol != NULL || scope_symbol->kind != SK_SCOPE, "Invalid scoping symbol", 0);

    define_local_entities_in_trees(visitor, statement_seq, scope_symbol->decl_context.current_scope);

    NODECL_WALK(visitor, statement_seq);

    visitor->indent_level--;
    indent(visitor);
    fprintf(visitor->file, "}\n");
}

static void codegen_top_level(nodecl_codegen_visitor_t* visitor, nodecl_t node)
{
    nodecl_t list = nodecl_get_children(node, 0);
    NODECL_WALK(visitor, list);
}

void c_cxx_codegen_translation_unit(FILE *f, AST a, scope_link_t* sl UNUSED_PARAMETER)
{
    nodecl_codegen_visitor_t codegen_visitor;
    nodecl_init_walker((nodecl_external_visitor_t*)&codegen_visitor, not_implemented_yet);

    codegen_visitor.file = f;
    codegen_visitor.indent_level = 0;
    codegen_visitor.current_sym = NULL;

    NODECL_VISITOR(&codegen_visitor)->visit_nodecl_top_level = NODECL_VISITOR_FUN(codegen_top_level);
    NODECL_VISITOR(&codegen_visitor)->visit_function_code = NODECL_VISITOR_FUN(codegen_function_code);
    NODECL_VISITOR(&codegen_visitor)->visit_compound_statement = NODECL_VISITOR_FUN(codegen_compound_statement);

    nodecl_t wrap = { a };
    nodecl_walk((nodecl_external_visitor_t*)&codegen_visitor, wrap);

    run_clear_list();
}

