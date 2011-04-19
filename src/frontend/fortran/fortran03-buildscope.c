#include "fortran03-buildscope.h"
#include "fortran03-scope.h"
#include "fortran03-exprtype.h"
#include "fortran03-prettyprint.h"
#include "fortran03-typeutils.h"
#include "fortran03-intrinsics.h"
#include "fortran03-modules.h"
#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-scopelink.h"
#include "cxx-utils.h"
#include "cxx-entrylist.h"
#include "cxx-typeutils.h"
#include "cxx-tltype.h"
#include "cxx-attrnames.h"
#include "cxx-exprtype.h"
#include "cxx-ambiguity.h"
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "red_black_tree.h"

static void unsupported_construct(AST a, const char* name);
static void unsupported_statement(AST a, const char* name);

static void null_dtor(const void* p UNUSED_PARAMETER) { }

void fortran_initialize_translation_unit_scope(translation_unit_t* translation_unit)
{
    decl_context_t decl_context;
    initialize_translation_unit_scope(translation_unit, &decl_context);

    translation_unit->module_cache = rb_tree_create((int (*)(const void*, const void*))strcasecmp, null_dtor, null_dtor);

    fortran_init_intrisics(decl_context);
}

static void build_scope_program_unit_seq(AST program_unit_seq, 
        decl_context_t decl_context);

void build_scope_fortran_translation_unit(translation_unit_t* translation_unit)
{
    AST a = translation_unit->parsed_tree;
    // Technically Fortran does not have a global scope but it is convenient to have one
    decl_context_t decl_context 
        = scope_link_get_global_decl_context(translation_unit->scope_link);

    scope_link_set(translation_unit->scope_link, a, decl_context);

    AST list = ASTSon0(a);
    if (list != NULL)
    {
        build_scope_program_unit_seq(list, decl_context);
    }
}

static void build_scope_program_unit_seq(AST program_unit_seq, 
        decl_context_t decl_context)
{
    AST it;
    for_each_element(program_unit_seq, it)
    {
        build_scope_program_unit(ASTSon1(it), 
                decl_context, 
                new_program_unit_context,
                NULL);
    }
}

static void add_unknown_symbol(decl_context_t decl_context, scope_entry_t* entry)
{
    P_LIST_ADD(decl_context.unknown_symbols, 
            decl_context.num_unknown_symbols, 
            entry);
}

static void clear_unknown_symbols(decl_context_t decl_context)
{
    const char* message = "";
    char unresolved_implicits = 0;
    int i;
    for (i = 0; i < decl_context.num_unknown_symbols; i++)
    {
        scope_entry_t* entry = decl_context.unknown_symbols[i];

        if ((entry->type_information == NULL
                    || basic_type_is_void(entry->type_information))
                    && !entry->entity_specs.is_builtin)
        {
            if (unresolved_implicits)
            {
                message = strappend(message, "\n");
            }

            char c[256] = { 0 };
            snprintf(c, 255, "%s:%d: error: symbol '%s' has no IMPLICIT info",
                    entry->file,
                    entry->line,
                    entry->symbol_name);
            c[255] = '\0';

            message = strappend(message, c);
            unresolved_implicits = 1;
        }
    }

    if (unresolved_implicits)
    {
        running_error("%s", message);
    }

    free(decl_context.unknown_symbols);
    decl_context.unknown_symbols = NULL;
    decl_context.num_unknown_symbols = 0;
}

static void update_unknown_symbols(decl_context_t decl_context)
{
    int i;
    for (i = 0; i < decl_context.num_unknown_symbols; i++)
    {
        scope_entry_t* entry = decl_context.unknown_symbols[i];

        ERROR_CONDITION(entry->type_information == NULL, "Invalid type for unknown entity '%s'\n", entry->symbol_name);

        if (entry->entity_specs.is_implicit_basic_type
                || basic_type_is_void(entry->type_information))
        {
            entry->type_information = update_basic_type_with_type(entry->type_information,
                    get_implicit_type_for_symbol(decl_context, entry->symbol_name));
        }
    }
}

// This function queries a symbol. If not found it uses implicit info to create
// one adding it to the set of unknown symbols of this context
//
// The difference of this function to query_name_with_locus is that
// query_name_with_locus always creates a SK_VARIABLE
static scope_entry_t* get_symbol_for_name_(decl_context_t decl_context, 
        AST locus, const char* name,
        char no_implicit)
{
    scope_entry_t* result = query_name_no_implicit_or_builtin(decl_context, name);
    if (result == NULL)
    {
        result = new_fortran_symbol(decl_context, name);
        if (!no_implicit)
        {
            result->type_information = get_implicit_type_for_symbol(decl_context, result->symbol_name);
        }
        else
        {
            result->type_information = get_void_type();
        }
        result->entity_specs.is_implicit_basic_type = 1;
        result->file = ASTFileName(locus);
        result->line = ASTLine(locus);

        if (decl_context.current_scope->related_entry != NULL)
        {
            P_LIST_ADD_ONCE( decl_context.current_scope->related_entry->entity_specs.related_symbols,
                    decl_context.current_scope->related_entry->entity_specs.num_related_symbols,
                    result);
        }

        add_unknown_symbol(decl_context, result);
    }

    return result;
}

static scope_entry_t* get_symbol_for_name(decl_context_t decl_context, AST locus, const char* name)
{
    return get_symbol_for_name_(decl_context, locus, name, /* no_implicit */ 0);
}

static scope_entry_t* get_symbol_for_name_untyped(decl_context_t decl_context, AST locus, const char* name)
{
    return get_symbol_for_name_(decl_context, locus, name, /* no_implicit */ 1);
}

static void build_scope_main_program_unit(AST program_unit, decl_context_t
        program_unit_context, scope_entry_t** program_unit_symbol);
static void build_scope_subroutine_program_unit(AST program_unit,
        decl_context_t program_unit_context, scope_entry_t**
        program_unit_symbol);
static void build_scope_function_program_unit(AST program_unit, decl_context_t
        program_unit_context, scope_entry_t** program_unit_symbol);
static void build_scope_module_program_unit(AST program_unit, decl_context_t
        program_unit_context, scope_entry_t** program_unit_symbol);
static void build_scope_block_data_program_unit(AST program_unit,
        decl_context_t program_unit_context, scope_entry_t**
        program_unit_symbol);

static void handle_opt_value_list(AST io_stmt, AST opt_value_list,
        decl_context_t decl_context);

void build_scope_program_unit(AST program_unit, 
        decl_context_t decl_context,
        decl_context_t (*new_context)(decl_context_t),
        scope_entry_t** program_unit_symbol)
{
    decl_context_t program_unit_context = new_context(decl_context);

    scope_entry_t* _program_unit_symbol = NULL;
    scope_link_set(CURRENT_COMPILED_FILE->scope_link, program_unit, program_unit_context);

    switch (ASTType(program_unit))
    {
        case AST_MAIN_PROGRAM_UNIT:
            {
                build_scope_main_program_unit(program_unit, program_unit_context, &_program_unit_symbol);
                break;
            }
        case AST_SUBROUTINE_PROGRAM_UNIT:
            {
                build_scope_subroutine_program_unit(program_unit, program_unit_context, &_program_unit_symbol);
                break;
            }
        case AST_FUNCTION_PROGRAM_UNIT:
            {
                build_scope_function_program_unit(program_unit, program_unit_context, &_program_unit_symbol);
                break;
            }
        case AST_MODULE_PROGRAM_UNIT :
            {
                build_scope_module_program_unit(program_unit, program_unit_context, &_program_unit_symbol);
                break;
            }
        case AST_BLOCK_DATA_PROGRAM_UNIT:
            {
                build_scope_block_data_program_unit(program_unit, program_unit_context, &_program_unit_symbol);
                break;
            }
        default:
            {
                internal_error("Unhandled node type '%s'\n", ast_print_node_type(ASTType(program_unit)));
            }
    }

    if (program_unit_symbol != NULL)
    {
        *program_unit_symbol = _program_unit_symbol;
    }

    if (decl_context.current_scope->related_entry != NULL
            && _program_unit_symbol != NULL)
    {
        scope_entry_t* sym = decl_context.current_scope->related_entry;
        P_LIST_ADD_ONCE(sym->entity_specs.related_symbols, 
                sym->entity_specs.num_related_symbols, 
                _program_unit_symbol);
    }
}

static scope_entry_t* new_procedure_symbol(decl_context_t decl_context, 
        AST name, AST prefix, AST suffix, AST dummy_arg_name_list,
        char is_function);
static void build_scope_program_body(AST program_body, 
        decl_context_t decl_context,
        char (*allowed_statement)(AST a, decl_context_t));
static void build_scope_program_body_module(AST program_body, 
        decl_context_t decl_context,
        char (*allowed_statement)(AST a, decl_context_t));

static char allow_all_statements(AST a UNUSED_PARAMETER, 
        decl_context_t decl_context UNUSED_PARAMETER)
{
    return 1;
}

static void build_scope_main_program_unit(AST program_unit, 
        decl_context_t program_unit_context, 
        scope_entry_t** program_unit_symbol)
{
    ERROR_CONDITION(program_unit_symbol == NULL, "Invalid parameter", 0)
    DEBUG_CODE()
    {
        fprintf(stderr, "==== [%s] Program unit: PROGRAM ===\n", ast_location(program_unit));
    }

    AST program_stmt = ASTSon0(program_unit);
    const char * program_name = "__MAIN__";
    if (program_stmt != NULL)
    {
        AST name = ASTSon0(program_stmt);
        program_name = ASTText(name);
    }
    scope_entry_t* program_sym = new_fortran_symbol(program_unit_context, program_name);
    program_sym->kind = SK_PROGRAM;
    program_sym->file = ASTFileName(program_unit);
    program_sym->line = ASTLine(program_unit);

    insert_alias(program_unit_context.current_scope->contained_in, program_sym,
            strappend("._", program_sym->symbol_name));

    program_sym->related_decl_context = program_unit_context;
    program_unit_context.current_scope->related_entry = program_sym;

    *program_unit_symbol = program_sym;

    AST program_body = ASTSon1(program_unit);
    if (program_body == NULL)
        return;

    build_scope_program_body(program_body, program_unit_context, allow_all_statements);

    ASTAttrSetValueType(program_unit, LANG_IS_FORTRAN_PROGRAM_UNIT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(program_unit, LANG_IS_FORTRAN_MAIN_PROGRAM, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(program_unit, LANG_FORTRAN_PROGRAM_UNIT_BODY, tl_type_t, tl_ast(program_body));
}

static void build_scope_function_program_unit(AST program_unit, 
        decl_context_t program_unit_context, 
        scope_entry_t** program_unit_symbol)
{
    ERROR_CONDITION(program_unit_symbol == NULL, "Invalid parameter", 0)
    DEBUG_CODE()
    {
        fprintf(stderr, "==== [%s] Program unit: FUNCTION ===\n", ast_location(program_unit));
    }
    AST function_stmt = ASTSon0(program_unit);

    AST prefix = ASTSon0(function_stmt);
    AST name = ASTSon1(function_stmt);
    AST function_prototype = ASTSon2(function_stmt);

    AST dummy_arg_name_list = ASTSon0(function_prototype);
    AST suffix = ASTSon1(function_prototype);

    scope_entry_t *new_entry = new_procedure_symbol(program_unit_context,
            name, prefix, suffix, 
            dummy_arg_name_list, /* is_function */ 1);

    insert_alias(program_unit_context.current_scope->contained_in, new_entry,
            strappend("._", new_entry->symbol_name));

    *program_unit_symbol = new_entry;

    new_entry->related_decl_context = program_unit_context;
    program_unit_context.current_scope->related_entry = new_entry;

    AST program_body = ASTSon1(program_unit);
    if (program_body == NULL)
        return;

    build_scope_program_body(program_body, program_unit_context, allow_all_statements);

    ASTAttrSetValueType(program_unit, LANG_IS_FORTRAN_PROGRAM_UNIT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(program_unit, LANG_IS_FORTRAN_FUNCTION, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(program_unit, LANG_FORTRAN_PROGRAM_UNIT_BODY, tl_type_t, tl_ast(program_body));
}

static void build_scope_subroutine_program_unit(AST program_unit, 
        decl_context_t program_unit_context, 
        scope_entry_t** program_unit_symbol)
{
    ERROR_CONDITION(program_unit_symbol == NULL, "Invalid parameter", 0)
    DEBUG_CODE()
    {
        fprintf(stderr, "==== [%s] Program unit: SUBROUTINE ===\n", ast_location(program_unit));
    }

    AST subroutine_stmt = ASTSon0(program_unit);

    AST prefix = ASTSon0(subroutine_stmt);
    AST name = ASTSon1(subroutine_stmt);

    AST function_prototype = ASTSon2(subroutine_stmt);

    AST dummy_arg_name_list = NULL;
    AST suffix = NULL;
    
    if (function_prototype != NULL)
    {
        dummy_arg_name_list = ASTSon0(function_prototype);
        suffix = ASTSon1(function_prototype);
    }

    scope_entry_t *new_entry = new_procedure_symbol(program_unit_context,
            name, prefix, suffix, 
            dummy_arg_name_list, /* is_function */ 0);

    *program_unit_symbol = new_entry;

    insert_alias(program_unit_context.current_scope->contained_in, new_entry,
            strappend("._", new_entry->symbol_name));

    new_entry->related_decl_context = program_unit_context;
    program_unit_context.current_scope->related_entry = new_entry;

    AST program_body = ASTSon1(program_unit);
    if (program_body == NULL)
        return;

    build_scope_program_body(program_body, program_unit_context, allow_all_statements);

    ASTAttrSetValueType(program_unit, LANG_IS_FORTRAN_PROGRAM_UNIT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(program_unit, LANG_IS_FORTRAN_SUBROUTINE, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(program_unit, LANG_FORTRAN_PROGRAM_UNIT_BODY, tl_type_t, tl_ast(program_body));
}

static void build_scope_module_program_unit(AST program_unit, 
        decl_context_t program_unit_context,
        scope_entry_t** program_unit_symbol)
{
    ERROR_CONDITION(program_unit_symbol == NULL, "Invalid parameter", 0)
    DEBUG_CODE()
    {
        fprintf(stderr, "==== [%s] Program unit: MODULE ===\n", ast_location(program_unit));
    }

    AST module_stmt = ASTSon0(program_unit);
    AST module_name = ASTSon0(module_stmt);

    scope_entry_t* new_entry = new_fortran_symbol(program_unit_context, ASTText(module_name));
    new_entry->kind = SK_MODULE;

    new_entry->related_decl_context = program_unit_context;
    new_entry->file = ASTFileName(module_stmt);
    new_entry->line = ASTLine(module_stmt);
    program_unit_context.current_scope->related_entry = new_entry;

    AST module_body = ASTSon1(program_unit);
    if (module_body == NULL)
        return;

    insert_alias(program_unit_context.current_scope->contained_in, new_entry,
            strappend("._", new_entry->symbol_name));

    *program_unit_symbol = new_entry;

    build_scope_program_body_module(module_body, program_unit_context, allow_all_statements);

    ASTAttrSetValueType(program_unit, LANG_IS_FORTRAN_PROGRAM_UNIT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(program_unit, LANG_IS_FORTRAN_MODULE, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(program_unit, LANG_FORTRAN_PROGRAM_UNIT_BODY, tl_type_t, tl_ast(module_body));

    // Keep the module in the file's module cache
    rb_tree_insert(CURRENT_COMPILED_FILE->module_cache, strtolower(new_entry->symbol_name), new_entry);

    // Store the module in a file
    dump_module_info(new_entry);
}

static void build_scope_block_data_program_unit(AST program_unit,
        decl_context_t program_unit_context UNUSED_PARAMETER,
        scope_entry_t** program_unit_symbol UNUSED_PARAMETER)
{
    // Do nothing with these
    DEBUG_CODE()
    {
        fprintf(stderr, "=== [%s] Program unit: BLOCK DATA ===\n", ast_location(program_unit));
    }

    ASTAttrSetValueType(program_unit, LANG_IS_FORTRAN_PROGRAM_UNIT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(program_unit, LANG_IS_FORTRAN_BLOCK_DATA, tl_type_t, tl_bool(1));
}

static type_t* gather_type_from_declaration_type_spec_(AST a, 
        decl_context_t decl_context);

static type_t* gather_type_from_declaration_type_spec(AST a, decl_context_t decl_context)
{
    return gather_type_from_declaration_type_spec_(a, decl_context);
}

static scope_entry_t* new_procedure_symbol(decl_context_t decl_context, 
        AST name, AST prefix, AST suffix, AST dummy_arg_name_list,
        char is_function)
{
    scope_entry_t* entry = NULL;

    entry = query_name_no_implicit_or_builtin(decl_context, ASTText(name));

    if (entry != NULL)
    {
        if (entry->defined
                || (!entry->entity_specs.is_parameter
                    && !entry->entity_specs.is_module_procedure))
        {
            running_error("%s: error: redeclaration of entity '%s'\n", 
                    ast_location(name), 
                    ASTText(name));
        }
    }

    if (entry == NULL)
    {
        entry = new_fortran_symbol(decl_context, ASTText(name));
    }

    entry->kind = SK_FUNCTION;
    entry->file = ASTFileName(name);
    entry->line = ASTLine(name);
    entry->entity_specs.is_implicit_basic_type = 1;
    entry->defined = 1;

    type_t* return_type = NULL;
    if (is_function)
    {
        return_type = get_implicit_type_for_symbol(decl_context, entry->symbol_name);
    }
    else
    {
        // Not an implicit basic type anymore
        entry->entity_specs.is_implicit_basic_type = 0;
    }

    if (prefix != NULL)
    {
        AST it;
        for_each_element(prefix, it)
        {
            AST prefix_spec = ASTSon1(it);
            ERROR_CONDITION(ASTType(prefix_spec) != AST_ATTR_SPEC, "Invalid tree", 0);

            const char* prefix_spec_str = ASTText(prefix_spec);

            if (strcasecmp(prefix_spec_str, "__declaration__") == 0)
            {
                if (!is_function)
                {
                    running_error("%s: error: declaration type-specifier is only valid for FUNCTION statement\n",
                            ast_location(prefix_spec));
                }

                AST declaration_type_spec = ASTSon0(prefix_spec);
                return_type = gather_type_from_declaration_type_spec(declaration_type_spec, decl_context);

                if (return_type != NULL)
                {
                    entry->entity_specs.is_implicit_basic_type = 0;
                    entry->type_information = return_type;
                }
            }
            else if (strcasecmp(prefix_spec_str, "elemental") == 0)
            {
                entry->entity_specs.is_elemental = 1;
            }
            else if (strcasecmp(prefix_spec_str, "pure") == 0)
            {
                entry->entity_specs.is_pure = 1;
            }
            else if (strcasecmp(prefix_spec_str, "recursive") == 0)
            {
                entry->entity_specs.is_recursive = 1;
            }
            else if ((strcasecmp(prefix_spec_str, "impure") == 0)
                    || (strcasecmp(prefix_spec_str, "module") == 0))
            {
                running_error("%s: error: unsupported specifier for procedures '%s'\n",
                        ast_location(prefix_spec),
                        fortran_prettyprint_in_buffer(prefix_spec));
            }
            else
            {
                internal_error("Invalid tree kind '%s' with spec '%s'\n", 
                        ast_print_node_type(ASTType(prefix_spec)),
                        ASTText(prefix_spec));
            }
        }
    }

    int num_dummy_arguments = 0;
    if (dummy_arg_name_list != NULL)
    {
        AST it;
        for_each_element(dummy_arg_name_list, it)
        {
            AST dummy_arg_name = ASTSon1(it);

            if (strcmp(ASTText(dummy_arg_name), "*") == 0)
            {
                fprintf(stderr, "%s: warning: deprecated alternate return in procedure declaration\n",
                        ast_location(dummy_arg_name));
                continue;
            }

            scope_entry_t* dummy_arg = get_symbol_for_name(decl_context, dummy_arg_name, ASTText(dummy_arg_name));

            // dummy_arg->kind = SK_VARIABLE;
            dummy_arg->type_information = 
                get_implicit_type_for_symbol(decl_context, ASTText(dummy_arg_name));
            dummy_arg->file = ASTFileName(dummy_arg_name);
            dummy_arg->line = ASTLine(dummy_arg_name);
            dummy_arg->entity_specs.is_parameter = 1;
            dummy_arg->entity_specs.parameter_position = num_dummy_arguments;

            P_LIST_ADD(entry->entity_specs.related_symbols,
                    entry->entity_specs.num_related_symbols,
                    dummy_arg);

            num_dummy_arguments++;
        }
    }

    if (suffix != NULL)
    {
        scope_entry_t* result_sym = NULL;
        // AST binding_spec = ASTSon0(suffix);
        AST result = ASTSon1(suffix);

        if (result != NULL)
        {
            if (!is_function)
            {
                running_error("%s: error: RESULT is only valid for FUNCTION statement\n",
                        ast_location(result));
            }

            result_sym = get_symbol_for_name(decl_context, result, ASTText(result));

            result_sym->kind = SK_VARIABLE;
            result_sym->file = ASTFileName(result);
            result_sym->line = ASTLine(result);
            result_sym->entity_specs.is_result = 1;

            result_sym->type_information = return_type;

            return_type = get_indirect_type(result_sym);

            P_LIST_ADD(entry->entity_specs.related_symbols,
                    entry->entity_specs.num_related_symbols,
                    result_sym);
        }
    }

    // Try to come up with a sensible type for this entity
    parameter_info_t parameter_info[num_dummy_arguments + 1];
    memset(parameter_info, 0, sizeof(parameter_info));

    int i;
    for (i = 0; i < num_dummy_arguments; i++)
    {
        parameter_info[i].type_info = get_indirect_type(entry->entity_specs.related_symbols[i]);
    }

    type_t* function_type = get_new_function_type(return_type, parameter_info, num_dummy_arguments);
    entry->type_information = function_type;

    return entry;
}

static void build_scope_internal_subprograms(AST internal_subprograms, 
        decl_context_t decl_context)
{
    if (internal_subprograms == NULL)
        return;

    AST it;
    for_each_element(internal_subprograms, it)
    {
        AST internal_subprogram = ASTSon1(it);
        scope_entry_t* subprogram_sym = NULL;
        build_scope_program_unit(internal_subprogram, 
                decl_context, 
                new_internal_program_unit_context,
                &subprogram_sym);

        // Add the symbol to the current scope
        scope_t* current_scope = decl_context.current_scope;
        insert_entry(current_scope, subprogram_sym);
    }
}

static char statement_is_executable(AST statement);
static void build_scope_ambiguity_statement(AST ambig_stmt, decl_context_t decl_context);

static void build_scope_program_body_module(AST program_body, decl_context_t decl_context,
        char (*allowed_statement)(AST, decl_context_t))
{
    AST program_unit_stmts = ASTSon0(program_body);
    AST internal_subprograms = ASTSon1(program_body);

    if (program_unit_stmts != NULL)
    {
        AST it;
        for_each_element(program_unit_stmts, it)
        {
            AST stmt = ASTSon1(it);

            // Exceptionally we run this one first otherwise it is not possible to
            // tell whether this is an executable or non-executable statement
            if (ASTType(stmt) == AST_AMBIGUITY)
            {
                build_scope_ambiguity_statement(stmt, decl_context);
            }

            if (!allowed_statement(stmt, decl_context))
            {
                fprintf(stderr, "%s: warning: this statement cannot be used in this context\n",
                        ast_location(stmt));
            }

            fortran_build_scope_statement(stmt, decl_context);
        }
    }

    build_scope_internal_subprograms(internal_subprograms, decl_context);

    clear_unknown_symbols(decl_context);

    ASTAttrSetValueType(program_body, LANG_FORTRAN_PROGRAM_UNIT_STATEMENTS, tl_type_t, tl_ast(program_unit_stmts));
    ASTAttrSetValueType(program_body, LANG_FORTRAN_PROGRAM_UNIT_INTERNAL_SUBPROGRAMS, tl_type_t, tl_ast(internal_subprograms));
}

static void build_scope_program_body(AST program_body, decl_context_t decl_context,
        char (*allowed_statement)(AST, decl_context_t))
{
    AST program_part = ASTSon0(program_body);
    AST program_unit_stmts = ASTSon0(program_part);

    AST internal_subprograms = ASTSon1(program_body);

    char seen_internal_subprograms = 0;
    char seen_executables = 0;

    if (program_unit_stmts != NULL)
    {
        AST it;
        for_each_element(program_unit_stmts, it)
        {
            AST stmt = ASTSon1(it);

            // Exceptionally we run this one first otherwise it is not possible to
            // tell whether this is an executable or non-executable statement
            if (ASTType(stmt) == AST_AMBIGUITY)
            {
                build_scope_ambiguity_statement(stmt, decl_context);
            }

            if (!allowed_statement(stmt, decl_context))
            {
                fprintf(stderr, "%s: warning: this statement cannot be used in this context\n",
                        ast_location(stmt));
            }

            if (!seen_executables 
                    && statement_is_executable(stmt))
            {
                // Make sure all symbols seen so far have basic type
                clear_unknown_symbols(decl_context);

                build_scope_internal_subprograms(internal_subprograms, decl_context);
                seen_executables = 1;
                seen_internal_subprograms = 1;
            }

            fortran_build_scope_statement(stmt, decl_context);
        }
    }

    if (!seen_executables)
    {
        clear_unknown_symbols(decl_context);
    }
    if (!seen_internal_subprograms)
    {
        build_scope_internal_subprograms(internal_subprograms, decl_context);
        seen_internal_subprograms = 1;
    }

    ASTAttrSetValueType(program_body, LANG_FORTRAN_PROGRAM_UNIT_STATEMENTS, tl_type_t, tl_ast(program_unit_stmts));
    ASTAttrSetValueType(program_body, LANG_FORTRAN_PROGRAM_UNIT_INTERNAL_SUBPROGRAMS, tl_type_t, tl_ast(internal_subprograms));
}

typedef void (*build_scope_statement_function_t)(AST statement, decl_context_t);

typedef
enum statement_kind_tag
{
    STMT_KIND_UNKNOWN = 0,
    STMT_KIND_EXECUTABLE = 1,
    STMT_KIND_NONEXECUTABLE = 2,
} statement_kind_t;

static statement_kind_t kind_nonexecutable_0(AST a UNUSED_PARAMETER)
{
    return STMT_KIND_NONEXECUTABLE;
}

static statement_kind_t kind_executable_0(AST a UNUSED_PARAMETER)
{
    return STMT_KIND_EXECUTABLE;
}

static statement_kind_t kind_of_son_1(AST a);

typedef struct build_scope_statement_handler_tag
{
    node_t ast_kind;
    build_scope_statement_function_t handler;
    statement_kind_t (*statement_kind)(AST);
} build_scope_statement_handler_t;

#define STATEMENT_HANDLER_TABLE \
 STATEMENT_HANDLER(AST_ACCESS_STATEMENT,             build_scope_access_stmt,           kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_ALLOCATABLE_STATEMENT,        build_scope_allocatable_stmt,      kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_ALLOCATE_STATEMENT,           build_scope_allocate_stmt,         kind_executable_0    ) \
 STATEMENT_HANDLER(AST_ALL_STOP_STATEMENT,           build_scope_allstop_stmt,          kind_executable_0    ) \
 STATEMENT_HANDLER(AST_ARITHMETIC_IF_STATEMENT,      build_scope_arithmetic_if_stmt,    kind_executable_0    ) \
 STATEMENT_HANDLER(AST_EXPRESSION_STATEMENT,         build_scope_expression_stmt,       kind_executable_0    ) \
 STATEMENT_HANDLER(AST_ASSOCIATE_CONSTRUCT,          build_scope_associate_construct,   kind_executable_0    ) \
 STATEMENT_HANDLER(AST_ASYNCHRONOUS_STATEMENT,       build_scope_asynchronous_stmt,     kind_executable_0    ) \
 STATEMENT_HANDLER(AST_IO_STATEMENT,                 build_io_stmt,                     kind_executable_0    ) \
 STATEMENT_HANDLER(AST_BIND_STATEMENT,               build_scope_bind_stmt,             kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_BLOCK_CONSTRUCT,              build_scope_block_construct,       kind_executable_0    ) \
 STATEMENT_HANDLER(AST_SWITCH_STATEMENT,             build_scope_case_construct,        kind_executable_0    ) \
 STATEMENT_HANDLER(AST_CASE_STATEMENT,               build_scope_case_statement,        kind_executable_0    ) \
 STATEMENT_HANDLER(AST_DEFAULT_STATEMENT,            build_scope_default_statement,     kind_executable_0    ) \
 STATEMENT_HANDLER(AST_CLOSE_STATEMENT,              build_scope_close_stmt,            kind_executable_0    ) \
 STATEMENT_HANDLER(AST_CODIMENSION_STATEMENT,        build_scope_codimension_stmt,      kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_COMMON_STATEMENT,             build_scope_common_stmt,           kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_COMPOUND_STATEMENT,           build_scope_compound_statement,    kind_executable_0    ) \
 STATEMENT_HANDLER(AST_COMPUTED_GOTO_STATEMENT,      build_scope_computed_goto_stmt,    kind_executable_0    ) \
 STATEMENT_HANDLER(AST_ASSIGNED_GOTO_STATEMENT,      build_scope_assigned_goto_stmt,    kind_executable_0    ) \
 STATEMENT_HANDLER(AST_LABEL_ASSIGN_STATEMENT,       build_scope_label_assign_stmt,     kind_executable_0    ) \
 STATEMENT_HANDLER(AST_LABELED_STATEMENT,            build_scope_labeled_stmt,          kind_of_son_1        ) \
 STATEMENT_HANDLER(AST_EMPTY_STATEMENT,              build_scope_continue_stmt,         kind_executable_0    ) \
 STATEMENT_HANDLER(AST_CRITICAL_CONSTRUCT,           build_scope_critical_construct,    kind_executable_0    ) \
 STATEMENT_HANDLER(AST_CONTINUE_STATEMENT,           build_scope_cycle_stmt,            kind_executable_0    ) \
 STATEMENT_HANDLER(AST_DATA_STATEMENT,               build_scope_data_stmt,             kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_DEALLOCATE_STATEMENT,         build_scope_deallocate_stmt,       kind_executable_0    ) \
 STATEMENT_HANDLER(AST_DERIVED_TYPE_DEF,             build_scope_derived_type_def,      kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_DIMENSION_STATEMENT,          build_scope_dimension_stmt,        kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_FOR_STATEMENT,                build_scope_do_construct,          kind_executable_0    ) \
 STATEMENT_HANDLER(AST_ENTRY_STATEMENT,              build_scope_entry_stmt,            kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_ENUM_DEF,                     build_scope_enum_def,              kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_EQUIVALENCE_STATEMENT,        build_scope_equivalence_stmt,      kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_BREAK_STATEMENT,              build_scope_exit_stmt,             kind_executable_0    ) \
 STATEMENT_HANDLER(AST_EXTERNAL_STATEMENT,           build_scope_external_stmt,         kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_FORALL_CONSTRUCT,             build_scope_forall_construct,      kind_executable_0    ) \
 STATEMENT_HANDLER(AST_FORALL_STATEMENT,             build_scope_forall_stmt,           kind_executable_0    ) \
 STATEMENT_HANDLER(AST_FORMAT_STATEMENT,             build_scope_format_stmt,           kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_GOTO_STATEMENT,               build_scope_goto_stmt,             kind_executable_0    ) \
 STATEMENT_HANDLER(AST_IF_ELSE_STATEMENT,            build_scope_if_construct,          kind_executable_0    ) \
 STATEMENT_HANDLER(AST_IMPLICIT_STATEMENT,           build_scope_implicit_stmt,         kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_IMPORT_STATEMENT,             build_scope_import_stmt,           kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_INTENT_STATEMENT,             build_scope_intent_stmt,           kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_INTERFACE_BLOCK,              build_scope_interface_block,       kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_INTRINSIC_STATEMENT,          build_scope_intrinsic_stmt,        kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_LOCK_STATEMENT,               build_scope_lock_stmt,             kind_executable_0    ) \
 STATEMENT_HANDLER(AST_NAMELIST_STATEMENT,           build_scope_namelist_stmt,         kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_NULLIFY_STATEMENT,            build_scope_nullify_stmt,          kind_executable_0    ) \
 STATEMENT_HANDLER(AST_OPEN_STATEMENT,               build_scope_open_stmt,             kind_executable_0    ) \
 STATEMENT_HANDLER(AST_OPTIONAL_STATEMENT,           build_scope_optional_stmt,         kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_PARAMETER_STATEMENT,          build_scope_parameter_stmt,        kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_POINTER_STATEMENT,            build_scope_pointer_stmt,          kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_PRINT_STATEMENT,              build_scope_print_stmt,            kind_executable_0    ) \
 STATEMENT_HANDLER(AST_PROCEDURE_DECL_STATEMENT,     build_scope_procedure_decl_stmt,   kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_PROTECTED_STATEMENT,          build_scope_protected_stmt,        kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_READ_STATEMENT,               build_scope_read_stmt,             kind_executable_0    ) \
 STATEMENT_HANDLER(AST_RETURN_STATEMENT,             build_scope_return_stmt,           kind_executable_0    ) \
 STATEMENT_HANDLER(AST_SAVE_STATEMENT,               build_scope_save_stmt,             kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_SELECT_TYPE_CONSTRUCT,        build_scope_select_type_construct, kind_executable_0    ) \
 STATEMENT_HANDLER(AST_STATEMENT_FUNCTION_STATEMENT, build_scope_stmt_function_stmt,    kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_STOP_STATEMENT,               build_scope_stop_stmt,             kind_executable_0    ) \
 STATEMENT_HANDLER(AST_PAUSE_STATEMENT,              build_scope_pause_stmt,            kind_executable_0    ) \
 STATEMENT_HANDLER(AST_SYNC_ALL_STATEMENT,           build_scope_sync_all_stmt,         kind_executable_0    ) \
 STATEMENT_HANDLER(AST_SYNC_IMAGES_STATEMENT,        build_scope_sync_images_stmt,      kind_executable_0    ) \
 STATEMENT_HANDLER(AST_SYNC_MEMORY_STATEMENT,        build_scope_sync_memory_stmt,      kind_executable_0    ) \
 STATEMENT_HANDLER(AST_TARGET_STATEMENT,             build_scope_target_stmt,           kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_DECLARATION_STATEMENT,        build_scope_type_declaration_stmt, kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_UNLOCK_STATEMENT,             build_scope_unlock_stmt,           kind_executable_0    ) \
 STATEMENT_HANDLER(AST_USE_STATEMENT,                build_scope_use_stmt,              kind_executable_0    ) \
 STATEMENT_HANDLER(AST_USE_ONLY_STATEMENT,           build_scope_use_stmt,              kind_executable_0    ) \
 STATEMENT_HANDLER(AST_VALUE_STATEMENT,              build_scope_value_stmt,            kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_VOLATILE_STATEMENT,           build_scope_volatile_stmt,         kind_nonexecutable_0 ) \
 STATEMENT_HANDLER(AST_WAIT_STATEMENT,               build_scope_wait_stmt,             kind_executable_0    ) \
 STATEMENT_HANDLER(AST_WHERE_CONSTRUCT,              build_scope_where_construct,       kind_executable_0    ) \
 STATEMENT_HANDLER(AST_WHERE_STATEMENT,              build_scope_where_stmt,            kind_executable_0    ) \
 STATEMENT_HANDLER(AST_WHILE_STATEMENT,              build_scope_while_stmt,            kind_executable_0    ) \
 STATEMENT_HANDLER(AST_WRITE_STATEMENT,              build_scope_write_stmt,            kind_executable_0    ) \
 STATEMENT_HANDLER(AST_PRAGMA_CUSTOM_CONSTRUCT,      build_scope_pragma_custom_ctr,     kind_executable_0  ) \
 STATEMENT_HANDLER(AST_PRAGMA_CUSTOM_DIRECTIVE,      build_scope_pragma_custom_dir,     kind_executable_0  ) \
 STATEMENT_HANDLER(AST_UNKNOWN_PRAGMA,               build_scope_continue_stmt,         kind_nonexecutable_0  ) \

// Prototypes
#define STATEMENT_HANDLER(_kind, _handler, _) \
    static void _handler(AST, decl_context_t);
STATEMENT_HANDLER_TABLE
#undef STATEMENT_HANDLER

// Table
#define STATEMENT_HANDLER(_kind, _handler, _stmt_kind) \
   { .ast_kind = _kind, .handler = _handler, .statement_kind = _stmt_kind },
static build_scope_statement_handler_t build_scope_statement_function[] = 
{
  STATEMENT_HANDLER_TABLE
};
#undef STATEMENT_HANDLER

static int build_scope_statement_function_init = 0;

static int build_scope_statement_function_compare(const void *a, const void *b)
{
    build_scope_statement_handler_t *pa = (build_scope_statement_handler_t*)a;
    build_scope_statement_handler_t *pb = (build_scope_statement_handler_t*)b;

    if (pa->ast_kind < pb->ast_kind)
        return -1;
    else if (pa->ast_kind > pb->ast_kind)
        return 1;
    else
        return 0;
}


static void init_statement_array(void)
{
    // Sort the array if needed
    if (!build_scope_statement_function_init)
    {
        // void qsort(void *base, size_t nmemb, size_t size,
        //    int(*compar)(const void *, const void *));
        qsort(build_scope_statement_function, 
                sizeof(build_scope_statement_function) / sizeof(build_scope_statement_function[0]),
                sizeof(build_scope_statement_function[0]),
                build_scope_statement_function_compare);
        build_scope_statement_function_init = 1;
    }
}

static char statement_get_kind(AST statement)
{
    init_statement_array();

    build_scope_statement_handler_t key = { .ast_kind = ASTType(statement) };
    build_scope_statement_handler_t *handler = NULL;

    handler = (build_scope_statement_handler_t*)bsearch(&key, build_scope_statement_function, 
            sizeof(build_scope_statement_function) / sizeof(build_scope_statement_function[0]),
            sizeof(build_scope_statement_function[0]),
            build_scope_statement_function_compare);

    ERROR_CONDITION(handler == NULL 
            || handler->statement_kind == NULL, "Invalid tree kind %s", ast_print_node_type(ASTType(statement)));

    return (handler->statement_kind)(statement);
}

static statement_kind_t kind_of_son_1(AST a)
{
    return statement_get_kind(ASTSon1(a));
}

static char statement_is_executable(AST statement)
{
    return statement_get_kind(statement) == STMT_KIND_EXECUTABLE;
}

#if 0
static char statement_is_nonexecutable(AST statement)
{
    return statement_get_kind(statement) == STMT_KIND_NONEXECUTABLE;
}
#endif

void fortran_build_scope_statement(AST statement, decl_context_t decl_context)
{
    init_statement_array();

    DEBUG_CODE()
    {
        fprintf(stderr, "=== [%s] Statement ===\n", ast_location(statement));
    }

    build_scope_statement_handler_t key = { .ast_kind = ASTType(statement) };
    build_scope_statement_handler_t *handler = NULL;

    handler = (build_scope_statement_handler_t*)bsearch(&key, build_scope_statement_function, 
            sizeof(build_scope_statement_function) / sizeof(build_scope_statement_function[0]),
            sizeof(build_scope_statement_function[0]),
            build_scope_statement_function_compare);
    if (handler == NULL 
            || handler->handler == NULL)
    {
        running_error("%s: sorry: unhandled statement %s\n", ast_location(statement), ast_print_node_type(ASTType(statement)));
    }
    else
    {
        (handler->handler)(statement, decl_context);
    }
}

const char* get_name_of_generic_spec(AST generic_spec)
{
    switch (ASTType(generic_spec))
    {
        case AST_SYMBOL:
            {
                return strtolower(ASTText(generic_spec));
            }
        case AST_OPERATOR_NAME:
            {
                return strtolower(strappend(".operator.", ASTText(generic_spec)));
            }
        case AST_IO_SPEC:
            {
                running_error("%s: sorry: io-specifiers for generic-specifiers not supported\n", 0);
            }
        default:
            {
                internal_error("%s: Invalid generic spec '%s'", 
                        ast_location(generic_spec), ast_print_node_type(ASTType(generic_spec)));
            }
    }
    return NULL;
}


static int compute_kind_specifier(AST kind_expr, decl_context_t decl_context)
{
    fortran_check_expression(kind_expr, decl_context);

    if (expression_is_constant(kind_expr))
    {
        return const_value_cast_to_4(expression_get_constant(kind_expr));
    }
    else
    {
        // We would issue a warning but since we are not implementing the
        // builtins, we just fallback to 4
        return 4;
    }
}

static type_t* choose_type_from_kind_table(AST expr, type_t** type_table, int num_types, int kind_size)
{
    type_t* result = NULL;
    if ((0 < kind_size)
            && (kind_size <= num_types))
    {
        result = type_table[kind_size];
    }

    if (result == NULL)
    {
        running_error("%s: error: KIND=%d not supported\n", ast_location(expr), kind_size);
    }

    return result;
}

#define MAX_INT_KIND 16
static char int_types_init = 0;
static type_t* int_types[MAX_INT_KIND + 1] = { 0 };
type_t* choose_int_type_from_kind(AST expr, int kind_size)
{
    if (!int_types_init)
    {
        int_types[type_get_size(get_signed_long_long_int_type())] = get_signed_long_long_int_type();
        int_types[type_get_size(get_signed_long_int_type())] = get_signed_long_int_type();
        int_types[type_get_size(get_signed_int_type())] = get_signed_int_type();
        int_types_init = 1;
    }
    return choose_type_from_kind_table(expr, int_types, MAX_INT_KIND, kind_size);
}
#undef MAX_INT_KIND

#define MAX_FLOAT_KIND 16
static char float_types_init = 0;
static type_t* float_types[MAX_FLOAT_KIND + 1] = { 0 };
type_t* choose_float_type_from_kind(AST expr, int kind_size)
{
    if (!float_types_init)
    {
        float_types[type_get_size(get_long_double_type())] = get_long_double_type();
        float_types[type_get_size(get_double_type())] = get_double_type();
        float_types[type_get_size(get_float_type())] = get_float_type();
        float_types_init = 1;
    }
    return choose_type_from_kind_table(expr, float_types, MAX_FLOAT_KIND, kind_size);
}
#undef MAX_FLOAT_KIND

#define MAX_LOGICAL_KIND 16
static char logical_types_init = 0;
static type_t* logical_types[MAX_LOGICAL_KIND + 1] = { 0 };
type_t* choose_logical_type_from_kind(AST expr, int kind_size)
{
    if (!logical_types_init)
    {
        logical_types[type_get_size(get_signed_long_long_int_type())] = get_bool_of_integer_type(get_signed_long_long_int_type());
        logical_types[type_get_size(get_signed_long_int_type())] = get_bool_of_integer_type(get_signed_long_int_type());
        logical_types[type_get_size(get_signed_int_type())] = get_bool_of_integer_type(get_signed_int_type());
        logical_types_init = 1;
    }
    return choose_type_from_kind_table(expr, logical_types, MAX_LOGICAL_KIND, kind_size);
}
#undef MAX_LOGICAL_KIND

static type_t* choose_type_from_kind(AST expr, decl_context_t decl_context, type_t* (*fun)(AST expr, int kind_size))
{
    int kind_size = compute_kind_specifier(expr, decl_context);
    return fun(expr, kind_size);
}

static type_t* get_derived_type_name(AST a, decl_context_t decl_context)
{
    ERROR_CONDITION(ASTType(a) != AST_DERIVED_TYPE_NAME, "Invalid tree '%s'\n", ast_print_node_type(ASTType(a)));

    AST name = ASTSon0(a);
    if (ASTSon1(a) != NULL)
    {
        running_error("%s: sorry: unsupported generic type-names", ast_location(ASTSon1(a)));
    }

    scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

    type_t* result = NULL;
    if (entry->kind == SK_CLASS)
    {
        result = get_user_defined_type(entry);
    }

    return result;
}

static type_t* gather_type_from_declaration_type_spec_(AST a, 
        decl_context_t decl_context)
{
    type_t* result = NULL;
    switch (ASTType(a))
    {
        case AST_INT_TYPE:
            {
                result = get_signed_int_type();
                if (ASTSon0(a) != NULL)
                {
                    result = choose_type_from_kind(ASTSon0(a), decl_context, choose_int_type_from_kind);
                }
                break;
            }
        case AST_FLOAT_TYPE:
            {
                result = get_float_type();
                if (ASTSon0(a) != NULL)
                {
                    result = choose_type_from_kind(ASTSon0(a), decl_context, choose_float_type_from_kind);
                }
                break;
            }
        case AST_DOUBLE_TYPE:
            {
                result = get_double_type();
                break;
            }
        case AST_COMPLEX_TYPE:
            {
                type_t* element_type = NULL; 
                if (ASTType(ASTSon0(a)) == AST_DECIMAL_LITERAL)
                {
                    element_type = choose_type_from_kind(ASTSon0(a), decl_context, choose_float_type_from_kind);
                }
                else
                {
                    element_type = gather_type_from_declaration_type_spec_(ASTSon0(a), decl_context);
                }

                result = get_complex_type(element_type);
                break;
            }
        case AST_CHARACTER_TYPE:
            {
                result = get_signed_char_type();
                AST char_selector = ASTSon0(a);
                AST len = NULL;
                AST kind = NULL;
                if (char_selector != NULL)
                {
                    len = ASTSon0(char_selector);
                    kind = ASTSon1(char_selector);
                }

                char is_undefined = 0;
                // Well, we cannot default to a kind of 4 because it'd be weird, so we simply ignore the kind
                if (kind != NULL)
                {
                    fprintf(stderr, "%s: warning: KIND of CHARACTER ignored, defaulting to 1\n",
                            ast_location(a));
                }
                if (len == NULL)
                {
                    len = ASTLeaf(AST_DECIMAL_LITERAL, ASTFileName(a), ASTLine(a), "1");
                }
                else if (ASTType(len) == AST_SYMBOL
                        && strcmp(ASTText(len), "*") == 0)
                {
                    is_undefined = 1;
                }
                if (!is_undefined)
                {
                    AST lower_bound = ASTLeaf(AST_DECIMAL_LITERAL, ASTFileName(len), ASTLine(len), "1");
                    result = get_array_type_bounds(result, lower_bound, len, decl_context);
                }
                else
                {
                    result = get_array_type(result, NULL, decl_context);
                }
                break;
            }
        case AST_BOOL_TYPE:
            {
                result = get_bool_type();
                if (ASTSon0(a) != NULL)
                {
                    result = choose_type_from_kind(ASTSon0(a), decl_context, choose_logical_type_from_kind);
                }
                break;
            }
        case AST_TYPE_NAME:
            {
                result = get_derived_type_name(ASTSon0(a), decl_context);
                if (result == NULL)
                {
                    running_error("%s: error: invalid type-specifier '%s'\n",
                            ast_location(a),
                            fortran_prettyprint_in_buffer(a));
                }
                break;
            }
        case AST_VECTOR_TYPE:
            {
                type_t* element_type = gather_type_from_declaration_type_spec_(ASTSon0(a), decl_context);
                // Generic vector
                result = get_vector_type(element_type, 0);
                break;
            }
        case AST_PIXEL_TYPE:
            {
                running_error("%s: sorry: PIXEL type-specifier not implemented\n",
                        ast_location(a));
                break;
            }
        case AST_CLASS_NAME:
            {
                running_error("%s: sorry: CLASS type-specifier not implemented\n",
                        ast_location(a));
                break;
            }
        default:
            {
                internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(a)));
            }
    }

    return result;
}



typedef
struct attr_spec_tag
{
    char is_allocatable;
    char is_asynchronous;

    char is_codimension;
    AST coarray_spec;

    char is_contiguous;

    char is_dimension;
    AST array_spec;

    char is_external;

    char is_intent;
    intent_kind_t intent_kind;

    char is_intrinsic;

    char is_optional;

    char is_constant;

    char is_pointer;
    
    char is_protected;

    char is_save;

    char is_target;

    char is_value;

    char is_volatile;

    char is_public;

    char is_private;

    char is_c_binding;
    const char* c_binding_name;
} attr_spec_t;

#define ATTR_SPEC_HANDLER_LIST \
ATTR_SPEC_HANDLER(allocatable) \
ATTR_SPEC_HANDLER(asynchronous) \
ATTR_SPEC_HANDLER(codimension) \
ATTR_SPEC_HANDLER(contiguous) \
ATTR_SPEC_HANDLER(dimension) \
ATTR_SPEC_HANDLER(external) \
ATTR_SPEC_HANDLER(intent) \
ATTR_SPEC_HANDLER(intrinsic) \
ATTR_SPEC_HANDLER(optional) \
ATTR_SPEC_HANDLER(parameter) \
ATTR_SPEC_HANDLER(pointer) \
ATTR_SPEC_HANDLER(protected) \
ATTR_SPEC_HANDLER(save) \
ATTR_SPEC_HANDLER(target) \
ATTR_SPEC_HANDLER(value) \
ATTR_SPEC_HANDLER(public) \
ATTR_SPEC_HANDLER(private) \
ATTR_SPEC_HANDLER(volatile) \
ATTR_SPEC_HANDLER(bind) 

// Forward declarations
#define ATTR_SPEC_HANDLER(_name) \
    static void attr_spec_##_name##_handler(AST attr_spec_item, decl_context_t decl_context, attr_spec_t* attr_spec);
ATTR_SPEC_HANDLER_LIST
#undef ATTR_SPEC_HANDLER

typedef struct attr_spec_handler_tag {
    const char* attr_name;
    void (*handler)(AST attr_spec_item, decl_context_t decl_context, attr_spec_t* attr_spec);
} attr_spec_handler_t;

// Table of handlers
attr_spec_handler_t attr_spec_handler_table[] = {
#define ATTR_SPEC_HANDLER(_name) \
    { #_name , attr_spec_##_name##_handler },
ATTR_SPEC_HANDLER_LIST
#undef ATTR_SPEC_HANDLER
};

static int attr_handler_cmp(const void *a, const void *b)
{
    return strcasecmp(((attr_spec_handler_t*)a)->attr_name,
            ((attr_spec_handler_t*)b)->attr_name);
}

static char attr_spec_handler_table_init = 0;
 
static void gather_attr_spec_item(AST attr_spec_item, decl_context_t decl_context, attr_spec_t *attr_spec)
{
    if (!attr_spec_handler_table_init)
    {
        qsort(attr_spec_handler_table, 
                sizeof(attr_spec_handler_table) / sizeof(attr_spec_handler_table[0]),
                sizeof(attr_spec_handler_table[0]),
                attr_handler_cmp);
        attr_spec_handler_table_init = 1;
    }

    switch (ASTType(attr_spec_item))
    {
        case AST_ATTR_SPEC:
            {
                attr_spec_handler_t key = { .attr_name = ASTText(attr_spec_item) };

                attr_spec_handler_t* handler = (attr_spec_handler_t*)bsearch(
                        &key,
                        attr_spec_handler_table, 
                        sizeof(attr_spec_handler_table) / sizeof(attr_spec_handler_table[0]),
                        sizeof(attr_spec_handler_table[0]),
                        attr_handler_cmp);

                if (handler == NULL 
                        || handler->handler == NULL)
                {
                    internal_error("Unhandled handler of '%s' (%s)\n", ASTText(attr_spec_item), ast_print_node_type(ASTType(attr_spec_item)));
                }

                (handler->handler)(attr_spec_item, decl_context, attr_spec);
                break;
            }
        default:
            {
                internal_error("Unhandled tree '%s'\n", ast_print_node_type(ASTType(attr_spec_item)));
            }
    }
}

static void attr_spec_allocatable_handler(AST a UNUSED_PARAMETER, 
        decl_context_t decl_context UNUSED_PARAMETER, attr_spec_t* attr_spec)
{
    attr_spec->is_allocatable = 1;
}

static void attr_spec_asynchronous_handler(AST a UNUSED_PARAMETER, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        attr_spec_t* attr_spec)
{
    attr_spec->is_asynchronous = 1;
}

static void attr_spec_codimension_handler(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        attr_spec_t* attr_spec)
{
    attr_spec->is_codimension = 1;
    attr_spec->coarray_spec = ASTSon0(a);
}

static void attr_spec_contiguous_handler(AST a UNUSED_PARAMETER, 
        decl_context_t decl_context UNUSED_PARAMETER,  
        attr_spec_t* attr_spec)
{
    attr_spec->is_contiguous = 1;
}

static void attr_spec_dimension_handler(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        attr_spec_t* attr_spec)
{
    attr_spec->is_dimension = 1;
    attr_spec->array_spec = ASTSon0(a);
}

static void attr_spec_external_handler(AST a UNUSED_PARAMETER, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        attr_spec_t* attr_spec)
{
    attr_spec->is_external = 1;
}

static void attr_spec_intent_handler(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        attr_spec_t* attr_spec)
{
    attr_spec->is_intent = 1;

    const char* intent_kind_str = ASTText(ASTSon0(a));
    if (strcasecmp(intent_kind_str, "in") == 0)
    {
        attr_spec->intent_kind = INTENT_IN;
    }
    else if (strcasecmp(intent_kind_str, "out") == 0)
    {
        attr_spec->intent_kind = INTENT_OUT;
    }
    else if (strcasecmp(intent_kind_str, "inout") == 0)
    {
        attr_spec->intent_kind = INTENT_INOUT;
    }
    else
    {
        internal_error("Invalid intent kind '%s'\n", intent_kind_str);
    }
}

static void attr_spec_intrinsic_handler(AST a UNUSED_PARAMETER, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        attr_spec_t* attr_spec)
{
    attr_spec->is_intrinsic = 1;
}

static void attr_spec_optional_handler(AST a UNUSED_PARAMETER,
        decl_context_t decl_context UNUSED_PARAMETER,
        attr_spec_t* attr_spec)
{
    attr_spec->is_optional = 1;
}

static void attr_spec_parameter_handler(AST a UNUSED_PARAMETER,
        decl_context_t decl_context UNUSED_PARAMETER,
        attr_spec_t* attr_spec)
{
    attr_spec->is_constant = 1;
}

static void attr_spec_pointer_handler(AST a UNUSED_PARAMETER,
        decl_context_t decl_context UNUSED_PARAMETER,
        attr_spec_t* attr_spec)
{
    attr_spec->is_pointer = 1;
}

static void attr_spec_protected_handler(AST a UNUSED_PARAMETER,
        decl_context_t decl_context UNUSED_PARAMETER,
        attr_spec_t* attr_spec)
{
    attr_spec->is_protected = 1;
}

static void attr_spec_save_handler(AST a UNUSED_PARAMETER,
        decl_context_t decl_context UNUSED_PARAMETER,
        attr_spec_t* attr_spec)
{
    attr_spec->is_save = 1;
}

static void attr_spec_target_handler(AST a UNUSED_PARAMETER,
        decl_context_t decl_context UNUSED_PARAMETER,
        attr_spec_t* attr_spec)
{
    attr_spec->is_target = 1;
}

static void attr_spec_value_handler(AST a UNUSED_PARAMETER,
        decl_context_t decl_context UNUSED_PARAMETER,
        attr_spec_t* attr_spec)
{
    attr_spec->is_value = 1;
}

static void attr_spec_volatile_handler(AST a UNUSED_PARAMETER,
        decl_context_t decl_context UNUSED_PARAMETER,
        attr_spec_t* attr_spec)
{
    attr_spec->is_volatile = 1;
}

static void attr_spec_public_handler(AST a UNUSED_PARAMETER,
        decl_context_t decl_context UNUSED_PARAMETER,
        attr_spec_t* attr_spec)
{
    attr_spec->is_public = 1;
}

static void attr_spec_private_handler(AST a UNUSED_PARAMETER,
        decl_context_t decl_context UNUSED_PARAMETER,
        attr_spec_t* attr_spec)
{
    attr_spec->is_public = 1;
}

static void attr_spec_bind_handler(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER,
        attr_spec_t* attr_spec)
{
    attr_spec->is_c_binding = 1;
    if (ASTSon0(a) != NULL)
    {
        attr_spec->c_binding_name = ASTText(ASTSon0(a));
    }
}

static void gather_attr_spec_list(AST attr_spec_list, decl_context_t decl_context, attr_spec_t *attr_spec)
{
    AST it;
    for_each_element(attr_spec_list, it)
    {
        AST attr_spec_item = ASTSon1(it);

        gather_attr_spec_item(attr_spec_item, decl_context, attr_spec);
    }
}

typedef
enum array_spec_kind_tag
{
    ARRAY_SPEC_KIND_NONE = 0,
    ARRAY_SPEC_KIND_EXPLICIT_SHAPE,
    ARRAY_SPEC_KIND_ASSUMED_SHAPE,
    ARRAY_SPEC_KIND_DEFERRED_SHAPE,
    ARRAY_SPEC_KIND_ASSUMED_SIZE,
    ARRAY_SPEC_KIND_IMPLIED_SHAPE,
    ARRAY_SPEC_KIND_ERROR,
} array_spec_kind_t;

static type_t* compute_type_from_array_spec(type_t* basic_type, 
        AST array_spec_list, decl_context_t decl_context,
        array_spec_kind_t* array_spec_kind)
{
    type_t* array_type = basic_type;
    // explicit-shape-spec   is   [lower:]upper
    // assumed-shape-spec    is   [lower]:
    // deferred-shape-spec   is   :
    // implied-shape-spec    is   [lower:]*

    // As a special case an assumed-size array-spec is [explicit-shape-spec ... ,] [lower:]*

    array_spec_kind_t kind = ARRAY_SPEC_KIND_NONE;

    // Note that we traverse the list backwards to create a type that matches
    // that of a C array
    AST it = NULL;
    for_each_element(array_spec_list, it)
    {
        AST array_spec_item = ASTSon1(it);
        AST lower_bound = ASTSon0(array_spec_item);
        AST upper_bound = ASTSon1(array_spec_item);

        if (lower_bound != NULL
                && (ASTType(lower_bound) != AST_SYMBOL
                    || (strcmp(ASTText(lower_bound), "*") != 0) ))
        {
            fortran_check_expression(lower_bound, decl_context);
        }

        if (upper_bound != NULL
                && (ASTType(upper_bound) != AST_SYMBOL
                    || (strcmp(ASTText(upper_bound), "*") != 0) ))
        {
            fortran_check_expression(upper_bound, decl_context);
        }

        if (lower_bound == NULL
                && upper_bound == NULL)
        {
            // (:)
            if (kind == ARRAY_SPEC_KIND_NONE)
            {
                kind = ARRAY_SPEC_KIND_DEFERRED_SHAPE;
            }
            else if (kind != ARRAY_SPEC_KIND_DEFERRED_SHAPE
                    && kind != ARRAY_SPEC_KIND_ASSUMED_SHAPE
                    && kind != ARRAY_SPEC_KIND_ERROR)
            {
                kind = ARRAY_SPEC_KIND_ERROR;
            }
        }
        else if (upper_bound != NULL
                && ASTType(upper_bound) == AST_SYMBOL
                && strcmp(ASTText(upper_bound), "*") == 0)
        {
            // (*)
            // (L:*)
            if (kind == ARRAY_SPEC_KIND_NONE)
            {
                kind = ARRAY_SPEC_KIND_IMPLIED_SHAPE;
            }
            else if (kind == ARRAY_SPEC_KIND_EXPLICIT_SHAPE)
            {
                kind = ARRAY_SPEC_KIND_ASSUMED_SIZE;
            }
            else if (kind != ARRAY_SPEC_KIND_ASSUMED_SIZE
                    && kind != ARRAY_SPEC_KIND_IMPLIED_SHAPE
                    && kind != ARRAY_SPEC_KIND_ERROR)
            {
                kind = ARRAY_SPEC_KIND_ERROR;
            }
        }
        else if (lower_bound != NULL
                && upper_bound == NULL)
        {
            // (L:)
            if (kind == ARRAY_SPEC_KIND_NONE
                    || kind == ARRAY_SPEC_KIND_DEFERRED_SHAPE)
            {
                kind = ARRAY_SPEC_KIND_ASSUMED_SHAPE;
            }
            else if (kind != ARRAY_SPEC_KIND_ASSUMED_SHAPE
                    && kind != ARRAY_SPEC_KIND_ERROR)
            {
                kind = ARRAY_SPEC_KIND_ERROR;
            }
        }
        else if (upper_bound != NULL)
        {
            // (U)
            // (:U)
            // (L:U)
            if (kind == ARRAY_SPEC_KIND_NONE)
            {
                kind = ARRAY_SPEC_KIND_EXPLICIT_SHAPE;
            }
            else if (kind != ARRAY_SPEC_KIND_EXPLICIT_SHAPE
                    && kind != ARRAY_SPEC_KIND_ERROR)
            {
                kind = ARRAY_SPEC_KIND_ERROR;
            }

            if (lower_bound == NULL)
            {
                lower_bound = ASTLeaf(AST_DECIMAL_LITERAL, ASTFileName(upper_bound), ASTLine(upper_bound), "1");
            }
        }

        array_type = get_array_type_bounds(array_type, lower_bound, upper_bound, decl_context);
    }

    if (array_spec_kind != NULL)
    {
        *array_spec_kind = kind;
    }

    return array_type;
}

static void build_scope_access_stmt(AST a, decl_context_t decl_context)
{
    attr_spec_t attr_spec;
    memset(&attr_spec, 0, sizeof(attr_spec));

    AST access_spec = ASTSon0(a);

    gather_attr_spec_item(access_spec, decl_context, &attr_spec);

    AST access_id_list = ASTSon1(a);
    if (access_id_list != NULL)
    {
        AST it;
        for_each_element(access_id_list, it)
        {
            AST access_id = ASTSon1(it);

            const char* name = get_name_of_generic_spec(access_id);

            scope_entry_t* sym = get_symbol_for_name(decl_context, access_id, name);

            if (sym->entity_specs.access != AS_UNKNOWN)
            {
                running_error("%s: access specifier already given for entity '%s'\n",
                        ast_location(access_id),
                        sym->symbol_name);
            }
            else
            {
                if (attr_spec.is_public)
                {
                    sym->entity_specs.access = AS_PUBLIC;
                }
                else if (attr_spec.is_private)
                {
                    sym->entity_specs.access = AS_PRIVATE;
                }
                else
                {
                    internal_error("code unreachable", 0);
                }
            }
        }
    }
    else
    {
        scope_entry_t* current_sym = decl_context.current_scope->related_entry;

        if (current_sym == NULL
                || current_sym->kind != SK_MODULE)
        {
            running_error("%s: error: wrong usage of access-statement\n",
                    ast_location(a));
        }

        if (current_sym->kind == SK_MODULE)
        {
            if (attr_spec.is_public)
            {
                current_sym->entity_specs.access = AS_PUBLIC;
            }
            else
            {
                current_sym->entity_specs.access = AS_PRIVATE;
            }
        }
    }
    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_dimension_decl(AST a, decl_context_t decl_context)
{
    // Do nothing for plain symbols
    if (ASTType(a) == AST_SYMBOL)
        return;

    ERROR_CONDITION(ASTType(a) != AST_DIMENSION_DECL,
            "Invalid tree", 0);

    AST name = ASTSon0(a);
    AST array_spec = ASTSon1(a);
    AST coarray_spec = ASTSon1(a);

    if (coarray_spec != NULL)
    {
       running_error("%s: sorry: coarrays not supported", ast_location(a));
    }

    scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

    if (entry->kind != SK_VARIABLE)
    {
        running_error("%s: error: invalid entity '%s' in dimension declaration\n", 
                ast_location(a),
                ASTText(name));
    }
    
    if (is_fortran_array_type(entry->type_information)
            || is_pointer_to_fortran_array_type(entry->type_information))
    {
        running_error("%s: error: entity '%s' already has a DIMENSION attribute\n",
                ast_location(a),
                entry->symbol_name);
    }

    type_t* array_type = compute_type_from_array_spec(entry->type_information, 
            array_spec,
            decl_context,
            /* array_spec_kind */ NULL);
    entry->type_information = array_type;

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_allocatable_stmt(AST a, decl_context_t decl_context)
{
    AST allocatable_decl_list = ASTSon0(a);
    AST it;

    for_each_element(allocatable_decl_list, it)
    {
        AST allocatable_decl = ASTSon1(it);
        build_dimension_decl(allocatable_decl, decl_context);

        AST name = NULL;
        if (ASTType(allocatable_decl) == AST_SYMBOL)
        {
            name = allocatable_decl;
        }
        else if (ASTType(allocatable_decl))
        {
            name = ASTSon0(allocatable_decl);
        }

        scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

        if (entry->kind != SK_VARIABLE)
        {
            running_error("%s: error: invalid entity '%s' in ALLOCATABLE clause\n", 
                    ast_location(name), 
                    ASTText(name));
        }

        if (!is_fortran_array_type(entry->type_information)
                && !is_pointer_to_fortran_array_type(entry->type_information))
        {
            running_error("%s: error: ALLOCATABLE attribute cannot be set to scalar entity '%s'\n",
                    ast_location(name),
                    ASTText(name));
        }

        if (entry->entity_specs.is_allocatable)
        {
            running_error("%s: error: attribute ALLOCATABLE was already set for entity '%s'\n",
                    ast_location(name),
                    ASTText(name));
        }
        entry->entity_specs.is_allocatable = 1;
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_allocate_stmt(AST a, decl_context_t decl_context)
{
    AST type_spec = ASTSon0(a);
    AST allocation_list = ASTSon1(a);
    AST alloc_opt_list = ASTSon2(a);

    if (type_spec != NULL)
    {
        running_error("%s: sorry: type-specifier not supported in ALLOCATE statement\n",
                ast_location(a));
    }

    AST it;
    for_each_element(allocation_list, it)
    {
        AST allocate_object = ASTSon1(it);

        // This one is here only for coarrays
        if (ASTType(allocate_object) == AST_DIMENSION_DECL)
        {
            running_error("%s: sorry: coarrays not supported\n", 
                    ast_location(allocate_object));
        }

        AST data_ref = allocate_object;
        fortran_check_expression(data_ref, decl_context);

        if (!is_error_type(expression_get_type(data_ref)))
        {
            scope_entry_t* entry = expression_get_symbol(data_ref);

            if (!entry->entity_specs.is_allocatable
                    && !is_pointer_type(entry->type_information))
            {
                running_error("%s: error: only ALLOCATABLE or POINTER can be used in an ALLOCATE statement\n", 
                        ast_location(a));
            }
        }
    }

    handle_opt_value_list(a, alloc_opt_list, decl_context);
}

static void unsupported_statement(AST a, const char* name)
{
    running_error("%s: sorry: %s statement not supported\n", 
            ast_location(a),
            name);
}

static void unsupported_construct(AST a, const char* name)
{
    running_error("%s: sorry: %s construct not supported\n", 
            ast_location(a),
            name);
}

static void build_scope_allstop_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "ALLSTOP");
}

static void build_scope_arithmetic_if_stmt(AST a, decl_context_t decl_context)
{
    AST numeric_expr = ASTSon0(a);
    fprintf(stderr, "%s: warning: deprecated arithmetic-if statement\n", 
            ast_location(a));
    fortran_check_expression(numeric_expr, decl_context);
}

static void build_scope_expression_stmt(AST a, decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "== [%s] Expression statement ==\n",
                ast_location(a));
    }
    AST expr = ASTSon0(a);
    if (!fortran_check_expression(expr, decl_context)
            && CURRENT_CONFIGURATION->strict_typecheck)
    {
        internal_error("Could not check expression '%s' at '%s'\n",
                fortran_prettyprint_in_buffer(ASTSon0(a)),
                ast_location(ASTSon0(a)));
    }

    if (!is_error_type(expression_get_type(expr)))
    {
        expression_set_type(a, expression_get_type(expr));
        expression_set_is_lvalue(a, expression_is_lvalue(a));
    }

    ASTAttrSetValueType(a, LANG_IS_EXPRESSION_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_IS_EXPRESSION_COMPONENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_IS_EXPRESSION_NEST, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_EXPRESSION_NESTED, tl_type_t, tl_ast(expr));
}

static void build_scope_associate_construct(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "ASSOCIATE");
}

static void build_scope_asynchronous_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "ASYNCHRONOUS");
}

static void build_scope_input_output_item_list(AST input_output_item_list, decl_context_t decl_context);

static void build_io_stmt(AST a, decl_context_t decl_context)
{
    AST io_spec_list = ASTSon0(a);
    handle_opt_value_list(a, io_spec_list, decl_context);

    AST input_output_item_list = ASTSon1(a);

    if (input_output_item_list != NULL)
    {
        build_scope_input_output_item_list(input_output_item_list, decl_context);
    }
}

static const char* get_common_name_str(const char* common_name)
{
    const char *common_name_str = ".common.unnamed";
    if (common_name != NULL)
    {
        common_name_str = strappend(".common.", strtolower(common_name));
    }
    return common_name_str;
}

static scope_entry_t* query_common_name(decl_context_t decl_context, const char* common_name)
{
    scope_entry_t* result = query_name_no_implicit_or_builtin(decl_context, 
            get_common_name_str(common_name));

    return result;
}

static void build_scope_bind_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    AST language_binding_spec = ASTSon0(a);
    AST bind_entity_list = ASTSon1(a);

    if (ASTType(language_binding_spec) != AST_BIND_C_SPEC)
    {
        running_error("%s: error: unsupported binding '%s'\n", 
                fortran_prettyprint_in_buffer(language_binding_spec));
    }

    AST it;
    for_each_element(bind_entity_list, it)
    {
        AST bind_entity = ASTSon1(it);

        scope_entry_t* entry = NULL;
        if (ASTType(bind_entity) == AST_COMMON_NAME)
        {
            entry = query_common_name(decl_context, ASTText(ASTSon0(bind_entity)));
        }
        else
        {
            entry = get_symbol_for_name(decl_context, bind_entity, ASTText(bind_entity));
        }

        entry->entity_specs.bind_c = 1;

        if (entry == NULL)
        {
            running_error("%s: error: unknown entity '%s' in BIND statement\n",
                    ast_location(bind_entity),
                    fortran_prettyprint_in_buffer(bind_entity));
        }
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_block_construct(AST a, decl_context_t decl_context)
{
    decl_context_t new_context = fortran_new_block_context(decl_context);

    AST block = ASTSon1(a);
    fortran_build_scope_statement(block, new_context);

    if (block != NULL)
    {
        scope_link_set(CURRENT_COMPILED_FILE->scope_link, block, new_context);
    }

    ASTAttrSetValueType(a, LANG_IS_COMPOUND_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_COMPOUND_STATEMENT_LIST, tl_type_t, tl_ast(ASTSon0(a)));
}

static void build_scope_case_construct(AST a, decl_context_t decl_context)
{
    AST expr = ASTSon0(a);
    AST statement = ASTSon1(a);

    fortran_check_expression(expr, decl_context);
    fortran_build_scope_statement(statement, decl_context);

    ASTAttrSetValueType(a, LANG_IS_SWITCH_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_SWITCH_STATEMENT_CONDITION, tl_type_t, tl_ast(expr));
    ASTAttrSetValueType(a, LANG_SWITCH_STATEMENT_BODY, tl_type_t, tl_ast(statement));
}

static void build_scope_case_statement(AST a, decl_context_t decl_context)
{
    AST case_selector = ASTSon0(a);
    AST statement = ASTSon1(a);

    AST case_value_range_list = ASTSon0(case_selector);
    AST it;
    for_each_element(case_value_range_list, it)
    {
        AST case_value_range = ASTSon1(it);

        if (ASTType(case_value_range) == AST_CASE_VALUE_RANGE)
        {
            AST lower_bound = ASTSon0(case_value_range);
            AST upper_bound = ASTSon1(case_value_range);

            if (lower_bound != NULL)
                fortran_check_expression(lower_bound, decl_context);
            if (upper_bound != NULL)
                fortran_check_expression(upper_bound, decl_context);
        }
        else
        {
            fortran_check_expression(case_value_range, decl_context);
        }
    }

    fortran_build_scope_statement(statement, decl_context);

    ASTAttrSetValueType(a, LANG_IS_CASE_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_CASE_EXPRESSION, tl_type_t, tl_ast(case_selector));
    ASTAttrSetValueType(a, LANG_CASE_STATEMENT_BODY, tl_type_t, tl_ast(statement));
}

static void build_scope_default_statement(AST a, decl_context_t decl_context)
{
    AST statement = ASTSon0(a);

    fortran_build_scope_statement(statement, decl_context);
}

static void build_scope_compound_statement(AST a, decl_context_t decl_context)
{
    AST it;

    AST list = ASTSon0(a);
    for_each_element(list, it)
    {
        AST statement = ASTSon1(it);

        fortran_build_scope_statement(statement, decl_context);
    }

    ASTAttrSetValueType(a, LANG_IS_COMPOUND_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_COMPOUND_STATEMENT_LIST, tl_type_t, tl_ast(list));
}

static void build_scope_close_stmt(AST a, decl_context_t decl_context)
{
    AST close_spec_list = ASTSon0(a);
    handle_opt_value_list(a, close_spec_list, decl_context);
}

static void build_scope_codimension_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "CODIMENSION");
}

static scope_entry_t* new_common(decl_context_t decl_context, const char* common_name)
{
    scope_entry_t* common_sym = new_fortran_symbol(decl_context, get_common_name_str(common_name));
    common_sym->kind = SK_COMMON;
    return common_sym;
}

static void build_scope_common_stmt(AST a, decl_context_t decl_context)
{
    AST common_block_item_list = ASTSon0(a);

    scope_entry_t* current_sym = decl_context.current_scope->related_entry;

    AST it;
    for_each_element(common_block_item_list, it)
    {
        AST common_block_item = ASTSon1(it);

        AST common_name = ASTSon0(common_block_item);
        AST common_block_object_list = ASTSon1(common_block_item);

        const char* common_name_str = ASTText(common_name);
        
        scope_entry_t* common_sym = query_name_no_implicit_or_builtin(decl_context, get_common_name_str(ASTText(common_name)));
        if (common_sym == NULL)
        {
            common_sym = new_common(decl_context, common_name_str);
            common_sym->file = ASTFileName(a);
            common_sym->line = ASTLine(a);

            if (current_sym != NULL)
            {
                P_LIST_ADD_ONCE(current_sym->entity_specs.related_symbols, 
                        current_sym->entity_specs.num_related_symbols,
                        common_sym);
            }
        }

        AST it2;
        for_each_element(common_block_object_list, it2)
        {
            AST common_block_object = ASTSon1(it2);

            AST name = NULL;
            AST array_spec = NULL;
            if (ASTType(common_block_object) == AST_SYMBOL)
            {
                name = common_block_object;
            }
            else if (ASTType(common_block_object) == AST_DIMENSION_DECL)
            {
                name = ASTSon0(common_block_object);
                array_spec = ASTSon1(common_block_object);
            }
            else
            {
                internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(common_block_object)));
            }

            scope_entry_t* sym = get_symbol_for_name(decl_context, name, ASTText(name));

            if (sym->entity_specs.is_in_common)
            {
                running_error("%s: error: entity '%s' is already in a COMMON\n", 
                        ast_location(name),
                        sym->symbol_name);
            }

            if (sym->kind == SK_UNDEFINED)
                sym->kind = SK_VARIABLE;

            sym->entity_specs.is_in_common = 1;
            sym->entity_specs.in_common = common_sym;

            if (array_spec != NULL)
            {
                if (is_fortran_array_type(sym->type_information)
                        || is_pointer_to_fortran_array_type(sym->type_information))
                {
                    running_error("%s: error: entity '%s' has already the DIMENSION attribute\n",
                            ast_location(a),
                            sym->symbol_name);
                }

                type_t* array_type = compute_type_from_array_spec(sym->type_information, 
                        array_spec,
                        decl_context,
                        /* array_spec_kind */ NULL);
                sym->type_information = array_type;
            }

            P_LIST_ADD(common_sym->entity_specs.related_symbols, 
                    common_sym->entity_specs.num_related_symbols,
                    sym);
        }
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_computed_goto_stmt(AST a, decl_context_t decl_context)
{
    fprintf(stderr, "%s: warning: deprecated computed-goto statement\n", 
            ast_location(a));
    fortran_check_expression(ASTSon1(a), decl_context);
}

static void build_scope_assigned_goto_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    fprintf(stderr, "%s: warning: deprecated assigned-goto statement\n", 
            ast_location(a));
}

static void build_scope_label_assign_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    fprintf(stderr, "%s: warning: deprecated label-assignment statement\n", 
            ast_location(a));
}

static scope_entry_t* query_label(AST label, 
        decl_context_t decl_context, 
        char is_definition)
{
    decl_context_t global_context = decl_context;
    global_context.current_scope = global_context.function_scope;

    const char* label_text = strappend(".label_", ASTText(label));
    scope_entry_list_t* entry_list = query_unqualified_name_str(global_context, label_text);

    scope_entry_t* new_label = NULL;
    if (entry_list == NULL)
    {
        new_label = new_symbol(decl_context, decl_context.function_scope, label_text);
        // Fix the symbol name (which for labels does not match the query name)
        new_label->symbol_name = ASTText(label);
        new_label->kind = SK_LABEL;
        new_label->line = ASTLine(label);
        new_label->file = ASTFileName(label);
        new_label->do_not_print = 1;
        new_label->defined = is_definition;
    }
    else
    {
        new_label = entry_list_head(entry_list);
        if (new_label->defined
                && is_definition)
        {
            fprintf(stderr, "%s: warning: label %s has already been defined in %s:%d\n",
                    ast_location(label),
                    new_label->symbol_name,
                    new_label->file, new_label->line);
        }
        else
        {
            new_label->defined = 1;
        }
    }

    entry_list_free(entry_list);
    return new_label;
}

static void build_scope_labeled_stmt(AST a, decl_context_t decl_context)
{
    AST label = ASTSon0(a);
    AST statement = ASTSon1(a);

    query_label(label, decl_context, /* is_definition */ 1);
    // Sign in the label
    fortran_build_scope_statement(statement, decl_context);

    ASTAttrSetValueType(a, LANG_IS_LABELED_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_STATEMENT_LABEL, tl_type_t, tl_ast(label));
    ASTAttrSetValueType(a, LANG_LABELED_STATEMENT, tl_type_t, tl_ast(statement));
}

static void build_scope_continue_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing for continue
    ASTAttrSetValueType(a, LANG_IS_EMPTY_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_critical_construct(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "CRITICAL");
}

static void build_scope_cycle_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing for cycle
    ASTAttrSetValueType(a, LANG_IS_CONTINUE_STATEMENT, tl_type_t, tl_bool(1));
}

static void generic_implied_do_handler(AST a, decl_context_t decl_context,
        void (*rec_handler)(AST, decl_context_t))
{
    AST implied_do_object_list = ASTSon0(a);
    AST implied_do_control = ASTSon1(a);

    decl_context_t new_context = fortran_new_block_context(decl_context);

    AST io_do_variable = ASTSon0(implied_do_control);
    AST lower_bound = ASTSon1(implied_do_control);
    AST upper_bound = ASTSon2(implied_do_control);
    AST stride = ASTSon3(implied_do_control);

    fortran_check_expression(lower_bound, decl_context);
    fortran_check_expression(upper_bound, decl_context);
    if (stride != NULL)
        fortran_check_expression(stride, decl_context);

    scope_entry_t* do_variable = new_fortran_symbol(new_context, ASTText(io_do_variable));

    do_variable->kind = SK_VARIABLE;
    do_variable->type_information 
        = get_const_qualified_type(get_signed_int_type());
    do_variable->file = ASTFileName(io_do_variable);
    do_variable->line = ASTLine(io_do_variable);

    scope_link_set(CURRENT_COMPILED_FILE->scope_link, implied_do_object_list, new_context);

    rec_handler(implied_do_object_list, new_context);
}

static void build_scope_data_stmt_object_list(AST data_stmt_object_list, decl_context_t decl_context)
{
    AST it2;
    for_each_element(data_stmt_object_list, it2)
    {
        AST data_stmt_object = ASTSon1(it2);
        if (ASTType(data_stmt_object) == AST_IMPLIED_DO)
        {
            generic_implied_do_handler(data_stmt_object, decl_context,
                    build_scope_data_stmt_object_list);
        }
        else
        {
            fortran_check_expression(data_stmt_object, decl_context);
        }
    }
}

static void build_scope_data_stmt(AST a, decl_context_t decl_context)
{
    AST data_stmt_set_list = ASTSon0(a);

    AST it;
    for_each_element(data_stmt_set_list, it)
    {
        AST data_stmt_set = ASTSon1(it);

        AST data_stmt_object_list = ASTSon0(data_stmt_set);
        build_scope_data_stmt_object_list(data_stmt_object_list, decl_context);

        AST data_stmt_value_list = ASTSon1(data_stmt_set);
        AST it2;
        for_each_element(data_stmt_value_list, it2)
        {
            AST data_stmt_value = ASTSon1(it2);
            if (ASTType(data_stmt_value) == AST_MULT_OP)
            {
                // Do not try to check at the same time because the mult does
                // not have to be valid
                fortran_check_expression(ASTSon0(data_stmt_value), decl_context);
                fortran_check_expression(ASTSon1(data_stmt_value), decl_context);
            }
            else
            {
                fortran_check_expression(data_stmt_value, decl_context);
            }
        }
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_deallocate_stmt(AST a, decl_context_t decl_context)
{
    AST allocate_object_list = ASTSon0(a);
    AST dealloc_opt_list = ASTSon1(a);

    AST it;
    for_each_element(allocate_object_list, it)
    {
        AST allocate_object = ASTSon1(it);

        if (ASTType(allocate_object) == AST_DIMENSION_DECL)
        {
            running_error("%s: sorry: coarrays not supported\n", 
                    ast_location(allocate_object));
        }

        AST data_ref = allocate_object;
        fortran_check_expression(data_ref, decl_context);

        if (!is_error_type(expression_get_type(data_ref)))
        {
            scope_entry_t* entry = expression_get_symbol(data_ref);

            if (!entry->entity_specs.is_allocatable
                    && !is_pointer_type(entry->type_information))
            {
                running_error("%s: error: only ALLOCATABLE or POINTER can be used in a DEALLOCATE statement\n", 
                        ast_location(a));
            }
        }
    }

    if (dealloc_opt_list != NULL)
    {
        handle_opt_value_list(a, dealloc_opt_list, decl_context);
    }
}

static void build_scope_derived_type_def(AST a, decl_context_t decl_context)
{
    AST derived_type_stmt = ASTSon0(a);
    AST derived_type_body = ASTSon1(a);

    AST type_attr_spec_list = ASTSon0(derived_type_stmt);
    AST name = ASTSon1(derived_type_stmt);
    AST type_param_name_list = ASTSon2(derived_type_stmt);

    if (type_param_name_list != NULL)
    {
        running_error("%s: sorry: derived types with type-parameters are not supported\n",
                ast_location(a));
    }

    char bind_c = 0;

    attr_spec_t attr_spec;
    memset(&attr_spec, 0, sizeof(attr_spec));

    AST it;
    if (type_attr_spec_list != NULL)
    {
        for_each_element(type_attr_spec_list, it)
        {
            AST type_attr_spec = ASTSon1(it);
            switch (ASTType(type_attr_spec))
            {
                case AST_ABSTRACT:
                    {
                        running_error("%s: error: ABSTRACT derived types are not supported\n", 
                                ast_location(type_attr_spec));
                        break;
                    }
                case AST_ATTR_SPEC:
                    {
                        gather_attr_spec_item(type_attr_spec, decl_context, &attr_spec);
                        break;
                    }
                case AST_BIND_C_SPEC:
                    {
                        bind_c = 1;
                        break;
                    }
                default:
                    {
                        internal_error("%s: unexpected tree\n",
                                ast_location(type_attr_spec));
                    }
            }
        }
    }

    scope_entry_t* class_name = new_fortran_symbol(decl_context, ASTText(name));
    class_name->kind = SK_CLASS;
    class_name->file = ASTFileName(name);
    class_name->line = ASTLine(name);
    class_name->type_information = get_new_class_type(decl_context, CK_STRUCT);

    class_name->entity_specs.bind_c = bind_c;

    if (attr_spec.is_public)
    {
        class_name->entity_specs.access = AS_PUBLIC;
    }
    else if (attr_spec.is_private)
    {
        class_name->entity_specs.access = AS_PRIVATE;
    }

    // Derived type body
    AST type_param_def_stmt_seq = ASTSon0(derived_type_body);
    AST private_or_sequence_seq = ASTSon1(derived_type_body);
    AST component_part = ASTSon2(derived_type_body);
    AST type_bound_procedure_part = ASTSon3(derived_type_body);

    if (type_param_def_stmt_seq != NULL)
    {
        running_error("%s: sorry: type-parameter definitions are not supported\n",
                ast_location(type_param_def_stmt_seq));
    }

    char is_sequence = 0;
    char fields_are_private = 0;
    if (private_or_sequence_seq != NULL)
    {
        for_each_element(private_or_sequence_seq, it)
        {
            AST private_or_sequence = ASTSon1(it);

            if (ASTType(private_or_sequence) == AST_SEQUENCE_STATEMENT)
            {
                if (is_sequence)
                {
                    running_error("%s: error: SEQUENCE statement specified twice", 
                            ast_location(private_or_sequence));
                }
                is_sequence = 1;
            }
            else if (ASTType(private_or_sequence) == AST_ACCESS_STATEMENT)
            {
                if (fields_are_private)
                {
                    running_error("%s: error: PRIVATE statement specified twice", 
                            ast_location(private_or_sequence));
                }
                // This can only be a private_stmt, no need to check it here
                fields_are_private = 1;
            }
            else
            {
                internal_error("%s: Unexpected statement '%s'\n", 
                        ast_location(private_or_sequence),
                        ast_print_node_type(ASTType(private_or_sequence)));
            }
        }
    }

    if (type_bound_procedure_part != NULL)
    {
        running_error("%s: sorry: type-bound procedures are not supported\n",
                ast_location(type_bound_procedure_part));
    }

    decl_context_t inner_decl_context = new_class_context(class_name->decl_context, class_name);
    class_type_set_inner_context(class_name->type_information, inner_decl_context);

    if (component_part != NULL)
    {
        scope_link_set(CURRENT_COMPILED_FILE->scope_link, component_part, inner_decl_context);

        for_each_element(component_part, it)
        {
            AST component_def_stmt = ASTSon1(it);

            if (ASTType(component_def_stmt) == AST_PROC_COMPONENT_DEF_STATEMENT)
            {
                running_error("%s: sorry: unsupported procedure components in derived type definition\n",
                        ast_location(component_def_stmt));
            }
            ERROR_CONDITION(ASTType(component_def_stmt) != AST_DATA_COMPONENT_DEF_STATEMENT, 
                    "Invalid tree", 0);

            AST declaration_type_spec = ASTSon0(component_def_stmt);
            AST component_attr_spec_list = ASTSon1(component_def_stmt);
            AST component_decl_list = ASTSon2(component_def_stmt);

            type_t* basic_type = gather_type_from_declaration_type_spec(declaration_type_spec, decl_context);

            memset(&attr_spec, 0, sizeof(attr_spec));

            if (component_attr_spec_list != NULL)
            {
                gather_attr_spec_list(component_attr_spec_list, decl_context, &attr_spec);
            }

            AST it2;
            for_each_element(component_decl_list, it2)
            {
                attr_spec_t current_attr_spec = attr_spec;
                AST declaration = ASTSon1(it2);

                AST component_name = ASTSon0(declaration);
                AST entity_decl_specs = ASTSon1(declaration);

                // TODO: We should check that the symbol has not been redefined 
                scope_entry_t* entry = new_fortran_symbol(inner_decl_context, ASTText(component_name));

                entry->kind = SK_VARIABLE;

                entry->file = ASTFileName(declaration);
                entry->line = ASTLine(declaration);

                entry->type_information = basic_type;
                entry->entity_specs.is_implicit_basic_type = 0;

                entry->defined = 1;

                AST char_length = NULL;
                AST initialization = NULL;
                if (entity_decl_specs != NULL)
                {
                    AST array_spec = ASTSon0(entity_decl_specs);
                    AST coarray_spec = ASTSon1(entity_decl_specs);
                    char_length = ASTSon2(entity_decl_specs);
                    initialization = ASTSon3(entity_decl_specs);

                    if (array_spec != NULL)
                    {
                        if (current_attr_spec.is_dimension)
                        {
                            running_error("%s: error: DIMENSION attribute specified twice\n", ast_location(declaration));
                        }
                        current_attr_spec.is_dimension = 1;
                        current_attr_spec.array_spec = array_spec;
                    }

                    if (coarray_spec != NULL)
                    {
                        if (current_attr_spec.is_codimension)
                        {
                            running_error("%s: error: CODIMENSION attribute specified twice\n", ast_location(declaration));
                        }
                        current_attr_spec.is_codimension = 1;
                        current_attr_spec.coarray_spec = coarray_spec;
                    }

                    if (char_length != NULL)
                    {
                        if (!is_fortran_character_type(entry->type_information))
                        {
                            running_error("%s: error: char-length specified but type is not CHARACTER\n", ast_location(declaration));
                        }

                        if (ASTType(char_length) != AST_SYMBOL
                                || strcmp(ASTText(char_length), "*") != 0)
                        {
                            fortran_check_expression(char_length, decl_context);
                            AST lower_bound = ASTLeaf(AST_DECIMAL_LITERAL, ASTFileName(char_length), ASTLine(char_length), "1");
                            entry->type_information = get_array_type_bounds(
                                    array_type_get_element_type(entry->type_information), 
                                    lower_bound, char_length, decl_context);
                        }
                        else
                        {
                            entry->type_information = get_array_type(
                                    array_type_get_element_type(entry->type_information), 
                                    NULL, decl_context);
                        }
                    }

                    // Stop the madness here
                    if (current_attr_spec.is_codimension)
                    {
                        running_error("%s: sorry: coarrays are not supported\n", ast_location(declaration));
                    }

                    if (current_attr_spec.is_dimension)
                    {
                        type_t* array_type = compute_type_from_array_spec(entry->type_information, 
                                current_attr_spec.array_spec,
                                decl_context,
                                /* array_spec_kind */ NULL);
                        entry->type_information = array_type;
                    }
                }

                if (fields_are_private
                        && entry->entity_specs.access == AS_UNKNOWN)
                {
                    entry->entity_specs.access = AS_PRIVATE;
                }

                class_type_add_nonstatic_data_member(class_name->type_information, entry);
            }
        }
    }

    if (decl_context.current_scope->related_entry != NULL)
    {
        scope_entry_t* entry = decl_context.current_scope->related_entry;

        P_LIST_ADD_ONCE(entry->entity_specs.related_symbols, 
                entry->entity_specs.num_related_symbols,
                class_name);
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_dimension_stmt(AST a, decl_context_t decl_context)
{
    AST array_name_dim_spec_list = ASTSon0(a);
    AST it;

    for_each_element(array_name_dim_spec_list, it)
    {
        AST dimension_decl = ASTSon1(it);
        AST name = ASTSon0(dimension_decl);

        scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

        if (is_fortran_array_type(entry->type_information)
                || is_pointer_to_fortran_array_type(entry->type_information))
        {
            running_error("%s: error: entity '%s' already has a DIMENSION attribute\n",
                    ast_location(name),
                    ASTText(name));
        }

        char is_pointer = is_pointer_type(entry->type_information);

        if (is_pointer_type(entry->type_information))
        {
            entry->type_information = pointer_type_get_pointee_type(entry->type_information);
        }

        AST array_spec = ASTSon1(dimension_decl);
        type_t* array_type = compute_type_from_array_spec(entry->type_information, 
                array_spec,
                decl_context,
                /* array_spec_kind */ NULL);

        if (entry->kind == SK_UNDEFINED)
            entry->kind = SK_VARIABLE;

        entry->type_information = array_type;

        if (is_pointer)
        {
            entry->type_information = get_pointer_type(entry->type_information);
        }
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_do_construct(AST a, decl_context_t decl_context)
{
    AST loop_control = ASTSon0(a);
    AST block = ASTSon1(a);
    
    AST assig = ASTSon0(loop_control);
    AST upper = ASTSon1(loop_control);
    AST stride = ASTSon2(loop_control);

    if (assig != NULL)
    {
        fortran_check_expression(assig, decl_context);

        AST do_loop_var = ASTSon0(assig);
        scope_entry_t* sym = expression_get_symbol(do_loop_var);
        if (sym != NULL
                && !is_integer_type(sym->type_information))
        {
            fprintf(stderr, "%s: warning: loop variable '%s' should be of integer type\n",
                    ast_location(a),
                    fortran_prettyprint_in_buffer(do_loop_var));
        }
    }
    if (upper != NULL)
        fortran_check_expression(upper, decl_context);
    if (stride != NULL)
        fortran_check_expression(stride, decl_context);

    fortran_build_scope_statement(block, decl_context);

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_DO_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_FORTRAN_DO_INIT, tl_type_t, tl_ast(assig));
    ASTAttrSetValueType(a, LANG_FORTRAN_DO_UPPER, tl_type_t, tl_ast(upper));
    ASTAttrSetValueType(a, LANG_FORTRAN_DO_STRIDE, tl_type_t, tl_ast(stride));
}

static void build_scope_entry_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    // AST name = ASTSon0(a);
    // AST dummy_arg_list = ASTSon1(a);
    // AST suffix = ASTSon2(a);

    // scope_entry_t* entry = ASTText(name);
    unsupported_statement(a, "ENTRY");
}

static void build_scope_enum_def(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_construct(a, "ENUM");
}

static void build_scope_equivalence_stmt(AST a, decl_context_t decl_context)
{
    AST equivalence_set_list = ASTSon0(a);

    AST it;
    for_each_element(equivalence_set_list, it)
    {
        AST equivalence_set = ASTSon1(it);

        AST equivalence_object = ASTSon0(equivalence_set);
        AST equivalence_object_list = ASTSon1(equivalence_set);

        fortran_check_expression(equivalence_object, decl_context);

        AST it2;
        for_each_element(equivalence_object_list, it2)
        {
            AST equiv_obj = ASTSon1(it2);
            fortran_check_expression(equiv_obj, decl_context);
        }
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_exit_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing for exit
    ASTAttrSetValueType(a, LANG_IS_BREAK_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_external_stmt(AST a, decl_context_t decl_context)
{
    AST name_list = ASTSon0(a);
    AST it;

    for_each_element(name_list, it)
    {
        AST name = ASTSon1(it);

        scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));
        entry->kind = SK_FUNCTION;

        if (!entry->entity_specs.is_extern)
        {
            if (is_void_type(entry->type_information))
            {
                // We do not know it, set a type like one of a PROCEDURE
                entry->type_information = get_nonproto_function_type(NULL, 0);
            }
            else
            {
                entry->type_information = get_nonproto_function_type(entry->type_information, 0);
            }
            // States it is extern
            entry->entity_specs.is_extern = 1;
        }
        else
        {
            running_error("%s: error: entity '%s' already has EXTERNAL attribute\n",
                    ast_location(name),
                    entry->symbol_name);
        }
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_forall_construct(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_construct(a, "FORALL");
}

static void build_scope_forall_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "FORALL");
}

static void build_scope_format_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing in FORMAT
    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_goto_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing for GOTO at the moment
}

static void build_scope_if_construct(AST a, decl_context_t decl_context)
{
    AST logical_expr = ASTSon0(a);
    AST then_statement = ASTSon1(a);
    AST else_statement = ASTSon2(a);

    fortran_check_expression(logical_expr, decl_context);
    fortran_build_scope_statement(then_statement, decl_context);
    if (else_statement != NULL)
    {
        fortran_build_scope_statement(else_statement, decl_context);
    }

    ASTAttrSetValueType(a, LANG_IS_IF_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_IF_STATEMENT_CONDITION, tl_type_t, tl_ast(logical_expr));
    ASTAttrSetValueType(a, LANG_IF_STATEMENT_THEN_BODY, tl_type_t, tl_ast(then_statement));
    ASTAttrSetValueType(a, LANG_IF_STATEMENT_ELSE_BODY, tl_type_t, tl_ast(else_statement));
}

static void build_scope_implicit_stmt(AST a, decl_context_t decl_context)
{
    AST implicit_spec_list = ASTSon0(a);
    if (implicit_spec_list == NULL)
    {
        if (implicit_has_been_set(decl_context))
        {
            if (is_implicit_none(decl_context))
            {
                running_error("%s: error: IMPLICIT NONE specified twice\n",
                        ast_location(a));
            }
            else 
            {
                running_error("%s: error: IMPLICIT NONE after IMPLICIT\n",
                        ast_location(a));
            }
        }
        set_implicit_none(decl_context);
    }
    else
    {
        if (implicit_has_been_set(decl_context)
                && is_implicit_none(decl_context))
        {
            running_error("%s: error: IMPLICIT after IMPLICIT NONE\n",
                    ast_location(a));
        }

        AST it;
        for_each_element(implicit_spec_list, it)
        {
            AST implicit_spec = ASTSon1(it);

            AST declaration_type_spec = ASTSon0(implicit_spec);
            AST letter_spec_list = ASTSon1(implicit_spec);

            type_t* basic_type = gather_type_from_declaration_type_spec(declaration_type_spec, decl_context);

            if (basic_type == NULL)
            {
                running_error("%s: error: invalid type specifier '%s' in IMPLICIT statement\n",
                        ast_location(declaration_type_spec),
                        fortran_prettyprint_in_buffer(declaration_type_spec));
            }

            AST it2;
            for_each_element(letter_spec_list, it2)
            {
                AST letter_spec = ASTSon1(it2);

                AST letter0 = ASTSon0(letter_spec);
                AST letter1 = ASTSon1(letter_spec);

                const char* letter0_str = ASTText(letter0);
                const char* letter1_str = NULL;

                if (letter1 != NULL)
                {
                    letter1_str = ASTText(letter1);
                }

                if (strlen(letter0_str) != 1
                        || !(('a' <= tolower(letter0_str[0]))
                            && (tolower(letter0_str[0]) <= 'z'))
                        || (letter1_str != NULL 
                            && (strlen(letter1_str) != 1
                                || !(('a' <= tolower(letter1_str[0]))
                                    && (tolower(letter1_str[0]) <= 'z')))))
                {
                    running_error("%s: error: invalid IMPLICIT letter specifier '%s'\n", 
                            ast_location(letter_spec),
                            fortran_prettyprint_in_buffer(letter_spec));
                }

                if (letter1_str == NULL)
                    letter1_str = letter0_str;

                set_implicit_info(decl_context, letter0_str[0], letter1_str[0], basic_type);
            }
        }
    }
    update_unknown_symbols(decl_context);

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_import_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "IMPORT");
}

static void build_scope_intent_stmt(AST a, decl_context_t decl_context)
{
    AST intent_spec = ASTSon0(a);
    AST dummy_arg_name_list = ASTSon1(a);

    AST it;
    for_each_element(dummy_arg_name_list, it)
    {
        AST dummy_arg = ASTSon1(it);

        scope_entry_t* entry = get_symbol_for_name(decl_context, dummy_arg, ASTText(dummy_arg));

        if (!entry->entity_specs.is_parameter)
        {
            running_error("%s: error: entity '%s' is not a dummy argument\n",
                    ast_location(dummy_arg),
                    fortran_prettyprint_in_buffer(dummy_arg));
        }

        if (entry->entity_specs.intent_kind != INTENT_INVALID)
        {
            running_error("%s: error: entity '%s' already has an INTENT attribute\n",
                    ast_location(dummy_arg),
                    fortran_prettyprint_in_buffer(dummy_arg));
        }

        attr_spec_t attr_spec;
        memset(&attr_spec, 0, sizeof(attr_spec));

        gather_attr_spec_item(intent_spec, decl_context, &attr_spec);

        entry->entity_specs.intent_kind = attr_spec.intent_kind;
    }
    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_interface_block(AST a, decl_context_t decl_context)
{
    AST interface_stmt = ASTSon0(a);
    AST interface_specification_seq = ASTSon1(a);

    AST abstract = ASTSon0(interface_stmt);
    if (abstract != NULL)
    {
        unsupported_construct(a, "ABSTRACT INTERFACE");
    }

    AST generic_spec = ASTSon1(interface_stmt);

    scope_entry_t** related_symbols = NULL;
    int num_related_symbols = 0;

    if (interface_specification_seq != NULL)
    {
        AST it;
        for_each_element(interface_specification_seq, it)
        {
            AST interface_specification = ASTSon1(it);

            if (ASTType(interface_specification) == AST_PROCEDURE)
            {
                unsupported_statement(interface_specification, "PROCEDURE");
            }
            else if (ASTType(interface_specification) == AST_MODULE_PROCEDURE)
            {
                if (decl_context.current_scope->related_entry->kind != SK_MODULE)
                {
                    running_error("%s: error: MODULE PROCEDURE statement not valid in this INTERFACE block\n",
                            ast_location(interface_specification));
                }

                AST procedure_name_list = ASTSon0(interface_specification);
                AST it2;
                for_each_element(procedure_name_list, it2)
                {
                    AST procedure_name = ASTSon1(it2);

                    scope_entry_t* entry = get_symbol_for_name_untyped(decl_context, procedure_name,
                            ASTText(procedure_name_list));

                    entry->kind = SK_FUNCTION;
                    entry->entity_specs.is_module_procedure = 1;

                    if (generic_spec != NULL)
                    {
                        P_LIST_ADD(related_symbols,
                                num_related_symbols,
                                entry);
                    }
                }
            }
            else if (ASTType(interface_specification) == AST_SUBROUTINE_PROGRAM_UNIT
                    || ASTType(interface_specification) == AST_FUNCTION_PROGRAM_UNIT)
            {
                scope_entry_t* interface_sym = NULL;
                build_scope_program_unit(interface_specification, 
                        decl_context, 
                        new_program_unit_context, 
                        &interface_sym);

                if (generic_spec != NULL)
                {
                    P_LIST_ADD(related_symbols,
                            num_related_symbols,
                            interface_sym);
                }
            }
            else
            {
                internal_error("Invalid tree '%s'\n", ast_print_node_type(ASTType(interface_specification)));
            }
        }
    }

    if (generic_spec != NULL)
    {
        const char* name = get_name_of_generic_spec(generic_spec);
        
        scope_entry_t* generic_spec_sym = query_name_no_implicit_or_builtin(decl_context, name);

        if (generic_spec_sym == NULL)
        {
            generic_spec_sym = new_fortran_symbol(decl_context, name);
            // If this name is not related to a specific interface, make it void
            generic_spec_sym->type_information = get_void_type();

            generic_spec_sym->file = ASTFileName(generic_spec);
            generic_spec_sym->line = ASTLine(generic_spec);
        }

        if (generic_spec_sym->kind != SK_UNDEFINED
                && generic_spec_sym->kind != SK_FUNCTION)
        {
            running_error("%s: error: redefining symbol '%s'\n", 
                    ast_location(generic_spec),
                    name);
        }

        generic_spec_sym->kind = SK_FUNCTION;
        generic_spec_sym->entity_specs.is_generic_spec = 1;

        int i;
        for (i = 0; i < num_related_symbols; i++)
        {
            P_LIST_ADD(generic_spec_sym->entity_specs.related_symbols,
                    generic_spec_sym->entity_specs.num_related_symbols,
                    related_symbols[i]);
        }

        scope_entry_t* current_symbol = decl_context.current_scope->related_entry;
        if (current_symbol != NULL)
        {
            P_LIST_ADD_ONCE(current_symbol->entity_specs.related_symbols, 
                    current_symbol->entity_specs.num_related_symbols,
                    generic_spec_sym);
        }
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_intrinsic_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    AST intrinsic_list = ASTSon0(a);

    AST it;
    for_each_element(intrinsic_list, it)
    {
        AST name = ASTSon1(it);

        scope_entry_t* entry = query_name_no_implicit(decl_context, ASTText(name));
        if (entry == NULL
                || !entry->entity_specs.is_builtin)
        {
            fprintf(stderr, "%s: warning: name '%s' is not known as an intrinsic\n", 
                    ast_location(name),
                    ASTText(name));
        }
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_lock_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "LOCK");
}

static void build_scope_namelist_stmt(AST a, decl_context_t decl_context)
{
    AST namelist_item_list = ASTSon0(a);

    scope_entry_t* current_symbol = decl_context.current_scope->related_entry;

    AST it;
    for_each_element(namelist_item_list, it)
    {
        AST namelist_item = ASTSon1(it);

        AST common_name = ASTSon0(namelist_item);
        AST namelist_group_object_list = ASTSon1(namelist_item);

        AST name = ASTSon0(common_name);

        scope_entry_t* new_namelist 
            = new_fortran_symbol(decl_context, ASTText(name));

        new_namelist->kind = SK_NAMELIST;
        new_namelist->file = ASTFileName(a);
        new_namelist->line = ASTLine(a);

        if (current_symbol != NULL)
        {
            P_LIST_ADD(current_symbol->entity_specs.related_symbols,
                    current_symbol->entity_specs.num_related_symbols,
                    new_namelist);
        }

        AST it2;
        for_each_element(namelist_group_object_list, it2)
        {
            AST namelist_item_name = ASTSon1(it2);

            scope_entry_t* namelist_element = get_symbol_for_name(decl_context, namelist_item_name, ASTText(namelist_item_name));

            namelist_element->entity_specs.is_in_namelist = 1;
            namelist_element->entity_specs.namelist = new_namelist;

            P_LIST_ADD(new_namelist->entity_specs.related_symbols,
                    new_namelist->entity_specs.num_related_symbols,
                    namelist_element);
        }
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_nullify_stmt(AST a, decl_context_t decl_context)
{
    AST pointer_object_list = ASTSon0(a);

    AST it;
    for_each_element(pointer_object_list, it)
    {
        AST pointer_object = ASTSon1(it);

        fortran_check_expression(pointer_object, decl_context);

        scope_entry_t* sym = expression_get_symbol(pointer_object);

        if (!is_pointer_type(sym->type_information))
        {
            running_error("%s: error: '%s' does not designate not a POINTER\n",
                    ast_location(a),
                    fortran_prettyprint_in_buffer(pointer_object));
        }
    }
}

static void build_scope_open_stmt(AST a, decl_context_t decl_context)
{
    AST connect_spec_list = ASTSon0(a);
    handle_opt_value_list(a, connect_spec_list, decl_context);
}

static void build_scope_optional_stmt(AST a, decl_context_t decl_context)
{
    AST name_list = ASTSon0(a);
    AST it;
    for_each_element(name_list, it)
    {
        AST name = ASTSon1(it);
        scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

        if (!entry->entity_specs.is_parameter)
        {
            running_error("%s: error: entity '%s' is not a dummy argument\n",
                    ASTText(name));
        }
        entry->entity_specs.is_optional = 1;
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_parameter_stmt(AST a, decl_context_t decl_context)
{
    AST named_constant_def_list = ASTSon0(a);

    AST it;

    for_each_element(named_constant_def_list, it)
    {
        AST named_constant_def = ASTSon1(it);

        AST name = ASTSon0(named_constant_def);
        AST constant_expr = ASTSon1(named_constant_def);

        fortran_check_expression(constant_expr, decl_context);

        scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

        if (is_void_type(entry->type_information))
        {
            running_error("%s: error: unknown entity '%s' in PARAMETER statement\n",
                    ast_location(name),
                    ASTText(name));
        }

        if (entry->kind == SK_UNDEFINED)
            entry->kind = SK_VARIABLE;

        entry->type_information = get_const_qualified_type(entry->type_information);
        entry->expression_value = constant_expr;
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_pointer_stmt(AST a, decl_context_t decl_context)
{
    AST pointer_decl_list = ASTSon0(a);
    AST it;

    for_each_element(pointer_decl_list, it)
    {
        AST pointer_decl = ASTSon1(it);

        AST name = pointer_decl;
        AST array_spec = NULL;
        if (ASTType(pointer_decl) == AST_DIMENSION_DECL)
        {
            name = ASTSon0(pointer_decl);
            array_spec = ASTSon1(pointer_decl);
        }

        scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

        if (is_pointer_type(entry->type_information))
        {
            running_error("%s: error: entity '%s' has already the POINTER attribute\n",
                    ast_location(a),
                    entry->symbol_name);
        }

        if (array_spec != NULL)
        {
            if (is_fortran_array_type(entry->type_information)
                    || is_pointer_to_fortran_array_type(entry->type_information))
            {
                running_error("%s: error: entity '%s' has already the DIMENSION attribute\n",
                        ast_location(a),
                        entry->symbol_name);
            }

            type_t* array_type = compute_type_from_array_spec(entry->type_information, 
                    array_spec,
                    decl_context,
                    /* array_spec_kind */ NULL);
            entry->type_information = array_type;
        }

        if (entry->kind == SK_UNDEFINED)
            entry->kind = SK_VARIABLE;

        entry->type_information = get_pointer_type(entry->type_information);
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_input_output_item(AST input_output_item, decl_context_t decl_context)
{
    if (ASTType(input_output_item) == AST_IMPLIED_DO)
    {
        generic_implied_do_handler(input_output_item, decl_context,
                build_scope_input_output_item_list);
    }
    else 
    {
        fortran_check_expression(input_output_item, decl_context);
    }
}

static void build_scope_input_output_item_list(AST input_output_item_list, decl_context_t decl_context)
{
    AST it;
    for_each_element(input_output_item_list, it)
    {
        build_scope_input_output_item(ASTSon1(it), decl_context);
    }
}

static void build_scope_print_stmt(AST a, decl_context_t decl_context)
{
    AST input_output_item_list = ASTSon1(a);

    build_scope_input_output_item_list(input_output_item_list, decl_context);
}

static void build_scope_procedure_decl_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "PROCEDURE");
}

static void build_scope_protected_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "PROTECTED");
}

static void build_scope_read_stmt(AST a, decl_context_t decl_context)
{
    AST io_control_spec_list = ASTSon0(a);
    handle_opt_value_list(a, io_control_spec_list, decl_context);

    if (ASTSon1(a) != NULL)
    {
        build_scope_input_output_item_list(ASTSon1(a), decl_context);
    }
}

static void build_scope_return_stmt(AST a, decl_context_t decl_context)
{
    AST int_expr = ASTSon1(a);
    if (int_expr != NULL)
    {
        fprintf(stderr, "%s: warning: deprecated RETURN with alternate return\n",
                ast_location(a));
        fortran_check_expression(a, decl_context);
    }
}

static void build_scope_save_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    AST saved_entity_list = ASTSon0(a);

    AST it;

    if (saved_entity_list == NULL)
    {
        fprintf(stderr, "%s: warning: SAVE statement without saved-entity-list is not properly supported at the moment\n",
                ast_location(a));
        return;
    }

    for_each_element(saved_entity_list, it)
    {
        AST saved_entity = ASTSon1(it);

        scope_entry_t* entry = NULL;
        if (ASTType(saved_entity) == AST_COMMON_NAME)
        {
            entry = query_common_name(decl_context, ASTText(ASTSon0(saved_entity)));

            if (entry == NULL)
            {
                running_error("%s: error: unknown common '%s' in SAVE statement", 
                        ast_location(a),
                        fortran_prettyprint_in_buffer(saved_entity));
            }
        }
        else
        {
            entry = get_symbol_for_name(decl_context, saved_entity, ASTText(saved_entity));
        }

        if (entry->kind == SK_UNDEFINED)
            entry->kind = SK_VARIABLE;

        entry->entity_specs.is_static = 1;
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_select_type_construct(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "SELECT TYPE");
}

static void build_scope_stmt_function_stmt(AST a, decl_context_t decl_context)
{
    AST name = ASTSon0(a);
    AST dummy_arg_name_list = ASTSon1(a);
    AST expr = ASTSon2(a);

    scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

    entry->kind = SK_FUNCTION;
    entry->entity_specs.is_stmt_function = 1;

    int num_dummy_arguments = 0;
    if (dummy_arg_name_list != NULL)
    {
        AST it;
        for_each_element(dummy_arg_name_list, it)
        {
            AST dummy_arg_item = ASTSon1(it);
            scope_entry_t* dummy_arg = get_symbol_for_name(decl_context, dummy_arg_item, ASTText(dummy_arg_item));

            if (!is_scalar_type(dummy_arg->type_information))
            {
                running_error("%s: error: dummy argument '%s' of statement function statement\n",
                        ast_location(dummy_arg_item),
                        fortran_prettyprint_in_buffer(dummy_arg_item));
            }

            // This can be used latter if trying to give a nonzero rank to this
            // entity
            dummy_arg->entity_specs.is_dummy_arg_stmt_function = 1;

            P_LIST_ADD(entry->entity_specs.related_symbols,
                    entry->entity_specs.num_related_symbols,
                    dummy_arg);

            num_dummy_arguments++;
        }
    }

    parameter_info_t parameter_info[1 + num_dummy_arguments];
    memset(parameter_info, 0, sizeof(parameter_info));

    int i;
    for (i = 0; i < num_dummy_arguments; i++)
    {
        parameter_info[i].type_info = get_indirect_type(entry->entity_specs.related_symbols[i]);
    }

    type_t* new_type = get_new_function_type(entry->type_information, 
            parameter_info, num_dummy_arguments);

    entry->type_information = new_type;

    fortran_check_expression(expr, decl_context);

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_stop_stmt(AST a, decl_context_t decl_context)
{
    AST stop_code = ASTSon0(a);
    if (stop_code != NULL)
    {
        fortran_check_expression(stop_code, decl_context);
    }
    
    ASTAttrSetValueType(a, LANG_IS_FORTRAN_STOP_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_pause_stmt(AST a, decl_context_t decl_context)
{
    fprintf(stderr, "%s: warning: deprecated PAUSE statement\n",
            ast_location(a));

    AST int_expr = ASTSon0(a);
    if (int_expr != NULL)
    {
        fortran_check_expression(int_expr, decl_context);
    }
}

static void build_scope_sync_all_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "SYNC ALL");
}

static void build_scope_sync_images_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "SYNC IMAGES");
}

static void build_scope_sync_memory_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "SYNC MEMORY");
}

static void build_scope_target_stmt(AST a, decl_context_t decl_context)
{
    AST target_decl_list = ASTSon0(a);

    AST it;
    for_each_element(target_decl_list, it)
    {
        AST target_decl = ASTSon1(it);

        AST name = target_decl;

        scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

        if (ASTType(target_decl_list) == AST_DIMENSION_DECL)
        {
            name = ASTSon0(target_decl);
            AST array_spec = ASTSon1(target_decl_list);
            AST coarray_spec = ASTSon2(target_decl_list);

            if (coarray_spec != NULL)
            {
                running_error("%s: sorry: coarrays are not supported\n", ast_location(name));
            }

            if (array_spec != NULL)
            {
                if (is_fortran_array_type(entry->type_information)
                        || is_pointer_to_fortran_array_type(entry->type_information))
                {
                    running_error("%s: error: DIMENSION attribute specified twice for entity '%s'\n", 
                            ast_location(a),
                            entry->symbol_name);
                }

                type_t* array_type = compute_type_from_array_spec(entry->type_information, 
                        array_spec,
                        decl_context,
                        /* array_spec_kind */ NULL);
                entry->type_information = array_type;
            }
        }

        if (entry->entity_specs.is_target)
        {
            running_error("%s: error: entity '%s' already has TARGET attribute\n", 
                    ast_location(target_decl),
                    entry->symbol_name);
        }

        if (entry->kind == SK_UNDEFINED)
            entry->kind = SK_VARIABLE;

        entry->entity_specs.is_target = 1;
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_type_declaration_stmt(AST a, decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "== [%s] Declaration statement ==\n", ast_location(a));
    }
    AST declaration_type_spec = ASTSon0(a);
    AST attr_spec_list = ASTSon1(a);
    AST entity_decl_list = ASTSon2(a);

    type_t* basic_type = gather_type_from_declaration_type_spec(declaration_type_spec, decl_context);

    attr_spec_t attr_spec;
    memset(&attr_spec, 0, sizeof(attr_spec));
    
    if (attr_spec_list != NULL)
    {
        gather_attr_spec_list(attr_spec_list, decl_context, &attr_spec);
    }

    AST it;
    for_each_element(entity_decl_list, it)
    {
        attr_spec_t current_attr_spec = attr_spec;
        AST declaration = ASTSon1(it);

        AST name = ASTSon0(declaration);
        AST entity_decl_specs = ASTSon1(declaration);

        scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

        if (!entry->entity_specs.is_implicit_basic_type)
        {
            running_error("%s: error: entity '%s' already has a basic type\n",
                    ast_location(name),
                    entry->symbol_name);
        }
        else
        {
            // It was not so much defined actually...
            entry->defined = 0;
        }

        if (entry->defined)
        {
            running_error("%s: error: redeclaration of entity '%s', first declared at '%s:%d'\n",
                    ast_location(declaration),
                    entry->symbol_name,
                    entry->file,
                    entry->line);
        }

        entry->type_information = update_basic_type_with_type(entry->type_information, basic_type);
        entry->entity_specs.is_implicit_basic_type = 0;
        entry->defined = 1;
        entry->file = ASTFileName(declaration);
        entry->line = ASTLine(declaration);

        AST char_length = NULL;
        AST initialization = NULL;
        if (entity_decl_specs != NULL)
        {
            AST array_spec = ASTSon0(entity_decl_specs);
            AST coarray_spec = ASTSon1(entity_decl_specs);
            char_length = ASTSon2(entity_decl_specs);
            initialization = ASTSon3(entity_decl_specs);

            if (array_spec != NULL)
            {
                if (current_attr_spec.is_dimension)
                {
                    running_error("%s: error: DIMENSION attribute specified twice\n", ast_location(declaration));
                }
                current_attr_spec.is_dimension = 1;
                current_attr_spec.array_spec = array_spec;
            }

            if (coarray_spec != NULL)
            {
                if (current_attr_spec.is_codimension)
                {
                    running_error("%s: error: CODIMENSION attribute specified twice\n", ast_location(declaration));
                }
                current_attr_spec.is_codimension = 1;
                current_attr_spec.coarray_spec = coarray_spec;
            }

            if (char_length != NULL)
            {
                if (ASTType(char_length) != AST_SYMBOL
                        || strcmp(ASTText(char_length), "*") != 0)
                {
                    fortran_check_expression(char_length, decl_context);

                    AST lower_bound = ASTLeaf(AST_DECIMAL_LITERAL, ASTFileName(char_length), ASTLine(char_length), "1");
                    entry->type_information = get_array_type_bounds(
                            array_type_get_element_type(entry->type_information), 
                            lower_bound, char_length, decl_context);
                }
                else
                {
                    entry->type_information = get_array_type(
                            array_type_get_element_type(entry->type_information), 
                            NULL, decl_context);
                }
            }

            if (initialization != NULL)
            {
                if (ASTType(initialization) == AST_POINTER_INITIALIZATION)
                {
                    if (!current_attr_spec.is_pointer)
                    {
                        running_error("%s: error: no POINTER attribute, required for pointer initialization\n",
                                ast_location(initialization));
                    }
                    initialization = ASTSon0(initialization);
                }
                fortran_check_expression(initialization, decl_context);
            }
        }

        // Stop the madness here
        if (current_attr_spec.is_codimension)
        {
            running_error("%s: sorry: coarrays are not supported\n", ast_location(declaration));
        }

        if (current_attr_spec.is_dimension)
        {
            type_t* array_type = compute_type_from_array_spec(entry->type_information, 
                    current_attr_spec.array_spec,
                    decl_context,
                    /* array_spec_kind */ NULL);
            entry->kind = SK_VARIABLE;
            entry->type_information = array_type;
        }

        if (initialization != NULL)
        {
            if (ASTType(initialization) == AST_POINTER_INITIALIZATION)
            {
                if (!current_attr_spec.is_pointer)
                {
                    running_error("%s: error: no POINTER attribute, required for pointer initialization\n",
                            ast_location(initialization));
                }
                initialization = ASTSon0(initialization);
            }
            fortran_check_expression(initialization, decl_context);

            entry->kind = SK_VARIABLE;
        }

        if (initialization != NULL)
        {
            entry->expression_value = initialization;
            entry->kind = SK_VARIABLE;
            if (!current_attr_spec.is_constant)
            {
                entry->entity_specs.is_static = 1;
            }
            else
            {
                entry->type_information = get_const_qualified_type(entry->type_information);
            }
        }

        if (current_attr_spec.is_constant && initialization == NULL)
        {
            running_error("%s: error: PARAMETER is missing an initializer\n", ast_location(declaration));
        }

        if (!current_attr_spec.is_constant)
        {
        }

        // FIXME - Should we do something with this attribute?
        if (current_attr_spec.is_value)
        {
            if (!entry->entity_specs.is_parameter)
            {
                running_error("%s: error: VALUE attribute is only for dummy arguments\n",
                        ast_location(declaration));
            }
        }

        if (current_attr_spec.is_intent)
        {
            if (!entry->entity_specs.is_parameter)
            {
                running_error("%s: error: INTENT attribute is only for dummy arguments\n",
                        ast_location(declaration));
            }
        }
        else
        {
            entry->entity_specs.intent_kind = current_attr_spec.intent_kind;
        }

        if (current_attr_spec.is_optional)
        {
            if (!entry->entity_specs.is_parameter)
            {
                running_error("%s: error: OPTIONAL attribute is only for dummy arguments\n",
                        ast_location(declaration));
            }
            entry->entity_specs.is_optional = 1;
        }

        if (current_attr_spec.is_allocatable)
        {
            if (!current_attr_spec.is_dimension)
            {
                running_error("%s: error: ALLOCATABLE attribute cannot be used on scalars\n", 
                        ast_location(declaration));
            }
            entry->entity_specs.is_allocatable = 1;
            entry->kind = SK_VARIABLE;
        }

        if (current_attr_spec.is_external
                || current_attr_spec.is_intrinsic)
        {
            entry->kind = SK_FUNCTION;
            entry->type_information = get_nonproto_function_type(entry->type_information, 0);
        }

        if (current_attr_spec.is_external)
        {
            entry->entity_specs.is_extern = 1;
        }

        if (current_attr_spec.is_save)
        {
            entry->entity_specs.is_static = 1;
        }

        if (current_attr_spec.is_pointer)
        {
            entry->type_information = get_pointer_type(entry->type_information);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Type of symbol '%s' is '%s'\n", entry->symbol_name, print_declarator(entry->type_information));
        }
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_unlock_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "UNLOCK");
}

static scope_entry_t* query_module_for_symbol_name(scope_entry_t* module_symbol, const char* name)
{
    scope_entry_t* sym_in_module = NULL;
    int i;
    for (i = 0; i < module_symbol->entity_specs.num_related_symbols; i++)
    {
        scope_entry_t* sym = module_symbol->entity_specs.related_symbols[i];

        if (strcasecmp(sym->symbol_name, name) == 0)
        {
            sym_in_module = sym;
            break;
        }
    }

    return sym_in_module;
}

static void build_scope_use_stmt(AST a, decl_context_t decl_context)
{
    AST module_nature = NULL;
    AST module_name = NULL;
    AST rename_list = NULL;
    AST only_list = NULL;

    char is_only = 0;

    if (ASTType(a) == AST_USE_STATEMENT)
    {
        module_nature = ASTSon0(a);
        module_name = ASTSon1(a);
        rename_list = ASTSon2(a);
    }
    else if (ASTType(a) == AST_USE_ONLY_STATEMENT)
    {
        module_nature = ASTSon0(a);
        module_name = ASTSon1(a);
        only_list = ASTSon2(a);
        is_only = 1;
    }
    else
    {
        internal_error("Unexpected node %s", ast_print_node_type(ASTType(a)));
    }

    if (module_nature != NULL)
    {
        running_error("%s: error: specifying the nature of the module is not supported\n",
                ast_location(a));
    }

    const char* module_name_str = strtolower(ASTText(module_name));

    scope_entry_t* module_symbol = NULL;

    // Query first in the module cache

    DEBUG_CODE()
    {
        fprintf(stderr, "BUILDSCOPE: Loading module '%s'\n", module_name_str);
    }

    rb_red_blk_node* query = rb_tree_query(CURRENT_COMPILED_FILE->module_cache, module_name_str);
    if (query != NULL)
    {
        module_symbol = (scope_entry_t*)rb_node_get_info(query);
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Module '%s' was in the module cache of this file\n", module_name_str);
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "BUILDSCOPE: Loading module '%s' from the filesystem\n", module_name_str);
        }
        // Load the file
        load_module_info(strtolower(ASTText(module_name)), &module_symbol);
        // And add it to the cache of opened modules
        ERROR_CONDITION(module_symbol == NULL, "Invalid symbol", 0);
        rb_tree_insert(CURRENT_COMPILED_FILE->module_cache, module_name_str, module_symbol);
    }

    insert_entry(decl_context.current_scope, module_symbol);

    if (!is_only)
    {
#define MAX_RENAMED_SYMBOLS 256
        int num_renamed_symbols = 0;
        scope_entry_t* renamed_symbols[MAX_RENAMED_SYMBOLS];
        memset(renamed_symbols, 0, sizeof(renamed_symbols));

        if (rename_list != NULL)
        {
            AST it;
            for_each_element(rename_list, it)
            {
                AST rename_tree = ASTSon1(it);

                AST local_name = ASTSon0(rename_tree);
                AST sym_in_module_name = ASTSon1(rename_tree);
                const char* sym_in_module_name_str = get_name_of_generic_spec(sym_in_module_name);
                scope_entry_t* sym_in_module = 
                    query_module_for_symbol_name(module_symbol, sym_in_module_name_str);

                if (sym_in_module == NULL)
                {
                    running_error("%s: error: symbol '%s' not found in module '%s'\n", 
                            ast_location(sym_in_module_name),
                            prettyprint_in_buffer(sym_in_module_name),
                            module_symbol->symbol_name);
                }

                insert_alias(decl_context.current_scope, sym_in_module, get_name_of_generic_spec(local_name));

                // "USE M, C => A, D => A" is valid so we avoid adding twice
                // 'A' in the list (it would be harmless, though)
                char found = 0;
                int i;
                for (i = 0; i < num_renamed_symbols && found; i++)
                {
                    found = (renamed_symbols[i] == sym_in_module);
                }
                if (!found)
                {
                    ERROR_CONDITION(num_renamed_symbols == MAX_RENAMED_SYMBOLS, "Too many renames", 0);
                    renamed_symbols[num_renamed_symbols] = sym_in_module;
                    num_renamed_symbols++;
                }
            }
        }

        // Now add the ones not renamed
        int i;
        for (i = 0; i < module_symbol->entity_specs.num_related_symbols; i++)
        {
            scope_entry_t* sym_in_module = module_symbol->entity_specs.related_symbols[i];
            char found = 0;
            int j;
            for (j = 0; j < num_renamed_symbols && !found; j++)
            {
                found = (renamed_symbols[j] == sym_in_module);
            }
            if (!found)
            {
                insert_entry(decl_context.current_scope, sym_in_module);
            }
        }
    }
    else // is_only
    {
        AST it;
        for_each_element(only_list, it)
        {
            AST only = ASTSon1(it);

            switch (ASTType(only))
            {
                case AST_RENAME:
                    {
                        AST local_name = ASTSon0(only);
                        AST sym_in_module_name = ASTSon1(only);
                        const char * sym_in_module_name_str = ASTText(sym_in_module_name);

                        scope_entry_t* sym_in_module = 
                            query_module_for_symbol_name(module_symbol, sym_in_module_name_str);

                        if (sym_in_module == NULL)
                        {
                            running_error("%s: error: symbol '%s' not found in module '%s'\n", 
                                    ast_location(sym_in_module_name),
                                    prettyprint_in_buffer(sym_in_module_name),
                                    module_symbol->symbol_name);
                        }

                        insert_alias(decl_context.current_scope, sym_in_module, get_name_of_generic_spec(local_name));

                        break;
                    }
                default:
                    {
                        // This is a generic specifier
                        AST sym_in_module_name = only;
                        const char * sym_in_module_name_str = ASTText(sym_in_module_name);

                        scope_entry_t* sym_in_module = 
                            query_module_for_symbol_name(module_symbol, sym_in_module_name_str);

                        if (sym_in_module == NULL)
                        {
                            running_error("%s: error: symbol '%s' not found in module '%s'\n", 
                                    ast_location(sym_in_module_name),
                                    prettyprint_in_buffer(sym_in_module_name),
                                    module_symbol->symbol_name);
                        }

                        insert_entry(decl_context.current_scope, sym_in_module);
                        break;
                    }
            }
        }
    }
}

static void build_scope_value_stmt(AST a, decl_context_t decl_context)
{
    AST name_list = ASTSon0(a);

    AST it;
    for_each_element(name_list, it)
    {
        AST name = ASTSon1(it);

        scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

        if (!entry->entity_specs.is_parameter)
        {
            running_error("%s: error: entity '%s' is not a dummy argument\n",
                    ast_location(name),
                    entry->symbol_name);
        }

        if (entry->kind == SK_UNDEFINED)
            entry->kind = SK_VARIABLE;

        entry->entity_specs.is_value = 1;
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_volatile_stmt(AST a, decl_context_t decl_context)
{
    AST name_list = ASTSon0(a);

    AST it;
    for_each_element(name_list, it)
    {
        AST name = ASTSon1(it);

        scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

        if (!entry->entity_specs.is_parameter)
        {
            running_error("%s: error: entity '%s' is not a dummy argument\n",
                    ast_location(name),
                    entry->symbol_name);
        }

        if (entry->kind == SK_UNDEFINED)
            entry->kind = SK_VARIABLE;

        if (!is_volatile_qualified_type(entry->type_information))
        {
            entry->type_information = get_volatile_qualified_type(entry->type_information);
        }
        else
        {
            running_error("%s: error: entity '%s' already has VOLATILE attribute\n",
                    ast_location(a), entry->symbol_name);
        }
    }

    ASTAttrSetValueType(a, LANG_IS_FORTRAN_SPECIFICATION_STATEMENT, tl_type_t, tl_bool(1));
}

static void build_scope_wait_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "WAIT");
}

static void build_scope_where_body_construct_seq(AST a, decl_context_t decl_context)
{
    if (a == NULL)
        return;

    AST it;
    for_each_element(a, it)
    {
        AST statement = ASTSon1(it);
        fortran_build_scope_statement(statement, decl_context);
    }
}

static void build_scope_mask_elsewhere_part_seq(AST mask_elsewhere_part_seq, decl_context_t decl_context)
{
    if (mask_elsewhere_part_seq == NULL)
        return;

    AST it;
    for_each_element(mask_elsewhere_part_seq, it)
    {
        AST mask_elsewhere_part = ASTSon1(it);

        AST masked_elsewhere_stmt = ASTSon0(mask_elsewhere_part);
        AST where_body_construct_seq = ASTSon1(mask_elsewhere_part);

        AST expr = ASTSon0(masked_elsewhere_stmt);
        fortran_check_expression(expr, decl_context);

        build_scope_where_body_construct_seq(where_body_construct_seq, decl_context);
    }
}

static void build_scope_where_construct(AST a, decl_context_t decl_context)
{
    AST where_construct_stmt = ASTSon0(a);
    AST mask_expr = ASTSon1(where_construct_stmt);
    fortran_check_expression(mask_expr, decl_context);
    
    AST where_construct_body = ASTSon1(a);

    AST main_where_body = ASTSon0(where_construct_body);
    build_scope_where_body_construct_seq(main_where_body, decl_context);

    AST mask_elsewhere_part_seq = ASTSon1(where_construct_body);
    build_scope_mask_elsewhere_part_seq(mask_elsewhere_part_seq, decl_context);
    
    // Do nothing with elsewhere_stmt ASTSon2(where_construct_body)

    AST elsewhere_body = ASTSon3(where_construct_body);
    build_scope_where_body_construct_seq(elsewhere_body, decl_context);
}

static void build_scope_where_stmt(AST a, decl_context_t decl_context)
{
    AST mask_expr = ASTSon0(a);
    fortran_check_expression(mask_expr, decl_context);
    AST where_assignment_stmt = ASTSon1(a);
    build_scope_expression_stmt(where_assignment_stmt, decl_context);
}

static void build_scope_while_stmt(AST a, decl_context_t decl_context)
{
    AST expr = ASTSon0(a);
    AST block = ASTSon1(a);

    fortran_check_expression(expr, decl_context);

    if (!is_bool_type(expression_get_type(expr)))
    {
        fprintf(stderr, "%s: warning: condition of DO WHILE loop is not a logical expression\n",
                ast_location(expr));
    }

    fortran_build_scope_statement(block, decl_context);

    ASTAttrSetValueType(a, LANG_IS_WHILE_STATEMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_WHILE_STATEMENT_CONDITION, tl_type_t, tl_ast(expr));
    ASTAttrSetValueType(a, LANG_WHILE_STATEMENT_BODY, tl_type_t, tl_ast(block));
}

static void build_scope_write_stmt(AST a, decl_context_t decl_context)
{
    handle_opt_value_list(a, ASTSon0(a), decl_context);

    AST input_output_item_list = ASTSon1(a);
    if (input_output_item_list != NULL)
    {
        build_scope_input_output_item_list(input_output_item_list, decl_context);
    }
}

static void build_scope_pragma_custom_ctr(AST a, decl_context_t decl_context)
{
    AST statement = ASTSon1(a);
    fortran_build_scope_statement(statement, decl_context);
}

static void build_scope_pragma_custom_dir(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing for directives
}

typedef void opt_value_fun_handler_t(AST io_stmt, AST opt_value, decl_context_t);

typedef struct opt_value_map_tag
{
    const char* name;
    opt_value_fun_handler_t *handler;
} opt_value_map_t;

#define OPT_VALUE_LIST \
  OPT_VALUE(access) \
  OPT_VALUE(acquired) \
  OPT_VALUE(action) \
  OPT_VALUE(advance) \
  OPT_VALUE(asynchronous) \
  OPT_VALUE(blank) \
  OPT_VALUE(decimal) \
  OPT_VALUE(delim) \
  OPT_VALUE(encoding) \
  OPT_VALUE(eor) \
  OPT_VALUE(err) \
  OPT_VALUE(end) \
  OPT_VALUE(errmsg) \
  OPT_VALUE(exist) \
  OPT_VALUE(file) \
  OPT_VALUE(fmt) \
  OPT_VALUE(form) \
  OPT_VALUE(formatted) \
  OPT_VALUE(id) \
  OPT_VALUE(iomsg) \
  OPT_VALUE(iostat) \
  OPT_VALUE(mold) \
  OPT_VALUE(named) \
  OPT_VALUE(newunit) \
  OPT_VALUE(nextrec) \
  OPT_VALUE(nml) \
  OPT_VALUE(number) \
  OPT_VALUE(opened) \
  OPT_VALUE(pad) \
  OPT_VALUE(pending) \
  OPT_VALUE(pos) \
  OPT_VALUE(position) \
  OPT_VALUE(read) \
  OPT_VALUE(readwrite) \
  OPT_VALUE(rec) \
  OPT_VALUE(recl) \
  OPT_VALUE(round) \
  OPT_VALUE(sequential) \
  OPT_VALUE(sign) \
  OPT_VALUE(size) \
  OPT_VALUE(source) \
  OPT_VALUE(stat) \
  OPT_VALUE(status) \
  OPT_VALUE(stream) \
  OPT_VALUE(unformatted) \
  OPT_VALUE(unit) \
  OPT_VALUE(write) \
  OPT_VALUE(ambiguous_io_spec) 

#define OPT_VALUE(_name) \
     static opt_value_fun_handler_t opt_##_name##_handler;
OPT_VALUE_LIST
#undef OPT_VALUE

opt_value_map_t opt_value_map[] =
{
#define OPT_VALUE(_name) \
     { #_name, opt_##_name##_handler },
    OPT_VALUE_LIST
#undef OPT_VALUE
};

static char opt_value_list_init = 0;

static int opt_value_map_compare(const void* v1, const void* v2)
{
    const opt_value_map_t* p1 = (const opt_value_map_t*) v1;
    const opt_value_map_t* p2 = (const opt_value_map_t*) v2;

    return strcasecmp(p1->name, p2->name);
}

static void handle_opt_value(AST io_stmt, AST opt_value, decl_context_t decl_context)
{
    opt_value_map_t key;
    key.name = ASTText(opt_value);

    ERROR_CONDITION(key.name == NULL, "Invalid opt_value without name of opt", 0);

    opt_value_map_t *elem =
        (opt_value_map_t*)bsearch(&key, opt_value_map, 
                sizeof(opt_value_map) / sizeof(opt_value_map[1]),
                sizeof(opt_value_map[0]),
                opt_value_map_compare);

    ERROR_CONDITION(elem == NULL, "Invalid opt-value '%s' at %s\n", key.name, ast_location(opt_value));
    ERROR_CONDITION(elem->handler == NULL, "Invalid handler for opt-value '%s'\n", key.name);

    (elem->handler)(io_stmt, opt_value, decl_context);
}

static void handle_opt_value_list(AST io_stmt, AST opt_value_list, decl_context_t decl_context)
{
    if (!opt_value_list_init)
    {
        qsort(opt_value_map, 
                sizeof(opt_value_map) / sizeof(opt_value_map[1]),
                sizeof(opt_value_map[0]),
                opt_value_map_compare);
        opt_value_list_init = 1;
    }
    AST it;
    for_each_element(opt_value_list, it)
    {
        AST opt_value = ASTSon1(it);
        handle_opt_value(io_stmt, opt_value, decl_context);
    }
}

static char opt_common_int_expr(AST value, decl_context_t decl_context, const char* opt_name)
{
    fortran_check_expression(value, decl_context);
    if (!is_integer_type(expression_get_type(value))
            && !(is_pointer_type(expression_get_type(value))
                && is_integer_type(pointer_type_get_pointee_type(expression_get_type(value)))))
    {
        fprintf(stderr, "%s: warning: specifier %s requires a character expression\n",
                ast_location(value),
                opt_name);
        return 0;
    }
    return 1;
}

static char opt_common_character_expr(AST value, decl_context_t decl_context, const char* opt_name)
{
    fortran_check_expression(value, decl_context);
    if (!is_fortran_character_type(expression_get_type(value))
            && !is_pointer_to_fortran_character_type(expression_get_type(value)))
    {
        fprintf(stderr, "%s: warning: specifier %s requires a character expression\n",
                ast_location(value),
                opt_name);
        return 0;
    }
    return 1;
}

static char opt_common_const_character_expr(AST value, decl_context_t decl_context, const char* opt_name)
{
    return opt_common_character_expr(value, decl_context, opt_name);
}

static char opt_common_int_variable(AST value, decl_context_t decl_context, const char* opt_name)
{
    fortran_check_expression(value, decl_context);
    if (expression_get_symbol(value) == NULL)
    { 
        scope_entry_t* sym = expression_get_symbol(value);
        if (sym == NULL
                || (!is_integer_type(sym->type_information)
                    && !(is_pointer_type(sym->type_information)
                        && is_integer_type(pointer_type_get_pointee_type(sym->type_information)))))
        {
            fprintf(stderr, "%s: warning: specifier %s requires an integer variable\n",
                    ast_location(value),
                    opt_name);
            return 0;
        }
    }
    return 1;
}

static char opt_common_logical_variable(AST value, decl_context_t decl_context, const char* opt_name)
{
    fortran_check_expression(value, decl_context);
    if (expression_get_symbol(value) == NULL)
    { 
        scope_entry_t* sym = expression_get_symbol(value);
        if (sym == NULL
                || (!is_bool_type(sym->type_information)
                    && !(is_pointer_type(sym->type_information)
                        && is_bool_type(pointer_type_get_pointee_type(sym->type_information)))))
        {
            fprintf(stderr, "%s: warning: specifier %s requires a logical variable\n",
                    ast_location(value),
                    opt_name);
            return 0;
        }
    }
    return 1;
}

static void opt_access_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "ACCESS");
}

static void opt_acquired_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    fortran_check_expression(value, decl_context);
    if (expression_get_symbol(value) == NULL
            || !is_bool_type(expression_get_symbol(value)->type_information))
    {
        fprintf(stderr, "%s: warning: specifier 'ACQUIRED LOCK' requires a logical variable\n",
                ast_location(value));
    }
}

static void opt_action_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "ACTION");
}

static void opt_advance_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_const_character_expr(value, decl_context, "ADVANCE");
}

static void opt_asynchronous_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "ASYNCHRONOUS");
}

static void opt_blank_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "BLANK");
}

static void opt_decimal_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "DECIMAL");
}

static void opt_delim_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "DELIM");
}

static void opt_encoding_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "ENCODING");
}

static void opt_eor_handler(AST io_stmt UNUSED_PARAMETER, 
        AST opt_value UNUSED_PARAMETER, 
        decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing
}

static void opt_err_handler(AST io_stmt UNUSED_PARAMETER, 
        AST opt_value UNUSED_PARAMETER, 
        decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing
}

static void opt_end_handler(AST io_stmt UNUSED_PARAMETER, 
        AST opt_value UNUSED_PARAMETER, 
        decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing
}

static void opt_errmsg_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "ERRMSG");
}

static void opt_exist_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_logical_variable(value, decl_context, "EXIST");
}

static void opt_file_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "FILE");
}

static void opt_fmt_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    if (!(ASTType(value) == AST_SYMBOL
            && strcmp(ASTText(value), "*") == 0))
    {
        fortran_check_expression(value, decl_context);

        type_t* t = expression_get_type(value);

        if (!((is_integer_type(t) && ASTType(value) == AST_DECIMAL_LITERAL)
                    || is_fortran_character_type(t)))
        {
            fprintf(stderr, "%s: warning: specifier FMT requires a character expression or a label of a FORMAT statement\n",
                    ast_location(value));
        }
    }
}

static void opt_form_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "FORM");
}

static void opt_formatted_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "FORMATTED");
}

static void opt_id_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_int_expr(value, decl_context, "ID");
}

static void opt_iomsg_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "IOMSG");
}

static void opt_iostat_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_int_expr(value, decl_context, "IOSTAT");
}

static void opt_mold_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    fortran_check_expression(value, decl_context);
}

static void opt_named_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_logical_variable(value, decl_context, "NAMED");
}

static void opt_newunit_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_int_variable(value, decl_context, "NEWUNIT");
}

static void opt_nextrec_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_int_variable(value, decl_context, "NEXTREC");
}

static void opt_nml_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    scope_entry_t* entry = query_name_no_implicit_or_builtin(decl_context, ASTText(value));
    if (entry == NULL
            || entry->kind != SK_NAMELIST)
    {
        fprintf(stderr, "%s: warning: entity '%s' in NML specifier is not a namelist\n",
                ast_location(value),
                ASTText(value));
    }
}

static void opt_number_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_int_variable(value, decl_context, "NUMBER");
}

static void opt_opened_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_logical_variable(value, decl_context, "OPENED");
}

static void opt_pad_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "PAD");
}

static void opt_pending_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_logical_variable(value, decl_context, "PENDING");
}

static void opt_pos_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_int_expr(value, decl_context, "POS");
}

static void opt_position_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "POSITION");
}

static void opt_read_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "READ");
}

static void opt_readwrite_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "READWRITE");
}

static void opt_rec_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_int_expr(value, decl_context, "REC");
}

static void opt_recl_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_int_expr(value, decl_context, "RECL");
}

static void opt_round_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "ROUND");
}

static void opt_sequential_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "SEQUENTIAL");
}

static void opt_sign_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "SIGN");
}

static void opt_size_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_int_expr(value, decl_context, "SIGN");
}

static void opt_source_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    fortran_check_expression(value, decl_context);
}

static void opt_stat_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_int_variable(value, decl_context, "STAT");
}

static void opt_status_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "STATUS");
}

static void opt_stream_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "STREAM");
}

static void opt_unformatted_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "UNFORMATTED");
}

static void opt_unit_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    if (!(ASTType(value) == AST_SYMBOL
            && strcmp(ASTText(value), "*") == 0))
    {
        fortran_check_expression(value, decl_context);

        type_t* t = expression_get_type(value);
        if (!(is_integer_type(t)
                    || ((expression_get_symbol(value) != NULL)
                        && is_fortran_character_type(expression_get_symbol(value)->type_information))))
        {
            fprintf(stderr, "%s: warning: specifier UNIT requires a character variable or a scalar integer expression\n",
                    ast_location(value));
        }
    }
}

static void opt_write_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value, decl_context_t decl_context)
{
    AST value = ASTSon0(opt_value);
    opt_common_character_expr(value, decl_context, "WRITE");
}

static int get_position_in_io_spec_list(AST value)
{
    AST list = ASTParent(value);

    int n = 0;
    AST it;

    for_each_element(list, it)
    {
        n++;
    }

    return n;
}

static void opt_ambiguous_io_spec_handler(AST io_stmt UNUSED_PARAMETER, AST opt_value_ambig, decl_context_t decl_context)
{
    // This ambiguous io spec handler exists because of the definition of io-control-spec
    //
    // io-control-spec -> [ UNIT = ] io-unit
    //                    [ FMT = ] format
    //                    [ NML = ] namelist-group-name
    //
    // Based on the following constraints we should be able to disambiguate
    //
    //   a) io-unit without UNIT = should be in the first position of the
    //      io-control-spec-list
    //   b) format without FMT = should be in the second position of the
    //      io-control-spec-list and the first shall be a io-unit
    //   c) namelist-group-name withouth NML = should be in the second position
    //      of the io-control-spec-list and the first shall be a io-unit
    //   d) it is not valid to specify both a format and a namelist-group-name
    //
    // A io-unit must be an scalar-int-expression, a character-variable or a '*'
    // A format must be a default-character-expression, a label or a '*'

    int io_unit_option = -1;
    int namelist_option = -1;
    int format_option = -1;

    int i;
    for (i = 0; i < ast_get_num_ambiguities(opt_value_ambig); i++)
    {
        AST option = ast_get_ambiguity(opt_value_ambig, i);
        const char* t = ASTText(option);
        ERROR_CONDITION((t == NULL), "io-spec is missing text", 0);

        int *p = NULL;
        if (strcasecmp(t, "unit") == 0)
        {
            p = &io_unit_option;
        }
        else if (strcasecmp(t, "fmt") == 0)
        {
            p = &format_option;
        }
        else if (strcasecmp(t, "nml") == 0)
        {
            p = &namelist_option;
        }
        else
        {
            internal_error("%s: Unexpected opt_value_ambig io-spec '%s'\n", ast_location(option), t);
        }

        ERROR_CONDITION(*p >= 0, "%s Repeated ambiguity tree!", ast_location(option));

        *p = i;
    }

    int position = get_position_in_io_spec_list(opt_value_ambig);


    char bad = 0;
    // First item
    if (position == 1)
    {
        if (io_unit_option < 0)
        {
            bad = 1;
        }
        else
        {
            // Force a io-unit
            ast_replace_with_ambiguity(opt_value_ambig, io_unit_option);
        }
    }
    // Second item
    else if (position == 2)
    {
        // We should check that the first one was a io-unit
        AST parent = ASTParent(opt_value_ambig);
        AST previous = ASTSon0(parent);
        AST io_spec = ASTSon1(previous);

        if ((ASTText(io_spec) == NULL)
                || (strcasecmp(ASTText(io_spec), "unit") != 0))
        {
            bad = 1;
        }
        else
        {
            if (namelist_option < 0)
            {
                // This can be only a FMT
                if (format_option < 0)
                {
                    bad = 1;
                }
                else
                {
                    ast_replace_with_ambiguity(opt_value_ambig, format_option);
                }
            }
            else
            {
                // Start checking if it is a real NML
                AST nml_io_spec = ast_get_ambiguity(opt_value_ambig, namelist_option);
                AST value = ASTSon0(nml_io_spec);

                scope_entry_t* entry = query_name_no_implicit_or_builtin(decl_context, ASTText(value));

                if (entry == NULL
                        || entry->kind != SK_NAMELIST)
                {
                    // This must be a FMT
                    if (format_option < 0)
                    {
                        bad = 1;
                    }
                    else
                    {
                        ast_replace_with_ambiguity(opt_value_ambig, format_option);
                    }
                }
                else
                {
                    // This is a NML
                    ast_replace_with_ambiguity(opt_value_ambig, namelist_option);
                }
            }
        }
    }
    else
    {
        bad = 1;
    }

    if (!bad)
    {
        // Not opt_value_ambig anymore
        handle_opt_value(io_stmt, opt_value_ambig, decl_context);
    }
    else
    {
        running_error("%s: error: invalid io-control-spec '%s'\n", 
                ast_location(opt_value_ambig),
                fortran_prettyprint_in_buffer(opt_value_ambig));
    }
}

static char check_statement_function_statement(AST stmt, decl_context_t decl_context)
{
    // F (X) = Y
    AST name = ASTSon0(stmt);
    AST dummy_arg_name_list = ASTSon1(stmt);
    AST expr = ASTSon2(stmt);

    scope_entry_t* entry = get_symbol_for_name(decl_context, name, ASTText(name));

    if (!is_scalar_type(entry->type_information))
        return 0;

    if (dummy_arg_name_list != NULL)
    {
        AST it;
        for_each_element(dummy_arg_name_list, it)
        {
            AST dummy_name = ASTSon1(it);
            scope_entry_t* dummy_arg = get_symbol_for_name(decl_context, dummy_name, ASTText(dummy_name));

            if (!is_scalar_type(dummy_arg->type_information))
                return 0;
        }
    }

    enter_test_expression();
    fortran_check_expression(expr, decl_context);
    leave_test_expression();

    if (is_error_type(expression_get_type(expr)))
        return 0;

    return 1;
}

static void build_scope_ambiguity_statement(AST ambig_stmt, decl_context_t decl_context)
{
    ERROR_CONDITION(ASTType(ambig_stmt) != AST_AMBIGUITY, "Invalid tree %s\n", ast_print_node_type(ASTType(ambig_stmt)));
    ERROR_CONDITION(strcmp(ASTText(ambig_stmt), "ASSIGNMENT") != 0, "Invalid ambiguity", 0);

    int num_ambig = ast_get_num_ambiguities(ambig_stmt);
    int i;

    int result = -1;
    int index_expr = -1;

    for (i = 0; i < num_ambig; i++)
    {
        AST stmt = ast_get_ambiguity(ambig_stmt, i);

        char ok = 0;
        switch (ASTType(stmt))
        {
            case AST_EXPRESSION_STATEMENT:
                {
                    enter_test_expression();
                    index_expr = i;
                    fortran_check_expression(ASTSon0(stmt), decl_context);
                    ok = !is_error_type(expression_get_type(ASTSon0(stmt)));
                    leave_test_expression();
                    break;
                }
            case AST_STATEMENT_FUNCTION_STATEMENT:
                {
                    ok = check_statement_function_statement(stmt, decl_context);
                    break;
                }
            default:
                {
                    internal_error("Invalid node '%s'\n", ast_print_node_type(ASTType(ambig_stmt)));
                }
        }

        if (ok)
        {
            if (result == -1)
            {
                result = i;
            }
            else
            {
                // It means that this could not be actually disambiguated
                result = -2;
            }
        }
    }

    ERROR_CONDITION(index_expr < 0, "Something is utterly broken in this ambiguous node\n", 0);

    if (result < 0)
    {
        // Default to an expression since 99% of times is what people meant
        AST expr = ast_get_ambiguity(ambig_stmt, index_expr);
        expression_clear_computed_info(expr);
        ast_replace_with_ambiguity(ambig_stmt, index_expr);
    }
    else
    {
        AST tree = ast_get_ambiguity(ambig_stmt, result);
        expression_clear_computed_info(tree);
        ast_replace_with_ambiguity(ambig_stmt, result);
    }
}
