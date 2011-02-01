#include "fortran03-buildscope.h"
#include "fortran03-scope.h"
#include "fortran03-exprtype.h"
#include "fortran03-prettyprint.h"
#include "fortran03-typeutils.h"
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
#include <string.h>
#include <stdlib.h>

void fortran_initialize_translation_unit_scope(translation_unit_t* translation_unit)
{
    decl_context_t decl_context;
    initialize_translation_unit_scope(translation_unit, &decl_context);
    // TODO: Fortran intrinsics
}

static void build_scope_program_unit_seq(AST program_unit_seq, 
        decl_context_t decl_context);

void build_scope_fortran_translation_unit(translation_unit_t* translation_unit)
{
    AST a = translation_unit->parsed_tree;
    // Technically Fortran does not have a global scope but it is convenient to have one
    decl_context_t decl_context 
        = scope_link_get_global_decl_context(translation_unit->scope_link);

    AST list = ASTSon0(a);
    if (list != NULL)
    {
        build_scope_program_unit_seq(list, decl_context);
    }
}

static void build_scope_program_unit(AST program_unit, 
        decl_context_t decl_context);
static void build_scope_program_unit_seq(AST program_unit_seq, 
        decl_context_t decl_context)
{
    AST it;
    for_each_element(program_unit_seq, it)
    {
        build_scope_program_unit(ASTSon1(it), decl_context);
    }
}

static void build_scope_main_program_unit(AST program_unit, decl_context_t program_unit_context);
static void build_scope_subroutine_program_unit(AST program_unit, decl_context_t program_unit_context);
static void build_scope_function_program_unit(AST program_unit, decl_context_t program_unit_context);
static void build_scope_module_program_unit(AST program_unit, decl_context_t program_unit_context);
static void build_scope_block_data_program_unit(AST program_unit, decl_context_t program_unit_context);

static void build_scope_program_unit(AST program_unit, 
        decl_context_t decl_context)
{

    switch (ASTType(program_unit))
    {
        case AST_MAIN_PROGRAM_UNIT:
            {
                build_scope_main_program_unit(program_unit, decl_context);
                break;
            }
        case AST_SUBROUTINE_PROGRAM_UNIT:
            {
                build_scope_subroutine_program_unit(program_unit, decl_context);
                break;
            }
        case AST_FUNCTION_PROGRAM_UNIT:
            {
                build_scope_function_program_unit(program_unit, decl_context);
                break;
            }
        case AST_MODULE_PROGRAM_UNIT :
            {
                build_scope_module_program_unit(program_unit, decl_context);
                break;
            }
        case AST_BLOCK_DATA_PROGRAM_UNIT:
            {
                build_scope_block_data_program_unit(program_unit, decl_context);
                break;
            }
        default:
            {
                internal_error("Unhandled node type '%s'\n", ast_print_node_type(ASTType(program_unit)));
            }
    }
}

static void build_scope_program_body(AST program_body, decl_context_t decl_context);

static void build_scope_main_program_unit(AST program_unit, decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "==== [%s] Program unit: PROGRAM ===\n", ast_location(program_unit));
    }
    decl_context_t program_unit_context = new_block_context(decl_context);

    AST program_stmt = ASTSon0(program_unit);
    const char * program_name = "__MAIN__";
    if (program_stmt != NULL)
    {
        AST name = ASTSon0(program_stmt);
        program_name = ASTText(name);
    }
    scope_entry_t* program_sym = new_symbol(decl_context, decl_context.current_scope, program_name);
    program_sym->kind = SK_PROGRAM;
    program_sym->file = ASTFileName(program_unit);
    program_sym->line = ASTLine(program_unit);
    program_sym->related_decl_context = program_unit_context;

    AST program_body = ASTSon1(program_unit);
    if (program_body == NULL)
        return;

    build_scope_program_body(program_body, program_unit_context);
}

static void build_scope_subroutine_program_unit(AST program_unit, decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "==== [%s] Program unit: SUBROUTINE ===\n", ast_location(program_unit));
    }
    decl_context_t program_unit_context = new_block_context(decl_context);

    AST program_body = ASTSon1(program_unit);
    if (program_body == NULL)
        return;

    build_scope_program_body(program_body, program_unit_context);
}

static void build_scope_function_program_unit(AST program_unit, decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "==== [%s] Program unit: FUNCTION ===\n", ast_location(program_unit));
    }
    decl_context_t program_unit_context = new_block_context(decl_context);

    AST program_body = ASTSon1(program_unit);
    if (program_body == NULL)
        return;

    build_scope_program_body(program_body, program_unit_context);
}

static void build_scope_module_program_unit(AST program_unit, decl_context_t decl_context UNUSED_PARAMETER)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "==== [%s] Program unit: MODULE ===\n", ast_location(program_unit));
    }
    // decl_context_t program_unit_context = new_block_context(decl_context);
    fprintf(stderr, "%s: sorry: MODULE program units not yet implemented\n", 
            ast_location(program_unit));
}

static void build_scope_block_data_program_unit(AST program_unit,
        decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing with these
    DEBUG_CODE()
    {
        fprintf(stderr, "=== [%s] Program unit: BLOCK DATA ===\n", ast_location(program_unit));
    }
    // decl_context_t program_unit_context = new_block_context(decl_context);
}

static void build_scope_program_part(AST program_part, decl_context_t decl_context);

static void build_scope_program_body(AST program_body, decl_context_t decl_context)
{
    AST program_part = ASTSon0(program_body);
    AST internal_subprograms = ASTSon1(program_body);

    if (internal_subprograms != NULL)
    {
        // We need to pre-register the names of the subprograms
    }

    build_scope_program_part(program_part, decl_context);
}

typedef void (*build_scope_statement_function_t)(AST statement, decl_context_t);
typedef struct build_scope_statement_handler_tag
{
    node_t ast_kind;
    build_scope_statement_function_t handler;
} build_scope_statement_handler_t;

#define STATEMENT_HANDLER_TABLE \
 STATEMENT_HANDLER(AST_ACCESS_STATEMENT, build_scope_access_stmt) \
 STATEMENT_HANDLER(AST_ALLOCATABLE_STATEMENT, build_scope_allocatable_stmt) \
 STATEMENT_HANDLER(AST_ALLOCATE_STATEMENT, build_scope_allocate_stmt) \
 STATEMENT_HANDLER(AST_ALL_STOP_STATEMENT, build_scope_allstop_stmt) \
 STATEMENT_HANDLER(AST_ARITHMETIC_IF_STATEMENT, build_scope_arithmetic_if_stmt) \
 STATEMENT_HANDLER(AST_EXPRESSION_STATEMENT, build_scope_expression_stmt) \
 STATEMENT_HANDLER(AST_ASSOCIATE_CONSTRUCT, build_scope_associate_construct) \
 STATEMENT_HANDLER(AST_ASYNCHRONOUS_STATEMENT, build_scope_asynchronous_stmt) \
 STATEMENT_HANDLER(AST_IO_STATEMENT, build_io_stmt) \
 STATEMENT_HANDLER(AST_BINDING_STATEMENT, build_scope_bind_stmt) \
 STATEMENT_HANDLER(AST_BLOCK_CONSTRUCT, build_scope_block_construct) \
 STATEMENT_HANDLER(AST_SELECT_CASE_CONSTRUCT, build_scope_case_construct) \
 STATEMENT_HANDLER(AST_CLOSE_STATEMENT, build_scope_close_stmt) \
 STATEMENT_HANDLER(AST_CODIMENSION_STATEMENT, build_scope_codimension_stmt) \
 STATEMENT_HANDLER(AST_COMMON_STATEMENT, build_scope_common_stmt) \
 STATEMENT_HANDLER(AST_COMPUTED_GOTO_STATEMENT, build_scope_computed_goto_stmt) \
 STATEMENT_HANDLER(AST_ASSIGNED_GOTO_STATEMENT, build_scope_assigned_goto_stmt) \
 STATEMENT_HANDLER(AST_LABEL_ASSIGN_STATEMENT, build_scope_label_assign_stmt) \
 STATEMENT_HANDLER(AST_EMPTY_STATEMENT, build_scope_continue_stmt) \
 STATEMENT_HANDLER(AST_CRITICAL_CONSTRUCT, build_scope_critical_construct) \
 STATEMENT_HANDLER(AST_CONTINUE_STATEMENT, build_scope_cycle_stmt) \
 STATEMENT_HANDLER(AST_DATA_STATEMENT, build_scope_data_stmt) \
 STATEMENT_HANDLER(AST_DEALLOCATE_STATEMENT, build_scope_deallocate_stmt) \
 STATEMENT_HANDLER(AST_DERIVED_TYPE_DEF, build_scope_derived_type_def) \
 STATEMENT_HANDLER(AST_DIMENSION_STATEMENT, build_scope_dimension_stmt) \
 STATEMENT_HANDLER(AST_FOR_STATEMENT, build_scope_do_construct) \
 STATEMENT_HANDLER(AST_ENTRY_STATEMENT, build_scope_entry_stmt) \
 STATEMENT_HANDLER(AST_ENUM_DEF, build_scope_enum_def) \
 STATEMENT_HANDLER(AST_EQUIVALENCE_STATEMENT, build_scope_equivalence_stmt) \
 STATEMENT_HANDLER(AST_BREAK_STATEMENT, build_scope_exit_stmt) \
 STATEMENT_HANDLER(AST_EXTERNAL_STATEMENT, build_scope_external_stmt) \
 STATEMENT_HANDLER(AST_FORALL_CONSTRUCT, build_scope_forall_construct) \
 STATEMENT_HANDLER(AST_FORALL_STATEMENT, build_scope_forall_stmt) \
 STATEMENT_HANDLER(AST_FORMAT_STATEMENT, build_scope_format_stmt) \
 STATEMENT_HANDLER(AST_GOTO_STATEMENT, build_scope_goto_stmt) \
 STATEMENT_HANDLER(AST_IF_ELSE_STATEMENT, build_scope_if_construct) \
 STATEMENT_HANDLER(AST_IMPLICIT_STATEMENT, build_scope_implicit_stmt) \
 STATEMENT_HANDLER(AST_IMPORT_STATEMENT, build_scope_import_stmt) \
 STATEMENT_HANDLER(AST_INTENT_STATEMENT, build_scope_intent_stmt) \
 STATEMENT_HANDLER(AST_INTERFACE_BLOCK, build_scope_interface_block) \
 STATEMENT_HANDLER(AST_INTRINSIC_STATEMENT, build_scope_intrinsic_stmt) \
 STATEMENT_HANDLER(AST_LOCK_STATEMENT, build_scope_lock_stmt) \
 STATEMENT_HANDLER(AST_NAMELIST_STATEMENT, build_scope_namelist_stmt) \
 STATEMENT_HANDLER(AST_NULLIFY_STATEMENT, build_scope_nullify_stmt) \
 STATEMENT_HANDLER(AST_OPEN_STATEMENT, build_scope_open_stmt) \
 STATEMENT_HANDLER(AST_OPTIONAL_STATEMENT, build_scope_optional_stmt) \
 STATEMENT_HANDLER(AST_PARAMETER_STATEMENT, build_scope_parameter_stmt) \
 STATEMENT_HANDLER(AST_POINTER_STATEMENT, build_scope_pointer_stmt) \
 STATEMENT_HANDLER(AST_PRINT_STATEMENT, build_scope_print_stmt) \
 STATEMENT_HANDLER(AST_PROCEDURE_DECL_STATEMENT, build_scope_procedure_declaration_stmt) \
 STATEMENT_HANDLER(AST_PROTECTED_STATEMENT, build_scope_protected_stmt) \
 STATEMENT_HANDLER(AST_READ_STATEMENT, build_scope_read_stmt) \
 STATEMENT_HANDLER(AST_RETURN_STATEMENT, build_scope_return_stmt) \
 STATEMENT_HANDLER(AST_SAVE_STATEMENT, build_scope_save_stmt) \
 STATEMENT_HANDLER(AST_SELECT_TYPE_CONSTRUCT, build_scope_select_type_construct) \
 STATEMENT_HANDLER(AST_STATEMENT_FUNCTION_STATEMENT, build_scope_stmt_function_stmt) \
 STATEMENT_HANDLER(AST_STOP_STATEMENT, build_scope_stop_stmt) \
 STATEMENT_HANDLER(AST_PAUSE_STATEMENT, build_scope_pause_stmt) \
 STATEMENT_HANDLER(AST_SYNC_ALL_STATEMENT, build_scope_sync_all_stmt) \
 STATEMENT_HANDLER(AST_SYNC_IMAGES_STATEMENT, build_scope_sync_images_stmt) \
 STATEMENT_HANDLER(AST_SYNC_MEMORY_STATEMENT, build_scope_sync_memory_stmt) \
 STATEMENT_HANDLER(AST_TARGET_STATEMENT, build_scope_target_stmt) \
 STATEMENT_HANDLER(AST_DECLARATION_STATEMENT, build_scope_type_declaration_stmt) \
 STATEMENT_HANDLER(AST_UNLOCK_STATEMENT, build_scope_unlock_stmt) \
 STATEMENT_HANDLER(AST_USE_STATEMENT, build_scope_use_stmt) \
 STATEMENT_HANDLER(AST_VALUE_STATEMENT, build_scope_value_stmt) \
 STATEMENT_HANDLER(AST_VOLATILE_STATEMENT, build_scope_volatile_stmt) \
 STATEMENT_HANDLER(AST_WAIT_STATEMENT, build_scope_wait_stmt) \
 STATEMENT_HANDLER(AST_WHERE_CONSTRUCT, build_scope_where_construct) \
 STATEMENT_HANDLER(AST_WHERE_STATEMENT, build_scope_where_stmt) \
 STATEMENT_HANDLER(AST_WRITE_STATEMENT, build_scope_write_stmt)

// Prototypes
#define STATEMENT_HANDLER(_kind, _handler) \
    static void _handler(AST, decl_context_t);
STATEMENT_HANDLER_TABLE
#undef STATEMENT_HANDLER

// Table
#define STATEMENT_HANDLER(_kind, _handler) \
   { .ast_kind = _kind, .handler = _handler },
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

static void build_scope_program_part(AST program_part, decl_context_t decl_context)
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

    AST it;
    for_each_element(program_part, it)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "=== [%s] Statement ===\n", ast_location(program_part));
        }
        AST statement = ASTSon1(it);

        build_scope_statement_handler_t key = { .ast_kind = ASTType(statement) };
        build_scope_statement_handler_t *handler = NULL;

        // void *bsearch(const void *key, const void *base,
        //       size_t nmemb, size_t size,
        //       int (*compar)(const void *, const void *));
        handler = (build_scope_statement_handler_t*)bsearch(&key, build_scope_statement_function, 
                sizeof(build_scope_statement_function) / sizeof(build_scope_statement_function[0]),
                sizeof(build_scope_statement_function[0]),
                build_scope_statement_function_compare);
        if (handler == NULL 
                || handler->handler == NULL)
        {
            fprintf(stderr, "%s: sorry: unhandled statement\n", ast_location(statement));
        }
        else
        {
            (handler->handler)(statement, decl_context);
        }
    }
}

const char* get_name_of_generic_spec(AST generic_spec)
{
    switch (ASTType(generic_spec))
    {
        case AST_SYMBOL:
        case AST_OPERATOR_NAME:
            {
                return ASTText(generic_spec);
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


static int compute_kind_specifier(AST kind_expr, decl_context_t decl_context UNUSED_PARAMETER)
{
    fprintf(stderr, "%s: warning: KIND not implemented yet, defaulting to 4\n", ast_location(kind_expr));
    return 4;
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
static type_t* choose_logical_type_from_kind(AST expr, int kind_size)
{
    if (!logical_types_init)
    {
        int_types[type_get_size(get_signed_long_long_int_type())] = get_bool_of_integer_type(get_signed_long_long_int_type());
        int_types[type_get_size(get_signed_long_int_type())] = get_bool_of_integer_type(get_signed_long_int_type());
        int_types[type_get_size(get_signed_int_type())] = get_bool_of_integer_type(get_signed_int_type());
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

    scope_entry_list_t* entry_list = query_id_expression(decl_context, name);
    if (entry_list == NULL)
        return NULL;

    scope_entry_t* entry = entry_list_head(entry_list);

    type_t* result = NULL;
    if (entry->kind == SK_CLASS)
    {
        result = get_user_defined_type(entry);
    }
    entry_list_free(entry_list);

    return result;
}

static type_t* gather_type_from_declaration_type_spec(AST a, decl_context_t decl_context)
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
                type_t* element_type = gather_type_from_declaration_type_spec(ASTSon0(a), decl_context);
                result = get_complex_type(element_type);
                break;
            }
        case AST_CHARACTER_TYPE:
            {
                result = get_unsigned_char_type();
                AST char_selector = ASTSon0(a);
                AST len = NULL;
                AST kind = NULL;
                if (char_selector != NULL)
                {
                    len = ASTSon0(char_selector);
                    kind = ASTSon1(char_selector);
                }

                // Well, we cannot default to a kind of 4 because it'd be weird, so we simply ignore the kind
                if (kind != NULL)
                {
                    fprintf(stderr, "%s: warning: KIND of CHARACTER ignored, defaulting to 1\n",
                            ast_location(a));
                }
                if (len != NULL)
                {
                    AST lower_bound = ASTLeaf(AST_DECIMAL_LITERAL, ASTFileName(len), ASTLine(len), "1");
                    result = get_array_type_bounds(result, lower_bound, len, decl_context);
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
                break;
            }
        case AST_VECTOR_TYPE:
            {
                type_t* element_type = gather_type_from_declaration_type_spec(ASTSon0(a), decl_context);
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
ATTR_SPEC_HANDLER(volatile) 

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

                if (handler == NULL)
                {
                    internal_error("Unhandled handler of '%s'\n", ASTText(attr_spec_item));
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
    AST it;
    for_each_element(access_id_list, it)
    {
        AST access_id = ASTSon1(it);

        const char* name = get_name_of_generic_spec(access_id);

        scope_entry_t* sym = query_name(decl_context, name);

        if (sym == NULL)
        {
            running_error("%s: unknown entity '%s'\n", 
                    ast_location(access_id),
                    name);
        }

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

    scope_entry_t* entry = query_name(decl_context, ASTText(name));

    if (entry== NULL
            || entry->kind != SK_VARIABLE)
    {
        running_error("%s: error: invalid entity '%s' in dimension declaration\n", 
                ast_location(a),
                name);
    }
    
    if (is_array_type(entry->type_information)
            || is_pointer_to_void_type(entry->type_information))
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

        scope_entry_t* entry = query_name(decl_context, ASTText(name));

        if (entry == NULL
                || entry->kind != SK_VARIABLE)
        {
            running_error("%s: error: invalid entity '%s' in ALLOCATABLE clause\n", 
                    ast_location(name), ASTText(name));
        }

        if (!is_array_type(entry->type_information)
                && !is_pointer_to_array(entry->type_information))
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
}

static void build_scope_allocate_stmt(AST a, decl_context_t decl_context)
{
    AST type_spec = ASTSon0(a);
    AST allocation_list = ASTSon1(a);
    // AST alloc_opt_list = ASTSon2(a);

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
}

static void unsupported_statement(AST a, const char* name)
{
    running_error("%s: sorry: %s statement not supported\n", 
            ast_location(a),
            name);
}

#if 0
static void unsupported_construct(AST a, const char* name)
{
    running_error("%s: sorry: %s construct not supported\n", 
            ast_location(a),
            name);
}
#endif

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
    if (!check_for_expression(expr, decl_context)
            && CURRENT_CONFIGURATION->strict_typecheck)
    {
        internal_error("Could not check expression '%s' at '%s'\n",
                fortran_prettyprint_in_buffer(ASTSon0(a)),
                ast_location(ASTSon0(a)));
    }

    if (expression_get_type(expr) != NULL)
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

static void build_io_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    running_error("%s: error: not yet implemented\n",
            ast_location(a));
}

static void build_scope_bind_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing at the moment
}

static void build_scope_block_construct(AST a, decl_context_t decl_context)
{
    decl_context_t new_context = new_block_context(decl_context);
    build_scope_program_part(ASTSon1(a), new_context);
}

static void build_scope_case_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    // FIXME - Adapt tree to that of C
}

static void build_scope_close_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_codimension_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "CODIMENSION");
}

static void build_scope_common_stmt(AST a, decl_context_t decl_context)
{
    AST common_block_item_list = ASTSon0(a);

    AST it;
    for_each_element(common_block_item_list, it)
    {
        AST common_block_item = ASTSon1(it);

        AST common_name = ASTSon0(common_block_item);
        AST common_block_object_list = ASTSon1(common_block_item);
        
        const char *common_name_str = ".common.unnamed";
        if (common_name != NULL)
        {
            common_name_str = strappend(".common.",
                    ASTText(common_name));
        }
        scope_entry_t* common_sym = query_name(decl_context, common_name_str);
        if (common_sym == NULL)
        {
            common_sym = new_symbol(decl_context, decl_context.current_scope, common_name_str);
            common_sym->kind = SK_COMMON;
            common_sym->file = ASTFileName(a);
            common_sym->line = ASTLine(a);
        }

        AST it2;
        for_each_element(common_block_object_list, it2)
        {
            AST common_block_object = ASTSon1(it2);

            AST name = NULL;
            if (ASTType(common_block_object) == AST_SYMBOL)
            {
                name = common_block_object;
            }
            else if (ASTType(common_block_item) == AST_DIMENSION_DECL)
            {
                name = ASTSon0(common_block_object);
            }

            scope_entry_t* sym = query_name(decl_context, ASTText(name));
            if (sym == NULL)
            {
                running_error("%s: error: unknown entity '%s'\n", 
                        ast_location(name),
                        fortran_prettyprint_in_buffer(name));
            }

            if (sym->entity_specs.is_in_common)
            {
                running_error("%s: error: entity '%s' is already in a COMMON\n", 
                        ast_location(name),
                        sym->symbol_name);
            }

            sym->entity_specs.is_in_common = 1;
            sym->entity_specs.in_common = common_sym;
        }
    }
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

static void build_scope_continue_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing for continue
}

static void build_scope_critical_construct(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "CRITICAL");
}

static void build_scope_cycle_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing for cycle
}

static void build_scope_data_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    // Do nothing for DATA
}

static void build_scope_deallocate_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_derived_type_def(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_dimension_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_do_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_entry_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_enum_def(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_equivalence_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_exit_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_external_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_forall_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_forall_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_format_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_goto_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_if_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_implicit_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_import_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_intent_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_interface_block(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_intrinsic_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_lock_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_namelist_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_nullify_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_open_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_optional_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_parameter_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_pointer_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_print_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_procedure_declaration_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_protected_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_read_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_return_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_save_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_select_type_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_stmt_function_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_stop_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_pause_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_sync_all_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_sync_images_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_sync_memory_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_target_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
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
        AST declaration = ASTSon1(it);

        AST name = ASTSon0(declaration);
        AST entity_decl_specs = ASTSon1(declaration);

        // We do not use 'query_name' because we do not want to use the
        // implicit information of the current scope
        scope_entry_list_t* entry_list = query_unqualified_name_str(decl_context, ASTText(name));
        scope_entry_t* entry = NULL;
        if (entry_list != NULL)
        {
            entry = entry_list_head(entry_list);
            if (!entry->entity_specs.is_implicit)
            {
                running_error("%s: error: redeclaration of entity '%s', first declared at '%s:%d'\n",
                        ast_location(declaration),
                        entry->file,
                        entry->line);
            }
            else
            {
                // Not implicit anymore
                entry->entity_specs.is_implicit = 0;
            }
        }
        else
        {
            entry = new_symbol(decl_context, decl_context.current_scope, ASTText(name));
            entry->kind = SK_VARIABLE;
            entry->file = ASTFileName(declaration);
            entry->line = ASTLine(declaration);
        }

        entry->type_information = basic_type;

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
                if (attr_spec.is_dimension)
                {
                    running_error("%s: error: DIMENSION attribute specified twice\n", ast_location(declaration));
                }
                attr_spec.is_dimension = 1;
                attr_spec.array_spec = array_spec;
            }

            if (coarray_spec != NULL)
            {
                if (attr_spec.is_codimension)
                {
                    running_error("%s: error: CODIMENSION attribute specified twice\n", ast_location(declaration));
                }
                attr_spec.is_codimension = 1;
                attr_spec.coarray_spec = coarray_spec;
            }

            if (char_length != NULL)
            {
                if (!is_character_type(entry->type_information))
                {
                    running_error("%s: error: char-length specified but type is not CHARACTER\n", ast_location(declaration));
                }

                AST lower_bound = ASTLeaf(AST_DECIMAL_LITERAL, ASTFileName(char_length), ASTLine(char_length), "1");
                entry->type_information = get_array_type_bounds(entry->type_information, lower_bound, char_length, decl_context);
            }
        }

        // Stop the madness here
        if (attr_spec.is_codimension)
        {
            running_error("%s: sorry: coarrays are not supported\n", ast_location(declaration));
        }

        if (attr_spec.is_dimension)
        {
            type_t* array_type = compute_type_from_array_spec(entry->type_information, 
                    attr_spec.array_spec,
                    decl_context,
                    /* array_spec_kind */ NULL);
            entry->type_information = array_type;
        }

        if (attr_spec.is_constant)
        {
            if (initialization == NULL)
            {
                running_error("%s: error: PARAMETER is missing an initializer\n", ast_location(declaration));
            }
            entry->type_information = get_const_qualified_type(entry->type_information);
            entry->expression_value = initialization;
        }

        // FIXME - Should we do something with this attribute?
        if (attr_spec.is_value)
        {
            if (!entry->entity_specs.is_parameter)
            {
                running_error("%s: error: VALUE attribute is only for dummy arguments\n",
                        ast_location(declaration));
            }
        }

        if (attr_spec.is_intent)
        {
            if (!entry->entity_specs.is_parameter)
            {
                running_error("%s: error: INTENT attribute is only for dummy arguments\n",
                        ast_location(declaration));
            }
        }
        else
        {
            entry->entity_specs.intent_kind = attr_spec.intent_kind;
        }

        if (attr_spec.is_optional)
        {
            if (!entry->entity_specs.is_parameter)
            {
                running_error("%s: error: OPTIONAL attribute is only for dummy arguments\n",
                        ast_location(declaration));
            }
        }

        if (attr_spec.is_allocatable)
        {
            if (!attr_spec.is_dimension)
            {
                running_error("%s: error: ALLOCATABLE attribute cannot be used on scalars\n", 
                        ast_location(declaration));
            }
            entry->entity_specs.is_allocatable = 1;
        }

        if (attr_spec.is_external
                || attr_spec.is_intrinsic)
        {
            entry->kind = SK_FUNCTION;
            entry->type_information = get_nonproto_function_type(entry->type_information, 0);
        }

        if (attr_spec.is_save)
        {
            entry->entity_specs.is_static = 1;
        }

        if (!attr_spec.is_constant)
        {
            if (initialization != NULL)
            {
                entry->entity_specs.is_static = 1;
                entry->expression_value = initialization;
            }
        }

        if (attr_spec.is_pointer)
        {
            entry->type_information = get_pointer_type(entry->type_information);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "Type of symbol '%s' is '%s'\n", entry->symbol_name, print_declarator(entry->type_information));
        }
    }
}

static void build_scope_unlock_stmt(AST a, decl_context_t decl_context UNUSED_PARAMETER)
{
    unsupported_statement(a, "UNLOCK");
}

static void build_scope_use_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_value_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_volatile_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_wait_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_where_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_where_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void build_scope_write_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}
