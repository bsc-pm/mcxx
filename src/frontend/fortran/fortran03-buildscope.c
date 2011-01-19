#include "fortran03-buildscope.h"
#include "fortran03-exprtype.h"
#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-scopelink.h"
#include "cxx-utils.h"

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
    decl_context_t program_unit_context = new_block_context(decl_context);

    switch (ASTType(program_unit))
    {
        case AST_MAIN_PROGRAM_UNIT:
            {
                build_scope_main_program_unit(program_unit, program_unit_context);
                break;
            }
        case AST_SUBROUTINE_PROGRAM_UNIT:
            {
                build_scope_subroutine_program_unit(program_unit, program_unit_context);
                break;
            }
        case AST_FUNCTION_PROGRAM_UNIT:
            {
                build_scope_function_program_unit(program_unit, program_unit_context);
                break;
            }
        case AST_MODULE_PROGRAM_UNIT :
            {
                build_scope_module_program_unit(program_unit, program_unit_context);
                break;
            }
        case AST_BLOCK_DATA_PROGRAM_UNIT:
            {
                build_scope_block_data_program_unit(program_unit, program_unit_context);
                break;
            }
        default:
            {
                internal_error("Unhandled node type '%s'\n", ast_print_node_type(ASTType(program_unit)));
            }
    }
}

static void build_scope_program_body(AST program_body, decl_context_t decl_context);

static void build_scope_main_program_unit(AST program_unit, decl_context_t program_unit_context)
{
    AST program_body = ASTSon1(program_unit);
    if (program_body == NULL)
        return;

    build_scope_program_body(program_body, program_unit_context);
}

static void build_scope_subroutine_program_unit(AST program_unit, decl_context_t program_unit_context)
{
    AST program_body = ASTSon1(program_unit);
    if (program_body == NULL)
        return;

    build_scope_program_body(program_body, program_unit_context);
}

static void build_scope_function_program_unit(AST program_unit, decl_context_t program_unit_context)
{
    AST program_body = ASTSon1(program_unit);
    if (program_body == NULL)
        return;

    build_scope_program_body(program_body, program_unit_context);
}

static void build_scope_module_program_unit(AST program_unit, decl_context_t program_unit_context UNUSED_PARAMETER)
{
    fprintf(stderr, "%s: sorry: MODULE program units not yet implemented\n", 
            ast_location(program_unit));
}

static void build_scope_block_data_program_unit(AST program_unit UNUSED_PARAMETER,
        decl_context_t program_unit_context UNUSED_PARAMETER)
{
    // Do nothing with these
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

void build_scope_access_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_allocatable_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_allocate_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_allstop_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_arithmetic_if_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_expression_stmt(AST a, decl_context_t decl_context)
{
    fortran_check_expression(a, decl_context);
}

void build_scope_associate_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_asynchronous_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_io_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_bind_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_block_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_case_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_close_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_codimension_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_common_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_computed_goto_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_assigned_goto_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_label_assign_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_continue_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_critical_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_cycle_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_data_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_deallocate_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_derived_type_def(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_dimension_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_do_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_entry_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_enum_def(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_equivalence_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_exit_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_external_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_forall_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_forall_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_format_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_goto_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_if_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_implicit_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_import_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_intent_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_interface_block(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_intrinsic_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_lock_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_namelist_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_nullify_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_open_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_optional_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_parameter_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_pointer_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_print_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_procedure_declaration_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_protected_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_read_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_return_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_save_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_select_type_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_stmt_function_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_stop_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_pause_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_sync_all_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_sync_images_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_sync_memory_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_target_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_type_declaration_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_unlock_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_use_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_value_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_volatile_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_wait_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_where_construct(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_where_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

void build_scope_write_stmt(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}
