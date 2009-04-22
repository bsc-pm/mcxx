#include "cxx-upc.h"

#include "cxx-scope.h"
#include "cxx-typeutils.h"
#include "cxx-driver.h"
#include "cxx-buildscope.h"

void upc_sign_in_builtins(decl_context_t decl_context)
{
    // THREADS
    scope_entry_t* upc_THREADS;

    upc_THREADS = new_symbol(decl_context, decl_context.global_scope, "THREADS");
    upc_THREADS->kind = SK_VARIABLE;
    upc_THREADS->type_information = get_const_qualified_type(get_signed_int_type());
    upc_THREADS->defined = 1;
    upc_THREADS->do_not_print = 1;
    upc_THREADS->file = "(global scope)";
    upc_THREADS->entity_specs.is_builtin = 1;
    if (CURRENT_CONFIGURATION(upc_threads) != NULL)
    {
        upc_THREADS->expression_value = internal_expression_parse(CURRENT_CONFIGURATION(upc_threads), decl_context);
    }

    // MYTHREAD
    scope_entry_t* upc_MYTHREAD;

    upc_MYTHREAD = new_symbol(decl_context, decl_context.global_scope, "MYTHREAD");
    upc_MYTHREAD->kind = SK_VARIABLE;
    upc_MYTHREAD->type_information = get_const_qualified_type(get_signed_int_type());
    upc_MYTHREAD->defined = 1;
    upc_MYTHREAD->do_not_print = 1;
    upc_MYTHREAD->file = "(global scope)";
    upc_MYTHREAD->entity_specs.is_builtin = 1;
    
    // UPC_MAX_BLOCK_SIZE
    scope_entry_t* upc_UPC_MAX_BLOCK_SIZE;

    upc_UPC_MAX_BLOCK_SIZE = new_symbol(decl_context, decl_context.global_scope, "UPC_MAX_BLOCK_SIZE");
    upc_UPC_MAX_BLOCK_SIZE->kind = SK_VARIABLE;
    upc_UPC_MAX_BLOCK_SIZE->type_information = get_const_qualified_type(get_signed_int_type());
    upc_UPC_MAX_BLOCK_SIZE->defined = 1;
    upc_UPC_MAX_BLOCK_SIZE->do_not_print = 1;
    upc_UPC_MAX_BLOCK_SIZE->file = "(global scope)";
    upc_UPC_MAX_BLOCK_SIZE->entity_specs.is_builtin = 1;

    // upc_lock_t
    scope_entry_t* upc_lock_t;

    upc_UPC_MAX_BLOCK_SIZE = new_symbol(decl_context, decl_context.global_scope, "upc_lock_t");
    upc_UPC_MAX_BLOCK_SIZE->kind = SK_TYPEDEF;
    upc_UPC_MAX_BLOCK_SIZE->defined = 1;
    upc_UPC_MAX_BLOCK_SIZE->type_information = get_new_typedef(get_void_type());
    upc_UPC_MAX_BLOCK_SIZE->do_not_print = 1;
    upc_UPC_MAX_BLOCK_SIZE->file = "(global scope)";
    upc_UPC_MAX_BLOCK_SIZE->entity_specs.is_builtin = 1;
}

