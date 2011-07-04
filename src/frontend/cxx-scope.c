/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-driver.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-solvetemplate.h"
#include "cxx-instantiation.h"
#include "cxx-prettyprint.h"
#include "cxx-cexpr.h"
#include "cxx-ambiguity.h"
#include "cxx-exprtype.h"
#include "cxx-buildscope.h"
#include "cxx-overload.h"
#include "cxx-tltype.h"
#include "cxx-attrnames.h"
#include "cxx-printscope.h"
#include "cxx-codegen.h"
#include "cxx-entrylist.h"
#include "red_black_tree.h"

static unsigned long long _bytes_used_scopes = 0;

unsigned long long scope_used_memory(void)
{
    return _bytes_used_scopes;
}

static unsigned long long _bytes_used_symbols = 0;

unsigned long long symbols_used_memory(void)
{
    return _bytes_used_symbols;
}

// Lookup of a simple name within a given declaration context
static scope_entry_list_t* name_lookup(decl_context_t decl_context, const char* name, 
        const char* filename, int line);

template_parameter_list_t* duplicate_template_argument_list(template_parameter_list_t* template_parameters)
{
    template_parameter_list_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);

    *result = *template_parameters;
    result->arguments = counted_calloc(template_parameters->num_parameters, sizeof(*result->arguments), &_bytes_used_scopes);

    int i;
    for (i = 0; i < result->num_parameters; i++)
    {
        // Copy pointers
        result->arguments[i] = template_parameters->arguments[i];
    }

    return result;
}

// Solve a template given a template-id, a list of found names for the template-id and the declaration context

// Looks up the qualification scope for a nested-name-spec
static decl_context_t lookup_qualification_scope(
        decl_context_t original_context,
        decl_context_t decl_context, 
        AST nested_name, 
        AST unqualifed_part, 
        type_t** dependent_type,
        char *is_valid);

static scope_entry_list_t* query_template_id(AST template_id, 
        decl_context_t template_name_context,
        decl_context_t template_parameters_context);

static scope_entry_list_t* query_qualified_name(decl_context_t decl_context,
        AST global_op,
        AST nested_name,
        AST unqualified_name);

// Scope creation functions
static scope_t* new_namespace_scope(scope_t* st, scope_entry_t* related_entry);
static scope_t* new_prototype_scope(scope_t* st);
static scope_t* new_block_scope(scope_t* enclosing_scope);
static scope_t* new_class_scope(scope_t* enclosing_scope, scope_entry_t* class_entry);

/* Scope creation functions */
/*
 * There was a time when the compiler worked directly with scopes instead
 * of declarative context, that these were the functions used
 * to create them.
 */

static int strcmp_vptr(const void* v1, const void *v2)
{
    return strcmp((const char*)v1, (const char*) v2);
}

static void null_dtor_func(const void *v UNUSED_PARAMETER) { }

// Any new scope should be created using this one
scope_t* _new_scope(void)
{
    scope_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);

    result->hash =
        rb_tree_create(strcmp_vptr, null_dtor_func, null_dtor_func);

    return result;
}

// Creates a new namespace scope and optionally it gives it a
// related_entry->symbol_name. Global scope has st == NULL and qualification_name == NULL
static scope_t* new_namespace_scope(scope_t* st, scope_entry_t* related_entry)
{
    scope_t* result = _new_scope();

    result->kind = NAMESPACE_SCOPE;
    result->contained_in = st;
    result->related_entry = related_entry;

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: New namespace scope '%p' (qualification='%s') created\n", 
                result, result->related_entry != NULL ? result->related_entry->symbol_name : "");
    }

    return result;
}

// Prototype scope, it is used only in function declarations (but not in function
// definitions where parameters go in a block scope
static scope_t* new_prototype_scope(scope_t* enclosing_scope)
{
    scope_t* result = _new_scope();

    result->kind = PROTOTYPE_SCOPE;
    result->contained_in = enclosing_scope;

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: New prototype scope '%p' created\n", result);
    }

    return result;
}

// Creates a new block scope. In the standard this is a local scope and it is
// the scope implicitly created by a function_definition or a compound
// statement
static scope_t* new_block_scope(scope_t* enclosing_scope)
{
    scope_t* result = _new_scope();

    result->kind = BLOCK_SCOPE;
    result->contained_in = enclosing_scope;

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: New block scope '%p' created\n", result);
    }
    
    return result;
}

// Creates a new function scope. This is the scope where labels (goto) are signed up.
// Should not be used too much :)
static scope_t* new_function_scope(void)
{
    scope_t* result = _new_scope();
    
    result->kind = FUNCTION_SCOPE;

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: New function scope '%p' created\n", result);
    }
    
    return result;
}

// Creates a new class scope and optionally it is given a qualification name
static scope_t* new_class_scope(scope_t* enclosing_scope, scope_entry_t* class_entry)
{
    scope_t* result = _new_scope();

    result->kind = CLASS_SCOPE;
    result->contained_in = enclosing_scope;

    ERROR_CONDITION(class_entry == NULL,
            "Related class can't be null", 0);
    result->related_entry = class_entry;

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: New class scope '%p' (qualif=%s) scope created\n",  
                result, class_entry->symbol_name);
    }
    
    return result;
}

/*
 * Decl context creation functions. Use it in cxx-buildscope.c
 * to properly create contexts
 */

// Creates an anew decl_context_t
static decl_context_t new_decl_context(void)
{
    decl_context_t result;
    memset(&result, 0, sizeof(result));

    return result;
}

// Creates a new global context
decl_context_t new_global_context(void)
{
    decl_context_t result = new_decl_context();

    scope_entry_t* global_scope_namespace 
        = counted_calloc(1, sizeof(*global_scope_namespace), &_bytes_used_scopes);
    global_scope_namespace->kind = SK_NAMESPACE;

    // Create global scope
    result.namespace_scope = new_namespace_scope(NULL, global_scope_namespace);

    // Make it the global one
    result.global_scope = result.namespace_scope;
    // and the current one
    result.current_scope = result.namespace_scope;

    global_scope_namespace->related_decl_context = result;
    global_scope_namespace->decl_context = result;

    return result;
}

decl_context_t new_namespace_context(decl_context_t enclosing_decl_context, 
        scope_entry_t* related_entry)
{
    ERROR_CONDITION((enclosing_decl_context.current_scope->kind != NAMESPACE_SCOPE),
            "Enclosing scope must be namespace scope", 0);
    ERROR_CONDITION((enclosing_decl_context.current_scope != enclosing_decl_context.namespace_scope), 
            "Enclosing namespace scope has a mismatch between its current scope and its namespace scope", 0);

    // Inherit current context
    decl_context_t result = enclosing_decl_context;

    // Create a new namespace scope contained in the previous one
    result.namespace_scope = new_namespace_scope(enclosing_decl_context.namespace_scope, 
            related_entry);
    // And make it the current one
    result.current_scope = result.namespace_scope;

    return result;
}

decl_context_t new_prototype_context(decl_context_t enclosing_decl_context)
{
    // Inherit current context
    decl_context_t result = enclosing_decl_context;

    // Create the prototype scope
    result.prototype_scope = new_prototype_scope(enclosing_decl_context.current_scope);
    // and make it the current scope
    result.current_scope = result.prototype_scope;

    return result;
}


decl_context_t new_block_context(decl_context_t enclosing_context)
{
    ERROR_CONDITION(enclosing_context.current_scope->kind != NAMESPACE_SCOPE
            && enclosing_context.current_scope->kind != CLASS_SCOPE
            && enclosing_context.current_scope->kind != BLOCK_SCOPE,
            "Enclosing scope is neither namespace nor class scope", 0);

    // Inherit block context
    decl_context_t result = enclosing_context;

    // Create new block scope
    result.block_scope = new_block_scope(enclosing_context.current_scope);

    // And update the current scope
    result.current_scope = result.block_scope;

    // Update the related entry if any
    if (enclosing_context.current_scope->kind == BLOCK_SCOPE)
    {
        result.current_scope->related_entry = enclosing_context.current_scope->related_entry;
    }

    return result;
}


decl_context_t new_function_context(decl_context_t enclosing_context)
{
    // Inherit the scope
    decl_context_t result = enclosing_context;
    // Create a new function scope for it
    result.function_scope = new_function_scope();
    // Note that we are not changing the current_scope, since nothing
    // works on it.
    return result;
}

decl_context_t new_class_context(decl_context_t enclosing_context, 
        scope_entry_t* class_entry)
{
    ERROR_CONDITION(enclosing_context.current_scope->kind != NAMESPACE_SCOPE
            && enclosing_context.current_scope->kind != CLASS_SCOPE
            && enclosing_context.current_scope->kind != BLOCK_SCOPE, /* The last case yields a local class */
            "Enclosing scope is neither namespace, class or local", 0
            );

    ERROR_CONDITION(class_entry->kind != SK_CLASS, "This is not a class", 0);

    // Inherit the scope
    decl_context_t result = enclosing_context;

    // Create new class scope
    result.class_scope = new_class_scope(enclosing_context.current_scope, class_entry);

    // And make it the current one
    result.current_scope = result.class_scope;

    return result;
}

void insert_alias(scope_t* sc, scope_entry_t* entry, const char* name)
{
    ERROR_CONDITION(name == NULL ||
            *name == '\0', "Insert alias called with an empty or NULL string", 0);

    const char* symbol_name = uniquestr(name);

    scope_entry_list_t* result_set = NULL;
    rb_red_blk_node* n = rb_tree_query(sc->hash, symbol_name);
    if (n != NULL)
    {
        result_set = (scope_entry_list_t*) rb_node_get_info(n);
    }

    if (result_set != NULL)
    {
        entry_list_add(result_set, entry);
    }
    else
    {
        result_set = entry_list_new(entry);
        rb_tree_add(sc->hash, symbol_name, result_set);
    }
}

// Normally we work on decl_context.current_scope but for template parameters
// sc != decl_context.current_scope so allow the user such freedom
scope_entry_t* new_symbol(decl_context_t decl_context, scope_t* sc, const char* name)
{
    ERROR_CONDITION(name == NULL ||
            *name == '\0', "New symbol called with an empty or NULL string", 0);

    scope_entry_t* result;

    result = counted_calloc(1, sizeof(*result), &_bytes_used_symbols);
    result->symbol_name = uniquestr(name);
    // Remember, for template parameters, .current_scope will not contain
    // its declaration scope but will be in .template_scope
    result->decl_context = decl_context;

    result->extended_data = counted_calloc(1, sizeof(*(result->extended_data)), &_bytes_used_symbols);
    extensible_struct_init(&result->extended_data);

    insert_alias(sc, result, result->symbol_name);

    return result;
}

char same_scope(scope_t* stA, scope_t* stB)
{
    return (stA->hash == stB->hash);
}

static const char* scope_names[] =
{
    [UNDEFINED_SCOPE] = "UNDEFINED_SCOPE",
    [NAMESPACE_SCOPE] = "NAMESPACE_SCOPE",
    [FUNCTION_SCOPE] = "FUNCTION_SCOPE",
    [PROTOTYPE_SCOPE] = "PROTOTYPE_SCOPE",
    [BLOCK_SCOPE] = "BLOCK_SCOPE",
    [CLASS_SCOPE] = "CLASS_SCOPE",
};

static scope_entry_list_t* query_name_in_scope(scope_t* sc, const char* name)
{
    DEBUG_CODE()
    {
        if (sc->related_entry == NULL)
        {
        fprintf(stderr, "SCOPE: Looking symbol '%s' in %p [kind=%s]...\n", 
                name, 
                sc, 
                scope_names[sc->kind]);
        }
        else
        {
        fprintf(stderr, "SCOPE: Looking symbol '%s' in %p [kind=%s, qualification=%s]...\n", 
                name, 
                sc, 
                scope_names[sc->kind],
                sc->related_entry->symbol_name);
        }
    }

    scope_entry_list_t* result = NULL;
    rb_red_blk_node *n = rb_tree_query(sc->hash, name);
    if (n != NULL)
    {
        result = (scope_entry_list_t*) rb_node_get_info(n);
    }

    DEBUG_CODE()
    {
        if (result == NULL)
        {
            fprintf(stderr, "SCOPE: Symbol '%s' NOT found in scope '%p'\n", name, sc);
        }
        else
        {
            fprintf(stderr, "SCOPE: Symbol '%s' found in scope '%p'\n", name, sc);
        }
    }

    return entry_list_copy(result);
}

/*
 * Insert entry in the scope
 */
void insert_entry(scope_t* sc, scope_entry_t* entry)
{
    ERROR_CONDITION((entry->symbol_name == NULL), "Inserting a symbol entry without name!", 0);
    
    scope_entry_list_t* result_set = NULL;
    rb_red_blk_node* n = rb_tree_query(sc->hash, entry->symbol_name);
    if (n != NULL)
    {
        result_set = (scope_entry_list_t*)rb_node_get_info(n);
    }

    if (result_set != NULL)
    {
        char do_not_add = 0;
        scope_entry_list_iterator_t* it = entry_list_iterator_begin(result_set);
        while (!entry_list_iterator_end(it) && !do_not_add)
        {
            if (entry_list_iterator_current(it) == entry)
            {
                do_not_add = 1;
            }

            entry_list_iterator_next(it);
        }
        entry_list_iterator_free(it);

        if (!do_not_add)
        {
            entry_list_add(result_set, entry);
        }
    }
    else
    {
        result_set = entry_list_new(entry);
        rb_tree_add(sc->hash, entry->symbol_name, result_set);
    }
}

void remove_entry(scope_t* sc UNUSED_PARAMETER, scope_entry_t* entry UNUSED_PARAMETER)
{
    internal_error("Not yet implemented", 0);
}

scope_entry_list_t* filter_symbol_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set)
{
    scope_entry_list_t* result = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        int i;
        char found = 0;
        for (i = 0; (i < num_kinds) && !found; i++)
        {
            if (entry->kind == symbol_kind_set[i])
            {
                result = entry_list_add(result, entry);
                found = 1;
            }
        }
    }
    entry_list_iterator_free(it);

    return result;
}

scope_entry_list_t* filter_symbol_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind)
{
    scope_entry_list_t* result = NULL;

    result = filter_symbol_kind_set(entry_list, 1, &symbol_kind);

    return result;
}


scope_entry_list_t* filter_symbol_non_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set)
{
    scope_entry_list_t* result = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        int i;
        char found = 0;
        for (i = 0; (i < num_kinds) && !found; i++)
        {
            if (entry->kind == symbol_kind_set[i])
            {
                found = 1;
            }
        }

        if (!found)
        {
            result = entry_list_add(result, entry);
        }
    }
    entry_list_iterator_free(it);

    return result;
}

scope_entry_list_t* filter_symbol_non_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind)
{
    scope_entry_list_t* result = NULL;

    result = filter_symbol_non_kind_set(entry_list, 1, &symbol_kind);

    return result;
}

scope_entry_list_t* filter_symbol_using_predicate(scope_entry_list_t* entry_list, char (*f)(scope_entry_t*))
{
    scope_entry_list_t* result = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (f(entry))
        {
            result = entry_list_add(result, entry);
        }
    }
    entry_list_iterator_free(it);

    return result;
}

// Attribute is_friend states that this symbol has been created to represent a friend declaration
// but should not be visible yet in the scope (though we need the symbol, otherwise there is no way
// to realize later that this symbol is friend!)
static char is_friend_symbol(scope_entry_t* entry)
{
    char is_friend = 0;
    if ((entry->kind == SK_CLASS
            || entry->kind == SK_FUNCTION)
            && is_template_specialized_type(entry->type_information))
    {
        type_t* template_type = template_specialized_type_get_related_template_type(entry->type_information);
        scope_entry_t* template_sym = template_type_get_related_symbol(template_type);
        is_friend =  template_sym->entity_specs.is_friend;
    }
    else
    {
        is_friend =  entry->entity_specs.is_friend;
    }

    return !is_friend;
}

scope_entry_list_t* filter_friends(scope_entry_list_t* entry_list)
{
    return filter_symbol_using_predicate(entry_list, is_friend_symbol);
}

static scope_entry_list_t* query_unqualified_name(
        decl_context_t decl_context,
        decl_context_t template_arg_ctx,
        AST unqualified_name);

scope_entry_list_t* query_id_expression_flags(decl_context_t decl_context,
        AST id_expression, decl_flags_t decl_flags)
{
    switch (ASTType(id_expression))
    {
        case AST_SYMBOL :
        case AST_TEMPLATE_ID :
        case AST_DESTRUCTOR_ID :
        case AST_DESTRUCTOR_TEMPLATE_ID :
        case AST_CONVERSION_FUNCTION_ID :
        case AST_OPERATOR_FUNCTION_ID :
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            return query_nested_name_flags(decl_context, NULL, NULL, id_expression, decl_flags);
        case AST_QUALIFIED_ID :
        case AST_QUALIFIED_TEMPLATE :
            return query_nested_name_flags(decl_context, ASTSon0(id_expression),
                    ASTSon1(id_expression),
                    ASTSon2(id_expression), decl_flags);
        default:
            {
                internal_error("Invalid tree '%s'", ast_print_node_type(ASTType(id_expression)));
            }
    }
}

/*
 * Query functions
 *
 * This is the only one that should be used outside
 */
scope_entry_list_t* query_nested_name_flags(decl_context_t decl_context, 
        AST global_op, 
        AST nested_name, 
        AST unqualified_name,
        decl_flags_t decl_flags)
{
    decl_context.decl_flags = DF_NONE;
    decl_context.decl_flags |= decl_flags;
    char is_unqualified = (nested_name == NULL) && (global_op == NULL);

    if (is_unqualified)
    {
        return query_unqualified_name(decl_context, decl_context, unqualified_name);
    }
    else
    {
        decl_context.decl_flags &= ~DF_ONLY_CURRENT_SCOPE;
        return query_qualified_name(decl_context, global_op, nested_name, unqualified_name);
    }
}

scope_entry_list_t* query_in_scope_str_flags(decl_context_t decl_context,
        const char* name, decl_flags_t decl_flags)
{
    decl_context.decl_flags = DF_NONE;
    decl_context.decl_flags |= decl_flags;
    decl_context.decl_flags |= DF_ONLY_CURRENT_SCOPE;
    return query_unqualified_name_str_flags(decl_context, name, decl_flags);
}

scope_entry_list_t* query_in_scope_flags(decl_context_t decl_context,
        AST unqualified_name, decl_flags_t decl_flags)
{
    decl_context.decl_flags = DF_NONE;
    decl_context.decl_flags |= decl_flags;
    decl_context.decl_flags |= DF_ONLY_CURRENT_SCOPE;
    return query_unqualified_name(decl_context, decl_context, unqualified_name);
}

scope_entry_list_t* query_unqualified_name_str_flags(decl_context_t decl_context,
        const char* unqualified_name, decl_flags_t decl_flags)
{
    decl_context.decl_flags = DF_NONE;
    decl_context.decl_flags |= decl_flags;

    // Adjust scopes
    if (BITMAP_TEST(decl_context.decl_flags, DF_LABEL))
    {
        // If we are looking for a label, use the function scope
        decl_context.current_scope = decl_context.function_scope;
    }

    // No line information here...
    return name_lookup(decl_context, unqualified_name, "(null)", 0);
}

static scope_entry_list_t* query_unqualified_name(
        decl_context_t decl_context,
        decl_context_t template_arg_ctx,
        AST unqualified_name)
{
    // Adjust scopes
    if (BITMAP_TEST(decl_context.decl_flags, DF_LABEL))
    {
        // If we are looking for a label, use the function scope
        decl_context.current_scope = decl_context.function_scope;
    }

    scope_entry_list_t *result = NULL;
    switch (ASTType(unqualified_name))
    {
        case AST_SYMBOL:
            {
                if (is_template_parameter_name(unqualified_name))
                {
                    // Special lookup, it uses query_unqualified_name_str, so it is safe
                    scope_entry_t* name = lookup_template_parameter_name(decl_context, 
                            unqualified_name);
                    if (name != NULL)
                    {
                        return entry_list_new(name);
                    }
                }

                const char* name = uniquestr(ASTText(unqualified_name));
                if (BITMAP_TEST(decl_context.decl_flags, DF_CONSTRUCTOR))
                {
                    name = strprepend(name, "constructor ");
                }
                result = name_lookup(decl_context, name, 
                        ASTFileName(unqualified_name), ASTLine(unqualified_name));

                if (result != NULL
                        && entry_list_size(result) == 1)
                {
                    scope_entry_t* entry = entry_list_head(result);
                    if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                            || entry->kind == SK_TEMPLATE_PARAMETER)
                    {
                        // This is a template parameter, label it 
                        set_as_template_parameter_name(unqualified_name, entry);
                    }
                }
            }
            break;
        case AST_TEMPLATE_ID:
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
                result = query_template_id(unqualified_name, decl_context, template_arg_ctx);
            }
            break;
        case AST_DESTRUCTOR_ID:
        case AST_DESTRUCTOR_TEMPLATE_ID:
            // They have as a name ~name
            {
                AST symbol = ASTSon0(unqualified_name);
                const char *name = ASTText(symbol);
                result = name_lookup(decl_context, name, 
                        ASTFileName(unqualified_name), ASTLine(unqualified_name));
            }
            break;
        case AST_CONVERSION_FUNCTION_ID:
            {
                decl_context_t modified_class_context = decl_context;
                modified_class_context.template_parameters = template_arg_ctx.template_parameters;

                nodecl_t dummy_nodecl_output = nodecl_null();
                char* conversion_function_name = 
                    get_conversion_function_name(modified_class_context, unqualified_name, /* result_type */ NULL, &dummy_nodecl_output);
                result = name_lookup(decl_context, conversion_function_name,
                        ASTFileName(unqualified_name), ASTLine(unqualified_name));
            }
            break;
        case AST_OPERATOR_FUNCTION_ID :
            {
                const char *operator_function_name = get_operator_function_name(unqualified_name);
                result = name_lookup(decl_context, operator_function_name,
                        ASTFileName(unqualified_name), ASTLine(unqualified_name));
                break;
            }
        default:
            {
                internal_error("Invalid node type '%s'\n", ast_print_node_type(ASTType(unqualified_name)));
            }
    }

    return result;
}

static scope_entry_list_t* query_in_namespace(scope_entry_t* namespace, 
        const char* name, decl_flags_t decl_flags,
        const char* filename, int line);
static scope_entry_list_t* query_in_class(scope_t* current_class_scope, 
        const char* name, decl_flags_t decl_flags,
        const char* filename, int line);
static scope_entry_list_t* query_template_id_aux(AST template_id, 
        decl_context_t template_name_context,
        decl_context_t template_parameters_context,
        scope_entry_list_t* (*query_function)(decl_context_t, const char*, const char*, int));

static scope_entry_list_t* query_final_template_id(decl_context_t lookup_context,
        const char* template_name,
        const char* filename,
        int line)
{
    scope_entry_list_t* result = NULL;

    if (lookup_context.current_scope->kind == NAMESPACE_SCOPE)
    {
        result = query_in_namespace(lookup_context.current_scope->related_entry, 
                template_name, 
                lookup_context.decl_flags, filename, line);
    }
    else if (lookup_context.current_scope->kind == CLASS_SCOPE)
    {
        result = query_in_class(lookup_context.current_scope, 
                template_name, 
                lookup_context.decl_flags,
                filename, line);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    return result;
}

static scope_entry_list_t* query_template_id_in_class(decl_context_t lookup_context,
        const char* template_name,
        const char* filename,
        int line)
{
    scope_entry_list_t* result = NULL;

    if (lookup_context.current_scope->kind == CLASS_SCOPE)
    {
        result = query_in_class(lookup_context.current_scope, 
                template_name, 
                lookup_context.decl_flags,
                filename, line);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    return result;
}

static scope_entry_list_t* query_template_id_in_namespace(decl_context_t lookup_context,
        const char* template_name,
        const char* filename,
        int line)
{
    scope_entry_list_t* result = NULL;

    if (lookup_context.current_scope->kind == NAMESPACE_SCOPE)
    {
        result = query_in_namespace(lookup_context.current_scope->related_entry, 
                template_name, 
                lookup_context.decl_flags,
                filename,
                line);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    return result;
}

static scope_entry_list_t* query_final_part_of_qualified(
        decl_context_t nested_name_context,
        decl_context_t lookup_context,
        AST unqualified_name)
{
    ERROR_CONDITION(lookup_context.current_scope->kind != CLASS_SCOPE
            && lookup_context.current_scope->kind != NAMESPACE_SCOPE,
            "Invalid scope found", 0);

    scope_entry_list_t* result = NULL;

    switch (ASTType(unqualified_name))
    {
        case AST_SYMBOL:
            {
                const char* name = uniquestr(ASTText(unqualified_name));
                if (BITMAP_TEST(lookup_context.decl_flags, DF_CONSTRUCTOR))
                {
                    name = strprepend(name, "constructor ");
                }

                if (lookup_context.current_scope->kind == NAMESPACE_SCOPE)
                {
                    result = query_in_namespace(lookup_context.current_scope->related_entry, name,
                            lookup_context.decl_flags,
                            ASTFileName(unqualified_name), ASTLine(unqualified_name));
                }
                else if (lookup_context.current_scope->kind == CLASS_SCOPE)
                {
                    result = query_in_class(lookup_context.current_scope, name, lookup_context.decl_flags,
                            ASTFileName(unqualified_name), ASTLine(unqualified_name));
                }
            }
            break;
        case AST_TEMPLATE_ID:
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
                result = query_template_id_aux(unqualified_name,
                        lookup_context,
                        nested_name_context,
                        query_final_template_id);
            }
            break;
        case AST_DESTRUCTOR_ID:
        case AST_DESTRUCTOR_TEMPLATE_ID:
            // They have as a name ~name
            {
                AST symbol = ASTSon0(unqualified_name);
                const char *name = ASTText(symbol);
                if (lookup_context.current_scope->kind == CLASS_SCOPE)
                {
                    result = query_in_class(lookup_context.current_scope, name, lookup_context.decl_flags,
                            ASTFileName(unqualified_name), ASTLine(unqualified_name));
                }
                else
                {
                    running_error("%s:%d: error: invalid destructor-id in non class-scope\n",
                            ASTFileName(unqualified_name), ASTLine(unqualified_name));
                }
            }
            break;
        case AST_CONVERSION_FUNCTION_ID:
            {
                decl_context_t modified_class_context = lookup_context;
                modified_class_context.template_parameters = nested_name_context.template_parameters;

                nodecl_t dummy_nodecl_output = nodecl_null();
                char* conversion_function_name = 
                    get_conversion_function_name(modified_class_context, unqualified_name, /* result_type */ NULL, &dummy_nodecl_output);

                if (lookup_context.current_scope->kind == CLASS_SCOPE)
                {
                    result = query_in_class(lookup_context.current_scope, conversion_function_name, lookup_context.decl_flags,
                            ASTFileName(unqualified_name), ASTLine(unqualified_name));
                }
                else
                {
                    running_error("%s:%d: error: invalid conversion-id in non class-scope\n",
                            ASTFileName(unqualified_name), ASTLine(unqualified_name));
                }
            }
            break;
        case AST_OPERATOR_FUNCTION_ID :
            {
                const char *operator_function_name = get_operator_function_name(unqualified_name);
                if (lookup_context.current_scope->kind == NAMESPACE_SCOPE)
                {
                    result = query_in_namespace(lookup_context.current_scope->related_entry, 
                            operator_function_name,
                            lookup_context.decl_flags,
                            ASTFileName(unqualified_name), ASTLine(unqualified_name));
                }
                else if (lookup_context.current_scope->kind == CLASS_SCOPE)
                {
                    result = query_in_class(lookup_context.current_scope, 
                            operator_function_name,
                            lookup_context.decl_flags,
                            ASTFileName(unqualified_name), ASTLine(unqualified_name));
                }
                break;
            }
        default:
            {
                internal_error("Invalid node type '%s'\n", ast_print_node_type(ASTType(unqualified_name)));
            }
    }

    return result;
}

static scope_entry_list_t* query_qualified_name(
        decl_context_t nested_name_context,
        AST global_op,
        AST nested_name,
        AST unqualified_name)
{
    DEBUG_CODE()
    {
        const char* c = prettyprint_in_buffer(unqualified_name);

        if (ASTType(unqualified_name) != AST_TEMPLATE_ID)
        {
            if (BITMAP_TEST(nested_name_context.decl_flags, DF_CONSTRUCTOR))
            {
                c = strprepend(c, "constructor ");
            }
        }

        fprintf(stderr, "SCOPE: Solving qualified-id '%s%s%s'\n", prettyprint_in_buffer(global_op), 
                prettyprint_in_buffer(nested_name),
                c);
    }

    // Ignore invalid nested name specs
    if (!check_nested_name_spec(nested_name, nested_name_context))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: This nested-name-spec '%s' is not valid\n", prettyprint_in_buffer(nested_name));
        }
        return NULL;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Nested-name-spec '%s' seems fine\n", prettyprint_in_buffer(nested_name));
        }
    }

    if (ASTType(unqualified_name) == AST_TEMPLATE_ID)
    {
        if (!solve_possibly_ambiguous_template_id(unqualified_name, nested_name_context))
        {
            return NULL;
        }
    }

    decl_context_t qualified_context;
    decl_context_t nested_part_context = nested_name_context;
    nested_part_context.decl_flags &= ~DF_CONSTRUCTOR;

    scope_entry_list_t* result = NULL;
    if (global_op != NULL)
    {
        nested_part_context.current_scope = nested_name_context.global_scope;
    }

    qualified_context = nested_part_context;

    // Looking up a nested name has a twofold process.  First we determine the
    // looking up scope by resolving all but the 'unqualified name' part
    type_t* dependent_type = NULL;

    char qualified_context_valid = 1;

    if (nested_name != NULL)
    {
        qualified_context = lookup_qualification_scope(
                nested_name_context,
                nested_part_context, 
                nested_name, 
                unqualified_name, 
                &dependent_type, 
                &qualified_context_valid);
    }

    if (!qualified_context_valid)
    {
        if (dependent_type != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Returning a dependent entity for '%s%s%s'\n",
                        prettyprint_in_buffer(global_op),
                        prettyprint_in_buffer(nested_name),
                        prettyprint_in_buffer(unqualified_name));
            }
            // Create a SK_DEPENDENT_ENTITY just to acknowledge that this was
            // dependent
            scope_entry_t* dependent_entity = counted_calloc(1, sizeof(*dependent_entity), &_bytes_used_scopes);
            if (ASTType(unqualified_name) == AST_SYMBOL)
            {
                dependent_entity->symbol_name = ASTText(unqualified_name);
            }
            else if (ASTType(unqualified_name) == AST_TEMPLATE_ID)
            {
                dependent_entity->symbol_name = ASTText(ASTSon0(unqualified_name));
            }
            else if (ASTType(unqualified_name) == AST_OPERATOR_FUNCTION_ID
                    || ASTType(unqualified_name) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
            {
                dependent_entity->symbol_name = get_operator_function_name(unqualified_name);
            }
            else if (ASTType(unqualified_name) == AST_CONVERSION_FUNCTION_ID)
            {
                nodecl_t dummy_nodecl_output = nodecl_null();
                dependent_entity->symbol_name = get_conversion_function_name(nested_name_context, 
                        unqualified_name, /* result_type */ NULL, &dummy_nodecl_output);
            }
            else if (ASTType(unqualified_name) == AST_DESTRUCTOR_ID
                    || ASTType(unqualified_name) == AST_DESTRUCTOR_TEMPLATE_ID)
            {
                AST symbol = ASTSon0(unqualified_name);
                const char *name = ASTText(symbol);
                dependent_entity->symbol_name = name;
            }
            else
            {
                internal_error("unhandled dependent name '%s'\n", prettyprint_in_buffer(unqualified_name));
            }

            dependent_entity->decl_context = nested_name_context;
            dependent_entity->kind = SK_DEPENDENT_ENTITY;
            dependent_entity->type_information = dependent_type;
            dependent_entity->language_dependent_value = 
                ASTMake3(AST_QUALIFIED_ID,
                        ast_copy(global_op), 
                        ast_copy(nested_name), 
                        ast_copy(unqualified_name),
                        ASTFileName(unqualified_name), ASTLine(unqualified_name), NULL);

            result = entry_list_new(dependent_entity);
            return result;
        }
        else
        {
            // Well, this was not dependent, so do not return anything
            return NULL;
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Solving '%s' of '%s%s'\n", 
                prettyprint_in_buffer(unqualified_name),
                prettyprint_in_buffer(global_op),
                prettyprint_in_buffer(nested_name));
    }

    // Given the scope now we can lookup the symbol in it. Note that if some
    // scope was given we have to be able to find something there
    decl_context_t lookup_context = qualified_context;
    lookup_context.decl_flags |= nested_name_context.decl_flags;

    result = query_final_part_of_qualified(nested_name_context, lookup_context, unqualified_name);
    return result;
}

static decl_context_t lookup_qualification_scope(
        decl_context_t original_context, 
        decl_context_t nested_name_context, 
        AST nested_name_spec, 
        AST unqualified_part, 
        type_t** dependent_type, 
        char *is_valid)
{
    ERROR_CONDITION(dependent_type == NULL, "Invalid argument is_dependent", 0);
    *dependent_type = NULL;
    ERROR_CONDITION(nested_name_spec == NULL, "The first nested-name-spec is null", 0);
    decl_context_t result;
    memset(&result, 0, sizeof(result));

    char allow_namespaces = 1;
    scope_entry_t* previous_symbol = NULL;

    AST current_nested_name = nested_name_spec;
    decl_context_t current_context = nested_name_context;

    while (current_nested_name != NULL)
    {
        AST current_name = ASTSon0(current_nested_name);
        AST next_nested_name_spec = ASTSon1(current_nested_name);

        scope_entry_list_t* current_entry_list = NULL;
        if (previous_symbol == NULL)
        {
            if (ASTType(current_name) == AST_TEMPLATE_ID)
            {
                current_entry_list = query_template_id(current_name, 
                        current_context, current_context);
            }
            else
            {
                current_entry_list = query_unqualified_name(current_context, original_context, current_name);
            }
        }
        else if (previous_symbol->kind == SK_CLASS)
        {
            if (ASTType(current_name) != AST_TEMPLATE_ID)
            {
                current_entry_list = query_in_class(current_context.current_scope, 
                        ASTText(current_name), 
                        current_context.decl_flags,
                        ASTFileName(current_name), 
                        ASTLine(current_name));
            }
            else
            {
                current_entry_list = query_template_id_aux(current_name, 
                        current_context, original_context, 
                        query_template_id_in_class);
            }
        }
        else if (previous_symbol->kind == SK_NAMESPACE)
        {
            if (ASTType(current_name) != AST_TEMPLATE_ID)
            {
                current_entry_list = query_in_namespace(current_context.current_scope->related_entry, 
                        ASTText(current_name),
                        current_context.decl_flags,
                        ASTFileName(current_name),
                        ASTLine(current_name));
            }
            else
            {
                current_entry_list = query_template_id_aux(current_name, 
                        current_context, original_context, 
                        query_template_id_in_namespace);
            }
        }
        else
        {
            internal_error("Invalid symbol kind '%d' of '%s'\n", 
                    previous_symbol->kind, 
                    previous_symbol->symbol_name);
        }

        if (current_entry_list == NULL
                || entry_list_size(current_entry_list) > 1)
        {
            entry_list_free(current_entry_list);
            *is_valid = 0;
            return result;
        }

        scope_entry_t* current_symbol = entry_list_head(current_entry_list);
        entry_list_free(current_entry_list);

        if (current_symbol->kind == SK_NAMESPACE)
        {
            if (!allow_namespaces)
            {
                internal_error("Invalidly nested namespace '%s' inside of a class\n", current_symbol->symbol_name);
            }

            // Update the context
            current_context = current_symbol->related_decl_context;
            current_context.decl_flags |= nested_name_context.decl_flags;
        }
        else if (current_symbol->kind == SK_CLASS
                || (current_symbol->kind == SK_TYPEDEF)
                || current_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER)
        {
            if (current_symbol->kind == SK_TYPEDEF)
            {
                type_t* t = advance_over_typedefs(current_symbol->type_information);

                if (is_dependent_typename_type(t))
                {
                    // This is dependent
                    *dependent_type = get_dependent_typename_type(current_symbol, 
                            nested_name_context, next_nested_name_spec, unqualified_part);
                    *is_valid = 0;
                    return result;
                }

                if (!is_named_type(t))
                {
                    running_error("%s: typedef name '%s' is not a namespace or class\n", 
                            ast_location(current_name),
                            prettyprint_in_buffer(current_name));
                }

                current_symbol = named_type_get_symbol(t);
            }

            if (current_symbol->kind == SK_CLASS)
            {
                type_t* class_type = current_symbol->type_information;

                if (class_type_is_incomplete_independent(class_type))
                {
                    instantiate_template_class(current_symbol, nested_name_context,
                            ASTFileName(current_name), ASTLine(current_name));
                }
                else if (class_type_is_incomplete_dependent(class_type)
                        // In some cases we do not want to examine uninstantiated templates
                        || (BITMAP_TEST(current_context.decl_flags, DF_DEPENDENT_TYPENAME)
                            && (class_type_is_complete_dependent(class_type)
                                || (current_symbol->decl_context.current_scope->kind == CLASS_SCOPE
                                    && is_dependent_type(current_symbol->decl_context.current_scope->related_entry->type_information))
                               )))
                {
                    // We cannot do anything else here but returning NULL
                    // and stating that it is dependent
                    *dependent_type = get_dependent_typename_type(current_symbol, 
                            nested_name_context, next_nested_name_spec, unqualified_part);
                    *is_valid = 0;
                    return result;
                }

                current_context = class_type_get_inner_context(class_type);
                current_context.decl_flags |= nested_name_context.decl_flags;
            }
            else if (current_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER)
            {
                *dependent_type = get_dependent_typename_type(current_symbol, 
                        nested_name_context, next_nested_name_spec, unqualified_part);
                *is_valid = 0;
                return result;
            }
            else
            {
                running_error("%s: aliased type of typedef name '%s' is not a namespace or class\n", 
                        ast_location(current_name),
                        prettyprint_in_buffer(current_name));
            }

            allow_namespaces = 0;
        }
        else if (current_symbol->kind == SK_TEMPLATE
                || current_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
        {
            running_error("%s: template-name '%s' used without template arguments\n", 
                    ast_location(current_name),
                    prettyprint_in_buffer(current_name));
        }
        else
        {
            running_error("%s: name '%s' is not a namespace or class\n", 
                    ast_location(current_name),
                    prettyprint_in_buffer(current_name));
        }

        previous_symbol = current_symbol;
        current_nested_name = next_nested_name_spec;
    }

    *is_valid = 1;
    result = current_context;
    
    return result;
}

typedef
struct class_scope_lookup_tag
{
    int path_length;
    type_t* path[MCXX_MAX_SCOPES_NESTING];
    char is_virtual[MCXX_MAX_SCOPES_NESTING];

    scope_entry_list_t* entry_list;
} class_scope_lookup_t;

static char can_be_inherited(scope_entry_t* entry)
{
    ERROR_CONDITION(entry == NULL, "Error, entry can't be null", 0);

    if (entry->entity_specs.is_conversion
            || entry->entity_specs.is_constructor
            /* || entry->entity_specs.is_injected_class_name*/ )
    {
        return 0;
    }

    // All other entities signed in in the scope can be inherited
    return 1;
}

static char is_injected_class_name(scope_entry_t* entry)
{
    ERROR_CONDITION(entry == NULL, "Error, entry can't be null", 0);

    if (entry->entity_specs.is_injected_class_name)
    {
        return 0;
    }

    return 1;
}

static scope_entry_list_t* filter_not_inherited_entities(scope_entry_list_t* list)
{
    return filter_symbol_using_predicate(list, can_be_inherited);
}

static scope_entry_list_t* filter_injected_class_name(scope_entry_list_t* list)
{
    return filter_symbol_using_predicate(list, is_injected_class_name);
}

void class_scope_lookup_rec(scope_t* current_class_scope, const char* name, 
        class_scope_lookup_t* derived, char is_virtual, char initial_lookup, decl_flags_t decl_flags,
        const char* filename, int line)
{
    int i;

    ERROR_CONDITION(current_class_scope->kind != CLASS_SCOPE, "Current scope is not class-scope", 0);

    type_t* current_class_type = current_class_scope->related_entry->type_information;

    ERROR_CONDITION(current_class_type == NULL, "Class scope does not have a class-type", 0);

    // Fill our information
    derived->path_length++;
    ERROR_CONDITION(derived->path_length == MCXX_MAX_SCOPES_NESTING, "Class path too long", 0);

    derived->path[derived->path_length - 1] = current_class_type;
    derived->is_virtual[derived->path_length - 1] = is_virtual;

    class_scope_lookup_t bases_lookup[MCXX_MAX_CLASS_BASES];
    memset(bases_lookup, 0, sizeof(bases_lookup));

    int num_bases = class_type_get_num_bases(current_class_type);
    ERROR_CONDITION(num_bases > MCXX_MAX_CLASS_BASES, "Too many bases", 0);

    for (i = 0; i < num_bases; i++)
    {
        char current_base_is_virtual = 0;
        char current_base_is_dependent = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        scope_entry_t* base_class_entry = class_type_get_base_num(current_class_type, i, 
                &current_base_is_virtual,
                &current_base_is_dependent,
                &access_specifier);

        if (current_base_is_dependent)
            continue;

        type_t* base_class_type = get_actual_class_type(base_class_entry->type_information);
        decl_context_t base_class_context = class_type_get_inner_context(base_class_type);
        scope_t* base_class_scope = base_class_context.current_scope;

        // Copy the info
        bases_lookup[i] = *derived;

        class_scope_lookup_rec(base_class_scope, name, &(bases_lookup[i]), 
                current_base_is_virtual, /* initial_lookup */ 0, decl_flags,
                filename, line);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Looking in class scope '");
        for (i = derived->path_length - 1; i >= 0; i--)
        {
            fprintf(stderr, "%s%s", 
                    class_type_get_inner_context(derived->path[i]).current_scope->related_entry->symbol_name,
                    (i > 0) ? "::" : "");
        }
        fprintf(stderr, "'\n");
    }

    scope_entry_list_t* entry_list = query_name_in_scope(current_class_scope, name);

    if (!initial_lookup)
    {
        scope_entry_list_t* old_entry_list = entry_list;
        entry_list = filter_not_inherited_entities(old_entry_list);
        entry_list_free(old_entry_list);
    }

    if (BITMAP_TEST(decl_flags, DF_NO_INJECTED_CLASS_NAME))
    {
        scope_entry_list_t* old_entry_list = entry_list;
        entry_list = filter_injected_class_name(entry_list);
        entry_list_free(old_entry_list);
    }

    if (entry_list != NULL)
    {
        // Use our results because of hiding
        derived->entry_list = entry_list;

        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Found in current class, hides any symbol in base classes\n");
        }
    }
    else
    {
        int num_contributing_bases = 0;

        for (i = 0; i < num_bases; i++)
        {
            if (bases_lookup[i].entry_list != NULL)
            {
                num_contributing_bases++;
            }
        }

        // Combine results from bases
        if (num_contributing_bases == 0)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: No bases and not found in current class\n");
            }
            // No results :)
            derived->entry_list = NULL;
        }
        else if (num_contributing_bases == 1)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Not found in current class but in only one (possibly indirect) base\n");
            }
            // Easy case where no merge must be performed, just get the result of the only base that is not null
            for (i = 0; i < num_bases; i++)
            {
                if (bases_lookup[i].entry_list != NULL)
                {
                    *derived = bases_lookup[i];
                    break;
                }
            }
        }
        else
        {
            // Merge must be done. Create a candidate result based on the first
            // and check if they refer to the same subobject (or static base
            // class)
            // Create the candidate, the first non null

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Name found in two or more bases, will have to check if they come from the same subobject\n");
            }

            char valid = 0;
            int index_candidate = -1;
            for (i = 0; i < num_bases; i++)
            {
                if (bases_lookup[i].entry_list != NULL)
                {
                    *derived = bases_lookup[i];
                    index_candidate = i;
                    valid = 1;
                }
            }

            char finished = 0;
            char several_subobjects = 0;
            for (i = 0; (i < num_bases) && !finished && valid; i++)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Checking entity '%s' found to come from different base classes\n",
                            name);
                }
                class_scope_lookup_t *current = &(bases_lookup[i]);

                if ((current->entry_list == NULL)
                        || (index_candidate == i))
                    continue;

                int path_candidate = derived->path_length - 1;
                int path_current = current->path_length - 1;

                DEBUG_CODE()
                {
                    int k;
                    fprintf(stderr, "SCOPE: First one is '");
                    for (k = 0; k < derived->path_length; k++)
                    {
                        fprintf(stderr, "%s%s%s", 
                                derived->is_virtual[k] ? "<v>" : "",
                                class_type_get_inner_context(derived->path[k]).current_scope->related_entry->symbol_name,
                                ((k+1) < derived->path_length) ? "::" : "");
                    }
                    fprintf(stderr, "'\n");
                    fprintf(stderr, "SCOPE: Second one is '");
                    for (k = 0; k < current->path_length; k++)
                    {
                        fprintf(stderr, "%s%s%s", 
                                current->is_virtual[k] ? "<v>" : "",
                                class_type_get_inner_context(current->path[k]).current_scope->related_entry->symbol_name,
                                ((k+1) < current->path_length) ? "::" : "");
                    }
                    fprintf(stderr, "'\n");
                }

                if (current->path[path_current]
                        != derived->path[path_candidate])
                {
                    // Cannot be the same because they do not end in the same class
                    finished = 1;
                    valid = 0;
                    DEBUG_CODE()
                    {
                        int k;
                        fprintf(stderr, "SCOPE: Entity '%s' entity found to come from different subobjects\n",
                                name);
                        fprintf(stderr, "SCOPE: First one is '");
                        for (k = 0; k < derived->path_length; k++)
                        {
                            fprintf(stderr, "%s%s%s", 
                                    derived->is_virtual[k] ? "<v>" : "",
                                    class_type_get_inner_context(derived->path[k]).current_scope->related_entry->symbol_name,
                                    ((k+1) < derived->path_length) ? "::" : "");
                        }
                        fprintf(stderr, "'\n");
                        fprintf(stderr, "SCOPE: Second one is '");
                        for (k = 0; k < current->path_length; k++)
                        {
                            fprintf(stderr, "%s%s%s", 
                                    current->is_virtual[k] ? "<v>" : "",
                                    class_type_get_inner_context(current->path[k]).current_scope->related_entry->symbol_name,
                                    ((k+1) < current->path_length) ? "::" : "");
                        }
                        fprintf(stderr, "'\n");
                    }
                }
                else
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "SCOPE: Entity '%s' seems to come from the same "
                                "class but it still might come from different subobjects\n", name);
                    }

                    // Unless we prove the opposite
                    // they come from several subobjects
                    several_subobjects = 1;
                    while ((path_candidate > 0
                                && path_current > 0)
                            && !finished)
                    {
                        if (derived->path[path_candidate] == current->path[path_current])
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "SCOPE: We reached same classes '%s' and '%s'\n",
                                        class_type_get_inner_context(derived->path[path_candidate]).current_scope->related_entry->symbol_name,
                                        class_type_get_inner_context(current->path[path_current]).current_scope->related_entry->symbol_name);

                            }
                            if (derived->is_virtual[path_candidate]
                                    && current->is_virtual[path_current])
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "SCOPE: Entity '%s' found to come from the same (shared) subobject\n",
                                            name);
                                }
                                several_subobjects = 0;
                                finished = 1;
                            }
                        }
                        path_current--;
                        path_candidate--;
                    }

                    if (several_subobjects)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "SCOPE: Entity '%s' found to come from the different subobjects\n",
                                    name);
                        }
                    }

                }
            }

            if (valid)
            {
                if (several_subobjects)
                {
                    scope_entry_list_iterator_t* it = NULL; 
                    for (it = entry_list_iterator_begin(derived->entry_list);
                            !entry_list_iterator_end(it);
                            entry_list_iterator_next(it))
                    {
                        scope_entry_t* entry = entry_list_iterator_current(it);
                        if (entry->kind == SK_VARIABLE
                                || entry->kind == SK_FUNCTION
                                /* || entry->kind == SK_TEMPLATE_FUNCTION */)
                        {
                            valid = (entry->entity_specs.is_static);
                            DEBUG_CODE()
                            {
                                if (!valid)
                                {
                                    fprintf(stderr, "SCOPE: One of the entities is not static so lookup of '%s' is not valid\n", 
                                            name);
                                }
                            }
                        }
                    }
                    entry_list_iterator_free(it);
                }
            }

            if (!valid)
            {
                entry_list_free(derived->entry_list);
                derived->entry_list = NULL;
            }
        }
    }
}

static scope_entry_list_t* query_in_class(scope_t* current_class_scope, const char* name, 
        decl_flags_t decl_flags,
        const char* filename, int line)
{
    class_scope_lookup_t result;
    memset(&result, 0, sizeof(result));

    class_scope_lookup_rec(current_class_scope, name, &result, 0, /* initial_lookup */ 1, decl_flags, filename, line);

    if (result.entry_list != NULL)
    {
        DEBUG_CODE()
        {
            int i;

            fprintf(stderr, "SCOPE: Class scope lookup started in class '%s' found name '%s' in '", 
                    current_class_scope->related_entry->symbol_name,
                    name);
            for (i = 0; i < result.path_length; i++)
            {
                fprintf(stderr, "%s%s", 
                        class_type_get_inner_context(result.path[i]).current_scope->related_entry->symbol_name,
                        ((i+1) < result.path_length) ? "::" : "");
            }
            fprintf(stderr, "'\n");
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Class scope lookup did not find any name '%s'\n", name);
        }
    }

    return result.entry_list;
}

scope_entry_list_t* class_context_lookup(decl_context_t decl_context, const char* name)
{
    ERROR_CONDITION(decl_context.current_scope->kind != CLASS_SCOPE, "This is not a class scope", 0);

    return query_in_class(decl_context.current_scope, name, decl_context.decl_flags, "(null)", 0);
}

static scope_entry_list_t* filter_any_non_type(scope_entry_list_t* entry_list)
{
    // Filter the types
    enum cxx_symbol_kind type_filter[] = {
        SK_ENUM ,
        SK_CLASS ,
        SK_TYPEDEF ,
        SK_TEMPLATE_TYPE_PARAMETER,
        SK_TEMPLATE_TEMPLATE_PARAMETER,
        SK_TEMPLATE,
        SK_GCC_BUILTIN_TYPE
    };

    return filter_symbol_kind_set(entry_list, STATIC_ARRAY_LENGTH(type_filter), type_filter);
}

static void error_ambiguity(scope_entry_list_t* entry_list, const char* filename, int line)
{
    fprintf(stderr, "%s:%d: error: ambiguity in reference to '%s'\n", filename, line, 
            entry_list_head(entry_list)->symbol_name);
    fprintf(stderr, "%s:%d: info: candidates are\n", filename, line);

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        fprintf(stderr, "%s:%d: info:    %s\n", 
                entry->file,
                entry->line,
                get_qualified_symbol_name(entry, entry->decl_context));
    }
    entry_list_iterator_free(it);

    running_error("%s:%d: error: lookup failed due to ambiguous reference '%s'\n", 
            filename, line, entry_list_head(entry_list)->symbol_name);
}


static void check_for_naming_ambiguity(scope_entry_list_t* entry_list, const char* filename, int line)
{
    if (entry_list == NULL
            || entry_list_size(entry_list) == 1)
        return;

    scope_entry_t* hiding_name = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (hiding_name == NULL
                && (entry->kind == SK_VARIABLE
                    || entry->kind == SK_ENUMERATOR))
        {
            hiding_name = entry;
        }
        else if (entry->kind == SK_FUNCTION
                || (entry->kind == SK_TEMPLATE
                    && named_type_get_symbol(template_type_get_primary_type(entry->type_information))->kind == SK_FUNCTION))
        {
            hiding_name = entry;
        }
        else if ((entry->kind == SK_CLASS
                    || entry->kind == SK_ENUM)
                && (hiding_name != NULL
                    && hiding_name->decl_context.current_scope == entry->decl_context.current_scope))
        {
        }
        else
        {
            error_ambiguity(entry_list, filename, line);
        }
    }
    entry_list_iterator_free(it);
}

static scope_entry_list_t* query_in_namespace_and_associates(
        scope_entry_t* namespace,
        const char* name, 
        int idx_associated_namespaces, 
        int num_associated_namespaces, 
        scope_entry_t** associated_namespaces, 
        decl_flags_t decl_flags,
        const char* filename,
        int line)
{
    ERROR_CONDITION(namespace->kind != SK_NAMESPACE, "Invalid symbol", 0);
    scope_entry_list_t* grand_result 
        = query_name_in_scope(namespace->related_decl_context.current_scope, name);

    if (grand_result != NULL)
        return grand_result;

    int i;
    for (i = idx_associated_namespaces; 
            i < num_associated_namespaces; 
            i++)
    {
        int new_num_associated_namespaces = num_associated_namespaces;
        scope_entry_t* associated_namespace = associated_namespaces[i];
        scope_t* current_scope = associated_namespace->related_decl_context.current_scope;

        // This namespace may have additional used namespaces which we will add to the list of namespaces 
        // if and only if they are not already there
        int k;
        for (k = 0; k < current_scope->num_used_namespaces; k++)
        {
            int j;
            char found = 0;
            for (j = 0; j < new_num_associated_namespaces && !found; j++)
            {
                found = (associated_namespaces[j] == current_scope->use_namespace[k]);
            }
            if (!found)
            {
                if ((idx_associated_namespaces + new_num_associated_namespaces) == MCXX_MAX_ASSOCIATED_NAMESPACES)
                    running_error("Too many associated namespaces > %d", MCXX_MAX_ASSOCIATED_NAMESPACES);
                associated_namespaces[idx_associated_namespaces + new_num_associated_namespaces] = current_scope->use_namespace[k];
                new_num_associated_namespaces++;
            }
        }

        scope_entry_list_t* result = query_in_namespace_and_associates(
                associated_namespace,
                name, 
                num_associated_namespaces, 
                new_num_associated_namespaces,
                associated_namespaces,
                decl_flags,
                filename, 
                line
                );

        scope_entry_list_t* old_grand_result = grand_result;
        grand_result = entry_list_merge(old_grand_result, result);
        entry_list_free(old_grand_result);
        entry_list_free(result);
        
        check_for_naming_ambiguity(grand_result, filename, line);
    }

    return grand_result;
}

static scope_entry_list_t* query_in_namespace(scope_entry_t* namespace, 
        const char* name, decl_flags_t decl_flags,
        const char* filename, int line)
{
    ERROR_CONDITION(namespace->kind != SK_NAMESPACE, "Invalid symbol", 0);

    scope_t* current_scope = namespace->related_decl_context.current_scope;

    scope_entry_t* associated_namespaces[MCXX_MAX_ASSOCIATED_NAMESPACES];

    int i;
    for (i = 0; i < current_scope->num_used_namespaces; i++)
    {
        if (i == MCXX_MAX_ASSOCIATED_NAMESPACES)
            running_error("Too many associated namespaces > %d", MCXX_MAX_ASSOCIATED_NAMESPACES);
        associated_namespaces[i] = current_scope->use_namespace[i];
    }

    return query_in_namespace_and_associates(namespace, name, 
            0, current_scope->num_used_namespaces, 
            associated_namespaces, decl_flags,
            filename, line); 
}

static scope_entry_list_t* name_lookup(decl_context_t decl_context, 
        const char* name, 
        const char* filename, 
        int line)
{
    ERROR_CONDITION(name == NULL, "Name cannot be null!", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Name lookup of '%s'\n", name);
    }

    // TEMPLATE_SCOPE is specially handled always
    template_parameter_list_t* template_parameters = decl_context.template_parameters;
    while (template_parameters != NULL)
    {
        int i;
        for (i = 0; i < template_parameters->num_parameters; i++)
        {
            template_parameter_t* tpl = template_parameters->parameters[i];

            if (tpl->entry != NULL
                    && strcmp(tpl->entry->symbol_name, name) == 0)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Name '%s' found in template parameter list. Nesting: %d. Position: %d\n",
                            name,
                            tpl->entry->entity_specs.template_parameter_nesting,
                            tpl->entry->entity_specs.template_parameter_position);
                }
                return entry_list_new(tpl->entry);
            }
        }

        template_parameters = template_parameters->enclosing;
    }

    scope_entry_list_t* result = NULL;

    int num_associated_namespaces = 0;
    scope_entry_t* associated_namespaces[MCXX_MAX_ASSOCIATED_NAMESPACES] = { 0 };

    scope_t* current_scope = decl_context.current_scope;

    while (result == NULL
            && current_scope != NULL)
    {
        // If this scope has associated ones, add them to the associated scopes
        int i;
        for (i = 0; i < current_scope->num_used_namespaces; i++)
        {
            int j;
            char found = 0;

            // Inlines are the only ones considered when doing a "only current
            // scope" lookup
            if (BITMAP_TEST(decl_context.decl_flags, DF_ONLY_CURRENT_SCOPE)
                        && !current_scope->use_namespace[i]->entity_specs.is_inline)
                continue;

            for (j = 0; j < num_associated_namespaces && !found; j++)
            {
                found = (associated_namespaces[j] == current_scope->use_namespace[i]);
            }
            if (!found)
            {
                if (num_associated_namespaces == MCXX_MAX_ASSOCIATED_NAMESPACES)
                    running_error("Too many associated scopes > %d", MCXX_MAX_ASSOCIATED_NAMESPACES);
                associated_namespaces[num_associated_namespaces] = current_scope->use_namespace[i];
                num_associated_namespaces++;
            }
        }

        if (current_scope->kind == CLASS_SCOPE
                && !BITMAP_TEST(decl_context.decl_flags, DF_ONLY_CURRENT_SCOPE))
        {
            result = query_in_class(current_scope, name, decl_context.decl_flags, 
                    filename, line);
        }
        else if (current_scope->kind == NAMESPACE_SCOPE)
        {
            result = query_in_namespace_and_associates(
                    current_scope->related_entry,
                    name, 0, num_associated_namespaces,
                    associated_namespaces, decl_context.decl_flags,
                    filename, line);
            num_associated_namespaces = 0;
        }
        else // BLOCK_SCOPE || PROTOTYPE_SCOPE || FUNCTION_SCOPE (although its contains should be NULL)
        {
            result = query_name_in_scope(current_scope, name);
        }

        if (BITMAP_TEST(decl_context.decl_flags, DF_ELABORATED_NAME))
        {
            scope_entry_list_t* old_result = result;
            result = filter_any_non_type(old_result);
            entry_list_free(old_result);
        }

        if (BITMAP_TEST(decl_context.decl_flags, DF_ONLY_CURRENT_SCOPE))
        {
            return result;
        }

        current_scope = current_scope->contained_in;
    }

    return result;
}

static template_parameter_list_t* complete_arguments_of_template_id(
        decl_context_t template_name_context,
        decl_context_t template_parameters_context,
        template_parameter_list_t* primary_template_parameters,
        template_parameter_list_t* template_parameters,
        const char* filename,
        int line);

static template_parameter_value_t* update_template_parameter_value(
        template_parameter_value_t* v,
        decl_context_t decl_context,
        const char* filename, int line)
{
    template_parameter_value_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);

    *result = *v;

    result->type = update_type(result->type, decl_context, filename, line);

    if (nodecl_is_cxx_raw(result->value))
    {
        AST expr = nodecl_unwrap_cxx_raw(result->value);
        expr = ast_copy_for_instantiation(expr);

        if(!check_expression(expr, decl_context))
        {
            internal_error("Updated nontype template parameter has an invalid expression '%s'", 
                    prettyprint_in_buffer(expr));
        }

        if (expression_is_constant(expr))
        {
            result->value = const_value_to_nodecl(expression_get_constant(expr));
        }
        else
        {
            result->value = expression_get_nodecl(expr);
        }
        
        // Force the type of the expression
        nodecl_set_type(result->value, result->type);
    }

    return result;
}

static type_t* update_dependent_typename(
        type_t* dependent_entry_type,
        dependent_name_part_t* dependent_parts,
        decl_context_t decl_context,
        const char* filename, int line)
{
    scope_entry_t* dependent_entry = named_type_get_symbol(dependent_entry_type);

    if (is_dependent_type(dependent_entry_type))
    {
        return get_dependent_typename_type_from_parts(dependent_entry,
                dependent_parts);
    }

    if (dependent_entry->kind == SK_TYPEDEF)
    {
        type_t* advanced_type = advance_over_typedefs(dependent_entry->type_information);

        ERROR_CONDITION(!is_named_type(advanced_type), "This is not a named type", 0);

        dependent_entry = named_type_get_symbol(advanced_type);
    }

    ERROR_CONDITION(dependent_entry->kind != SK_CLASS, "Must be a class-name", 0);

    ERROR_CONDITION(dependent_parts == NULL, "Dependent parts cannot be empty", 0);

    scope_entry_t* current_member = dependent_entry;

    decl_context_t class_context = class_type_get_inner_context(current_member->type_information);

    while (dependent_parts->next != NULL)
    {
        ERROR_CONDITION(dependent_parts->related_type != NULL, "Dependent part has a related type", 0);

        if (is_dependent_type(get_user_defined_type(current_member)))
        {
            return get_dependent_typename_type_from_parts(current_member,
                    dependent_parts);
        }

        if (class_type_is_incomplete_independent(get_actual_class_type(current_member->type_information)))
        {
            instantiate_template_class(current_member, class_context, filename, line);
        }

        class_context = class_type_get_inner_context(current_member->type_information);

        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Looking for dependent-part '%s'\n", dependent_parts->name);
        }

        scope_entry_list_t* member_list = query_in_scope_str(class_context, dependent_parts->name);

        if (member_list == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Nothing was found for dependent-part '%s'\n", dependent_parts->name);
            }
            return NULL;
        }

        if (entry_list_size(member_list) > 1)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Too many symbols where found for '%s'\n", dependent_parts->name);
            }
            entry_list_free(member_list);
            return NULL;
        }

        scope_entry_t* member = entry_list_head(member_list);
        entry_list_free(member_list);

        if (member->kind == SK_TYPEDEF)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Got a typedef when looking up dependent-part '%s'\n", dependent_parts->name);
            }
            type_t* advanced_type = advance_over_typedefs(member->type_information);

            if (is_named_class_type(advanced_type))
            {
                member = named_type_get_symbol(advanced_type);
            }
        }

        if (member->kind == SK_CLASS)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Got a class when looking up dependent-part '%s'\n", dependent_parts->name);
            }
            if (dependent_parts->template_arguments != NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: But this part has template arguments, so it is not valid\n");
                }
                return NULL;
            }

            current_member = member;
        }
        else if (member->kind == SK_TEMPLATE)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Got a template-name when looking up dependent-part '%s'\n", 
                        dependent_parts->name);
            }

            if (dependent_parts->template_arguments == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: But this part does not have template arguments, so it is not valid\n");
                }
                return NULL;
            }

            // TEMPLATE RESOLUTION
            type_t* template_type = member->type_information;

            // Only template classes
            if (named_type_get_symbol(template_type_get_primary_type(template_type))->kind == SK_FUNCTION)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: The named template is a template function, so it is not valid\n");
                }
                return NULL;
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Requesting specialization '%s'\n", 
                        dependent_parts->name);
            }

            template_parameter_list_t *primary_template_parameters = template_type_get_template_parameters(template_type);
            template_parameter_list_t *updated_template_parameters = duplicate_template_argument_list(dependent_parts->template_arguments);

            updated_template_parameters = complete_arguments_of_template_id(
                    class_context,
                    decl_context,
                    primary_template_parameters,
                    updated_template_parameters,
                    filename, line);

            if (updated_template_parameters->num_parameters != primary_template_parameters->num_parameters)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Template argument count does not match template parameter count\n");
                }
                return NULL;
            }

            type_t* specialized_type = template_type_get_specialized_type(
                    template_type,
                    updated_template_parameters,
                    class_context, line, filename);

            current_member = named_type_get_symbol(specialized_type);
        }
        else 
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Unexpected symbol for part '%s'\n", dependent_parts->name);
            }
            return NULL;
        }

        dependent_parts = dependent_parts->next;
    }

    // Last part
    if (is_dependent_type(get_user_defined_type(current_member)))
    {
        return get_dependent_typename_type_from_parts(current_member,
                dependent_parts);
    }

    if (class_type_is_incomplete_independent(get_actual_class_type(current_member->type_information)))
    {
        instantiate_template_class(current_member, class_context, filename, line);
    }

    class_context = class_type_get_inner_context(current_member->type_information);

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Looking for last dependent-part '%s'\n", dependent_parts->name);
    }

    scope_entry_list_t* member_list = query_in_scope_str(class_context, dependent_parts->name);

    if (member_list == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Nothing was found for dependent-part '%s'\n", dependent_parts->name);
        }
        return NULL;
    }

    if (entry_list_size(member_list) > 1)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Too many symbols where found for '%s'\n", dependent_parts->name);
        }
        entry_list_free(member_list);
        return NULL;
    }

    scope_entry_t* member = entry_list_head(member_list);
    entry_list_free(member_list);

    if (member->kind == SK_CLASS
            || member->kind == SK_TYPEDEF
            || member->kind == SK_ENUM)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Got a typename when looking up dependent-part '%s'\n", dependent_parts->name);
        }
        if (dependent_parts->template_arguments != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: But this part has template arguments, so it is not valid\n");
            }
            return NULL;
        }

        current_member = member;
    }
    else if (member->kind == SK_TEMPLATE)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Got a template-name when looking up dependent-part '%s'\n", 
                    dependent_parts->name);
        }

        if (dependent_parts->template_arguments == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: But this part does not have template arguments, so it is not valid\n");
            }
            return NULL;
        }

        // TEMPLATE RESOLUTION
        type_t* template_type = member->type_information;

        // Only template classes
        if (named_type_get_symbol(template_type_get_primary_type(template_type))->kind == SK_FUNCTION)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: The named template is a template function, so it is not valid\n");
            }
            return NULL;
        }

        template_parameter_list_t* primary_template_parameters = template_type_get_template_parameters(template_type);
        template_parameter_list_t* updated_template_parameters = duplicate_template_argument_list(dependent_parts->template_arguments);

        updated_template_parameters = complete_arguments_of_template_id(
                class_context,
                decl_context,
                primary_template_parameters,
                updated_template_parameters,
                filename, line);

        if (updated_template_parameters->num_parameters != primary_template_parameters->num_parameters)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Template argument count does not match template parameter count\n");
            }
            return NULL;
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: requesting specialization '%s'\n", 
                    dependent_parts->name);
        }

        type_t* specialized_type = template_type_get_specialized_type(
                template_type,
                updated_template_parameters,
                class_context, line, filename);

        current_member = named_type_get_symbol(specialized_type);

    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Unexpected symbol for part '%s'\n", dependent_parts->name);
        }

        return NULL;
    }

    return get_user_defined_type(current_member);
}


static type_t* update_type_aux_(type_t* orig_type, 
        decl_context_t decl_context,
        const char* filename, int line)
{
    ERROR_CONDITION(orig_type == NULL, "Error, type is null", 0);

    if (is_named_type(orig_type))
    {
        scope_entry_t* entry = named_type_get_symbol(orig_type);

        // Nesting level does not have to be checked because 
        // instantiation will do all the work automatically
        //
        // template <typename _T>
        // struct A
        // {
        //   template <typename _Q = _T*>
        //   struct B
        //   {
        //     typedef _T T;
        //   };
        // };
        //
        // A<int>::B<>::T t;
        //
        // "A<int>::" will cause an instantiation with "_T <- int"
        // so this "B<>" is a full template with a default parameter
        // of "int*". So here we are dealing with things that are in
        // the same template nesting level, e.g.:
        //
        // template <typename _T, typename _Q = _T*>
        // struct A
        // {
        // };
        //
        // A<int> it is actually A<int, int*>
        //
        if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
        {
            scope_entry_t* argument = lookup_of_template_parameter(
                    decl_context,
                    entry->entity_specs.template_parameter_nesting,
                    entry->entity_specs.template_parameter_position);

            if (argument == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: No update performed for template parameter '%s'"
                            " since a template argument for it was not found\n",
                            print_declarator(orig_type));
                }
                return orig_type;
            }

            if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                    && (argument->kind == SK_TYPEDEF
                        || argument->kind == SK_TEMPLATE_TYPE_PARAMETER))
            {
                // Sum the qualification
                //
                // template <typename _T, typename _Q = volatile _T>
                // struct A
                // {
                // };
                //
                // "A<const int>" -> "A<const int, const volatile int>"
                cv_qualifier_t cv_qualif = get_cv_qualifier(orig_type);
                if (argument->kind != SK_TEMPLATE_TYPE_PARAMETER)
                {
                    cv_qualif |= get_cv_qualifier(argument->type_information);
                    return get_cv_qualified_type(argument->type_information, cv_qualif);
                }
                else
                {
                    return get_cv_qualified_type(get_user_defined_type(argument), cv_qualif);
                }
            }
            else if (entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                    && (argument->kind == SK_TEMPLATE
                        || argument->kind == SK_TEMPLATE_TEMPLATE_PARAMETER))
            {
                return get_user_defined_type(argument);
            }
            else
            {
                internal_error("Wrong pair template-argument/template-parameter", 0);
            }
        }
        else if (entry->kind == SK_TEMPLATE)
        {
            // A template-name return ummodified
            return orig_type;
        }
        else if (is_template_specialized_type(entry->type_information))
        {
            // Update the arguments of this templated type
            //
            // template <typename _T, typename _Q = A<_T*> >
            // struct B
            // {
            // };

            type_t* template_type = 
                template_specialized_type_get_related_template_type(entry->type_information);
            template_parameter_list_t* primary_template_parameters = template_type_get_template_parameters(template_type);
            scope_entry_t* template_related_symbol =
                template_type_get_related_symbol(template_type);

            if (template_related_symbol != NULL
                    && template_related_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                // This specialized template type comes after a template template parameter,
                // so we have to update it using the template arguments
                // We need to update this template type too
                scope_entry_t* argument = lookup_of_template_parameter(
                        decl_context,
                        template_related_symbol->entity_specs.template_parameter_nesting,
                        template_related_symbol->entity_specs.template_parameter_position);

                ERROR_CONDITION(entry == NULL, "This should not be NULL", 0);

                // Now update the template_type with the new one
                template_type = argument->type_information;
            }

            cv_qualifier_t cv_qualif = CV_NONE;
            advance_over_typedefs_with_cv_qualif(orig_type, &cv_qualif);

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Have to update template specialized type '%s'\n",
                        print_declarator(orig_type));
            }

            template_parameter_list_t* template_parameters = 
                template_specialized_type_get_template_arguments(entry->type_information);

            template_parameter_list_t* updated_template_parameters = duplicate_template_argument_list(template_parameters);

            int i;
            for (i = 0; i < template_parameters->num_parameters; i++)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Updating template argument %d of specialized template class\n", i);
                }
                template_parameter_value_t* updated_argument = update_template_parameter_value(
                        template_parameters->arguments[i],
                        decl_context, filename, line);

                updated_template_parameters->arguments[i] = updated_argument;
            }
            
            // Once the types have been updated, reask for a specialization

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Reasking for specialization\n");
            }
            type_t* updated_specialized = 
                template_type_get_specialized_type(template_type, 
                        updated_template_parameters, 
                        decl_context,
                        line, filename);
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: END OF Reasking for specialization\n");
            }

            DEBUG_CODE()
            {
                scope_entry_t* specialization = named_type_get_symbol(updated_specialized);
                fprintf(stderr, "SCOPE: Specialized type found in template argument updated to '%s' (%p)\n", 
                        print_declarator(updated_specialized),
                        specialization->type_information);
            }

            updated_specialized = get_cv_qualified_type(updated_specialized, cv_qualif);

            return updated_specialized;
        }
        else if (entry->kind == SK_TYPEDEF)
        {
            cv_qualifier_t cv_qualif = get_cv_qualifier(orig_type);
            return get_cv_qualified_type(
                    update_type_aux_(entry->type_information, 
                        decl_context, filename, line),
                    cv_qualif);
        }
        else
        {
            // Return it unmodified
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Not updating type '%s'\n", print_declarator(orig_type));
            }
            return orig_type;
        }
    }
    else if (is_lvalue_reference_type(orig_type))
    {
        type_t* referenced = reference_type_get_referenced_type(orig_type);

        type_t* updated_referenced = update_type_aux_(referenced, decl_context, filename, line);

        if (updated_referenced == NULL)
            return NULL;

        type_t* result_type = get_lvalue_reference_type(updated_referenced);

        return result_type;
    }
    else if (is_pointer_type(orig_type))
    {
        cv_qualifier_t cv_qualifier = get_cv_qualifier(orig_type);

        type_t* pointee = pointer_type_get_pointee_type(orig_type);

        type_t* updated_pointee = update_type_aux_(pointee, decl_context, filename, line);

        if (updated_pointee == NULL)
            return NULL;

        type_t* result_type = get_pointer_type(updated_pointee);

        result_type = get_cv_qualified_type(result_type, cv_qualifier);

        return result_type;
    }
    else if (is_pointer_to_member_type(orig_type))
    {
        cv_qualifier_t cv_qualifier = get_cv_qualifier(orig_type);

        type_t* pointee = pointer_type_get_pointee_type(orig_type);
        type_t* updated_pointee = update_type_aux_(pointee, decl_context, filename, line);

        if (updated_pointee == NULL)
            return NULL;

        type_t* pointee_class = pointer_to_member_type_get_class_type(orig_type);
        pointee_class = update_type_aux_(pointee_class, decl_context, filename, line);

        // If it is not a named class type _and_ it is not a template type
        // parameter, then this is not a valid pointer to member type
        if (!is_named_class_type(pointee_class)
                && (!is_named_type(pointee_class)
                    // if it is a named type it must be a template type parameter
                    || named_type_get_symbol(pointee_class)->kind != SK_TEMPLATE_TYPE_PARAMETER) )
        {
            return NULL;
        }

        type_t* result_type = get_pointer_to_member_type(updated_pointee,
                named_type_get_symbol(pointee_class));

        result_type = get_cv_qualified_type(result_type, cv_qualifier);

        return result_type;
    }
    else if (is_function_type(orig_type))
    {
        cv_qualifier_t cv_qualifier = get_cv_qualifier(orig_type);

        type_t* return_type = function_type_get_return_type(orig_type);
        if (return_type != NULL)
        {
            return_type = update_type_aux_(return_type, decl_context, filename, line);
            // Something went wrong here for the return type
            if (return_type == NULL)
                return NULL;
        }

        parameter_info_t parameter_types[MCXX_MAX_FUNCTION_PARAMETERS];
        memset(parameter_types, 0, sizeof(parameter_types));
        int num_parameters = 0;
        int last = function_type_get_num_parameters(orig_type);

        char has_ellipsis = function_type_get_has_ellipsis(orig_type);

        if (has_ellipsis)
            last--;

        int i;
        for (i = 0; i < last; i++)
        {
            type_t* param_orig_type = function_type_get_parameter_type_num(orig_type, i);

            param_orig_type = update_type_aux_(param_orig_type, 
                    decl_context, filename, line);

            if (param_orig_type == NULL)
                return NULL;

            parameter_info_t parameter_info;

            memset(&parameter_info, 0, sizeof(parameter_info));
            parameter_info.type_info = param_orig_type;

            ERROR_CONDITION(num_parameters >= MCXX_MAX_FUNCTION_PARAMETERS,
                    "Too many parameters", 0);

            parameter_types[num_parameters] = parameter_info;
            num_parameters++;
        }

        if (has_ellipsis)
        {
            parameter_info_t parameter_info;

            memset(&parameter_info, 0, sizeof(parameter_info));
            parameter_info.is_ellipsis = 1;

            ERROR_CONDITION(num_parameters >= MCXX_MAX_FUNCTION_PARAMETERS,
                    "Too many parameters", 0);

            parameter_types[num_parameters] = parameter_info;
            num_parameters++;
        }

        type_t* updated_function_type = get_new_function_type(return_type,
                parameter_types, num_parameters);

        updated_function_type = get_cv_qualified_type(updated_function_type, cv_qualifier);

        return updated_function_type;
    }
    else if (is_array_type(orig_type))
    {
        cv_qualifier_t cv_qualifier = get_cv_qualifier(orig_type);

        nodecl_t array_size = array_type_get_array_size_expr(orig_type);
        decl_context_t array_size_context = array_type_get_array_size_expr_context(orig_type);

        if (nodecl_is_cxx_raw(array_size))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Updating expression '%s' as it is dependent\n",
                        c_cxx_codegen_to_str(array_size));
            }

            // Update the context
            array_size_context = decl_context;

            // NODECL_CXX_RAW have as its zero-th children a C++ expression tree
            AST new_array_size = nodecl_unwrap_cxx_raw(array_size);
            new_array_size = ast_copy_for_instantiation(new_array_size);
            if (!check_expression(new_array_size, array_size_context))
            {
                running_error("%s: error: could not update array dimension",
                        ast_location(new_array_size));
            }

            array_size = expression_get_nodecl(new_array_size);
            if (nodecl_is_cxx_raw(array_size))
            {
                internal_error("%s: After being updated, a dependent expression did not become non-dependent", 
                        nodecl_get_locus(array_size));
            }
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Expression '%s' successfully updated\n",
                        c_cxx_codegen_to_str(array_size));
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Not updating expression '%s' (%s) does not seem to be dependent\n",
                        c_cxx_codegen_to_str(array_size),
                        ast_print_node_type(nodecl_get_kind(array_size)));
            }
        }

        type_t* element_type = array_type_get_element_type(orig_type);
        element_type = update_type_aux_(element_type,
                decl_context, filename, line);

        if (element_type == NULL)
            return NULL;

        type_t* updated_array_type = get_array_type(element_type, 
                array_size, 
                array_size_context);

        updated_array_type = get_cv_qualified_type(updated_array_type, cv_qualifier);

        return updated_array_type;
    }
    else if (is_dependent_typename_type(orig_type))
    {
        scope_entry_t* dependent_entry = NULL;
        dependent_name_part_t* dependent_parts = NULL;

        cv_qualifier_t cv_qualif = get_cv_qualifier(orig_type);

        dependent_typename_get_components(orig_type, 
                &dependent_entry, &dependent_parts);

        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Updating typename '%s'\n",
                    print_declarator(orig_type));
        }

        type_t* fixed_type = NULL;
        fixed_type = update_type_aux_(get_user_defined_type(dependent_entry),
                decl_context, filename, line);

        if (fixed_type == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Dependent type could not be updated\n");
            }
            return NULL;
        }

        if (is_dependent_typename_type(fixed_type))
        {
            cv_qualifier_t cv_qualif_dep = get_cv_qualifier(fixed_type);
            // We are updating the base entry (T) of a dependent typename
            // [T]::T1 with another dependent typename [S]::S2
            // so the updated type should be [S]::S2::T1
            DEBUG_CODE()
             {
                fprintf(stderr, "SCOPE: Entry of dependent type is being "
                        "replaced by another dependent type, appending both\n");
            }

            scope_entry_t* fix_dependent_entry = NULL;
            dependent_name_part_t* fix_dependent_parts = NULL;

            dependent_typename_get_components(fixed_type, 
                    &fix_dependent_entry, &fix_dependent_parts);

            // Now append the dependent parts of this type 

            dependent_name_part_t* updated_dependent_part = copy_dependent_parts(fix_dependent_parts);
            dependent_name_part_t* appended_dependent_part = copy_dependent_parts(dependent_parts);

            // Link both lists
            dependent_name_part_t* it = updated_dependent_part;
            while (it->next != NULL) 
                it = it->next;
            it->next = appended_dependent_part;

            cv_qualif |= cv_qualif_dep;

            fixed_type = get_user_defined_type(fix_dependent_entry);
            dependent_parts = updated_dependent_part;
        }
        else if (!is_named_type(fixed_type))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Dependent type '%s' is not a named type\n",
                        print_declarator(fixed_type));
            }
            return NULL;
        }

        if (is_named_enumerated_type(fixed_type))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Dependent type '%s' is an enumerator\n", 
                        print_declarator(fixed_type));
            }
            return NULL;
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Dependent entity updated to '%s'\n",
                    print_declarator(fixed_type));
        }

        type_t* updated_type =
            update_dependent_typename(fixed_type, dependent_parts, decl_context, filename, line);

        if (updated_type != NULL)
        {
            updated_type = get_cv_qualified_type(updated_type, cv_qualif);
        }

        return updated_type;
    }
    else
    {
        // Fallback
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: No update performed for type '%s'\n",
                    print_declarator(orig_type));
        }
        return orig_type;
    }

    return NULL;
}

type_t* update_type(type_t* orig_type, 
        decl_context_t decl_context,
        const char* filename, int line)
{
    DEBUG_CODE()
    {
            fprintf(stderr, "SCOPE: Updating type '%s'\n", print_declarator(orig_type));
    }

    type_t* result = update_type_aux_(orig_type, decl_context,
            filename, line);

    DEBUG_CODE()
    {
        if (result != NULL)
        {
            fprintf(stderr, "SCOPE: Type '%s' has been updated to '%s'\n", print_declarator(orig_type), print_declarator(result));
        }
        else
        {
            fprintf(stderr, "SCOPE: Type '%s' has been updated to '(null)'\n", print_declarator(orig_type));
        }
    }

    return result;
}

type_t* update_type_for_instantiation(type_t* orig_type,
        decl_context_t context_of_being_instantiated,
        const char* filename, int line)
{
    DEBUG_CODE()
    {
            fprintf(stderr, "SCOPE: While instantiating, updating type '%s'\n", print_declarator(orig_type));
    }

    type_t* result = update_type_aux_(orig_type, context_of_being_instantiated,
            filename, line);

    if (result == NULL)
    {
        running_error("%s:%d: error: type '%s' rendered invalid during instantiation\n",
                filename, line, print_type_str(orig_type, context_of_being_instantiated));
    }

    ERROR_CONDITION(result == NULL, "Invalid type update of '%s' during instantiation", print_declarator(orig_type));
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Type '%s' has been updated to '%s'\n", print_declarator(orig_type), print_declarator(result));
    }

    return result;
}

template_parameter_list_t* get_template_parameters_from_syntax(
        AST template_parameters_list_tree,
        decl_context_t template_parameters_context)
{
    template_parameter_list_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);
    if (template_parameters_list_tree == NULL)
    {
        return result;
    }

    int position = 0;
    AST iter;
    for_each_element(template_parameters_list_tree, iter)
    {
        AST template_parameter = ASTSon1(iter);

        template_parameter_value_t* t_argument = counted_calloc(1, sizeof(*t_argument), &_bytes_used_scopes);

        switch (ASTType(template_parameter))
        {
            case AST_TEMPLATE_EXPRESSION_ARGUMENT :
                {
                    // if (template_parameter_list->parameters[position]->kind != TPK_NONTYPE)
                    // {
                    //     DEBUG_CODE()
                    //     {
                    //         fprintf(stderr, "Got a non-type template argument but parameter %d "
                    //                 "is not a nontype template parameter\n",
                    //                 position);
                    //     }
                    //     return 0;
                    // }
                    AST expr = ASTSon0(template_parameter);

                    // FIXME - Maybe this expr has already been checked in another context
                    check_expression(expr, template_parameters_context);

                    type_t* expr_type = expression_get_type(expr);

                    t_argument->value = expression_get_nodecl(expr);

                    if (expression_is_constant(expr))
                    {
                        t_argument->value = const_value_to_nodecl(expression_get_constant(expr));
                    }

                    t_argument->type = expr_type;
                    t_argument->kind = TPK_NONTYPE;
                    break;
                }
            case AST_TEMPLATE_TYPE_ARGUMENT :
                {
                    AST type_template_parameter = ASTSon0(template_parameter);
                    AST type_specifier_seq = ASTSon0(type_template_parameter);
                    AST abstract_decl = ASTSon1(type_template_parameter);

                    // A type_specifier_seq is essentially a subset of a
                    // declarator_specifier_seq so we can reuse existing functions
                    type_t* type_info;
                    gather_decl_spec_t gather_info;
                    memset(&gather_info, 0, sizeof(gather_info));

                    nodecl_t dummy_nodecl_output = nodecl_null();
                    build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                            template_parameters_context, 
                            &dummy_nodecl_output);

                    type_t* declarator_type;
                    compute_declarator_type(abstract_decl, &gather_info, type_info, &declarator_type,
                            template_parameters_context, &dummy_nodecl_output);

                    if (is_template_type(declarator_type))
                    {
                        t_argument->kind = TPK_TEMPLATE;
                    }
                    else
                    {
                        t_argument->kind = TPK_TYPE;
                    }
                    t_argument->type = declarator_type;
                    break;
                }
            case AST_AMBIGUITY :
                {
                    internal_error("Ambiguous node", 0);
                }
            default:
                {
                    internal_error("Invalid node %s", ast_print_node_type(ASTType(template_parameter)));
                }
        }
        
        int num_parameters = result->num_parameters;
        // Empty parameter, it will be filled later
        P_LIST_ADD(result->parameters, 
                num_parameters,
                NULL);
        P_LIST_ADD(result->arguments, 
                result->num_parameters,
                t_argument);
    }

    return result;
}

static void update_unresolved_overloaded_type(type_t* unresolved_type, type_t* solved_type, AST tree)
{
    if (tree != NULL)
    {
        if (expression_get_type(tree) == unresolved_type)
        {
            expression_set_type(tree, solved_type);
        }

        int i;
        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            update_unresolved_overloaded_type(unresolved_type, solved_type, ASTChild(tree, i));
        }
    }
}

static template_parameter_list_t* complete_arguments_of_template_id(
        decl_context_t template_name_context,
        decl_context_t template_parameters_context,
        template_parameter_list_t* primary_template_parameters,
        template_parameter_list_t* template_parameters,
        const char* filename,
        int line)
{
    internal_error("Not yet implemented", 0);
#if 0
    template_parameter_list_t* result = template_parameters;

    int num_argument;
    decl_context_t updated_decl_context = template_name_context;

    updated_decl_context.template_parameters = counted_calloc(1, sizeof(*updated_decl_context.template_parameters), &_bytes_used_symbols);
    if (template_name_context.template_parameters != NULL)
    {
        updated_decl_context.template_parameters->enclosing = template_name_context.template_parameters->enclosing;
    }

    for (num_argument = 0; num_argument < result->num_arguments; num_argument++)
    {
        template_parameter_t* current_template_parameter = result->argument_list[num_argument];

        if (current_template_parameter->kind == TAK_NONTYPE)
        {
            current_template_parameter->type = update_type(
                    template_parameters->parameters[num_argument]->entry->type_information,
                    updated_decl_context,
                    filename, line);
            current_template_parameter->expression_context = template_parameters_context;

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Type of nontype template parameter updated to '%s'\n",
                        print_declarator(current_template_parameter->type));
            }

            /*
             * If the type is an address of function try to solve it
             */
            if (expression_get_type(current_template_parameter->expression) != NULL
                    && is_unresolved_overloaded_type(expression_get_type(current_template_parameter->expression)))
            {
                // Try to solve it
                scope_entry_list_t* unresolved_set = 
                            unresolved_overloaded_type_get_overload_set(expression_get_type(current_template_parameter->expression));

                scope_entry_t* solved_function =
                    address_of_overloaded_function(
                            unresolved_set,
                            unresolved_overloaded_type_get_explicit_template_parameters(expression_get_type(current_template_parameter->expression)),
                            current_template_parameter->type,
                            updated_decl_context,
                            filename,
                            line);
                entry_list_free(unresolved_set);

                if (solved_function != NULL)
                {
                    // Update the type throughout the expression (this is needed when evaluating it)
                    update_unresolved_overloaded_type(expression_get_type(current_template_parameter->expression),
                            solved_function->type_information,
                            current_template_parameter->expression);
                }
            }

            ERROR_CONDITION(current_template_parameter->type == NULL, "Could not update properly template argument", 0);
        }
    }

    if (num_argument < template_parameters->num_parameters)
    {
        // Complete with default template arguments
        for (; num_argument < template_parameters->num_parameters; num_argument++)
        {
            template_parameter_t* template_parameter = 
                template_parameters->parameters[num_argument];

            ERROR_CONDITION(!template_parameter->has_default_argument,
                    "Template parameter '%d' lacks a default argument",
                    num_argument);

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Updating template argument %d\n", 
                        num_argument);
            }

            template_parameter_t* original_default_arg = template_parameter->default_template_parameter;

            template_parameter_t* current_template_parameter = update_template_parameter(
                    original_default_arg, 
                    updated_decl_context, 
                    filename, line,
                    /* overwrite_context */ 0);

            P_LIST_ADD(result->argument_list, result->num_arguments, current_template_parameter);

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Template argument updated\n");
            }
        }
    }

    return result;
#endif
}

static template_parameter_list_t *get_template_parameters_of_template_id(
        AST template_id,
        type_t* template_type,
        decl_context_t template_name_context,
        decl_context_t template_parameters_context,
        char *valid)
{
    // Solve any pending ambiguity
    if (!solve_possibly_ambiguous_template_id(template_id, template_parameters_context))
    {
        *valid = 0;
        return NULL;
    }

    template_parameter_list_t* primary_template_parameters =
        template_type_get_template_parameters(template_type);

    int nesting_level = get_template_nesting_of_context(template_name_context);

    AST template_parameters_list_tree = ASTSon1(template_id);

    // Get the types raw from the syntax
    template_parameter_list_t *template_parameters = get_template_parameters_from_syntax(template_parameters_list_tree, 
            template_parameters_context);

    if (template_parameters->num_parameters > primary_template_parameters->num_parameters)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Too many template arguments %d > %d", 
                    template_parameters->num_parameters, 
                    primary_template_parameters->num_parameters);
        }

        *valid = 0;
        return NULL;
    }

    decl_context_t new_template_context = template_name_context;
    new_template_context.template_parameters = template_parameters;

    // Now review template parameters
    int i;
    for (i = 0; i < primary_template_parameters->num_parameters; i++)
    {
        if (i >= template_parameters->num_parameters)
        {
            if (primary_template_parameters->arguments[i] == NULL)
            {
                // One of the template parameters is lacking an argument
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Template argument %d is missing", i);
                }
                *valid = 0;
                return NULL;
            }
            else
            {
                int num_parameters = template_parameters->num_parameters;
                P_LIST_ADD(template_parameters->parameters,
                        num_parameters,
                        primary_template_parameters->parameters[i]);
                template_parameter_value_t* v = update_template_parameter_value(primary_template_parameters->arguments[i],
                        new_template_context,
                        ASTFileName(template_id), ASTLine(template_id));
                P_LIST_ADD(template_parameters->arguments, template_parameters->num_parameters, v);
            }
        }
        else
        {
            // Set the template parameter
            template_parameters->parameters[i] = primary_template_parameters->parameters[i];

            // And check it matches what we got
            if (template_parameters->parameters[i]->kind != template_parameters->arguments[i]->kind)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Template parameter kind and template argument kind do not match\n");
                    *valid = 0;
                    return NULL;
                }
            }
        }
    }

    *valid = 1;
    return template_parameters;
}

// This function never instantiates a template, it might create a specialization though
static scope_entry_list_t* query_template_id_aux(AST template_id, 
        decl_context_t template_name_context,
        decl_context_t template_parameters_context,
        scope_entry_list_t* (*query_function)(decl_context_t, const char*, const char*, int))
{
    ERROR_CONDITION(ASTType(template_id) != AST_TEMPLATE_ID
            && ASTType(template_id) != AST_OPERATOR_FUNCTION_ID_TEMPLATE, 
            "This is not a valid template-id", 0);

    template_name_context.decl_flags |= DF_NO_INJECTED_CLASS_NAME;

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Looking up template-id '%s'\n", prettyprint_in_buffer(template_id));
    }

    // First lookup the template-name in the template-name context
    const char *template_name = NULL;
    if (ASTType(template_id) == AST_TEMPLATE_ID)
    {
        AST template_name_tree = ASTSon0(template_id);
        template_name = ASTText(template_name_tree);
    }
    else if (ASTType(template_id) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
    {
        template_name = get_operator_function_name(template_id);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    scope_entry_t* template_symbol = NULL;
    scope_entry_list_t* template_symbol_list = NULL;

    if (is_template_parameter_name(template_id))
    {
        template_symbol = lookup_template_parameter_name(template_name_context, template_id);
    }
    else
    {
        template_symbol_list = query_function(template_name_context, template_name, 
                ASTFileName(template_id), ASTLine(template_id));

        // Filter template-names
        enum cxx_symbol_kind template_name_filter[] = {
            SK_TEMPLATE_TEMPLATE_PARAMETER,
            SK_TEMPLATE,
        };

        template_symbol_list = filter_symbol_kind_set(template_symbol_list, 
                STATIC_ARRAY_LENGTH(template_name_filter), 
                template_name_filter);

        if (template_symbol_list == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: No template-name '%s' was found in this scope\n", template_name);
            }
            return NULL;
        }

        template_symbol = entry_list_head(template_symbol_list);
    }

    type_t* generic_type = template_symbol->type_information;

    type_t* primary_type = 
        named_type_get_symbol(template_type_get_primary_type(generic_type))->type_information;

    type_t* specialized_type = NULL;
    if (is_unnamed_class_type(primary_type))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: This is a template class-name\n");
        }

        // Now compute the type of the template arguments
        char is_valid = 0;
        template_parameter_list_t* template_parameters = get_template_parameters_of_template_id(
                template_id,
                generic_type, 
                template_name_context, 
                template_parameters_context, 
                &is_valid);

        if (!is_valid)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Template arguments of '%s' are not valid for template-name '%s'\n", 
                        prettyprint_in_buffer(ASTSon1(template_id)),
                        template_name);
            }
            return NULL;
        }

        specialized_type = template_type_get_specialized_type(generic_type, 
                template_parameters,
                template_name_context,
                ASTLine(template_id), 
                ASTFileName(template_id));

        if (specialized_type != NULL)
        {

            if (template_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                set_as_template_parameter_name(template_id, template_symbol);
            }

            ERROR_CONDITION(!is_named_type(specialized_type), "This should be a named type", 0);

            // Crappy
            scope_entry_list_t* result = entry_list_new(named_type_get_symbol(specialized_type));
            return result;
        }
        else
        {
            return NULL;
        }
    }
    else if (is_function_type(primary_type))
    {
        // Now we have to solve the best function
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: This is a template function-name\n");
        }
        
        // Solve any pending ambiguity
        if (!solve_possibly_ambiguous_template_id(template_id, template_parameters_context))
        {
            return NULL;
        }
        
        // Let the user of this function select the proper template
        return template_symbol_list;
    }
    else 
    {
        internal_error("Invalid templated type", 0);
    }
    
    return NULL;
}

static scope_entry_list_t* query_template_id(AST template_id, 
        decl_context_t template_name_context,
        decl_context_t template_parameters_context)
{
    return query_template_id_aux(template_id,
            template_name_context,
            template_parameters_context,
            name_lookup);
}

// Only for "simple" symbols, this is, that are not members and they are simply contained
// in a nest of namespaces
static const char* get_fully_qualified_symbol_name_simple(decl_context_t decl_context, 
        const char* current_qualif_name)
{
    const char* result = current_qualif_name;

    scope_t* current_scope = decl_context.current_scope;

    if (current_scope->kind == NAMESPACE_SCOPE)
    {
        while (current_scope != NULL)
        {
            if (current_scope->related_entry != NULL
                    && current_scope->related_entry->symbol_name != NULL
                    && strcmp(current_scope->related_entry->symbol_name, "(unnamed)") != 0)
            {
                const char* nested_name = strappend(current_scope->related_entry->symbol_name, "::");
                result = strappend(nested_name, result);
            }

            current_scope = current_scope->contained_in;
        }

        CXX_LANGUAGE()
        {
            result = strappend("::", result);
        }
    }

    return result;
}

const char* get_template_arguments_str(scope_entry_t* entry, 
        decl_context_t decl_context)
{
    const char* result = "";

    // It is not enough with the name, we have to print the arguments
    result = strappend(result, "<");
    template_parameter_list_t* template_parameters = template_specialized_type_get_template_arguments(entry->type_information);

    int i;
    for (i = 0; i < template_parameters->num_parameters; i++)
    {
        if (i != 0)
        {
            result = strappend(result, ", ");
        }

        template_parameter_value_t* argument = template_parameters->arguments[i];

        if (argument == NULL)
        {
            result = strappend(result, template_parameters->parameters[i]->entry->symbol_name);
            continue;
        }

        const char* argument_str = NULL;
        switch (argument->kind)
        {
            // Print the type
            case TPK_TEMPLATE:
            case TPK_TYPE:
                {
                    const char* abstract_declaration;

                    abstract_declaration = 
                        get_declaration_string_internal(argument->type, decl_context, "", "", 0, 0, NULL, 0);

                    argument_str = abstract_declaration;
                    break;
                }
            case TPK_NONTYPE:
                {
                    argument_str = c_cxx_codegen_to_str(argument->value);
                    break;
                }
            default:
                {
                    internal_error("Undefined template argument\n", 0);
                    break;
                }
        }

        if (result[strlen(result) - 1] == '<'
               && argument_str != NULL
               && argument_str[0] == ':')
        {
            result = strappend(result, " ");
        }

        result = strappend(result, argument_str);
    }

    if (result[strlen(result) - 1] == '>')
    {
        result = strappend(result, " >");
    }
    else
    {
        result = strappend(result, ">");
    }

    return result;
}

const char* unmangle_symbol_name(scope_entry_t* entry)
{
    const char* name = entry->symbol_name;
    // constructor A
    if ((strlen(name) > strlen("constructor "))
            && (strncmp(name, "constructor ", strlen("constructor ")) == 0))
    {
        return uniquestr(name + strlen("constructor "));
    }
    else if (entry->entity_specs.is_conversion)
    {
        return strappend("operator ", 
                print_type_str(function_type_get_return_type(entry->type_information), entry->decl_context));
    }
    return name;
}

// Get the fully qualified symbol name in the scope of the ocurrence
static const char* get_fully_qualified_symbol_name_ex(scope_entry_t* entry, 
        decl_context_t decl_context, char* is_dependent, int* max_qualif_level,
        char no_templates)
{
    // DEBUG_CODE()
    // {
    //     fprintf(stderr, "SCOPE: Getting fully qualified symbol name for '%s'\n", entry->symbol_name);
    // }

    // If this is the injected symbol, ignore it and get the real entry
    if (entry->entity_specs.is_injected_class_name)
    {
        entry = entry->entity_specs.injected_class_referred_symbol;
    }

    const char* result = uniquestr(unmangle_symbol_name(entry));

    char current_has_template_parameters = 0;

    if (entry->kind == SK_TEMPLATE_PARAMETER
            || entry->kind == SK_TEMPLATE_TYPE_PARAMETER
            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        // This symbol must be looked up for the proper real name
        result = entry->symbol_name;

        // This is obviously dependent
        (*is_dependent) |= 1;
        return result;
    }
    else if (!no_templates
            && entry->type_information != NULL
            && is_template_specialized_type(entry->type_information)
            && template_specialized_type_get_template_arguments(entry->type_information) != NULL)
    {
        current_has_template_parameters = 1;
        const char *template_parameters = get_template_arguments_str(entry, decl_context);
        result = strappend(result, template_parameters);

        (*is_dependent) |= is_dependent_type(entry->type_information);
    }

    if (entry->entity_specs.is_member)
    {
        // We need the qualification of the class
        ERROR_CONDITION(!is_named_class_type(entry->entity_specs.class_type), "The class of a member must be named", 0);

        scope_entry_t* class_symbol = named_type_get_symbol(entry->entity_specs.class_type);

        (*max_qualif_level)++;

        char prev_is_dependent = 0;
        const char* class_qualification = 
            get_fully_qualified_symbol_name(class_symbol, decl_context, &prev_is_dependent, max_qualif_level);

        class_qualification = strappend(class_qualification, "::");

        if (prev_is_dependent
                && current_has_template_parameters)
        {
            class_qualification = strappend(class_qualification, "template ");
        }

        (*is_dependent) |= prev_is_dependent;

        result = strappend(class_qualification, result);
    } 
    else if (!entry->entity_specs.is_member)
    {
        // This symbol is already simple enough
        result = get_fully_qualified_symbol_name_simple(entry->decl_context, result);
    }

    return result;
}

const char* get_fully_qualified_symbol_name(scope_entry_t* entry, 
        decl_context_t decl_context, char* is_dependent, int* max_qualif_level)
{
    return get_fully_qualified_symbol_name_ex(entry,
            decl_context, is_dependent, max_qualif_level, /* no_templates */ 0);
}

const char* get_fully_qualified_symbol_name_without_template(scope_entry_t* entry, 
        decl_context_t decl_context, char* is_dependent, int* max_qualif_level)
{
    return get_fully_qualified_symbol_name_ex(entry,
            decl_context, is_dependent, max_qualif_level, /* no_templates */ 1);
}

const char* get_qualified_symbol_name(scope_entry_t* entry, decl_context_t decl_context)
{
    int max_qualif_level = 0;
    char is_dependent = 0;

    return get_fully_qualified_symbol_name(entry, decl_context, &is_dependent, &max_qualif_level);
}

decl_context_t decl_context_empty()
{
    decl_context_t result;
    memset(&result, 0, sizeof(result));
    return result;
}

// FIXME - This function should be removed some day. It exists solely for UDR
// lookups...
//
// It is a simplified clone of name lookup but checks enclosing LEXICAL scopes
// even if we have found something in the current one
//
// Note that cascade lookup DOES NOT take into account used namespaces or
// template scopes
scope_entry_list_t* cascade_lookup(decl_context_t decl_context, const char* name, 
        const char* filename, int line)
{
    ERROR_CONDITION(name == NULL, "Name cannot be null!", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Name lookup of '%s'\n", name);
    }

    scope_entry_list_t* result = NULL;

    int num_associated_namespaces = 0;
    scope_entry_t* associated_namespaces[MCXX_MAX_ASSOCIATED_NAMESPACES] = { 0 };

    scope_t* current_scope = decl_context.current_scope;

    while (current_scope != NULL)
    {
        if (current_scope->kind == CLASS_SCOPE
                && !BITMAP_TEST(decl_context.decl_flags, DF_ONLY_CURRENT_SCOPE))
        {
            scope_entry_list_t* old_result = result;
            scope_entry_list_t* current_class = query_in_class(current_scope, name, decl_context.decl_flags, 
                        filename, line);
            result = entry_list_merge(old_result, current_class);
            entry_list_free(old_result);
            entry_list_free(current_class);
        }
        else if (current_scope->kind == NAMESPACE_SCOPE)
        {
            scope_entry_list_t* old_result = result;
            scope_entry_list_t* current_namespace = query_in_namespace_and_associates(
                    current_scope->related_entry,
                    name, 0, num_associated_namespaces,
                    associated_namespaces, decl_context.decl_flags,
                    filename, line);
            result = entry_list_merge(old_result, current_namespace);

            entry_list_free(old_result);
            entry_list_free(current_namespace);
            num_associated_namespaces = 0;
        }
        else // BLOCK_SCOPE || PROTOTYPE_SCOPE || FUNCTION_SCOPE (although its contains should be NULL)
        {
            scope_entry_list_t* current_scope_list = query_name_in_scope(current_scope, name);
            scope_entry_list_t* old_result = result;
            result = entry_list_merge(old_result, current_scope_list);
            entry_list_free(old_result);
            entry_list_free(current_scope_list);
        }

        if (BITMAP_TEST(decl_context.decl_flags, DF_ELABORATED_NAME))
        {
            scope_entry_list_t* old_result = result;
            result = filter_any_non_type(old_result);
            entry_list_free(old_result);
        }

        if (BITMAP_TEST(decl_context.decl_flags, DF_ONLY_CURRENT_SCOPE))
        {
            return result;
        }

        current_scope = current_scope->contained_in;
    }

    return result;
}

scope_entry_t* lookup_of_template_parameter(decl_context_t context,
        int template_parameter_nesting, 
        int template_parameter_position)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Looking for template parameter (%d, %d)\n",
                template_parameter_nesting,
                template_parameter_position);
    }
    ERROR_CONDITION(template_parameter_nesting <= 0, "Nesting must be at least 1", 0);

    template_parameter_t** levels[MCXX_MAX_TEMPLATE_NESTING_LEVELS] = { 0 };
    template_parameter_value_t** value_levels[MCXX_MAX_TEMPLATE_NESTING_LEVELS] = { 0 };
    int num_items[MCXX_MAX_TEMPLATE_NESTING_LEVELS] = { 0 };

    template_parameter_list_t *template_parameters = context.template_parameters;

    int i = 0;
    while (template_parameters != NULL)
    {
        if (template_parameters->parameters != NULL)
        {
            ERROR_CONDITION(i == MCXX_MAX_TEMPLATE_NESTING_LEVELS, "Too many template nesting levels", 0);
            levels[i] = template_parameters->parameters;
            value_levels[i] = template_parameters->arguments;
            num_items[i] = template_parameters->num_parameters;
            i++;
        }

        template_parameters = template_parameters->enclosing;
    }

    // Nesting is too deep
    if (template_parameter_nesting > i)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Template parameter not found: nesting %d is too deep\n", template_parameter_nesting);
        }
        return NULL;
    }

    template_parameter_t** current_nesting = levels[i - template_parameter_nesting];
    template_parameter_value_t** current_values = value_levels[i - template_parameter_nesting];
    int current_num_items = num_items[i - template_parameter_nesting];
    
    // This position does not exist
    if (current_num_items < (template_parameter_position + 1))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Template parameter not found: there are only %d parameters but position %d was requested\n", 
                    current_num_items, 
                    template_parameter_position);
        }
        return NULL;
    }

    scope_entry_t* parameter_entry = current_nesting[template_parameter_position]->entry;
    ERROR_CONDITION(parameter_entry == NULL, "Invalid symbol", 0);

    template_parameter_value_t* value = current_values[template_parameter_position];
    if (value != NULL
            && !value->is_default)
    {
        if (value->entry == NULL)
        {
            value->entry = counted_calloc(1, sizeof(*value->entry), &_bytes_used_scopes);
            value->entry->symbol_name = parameter_entry->symbol_name;
            value->entry->decl_context = context;
            value->entry->entity_specs.is_template_parameter = 1;

            switch (value->kind)
            {
                case TPK_NONTYPE:
                    {
                        value->entry->kind = SK_VARIABLE;
                        value->entry->type_information = value->type;
                        value->entry->value = value->value;
                        break;
                    }
                case TPK_TYPE:
                    {
                        value->entry->kind = SK_TYPEDEF;
                        value->entry->type_information = value->type;
                        break;
                    }
                case TPK_TEMPLATE:
                    {
                        value->entry->kind = SK_TEMPLATE;
                        value->entry->type_information = value->type;
                        value->entry->type_information = 
                            named_type_get_symbol(value->type)->type_information;
                        break;
                    }
                default:
                    internal_error("Unexpected value kind", 0);
                    break;
            }
        }
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Template parameter (%d, %d) found with value. Name '%s'\n",
                    template_parameter_nesting,
                    template_parameter_position,
                    value->entry->symbol_name);
        }
        return value->entry;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Template parameter (%d, %d) found. Name '%s'\n",
                template_parameter_nesting,
                template_parameter_position,
                parameter_entry->symbol_name);
    }

    return parameter_entry;
}

void set_as_template_parameter_name(AST a, scope_entry_t* template_param_sym)
{
    ERROR_CONDITION(template_param_sym == NULL, "template parameter symbol cannot be NULL", 0);

    ASTAttrSetValueType(a, LANG_IS_TEMPLATE_PARAMETER_NAME, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(a, LANG_TEMPLATE_PARAMETER_NAME_SYMBOL, tl_type_t, tl_symbol(template_param_sym));
}

char is_template_parameter_name(AST a)
{
    tl_type_t* t = (tl_type_t*)(ASTAttrValue(a, LANG_IS_TEMPLATE_PARAMETER_NAME));
    if (t == NULL)
        return 0;
    return t->data._boolean;
}

char is_unqualified_id_expression(AST a)
{
    return a != NULL
        && (ASTType(a) == AST_SYMBOL
                || ASTType(a) == AST_TEMPLATE_ID
                || ASTType(a) == AST_CONVERSION_FUNCTION_ID
                || ASTType(a) == AST_DESTRUCTOR_ID
                || ASTType(a) == AST_DESTRUCTOR_TEMPLATE_ID
                || ASTType(a) == AST_OPERATOR_FUNCTION_ID
                || ASTType(a) == AST_OPERATOR_FUNCTION_ID_TEMPLATE);
}

static char is_inline_namespace_of_(scope_t* inner_namespace_scope, scope_t* outer_namespace_scope)
{
    if (inner_namespace_scope == NULL)
        return 0;

    scope_entry_t* inner_namespace_sym = inner_namespace_scope->related_entry;

    if (inner_namespace_sym == NULL)
        return 0;

    scope_t* contained_in = inner_namespace_scope->contained_in;

    if (inner_namespace_sym->entity_specs.is_inline
                && (contained_in == outer_namespace_scope))
    {
        return 1;
    }
    else if (inner_namespace_sym->entity_specs.is_inline)
    {
        return is_inline_namespace_of_(contained_in, outer_namespace_scope);
    }
    else 
    {
        return 0;
    }
}

char is_inline_namespace_of(decl_context_t inner_namespace_ctx, decl_context_t outer_namespace_ctx)
{
    return is_inline_namespace_of_(inner_namespace_ctx.namespace_scope, 
            outer_namespace_ctx.namespace_scope);
}

static const char* symbol_kind_table_str[] =
{
    [SK_UNDEFINED] = "<<invalid symbol>>",
#define SYMBOL_KIND(x, _) \
        [x] = #x, 
    SYMBOL_KIND_TABLE
#ifdef FORTRAN_SUPPORT
    SYMBOL_KIND_TABLE_FORTRAN
#endif
#undef SYMBOL_KIND
};

const char* symbol_kind_name(scope_entry_t* entry)
{
    if (entry == NULL)
        return "<<null symbol>>";
    else if (entry->kind >= SK_LAST_KIND)
        return "<<invalid kind>>";
    else
        return symbol_kind_table_str[entry->kind];
}

struct fun_adaptor_data_tag
{
    void *data;
    void (*fun)(scope_entry_list_t*, void*);
};

static void for_each_fun_adaptor(const void* key UNUSED_PARAMETER, void* info, void* data)
{
    scope_entry_list_t* it = (scope_entry_list_t*)info;
    struct fun_adaptor_data_tag * adaptor_data = (struct fun_adaptor_data_tag*) data;

    (adaptor_data->fun)(entry_list_copy(it), adaptor_data->data);
}

void scope_for_each_entity(scope_t* sc, void *data, void (*fun)(scope_entry_list_t*, void*))
{
    struct fun_adaptor_data_tag fun_adaptor_data = { .data = data, .fun = fun };

    rb_tree_walk(sc->hash, for_each_fun_adaptor, &fun_adaptor_data);
}

scope_entry_t* new_scope_symbol(decl_context_t decl_context)
{
    scope_entry_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);

    char c[256];
    snprintf(c, 255, "<<scoping symbol %p>>", decl_context.current_scope);
    c[255] = '\0';

    result->kind = SK_SCOPE;
    result->symbol_name = uniquestr(c);
    result->decl_context = decl_context;

    return result;
}

int get_template_nesting_of_context(decl_context_t decl_context)
{
    template_parameter_list_t* template_parameters = decl_context.template_parameters;
    return get_template_nesting_of_template_parameters(template_parameters);
}

int get_template_nesting_of_template_parameters(template_parameter_list_t* template_parameters)
{
    int nesting = 0;
    while (template_parameters != NULL)
    {
        if (template_parameters->parameters != NULL)
        {
            nesting++;
        }
        template_parameters = template_parameters->enclosing;
    }

    return nesting;
}
