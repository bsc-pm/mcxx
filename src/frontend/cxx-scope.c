/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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
#include "red_black_tree.h"


static unsigned long long _bytes_used_scopes = 0;

extensible_schema_t scope_entry_extensible_schema = EMPTY_EXTENSIBLE_SCHEMA;

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

// Solve a template given a template-id, a list of found names for the template-id and the declaration context

// Looks up the qualification scope for a nested-name-spec
static decl_context_t lookup_qualification_scope(
        decl_context_t original_context,
        decl_context_t decl_context, 
        AST nested_name, 
        AST unqualifed_part, 
        type_t** dependent_type,
        char *is_valid);

static decl_context_t lookup_qualification_scope_in_namespace(
        decl_context_t original_context,
        decl_context_t nested_name_context, 
        scope_entry_t* namespace, 
        AST nested_name_spec, 
        AST unqualified_part, 
        type_t **is_dependent, 
        char *is_valid);
static decl_context_t lookup_qualification_scope_in_class(
        decl_context_t original_context,
        decl_context_t nested_name_context, 
        scope_entry_t* class_name, 
        AST nested_name_spec, 
        AST unqualified_part, 
        type_t **is_dependent, 
        char *is_valid);

static scope_entry_list_t* query_template_id(AST template_id, 
        decl_context_t template_name_context,
        decl_context_t template_arguments_context);

static scope_entry_list_t* query_qualified_name(decl_context_t decl_context,
        AST global_op,
        AST nested_name,
        AST unqualified_name);

// Scope creation functions
static scope_t* new_scope(void);
static scope_t* new_namespace_scope(scope_t* st, scope_entry_t* related_entry);
static scope_t* new_prototype_scope(scope_t* st);
static scope_t* new_block_scope(scope_t* enclosing_scope);
static scope_t* new_class_scope(scope_t* enclosing_scope, scope_entry_t* class_entry);
static scope_t* new_template_scope(scope_t* enclosing_scope);

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
static scope_t* new_scope(void)
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
    scope_t* result = new_scope();

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
    scope_t* result = new_scope();

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
    scope_t* result = new_scope();

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
    scope_t* result = new_scope();
    
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
    scope_t* result = new_scope();

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

// A template scope is an aside scope where all template parameters are stored
// it is inherited everywhere and only created/updated in templated declarations
// They form a specific stack of template_scopes and they are never current_scope
// (so functions working on template parameters must work explicitly in the template_scope
// of decl_context_t)
static scope_t* new_template_scope(scope_t* enclosing_scope)
{
    scope_t* result = new_scope();
    result->kind = TEMPLATE_SCOPE;
    result->contained_in = enclosing_scope;

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: New template scope '%p' created\n", result);
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

    global_scope_namespace->namespace_decl_context = result;

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

decl_context_t new_template_context(decl_context_t enclosing_context)
{
    ERROR_CONDITION(enclosing_context.template_scope != NULL
            && enclosing_context.template_scope->kind != TEMPLATE_SCOPE,
            "Enclosing template-scope is not a template scope", 0);

    // Inherit the current decl context
    decl_context_t result = enclosing_context;

    // And stack another template_scope
    result.template_scope = new_template_scope(enclosing_context.template_scope);
    // But do not make it the current! Remember, it is an aside template
    // orthogonal to the current context

    DEBUG_CODE()
    {
        int n = 0;
        scope_t* template_scope = result.template_scope;

        while (template_scope != NULL)
        {
            n++;
            template_scope = template_scope->contained_in;
        }

        fprintf(stderr, "SCOPE: Template context has depth '%d'\n", n);
    }

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
        scope_entry_list_t* new_set = (scope_entry_list_t*) counted_calloc(1, sizeof(*new_set), &_bytes_used_symbols);

        // Put the new entry in front of the previous
        *new_set = *result_set;

        result_set->next = new_set;
        result_set->entry = entry;
    }
    else
    {
        result_set = (scope_entry_list_t*) counted_calloc(1, sizeof(*result_set), &_bytes_used_symbols);
        result_set->entry = entry;
        result_set->next = NULL; // redundant, though

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
    extensible_struct_init(result->extended_data, &scope_entry_extensible_schema);

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
    [TEMPLATE_SCOPE] = "TEMPLATE_SCOPE",
};

static scope_entry_list_t* query_name_in_scope(scope_t* sc, const char* name)
{
    DEBUG_CODE()
    {
        if (sc->related_entry == NULL)
        {
        fprintf(stderr, "SCOPE: Looking symbol '%s' in %s (scope=%p, hash=%p)...\n", 
                name, scope_names[sc->kind], sc, sc->hash);
        }
        else
        {
        fprintf(stderr, "SCOPE: Looking symbol '%s' in %s (qualification=%s, scope=%p, hash=%p)...\n", 
                name, scope_names[sc->kind], 
                sc->related_entry->symbol_name,
                sc, sc->hash);
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
            fprintf(stderr, "SCOPE: Symbol '%s' NOT found in scope '%p' [%p]\n", name, sc, sc->hash);
        }
        else
        {
            fprintf(stderr, "SCOPE: Symbol '%s' found in scope '%p' [%p]\n", name, sc, sc->hash);
        }
    }

    return result;
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
        // Do not add it twice
        scope_entry_list_t* it = result_set;
        while (it != NULL)
        {
            if (it->entry == entry)
                return;

            it = it->next;
        }

        scope_entry_list_t* new_set = (scope_entry_list_t*) counted_calloc(1, sizeof(*new_set), &_bytes_used_scopes);

        // Put the new entry in front of the previous
        *new_set = *result_set;

        result_set->next = new_set;
        result_set->entry = entry;
    }
    else
    {
        result_set = (scope_entry_list_t*) counted_calloc(1, sizeof(*result_set), &_bytes_used_scopes);
        result_set->entry = entry;
        result_set->next = NULL; // redundant, though

        rb_tree_add(sc->hash, entry->symbol_name, result_set);
    }
}



void remove_entry(scope_t* sc, scope_entry_t* entry)
{
    ERROR_CONDITION((entry->symbol_name == NULL), "Removing a symbol entry without name!", 0);

    scope_entry_list_t* result_set = NULL;

    rb_red_blk_node* n = rb_tree_query(sc->hash, entry->symbol_name);
    if (n != NULL)
    {
        result_set = (scope_entry_list_t*)rb_node_get_info(n);
    }

    scope_entry_list_t* current = result_set;
    scope_entry_list_t* previous = NULL;

    while (current != NULL)
    {
        if (current->entry == entry)
        {
            if (previous != NULL)
            {
                // Unlink from the structure
                previous->next = current->next;
            }
            else
            {
                // Delete the whole entry
                rb_tree_delete(sc->hash, n);
            }
            break;
        }

        previous = current;
        current = current->next;
    }
}

scope_entry_list_t* create_list_from_entry(scope_entry_t* entry)
{
    scope_entry_list_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);
    result->entry = entry;
    result->next = NULL;

    return result;
}

scope_entry_list_t* filter_symbol_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set)
{
    scope_entry_list_t* result = NULL;
    scope_entry_list_t* iter = entry_list;
    
    while (iter != NULL)
    {
        int i;
        char found = 0;
        for (i = 0; (i < num_kinds) && !found; i++)
        {
            if (iter->entry->kind == symbol_kind_set[i])
            {
                scope_entry_list_t* new_item = counted_calloc(1, sizeof(*new_item), &_bytes_used_scopes);
                new_item->entry = iter->entry;
                new_item->next = result;
                result = new_item;
                found = 1;
            }
        }

        iter = iter->next;
    }

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
    scope_entry_list_t* iter = entry_list;
    
    while (iter != NULL)
    {
        int i;
        char found = 0;
        for (i = 0; (i < num_kinds) && !found; i++)
        {
            if (iter->entry->kind == symbol_kind_set[i])
            {
                found = 1;
            }
        }

        if (!found)
        {
            scope_entry_list_t* new_item = counted_calloc(1, sizeof(*new_item), &_bytes_used_scopes);
            new_item->entry = iter->entry;
            new_item->next = result;
            result = new_item;
        }

        iter = iter->next;
    }

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
    // Tries to avoid some copies by implementing a rough copy-on-write
    char nothing_filtered = 1;
    scope_entry_list_t* result = entry_list;

    scope_entry_list_t* iter = entry_list;
    
    while (iter != NULL)
    {
        if (!nothing_filtered)
        {
            if (f(iter->entry))
            {
                scope_entry_list_t* new_item = counted_calloc(1, sizeof(*new_item), &_bytes_used_scopes);
                new_item->entry = iter->entry;
                new_item->next = result;
                result = new_item;
            }
            else
            {
                // Do nothing
            }
        }
        else
        {
            if (f(iter->entry))
            {
                // Do nothing
            }
            else
            {
                // Copy the original list into result
                // and update nothing_filtered
                scope_entry_list_t *iter_copy = entry_list;

                result = NULL;

                while (iter_copy != iter)
                {
                    scope_entry_list_t* new_item = counted_calloc(1, sizeof(*new_item), &_bytes_used_scopes);
                    new_item->entry = iter_copy->entry;
                    new_item->next = result;
                    result = new_item;

                    iter_copy = iter_copy->next;
                }

                nothing_filtered = 0;
            }
        }

        iter = iter->next;
    }

    return result;
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
                        return create_list_from_entry(name);
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
                        && result->next == NULL
                        && (result->entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                            || result->entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                            || result->entry->kind == SK_TEMPLATE_PARAMETER))
                {
                    // This is a template parameter, label it 
                    set_as_template_parameter_name(unqualified_name, result->entry);
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
                modified_class_context.template_scope = template_arg_ctx.template_scope;

                char* conversion_function_name = 
                    get_conversion_function_name(modified_class_context, unqualified_name, /* result_type */ NULL);
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
        decl_context_t template_arguments_context,
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
                modified_class_context.template_scope = nested_name_context.template_scope;

                char* conversion_function_name = 
                    get_conversion_function_name(modified_class_context, unqualified_name, /* result_type */ NULL);

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
                dependent_entity->symbol_name = get_conversion_function_name(nested_name_context, 
                        unqualified_name, /* result_type */ NULL);
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
            dependent_entity->expression_value = 
                ASTMake3(AST_QUALIFIED_ID,
                        ast_copy(global_op), 
                        ast_copy(nested_name), 
                        ast_copy(unqualified_name),
                        ASTFileName(unqualified_name), ASTLine(unqualified_name), NULL);

            result = create_list_from_entry(dependent_entity);
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

static enum cxx_symbol_kind classes_or_namespaces_filter[] = {
    // These two are obvious
    SK_CLASS, 
    SK_NAMESPACE, 
    // This one is somewhat annoying
    SK_TYPEDEF, 
    SK_TEMPLATE,
    // These two are involved in 'typename _T::' things
    SK_TEMPLATE_TYPE_PARAMETER, 
    SK_TEMPLATE_TEMPLATE_PARAMETER,
    // Inner functions might return this
    SK_DEPENDENT_ENTITY
};

static int classes_or_namespaces_filter_num_elements = STATIC_ARRAY_LENGTH(classes_or_namespaces_filter);

static decl_context_t lookup_qualification_scope(
        decl_context_t original_context, 
        decl_context_t nested_name_context, 
        AST nested_name_spec, 
        AST unqualified_part, 
        type_t** dependent_type, 
        char *is_valid)
{
    /*
     * A nested-name-spec is of the form
     *
     * class-or-namespace :: nested-name-spec
     * class-or-namespace :: template nested-name-spec (nested-name-spec here should be a template-id)
     *
     * where class-or-namespace is of the form
     *
     *   IDENTIFIER (class-name or namespace-name)
     *   template-id (class-name only)
     *
     * Several 'disgusting' things might happen here. The most obvious one,
     * when the nested-name-spec has a dependent component (most of the time
     * is the first one, but we cannot assume such a thing)
     *
     * These are examples of dependent things 
     *
     * template <typename _T, typename _Q>
     * struct A
     * {
     *   typedef typename _T::X P1;
     *   typedef typename _T::template Y<_Q> P2;
     * };
     */
    ERROR_CONDITION(dependent_type == NULL, "Invalid argument is_dependent", 0);
    *dependent_type = NULL;
    ERROR_CONDITION(nested_name_spec == NULL, "The first nested-name-spec is null", 0);
    decl_context_t result;
    memset(&result, 0, sizeof(result));

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Solving nested-name-spec '%s'\n", prettyprint_in_buffer(nested_name_spec));
    }

    AST first_qualification = ASTSon0(nested_name_spec);
    AST next_nested_name_spec = ASTSon1(nested_name_spec);
    
    ERROR_CONDITION((ASTType(first_qualification) != AST_SYMBOL 
                && (ASTType(first_qualification) != AST_TEMPLATE_ID)), 
            "The qualification part is neither a symbol nor a template-id", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Solving first component '%s' of qualified-id '%s'\n", 
                prettyprint_in_buffer(first_qualification), 
                prettyprint_in_buffer(nested_name_spec));
    }

    scope_entry_list_t* starting_symbol_list = NULL;
    {
        // The first is solved as an unqualified entity, taking into account that it might be
        // a dependent entity (like '_T' above)
        decl_context_t initial_nested_name_context = nested_name_context;

        if (ASTType(first_qualification) == AST_TEMPLATE_ID)
        {
            starting_symbol_list = query_template_id(first_qualification, 
                    initial_nested_name_context, initial_nested_name_context);
        }
        else
        {
            starting_symbol_list = query_unqualified_name(initial_nested_name_context, original_context, first_qualification);
        }
    }
    // Filter found symbols
    starting_symbol_list = filter_symbol_kind_set(starting_symbol_list, 
            classes_or_namespaces_filter_num_elements, classes_or_namespaces_filter);

    // Nothing was found
    if (starting_symbol_list == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: First component '%s' not found\n", prettyprint_in_buffer(first_qualification));
        }
        *is_valid = 0;
        return new_decl_context();
    }

    scope_entry_t* starting_symbol = starting_symbol_list->entry;

    // This is an already dependent name
    if (BITMAP_TEST(nested_name_context.decl_flags, DF_DEPENDENT_TYPENAME)
            && starting_symbol->kind == SK_CLASS
            && starting_symbol->decl_context.current_scope->kind == CLASS_SCOPE
            && is_dependent_type(starting_symbol->decl_context.current_scope->related_entry->type_information))
    {
        // We cannot do anything else here but returning NULL
        // and stating that it is dependent
        *dependent_type = get_dependent_typename_type(starting_symbol, 
                nested_name_context, next_nested_name_spec, unqualified_part);
        *is_valid = 0;

        memset(&result, 0, sizeof(result));
        return result;
    }
    else if (starting_symbol->kind == SK_NAMESPACE)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' found to be a namespace\n", prettyprint_in_buffer(first_qualification));
        }
        // If it is a namespace work on the namespace
        result = lookup_qualification_scope_in_namespace(
                original_context, 
                nested_name_context, 
                starting_symbol, 
                next_nested_name_spec, 
                unqualified_part, 
                dependent_type, 
                is_valid);
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' found to be a class-name\n", prettyprint_in_buffer(first_qualification));
        }
        // Otherwise deal with classes
        result = lookup_qualification_scope_in_class(
                original_context, 
                nested_name_context, 
                starting_symbol, 
                next_nested_name_spec, 
                unqualified_part, 
                dependent_type, 
                is_valid);
    }

    return result;
}

// Lookup qualification within namespaces
static decl_context_t lookup_qualification_scope_in_namespace(
        decl_context_t original_context,
        decl_context_t nested_name_context, 
        scope_entry_t* namespace, 
        AST nested_name_spec, 
        AST unqualified_part, 
        type_t** dependent_type, 
        char *is_valid)
{
    // Lookup the name in the related scope of this namespace
    decl_context_t namespace_context = namespace->namespace_decl_context;

    ERROR_CONDITION(namespace_context.current_scope->kind != NAMESPACE_SCOPE, 
            "Scope is not a namespace one", 0);

    // No more nested-name-spec left
    if (nested_name_spec == NULL)
        return namespace_context;

    // Update the context
    decl_context_t decl_context = namespace->namespace_decl_context;
    decl_context.decl_flags |= nested_name_context.decl_flags;

    AST current_name = ASTSon0(nested_name_spec);
    AST next_nested_name_spec = ASTSon1(nested_name_spec);

    ERROR_CONDITION(((ASTType(current_name) != AST_SYMBOL)
            && (ASTType(current_name) != AST_TEMPLATE_ID)), 
            "nested-name-spec is neither an identifier nor a template-id", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Solving component of nested-name '%s'\n", prettyprint_in_buffer(current_name));
    }

    decl_context_t lookup_context = decl_context;

    scope_entry_list_t* symbol_list = NULL;
    if (ASTType(current_name) != AST_TEMPLATE_ID)
    {
        symbol_list = query_in_namespace(lookup_context.current_scope->related_entry, 
                ASTText(current_name),
                lookup_context.decl_flags,
                ASTFileName(current_name),
                ASTLine(current_name));
    }
    else
    {
        symbol_list = query_template_id_aux(current_name, lookup_context, original_context, query_template_id_in_namespace);
    }

    symbol_list = filter_symbol_kind_set(symbol_list, classes_or_namespaces_filter_num_elements, classes_or_namespaces_filter);

    if (symbol_list == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' not found\n", prettyprint_in_buffer(current_name));
        }
        *is_valid = 0;
        return new_decl_context();
    }

    scope_entry_t* symbol = symbol_list->entry;

    if (symbol->kind == SK_DEPENDENT_ENTITY)
    {
        // Is this possible within a namespace ?
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' is a dependent entity\n", prettyprint_in_buffer(current_name));
        }
        internal_error("Verify this case, is it possible?", 0);
        *is_valid = 0;
        return new_decl_context();
    }
    else if (symbol->kind == SK_NAMESPACE)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' found to be a namespace\n", prettyprint_in_buffer(current_name));
        }
        return lookup_qualification_scope_in_namespace(
                original_context,
                nested_name_context, 
                symbol, 
                next_nested_name_spec, 
                unqualified_part, 
                dependent_type, 
                is_valid);
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' found to be a class-name\n", prettyprint_in_buffer(current_name));
        }
        return lookup_qualification_scope_in_class(
                original_context,
                nested_name_context, 
                symbol, 
                next_nested_name_spec, 
                unqualified_part, 
                dependent_type, 
                is_valid);
    }
}

static decl_context_t lookup_qualification_scope_in_class(
        decl_context_t original_context,
        decl_context_t nested_name_context, 
        scope_entry_t* class_name, 
        AST nested_name_spec, 
        AST unqualified_part, 
        type_t** dependent_type, 
        char *is_valid)
{
    ERROR_CONDITION(class_name == NULL, "The class name cannot be null", 0);

    type_t* class_type = NULL;

    // Typedefs are handled the first because they may lead to other named
    // entities
    if (class_name->kind == SK_TYPEDEF)
    {
        class_type = advance_over_typedefs(class_name->type_information);
        /*
         * A typedef can be anything
         *
         * struct A
         * {
         *   typedef int K; 
         * };
         *
         * typedef A B;
         *
         * B::K k; <-- 'B' is an alias of 'A' (1) (SK_CLASS)
         *
         * template <typename _T>
         * struct C
         * {
         *   typedef _T K;
         *   typedef typename K::M P; <-- 'K' is an alias for '_T' (2) (SK_TEMPLATE_TYPE_PARAMETER)
         *   typename P::N f;         <-- 'P' is an alias for 'K::M' (3) 
         * };
         *
         * typedef C<int> M;
         * M::K k; <-- 'M' is an alias of 'C<int>' (4) (SK_CLASS)
         */

        if (is_named_type(class_type))
        {
            // If it is a named type it can be (1), (2) and (4)
            class_name = named_type_get_symbol(class_type);
        }
        else
        {
            // If it is not a named type then it can only be (3)
            if (is_dependent_typename_type(class_type))
            {
                // This is dependent
                *dependent_type = get_dependent_typename_type(class_name, 
                        nested_name_context, nested_name_spec, unqualified_part);
                *is_valid = 0;
                return new_decl_context();
            }

            internal_error("Invalid typedef", 0);
        }
    }

    /*
     * Several cases are handled here
     *
     */
    if (class_name->kind == SK_CLASS)
    {
        // Normal, non-template classes
        /* struct A { typedef int T; };
         *
         * A::T t; <-- 'A' here is a SK_CLASS
         */
        class_type = class_name->type_information;

        // If this is a template-specialized class it can
        // be dependent or independent. If it is independent
        // and incomplete it must be instantiated
        //
        /*
         * template <typename _T>
         * struct A { 
         *   typedef _T T;
         * };
         *
         * A<int>::T t; <-- 'A<int>' here is a SK_CLASS specialized incomplete
         *                  independent so it must be instantiated
         *
         */
        if (is_template_specialized_type(class_type))
        {
            if (class_type_is_incomplete_independent(class_type))
            {
                instantiate_template_class(class_name, nested_name_context,
                        ASTFileName(unqualified_part), ASTLine(unqualified_part));
            }
            else if (class_type_is_incomplete_dependent(class_type)
                    // In some cases we do not want to examine uninstantiated templates
                    || (class_type_is_complete_dependent(class_type)
                        && BITMAP_TEST(nested_name_context.decl_flags, DF_DEPENDENT_TYPENAME)))
            {
                // We cannot do anything else here but returning NULL
                // and stating that it is dependent
                *dependent_type = get_dependent_typename_type(class_name, 
                        nested_name_context, nested_name_spec, unqualified_part);
                *is_valid = 0;

                decl_context_t result;
                memset(&result, 0, sizeof(result));
                return result;
            }
        }
    }
    else if (class_name->kind == SK_TEMPLATE_TYPE_PARAMETER
            || class_name->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        /*
         * A type template parameter
         *
         * template <typename _T>
         * struct A
         * {
         *   typename _T::B k; <-- '_T' is a type-template-parameter
         * };
         *
         */
        // We cannot do anything else here but returning NULL
        // and stating that it is dependent
        *dependent_type = get_dependent_typename_type(class_name, 
                nested_name_context, nested_name_spec, unqualified_part);

        *is_valid = 0;

        decl_context_t result;
        memset(&result, 0, sizeof(result));
        return result;
    }
    else
    {
        internal_error("Symbol kind %d not valid", class_name->kind);
    }

    ERROR_CONDITION(!is_class_type(class_type), "The class_type is not a class type actually", 0);

    decl_context_t class_context = class_type_get_inner_context(class_type);
    class_context.decl_flags |= nested_name_context.decl_flags;

    ERROR_CONDITION((class_context.current_scope->kind != CLASS_SCOPE), "Class scope is not CLASS_SCOPE", 0);

    // Nothing else to be done
    if (nested_name_spec == NULL)
    {
        return class_context;
    }

    // Look up the next qualification item
    AST current_name = ASTSon0(nested_name_spec);
    AST next_nested_name_spec = ASTSon1(nested_name_spec);

    // Note, that once a class-name has been designed in a nested-name-spec only classes
    // can appear, since classes cannot have namespaces inside them
    scope_entry_list_t* symbol_list = NULL;
    if (ASTType(current_name) != AST_TEMPLATE_ID
            && ASTType(current_name) != AST_OPERATOR_FUNCTION_ID_TEMPLATE)
    {
        symbol_list = query_in_class(class_context.current_scope, 
                ASTText(current_name), 
                class_context.decl_flags,
                ASTFileName(current_name), 
                ASTLine(current_name));
    }
    else
    {
        symbol_list = query_template_id_aux(current_name, 
                class_context, 
                original_context, query_template_id_in_class);
    }

    symbol_list = filter_symbol_kind_set(symbol_list, classes_or_namespaces_filter_num_elements, classes_or_namespaces_filter);

    if (symbol_list == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: No class-name found for component '%s'\n", prettyprint_in_buffer(current_name));
        }

        *is_valid = 0;
        return new_decl_context();
    }

    ERROR_CONDITION(symbol_list->next != NULL, "More than one class found", 0);

    scope_entry_t* symbol = symbol_list->entry;

    if (symbol->kind == SK_DEPENDENT_ENTITY)
    {
        // How this can happen ?
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' is a dependent entity\n", prettyprint_in_buffer(current_name));
        }

        *is_valid = 0;
        return new_decl_context();
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Component '%s' found to be a class-name\n", prettyprint_in_buffer(current_name));
    }

    return lookup_qualification_scope_in_class(
            original_context,
            class_context, 
            symbol, 
            next_nested_name_spec, 
            unqualified_part, 
            dependent_type, 
            is_valid);
}

#define MAX_CLASS_PATH (64)

typedef
struct class_scope_lookup_tag
{
    int path_length;
    type_t* path[MAX_CLASS_PATH];
    char is_virtual[MAX_CLASS_PATH];

    scope_entry_list_t* entry_list;
} class_scope_lookup_t;

// scope_entry_list_t* filter_symbol_using_predicate(scope_entry_list_t* entry_list, char (*f)(scope_entry_t*))
//
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
    ERROR_CONDITION(derived->path_length == MAX_CLASS_PATH, "Class path too long", 0);

    derived->path[derived->path_length - 1] = current_class_type;
    derived->is_virtual[derived->path_length - 1] = is_virtual;

#define MAX_BASES (64)
    class_scope_lookup_t bases_lookup[MAX_BASES];
    memset(bases_lookup, 0, sizeof(bases_lookup));

    int num_bases = class_type_get_num_bases(current_class_type);
    ERROR_CONDITION(num_bases > MAX_BASES, "Too many bases", 0);

    for (i = 0; i < num_bases; i++)
    {
        char current_base_is_virtual = 0;
        char current_base_is_dependent = 0;
        scope_entry_t* base_class_entry = class_type_get_base_num(current_class_type, i, 
                &current_base_is_virtual,
                &current_base_is_dependent);

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
        entry_list = filter_not_inherited_entities(entry_list);
    }

    if (BITMAP_TEST(decl_flags, DF_NO_INJECTED_CLASS_NAME))
    {
        entry_list = filter_injected_class_name(entry_list);
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
                    // Ensure that all data members or member functions
                    // are static
                    scope_entry_list_t* it = derived->entry_list;
                    while ((it != NULL) && valid)
                    {
                        scope_entry_t* entry = it->entry;
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

                        it = it->next;
                    }
                }
            }

            if (!valid)
            {
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

static char first_is_subset_of_second(scope_entry_list_t* entry_list_1, scope_entry_list_t* entry_list_2)
{
    ERROR_CONDITION(entry_list_1 == NULL
            || entry_list_2 == NULL, "Error one list is NULL", 0);

    while (entry_list_1 != NULL)
    {
        scope_entry_list_t* it = entry_list_2;

        char found = 0;
        while (it != NULL && !found)
        {
            found = (entry_list_1->entry == it->entry);
            it = it->next;
        }

        if (!found)
            return 0;

        entry_list_1 = entry_list_1->next;
    }

    return 1;
}

static void error_ambiguity(scope_entry_list_t* entry_list, const char* filename, int line)
{
    scope_entry_list_t* it = entry_list;

    fprintf(stderr, "%s:%d: error: ambiguity in reference to '%s'\n", filename, line, entry_list->entry->symbol_name);
    fprintf(stderr, "%s:%d: info: candidates are\n", filename, line);
    while (it != NULL)
    {
        int max_qualif_level = 0;
        char is_dependent = 0;

        fprintf(stderr, "%s:%d: info:    %s\n", 
                it->entry->file,
                it->entry->line,
                get_fully_qualified_symbol_name(it->entry,
                    it->entry->decl_context, &is_dependent, &max_qualif_level));
        it = it->next;
    }

    running_error("%s:%d: error: lookup failed due to ambiguous reference '%s'\n", 
            filename, line, entry_list->entry->symbol_name);
}


static void check_for_naming_ambiguity(scope_entry_list_t* entry_list, const char* filename, int line)
{
    if (entry_list == NULL
            || entry_list->next == NULL)
        return;

    scope_entry_list_t* it = entry_list;

    scope_entry_t* hiding_name = NULL;

    while (it != NULL)
    {
        if (hiding_name == NULL
                && (it->entry->kind == SK_VARIABLE
                    || it->entry->kind == SK_ENUMERATOR))
        {
            hiding_name = it->entry;
        }
        else if (it->entry->kind == SK_FUNCTION
                || (it->entry->kind == SK_TEMPLATE
                    && named_type_get_symbol(template_type_get_primary_type(it->entry->type_information))->kind == SK_FUNCTION))
        {
            hiding_name = it->entry;
        }
        else if ((it->entry->kind == SK_CLASS
                    || it->entry->kind == SK_ENUM)
                && (hiding_name != NULL
                    && hiding_name->decl_context.current_scope == it->entry->decl_context.current_scope))
        {
        }
        else
        {
            error_ambiguity(entry_list, filename, line);
        }
        it = it->next;
    }
}

#define MAX_ASSOCIATED_NAMESPACES (64)

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
        = query_name_in_scope(namespace->namespace_decl_context.current_scope, name);

    if (grand_result != NULL)
        return grand_result;

    int i;
    for (i = idx_associated_namespaces; 
            i < num_associated_namespaces; 
            i++)
    {
        int new_num_associated_namespaces = num_associated_namespaces;
        scope_entry_t* associated_namespace = associated_namespaces[i];
        scope_t* current_scope = associated_namespace->namespace_decl_context.current_scope;

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
                if ((idx_associated_namespaces + new_num_associated_namespaces) == MAX_ASSOCIATED_NAMESPACES)
                    running_error("Too many associated namespaces > %d", MAX_ASSOCIATED_NAMESPACES);
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

        grand_result = merge_scope_entry_list(grand_result, result);
        
        check_for_naming_ambiguity(grand_result, filename, line);
    }

    return grand_result;
}

static scope_entry_list_t* query_in_namespace(scope_entry_t* namespace, 
        const char* name, decl_flags_t decl_flags,
        const char* filename, int line)
{
    ERROR_CONDITION(namespace->kind != SK_NAMESPACE, "Invalid symbol", 0);

    scope_t* current_scope = namespace->namespace_decl_context.current_scope;

    scope_entry_t* associated_namespaces[MAX_ASSOCIATED_NAMESPACES];

    int i;
    for (i = 0; i < current_scope->num_used_namespaces; i++)
    {
        if (i == MAX_ASSOCIATED_NAMESPACES)
            running_error("Too many associated namespaces > %d", MAX_ASSOCIATED_NAMESPACES);
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
    scope_t* template_scope = decl_context.template_scope;
    while (template_scope != NULL)
    {
        ERROR_CONDITION((template_scope->kind != TEMPLATE_SCOPE), "This is not a template scope!", 0);

        scope_entry_list_t *template_query = query_name_in_scope(template_scope, name);

        // If something is found in template query
        // it has higher priority
        if (template_query != NULL)
        {
            return template_query;
        }

        // If nothing was found, look up in the enclosing template_scope
        // (they form a stack of template_scopes)
        template_scope = template_scope->contained_in;
    }

    scope_entry_list_t* result = NULL;

    int num_associated_namespaces = 0;
    scope_entry_t* associated_namespaces[MAX_ASSOCIATED_NAMESPACES] = { 0 };

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
                if (num_associated_namespaces == MAX_ASSOCIATED_NAMESPACES)
                    running_error("Too many associated scopes > %d", MAX_ASSOCIATED_NAMESPACES);
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
            result = filter_any_non_type(result);
        }

        if (BITMAP_TEST(decl_context.decl_flags, DF_ONLY_CURRENT_SCOPE))
        {
            return result;
        }

        current_scope = current_scope->contained_in;
    }

    return result;
}

decl_context_t update_context_with_template_arguments(
        decl_context_t context,
        template_argument_list_t* given_template_args)
{
    decl_context_t updated_context = new_template_context(context);

    int i;
    for (i = 0; i < given_template_args->num_arguments; i++)
    {
        template_argument_t* current_template_argument = given_template_args->argument_list[i];
        char tpl_param_name[256] = { 0 };

        snprintf(tpl_param_name, 255, ".tpl_%d_%d",
                current_template_argument->nesting,
                current_template_argument->position);

        switch (current_template_argument->kind)
        {
            case TAK_TYPE :
                {
                    scope_entry_t* param_symbol = new_symbol(updated_context,
                            updated_context.template_scope, tpl_param_name);
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "SCOPE: Adding '%s' as a type template parameter to '%s'\n",
                                tpl_param_name,
                                print_declarator(current_template_argument->type));
                    }
                    // We use a typedef
                    param_symbol->kind = SK_TYPEDEF;
                    param_symbol->entity_specs.is_template_argument = 1;
                    param_symbol->type_information = current_template_argument->type;

                    break;
                }
            case TAK_TEMPLATE :
                {
                    scope_entry_t* param_symbol = new_symbol(updated_context,
                            updated_context.template_scope, tpl_param_name);
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "SCOPE: Adding '%s' as a template template parameter to '%s'\n",
                                tpl_param_name,
                                named_type_get_symbol(current_template_argument->type)->symbol_name);
                    }
                    // The template type has to be used here
                    param_symbol->kind = SK_TEMPLATE;
                    param_symbol->entity_specs.is_template_argument = 1;
                    // These are always kept as named types in the compiler
                    param_symbol->type_information = 
                        named_type_get_symbol(current_template_argument->type)->type_information;
                    break;
                }
            case TAK_NONTYPE :
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "SCOPE: Adding '%s' as a nontype template parameter to '%s'\n",
                                tpl_param_name,
                                prettyprint_in_buffer(current_template_argument->expression));
                    }

                    scope_entry_t* param_symbol = new_symbol(updated_context,
                            updated_context.template_scope, tpl_param_name);
                    param_symbol->kind = SK_VARIABLE;
                    param_symbol->entity_specs.is_template_argument = 1;
                    param_symbol->type_information = current_template_argument->type;

                    // Fold it, as makes things easier
                    if (expression_is_constant(current_template_argument->expression))
                    {
                        param_symbol->expression_value = const_value_to_tree(
                                expression_get_constant(current_template_argument->expression)
                                );
                    }
                    else
                    {
                        param_symbol->expression_value = current_template_argument->expression;
                    }
                    break;
                }
            default:
                {
                    internal_error("Invalid parameter kind", 0);
                }
        }
    }

    return updated_context;
}

static template_argument_list_t* complete_arguments_of_template_id(
        decl_context_t template_name_context,
        decl_context_t template_arguments_context,
        template_parameter_list_t* template_parameters,
        template_argument_list_t* template_arguments,
        const char* filename,
        int line);

static template_argument_list_t* duplicate_template_arguments(
        template_argument_list_t* orig_tpl_args)
{
    // Duplicate template arguments because we may have to modify them
    template_argument_list_t* template_arguments = counted_calloc(1, sizeof(*template_arguments), &_bytes_used_scopes);
    template_arguments->num_arguments = orig_tpl_args->num_arguments;

    template_arguments->argument_list = counted_calloc(template_arguments->num_arguments, 
            sizeof(*template_arguments->argument_list), &_bytes_used_scopes);
    int i;
    for (i = 0; i < template_arguments->num_arguments; i++)
    {
        template_arguments->argument_list[i] = orig_tpl_args->argument_list[i];
    }

    return template_arguments;
}

static template_argument_t* update_template_argument(
        template_argument_t* current_template_arg,
        decl_context_t template_arguments_context,
        const char *filename, int line,
        char overwrite_context);

static template_argument_list_t* update_template_argument_list(
        template_argument_list_t* template_arguments,
        decl_context_t template_arguments_context,
        const char *filename, int line,
        char overwrite_context)
{
    int i;
    for (i = 0; i < template_arguments->num_arguments; i++)
    {
        template_arguments->argument_list[i] = update_template_argument(
                template_arguments->argument_list[i],
                template_arguments_context, 
                filename, 
                line, 
                overwrite_context);
    }

    return template_arguments;
}

static type_t* update_dependent_typename(
        type_t* dependent_entry_type,
        dependent_name_part_t* dependent_parts,
        decl_context_t decl_context,
        const char* filename, int line,
        char instantiation_update)
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

        if (member_list->next != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Too many symbols where found for '%s'\n", dependent_parts->name);
            }
            return NULL;
        }

        scope_entry_t* member = member_list->entry;

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

            template_argument_list_t* template_arguments = duplicate_template_arguments(dependent_parts->template_arguments);
            template_parameter_list_t *template_parameters = template_type_get_template_parameters(template_type);

            template_arguments = complete_arguments_of_template_id(
                    class_context,
                    decl_context,
                    template_parameters,
                    template_arguments,
                    filename, line);

            template_arguments = update_template_argument_list(template_arguments,
                    decl_context, filename, line,
                    /* overwrite_context */ instantiation_update);

            if (template_arguments->num_arguments != template_parameters->num_template_parameters)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Template argument count does not match template parameter count\n");
                }
                return NULL;
            }

            type_t* specialized_type = template_type_get_specialized_type(
                    template_type,
                    template_arguments,
                    template_type_get_template_parameters(template_type),
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

    if (member_list->next != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Too many symbols where found for '%s'\n", dependent_parts->name);
        }
        return NULL;
    }

    scope_entry_t* member = member_list->entry;

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

        template_argument_list_t* template_arguments = duplicate_template_arguments(dependent_parts->template_arguments);
        template_parameter_list_t *template_parameters = template_type_get_template_parameters(template_type);

        template_arguments = complete_arguments_of_template_id(
                class_context,
                decl_context,
                template_parameters,
                template_arguments,
                filename, line);

        template_arguments = update_template_argument_list(template_arguments,
                decl_context, filename, line, /* overwrite_context */ instantiation_update);

        if (template_arguments->num_arguments != template_parameters->num_template_parameters)
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
                template_arguments,
                template_type_get_template_parameters(template_type),
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
        const char* filename, int line,
        char instantiation_update)
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
            template_parameter_list_t* template_parameters = template_type_get_template_parameters(template_type);
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

            template_argument_list_t* updated_template_arguments = 
                counted_calloc(1, sizeof(*updated_template_arguments), &_bytes_used_scopes);

            template_argument_list_t* template_arguments = 
                template_specialized_type_get_template_arguments(entry->type_information);

            int i;
            for (i = 0; i < template_arguments->num_arguments; i++)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Updating template argument %d of specialized template class\n", i);
                }
                template_argument_t* updated_argument = update_template_argument(
                        template_arguments->argument_list[i],
                        decl_context, filename, line,
                        /* overwrite_context */ instantiation_update);

                P_LIST_ADD(updated_template_arguments->argument_list, updated_template_arguments->num_arguments, updated_argument);
            }
            
            // Once the types have been updated, reask for a specialization

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Reasking for specialization\n");
            }
            type_t* updated_specialized = 
                template_type_get_specialized_type(template_type, 
                        updated_template_arguments, 
                        template_parameters,
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
                        decl_context, filename, line, 
                        instantiation_update),
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

        type_t* updated_referenced = update_type_aux_(referenced, decl_context, filename, line, instantiation_update);

        if (updated_referenced == NULL)
            return NULL;

        type_t* result_type = get_lvalue_reference_type(updated_referenced);

        return result_type;
    }
    else if (is_pointer_type(orig_type))
    {
        cv_qualifier_t cv_qualifier = get_cv_qualifier(orig_type);

        type_t* pointee = pointer_type_get_pointee_type(orig_type);

        type_t* updated_pointee = update_type_aux_(pointee, decl_context, filename, line, instantiation_update);

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
        type_t* updated_pointee = update_type_aux_(pointee, decl_context, filename, line, instantiation_update);

        if (updated_pointee == NULL)
            return NULL;

        type_t* pointee_class = pointer_to_member_type_get_class_type(orig_type);
        pointee_class = update_type_aux_(pointee_class, decl_context, filename, line, instantiation_update);

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
            return_type = update_type_aux_(return_type, decl_context, filename, line, instantiation_update);
            // Something went wrong here for the return type
            if (return_type == NULL)
                return NULL;
        }

#define MAX_PARAMETERS (256)
        parameter_info_t parameter_types[MAX_PARAMETERS];
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
                    decl_context, filename, line, instantiation_update);

            if (param_orig_type == NULL)
                return NULL;

            parameter_info_t parameter_info;

            memset(&parameter_info, 0, sizeof(parameter_info));
            parameter_info.type_info = param_orig_type;

            ERROR_CONDITION(num_parameters >= MAX_PARAMETERS,
                    "Too many parameters", 0);

            parameter_types[num_parameters] = parameter_info;
            num_parameters++;
        }

        if (has_ellipsis)
        {
            parameter_info_t parameter_info;

            memset(&parameter_info, 0, sizeof(parameter_info));
            parameter_info.is_ellipsis = 1;

            ERROR_CONDITION(num_parameters >= MAX_PARAMETERS,
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

        AST expression = array_type_get_array_size_expr(orig_type);
        decl_context_t expr_context = array_type_get_array_size_expr_context(orig_type);

        AST updated_expr = expression;
        decl_context_t updated_expr_context = expr_context;
        if (updated_expr != NULL)
        {
            updated_expr = ast_copy_for_instantiation(updated_expr);
            updated_expr_context = decl_context;
            if (!check_for_expression(updated_expr, updated_expr_context))
            {
                running_error("%s: error: could not update array dimension",
                        ast_location(expression));
            }
        }

        type_t* element_type = array_type_get_element_type(orig_type);
        element_type = update_type_aux_(element_type,
                decl_context, filename, line, instantiation_update);

        if (element_type == NULL)
            return NULL;

        type_t* updated_array_type = get_array_type(element_type, updated_expr, updated_expr_context);

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
                decl_context, filename, line, instantiation_update);

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
            // so the updated type should be [S]::S2::S1
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
            update_dependent_typename(fixed_type, dependent_parts, decl_context, filename, line, instantiation_update);

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
            filename, line, /* instantiation_update */ 0);

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
            filename, line, /* instantiation_update */ 1);

    ERROR_CONDITION(result == NULL, "Invalid type update of '%s' during instantiation", print_declarator(orig_type));
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Type '%s' has been updated to '%s'\n", print_declarator(orig_type), print_declarator(result));
    }

    return result;
}

static template_argument_t* update_template_argument(
        template_argument_t* current_template_arg,
        decl_context_t decl_context,
        const char *filename, int line,
        char overwrite_context)
{
    template_argument_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);
    result->kind = current_template_arg->kind;

    switch (current_template_arg->kind)
    {
        case TAK_TYPE:
            {
                result->type = update_type(current_template_arg->type, 
                        decl_context, filename, line);

                ERROR_CONDITION ((result->type == NULL), 
                        "type template argument could not be updated", 0);
                break;
            }
        case TAK_TEMPLATE:
            {
                result->type = update_type(current_template_arg->type, 
                        decl_context, filename, line);

                ERROR_CONDITION ((result->type == NULL), 
                        "template template argument could not be updated", 0);
                break;
            }
        case TAK_NONTYPE:
            {
                result->type = update_type(current_template_arg->type, 
                        decl_context, filename, line);

                // We really need to copy this tree because it comes from another tree
                // whose type was already computed in another context, and we do not
                // want to overwrite it
                result->expression =
                    ast_copy_for_instantiation(current_template_arg->expression);
                if (!overwrite_context)
                {
                    // Use the new template scope
                    result->expression_context = current_template_arg->expression_context;
                    result->expression_context.template_scope = decl_context.template_scope;
                }
                else
                {
                    // Overwrite with the given context (used only during instantiation)
                    result->expression_context = decl_context;
                }

                // Update type information 
                if(!check_for_expression(result->expression, result->expression_context))
                {
                    internal_error("Updated nontype template parameter has an invalid expression '%s'", 
                            prettyprint_in_buffer(result->expression));
                }

                // Fold the argument if possible
                if (expression_is_constant(result->expression))
                {
                    result->expression = const_value_to_tree(expression_get_constant(result->expression));
                }

                // Ensure the "destination" type of this nontype template
                // argument is OK (this is important for unresolved overloads)
                // FIXME - We should check that a standard conversion
                // (involving no user-defined conversions) is possible
                expression_set_type(result->expression, result->type);

                ERROR_CONDITION ((result->type == NULL), 
                        "nontype/template template could not be updated", 0);

                break;
            }
        default:
            {
                internal_error("Invalid argument kind", 0);
            }
            break;
    }

    result->position = current_template_arg->position;
    result->nesting = current_template_arg->nesting;

    return result;
}

template_argument_list_t* get_template_arguments_from_syntax(
        AST template_arguments_list,
        decl_context_t template_arguments_context,
        int nesting_level)
{
    template_argument_list_t *result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);

    if (template_arguments_list == NULL)
        return result;

    int position = 0;
    AST iter;
    for_each_element(template_arguments_list, iter)
    {
        AST template_argument = ASTSon1(iter);

        template_argument_t* t_argument = counted_calloc(1, sizeof(*t_argument), &_bytes_used_scopes);

        switch (ASTType(template_argument))
        {
            case AST_TEMPLATE_EXPRESSION_ARGUMENT :
                {
                    t_argument->kind = TAK_NONTYPE;

                    t_argument->expression = ASTSon0(template_argument);
                    t_argument->expression_context = template_arguments_context;

                    type_t* expr_type = expression_get_type(t_argument->expression);

                    if (expression_is_constant(t_argument->expression))
                    {
                        t_argument->expression = const_value_to_tree(expression_get_constant(t_argument->expression));
                    }
                    else
                    {
                        t_argument->expression = ast_copy_for_instantiation(t_argument->expression);
                        check_for_expression(t_argument->expression, template_arguments_context);
                    }
                    t_argument->type = expr_type;
                    break;
                }
            case AST_TEMPLATE_TYPE_ARGUMENT :
                {
                    AST type_template_argument = ASTSon0(template_argument);
                    AST type_specifier_seq = ASTSon0(type_template_argument);
                    AST abstract_decl = ASTSon1(type_template_argument);

                    // A type_specifier_seq is essentially a subset of a
                    // declarator_specifier_seq so we can reuse existing functions
                    type_t* type_info;
                    gather_decl_spec_t gather_info;
                    memset(&gather_info, 0, sizeof(gather_info));

                    build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                            template_arguments_context);

                    type_t* declarator_type;
                    compute_declarator_type(abstract_decl, &gather_info, type_info, &declarator_type,
                            template_arguments_context);

                    t_argument->type = declarator_type;

                    if (is_named_type(t_argument->type)
                            && is_template_type(named_type_get_symbol(t_argument->type)->type_information))
                    {
                        // A template-name
                        t_argument->kind = TAK_TEMPLATE;
                    }
                    else
                    {
                        t_argument->kind = TAK_TYPE;
                    }
                    break;
                }
            case AST_AMBIGUITY :
                {
                    internal_error("Ambiguous node", 0);
                }
            default:
                {
                    internal_error("Invalid node %s", ast_print_node_type(ASTType(template_argument)));
                }
        }

        t_argument->nesting = nesting_level;
        t_argument->position = position;
        position++;

        P_LIST_ADD(result->argument_list, result->num_arguments, t_argument);
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
        for (i = 0; i < MAX_AST_CHILDREN; i++)
        {
            update_unresolved_overloaded_type(unresolved_type, solved_type, ASTChild(tree, i));
        }
    }
}

static void sign_in_template_name(template_argument_t* current_template_argument,
        decl_context_t updated_decl_context)
{
    char tpl_param_name[256] = { 0 };

    snprintf(tpl_param_name, 255, ".tpl_%d_%d",
            current_template_argument->nesting,
            current_template_argument->position);
    scope_entry_t* param_symbol = new_symbol(updated_decl_context,
            updated_decl_context.template_scope, tpl_param_name);

    switch (current_template_argument->kind)
    {
        case TAK_TYPE:
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Signing in '%s' as a type template parameter to '%s'\n",
                            tpl_param_name,
                            print_declarator(current_template_argument->type));
                }
                // Sign in template parameter
                param_symbol->kind = SK_TYPEDEF;
                param_symbol->entity_specs.is_template_argument = 1;
                param_symbol->type_information = current_template_argument->type;
                break;
            }
        case TAK_TEMPLATE:
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Signing in '%s' as a template template parameter to '%s'\n",
                            tpl_param_name,
                            named_type_get_symbol(current_template_argument->type)->symbol_name);
                }
                // Sign in template parameter
                param_symbol->kind = SK_TEMPLATE;
                param_symbol->entity_specs.is_template_argument = 1;
                // These are always kept as named types in the compiler
                param_symbol->type_information = 
                    named_type_get_symbol(current_template_argument->type)->type_information;
                break;
            }
        case TAK_NONTYPE:
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Signing in '%s' as a nontype template parameter to '%s'\n",
                            tpl_param_name,
                            prettyprint_in_buffer(current_template_argument->expression));
                }
                // Sign in template parameter
                param_symbol->kind = SK_VARIABLE;
                param_symbol->entity_specs.is_template_argument = 1;
                param_symbol->type_information = current_template_argument->type;

                if (expression_is_constant(current_template_argument->expression))
                {
                    param_symbol->expression_value = const_value_to_tree(
                            expression_get_constant(current_template_argument->expression));
                }

                break;
            }
        default:
            {
                internal_error("Invalid template argument kind", 0);
            }
            break;
    }
}

static template_argument_list_t* complete_arguments_of_template_id(
        decl_context_t template_name_context,
        decl_context_t template_arguments_context,
        template_parameter_list_t* template_parameters,
        template_argument_list_t* template_arguments,
        const char* filename,
        int line)
{
    template_argument_list_t* result = template_arguments;

    int num_argument;
    decl_context_t updated_decl_context = new_template_context(template_name_context);

    for (num_argument = 0; num_argument < result->num_arguments; num_argument++)
    {
        template_argument_t* current_template_argument = result->argument_list[num_argument];

        if (current_template_argument->kind == TAK_NONTYPE)
        {
            current_template_argument->type = update_type(
                    template_parameters->template_parameters[num_argument]->entry->type_information,
                    updated_decl_context,
                    filename, line);
            current_template_argument->expression_context = template_arguments_context;

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Type of nontype template parameter updated to '%s'\n",
                        print_declarator(current_template_argument->type));
            }

            /*
             * If the type is an address of function try to solve it
             */
            if (expression_get_type(current_template_argument->expression) != NULL
                    && is_unresolved_overloaded_type(expression_get_type(current_template_argument->expression)))
            {
                // Try to solve it
                scope_entry_t* solved_function =
                    address_of_overloaded_function(
                            unresolved_overloaded_type_get_overload_set(expression_get_type(current_template_argument->expression)),
                            unresolved_overloaded_type_get_explicit_template_arguments(expression_get_type(current_template_argument->expression)),
                            current_template_argument->type,
                            updated_decl_context,
                            filename,
                            line);

                if (solved_function != NULL)
                {
                    // Update the type throughout the expression (this is needed when evaluating it)
                    update_unresolved_overloaded_type(expression_get_type(current_template_argument->expression),
                            solved_function->type_information,
                            current_template_argument->expression);
                }
            }

            ERROR_CONDITION(current_template_argument->type == NULL, "Could not update properly template argument", 0);
        }

        sign_in_template_name(current_template_argument, updated_decl_context);
    }

    if (num_argument < template_parameters->num_template_parameters)
    {
        // Complete with default template arguments
        for (; num_argument < template_parameters->num_template_parameters; num_argument++)
        {
            template_parameter_t* template_parameter = 
                template_parameters->template_parameters[num_argument];

            ERROR_CONDITION(!template_parameter->has_default_argument,
                    "Template parameter '%d' lacks a default argument",
                    num_argument);

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Updating template argument %d\n", 
                        num_argument);
            }

            template_argument_t* original_default_arg = template_parameter->default_template_argument;

            template_argument_t* current_template_argument = update_template_argument(
                    original_default_arg, 
                    updated_decl_context, 
                    filename, line,
                    /* overwrite_context */ 0);

            sign_in_template_name(current_template_argument, updated_decl_context);

            P_LIST_ADD(result->argument_list, result->num_arguments, current_template_argument);

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Template argument updated\n");
            }
        }
    }

    return result;
}

static template_argument_list_t *get_template_arguments_of_template_id(
        AST template_id,
        type_t* template_type,
        decl_context_t template_name_context,
        decl_context_t template_arguments_context,
        char *valid)
{
    // Solve any pending ambiguity
    if (!solve_possibly_ambiguous_template_id(template_id, template_arguments_context))
    {
        *valid = 0;
        return NULL;
    }

    AST template_arguments_list = ASTSon1(template_id);

    int nesting_level = template_type_get_nesting_level(template_type);

    // Get the types raw from the syntax
    template_argument_list_t* result = 
        get_template_arguments_from_syntax(template_arguments_list, 
                template_arguments_context, nesting_level);

    *valid = 1;
    template_parameter_list_t* template_parameters = template_type_get_template_parameters(template_type);
    if (result->num_arguments > template_parameters->num_template_parameters)
    {
        // Too many template parameters
        DEBUG_CODE()
        {
            fprintf(stderr, "Too many template arguments for this template\n");
        }
        *valid = 0;
        return NULL;
    }

    // Now review the template arguments got from syntax and update if needed
    int num_argument;
    for (num_argument = 0; num_argument < result->num_arguments; num_argument++)
    {
        template_argument_t* current_arg = result->argument_list[num_argument];
        switch (current_arg->kind)
        {
            case TAK_TYPE:
                {
                    if (template_parameters->template_parameters[num_argument]->kind != TPK_TYPE)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Got a type template argument but template parameter %d "
                                    "is not a type template parameter\n",
                                    num_argument);
                        }
                        *valid = 0;
                        return NULL;
                    }
                    break;
                }
            case TAK_TEMPLATE:
                {
                    if (template_parameters->template_parameters[num_argument]->kind != TPK_TEMPLATE)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Got a template template argument but template parameter %d "
                                    "is not a template template parameter\n",
                                    num_argument);
                        }
                        *valid = 0;
                        return NULL;
                    }

                    break;
                }
            case TAK_NONTYPE:
                {
                    if ((template_parameters->template_parameters[num_argument]->kind != TPK_NONTYPE))
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Got a non-type template argument but parameter %d "
                                    "is not a nontype template parameter\n",
                                    num_argument);
                        }
                        *valid = 0;
                        return NULL;
                    }
                    break;
                }
            default:
                {
                    internal_error("Invalid template argument kind", 0);
                }
                break;
        }
    }

    return complete_arguments_of_template_id(
            template_name_context,
            template_arguments_context,
            template_parameters,
            result,
            ASTFileName(template_id), ASTLine(template_id));
}

// This function never instantiates a template, it might create a specialization though
static scope_entry_list_t* query_template_id_aux(AST template_id, 
        decl_context_t template_name_context,
        decl_context_t template_arguments_context,
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
    else // AST_OPERATOR_FUNCTION_ID_TEMPLATE
    {
        template_name = get_operator_function_name(template_id);
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

        template_symbol = template_symbol_list->entry;
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
        template_argument_list_t* template_arguments = get_template_arguments_of_template_id(
                template_id,
                generic_type, 
                template_name_context, 
                template_arguments_context, 
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

        template_parameter_list_t* template_parameters =
            template_type_get_template_parameters(generic_type);

        specialized_type = template_type_get_specialized_type(generic_type, 
                template_arguments, 
                template_parameters,
                template_arguments_context, 
                ASTLine(template_id), ASTFileName(template_id));

        if (specialized_type != NULL)
        {

            if (template_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                set_as_template_parameter_name(template_id, template_symbol);
            }

            ERROR_CONDITION(!is_named_type(specialized_type), "This should be a named type", 0);

            // Crappy
            scope_entry_list_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);
            result->entry = named_type_get_symbol(specialized_type);

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
        if (!solve_possibly_ambiguous_template_id(template_id, template_arguments_context))
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
        decl_context_t template_arguments_context)
{
    return query_template_id_aux(template_id,
            template_name_context,
            template_arguments_context,
            name_lookup);
}

scope_entry_list_t *copy_entry_list(scope_entry_list_t* orig)
{
    scope_entry_list_t* result = NULL;
    if (orig == NULL)
        return result;

    result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);

    result->entry = orig->entry;
    result->next = copy_entry_list(orig->next);

    return result;
}

// Like append but avoids repeated symbols
scope_entry_list_t* merge_scope_entry_list(scope_entry_list_t* list_1, scope_entry_list_t* list_2)
{
    // Some simple cases
    if (list_1 == NULL)
    {
        return list_2;
    }
    else if (list_2 == NULL)
    {
        return list_1;
    }
    if (first_is_subset_of_second(list_1, list_2))
    {
        return list_2;
    }
    else if (first_is_subset_of_second(list_2, list_1))
    {
        return list_1;
    }

    // Full merge
    // Copy list_1
    scope_entry_list_t* result = copy_entry_list(list_1);

    scope_entry_list_t* last = result;
    while (last->next != NULL)
        last = last->next;

    scope_entry_list_t* it_list_2 = list_2;
    while (it_list_2 != NULL)
    {
        char found = 0;
        
        scope_entry_list_t* it_res = result;
        while (it_res != NULL && !found)
        {
            if (it_res->entry == it_list_2->entry)
                found = 1;

            it_res = it_res->next;
        }

        if (!found)
        {
            last->next = counted_calloc(1, sizeof(*last->next), &_bytes_used_scopes);
            last->next->entry = it_list_2->entry;
            last = last->next;
        }

        it_list_2 = it_list_2->next;
    }

    return result;
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
                    && current_scope->related_entry->symbol_name != NULL)
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

typedef
struct find_template_parameter_data_tag
{
    char found;
    scope_entry_t* template_param;
    scope_entry_t* entry;
} find_template_parameter_data_t;

static void find_template_parameter_aux(const void* key UNUSED_PARAMETER, void* info, void* data)
{
    find_template_parameter_data_t* p_data = (find_template_parameter_data_t*)data;

    if (p_data->found)
        return;

    scope_entry_list_t* entry_list = (scope_entry_list_t*) info;
    scope_entry_t* entry = entry_list->entry;

    if (entry->kind == p_data->template_param->kind
            && (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            && (entry->entity_specs.template_parameter_position 
                == p_data->template_param->entity_specs.template_parameter_position)
            && (entry->entity_specs.template_parameter_nesting 
                == p_data->template_param->entity_specs.template_parameter_nesting))
    {
        p_data->entry = entry;
        p_data->found = 1;
    }
}

static scope_entry_t* find_template_parameter(scope_t* st, scope_entry_t* template_param)
{
    find_template_parameter_data_t data;
    memset(&data, 0, sizeof(data));
    data.template_param = template_param;

    rb_tree_walk(st->hash, find_template_parameter_aux, &data);

    return data.entry;
}

static const char* give_name_for_template_parameter(scope_entry_t* entry, decl_context_t decl_context)
{
    scope_t* st = decl_context.template_scope;
    while (st != NULL)
    {
        scope_entry_t* template_parameter = find_template_parameter(st, entry);
        if (template_parameter != NULL)
        {
            return template_parameter->symbol_name;
        }

        st = st->contained_in;
    }

    return entry->symbol_name;
}

static const char* get_unqualified_template_symbol_name(scope_entry_t* entry, 
        decl_context_t decl_context)
{
    const char* result = "";

    // It is not enough with the name, we have to print the arguments
    result = strappend(result, "<");
    template_argument_list_t* template_arguments = template_specialized_type_get_template_arguments(entry->type_information);

    int i;
    for (i = 0; i < template_arguments->num_arguments; i++)
    {
        template_argument_t* template_argument = template_arguments->argument_list[i];

        // Avoid unnecessary cluttering and the fact that these implicit types
        // come from strange typedefs that would force us to solve completely
        if (template_argument->implicit)
            continue;

        if (i != 0)
        {
            result = strappend(result, ", ");
        }

        switch (template_argument->kind)
        {
            // Print the type
            case TAK_TEMPLATE:
            case TAK_TYPE:
                {
                    const char* abstract_declaration;

                    abstract_declaration = 
                        get_declaration_string_internal(template_argument->type, decl_context, "", "", 0, NULL, NULL, 0);

                    result = strappend(result, abstract_declaration);
                    break;
                }
            case TAK_NONTYPE:
                {
                    result = strappend(result, prettyprint_in_buffer(template_argument->expression));
                    break;
                }
            default:
                {
                    internal_error("Undefined template argument\n", 0);
                    break;
                }
        }
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

    const char* result = uniquestr(entry->symbol_name);

    if (entry->kind == SK_TEMPLATE_PARAMETER
            || entry->kind == SK_TEMPLATE_TYPE_PARAMETER
            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        // This symbol must be looked up for the proper real name
        result = give_name_for_template_parameter(entry, decl_context);

        // This is obviously dependent
        (*is_dependent) |= 1;
        return result;
    }
    else if (!no_templates
            && entry->type_information != NULL
            && is_template_specialized_type(entry->type_information)
            && template_specialized_type_get_template_arguments(entry->type_information) != NULL
            && template_specialized_type_get_template_arguments(entry->type_information)->num_arguments > 0)
    {
        const char *template_arguments = get_unqualified_template_symbol_name(entry, decl_context);
        result = strappend(result, template_arguments);
    }

    if (entry->entity_specs.is_member)
    {
        // We need the qualification of the class
        ERROR_CONDITION(!is_named_class_type(entry->entity_specs.class_type), "The class of a member must be named", 0);

        scope_entry_t* class_symbol = named_type_get_symbol(entry->entity_specs.class_type);

        (*max_qualif_level)++;

        const char* class_qualification = 
            get_fully_qualified_symbol_name(class_symbol, decl_context, is_dependent, max_qualif_level);

        class_qualification = strappend(class_qualification, "::");

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

void scope_entry_dynamic_initializer(void)
{
    // Initialize the schema of scope entries
    extensible_schema_init(&scope_entry_extensible_schema);
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
    scope_entry_t* associated_namespaces[MAX_ASSOCIATED_NAMESPACES] = { 0 };

    scope_t* current_scope = decl_context.current_scope;

    while (current_scope != NULL)
    {
        if (current_scope->kind == CLASS_SCOPE
                && !BITMAP_TEST(decl_context.decl_flags, DF_ONLY_CURRENT_SCOPE))
        {
            result = merge_scope_entry_list(result, 
                    query_in_class(current_scope, name, decl_context.decl_flags, 
                        filename, line));
        }
        else if (current_scope->kind == NAMESPACE_SCOPE)
        {
            result = merge_scope_entry_list(result,
                    query_in_namespace_and_associates(
                        current_scope->related_entry,
                        name, 0, num_associated_namespaces,
                        associated_namespaces, decl_context.decl_flags,
                        filename, line));
            num_associated_namespaces = 0;
        }
        else // BLOCK_SCOPE || PROTOTYPE_SCOPE || FUNCTION_SCOPE (although its contains should be NULL)
        {
            result = merge_scope_entry_list(result, query_name_in_scope(current_scope, name));
        }

        if (BITMAP_TEST(decl_context.decl_flags, DF_ELABORATED_NAME))
        {
            result = filter_any_non_type(result);
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
    char tpl_param_name[256] = { 0 };

    snprintf(tpl_param_name, 255, ".tpl_%d_%d",
            template_parameter_nesting,
            template_parameter_position);

    scope_entry_list_t* entry_list = query_unqualified_name_str(context, tpl_param_name);

    if (entry_list == NULL)
        return NULL;
    else
        return entry_list->entry;
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
