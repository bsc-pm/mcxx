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
#include "hash.h"
#include "hash_iterator.h"


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
static scope_entry_list_t* name_lookup(decl_context_t decl_context, const char* name);

// Solve a template given a template-id, a list of found names for the template-id and the declaration context

// Looks up the qualification scope for a nested-name-spec
static scope_t* lookup_qualification_scope(decl_context_t decl_context, AST nested_name, AST unqualifed_part, type_t** dependent_type);

static scope_t* lookup_qualification_scope_in_namespace(decl_context_t nested_name_context, scope_entry_t* namespace, 
        AST nested_name_spec, AST unqualified_part, type_t **is_dependent);
static scope_t* lookup_qualification_scope_in_class(decl_context_t nested_name_context, scope_entry_t* class_name, 
        AST nested_name_spec, AST unqualified_part, type_t **is_dependent);

static scope_entry_list_t* query_template_id(AST template_id, 
        decl_context_t template_name_context,
        decl_context_t template_arguments_context);

static scope_entry_list_t* query_qualified_name(decl_context_t decl_context,
        AST global_op,
        AST nested_name,
        AST unqualified_name);

// Appends scope entry lists
static scope_entry_list_t* append_scope_entry_list(scope_entry_list_t* orig, scope_entry_list_t* appended);

// Scope creation functions
static scope_t* new_scope(void);
static scope_t* new_namespace_scope(scope_t* st, const char* qualification_name);
static scope_t* new_prototype_scope(scope_t* st);
static scope_t* new_block_scope(scope_t* enclosing_scope);
static scope_t* new_class_scope(scope_t* enclosing_scope, 
        const char* qualification_name, type_t* class_type);
static scope_t* new_template_scope(scope_t* enclosing_scope);

static scope_entry_list_t* name_lookup_used_namespaces(decl_context_t decl_context, 
        scope_t* current_scope, const char* name);

/* Scope creation functions */
/*
 * There was a time when the compiler worked directly with scopes instead
 * of declarative context, that these were the functions used
 * to create them.
 */

// Any new scope should be created using this one
static scope_t* new_scope(void)
{
    scope_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);

    result->hash = hash_create(HASH_SIZE, HASHFUNC(prime_hash), KEYCMPFUNC(strcmp));

    return result;
}

// Creates a new namespace scope and optionally it gives it a
// qualification_name. Global scope has st == NULL and qualification_name == NULL
static scope_t* new_namespace_scope(scope_t* st, const char* qualification_name)
{
    scope_t* result = new_scope();

    result->kind = NAMESPACE_SCOPE;
    result->contained_in = st;
    if (qualification_name != NULL)
    {
        result->qualification_name = uniquestr(qualification_name);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: New namespace scope '%p' (qualification='%s') created\n", 
                result, result->qualification_name);
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
static scope_t* new_class_scope(scope_t* enclosing_scope, const char* qualification_name, 
        type_t* class_type)
{
    scope_t* result = new_scope();

    result->kind = CLASS_SCOPE;
    result->contained_in = enclosing_scope;

    result->class_type = class_type;

    if (qualification_name != NULL)
    {
        result->qualification_name = uniquestr(qualification_name);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: New class scope '%p' (qualif=%s) scope created\n",  
                result, result->qualification_name);
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

    // Create global scope
    result.namespace_scope = new_namespace_scope(NULL, NULL);
    // Make it the global one
    result.global_scope = result.namespace_scope;
    // and the current one
    result.current_scope = result.namespace_scope;

    return result;
}

decl_context_t new_namespace_context(decl_context_t enclosing_decl_context, 
        const char* qualification_name)
{
    ERROR_CONDITION((enclosing_decl_context.current_scope->kind != NAMESPACE_SCOPE),
            "Enclosing scope must be namespace scope", 0);
    ERROR_CONDITION((enclosing_decl_context.current_scope != enclosing_decl_context.namespace_scope), 
            "Enclosing namespace scope has a mismatch between its current scope and its namespace scope", 0);

    // Inherit current context
    decl_context_t result = enclosing_decl_context;

    // Create a new namespace scope contained in the previous one
    result.namespace_scope = new_namespace_scope(enclosing_decl_context.namespace_scope, qualification_name);
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
        const char* qualification_name, type_t* class_type)
{
    ERROR_CONDITION(enclosing_context.current_scope->kind != NAMESPACE_SCOPE
            && enclosing_context.current_scope->kind != CLASS_SCOPE
            && enclosing_context.current_scope->kind != BLOCK_SCOPE, /* The last case yields a local class */
            "Enclosing scope is neither namespace, class or local", 0
            );

    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class", 0);

    // Inherit the scope
    decl_context_t result = enclosing_context;

    // Create new class scope
    result.class_scope = new_class_scope(enclosing_context.current_scope, qualification_name, class_type);

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


// Normally we work on decl_context.current_scope but for template parameters
// sc != decl_context.current_scope so allow the user such freedom
scope_entry_t* new_symbol(decl_context_t decl_context, scope_t* sc, const char* name)
{
    ERROR_CONDITION(name == NULL ||
            *name == '\0', "New symbol called with an empty or NULL string", 0);

    scope_entry_list_t* result_set = (scope_entry_list_t*) hash_get(sc->hash, name);

    scope_entry_t* result;

    result = counted_calloc(1, sizeof(*result), &_bytes_used_symbols);
    result->symbol_name = uniquestr(name);
    // Remember, for template parameters, .current_scope will not contain
    // its declaration scope but will be in .template_scope
    result->decl_context = decl_context;

    result->extended_data = counted_calloc(1, sizeof(*(result->extended_data)), &_bytes_used_symbols);
    extensible_struct_init(result->extended_data, &scope_entry_extensible_schema);

    if (result_set != NULL)
    {
        scope_entry_list_t* new_set = (scope_entry_list_t*) counted_calloc(1, sizeof(*new_set), &_bytes_used_symbols);

        // Put the new entry in front of the previous
        *new_set = *result_set;

        result_set->next = new_set;
        result_set->entry = result;
    }
    else
    {
        result_set = (scope_entry_list_t*) counted_calloc(1, sizeof(*result_set), &_bytes_used_symbols);
        result_set->entry = result;
        result_set->next = NULL; // redundant, though

        hash_put(sc->hash, result->symbol_name, result_set);
    }

    return result;
}

char same_scope(scope_t* stA, scope_t* stB)
{
    return (stA->hash == stB->hash);
}

static char* scope_names[] =
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
        if (sc->qualification_name == NULL)
        {
        fprintf(stderr, "SCOPE: Looking symbol '%s' in %s (scope=%p, hash=%p)...\n", 
                name, scope_names[sc->kind], sc, sc->hash);
        }
        else
        {
        fprintf(stderr, "SCOPE: Looking symbol '%s' in %s (qualification=%s, scope=%p, hash=%p)...\n", 
                name, scope_names[sc->kind], 
                sc->qualification_name,
                sc, sc->hash);
        }
    }
    scope_entry_list_t* result = (scope_entry_list_t*) hash_get(sc->hash, name);

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
    
    scope_entry_list_t* result_set = (scope_entry_list_t*) hash_get(sc->hash, entry->symbol_name);


    if (result_set != NULL)
    {
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

        hash_put(sc->hash, entry->symbol_name, result_set);
    }
}


void remove_entry(scope_t* sc, scope_entry_t* entry)
{
    ERROR_CONDITION((entry->symbol_name == NULL), "Removing a symbol entry without name!", 0);

    scope_entry_list_t* result_set = (scope_entry_list_t*) hash_get(sc->hash, entry->symbol_name);

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
                hash_delete(sc->hash, entry->symbol_name);
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

static scope_entry_list_t* query_unqualified_name(decl_context_t decl_context,
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
        decl_context.decl_flags |= DF_UNQUALIFIED_NAME;
        return query_unqualified_name(decl_context, unqualified_name);
    }
    else
    {
        decl_context.decl_flags |= DF_QUALIFIED_NAME;
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
    return query_unqualified_name(decl_context, unqualified_name);
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

    return name_lookup(decl_context, unqualified_name);
}

static scope_entry_list_t* query_unqualified_name(decl_context_t decl_context,
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
                const char* name = uniquestr(ASTText(unqualified_name));
                if (BITMAP_TEST(decl_context.decl_flags, DF_CONSTRUCTOR))
                {
                    name = strprepend(name, "constructor ");
                }
                result = name_lookup(decl_context, name);
            }
            break;
        case AST_TEMPLATE_ID:
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
                // Note that here we are assuming that A<e> both 'A' and 'e' will
                // be found in the same context. This holds for simply unqualified
                // searches but does not for qualified lookups
                ERROR_CONDITION(
                        BITMAP_TEST(decl_context.decl_flags, DF_QUALIFIED_NAME),
                        "Do not use this function when solving a template-id in a qualified name. "
                        "It is likely that the template-name and its arguments will not be in the same context", 
                        0);
                result = query_template_id(unqualified_name, decl_context, decl_context);
            }
            break;
        case AST_DESTRUCTOR_ID:
        case AST_DESTRUCTOR_TEMPLATE_ID:
            // They have as a name ~name
            {
                AST symbol = ASTSon0(unqualified_name);
                const char *name = ASTText(symbol);
                result = name_lookup(decl_context, name);
            }
            break;
        case AST_CONVERSION_FUNCTION_ID:
            {
                char* conversion_function_name = 
                    get_conversion_function_name(decl_context, unqualified_name, /* result_type */ NULL);
                result = name_lookup(decl_context, conversion_function_name);
            }
            break;
        case AST_OPERATOR_FUNCTION_ID :
            {
                const char *operator_function_name = get_operator_function_name(unqualified_name);
                result = name_lookup(decl_context, operator_function_name);
                break;
            }
        default:
            {
                internal_error("Invalid node type '%s'\n", ast_print_node_type(ASTType(unqualified_name)));
            }
    }

    return result;
}


static scope_entry_list_t* query_qualified_name(decl_context_t nested_name_context,
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

    scope_t* qualified_scope = NULL;
    decl_context_t nested_part_context = nested_name_context;
    nested_part_context.decl_flags &= ~DF_CONSTRUCTOR;

    scope_entry_list_t* result = NULL;
    if (global_op != NULL)
    {
        nested_part_context.current_scope = nested_name_context.global_scope;
    }

    qualified_scope = nested_part_context.current_scope;

    // Looking up a nested name has a twofold process.  First we determine the
    // looking up scope by resolving all but the 'unqualified name' part
    type_t* dependent_type = NULL;

    if (nested_name != NULL)
    {
        qualified_scope = lookup_qualification_scope(nested_part_context, nested_name, unqualified_name, &dependent_type);
    }

    if (qualified_scope == NULL)
    {
        if (dependent_type != NULL)
        {
            // Create a SK_DEPENDENT_ENTITY just to acknowledge that this was
            // dependent
            scope_entry_t* dependent_entity = counted_calloc(1, sizeof(*dependent_entity), &_bytes_used_scopes);
            dependent_entity->kind = SK_DEPENDENT_ENTITY;
            dependent_entity->type_information = dependent_type;

            result = create_list_from_entry(dependent_entity);
            return result;
        }
        else
        {
            // Well, this was not dependent, so do not return anything
            return NULL;
        }
    }

    // Given the scope now we can lookup the symbol in it. Note that if some
    // scope was given we have to be able to find something there
    decl_context_t lookup_context = nested_name_context;
    lookup_context.current_scope = qualified_scope;
    lookup_context.decl_flags |= nested_name_context.decl_flags;

    if (ASTType(unqualified_name) != AST_TEMPLATE_ID
            && ASTType(unqualified_name) != AST_OPERATOR_FUNCTION_ID_TEMPLATE)
    {
        result = query_unqualified_name(lookup_context, unqualified_name);
    }
    else
    {
        result = query_template_id(unqualified_name, lookup_context, nested_name_context);
    }

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

static scope_t* lookup_qualification_scope(decl_context_t nested_name_context, 
        AST nested_name_spec, AST unqualified_part, type_t** dependent_type)
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
    scope_t* result = NULL;

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
        initial_nested_name_context.decl_flags &= ~DF_QUALIFIED_NAME;

        if (ASTType(first_qualification) == AST_TEMPLATE_ID)
        {
            starting_symbol_list = query_template_id(first_qualification, 
                    initial_nested_name_context, initial_nested_name_context);
        }
        else
        {
            starting_symbol_list = query_unqualified_name(initial_nested_name_context, first_qualification);
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
        return NULL;
    }

    scope_entry_t* starting_symbol = starting_symbol_list->entry;

    if (starting_symbol->kind == SK_DEPENDENT_ENTITY)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' is a dependent entity\n", prettyprint_in_buffer(first_qualification));
        }
        //
        // Is this case possible ? 
        //
        internal_error("Verify this case", 0);
        return NULL;
    }
    else if (starting_symbol->kind == SK_NAMESPACE)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' found to be a namespace\n", prettyprint_in_buffer(first_qualification));
        }
        // If it is a namespace work on the namespace
        result = lookup_qualification_scope_in_namespace(nested_name_context, starting_symbol, 
                next_nested_name_spec, unqualified_part, dependent_type);
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' found to be a class-name\n", prettyprint_in_buffer(first_qualification));
        }
        // Otherwise deal with classes
        result = lookup_qualification_scope_in_class(nested_name_context, starting_symbol, 
                next_nested_name_spec, unqualified_part, dependent_type);
    }

    return result;
}

// Lookup qualification within namespaces
static scope_t* lookup_qualification_scope_in_namespace(decl_context_t nested_name_context, scope_entry_t* namespace, 
        AST nested_name_spec, AST unqualified_part, type_t** dependent_type)
{
    // Lookup the name in the related scope of this namespace
    scope_t* namespace_scope = namespace->namespace_decl_context.current_scope;

    ERROR_CONDITION(namespace_scope->kind != NAMESPACE_SCOPE, 
            "Scope is not a namespace one", 0);

    // No more nested-name-spec left
    if (nested_name_spec == NULL)
        return namespace_scope;

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

    scope_entry_list_t* symbol_list = NULL;
    if (ASTType(current_name) != AST_TEMPLATE_ID)
    {
        symbol_list = query_unqualified_name(decl_context, current_name);
    }
    else
    {
        symbol_list = query_template_id(current_name, decl_context, nested_name_context);
    }

    symbol_list = filter_symbol_kind_set(symbol_list, classes_or_namespaces_filter_num_elements, classes_or_namespaces_filter);

    if (symbol_list == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' not found\n", prettyprint_in_buffer(current_name));
        }
        return NULL;
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
        return NULL;
    }
    else if (symbol->kind == SK_NAMESPACE)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' found to be a namespace\n", prettyprint_in_buffer(current_name));
        }
        return lookup_qualification_scope_in_namespace(nested_name_context, symbol, 
                next_nested_name_spec, unqualified_part, dependent_type);
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Component '%s' found to be a class-name\n", prettyprint_in_buffer(current_name));
        }
        return lookup_qualification_scope_in_class(nested_name_context, symbol, 
                next_nested_name_spec, unqualified_part, dependent_type);
    }
}

static scope_t* lookup_qualification_scope_in_class(decl_context_t nested_name_context, scope_entry_t* class_name, 
        AST nested_name_spec, AST unqualified_part, type_t** dependent_type)
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
            // If it is not a named type thus it only can be (3)
            if (is_dependent_typename_type(class_type))
            {
                // This is dependent
                *dependent_type = get_dependent_typename_type(class_name, 
                        nested_name_context, nested_name_spec, unqualified_part);
                return NULL;
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
                // Bogus context
                // FIXME - It is useful to know where a template was instantiated
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
                return NULL;
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
        return NULL;
    }
    else
    {
        internal_error("Symbol kind %d not valid", class_name->kind);
    }

    ERROR_CONDITION(!is_class_type(class_type), "The class_type is not a class type actually", 0);

    decl_context_t decl_context = class_type_get_inner_context(class_type);
    decl_context.decl_flags |= nested_name_context.decl_flags;
    scope_t* class_scope = NULL;
    class_scope = decl_context.current_scope;

    ERROR_CONDITION((class_scope == NULL), "No class scope was in that class", 0);
    ERROR_CONDITION((class_scope->kind != CLASS_SCOPE), "Class scope is not CLASS_SCOPE", 0);

    // Nothing else to be done
    if (nested_name_spec == NULL)
    {
        return class_scope;
    }

    // Look up the next qualification item
    AST current_name = ASTSon0(nested_name_spec);
    AST next_nested_name_spec = ASTSon1(nested_name_spec);

    // Note, that once a class-name has been designed in a nested-name-spec only classes
    // can appear, since classes cannot have namespaces inside them
    scope_entry_list_t* symbol_list = NULL;
    if (ASTType(current_name) != AST_TEMPLATE_ID)
    {
        symbol_list = query_unqualified_name(decl_context, current_name);
    }
    else
    {
        // FIXME - This might fail
        symbol_list = query_template_id(current_name, decl_context, nested_name_context);
    }

    symbol_list = filter_symbol_kind_set(symbol_list, classes_or_namespaces_filter_num_elements, classes_or_namespaces_filter);

    if (symbol_list == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: No class-name found for component '%s'\n", prettyprint_in_buffer(current_name));
        }
        return NULL;
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
        return NULL;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Component '%s' found to be a class-name\n", prettyprint_in_buffer(current_name));
    }

    return lookup_qualification_scope_in_class(decl_context, symbol, next_nested_name_spec, unqualified_part, dependent_type);
}

static scope_entry_list_t* name_lookup_used_namespaces(decl_context_t decl_context, 
        scope_t* current_scope, const char* name)
{
    ERROR_CONDITION(current_scope == NULL, "Current scope is null", 0);
    ERROR_CONDITION(name == NULL, "name is null", 0);

    scope_entry_list_t* result = NULL;

    // Look up in directly used namespaces, this always has to be performed
    int i;
    for (i = 0; i < current_scope->num_used_namespaces; i++)
    {
        scope_t* used_namespace = current_scope->use_namespace[i];
        scope_entry_list_t* used_namespace_search = query_name_in_scope(used_namespace, name);

        result = append_scope_entry_list(result, used_namespace_search);
    }

    // Lookup to indirectly used namespaces is only performed if unqualified
    // name or if qualified name but nothing was found in the directly used namespaces
    if (BITMAP_TEST(decl_context.decl_flags, DF_UNQUALIFIED_NAME)
            || (BITMAP_TEST(decl_context.decl_flags, DF_QUALIFIED_NAME)
                && result == NULL))
    {
        for (i = 0; i < current_scope->num_used_namespaces; i++)
        {
            scope_t* used_namespace = current_scope->use_namespace[i];
            scope_entry_list_t* recursed_search = name_lookup_used_namespaces(decl_context, used_namespace, name);

            result = append_scope_entry_list(result, recursed_search);
        }
    }

    return result;
}

#define MAX_CLASS_PATH (32)

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
        class_scope_lookup_t* derived, char is_virtual, char initial_lookup, decl_flags_t decl_flags)
{
    int i;

    ERROR_CONDITION(current_class_scope->kind != CLASS_SCOPE, "Current scope is not class-scope", 0);

    type_t* current_class_type = current_class_scope->class_type;

    ERROR_CONDITION(current_class_type == NULL, "Class scope does not have a class-type", 0);

    // Fill our information
    derived->path_length++;
    ERROR_CONDITION(derived->path_length == MAX_CLASS_PATH, "Class path too long", 0);

    derived->path[derived->path_length - 1] = current_class_type;
    derived->is_virtual[derived->path_length - 1] = is_virtual;

#define MAX_BASES (32)
    class_scope_lookup_t bases_lookup[MAX_BASES];
    memset(bases_lookup, 0, sizeof(bases_lookup));

    int num_bases = class_type_get_num_bases(current_class_type);
    ERROR_CONDITION(num_bases > MAX_BASES, "Too many bases", 0);

    for (i = 0; i < num_bases; i++)
    {
        char current_base_is_virtual = 0;
        scope_entry_t* base_class_entry = class_type_get_base_num(current_class_type, i, &current_base_is_virtual);
        type_t* base_class_type = base_class_entry->type_information;
        decl_context_t base_class_context = class_type_get_inner_context(base_class_type);
        scope_t* base_class_scope = base_class_context.current_scope;

        // Copy the info
        bases_lookup[i] = *derived;

        class_scope_lookup_rec(base_class_scope, name, &(bases_lookup[i]), 
                current_base_is_virtual, /* initial_lookup */ 0, decl_flags);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Looking in class scope '");
        for (i = 0; i < derived->path_length; i++)
        {
            fprintf(stderr, "%s%s", 
                    class_type_get_inner_context(derived->path[i]).current_scope->qualification_name,
                    ((i+1) < derived->path_length) ? "::" : "");
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
                                class_type_get_inner_context(derived->path[k]).current_scope->qualification_name,
                                ((k+1) < derived->path_length) ? "::" : "");
                    }
                    fprintf(stderr, "'\n");
                    fprintf(stderr, "SCOPE: Second one is '");
                    for (k = 0; k < current->path_length; k++)
                    {
                        fprintf(stderr, "%s%s%s", 
                                current->is_virtual[k] ? "<v>" : "",
                                class_type_get_inner_context(current->path[k]).current_scope->qualification_name,
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
                                    class_type_get_inner_context(derived->path[k]).current_scope->qualification_name,
                                    ((k+1) < derived->path_length) ? "::" : "");
                        }
                        fprintf(stderr, "'\n");
                        fprintf(stderr, "SCOPE: Second one is '");
                        for (k = 0; k < current->path_length; k++)
                        {
                            fprintf(stderr, "%s%s%s", 
                                    current->is_virtual[k] ? "<v>" : "",
                                    class_type_get_inner_context(current->path[k]).current_scope->qualification_name,
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
                                        class_type_get_inner_context(derived->path[path_candidate]).current_scope->qualification_name,
                                        class_type_get_inner_context(current->path[path_current]).current_scope->qualification_name);

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

static scope_entry_list_t* class_scope_lookup(scope_t* current_class_scope, const char* name, 
        decl_flags_t decl_flags)
{
    class_scope_lookup_t result;

    memset(&result, 0, sizeof(result));

    class_scope_lookup_rec(current_class_scope, name, &result, 0, /* initial_lookup */ 1, decl_flags);

    if (result.entry_list != NULL)
    {
        DEBUG_CODE()
        {
            int i;

            fprintf(stderr, "SCOPE: Class scope lookup started in class '%s' found name '%s' in '", 
                    current_class_scope->qualification_name,
                    name);
            for (i = 0; i < result.path_length; i++)
            {
                fprintf(stderr, "%s%s", 
                        class_type_get_inner_context(result.path[i]).current_scope->qualification_name,
                        ((i+1) < result.path_length) ? "::" : "");
            }
            fprintf(stderr, "'\n");
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Class scope lookup did not found any name '%s'\n", name);
        }
    }

    return result.entry_list;
}

scope_entry_list_t* class_context_lookup(decl_context_t decl_context, const char* name)
{
    ERROR_CONDITION(decl_context.current_scope->kind != CLASS_SCOPE, "This is not a class scope", 0);

    return class_scope_lookup(decl_context.current_scope, name, decl_context.decl_flags);
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

static scope_entry_list_t* name_lookup(decl_context_t decl_context, const char* name)
{
    ERROR_CONDITION(name == NULL, "Name cannot be null!", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Name lookup of '%s'\n", name);
    }

    scope_entry_list_t *result = NULL;

    if (!BITMAP_TEST(decl_context.decl_flags, DF_ONLY_CURRENT_SCOPE))
    {
        // This one has higher priority
        scope_t* template_scope = decl_context.template_scope;
        // The frontend should keep the template scope always up to date
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
    }
    // Otherwise continue with normal lookup
    ERROR_CONDITION((result != NULL), "The symbols found here should be NULL!", 0);

    scope_t* current_scope = decl_context.current_scope;
    while (current_scope != NULL)
    {
        if (current_scope->kind != CLASS_SCOPE
                || BITMAP_TEST(decl_context.decl_flags, DF_ONLY_CURRENT_SCOPE))
        {
            result = query_name_in_scope(current_scope, name);
        }
        else
        {
            // Class scopes require slightly different strategy
            result = class_scope_lookup(current_scope, name, decl_context.decl_flags);
        }

        if (BITMAP_TEST(decl_context.decl_flags, DF_ELABORATED_NAME))
        {
            result = filter_any_non_type(result);
        }

        // This object would hide any other in the enclosing scopes
        if (result != NULL)
        {
            return result;
        }
        
        // Do not look anything else
        if (BITMAP_TEST(decl_context.decl_flags, DF_ONLY_CURRENT_SCOPE))
        {
            return NULL;
        }

        // Otherwise, if this is a NAMESPACE_SCOPE, lookup in the used namespaces
        // note that the objects there are not hidden, but added
        if (current_scope->num_used_namespaces > 0)
        {
            result = name_lookup_used_namespaces(decl_context, current_scope, name);
            if (BITMAP_TEST(decl_context.decl_flags, DF_ELABORATED_NAME))
            {
                result = filter_any_non_type(result);
            }
            // If this yields any result, return them, no more search needed
            if (result != NULL)
            {
                return result;
            }
        }

        // If we are in a qualified lookup never lookup the enclosing one
        if (BITMAP_TEST(decl_context.decl_flags, DF_QUALIFIED_NAME))
        {
            return NULL;
        }

        ERROR_CONDITION((result != NULL), "The symbols found here should be NULL!", 0);

        current_scope = current_scope->contained_in;
    }

    return result;
}

static template_argument_t* get_corresponding_template_argument(
        template_argument_list_t* given_template_args,
        int position,
        int nesting)
{
    int i;
    for (i = 0; i < given_template_args->num_arguments; i++)
    {
        template_argument_t* result = given_template_args->argument_list[i];
        if (result->position == position
                && result->nesting == nesting)
        {
            return result;
        }
    }

    return NULL;
}

// Replaces symbols of template parameters with template arguments seen so far
//
/*
 * template <int _N, int _M = _N + 1>
 * struct A
 * {
 * };
 *
 * A<5> // must be like A<5, 6>
 *
 * default_template_arguments_context is the context of '_N + 1' where '_N' and '_M' live
 * real_template_arguments_context is the context of '5'
 */
static decl_context_t replace_template_parameters_with_values(
        template_argument_list_t* given_template_args,
        decl_context_t default_template_arguments_context,
        decl_context_t real_template_arguments_context)
{
    // Do nothing
    if (default_template_arguments_context.template_scope == NULL)
        return real_template_arguments_context;

    // We are like 'patching' the real_template_arguments_context with those symbols
    // in the default_template_arguments_context
    decl_context_t fake_context = new_template_context(real_template_arguments_context);

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Replacing template parameters\n");
    }

    Iterator* it = (Iterator*) hash_iterator_create(
            default_template_arguments_context.template_scope->hash);
    for (iterator_first(it); !iterator_finished(it); iterator_next(it))
    {
        scope_entry_list_t* entry_list = (scope_entry_list_t*) iterator_item(it);
        scope_entry_t* entry = entry_list->entry;

        template_argument_t *current_argument =
            get_corresponding_template_argument(given_template_args,
                    entry->entity_specs.template_parameter_position,
                    entry->entity_specs.template_parameter_nesting);

        if (current_argument == NULL)
        {
            // Maybe it has not been binded yet
            continue;
        }

        if(entry->entity_specs.template_parameter_position != current_argument->position
                || entry->entity_specs.template_parameter_nesting != current_argument->nesting)
        {
            continue;
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE:     Considering template parameter '%s'\n", entry->symbol_name);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE:     Creating interception symbol '%s' in fake context\n", entry->symbol_name);
        }

        // Create the new symbol in the interception context
        scope_entry_t* new_entry = new_symbol(fake_context, fake_context.template_scope, entry->symbol_name);

        // type template parameter
        if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER)
        {
            // Create a typedef, 
            new_entry->kind = SK_TYPEDEF;

            type_t* argument_type = current_argument->type;

            new_entry->entity_specs.is_template_argument = 1;
            new_entry->type_information = get_new_typedef(argument_type);

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE:     Making type template parameter '%s' of type '%s'\n", 
                        new_entry->symbol_name,
                        print_declarator(new_entry->type_information));
            }
        }
        // non-type template parameter
        else if (entry->kind == SK_TEMPLATE_PARAMETER)
        {
            type_t* expr_type = ASTExprType(current_argument->expression);
            if (expr_type != NULL
                    && is_dependent_expr_type(expr_type)
                    && is_named_type(expr_type)
                    && named_type_get_symbol(expr_type)->kind == SK_TEMPLATE_PARAMETER)
            {
                scope_entry_t* template_parameter = named_type_get_symbol(expr_type);
                // Here we sign in a variable with a related expression
                new_entry->kind = SK_TEMPLATE_PARAMETER;

                new_entry->type_information = NULL;

                new_entry->entity_specs.is_template_parameter = 1;
                new_entry->entity_specs.template_parameter_position = 
                    template_parameter->entity_specs.template_parameter_position;
                new_entry->entity_specs.template_parameter_nesting = 
                    template_parameter->entity_specs.template_parameter_nesting;
            }
            else
            {
                // Here we sign in a variable with a related expression
                new_entry->kind = SK_VARIABLE;

                AST constant_initializer = 
                    ast_copy_for_instantiation(current_argument->expression);

                new_entry->entity_specs.is_template_argument = 1;
                new_entry->expression_value = constant_initializer;
                new_entry->type_information = current_argument->type;
                new_entry->decl_context = current_argument->expression_context;

                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE:     Making non-type template parameter '%s' to have value '%s'\n", 
                            new_entry->symbol_name,
                            prettyprint_in_buffer(constant_initializer));
                }
            }
        }
        // template template parameter
        else if (entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
        {
            new_entry->kind = SK_TEMPLATE;
            // type_t* template_argument_type = 
            //     current_argument->type;

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE:     Making template template parameter '%s' to be '%s'\n", 
                        new_entry->symbol_name,
                        print_declarator(new_entry->type_information));
            }
            internal_error("Not yet implemented", 0);
        }
        else
        {
            internal_error("Unexpected symbol kind '%d'\n", entry->kind);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE:\n");
        }
    }

    return fake_context;
}


type_t* update_type(template_argument_list_t* given_template_args,
        type_t* orig_type, 
        decl_context_t template_arguments_context,
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
            template_argument_t* argument =
                get_corresponding_template_argument(given_template_args, 
                        entry->entity_specs.template_parameter_position,
                        entry->entity_specs.template_parameter_nesting);

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
                    && argument->kind == TAK_TYPE)
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
                cv_qualif |= get_cv_qualifier(argument->type);

                return get_cv_qualified_type(argument->type, cv_qualif);
            }
            else if (entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                    && argument->kind == TAK_TEMPLATE)
            {
                return argument->type;
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

            if (template_related_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                // This specialized template type comes after a template template parameter,
                // so we have to update it using the template arguments
                // We need to update this template type too
                template_argument_t* argument =
                    get_corresponding_template_argument(given_template_args, 
                            template_related_symbol->entity_specs.template_parameter_position,
                            template_related_symbol->entity_specs.template_parameter_nesting);

                // Now update the template_type with the new one
                template_type = named_type_get_symbol(argument->type)->type_information;
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
                    fprintf(stderr, "SCOPE: Updating template argument of specialized template class %d\n", i);
                }
                template_argument_t* updated_argument = update_template_argument(
                        given_template_args, 
                        template_arguments->argument_list[i],
                        template_arguments_context, filename, line);

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
                        template_arguments_context,
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
        else
        {
            // Return it unmodified
            return orig_type;
        }
    }
    else if (is_lvalue_reference_type(orig_type))
    {
        type_t* referenced = reference_type_get_referenced_type(orig_type);

        type_t* updated_referenced = update_type(given_template_args, referenced, template_arguments_context, filename, line);

        if (updated_referenced == NULL)
            return NULL;

        type_t* result_type = get_lvalue_reference_type(updated_referenced);

        return result_type;
    }
    else if (is_pointer_type(orig_type))
    {
        cv_qualifier_t cv_qualifier = get_cv_qualifier(orig_type);

        type_t* pointee = pointer_type_get_pointee_type(orig_type);

        type_t* updated_pointee = update_type(given_template_args, pointee, template_arguments_context, filename, line);

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
        type_t* updated_pointee = update_type(given_template_args, pointee, template_arguments_context, filename, line);

        if (updated_pointee == NULL)
            return NULL;

        type_t* pointee_class = pointer_to_member_type_get_class_type(orig_type);
        pointee_class = update_type(given_template_args, pointee_class, template_arguments_context, filename, line);

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
            return_type = update_type(given_template_args, return_type, template_arguments_context, filename, line);
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

            param_orig_type = update_type(given_template_args, param_orig_type, 
                    template_arguments_context, filename, line);

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

        AST updated_expr = ast_copy_for_instantiation(expression);
        decl_context_t updated_expr_context = expr_context;
        if (updated_expr != NULL)
        {
            updated_expr_context = replace_template_parameters_with_values(
                    given_template_args, 
                    expr_context, 
                    template_arguments_context);
            // Update type info
            ERROR_CONDITION (!check_for_expression(updated_expr, updated_expr_context),
                    "Updated expression '%s' in array declaration could not be checked", 0);

            if (!is_dependent_expression(updated_expr, updated_expr_context)
                    && (ASTExprType(updated_expr) == NULL
                        || !is_unresolved_overloaded_type(ASTExprType(updated_expr))))
            {
                literal_value_t literal = evaluate_constant_expression(updated_expr, updated_expr_context);
                updated_expr = tree_from_literal_value(literal);
            }
        }

        type_t* element_type = array_type_get_element_type(orig_type);
        element_type = update_type(given_template_args, element_type,
                template_arguments_context, filename, line);

        if (element_type == NULL)
            return NULL;

        type_t* updated_array_type = get_array_type(element_type, updated_expr, updated_expr_context);

        updated_array_type = get_cv_qualified_type(updated_array_type, cv_qualifier);

        return updated_array_type;
    }
    else if (is_dependent_typename_type(orig_type))
    {
        decl_context_t dependent_decl_context;
        scope_entry_t* dependent_entry = NULL;
        AST nested_name = NULL;
        AST unqualified_part = NULL;

        dependent_typename_get_components(orig_type, 
                &dependent_entry, &dependent_decl_context, 
                &nested_name, &unqualified_part);

        type_t* fixed_type = NULL;
        fixed_type = update_type(given_template_args, get_user_defined_type(dependent_entry),
                template_arguments_context, filename, line);

        if (fixed_type == NULL)
            return NULL;

        if (!is_named_class_type(fixed_type))
        {
            return NULL;
        }

        // Now lookup again in this class
        // Get the inner class
        type_t* class_type = get_actual_class_type(fixed_type);

        if (class_type_is_incomplete_independent(class_type))
        {
            instantiate_template_class(named_type_get_symbol(fixed_type),
                    template_arguments_context, filename, line);
        }
        else if (class_type_is_incomplete_dependent(class_type)
                || class_type_is_complete_dependent(class_type))
        {
            // Nothing can be done here
            return orig_type;
        }

        decl_context_t inner_context = class_type_get_inner_context(class_type);

        scope_entry_list_t* result_list = query_qualified_name(inner_context, 
                NULL, nested_name, unqualified_part);

        result_list = filter_any_non_type(result_list);

        if (result_list == NULL)
        {
            return NULL;
        }

        scope_entry_t* entry = result_list->entry;

        return entry->type_information;
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

template_argument_t* update_template_argument(template_argument_list_t* given_template_args,
        template_argument_t* current_template_arg,
        decl_context_t template_arguments_context,
        const char *filename, int line)
{
    template_argument_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);
    result->kind = current_template_arg->kind;

    switch (current_template_arg->kind)
    {
        case TAK_TYPE:
            {
                result->type = update_type(given_template_args, current_template_arg->type, 
                        template_arguments_context, filename, line);

                ERROR_CONDITION ((result->type == NULL), 
                        "type template argument could not be updated", 0);
                break;
            }
        case TAK_TEMPLATE:
            {
                result->type = update_type(given_template_args, 
                        current_template_arg->type, 
                        template_arguments_context, filename, line);

                ERROR_CONDITION ((result->type == NULL), 
                        "template template argument could not be updated", 0);
                break;
            }
        case TAK_NONTYPE:
            {
                result->type = update_type(given_template_args, current_template_arg->type, 
                        template_arguments_context, filename, line);
                result->expression_context = replace_template_parameters_with_values(
                        given_template_args, 
                        current_template_arg->expression_context, 
                        template_arguments_context);

                // We do not want any residual type information here
                result->expression = ast_copy_for_instantiation(current_template_arg->expression);

                // Update type information 
                ERROR_CONDITION( (!check_for_expression(result->expression, result->expression_context)),
                        "Updated nontype template parameter has an invalid expression", 0);

                type_t* expr_type = ASTExprType(result->expression);
                // Fold the argument
                if (!is_dependent_expression(result->expression, result->expression_context)
                        && (expr_type == NULL
                            || !is_unresolved_overloaded_type(expr_type)))
                {
                    literal_value_t literal 
                        = evaluate_constant_expression(result->expression, result->expression_context);

                    result->expression = tree_from_literal_value(literal);
                }

                ERROR_CONDITION ((result->type == NULL), 
                        "type/template template could not be updated", 0);

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

                    type_t* expr_type = ASTExprType(t_argument->expression);

                    if (!is_dependent_expression(t_argument->expression, 
                                t_argument->expression_context)
                            && (expr_type == NULL 
                                || !is_unresolved_overloaded_type(expr_type)))
                    {
                        literal_value_t literal = evaluate_constant_expression(t_argument->expression,
                                t_argument->expression_context);
                        t_argument->expression = tree_from_literal_value(literal);
                        t_argument->type = expr_type;
                    }
                    else
                    {
                        // Do not clear extended data
                        t_argument->expression = ast_copy(t_argument->expression);
                    }
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
        if (ASTExprType(tree) == unresolved_type)
        {
            ast_set_expression_type(tree, solved_type);
        }

        int i;
        for (i = 0; i < MAX_AST_CHILDREN; i++)
        {
            update_unresolved_overloaded_type(unresolved_type, solved_type, ASTChild(tree, i));
        }
    }
}

template_argument_list_t *get_template_arguments_of_template_id(
        AST template_id,
        type_t* template_type,
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

                    current_arg->type = update_type(result, 
                            template_parameters->template_parameters[num_argument]->entry->type_information,
                            template_arguments_context,
                            ASTFileName(template_id), ASTLine(template_id));
                    current_arg->expression_context = template_arguments_context;

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "SCOPE: Type of nontype template parameter updated to '%s'\n",
                                print_declarator(current_arg->type));
                    }

                    /*
                     * If the type is an address of function try to solve it
                     */
                    if (ASTExprType(current_arg->expression) != NULL
                            && is_unresolved_overloaded_type(ASTExprType(current_arg->expression)))
                    {
                        // Try to solve it
                        scope_entry_t* solved_function =
                            address_of_overloaded_function(
                                    unresolved_overloaded_type_get_overload_set(ASTExprType(current_arg->expression)),
                                    unresolved_overloaded_type_get_explicit_template_arguments(ASTExprType(current_arg->expression)),
                                    current_arg->type,
                                    template_arguments_context,
                                    ASTFileName(template_id),
                                    ASTLine(template_id));

                        if (solved_function != NULL)
                        {
                            // Update the type throughout the expression (this is needed when evaluating it)
                            update_unresolved_overloaded_type(ASTExprType(current_arg->expression),
                                    solved_function->type_information,
                                    current_arg->expression);

                            if (function_type_is_incomplete_independent(solved_function->type_information))
                            {
                                instantiate_template_function(solved_function,
                                        template_arguments_context,
                                        ASTFileName(template_id),
                                        ASTLine(template_id));
                            }
                        }
                    }


                    ERROR_CONDITION(current_arg->type == NULL, "Could not update properly template argument", 0);
                    break;
                }
            default:
                {
                    internal_error("Invalid template argument kind", 0);
                }
                break;
        }
    }

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

        template_argument_t* updated_argument = update_template_argument(result, 
                original_default_arg, template_arguments_context, 
                ASTFileName(template_id), ASTLine(template_id));

        P_LIST_ADD(result->argument_list, result->num_arguments, updated_argument);

        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Template argument updated\n");
        }
    }

    return result;
}

// This function never instantiates a template, it might create a specialization though
static scope_entry_list_t* query_template_id(AST template_id, 
        decl_context_t template_name_context,
        decl_context_t template_arguments_context)
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

    scope_entry_list_t* template_symbol_list = name_lookup(template_name_context, template_name);

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

    scope_entry_t* template_symbol = template_symbol_list->entry;

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
        template_argument_list_t* template_arguments = get_template_arguments_of_template_id(template_id,
                generic_type, template_arguments_context, &is_valid);

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

        if (template_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
        {
            // The specialized type alwas has to be dependent to avoid
            // instantiating it
            class_type_set_incomplete_dependent(named_type_get_symbol(specialized_type)->type_information);
        }

        ERROR_CONDITION(!is_named_type(specialized_type), "This should be a named type", 0);

        // Crappy
        scope_entry_list_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);
        result->entry = named_type_get_symbol(specialized_type);

        return result;
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

static scope_entry_list_t* append_scope_entry_list(scope_entry_list_t* orig, scope_entry_list_t* appended)
{
    // This avoids some copies
    if (orig == NULL)
    {
        return appended;
    }
    if (appended == NULL)
    {
        return orig;
    }

    // We have to copy the list to avoid modifying the orig one
    // that might be referenced in the scopes
    scope_entry_list_t* copy_orig = copy_entry_list(orig);
    scope_entry_list_t* it = copy_orig;
    while (it->next != NULL)
    {
        it = it->next;
    }

    it->next = appended;

    return copy_orig;
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
            if (current_scope->qualification_name != NULL)
            {
                const char* nested_name = strappend(current_scope->qualification_name, "::");
                result = strappend(nested_name, result);
            }

            current_scope = current_scope->contained_in;
        }
    }

    return result;
}

static scope_entry_t* find_template_parameter(scope_t* st, scope_entry_t* template_param)
{
    Iterator* it = (Iterator*) hash_iterator_create(st->hash);
    for ( iterator_first(it); 
            !iterator_finished(it); 
            iterator_next(it))
    {
        scope_entry_list_t* entry_list = (scope_entry_list_t*) iterator_item(it);

        scope_entry_t* entry = entry_list->entry;

        if (entry->kind == template_param->kind
                && (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                    || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
                && (entry->entity_specs.template_parameter_position 
                    == template_param->entity_specs.template_parameter_position)
                && (entry->entity_specs.template_parameter_nesting 
                    == template_param->entity_specs.template_parameter_nesting))
        {
            return entry;
        }
    }

    return NULL;
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
            && is_template_specialized_type(entry->type_information))
    {
        const char *template_arguments = get_unqualified_template_symbol_name(entry, decl_context);
        result = strappend(result, template_arguments);
    }


    if (entry->entity_specs.is_member)
    {
        // We need the qualification of the class
        if (is_named_class_type(entry->entity_specs.class_type))
        {
            scope_entry_t* class_symbol = named_type_get_symbol(entry->entity_specs.class_type);

            (*max_qualif_level)++;

            const char* class_qualification = 
                get_fully_qualified_symbol_name(class_symbol, decl_context, is_dependent, max_qualif_level);

            class_qualification = strappend(class_qualification, "::");

            result = strappend(class_qualification, result);
        }
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

scope_entry_list_t* cascade_lookup(decl_context_t decl_context, const char* name)
{
    // This function is a simplified version of name_lookup, it turns that
    // name_lookup is complex enough to avoid touching it unless needed

    ERROR_CONDITION(name == NULL, "Name cannot be null!", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Cascade lookup of '%s'\n", name);
    }

    scope_entry_list_t *result = NULL;

    // This one has higher priority
    scope_t* template_scope = decl_context.template_scope;
    // The frontend should keep the template scope always up to date
    while (template_scope != NULL)
    {
        ERROR_CONDITION((template_scope->kind != TEMPLATE_SCOPE), "This is not a template scope!", 0);

        result = append_scope_entry_list(result, query_name_in_scope(template_scope, name));

        // If nothing was found, look up in the enclosing template_scope
        // (they form a stack of template_scopes)
        template_scope = template_scope->contained_in;
    }

    scope_t* current_scope = decl_context.current_scope;
    while (current_scope != NULL)
    {
        if (current_scope->kind != CLASS_SCOPE)
        {
            result = append_scope_entry_list(result, query_name_in_scope(current_scope, name));
        }
        else
        {
            // Class scopes require slightly different strategy
            result = append_scope_entry_list(result, class_scope_lookup(current_scope, name, decl_context.decl_flags));
        }

        // Otherwise, if this is a NAMESPACE_SCOPE, lookup in the used namespaces
        // note that the objects there are not hidden, but added
        if (current_scope->num_used_namespaces > 0)
        {
            result = append_scope_entry_list(result, name_lookup_used_namespaces(decl_context, current_scope, name));
        }

        current_scope = current_scope->contained_in;
    }

    return result;
}
