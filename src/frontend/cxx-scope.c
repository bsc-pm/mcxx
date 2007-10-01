/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
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
#include "cxx-buildscope.h"
#include "hash.h"
#include "hash_iterator.h"

// Lookup of a simple name within a given declaration context
static scope_entry_list_t* name_lookup(decl_context_t decl_context, char* name);

// Solve a template given a template-id, a list of found names for the template-id and the declaration context
// static scope_entry_list_t* solve_template(decl_context_t decl_context, scope_entry_list_t* found_names, AST template_id);

// Looks up the qualification scope for a nested-name-spec
static scope_t* lookup_qualification_scope(decl_context_t decl_context, AST nested_name, char *is_dependent);

static scope_t* lookup_qualification_scope_in_namespace(decl_context_t nested_name_context, scope_entry_t* namespace, 
        AST nested_name_spec, char *is_dependent);
static scope_t* lookup_qualification_scope_in_class(decl_context_t nested_name_context, scope_entry_t* class_name, 
        AST nested_name_spec, char *is_dependent);

static scope_entry_list_t* query_template_id(AST template_id, decl_context_t lookup_context, 
        decl_context_t nested_name_context, decl_flags_t decl_flags);

// Appends scope entry lists
static scope_entry_list_t* append_scope_entry_list(scope_entry_list_t* orig, scope_entry_list_t* appended);

// Scope creation functions
static scope_t* new_scope(void);
static scope_t* new_namespace_scope(scope_t* st, char* qualification_name);
static scope_t* new_prototype_scope(scope_t* st);
static scope_t* new_block_scope(scope_t* enclosing_scope);
static scope_t* new_class_scope(scope_t* enclosing_scope, char* qualification_name);
static scope_t* new_template_scope(scope_t* enclosing_scope);

static scope_entry_list_t* name_lookup_bases(scope_t* current_scope, char* name);
static scope_entry_list_t* name_lookup_used_namespaces(decl_context_t decl_context, 
        scope_t* current_scope, char* name);

/* Scope creation functions */
/*
 * There was a time when the compiler worked directly with scopes instead
 * of declarative context, that these were the functions used
 * to create them.
 */

// Any new scope should be created using this one
static scope_t* new_scope(void)
{
    scope_t* result = calloc(1, sizeof(*result));

    result->hash = hash_create(HASH_SIZE, HASHFUNC(prime_hash), KEYCMPFUNC(strcmp));

    return result;
}

// Creates a new namespace scope and optionally it gives it a
// qualification_name. Global scope has st == NULL and qualification_name == NULL
static scope_t* new_namespace_scope(scope_t* st, char* qualification_name)
{
    scope_t* result = new_scope();

    result->kind = NAMESPACE_SCOPE;
    result->contained_in = st;
    if (qualification_name != NULL)
    {
        result->qualification_name = strdup(qualification_name);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "%s: New namespace scope '%p' (qualification='%s') created\n", 
                __FUNCTION__, result, result->qualification_name);
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
        fprintf(stderr, "%s: new prototype scope '%p' created\n", __FUNCTION__, result);
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
        fprintf(stderr, "%s: New block scope '%p' created\n", __FUNCTION__, result);
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
        fprintf(stderr, "%s: New function scope '%p' created\n", __FUNCTION__, result);
    }
    
    return result;
}

// Creates a new class scope and optionally it is given a qualification name
static scope_t* new_class_scope(scope_t* enclosing_scope, char* qualification_name)
{
    scope_t* result = new_scope();

    result->kind = CLASS_SCOPE;
    result->contained_in = enclosing_scope;

    if (qualification_name != NULL)
    {
        result->qualification_name = strdup(qualification_name);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "%s: New class scope '%p' (qualif=%s) scope created\n", __FUNCTION__, 
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
        fprintf(stderr, "%s: New template scope '%p' created\n", __FUNCTION__, result);
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

decl_context_t new_namespace_context(decl_context_t enclosing_decl_context, char* qualification_name)
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

decl_context_t new_class_context(decl_context_t enclosing_context, char* qualification_name)
{
    ERROR_CONDITION(enclosing_context.current_scope->kind != NAMESPACE_SCOPE
            && enclosing_context.current_scope->kind != CLASS_SCOPE
            && enclosing_context.current_scope->kind != BLOCK_SCOPE, /* The last case yields a local class */
            "Enclosing scope is neither namespace, class or local", 0
            );

    // Inherit the scope
    decl_context_t result = enclosing_context;

    // Create new class scope
    result.class_scope = new_class_scope(enclosing_context.current_scope, qualification_name);

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

    return result;
}


// Normally we work on decl_context.current_scope but for template parameters
// sc != decl_context.current_scope so allow the user such freedom
scope_entry_t* new_symbol(decl_context_t decl_context, scope_t* sc, char* name)
{
    scope_entry_list_t* result_set = (scope_entry_list_t*) hash_get(sc->hash, name);

    scope_entry_t* result;

    result = calloc(1, sizeof(*result));
    result->symbol_name = strdup(name);
    // Remember, for template parameters, .current_scope will not contain
    // its declaration scope but will be in .template_scope
    result->decl_context = decl_context;

    if (result_set != NULL)
    {
        scope_entry_list_t* new_set = (scope_entry_list_t*) calloc(1, sizeof(*new_set));

        // Put the new entry in front of the previous
        *new_set = *result_set;

        result_set->next = new_set;
        result_set->entry = result;
    }
    else
    {
        result_set = (scope_entry_list_t*) calloc(1, sizeof(*result_set));
        result_set->entry = result;
        result_set->next = NULL; // redundant, though

        hash_put(sc->hash, name, result_set);
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

static scope_entry_list_t* query_name_in_scope(scope_t* sc, char* name)
{
    DEBUG_CODE()
    {
        if (sc->qualification_name == NULL)
        {
        fprintf(stderr, "Looking symbol '%s' in %s (scope=%p, hash=%p)...\n", 
                name, scope_names[sc->kind], sc, sc->hash);
        }
        else
        {
        fprintf(stderr, "Looking symbol '%s' in %s (qualification=%s, scope=%p, hash=%p)...\n", 
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
            fprintf(stderr, "Symbol '%s' NOT found in scope '%p' [%p]\n", name, sc, sc->hash);
        }
        else
        {
            fprintf(stderr, "Symbol '%s' found in scope '%p' [%p]\n", name, sc, sc->hash);
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
        scope_entry_list_t* new_set = (scope_entry_list_t*) calloc(1, sizeof(*new_set));

        // Put the new entry in front of the previous
        *new_set = *result_set;

        result_set->next = new_set;
        result_set->entry = entry;
    }
    else
    {
        result_set = (scope_entry_list_t*) calloc(1, sizeof(*result_set));
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
    scope_entry_list_t* result = calloc(1, sizeof(*result));
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
                scope_entry_list_t* new_item = calloc(1, sizeof(*new_item));
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
            scope_entry_list_t* new_item = calloc(1, sizeof(*new_item));
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
    scope_entry_list_t* result = NULL;
    scope_entry_list_t* iter = entry_list;
    
    while (iter != NULL)
    {
        if (f(iter->entry))
        {
            scope_entry_list_t* new_item = calloc(1, sizeof(*new_item));
            new_item->entry = iter->entry;
            new_item->next = result;
            result = new_item;
        }

        iter = iter->next;
    }

    return result;
}

/*
 * Query functions
 *
 * This is the only one that should be used
 */
static scope_entry_list_t* query_qualified_name(decl_context_t decl_context,
        AST global_op,
        AST nested_name,
        AST unqualified_name);
static scope_entry_list_t* query_unqualified_name(decl_context_t decl_context,
        AST unqualified_name);

scope_entry_list_t* query_id_expression_flags(decl_context_t decl_context,
        AST id_expression, decl_flags_t decl_flags)
{
    switch (ASTType(id_expression))
    {
        case AST_SYMBOL :
        case AST_DESTRUCTOR_ID :
        case AST_DESTRUCTOR_TEMPLATE_ID :
        case AST_CONVERSION_FUNCTION_ID :
        case AST_OPERATOR_FUNCTION_ID :
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            return query_nested_name_flags(decl_context, NULL, NULL, id_expression, decl_flags);
        case AST_QUALIFIED_OPERATOR_FUNCTION_ID :
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
        // Friend lookups are not spec
        return query_qualified_name(decl_context, global_op, nested_name, unqualified_name);
    }
}

scope_entry_list_t* query_in_scope_str_flags(decl_context_t decl_context,
        char* name, decl_flags_t decl_flags)
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
        char* unqualified_name, decl_flags_t decl_flags)
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
                char* name = ASTText(unqualified_name);
                if (BITMAP_TEST(decl_context.decl_flags, DF_CONSTRUCTOR))
                {
                    name = strprepend(name, "constructor ");
                }
                result = name_lookup(decl_context, name);
            }
            break;
        case AST_TEMPLATE_ID:
            {
                // Note that here we are assuming that A<e> both 'A' and 'e' will
                // can be found in the same context. This holds for simply unqualified
                // searches but does not for qualified lookups
                ERROR_CONDITION(
                        BITMAP_TEST(decl_context.decl_flags, DF_QUALIFIED_NAME),
                        "Do not use this function when solving a template-id in a qualified name. "
                        "It is likely that the template-name and its arguments will not be in the same context", 
                        0);
                result = query_template_id(unqualified_name, decl_context, decl_context, DF_NONE);
            }
            break;
        case AST_DESTRUCTOR_ID:
        case AST_DESTRUCTOR_TEMPLATE_ID:
            // They have as a name ~name
            {
                AST symbol = ASTSon0(unqualified_name);
                char *name = ASTText(symbol);
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
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
                char *operator_function_name = get_operator_function_name(unqualified_name);
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
        char* c = prettyprint_in_buffer(unqualified_name);

        if (ASTType(unqualified_name) != AST_TEMPLATE_ID)
        {
            if (BITMAP_TEST(nested_name_context.decl_flags, DF_CONSTRUCTOR))
            {
                c = strprepend(c, "constructor ");
            }
        }

        fprintf(stderr, "Solving qualified-id '%s%s%s'\n", prettyprint_in_buffer(global_op), 
                prettyprint_in_buffer(nested_name),
                c);
    }

    // Ignore invalid nested name specs
    if (!check_nested_name_spec(nested_name, nested_name_context))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "This nested-name-spec '%s' is not valid\n", prettyprint_in_buffer(nested_name));
        }
        return NULL;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Nested-name-spec '%s' seems fine\n", prettyprint_in_buffer(nested_name));
        }
    }

    if (ASTType(unqualified_name) == AST_TEMPLATE_ID)
    {
        solve_possibly_ambiguous_template_id(unqualified_name, nested_name_context);
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
    char is_dependent = 0;

    if (nested_name != NULL)
    {
        qualified_scope = lookup_qualification_scope(nested_part_context, nested_name, &is_dependent);
    }

    if (qualified_scope == NULL)
    {
        if (is_dependent)
        {
            // Create a SK_DEPENDENT_ENTITY just to acknowledge that this was
            // dependent
            scope_entry_t* dependent_entity = calloc(1, sizeof(*dependent_entity));
            dependent_entity->kind = SK_DEPENDENT_ENTITY;

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
    if (ASTType(unqualified_name) != AST_TEMPLATE_ID)
    {
        result = query_in_scope_flags(lookup_context, unqualified_name, nested_name_context.decl_flags);
        // If this is a class_scope we have too give a try to bases
        if (lookup_context.current_scope->kind == CLASS_SCOPE
                && result == NULL)
        {
            result = name_lookup_bases(lookup_context.current_scope, ASTText(unqualified_name));
        }
    }
    else
    {
        decl_flags_t decl_flags = DF_ONLY_CURRENT_SCOPE;

        if (is_dependent)
            decl_flags |= DF_NO_FAIL;

        result = query_template_id(unqualified_name, lookup_context, nested_name_context, decl_flags);
    }

    // Make it dependent if the last component was not found
    if (result == NULL
            && is_dependent)
    {
        scope_entry_t* dependent_entity = calloc(1, sizeof(*dependent_entity));
        dependent_entity->kind = SK_DEPENDENT_ENTITY;

        result = create_list_from_entry(dependent_entity);
    }

    return result;
}

static enum cxx_symbol_kind classes_or_namespaces_filter[] = {
    // These two are obvious
    SK_CLASS, 
    SK_NAMESPACE, 
    // This one is somewhat annoying
    SK_TYPEDEF, 
    // These two are involved in 'typename _T::' things
    SK_TEMPLATE_TYPE_PARAMETER, 
    SK_TEMPLATE_TEMPLATE_PARAMETER,
    // SK_TEMPLATE_SPECIALIZED_CLASS seems odd here,
    // but it is possible like it is SK_CLASS. Again this
    // is due to the injected symbol of an instantiated
    // class
    SK_TEMPLATE_SPECIALIZED_CLASS,
    // This one only happens when there is an exact match
    SK_TEMPLATE_PRIMARY_CLASS,
    // Inner functions might return this
    SK_DEPENDENT_ENTITY
};

static int classes_or_namespaces_filter_num_elements =
    sizeof(classes_or_namespaces_filter) / sizeof(classes_or_namespaces_filter[0]);

static scope_t* lookup_qualification_scope(decl_context_t nested_name_context, 
        AST nested_name_spec, char *is_dependent)
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
    ERROR_CONDITION(is_dependent == NULL, "Invalid argument is_dependent", 0);
    *is_dependent = 0;
    ERROR_CONDITION(nested_name_spec == NULL, "The first nested-name-spec is null", 0);
    scope_t* result = NULL;

    DEBUG_CODE()
    {
        fprintf(stderr, "Solving nested-name-spec '%s'\n", prettyprint_in_buffer(nested_name_spec));
    }

    AST first_qualification = ASTSon0(nested_name_spec);
    AST next_nested_name_spec = ASTSon1(nested_name_spec);
    
    ERROR_CONDITION((ASTType(first_qualification) != AST_SYMBOL 
                && (ASTType(first_qualification) != AST_TEMPLATE_ID)), 
            "The qualification part is neither a symbol nor a template-id", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "Solving first component '%s' of qualified-id '%s'\n", 
                prettyprint_in_buffer(first_qualification), 
                prettyprint_in_buffer(nested_name_spec));
    }

    // The first is solved as an unqualified entity, taking into account that it might be
    // a dependent entity (like '_T' above)
    scope_entry_list_t* starting_symbol_list = NULL;
    if (ASTType(first_qualification) == AST_TEMPLATE_ID)
    {
        starting_symbol_list = query_template_id(first_qualification, 
                nested_name_context, nested_name_context, DF_NONE);
    }
    else
    {
        starting_symbol_list = query_unqualified_name(nested_name_context, first_qualification);
    }
    // Filter found symbols
    starting_symbol_list = filter_symbol_kind_set(starting_symbol_list, 
            classes_or_namespaces_filter_num_elements, classes_or_namespaces_filter);


    // Nothing was found
    if (starting_symbol_list == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "First component '%s' not found\n", prettyprint_in_buffer(first_qualification));
        }
        return NULL;
    }

    scope_entry_t* starting_symbol = starting_symbol_list->entry;

    if (starting_symbol->kind == SK_DEPENDENT_ENTITY)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Component '%s' is a dependent entity\n", prettyprint_in_buffer(first_qualification));
        }
        *is_dependent = 1;
        return NULL;
    }
    else if (starting_symbol->kind == SK_NAMESPACE)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Component '%s' found to be a namespace\n", prettyprint_in_buffer(first_qualification));
        }
        // If it is a namespace work on the namespace
        result = lookup_qualification_scope_in_namespace(nested_name_context, starting_symbol, 
                next_nested_name_spec, is_dependent);
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Component '%s' found to be a class-name\n", prettyprint_in_buffer(first_qualification));
        }
        // Otherwise deal with classes
        result = lookup_qualification_scope_in_class(nested_name_context, starting_symbol, 
                next_nested_name_spec, is_dependent);
    }

    return result;
}

// Lookup qualification within namespaces
static scope_t* lookup_qualification_scope_in_namespace(decl_context_t nested_name_context, scope_entry_t* namespace, 
        AST nested_name_spec, char *is_dependent)
{
    // Lookup the name in the related scope of this namespace
    scope_t* namespace_scope = namespace->related_decl_context.current_scope;

    ERROR_CONDITION(namespace_scope->kind != NAMESPACE_SCOPE, 
            "Scope is not a namespace one", 0);

    // No more nested-name-spec left
    if (nested_name_spec == NULL)
        return namespace_scope;

    // Update the context
    decl_context_t decl_context = namespace->related_decl_context;

    AST current_name = ASTSon0(nested_name_spec);
    AST next_nested_name_spec = ASTSon1(nested_name_spec);

    ERROR_CONDITION(((ASTType(current_name) != AST_SYMBOL)
            && (ASTType(current_name) != AST_TEMPLATE_ID)), 
            "nested-name-spec is neither an identifier nor a template-id", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "Solving component of nested-name '%s'\n", prettyprint_in_buffer(current_name));
    }

    scope_entry_list_t* symbol_list = NULL;
    if (ASTType(current_name) != AST_TEMPLATE_ID)
    {
        symbol_list = query_in_scope(decl_context, current_name);
    }
    else
    {
        symbol_list = query_template_id(current_name, decl_context, nested_name_context, DF_ONLY_CURRENT_SCOPE);
    }

    symbol_list = filter_symbol_kind_set(symbol_list, classes_or_namespaces_filter_num_elements, classes_or_namespaces_filter);

    if (symbol_list == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Component '%s' not found\n", prettyprint_in_buffer(current_name));
        }
        return NULL;
    }

    scope_entry_t* symbol = symbol_list->entry;

    if (symbol->kind == SK_DEPENDENT_ENTITY)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Component '%s' is a dependent entity\n", prettyprint_in_buffer(current_name));
        }
        *is_dependent = 1;
        return NULL;
    }
    else if (symbol->kind == SK_NAMESPACE)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Component '%s' found to be a namespace\n", prettyprint_in_buffer(current_name));
        }
        return lookup_qualification_scope_in_namespace(nested_name_context, symbol, 
                next_nested_name_spec, is_dependent);
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Component '%s' found to be a class-name\n", prettyprint_in_buffer(current_name));
        }
        return lookup_qualification_scope_in_class(nested_name_context, symbol, 
                next_nested_name_spec, is_dependent);
    }
}

static scope_t* lookup_qualification_scope_in_class(decl_context_t nested_name_context, scope_entry_t* class_name, 
        AST nested_name_spec, char *is_dependent)
{
    ERROR_CONDITION(class_name == NULL, "The class name cannot be null", 0);

    scope_t* class_scope = NULL;
    type_t* class_type = NULL;

    /*
     * typedefs are handled twice because they might be naming other
     * named entities
     */
    if (class_name->kind == SK_TYPEDEF)
    {
        class_type = advance_over_typedefs(class_name->type_information);
        if (is_named_type(class_type))
        {
            class_name = named_type_get_symbol(class_type);
        }
    }

    if (class_name->kind == SK_CLASS)
    {
        // Normal, non-template classes
        class_type = class_name->type_information;
    }
    else if (class_name->kind == SK_TYPEDEF)
    {
        // Only unnamed typedefs should reach here, and definitely they cannot
        // be templates
        /*
         * typedefs are annoying because they might name unnamed classes
         * or templated ones
         *
         * typedef struct
         * {
         *     typedef int T;
         *     typedef struct
         *     {
         *         typedef int P;
         *     } B;
         * } A;
         * 
         * A::T t1;
         * A::B::P t2;
         *
         * but, fortunately, this only concerns to the scope considered
         * which, since this is a qualified-name, must have some name, even
         * if it is by means of a typedef.
         */
        /*
         * We will work on the types. First advance the type over any typedef
         */
        class_type = advance_over_typedefs(class_name->type_information);
        ERROR_CONDITION(is_named_type(class_type), "This type must already be unnamed", 0);
    }
    // templated classes (both primaries and specializations)
    else if ((class_name->kind == SK_TEMPLATE_PRIMARY_CLASS)
            || (class_name->kind == SK_TEMPLATE_SPECIALIZED_CLASS))
    {
        class_type = class_name->type_information;

        if (class_type_is_incomplete_independent(class_type))
        {
            instantiate_template(class_name, nested_name_context);
        }
        else if (class_type_is_complete_dependent(class_type)
                || class_type_is_incomplete_dependent(class_type))
        {
            // Others will be dependent entities
            *is_dependent = 1;
        }
        // Complete independent are not a problem
    }
    else if (class_name->kind == SK_TEMPLATE_TYPE_PARAMETER
            || class_name->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        // We cannot do anything else here but returning NULL
        // and stating that it is dependent
        *is_dependent = 1;
        class_type = NULL;
    }
    else
    {
        internal_error("Symbol kind %d not valid", class_name->kind);
    }

    // The type might be a dependent one
    if (is_template_dependent_type(class_type))
    {
        *is_dependent = 1;
    }

    decl_context_t decl_context = class_name->related_decl_context;

    // In this case, nothing to do remains
    if (*is_dependent 
            && decl_context.current_scope == NULL)
        return NULL;

    ERROR_CONDITION((class_type == NULL), "Class type null", 0);

    ERROR_CONDITION(!is_class_type(class_type), "The class_type is not a class type actually", 0);

    // This gives the _real_ class type (not any indirection by means of class names)
    class_type = get_actual_class_type(class_type);

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
        symbol_list = query_in_scope(decl_context, current_name);
        if (symbol_list == NULL)
        {
            // This is a class scope, we have to lookup bases too
            symbol_list = name_lookup_bases(decl_context.current_scope, ASTText(current_name));
        }
    }
    else
    {
        decl_flags_t decl_flags = DF_ONLY_CURRENT_SCOPE;

        // If this is true here we DO have a related_decl_context 
        // but we do not want to fail if nothing found there
        if (*is_dependent)
            decl_flags |= DF_NO_FAIL;

        symbol_list = query_template_id(current_name, decl_context, nested_name_context, decl_flags);
    }

    symbol_list = filter_symbol_kind_set(symbol_list, classes_or_namespaces_filter_num_elements, classes_or_namespaces_filter);

    if (symbol_list == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "No class-name found for component '%s'\n", prettyprint_in_buffer(current_name));
        }
        return NULL;
    }

    ERROR_CONDITION(symbol_list->next != NULL, "More than one class found", 0);

    scope_entry_t* symbol = symbol_list->entry;

    if (symbol->kind == SK_DEPENDENT_ENTITY)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Component '%s' is a dependent entity\n", prettyprint_in_buffer(current_name));
        }
        *is_dependent = 1;
        return NULL;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Component '%s' found to be a class-name\n", prettyprint_in_buffer(current_name));
    }

    return lookup_qualification_scope_in_class(decl_context, symbol, next_nested_name_spec, is_dependent);
}

static scope_entry_list_t* name_lookup_used_namespaces(decl_context_t decl_context, 
        scope_t* current_scope, char* name)
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

static scope_entry_list_t* name_lookup_bases(scope_t* current_scope, char* name)
{
    scope_entry_list_t* result = NULL;
    ERROR_CONDITION(current_scope->kind != CLASS_SCOPE, "Current scope is not class-scope", 0);

    int i;
    for (i = 0; i < current_scope->num_base_scopes; i++)
    {
        scope_t* base_scope = current_scope->base_scope[i];
        scope_entry_list_t* base_scope_search = query_name_in_scope(base_scope, name);

        result = append_scope_entry_list(result, base_scope_search);

        // Recursively find bases of bases
        scope_entry_list_t* recursive_base_search = 
            name_lookup_bases(base_scope, name);

        result = append_scope_entry_list(result, recursive_base_search);
    }

    return result;
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
        SK_TEMPLATE_PRIMARY_CLASS,
        SK_TEMPLATE_SPECIALIZED_CLASS,
        SK_GCC_BUILTIN_TYPE
    };

    int type_filter_num = sizeof(type_filter) / sizeof(type_filter[0]);

    return filter_symbol_kind_set(entry_list, type_filter_num, type_filter);
}

static scope_entry_list_t* name_lookup(decl_context_t decl_context, char* name)
{
    ERROR_CONDITION(name == NULL, "Name cannot be null!", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "Name lookup of '%s'\n", name);
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
        result = query_name_in_scope(current_scope, name);

        if (BITMAP_TEST(decl_context.decl_flags, DF_ELABORATED_NAME))
        {
            result = filter_any_non_type(result);
        }

        // This object would hide any other in the enclosing scopes
        if (result != NULL)
            return result;
        
        // Do not look anything else
        if (BITMAP_TEST(decl_context.decl_flags, DF_ONLY_CURRENT_SCOPE))
            return NULL;

        // Otherwise, if this is a NAMESPACE_SCOPE, lookup in the used namespaces
        // note that the objects here are not hidden, but added
        if (current_scope->kind == NAMESPACE_SCOPE
                && current_scope->num_used_namespaces > 0)
        {
            result = name_lookup_used_namespaces(decl_context, current_scope, name);
            if (BITMAP_TEST(decl_context.decl_flags, DF_ELABORATED_NAME))
            {
                result = filter_any_non_type(result);
            }
            // If this yields any result, return them, no more search needed
            if (result != NULL)
                return result;
        }

        ERROR_CONDITION((result != NULL), "The symbols found here should be NULL!", 0);

        // If this is a CLASS_SCOPE, lookup in the base classes, similarly
        // to the used namespaces
        if (current_scope->kind == CLASS_SCOPE
                && (current_scope->num_base_scopes > 0))
        {
            result = name_lookup_bases(current_scope, name);
            if (BITMAP_TEST(decl_context.decl_flags, DF_ELABORATED_NAME))
            {
                result = filter_any_non_type(result);
            }
            // if this yields any result, return them, no more search needed
            if (result != NULL)
                return result;
        }

        ERROR_CONDITION((result != NULL), "The symbols found here should be NULL!", 0);

        current_scope = current_scope->contained_in;
    }

    return result;
}

// This function never instantiates a template, it might create a specialization though
static scope_entry_list_t* query_template_id(AST template_id, decl_context_t lookup_context, 
        decl_context_t nested_name_context, decl_flags_t decl_flags)
{
    AST symbol = ASTSon0(template_id);

    // Mix all flags in one variable
    decl_flags |= lookup_context.decl_flags;
    decl_flags |= nested_name_context.decl_flags;
    
    // First search the primary template and all its specializations
    DEBUG_CODE()
    {
        fprintf(stderr, "Trying to resolve template '%s'\n", ASTText(symbol));
    }

    scope_entry_list_t* entry_list; 
    if (BITMAP_TEST(decl_flags, DF_ONLY_CURRENT_SCOPE))
    {
        entry_list = query_in_scope_str(lookup_context, ASTText(symbol));
    }
    else
    {
        entry_list = query_unqualified_name_str(lookup_context, ASTText(symbol));
    }
    
    // Sometimes we end with the injected symbol, that is fairly useless here
    // so readjust the lookup_context and lookup again
    if (entry_list != NULL
            && entry_list->entry->injected_class_name)
    {
        scope_entry_t* injected_symbol = entry_list->entry;
        lookup_context = injected_symbol->injected_class_referred_symbol->decl_context;
        if (BITMAP_TEST(decl_flags, DF_ONLY_CURRENT_SCOPE))
        {
            entry_list = query_in_scope_str(lookup_context, ASTText(symbol));
        }
        else
        {
            entry_list = query_unqualified_name_str(lookup_context, ASTText(symbol));
        }
    }

    // Filter things that may be used as template-id
    enum cxx_symbol_kind filter_templates[5] = {
        SK_TEMPLATE_PRIMARY_CLASS, 
        SK_TEMPLATE_SPECIALIZED_CLASS,
        SK_TEMPLATE_FUNCTION,
        SK_TEMPLATE_TEMPLATE_PARAMETER,
        SK_TEMPLATE_ALIAS
    };

    entry_list = filter_symbol_kind_set(entry_list, 5, filter_templates);

    if (entry_list == NULL)
    {
        if (BITMAP_TEST(decl_flags, DF_NO_FAIL))
        {
            return NULL;
        }
        else
        {
            internal_error("Template '%s' not found! (%s)\n", 
                prettyprint_in_buffer(template_id),
                node_information(template_id));
        }
    }

    scope_entry_list_t* template_functions = filter_symbol_kind(entry_list, SK_TEMPLATE_FUNCTION);
    if (template_functions != NULL)
    {
        // This is naming a template function
        // Just return them, do not do anything else
        solve_possibly_ambiguous_template_id(template_id, nested_name_context);

        return template_functions;
    }

    // A template template parameter does not need anything else
    scope_entry_list_t* template_template_param = filter_symbol_kind(entry_list, SK_TEMPLATE_TEMPLATE_PARAMETER);
    if (template_template_param != NULL)
    {
        return template_template_param;
    }

    // Solve template_alias properly
    scope_entry_list_t* template_alias_list = filter_symbol_kind(entry_list, SK_TEMPLATE_ALIAS);
    if (template_alias_list != NULL)
    {
        scope_entry_t* template_alias = template_alias_list->entry;

        type_t* alias_type_info = template_alias->template_alias_type;

        ERROR_CONDITION(!is_named_type(alias_type_info),
                "A template alias should refer to a template-name", 0);
        scope_entry_t *referred_template = named_type_get_symbol(alias_type_info);

        DEBUG_CODE()
        {
            fprintf(stderr, "Symbol '%s' is a template alias that refers to '%s'\n", ASTText(symbol),
                    referred_template->symbol_name);
        }

        entry_list = query_in_scope_str(referred_template->decl_context, referred_template->symbol_name);
    }

    enum cxx_symbol_kind filter_template_classes[3] = {
        SK_TEMPLATE_PRIMARY_CLASS, 
        SK_TEMPLATE_SPECIALIZED_CLASS,
        SK_TEMPLATE_TEMPLATE_PARAMETER
    };

    entry_list = filter_symbol_kind_set(entry_list, 3, filter_template_classes);

    if (entry_list == NULL)
    {
        if (BITMAP_TEST(decl_flags, DF_NO_FAIL))
        {
            return NULL;
        }
        else
        {
            internal_error("template-id in '%s' not found\n", node_information(template_id));
        }
    }

    template_argument_list_t* current_template_arguments = NULL;
    // Note the scope being different here
    build_scope_template_arguments(lookup_context, nested_name_context,
            template_id,
            &current_template_arguments);

    // Now we check all the template arguments to see if any of them is
    // dependent
    int i;
    char seen_dependent_args = 0;
    for (i = 0; (i < current_template_arguments->num_arguments)
            && !seen_dependent_args; i++)
    {
        template_argument_t* argument = current_template_arguments->argument_list[i];
        if (argument->kind == TAK_TYPE)
        {
            if (is_dependent_type(argument->type, nested_name_context))
            {
                DEBUG_CODE()
                {
                    // fprintf(stderr, "-> Dependent type template argument '%s'\n",
                    //         prettyprint_in_buffer(argument->argument_tree));
                }
                seen_dependent_args = 1;
            }
        }
        else if (argument->kind == TAK_TEMPLATE)
        {
            // Fix this
        }
        else if (argument->kind == TAK_NONTYPE)
        {
            if (is_dependent_expression(argument->expression, argument->expression_context))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "-> Dependent expression template argument '%s'\n",
                            prettyprint_in_buffer(argument->expression));
                }
                seen_dependent_args = 1;
            }
        }
    }

    // If this is considered in the context of an expression and dependent args
    // have been seen, create a dependent entity
    if (BITMAP_TEST(decl_flags, DF_EXPRESSION) 
            && seen_dependent_args)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "This is a dependent template-id used in an expression\n");
        }

        scope_entry_t* dependent_entity = calloc(1, sizeof(*dependent_entity));
        dependent_entity->kind = SK_DEPENDENT_ENTITY;

        return create_list_from_entry(dependent_entity);
    }

    // If we have seen dependent arguments, we will not create any
    // specialization unless we are asked to
    if (BITMAP_TEST(decl_flags, DF_ALWAYS_CREATE_SPECIALIZATION))
    {
        // Forget we saw dependent arguments
        seen_dependent_args = 0;
    }
    
    DEBUG_CODE()
    {
        fprintf(stderr, "-> Looking for exact match templates\n");
    }

    matching_pair_t* matched_template = solve_template(nested_name_context, 
            entry_list, current_template_arguments, /* exact = */ 1);
    if (matched_template != NULL)
    {
        // There is an exact match
        scope_entry_t* matched_entry = matched_template->entry;
        DEBUG_CODE()
        {
            fprintf(stderr, "-> Just returning the exact matching template %p in line %d\n", 
                    matched_entry, matched_entry->line);
        }
        return create_list_from_entry(matched_entry);
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "-> No exact match found\n");
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "-> Looking for an unificable template\n");
    }

    // Look for an unificable template
    matched_template = solve_template(nested_name_context, entry_list, current_template_arguments, /* exact= */ 0);

    if (matched_template != NULL)
    {
        // If it is a nested class, check it is actually dependent
        if (matched_template->entry->is_member)
        {
            seen_dependent_args |= is_dependent_type(matched_template->entry->class_type, lookup_context);
        }
        // If the template-id is independent or it was dependent but we have been
        // asked to create a specialization, create it
        if (!seen_dependent_args
                || BITMAP_TEST(decl_flags, DF_ALWAYS_CREATE_SPECIALIZATION))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "-> Creating an incomplete specialization\n");
            }
            scope_entry_t* holding_symbol = create_holding_symbol_for_template(matched_template,
                    current_template_arguments,
                    ASTLine(template_id), ASTFileName(template_id),
                    nested_name_context);
            return create_list_from_entry(holding_symbol);

        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "-> Returning the unificable template %p in line %d. No specialization has been created\n",
                        matched_template, 
                        matched_template->entry->line);
            }
            return create_list_from_entry(matched_template->entry);
        }
    }
    else
    {
        // There is always a template that matches, the primary one
        internal_error("No template found that matches '%s' in %s (but primary template always matches)", 
                prettyprint_in_buffer(template_id), node_information(template_id));
        return NULL;
    }
}

static scope_entry_list_t *copy_entry_list(scope_entry_list_t* orig)
{
    scope_entry_list_t* result = NULL;
    if (orig == NULL)
        return result;

    result = calloc(1, sizeof(*result));

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
static char* get_fully_qualified_symbol_name_simple(decl_context_t decl_context, char* current_qualif_name)
{
    // DEBUG_CODE()
    // {
    //     fprintf(stderr, "Getting qualification via scope current='%s'\n", current_qualif_name);
    // }
    char* result = current_qualif_name;

    scope_t* current_scope = decl_context.current_scope;

    while (current_scope != NULL)
    {
        if (current_scope->qualification_name != NULL)
        {
            char* nested_name = strappend(current_scope->qualification_name, "::");
            result = strappend(nested_name, result);
        }

        current_scope = current_scope->contained_in;
    }

    // DEBUG_CODE()
    // {
    //     fprintf(stderr, "Fully qualified name simple '%s'\n", result);
    // }

    return result;
}

static scope_entry_t* find_template_parameter(scope_t* st, scope_entry_t* template_param, decl_context_t decl_context)
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
                    || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER))
        {
            type_t* type_current = entry->type_information;
            type_t* type_param = template_param->type_information;


            if (equivalent_types(type_current, type_param, CVE_CONSIDER, decl_context))
            {
                return entry;
            }
        }
    }

    return NULL;
}

static char* give_name_for_template_parameter(scope_entry_t* entry, decl_context_t decl_context)
{
    char found = 0;

    scope_t* st = decl_context.template_scope;
    while (st != NULL)
    {
        scope_entry_t* template_parameter = find_template_parameter(st, entry, decl_context);
        if (template_parameter != NULL)
        {
            return template_parameter->symbol_name;
        }

        st = st->contained_in;
    }

    if (!found)
    {
        internal_error("Template parameter '%s' not found in scope\n", entry->symbol_name);
    }

    return NULL;
}

char* get_unqualified_template_symbol_name(scope_entry_t* entry, decl_context_t decl_context)
{
    char* result = "";

    // It is not enough with the name, we have to print the arguments
    result = strappend(result, "<");
    template_argument_list_t* template_arguments = template_type_get_template_arguments(entry->type_information);

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
                    char* abstract_declaration;

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

    result = strappend(result, ">");

    return result;
}

// Get the fully qualified symbol name in the scope of the ocurrence
char* get_fully_qualified_symbol_name(scope_entry_t* entry, decl_context_t decl_context, char* is_dependent, int* max_qualif_level)
{
    // DEBUG_CODE()
    // {
    //     fprintf(stderr, "Getting fully qualified symbol name for '%s'\n", entry->symbol_name);
    // }

    // If this is the injected symbol, ignore it and get the real entry
    if (entry->injected_class_name)
    {
        entry = entry->injected_class_referred_symbol;
    }

    char* result = strdup(entry->symbol_name);

    if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        // This symbol must be looked up for the proper real name
        result = give_name_for_template_parameter(entry, decl_context);

        (*is_dependent) |= is_dependent_type(entry->type_information, decl_context);
        return result;
    }

    if (entry->kind == SK_TEMPLATE_PRIMARY_CLASS
            || entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
    {
        result = strappend(result, get_unqualified_template_symbol_name(entry, decl_context));
        if (entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
        {
            // They are always dependent
            (*is_dependent) = 1;
        }
        else // if (entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
        {
            // They are only dependent if it is so in the template
            if (class_type_is_incomplete_dependent(entry->type_information)
                    || class_type_is_complete_dependent(entry->type_information))
            {
                (*is_dependent) = 1;
            }
        }
    }

    if (entry->is_member)
    {
        // DEBUG_CODE()
        // {
        //     fprintf(stderr, "The symbol is a member, getting the qualified symbol name of the enclosing class\n");
        // }
        // We need the qualification of the class
        if (is_named_class_type(entry->class_type))
        {
            scope_entry_t* class_symbol = named_type_get_symbol(entry->class_type);

            (*max_qualif_level)++;

            char* class_qualification = get_fully_qualified_symbol_name(class_symbol, decl_context, is_dependent, max_qualif_level);

            // DEBUG_CODE()
            // {
            //     fprintf(stderr, "The qualified name of the enclosing class of '%s' is '%s'\n", result, class_qualification);
            // }

            class_qualification = strappend(class_qualification, "::");

            result = strappend(class_qualification, result);
        }
    } 
    else if (!entry->is_member)
    {
        // This symbol is already simple enough
        result = get_fully_qualified_symbol_name_simple(entry->decl_context, result);
    }

    // DEBUG_CODE()
    // {
    //     fprintf(stderr, "Fully qualified name is '%s'\n", result);
    // }

    return result;
}

