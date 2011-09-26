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
#include "cxx-printscope.h"
#include "cxx-codegen.h"
#include "cxx-entrylist.h"
#include "cxx-diagnostic.h"
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
static scope_entry_list_t* name_lookup(decl_context_t decl_context, 
        const char* name, 
        decl_flags_t decl_flags,
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
        result_set = entry_list_prepend(result_set, entry);
    }
    else
    {
        result_set = entry_list_new(entry);
    }

    rb_tree_insert(sc->hash, symbol_name, result_set);
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
            result_set = entry_list_prepend(result_set, entry);
            rb_tree_insert(sc->hash, entry->symbol_name, result_set);
        }
    }
    else
    {
        result_set = entry_list_new(entry);
        rb_tree_insert(sc->hash, entry->symbol_name, result_set);
    }
}

void remove_entry(scope_t* sc, scope_entry_t* entry)
{
    rb_red_blk_node* n = rb_tree_query(sc->hash, entry->symbol_name);
    if (n == NULL)
        return;

    scope_entry_list_t* entry_list = (scope_entry_list_t*)rb_node_get_info(n);
    entry_list_remove(entry_list, entry);
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

scope_entry_list_t* filter_symbol_using_predicate(scope_entry_list_t* entry_list, char (*f)(scope_entry_t*, void*), void* p)
{
    scope_entry_list_t* result = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (f(entry, p))
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
char is_friend_declared(scope_entry_t* entry)
{
    char is_friend = 0;
    if ((entry->kind == SK_CLASS
            || entry->kind == SK_FUNCTION)
            && is_template_specialized_type(entry->type_information))
    {
        type_t* template_type = template_specialized_type_get_related_template_type(entry->type_information);
        scope_entry_t* template_sym = template_type_get_related_symbol(template_type);
        is_friend = template_sym->entity_specs.is_friend_declared;
    }
    else
    {
        is_friend =  entry->entity_specs.is_friend_declared;
    }

    return is_friend;
}

static char is_not_friend_declared(scope_entry_t* entry, void* p UNUSED_PARAMETER)
{
    return !is_friend_declared(entry);
}

scope_entry_list_t* filter_friend_declared(scope_entry_list_t* entry_list)
{
    return filter_symbol_using_predicate(entry_list, is_not_friend_declared, NULL);
}

static void compute_nodecl_name_from_unqualified_id(AST unqualified_id, decl_context_t decl_context, nodecl_t* nodecl_output);

scope_entry_list_t* query_id_expression_flags(decl_context_t decl_context,
        AST id_expression, decl_flags_t decl_flags)
{
    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_id_expression(id_expression, decl_context, &nodecl_name);

    if (nodecl_is_err_expr(nodecl_name))
        return NULL;

    scope_entry_list_t* result = query_nodecl_name_flags(decl_context, nodecl_name, decl_flags);

    nodecl_free(nodecl_name);

    return result;
}

/*
 * Query functions
 */
scope_entry_list_t* query_nested_name_flags(decl_context_t decl_context, 
        AST global_op, 
        AST nested_name, 
        AST unqualified_name,
        decl_flags_t decl_flags)
{
    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_qualified_name(global_op, nested_name, unqualified_name, decl_context, &nodecl_name);

    if (nodecl_is_err_expr(nodecl_name))
        return NULL;

    scope_entry_list_t* result = query_nodecl_name_flags(decl_context, nodecl_name, decl_flags);

    nodecl_free(nodecl_name);

    return result;
}

scope_entry_list_t* query_in_scope_flags(decl_context_t decl_context,
        AST unqualified_name, decl_flags_t decl_flags)
{
    decl_flags |= DF_ONLY_CURRENT_SCOPE;

    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_unqualified_id(unqualified_name, decl_context, &nodecl_name);

    if (nodecl_is_err_expr(nodecl_name))
        return NULL;

    scope_entry_list_t* result = query_nodecl_name_flags(decl_context, nodecl_name, decl_flags);

    nodecl_free(nodecl_name);

    return result;
}

scope_entry_list_t* query_name_str_flags(decl_context_t decl_context,
        const char* unqualified_name, decl_flags_t decl_flags)
{
    nodecl_t nodecl_name = nodecl_make_cxx_dep_name_simple(unqualified_name, NULL, 0);

    scope_entry_list_t* result = query_nodecl_name_flags(decl_context, nodecl_name, decl_flags);

    nodecl_free(nodecl_name);

    return result;
}

scope_entry_list_t* query_in_scope_str_flags(decl_context_t decl_context,
        const char* name, decl_flags_t decl_flags)
{
    return query_name_str_flags(decl_context, name, decl_flags | DF_ONLY_CURRENT_SCOPE);
}


static scope_entry_list_t* qualified_query_in_namespace(scope_entry_t* namespace, 
        const char* name, decl_flags_t decl_flags,
        const char* filename, int line);
static scope_entry_list_t* query_in_class(scope_t* current_class_scope, 
        const char* name, decl_flags_t decl_flags,
        const char* filename, int line);

static void build_dependent_parts_for_symbol_rec(
        scope_entry_t* entry,
        const char* filename, 
        int line,
        scope_entry_t** dependent_entry,
        nodecl_t* nodecl_output);

static scope_entry_t* create_new_dependent_entity(
        decl_context_t decl_context,
        scope_entry_t* dependent_entry,
        int nested_name_index,
        int nested_name_size,
        const char* filename, 
        int line,
        nodecl_t* parts)
{
    nodecl_t nodecl_list = nodecl_null();
    scope_entry_t* updated_dependent_entry = NULL;

    // We may have to extend the nodecl name here
    if (dependent_entry->kind == SK_TYPEDEF
            && is_class_type(dependent_entry->type_information))
    {
        dependent_entry = named_type_get_symbol(advance_over_typedefs(dependent_entry->type_information));
    }
    if (dependent_entry->kind == SK_CLASS)
    {
        build_dependent_parts_for_symbol_rec(dependent_entry,
                filename, line, &updated_dependent_entry, &nodecl_list);
    }

    if (updated_dependent_entry == NULL)
        updated_dependent_entry = dependent_entry;

    int i;
    for (i = nested_name_index + 1; i < nested_name_size; i++)
    {
        nodecl_list = nodecl_append_to_list(nodecl_list, parts[i]);
    }

    nodecl_t nodecl_parts = nodecl_make_cxx_dep_name_nested(nodecl_list, NULL, 0);

    // FIXME - Cache these symbols
    scope_entry_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);

    result->kind = SK_DEPENDENT_ENTITY;
    result->decl_context = decl_context;
    result->symbol_name = dependent_entry->symbol_name;
    result->type_information = get_dependent_typename_type_from_parts(updated_dependent_entry, nodecl_parts);

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

static char can_be_inherited(scope_entry_t* entry, void* p UNUSED_PARAMETER)
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

#if 0
static char is_injected_class_name(scope_entry_t* entry, void* p UNUSED_PARAMETER)
{
    ERROR_CONDITION(entry == NULL, "Error, entry can't be null", 0);

    if (entry->entity_specs.is_injected_class_name)
    {
        return 0;
    }

    return 1;
}
#endif

static scope_entry_list_t* filter_not_inherited_entities(scope_entry_list_t* list)
{
    return filter_symbol_using_predicate(list, can_be_inherited, NULL);
}

#if 0
static scope_entry_list_t* filter_injected_class_name(scope_entry_list_t* list)
{
    return filter_symbol_using_predicate(list, is_injected_class_name, NULL);
}
#endif

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
        for (i = 0; i < derived->path_length - 1; i++)
        {
            fprintf(stderr, "%s%s", 
                    (i > 0) ? "::" : "",
                    class_type_get_inner_context(derived->path[i]).current_scope->related_entry->symbol_name);
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

nodecl_t nodecl_name_get_last_part(nodecl_t nodecl_name)
{
    if (nodecl_get_kind(nodecl_name) == NODECL_CXX_DEP_GLOBAL_NAME_NESTED
            || nodecl_get_kind(nodecl_name) == NODECL_CXX_DEP_NAME_NESTED)
    {
        int num_items = 0;
        nodecl_t* list = nodecl_unpack_list(nodecl_get_child(nodecl_name, 0), &num_items);

        nodecl_t last_part = list[num_items - 1];

        free(list);

        return last_part;
    }

    return nodecl_name;
}

char nodecl_name_ends_in_template_id(nodecl_t nodecl_name)
{
    return (nodecl_get_kind(nodecl_name_get_last_part(nodecl_name)) == NODECL_CXX_DEP_TEMPLATE_ID);
}

template_parameter_list_t* nodecl_name_name_last_template_arguments(nodecl_t nodecl_name)
{
    if (nodecl_name_ends_in_template_id(nodecl_name))
    {
        return nodecl_get_template_parameters(nodecl_name_get_last_part(nodecl_name));
    }
    else
    {
        return NULL;
    }
}

static void build_dependent_parts_for_symbol_rec(
        scope_entry_t* entry,
        const char* filename, 
        int line,
        scope_entry_t** dependent_entry,
        nodecl_t* nodecl_output)
{
    ERROR_CONDITION(entry->kind != SK_CLASS, "Invalid symbol", 0);
    type_t* enclosing = class_type_get_enclosing_class_type(entry->type_information);

    if (enclosing != NULL
            && is_dependent_type(enclosing))
    {
        nodecl_t nodecl_prev = nodecl_null();

        build_dependent_parts_for_symbol_rec(named_type_get_symbol(enclosing), filename, line, dependent_entry, &nodecl_prev);

        template_parameter_list_t* template_arguments = NULL;
        if (is_template_specialized_type(entry->type_information))
        {
            template_arguments = template_specialized_type_get_template_arguments(entry->type_information);
        }

        nodecl_t nodecl_current = nodecl_make_cxx_dep_name_simple(entry->symbol_name, filename, line);
        if (template_arguments != NULL)
        {
            nodecl_current = nodecl_make_cxx_dep_template_id(nodecl_current, template_arguments, filename, line);
        }

        if (nodecl_is_null(nodecl_prev))
        {
            *nodecl_output = nodecl_make_list_1(nodecl_current);
        }
        else
        {
            *nodecl_output = nodecl_concat_lists(nodecl_prev, nodecl_make_list_1(nodecl_current));
        }
    }
    else
    {
        *dependent_entry = entry;
        *nodecl_output = nodecl_null();
    }
}

type_t* build_dependent_typename_for_entry(
        scope_entry_t* class_symbol,
        nodecl_t nodecl_name,
        const char* filename,
        int line)
{
    nodecl_t nodecl_prev = nodecl_null();
    scope_entry_t* dependent_entry = NULL;
    build_dependent_parts_for_symbol_rec(class_symbol,
            filename, line, &dependent_entry, &nodecl_prev);

    nodecl_t nodecl_last = nodecl_name_get_last_part(nodecl_name);

    template_parameter_list_t* template_arguments = NULL;

    nodecl_t nodecl_current = nodecl_null(); 

    if (nodecl_get_kind(nodecl_last) == NODECL_CXX_DEP_TEMPLATE_ID)
    {
        template_arguments = nodecl_get_template_parameters(nodecl_name);
        nodecl_current = nodecl_copy(nodecl_get_child(nodecl_name, 0));
    }
    else
    {
        nodecl_current = nodecl_copy(nodecl_name);
    }

    if (template_arguments != NULL)
    {
        nodecl_current = nodecl_make_cxx_dep_template_id(nodecl_current, template_arguments, filename, line);
    }

    nodecl_t dependent_parts = nodecl_null();
    if (nodecl_is_null(nodecl_prev))
    {
        dependent_parts = nodecl_make_list_1(nodecl_current);
    }
    else
    {
        dependent_parts = nodecl_append_to_list(nodecl_prev, nodecl_current);
    }
    dependent_parts = nodecl_make_cxx_dep_name_nested(dependent_parts, filename, line);

    type_t* result = get_dependent_typename_type_from_parts(dependent_entry, dependent_parts);

    return result;
}

static scope_entry_list_t* query_in_class(scope_t* current_class_scope, 
        const char* name, 
        decl_flags_t decl_flags,
        const char* filename, 
        int line)
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
        scope_entry_t* entry = entry_advance_aliases(entry_list_iterator_current(it));

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
                && (hiding_name == NULL
                    || (hiding_name->decl_context.current_scope == entry->decl_context.current_scope)))
        {
        }
        else
        {
            error_ambiguity(entry_list, filename, line);
        }
    }
    entry_list_iterator_free(it);
}

static scope_entry_list_t* entry_list_merge_aliases(scope_entry_list_t* list1, scope_entry_list_t* list2)
{
    scope_entry_list_t* result = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(list1);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_advance_aliases(entry_list_iterator_current(it));
        result = entry_list_add_once(result, entry);
    }
    entry_list_iterator_free(it);

    for (it = entry_list_iterator_begin(list2);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_advance_aliases(entry_list_iterator_current(it));
        result = entry_list_add_once(result, entry);
    }
    entry_list_iterator_free(it);

    return result;
}

struct associated_namespace_tag
{
    scope_t* scope_of_using; // Scope where the using directive was found
    scope_entry_t* nominated; // Scope of the nominated namespace
    char visited; 
};

typedef struct associated_namespace_tag associated_namespace_t;

char scope_is_enclosed_by(scope_t* scope, scope_t* potential_enclosing)
{
    while (scope != NULL)
    {
        if (scope == potential_enclosing)
        {
            return 1;
        }
        scope = scope->contained_in;
    }
    return 0;
}

static scope_entry_list_t* unqualified_query_in_namespace(
        scope_entry_t* namespace,
        const char* name,
        int num_associated_namespaces, 
        associated_namespace_t* associated_namespaces,
        const char* filename, int line)
{
    scope_t* current_scope = namespace->related_decl_context.current_scope;
    scope_entry_list_t* grand_result 
        = query_name_in_scope(current_scope, name);

    int i;
    for (i = 0; i < num_associated_namespaces; i++)
    {
        if (associated_namespaces[i].visited)
            continue;

        scope_t* scope_of_nominated = associated_namespaces[i].nominated->decl_context.current_scope;

        if (scope_is_enclosed_by(scope_of_nominated, current_scope)
                && scope_is_enclosed_by(associated_namespaces[i].scope_of_using, current_scope))
        {
            scope_t* associated_namespace = associated_namespaces[i].nominated->related_decl_context.current_scope;

            scope_entry_list_t* result = query_name_in_scope(associated_namespace, name);
            associated_namespaces[i].visited = 1;

            scope_entry_list_t* old_grand_result = grand_result;
            grand_result = entry_list_merge_aliases(old_grand_result, result);
            entry_list_free(old_grand_result);
            entry_list_free(result);

            check_for_naming_ambiguity(grand_result, filename, line);
        }
    }

    return grand_result;
}

static scope_entry_list_t* qualified_query_in_namespace_rec(scope_entry_t* namespace, 
        const char* name, 
        decl_flags_t decl_flags,
        const char* filename, int line,
        int num_visited_namespaces,
        scope_entry_t** visited_namespaces)
{
    ERROR_CONDITION(namespace->kind != SK_NAMESPACE, "Invalid symbol", 0);
    scope_t* namespace_scope = namespace->related_decl_context.current_scope;
    scope_entry_list_t* grand_result = query_name_in_scope(namespace_scope, name);

    if (grand_result != NULL)
        return grand_result;

    ERROR_CONDITION(num_visited_namespaces == MCXX_MAX_ASSOCIATED_NAMESPACES, 
            "Too many associated namespaces %d", num_visited_namespaces);
    visited_namespaces[num_visited_namespaces] = namespace;
    num_visited_namespaces++;

    int i;
    for (i = 0; 
            i < namespace_scope->num_used_namespaces;
            i++)
    {
        scope_entry_t* used_namespace = namespace_scope->use_namespace[i];

        char found = 0;
        int j;
        for (j = 0; j < num_visited_namespaces && !found; j++)
        {
            if (visited_namespaces[i] == used_namespace)
            {
                found = 1;
            }
        }

        if (found)
            continue;

        scope_entry_list_t* result = qualified_query_in_namespace_rec(used_namespace, 
                name,
                decl_flags,
                filename, line,
                num_visited_namespaces,
                visited_namespaces);

        scope_entry_list_t* old_grand_result = grand_result;
        grand_result = entry_list_merge_aliases(old_grand_result, result);
        entry_list_free(old_grand_result);
        entry_list_free(result);

        check_for_naming_ambiguity(grand_result, filename, line);
    }

    return grand_result;
}

static scope_entry_list_t* qualified_query_in_namespace(scope_entry_t* namespace, 
        const char* name, 
        decl_flags_t decl_flags,
        const char* filename, int line)
{
    scope_entry_t* visited_namespaces[MCXX_MAX_ASSOCIATED_NAMESPACES];

    scope_entry_list_t* result = qualified_query_in_namespace_rec(namespace, name, decl_flags, 
            filename, line, 
            0, visited_namespaces);

    return result;
}

static void transitive_add_using_namespaces(decl_flags_t decl_flags, 
        scope_t* scope_of_using,
        scope_t* current_scope,
        int *num_associated_namespaces,
        associated_namespace_t associated_namespaces[MCXX_MAX_ASSOCIATED_NAMESPACES])
{
    int i;
    for (i = 0; i < current_scope->num_used_namespaces; i++)
    {
        int j;
        char found = 0;

        // Inlines are the only ones considered when doing a "only current
        // scope" lookup
        if (BITMAP_TEST(decl_flags, DF_ONLY_CURRENT_SCOPE)
                && !current_scope->use_namespace[i]->entity_specs.is_inline)
            continue;

        for (j = 0; j < (*num_associated_namespaces) && !found; j++)
        {
            found = (associated_namespaces[j].nominated == current_scope->use_namespace[i]);
        }
        if (!found)
        {
            if ((*num_associated_namespaces) == MCXX_MAX_ASSOCIATED_NAMESPACES)
                running_error("Too many associated scopes > %d", MCXX_MAX_ASSOCIATED_NAMESPACES);
            associated_namespaces[(*num_associated_namespaces)].scope_of_using = scope_of_using;
            associated_namespaces[(*num_associated_namespaces)].nominated = current_scope->use_namespace[i];
            associated_namespaces[(*num_associated_namespaces)].visited = 0;
            (*num_associated_namespaces)++;

            transitive_add_using_namespaces(decl_flags, 
                    scope_of_using,
                    current_scope->use_namespace[i]->related_decl_context.current_scope, 
                    num_associated_namespaces, associated_namespaces);
        }
    }
}

static scope_entry_list_t* name_lookup(decl_context_t decl_context, 
        const char* name, 
        decl_flags_t decl_flags,
        const char* filename, int line)
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
    associated_namespace_t associated_namespaces[MCXX_MAX_ASSOCIATED_NAMESPACES];
    memset(associated_namespaces, 0, sizeof(associated_namespaces));

    scope_t* current_scope = decl_context.current_scope;

    while (result == NULL
            && current_scope != NULL)
    {
        transitive_add_using_namespaces(decl_flags, 
                current_scope, 
                current_scope,
                &num_associated_namespaces, 
                associated_namespaces);

        if (current_scope->kind == CLASS_SCOPE)
        {
            if (!BITMAP_TEST(decl_flags, DF_ONLY_CURRENT_SCOPE))
            {
                result = query_in_class(current_scope, name, decl_flags, 
                        filename, line);
            }
            else
            {
                result = query_name_in_scope(current_scope, name);
            }
        }
        else if (current_scope->kind == NAMESPACE_SCOPE)
        {
            result = unqualified_query_in_namespace(
                    current_scope->related_entry,
                    name, num_associated_namespaces,
                    associated_namespaces, 
                    filename, line);
        }
        else // BLOCK_SCOPE || PROTOTYPE_SCOPE || FUNCTION_SCOPE (although its contains should be NULL)
        {
            result = query_name_in_scope(current_scope, name);
        }

        if (BITMAP_TEST(decl_flags, DF_ELABORATED_NAME))
        {
            scope_entry_list_t* old_result = result;
            result = filter_any_non_type(old_result);
            entry_list_free(old_result);
        }

        if (BITMAP_TEST(decl_flags, DF_ONLY_CURRENT_SCOPE))
        {
            return result;
        }

        current_scope = current_scope->contained_in;
    }

    return result;
}

static nodecl_t update_nodecl_template_argument_expression(nodecl_t nodecl, 
		decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Updating expression '%s'\n", 
                c_cxx_codegen_to_str(nodecl));
    }

    nodecl_t nodecl_output = nodecl_null();

    nodecl_t nodecl_inst = instantiate_expression(nodecl, decl_context);
    check_nodecl_nontype_template_argument_expression(nodecl_inst, 
            decl_context, 
            &nodecl_output);

    return nodecl_output;
}

static nodecl_t update_nodecl_constant_expression(nodecl_t nodecl, 
		decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Updating expression '%s'\n", 
                c_cxx_codegen_to_str(nodecl));
    }
    nodecl = instantiate_expression(nodecl, decl_context);

    if (!nodecl_is_constant(nodecl)
            && !nodecl_expr_is_value_dependent(nodecl))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: expression '%s' is not constant\n",
                    nodecl_get_locus(nodecl),
                    c_cxx_codegen_to_str(nodecl));
        }
    }

    return nodecl;
}

static template_parameter_value_t* update_template_parameter_value_aux(
        template_parameter_value_t* v,
        decl_context_t decl_context,
        char is_template_class,
        const char* filename, int line)
{
    template_parameter_value_t* result = counted_calloc(1, sizeof(*result), &_bytes_used_scopes);

    *result = *v;
    result->is_default = 0;

    result->type = update_type(result->type, decl_context, filename, line);
    
    if (is_template_class)
    {
        result->type = advance_over_typedefs(result->type);
    }

    if (result->kind == TPK_NONTYPE)
    {
        if (!nodecl_is_null(result->value))
        {
            result->value = update_nodecl_template_argument_expression(result->value, decl_context);

            if (nodecl_is_err_expr(result->value))
            {
                return NULL;
            }

            // Force the type of the expression
            nodecl_set_type(result->value, result->type);
        }
    }

    return result;
}

template_parameter_value_t* update_template_parameter_value_of_template_class(
        template_parameter_value_t* v,
        decl_context_t decl_context,
        const char* filename, int line)
{
    return update_template_parameter_value_aux(v, decl_context, /* is_template_class */ 1, filename, line);
}

template_parameter_value_t* update_template_parameter_value(
        template_parameter_value_t* v,
        decl_context_t decl_context,
        const char* filename, int line)
{
    return update_template_parameter_value_aux(v, decl_context, /* is_template_class */ 0, filename, line);
}

static template_parameter_list_t* update_template_argument_list_in_dependent_typename(
        decl_context_t decl_context,
        template_parameter_list_t* dependent_type_template_arguments,
        const char* filename, 
        int line)
{
    template_parameter_list_t* result = duplicate_template_argument_list(dependent_type_template_arguments);

    int i;
    for (i = 0; i < result->num_parameters; i++)
    {
        result->arguments[i] = update_template_parameter_value(
                result->arguments[i],
                decl_context,
                filename, line);

        if (result->arguments[i] == NULL)
            return NULL;
    }

    return result;
}

static type_t* update_dependent_typename(
        type_t* dependent_entry_type,
        nodecl_t dependent_parts,
        decl_context_t decl_context,
        const char* filename,
        int line)
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

    if(nodecl_is_null(dependent_parts))
    {
        return get_user_defined_type(dependent_entry);
    }

    scope_entry_t* current_member = dependent_entry;

    if (class_type_is_incomplete_independent(current_member->type_information))
    {
        instantiate_template_class(current_member, current_member->decl_context,
                filename, line);
    }

    // We need to update dependent parts, lest there was a template-id
    int num_parts = 0;
    int i;
    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(dependent_parts, 0), &num_parts);
    nodecl_t new_dependent_parts_list = nodecl_null();
    for (i = 0; i < num_parts; i++)
    {
        nodecl_t new_current_part = nodecl_copy(list[i]);

        if (nodecl_get_kind(new_current_part) == NODECL_CXX_DEP_TEMPLATE_ID)
        {
            template_parameter_list_t* template_arguments 
                = nodecl_get_template_parameters(new_current_part);
            template_parameter_list_t* new_template_arguments 
                = update_template_argument_list_in_dependent_typename(decl_context, 
                        template_arguments, 
                        filename, line);

            if (new_template_arguments == NULL)
                return NULL;

            nodecl_set_template_parameters(new_current_part, new_template_arguments);
        }

        new_dependent_parts_list = nodecl_append_to_list(new_dependent_parts_list,
                new_current_part);
    }
    nodecl_t new_dependent_parts = nodecl_make_cxx_dep_name_nested(new_dependent_parts_list, filename, line);

    scope_entry_list_t* entry_list = query_nodecl_name(
            class_type_get_inner_context(current_member->type_information),
            new_dependent_parts);

    if (entry_list == NULL)
        return NULL;

    scope_entry_t* member = entry_list_head(entry_list);
    entry_list_free(entry_list);

    if (member->kind == SK_CLASS
            || member->kind == SK_TYPEDEF
            || member->kind == SK_ENUM)
    {
        return get_user_defined_type(member);
    }
    else if (member->kind == SK_DEPENDENT_ENTITY)
    {
        return member->type_information;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: After updating dependent typename the obtained symbol has kind '%s' which is not a valid type\n",
                    symbol_kind_name(member));
        }
        return NULL;
    }
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
            // template_parameter_list_t* primary_template_parameters = template_type_get_template_parameters(template_type);
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
                template_parameter_value_t* updated_argument = update_template_parameter_value_of_template_class(
                        template_parameters->arguments[i],
                        decl_context, 
                        filename, line);

                if (updated_argument == NULL)
                    return NULL;

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
                        filename, line);
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
                        decl_context, 
                        filename, line),
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

        type_t* updated_referenced = update_type_aux_(referenced, decl_context, 
                filename, line);

        if (updated_referenced == NULL)
            return NULL;

        type_t* result_type = get_lvalue_reference_type(updated_referenced);

        return result_type;
    }
    else if (is_pointer_type(orig_type))
    {
        cv_qualifier_t cv_qualifier = get_cv_qualifier(orig_type);

        type_t* pointee = pointer_type_get_pointee_type(orig_type);

        type_t* updated_pointee = update_type_aux_(pointee, decl_context, 
                filename, line);

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
        type_t* updated_pointee = update_type_aux_(pointee, decl_context, 
                filename, line);

        if (updated_pointee == NULL)
            return NULL;

        type_t* pointee_class = pointer_to_member_type_get_class_type(orig_type);
        pointee_class = update_type_aux_(pointee_class, decl_context, 
                filename, line);

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
            return_type = update_type_aux_(return_type, decl_context, 
                    filename, line);
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

            param_orig_type = update_type_aux_(param_orig_type, decl_context, 
                    filename, line);

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

        // Context of the array
        decl_context_t array_size_context = array_type_get_array_size_expr_context(orig_type);

        if (!nodecl_is_null(array_size))
        {
            array_size = update_nodecl_constant_expression(array_size, decl_context);

            if (nodecl_get_kind(array_size) == NODECL_ERR_EXPR)
            {
                running_error("%s: error: could not update array dimension",
                        nodecl_get_locus(array_size));
            }

            if (nodecl_expr_is_value_dependent(array_size))
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

        type_t* element_type = array_type_get_element_type(orig_type);
        element_type = update_type_aux_(element_type, decl_context, 
                filename, line);

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
        cv_qualifier_t cv_qualif = get_cv_qualifier(orig_type);

        scope_entry_t* dependent_entry = NULL;
        nodecl_t dependent_parts = nodecl_null();

        dependent_typename_get_components(orig_type, 
                &dependent_entry, &dependent_parts);

        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Updating typename '%s'\n",
                    print_declarator(orig_type));
        }

        type_t* fixed_type = NULL;
        fixed_type = update_type_aux_(get_user_defined_type(dependent_entry),
                decl_context,
                filename, line);

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
            nodecl_t fix_dependent_parts = nodecl_null();
            dependent_typename_get_components(fixed_type, 
                    &fix_dependent_entry, &fix_dependent_parts);

            // Now append the dependent parts of this type 
            nodecl_t appended_dependent_parts = nodecl_null();

            int num_items = 0;
            nodecl_t* list = NULL;
            int i;

            list = nodecl_unpack_list(fix_dependent_parts, &num_items);
            for (i = 0; i < num_items; i++)
            {
                appended_dependent_parts = nodecl_append_to_list(appended_dependent_parts, list[i]);
            }
            free(list);

            list = nodecl_unpack_list(dependent_parts, &num_items);
            for (i = 0; i < num_items; i++)
            {
                appended_dependent_parts = nodecl_append_to_list(appended_dependent_parts, list[i]);
            }
            free(list);

            cv_qualif |= cv_qualif_dep;

            fixed_type = get_user_defined_type(fix_dependent_entry);
            dependent_parts = appended_dependent_parts;
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
    else if (is_gcc_typeof_expr(orig_type))
    {
        nodecl_t nodecl_expr = gcc_typeof_expr_type_get_expression(orig_type);

        nodecl_t nodecl_new_expr = instantiate_expression(nodecl_expr, decl_context);

        if (nodecl_is_err_expr(nodecl_new_expr))
        {
            return NULL;
        }
        else if (nodecl_expr_is_type_dependent(nodecl_new_expr))
        {
            return orig_type;
        }
        else
        {
            return nodecl_get_type(nodecl_new_expr);
        }
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

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Type '%s' has been updated to '%s'\n", print_declarator(orig_type), print_declarator(result));
    }

    return result;
}

static template_parameter_value_t* get_single_template_argument_from_syntax(AST template_parameter, 
        decl_context_t template_parameters_context, int position)
{

    switch (ASTType(template_parameter))
    {
        case AST_TEMPLATE_EXPRESSION_ARGUMENT :
            {
                template_parameter_value_t* t_argument = 
                    counted_calloc(1, sizeof(*t_argument), &_bytes_used_scopes);

                AST expr = ASTSon0(template_parameter);

                nodecl_t nodecl_expr = nodecl_null();
                check_nontype_template_argument_expression(expr, template_parameters_context, &nodecl_expr);

                if (nodecl_is_err_expr(nodecl_expr))
                {
                    if (!checking_ambiguity())
                    {
                        error_printf("%s: error: invalid template-argument number %d\n",
                                ast_location(template_parameter),
                                position);
                    }
                    return NULL;
                }

                t_argument->value = nodecl_expr;

                t_argument->type = nodecl_get_type(nodecl_expr);
                t_argument->kind = TPK_NONTYPE;

                return t_argument;
                break;
            }
        case AST_TEMPLATE_TYPE_ARGUMENT :
            {
                template_parameter_value_t* t_argument = 
                    counted_calloc(1, sizeof(*t_argument), &_bytes_used_scopes);

                AST type_template_parameter = ASTSon0(template_parameter);
                AST type_specifier_seq = ASTSon0(type_template_parameter);
                AST abstract_decl = ASTSon1(type_template_parameter);

                // A type_specifier_seq is essentially a subset of a
                // declarator_specifier_seq so we can reuse existing functions
                type_t* type_info;
                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));
                gather_info.allow_class_template_names = 1;

                nodecl_t dummy_nodecl_output = nodecl_null();
                build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info,
                        template_parameters_context, 
                        &dummy_nodecl_output);

                if (is_error_type(type_info))
                {
                    if (!checking_ambiguity())
                    {
                        error_printf("%s: error: invalid template-argument number %d\n",
                                ast_location(template_parameter),
                                position);
                    }
                    return NULL;
                }

                type_t* declarator_type;
                compute_declarator_type(abstract_decl, &gather_info, type_info, &declarator_type,
                        template_parameters_context, &dummy_nodecl_output);

                if (is_named_type(declarator_type)
                        && (named_type_get_symbol(declarator_type)->kind == SK_TEMPLATE
                            || named_type_get_symbol(declarator_type)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER))
                {
                    if (abstract_decl != NULL)
                    {
                        if (!checking_ambiguity())
                        {
                            error_printf("%s: error: invalid template-argument number %d\n",
                                    ast_location(template_parameter),
                                    position);
                        }
                    }
                    t_argument->kind = TPK_TEMPLATE;
                }
                else
                {
                    t_argument->kind = TPK_TYPE;
                }
                t_argument->type = declarator_type;

                return t_argument;
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_template_argument(template_parameter, template_parameters_context);

                return get_single_template_argument_from_syntax(template_parameter, template_parameters_context, position);
                break;
            }
        default:
            {
                internal_error("Invalid node %s", ast_print_node_type(ASTType(template_parameter)));
            }
    }
    return NULL;
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

        template_parameter_value_t* t_argument = get_single_template_argument_from_syntax(template_parameter,
                template_parameters_context, position);

        if (t_argument == NULL)
            return NULL;
        
        int num_parameters = result->num_parameters;
        // Empty parameter, it will be filled at a later moment
        P_LIST_ADD(result->parameters, 
                num_parameters,
                NULL);
        P_LIST_ADD(result->arguments, 
                result->num_parameters,
                t_argument);

        position++;
    }

    return result;
}

static template_parameter_list_t* complete_template_parameters_of_template_class(
        decl_context_t template_name_context,
        type_t* template_type,
        template_parameter_list_t* template_parameters,
        const char* filename, int line)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Completing template arguments for class template type\n");
    }

    template_parameter_list_t* primary_template_parameters =
        template_type_get_template_parameters(template_type);

    template_parameter_list_t* result = duplicate_template_argument_list(template_parameters);

    // Note: we are creating a new template parameter list but it is a sibling
    // of the primary template so we must ensure they have the same nesting in the
    // hierarchy of template parameters
    result->enclosing = primary_template_parameters->enclosing;

    if (result->num_parameters > primary_template_parameters->num_parameters)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Too many template arguments %d > %d", 
                    result->num_parameters, 
                    primary_template_parameters->num_parameters);
        }

        if (!checking_ambiguity())
        {
            error_printf("%s:%d: error: too many template-arguments for template class\n",
                    filename, line);
        }

        return NULL;
    }

    decl_context_t new_template_context = template_name_context;
    new_template_context.template_parameters = result;
    
    // Now review template parameters
    int i;
    for (i = 0; i < primary_template_parameters->num_parameters; i++)
    {
        if (i >= result->num_parameters)
        {
            if (primary_template_parameters->arguments[i] == NULL)
            {
                // One of the template parameters is lacking an argument
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Template argument %d is missing", i);
                }

                if (!checking_ambiguity())
                {
                    error_printf("%s:%d: error: template argument number %d is missing and there is no default template argument for it\n",
                            filename, line, i);
                }

                return NULL;
            }
            else
            {
                // Note that this adds the parameter and the argument, they will be updated later (if needed)
                int num_parameters = result->num_parameters;
                P_LIST_ADD(result->parameters,
                        num_parameters,
                        primary_template_parameters->parameters[i]);
                template_parameter_value_t* v = update_template_parameter_value_of_template_class(primary_template_parameters->arguments[i],
                        new_template_context,
                        filename, line);
                P_LIST_ADD(result->arguments, result->num_parameters, v);
            }
        }
        else
        {
            // Set the template parameter
            result->parameters[i] = primary_template_parameters->parameters[i];

            // And check it matches what we got
            if (result->parameters[i]->kind != result->arguments[i]->kind)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Template parameter kind and template argument kind do not match\n");
                }

                if (!checking_ambiguity())
                {
                    error_printf("%s:%d: error: kind of template argument number %d does not match "
                            "that of the corresponding template parameter\n",
                            filename, line, i);
                }


                return NULL;
            }
        }
        
        // Nontype template arguments must be adjusted first
        if (result->arguments[i]->kind == TPK_NONTYPE)
        {
            // We need to do this because of cases like this
            //
            // N in    template <typename T, T N>                       
            // PF in   template <typename R, typename A, R (*PF)(A)>    
            result->arguments[i]->type = update_type(
                    result->parameters[i]->entry->type_information,
                    new_template_context,
                    filename, line);

            type_t* dest_type = result->arguments[i]->type;

            if (!nodecl_expr_is_value_dependent(result->arguments[i]->value))
            {
                type_t* arg_type = nodecl_get_type(result->arguments[i]->value);
                if (is_unresolved_overloaded_type(arg_type))
                {
                    // We got an unresolved entity here, try to solve it
                    scope_entry_t* entry = address_of_overloaded_function(
                            unresolved_overloaded_type_get_overload_set(arg_type),
                            unresolved_overloaded_type_get_explicit_template_arguments(arg_type),
                            dest_type,
                            new_template_context,
                            filename, line);

                    if (entry == NULL)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "SCOPE: Cannot solve unresolved overload in template argument expression to"
                                    " the type of the template parameter\n");
                        }
                        if (!checking_ambiguity())
                        {
                            error_printf("%s:%d: error: cannot solve address of overload function in template argument number %d",
                                    filename, line, i);
                        }
                        return NULL;
                    }

                    // If the symbol is not null, update the argument with its real function
                    result->arguments[i]->value = nodecl_make_symbol(entry, filename, line);
                    nodecl_set_type(result->arguments[i]->value, entry->type_information);
                }
                else
                {
                    // We can't allow a user defined conversion here since it
                    // would mean executing user code at compile time, which is
                    // not possible, so we check for a SCS.
                    //
                    standard_conversion_t scs_conv;
                    if (!standard_conversion_between_types(&scs_conv, arg_type, dest_type))
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "SCOPE: Cannot convert template argument expression to the type of the template parameter\n");
                        }
                        if (!checking_ambiguity())
                        {
                            error_printf("%s:%d: error: cannot convert type '%s' of template argument type %d to "
                                    "the type '%s' of its template parameter",
                                    filename, line, 
                                    print_type_str(arg_type, template_name_context),
                                    i,
                                    print_type_str(dest_type, template_name_context));
                        }
                        return NULL;
                    }
                }
            }
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Finished completing template arguments\n");
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

const char* template_arguments_to_str(
        template_parameter_list_t* template_parameters,
        decl_context_t decl_context)
{
    if (template_parameters->num_parameters == 0)
        return "";

    const char* result = "";
    // It is not enough with the name, we have to print the arguments
    result = strappend(result, "<");

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


const char* get_template_arguments_str(scope_entry_t* entry, 
        decl_context_t decl_context)
{
    template_parameter_list_t* template_parameters = template_specialized_type_get_template_arguments(entry->type_information);
    return template_arguments_to_str(template_parameters, decl_context);
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

static const char* get_fully_qualified_symbol_name_of_dependent_typename(
        scope_entry_t* entry,
        decl_context_t decl_context,
        char *is_dependent, int *max_qualif_level)
{
    scope_entry_t* dependent_entry = NULL;
    nodecl_t nodecl_parts = nodecl_null();

    dependent_typename_get_components(entry->type_information, 
            &dependent_entry, &nodecl_parts);

    const char* result = get_fully_qualified_symbol_name(dependent_entry,
            dependent_entry->decl_context,
            is_dependent, max_qualif_level);

    int num_parts = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(nodecl_parts, 0), &num_parts);

    int i;
    for (i = 0; i < num_parts; i++)
    {
        nodecl_t current_part = list[i];

        nodecl_t simple_current_part = current_part;
        template_parameter_list_t* template_parameters = NULL;

        if (nodecl_get_kind(current_part) == NODECL_CXX_DEP_TEMPLATE_ID)
        {
            template_parameters = nodecl_get_template_parameters(current_part);
            simple_current_part = nodecl_get_child(current_part, 0);
        }

        const char* name = nodecl_get_text(simple_current_part);

        result = strappend(result, "::");
        if (template_parameters != NULL && is_dependent)
        {
            result = strappend(result, "template ");
        }
        result = strappend(result, name);

        if (template_parameters != NULL)
        {
            result = strappend(result, "< ");
            int j;
            for (j = 0; j < template_parameters->num_parameters; j++)
            {
                template_parameter_value_t * template_arg = template_parameters->arguments[j];

                switch (template_arg->kind)
                {
                    case TPK_TYPE:
                        {
                            result = strappend(result, 
                                    print_type_str(template_arg->type, decl_context));
                            break;
                        }
                    case TPK_NONTYPE:
                        {
                            result = strappend(result, 
                                    c_cxx_codegen_to_str(template_arg->value));
                            break;
                        }
                    case TPK_TEMPLATE:
                        {
                            result = strappend(result,
                                    get_qualified_symbol_name(named_type_get_symbol(template_arg->type), 
                                        decl_context));
                            break;
                        }
                    default:
                        {
                            internal_error("Invalid template argument kind", 0);
                        }
                }

                if ((j + 1) < template_parameters->num_parameters)
                {
                    result = strappend(result, ", ");
                }
            }

            if (result[strlen(result) - 1] == '>')
            {
                result = strappend(result, " ");
            }

            result = strappend(result, ">");
        }
    }

    return result;
}

// Get the fully qualified symbol name in the scope of the ocurrence
static const char* get_fully_qualified_symbol_name_ex(scope_entry_t* entry, 
        decl_context_t decl_context, char* is_dependent, int* max_qualif_level,
        char no_templates, char only_classes)
{
    // DEBUG_CODE()
    // {
    //     fprintf(stderr, "SCOPE: Getting fully qualified symbol name for '%s'\n", entry->symbol_name);
    // }

    // If this is the injected symbol, ignore it and get the real entry
    if (entry->entity_specs.is_injected_class_name)
    {
        // The injected class name is a member
        entry = named_type_get_symbol(entry->entity_specs.class_type);
    }

    const char* result = ""; 
    // Do not print anonymous unions or variables of anonymous unions
    if (!entry->entity_specs.is_anonymous_union
            && !(is_named_class_type(entry->type_information)
                && named_type_get_symbol(entry->type_information)->entity_specs.is_anonymous_union))
    {
        result = uniquestr(unmangle_symbol_name(entry));
    }

    char current_has_template_parameters = 0;

    if (entry->kind == SK_TEMPLATE_PARAMETER
            || entry->kind == SK_TEMPLATE_TYPE_PARAMETER
            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        // This symbol must be looked up for the proper real name
        scope_entry_t* real_name = lookup_of_template_parameter(
                decl_context,
                entry->entity_specs.template_parameter_nesting,
                entry->entity_specs.template_parameter_position);

        // ERROR_CONDITION(real_name == NULL, "Invalid template parameter symbol", 0);

        if (real_name != NULL)
        {
            result = real_name->symbol_name;
        }
        else
        {
            result = strappend("/* ??? */", entry->symbol_name);
        }

        // This is dependent
        (*is_dependent) |= 1;
        return result;
    }
    else if (entry->kind == SK_DEPENDENT_ENTITY)
    {
        return get_fully_qualified_symbol_name_of_dependent_typename(entry, decl_context,
                is_dependent, max_qualif_level);
    }
    else if (!no_templates
            && entry->type_information != NULL
            && is_template_specialized_type(entry->type_information)
            && template_specialized_type_get_template_arguments(entry->type_information) != NULL
            && !entry->entity_specs.is_conversion)
    {
        current_has_template_parameters = 1;
        const char *template_parameters = get_template_arguments_str(entry, decl_context);
        result = strappend(result, template_parameters);

        (*is_dependent) |= is_dependent_type(entry->type_information);

        type_t* template_type = template_specialized_type_get_related_template_type(entry->type_information);
        scope_entry_t* template_sym = template_type_get_related_symbol(template_type);
        if (template_sym->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
        {
            // This is dependent
            (*is_dependent) |= 1;
            return result;
        }
    }

    if (entry->entity_specs.is_member)
    {
        // We need the qualification of the class
        ERROR_CONDITION(!is_named_class_type(entry->entity_specs.class_type), "The class of a member must be named", 0);

        scope_entry_t* class_symbol = named_type_get_symbol(entry->entity_specs.class_type);

        (*max_qualif_level)++;

        char prev_is_dependent = 0;
        const char* class_qualification = 
            get_fully_qualified_symbol_name_ex(class_symbol, decl_context, &prev_is_dependent, max_qualif_level, 
                    /* no_templates */ 0, only_classes);

        if (!class_symbol->entity_specs.is_anonymous_union)
        {
            class_qualification = strappend(class_qualification, "::");
        }

        if (prev_is_dependent
                && current_has_template_parameters)
        {
            class_qualification = strappend(class_qualification, "template ");
        }

        (*is_dependent) |= prev_is_dependent;

        result = strappend(class_qualification, result);
    } 
    else if (!entry->entity_specs.is_member
            && !only_classes)
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
            decl_context, is_dependent, max_qualif_level, /* no_templates */ 0, /* only_classes */ 0);
}

const char* get_fully_qualified_symbol_name_without_template(scope_entry_t* entry, 
        decl_context_t decl_context, char* is_dependent, int* max_qualif_level)
{
    return get_fully_qualified_symbol_name_ex(entry,
            decl_context, is_dependent, max_qualif_level, /* no_templates */ 1, /* only_classes */ 0);
}

const char* get_class_qualification_of_symbol(scope_entry_t* entry,
        decl_context_t decl_context, char* is_dependent, int* max_qualif_level)
{
    return get_fully_qualified_symbol_name_ex(entry,
            decl_context, is_dependent, max_qualif_level, /* no_templates */ 0, /* only_classes */ 1);
}

const char* get_class_qualification_of_symbol_without_template(scope_entry_t* entry,
        decl_context_t decl_context, char* is_dependent, int* max_qualif_level)
{
    return get_fully_qualified_symbol_name_ex(entry,
            decl_context, is_dependent, max_qualif_level, /* no_templates */ 1, /* only_classes */ 1);
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
scope_entry_list_t* cascade_lookup(decl_context_t decl_context, 
        const char* name, 
        decl_flags_t decl_flags,
        const char* filename, int line)
{
    ERROR_CONDITION(name == NULL, "Name cannot be null!", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Name lookup of '%s'\n", name);
    }

    scope_entry_list_t* result = NULL;

    int num_associated_namespaces = 0;
    associated_namespace_t associated_namespaces[MCXX_MAX_ASSOCIATED_NAMESPACES];
    memset(associated_namespaces, 0, sizeof(associated_namespaces));

    scope_t* current_scope = decl_context.current_scope;

    while (current_scope != NULL)
    {
        if (current_scope->kind == CLASS_SCOPE
                && !BITMAP_TEST(decl_flags, DF_ONLY_CURRENT_SCOPE))
        {
            scope_entry_list_t* old_result = result;
            scope_entry_list_t* current_class = query_in_class(current_scope, name, decl_flags, 
                        filename, line);
            result = entry_list_merge(old_result, current_class);
            entry_list_free(old_result);
            entry_list_free(current_class);
        }
        else if (current_scope->kind == NAMESPACE_SCOPE)
        {
            scope_entry_list_t* old_result = result;
            scope_entry_list_t* current_namespace = unqualified_query_in_namespace(
                    current_scope->related_entry,
                    name, num_associated_namespaces,
                    associated_namespaces, filename, line);
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

        if (BITMAP_TEST(decl_flags, DF_ELABORATED_NAME))
        {
            scope_entry_list_t* old_result = result;
            result = filter_any_non_type(old_result);
            entry_list_free(old_result);
        }

        if (BITMAP_TEST(decl_flags, DF_ONLY_CURRENT_SCOPE))
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
        }

        i++;
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
            if (value->kind == TPK_NONTYPE
                    || value->kind == TPK_TYPE)
            {
                value->entry = counted_calloc(1, sizeof(*value->entry), &_bytes_used_scopes);
                value->entry->symbol_name = parameter_entry->symbol_name;
                value->entry->decl_context = context;
                value->entry->entity_specs.is_template_parameter = 1;
            }

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
                        // Use the original symbol (we will not know it is a
                        // template_parameter name, though)
                        value->entry = named_type_get_symbol(value->type);
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
        nesting++;
        template_parameters = template_parameters->enclosing;
    }

    return nesting;
}

// Debugging
void print_template_parameter_list_aux(template_parameter_list_t* template_parameters, int* n)
{
    if (template_parameters == NULL)
        return;

    print_template_parameter_list_aux(template_parameters->enclosing, n);

    (*n)++;

    if (template_parameters->num_parameters > 0)
    {
        int i;
        for (i = 0; i < template_parameters->num_parameters; i++)
        {
            const char* kind_name = "<<unknown>>";
            switch (template_parameters->parameters[i]->kind)
            {
                case TPK_NONTYPE: kind_name = "nontype"; break;
                case TPK_TYPE: kind_name = "type"; break;
                case TPK_TEMPLATE: kind_name = "template"; break;
                default: break;
            }
            fprintf(stderr, "* Nesting: %d | Position: %d | Name: %s | Kind : %s\n", *n, i, 
                    template_parameters->parameters[i]->entry->symbol_name,
                    kind_name);

            template_parameter_value_t* v = template_parameters->arguments[i];
            if (v == NULL)
            {
                fprintf(stderr, "  Argument: <<NONE>>\n");
            }
            else
            {
                switch (v->kind)
                {
                    case TPK_TYPE:
                    case TPK_TEMPLATE:
                        {
                            fprintf(stderr, "  Argument: %s\n", print_declarator(v->type));
                            break;
                        }
                    case TPK_NONTYPE:
                        {
                            fprintf(stderr, "  Argument: %s\n", c_cxx_codegen_to_str(v->value));
                            fprintf(stderr, "  (Type: %s)\n", print_declarator(v->type));
                            break;
                        }
                    default:
                        {
                            fprintf(stderr, "  Argument: ????\n");
                            break;
                        }
                }
            }
        }
    }
    else
    {
        fprintf(stderr, "* Nesting: %d <<<EMPTY>>>\n", *n);
    }
}

void print_template_parameter_list(template_parameter_list_t* template_parameters)
{
    if (template_parameters == NULL)
    {
        fprintf(stderr, "<<<No template parameters>>>\n");
    }
    int n = 0;
    print_template_parameter_list_aux(template_parameters, &n);
}

scope_entry_list_t* query_nodecl_name_flags(decl_context_t decl_context,
        nodecl_t nodecl_name, decl_flags_t decl_flags);

static scope_entry_list_t* query_nodecl_simple_name(decl_context_t decl_context,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags)
{
    ERROR_CONDITION(nodecl_get_kind(nodecl_name) != NODECL_CXX_DEP_NAME_SIMPLE, "Invalid nodecl", 0);

    if (BITMAP_TEST(decl_flags, DF_LABEL))
    {
        decl_context.current_scope = decl_context.function_scope;
    }

    const char* filename = nodecl_get_filename(nodecl_name);
    int line = nodecl_get_line(nodecl_name);
    const char* name = nodecl_get_text(nodecl_name);

    if (BITMAP_TEST(decl_flags, DF_CONSTRUCTOR))
    {
        name = strappend("constructor ", name);
    }

    scope_entry_list_t* result = name_lookup(decl_context, name, decl_flags, filename, line);

    scope_entry_t* head = NULL;
    if (result != NULL
            && BITMAP_TEST(decl_flags, DF_DEPENDENT_TYPENAME)
            && (head = entry_list_head(result))->entity_specs.is_member
            && is_dependent_type(head->entity_specs.class_type))
    {
        scope_entry_t* new_sym = counted_calloc(1, sizeof(*new_sym), &_bytes_used_scopes);
        new_sym->kind = SK_DEPENDENT_ENTITY;
        new_sym->symbol_name = nodecl_get_text(nodecl_name_get_last_part(nodecl_name));
        new_sym->decl_context = decl_context;
        new_sym->file = filename;
        new_sym->line = line;
        new_sym->type_information = build_dependent_typename_for_entry(
                named_type_get_symbol(head->entity_specs.class_type),
                nodecl_name, 
                filename,
                line);

        entry_list_free(result);

        return entry_list_new(new_sym);
    }

    return result;
}

static scope_entry_list_t* query_nodecl_simple_name_in_class(decl_context_t decl_context,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags)
{
    if (decl_context.current_scope->kind != CLASS_SCOPE)
    {
        internal_error("Code unreachable", 0);
    }

    const char* filename = nodecl_get_filename(nodecl_name);
    int line = nodecl_get_line(nodecl_name);
    const char* name = nodecl_get_text(nodecl_name);

    if (BITMAP_TEST(decl_flags, DF_CONSTRUCTOR))
    {
        name = strappend("constructor ", name);
    }

    if (BITMAP_TEST(decl_flags, DF_DEPENDENT_TYPENAME)
            && is_dependent_type(decl_context.current_scope->related_entry->type_information))
    {
        scope_entry_t* new_sym = counted_calloc(1, sizeof(*new_sym), &_bytes_used_scopes);
        new_sym->kind = SK_DEPENDENT_ENTITY;
        new_sym->decl_context = decl_context;
        new_sym->file = filename;
        new_sym->line = line;
        new_sym->symbol_name = nodecl_get_text(nodecl_name_get_last_part(nodecl_name));
        new_sym->type_information = build_dependent_typename_for_entry(
                decl_context.current_scope->related_entry,
                nodecl_name, 
                filename, line);

        return entry_list_new(new_sym);
    }

    return query_in_class(decl_context.current_scope, 
            name, 
            decl_flags,
            filename, line);
}

static scope_entry_list_t* query_nodecl_simple_name_in_namespace(decl_context_t decl_context,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags)
{
    const char* filename = nodecl_get_filename(nodecl_name);
    int line = nodecl_get_line(nodecl_name);
    const char* name = nodecl_get_text(nodecl_name);

    return qualified_query_in_namespace(decl_context.current_scope->related_entry, 
            name,
            decl_flags,
            filename,
            line);
}

scope_entry_list_t* query_nodecl_template_id(decl_context_t decl_context, 
        nodecl_t nodecl_name, decl_flags_t decl_flags,
        scope_entry_list_t* (*query_fun_nodecl)(decl_context_t, nodecl_t, decl_flags_t)
        )
{
    nodecl_t simple_name = nodecl_get_child(nodecl_name, 0);

    template_parameter_list_t* template_parameters = nodecl_get_template_parameters(nodecl_name);

    if (template_parameters == NULL)
        return NULL;

    scope_entry_list_t* entry_list = query_fun_nodecl(decl_context, simple_name, 
            decl_flags);

    // Ignore injected class name
    if (entry_list != NULL
            && entry_list_head(entry_list)->entity_specs.is_injected_class_name)
    {
        scope_entry_t* entry_name = named_type_get_symbol(entry_list_head(entry_list)->entity_specs.class_type);

        if (!is_template_specialized_type(entry_name->type_information))
        {
            entry_list_free(entry_list);
            return NULL;
        }

        type_t* template_type = template_specialized_type_get_related_template_type(entry_name->type_information);
        entry_name = template_type_get_related_symbol(template_type);
        entry_list_free(entry_list);
        entry_list = entry_list_new(entry_name);
    }

    // Filter template-names
    enum cxx_symbol_kind template_name_filter[] = {
        SK_TEMPLATE_TEMPLATE_PARAMETER,
        SK_TEMPLATE,
        SK_USING,
    };

    entry_list = filter_symbol_kind_set(entry_list, 
            STATIC_ARRAY_LENGTH(template_name_filter), 
            template_name_filter);

    if (entry_list == NULL)
        return NULL;

    scope_entry_t* template_symbol = entry_advance_aliases(entry_list_head(entry_list));

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

        template_parameter_list_t* completed_template_parameters = 
            complete_template_parameters_of_template_class(decl_context,
                    generic_type,
                    template_parameters,
                    nodecl_get_filename(nodecl_name), 
                    nodecl_get_line(nodecl_name));
        
        if (completed_template_parameters == NULL)
            return NULL;

        specialized_type = template_type_get_specialized_type(generic_type, 
                completed_template_parameters,
                decl_context,
                nodecl_get_filename(nodecl_name),
                nodecl_get_line(nodecl_name));

        if (specialized_type != NULL)
        {
            ERROR_CONDITION(!is_named_type(specialized_type), "This should be a named type", 0);

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

        // Let the user of this function select the proper template
        return entry_list;
    }
    else 
    {
        internal_error("Invalid templated type", 0);
    }
}

static char same_type_conversion(scope_entry_t* entry, void *p)
{
    if (entry->kind == SK_FUNCTION)
    {
        type_t* t = (type_t*)p;

        type_t* conversion_type = function_type_get_return_type(entry->type_information);
        return equivalent_types(conversion_type, t);
    }
    else if (entry->kind == SK_TEMPLATE)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

static scope_entry_list_t* query_nodecl_conversion_name(decl_context_t decl_context,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags UNUSED_PARAMETER)
{
    // We need a class scope around that we will check first
    if (decl_context.class_scope == NULL)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: conversion-id requires an enclosing class scope\n", 
                    nodecl_get_locus(nodecl_name));
        }
        return NULL;
    }

    // We keep this tree because of the double lookup required for conversion-id
    AST type_id = nodecl_get_ast(nodecl_get_child(nodecl_name, 0));

    // Lookup first in class scope
    decl_context_t class_context = decl_context;
    class_context.current_scope = class_context.class_scope;

    type_t* t = compute_type_for_type_id_tree(type_id, class_context);
    if (t == NULL)
    {
        // Try the current scope
        t = compute_type_for_type_id_tree(type_id, decl_context);

        if (t == NULL)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: type-id %s of conversion-id not found\n",
                        nodecl_get_locus(nodecl_name),
                        prettyprint_in_buffer(type_id));
            }
        }
    }

    scope_entry_list_t* entry_list = query_name_in_scope(class_context.class_scope, "$.operator");

    scope_entry_list_t* result = filter_symbol_using_predicate(entry_list, same_type_conversion, t);

    entry_list_free(entry_list);

    return result;
}

static scope_entry_list_t* query_nodecl_qualified_name_aux(decl_context_t decl_context,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags,
        char check_symbol_in_nest(scope_entry_t* previous_sym, 
            scope_entry_t* current_sym, 
            char is_last, 
            const char* filename,
            int line,
            void* data),
        void *check_symbol_data)
{
    ERROR_CONDITION(nodecl_get_kind(nodecl_name) != NODECL_CXX_DEP_NAME_NESTED
            && nodecl_get_kind(nodecl_name) != NODECL_CXX_DEP_GLOBAL_NAME_NESTED,
            "Invalid nodecl", 0);

    char is_global = (nodecl_get_kind(nodecl_name) == NODECL_CXX_DEP_GLOBAL_NAME_NESTED);

    scope_entry_t* previous_symbol = NULL;

    decl_flags_t nested_flags = decl_flags;
    nested_flags &= ~DF_LABEL;
    nested_flags &= ~DF_CONSTRUCTOR;
    nested_flags &= ~DF_DEPENDENT_TYPENAME;

    int num_items = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(nodecl_name, 0), &num_items);

    decl_context_t current_context = decl_context;

    if (is_global)
    {
        current_context.current_scope = current_context.global_scope;
        previous_symbol = current_context.global_scope->related_entry;
    }

    char allow_namespaces = 1;

    int i;
    // Note that we do not handle the last name here
    for (i = 0; i < num_items - 1; i++)
    {
        nodecl_t current_name = list[i];

        scope_entry_list_t* current_entry_list = NULL;
        scope_entry_t* current_symbol = NULL;

        if (previous_symbol == NULL)
        {
            current_entry_list = query_nodecl_name_flags(current_context, current_name, nested_flags);
        }
        else if (previous_symbol->kind == SK_CLASS)
        {
            if (nodecl_get_kind(current_name) != NODECL_CXX_DEP_TEMPLATE_ID)
            {
                current_entry_list = query_nodecl_simple_name_in_class(
                        current_context,
                        current_name,
                        nested_flags);
            }
            else
            {
                current_entry_list = query_nodecl_template_id(
                        current_context,
                        current_name, nested_flags,
                        query_nodecl_simple_name_in_class);
            }
        }
        else if (previous_symbol->kind == SK_NAMESPACE)
        {
            if (nodecl_get_kind(current_name) != NODECL_CXX_DEP_TEMPLATE_ID)
            {
                current_entry_list = query_nodecl_simple_name_in_namespace(
                        current_context,
                        current_name,
                        nested_flags);
            }
            else
            {
                current_entry_list = query_nodecl_template_id(current_context,
                        current_name, nested_flags,
                        query_nodecl_simple_name_in_namespace);
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
            free(list);
            return NULL;
        }

        current_symbol = entry_list_head(current_entry_list);

        if (current_symbol->kind == SK_NAMESPACE)
        {
            if (!allow_namespaces)
            {
                internal_error("Invalidly nested namespace '%s' inside of a class\n", current_symbol->symbol_name);
            }

            // Update the context
            current_context = current_symbol->related_decl_context;
        }
        else if (current_symbol->kind == SK_CLASS
                || (current_symbol->kind == SK_TYPEDEF)
                || current_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER
                || current_symbol->kind == SK_DEPENDENT_ENTITY)
        {
            if (current_symbol->kind == SK_TYPEDEF)
            {
                type_t* t = advance_over_typedefs(current_symbol->type_information);

                if (is_dependent_typename_type(t))
                {
                    scope_entry_t* dependent_symbol = create_new_dependent_entity(
                            decl_context,
                            current_symbol,
                            i, num_items,
                            nodecl_get_filename(current_name),
                            nodecl_get_line(current_name),
                            list);

                    free(list);

                    return entry_list_new(dependent_symbol);
                }

                if (!is_named_type(t))
                {
                    if (!checking_ambiguity())
                    {
                        error_printf("%s: typedef name '%s' is not a namespace or class\n", 
                                nodecl_get_locus(current_name),
                                nodecl_get_text(current_name));
                    }
                    return NULL;
                }

                current_symbol = named_type_get_symbol(t);
            }

            if (current_symbol->kind == SK_CLASS)
            {
                type_t* class_type = current_symbol->type_information;

                if (class_type_is_incomplete_independent(class_type))
                {
                    instantiate_template_class(current_symbol, current_symbol->decl_context,
                            nodecl_get_filename(current_name), nodecl_get_line(current_name));
                }
                else if (class_type_is_incomplete_dependent(class_type)
                        // In some cases we do not want to examine uninstantiated templates
                        || (BITMAP_TEST(decl_flags, DF_DEPENDENT_TYPENAME)
                            // Why are we checking this?
                            && (class_type_is_complete_dependent(class_type)
                                || (current_symbol->decl_context.current_scope->kind == CLASS_SCOPE
                                    && is_dependent_type(current_symbol->decl_context.current_scope->related_entry->type_information))
                               )))
                {
                    scope_entry_t* dependent_symbol = create_new_dependent_entity(
                            decl_context,
                            current_symbol,
                            i, num_items,
                            nodecl_get_filename(current_name),
                            nodecl_get_line(current_name),
                            list);
                    free(list);
                    return entry_list_new(dependent_symbol);
                }

                current_context = class_type_get_inner_context(class_type);
            }
            else if (current_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER
                    || current_symbol->kind == SK_DEPENDENT_ENTITY)
            {
                scope_entry_t* dependent_symbol = create_new_dependent_entity(
                        decl_context,
                        current_symbol,
                        i, num_items,
                        nodecl_get_filename(current_name),
                        nodecl_get_line(current_name),
                        list);
                free(list);
                return entry_list_new(dependent_symbol);
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            allow_namespaces = 0;
        }
        else if (current_symbol->kind == SK_TEMPLATE
                || current_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: template-name '%s' used without template arguments\n", 
                        nodecl_get_locus(current_name),
                        nodecl_get_text(current_name));
            }
            return NULL;
        }
        else
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: name '%s' is not a namespace or class\n", 
                        nodecl_get_locus(current_name),
                        nodecl_get_text(current_name));
            }
            return NULL;
        }

        if (check_symbol_in_nest != NULL
                && !check_symbol_in_nest(previous_symbol, current_symbol, 
                    /* is_last */ 0,
                    nodecl_get_filename(current_name),
                    nodecl_get_line(current_name),
                    check_symbol_data))
        {
            return NULL;
        }

        previous_symbol = current_symbol;
    }

    nodecl_t last_name = list[num_items - 1];


    scope_entry_list_t* result = NULL;
    
    if (previous_symbol == NULL)
    {
        result = query_nodecl_name_flags(current_context, last_name, decl_flags);
    }
    else if (previous_symbol->kind == SK_NAMESPACE)
    {
        if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_NAME_SIMPLE)
        {
            result = query_nodecl_simple_name_in_namespace(
                    current_context,
                    last_name,
                    decl_flags);
        }
        else if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_TEMPLATE_ID)
        {
            result = query_nodecl_template_id(current_context,
                    last_name, decl_flags,
                    query_nodecl_simple_name_in_namespace);
        }
        else if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_NAME_CONVERSION)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: conversion-id is not valid in a non-class scope\n",
                        nodecl_get_locus(last_name));
            }
            return NULL;
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    else if (previous_symbol->kind == SK_CLASS)
    {
        if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_NAME_SIMPLE)
        {
            result = query_nodecl_simple_name_in_class(
                    current_context,
                    last_name,
                    decl_flags);
        }
        else if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_TEMPLATE_ID)
        {
            result = query_nodecl_template_id(current_context,
                    last_name, decl_flags,
                    query_nodecl_simple_name_in_class);
        }
        else if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_NAME_CONVERSION)
        {
            result = query_nodecl_conversion_name(current_context,
                    last_name, decl_flags);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    if (result != NULL
            && check_symbol_in_nest != NULL
            && !check_symbol_in_nest(previous_symbol, 
                entry_list_head(result),
                /* is_last */ 1,
                nodecl_get_filename(last_name),
                nodecl_get_line(last_name),
                check_symbol_data))
    {
        free(result);
        return NULL;
    }

    free(list);

    return result;
}

static scope_entry_list_t* query_nodecl_qualified_name(decl_context_t decl_context,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags)
{
    return query_nodecl_qualified_name_aux(decl_context,
            nodecl_name,
            decl_flags,
            NULL, NULL);

}


static char check_symbol_is_base_or_member(scope_entry_t* previous_symbol, 
        scope_entry_t* current_symbol, 
        char is_last, 
        const char* filename,
        int line,
        void* data)
{
    scope_entry_t* class_symbol = (scope_entry_t*)data;

    if (previous_symbol != NULL)
    {
        class_symbol = previous_symbol;
    }

    if (class_symbol->entity_specs.is_injected_class_name)
    {
        class_symbol = named_type_get_symbol(class_symbol->entity_specs.class_type);
    }

    ERROR_CONDITION(class_symbol->kind != SK_CLASS, "This should be a class", 0);

    if (is_last)
    {
        // If we are the last component we must be a member of class_symbol
        if (!(current_symbol->entity_specs.is_member
                    && (class_type_is_base(current_symbol->entity_specs.class_type, class_symbol->type_information)
                        || named_type_get_symbol(current_symbol->entity_specs.class_type) == class_symbol)))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s:%d: error: '%s' is not a member of '%s'\n",
                        filename, line,
                        current_symbol->symbol_name,
                        get_qualified_symbol_name(class_symbol, class_symbol->decl_context));
            }
            return 0;
        }
    }
    else
    {
        if (current_symbol->kind == SK_TYPEDEF)
        {
            if (is_named_type(advance_over_typedefs(current_symbol->type_information)))
            {
                current_symbol = named_type_get_symbol(advance_over_typedefs(current_symbol->type_information));
            }
        }
        // If we are not the last component we must be a base class
        if (current_symbol->kind != SK_CLASS
                || !class_type_is_base(current_symbol->type_information, class_symbol->type_information))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s:%d: error: '%s' is not a base of '%s'\n",
                        filename, line,
                        current_symbol->symbol_name,
                        get_qualified_symbol_name(class_symbol, class_symbol->decl_context));
            }
            return 0;
        }
    }
    return 1;
}

static scope_entry_list_t* query_nodecl_qualified_name_in_class(decl_context_t decl_context,
        scope_entry_t* class_symbol,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags)
{
    return query_nodecl_qualified_name_aux(decl_context,
            nodecl_name,
            decl_flags,
            check_symbol_is_base_or_member,
            class_symbol);
}

static scope_entry_list_t* query_nodecl_name_in_class_aux(
        decl_context_t decl_context,
        scope_entry_t* class_symbol,
        nodecl_t nodecl_name, 
        decl_flags_t decl_flags)
{
    switch (nodecl_get_kind(nodecl_name))
    {
        case NODECL_CXX_DEP_NAME_SIMPLE:
            {
                return query_nodecl_simple_name_in_class(
                        class_type_get_inner_context(class_symbol->type_information), 
                        nodecl_name, decl_flags);
                break;
            }
        case NODECL_CXX_DEP_TEMPLATE_ID:
            {
                return query_nodecl_template_id(
                        class_type_get_inner_context(class_symbol->type_information), 
                        nodecl_name, decl_flags, 
                        query_nodecl_simple_name_in_class);
                break;
            }
        case NODECL_CXX_DEP_NAME_CONVERSION:
            {
                return query_nodecl_conversion_name(
                        class_type_get_inner_context(class_symbol->type_information), 
                        nodecl_name, decl_flags);
                break;
            }
        case NODECL_CXX_DEP_NAME_NESTED:
        case NODECL_CXX_DEP_GLOBAL_NAME_NESTED:
            {
                return query_nodecl_qualified_name_in_class(decl_context, class_symbol, nodecl_name, decl_flags);
                break;
            }
        default:
            {
                internal_error("Invalid nodecl kind '%s'\n", ast_print_node_type(nodecl_get_kind(nodecl_name)));
            }
    }
    return NULL;
}

static scope_entry_list_t* query_nodecl_name_in_class_aux(
        decl_context_t decl_context,
        scope_entry_t* class_symbol,
        nodecl_t nodecl_name, 
        decl_flags_t decl_flags);

scope_entry_list_t* query_nodecl_name_in_class_flags(
        decl_context_t decl_context,
        scope_entry_t* class_symbol,
        nodecl_t nodecl_name, decl_flags_t decl_flags)
{
    ERROR_CONDITION(class_symbol == NULL 
            || class_symbol->kind != SK_CLASS, "Invalid symbol", 0);

    type_t* class_type = class_symbol->type_information;

    if (class_type_is_incomplete_independent(class_type))
    {
        instantiate_template_class(class_symbol, class_symbol->decl_context, 
                nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));
    }

    return query_nodecl_name_in_class_aux(decl_context, class_symbol, nodecl_name, decl_flags);
}

scope_entry_list_t* query_nodecl_name_flags(decl_context_t decl_context,
        nodecl_t nodecl_name, decl_flags_t decl_flags)
{
    switch (nodecl_get_kind(nodecl_name))
    {
        case NODECL_CXX_DEP_NAME_SIMPLE:
            {
                return query_nodecl_simple_name(decl_context, nodecl_name, decl_flags);
                break;
            }
        case NODECL_CXX_DEP_TEMPLATE_ID:
            {
                return query_nodecl_template_id(decl_context, nodecl_name, decl_flags, 
                        query_nodecl_simple_name);
                break;
            }
        case NODECL_CXX_DEP_NAME_CONVERSION:
            {
                return query_nodecl_conversion_name(decl_context, nodecl_name, decl_flags);
                break;
            }
        case NODECL_CXX_DEP_NAME_NESTED:
        case NODECL_CXX_DEP_GLOBAL_NAME_NESTED:
            {
                return query_nodecl_qualified_name(decl_context, nodecl_name, decl_flags);
                break;
            }
        default:
            {
                internal_error("Invalid nodecl kind '%s'\n", ast_print_node_type(nodecl_get_kind(nodecl_name)));
            }
    }
    return NULL;
}

static void compute_nodecl_name_from_unqualified_id(AST unqualified_id, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    switch (ASTType(unqualified_id))
    {
        case AST_SYMBOL:
            {
                *nodecl_output = nodecl_make_cxx_dep_name_simple(
                        ASTText(unqualified_id), 
                        ASTFileName(unqualified_id), 
                        ASTLine(unqualified_id));
                break;
            }
        case AST_TEMPLATE_ID:
            {
                const char* name = ASTText(ASTSon0(unqualified_id));
                AST template_arguments = ASTSon1(unqualified_id);

                template_parameter_list_t* template_parameters = 
                    get_template_parameters_from_syntax(template_arguments, decl_context);

                if (template_parameters == NULL)
                {
                    *nodecl_output = nodecl_make_err_expr(
                            ASTFileName(unqualified_id), 
                            ASTLine(unqualified_id));
                    return;
                }

                *nodecl_output = nodecl_make_cxx_dep_template_id(
                        nodecl_make_cxx_dep_name_simple(
                            name,
                            ASTFileName(unqualified_id), 
                            ASTLine(unqualified_id)),
                        template_parameters,
                        ASTFileName(unqualified_id), 
                        ASTLine(unqualified_id));
                break;
            }
        case AST_OPERATOR_FUNCTION_ID:
            {
                const char* name = 
                        get_operator_function_name(unqualified_id);
                *nodecl_output = nodecl_make_cxx_dep_name_simple(
                        name,
                        ASTFileName(unqualified_id), 
                        ASTLine(unqualified_id));
                break;
            }
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE:
            {
                const char* name = 
                        get_operator_function_name(unqualified_id);

                AST template_arguments = ASTSon1(unqualified_id);
                template_parameter_list_t* template_parameters = 
                    get_template_parameters_from_syntax(template_arguments, decl_context);

                if (template_parameters == NULL)
                {
                    *nodecl_output = nodecl_make_err_expr(
                            ASTFileName(unqualified_id), 
                            ASTLine(unqualified_id));
                    return;
                }

                *nodecl_output = nodecl_make_cxx_dep_template_id(
                        nodecl_make_cxx_dep_name_simple(
                            name,
                            ASTFileName(unqualified_id), 
                            ASTLine(unqualified_id)),
                        template_parameters,
                        ASTFileName(unqualified_id), 
                        ASTLine(unqualified_id));
                break;
            }
        case AST_CONVERSION_FUNCTION_ID:
            {
                *nodecl_output = nodecl_make_cxx_dep_name_conversion(
                        ASTFileName(unqualified_id), 
                        ASTLine(unqualified_id));
                // This is ugly but we need to keep the original tree around before lowering it into nodecl
                AST conversion_type_id = ast_copy(ASTSon0(unqualified_id));
                ast_set_child(nodecl_get_ast(*nodecl_output), 0, conversion_type_id);
                break;
            }
        case AST_DESTRUCTOR_ID:
            {
                AST symbol = ASTSon0(unqualified_id);
                *nodecl_output = nodecl_make_cxx_dep_name_simple(
                        ASTText(symbol), 
                        ASTFileName(unqualified_id), 
                        ASTLine(unqualified_id));
                break;
            }
        case AST_DESTRUCTOR_TEMPLATE_ID :
            {
                internal_error("Not supported yet", 0);
                // AST symbol = ASTSon0(expression);
                // AST template_args 
                // *nodecl_output = nodecl_make_cxx_dep_name_simple(
                //         nodecl_null(),
                //         ASTText(symbol), 
                //         ASTFileName(unqualified_id), 
                //         ASTLine(unqualified_id));
                break;
            }
        default:
            {
                internal_error("Unexpected tree of type '%s'\n", ast_print_node_type(ASTType(unqualified_id)));
            }
    }
}

void compute_nodecl_name_from_nested_part(AST nested_part,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_nested = nodecl_null();
    AST nested_it = nested_part;
    while (nested_it != NULL)
    {
        nodecl_t current = nodecl_null();
        AST nested_name = ASTSon0(nested_it);
        compute_nodecl_name_from_unqualified_id(nested_name, 
                decl_context,
                &current);

        if (nodecl_is_err_expr(current))
        {
            *nodecl_output = nodecl_make_err_expr(ASTFileName(nested_part), ASTLine(nested_part));
            return;
        }

        nodecl_nested = nodecl_append_to_list(nodecl_nested, current);
        nested_it = ASTSon1(nested_it);
    }
    *nodecl_output = nodecl_nested;
}

void compute_nodecl_name_from_nested_name(AST nested_part, 
        AST unqualified_part, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_nested = nodecl_null();
    compute_nodecl_name_from_nested_part(nested_part, decl_context, &nodecl_nested);

    if (nodecl_is_err_expr(nodecl_nested))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(nested_part), ASTLine(nested_part));
        return;
    }

    nodecl_t nodecl_unqualified = nodecl_null();
    compute_nodecl_name_from_unqualified_id(unqualified_part, decl_context, &nodecl_unqualified);

    if (nodecl_is_err_expr(nodecl_unqualified))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(unqualified_part), ASTLine(unqualified_part));
        return;
    }

    nodecl_nested = nodecl_append_to_list(nodecl_nested, nodecl_unqualified);

    *nodecl_output = nodecl_make_cxx_dep_name_nested(nodecl_nested,
            ASTFileName(unqualified_part), 
            ASTLine(unqualified_part));
}

void compute_nodecl_name_from_qualified_name(AST global_op, AST nested_name_spec, AST unqualified_id, decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    if (global_op != NULL
            || nested_name_spec != NULL)
    {
        nodecl_t nodecl_nested = nodecl_null();
        compute_nodecl_name_from_nested_part(nested_name_spec, decl_context, &nodecl_nested);

        if (!nodecl_is_null(nodecl_nested)
                && nodecl_is_err_expr(nodecl_nested))
        {
            *nodecl_output = nodecl_make_err_expr(ASTFileName(nested_name_spec), ASTLine(nested_name_spec));
            return;
        }

        nodecl_t nodecl_unqualified = nodecl_null();
        compute_nodecl_name_from_unqualified_id(unqualified_id, decl_context, &nodecl_unqualified);

        if (nodecl_is_err_expr(nodecl_unqualified))
        {
            *nodecl_output = nodecl_make_err_expr(ASTFileName(unqualified_id), ASTLine(unqualified_id));
            return;
        }

        nodecl_nested = nodecl_append_to_list(nodecl_nested, nodecl_unqualified);

        nodecl_t (*nodecl_nested_fun)(nodecl_t, const char*, int) = nodecl_make_cxx_dep_name_nested;
        if (global_op != NULL)
        {
            nodecl_nested_fun = nodecl_make_cxx_dep_global_name_nested;
        }

        *nodecl_output = nodecl_nested_fun(nodecl_nested, ASTFileName(unqualified_id), ASTLine(unqualified_id));
    }
    else
    {
        compute_nodecl_name_from_unqualified_id(unqualified_id, decl_context, nodecl_output);
    }
}

void compute_nodecl_name_from_id_expression(AST id_expression, decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    switch (ASTType(id_expression))
    {
        case AST_QUALIFIED_ID:
        case AST_QUALIFIED_TEMPLATE:
            {
                AST global_op = ASTSon0(id_expression);
                AST nested_name_spec = ASTSon1(id_expression);
                AST unqualified_id = ASTSon2(id_expression);

                compute_nodecl_name_from_qualified_name(global_op, nested_name_spec, unqualified_id, decl_context, nodecl_output);
                break;
            }
        default:
            {
                compute_nodecl_name_from_unqualified_id(id_expression, decl_context, nodecl_output);
            }
    }
}

scope_entry_list_t* class_context_lookup(decl_context_t decl_context, 
        decl_flags_t decl_flags, const char* name)
{
    ERROR_CONDITION(decl_context.current_scope->kind != CLASS_SCOPE, "This is not a class scope", 0);

    return query_in_class(decl_context.current_scope, name, decl_flags, "(null)", 0);
}

scope_entry_list_t* query_dependent_entity_in_context(decl_context_t decl_context,
        scope_entry_t* dependent_entity,
        const char* filename,
        int line)
{
    ERROR_CONDITION(dependent_entity->kind != SK_DEPENDENT_ENTITY, "Invalid symbol", 0);

    scope_entry_t* dependent_entry = NULL;
    nodecl_t dependent_parts = nodecl_null();

    dependent_typename_get_components(dependent_entity->type_information, &dependent_entry, &dependent_parts);

    switch (dependent_entry->kind)
    {
        case SK_CLASS:
        case SK_TYPEDEF:
        case SK_TEMPLATE_TYPE_PARAMETER:
            {
                type_t* new_class_type = update_type_for_instantiation(get_user_defined_type(dependent_entry),
                        decl_context,
                        filename,
                        line);

                if (is_dependent_type(new_class_type))
                {
                    scope_entry_t* new_sym = counted_calloc(1, sizeof(*new_sym), &_bytes_used_scopes);
                    new_sym->kind = SK_DEPENDENT_ENTITY;
                    new_sym->file = filename;
                    new_sym->line = line;
                    new_sym->symbol_name = dependent_entity->symbol_name;
                    new_sym->decl_context = decl_context;
                    new_sym->type_information = get_dependent_typename_type_from_parts(
                            named_type_get_symbol(new_class_type),
                            dependent_parts);

                    return entry_list_new(new_sym);
                }
                else if (!is_class_type(new_class_type))
                {
                    if (!checking_ambiguity())
                    {
                        error_printf("%s:%d: error: '%s' does not name a class type\n",
                                filename, line,
                                print_type_str(dependent_entity->type_information, dependent_entity->decl_context));
                    }
                    return NULL;
                }
                else
                {
                    if (!nodecl_is_null(dependent_parts))
                    {
                        scope_entry_t* class_sym = named_type_get_symbol(new_class_type);

                        if (class_type_is_incomplete_independent(class_sym->type_information))
                        {
                            instantiate_template_class(class_sym, class_sym->decl_context,
                                    filename, line);
                        }

                        return query_nodecl_name_flags(
                                class_type_get_inner_context(class_sym->type_information),
                                dependent_parts, DF_DEPENDENT_TYPENAME);
                    }
                    else
                    {
                        return entry_list_new(named_type_get_symbol(new_class_type));
                    }
                }

                break;
            }
        default:
            {
                internal_error("Invalid symbol kind\n", 0);
            }
    }

    return NULL;
}
