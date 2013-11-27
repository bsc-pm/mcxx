/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
        const locus_t* locus);

template_parameter_list_t* duplicate_template_argument_list(template_parameter_list_t* template_parameters)
{
    ERROR_CONDITION(template_parameters == NULL, "Template parameters cannot be NULL here", 0);

    template_parameter_list_t* result = counted_xcalloc(1, sizeof(*result), &_bytes_used_scopes);

    *result = *template_parameters;
    result->arguments = counted_xcalloc(template_parameters->num_parameters, sizeof(*result->arguments), &_bytes_used_scopes);

    int i;
    for (i = 0; i < result->num_parameters; i++)
    {
        // Copy pointers
        result->arguments[i] = template_parameters->arguments[i];
    }
    result->is_explicit_specialization = template_parameters->is_explicit_specialization;

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
    scope_t* result = counted_xcalloc(1, sizeof(*result), &_bytes_used_scopes);

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
        = counted_xcalloc(1, sizeof(*global_scope_namespace), &_bytes_used_scopes);
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

    ERROR_CONDITION(class_entry->kind != SK_CLASS && class_entry->kind != SK_ENUM, "This is not a class or enum", 0);

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

static const char* scope_names[] =
{
    [UNDEFINED_SCOPE] = "UNDEFINED_SCOPE",
    [NAMESPACE_SCOPE] = "NAMESPACE_SCOPE",
    [FUNCTION_SCOPE] = "FUNCTION_SCOPE",
    [PROTOTYPE_SCOPE] = "PROTOTYPE_SCOPE",
    [BLOCK_SCOPE] = "BLOCK_SCOPE",
    [CLASS_SCOPE] = "CLASS_SCOPE",
};

scope_entry_t* new_symbol(decl_context_t decl_context, scope_t* sc, const char* name)
{
    ERROR_CONDITION(name == NULL ||
            *name == '\0', "New symbol called with an empty or NULL string", 0);

    scope_entry_t* result;

    result = counted_xcalloc(1, sizeof(*result), &_bytes_used_symbols);
    result->symbol_name = uniquestr(name);
    // Remember, for template parameters, .current_scope will not contain
    // its declaration scope but will be in .template_scope
    result->decl_context = decl_context;

    result->extended_data = counted_xcalloc(1, sizeof(*(result->extended_data)), &_bytes_used_symbols);
    extensible_struct_init(&result->extended_data);
    insert_alias(sc, result, result->symbol_name);

    return result;
}

char same_scope(scope_t* stA, scope_t* stB)
{
    return (stA->hash == stB->hash);
}

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
    rb_tree_insert(sc->hash, entry->symbol_name, entry_list_remove(entry_list, entry));
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

static scope_entry_list_t* filter_friend_declared(scope_entry_list_t* entry_list)
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
    nodecl_t nodecl_name = nodecl_make_cxx_dep_name_simple(unqualified_name, make_locus("", 0, 0));

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
        const locus_t* locus);
static scope_entry_list_t* query_in_class(scope_t* current_class_scope, 
        const char* name, decl_flags_t decl_flags,
        const locus_t* locus);

static void build_dependent_parts_for_symbol_rec(
        scope_entry_t* entry,
        const locus_t* locus,
        scope_entry_t** dependent_entry,
        nodecl_t* nodecl_output);

static char class_has_dependent_bases(scope_entry_t* class_symbol)
{
    ERROR_CONDITION(class_symbol->kind != SK_CLASS, "Invalid symbol", 0);
    int num_bases = class_type_get_num_bases(class_symbol->type_information);
    int i;
    for (i = 0; i < num_bases; i++)
    {
        char current_base_is_virtual = 0;
        char current_base_is_dependent = 0;
        char current_base_is_expansion = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        class_type_get_base_num(class_symbol->type_information, i,
                &current_base_is_virtual,
                &current_base_is_dependent,
                &current_base_is_expansion,
                &access_specifier);

        if (current_base_is_dependent)
            return 1;
    }

    return 0;
}

char symbol_is_member_of_dependent_class(scope_entry_t* entry)
{
    return entry->entity_specs.is_member 
        && is_dependent_type(entry->entity_specs.class_type);
}

char symbol_is_local_of_dependent_function(scope_entry_t* entry)
{
    return entry->decl_context.current_scope->kind == BLOCK_SCOPE
        && entry->decl_context.current_scope->related_entry->kind == SK_FUNCTION
        && (is_dependent_type(entry->decl_context.current_scope->related_entry->type_information)
                || symbol_is_member_of_dependent_class(entry));
}

scope_entry_t* get_function_or_class_where_symbol_depends(scope_entry_t* entry)
{
    if (symbol_is_member_of_dependent_class(entry))
    {
        return named_type_get_symbol(entry->entity_specs.class_type);
    }
    else if (symbol_is_local_of_dependent_function(entry))
    {
        return entry->decl_context.current_scope->related_entry;
    }
    else
    {
        internal_error("This symbol is not in a dependent class or function\n", 0);
    }
}

scope_entry_t* class_symbol_get_canonical_symbol(scope_entry_t* class_symbol)
{
    ERROR_CONDITION(class_symbol->kind != SK_CLASS, "Invalid symbol", 0);

    if (class_symbol->entity_specs.alias_to != NULL)
        return class_symbol->entity_specs.alias_to;

    return class_symbol;
}

char class_is_in_lexical_scope(decl_context_t decl_context, 
        scope_entry_t* class_symbol)
{
    ERROR_CONDITION(class_symbol->kind != SK_CLASS, "Invalid symbol", 0);
    if (class_symbol->entity_specs.is_injected_class_name)
    {
        class_symbol = named_type_get_symbol(class_symbol->entity_specs.class_type);
    }

    if (decl_context.class_scope == NULL)
        return 0;

    scope_entry_t* class_in_scope = decl_context.class_scope->related_entry;

    if (class_symbol_get_canonical_symbol(class_symbol) 
            == class_symbol_get_canonical_symbol(class_in_scope))
    {
        return 1;
    }
    else
    {
        type_t* enclosing_class = class_type_get_enclosing_class_type(class_in_scope->type_information);
        if (enclosing_class != NULL)
        {
            decl_context_t class_context = class_type_get_inner_context(enclosing_class);
            return class_is_in_lexical_scope(class_context, class_symbol);
        }
    }

    return 0;
}

static scope_entry_t* create_new_dependent_entity(
        decl_context_t decl_context,
        scope_entry_t* dependent_entry,
        int nested_name_index,
        int nested_name_size,
        const locus_t* locus,
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
                locus, &updated_dependent_entry, &nodecl_list);
    }

    if (updated_dependent_entry == NULL)
        updated_dependent_entry = dependent_entry;

    int i;
    for (i = nested_name_index + 1; i < nested_name_size; i++)
    {
        nodecl_list = nodecl_append_to_list(nodecl_list, parts[i]);
    }

    nodecl_t nodecl_parts = nodecl_make_cxx_dep_name_nested(nodecl_list, make_locus("", 0, 0));

    // FIXME - Cache these symbols
    scope_entry_t* result = counted_xcalloc(1, sizeof(*result), &_bytes_used_scopes);

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
        const locus_t* locus)
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
        // Check bases if not found in the current class
        int num_all_bases = class_type_get_num_bases(current_class_type);

        int num_nondependent_bases = 0;
        for (i = 0; i < num_all_bases; i++)
        {
            char current_base_is_virtual = 0;
            char current_base_is_dependent = 0;
            char current_base_is_expansion = 0;
            access_specifier_t access_specifier = AS_UNKNOWN;
            /* scope_entry_t* base_class_entry = */ class_type_get_base_num(current_class_type, i, 
                    &current_base_is_virtual,
                    &current_base_is_dependent,
                    &current_base_is_expansion,
                    &access_specifier);

            if (current_base_is_dependent)
                continue;

            num_nondependent_bases++;
        }
        // Combine results from bases
        if (num_nondependent_bases == 0)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: No bases and not found in current class\n");
            }
            // No results :)
            derived->entry_list = NULL;
        }
        else
        {
            class_scope_lookup_t bases_lookup[num_nondependent_bases + 1];
            memset(bases_lookup, 0, sizeof(bases_lookup));

            int num_bases = 0;
            for (i = 0; i < num_all_bases; i++)
            {
                char current_base_is_virtual = 0;
                char current_base_is_dependent = 0;
                char current_base_is_expansion = 0;
                access_specifier_t access_specifier = AS_UNKNOWN;
                scope_entry_t* base_class_entry = class_type_get_base_num(current_class_type, i, 
                        &current_base_is_virtual,
                        &current_base_is_dependent,
                        &current_base_is_expansion,
                        &access_specifier);

                if (current_base_is_dependent)
                    continue;

                type_t* base_class_type = get_actual_class_type(base_class_entry->type_information);
                decl_context_t base_class_context = class_type_get_inner_context(base_class_type);
                scope_t* base_class_scope = base_class_context.current_scope;

                // Get the info from the base
                class_scope_lookup_t current_base_info = *derived;

                // Lookup in the base
                class_scope_lookup_rec(base_class_scope, name, &current_base_info,
                        current_base_is_virtual, /* initial_lookup */ 0, decl_flags,
                        locus);

                // If the result is valid, add it
                if (current_base_info.entry_list != NULL)
                {
                    bases_lookup[num_bases] = current_base_info;
                    num_bases++;
                }
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

            // Combine results from bases
            if (num_bases == 1)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Not found in current class but in only one (possibly indirect) base\n");
                }

                // Only a single base
                *derived = bases_lookup[0];
            }
            else if (num_bases > 1)
            {
                // Merge must be done. Create a candidate result based on the first
                // and check if they refer to the same subobject (or static base
                // class)
                // Create the candidate, the first non null

                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Name found in two or more bases, will have to check if they come from the same subobject\n");
                }

                char valid = 1;
                int index_candidate = 0;
                *derived = bases_lookup[0];

                char finished = 0;
                char several_subobjects = 0;
                for (i = 0; (i < num_bases) && !finished && valid; i++)
                {
                    if (index_candidate == i)
                        continue;

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "SCOPE: Checking entity '%s' found to come from different base classes\n",
                                name);
                    }
                    class_scope_lookup_t *current = &(bases_lookup[i]);


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
                                        fprintf(stderr, "SCOPE: One of the entities is not static "
                                                "so lookup of '%s' is not valid\n", 
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
}

nodecl_t nodecl_name_get_last_part(nodecl_t nodecl_name)
{
    if (nodecl_get_kind(nodecl_name) == NODECL_CXX_DEP_GLOBAL_NAME_NESTED
            || nodecl_get_kind(nodecl_name) == NODECL_CXX_DEP_NAME_NESTED)
    {
        int num_items = 0;
        nodecl_t* list = nodecl_unpack_list(nodecl_get_child(nodecl_name, 0), &num_items);

        nodecl_t last_part = list[num_items - 1];

        xfree(list);

        return last_part;
    }

    return nodecl_name;
}

char nodecl_name_ends_in_template_id(nodecl_t nodecl_name)
{
    return (nodecl_get_kind(nodecl_name_get_last_part(nodecl_name)) == NODECL_CXX_DEP_TEMPLATE_ID);
}

template_parameter_list_t* nodecl_name_get_last_template_arguments(nodecl_t nodecl_name)
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
        const locus_t* locus,
        scope_entry_t** dependent_entry,
        nodecl_t* nodecl_output)
{
    ERROR_CONDITION(entry->kind != SK_CLASS && entry->kind != SK_FUNCTION, "Invalid symbol", 0);
    type_t* enclosing = NULL;

    if (entry->kind == SK_CLASS)
    {
        enclosing = class_type_get_enclosing_class_type(entry->type_information);
    }

    if (enclosing != NULL
            && is_dependent_type(enclosing))
    {
        nodecl_t nodecl_prev = nodecl_null();

        build_dependent_parts_for_symbol_rec(named_type_get_symbol(enclosing), locus, dependent_entry, &nodecl_prev);

        if (!entry->entity_specs.is_anonymous_union)
        {
            template_parameter_list_t* template_arguments = NULL;
            if (is_template_specialized_type(entry->type_information))
            {
                template_arguments = template_specialized_type_get_template_arguments(entry->type_information);
            }

            nodecl_t nodecl_current = nodecl_make_cxx_dep_name_simple(entry->symbol_name, locus);
            if (template_arguments != NULL)
            {
                // If our enclosing is dependent, we need a 'template '
                nodecl_current = nodecl_make_cxx_dep_template_id(nodecl_current, "template ", template_arguments, locus);
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
            *nodecl_output = nodecl_prev;
        }
    }
    else
    {
        ERROR_CONDITION(entry->entity_specs.is_anonymous_union, 
                "Unexpected case of anonymous union dependent that is not enclosed in any class", 0);

        *dependent_entry = entry;
        *nodecl_output = nodecl_null();
    }
}

type_t* build_dependent_typename_for_entry(
        scope_entry_t* class_symbol,
        nodecl_t nodecl_name,
        const locus_t* locus)
{
    nodecl_t nodecl_prev = nodecl_null();
    scope_entry_t* dependent_entry = NULL;
    build_dependent_parts_for_symbol_rec(class_symbol,
            locus, &dependent_entry, &nodecl_prev);

    nodecl_t nodecl_last = nodecl_name_get_last_part(nodecl_name);

    template_parameter_list_t* template_arguments = NULL;

    nodecl_t nodecl_current = nodecl_null(); 

    const char* template_tag = "";
    if (nodecl_get_kind(nodecl_last) == NODECL_CXX_DEP_TEMPLATE_ID)
    {
        template_tag = nodecl_get_text(nodecl_last);
        template_arguments = nodecl_get_template_parameters(nodecl_name);
        nodecl_current = nodecl_shallow_copy(nodecl_get_child(nodecl_name, 0));
    }
    else
    {
        nodecl_current = nodecl_shallow_copy(nodecl_name);
    }

    if (template_arguments != NULL)
    {
        nodecl_current = nodecl_make_cxx_dep_template_id(nodecl_current, template_tag, template_arguments, locus);
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
    dependent_parts = nodecl_make_cxx_dep_name_nested(dependent_parts, locus);

    type_t* result = get_dependent_typename_type_from_parts(dependent_entry, dependent_parts);

    return result;
}

static scope_entry_list_t* query_in_class(scope_t* current_class_scope, 
        const char* name, 
        decl_flags_t decl_flags,
        const locus_t* locus)
{
    class_scope_lookup_t result;
    memset(&result, 0, sizeof(result));

    class_scope_lookup_rec(current_class_scope, name, &result, 0, /* initial_lookup */ 1, decl_flags, locus);

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

static void error_ambiguity(scope_entry_list_t* entry_list, const locus_t* locus)
{
    fprintf(stderr, "%s: error: ambiguity in reference to '%s'\n", locus_to_str(locus),
            entry_list_head(entry_list)->symbol_name);
    fprintf(stderr, "%s: info: candidates are\n", locus_to_str(locus));

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        fprintf(stderr, "%s: info:    %s\n", 
                locus_to_str(entry->locus),
                get_qualified_symbol_name(entry, entry->decl_context));
    }
    entry_list_iterator_free(it);

    running_error("%s: error: lookup failed due to ambiguous reference '%s'\n", 
            locus_to_str(locus), entry_list_head(entry_list)->symbol_name);
}


static void check_for_naming_ambiguity(scope_entry_list_t* entry_list, const locus_t* locus)
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
            hiding_name = entry;
        }
        // Bit special case: two or more typedefs sharing the same name and type
        // Example:
        //      namespace N {
        //          typedef unsigned int size_t;
        //      }
        //      using namespace N;
        //      typedef unsigned int size_t;
        //
        //      size_t var;
        //
        else if (entry->kind == SK_TYPEDEF
                && (hiding_name == NULL
                    || (hiding_name->kind == SK_TYPEDEF
                        && equivalent_types(hiding_name->type_information, entry->type_information))))
        {
            hiding_name = entry;
        }
        else
        {
            error_ambiguity(entry_list, locus);
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

char scope_is_enclosed_by(const scope_t *const scope, const scope_t *const potential_enclosing)
{
    const scope_t* it = scope;

    while (it != NULL)
    {
        if (it == potential_enclosing)
        {
            return 1;
        }
        it = it->contained_in;
    }
    return 0;
}

static scope_entry_list_t* unqualified_query_in_namespace(
        scope_entry_t* namespace,
        const char* name,
        decl_flags_t decl_flags,
        int num_associated_namespaces, 
        associated_namespace_t* associated_namespaces,
        const locus_t* locus)
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

            // Are we looking for an elaborated class specifier?
            if (BITMAP_TEST(decl_flags, DF_STRUCT)
                    || BITMAP_TEST(decl_flags, DF_CLASS)
                    || BITMAP_TEST(decl_flags, DF_UNION))
            {
                enum cxx_symbol_kind type_filter[] = {
                    SK_CLASS,
                    SK_TYPEDEF,
                    SK_TEMPLATE_TYPE_PARAMETER,
                    SK_TEMPLATE_TEMPLATE_PARAMETER,
                    SK_TEMPLATE,
                    SK_GCC_BUILTIN_TYPE
                };

                scope_entry_list_t* old_result = grand_result;
                grand_result = filter_symbol_kind_set(old_result, STATIC_ARRAY_LENGTH(type_filter), type_filter);
                entry_list_free(old_result);
            }

            // Are we looking for an elaborated enum specifier?
            if (BITMAP_TEST(decl_flags, DF_ENUM))
            {
                enum cxx_symbol_kind type_filter[] = {
                    SK_ENUM,
                    SK_TYPEDEF,
                    SK_TEMPLATE_TYPE_PARAMETER,
                    SK_TEMPLATE_TEMPLATE_PARAMETER,
                    SK_TEMPLATE,
                    SK_GCC_BUILTIN_TYPE
                };

                scope_entry_list_t* old_result = grand_result;
                grand_result = filter_symbol_kind_set(old_result, STATIC_ARRAY_LENGTH(type_filter), type_filter);
                entry_list_free(old_result);
            }

            check_for_naming_ambiguity(grand_result, locus);
        }
    }
    return grand_result;
}

static scope_entry_list_t* qualified_query_in_namespace_rec(scope_entry_t* namespace, 
        const char* name, 
        decl_flags_t decl_flags,
        const locus_t* locus,
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
                locus,
                num_visited_namespaces,
                visited_namespaces);

        scope_entry_list_t* old_grand_result = grand_result;
        grand_result = entry_list_merge_aliases(old_grand_result, result);
        entry_list_free(old_grand_result);
        entry_list_free(result);

        // Are we looking for an elaborated class specifier?
        if (BITMAP_TEST(decl_flags, DF_STRUCT)
                || BITMAP_TEST(decl_flags, DF_CLASS)
                || BITMAP_TEST(decl_flags, DF_UNION))
        {
            enum cxx_symbol_kind type_filter[] = {
                SK_CLASS,
                SK_TYPEDEF,
                SK_TEMPLATE_TYPE_PARAMETER,
                SK_TEMPLATE_TEMPLATE_PARAMETER,
                SK_TEMPLATE,
                SK_GCC_BUILTIN_TYPE
            };

            scope_entry_list_t* old_result = grand_result;
            grand_result = filter_symbol_kind_set(old_result, STATIC_ARRAY_LENGTH(type_filter), type_filter);
            entry_list_free(old_result);
        }

        // Are we looking for an elaborated enum specifier?
        if (BITMAP_TEST(decl_flags, DF_ENUM))
        {
            enum cxx_symbol_kind type_filter[] = {
                SK_ENUM,
                SK_TYPEDEF,
                SK_TEMPLATE_TYPE_PARAMETER,
                SK_TEMPLATE_TEMPLATE_PARAMETER,
                SK_TEMPLATE,
                SK_GCC_BUILTIN_TYPE
            };

            scope_entry_list_t* old_result = grand_result;
            grand_result = filter_symbol_kind_set(old_result, STATIC_ARRAY_LENGTH(type_filter), type_filter);
            entry_list_free(old_result);
        }

        check_for_naming_ambiguity(grand_result, locus);
    }

    return grand_result;
}

static scope_entry_list_t* qualified_query_in_namespace(scope_entry_t* namespace, 
        const char* name, 
        decl_flags_t decl_flags,
        const locus_t* locus)
{
    scope_entry_t* visited_namespaces[MCXX_MAX_ASSOCIATED_NAMESPACES];

    scope_entry_list_t* result = qualified_query_in_namespace_rec(namespace, name, decl_flags, 
            locus, 
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
        const locus_t* locus)
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

            if (tpl != NULL // This could be null because of variadic template arguments
                    && tpl->entry != NULL
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
                        locus);
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
                    name,
                    decl_flags,
                    num_associated_namespaces,
                    associated_namespaces, 
                    locus);
        }
        else // BLOCK_SCOPE || PROTOTYPE_SCOPE || FUNCTION_SCOPE (although its contains should be NULL)
        {
            result = query_name_in_scope(current_scope, name);
        }


        if (BITMAP_TEST(decl_flags, DF_IGNORE_FRIEND_DECL))
        {
            scope_entry_list_t* old_result = result;
            result = filter_friend_declared(result);
            entry_list_free(old_result);
        }

        if (BITMAP_TEST(decl_flags, DF_STRUCT)
                || BITMAP_TEST(decl_flags, DF_CLASS)
                || BITMAP_TEST(decl_flags, DF_UNION))
        {
            enum cxx_symbol_kind type_filter[] = {
                SK_CLASS,
                SK_TYPEDEF,
                SK_TEMPLATE_TYPE_PARAMETER,
                SK_TEMPLATE_TEMPLATE_PARAMETER,
                SK_TEMPLATE,
                SK_GCC_BUILTIN_TYPE
            };

            scope_entry_list_t* old_result = result;
            result = filter_symbol_kind_set(old_result, STATIC_ARRAY_LENGTH(type_filter), type_filter);
            entry_list_free(old_result);
        }

        if (BITMAP_TEST(decl_flags, DF_ENUM))
        {
            enum cxx_symbol_kind type_filter[] = {
                SK_ENUM,
                SK_TYPEDEF,
                SK_TEMPLATE_TYPE_PARAMETER,
                SK_TEMPLATE_TEMPLATE_PARAMETER,
                SK_TEMPLATE,
                SK_GCC_BUILTIN_TYPE
            };

            scope_entry_list_t* old_result = result;
            result = filter_symbol_kind_set(old_result, STATIC_ARRAY_LENGTH(type_filter), type_filter);
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
		decl_context_t decl_context,
        int pack_index)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Updating expression '%s'\n", 
                codegen_to_str(nodecl, decl_context));
    }

    nodecl_t nodecl_output = nodecl_null();

    ERROR_CONDITION(!(pack_index < 0), "Pack expansion inside expressions not yet implemented", 0);

    nodecl_t nodecl_inst = instantiate_expression(nodecl, decl_context);

    if (nodecl_is_list(nodecl_inst))
    {
        int num_items;
        nodecl_t* list = nodecl_unpack_list(nodecl_inst, &num_items);

        int i;
        for (i = 0; i < num_items; i++)
        {
            nodecl_t nodecl_item = nodecl_null();
            check_nodecl_nontype_template_argument_expression(list[i],
                    decl_context,
                    &nodecl_item);
            if (nodecl_is_err_expr(nodecl_item))
            {
                xfree(list);
                return nodecl_item;
            }
            list[i] = nodecl_item;
        }

        nodecl_output = nodecl_make_list_n(num_items, list);
        xfree(list);
    }
    else
    {
        check_nodecl_nontype_template_argument_expression(nodecl_inst,
                decl_context,
                &nodecl_output);
    }

    return nodecl_output;
}

static nodecl_t update_nodecl_constant_expression(nodecl_t nodecl,
		decl_context_t decl_context,
        int pack_index)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Updating expression '%s'\n", 
                codegen_to_str(nodecl, decl_context));
    }
    nodecl = instantiate_expression_with_pack_index(nodecl, decl_context, pack_index);

    if (!nodecl_is_constant(nodecl)
            && !nodecl_expr_is_value_dependent(nodecl))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: expression '%s' is not constant\n",
                    nodecl_locus_to_str(nodecl),
                    codegen_to_str(nodecl, decl_context));
        }
    }

    return nodecl;
}

char template_parameter_kind_is_pack(enum template_parameter_kind k)
{
    return k == TPK_NONTYPE_PACK
        || k == TPK_TYPE_PACK
        || k == TPK_TEMPLATE_PACK;
}

char template_argument_is_pack(template_parameter_value_t* value)
{
    return (((value->kind == TPK_TYPE
                    || value->kind == TPK_TEMPLATE)
                && is_pack_type(value->type))
            || (value->kind == TPK_NONTYPE
                && nodecl_get_kind(value->value) == NODECL_CXX_VALUE_PACK));
}

static type_t* update_type_aux_(type_t* orig_type, 
        decl_context_t decl_context,
        const locus_t* locus,
        int pack_index);

static template_parameter_value_t* update_template_parameter_value_aux(
        template_parameter_value_t* v,
        decl_context_t decl_context,
        char is_template_class,
        const locus_t* locus,
        int pack_index)
{
    template_parameter_value_t* result = counted_xcalloc(1, sizeof(*result), &_bytes_used_scopes);
    result->kind = v->kind;
    result->is_default = 0;

    type_t* updated_type = update_type_aux_(v->type, decl_context, locus, pack_index);
    if (updated_type == NULL)
    {
        return NULL;
    }

    if (is_template_class)
    {
        updated_type = advance_over_typedefs(updated_type);
    }

    result->type = updated_type;

    if (result->kind == TPK_NONTYPE)
    {
        if (!nodecl_is_null(v->value))
        {
            if (nodecl_is_list(v->value))
            {
                int num_items;
                nodecl_t* list = nodecl_unpack_list(v->value, &num_items);
                int i;
                nodecl_t updated_list = nodecl_null();
                for (i = 0; i < num_items; list++)
                {
                    nodecl_t updated_expr =
                        update_nodecl_template_argument_expression(v->value, decl_context, pack_index);

                    if (nodecl_is_err_expr(updated_expr))
                    {
                        return NULL;
                    }

                    // Force, if possible the type of the expression
                    if (is_sequence_of_types(result->type)
                            && sequence_of_types_get_num_types(result->type) > i)
                    {
                        nodecl_set_type(updated_list, sequence_of_types_get_type_num(result->type, i));
                    }

                    updated_list = nodecl_append_to_list(updated_list, updated_expr);
                }
            }
            else
            {
                result->value =
                    update_nodecl_template_argument_expression(v->value, decl_context, pack_index);

                if (nodecl_is_err_expr(result->value))
                {
                    return NULL;
                }
                // Force the type of the expression
                nodecl_set_type(result->value, result->type);
            }
        }
    }

    return result;
}

template_parameter_value_t* update_template_parameter_value_of_template_class(
        template_parameter_value_t* v,
        decl_context_t decl_context,
        const locus_t* locus,
        int pack_index)
{
    return update_template_parameter_value_aux(v, decl_context, /* is_template_class */ 1, locus, pack_index);
}

template_parameter_value_t* update_template_parameter_value(
        template_parameter_value_t* v,
        decl_context_t decl_context,
        const locus_t* locus,
        int pack_index)
{
    return update_template_parameter_value_aux(v, decl_context, /* is_template_class */ 0, locus, pack_index);
}

template_parameter_list_t* update_template_argument_list(
        decl_context_t decl_context,
        template_parameter_list_t* dependent_type_template_arguments,
        const locus_t* locus,
        int pack_index)
{
    template_parameter_list_t* result = duplicate_template_argument_list(dependent_type_template_arguments);

    int i;
    for (i = 0; i < result->num_parameters; i++)
    {
        result->arguments[i] = update_template_parameter_value(
                result->arguments[i],
                decl_context,
                locus,
                pack_index);

        if (result->arguments[i] == NULL)
            return NULL;
    }

    return result;
}

static nodecl_t update_dependent_typename_dependent_parts(
        nodecl_t dependent_parts,
        decl_context_t decl_context,
        const locus_t* locus,
        int pack_index)
{
    // We need to update dependent parts, lest there was a template-id
    int num_parts = 0;
    int i;
    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(dependent_parts, 0), &num_parts);
    nodecl_t new_dependent_parts_list = nodecl_null();
    for (i = 0; i < num_parts; i++)
    {
        nodecl_t new_current_part = nodecl_shallow_copy(list[i]);
        if (nodecl_get_kind(new_current_part) == NODECL_CXX_DEP_TEMPLATE_ID)
        {
            template_parameter_list_t* template_arguments
                = nodecl_get_template_parameters(new_current_part);

            template_parameter_list_t* new_template_arguments
                = update_template_argument_list(decl_context,
                        template_arguments,
                        locus,
                        pack_index);

            nodecl_set_template_parameters(new_current_part, new_template_arguments);
        }

        new_dependent_parts_list = nodecl_append_to_list(new_dependent_parts_list,
                new_current_part);
    }

    nodecl_t new_dependent_parts = nodecl_make_cxx_dep_name_nested(new_dependent_parts_list, locus);

    return new_dependent_parts;
}

static type_t* update_dependent_typename(
        type_t* dependent_entry_type,
        nodecl_t dependent_parts,
        decl_context_t decl_context,
        const locus_t* locus,
        int pack_index)
{
    ERROR_CONDITION(
            !nodecl_is_null(dependent_parts)
            && nodecl_get_kind(dependent_parts) != NODECL_CXX_DEP_NAME_NESTED, "Invalid tree", 0);

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

    instantiate_template_class_if_needed(current_member, current_member->decl_context, locus);

    nodecl_t new_dependent_parts = update_dependent_typename_dependent_parts(dependent_parts, decl_context, locus,
            pack_index);

    scope_entry_list_t* entry_list = query_nodecl_name(
            class_type_get_inner_context(current_member->type_information),
            new_dependent_parts);

    if (entry_list == NULL)
        return NULL;

    scope_entry_t* member = entry_list_head(entry_list);

    if (member->kind == SK_USING)
        member = member->entity_specs.alias_to;

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


static int get_length_of_pack_expansion_common(int num_packs_to_expand,
        scope_entry_t** packs_to_expand,
        decl_context_t decl_context,
        const locus_t* locus UNUSED_PARAMETER)
{
    ERROR_CONDITION(num_packs_to_expand == 0, "This should not happen", 0);

    int num_expanded_types = 0;
    type_t** expanded_types = NULL;

    int num_expanded_values = 0;
    nodecl_t* expanded_values = NULL;

    int i;
    // Now get their sequence types (if any)
    for (i = 0; i < num_packs_to_expand; i++)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Found pack '%s'\n",
                    packs_to_expand[i]->symbol_name);
        }
        if (packs_to_expand[i]->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                || packs_to_expand[i]->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
        {
            scope_entry_t* argument = lookup_of_template_parameter(
                    decl_context,
                    packs_to_expand[i]->entity_specs.template_parameter_nesting,
                    packs_to_expand[i]->entity_specs.template_parameter_position);
            if (argument != NULL && is_sequence_of_types(argument->type_information))
            {
                P_LIST_ADD(expanded_types, num_expanded_types, argument->type_information);
            }
        }
        else if (packs_to_expand[i]->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK)
        {
            scope_entry_t* argument = lookup_of_template_parameter(
                    decl_context,
                    packs_to_expand[i]->entity_specs.template_parameter_nesting,
                    packs_to_expand[i]->entity_specs.template_parameter_position);
            if (argument != NULL && nodecl_is_list(argument->value))
            {
                P_LIST_ADD(expanded_values, num_expanded_values, argument->value);
            }
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    char give_up = 0;
    int len = -1;
    for (i = 0; i < num_expanded_types && !give_up; i++)
    {
        int current_len = sequence_of_types_get_num_types(expanded_types[i]);
        if (len < 0)
        {
            len = current_len;
        }
        else if (len != current_len)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Length mismatch during expansion pack '%d' != '%d'\n",
                        len, current_len);
            }
            give_up = 1;
        }
    }
    for (i = 0; i < num_expanded_values && !give_up; i++)
    {
        int current_len = nodecl_list_length(expanded_values[i]);
        if (len < 0)
        {
            len = current_len;
        }
        else if (len != current_len)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Length mismatch during expansion pack '%d' != '%d'\n",
                        len, current_len);
            }
            give_up = 1;
        }
    }

    return len;
}

int get_length_of_pack_expansion_from_expression(nodecl_t expr,
        decl_context_t decl_context,
        const locus_t* locus)
{
    scope_entry_t** packs_to_expand = NULL;
    int num_packs_to_expand = 0;

    get_packs_in_expression(expr, &packs_to_expand, &num_packs_to_expand);

    int len = get_length_of_pack_expansion_common(num_packs_to_expand, packs_to_expand, decl_context, locus);

    xfree(packs_to_expand);

    return len;
}

int get_length_of_pack_expansion_from_type(type_t* pack_type,
        decl_context_t decl_context,
        const locus_t* locus)
{
    scope_entry_t** packs_to_expand = NULL;
    int num_packs_to_expand = 0;

    get_packs_in_type(pack_type_get_packed_type(pack_type), &packs_to_expand, &num_packs_to_expand);

    int len = get_length_of_pack_expansion_common(num_packs_to_expand, packs_to_expand, decl_context, locus);

    xfree(packs_to_expand);

    return len;
}

type_t* update_type_for_auto(type_t* t, type_t* template_parameter)
{
    if (is_auto_type(t))
    {
        return get_cv_qualified_type(template_parameter, get_cv_qualifier(t));
    }
    else if (is_pointer_type(t))
    {
        type_t* result = update_type_for_auto(pointer_type_get_pointee_type(t), template_parameter);
        return get_cv_qualified_type(
                get_pointer_type(result),
                get_cv_qualifier(t));
    }
    else if (is_lvalue_reference_type(t))
    {
        type_t* result = update_type_for_auto(reference_type_get_referenced_type(t), template_parameter);
        return get_cv_qualified_type(
                get_lvalue_reference_type(result),
                get_cv_qualifier(t));
    }
    else if (is_rvalue_reference_type(t))
    {
        type_t* result = update_type_for_auto(reference_type_get_referenced_type(t), template_parameter);
        return get_cv_qualified_type(
                get_rvalue_reference_type(result),
                get_cv_qualifier(t));
    }
    else if (is_array_type(t))
    {
        type_t* result = update_type_for_auto(array_type_get_element_type(t), template_parameter);
        return get_cv_qualified_type(
                get_array_type(result,
                    array_type_get_array_size_expr(t),
                    array_type_get_array_size_expr_context(t)),
                get_cv_qualifier(t));
    }
    else if (is_function_type(t))
    {
        type_t* return_type = update_type_for_auto(function_type_get_return_type(t), template_parameter);

        parameter_info_t *parameter_types = NULL;
        int num_parameter_types = 0;

        int last = function_type_get_num_parameters(t);

        char has_ellipsis = function_type_get_has_ellipsis(t);

        if (has_ellipsis)
            last--;

        int i;
        for (i = 0; i < last; i++)
        {
            type_t* param_orig_type = function_type_get_parameter_type_num(t, i);

            // Technically this should not happen, do it anyway
            param_orig_type = update_type_for_auto(param_orig_type, template_parameter);

            parameter_info_t parameter_info = get_parameter_info_for_type(param_orig_type);
            P_LIST_ADD(parameter_types, num_parameter_types, parameter_info);
        }

        type_t* updated_function_type = get_new_function_type(return_type,
                parameter_types, num_parameter_types, REF_QUALIFIER_NONE);

        return get_cv_qualified_type(updated_function_type,
                get_cv_qualifier(t));
    }
    else if (is_vector_type(t))
    {
        return get_cv_qualified_type(
                get_vector_type(
                    update_type_for_auto(vector_type_get_element_type(t), template_parameter),
                    vector_type_get_vector_size(t)),
                get_cv_qualifier(t));
    }
    else
    {
        return t;
    }
}


static type_t* update_pack_type(type_t* pack_type, decl_context_t decl_context, const locus_t* locus)
{
    // T... may be expanded to {T1, T2, T3, ...} or just be replaced by
    // {S...} in such case we have to assume that the updated type is S...
    // (not {S...})

    int len = get_length_of_pack_expansion_from_type(pack_type, decl_context, locus);
    if (len < 0)
        return NULL;

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Expansion pack length is '%d'\n", len);
    }

    type_t** types = NULL;
    types = xcalloc(len, sizeof(*types));
    int i;
    for (i = 0; i < len; i++)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Pack expansion of '%s' for item number %d\n",
                    print_declarator(pack_type), i);
        }
        types[i] = update_type_aux_(
                pack_type_get_packed_type(pack_type),
                decl_context,
                locus,
                i);
        types[i] = get_cv_qualified_type(types[i],
                // Add cv-qualification
                get_cv_qualifier(types[i])
                | get_cv_qualifier(pack_type_get_packed_type(pack_type)));
        if (types[i] == NULL)
        {
            xfree(types);
            return NULL;
        }
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Pack expanded in item %d expanded to '%s'\n",
                    i, print_declarator(types[i]));
        }
    }

    type_t* result = get_sequence_of_types(len, types);
    xfree(types);

    return result;
}

static type_t* update_type_aux_(type_t* orig_type, 
        decl_context_t decl_context,
        const locus_t* locus,
        // -1 if not expanding any pack
        int pack_index)
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
                || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                // template packs
                || entry->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
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
                        && argument->kind == SK_TEMPLATE_TYPE_PARAMETER)
            {
                // A type template parameter replaced by another one
                cv_qualifier_t cv_qualif = get_cv_qualifier(orig_type);
                return get_cv_qualified_type(get_user_defined_type(argument), cv_qualif);
            }
            else if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                    && argument->kind == SK_TYPEDEF)
            {
                // A type template parameter replaced by another type
                //
                // Note that we have to add the qualification
                //
                //   template <typename _T, typename _Q = volatile _T>
                //   struct A { };
                //
                //   A<const int>   ->   A<const int, const volatile int>
                cv_qualifier_t cv_qualif = get_cv_qualifier(orig_type);
                cv_qualif |= get_cv_qualifier(argument->type_information);
                return get_cv_qualified_type(argument->type_information, cv_qualif);
            }
            else if (entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                    && (argument->kind == SK_TEMPLATE
                        || argument->kind == SK_TEMPLATE_TEMPLATE_PARAMETER))
            {
                // A template template parameter replaced by a template-name (SK_TEMPLATE) or
                // another template template parameter (SK_TEMPLATE_TEMPLATE_PARAMETER)
                return get_user_defined_type(argument);
            }
            else if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                    && argument->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK)
            {
                // A type template parameter pack replaced by another template
                // type parameter pack
                ERROR_CONDITION(!is_pack_type(argument->type_information),
                        "This type should be a pack type but it is '%s'", print_declarator(argument->type_information));
                // FIXME - Should we augment the qualifier of the packed type as well?
                return get_user_defined_type(argument);
            }
            else if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                    && argument->kind == SK_TYPEDEF_PACK)
            {
                // A type template parameter pack replaced by a sequence of types
                ERROR_CONDITION(!is_sequence_of_types(argument->type_information),
                        "This type should be a sequence of types but it is '%s'", print_declarator(argument->type_information));
                // FIXME - We may need to add up the cv-qualifier to the
                // members of the sequence type
                if (pack_index < 0)
                {
                    // If we are not expanding. Return the sequence as a whole
                    return argument->type_information;
                }
                else
                {
                    // We are expanding a pack, return the requested index in
                    // the sequence
                    return sequence_of_types_get_type_num(
                            argument->type_information,
                            pack_index);
                }
            }
            // else if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
            //         && argument->kind == SK_TYPEDEF_PACK)
            // {
            //     // This happens when a type template argument expansion is being
            //     // used where a (nonpack) template type is expected. For this to be valid
            //     // the sequence should be of length 1, but it is the caller who sould
            //     // check this
            //     //
            //     // We return the whole sequence and let the caller deal with it
            //     return argument->type_information;
            // }
            else if (entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK
                    && argument->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
            {
                // A template-template parameter pack being replaced with
                // another template-template parameter pack
                ERROR_CONDITION(!is_pack_type(argument->type_information),
                        "This type should be a pack type but it is '%s'", print_declarator(argument->type_information));
                return argument->type_information;
            }
            else if (entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK
                    && argument->kind == SK_TEMPLATE_PACK)
            {
                // A template template parameter pack replaced by a sequence of templates
                ERROR_CONDITION(!is_sequence_of_types(argument->type_information),
                        "This type should be a sequence of types but it is '%s'", print_declarator(argument->type_information));
                // FIXME - We may need to add up the cv-qualifier to the
                // members of the sequence type
                if (pack_index < 0)
                {
                    // If we are not expanding. Return the sequence as a whole
                    return argument->type_information;
                }
                else
                {
                    // We are expanding a pack, return the requested index in
                    // the sequence
                    return sequence_of_types_get_type_num(
                            argument->type_information,
                            pack_index);
                }
            }
            else
            {
                internal_error("Wrong pair template-argument/template-parameter %s != %s",
                        symbol_kind_name(entry), symbol_kind_name(argument));
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

            template_parameter_list_t* current_template_arguments =
                template_specialized_type_get_template_arguments(entry->type_information);

            // First make an update for all the current template arguments
            template_parameter_value_t **updated_parameter_values = xcalloc(current_template_arguments->num_parameters, 
                    sizeof(*updated_parameter_values));

            int i;
            for (i = 0; i < current_template_arguments->num_parameters; i++)
            {
                updated_parameter_values[i] = update_template_parameter_value_of_template_class(
                        current_template_arguments->arguments[i],
                        decl_context, locus,
                        pack_index);

                if (updated_parameter_values[i] == NULL)
                {
                     DEBUG_CODE()
                     {
                         fprintf(stderr, "SCOPE: Update of template argument %d failed\n", i);
                     }
                     return NULL;
                }
            }

            // One template parameter might be a pack which forces us to assume the updated list is fine
            char there_are_pack_arguments = 0;
            for (i = 0; i < current_template_arguments->num_parameters && !there_are_pack_arguments; i++)
            {
                there_are_pack_arguments = (template_argument_is_pack(updated_parameter_values[i]));
            }

            if (there_are_pack_arguments)
            {
                // Repeat this code here because the code that expands the
                // template packs is very long
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: We found pack arguments, so we won't expand the template arguments\n");
                    fprintf(stderr, "SCOPE: Reasking for specialization\n");
                }
                template_parameter_list_t *updated_template_arguments = xcalloc(1, sizeof(*updated_template_arguments));
                updated_template_arguments->enclosing = current_template_arguments->enclosing;
                updated_template_arguments->is_explicit_specialization = current_template_arguments->is_explicit_specialization;
                updated_template_arguments->parameters = primary_template_parameters->parameters;
                updated_template_arguments->arguments = updated_parameter_values;
                updated_template_arguments->num_parameters = current_template_arguments->num_parameters;

                type_t* updated_specialized = 
                    template_type_get_specialized_type(template_type, 
                            updated_template_arguments, 
                            decl_context,
                            locus);
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

            int num_expanded_parameter_values = 0;
            template_parameter_value_t** expanded_parameter_values = NULL;
            int i_param = 0;
            int i_arg = 0;

            // Here we first expand everything
            while (i_arg < current_template_arguments->num_parameters
                    && i_param < primary_template_parameters->num_parameters)
            {
                if (template_parameter_kind_is_pack(primary_template_parameters->parameters[i_param]->kind))
                {
                    P_LIST_ADD(expanded_parameter_values, num_expanded_parameter_values, updated_parameter_values[i_arg]);
                    i_param++;
                }
                else
                {
                    switch (updated_parameter_values[i_arg]->kind)
                    {
                        case TPK_TYPE:
                        case TPK_TEMPLATE:
                            {
                                if (is_sequence_of_types(updated_parameter_values[i_arg]->type))
                                {
                                    int k, num_items = sequence_of_types_get_num_types(updated_parameter_values[i_arg]->type);
                                    for (k = 0; k < num_items; k++)
                                    {
                                        template_parameter_value_t *new_value = xcalloc(1, sizeof(*new_value));
                                        new_value->kind = updated_parameter_values[i_arg]->kind;
                                        new_value->type =
                                                sequence_of_types_get_type_num(updated_parameter_values[i_arg]->type, k);

                                        P_LIST_ADD(expanded_parameter_values,
                                                num_expanded_parameter_values,
                                                new_value);
                                        i_param++;
                                    }
                                    if (i_param > primary_template_parameters->num_parameters)
                                    {
                                        DEBUG_CODE()
                                        {
                                            fprintf(stderr, "SCOPE: Too many template arguments after expansion of "
                                                    "template type/template argument\n");
                                        }
                                        xfree(updated_parameter_values);
                                        xfree(expanded_parameter_values);
                                        return NULL;
                                    }
                                }
                                else
                                {
                                    P_LIST_ADD(expanded_parameter_values,
                                            num_expanded_parameter_values,
                                            updated_parameter_values[i_arg]);
                                    i_param++;
                                }
                                break;
                            }
                        case TPK_NONTYPE:
                            {
                                if (nodecl_is_list(updated_parameter_values[i_arg]->value))
                                {
                                    int num_items = 0;
                                    nodecl_t* list = nodecl_unpack_list(updated_parameter_values[i_arg]->value, &num_items);

                                    int k;
                                    for (k = 0; k < num_items; k++)
                                    {
                                        template_parameter_value_t *new_value = xcalloc(1, sizeof(*new_value));
                                        new_value->kind = updated_parameter_values[i_arg]->kind;
                                        // FIXME - What about this case? template <typename ...T, T...N>
                                        new_value->type = updated_parameter_values[i_arg]->type;
                                        new_value->value = list[k];
                                        i_param++;
                                    }
                                    xfree(list);

                                    if (i_param > primary_template_parameters->num_parameters)
                                    {
                                        DEBUG_CODE()
                                        {
                                            fprintf(stderr, "SCOPE: Too many template arguments after expansion of "
                                                    "template nontype argument\n");
                                        }
                                        xfree(updated_parameter_values);
                                        xfree(expanded_parameter_values);
                                        return NULL;
                                    }
                                }
                                else
                                {
                                    P_LIST_ADD(expanded_parameter_values,
                                            num_expanded_parameter_values,
                                            updated_parameter_values[i_arg]);
                                    i_param++;
                                }
                                break;
                            }
                        default: internal_error("Code unreachable", 0);
                    }
                }
                i_arg++;
            }

            // We know that the list does not overrun, make sure the arguments match the parameters
            for (i = 0; i < num_expanded_parameter_values; i++)
            {
                if (template_parameter_kind_get_base_kind(primary_template_parameters->parameters[i]->kind)
                        != template_parameter_kind_get_base_kind(expanded_parameter_values[i]->kind))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "SCOPE: After expansion, template arguments do not match their template parameters\n");
                    }
                    xfree(updated_parameter_values);
                    xfree(expanded_parameter_values);
                    return NULL;
                }
            }

            // Build a proper list for this
            template_parameter_list_t *updated_template_arguments = xcalloc(1, sizeof(*updated_template_arguments));
            updated_template_arguments->enclosing = current_template_arguments->enclosing;
            updated_template_arguments->is_explicit_specialization = current_template_arguments->is_explicit_specialization;
            updated_template_arguments->parameters = primary_template_parameters->parameters;
            updated_template_arguments->arguments = expanded_parameter_values;
            updated_template_arguments->num_parameters = num_expanded_parameter_values;

            // Everything seems fine now except for one thing, it might happen that we still need default arguments yet
            decl_context_t new_template_context = decl_context;
            new_template_context.template_parameters = updated_template_arguments;

            for (i = num_expanded_parameter_values; i < primary_template_parameters->num_parameters; i++)
            {
                 if (primary_template_parameters->arguments[i] == NULL)
                 {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "SCOPE: We have too little template arguments\n");
                    }
                    xfree(updated_parameter_values);
                    xfree(expanded_parameter_values);
                    return NULL;
                 }
                 else
                 {
                     template_parameter_value_t* v = update_template_parameter_value_of_template_class(
                             primary_template_parameters->arguments[i],
                             new_template_context,
                             locus,
                             pack_index);
                     P_LIST_ADD(updated_template_arguments->arguments, updated_template_arguments->num_parameters, v);
                 }
            }

            ERROR_CONDITION(updated_template_arguments->num_parameters != primary_template_parameters->num_parameters,
                    "After expanding template arguments, the number of template arguments does not match those of the "
                    "primery template parameter (args=%d vs params=%d)\n",
                    updated_template_arguments->num_parameters,
                    primary_template_parameters->num_parameters);

            // This was actually a temporary
            xfree(updated_parameter_values);

            // Once the types have been updated, reask for a specialization
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Reasking for specialization\n");
            }
            type_t* updated_specialized =
                template_type_get_specialized_type(template_type,
                        updated_template_arguments,
                        decl_context,
                        locus);
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
                        locus,
                        pack_index),
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
                locus, pack_index);

        if (updated_referenced == NULL)
            return NULL;

        type_t* result_type = get_lvalue_reference_type(updated_referenced);

        return result_type;
    }
    else if (is_rvalue_reference_type(orig_type))
    {
        type_t* referenced = reference_type_get_referenced_type(orig_type);

        type_t* updated_referenced = update_type_aux_(referenced, decl_context,
                locus, pack_index);

        if (updated_referenced == NULL)
            return NULL;

        type_t* result_type = get_rvalue_reference_type(updated_referenced);

        return result_type;
    }
    else if (is_pointer_type(orig_type))
    {
        cv_qualifier_t cv_qualifier = get_cv_qualifier(orig_type);

        type_t* pointee = pointer_type_get_pointee_type(orig_type);

        type_t* updated_pointee = update_type_aux_(pointee, decl_context,
                locus, pack_index);

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
                locus, pack_index);

        if (updated_pointee == NULL)
            return NULL;

        type_t* pointee_class = pointer_to_member_type_get_class_type(orig_type);
        pointee_class = update_type_aux_(pointee_class, decl_context, 
                locus, pack_index);

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
                    locus, pack_index);
            // Something went wrong here for the return type
            if (return_type == NULL)
                return NULL;
        }

        parameter_info_t *packed_parameter_types = NULL;
        int num_packed_parameter_types = 0;

        int last = function_type_get_num_parameters(orig_type);

        char has_ellipsis = function_type_get_has_ellipsis(orig_type);

        if (has_ellipsis)
            last--;

        int i;
        for (i = 0; i < last; i++)
        {
            type_t* param_orig_type = function_type_get_parameter_type_num(orig_type, i);

            param_orig_type = update_type_aux_(param_orig_type, decl_context, 
                    locus, pack_index);

            if (param_orig_type == NULL)
                return NULL;

            parameter_info_t parameter_info = get_parameter_info_for_type(param_orig_type);
            P_LIST_ADD(packed_parameter_types, num_packed_parameter_types, parameter_info);
        }

        // Look for sequenced types
        char expansions_found = 0;
        for (i = 0; i < num_packed_parameter_types && !expansions_found; i++)
        {
            if (is_sequence_of_types(packed_parameter_types[i].type_info))
            {
                expansions_found = 1;
            }
        }

        int num_unpacked_parameter_types = 0;
        parameter_info_t *unpacked_parameter_types = NULL;

        if (!expansions_found)
        {
            unpacked_parameter_types = packed_parameter_types;
            num_unpacked_parameter_types = num_packed_parameter_types;
        }
        else
        {
            for (i = 0; i < num_packed_parameter_types; i++)
            {
                if (is_sequence_of_types(packed_parameter_types[i].type_info))
                {
                    int j, N = sequence_of_types_get_num_types(packed_parameter_types[i].type_info);
                    for (j = 0; j < N; j++)
                    {
                        type_t* param_orig_type = sequence_of_types_get_type_num(packed_parameter_types[i].type_info, j);
                        parameter_info_t parameter_info = get_parameter_info_for_type(param_orig_type);
                        P_LIST_ADD(unpacked_parameter_types, num_unpacked_parameter_types, parameter_info);
                    }
                }
                else
                {
                    P_LIST_ADD(unpacked_parameter_types, num_unpacked_parameter_types, packed_parameter_types[i]);
                }
            }

            xfree(packed_parameter_types);
            packed_parameter_types = NULL;
        }

        if (has_ellipsis)
        {
            parameter_info_t parameter_info;
            memset(&parameter_info, 0, sizeof(parameter_info));
            parameter_info.is_ellipsis = 1;

            P_LIST_ADD(unpacked_parameter_types, num_unpacked_parameter_types, parameter_info);
        }

        type_t* updated_function_type = get_new_function_type(return_type,
                unpacked_parameter_types, num_unpacked_parameter_types, REF_QUALIFIER_NONE);

        xfree(unpacked_parameter_types);

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
            array_size = update_nodecl_constant_expression(array_size, decl_context, pack_index);

            if (nodecl_get_kind(array_size) == NODECL_ERR_EXPR)
            {
                running_error("%s: error: could not update array dimension",
                        nodecl_locus_to_str(array_size));
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Expression '%s' successfully updated\n",
                        codegen_to_str(array_size, decl_context));
            }
        }

        type_t* element_type = array_type_get_element_type(orig_type);
        element_type = update_type_aux_(element_type, decl_context, 
                locus, pack_index);

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
                locus, pack_index);

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

            ERROR_CONDITION(nodecl_get_kind(fix_dependent_parts) != NODECL_CXX_DEP_NAME_NESTED, "Invalid tree kind", 0);

            list = nodecl_unpack_list(nodecl_get_child(fix_dependent_parts, 0), &num_items);
            for (i = 0; i < num_items; i++)
            {
                appended_dependent_parts = nodecl_append_to_list(appended_dependent_parts, list[i]);
            }
            xfree(list);

            ERROR_CONDITION(nodecl_get_kind(dependent_parts) != NODECL_CXX_DEP_NAME_NESTED, "Invalid tree kind", 0);

            list = nodecl_unpack_list(nodecl_get_child(dependent_parts, 0), &num_items);
            for (i = 0; i < num_items; i++)
            {
                appended_dependent_parts = nodecl_append_to_list(appended_dependent_parts, list[i]);
            }
            xfree(list);

            cv_qualif |= cv_qualif_dep;

            fixed_type = get_user_defined_type(fix_dependent_entry);

            dependent_parts = nodecl_make_cxx_dep_name_nested(appended_dependent_parts, 
                    nodecl_get_locus(dependent_parts));
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
            update_dependent_typename(fixed_type, dependent_parts, decl_context, locus, pack_index);

        if (updated_type != NULL)
        {
            updated_type = get_cv_qualified_type(updated_type, cv_qualif);
        }

        return updated_type;
    }
    else if (is_gcc_typeof_expr(orig_type))
    {
        nodecl_t nodecl_expr = gcc_typeof_expr_type_get_expression(orig_type);

        enter_test_expression();
        nodecl_t nodecl_new_expr = instantiate_expression(nodecl_expr, decl_context);
        leave_test_expression();

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
    else if (is_pack_type(orig_type))
    {
        return update_pack_type(orig_type, decl_context, locus);
    }
    else if (is_sequence_of_types(orig_type))
    {
        int num_types = sequence_of_types_get_num_types(orig_type);
        type_t* types[num_types + 1];
        int i;
        for (i = 0; i < num_types; i++)
        {
            types[i] = update_type_aux_(sequence_of_types_get_type_num(orig_type, i), decl_context, locus, pack_index);
            if (types[i] == NULL)
                return NULL;
        }

        return get_sequence_of_types(num_types, types);
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
        const locus_t* locus)
{
    DEBUG_CODE()
    {
            fprintf(stderr, "SCOPE: Updating type '%s'\n", print_declarator(orig_type));
    }

    type_t* result = update_type_aux_(orig_type, decl_context, locus, /* pack_index */ -1);

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

static type_t* update_gcc_type_attributes(type_t* orig_type, type_t* result,
        decl_context_t context_of_being_instantiated,
        const locus_t* locus,
        int pack_index UNUSED_PARAMETER)
{
    int num_gcc_attributes = 0;
    gcc_attribute_t *gcc_attributes = NULL;
    variant_type_get_gcc_attributes(orig_type, &num_gcc_attributes, &gcc_attributes);

    int i;
    for (i = 0; i < num_gcc_attributes; i++)
    {
        gcc_attribute_t new_gcc_attr;
        new_gcc_attr.attribute_name = gcc_attributes[i].attribute_name;
        new_gcc_attr.expression_list = gcc_attributes[i].expression_list;

        if (strcmp(gcc_attributes[i].attribute_name, "aligned") == 0)
        {
            nodecl_t aligned_attribute = instantiate_expression(
                    nodecl_list_head(new_gcc_attr.expression_list),
                    context_of_being_instantiated);
            if (nodecl_is_err_expr(aligned_attribute))
            {
                result = NULL;
            }
            else if (!nodecl_expr_is_value_dependent(aligned_attribute)
                    && !nodecl_is_constant(aligned_attribute))
            {
                error_printf("%s: error: 'aligned' attribute of type '%s' after instantiation is not constant\n",
                        locus_to_str(locus),
                        print_type_str(orig_type, context_of_being_instantiated));
                result = NULL;
            }
            else
            {
                new_gcc_attr.expression_list = nodecl_make_list_1(aligned_attribute);
            }
        }

        if (result == NULL)
            return NULL;

        result = get_variant_type_add_gcc_attribute(result, new_gcc_attr);
    }

    return result;
}

type_t* update_type_for_instantiation(type_t* orig_type,
        decl_context_t context_of_being_instantiated,
        const locus_t* locus,
        int pack_index)
{
    DEBUG_CODE()
    {
            fprintf(stderr, "SCOPE: While instantiating, updating type '%s'\n", print_declarator(orig_type));
    }

    type_t* result = update_type_aux_(orig_type,
            context_of_being_instantiated,
            locus, pack_index);

    result = update_gcc_type_attributes(orig_type, result, context_of_being_instantiated, locus, pack_index);

    if (result == NULL)
    {
        running_error("%s: error: type '%s' rendered invalid during instantiation\n",
                locus_to_str(locus), print_type_str(orig_type, context_of_being_instantiated));
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
    char is_expansion = 0;
    if (ASTType(template_parameter) == AST_TEMPLATE_ARGUMENT_PACK_EXPANSION)
    {
        template_parameter = ASTSon0(template_parameter);
        is_expansion = 1;
    }

    if (ASTType(template_parameter) == AST_AMBIGUITY)
    {
        solve_ambiguous_template_argument(template_parameter, template_parameters_context);
    }

    switch (ASTType(template_parameter))
    {
        case AST_TEMPLATE_EXPRESSION_ARGUMENT :
            {
                template_parameter_value_t* t_argument =
                    counted_xcalloc(1, sizeof(*t_argument), &_bytes_used_scopes);

                AST expr = ASTSon0(template_parameter);

                nodecl_t nodecl_expr = nodecl_null();
                check_nontype_template_argument_expression(expr, template_parameters_context, &nodecl_expr);

                if (nodecl_is_err_expr(nodecl_expr))
                    return NULL;

                if (is_expansion)
                {
                    nodecl_expr = nodecl_make_cxx_value_pack(
                            nodecl_expr,
                            nodecl_get_type(nodecl_expr),
                            nodecl_get_locus(nodecl_expr));
                }

                t_argument->kind = TPK_NONTYPE;
                t_argument->value = nodecl_expr;
                t_argument->type = nodecl_get_type(nodecl_expr);

                return t_argument;
                break;
            }
        case AST_TEMPLATE_TYPE_ARGUMENT :
            {
                template_parameter_value_t* t_argument = 
                    counted_xcalloc(1, sizeof(*t_argument), &_bytes_used_scopes);

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
                        template_parameters_context, &dummy_nodecl_output);

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

                if (is_error_type(declarator_type))
                {
                    if (!checking_ambiguity())
                    {
                        error_printf("%s: error: invalid template-argument number %d\n",
                                ast_location(template_parameter),
                                position);
                    }
                    return NULL;
                }

                if (is_named_type(declarator_type)
                        && (named_type_get_symbol(declarator_type)->kind == SK_TEMPLATE
                            || named_type_get_symbol(declarator_type)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                            || named_type_get_symbol(declarator_type)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK))
                {
                    if (abstract_decl != NULL)
                    {
                        if (!checking_ambiguity())
                        {
                            error_printf("%s: error: invalid template-argument number %d\n",
                                    ast_location(template_parameter),
                                    position);
                        }
                        return NULL;
                    }
                    t_argument->kind = TPK_TEMPLATE;
                }
                else
                {
                    t_argument->kind = TPK_TYPE;
                }

                if (is_expansion)
                {
                    declarator_type = get_pack_type(declarator_type);
                }

                t_argument->type = declarator_type;

                return t_argument;
                break;
            }
        default:
            {
                internal_error("Invalid node %s", ast_print_node_type(ASTType(template_parameter)));
            }
    }
    return NULL;
}

template_parameter_list_t* get_template_arguments_from_syntax(
        AST template_parameters_list_tree,
        decl_context_t template_parameters_context)
{
    template_parameter_list_t* result = counted_xcalloc(1, sizeof(*result), &_bytes_used_scopes);

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
        // Empty parameter, it will be filled elsewhere
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

enum template_parameter_kind template_parameter_kind_get_base_kind(enum template_parameter_kind kind)
{
    switch (kind)
    {
        case TPK_NONTYPE_PACK: { return TPK_NONTYPE; }
        case TPK_TYPE_PACK: { return TPK_TYPE; }
        case TPK_TEMPLATE_PACK: { return TPK_TEMPLATE; }

        case TPK_NONTYPE:
        case TPK_TYPE:
        case TPK_TEMPLATE:
            { return kind; }
        default:
            {
                internal_error("Unexpected pack kind", 0);
            }
    }
}

static template_parameter_list_t* complete_template_parameters_of_template_class(
        decl_context_t template_name_context,
        type_t* template_type,
        template_parameter_list_t* template_parameters,
        const locus_t* locus)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Completing template arguments for class template type\n");
    }

    template_parameter_list_t* primary_template_parameters =
        template_type_get_template_parameters(template_type);

    template_parameter_list_t* result = duplicate_template_argument_list(template_parameters);

    char there_are_pack_arguments = 0;
    int i;
    for (i = 0; i < result->num_parameters && !there_are_pack_arguments; i++)
    {
        there_are_pack_arguments = template_argument_is_pack(result->arguments[i]);
    }

    if (there_are_pack_arguments)
    {
        // We can't actually complete anything here template packs may be assigned
        // to more than one template parameter, so assume this is fine, let it fail
        // later when we update the types
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Cannot complete anything because "
                    "we found pack expansions as template arguments\n");
        }
        return result;
    }

    // Note: we are creating a new template parameter list but it is a sibling
    // of the primary template so we must ensure they have the same nesting in the
    // hierarchy of template parameters
    result->enclosing = primary_template_parameters->enclosing;

    template_parameter_t* last = NULL;
    char last_is_variadic = 0;
    if (primary_template_parameters->num_parameters > 0)
    {
        last = primary_template_parameters->parameters[primary_template_parameters->num_parameters - 1];
        last_is_variadic = template_parameter_kind_is_pack(last->kind);
    }

    if ((result->num_parameters > primary_template_parameters->num_parameters)
            && !last_is_variadic)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Too many template arguments %d > %d", 
                    result->num_parameters, 
                    primary_template_parameters->num_parameters);
        }

        if (!checking_ambiguity())
        {
            error_printf("%s: error: too many template-arguments for template class\n",
                    locus_to_str(locus));
        }

        return NULL;
    }

    decl_context_t new_template_context = template_name_context;
    new_template_context.template_parameters = result;

    int non_variadic_parameters = primary_template_parameters->num_parameters;
    if (last_is_variadic)
    {
        non_variadic_parameters--;
    }

    // Now review template parameters. Note that these parameters will not be packs but
    // their arguments might be pack expansions.
    char last_argument_was_pack = 0;
    for (i = 0; i < non_variadic_parameters; i++)
    {
        if (i >= result->num_parameters)
        {
            if (primary_template_parameters->arguments[i] == NULL)
            {
                if (!last_argument_was_pack)
                {
                    // One of the template parameters is lacking an argument
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "SCOPE: Template argument %d is missing", i);
                    }

                    if (!checking_ambiguity())
                    {
                        error_printf("%s: error: template argument number %d is missing and there is no default template argument for it\n",
                                locus_to_str(locus), i);
                    }

                    return NULL;
                }
                else
                {
                    // It may happen that there are less template arguments than template parameters in cases like this
                    /*
                       template <typename T1, typename T2>
                       struct A
                       {
                       };

                       template <typename ...T>
                       struct B
                       {
                           A<T...> a; // Only one template parameter, but it could be extended by the template argument pack
                       };
                     */
                    break;
                }
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
                        locus, /* pack_index */ -1);
                P_LIST_ADD(result->arguments, result->num_parameters, v);
            }
        }
        else
        {
            // Set the template parameter
            result->parameters[i] = primary_template_parameters->parameters[i];

            // And check it matches what we got (note that expansions may match as well)
            if (result->parameters[i]->kind != template_parameter_kind_get_base_kind(result->arguments[i]->kind))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Template parameter kind and template argument kind do not match\n");
                }

                if (!checking_ambiguity())
                {
                    error_printf("%s: error: kind of template argument number %d does not match "
                            "that of the corresponding template parameter\n",
                            locus_to_str(locus), i + 1);
                }

                return NULL;
            }

            if (template_argument_is_pack(result->arguments[i]))
            {
                last_argument_was_pack = 1;
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
                    locus);

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
                            locus);

                    if (entry == NULL)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "SCOPE: Cannot solve unresolved overload in template argument expression to"
                                    " the type of the template parameter\n");
                        }
                        if (!checking_ambiguity())
                        {
                            error_printf("%s: error: cannot solve address of overload function in template argument number %d",
                                    locus_to_str(locus), i);
                        }
                        return NULL;
                    }

                    // If the symbol is not null, update the argument with its real function
                    result->arguments[i]->value = nodecl_make_symbol(entry, locus);
                    nodecl_set_type(result->arguments[i]->value, entry->type_information);
                }
                else
                {
                    // We can't allow a user defined conversion here since it
                    // would mean executing user code at compile time, which is
                    // not possible, so we check for a SCS.
                    //
                    if (!is_dependent_type(arg_type))
                    {
                        standard_conversion_t scs_conv;
                        if (!standard_conversion_between_types(&scs_conv, arg_type, get_unqualified_type(dest_type)))
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "SCOPE: Cannot convert template argument expression to the type of the template parameter\n");
                            }
                            if (!checking_ambiguity())
                            {
                                error_printf("%s: error: type '%s' of template argument %d cannot be converted to "
                                        "type '%s' of the corresponding template parameter\n",
                                        locus_to_str(locus),
                                        print_type_str(arg_type, template_name_context),
                                        i + 1,
                                        print_type_str(dest_type, template_name_context));
                            }
                            return NULL;
                        }
                    }
                }
            }
        }
    }

    if (last_is_variadic)
    {
        // Now review template arguments of the final pack parameter
        if (result->num_parameters > non_variadic_parameters)
        {
            enum template_parameter_kind pack_base_kind = template_parameter_kind_get_base_kind(last->kind);

            // We will fold this argument
            int last_argument_index = i;
            template_parameter_value_t* folded_value = xcalloc(1, sizeof(*folded_value));
            folded_value->kind = pack_base_kind;

            for (; i < result->num_parameters; i++)
            {
                // Set the template parameter
                result->parameters[i] = last;

                // And check it matches the base kind of this pack
                if (pack_base_kind != result->arguments[i]->kind)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "SCOPE: Template parameter pack kind and template argument kind do not match\n");
                    }

                    if (!checking_ambiguity())
                    {
                        error_printf("%s: error: kind of template argument number %d does not match "
                                "that of the corresponding template parameter pack\n",
                                locus_to_str(locus), i + 1);
                    }


                    return NULL;
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
                            locus);

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
                                    locus);

                            if (entry == NULL)
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "SCOPE: Cannot solve unresolved overload in template argument expression to"
                                            " the type of the template parameter\n");
                                }
                                if (!checking_ambiguity())
                                {
                                    error_printf("%s: error: cannot solve address of overload function in template argument number %d",
                                            locus_to_str(locus), i);
                                }
                                return NULL;
                            }

                            // If the symbol is not null, update the argument with its real function
                            result->arguments[i]->value = nodecl_make_symbol(entry, locus);
                            nodecl_set_type(result->arguments[i]->value, entry->type_information);
                        }
                        else
                        {
                            // We can't allow a user defined conversion here since it
                            // would mean executing user code at compile time, which is
                            // not possible, so we check for a SCS.
                            //
                            if (!is_dependent_type(arg_type))
                            {
                                standard_conversion_t scs_conv;
                                if (!standard_conversion_between_types(&scs_conv, arg_type, get_unqualified_type(dest_type)))
                                {
                                    DEBUG_CODE()
                                    {
                                        fprintf(stderr, "SCOPE: Cannot convert template argument expression to the type of the template parameter\n");
                                    }
                                    if (!checking_ambiguity())
                                    {
                                        error_printf("%s: error: type '%s' of template argument %d cannot be converted to "
                                                "type '%s' of the corresponding template parameter pack\n",
                                                locus_to_str(locus),
                                                print_type_str(arg_type, template_name_context),
                                                i + 1,
                                                print_type_str(dest_type, template_name_context));
                                    }
                                    return NULL;
                                }
                            }
                        }
                    }
                }

                folded_value->type = get_sequence_of_types_append_type(folded_value->type, result->arguments[i]->type);
                if (result->arguments[i]->kind == TPK_NONTYPE)
                {
                    folded_value->value = nodecl_append_to_list(folded_value->value, result->arguments[i]->value);
                }
            }

            result->arguments[last_argument_index] = folded_value;
            result->num_parameters = last_argument_index + 1;
        }
        else if (result->num_parameters == non_variadic_parameters)
        {
            // Empty argument for this variadic parameter
            // Create an empty one
            template_parameter_value_t* new_value = xcalloc(1, sizeof(*new_value));
            new_value->kind = template_parameter_kind_get_base_kind(last->kind);
            new_value->type = get_sequence_of_types(0, NULL);
            new_value->value = nodecl_null(); // Empty list

            int num_args = result->num_parameters;
            P_LIST_ADD(result->arguments, num_args, new_value);
            P_LIST_ADD(result->parameters, result->num_parameters, last);
        }
        else
        {
            internal_error("Too few arguments at this point\n", 0);
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

static const char* template_arguments_to_str_ex(
        template_parameter_list_t* template_parameters,
        int first_argument_to_be_printed,
        char print_first_level_bracket,
        decl_context_t decl_context,
        print_type_callback_t print_type_fun,
        void *print_type_data
        )

{
    if (template_parameters->num_parameters == 0
            || template_parameters->num_parameters <= first_argument_to_be_printed)
        return "";

    const char* result = "";
    if (print_first_level_bracket)
    {
        // It is not enough with the name, we have to print the arguments
        result = strappend(result, "<");
    }

    int i;
    char print_comma = 0;
    for (i = first_argument_to_be_printed; i < template_parameters->num_parameters; i++, print_comma = 1)
    {
        template_parameter_value_t* argument = template_parameters->arguments[i];

        if (print_comma)
        {
            if (argument == NULL
                    || !is_sequence_of_types(argument->type)
                    || sequence_of_types_get_num_types(argument->type) > 0)
            {
                result = strappend(result, ", ");
            }
        }

        if (argument == NULL)
        {
            result = strappend(result, template_parameters->parameters[i]->entry->symbol_name);
            continue;
        }

        const char* argument_str = "";
        switch (argument->kind)
        {
            case TPK_TYPE:
                {
                    argument_str = strappend(argument_str, 
                            print_type_fun(argument->type, decl_context, print_type_data));
                    break;
                }
            case TPK_NONTYPE:
                {
                    // The first unparenthesized '>' indicates the end of
                    // template arguments. For this reason, in some cases we
                    // need to parenthesize this template argument.
                    char codegen_of_nontype_template_argument;
                    codegen_of_nontype_template_argument = 1;

                    codegen_set_parameter(CODEGEN_PARAM_NONTYPE_TEMPLATE_ARGUMENT,
                            (void*)&codegen_of_nontype_template_argument);

                    if (nodecl_is_list(argument->value))
                    {
                        int num_items;
                        int j;
                        nodecl_t* list = nodecl_unpack_list(argument->value, &num_items);
                        for (j = 0; j < num_items; j++)
                        {
                            if (j > 0)
                                argument_str = strappend(argument_str, ", ");

                            argument_str = strappend(argument_str, codegen_to_str(list[j], decl_context));
                        }

                        xfree(list);
                    }
                    else
                    {
                        argument_str = codegen_to_str(argument->value, decl_context);
                    }

                    codegen_of_nontype_template_argument = 0;
                    codegen_set_parameter(CODEGEN_PARAM_NONTYPE_TEMPLATE_ARGUMENT,
                            (void*)&codegen_of_nontype_template_argument);

                    break;
                }
            case TPK_TEMPLATE:
                {
                    type_t* template_type = argument->type;
                    if (is_pack_type(argument->type))
                    {
                        template_type = pack_type_get_packed_type(argument->type);
                    }
                    if (is_sequence_of_types(template_type))
                    {
                        int num_types = sequence_of_types_get_num_types(template_type);
                        int k;
                        for (k = 0; k < num_types; k++)
                        {
                            if (k > 0)
                                argument_str = strappend(argument_str, ", ");

                            argument_str = strappend(argument_str,
                                    get_qualified_symbol_name(
                                        named_type_get_symbol(sequence_of_types_get_type_num(template_type, k)),
                                        decl_context)
                                    );
                        }
                    }
                    else
                    {
                        argument_str = get_qualified_symbol_name(
                                named_type_get_symbol(template_type),
                                decl_context);
                    }
                    if (is_pack_type(argument->type))
                    {
                        argument_str = strappend(argument_str, " ...");
                    }
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

    if (print_first_level_bracket)
    {
        if (result[strlen(result) - 1] == '>')
        {
            result = strappend(result, " >");
        }
        else
        {
            result = strappend(result, ">");
        }
    }

    return result;
}

static const char* print_type_str_internal(type_t* t, decl_context_t decl_context, void *data UNUSED_PARAMETER)
{
    return print_type_str(t, decl_context);
}

const char* template_arguments_to_str(
        template_parameter_list_t* template_parameters,
        int first_argument_to_be_printed,
        char print_first_level_bracket,
        decl_context_t decl_context)
{
    return template_arguments_to_str_ex(template_parameters,
            first_argument_to_be_printed,
            print_first_level_bracket,
            decl_context,
            print_type_str_internal,
            NULL
            );
}

const char* get_template_arguments_str(scope_entry_t* entry, 
        decl_context_t decl_context)
{
    template_parameter_list_t* template_parameters = template_specialized_type_get_template_arguments(entry->type_information);
    return template_arguments_to_str(template_parameters,
            /* first_argument_to_be_printed */ 0,
            /* first_level_brackets */ 1, 
            decl_context);
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

static const char* get_fully_qualified_symbol_name_of_depedent_typename_internal_impl(
        scope_entry_t* entry,
        decl_context_t decl_context,
        char *is_dependent, int *max_qualif_level,
        print_type_callback_t print_type_fun,
        void *print_type_data)
{
    scope_entry_t* dependent_entry = NULL;
    nodecl_t nodecl_parts = nodecl_null();

    dependent_typename_get_components(entry->type_information,
            &dependent_entry, &nodecl_parts);

    const char* result = get_fully_qualified_symbol_name_ex(dependent_entry,
            decl_context, is_dependent, max_qualif_level,
            /* no_templates */ 0, /* only_classes */ 0,
            /* do_not_emit_template_keywords */ 0,
            print_type_fun, print_type_data);

    int num_parts = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(nodecl_parts, 0), &num_parts);

    *max_qualif_level += num_parts;

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
            result = strappend(result, "<");
            const char* template_arguments_str =
                template_arguments_to_str_ex(template_parameters,
                        /* first argument to be printed */ 0,
                        /* we always emit < and > */ 0,
                        decl_context,
                        print_type_fun,
                        print_type_data);

            result = strappend(result, template_arguments_str);
            result = strappend(result, ">");
        }
    }

    return result;
}

// Get the fully qualified symbol name in the scope of the ocurrence
const char* get_fully_qualified_symbol_name_ex(scope_entry_t* entry,
        decl_context_t decl_context,
        char* is_dependent, int* max_qualif_level,
        char no_templates,
        char only_classes,
        char do_not_emit_template_keywords,
        print_type_callback_t print_type_fun,
        void *print_type_data
        )
{
    // DEBUG_CODE()
    // {
    //     fprintf(stderr, "SCOPE: Getting fully qualified symbol name for '%s'\n", entry->symbol_name);
    // }

    // Do not qualify symbol names if they appear inside an anonymous namespace
    const char* result = "";
    char current_has_template_parameters = 0;
    if (entry->decl_context.namespace_scope->related_entry->symbol_name != NULL
            && strcmp(entry->decl_context.namespace_scope->related_entry->symbol_name, "(unnamed)") == 0)
    {
        result = uniquestr(unmangle_symbol_name(entry));

        if (!no_templates
                && entry->type_information != NULL
                && is_template_specialized_type(entry->type_information)
                && template_specialized_type_get_template_arguments(entry->type_information) != NULL
                && !entry->entity_specs.is_conversion)
        {
            current_has_template_parameters = 1;

            template_parameter_list_t* template_parameter_list = template_specialized_type_get_template_arguments(entry->type_information);
            const char* template_parameters =  template_arguments_to_str_ex(template_parameter_list,
                    /* first_argument_to_be_printed */ 0,
                    /* first_level_brackets */ 1,
                    decl_context,
                    print_type_fun,
                    print_type_data);

            result = strappend(result, template_parameters);

            (*is_dependent) |= is_dependent_type(entry->type_information);

            type_t* template_type = template_specialized_type_get_related_template_type(entry->type_information);
            scope_entry_t* template_sym = template_type_get_related_symbol(template_type);
            if (template_sym->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                // This is dependent
                (*is_dependent) = 1;
            }
        }
        return result;
    }

    // If this is the injected symbol, ignore it and get the real entry
    if (entry->entity_specs.is_injected_class_name)
    {
        // The injected class name is a member
        entry = named_type_get_symbol(entry->entity_specs.class_type);
    }

    // Do not print anonymous unions or variables of anonymous unions
    if (!entry->entity_specs.is_anonymous_union
            && !(is_named_class_type(entry->type_information)
                && named_type_get_symbol(entry->type_information)->entity_specs.is_anonymous_union))
    {
        result = uniquestr(unmangle_symbol_name(entry));
    }


    if (entry->kind == SK_TEMPLATE_NONTYPE_PARAMETER
            || entry->kind == SK_TEMPLATE_TYPE_PARAMETER
            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
            || entry->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK
            || entry->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
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
        (*is_dependent) = 1;
        return result;
    }
    else if (entry->kind == SK_DEPENDENT_ENTITY)
    {
        return get_fully_qualified_symbol_name_of_depedent_typename_internal_impl(
                entry, decl_context, is_dependent, max_qualif_level,
                print_type_fun, print_type_data);
    }
    else if (!no_templates
            && entry->type_information != NULL
            && is_template_specialized_type(entry->type_information)
            && template_specialized_type_get_template_arguments(entry->type_information) != NULL
            && !entry->entity_specs.is_conversion)
    {
        current_has_template_parameters = 1;

        template_parameter_list_t* template_parameter_list = template_specialized_type_get_template_arguments(entry->type_information);
        const char* template_parameters =  template_arguments_to_str_ex(template_parameter_list,
                /* first_argument_to_be_printed */ 0,
                /* first_level_brackets */ 1, 
                decl_context,
                print_type_fun,
                print_type_data);

        result = strappend(result, template_parameters);

        (*is_dependent) |= is_dependent_type(entry->type_information);

        type_t* template_type = template_specialized_type_get_related_template_type(entry->type_information);
        scope_entry_t* template_sym = template_type_get_related_symbol(template_type);
        if (template_sym->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
        {
            // This is dependent
            (*is_dependent) = 1;
            return result;
        }
    }

    if (entry->kind == SK_TYPEDEF)
    {
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
            get_fully_qualified_symbol_name_ex(class_symbol, decl_context, &prev_is_dependent, max_qualif_level,
                    /* no_templates */ 0, only_classes, do_not_emit_template_keywords, print_type_fun, print_type_data);

        if (!class_symbol->entity_specs.is_anonymous_union)
        {
            class_qualification = strappend(class_qualification, "::");
        }

        if (prev_is_dependent
                && current_has_template_parameters
                && !do_not_emit_template_keywords)
        {
            class_qualification = strappend(class_qualification, "template ");
        }

        (*is_dependent) |= prev_is_dependent;

        result = strappend(class_qualification, result);
    }
    else if (IS_CXX11_LANGUAGE && entry->kind == SK_ENUMERATOR)
    {
        // In C++11 we qualify enumerators that are not members
        scope_entry_t* enum_symbol = named_type_get_symbol(entry->type_information);
        char prev_is_dependent = 0;
        const char* enum_qualification =
            get_fully_qualified_symbol_name_ex(enum_symbol, decl_context, &prev_is_dependent, max_qualif_level,
                    /* no_templates */ 0, only_classes, do_not_emit_template_keywords, print_type_fun, print_type_data);

        enum_qualification = strappend(enum_qualification, "::");

        (*is_dependent) |= prev_is_dependent;

        result = strappend(enum_qualification, result);
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
            decl_context, is_dependent, max_qualif_level,
            /* no_templates */ 0, /* only_classes */ 0,
            /* do_not_emit_template_keywords */ 0,
            print_type_str_internal,
            NULL);
}

const char* get_fully_qualified_symbol_name_without_template(scope_entry_t* entry,
        decl_context_t decl_context, char* is_dependent, int* max_qualif_level)
{
    return get_fully_qualified_symbol_name_ex(entry,
            decl_context, is_dependent, max_qualif_level,
            /* no_templates */ 1, /* only_classes */ 0,
            /* do_not_emit_template_keywords */ 0,
            print_type_str_internal,
            NULL);
}

const char* get_class_qualification_of_symbol(scope_entry_t* entry,
        decl_context_t decl_context, char* is_dependent, int* max_qualif_level)
{
    return get_fully_qualified_symbol_name_ex(entry,
            decl_context, is_dependent, max_qualif_level,
            /* no_templates */ 0, /* only_classes */ 1,
            /* do_not_emit_template_keywords */ 1,
            print_type_str_internal,
            NULL);
}

const char* get_class_qualification_of_symbol_without_template(scope_entry_t* entry,
        decl_context_t decl_context, char* is_dependent, int* max_qualif_level)
{
    return get_fully_qualified_symbol_name_ex(entry,
            decl_context, is_dependent, max_qualif_level,
            /* no_templates */ 1, /* only_classes */ 1,
            /* do_not_emit_template_keywords */ 1,
            print_type_str_internal,
            NULL);
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


    int j = 0;
    {
        int i = 0;
        while (template_parameters != NULL)
        {
            if (template_parameters->parameters != NULL || template_parameters->is_explicit_specialization)
            {
                ERROR_CONDITION(j == MCXX_MAX_TEMPLATE_NESTING_LEVELS, "Too many template nesting levels", 0);
                levels[j] = template_parameters->parameters;
                value_levels[j] = template_parameters->arguments;
                num_items[j] = template_parameters->num_parameters;
                j++;
            }

            i++;
            template_parameters = template_parameters->enclosing;
        }
    }
    
    // Nesting is too deep
    if (template_parameter_nesting > j)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Template parameter not found: nesting %d is too deep\n", template_parameter_nesting);
        }
        return NULL;
    }

    template_parameter_t** current_nesting = levels[j - template_parameter_nesting];
    template_parameter_value_t** current_values = value_levels[j - template_parameter_nesting];
    int current_num_items = num_items[j - template_parameter_nesting];
    
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

    template_parameter_t* current_template_parameter = current_nesting[template_parameter_position];
    if (current_template_parameter == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Template parameter %d is NULL\n",
                    template_parameter_position);
        }
        return NULL;
    }
    scope_entry_t* parameter_entry = current_template_parameter->entry;
    ERROR_CONDITION(parameter_entry == NULL, "Invalid symbol", 0);

    template_parameter_value_t* value = current_values[template_parameter_position];
    if (value != NULL
            && !value->is_default)
    {
        if (value->entry == NULL)
        {
            value->entry = counted_xcalloc(1, sizeof(*value->entry), &_bytes_used_scopes);
            value->entry->symbol_name = parameter_entry->symbol_name;
            value->entry->decl_context = context;
            value->entry->entity_specs.is_template_parameter = 1;

            switch (current_template_parameter->kind)
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
                        // FIXME - This is a bit nonregular respect the other
                        // cases, can it be fixed? Maybe using a template-alias?
                        xfree(value->entry);
                        value->entry = named_type_get_symbol(value->type);
                        break;
                    }
                case TPK_NONTYPE_PACK:
                    {
                        value->entry->kind = SK_VARIABLE_PACK;
                        value->entry->type_information = value->type;
                        value->entry->value = value->value;;
                        break;
                    }
                case TPK_TYPE_PACK:
                    {
                        value->entry->kind = SK_TYPEDEF_PACK;
                        value->entry->type_information = value->type;
                        break;
                    }
                case TPK_TEMPLATE_PACK:
                    {
                        value->entry->kind = SK_TEMPLATE_PACK;
                        value->entry->type_information = value->type;
                        break;
                    }
                default:
                    internal_error("Unexpected value kind", 0);
                    break;
            }
        }
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Template parameter (%d, %d) found with value. Symbol='%s' ",
                    template_parameter_nesting,
                    template_parameter_position,
                    value->entry->symbol_name);
            fprintf(stderr, " Type='%s' ",
                    print_declarator(value->entry->type_information));
            if (template_parameter_kind_get_base_kind(value->kind) == TPK_NONTYPE)
            {
                if (nodecl_is_list(value->entry->value))
                {
                    fprintf(stderr, " Value='");
                    int num_items_pack;
                    nodecl_t* list = nodecl_unpack_list(value->entry->value, &num_items_pack);
                    int k;
                    for (k = 0; k < num_items_pack; k++)
                    {
                        if (k > 0)
                            fprintf(stderr, ", ");

                        if (!nodecl_is_constant(list[k]))
                        {
                            fprintf(stderr, "#%d<<non-constant>>", k);
                        }
                        else
                        {
                            fprintf(stderr, "%s",
                                codegen_to_str(const_value_to_nodecl(nodecl_get_constant(list[k])),
                                    context));
                        }
                    }
                    fprintf(stderr, "'");

                    xfree(list);
                }
                else
                {
                    if (!nodecl_is_constant(value->entry->value))
                    {
                        fprintf(stderr, " Value='<<nonconstant>>'");
                    }
                    else
                    {
                        fprintf(stderr, " Value='%s'", 
                                codegen_to_str(const_value_to_nodecl(nodecl_get_constant(value->entry->value)),
                                    context));
                    }
                }
            }
            fprintf(stderr, "\n");
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

char is_qualified_id_expression(AST a)
{
    return a != NULL
        && (ASTType(a) == AST_QUALIFIED_ID);
}

char is_id_expression(AST a)
{
    return is_unqualified_id_expression(a) || is_qualified_id_expression(a);
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
    SYMBOL_KIND_TABLE_FORTRAN
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

const char* symbol_kind_to_str(enum cxx_symbol_kind symbol_kind)
{
    ERROR_CONDITION (symbol_kind >= SK_LAST_KIND, "Invalid kind", 0);
    return symbol_kind_table_str[symbol_kind];
}

enum cxx_symbol_kind symbol_str_to_kind(const char* str)
{
    ERROR_CONDITION(str == NULL, "Invalid string", 0);

    if (0);
#define SYMBOL_KIND(x, _) \
    else if (strcmp(#x, str) == 0) return x;
    SYMBOL_KIND_TABLE
    SYMBOL_KIND_TABLE_FORTRAN
#undef SYMBOL_KIND

    return SK_UNDEFINED;
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
                            fprintf(stderr, "  Argument: ");

                            if (nodecl_is_list(v->value))
                            {
                                int num_items;
                                nodecl_t* list = nodecl_unpack_list(v->value, &num_items);
                                int j;
                                for (j = 0; j < num_items; j++)
                                {
                                    if (j > 0)
                                        fprintf(stderr, ", ");

                                    fprintf(stderr, "%s", codegen_to_str(list[j],
                                                CURRENT_COMPILED_FILE->global_decl_context));
                                }

                                fprintf(stderr, "\n");

                                xfree(list);
                            }
                            else
                            {
                                fprintf(stderr, "%s\n", codegen_to_str(v->value,
                                            CURRENT_COMPILED_FILE->global_decl_context));
                            }

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

static scope_entry_list_t* query_nodecl_simple_name(
        decl_context_t decl_context,
        decl_context_t top_level_decl_context,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags)
{
    ERROR_CONDITION(nodecl_get_kind(nodecl_name) != NODECL_CXX_DEP_NAME_SIMPLE, "Invalid nodecl", 0);

    if (BITMAP_TEST(decl_flags, DF_LABEL))
    {
        decl_context.current_scope = decl_context.function_scope;
    }

    const locus_t* locus = nodecl_get_locus(nodecl_name);
    const char* name = nodecl_get_text(nodecl_name);

    if (BITMAP_TEST(decl_flags, DF_CONSTRUCTOR))
    {
        name = strappend("constructor ", name);
    }

    scope_entry_list_t* result = name_lookup(decl_context, name, decl_flags, locus);

    if (result != NULL)
    {
        scope_entry_t* head = entry_list_head(result);

        if (head->entity_specs.is_injected_class_name)
        {
            head = named_type_get_symbol(head->entity_specs.class_type);
        }

        if (head->entity_specs.is_member)
        {
            if (class_is_in_lexical_scope(top_level_decl_context, 
                        named_type_get_symbol(head->entity_specs.class_type))
                    && !class_has_dependent_bases(named_type_get_symbol(head->entity_specs.class_type)))
            {
                // Do nothing if the class is in scope
            }
            else if (BITMAP_TEST(decl_flags, DF_DEPENDENT_TYPENAME)
                    && !BITMAP_TEST(decl_flags, DF_DO_NOT_CREATE_UNQUALIFIED_DEPENDENT_ENTITY)
                    && is_dependent_type(head->entity_specs.class_type))
            {
                scope_entry_t* new_sym = counted_xcalloc(1, sizeof(*new_sym), &_bytes_used_scopes);
                new_sym->kind = SK_DEPENDENT_ENTITY;
                new_sym->symbol_name = nodecl_get_text(nodecl_name_get_last_part(nodecl_name));
                new_sym->decl_context = decl_context;
                new_sym->locus = locus;
                new_sym->type_information = build_dependent_typename_for_entry(
                        named_type_get_symbol(head->entity_specs.class_type),
                        nodecl_name,
                        locus);

                entry_list_free(result);

                return entry_list_new(new_sym);
            }
        }
    }

    return result;
}

static scope_entry_list_t* query_nodecl_simple_name_in_enum(
        decl_context_t decl_context,
        decl_context_t top_level_decl_context UNUSED_PARAMETER,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags UNUSED_PARAMETER)
{
    if (decl_context.current_scope->kind != CLASS_SCOPE)
    {
        internal_error("Code unreachable", 0);
    }

    const char* name = nodecl_get_text(nodecl_name);

    scope_entry_list_t* entry_list = query_name_in_scope(decl_context.class_scope, name);

    return entry_list;
}

static scope_entry_list_t* query_nodecl_simple_name_in_class(
        decl_context_t decl_context,
        decl_context_t top_level_decl_context,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags)
{
    if (decl_context.current_scope->kind != CLASS_SCOPE)
    {
        internal_error("Code unreachable", 0);
    }

    const locus_t* locus = nodecl_get_locus(nodecl_name);
    const char* name = nodecl_get_text(nodecl_name);

    if (BITMAP_TEST(decl_flags, DF_CONSTRUCTOR))
    {
        name = strappend("constructor ", name);
    }

    if (name[0]== '~')
    {
        // This is a destructor-id
        name++;

        // First: lookup inside the class
        scope_entry_list_t* entry_list = query_in_class(
                decl_context.current_scope,
                name,
                decl_flags,
                locus);

        // Second: lookup in the global context
        if (entry_list == NULL)
        {
            entry_list = name_lookup(
                    top_level_decl_context,
                    name,
                    decl_flags,
                    locus);
        }

        if (entry_list == NULL)
            return NULL;

        scope_entry_t* entry = entry_list_head(entry_list);
        scope_entry_t* current_class = decl_context.current_scope->related_entry;
        if (equivalent_types(get_user_defined_type(current_class), get_user_defined_type(entry)))
        {
            return entry_list_new(class_type_get_destructor(current_class->type_information));
        }
        else
        {
            return NULL;
        }
    }
    if (class_is_in_lexical_scope(top_level_decl_context, decl_context.current_scope->related_entry)
            // Do not give up if there are dependent bases as they might be
            // providing this entity during instantiation
            && !class_has_dependent_bases(decl_context.current_scope->related_entry))
    {
        // Do nothing if the class is in lexical scope
    }
    else if (BITMAP_TEST(decl_flags, DF_DEPENDENT_TYPENAME)
            && is_dependent_type(decl_context.current_scope->related_entry->type_information))
    {
        scope_entry_t* new_sym = counted_xcalloc(1, sizeof(*new_sym), &_bytes_used_scopes);
        new_sym->kind = SK_DEPENDENT_ENTITY;
        new_sym->decl_context = decl_context;
        new_sym->locus = locus;
        new_sym->symbol_name = nodecl_get_text(nodecl_name_get_last_part(nodecl_name));
        new_sym->type_information = build_dependent_typename_for_entry(
                decl_context.current_scope->related_entry,
                nodecl_name, 
                locus);

        return entry_list_new(new_sym);
    }

    return query_in_class(decl_context.current_scope, 
            name, 
            decl_flags,
            locus);
}

static scope_entry_list_t* query_nodecl_simple_name_in_namespace(
        decl_context_t decl_context,
        decl_context_t top_level_decl_context UNUSED_PARAMETER,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags)
{
    const locus_t* locus = nodecl_get_locus(nodecl_name);
    const char* name = nodecl_get_text(nodecl_name);

    return qualified_query_in_namespace(decl_context.current_scope->related_entry, 
            name,
            decl_flags,
            locus);
}

scope_entry_list_t* query_nodecl_template_id(
        decl_context_t decl_context, 
        decl_context_t top_level_decl_context, 
        nodecl_t nodecl_name, 
        decl_flags_t decl_flags,
        scope_entry_list_t* (*query_fun_nodecl)(
            decl_context_t current_context, 
            decl_context_t top_level_decl_context, 
            nodecl_t, 
            decl_flags_t)
        )
{
    nodecl_t simple_name = nodecl_get_child(nodecl_name, 0);

    template_parameter_list_t* template_parameters = nodecl_get_template_parameters(nodecl_name);

    if (template_parameters == NULL)
        return NULL;

    scope_entry_list_t* entry_list = query_fun_nodecl(
            decl_context, 
            top_level_decl_context,
            simple_name, 
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
        SK_USING_TYPENAME,
        SK_DEPENDENT_ENTITY
    };

    entry_list = filter_symbol_kind_set(entry_list, 
            STATIC_ARRAY_LENGTH(template_name_filter), 
            template_name_filter);

    if (entry_list == NULL)
        return NULL;

    scope_entry_t* template_symbol = entry_advance_aliases(entry_list_head(entry_list));

    if (template_symbol->kind == SK_DEPENDENT_ENTITY)
    {
        scope_entry_t* dependent_entity = NULL;
        nodecl_t nodecl_parts = nodecl_null();
        dependent_typename_get_components(template_symbol->type_information, &dependent_entity, &nodecl_parts);
        // nodecl_parts here lacks the template-id part

        scope_entry_t* new_sym = counted_xcalloc(1, sizeof(*new_sym), &_bytes_used_scopes);
        new_sym->kind = SK_DEPENDENT_ENTITY;
        new_sym->locus = nodecl_get_locus(nodecl_name);
        new_sym->symbol_name = dependent_entity->symbol_name;
        new_sym->decl_context = decl_context;
        new_sym->type_information = build_dependent_typename_for_entry(
            dependent_entity,
            nodecl_name,
            nodecl_get_locus(nodecl_name));

        entry_list_free(entry_list);
        return entry_list_new(new_sym);
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

        template_parameter_list_t* completed_template_parameters =
            complete_template_parameters_of_template_class(decl_context,
                    generic_type,
                    template_parameters,
                    nodecl_get_locus(nodecl_name));

        if (completed_template_parameters == NULL)
            return NULL;

        specialized_type = template_type_get_specialized_type(generic_type,
                completed_template_parameters,
                decl_context,
                nodecl_get_locus(nodecl_name));

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

typedef
struct same_type_conversion_info_tag
{
    type_t* t;
    decl_context_t decl_context;
} same_type_conversion_t;

static char same_type_conversion(scope_entry_t* entry, void *p)
{
    if (entry->kind == SK_FUNCTION)
    {
        struct same_type_conversion_info_tag* same_info = (struct same_type_conversion_info_tag*)p;
        type_t* t = same_info->t;
        decl_context_t decl_context = same_info->decl_context;

        type_t* conversion_type = function_type_get_return_type(entry->type_information);
        return equivalent_types_in_context(conversion_type, t, decl_context);
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

scope_entry_list_t* query_conversion_function_info(decl_context_t decl_context, type_t* t)
{
    ERROR_CONDITION(decl_context.class_scope == NULL, "We need a class scope here", 0);
    decl_context_t class_context = decl_context;
    class_context.current_scope = class_context.class_scope;

    struct same_type_conversion_info_tag same_info;
    same_info.t = t;
    // I guess the appropiate one is the class context!
    same_info.decl_context = class_context;

    scope_entry_list_t* entry_list = query_name_in_scope(class_context.class_scope, "$.operator");

    scope_entry_list_t* result = filter_symbol_using_predicate(entry_list, same_type_conversion, &same_info);

    entry_list_free(entry_list);

    return result;
}

static scope_entry_list_t* query_nodecl_conversion_name(
        decl_context_t decl_context,
        decl_context_t top_level_decl_context,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags UNUSED_PARAMETER)
{
    // We need a class scope around that we will check first
    if (decl_context.class_scope == NULL)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: conversion-id requires an enclosing class scope\n", 
                    nodecl_locus_to_str(nodecl_name));
        }
        return NULL;
    }

    // We kept this tree because of the complicated lookup required for conversion-id
    AST type_id = nodecl_get_ast(nodecl_get_child(nodecl_name, 2));
    ERROR_CONDITION(type_id == NULL, "Invalid node created by compute_nodecl_name_from_unqualified_id\n", 0);

    // Nullify tree so it won't bee freed afterwards if we discard this tree
    nodecl_set_child(nodecl_name, 2, nodecl_null());

    AST type_specifier_seq = ASTSon0(type_id);
    AST type_spec = ASTSon1(type_specifier_seq);

    // Build the type tree
    if (ASTType(type_spec) == AST_SIMPLE_TYPE_SPEC)
    {
        AST id_expression = ASTSon0(type_spec);

        decl_context_t expression_context =
            nodecl_get_decl_context(nodecl_get_child(nodecl_name, 0));

        nodecl_t nodecl_id_expression = nodecl_null();
        compute_nodecl_name_from_id_expression(id_expression, expression_context, &nodecl_id_expression);

        ast_set_child(type_specifier_seq, 1, nodecl_get_ast(nodecl_id_expression));
    }

    type_t* type_looked_up_in_class = get_error_type();
    type_t* type_looked_up_in_enclosing = get_error_type();

    decl_context_t class_context = decl_context;
    if (decl_context.class_scope != NULL)
    {
        // Lookup in class scope if available
        class_context.current_scope = class_context.class_scope;

        enter_test_expression();
        type_looked_up_in_class = compute_type_for_type_id_tree(type_id, class_context,
                /* out_simple_type */ NULL, /* out_gather_info */ NULL);
        leave_test_expression();
    }

    enter_test_expression();
    type_looked_up_in_enclosing = compute_type_for_type_id_tree(type_id, top_level_decl_context,
            /* out_simple_type */ NULL, /* out_gather_info */ NULL
            );
    leave_test_expression();

    type_t* t = type_looked_up_in_class;
    if (is_error_type(t))
    {
        t = type_looked_up_in_enclosing;
    }
    else
    {
        if (!is_error_type(type_looked_up_in_enclosing))
        {
            if (!equivalent_types(t, type_looked_up_in_enclosing))
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: type of conversion found in class scope (%s) and the type in "
                            "scope of the id-expression (%s) should match\n",
                            nodecl_locus_to_str(nodecl_name),
                            print_type_str(type_looked_up_in_class, class_context),
                            print_type_str(type_looked_up_in_enclosing, top_level_decl_context)
                            );
                }
                return NULL;
            }
        }
    }

    // If still not found, error
    if (is_error_type(t))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: type-id %s of conversion-id not found\n",
                    nodecl_locus_to_str(nodecl_name),
                    prettyprint_in_buffer(type_id));
        }
        return NULL;
    }

    if (class_context.class_scope == NULL)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: 'operator %s' requires a class scope\n", 
                    nodecl_locus_to_str(nodecl_name),
                    prettyprint_in_buffer(type_id));
        }
        return NULL;
    }

    // Keep the type
    nodecl_set_child(
            nodecl_name, 1,
            nodecl_make_type(t, nodecl_get_locus(nodecl_name)));

    return query_conversion_function_info(class_context, t);
}

static scope_entry_list_t* query_nodecl_qualified_name_internal(
        decl_context_t decl_context,
        decl_context_t current_context,
        scope_entry_t* previous_symbol,
        char allow_namespaces,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags,
        char check_symbol_in_nest(scope_entry_t* previous_sym, 
            scope_entry_t* current_sym, 
            char is_last, 
            const locus_t* locus,
            void* data),
        void *check_symbol_data)
{
    ERROR_CONDITION(nodecl_get_kind(nodecl_name) != NODECL_CXX_DEP_NAME_NESTED
            && nodecl_get_kind(nodecl_name) != NODECL_CXX_DEP_GLOBAL_NAME_NESTED,
            "Invalid nodecl", 0);

    decl_flags_t nested_flags = decl_flags;
    nested_flags &= ~DF_LABEL;
    nested_flags &= ~DF_CONSTRUCTOR;
    nested_flags &= ~DF_DEPENDENT_TYPENAME;

    int num_items = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(nodecl_name, 0), &num_items);

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
                        decl_context,
                        current_name,
                        nested_flags);
            }
            else
            {
                current_entry_list = query_nodecl_template_id(
                        current_context,
                        decl_context,
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
                        decl_context,
                        current_name,
                        nested_flags);
            }
            else
            {
                current_entry_list = query_nodecl_template_id(
                        current_context,
                        decl_context,
                        current_name, nested_flags,
                        query_nodecl_simple_name_in_namespace);
            }
        }
        else if (IS_CXX11_LANGUAGE && previous_symbol->kind == SK_ENUM)
        {
            // We only allow enums to be the previous symbol of the last component
            // of the nested name specifier.
            if (!checking_ambiguity())
            {
                error_printf("%s: error: invalid nested-name-specifier\n",
                        nodecl_locus_to_str(current_name));
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
            xfree(list);
            return NULL;
        }

        current_symbol = entry_advance_aliases(entry_list_head(current_entry_list));

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
                || (IS_CXX11_LANGUAGE && current_symbol->kind == SK_ENUM)
                || current_symbol->kind == SK_TYPEDEF
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
                            nodecl_get_locus(current_name),
                            list);

                    xfree(list);

                    return entry_list_new(dependent_symbol);
                }

                if (!is_named_type(t))
                {
                    if (!checking_ambiguity())
                    {
                        CXX03_LANGUAGE()
                        {
                            error_printf("%s: typedef name '%s' is not a namespace or class\n", 
                                    nodecl_locus_to_str(current_name),
                                    nodecl_get_text(current_name));
                        }
                        CXX11_LANGUAGE()
                        {
                            error_printf("%s: typedef name '%s' is not a namespace, class or enum\n", 
                                    nodecl_locus_to_str(current_name),
                                    nodecl_get_text(current_name));
                        }
                    }
                    return NULL;
                }

                current_symbol = named_type_get_symbol(t);
            }

            if (IS_CXX11_LANGUAGE && current_symbol->kind == SK_ENUM)
            {
                current_context = current_symbol->related_decl_context;
            }
            else if (current_symbol->kind == SK_CLASS)
            {
                type_t* class_type = current_symbol->type_information;

                if (class_is_in_lexical_scope(decl_context, current_symbol))
                {
                    // Do nothing if the class is in lexical scope
                }
                else if (template_class_needs_to_be_instantiated(current_symbol))
                {
                    instantiate_template_class_if_needed(current_symbol, current_symbol->decl_context,
                            nodecl_get_locus(current_name));
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
                            nodecl_get_locus(current_name),
                            list);
                    xfree(list);
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
                        nodecl_get_locus(current_name),
                        list);
                xfree(list);
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
                        nodecl_locus_to_str(current_name),
                        nodecl_get_text(current_name));
            }
            return NULL;
        }
        else
        {
            if (!checking_ambiguity())
            {
                CXX03_LANGUAGE()
                {
                    error_printf("%s: error: name '%s' is not a namespace or class\n", 
                            nodecl_locus_to_str(current_name),
                            nodecl_get_text(current_name));
                }
                CXX11_LANGUAGE()
                {
                    error_printf("%s: error: name '%s' is not a namespace, class or enum\n", 
                            nodecl_locus_to_str(current_name),
                            nodecl_get_text(current_name));
                }
            }
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
                    decl_context,
                    last_name,
                    decl_flags);
        }
        else if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_TEMPLATE_ID)
        {
            result = query_nodecl_template_id(
                    current_context,
                    decl_context,
                    last_name, decl_flags,
                    query_nodecl_simple_name_in_namespace);
        }
        else if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_NAME_CONVERSION)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: conversion-id is not valid in a non-class scope\n",
                        nodecl_locus_to_str(last_name));
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
                    decl_context,
                    last_name,
                    decl_flags);
        }
        else if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_TEMPLATE_ID)
        {
            result = query_nodecl_template_id(
                    current_context,
                    decl_context,
                    last_name, 
                    decl_flags,
                    query_nodecl_simple_name_in_class);
        }
        else if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_NAME_CONVERSION)
        {
            result = query_nodecl_conversion_name(current_context, decl_context,
                    last_name, decl_flags);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    else if (previous_symbol->kind == SK_ENUM
            && nodecl_get_kind(last_name) == NODECL_CXX_DEP_NAME_SIMPLE)
    {
        if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_NAME_SIMPLE)
        {
            result = query_nodecl_simple_name_in_enum(
                    current_context,
                    decl_context,
                    last_name,
                    decl_flags);
        }
        else
        {
            // Anything else is ill-formed
            if (!checking_ambiguity())
            {
                error_printf("%s: error: invalid name in nested-name specifier\n",
                        nodecl_locus_to_str(last_name));
            }
            return NULL;
        }
    }

    if (result != NULL
            && check_symbol_in_nest != NULL
            && !check_symbol_in_nest(previous_symbol, 
                entry_list_head(result),
                /* is_last */ 1,
                nodecl_get_locus(last_name),
                check_symbol_data))
    {
        xfree(result);
        return NULL;
    }

    xfree(list);

    return result;
}

static scope_entry_list_t* query_nodecl_qualified_name_common(decl_context_t decl_context,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags,
        char allow_namespaces,
        char check_symbol_in_nest(scope_entry_t* previous_sym,
            scope_entry_t* current_sym,
            char is_last,
            const locus_t* locus,
            void* data),
        void *check_symbol_data)
{
    scope_entry_t* previous_symbol = NULL;
    char is_global = (nodecl_get_kind(nodecl_name) == NODECL_CXX_DEP_GLOBAL_NAME_NESTED);

    decl_context_t current_context = decl_context;
    if (is_global)
    {
        current_context.current_scope = current_context.global_scope;
        previous_symbol = current_context.global_scope->related_entry;
        allow_namespaces = 1;
    }

    return query_nodecl_qualified_name_internal(
            decl_context,
            current_context,
            previous_symbol,
            allow_namespaces,
            nodecl_name,
            decl_flags,
            check_symbol_in_nest,
            check_symbol_data);
}

static scope_entry_list_t* query_nodecl_qualified_name(decl_context_t decl_context,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags)
{
    return query_nodecl_qualified_name_common(decl_context,
            nodecl_name,
            decl_flags,
            /* allow_namespaces */ 1,
            NULL, NULL);

}


static char check_symbol_is_base_or_member(scope_entry_t* previous_symbol, 
        scope_entry_t* current_symbol, 
        char is_last, 
        const locus_t* locus,
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

    if (class_symbol->kind != SK_CLASS)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: '%s' must be a class\n",
                    locus_to_str(locus),
                    class_symbol->symbol_name);
        }
        return 0;
    }

    if (is_last)
    {
        // If we are the last component we must be a member of class_symbol
        if (!(current_symbol->entity_specs.is_member
                    && class_type_is_base(
                        current_symbol->entity_specs.class_type,
                        get_user_defined_type(class_symbol))))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: '%s' is not a member of '%s'\n",
                        locus_to_str(locus),
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
                || !class_type_is_base(
                    get_user_defined_type(current_symbol),
                    get_user_defined_type(class_symbol)))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: '%s' is not a base of '%s'\n",
                        locus_to_str(locus),
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
    return query_nodecl_qualified_name_common(decl_context,
            nodecl_name,
            decl_flags,
            /* allow_namespaces */ 0,
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
                        decl_context,
                        nodecl_name, 
                        decl_flags);
                break;
            }
        case NODECL_CXX_DEP_TEMPLATE_ID:
            {
                return query_nodecl_template_id(
                        class_type_get_inner_context(class_symbol->type_information), 
                        decl_context,
                        nodecl_name, 
                        decl_flags, 
                        query_nodecl_simple_name_in_class);
                break;
            }
        case NODECL_CXX_DEP_NAME_CONVERSION:
            {
                return query_nodecl_conversion_name(
                        class_type_get_inner_context(class_symbol->type_information), 
                        decl_context,
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

    instantiate_template_class_if_needed(class_symbol, class_symbol->decl_context, 
            nodecl_get_locus(nodecl_name));

    decl_context_t inner_class_context = class_type_get_inner_context(class_type);
    if (inner_class_context.class_scope == NULL)
    {
        // There is no way this can suceed at all
        return NULL;
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
                return query_nodecl_simple_name(decl_context, decl_context, nodecl_name, decl_flags);
                break;
            }
        case NODECL_CXX_DEP_TEMPLATE_ID:
            {
                return query_nodecl_template_id(decl_context, decl_context, nodecl_name, decl_flags, 
                        query_nodecl_simple_name);
                break;
            }
        case NODECL_CXX_DEP_NAME_CONVERSION:
            {
                return query_nodecl_conversion_name(decl_context, decl_context, nodecl_name, decl_flags);
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
                        ast_get_locus(unqualified_id));
                break;
            }
        case AST_TEMPLATE_ID:
            {
                const char* name = ASTText(ASTSon0(unqualified_id));
                AST template_arguments = ASTSon1(unqualified_id);

                template_parameter_list_t* template_parameters = NULL;
                if (template_arguments != NULL &&
                        ASTType(template_arguments) == AST_AMBIGUITY)
                {
                    template_parameters =
                        solve_ambiguous_list_of_template_arguments(template_arguments, decl_context);
                }
                else
                {
                    template_parameters =
                        get_template_arguments_from_syntax(template_arguments, decl_context);
                }

                if (template_parameters == NULL)
                {
                    *nodecl_output = nodecl_make_err_expr(
                            ast_get_locus(unqualified_id));
                    return;
                }

                const char* template_tag = "";
                if (ast_get_text(unqualified_id) != NULL)
                    template_tag = ast_get_text(unqualified_id);

                *nodecl_output = nodecl_make_cxx_dep_template_id(
                        nodecl_make_cxx_dep_name_simple(
                            name,
                            ast_get_locus(unqualified_id)),
                        template_tag,
                        template_parameters,
                        ast_get_locus(unqualified_id));
                break;
            }
        case AST_OPERATOR_FUNCTION_ID:
            {
                const char* name = 
                        get_operator_function_name(unqualified_id);
                *nodecl_output = nodecl_make_cxx_dep_name_simple(
                        name,
                        ast_get_locus(unqualified_id));
                break;
            }
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE:
            {
                const char* name =
                        get_operator_function_name(unqualified_id);

                AST template_arguments = ASTSon1(unqualified_id);

                template_parameter_list_t* template_parameters = NULL;
                if (template_arguments != NULL &&
                        ASTType(template_arguments) == AST_AMBIGUITY)
                {
                    template_parameters =
                        solve_ambiguous_list_of_template_arguments(template_arguments, decl_context);
                }
                else
                {
                    template_parameters =
                        get_template_arguments_from_syntax(template_arguments, decl_context);
                }

                if (template_parameters == NULL)
                {
                    *nodecl_output = nodecl_make_err_expr(
                            ast_get_locus(unqualified_id));
                    return;
                }

                *nodecl_output = nodecl_make_cxx_dep_template_id(
                        nodecl_make_cxx_dep_name_simple(
                            name,
                            ast_get_locus(unqualified_id)),
                        /* template_tag */ "",
                        template_parameters,
                        ast_get_locus(unqualified_id));
                break;
            }
        case AST_CONVERSION_FUNCTION_ID:
            {
                *nodecl_output = nodecl_make_cxx_dep_name_conversion(
                        nodecl_make_context(
                            /* optional statement sequence */ nodecl_null(),
                            decl_context,
                            ast_get_locus(unqualified_id)),
                        nodecl_null(),
                        ast_get_locus(unqualified_id));

                // This is ugly but we need to keep the original tree around before lowering it into nodecl
                AST conversion_type_id = ASTSon0(unqualified_id);
                AST parent = ast_get_parent(conversion_type_id);

                ast_set_child(nodecl_get_ast(*nodecl_output), 2, conversion_type_id);

                // Make some work to prevent that ambiguities slip in
                // That ast_set_child actually botched the original AST, fix it
                ast_set_parent(conversion_type_id, parent);
                AST type_specifier_seq = ASTSon0(conversion_type_id);
                AST type_spec = ASTSon1(type_specifier_seq);

                // Build the type tree
                if (ASTType(type_spec) == AST_SIMPLE_TYPE_SPEC)
                {
                    AST id_expression = ASTSon0(type_spec);

                    nodecl_t nodecl_id_expression = nodecl_null();
                    // This will fix the tree as a side effect
                    compute_nodecl_name_from_id_expression(id_expression, decl_context, &nodecl_id_expression);
                }

                break;
            }
        case AST_DESTRUCTOR_ID:
            {
                AST symbol = ASTSon0(unqualified_id);
                *nodecl_output = nodecl_make_cxx_dep_name_simple(
                        ASTText(symbol), 
                        ast_get_locus(unqualified_id));
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
        if (ASTType(nested_it) == AST_AMBIGUITY)
        {
            solve_ambiguous_nested_part(nested_it, decl_context);
        }

        AST nested_name = ASTSon0(nested_it);
        compute_nodecl_name_from_unqualified_id(nested_name, 
                decl_context,
                &current);

        if (nodecl_is_err_expr(current))
        {
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(nested_part));
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
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(nested_part));
        return;
    }

    nodecl_t nodecl_unqualified = nodecl_null();
    compute_nodecl_name_from_unqualified_id(unqualified_part, decl_context, &nodecl_unqualified);

    if (nodecl_is_err_expr(nodecl_unqualified))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(unqualified_part));
        return;
    }

    nodecl_nested = nodecl_append_to_list(nodecl_nested, nodecl_unqualified);

    *nodecl_output = nodecl_make_cxx_dep_name_nested(nodecl_nested,
            ast_get_locus(unqualified_part));
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
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(nested_name_spec));
            return;
        }

        nodecl_t nodecl_unqualified = nodecl_null();
        compute_nodecl_name_from_unqualified_id(unqualified_id, decl_context, &nodecl_unqualified);

        if (nodecl_is_err_expr(nodecl_unqualified))
        {
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(unqualified_id));
            return;
        }

        nodecl_nested = nodecl_append_to_list(nodecl_nested, nodecl_unqualified);

        nodecl_t (*nodecl_nested_fun)(nodecl_t, const locus_t*) = nodecl_make_cxx_dep_name_nested;
        if (global_op != NULL)
        {
            nodecl_nested_fun = nodecl_make_cxx_dep_global_name_nested;
        }

        *nodecl_output = nodecl_nested_fun(nodecl_nested, ast_get_locus(unqualified_id));
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

    return query_in_class(decl_context.current_scope, name, decl_flags, make_locus("", 0, 0));
}

scope_entry_list_t* query_dependent_entity_in_context(decl_context_t decl_context,
        scope_entry_t* dependent_entity,
        const locus_t* locus)
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
                        decl_context, locus, /* pack_index */ -1);

                if (is_dependent_type(new_class_type))
                {
                    scope_entry_t* new_sym = counted_xcalloc(1, sizeof(*new_sym), &_bytes_used_scopes);
                    new_sym->kind = SK_DEPENDENT_ENTITY;
                    new_sym->locus = locus;
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
                        error_printf("%s: error: '%s' does not name a class type\n",
                                locus_to_str(locus),
                                print_type_str(dependent_entity->type_information, dependent_entity->decl_context));
                    }
                    return NULL;
                }
                else
                {
                    if (!nodecl_is_null(dependent_parts))
                    {
                        scope_entry_t* class_sym = named_type_get_symbol(new_class_type);

                        // Make sure class_type_get_inner_context does not return a bogus context below
                        instantiate_template_class_if_needed(class_sym, class_sym->decl_context, locus);

                        nodecl_t update_dependent_parts = update_dependent_typename_dependent_parts(
                                dependent_parts,
                                decl_context,
                                locus, /* pack_index */ -1);

                        ERROR_CONDITION(nodecl_get_kind(update_dependent_parts) == NODECL_CXX_DEP_GLOBAL_NAME_NESTED,
                                "This is not possible here", 0);
                        if (nodecl_get_kind(update_dependent_parts) != NODECL_CXX_DEP_NAME_NESTED)
                        {
                            return query_nodecl_name_flags(
                                    class_type_get_inner_context(class_sym->type_information),
                                    update_dependent_parts, DF_DEPENDENT_TYPENAME);
                        }
                        else
                        {
                            return query_nodecl_qualified_name_internal(
                                    decl_context,
                                    /* current_context */ class_type_get_inner_context(class_sym->type_information),
                                    class_sym,
                                    /* allow_namespaces */ 0,
                                    update_dependent_parts,
                                    DF_DEPENDENT_TYPENAME,
                                    check_symbol_is_base_or_member,
                                    class_sym);
                        }
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

const char* symbol_to_source(scope_entry_t* entry)
{
    const char* pack = pack_pointer("symbol", (void*)entry);

    const char* c = NULL;

    uniquestr_sprintf(&c, "%s%s%s", 
            "@SYMBOL-LITERAL-REF@(", pack, ")");

    return c;
}

static scope_entry_t* get_ultimate_symbol_from_module(scope_entry_t* entry)
{
    if (entry->entity_specs.from_module
            && entry->entity_specs.alias_to != NULL)
    {
        return get_ultimate_symbol_from_module(entry->entity_specs.alias_to);
    }
    else
    {
        return entry;
    }
}

void symbol_set_as_parameter_of_function(scope_entry_t* entry, scope_entry_t* function,
        int nesting,
        int position)
{
    ERROR_CONDITION(entry == NULL, "The symbol is null", 0);
    ERROR_CONDITION(function == NULL, "The function symbol should not be null", 0);
    ERROR_CONDITION(position < 0, "Invalid position %d\n", position);

    function = get_ultimate_symbol_from_module(function);

    int idx = -1;

    int i;
    for (i = 0; (i < entry->entity_specs.num_function_parameter_info) && (idx < 0); i++)
    {
        if (entry->entity_specs.function_parameter_info[i].function == function)
            idx = i;
    }

    if (idx < 0)
    {
        function_parameter_info_t function_parameter_info;
        memset(&function_parameter_info, 0, sizeof(function_parameter_info));

        function_parameter_info.function = function;
        function_parameter_info.nesting = nesting;
        function_parameter_info.position = position;

        P_LIST_ADD(entry->entity_specs.function_parameter_info,
                entry->entity_specs.num_function_parameter_info,
                function_parameter_info);
    }
    else
    {
        entry->entity_specs.function_parameter_info[idx].function = function;
        entry->entity_specs.function_parameter_info[idx].nesting = nesting;
        entry->entity_specs.function_parameter_info[idx].position = position;
    }
}

char symbol_is_parameter_of_function(scope_entry_t* entry, scope_entry_t* function)
{
    ERROR_CONDITION(entry == NULL, "The symbol is null", 0);
    ERROR_CONDITION(function == NULL, "The function symbol should not be null", 0);

    function = get_ultimate_symbol_from_module(function);

    int idx = -1;
    int i;
    for (i = 0; (i < entry->entity_specs.num_function_parameter_info) && (idx < 0); i++)
    {
        if (entry->entity_specs.function_parameter_info[i].function == function)
            idx = i;
    }

    return !(idx < 0);
}

int symbol_get_parameter_position_in_function(scope_entry_t* entry, scope_entry_t* function)
{
    ERROR_CONDITION(entry == NULL, "The symbol is null", 0);
    ERROR_CONDITION(function == NULL, "The function symbol should not be null", 0);

    function = get_ultimate_symbol_from_module(function);

    int i;
    for (i = 0; (i < entry->entity_specs.num_function_parameter_info); i++)
    {
        if (entry->entity_specs.function_parameter_info[i].function == function)
        {
            return entry->entity_specs.function_parameter_info[i].position;
        }
    }

    internal_error("This symbol is not a parameter of the function", 0);
}

int symbol_get_parameter_nesting_in_function(scope_entry_t* entry, scope_entry_t* function)
{
    ERROR_CONDITION(entry == NULL, "The symbol is null", 0);
    ERROR_CONDITION(function == NULL, "The function symbol should not be null", 0);

    function = get_ultimate_symbol_from_module(function);

    int i;
    for (i = 0; (i < entry->entity_specs.num_function_parameter_info); i++)
    {
        if (entry->entity_specs.function_parameter_info[i].function == function)
        {
            return entry->entity_specs.function_parameter_info[i].nesting;
        }
    }

    internal_error("This symbol is not a parameter of the function", 0);
}

char is_dependent_function(scope_entry_t* entry)
{
    ERROR_CONDITION(entry == NULL, "The symbol is null", 0);

    return is_dependent_type(entry->type_information)
        || (entry->kind == SK_DEPENDENT_FRIEND_FUNCTION)
        || (entry->entity_specs.is_member
                && is_dependent_type(entry->entity_specs.class_type));
}

nodecl_t symbol_get_aligned_attribute(scope_entry_t* entry)
{
    ERROR_CONDITION(entry == NULL, "Invalid symbol", 0);
    int i;
    for (i = 0; i < entry->entity_specs.num_gcc_attributes; i++)
    {
        if (strcmp(entry->entity_specs.gcc_attributes[i].attribute_name, "aligned") == 0)
        {
            return nodecl_list_head(entry->entity_specs.gcc_attributes[i].expression_list);
        }
    }
    return nodecl_null();
}
