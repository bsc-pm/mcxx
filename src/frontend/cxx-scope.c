/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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
#include "dhash_ptr.h"


// Lookup of a simple name within a given declaration context
static scope_entry_list_t* name_lookup(const decl_context_t* decl_context, 
        const char* name, 
        field_path_t* field_path,
        decl_flags_t decl_flags,
        const locus_t* locus);

void field_path_init(field_path_t* field_path)
{
    ERROR_CONDITION(field_path == NULL, "Invalid field path", 0);

    field_path->length = 0;
}

static void field_path_add(field_path_t* field_path, scope_entry_t* symbol)
{
    if (field_path == NULL)
        return;

    ERROR_CONDITION(field_path->length == MCXX_MAX_FIELD_PATH, "Too many symbols in field path", 0);

    field_path->path[field_path->length] = symbol;
    field_path->length++;
}

#if 0
static void field_path_prepend(field_path_t* field_path, scope_entry_t* symbol)
{
    if (field_path == NULL)
        return;

    ERROR_CONDITION(field_path->length == MCXX_MAX_FIELD_PATH, "Too many symbols in field path", 0);

    // Shift right
    int i;
    for (i = field_path->length - 1; i >= 0; i--)
    {
        field_path->path[i+1] = field_path->path[i];
    }
    field_path->path[0] = symbol;
    field_path->length++;
}
#endif

template_parameter_list_t* duplicate_template_argument_list(template_parameter_list_t* tpl)
{
    ERROR_CONDITION(tpl == NULL, "Template parameters cannot be NULL here", 0);

    template_parameter_list_t* result = NEW0(template_parameter_list_t);
    result->num_parameters = tpl->num_parameters;
    result->parameters = tpl->parameters;

    result->arguments = NEW_VEC0(template_parameter_value_t*, tpl->num_parameters);
    int i;
    for (i = 0; i < result->num_parameters; i++)
    {
        if (tpl->arguments[i] != NULL)
        {
            result->arguments[i] = NEW(template_parameter_value_t);
            *result->arguments[i] = *tpl->arguments[i];
            result->arguments[i]->value = nodecl_shallow_copy(tpl->arguments[i]->value);
        }
    }

    result->enclosing = tpl->enclosing;
    result->is_explicit_specialization = tpl->is_explicit_specialization;

    return result;
}

void free_template_parameter_list(template_parameter_list_t* tpl)
{
    if (tpl == NULL)
        return;

    int i;
    for (i = 0; i < tpl->num_parameters; i++)
    {
        if (tpl->arguments[i] != NULL)
        {
            nodecl_free(tpl->arguments[i]->value);
        }
        DELETE(tpl->arguments[i]);
    }
    DELETE(tpl->arguments);
    DELETE(tpl);
}

static void copy_template_parameter_list(template_parameter_list_t* dest, template_parameter_list_t* src)
{
    ERROR_CONDITION(src == NULL, "Invalid source", 0);

    int i;

    memset(dest, 0, sizeof(*dest));
    dest->enclosing = src->enclosing;
    dest->parameters = NULL;
    if (src->parameters != NULL)
    {
        dest->parameters = NEW_VEC0(template_parameter_t*, src->num_parameters);
        memcpy(dest->parameters, src->parameters, src->num_parameters * sizeof(*src->parameters));
    }
    dest->arguments = NEW_VEC0(template_parameter_value_t*, src->num_parameters);
    for (i = 0; i < src->num_parameters; i++)
    {
        if (src->arguments[i] != NULL)
        {
            dest->arguments[i] = NEW(template_parameter_value_t);
            *dest->arguments[i] = *src->arguments[i];
            dest->arguments[i]->value = nodecl_shallow_copy(src->arguments[i]->value);
        }
    }
    dest->is_explicit_specialization = src->is_explicit_specialization;
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

// static int strcmp_vptr(const void* v1, const void *v2)
// {
//     return strcmp((const char*)v1, (const char*) v2);
// }

// static int uniquestr_vptr(const void* v1, const void* v2)
// {
//     if (v1 < v2)
//         return -1;
//     else if (v1 > v2)
//         return 1;
//     else
//         return 0;
// }

// Any new scope should be created using this one
scope_t* _new_scope(void)
{
    scope_t* result = NEW0(scope_t);

    result->dhash = dhash_ptr_new(5);

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

// Creates an anew const decl_context_t*
static decl_context_t* new_decl_context(void)
{
    decl_context_t* result = NEW0(decl_context_t);
    return result;
}

// Creates a new global context
decl_context_t* new_global_context(void)
{
    decl_context_t* result = new_decl_context();

    scope_entry_t* global_scope_namespace = NEW0(scope_entry_t);
    global_scope_namespace->kind = SK_NAMESPACE;

    // Create global scope
    result->namespace_scope = new_namespace_scope(NULL, global_scope_namespace);

    // Make it the global one
    result->global_scope = result->namespace_scope;
    // and the current one
    result->current_scope = result->namespace_scope;

    global_scope_namespace->related_decl_context = result;
    global_scope_namespace->decl_context = result;

    return result;
}

decl_context_t* decl_context_clone(const decl_context_t* t)
{
    decl_context_t* result = NEW(decl_context_t);
    *result = *t;

    return result;
}

decl_context_t* new_namespace_context(const decl_context_t* enclosing_decl_context, 
        scope_entry_t* related_entry)
{
    ERROR_CONDITION((enclosing_decl_context->current_scope->kind != NAMESPACE_SCOPE),
            "Enclosing scope must be namespace scope", 0);
    ERROR_CONDITION((enclosing_decl_context->current_scope != enclosing_decl_context->namespace_scope), 
            "Enclosing namespace scope has a mismatch between its current scope and its namespace scope", 0);

    // Inherit current context
    decl_context_t* result = decl_context_clone(enclosing_decl_context);

    // Create a new namespace scope contained in the previous one
    result->namespace_scope = new_namespace_scope(enclosing_decl_context->namespace_scope, 
            related_entry);
    // And make it the current one
    result->current_scope = result->namespace_scope;

    return result;
}

decl_context_t* new_prototype_context(const decl_context_t* enclosing_decl_context)
{
    // Inherit current context
    decl_context_t* result = decl_context_clone(enclosing_decl_context);

    // Create the prototype scope
    result->prototype_scope = new_prototype_scope(enclosing_decl_context->current_scope);
    // and make it the current scope
    result->current_scope = result->prototype_scope;

    return result;
}


decl_context_t* new_block_context(const decl_context_t* enclosing_context)
{
    ERROR_CONDITION(enclosing_context->current_scope->kind != NAMESPACE_SCOPE
            && enclosing_context->current_scope->kind != CLASS_SCOPE
            && enclosing_context->current_scope->kind != BLOCK_SCOPE,
            "Enclosing scope is neither namespace nor class scope", 0);

    // Inherit block context
    decl_context_t* result = decl_context_clone(enclosing_context);

    // Create new block scope
    result->block_scope = new_block_scope(result->current_scope);

    // And update the current scope
    result->current_scope = result->block_scope;

    // Update the related entry if any
    if (enclosing_context->current_scope->kind == BLOCK_SCOPE)
    {
        result->current_scope->related_entry = enclosing_context->current_scope->related_entry;
    }

    return result;
}

decl_context_t* new_function_context(const decl_context_t* enclosing_context)
{
    // Inherit the scope
    decl_context_t* result = decl_context_clone(enclosing_context);
    // Create a new function scope for it
    result->function_scope = new_function_scope();
    // Note that we are not changing the current_scope, since nothing
    // works on it.
    return result;
}

decl_context_t* new_class_context(const decl_context_t* enclosing_context, 
        scope_entry_t* class_entry)
{
    ERROR_CONDITION(enclosing_context->current_scope->kind != NAMESPACE_SCOPE
            && enclosing_context->current_scope->kind != CLASS_SCOPE
            && enclosing_context->current_scope->kind != BLOCK_SCOPE, /* The last case yields a local class */
            "Enclosing scope is neither namespace, class or local", 0
            );

    ERROR_CONDITION(class_entry->kind != SK_CLASS && class_entry->kind != SK_ENUM, "This is not a class or enum", 0);

    // Inherit the scope
    decl_context_t* result = decl_context_clone(enclosing_context);

    // Create new class scope
    result->class_scope = new_class_scope(enclosing_context->current_scope, class_entry);

    // And make it the current one
    result->current_scope = result->class_scope;

    return result;
}

void insert_alias(scope_t* sc, scope_entry_t* entry, const char* name)
{
    ERROR_CONDITION(name == NULL ||
            *name == '\0', "Insert alias called with an empty or NULL string", 0);

    const char* symbol_name = uniquestr(name);

    scope_entry_list_t* result_set = (scope_entry_list_t*)dhash_ptr_query(sc->dhash, symbol_name);

    if (result_set != NULL)
    {
        result_set = entry_list_prepend(result_set, entry);
    }
    else
    {
        result_set = entry_list_new(entry);
    }

    dhash_ptr_insert(sc->dhash, symbol_name, result_set);
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

scope_entry_t* new_symbol(const decl_context_t* decl_context, scope_t* sc, const char* name)
{
    ERROR_CONDITION(name == NULL ||
            *name == '\0', "New symbol called with an empty or NULL string", 0);

    // ERROR_CONDITION(name != uniquestr(name), "Invalid name", 0);

    scope_entry_t* result = NEW0(scope_entry_t);

    result->symbol_name = uniquestr(name);
    result->decl_context = decl_context;

    insert_alias(sc, result, result->symbol_name);

    return result;
}

char same_scope(scope_t* stA, scope_t* stB)
{
    return (stA->dhash == stB->dhash);
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

    scope_entry_list_t *result = (scope_entry_list_t*)dhash_ptr_query(sc->dhash, name);

    // ERROR_CONDITION(name != uniquestr(name), "Invalid name", 0);

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
    // ERROR_CONDITION(entry->symbol_name != uniquestr(entry->symbol_name), "Name of symbol not canonical", 0);

    scope_entry_list_t* result_set = (scope_entry_list_t*)dhash_ptr_query(sc->dhash, entry->symbol_name);

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
            dhash_ptr_insert(sc->dhash, entry->symbol_name, result_set);
        }
    }
    else
    {
        result_set = entry_list_new(entry);
        dhash_ptr_insert(sc->dhash, entry->symbol_name, result_set);
    }
}

void remove_entry(scope_t* sc, scope_entry_t* entry)
{
    scope_entry_list_t* entry_list = dhash_ptr_query(sc->dhash, entry->symbol_name);
    if (entry_list == NULL)
        return;

    entry_list = entry_list_remove(entry_list, entry);

    if (entry_list_size(entry_list) > 1)
    {
        dhash_ptr_insert(sc->dhash, entry->symbol_name, entry_list);
    }
    else
    {
        dhash_ptr_remove(sc->dhash, entry->symbol_name);
    }
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
        is_friend = symbol_entity_specs_get_is_friend_declared(template_sym);
    }
    else
    {
        is_friend =  symbol_entity_specs_get_is_friend_declared(entry);
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

static void compute_nodecl_name_from_unqualified_id(AST unqualified_id, const decl_context_t* decl_context, nodecl_t* nodecl_output);

scope_entry_list_t* query_id_expression_flags(const decl_context_t* decl_context,
        AST id_expression,
        field_path_t* field_path,
        decl_flags_t decl_flags)
{
    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_id_expression(id_expression, decl_context, &nodecl_name);

    if (nodecl_is_err_expr(nodecl_name))
        return NULL;

    scope_entry_list_t* result = query_nodecl_name_flags(decl_context, nodecl_name, field_path, decl_flags);

    nodecl_free(nodecl_name);

    return result;
}

/*
 * Query functions
 */
scope_entry_list_t* query_nested_name_flags(const decl_context_t* decl_context, 
        AST global_op, 
        AST nested_name, 
        AST unqualified_name,
        field_path_t* field_path,
        decl_flags_t decl_flags)
{
    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_qualified_name(global_op, nested_name, unqualified_name, decl_context, &nodecl_name);

    if (nodecl_is_err_expr(nodecl_name))
        return NULL;

    scope_entry_list_t* result = query_nodecl_name_flags(decl_context, nodecl_name, field_path, decl_flags);

    nodecl_free(nodecl_name);

    return result;
}

scope_entry_list_t* query_in_scope_flags(const decl_context_t* decl_context,
        AST unqualified_name,
        field_path_t *field_path,
        decl_flags_t decl_flags)
{
    decl_flags |= DF_ONLY_CURRENT_SCOPE;

    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_unqualified_id(unqualified_name, decl_context, &nodecl_name);

    if (nodecl_is_err_expr(nodecl_name))
        return NULL;

    scope_entry_list_t* result = query_nodecl_name_flags(decl_context, nodecl_name, field_path, decl_flags);

    nodecl_free(nodecl_name);

    return result;
}

scope_entry_list_t* query_name_str_flags(const decl_context_t* decl_context,
        const char* unqualified_name,
        field_path_t* field_path,
        decl_flags_t decl_flags)
{
    nodecl_t nodecl_name = nodecl_make_cxx_dep_name_simple(unqualified_name, make_locus("", 0, 0));

    scope_entry_list_t* result = query_nodecl_name_flags(decl_context, nodecl_name, field_path, decl_flags);

    nodecl_free(nodecl_name);

    return result;
}

scope_entry_list_t* query_in_scope_str_flags(const decl_context_t* decl_context,
        const char* name, field_path_t* field_path,
        decl_flags_t decl_flags)
{
    return query_name_str_flags(decl_context, name, field_path, decl_flags | DF_ONLY_CURRENT_SCOPE);
}


static scope_entry_list_t* qualified_query_in_namespace(scope_entry_t* namespace, 
        const char* name, decl_flags_t decl_flags,
        const locus_t* locus);
static scope_entry_list_t* query_in_class(scope_t* current_class_scope, 
        const char* name,
        field_path_t* field_path,
        decl_flags_t decl_flags,
        type_t* type_of_conversion,
        const locus_t* locus);

static void build_dependent_parts_for_symbol_rec(
        scope_entry_t* entry,
        const locus_t* locus,
        scope_entry_t** dependent_entry,
        nodecl_t* nodecl_output);

char symbol_is_member_of_dependent_class(scope_entry_t* entry)
{
    return symbol_entity_specs_get_is_member(entry)
        && is_dependent_type(symbol_entity_specs_get_class_type(entry));
}

char symbol_is_local_of_dependent_function(scope_entry_t* entry)
{
    return entry->decl_context->current_scope->kind == BLOCK_SCOPE
        && entry->decl_context->current_scope->related_entry->kind == SK_FUNCTION
        && (is_dependent_type(entry->decl_context->current_scope->related_entry->type_information)
                || symbol_is_member_of_dependent_class(entry));
}

scope_entry_t* get_function_or_class_where_symbol_depends(scope_entry_t* entry)
{
    if (symbol_is_member_of_dependent_class(entry))
    {
        return named_type_get_symbol(symbol_entity_specs_get_class_type(entry));
    }
    else if (symbol_is_local_of_dependent_function(entry))
    {
        return entry->decl_context->current_scope->related_entry;
    }
    else
    {
        internal_error("This symbol is not in a dependent class or function\n", 0);
    }
}

scope_entry_t* class_symbol_get_canonical_symbol(scope_entry_t* class_symbol)
{
    ERROR_CONDITION(class_symbol->kind != SK_CLASS, "Invalid symbol", 0);

    if (symbol_entity_specs_get_alias_to(class_symbol) != NULL)
        return symbol_entity_specs_get_alias_to(class_symbol);

    return class_symbol;
}

static char class_is_immediate_lexical_scope(const decl_context_t* decl_context,
        scope_entry_t* class_symbol)
{
    ERROR_CONDITION(class_symbol->kind != SK_CLASS, "Invalid symbol", 0);
    if (symbol_entity_specs_get_is_injected_class_name(class_symbol))
    {
        class_symbol = named_type_get_symbol(symbol_entity_specs_get_class_type(class_symbol));
    }

    if (decl_context->class_scope == NULL)
        return 0;

    scope_entry_t* class_in_scope = decl_context->class_scope->related_entry;

    if (class_in_scope->kind == SK_ENUM)
        return 0;

    if (class_symbol_get_canonical_symbol(class_symbol)
            == class_symbol_get_canonical_symbol(class_in_scope))
    {
        return 1;
    }

    return 0;
}

char class_is_in_lexical_scope(const decl_context_t* decl_context, 
        scope_entry_t* class_symbol)
{
    ERROR_CONDITION(class_symbol->kind != SK_CLASS, "Invalid symbol", 0);
    if (symbol_entity_specs_get_is_injected_class_name(class_symbol))
    {
        class_symbol = named_type_get_symbol(symbol_entity_specs_get_class_type(class_symbol));
    }

    if (decl_context->class_scope == NULL)
        return 0;

    scope_entry_t* class_in_scope = decl_context->class_scope->related_entry;

    if (class_in_scope->kind == SK_ENUM)
        return 0;

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
            const decl_context_t* class_context = class_type_get_inner_context(enclosing_class);
            return class_is_in_lexical_scope(class_context, class_symbol);
        }
    }

    return 0;
}

char compute_type_of_dependent_conversion_type_id(
        nodecl_t nodecl_name,
        const decl_context_t* decl_context)
{
    // This is weird, the left hand side of this class-member access is
    // dependent, so technically we cannot check anything in its scope
    // (maybe it is a class), so we will go with the current one.
    //
    // In C++2003 technically this would lead to an ambiguous case once the
    // function is instantiated, since we still have to do a double lookup and
    // verify that it matches, but there could exist a T::C<T*> and a ::C<T*>.
    //
    // In C++2011 we have to prioritize the class lookup before the lookup in the
    // postfix-expression, so C<T*> will always be solved by the postfix context.
    //
    // template <typename T>
    // void f()
    // {
    //   T t;
    //   t.operator C<T*>(); // C<T*> or T::C<T*> ??? We will try the first one
    //   // Note that if the former is desired we can always write 'typename T::C<T*>'
    //   // so this problem can always be worked around
    // }
    nodecl_t nodecl_type = nodecl_get_child(nodecl_name, 1);
    AST type_id = nodecl_get_ast(nodecl_get_child(nodecl_name, 2));
    ERROR_CONDITION((type_id == NULL)
            == nodecl_is_null(nodecl_type),
            "Invalid node created by compute_nodecl_name_from_unqualified_id\n", 0);

    if (!nodecl_is_null(nodecl_type))
        return 1;

    // Nullify tree so it won't bee freed afterwards if we discard this tree.
    nodecl_set_child(nodecl_name, 2, nodecl_null());

    AST type_specifier_seq = ASTSon0(type_id);
    AST type_spec = ASTSon1(type_specifier_seq);

    // Build the type tree
    if (ASTKind(type_spec) == AST_SIMPLE_TYPE_SPEC)
    {
        AST id_expression = ASTSon0(type_spec);

        const decl_context_t* expression_context =
            nodecl_get_decl_context(nodecl_get_child(nodecl_name, 0));

        nodecl_t nodecl_id_expression = nodecl_null();
        compute_nodecl_name_from_id_expression(id_expression, expression_context, &nodecl_id_expression);

        ast_set_child(type_specifier_seq, 1, nodecl_get_ast(nodecl_id_expression));
    }

    diagnostic_context_push_buffered();
    type_t* t = compute_type_for_type_id_tree(type_id, decl_context,
            /* out_simple_type */ NULL, /* out_gather_info */ NULL);
    diagnostic_context_pop_and_discard();

    // If not found, error
    if (is_error_type(t))
    {
        error_printf_at(
                nodecl_get_locus(nodecl_name),
                "type-id %s of conversion-id not found\n",
                prettyprint_in_buffer(type_id));
        return 0;
    }

    // Do not let these nodes outlive the FE
    ast_free(type_id);

    // Keep the type
    nodecl_set_child(
            nodecl_name, 1,
            nodecl_make_type(t, nodecl_get_locus(nodecl_name)));

    return 1;
}

static scope_entry_t* create_new_dependent_entity(
        const decl_context_t* decl_context,
        scope_entry_t* dependent_entry,
        int nested_name_index,
        int nested_name_size,
        const locus_t* locus,
        nodecl_t* parts)
{
    nodecl_t nodecl_list = nodecl_null();
    scope_entry_t* updated_dependent_entry = NULL;

    if (is_dependent_typename_type(dependent_entry->type_information))
    {
        dependent_typename_get_components(dependent_entry->type_information,
                &updated_dependent_entry, &nodecl_list);

        nodecl_list = nodecl_shallow_copy(nodecl_get_child(nodecl_list, 0));
    }
    else
    {
        if (dependent_entry->kind == SK_TYPEDEF)
        {
            type_t* t = advance_over_typedefs(dependent_entry->type_information);
            if (is_named_type(t))
                dependent_entry = named_type_get_symbol(t);
        }

        ERROR_CONDITION(!is_valid_symbol_for_dependent_typename(dependent_entry),
                "Unexpected symbol '%s' of kind '%s'\n",
                dependent_entry->symbol_name,
                symbol_kind_name(dependent_entry));


        if (dependent_entry->kind == SK_CLASS)
        {
            build_dependent_parts_for_symbol_rec(dependent_entry,
                    locus, &updated_dependent_entry, &nodecl_list);
        }
    }

    if (updated_dependent_entry == NULL)
        updated_dependent_entry = dependent_entry;

    int i;
    for (i = nested_name_index + 1; i < nested_name_size; i++)
    {
        nodecl_t current_part = nodecl_shallow_copy(parts[i]);
        nodecl_list = nodecl_append_to_list(nodecl_list, current_part);
        if (nodecl_get_kind(current_part) == NODECL_CXX_DEP_NAME_CONVERSION)
        {
            char ok = compute_type_of_dependent_conversion_type_id(
                    current_part,
                    decl_context);
            if (!ok)
                return NULL;
        }
    }

    nodecl_t nodecl_parts = nodecl_make_cxx_dep_name_nested(nodecl_list, make_locus("", 0, 0));

    type_t* dependent_type = get_dependent_typename_type_from_parts(updated_dependent_entry, nodecl_parts);

    nodecl_free(nodecl_parts);

    static dhash_ptr_t *_dependent_symbols = NULL;
    if (_dependent_symbols == NULL)
    {
        _dependent_symbols = dhash_ptr_new(5);
    }

    // Try to reuse an existing symbol through its dependent type
    scope_entry_t * result = dhash_ptr_query(_dependent_symbols, (const char*)dependent_type);

    if (result == NULL)
    {
        result = NEW0(scope_entry_t);

        result->kind = SK_DEPENDENT_ENTITY;
        result->decl_context = decl_context;
        result->symbol_name = dependent_entry->symbol_name;
        result->type_information = dependent_type;

        dhash_ptr_insert(_dependent_symbols, (const char*)dependent_type, result);
    }

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

    if (symbol_entity_specs_get_is_constructor(entry))
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

    if (symbol_entity_specs_get_is_injected_class_name(entry))
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

typedef
struct same_type_conversion_info_tag
{
    type_t* t;
    const decl_context_t* decl_context;
} same_type_conversion_t;

static char same_type_conversion(scope_entry_t* entry, void *p)
{
    if (entry->kind == SK_FUNCTION)
    {
        struct same_type_conversion_info_tag* same_info = (struct same_type_conversion_info_tag*)p;
        type_t* t = same_info->t;
        // const decl_context_t* decl_context = same_info->decl_context;

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


void class_scope_lookup_rec(scope_t* current_class_scope, const char* name, 
        class_scope_lookup_t* derived,
        char is_virtual,
        char initial_lookup,
        decl_flags_t decl_flags,
        type_t* type_of_conversion,
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

    if (type_of_conversion != NULL)
    {
        scope_entry_list_t* old_entry_list = entry_list;
        struct same_type_conversion_info_tag same_info;
        same_info.t = type_of_conversion;
        same_info.decl_context = class_type_get_inner_context(current_class_scope->related_entry->type_information);

        entry_list = filter_symbol_using_predicate(entry_list, same_type_conversion, &same_info);
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
    else if (is_enum_type(current_class_type))
    {
        // All done for enums
    }
    else if (is_class_type(current_class_type))
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
                const decl_context_t* base_class_context = class_type_get_inner_context(base_class_type);
                scope_t* base_class_scope = base_class_context->current_scope;

                // Get the info from the base
                class_scope_lookup_t current_base_info = *derived;

                // Lookup in the base
                class_scope_lookup_rec(base_class_scope, name, &current_base_info,
                        current_base_is_virtual, /* initial_lookup */ 0, decl_flags,
                        type_of_conversion, locus);

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
                            class_type_get_inner_context(derived->path[i])->current_scope->related_entry->symbol_name);
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
                                    class_type_get_inner_context(derived->path[k])->current_scope->related_entry->symbol_name,
                                    ((k+1) < derived->path_length) ? "::" : "");
                        }
                        fprintf(stderr, "'\n");
                        fprintf(stderr, "SCOPE: Second one is '");
                        for (k = 0; k < current->path_length; k++)
                        {
                            fprintf(stderr, "%s%s%s", 
                                    current->is_virtual[k] ? "<v>" : "",
                                    class_type_get_inner_context(current->path[k])->current_scope->related_entry->symbol_name,
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
                                        class_type_get_inner_context(derived->path[k])->current_scope->related_entry->symbol_name,
                                        ((k+1) < derived->path_length) ? "::" : "");
                            }
                            fprintf(stderr, "'\n");
                            fprintf(stderr, "SCOPE: Second one is '");
                            for (k = 0; k < current->path_length; k++)
                            {
                                fprintf(stderr, "%s%s%s", 
                                        current->is_virtual[k] ? "<v>" : "",
                                        class_type_get_inner_context(current->path[k])->current_scope->related_entry->symbol_name,
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
                                            class_type_get_inner_context(derived->path[path_candidate])->current_scope->related_entry->symbol_name,
                                            class_type_get_inner_context(current->path[path_current])->current_scope->related_entry->symbol_name);

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
                                valid = valid && symbol_entity_specs_get_is_static(entry);
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
                            else if (entry->kind == SK_CLASS)
                            {
                                valid = 0;
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "SCOPE: Ambiguous base name %s\n",
                                            name);
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
    else
    {
        internal_error("Code unreachable", 0);
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

        DELETE(list);

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

        if (!symbol_entity_specs_get_is_anonymous_union(entry))
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
        ERROR_CONDITION(symbol_entity_specs_get_is_anonymous_union(entry), 
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

    nodecl_t nodecl_current;

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

    nodecl_t dependent_parts;
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
        field_path_t* field_path UNUSED_PARAMETER,
        decl_flags_t decl_flags,
        type_t* type_of_conversion, // Only used for conversions
        const locus_t* locus)
{
    class_scope_lookup_t result;
    memset(&result, 0, sizeof(result));

    class_scope_lookup_rec(current_class_scope, name, &result, 0, /* initial_lookup */ 1, decl_flags, type_of_conversion, locus);

    if (result.entry_list != NULL)
    {
        int i;
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Class scope lookup started in class '%s' found name '%s' in '", 
                    current_class_scope->related_entry->symbol_name,
                    name);
            for (i = 0; i < result.path_length; i++)
            {
                fprintf(stderr, "%s%s", 
                        class_or_enum_type_get_inner_context(result.path[i])->current_scope->related_entry->symbol_name,
                        ((i+1) < result.path_length) ? "::" : "");
            }
            fprintf(stderr, "'\n");
        }

        scope_entry_t* looked_up_symbol = entry_list_head(result.entry_list);

        if (symbol_entity_specs_get_is_injected_class_name(looked_up_symbol))
        {
            looked_up_symbol = named_type_get_symbol(symbol_entity_specs_get_class_type(looked_up_symbol));
        }

        for (i = 0; i < result.path_length; i++)
        {
            scope_entry_t* class_symbol = class_or_enum_type_get_inner_context(result.path[i])->current_scope->related_entry;
            if (class_symbol == looked_up_symbol)
                break;
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
    const char* candidates;

    uniquestr_sprintf(&candidates, "%s: info: candidates are\n", locus_to_str(locus));

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        const char *single_candidate;
        uniquestr_sprintf(&single_candidate, "%s: info:    %s\n", 
                locus_to_str(entry->locus),
                get_qualified_symbol_name(entry, entry->decl_context));

        candidates = strappend(candidates, single_candidate);
    }
    entry_list_iterator_free(it);

    error_printf_at(locus, "ambiguity in reference to '%s'\n%s", entry_list_head(entry_list)->symbol_name, candidates);
}

static char check_for_naming_ambiguity(scope_entry_list_t* entry_list, const locus_t* locus)
{
    if (entry_list == NULL
            || entry_list_size(entry_list) == 1)
        return 1;

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
                    || (hiding_name->decl_context->current_scope == entry->decl_context->current_scope)))
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
            entry_list_iterator_free(it);
            return 0;
        }
    }
    entry_list_iterator_free(it);

    return 1;
}

static scope_entry_list_t* entry_list_merge_aliases(scope_entry_list_t* list1, scope_entry_list_t* list2)
{
    scope_entry_list_t* real_list = NULL;
    scope_entry_list_t* result = NULL;

    scope_entry_list_t* lists[] = { list1, list2 };

    int i;
    for (i = 0; i < 2; i++)
    {
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(lists[i]);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_entry = entry_list_iterator_current(it);
            scope_entry_t* real_sym = entry_advance_aliases(current_entry);

            if (!entry_list_contains(real_list, real_sym))
            {
                result = entry_list_add(result, current_entry);
            }
            real_list = entry_list_add_once(real_list, real_sym);
        }
        entry_list_iterator_free(it);
    }

    entry_list_free(real_list);

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
    scope_t* current_scope = namespace->related_decl_context->current_scope;
    scope_entry_list_t* grand_result 
        = query_name_in_scope(current_scope, name);

    int i;
    for (i = 0; i < num_associated_namespaces; i++)
    {
        if (associated_namespaces[i].visited)
            continue;

        scope_t* scope_of_nominated = associated_namespaces[i].nominated->decl_context->current_scope;

        if (scope_is_enclosed_by(scope_of_nominated, current_scope)
                && scope_is_enclosed_by(associated_namespaces[i].scope_of_using, current_scope))
        {
            scope_t* associated_namespace = associated_namespaces[i].nominated->related_decl_context->current_scope;

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
                    SK_TEMPLATE_TYPE_PARAMETER_PACK,
                    SK_TEMPLATE_TEMPLATE_PARAMETER,
                    SK_TEMPLATE_TEMPLATE_PARAMETER_PACK,
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
                    SK_TEMPLATE_TYPE_PARAMETER_PACK,
                    SK_TEMPLATE_TEMPLATE_PARAMETER,
                    SK_TEMPLATE_TEMPLATE_PARAMETER_PACK,
                    SK_TEMPLATE,
                    SK_GCC_BUILTIN_TYPE
                };

                scope_entry_list_t* old_result = grand_result;
                grand_result = filter_symbol_kind_set(old_result, STATIC_ARRAY_LENGTH(type_filter), type_filter);
                entry_list_free(old_result);
            }

            if (!check_for_naming_ambiguity(grand_result, locus))
                return NULL;
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
    scope_t* namespace_scope = namespace->related_decl_context->current_scope;
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
            if (visited_namespaces[j] == used_namespace)
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
                SK_TEMPLATE_TYPE_PARAMETER_PACK,
                SK_TEMPLATE_TEMPLATE_PARAMETER,
                SK_TEMPLATE_TEMPLATE_PARAMETER_PACK,
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
                SK_TEMPLATE_TYPE_PARAMETER_PACK,
                SK_TEMPLATE_TEMPLATE_PARAMETER,
                SK_TEMPLATE_TEMPLATE_PARAMETER_PACK,
                SK_TEMPLATE,
                SK_GCC_BUILTIN_TYPE
            };

            scope_entry_list_t* old_result = grand_result;
            grand_result = filter_symbol_kind_set(old_result, STATIC_ARRAY_LENGTH(type_filter), type_filter);
            entry_list_free(old_result);
        }

        if (!check_for_naming_ambiguity(grand_result, locus))
            return NULL;
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
        associated_namespace_t **associated_namespaces)
{
    int i;
    for (i = 0; i < current_scope->num_used_namespaces; i++)
    {
        int j;
        char found = 0;

        // Inlines are the only ones considered when doing a "only current
        // scope" lookup
        if (BITMAP_TEST(decl_flags, DF_ONLY_CURRENT_SCOPE)
                && !symbol_entity_specs_get_is_inline(current_scope->use_namespace[i]))
            continue;

        for (j = 0; j < (*num_associated_namespaces) && !found; j++)
        {
            found = ((*associated_namespaces)[j].nominated == current_scope->use_namespace[i]);
        }
        if (!found)
        {
            associated_namespace_t assoc_namespace;
            assoc_namespace.scope_of_using = scope_of_using;
            assoc_namespace.nominated = current_scope->use_namespace[i];
            assoc_namespace.visited = 0;

            P_LIST_ADD(*associated_namespaces, *num_associated_namespaces, assoc_namespace);

            transitive_add_using_namespaces(decl_flags,
                    scope_of_using,
                    current_scope->use_namespace[i]->related_decl_context->current_scope,
                    num_associated_namespaces, associated_namespaces);
        }
    }
}

static scope_entry_list_t* name_lookup(const decl_context_t* decl_context,
        const char* name,
        field_path_t* field_path,
        decl_flags_t decl_flags,
        const locus_t* locus)
{
    ERROR_CONDITION(name == NULL, "Name cannot be null!", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Name lookup of '%s'\n", name);
    }

    // TEMPLATE_SCOPE is specially handled always
    template_parameter_list_t* template_parameters = decl_context->template_parameters;
    while (template_parameters != NULL)
    {
        int i;
        for (i = 0; i < template_parameters->num_parameters; i++)
        {
            template_parameter_t* tpl = template_parameters->parameters[i];

            if (tpl != NULL // This could be null because of variadic template arguments
                    && tpl->entry != NULL
                    && tpl->entry->symbol_name == name)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Name '%s' found in template parameter list. Nesting: %d. Position: %d\n",
                            name,
                            symbol_entity_specs_get_template_parameter_nesting(tpl->entry),
                            symbol_entity_specs_get_template_parameter_position(tpl->entry));
                }

                return entry_list_new(tpl->entry);
            }
        }

        template_parameters = template_parameters->enclosing;
    }

    scope_entry_list_t* result = NULL;

    associated_namespace_t* associated_namespaces = NULL;
    int num_associated_namespaces = 0;

    scope_t* current_scope = decl_context->current_scope;

    while (result == NULL
            && current_scope != NULL)
    {
        transitive_add_using_namespaces(decl_flags, 
                current_scope, 
                current_scope,
                &num_associated_namespaces, 
                &associated_namespaces);

        if (current_scope->kind == CLASS_SCOPE)
        {
            if (!BITMAP_TEST(decl_flags, DF_ONLY_CURRENT_SCOPE))
            {
                result = query_in_class(current_scope, name,
                        field_path,
                        decl_flags,
                        NULL,
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
                SK_TEMPLATE_TYPE_PARAMETER_PACK,
                SK_TEMPLATE_TEMPLATE_PARAMETER,
                SK_TEMPLATE_TEMPLATE_PARAMETER_PACK,
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
                SK_TEMPLATE_TYPE_PARAMETER_PACK,
                SK_TEMPLATE_TEMPLATE_PARAMETER,
                SK_TEMPLATE_TEMPLATE_PARAMETER_PACK,
                SK_TEMPLATE,
                SK_GCC_BUILTIN_TYPE
            };

            scope_entry_list_t* old_result = result;
            result = filter_symbol_kind_set(old_result, STATIC_ARRAY_LENGTH(type_filter), type_filter);
            entry_list_free(old_result);
        }

        // The first unqualified-id of a nested-name-specifier
        if (BITMAP_TEST(decl_flags, DF_NESTED_NAME_FIRST))
        {
            enum cxx_symbol_kind nested_name_first_cxx03[] = {
                SK_CLASS,
                SK_NAMESPACE,
                SK_TYPEDEF,
                SK_TEMPLATE_TYPE_PARAMETER,
                SK_USING,
            };

            enum cxx_symbol_kind nested_name_first_cxx11[] = {
                SK_CLASS,
                SK_NAMESPACE,
                SK_TYPEDEF,
                SK_TEMPLATE_TYPE_PARAMETER,
                SK_USING,
                // C++11
                SK_ENUM,
                SK_TEMPLATE_TYPE_PARAMETER_PACK,
            };

            scope_entry_list_t* old_result = result;
            CXX03_LANGUAGE()
            {
                result = filter_symbol_kind_set(old_result,
                        STATIC_ARRAY_LENGTH(nested_name_first_cxx03),
                        nested_name_first_cxx03);
            }
            CXX11_LANGUAGE()
            {
                result = filter_symbol_kind_set(old_result,
                        STATIC_ARRAY_LENGTH(nested_name_first_cxx11),
                        nested_name_first_cxx11);
            }
            entry_list_free(old_result);

            // Now verify typedefs
            if (result != NULL)
            {
                scope_entry_t* alias_name = entry_list_head(result);
                alias_name = entry_advance_aliases(alias_name);

                if (alias_name->kind == SK_TYPEDEF)
                {
                    type_t* t = advance_over_typedefs(alias_name->type_information);

                    if (!is_class_type(t)
                            && !is_dependent_typename_type(t)
                            && !is_typeof_expr(t)
                            && !(is_named_type(t)
                                && named_type_get_symbol(t)->kind == SK_TEMPLATE_TYPE_PARAMETER)
                            && !(IS_CXX11_LANGUAGE
                                && is_named_type(t)
                                && named_type_get_symbol(t)->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK))
                    {
                        // This cannot be a class-name at all
                        entry_list_free(result);
                        result = NULL;
                    }
                }
            }
        }

        if (BITMAP_TEST(decl_flags, DF_ONLY_CURRENT_SCOPE))
        {
            DELETE(associated_namespaces);
            return result;
        }

        current_scope = current_scope->contained_in;
    }

    DELETE(associated_namespaces);

    return result;
}

static nodecl_t update_nodecl_template_argument_expression(
        nodecl_t nodecl,
        const decl_context_t* decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map,
        int pack_index)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Updating expression '%s'\n", 
                codegen_to_str(nodecl, decl_context));
    }

    nodecl_t nodecl_output = nodecl_null();

    nodecl_t nodecl_inst = instantiate_expression(nodecl, decl_context,
            instantiation_symbol_map, /* pack_index */ pack_index);

    if (nodecl_is_null(nodecl_inst)
            || nodecl_is_list(nodecl_inst))
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
                DELETE(list);
                return nodecl_item;
            }
            list[i] = nodecl_item;
        }

        nodecl_output = nodecl_make_list_n(num_items, list);
        DELETE(list);
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
        const decl_context_t* decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map,
        int pack_index)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Updating expression '%s'\n", 
                codegen_to_str(nodecl, decl_context));
    }

    nodecl = instantiate_expression(nodecl, decl_context,
            instantiation_symbol_map, pack_index);

    if (!nodecl_is_constant(nodecl)
            && !nodecl_expr_is_value_dependent(nodecl))
    {
        error_printf_at(nodecl_get_locus(nodecl), "expression '%s' is not constant\n",
                codegen_to_str(nodecl, decl_context));
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
                && !nodecl_is_null(value->value)
                && nodecl_get_kind(value->value) == NODECL_CXX_VALUE_PACK));
}

static type_t* update_type_aux_(type_t* orig_type, 
        const decl_context_t* decl_context,
        const locus_t* locus,
        instantiation_symbol_map_t* instantiation_symbol_map,
        int pack_index);

static template_parameter_value_t* update_template_parameter_value_aux(
        template_parameter_value_t* v,
        const decl_context_t* decl_context,
        char is_template_class,
        instantiation_symbol_map_t* instantiation_symbol_map,
        const locus_t* locus,
        int pack_index)
{
    template_parameter_value_t* result = NEW0(template_parameter_value_t);
    result->kind = v->kind;
    result->is_default = 0;

    if (result->kind == TPK_TYPE
            || result->kind == TPK_TEMPLATE)
    {
        type_t* updated_type = update_type_aux_(v->type, decl_context, locus,
                instantiation_symbol_map, pack_index);
        if (updated_type == NULL)
        {
            DELETE(result);
            return NULL;
        }

        if (is_template_class)
        {
            updated_type = advance_over_typedefs(updated_type);
        }

        result->type = updated_type;
    }
    else if (result->kind == TPK_NONTYPE)
    {
        if (!nodecl_is_null(v->value))
        {
            if (nodecl_is_list(v->value))
            {
                int num_items;
                nodecl_t* list = nodecl_unpack_list(v->value, &num_items);
                int i;
                nodecl_t updated_list = nodecl_null();

                type_t* type_sequence = NULL;

                for (i = 0; i < num_items; i++)
                {
                    nodecl_t updated_expr =
                        update_nodecl_template_argument_expression(list[i], decl_context,
                                instantiation_symbol_map,
                                pack_index);

                    if (nodecl_is_err_expr(updated_expr))
                    {
                        DELETE(result);
                        nodecl_free(updated_list);
                        return NULL;
                    }

                    updated_list = nodecl_append_to_list(updated_list, updated_expr);

                    type_sequence = get_sequence_of_types_append_type(type_sequence, nodecl_get_type(updated_expr));
                }

                result->value = updated_list;
                result->type = type_sequence;
            }
            else
            {
                result->value =
                    update_nodecl_template_argument_expression(v->value, decl_context,
                            instantiation_symbol_map, pack_index);

                if (!nodecl_is_null(result->value))
                {
                    if (nodecl_is_err_expr(result->value))
                    {
                        DELETE(result);
                        return NULL;
                    }

                    result->type = nodecl_get_type(result->value);
                }
                else
                {
                    result->type = get_sequence_of_types(0, NULL);
                }
            }
        }
    }

    return result;
}

template_parameter_value_t* update_template_parameter_value_of_template_class(
        template_parameter_value_t* v,
        const decl_context_t* decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map,
        const locus_t* locus,
        int pack_index)
{
    return update_template_parameter_value_aux(v, decl_context, /* is_template_class */ 1,
            instantiation_symbol_map, locus, pack_index);
}

template_parameter_value_t* update_template_parameter_value(
        template_parameter_value_t* v,
        const decl_context_t* decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map,
        const locus_t* locus,
        int pack_index)
{
    return update_template_parameter_value_aux(v, decl_context, /* is_template_class */ 0,
            instantiation_symbol_map, locus, pack_index);
}

template_parameter_list_t* update_template_argument_list(
        const decl_context_t* decl_context,
        template_parameter_list_t* dependent_type_template_arguments,
        instantiation_symbol_map_t* instantiation_symbol_map,
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
                instantiation_symbol_map,
                locus,
                pack_index);

        if (result->arguments[i] == NULL)
        {
            free_template_parameter_list(result);
            return NULL;
        }
    }

    return result;
}

static nodecl_t update_dependent_typename_dependent_parts(
        nodecl_t dependent_parts,
        const decl_context_t* decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map,
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
                        instantiation_symbol_map,
                        locus,
                        pack_index);

            nodecl_set_template_parameters(new_current_part, new_template_arguments);
        }

        new_dependent_parts_list = nodecl_append_to_list(new_dependent_parts_list,
                new_current_part);
    }

    DELETE(list);

    nodecl_t new_dependent_parts = nodecl_make_cxx_dep_name_nested(new_dependent_parts_list, locus);

    return new_dependent_parts;
}

static type_t* update_dependent_typename(
        type_t* orig_dependent_type,
        type_t* dependent_entry_type,
        nodecl_t dependent_parts,
        const decl_context_t* decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map,
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

    if (nodecl_is_null(dependent_parts))
    {
        return get_user_defined_type(dependent_entry);
    }

    scope_entry_t* current_member = dependent_entry;

    char possible = class_type_complete_if_possible(current_member, current_member->decl_context, locus);

    if (!possible)
        return NULL;

    nodecl_t new_dependent_parts = update_dependent_typename_dependent_parts(dependent_parts, decl_context,
            instantiation_symbol_map, locus, pack_index);

    field_path_t field_path;
    field_path_init(&field_path);

    scope_entry_list_t* entry_list = query_nodecl_name_flags(
            class_type_get_inner_context(current_member->type_information),
            new_dependent_parts, &field_path, DF_DEPENDENT_TYPENAME);

    if (entry_list == NULL)
    {
        error_printf_at(locus, "type '%s' does not refer to an existing entity\n",
                print_type_str(orig_dependent_type, decl_context));
        return NULL;
    }

    scope_entry_t* member = entry_list_head(entry_list);

    if (member->kind == SK_USING)
        member = symbol_entity_specs_get_alias_to(member);

    entry_list_free(entry_list);

    if (member->kind == SK_CLASS
            || member->kind == SK_TYPEDEF
            || member->kind == SK_ENUM)
    {
        if (is_error_type(member->type_information))
            return NULL;

        return get_user_defined_type(member);
    }
    else if (member->kind == SK_TEMPLATE_ALIAS)
    {
        if (is_error_type(member->type_information))
            return NULL;

        return member->type_information;
    }
    else if (member->kind == SK_DEPENDENT_ENTITY)
    {
        if (is_error_type(member->type_information))
            return NULL;

        return member->type_information;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: After updating dependent typename the obtained symbol has kind '%s' which is not a valid type\n",
                    symbol_kind_name(member));
        }
        error_printf_at(locus, "type '%s' does not refer to a valid typename\n",
                print_type_str(orig_dependent_type, decl_context));
        return NULL;
    }
}


static int get_length_of_pack_expansion_common(int num_packs_to_expand,
        scope_entry_t** packs_to_expand,
        const decl_context_t* decl_context,
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
                    symbol_entity_specs_get_template_parameter_nesting(packs_to_expand[i]),
                    symbol_entity_specs_get_template_parameter_position(packs_to_expand[i]));
            if (argument != NULL && is_sequence_of_types(argument->type_information))
            {
                P_LIST_ADD(expanded_types, num_expanded_types, argument->type_information);
            }
        }
        else if (packs_to_expand[i]->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK)
        {
            scope_entry_t* argument = lookup_of_template_parameter(
                    decl_context,
                    symbol_entity_specs_get_template_parameter_nesting(packs_to_expand[i]),
                    symbol_entity_specs_get_template_parameter_position(packs_to_expand[i]));
            if (argument != NULL &&
                    (nodecl_is_null(argument->value) // An empty sequence
                     || nodecl_is_list(argument->value)))
            {
                P_LIST_ADD(expanded_values, num_expanded_values, argument->value);
            }
        }
        else if (packs_to_expand[i]->kind == SK_VARIABLE_PACK)
        {
            // Can be sequence of types
            if (is_sequence_of_types(packs_to_expand[i]->type_information))
            {
                P_LIST_ADD(expanded_types, num_expanded_types, packs_to_expand[i]->type_information);
            }
        }
        else
        {
            internal_error("Code unreachable %s", symbol_kind_name(packs_to_expand[i]));
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
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    scope_entry_t** packs_to_expand = NULL;
    int num_packs_to_expand = 0;

    get_packs_in_expression(expr, &packs_to_expand, &num_packs_to_expand);

    int len = get_length_of_pack_expansion_common(num_packs_to_expand, packs_to_expand, decl_context, locus);

    DELETE(packs_to_expand);

    return len;
}

int get_length_of_pack_expansion_from_type(type_t* pack_type,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    scope_entry_t** packs_to_expand = NULL;
    int num_packs_to_expand = 0;

    get_packs_in_type(pack_type_get_packed_type(pack_type), &packs_to_expand, &num_packs_to_expand);

    int len = get_length_of_pack_expansion_common(num_packs_to_expand, packs_to_expand, decl_context, locus);

    DELETE(packs_to_expand);

    return len;
}

type_t* update_type_for_auto(type_t* t, type_t* template_parameter)
{
    if (is_auto_type(t)
            || is_decltype_auto_type(t))
    {
        return get_cv_qualified_type(template_parameter,
                get_cv_qualifier(t) | get_cv_qualifier(template_parameter));
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
        return get_lvalue_reference_type(no_ref(result));
    }
    else if (is_rvalue_reference_type(t))
    {
        type_t* result = update_type_for_auto(reference_type_get_referenced_type(t), template_parameter);
        if (is_any_reference_type(result))
            return result;
        else
            return get_rvalue_reference_type(result);
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
                get_vector_type_by_bytes(
                    update_type_for_auto(vector_type_get_element_type(t), template_parameter),
                    vector_type_get_vector_size_in_bytes(t)),
                get_cv_qualifier(t));
    }
    else
    {
        return t;
    }
}

static type_t* update_pack_type(type_t* pack_type,
        const decl_context_t* decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map,
        const locus_t* locus)
{
    int len = get_length_of_pack_expansion_from_type(pack_type, decl_context, locus);
    if (len < 0)
    {
        // Simply update the packed type
        type_t* packed_type = pack_type_get_packed_type(pack_type);
        type_t* updated_packed_type = update_type_aux_(packed_type, decl_context, locus,
                instantiation_symbol_map,
                /* pack_index */ -1);

        if (updated_packed_type == NULL)
            return NULL;

        return get_pack_type(updated_packed_type);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Expansion pack length is '%d'\n", len);
    }

    type_t** types = NULL;
    types = NEW_VEC0(type_t*, len);
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
                instantiation_symbol_map,
                i);
        if (types[i] != NULL)
            types[i] = get_cv_qualified_type(types[i],
                    // Add cv-qualification
                    get_cv_qualifier(types[i])
                    | get_cv_qualifier(pack_type_get_packed_type(pack_type)));
        if (types[i] == NULL)
        {
            DELETE(types);
            return NULL;
        }
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Pack expanded in item %d expanded to '%s'\n",
                    i, print_declarator(types[i]));
        }
    }

    type_t* result = get_sequence_of_types(len, types);
    DELETE(types);

    return result;
}

static char add_mapping_for_return_type_symbol(scope_entry_t* entry,
        const decl_context_t* decl_context,
        const locus_t* locus,
        int pack_index,
        instantiation_symbol_map_t* instantiation_symbol_map,
        int *num_new_symbols,
        scope_entry_t*** new_symbols)
{
    if (instantiation_symbol_do_map(instantiation_symbol_map, entry) != NULL)
        return 1;

    type_t* new_type =
        update_type_aux_(entry->type_information,
                decl_context,
                locus,
                instantiation_symbol_map,
                pack_index);

    if (new_type == NULL)
        return 0;

    scope_entry_t* new_entry = NEW0(scope_entry_t);
    new_entry->symbol_name = entry->symbol_name;
    new_entry->kind = entry->kind;
    new_entry->decl_context = decl_context;
    new_entry->type_information = new_type;
    new_entry->locus = entry->locus;

    scope_entry_t* proxy = get_function_declaration_proxy();
    symbol_set_as_parameter_of_function(new_entry,
            proxy,
            symbol_get_parameter_nesting_in_function(entry, proxy),
            symbol_get_parameter_position_in_function(entry, proxy));

    instantiation_symbol_map_add(instantiation_symbol_map,
            entry,
            new_entry);

    P_LIST_ADD(*new_symbols, *num_new_symbols, new_entry);

    if (new_entry->kind == SK_VARIABLE_PACK
            && is_sequence_of_types(new_entry->type_information))
    {
        int num_types = sequence_of_types_get_num_types(new_entry->type_information);

        nodecl_t nodecl_sym_list = nodecl_null();

        int num_sub_parameter;
        for (num_sub_parameter = 0; num_sub_parameter < num_types; num_sub_parameter++)
        {
            type_t* t = sequence_of_types_get_type_num(new_entry->type_information,
                    num_sub_parameter);

            const char* c = NULL;
            uniquestr_sprintf(
                    &c, "_%s__%d",
                    new_entry->symbol_name,
                    num_sub_parameter);

            scope_entry_t* new_sub_parameter = NEW0(scope_entry_t);

            new_sub_parameter->symbol_name = c;
            new_sub_parameter->kind = SK_VARIABLE;
            new_sub_parameter->decl_context = decl_context;
            new_sub_parameter->type_information = t;

            nodecl_t nodecl_sub_symbol = nodecl_make_symbol(
                    new_sub_parameter,
                    locus);
            nodecl_set_type(nodecl_sub_symbol, lvalue_ref(new_sub_parameter->type_information));
            nodecl_sym_list = nodecl_append_to_list(nodecl_sym_list,
                    nodecl_sub_symbol);

            P_LIST_ADD(*new_symbols, *num_new_symbols, new_sub_parameter);
        }

        new_entry->value = nodecl_sym_list;
    }

    return 1;
}

static char add_mappings_for_return_type_expr(nodecl_t node,
        const decl_context_t* decl_context,
        const locus_t* locus,
        int pack_index,
        instantiation_symbol_map_t* instantiation_symbol_map,
        int *num_new_symbols,
        scope_entry_t*** new_symbols)
{
    if (nodecl_is_null(node))
        return 1;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        char ok = add_mappings_for_return_type_expr(
                nodecl_get_child(node, i),
                decl_context,
                locus,
                pack_index,
                instantiation_symbol_map,
                num_new_symbols,
                new_symbols);

        if (!ok)
            return 0;
    }

    scope_entry_t* entry = nodecl_get_symbol(node);

    if (entry != NULL
            && (entry->kind == SK_VARIABLE
                || entry->kind == SK_VARIABLE_PACK)
            && symbol_is_parameter_of_function(entry, get_function_declaration_proxy()))
    {
        char ok = add_mapping_for_return_type_symbol(
                entry,
                decl_context,
                locus,
                pack_index,
                instantiation_symbol_map,
                num_new_symbols,
                new_symbols);

        if (!ok)
            return 0;
    }

    return 1;
}

static char add_mappings_for_return_type(type_t* t,
        const decl_context_t* decl_context,
        const locus_t* locus,
        int pack_index,
        instantiation_symbol_map_t* return_instantiation_symbol_map,
        int *num_new_symbols,
        scope_entry_t*** new_symbols)
{
    if (t == NULL)
        return 1;

    if (is_typeof_expr(t))
    {
        char ok = add_mappings_for_return_type_expr(
                typeof_expr_type_get_expression(t),
                decl_context,
                locus,
                pack_index,
                return_instantiation_symbol_map,
                num_new_symbols,
                new_symbols);
        if (!ok)
            return 0;
    }
    else if (is_pointer_type(t))
    {
        char ok = add_mappings_for_return_type(
                pointer_type_get_pointee_type(t),
                decl_context,
                locus,
                pack_index,
                return_instantiation_symbol_map,
                num_new_symbols,
                new_symbols);

        if (!ok)
            return 0;
    }
    else if (is_pointer_to_member_type(t))
    {
        char ok = add_mappings_for_return_type(
                pointer_type_get_pointee_type(t),
                decl_context,
                locus,
                pack_index,
                return_instantiation_symbol_map,
                num_new_symbols,
                new_symbols);

        if (!ok)
            return 0;
    }
    else if (is_any_reference_type(t))
    {
        char ok = add_mappings_for_return_type(
                reference_type_get_referenced_type(t),
                decl_context,
                locus,
                pack_index,
                return_instantiation_symbol_map,
                num_new_symbols,
                new_symbols);

        if (!ok)
            return 0;
    }
    else if (is_array_type(t))
    {
        char ok = add_mappings_for_return_type(
                array_type_get_element_type(t),
                decl_context,
                locus,
                pack_index,
                return_instantiation_symbol_map,
                num_new_symbols,
                new_symbols);

        if (!ok)
            return 0;

        ok = add_mappings_for_return_type_expr(
                array_type_get_array_size_expr(t),
                decl_context,
                locus,
                pack_index,
                return_instantiation_symbol_map,
                num_new_symbols,
                new_symbols);

        if (!ok)
            return 0;
    }
    else if (is_dependent_typename_type(t))
    {
        scope_entry_t* dependent_entry = NULL;
        nodecl_t dependent_parts = nodecl_null();

        dependent_typename_get_components(t,
                &dependent_entry, &dependent_parts);

        char ok = add_mappings_for_return_type(
                dependent_entry->type_information,
                decl_context,
                locus,
                pack_index,
                return_instantiation_symbol_map,
                num_new_symbols,
                new_symbols);

        if (!ok)
            return 0;
    }

    return 1;
}

static template_parameter_list_t* complete_template_parameters_of_template_class(
        const decl_context_t* template_name_context,
        type_t* template_type,
        template_parameter_list_t* template_parameters,
        const locus_t* locus);

static type_t* update_type_aux_(type_t* orig_type, 
        const decl_context_t* decl_context,
        const locus_t* locus,
        // may be null
        instantiation_symbol_map_t* instantiation_symbol_map,
        // -1 if not expanding any pack
        int pack_index)
{
    ERROR_CONDITION(orig_type == NULL, "Error, type is null", 0);

    if (is_named_type(orig_type))
    {
        scope_entry_t* orig_symbol = named_type_get_symbol(orig_type);

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
        if (orig_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER
                || orig_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                // template packs
                || orig_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                || orig_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
        {
            scope_entry_t* new_sym = lookup_of_template_parameter(
                    decl_context,
                    symbol_entity_specs_get_template_parameter_nesting(orig_symbol),
                    symbol_entity_specs_get_template_parameter_position(orig_symbol));

            if (new_sym == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: No update performed for template parameter '%s'"
                            " since a template symbol for it was not found\n",
                            print_declarator(orig_type));
                }
                return orig_type;
            }

            // Note that for template-types (not specializations) do not have cv-qualifiers
            cv_qualifier_t cv_qualif_orig = CV_NONE;
            advance_over_typedefs_with_cv_qualif(orig_type, &cv_qualif_orig);

            cv_qualifier_t cv_qualif_new = CV_NONE;
            advance_over_typedefs_with_cv_qualif(new_sym->type_information, &cv_qualif_new);

            if (orig_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER
                        && new_sym->kind == SK_TEMPLATE_TYPE_PARAMETER)
            {
                // A type template parameter replaced by another one
                return get_cv_qualified_type(get_user_defined_type(new_sym), cv_qualif_orig | cv_qualif_new);
            }
            else if (orig_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER
                    && new_sym->kind == SK_TYPEDEF)
            {
                // A type template parameter replaced by another type
                //
                // Note that we have to add the qualification
                //
                //   template <typename _T, typename _Q = volatile _T>
                //   struct A { };
                //
                //   A<const int>   ->   A<const int, const volatile int>

                return get_cv_qualified_type(new_sym->type_information, cv_qualif_orig | cv_qualif_new);
            }
            else if (orig_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                    && (new_sym->kind == SK_TEMPLATE
                        || new_sym->kind == SK_TEMPLATE_TEMPLATE_PARAMETER))
            {
                // A template template parameter replaced by a template-name (SK_TEMPLATE) or
                // another template template parameter (SK_TEMPLATE_TEMPLATE_PARAMETER)
                return get_user_defined_type(new_sym);
            }
            else if (orig_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                    && new_sym->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK)
            {
                // A type template parameter pack replaced by another template
                // type parameter pack
                return get_cv_qualified_type(get_user_defined_type(new_sym), cv_qualif_orig | cv_qualif_new);
            }
            else if (orig_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                    && new_sym->kind == SK_TYPEDEF)
            {
                // This happens when the template pack is used as a type-name _inside_
                // a pack expansion.
                //
                // template <typename T, typename M = T*>
                // struct B { };
                //
                // template <typename ...S>
                // struct A
                // {
                //      void f(B<S>...);
                // };
                //
                // B<S>... has to expand into B<S, S*>... but note that the new_sym S is a pack
                // while M is a typedef (the type of which is T*)

                return get_cv_qualified_type(new_sym->type_information, cv_qualif_orig | cv_qualif_new);
            }
            else if (orig_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                    && new_sym->kind == SK_TYPEDEF_PACK)
            {
                if (is_named_type(new_sym->type_information)
                        && named_type_get_symbol(new_sym->type_information)->kind == SK_TEMPLATE_TYPE_PARAMETER)
                {
                    // This happens when unification has unified a pack with another T... <- S... (may happen
                    // when ordering two partial specializations)
                    return get_cv_qualified_type(new_sym->type_information, cv_qualif_orig | cv_qualif_new);
                }
                else
                {
                    if (pack_index < 0)
                    {
                        if (is_pack_type(new_sym->type_information))
                            // If we are not expanding and this is a pack return the packed type
                            // and let the caller build another pack
                            return get_cv_qualified_type(
                                    pack_type_get_packed_type(new_sym->type_information),
                                    cv_qualif_orig | cv_qualif_new);
                        else
                            // otherwise return the sequence of types as a whole
                            return get_cv_qualified_type(
                                    new_sym->type_information,
                                    cv_qualif_orig | cv_qualif_new);
                    }
                    else
                    {
                        // We are expanding a pack, return the requested index in
                        // the sequence

                        type_t* item = sequence_of_types_get_type_num(
                                new_sym->type_information,
                                pack_index);

                        return get_cv_qualified_type(item, cv_qualif_orig | cv_qualif_new);
                    }
                }
            }
            else if (orig_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK
                    && new_sym->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
            {
                // A template-template parameter pack being replaced with
                // another template-template parameter pack
                ERROR_CONDITION(!is_pack_type(new_sym->type_information),
                        "This type should be a pack type but it is '%s'", print_declarator(new_sym->type_information));
                return new_sym->type_information;
            }
            else if (orig_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK
                    && new_sym->kind == SK_TEMPLATE_PACK)
            {
                // A template template parameter pack replaced by a sequence of templates
                ERROR_CONDITION(!is_sequence_of_types(new_sym->type_information),
                        "This type should be a sequence of types but it is '%s'", print_declarator(new_sym->type_information));
                // FIXME - We may need to add up the cv-qualifier to the
                // members of the sequence type
                if (pack_index < 0)
                {
                    // If we are not expanding. Return the sequence as a whole
                    return new_sym->type_information;
                }
                else
                {
                    // We are expanding a pack, return the requested index in
                    // the sequence
                    return sequence_of_types_get_type_num(
                            new_sym->type_information,
                            pack_index);
                }
            }
            else
            {
                internal_error("Wrong pair template-parameter = %s vs template-new_sym = %s",
                        symbol_kind_name(orig_symbol),
                        symbol_kind_name(new_sym));
            }
        }
        else if (orig_symbol->kind == SK_TEMPLATE)
        {
            return get_user_defined_type(instantiation_symbol_try_to_map(instantiation_symbol_map, orig_symbol));
        }
        else if (is_template_specialized_type(orig_symbol->type_information))
        {
            // Update the arguments of this templated type
            //
            // template <typename _T, typename _Q = A<_T*> >
            // struct B
            // {
            // };

            type_t* template_type =
                template_specialized_type_get_related_template_type(orig_symbol->type_information);
            scope_entry_t* template_related_symbol =
                template_type_get_related_symbol(template_type);
            ERROR_CONDITION(template_related_symbol == NULL, "Invalid related template type", 0);

            if (template_related_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                    || template_related_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
            {
                // This specialized template type comes after a template template parameter,
                // so we have to update it using the template arguments
                // We need to update this template type too
                scope_entry_t* argument = lookup_of_template_parameter(
                        decl_context,
                        symbol_entity_specs_get_template_parameter_nesting(template_related_symbol),
                        symbol_entity_specs_get_template_parameter_position(template_related_symbol));

                ERROR_CONDITION(orig_symbol == NULL, "This should not be NULL", 0);

                // Now update the template_type with the new one
                template_type = argument->type_information;

                if (template_related_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK
                        && pack_index >= 0)
                {
                    template_type = sequence_of_types_get_type_num(argument->type_information, pack_index);
                    // FIXME - This is very nonregular respect the usual template case
                    if (is_named_type(template_type))
                    {
                        template_type = named_type_get_symbol(template_type)->type_information;
                    }
                }
            }
            else
            {
                template_related_symbol = instantiation_symbol_try_to_map(
                        instantiation_symbol_map,
                        template_related_symbol);

                template_type = template_related_symbol->type_information;
            }

            cv_qualifier_t cv_qualif = CV_NONE;
            advance_over_typedefs_with_cv_qualif(orig_type, &cv_qualif);

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Have to update template specialized type '%s'\n",
                        print_declarator(orig_type));
            }

            template_parameter_list_t* current_template_arguments =
                template_specialized_type_get_template_arguments(orig_symbol->type_information);

            // First make an update for all the current template arguments
            template_parameter_value_t **updated_parameter_values =
                NEW_VEC0(template_parameter_value_t*, current_template_arguments->num_parameters);

            int i;
            for (i = 0; i < current_template_arguments->num_parameters; i++)
            {
                updated_parameter_values[i] = update_template_parameter_value_of_template_class(
                        current_template_arguments->arguments[i],
                        decl_context,
                        instantiation_symbol_map,
                        locus, pack_index);

                if (updated_parameter_values[i] == NULL)
                {
                     DEBUG_CODE()
                     {
                         fprintf(stderr, "SCOPE: Update of template argument %d failed\n", i);
                     }
                     DELETE(updated_parameter_values);
                     return NULL;
                }
            }

            template_parameter_list_t* expanded_template_parameters = NEW0(template_parameter_list_t);
            int i_arg = 0;

            // Expand all the arguments
            while (i_arg < current_template_arguments->num_parameters)
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
                                    template_parameter_value_t *new_value = NEW0(template_parameter_value_t);
                                    new_value->kind = updated_parameter_values[i_arg]->kind;
                                    new_value->type =
                                        sequence_of_types_get_type_num(updated_parameter_values[i_arg]->type, k);

                                    P_LIST_ADD(expanded_template_parameters->arguments,
                                            expanded_template_parameters->num_parameters,
                                            new_value);
                                }
                            }
                            else
                            {
                                P_LIST_ADD(expanded_template_parameters->arguments,
                                        expanded_template_parameters->num_parameters,
                                        updated_parameter_values[i_arg]);
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
                                    template_parameter_value_t *new_value = NEW0(template_parameter_value_t);
                                    new_value->kind = updated_parameter_values[i_arg]->kind;
                                    // FIXME - What about this case? template <typename ...T, T...N>
                                    new_value->type = updated_parameter_values[i_arg]->type;
                                    new_value->value = nodecl_shallow_copy(list[k]);

                                    P_LIST_ADD(expanded_template_parameters->arguments,
                                            expanded_template_parameters->num_parameters,
                                            new_value);
                                }
                                DELETE(list);
                                nodecl_free(updated_parameter_values[i_arg]->value);
                            }
                            else
                            {
                                P_LIST_ADD(expanded_template_parameters->arguments,
                                        expanded_template_parameters->num_parameters,
                                        updated_parameter_values[i_arg]);
                            }
                            break;
                        }
                    default: internal_error("Code unreachable", 0);
                }
                i_arg++;
            }
            DELETE(updated_parameter_values);

            // Allocate room for the parameters, this is required by complete_template_parameters_of_template_class
            expanded_template_parameters->parameters = NEW_VEC0(
                    template_parameter_t*,
                    expanded_template_parameters->num_parameters);

            template_parameter_list_t* updated_template_arguments = complete_template_parameters_of_template_class(
                    decl_context,
                    template_type,
                    expanded_template_parameters,
                    locus);

            if (updated_template_arguments == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Completion of template parameters failed\n");
                }
                DELETE(expanded_template_parameters->parameters);
                free_template_parameter_list(expanded_template_parameters);
                return NULL;
            }

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
            if (updated_specialized == NULL
                    || (template_specialized_type_get_template_parameters(
                            named_type_get_symbol(updated_specialized)->type_information)
                        ->parameters !=
                        expanded_template_parameters->parameters))
            {
                DELETE(expanded_template_parameters->parameters);
            }
            free_template_parameter_list(expanded_template_parameters);
            free_template_parameter_list(updated_template_arguments);

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: END OF Reasking for specialization\n");
            }

            if (updated_specialized == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Specialization request failed\n");
                }
                return NULL;
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
        else if (orig_symbol->kind == SK_TYPEDEF)
        {
            cv_qualifier_t cv_qualif_orig = CV_NONE;
            advance_over_typedefs_with_cv_qualif(orig_type, &cv_qualif_orig);

            type_t* new_type = 
                    update_type_aux_(orig_symbol->type_information,
                        decl_context,
                        locus, instantiation_symbol_map, pack_index);

            cv_qualifier_t cv_qualif_new = CV_NONE;
            advance_over_typedefs_with_cv_qualif(new_type, &cv_qualif_new);

            return get_cv_qualified_type(new_type, cv_qualif_orig | cv_qualif_new);
        }
        else if (orig_symbol->kind == SK_DEPENDENT_ENTITY)
        {
            cv_qualifier_t cv_qualif_orig = CV_NONE;
            advance_over_typedefs_with_cv_qualif(orig_type, &cv_qualif_orig);

            type_t* new_type = update_type_aux_(orig_symbol->type_information,
                    decl_context,
                    locus, instantiation_symbol_map, pack_index);

            cv_qualifier_t cv_qualif_new = CV_NONE;
            advance_over_typedefs_with_cv_qualif(new_type, &cv_qualif_new);

            return get_cv_qualified_type(new_type, cv_qualif_orig | cv_qualif_new);
        }
        else if (orig_symbol->kind == SK_CLASS)
        {
            cv_qualifier_t cv_qualif_orig = CV_NONE;
            advance_over_typedefs_with_cv_qualif(orig_type, &cv_qualif_orig);

            return get_cv_qualified_type(
                    get_user_defined_type(
                        instantiation_symbol_try_to_map(instantiation_symbol_map, orig_symbol)),
                    cv_qualif_orig);
        }
        else if (orig_symbol->kind == SK_ENUM)
        {
            cv_qualifier_t cv_qualif_orig = CV_NONE;
            advance_over_typedefs_with_cv_qualif(orig_type, &cv_qualif_orig);

            return get_cv_qualified_type(
                    get_user_defined_type(
                        instantiation_symbol_try_to_map(instantiation_symbol_map, orig_symbol)),
                    cv_qualif_orig);
        }
        else if (orig_symbol->kind == SK_DECLTYPE)
        {
            return update_type_aux_(orig_symbol->type_information,
                                    decl_context,
                                    locus,
                                    instantiation_symbol_map,
                                    pack_index);
        }
        else
        {
            // Return it unmodified
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Not updating named type '%s'\n", print_declarator(orig_type));
            }
            return orig_type;
        }
    }
    else if (is_lvalue_reference_type(orig_type))
    {
        type_t* referenced = reference_type_get_referenced_type(orig_type);

        type_t* updated_referenced = update_type_aux_(referenced, decl_context,
                locus, instantiation_symbol_map, pack_index);

        if (updated_referenced == NULL)
            return NULL;

        // Any attempt to create a lvalue reference of any reference type is an
        // lvalue reference type
        type_t* result_type = get_lvalue_reference_type(no_ref(updated_referenced));

        return result_type;
    }
    else if (is_rvalue_reference_type(orig_type))
    {
        type_t* referenced = reference_type_get_referenced_type(orig_type);

        type_t* updated_referenced = update_type_aux_(referenced, decl_context,
                locus, instantiation_symbol_map, pack_index);

        if (updated_referenced == NULL)
            return NULL;

        type_t* result_type = NULL;

        // Any attempt to create a rvalue reference of any reference type is
        // that reference type
        if (is_any_reference_type(updated_referenced))
            result_type = updated_referenced;
        else
            result_type = get_rvalue_reference_type(updated_referenced);

        return result_type;
    }
    else if (is_pointer_type(orig_type))
    {
        cv_qualifier_t cv_qualifier = get_cv_qualifier(orig_type);

        type_t* pointee = pointer_type_get_pointee_type(orig_type);

        type_t* updated_pointee = update_type_aux_(pointee, decl_context,
                locus, instantiation_symbol_map, pack_index);

        if (updated_pointee == NULL)
            return NULL;

        if (is_any_reference_type(updated_pointee))
        {
            error_printf_at(locus, "attempt to create a pointer to reference\n");
            return NULL;
        }

        type_t* result_type = get_pointer_type(updated_pointee);

        result_type = get_cv_qualified_type(result_type, cv_qualifier);

        return result_type;
    }
    else if (is_pointer_to_member_type(orig_type))
    {
        cv_qualifier_t cv_qualifier = get_cv_qualifier(orig_type);

        type_t* pointee = pointer_type_get_pointee_type(orig_type);
        type_t* updated_pointee = update_type_aux_(pointee, decl_context, 
                locus, instantiation_symbol_map, pack_index);

        if (updated_pointee == NULL)
            return NULL;

        type_t* pointee_class = pointer_to_member_type_get_class_type(orig_type);
        pointee_class = update_type_aux_(pointee_class, decl_context, 
                locus, instantiation_symbol_map, pack_index);

        // If it is not a named class type _and_ it is not a template type
        // parameter, then this is not a valid pointer to member type
        if (!is_dependent_type(pointee_class)
                && !is_named_class_type(pointee_class))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: When updating pointer to member, the class type is wrong\n");
            }
            return NULL;
        }

        type_t* result_type = get_pointer_to_member_type(updated_pointee,
                pointee_class);

        result_type = get_cv_qualified_type(result_type, cv_qualifier);

        return result_type;
    }
    else if (is_function_type(orig_type))
    {
        cv_qualifier_t cv_qualifier = get_cv_qualifier(orig_type);

        type_t* return_type = NULL;

        // For functions with trailing return we will update their return type
        // after the parameters (because this matches the lexical order as
        // required by C++)
        char has_trailing_return = function_type_get_has_trailing_return(orig_type);
        if (!has_trailing_return)
        {
            return_type = function_type_get_return_type(orig_type);
            if (return_type != NULL)
            {
                return_type = update_type_aux_(return_type, decl_context, 
                        locus, instantiation_symbol_map, pack_index);
                // Something went wrong here for the return type
                if (return_type == NULL)
                    return NULL;
            }
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
                    locus, instantiation_symbol_map, pack_index);

            if (param_orig_type == NULL)
                return NULL;

            if (is_void_type(param_orig_type))
            {
                error_printf_at(locus, "attempt to create a function type with a void parameter type\n");
                return NULL;
            }

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

                        if (is_void_type(param_orig_type))
                        {
                            error_printf_at(locus, "attempt to create a function type with a void parameter type\n");
                            return NULL;
                        }

                        parameter_info_t parameter_info = get_parameter_info_for_type(param_orig_type);
                        P_LIST_ADD(unpacked_parameter_types, num_unpacked_parameter_types, parameter_info);
                    }
                }
                else
                {
                    P_LIST_ADD(unpacked_parameter_types, num_unpacked_parameter_types, packed_parameter_types[i]);
                }
            }

            DELETE(packed_parameter_types);
            packed_parameter_types = NULL;
        }

        if (has_ellipsis)
        {
            parameter_info_t parameter_info;
            memset(&parameter_info, 0, sizeof(parameter_info));
            parameter_info.is_ellipsis = 1;
            parameter_info.type_info = get_ellipsis_type();

            P_LIST_ADD(unpacked_parameter_types, num_unpacked_parameter_types, parameter_info);
        }

        if (has_trailing_return)
        {
            return_type = function_type_get_return_type(orig_type);
            if (return_type != NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: Setting environment for a trailing type return update\n");
                }
                instantiation_symbol_map_t* return_instantiation_symbol_map =
                    instantiation_symbol_map_push(instantiation_symbol_map);

                // Consider this case
                //
                // template <typename T>
                // T* g(T t);
                //
                // template <typename T>
                // auto foo(T t) -> decltype(g(t));
                //
                // When updating 'foo' we need to create an artificial symbol 't' with the proper
                // update type so we can fully instantiate the type of 'g(t)'.
                int num_new_symbols = 0;
                scope_entry_t** new_symbols = NULL;
                char ok = add_mappings_for_return_type(return_type,
                        decl_context,
                        locus,
                        pack_index,
                        return_instantiation_symbol_map,
                        &num_new_symbols,
                        &new_symbols);

                if (!ok)
                {
                    // Something went wrong when updating the symbols
                    for (i = 0; i < num_new_symbols; i++)
                    {
                        DELETE(new_symbols[i]);
                    }
                    DELETE(new_symbols);

                    return NULL;
                }

                return_type = update_type_aux_(return_type, decl_context,
                        locus, return_instantiation_symbol_map, pack_index);

                instantiation_symbol_map_pop(return_instantiation_symbol_map);

                // Something went wrong here for the return type
                if (return_type == NULL)
                {
                    // Free the artificial mappings since they are not going to
                    // be used at all
                    for (i = 0; i < num_new_symbols; i++)
                    {
                        DELETE(new_symbols[i]);
                    }
                    DELETE(new_symbols);

                    return NULL;
                }

                // Note that we do not free each new_symbol in case of success
                // as they may be actually used in the expression
                DELETE(new_symbols);
            }
        }


        type_t* updated_function_type = NULL;
        if (!has_trailing_return)
            updated_function_type = get_new_function_type(return_type,
                    unpacked_parameter_types, num_unpacked_parameter_types, REF_QUALIFIER_NONE);
        else
            updated_function_type = get_new_function_type_trailing_type(return_type,
                    unpacked_parameter_types, num_unpacked_parameter_types, REF_QUALIFIER_NONE);

        DELETE(unpacked_parameter_types);

        updated_function_type = get_cv_qualified_type(updated_function_type, cv_qualifier);

        return updated_function_type;
    }
    else if (is_array_type(orig_type))
    {
        cv_qualifier_t cv_qualifier = get_cv_qualifier(orig_type);

        type_t* element_type = array_type_get_element_type(orig_type);
        element_type = update_type_aux_(element_type, decl_context, 
                locus, instantiation_symbol_map, pack_index);

        if (element_type == NULL)
            return NULL;

        if (is_void_type(element_type))
        {
            error_printf_at(locus, "attempt to create an array of void\n");
            return NULL;
        }
        else if (is_any_reference_type(element_type))
        {
            error_printf_at(locus, "attempt to create an array of reference type\n");
            return NULL;
        }
        else if (is_function_type(element_type))
        {
            error_printf_at(locus, "attempt to create an array of function type\n");
            return NULL;
        }

        nodecl_t array_size = array_type_get_array_size_expr(orig_type);

        // Context of the array
        const decl_context_t* array_size_context = array_type_get_array_size_expr_context(orig_type);

        if (!nodecl_is_null(array_size))
        {
            array_size = update_nodecl_constant_expression(array_size, decl_context,
                    instantiation_symbol_map, pack_index);

            if (nodecl_get_kind(array_size) == NODECL_ERR_EXPR)
                return NULL;

            if (nodecl_is_constant(array_size)
                    && const_value_is_zero(
                        const_value_gte(
                            nodecl_get_constant(array_size),
                            const_value_get_zero(/*bytes*/ 4, /* sign*/ 1))))
            {
                error_printf_at(locus, "attempt to create an array of negative size\n");
                return NULL;
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Expression '%s' successfully updated\n",
                        codegen_to_str(array_size, decl_context));
            }
        }

        type_t* updated_array_type = get_array_type(element_type, 
                array_size, 
                array_size_context);

        updated_array_type = get_cv_qualified_type(updated_array_type, 
                // combine qualifier
                cv_qualifier | get_cv_qualifier(updated_array_type));

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
        fixed_type = update_type_aux_(
                get_user_defined_type(dependent_entry),
                decl_context,
                locus,
                instantiation_symbol_map,
                pack_index);

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
            DELETE(list);

            ERROR_CONDITION(nodecl_get_kind(dependent_parts) != NODECL_CXX_DEP_NAME_NESTED, "Invalid tree kind", 0);

            list = nodecl_unpack_list(nodecl_get_child(dependent_parts, 0), &num_items);
            for (i = 0; i < num_items; i++)
            {
                appended_dependent_parts = nodecl_append_to_list(appended_dependent_parts, list[i]);
            }
            DELETE(list);

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
            error_printf_at(locus, "'%s' in '%s' is not a valid typename\n",
                    dependent_entry->symbol_name,
                    print_type_str(orig_type, decl_context));

            return NULL;
        }

        if (is_named_enumerated_type(fixed_type))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Dependent type '%s' is an enumerator\n", 
                        print_declarator(fixed_type));
            }
            error_printf_at(locus, "'%s' in '%s' has become an enumerator name\n",
                    dependent_entry->symbol_name,
                    print_type_str(orig_type, decl_context));
            return NULL;
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Dependent entity updated to '%s'\n",
                    print_declarator(fixed_type));
        }

        type_t* updated_type =
            update_dependent_typename(
                    orig_type,
                    fixed_type, dependent_parts, decl_context,
                    instantiation_symbol_map, locus, pack_index);

        if (updated_type != NULL)
        {
            // We are not reconstructing a type but making a replacement, so we
            // have to add qualifiers rather than simply restoring the original
            // one
            cv_qualifier_t cv_qualifiers_new = CV_NONE;
            advance_over_typedefs_with_cv_qualif(updated_type, &cv_qualifiers_new);

            updated_type = get_cv_qualified_type(updated_type, 
                    cv_qualifiers_new | cv_qualif);
        }

        return updated_type;
    }
    else if (is_typeof_expr(orig_type))
    {
        nodecl_t nodecl_expr = typeof_expr_type_get_expression(orig_type);

        nodecl_t nodecl_new_expr = instantiate_expression_non_executable(nodecl_expr, decl_context,
                instantiation_symbol_map, /* pack_index */ -1);

        if (nodecl_is_err_expr(nodecl_new_expr))
        {
            return NULL;
        }
        else if (nodecl_expr_is_type_dependent(nodecl_new_expr))
        {
            return get_typeof_expr_dependent_type(nodecl_new_expr,
                    decl_context,
                    typeof_expr_type_is_decltype(orig_type));
        }
        else if (typeof_expr_type_is_decltype(orig_type))
        {
            type_t* result = compute_type_of_decltype_nodecl(nodecl_new_expr, decl_context);
            return result;
        }
        else
        {
            type_t* result = nodecl_get_type(nodecl_new_expr);
            return result;
        }
    }
    else if (is_pack_type(orig_type))
    {
        return update_pack_type(orig_type, decl_context, instantiation_symbol_map, locus);
    }
    else if (is_sequence_of_types(orig_type))
    {
        int num_types = sequence_of_types_get_num_types(orig_type);
        if (pack_index == -1)
        {
            type_t* types[num_types + 1];
            int i;
            for (i = 0; i < num_types; i++)
            {
                types[i] = update_type_aux_(sequence_of_types_get_type_num(orig_type, i), decl_context, locus,
                        instantiation_symbol_map, pack_index);
                if (types[i] == NULL)
                    return NULL;
            }

            return get_sequence_of_types_flattened(num_types, types);
        }
        else
        {
            if (pack_index >= num_types)
                return NULL;

            return update_type_aux_(sequence_of_types_get_type_num(orig_type, pack_index),
                    decl_context, locus, instantiation_symbol_map, /* pack_index */ -1);
        }
    }
    else if (is_gxx_underlying_type(orig_type))
    {
        type_t* orig_underlying_type = gxx_underlying_type_get_underlying_type(orig_type);

        type_t* updated_underlying_type = update_type_aux_(orig_underlying_type,
                decl_context, locus, instantiation_symbol_map, pack_index);
        if (updated_underlying_type == NULL)
            return NULL;

        if (is_dependent_type(updated_underlying_type)
                || (is_enum_type(updated_underlying_type)
                    && is_dependent_type(enum_type_get_underlying_type(updated_underlying_type))))
        {
            return get_gxx_underlying_type(updated_underlying_type);
        }
        else if (is_enum_type(updated_underlying_type))
        {
            return enum_type_get_underlying_type(updated_underlying_type);
        }
        else
        {
            error_printf_at(locus, "__underlying_type(%s) is not a class or enum\n",
                    print_type_str(updated_underlying_type, decl_context));
            return NULL;
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
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    DEBUG_CODE()
    {
            fprintf(stderr, "SCOPE: Updating type '%s'\n", print_declarator(orig_type));
    }

    type_t* result = update_type_aux_(orig_type, decl_context, locus,
            /* instantiation_symbol_map */ NULL,
            /* pack_index */ -1);

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

type_t* update_type_with_pack_index(type_t* orig_type,
        const decl_context_t* decl_context,
        const locus_t* locus,
        int pack_index)
{
    DEBUG_CODE()
    {
            fprintf(stderr, "SCOPE: Updating type '%s'\n", print_declarator(orig_type));
    }

    type_t* result = update_type_aux_(orig_type, decl_context, locus,
            /* instantiation_symbol_map */ NULL,
            pack_index);

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
        const decl_context_t* context_of_being_instantiated,
        const locus_t* locus,
        instantiation_symbol_map_t* instantiation_symbol_map,
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
            nodecl_t aligned_attribute = instantiate_expression_non_executable(
                    nodecl_list_head(new_gcc_attr.expression_list),
                    context_of_being_instantiated,
                    instantiation_symbol_map, /* pack_index */ -1);
            if (nodecl_is_err_expr(aligned_attribute))
            {
                result = NULL;
            }
            else if (!nodecl_expr_is_value_dependent(aligned_attribute)
                    && !nodecl_is_constant(aligned_attribute))
            {
                error_printf_at(locus, "'aligned' attribute of type '%s' after instantiation is not constant\n",
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
        const decl_context_t* context_of_being_instantiated,
        const locus_t* locus,
        instantiation_symbol_map_t* instantiation_symbol_map,
        int pack_index)
{
    DEBUG_CODE()
    {
            fprintf(stderr, "SCOPE: While instantiating, updating type '%s'\n", print_declarator(orig_type));
    }

    type_t* result = update_type_aux_(orig_type,
            context_of_being_instantiated,
            locus,
            instantiation_symbol_map,
            pack_index);

    result = update_gcc_type_attributes(orig_type, result, context_of_being_instantiated, locus,
            instantiation_symbol_map, pack_index);

    if (result == NULL)
    {
        // error_printf("%s: error: type '%s' rendered invalid during instantiation\n",
        //         locus_to_str(locus), print_type_str(orig_type, context_of_being_instantiated));
        result = get_error_type();
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Type '%s' has been updated to '%s'\n", print_declarator(orig_type), print_declarator(result));
    }

    return result;
}

typedef
struct template_argument_info_tag
{
    int position;
    dhash_ptr_t* disambig_hash;
} template_argument_info_t;

static template_parameter_value_t* get_single_template_argument_from_syntax(AST template_parameter, 
        const decl_context_t* template_parameters_context,
        dhash_ptr_t* disambig_hash,
        int position);

static char check_single_template_argument_from_syntax(AST template_parameter, 
        const decl_context_t* template_parameters_context,
        int position UNUSED_PARAMETER,
        void* info)
{
    template_argument_info_t *targ_info = (template_argument_info_t*)info;

    template_parameter_value_t* res = get_single_template_argument_from_syntax(template_parameter,
            template_parameters_context,
            targ_info->disambig_hash,
            targ_info->position);

    if (res != NULL)
    {
        DELETE(res);
        return 1;
    }
    else
    {
        return 0;
    }
}

static int choose_type_template_argument(
        AST current,
        AST previous,
        int current_idx UNUSED_PARAMETER,
        int previous_idx UNUSED_PARAMETER,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        void* info UNUSED_PARAMETER)
{
    // Prioritize template-type arguments
    int i = either_type(
            current,
            previous,
            AST_TEMPLATE_EXPRESSION_ARGUMENT,
            AST_TEMPLATE_TYPE_ARGUMENT);
    if (i != 0)
        return i;

    i = either_type(
            current,
            previous,
            AST_TEMPLATE_EXPRESSION_ARGUMENT_PACK,
            AST_TEMPLATE_TYPE_ARGUMENT_PACK);
    return i;
}

static template_parameter_value_t* get_single_template_argument_from_syntax(AST template_parameter, 
        const decl_context_t* template_parameters_context,
        dhash_ptr_t* disambig_hash,
        int position)
{
    if (ASTKind(template_parameter) == AST_AMBIGUITY)
    {
        template_argument_info_t targ_info = {
            .position = position,
            .disambig_hash = disambig_hash,
        };
        solve_ambiguity_generic(
                template_parameter,
                template_parameters_context,
                &targ_info,
                check_single_template_argument_from_syntax,
                choose_type_template_argument,
                NULL);
    }

    switch (ASTKind(template_parameter))
    {
        case AST_TEMPLATE_EXPRESSION_ARGUMENT :
        case AST_TEMPLATE_EXPRESSION_ARGUMENT_PACK :
            {
                char is_expansion = 0;
                if (ASTKind(template_parameter) == AST_TEMPLATE_EXPRESSION_ARGUMENT_PACK)
                {
                    is_expansion = 1;
                }

                template_parameter_value_t* t_argument = NEW0(template_parameter_value_t);

                AST expr = ASTSon0(template_parameter);

                nodecl_t nodecl_expr = nodecl_null();
                check_nontype_template_argument_expression(expr, template_parameters_context, &nodecl_expr);

                if (nodecl_is_err_expr(nodecl_expr))
                {
                    DELETE(t_argument);
                    return NULL;
                }

                if (is_expansion)
                {
                    nodecl_expr = nodecl_make_cxx_value_pack(
                            nodecl_expr,
                            nodecl_get_type(nodecl_expr),
                            nodecl_get_locus(nodecl_expr));
                    nodecl_expr_set_is_type_dependent(nodecl_expr,
                            is_dependent_type(nodecl_get_type(nodecl_expr)));
                    nodecl_expr_set_is_value_dependent(nodecl_expr, 1);
                }

                t_argument->kind = TPK_NONTYPE;
                t_argument->value = nodecl_expr;
                t_argument->type = nodecl_get_type(nodecl_expr);

                return t_argument;
                break;
            }
        case AST_TEMPLATE_TYPE_ARGUMENT :
        case AST_TEMPLATE_TYPE_ARGUMENT_PACK :
            {
                char is_expansion = 0;
                if (ASTKind(template_parameter) == AST_TEMPLATE_TYPE_ARGUMENT_PACK)
                {
                    is_expansion = 1;
                }

                template_parameter_value_t* t_argument = NEW0(template_parameter_value_t);

                AST type_template_parameter = ASTSon0(template_parameter);
                AST type_specifier_seq = ASTSon0(type_template_parameter);
                AST abstract_decl = ASTSon1(type_template_parameter);

                char keep_is_inside_pack_expansion = get_is_inside_pack_expansion();
                if (is_expansion)
                {
                    set_is_inside_pack_expansion(1);
                }

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
                    set_is_inside_pack_expansion(keep_is_inside_pack_expansion);
                    DELETE(t_argument);
                    error_printf_at(ast_get_locus(template_parameter), "invalid template-argument number %d\n",
                            position);
                    return NULL;
                }

                type_t* declarator_type;
                compute_declarator_type(abstract_decl, &gather_info, type_info, &declarator_type,
                        template_parameters_context, &dummy_nodecl_output);

                if (is_error_type(declarator_type))
                {
                    set_is_inside_pack_expansion(keep_is_inside_pack_expansion);
                    DELETE(t_argument);
                    error_printf_at(ast_get_locus(template_parameter), "invalid template-argument number %d\n",
                            position);
                    return NULL;
                }

                if (get_is_inside_pack_expansion()
                        && !keep_is_inside_pack_expansion
                        && type_does_not_contain_any_template_parameter_pack(declarator_type,
                            ast_get_locus(template_parameter)))
                {
                    set_is_inside_pack_expansion(keep_is_inside_pack_expansion);
                    DELETE(t_argument);
                    return NULL;
                }

                if (is_named_type(declarator_type)
                        && (named_type_get_symbol(declarator_type)->kind == SK_TEMPLATE
                            || named_type_get_symbol(declarator_type)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                            || named_type_get_symbol(declarator_type)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK))
                {
                    if (abstract_decl != NULL)
                    {
                        set_is_inside_pack_expansion(keep_is_inside_pack_expansion);
                        DELETE(t_argument);
                        error_printf_at(ast_get_locus(template_parameter), "invalid template-argument number %d\n",
                                position);
                        return NULL;
                    }
                    t_argument->kind = TPK_TEMPLATE;
                }
                else
                {
                    t_argument->kind = TPK_TYPE;
                }

                set_is_inside_pack_expansion(keep_is_inside_pack_expansion);
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
                internal_error("Invalid node %s", ast_print_node_type(ASTKind(template_parameter)));
            }
    }
    return NULL;
}



static char template_parameter_list_invalid_marker = 0;

static void get_template_arguments_from_syntax_rec(
        AST template_parameters_list_tree,
        const decl_context_t* template_parameters_context,
        dhash_ptr_t* disambig_hash,

        // Out
        template_parameter_list_t** result,
        int *position)
{
    ERROR_CONDITION(template_parameters_list_tree == NULL, "Invalid tree", 0);

    void * hashed_data = dhash_ptr_query(disambig_hash, (char*)template_parameters_list_tree);

    if (hashed_data != NULL)
    {
        if ((char*)hashed_data == &template_parameter_list_invalid_marker)
        {
            // fprintf(stderr, "%p -> hit failure\n", template_parameters_list_tree);
            free_template_parameter_list(*result);
            *result = NULL;
            return;
        }
        else
        {
            // fprintf(stderr, "%p -> hit valid\n", template_parameters_list_tree);
            template_parameter_list_t* cached_template_parameter_list = (template_parameter_list_t*)hashed_data;
            copy_template_parameter_list(*result, cached_template_parameter_list);
            (*position) += (*result)->num_parameters;
            return;
        }
    }
    else
    {
        // fprintf(stderr, "%p -> miss\n", template_parameters_list_tree);
    }

    if (ASTKind(template_parameters_list_tree) == AST_AMBIGUITY)
    {
        // We have to try every interpretation and keep the good one. Only one should end being valid
        int num_ambiguities = ast_get_num_ambiguities(template_parameters_list_tree);

        ERROR_CONDITION(num_ambiguities <= 0, "Should not happen", 0);
        template_parameter_list_t* potential_results[num_ambiguities];
        int potential_positions[num_ambiguities];

        int valid = -1;

        diagnostic_context_t* ambig_diag[num_ambiguities + 1];

        int i;
        for (i = 0; i < num_ambiguities; i++)
        {
            AST current_interpretation = ast_get_ambiguity(template_parameters_list_tree, i);

            ast_fix_parents_inside_intepretation(current_interpretation);

            potential_results[i] = NEW0(template_parameter_list_t);
            copy_template_parameter_list(potential_results[i], *result);
            potential_positions[i] = *position;

            ambig_diag[i] = diagnostic_context_push_buffered();
            get_template_arguments_from_syntax_rec(
                    current_interpretation,
                    template_parameters_context,
                    disambig_hash,

                    &potential_results[i],
                    &potential_positions[i]);
            diagnostic_context_pop();

            if (potential_results[i] != NULL)
            {
                if (valid < 0)
                {
                    valid = i;
                }
                else
                {
                    internal_error("Two possible interpretations for a template-parameter-list", 0);
                }
            }
            else
            {
                free_template_parameter_list(potential_results[i]);
            }
        }

        if (valid < 0)
        {
            // Commit everything
            diagnostic_context_t* combine_diagnostics = diagnostic_context_push_buffered();
            for (i = 0; i < num_ambiguities; i++)
            {
                diagnostic_context_commit(ambig_diag[i]);
            }
            diagnostic_context_pop();
            diagnostic_context_commit(combine_diagnostics);

            free_template_parameter_list(*result);
            *result = NULL;

            dhash_ptr_insert(disambig_hash, (char*)template_parameters_list_tree, &template_parameter_list_invalid_marker);

            return;
        }
        else
        {
            for (i = 0; i < num_ambiguities; i++)
            {
                if (i == valid)
                {
                    diagnostic_context_commit(ambig_diag[i]);
                }
                else
                {
                    diagnostic_context_discard(ambig_diag[i]);
                }
            }
            ast_replace_with_ambiguity(template_parameters_list_tree, valid);

            // Update the result with the new ones
            free_template_parameter_list(*result);
            *result = NEW0(template_parameter_list_t);
            copy_template_parameter_list(*result, potential_results[valid]);
            *position = potential_positions[valid];

            template_parameter_list_t* cached = NEW0(template_parameter_list_t);
            copy_template_parameter_list(cached, potential_results[valid]);
            dhash_ptr_insert(disambig_hash, (char*)template_parameters_list_tree, cached);
        }
    }
    else if (ASTKind(template_parameters_list_tree) == AST_NODE_LIST)
    {
        // If we are not the first, invoke recursively
        if (ASTSon0(template_parameters_list_tree) != NULL)
        {
            get_template_arguments_from_syntax_rec(
                    ASTSon0(template_parameters_list_tree),
                    template_parameters_context,
                    disambig_hash,

                    result,
                    position);
            if (*result == NULL)
            {
                dhash_ptr_insert(disambig_hash, (char*)template_parameters_list_tree, &template_parameter_list_invalid_marker);
                return;
            }
        }

        AST template_parameter = ASTSon1(template_parameters_list_tree);

        template_parameter_value_t* t_argument = get_single_template_argument_from_syntax(
                template_parameter,
                template_parameters_context,
                disambig_hash,
                *position);

        if (t_argument == NULL)
        {
            free_template_parameter_list(*result);
            *result = NULL;

            dhash_ptr_insert(disambig_hash, (char*)template_parameters_list_tree, &template_parameter_list_invalid_marker);
            return;
        }

        P_LIST_ADD((*result)->arguments,
                (*result)->num_parameters,
                t_argument);

        (*position)++;

        template_parameter_list_t* cached = NEW0(template_parameter_list_t);
        copy_template_parameter_list(cached, *result);
        dhash_ptr_insert(disambig_hash, (char*)template_parameters_list_tree, cached);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

static void destroy_cached_template_parameter_lists(const char* key UNUSED_PARAMETER,
        void *info,
        void *walk_info UNUSED_PARAMETER)
{
    if (info == NULL
            || (char*)info == &template_parameter_list_invalid_marker)
        return;

    template_parameter_list_t* tpl = (template_parameter_list_t*)info;
    DELETE(tpl->parameters);
    free_template_parameter_list(tpl);
}

template_parameter_list_t* get_template_arguments_from_syntax(
        AST template_parameters_list_tree,
        const decl_context_t* template_parameters_context)
{
    template_parameter_list_t* result = NEW0(template_parameter_list_t);
    if (template_parameters_list_tree == NULL)
    {
        return result;
    }

    dhash_ptr_t* disambig_hash = dhash_ptr_new(5);

    int position = 1;
    get_template_arguments_from_syntax_rec(
            template_parameters_list_tree,
            template_parameters_context,
            disambig_hash,

            &result,
            &position);

    if (result != NULL)
    {
        // Empty parameters, they will be filled elsewhere
        result->parameters = NEW_VEC0(template_parameter_t*, result->num_parameters);
    }

    dhash_ptr_walk(disambig_hash, destroy_cached_template_parameter_lists, NULL);
    dhash_ptr_destroy(disambig_hash);

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
        const decl_context_t* template_name_context,
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

    if ((primary_template_parameters->num_parameters < result->num_parameters)
            && !last_is_variadic)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: Too many template arguments %d > %d\n",
                    result->num_parameters,
                    primary_template_parameters->num_parameters);
        }

        error_printf_at(locus, "too many template-arguments for template class\n");

        free_template_parameter_list(result);
        return NULL;
    }

    // Make room for all the arguments
    if (result->num_parameters < primary_template_parameters->num_parameters)
    {
        result->arguments = NEW_REALLOC(
                template_parameter_value_t*,
                result->arguments,
                primary_template_parameters->num_parameters);
        int k;
        for (k = result->num_parameters; k < primary_template_parameters->num_parameters; k++)
        {
            result->arguments[k] = NULL;
        }
    }

    int original_num_arguments = result->num_parameters;
    result->parameters = primary_template_parameters->parameters;
    result->num_parameters = primary_template_parameters->num_parameters;

    decl_context_t* new_template_context = decl_context_clone(template_name_context);
    new_template_context->template_parameters = result;

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
        if (i >= original_num_arguments)
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

                    error_printf_at(locus, "template argument number %d is missing and there is no default template argument for it\n", i);

                    free_template_parameter_list(result);
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
                    internal_error("Code unreachable", 0);
                    break;
                }
            }
            else
            {
                // Note that this adds the parameter and the argument, they will be updated later (if needed)
                template_parameter_value_t* v = update_template_parameter_value_of_template_class(
                        primary_template_parameters->arguments[i],
                        new_template_context,
                        /* instantiation_symbol_map */ NULL,
                        locus, /* pack_index */ -1);

                if (v == NULL)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "SCOPE: Update of template argument %d failed\n", i);
                    }

                    free_template_parameter_list(result);
                    return NULL;
                }

                // Recall that this was implicit added in this list
                v->is_implicit = 1;

                result->arguments[i] = v;
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

                error_printf_at(
                        locus,
                        "kind of template argument number %d does not match "
                        "that of the corresponding template parameter\n",
                        i + 1);

                free_template_parameter_list(result);
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

            if (!nodecl_expr_is_value_dependent(result->arguments[i]->value)
                    // Sometimes it may happen that the argument itself is not dependent
                    // but the updated type is.
                    //
                    // template <typename T, T N>
                    // struct A { };
                    // template <typename T>
                    // void f()
                    // {
                    //   A<T, 10> a; // 10 is not value dependent but the type
                    //               // of its parameter is dependent (T)
                    // }
                    && !is_dependent_type(result->arguments[i]->type))
            {
                nodecl_t nodecl_arg = nodecl_null();
                char ok = check_nodecl_template_argument_can_be_converted_to_parameter_type(
                        result->arguments[i]->value,
                        dest_type,
                        template_name_context,
                        &nodecl_arg);

                if (!ok)
                {
                    type_t* arg_type = nodecl_get_type(result->arguments[i]->value);
                    error_printf_at(
                            locus,
                            "type '%s' of template argument %d cannot be converted to "
                            "type '%s' of the corresponding template parameter\n",
                            print_type_str(arg_type, template_name_context),
                            i + 1,
                            print_type_str(dest_type, template_name_context));
                    free_template_parameter_list(result);
                    return NULL;
                }

                nodecl_free(result->arguments[i]->value);
                result->arguments[i]->value = nodecl_arg;
            }
        }
    }

    if (last_is_variadic)
    {
        // Now review template arguments of the final pack parameter
        if (original_num_arguments > non_variadic_parameters)
        {
            enum template_parameter_kind pack_base_kind = template_parameter_kind_get_base_kind(last->kind);

            // We will fold this argument
            int last_argument_index = i;
            template_parameter_value_t* folded_value = NEW0(template_parameter_value_t);
            folded_value->kind = pack_base_kind;

            type_t* parameter_type = NULL;

            if (last->kind == TPK_NONTYPE_PACK)
            {
                // Update the parameter type of this nontype template parameter pack
                parameter_type = update_type(
                        last->entry->type_information,
                        new_template_context,
                        locus);
            }

            for (; i < original_num_arguments; i++)
            {
                if (pack_base_kind != result->arguments[i]->kind)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "SCOPE: Template parameter pack kind and template argument kind do not match\n");
                    }

                    error_printf_at(locus,
                            "kind of template argument number %d does not match "
                            "that of the corresponding template parameter pack\n",
                            i + 1);

                    free_template_parameter_list(result);
                    return NULL;
                }

                if (last->kind != TPK_NONTYPE_PACK)
                {
                    // For type or template template parameter pack, use the type of the
                    // argument
                    parameter_type = result->arguments[i]->type;
                }

                // Nontype template arguments must be adjusted first
                if (result->arguments[i]->kind == TPK_NONTYPE)
                {
                    // We need to do this because of cases like this
                    //
                    // N in    template <typename T>
                    //         template <T ...N>
                    //
                    // or
                    //
                    // N in    template <typename ...T>
                    //         template <T... N>
                    //
                    if (is_sequence_of_types(parameter_type))
                    {
                        // This case of the two above
                        //    N in    template <typename ...T>
                        //            template <T... N>
                        int index_of_type = last_argument_index - i;
                        if (index_of_type >= sequence_of_types_get_num_types(parameter_type))
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "SCOPE: Template argument pack is expanding '%s' "
                                        "but there are too many elements (this is element %d)\n",
                                        print_declarator(parameter_type), index_of_type);
                            }
                            error_printf_at(locus,
                                    "too many template arguments for"
                                    " the template parameter pack\n");

                            free_template_parameter_list(result);
                            return NULL;
                        }

                        parameter_type = sequence_of_types_get_type_num(parameter_type, index_of_type);
                    }

                    if (nodecl_is_null(result->arguments[i]->value))
                    {
                        // May happen if we deduce an empty sequence of nontype arguments
                    }
                    else if (!nodecl_expr_is_value_dependent(result->arguments[i]->value))
                    {
                        type_t* arg_type = nodecl_get_type(result->arguments[i]->value);
                        if (!is_dependent_type(arg_type))
                        {
                            nodecl_t nodecl_arg = nodecl_null();
                            char ok = check_nodecl_template_argument_can_be_converted_to_parameter_type(
                                    result->arguments[i]->value,
                                    get_unqualified_type(parameter_type),
                                    template_name_context,
                                    &nodecl_arg);

                            if (!ok)
                            {
                                error_printf_at(locus, "type '%s' of template argument %d cannot be converted to "
                                        "type '%s' of the corresponding template parameter\n",
                                        print_type_str(arg_type, template_name_context),
                                        i + 1,
                                        print_type_str(parameter_type, template_name_context));
                                free_template_parameter_list(result);
                                return NULL;
                            }

                            nodecl_free(result->arguments[i]->value);
                            result->arguments[i]->value = nodecl_arg;
                        }
                    }
                }

                folded_value->type = get_sequence_of_types_append_type(folded_value->type, parameter_type);
                if (result->arguments[i]->kind == TPK_NONTYPE)
                {
                    // Note that result->arguments[i]->value may be NULL but
                    // this is fine as it won't enlarge the list
                    folded_value->value = nodecl_append_to_list(folded_value->value, result->arguments[i]->value);
                }
            }

            result->parameters[last_argument_index]
                = primary_template_parameters->parameters[last_argument_index];
            result->arguments[last_argument_index] = folded_value;
        }
        else if (original_num_arguments == non_variadic_parameters)
        {
            // Empty argument for this variadic parameter
            // Create an empty one
            template_parameter_value_t* new_value = NEW0(template_parameter_value_t);
            new_value->kind = template_parameter_kind_get_base_kind(last->kind);
            new_value->type = get_sequence_of_types(0, NULL);
            new_value->value = nodecl_null(); // Empty list

            result->arguments[i] = new_value;
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

    if (original_num_arguments > primary_template_parameters->num_parameters)
    {
        // Adjust the size of this buffer
        result->arguments = NEW_REALLOC(
                template_parameter_value_t*,
                result->arguments,
                result->num_parameters);
    }

    return result;
}

// Only for "simple" symbols, this is, that are not members and they are simply contained
// in a nest of namespaces
static const char* get_fully_qualified_symbol_name_simple(
        scope_entry_t* entry,
        const decl_context_t* decl_context,
        const char* current_qualif_name)
{
    const decl_context_t* symbol_decl_context = entry->decl_context;
    const char* result = current_qualif_name;

    scope_t* current_scope = symbol_decl_context->current_scope;

    if (current_scope->kind == NAMESPACE_SCOPE)
    {
        char extra_qualification_required = 1;
        char last_was_anonymous = 0;
        while (current_scope != NULL)
        {
            char current_is_anonymous = 0;
            if (current_scope->related_entry != NULL
                    && current_scope->related_entry->symbol_name != NULL
                    && strcmp(current_scope->related_entry->symbol_name, "(unnamed)") == 0)
                current_is_anonymous = 1;

            if (decl_context->namespace_scope == current_scope
                    && (current_is_anonymous
                        || last_was_anonymous))
            {
                extra_qualification_required = 0;
                break;
            }

            if (current_scope->related_entry != NULL
                    && current_scope->related_entry->symbol_name != NULL
                    && strcmp(current_scope->related_entry->symbol_name, "(unnamed)") != 0)
            {
                const char* nested_name = strappend(current_scope->related_entry->symbol_name, "::");
                result = strappend(nested_name, result);
            }

            current_scope = current_scope->contained_in;
            last_was_anonymous = current_is_anonymous;
        }

        CXX_LANGUAGE()
        {
            if (extra_qualification_required)
                result = strappend("::", result);
        }
    }

    return result;
}

static const char* template_arguments_to_str_ex(
        template_parameter_list_t* template_parameters,
        int first_argument_to_be_printed,
        char print_first_level_bracket,
        const decl_context_t* decl_context,
        print_type_callback_t print_type_fun,
        void *print_type_data
        )

{
    if (template_parameters == NULL
            || template_parameters->num_parameters <= first_argument_to_be_printed)
        return "";

    strbuilder_t *result = strbuilder_new();
    if (print_first_level_bracket)
    {
        // It is not enough with the name, we have to print the arguments
        strbuilder_append(result, "<");
    }

    int i;
    char print_comma = 0;
    for (i = first_argument_to_be_printed; i < template_parameters->num_parameters; i++, print_comma = 1)
    {
        template_parameter_value_t* current_argument = template_parameters->arguments[i];

        if (current_argument != NULL
                && current_argument->is_implicit)
            continue;

        if (print_comma)
        {
            if (current_argument == NULL
                    || !is_sequence_of_types(current_argument->type)
                    || sequence_of_types_get_num_types(current_argument->type) > 0)
            {
                strbuilder_append(result, ", ");
            }
        }

        if (current_argument == NULL)
        {
            strbuilder_append(result, template_parameters->parameters[i]->entry->symbol_name);
            continue;
        }

        strbuilder_t *argument = strbuilder_new();
        switch (current_argument->kind)
        {
            case TPK_TYPE:
                {
                    strbuilder_append(argument,
                            print_type_fun(current_argument->type, decl_context, print_type_data));
                    break;
                }
            case TPK_NONTYPE:
                {
                    // The first unparenthesized '>' indicates the end of
                    // template arguments. For this reason, in some cases we
                    // need to parenthesize this template current_argument.
                    char codegen_of_nontype_template_argument;
                    codegen_of_nontype_template_argument = 1;

                    codegen_set_parameter(CODEGEN_PARAM_NONTYPE_TEMPLATE_ARGUMENT,
                            (void*)&codegen_of_nontype_template_argument);

                    if (nodecl_is_list(current_argument->value))
                    {
                        if (debug_options.show_template_packs)
                        {
                            strbuilder_append(argument, " /* { */ ");
                        }

                        int num_items;
                        int j;
                        nodecl_t* list = nodecl_unpack_list(current_argument->value, &num_items);
                        for (j = 0; j < num_items; j++)
                        {
                            if (j > 0)
                                strbuilder_append(argument, ", ");

                            strbuilder_append(argument, codegen_to_str(list[j], decl_context));
                        }

                        if (debug_options.show_template_packs)
                        {
                            strbuilder_append(argument, " /* } */ ");
                        }

                        DELETE(list);
                    }
                    else
                    {
                        strbuilder_append(argument,
                                codegen_to_str(current_argument->value, decl_context));
                    }

                    codegen_of_nontype_template_argument = 0;
                    codegen_set_parameter(CODEGEN_PARAM_NONTYPE_TEMPLATE_ARGUMENT,
                            (void*)&codegen_of_nontype_template_argument);

                    break;
                }
            case TPK_TEMPLATE:
                {
                    type_t* template_type = current_argument->type;
                    if (is_pack_type(current_argument->type))
                    {
                        template_type = pack_type_get_packed_type(current_argument->type);
                    }
                    if (is_sequence_of_types(template_type))
                    {
                        if (debug_options.show_template_packs)
                        {
                            strbuilder_append(argument, " /* { */ ");
                        }

                        int num_types = sequence_of_types_get_num_types(template_type);
                        int k;
                        for (k = 0; k < num_types; k++)
                        {
                            if (k > 0)
                                strbuilder_append(argument, ", ");

                            strbuilder_append(argument,
                                    get_qualified_symbol_name(
                                        named_type_get_symbol(sequence_of_types_get_type_num(template_type, k)),
                                        decl_context)
                                    );
                        }

                        if (debug_options.show_template_packs)
                        {
                            strbuilder_append(argument, " /* } */ ");
                        }
                    }
                    else
                    {
                        strbuilder_append(argument,
                                get_qualified_symbol_name(
                                    named_type_get_symbol(template_type),
                                    decl_context));
                    }
                    if (is_pack_type(current_argument->type))
                    {
                        strbuilder_append(argument, " ...");
                    }
                    break;
                }
            default:
                {
                    internal_error("Undefined template current_argument\n", 0);
                    break;
                }
        }

        const char* result_str = strbuilder_str(result);
        const char* argument_str = strbuilder_str(argument);
        if ((strlen(result_str) > 0)
                && result_str[strlen(result_str) - 1] == '<'
                && argument_str != NULL
                && argument_str[0] == ':')
        {
            strbuilder_append(result, " ");
        }

        strbuilder_append(result, strbuilder_str(argument));
        strbuilder_free(argument);
    }

    if (print_first_level_bracket)
    {
        const char* result_str = strbuilder_str(result);
        if (result_str[strlen(result_str) - 1] == '>')
        {
            strbuilder_append(result, " >");
        }
        else
        {
            strbuilder_append(result, ">");
        }
    }

    const char* str = uniquestr(strbuilder_str(result));
    strbuilder_free(result);

    return str;
}

static const char* print_type_str_internal(type_t* t, const decl_context_t* decl_context, void *data UNUSED_PARAMETER)
{
    return print_type_str(t, decl_context);
}

const char* template_arguments_to_str(
        template_parameter_list_t* template_parameters,
        int first_argument_to_be_printed,
        char print_first_level_bracket,
        const decl_context_t* decl_context)
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
        const decl_context_t* decl_context)
{
    template_parameter_list_t* template_parameters = template_specialized_type_get_template_arguments(entry->type_information);
    return template_arguments_to_str(template_parameters,
            /* first_argument_to_be_printed */ 0,
            /* first_level_brackets */ 1, 
            decl_context);
}

static const char* unmangle_symbol_name_ex(scope_entry_t* entry, const decl_context_t* decl_context)
{
    const char* name = entry->symbol_name;
    // constructor A
    if ((strlen(name) > strlen("constructor "))
            && (strncmp(name, "constructor ", strlen("constructor ")) == 0))
    {
        return uniquestr(name + strlen("constructor "));
    }
    else if (symbol_entity_specs_get_is_conversion(entry))
    {
        return strappend("operator ",
                get_declaration_string_ex(
                    function_type_get_return_type(entry->type_information),
                    decl_context,
                    /* symbol_name */"",
                    /* initializer */ "",
                    /* semicolon */ 0,
                    /* num_parameter_names */ 0,
                    /* parameter_names */ NULL,
                    /* parameter_attributes */ NULL,
                    /* is_parameter */ 0,
                    /* unparenthesize_operator_ptr */ 1,
                    get_simple_type_name_string_internal_common,
                    NULL));
    }
    return name;
}

const char* unmangle_symbol_name(scope_entry_t* entry)
{
    return unmangle_symbol_name_ex(entry, entry->decl_context);
}

static const char* get_fully_qualified_symbol_name_of_depedent_typename_internal_impl(
        scope_entry_t* entry,
        const decl_context_t* decl_context,
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
            if (result[strlen(result) - 1] == '>')
            {
                result = strappend(result, " >");
            }
            else
            {
                result = strappend(result, ">");
            }
        }
    }

    return result;
}

// Get the fully qualified symbol name in the scope of the ocurrence
const char* get_fully_qualified_symbol_name_ex(scope_entry_t* entry,
        const decl_context_t* decl_context,
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

    // If this is the injected symbol, ignore it and get the real entry
    if (symbol_entity_specs_get_is_injected_class_name(entry))
    {
        // The injected class name is a member
        entry = named_type_get_symbol(symbol_entity_specs_get_class_type(entry));
    }

    // Do not print anonymous unions or variables of anonymous unions
    if (!symbol_entity_specs_get_is_anonymous_union(entry)
            && !(is_named_class_type(entry->type_information)
                && symbol_entity_specs_get_is_anonymous_union(named_type_get_symbol(entry->type_information))))
    {
        result = uniquestr(unmangle_symbol_name_ex(entry, decl_context));
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
                symbol_entity_specs_get_template_parameter_nesting(entry),
                symbol_entity_specs_get_template_parameter_position(entry));

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
            && (entry->kind == SK_CLASS
                || (entry->kind == SK_FUNCTION
                    && !symbol_entity_specs_get_is_conversion(entry))
                || entry->kind == SK_TEMPLATE_ALIAS))
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
        if (template_sym->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                || template_sym->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
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

    if (symbol_entity_specs_get_is_member(entry)
            // Lambda classes are unnamed by definition
            && !class_type_is_lambda(symbol_entity_specs_get_class_type(entry)))
    {
        // We need the qualification of the class
        ERROR_CONDITION(!is_named_class_type(symbol_entity_specs_get_class_type(entry)), "The class of a member must be named", 0);

        scope_entry_t* class_symbol = named_type_get_symbol(symbol_entity_specs_get_class_type(entry));

        (*max_qualif_level)++;

        char prev_is_dependent = 0;
        const char* class_qualification =
            get_fully_qualified_symbol_name_ex(class_symbol, decl_context, &prev_is_dependent, max_qualif_level,
                    /* no_templates */ 0, only_classes, do_not_emit_template_keywords, print_type_fun, print_type_data);

        if (!symbol_entity_specs_get_is_anonymous_union(class_symbol))
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
    else if (IS_CXX11_LANGUAGE
            && entry->kind == SK_ENUMERATOR
            && is_named_type(entry->type_information))
    {
        // In C++11 we qualify enumerators
        scope_entry_t* enum_symbol = named_type_get_symbol(entry->type_information);

        // This is a workaround for a <g++-4.10 bug where fully qualifying
        // an unscoped enum may fail in some contexts
#define GCC_UNSCOPED_ENUM_BUG 1

        const char* enum_qualification = NULL;
        char prev_is_dependent = 0;
#ifdef GCC_UNSCOPED_ENUM_BUG
        if (is_scoped_enum_type(entry->type_information))
        {
#endif
            enum_qualification =
                get_fully_qualified_symbol_name_ex(enum_symbol, decl_context, &prev_is_dependent, max_qualif_level,
                        /* no_templates */ 0, only_classes, do_not_emit_template_keywords, print_type_fun, print_type_data);
            enum_qualification = strappend(enum_qualification, "::");
            (*is_dependent) |= prev_is_dependent;

            result = strappend(enum_qualification, result);
#ifdef GCC_UNSCOPED_ENUM_BUG
        }
        else if (symbol_entity_specs_get_is_member(enum_symbol))
        {
            scope_entry_t* class_symbol = named_type_get_symbol(symbol_entity_specs_get_class_type(enum_symbol));
            enum_qualification =
                get_fully_qualified_symbol_name_ex(class_symbol, decl_context, &prev_is_dependent, max_qualif_level,
                        /* no_templates */ 0, only_classes, do_not_emit_template_keywords, print_type_fun, print_type_data);
            enum_qualification = strappend(enum_qualification, "::");
            (*is_dependent) |= prev_is_dependent;

            result = strappend(enum_qualification, result);
        }
        else
        {
            result = get_fully_qualified_symbol_name_simple(enum_symbol, decl_context, result);
        }
#endif
    }
    else if (!symbol_entity_specs_get_is_member(entry)
            && !only_classes)
    {
        // This symbol is already simple enough
        result = get_fully_qualified_symbol_name_simple(entry, decl_context, result);
    }

    return result;
}

const char* get_fully_qualified_symbol_name(scope_entry_t* entry,
        const decl_context_t* decl_context, char* is_dependent, int* max_qualif_level)
{
    return get_fully_qualified_symbol_name_ex(entry,
            decl_context, is_dependent, max_qualif_level,
            /* no_templates */ 0, /* only_classes */ 0,
            /* do_not_emit_template_keywords */ 0,
            print_type_str_internal,
            NULL);
}

const char* get_fully_qualified_symbol_name_without_template(scope_entry_t* entry,
        const decl_context_t* decl_context, char* is_dependent, int* max_qualif_level)
{
    return get_fully_qualified_symbol_name_ex(entry,
            decl_context, is_dependent, max_qualif_level,
            /* no_templates */ 1, /* only_classes */ 0,
            /* do_not_emit_template_keywords */ 0,
            print_type_str_internal,
            NULL);
}

const char* get_class_qualification_of_symbol(scope_entry_t* entry,
        const decl_context_t* decl_context, char* is_dependent, int* max_qualif_level)
{
    return get_fully_qualified_symbol_name_ex(entry,
            decl_context, is_dependent, max_qualif_level,
            /* no_templates */ 0, /* only_classes */ 1,
            /* do_not_emit_template_keywords */ 1,
            print_type_str_internal,
            NULL);
}

const char* get_class_qualification_of_symbol_without_template(scope_entry_t* entry,
        const decl_context_t* decl_context, char* is_dependent, int* max_qualif_level)
{
    return get_fully_qualified_symbol_name_ex(entry,
            decl_context, is_dependent, max_qualif_level,
            /* no_templates */ 1, /* only_classes */ 1,
            /* do_not_emit_template_keywords */ 1,
            print_type_str_internal,
            NULL);
}

const char* get_qualified_symbol_name(scope_entry_t* entry, const decl_context_t* decl_context)
{
    int max_qualif_level = 0;
    char is_dependent = 0;

    return get_fully_qualified_symbol_name(entry, decl_context, &is_dependent, &max_qualif_level);
}

decl_context_t* decl_context_empty()
{
    decl_context_t* result = NEW0(decl_context_t);
    return result;
}

scope_entry_t* lookup_of_template_parameter(const decl_context_t* context,
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

    template_parameter_list_t *template_parameters = context->template_parameters;

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
    if (current_nesting == NULL)
    {
        // Should not happen
        return NULL;
    }

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
            value->entry = NEW0(scope_entry_t);
            value->entry->symbol_name = parameter_entry->symbol_name;
            value->entry->decl_context = context;
            symbol_entity_specs_set_is_template_parameter(value->entry, 1);

            switch (current_template_parameter->kind)
            {
                case TPK_NONTYPE:
                    {
                        value->entry->kind = SK_VARIABLE;
                        value->entry->type_information = value->type;
                        value->entry->value = nodecl_shallow_copy(value->value);
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
                        DELETE(value->entry);
                        value->entry = named_type_get_symbol(value->type);
                        break;
                    }
                case TPK_NONTYPE_PACK:
                    {
                        value->entry->kind = SK_VARIABLE_PACK;
                        value->entry->type_information = value->type;
                        value->entry->value = nodecl_shallow_copy(value->value);
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

                    DELETE(list);
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
        && (ASTKind(a) == AST_SYMBOL
                || ASTKind(a) == AST_TEMPLATE_ID
                || ASTKind(a) == AST_CONVERSION_FUNCTION_ID
                || ASTKind(a) == AST_DESTRUCTOR_ID
                || ASTKind(a) == AST_DESTRUCTOR_TEMPLATE_ID
                || ASTKind(a) == AST_OPERATOR_FUNCTION_ID
                || ASTKind(a) == AST_OPERATOR_FUNCTION_ID_TEMPLATE);
}

char is_qualified_id_expression(AST a)
{
    return a != NULL
        && (ASTKind(a) == AST_QUALIFIED_ID);
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

    if (symbol_entity_specs_get_is_inline(inner_namespace_sym)
                && (contained_in == outer_namespace_scope))
    {
        return 1;
    }
    else if (symbol_entity_specs_get_is_inline(inner_namespace_sym))
    {
        return is_inline_namespace_of_(contained_in, outer_namespace_scope);
    }
    else 
    {
        return 0;
    }
}

char is_inline_namespace_of(const decl_context_t* inner_namespace_ctx, const decl_context_t* outer_namespace_ctx)
{
    return is_inline_namespace_of_(inner_namespace_ctx->namespace_scope, 
            outer_namespace_ctx->namespace_scope);
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

static const char* symbol_kind_descriptive_name_table_str[] =
{
    [SK_UNDEFINED] = "<<invalid symbol>>",
#define SYMBOL_KIND(x, desc) \
        [x] = desc,
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

const char* symbol_kind_descriptive_name(enum cxx_symbol_kind symbol_kind)
{
    ERROR_CONDITION (symbol_kind >= SK_LAST_KIND, "Invalid kind", 0);
    return symbol_kind_descriptive_name_table_str[symbol_kind];
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

    dhash_ptr_walk(sc->dhash, (dhash_ptr_walk_fn*)for_each_fun_adaptor, &fun_adaptor_data);
}

int get_template_nesting_of_context(const decl_context_t* decl_context)
{
    template_parameter_list_t* template_parameters = decl_context->template_parameters;
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
            if (template_parameters->parameters[i] != NULL)
            {
                switch (template_parameters->parameters[i]->kind)
                {
                    case TPK_NONTYPE: kind_name = "nontype"; break;
                    case TPK_TYPE: kind_name = "type"; break;
                    case TPK_TEMPLATE: kind_name = "template"; break;

                    case TPK_NONTYPE_PACK: kind_name = "nontype pack"; break;
                    case TPK_TYPE_PACK: kind_name = "type pack"; break;
                    case TPK_TEMPLATE_PACK: kind_name = "template pack"; break;
                    default: break;
                }
                fprintf(stderr, "* Nesting: %d | Position: %d | Name: %s | Kind : %s\n", *n, i, 
                        template_parameters->parameters[i]->entry != NULL
                        ?  template_parameters->parameters[i]->entry->symbol_name
                        : "<<unknown symbol>>",
                        kind_name);
            }
            else
            {
                fprintf(stderr, "* Nesting: %d | Position: %d | <<unknown parameter>\n", *n, i);
            }

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

                                DELETE(list);
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

scope_entry_list_t* query_nodecl_name_flags(const decl_context_t* decl_context,
        nodecl_t nodecl_name, field_path_t* field_path, decl_flags_t decl_flags);

static scope_entry_list_t* query_nodecl_simple_name(
        const decl_context_t* decl_context,
        const decl_context_t* top_level_decl_context,
        nodecl_t nodecl_name,
        field_path_t* field_path,
        decl_flags_t decl_flags)
{
    ERROR_CONDITION(nodecl_get_kind(nodecl_name) != NODECL_CXX_DEP_NAME_SIMPLE, "Invalid nodecl", 0);

    decl_context_t* decl_context_label = NULL;
    if (BITMAP_TEST(decl_flags, DF_LABEL))
    {
        decl_context_label = decl_context_clone(decl_context);
        decl_context_label->current_scope = decl_context->function_scope;
        decl_context = decl_context_label;
    }

    const locus_t* locus = nodecl_get_locus(nodecl_name);
    const char* name = nodecl_get_text(nodecl_name);

    if (BITMAP_TEST(decl_flags, DF_CONSTRUCTOR))
    {
        name = strappend("constructor ", name);
    }

    scope_entry_list_t* result = name_lookup(decl_context, name,
            field_path, decl_flags, locus);

    if (result != NULL)
    {
        scope_entry_t* head = entry_list_head(result);

        if (symbol_entity_specs_get_is_injected_class_name(head))
        {
            head = named_type_get_symbol(symbol_entity_specs_get_class_type(head));
        }

        if (symbol_entity_specs_get_is_member(head))
        {
            if (class_is_immediate_lexical_scope(top_level_decl_context,
                        named_type_get_symbol(symbol_entity_specs_get_class_type(head))))
            {
                // Do nothing if the class is in scope
            }
            else if (BITMAP_TEST(decl_flags, DF_DEPENDENT_TYPENAME)
                    && !BITMAP_TEST(decl_flags, DF_DO_NOT_CREATE_UNQUALIFIED_DEPENDENT_ENTITY)
                    && is_dependent_type(symbol_entity_specs_get_class_type(head)))
            {
                scope_entry_t* new_sym = NEW0(scope_entry_t);
                new_sym->kind = SK_DEPENDENT_ENTITY;
                new_sym->symbol_name = nodecl_get_text(nodecl_name_get_last_part(nodecl_name));
                new_sym->decl_context = decl_context;
                new_sym->locus = locus;
                new_sym->type_information = build_dependent_typename_for_entry(
                        named_type_get_symbol(symbol_entity_specs_get_class_type(head)),
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
        const decl_context_t* decl_context,
        const decl_context_t* top_level_decl_context UNUSED_PARAMETER,
        nodecl_t nodecl_name,
        decl_flags_t decl_flags UNUSED_PARAMETER)
{
    if (decl_context->current_scope->kind != CLASS_SCOPE)
    {
        internal_error("Code unreachable", 0);
    }

    const char* name = nodecl_get_text(nodecl_name);

    scope_entry_list_t* entry_list = query_name_in_scope(decl_context->class_scope, name);

    return entry_list;
}

static scope_entry_list_t* query_nodecl_simple_name_in_class(
        const decl_context_t* decl_context,
        const decl_context_t* top_level_decl_context,
        nodecl_t nodecl_name,
        field_path_t* field_path,
        decl_flags_t decl_flags)
{
    if (decl_context->current_scope->kind != CLASS_SCOPE)
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
        name = uniquestr(name + 1);

        // First: lookup inside the class
        scope_entry_list_t* entry_list = query_in_class(
                decl_context->current_scope,
                uniquestr(name),
                field_path,
                decl_flags,
                NULL,
                locus);

        // Second: lookup in the global context
        if (entry_list == NULL)
        {
            entry_list = name_lookup(
                    top_level_decl_context,
                    name,
                    NULL,
                    decl_flags,
                    locus);
        }

        if (entry_list == NULL)
            return NULL;

        scope_entry_t* entry = entry_list_head(entry_list);
        scope_entry_t* current_class = decl_context->current_scope->related_entry;
        if (equivalent_types(get_user_defined_type(current_class), get_user_defined_type(entry)))
        {
            scope_entry_t* destructor = class_type_get_destructor(current_class->type_information);
            return entry_list_new(destructor);
        }
        else
        {
            return NULL;
        }
    }
    if (BITMAP_TEST(decl_flags, DF_DEPENDENT_TYPENAME)
            && is_dependent_type(decl_context->current_scope->related_entry->type_information))
    {
        if (class_is_immediate_lexical_scope(top_level_decl_context, decl_context->current_scope->related_entry))
        {
            // Do nothing if the class is in lexical scope
            diagnostic_context_t* dc = diagnostic_context_push_buffered();
            scope_entry_list_t* first_attempt = query_in_class(decl_context->current_scope,
                    name,
                    field_path,
                    decl_flags,
                    NULL,
                    locus);
            diagnostic_context_pop();
            if (first_attempt != NULL)
            {
                diagnostic_context_commit(dc);
                return first_attempt;
            }
            else
            {
                diagnostic_context_discard(dc);
            }
        }

        scope_entry_t* new_sym = NEW0(scope_entry_t);
        new_sym->kind = SK_DEPENDENT_ENTITY;
        new_sym->decl_context = decl_context;
        new_sym->locus = locus;
        new_sym->symbol_name = nodecl_get_text(nodecl_name_get_last_part(nodecl_name));
        new_sym->type_information = build_dependent_typename_for_entry(
                decl_context->current_scope->related_entry,
                nodecl_name, 
                locus);

        return entry_list_new(new_sym);
    }

    return query_in_class(decl_context->current_scope,
            name,
            field_path,
            decl_flags,
            NULL,
            locus);
}

static scope_entry_list_t* query_nodecl_simple_name_in_namespace(
        const decl_context_t* decl_context,
        const decl_context_t* top_level_decl_context UNUSED_PARAMETER,
        nodecl_t nodecl_name,
        field_path_t* field_path UNUSED_PARAMETER,
        decl_flags_t decl_flags)
{
    const locus_t* locus = nodecl_get_locus(nodecl_name);
    const char* name = nodecl_get_text(nodecl_name);

    return qualified_query_in_namespace(decl_context->current_scope->related_entry, 
            name,
            decl_flags,
            locus);
}

scope_entry_list_t* query_nodecl_template_id(
        const decl_context_t* decl_context, 
        const decl_context_t* top_level_decl_context, 
        nodecl_t nodecl_name, 
        field_path_t* field_path,
        decl_flags_t decl_flags,
        scope_entry_list_t* (*query_fun_nodecl)(
            const decl_context_t* current_context, 
            const decl_context_t* top_level_decl_context, 
            nodecl_t, 
            field_path_t*,
            decl_flags_t)
        )
{

    nodecl_t simple_name = nodecl_get_child(nodecl_name, 0);

    // Lookup must behave different for template destructor-ids
    char is_destructor_id = nodecl_get_text(simple_name)[0] == '~';

    template_parameter_list_t* template_parameters = nodecl_get_template_parameters(nodecl_name);

    if (template_parameters == NULL)
        return NULL;

    scope_entry_list_t* entry_list = query_fun_nodecl(
            decl_context, 
            top_level_decl_context,
            simple_name, 
            field_path,
            decl_flags);

    // Ignore injected class name
    if (entry_list != NULL
            && symbol_entity_specs_get_is_injected_class_name(entry_list_head(entry_list)))
    {
        scope_entry_t* entry_name = named_type_get_symbol(
                symbol_entity_specs_get_class_type(entry_list_head(entry_list)));

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

    if (!is_destructor_id)
    {
        // Filter template-names
        enum cxx_symbol_kind template_name_filter[] = {
            SK_TEMPLATE_TEMPLATE_PARAMETER_PACK,
            SK_TEMPLATE_TEMPLATE_PARAMETER,
            SK_TEMPLATE,
            SK_USING,
            SK_USING_TYPENAME,
            SK_DEPENDENT_ENTITY
        };

        scope_entry_list_t* old_entry_list = entry_list;
        entry_list = filter_symbol_kind_set(entry_list, 
                STATIC_ARRAY_LENGTH(template_name_filter), 
                template_name_filter);
        entry_list_free(old_entry_list);

        if (entry_list == NULL)
        {
            return NULL;
        }

        scope_entry_t* template_symbol = entry_advance_aliases(entry_list_head(entry_list));

        if (template_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK
                && !get_is_inside_pack_expansion())
        {
            error_printf_at(nodecl_get_locus(nodecl_name), "template template parameter pack '%s' not inside a pack expansion\n",
                    template_symbol->symbol_name);
            return NULL;
        }

        if (template_symbol->kind == SK_DEPENDENT_ENTITY)
        {
            scope_entry_t* dependent_entity = NULL;
            nodecl_t nodecl_parts = nodecl_null();
            dependent_typename_get_components(template_symbol->type_information, &dependent_entity, &nodecl_parts);
            // nodecl_parts here lacks the template-id part

            scope_entry_t* new_sym = NEW0(scope_entry_t);
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

        scope_entry_t* primary_symbol = named_type_get_symbol(template_type_get_primary_type(generic_type));

        type_t* specialized_type = NULL;
        if (primary_symbol->kind == SK_CLASS)
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
            {
                entry_list_free(entry_list);
                return NULL;
            }

            specialized_type = template_type_get_specialized_type(generic_type,
                    completed_template_parameters,
                    decl_context,
                    nodecl_get_locus(nodecl_name));
            free_template_parameter_list(completed_template_parameters);

            if (specialized_type != NULL)
            {
                ERROR_CONDITION(!is_named_type(specialized_type), "This should be a named type", 0);

                scope_entry_list_t* result = entry_list_new(named_type_get_symbol(specialized_type));

                entry_list_free(entry_list);
                return result;
            }
            else
            {
                entry_list_free(entry_list);
                return NULL;
            }
        }
        else if (primary_symbol->kind == SK_TEMPLATE_ALIAS)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: This is a template alias\n");
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
            free_template_parameter_list(completed_template_parameters);

            if (specialized_type != NULL)
            {
                ERROR_CONDITION(!is_named_type(specialized_type), "This should be a named type", 0);

                scope_entry_list_t* result = entry_list_new(named_type_get_symbol(specialized_type));

                entry_list_free(entry_list);
                return result;
            }
            else
            {
                entry_list_free(entry_list);
                return NULL;
            }
        }
        else if (primary_symbol->kind == SK_FUNCTION)
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
    else
    {
        // Filter destructor template-id
        enum cxx_symbol_kind template_name_filter[] = {
            SK_FUNCTION,
            SK_USING,
            SK_USING_TYPENAME,
        };


        scope_entry_list_t* old_entry_list = entry_list;
        entry_list = filter_symbol_kind_set(entry_list, 
                STATIC_ARRAY_LENGTH(template_name_filter), 
                template_name_filter);
        entry_list_free(old_entry_list);

        if (entry_list == NULL)
        {
            return NULL;
        }

        scope_entry_t* destructor_symbol = entry_advance_aliases(entry_list_head(entry_list));

        if (destructor_symbol->kind == SK_DEPENDENT_ENTITY)
        {
            scope_entry_t* dependent_entity = NULL;
            nodecl_t nodecl_parts = nodecl_null();
            dependent_typename_get_components(destructor_symbol->type_information, &dependent_entity, &nodecl_parts);
            // nodecl_parts here lacks the template-id part

            scope_entry_t* new_sym = NEW0(scope_entry_t);
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
        else if (destructor_symbol->kind != SK_FUNCTION)
        {
            internal_error("Invalid symbol for a template destructor-id", 0);
        }

        ERROR_CONDITION(!symbol_entity_specs_get_is_member(destructor_symbol), "A destructor must be member", 0);

        if (!is_template_specialized_type(get_actual_class_type(symbol_entity_specs_get_class_type(destructor_symbol))))
        {
            error_printf_at(nodecl_get_locus(nodecl_name), "designated class in destructor-id is not a template-type\n");
            return NULL;
        }

        if (is_dependent_type(symbol_entity_specs_get_class_type(destructor_symbol)))
            return entry_list_new(destructor_symbol);

        // If not dependent, we will verify that the template-arguments square
        // with those of the template class

        type_t* generic_type = template_specialized_type_get_related_template_type(
                get_actual_class_type(symbol_entity_specs_get_class_type(destructor_symbol)));
        template_parameter_list_t* completed_template_parameters =
            complete_template_parameters_of_template_class(decl_context,
                    generic_type,
                    template_parameters,
                    nodecl_get_locus(nodecl_name));

        if (completed_template_parameters == NULL)
        {
            entry_list_free(entry_list);
            return NULL;
        }

        type_t* specialized_type = template_type_get_specialized_type(generic_type,
                completed_template_parameters,
                decl_context,
                nodecl_get_locus(nodecl_name));
        free_template_parameter_list(completed_template_parameters);

        if (!equivalent_types(specialized_type, symbol_entity_specs_get_class_type(destructor_symbol)))
        {
            error_printf_at(nodecl_get_locus(nodecl_name), "template destructor-id '%s' is not a valid destructor designation for class '%s'\n",
                    codegen_to_str(nodecl_name, decl_context),
                    print_type_str(symbol_entity_specs_get_class_type(destructor_symbol),
                        decl_context)
                    );
            return NULL;
        }

        return entry_list_new(destructor_symbol);
    }
}

scope_entry_list_t* query_conversion_function_info(const decl_context_t* decl_context, type_t* conversion_type, const locus_t* locus)
{
    ERROR_CONDITION(decl_context->class_scope == NULL, "We need a class scope here", 0);

    scope_entry_list_t* entry_list = query_in_class(decl_context->class_scope, UNIQUESTR_LITERAL("$.operator"), NULL, DF_NONE, conversion_type, locus);

    return entry_list;
}

static scope_entry_list_t* query_nodecl_conversion_name(
        const decl_context_t* decl_context,
        const decl_context_t* top_level_decl_context,
        nodecl_t nodecl_name,
        field_path_t *field_path UNUSED_PARAMETER,
        decl_flags_t decl_flags UNUSED_PARAMETER)
{
    // We need a class scope around that we will check first
    if (decl_context->class_scope == NULL)
    {
        error_printf_at(nodecl_get_locus(nodecl_name), "conversion-id requires an enclosing class scope\n");
        return NULL;
    }

    decl_context_t* class_context = decl_context_clone(decl_context);
    // Lookup in class scope if available
    class_context->current_scope = class_context->class_scope;

    nodecl_t nodecl_conversion_type = nodecl_get_child(nodecl_name, 1);

    // We kept this tree because of the complicated lookup required for conversion-id
    AST type_id = nodecl_get_ast(nodecl_get_child(nodecl_name, 2));
    ERROR_CONDITION((type_id == NULL) == /* Both cannot be NULL or non-NULL at the same time */
            nodecl_is_null(nodecl_get_child(nodecl_name, 1)),
            "This NODECL_CXX_DEP_NAME_CONVERSION has wrong type information", 0);

    type_t* conversion_type = NULL;
    if (type_id != NULL)
    {
        // The type has not yet been synthesized at this point

        // Nullify tree so it won't bee freed afterwards if we discard this tree
        nodecl_set_child(nodecl_name, 2, nodecl_null());

        AST type_specifier_seq = ASTSon0(type_id);
        AST type_spec = ASTSon1(type_specifier_seq);

        // Build the type tree
        if (ASTKind(type_spec) == AST_SIMPLE_TYPE_SPEC)
        {
            AST id_expression = ASTSon0(type_spec);

            const decl_context_t* expression_context =
                nodecl_get_decl_context(nodecl_get_child(nodecl_name, 0));

            nodecl_t nodecl_id_expression = nodecl_null();
            compute_nodecl_name_from_id_expression(id_expression, expression_context, &nodecl_id_expression);

            ast_set_child(type_specifier_seq, 1, nodecl_get_ast(nodecl_id_expression));
        }

        diagnostic_context_push_buffered();
        type_t* type_looked_up_in_class = compute_type_for_type_id_tree(type_id, class_context,
                /* out_simple_type */ NULL, /* out_gather_info */ NULL);
        diagnostic_context_pop_and_discard();

        diagnostic_context_push_buffered();
        type_t* type_looked_up_in_enclosing = compute_type_for_type_id_tree(type_id, top_level_decl_context,
                /* out_simple_type */ NULL, /* out_gather_info */ NULL
                );
        diagnostic_context_pop_and_discard();

        type_t* t = type_looked_up_in_class;
        if (is_error_type(t))
        {
            t = type_looked_up_in_enclosing;
        }
        else if (IS_CXX03_LANGUAGE
                && !is_error_type(type_looked_up_in_enclosing))
        {
            if (!equivalent_types(t, type_looked_up_in_enclosing))
            {
                error_printf_at(
                        nodecl_get_locus(nodecl_name),
                        "type of conversion found in class scope (%s) and the type in "
                        "scope of the id-expression (%s) should match\n",
                        print_type_str(type_looked_up_in_class, class_context),
                        print_type_str(type_looked_up_in_enclosing, top_level_decl_context)
                        );
                return NULL;
            }
        }

        // If still not found, error
        if (is_error_type(t))
        {
            error_printf_at(nodecl_get_locus(nodecl_name), "type-id %s of conversion-id not found\n",
                    prettyprint_in_buffer(type_id));
            return NULL;
        }

        if (class_context->class_scope == NULL)
        {
            error_printf_at(nodecl_get_locus(nodecl_name), "'operator %s' requires a class scope\n",
                    prettyprint_in_buffer(type_id));
            return NULL;
        }

        ast_free(type_id);

        // Keep the type
        nodecl_set_child(
                nodecl_name, 1,
                nodecl_make_type(t, nodecl_get_locus(nodecl_name)));

        conversion_type = t;
    }
    else
    {
        // We already synthesized the type for this conversion id
        conversion_type = nodecl_get_type(nodecl_conversion_type);
    }
    ERROR_CONDITION(conversion_type == NULL, "No type was computed!", 0);

    scope_entry_list_t* result = query_conversion_function_info(class_context, conversion_type, nodecl_get_locus(nodecl_name));
    return result;
}

static scope_entry_list_t* query_nodecl_qualified_name_internal(
        const decl_context_t* decl_context,
        const decl_context_t* current_context,
        scope_entry_t* previous_symbol,
        char allow_namespaces,
        nodecl_t nodecl_name,
        field_path_t* field_path,
        decl_flags_t decl_flags,
        char is_global,
        scope_entry_list_t* query_first_name(const decl_context_t*,
            nodecl_t,
            field_path_t*,
            decl_flags_t,
            char is_global,
            void *data),
        char check_symbol_in_nest(
            // Checks a given A::B::C::x
            // (note that A::B::C might be NULL when the qualified name is ::x)
            scope_entry_t* nested_name_spec_symbol,  // A::B::C
            scope_entry_t* current_sym,              // x looked up inside A::B::C
            const locus_t* locus,
            field_path_t* field_path,
            void* data),
        void *extra_data)
{
    ERROR_CONDITION(previous_symbol != NULL
            && previous_symbol->kind != SK_NAMESPACE
            && previous_symbol->kind != SK_CLASS
            && (!IS_CXX11_LANGUAGE && previous_symbol->kind != SK_ENUM),
            "Invalid previous symbol '%s' of kind '%s'\n",
            previous_symbol->symbol_name,
            symbol_kind_name(previous_symbol));

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
            current_entry_list =
                query_first_name(current_context, current_name, NULL, nested_flags, is_global, extra_data);
        }
        else if (previous_symbol->kind == SK_CLASS)
        {
            if (nodecl_get_kind(current_name) != NODECL_CXX_DEP_TEMPLATE_ID)
            {
                current_entry_list = query_nodecl_simple_name_in_class(
                        current_context,
                        decl_context,
                        current_name,
                        NULL,
                        nested_flags);
            }
            else
            {
                current_entry_list = query_nodecl_template_id(
                        current_context,
                        decl_context,
                        current_name,
                        NULL,
                        nested_flags,
                        query_nodecl_simple_name_in_class);
            }
        }
        else if (previous_symbol->kind == SK_NAMESPACE)
        {
            if (nodecl_get_kind(current_name) == NODECL_CXX_DEP_TEMPLATE_ID)
            {
                current_entry_list = query_nodecl_template_id(
                        current_context,
                        decl_context,
                        current_name,
                        NULL,
                        nested_flags,
                        query_nodecl_simple_name_in_namespace);
            }
            else if (nodecl_get_kind(current_name) == NODECL_CXX_DEP_DECLTYPE)
            {
                // N::decltype(a)::MyType
                //
                // Skipping useless namespace qualification
                current_entry_list =
                    query_first_name(current_context, current_name, NULL, nested_flags, is_global, extra_data);
            }
            else
            {
                current_entry_list = query_nodecl_simple_name_in_namespace(
                        current_context,
                        decl_context,
                        current_name,
                        NULL,
                        nested_flags);
            }
        }
        else if (IS_CXX11_LANGUAGE && previous_symbol->kind == SK_ENUM)
        {
            // We only allow enums to be the previous symbol of the last component
            // of the nested name specifier.
            error_printf_at(nodecl_get_locus(current_name), "invalid nested-name-specifier\n");
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
            DELETE(list);
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
                || (IS_CXX11_LANGUAGE && current_symbol->kind == SK_TEMPLATE_ALIAS)
                || (IS_CXX11_LANGUAGE && current_symbol->kind == SK_DECLTYPE)
                || current_symbol->kind == SK_TYPEDEF
                || current_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER
                || current_symbol->kind == SK_DEPENDENT_ENTITY)
        {
            if (current_symbol->kind == SK_TYPEDEF
                    || current_symbol->kind == SK_TEMPLATE_ALIAS)
            {
                type_t* t = advance_over_typedefs(current_symbol->type_information);

                if (is_dependent_typename_type(t)
                        || is_typeof_expr(t))
                {
                    scope_entry_t* dependent_symbol = create_new_dependent_entity(
                            decl_context,
                            current_symbol,
                            i, num_items,
                            nodecl_get_locus(current_name),
                            list);
                    DELETE(list);

                    if (dependent_symbol == NULL)
                        return NULL;

                    return entry_list_new(dependent_symbol);
                }

                if (!is_named_type(t))
                {
                    CXX03_LANGUAGE()
                    {
                        error_printf_at(nodecl_get_locus(current_name), "typedef name '%s' is not a namespace or class\n",
                                codegen_to_str(current_name, decl_context));
                    }
                    CXX11_LANGUAGE()
                    {
                        error_printf_at(nodecl_get_locus(current_name), "typedef name '%s' is not a namespace, class or enum\n",
                                codegen_to_str(current_name, decl_context));
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
                else
                {
                    class_type_complete_if_possible(current_symbol, current_symbol->decl_context,
                            nodecl_get_locus(current_name));

                    if (class_type_is_incomplete_dependent(class_type)
                            // In some cases we do not want to examine uninstantiated templates
                            || (BITMAP_TEST(decl_flags, DF_DEPENDENT_TYPENAME)
                                // Why are we checking this?
                                && (class_type_is_complete_dependent(class_type)
                                    || (current_symbol->decl_context->current_scope->kind == CLASS_SCOPE
                                        && is_dependent_type(current_symbol->decl_context->current_scope->related_entry->type_information))
                                   )))
                    {
                        scope_entry_t* dependent_symbol = create_new_dependent_entity(
                                decl_context,
                                current_symbol,
                                i, num_items,
                                nodecl_get_locus(current_name),
                                list);
                        DELETE(list);

                        if (dependent_symbol == NULL)
                            return NULL;

                        return entry_list_new(dependent_symbol);
                    }

                    if (is_incomplete_type(current_symbol->type_information))
                    {
                        return NULL;
                    }
                }

                current_context = class_type_get_inner_context(class_type);
            }
            else if (current_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER
                    || current_symbol->kind == SK_DEPENDENT_ENTITY
                    || current_symbol->kind == SK_DECLTYPE)
            {
                scope_entry_t* dependent_symbol = create_new_dependent_entity(
                        decl_context,
                        current_symbol,
                        i, num_items,
                        nodecl_get_locus(current_name),
                        list);
                DELETE(list);

                if (dependent_symbol == NULL)
                    return NULL;

                return entry_list_new(dependent_symbol);
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            allow_namespaces = 0;
        }
        else if (current_symbol->kind == SK_TEMPLATE
                || current_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                || current_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
        {
            error_printf_at(nodecl_get_locus(current_name), "template-name '%s' used without template arguments\n",
                    nodecl_get_text(current_name));
            return NULL;
        }
        else
        {
            CXX03_LANGUAGE()
            {
                error_printf_at(nodecl_get_locus(current_name), "name '%s' is not a namespace or class\n",
                        codegen_to_str(current_name, decl_context));
            }
            CXX11_LANGUAGE()
            {
                error_printf_at(nodecl_get_locus(current_name), "name '%s' is not a namespace, class or enum\n",
                        codegen_to_str(current_name, decl_context));
            }
            return NULL;
        }
        previous_symbol = current_symbol;
    }

    nodecl_t last_name = list[num_items - 1];

    scope_entry_list_t* result = NULL;

    if (previous_symbol == NULL)
    {
        // Note that previous_symbol can be NULL if this qualified-id is of the
        // form ::Name (without a nested-name-specifier)
        result = query_nodecl_name_flags(current_context, last_name, NULL, decl_flags);
    }
    else if (previous_symbol->kind == SK_NAMESPACE)
    {
        if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_NAME_SIMPLE)
        {
            result = query_nodecl_simple_name_in_namespace(
                    current_context,
                    decl_context,
                    last_name,
                    NULL,
                    decl_flags);
        }
        else if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_TEMPLATE_ID)
        {
            result = query_nodecl_template_id(
                    current_context,
                    decl_context,
                    last_name,
                    NULL,
                    decl_flags,
                    query_nodecl_simple_name_in_namespace);
        }
        else if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_NAME_CONVERSION)
        {
            error_printf_at(nodecl_get_locus(last_name), "conversion-id is not valid in a non-class scope\n");
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
                    field_path,
                    decl_flags);
        }
        else if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_TEMPLATE_ID)
        {
            result = query_nodecl_template_id(
                    current_context,
                    decl_context,
                    last_name, 
                    field_path,
                    decl_flags,
                    query_nodecl_simple_name_in_class);
        }
        else if (nodecl_get_kind(last_name) == NODECL_CXX_DEP_NAME_CONVERSION)
        {
            result = query_nodecl_conversion_name(current_context, decl_context,
                    last_name, field_path, decl_flags);
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
            error_printf_at(nodecl_get_locus(last_name), "invalid name in nested-name specifier\n");
            return NULL;
        }
    }

    if (result != NULL
            && check_symbol_in_nest != NULL
            && !check_symbol_in_nest(
                previous_symbol,
                entry_list_head(result),
                nodecl_get_locus(last_name),
                field_path,
                extra_data))
    {
        DELETE(result);
        return NULL;
    }

    DELETE(list);

    return result;
}

static scope_entry_list_t* query_nodecl_name_first(const decl_context_t* current_context,
        nodecl_t current_name,
        field_path_t* field_path,
        decl_flags_t decl_flags,
        char is_global UNUSED_PARAMETER,
        void *extra_info UNUSED_PARAMETER)
{
    if (nodecl_get_kind(current_name) == NODECL_CXX_DEP_NAME_SIMPLE)
    {
        return query_nodecl_simple_name(current_context,
                current_context,
                current_name,
                field_path,
                decl_flags | DF_NESTED_NAME_FIRST);
    }
    else
    {
        return query_nodecl_name_flags(current_context, current_name, field_path, decl_flags);
    }
}

struct class_lookup_info_tag
{
    scope_entry_t* class_symbol;
    char is_destructor_id;
};

static scope_entry_list_t* query_nodecl_name_first_in_class(
        const decl_context_t* current_context,
        nodecl_t current_name,
        field_path_t* field_path UNUSED_PARAMETER,
        decl_flags_t decl_flags,
        char is_global,
        void *extra_info)
{
    scope_entry_t* class_symbol = ((struct class_lookup_info_tag*)extra_info)->class_symbol;
    char is_destructor_id = ((struct class_lookup_info_tag*)extra_info)->is_destructor_id;

    scope_entry_list_t* entry_list_postfix =
        query_nodecl_name_flags(current_context, current_name, NULL, decl_flags);

    scope_entry_list_t* entry_list_in_class = NULL;
    if (!is_global)
    {
        if (nodecl_get_kind(current_name) != NODECL_CXX_DEP_TEMPLATE_ID)
        {
            entry_list_in_class = query_nodecl_simple_name_in_class(
                    class_type_get_inner_context(class_symbol->type_information),
                    current_context,
                    current_name,
                    NULL,
                    decl_flags);
        }
        else
        {
            entry_list_in_class = query_nodecl_template_id(
                    class_type_get_inner_context(class_symbol->type_information),
                    current_context,
                    current_name,
                    NULL,
                    decl_flags,
                    query_nodecl_simple_name_in_class);
        }
    }

    /*
       If the id-expression in a class member access is a qualified-id of the
       form

           class-name-or-namespace-name::...

       the class-name-or-namespace-name following the . or -> operator is
       looked up both in the context of the entire postfix-expression and in
       the scope of the class of the object expression. If the name is found
       only in the scope of the class of the object expression, the name shall
       refer to a class-name. If the name is found only in the context of the
       entire postfix-expression, the name shall refer to a class-name or
       namespace-name.  If the name is found in both contexts, the
       class-name-or-namespace-name shall refer to the same entity.
    */

    if (entry_list_postfix != NULL
            && entry_list_in_class != NULL)
    {
        scope_entry_t* entry1 = entry_list_head(entry_list_postfix);
        scope_entry_t* entry2 = entry_list_head(entry_list_in_class);

        if (symbol_entity_specs_get_is_injected_class_name(entry1))
        {
            entry1 = class_symbol = named_type_get_symbol(symbol_entity_specs_get_class_type(entry1));
        }
        if (symbol_entity_specs_get_is_injected_class_name(entry2))
        {
            entry2 = class_symbol = named_type_get_symbol(symbol_entity_specs_get_class_type(entry2));
        }

        if (entry1 != entry2)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: When looking up the first part of the id-expression "
                        "of a class-member-access the two lookups do not match\n");
            }
            if (is_destructor_id
                    && entry1->kind == SK_TEMPLATE
                    && entry2->kind == SK_CLASS
                    && is_template_specialized_type(entry2->type_information)
                    && !is_dependent_type(entry2->type_information)
                    && template_type_get_related_symbol(
                        template_specialized_type_get_related_template_type(
                            entry2->type_information)) == entry1)
            {
                /* This is for this case
                 *
                 * template <typename T>
                 * struct A { };
                 *
                 * void f(A<int>& a)
                 * {
                 *    a.A::~A();
                 *    // Note that 'A' here is a template-name at the postfix
                 *    // lookup but the injected class-name inside the class.
                 *    // So we act as if the user had written a.A<int>::~A();
                 * }
                 */
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: But we are looking up a destructor-id and we are naming the template-name of "
                            "the injected class-name, so the lookup will use the in-class lookup result\n");
                }
                return entry_list_in_class;
            }
            else
            {
                return NULL;
            }
        }

        entry_list_free(entry_list_in_class);

        return entry_list_postfix;
    }
    else if (entry_list_postfix != NULL)
    {
        scope_entry_t* entry = entry_list_head(entry_list_postfix);

        if (entry->kind == SK_TYPEDEF)
        {
            type_t* t = advance_over_typedefs(entry->type_information);

            if (!is_named_type(t))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: When looking up the first part of the "
                            "id-expression we found it as a typedef-name in the context of the postfix "
                            "expression but it is not a class\n");
                }
                return NULL;
            }

            entry = named_type_get_symbol(t);
        }

        if (entry->kind != SK_NAMESPACE
                && entry->kind != SK_CLASS)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: When looking up the first part of the "
                        "id-expression we only found it in the context of the postfix "
                        "expression but it is not a namespace nor a class\n");
            }
            return NULL;
        }

        return entry_list_postfix;
    }
    else if (entry_list_in_class != NULL)
    {
        scope_entry_t* entry = entry_list_head(entry_list_in_class);

        if (entry->kind == SK_TYPEDEF)
        {
            type_t* t = advance_over_typedefs(entry->type_information);

            if (!is_named_type(t))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "SCOPE: When looking up the first part of the "
                            "id-expression we only found it in the context of the class "
                            "as a typedef-name but it is not a class\n");
                }
                return NULL;
            }

            entry = named_type_get_symbol(t);
        }

        if (entry->kind != SK_CLASS)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: When looking up the first part of the "
                        "id-expression we only found it in the context of the class "
                        "but it is not a class\n");
            }
            return NULL;
        }

        return entry_list_in_class;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCOPE: When looking up the first part of the "
                    "id-expression we did not find it anywhere\n");
        }
        return NULL;
    }
}

static scope_entry_list_t* query_nodecl_qualified_name_common(const decl_context_t* decl_context,
        nodecl_t nodecl_name,
        field_path_t *field_path,
        decl_flags_t decl_flags,
        char allow_namespaces,
        scope_entry_list_t* query_first_name(const decl_context_t*,
            nodecl_t,
            field_path_t*,
            decl_flags_t,
            char is_global,
            void *data),
        char check_symbol_in_nest(scope_entry_t* previous_sym,
            scope_entry_t* current_sym,
            const locus_t* locus,
            field_path_t* field_path,
            void* data),
        void *extra_data)
{
    scope_entry_t* previous_symbol = NULL;
    char is_global = (nodecl_get_kind(nodecl_name) == NODECL_CXX_DEP_GLOBAL_NAME_NESTED);

    decl_context_t *global_context = NULL;
    const decl_context_t* current_context = decl_context;
    if (is_global)
    {
        global_context = decl_context_clone(decl_context);
        global_context->current_scope = global_context->global_scope;
        previous_symbol = global_context->global_scope->related_entry;
        allow_namespaces = 1;

        current_context = global_context;
    }

    return query_nodecl_qualified_name_internal(
            decl_context,
            current_context,
            previous_symbol,
            allow_namespaces,
            nodecl_name,
            field_path,
            decl_flags,
            is_global,
            query_first_name,
            check_symbol_in_nest,
            extra_data);
}

static scope_entry_list_t* query_nodecl_qualified_name(const decl_context_t* decl_context,
        nodecl_t nodecl_name,
        field_path_t* field_path,
        decl_flags_t decl_flags)
{
    return query_nodecl_qualified_name_common(decl_context,
            nodecl_name,
            field_path,
            decl_flags,
            /* allow_namespaces */ 1,
            query_nodecl_name_first,
            NULL, NULL);
}

#if 0
static char field_path_prepend_with_subobject_path(
        field_path_t* field_path,
        scope_entry_t* first_base_path,
        scope_entry_t* class_symbol)
{
    if (equivalent_types(first_base_path->type_information,
                class_symbol->type_information))
    {
        // This is the class we were looking for
        return 1;
    }
    else
    {
        // Walk upwards
        int i;
        int num_bases = class_type_get_num_bases(class_symbol->type_information);
        for (i = 0; i < num_bases; i++)
        {
            char is_virtual = 0;
            char is_dependent = 0;
            char is_expansion = 0;
            access_specifier_t access_specifier = AS_UNKNOWN;
            scope_entry_t* current_base = class_type_get_base_num(class_symbol->type_information, i,
                    &is_virtual,
                    &is_dependent,
                    &is_expansion,
                    &access_specifier);

            // Should not happen, ignore them
            if (is_dependent || is_expansion)
                continue;

            if (field_path_prepend_with_subobject_path(field_path,
                        first_base_path,
                        current_base))
            {
                field_path_prepend(field_path, class_symbol);

                // There is no need to check anything else because we know this base is not ambiguous
                return 1;
            }
        }
    }
    return 0;
}
#endif

static char check_symbol_is_base_or_member(
        scope_entry_t* nested_name_spec_symbol,
        scope_entry_t* current_symbol, 
        const locus_t* locus,
        field_path_t* field_path,
        void* data)
{
    scope_entry_t* class_symbol = ((struct class_lookup_info_tag*)data)->class_symbol;

    if (symbol_entity_specs_get_is_injected_class_name(class_symbol))
    {
        class_symbol = named_type_get_symbol(symbol_entity_specs_get_class_type(class_symbol));
    }

    if (class_symbol->kind != SK_CLASS)
    {
        error_printf_at(locus, "'%s' must be a class\n",
                class_symbol->symbol_name);
        return 0;
    }

    if (nested_name_spec_symbol != NULL)
    {
        scope_entry_t* checked_symbol = nested_name_spec_symbol;
        if (nested_name_spec_symbol->kind == SK_NAMESPACE)
        {
            // This qualified-id is of the form N::C or N1::N2::C
            checked_symbol = current_symbol;
        }

        if (checked_symbol->kind != SK_CLASS)
        {
            error_printf_at(locus, "'%s' must be a class\n",
                    checked_symbol->symbol_name);
            return 0;
        }
        else if (!class_type_is_base_instantiating(
                    checked_symbol->type_information,
                    class_symbol->type_information,
                    locus))
        {
            error_printf_at(locus, "'%s' is not a base of '%s'\n",
                    get_qualified_symbol_name(checked_symbol, checked_symbol->decl_context),
                    get_qualified_symbol_name(class_symbol, class_symbol->decl_context));
            return 0;
        }
        else if (class_type_is_ambiguous_base_of_derived_class(
                    checked_symbol->type_information,
                    class_symbol->type_information))
        {
            error_printf_at(locus, "'%s' is an ambiguous base of '%s'\n",
                    get_qualified_symbol_name(checked_symbol, checked_symbol->decl_context),
                    get_qualified_symbol_name(class_symbol, class_symbol->decl_context));
            return 0;
        }
    }

    // If we are the last component we must be a member of class_symbol (unless
    // our nested-name-specifier designated a namespace
    if ((nested_name_spec_symbol == NULL
            || nested_name_spec_symbol->kind != SK_NAMESPACE)
            && !(symbol_entity_specs_get_is_member(current_symbol)
                && class_type_is_base_instantiating(
                    symbol_entity_specs_get_class_type(current_symbol),
                    get_user_defined_type(class_symbol),
                    locus)))
    {
        error_printf_at(locus, "'%s' is not a member of '%s'\n",
                get_qualified_symbol_name(current_symbol, current_symbol->decl_context),
                get_qualified_symbol_name(class_symbol, class_symbol->decl_context));
        return 0;
    }


    // Remember this unambiguous base
    field_path_add(field_path, nested_name_spec_symbol);

    return 1;
}

static scope_entry_list_t* query_nodecl_qualified_name_in_class(const decl_context_t* decl_context,
        scope_entry_t* class_symbol,
        nodecl_t nodecl_name,
        field_path_t* field_path,
        decl_flags_t decl_flags)
{
    nodecl_t nodecl_last = nodecl_name_get_last_part(nodecl_name);
    char is_destructor_id = (nodecl_get_kind(nodecl_last) == NODECL_CXX_DEP_NAME_SIMPLE
            && (nodecl_get_text(nodecl_last)[0] == '~'))
        || (nodecl_get_kind(nodecl_last) == NODECL_CXX_DEP_TEMPLATE_ID
                && ((nodecl_get_text(nodecl_get_child(nodecl_last, 0))[0]) == '~'));


    struct class_lookup_info_tag class_lookup_info =
    {
        .class_symbol = class_symbol,
        .is_destructor_id = is_destructor_id,
    };

    return query_nodecl_qualified_name_common(decl_context,
            nodecl_name,
            field_path,
            decl_flags,
            /* allow_namespaces */ 1,
            query_nodecl_name_first_in_class,
            check_symbol_is_base_or_member,
            &class_lookup_info);
}

static scope_entry_list_t* query_nodecl_name_in_class_aux(
        const decl_context_t* decl_context,
        scope_entry_t* class_symbol,
        nodecl_t nodecl_name, 
        field_path_t* field_path,
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
                        field_path,
                        decl_flags);
                break;
            }
        case NODECL_CXX_DEP_TEMPLATE_ID:
            {
                return query_nodecl_template_id(
                        class_type_get_inner_context(class_symbol->type_information), 
                        decl_context,
                        nodecl_name, 
                        field_path,
                        decl_flags, 
                        query_nodecl_simple_name_in_class);
                break;
            }
        case NODECL_CXX_DEP_NAME_CONVERSION:
            {
                return query_nodecl_conversion_name(
                        class_type_get_inner_context(class_symbol->type_information), 
                        decl_context,
                        nodecl_name,
                        field_path,
                        decl_flags);
                break;
            }
        case NODECL_CXX_DEP_NAME_NESTED:
        case NODECL_CXX_DEP_GLOBAL_NAME_NESTED:
            {
                return query_nodecl_qualified_name_in_class(decl_context, class_symbol, nodecl_name, field_path, decl_flags);
                break;
            }
        default:
            {
                internal_error("Invalid nodecl kind '%s'\n", ast_print_node_type(nodecl_get_kind(nodecl_name)));
            }
    }
    return NULL;
}

scope_entry_list_t* query_nodecl_name_in_class_flags(
        const decl_context_t* decl_context,
        scope_entry_t* class_symbol,
        nodecl_t nodecl_name,
        field_path_t* field_path,
        decl_flags_t decl_flags)
{
    ERROR_CONDITION(class_symbol == NULL
            || class_symbol->kind != SK_CLASS, "Invalid symbol", 0);

    type_t* class_type = class_symbol->type_information;

    class_type_complete_if_needed(class_symbol, class_symbol->decl_context,
            nodecl_get_locus(nodecl_name));

    if (is_incomplete_type(class_symbol->type_information))
        return NULL;

    const decl_context_t* inner_class_context = class_type_get_inner_context(class_type);
    if (inner_class_context->class_scope == NULL)
    {
        // There is no way this can suceed at all
        return NULL;
    }

    scope_entry_list_t* entry_list = query_nodecl_name_in_class_aux(decl_context, class_symbol, nodecl_name, field_path, decl_flags);

    return entry_list;
}

static scope_entry_list_t* query_nodecl_decltype(const decl_context_t* decl_context, nodecl_t nodecl_name)
{
    scope_entry_list_t* result = NULL;
    type_t* computed_type = nodecl_get_type(nodecl_name);
    if (!is_dependent_type(computed_type))
    {
        scope_entry_t* class_symbol = named_type_get_symbol(computed_type);
        result = entry_list_new(class_symbol);
    }
    else
    {
        // Creating a new artificial symbol that represents the whole decltype-specifier
        scope_entry_t* new_sym = NEW0(scope_entry_t);
        new_sym->kind = SK_DECLTYPE;
        new_sym->locus = nodecl_get_locus(nodecl_name);
        new_sym->symbol_name = ".decltype_auxiliar_symbol";
        new_sym->decl_context = decl_context;
        new_sym->type_information = computed_type;
        result = entry_list_new(new_sym);
    }
    return result;
}

scope_entry_list_t* query_nodecl_name_flags(const decl_context_t* decl_context,
        nodecl_t nodecl_name,
        field_path_t* field_path,
        decl_flags_t decl_flags)
{
    switch (nodecl_get_kind(nodecl_name))
    {
        case NODECL_CXX_DEP_NAME_SIMPLE:
            {
                return query_nodecl_simple_name(decl_context, decl_context, nodecl_name, field_path, decl_flags);
            }
        case NODECL_CXX_DEP_TEMPLATE_ID:
            {
                return query_nodecl_template_id(decl_context, decl_context,
                        nodecl_name, field_path, decl_flags, query_nodecl_simple_name);
            }
        case NODECL_CXX_DEP_NAME_CONVERSION:
            {
                return query_nodecl_conversion_name(decl_context, decl_context, nodecl_name, field_path, decl_flags);
            }
        case NODECL_CXX_DEP_NAME_NESTED:
        case NODECL_CXX_DEP_GLOBAL_NAME_NESTED:
            {
                return query_nodecl_qualified_name(decl_context, nodecl_name, field_path, decl_flags);
            }
        case NODECL_CXX_DEP_DECLTYPE:
            {
                return query_nodecl_decltype(decl_context, nodecl_name);
            }
        default:
            {
                internal_error("Invalid nodecl kind '%s'\n", ast_print_node_type(nodecl_get_kind(nodecl_name)));
            }
    }
    return NULL;
}

static void compute_nodecl_name_from_unqualified_id(AST unqualified_id, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    switch (ASTKind(unqualified_id))
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

                template_parameter_list_t* template_parameters =
                    get_template_arguments_from_syntax(template_arguments, decl_context);

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

                template_parameter_list_t* template_parameters =
                        get_template_arguments_from_syntax(template_arguments, decl_context);

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

                // This is ugly but we need to keep the original tree around before lowering it into nodecl
                // So we copy the AST_CONVERSION_TYPE_ID
                AST conversion_type_id =
                        ASTSon0(unqualified_id);

                // Make some work to prevent that ambiguities slip in
                AST type_specifier_seq = ASTSon0(conversion_type_id);
                AST type_spec = ASTSon1(type_specifier_seq);

                // Build the type tree
                if (ASTKind(type_spec) == AST_SIMPLE_TYPE_SPEC)
                {
                    AST id_expression = ASTSon0(type_spec);

                    nodecl_t nodecl_id_expression = nodecl_null();
                    // This will fix the tree as a side effect
                    compute_nodecl_name_from_id_expression(id_expression, decl_context, &nodecl_id_expression);
                }

                conversion_type_id = ast_copy(conversion_type_id);

                *nodecl_output = nodecl_make_cxx_dep_name_conversion(
                        nodecl_make_context(
                            /* optional statement sequence */ nodecl_null(),
                            decl_context,
                            ast_get_locus(unqualified_id)),
                        nodecl_null(),
                        /* literal type id */ _nodecl_wrap(conversion_type_id),
                        ast_get_locus(unqualified_id));


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
                // This syntax has a redundant template argument ~A<int>
                const char* name = ASTText(unqualified_id);

                AST template_id = ASTSon0(unqualified_id);
                AST template_arguments = ASTSon1(template_id);

                template_parameter_list_t* template_parameters =
                    get_template_arguments_from_syntax(template_arguments, decl_context);

                if (template_parameters == NULL)
                {
                    *nodecl_output = nodecl_make_err_expr(ast_get_locus(unqualified_id));
                    return;
                }

                const char* template_tag = "";
                if (ast_get_text(template_id) != NULL)
                    template_tag = ast_get_text(template_id);

                *nodecl_output = nodecl_make_cxx_dep_template_id(
                        nodecl_make_cxx_dep_name_simple(
                            name,
                            ast_get_locus(unqualified_id)),
                        template_tag,
                        template_parameters,
                        ast_get_locus(unqualified_id));

                break;
            }
        case AST_DECLTYPE:
            {
                type_t* computed_type =
                    compute_type_of_decltype(unqualified_id, decl_context);

                if (is_error_type(computed_type))
                {
                    *nodecl_output = nodecl_make_err_expr(ast_get_locus(unqualified_id));
                    return;
                }

                if (!is_dependent_type(no_ref(computed_type))
                        && !is_class_type(no_ref(computed_type))
                        && !is_enum_type(no_ref(computed_type)))
                {
                    error_printf_at(ast_get_locus(unqualified_id),
                            "'%s' does not name a class or enum type\n",
                            print_type_str(no_ref(computed_type), decl_context));

                    *nodecl_output = nodecl_make_err_expr(ast_get_locus(unqualified_id));
                    return;
                }

                *nodecl_output = nodecl_make_cxx_dep_decltype(
                        computed_type,
                        ast_get_locus(unqualified_id));
                break;
            }
        default:
            {
                internal_error("Unexpected tree of type '%s'\n", ast_print_node_type(ASTKind(unqualified_id)));
            }
    }
}

void compute_nodecl_name_from_nested_part(AST nested_part,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_nested = nodecl_null();

    if (nested_part != NULL)
    {
        if (ASTKind(nested_part) == AST_AMBIGUITY)
        {
            solve_ambiguous_nested_part(nested_part, decl_context);
        }

        AST it;
        for_each_element(nested_part, it)
        {
            AST nested_name = ASTSon1(it);

            nodecl_t nodecl_current = nodecl_null();
            compute_nodecl_name_from_unqualified_id(
                    nested_name,
                    decl_context,
                    &nodecl_current);

            if (nodecl_is_err_expr(nodecl_current))
            {
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(nested_part));
                return;
            }

            nodecl_nested = nodecl_append_to_list(nodecl_nested, nodecl_current);
        }
    }
    
    *nodecl_output = nodecl_nested;
}

void compute_nodecl_name_from_nested_name(AST nested_part, 
        AST unqualified_part, 
        const decl_context_t* decl_context, 
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

void compute_nodecl_name_from_qualified_name(AST global_op, AST nested_name_spec, AST unqualified_id, const decl_context_t* decl_context,
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

void compute_nodecl_name_from_id_expression(AST id_expression, const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    switch (ASTKind(id_expression))
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

scope_entry_list_t* class_context_lookup(const decl_context_t* decl_context, 
        field_path_t* field_path,
        decl_flags_t decl_flags,
        const char* name,
        const locus_t* locus)
{
    ERROR_CONDITION(decl_context->current_scope->kind != CLASS_SCOPE, "This is not a class scope", 0);

    return query_in_class(decl_context->current_scope, name, field_path, decl_flags, NULL, locus);
}

scope_entry_list_t* query_dependent_entity_in_context(
        const decl_context_t* decl_context,
        scope_entry_t* dependent_entity,
        int pack_index,
        field_path_t* field_path,
        instantiation_symbol_map_t* instantiation_symbol_map,
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
                type_t* new_class_type = update_type_for_instantiation(
                        get_user_defined_type(dependent_entry),
                        decl_context, locus,
                        instantiation_symbol_map,
                        pack_index);

                if (is_dependent_typename_type(new_class_type))
                {
                    // We are updating the base entry (T) of a dependent typename
                    // [T]::T1 with another dependent typename [S]::S2
                    // so the updated type should be [S]::S2::T1
                    scope_entry_t* new_class_dependent_entry = NULL;
                    nodecl_t new_class_dependent_parts = nodecl_null();
                    dependent_typename_get_components(new_class_type,
                            &new_class_dependent_entry, &new_class_dependent_parts);

                    // Now append the dependent parts of this type
                    nodecl_t appended_dependent_parts = nodecl_null();

                    int num_items = 0;
                    nodecl_t* list = NULL;
                    int i;

                    ERROR_CONDITION(nodecl_get_kind(new_class_dependent_parts) != NODECL_CXX_DEP_NAME_NESTED, "Invalid tree kind", 0);

                    list = nodecl_unpack_list(nodecl_get_child(new_class_dependent_parts, 0), &num_items);
                    for (i = 0; i < num_items; i++)
                    {
                        appended_dependent_parts = nodecl_append_to_list(appended_dependent_parts, list[i]);
                    }
                    DELETE(list);

                    ERROR_CONDITION(nodecl_get_kind(dependent_parts) != NODECL_CXX_DEP_NAME_NESTED, "Invalid tree kind", 0);

                    list = nodecl_unpack_list(nodecl_get_child(dependent_parts, 0), &num_items);
                    for (i = 0; i < num_items; i++)
                    {
                        appended_dependent_parts = nodecl_append_to_list(appended_dependent_parts, list[i]);
                    }
                    DELETE(list);

                    dependent_parts = nodecl_make_cxx_dep_name_nested(
                            appended_dependent_parts,
                            nodecl_get_locus(dependent_parts));

                    scope_entry_t* new_sym = NEW0(scope_entry_t);
                    new_sym->kind = SK_DEPENDENT_ENTITY;
                    new_sym->locus = locus;
                    new_sym->symbol_name = new_class_dependent_entry->symbol_name;
                    new_sym->decl_context = decl_context;
                    new_sym->type_information = get_dependent_typename_type_from_parts(
                            new_class_dependent_entry,
                            dependent_parts);

                    return entry_list_new(new_sym);
                }
                else if (is_named_type(new_class_type)
                        && is_dependent_type(new_class_type))
                {
                    scope_entry_t* new_sym = NEW0(scope_entry_t);
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
                    error_printf_at(locus, "'%s' does not name a class type\n",
                            print_type_str(dependent_entity->type_information, dependent_entity->decl_context));
                    return NULL;
                }
                else
                {
                    if (!nodecl_is_null(dependent_parts))
                    {
                        scope_entry_t* class_sym = named_type_get_symbol(new_class_type);

                        if (class_sym->kind == SK_TYPEDEF)
                        {
                            // Make sure a typedef does not slip in since
                            // lookup functions may not expect them
                            class_sym = named_type_get_symbol(
                                    advance_over_typedefs(class_sym->type_information)
                                    );
                        }

                        // Make sure class_type_get_inner_context does not return a bogus context below
                        class_type_complete_if_needed(class_sym, class_sym->decl_context, locus);

                        if (is_incomplete_type(class_sym->type_information))
                            return NULL;

                        nodecl_t update_dependent_parts = update_dependent_typename_dependent_parts(
                                dependent_parts,
                                decl_context,
                                instantiation_symbol_map,
                                locus, pack_index);

                        ERROR_CONDITION(nodecl_get_kind(update_dependent_parts) == NODECL_CXX_DEP_GLOBAL_NAME_NESTED,
                                "This is not possible here", 0);
                        if (nodecl_get_kind(update_dependent_parts) != NODECL_CXX_DEP_NAME_NESTED)
                        {
                            return query_nodecl_name_flags(
                                    class_type_get_inner_context(class_sym->type_information),
                                    update_dependent_parts,
                                    field_path,
                                    DF_DEPENDENT_TYPENAME);
                        }
                        else
                        {
                            return query_nodecl_qualified_name_internal(
                                    decl_context,
                                    /* current_context */ class_type_get_inner_context(class_sym->type_information),
                                    class_sym,
                                    /* allow_namespaces */ 0,
                                    update_dependent_parts,
                                    field_path,
                                    DF_DEPENDENT_TYPENAME,
                                    /* is_global */ 0,
                                    query_nodecl_name_first,
                                    NULL,
                                    NULL);
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
    if (symbol_entity_specs_get_from_module(entry)
            && symbol_entity_specs_get_alias_to(entry) != NULL)
    {
        return get_ultimate_symbol_from_module(symbol_entity_specs_get_alias_to(entry));
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

    int i, num_params = symbol_entity_specs_get_num_function_parameter_info(entry);
    for (i = 0; (i < num_params) && (idx < 0); i++)
    {
        if (symbol_entity_specs_get_function_parameter_info_num(entry, i).function == function)
            idx = i;
    }

    function_parameter_info_t function_parameter_info;
    memset(&function_parameter_info, 0, sizeof(function_parameter_info));

    function_parameter_info.function = function;
    function_parameter_info.nesting = nesting;
    function_parameter_info.position = position;

    if (idx < 0)
    {
        symbol_entity_specs_add_function_parameter_info(entry, function_parameter_info);
    }
    else
    {
        symbol_entity_specs_set_function_parameter_info_num(entry, idx, function_parameter_info);
    }
}

char symbol_is_parameter_of_function(scope_entry_t* entry, scope_entry_t* function)
{
    ERROR_CONDITION(entry == NULL, "The symbol is null", 0);
    ERROR_CONDITION(function == NULL, "The function symbol should not be null", 0);

    function = get_ultimate_symbol_from_module(function);

    int idx = -1;
    int i, num_params = symbol_entity_specs_get_num_function_parameter_info(entry);
    for (i = 0; (i < num_params) && (idx < 0); i++)
    {
        if (symbol_entity_specs_get_function_parameter_info_num(entry, i).function == function)
            idx = i;
    }

    return !(idx < 0);
}

int symbol_get_parameter_position_in_function(scope_entry_t* entry, scope_entry_t* function)
{
    ERROR_CONDITION(entry == NULL, "The symbol is null", 0);
    ERROR_CONDITION(function == NULL, "The function symbol should not be null", 0);

    function = get_ultimate_symbol_from_module(function);

    int i, num_params = symbol_entity_specs_get_num_function_parameter_info(entry);
    for (i = 0; (i < num_params); i++)
    {
        if (symbol_entity_specs_get_function_parameter_info_num(entry, i).function == function)
        {
            return symbol_entity_specs_get_function_parameter_info_num(entry, i).position;
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
    for (i = 0; (i < symbol_entity_specs_get_num_function_parameter_info(entry)); i++)
    {
        if (symbol_entity_specs_get_function_parameter_info_num(entry, i).function == function)
        {
            return symbol_entity_specs_get_function_parameter_info_num(entry, i).nesting;
        }
    }

    internal_error("This symbol is not a parameter of the function", 0);
}

char is_dependent_function(scope_entry_t* entry)
{
    ERROR_CONDITION(entry == NULL, "The symbol is null", 0);

    // If the function has dependent type, or is a dependent friend or a member
    // of a dependent class, then it is dependent
    return is_dependent_type(entry->type_information)
        || entry->kind == SK_DEPENDENT_FRIEND_FUNCTION
        || (symbol_entity_specs_get_is_member(entry)
                && is_dependent_type(symbol_entity_specs_get_class_type(entry)));
}

char symbol_has_gcc_attribute(scope_entry_t* entry, const char* name, gcc_attribute_t* gcc_attr)
{
    ERROR_CONDITION(entry == NULL, "Invalid symbol", 0);
    int i;
    int num_attrs = symbol_entity_specs_get_num_gcc_attributes(entry);
    for (i = 0; i < num_attrs; i++)
    {
        if (strcmp(symbol_entity_specs_get_gcc_attributes_num(entry, i).attribute_name, name) == 0)
        {
            if (gcc_attr != NULL)
                *gcc_attr = symbol_entity_specs_get_gcc_attributes_num(entry, i);
            return 1;
        }
    }
    return 0;
}

void symbol_update_gcc_attribute(scope_entry_t* entry, const char* name, gcc_attribute_t gcc_attr)
{
    ERROR_CONDITION(entry == NULL, "Invalid symbol", 0);
    int i;
    int num_attrs = symbol_entity_specs_get_num_gcc_attributes(entry);
    for (i = 0; i < num_attrs; i++)
    {
        if (strcmp(symbol_entity_specs_get_gcc_attributes_num(entry, i).attribute_name, name) == 0)
        {
            symbol_entity_specs_set_gcc_attributes_num(entry, i, gcc_attr);
            return;
        }
    }

    internal_error("Attribute '%s' cannot be updated\n", name);
}

nodecl_t symbol_get_aligned_attribute(scope_entry_t* entry)
{
    ERROR_CONDITION(entry == NULL, "Invalid symbol", 0);

    gcc_attribute_t gcc_attr;
    if (symbol_has_gcc_attribute(entry, "aligned", &gcc_attr))
    {
        return nodecl_list_head(gcc_attr.expression_list);
    }

    return nodecl_null();
}

void symbol_clear_indirect_types(scope_entry_t* entry)
{
    entry->_indirect_type[0] = NULL;
    entry->_indirect_type[1] = NULL;
}
