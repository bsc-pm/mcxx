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

#include "fortran03-scope.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"
#include "cxx-entrylist.h"
#include "cxx-diagnostic.h"
#include "cxx-ambiguity.h"
#include "fortran03-buildscope.h"
#include "fortran03-typeutils.h"
#include "fortran03-intrinsics.h"
#include <string.h>
#include <ctype.h>

long long unsigned int _bytes_fortran_scope = 0;

typedef struct type_tag* implicit_letter_set_t['z' - 'a' + 1];

typedef
struct implicit_info_data_tag
{
    char letter_set_is_shared;
    implicit_letter_set_t* implicit_letter_set;
} implicit_info_data_t;

struct implicit_info_tag
{
    implicit_info_data_t* data;
};

static implicit_letter_set_t* allocate_implicit_letter_set(void)
{
    implicit_letter_set_t* result = NEW0(implicit_letter_set_t);

    return result;
}

static implicit_info_data_t* allocate_implicit_info_data(void)
{
    implicit_info_data_t* result = NEW0(implicit_info_data_t);

    result->implicit_letter_set = allocate_implicit_letter_set();

    return result;
}
    
static implicit_info_t* allocate_implicit_info(void)
{
    implicit_info_t* result = NEW0(implicit_info_t);

    result->data = allocate_implicit_info_data();
    result->data->implicit_letter_set = allocate_implicit_letter_set();

    return result;
}

static implicit_info_t* allocate_implicit_info_sharing_set(implicit_info_t* implicit_letter_set)
{
    implicit_info_t* result = NEW0(implicit_info_t);

    result->data = allocate_implicit_info_data();
    result->data->implicit_letter_set = implicit_letter_set->data->implicit_letter_set;
    result->data->letter_set_is_shared = 1;

    return result;
}

static implicit_info_t* get_default_fortran_implicit(void)
{
    static implicit_info_t* result;
    if (result == NULL)
    {
        result = allocate_implicit_info();

        char c;
        for (c = 'a'; c <= 'z'; c++)
        {
            (*(result->data->implicit_letter_set))[c - 'a'] = fortran_get_default_real_type();
        }
        for (c = 'i'; c <= 'n'; c++)
        {
            (*(result->data->implicit_letter_set))[c - 'a'] = fortran_get_default_integer_type();
        }
        result->data->letter_set_is_shared = 1;
    }

    return result;
}

static void copy_on_write_implicit(const decl_context_t* decl_context)
{
    if (decl_context->implicit_info->data->letter_set_is_shared)
    {
        implicit_info_data_t* old_implicit_info = decl_context->implicit_info->data;

        decl_context->implicit_info->data = allocate_implicit_info_data();

        if (old_implicit_info->implicit_letter_set != NULL)
        {
            memcpy(decl_context->implicit_info->data->implicit_letter_set,
                    old_implicit_info->implicit_letter_set,
                    sizeof (*old_implicit_info->implicit_letter_set));
        }
    }
}

void set_implicit_info(const decl_context_t* decl_context, char from_letter, char to_letter, type_t* type)
{
    from_letter = tolower(from_letter);
    to_letter = tolower(to_letter);

    copy_on_write_implicit(decl_context);

    char letter = from_letter;
    while (letter <= to_letter)
    {
        ERROR_CONDITION(!('a' <= letter
                    && letter <= 'z'), "Invalid letter %c", letter);
        (*(decl_context->implicit_info->data->implicit_letter_set))[letter - 'a'] = type;

        letter++;
    }
}

void set_implicit_none(const decl_context_t* decl_context)
{
    copy_on_write_implicit(decl_context);

    decl_context->implicit_info->data->implicit_letter_set = NULL;
}

char is_implicit_none(const decl_context_t* decl_context)
{
    return decl_context->implicit_info->data->implicit_letter_set == NULL;
}

char implicit_has_been_set(const decl_context_t* decl_context)
{
    return !decl_context->implicit_info->data->letter_set_is_shared;
}

const decl_context_t* new_program_unit_context(const decl_context_t* decl_context)
{
    decl_context_t* result = new_block_context(decl_context);
    result = new_function_context(result);
    result->implicit_info = allocate_implicit_info_sharing_set(get_default_fortran_implicit());
    result->current_scope->related_entry = NULL;

    return result;
}

const decl_context_t* new_internal_program_unit_context(const decl_context_t* decl_context)
{
    decl_context_t* result = new_block_context(decl_context);
    result = new_function_context(result);
    result->implicit_info = allocate_implicit_info_sharing_set(decl_context->implicit_info);
    result->current_scope->related_entry = NULL;

    return result;
}

static scope_entry_t* new_implicit_symbol(const decl_context_t* decl_context, AST location, const char* name)
{
    char first_letter = tolower(name[0]);
    // Special names for operators and other non regularly named stuff will not get here
    if (('a' <= first_letter)
            && (first_letter <= 'z'))
    {
        type_t* implicit_type = 
            (*(decl_context->implicit_info->data->implicit_letter_set))[first_letter - 'a'];

        ERROR_CONDITION(implicit_type == NULL, "this type can not be NULL", 0);

        //The implicits symbols will be stored in the current scope of the program unit
        const decl_context_t* program_unit_context = decl_context->current_scope->related_entry->related_decl_context;
        scope_entry_t* sym = new_symbol(program_unit_context, program_unit_context->current_scope, strtolower(name));
        sym->kind = SK_UNDEFINED;
        sym->type_information = implicit_type;
        symbol_entity_specs_set_is_implicit_basic_type(sym, 1);
        
        if (location != NULL)
        {
            sym->locus = ast_get_locus(location);
        }

        return sym;
    }

    return NULL;
}

type_t* get_implicit_type_for_symbol(const decl_context_t* decl_context, const char* name)
{
    type_t* implicit_type = NULL;
    char first_letter = tolower(name[0]);

    if (decl_context->implicit_info != NULL
            && decl_context->implicit_info->data != NULL
            && decl_context->implicit_info->data->implicit_letter_set != NULL
            && ('a' <= first_letter)
            && (first_letter <= 'z'))
    {
        implicit_type = 
            (*(decl_context->implicit_info->data->implicit_letter_set))[first_letter - 'a'];
    }

    // This is a special void that can be distinguished from plain void
    if (implicit_type == NULL)
        implicit_type = get_implicit_none_type();

    return implicit_type;
}

scope_entry_t* fortran_get_variable_with_locus(const decl_context_t* decl_context, AST location, const char* name)
{
    ERROR_CONDITION(location == NULL, "Locus is needed", 0);

    scope_entry_t* result = fortran_query_name_str(decl_context, name, ast_get_locus(location));

    if (result == NULL)
    {
        if (decl_context->implicit_info != NULL
                && decl_context->implicit_info->data != NULL
                && decl_context->implicit_info->data->implicit_letter_set != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCOPE: Getting implicit entity for name '%s'\n", name);
            }
            result = new_implicit_symbol(decl_context, location, name);
            if(result != NULL)
            {
                result->kind = SK_VARIABLE;
                remove_unknown_kind_symbol(decl_context, result);
            }
        }
        DEBUG_CODE()
        {
            if (result == NULL)
            {
                fprintf(stderr, "SCOPE: There is no implicit name for entity '%s'\n", name);
            }
        }
    }

    return result;
}

const decl_context_t* fortran_new_block_context(const decl_context_t* decl_context)
{
    const decl_context_t* result = new_block_context(decl_context);
    return result;
}

scope_entry_t* new_fortran_implicit_symbol(const decl_context_t* decl_context, AST location, const char* name)
{
    scope_entry_t* new_entry = new_implicit_symbol(decl_context, location, name);
    add_unknown_kind_symbol(decl_context, new_entry);
    return new_entry;
}

scope_entry_t* new_fortran_symbol_not_unknown(const decl_context_t* decl_context, const char* name)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPE: Creating new symbol '%s' in scope '%p'\n", 
                strtolower(name),
                decl_context->current_scope);
    }

    scope_entry_t * new_entry = new_symbol(decl_context, decl_context->current_scope, strtolower(name));
    return new_entry;
}

scope_entry_t* new_fortran_symbol(const decl_context_t* decl_context, const char* name)
{
    scope_entry_t* new_entry = new_fortran_symbol_not_unknown(decl_context, name);

    add_unknown_kind_symbol(decl_context, new_entry);
    return new_entry;
}

scope_entry_t* query_name_in_class(const decl_context_t* class_context, const char* name,
        const locus_t* locus)
{
    scope_entry_t* entry = NULL;
    scope_entry_list_t* entry_list = class_context_lookup(class_context, NULL, DF_NONE, strtolower(name), locus);
    if (entry_list != NULL)
    {
        entry = entry_list_head(entry_list);
    }
    entry_list_free(entry_list);

    return entry;
}

scope_entry_t* fortran_get_ultimate_symbol(scope_entry_t* entry)
{
    while (entry != NULL
            && symbol_entity_specs_get_from_module(entry) != NULL)
    {
        scope_entry_t* aliased = symbol_entity_specs_get_alias_to(entry);
        DEBUG_CODE()
        {
            fprintf(stderr, "ADVANCING '%s.%s' TO '%s.%s'\n",
                    symbol_entity_specs_get_from_module(entry)->symbol_name,
                    entry->symbol_name,
                    (symbol_entity_specs_get_from_module(aliased) != NULL
                     ? symbol_entity_specs_get_from_module(aliased)->symbol_name
                     : (symbol_entity_specs_get_in_module(aliased) != NULL
                         ?  symbol_entity_specs_get_in_module(aliased)->symbol_name 
                         : "<<UNKNOWN-MODULE>>")),
                    aliased->symbol_name
                   );
        }
        entry = aliased;
    }

    ERROR_CONDITION(entry == NULL, "Invalid symbol", 0);

    return entry;
}

static char mean_the_same_entity(scope_entry_list_t* entry_list)
{
    scope_entry_t* entry = NULL;
    scope_entry_list_iterator_t* it = NULL;
    char result = 1;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it) && result;
            entry_list_iterator_next(it))
    {
        scope_entry_t* current = entry_list_iterator_current(it);
        if (entry == NULL)
        {
            entry = current;
        }
        else
        {
            result = (fortran_get_ultimate_symbol(entry) == fortran_get_ultimate_symbol(current));
        }
    }
    entry_list_iterator_free(it);

    return result;
}

static void diagnostic_ambiguity(scope_entry_list_t* entry_list)
{
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        info_printf_at(entry->locus, "name '%s' first bound here\n", entry->symbol_name);
    }
    entry_list_iterator_free(it);
}

scope_entry_t* fortran_query_name_str(const decl_context_t* decl_context, 
        const char* unqualified_name,
        const locus_t* locus)
{
    scope_entry_t* result = NULL;
    decl_context_t* current_decl_context = decl_context_clone(decl_context);
    scope_t* current_scope = decl_context->current_scope;

    while (result == NULL 
            && current_scope != NULL)
    {
        current_decl_context->current_scope = current_scope;
        scope_entry_list_t* result_list = query_in_scope_str(current_decl_context, strtolower(unqualified_name), NULL);    
        if (result_list != NULL)
        {
            if (entry_list_size(result_list) > 1
                    && !mean_the_same_entity(result_list))
            {
                error_printf_at(locus, "name '%s' is ambiguous\n", unqualified_name);
                diagnostic_ambiguity(result_list);
            }

            result = entry_list_head(result_list);
            entry_list_free(result_list);

            // Some symbols in the global scope must be ignored
            if (decl_context->global_scope == current_scope
                    && symbol_entity_specs_get_is_global_hidden(result))
            {
                result = NULL;
            }
        }

        current_scope = current_scope->contained_in;
    }
    
    return result;
}

static char symbol_is_intrinsic_function(scope_entry_t* sym, void* data UNUSED_PARAMETER)
{
    return sym != NULL
        && sym->kind == SK_FUNCTION
        && symbol_entity_specs_get_is_builtin(sym)
        // Generic ones
        && (symbol_entity_specs_get_emission_template(sym) == NULL
                // Or specific intrinsics whose name is not the same as their generic
                || (symbol_entity_specs_get_emission_template(sym) != NULL
                    && strcasecmp(symbol_entity_specs_get_emission_template(sym)->symbol_name, sym->symbol_name) != 0));
}

static char symbol_is_intrinsic_function_not_from_module(scope_entry_t* sym, void* data UNUSED_PARAMETER)
{
    return symbol_is_intrinsic_function(sym, data)
        && symbol_entity_specs_get_from_module(sym) == NULL;
}

static char symbol_is_generic_intrinsic_function(scope_entry_t* sym, void* data UNUSED_PARAMETER)
{
    return symbol_is_intrinsic_function(sym, data)
        && symbol_entity_specs_get_emission_template(sym) == NULL
        && is_computed_function_type(sym->type_information);
}


static char symbol_is_generic_intrinsic_function_not_from_module(scope_entry_t* sym, void* data UNUSED_PARAMETER)
{
    return symbol_is_generic_intrinsic_function(sym, data)
        && symbol_entity_specs_get_from_module(sym) == NULL;
}

scope_entry_t* fortran_query_intrinsic_name_str(const decl_context_t* decl_context, const char* unqualified_name)
{
    const decl_context_t* global_context = fortran_get_context_of_intrinsics(decl_context);

    scope_entry_list_t* global_list = query_in_scope_str(global_context, strtolower(unqualified_name), NULL);

    scope_entry_list_t* result_list = filter_symbol_using_predicate(global_list,
            symbol_is_intrinsic_function_not_from_module, NULL);
    entry_list_free(global_list);

    scope_entry_t* result = NULL;
    if (result_list != NULL)
    {
        result = entry_list_head(result_list);
    }

    return result;
}

#if 0
static char all_names_are_generic_specifiers(scope_entry_list_t* entry_list)
{
    if (entry_list == 0)
        return 0;

    char result = 1;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it) && result;
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        result = entry->kind == SK_GENERIC_NAME;
    }
    entry_list_iterator_free(it);

    return result;
}
#endif

static char symbol_is_generic_specifier(scope_entry_t* sym, void* data UNUSED_PARAMETER)
{
    return sym != NULL
        && sym->kind == SK_GENERIC_NAME;
}

static char symbol_is_generic_specifier_or_generic_intrinsic(scope_entry_t* sym, void* data UNUSED_PARAMETER)
{
    sym = fortran_get_ultimate_symbol(sym);

    return symbol_is_generic_specifier(sym, data)
        || symbol_is_generic_intrinsic_function(sym, data);
}

static char a_name_is_established_generic(scope_entry_list_t* entry_list)
{
    if (entry_list == 0)
        return 0;

    char result = 1;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it) && result;
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        if (symbol_is_generic_specifier_or_generic_intrinsic(entry, NULL))
        {
            entry_list_iterator_free(it);
            return 1;
        }
    }
    entry_list_iterator_free(it);

    return 0;
}

static char no_name_has_been_established_generic(scope_entry_list_t* entry_list)
{
    return !a_name_is_established_generic(entry_list);
}

// This routine performs a usual Fortran lookup unless it finds a generic
// specifier, when found it will continue the lookup upwards the lexical scope
// since there may be more than one generic specifier in a given scope or in
// the enclosing one
//
// See ticket #923
scope_entry_list_t* fortran_query_name_str_for_function(const decl_context_t* decl_context,
        const char* unqualified_name,
        const locus_t* locus)
{
    scope_entry_list_t* result_list = NULL;
    decl_context_t* current_decl_context = decl_context_clone(decl_context);
    scope_t* current_scope = decl_context->current_scope;

    char keep_trying = 1;

    computed_function_type_t* intrinsics_seen = NULL;
    int num_intrinsics_seen = 0;

    while (keep_trying
            && current_scope != NULL)
    {
        current_decl_context->current_scope = current_scope;
        scope_entry_list_t* entry_list = query_in_scope_str(current_decl_context, strtolower(unqualified_name), NULL);
        if (entry_list != NULL)
        {
            // If we find more than one name but not all are generic specifiers
            // this is an ambiguity case
            if (entry_list_size(entry_list) > 1)
            {
                if (a_name_is_established_generic(entry_list))
                {
                    // Get all the generic specifiers and generic interfaces only
                    scope_entry_list_t* generic_specifiers = filter_symbol_using_predicate(entry_list,
                            symbol_is_generic_specifier_or_generic_intrinsic, NULL);
                    entry_list_free(entry_list);
                    entry_list = generic_specifiers;
                }
                else
                {
                    if (!mean_the_same_entity(entry_list))
                    {
                        error_printf_at(locus, "name '%s' is ambiguous\n", unqualified_name);
                        diagnostic_ambiguity(entry_list);
                    }

                    // Use the first entry found
                    result_list = entry_list_new(entry_list_head(entry_list));
                    entry_list_free(entry_list);
                    DELETE(intrinsics_seen);
                    return result_list;
                }
            }

            // If no name is not a generic specifier do not continue
            // the lookup
            keep_trying = !no_name_has_been_established_generic(entry_list);

            // Add all the symbols found.
            // First the generic specifiers (if any)
            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(entry_list);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* current = entry_list_iterator_current(it);
                if (!(decl_context->global_scope == current_scope
                            && symbol_entity_specs_get_is_global_hidden(current))
                        && symbol_is_generic_specifier(current, NULL))
                {
                    result_list = entry_list_add_once(result_list, current);
                }
            }
            entry_list_iterator_free(it);

            // Second the intrinsics the generic intrinsics (if any)
            for (it = entry_list_iterator_begin(entry_list);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* current = entry_list_iterator_current(it);
                if (!(decl_context->global_scope == current_scope
                            && symbol_entity_specs_get_is_global_hidden(current))
                        && symbol_is_generic_intrinsic_function(current, NULL))
                {
                    computed_function_type_t fun
                        = computed_function_type_get_computing_function(current->type_information);

                    int i; char found = 0;
                    for (i = 0; i < num_intrinsics_seen && !found; i++)
                    {
                        found = (intrinsics_seen[i] == fun);
                    }

                    // Do not add repeated intrinsics
                    if (!found)
                    {
                        P_LIST_ADD(intrinsics_seen, num_intrinsics_seen, fun);
                        result_list = entry_list_add_once(result_list, current);
                    }
                }
            }
            entry_list_iterator_free(it);

            // Finally all the remaining symbols
            for (it = entry_list_iterator_begin(entry_list);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* current = entry_list_iterator_current(it);
                if (!(decl_context->global_scope == current_scope
                            && symbol_entity_specs_get_is_global_hidden(current))
                        && !symbol_is_generic_specifier(current, NULL)
                        && !symbol_is_generic_intrinsic_function(current, NULL))
                {
                    result_list = entry_list_add_once(result_list, current);
                }
            }
            entry_list_iterator_free(it);

            entry_list_free(entry_list);
        }

        current_scope = current_scope->contained_in;
    }

    if (a_name_is_established_generic(result_list))
    {
        scope_entry_t* intrinsic = fortran_query_intrinsic_name_str(decl_context, unqualified_name);
        if (intrinsic != NULL
                && symbol_is_generic_intrinsic_function_not_from_module(intrinsic, NULL))
        {
            computed_function_type_t fun
                = computed_function_type_get_computing_function(intrinsic->type_information);

            int i; char found = 0;
            for (i = 0 ; i < num_intrinsics_seen && !found; i++)
            {
                found = (intrinsics_seen[i] == fun);
            }

            // Do not add repeated intrinsics
            if (!found)
            {
                result_list = entry_list_add_once(result_list, intrinsic);
                P_LIST_ADD(intrinsics_seen, num_intrinsics_seen, fun);
            }
        }
    }

    DELETE(intrinsics_seen);
    return result_list;
}

scope_entry_list_t* fortran_query_module_for_name(scope_entry_t* module_symbol, const char* name)
{
    ERROR_CONDITION(module_symbol == NULL
            || module_symbol->kind != SK_MODULE, "Invalid symbol", 0);
    ERROR_CONDITION(name == NULL, "Invalid name", 0);

    scope_entry_list_t* result = NULL;
    int i;
    for (i = 0; i < symbol_entity_specs_get_num_related_symbols(module_symbol); i++)
    {
        scope_entry_t* sym = symbol_entity_specs_get_related_symbols_num(module_symbol, i);

        if (strcasecmp(sym->symbol_name, name) == 0
                // Filter private symbols
                && symbol_entity_specs_get_access(sym) != AS_PRIVATE)
        {
            result = entry_list_add_once(result, sym);
        }
    }

    return result;
}
