/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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


#include <string.h>

#include "cxx-nodecl-deep-copy.h"
#include "cxx-nodecl-output.h"
#include "cxx-scope.h"
#include "cxx-entrylist.h"
#include "cxx-utils.h"
#include "cxx-symbol-deep-copy.h"
#include "cxx-typeutils.h"

// Machine generated in cxx-nodecl-deep-copy-base.c
extern nodecl_t nodecl_deep_copy_rec(nodecl_t n, 
        const decl_context_t* new_decl_context,
        // Inherited
        symbol_map_t* inherited,
        // Synthesized
        symbol_map_t** synthesized,

        // Extra synthesized maps (may be null)
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map
        );

struct nested_symbol_map_tag
{
    symbol_map_t base_;

    symbol_map_t* enclosing_map;

    int num_mappings;
    scope_entry_t** source_list;
    scope_entry_t** target_list;
};

static decl_context_t* copy_block_scope(decl_context_t* new_decl_context, 
        const decl_context_t* orig_decl_context, 
        nested_symbol_map_t* nested_symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map);

static decl_context_t* update_function_scope(decl_context_t* new_decl_context,
        const decl_context_t* orig_decl_context,
        nested_symbol_map_t* nested_symbol_map);
static decl_context_t* copy_function_scope(decl_context_t* new_decl_context,
        const decl_context_t* orig_decl_context,
        nested_symbol_map_t* nested_symbol_map,
        nodecl_deep_copy_map_t *nodecl_deep_copy_map,
        symbol_deep_copy_map_t *symbol_deep_copy_map);

static scope_entry_t* empty_map_fun(symbol_map_t* map UNUSED_PARAMETER, scope_entry_t* entry)
{
    return entry;
}

static void empty_map_dtor(symbol_map_t* map UNUSED_PARAMETER) { }

static symbol_map_t* get_empty_map(void)
{
    static symbol_map_t* result = NULL;

    if (result == NULL)
    {
        result = NEW0(symbol_map_t);
        result->map = empty_map_fun;
        result->dtor = empty_map_dtor;
    }

    return result;
}

static scope_entry_t* nested_symbol_map_fun_immediate(symbol_map_t* symbol_map, scope_entry_t* entry)
{
    if (entry == NULL)
        return NULL;

    nested_symbol_map_t *p = (nested_symbol_map_t*)symbol_map;

    scope_entry_t* result = entry;

    int i;
    for (i = 0; i < p->num_mappings; i++)
    {
        if (p->source_list[i] == entry)
        {
             result = p->target_list[i];
             break;
        }
    }

    return result;
}

static scope_entry_t* nested_symbol_map_fun(symbol_map_t* symbol_map, scope_entry_t* entry)
{
    if (entry == NULL)
        return NULL;

    nested_symbol_map_t *p = (nested_symbol_map_t*)symbol_map;

    char found = 0;
    scope_entry_t* result = entry;

    // First ourselves
    int i;
    for (i = 0; i < p->num_mappings; i++)
    {
        if (p->source_list[i] == entry)
        {
             result = p->target_list[i];
             found = 1;
             break;
        }
    }

    // Defer to enclosing map
    if (!found)
    {
        result = p->enclosing_map->map(p->enclosing_map, entry);
    }

    return result;
}

static void nested_symbol_map_dtor(symbol_map_t* symbol_map UNUSED_PARAMETER) { }

nested_symbol_map_t* new_nested_symbol_map(symbol_map_t* enclosing_map)
{
    nested_symbol_map_t *nested_symbol_map = NEW0(nested_symbol_map_t);

    nested_symbol_map->base_.map = nested_symbol_map_fun;
    nested_symbol_map->base_.dtor = nested_symbol_map_dtor;

    nested_symbol_map->enclosing_map = enclosing_map;

    return nested_symbol_map;
}

void nested_map_add(nested_symbol_map_t* nested_symbol_map, scope_entry_t* source, scope_entry_t* target)
{
    // P_LIST_ADD modifies the second argument
    int num_mappings = nested_symbol_map->num_mappings;

    P_LIST_ADD(nested_symbol_map->source_list, num_mappings, source);
    P_LIST_ADD(nested_symbol_map->target_list, nested_symbol_map->num_mappings, target);
}

static nodecl_t nodecl_deep_copy_context_(nodecl_t n,
        decl_context_t* new_decl_context,
        symbol_map_t* enclosing_map,
        symbol_map_t** new_map,

        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map,

        char create_new_function_context
        )
{
    const decl_context_t* orig_decl_context = nodecl_get_decl_context(n);

    if (orig_decl_context->current_scope->kind != BLOCK_SCOPE)
    {
        internal_error("Attempted to perform a deep copy of a context involving a non block-scope."
                "\nThis is not supported\nContext node at '%s'\n",
                nodecl_locus_to_str(n));
    }

    nested_symbol_map_t* nested_symbol_map = new_nested_symbol_map(enclosing_map);

    // Is this block scope being copied? Check the map
    scope_t* mapped_block_scope = (scope_t*)nested_symbol_map->base_.map(&nested_symbol_map->base_,
            (scope_entry_t*)orig_decl_context->block_scope);

    if (mapped_block_scope == orig_decl_context->block_scope)
    {
        new_decl_context = copy_block_scope(new_decl_context,
                orig_decl_context,
                nested_symbol_map,
                nodecl_deep_copy_map,
                symbol_deep_copy_map);

        if (create_new_function_context)
        {
            new_decl_context->block_scope->related_entry
                = nested_symbol_map_fun((symbol_map_t*)nested_symbol_map,
                        orig_decl_context->block_scope->related_entry);
        }
    }
    else
    {
        new_decl_context->block_scope = mapped_block_scope;
        new_decl_context->current_scope = mapped_block_scope;
    }

    if (create_new_function_context)
    {
        new_decl_context = copy_function_scope(new_decl_context,
                orig_decl_context,
                nested_symbol_map,
                nodecl_deep_copy_map,
                symbol_deep_copy_map);

        new_decl_context->function_scope->related_entry
            = nested_symbol_map_fun((symbol_map_t*)nested_symbol_map,
                    orig_decl_context->function_scope->related_entry);

    }
    else
    {
        new_decl_context = update_function_scope(new_decl_context,
                orig_decl_context,
                nested_symbol_map);
    }

    nodecl_t in_context;
    in_context = nodecl_deep_copy_rec(nodecl_get_child(n, 0), new_decl_context, 
            (symbol_map_t*)nested_symbol_map, new_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map);

    nodecl_t result = nodecl_make_context(in_context,
            new_decl_context,
            nodecl_get_locus(n));

    return result;
}

nodecl_t nodecl_deep_copy_context(nodecl_t n,
        decl_context_t* new_decl_context,
        symbol_map_t* enclosing_map,
        symbol_map_t** new_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map)
{
    return nodecl_deep_copy_context_(n, new_decl_context, enclosing_map, new_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map,
            /* create_new_function_context */ 0);
}


typedef
struct closure_hash_tag
{
    const decl_context_t* new_decl_context;
    nested_symbol_map_t* nested_symbol_map;

    nodecl_deep_copy_map_t* nodecl_deep_copy_map;
    symbol_deep_copy_map_t* symbol_deep_copy_map;

    scope_entry_list_t* symbols;
} closure_hash_t;

static void create_symbols(const char* name UNUSED_PARAMETER,
        scope_entry_list_t* entry_list,
        closure_hash_t* data)
{
    scope_entry_list_iterator_t *it;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        scope_entry_t* mapped_symbol = nested_symbol_map_fun_immediate((symbol_map_t*)data->nested_symbol_map, entry);

        if (mapped_symbol == entry)
        {
            // then create a new symbol in the scope being created...
            scope_entry_t* new_entry = NEW0(scope_entry_t);
            new_entry->decl_context = data->new_decl_context;
            new_entry->symbol_name = entry->symbol_name;

            // and map the symbol to the mapped one
            nested_map_add(data->nested_symbol_map, entry, new_entry);

            // remember the mapping
            symbol_deep_copy_map_add(data->symbol_deep_copy_map, entry, new_entry);

            // Classes require extra mappings
            if (entry->kind == SK_CLASS)
            {
                new_entry->kind = SK_CLASS; // Required by new_class_context
                const decl_context_t* new_class_ctx = new_class_context(data->new_decl_context, new_entry);

                // Keep a mapping in the symbol map
                nested_map_add(data->nested_symbol_map,
                        (scope_entry_t*)class_type_get_inner_context(entry->type_information),
                        (scope_entry_t*)new_class_ctx);

                // Make sure the context of the members is the class context
                closure_hash_t* new_data = NEW(closure_hash_t);
                *new_data = *data;
                new_data->new_decl_context = new_class_ctx;

                scope_entry_list_t* member_list = class_type_get_members(entry->type_information);
                create_symbols(name,
                        member_list,
                        new_data);
                entry_list_free(member_list);

                DELETE(new_data);
            }
        }
    }
    entry_list_iterator_free(it);
}

static void register_symbols(const char* name,
        scope_entry_list_t* entry_list,
        closure_hash_t* data)
{
    scope_entry_list_iterator_t *it;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        scope_entry_t* mapped_symbol = nested_symbol_map_fun_immediate((symbol_map_t*)data->nested_symbol_map, entry);

        if (mapped_symbol != entry)
        {
            insert_alias(data->new_decl_context->current_scope, mapped_symbol, name);

            // Classes require extra mappings
            if (entry->kind == SK_CLASS)
            {
                // Make sure the context of the members is the class context
                closure_hash_t* new_data = NEW(closure_hash_t);
                *new_data = *data;

                new_data->new_decl_context = (const decl_context_t*)
                    // We need to abuse a bit of the scope map to retrieve the new class context
                    data->nested_symbol_map->base_.map(
                            &data->nested_symbol_map->base_,
                            (scope_entry_t*)class_type_get_inner_context(entry->type_information));

                scope_entry_list_t* member_list = class_type_get_members(entry->type_information);
                register_symbols(name,
                        member_list,
                        new_data);
                entry_list_free(member_list);

                DELETE(new_data);
            }
        }
    }
    entry_list_iterator_free(it);
}

static void gather_all_symbols_in_scope(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data);
static void fill_symbols(closure_hash_t* data);

static void copy_scope(const decl_context_t* new_decl_context, scope_t* original_scope,
        nested_symbol_map_t* nested_symbol_map,

        nodecl_deep_copy_map_t *nodecl_deep_copy_map,
        symbol_deep_copy_map_t *symbol_deep_copy_map)
{
    closure_hash_t closure_info;
    memset(&closure_info, 0, sizeof(closure_info));

    closure_info.new_decl_context = new_decl_context;
    closure_info.nested_symbol_map = nested_symbol_map;
    closure_info.nodecl_deep_copy_map = nodecl_deep_copy_map;
    closure_info.symbol_deep_copy_map = symbol_deep_copy_map;

    // First walk, sign in all the names but leave them empty
    dhash_ptr_walk(original_scope->dhash, (dhash_ptr_walk_fn*)create_symbols, &closure_info);
    dhash_ptr_walk(original_scope->dhash, (dhash_ptr_walk_fn*)register_symbols, &closure_info);

    // Gather all the symbols of the scope in closure.symbols
    dhash_ptr_walk(original_scope->dhash, (dhash_ptr_walk_fn*)gather_all_symbols_in_scope, &closure_info);
    // And fill them
    fill_symbols(&closure_info);

    entry_list_free(closure_info.symbols);
}

static decl_context_t* copy_function_scope(decl_context_t* new_decl_context,
        const decl_context_t* orig_decl_context,
        nested_symbol_map_t* nested_symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map)
{
    new_decl_context = new_function_context(new_decl_context);

    scope_t* old_current = new_decl_context->current_scope;

    // Keep a mapping in the symbol map
    nested_map_add(nested_symbol_map,
            (scope_entry_t*)orig_decl_context->function_scope,
            (scope_entry_t*)new_decl_context->function_scope);

    new_decl_context->current_scope = new_decl_context->function_scope;
    copy_scope(new_decl_context,
            orig_decl_context->function_scope,
            nested_symbol_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map);

    new_decl_context->current_scope = old_current;

    return new_decl_context;
}

static decl_context_t* update_function_scope(decl_context_t* new_decl_context,
        const decl_context_t* orig_decl_context,
        nested_symbol_map_t* nested_symbol_map)
{
    scope_t* function_scope = (scope_t*)nested_symbol_map->base_.map(&nested_symbol_map->base_, 
            (scope_entry_t*)orig_decl_context->function_scope);

    new_decl_context->function_scope = function_scope;

    return new_decl_context;
}

static decl_context_t* copy_block_scope(decl_context_t* new_decl_context, 
        const decl_context_t* orig_decl_context, 
        nested_symbol_map_t* nested_symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map)
{
    new_decl_context = new_block_context(new_decl_context);

    // Keep a mapping in the symbol map
    nested_map_add(nested_symbol_map,
            (scope_entry_t*)orig_decl_context->block_scope,
            (scope_entry_t*)new_decl_context->block_scope);

    copy_scope(new_decl_context,
            orig_decl_context->block_scope,
            nested_symbol_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map);

    return new_decl_context;
}

static void fill_single_symbol(
        scope_entry_t* entry,
        scope_entry_t* mapped_symbol,
        closure_hash_t* data)
{
    ERROR_CONDITION(entry == mapped_symbol, "Invalid symbol", 0);

    nodecl_deep_copy_map_t* nodecl_deep_copy_map = data->nodecl_deep_copy_map;
    symbol_deep_copy_map_t* symbol_deep_copy_map = data->symbol_deep_copy_map;

    symbol_deep_copy_compute_maps(mapped_symbol,
            entry, data->new_decl_context,
            (symbol_map_t*)data->nested_symbol_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map);
}

typedef
struct symbol_fill_info_tag
{
    scope_entry_t* symbol;
    scope_entry_t* mapped;
    closure_hash_t* data;

    scope_entry_t** depends;
    int num_depends;

} symbol_fill_info_t;

static void gather_all_symbols_to_fill(
        scope_entry_list_t* entry_list,
        closure_hash_t* data,
        int *num_symbols,
        symbol_fill_info_t** symbol_fill_info)
{
    scope_entry_list_iterator_t *it;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        scope_entry_t* mapped_symbol = nested_symbol_map_fun_immediate((symbol_map_t*)data->nested_symbol_map, entry);

        if (mapped_symbol != entry)
        {
            symbol_fill_info_t fill_info =
            {
                .symbol = entry,
                .mapped = mapped_symbol,
                .data = data,
                .num_depends = 0,
                .depends = NULL
            };

            P_LIST_ADD(*symbol_fill_info, *num_symbols, fill_info);

            if (entry->kind == SK_CLASS)
            {
                // Make sure the context of the members is the class context
                closure_hash_t* new_data = NEW(closure_hash_t);
                *new_data = *data;

                new_data->new_decl_context = (const decl_context_t*)
                    // We need to abuse a bit of the scope map to retrieve the new class context
                    data->nested_symbol_map->base_.map(
                            &data->nested_symbol_map->base_,
                            (scope_entry_t*)class_type_get_inner_context(entry->type_information));

                scope_entry_list_t* member_list = class_type_get_members(entry->type_information);
                gather_all_symbols_to_fill(member_list,
                        new_data,
                        num_symbols,
                        symbol_fill_info);
                entry_list_free(member_list);
            }
        }
    }
    entry_list_iterator_free(it);
}

static void compute_dependences_value(symbol_fill_info_t* symbol_fill_info,
        nodecl_t node,
        closure_hash_t* data)
{
    if (nodecl_is_null(node))
        return;

    if(nodecl_get_kind(node) == NODECL_SYMBOL)
    {
        scope_entry_t* entry = nodecl_get_symbol(node);

        // We shouldn't depend on ourselves
        if (symbol_fill_info->symbol == entry)
            return;

        scope_entry_t* mapped_symbol =
            nested_symbol_map_fun_immediate((symbol_map_t*)data->nested_symbol_map, entry);

        // We only add mapped symbols to the list of dependent symbols
        if (entry != mapped_symbol)
        {
            P_LIST_ADD_ONCE(symbol_fill_info->depends, symbol_fill_info->num_depends, entry);
        }
    }
    else
    {
        int i;
        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            compute_dependences_value(symbol_fill_info,
                    nodecl_get_child(node, i),
                    data);
        }
    }
}

static void compute_dependences_type(symbol_fill_info_t* symbol_fill_info,
        type_t* t,
        closure_hash_t* data)
{
    if (is_named_type(t)
            && named_type_get_symbol(t) != symbol_fill_info->symbol)
    {
        scope_entry_t* entry = named_type_get_symbol(t);
        scope_entry_t* mapped_symbol = nested_symbol_map_fun_immediate((symbol_map_t*)data->nested_symbol_map, entry);

        if (entry != mapped_symbol)
        {
            P_LIST_ADD_ONCE(symbol_fill_info->depends, symbol_fill_info->num_depends, entry);
        }
    }
    else if (is_pointer_type(t))
    {
        compute_dependences_type(symbol_fill_info,
                pointer_type_get_pointee_type(t),
                data);
    }
    else if (is_pointer_to_member_type(t))
    {
        compute_dependences_type(symbol_fill_info,
                pointer_type_get_pointee_type(t),
                data);

        compute_dependences_type(symbol_fill_info,
                pointer_to_member_type_get_class_type(t),
                data);
    }
    else if (is_any_reference_type(t))
    {
        compute_dependences_type(symbol_fill_info,
                no_ref(t),
                data);
    }
    else if (is_array_type(t))
    {
        compute_dependences_type(
                symbol_fill_info,
                array_type_get_element_type(t),
                data);
    }
    else if (is_function_type(t))
    {
        compute_dependences_type(
                symbol_fill_info,
                function_type_get_return_type(t),
                data);

        if (!function_type_get_lacking_prototype(t))
        {
            int i, N = function_type_get_num_parameters(t), P = N;

            if (function_type_get_has_ellipsis(t))
            {
                P = N - 1;
            }

            for (i = 0; i < P; i++)
            {
                compute_dependences_type(
                        symbol_fill_info,
                        function_type_get_parameter_type_num(t, i),
                        data);
            }
        }
    }
    else if (is_vector_type(t))
    {
        compute_dependences_type(
                symbol_fill_info,
                vector_type_get_element_type(t),
                data);
    }
}

static void compute_dependences(symbol_fill_info_t* symbol_fill_info,
        closure_hash_t* data)
{
    compute_dependences_type(symbol_fill_info,
            symbol_fill_info->symbol->type_information,
            data);

    if (symbol_fill_info->symbol->kind == SK_VARIABLE)
    {
        compute_dependences_value(symbol_fill_info,
                symbol_fill_info->symbol->value,
                data);
    }
}

static void remove_symbol_from_deps(
        symbol_fill_info_t* symbol_fill_info,
        scope_entry_t* symbol)
{
    P_LIST_REMOVE(symbol_fill_info->depends, symbol_fill_info->num_depends, symbol);
}

static void gather_all_symbols_in_scope(const char* name UNUSED_PARAMETER,
        scope_entry_list_t* entry_list,
        closure_hash_t* data)
{
    data->symbols = entry_list_concat(data->symbols, entry_list);
}

static void fill_symbols(closure_hash_t* data)
{
    // Gather first all symbols including indirect ones of SK_CLASSes
    int num_symbols = 0;
    symbol_fill_info_t* symbol_fill_info = 0;

    gather_all_symbols_to_fill(data->symbols, data, &num_symbols, &symbol_fill_info);

    // We have to fill symbols in topological order, otherwise we may have
    // incomplete information when filling them (in particular TYPEDEFs are
    // very sensitive to this)

    // First for each symbol traverse its type in case it depends on another mapped symbol
    int i;
    for (i = 0; i < num_symbols; i++)
    {
        compute_dependences(&symbol_fill_info[i], data);
    }

    // Now define symbols as long as they have no dependences
    // (this implements the topological order)
    int remaining_symbols;
    for(remaining_symbols = num_symbols;
            remaining_symbols > 0;
            )
    {
        int old_remaining_symbols = remaining_symbols;

        for (i = 0; i < num_symbols; i++)
        {
            if (symbol_fill_info[i].symbol != NULL // not yet processed
                    && symbol_fill_info[i].num_depends == 0)
            {
                fill_single_symbol(
                        symbol_fill_info[i].symbol,
                        symbol_fill_info[i].mapped,
                        symbol_fill_info[i].data);

                // Now remove this symbol from other symbols
                // This is the inefficient part of this implementation
                int j;
                for (j = 0; j < num_symbols; j++)
                {
                    if (i == j)
                        continue;

                    remove_symbol_from_deps(
                            &symbol_fill_info[j],
                            symbol_fill_info[i].symbol);
                }

                symbol_fill_info[i].symbol = NULL; // already processed
                remaining_symbols--;
            }
        }

        // Sanity check that we are progressing
        ERROR_CONDITION(remaining_symbols == old_remaining_symbols,
                "No symbol filled", 0);
    }

    // Cleanup extra closures that may have been created during gather_all_symbols_to_fill
    closure_hash_t** extra_closures = NULL;
    int num_extra_closures = 0;
    for (i = 0; i < num_symbols; i++)
    {
        if (symbol_fill_info[i].data != data) // 'data' will be freed by the caller
        {
            P_LIST_ADD_ONCE(extra_closures, num_extra_closures, symbol_fill_info[i].data);
        }
    }
    for (i = 0; i < num_extra_closures; i++)
    {
        DELETE(extra_closures[i]);
    }
    DELETE(extra_closures);
}

nodecl_t nodecl_deep_copy_function_code(nodecl_t n,
        decl_context_t* new_decl_context,
        symbol_map_t* symbol_map UNUSED_PARAMETER,
        symbol_map_t** synth_symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map
        )
{
    scope_entry_t* orig_symbol = nodecl_get_symbol(n);
    scope_entry_t* symbol = (*synth_symbol_map)->map(*synth_symbol_map, orig_symbol);

    // Skip unmapped member functions
    if (symbol == orig_symbol
            && symbol_entity_specs_get_is_member(orig_symbol))
        return nodecl_null();

    ERROR_CONDITION( (symbol == orig_symbol), "When copying a NODECL_FUNCTION_CODE, the function symbol must always be mapped, "
            "otherwise there would be two function code trees for the same symbol", 0);

    nodecl_t child_0 = nodecl_deep_copy_context_(
            nodecl_get_child(n, 0),
            new_decl_context,
            (*synth_symbol_map),
            synth_symbol_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map,
            /* create_new_function_context */ 1);

    nodecl_t child_1 = nodecl_deep_copy_rec(nodecl_get_child(n, 1),
            new_decl_context,
            (*synth_symbol_map),
            synth_symbol_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map);
    const locus_t* location = nodecl_get_locus(n);
    nodecl_t result = nodecl_make_function_code(child_0, child_1, symbol, location);

    symbol_deep_copy_compute_maps(symbol,
            orig_symbol,
            symbol->decl_context,
            (*synth_symbol_map),
            nodecl_deep_copy_map,
            symbol_deep_copy_map);

    symbol_entity_specs_set_function_code(symbol, result);
    symbol->related_decl_context = nodecl_get_decl_context(nodecl_get_child(result, 0));
    return result;
}

nodecl_t nodecl_deep_copy_compute_maps(nodecl_t n,
        const decl_context_t* new_decl_context,
        symbol_map_t* symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map)
{
    if (symbol_map == NULL)
        symbol_map = get_empty_map();

    symbol_map_t *synth_map = NULL;

    nodecl_t result = nodecl_deep_copy_rec(n, new_decl_context,
            symbol_map,
            &synth_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map);

    return result;
}

nodecl_t nodecl_deep_copy(nodecl_t n, const decl_context_t* new_decl_context, symbol_map_t* symbol_map)
{
    return nodecl_deep_copy_compute_maps(n,
            new_decl_context,
            symbol_map,
            /* nodecl_deep_copy_map */ NULL,
            /* symbol_deep_copy_map */ NULL);
}


// nodecl_deep_copy_map_t
// symbol_deep_copy_map_t

struct nodecl_deep_copy_map_tag
{
    int num_mappings;
    nodecl_t *orig;
    nodecl_t *copied;
};

struct symbol_deep_copy_map_tag
{
    int num_mappings;
    scope_entry_t **orig;
    scope_entry_t **copied;
};

nodecl_deep_copy_map_t* nodecl_deep_copy_map_new(void)
{
    nodecl_deep_copy_map_t* nodecl_deep_copy_map = NEW0(nodecl_deep_copy_map_t);
    return nodecl_deep_copy_map;
}

symbol_deep_copy_map_t* symbol_deep_copy_map_new(void)
{
    symbol_deep_copy_map_t* symbol_deep_copy_map = NEW0(symbol_deep_copy_map_t);
    return symbol_deep_copy_map;
}

void nodecl_deep_copy_map_free(nodecl_deep_copy_map_t* nodecl_deep_copy_map)
{
    if (nodecl_deep_copy_map == NULL)
        return;

    DELETE(nodecl_deep_copy_map->orig);
    DELETE(nodecl_deep_copy_map->copied);

    DELETE(nodecl_deep_copy_map);
}

void symbol_deep_copy_map_free(symbol_deep_copy_map_t* symbol_deep_copy_map)
{
    if (symbol_deep_copy_map == NULL)
        return;

    DELETE(symbol_deep_copy_map->orig);
    DELETE(symbol_deep_copy_map->copied);

    DELETE(symbol_deep_copy_map);
}

void nodecl_deep_copy_map_traverse(
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        void* info,
        void (*fn)(nodecl_t orig, nodecl_t copy, void* info))
{
    if (nodecl_deep_copy_map == NULL)
        return;

    int i;
    for (i = 0; i < nodecl_deep_copy_map->num_mappings; i++)
    {
        fn(nodecl_deep_copy_map->orig[i], nodecl_deep_copy_map->copied[i], info);
    }
}

void symbol_deep_copy_map_traverse(
        symbol_deep_copy_map_t* symbol_deep_copy_map,
        void* info,
        void (*fn)(scope_entry_t* orig, scope_entry_t* copy, void* info))
{
    if (symbol_deep_copy_map == NULL)
        return;

    int i;
    for (i = 0; i < symbol_deep_copy_map->num_mappings; i++)
    {
        fn(symbol_deep_copy_map->orig[i], symbol_deep_copy_map->copied[i], info);
    }
}

/* Used in cxx-nodecl-deep-copy-base.c */
extern void nodecl_deep_copy_map_add(nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        nodecl_t orig,
        nodecl_t copied)
{
    if (nodecl_deep_copy_map == NULL)
        return;

    int num_mappings = nodecl_deep_copy_map->num_mappings;
    P_LIST_ADD(nodecl_deep_copy_map->orig, num_mappings, orig);
    P_LIST_ADD(nodecl_deep_copy_map->copied, nodecl_deep_copy_map->num_mappings, copied);
}

/* Used in cxx-typeutils.c */
extern void symbol_deep_copy_map_add(symbol_deep_copy_map_t* symbol_deep_copy_map,
        scope_entry_t *orig,
        scope_entry_t *copied)
{
    if (symbol_deep_copy_map == NULL)
        return;

    int num_mappings = symbol_deep_copy_map->num_mappings;
    P_LIST_ADD(symbol_deep_copy_map->orig, num_mappings, orig);
    P_LIST_ADD(symbol_deep_copy_map->copied, symbol_deep_copy_map->num_mappings, copied);
}
