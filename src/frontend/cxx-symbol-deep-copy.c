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


#include "cxx-symbol-deep-copy.h"
#include "cxx-nodecl-deep-copy.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"

// This function is machine generated in cxx-symbol-deep-copy-entity-specs.c
extern void symbol_deep_copy_entity_specs(scope_entry_t* dest,
        scope_entry_t* source,
        const decl_context_t* new_decl_context,
        symbol_map_t* symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map);

void symbol_deep_copy_compute_maps(scope_entry_t* dest, 
        scope_entry_t* source,
        const decl_context_t* new_decl_context,
        symbol_map_t* symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map)
{
    ERROR_CONDITION(source->kind == SK_NAMESPACE, "Namespaces should not be replicated!", 0);

    // Note that context is not copied, thus this symbol should already have a
    // meaningful context for itself
    dest->kind = source->kind;

    dest->defined = source->defined;

    dest->type_information = type_deep_copy_compute_maps(
            source->type_information,
            dest,
            new_decl_context,
            symbol_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map);

    dest->related_decl_context = source->related_decl_context;

    dest->value = nodecl_deep_copy_compute_maps(source->value,
            new_decl_context,
            symbol_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map);

    dest->locus = source->locus;

    dest->do_not_print = source->do_not_print;

    symbol_deep_copy_entity_specs(dest,
            source,
            new_decl_context,
            symbol_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map);

    if (dest->kind == SK_FUNCTION)
    {
        int i;
        for (i = 0; i < symbol_entity_specs_get_num_related_symbols(dest); i++)
        {
            scope_entry_t* current_symbol = symbol_entity_specs_get_related_symbols_num(dest, i);
            symbol_set_as_parameter_of_function(current_symbol, dest,
                    symbol_get_parameter_nesting_in_function(symbol_entity_specs_get_related_symbols_num(source, i), source),
                    symbol_get_parameter_position_in_function(symbol_entity_specs_get_related_symbols_num(source, i), source));
        }
    }
}

void symbol_deep_copy(scope_entry_t* dest, 
        scope_entry_t* source,
        const decl_context_t* new_decl_context,
        symbol_map_t* symbol_map)
{
    symbol_deep_copy_compute_maps(dest, source, new_decl_context, symbol_map,
            /* nodecl_deep_copy_map_t */ NULL,
            /* symbol_deep_copy_map_t */ NULL);
}
