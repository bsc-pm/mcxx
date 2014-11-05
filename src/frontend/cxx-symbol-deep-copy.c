#include "cxx-symbol-deep-copy.h"
#include "cxx-nodecl-deep-copy.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"

// This function is machine generated in cxx-symbol-deep-copy-entity-specs.c
extern void symbol_deep_copy_entity_specs(scope_entry_t* dest,
        scope_entry_t* source,
        decl_context_t new_decl_context,
        symbol_map_t* symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map);

void symbol_deep_copy_compute_maps(scope_entry_t* dest, 
        scope_entry_t* source,
        decl_context_t new_decl_context,
        symbol_map_t* symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map)
{
    ERROR_CONDITION(source->kind == SK_CLASS
            && symbol_entity_specs_get_from_module(source) == NULL,
            "Local class replication not implemented yet", 0);
    ERROR_CONDITION(source->kind == SK_NAMESPACE, "Namespaces should not be replicated!", 0);

    // Note that context is not copied, thus this symbol should already have a
    // meaningful context for itself
    dest->kind = source->kind;

    dest->defined = source->defined;

    dest->type_information = type_deep_copy_compute_maps(
            source->type_information,
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

    if (dest->kind == SK_FUNCTION
            // Generic specifiers are tagged as SK_FUNCTION
            && !symbol_entity_specs_get_is_generic_spec(dest))
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

    // FIXME - Not copying extended data. There is no way to reliably copy such information
}

void symbol_deep_copy(scope_entry_t* dest, 
        scope_entry_t* source,
        decl_context_t new_decl_context,
        symbol_map_t* symbol_map)
{
    symbol_deep_copy_compute_maps(dest, source, new_decl_context, symbol_map,
            /* nodecl_deep_copy_map_t */ NULL,
            /* symbol_deep_copy_map_t */ NULL);
}
