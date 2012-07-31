#include "cxx-nodecl-deep-copy.h"
#include "cxx-nodecl-output.h"
#include "cxx-scope.h"
#include "cxx-entrylist.h"
#include "cxx-utils.h"
#include "cxx-symbol-deep-copy.h"
#include <string.h>

// Machine generated in cxx-nodecl-deep-copy-base.c
extern nodecl_t nodecl_deep_copy_rec(nodecl_t n, 
        decl_context_t new_decl_context,
        // Inherited
        symbol_map_t* inherited,
        // Synthesized
        symbol_map_t** synthesized);

typedef
struct nested_symbol_map_tag
{
    symbol_map_t base_;

    symbol_map_t* enclosing_map;

    int num_mappings;
    scope_entry_t** source_list;
    scope_entry_t** target_list;
} nested_symbol_map_t;

static void copy_block_scope(decl_context_t new_block_context_, scope_t* block_scope, nested_symbol_map_t *nested_symbol_map);

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
        result = calloc(1, sizeof(*result));
        result->map = empty_map_fun;
        result->dtor = empty_map_dtor;
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

static nested_symbol_map_t* new_nested_symbol_map(symbol_map_t* enclosing_map)
{
    nested_symbol_map_t *nested_symbol_map = calloc(1, sizeof(*nested_symbol_map));

    nested_symbol_map->base_.map = nested_symbol_map_fun;
    nested_symbol_map->base_.dtor = nested_symbol_map_dtor;

    nested_symbol_map->enclosing_map = enclosing_map;

    return nested_symbol_map;
}

static void nested_map_add(nested_symbol_map_t* nested_symbol_map, scope_entry_t* source, scope_entry_t* target)
{
    // P_LIST_ADD modifies the second argument
    int num_mappings = nested_symbol_map->num_mappings;

    P_LIST_ADD(nested_symbol_map->source_list, num_mappings, source);
    P_LIST_ADD(nested_symbol_map->target_list, nested_symbol_map->num_mappings, target);
}

nodecl_t nodecl_deep_copy_context(nodecl_t n,
        decl_context_t new_decl_context,
        symbol_map_t* enclosing_map,
        symbol_map_t** new_map)
{
    decl_context_t orig_decl_context = nodecl_get_decl_context(n);

    if (orig_decl_context.current_scope->kind != BLOCK_SCOPE)
    {
        internal_error("Attempted to perform a deep copy of a context involving a non block-scope."
                "\nThis is not supported\nContext node at '%s'\n",
                nodecl_get_locus(n));
    }

    decl_context_t new_block_context_ = new_block_context(new_decl_context);

    nested_symbol_map_t* nested_symbol_map = new_nested_symbol_map(enclosing_map);

    copy_block_scope(new_block_context_, orig_decl_context.block_scope, nested_symbol_map);

    nodecl_t in_context;
    in_context = nodecl_deep_copy_rec(nodecl_get_child(n, 0), new_block_context_, 
            (symbol_map_t*)nested_symbol_map, new_map);

    nodecl_t result = nodecl_make_context(in_context,
            new_block_context_,
            nodecl_get_filename(n),
            nodecl_get_line(n));

    return result;
}


typedef
struct closure_hash_tag
{
    decl_context_t new_block_context_;
    scope_t* original_block_scope;
    nested_symbol_map_t* nested_symbol_map;

    int num_filled;
    scope_entry_t** filled_symbols;
} closure_hash_t;

static void register_symbols_generic(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data,
        char (*filter)(scope_entry_t* entry))
{
    scope_entry_list_iterator_t *it;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (!filter(entry))
            continue;

        scope_entry_t* mapped_symbol = nested_symbol_map_fun((symbol_map_t*)data->nested_symbol_map, entry);

        if (mapped_symbol == entry)
        {
            // There was no map, create it now
            scope_entry_t* new_entry = new_symbol(data->new_block_context_, data->new_block_context_.current_scope, name);

            nested_map_add(data->nested_symbol_map, entry, new_entry);

            mapped_symbol = new_entry;
        }
        else
        {
            insert_alias(data->new_block_context_.current_scope, mapped_symbol, name);
        }
    }
    entry_list_iterator_free(it);
}

static char any_symbols(scope_entry_t* entry UNUSED_PARAMETER)
{
    return 1;
}

static char symbols_of_fortran_program_unit(scope_entry_t* entry)
{
    return (entry->kind == SK_FUNCTION
            && !entry->entity_specs.is_nested_function);
}

static void register_symbols(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data)
{
    register_symbols_generic(name, entry_list, data, any_symbols);
}

static void register_symbols_of_fortran_program_unit(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data)
{
    register_symbols_generic(name, entry_list, data, symbols_of_fortran_program_unit);
}

static void fill_symbols(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data);
static void fill_symbols_of_fortran_program_unit(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data);

void free_closure_info(nested_symbol_map_t* nested_symbol_map UNUSED_PARAMETER)
{
    // free(nested_symbol_map->source_list);
    // free(nested_symbol_map->target_list);
}

static void copy_block_scope(decl_context_t new_block_context_, scope_t* block_scope, nested_symbol_map_t* nested_symbol_map)
{
    closure_hash_t closure_info;
    memset(&closure_info, 0, sizeof(closure_info));

    closure_info.new_block_context_ = new_block_context_;
    closure_info.original_block_scope = block_scope;
    closure_info.nested_symbol_map = nested_symbol_map;

    // First walk, sign in all the names but leave them empty
    rb_tree_walk(block_scope->hash, (void (*)(const void*, void*, void*))register_symbols, &closure_info);
    // Fill the created symbols
    rb_tree_walk(block_scope->hash, (void (*)(const void*, void*, void*))fill_symbols, &closure_info);

    // free(closure_info.filled_symbols);
}

static void register_symbols_of_fortran_program_unit(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data);

void copy_fortran_program_unit(scope_entry_t* new_program_unit,
        scope_entry_t* original_program_unit,
        symbol_map_t** out_symbol_map)
{
    nested_symbol_map_t *nested_symbol_map = new_nested_symbol_map(get_empty_map());

    decl_context_t new_block_context_ = new_program_unit->related_decl_context;
    scope_t* block_scope = original_program_unit->related_decl_context.block_scope;

    // - Block scope
    closure_hash_t *closure_info = calloc(1, sizeof(*closure_info));
    memset(closure_info, 0, sizeof(*closure_info));

    closure_info->new_block_context_ = new_block_context_;
    closure_info->original_block_scope = block_scope;
    closure_info->nested_symbol_map = nested_symbol_map;

    // First walk, sign in all the names but leave them empty
    rb_tree_walk(block_scope->hash, (void (*)(const void*, void*, void*))register_symbols_of_fortran_program_unit, closure_info);
    // Fill the created symbols
    rb_tree_walk(block_scope->hash, (void (*)(const void*, void*, void*))fill_symbols_of_fortran_program_unit, closure_info);

    *out_symbol_map = (symbol_map_t*)nested_symbol_map;
}

static void fill_symbols_generic(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data,
        char (*filter)(scope_entry_t*))
{
    scope_entry_list_iterator_t *it;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (!filter(entry))
            continue;

        scope_entry_t* mapped_symbol = nested_symbol_map_fun((symbol_map_t*)data->nested_symbol_map, entry);

        ERROR_CONDITION( (mapped_symbol == entry), "Invalid mapping for symbol '%s'\n", name);

        int i;
        for (i = 0; i < data->num_filled; i++)
        {
            if (data->filled_symbols[i] == entry)
                return;
        }

        P_LIST_ADD(data->filled_symbols, data->num_filled, entry);

        symbol_deep_copy(mapped_symbol, entry, data->new_block_context_, (symbol_map_t*)data->nested_symbol_map);
    }
    entry_list_iterator_free(it);
}

static void fill_symbols(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data)
{
    fill_symbols_generic(name, entry_list, data, any_symbols);
}

static void fill_symbols_of_fortran_program_unit(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data)
{
    fill_symbols_generic(name, entry_list, data, symbols_of_fortran_program_unit);
}

nodecl_t nodecl_deep_copy(nodecl_t n, decl_context_t new_decl_context, symbol_map_t* symbol_map)
{
    if (symbol_map == NULL)
        symbol_map = get_empty_map();

    symbol_map_t *synth_map = NULL;

    nodecl_t result = nodecl_deep_copy_rec(n, new_decl_context,
            symbol_map,
            &synth_map);

    return result;
}
