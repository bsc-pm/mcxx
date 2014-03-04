#include <string.h>

#include "cxx-nodecl-deep-copy.h"
#include "cxx-nodecl-output.h"
#include "cxx-scope.h"
#include "cxx-entrylist.h"
#include "cxx-utils.h"
#include "cxx-symbol-deep-copy.h"

// Machine generated in cxx-nodecl-deep-copy-base.c
extern nodecl_t nodecl_deep_copy_rec(nodecl_t n, 
        decl_context_t new_decl_context,
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

static decl_context_t copy_block_scope(decl_context_t new_decl_context, 
        decl_context_t orig_decl_context, 
        nested_symbol_map_t* nested_symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map);

static decl_context_t update_function_scope(decl_context_t new_decl_context,
        decl_context_t orig_decl_context,
        nested_symbol_map_t* nested_symbol_map);
static decl_context_t copy_function_scope(decl_context_t new_decl_context,
        decl_context_t orig_decl_context,
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
        result = xcalloc(1, sizeof(*result));
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

nested_symbol_map_t* new_nested_symbol_map(symbol_map_t* enclosing_map)
{
    nested_symbol_map_t *nested_symbol_map = xcalloc(1, sizeof(*nested_symbol_map));

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
        decl_context_t new_decl_context,
        symbol_map_t* enclosing_map,
        symbol_map_t** new_map,

        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map,

        char create_new_function_context
        )
{
    decl_context_t orig_decl_context = nodecl_get_decl_context(n);

    if (orig_decl_context.current_scope->kind != BLOCK_SCOPE)
    {
        internal_error("Attempted to perform a deep copy of a context involving a non block-scope."
                "\nThis is not supported\nContext node at '%s'\n",
                nodecl_locus_to_str(n));
    }

    nested_symbol_map_t* nested_symbol_map = new_nested_symbol_map(enclosing_map);

    // Is this block scope being copied? Check the map
    scope_t* mapped_block_scope = (scope_t*)nested_symbol_map->base_.map(&nested_symbol_map->base_,
            (scope_entry_t*)orig_decl_context.block_scope);

    if (mapped_block_scope == orig_decl_context.block_scope)
    {
        new_decl_context = copy_block_scope(new_decl_context,
                orig_decl_context,
                nested_symbol_map,
                nodecl_deep_copy_map,
                symbol_deep_copy_map);
    }
    else
    {
        new_decl_context.block_scope = mapped_block_scope;
        new_decl_context.current_scope = mapped_block_scope;
    }

    if (create_new_function_context)
    {
        new_decl_context = copy_function_scope(new_decl_context,
                orig_decl_context,
                nested_symbol_map,
                nodecl_deep_copy_map,
                symbol_deep_copy_map);
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
        decl_context_t new_decl_context,
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
    scope_t* original_scope;
    decl_context_t new_decl_context;
    nested_symbol_map_t* nested_symbol_map;

    nodecl_deep_copy_map_t* nodecl_deep_copy_map;
    symbol_deep_copy_map_t* symbol_deep_copy_map;

    int num_filled;
    scope_entry_t** filled_symbols;
} closure_hash_t;

static void symbol_deep_copy_map_add(symbol_deep_copy_map_t* symbol_deep_copy_map,
        scope_entry_t *orig,
        scope_entry_t *copied);

static void register_symbols_generic(const char* name,
        scope_entry_list_t* entry_list,
        closure_hash_t* data,
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

        char is_proper_symbol = data->original_scope == entry->decl_context.current_scope;

        scope_entry_t* mapped_symbol = nested_symbol_map_fun((symbol_map_t*)data->nested_symbol_map, entry);

        if (mapped_symbol == entry // If the symbol is not mapped...
                || (is_proper_symbol // or is a proper symbol that has been
                                     // mapped but not to a symbol of the scope being created...
                    && mapped_symbol->decl_context.current_scope != data->new_decl_context.current_scope))
        {
            // then create a new symbol in the scope being created...
            scope_entry_t* new_entry = new_symbol(data->new_decl_context, data->new_decl_context.current_scope, name);

            symbol_deep_copy_map_add(data->symbol_deep_copy_map, entry, new_entry);

            // and map the symbol to the mapped one
            nested_map_add(data->nested_symbol_map, entry, new_entry);

            mapped_symbol = new_entry;
        }
        else
        {
            insert_alias(data->new_decl_context.current_scope, mapped_symbol, name);
            // We do not want these symbols be filled again
            P_LIST_ADD(data->filled_symbols, data->num_filled, entry);
        }
    }
    entry_list_iterator_free(it);
}

static char any_symbols(scope_entry_t* entry UNUSED_PARAMETER)
{
    return 1;
}

static void register_symbols(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data)
{
    register_symbols_generic(name, entry_list, data, any_symbols);
}

static void fill_symbols(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data);

void xfree_closure_info(nested_symbol_map_t* nested_symbol_map UNUSED_PARAMETER)
{
    // xfree(nested_symbol_map->source_list);
    // xfree(nested_symbol_map->target_list);
}

static void copy_scope(decl_context_t new_decl_context, scope_t* original_scope,
        nested_symbol_map_t* nested_symbol_map,

        nodecl_deep_copy_map_t *nodecl_deep_copy_map,
        symbol_deep_copy_map_t *symbol_deep_copy_map)
{
    closure_hash_t closure_info;
    memset(&closure_info, 0, sizeof(closure_info));

    closure_info.original_scope = original_scope;
    closure_info.new_decl_context = new_decl_context;
    closure_info.nested_symbol_map = nested_symbol_map;
    closure_info.nodecl_deep_copy_map = nodecl_deep_copy_map;
    closure_info.symbol_deep_copy_map = symbol_deep_copy_map;

    // First walk, sign in all the names but leave them empty
    rb_tree_walk(original_scope->hash, (void (*)(const void*, void*, void*))register_symbols, &closure_info);
    // Fill the created symbols
    rb_tree_walk(original_scope->hash, (void (*)(const void*, void*, void*))fill_symbols, &closure_info);

    // xfree(closure_info.filled_symbols);
    
    new_decl_context.current_scope->related_entry 
        = nested_symbol_map_fun((symbol_map_t*)nested_symbol_map, original_scope->related_entry);
}

static decl_context_t copy_function_scope(decl_context_t new_decl_context,
        decl_context_t orig_decl_context,
        nested_symbol_map_t* nested_symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map)
{
    new_decl_context = new_function_context(new_decl_context);

    scope_t* old_current = new_decl_context.current_scope;

    // Keep a mapping in the symbol map
    nested_map_add(nested_symbol_map,
            (scope_entry_t*)orig_decl_context.function_scope,
            (scope_entry_t*)new_decl_context.function_scope);

    new_decl_context.current_scope = new_decl_context.function_scope;
    copy_scope(new_decl_context,
            orig_decl_context.function_scope,
            nested_symbol_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map);

    new_decl_context.current_scope = old_current;

    return new_decl_context;
}

static decl_context_t update_function_scope(decl_context_t new_decl_context,
        decl_context_t orig_decl_context,
        nested_symbol_map_t* nested_symbol_map)
{
    scope_t* function_scope = (scope_t*)nested_symbol_map->base_.map(&nested_symbol_map->base_, 
            (scope_entry_t*)orig_decl_context.function_scope);

    new_decl_context.function_scope = function_scope;

    return new_decl_context;
}

static decl_context_t copy_block_scope(decl_context_t new_decl_context, 
        decl_context_t orig_decl_context, 
        nested_symbol_map_t* nested_symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map)
{
    new_decl_context = new_block_context(new_decl_context);

    // Keep a mapping in the symbol map
    nested_map_add(nested_symbol_map,
            (scope_entry_t*)orig_decl_context.block_scope,
            (scope_entry_t*)new_decl_context.block_scope);

    copy_scope(new_decl_context,
            orig_decl_context.block_scope,
            nested_symbol_map,
            nodecl_deep_copy_map,
            symbol_deep_copy_map);

    return new_decl_context;
}

static void fill_symbols_generic(const char* name,
        scope_entry_list_t* entry_list,
        closure_hash_t* data,
        char (*filter)(scope_entry_t*),
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map)
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

        symbol_deep_copy_compute_maps(mapped_symbol,
                entry, data->new_decl_context,
                (symbol_map_t*)data->nested_symbol_map,
                nodecl_deep_copy_map,
                symbol_deep_copy_map);
    }
    entry_list_iterator_free(it);
}

static void fill_symbols(const char* name, scope_entry_list_t* entry_list, closure_hash_t* data)
{
    fill_symbols_generic(name, entry_list, data, any_symbols,
            data->nodecl_deep_copy_map, data->symbol_deep_copy_map);
}

nodecl_t nodecl_deep_copy_function_code(nodecl_t n,
        decl_context_t new_decl_context,
        symbol_map_t* symbol_map UNUSED_PARAMETER,
        symbol_map_t** synth_symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map
        )
{
    scope_entry_t* orig_symbol = nodecl_get_symbol(n);
    scope_entry_t* symbol = (*synth_symbol_map)->map(*synth_symbol_map, orig_symbol);

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

    symbol->entity_specs.function_code = result;
    symbol->related_decl_context = nodecl_get_decl_context(nodecl_get_child(result, 0));
    return result;
}

nodecl_t nodecl_deep_copy_compute_maps(nodecl_t n,
        decl_context_t new_decl_context,
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

nodecl_t nodecl_deep_copy(nodecl_t n, decl_context_t new_decl_context, symbol_map_t* symbol_map)
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
    nodecl_deep_copy_map_t* nodecl_deep_copy_map = xcalloc(1, sizeof(*nodecl_deep_copy_map));
    return nodecl_deep_copy_map;
}

symbol_deep_copy_map_t* symbol_deep_copy_map_new(void)
{
    symbol_deep_copy_map_t* symbol_deep_copy_map = xcalloc(1, sizeof(*symbol_deep_copy_map));
    return symbol_deep_copy_map;
}

void nodecl_deep_copy_map_free(nodecl_deep_copy_map_t* nodecl_deep_copy_map)
{
    if (nodecl_deep_copy_map == NULL)
        return;

    xfree(nodecl_deep_copy_map->orig);
    xfree(nodecl_deep_copy_map->copied);

    xfree(nodecl_deep_copy_map);
}

void symbol_deep_copy_map_free(symbol_deep_copy_map_t* symbol_deep_copy_map)
{
    if (symbol_deep_copy_map == NULL)
        return;

    xfree(symbol_deep_copy_map->orig);
    xfree(symbol_deep_copy_map->copied);

    xfree(symbol_deep_copy_map);
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

static void symbol_deep_copy_map_add(symbol_deep_copy_map_t* symbol_deep_copy_map,
        scope_entry_t *orig,
        scope_entry_t *copied)
{
    if (symbol_deep_copy_map == NULL)
        return;

    int num_mappings = symbol_deep_copy_map->num_mappings;
    P_LIST_ADD(symbol_deep_copy_map->orig, num_mappings, orig);
    P_LIST_ADD(symbol_deep_copy_map->copied, symbol_deep_copy_map->num_mappings, copied);
}
