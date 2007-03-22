#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "cxx-scopelink.h"
#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-utils.h"

typedef struct scope_link_entry_tag
{
    scope_t* st;
    decl_context_t decl_context;
} scope_link_entry_t;

static int integer_comp (void *key1, void *key2)
{
    intptr_t a = (intptr_t)(key1);
    intptr_t b = (intptr_t)(key2);

    if (a == b)
    {
        return 0;
    }
    else if (a < b)
    {
        return -1;
    }
    else return 1;
}

static int pointer_hash(void* key, int size)
{
    intptr_t v = (intptr_t)(key);

    return (v % size);
}

scope_link_t* scope_link_new(void)
{
    scope_link_t* result = calloc(1, sizeof(*result));

    result->h = hash_create(23, pointer_hash, integer_comp);

    return result;
}

void scope_link_set(scope_link_t* sl, AST a, scope_t* st, decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "scope_link -> Linking node '%s' with scope '%p'\n", 
                node_information(a),
                st);
    }

    scope_link_entry_t* new_entry = calloc(1, sizeof(*new_entry));

    new_entry->st = st;
    new_entry->decl_context = decl_context;

    hash_put(sl->h, a, new_entry);
}

static scope_link_entry_t* scope_link_get(scope_link_t* sl, AST a)
{
    scope_link_entry_t* result = NULL;

    while (a != NULL)
    {
        scope_link_entry_t* entry = (scope_link_entry_t*)hash_get(sl->h, a);

        if (entry != NULL)
        {
            result = entry;
            break;
        }

        a = ASTParent(a);
    }

    return NULL;
}

scope_t* scope_link_get_scope(scope_link_t* sl, AST a)
{
    scope_link_entry_t* entry = scope_link_get(sl, a);

    if (entry == NULL)
        return NULL;
    return 
        entry->st;
}

decl_context_t scope_link_get_decl_context(scope_link_t* sl, AST a)
{
    scope_link_entry_t* entry = scope_link_get(sl, a);

    if (entry == NULL)
        return default_decl_context;
    return 
        entry->decl_context;
}

static scope_link_entry_t* scope_link_direct_get_scope(scope_link_t* sl, AST a)
{
    scope_link_entry_t* result = (scope_link_entry_t*)hash_get(sl->h, a);

    return result;
}

static AST duplicate_ast_with_scope_link_rec(AST a, scope_link_t* orig, scope_link_t* new_sl);

AST duplicate_ast_with_scope_link(AST a, scope_link_t* orig, scope_link_t* new_sl)
{
    // This scope must be always available
    AST result = duplicate_ast_with_scope_link_rec(a, orig, new_sl);

    scope_t* st = scope_link_get_scope(orig, a);
    decl_context_t decl_context = scope_link_get_decl_context(orig, a);
    scope_link_set(new_sl, result, st, decl_context);

    return result;
}


static AST duplicate_ast_with_scope_link_rec(AST a, scope_link_t* orig, scope_link_t* new_sl)
{
    if (a == NULL)
        return NULL;

    AST result = calloc(1, sizeof(*result));

    // extensible_struct_t orig_extended_data = result->extended_data;

    // Update the scope_link
    scope_link_entry_t* sl_entry = scope_link_direct_get_scope(orig, a);
    if (sl_entry != NULL)
    {
        scope_link_set(new_sl, result, sl_entry->st, sl_entry->decl_context);
    }

    // Copy everything by value
    *result = *a;

    // Restore original extended data
    // result->extended_data = orig_extended_data;

    int i;
    for (i = 0; i < ASTNumChildren(result); i++)
    {
        ASTChild(result, i) = duplicate_ast_with_scope_link_rec(ASTChild(a, i), orig, new_sl);
        if (ASTChild(result, i) != NULL)
        {
            ASTParent(ASTChild(result, i)) = result;
        }
    }

    if (ASTText(a) != NULL)
    {
        ASTText(result) = strdup(ASTText(a));
    }
    ASTParent(result) = NULL;

    return result;
}

