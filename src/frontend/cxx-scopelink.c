/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "cxx-scopelink.h"
#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-utils.h"

static unsigned long long _bytes_scopelink = 0;

unsigned long long scopelink_used_memory(void)
{
    return _bytes_scopelink;
}

struct scope_link_tag
{
    Hash* h;
    decl_context_t global_decl_context;
};

typedef struct scope_link_entry_tag
{
    decl_context_t decl_context;
} scope_link_entry_t;

scope_link_t* scope_link_new(decl_context_t global_decl_context)
{
    scope_link_t* result = counted_calloc(1, sizeof(*result), &_bytes_scopelink);

    result->h = hash_create(HASH_SIZE, HASHFUNC(pointer_hash), KEYCMPFUNC(integer_comp));
    result->global_decl_context = global_decl_context;

    return result;
}

decl_context_t scope_link_get_global_decl_context(scope_link_t* sl)
{
    return sl->global_decl_context;
}

void scope_link_set(scope_link_t* sl, AST a, decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPELINK: Linking node '%s' with scope '%p'\n", 
                ast_location(a),
                decl_context.current_scope);
    }

    if (a == NULL)
        return;

    scope_link_entry_t* new_entry = counted_calloc(1, sizeof(*new_entry), &_bytes_scopelink);

    new_entry->decl_context = decl_context;

    hash_put(sl->h, a, new_entry);
}

void scope_link_unset(scope_link_t* sl, AST a)
{
    scope_link_entry_t* result = (scope_link_entry_t*)hash_get(sl->h, a);
    if (result != NULL)
        hash_delete(sl->h, a);
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

    return result;
}

decl_context_t scope_link_get_decl_context(scope_link_t* sl, AST a)
{
    scope_link_entry_t* entry = scope_link_get(sl, a);

    if (entry == NULL)
        return sl->global_decl_context;
    return 
        entry->decl_context;
}

char scope_link_direct_get_scope(scope_link_t* sl, AST a, decl_context_t *decl_result)
{
    scope_link_entry_t* result = (scope_link_entry_t*)hash_get(sl->h, a);

    if (result != NULL)
    {
        *decl_result = result->decl_context;
        return 1;
    }
    else
    {
        return 0;
    }
}
