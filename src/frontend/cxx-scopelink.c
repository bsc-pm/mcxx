/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "cxx-scopelink.h"
#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-utils.h"
#include "red_black_tree.h"

static unsigned long long _bytes_scopelink = 0;

unsigned long long scopelink_used_memory(void)
{
    return _bytes_scopelink;
}

struct scope_link_tag
{
    rb_red_blk_tree* h;
    decl_context_t global_decl_context;
};

typedef struct scope_link_entry_tag
{
    decl_context_t decl_context;
} scope_link_entry_t;

static void null_dtor(const void* v UNUSED_PARAMETER) { }

static int intptr_t_comp(const void *v1, const void *v2)
{
    intptr_t p1 = (intptr_t)(v1);
    intptr_t p2 = (intptr_t)(v2);

    if (p1 < p2)
        return -1;
    else if (p1 > p2)
        return 1;
    else
        return 0;
}

scope_link_t* scope_link_new(decl_context_t global_decl_context)
{
    scope_link_t* result = counted_calloc(1, sizeof(*result), &_bytes_scopelink);

    result->h = rb_tree_create(intptr_t_comp, null_dtor, null_dtor);
    result->global_decl_context = global_decl_context;

    return result;
}

decl_context_t scope_link_get_global_decl_context(scope_link_t* sl)
{
    return sl->global_decl_context;
}

void scope_link_set(scope_link_t* sl, AST a, decl_context_t decl_context)
{
    if (a == NULL || decl_context.current_scope == NULL)
        return;

    DEBUG_CODE()
    {
        fprintf(stderr, "SCOPELINK: Linking node '%s' with scope '%p'\n", 
                ast_location(a),
                decl_context.current_scope);
    }


    rb_red_blk_node *node = rb_tree_query(sl->h, a);
    if (node == NULL)
    {
        scope_link_entry_t* new_entry = counted_calloc(1, sizeof(*new_entry), &_bytes_scopelink);
        new_entry->decl_context = decl_context;
        rb_tree_add(sl->h, a, new_entry);
    }
    else
    {
        scope_link_entry_t* entry = rb_node_get_info(node);
        entry->decl_context = decl_context;
    }
}

void scope_link_unset(scope_link_t* sl, AST a)
{
    rb_red_blk_node *node = rb_tree_query(sl->h, a);
    if (node != NULL)
        rb_tree_delete(sl->h, node);
}

static scope_link_entry_t* scope_link_get(scope_link_t* sl, AST a)
{
    scope_link_entry_t* result = NULL;

    while (a != NULL)
    {
        scope_link_entry_t* entry = NULL;
        rb_red_blk_node *node = rb_tree_query(sl->h, a);
        if (node != NULL)
        {
            entry = rb_node_get_info(node);
        }

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
    scope_link_entry_t* result = NULL;
    rb_red_blk_node *node = rb_tree_query(sl->h, a);
    if (node != NULL)
    {
        result = rb_node_get_info(node);
    }

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
