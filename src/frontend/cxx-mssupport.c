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




#include <string.h>
#include <ctype.h>

#include "cxx-mssupport.h"
#include "cxx-ast.h"
#include "cxx-prettyprint.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-typeutils.h"
#include "cxx-ambiguity.h"
#include "cxx-exprtype.h"
#include "cxx-tltype.h"
#include "cxx-entrylist.h"
#include "cxx-diagnostic.h"

static void gather_ms_declspec_item(AST a,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context)
{
    ERROR_CONDITION(ASTKind(a) != AST_MS_DECLSPEC_ITEM, "Invalid node", 0);

    const char* declspec_name = ASTText(a);
    ERROR_CONDITION(declspec_name == NULL, "Invalide node", 0);

    if (strcmp(declspec_name, "align") == 0)
    {
        AST expr_list = ASTSon0(a);
        int num_items = 0;
        if (expr_list != NULL)
        {
            AST it = NULL;
            for_each_element(expr_list, it)
            {
                num_items++;
            }
        }

        if (num_items == 1)
        {
            AST align_expr = ASTSon1(ast_list_head(expr_list));

            nodecl_t nodecl_align_expr = nodecl_null();
            check_expression(align_expr, decl_context, &nodecl_align_expr);

            if (nodecl_is_err_expr(nodecl_align_expr))
                return;

            ERROR_CONDITION(gather_info->num_ms_attributes >= MCXX_MAX_GCC_ATTRIBUTES_PER_SYMBOL,
                    "Too many __declspecs in this symbol", 0);

            gcc_attribute_t new_ms_attribute;
            new_ms_attribute.attribute_name = declspec_name;
            new_ms_attribute.expression_list =
                nodecl_make_list_1(nodecl_align_expr);

            P_LIST_ADD(gather_info->ms_attributes, gather_info->num_ms_attributes, new_ms_attribute);
        }
        else if (num_items == 2)
        {
            warn_printf_at(ast_get_locus(a), "ignoring unsupported form of __declspec(align) specifier\n");
        }
        else
        {
            warn_printf_at(ast_get_locus(a), "ignoring malformed __declspec(align) specifier\n");
        }
    }
    else if (strcmp(declspec_name, "intrin_type") == 0)
    {
            ERROR_CONDITION(gather_info->num_ms_attributes >= MCXX_MAX_GCC_ATTRIBUTES_PER_SYMBOL,
                    "Too many __declspecs in this symbol", 0);

            gcc_attribute_t new_ms_attribute;
            new_ms_attribute.attribute_name = declspec_name;
            new_ms_attribute.expression_list = nodecl_null();

            P_LIST_ADD(gather_info->ms_attributes, gather_info->num_ms_attributes, new_ms_attribute);
    }
    else
    {
        warn_printf_at(ast_get_locus(a), "ignoring unhandled __declspec(%s)\n", declspec_name);
    }
}

void gather_ms_declspec(AST a,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context)
{
    ERROR_CONDITION(ASTKind(a) != AST_MS_DECLSPEC, "Invalid node", 0);
    AST list = ASTSon0(a), it = NULL;

    for_each_element(list, it)
    {
        gather_ms_declspec_item(ASTSon1(it), gather_info, decl_context);
    }
}

void gather_ms_declspec_list(AST a,
        gather_decl_spec_t* gather_info,
        const decl_context_t* decl_context)
{
    ERROR_CONDITION(ASTKind(a) != AST_NODE_LIST, "Node must be a list", 0);

    AST list = a, it = NULL;
    for_each_element(list, it)
    {
        gather_ms_declspec(ASTSon1(it), gather_info, decl_context);
    }
}

// This one is similar to keep_gcc_attributes_in_symbol
void keep_ms_declspecs_in_symbol(
        scope_entry_t* entry,
        gather_decl_spec_t* gather_info)
{
    // Combine them
    int i;
    for (i = 0; i < gather_info->num_ms_attributes; i++)
    {
        char found = 0;
        int j;
        for (j = 0; j < symbol_entity_specs_get_num_ms_attributes(entry) && !found; j++)
        {
            found = (strcmp(symbol_entity_specs_get_ms_attributes_num(entry, j).attribute_name,
                        gather_info->ms_attributes[i].attribute_name) == 0);
        }

        if (found)
        {
            // Update with the freshest value 
            gcc_attribute_t ms_attr = symbol_entity_specs_get_ms_attributes_num(entry, j - 1);
            ms_attr.expression_list = gather_info->ms_attributes[i].expression_list;
            symbol_entity_specs_set_ms_attributes_num(entry, j - 1, ms_attr);
        }
        else
        {
            symbol_entity_specs_add_ms_attributes(entry,
                    gather_info->ms_attributes[i]);
        }
    }
}

void apply_ms_attribute_to_type(AST a UNUSED_PARAMETER,
        type_t** type UNUSED_PARAMETER,
        const decl_context_t* decl_context UNUSED_PARAMETER)
{
}
