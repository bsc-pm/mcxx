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



#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cxx-driver.h"
#include "cxx-graphviz.h"
#include "cxx-ast.h"
#include "cxx-exprtype.h"
#include "cxx-tltype.h"
#include "cxx-utils.h"

/*
   ****************************************************
   Graphviz Output
   ****************************************************
   http://www.graphviz.org/
 */

static char* quote_protect(const char *c)
{
    char *result = calloc(2*strlen(c), sizeof(char));

    const char *p = c;
    char *q = result;
    char *end = result + 2*strlen(c) - 1;

    while (*p != '\0'
            && (q < end))
    {
        char ignore = 0;
        if (*p == '\\'
                || *p == '"')
        {
            *(q++) = '\\';
        }
        else if (*p == '\n' 
                || *p == '\r')
        {
            *(q++) = '\\';
            *(q++) = 'n';
            ignore = 1;
        }

        if (!ignore)
        {
            *(q++) = *p;
        }
        p++;
    }

    return result;
}

static void symbol_dump_graphviz(FILE* f, scope_entry_t* entry)
{
    const char* symbol_name = entry->symbol_name;
    CXX_LANGUAGE()
    {
        if (entry->kind == SK_FUNCTION)
        {
            symbol_name = print_decl_type_str(entry->type_information, entry->decl_context,
                    get_qualified_symbol_name(entry, entry->decl_context));
        }
        else
        {
            symbol_name = get_qualified_symbol_name(entry, entry->decl_context);
        }
    }
    fprintf(f, "sym_%zd[shape=egg,label=\"%s\\n%s:%d\"]", (size_t)entry, symbol_name, entry->file, entry->line);
}

static void ast_dump_graphviz_rec(AST a, FILE* f, size_t parent_node, int position, char is_extended UNUSED_PARAMETER)
{
    // static char* octagon = "octagon";
    // static char* doubleoctagon = "doubleoctagon";
    static char* ellipse = "ellipse";
    static char* mdiamond = "Mdiamond";
    static char* box = "box";
    char* shape;

    // I know this is not exact, but there is a %z qualifier in printf
    // while there is not such thing for intptr_t
    size_t current_node = (size_t)a;

    if (a != NULL)
    {
        // Select shape
        shape = box;
        if (ASTType(a) == AST_AMBIGUITY) shape = ellipse;
        if (ASTType(a) == AST_NODE_LIST) shape = mdiamond;
        // if (a->construct_type == CT_SPECIFICATION) shape = ellipse;
        // else if (a->construct_type == CT_OMP_SPECIFICATION) shape = mdiamond;
        // else if (a->construct_type == CT_EXECUTABLE) shape = octagon;
        // else if (a->construct_type == CT_OMP_EXECUTABLE) shape = doubleoctagon;

        if (ASTText(a))
        {
            char *quoted = quote_protect(ASTText(a));

            fprintf(f, "n%zd[shape=%s,label=\"%s\\nNode=%p\\nParent=%p\\n%s\\nText: \\\"%s\\\"\"]\n", 
                    current_node, shape, ast_print_node_type(ASTType(a)), a, ASTParent(a), ast_location(a), quoted);

            free(quoted);
        }
        else
        {
            fprintf(f, "n%zd[shape=%s,label=\"%s\\nNode=%p\\nParent=%p\\n%s\"]\n", 
                    current_node, shape, ast_print_node_type(ASTType(a)), a, ASTParent(a), ast_location(a));
        }

        // Print this only for non extended referenced nodes
        if (parent_node != 0)
        {
            fprintf(f, "n%zd -> n%zd [label=\"%d\"]\n", parent_node, current_node, position);
        }

        if (ASTType(a) != AST_AMBIGUITY)
        {
            int i;
            if (!is_extended)
            {
                for(i = 0; i < ASTNumChildren(a); i++)
                {
                    if (ASTChild(a, i) != NULL)
                    {
                        ast_dump_graphviz_rec(ASTChild(a, i), f, current_node, i, /* is_extended */ is_extended);
                    }
                }
            }

            // Now print all extended trees referenced here
            // First get all TL_AST in 'orig' that point to its childrens

            extensible_struct_t* extended_data = ast_get_extensible_struct(a);

            if (extended_data != NULL
                    && !is_extended)
            {
                int num_fields = 0;
                const char** keys = NULL;
                void** values = NULL;

                extensible_struct_get_all_data(extended_data, &num_fields, &keys, &values);

                for (i = 0; i < num_fields; i++)
                {
                    const char* field_name = keys[i];
                    void *data = values[i];

                    if (ast_field_name_is_link_to_child(field_name))
                    {
                        AST child = data;
                        if (child != a)
                        {
                            ast_dump_graphviz_rec(child, f, /* parent_node */ 0, /* position */ 0, /* is_extended */ 1);
                        }

                        // Add an edge
                        fprintf(f, "n%zd -> n%zd [label=\"%s\",style=dashed]\n",
                                current_node,
                                (size_t)(child),
                                field_name);
                    }
                }
            }
        }
        else if (ASTType(a) == AST_AMBIGUITY)
        {
            int i;
            for(i = 0; i < ast_get_num_ambiguities(a); i++)
            {
                ast_dump_graphviz_rec(ast_get_ambiguity(a, i), f, current_node, i, /* is_extended */ 0);
            }
        }

        scope_entry_t* entry = expression_get_symbol(a);

        if (entry != NULL)
        {
            symbol_dump_graphviz(f, entry);
            fprintf(f, "n%zd -> sym_%zd [label=\"%s\",style=dotted]\n",
                    current_node,
                    (size_t)entry,
                    "sym");
        }
    }
    else
    {
        fprintf(f, "n%zd[shape=circle,label=\"\",fixedsize=true,style=filled,fillcolor=black,height=0.1,width=0.1]\n", current_node);
        if (parent_node != 0)
        {
            fprintf(f, "n%zd -> n%zd [label=\"%d\"]\n", parent_node, current_node, position);
        }
    }
}


void ast_dump_graphviz(AST a, FILE* f)
{
    fprintf(f, "digraph mcxx_ast { \n");
    ast_dump_graphviz_rec(a, f, 0, 0, /* is_extended */ 0);
    fprintf(f, "}\n");
}
