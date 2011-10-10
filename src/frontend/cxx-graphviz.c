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

static rb_red_blk_tree* pointer_set = NULL;

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
    if (rb_tree_query(pointer_set, entry) != NULL)
        return;

    const char* symbol_name = entry->symbol_name;
    if (symbol_name != NULL)
    {
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
    }
    else
    {
        symbol_name = "<<unnamed symbol>>";
    }
    fprintf(f, "sym_%zd[fontcolor=\"/dark28/2\",color=\"/dark28/2\", shape=rectangle,label=\"%s\\n%s:%d\"]\n", 
            (size_t)entry, symbol_name, entry->file, entry->line);

    rb_tree_insert(pointer_set, entry, entry);
}

static void scope_t_dump_graphviz(FILE* f, scope_t* scope)
{
    if (rb_tree_query(pointer_set, scope) != NULL)
        return;

    size_t s = (size_t)scope;

    const char* scope_kind = NULL;
    switch (scope->kind)
    {
        case UNDEFINED_SCOPE: { scope_kind = "UNDEFINED_SCOPE"; break; } 
        case NAMESPACE_SCOPE: { scope_kind = "NAMESPACE_SCOPE"; break; }
        case FUNCTION_SCOPE: { scope_kind = "FUNCTION_SCOPE"; break; } 
        case PROTOTYPE_SCOPE: { scope_kind = "PROTOTYPE_SCOPE"; break; } 
        case BLOCK_SCOPE: { scope_kind = "BLOCK_SCOPE"; break; } 
        case CLASS_SCOPE: { scope_kind = "CLASS_SCOPE"; break; } 
        default: { scope_kind = "<<UNKNOWN SCOPE>>"; break; }
    }

    fprintf(f, "scope_%zd[label=\"%s\\n%p\", shape=rectangle, fontcolor=\"/dark28/1\",color=\"/dark28/1\"]\n", s, scope_kind, scope);

    if (scope->related_entry != NULL)
    {
        symbol_dump_graphviz(f, scope->related_entry);

        fprintf(f, "scope_%zd -> sym_%zd [layer=\"symbols\",label=\"%s\",fontcolor=\"/dark28/2\",color=\"/dark28/2\"]\n",
                s, (size_t)scope->related_entry, "sym");
    }

    rb_tree_insert(pointer_set, scope, scope);

    if (scope->contained_in != NULL)
    {
        scope_t_dump_graphviz(f, scope->contained_in);
        fprintf(f, "scope_%zd -> scope_%zd [layer=\"symbols\",style=dashed,label=\"%s\",fontcolor=\"/dark28/1\",color=\"/dark28/1\"]\n",
                s, (size_t)scope->contained_in, "contained_in");
    }
}

static int decl_context_t_dump_graphviz(FILE* f, decl_context_t decl_context)
{
    static int i = 1;


#define DUMP_ALL \
    DUMP_SCOPE(block) \
    DUMP_SCOPE(prototype) \
    DUMP_SCOPE(class) \
    DUMP_SCOPE(function) \
    DUMP_SCOPE(namespace) \
    DUMP_SCOPE(global) \
    DUMP_SCOPE(current) 


#define DUMP_SCOPE(name) \
    if (decl_context.name##_scope != NULL) \
    { \
        scope_t_dump_graphviz(f, decl_context.name##_scope); \
    }
    DUMP_ALL
#undef DUMP_SCOPE

    int num = 0;
#define DUMP_SCOPE(name) \
    if (decl_context.name##_scope != NULL) \
    { \
        if (num != 0) \
            fprintf(f, "| "); \
        fprintf(f, "<%s> %s", #name, #name); \
        num++; \
    }
    fprintf(f, "decl_context_%d[shape=record, fontcolor=\"/dark28/1\", color=\"/dark28/1\", layer=\"scopes\", label=\"<context> Context|", i);
    DUMP_ALL
    fprintf(f, "\"]\n");
#undef DUMP_SCOPE

#define DUMP_SCOPE(name) \
    if (decl_context.name##_scope != NULL) \
    { \
        fprintf(f, "decl_context_%d:" #name " -> scope_%zd [layer=\"scopes\", color=\"/dark28/1\"]\n", i, (size_t)decl_context.name##_scope); \
    }
    DUMP_ALL
#undef DUMP_SCOPE

    return i++;
}

static void ast_dump_graphviz_rec(AST a, FILE* f, size_t parent_node, int position)
{
    if (rb_tree_query(pointer_set, a) != NULL)
        return;

    const char* shape = "box";
    const char* color = "color=\"#000000\",fontcolor=\"#000000\"";

    // I know this is not exact, but there is a %z qualifier in printf
    // while there is not such thing for intptr_t
    size_t current_node = (size_t)a;

    if (a != NULL)
    {
        if (ASTType(a) == AST_AMBIGUITY) 
        {
            shape = "ellipse";
            color = "color=\"/dark28/4\",fontcolor=\"/dark28/4\"";
        }
        else if (ASTType(a) == AST_NODE_LIST) 
        {
            shape = "Mdiamond";
        }

        if (ASTText(a))
        {
            char *quoted = quote_protect(ASTText(a));

            fprintf(f, "n%zd[layer=\"trees\",%s,shape=%s,label=\"%s\\nNode=%p\\nParent=%p\\n%s\\nText: \\\"%s\\\"\"]\n", 
                    current_node, color, shape, ast_print_node_type(ASTType(a)), a, ASTParent(a), ast_location(a), quoted);

            free(quoted);
        }
        else
        {
            fprintf(f, "n%zd[layer=\"trees\",%s,shape=%s,label=\"%s\\nNode=%p\\nParent=%p\\n%s\"]\n", 
                    current_node, color, shape, ast_print_node_type(ASTType(a)), a, ASTParent(a), ast_location(a));
        }

        // Print this only for non extended referenced nodes
        if (parent_node != 0)
        {
            fprintf(f, "n%zd -> n%zd [layer=\"trees\",label=\"%d\"]\n", parent_node, current_node, position);
        }

        if (ASTType(a) != AST_AMBIGUITY)
        {
            int i;
            for(i = 0; i < ASTNumChildren(a); i++)
            {
                if (ASTChild(a, i) != NULL)
                {
                    ast_dump_graphviz_rec(ASTChild(a, i), f, current_node, i);
                }
            }

            // Now print all extended trees referenced here
            // First get all TL_AST in 'orig' that point to its childrens

            extensible_struct_t* extended_data = ast_get_extensible_struct(a);

            if (extended_data != NULL)
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
                            ast_dump_graphviz_rec(child, f, /* parent_node */ 0, /* position */ 0);
                        }

                        // Add an edge
                        fprintf(f, "n%zd -> n%zd [layer=\"trees\",label=\"%s\"]\n",
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
                ast_dump_graphviz_rec(ast_get_ambiguity(a, i), f, current_node, i);
            }
        }

        if (ASTType(a) == NODECL_CONTEXT || 
                ASTType(a) == NODECL_PRAGMA_CONTEXT)
        {
            int k = decl_context_t_dump_graphviz(f, nodecl_get_decl_context(_nodecl_wrap(a)));

            fprintf(f, "n%zd -> decl_context_%d:context:n [layer=\"scopes\", label=\"%s\",fontcolor=\"/dark28/1\",color=\"/dark28/1\"]\n",
                    current_node, k,
                    "context");
        }

        scope_entry_t* entry = nodecl_get_symbol(_nodecl_wrap(a));

        if (entry != NULL)
        {
            symbol_dump_graphviz(f, entry);
            fprintf(f, "n%zd -> sym_%zd [layer=\"symbols\",label=\"%s\",fontcolor=\"/dark28/2\",color=\"/dark28/2\"]\n",
                    current_node,
                    (size_t)entry,
                    "sym");
        }
    }
    else
    {
        fprintf(f, "n%zd[shape=circle,label=\"\",fixedsize=true,style=filled,fillcolor=black,height=0.1,width=0.1,layer=\"trees\"]\n", current_node);
        if (parent_node != 0)
        {
            fprintf(f, "n%zd -> n%zd [label=\"%d\"]\n", parent_node, current_node, position);
        }
    }

    rb_tree_insert(pointer_set, a, a);
}

static int comp_vptr(const void* v1, const void *v2)
{
    if (v1 < v2)
        return -1;
    else if (v1 > v2)
        return 1;
    else 
        return 0;
}

static void null_dtor(const void* v UNUSED_PARAMETER) { }

void ast_dump_graphviz(AST a, FILE* f)
{
    pointer_set = rb_tree_create(comp_vptr, null_dtor, null_dtor);

    fprintf(f, "digraph mcxx_ast { \n");
    // fprintf(f, "   nodesep=0.5;\n");
    // fprintf(f, "   splines=polyline;\n");
    fprintf(f, "   colorscheme=ColorBrewer;\n");
    fprintf(f, "   node [layer=\"all\"];\n");
	fprintf(f, "   edge [layer=\"all\"];\n");
    ast_dump_graphviz_rec(a, f, 0, 0);
    fprintf(f, "}\n");
}
