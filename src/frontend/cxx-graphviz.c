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




#include <stdlib.h>
#include <string.h>

#include "cxx-driver.h"
#include "cxx-graphviz.h"
#include "cxx-ast.h"
#include "cxx-exprtype.h"
#include "cxx-cexpr.h"
#include "cxx-tltype.h"
#include "cxx-utils.h"

/*
   ****************************************************
   Graphviz Output
   ****************************************************
   http://www.graphviz.org/
 */

static void ast_dump_graphviz_rec(AST a, FILE* f, size_t parent_node, int position);

static rb_red_blk_tree* pointer_set = NULL;

static char* quote_protect(const char *c)
{
    char *result = NEW_VEC0(char, 2*strlen(c));

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
    rb_tree_insert(pointer_set, entry, entry);

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

    fprintf(f, "sym_%zd[fontcolor=\"/dark28/2\",color=\"/dark28/2\", shape=rectangle,label=\"%p\\n%s\\n%s:%u\"]\n", 
            (size_t)entry, entry, symbol_name, locus_get_filename(entry->locus), locus_get_line(entry->locus));


    if (!nodecl_is_null(entry->value))
    {
        ast_dump_graphviz_rec(nodecl_get_ast(entry->value), f, 0, -1);

        fprintf(f, "sym_%zd -> n%zd [label=\"value\"]\n",
                (size_t)entry,
                (size_t)nodecl_get_ast(entry->value));
    }

    if (!nodecl_is_null(symbol_entity_specs_get_function_code(entry)))
    {
        ast_dump_graphviz_rec(nodecl_get_ast(symbol_entity_specs_get_function_code(entry)), f, 0, -1);

        fprintf(f, "sym_%zd -> n%zd [label=\"function_code\"]\n",
                (size_t)entry,
                (size_t)nodecl_get_ast(symbol_entity_specs_get_function_code(entry)));
    }
}

static void cval_dump_graphviz(FILE* f, const_value_t* cval)
{
    if (rb_tree_query(pointer_set, cval) != NULL)
        return;
    rb_tree_insert(pointer_set, cval, cval);

    fprintf(f, "const_%zd[fontcolor=\"/dark28/5\",color=\"/dark28/5\", shape=rectangle,label=\"%s\"]\n",
            (size_t)cval,
            quote_protect(const_value_to_str(cval)));
}

static void scope_t_dump_graphviz(FILE* f, scope_t* scope)
{
    if (rb_tree_query(pointer_set, scope) != NULL)
        return;
    rb_tree_insert(pointer_set, scope, scope);

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

        fprintf(f, "scope_%zd -> sym_%zd [label=\"%s\",fontcolor=\"/dark28/2\",color=\"/dark28/2\"]\n",
                s, (size_t)scope->related_entry, "sym");
    }

    if (scope->contained_in != NULL)
    {
        scope_t_dump_graphviz(f, scope->contained_in);
        fprintf(f, "scope_%zd -> scope_%zd [style=dashed,label=\"%s\",fontcolor=\"/dark28/1\",color=\"/dark28/1\"]\n",
                s, (size_t)scope->contained_in, "contained_in");
    }
}

static int decl_context_t_dump_graphviz(FILE* f, const decl_context_t* decl_context)
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
    if (decl_context->name##_scope != NULL) \
    { \
        scope_t_dump_graphviz(f, decl_context->name##_scope); \
    }
    DUMP_ALL
#undef DUMP_SCOPE

    int num = 0;
#define DUMP_SCOPE(name) \
    if (decl_context->name##_scope != NULL) \
    { \
        if (num != 0) \
            fprintf(f, "| "); \
        fprintf(f, "<%s> %s", #name, #name); \
        num++; \
    }
    fprintf(f, "decl_context_%d[shape=record, fontcolor=\"/dark28/1\", color=\"/dark28/1\", label=\"{<context> Context|{", i);
    DUMP_ALL
    fprintf(f, "}}\"]\n");
#undef DUMP_SCOPE

#define DUMP_SCOPE(name) \
    if (decl_context->name##_scope != NULL) \
    { \
        fprintf(f, "decl_context_%d:" #name " -> scope_%zd [color=\"/dark28/1\"]\n", i, (size_t)decl_context->name##_scope); \
    }
    DUMP_ALL
#undef DUMP_SCOPE

    return i++;
}


static void ast_dump_graphviz_rec(AST a, FILE* f, size_t parent_node, int position)
{
    if (rb_tree_query(pointer_set, a) != NULL)
        return;
    rb_tree_insert(pointer_set, a, a);

    const char* shape = "box";
    const char* color = "color=\"#000000\",fontcolor=\"#000000\"";

    // I know this is not exact, but there is a %z qualifier in printf
    // while there is not such thing for intptr_t
    size_t current_node = (size_t)a;

    if (a != NULL)
    {
        char list_is_ok = 0;
        if (ASTKind(a) == AST_AMBIGUITY)
        {
            shape = "ellipse";
            color = "color=\"/dark28/4\",fontcolor=\"/dark28/4\"";
        }
        else if (ASTKind(a) == AST_NODE_LIST)
        {
            list_is_ok = ast_check_list_tree(a);
            if (!list_is_ok)
                shape = "Mdiamond";
        }

        if (ASTKind(a) == AST_NODE_LIST
                && list_is_ok)
        {
            shape="record";
            fprintf(f, "n%zd[%s,shape=%s,label=\"{AST_NODE_LIST\\nNode=%p\\nParent=%p\\n%s|{",
                    current_node, color, shape, a, ASTParent(a), ast_location(a));

            AST it = NULL;
            int i = 0;
            for_each_element(a, it)
            {
                if (i > 0)
                    fprintf(f, "|");
                fprintf(f, "<i%d> %d", i, i);
                i++;
            }

            fprintf(f, "}}\"]\n");

            i = 0;
            for_each_element(a, it)
            {
                AST item = ASTSon1(it);

                ast_dump_graphviz_rec(item, f, /* current_node */ 0, /* position */ 0);

                if (item != NULL)
                {
                    fprintf(f, "n%zd:i%d -> n%zd\n", current_node, i, (size_t)item);
                }
                i++;
            }
        }
        else if (ASTKind(a) != AST_AMBIGUITY)
        {
            fprintf(f, "n%zd[%s,shape=%s,label=\"%s\\nNode=%p\\nParent=%p\\n%s",
                    current_node, color, shape, ast_print_node_type(ASTKind(a)), a, ASTParent(a), ast_location(a));
            if (ASTText(a) != NULL)
            {
                char *quoted = quote_protect(ASTText(a));
                fprintf(f, "\\nText: \\\"%s\\\"", quoted);
                DELETE(quoted);
            }

            type_t* t = nodecl_get_type(_nodecl_wrap(a));
            if (t != NULL)
            {
                char *quoted = quote_protect(print_declarator(t));
                fprintf(f, "\\nType: \\\"%s\\\"", quoted);
                DELETE(quoted);
            }

            fprintf(f, "\"]\n");

            int i;
            for(i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
            {
                if (ASTChild(a, i) != NULL)
                {
                    ast_dump_graphviz_rec(ASTChild(a, i), f, current_node, i);

                    // Print this only for non extended referenced nodes
                    if (ASTChild(a, i) != NULL)
                    {
                        fprintf(f, "n%zd -> n%zd [label=\"%d\"]\n", (size_t)a, (size_t)ASTChild(a, i), i);
                    }

                }
            }
        }
        else if (ASTKind(a) == AST_AMBIGUITY)
        {
            fprintf(f, "n%zd[%s,shape=%s,label=\"%s\\nNode=%p\\nParent=%p\\n%s\"]\n", 
                    current_node, color, shape, ast_print_node_type(ASTKind(a)), a, ASTParent(a), ast_location(a));

            int i;
            for(i = 0; i < ast_get_num_ambiguities(a); i++)
            {
                ast_dump_graphviz_rec(ast_get_ambiguity(a, i), f, current_node, i);

                fprintf(f, "n%zd -> n%zd [label=\"%d\"]\n", (size_t)a, (size_t)ast_get_ambiguity(a, i), i);
            }
        }

        if (ASTKind(a) == NODECL_CONTEXT || 
                ASTKind(a) == NODECL_PRAGMA_CONTEXT)
        {
            int k = decl_context_t_dump_graphviz(f, nodecl_get_decl_context(_nodecl_wrap(a)));

            fprintf(f, "n%zd -> decl_context_%d:context:n [label=\"%s\",fontcolor=\"/dark28/1\",color=\"/dark28/1\"]\n",
                    current_node, k,
                    "context");
        }

        scope_entry_t* entry = nodecl_get_symbol(_nodecl_wrap(a));
        if (entry != NULL)
        {
            symbol_dump_graphviz(f, entry);
            fprintf(f, "n%zd -> sym_%zd [label=\"%s\",fontcolor=\"/dark28/2\",color=\"/dark28/2\"]\n",
                    current_node,
                    (size_t)entry,
                    "sym");
        }

        const_value_t* cval = nodecl_get_constant(_nodecl_wrap(a));
        if (cval != NULL)
        {
            cval_dump_graphviz(f, cval);
            fprintf(f, "n%zd -> const_%zd [label=\"%s\",fontcolor=\"/dark28/5\",color=\"/dark28/5\"]\n",
                    current_node,
                    (size_t)cval,
                    "const");
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
    fprintf(f, "   ordering=out;\n");
    fprintf(f, "   colorscheme=ColorBrewer;\n");
    ast_dump_graphviz_rec(a, f, 0, 0);
    fprintf(f, "}\n");
}
