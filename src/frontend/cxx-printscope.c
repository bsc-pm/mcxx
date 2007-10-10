/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-prettyprint.h"
#include "cxx-printscope.h"
#include "hash_iterator.h"
#include "cxx-typeutils.h"

/*
 * Building a symbol table for C++ is such a hard thing that we need ways to debug it.
 */
static void print_scope_full_context(decl_context_t decl_context, int global_indent);
static void print_scope_full(scope_t* scope, int global_indent);
static void print_scope_entry_list(scope_entry_list_t* entry_list, int global_indent);
static void print_scope_entry(scope_entry_t* entry, int global_indent);

static void indent_at_level(FILE* f, int n)
{
    int i;
    for (i = 0; i < 4*n; i++) 
    { 
        fprintf(f, " "); 
    } 
}

#define PRINT_INDENTED_LINE(f, n, ...) \
    do { \
        indent_at_level(f, n); \
        fprintf(f,  __VA_ARGS__ ); \
    } while (0);

static char* symbol_kind_names[] =
{
    [SK_UNDEFINED] = "SK_UNDEFINED",
    [SK_CLASS] = "SK_CLASS",
    [SK_ENUM] = "SK_ENUM",
    [SK_ENUMERATOR] = "SK_ENUMERATOR",
    [SK_FUNCTION] = "SK_FUNCTION",
    [SK_LABEL] = "SK_LABEL",
    [SK_NAMESPACE] = "SK_NAMESPACE",
    [SK_VARIABLE] = "SK_VARIABLE",
    [SK_TYPEDEF] = "SK_TYPEDEF",
    [SK_TEMPLATE_PRIMARY_CLASS] = "SK_TEMPLATE_PRIMARY_CLASS",
    [SK_TEMPLATE_SPECIALIZED_CLASS] = "SK_TEMPLATE_SPECIALIZED_CLASS",
    [SK_TEMPLATE_FUNCTION] = "SK_TEMPLATE_FUNCTION",
    [SK_TEMPLATE_ALIAS] = "SK_TEMPLATE_ALIAS",
    [SK_TEMPLATE_PARAMETER] = "SK_TEMPLATE_PARAMETER", 
    [SK_TEMPLATE_TYPE_PARAMETER] = "SK_TEMPLATE_TYPE_PARAMETER", 
    [SK_TEMPLATE_TEMPLATE_PARAMETER] = "SK_TEMPLATE_TEMPLATE_PARAMETER", 
    // GCC Extension for builtin types
    [SK_GCC_BUILTIN_TYPE] = "SK_GCC_BUILTIN_TYPE",
};

// static char* scope_names[] =
// {
//     [UNDEFINED_SCOPE] = "UNDEFINED_SCOPE",
//     [NAMESPACE_SCOPE] = "NAMESPACE_SCOPE",
//     [FUNCTION_SCOPE] = "FUNCTION_SCOPE",
//     [PROTOTYPE_SCOPE] = "PROTOTYPE_SCOPE",
//     [BLOCK_SCOPE] = "BLOCK_SCOPE",
//     [CLASS_SCOPE] = "CLASS_SCOPE",
//     [TEMPLATE_SCOPE] = "TEMPLATE_SCOPE",
// };

void print_scope(decl_context_t decl_context)
{
    print_scope_full_context(decl_context, 0);
}

static void print_scope_full_context(decl_context_t decl_context, int global_indent)
{
    scope_t* st = decl_context.current_scope;

    if (st == NULL)
        return;

    print_scope_full(st, global_indent);

    scope_t* template_scope = decl_context.template_scope;
    int k = 1;
    while (template_scope != NULL)
    {
        PRINT_INDENTED_LINE(stderr, global_indent + k, "[TEMPLATE_SCOPE - %p]\n", 
               template_scope);
        print_scope_full(template_scope, global_indent + k + 1);

        ++k;
        template_scope = template_scope->contained_in;
    }
    if (decl_context.function_scope != NULL)
    {
        PRINT_INDENTED_LINE(stderr, global_indent + 1, "[FUNCTION_SCOPE - %p]\n", 
                decl_context.function_scope);
        print_scope_full(decl_context.function_scope, global_indent + 2);
    }
}

static void print_scope_full(scope_t* st, int global_indent)
{
    Iterator *it;
    
    it = (Iterator*) hash_iterator_create(st->hash);
    for ( iterator_first(it); 
            !iterator_finished(it); 
            iterator_next(it))
    {
        scope_entry_list_t* entry_list = (scope_entry_list_t*) iterator_item(it);

        print_scope_entry_list(entry_list, global_indent);
    }
}

static void print_scope_entry_list(scope_entry_list_t* entry_list, int global_indent)
{
    while (entry_list != NULL)
    {
        if (entry_list->entry->do_not_print)
        {
            entry_list = entry_list->next;
            continue;
        }
        scope_entry_t* entry = entry_list->entry;
        print_scope_entry(entry, global_indent);

        print_scope_full_context(entry->related_decl_context, global_indent + 1);

        entry_list = entry_list->next;
    }
}



static void print_scope_entry(scope_entry_t* entry, int global_indent)
{
    PRINT_INDENTED_LINE(stderr, global_indent, "* \"%s\" %s", entry->symbol_name, symbol_kind_names[entry->kind]);

    if (entry->defined)
    {
        fprintf(stderr, " [DEFINED]");
    }

    fprintf(stderr, "\n");

    PRINT_INDENTED_LINE(stderr, global_indent+1, "Declared in %s:%d\n", entry->file, entry->line);

    if (entry->kind == SK_VARIABLE)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Type: %s\n", 
                print_declarator(entry->type_information, entry->decl_context));
    }
    if (entry->kind == SK_TEMPLATE_PARAMETER
            || entry->kind == SK_TEMPLATE_TYPE_PARAMETER
            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Type: %s\n", 
                get_named_type_name(entry));
    }
    if (entry->kind == SK_VARIABLE && entry->entity_specs.is_parameter)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Is parameter %d of the function\n",
                entry->entity_specs.parameter_position);
    }

    if (entry->kind == SK_TYPEDEF)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1,  "Aliased type: %s\n",
                print_declarator(typedef_type_get_aliased_type(entry->type_information), entry->decl_context));
    }

    if (entry->kind == SK_GCC_BUILTIN_TYPE)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "(builtin type)\n");
    }

    if (entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS
            || entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Template: %p\n", entry);
    }

    if (entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS
            || entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
    {
        if (class_type_is_complete_dependent(entry->type_information))
        {
            PRINT_INDENTED_LINE(stderr, global_indent+1, "Dependent complete template\n");
        }
        else if (class_type_is_complete_independent(entry->type_information))
        {
            PRINT_INDENTED_LINE(stderr, global_indent+1, "Independent complete template\n");
        }
        else if (class_type_is_incomplete_dependent(entry->type_information))
        {
            PRINT_INDENTED_LINE(stderr, global_indent+1, "Dependent incomplete template\n");
        }
        else if (class_type_is_incomplete_independent(entry->type_information))
        {
            PRINT_INDENTED_LINE(stderr, global_indent+1, "Independent incomplete template\n");
        }
        else
        {
            PRINT_INDENTED_LINE(stderr, global_indent+1, "Template nature unknown\n");
        }
    }

    if (entry->kind == SK_ENUMERATOR)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Type: %s\n",
                print_declarator(entry->type_information, entry->decl_context));
    }

    if ((entry->kind == SK_VARIABLE || entry->kind == SK_ENUMERATOR)
            && entry->expression_value != NULL)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Expression value: %s\n",
                prettyprint_in_buffer(entry->expression_value));
    }

    if (entry->kind == SK_FUNCTION
            || entry->kind == SK_TEMPLATE_FUNCTION)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Prototype: %s\n",
                print_declarator(entry->type_information, entry->decl_context));
        // print_scope_full(entry->related_decl_context.current_scope, global_indent+1);
        C_LANGUAGE()
        {
            if (function_type_get_lacking_prototype(entry->type_information))
            {
                PRINT_INDENTED_LINE(stderr, global_indent+1, "This function does not have prototype yet\n");
            }
        }
        CXX_LANGUAGE()
        {
            if (entry->entity_specs.is_conversion)
            {
                PRINT_INDENTED_LINE(stderr, global_indent+1, "Conversion function\n");
            }
        }
        if (!entry->defined)
        {
            print_scope_full(entry->decl_context.prototype_scope, global_indent+1);
        }
    }

    if (entry->entity_specs.is_member)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Is member\n");
    }
}
