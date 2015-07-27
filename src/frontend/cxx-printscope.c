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




#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "red_black_tree.h"
#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-prettyprint.h"
#include "cxx-printscope.h"
#include "cxx-entrylist.h"
#include "cxx-typeutils.h"
#include "cxx-codegen.h"

/*
 * Building a symbol table for C++ is such a hard thing that we need ways to debug it.
 */
static void print_scope_full_context(const decl_context_t* decl_context, int global_indent);
static void print_scope_full(scope_t* scope, int global_indent);
static void print_scope_entry_list(const char* key, scope_entry_list_t* entry_list, int global_indent);
static void print_scope_entry(const char* key, scope_entry_t* entry, int global_indent);

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

typedef
struct print_context_data_tag
{
    int global_indent;
    int num_scopes;
    scope_t* scope_set[MCXX_MAX_SCOPES_NESTING];
    rb_red_blk_tree *symbol_set;
} print_context_data_t;

static print_context_data_t print_context_data;

static void null_dtor(const void* v UNUSED_PARAMETER) { }
static int comp_vptr(const void* v1, const void *v2)
{
    if (v1 < v2)
        return -1;
    else if (v1 > v2)
        return 1;
    else 
        return 0;
}

void print_scope(const decl_context_t* decl_context)
{
    memset(&print_context_data, 0, sizeof(print_context_data));
    print_context_data.symbol_set = rb_tree_create(comp_vptr, null_dtor, null_dtor);
    print_scope_full_context(decl_context, 0);
}


static void print_scope_full_context(const decl_context_t* decl_context, int global_indent)
{
    scope_t* st = decl_context->current_scope;
    if (st == NULL)
        return;

    print_scope_full(st, global_indent);

    if (decl_context->function_scope != NULL)
    {
        PRINT_INDENTED_LINE(stderr, global_indent + 1, "[FUNCTION_SCOPE - %p]\n", 
                decl_context->function_scope);
        print_scope_full(decl_context->function_scope, global_indent + 2);
    }
}

static void print_scope_full_aux(const void* key, void* info, void* data)
{
    scope_entry_list_t* entry_list = (scope_entry_list_t*)info;

    print_scope_entry_list((const char*)key, entry_list, *(int*)data);
}

static void print_scope_full(scope_t* st, int global_indent)
{
    int i;
    for (i = 0; i < print_context_data.num_scopes; i++)
    {
        if (print_context_data.scope_set[i] == st)
        {
            PRINT_INDENTED_LINE(stderr, global_indent, "<<Recursive scope not printed>>\n");
            return;
        }
    }

    print_context_data.scope_set[print_context_data.num_scopes] = st;
    print_context_data.num_scopes++;

    dhash_ptr_walk(st->dhash, (dhash_ptr_walk_fn*)print_scope_full_aux, &global_indent);

    print_context_data.num_scopes--;
    print_context_data.scope_set[print_context_data.num_scopes] = NULL;
}

static void print_scope_entry_list(const char* key, scope_entry_list_t* entry_list, int global_indent)
{
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        if (entry->do_not_print)
        {
            continue;
        }
        print_scope_entry(key, entry, global_indent);
    }
    entry_list_iterator_free(it);
}



static void print_scope_entry(const char* key, scope_entry_t* entry, int global_indent)
{
    if (strcmp(key, entry->symbol_name) == 0)
    {
        PRINT_INDENTED_LINE(stderr, global_indent, "* \"%s\" %s", entry->symbol_name, symbol_kind_name(entry));
    }
    else
    {
        PRINT_INDENTED_LINE(stderr, global_indent, "* [ \"%s\" -> ] \"%s\" %s", key, entry->symbol_name, symbol_kind_name(entry));
    }

    if (rb_tree_query(print_context_data.symbol_set, entry) != NULL)
    {
        PRINT_INDENTED_LINE(stderr, 0, " <<<Symbol already printed>>>\n");
        return;
    }

    rb_tree_insert(print_context_data.symbol_set, entry, entry);

    if (entry->defined)
    {
        fprintf(stderr, " [DEFINED]");
    }

    fprintf(stderr, "\n");

    PRINT_INDENTED_LINE(stderr, global_indent+1, "Declared in %s\n", locus_to_str(entry->locus));

    if (entry->kind == SK_UNDEFINED)
    {
        if (entry->type_information != NULL)
        {
            PRINT_INDENTED_LINE(stderr, global_indent+1, "Type: %s\n", 
                    print_declarator(entry->type_information));
        }
    }
    if (entry->kind == SK_VARIABLE)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Type: %s\n", 
                print_declarator(entry->type_information));

        if (symbol_entity_specs_get_is_bitfield(entry))
        {
            PRINT_INDENTED_LINE(stderr, global_indent + 1, "Bitfield of size: %s\n", 
                    codegen_to_str(symbol_entity_specs_get_bitfield_size(entry), nodecl_retrieve_context(symbol_entity_specs_get_bitfield_size(entry))));
        }
    }
    if (entry->kind == SK_TEMPLATE_NONTYPE_PARAMETER
            || entry->kind == SK_TEMPLATE_TYPE_PARAMETER
            || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Type: %s\n", 
                print_type_str(get_user_defined_type(entry), entry->decl_context));
    }
    if (entry->kind == SK_VARIABLE
            && symbol_is_parameter_of_function(entry,
                entry->decl_context->current_scope->related_entry))
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Is parameter the function\n");
    }

    if (entry->kind == SK_TEMPLATE)
    {
        PRINT_INDENTED_LINE(stderr, global_indent + 1, "Related specializations:\n");
        int i;
        for (i = 0; i < template_type_get_num_specializations(entry->type_information); i++)
        {
            type_t* named_type = template_type_get_specialization_num(entry->type_information, i);
            scope_entry_t* specialization = named_type_get_symbol(named_type);

            PRINT_INDENTED_LINE(stderr, global_indent + 1, "Specialization: [%d] %p\n", i, specialization->type_information);

            print_scope_entry(specialization->symbol_name, specialization, global_indent + 1);
        }

    }

    if (entry->kind == SK_CLASS)
    {
        print_scope_full_context(class_type_get_inner_context(entry->type_information),
                global_indent + 1);
    }

    if (entry->kind == SK_NAMESPACE)
    {
        print_scope_full_context(entry->related_decl_context, global_indent + 1);
    }

    if (entry->type_information != NULL 
            && is_template_specialized_type(entry->type_information))
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Type is specialized:\n");

#if 0
        template_parameter_list_t* arguments = 
            template_specialized_type_get_template_parameters(entry->type_information);

        if (arguments != NULL)
        {
            int j;
            for (j = 0; j < arguments->num_arguments; j++)
            {
                template_parameter_t* current_argument = arguments->argument_list[j];

                char* argument_kind[] =
                {
                    [TAK_NONTYPE] = "nontype template argument",
                    [TAK_TYPE] = "type template argument",
                    [TAK_TEMPLATE] = "template template argument",
                };

                const char *template_arg_info = NULL;

                if (current_argument->kind == TAK_TYPE
                        || current_argument->kind == TAK_TEMPLATE)
                {
                    template_arg_info = print_declarator(current_argument->type);
                }
                else if (current_argument->kind == TAK_NONTYPE)
                {
                    template_arg_info = prettyprint_in_buffer(current_argument->expression);
                }

                PRINT_INDENTED_LINE(stderr, global_indent+2, "[%d] : %s - %s\n", 
                        j,
                        argument_kind[current_argument->kind],
                        template_arg_info);
            }
        }
        else
        {
            PRINT_INDENTED_LINE(stderr, global_indent + 2, "%s", "Invalid template arguments!!!\n");
        }
#endif

        if (is_class_type(entry->type_information))
        {
            type_t* actual = get_actual_class_type(entry->type_information);
            if (class_type_is_complete_dependent(actual))
            {
                PRINT_INDENTED_LINE(stderr, global_indent + 1, "%s", "Complete dependent\n");
            }
            else if (class_type_is_complete_independent(actual))
            {
                PRINT_INDENTED_LINE(stderr, global_indent + 1, "%s", "Complete independent\n");
            }
            else if (class_type_is_incomplete_independent(actual))
            {
                PRINT_INDENTED_LINE(stderr, global_indent + 1, "%s", "Incomplete independent\n");
            }
            else if (class_type_is_incomplete_dependent(actual))
            {
                PRINT_INDENTED_LINE(stderr, global_indent + 1, "%s", "Incomplete dependent\n");
            }
            else
            {
                PRINT_INDENTED_LINE(stderr, global_indent + 1, "%s", "No template nature known\n");
            }
        }
    }

    if (entry->kind == SK_TYPEDEF)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1,  "Aliased type: %s\n",
                print_declarator(entry->type_information));
    }

    if (entry->kind == SK_GCC_BUILTIN_TYPE)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "(builtin type)\n");
    }

    if (entry->kind == SK_ENUMERATOR)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Type: %s\n",
                print_declarator(entry->type_information));
    }

    if ((entry->kind == SK_VARIABLE || entry->kind == SK_ENUMERATOR)
            && !nodecl_is_null(entry->value))
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Expression value: %s\n",
                codegen_to_str(entry->value, nodecl_retrieve_context(entry->value)));
    }

    if (entry->kind == SK_FUNCTION)
    {
        if (!is_computed_function_type(entry->type_information))
        {
            PRINT_INDENTED_LINE(stderr, global_indent+1, "Prototype: %s\n",
                    print_declarator(entry->type_information));
            C_LANGUAGE()
            {
                if (function_type_get_lacking_prototype(entry->type_information))
                {
                    PRINT_INDENTED_LINE(stderr, global_indent+1, "This function does not have prototype yet\n");
                }
            }
            CXX_LANGUAGE()
            {
                if (symbol_entity_specs_get_is_conversion(entry))
                {
                    PRINT_INDENTED_LINE(stderr, global_indent+1, "Conversion function\n");
                }

                int i;
                for (i = 0; i < symbol_entity_specs_get_num_parameters(entry); i++)
                {
                    if (symbol_entity_specs_get_default_argument_info_num(entry, i) != NULL)
                    {
                        PRINT_INDENTED_LINE(stderr, global_indent + 1, "Default argument for parameter '%d' is '%s' \n", 
                                i,
                                codegen_to_str(symbol_entity_specs_get_default_argument_info_num(entry, i)->argument, 
                                    nodecl_retrieve_context(symbol_entity_specs_get_default_argument_info_num(entry, i)->argument)));
                    }
                }
            }
        }
        else 
        {
            PRINT_INDENTED_LINE(stderr, global_indent+1, "Computed function type\n");
        }
    }

    if (symbol_entity_specs_get_is_trivial(entry))
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Is trivial\n");
    }

    if (symbol_entity_specs_get_is_deleted(entry))
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Is deleted\n");
    }

    if (symbol_entity_specs_get_is_member(entry))
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Is member of '%s'\n",
                print_declarator(symbol_entity_specs_get_class_type(entry)));
        if (entry->kind == SK_VARIABLE
                && !symbol_entity_specs_get_is_static(entry))
        {
            PRINT_INDENTED_LINE(stderr, global_indent+1, "Offset of nonstatic member (at byte boundary): %zd\n",
                    symbol_entity_specs_get_field_offset(entry));
            if (symbol_entity_specs_get_is_bitfield(entry))
            {
                PRINT_INDENTED_LINE(stderr, global_indent+1, "First bit: %d\n",
                        symbol_entity_specs_get_bitfield_first(entry));
                PRINT_INDENTED_LINE(stderr, global_indent+1, "Last bit: %d\n",
                        symbol_entity_specs_get_bitfield_last(entry));
            }
        }
    }

    if (symbol_entity_specs_get_is_static(entry))
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Is static\n");
    }
    if (symbol_entity_specs_get_is_conversion(entry))
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Is conversion\n");
    }
    if (symbol_entity_specs_get_is_friend_declared(entry))
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Is friend-declared symbol\n");
    }
    
    if(entry->kind == SK_CLASS)
    {
         scope_entry_list_t* friends_list = class_type_get_friends(entry->type_information);
         if(friends_list != NULL)
         {
             scope_entry_list_iterator_t* it = NULL;
             PRINT_INDENTED_LINE(stderr, global_indent+1, "Friends: ");
             for (it = entry_list_iterator_begin(friends_list);
                     !entry_list_iterator_end(it);
                     entry_list_iterator_next(it))
             {
                 scope_entry_t* current_friend = entry_list_iterator_current(it);
                 if(current_friend->symbol_name != NULL) 
                 {
                     PRINT_INDENTED_LINE(stderr,0,"\"%s\" %s, ",
                             current_friend->symbol_name, symbol_kind_name(current_friend));
                 }
                 else
                 {
                     PRINT_INDENTED_LINE(stderr,0,"\"UNNAMED\" %s, ",
                             symbol_kind_name(current_friend));
                 }
             }
             PRINT_INDENTED_LINE(stderr, global_indent+1, "\n");
             entry_list_iterator_free(it);
         }
    }
    if (entry->kind == SK_PROGRAM
            || entry->kind == SK_FUNCTION
            || entry->kind == SK_MODULE
            || entry->kind == SK_GENERIC_NAME)
    {
        print_scope_full_context(entry->related_decl_context, global_indent + 1);
    }
    if (symbol_entity_specs_get_num_related_symbols(entry) != 0)
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "Related symbols of this symbol\n");
        int i;
        for (i = 0; i < symbol_entity_specs_get_num_related_symbols(entry); i++)
        {
            scope_entry_t* related_entry = symbol_entity_specs_get_related_symbols_num(entry, i);
            if (related_entry != NULL)
            {
                PRINT_INDENTED_LINE(stderr, global_indent+1, "[%d] \"%s\" at %s\n",
                        i, related_entry->symbol_name, locus_to_str(related_entry->locus));
            }
            else
            {
                PRINT_INDENTED_LINE(stderr, global_indent+1, "[%d] NULL SYMBOL!!!\n", i);
            }
        }
    }
    else
    {
        PRINT_INDENTED_LINE(stderr, global_indent+1, "No related symbols\n");
    }
}
