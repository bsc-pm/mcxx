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
#include <string.h>
#include "cxx-utils.h"
#include "cxx-ast.h"
#include "cxx-solvetemplate.h"
#include "cxx-instantiation.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"
#include "cxx-typeutils.h"
#include "cxx-cexpr.h"

#include "cxx-printscope.h"

static void instantiate_primary_template(scope_entry_t* matched_template,
        scope_entry_t* instance_symbol,
        template_argument_list_t* template_argument_list, 
        decl_context_t decl_context)
{
    ERROR_CONDITION((matched_template->kind != SK_TEMPLATE_PRIMARY_CLASS), "Unexpected symbol kind '%d'\n", matched_template->kind);
    
    // Fix the template context, this symbol has one less template scope than the matching
    // template.
    instance_symbol->decl_context.template_scope = instance_symbol->decl_context.template_scope->contained_in;

    template_parameter_t** template_parameter_list = matched_template->template_parameter_info;
    int num_template_parameters = matched_template->num_template_parameters;
    int i;

    if (template_argument_list->num_arguments != num_template_parameters)
    {
        internal_error("Number of template arguments vs template parameters does not match %d != %d", 
                template_argument_list->num_arguments, num_template_parameters);
    }

    AST instantiate_tree = duplicate_ast(matched_template->type_information->type->template_class_body);

    // Now create a new class context and inject template parameters with its argument value
    decl_context_t new_related_decl_context = instance_symbol->related_decl_context;
    new_related_decl_context = new_template_context(new_related_decl_context);

    for (i = 0; i < num_template_parameters; i++)
    {
        template_argument_t* template_argument = template_argument_list->argument_list[i];
        template_parameter_t* template_parameter = template_parameter_list[i];
        char* name = template_parameter->template_parameter_name;

        if (name != NULL)
        {
            switch (template_parameter->kind)
            {
                case TPK_TYPE :
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Injecting type '%s' into the instantiate scope\n", name);
                        }
                        // Note that we sign in the symbol in template_scope and not in current_scope
                        scope_entry_t* injected_type = new_symbol(new_related_decl_context, 
                                new_related_decl_context.template_scope, name);

                        // We use a typedef
                        injected_type->kind = SK_TYPEDEF;

                        injected_type->type_information = calloc(1, sizeof(*(injected_type->type_information)));
                        injected_type->type_information->kind = TK_DIRECT;

                        injected_type->type_information->type = calloc(1, 
                                sizeof(*(injected_type->type_information->type)));
                        injected_type->type_information->type->kind = STK_TYPEDEF;

                        injected_type->type_information->type->aliased_type = template_argument->type;
                        break;
                    }
                case TPK_TEMPLATE :
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Injecting template-alias '%s' into the instantiate scope\n", name);
                        }

                        // Note that we sign in the symbol in template_scope and not in current_scope
                        scope_entry_t* injected_type = new_symbol(new_related_decl_context, 
                                new_related_decl_context.template_scope, name);

                        injected_type->kind = SK_TEMPLATE_ALIAS;

                        injected_type->template_alias_type = template_argument->type;
                        break;
                    }
                case TPK_NONTYPE :
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Injecting nontype '%s' into the instantiate scope\n", name);
                        }
                        
                        // Note that we sign in the symbol in template_scope and not in current_scope
                        scope_entry_t* injected_nontype = new_symbol(new_related_decl_context, 
                                new_related_decl_context.template_scope, name);
                        injected_nontype->kind = SK_VARIABLE;
                        injected_nontype->type_information = template_argument->type;

                        // Fold it, as it makes thing easier later
                        literal_value_t literal_value = evaluate_constant_expression(template_argument->argument_tree, 
                                template_argument->decl_context);
                        AST evaluated_tree = tree_from_literal_value(literal_value);
                        AST fake_initializer = ASTMake1(AST_CONSTANT_INITIALIZER, evaluated_tree, ASTLine(evaluated_tree), NULL);
                        injected_nontype->expression_value = fake_initializer;
                        break;
                    }
                default :
                    internal_error("Unexpected template parameter kind %d\n", template_parameter->kind);
            }
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "--------> Building scope of instantiated template '%s'\n", matched_template->symbol_name);
        print_scope(new_related_decl_context);
    }

    instance_symbol->related_decl_context = new_related_decl_context;

    type_t* simple_type_info = calloc(1, sizeof(*simple_type_info));

    simple_type_info->kind = TK_DIRECT;
    simple_type_info->type = calloc(1, sizeof(*(simple_type_info->type)));
    simple_type_info->type->kind = STK_USER_DEFINED;
    simple_type_info->type->user_defined_type = instance_symbol;

    AST template_class_base_clause = matched_template->type_information->type->template_class_base_clause;

    if (template_class_base_clause != NULL)
    {
        build_scope_base_clause(template_class_base_clause, 
                instance_symbol->type_information->type->class_info, new_related_decl_context);
    }

    // Inject the class name
    scope_entry_t* injected_symbol = new_symbol(instance_symbol->related_decl_context, 
            instance_symbol->related_decl_context.current_scope, instance_symbol->symbol_name);

    *injected_symbol = *instance_symbol;

    injected_symbol->do_not_print = 1;
    injected_symbol->injected_class_name = 1;
    injected_symbol->injected_class_referred_symbol = instance_symbol;

    if (instantiate_tree != NULL)
    {
        // Fix this 'AS_PUBLIC' one day
        build_scope_member_specification(instance_symbol->related_decl_context, instantiate_tree, AS_PUBLIC,
                simple_type_info);
    }

    instance_symbol->defined = 1;

    DEBUG_CODE()
    {
        fprintf(stderr, "--------> Instantiation ended\n");
    }
}

static void instantiate_specialized_template(scope_entry_t* matched_template, 
        scope_entry_t* instance_symbol,
        template_argument_list_t* template_argument_list, 
        unification_set_t* unification_set,
        decl_context_t decl_context)
{
    ERROR_CONDITION((matched_template->kind != SK_TEMPLATE_SPECIALIZED_CLASS), "Unexpected symbol kind '%d'\n", matched_template->kind);
    
    // Fix the template context, this symbol has one less template scope than the matching
    // template.
    instance_symbol->decl_context.template_scope = instance_symbol->decl_context.template_scope->contained_in;

    template_parameter_t** template_parameter_list = matched_template->template_parameter_info;
    int num_template_parameters = matched_template->num_template_parameters;
    int i;

    AST instantiate_tree = duplicate_ast(matched_template->type_information->type->template_class_body);

    // Now create a new class context and inject template parameters with its argument value
    decl_context_t new_related_decl_context = instance_symbol->related_decl_context;
    new_related_decl_context = new_template_context(new_related_decl_context);

    DEBUG_CODE()
    {
        fprintf(stderr, "Injecting %d template parameters\n", num_template_parameters);
    }

    for (i = 0; i < num_template_parameters; i++)
    {
        template_parameter_t* template_parameter = template_parameter_list[i];
        char* name = template_parameter->template_parameter_name;

        if (name != NULL)
        {
            switch (template_parameter->kind)
            {
                case TPK_TYPE :
                    {
                        // Search the name in the unification set
                        int j;
                        for (j = 0; j < unification_set->num_elems; j++)
                        {
                            unification_item_t* unification_item = unification_set->unif_list[j];

                            type_t* template_parameter_type = template_parameter->type_info;

                            if ((unification_item->parameter_num == template_parameter_type->type->template_parameter_num)
                                    && (unification_item->parameter_nesting == template_parameter_type->type->template_parameter_nesting))
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "Injecting type '%s' into the instantiate scope\n", name);
                                }

                                // Note that we sign in the symbol in template_scope and not in current_scope
                                scope_entry_t* injected_type = new_symbol(new_related_decl_context, new_related_decl_context.template_scope, name);

                                // We use a typedef
                                injected_type->kind = SK_TYPEDEF;

                                injected_type->type_information = calloc(1, sizeof(*(injected_type->type_information)));
                                injected_type->type_information->kind = TK_DIRECT;

                                injected_type->type_information->type = calloc(1, 
                                        sizeof(*(injected_type->type_information->type)));
                                injected_type->type_information->type->kind = STK_TYPEDEF;
                                injected_type->type_information->type->aliased_type = unification_item->value;
                                break;
                            }
                        }
                        break;
                    }
                case TPK_TEMPLATE :
                    {
                        // Fix this one day, because the number of things that can be specialized for
                        // this kind of parameters are just template-id, so it seems not difficult to do
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Injecting template-alias '%s' into the instantiate scope\n", name);
                        }

                        internal_error("Injecting template-alias in partial specializations not yet implemented", 0);
                        break;
                    }
                case TPK_NONTYPE :
                    {
                        // Search the name in the unification set
                        int j;
                        for (j = 0; j < unification_set->num_elems; j++)
                        {
                            unification_item_t* unification_item = unification_set->unif_list[j];

                            type_t* template_parameter_type = template_parameter->type_info;

                            if ((unification_item->parameter_num == template_parameter_type->type->template_parameter_num)
                                    && (unification_item->parameter_nesting == template_parameter_type->type->template_parameter_nesting))
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "Injecting nontype '%s' into the instantiate scope\n", name);
                                }
                                // Note that we sign in the symbol in template_scope and not in current_scope
                                scope_entry_t* injected_nontype = new_symbol(new_related_decl_context, new_related_decl_context.template_scope, name);
                                injected_nontype->kind = SK_VARIABLE;
                                // FIXME this type should be in the unification item
                                // and not from the parameter
                                injected_nontype->type_information = template_parameter->type_info;

                                // Fold it, as makes thing easier
                                literal_value_t literal_value = evaluate_constant_expression(unification_item->expression, 
                                        unification_item->decl_context);
                                AST evaluated_tree = tree_from_literal_value(literal_value);
                                AST fake_initializer = ASTMake1(AST_CONSTANT_INITIALIZER, evaluated_tree, ASTLine(evaluated_tree), NULL);

                                injected_nontype->expression_value = fake_initializer;
                                break;
                            }
                        }
                        break;
                    }
                default :
                    internal_error("Unexpected template parameter kind %d\n", template_parameter->kind);
            }
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Template parameters injected\n");
    }


    DEBUG_CODE()
    {
        fprintf(stderr, "--------> Building scope of instantiated template '%s'\n", matched_template->symbol_name);
        print_scope(new_related_decl_context);
    }

    instance_symbol->related_decl_context = new_related_decl_context;

    type_t* simple_type_info = calloc(1, sizeof(*simple_type_info));

    simple_type_info->kind = TK_DIRECT;
    simple_type_info->type = calloc(1, sizeof(*(simple_type_info->type)));
    simple_type_info->type->kind = STK_USER_DEFINED;
    simple_type_info->type->user_defined_type = instance_symbol;

    AST template_class_base_clause = matched_template->type_information->type->template_class_base_clause;

    if (template_class_base_clause != NULL)
    {
        build_scope_base_clause(template_class_base_clause, 
                instance_symbol->type_information->type->class_info, instance_symbol->related_decl_context);
    }
    
    // Inject the class name
    scope_entry_t* injected_symbol = new_symbol(instance_symbol->related_decl_context, 
            instance_symbol->related_decl_context.current_scope, instance_symbol->symbol_name);

    *injected_symbol = *instance_symbol;

    injected_symbol->do_not_print = 1;
    injected_symbol->injected_class_name = 1;
    injected_symbol->injected_class_referred_symbol = instance_symbol;

    if (instantiate_tree != NULL)
    {
        // Fix this AS_PUBLIC one day
        build_scope_member_specification(instance_symbol->related_decl_context, instantiate_tree, AS_PUBLIC,
                simple_type_info);
    }

    // The symbol is defined after this
    instance_symbol->defined = 1;

    DEBUG_CODE()
    {
        fprintf(stderr, "--------> Instantiation ended\n");
    }
}

static void fill_template_specialized_info(scope_entry_t* instance_symbol, 
        matching_pair_t* matching_pair,
        template_argument_list_t* arguments)
{
    scope_entry_t* matched_template = matching_pair->entry;

    instance_symbol->kind = SK_TEMPLATE_SPECIALIZED_CLASS;
    instance_symbol->type_information = calloc(1, sizeof(*(instance_symbol->type_information)));
    instance_symbol->type_information->kind = TK_DIRECT;
    instance_symbol->type_information->type = calloc(1, sizeof(*(instance_symbol->type_information->type)));
    instance_symbol->type_information->type->kind = STK_CLASS;
    instance_symbol->type_information->type->type_decl_context = instance_symbol->decl_context;
    instance_symbol->type_information->type->class_info = calloc(1, 
            sizeof(*(instance_symbol->type_information->type->class_info)));
    instance_symbol->type_information->type->matching_pair = matching_pair;

    instance_symbol->is_member = matched_template->is_member;
    instance_symbol->class_type = matched_template->class_type;

    char* qualification_name = matched_template->symbol_name;


    // Create its new class context
    decl_context_t class_context = new_class_context(instance_symbol->decl_context, qualification_name);

    DEBUG_CODE()
    {
        fprintf(stderr, "New inner_scope %p with qualification name '%s'\n", class_context.current_scope, qualification_name);
    }

    instance_symbol->type_information->type->class_info->inner_decl_context = class_context;
    instance_symbol->related_decl_context = class_context;

    DEBUG_CODE()
    {
        fprintf(stderr, "Symbol '%s' %p set to come from instantiation\n",
                instance_symbol->symbol_name, instance_symbol);
    }

    // State as instantiated
    instance_symbol->type_information->type->template_nature = TPN_COMPLETE_INDEPENDENT;
    instance_symbol->type_information->type->template_arguments = arguments;
}

scope_entry_t* create_holding_symbol_for_template(matching_pair_t* matched_template, template_argument_list_t*
        arguments, int instantiation_line, char* instantiation_file, decl_context_t decl_context)
{
    scope_entry_t* matched_template_entry = matched_template->entry;
    decl_context_t matched_entry_context = matched_template_entry->decl_context;

    scope_entry_t* instance_symbol = new_symbol(matched_entry_context, 
            matched_entry_context.current_scope, matched_template_entry->symbol_name);

    DEBUG_CODE()
    {
        fprintf(stderr, "Creating the holding symbol (%p) for '%s' due to instantiation in line %d\n", 
                instance_symbol,
                matched_template_entry->symbol_name,
                instantiation_line);
    }
    instance_symbol->line = instantiation_line;
    instance_symbol->file = instantiation_file;

    // This must be fixed for explicit specializations since they have one less template nesting
    // than their primary template. When the created symbol is used for actual instantiation
    // this should already be zero. When the created symbol is just a dependent specialization
    // this can be nonzero.
    instance_symbol->decl_context.template_nesting = decl_context.template_nesting;

    fill_template_specialized_info(instance_symbol, matched_template, arguments);

    // This should not come from instantiation
    if (is_dependent_type(instance_symbol->type_information, decl_context))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "The holding symbol '%s' line %d %p does not come from instantiation and is dependent\n",
                    instance_symbol->symbol_name, instantiation_line, instance_symbol);
        }
        instance_symbol->type_information->type->template_nature = TPN_INCOMPLETE_DEPENDENT;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "The holding symbol '%s' line %d %p does not come from instantiation and is NOT dependent\n",
                    instance_symbol->symbol_name, instantiation_line, instance_symbol);
        }
        instance_symbol->type_information->type->template_nature = TPN_INCOMPLETE_INDEPENDENT;
    }

    return instance_symbol;
}

static void instantiate_template_in_symbol(scope_entry_t* instance_symbol, 
        matching_pair_t* match_pair, template_argument_list_t* arguments, 
        decl_context_t decl_context)
{
    scope_entry_t* matched_template = match_pair->entry;
    unification_set_t* unification_set = match_pair->unif_set;

    fill_template_specialized_info(instance_symbol, match_pair, arguments);

    DEBUG_CODE()
    {
        fprintf(stderr, ">> instantiate_template over given symbol %p -> '%s'\n", instance_symbol, matched_template->symbol_name);
    }
    
    switch (matched_template->kind)
    {
        case SK_TEMPLATE_PRIMARY_CLASS :
            {
                instantiate_primary_template(matched_template, instance_symbol, 
                        arguments, decl_context);
                break;
            }
        case SK_TEMPLATE_SPECIALIZED_CLASS :
            {
                instantiate_specialized_template(matched_template, instance_symbol, arguments, 
                        unification_set, decl_context);
                break;
            }
        default :
            {
                internal_error("Unexpected kind %d\n", matched_template->kind);
            }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "<< instantiate_template -> '%s'\n", matched_template->symbol_name);
    }
}

// Instantiates a symbol if needed
void instantiate_template(scope_entry_t* entry, decl_context_t decl_context)
{
    if (entry->kind != SK_TEMPLATE_SPECIALIZED_CLASS)
    {
        internal_error("Symbol '%s' is not a specialized template", entry->symbol_name);
    }
            
    if (entry->type_information->type->template_nature != TPN_INCOMPLETE_INDEPENDENT)
    {
        internal_error("Symbol '%s' is not a specialized template suitable for full instantiation", 
                entry->symbol_name);
    }
            
    instantiate_template_in_symbol(entry, entry->type_information->type->matching_pair, 
            entry->type_information->type->template_arguments, decl_context);
}
