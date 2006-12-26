#include <string.h>
#include "cxx-utils.h"
#include "cxx-ast.h"
#include "cxx-solvetemplate.h"
#include "cxx-instantiation.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"
#include "cxx-typeutils.h"

#include "cxx-printscope.h"

static decl_context_t build_proper_instantiation_context(decl_context_t decl_context)
{
	return decl_context;
}

static void instantiate_primary_template(scope_entry_t* matched_template,
        scope_entry_t* instance_symbol,
        template_argument_list_t* template_argument_list, 
		scope_t* st, decl_context_t decl_context)
{
    ERROR_CONDITION((matched_template->kind != SK_TEMPLATE_PRIMARY_CLASS), "Unexpected symbol kind '%d'\n", matched_template->kind);

    template_parameter_t** template_parameter_list = matched_template->template_parameter_info;
    int num_template_parameters = matched_template->num_template_parameters;
    int i;

    if (template_argument_list->num_arguments != num_template_parameters)
    {
        internal_error("Number of template arguments vs template parameters does not match %d != %d", 
                template_argument_list->num_arguments, num_template_parameters);
    }

    AST instantiate_tree = matched_template->type_information->type->template_class_body;

    // Now create a new scope and inject template parameters with its argument value
    scope_t* instantiate_scope = new_template_scope(matched_template->scope);

    instantiate_scope->template_scope = matched_template->scope->template_scope;
    matched_template->scope->template_scope = instantiate_scope;
    instance_symbol->related_scope->template_scope = instantiate_scope;

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
                        scope_entry_t* injected_type = new_symbol(instantiate_scope, name);

                        // We use a typedef
                        injected_type->kind = SK_TYPEDEF;

                        injected_type->type_information = calloc(1, sizeof(*(injected_type->type_information)));
                        injected_type->type_information->kind = TK_DIRECT;

                        injected_type->type_information->type = calloc(1, 
                                sizeof(*(injected_type->type_information->type)));
                        injected_type->type_information->type->kind = STK_TYPEDEF;

                        // injected_type->type_information->type->aliased_type = advance_over_typedefs(template_argument->type);
                        injected_type->type_information->type->aliased_type = template_argument->type;
                        break;
                    }
                case TPK_TEMPLATE :
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Injecting template-alias '%s' into the instantiate scope\n", name);
                        }

                        scope_entry_t* injected_type = new_symbol(instantiate_scope, name);

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
                        scope_entry_t* injected_nontype = new_symbol(instantiate_scope, name);
                        injected_nontype->kind = SK_VARIABLE;
                        injected_nontype->type_information = template_parameter->type_info;
                        AST duplicated_tree = duplicate_ast(template_argument->argument_tree);

                        AST fake_initializer = ASTMake1(AST_CONSTANT_INITIALIZER, duplicated_tree, ASTLine(duplicated_tree), NULL);
                        injected_nontype->expression_value = fake_initializer;
                        break;
                    }
                default :
                    internal_error("Unexpected template parameter kind %d\n", template_parameter->kind);
            }
        }
    }

    // Build scope over the new tree
    decl_context_t new_decl_context = build_proper_instantiation_context(decl_context);

    DEBUG_CODE()
    {
        fprintf(stderr, "--------> Building scope of instantiated template '%s'\n", matched_template->symbol_name);
        print_scope(instantiate_scope);
    }

    type_t* simple_type_info = calloc(1, sizeof(*simple_type_info));

    simple_type_info->kind = TK_DIRECT;
    simple_type_info->type = calloc(1, sizeof(*(simple_type_info->type)));
    simple_type_info->type->kind = STK_USER_DEFINED;
    simple_type_info->type->user_defined_type = instance_symbol;

    AST template_class_base_clause = matched_template->type_information->type->template_class_base_clause;

    if (template_class_base_clause != NULL)
    {
        build_scope_base_clause(template_class_base_clause, instance_symbol->scope, instance_symbol->related_scope, 
                instance_symbol->type_information->type->class_info, new_decl_context);
    }

    // Inject the class name
    scope_entry_t* injected_symbol = new_symbol(instance_symbol->related_scope, instance_symbol->symbol_name);

    *injected_symbol = *instance_symbol;

    injected_symbol->do_not_print = 1;
    injected_symbol->injected_class_name = 1;
    injected_symbol->injected_class_referred_symbol = instance_symbol;

    if (instantiate_tree != NULL)
    {
        build_scope_member_specification(instance_symbol->related_scope, instantiate_tree, AS_PUBLIC,
                simple_type_info, new_decl_context);
    }

    instance_symbol->related_scope->template_scope = NULL;
    matched_template->scope->template_scope = instantiate_scope->template_scope;
    instantiate_scope->template_scope = NULL;

    instance_symbol->defined = 1;

    DEBUG_CODE()
    {
        fprintf(stderr, "--------> Instantiation ended\n");
    }
}

static void instantiate_specialized_template(scope_entry_t* matched_template, 
        scope_entry_t* instance_symbol,
        template_argument_list_t* template_argument_list, 
        unification_set_t* unification_set, scope_t* st,
		decl_context_t decl_context)
{
    ERROR_CONDITION((matched_template->kind != SK_TEMPLATE_SPECIALIZED_CLASS), "Unexpected symbol kind '%d'\n", matched_template->kind);

    template_parameter_t** template_parameter_list = matched_template->template_parameter_info;
    int num_template_parameters = matched_template->num_template_parameters;
    int i;

    AST instantiate_tree = matched_template->type_information->type->template_class_body;


    // Now create a new scope and inject template parameters with its argument value
    scope_t* instantiate_scope = new_template_scope(matched_template->scope);

    instantiate_scope->template_scope = matched_template->scope->template_scope;
    matched_template->scope->template_scope = instantiate_scope;
    instance_symbol->related_scope->template_scope = instantiate_scope;

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

                            if (unification_item->parameter_name != NULL)
                            {
                                if (strcmp(unification_item->parameter_name, template_parameter->template_parameter_name) == 0)
                                {
                                    DEBUG_CODE()
                                    {
                                        fprintf(stderr, "Injecting type '%s' into the instantiate scope\n", name);
                                    }
                                    scope_entry_t* injected_type = new_symbol(instantiate_scope, name);

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
                        }
                        break;
                    }
                case TPK_TEMPLATE :
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Injecting template-alias '%s' into the instantiate scope\n", name);
                        }

                        internal_error("Not yet implemented", 0);

                        break;
                    }
                case TPK_NONTYPE :
                    {
                        // Search the name in the unification set
                        int j;
                        for (j = 0; j < unification_set->num_elems; j++)
                        {
                            unification_item_t* unification_item = unification_set->unif_list[j];

                            if (unification_item->parameter_name != NULL)
                            {
                                if (strcmp(unification_item->parameter_name, 
                                            template_parameter->template_parameter_name) == 0)
                                {
                                    DEBUG_CODE()
                                    {
                                        fprintf(stderr, "Injecting nontype '%s' into the instantiate scope\n", name);
                                    }
                                    scope_entry_t* injected_nontype = new_symbol(instantiate_scope, name);
                                    injected_nontype->kind = SK_VARIABLE;
                                    injected_nontype->type_information = template_parameter->type_info;
                                    AST duplicated_tree = duplicate_ast(unification_item->expression);

                                    AST fake_initializer = ASTMake1(AST_CONSTANT_INITIALIZER, duplicated_tree, ASTLine(duplicated_tree), NULL);
                                    injected_nontype->expression_value = fake_initializer;
                                    break;
                                }
                            }
                        }
                        break;
                    }
                default :
                    internal_error("Unexpected template parameter kind %d\n", template_parameter->kind);
            }
        }
    }

    // Build scope over the new tree
    decl_context_t new_decl_context = build_proper_instantiation_context(decl_context);

    DEBUG_CODE()
    {
        fprintf(stderr, "--------> Building scope of instantiated template '%s'\n", matched_template->symbol_name);
        print_scope(instantiate_scope);
    }

    instance_symbol->related_scope->template_scope = matched_template->scope->template_scope;
    matched_template->scope->template_scope = instance_symbol->related_scope;

    type_t* simple_type_info = calloc(1, sizeof(*simple_type_info));

    simple_type_info->kind = TK_DIRECT;
    simple_type_info->type = calloc(1, sizeof(*(simple_type_info->type)));
    simple_type_info->type->kind = STK_USER_DEFINED;
    simple_type_info->type->user_defined_type = instance_symbol;

    AST template_class_base_clause = matched_template->type_information->type->template_class_base_clause;

    if (template_class_base_clause != NULL)
    {
        build_scope_base_clause(template_class_base_clause, instance_symbol->scope, instance_symbol->related_scope, 
                instance_symbol->type_information->type->class_info, new_decl_context);
    }
    
    // Inject the class name
    scope_entry_t* injected_symbol = new_symbol(instance_symbol->related_scope, instance_symbol->symbol_name);

    *injected_symbol = *instance_symbol;

    injected_symbol->do_not_print = 1;
    injected_symbol->injected_class_name = 1;
    injected_symbol->injected_class_referred_symbol = instance_symbol;

    if (instantiate_tree != NULL)
    {
        build_scope_member_specification(instance_symbol->related_scope, instantiate_tree, AS_PUBLIC,
                simple_type_info, new_decl_context);
    }

    instance_symbol->related_scope->template_scope = NULL;
    matched_template->scope->template_scope = instantiate_scope->template_scope;
    instantiate_scope->template_scope = NULL;

    instance_symbol->defined = 1;

    DEBUG_CODE()
    {
        fprintf(stderr, "--------> Instantiation ended\n");
    }
}

static void fill_template_specialized_info(scope_entry_t* instance_symbol, 
        scope_entry_t* matched_template,
        template_argument_list_t* arguments)
{
    int line = instance_symbol->line;
    scope_t* symbol_scope = instance_symbol->scope;
    char* symbol_name = instance_symbol->symbol_name;
    memset(instance_symbol, 0, sizeof(*instance_symbol));
    instance_symbol->symbol_name = symbol_name;
    instance_symbol->scope = symbol_scope;
    instance_symbol->line = line;

    instance_symbol->kind = SK_TEMPLATE_SPECIALIZED_CLASS;
    instance_symbol->type_information = calloc(1, sizeof(*(instance_symbol->type_information)));
    instance_symbol->type_information->kind = TK_DIRECT;
    instance_symbol->type_information->type = calloc(1, sizeof(*(instance_symbol->type_information->type)));
    instance_symbol->type_information->type->kind = STK_CLASS;
    instance_symbol->type_information->type->class_info = calloc(1, 
            sizeof(*(instance_symbol->type_information->type->class_info)));

    instance_symbol->is_member = matched_template->is_member;
    instance_symbol->class_type = matched_template->class_type;

    scope_t* inner_scope = new_class_scope(instance_symbol->scope);

    DEBUG_CODE()
    {
        fprintf(stderr, "New inner_scope %p\n", inner_scope);
    }

    instance_symbol->type_information->type->class_info->inner_scope = inner_scope;
    instance_symbol->related_scope = inner_scope;

    // Save the inner scope in the class type
    // (it is used when checking member acesses)
    instance_symbol->type_information->type->class_info->inner_scope = inner_scope;

    DEBUG_CODE()
    {
        fprintf(stderr, "Symbol '%s' %p set to come from instantiation\n",
                instance_symbol->symbol_name, instance_symbol);
    }

    instance_symbol->type_information->type->from_instantiation = 1;
    instance_symbol->type_information->type->template_arguments = arguments;
}

scope_entry_t* create_holding_symbol_for_template(scope_entry_t* matched_template, template_argument_list_t*
        arguments, scope_t* st, int instantiation_line)
{
    scope_entry_t* instance_symbol = new_symbol(matched_template->scope, matched_template->symbol_name);
    DEBUG_CODE()
    {
        fprintf(stderr, "Creating the holding symbol (%p) for '%s' due to instantiation in line %d\n", 
                instance_symbol,
                matched_template->symbol_name,
                instantiation_line);
    }
    instance_symbol->line = instantiation_line;

    fill_template_specialized_info(instance_symbol, matched_template, arguments);

    // This should not come from instantiation
    DEBUG_CODE()
    {
        fprintf(stderr, "The holding symbol '%s' %p does not come from instantiation\n",
                instance_symbol->symbol_name, instance_symbol);
    }
    instance_symbol->type_information->type->from_instantiation = 0;

    return instance_symbol;
}

void instantiate_template_in_symbol(scope_entry_t* instance_symbol, 
        matching_pair_t* match_pair, template_argument_list_t* arguments, scope_t* st,
		decl_context_t decl_context)
{
    scope_entry_t* matched_template = match_pair->entry;
    unification_set_t* unification_set = match_pair->unif_set;

    fill_template_specialized_info(instance_symbol, matched_template, arguments);

    DEBUG_CODE()
    {
        fprintf(stderr, ">> instantiate_template over given symbol %p -> '%s'\n", instance_symbol, matched_template->symbol_name);
    }

    switch (matched_template->kind)
    {
        case SK_TEMPLATE_PRIMARY_CLASS :
            {
                instantiate_primary_template(matched_template, instance_symbol, 
						arguments, st, decl_context);
                break;
            }
        case SK_TEMPLATE_SPECIALIZED_CLASS :
            {
                instantiate_specialized_template(matched_template, instance_symbol, arguments, 
						unification_set, st, decl_context);
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

void instantiate_template(matching_pair_t* match_pair, template_argument_list_t* arguments, 
		scope_t* st, int instantiation_line, decl_context_t decl_context)
{
    scope_entry_t* matched_template = match_pair->entry;
    // unification_set_t* unification_set = match_pair->unif_set;

    scope_entry_t* instance_symbol = new_symbol(matched_template->scope, matched_template->symbol_name);

    DEBUG_CODE()
    {
        fprintf(stderr, "Creating the instantiated new symbol (%p) due to instantiation in line %d\n", 
                instance_symbol,
                instantiation_line);
    }

    instance_symbol->line = instantiation_line;

    instantiate_template_in_symbol(instance_symbol, match_pair, arguments, st, decl_context);
}
