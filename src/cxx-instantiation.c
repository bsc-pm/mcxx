#include <string.h>
#include "cxx-utils.h"
#include "cxx-ast.h"
#include "cxx-solvetemplate.h"
#include "cxx-instantiation.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"

#include "cxx-printscope.h"

static void instantiate_incomplete_primary_template(scope_entry_t* matched_template,
		template_argument_list_t* template_argument_list, scope_t* st)
{
	// This routine is planned for a future where we will be able to
	// "pre-instantiate" things
}

static void instantiate_primary_template(scope_entry_t* matched_template,
		scope_entry_t* instance_symbol,
		template_argument_list_t* template_argument_list, scope_t* st)
{
	if (matched_template->kind != SK_TEMPLATE_PRIMARY_CLASS)
	{
		internal_error("Unexpected symbol kind '%d'\n", matched_template->kind);
	}

	template_parameter_t** template_parameter_list = matched_template->template_parameter_info;
	int num_template_parameters = matched_template->num_template_parameters;
	int i;

	if (template_argument_list->num_arguments != num_template_parameters)
	{
		internal_error("Number of template arguments vs template parameters does not match %d != %d", 
				template_argument_list->num_arguments, num_template_parameters);
	}

	AST instantiate_tree = matched_template->type_information->type->template_class_body;

	if (instantiate_tree == NULL)
	{
		fprintf(stderr, "This instantiation refers to an incomplete type\n");
		instantiate_incomplete_primary_template(matched_template, template_argument_list, st);
		return;
	}

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
				case TPK_TEMPLATE :
					{
						fprintf(stderr, "Injecting type '%s' into the instantiate scope\n", name);
						scope_entry_t* injected_type = new_symbol(instantiate_scope, name);

						// We use a typedef
						injected_type->kind = SK_TYPEDEF;

						injected_type->type_information = GC_CALLOC(1, sizeof(*(injected_type->type_information)));
						injected_type->type_information->kind = TK_DIRECT;

						injected_type->type_information->type = GC_CALLOC(1, 
								sizeof(*(injected_type->type_information->type)));
						injected_type->type_information->type->kind = STK_TYPEDEF;

						if (!template_argument->implicit)
						{
							injected_type->type_information->type->aliased_type = template_argument->type;
						}
						else
						{
							// Construct the type
							AST default_arg_type_spec_seq = ASTSon0(template_parameter->default_tree);
							// This declarator can be null
							AST default_arg_declarator = ASTSon1(template_parameter->default_tree);

							type_t* type_info = NULL;
							gather_decl_spec_t gather_info;
							memset(&gather_info, 0, sizeof(gather_info));

							build_scope_decl_specifier_seq(default_arg_type_spec_seq, matched_template->scope, 
									&gather_info, &type_info,
									default_decl_context);

							if (default_arg_declarator != NULL)
							{
								type_t* declarator_type = NULL;
								build_scope_declarator(default_arg_declarator, matched_template->scope, &gather_info, type_info, &declarator_type,
										default_decl_context);
								injected_type->type_information->type->aliased_type = declarator_type;
							}
							else
							{
								injected_type->type_information->type->aliased_type = type_info;
							}

						}
						break;
					}
				case TPK_NONTYPE :
					{
						fprintf(stderr, "Injecting nontype '%s' into the instantiate scope\n", name);
						scope_entry_t* injected_nontype = new_symbol(instantiate_scope, name);
						injected_nontype->kind = SK_VARIABLE;
						injected_nontype->type_information = template_parameter->type_info;
						injected_nontype->expression_value = template_argument->argument_tree;
						break;
					}
				default :
					internal_error("Unexpected template parameter kind %d\n", template_parameter->kind);
			}
		}
	}

	// Build scope over the new tree
	decl_context_t decl_context;
	memset(&decl_context, 0, sizeof(decl_context));

	fprintf(stderr, "--------> Building scope of instantiated template '%s'\n", matched_template->symbol_name);
	print_scope(instantiate_scope, 0);

	type_t* simple_type_info = GC_CALLOC(1, sizeof(*simple_type_info));

	simple_type_info->kind = TK_DIRECT;
	simple_type_info->type = GC_CALLOC(1, sizeof(*(simple_type_info->type)));
	simple_type_info->type->kind = STK_USER_DEFINED;
	simple_type_info->type->user_defined_type = instance_symbol;


	build_scope_member_specification(instance_symbol->related_scope, instantiate_tree, AS_PUBLIC,
			simple_type_info, decl_context);

	instance_symbol->related_scope->template_scope = NULL;
	matched_template->scope->template_scope = instantiate_scope->template_scope;
	instantiate_scope->template_scope = NULL;

	instance_symbol->defined = 1;

	fprintf(stderr, "--------> Instantiation ended\n");
}

static void instantiate_incomplete_specialized_template(scope_entry_t* matched_template, template_argument_list_t* template_argument_list, 
		unification_set_t* unification_set, scope_t* st)
{
	// This routine is planned for a future where we will be able to
	// "pre-instantiate" things
}

static void instantiate_specialized_template(scope_entry_t* matched_template, 
		scope_entry_t* instance_symbol,
		template_argument_list_t* template_argument_list, 
		unification_set_t* unification_set, scope_t* st)
{
	if (matched_template->kind != SK_TEMPLATE_SPECIALIZED_CLASS)
	{
		internal_error("Unexpected symbol kind '%d'\n", matched_template->kind);
	}

	template_parameter_t** template_parameter_list = matched_template->template_parameter_info;
	int num_template_parameters = matched_template->num_template_parameters;
	int i;

	AST instantiate_tree = matched_template->type_information->type->template_class_body;

	if (instantiate_tree == NULL)
	{
		fprintf(stderr, "This instantiation refers to an incomplete type\n");
		instantiate_incomplete_specialized_template(matched_template, template_argument_list, unification_set, st);
		return;
	}

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
				case TPK_TEMPLATE :
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
									fprintf(stderr, "Injecting type '%s' into the instantiate scope\n", name);
									scope_entry_t* injected_type = new_symbol(instantiate_scope, name);

									// We use a typedef
									injected_type->kind = SK_TYPEDEF;

									injected_type->type_information = GC_CALLOC(1, sizeof(*(injected_type->type_information)));
									injected_type->type_information->kind = TK_DIRECT;

									injected_type->type_information->type = GC_CALLOC(1, 
											sizeof(*(injected_type->type_information->type)));
									injected_type->type_information->type->kind = STK_TYPEDEF;
									injected_type->type_information->type->aliased_type = unification_item->value;
									break;
								}
							}
						}
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
									fprintf(stderr, "Injecting nontype '%s' into the instantiate scope\n", name);
									scope_entry_t* injected_nontype = new_symbol(instantiate_scope, name);
									injected_nontype->kind = SK_VARIABLE;
									injected_nontype->type_information = template_parameter->type_info;
									injected_nontype->expression_value = unification_item->expression;
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
	decl_context_t decl_context;
	memset(&decl_context, 0, sizeof(decl_context));

	fprintf(stderr, "--------> Building scope of instantiated template '%s'\n", matched_template->symbol_name);
	print_scope(instantiate_scope, 0);

	instance_symbol->related_scope->template_scope = matched_template->scope->template_scope;
	matched_template->scope->template_scope = instance_symbol->related_scope;

	type_t* simple_type_info = GC_CALLOC(1, sizeof(*simple_type_info));

	simple_type_info->kind = TK_DIRECT;
	simple_type_info->type = GC_CALLOC(1, sizeof(*(simple_type_info->type)));
	simple_type_info->type->kind = STK_USER_DEFINED;
	simple_type_info->type->user_defined_type = instance_symbol;


	build_scope_member_specification(instance_symbol->related_scope, instantiate_tree, AS_PUBLIC,
			simple_type_info, decl_context);

	instance_symbol->related_scope->template_scope = NULL;
	matched_template->scope->template_scope = instantiate_scope->template_scope;
	instantiate_scope->template_scope = NULL;

	instance_symbol->defined = 1;

	fprintf(stderr, "--------> Instantiation ended\n");
}

void instantiate_template_in_symbol(scope_entry_t* instance_symbol, 
		matching_pair_t* match_pair, template_argument_list_t* arguments, scope_t* st)
{
	scope_entry_t* matched_template = match_pair->entry;
	unification_set_t* unification_set = match_pair->unif_set;

	scope_t* symbol_scope = instance_symbol->scope;
	char* symbol_name = instance_symbol->symbol_name;
	memset(instance_symbol, 0, sizeof(*instance_symbol));
	instance_symbol->symbol_name = symbol_name;
	instance_symbol->scope = symbol_scope;

	instance_symbol->kind = SK_TEMPLATE_SPECIALIZED_CLASS;
	instance_symbol->type_information = GC_CALLOC(1, sizeof(*(instance_symbol->type_information)));
	instance_symbol->type_information->kind = TK_DIRECT;
	instance_symbol->type_information->type = GC_CALLOC(1, sizeof(*(instance_symbol->type_information->type)));
	instance_symbol->type_information->type->kind = STK_CLASS;
	instance_symbol->type_information->type->class_info = GC_CALLOC(1, 
			sizeof(*(instance_symbol->type_information->type->class_info)));

	scope_t* inner_scope = new_class_scope(instance_symbol->scope);
	fprintf(stderr, "New inner_scope %p\n", inner_scope);
	instance_symbol->type_information->type->class_info->inner_scope = inner_scope;
	instance_symbol->related_scope = inner_scope;

	// Save the inner scope in the class type
	// (it is used when checking member acesses)
	instance_symbol->type_information->type->class_info->inner_scope = inner_scope;
	instance_symbol->type_information->type->from_instantiation = 1;

	instance_symbol->type_information->type->template_arguments = arguments;

	fprintf(stderr, ">> instantiate_template over given symbol %p-> '%s'\n", instance_symbol, matched_template->symbol_name);

	switch (matched_template->kind)
	{
		case SK_TEMPLATE_PRIMARY_CLASS :
			{
				instantiate_primary_template(matched_template, instance_symbol, arguments, st);
				break;
			}
		case SK_TEMPLATE_SPECIALIZED_CLASS :
			{
				instantiate_specialized_template(matched_template, instance_symbol, arguments, unification_set, st);
				break;
			}
		default :
			{
				internal_error("Unexpected kind %d\n", matched_template->kind);
			}
	}
	fprintf(stderr, "<< instantiate_template -> '%s'\n", matched_template->symbol_name);
}

void instantiate_template(matching_pair_t* match_pair, template_argument_list_t* arguments, scope_t* st)
{
	scope_entry_t* matched_template = match_pair->entry;
	// unification_set_t* unification_set = match_pair->unif_set;

	fprintf(stderr, "Creating the instantiated new symbol\n");
	scope_entry_t* instance_symbol = new_symbol(matched_template->scope, matched_template->symbol_name);

	instantiate_template_in_symbol(instance_symbol, match_pair, arguments, st);
}
