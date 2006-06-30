#include <string.h>
#include "cxx-utils.h"
#include "cxx-ast.h"
#include "cxx-solvetemplate.h"
#include "cxx-instantiation.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"

#include "cxx-printscope.h"

scope_entry_t* instantiate_primary_template(scope_entry_t* matched_template,
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
		// internal_error("Cannot instantiate an incomplete type\n", 0);
		return matched_template;
	}

	// Wrap the tree
	instantiate_tree = ASTMake3(AST_DECL_SPECIFIER_SEQ, NULL, 
			duplicate_ast(instantiate_tree), NULL, ASTLine(instantiate_tree), NULL);


	// Now create a new scope and inject template parameters with its argument value
	scope_t* instantiate_scope = new_namespace_scope(st);

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
						injected_type->type_information->type->aliased_type = template_argument->type;
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
	gather_decl_spec_t gather_info;
	memset(&gather_info, 0, sizeof(gather_info));

	simple_type_t* simple_type_info = NULL;

	decl_context_t decl_context;
	memset(&decl_context, 0, sizeof(decl_context));

	decl_context.decl_flags |= DF_INSTANTIATION;

	fprintf(stderr, "--------> Building scope of instantiated template\n");

	build_scope_decl_specifier_seq(instantiate_tree, instantiate_scope, &gather_info, &simple_type_info, decl_context);

	print_scope(instantiate_scope, 0);

	// Get the newly declarated symbol
	scope_entry_list_t* instantiated_template_list = query_in_symbols_of_scope(instantiate_scope, matched_template->symbol_name);

	if (instantiated_template_list == NULL)
	{
		internal_error("The instantiated template does not appear in the instantiate scope\n", 0);
	}

	scope_entry_t* instantiated_template = instantiated_template_list->entry;

	// Make it look like a specialized template
	instantiated_template->kind = SK_TEMPLATE_SPECIALIZED_CLASS;
	instantiated_template->type_information->type->template_arguments = template_argument_list;

	// And inject in the scope of the original template class
	insert_entry(matched_template->scope, instantiated_template);

	return instantiated_template;
}

scope_entry_t* instantiate_specialized_template(scope_entry_t* matched_template, template_argument_list_t* template_argument_list, 
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
		// internal_error("Cannot instantiate an incomplete type\n", 0);
		return matched_template;
	}

	// Remove template-id from the class-name
	instantiate_tree = duplicate_ast(instantiate_tree);

	// instantiate_tree is an AST_CLASS_SPECIFIER
	AST class_head = ASTSon0(instantiate_tree);

	if (ASTType(class_head) != AST_CLASS_HEAD)
	{
		internal_error("Expecting a class-head here\n", 0);
	}

	// class_name should be a template-id
	AST class_name = ASTSon2(class_head);
	if (ASTType(class_name) != AST_TEMPLATE_ID)
	{
		internal_error("Expecting a template-id here!\n", 0);
	}

	// Convert it into a symbol
	ASTType(class_name) = AST_SYMBOL;
	ASTText(class_name) = ASTText(ASTSon0(class_name));
	ASTLineLval(class_name) = ASTLine(ASTSon0(class_name));
	ASTNumChildren(class_name) = 0;
	ASTSon0(class_name) = NULL;
	ASTSon1(class_name) = NULL;

	// Wrap the tree
	instantiate_tree = ASTMake3(AST_DECL_SPECIFIER_SEQ, NULL, 
			instantiate_tree, NULL, ASTLine(instantiate_tree), NULL);

	// Now create a new scope and inject template parameters with its argument value
	scope_t* instantiate_scope = new_namespace_scope(st);

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
	gather_decl_spec_t gather_info;
	memset(&gather_info, 0, sizeof(gather_info));

	simple_type_t* simple_type_info = NULL;

	decl_context_t decl_context;
	memset(&decl_context, 0, sizeof(decl_context));

	decl_context.decl_flags |= DF_INSTANTIATION;

	fprintf(stderr, "--------> Building scope of instantiated template\n");

	build_scope_decl_specifier_seq(instantiate_tree, instantiate_scope, &gather_info, &simple_type_info, decl_context);

	print_scope(instantiate_scope, 0);

	// Get the newly declarated symbol
	scope_entry_list_t* instantiated_template_list = query_in_symbols_of_scope(instantiate_scope, matched_template->symbol_name);

	if (instantiated_template_list == NULL)
	{
		internal_error("The instantiated template does not appear in the instantiate scope\n", 0);
	}

	scope_entry_t* instantiated_template = instantiated_template_list->entry;

	// Make it look like a specialized template
	instantiated_template->kind = SK_TEMPLATE_SPECIALIZED_CLASS;
	instantiated_template->type_information->type->template_arguments = template_argument_list;

	// And inject in the scope of the original template class
	insert_entry(matched_template->scope, instantiated_template);

	return instantiated_template;
}

scope_entry_t* instantiate_template(scope_entry_t* matched_template, template_argument_list_t* arguments, 
		unification_set_t* unification_set, scope_t* st)
{
	switch (matched_template->kind)
	{
		case SK_TEMPLATE_PRIMARY_CLASS :
			{
				return instantiate_primary_template(matched_template, arguments, st);
			}
		case SK_TEMPLATE_SPECIALIZED_CLASS :
			{
				return instantiate_specialized_template(matched_template, arguments, unification_set, st);
			}
		default :
			{
				internal_error("Unexpected kind %d\n", matched_template->kind);
			}
	}
}
