#include "cxx-koenig.h"
#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-typecalc.h"
#include "cxx-utils.h"
#include "cxx-driver.h"
#include "hash_iterator.h"

/*
 * This file implements Koenig lookup, technically named "argument dependent lookup"
 */

static void compute_associated_namespaces_and_classes(scope_t* st, AST arguments, 
		scope_t** associated_namespaces, int* num_associated_namespaces,
		scope_t** associated_classes, int* num_associated_classes);

static void compute_associated_namespaces_and_classes_of_type(scope_t* st, 
		type_t* type_info, 
		scope_t** associated_namespaces, int* num_associated_namespaces,
		scope_t** associated_classes, int* num_associated_classes);


scope_entry_list_t* lookup_unqualified_function(scope_t* st, char* name, AST arguments)
{
	scope_entry_list_t* ordinary_lookup = query_unqualified_name(st, name);

	if (ordinary_lookup != NULL
			&& ordinary_lookup->entry->kind == SK_FUNCTION)
	{
		// Check it is a class member function
		scope_entry_t* entry = ordinary_lookup->entry;

		if (entry->scope->kind == CLASS_SCOPE)
		{
			// This function is in a class scope, thus it is a member function
			// standard says that associated namespaces should not be considered now
			return ordinary_lookup;
		}
	}

	scope_entry_list_t* result = ordinary_lookup;

	// Now construct the set of associated namespaces and classes
	int num_associated_namespaces = 0;
	scope_t** associated_namespaces = NULL;
	int num_associated_classes = 0;
	scope_t** associated_classes = NULL;
	compute_associated_namespaces_and_classes(st, arguments, associated_namespaces, &num_associated_namespaces,
			associated_classes, &num_associated_classes);

	int i;
	for (i = 0; i < num_associated_namespaces; i++)
	{
		scope_entry_list_t* current_symbols;
		scope_t* associated = associated_namespaces[i];
		current_symbols = query_in_symbols_of_scope(st, name);

		result = append_scope_entry_lists(result, current_symbols);
	}

	for (i = 0; i < num_associated_classes; i++)
	{
		scope_entry_list_t* current_symbols;
		scope_t* associated = associated_classes[i];
		current_symbols = query_in_symbols_of_scope(st, name);

		result = append_scope_entry_lists(result, current_symbols);
	}

	return result;
}

static void compute_associated_classes_and_namespaces_of_template_scope(
		scope_t* st,
		scope_t* template_scope,
		scope_t** associated_namespaces, int* num_associated_namespaces,
		scope_t** associated_classes, int* num_associated_classes)
{
	// This is a bit ugly, but must be made this way
	//
	// Iterate every symbol defined in this template scope
	Iterator* it = (Iterator*) hash_iterator_create(template_scope->hash);

	for ( iterator_first(it); 
			!iterator_finished(it); 
			iterator_next(it))
	{
		scope_entry_list_t* entry_list = (scope_entry_list_t*) iterator_item(it);

		// There should not be repeated symbols here
		scope_entry_t* entry = entry_list->entry;

		if (entry->kind != SK_TEMPLATE_PARAMETER)
		{
			internal_error("Unexpected symbol kind %d", entry->kind);
		}

#warning Consider template arguments
	}
}


static void compute_associated_classes_of_bases(
		int num_bases, base_class_info_t** base_class_list,
	    scope_t** associated_classes, int* num_associated_classes)
{
	int i;
	for (i = 0; i < num_bases; i++)
	{
		base_class_info_t* base_class_info = base_class_list[i];
		type_t* base_class_type = base_class_info->class_type;

		if (base_class_type->kind != TK_DIRECT
				|| base_class_type->type->kind != STK_USER_DEFINED)
		{
			internal_error("A user defined name class was expected", 0);
		}

		base_class_type = base_class_type->type->user_defined_type->type_information;

		if (base_class_type->kind != TK_DIRECT
				|| base_class_type->type->kind != STK_CLASS)
		{
			internal_error("This is not a class!", 0);
		}
		simple_type_t* base_simple_class_type = base_class_type->type;

		// The class itself
		P_LIST_ADD_ONCE(associated_classes, *num_associated_classes, base_simple_class_type->class_info->inner_scope);

		// And its bases
		compute_associated_classes_of_bases(
				base_simple_class_type->class_info->num_bases,
				base_simple_class_type->class_info->base_classes_list,
				associated_classes, num_associated_classes);
	}
}

static void compute_associated_namespaces_and_classes_of_direct_type(
		scope_t* st,
		type_t* direct_type,
		scope_t** associated_namespaces, int* num_associated_namespaces,
		scope_t** associated_classes, int* num_associated_classes)
{
	if (direct_type->kind != TK_DIRECT)
	{
		internal_error("This routine expects a direct type", 0);
	}

	simple_type_t* simple_type = direct_type->type;

	switch (simple_type->kind)
	{
		case STK_BUILTIN_TYPE :
			{
				// Empty set of associated namespaces and classes
				break;
			}
		case STK_USER_DEFINED :
			{
				// Will be the user defined type
				compute_associated_namespaces_and_classes_of_type(st, 
						simple_type->user_defined_type->type_information, 
						associated_namespaces, num_associated_namespaces,
						associated_classes, num_associated_classes);
				break;
			}
		case STK_TYPEDEF :
			{
				// Will be the aliased type
				compute_associated_namespaces_and_classes_of_type(st, 
						simple_type->aliased_type, 
						associated_namespaces, num_associated_namespaces,
						associated_classes, num_associated_classes);
				break;
			}
		case STK_ENUM :
			{
				// Get the scope of the definition of the enum
				scope_t* definition_scope = simple_type->type_scope;
				if (definition_scope->kind == CLASS_SCOPE)
				{
					P_LIST_ADD_ONCE(associated_classes, *num_associated_classes, definition_scope);

					// Now move the scope to the first enclosing namespace scope (jump over
					// template scopes and classes)
					definition_scope = definition_scope->contained_in;

					while (definition_scope != NULL && 
							(definition_scope->kind == TEMPLATE_SCOPE
							 || definition_scope->kind == CLASS_SCOPE))
					{
						if (definition_scope->kind == TEMPLATE_SCOPE)
						{
							compute_associated_classes_and_namespaces_of_template_scope(st,
									definition_scope,
									associated_namespaces, num_associated_namespaces,
									associated_classes, num_associated_classes);

						}
						definition_scope = definition_scope->contained_in;
					}
				}

				if (definition_scope->kind == NAMESPACE_SCOPE)
				{
					if (definition_scope != compilation_options.global_scope)
					{
						P_LIST_ADD_ONCE(associated_namespaces, *num_associated_namespaces, definition_scope);
					}
				}
				else
				{
					internal_error("Unexpected scope kind %d", definition_scope->kind);
				}
				break;
			}
		case STK_CLASS :
			{
				// The class itself
				P_LIST_ADD_ONCE(associated_classes, *num_associated_classes, simple_type->class_info->inner_scope);

				scope_t** base_associated_classes = NULL;
				int num_base_associated_classes = 0;

				// Direct and indirect bases
				compute_associated_classes_of_bases(
						simple_type->class_info->num_bases, simple_type->class_info->base_classes_list,
						base_associated_classes, &num_base_associated_classes);

				// For every associated class compute the namespaces where they are defined
				int i;
				for (i = 0; i < num_base_associated_classes; i++)
				{
					scope_t* base_associated_class = base_associated_classes[i];

					scope_t* base_associated_namespace = base_associated_class->contained_in;

					while (base_associated_namespace != NULL
							&& (base_associated_namespace->kind == TEMPLATE_SCOPE
								|| base_associated_namespace->kind == CLASS_SCOPE))
					{
						if (base_associated_namespace->kind == TEMPLATE_SCOPE)
						{
							compute_associated_classes_and_namespaces_of_template_scope(st,
									base_associated_namespace,
									associated_namespaces, num_associated_namespaces,
									associated_classes, num_associated_classes);

						}
						base_associated_namespace = base_associated_namespace->contained_in;
					}

					if (base_associated_namespace->kind == NAMESPACE_SCOPE)
					{
						if (base_associated_namespace != compilation_options.global_scope)
						{
							P_LIST_ADD_ONCE(associated_namespaces, *num_associated_namespaces, base_associated_namespace);
						}
					}
					else
					{
						internal_error("Unexpected scope kind %d", base_associated_namespace->kind);
					}
				}

				break;
			}
		default :
			{
				internal_error("Invalid simple type kind %d\n", direct_type->kind);
			}
	}
}

static void compute_associated_namespaces_and_classes_of_type(scope_t* st, 
		type_t* type_info, 
		scope_t** associated_namespaces, int* num_associated_namespaces,
		scope_t** associated_classes, int* num_associated_classes)
{
	switch (type_info->kind)
	{
		case TK_DIRECT :
			{
				compute_associated_namespaces_and_classes_of_direct_type(st, type_info,
						associated_namespaces, num_associated_namespaces,
						associated_classes, num_associated_classes);
				break;
			}
		case TK_POINTER :
		case TK_REFERENCE :
			{
				compute_associated_namespaces_and_classes_of_type(st, 
						type_info->pointer->pointee,
						associated_namespaces, num_associated_namespaces,
						associated_classes, num_associated_classes);
				break;
			}
		case TK_ARRAY :
			{
				compute_associated_namespaces_and_classes_of_type(st,
						type_info->array->element_type,
						associated_namespaces, num_associated_namespaces,
						associated_classes, num_associated_classes);
				break;
			}
		case TK_POINTER_TO_MEMBER :
			{
				// The member type
				compute_associated_namespaces_and_classes_of_type(st,
						type_info->pointer->pointee,
						associated_namespaces, num_associated_namespaces,
						associated_classes, num_associated_classes);
				
				// The related class
				compute_associated_namespaces_and_classes_of_type(st,
						type_info->pointer->pointee_class->type_information,
						associated_namespaces, num_associated_namespaces,
						associated_classes, num_associated_classes);
				break;
			}
		default :
			{
				break;
			}
	}
}

void compute_associated_namespaces_and_classes(scope_t* st, AST arguments, 
		scope_t** associated_namespaces, int* num_associated_namespaces,
		scope_t** associated_classes, int* num_associated_classes)
{
	AST iter;
	for_each_element(arguments, iter)
	{
		AST arg_expr = ASTSon1(iter);

		calculated_type_t* calc_type = calculate_expression_type(arg_expr, st);

		if (calc_type->num_types != 1)
		{
			internal_error("Unsupported overloading in argument %d", calc_type->num_types);
		}

		compute_associated_namespaces_and_classes_of_type(st, calc_type->types[0],
				associated_namespaces, num_associated_namespaces,
				associated_classes, num_associated_classes);
	}
}
