#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gc.h>
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-driver.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-solvetemplate.h"
#include "cxx-instantiation.h"
#include "cxx-prettyprint.h"
#include "cxx-cexpr.h"
#include "cxx-ambiguity.h"
#include "hash.h"

static scope_t* new_scope(void)
{
	scope_t* sc = GC_CALLOC(1, sizeof(*sc));
	sc->hash = hash_create(HASH_SIZE, HASHFUNC(prime_hash), KEYCMPFUNC(strcmp));

	return sc;
}

// Creates a new namespace scope, a new global scope is created by just
// passing a NULL enclosing namespace
scope_t* new_namespace_scope(scope_t* enclosing_scope)
{
	scope_t* result = new_scope();

	result->kind = NAMESPACE_SCOPE;
	result->contained_in = enclosing_scope;

	result->template_scope = (enclosing_scope != NULL) ? enclosing_scope->template_scope : NULL;

	return result;
}

// Creates a new prototype scope
scope_t* new_prototype_scope(scope_t* enclosing_scope)
{
	scope_t* result = new_scope();
	result->kind = PROTOTYPE_SCOPE;

	result->contained_in = enclosing_scope;

	result->template_scope = (enclosing_scope != NULL) ? enclosing_scope->template_scope : NULL;

	return result;
}

// Creates a new block scope
scope_t* new_block_scope(scope_t* enclosing_scope, scope_t* prototype_scope, scope_t* function_scope)
{
	scope_t* result = new_scope();

	result->kind = BLOCK_SCOPE;
	result->prototype_scope = prototype_scope;
	result->function_scope = function_scope;
	result->contained_in = enclosing_scope;

	result->template_scope = (enclosing_scope != NULL) ? enclosing_scope->template_scope : NULL;
	
	// Create artificial entry for the block scope
	static int scope_number = 1000;
	char* c = GC_CALLOC(256, sizeof(char));
	sprintf(c, "(block scope #%05d)", scope_number);
	scope_number++;

	scope_entry_t* new_block_scope_entry = new_symbol(enclosing_scope, c);
	new_block_scope_entry->kind = SK_SCOPE;

	new_block_scope_entry->related_scope = result;
	
	return result;
}

// Creates a new function scope
scope_t* new_function_scope(scope_t* enclosing_scope, scope_t* prototype_scope)
{
	scope_t* result = new_scope();
	
	result->kind = FUNCTION_SCOPE;
	result->prototype_scope = prototype_scope;
	result->contained_in = enclosing_scope;

	result->template_scope = (enclosing_scope != NULL) ? enclosing_scope->template_scope : NULL;
	
	return result;
}

// Creates a new class scope
scope_t* new_class_scope(scope_t* enclosing_scope)
{
	scope_t* result = new_scope();

	result->kind = CLASS_SCOPE;

	result->contained_in = enclosing_scope;

	result->template_scope = (enclosing_scope != NULL) ? enclosing_scope->template_scope : NULL;
	
	return result;
}

scope_t* new_template_scope(scope_t* enclosing_scope)
{
	scope_t* result = new_scope();

	result->kind = TEMPLATE_SCOPE;
	// result->contained_in = enclosing_scope;
	// result->template_scope = (enclosing_scope != NULL) ? enclosing_scope->template_scope : NULL;

	return result;
}

scope_entry_t* new_symbol(scope_t* sc, char* name)
{
	scope_entry_list_t* result_set = (scope_entry_list_t*) hash_get(sc->hash, name);

	scope_entry_t* result;

	result = GC_CALLOC(1, sizeof(*result));
	result->symbol_name = GC_STRDUP(name);
	result->scope = sc;

	if (result_set != NULL)
	{
		scope_entry_list_t* new_set = (scope_entry_list_t*) GC_CALLOC(1, sizeof(*new_set));

		// Put the new entry in front of the previous
		*new_set = *result_set;

		result_set->next = new_set;
		result_set->entry = result;
	}
	else
	{
		result_set = (scope_entry_list_t*) GC_CALLOC(1, sizeof(*result_set));
		result_set->entry = result;
		result_set->next = NULL; // redundant, though

		hash_put(sc->hash, name, result_set);
	}

	return result;
}

scope_entry_list_t* query_in_symbols_of_scope(scope_t* sc, char* name)
{
	fprintf(stderr, "Looking in symbols of scope %p -> '%s'...", sc, name);
	scope_entry_list_t* result = (scope_entry_list_t*) hash_get(sc->hash, name);

	if (result == NULL)
	{
		fprintf(stderr, "not found\n");
	}
	else
	{
		fprintf(stderr, "found\n");
	}

	return result;
}

/*
 * Insert entry in the scope
 */
void insert_entry(scope_t* sc, scope_entry_t* entry)
{
	if (entry->symbol_name == NULL)
	{
		internal_error("Inserting a symbol entry without name!", 0);
	}
	
	scope_entry_list_t* result_set = (scope_entry_list_t*) hash_get(sc->hash, entry->symbol_name);


	if (result_set != NULL)
	{
		scope_entry_list_t* new_set = (scope_entry_list_t*) GC_CALLOC(1, sizeof(*new_set));

		// Put the new entry in front of the previous
		*new_set = *result_set;

		result_set->next = new_set;
		result_set->entry = entry;
	}
	else
	{
		result_set = (scope_entry_list_t*) GC_CALLOC(1, sizeof(*result_set));
		result_set->entry = entry;
		result_set->next = NULL; // redundant, though

		hash_put(sc->hash, entry->symbol_name, result_set);
	}
}


void remove_entry(scope_t* sc, scope_entry_t* entry)
{
	if (entry->symbol_name == NULL)
	{
		internal_error("Removing a symbol entry without name!", 0);
	}

	scope_entry_list_t* result_set = (scope_entry_list_t*) hash_get(sc->hash, entry->symbol_name);

	scope_entry_list_t* current = result_set;
	scope_entry_list_t* previous = NULL;

	while (current != NULL)
	{
		if (current->entry == entry)
		{
			if (previous != NULL)
			{
				// Unlink from the structure
				previous->next = current->next;
			}
			else
			{
				// Delete the whole entry
				hash_delete(sc->hash, entry->symbol_name);
			}
			break;
		}

		previous = current;
		current = current->next;
	}
}

/*
 * Returns the scope of this nested name specification
 */
scope_t* query_nested_name_spec(scope_t* sc, AST global_op, AST nested_name,
        scope_entry_list_t** result_entry_list, char* is_dependent)
{
	return query_nested_name_spec_flags(sc, global_op, nested_name, result_entry_list, is_dependent, LF_NONE);
}

scope_t* query_nested_name_spec_flags(scope_t* sc, AST global_op, AST
        nested_name, scope_entry_list_t** result_entry_list, char*
        is_dependent, lookup_flags_t lookup_flags)
{
	int qualif_level = 0;
	// We'll start the search in "sc"
	scope_t* lookup_scope = sc;
	scope_entry_list_t* entry_list = NULL;

	*is_dependent = 0;

	// We've been told to look up in the first enclosing namespace scope
	if (BITMAP_TEST(lookup_flags, LF_IN_NAMESPACE_SCOPE))
	{
		lookup_scope = copy_scope(enclosing_namespace_scope(lookup_scope));

		if (lookup_scope == NULL)
		{
			internal_error("Namespace scope not found!\n", 0);
		}

		lookup_scope->template_scope = sc->template_scope;
	}

	// unless we are told to start in the global scope
	if (global_op != NULL)
	{
		lookup_scope = compilation_options.global_scope;
	}

	lookup_scope = copy_scope(lookup_scope);
	lookup_scope->template_scope = sc->template_scope;

	char seen_class = 0;
	// Traverse the qualification tree
	while (nested_name != NULL)
	{
		AST nested_name_spec = ASTSon0(nested_name);

		switch (ASTType(nested_name_spec))
		{
			case AST_SYMBOL :
				{
					// Do nothing if we have seen that this is dependent
					if (*is_dependent)
					{
						break;
					}

					if (qualif_level == 0)
					{
						entry_list = query_unqualified_name(lookup_scope, ASTText(nested_name_spec));
					}
					else
					{
						entry_list = query_in_symbols_of_scope(lookup_scope, ASTText(nested_name_spec));
					}

					// If not found, null
					if (entry_list == NULL)
					{
						return NULL;
					}

					// Now filter for SK_CLASS or SK_NAMESPACE
					enum cxx_symbol_kind filter[5] = {SK_CLASS, SK_NAMESPACE, SK_TYPEDEF, 
						SK_TEMPLATE_TYPE_PARAMETER, SK_TEMPLATE_TEMPLATE_PARAMETER};
					entry_list = filter_symbol_kind_set(entry_list, 5, filter);

					if (entry_list == NULL)
					{
						return NULL;
					}

					scope_entry_t* entry = entry_list->entry;

					if (entry->kind == SK_TYPEDEF)
					{
						// Advance over typedefs
						type_t* aliased_type = entry->type_information;
						aliased_type = advance_over_typedefs(aliased_type);

						// Now get the entry_t*
						// If this is a typedef it can only be a typedef against a type
						// that should be a class
						if (aliased_type->kind != TK_DIRECT
								|| aliased_type->type->kind != STK_USER_DEFINED)
						{
							return NULL;
						}
						entry = aliased_type->type->user_defined_type;

						if (entry == NULL)
						{
							return NULL;
						}

						// We need to instantiate it
						if (!BITMAP_TEST(lookup_flags, LF_EXPRESSION))
						{
							if (entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS
									&& !entry->type_information->type->from_instantiation)
							{
								// Instantiation happenning here
								fprintf(stderr, "Instantiation of '%s' within qualified name lookup\n", entry->symbol_name);
								scope_entry_list_t* candidates = query_in_symbols_of_scope(entry->scope, entry->symbol_name);

								candidates = filter_entry_from_list(candidates, entry);

								template_argument_list_t* current_template_arguments = entry->type_information->type->template_arguments;

								matching_pair_t* matched_template = solve_template(candidates,
										current_template_arguments, entry->scope, 0);

								instantiate_template_in_symbol(entry, matched_template, current_template_arguments, entry->scope);
							}
						}
						else
						{
							if ((entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS
										|| entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
									&& !entry->type_information->type->from_instantiation)
							{
								*is_dependent = 1;
								return NULL;
							}
						}
					}

					if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
							|| entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
					{
						*is_dependent = 1;
						break;
					}

					// Classes do not have namespaces within them
					if (seen_class && (entry->kind == SK_NAMESPACE))
					{
						return NULL;
					}

					// Once seen a class no more namespaces can appear
					if (entry->kind == SK_CLASS)
					{
						seen_class = 1;
					}

					// It looks fine, update the scope
					scope_t* previous_scope = lookup_scope;
					lookup_scope = copy_scope(entry->related_scope);

					// This can be null if the type is incomplete and we are
					// declaring a pointer to member
					if (lookup_scope != NULL)
					{
						lookup_scope->template_scope = previous_scope->template_scope;
					}

					break;
				}
			case AST_TEMPLATE_ID :
				{
					solve_possibly_ambiguous_template_id(nested_name_spec, sc);

					// Nothing else is necessary if we've seen that this was dependent
					if (*is_dependent)
					{
						break;
					}

					if (qualif_level == 0)
					{
						entry_list = query_unqualified_template_id_flags(nested_name_spec, sc, lookup_scope, LF_INSTANTIATE | lookup_flags);
					}
					else
					{
						entry_list = query_template_id_flags(nested_name_spec, sc, lookup_scope, LF_INSTANTIATE | lookup_flags);
					}

					if (entry_list == NULL)
					{
						return NULL;
					}

					if (entry_list->entry->kind == SK_DEPENDENT_ENTITY)
					{
						*is_dependent = 1;
						break;
					}

					scope_t* previous_scope = lookup_scope;
					lookup_scope = copy_scope(entry_list->entry->related_scope);

					// This can be null if the type is incomplete and we are
					// declaring a pointer to member
					if (lookup_scope != NULL)
					{
						lookup_scope->template_scope = previous_scope->template_scope;
					}

					seen_class = 1;
					break;
				}
			default:
				{
					internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(nested_name_spec)));
					break;
				}
		}

		qualif_level++;
		nested_name = ASTSon1(nested_name);
	}

	// If this was seen as dependent, do not return anything
	if (*is_dependent)
	{
		return NULL;
	}

	if (result_entry_list != NULL)
	{
		*result_entry_list = entry_list;
	}

	return lookup_scope;
}

scope_entry_list_t* query_nested_name(scope_t* sc, AST global_op, AST nested_name, AST name, 
		unqualified_lookup_behaviour_t unqualified_lookup)
{
	return query_nested_name_flags(sc, global_op, nested_name, name, unqualified_lookup, LF_NONE);
}

// Similar to query_nested_name_spec but searches the name
scope_entry_list_t* query_nested_name_flags(scope_t* sc, AST global_op, AST nested_name, AST name, 
		unqualified_lookup_behaviour_t unqualified_lookup, lookup_flags_t lookup_flags)
{
	scope_entry_list_t* result = NULL;
	scope_t* lookup_scope;

	if (global_op == NULL && nested_name == NULL)
	{
		char* symbol_name = ASTText(name);

		lookup_scope = sc;

		if (BITMAP_TEST(lookup_flags, LF_CONSTRUCTOR))
		{
			symbol_name = strprepend(symbol_name, "constructor ");
		}

		if (BITMAP_TEST(lookup_flags, LF_IN_NAMESPACE_SCOPE))
		{
			lookup_scope = copy_scope(enclosing_namespace_scope(sc));
			if (lookup_scope == NULL)
			{
				internal_error("Enclosing namespace not found\n", 0);
			}

			lookup_scope->template_scope = sc->template_scope;
		}

		// This is an unqualified identifier
		switch (ASTType(name))
		{
			case AST_SYMBOL :
                switch (unqualified_lookup)
				{
					case FULL_UNQUALIFIED_LOOKUP :
						{
							result = query_unqualified_name(lookup_scope, symbol_name);
							break;
						}
					case NOFULL_UNQUALIFIED_LOOKUP :
						{
							result = query_in_symbols_of_scope(lookup_scope, symbol_name);
							break;
						}
					default :
						{
							internal_error("Invalid lookup behaviour", 0);
						}
				}
				break;
			case AST_TEMPLATE_ID:
				{
					solve_possibly_ambiguous_template_id(name, sc);
					result = query_unqualified_template_id_flags(name, sc, lookup_scope, lookup_flags);
				}
				break;
			default :
				internal_error("Unexpected node type '%s'\n", ast_print_node_type(ASTType(name)));
		}
	}
	else
	{
		char is_dependent = 0;
		if ((lookup_scope = query_nested_name_spec_flags(sc, global_op, nested_name, 
						NULL, &is_dependent, lookup_flags)) != NULL)
		{
			// We have to inherit the template_scope
			// scope_t* saved_scope = lookup_scope->template_scope;
			// lookup_scope->template_scope = template_scope;

			switch (ASTType(name))
			{
				case AST_SYMBOL :
					{
						char* symbol_name = ASTText(name);

						if (BITMAP_TEST(lookup_flags, LF_CONSTRUCTOR))
						{
							symbol_name = strprepend(symbol_name, "constructor ");
						}

						result = query_unqualified_name_flags(lookup_scope, symbol_name, lookup_flags | LF_FROM_QUALIFIED);
					}
					break;
				case AST_TEMPLATE_ID:
					{
						solve_possibly_ambiguous_template_id(name, sc);
						result = query_template_id_flags(name, sc, lookup_scope, lookup_flags);
					}
					break;
				case AST_CONVERSION_FUNCTION_ID :
					{
						char* conversion_function_name = get_conversion_function_name(name, lookup_scope, NULL);
						result = query_unqualified_name_flags(lookup_scope, conversion_function_name, lookup_flags | LF_FROM_QUALIFIED);
						break;
					}
				case AST_DESTRUCTOR_TEMPLATE_ID :
				case AST_DESTRUCTOR_ID :
					{
						char* symbol_name = ASTText(ASTSon0(name));
						result = query_unqualified_name_flags(lookup_scope, symbol_name, lookup_flags | LF_FROM_QUALIFIED);
						break;
					}
				case AST_OPERATOR_FUNCTION_ID  :
					{
						// An unqualified operator_function_id "operator +"
						char* operator_function_name = get_operator_function_name(name);

						result = query_unqualified_name_flags(lookup_scope, operator_function_name, 
								lookup_flags | LF_FROM_QUALIFIED);
						break;
					}
				default :
					internal_error("Unexpected node type '%s'\n", ast_print_node_type(ASTType(name)));
			}

			// lookup_scope->template_scope = saved_scope;
		}
        else if (is_dependent)
        {
            scope_entry_t* dependent_entity = GC_CALLOC(1, sizeof(*dependent_entity));
            dependent_entity->kind = SK_DEPENDENT_ENTITY;

            return create_list_from_entry(dependent_entity);
        }
	}

	return result;
}


static scope_entry_list_t* query_template_id_internal(AST template_id, scope_t* sc, scope_t* lookup_scope, 
		unqualified_lookup_behaviour_t unqualified_lookup, lookup_flags_t lookup_flags)
{
	AST symbol = ASTSon0(template_id);
	fprintf(stderr, "Trying to resolve template '%s'\n", ASTText(symbol));

	scope_entry_list_t* entry_list; 
	
	if (unqualified_lookup == FULL_UNQUALIFIED_LOOKUP)
	{
		entry_list = query_unqualified_name(lookup_scope, ASTText(symbol));
	}
	else
	{
		entry_list = query_in_symbols_of_scope(lookup_scope, ASTText(symbol));
	}

	enum cxx_symbol_kind filter_templates[4] = {
		SK_TEMPLATE_PRIMARY_CLASS, 
		SK_TEMPLATE_SPECIALIZED_CLASS,
		SK_TEMPLATE_FUNCTION,
		SK_TEMPLATE_TEMPLATE_PARAMETER
	};

	entry_list = filter_symbol_kind_set(entry_list, 4, filter_templates);

	if (entry_list == NULL)
	{
		if (BITMAP_TEST(lookup_flags, LF_NO_FAIL))
		{
			return NULL;
		}
		else
		{
			internal_error("Template not found! (line=%d)\n", ASTLine(template_id));
		}
	}

	scope_entry_list_t* template_functions = filter_symbol_kind(entry_list, SK_TEMPLATE_FUNCTION);
	if (template_functions != NULL)
	{
		// This is naming a template function
		// Just return them, do not instantiate
		solve_possibly_ambiguous_template_id(template_id, sc);

		return template_functions;
	}
	
	// Now readjust the scope to really find the templates and not just the
	// injected class name
	lookup_scope = entry_list->entry->scope;
	if (unqualified_lookup == FULL_UNQUALIFIED_LOOKUP)
	{
		entry_list = query_unqualified_name(lookup_scope, ASTText(symbol));
	}
	else
	{
		entry_list = query_in_symbols_of_scope(lookup_scope, ASTText(symbol));
	}

	enum cxx_symbol_kind filter_template_classes[3] = {
		SK_TEMPLATE_PRIMARY_CLASS, 
		SK_TEMPLATE_SPECIALIZED_CLASS,
		SK_TEMPLATE_TEMPLATE_PARAMETER
	};

	entry_list = filter_symbol_kind_set(entry_list, 3, filter_template_classes);

	if (entry_list == NULL)
	{
		if (BITMAP_TEST(lookup_flags, LF_NO_FAIL))
		{
			return NULL;
		}
		else
		{
			internal_error("Template not found! (line=%d)\n", ASTLine(template_id));
		}
	}

	template_argument_list_t* current_template_arguments = NULL;
	// Note the scope being different here
	build_scope_template_arguments(template_id, lookup_scope, sc, sc, &current_template_arguments);

	// First try to match exactly an existing template
	// because this is a parameterized template-id
	char will_not_instantiate = 1;

	char always_create_specialization = 0;

	if (BITMAP_TEST(lookup_flags, LF_INSTANTIATE))
	{
		will_not_instantiate = 0;
	}

	if (BITMAP_TEST(lookup_flags, LF_ALWAYS_CREATE_SPECIALIZATION))
	{
		always_create_specialization = 1;
	}

	int i;
    char seen_dependent_args = 0;
	for (i = 0; (i < current_template_arguments->num_arguments)
			&& !seen_dependent_args; i++)
	{
		template_argument_t* argument = current_template_arguments->argument_list[i];
		if (argument->kind == TAK_TYPE)
		{
			if (is_dependent_tree(argument->argument_tree, argument->scope))
			{
				seen_dependent_args = 1;
			}
		}
		else if (argument->kind == TAK_NONTYPE)
		{
			literal_value_t value = evaluate_constant_expression(argument->argument_tree, argument->scope);

			if (value.kind == LVK_DEPENDENT_EXPR)
			{
				seen_dependent_args = 1;
			}

			if (value.kind == LVK_INVALID)
			{
				fprintf(stderr, "-> Template not returned since one of its arguments its an invalid expression\n");
				return NULL;
			}
		}
	}

	// If this is considered in the context of an expression and dependent args
	// have been seen, create a dependent entity
    if (BITMAP_TEST(lookup_flags, LF_EXPRESSION) 
            && seen_dependent_args)
    {
        fprintf(stderr, "This is a dependent template-id used in an expression\n");
        scope_entry_t* dependent_entity = GC_CALLOC(1, sizeof(*dependent_entity));
        dependent_entity->kind = SK_DEPENDENT_ENTITY;

        return create_list_from_entry(dependent_entity);
    }

	// If we have seen dependent arguments, we will not instantiate
	if (seen_dependent_args)
	{
		will_not_instantiate |= seen_dependent_args;
		fprintf(stderr, "This template-id has dependent arguments and will not be instantiated here\n");
	}
	
	fprintf(stderr, "-> Looking for exact match templates\n");

	matching_pair_t* matched_template = solve_template(entry_list,
			current_template_arguments, sc, /* exact = */ 1);

	if (matched_template != NULL)
	{
		// If the template we are selecting is not incomplete or we are not
		// willing to instantiate
		scope_entry_t* matched_entry = matched_template->entry;

		if (!will_not_instantiate 
				&& !matched_entry->type_information->type->from_instantiation)
		{
			fprintf(stderr, "-> Instantiating something that was declared before but not instantiated\n");

			scope_entry_list_t* fixed_entry_list = filter_entry_from_list(entry_list, matched_entry);
			matched_template = solve_template(fixed_entry_list,
					current_template_arguments, sc, 0);

			instantiate_template_in_symbol(matched_entry, matched_template, current_template_arguments, sc);

			// And now restart this function but now we want an exact match
			return query_template_id_internal(template_id, sc, lookup_scope, unqualified_lookup, 
					lookup_flags & (~LF_INSTANTIATE));
		}
		else
		{
			fprintf(stderr, "-> Just returning the matching template %p\n", matched_entry);
			return create_list_from_entry(matched_entry);
		}
	}
	else
	{
		fprintf(stderr, "-> Not selected an exact template\n");
	}

	// If we are here there is no exact match thus we may have to instantiate
	// the template
	
	if (!will_not_instantiate)
	{
		fprintf(stderr, "-> Solving the template without exact match\n");
		matched_template = solve_template(entry_list, current_template_arguments, sc, /* exact= */ 0);

		if (matched_template == NULL)
		{

			fprintf(stderr, "-> Nothing has matched, no template was selected\n");
			return NULL;
		}

		{
			int i;
			fprintf(stderr, "=== Unification details for selected template %p\n", matched_template->entry);

			for (i = 0; i < matched_template->unif_set->num_elems; i++)
			{
				unification_item_t* unif_item = matched_template->unif_set->unif_list[i];
				fprintf(stderr, "Parameter num: %d || Parameter nesting: %d || Parameter name: %s <- ",
						unif_item->parameter_num, unif_item->parameter_nesting, unif_item->parameter_name);
				if (unif_item->value != NULL)
				{
					fprintf(stderr, "[type] ");
					print_declarator(unif_item->value, sc);
				}
				else if (unif_item->expression != NULL)
				{
					fprintf(stderr, "[expr] ");
					prettyprint(stderr, unif_item->expression);
				}
				else
				{
					fprintf(stderr, "(unknown)");
				}
				fprintf(stderr, "\n");
			}
			fprintf(stderr, "=== End of unification details for selected template\n");
		}

		fprintf(stderr, "-> Instantiating the template\n");
		// We have to instantiate the template
		instantiate_template(matched_template, current_template_arguments, sc, ASTLine(template_id));

		// And now restart this function but now we want an exact match
		return query_template_id_internal(template_id, sc, lookup_scope, unqualified_lookup, 
				lookup_flags & (~LF_INSTANTIATE));
	}
	else
	{
		return create_list_from_entry(
				create_holding_symbol_for_template(entry_list->entry, current_template_arguments,
					sc, ASTLine(template_id))
				);
	}
}

scope_entry_list_t* query_unqualified_template_id(AST template_id, scope_t* sc, scope_t* lookup_scope)
{
	return query_template_id_internal(template_id, sc, lookup_scope, FULL_UNQUALIFIED_LOOKUP, LF_NONE);
}

scope_entry_list_t* query_unqualified_template_id_flags(AST template_id, scope_t* sc, scope_t* lookup_scope,
		lookup_flags_t lookup_flags)
{
	return query_template_id_internal(template_id, sc, lookup_scope, FULL_UNQUALIFIED_LOOKUP, lookup_flags);
}

scope_entry_list_t* query_template_id(AST template_id, scope_t* sc, scope_t* lookup_scope)
{
	return query_template_id_internal(template_id, sc, lookup_scope, NOFULL_UNQUALIFIED_LOOKUP, LF_NONE);
}

scope_entry_list_t* query_template_id_flags(AST template_id, scope_t* sc, 
		scope_t* lookup_scope, lookup_flags_t lookup_flags)
{
	return query_template_id_internal(template_id, sc, lookup_scope, NOFULL_UNQUALIFIED_LOOKUP, lookup_flags);
}

scope_entry_list_t* query_id_expression(scope_t* sc, AST id_expr, unqualified_lookup_behaviour_t unqualified_lookup)
{
	return query_id_expression_flags(sc, id_expr, unqualified_lookup, LF_NONE);
}

scope_entry_list_t* query_id_expression_flags(scope_t* sc, AST id_expr, 
		unqualified_lookup_behaviour_t unqualified_lookup, lookup_flags_t lookup_flags)
{
	switch (ASTType(id_expr))
	{
		// Unqualified ones
		case AST_SYMBOL :
			{
				char* id_expr_name = ASTText(id_expr);

				if (BITMAP_TEST(lookup_flags, LF_CONSTRUCTOR))
				{
					id_expr_name = strprepend(id_expr_name, "constructor ");
				}

				scope_entry_list_t* result = NULL;
                switch (unqualified_lookup)
				{
					case FULL_UNQUALIFIED_LOOKUP :
						{
							result = query_unqualified_name(sc, id_expr_name);
							break;
						}
					case NOFULL_UNQUALIFIED_LOOKUP :
						{
							result = query_in_symbols_of_scope(sc, id_expr_name);
							break;
						}
					default :
						{
							internal_error("Invalid lookup behaviour", 0);
						}
				}
				return result;
				break;
			}
		case AST_DESTRUCTOR_TEMPLATE_ID :
		case AST_DESTRUCTOR_ID :
			{
				// An unqualified destructor name "~name"
				// 'name' should be a class in this scope
				AST symbol = ASTSon0(id_expr);
				scope_entry_list_t* result = query_in_symbols_of_scope(sc, ASTText(symbol));

				return result;

				break;
			}
		case AST_TEMPLATE_ID :
			{
				// An unqualified template_id "identifier<stuff>"
				AST symbol = ASTSon0(id_expr);

				solve_possibly_ambiguous_template_id(id_expr, sc);

				scope_entry_list_t* result = query_unqualified_name(sc, ASTText(symbol));

				return result;
				break;
			}
		case AST_OPERATOR_FUNCTION_ID :
			{
				// An unqualified operator_function_id "operator +"
				char* operator_function_name = get_operator_function_name(id_expr);

				scope_entry_list_t* result = query_in_symbols_of_scope(sc, operator_function_name);

				return result;
				break;
			}
		case AST_CONVERSION_FUNCTION_ID :
			{
				// An unqualified conversion_function_id "operator T"
				// Why this has no qualified equivalent ?
				internal_error("Unsupported conversion function id", 0);
				break;
			}
			// Qualified ones
		case AST_QUALIFIED_ID :
			{
				// A qualified id "a::b::c"
				AST global_op = ASTSon0(id_expr);
				AST nested_name = ASTSon1(id_expr);
				AST symbol = ASTSon2(id_expr);

				scope_entry_list_t* result = query_nested_name_flags(sc, global_op, nested_name, 
                        symbol, FULL_UNQUALIFIED_LOOKUP, lookup_flags);

				return result;
				break;
			}
		case AST_QUALIFIED_TEMPLATE :
			{
				// A qualified template "a::b::template c<a>"
				internal_error("Unsupported qualified template", 0);
				break;
			}
		case AST_QUALIFIED_OPERATOR_FUNCTION_ID :
			{
				// A qualified operator function_id "a::b::operator +"
				internal_error("Unsupported qualified operator id", 0);
				break;
			}
		default :
			{
				internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(id_expr)));
				break;
			}
	}

	return NULL;
}

scope_entry_list_t* create_list_from_entry(scope_entry_t* entry)
{
	scope_entry_list_t* result = GC_CALLOC(1, sizeof(*result));
	result->entry = entry;
	result->next = NULL;

	return result;
}

static scope_entry_list_t* query_in_template_nesting(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
	scope_entry_list_t* result = NULL;

	while (st != NULL && result == NULL)
	{
		fprintf(stderr, "template lookup: ");
		result = query_in_symbols_of_scope(st, unqualified_name);
		st = st->template_scope;
	}

	return result;
}

static scope_entry_list_t* lookup_block_scope(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
	// First check the scope
	scope_entry_list_t* result = NULL;
	fprintf(stderr, "Looking up '%s' in block...", unqualified_name);
	result = query_in_symbols_of_scope(st, unqualified_name);

	if (result != NULL)
	{
		return result;
	}

	// Search in the namespaces
	fprintf(stderr, "not found.\nLooking up '%s' in used namespaces...", unqualified_name);
	int i;
	for (i = 0; i < st->num_used_namespaces; i++)
	{
		result = query_in_symbols_of_scope(st->use_namespace[i], unqualified_name);
		if (result != NULL)
		{
			return result;
		}
	}

	// Search in the scope of labels
	if (st->function_scope != NULL)
	{
		fprintf(stderr, "not found.\nLooking up '%s' in labels...", unqualified_name);
		result = query_in_symbols_of_scope(st->function_scope, unqualified_name);
		if (result != NULL)
		{
			return result;
		}
	}
	
	// Search in the scope of parameters
	if (st->prototype_scope != NULL)
	{
		fprintf(stderr, "not found.\nLooking up '%s' in parameters...", unqualified_name);
		result = query_in_symbols_of_scope(st->prototype_scope, unqualified_name);
		if (result != NULL)
		{
			return result;
		}
	}

	if (!BITMAP_TEST(lookup_flags, LF_FROM_QUALIFIED))
	{
		// Otherwise, if template scoping is available, check in the template scope
		if (st->template_scope != NULL)
		{
			fprintf(stderr, "not found.\nLooking up '%s' in template parameters...", unqualified_name);
			result = query_in_template_nesting(st->template_scope, unqualified_name, lookup_flags);
			if (result != NULL)
			{
				return result;
			}
		}

		// Otherwise try to find anything in the enclosing scope
		if (st->contained_in != NULL)
		{
			fprintf(stderr, "not found.\nLooking up '%s' in the enclosed scope...", unqualified_name);
			result = query_unqualified_name_flags(st->contained_in, unqualified_name, lookup_flags);
		}
	}

	return result;
}

static scope_entry_list_t* lookup_prototype_scope(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
	scope_entry_list_t* result = NULL;
	
	// Otherwise, if template scoping is available, check in the template scope
	if (st->template_scope != NULL)
	{
		fprintf(stderr, "Looking up '%s' in template parameters...", unqualified_name);
		result = query_in_template_nesting(st->template_scope, unqualified_name, lookup_flags);
		if (result != NULL)
		{
			return result;
		}
	}

	// Otherwise try to find anything in the enclosing scope
	if (st->contained_in != NULL)
	{
		fprintf(stderr, "not found.\nLooking up '%s' in the enclosed scope...", unqualified_name);
		result = query_unqualified_name_flags(st->contained_in, unqualified_name, lookup_flags);
	}

	return result;
}

static scope_entry_list_t* lookup_namespace_scope(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
	// First check the scope
	scope_entry_list_t* result = NULL;
	fprintf(stderr, "Looking up '%s' within namespace...", unqualified_name);
	result = query_in_symbols_of_scope(st, unqualified_name);

	if (result != NULL)
	{
		return result;
	}
	
	// Otherwise, if template scoping is available, check in the template scope
	if (st->template_scope != NULL)
	{
		fprintf(stderr, "Looking up '%s' in template parameters...", unqualified_name);
		result = query_in_template_nesting(st->template_scope, unqualified_name, lookup_flags);
		if (result != NULL)
		{
			return result;
		}
	}

	// Search in the namespaces
	fprintf(stderr, "not found.\nLooking up '%s' in used namespaces (%d)...", unqualified_name, st->num_used_namespaces);
	int i;
	for (i = 0; i < st->num_used_namespaces; i++)
	{
		result = query_in_symbols_of_scope(st->use_namespace[i], unqualified_name);
		if (result != NULL)
		{
			return result;
		}
	}

	if (!BITMAP_TEST(lookup_flags, LF_FROM_QUALIFIED))
	{
		// Otherwise try to find anything in the enclosing scope
		if (st->contained_in != NULL)
		{
			fprintf(stderr, "not found.\nLooking up '%s' in the enclosed scope...", unqualified_name);
			result = query_unqualified_name_flags(st->contained_in, unqualified_name, lookup_flags);
		}
	}

	return result;
}

static scope_entry_list_t* lookup_function_scope(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
	// First check the scope
	scope_entry_list_t* result = NULL;
	fprintf(stderr, "Looking up '%s' in function scope...", unqualified_name);
	result = query_in_symbols_of_scope(st, unqualified_name);
	//
	// Otherwise try to find anything in the enclosing scope
	if (st->contained_in != NULL)
	{
		fprintf(stderr, "not found.\nLooking up '%s' in the enclosed scope...", unqualified_name);
		result = query_unqualified_name_flags(st->contained_in, unqualified_name, lookup_flags);
	}

	return result;
}

static scope_entry_list_t* lookup_class_scope(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
	// First check the scope
	scope_entry_list_t* result = NULL;
	fprintf(stderr, "Looking up '%s' in class...", unqualified_name);
	result = query_in_symbols_of_scope(st, unqualified_name);

	if (result != NULL)
	{
		return result;
	}

	// Search in the namespaces
	fprintf(stderr, "not found.\nLooking up '%s' in used namespaces...", unqualified_name);
	int i;
	for (i = 0; i < st->num_used_namespaces; i++)
	{
		result = query_in_symbols_of_scope(st->use_namespace[i], unqualified_name);
		if (result != NULL)
		{
			return result;
		}
	}
	
	// Search in the bases
	fprintf(stderr, "not found.\nLooking up '%s' in bases (%d)...", unqualified_name, st->num_base_scopes);
	for (i = 0; i < st->num_base_scopes; i++)
	{
		result = query_unqualified_name_flags(st->base_scope[i], unqualified_name, lookup_flags);
		if (result != NULL)
		{
			return result;
		}
	}

	if (!BITMAP_TEST(lookup_flags, LF_FROM_QUALIFIED))
	{
		// Otherwise, if template scoping is available, check in the template scope
		if (st->template_scope != NULL)
		{
			fprintf(stderr, "not found.\nLooking up '%s' in template parameters...", unqualified_name);
			result = query_in_template_nesting(st->template_scope, unqualified_name, lookup_flags);
			if (result != NULL)
			{
				return result;
			}
		}

		// Otherwise try to find anything in the enclosing scope
		if (st->contained_in != NULL)
		{
			fprintf(stderr, "not found.\nLooking up '%s' in the enclosed scope...", unqualified_name);
			result = query_unqualified_name_flags(st->contained_in, unqualified_name, lookup_flags);
		}
	}

	return result;
}

static scope_entry_list_t* lookup_template_scope(scope_t* st, char* unqualified_name, lookup_flags_t lookup_flags)
{
	// First check the scope
	scope_entry_list_t* result = NULL;
	fprintf(stderr, "Looking up '%s' in template scope...", unqualified_name);
	result = query_in_symbols_of_scope(st, unqualified_name);

	if (result != NULL)
	{
		return result;
	}

	// Otherwise, if template scoping is available, check in the template scope
	if (st->template_scope != NULL)
	{
		fprintf(stderr, "not found.\nLooking up '%s' in template parameters...", unqualified_name);
		result = query_in_template_nesting(st->template_scope, unqualified_name, lookup_flags);
		if (result != NULL)
		{
			return result;
		}
	}

	// Otherwise try to find anything in the enclosing scope
	if (st->contained_in != NULL)
	{
		fprintf(stderr, "not found.\nLooking up '%s' in the enclosed scope...", unqualified_name);
		result = query_unqualified_name_flags(st->contained_in, unqualified_name, lookup_flags);
	}

	return result;
}

scope_entry_list_t* query_unqualified_name_flags(scope_t* st, char* unqualified_name, 
		lookup_flags_t lookup_flags)
{
	static int nesting_level = 0;

	nesting_level++;

	scope_entry_list_t* result = NULL;

	switch (st->kind)
	{
		case PROTOTYPE_SCOPE :
			fprintf(stderr, "Starting lookup in prototype scope %p\n", st);
			result = lookup_prototype_scope(st, unqualified_name, lookup_flags);
			break;
		case NAMESPACE_SCOPE :
			fprintf(stderr, "Starting lookup in namespace scope %p\n", st);
			result = lookup_namespace_scope(st, unqualified_name, lookup_flags);
			break;
		case FUNCTION_SCOPE :
			fprintf(stderr, "Starting lookup in function scope %p\n", st);
			result = lookup_function_scope(st, unqualified_name, lookup_flags);
			break;
		case BLOCK_SCOPE :
			fprintf(stderr, "Starting lookup in block scope %p\n", st);
			result = lookup_block_scope(st, unqualified_name, lookup_flags);
			break;
		case CLASS_SCOPE :
			fprintf(stderr, "Starting lookup in class scope %p\n", st);
			result = lookup_class_scope(st, unqualified_name, lookup_flags);
			break;
		case TEMPLATE_SCOPE :
			fprintf(stderr, "Starting lookup in template scope %p\n", st);
			result = lookup_template_scope(st, unqualified_name, lookup_flags);
			break;
		case UNDEFINED_SCOPE :
		default :
			internal_error("Invalid scope kind=%d!\n", st->kind);
	}

	nesting_level--;

	if (nesting_level == 0)
	{
		if (result != NULL)
		{
			fprintf(stderr, "found\n");
		}
		else
		{
			fprintf(stderr, "not found.\n");
		}
	}

	return result;
}

scope_entry_list_t* query_unqualified_name(scope_t* st, char* unqualified_name)
{
	return query_unqualified_name_flags(st, unqualified_name, LF_NONE);
}

// char incompatible_symbol_exists(scope_t* sc, AST id_expr, enum cxx_symbol_kind symbol_kind)
// {
// 	scope_entry_list_t* entry_list = query_id_expression(sc, id_expr);
// 	char found_incompatible = 0;
// 
// 	while (!found_incompatible && entry_list != NULL)
// 	{
// 		found_incompatible = (entry_list->entry->kind != symbol_kind);
// 
// 		entry_list = entry_list->next;
// 	}
// 
// 	return found_incompatible;
// }


scope_entry_list_t* filter_symbol_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set)
{
	scope_entry_list_t* result = NULL;
	scope_entry_list_t* iter = entry_list;
	
	while (iter != NULL)
	{
		int i;
		char found = 0;
		for (i = 0; (i < num_kinds) && !found; i++)
		{
			if (iter->entry->kind == symbol_kind_set[i])
			{
				scope_entry_list_t* new_item = GC_CALLOC(1, sizeof(*new_item));
				new_item->entry = iter->entry;
				new_item->next = result;
				result = new_item;
				found = 1;
			}
		}

		iter = iter->next;
	}

	return result;
}

scope_entry_list_t* filter_symbol_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind)
{
	scope_entry_list_t* result = NULL;

	result = filter_symbol_kind_set(entry_list, 1, &symbol_kind);

	return result;
}


scope_entry_list_t* filter_symbol_non_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set)
{
	scope_entry_list_t* result = NULL;
	scope_entry_list_t* iter = entry_list;
	
	while (iter != NULL)
	{
		int i;
		char found = 0;
		for (i = 0; (i < num_kinds) && !found; i++)
		{
			if (iter->entry->kind == symbol_kind_set[i])
			{
				found = 1;
			}
		}

		if (!found)
		{
			scope_entry_list_t* new_item = GC_CALLOC(1, sizeof(*new_item));
			new_item->entry = iter->entry;
			new_item->next = result;
			result = new_item;
		}

		iter = iter->next;
	}

	return result;
}

scope_entry_list_t* filter_symbol_non_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind)
{
	scope_entry_list_t* result = NULL;

	result = filter_symbol_non_kind_set(entry_list, 1, &symbol_kind);

	return result;
}

scope_entry_list_t* filter_entry_from_list(scope_entry_list_t* entry_list, scope_entry_t* entry)
{
	scope_entry_list_t* result = NULL;
	scope_entry_list_t* iter = entry_list;
	
	while (iter != NULL)
	{
		if (iter->entry != entry)
		{
			scope_entry_list_t* new_item = GC_CALLOC(1, sizeof(*new_item));
			new_item->entry = iter->entry;
			new_item->next = result;
			result = new_item;
		}

		iter = iter->next;
	}

	return result;
}

scope_entry_list_t* filter_symbol_using_predicate(scope_entry_list_t* entry_list, char (*f)(scope_entry_t*))
{
	scope_entry_list_t* result = NULL;
	scope_entry_list_t* iter = entry_list;
	
	while (iter != NULL)
	{
		if (f(iter->entry))
		{
			scope_entry_list_t* new_item = GC_CALLOC(1, sizeof(*new_item));
			new_item->entry = iter->entry;
			new_item->next = result;
			result = new_item;
		}

		iter = iter->next;
	}

	return result;
}

/*
 * Returns a type if and only if this entry_list contains just one type
 * specifier. If another identifier is found it returns NULL
 */
scope_entry_t* filter_simple_type_specifier(scope_entry_list_t* entry_list)
{
	int non_type_name = 0;
	scope_entry_t* result = NULL;

	char seen_class_name = 0;
	char seen_function_name = 0;

	while (entry_list != NULL)
	{
		scope_entry_t* simple_type_entry = entry_list->entry;

		if (simple_type_entry->kind != SK_ENUM 
				&& simple_type_entry->kind != SK_CLASS 
				&& simple_type_entry->kind != SK_TYPEDEF 
				&& simple_type_entry->kind != SK_TEMPLATE_TYPE_PARAMETER
				&& simple_type_entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER
				&& simple_type_entry->kind != SK_TEMPLATE_PRIMARY_CLASS
				&& simple_type_entry->kind != SK_TEMPLATE_SPECIALIZED_CLASS
				&& simple_type_entry->kind != SK_GCC_BUILTIN_TYPE)
		{
			// Functions with name of a class are constructors and do not hide
			// the class symbol
			seen_function_name |= (simple_type_entry->kind == SK_FUNCTION);
			fprintf(stderr, "Found a '%d' that is non type\n", simple_type_entry->kind);
			non_type_name++;
		}
		else
		{
			seen_class_name |= simple_type_entry->kind == SK_CLASS;
			result = simple_type_entry;
		}

		entry_list = entry_list->next;
	}

	// There is something that is not a type name here and hides this simple type spec
	if (non_type_name != 0 
			&& !seen_class_name 
			&& !seen_function_name)
	{
		return NULL;
	}
	else
	{
		return result;
	}
}

scope_entry_list_t* append_scope_entry_lists(scope_entry_list_t* a, scope_entry_list_t* b)
{
	scope_entry_list_t* result = NULL;
	scope_entry_list_t* iter;

	iter = a;
	while (iter != NULL)
	{
		scope_entry_list_t* new_entry = GC_CALLOC(1, sizeof(*new_entry));

		new_entry->entry = iter->entry;
		new_entry->next = result;

		result = new_entry;

		iter = iter->next;
	}

	iter = b;
	while (iter != NULL)
	{
		scope_entry_list_t* new_entry = GC_CALLOC(1, sizeof(*new_entry));

		new_entry->entry = iter->entry;
		new_entry->next = result;

		result = new_entry;

		iter = iter->next;
	}

	return result;
}

scope_t* enclosing_namespace_scope(scope_t* st)
{
	if (st == NULL)
		return NULL;

	if (st->contained_in == NULL)
	{
		return NULL;
	}

	while (st != NULL
			&& st->kind != NAMESPACE_SCOPE)
	{
		st = st->contained_in;
	}

	return st;
}

// Copy scope to be able to retrieve data exactly as it was in that point
// (useful for dynamic scoping like template scopes that disappear)
scope_t* copy_scope(scope_t* st)
{
	if (st == NULL)
		return NULL;

	scope_t* result = GC_CALLOC(1, sizeof(*result));

	// bitwise copy
	*result = *st;

	return result;
}

