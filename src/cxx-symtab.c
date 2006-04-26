#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "cxx-symtab.h"
#include "cxx-driver.h"
#include "cxx-utils.h"
#include "hash.h"


symtab_t* new_symtab()
{
	symtab_t* st = calloc(1, sizeof(*st));
	st->hash = hash_create(HASH_SIZE, HASHFUNC(prime_hash), KEYCMPFUNC(strcmp));
	st->parent = NULL;

	return st;
}

symtab_t* enter_scope(symtab_t* parent)
{
	symtab_t* st = new_symtab();
	st->parent = parent;

	return st;
}

symtab_entry_t* new_symbol(symtab_t* st, char* name)
{
	symtab_entry_list_t* result_set = (symtab_entry_list_t*) hash_get(st->hash, name);

	symtab_entry_t* result;

	result = calloc(1, sizeof(*result));
	result->symbol_name = strdup(name);
	result->scope = st;

	if (result_set != NULL)
	{
		symtab_entry_list_t* new_set = (symtab_entry_list_t*) calloc(1, sizeof(*new_set));

		// Put the new entry in front of the previous
		*new_set = *result_set;

		result_set->next = new_set;
		result_set->entry = result;
	}
	else
	{
		result_set = (symtab_entry_list_t*) calloc(1, sizeof(*result_set));
		result_set->entry = result;
		result_set->next = NULL; // redundant, though

		hash_put(st->hash, name, result_set);
	}

	return result;
}

symtab_entry_list_t* query_in_current_scope(symtab_t* st, char* name)
{
	symtab_entry_list_t* result = (symtab_entry_list_t*) hash_get(st->hash, name);

	return result;
}

// Note that the resulting list is not a merge of all the scopes but the first
// scope that yields a non-null result
symtab_entry_list_t* query_in_current_and_upper_scope(symtab_t* st, char* name)
{
	symtab_t* scope = st;
	while (scope != NULL)
	{
		symtab_entry_list_t* result = query_in_current_scope(scope, name);
		if (result != NULL)
		{
			return result;
		}
		scope = scope->parent;
	}

	return NULL;
}

/*
 * Insert entry in the symtab
 */
void insert_entry(symtab_t* st, symtab_entry_t* entry)
{
	if (entry->symbol_name == NULL)
	{
		internal_error("Inserting an symbol entry without name!", 0);
	}
	
	symtab_entry_list_t* result_set = (symtab_entry_list_t*) hash_get(st->hash, entry->symbol_name);


	if (result_set != NULL)
	{
		symtab_entry_list_t* new_set = (symtab_entry_list_t*) calloc(1, sizeof(*new_set));

		// Put the new entry in front of the previous
		*new_set = *result_set;

		result_set->next = new_set;
		result_set->entry = entry;
	}
	else
	{
		result_set = (symtab_entry_list_t*) calloc(1, sizeof(*result_set));
		result_set->entry = entry;
		result_set->next = NULL; // redundant, though

		hash_put(st->hash, entry->symbol_name, result_set);
	}
}

/*
 * Returns a type if and only if this entry_list contains just one type
 * specifier. If another identifier is found it returns NULL
 */
symtab_entry_t* filter_simple_type_specifier(symtab_entry_list_t* entry_list)
{
	int non_type_name = 0;
	symtab_entry_t* result = NULL;

	while (entry_list != NULL)
	{
		symtab_entry_t* simple_type_entry = entry_list->entry;

		if (simple_type_entry->kind != SK_ENUM &&
				simple_type_entry->kind != SK_CLASS &&
				simple_type_entry->kind != SK_TYPEDEF &&
				simple_type_entry->kind != SK_TEMPLATE_PARAMETER)
		{
			non_type_name++;
		}
		else
		{
			result = simple_type_entry;
		}

		entry_list = entry_list->next;
	}

	// There is something that is not a type name here and hides this simple type spec
	if (non_type_name != 0)
		return NULL;
	else
		return result;
}

/*
 * This function queries the symbol table through a nested name specifier. If
 * result_lookup_scope is nonnull it will hold the scope of this nested name
 * specifier. The result is the symbol in the symtab of this nested name
 * specification.
 * 
 * The value of result_lookup_scope is not reliable if the function returns
 * NULL
 */
symtab_entry_list_t* query_nested_name_spec(symtab_t* st, symtab_t** result_lookup_scope, AST global_op, AST nested_name)
{
	if (result_lookup_scope != NULL)
		*result_lookup_scope = NULL;
	
	symtab_t* lookup_scope = st;
	symtab_entry_list_t* entry_list = NULL;

	if (global_op != NULL)
	{
		lookup_scope = compilation_options.global_scope;
	}

	while (nested_name != NULL)
	{
		AST nested_name_spec = ASTSon0(nested_name);
		char seen_class = 0;

		switch (ASTType(nested_name_spec))
		{
			case AST_SYMBOL :
				{
					entry_list = 
						query_in_current_and_upper_scope(lookup_scope, ASTText(nested_name_spec));

					if (entry_list == NULL)
						return NULL;

					if (entry_list->entry->kind != SK_CLASS
							&& entry_list->entry->kind != SK_NAMESPACE)
					{
						return NULL;
					}

					if (seen_class 
							&& entry_list->entry->kind == SK_NAMESPACE)
						return NULL;

					if (entry_list->entry->kind == SK_CLASS)
						seen_class = 1;

					lookup_scope = entry_list->entry->inner_scope;

					break;
				}
			case AST_TEMPLATE_ID :
				{
					internal_error("Unsupported template id", 0);
					break;
				}
			default:
				{
					internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(nested_name_spec)));
					break;
				}
		}

		nested_name = ASTSon1(nested_name);
	}

	if (result_lookup_scope != NULL)
		*result_lookup_scope = lookup_scope;

	return entry_list;
}

symtab_entry_list_t* query_id_expression(symtab_t* st, AST id_expr)
{
	switch (ASTType(id_expr))
	{
		// Unqualified ones
		case AST_SYMBOL :
			{
				return query_in_current_and_upper_scope(st, ASTText(id_expr));
				break;
			}
		case AST_DESTRUCTOR_ID :
			{
				// An unqualified destructor name "~name"
				// 'name' should be a class in this scope
				AST symbol = ASTSon0(id_expr);
				symtab_entry_list_t* result = query_in_current_and_upper_scope(st, ASTText(symbol));

				return result;

				break;
			}
		case AST_TEMPLATE_ID :
			{
				// An unqualified template_id "identifier<stuff>"
				internal_error("Unsupported template id", 0);
				break;
			}
		case AST_OPERATOR_FUNCTION_ID :
			{
				// An unqualified operator_function_id "operator +"
				internal_error("Unsupported operator id", 0);
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
				symtab_t* lookup_scope;
				AST global_op = ASTSon0(id_expr);
				AST nested_name = ASTSon1(id_expr);

				if (query_nested_name_spec(st, &lookup_scope, global_op, nested_name) == NULL)
					return NULL;

				return query_id_expression(lookup_scope, ASTSon2(id_expr));
				break;
			}
		case AST_QUALIFIED_TEMPLATE :
			{
				// A qualified template "a::b::template c" [?]
				internal_error("Unsupported qualified template", 0);
				break;
			}
		case AST_QUALIFIED_TEMPLATE_ID :
			{
				// A qualified template_id "a::b::c<int>"
				internal_error("Unsupported qualified template id", 0);
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

symtab_entry_list_t* create_list_from_entry(symtab_entry_t* entry)
{
	symtab_entry_list_t* result = calloc(1, sizeof(*result));
	result->entry = entry;
	result->next = NULL;

	return result;
}

char incompatible_symbol_exists(symtab_t* st, AST id_expr, enum cxx_symbol_kind symbol_kind)
{
	symtab_entry_list_t* entry_list = query_id_expression(st, id_expr);
	char found_incompatible = 0;

	while (!found_incompatible && entry_list != NULL)
	{
		found_incompatible = (entry_list->entry->kind != symbol_kind);

		entry_list = entry_list->next;
	}

	return found_incompatible;
}
