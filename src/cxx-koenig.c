#include "cxx-koenig.h"
#include "cxx-ast.h"
#include "cxx-scope.h"

static void compute_associated_namespaces_and_classes(scope_t* st, AST arguments, 
		scope_t** associated_namespaces, int* num_associated_namespaces,
		scope_t** associated_classes, int* num_associated_classes);

/*
 * This file implements Koenig lookup, technically named "argument dependent lookup"
 */

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
		current_symbols = query_unqualified_name(st, name);
#warning Add these symbols to the result set
	}

	for (i = 0; i < num_associated_classes; i++)
	{
		scope_entry_list_t* current_symbols;
		scope_t* associated = associated_classes[i];
		current_symbols = query_unqualified_name(st, name);
#warning Add these symbols to the result set
	}

#warning Fix this
	return NULL;
}

void compute_associated_namespaces_and_classes(scope_t* st, AST arguments, 
		scope_t** associated_namespaces, int* num_associated_namespaces,
		scope_t** associated_classes, int* num_associated_classes)
{
}
