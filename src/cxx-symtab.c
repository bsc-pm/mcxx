#include <stdlib.h>
#include <string.h>
#include "cxx-symtab.h"
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
				simple_type_entry->kind != SK_TYPEDEF)
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
