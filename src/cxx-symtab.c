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
	symtab_entry_t* result = (symtab_entry_t*) hash_get(st->hash, name);

	if (result != NULL)
	{
		return NULL;
	}

	result = calloc(1, sizeof(*result));
	result->symbol_name = strdup(name);

	return result;
}

symtab_entry_t* query_in_current_scope(symtab_t* st, char* name)
{
	symtab_entry_t* result = (symtab_entry_t*) hash_get(st->hash, name);

	return result;
}

symtab_entry_t* query_in_current_and_upper_scope(symtab_t* st, char* name)
{
	symtab_t* scope = st;
	while (st != NULL)
	{
		symtab_entry_t* result = query_in_current_scope(scope, name);
		if (result != NULL)
		{
			return result;
		}
		st = st->parent;
	}

	return NULL;
}
