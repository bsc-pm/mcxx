#include <stdint.h>
#include "cxx-scopelink.h"
#include "gc.h"
#include "cxx-ast.h"
#include "cxx-scope.h"

static int integer_comp (void *key1, void *key2)
{
	intptr_t a = (intptr_t)(key1);
	intptr_t b = (intptr_t)(key2);

	if (a == b)
	{
		return 0;
	}
	else if (a < b)
	{
		return -1;
	}
	else return 1;
}

static int pointer_hash(void* key, int size)
{
	intptr_t v = (intptr_t)(key);

	return (v % size);
}

scope_link_t* scope_link_new(void)
{
	scope_link_t* result = GC_CALLOC(1, sizeof(*result));

	result->h = hash_create(23, pointer_hash, integer_comp);

	return result;
}

void scope_link_set(scope_link_t* sl, AST a, scope_t* st)
{
	hash_put(sl->h, a, st);
}

scope_t* scope_link_get(scope_link_t* sl, AST a)
{
	scope_t* result = NULL;

	while (a != NULL)
	{
		result = (scope_t*)hash_get(sl->h, a);

		if (result != NULL)
		{
			return result;
		}

		a = ASTParent(a);
	}

	return NULL;
}
