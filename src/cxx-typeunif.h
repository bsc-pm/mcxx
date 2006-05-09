#ifndef CXX_TYPEUNIF_H
#define CXX_TYPEUNIF_H

#include "cxx-symtab.h"

typedef struct unification_item_tag
{
	// parameter type <- value
	int parameter;
	type_t* value;
} unification_item_t;

typedef struct 
{
	int num_elems;
	unification_item_t** unif_list;
} unification_set_t;

char unificate_two_types(type_t* t1, type_t* t2, symtab_t* st, unification_set_t** unif_set);

#endif
