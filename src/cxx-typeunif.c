#include <gc.h>
#include "cxx-typeunif.h"
#include "cxx-typeutils.h"
#include "cxx-cexpr.h"
#include "cxx-symtab.h"
#include "cxx-utils.h"

static type_t* get_template_parameter_unification(unification_set_t* unif_set, int n);

// Will try to find a substitution to unificate t1 to t2
//
// e.g.   Q*    can    be unificated to   T**    with   [Q <- T*]
//        T**   cannot be unificated to   Q*
//
char unificate_two_types(type_t* t1, type_t* t2, symtab_t* st, unification_set_t** unif_set)
{
	// Check first if t1 is a template parameter
	type_t* user_defined_type = NULL;
	
	// If the user defined type points to a template parameter, we will use the
	// template parameter
	if (t1->kind == TK_DIRECT && 
			t1->type->kind == STK_USER_DEFINED)
	{
		user_defined_type = t1->type->user_defined_type->type_information;
		if (user_defined_type->kind != TK_DIRECT
				|| user_defined_type->type->kind != STK_TYPE_TEMPLATE_PARAMETER)
		{
			user_defined_type = NULL;
		}
	}

	// If it is a template parameter (or a user defined type pointing to it)
	// then perform unification
	//
	// First check if this parameter has not been already unified
	if ((t1->kind == TK_DIRECT
			&& t1->type->kind == STK_TYPE_TEMPLATE_PARAMETER)
			|| user_defined_type != NULL )
	{
		if (user_defined_type != NULL)
		{
			t1 = user_defined_type;
		}

		type_t* previous_unif = get_template_parameter_unification(*unif_set, t1->type->template_parameter_num);
		if (previous_unif == NULL)
		{
			unification_item_t* unif_item = GC_CALLOC(1, sizeof(*unif_item));

			// This number will be the position of the argument
			// within the specialization ! Not of the whole template
			unif_item->parameter = t1->type->template_parameter_num;
			unif_item->value = t2;

			P_LIST_ADD((*unif_set)->unif_list, (*unif_set)->num_elems, unif_item);
		}
		else
		{
			// Check is the same unification we are going to do
			if (!equivalent_types(previous_unif, t2, st))
			{
				// They're not equivalent, thus not unificable
				return 0;
			}
		}

		// They have been unified
		return 1;
	}

	// t1 is not a template parameter, so to be unificable they have to be of
	// same shape
	if (t1->kind != t2->kind)
		return 0;

	// t1->kind == t2->kind
	switch (t1->kind)
	{
		case TK_DIRECT :
			{
				// If they were unificable they would have been unified before
				return equivalent_simple_types(t1->type, t2->type, st);
				break;
			}
		case TK_REFERENCE :
		case TK_POINTER :
			{
				return unificate_two_types(t1->pointer->pointee, t2->pointer->pointee, st, unif_set);
				break;
			}
		case TK_POINTER_TO_MEMBER :
			// TODO - What to do here ?
			break;
		case TK_ARRAY :
			{
				literal_value_t v1 = evaluate_constant_expression(t1->array->array_expr, st);
				literal_value_t v2 = evaluate_constant_expression(t2->array->array_expr, st);

				if (equal_literal_values(v1, v2, st))
				{
					return unificate_two_types(t1->array->element_type, t2->array->element_type, st, unif_set);
				}
				break;
			}
		case TK_FUNCTION :
			// TODO - think on this
			// A function will be unified by steps. First unify the return
			// then the parameters
			{
				if (!unificate_two_types(t1->function->return_type, t2->function->return_type, st, unif_set))
				{
					return 0;
				}

				if (t1->function->num_parameters != t2->function->num_parameters)
				{
					return 0;
				}

				int i;
				for (i = 0; i < t1->function->num_parameters; i++)
				{
					type_t* par1 = t1->function->parameter_list[i];
					type_t* par2 = t2->function->parameter_list[i];

					if (!unificate_two_types(par1, par2, st, unif_set))
					{
						return 0;
					}
				}
			}
		default :
			internal_error("Unknown type kind %d\n", t1->kind);
	}
	// Unifications succeeded
	return 1;
}

static type_t* get_template_parameter_unification(unification_set_t* unif_set, int n)
{
	int i;
	for (i = 0; i < unif_set->num_elems; i++)
	{
		if (unif_set->unif_list[i]->parameter == n)
		{
			return unif_set->unif_list[i]->value;
		}
	}

	return NULL;
}
