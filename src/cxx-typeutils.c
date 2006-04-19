#include "cxx-typeutils.h"
#include "cxx-utils.h"

/*
 * This file contains routines destined to work with types.  Comparing two
 * types, comparing function declarations and definitions, etc.
 */
static char is_typedef_type(type_t* t);
static type_t* aliased_type(type_t* t);
static type_t* base_type(type_t* t);
static char equivalent_simple_types(simple_type_t *t1, simple_type_t *t2);
static char equivalent_builtin_type(simple_type_t *t1, simple_type_t *t2);
static char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2);
static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2);
static char equivalent_array_type(array_info_t* t1, array_info_t* t2);
static char equivalent_function_type(function_info_t* t1, function_info_t* t2);
static char compatible_parameters(function_info_t* t1, function_info_t* t2);


/*
 * States if two types are equivalent. This means that they are the same
 * (ignoring typedefs). Just plain comparison, no standard conversion is
 * performed. cv-qualifiers are relevant for comparison
 */
char equivalent_types(type_t* t1, type_t* t2)
{
	// Advance over typedefs
	while (is_typedef_type(t1))
	{
		t1 = aliased_type(t1);
	}

	while (is_typedef_type(t2))
	{
		t2 = aliased_type(t2);
	}

	if (t1->kind != t2->kind)
	{
		// They cannot be the same
		return 0;
	}

	switch (t1->kind)
	{
		case TK_DIRECT :
			return equivalent_simple_types(t1->type, t2->type);
			break;
		case TK_POINTER :
			return equivalent_pointer_type(t1->pointer, t2->pointer);
			break;
		case TK_REFERENCE :
			return equivalent_pointer_type(t1->pointer, t2->pointer);
			break;
		case TK_POINTER_TO_MEMBER :
			break;
		case TK_ARRAY :
			return equivalent_array_type(t1->array, t2->array);
			break;
		case TK_FUNCTION :
			return equivalent_function_type(t1->function, t2->function);
			break;
		default :
			internal_error("Unknown type kind (%d)\n", t1->kind);
	}

	return 0;
}

static char equivalent_simple_types(simple_type_t *t1, simple_type_t *t2)
{
	if (t1->kind != t2->kind)
	{
		// typedefs have been handled in an earlier place, so 
		// this cannot be the same type
		return 0;
	}

	switch (t1->kind)
	{
		case STK_BUILTIN_TYPE :
			return equivalent_builtin_type(t1, t2);
			break;
		case STK_CLASS :
			/* Fall-through */
		case STK_ENUM :
			// Pointer comparison MUST work
			// (if not, something is broken)
			return t1 == t2;
			break;
		case STK_USER_DEFINED :
			return equivalent_types(t1->user_defined_type->type_information, 
					t2->user_defined_type->type_information);
			break;
		case STK_TYPEDEF :
			internal_error("A typedef cannot reach here", 0);
			break;
		default :
			internal_error("Unknown simple type kind (%d)", t1->kind);
			return 0;
	}

	return 0;
}

static char equivalent_builtin_type(simple_type_t *t1, simple_type_t *t2)
{
	if (t1->builtin_type != t2->builtin_type)
	{
		return 0;
	}

	// Ok, up to here "unsigned int" and "signed int" are the same
	// The same happens with "long int" and "int"
	//
	// long
	if (t1->builtin_type == BT_INT
			|| t1->builtin_type == BT_DOUBLE)
	{
		if (t1->is_long != t2->is_long)
			return 0;
	}

	// short
	if (t1->builtin_type == BT_INT)
	{
		if (t1->is_short != t2->is_short)
			return 0;
	}

	// unsigned
	if (t1->builtin_type == BT_INT
			|| t1->builtin_type == BT_CHAR)
	{
		if (t1->is_unsigned != t2->is_unsigned)
			return 0;
	}
	
	// signed
	if (t1->builtin_type == BT_INT
			|| t1->builtin_type == BT_CHAR)
	{
		if (t1->is_signed != t2->is_signed)
			return 0;
	}
	
	if (!equivalent_cv_qualification(t1->cv_qualifier, t2->cv_qualifier))
	{
		return 0;
	}

	// Ok, nothing makes us think they might be different
	return 1;
}

static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2)
{
	if (!equivalent_types(t1->pointee, t2->pointee))
	{
		return 0;
	}

	return (equivalent_cv_qualification(t1->cv_qualifier, t2->cv_qualifier));
}

static char equivalent_array_type(array_info_t* t1, array_info_t* t2)
{
	if (!equivalent_types(t1->element_type, t2->element_type))
		return 0;

	// TODO - Check that dimensions are the same
	// But we need an evaluator of expressions
#if 0
	literal_value_t v1 = evaluate_constant_expression(t1->array_expr);
	literal_value_t v2 = evaluate_constant_expression(t2->array_expr);

	if (!equal_literal_values(v1, v2))
		return 0;
#endif
	
	return 1;
}

static char equivalent_function_type(function_info_t* t1, function_info_t* t2)
{
	if (!equivalent_types(t1->return_type, t2->return_type))
		return 0;

	if (!compatible_parameters(t1, t2))
		return 0;

	return 1;
}

static char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
	// Oh, this turned to be that easy
	return (cv1 == cv2);
}

static char compatible_parameters(function_info_t* t1, function_info_t* t2)
{
	if (t1->num_parameters != t2->num_parameters)
		return 0;

	char still_compatible = 1;
	int i;

	for (i = 0; (i < t1->num_parameters) && still_compatible; i++)
	{
		type_t* par1 = t1->parameter_list[i];
		type_t* par2 = t2->parameter_list[i];

		if (!equivalent_types(par1, par2))
		{
			// They are not equivalent types.
			//
			// Try to apply criteria of compatibility as defined in clause 13
			// of C++ standard

			/*
			 * Compatibility between pointers and first dimension of an array
			 *
			 * i.e.  
			 *       'int (*k)[10]' is compatible with     'int k[5][10]'
			 *       'int (*k)[10]' is NOT compatible with 'int k[5][15]'
			 */
			if ((par1->kind == TK_ARRAY && 
						par2->kind == TK_POINTER)
					|| (par1->kind == TK_POINTER && 
						par2->kind == TK_ARRAY))
			{
				type_t* array_type = (par1->kind == TK_ARRAY) ? par1 : par2;
				type_t* pointer_type = (par1->kind == TK_POINTER) ? par1 : par2;

				if (!equivalent_types(array_type->array->element_type, pointer_type->pointer->pointee))
				{
					still_compatible = 0;
				}
			}
			/*
			 * Compatibility between pointer to function and function parameter
			 *
			 * i.e.
			 *    'void f(int k(bool))' is compatible with 'void g(int (*t)(bool)'
			 */
			else if ((par1->kind == TK_FUNCTION &&
						par2->kind == TK_POINTER)
					|| (par1->kind == TK_POINTER &&
						par2->kind == TK_FUNCTION))
			{
				type_t* pointer_type = (par1->kind == TK_POINTER) ? par1 : par2;
				type_t* function_type = (par1->kind == TK_FUNCTION) ? par1 : par2;

				// Let's avoid unnecessary work
				if (pointer_type->pointer->pointee->kind != TK_FUNCTION)
				{
					still_compatible = 0;
				}
				else
				{
					if (!equivalent_types(pointer_type->pointer->pointee, function_type))
					{
						still_compatible = 0;
					}
				}
			}
			/*
			 * Compatibility between cv-qualified and non cv-qualified parameters
			 * in the outermost level of the parameter type specification
			 *
			 * i.e.
			 *    'void f(const int k)' is compatible with 'void g(int k)'
			 * 
			 * The outermost level is the same as the base type cv-qualification
			 */
			else 
			{
				type_t* base_t1 = base_type(par1);
				type_t* base_t2 = base_type(par2);
				cv_qualifier_t cv_qualif1 = base_t1->type->cv_qualifier;
				cv_qualifier_t cv_qualif2 = base_t2->type->cv_qualifier;

				// Save the cv_qualification for both types and try to match
				// them with an empty qualification (This can be improved, I
				// know)
				base_t1->type->cv_qualifier = CV_NONE;
				base_t2->type->cv_qualifier = CV_NONE;

				if (!equivalent_types(par1, par2))
				{
					still_compatible = 0;
				}

				// Restore the cv_qualifiers
				base_t1->type->cv_qualifier = cv_qualif1;
				base_t2->type->cv_qualifier = cv_qualif2;
			}

		}
	}

	return still_compatible;
}

static char is_typedef_type(type_t* t1)
{
	return (t1->kind == TK_DIRECT 
			&& t1->type->kind == STK_TYPEDEF);
}

static type_t* aliased_type(type_t* t1)
{
	if (!is_typedef_type(t1))
		internal_error("This is not a 'typedef' type", 0);

	return (t1->type->aliased_type);
}

static type_t* base_type(type_t* t1)
{
	while (t1->kind != TK_DIRECT)
	{
		switch (t1->kind)
		{
			case TK_POINTER :
			case TK_REFERENCE :
			case TK_POINTER_TO_MEMBER :
				t1 = t1->pointer->pointee;
				break;
			case TK_FUNCTION :
				t1 = t1->function->return_type;
				break;
			case TK_ARRAY :
				t1 = t1->array->element_type;
			default:
				internal_error("Unknown type kind", 0);
		}
	}

	return t1;
}
