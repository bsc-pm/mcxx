#include "cxx-typeutils.h"
#include "cxx-utils.h"

/*
 * This file contains routines destined to work with types.  Comparing two
 * types, comparing function declarations and definitions, etc.
 */
static char is_typedef_type(type_t* t);
type_t* aliased_type(type_t* t);
static char equivalent_simple_types(simple_type_t *t1, simple_type_t *t2);
static char equivalent_builtin_type(simple_type_t *t1, simple_type_t *t2);
static char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2);

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
			break;
		case TK_REFERENCE :
			break;
		case TK_POINTER_TO_MEMBER :
			break;
		case TK_ARRAY :
			break;
		case TK_FUNCTION :
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

static char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
	// Oh, this turned to be that easy
	return (cv1 == cv2);
}

static char is_typedef_type(type_t* t1)
{
	return (t1->kind == TK_DIRECT 
			&& t1->type->kind == STK_TYPEDEF);
}

type_t* aliased_type(type_t* t1)
{
	if (!is_typedef_type(t1))
		internal_error("This is not a 'typedef' type", 0);

	return (t1->type->aliased_type);
}
