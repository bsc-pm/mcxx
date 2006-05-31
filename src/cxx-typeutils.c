#include <stdio.h>
#include <string.h>
#include <gc.h>
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-prettyprint.h"

/*
 * This file contains routines destined to work with types.  Comparing two
 * types, comparing function declarations and definitions, etc.
 */
static char is_typedef_type(type_t* t);
static type_t* aliased_type(type_t* t);
static type_t* base_type(type_t* t);
static char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2, enum cv_equivalence_t cv_equiv);
static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2, scope_t* st, enum cv_equivalence_t cv_equiv);
static char equivalent_array_type(array_info_t* t1, array_info_t* t2, scope_t* st, enum cv_equivalence_t cv_equiv);
static char equivalent_function_type(function_info_t* t1, function_info_t* t2, scope_t* st, enum cv_equivalence_t cv_equiv);
static char compatible_parameters(function_info_t* t1, function_info_t* t2, scope_t* st, enum cv_equivalence_t cv_equiv);

/*
 * States if two types are equivalent. This means that they are the same
 * (ignoring typedefs). Just plain comparison, no standard conversion is
 * performed. cv-qualifiers are relevant for comparison
 */
char equivalent_types(type_t* t1, type_t* t2, scope_t* st, enum cv_equivalence_t cv_equiv)
{
	if (t1 == NULL || t2 == NULL)
		return 1;

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
			return equivalent_simple_types(t1->type, t2->type, st, cv_equiv);
			break;
		case TK_POINTER :
			return equivalent_pointer_type(t1->pointer, t2->pointer, st, cv_equiv);
			break;
		case TK_REFERENCE :
			return equivalent_pointer_type(t1->pointer, t2->pointer, st, cv_equiv);
			break;
		case TK_POINTER_TO_MEMBER :
			break;
		case TK_ARRAY :
			return equivalent_array_type(t1->array, t2->array, st, cv_equiv);
			break;
		case TK_FUNCTION :
			return equivalent_function_type(t1->function, t2->function, st, cv_equiv);
			break;
		default :
			internal_error("Unknown type kind (%d)\n", t1->kind);
	}

	return 0;
}

char equivalent_simple_types(simple_type_t *t1, simple_type_t *t2, scope_t* st, enum cv_equivalence_t cv_equiv)
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
			return equivalent_builtin_type(t1, t2, cv_equiv);
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
					t2->user_defined_type->type_information, st, cv_equiv);
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

char equivalent_builtin_type(simple_type_t* t1, simple_type_t *t2, enum cv_equivalence_t cv_equiv)
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
	
	if (!equivalent_cv_qualification(t1->cv_qualifier, t2->cv_qualifier, cv_equiv))
	{
		return 0;
	}

	// Ok, nothing makes us think they might be different
	return 1;
}

static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2, scope_t* st, enum cv_equivalence_t cv_equiv)
{
	if (!equivalent_types(t1->pointee, t2->pointee, st, cv_equiv))
	{
		return 0;
	}

	return (equivalent_cv_qualification(t1->cv_qualifier, t2->cv_qualifier, cv_equiv));
}

static char equivalent_array_type(array_info_t* t1, array_info_t* t2, scope_t* st, enum cv_equivalence_t cv_equiv)
{
	if (!equivalent_types(t1->element_type, t2->element_type, st, cv_equiv))
		return 0;

	literal_value_t v1 = evaluate_constant_expression(t1->array_expr, st);
	literal_value_t v2 = evaluate_constant_expression(t2->array_expr, st);

	if (!equal_literal_values(v1, v2, st))
		return 0;
	
	return 1;
}

char overloaded_function(function_info_t* t1, function_info_t* t2, scope_t* st, enum cv_equivalence_t cv_equiv)
{
	if (!compatible_parameters(t1, t2, st, cv_equiv))
		return 1;

	// If one has return type but the other does not this is an overload
	// (technically this is ill-formed)
	if (((t1->return_type->kind == TK_DIRECT && t1->return_type->type == NULL)
				&& (t2->return_type->kind == TK_DIRECT && t2->return_type->type != NULL))
			|| ((t2->return_type->kind == TK_DIRECT && t2->return_type->type == NULL)
				&& (t1->return_type->kind == TK_DIRECT && t1->return_type->type != NULL)))
		return 1;
			

	// Destructors, constructors, operator functions and conversion functions
	// will not have a full direct type
	if ((t1->return_type->kind == TK_DIRECT && t1->return_type->type == NULL)
			&& (t2->return_type->kind == TK_DIRECT && t2->return_type->type == NULL))
		return 0;

	if (!equivalent_types(t1->return_type, t2->return_type, st, cv_equiv))
	{
		running_error("You are trying to overload a function by only modifying its return type", 0);
	}

	return 0;
}

static char equivalent_function_type(function_info_t* t1, function_info_t* t2, scope_t* st, enum cv_equivalence_t cv_equiv)
{
	if (!equivalent_types(t1->return_type, t2->return_type, st, cv_equiv))
		return 0;

	if (!compatible_parameters(t1, t2, st, cv_equiv))
		return 0;

	return 1;
}

static char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2, enum cv_equivalence_t cv_equiv)
{
	// Oh, this turned to be that easy
	if (cv_equiv == CVE_CONSIDER)
	{
		return (cv1 == cv2);
	}
	else
	{
		return 1;
	}
}

static char compatible_parameters(function_info_t* t1, function_info_t* t2, scope_t* st, enum cv_equivalence_t cv_equiv)
{
	if (t1->num_parameters != t2->num_parameters)
		return 0;

	char still_compatible = 1;
	int i;

	for (i = 0; (i < t1->num_parameters) && still_compatible; i++)
	{
		type_t* par1 = t1->parameter_list[i]->type_info;
		type_t* par2 = t2->parameter_list[i]->type_info;

		if (!equivalent_types(par1, par2, st, cv_equiv))
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

				if (!equivalent_types(array_type->array->element_type, pointer_type->pointer->pointee, st, cv_equiv))
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
					if (!equivalent_types(pointer_type->pointer->pointee, function_type, st, cv_equiv))
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

				if (!equivalent_types(par1, par2, st, cv_equiv))
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
	if ((t1->kind == TK_DIRECT 
			&& t1->type->kind == STK_TYPEDEF))
	{
		return 1;
	}

	if (t1->kind == TK_DIRECT
			&& t1->type->kind == STK_USER_DEFINED)
	{
		scope_entry_t* user_defined_entry = t1->type->user_defined_type;
		type_t* user_defined_type = user_defined_entry->type_information;

		if (user_defined_type->kind == TK_DIRECT &&
				user_defined_type->type->kind == STK_TYPEDEF)
		{
			return 1;
		}
	}

	return 0;
}

static type_t* aliased_type(type_t* t1)
{
	if (!is_typedef_type(t1))
		internal_error("This is not a 'typedef' type", 0);

	if (t1->kind == TK_DIRECT && t1->type->kind == STK_TYPEDEF)
	{
		return (t1->type->aliased_type);
	}
	else
	{
		scope_entry_t* user_defined_entry = t1->type->user_defined_type;
		type_t* user_defined_type = user_defined_entry->type_information;

		return user_defined_type->type->aliased_type;
	}
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
				break;
			default:
				internal_error("Unknown type kind %d", t1->kind);
		}
	}

	return t1;
}

char is_fundamental_type(type_t* t)
{
	// Advance over typedefs
	while (t->kind == TK_DIRECT
			&& t->type->kind == STK_TYPEDEF)
	{
		t = t->type->aliased_type;
	}

	return (t->kind == TK_DIRECT
			&& t->type->kind == STK_BUILTIN_TYPE);
}

char is_integral_type(type_t* t)
{
	// Advance over typedefs
	while (t->kind == TK_DIRECT
			&& t->type->kind == STK_TYPEDEF)
	{
		t = t->type->aliased_type;
	}

	return (t->kind == TK_DIRECT
			&& t->type->kind == STK_BUILTIN_TYPE
			&& t->type->builtin_type == BT_INT);
}

char is_floating_type(type_t* t)
{
	// Advance over typedefs
	while (t->kind == TK_DIRECT
			&& t->type->kind == STK_TYPEDEF)
	{
		t = t->type->aliased_type;
	}

	return (t->kind == TK_DIRECT
			&& t->type->kind == STK_BUILTIN_TYPE
			&& (t->type->builtin_type == BT_FLOAT
				|| t->type->builtin_type == BT_DOUBLE));
}

char can_be_promoted_to_dest(type_t* orig, type_t* dest)
{
	simple_type_t* orig_simple_type = orig->type;
	simple_type_t* dest_simple_type = dest->type;

	// A float always can be promoted to double
	if (orig_simple_type->builtin_type == BT_FLOAT
			&& dest_simple_type->builtin_type == BT_DOUBLE)
	{
		return 1;
	}

	// A wchar_t can be promoted to a plain int
#warning "This depends on the exact environment"
	if (orig_simple_type->builtin_type == BT_WCHAR
			&& dest_simple_type->builtin_type == BT_INT
			&& !dest_simple_type->is_short
			&& !dest_simple_type->is_long
			&& !dest_simple_type->is_unsigned)
	{
		return 1;
	}

	// A bool can be promoted to a plain int
#warning "This depends on the exact environment"
	if (orig_simple_type->builtin_type == BT_BOOL
			&& dest_simple_type->builtin_type == BT_INT
			&& !dest_simple_type->is_short
			&& !dest_simple_type->is_long
			&& !dest_simple_type->is_unsigned)
	{
		return 1;
	}

	// A short, either signed or unsigned, can be promoted to a plain int
#warning "This depends on the exact environment"
	if (orig_simple_type->builtin_type == BT_INT
			&& orig_simple_type->is_short
			&& dest_simple_type->builtin_type == BT_INT
			&& !dest_simple_type->is_short
			&& !dest_simple_type->is_long
			&& !dest_simple_type->is_unsigned)
	{
		return 1;
	}

	// A char, either signed or unsigned, can be promoted to a plain int
#warning "This depends on the exact environment"
	if (orig_simple_type->builtin_type == BT_CHAR
			&& dest_simple_type->builtin_type == BT_INT
			&& !dest_simple_type->is_short
			&& !dest_simple_type->is_long
			&& !dest_simple_type->is_unsigned)
	{
		return 1;
	}

#warning Missing the case for bitfields

	// Doesn't look promotionable to me
	return 0;
}

char can_be_converted_to_dest(type_t* orig, type_t* dest)
{
	simple_type_t* orig_simple_type = orig->type;
	simple_type_t* dest_simple_type = dest->type;

	// Anything can be converted to anything fundamental (except for void
	// types, that in general should not appear in the code as rvalues ...)
	if (orig_simple_type->builtin_type != BT_VOID
			&& dest_simple_type->builtin_type != BT_VOID)
	{
		return 1;
	}

	// Does not look convertible
	return 0;
}


char pointer_can_be_converted_to_dest_rec(type_t* orig, type_t* dest, scope_t* st, char* all_previous_are_const)
{
	if (orig->kind != dest->kind)
	{
		return 0;
	}
	// orig->kind == dest->kind

	if (orig->kind != TK_POINTER) 
	{
		return equivalent_types(orig, dest, st, CVE_CONSIDER);
	}

	// orig->kind == dest->kind == TK_POINTER
	// Example:
	//    orig : int * const * const a;
	//    dest:  int * const *       a;
	//
	//  (orig can be converted to dest)

	// If the dest pointer is qualified, so does have to the orig one
	if (dest->pointer->cv_qualifier != CV_NONE 
			&& dest->pointer->cv_qualifier != orig->pointer->cv_qualifier)
	{
		return 0;
	}

	// If the origin pointer is const-qualified every previous pointer
	// should have been const-qualified
	if ((dest->pointer->cv_qualifier & CV_CONST) == CV_CONST)
	{
		if (!(*all_previous_are_const))
		{
			return 0;
		}
	}
	else
	{
		*all_previous_are_const = 0;
	}

	return pointer_can_be_converted_to_dest_rec(orig->pointer->pointee, dest->pointer->pointee, st, all_previous_are_const);
}

char pointer_can_be_converted_to_dest(type_t* orig, type_t* dest, scope_t* st)
{
	// This holds for the first pointer
	char all_previous_are_const = 1;

	return pointer_can_be_converted_to_dest_rec(orig, dest, st, &all_previous_are_const);
}

/*
 * This function just creates a full type_t from a simple_type_t.
 * It is useful when no declarator information is available.
 */
type_t* simple_type_to_type(simple_type_t* simple_type_info)
{
	type_t* result = GC_CALLOC(1, sizeof(*result));
	result->kind = TK_DIRECT;
	// result->type = copy_simple_type(simple_type_info);
	result->type = simple_type_info;

	return result;
}

/* Copy functions */

// This function copies the type information of an enum
enum_info_t* copy_enum_info(enum_info_t* enum_info)
{
	enum_info_t* result = GC_CALLOC(1, sizeof(*result));

	*result = *enum_info;

	int i;
	for (i = 0; i < result->num_enumeration; i++)
	{
		// Note, we copy the references here
		result->enumeration_list[i] = enum_info->enumeration_list[i];
	}

	return result;
}

// This function copies the type information of a pointer
pointer_info_t* copy_pointer_info(pointer_info_t* pointer_info)
{
	pointer_info_t* result = GC_CALLOC(1, sizeof(*result));
	*result = *pointer_info;
	
	result->pointee = copy_type(result->pointee);

	return result;
}

// This function copies the type information of an array
array_info_t* copy_array_info(array_info_t* array_info)
{
	array_info_t* result = GC_CALLOC(1, sizeof(*result));
	*result = *array_info;
	
	result->array_expr = duplicate_ast(array_info->array_expr);
	result->element_type = copy_type(array_info->element_type);
	
	return result;
}

// This function copies the type information of a function
function_info_t* copy_function_info(function_info_t* function_info)
{
	function_info_t* result = GC_CALLOC(1, sizeof(*result));
	*result = *function_info;

	result->return_type = copy_type(function_info->return_type);
	
	int i;
	for (i = 0; i < function_info->num_parameters; i++)
	{
		result->parameter_list[i]->type_info = copy_type(function_info->parameter_list[i]->type_info);
		if (function_info->parameter_list[i]->default_argument != NULL)
		{
			result->parameter_list[i]->default_argument = duplicate_ast(function_info->parameter_list[i]->default_argument);
		}
	}
	
	return result;
}

// This function copies a full fledged type
type_t* copy_type(type_t* type)
{
	type_t* result = GC_CALLOC(1, sizeof(*result));

	*result = *type;

	if (result->pointer != NULL)
	{
		result->pointer = copy_pointer_info(type->pointer);
	}

	if (result->array != NULL)
	{
		result->array = copy_array_info(type->array);
	}

	if (result->function != NULL)
	{
		result->function = copy_function_info(type->function);
	}

	if (result->type != NULL)
	{
		result->type = copy_simple_type(type->type);
	}

	return result;
}

// This function copies class type information
class_info_t* copy_class_info(class_info_t* class_info)
{
	class_info_t* result = GC_CALLOC(1, sizeof(*result));

	*result = *class_info;

	return result;
}

// This function copies a simple type
simple_type_t* copy_simple_type(simple_type_t* type_info)
{
	simple_type_t* result = GC_CALLOC(1, sizeof(*result));

	// Bitwise copy for every thing that can be directly copied
	*result = *type_info;

	if (result->enum_info != NULL)
	{
		result->enum_info = copy_enum_info(type_info->enum_info);
	}

	if (result->class_info != NULL)
	{
		result->class_info = copy_class_info(type_info->class_info);
	}

	return result;
}

// Gives the name of a builtin type
const char* get_builtin_type_name(simple_type_t* simple_type_info, scope_t* st)
{
	static char result[256];

	memset(result, 0, sizeof(char)*255);
	if ((simple_type_info->cv_qualifier & CV_CONST) == CV_CONST)
	{
		strcat(result, "const ");
	}

	if ((simple_type_info->cv_qualifier & CV_VOLATILE) == CV_VOLATILE)
	{
		strcat(result, "volatile ");
	}

	switch (simple_type_info->kind)
	{
		case STK_BUILTIN_TYPE :
			{
				switch (simple_type_info->builtin_type)
				{
					case BT_INT :
						strcat(result, "int");
						break;
					case BT_BOOL :
						strcat(result, "bool");
						break;
					case BT_FLOAT :
						strcat(result, "float");
						break;
					case BT_DOUBLE :
						strcat(result, "double");
						break;
					case BT_WCHAR :
						strcat(result, "wchar_t");
						break;
					case BT_CHAR :
						strcat(result, "char");
						break;
					case BT_VOID :
						strcat(result, "void");
						break;
					case BT_UNKNOWN :
					default :
						strcat(result, "¿¿¿unknown builtin type???");
						break;
				}
				break;
			}
		case STK_USER_DEFINED :
			{
				scope_entry_t* user_defined_type = simple_type_info->user_defined_type;
				switch (user_defined_type->kind)
				{
					case SK_ENUM :
						snprintf(result, 255, "enum %s", user_defined_type->symbol_name);
						break;
					case SK_CLASS :
						snprintf(result, 255, "class %s", user_defined_type->symbol_name);
						break;
					case SK_TYPEDEF :
						snprintf(result, 255, "typedef %s", user_defined_type->symbol_name);
						break;
					case SK_TEMPLATE_PARAMETER :
						snprintf(result, 255, "type template parameter #%d %s", 
								user_defined_type->type_information->type->template_parameter_num,
								user_defined_type->symbol_name);
						break;
					case SK_TEMPLATE_PRIMARY_CLASS :
						snprintf(result, 255, "primary template class %s (%p)", user_defined_type->symbol_name, user_defined_type);
						break;
					case SK_TEMPLATE_SPECIALIZED_CLASS :
						snprintf(result, 255, "specialized template class %s (%p)", user_defined_type->symbol_name, user_defined_type);
						break;
					default :
						strcat(result, "¿¿¿unknown user defined type???");
				}
				break;
			}
		case STK_ENUM :
			strcat(result, "enum <anonymous>");
			break;
		case STK_CLASS :
			strcat(result, "class <anonymous>");
			break;
		case STK_TYPE_TEMPLATE_PARAMETER :
			strcat(result, "template type parameter T");
			break;
		default :
			{
				break;
			}
	}


	return result;
}

// This prints a declarator in English. It is intended for debugging purposes
void print_declarator(type_t* printed_declarator, scope_t* st)
{
	do 
	{
		switch (printed_declarator->kind)
		{
			case TK_DIRECT :
				if (printed_declarator->type != NULL)
				{
					fprintf(stderr, "%s", get_builtin_type_name(printed_declarator->type, st));
				}
				else
				{
					fprintf(stderr, "(nothing)");
				}
				printed_declarator = NULL;
				break;
			case TK_POINTER :
				if ((printed_declarator->pointer->cv_qualifier & CV_CONST) == CV_CONST)
				{
					fprintf(stderr, "const ");
				}
				if ((printed_declarator->pointer->cv_qualifier & CV_VOLATILE) == CV_VOLATILE)
				{
					fprintf(stderr, "volatile ");
				}
				fprintf(stderr, "pointer to ");
				printed_declarator = printed_declarator->pointer->pointee;
				break;
			case TK_REFERENCE :
				if ((printed_declarator->pointer->cv_qualifier & CV_CONST) == CV_CONST)
				{
					fprintf(stderr, "const ");
				}
				if ((printed_declarator->pointer->cv_qualifier & CV_VOLATILE) == CV_VOLATILE)
				{
					fprintf(stderr, "volatile ");
				}
				fprintf(stderr, "reference to ");
				printed_declarator = printed_declarator->pointer->pointee;
				break;
			case TK_POINTER_TO_MEMBER :
				if ((printed_declarator->pointer->cv_qualifier & CV_CONST) == CV_CONST)
				{
					fprintf(stderr, "const ");
				}
				if ((printed_declarator->pointer->cv_qualifier & CV_VOLATILE) == CV_VOLATILE)
				{
					fprintf(stderr, "volatile ");
				}
				fprintf(stderr, "pointer to member of ");
				print_declarator(printed_declarator->pointer->pointee_class->type_information, st);
				fprintf(stderr, " to ");
				printed_declarator = printed_declarator->pointer->pointee;
				break;
			case TK_ARRAY :
				fprintf(stderr, "array ");
				prettyprint(stderr, printed_declarator->array->array_expr);
				fprintf(stderr, " of ");
				printed_declarator = printed_declarator->array->element_type;
				break;
			case TK_FUNCTION :
				{
					int i;
					fprintf(stderr, "function (");
					for (i = 0; i < printed_declarator->function->num_parameters; i++)
					{
						print_declarator(printed_declarator->function->parameter_list[i]->type_info, st);
						if ((i+1) < printed_declarator->function->num_parameters)
						{
							fprintf(stderr, ", ");
						}
					}
					fprintf(stderr, ")");
					if ((printed_declarator->function->cv_qualifier & CV_CONST) == CV_CONST)
					{
						fprintf(stderr, " const");
					}
					if ((printed_declarator->function->cv_qualifier & CV_VOLATILE) == CV_VOLATILE)
					{
						fprintf(stderr, " volatile");
					}
					fprintf(stderr, " returning ");
					printed_declarator = printed_declarator->function->return_type;
					break;
				}
			default :
				internal_error("Unhandled type kind '%d'\n", printed_declarator->kind);
				break;
		}
	} while (printed_declarator != NULL);
}
