#ifndef CXX_OVERLOAD_H
#define CXX_OVERLOAD_H

#include "cxx-ast.h"
#include "cxx-scope.h"

enum ics_kind
{
	ICS_UNKNOWN = 0,
	ICS_STANDARD,
	ICS_USER_DEFINED,
	ICS_ELLIPSIS,
};

#define BITMAP(x) (1 << x)

enum scs_category_t
{
	SCS_UNKNOWN = 0,
	SCS_IDENTITY = BITMAP(1),
	SCS_LVALUE_TRANSFORMATION = BITMAP(2),
	SCS_PROMOTION = BITMAP(3),
	SCS_CONVERSION = BITMAP(4),
	SCS_QUALIFICATION_ADJUSTMENT = BITMAP(5),
};

#undef BITMAP

enum udc_category_t
{
	UDC_UNKNOWN = 0,
	UDC_VALID,
	UDC_AMBIGUOUS
};

typedef struct standard_conversion_sequence_tag
{
	enum scs_category_t scs_category;

	type_t* orig_type;
	type_t* dest_type;

	char is_pointer_to_bool;
	char is_nonvoid_pointer_to_void;
} standard_conversion_sequence_t;

typedef struct user_defined_conversion_tag
{
	enum udc_category_t udc_category;

	conversion_function_t* udc_conv_funct;
	scope_entry_t* udc_constr_funct;
} user_defined_conversion_t;

typedef struct one_implicit_conversion_sequence_tag
{
	enum ics_kind kind;

	standard_conversion_sequence_t standard_conversion[2];
	user_defined_conversion_t user_defined;
} one_implicit_conversion_sequence_t;

// Represents a whole ICS for all arguments
typedef struct implicit_conversion_sequence_tag
{
	int num_arg;
	one_implicit_conversion_sequence_t** conversion;
} implicit_conversion_sequence_t;

typedef struct viable_function_list_tag
{
	scope_entry_t* entry;
	int ics_num_args;
	implicit_conversion_sequence_t* ics;
	struct viable_function_list_tag* next;
} viable_function_list_t;


#endif // CXX_OVERLOAD
