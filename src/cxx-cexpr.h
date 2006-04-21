#ifndef CXX_CEXPR_H
#define CXX_CEXPR_H

#include "cxx-symtab.h"

enum literal_value_kind_t
{
	LVK_INVALID = 0,
	LVK_SIGNED_INT,
	LVK_UNSIGNED_INT,
	LVK_SIGNED_LONG,
	LVK_UNSIGNED_LONG,
	LVK_BOOL,
	LVK_CHARACTER
	// TODO - Float values ?
};

typedef struct
{
	enum literal_value_kind_t kind;

	union 
	{
		signed int signed_int;
		unsigned int unsigned_int;
		signed long int signed_long;
		unsigned long int unsigned_long;
		char boolean_value;
		char character_value;
		// TODO - Float values ?
	} value;
} literal_value_t;

literal_value_t evaluate_constant_expression(AST a, symtab_t* st);
char value_is_zero(literal_value_t v);
literal_value_t literal_value_zero();
literal_value_t literal_value_minus_one();
literal_value_t increment_literal_value(literal_value_t e);
AST tree_from_literal_value(literal_value_t e);
char equal_literal_values(literal_value_t v1, literal_value_t v2, symtab_t* st);

#endif // CXX_CEXPR_H
