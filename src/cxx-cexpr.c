#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "cxx-cexpr.h"
#include "cxx-ast.h"
#include "cxx-utils.h"

/*
 * This file implements an evaluator of constant expressions in C++
 */
static void gather_integer_literal_suffix(char* text, char* is_long, char* is_unsigned);

void promote_values(literal_value_t v1, literal_value_t v2, 
		literal_value_t* out_v1, literal_value_t* out_v2)
{
	*out_v1 = v1;
	*out_v2 = v2;
	if (v1.kind == v2.kind)
	{
		return;
	}

	switch (v1.kind)
	{
		case LVK_UNSIGNED_LONG :
			{
				out_v2->kind = LVK_UNSIGNED_LONG;
				switch (v2.kind)
				{
					case LVK_SIGNED_INT :
						{
							out_v2->value.unsigned_long = (unsigned long) v2.value.signed_int;
							break;
						}
					case LVK_UNSIGNED_INT :
						{
							out_v2->value.unsigned_long = (unsigned long) v2.value.unsigned_int;
							break;
						}
					case LVK_SIGNED_LONG :
						{
							out_v2->value.unsigned_long = (unsigned long) v2.value.signed_long;
							break;
						}
					default :
						internal_error("Unknown value kind", 0);
				}
				break;
			}
		default :
			break;
	}
}

literal_value_t create_value_from_literal(AST a)
{
	literal_value_t result;
	memset(&result, 0, sizeof(result));

	char is_long, is_unsigned;
	char* literal_text = ASTText(a);
	// literal rule
	switch (ASTType(a))
	{
		case AST_DECIMAL_LITERAL :
		case AST_OCTAL_LITERAL :
		case AST_HEXADECIMAL_LITERAL :
		case AST_FLOATING_LITERAL :
			gather_integer_literal_suffix(literal_text, &is_long, &is_unsigned);
			if (is_long) 
			{
				if (is_unsigned)
				{
					result.kind = LVK_UNSIGNED_LONG;
					result.value.unsigned_long = strtoul(literal_text, NULL, 0);
				}
				else
				{
					result.kind = LVK_SIGNED_LONG;
					result.value.unsigned_long = strtol(literal_text, NULL, 0);
				}
			}
			else // Is not long
			{
				if (is_unsigned)
				{
					result.kind = LVK_UNSIGNED_INT;
					result.value.unsigned_int = strtod(literal_text, NULL);
				}
				else 
				{
					result.kind = LVK_SIGNED_INT;
					result.value.signed_int = strtod(literal_text, NULL);
				}
			}
			break;
		case AST_BOOLEAN_LITERAL : 
			result.kind = LVK_BOOL;
			result.value.boolean_value = (strcmp(literal_text, "false")) ? 0 : 1;
			break;
		case AST_CHARACTER_LITERAL :
			if (toupper(literal_text[0]) == 'L')
			{
				// We should use iconv or something similar to get this correct
				internal_error("Unsupported wide character in constant expression evaluation", 0);
			}
			else
			{
				result.kind = LVK_CHARACTER;
				// literal_text[0] is '\''
				result.value.character_value = literal_text[1];
			}
			break;
		default :
			internal_error("Unknown literal", 0);
			break;
	}

	return result;
}

static void gather_integer_literal_suffix(char* text, char* is_long, char* is_unsigned)
{
	int i = 0;
	char* suffix = &text[strlen(text) - 1];

	*is_long = 0;
	*is_unsigned = 0;

	while ((i < 3)
			&& (toupper(*suffix) == 'U' 
				|| toupper(*suffix) == 'L'))
	{
		if (toupper(*suffix) == 'U')
		{
			*is_long++;
		}
		else // if (toupper(*suffix) == 'L')
		{
			*is_unsigned = 1;
		}
		suffix--;
		i++;
	}
}
