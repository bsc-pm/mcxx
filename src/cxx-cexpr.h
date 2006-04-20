#ifndef CXX_CEXPR_H
#define CXX_CEXPR_H

enum literal_value_kind_t
{
	LVK_INVALID = 0,
	LVK_SIGNED_INT,
	LVK_UNSIGNED_INT,
	LVK_SIGNED_LONG,
	LVK_UNSIGNED_LONG,
	LVK_BOOL,
	LVK_CHARACTER,
	LVK_WCHAR_T
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
		wchar_t wide_character_value;
		// TODO - Float values ?
	} value;
} literal_value_t;

#endif // CXX_CEXPR_H
