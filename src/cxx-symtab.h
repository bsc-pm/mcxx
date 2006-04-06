#ifndef CXX_SYMTAB_H
#define CXX_SYMTAB_H

#include "cxx-ast.h"
#include "hash.h"

#define BITMAP(x) (1 << x)

/*
 * A symbol table is represented by a symtab_t*
 *
 * Entries in the symbol table are symtab_entry_t*
 *
 * Every entry can have a non-null type_information field type_t*
 * 
 * A type_t* represents a full C++ type in a hierarchical way.
 *   -> pointer
 *   -> array
 *   -> function
 *   -> type (direct type including builtin's, class, enums)
 */

enum cxx_symbol_kind
{
	SK_UNDEFINED = 0,
	SK_CLASS,
	SK_ENUM,
	SK_FUNCTION,
	SK_LABEL,
	SK_NAMESPACE,
	SK_VARIABLE,
	SK_TYPEDEF
};

typedef enum {
	CV_NONE = 0,
	CV_CONST = BITMAP(1),
	CV_VOLATILE = BITMAP(2),
	CV_RESTRICT = BITMAP(3)
} cv_qualifier_t;

typedef struct {
	int TODO;
} exception_spec_t;

// For type_t
enum type_kind
{
	TK_DIRECT,
	TK_POINTER,
	TK_REFERENCE,
	TK_POINTER_TO_MEMBER,
	TK_ARRAY,
	TK_FUNCTION
};

// For simple_type_t
typedef enum builtin_type_tag
{
	BT_UNKNOWN = 0,
	BT_INT,
	BT_BOOL,
	BT_FLOAT,
	BT_DOUBLE,
	BT_CHAR,
	BT_WCHAR,
	BT_VOID,
	BT_USER_DEFINED,
} builtin_type_t;

struct symtab_entry_tag;

typedef struct {
	char* name;
	AST value;
} enumeration_item_t;

typedef struct enum_information_tag {
	int num_enumeration;
	enumeration_item_t** enumeration_list;
} enum_info_t;

typedef enum access_specifier_t
{
	AS_UNKNOWN = 0,
	AS_PUBLIC,
	AS_PRIVATE,
	AS_PROTECTED
} access_specifier_t;

struct simple_type_tag;

typedef struct {
	// Access specifier
	access_specifier_t access_spec;
	// Member name, this will come from the declarator 
	char* name;
	// Type of the member
	struct type_tag* type_info;
} member_item_t;

typedef struct class_information_tag {
	int num_members;
	member_item_t** member_list;
} class_info_t;

// Direct type (including classes and enums)
typedef struct simple_type_tag {
	builtin_type_t builtin_type;
	char is_long; // This can be 0, 1 or 2
	char is_short;
	char is_unsigned;
	char is_signed;

	// Previously declared type
	struct symtab_entry_tag* user_defined_type;

	// For enums
	enum_info_t* enum_info;
	
	// For classes
	class_info_t* class_info;

	cv_qualifier_t cv_qualifier;
} simple_type_t;

// Function information
typedef struct function_tag
{
	struct type_tag* return_type;
	int num_parameters;
	struct type_tag** parameter_list;
	cv_qualifier_t cv_qualifier;
	exception_spec_t exception_spec;

	int is_inline;
	int is_virtual;
	int is_pure; // is_pure implies is_virtual
	int is_static; // local linkage
	int is_explicit;
} function_info_t;

// Pointers, references and pointers to members
typedef struct pointer_tag
{
	cv_qualifier_t cv_qualifier;
	struct type_tag* pointee;
	// ¿¿¿ If pointer to member ???
	// struct symtab_entry_tag* pointee_class;
} pointer_info_t;

// Array information
typedef struct array_tag
{
	AST array_expr;
	struct type_tag* element_type;
} array_info_t;

// This structure is able to hold type information for a given symbol
// note it being decoupled from its declarator 
typedef struct type_tag
{
	// Kind of the type
	enum type_kind kind;

	// Pointer
	pointer_info_t* pointer;

	// Array
	array_info_t* array;

	// Function
	function_info_t* function;

	// "Simple" type
	simple_type_t* type;
} type_t;

struct symtab_tag;

typedef struct symtab_entry_tag
{
	enum cxx_symbol_kind kind;
	char* symbol_name;

	// Scope of this symbol when declared
	struct symtab_tag* scope;

	type_t* type_information;
} symtab_entry_t;

typedef struct symtab_tag
{
	Hash* hash;

	// Can be null 
	struct symtab_tag* parent;
} symtab_t;

#undef BITMAP

// Functions to handle symbol table
symtab_t* new_symtab();
symtab_t* enter_scope(symtab_t* parent);
symtab_entry_t* new_symbol(symtab_t* st, char* name);
symtab_entry_t* query_in_current_scope(symtab_t* st, char* name);
symtab_entry_t* query_in_current_and_upper_scope(symtab_t* st, char* name);

#endif // CXX_SYMTAB_H
