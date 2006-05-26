#ifndef CXX_SCOPE_H
#define CXX_SCOPE_H

#include "cxx-ast.h"
#include "hash.h"

#define BITMAP(x) (1 << x)

/*
 * A scope is represented by a scope_t*
 *
 * Entries in the scope are scope_entry_t*
 *
 * Every entry can have a non-null type_information field type_t*
 * 
 * A type_t* represents a full C++ type in a hierarchical way.
 *   -> pointer
 *   -> array
 *   -> function
 *   -> type (direct type including builtin's, class, enums, typedef)
 */

enum cxx_symbol_kind
{
	SK_UNDEFINED = 0,
	SK_CLASS,
	SK_ENUM,
	SK_ENUMERATOR, // The elements an enum is made of
	SK_FUNCTION,
	SK_LABEL,
	SK_NAMESPACE,
	SK_VARIABLE,
	SK_TYPEDEF,
	// Lots of stuff related to the C++ "template madness"
	SK_TEMPLATE_PRIMARY_CLASS,
	SK_TEMPLATE_SPECIALIZED_CLASS,
	SK_TEMPLATE_FUNCTION,
	SK_TEMPLATE_PARAMETER
};

typedef enum {
	CV_NONE = 0,
	CV_CONST = BITMAP(1),
	CV_VOLATILE = BITMAP(2),
	CV_RESTRICT = BITMAP(3)
} cv_qualifier_t;

typedef struct {
	int num_exception_types;
	struct type_tag** exception_type_seq;
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
} builtin_type_t;

typedef enum simple_type_kind_tag
{
	STK_BUILTIN_TYPE,
	STK_CLASS,
	STK_ENUM,
	STK_TYPEDEF,
	STK_USER_DEFINED,
	// Templates stuff
	STK_TYPE_TEMPLATE_PARAMETER
} simple_type_kind_t;

struct scope_entry_tag;

typedef struct {
	char* name;
	AST value;
} enumeration_item_t;

typedef struct enum_information_tag {
	int num_enumeration;
	struct scope_entry_tag** enumeration_list;
} enum_info_t;

enum template_parameter_kind
{
	TPK_UNKNOWN = 0,
	TPK_NONTYPE,
	TPK_TYPE,
	TPK_TEMPLATE
};

typedef struct template_parameter {
	enum template_parameter_kind kind;

	struct type_tag* type_info;

	AST default_argument;
} template_parameter_t;

typedef enum access_specifier_t
{
	AS_UNKNOWN = 0,
	AS_PUBLIC,
	AS_PRIVATE,
	AS_PROTECTED
} access_specifier_t;

struct simple_type_tag;

enum class_kind_t {
	CK_STRUCT,
	CK_CLASS,
	CK_UNION
};

typedef struct class_information_tag {
	enum class_kind_t class_kind;

	int is_template;

	// Special functions
	struct scope_entry_tag* destructor;

	int num_conversion_functions;
	struct scope_entry_tag** conversion_function_list;

	int num_operator_functions;
	struct scope_entry_tag** operator_function_list;

	int num_constructors;
	struct scope_entry_tag** constructor_list;
} class_info_t;

enum template_argument_kind
{
	TAK_UNDEFINED = 0,
	TAK_NONTYPE,
	TAK_TYPE
};

typedef struct template_argument_tag
{
	enum template_argument_kind kind;

	// If TAK_NONTYPE this is the constant expression
	AST expression;

	// Otherwise we should have type_t here
	struct type_tag* type;
} template_argument_t;

typedef struct template_argument_list_tag {
	int num_arguments;
	template_argument_t** argument_list;
} template_argument_list_t;

// Direct type (including classes and enums)
typedef struct simple_type_tag {
	simple_type_kind_t kind;
	builtin_type_t builtin_type;
	char is_long; // This can be 0, 1 or 2
	char is_short;
	char is_unsigned;
	char is_signed;

	// Previously declared type. should be completely "cv-unqualified"
	//
	// If this is a STK_TEMPLATE_CLASS this will be NULL since there
	// is no "real type" backing this
	struct scope_entry_tag* user_defined_type;

	// For typedefs
	struct type_tag* aliased_type;

	// For enums
	enum_info_t* enum_info;
	
	// For classes
	class_info_t* class_info;

	// For template classes
	// Template arguments for specializations and instantiations
	template_argument_list_t* template_arguments;
	
	// For template parameters, the positional number of this argument
	// in the template
	int template_parameter_num;

	cv_qualifier_t cv_qualifier;
} simple_type_t;

// Function information
typedef struct function_tag
{
	struct type_tag* return_type;
	int num_parameters;
	struct type_tag** parameter_list;
	cv_qualifier_t cv_qualifier;
	exception_spec_t* exception_spec;

	AST function_body;

	int is_template;

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

	struct scope_entry_tag* pointee_class;
} pointer_info_t;

// Array information
typedef struct array_tag
{
	AST array_expr;
	struct type_tag* element_type;
} array_info_t;

// States the "temporarieness" of a type
typedef enum 
{
	UNDEFINED_TEMPORARY = 0,
	NOT_TEMPORARY,
	IS_TEMPORARY
} temporary_status_t;

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

	// Used only in type calculus (states if a type is a temporary)
	temporary_status_t temporary_status;
} type_t;

struct scope_tag;

// This is an entry in the scope
typedef struct scope_entry_tag
{
	char* symbol_name;
	enum cxx_symbol_kind kind;

	// This allows us to enforce the one-definition-rule within a translation unit
	int defined;

	// Scope of this entry when declared
	struct scope_tag* scope;

	// For everything related to a type
	type_t* type_information;

	// Related scope. For the scope defined after this name
	// namespaces, classes and functions
	struct scope_tag* related_scope;

	// Initializations of several kind are saved here
	//   - initialization of const objects
	//   - enumerator values
	AST expression_value;

	// For template parameters
	int num_template_parameters;
	template_parameter_t** template_parameter_info;

	// Linkage
	char* linkage_spec;
} scope_entry_t;

// This is what the scope returns
typedef struct scope_entry_list
{
	// The current entry
	scope_entry_t* entry;
	
	// Next entry under this name (NULL if last)
	struct scope_entry_list* next;
} scope_entry_list_t;

enum scope_kind
{
	UNDEFINED_SCOPE = 0, // Undefined scope, to early catch errors
	NAMESPACE_SCOPE, // Scope of a namespace
	FUNCTION_SCOPE, // Label declarations and gotos 
	PROTOTYPE_SCOPE, // Scope of a prototype
	BLOCK_SCOPE, // Corresponds to the scope of a compound statement
	CLASS_SCOPE, // Class scope
	TEMPLATE_SCOPE // Template scope, will get inherited everywhere if necessary
};

// This is the scope
typedef struct scope_tag
{
	// Kind of this scope
	enum scope_kind kind;
	
	// Hash of scope_entry_list
	Hash* hash;

	// Relationships with other scopes
	// Nesting relationship is expressed by "contained_in"
	struct scope_tag* contained_in; 

	// using namespace statements (using directives) will fill this
	int num_used_namespaces;
	struct scope_tag** use_namespace;

	// Base scopes
	int num_base_scopes;
	struct scope_tag** base_scope;

	// Prototype scope
	struct scope_tag* prototype_scope;
	
	// Function scope
	struct scope_tag* function_scope;

	// Template scope
	struct scope_tag* template_scope;
} scope_t;

#undef BITMAP

scope_t* new_namespace_scope(scope_t* enclosing_scope);
scope_t* new_prototype_scope(scope_t* enclosing_scope);
scope_t* new_block_scope(scope_t* enclosing_scope, scope_t* prototype_scope, scope_t* function_scope);
scope_t* new_function_scope(scope_t* enclosing_scope, scope_t* prototype_scope);
scope_t* new_class_scope(scope_t* enclosing_scope);
scope_t* new_template_scope(scope_t* enclosing_scope);

// Functions to handle scope
scope_entry_t* new_symbol(scope_t* st, char* name);
scope_entry_list_t* create_list_from_entry(scope_entry_t* entry);
void insert_entry(scope_t* st, scope_entry_t* entry);

typedef enum unqualified_lookup_behaviour_tag
{
	NOFULL_UNQUALIFIED_LOOKUP = 0,
	FULL_UNQUALIFIED_LOOKUP = 1
} unqualified_lookup_behaviour_t;

// Higher level functions when dealing with the scope
scope_entry_t* filter_simple_type_specifier(scope_entry_list_t* entry_list);

// Given a list of symbols, purge all those that are not of symbol_kind kind
scope_entry_list_t* filter_symbol_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind);
// Similar but can be used to filter based on a kind set
scope_entry_list_t* filter_symbol_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set);

// Opposite filtering
scope_entry_list_t* filter_symbol_non_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind);
scope_entry_list_t* filter_symbol_non_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set);

// Everything built by an id_expression can be queried with this function
scope_entry_list_t* query_id_expression(scope_t* st, AST id_expr, unqualified_lookup_behaviour_t unqualified_lookup);

// Performs a full unqualified lookup
scope_entry_list_t* query_unqualified_name(scope_t* st, char* unqualified_name);

// Nested names
//    This one should be enough for most cases
scope_entry_list_t* query_nested_name(scope_t* sc, AST global_op, AST nested_name, AST name, 
        unqualified_lookup_behaviour_t unqualified_lookup);
//    These are here for the purpose of flexibility but should be rarely needed
scope_t* query_nested_name_spec(scope_t* sc, AST global_op, AST nested_name, scope_entry_list_t** result_entry_list);
// char incompatible_symbol_exists(scope_t* st, AST id_expr, enum cxx_symbol_kind symbol_kind);
scope_entry_list_t* query_template_id(AST nested_name_spec, scope_t* st, scope_t* lookup_scope);
scope_entry_list_t* query_unqualified_template_id(AST template_id, scope_t* sc, scope_t* lookup_scope);
scope_entry_list_t* query_in_symbols_of_scope(scope_t* sc, char* name);

#endif // CXX_SCOPE_H
