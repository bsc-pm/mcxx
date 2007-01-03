#ifndef CXX_SCOPE_H
#define CXX_SCOPE_H

#include "cxx-ast.h"
#include "hash.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

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

struct scope_tag;
struct decl_context_tag;

enum cxx_symbol_kind
{
    SK_UNDEFINED = 0,
    SK_CLASS, // [1] this names a plain class
    SK_ENUM, // [2] this names an enum
    SK_ENUMERATOR, // [3] this names an enumerator (the elements an enum is made of)
    SK_FUNCTION,  // [4] this names a plain function
    SK_LABEL, // [5] this names a label (currently unused)
    SK_NAMESPACE, // [6] this names a namespace
    SK_VARIABLE, // [7] this names an object
    SK_TYPEDEF, // [8] this names a typedef
    // Lots of stuff related to the C++ "template madness"
    SK_TEMPLATE_PRIMARY_CLASS, // [9] this names a primary template
    SK_TEMPLATE_SPECIALIZED_CLASS, // [10] this names a specialized template class
    SK_TEMPLATE_FUNCTION, // [11] this names a template function
	SK_TEMPLATE_ALIAS, // [12] this names something that aliases a template-name (used solely in instantiation)
    SK_TEMPLATE_PARAMETER, // [13] nontype parameters like N in "template<int N>"
    SK_TEMPLATE_TYPE_PARAMETER, // [14] plain type parameters like T in "template <class T>"
    SK_TEMPLATE_TEMPLATE_PARAMETER, // [15] template template parameters like Q in "template<template<typename P> class Q>"
    // Artificial symbol representing scopes - used only for debugging purposes
    // should not be considered as a symbol
    SK_SCOPE, // [16]
    // GCC Extension for builtin types
    SK_GCC_BUILTIN_TYPE, // [17]
    // Dependent entity that is named but nothing is known at the moment
    SK_DEPENDENT_ENTITY // [18]
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
    TK_UNKNOWN = 0,
    TK_DIRECT,             // 1
    TK_POINTER,            // 2
    TK_REFERENCE,          // 3
    TK_POINTER_TO_MEMBER,  // 4
    TK_ARRAY,              // 5
    TK_FUNCTION,           // 6
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
    STK_UNDEFINED = 0, 
    STK_BUILTIN_TYPE, // [1] int, float, char, wchar_t, bool, void {identifier};
    STK_CLASS, // [2] struct {identifier};
    STK_ENUM, // [3] enum {identifier}
    STK_TYPEDEF, // [4] typedef int {identifier};
    STK_USER_DEFINED, // [5] A {identifier};
    // Templates stuff
    STK_TYPE_TEMPLATE_PARAMETER, // [6] template <class {identifier}> struct B {};
	STK_TEMPLATE_TEMPLATE_PARAMETER, // [7] template <template <...> class {identifier}> struct B {};
    STK_TEMPLATE_DEPENDENT_TYPE, // [8] template <class T> struct B { typename T::a {identifier}; };
    // GCC Extensions
    STK_VA_LIST, // [9] __builtin_va_list {identifier};
    STK_TYPEOF  // [10] __typeof__(int) {identifier};
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

typedef struct template_parameter 
{
    AST parameter_tree;
    enum template_parameter_kind kind;
    char* template_parameter_name;

	struct scope_entry_tag* template_parameter_symbol;

    struct type_tag* type_info;

    // struct type_tag* default_type;

    AST default_tree;
    struct scope_tag* default_argument_scope;
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

typedef struct conversion_function_info_tag
{
    struct type_tag* conversion_type;
    cv_qualifier_t cv_qualifier;
} conversion_function_t;

typedef struct base_class_info_tag
{
    struct type_tag* class_type;
    struct scope_entry_tag* class_symbol;
    access_specifier_t access_specifier;
    char is_virtual;
} base_class_info_t;

typedef struct class_information_tag {
    enum class_kind_t class_kind;

    // Related inner scope to this class
    struct scope_tag* inner_scope;

    // Special functions
    struct scope_entry_tag* destructor;

    int num_conversion_functions;
    struct conversion_function_info_tag** conversion_function_list;

    int num_operator_functions;
    struct scope_entry_tag** operator_function_list;

    int num_constructors;
    struct scope_entry_tag** constructor_list;

    int num_bases;
    base_class_info_t** base_classes_list;
} class_info_t;

enum template_argument_kind
{
    TAK_UNDEFINED = 0,
    TAK_NONTYPE,
    TAK_TYPE,
    TAK_TEMPLATE
};

typedef struct template_argument_tag
{
    enum template_argument_kind kind;

    AST argument_tree;

    // Scope for the expression
    struct scope_tag* scope;

    // Otherwise we should have type_t here
    struct type_tag* type;

    // The argument was implicitly defined by default template argument
    char implicit;
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

    char is_static; // local linkage or class-static

    // GCC extension
    char is_complex;

    // Previously declared type. should be completely "cv-unqualified"
    //
    // If this is a template-class this will be NULL since there
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
    
    // Used in unification
    char* template_parameter_name;

    // Used when instantiating a template class
    AST template_class_base_clause;
    AST template_class_body;

    // For template parameters, the positional number of this argument in the
    // template and its nesting level (this should be enough to define
    // completely a template parameter in a "nameless" way)
    int template_parameter_nesting;
    int template_parameter_num;

    // Scope where this type was declared if not builtin
    struct scope_tag* type_scope;

    // For typeof and template dependent types
    AST typeof_expr;
    struct scope_tag* typeof_scope;

    // For instantiation purposes
	// 
	// The specialized template has already been instantiated
    char from_instantiation; 
    
} simple_type_t;

typedef struct parameter_info_tag
{
    char is_ellipsis;
    struct type_tag* type_info;
    AST default_argument;
} parameter_info_t;

// Function information
typedef struct function_tag
{
    struct type_tag* return_type;
    int num_parameters;
    parameter_info_t** parameter_list;
    exception_spec_t* exception_spec;

    // For instantiating template function purposes
    AST function_body;

    int is_static; // local linkage or class-static
    int is_inline;
    int is_virtual;
    int is_pure; // is_pure implies is_virtual
    int is_explicit;

    int is_constructor; // States if this functions is a constructor

    // For template parameters
    int num_template_parameters;
    template_parameter_t** template_parameter_info;

	// This is for template functions
    int num_template_parameters_in_scope;
    template_parameter_t** template_parameter_in_scope_info;

    // Information about the nesting of this function within templates
    int template_nesting;
} function_info_t;

// Pointers, references and pointers to members
typedef struct pointer_tag
{
    struct type_tag* pointee;
    struct scope_entry_tag* pointee_class;
} pointer_info_t;

// Array information
typedef struct array_tag
{
    AST array_expr;
    struct scope_tag* array_expr_scope;
    struct type_tag* element_type;
} array_info_t;

// States the "temporarieness" of a type
typedef enum 
{
    VT_UNDEFINED = 0,
    VT_LVALUE,
    VT_RVALUE
} value_type_t;

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

    // cv-qualifier related to this type
    cv_qualifier_t cv_qualifier;

} type_t;

typedef enum dependency_info_tag
{
    DI_UNKNOWN = 0,
    DI_NOT_DEPENDENT,
    DI_DEPENDENT,
    DI_BUSY // This means it is being calculated now
} dependency_info_t;

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
    //  - initialization of const objects
    //  - enumerator values
    AST expression_value;

    // For template parameters
    int num_template_parameters;
    template_parameter_t** template_parameter_info;

    // Linkage
    char* linkage_spec;

    // Line where this simbol was signed up
    int line;

    // Do not print this symbol (because of recursion, hiding, etc)
    char do_not_print;

    char injected_class_name;
    struct scope_entry_tag* injected_class_referred_symbol;
	
	// For template-alias
	struct type_tag* template_alias_type;

    // Is a member ?
    char is_member;
    // of who ?
    struct type_tag* class_type;

	// Point in the AST where this was declared. This is approximate, just to
	// find the simple_declaration, member_declaration or function_definition
	// holding this one
	AST point_of_declaration;

    // Dependency info. It states if this symbol has a template-dependent nature
    // A value of DI_UNKNOWN means this has not been already computed
    //
    // At the moment, this is used only for variables and enumerators.  It is
    // intended to avoid an infinite recursion when computing whether an enum
    // or enumerator is dependent.  An enum will check every of its
    // enumerators, and an enumerator will check its enum type
    dependency_info_t dependency_info;
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

	// Qualification name
	char* qualification_name;

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


scope_t* new_namespace_scope(scope_t* enclosing_scope, char* qualification_name);
scope_t* new_prototype_scope(scope_t* enclosing_scope);
scope_t* new_block_scope(scope_t* enclosing_scope, scope_t* prototype_scope, scope_t* function_scope);
scope_t* new_function_scope(scope_t* enclosing_scope, scope_t* prototype_scope);
scope_t* new_class_scope(scope_t* enclosing_scope, char* qualification_name);
scope_t* new_template_scope(scope_t* enclosing_scope);

// Functions to handle scope
scope_entry_t* new_symbol(scope_t* st, char* name);
void remove_entry(scope_t* st, scope_entry_t* entry);
void insert_entry(scope_t* st, scope_entry_t* entry);

typedef enum unqualified_lookup_behaviour_tag
{
    NOFULL_UNQUALIFIED_LOOKUP = 0,
    FULL_UNQUALIFIED_LOOKUP = 1
} unqualified_lookup_behaviour_t;

typedef enum lookup_flags_tag
{
    LF_NONE = 0,
    LF_CONSTRUCTOR = BITMAP(1),
    LF_EXPRESSION = BITMAP(2),
    LF_INSTANTIATE = BITMAP(3),
    LF_IN_NAMESPACE_SCOPE = BITMAP(4),
    LF_FROM_QUALIFIED = BITMAP(5),
    LF_ALWAYS_CREATE_SPECIALIZATION = BITMAP(6),
    LF_NO_FAIL = BITMAP(7),
    LF_NO_INSTANTIATE = BITMAP(8)
} lookup_flags_t ;

// Higher level functions when dealing with the scope
scope_entry_t* filter_simple_type_specifier(scope_entry_list_t* entry_list);

// Given a list of symbols, purge all those that are not of symbol_kind kind
scope_entry_list_t* filter_symbol_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind);
// Similar but can be used to filter based on a kind set
scope_entry_list_t* filter_symbol_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set);

// Opposite filtering
scope_entry_list_t* filter_symbol_non_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind);
scope_entry_list_t* filter_symbol_non_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set);

scope_entry_list_t* filter_entry_from_list(scope_entry_list_t* entry_list, scope_entry_t* entry);

scope_entry_list_t* filter_symbol_using_predicate(scope_entry_list_t* entry_list, char (*f)(scope_entry_t*));

// Everything built by an id_expression can be queried with this function
scope_entry_list_t* query_id_expression(scope_t* st, AST id_expr, 
        unqualified_lookup_behaviour_t unqualified_lookup, struct decl_context_tag decl_context);

scope_entry_list_t* query_id_expression_flags(scope_t* st, AST id_expr, 
        unqualified_lookup_behaviour_t unqualified_lookup, 
        lookup_flags_t lookup_flags, struct decl_context_tag decl_context);

// Performs a full unqualified lookup
scope_entry_list_t* query_unqualified_name(scope_t* st, char* unqualified_name);
scope_entry_list_t* query_unqualified_name_flags(scope_t* st, char* unqualified_name, 
        lookup_flags_t lookup_flags);

// Nested names
//    This one should be enough for most cases
scope_entry_list_t* query_nested_name(scope_t* sc, AST global_op, AST nested_name, AST name, 
        unqualified_lookup_behaviour_t unqualified_lookup, struct decl_context_tag decl_context);

scope_entry_list_t* query_nested_name_flags(scope_t* sc, AST global_op, AST nested_name, AST name, 
        unqualified_lookup_behaviour_t unqualified_lookup, lookup_flags_t lookup_flags,
        struct decl_context_tag decl_context);
//    These are here for the purpose of flexibility but should be rarely needed
scope_t* query_nested_name_spec(scope_t* sc, AST global_op, AST nested_name, scope_entry_list_t** result_entry_list, 
        char* is_dependent, struct decl_context_tag decl_context);
scope_t* query_nested_name_spec_flags(scope_t* sc, AST global_op, AST nested_name, scope_entry_list_t** result_entry_list,
        char* is_dependent, lookup_flags_t lookup_flags, struct decl_context_tag decl_context);
// char incompatible_symbol_exists(scope_t* st, AST id_expr, enum cxx_symbol_kind symbol_kind);
scope_entry_list_t* query_template_id(AST nested_name_spec, scope_t* st, scope_t* lookup_scope,
        struct decl_context_tag decl_context);
scope_entry_list_t* query_template_id_flags(AST nested_name_spec, scope_t* st, scope_t* lookup_scope,
        lookup_flags_t lookup_flags, struct decl_context_tag decl_context);
scope_entry_list_t* query_unqualified_template_id(AST template_id, scope_t* sc, scope_t* lookup_scope,
        struct decl_context_tag decl_context);
scope_entry_list_t* query_unqualified_template_id_flags(AST template_id, scope_t* sc, scope_t* lookup_scope, 
        lookup_flags_t lookup_flags,
        struct decl_context_tag decl_context);
scope_entry_list_t* query_in_symbols_of_scope(scope_t* sc, char* name);

// Manipulators
scope_entry_list_t* create_list_from_entry(scope_entry_t* entry);
scope_entry_list_t* append_scope_entry_lists(scope_entry_list_t* a, scope_entry_list_t* b);

// Looking for scopes
scope_t* enclosing_namespace_scope(scope_t* st);

// Copy scope
scope_t* copy_scope(scope_t* st);

// Get the fully qualified symbol name in the scope of the ocurrence
char* get_fully_qualified_symbol_name(scope_entry_t* entry, scope_t* st);

#undef BITMAP

MCXX_END_DECLS

#endif // CXX_SCOPE_H
