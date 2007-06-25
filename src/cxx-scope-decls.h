/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef CXX_SCOPE_DECLS_H
#define CXX_SCOPE_DECLS_H

#include "cxx-macros.h"
#include "cxx-ast-decls.h"
#include "hash.h"

MCXX_BEGIN_DECLS

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

#define BITMAP(x) (1 << x)

// Kind of cv-qualification
typedef enum {
    CV_NONE = 0, // [no qualification]
    CV_CONST = BITMAP(1), // const
    CV_VOLATILE = BITMAP(2),  // volatile
    CV_RESTRICT = BITMAP(3) // __restrict in C++ || restrict in C99
} cv_qualifier_t;

#undef BITMAP

// An exception specifier used in function info
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
typedef 
enum builtin_type_tag
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

typedef 
enum simple_type_kind_tag
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

// Information of enums
typedef 
struct enum_information_tag {
    int num_enumeration; // Number of enumerations declared for this enum
    struct scope_entry_tag** enumeration_list; // The symtab entry of the enum
} enum_info_t;

enum template_parameter_kind
{
    TPK_UNKNOWN = 0,
    TPK_NONTYPE, // template <int N> <-- 'N'
    TPK_TYPE, // template <class T> <-- 'T'
    TPK_TEMPLATE // template <template <typename Q> class V > <-- 'V'
};

// A template parameter
//
// template <class T, int N> <-- these are parameters
typedef struct template_parameter 
{
    // Kind of the parameter
    enum template_parameter_kind kind;

    // The tree of the parameter
    AST parameter_tree;

    // Its name
    char* template_parameter_name;

    // The related symbol associated to this parameter
    struct scope_entry_tag* template_parameter_symbol;

    // Type info of nontype template parameters
    struct type_tag* type_info;

    // struct type_tag* default_type;

    // Default parameter information
    //    template <int N = 10>   
    //    template <class T = int> 
    //    template <template <typename Q> class V = std::vector>
    // The tree (10, int, std::vector)
    AST default_tree;
    // and its scope
    struct scope_tag* default_argument_scope;
} template_parameter_t;

// Access specifier, saved but not enforced by the compiler
typedef enum access_specifier_t
{
    AS_UNKNOWN = 0,
    AS_PUBLIC, // public
    AS_PRIVATE, // private
    AS_PROTECTED // protected
} access_specifier_t;

struct simple_type_tag;

enum class_kind_t {
    CK_STRUCT, // struct
    CK_CLASS, // class
    CK_UNION // union 
};

// Conversion function info
//
// class B
// {
//   operator T() { ... }
// };
typedef 
struct conversion_function_info_tag
{
    // Which type it converts to
    struct type_tag* conversion_type;

    // Its cv-qualifier
    //
    //  operator T() const 
    cv_qualifier_t cv_qualifier;
} conversion_function_t;

// Base class info (parent classes of a given class)
typedef 
struct base_class_info_tag
{
    // The parent class type
    struct type_tag* class_type;

    // The parent class symbol
    struct scope_entry_tag* class_symbol;

    // The access specifier (public, private, protected inheritance)
    access_specifier_t access_specifier;

    // A virtual base
    char is_virtual;
} base_class_info_t;

// Information of a class
typedef 
struct class_information_tag {
    // Kind of class {struct, class}
    enum class_kind_t class_kind;

    // Related inner scope to this class
    struct scope_tag* inner_scope;

    // Special functions
    struct scope_entry_tag* destructor;

    // Conversion functions info
    int num_conversion_functions;
    struct conversion_function_info_tag** conversion_function_list;

    // Operator function info
    int num_operator_functions;
    struct scope_entry_tag** operator_function_list;

    // Class constructors info
    int num_constructors;
    struct scope_entry_tag** constructor_list;

    // Base (parent classes) info
    int num_bases;
    base_class_info_t** base_classes_list;
} class_info_t;

// Template arguments are the things that go between '<' and '>' in a
// template-id
//
// MyBoundedStack<int, 100> mStack; <-- 'int' and '100' are template arguments
enum template_argument_kind
{
    TAK_UNDEFINED = 0,
    TAK_NONTYPE, // an nontype template argument (basically an int or pointer)
    TAK_TYPE, // a type template argument (the common case)
    TAK_TEMPLATE // template template argument (not very usual)
};

typedef 
struct template_argument_tag
{
    // Kind of the template argument
    enum template_argument_kind kind;

    // Argument tree. Used for nontype template arguments
    AST argument_tree;

    // Scope for the expression
    struct scope_tag* scope;

    // If the template argument is a type template argument (or a template
    // template one) the type should be here
    struct type_tag* type;

    // This argument was implicitly defined by default template argument
    char implicit;
} template_argument_t;

// List of template arguments
typedef 
struct template_argument_list_tag {
    int num_arguments;
    template_argument_t** argument_list;
} template_argument_list_t;

typedef 
enum template_nature_tag
{
    TPN_UNKNOWN = 0,
    // TPN_COMPLETE_DEPENDENT means that the template has been declared and has
    // been defined but depends on any template parameter. Examples,
    // 
    // template <class T>
    // struct A { };
    //
    // template <class T>
    // struct A<T*> { };
    TPN_COMPLETE_DEPENDENT, 
    // TPN_COMPLETE_INDEPENDENT means that the template has been declared
    // and defined and does not depend on any template parameter. Templates
    // instantiated by lookup should fall in this cathegory.
    //
    // template <>
    // struct A<int>
    // {
    // };
    //
    // A<float>::K t; <-- Here 'A<float>' would be instantiated in order
    //                    to lookup K and it would be a TPN_COMPLETE_INDEPENDENT
    TPN_COMPLETE_INDEPENDENT, 
    // TPN_INCOMPLETE_DEPENDENT means that the template has been declared but
    // has not been defined and depends on template parameters. Examples,
    //
    // template <class T>
    // struct A; <-- 'A<T>' is TPN_INCOMPLETE_DEPENDENT
    //
    // template <class T>
    // struct A<T*>; <-- 'A<T*>' is TPN_INCOMPLETE_DEPENDENT
    //
    // template <class T>
    // struct B { <-- B here would be TPN_COMPLETE_DEPENDENT
    //     typedef C<T*> B_pT; <-- for some existing template 'C' 
    //                             'C<T*>' should be TPN_INCOMPLETE_DEPENDENT
    // };
    TPN_INCOMPLETE_DEPENDENT, 

    // TPN_INCOMPLETE_INDEPENDENT includes explicit specializations and
    // typedefs of independent templates that are simply declared but not
    // defined and do not depend on any template argument
    //
    // template <>
    // struct A<int>;
    //
    // typedef A<char*> A_pchar;
    TPN_INCOMPLETE_INDEPENDENT
} template_nature_t;

// Direct type covers types that are not pointers to something, neither
// functions to something neither arrays to something.  So every basic type is
// represented here including builtin types, classes, structs, enums, unions
// and other nuclear types (like type template parameters)
typedef 
struct simple_type_tag {
    // Kind
    simple_type_kind_t kind;

    // if Kind == STK_BUILTIN_TYPE here we have
    // the exact builtin type
    builtin_type_t builtin_type;

    // This can be 0, 1 (long) or 2 (long long)
    char is_long; 
    // short
    char is_short;
    // unsigned
    char is_unsigned;
    // signed
    char is_signed;

    // This is something not strictly related to the type...
    // static
    char is_static; 

    // GCC extension
    // __Complex float
    char is_complex;

    // This type exists after another symbol, for
    // instance
    //
    // class A
    // {
    // };
    // A b;
    //
    // creates an 'A' symbol of type SK_CLASS
    // and a 'b' symbol SK_VARIABLE with type STK_USER_DEFINED
    // pointing to 'A' symbol
    struct scope_entry_tag* user_defined_type;

    // For typedefs (kind == STK_TYPEDEF)
    // the aliased type
    struct type_tag* aliased_type;

    // For enums (kind == STK_ENUM)
    enum_info_t* enum_info;
    
    // For classes (kind == STK_CLASS)
    // this includes struct/class/union
    class_info_t* class_info;

    // For template classes
    // Template arguments for specializations and instantiations
    // (kind == STK_CLASS)
    template_argument_list_t* template_arguments;
    
    // Used in unification
    // (kind == STK_TYPE_TEMPLATE_PARAMETER)
    char* template_parameter_name;

    // Used when instantiating a template class
    // (kind == STK_CLASS)
    AST template_class_base_clause;
    AST template_class_body;

    // For template parameters, the positional number of this argument in the
    // template and its nesting level (this should be enough to define
    // completely a template parameter in a "nameless" way)
    // (kind == STK_TYPE_TEMPLATE_PARAMETER)
    // (kind == STK_TEMPLATE_TEMPLATE_PARAMETER)
    int template_parameter_nesting;
    int template_parameter_num;

    // Scope where this type was declared if not builtin
    struct scope_tag* type_scope;

    // For typeof and template dependent types
    // (kind == STK_TYPEOF)
    // (kind == STK_TEMPLATE_DEPENDENT_TYPE)
    AST typeof_expr;
    struct scope_tag* typeof_scope;

    // For instantiation purposes
    // 
    // The specialized template has already been instantiated
    // (kind == STK_CLASS)
    template_nature_t template_nature;
    
    // Saved decl_context for classes (this is used in later phases when we
    // want to add new members by means of parsing)
    // (kind == STK_CLASS)
    struct decl_context_tag* decl_context;
} simple_type_t;

// Information of a parameter
typedef 
struct parameter_info_tag
{
    // This parameter is '...'
    char is_ellipsis;
    // Otherwise it has the type here
    struct type_tag* type_info;
    // Default argument tree for the parameter
    //    int f(int a = 10);
    AST default_argument;
} parameter_info_t;

// Function information
typedef 
struct function_tag
{
    // The returning type of the function
    struct type_tag* return_type;

    // Parameter information
    int num_parameters;
    parameter_info_t** parameter_list;

    // Exception specifier for this function
    exception_spec_t* exception_spec;

    // For instantiating template function purposes
    AST function_body;

    // static (local linkage or static member)
    int is_static; 

    // inline
    int is_inline;

    // virtual
    int is_virtual;

    // virtual void f() = 0;
    // pure implies virtual (not the opposite, though)
    int is_pure; 

    // explicit constructor
    // only meaningful in constructors invokable with one parameter
    //
    // class A
    // {
    //   explicit A(int n) { }
    // };
    int is_explicit;

    // States if this function has been declared or defined without prototype.
    // This is only meaningful in C but not in C++ where all functions do have
    // prototype
    int lacks_prototype;

    // States if this functions is a constructor hence it will lack
    // returning type (returning_type == NULL)
    int is_constructor; 

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
typedef 
struct pointer_tag
{
    // The pointee type
    struct type_tag* pointee;

    // If the type was a TK_POINTER_TO_MEMBER
    // the pointee class
    struct scope_entry_tag* pointee_class;
} pointer_info_t;

// Array information
typedef 
struct array_tag
{
    // Array sizes
    AST array_expr;

    // Scope of the array size expression
    struct scope_tag* array_expr_scope;

    // The type of the array elements
    struct type_tag* element_type;
} array_info_t;

// States the "temporarieness" of a type
typedef enum 
{
    VT_UNDEFINED = 0,
    VT_LVALUE, // a lvalue
    VT_RVALUE  // a rvalue
} value_type_t;

// This structure is able to hold type information for a given symbol
// note it being decoupled from its declarator 
typedef 
struct type_tag
{
    // Kind of the type
    enum type_kind kind;

    // Pointer
    // (kind == TK_POINTER)
    // (kind == TK_POINTER_TO_MEMBER)
    pointer_info_t* pointer;

    // Array
    // (kind == TK_ARRAY)
    array_info_t* array;

    // Function
    // (kind == TK_FUNCTION)
    function_info_t* function;

    // "Simple" type
    // (kind == TK_DIRECT)
    simple_type_t* type;

    // cv-qualifier related to this type
    // The cv-qualifier is in the type
    cv_qualifier_t cv_qualifier;
} type_t;

// Dependent entity means it depends somehow on a template parameter
typedef 
enum dependency_info_tag
{
    DI_UNKNOWN = 0,
    DI_NOT_DEPENDENT, // This entity is not dependent
    DI_DEPENDENT, // The entity is dependent
    DI_BUSY // This means it is being calculated now. This happens in enums where
        // we have to check every enumerator in order to realize if the whole
        // enum is or not dependent. In this case, infinite recursion could happen
        // if no care is taken
} dependency_info_t;

// This is an entry in the scope
typedef 
struct scope_entry_tag
{
    // The symbol name
    char* symbol_name;

    // Kind of this symbol
    enum cxx_symbol_kind kind;

    // This allows us to enforce the one-definition-rule within a translation unit
    int defined;

    // Scope of this entry when declared
    struct scope_tag* scope;

    // Type information of this symbol
    type_t* type_information;

    // Related scope. This is the scope defined within a class or a function.
    struct scope_tag* related_scope;

    // Initializations of several kind are saved here
    //  - initialization of const objects
    //  - enumerator values
    AST expression_value;

    // For template parameters alive in the declaration
    // of this entity
    int num_template_parameters;
    template_parameter_t** template_parameter_info;

    // Linkage specifier ("C" or "C++")
    // Unused field
    char* linkage_spec;

    // Line where this simbol was signed up
    int line;

    // Do not print this symbol (because of recursion, hiding, etc) Used
    // specially for the injected class-name, where printing it in print scope
    // routines would create an infinite recursion.
    char do_not_print;

    // States if this is the injected class name of every class
    char injected_class_name;
    // and its symbol
    struct scope_entry_tag* injected_class_referred_symbol;
    
    // For template-alias
    // Used for template template parameters
    struct type_tag* template_alias_type;

    // Is a member entity (function or data)
    char is_member;
    // and its class
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

// This is what the scope returns a list of symbols due to function overloading
// and template class specialization
typedef struct scope_entry_list
{
    // The current entry
    scope_entry_t* entry;
    
    // Next entry under this name (NULL if last)
    struct scope_entry_list* next;
} scope_entry_list_t;

// Scope kind
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
typedef 
struct scope_tag
{
    // Kind of this scope
    enum scope_kind kind;

    // Hash of scope_entry_list
    Hash* hash;

    // Qualification name of this scope this holds the name we have to prepend
    // to an entity of this scope in order to qualify it in the enclosing
    // scope.
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

typedef 
enum unqualified_lookup_behaviour_tag
{
    NOFULL_UNQUALIFIED_LOOKUP = 0,
    FULL_UNQUALIFIED_LOOKUP = 1
} unqualified_lookup_behaviour_t;

#define BITMAP(x) (1 << x)

typedef 
enum lookup_flags_tag
{
    LF_NONE = 0,
    LF_CONSTRUCTOR = BITMAP(1),
    LF_EXPRESSION = BITMAP(2),
    LF_INSTANTIATE = BITMAP(3),
    LF_IN_NAMESPACE_SCOPE = BITMAP(4),
    LF_FROM_QUALIFIED = BITMAP(5),
    // LF_ALWAYS_CREATE_SPECIALIZATION = BITMAP(6),
    LF_NO_FAIL = BITMAP(7),
    LF_NO_INSTANTIATE = BITMAP(8)
} lookup_flags_t ;

#undef BITMAP

MCXX_END_DECLS

#endif // CXX_SCOPE_DECLS_H
