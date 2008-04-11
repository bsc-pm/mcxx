/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#include <stdio.h>
#include <string.h>
#include "cxx-buildscope.h"
#include "cxx-typeutils.h"
#include "cxx-typeunif.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-exprtype.h"
#include "cxx-prettyprint.h"
#include "cxx-driver.h"
#include "cxx-ambiguity.h"
#include "hash.h"

/*
 * --
 */
typedef unsigned int _size_t;

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
    TK_LVALUE_REFERENCE,   // 3
    TK_RVALUE_REFERENCE,   // 4
    TK_POINTER_TO_MEMBER,  // 5
    TK_ARRAY,              // 6
    TK_FUNCTION,           // 7
    TK_OVERLOAD,           // 8
    TK_VECTOR,             // 9
    TK_ELLIPSIS,           // 10
    TK_COMPUTED,           // 11
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
    STK_TEMPLATE_TYPE, // [6] a template type
    // An unknown type
    STK_TEMPLATE_DEPENDENT_TYPE, // [7]
    // GCC Extensions
    STK_VA_LIST, // [8] __builtin_va_list {identifier};
    STK_TYPEOF  // [9] __typeof__(int) {identifier};
} simple_type_kind_t;

struct scope_entry_tag;

// Information of enums
typedef 
struct enum_information_tag {
    int num_enumeration; // Number of enumerations declared for this enum
    struct scope_entry_tag** enumeration_list; // The symtab entry of the enum
} enum_info_t;

struct simple_type_tag;

enum class_kind_t {
    CK_STRUCT, // struct
    CK_CLASS, // class
    CK_UNION // union 
};

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

    // A dependent class
    char is_dependent;
    
    // Currently unused
    char is_local_class;
    struct scope_entry_tag* enclosing_function;

    // The inner decl context created by this class
    decl_context_t inner_decl_context;

    // Destructor
    struct scope_entry_tag* destructor;

    // Conversion functions info
    int num_conversion_functions;
    struct scope_entry_tag** conversion_functions;

    // Operator function info
    int num_copy_assignment_operator_functions;
    struct scope_entry_tag** copy_assignment_operator_function_list;

    // Class constructors info
    int num_constructors;
    struct scope_entry_tag** constructor_list;

    // Copy constructors
    int num_copy_constructors;
    struct scope_entry_tag** copy_constructor_list;

    // Nonstatic data members
    int num_nonstatic_data_members;
    struct scope_entry_tag** nonstatic_data_members;
    
    // Static data members
    int num_static_data_members;
    struct scope_entry_tag** static_data_members;

    // Base (parent classes) info
    int num_bases;
    base_class_info_t** base_classes_list;
} class_info_t;

// Template arguments are the things that go between '<' and '>' in a
// template-id
//
// MyBoundedStack<int, 100> mStack; <-- 'int' and '100' are template arguments

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

    
    // Used when instantiating a template class
    // (kind == STK_CLASS)
    AST template_class_base_clause;
    AST template_class_body;

    // Decl environment where this type was declared if not builtin The scope
    // where this type was declared since sometimes, types do not have any name
    // related to them
    // (kind == STK_ENUM)
    // (kind == STK_CLASS)
    decl_context_t type_decl_context;

    // For typeof and template dependent types
    // (kind == STK_TYPEOF)
    // (kind == STK_TEMPLATE_DEPENDENT_TYPE)
    AST typeof_expr;
    decl_context_t typeof_decl_context;
    char typeof_is_expr;

    // For instantiation purposes
    // 
    // The specialized template has already been instantiated
    // (kind == STK_CLASS)
    template_nature_t template_nature;

    // For template types
    template_parameter_list_t* template_parameter_list;
    // This is a STK_USER_DEFINED
    type_t* primary_specialization;
    // Sometimes we need the original symbol defining this template type
    scope_entry_t* related_template_symbol;

    // Specialized types
    int num_specialized_types;
    // These are a STK_USER_DEFINED
    type_t** specialized_types;

    // Template dependent types (STK_TEMPLATE_DEPENDENT_TYPE)
    scope_entry_t* dependent_entry;
    AST dependent_nested_name;
    AST dependent_unqualified_part;

} simple_type_t;


// Function information
typedef 
struct function_tag
{
    // The returning type of the function
    struct type_tag* return_type;

    // Parameter information
    int num_parameters;
    parameter_info_t** parameter_list;

    // States if this function has been declared or defined without prototype.
    // This is only meaningful in C but not in C++ where all functions do have
    // prototype
    int lacks_prototype;
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
    decl_context_t array_expr_decl_context;

    // The type of the array elements
    struct type_tag* element_type;

    // Is literal string type ?
    char is_literal_string;
} array_info_t;

// Vector type
typedef struct vector_tag
{
    unsigned int vector_size;
    struct type_tag* element_type;
} vector_info_t;

// This structure is able to hold type information for a given symbol
// note it being decoupled from its declarator 
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
    
    // For unresolved overload function types 
    // (kind == TK_OVERLOAD)
    scope_entry_list_t* overload_set;
    template_argument_list_t* explicit_template_argument_list;

    // Vector Type
    // (kind == TK_VECTOR)
    vector_info_t* vector;

    // cv-qualifier related to this type
    // The cv-qualifier is in the type
    cv_qualifier_t cv_qualifier;

    // Unqualified type, itself if the type is not qualified
    struct type_tag* unqualified_type;

    // For parameter types, if not null it means some adjustement was done
    struct type_tag* original_type;
    // For template specialized parameters
    char is_template_specialized_type;
    template_argument_list_t* template_arguments;
    struct type_tag* related_template_type;
    // It is not obvious why do we need this, but it is for
    // checking that unification actually succeed
    // It is only NON-null for complete types
    template_parameter_list_t* template_parameters;

    // The sizeof of the type
    _size_t size;

    // (kind == TK_COMPUTED)
    computed_function_type_t compute_type_function;
};


const standard_conversion_t no_scs_conversion = { 
    .orig = NULL,
    .dest = NULL,
    .conv = { SCI_NO_CONVERSION, SCI_NO_CONVERSION, SCI_NO_CONVERSION } 
};

/*
 * Typing environment
 */

struct type_environment_tag
{
    // The type of sizeof
    type_t* sizeof_expr_type;

    // bool
    _size_t sizeof_bool;

    // wchar_t
    _size_t sizeof_wchar_t;

    // short
    _size_t sizeof_unsigned_short;
    _size_t sizeof_signed_short;

    // int
    _size_t sizeof_signed_int;
    _size_t sizeof_unsigned_int;
    
    // long
    _size_t sizeof_signed_long;
    _size_t sizeof_unsigned_long;
    
    // long long
    _size_t sizeof_signed_long_long;
    _size_t sizeof_unsigned_long_long;
    
    // float
    _size_t sizeof_float;
    
    // double
    _size_t sizeof_double;

    // long double
    _size_t sizeof_long_double;

    // pointer
    _size_t sizeof_pointer;
    _size_t sizeof_pointer_to_data_member;
    // this one exists because a pointer to function
    // does not have to be compatible with a regular
    // pointer to data
    _size_t sizeof_function_pointer;
    _size_t sizeof_pointer_to_member_function;

    // function that computes the size of a class type
    // this typically will follow some underlying ABI
    _size_t (*sizeof_class)(type_t*);
    
    // function that computes the size of a union type
    _size_t (*sizeof_union)(type_t*);
};

// Linux IA-32
static type_environment_t type_environment_linux_ia32_ = 
{
    // FIXME
    // .sizeof_expr_type = get_unsigned_int_type(),
    .sizeof_unsigned_short = 2,
    .sizeof_signed_short = 2,

    .sizeof_wchar_t = 2,

    .sizeof_unsigned_int = 4,
    .sizeof_signed_int = 4,

    .sizeof_unsigned_long = 4,
    .sizeof_signed_long = 4,

    .sizeof_unsigned_long_long = 8,
    .sizeof_signed_long_long = 8,

    .sizeof_float = 4,

    .sizeof_double = 8,

    // This in PowerPC is 16. In Intel IA32 it is 12 This is the type that is
    // likely to have the most bizarre values out there
    .sizeof_long_double = 12,

    .sizeof_pointer = 4,
    .sizeof_pointer_to_data_member = 4,
    .sizeof_function_pointer = 4,
    .sizeof_pointer_to_member_function = 4,

    // Not yet implemented but sizeof_class will both implement the underlying
    // C ABI and the C++ ABI (likely System V ABI and Itanium C++ ABI since
    // they are the most usual in Linux systems)
    .sizeof_class = NULL,
    .sizeof_union = NULL,
};

type_environment_t* type_environment_linux_ia32 = &type_environment_linux_ia32_;

/*
 * --
 */

/*
 * This file contains routines destined to work with types.  Comparing two
 * types, comparing function declarations and definitions, etc.
 */
static type_t* get_aliased_type(type_t* t);
static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2);
static char equivalent_array_type(array_info_t* t1, array_info_t* t2);
static char equivalent_function_type(type_t* t1, type_t* t2);
static char equivalent_vector_type(type_t* t1, type_t* t2);
static char compatible_parameters(function_info_t* t1, function_info_t* t2);
static char compare_template_dependent_typename_types(type_t* t1, type_t* t2);
static char equivalent_pointer_to_member_type(type_t* t1, type_t* t2);

static long long unsigned int _bytes_due_to_type_system = 0;

long long unsigned int type_system_used_memory(void)
{
    return _bytes_due_to_type_system;
}

/* Type constructors : Builtins */

static type_t* get_simple_type(void)
{
    type_t* result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);
    result->kind = TK_DIRECT;
    result->type = counted_calloc(1, sizeof(*result->type), &_bytes_due_to_type_system);
    result->unqualified_type = result;
    return result;
}

type_t* get_char_type(void)
{
    // TODO: This  should be modified by a compiler flag
    // return get_signed_char_type();

    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_CHAR;
        _type->size = 1;
    }

    return _type;
}

type_t* get_signed_char_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_CHAR;
        _type->type->is_signed = 1;
        _type->size = 1;
    }

    return _type;
}

type_t* get_unsigned_char_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_CHAR;
        _type->type->is_unsigned = 1;
        _type->size = 1;
    }

    return _type;
}

type_t* get_wchar_t_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_WCHAR;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_wchar_t;
    }

    return _type;
}

type_t* get_bool_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_BOOL;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_bool;
    }

    return _type;
}

type_t* get_signed_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_signed_int;
    }

    return _type;
}

type_t* get_signed_short_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_short = 1;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_signed_short;
    }

    return _type;
}

type_t* get_signed_long_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_long = 1;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_signed_long;
    }

    return _type;
}

type_t* get_signed_long_long_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_long = 2;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_signed_long_long;
    }

    return _type;
}


type_t* get_unsigned_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_unsigned = 1;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_unsigned_int;
    }

    return _type;
}

type_t* get_size_t_type(void)
{
    // FIXME Make this configurable, at the moment this is valid only in ILP32
    // and ILP64 (IL32P64 and I32LP64 systems will break)
    return get_unsigned_int_type();
}

type_t* get_unsigned_short_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_unsigned = 1;
        _type->type->is_short = 1;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_unsigned_short;
    }

    return _type;
}

type_t* get_unsigned_long_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_unsigned = 1;
        _type->type->is_long = 1;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_unsigned_long;
    }

    return _type;
}

type_t* get_unsigned_long_long_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_unsigned = 1;
        _type->type->is_long = 2;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_unsigned_long_long;
    }

    return _type;
}

type_t* get_float_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_FLOAT;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_float;
    }

    return _type;
}

type_t* get_double_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_DOUBLE;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_double;
    }

    return _type;
}

type_t* get_long_double_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_DOUBLE;
        _type->type->is_long = 1;
        _type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_long_double;
    }

    return _type;
}

type_t* get_void_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_VOID;

        // This is an incomplete type but gcc in C returns 1 for sizeof(void)
        _type->size = 1;
    }

    return _type;
}

type_t* get_gcc_typeof_type(AST type_tree, decl_context_t decl_context)
{
    type_t* type = get_simple_type();

    type->type->kind = STK_TYPEOF;
    type->type->typeof_is_expr = 0;
    type->type->typeof_expr = type_tree;
    type->type->typeof_decl_context = decl_context;

    return type;
}

type_t* get_gcc_typeof_expr_type(AST type_expr, decl_context_t decl_context)
{
    type_t* type = get_simple_type();

    type->type->kind = STK_TYPEOF;
    type->type->typeof_is_expr = 1;
    type->type->typeof_expr = type_expr;
    type->type->typeof_decl_context = decl_context;

    return type;
}

type_t* get_gcc_builtin_va_list_type(void)
{
    static type_t* result = NULL;

    if (result == NULL)
    {
        result = get_simple_type();

        result->type->kind = STK_VA_LIST;
    }

    return result;
}

char is_gcc_builtin_va_list(type_t *t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_VA_LIST);
}

type_t* get_user_defined_type(scope_entry_t* entry)
{
    type_t* type_info = get_simple_type();

    type_info->type->kind = STK_USER_DEFINED;
    type_info->type->user_defined_type = entry;

    if (entry->type_information != NULL)
    {
        ERROR_CONDITION(entry->type_information->unqualified_type == NULL, "This cannot be null", 0);
    }

    return type_info;
}

type_t* get_dependent_typename_type(scope_entry_t* dependent_entity, 
        decl_context_t decl_context,
        AST nested_name, 
        AST unqualified_part)
{
    type_t* type_info = get_simple_type();

    type_info->type->kind = STK_TEMPLATE_DEPENDENT_TYPE;
    type_info->type->dependent_entry = dependent_entity;
    type_info->type->typeof_decl_context = decl_context;
    type_info->type->dependent_nested_name = nested_name;
    type_info->type->dependent_unqualified_part = unqualified_part;

    return type_info;
}

void dependent_typename_get_components(type_t* t, scope_entry_t** dependent_entry, 
        decl_context_t* decl_context,
        AST *nested_name, AST *unqualified_part)
{
    ERROR_CONDITION(!is_dependent_typename_type(t), "This is not a dependent typename", 0);

    *dependent_entry = t->type->dependent_entry;
    *decl_context = t->type->typeof_decl_context;
    *nested_name = t->type->dependent_nested_name;
    *unqualified_part = t->type->dependent_unqualified_part;
}

type_t* get_new_enum_type(decl_context_t decl_context)
{
    type_t* type_info = get_simple_type();

    type_info->type->enum_info = (enum_info_t*) counted_calloc(1, sizeof(*type_info->type->enum_info), &_bytes_due_to_type_system);
    type_info->type->kind = STK_ENUM;
    type_info->type->type_decl_context = decl_context;

    return type_info;
}

type_t* get_new_class_type(decl_context_t decl_context)
{
    type_t* type_info = get_simple_type();

    type_info->type->class_info = counted_calloc(1, sizeof(*type_info->type->class_info), &_bytes_due_to_type_system);
    type_info->type->kind = STK_CLASS;
    type_info->type->type_decl_context = decl_context;

    return type_info;
}

static template_argument_list_t* compute_arguments_primary(template_parameter_list_t* template_parameter_list)
{
    int i;

    template_argument_list_t* result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);

    for (i = 0; i < template_parameter_list->num_template_parameters; i++)
    {
        template_parameter_t* template_parameter = template_parameter_list->template_parameters[i];

        template_argument_t* new_template_argument = counted_calloc(1, sizeof(*new_template_argument), &_bytes_due_to_type_system);

        switch (template_parameter->kind)
        {
            case TPK_TYPE :
                {
                    new_template_argument->kind = TAK_TYPE;
                    new_template_argument->type = get_user_defined_type(template_parameter->entry);

                    break;
                }
            case TPK_TEMPLATE :
                {
                    new_template_argument->kind = TAK_TEMPLATE;
                    new_template_argument->type = get_user_defined_type(template_parameter->entry);

                    break;
                }
            case TPK_NONTYPE :
                {
                    new_template_argument->kind = TAK_NONTYPE;
                    new_template_argument->type = template_parameter->entry->type_information;

                    // Fake an expression
                    new_template_argument->expression = ASTLeaf(AST_SYMBOL, 
                            template_parameter->entry->line,
                            template_parameter->entry->symbol_name);
                    new_template_argument->expression_context = template_parameter->entry->decl_context;

                    if (!check_for_expression(new_template_argument->expression,
                                template_parameter->entry->decl_context))
                    {
                        internal_error("Created nontype template argument could not be checked", 0);
                    }

                    break;
                }
            default :
                {
                    internal_error("Invalid template parameter kind %d\n", template_parameter->kind);
                }
        }

        new_template_argument->position = template_parameter->entry->entity_specs.template_parameter_position;
        new_template_argument->nesting = template_parameter->entry->entity_specs.template_parameter_nesting;

        P_LIST_ADD(result->argument_list, result->num_arguments, new_template_argument);
    }

    return result;
}

type_t* get_new_template_type(template_parameter_list_t* template_parameter_list, type_t* primary_type,
        const char* template_name, decl_context_t decl_context, int line, const char* filename)
{
    type_t* type_info = get_simple_type();
    type_info->type->kind = STK_TEMPLATE_TYPE;
    type_info->type->template_parameter_list = template_parameter_list;

    // Primary "specialization"
    scope_entry_t* primary_symbol = NULL;
    primary_symbol = counted_calloc(1, sizeof(*primary_symbol), &_bytes_due_to_type_system);
    primary_symbol->symbol_name = template_name;
    if (is_unnamed_class_type(primary_type))
    {
        primary_symbol->kind = SK_CLASS;
    }
    else if (is_function_type(primary_type))
    {
        primary_symbol->kind = SK_FUNCTION;
    }
    else
    {
        internal_error("Invalid templated type\n", 0);
    }
    primary_symbol->type_information = primary_type;
    primary_symbol->decl_context = decl_context;

    primary_symbol->line = line;
    primary_symbol->file = filename;

    primary_type->is_template_specialized_type = 1;
    primary_type->template_arguments = compute_arguments_primary(template_parameter_list);
    primary_type->template_parameters = template_parameter_list;
    primary_type->related_template_type = type_info;

    if (is_unnamed_class_type(primary_type))
    {
        class_type_set_incomplete_dependent(primary_type);
        class_type_set_is_dependent(primary_type, 1);
    }

    type_info->type->primary_specialization = get_user_defined_type(primary_symbol);

    return type_info;
}

void set_as_template_specialized_type(type_t* type_to_specialize, 
        template_argument_list_t * template_arguments, 
        template_parameter_list_t* template_parameters,
        type_t* template_type)
{
    ERROR_CONDITION(!is_function_type(type_to_specialize)
            && !is_unnamed_class_type(type_to_specialize), "This must be a class or function type", 0);

    if (template_type != NULL)
    {
        ERROR_CONDITION(!is_template_type(template_type), "This must be a template type", 0);
    }

    type_to_specialize->is_template_specialized_type = 1;
    type_to_specialize->template_arguments = template_arguments;
    type_to_specialize->related_template_type = template_type;
    // This one can be NULL
    type_to_specialize->template_parameters = template_parameters;
}

char is_template_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_TEMPLATE_TYPE);
}

void template_type_set_related_symbol(type_t* t, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_template_type(t), "This is not a template type", 0);
    t->type->related_template_symbol = entry;
}

scope_entry_t* template_type_get_related_symbol(type_t* t)
{
    ERROR_CONDITION(!is_template_type(t), "This is not a template type", 0);
    return t->type->related_template_symbol;
}

int template_type_get_nesting_level(type_t* t)
{
    ERROR_CONDITION(!is_template_type(t), "This is not a template type", 0);

    template_parameter_list_t *template_parameters = template_type_get_template_parameters(t);

    ERROR_CONDITION(template_parameters->num_template_parameters == 0,
            "Invalid template parameters", 0);

    // Use the first one since all template parameters will be in the same nesting 
    int nesting 
        = template_parameters->template_parameters[0]->entry->entity_specs.template_parameter_nesting;

    // Sanity check
    int i;
    for (i = 1; i < template_parameters->num_template_parameters; i++)
    {
        // They must agree
        ERROR_CONDITION( (template_parameters->template_parameters[i]->entry->entity_specs.template_parameter_nesting
                    != nesting),
                "Invalid template parameters, their nesting is not the same", 0);
    }

    return nesting;
}

type_t* template_type_get_primary_type(type_t* t)
{
    ERROR_CONDITION(!is_template_type(t), "This is not a template type", 0);
    return t->type->primary_specialization;
}

static char same_template_argument_list(
        template_argument_list_t* template_argument_list_1,
        template_argument_list_t* template_argument_list_2)
{
    ERROR_CONDITION (
            (template_argument_list_1->num_arguments !=
            template_argument_list_2->num_arguments),
            "Argument lists should match", 0);

    int i;
    for (i = 0; i < template_argument_list_1->num_arguments; i++)
    {
        template_argument_t* targ_1 = template_argument_list_1->argument_list[i];
        template_argument_t* targ_2 = template_argument_list_2->argument_list[i];

        ERROR_CONDITION(targ_1->kind != targ_2->kind,
                "They should be of the same kind", 0);

        ERROR_CONDITION(targ_1->position != targ_2->position
                || targ_2->nesting != targ_2->nesting,
                "Invalid comparison of template arguments with different coordinates\n", 0);

        switch (targ_1->kind)
        {
            case TAK_TYPE:
                {
                    if (!equivalent_types(targ_1->type,
                                targ_2->type))
                    {
                        return 0;
                    }
                    break;
                }
            case TAK_NONTYPE:
                {
                    if (!same_functional_expression(targ_1->expression,
                                 targ_1->expression_context,
                                 targ_2->expression,
                                 targ_2->expression_context))
                    {
                        return 0;
                    }
                    break;
                }
            case TAK_TEMPLATE:
                {
                    type_t* type_1 = targ_1->type;
                    type_t* type_2 = targ_2->type;

                    if (!equivalent_types(type_1, type_2))
                    {
                        return 0;
                    }
                    break;
                }
            default:
                {
                    internal_error("Invalid template argument kind", 0);
                }
        }
    }

    return 1;
}

char has_dependent_template_arguments(template_argument_list_t* template_arguments,
        decl_context_t decl_context)
{
    int i;
    for (i = 0; i < template_arguments->num_arguments; i++)
    {
        template_argument_t* curr_argument = template_arguments->argument_list[i];

        if (curr_argument->kind == TAK_TYPE)
        {
            if (is_dependent_type(curr_argument->type, decl_context))
            {
                return 1;
            }
        }
        else if (curr_argument->kind == TAK_TEMPLATE)
        {
            if (named_type_get_symbol(curr_argument->type)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                return 1;
            }
        }
        else if (curr_argument->kind == TAK_NONTYPE)
        {
            if (is_dependent_expression(curr_argument->expression,
                        curr_argument->expression_context))
            {
                return 1;
            }
        }
    }
    return 0;
}

type_t* template_type_get_specialized_type(type_t* t, 
        template_argument_list_t* template_argument_list,
        template_parameter_list_t *template_parameters, 
        decl_context_t decl_context, 
        int line, const char* filename)
{
    ERROR_CONDITION(!is_template_type(t), "This is not a template type", 0);

    // Search an existing specialization
    int i;
    for (i = 0; i < template_type_get_num_specializations(t); i++)
    {
        type_t* specialization = template_type_get_specialization_num(t, i);

        scope_entry_t* entry = named_type_get_symbol(specialization);
        template_argument_list_t* arguments = 
            template_specialized_type_get_template_arguments(entry->type_information);

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Checking with specialization '%s' (%p) at '%s:%d'\n",
                    entry->symbol_name,
                    entry->type_information,
                    entry->file,
                    entry->line);
        }

        if (same_template_argument_list(template_argument_list, arguments))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Returning template type %p\n", entry->type_information);
            }

            if (BITMAP_TEST(decl_context.decl_flags, DF_UPDATE_TEMPLATE_ARGUMENTS))
            {
               entry->type_information->template_arguments = template_argument_list;
            }

            return specialization;
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: No existing specialization matches, creating a fresh one\n");
    }

    type_t* specialized_type = NULL;
    scope_entry_t* primary_symbol = named_type_get_symbol(t->type->primary_specialization);

    if (primary_symbol->kind == SK_CLASS
            || primary_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
    {
        specialized_type = get_new_class_type(primary_symbol->decl_context);
    }
    else if (primary_symbol->kind == SK_FUNCTION)
    {
        // This will give us an updated function type
        specialized_type = update_type(template_argument_list, 
                primary_symbol->type_information, decl_context, filename, line);
    }
    else
    {
        internal_error("Invalid templated type", 0);
    }

    // State that this is a template specialized type
    specialized_type->is_template_specialized_type = 1;
    specialized_type->template_arguments = template_argument_list;
    // This can be NULL
    specialized_type->template_parameters = template_parameters;
    specialized_type->related_template_type = t;

    // State the class type nature
    if (specialized_type->kind == SK_CLASS)
    {
        if (has_dependent_template_arguments(template_argument_list, decl_context)
                || primary_symbol->entity_specs.is_template_parameter)
        {
            class_type_set_incomplete_dependent(specialized_type);
            class_type_set_is_dependent(specialized_type, 1);
        }
        else
        {
            class_type_set_incomplete_independent(specialized_type);
            class_type_set_is_dependent(specialized_type, 0);
        }
    }

    // Create a fake symbol with the just created specialized type
    scope_entry_t* specialized_symbol = counted_calloc(1, sizeof(*specialized_symbol), &_bytes_due_to_type_system);

    specialized_symbol->symbol_name = primary_symbol->symbol_name;
    specialized_symbol->kind = primary_symbol->kind;
    specialized_symbol->type_information = specialized_type;
    specialized_symbol->decl_context = primary_symbol->decl_context;

    specialized_symbol->line = line;
    specialized_symbol->file = filename;

    // Keep information of the entity
    specialized_symbol->entity_specs = primary_symbol->entity_specs;
    
    // Remove the extra template-scope we got from the primary one
    specialized_symbol->decl_context.template_scope = 
        specialized_symbol->decl_context.template_scope->contained_in;

    P_LIST_ADD(t->type->specialized_types, t->type->num_specialized_types, 
            get_user_defined_type(specialized_symbol));

    return get_user_defined_type(specialized_symbol);
}

template_parameter_list_t* template_type_get_template_parameters(type_t* t)
{
    ERROR_CONDITION(!is_template_type(t),
            "This is not a template type", 0);

    return t->type->template_parameter_list;
}

int template_type_get_num_specializations(type_t* t)
{
    ERROR_CONDITION(!is_template_type(t),
            "This is not a template type", 0);

    // +1 because of primary
    return t->type->num_specialized_types + 1;
}

type_t* template_type_get_specialization_num(type_t* t, int i)
{
    ERROR_CONDITION(!is_template_type(t),
            "This is not a template type", 0);

    if (i == 0)
    {
        return t->type->primary_specialization;
    }
    else
    {
        return t->type->specialized_types[i-1];
    }
}

void template_type_update_template_parameters(type_t* t, template_parameter_list_t* new_template_parameters)
{
    ERROR_CONDITION(!is_template_type(t),
            "This is not a template type", 0);

    template_parameter_list_t *template_parameters = t->type->template_parameter_list;

    ERROR_CONDITION(template_parameters->num_template_parameters 
            != new_template_parameters->num_template_parameters,
            "Template parameters should be of the same length", 0);

    int i;

    for (i = 0; i < template_parameters->num_template_parameters; i++)
    {
        template_parameter_t* template_parameter = template_parameters->template_parameters[i];
        template_parameter_t* new_template_parameter = new_template_parameters->template_parameters[i];

        ERROR_CONDITION ((new_template_parameter->kind != template_parameter->kind),
                        "Template parameter kinds do not match", 0);

        // Always update the entry (because the name might have changed)
        template_parameter->entry = new_template_parameter->entry;

        if (new_template_parameter->has_default_argument
                && !template_parameter->has_default_argument)
        {
            // Update the template parameter
            template_parameter->has_default_argument = 1;
            template_parameter->default_template_argument = new_template_parameter->default_template_argument;
        }
    }

    // Now reupdate the faked arguments for the primary template
    scope_entry_t* primary_symbol = named_type_get_symbol(t->type->primary_specialization);
    primary_symbol->type_information->template_arguments = compute_arguments_primary(template_parameters);
    primary_symbol->type_information->template_parameters = template_parameters;
}

char is_template_specialized_type(type_t* t)
{
    return (t != NULL && t->is_template_specialized_type);
}

template_argument_list_t* template_specialized_type_get_template_arguments(type_t* t)
{
    ERROR_CONDITION(!is_template_specialized_type(t),
            "This is not a template specialized type", 0);
    return t->template_arguments;
}

type_t* template_specialized_type_get_related_template_type(type_t* t)
{
    ERROR_CONDITION(!is_template_specialized_type(t),
            "This is not a template specialized type", 0);

    return t->related_template_type;
}

void template_specialized_type_update_template_parameters(type_t* t, template_parameter_list_t* template_parameters)
{
    ERROR_CONDITION(!is_template_specialized_type(t),
            "This is not a template specialized type", 0);

    t->template_parameters = template_parameters;
}

template_parameter_list_t* template_specialized_type_get_template_parameters(type_t* t)
{
    ERROR_CONDITION(!is_template_specialized_type(t),
            "This is not a template specialized type", 0);

    return t->template_parameters;
}

type_t* get_new_typedef(type_t* t)
{
    static Hash *_typedef_hash = NULL;

    if (_typedef_hash == NULL)
    {
        _typedef_hash = hash_create(HASH_SIZE, HASHFUNC(pointer_hash), KEYCMPFUNC(integer_comp));
    }

    type_t* result = hash_get(_typedef_hash, t);

    if (result == NULL)
    {
        result = get_simple_type();

        result->type->kind = STK_TYPEDEF;
        result->type->aliased_type = t;

        hash_put(_typedef_hash, t, result);
    }

    return result;
}

type_t* get_complex_type(type_t* t)
{
    static Hash *_complex_hash = NULL;

    if (_complex_hash == NULL)
    {
        _complex_hash = hash_create(HASH_SIZE, HASHFUNC(pointer_hash), KEYCMPFUNC(integer_comp));
    }

    type_t* result = hash_get(_complex_hash, t);

    if (result == NULL)
    {
        result = get_simple_type();

        *result->type = *t->type;
        result->type->is_complex = 1;

        hash_put(_complex_hash, t, result);
    }

    return result;
}

static Hash *_qualification[(CV_CONST | CV_VOLATILE | CV_RESTRICT) + 1];
static void init_qualification_hash(void)
{
    static char _qualif_hash_initialized = 0;

    if (!_qualif_hash_initialized)
    {
        int i;
        for (i = 0; i < 8; i++)
        {
            _qualification[i] = hash_create(HASH_SIZE, HASHFUNC(pointer_hash), KEYCMPFUNC(integer_comp));
        }
        _qualif_hash_initialized = 1;
    }
}

type_t* get_unqualified_type(type_t* t)
{
    t = advance_over_typedefs(t);
    ERROR_CONDITION(t->unqualified_type == NULL, "This cannot be NULL", 0);
    return t->unqualified_type;
}

static
type_t* get_qualified_type(type_t* original, cv_qualifier_t cv_qualification)
{
    // Ensure it is initialized
    init_qualification_hash();

    ERROR_CONDITION(original->unqualified_type == NULL, "This cannot be NULL", 0);

    if (cv_qualification == CV_NONE)
    {
        return original->unqualified_type;
    }

    // Lookup based on the unqualified type
    type_t* qualified_type = (type_t*)hash_get(
            _qualification[(int)(cv_qualification)], 
            original->unqualified_type);

    if (qualified_type == NULL)
    {
        qualified_type = counted_calloc(1, sizeof(*qualified_type), &_bytes_due_to_type_system);
        *qualified_type = *original;
        qualified_type->cv_qualifier = cv_qualification;
        qualified_type->unqualified_type = original->unqualified_type;

        hash_put(_qualification[(int)(cv_qualification)], 
                original->unqualified_type, 
                qualified_type);
    }

    return qualified_type;
}

type_t* get_cv_qualified_type(type_t* t, cv_qualifier_t cv_qualifier)
{
    return get_qualified_type(t, cv_qualifier);
}

type_t* get_const_qualified_type(type_t* t)
{
    return get_qualified_type(t, (t->cv_qualifier | CV_CONST));
}

type_t* get_volatile_qualified_type(type_t* t)
{
    return get_qualified_type(t, (t->cv_qualifier | CV_VOLATILE));
}

type_t* get_restrict_qualified_type(type_t* t)
{
    return get_qualified_type(t, (t->cv_qualifier | CV_RESTRICT));
}

type_t* get_pointer_type(type_t* t)
{
    static Hash *_pointer_types = NULL;

    if (_pointer_types == NULL)
    {
        _pointer_types = hash_create(HASH_SIZE, HASHFUNC(pointer_hash), KEYCMPFUNC(integer_comp));
    }

    type_t* pointed_type = hash_get(_pointer_types, t);

    if (pointed_type == NULL)
    {
        pointed_type = counted_calloc(1, sizeof(*pointed_type), &_bytes_due_to_type_system);
        pointed_type->kind = TK_POINTER;
        pointed_type->unqualified_type = pointed_type;
        pointed_type->pointer = counted_calloc(1, sizeof(*pointed_type->pointer), &_bytes_due_to_type_system);
        pointed_type->pointer->pointee = t;

        if (is_function_type(t))
        {
            pointed_type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_function_pointer;
        }
        else
        {
            pointed_type->size = CURRENT_CONFIGURATION(type_environment)->sizeof_pointer;
        }

        hash_put(_pointer_types, t, pointed_type);
    }

    return pointed_type;
}

static Hash *_lvalue_reference_types = NULL;
static Hash *_rvalue_reference_types = NULL;

static type_t* get_internal_reference_type(type_t* t, char is_rvalue_ref)
{
    C_LANGUAGE()
    {
        internal_error("No referenced types should be created in C", 0);
    }

    if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        internal_error("Trying to create a reference to reference type." , 0);
    }

    ERROR_CONDITION(t == NULL,
            "Trying to create a reference of a null type", 0);

    Hash **_reference_types = NULL;
    if (is_rvalue_ref)
    {
        _reference_types = &_lvalue_reference_types;
    }
    else
    {
        _reference_types = &_rvalue_reference_types;
    }

    if ((*_reference_types) == NULL)
    {
        (*_reference_types) = hash_create(HASH_SIZE, HASHFUNC(pointer_hash), KEYCMPFUNC(integer_comp));
    }

    type_t* referenced_type = hash_get((*_reference_types), t);

    if (referenced_type == NULL)
    {
        referenced_type = counted_calloc(1, sizeof(*referenced_type), &_bytes_due_to_type_system);
        if (!is_rvalue_ref)
        {
            referenced_type->kind = TK_LVALUE_REFERENCE;
        }
        else
        {
            referenced_type->kind = TK_RVALUE_REFERENCE;
        }
        referenced_type->unqualified_type = referenced_type;
        referenced_type->pointer = counted_calloc(1, sizeof(*referenced_type->pointer), &_bytes_due_to_type_system);
        referenced_type->pointer->pointee = t;

        hash_put((*_reference_types), t, referenced_type);
    }

    return referenced_type;
}

type_t* get_lvalue_reference_type(type_t* t)
{
    return get_internal_reference_type(t, /* is_rvalue_ref */ 0);
}

type_t* get_rvalue_reference_type(type_t* t)
{
    return get_internal_reference_type(t, /* is_rvalue_ref */ 1);
}

type_t* get_pointer_to_member_type(type_t* t, scope_entry_t* class_entry)
{
    static Hash *_class_types = NULL;

    // First lookup using the class symbol
    if (_class_types == NULL)
    {
        _class_types = hash_create(HASH_SIZE, HASHFUNC(pointer_hash), KEYCMPFUNC(integer_comp));
    }

    // First then lookup using the 
    Hash* class_type_hash = hash_get(_class_types, class_entry);

    if (class_type_hash == NULL)
    {
        class_type_hash = hash_create(HASH_SIZE, HASHFUNC(pointer_hash), KEYCMPFUNC(integer_comp));

        hash_put(_class_types, class_entry, class_type_hash);
    }

    type_t* pointer_to_member = hash_get(class_type_hash, t);

    if (pointer_to_member == NULL)
    {
        pointer_to_member = counted_calloc(1, sizeof(*pointer_to_member), &_bytes_due_to_type_system);
        pointer_to_member->kind = TK_POINTER_TO_MEMBER;
        pointer_to_member->unqualified_type = pointer_to_member;
        pointer_to_member->pointer = counted_calloc(1, sizeof(*pointer_to_member->pointer), &_bytes_due_to_type_system);
        pointer_to_member->pointer->pointee = t;
        pointer_to_member->pointer->pointee_class = class_entry;

        if (is_function_type(t))
        {
            pointer_to_member->size 
                = CURRENT_CONFIGURATION(type_environment)->sizeof_pointer_to_member_function;
        }
        else
        {
            pointer_to_member->size 
                = CURRENT_CONFIGURATION(type_environment)->sizeof_pointer_to_data_member;
        }

        hash_put(class_type_hash, t, pointer_to_member);
    }

    return pointer_to_member;
}

type_t* get_array_type(type_t* element_type, AST expression, decl_context_t decl_context)
{
    // This type is not efficiently managed since sometimes we cannot state
    // which is its length. On the other hand, inside type calculus this type
    // is barely needed since normally expressions cannot express array types.
    //
    // E.g.
    //
    // // C99
    // void f(int x, int y, int v[x][y]);
    // {
    //    // This one is only valid in GCC
    //    int k[x + y];
    // }
    //
    // // C++ 
    // template <int _N, int _M>
    // void f()
    // {
    //   int k[_N + _M];
    // } 
    //
    //
    // Fold if possible the expression
    type_t* result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);
    result->kind = TK_ARRAY;
    result->unqualified_type = result;
    result->array = counted_calloc(1, sizeof(*(result->array)), &_bytes_due_to_type_system);
    result->array->element_type = element_type;
    result->array->array_expr = expression;
    result->array->array_expr_decl_context = decl_context;

#if 0
    // FIX THIS - We need "is_constant_expression" in cxx-cexpr.c
    if (!is_dependent_expression(expression, decl_context))
    {
        literal_value_t literal_val 
            = evaluate_constant_expression(expression, decl_context);
        result->size = element_type->size * literal_value_to_uint(literal_val);
    }
#endif

    return result;
}

type_t* get_vector_type(type_t* element_type, unsigned int vector_size)
{
    // This type is not efficiently managed
    type_t* result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);
    
    result->kind = TK_VECTOR;
    result->unqualified_type = result;

    result->vector = counted_calloc(1, sizeof(*(result->vector)), &_bytes_due_to_type_system);
    result->vector->element_type = element_type;
    result->vector->vector_size = vector_size;

    return result;
}

char is_vector_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_VECTOR);
}

int vector_type_get_vector_size(type_t* t)
{
    ERROR_CONDITION(!is_vector_type(t), "This is not a vector type", 0);
    t = advance_over_typedefs(t);

    return t->vector->vector_size;
}

type_t* vector_type_get_element_type(type_t* t)
{
    ERROR_CONDITION(!is_vector_type(t), "This is not a vector type", 0);
    t = advance_over_typedefs(t);

    return t->vector->element_type;
}

type_t* get_new_function_type(type_t* t, parameter_info_t* parameter_info, int num_parameters)
{
    type_t* result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);

    result->kind = TK_FUNCTION;
    result->unqualified_type = result;
    result->function = counted_calloc(1, sizeof(*(result->function)), &_bytes_due_to_type_system);
    result->function->return_type = t;

    result->function->parameter_list = counted_calloc(num_parameters, sizeof(*( result->function->parameter_list )), &_bytes_due_to_type_system);
    result->function->num_parameters = num_parameters;

    int i;
    for (i = 0; i < num_parameters; i++)
    {
        parameter_info_t* new_parameter = counted_calloc(1, sizeof(*new_parameter), &_bytes_due_to_type_system);

        *new_parameter = parameter_info[i];

        result->function->parameter_list[i] = new_parameter;
    }


    return result;
}

type_t* get_nonproto_function_type(type_t* t, int num_parameters)
{
    // This type is not efficiently managed
    type_t* result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);

    result->kind = TK_FUNCTION;
    result->unqualified_type = result;
    result->function = counted_calloc(1, sizeof(*(result->function)), &_bytes_due_to_type_system);
    result->function->return_type = t;
    result->function->lacks_prototype = 1;

    int i;
    for (i = 0; i < num_parameters; i++)
    {
        parameter_info_t* new_parameter = counted_calloc(1, sizeof(*new_parameter), &_bytes_due_to_type_system);

        new_parameter->type_info = get_signed_int_type();

        P_LIST_ADD(result->function->parameter_list, 
                result->function->num_parameters, new_parameter);
    }

    return result;
}

int function_type_get_num_parameters(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    function_type = advance_over_typedefs(function_type);

    return function_type->function->num_parameters;
}

type_t* function_type_get_parameter_type_num(type_t* function_type, int num_param)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    function_type = advance_over_typedefs(function_type);

    ERROR_CONDITION(num_param >= function_type->function->num_parameters, 
            "Requested parameter %d out of bounds (number of parameters is %d)", 
            num_param, function_type->function->num_parameters);

    return function_type->function->parameter_list[num_param]->type_info;
}

char class_type_is_incomplete_dependent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->type->template_nature == TPN_INCOMPLETE_DEPENDENT;
}

char class_type_is_complete_dependent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->type->template_nature == TPN_COMPLETE_DEPENDENT;
}

char class_type_is_incomplete_independent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->type->template_nature == TPN_INCOMPLETE_INDEPENDENT;
}

char class_type_is_complete_independent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->type->template_nature == TPN_COMPLETE_INDEPENDENT;
}

char class_type_get_is_dependent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->type->class_info->is_dependent;
}

void class_type_set_is_dependent(type_t* t, char is_dependent)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    t->type->class_info->is_dependent = is_dependent;
}

void class_type_set_incomplete_dependent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    t->type->template_nature = TPN_INCOMPLETE_DEPENDENT;
}

void class_type_add_constructor(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    P_LIST_ADD(class_type->type->class_info->constructor_list, class_type->type->class_info->num_constructors, entry);
}

void class_type_set_destructor(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    class_type->type->class_info->destructor = entry;
}

scope_entry_t* class_type_get_destructor(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    return class_type->type->class_info->destructor;
}

int class_type_get_num_copy_assignment_operators(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    return class_type->type->class_info->num_copy_assignment_operator_functions;
}

scope_entry_t* class_type_get_copy_assignment_operator_num(type_t* class_type, int num)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    return class_type->type->class_info->copy_assignment_operator_function_list[num];
}

void class_type_add_copy_assignment_operator(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    P_LIST_ADD(class_type->type->class_info->copy_assignment_operator_function_list, 
            class_type->type->class_info->num_copy_assignment_operator_functions, entry);
}

void class_type_add_copy_constructor(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    P_LIST_ADD(class_type->type->class_info->copy_constructor_list,
            class_type->type->class_info->num_copy_constructors, entry);
}

int class_type_get_num_copy_constructors(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    return class_type->type->class_info->num_copy_constructors;
}

scope_entry_t* class_type_get_copy_constructor_num(type_t* class_type, int num)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    return class_type->type->class_info->copy_constructor_list[num];
}

void class_type_add_conversion_function(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    // Only add once
    P_LIST_ADD_ONCE(class_type->type->class_info->conversion_functions, 
            class_type->type->class_info->num_conversion_functions, entry);
}

void class_type_add_nonstatic_data_member(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    P_LIST_ADD(class_type->type->class_info->nonstatic_data_members, 
            class_type->type->class_info->num_nonstatic_data_members, entry);
}

void class_type_add_static_data_member(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    P_LIST_ADD(class_type->type->class_info->static_data_members, 
            class_type->type->class_info->num_static_data_members, entry);
}

void class_type_set_complete_dependent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    t->type->template_nature = TPN_COMPLETE_DEPENDENT;
}

void class_type_set_incomplete_independent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    t->type->template_nature = TPN_INCOMPLETE_INDEPENDENT;
}

void class_type_set_complete_independent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    t->type->template_nature = TPN_COMPLETE_INDEPENDENT;
}

void class_type_set_instantiation_trees(type_t* t, AST body, AST base_clause)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);

    t->type->template_class_base_clause = base_clause;
    t->type->template_class_body = body;
}

void class_type_get_instantiation_trees(type_t* t, AST *body, AST *base_clause)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);

    *body = t->type->template_class_body;
    *base_clause = t->type->template_class_base_clause;
}

void enum_type_add_enumerator(type_t* t, scope_entry_t* enumeration_item)
{
    simple_type_t* enum_type = t->type;
    P_LIST_ADD(enum_type->enum_info->enumeration_list, 
            enum_type->enum_info->num_enumeration,
            enumeration_item);
}

// This function returns a copy of the old type
type_t* unnamed_class_enum_type_set_name(type_t* t, scope_entry_t* entry)
{
    ERROR_CONDITION (!(t->kind == TK_DIRECT 
                && (t->type->kind == STK_CLASS
                    || t->type->kind == STK_ENUM)), 
            "This should be an unnamed enum or class\n", 0);

    type_t* new_type = counted_calloc(1, sizeof(*new_type), &_bytes_due_to_type_system);

    // Wild copy
    *new_type = *t;

    *t = *(get_user_defined_type(entry));

    return new_type;
}
// ---

type_t* advance_over_typedefs_with_cv_qualif(type_t* t1, cv_qualifier_t* cv_qualif)
{
    if (t1 == NULL)
        return NULL;

    if (cv_qualif != NULL)
    {
        *cv_qualif = t1->cv_qualifier;
    }
    // Advance over typedefs
    while (is_typedef_type(t1))
    {
        t1 = get_aliased_type(t1);
        if (cv_qualif != NULL)
        {
            *cv_qualif |= t1->cv_qualifier;
        }
    }

    return t1;
}

char is_typedef_type(type_t* t1)
{
    if (t1 == NULL)
        return 0;

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

        if (user_defined_type != NULL 
                && user_defined_type->kind == TK_DIRECT 
                && user_defined_type->type != NULL 
                && user_defined_type->type->kind == STK_TYPEDEF)
        {
            return 1;
        }
    }

    return 0;
}

static type_t* get_aliased_type(type_t* t1)
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

type_t* typedef_type_get_aliased_type(type_t* t1)
{
    return get_aliased_type(t1);
}

char function_type_get_lacking_prototype(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    function_type = advance_over_typedefs(function_type);

    return function_type->function->lacks_prototype;
}

char function_type_get_has_ellipsis(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    function_type = advance_over_typedefs(function_type);

    if (function_type->function->num_parameters == 0)
        return 0;

    return function_type
        ->function
        ->parameter_list[function_type->function->num_parameters - 1]
        ->is_ellipsis;
}

void class_type_add_base_class(type_t* class_type, scope_entry_t* base_class, char is_virtual)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    base_class_info_t* new_base_class = counted_calloc(1, sizeof(*new_base_class), &_bytes_due_to_type_system);
    new_base_class->class_symbol = base_class;
    /* redundant */ new_base_class->class_type = base_class->type_information;
    new_base_class->is_virtual = is_virtual;

    class_info_t* class_info = class_type->type->class_info;
    // Only add once
    P_LIST_ADD_ONCE(class_info->base_classes_list, class_info->num_bases, new_base_class);
}

void class_type_set_inner_context(type_t* class_type, decl_context_t decl_context)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    class_type->type->class_info->inner_decl_context = decl_context;
}

decl_context_t class_type_get_inner_context(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    return class_type->type->class_info->inner_decl_context;
}

int class_type_get_num_bases(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    class_info_t* class_info = class_type->type->class_info;

    return class_info->num_bases;
}

int class_type_get_num_nonstatic_data_members(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    class_info_t* class_info = class_type->type->class_info;

    return class_info->num_nonstatic_data_members;
}

scope_entry_t* class_type_get_nonstatic_data_member_num(type_t* class_type, int i)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    class_info_t* class_info = class_type->type->class_info;

    return class_info->nonstatic_data_members[i];
}

int class_type_get_num_static_data_members(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    class_info_t* class_info = class_type->type->class_info;

    return class_info->num_static_data_members;
}

scope_entry_t* class_type_get_static_data_member_num(type_t* class_type, int i)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    class_info_t* class_info = class_type->type->class_info;

    return class_info->static_data_members[i];
}

scope_entry_t* class_type_get_base_num(type_t* class_type, int num, char *is_virtual)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    class_info_t* class_info = class_type->type->class_info;

    if (is_virtual != NULL)
    {
        *is_virtual = class_info->base_classes_list[num]->is_virtual;
    }

    return class_info->base_classes_list[num]->class_symbol;
}

int class_type_get_num_constructors(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    return class_type->type->class_info->num_constructors;
}

scope_entry_t* class_type_get_constructors_num(type_t* class_type, int num)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    return class_type->type->class_info->constructor_list[num];
}

int class_type_get_num_conversions(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    return class_type->type->class_info->num_conversion_functions;
}

scope_entry_t* class_type_get_conversion_num(type_t* class_type, int num)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    return class_type->type->class_info->conversion_functions[num];
}

scope_entry_list_t* class_type_get_all_conversions(type_t* class_type, decl_context_t decl_context)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    // For every base class, get its conversions
    int i;
    int num_bases = class_type_get_num_bases(class_type);
    scope_entry_list_t* base_result = NULL;
    for (i = 0; i < num_bases; i++)
    {
        type_t* base_class_type = class_type_get_base_num(class_type, i, /* is_virtual = */ NULL)->type_information;
        scope_entry_list_t* base_conversors = class_type_get_all_conversions(base_class_type, decl_context);

        // Append
        if (base_result == NULL)
        {
            base_result = base_conversors;
        }
        else
        {
            scope_entry_list_t* last = base_result;
            while (last->next != NULL)
                last = last->next;
            
            last->next = base_conversors;
        }
    }

    // Now for every conversor of this class, remove it from 'result'
    scope_entry_list_t* this_class_conversors = NULL;
    int num_conversors = class_type_get_num_conversions(class_type);
    for (i = 0; i < num_conversors; i++)
    {
        scope_entry_t* entry = class_type_get_conversion_num(class_type, i);

        scope_entry_list_t* prev_it = NULL;
        scope_entry_list_t* it = base_result;

        while (it != NULL)
        {
            scope_entry_t* current = it->entry;

            if (equivalent_types(current->type_information, entry->type_information))
            {
                // Remove 'it' from the list
                if (prev_it == NULL)
                {
                    // Update the head
                    base_result = it->next;
                }
                else
                {
                    prev_it->next = it->next;
                }
                it = it->next;
            }
            else
            {
                prev_it = it;
                it = it->next;
            }
        }

        // At the same time build the conversor list of this class
        {
            scope_entry_list_t* new_item = counted_calloc(1, sizeof(*new_item), &_bytes_due_to_type_system);
            new_item->entry = entry;
            new_item->next = this_class_conversors;

            this_class_conversors = new_item;
        }
    }

    // Now append the filtered one to the result
    //
    {
        scope_entry_list_t* last = this_class_conversors;

        if (this_class_conversors == NULL)
        {
            this_class_conversors = base_result;
        }
        else
        {
            while (last->next != NULL)
                last = last->next;

            last->next = base_result;
        }
    }

    return this_class_conversors;
}

type_t* advance_over_typedefs(type_t* t1)
{
    return advance_over_typedefs_with_cv_qualif(t1, NULL);
}

/*
 * States if two types are equivalent. This means that they are the same
 * (ignoring typedefs). Just plain comparison, no standard conversion is
 * performed. cv-qualifiers are relevant for comparison
 */
static char equivalent_simple_types(type_t *t1, type_t *t2);

static type_t* advance_dependent_typename(type_t* t)
{
    ERROR_CONDITION(!is_dependent_typename_type(t), "This must be a dependent typename", 0);

    cv_qualifier_t cv_qualif = t->cv_qualifier;

    decl_context_t dependent_decl_context;
    scope_entry_t* dependent_entry = NULL;
    AST nested_name = NULL;
    AST unqualified_part = NULL;

    dependent_typename_get_components(t, &dependent_entry, 
            &dependent_decl_context, &nested_name, &unqualified_part);

    if (dependent_entry->kind == SK_TEMPLATE_TYPE_PARAMETER)
        return t;

    if (dependent_entry->kind == SK_CLASS)
    {
        type_t* class_type = dependent_entry->type_information;

        decl_context_t inner_context = class_type_get_inner_context(class_type);

        scope_entry_list_t* result_list = query_nested_name(inner_context, 
                NULL, nested_name, unqualified_part);

        if (result_list != NULL)
        {
            ERROR_CONDITION(result_list->next != NULL,
                    "Invalid result when solving a dependent typename", 0);

            // Add the qualifications found so far
            cv_qualifier_t cv_qualif_2 = CV_NONE;
            advance_over_typedefs_with_cv_qualif(result_list->entry->type_information, &cv_qualif_2);
            cv_qualif_2 |= cv_qualif;

            return get_cv_qualified_type(get_user_defined_type(result_list->entry), cv_qualif_2);
        }
    }

    return t;
}

char equivalent_types(type_t* t1, type_t* t2)
{

    ERROR_CONDITION( (t1 == NULL || t2 == NULL), "No type can be null here", 0);

    cv_qualifier_t cv_qualifier_t1, cv_qualifier_t2;

    // This is a small adjustement that has to be performed because of the stupid
    // nature of dependent typenames
    // Try to advance as much as possible every type because of typedefs
    // like in this example
    //
    // template <typename _T>
    // struct B
    // {
    //   typedef typename A<_T>::T T;
    //   T f1();
    //   T f2();
    // };
    //
    // template <typename _T>
    // typename B<_T>::T f1()
    // {
    // }
    //
    // template <typename _T>
    // typename A<_T>::T f2()
    // {
    // }
    //
    // In this context both A<_T>::T and B<_T>::T are the same
    //
    if (is_dependent_typename_type(t1))
    {
        t1 = advance_dependent_typename(t1);
    }

    if (is_dependent_typename_type(t2))
    {
        t2 = advance_dependent_typename(t2);
    }


    // Advance over typedefs
    t1 = advance_over_typedefs_with_cv_qualif(t1, &cv_qualifier_t1);
    t2 = advance_over_typedefs_with_cv_qualif(t2, &cv_qualifier_t2);

    // FIXME
    if (t1->kind != t2->kind)
    {
        return 0;
    }

    char result = 0;

    switch (t1->kind)
    {
        case TK_DIRECT :
            result = equivalent_simple_types(t1, t2);
            break;
        case TK_POINTER :
            result = equivalent_pointer_type(t1->pointer, t2->pointer);
            break;
        case TK_LVALUE_REFERENCE :
            result = equivalent_pointer_type(t1->pointer, t2->pointer);
            break;
        case TK_RVALUE_REFERENCE :
            result = equivalent_pointer_type(t1->pointer, t2->pointer);
            break;
        case TK_POINTER_TO_MEMBER :
            result = equivalent_pointer_to_member_type(t1, t2);
            break;
        case TK_ARRAY :
            result = equivalent_array_type(t1->array, t2->array);
            break;
        case TK_FUNCTION :
            result = equivalent_function_type(t1, t2);
            break;
        case TK_VECTOR :
            result = equivalent_vector_type(t1, t2);
            break;
        default :
            internal_error("Unknown type kind (%d)\n", t1->kind);
    }

    result &= equivalent_cv_qualification(cv_qualifier_t1, cv_qualifier_t2);

    return result;
}

static
char equivalent_builtin_type(simple_type_t* t1, simple_type_t *t2);

static char equivalent_named_types(scope_entry_t* s1, scope_entry_t* s2)
{
    if (s1->entity_specs.is_template_parameter
            || s2->entity_specs.is_template_parameter)
    {
        if (s1->entity_specs.is_template_parameter
                && s2->entity_specs.is_template_parameter)
        {
            return ((s1->kind == s2->kind)
                    && (s1->entity_specs.template_parameter_nesting == s2->entity_specs.template_parameter_nesting)
                    && (s1->entity_specs.template_parameter_position == s2->entity_specs.template_parameter_position));
        }
        else
        {
            return 0;
        }
    }
    else
    {
        return equivalent_types(s1->type_information, s2->type_information);
    }
}

char equivalent_simple_types(type_t *p_t1, type_t *p_t2)
{
    simple_type_t* t1 = p_t1->type;
    simple_type_t* t2 = p_t2->type;

    char result = 0;
    if (t1->kind != t2->kind)
    {
        return 0;
    }

    switch (t1->kind)
    {
        case STK_BUILTIN_TYPE :
            result = equivalent_builtin_type(t1, t2);
            break;
        case STK_TEMPLATE_TYPE :
            /* Fall-through */
        case STK_CLASS :
            /* Fall-through */
        case STK_ENUM :
            // Pointer comparison MUST work
            // (if not, something is broken)
            result = (t1 == t2);
            break;
        case STK_USER_DEFINED :
            result = equivalent_named_types(t1->user_defined_type, 
                    t2->user_defined_type);
            break;
        case STK_TEMPLATE_DEPENDENT_TYPE :
            result = compare_template_dependent_typename_types(p_t1, p_t2);
            break;
        case STK_TYPEOF :
            internal_error("__typeof__ comparison still not implemented", 0);
            break;
        case STK_VA_LIST :
            // If both are __builtin_va_list, this is trivially true
            result = 1;
            break;
        default :
            internal_error("Unknown simple type kind (%d)", t1->kind);
            return 0;
    }

    return result;
}

char equivalent_builtin_type(simple_type_t* t1, simple_type_t *t2)
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

    // GCC extension for complex 
    if (t1->is_complex != t2->is_complex)
    {
        return 0;
    }
    
    // Ok, nothing makes us think they might be different
    return 1;
}

static char equivalent_pointer_to_member_type(type_t* t1, type_t* t2)
{
    return equivalent_pointer_type(t1->pointer, 
            t2->pointer)
        && equivalent_types(get_user_defined_type(t1->pointer->pointee_class), 
                get_user_defined_type(t2->pointer->pointee_class));
}

static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2)
{
    if (!equivalent_types(t1->pointee, t2->pointee))
    {
        return 0;
    }

    return 1;
}

static char equivalent_array_type(array_info_t* t1, array_info_t* t2)
{
    if (!equivalent_types(t1->element_type, t2->element_type))
        return 0;

    if (t1->array_expr != NULL
            && t2->array_expr != NULL)
    {
        CXX_LANGUAGE()
        {
            if (!same_functional_expression(t1->array_expr, t1->array_expr_decl_context, 
                        t2->array_expr, t2->array_expr_decl_context))
                return 0;
        }
        C_LANGUAGE()
        {
            literal_value_t literal_1 
                = evaluate_constant_expression(t1->array_expr, t1->array_expr_decl_context);
            literal_value_t literal_2
                = evaluate_constant_expression(t2->array_expr, t2->array_expr_decl_context);

            if (literal_1.kind != LVK_DEPENDENT_EXPR
                    && literal_1.kind != LVK_INVALID
                    && literal_2.kind != LVK_DEPENDENT_EXPR
                    && literal_2.kind != LVK_INVALID
                    && !equal_literal_values(literal_1, literal_2))
            {
                return 0;
            }
            else
            {
                // Otherwise do nothing since VLA's are sort of a flexible thing
                //
                // void f(int n, int a[10][n]);
                // void f(int n, int a[10][n+1]);
                //
                // They are not incompatible
            }
        }
    }
    else
    {
        // int a[] does not match with int a[10]; (it will match via
        // array-to-pointer, but this is not the case we are handling now)
        if ((t1->array_expr == NULL
                && t2->array_expr != NULL)
                || (t1->array_expr != NULL
                    && t2->array_expr == NULL))
        {
            return 0;
        }
    }
    
    return 1;
}

cv_qualifier_t* get_innermost_cv_qualifier(type_t* t)
{
    // For types that do not have a cv qualifier on their own
    static cv_qualifier_t dummy_cv_qualif = CV_NONE;

    // This will avoid accidental modifications from outside
    dummy_cv_qualif = CV_NONE;

    switch (t->kind)
    {
        case TK_DIRECT :
            {
                return &(t->cv_qualifier);
                break;
            }
        case TK_ARRAY :
            {
                return get_innermost_cv_qualifier(t->array->element_type);
            }
        case TK_POINTER :
        case TK_POINTER_TO_MEMBER :
        case TK_LVALUE_REFERENCE :
        case TK_RVALUE_REFERENCE :
            {
                return get_innermost_cv_qualifier(t->pointer->pointee);
            }
        case TK_FUNCTION :
            {
                return get_innermost_cv_qualifier(t->function->return_type);
            }
        default:
            {
                internal_error("Unexpected node type %d\n", t->kind);
            }
    }
}

#if 0
/*
 * This function just checks functional types
 */
char overloaded_function(type_t* ft1, type_t* ft2, decl_context_t decl_context)
{
    function_info_t* t1 = ft1->function;
    function_info_t* t2 = ft2->function;

    if (!compatible_parameters(t1, t2, decl_context))
        return 1;

    // If one has return type but the other does not this is an overload
    // (technically this is ill-formed)
    if (((t1->return_type == NULL)
                && (t2->return_type != NULL))
            || ((t2->return_type == NULL)
                && (t1->return_type != NULL)))
        return 1;

    if (!equivalent_cv_qualification(ft1->cv_qualifier, 
                ft2->cv_qualifier))
        return 1;


    // Destructors, constructors, operator functions and conversion functions
    // will not have a full direct type
    if (t1->return_type == NULL 
            && t2->return_type == NULL)
        return 0;

    if (!equivalent_types(t1->return_type, t2->return_type, decl_context))
    {
        return 1;
    }

    return 0;
}
#endif

static char equivalent_vector_type(type_t* t1, type_t* t2)
{
    // This mimics gcc behaviour
    return ((equivalent_types(t1->vector->element_type, t2->vector->element_type)))
        && (t1->vector->vector_size == t2->vector->vector_size);
}

static char equivalent_function_type(type_t* ft1, type_t* ft2)
{
    function_info_t* t1 = ft1->function;
    function_info_t* t2 = ft2->function;

    if (t1->return_type == NULL
            && t2->return_type == NULL)
    {
        // If both are null they are equivalent
    }
    else if (t1->return_type == NULL
            || t2->return_type == NULL)
    {
        // This path reveals some error but let ignore it
        return 0;
    }
    else if (!equivalent_types(t1->return_type, t2->return_type))
    {
        return 0;
    }

    if (!compatible_parameters(t1, t2))
        return 0;

    if (!equivalent_cv_qualification(ft1->cv_qualifier, ft2->cv_qualifier))
        return 0;

    return 1;
}

char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
    // Oh, this turned to be that easy
    return (cv1 == cv2);
}

static char compatible_parameters(function_info_t* t1, function_info_t* t2)
{
    if (t1->num_parameters != t2->num_parameters)
        return 0;

    char still_compatible = 1;
    int i;

    for (i = 0; (i < t1->num_parameters) && still_compatible; i++)
    {
        if (t1->parameter_list[i]->is_ellipsis
                || t2->parameter_list[i]->is_ellipsis)
        {
            still_compatible = (t1->parameter_list[i]->is_ellipsis && t2->parameter_list[i]->is_ellipsis);
            continue;
        }

        type_t* par1 = t1->parameter_list[i]->type_info;
        type_t* par2 = t2->parameter_list[i]->type_info;

        if (!equivalent_types(get_unqualified_type(par1), get_unqualified_type(par2)))
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

                if (!equivalent_types(array_type->array->element_type, pointer_type->pointer->pointee))
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
                    if (!equivalent_types(pointer_type->pointer->pointee, function_type))
                    {
                        still_compatible = 0;
                    }
                }
            }
            else // No other applies
            {
                still_compatible = 0;
            }
        }
    }

    return still_compatible;
}

static char syntactic_comparison_of_template_id(AST template_id_1, decl_context_t decl_context_1,
        AST template_id_2, decl_context_t decl_context_2, int nesting_level)
{
    ERROR_CONDITION((ASTType(template_id_1) != AST_TEMPLATE_ID
                || ASTType(template_id_2) != AST_TEMPLATE_ID), 
            "Only template-id are valid", 0);

    AST symbol_name_1 = ASTSon0(template_id_1);
    AST symbol_name_2 = ASTSon0(template_id_2);
    if (strcmp(ASTText(symbol_name_1), ASTText(symbol_name_2)) != 0)
    {
        return 0;
    }

    AST template_arguments_1 = ASTSon1(template_id_1);
    AST template_arguments_2 = ASTSon1(template_id_2);

    template_argument_list_t* t_arg_list_1 = get_template_arguments_from_syntax(template_arguments_1, 
            decl_context_1, nesting_level);
    template_argument_list_t* t_arg_list_2 = get_template_arguments_from_syntax(template_arguments_2, 
            decl_context_2, nesting_level);

    return same_template_argument_list(t_arg_list_1, t_arg_list_2);
}

static char syntactic_comparison_of_symbol(AST symbol_1, AST symbol_2)
{
    ERROR_CONDITION((ASTType(symbol_1) != AST_SYMBOL
                || ASTType(symbol_2) != AST_SYMBOL), 
            "Only symbols are valid", 0);
    return (strcmp(ASTText(symbol_1), ASTText(symbol_2)) == 0);
}

char syntactic_comparison_of_nested_names(
        AST nested_name_1, AST nested_name_2, decl_context_t decl_context_1,
        AST unqualified_part_1, AST unqualified_part_2, decl_context_t decl_context_2)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "Comparing nested-name parts '%s%s' vs '%s%s'\n", 
                prettyprint_in_buffer(nested_name_1), 
                prettyprint_in_buffer(unqualified_part_1), 
                prettyprint_in_buffer(nested_name_2), 
                prettyprint_in_buffer(unqualified_part_2));
    }

    int nesting_level = 0;

    while (nested_name_1 != NULL
            && nested_name_2 != NULL)
    {
        AST current_name_1 = ASTSon0(nested_name_1);
        AST current_name_2 = ASTSon0(nested_name_2);

        if (ASTType(current_name_1) != ASTType(current_name_2))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Nested-name element is different '%s' vs '%s'\n",
                        ast_print_node_type(ASTType(current_name_1)),
                        ast_print_node_type(ASTType(current_name_2)));
            }
            return 0;
        }

        if (ASTType(current_name_1) == AST_SYMBOL)
        {
            if (!syntactic_comparison_of_symbol(current_name_1, current_name_2))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Syntactic comparison of symbols '%s' vs '%s' failed\n",
                            prettyprint_in_buffer(current_name_1),
                            prettyprint_in_buffer(current_name_2));
                }
                return 0;
            }
        }
        else if (ASTType(current_name_1) == AST_TEMPLATE_ID)
        {
            if (!syntactic_comparison_of_template_id(current_name_1, decl_context_1,
                        current_name_2, decl_context_2, nesting_level))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Syntactic comparison of template-ids '%s' vs '%s' failed\n",
                            prettyprint_in_buffer(current_name_1),
                            prettyprint_in_buffer(current_name_2));
                }
                return 0;
            }
            nesting_level++;
        }
        else
        {
            internal_error("Invalid node type '%s'\n", ast_print_node_type(ASTType(current_name_1)));
        }

        nested_name_1 = ASTSon1(nested_name_1);
        nested_name_2 = ASTSon1(nested_name_2);
    }

    if (nested_name_1 != NULL
            || nested_name_2 != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "One of the nested names is longer than the other\n");
        }
        return 0;
    }


    if (ASTType(unqualified_part_1) != ASTType(unqualified_part_2))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Unqualified part node kind '%s' is not the same as '%s'\n",
                    ast_print_node_type(ASTType(unqualified_part_1)),
                    ast_print_node_type(ASTType(unqualified_part_2)));
        }
        return 0;
    }

    if (ASTType(unqualified_part_1) == AST_SYMBOL)
    {
        if (!syntactic_comparison_of_symbol(unqualified_part_1, unqualified_part_2))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Syntactic comparison of unqualified symbols '%s' vs '%s' failed\n",
                        prettyprint_in_buffer(unqualified_part_1),
                        prettyprint_in_buffer(unqualified_part_2));
            }
            return 0;
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Syntactic comparison of unqualified template-id '%s' vs '%s' failed\n",
                    prettyprint_in_buffer(unqualified_part_1),
                    prettyprint_in_buffer(unqualified_part_2));
        }
        if (!syntactic_comparison_of_template_id(unqualified_part_1, 
                    decl_context_1,
                    unqualified_part_2, decl_context_2, nesting_level))
            return 0;
        nesting_level++;
    }

    return 1;
}

static char compare_template_dependent_typename_types(type_t* p_t1, type_t* p_t2)
{
    DEBUG_CODE()
    {
        fprintf(stderr , "Comparing template dependent typenames '%s' and '%s'\n",
                print_declarator(p_t1),
                print_declarator(p_t2));
    }
    // It is likely that in these contrived cases the user will use a typedef
    // to help himself so most of the time this fast path will be fired
    if (p_t1 == p_t2)
        return 1;

    // This should be easier now, no context needed!
    decl_context_t decl_context_1;
    scope_entry_t* dependent_entry_1 = NULL;
    AST nested_name_1 = NULL;
    AST unqualified_part_1 = NULL;

    decl_context_t decl_context_2;
    scope_entry_t* dependent_entry_2;
    AST nested_name_2;
    AST unqualified_part_2;

    dependent_typename_get_components(p_t1, &dependent_entry_1, 
            &decl_context_1, &nested_name_1, &unqualified_part_1);
    type_t* type_to_compare_1 = NULL;
    if (dependent_entry_1->kind == SK_TEMPLATE_TYPE_PARAMETER)
    {
        type_to_compare_1 = get_user_defined_type(dependent_entry_1);
    }
    else
    {
        type_to_compare_1 = dependent_entry_1->type_information;
    }

    dependent_typename_get_components(p_t2, &dependent_entry_2, 
            &decl_context_2, &nested_name_2, &unqualified_part_2);
    type_t* type_to_compare_2 = NULL;
    if (dependent_entry_2->kind == SK_TEMPLATE_TYPE_PARAMETER)
    {
        type_to_compare_2 = get_user_defined_type(dependent_entry_2);
    }
    else
    {
        type_to_compare_2 = dependent_entry_2->type_information;
    }

    if (equivalent_types(type_to_compare_1,
                type_to_compare_2))
    {
        return syntactic_comparison_of_nested_names(
                nested_name_1, nested_name_2, decl_context_1,
                unqualified_part_1, unqualified_part_2, decl_context_2);
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Dependent entry is already different\n");
        }
        return 0;
    }

    return 1;
}

char is_builtin_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE);
}

char is_fundamental_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE);
}

char is_non_derived_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT);
}

char is_integer_type(type_t* t)
{
    return is_integral_type(t);
}

char is_any_int_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT);
}

char is_any_unsigned_int_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && t->type->is_unsigned);
}

char is_any_signed_int_type(type_t* t)
{
    return (is_any_int_type(t) 
            && !is_any_unsigned_int_type(t));
}

char is_integral_type(type_t* t)
{
    return (is_any_int_type(t)
            || is_bool_type(t)
            || is_character_type(t)
            || is_wchar_t_type(t)
            // In C, enumerated types are integral types
            || (is_enumerated_type(t) && IS_C_LANGUAGE));
}

char is_signed_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            // The next one is silly but I wanted to express that
            // is_signed flag is of little utility in 'int'
            && (t->type->is_signed || !t->type->is_signed) 
            && !t->type->is_unsigned
            && !t->type->is_long
            && !t->type->is_short
            && !t->type->is_complex);
}

char is_unsigned_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && !t->type->is_signed
            && t->type->is_unsigned
            && !t->type->is_long
            && !t->type->is_short
            && !t->type->is_complex);
}

char is_signed_short_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && !t->type->is_unsigned
            && !t->type->is_long
            && t->type->is_short
            && !t->type->is_complex);
}

char is_unsigned_short_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && t->type->is_unsigned
            && !t->type->is_long
            && t->type->is_short
            && !t->type->is_complex);
}

char is_signed_long_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && !t->type->is_unsigned
            && (t->type->is_long == 1)
            && !t->type->is_short
            && !t->type->is_complex);
}

char is_unsigned_long_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && t->type->is_unsigned
            && (t->type->is_long == 1)
            && !t->type->is_short
            && !t->type->is_complex);
}

char is_signed_long_long_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && !t->type->is_unsigned
            && (t->type->is_long == 2)
            && !t->type->is_short
            && !t->type->is_complex);
}

char is_unsigned_long_long_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && t->type->is_unsigned
            && (t->type->is_long == 2)
            && !t->type->is_short
            && !t->type->is_complex);
}

char is_character_type(type_t* t)
{
    return is_signed_char_type(t) || is_unsigned_char_type(t);
}

char is_char_type(type_t* t)
{
    // FIXME: Make a flag to choose signed or unsigned chars
    return is_signed_char_type(t);
}

char is_wchar_t_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_WCHAR);
}

char is_signed_char_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_CHAR
            && !t->type->is_unsigned);
}

char is_unsigned_char_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_CHAR
            && t->type->is_unsigned);
}

char is_pointer_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_POINTER);
}

char is_function_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_FUNCTION);
}

type_t* function_type_get_return_type(type_t* t)
{
    ERROR_CONDITION(!is_function_type(t), "This is not a function type", 0);
    t = advance_over_typedefs(t);

    return t->function->return_type;
}

// Can be used both for pointers and pointers to members
type_t* pointer_type_get_pointee_type(type_t *t)
{
    ERROR_CONDITION(!is_pointer_type(t)
            && !is_pointer_to_member_type(t), "This is not a pointer/pointer to member type", 0);
    t = advance_over_typedefs(t);

    return t->pointer->pointee;
}

scope_entry_t* pointer_to_member_type_get_class(type_t *t)
{
    ERROR_CONDITION(!is_pointer_to_member_type(t), "This is not a pointer to member type", 0);
    t = advance_over_typedefs(t);

    return t->pointer->pointee_class;
}

type_t* pointer_to_member_type_get_class_type(type_t *t)
{
    ERROR_CONDITION(!is_pointer_to_member_type(t), "This is not a pointer to member type", 0);
    scope_entry_t* entry = pointer_to_member_type_get_class(t);

    return get_user_defined_type(entry);
}

type_t* array_type_get_element_type(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->element_type;
}

AST array_type_get_array_size_expr(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->array_expr;
}

decl_context_t array_type_get_array_size_expr_context(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->array_expr_decl_context;
}

char is_array_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL && t->kind == TK_ARRAY);
}

char is_pointer_to_class_type(type_t* t1)
{
    return (is_pointer_type(t1) 
            && is_class_type(pointer_type_get_pointee_type(t1)));
}

char is_pointer_to_function_type(type_t* t1)
{
    return (is_pointer_type(t1) 
            && is_function_type(pointer_type_get_pointee_type(t1)));
}

char is_lvalue_reference_to_class_type(type_t* t1)
{
    return (is_lvalue_reference_type(t1) 
            && is_class_type(reference_type_get_referenced_type(t1)));
}

char is_void_pointer_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_POINTER
            && is_void_type(t->pointer->pointee));
}

char is_void_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_VOID);
}

char is_pointer_to_member_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_POINTER_TO_MEMBER);
}

char is_enumerated_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && ( 
                (t->kind == TK_DIRECT 
                 && t->type->kind == STK_ENUM)
                || (is_named_type(t) 
                    && is_enumerated_type(
                        named_type_get_symbol(t)
                        ->type_information))
               )
           );
}

char is_named_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_USER_DEFINED
            && t->type->user_defined_type != NULL);
}

scope_entry_t* named_type_get_symbol(type_t* t)
{
    if (is_named_type(t))
    {
        return t->type->user_defined_type;
    }
    else
    {
        internal_error("This is not a named type\n", 0);
    }
}

char is_floating_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && (t->type->builtin_type == BT_FLOAT
                || t->type->builtin_type == BT_DOUBLE));
}

char is_arithmetic_type(type_t* t)
{
    return is_integral_type(t) || is_floating_type(t);
}

char is_int_or_floating_type(type_t* t)
{
    return is_any_int_type(t) || is_floating_type(t);
}

char is_double_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && is_floating_type(t) 
            && t->type->builtin_type == BT_DOUBLE
            && !t->type->is_long);
}

char is_long_double_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && is_floating_type(t) 
            && t->type->builtin_type == BT_DOUBLE
            && t->type->is_long);
}

char is_float_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && is_floating_type(t) 
            && t->type->builtin_type == BT_FLOAT);
}

char is_complex_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && is_non_derived_type(t)
            && t->type->is_complex);
}

type_t* reference_type_get_referenced_type(type_t* t1)
{
    ERROR_CONDITION(!is_lvalue_reference_type(t1)
            && !is_rvalue_reference_type(t1), 
            "This is not a reference type", 0);
    t1 = advance_over_typedefs(t1);

    return t1->pointer->pointee;
}

// For C, it does nothing
// For C++, it removes the reference type, returning the referenced type
type_t* no_ref(type_t* t)
{
    CXX_LANGUAGE()
    {
        if (is_lvalue_reference_type(t)
                || is_rvalue_reference_type(t))
            return reference_type_get_referenced_type(t);
    }
    return t;
}

char is_lvalue_reference_type(type_t* t1)
{
    t1 = advance_over_typedefs(t1);

    return (t1 != NULL
            && t1->kind == TK_LVALUE_REFERENCE);
}

char is_rvalue_reference_type(type_t* t1)
{
    t1 = advance_over_typedefs(t1);

    return (t1 != NULL
            && t1->kind == TK_RVALUE_REFERENCE);
}

decl_context_t enum_type_get_context(type_t* t)
{
    ERROR_CONDITION(!is_enumerated_type(t), "This is not an enumerated type", 0);
    t = advance_over_typedefs(t);
    if (is_named_type(t))
    {
        t = named_type_get_symbol(t)->type_information;
    }
    return t->type->type_decl_context;
}

decl_context_t class_type_get_context(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->type->type_decl_context;
}

char is_bool_type(type_t* t1)
{
    // Advance over typedefs
    t1 = advance_over_typedefs(t1);

    return (t1 != NULL 
            && t1->kind == TK_DIRECT
            && t1->type->kind == STK_BUILTIN_TYPE
            && t1->type->builtin_type == BT_BOOL);
}

char is_dependent_typename_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_TEMPLATE_DEPENDENT_TYPE);
}

type_t* get_actual_class_type(type_t* class_type)
{
    class_type = advance_over_typedefs(class_type);
    if (is_named_class_type(class_type))
    {
        return class_type->type->user_defined_type->type_information;
    }
    else if (is_unnamed_class_type(class_type))
    {
        return class_type;
    }
    else
    {
        internal_error("This is not a class type!", 0);
    }
}

char is_class_type(type_t* possible_class)
{
    return (is_named_class_type(possible_class) || is_unnamed_class_type(possible_class));
}

char is_unnamed_class_type(type_t* possible_class)
{
    possible_class = advance_over_typedefs(possible_class);
    return (possible_class != NULL
            && possible_class->kind == TK_DIRECT
            && possible_class->type->kind == STK_CLASS);
}

char is_named_class_type(type_t* possible_class)
{
    possible_class = advance_over_typedefs(possible_class);
    return (possible_class != NULL
            && possible_class->kind == TK_DIRECT
            && possible_class->type->kind == STK_USER_DEFINED
            && possible_class->type->user_defined_type != NULL
            && possible_class->type->user_defined_type->type_information != NULL
            && possible_class->type->user_defined_type->type_information->kind == TK_DIRECT
            && possible_class->type->user_defined_type->type_information->type->kind == STK_CLASS);
}


char class_type_is_base(type_t* possible_base, type_t* possible_derived)
{
    possible_base = advance_over_typedefs(possible_base);
    possible_derived = advance_over_typedefs(possible_derived);

    ERROR_CONDITION(!is_class_type(possible_base)
            || !is_class_type(possible_derived), 
            "This function expects class types", 0);

    if (is_named_class_type(possible_base))
    {
        possible_base = named_type_get_symbol(possible_base)->type_information;
    }
    if (is_named_class_type(possible_derived))
    {
        ERROR_CONDITION(is_named_type(possible_derived)
                && class_type_is_incomplete_independent(get_actual_class_type(possible_derived)),
                "Cannot test if a class type is derived of another if "
                "the potentially derived is independent incomplete\n", 0);
        possible_derived = named_type_get_symbol(possible_derived)->type_information;
    }

    // Search in bases of the derived
    int i;
    for (i = 0; i < class_type_get_num_bases(possible_derived); i++)
    {
        char is_virtual = 0;
        type_t* current_base = class_type_get_base_num(possible_derived, i, &is_virtual)
            ->type_information;

        if (current_base == possible_base)
            return 1;
    }

    // Recursively search in bases of the derived
    for (i = 0; i < class_type_get_num_bases(possible_derived); i++)
    {
        char is_virtual = 0;
        type_t* current_base = class_type_get_base_num(possible_derived, i, &is_virtual)
            ->type_information;

        if (class_type_is_base(possible_base, current_base))
            return 1;
    }

    // Not found
    return 0;
}

char class_type_is_derived(type_t* possible_derived, type_t* possible_base)
{
    return class_type_is_base(possible_base, possible_derived);
}

char is_pointer_to_void_type(type_t* t)
{
    return (is_pointer_type(t)
            && is_void_type(pointer_type_get_pointee_type(t)));
}

char pointer_to_class_type_is_base(type_t* possible_pclass_base,
        type_t* possible_pclass_derived)
{
    ERROR_CONDITION(!is_pointer_to_class_type(possible_pclass_base)
            || !is_pointer_to_class_type(possible_pclass_derived),
            "Both thypes must be pointer to class", 0);

    type_t* possible_base = pointer_type_get_pointee_type(possible_pclass_base);
    type_t* possible_derived = pointer_type_get_pointee_type(possible_pclass_derived);

    return class_type_is_base(possible_base, possible_derived);
}

char pointer_to_class_type_is_derived(type_t* possible_pclass_derived,
        type_t* possible_pclass_base)
{
    return pointer_to_class_type_is_base(possible_pclass_base, possible_pclass_derived);
}

cv_qualifier_t get_cv_qualifier(type_t* type_info)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);
    return type_info->cv_qualifier;
}

static char template_id_is_dependent(AST expression, AST template_id, decl_context_t decl_context)
{
    AST template_argument_list = ASTSon1(template_id);

    if (template_argument_list != NULL)
    {
        AST list, iter;
        list = template_argument_list;

        for_each_element(list, iter)
        {
            AST template_argument = ASTSon1(iter);

            switch (ASTType(template_argument))
            {
                case AST_TEMPLATE_EXPRESSION_ARGUMENT : 
                    {
                        AST template_argument_expression = ASTSon1(template_argument);
                        if (is_dependent_expression(template_argument_expression, decl_context))
                        {
                            ast_set_expression_type(expression, get_dependent_expr_type());
                            return 1;
                        }
                        break;
                    }
                case AST_TEMPLATE_TYPE_ARGUMENT :
                    {
                        AST type_id = ASTSon0(template_argument);

                        AST type_specifier = ASTSon0(type_id);

                        gather_decl_spec_t gather_info;
                        memset(&gather_info, 0, sizeof(gather_info));

                        type_t* simple_type_info = NULL;
                        // Fix this
                        build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info, 
                                decl_context);
                        if (is_dependent_type(simple_type_info, decl_context))
                        {
                            ast_set_expression_type(expression, get_dependent_expr_type());
                            return 1;
                        }

                        break;
                    }
                default:
                    break;
            }
        }
    }

    return 0;
}

char is_dependent_expression(AST expression, decl_context_t decl_context)
{
    ERROR_CONDITION(expression == NULL, "This cannot be null", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "Checking whether '%s' expression is dependent\n", 
                prettyprint_in_buffer(expression));
    }

    if (ASTExprType(expression) != NULL)
    {
        if (is_dependent_expr_type(ASTExprType(expression)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Expression '%s' was already marked as dependent\n",
                        prettyprint_in_buffer(expression));
            }
            return 1;
        }
    }

    switch (ASTType(expression))
    {
        case AST_EXPRESSION : 
        case AST_INITIALIZER :
        case AST_INITIALIZER_EXPR :
        case AST_CONSTANT_INITIALIZER : 
        case AST_CONSTANT_EXPRESSION : 
        case AST_PARENTHESIZED_EXPRESSION :
            {
                return is_dependent_expression(ASTSon0(expression), decl_context);
            }
        case AST_INITIALIZER_BRACES :
            {
                AST initializer_list = ASTSon0(expression);
                AST iter;

                for_each_element(initializer_list, iter)
                {
                    AST initializer = ASTSon1(iter);

                    if (is_dependent_expression(initializer, decl_context))
                    {
                        ast_set_expression_type(expression, get_dependent_expr_type());
                        return 1;
                    }
                }
                return 0;
            }
        case AST_DESIGNATED_INITIALIZER :
            {
                // [1][2] = 3
                // a.b = 4
                // AST designation = ASTSon0(expression);
                // AST initializer_clause = ASTSon1(expression);

                internal_error("Yet to implement", 0);
                return 0;
                break;
            }
        case AST_DESIGNATION : 
            {
                // [1][2] {= 3}
                // a.b {= 3}
                AST designator_list = ASTSon0(expression);
                AST iter;

                for_each_element(designator_list, iter)
                {
                    AST designator = ASTSon1(iter);

                    if (is_dependent_expression(designator, decl_context))
                    {
                        ast_set_expression_type(expression, get_dependent_expr_type());
                        return 1;
                    }
                }

                return 0;
                break;
            }
        case AST_INDEX_DESIGNATOR :
            {
                // [1]{[2] = 3}
                return is_dependent_expression(ASTSon0(expression), decl_context);
            }
        case AST_FIELD_DESIGNATOR :
            {
                // a{.b = 3}
                return 0;
            }
            // Primaries
        case AST_DECIMAL_LITERAL :
        case AST_OCTAL_LITERAL :
        case AST_HEXADECIMAL_LITERAL :
        case AST_FLOATING_LITERAL :
        case AST_BOOLEAN_LITERAL :
        case AST_CHARACTER_LITERAL :
        case AST_STRING_LITERAL :
            {
                return 0;
            }
            // FIXME : 'this' depends exclusively on the current context
        case AST_THIS_VARIABLE :
            {
                internal_error("Yet to implement", 0);
                return 0;
            }
        case AST_TEMPLATE_ID :
            {
                // Template functions can be explicitly selected with template_id
                // that might be dependent
                if (template_id_is_dependent(expression, expression, decl_context))
                {
                    return 1;
                }
                return 0;
                break;
            }
        case AST_SYMBOL :
        case AST_QUALIFIED_ID :
        case AST_QUALIFIED_TEMPLATE :
            {
                scope_entry_list_t* entry_list = 
                    query_id_expression_flags(decl_context, expression, DF_DEPENDENT_TYPENAME);

                if (entry_list == NULL)
                {
                    internal_error("Symbol '%s' in '%s' not found\n", prettyprint_in_buffer(expression),
                            ast_location(expression));
                }

                if (entry_list->entry->kind == SK_DEPENDENT_ENTITY)
                {
                    return 1;
                }

                // Check for additional template-id's
                if ((ASTType(expression) == AST_QUALIFIED_ID
                        || ASTType(expression) == AST_QUALIFIED_TEMPLATE)
                        && ASTType(ASTSon2(expression)) == AST_TEMPLATE_ID)
                {
                    if (template_id_is_dependent(expression, ASTSon2(expression), decl_context))
                        return 1;

                    // No need to check anything else.
                    //
                    // A::f<int> can't be dependent because if 'A' was dependent we would
                    // get a SK_DEPENDENT_ENTITY. Likewise for 'A<T>::f<int>'
                    return 0;
                }

                scope_entry_t* entry = entry_list->entry;

                if(entry->dependency_info == DI_UNKNOWN)
                {
                    // Maybe this is a const-variable initialized with a dependent expression
                    char result = 0;
                    // We already checked SK_DEPENDENT_ENTITY before
                    if (entry->kind == SK_TEMPLATE_PARAMETER)
                    {
                        result = 1;
                    }
                    else if ((entry->kind == SK_VARIABLE
                                || entry->kind == SK_ENUMERATOR))
                    {
                        if (entry->expression_value != NULL)
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "Computing initialization dependency of expression '%s'\n", 
                                        prettyprint_in_buffer(entry->expression_value));
                            }
                            entry->dependency_info = DI_BUSY;
                            result |= is_dependent_expression(entry->expression_value, entry->decl_context);
                        }
                    }
                    else if (entry->kind == SK_TEMPLATE)
                    {
                        // This can reach here because of 'A::f' and
                        // f is a template member function of 'A', so by itself
                        // it is not dependent
                        result = 0;
                    }

                    if (entry->type_information != NULL)
                    {
                        result |= is_dependent_type(entry->type_information, decl_context);
                    }

                    entry->dependency_info = (result ? DI_DEPENDENT : DI_NOT_DEPENDENT);
                }

                if (entry->dependency_info == DI_DEPENDENT)
                {
                    ast_set_expression_type(expression, get_dependent_expr_type());
                }
                return (entry->dependency_info == DI_DEPENDENT);
            }
            // Postfix expressions
        case AST_ARRAY_SUBSCRIPT :
            {
                return is_dependent_expression(ASTSon0(expression), decl_context)
                    || is_dependent_expression(ASTSon1(expression), decl_context);
            }
        case AST_FUNCTION_CALL :
            {
                char invoked_dependent = is_dependent_expression(ASTSon0(expression), decl_context);

                if (invoked_dependent)
                    return 1;

                AST expression_list = ASTSon1(expression);

                if (expression_list != NULL)
                {
                    AST iter;
                    for_each_element(expression_list, iter)
                    {
                        AST current_expression = ASTSon1(iter);

                        if (is_dependent_expression(current_expression, decl_context))
                        {
                            ast_set_expression_type(expression, get_dependent_expr_type());
                            return 1;
                        }
                    }
                }

                return 0;
            }
        case AST_EXPLICIT_TYPE_CONVERSION :
            {
                AST type_specifier = ast_copy(ASTSon0(expression));

                // Create a full-fledged type_specifier_seq
                AST type_specifier_seq = ASTMake3(AST_TYPE_SPECIFIER_SEQ, NULL, 
                        type_specifier, NULL, ASTLine(type_specifier), NULL);

                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));

                type_t* simple_type_info = NULL;

                // Fix this
                build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &simple_type_info, 
                        decl_context);

                if (is_dependent_type(simple_type_info, decl_context))
                {
                    ast_set_expression_type(expression, get_dependent_expr_type());
                    return 1;
                }

                AST expression_list = ASTSon1(expression);

                if (expression_list != NULL)
                {
                    AST iter;
                    for_each_element(expression_list, iter)
                    {
                        AST current_expression = ASTSon1(iter);

                        if (is_dependent_expression(current_expression, decl_context))
                        {
                            ast_set_expression_type(expression, get_dependent_expr_type());
                            return 1;
                        }
                    }
                }

                return 0;
            }
        case AST_TYPENAME_EXPLICIT_TYPE_CONVERSION :
            {
                internal_error("Yet to implement", 0);
                return 1;
            }
        case AST_TYPENAME_TEMPLATE_EXPLICIT_TYPE_CONVERSION :
        case AST_TYPENAME_TEMPLATE_TEMPLATE_EXPLICIT_TYPE_CONVERSION :
            {
                internal_error("Yet to implement", 0);
                return 1;
            }
        case AST_SIZEOF :
            {
                return is_dependent_expression(ASTSon0(expression), decl_context);
            }
        case AST_SIZEOF_TYPEID :
            {
                AST type_id = ASTSon0(expression);

                AST type_specifier = ASTSon0(type_id);
                AST abstract_declarator = ASTSon1(type_id);

                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));

                type_t* simple_type_info = NULL;
                // Fix this
                build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info, 
                        decl_context);

                type_t* declarator_type = NULL;
                compute_declarator_type(abstract_declarator, &gather_info, simple_type_info, 
                        &declarator_type, decl_context);

                return is_dependent_type(simple_type_info, decl_context);
            }
        case AST_DERREFERENCE :
        case AST_REFERENCE :
        case AST_PLUS_OP :
        case AST_NEG_OP :
        case AST_NOT_OP :
        case AST_COMPLEMENT_OP :
            {
                return is_dependent_expression(ASTSon0(expression), decl_context);
            }
            // Cast expression
        case AST_CAST_EXPRESSION :
            // They share the same tree layout
        case AST_STATIC_CAST :
        case AST_DYNAMIC_CAST :
        case AST_REINTERPRET_CAST :
        case AST_CONST_CAST :
            {
                AST type_id = ASTSon0(expression);

                AST type_specifier = ASTSon0(type_id);
                AST abstract_declarator = ASTSon1(type_id);

                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));

                type_t* simple_type_info = NULL;
                // Fix this
                build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info, 
                        decl_context);

                type_t* declarator_type = NULL;
                compute_declarator_type(abstract_declarator, &gather_info, simple_type_info, 
                        &declarator_type, decl_context);

                if (is_dependent_type(simple_type_info, decl_context))
                {
                    ast_set_expression_type(expression, get_dependent_expr_type());
                    return 1;
                }
                else
                {
                    return is_dependent_expression(ASTSon1(expression), decl_context);
                }
            }
        case AST_MULT_OP :
        case AST_DIV_OP :
        case AST_MOD_OP :
        case AST_ADD_OP :
        case AST_MINUS_OP :
        case AST_SHL_OP :
        case AST_SHR_OP :
        case AST_LOWER_THAN :
        case AST_GREATER_THAN :
        case AST_GREATER_OR_EQUAL_THAN :
        case AST_LOWER_OR_EQUAL_THAN :
        case AST_EQUAL_OP :
        case AST_DIFFERENT_OP :
        case AST_BITWISE_AND :
        case AST_BITWISE_XOR :
        case AST_BITWISE_OR :
        case AST_LOGICAL_AND :
        case AST_LOGICAL_OR :
            {
                return is_dependent_expression(ASTSon0(expression), decl_context)
                    || is_dependent_expression(ASTSon1(expression), decl_context);
            }
        case AST_CONDITIONAL_EXPRESSION :
            {
                return is_dependent_expression(ASTSon0(expression), decl_context)
                    || is_dependent_expression(ASTSon1(expression), decl_context)
                    || is_dependent_expression(ASTSon2(expression), decl_context);
            }
        default :
            {
                internal_error("Unexpected node '%s' %s", ast_print_node_type(ASTType(expression)), 
                        ast_location(expression));
                break;
            }
            return 0;
    }
}

char is_dependent_simple_type(type_t* type_info, decl_context_t decl_context)
{
    if (type_info == NULL)
        return 0;

    ERROR_CONDITION(!is_non_derived_type(type_info), "This function expects a direct type", 0);

    if (is_dependent_typename_type(type_info))
    {
        return 1;
    }
    else if (is_typedef_type(type_info))
    {
        return is_dependent_type(typedef_type_get_aliased_type(type_info), decl_context);
    }
    else if (is_named_type(type_info))
    {
        scope_entry_t* symbol = named_type_get_symbol(type_info);

        if (symbol->dependency_info == DI_UNKNOWN)
        {
            // We have to compute it
            symbol->dependency_info = DI_BUSY;

            char result = 0;

            if (symbol->kind == SK_TEMPLATE_PARAMETER
                    || symbol->kind == SK_TEMPLATE_TYPE_PARAMETER
                    || symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                result = 1;
            }
            else
            {
                ERROR_CONDITION(symbol->type_information == NULL, "This cannot be null", 0);
                result = is_dependent_type(symbol->type_information, decl_context);
            }

            // If the symbol type is member, we have to check the class
            // where it belongs
            if (!result && symbol->entity_specs.is_member)
            {
                result |= is_dependent_type(symbol->entity_specs.class_type, decl_context);
            }

            symbol->dependency_info = (result ? DI_DEPENDENT : DI_NOT_DEPENDENT);
        }

        return (symbol->dependency_info == DI_DEPENDENT);
    }
    else if (is_enumerated_type(type_info))
    {
        // FIXME This, there are no functions to access an enum type 
        enum_info_t* enum_info = type_info->type->enum_info;

        int i;
        for (i = 0; i < enum_info->num_enumeration; i++)
        {
            scope_entry_t* entry = enum_info->enumeration_list[i];

            if (entry->expression_value != NULL
                    && entry->dependency_info == DI_UNKNOWN)
            {
                if (is_dependent_expression(entry->expression_value, entry->decl_context))
                {
                    return 1;
                }
            }
            else
            {
                return (entry->dependency_info == DI_DEPENDENT);
            }
        }

        return 0;
    }
    else if (is_unnamed_class_type(type_info))
    {
        return class_type_get_is_dependent(type_info);
    }
    else if (is_builtin_type(type_info))
    {
        return 0;
    }
    else if (is_gcc_builtin_va_list(type_info))
    {
        return 0;
    }
    else
    {
        internal_error("Invalid simple type", 0);
    }
}

char is_dependent_type(type_t* type, decl_context_t decl_context)
{
    ERROR_CONDITION(type == NULL, "This cannot be null", 0);

    type = advance_over_typedefs(type);

    if (is_non_derived_type(type))
    {
        return is_dependent_simple_type(type, decl_context);
    }
    else if (is_array_type(type))
    {
        return is_dependent_type(array_type_get_element_type(type), decl_context)
            || ((array_type_get_array_size_expr(type) != NULL)
                    && is_dependent_expression(array_type_get_array_size_expr(type),
                        array_type_get_array_size_expr_context(type)));
    }
    else if (is_function_type(type))
    {
        type_t* return_type = function_type_get_return_type(type);

        if (return_type != NULL
                && is_dependent_type(return_type, decl_context))
        {
            return 1;
        }

        int i;
        int num_parameters = function_type_get_num_parameters(type);
        if (function_type_get_has_ellipsis(type))
            num_parameters--;

        for (i = 0; i < num_parameters; i++)
        {
            type_t* parameter_type = function_type_get_parameter_type_num(type, i);

            if (is_dependent_type(parameter_type, decl_context))
            {
                return 1;
            }
        }

        return 0;
    }
    else if (is_vector_type(type))
    {
        return is_dependent_type(vector_type_get_element_type(type), decl_context);
    }
    else if (is_pointer_type(type))
    {
        return is_dependent_type(pointer_type_get_pointee_type(type), decl_context);
    }
    else if (is_lvalue_reference_type(type) 
            || is_rvalue_reference_type(type))
    {
        return is_dependent_type(reference_type_get_referenced_type(type), decl_context);
    }
    else if (is_pointer_to_member_type(type))
    {
        return is_dependent_type(pointer_type_get_pointee_type(type), decl_context)
            || is_dependent_type(pointer_to_member_type_get_class_type(type), decl_context);
    }
    else if (is_vector_type(type))
    {
        return is_dependent_type(vector_type_get_element_type(type), decl_context);
    }
    // else if (is_unresolved_overloaded_type(type))
    // {
    //     // This should not be dependent
    //     return 0;
    // }
    else
    {
        internal_error("Unknown type kind %d\n", type->kind);
    }
}

// This jumps over user defined types and typedefs
scope_entry_t* give_real_entry(scope_entry_t* entry)
{
    scope_entry_t* result = entry;

    type_t* t = entry->type_information;

    if (t != NULL)
    {
        t = advance_over_typedefs(t);
    }

    while (t != NULL 
            && t->kind == TK_DIRECT
            && t->type->kind == STK_USER_DEFINED)
    {
        result = t->type->user_defined_type;
        t = result->type_information;
        if (t != NULL)
        {
            t = advance_over_typedefs(t);
        }
    }

    if (result->entity_specs.is_injected_class_name)
    {
        result = result->entity_specs.injected_class_referred_symbol;
    }

    return result;
}

static const char* get_cv_qualifier_string(type_t* type_info)
{
    const char* result = "";

    if (BITMAP_TEST(type_info->cv_qualifier, CV_CONST))
    {
        result = strappend(result, "const ");
    }

    if (BITMAP_TEST(type_info->cv_qualifier, CV_VOLATILE))
    {
        result = strappend(result, "volatile ");
    }

    if (BITMAP_TEST(type_info->cv_qualifier, CV_RESTRICT))
    {
        // Be conservative for now 
        // C_LANGUAGE()
        // {
        //     result = strappend(result, "restrict ");
        // }
        // CXX_LANGUAGE()
        {
            result = strappend(result, "__restrict ");
        }
    }

    return result;
}


// States if a declarator of this type will need parentheses
static char declarator_needs_parentheses(type_t* type_info)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);

    char result = 0;
    if (type_info->kind == TK_POINTER_TO_MEMBER
            || type_info->kind == TK_POINTER
            || type_info->kind == TK_LVALUE_REFERENCE
            || type_info->kind == TK_RVALUE_REFERENCE)
    {
        type_t* pointee = type_info->pointer->pointee;
        result = (pointee->kind != TK_POINTER_TO_MEMBER
                && pointee->kind != TK_POINTER
                && pointee->kind != TK_LVALUE_REFERENCE
                && pointee->kind != TK_RVALUE_REFERENCE
                && pointee->kind != TK_DIRECT);
    }

    return result;
}

// Gives a string with the name of this simple type
static const char* get_simple_type_name_string_internal(decl_context_t decl_context, simple_type_t* simple_type)
{
    ERROR_CONDITION(simple_type == NULL, "This cannot be null", 0);

    const char* result = "";
    switch ((int)simple_type->kind)
    {
        case STK_USER_DEFINED :
            {
                // Fix this
                scope_entry_t* entry = simple_type->user_defined_type;

                char is_dependent = 0;
                int max_level = 0;
                result = get_fully_qualified_symbol_name(entry,
                        decl_context, &is_dependent, &max_level);

                // If is a dependent name and it is qualified then it can be
                // given a "typename" keyword (in some cases one must do that)
                if (is_dependent && max_level > 0)
                {
                    result = strappend("typename ", result);
                }
                break;
            }
        case STK_TYPEOF :
            {
                result = "__typeof_not_supported_yet__";
                break;
            }
        case STK_VA_LIST :
            {
                result = "__builtin_va_list";
                break;
            }
        case STK_BUILTIN_TYPE :
            {
                if (simple_type->is_unsigned)
                {
                    result = "unsigned ";
                }
                else if (simple_type->is_signed)
                {
                    result = "signed ";
                }

                if (simple_type->is_complex)
                {
                    result = strappend(result, "_Complex ");
                }

                if (simple_type->is_long == 1)
                {
                    result = strappend(result, "long ");
                }
                else if (simple_type->is_long >= 2)
                {
                    result = strappend(result, "long long ");
                }
                else if (simple_type->is_short)
                {
                    result = strappend(result, "short ");
                }

                switch ((int)simple_type->builtin_type)
                {
                    case BT_INT :
                        {
                            result = strappend(result, "int");
                            break;
                        }
                    case BT_CHAR :
                        {
                            result = strappend(result, "char");
                            break;
                        }
                    case BT_WCHAR :
                        {
                            result = strappend(result, "wchar_t");
                            break;
                        }
                    case BT_FLOAT :
                        {
                            result = strappend(result, "float");
                            break;
                        }
                    case BT_DOUBLE :
                        {
                            result = strappend(result, "double");
                            break;
                        }
                    case BT_BOOL :
                        {
                            CXX_LANGUAGE()
                            {
                                result = strappend(result, "bool");
                            }
                            C_LANGUAGE()
                            {
                                result = strappend(result, "_Bool");
                            }
                            break;
                        }
                    case BT_VOID :
                        {
                            result = strappend(result, "void");
                            break;
                        }
                    case BT_UNKNOWN :
                        {
                            result = strappend(result, " ");
                            break;
                        }
                    default :
                        break;
                }
                break;
            }
        case STK_CLASS :
            {
                internal_error("Type STK_CLASS invalid\n", 0);
                break;
            }
        case STK_TEMPLATE_DEPENDENT_TYPE :
            {
                result = prettyprint_in_buffer(simple_type->typeof_expr);
                break;
            }
        default:
            {
                internal_error("Unknown simple type kind '%d'\n", simple_type->kind);
                break;
            }
    }

    return result;
}

// Gives the simple type name of a full fledged type
const char* get_simple_type_name_string(decl_context_t decl_context, type_t* type_info)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);

    const char* result = "";
    switch ((int)(type_info->kind))
    {
        case TK_DIRECT :
            {
                result = get_cv_qualifier_string(type_info);
                result = strappend(result, get_simple_type_name_string_internal(decl_context, type_info->type));
                return result;
                break;
            }
        case TK_FUNCTION :
            {
                result = get_simple_type_name_string(decl_context, type_info->function->return_type);
                break;
            }
        case TK_POINTER :
        case TK_LVALUE_REFERENCE :
        case TK_RVALUE_REFERENCE :
        case TK_POINTER_TO_MEMBER :
            {
                result = get_simple_type_name_string(decl_context, type_info->pointer->pointee);
                break;
            }
        case TK_ARRAY :
            {
                result = get_simple_type_name_string(decl_context, type_info->array->element_type);
                break;
            }
        default:
            break;
    }
    return result;
}

static const char* get_type_name_string(decl_context_t decl_context,
        type_t* type_info, 
        const char* symbol_name,
        int* num_parameter_names,
        const char*** parameter_names,
        char is_parameter);

// Returns a declaration string given a type, a symbol name, an optional initializer
// and a semicolon
const char* get_declaration_string_internal(type_t* type_info, 
        decl_context_t decl_context,
        const char* symbol_name, const char* initializer, 
        char semicolon,
        int* num_parameter_names,
        const char*** parameter_names,
        char is_parameter)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);

    const char* base_type_name = get_simple_type_name_string(decl_context, type_info);
    const char* declarator_name = get_type_name_string(decl_context, type_info, symbol_name, 
            num_parameter_names, parameter_names, is_parameter);

    const char* result;

    result = base_type_name;
    if (strcmp(declarator_name, "") != 0)
    {
        result = strappend(result, " ");
        result = strappend(result, declarator_name);
    }

    // FIXME Should check if copy-constructor is not flagged as "explicit"
    // (for parameters this can be useful to declare default arguments)
    if (strcmp(initializer, "") != 0)
    {
        result = strappend(result, " = ");
        result = strappend(result, initializer);
    }

    if (semicolon)
    {
        result = strappend(result, ";");
    }

    return result;
}

static void get_type_name_str_internal(decl_context_t decl_context,
        type_t* type_info, 
        const char** left,
        const char** right,
        int* num_parameter_names,
        const char*** parameter_names,
        char is_parameter);

static const char* get_type_name_string(decl_context_t decl_context,
        type_t* type_info, 
        const char* symbol_name,
        int* num_parameter_names,
        const char*** parameter_names,
        char is_parameter)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);

    const char* left = "";
    const char* right = "";
    get_type_name_str_internal(decl_context, type_info, &left, &right, 
            num_parameter_names, parameter_names, is_parameter);

    const char* result = strappend(left, symbol_name);
    result = strappend(result, right);

    return result;
}

char is_const_qualified_type(type_t* t1)
{
    cv_qualifier_t cv1 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(t1, &cv1);

    return ((cv1 & CV_CONST) == CV_CONST);
}

char is_volatile_qualified_type(type_t* t1)
{
    cv_qualifier_t cv1 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(t1, &cv1);

    return ((cv1 & CV_VOLATILE) == CV_VOLATILE);
}

char is_restrict_qualified_type(type_t* t1)
{
    cv_qualifier_t cv1 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(t1, &cv1);

    return ((cv1 & CV_RESTRICT) == CV_RESTRICT);
}

char is_const_qualified(cv_qualifier_t cv)
{
    return ((cv & CV_CONST) == CV_CONST);
}

char is_volatile_qualified(cv_qualifier_t cv)
{
    return ((cv & CV_VOLATILE) == CV_VOLATILE);
}

char is_restrict_qualified(cv_qualifier_t cv)
{
    return ((cv & CV_RESTRICT) == CV_RESTRICT);
}

char is_less_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
    // Let's ignore __restrict for now

    if ((cv1 == CV_NONE)
            && (cv2 != CV_NONE))
        return 1;

    if (cv1 != cv2)
    {
        cv_qualifier_t cv_qualifiers[] =
        {
            CV_CONST,
            CV_VOLATILE,
            CV_RESTRICT,
            CV_CONST | CV_VOLATILE,
            CV_CONST | CV_RESTRICT,
            CV_VOLATILE | CV_RESTRICT,
            CV_CONST | CV_VOLATILE | CV_RESTRICT
        };

        unsigned int i;
        for (i = 0; i < STATIC_ARRAY_LENGTH(cv_qualifiers); i++)
        {
            if ((cv1 == cv_qualifiers[i])
                    && ((cv2 & cv_qualifiers[i]) == cv_qualifiers[i]))
                return 1;
        }
    }
    return 0;
}

char is_equal_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
    return (cv1 == cv2);
}

char is_less_or_equal_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
    return is_less_cv_qualified(cv1, cv2)
        || is_equal_cv_qualified(cv1, cv2);
}

char is_more_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
    return !is_less_or_equal_cv_qualified(cv1, cv2);
}

char is_more_or_equal_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
    return !is_less_cv_qualified(cv1, cv2);
}

char is_less_cv_qualified_type(type_t* t1, type_t* t2)
{
    cv_qualifier_t cv1 = CV_NONE;
    cv_qualifier_t cv2 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(t1, &cv1);
    advance_over_typedefs_with_cv_qualif(t2, &cv2);

    return is_less_cv_qualified(cv1, cv2);
}

char is_equally_cv_qualified_type(type_t* t1, type_t* t2)
{
    cv_qualifier_t cv1 = CV_NONE;
    cv_qualifier_t cv2 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(t1, &cv1);
    advance_over_typedefs_with_cv_qualif(t2, &cv2);

    return is_equal_cv_qualified(cv1, cv2);
}

char is_less_or_equal_cv_qualified_type(type_t* t1, type_t* t2)
{
    return (is_less_cv_qualified_type(t1, t2)
            || is_equally_cv_qualified_type(t1, t2));
}

char is_more_cv_qualified_type(type_t* t1, type_t* t2)
{
    return !is_less_or_equal_cv_qualified_type(t1, t2);
}

char is_more_or_equal_cv_qualified_type(type_t* t1, type_t* t2)
{
    return !is_less_cv_qualified_type(t1, t2);
}


// Constructs a proper declarator
static void get_type_name_str_internal(decl_context_t decl_context,
        type_t* type_info, 
        const char** left,
        const char** right,
        int* num_parameter_names,
        const char*** parameter_names,
        char is_parameter)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);

    switch (type_info->kind)
    {
        case TK_DIRECT :
            {
                break;
            }
        case TK_POINTER :
            {
                get_type_name_str_internal(decl_context, type_info->pointer->pointee, left, right, 
                        num_parameter_names, parameter_names, is_parameter);

                // Should this change, change the case for TK_ARRAY and "is_parameter == 1"
                if (declarator_needs_parentheses(type_info))
                {
                    (*left) = strappend((*left), "(");
                }

                (*left) = strappend((*left), "*");
                (*left) = strappend((*left), get_cv_qualifier_string(type_info));

                if (declarator_needs_parentheses(type_info))
                {
                    (*right) = strappend(")", (*right));
                }
                break;
            }
        case TK_POINTER_TO_MEMBER :
            {
                get_type_name_str_internal(decl_context, type_info->pointer->pointee, left, right, 
                        num_parameter_names,
                        parameter_names, is_parameter);

                if (declarator_needs_parentheses(type_info))
                {
                    (*left) = strappend((*left), "(");
                }

                (*left) = strappend((*left), type_info->pointer->pointee_class->symbol_name);

                (*left) = strappend((*left), "::");
                (*left) = strappend((*left), "*");
                (*left) = strappend((*left), get_cv_qualifier_string(type_info));


                if (declarator_needs_parentheses(type_info))
                {
                    (*right) = strappend(")", (*right));
                }
                break;
            }
        case TK_RVALUE_REFERENCE :
        case TK_LVALUE_REFERENCE :
            {
                get_type_name_str_internal(decl_context, type_info->pointer->pointee, left, right, 
                        num_parameter_names, parameter_names, is_parameter);

                if (declarator_needs_parentheses(type_info))
                {
                    (*left) = strappend((*left), "(");
                }

                if (type_info->kind == TK_LVALUE_REFERENCE)
                {
                    (*left) = strappend((*left), "&");
                }
                else
                {
                    (*left) = strappend((*left), "&&");
                }

                if (declarator_needs_parentheses(type_info))
                {
                    (*right) = strappend(")", (*right));
                }
                break;
            }
        case TK_ARRAY :
            {
                if (is_parameter
                        && (type_info->array->array_expr == NULL))
                {
                    // Get rid of those annoying unbounded arrays
                    // in parameters
                    //
                    // This is not valid, but works most of the time...
                    const char* array_expr = uniquestr("[0]");

                    (*right) = strappend((*right), array_expr);

                    get_type_name_str_internal(decl_context, type_info->array->element_type, left, right, 
                            num_parameter_names, parameter_names, is_parameter);
                }
                else
                {
                    const char* array_expr = strappend("[", prettyprint_in_buffer(type_info->array->array_expr));
                    array_expr = strappend(array_expr, "]");

                    (*right) = strappend((*right), array_expr);

                    get_type_name_str_internal(decl_context, type_info->array->element_type, left, right, 
                            num_parameter_names, parameter_names, is_parameter);
                }
                break;
            }
        case TK_FUNCTION :
            {
                get_type_name_str_internal(decl_context, type_info->function->return_type, left, right, 
                        num_parameter_names, parameter_names, is_parameter);

                const char* prototype;
                prototype = "(";
                int i;
                for (i = 0; i < type_info->function->num_parameters; i++)
                {
                    if (i > 0)
                    {
                        prototype = strappend(prototype, ", ");
                    }

                    if (type_info->function->parameter_list[i]->is_ellipsis)
                    {
                        prototype = strappend(prototype, "...");
                    }
                    else
                    {
                        if (parameter_names == NULL)
                        {
                            // Abstract declarator
                            prototype = strappend(prototype,
                                    get_declaration_string_internal(type_info->function->parameter_list[i]->type_info, decl_context, 
                                        "", "", 0, NULL, NULL, 1));
                        }
                        else
                        {
                            // We create a name
                            char parameter_name[20];
                            snprintf(parameter_name, 19, "_p_%d", i);
                            parameter_name[19] = '\0';

                            P_LIST_ADD((*parameter_names), (*num_parameter_names), uniquestr(parameter_name));

                            prototype = strappend(prototype,
                                    get_declaration_string_internal(type_info->function->parameter_list[i]->type_info, decl_context, 
                                        parameter_name, "", 0, NULL, NULL, 1));
                        }
                    }
                }
                // For C we might need to explicitly add 'void'
                C_LANGUAGE()
                {
                    if (type_info->function->num_parameters == 0
                            && !type_info->function->lacks_prototype)
                    {
                        prototype = strappend(prototype, "void");
                    }
                }
                prototype = strappend(prototype, ") ");
                prototype = strappend(prototype, get_cv_qualifier_string(type_info));

                (*right) = strappend((*right), prototype);
                break;
            }
        case TK_VECTOR :
            {
                char c[256];

                get_type_name_str_internal(decl_context, type_info->vector->element_type, left, right, 
                        num_parameter_names, parameter_names, is_parameter);

                snprintf(c, 255, "__attribute__((vector_size(%d)))", 
                        type_info->vector->vector_size);
                c[255] = '\0';

                (*right) = strappend((*right), c);
                break;
            }
        default:
            {
                fprintf(stderr, "Unknown type kind '%d'\n", (int)type_info->kind);
                break;
            }
    }
}

/** 
 * Debugging functions
 * **/

static
const char *get_named_simple_type_name(scope_entry_t* user_defined_type)
{
    ERROR_CONDITION(user_defined_type == NULL, "This cannot be null", 0);

    const char* result = uniquestr("");

    const int MAX_LENGTH = 1023;
    char* user_defined_str = counted_calloc(MAX_LENGTH + 1, sizeof(char), &_bytes_due_to_type_system);

    switch (user_defined_type->kind)
    {
        case SK_ENUM :
            {
                int max_level = 0;
                char is_dependent = 0;
                snprintf(user_defined_str, MAX_LENGTH, "enum %s {%s:%d}", 
                        get_fully_qualified_symbol_name(user_defined_type, user_defined_type->decl_context, 
                            &is_dependent, &max_level),
                        user_defined_type->file,
                        user_defined_type->line);
                break;
            }
        case SK_CLASS :
            {
                int max_level = 0;
                char is_dependent = 0;

                snprintf(user_defined_str, MAX_LENGTH, "class %s {%s:%d}", 
                        get_fully_qualified_symbol_name(user_defined_type, user_defined_type->decl_context,
                            &is_dependent, &max_level),
                        user_defined_type->file,
                        user_defined_type->line);
                break;
            }
        case SK_TYPEDEF :
            {
                type_t* aliased_type = advance_over_typedefs(user_defined_type->type_information);

                snprintf(user_defined_str, MAX_LENGTH, "%s", 
                        print_declarator(aliased_type));
            }
            break;
        case SK_TEMPLATE_TYPE_PARAMETER :
            snprintf(user_defined_str, MAX_LENGTH, "<type-template parameter '%s' (%d,%d)>",
                    user_defined_type->symbol_name,
                    user_defined_type->entity_specs.template_parameter_nesting,
                    user_defined_type->entity_specs.template_parameter_position);
            break;
        case SK_TEMPLATE_TEMPLATE_PARAMETER :
            snprintf(user_defined_str, MAX_LENGTH, "<template-template parameter '%s' (%d,%d)>",
                    user_defined_type->symbol_name,
                    user_defined_type->entity_specs.template_parameter_nesting,
                    user_defined_type->entity_specs.template_parameter_position);
            break;
        case SK_TEMPLATE_PARAMETER :
            snprintf(user_defined_str, MAX_LENGTH, "<nontype-template parameter '%s' (%d,%d)>", 
                    user_defined_type->symbol_name,
                    user_defined_type->entity_specs.template_parameter_nesting,
                    user_defined_type->entity_specs.template_parameter_position);
            break;
        case SK_TEMPLATE :
            snprintf(user_defined_str, MAX_LENGTH, "<template-name '%s'>", 
                    user_defined_type->symbol_name);
            break;
            break;
        case SK_GCC_BUILTIN_TYPE :
            snprintf(user_defined_str, MAX_LENGTH, "__builtin_va_list");
            break;
        case SK_DEPENDENT_ENTITY :
            snprintf(user_defined_str, MAX_LENGTH, "<dependent entity>");
            break;
        default :
            snprintf(user_defined_str, MAX_LENGTH, "¿¿¿unknown user defined type??? (kind=%d)", user_defined_type->kind);
    }
    result = strappend(result, user_defined_str);

    return result;
}

const char* get_named_type_name(scope_entry_t* entry)
{
    ERROR_CONDITION(entry == NULL, "This cannot be null", 0);
    return get_named_simple_type_name(entry);
}

// Gives the name of a builtin type. This routine is for debugging
static const char* get_builtin_type_name(type_t* type_info)
{
    simple_type_t* simple_type_info = type_info->type;
    ERROR_CONDITION(simple_type_info == NULL, "This cannot be null", 0);
    const char* result = uniquestr("");

    if (simple_type_info->is_long == 1)
    {
        result = strappend(result, "long ");
    }

    if (simple_type_info->is_long >= 2)
    {
        result = strappend(result, "long long ");
    }

    if (simple_type_info->is_short)
    {
        result = strappend(result, "short ");
    }

    if (simple_type_info->is_unsigned)
    {
        result = strappend(result, "unsigned ");
    }

    if (simple_type_info->is_complex)
    {
        result = strappend(result, "_Complex ");
    }

    switch (simple_type_info->kind)
    {
        case STK_BUILTIN_TYPE :
            {
                switch (simple_type_info->builtin_type)
                {
                    case BT_INT :
                        result = strappend(result, "int");
                        break;
                    case BT_BOOL :
                        result = strappend(result, "bool");
                        break;
                    case BT_FLOAT :
                        result = strappend(result, "float");
                        break;
                    case BT_DOUBLE :
                        result = strappend(result, "double");
                        break;
                    case BT_WCHAR :
                        result = strappend(result, "wchar_t");
                        break;
                    case BT_CHAR :
                        result = strappend(result, "char");
                        break;
                    case BT_VOID :
                        result = strappend(result, "void");
                        break;
                    case BT_UNKNOWN :
                    default :
                        result = strappend(result, "¿¿¿unknown builtin type???");
                        break;
                }
                break;
            }
        case STK_USER_DEFINED :
            result = get_named_simple_type_name(simple_type_info->user_defined_type);
            break;
        case STK_ENUM :
            {
                char c[256] = { 0 };
                snprintf(c, 255, "enum <anonymous> %p", type_info);
                result = strappend(result, c);
            }
            break;
        case STK_CLASS :
            {
                const char *template_arguments = "";
                {
                    int i;
                    type_t* actual_class = type_info;
                    if (actual_class->is_template_specialized_type
                            && actual_class->template_arguments != NULL)
                    {
                        template_arguments = strappend(template_arguments, "< ");
                        for (i = 0; i < actual_class->template_arguments->num_arguments; i++)
                        {
                            template_argument_t* template_argument = 
                                actual_class->template_arguments->argument_list[i];

                            switch (template_argument->kind)
                            {
                                case TAK_TYPE:
                                case TAK_TEMPLATE:
                                    {
                                        template_arguments = strappend(template_arguments, 
                                                print_declarator(template_argument->type));
                                        break;
                                    }
                                case TAK_NONTYPE:
                                    {
                                        template_arguments = strappend(template_arguments, 
                                                prettyprint_in_buffer(template_argument->expression));
                                        break;
                                    }
                                default:
                                    {
                                        template_arguments = strappend(template_arguments,
                                                " << unknown template argument >> ");
                                        break;
                                    }
                            }
                            if ((i + 1) < actual_class->template_arguments->num_arguments)
                            {
                                template_arguments = strappend(template_arguments, ", ");
                            }
                        }
                        template_arguments = strappend(template_arguments, " >");
                    }
                }

                char c[256] = { 0 };
                snprintf(c, 255, "class <anonymous>%s %p", template_arguments, type_info);
                result = strappend(result, c);
            }
            break;
        case STK_VA_LIST :
            result = strappend(result, "__builtin_va_list");
            break;
        case STK_TYPEOF :
            result = strappend(result, "__typeof");
            break;
        case STK_TEMPLATE_DEPENDENT_TYPE :
            {
                char c[256] = { 0 };
                snprintf(c, 255, "<template dependent type [%s]::%s%s>", 
                        get_named_simple_type_name(simple_type_info->dependent_entry),
                        prettyprint_in_buffer(simple_type_info->dependent_nested_name),
                        prettyprint_in_buffer(simple_type_info->dependent_unqualified_part));
                result = strappend(result, c);
            }
            break;
        case STK_TEMPLATE_TYPE :
            {
                // FIXME - this should be much more informative
                char c[256] = { 0 };
                snprintf(c, 255, "<template type %p>", 
                        type_info);
                result = strappend(result, c);
                break;
            }
        case STK_TYPEDEF :
            result = strappend(result, print_declarator(advance_over_typedefs(simple_type_info->aliased_type)));
            break;
        default :
            {
                char c[50];
                snprintf(c, 49, "(unknown simple type = %d)", simple_type_info->kind);
                result = strappend(result, c);
                break;
            }
    }

    return result;
}

// This prints a declarator in English. It is intended for debugging purposes
const char* print_declarator(type_t* printed_declarator)
{
    ERROR_CONDITION(printed_declarator == NULL, "This cannot be null", 0);
    const char* tmp_result = "";

    if (is_ellipsis_type(printed_declarator))
    {
        tmp_result = "< ellipsis type >";
        return tmp_result;
    }
    else if (is_dependent_expr_type(printed_declarator))
    {
        tmp_result = "< dependent expression type >";
        return tmp_result;
    }
    else if (is_unresolved_overloaded_type(printed_declarator))
    {
        tmp_result = "< unresolved overload function type >";
        return tmp_result;
    }

    do 
    {
        if ((printed_declarator->cv_qualifier & CV_CONST) == CV_CONST)
        {
            tmp_result = strappend(tmp_result, "const ");
        }
        if ((printed_declarator->cv_qualifier & CV_VOLATILE) == CV_VOLATILE)
        {
            tmp_result = strappend(tmp_result, "volatile ");
        }
        if ((printed_declarator->cv_qualifier & CV_RESTRICT) == CV_RESTRICT)
        {
            CXX_LANGUAGE()
            {
                tmp_result = strappend(tmp_result, "__restrict ");
            }
            C_LANGUAGE()
            {
                tmp_result = strappend(tmp_result, "restrict ");
            }
        }
        switch (printed_declarator->kind)
        {
            case TK_DIRECT :
                if (printed_declarator->type != NULL)
                {
                    tmp_result = strappend(tmp_result, get_builtin_type_name(printed_declarator));
                }
                else
                {
                    tmp_result = strappend(tmp_result, "(nothing)");
                }
                printed_declarator = NULL;
                break;
            case TK_OVERLOAD :
                {
                    tmp_result = strappend(tmp_result, " <unresolved overload function type> ");
                }
                printed_declarator = NULL;
                break;
            case TK_POINTER :
                tmp_result = strappend(tmp_result, "pointer to ");
                printed_declarator = printed_declarator->pointer->pointee;
                break;
            case TK_LVALUE_REFERENCE :
                tmp_result = strappend(tmp_result, "(lvalue) reference to ");
                printed_declarator = printed_declarator->pointer->pointee;
                break;
            case TK_RVALUE_REFERENCE :
                tmp_result = strappend(tmp_result, "rvalue reference to ");
                printed_declarator = printed_declarator->pointer->pointee;
                break;
            case TK_POINTER_TO_MEMBER :
                tmp_result = strappend(tmp_result, "pointer to member of ");
                if (printed_declarator->pointer->pointee_class != NULL)
                {
                    tmp_result = strappend(tmp_result, get_named_type_name(printed_declarator->pointer->pointee_class));
                }
                else
                {
                    tmp_result = strappend(tmp_result, "(unknown class)");
                }
                tmp_result = strappend(tmp_result, " to ");
                printed_declarator = printed_declarator->pointer->pointee;
                break;
            case TK_ARRAY :
                tmp_result = strappend(tmp_result, "array ");
                if (printed_declarator->array->array_expr != NULL)
                {
                    tmp_result = strappend(tmp_result, prettyprint_in_buffer(printed_declarator->array->array_expr));
                    tmp_result = strappend(tmp_result, " of ");
                }
                else
                {
                    tmp_result = strappend(tmp_result, " of ");
                }
                printed_declarator = printed_declarator->array->element_type;
                break;
            case TK_FUNCTION :
                {
                    int i;
                    tmp_result = strappend(tmp_result, "function");

                    if (printed_declarator->is_template_specialized_type
                            && printed_declarator->template_arguments != NULL)
                    {
                        tmp_result = strappend(tmp_result, "< ");
                        for (i = 0; i < printed_declarator->template_arguments->num_arguments; i++)
                        {
                            template_argument_t* template_argument = 
                                printed_declarator->template_arguments->argument_list[i];

                            switch (template_argument->kind)
                            {
                                case TAK_TYPE:
                                case TAK_TEMPLATE:
                                    {
                                        tmp_result = strappend(tmp_result, 
                                                print_declarator(template_argument->type));
                                        break;
                                    }
                                case TAK_NONTYPE:
                                    {
                                        tmp_result = strappend(tmp_result, 
                                                prettyprint_in_buffer(template_argument->expression));
                                        break;
                                    }
                                default:
                                    {
                                        tmp_result = strappend(tmp_result,
                                                " << unknown template argument >> ");
                                        break;
                                    }
                            }
                            if ((i + 1) < printed_declarator->template_arguments->num_arguments)
                            {
                                tmp_result = strappend(tmp_result, ", ");
                            }
                        }
                        tmp_result = strappend(tmp_result, " >");
                    }
                    
                    tmp_result = strappend(tmp_result, " (");
                    for (i = 0; i < printed_declarator->function->num_parameters; i++)
                    {
                        if (!printed_declarator->function->parameter_list[i]->is_ellipsis)
                        {
                            tmp_result = strappend(tmp_result, 
                                    print_declarator(printed_declarator->function->parameter_list[i]->type_info)
                                  );
                        }
                        else
                        {
                            tmp_result = strappend(tmp_result, "...");
                        }
                        if ((i+1) < printed_declarator->function->num_parameters)
                        {
                            tmp_result = strappend(tmp_result, ", ");
                        }
                    }
                    tmp_result = strappend(tmp_result, ")");
                    tmp_result = strappend(tmp_result, " returning ");
                    printed_declarator = printed_declarator->function->return_type;
                    break;
                }
            case TK_VECTOR:
                {
                    char c[256];
                    snprintf(c, 255, "vector of size %d of ", 
                            printed_declarator->vector->vector_size);
                    c[255] = '\0';
                    tmp_result = strappend(tmp_result, c);
                    printed_declarator = printed_declarator->vector->element_type;
                    break;
                }
            case TK_COMPUTED:
                {
                    char c[256];
                    snprintf(c, 255, "<computed function type>");
                    c[255] = '\0';
                    printed_declarator = NULL;
                    break;
                }
            default :
                internal_error("Unhandled type kind '%d'\n", printed_declarator->kind);
                break;
        }
    } while (printed_declarator != NULL);

    return tmp_result;
}


static standard_conversion_t identity_scs(type_t* t_orig, type_t* t_dest)
{
    standard_conversion_t result = {
        .orig = t_orig,
        .dest = t_dest,
        .conv = { SCI_IDENTITY, SCI_NO_CONVERSION, SCI_NO_CONVERSION }
    };

    return result;
}

char standard_conversion_is_identity(standard_conversion_t scs)
{
    return (scs.conv[0] == SCI_IDENTITY);
}

char standard_conversion_is_invalid(standard_conversion_t scs)
{
    return (scs.conv[0] == SCI_NO_CONVERSION
            && scs.conv[1] == SCI_NO_CONVERSION
            && scs.conv[2] == SCI_NO_CONVERSION);
}

type_t* standard_conversion_get_orig_type(standard_conversion_t scs)
{
    return scs.orig;
}

type_t* standard_conversion_get_dest_type(standard_conversion_t scs)
{
    return scs.dest;
}

char pointer_types_are_similar(type_t* t_orig, type_t* t_dest)
{
    type_t* orig = t_orig;
    type_t* dest = t_dest;

    if (is_array_type(orig))
    {
        orig = get_pointer_type(array_type_get_element_type(orig));
    }
    else if (is_function_type(orig))
    {
        orig = get_pointer_type(orig);
    }

    if (is_array_type(dest))
    {
        dest = get_pointer_type(array_type_get_element_type(dest));
    }
    else if (is_function_type(dest))
    {
        dest = get_pointer_type(dest);
    }

    // C, C++
    if ((is_void_pointer_type(orig)
                && is_pointer_type(dest))
            || (is_pointer_type(orig)
                && is_void_pointer_type(dest)))
    {
        return 1;
    }

    C_LANGUAGE()
    {
        // Just in C
        if (is_pointer_type(orig)
                && is_pointer_type(dest))
        {
            return 1;
        }

        if ((is_integer_type(orig)
                && is_pointer_type(dest))
                || (is_integer_type(dest)
                    && is_pointer_type(orig)))
        {
            return 1;
        }
    }

    // This additional comparison is just for C++
    while (is_pointer_type(orig)
            && is_pointer_type(dest))
    {
        orig = pointer_type_get_pointee_type(orig);
        dest = pointer_type_get_pointee_type(dest);
    }

    // Zero type of C++
    if ((is_zero_type(orig)
                && is_pointer_type(dest))
            || (is_zero_type(dest)
                && is_pointer_type(orig)))
    {
        return 1;
    }

    return equivalent_types(get_unqualified_type(orig), get_unqualified_type(dest));
}

// This function checks at the same time similarity and convertibility
char pointer_types_can_be_converted(type_t* orig, type_t* dest)
{
#define MAX_QUALIFS (256)
    ERROR_CONDITION(
            !((is_pointer_type(orig) 
                    && is_pointer_type(dest))
                || (is_pointer_to_member_type(orig) 
                    && is_pointer_to_member_type(dest))), 
            "They have to be both pointers or both pointer to member ", 0);

    // First check they are similar
    type_t* t1 = orig;
    type_t* t2 = dest;

    int num_qualifs = 0;
    cv_qualifier_t qualifs1[MAX_QUALIFS];
    cv_qualifier_t qualifs2[MAX_QUALIFS];

    while ((is_pointer_type(t1)
                && is_pointer_type(t2))
            || (is_pointer_to_member_type(t1)
                && is_pointer_to_member_type(t2)))
    {
        ERROR_CONDITION(num_qualifs >= MAX_QUALIFS, "Too much qualifiers\n", 0);
        qualifs1[num_qualifs] = get_cv_qualifier(t1);
        qualifs2[num_qualifs] = get_cv_qualifier(t2);
        num_qualifs++;

        // If they are pointer to member, ensure they point to the same class
        if (is_pointer_to_member_type(t1)
                && is_pointer_to_member_type(t2))
        {
            if (!equivalent_types(pointer_to_member_type_get_class_type(t1),
                        pointer_to_member_type_get_class_type(t2)))
            {
                return 0;
            }
        }

        t1 = pointer_type_get_pointee_type(t1);
        t2 = pointer_type_get_pointee_type(t2);
    }

    // Add the qualifier of the non-pointer-type
    ERROR_CONDITION(num_qualifs >= MAX_QUALIFS, "Too much qualifiers\n", 0);
    qualifs1[num_qualifs] = get_cv_qualifier(t1);
    qualifs2[num_qualifs] = get_cv_qualifier(t2);
    num_qualifs++;

    if (!equivalent_types(get_unqualified_type(t1), get_unqualified_type(t2)))
        return 0;

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: Qualification conversion. Number of qualifications: %d\n", num_qualifs);
    }
    // First property: if 'const' in cv[1,i] then 'const' in cv[2,i],
    // conversely for 'volatile', forall i > 0
    //
    // Note that we are ignoring the top level one since
    //   'int*' is compatible with 'int * const'
    int i;
    for (i = 1; i < num_qualifs; i++)
    {
        if (is_const_qualified(qualifs1[i])
                && !is_const_qualified(qualifs2[i]))
        {
            return 0;
        }
        if (is_volatile_qualified(qualifs1[i])
                && !is_volatile_qualified(qualifs2[i]))
        {
            return 0;
        }

        // Second property
        // I guess that a better algorithm exists for this one
        if (!is_equal_cv_qualified(qualifs1[i], qualifs2[i]))
        {
            // DEBUG_CODE()
            // {
            //     fprintf(stderr, "Since cv1,%d is different to cv2,%d we have to check that for 0 < k < %d, cv2,k contains 'const'\n",
            //             i, i, i);
            // }
            int j;
            for (j = 1; j < i; j++)
            {
                // DEBUG_CODE()
                // {
                //     fprintf(stderr, "Checking if cv2,%d contains 'const'\n", j);
                // }
                if (!is_const_qualified(qualifs2[j]))
                {
                    return 0;
                }
            }
        }
    }

    // It can be converted
    return 1;
}

char standard_conversion_between_types(standard_conversion_t *result, type_t* t_orig, type_t* t_dest)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "SCS: Trying to find a standard conversion from '%s' to '%s'\n",
                print_declarator(t_orig),
                print_declarator(t_dest));
    }

    (*result) = no_scs_conversion;
    (*result).orig = t_orig;
    (*result).dest = t_dest;

    type_t* orig = t_orig;
    type_t* dest = t_dest;

    // Identity check
    if (equivalent_types(orig, dest))
    {
        (*result) = identity_scs(t_orig, t_dest);
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Exactly the same type\n");
        }
        return 1;
    }

    // Special cases of identity due to how references can be initialized
    // cv1 T -> const cv2 T&
    // cv1 T -> cv2 T&&
    if ((is_lvalue_reference_type(dest)
            && is_const_qualified_type(reference_type_get_referenced_type(dest)))
            || is_rvalue_reference_type(dest))
    {
        type_t* unqualif_orig = get_unqualified_type(orig);
        type_t* unqualif_dest = get_unqualified_type(
                reference_type_get_referenced_type(dest)
                );

        if (equivalent_types(unqualif_orig, unqualif_dest)
                || (is_class_type(unqualif_dest) 
                    && is_class_type(unqualif_orig)
                    && class_type_is_base(unqualif_dest, unqualif_orig)))
        {
            (*result) = identity_scs(t_orig, t_dest);
            DEBUG_CODE()
            {
                if (is_rvalue_reference_type(dest))
                {
                    fprintf(stderr, "SCS: This is a binding to a rvalue-reference by means of an rvalue\n");
                }
                else
                {
                    fprintf(stderr, "SCS: This is a binding to a const lvalue-reference by means of an rvalue\n");
                }
            }
            return 1;
        }
    }
    // cv1 T1& -> cv2 T&&
    if (is_lvalue_reference_type(orig)
            && is_rvalue_reference_type(dest)
            && equivalent_types(get_unqualified_type(reference_type_get_referenced_type(orig)),
                get_unqualified_type(reference_type_get_referenced_type(dest)))
            && is_more_or_equal_cv_qualified_type(reference_type_get_referenced_type(dest),
                reference_type_get_referenced_type(orig)))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: This is a binding to a rvalue-reference by means of a lvalue\n");
        }
        (*result) = identity_scs(t_orig, t_dest);
        return 1;
    }
    // cv1 T1& -> cv2 T2&
    if (is_lvalue_reference_type(orig)
            && is_lvalue_reference_type(dest))
    {
        type_t* ref_dest = reference_type_get_referenced_type(dest);
        type_t* ref_orig = reference_type_get_referenced_type(orig);

        type_t* unqualif_ref_orig = get_unqualified_type(ref_orig);
        type_t* unqualif_ref_dest = get_unqualified_type(ref_dest);

        if ((equivalent_types(unqualif_ref_orig, unqualif_ref_dest)
                    || (is_class_type(unqualif_ref_dest)
                        && is_class_type(unqualif_ref_orig)
                        && class_type_is_base(unqualif_ref_dest, unqualif_ref_orig)))
                && is_more_or_equal_cv_qualified_type(ref_dest, ref_orig))
        {
            (*result) = identity_scs(t_orig, t_dest);
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: This is a binding to a reference by means of lvalue\n");
            }
            return 1;
        }
    }

    // First kind of conversion
    //
    //   lvalue-to-rvalue <-- this means 'T&' to 'T'
    //   array-to-pointer
    //   function-to-pointer
    //
    // We remember whether the original was a string because we will lose this
    // information when we drop the array type
    char is_literal_string = is_literal_string_type(no_ref(orig));
    if (is_array_type(no_ref(orig)))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Applying array-to-pointer conversion\n");
        }
        (*result).conv[0] = SCI_ARRAY_TO_POINTER;
        orig = get_pointer_type(array_type_get_element_type(no_ref(orig)));
    }
    else if (is_function_type(no_ref(orig)))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Applying function-to-pointer conversion\n");
        }
        (*result).conv[0] = SCI_FUNCTION_TO_POINTER;
        orig = get_pointer_type(no_ref(orig));
    }
    else if (is_lvalue_reference_type(orig)
            && !is_lvalue_reference_type(dest))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Applying lvalue-to-rvalue conversion\n");
        }
        (*result).conv[0] = SCI_LVALUE_TO_RVALUE;
        orig = reference_type_get_referenced_type(orig);
    }

    // Second kind of conversion
    //
    //   integral promotions
    //   floating promotions
    //   integral conversion
    //   floating point conversion
    //   floating integral conversion
    //   pointer conversion
    //   pointer-to-member conversion
    //   boolean conversion
    /*
     * FIXME, enums can be promoted to different underlying types
     * Now assuming that all are int
     */
    if (!equivalent_types(dest, orig))
    {
        if (is_signed_int_type(dest)
                && (is_char_type(orig)
                    || is_signed_char_type(orig)
                    || is_unsigned_char_type(orig)
                    || is_signed_short_int_type(orig)
                    || is_unsigned_short_int_type(orig)
                    || is_wchar_t_type(orig)
                    || is_enumerated_type(orig)
                    || is_bool_type(orig)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying integral promotion\n");
            }
            (*result).conv[1] = SCI_INTEGRAL_PROMOTION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        /* vector type - this is to make overload mechanism happy */
        else if (is_vector_type(dest)
                && is_vector_type(orig)
                && vector_types_can_be_converted(dest, orig)
                && !equivalent_types(vector_type_get_element_type(dest),
                    vector_type_get_element_type(orig)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying vectorial integral promotion\n");
            }
            (*result).conv[1] = SCI_VECTOR_INTEGRAL_PROMOTION;
            orig = dest;
        }
        else if ((is_double_type(dest)
                    && is_float_type(orig))
                || (is_long_double_type(dest)
                    && (is_float_type(orig) 
                        || is_double_type(orig))))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying floating promotion\n");
            }
            (*result).conv[1] = SCI_FLOATING_PROMOTION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_integer_type(dest)
                && (is_integer_type(orig) 
                    || is_enumerated_type(orig))
                && !is_bool_type(dest)
                && !is_bool_type(orig)
                && !equivalent_types(dest, orig))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying integral conversion\n");
            }
            (*result).conv[1] = SCI_INTEGRAL_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_integer_type(dest)
                && is_bool_type(orig))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying integral conversion from bool\n");
            }
            (*result).conv[1] = SCI_INTEGRAL_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_floating_type(dest)
                && is_floating_type(orig))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying floating conversion\n");
            }
            (*result).conv[1] = SCI_FLOATING_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if ((is_floating_type(orig)
                    && is_integer_type(dest))
                || (is_integer_type(orig)
                    && is_floating_type(dest)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying floating-integral conversion\n");
            }
            (*result).conv[1] = SCI_FLOATING_INTEGRAL_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_floating_type(dest)
                && is_bool_type(orig))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying floating-integral conversion from bool\n");
            }
            (*result).conv[1] = SCI_FLOATING_INTEGRAL_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (IS_CXX_LANGUAGE
                && is_zero_type(orig)
                && (is_pointer_type(dest)
                    || is_pointer_to_member_type(dest)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying pointer-conversion from 0 to pointer\n");
            }

            (*result).conv[1] = SCI_POINTER_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_pointer_type(orig)
                && is_pointer_to_void_type(dest))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying pointer-conversion to void*\n");
            }
            (*result).conv[1] = SCI_POINTER_CONVERSION;

            // We need to keep the cv-qualification of the original pointer
            // e.g.: 'const int*' -> 'void*'
            // will conver the original into 'const void*'
            orig = get_pointer_type(
                    get_cv_qualified_type(get_void_type(),
                        get_cv_qualifier(pointer_type_get_pointee_type(orig))));
        }
        else if (IS_C_LANGUAGE
                && is_pointer_type(dest) 
                && !is_pointer_to_void_type(dest)
                && is_pointer_to_void_type(orig))
        {
            // The following is valid in C
            //
            // int* c = malloc(sizeof(int)); 
            //
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying pointer-conversion from void* to another pointer type\n");
            }

            (*result).conv[1] = SCI_POINTER_CONVERSION;
            dest = orig;
        }
        else if (IS_C_LANGUAGE
                && is_integral_type(orig)
                && is_pointer_type(dest))
        {
            // This should deserve a warning, but allow it anyway
            // This is not valid in C++ but "tolerated" in C
            //
            // int * p;
            //
            // p = 3;
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying integer to pointer conversion.\n");
                fprintf(stderr, "SCS: Warning: This conversion should be explicited by means of a cast!\n");
            }

            (*result).conv[1] = SCI_POINTER_CONVERSION;
            dest = orig;
        }
        else if (IS_C_LANGUAGE
                && is_integral_type(dest)
                && is_pointer_type(orig))
        {
            // This should deserve a warning, but allow it anyway
            // This is not valid in C++ but "tolerated" in C
            //
            // int a;
            // int *p;
            //
            // a = *p;
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying pointer to integer conversion.\n");
                fprintf(stderr, "SCS: Warning: This conversion should be explicited by means of a cast!\n");
            }

            (*result).conv[1] = SCI_POINTER_CONVERSION;
            dest = orig;
        }
        else if (is_pointer_to_class_type(orig)
                && is_pointer_to_class_type(dest)
                && pointer_to_class_type_is_base(dest, orig))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying pointer conversion to pointer to base class\n");
            }
            (*result).conv[1] = SCI_POINTER_CONVERSION;
            // Note that we make orig to be the dest class pointer, because we want
            // to state qualification conversion later
            orig = get_pointer_type(
                    get_unqualified_type(
                        pointer_type_get_pointee_type(dest) // given 'cv1 A cv2*' this returns 'cv1 A'
                        )  // This returns 'A', not cv-qualified
                    ); // This returns 'A*'
        }
        else if (is_pointer_to_member_type(orig)
                && is_pointer_to_member_type(dest)
                // Note: we will check that they are valid pointer-to-members later, in qualification conversion
                // Note: inverted logic here, since pointers to member are compatible downwards the class hierarchy
                && class_type_is_base(pointer_to_member_type_get_class_type(orig), pointer_to_member_type_get_class_type(dest)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying pointer-to-member conversion to pointer-to-member of derived class\n");
            }
            (*result).conv[1] = SCI_POINTER_TO_MEMBER_CONVERSION;
            // Note that orig is converted to an unqualified version of the dest type.
            // Given dest as 'cv1 T (A::* cv2)' we will set orig to 'T (A::*)'
            orig = get_pointer_to_member_type(
                    get_unqualified_type(pointer_type_get_pointee_type(dest)), // This gives us 'T'
                    pointer_to_member_type_get_class(dest) // This is 'A'
                    );
        }
        else if (is_bool_type(dest)
                && !is_bool_type(orig)
                && (is_integral_type(orig)
                    || is_enumerated_type(orig)
                    || is_pointer_type(orig)
                    || is_pointer_to_member_type(orig)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying boolean conversion\n");
            }
            (*result).conv[1] = SCI_BOOLEAN_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
    }

    // Third kind of conversion
    //
    //  qualification-conversion
    //
    if (!equivalent_types(orig, dest) 
            && ((is_pointer_type(orig) 
                    && is_pointer_type(dest))
                || (is_pointer_to_member_type(orig) 
                    && is_pointer_to_member_type(dest))))
    {
        if (pointer_types_can_be_converted(orig, dest))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying qualification conversion\n");
            }
            (*result).conv[2] = SCI_QUALIFICATION_CONVERSION;
            orig = dest;
        }
        else if (IS_CXX_LANGUAGE
                && is_literal_string // We saved this before dropping the array
                && is_pointer_type(dest)
                && is_char_type(pointer_type_get_pointee_type(dest))
                && !is_const_qualified_type(pointer_type_get_pointee_type(dest)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying deprecated string literal conversion to 'char*'\n");
            }
            (*result).conv[2] = SCI_QUALIFICATION_CONVERSION;
            orig = dest;
        }
        else if (IS_CXX_LANGUAGE
                && is_literal_string // We saved this before dropping the array
                && is_pointer_type(dest)
                && is_wchar_t_type(pointer_type_get_pointee_type(dest))
                && !is_const_qualified_type(pointer_type_get_pointee_type(dest)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying deprecated wide string literal conversion to 'wchar_t*'\n");
            }
            (*result).conv[2] = SCI_QUALIFICATION_CONVERSION;
            orig = dest;
        }
        else if (IS_C_LANGUAGE)
        {
            // C allows such cases
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying pointer to pointer conversion\n");
                fprintf(stderr, "SCS: Warning: This conversion should be explicited by means of a cast!\n");
            }
            (*result).conv[2] = SCI_QUALIFICATION_CONVERSION;
            orig = dest;
        }
    }

    // Here being restrict does not matter
    if (is_restrict_qualified_type(orig))
    {
        orig = get_cv_qualified_type(orig, get_cv_qualifier(orig) & (~CV_RESTRICT));
    }
    if (is_restrict_qualified_type(dest))
    {
        dest = get_cv_qualified_type(dest, get_cv_qualifier(dest) & (~CV_RESTRICT));
    }

    // Here being volatile does not matter
    if (is_volatile_qualified_type(orig))
    {
        orig = get_cv_qualified_type(orig, get_cv_qualifier(orig) & (~CV_VOLATILE));
    }
    if (is_volatile_qualified_type(dest))
    {
        dest = get_cv_qualified_type(dest, get_cv_qualifier(dest) & (~CV_VOLATILE));
    }
    
    // Drop any cv-qualification of the original since it does not prevent
    // from converting it to a less qualified one dest
    //
    //   const int n;
    //   int m;
    //
    //   n = m; <-- error (orig: int | dest: const int)
    //   m = n; <-- ok (orig: const int | dest: int)
    orig = get_unqualified_type(orig);

    DEBUG_CODE()
    {
        fprintf(stderr, "SCS: Checking types converted so far '%s' and '%s' are equivalent\n",
                print_declarator(orig),
                print_declarator(dest));
    }
    char valid_conversion = equivalent_types(orig, dest);

    if (!valid_conversion)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: No conversion is possible from '%s' to '%s'\n",
                    print_declarator(t_orig),
                    print_declarator(t_dest));
        }
        (*result) = no_scs_conversion;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: There is a standard conversion from '%s' to '%s'\n",
                    print_declarator(t_orig),
                    print_declarator(t_dest));
        }
    }

    return valid_conversion;
}

type_t* get_unresolved_overloaded_type(scope_entry_list_t* overload_set,
        template_argument_list_t* explicit_template_arguments)
{
    type_t* result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);

    result->kind = TK_OVERLOAD;

    result->unqualified_type = result;
    result->overload_set = overload_set;
    result->explicit_template_argument_list = explicit_template_arguments;

    return result;
}

char is_unresolved_overloaded_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_OVERLOAD);
}

scope_entry_list_t *unresolved_overloaded_type_get_overload_set(type_t* t)
{
    ERROR_CONDITION(!is_unresolved_overloaded_type(t), "This is not an unresolved overloaded type", 0);

    return t->overload_set;
}

template_argument_list_t* unresolved_overloaded_type_get_explicit_template_arguments(type_t* t)
{
    ERROR_CONDITION(!is_unresolved_overloaded_type(t), "This is not an unresolved overloaded type", 0);

    return t->explicit_template_argument_list;
}

static type_t* _dependent_type = NULL;

type_t* get_dependent_expr_type(void)
{
    if (_dependent_type == NULL)
    {
        _dependent_type = get_simple_type();
    }
    return _dependent_type;
}

char is_dependent_expr_type(type_t* t)
{
    return (t != NULL
            && (t->unqualified_type == _dependent_type
                || (is_named_type(t) 
                    && (named_type_get_symbol(t)->kind == SK_TEMPLATE_PARAMETER)))
           );
}

static type_t* _zero_type = NULL;

// Special type for '0'
type_t* get_zero_type(void)
{
    if (_zero_type == NULL)
    {
        _zero_type = get_simple_type();
        _zero_type->type->kind = STK_BUILTIN_TYPE;
        _zero_type->type->builtin_type = BT_INT;
    }

    return _zero_type;
}

char is_zero_type(type_t* t)
{
    return ((_zero_type != NULL)
            && (t == _zero_type));
}

static int _literal_string_set_num_elements = 0;
static type_t** _literal_string_set = NULL;

static int _literal_wide_string_set_num_elements = 0;
static type_t** _literal_wide_string_set = NULL;

type_t* get_literal_string_type(int length, char is_wchar)
{
    int *max_length = &_literal_string_set_num_elements;
    type_t*** set = &_literal_string_set;

    if (is_wchar)
    {
        max_length = &_literal_wide_string_set_num_elements;
        set = &_literal_wide_string_set;
    }

    // Allocate exponentially
    while ((*max_length) < length)
    {
        // The +1 is important or we will never grow
        int previous_max_length = (*max_length);
        (*max_length) = (*max_length) * 2 + 1;

        // +1 is because of zero position (never used)
        (*set) = realloc(*set, sizeof(type_t*) * ((*max_length) + 1));

        // Clear new slots
        int i;
        for (i = previous_max_length; i <= (*max_length); i++)
        {
            (*set)[i] = NULL;
        }
    }

    if ((*set)[length] == NULL)
    {

        /* Create an array type */
        char c[256];
        snprintf(c, 255, "%d", length); c[255] = '\0';
        AST integer_literal = ASTLeaf(AST_DECIMAL_LITERAL, 0, c);

        type_t* char_type = NULL;

        if (!is_wchar)
        {
            char_type = get_char_type();
        }
        else
        {
            char_type = get_wchar_t_type();
        }
        CXX_LANGUAGE()
        {
            char_type = get_cv_qualified_type(char_type, CV_CONST);
        }

        /*
         * FIXME - We need a decl context here 
         */
        decl_context_t decl_context;
        memset(&decl_context, 0, sizeof(decl_context));

        type_t* array_type = get_array_type(char_type, integer_literal, decl_context);

        // Set that this array is actually a string literal
        array_type->array->is_literal_string = 1;

        (*set)[length] = array_type;
    }

    return (*set)[length];
}

char is_literal_string_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (is_array_type(t)
            && t->array->is_literal_string);
}

static type_t* _ellipsis_type = NULL;

type_t* get_ellipsis_type(void)
{
    if (_ellipsis_type == NULL)
    {
        _ellipsis_type = counted_calloc(1, sizeof(*_ellipsis_type), &_bytes_due_to_type_system);
        _ellipsis_type->kind = TK_ELLIPSIS;
    }

    return _ellipsis_type;
}

char is_ellipsis_type(type_t* t)
{
    return ((_ellipsis_type != NULL)
            && (t == _ellipsis_type));
}

static type_t* _throw_expr_type = NULL;

type_t* get_throw_expr_type(void)
{
    if (_throw_expr_type == NULL)
    {
        _throw_expr_type = get_simple_type();
        _throw_expr_type->type->kind = STK_BUILTIN_TYPE;
        _throw_expr_type->type->builtin_type = BT_VOID;
    }

    return _throw_expr_type;
}

char is_throw_expr_type(type_t* t)
{
    return ((_throw_expr_type != NULL)
            && (t == _throw_expr_type));
}

static type_t* _pseudo_destructor_call_type = NULL;

type_t* get_pseudo_destructor_call_type(void)
{
    if (_pseudo_destructor_call_type == NULL)
    {
        _pseudo_destructor_call_type = 
            get_pointer_type(get_new_function_type(get_void_type(), NULL, 0));
    }

    return _pseudo_destructor_call_type;
}

char is_pseudo_destructor_call_type(type_t *t)
{
    return (_pseudo_destructor_call_type != NULL) && 
        t == _pseudo_destructor_call_type;
}

int get_sizeof_type(type_t* t)
{
    return t->size;
}

// This states if vector type t1 can be assigned/initialized using a vector type t2
char vector_types_can_be_converted(type_t* t1, type_t* t2)
{
    ERROR_CONDITION(!is_vector_type(t1)
            || !is_vector_type(t2), 
            "This is not a vector type", 0);

    char both_are_integral = 
        is_integral_type(vector_type_get_element_type(t1))
        && is_integral_type(vector_type_get_element_type(t2));

    char equal_types = equivalent_types(t1, t2);

    char both_have_same_vector_size =
        vector_type_get_vector_size(t1) == vector_type_get_vector_size(t2);

    if ((!both_are_integral || !both_have_same_vector_size)
            && !equal_types)
    {
        return 0;
    }
    return 1;
}

struct type_tag* get_computed_function_type(computed_function_type_t compute_type_function)
{
    type_t* result = counted_calloc(1, sizeof( *result ), &_bytes_due_to_type_system);

    result->kind = TK_COMPUTED;
    result->unqualified_type = result;
    result->compute_type_function = compute_type_function;

    return result;
}

char is_computed_function_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_COMPUTED);
}

computed_function_type_t computed_function_type_get_computing_function(type_t* t)
{
    ERROR_CONDITION(!is_computed_function_type(t),
            "This is not a computed function type!", 0);
    return t->compute_type_function;
}
