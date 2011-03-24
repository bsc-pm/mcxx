/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information 
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/



#include <stdio.h>
#include <string.h>
#include "cxx-buildscope.h"
#include "cxx-typeutils.h"
#include "cxx-typeenviron.h"
#include "cxx-typeunif.h"
#include "cxx-type-trie.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-exprtype.h"
#include "cxx-prettyprint.h"
#include "cxx-driver.h"
#include "cxx-ambiguity.h"
#include "cxx-tltype.h"
#include "cxx-attrnames.h"
#include "cxx-entrylist.h"

#include "red_black_tree.h"

/*
 * --
 */
static long long unsigned int _bytes_due_to_type_system = 0;

// An exception specifier used in function info
typedef struct {
    int num_exception_types;
    type_t** exception_type_seq;
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
    TK_BRACED_LIST,        // 12
    TK_ERROR,
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
    STK_BUILTIN_TYPE, // [1] int, float, char, wchar_t, bool, void;
    STK_COMPLEX,
    STK_CLASS, // [2] struct {identifier};
    STK_ENUM, // [3] enum {identifier}
    STK_INDIRECT, // [4] A type defined after the type of another symbol
    STK_TEMPLATE_TYPE, // [5] a template type
    // An unknown type
    STK_TEMPLATE_DEPENDENT_TYPE, // [6]
    // GCC Extensions
    STK_VA_LIST, // [7] __builtin_va_list {identifier};
    STK_TYPEOF  // [8] __typeof__(int) {identifier};
} simple_type_kind_t;

// Information of enums
typedef 
struct enum_information_tag {
    int num_enumeration; // Number of enumerations declared for this enum
    scope_entry_t** enumeration_list; // The symtab entry of the enum
    type_t* underlying_type; // The underlying type of this enum type
} enum_info_t;

struct simple_type_tag;


// Base class info (parent classes of a given class)
typedef 
struct base_class_info_tag
{
    // The parent class type
    type_t* class_type;

    // The parent class symbol
    scope_entry_t* class_symbol;

    // The access specifier (public, private, protected inheritance)
    access_specifier_t access_specifier;

    // A virtual base
    unsigned char is_virtual:1;
    // A dependent base
    unsigned char is_dependent:1;

    // Used when laying classes out
    _size_t base_offset;
} base_class_info_t;

typedef
struct virtual_base_class_info_tag
{
    scope_entry_t* virtual_base;
    _size_t virtual_base_offset;
} virtual_base_class_info_t;

// Information of a class
typedef 
struct class_information_tag {
    // Kind of class {struct, class}
    enum class_kind_t class_kind:4;

    // Currently unused
    unsigned char is_local_class:1;

    // Is abstract class
    unsigned char is_abstract:1;

    // Enclosing class type
    type_t* enclosing_class_type;

    // The inner decl context created by this class
    decl_context_t inner_decl_context;
    
    // All members must be here, but can also be in lists below
    int num_members;
    scope_entry_t** members;

    // Destructor
    scope_entry_t* destructor;

    // Member functions
    int num_member_functions;
    scope_entry_t** member_functions;

    // Conversion functions info
    int num_conversion_functions;
    scope_entry_t** conversion_functions;

    // Copy assignment function info
    int num_copy_assignment_operator_functions;
    scope_entry_t** copy_assignment_operator_function_list;

    // Move assignment function info
    int num_move_assignment_operator_functions;
    scope_entry_t** move_assignment_operator_function_list;

    // Class constructors info
    int num_constructors;
    scope_entry_t** constructor_list;

    // Default constructor
    scope_entry_t* default_constructor;

    // Copy constructors
    int num_copy_constructors;
    scope_entry_t** copy_constructor_list;

    // Move constructors
    int num_move_constructors;
    scope_entry_t** move_constructor_list;

    // Nonstatic data members
    int num_nonstatic_data_members;
    scope_entry_t** nonstatic_data_members;
    
    // Static data members
    int num_static_data_members;
    scope_entry_t** static_data_members;

    // Virtual functions
    int num_virtual_functions;
    scope_entry_t** virtual_functions;

    // Typenames (either typedefs, enums or inner classes)
    int num_typenames;
    scope_entry_t** typenames;

    // Base (parent classes) info
    int num_bases;
    base_class_info_t** base_classes_list;

    // Virtual base info (used only when laying types)
    int num_virtual_bases;
    virtual_base_class_info_t** virtual_base_classes_list;

    // Info for laying out 
    _size_t non_virtual_size;
    _size_t non_virtual_align;
} class_info_t;

// Direct type covers types that are not pointers to something, neither
// functions to something neither arrays to something.  So every basic type is
// represented here including builtin types, classes, structs, enums, unions
// and other nuclear types (like type template parameters)
typedef 
struct simple_type_tag {
    // Kind
    simple_type_kind_t kind:4;

    // if Kind == STK_BUILTIN_TYPE here we have
    // the exact builtin type
    builtin_type_t builtin_type:4;

    // This can be 0, 1 (long) or 2 (long long)
    unsigned char is_long:2; 
    // short
    unsigned char is_short:1;
    // unsigned
    unsigned char is_unsigned:1;
    // signed
    unsigned char is_signed:1;

    // For typeof and template dependent types
    // (kind == STK_TYPEOF)
    unsigned char typeof_is_expr:1;

    // States that the STK_INDIRECT is a not the last indirect
    unsigned char is_indirect:1;

    // This type exists after another symbol, for
    // instance
    //
    // class A
    // {
    // };
    // A b;
    //
    // creates an 'A' symbol of type SK_CLASS
    // and a 'b' symbol SK_VARIABLE with type STK_INDIRECT
    // pointing to 'A' symbol
    scope_entry_t* user_defined_type;

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
    AST typeof_expr;
    decl_context_t typeof_decl_context;

    // For template types
    template_parameter_list_t* template_parameter_list;
    // This is a STK_INDIRECT
    type_t* primary_specialization;
    // Sometimes we need the original symbol defining this template type
    scope_entry_t* related_template_symbol;

    // Specialized types
    int num_specialized_types;
    // These are a STK_INDIRECT
    type_t** specialized_types;

    // Template dependent types (STK_TEMPLATE_DEPENDENT_TYPE)
    scope_entry_t* dependent_entry;
    dependent_name_part_t* dependent_parts;

    // Complex types, base type of the complex type
    type_t* complex_element;
} simple_type_t;


// Function information
typedef 
struct function_tag
{
    // The returning type of the function
    type_t* return_type;

    // Parameter information
    int num_parameters;
    parameter_info_t** parameter_list;

    // States if this function has been declared or defined without prototype.
    // This is only meaningful in C but not in C++ where all functions do have
    // prototype
    unsigned char lacks_prototype:1;
} function_info_t;

// Pointers, references and pointers to members
typedef 
struct pointer_tag
{
    // The pointee type
    type_t* pointee;

    // If the type was a TK_POINTER_TO_MEMBER
    // the pointee class
    scope_entry_t* pointee_class;
} pointer_info_t;

// Array information
typedef 
struct array_tag
{
    // Whole size. If the array is created using lower and upper boundaries
    // this one is upper - lower + 1
    AST whole_size;
    // This is always zero in C
    // It may be non zero in Fortran
    AST lower_bound; 

    // This is the upper bound. If the array is created using the size of the
    // array, this is always whole_size - 1
    AST upper_bound;

    // Scope of the array size expressions
    decl_context_t array_expr_decl_context;

    // The type of the array elements
    type_t* element_type;

    // Is literal string type ?
    unsigned char is_literal_string:1;
    unsigned char is_vla:1;
    // This one states that we should be using dim instead of array_expr_decl_context
    unsigned char is_plain:1;
} array_info_t;

// Vector type
typedef struct vector_tag
{
    unsigned int vector_size;
    type_t* element_type;
} vector_info_t;

typedef
struct braced_list_info_tag
{
    int num_types;
    type_t** type_list;
} braced_list_info_t;

typedef
struct common_type_info_tag
{
    // See below for more detailed descriptions
    unsigned char is_template_specialized_type:1;
    unsigned char valid_size:1;

    unsigned char is_dependent:1;
    unsigned char is_incomplete:1;

    // The sizeof and alignment of the type
    // They are only valid once 'valid_size' is true
    // --> char valid_size;
    _size_t size;
    _size_t alignment;
    // This is here only for C++
    _size_t data_size;
} common_type_info_t;

// This is the basic type information, except for kind and cv-qualifier
// all fields have to be pointers, common fields that are not pointers are
// stored in the struct of the field 'info'
struct type_tag
{
    // Kind of the type
    enum type_kind kind:4;

    // cv-qualifier related to this type
    // The cv-qualifier is in the type
    cv_qualifier_t cv_qualifier;

    // We use a pointer so we can safely copy in cv-qualified versions
    struct common_type_info_tag* info;

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

    // Braced list type
    braced_list_info_t* braced_type;

    // Vector Type
    // (kind == TK_VECTOR)
    vector_info_t* vector;

    // Unqualified type, itself if the type is not qualified
    type_t* unqualified_type;

    // For parameter types, if not null it means some adjustement was done
    type_t* original_type;
    // For template specialized parameters
    // --> char is_template_specialized_type;
    template_argument_list_t* template_arguments;
    type_t* related_template_type;
    // It is not obvious why do we need this, but it is for
    // checking that unification actually succeed
    // It is only NON-null for complete types
    template_parameter_list_t* template_parameters;

    // (kind == TK_COMPUTED)
    computed_function_type_t compute_type_function;
};

static common_type_info_t* new_common_type_info(void)
{
    common_type_info_t* result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);
    return result;
}

static type_t* new_empty_type(void)
{
    type_t* result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);
    result->info = new_common_type_info();
    return result;
}

const standard_conversion_t no_scs_conversion = { 
    .orig = NULL,
    .dest = NULL,
    .conv = { SCI_NO_CONVERSION, SCI_NO_CONVERSION, SCI_NO_CONVERSION } 
};


static unsigned int _function_type_counter = 0;
static unsigned int _function_type_reused = 0;
static unsigned int _class_type_counter = 0;
static unsigned int _array_type_counter = 0;
static unsigned int _pointer_type_counter = 0;
static unsigned int _pointer_to_member_type_counter = 0;
static unsigned int _reference_type_counter = 0;
static unsigned int _template_type_counter = 0;
static unsigned int _qualified_type_counter = 0;
static unsigned int _vector_type_counter = 0;
static unsigned int _enum_type_counter = 0;

unsigned int get_function_type_counter(void)
{
    return _function_type_counter;
}

unsigned int get_function_type_reused(void)
{
    return _function_type_reused;
}

unsigned int get_class_type_counter(void)
{
    return _class_type_counter;
}

unsigned int get_array_type_counter(void)
{
    return _array_type_counter;
}

unsigned int get_pointer_type_counter(void)
{
    return _pointer_type_counter;
}

unsigned int get_pointer_to_member_type_counter(void)
{
    return _pointer_to_member_type_counter;
}

unsigned int get_reference_type_counter(void)
{
    return _reference_type_counter;
}

unsigned int get_template_type_counter(void)
{
    return _template_type_counter;
}

unsigned int get_qualified_type_counter(void)
{
    return _qualified_type_counter;
}

unsigned int get_vector_type_counter(void)
{
    return _vector_type_counter;

}unsigned int get_enum_type_counter(void)
{
    return _enum_type_counter;
}

size_t get_type_t_size(void)
{
    return sizeof(type_t);
}

/*
 * --
 */

/*
 * This file contains routines destined to work with types.  Comparing two
 * types, comparing function declarations and definitions, etc.
 */
static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2);
static char equivalent_array_type(array_info_t* t1, array_info_t* t2);
static char equivalent_function_type(type_t* t1, type_t* t2);
static char equivalent_vector_type(type_t* t1, type_t* t2);
static char compatible_parameters(function_info_t* t1, function_info_t* t2);
static char compare_template_dependent_typename_types(type_t* t1, type_t* t2);
static char equivalent_pointer_to_member_type(type_t* t1, type_t* t2);


long long unsigned int type_system_used_memory(void)
{
    return _bytes_due_to_type_system;
}

/* Type constructors : Builtins */

static type_t* get_simple_type(void)
{
    type_t* result = new_empty_type();
    result->kind = TK_DIRECT;
    result->type = counted_calloc(1, sizeof(*result->type), &_bytes_due_to_type_system);
    result->unqualified_type = result;
    return result;
}

type_t* get_char_type(void)
{
    // This special char is not signed nor unsigned
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_CHAR;
        _type->info->size = 1;
        _type->info->alignment = 1;
        _type->info->valid_size = 1;
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
        _type->info->size = 1;
        _type->info->alignment = 1;
        _type->info->valid_size = 1;
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
        _type->info->size = 1;
        _type->info->alignment = 1;
        _type->info->valid_size = 1;
    }

    return _type;
}

type_t* get_wchar_t_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        CXX_LANGUAGE()
        {
            _type = get_simple_type();
            _type->type->kind = STK_BUILTIN_TYPE;
            _type->type->builtin_type = BT_WCHAR;
            _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_wchar_t;
            _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_wchar_t;
            _type->info->valid_size = 1;
        }
        // In C there is no wchar_t type, use 'int'
        C_LANGUAGE()
        {
            _type = get_signed_int_type();
        }
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
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_bool;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_bool;
        _type->info->valid_size = 1;
    }

    return _type;
}

type_t* get_bool_of_integer_type(type_t* t)
{
    ERROR_CONDITION(!is_integer_type(t), "Invalid type for bool", 0);

#define MAX_BOOL_SIZE 16
    static type_t* _bool_types[MAX_BOOL_SIZE + 1] = { 0 };

    _size_t s = type_get_size(t);

    ERROR_CONDITION(s > MAX_BOOL_SIZE, "Integer of size %d too big for bool", s);

    if (_bool_types[s] == NULL)
    {
        _bool_types[s] = get_simple_type();
        _bool_types[s]->type->kind = STK_BUILTIN_TYPE;
        _bool_types[s]->type->builtin_type = BT_BOOL;
        _bool_types[s]->info->size = type_get_size(t);
        _bool_types[s]->info->alignment = type_get_alignment(t);
        _bool_types[s]->info->valid_size = 1;
    }

    return _bool_types[s];
}

type_t* get_signed_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_signed_int;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_signed_int;
        _type->info->valid_size = 1;
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
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_signed_short;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_signed_short;
        _type->info->valid_size = 1;
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
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_signed_long;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_signed_long;
        _type->info->valid_size = 1;
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
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_signed_long_long;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_signed_long_long;
        _type->info->valid_size = 1;
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
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_unsigned_int;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_unsigned_int;
        _type->info->valid_size = 1;
    }

    return _type;
}

type_t* get_size_t_type(void)
{
    if (!CURRENT_CONFIGURATION->disable_sizeof)
    {
        return (CURRENT_CONFIGURATION->type_environment->type_of_sizeof)();
    }
    else
    {
        return get_unsigned_int_type();
    }
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
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_unsigned_short;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_unsigned_short;
        _type->info->valid_size = 1;
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
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_unsigned_long;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_unsigned_long;
        _type->info->valid_size = 1;
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
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_unsigned_long_long;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_unsigned_long_long;
        _type->info->valid_size = 1;
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
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_float;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_float;
        _type->info->valid_size = 1;
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
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_double;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_double;
        _type->info->valid_size = 1;
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
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_long_double;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_long_double;
        _type->info->valid_size = 1;
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
        C_LANGUAGE()
        {
            _type->info->size = 1;
            _type->info->alignment = 1;
            _type->info->valid_size = 1;
        }
        CXX_LANGUAGE()
        {
            _type->info->is_incomplete = 1;
        }
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

        result->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_builtin_va_list;
        result->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_builtin_va_list;
        result->info->valid_size = 1;
    }

    return result;
}

char is_gcc_builtin_va_list(type_t *t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_VA_LIST);
}

static void null_dtor(const void* v UNUSED_PARAMETER) { }

static type_t* rb_tree_query_type(rb_red_blk_tree* tree, type_t* t)
{
    type_t* result = NULL;
    rb_red_blk_node* n = rb_tree_query(tree, t);
    if (n != NULL)
    {
        result = (type_t*)rb_node_get_info(n);
    }

    return result;
}

static type_t* rb_tree_query_symbol(rb_red_blk_tree* tree, scope_entry_t* sym)
{
    type_t* result = NULL;
    rb_red_blk_node* n = rb_tree_query(tree, sym);
    if (n != NULL)
    {
        result = (type_t*)rb_node_get_info(n);
    }

    return result;
}

static type_t* get_indirect_type_(scope_entry_t* entry, char indirect)
{
    static rb_red_blk_tree *_user_defined_types_arr[2] = { NULL, NULL };

    if (_user_defined_types_arr[!!indirect] == NULL)
    {
        _user_defined_types_arr[!!indirect] = rb_tree_create(integer_comp, null_dtor, null_dtor);
    }

    rb_red_blk_tree * _user_defined_types = _user_defined_types_arr[!!indirect];
    
    type_t* type_info = rb_tree_query_symbol(_user_defined_types, entry);

    if (type_info == NULL)
    {
        type_info = get_simple_type();

        type_info->type->kind = STK_INDIRECT;
        type_info->type->user_defined_type = entry;
        type_info->type->is_indirect = indirect;

        if (entry->type_information != NULL)
        {
            ERROR_CONDITION(entry->type_information->unqualified_type == NULL, "This cannot be null", 0);
        }

        if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
        {
            type_info->info->is_dependent = 1;
        }
        else if (entry->type_information != NULL)
        {
            type_info->info->is_dependent = is_dependent_type(entry->type_information);
        }

        rb_tree_add(_user_defined_types, entry, type_info);
    }

    return type_info;
}

type_t* get_user_defined_type(scope_entry_t* entry)
{
    return get_indirect_type_(entry, /* indirect */ entry->kind == SK_TYPEDEF);
}

type_t* get_indirect_type(scope_entry_t* entry)
{
    return get_indirect_type_(entry, /* indirect */ 1);
}

static dependent_name_part_t* get_dependent_nested_part(
        decl_context_t decl_context,
        AST nested_part,
        int *nesting_level)
{
    ERROR_CONDITION(nested_part == NULL, "This tree cannot be NULL", 0);

    dependent_name_part_t* result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);

    if (ASTType(nested_part) == AST_SYMBOL)
    {
        result->name = ASTText(nested_part);
    }
    else if (ASTType(nested_part) == AST_TEMPLATE_ID)
    {
        AST sym = ASTSon0(nested_part);

        result->name = ASTText(sym);

        result->template_arguments = get_template_arguments_from_syntax(
                ASTSon1(nested_part),
                decl_context,
                *nesting_level);
        (*nesting_level)++;
    }
    else if (ASTType(nested_part) == AST_OPERATOR_FUNCTION_ID
            || ASTType(nested_part) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
    {
        result->name = get_operator_function_name(nested_part);

        if (ASTType(nested_part) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
        {
            result->template_arguments = get_template_arguments_from_syntax(
                    ASTSon1(nested_part),
                    decl_context,
                    *nesting_level);
            (*nesting_level)++;
        }
    }
    else if (ASTType(nested_part) == AST_CONVERSION_FUNCTION_ID)
    {
        result->name = get_conversion_function_name(decl_context, nested_part, 
                &result->related_type);
    }
    else if (ASTType(nested_part) == AST_DESTRUCTOR_ID)
    {
        return get_dependent_nested_part(decl_context, 
                ASTSon0(nested_part), 
                nesting_level);
    }
    else
    {
        internal_error("Invalid tree of kind '%s'\n", ast_print_node_type(ASTType(nested_part)));
    }

    return result;
}

static dependent_name_part_t* compute_dependent_parts_of_dependent_symbol(
        scope_entry_t* entry, 
        scope_entry_t** enclosing_dependent_entity,
        int *nesting_level)
{
    dependent_name_part_t* result = NULL;
    dependent_name_part_t* enclosing = NULL;
    if (entry->decl_context.current_scope->kind == CLASS_SCOPE
            && entry->decl_context.current_scope->related_entry->kind == SK_CLASS)
    {
        enclosing = compute_dependent_parts_of_dependent_symbol(
                entry->decl_context.current_scope->related_entry,
                enclosing_dependent_entity,
                nesting_level);
    }

    if (is_template_specialized_type(entry->type_information))
    {
        (*nesting_level)++;
    }

    if (is_dependent_type(entry->type_information))
    {
        if (*enclosing_dependent_entity == NULL)
        {
            *enclosing_dependent_entity = entry;
        }
        else
        {
            result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);
            result->name = entry->symbol_name;

            if (is_template_specialized_type(entry->type_information))
            {
                result->template_arguments = template_specialized_type_get_template_arguments(entry->type_information);
            }

            if (enclosing != NULL)
            {
                dependent_name_part_t* tmp = enclosing;

                while (tmp->next != NULL) 
                    tmp = tmp->next;
                tmp->next = result;

                result = enclosing;
            }
        }
    }

    return result;
}

static dependent_name_part_t* compute_dependent_parts(
        decl_context_t decl_context,
        AST nested_name,
        AST unqualified_part,
        int *nesting_level)
{
    if (nested_name == NULL)
    {
        return get_dependent_nested_part(decl_context, 
                unqualified_part,
                nesting_level);
    }
    else
    {
        AST next_part = ASTSon1(nested_name);

        dependent_name_part_t* next = compute_dependent_parts(
                decl_context,
                next_part,
                unqualified_part, 
                nesting_level);

        dependent_name_part_t* current = get_dependent_nested_part(
                decl_context, ASTSon0(nested_name), 
                nesting_level);

        current->next = next;

        return current;
    }
}

dependent_name_part_t* copy_dependent_parts(
        dependent_name_part_t* dependent_part)
{
    if (dependent_part == NULL)
    {
        return NULL;
    }
    else
    {
        dependent_name_part_t* result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);

        // Plain copy
        *result = *dependent_part;

        result->next = copy_dependent_parts(dependent_part->next);
        return result;
    }
}

type_t* get_dependent_typename_type_from_parts(scope_entry_t* dependent_entity, 
        dependent_name_part_t* dependent_parts)
{

    scope_entry_t* most_enclosing_dependent_entity = NULL;

    int nesting_level = 1;

    dependent_name_part_t* dependent_parts_enclosing =
        compute_dependent_parts_of_dependent_symbol(dependent_entity,
                &most_enclosing_dependent_entity,
                &nesting_level);

    if (most_enclosing_dependent_entity != NULL)
        dependent_entity = most_enclosing_dependent_entity;

    if (dependent_parts_enclosing == NULL
            && dependent_parts == NULL)
        return get_user_defined_type(dependent_entity);

    type_t* type_info = get_simple_type();

    type_info->type->kind = STK_TEMPLATE_DEPENDENT_TYPE;
    type_info->type->dependent_entry = dependent_entity;

    ERROR_CONDITION(dependent_entity->kind != SK_TEMPLATE_TYPE_PARAMETER
            && dependent_entity->kind != SK_CLASS
            && dependent_entity->kind != SK_TYPEDEF, 
            "Invalid dependent entity of kind '%d'", dependent_entity->kind);

    if (dependent_parts_enclosing != NULL)
    {
        dependent_name_part_t* tmp = dependent_parts_enclosing;

        while (tmp->next != NULL)
            tmp = tmp->next;
        tmp->next = dependent_parts;

        dependent_parts = dependent_parts_enclosing;
    }

    type_info->type->dependent_parts = dependent_parts;

    type_info->info->is_dependent = 1;

    return type_info;
}

type_t* get_dependent_typename_type(scope_entry_t* dependent_entity, 
        decl_context_t decl_context,
        AST nested_name, 
        AST unqualified_part)
{
    // Sometimes we need to add more dependent types because the referenced
    // dependent entity is actually nested inside dependent parts

    scope_entry_t* most_enclosing_dependent_entity = NULL;

    int nesting_level = 1;

    dependent_name_part_t* dependent_parts_enclosing =
        compute_dependent_parts_of_dependent_symbol(dependent_entity,
                &most_enclosing_dependent_entity,
                &nesting_level);

    if (most_enclosing_dependent_entity != NULL)
        dependent_entity = most_enclosing_dependent_entity;

    dependent_name_part_t* dependent_parts = compute_dependent_parts(
            decl_context,
            nested_name,
            unqualified_part,
            &nesting_level);

    if (dependent_parts_enclosing != NULL)
    {
        dependent_name_part_t* tmp = dependent_parts_enclosing;

        while (tmp->next != NULL)
            tmp = tmp->next;
        tmp->next = dependent_parts;

        dependent_parts = dependent_parts_enclosing;
    }

    return get_dependent_typename_type_from_parts(dependent_entity, 
            dependent_parts);
}


void dependent_typename_get_components(type_t* t, 
        scope_entry_t** dependent_entry, 
        dependent_name_part_t** dependent_parts)
{
    ERROR_CONDITION(!is_dependent_typename_type(t), "This is not a dependent typename", 0);
    t = advance_over_typedefs(t);

    *dependent_entry = t->type->dependent_entry;
    *dependent_parts = t->type->dependent_parts;
}

type_t* get_new_enum_type(decl_context_t decl_context)
{
    _enum_type_counter++;

    type_t* type_info = get_simple_type();

    type_info->type->enum_info = (enum_info_t*) counted_calloc(1, sizeof(*type_info->type->enum_info), &_bytes_due_to_type_system);
    type_info->type->kind = STK_ENUM;
    type_info->type->type_decl_context = decl_context;

    // This is incomplete by default
    type_info->info->is_incomplete = 1;

    return type_info;
}

type_t* get_new_class_type(decl_context_t decl_context, enum class_kind_t class_kind)
{
    _class_type_counter++;

    type_t* type_info = get_simple_type();

    type_info->type->class_info = counted_calloc(1, sizeof(*type_info->type->class_info), &_bytes_due_to_type_system);
    type_info->type->class_info->class_kind = class_kind;
    type_info->type->kind = STK_CLASS;
    type_info->type->type_decl_context = decl_context;

    // This is incomplete by default
    type_info->info->is_incomplete = 1;

    // Initially assume it is an aggregate

    return type_info;
}

enum class_kind_t class_type_get_class_kind(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);

    t = get_actual_class_type(t);

    return t->type->class_info->class_kind;
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
                            template_parameter->entry->file,
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

static type_t* _get_duplicated_function_type(type_t* function_type);
static type_t* _get_duplicated_class_type(type_t* function_type);

type_t* get_new_template_type(template_parameter_list_t* template_parameter_list, type_t* primary_type,
        const char* template_name, decl_context_t decl_context, int line, const char* filename)
{
    _template_type_counter++;

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
        primary_type = _get_duplicated_class_type(primary_type);
    }
    else if (is_function_type(primary_type))
    {
        primary_symbol->kind = SK_FUNCTION;
        primary_type = _get_duplicated_function_type(primary_type);
    }
    else
    {
        internal_error("Invalid templated type\n", 0);
    }
    primary_symbol->type_information = primary_type;
    primary_symbol->decl_context = decl_context;

    primary_symbol->line = line;
    primary_symbol->file = filename;

    primary_type->info->is_template_specialized_type = 1;
    primary_type->template_arguments = compute_arguments_primary(template_parameter_list);
    primary_type->template_parameters = template_parameter_list;
    primary_type->related_template_type = type_info;

    if (template_parameter_list->num_template_parameters != 0)
    {
        set_is_dependent_type(primary_type, 1);
    }
    else
    {
        // If it is zero this means it is a special uninstantiated
        // (non-template) class member
        set_is_dependent_type(primary_type, 0);
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

    type_to_specialize->info->is_template_specialized_type = 1;
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

    ERROR_CONDITION(entry->kind != SK_TEMPLATE
            && entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER,
            "Invalid symbol for a template type", 0);

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
                                 targ_2->expression_context,
                                 deduction_flags_empty()))
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

char has_dependent_template_arguments(template_argument_list_t* template_arguments)
{
    int i;
    for (i = 0; i < template_arguments->num_arguments; i++)
    {
        template_argument_t* curr_argument = template_arguments->argument_list[i];

        if (curr_argument->kind == TAK_TYPE)
        {
            if (is_dependent_type(curr_argument->type))
            {
                return 1;
            }
        }
        else if (curr_argument->kind == TAK_TEMPLATE)
        {
            if (is_named_type(curr_argument->type)
                    && named_type_get_symbol(curr_argument->type)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                return 1;
            }
        }
        else if (curr_argument->kind == TAK_NONTYPE)
        {
            if (expression_is_value_dependent(curr_argument->expression))
            {
                return 1;
            }
        }
    }
    return 0;
}

type_t* template_type_get_matching_specialized_type(type_t* t,
        template_argument_list_t* template_argument_list,
        decl_context_t decl_context)
{
    ERROR_CONDITION(!is_template_type(t), "This is not a template type", 0);

    // Search an existing specialization
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: Searching an existing specialization that matches the requested one\n");
        fprintf(stderr, "TYPEUTILS: There are '%d' specializations of this template type\n", 
                template_type_get_num_specializations(t));
    }
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
                    print_type_str(specialization, decl_context),
                    entry->type_information,
                    entry->file,
                    entry->line);
        }

        if (same_template_argument_list(template_argument_list, arguments)
                // If this template type is 0-parameterized, the primary never matches
                && !(specialization == template_type_get_primary_type(t)
                    && template_type_get_template_parameters(t)->num_template_parameters == 0))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: An existing specialization matches '%s'\n", print_declarator(entry->type_information));
                fprintf(stderr, "TYPEUTILS: Returning template %s %p\n", 
                        print_type_str(specialization, decl_context),
                        entry->type_information);
            }

            if (BITMAP_TEST(decl_context.decl_flags, DF_UPDATE_TEMPLATE_ARGUMENTS))
            {
               entry->type_information->template_arguments = template_argument_list;
            }

            return specialization;
        }
    }
    return NULL;
}

type_t* template_type_get_specialized_type_after_type(type_t* t, 
        template_argument_list_t* template_argument_list,
        template_parameter_list_t *template_parameters, 
        type_t* after_type,
        decl_context_t decl_context, 
        int line, const char* filename)
{
    type_t* existing_spec = template_type_get_matching_specialized_type(t, 
            template_argument_list, 
            decl_context);

    if (existing_spec != NULL)
    {
        return existing_spec;
    }

    char has_dependent_temp_args = has_dependent_template_arguments(template_argument_list);

    type_t* specialized_type = after_type;

    scope_entry_t* primary_symbol = named_type_get_symbol(t->type->primary_specialization);

    if (primary_symbol->kind == SK_CLASS)
    {
        if (specialized_type == NULL)
        {
            specialized_type = get_new_class_type(primary_symbol->decl_context,
                    class_type_get_class_kind(
                        get_actual_class_type(primary_symbol->type_information)));
        }
        else
        {
            specialized_type = _get_duplicated_class_type(specialized_type);
        }

        class_type_set_enclosing_class_type(specialized_type,
                class_type_get_enclosing_class_type(primary_symbol->type_information));
    }
    else if (primary_symbol->kind == SK_FUNCTION)
    {
        decl_context_t updated_context 
            = update_context_with_template_arguments(primary_symbol->decl_context, template_argument_list);
        type_t* updated_function_type = update_type(primary_symbol->type_information, updated_context, filename, line);

        // If we cannot update the type, give up, as probably this is SFINAE 
        if (updated_function_type == NULL)
            return NULL;

        // This will give us a new function type
        if (specialized_type == NULL)
        {
            specialized_type = _get_duplicated_function_type(updated_function_type);
        }
    }
    else
    {
        internal_error("Invalid templated type", 0);
    }

    // State that this is a template specialized type
    specialized_type->info->is_template_specialized_type = 1;
    specialized_type->template_arguments = template_argument_list;
    // This can be NULL
    specialized_type->template_parameters = template_parameters;
    specialized_type->related_template_type = t;

    // State the class type nature
    if (primary_symbol->kind == SK_CLASS)
    {
        type_t* enclosing_class_type = class_type_get_enclosing_class_type(specialized_type);
        if (has_dependent_temp_args
                || (t->type->related_template_symbol != NULL
                    && t->type->related_template_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
                || (enclosing_class_type != NULL
                    && is_dependent_type(enclosing_class_type)))
        {
            set_is_dependent_type(specialized_type, /* is_dependent */ 1);
        }
        else
        {
            set_is_dependent_type(specialized_type, /* is_dependent */ 0);
        }
    }
    else
    {
        if (has_dependent_temp_args)
        {
            set_is_dependent_type(specialized_type, /* is_dependent */ 1);
        }
        else
        {
            set_is_dependent_type(specialized_type, /* is_dependent */ 0);
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
    specialized_symbol->point_of_declaration = primary_symbol->point_of_declaration;

    // Keep information of the entity except for template_is_declared which
    // must be cleared at this point
    specialized_symbol->entity_specs = primary_symbol->entity_specs;
    specialized_symbol->entity_specs.is_user_declared = 0;
    
    // Remove the extra template-scope we got from the primary one
    // specialized_symbol->decl_context.template_scope = 
    //     specialized_symbol->decl_context.template_scope->contained_in;

    P_LIST_ADD(t->type->specialized_types, t->type->num_specialized_types, 
            get_user_defined_type(specialized_symbol));

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: No existing specialization matches for '%s', creating a fresh one of type '%s' %p\n",
                primary_symbol->symbol_name,
                print_declarator(specialized_symbol->type_information),
                specialized_symbol->type_information);
    }

    return get_user_defined_type(specialized_symbol);
}


type_t* template_type_get_specialized_type(type_t* t, 
        template_argument_list_t* template_argument_list,
        template_parameter_list_t *template_parameters, 
        decl_context_t decl_context, 
        int line, const char* filename)
{
    return template_type_get_specialized_type_after_type(t,
            template_argument_list,
            template_parameters,
            /* after_type */ NULL /* It will create an empty one */,
            decl_context,
            line, filename);
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

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: Updating template parameters\n");
    }

    int i;

    for (i = 0; i < template_parameters->num_template_parameters; i++)
    {
        template_parameter_t* template_parameter = template_parameters->template_parameters[i];
        template_parameter_t* new_template_parameter = new_template_parameters->template_parameters[i];

        ERROR_CONDITION ((new_template_parameter->kind != template_parameter->kind),
                        "Template parameter kinds do not match", 0);

        if (new_template_parameter->has_default_argument
                && !template_parameter->has_default_argument)
        {
            // Update the template parameter
            template_parameter->has_default_argument = 1;
            template_parameter->default_template_argument = new_template_parameter->default_template_argument;
        }
    }
}

char is_template_specialized_type(type_t* t)
{
    return (t != NULL && t->info->is_template_specialized_type);
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

type_t* get_complex_type(type_t* t)
{
    static rb_red_blk_tree *_complex_hash = NULL;

    if (_complex_hash == NULL)
    {
        _complex_hash = rb_tree_create(integer_comp, null_dtor, null_dtor);
    }

    type_t* result = rb_tree_query_type(_complex_hash, t);

    if (result == NULL)
    {
        result = get_simple_type();

        result->type->kind = STK_COMPLEX;
        result->type->complex_element = t;
        // FIXME - A complex is always twice its base type?
        result->info->size = 2 * t->info->size;
        result->info->alignment = t->info->alignment;
        result->info->valid_size = 1;

        rb_tree_add(_complex_hash, t, result);
    }

    return result;
}

type_t* complex_type_get_base_type(type_t* t)
{
    ERROR_CONDITION(!is_complex_type(t), "This is not a complex type", 0);

    t = advance_over_typedefs(t);

    return t->type->complex_element;
}

static rb_red_blk_tree *_qualification[(CV_CONST | CV_VOLATILE | CV_RESTRICT) + 1];
static void init_qualification_hash(void)
{
    static char _qualif_hash_initialized = 0;

    if (!_qualif_hash_initialized)
    {
        int i;
        for (i = 0; 
                i < (int)((CV_CONST|CV_VOLATILE|CV_RESTRICT) + 1);
                i++)
        {
            _qualification[i] = rb_tree_create(integer_comp, null_dtor, null_dtor);
        }
        _qualif_hash_initialized = 1;
    }
}

static void _get_array_type_components(type_t* array_type, 
        AST *whole_size, AST *lower_bound, AST *upper_bound, decl_context_t* decl_context);

static type_t* _get_array_type(type_t* element_type, 
        AST whole_size, AST lower_bound, AST upper_bound, decl_context_t decl_context);

static type_t* _clone_array_type(type_t* array_type, type_t* new_element_type)
{
        AST whole_size = NULL;
        AST lower_bound = NULL;
        AST upper_bound = NULL;
        decl_context_t decl_context;
        memset(&decl_context, 0, sizeof(decl_context));

        _get_array_type_components(array_type, &whole_size, &lower_bound, &upper_bound, &decl_context);

        // And now rebuild the array type
        type_t* result = _get_array_type(new_element_type, 
                ast_copy_for_instantiation(whole_size), 
                ast_copy_for_instantiation(lower_bound), 
                ast_copy_for_instantiation(upper_bound), 
                decl_context);
        return result;
}


type_t* get_unqualified_type(type_t* t)
{
    t = advance_over_typedefs(t);

    if (t->kind == TK_ARRAY)
    {
        type_t* element_unqualif = get_unqualified_type(t->array->element_type);
        return _clone_array_type(t, element_unqualif);
    }

    ERROR_CONDITION(t->unqualified_type == NULL, "This cannot be NULL", 0);
    return t->unqualified_type;
}

static
type_t* get_qualified_type(type_t* original, cv_qualifier_t cv_qualification)
{
    // Ensure it is initialized
    init_qualification_hash();

    original = advance_over_typedefs_with_cv_qualif(original, NULL);

    ERROR_CONDITION(original == NULL, "This cannot be NULL", 0);
    ERROR_CONDITION(original->unqualified_type == NULL, "This cannot be NULL", 0);

    // The standard forces us to do some strange things here
    if (original->kind == TK_ARRAY)
    {
        // Now clone the type
        type_t* qualif_element_type = get_qualified_type(original->array->element_type, cv_qualification);
        return _clone_array_type(original, qualif_element_type);
    }

    if (cv_qualification == CV_NONE)
    {
        return original->unqualified_type;
    }

    // Lookup based on the unqualified type
    type_t* qualified_type = (type_t*)rb_tree_query_type(
            _qualification[(int)(cv_qualification)], 
            original->unqualified_type);

    if (qualified_type == NULL)
    {
        _qualified_type_counter++;
        qualified_type = new_empty_type();
        *qualified_type = *original;
        qualified_type->cv_qualifier = cv_qualification;
        qualified_type->unqualified_type = original->unqualified_type;

        rb_tree_add(_qualification[(int)(cv_qualification)], 
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
    ERROR_CONDITION(t == NULL, "Invalid NULL type", 0);

    static rb_red_blk_tree *_pointer_types = NULL;

    if (_pointer_types == NULL)
    {
        _pointer_types = rb_tree_create(integer_comp, null_dtor, null_dtor);
    }

    type_t* pointed_type = rb_tree_query_type(_pointer_types, t);

    if (pointed_type == NULL)
    {
        _pointer_type_counter++;
        pointed_type = new_empty_type();
        pointed_type->kind = TK_POINTER;
        pointed_type->unqualified_type = pointed_type;
        pointed_type->pointer = counted_calloc(1, sizeof(*pointed_type->pointer), &_bytes_due_to_type_system);
        pointed_type->pointer->pointee = t;

        if (is_function_type(t))
        {
            pointed_type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_function_pointer;
            pointed_type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_function_pointer;
        }
        else
        {
            pointed_type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_pointer;
            pointed_type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_pointer;
        }

        pointed_type->info->valid_size = 1;

        pointed_type->info->is_dependent = is_dependent_type(t);

        rb_tree_add(_pointer_types, t, pointed_type);
    }

    return pointed_type;
}

static rb_red_blk_tree *_lvalue_reference_types = NULL;
static rb_red_blk_tree *_rvalue_reference_types = NULL;

static type_t* get_internal_reference_type(type_t* t, char is_rvalue_ref)
{
    C_LANGUAGE()
    {
        internal_error("No referenced types should be created in C", 0);
    }

    ERROR_CONDITION(t == NULL, "Invalid reference type", 0);

    if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        internal_error("Trying to create a reference to reference type." , 0);
    }

    ERROR_CONDITION(t == NULL,
            "Trying to create a reference of a null type", 0);

    rb_red_blk_tree **reference_types = NULL;
    if (is_rvalue_ref)
    {
        reference_types = &_lvalue_reference_types;
    }
    else
    {
        reference_types = &_rvalue_reference_types;
    }

    if ((*reference_types) == NULL)
    {
        (*reference_types) = rb_tree_create(integer_comp, null_dtor, null_dtor);
    }

    type_t* referenced_type = rb_tree_query_type((*reference_types), t);

    if (referenced_type == NULL)
    {
        _reference_type_counter++;
        referenced_type = new_empty_type();
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

        referenced_type->info->is_dependent = is_dependent_type(t);

        rb_tree_add((*reference_types), t, referenced_type);
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
    ERROR_CONDITION(t == NULL, "Invalid NULL type", 0);

    static rb_red_blk_tree *_class_types = NULL;

    if (_class_types == NULL)
    {
        _class_types = rb_tree_create(integer_comp, null_dtor, null_dtor);
    }

    // First lookup using the class symbol
    rb_red_blk_tree * class_type_hash = NULL;
    rb_red_blk_node * n = rb_tree_query(_class_types, class_entry);
    if (n != NULL)
    {
        class_type_hash = rb_node_get_info(n);
    }

    if (class_type_hash == NULL)
    {
        class_type_hash = rb_tree_create(integer_comp, null_dtor, null_dtor);

        rb_tree_add(_class_types, class_entry, class_type_hash);
    }

    type_t* pointer_to_member = rb_tree_query_type(class_type_hash, t);

    if (pointer_to_member == NULL)
    {
        _pointer_to_member_type_counter++;
        pointer_to_member = new_empty_type();
        pointer_to_member->kind = TK_POINTER_TO_MEMBER;
        pointer_to_member->unqualified_type = pointer_to_member;
        pointer_to_member->pointer = counted_calloc(1, sizeof(*pointer_to_member->pointer), &_bytes_due_to_type_system);
        pointer_to_member->pointer->pointee = t;
        pointer_to_member->pointer->pointee_class = class_entry;

        if (is_function_type(t))
        {
            pointer_to_member->info->size 
                = CURRENT_CONFIGURATION->type_environment->sizeof_pointer_to_member_function;
            pointer_to_member->info->alignment
                = CURRENT_CONFIGURATION->type_environment->alignof_pointer_to_member_function;
        }
        else
        {
            pointer_to_member->info->size 
                = CURRENT_CONFIGURATION->type_environment->sizeof_pointer_to_data_member;
            pointer_to_member->info->alignment
                = CURRENT_CONFIGURATION->type_environment->alignof_pointer_to_data_member;
        }

        pointer_to_member->info->valid_size = 1;

        pointer_to_member->info->is_dependent = is_dependent_type(t) 
            || class_entry->kind == SK_TEMPLATE_TYPE_PARAMETER 
            || is_dependent_type(class_entry->type_information);

        rb_tree_add(class_type_hash, t, pointer_to_member);
    }

    return pointer_to_member;
}

typedef struct array_sized_hash
{
    _size_t whole_size;
    _size_t lower_bound;
    _size_t upper_bound;
    rb_red_blk_tree *element_hash;
} array_sized_hash_t;

static array_sized_hash_t *_array_sized_hash = NULL;
static int _array_sized_hash_size = 0;

static rb_red_blk_tree* _init_array_sized_hash(array_sized_hash_t *array_sized_hash_elem, 
        _size_t whole_size,
        _size_t lower_bound,
        _size_t upper_bound)
{
    array_sized_hash_elem->whole_size = whole_size;
    array_sized_hash_elem->lower_bound = lower_bound;
    array_sized_hash_elem->upper_bound = upper_bound;
    array_sized_hash_elem->element_hash = rb_tree_create(integer_comp, null_dtor, null_dtor);

    return array_sized_hash_elem->element_hash;
}

/*
   void *bsearch(const void *key, const void *base,
   size_t nmemb, size_t size,
   int (*compar)(const void *, const void *));
 */

static char tuple_lower_than(
        int a0, int a1,
        int b0, int b1)
{
    if (a0 < b0)
        return 1;
    else if (a0 > b0)
        return 0;
    else
        return a1 < b1;
}

static char triple_lower_than(
        int a0, int a1, int a2,
        int b0, int b1, int b2)
{
    if (a0 < b0)
        return 1;
    else if (a0 > b0)
        return 0;
    else // a0 == b0 
        return tuple_lower_than(a1, a2, b1, b2);
}

int array_hash_compar(const void* v1, const void* v2)
{
    const array_sized_hash_t* a1 = (const array_sized_hash_t*)v1;
    const array_sized_hash_t* a2 = (const array_sized_hash_t*)v2;

    // (x0, y0, z0) < (x1, y1, z1)
    if (triple_lower_than(
                a1->whole_size,
                a1->lower_bound,
                a1->upper_bound,
                a2->whole_size,
                a2->lower_bound,
                a2->upper_bound))
    {
        return -1;
    }
    // (x0, y0, z0) > (x1, y1, z1)
    else if (triple_lower_than(
                a2->whole_size,
                a2->lower_bound,
                a2->upper_bound,
                a1->whole_size,
                a1->lower_bound,
                a1->upper_bound))
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

static rb_red_blk_tree* get_array_sized_hash(_size_t whole_size, _size_t lower_bound, _size_t upper_bound)
{
    array_sized_hash_t key = { .whole_size = whole_size, .lower_bound = lower_bound, .upper_bound = upper_bound };

    array_sized_hash_t* sized_hash = bsearch(&key, 
            _array_sized_hash, _array_sized_hash_size, 
            sizeof(array_sized_hash_t), array_hash_compar);

    if (sized_hash == NULL)
    {
        _array_sized_hash_size++;
        _array_sized_hash = realloc(_array_sized_hash, _array_sized_hash_size * sizeof(array_sized_hash_t));

        rb_red_blk_tree* result = _init_array_sized_hash(&_array_sized_hash[_array_sized_hash_size - 1], 
                whole_size, lower_bound, upper_bound);

        // So we can use bsearch again
        qsort(_array_sized_hash, _array_sized_hash_size, sizeof(array_sized_hash_t), array_hash_compar);

        return result;
    }
    else
    {
        return sized_hash->element_hash;
    }
}

type_t* get_array_type_str(type_t* element_type, const char* dim)
{
    AST expr = ASTLeaf(AST_DIMENSION_STR, NULL, 0, dim);
    return get_array_type(element_type, expr, CURRENT_COMPILED_FILE->global_decl_context);
}

// This is used only for cloning array types
static void _get_array_type_components(type_t* array_type, 
        AST *whole_size, AST *lower_bound, AST *upper_bound, decl_context_t* decl_context)
{
    ERROR_CONDITION((array_type->kind != TK_ARRAY), "Not an array type!", 0);

    *whole_size = array_type->array->whole_size;
    *lower_bound = array_type->array->lower_bound;
    *upper_bound = array_type->array->upper_bound;
    *decl_context = array_type->array->array_expr_decl_context;
}

// This function owns the three trees passed to it (unless they are NULL, of
// course)
static type_t* _get_array_type(type_t* element_type, 
        AST whole_size, AST lower_bound, AST upper_bound, decl_context_t decl_context)
{
    ERROR_CONDITION(element_type == NULL, "Invalid element type", 0);

    ERROR_CONDITION(whole_size != NULL
            && (lower_bound == NULL || upper_bound == NULL),
            "Invalid definition of boundaries for array", 0);

        // Properly set the scope links, as if the expressions belonged to decl_context
    if (lower_bound != NULL)
        scope_link_set(CURRENT_COMPILED_FILE->scope_link, lower_bound, decl_context);
    if (upper_bound != NULL)
        scope_link_set(CURRENT_COMPILED_FILE->scope_link, upper_bound, decl_context);
    if (whole_size != NULL)
        scope_link_set(CURRENT_COMPILED_FILE->scope_link, whole_size, decl_context);

    char expression_sizes_ok = 1;
    char whole_size_is_constant = 0;
    _size_t whole_size_k = 0;

    char lower_bound_is_constant = 0;
    _size_t lower_bound_k = 0;

    char upper_bound_is_constant = 0;
    _size_t upper_bound_k = 0;

    // We try to reuse this type as much as possible
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
    // First try to fold as many trees as possible to free them
    if (whole_size != NULL)
    {
        struct {
            AST *tree;
            char* pred;
            _size_t* value;
        } data[] = 
        {
            { &whole_size, &whole_size_is_constant, &whole_size_k },
            { &lower_bound, &lower_bound_is_constant, &lower_bound_k },
            { &upper_bound, &upper_bound_is_constant, &upper_bound_k },
            { NULL, NULL, NULL }
        };
        
        int i;
        for (i = 0; data[i].tree != NULL; i++)
        {
            if (data[i].tree == NULL)
                continue;
            char check_expr = check_for_expression(*(data[i].tree), decl_context);
            if (check_expr)
            {
                if (expression_is_constant(*(data[i].tree)))
                {
                    *(data[i].pred) = 1;
                    *(data[i].value) = const_value_cast_to_8(
                            expression_get_constant(*(data[i].tree)));
                    AST old = *(data[i].tree);
                    *(data[i].tree) = const_value_to_tree(expression_get_constant(*(data[i].tree)));
                    ast_free(old);
                }
            }
            else
            {
                expression_sizes_ok = 0;
            }
        }
    }

    type_t* result = NULL;

    if (whole_size == NULL)
    {
        // Use the same strategy we use for pointers
        static rb_red_blk_tree *_undefined_array_types = NULL;

        if (_undefined_array_types == NULL)
        {
            _undefined_array_types = rb_tree_create(integer_comp, null_dtor, null_dtor);
        }

        type_t* undefined_array_type = rb_tree_query_type(_undefined_array_types, element_type);
        if (undefined_array_type == NULL)
        {
            _array_type_counter++;
            result = new_empty_type();
            result->kind = TK_ARRAY;
            result->unqualified_type = result;
            result->array = counted_calloc(1, sizeof(*(result->array)), &_bytes_due_to_type_system);
            result->array->element_type = element_type;
            result->array->whole_size = NULL;
            result->array->array_expr_decl_context = decl_context;

            result->info->is_incomplete = 1;

            result->info->is_dependent = is_dependent_type(element_type);

            rb_tree_add(_undefined_array_types, element_type, result);
        }
        else
        {
            result = undefined_array_type;
        }
    }
    else
    {
        if (!CURRENT_CONFIGURATION->disable_sizeof
                && whole_size_is_constant
                && lower_bound_is_constant
                && upper_bound_is_constant)
        {
            rb_red_blk_tree* array_sized_hash = get_array_sized_hash(whole_size_k, lower_bound_k, upper_bound_k);

            type_t* array_type = rb_tree_query_type(array_sized_hash, element_type);

            if (array_type == NULL)
            {
                _array_type_counter++;
                result = new_empty_type();
                result->kind = TK_ARRAY;
                result->unqualified_type = result;
                result->array = counted_calloc(1, sizeof(*(result->array)), &_bytes_due_to_type_system);
                result->array->element_type = element_type;

                result->array->whole_size = whole_size;
                result->array->lower_bound = lower_bound;
                result->array->upper_bound = upper_bound;

                result->array->array_expr_decl_context = decl_context;
                rb_tree_add(array_sized_hash, element_type, result);

                result->info->is_dependent = is_dependent_type(element_type);
            }
            else
            {
                result = array_type;
            }
        }
        else
        {
            _array_type_counter++;
            result = new_empty_type();
            result->kind = TK_ARRAY;
            result->unqualified_type = result;
            result->array = counted_calloc(1, sizeof(*(result->array)), &_bytes_due_to_type_system);
            result->array->element_type = element_type;
            result->array->whole_size = whole_size;
            result->array->lower_bound = lower_bound;
            result->array->upper_bound = upper_bound;
            result->array->array_expr_decl_context = decl_context;

            C_LANGUAGE()
            {
                if (expression_sizes_ok)
                {
                    // This is a VLA
                    // In C++ there are no VLA's but this path can be followed by
                    // dependent arrays
                    result->array->is_vla = 1;
                }
            }

            result->info->is_dependent = is_dependent_type(element_type);

            if (expression_sizes_ok
                    && !result->info->is_dependent
                    && expression_is_value_dependent(whole_size))
            {
                result->info->is_dependent = 1;
            }
        }
    }

    return result;
}

static const_AST get_zero_tree() 
{
    static AST zero_tree = NULL;
    if (zero_tree == NULL)
    {
        zero_tree = ASTLeaf(AST_OCTAL_LITERAL, NULL, 0, "0");
    }
    return zero_tree;
}

static const_AST get_one_tree() 
{
    static AST one_tree = NULL;
    if (one_tree == NULL)
    {
        one_tree = ASTLeaf(AST_DECIMAL_LITERAL, NULL, 0, "1");
    }
    return one_tree;
}

type_t* get_array_type(type_t* element_type, AST whole_size, decl_context_t decl_context)
{
    whole_size = ast_copy_for_instantiation(whole_size);

    AST lower_bound = NULL; 
    AST upper_bound = NULL; 
    if (whole_size != NULL)
    {
        lower_bound = ast_copy_for_instantiation(get_zero_tree());
        ast_set_filename(lower_bound, ASTFileName(whole_size));
        ast_set_line(lower_bound, ASTLine(whole_size));

        upper_bound = ASTMake2(AST_MINUS_OP, 
                ASTMake1(AST_PARENTHESIZED_EXPRESSION, ast_copy_for_instantiation(whole_size), ASTFileName(whole_size), ASTLine(whole_size), NULL), 
                ast_copy_for_instantiation(get_one_tree()), 
                ASTFileName(whole_size), ASTLine(whole_size), NULL);
    }

    return _get_array_type(element_type, whole_size, lower_bound, upper_bound, decl_context);
}

type_t* get_array_type_bounds(type_t* element_type,
        AST lower_bound,
        AST upper_bound,
        decl_context_t decl_context)
{
    AST whole_size = NULL;
    lower_bound = ast_copy_for_instantiation(lower_bound);
    upper_bound = ast_copy_for_instantiation(upper_bound);

    if (lower_bound != NULL
            && upper_bound != NULL)
    {
        AST one_tree = ast_copy_for_instantiation(get_one_tree());
        ast_set_filename(one_tree, ASTFileName(lower_bound));
        ast_set_line(one_tree, ASTLine(lower_bound));

        whole_size = 
            ASTMake2(AST_ADD_OP, 
                    ASTMake1(AST_PARENTHESIZED_EXPRESSION, 
                        ASTMake2(AST_MINUS_OP, 
                            ast_copy_for_instantiation(upper_bound), 
                            ast_copy_for_instantiation(lower_bound), 
                            ASTFileName(lower_bound), ASTLine(lower_bound), NULL),
                        ASTFileName(lower_bound), ASTLine(lower_bound), NULL),
                    one_tree,
                    ASTFileName(lower_bound), ASTLine(lower_bound), NULL);
    }

    return _get_array_type(element_type, whole_size, lower_bound, upper_bound, decl_context);
}

type_t* get_vector_type(type_t* element_type, unsigned int vector_size)
{
    ERROR_CONDITION(element_type == NULL, "Invalid type", 0);

    _vector_type_counter++;
    // This type is not efficiently managed
    type_t* result = new_empty_type();
    
    result->kind = TK_VECTOR;
    result->unqualified_type = result;

    result->vector = counted_calloc(1, sizeof(*(result->vector)), &_bytes_due_to_type_system);
    result->vector->element_type = element_type;
    result->vector->vector_size = vector_size;

    result->info->is_dependent = is_dependent_type(element_type);

    return result;
}

char is_vector_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_VECTOR);
}

type_t* get_generic_vector_type(type_t* element_type)
{
    return get_vector_type(element_type, 0);
}

char is_generic_vector_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_VECTOR
	    && t->vector->vector_size == 0);
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

static type_t* _get_new_function_type(type_t* t, parameter_info_t* parameter_info, int num_parameters)
{
    _function_type_counter++;

    type_t* result = new_empty_type();

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

    // Technically this is not valid, but we will allow it in C
    C_LANGUAGE()
    {
        result->info->size = 1;
        result->info->alignment = 1;
        result->info->valid_size = 1;
    }

    return result;
}

static type_t* _get_duplicated_class_type(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type!", 0);

    type_t* result = counted_calloc(1, sizeof(*result), &_bytes_due_to_type_system);
    *result = *class_type;

    // These are the parts relevant for duplication
    result->info = counted_calloc(1, sizeof(*result->info), &_bytes_due_to_type_system);
    *result->info = *class_type->info;

    result->type = counted_calloc(1, sizeof(*result->type), &_bytes_due_to_type_system);
    *result->type = *class_type->type;

    result->type->class_info = counted_calloc(1, sizeof(*result->type->class_info), &_bytes_due_to_type_system);
    *result->type->class_info = *class_type->type->class_info;

    return result;
}

static type_t* _get_duplicated_function_type(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type!", 0);

    function_type = advance_over_typedefs(function_type);

    int num_parameters = function_type->function->num_parameters;
    parameter_info_t parameter_list[num_parameters];
    
    int i;
    for (i = 0; i < num_parameters; i++)
    {
        parameter_list[i] = *(function_type->function->parameter_list[i]);
    }

    type_t* result = _get_new_function_type(
            function_type->function->return_type,
            parameter_list,
            num_parameters);

    // Preserve the cv qualifier
    result = get_cv_qualified_type(result, get_cv_qualifier(function_type));

    return result;
}

type_t* get_new_function_type(type_t* t, parameter_info_t* parameter_info, int num_parameters)
{
    static type_trie_t *_no_type_functions = NULL;
    static type_trie_t *_functions = NULL;

    type_trie_t* used_trie = NULL;

    if (t == NULL)
    {
        if (_no_type_functions == NULL)
        {
            _no_type_functions = allocate_type_trie();
        }

        used_trie = _no_type_functions;
    }
    else
    {
        if (_functions == NULL)
        {
            _functions = allocate_type_trie();
        }

        used_trie = _functions;
    }

    const type_t* type_seq[num_parameters + 1];
    //  Don't worry, this 'void' is just for the trie
    type_seq[0] = (t != NULL ? t : get_void_type());

    char fun_type_is_dependent = 0;

    if (t != NULL)
    {
        fun_type_is_dependent = is_dependent_type(t);
    }

    int i;
    for (i = 0; i < num_parameters; i++)
    {
        if (!parameter_info[i].is_ellipsis)
        {
            if (parameter_info[i].nonadjusted_type_info != NULL)
            {
                type_seq[i + 1] = parameter_info[i].nonadjusted_type_info;
            }
            else
            {
                type_seq[i + 1] = parameter_info[i].type_info;
            }

            fun_type_is_dependent = 
                fun_type_is_dependent || 
                is_dependent_type(parameter_info[i].type_info);
        }
        else
        {
            // This type is just for the trie 
            type_seq[i + 1] = get_ellipsis_type();
        }
    }

    // Cast to drop 'const'
    type_t* function_type = (type_t*)lookup_type_trie(used_trie, 
            type_seq, num_parameters + 1);

    if (function_type == NULL)
    {
        type_t* new_funct_type = _get_new_function_type(t, parameter_info, num_parameters);
        insert_type_trie(used_trie, type_seq, num_parameters + 1, new_funct_type);
        function_type = new_funct_type;

        set_is_dependent_type(function_type, fun_type_is_dependent);
    }
    else
    {
        _function_type_reused++;
    }
    
    return function_type;
}

type_t* get_nonproto_function_type(type_t* t, int num_parameters)
{
    // This type is not efficiently managed
    type_t* result = new_empty_type();

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

type_t* function_type_get_nonadjusted_parameter_type_num(type_t* function_type, int num_param)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    function_type = advance_over_typedefs(function_type);

    ERROR_CONDITION(num_param >= function_type->function->num_parameters, 
            "Requested parameter %d out of bounds (number of parameters is %d)", 
            num_param, function_type->function->num_parameters);

    type_t* result = function_type->function->parameter_list[num_param]->nonadjusted_type_info;

    // Should the user not have provided an original type (since it is not
    // required to build a function type) return the adjusted one
    if (result == NULL)
    {
        result = function_type->function->parameter_list[num_param]->type_info;
    }

    return result;
}

char class_type_is_incomplete_dependent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->info->is_template_specialized_type
        && t->info->is_dependent
        && t->info->is_incomplete;
}

char class_type_is_complete_dependent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->info->is_template_specialized_type
        && t->info->is_dependent
        && !t->info->is_incomplete;
}

char class_type_is_incomplete_independent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->info->is_template_specialized_type
        && !t->info->is_dependent
        && t->info->is_incomplete;
}

char class_type_is_complete_independent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->info->is_template_specialized_type
        && !t->info->is_dependent
        && !t->info->is_incomplete;
}

char class_type_is_empty(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "Invalid class type", 0);

    // If the class is dynamic it cannot empty
    if (class_type_is_dynamic(t))
        return 0;

    type_t* class_type = get_actual_class_type(t);

    int num_of_non_empty_nonstatics_data_members = 0;

    int i;
    for (i = 0; i < class_type_get_num_nonstatic_data_members(class_type); i++)
    {
        scope_entry_t* entry = class_type_get_nonstatic_data_member_num(class_type, i);

        if (!entry->entity_specs.is_bitfield
                || const_value_is_nonzero(expression_get_constant(entry->entity_specs.bitfield_expr)))
        {
            num_of_non_empty_nonstatics_data_members++;
        }
    }

    char has_virtual_bases = 0;

    char has_nonempty_bases = 0;

    for (i = 0; i < class_type_get_num_bases(class_type); i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        scope_entry_t* base_class = class_type_get_base_num(class_type, i, &is_virtual, &is_dependent);

        if (is_dependent)
            continue;

        has_virtual_bases |= is_virtual;

        has_nonempty_bases |= !class_type_is_empty(base_class->type_information);
    }

    return (num_of_non_empty_nonstatics_data_members == 0
            && !has_virtual_bases
            && !has_nonempty_bases);
}

char class_type_is_abstract(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type!", 0);

    return class_type->type->class_info->is_abstract;
}

void class_type_set_is_abstract(type_t* class_type, char is_abstract)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type!", 0);

    class_type->type->class_info->is_abstract = is_abstract;
}

char class_type_is_dynamic(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type!", 0);

    type_t* class_type = get_actual_class_type(t);

    int num_virtuals = class_type_get_num_virtual_functions(class_type);

    // If we have virtual functions we are dynamic
    if (num_virtuals != 0)
        return 1;

    // If our destructor is dynamic, we are dynamic
    scope_entry_t* destructor = class_type_get_destructor(class_type);

    if (destructor != NULL
            && destructor->entity_specs.is_virtual)
        return 1;

    // If any of our bases is dynamic or a virtual base, we are dynamic
    int num_bases = class_type_get_num_bases(class_type);
    int i;
    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        scope_entry_t* base_class = class_type_get_base_num(class_type, i, &is_virtual, &is_dependent);

        if (is_dependent)
            continue;

        if (is_virtual
                || class_type_is_dynamic(base_class->type_information))
            return 1;
    }

    return 0;
}

#define MAX_CLASS_TYPES (256)

static char has_non_virtual_empty_base_class_not_zero_offset_rec(type_t* class_type,
        type_t* list[MAX_CLASS_TYPES], int num_elems)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "Invalid unnamed class type", 0);

    int i;
    for (i = 0; i < class_type_get_num_bases(class_type); i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        scope_entry_t* base_class = class_type_get_base_num(class_type, i, &is_virtual, &is_dependent);

        if (is_dependent)
            continue;

        // If not morally virtual and empty
        if (!is_virtual
                && class_type_is_empty(base_class->type_information))
        {
            ERROR_CONDITION(num_elems == MAX_CLASS_TYPES,
                    "Too deep hierarchy > %d\n", MAX_CLASS_TYPES);

            int j;
            for (j = 0; j < num_elems; j++)
            {
                if (equivalent_types(base_class->type_information, list[j]))
                {
                    return 1;
                }
            }
            list[num_elems] = base_class->type_information;

            if (has_non_virtual_empty_base_class_not_zero_offset_rec(
                    get_actual_class_type(base_class->type_information),
                    list, num_elems + 1))
                return 1;
        }
    }

    return 0;
}

static char has_non_virtual_empty_base_class_not_zero_offset(type_t* class_type)
{
    ERROR_CONDITION(!is_class_type(class_type),
            "This is not a class type", 0);

    type_t* list[MAX_CLASS_TYPES] = { 0 };

    return has_non_virtual_empty_base_class_not_zero_offset_rec(
            get_actual_class_type(class_type), 
            list, /* num_elems */ 0);
}

#undef MAX_CLASS_TYPES

char class_type_is_nearly_empty(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type!", 0);

    // A nearly empty class must be dynamic
    if (!class_type_is_dynamic(t))
        return 0;

    type_t* class_type = get_actual_class_type(t);

    int i;
    for (i = 0; i < class_type_get_num_nonstatic_data_members(class_type); i++)
    {
        scope_entry_t* entry = class_type_get_nonstatic_data_member_num(class_type, i);

        if (!entry->entity_specs.is_bitfield
                || const_value_is_nonzero(expression_get_constant(entry->entity_specs.bitfield_expr)))
        {
            // If we are not empty, we are not nearly empty either
            return 0;
        }
    }

    // This is implemented likewise it is in GCC
    char seen_non_virtual_nearly_empty = 0;

    for (i = 0; i < class_type_get_num_bases(class_type); i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        scope_entry_t* base_class = class_type_get_base_num(class_type, i, &is_virtual, &is_dependent);

        if (is_dependent)
            continue;

        if (!is_virtual)
        {
            if (class_type_is_nearly_empty(base_class->type_information))
            {
                if (!seen_non_virtual_nearly_empty)
                    // At most one direct non-virtual nearly empty base class is
                    // allowed in a nearly empty class
                    seen_non_virtual_nearly_empty = 1;
                else
                    // More than one direct non-virtual nearly empty base class makes
                    // this class not a nearly empty class
                    return 0;
            }
            else if (!class_type_is_empty(base_class->type_information))
                return 0;
        }
    }

    // Now we have to check that no empty base gets laid out at a nonzero
    // offset.  This only can happen if a base class type that is empty appears
    // twice in the hierarchy and it is not morally virtual
    if (has_non_virtual_empty_base_class_not_zero_offset(class_type))
        return 0;

    return 1;
}

char class_type_get_is_dependent(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->info->is_dependent;
}

void class_type_set_enclosing_class_type(type_t* t, type_t* enclosing_class_type)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);

    ERROR_CONDITION(enclosing_class_type != NULL 
            && !is_class_type(enclosing_class_type), "This is not a class type", 0);

    t = get_actual_class_type(t);

    t->type->class_info->enclosing_class_type = enclosing_class_type;
}

type_t* class_type_get_enclosing_class_type(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);

    t = get_actual_class_type(t);

    return t->type->class_info->enclosing_class_type;
}

int class_type_get_num_bases(type_t* class_type)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);
    class_info_t* class_info = class_type->type->class_info;

    return class_info->num_bases;
}

void class_type_add_constructor(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    P_LIST_ADD_ONCE(class_type->type->class_info->constructor_list, class_type->type->class_info->num_constructors, entry);
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

void class_type_set_default_constructor(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    class_type->type->class_info->default_constructor = entry;
}

scope_entry_t* class_type_get_default_constructor(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    return class_type->type->class_info->default_constructor;
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
    P_LIST_ADD_ONCE(class_type->type->class_info->copy_assignment_operator_function_list, 
            class_type->type->class_info->num_copy_assignment_operator_functions, entry);
}

int class_type_get_num_move_assignment_operators(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    return class_type->type->class_info->num_move_assignment_operator_functions;
}

scope_entry_t* class_type_get_move_assignment_operator_num(type_t* class_type, int num)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    return class_type->type->class_info->move_assignment_operator_function_list[num];
}

void class_type_add_move_assignment_operator(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    P_LIST_ADD_ONCE(class_type->type->class_info->move_assignment_operator_function_list, 
            class_type->type->class_info->num_move_assignment_operator_functions, entry);
}

void class_type_add_copy_constructor(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    P_LIST_ADD_ONCE(class_type->type->class_info->copy_constructor_list,
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

void class_type_add_move_constructor(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    P_LIST_ADD_ONCE(class_type->type->class_info->move_constructor_list,
            class_type->type->class_info->num_move_constructors, entry);
}

int class_type_get_num_move_constructors(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    return class_type->type->class_info->num_move_constructors;
}

scope_entry_t* class_type_get_move_constructor_num(type_t* class_type, int num)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    return class_type->type->class_info->move_constructor_list[num];
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

void class_type_add_member_function(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    P_LIST_ADD_ONCE(class_type->type->class_info->member_functions, 
            class_type->type->class_info->num_member_functions, entry);
}

void class_type_add_typename(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    ERROR_CONDITION(entry->kind != SK_TYPEDEF
            && entry->kind != SK_CLASS
            && entry->kind != SK_ENUM
            && entry->kind != SK_TEMPLATE,
            "Invalid member typename", 0);

    P_LIST_ADD_ONCE(class_type->type->class_info->typenames, 
            class_type->type->class_info->num_typenames, entry);
}

void class_type_add_member(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    // It may happen that a type is added twice (redeclared classes ...)
    P_LIST_ADD_ONCE(class_type->type->class_info->members, 
            class_type->type->class_info->num_members, entry);
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

char is_enum_type(type_t* t)
{
    return is_unnamed_enumerated_type(t)
        || is_named_enumerated_type(t);
}

char is_unnamed_enumerated_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_ENUM);
}

char is_named_enumerated_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (is_named_type(t)
            && is_unnamed_enumerated_type(named_type_get_symbol(t)->type_information));
}

type_t* get_actual_enum_type(type_t* t)
{
    if (is_unnamed_enumerated_type(t))
        return advance_over_typedefs(t);
    else if (is_named_enumerated_type(t))
        return named_type_get_symbol(advance_over_typedefs(t))->type_information;
    else
        return NULL;
}

void enum_type_add_enumerator(type_t* t, scope_entry_t* enumeration_item)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enum type", 0);

    t = get_actual_enum_type(t);

    simple_type_t* enum_type = t->type;
    P_LIST_ADD(enum_type->enum_info->enumeration_list, 
            enum_type->enum_info->num_enumeration,
            enumeration_item);
}

scope_entry_t* enum_type_get_enumerator_num(type_t* t, int n)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enum type", 0);

    t = get_actual_enum_type(t);

    simple_type_t* enum_type = t->type;
    return enum_type->enum_info->enumeration_list[n];
}

int enum_type_get_num_enumerators(type_t* t)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enum type", 0);
    t = get_actual_enum_type(t);

    simple_type_t* enum_type = t->type;

    return enum_type->enum_info->num_enumeration;
}

type_t* enum_type_get_underlying_type(type_t* t)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enum type", 0);

    t = get_actual_enum_type(t);
    simple_type_t* enum_type = t->type;

    return enum_type->enum_info->underlying_type;
}

void enum_type_set_underlying_type(type_t* t, type_t* underlying_type)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enum type", 0);

    t = get_actual_enum_type(t);
    simple_type_t* enum_type = t->type;

    enum_type->enum_info->underlying_type = underlying_type;
}

type_t* advance_over_typedefs_with_cv_qualif(type_t* t1, cv_qualifier_t* cv_qualif)
{
    if (t1 == NULL)
        return NULL;

    if (cv_qualif != NULL)
    {
        *cv_qualif |= t1->cv_qualifier;
    }

    // Advance over typedefs
    while (t1->kind == TK_DIRECT
            && t1->type->kind == STK_INDIRECT
            && t1->type->user_defined_type != NULL
            && t1->type->is_indirect)
    {
        t1 = t1->type->user_defined_type->type_information;
        if (cv_qualif != NULL)
        {
            *cv_qualif |= t1->cv_qualifier;
        }
    }

    // Arrays add the element qualification 
    //
    // Note: DO NOT use is_array because it uses advance_over_typedefs which
    // ends using advance_over_typedefs_with_cv_qualif
    if (t1->kind == TK_ARRAY)
    {
        advance_over_typedefs_with_cv_qualif(t1->array->element_type, cv_qualif);
    }

    return t1;
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

void class_type_add_base_class(type_t* class_type, scope_entry_t* base_class, 
        char is_virtual, char is_dependent)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    base_class_info_t* new_base_class = counted_calloc(1, sizeof(*new_base_class), &_bytes_due_to_type_system);
    new_base_class->class_symbol = base_class;
    /* redundant */ new_base_class->class_type = base_class->type_information;
    new_base_class->is_virtual = is_virtual;
    new_base_class->is_dependent = is_dependent;

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

int class_type_get_num_member_functions(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    return class_type->type->class_info->num_member_functions;
}

scope_entry_t* class_type_get_member_function_num(type_t* class_type, int i)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    return class_type->type->class_info->member_functions[i];
}

scope_entry_t* class_type_get_base_num(type_t* class_type, int num, char *is_virtual, char *is_dependent)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);

    class_info_t* class_info = class_type->type->class_info;

    if (is_virtual != NULL)
    {
        *is_virtual = class_info->base_classes_list[num]->is_virtual;
    }

    if (is_dependent != NULL)
    {
        *is_dependent = class_info->base_classes_list[num]->is_dependent;
    }

    return class_info->base_classes_list[num]->class_symbol;
}

_size_t class_type_get_offset_direct_base(type_t* class_type, scope_entry_t* direct_base)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    class_info_t* class_info = class_type->type->class_info;

    int num_bases = class_info->num_bases;
    int i;
    for (i = 0; i < num_bases; i++)
    {
        if (class_info->base_classes_list[i]->class_symbol == direct_base)
        {
            return class_info->base_classes_list[i]->base_offset;
        }
    }

    internal_error("Unreachable code", 0);
}

void class_type_set_offset_direct_base(type_t* class_type, scope_entry_t* direct_base, _size_t base_offset)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    class_info_t* class_info = class_type->type->class_info;

    int num_bases = class_info->num_bases;
    int i;
    for (i = 0; i < num_bases; i++)
    {
        if (class_info->base_classes_list[i]->class_symbol == direct_base)
        {
            class_info->base_classes_list[i]->base_offset = base_offset;
            return;
        }
    }

    internal_error("Unreachable code", 0);
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

int class_type_get_num_typenames(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    return class_type->type->class_info->num_typenames;
}

scope_entry_t* class_type_get_typename_num(type_t* class_type, int num)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    return class_type->type->class_info->typenames[num];
}

int class_type_get_num_members(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    return class_type->type->class_info->num_members;
}

scope_entry_t* class_type_get_member_num(type_t* class_type, int num)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    return class_type->type->class_info->members[num];
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
        char is_dependent = 0;
        scope_entry_t* class_entry = class_type_get_base_num(class_type, i, 
                /* is_virtual = */ NULL, /* is_dependent */ &is_dependent);
        type_t* base_class_type = class_entry->type_information;

        if (is_dependent)
            continue;

        scope_entry_list_t* base_conversors = class_type_get_all_conversions(base_class_type, decl_context);

        base_result = entry_list_merge(base_result, base_conversors);
    }

    // Now for every conversor of this class, remove it from 'result'
    scope_entry_list_t* this_class_conversors = NULL;
    int num_conversors = class_type_get_num_conversions(class_type);
    for (i = 0; i < num_conversors; i++)
    {
        scope_entry_t* entry = class_type_get_conversion_num(class_type, i);
        // At the same time build the conversor list of this class
        this_class_conversors = entry_list_add(this_class_conversors, entry);
    }

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(base_result);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        char found = 0;
        scope_entry_list_iterator_t* it2 = NULL;
        for (it2 = entry_list_iterator_begin(this_class_conversors);
                !entry_list_iterator_end(it2) && !found;
                entry_list_iterator_next(it2))
        {
            scope_entry_t* entry2 = entry_list_iterator_current(it2);

            found = equivalent_types(entry->type_information, entry2->type_information);
        }
        entry_list_iterator_free(it2);

        if (!found)
        {
            this_class_conversors = entry_list_add(this_class_conversors, entry);
        }
    }
    entry_list_iterator_free(it);

    return this_class_conversors;
}

type_t* advance_over_typedefs(type_t* t1)
{
    cv_qualifier_t cv = CV_NONE;
    t1 = advance_over_typedefs_with_cv_qualif(t1, &cv);

    if (cv != CV_NONE)
        return get_cv_qualified_type(t1, cv);
    else
        return t1;
}

/*
 * States if two types are equivalent. This means that they are the same
 * (ignoring typedefs). Just plain comparison, no standard conversion is
 * performed. cv-qualifiers are relevant for comparison
 */
static char equivalent_simple_types(type_t *t1, type_t *t2);

static type_t* advance_dependent_typename(type_t* t);

char equivalent_types(type_t* t1, type_t* t2)
{
    ERROR_CONDITION( (t1 == NULL || t2 == NULL), "No type can be null here", 0);

    cv_qualifier_t cv_qualifier_t1 = CV_NONE, cv_qualifier_t2 = CV_NONE;

    // Advance over typedefs
    t1 = advance_over_typedefs_with_cv_qualif(t1, &cv_qualifier_t1);
    t2 = advance_over_typedefs_with_cv_qualif(t2, &cv_qualifier_t2);

    if (is_dependent_typename_type(t1)
            || is_dependent_typename_type(t2))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Comparing two different types where one is a dependent typename\n");
        }
        // Try to advance dependent typenames if we are comparing a
        // dependent typename with another non dependent one
        // FIXME - There are cases where this should not be attempted
        if (is_dependent_typename_type(t1))
        {
            t1 = advance_dependent_typename(t1);
        }
        if (is_dependent_typename_type(t2))
        {
            t2 = advance_dependent_typename(t2);
        }
    }

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
        case TK_ERROR:
            // This is always true
            result = 1;
            break;
        case TK_OVERLOAD:
            // This is always false
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
        case STK_INDIRECT :
            result = equivalent_named_types(t1->user_defined_type, 
                    t2->user_defined_type);
            break;
        case STK_TEMPLATE_DEPENDENT_TYPE :
            result = compare_template_dependent_typename_types(p_t1, p_t2);
            break;
        case STK_TYPEOF :
            // internal_error("__typeof__ comparison still not implemented", 0);
            // Nobody compares these structurally, but using the "name" (e.g. the pointer)
            result = (t1 == t2);
            break;
        case STK_VA_LIST :
            // If both are __builtin_va_list, this is trivially true
            result = 1;
            break;
        case STK_COMPLEX:
            return equivalent_types(t1->complex_element, t2->complex_element);
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

    if (t1->whole_size != NULL
            && t2->whole_size != NULL)
    {
        CXX_LANGUAGE()
        {
            if (!same_functional_expression(t1->whole_size, t1->array_expr_decl_context, 
                        t2->whole_size, t2->array_expr_decl_context, deduction_flags_empty()))
                return 0;
        }
        C_LANGUAGE()
        {
            if (expression_is_constant(t1->whole_size)
                    && expression_is_constant(t2->whole_size))
            {
                if( const_value_is_zero(
                            const_value_eq(
                                expression_get_constant(t1->whole_size), 
                                expression_get_constant(t2->whole_size))))
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
        if ((t1->whole_size == NULL
                && t2->whole_size != NULL)
                || (t1->whole_size != NULL
                    && t2->whole_size == NULL))
        {
            return 0;
        }
    }
    
    return 1;
}

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

static const char* get_template_arguments_list_str(template_argument_list_t* template_arguments);

// FIXME - This function looks rather similar to update_dependent_typename, can
// we abstract them away?
static type_t* advance_dependent_typename_aux(
        type_t* original_type,
        type_t* dependent_entry_type,
        dependent_name_part_t* dependent_parts)
{
    if (!is_named_type(dependent_entry_type))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Not a named type, returning it unchanged\n");
        }
        return original_type;
    }

    scope_entry_t* dependent_entry = named_type_get_symbol(dependent_entry_type);

    if (dependent_entry->kind == SK_TEMPLATE_TYPE_PARAMETER
            || dependent_entry->kind == SK_TYPEDEF)
    {
        // No way if this is an involved dependent typename
        return original_type;
    }

    ERROR_CONDITION(dependent_entry->kind != SK_CLASS, "Must be a class-name", 0);
    ERROR_CONDITION(dependent_parts == NULL, "Dependent parts cannot be empty", 0);

    scope_entry_t* current_member = dependent_entry;

    decl_context_t class_context;

    while (dependent_parts->next != NULL)
    {
        ERROR_CONDITION(dependent_parts->related_type != NULL, "Dependent part has a related type", 0);

        class_context = class_type_get_inner_context(current_member->type_information);

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Looking for dependent-part '%s'\n", dependent_parts->name);
        }

        scope_entry_list_t* member_list = query_in_scope_str(class_context, dependent_parts->name);

        if (member_list == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Nothing was found for dependent-part '%s'\n", dependent_parts->name);
            }

            return get_dependent_typename_type_from_parts(current_member, 
                    copy_dependent_parts(dependent_parts));
        }

        if (entry_list_size(member_list) > 1)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Too many symbols where found for '%s'\n", dependent_parts->name);
            }
            return get_dependent_typename_type_from_parts(current_member, 
                    copy_dependent_parts(dependent_parts));
        }

        scope_entry_t* member = entry_list_head(member_list);

        if (member->kind == SK_TYPEDEF)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Got a typedef when looking up dependent-part '%s'\n", dependent_parts->name);
            }
            type_t* advanced_type = advance_over_typedefs(member->type_information);

            if (is_named_class_type(advanced_type))
            {
                member = named_type_get_symbol(advanced_type);
            }
        }

        if (member->kind == SK_CLASS)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Got a class when looking up dependent-part '%s'\n", dependent_parts->name);
            }
            if (dependent_parts->template_arguments != NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: But this part has template arguments, so it is not valid\n");
                }
                return get_dependent_typename_type_from_parts(current_member, 
                        copy_dependent_parts(dependent_parts));
            }

            current_member = member;
        }
        else if (member->kind == SK_TEMPLATE)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Got a template-name when looking up dependent-part '%s'\n", 
                        dependent_parts->name);
            }

            if (dependent_parts->template_arguments == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: But this part does not have template arguments, so it is not valid\n");
                }
                return get_dependent_typename_type_from_parts(current_member, 
                        copy_dependent_parts(dependent_parts));
            }

            // TEMPLATE RESOLUTION
            type_t* template_type = member->type_information;

            // Only template classes
            if (named_type_get_symbol(template_type_get_primary_type(template_type))->kind == SK_FUNCTION)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: The named template is a template function, so it is not valid\n");
                }
                return get_dependent_typename_type_from_parts(current_member, 
                        copy_dependent_parts(dependent_parts));
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Requesting specialization '%s'\n", 
                        dependent_parts->name);
            }

            template_argument_list_t* template_arguments = dependent_parts->template_arguments;

            type_t* specialized_type = template_type_get_specialized_type(
                    template_type,
                    template_arguments,
                    template_type_get_template_parameters(template_type),
                    class_context, 
                    // They should not be needed
                    0, NULL);

            current_member = named_type_get_symbol(specialized_type);

            if (current_member == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: Somehow when requesting a specialization nothing was returned");
                }
                return get_dependent_typename_type_from_parts(current_member, 
                        copy_dependent_parts(dependent_parts));
            }
        }
        else 
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Unexpected symbol for part '%s'\n", dependent_parts->name);
            }
            return get_dependent_typename_type_from_parts(current_member, 
                    copy_dependent_parts(dependent_parts));
        }

        dependent_parts = dependent_parts->next;
    }

    // Last part
    class_context = class_type_get_inner_context(current_member->type_information);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: Looking for last dependent-part '%s'\n", dependent_parts->name);
    }

    scope_entry_list_t* member_list = query_in_scope_str(class_context, dependent_parts->name);

    if (member_list == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Nothing was found for dependent-part '%s'\n", dependent_parts->name);
        }
        return get_dependent_typename_type_from_parts(current_member, 
                copy_dependent_parts(dependent_parts));
    }

    if (entry_list_size(member_list) > 1)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Too many symbols where found for '%s'\n", dependent_parts->name);
        }
        return get_dependent_typename_type_from_parts(current_member, 
                copy_dependent_parts(dependent_parts));
    }

    scope_entry_t* member = entry_list_head(member_list);

    if (member->kind == SK_CLASS
            || member->kind == SK_TYPEDEF)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Got a typename when looking up dependent-part '%s'\n", dependent_parts->name);
        }
        if (dependent_parts->template_arguments != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: But this part has template arguments, so it is not valid\n");
            }
            return get_dependent_typename_type_from_parts(current_member, 
                    copy_dependent_parts(dependent_parts));
        }
        
        if (member->kind == SK_TYPEDEF)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Got a typedef when looking up dependent-part '%s'\n", dependent_parts->name);
            }
            type_t* advanced_type = advance_over_typedefs(member->type_information);

            if (is_named_class_type(advanced_type))
            {
                member = named_type_get_symbol(advanced_type);
            }
        }

        current_member = member;
    }
    else if (member->kind == SK_TEMPLATE)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Got a template-name when looking up dependent-part '%s'\n", 
                    dependent_parts->name);
        }

        if (dependent_parts->template_arguments == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: But this part does not have template arguments, so it is not valid\n");
            }
            return get_dependent_typename_type_from_parts(current_member, 
                    copy_dependent_parts(dependent_parts));
        }

        // TEMPLATE RESOLUTION
        type_t* template_type = member->type_information;

        // Only template classes
        if (named_type_get_symbol(template_type_get_primary_type(template_type))->kind == SK_FUNCTION)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: The named template is a template function, so it is not valid\n");
            }
            return get_dependent_typename_type_from_parts(current_member, 
                    copy_dependent_parts(dependent_parts));
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: requesting specialization '%s'\n", 
                    dependent_parts->name);
        }

        type_t* specialized_type = template_type_get_specialized_type(
                template_type,
                dependent_parts->template_arguments,
                template_type_get_template_parameters(template_type),
                class_context, 
                // They should not be needed
                /*line*/0, /*filename*/NULL);

        current_member = named_type_get_symbol(specialized_type);

    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Unexpected symbol for part '%s'\n", dependent_parts->name);
        }
        return get_dependent_typename_type_from_parts(current_member, 
                copy_dependent_parts(dependent_parts));
    }

    // This looks a bit twisted
    type_t* result_type = 
        advance_over_typedefs(get_user_defined_type(current_member));

    return result_type;
}

static type_t* advance_dependent_typename(type_t* t)
{
    // A dependent typename is a dependent entity followed by a sequence of
    // syntactic bits Advancing them means examining uninstantiated types,
    // which is always a dangerous thing to do
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: Advancing dependent typename '%s'\n", print_declarator(t));
    }

    cv_qualifier_t cv_qualif = get_cv_qualifier(t);

    ERROR_CONDITION(!is_dependent_typename_type(t), "This is not a dependent typename", 0);

    scope_entry_t* dependent_entry = NULL;
    dependent_name_part_t* dependent_parts = NULL;

    dependent_typename_get_components(t, &dependent_entry, &dependent_parts);

    type_t* dependent_entry_type = get_user_defined_type(dependent_entry);

    type_t* result = advance_dependent_typename_aux(t, dependent_entry_type, dependent_parts);
    result = get_qualified_type(result, cv_qualif);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: Type was advanced to '%s'\n",
                print_declarator(result));
    }

    if (is_dependent_typename_type(result))
    {
        scope_entry_t* new_dependent_entry = NULL;
        dependent_typename_get_components(result, &new_dependent_entry, &dependent_parts);

        if (new_dependent_entry != dependent_entry)
        {
            return advance_dependent_typename(result);
        }
    }

    return result;
}

static char syntactic_comparison_of_dependent_parts(
        dependent_name_part_t* dependent_parts_1,
        dependent_name_part_t* dependent_parts_2)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: Comparing (syntactically) nested-name parts '");
        dependent_name_part_t* part = dependent_parts_1;
        while (part != NULL)
        {
            fprintf(stderr, "%s%s%s",
                    part->name,
                    part->template_arguments == NULL ? "" : get_template_arguments_list_str(part->template_arguments),
                    part->next == NULL ? "" : "::");
            part = part->next;
        }
        fprintf(stderr, "' vs '");
        part = dependent_parts_2;
        while (part != NULL)
        {
            fprintf(stderr, "%s%s%s",
                    part->name,
                    part->template_arguments == NULL ? "" : get_template_arguments_list_str(part->template_arguments),
                    part->next == NULL ? "" : "::");
            part = part->next;
        }
        fprintf(stderr, "'\n");
    }

    while (dependent_parts_1 != NULL
            && dependent_parts_2 != NULL)
    {
        if (strcmp(dependent_parts_1->name, dependent_parts_2->name) != 0)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Mismatch of component name '%s' != '%s'\n",
                        dependent_parts_1->name,
                        dependent_parts_2->name);
            }
            return 0;
        }

        if ((dependent_parts_1->template_arguments == NULL
                    && dependent_parts_2->template_arguments != NULL)
                || (dependent_parts_1->template_arguments != NULL
                    && dependent_parts_2->template_arguments == NULL))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Mismatch in the kind of components %s type has template arguments while %s does not\n",
                        dependent_parts_1->template_arguments != NULL ? "first" : "second",
                        dependent_parts_1->template_arguments == NULL ? "first" : "second");
            }
            return 0;
        }
        
        if ((dependent_parts_1->related_type == NULL
                    && dependent_parts_2->related_type != NULL)
                || (dependent_parts_1->related_type != NULL
                    && dependent_parts_2->related_type == NULL))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Mismatch in the kind of components %s type has a related type while %s does not\n",
                        dependent_parts_1->related_type != NULL ? "first" : "second",
                        dependent_parts_1->related_type == NULL ? "first" : "second");
            }
        }

        if (dependent_parts_1->related_type != NULL
                && dependent_parts_2->related_type != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Need to compare related types\n");
            }

            if (!equivalent_types(dependent_parts_1->related_type,
                        dependent_parts_2->related_type))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: Related types do not match\n");
                }
                return 0;
            }
        }

        if (dependent_parts_1->template_arguments != NULL
                    && dependent_parts_2->template_arguments != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Need to compare template arguments\n");
            }

            if (!same_template_argument_list(dependent_parts_1->template_arguments,
                        dependent_parts_2->template_arguments))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: Template arguments do not match\n");
                }
                return 0;
            }
        }

        dependent_parts_1 = dependent_parts_1->next;
        dependent_parts_2 = dependent_parts_2->next;
    }

    if (dependent_parts_1 != NULL
            || dependent_parts_2 != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: One of the nested names is longer than the other\n");
        }
        return 0;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: Both dependent typenames seem the same\n");
    }
    return 1;
}

static char compare_template_dependent_typename_types(type_t* p_t1, type_t* p_t2)
{
    // It is likely that in these contrived cases users will use a typedef
    // to help themselves so most of the time this fast path will be fired
    if (p_t1 == p_t2)
    {
        DEBUG_CODE()
        {
            fprintf(stderr , "TYPEUTILS: Dependent typenames are trivially the same\n");
        }
        return 1;
    }


    DEBUG_CODE()
    {
        fprintf(stderr , "TYPEUTILS: Comparing template dependent typenames '%s' and '%s'\n",
                print_declarator(p_t1),
                print_declarator(p_t2));
    }

    scope_entry_t* dependent_entry_1 = NULL;
    dependent_name_part_t* dependent_parts_1 = NULL;

    dependent_typename_get_components(p_t1, 
            &dependent_entry_1,
            &dependent_parts_1);

    type_t* type_to_compare_1 = NULL;
    if (dependent_entry_1->kind == SK_TEMPLATE_TYPE_PARAMETER)
    {
        type_to_compare_1 = get_user_defined_type(dependent_entry_1);
    }
    else
    {
        type_to_compare_1 = dependent_entry_1->type_information;
    }

    scope_entry_t* dependent_entry_2 = NULL;
    dependent_name_part_t* dependent_parts_2 = NULL;

    dependent_typename_get_components(p_t2, 
            &dependent_entry_2,
            &dependent_parts_2);

    type_t* type_to_compare_2 = NULL;
    if (dependent_entry_2->kind == SK_TEMPLATE_TYPE_PARAMETER)
    {
        type_to_compare_2 = get_user_defined_type(dependent_entry_2);
    }
    else
    {
        type_to_compare_2 = dependent_entry_2->type_information;
    }

    if (equivalent_types(type_to_compare_1, type_to_compare_2))
    {
        return syntactic_comparison_of_dependent_parts(dependent_parts_1, dependent_parts_2);
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Dependent entry is different\n");
        }
        return 0;
    }
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
    return t != NULL 
        && t->kind == TK_DIRECT;
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
            || (is_enum_type(t) && IS_C_LANGUAGE));
}

char is_signed_integral_type(type_t* t)
{
    return is_signed_char_type(t)
        || is_signed_short_int_type(t)
        || is_signed_int_type(t)
        || is_signed_long_int_type(t)
        || is_signed_long_long_int_type(t);
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
            && !t->type->is_short);
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
            && !t->type->is_short);
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
            && t->type->is_short);
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
            && t->type->is_short);
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
            && !t->type->is_short);
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
            && !t->type->is_short);
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
            && !t->type->is_short);
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
            && !t->type->is_short);
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
    C_LANGUAGE()
    {
        // In C, there is no wchar_t, at the moment use a plain 'int'
        return is_signed_int_type(t);
    }

    // C++
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

    return t->array->whole_size;
}

char array_type_is_unknown_size(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->whole_size == NULL;
}

AST array_type_get_array_lower_bound(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    ERROR_CONDITION(array_type_is_unknown_size(t), "Array of unknown size does not have lower bound", 0);
    t = advance_over_typedefs(t);

    return t->array->lower_bound;
}

AST array_type_get_array_upper_bound(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    ERROR_CONDITION(array_type_is_unknown_size(t), "Array of unknown size does not have upper bound", 0);
    t = advance_over_typedefs(t);

    return t->array->upper_bound;
}

decl_context_t array_type_get_array_size_expr_context(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->array_expr_decl_context;
}

char array_type_is_vla(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->is_vla;
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

char is_rvalue_reference_to_class_type(type_t* t1)
{
    return (is_rvalue_reference_type(t1) 
            && is_class_type(reference_type_get_referenced_type(t1)));
}

char is_reference_to_class_type(type_t* t1)
{
    return is_lvalue_reference_to_class_type(t1)
        || is_rvalue_reference_to_class_type(t1);
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


char is_named_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_INDIRECT
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
    return is_integral_type(t) || is_floating_type(t) || is_complex_type(t);
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
            && t->type->kind == STK_COMPLEX);
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
    ERROR_CONDITION(!is_enum_type(t), "This is not an enumerated type", 0);
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
    t = advance_over_typedefs(t);
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

char is_union_type(type_t* possible_union)
{
    if (!is_class_type(possible_union))
        return 0;

    type_t* actual_class = get_actual_class_type(possible_union);

    return (actual_class->type->class_info->class_kind == CK_UNION);
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
            && possible_class->type->kind == STK_INDIRECT
            && possible_class->type->user_defined_type != NULL
            && possible_class->type->user_defined_type->type_information != NULL
            && possible_class->type->user_defined_type->type_information->kind == TK_DIRECT
            && possible_class->type->user_defined_type->type_information->type->kind == STK_CLASS);
}


char class_type_is_base(type_t* possible_base, type_t* possible_derived)
{
    possible_base = get_unqualified_type(advance_over_typedefs(possible_base));
    possible_derived = get_unqualified_type(advance_over_typedefs(possible_derived));

    ERROR_CONDITION(!is_class_type(possible_base)
            || !is_class_type(possible_derived), 
            "This function expects class types", 0);

    if (equivalent_types(possible_base, possible_derived))
        return 1;

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
        char is_dependent = 0;
        type_t* current_base = class_type_get_base_num(possible_derived, i, 
                &is_virtual, &is_dependent)
            ->type_information;

        if (is_dependent)
            continue;

        if (current_base == possible_base)
            return 1;
    }

    // Recursively search in bases of the derived
    for (i = 0; i < class_type_get_num_bases(possible_derived); i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        type_t* current_base = class_type_get_base_num(possible_derived, i, &is_virtual, &is_dependent)
            ->type_information;

        if (is_dependent)
            continue;

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
    if (is_array_type(type_info))
    {
        return get_cv_qualifier(array_type_get_element_type(type_info));
    }
    return type_info->cv_qualifier;
}

type_t* canonical_type(type_t* type)
{
    if (type == NULL)
        return NULL;

    while ((is_named_type(type)
                && named_type_get_symbol(type)->type_information != NULL))
    {
        type = advance_over_typedefs(type);
        if (is_named_type(type)
                && named_type_get_symbol(type)->type_information != NULL)
        {
            type = named_type_get_symbol(type)->type_information;
        }
    }

    return type;
}

char is_dependent_type(type_t* type)
{
    if (type == NULL)
        return 0;

    type = canonical_type(type);

    return type->info->is_dependent;
}

void set_is_dependent_type(type_t* t, char is_dependent)
{
    t = canonical_type(t);
    t->info->is_dependent = is_dependent;
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

    type_info = advance_over_typedefs(type_info);

    char result = 0;
    if (type_info->kind == TK_POINTER_TO_MEMBER
            || type_info->kind == TK_POINTER
            || type_info->kind == TK_LVALUE_REFERENCE
            || type_info->kind == TK_RVALUE_REFERENCE)
    {
        type_t* pointee = type_info->pointer->pointee;

        pointee = advance_over_typedefs(pointee);

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
        case STK_INDIRECT :
            {
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
        case STK_COMPLEX:
            {
                result = strappend(result, "_Complex ");
                result = strappend(result, 
                        get_simple_type_name_string(decl_context, simple_type->complex_element));
                break;
            }
        case STK_CLASS :
            {
                result = uniquestr("class <anonymous>");
                break;
            }
        case STK_ENUM :
            {
                result = uniquestr("enum <anonymous>");
                break;
            }
        case STK_TEMPLATE_DEPENDENT_TYPE :
            {
                // result = prettyprint_in_buffer(simple_type->typeof_expr);
                result = get_qualified_symbol_name(simple_type->dependent_entry,
                        decl_context);

                dependent_name_part_t* parts = simple_type->dependent_parts;
                while (parts != NULL)
                {
                    result = strappend(result, "::");
                    result = strappend(result, parts->name);

                    if (parts->template_arguments != NULL)
                    {
                        result = strappend(result, "< ");
                        int i;
                        for (i = 0; i < parts->template_arguments->num_arguments; i++)
                        {
                            template_argument_t * template_arg = parts->template_arguments->argument_list[i];

                            switch (template_arg->kind)
                            {
                                case TAK_TYPE:
                                    {
                                        result = strappend(result, 
                                                print_type_str(template_arg->type, decl_context));
                                        break;
                                    }
                                case TAK_NONTYPE:
                                    {
                                        result = strappend(result, 
                                                prettyprint_in_buffer(template_arg->expression));
                                        break;
                                    }
                                case TAK_TEMPLATE:
                                    {
                                        result = strappend(result,
                                                get_qualified_symbol_name(named_type_get_symbol(template_arg->type), 
                                                    decl_context));
                                        break;
                                    }
                                default:
                                    {
                                        internal_error("Invalid template argument kind", 0);
                                    }
                            }

                            if ((i + 1) < parts->template_arguments->num_arguments)
                            {
                                result = strappend(result, ", ");
                            }
                        }

                        if (result[strlen(result) - 1] == '>')
                        {
                            result = strappend(result, " ");
                        }

                        result = strappend(result, ">");
                    }

                    parts = parts->next;
                }

                break;
            }
        case STK_TEMPLATE_TYPE:
            {
                result = get_simple_type_name_string(decl_context, simple_type->primary_specialization);
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
    const char* result = "";

    if (type_info == NULL)
        return result;
    
    if (is_unresolved_overloaded_type(type_info))
    {
        result = uniquestr("<unresolved overloaded function type>");
    }
    else if (is_error_type(type_info))
    {
        result = uniquestr("<error-type>");
    }
    else
    {
        result = get_cv_qualifier_string(type_info);
        result = strappend(result, get_simple_type_name_string_internal(decl_context, type_info->type));
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

    type_t* base_type = get_foundation_type(type_info);
    const char* base_type_name = get_simple_type_name_string(decl_context, base_type);
    const char* declarator_name = get_type_name_string(decl_context, type_info, symbol_name, 
            num_parameter_names, parameter_names, is_parameter);

    const char* result;

    result = base_type_name;
    if (strcmp(base_type_name, "") != 0
            && strcmp(declarator_name, "") != 0)
    {
        result = strappend(result, " ");
    }
    result = strappend(result, declarator_name);
    
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

    {
        cv_qualifier_t cv = CV_NONE;
        type_info = advance_over_typedefs_with_cv_qualif(type_info, &cv);
        type_info = get_cv_qualified_type(type_info, cv);
    }

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
                        && (type_info->array->whole_size == NULL))
                {
                    // Get rid of those annoying unbounded arrays
                    // in parameters
                    //
                    // This is not valid, but works most of the time...
                    const char* whole_size = uniquestr("[0]");

                    (*right) = strappend((*right), whole_size);

                    get_type_name_str_internal(decl_context, type_info->array->element_type, left, right, 
                            num_parameter_names, parameter_names, is_parameter);
                }
                else
                {
                    const char* whole_size = strappend("[", prettyprint_in_buffer(type_info->array->whole_size));
                    whole_size = strappend(whole_size, "]");

                    (*right) = strappend((*right), whole_size);

                    get_type_name_str_internal(decl_context, type_info->array->element_type, left, right, 
                            num_parameter_names, parameter_names, is_parameter);
                }
                break;
            }
        case TK_FUNCTION :
            {
                if (type_info->function->return_type != NULL)
                {
                    get_type_name_str_internal(decl_context, type_info->function->return_type, left, right, 
                            num_parameter_names, parameter_names, is_parameter);
                }

                const char* prototype = "";
                int i;
                prototype = strappend(prototype, "(");
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
                prototype = strappend(prototype, ")");
                if (get_cv_qualifier(type_info) != CV_NONE)
                {
                    prototype = strappend(prototype, " ");
                    prototype = strappend(prototype, get_cv_qualifier_string(type_info));
                }

                (*right) = strappend((*right), prototype);
                break;
            }
        case TK_VECTOR :
            {
                char c[256];

                get_type_name_str_internal(decl_context, type_info->vector->element_type, left, right, 
                        num_parameter_names, parameter_names, is_parameter);

                //generic_vector
                if (type_info->vector->vector_size == 0)
                {
                    snprintf(c, 255, "__attribute__((generic_vector)) ");
                }
                else
                {
                    snprintf(c, 255, "__attribute__((vector_size(%d))) ", 
                             type_info->vector->vector_size);
                }
                c[255] = '\0';

                (*left) = strappend((*left), c);
                break;
            }
        case TK_OVERLOAD:
        case TK_ERROR:
            {
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
                snprintf(user_defined_str, MAX_LENGTH, "enum %s {%s:%d}", 
                        get_qualified_symbol_name(user_defined_type, user_defined_type->decl_context),
                        user_defined_type->file,
                        user_defined_type->line);
                break;
            }
        case SK_CLASS :
            {
                snprintf(user_defined_str, MAX_LENGTH, "class %s {%s:%d}", 
                        get_qualified_symbol_name(user_defined_type, user_defined_type->decl_context),
                        user_defined_type->file,
                        user_defined_type->line);
                break;
            }
        case SK_TYPEDEF :
        case SK_VARIABLE :
        case SK_FUNCTION :
        case SK_UNDEFINED :
            {
                type_t* aliased_type = advance_over_typedefs(user_defined_type->type_information);

                snprintf(user_defined_str, MAX_LENGTH, "%s", 
                        print_declarator(aliased_type));
            }
            break;
        case SK_TEMPLATE_TYPE_PARAMETER :
            snprintf(user_defined_str, MAX_LENGTH, "<type-template parameter '%s' (%d,%d) %s:%d>",
                    user_defined_type->symbol_name,
                    user_defined_type->entity_specs.template_parameter_nesting,
                    user_defined_type->entity_specs.template_parameter_position,
                    user_defined_type->file,
                    user_defined_type->line
                    );
            break;
        case SK_TEMPLATE_TEMPLATE_PARAMETER :
            snprintf(user_defined_str, MAX_LENGTH, "<template-template parameter '%s' (%d,%d) %s:%d>",
                    user_defined_type->symbol_name,
                    user_defined_type->entity_specs.template_parameter_nesting,
                    user_defined_type->entity_specs.template_parameter_position,
                    user_defined_type->file,
                    user_defined_type->line
                    );
            break;
        case SK_TEMPLATE_PARAMETER :
            snprintf(user_defined_str, MAX_LENGTH, "<nontype-template parameter '%s' (%d,%d) %s:%d>", 
                    user_defined_type->symbol_name,
                    user_defined_type->entity_specs.template_parameter_nesting,
                    user_defined_type->entity_specs.template_parameter_position,
                    user_defined_type->file,
                    user_defined_type->line);
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
            snprintf(user_defined_str, MAX_LENGTH, "<<<unknown user defined type>>> (kind=%d)", user_defined_type->kind);
    }
    result = strappend(result, user_defined_str);

    return result;
}

// Prints the template arguments, this routine is for debugging
static const char* get_template_arguments_list_str(template_argument_list_t* template_arguments)
{
    const char* result = "";
    result = strappend(result, "< ");
    int i;
    for (i = 0; i < template_arguments->num_arguments; i++)
    {
        template_argument_t* template_argument = 
            template_arguments->argument_list[i];

        char c[256];
        snprintf(c, 255, "[[%d, %d]] ", 
                template_argument->nesting,
                template_argument->position);

        result = strappend(result, uniquestr(c));

        switch (template_argument->kind)
        {
            case TAK_TYPE:
            case TAK_TEMPLATE:
                {
                    result = strappend(result, 
                            print_declarator(template_argument->type));
                    break;
                }
            case TAK_NONTYPE:
                {
                    result = strappend(result, 
                            prettyprint_in_buffer(template_argument->expression));
                    break;
                }
            default:
                {
                    result = strappend(result,
                            " << unknown template argument >> ");
                    break;
                }
        }
        if ((i + 1) < template_arguments->num_arguments)
        {
            result = strappend(result, ", ");
        }
    }
    result = strappend(result, "> ");

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

    if (simple_type_info->is_signed)
    {
        result = strappend(result, "signed ");
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
        case STK_COMPLEX:
            {
                result = strappend(result, "_Complex ");
                result = strappend(result, print_declarator(simple_type_info->complex_element));
                break;
            }
        case STK_INDIRECT :
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
                    type_t* actual_class = type_info;
                    if (actual_class->info->is_template_specialized_type
                            && actual_class->template_arguments != NULL)
                    {
                        template_arguments = get_template_arguments_list_str(
                                actual_class->template_arguments);
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
            result = strappend(result, "__typeof__(");
            result = strappend(result, prettyprint_in_buffer(simple_type_info->typeof_expr));
            result = strappend(result, ")");
            break;
        case STK_TEMPLATE_DEPENDENT_TYPE :
            {
                // snprintf(c, 255, "<template dependent type [%s]::%s%s>", 
                //         get_named_simple_type_name(simple_type_info->dependent_entry),
                //         prettyprint_in_buffer(simple_type_info->dependent_nested_name),
                //         prettyprint_in_buffer(simple_type_info->dependent_unqualified_part));

                result = strappend(result, "<template dependent type: [");
                result = strappend(result, 
                        get_named_simple_type_name(simple_type_info->dependent_entry));
                result = strappend(result, "]");

                dependent_name_part_t* parts = simple_type_info->dependent_parts;
                while (parts != NULL)
                {
                    result = strappend(result, "::");
                    result = strappend(result, parts->name);

                    if (parts->template_arguments != NULL)
                    {
                        result = strappend(result, 
                                get_template_arguments_list_str(parts->template_arguments));
                    }

                    parts = parts->next;
                }

                result = strappend(result, ">");
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
    DEBUG_CODE()
    {
        if (printed_declarator == NULL)
        {
            return "<<NULL>>";
        }
    }

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
            case TK_ERROR:
                {
                    tmp_result = strappend(tmp_result, "<error-type>");
                    printed_declarator = NULL;
                    break;
                }
            case TK_OVERLOAD :
                {
                    tmp_result = strappend(tmp_result, " <unresolved overload function type> ");
                }
                printed_declarator = NULL;
                break;
            case TK_BRACED_LIST:
                {
                    tmp_result = strappend(tmp_result, " <braced-list-type>{ ");
                    int i;
                    for (i = 0; i < braced_list_type_get_num_types(printed_declarator); i++)
                    {
                        if (i != 0)
                        {
                            strappend(tmp_result, ", ");
                        }

                        strappend(tmp_result, 
                                print_declarator(braced_list_type_get_type_num(printed_declarator, i)));
                    }
                    tmp_result = strappend(tmp_result, " } ");
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
                tmp_result = strappend(tmp_result, "[");
                tmp_result = strappend(tmp_result, prettyprint_in_buffer(printed_declarator->array->lower_bound));
                tmp_result = strappend(tmp_result, ":");
                tmp_result = strappend(tmp_result, prettyprint_in_buffer(printed_declarator->array->upper_bound));
                tmp_result = strappend(tmp_result, "] of ");
                printed_declarator = printed_declarator->array->element_type;
                break;
            case TK_FUNCTION :
                {
                    int i;
                    tmp_result = strappend(tmp_result, "function");

                    if (printed_declarator->info->is_template_specialized_type
                            && printed_declarator->template_arguments != NULL)
                    {
                        tmp_result = strappend(tmp_result, "< ");
                        for (i = 0; i < printed_declarator->template_arguments->num_arguments; i++)
                        {
                            template_argument_t* template_argument = 
                                printed_declarator->template_arguments->argument_list[i];

                            char c[256];
                            snprintf(c, 255, "[[%d, %d]] ", 
                                    template_argument->nesting,
                                    template_argument->position);

                            tmp_result = strappend(tmp_result, uniquestr(c));

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
                    if (printed_declarator->function->return_type != NULL)
                    {
                        tmp_result = strappend(tmp_result, " returning ");
                    }
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

    // Zero type of C++
    if ((is_zero_type(orig)
                && is_pointer_type(dest))
            || (is_zero_type(dest)
                && is_pointer_type(orig)))
    {
        return 1;
    }

    // It turned that none was a pointer!
    if (!is_pointer_type(orig)
            || !is_pointer_type(dest))
    {
        return 0;
    }

    // This additional comparison is just for C++
    while (is_pointer_type(orig)
            && is_pointer_type(dest))
    {
        orig = pointer_type_get_pointee_type(orig);
        dest = pointer_type_get_pointee_type(dest);
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

        standard_conversion_t conversion_among_lvalues = no_scs_conversion;

        if (standard_conversion_between_types(&conversion_among_lvalues, unqualif_orig, unqualif_dest)
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
    // cv1 T1& -> cv2 T1&&
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

    //Vector types
    //scalar -->__attribute_((vector_size(X)))  
    if(is_vector_type(no_ref(dest)) 
            && is_scalar_type(no_ref(orig)))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Applying scalar-to-vector conversion\n");
        }
        dest = vector_type_get_element_type(dest);
    }

    // Lower complex types
    // char dest_is_complex = 0;
    if (is_complex_type(dest))
    {
        // dest_is_complex = 1;
        dest = complex_type_get_base_type(dest);
    }
    // char orig_is_complex = 0;
    if (is_complex_type(orig))
    {
        // orig_is_complex = 1;
        orig = complex_type_get_base_type(orig);
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
     * FIXME, enums can be promoted to different underlying types. Now assuming
     * that all are int
     *
     * Note that the types compared here must be unqualified, since we
     * don't care their qualification here
     */
    if (!equivalent_types(get_unqualified_type(dest), get_unqualified_type(orig)))
    {
        if (is_signed_int_type(dest)
                && (is_char_type(orig)
                    || is_signed_char_type(orig)
                    || is_unsigned_char_type(orig)
                    || is_signed_short_int_type(orig)
                    || is_unsigned_short_int_type(orig)
                    || is_wchar_t_type(orig)
                    || is_enum_type(orig)
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
                    || is_enum_type(orig))
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
            dest = get_unqualified_type(orig);
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
            dest = get_unqualified_type(orig);
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
            dest = get_unqualified_type(orig);
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
                    || is_enum_type(orig)
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

type_t* get_unresolved_overloaded_type(const scope_entry_list_t* overload_set,
        template_argument_list_t* explicit_template_arguments)
{
    type_t* result = new_empty_type();

    result->kind = TK_OVERLOAD;

    result->unqualified_type = result;
    // Use a smarter approach for this one
    result->overload_set = entry_list_copy(overload_set);
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

    return entry_list_copy(t->overload_set);
}

template_argument_list_t* unresolved_overloaded_type_get_explicit_template_arguments(type_t* t)
{
    ERROR_CONDITION(!is_unresolved_overloaded_type(t), "This is not an unresolved overloaded type", 0);

    return t->explicit_template_argument_list;
}

scope_entry_t* unresolved_overloaded_type_simplify(type_t* t, decl_context_t decl_context, int line, const char* filename)
{
    ERROR_CONDITION(!is_unresolved_overloaded_type(t), "This is not an unresolved overloaded type", 0);

    if (entry_list_size(t->overload_set) > 1)
        return NULL;

    scope_entry_t* entry = entry_list_head(t->overload_set);
    template_argument_list_t *argument_list = t->explicit_template_argument_list;

    if (entry->kind != SK_TEMPLATE)
    {
        return entry;
    }
    else if (argument_list == NULL)
    {
        return NULL;
    }

    // Get a specialization of this template
    type_t* specialization_type = template_type_get_primary_type(entry->type_information);
    scope_entry_t* specialization_symbol = named_type_get_symbol(specialization_type);
    type_t* specialized_function_type = specialization_symbol->type_information;

    template_parameter_list_t* template_parameters = 
        template_specialized_type_get_template_parameters(specialized_function_type);

    type_t* named_specialization_type = template_type_get_specialized_type(entry->type_information,
            argument_list, template_parameters,
            decl_context, line, filename);

    if (!is_dependent_type(named_specialization_type))
    {
        return named_type_get_symbol(named_specialization_type);
    }
    else
    {
        return NULL;
    }
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
    return (_dependent_type != NULL
            && t != NULL
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
        _zero_type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_signed_int;
        _zero_type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_signed_int;
        _zero_type->info->valid_size = 1;
    }

    return _zero_type;
}

static type_t* _null_type = NULL;

// special type for '__null' and forthcoming 'nullptr_t'
type_t* get_null_type(void)
{
    if (_null_type == NULL)
    {
        _null_type = get_simple_type();
        _null_type->type->kind = STK_BUILTIN_TYPE;
        _null_type->type->builtin_type = BT_INT;

        _null_type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_pointer;
        _null_type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_pointer;
        _null_type->info->valid_size = 1;

        // Fix the underlying integer type
        // The two first are highly unlikely out of the embedded world
        // FIXME - Should we use also unsigned?
        if (_null_type->info->size == 1)
        {
            _null_type->type->builtin_type = BT_CHAR;
        }
        else if (_null_type->info->size == CURRENT_CONFIGURATION->type_environment->sizeof_signed_short)
        {
            // Set 'short'
            _null_type->type->is_short = 1;
        }
        else if (_null_type->info->size == CURRENT_CONFIGURATION->type_environment->sizeof_signed_int)
        {
            // Do nothing
        }
        else if (_null_type->info->size == CURRENT_CONFIGURATION->type_environment->sizeof_signed_long)
        {
            // Set 'long'
            _null_type->type->is_long = 1;
        }
        else if (_null_type->info->size == CURRENT_CONFIGURATION->type_environment->sizeof_signed_long_long)
        {
            // Set 'long'
            _null_type->type->is_long = 2;
        }
    }

    return _null_type;
}

static type_t* _error_type = NULL;
type_t* get_error_type(void)
{
    if (_error_type == NULL)
    {
        _error_type = counted_calloc(1, sizeof(*_error_type), &_bytes_due_to_type_system);
        _error_type->kind = TK_ERROR;
        _error_type->unqualified_type = _error_type;
        _error_type->info = 
            counted_calloc(1, sizeof(*_error_type->info), &_bytes_due_to_type_system);
    }
    return _error_type;
}

char is_error_type(type_t* t)
{
    // We do not allow a NULL type here at the moment
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    return (_error_type != NULL && t == _error_type);
}

char is_zero_type(type_t* t)
{
    return ((_zero_type != NULL
            && t == _zero_type)
            || (_null_type != NULL
                && t == _null_type));
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
        AST integer_literal = ASTLeaf(AST_DECIMAL_LITERAL, NULL, 0, c);

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
        _ellipsis_type = new_empty_type();
        _ellipsis_type->kind = TK_ELLIPSIS;
    }

    return _ellipsis_type;
}


char is_ellipsis_type(type_t* t)
{
    return ((_ellipsis_type != NULL)
            && (t == _ellipsis_type));
}

type_t* get_braced_list_type(int num_types, type_t** type_list)
{
    type_t* result = new_empty_type();

    result->kind = TK_BRACED_LIST;

    result->braced_type = counted_calloc(1, sizeof(*result->braced_type), &_bytes_due_to_type_system);

    result->braced_type->num_types = num_types;
    result->braced_type->type_list = type_list;

    return result;
}

int braced_list_type_get_num_types(type_t* t)
{
    ERROR_CONDITION (!is_braced_list_type(t), "This is not a braced list type", 0);
    return t->braced_type->num_types;
}

type_t** braced_list_type_get_types(type_t* t)
{
    ERROR_CONDITION (!is_braced_list_type(t), "This is not a braced list type", 0);
    return t->braced_type->type_list;
}

type_t* braced_list_type_get_type_num(type_t* t, int num)
{
    ERROR_CONDITION (!is_braced_list_type(t), "This is not a braced list type", 0);
    return t->braced_type->type_list[num];
}

char is_braced_list_type(type_t* t)
{
    return ((t != NULL)
            && (t->kind == TK_BRACED_LIST));
}

static type_t* _throw_expr_type = NULL;

type_t* get_throw_expr_type(void)
{
    if (_throw_expr_type == NULL)
    {
        _throw_expr_type = get_simple_type();
        _throw_expr_type->type->kind = STK_BUILTIN_TYPE;
        _throw_expr_type->type->builtin_type = BT_VOID;
        _throw_expr_type->info->is_incomplete = 1;
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
    return t->info->size;
}

type_t* get_computed_function_type(computed_function_type_t compute_type_function)
{
    type_t* result = new_empty_type();

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

char is_scalar_type(type_t* t)
{
    return (!is_pointer_type(t)
            && !is_pointer_to_member_type(t)
            && !is_array_type(t)
            && !is_lvalue_reference_type(t)
            && !is_rvalue_reference_type(t)
            && !is_function_type(t)
            && !is_vector_type(t));
}


char is_incomplete_type(type_t* t)
{
    t = canonical_type(t);
    return t->info->is_incomplete;
}

char is_complete_type(type_t* t)
{
    return !is_incomplete_type(t);
}

void set_is_incomplete_type(type_t* t, char is_incomplete)
{
    t = canonical_type(t);
    t->info->is_incomplete = is_incomplete;
}

void set_is_complete_type(type_t* t, char is_complete)
{
    set_is_incomplete_type(t, !is_complete);
}

scope_entry_list_t* class_type_get_all_bases(type_t *t, char include_dependent)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);

    scope_entry_list_t *result = NULL;

    int i; 
    int num_bases = class_type_get_num_bases(t);
    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        scope_entry_t* base_class = class_type_get_base_num(t, i, &is_virtual, &is_dependent);

        if (is_dependent && !include_dependent)
            continue;

        // Add the current class if it is not already in the result

        char found = 0;

        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(result);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            if (equivalent_types(base_class->type_information, 
                        entry->type_information))
            {
                found = 1;
                break;
            }
        }
        entry_list_iterator_free(it);

        if (!found)
        {
            result = entry_list_add(result, base_class);
        }

        // Now recursively get all the bases of this base
        scope_entry_list_t* base_list = class_type_get_all_bases(base_class->type_information, 
                /* include_dependent */ 0);

        // Add those that are not already in the result
        for (it = entry_list_iterator_begin(base_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);

            found = 0;
            scope_entry_list_iterator_t* it2 = NULL;
            for (it2 = entry_list_iterator_begin(result);
                    !entry_list_iterator_end(it2);
                    entry_list_iterator_next(it2))
            {
                scope_entry_t* entry2 = entry_list_iterator_current(it2);
                if (equivalent_types(entry->type_information, 
                            entry2->type_information))
                {
                    found = 1;
                    break;
                }
            }
            entry_list_iterator_free(it2);

            if (!found)
            {
                result = entry_list_add(result, entry); 
            }
        }
        entry_list_iterator_free(it);
    }

    return result;
}

static char covariant_return(type_t* overrided_type, type_t* virtual_type)
{
    if (equivalent_types(overrided_type, virtual_type))
        return 1;

    if ((is_pointer_to_class_type(overrided_type)
            && is_pointer_to_class_type(virtual_type))
            || (is_reference_to_class_type(overrided_type)
                && is_reference_to_class_type(virtual_type)))
    {
        if (is_pointer_to_class_type(overrided_type)
                && is_pointer_to_class_type(virtual_type))
        {
            overrided_type = pointer_type_get_pointee_type(overrided_type);
            virtual_type = pointer_type_get_pointee_type(virtual_type);
        }
        else
        {
            overrided_type = reference_type_get_referenced_type(overrided_type);
            virtual_type = reference_type_get_referenced_type(virtual_type);
        }

        if (class_type_is_base(virtual_type, overrided_type))
            return 1;
    }
    return 0;
}

char function_type_can_override(type_t* potential_overrider, type_t* function_type)
{
    return compatible_parameters(potential_overrider->function, function_type->function)
        && covariant_return(potential_overrider, function_type);
}

#if 0
static char has_overrider(scope_entry_t* entry, scope_entry_list_t* list)
{
    char result = 0;

    while (list != NULL && !result)
    {
        // 'current_entry' comes from a list built from a derived class, so it 
        // has to be considered always a potential final overrider of 'entry'
        scope_entry_t* current_entry = list->entry;

        result = (strcmp(current_entry->symbol_name, entry->symbol_name) == 0
            && function_type_can_override(current_entry->type_information, entry->type_information));

        list = list->next;
    }

    return result;
}
#endif

void class_type_add_virtual_function(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "Invalid class type", 0);
    P_LIST_ADD_ONCE(class_type->type->class_info->virtual_functions,
            class_type->type->class_info->num_virtual_functions,
            entry);
}

int class_type_get_num_virtual_functions(type_t* class_type)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "Invalid class type", 0);
    return class_type->type->class_info->num_virtual_functions;
}

scope_entry_t* class_type_get_virtual_function_num(type_t* class_type, int i)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "Invalid class type", 0);

    return class_type->type->class_info->virtual_functions[i];
}

char class_type_is_trivially_copiable(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "It must be a class type", 0);
    type_t* class_type = get_actual_class_type(t);

    /*
       A trivially copyable class is a class that:
       - has no non-trivial copy constructors (12.8),
       - has no non-trivial move constructors (12.8),
       - has no non-trivial copy assignment operators (13.5.3, 12.8),
       - has no non-trivial move assignment operators (13.5.3, 12.8), and
       - has a trivial destructor (12.4).
     */

    int i;
    for (i = 0; i < class_type_get_num_copy_constructors(class_type); i++)
    {
        scope_entry_t* entry = class_type_get_copy_constructor_num(class_type, i);

        if (!entry->entity_specs.is_trivial)
            return 0;
    }

    for (i = 0; i < class_type_get_num_move_constructors(class_type); i++)
    {
        scope_entry_t* entry = class_type_get_move_constructor_num(class_type, i);

        if (!entry->entity_specs.is_trivial)
            return 0;
    }

    for (i = 0; i < class_type_get_num_copy_assignment_operators(class_type); i++)
    {
        scope_entry_t* entry = class_type_get_copy_assignment_operator_num(class_type, i);

        if (!entry->entity_specs.is_trivial)
            return 0;
    }

    for (i = 0; i < class_type_get_num_move_assignment_operators(class_type); i++)
    {
        scope_entry_t* entry = class_type_get_move_assignment_operator_num(class_type, i);

        if (!entry->entity_specs.is_trivial)
            return 0;
    }

    scope_entry_t* destructor = class_type_get_destructor(class_type);

    if (destructor != NULL
            && !destructor->entity_specs.is_trivial)
        return 0;

    return 1;
}

char class_type_is_trivial(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "It must be a class type", 0);
    type_t* class_type = get_actual_class_type(t);

    scope_entry_t* default_ctr = class_type_get_default_constructor(class_type);

    if (default_ctr != NULL && !default_ctr->entity_specs.is_trivial)
        return 0;

    if (!class_type_is_trivially_copiable(t))
        return 0;

    return 1;
}

char class_type_is_standard_layout(type_t* t)
{
    /*
       A standard-layout class is a class that:
       - has no non-static data members of type non-standard-layout class (or array of such types) or reference,
       - has no virtual functions (10.3) and no virtual base classes (10.1),
       - has the same access control (Clause 11) for all non-static data members,
       - has no non-standard-layout base classes,
       - either has no non-static data members in the most-derived class and at most one base class with
       non-static data members, or has no base classes with non-static data members, and
       - has no base classes of the same type as the first non-static data member.10
     */
    ERROR_CONDITION(!is_class_type(t), "It must be a class type", 0);
    type_t* class_type = get_actual_class_type(t);

    int i;
    for (i = 0; i < class_type_get_num_nonstatic_data_members(class_type); i++)
    {
        scope_entry_t* data_member = class_type_get_nonstatic_data_member_num(class_type, i);

        type_t* data_member_type = data_member->type_information;

        if (is_lvalue_reference_type(data_member_type)
                || is_rvalue_reference_type(data_member_type))
            return 0;

        if (is_array_type(data_member_type))
            data_member_type = array_type_get_element_type(data_member_type);
        
        if (is_class_type(data_member_type)
                && !class_type_is_standard_layout(data_member_type))
            return 0;
    }

    for (i = 0; i < class_type_get_num_member_functions(class_type); i++)
    {
        scope_entry_t* member_function = class_type_get_member_function_num(class_type, i);

        if (member_function->entity_specs.is_virtual)
            return 0;
    }

    for (i = 0 ; i < class_type_get_num_bases(class_type); i++)
    {
        char is_virtual = 0, is_dependent = 0;
        /* scope_entry_t* base = */ class_type_get_base_num(class_type, i, &is_virtual, &is_dependent);

        if (is_virtual)
            return 0;
    }

    access_specifier_t access = AS_UNKNOWN;

    for (i = 0; i < class_type_get_num_nonstatic_data_members(class_type); i++)
    {
        scope_entry_t* data_member = class_type_get_nonstatic_data_member_num(class_type, i);

        if (access == AS_UNKNOWN)
        {
            access = data_member->entity_specs.access;
        }
        else if (access != data_member->entity_specs.access)
        {
            return 0;
        }
    }

    for (i = 0 ; i < class_type_get_num_bases(class_type); i++)
    {
        char is_virtual = 0, is_dependent = 0;
        scope_entry_t* base = class_type_get_base_num(class_type, i, &is_virtual, &is_dependent);

        if (!class_type_is_standard_layout(base->type_information))
            return 0;
    }

    if (class_type_get_num_nonstatic_data_members(class_type) == 0)
    {
        int nonempty = 0;
        for (i = 0 ; i < class_type_get_num_bases(class_type); i++)
        {
            char is_virtual = 0, is_dependent = 0;
            scope_entry_t* base = class_type_get_base_num(class_type, i, &is_virtual, &is_dependent);

            nonempty += (class_type_get_num_nonstatic_data_members(base->type_information) != 0);
        }

        if (nonempty > 1)
            return 0;
    }
    else
    {
        for (i = 0 ; i < class_type_get_num_bases(class_type); i++)
        {
            char is_virtual = 0, is_dependent = 0;
            scope_entry_t* base = class_type_get_base_num(class_type, i, &is_virtual, &is_dependent);

            if (class_type_get_num_nonstatic_data_members(base->type_information) != 0)
            {
                return 0;
            }
        }

        scope_entry_t* first_nonstatic = class_type_get_nonstatic_data_member_num(class_type, 0);

        for (i = 0 ; i < class_type_get_num_bases(class_type); i++)
        {
            char is_virtual = 0, is_dependent = 0;
            scope_entry_t* base = class_type_get_base_num(class_type, i, &is_virtual, &is_dependent);

            if (equivalent_types(first_nonstatic->type_information, base->type_information))
                return 0;
        }
    }

    return 1;
}

char is_aggregate_type(type_t* t)
{
    /*
       An aggregate is an array or a class (Clause 9) with no user-provided
       constructors (12.1), no brace-or-equal initializers for non-static data
       members (9.2), no private or protected non-static data members (Clause
       11), no base classes (Clause 10), and no virtual functions (10.3).
     */
    if (is_array_type(t))
        return 1;

    if (is_class_type(t))
    {
        type_t* class_type = get_actual_class_type(t);

        // No user provided constructors
        int i;
        for (i = 0; 
                i < class_type_get_num_constructors(class_type);
                i++)
        {
            scope_entry_t* entry = class_type_get_constructors_num(class_type, i);

            if (entry->entity_specs.is_user_declared)
                return 0;
        }

        for (i = 0;
                i < class_type_get_num_nonstatic_data_members(class_type);
                i++)
        {
            scope_entry_t* entry = class_type_get_nonstatic_data_member_num(class_type, i);

            CXX1X_LANGUAGE()
            {
                // No initializer for nonstatic data member
                if (entry->expression_value != NULL)
                    return 0;
            }

            // No private or protected non-static data members
            if (entry->entity_specs.access == AS_PRIVATE
                    || entry->entity_specs.access == AS_PROTECTED)
                return 0;
        }

        // No base classes
        if (class_type_get_num_bases(class_type) != 0)
            return 0;

        for (i = 0; 
                i < class_type_get_num_member_functions(class_type);
                i++)
        {
            scope_entry_t* entry = class_type_get_member_function_num(class_type, i);

            // No virtual functions
            if (entry->entity_specs.is_virtual)
                return 0;
        }

        return 1;
    }

    return 0;
}

char class_type_is_pod(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "It must be a class type", 0);

    C_LANGUAGE()
    {
        // Nobody should be calling this, but if so, yes, in C this is a POD
        return 1;
    }

    CXX03_LANGUAGE()
    {
        /*
           A POD-struct is an aggregate class that has no non-static data members of type non-POD-struct,
           non-POD-union (or array of such types) or reference, and has no user-defined copy assignment operator
           and no user-defined destructor. Similarly, a POD-union is an aggregate union that has no non-static data
           members of type non-POD-struct, non-POD-union (or array of such types) or reference, and has no userdefined
           copy assignment operator and no user-defined destructor. A POD class is a class that is either a
           POD-struct or a POD-union
         */
        if (!is_aggregate_type(t))
            return 0;

        type_t* class_type = get_actual_class_type(t);

        int i;
        for (i = 0;
                i < class_type_get_num_nonstatic_data_members(class_type);
                i++)
        {
            scope_entry_t* entry = class_type_get_nonstatic_data_member_num(class_type, i);

            if (!is_pod_type(entry->type_information))
                return 0;
        }

        for (i = 0; i < class_type_get_num_copy_assignment_operators(class_type); i++)
        {
            scope_entry_t* entry = class_type_get_copy_assignment_operator_num(class_type, i);

            if (entry->entity_specs.is_user_declared)
                return 0;
        }

        scope_entry_t* destructor = class_type_get_destructor(class_type);
        if (destructor != NULL && destructor->entity_specs.is_user_declared)
            return 0;
    }

    CXX1X_LANGUAGE()
    {
        if (!class_type_is_trivial(t)
                || !class_type_is_standard_layout(t))
            return 0;

        type_t* class_type = get_actual_class_type(t);

        int i;
        for (i = 0;
                i < class_type_get_num_nonstatic_data_members(class_type);
                i++)
        {
            scope_entry_t* entry = class_type_get_nonstatic_data_member_num(class_type, i);

            if (!is_pod_type(entry->type_information))
                return 0;
        }
    }

    return 1;
}

static char closure_of_simple_properties(type_t* t, char (*class_prop)(type_t*))
{
    if (is_integral_type(t)
            || is_enum_type(t)
            || is_floating_type(t))
        return 1;

    if (is_pointer_type(t))
        return 1;

    if (is_pointer_to_member_type(t))
        return 1;

    if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
        return 0;

    if (is_array_type(t))
        return closure_of_simple_properties(array_type_get_element_type(t), class_prop);

    if (is_class_type(t))
        return class_prop(t);

    internal_error("Unhandled type", 0);
}

char is_pod_type(type_t* t)
{
    return closure_of_simple_properties(t, class_type_is_pod);
}

char is_trivially_copiable_type(type_t* t)
{
    return closure_of_simple_properties(t, class_type_is_trivially_copiable);
}

char is_standard_layout_type(type_t* t)
{
    return closure_of_simple_properties(t, class_type_is_standard_layout);
}

char type_is_runtime_sized(type_t* t)
{
    // This function is only valid in C but we could relax it a bit for C++
    CXX_LANGUAGE()
    {
        // No type is runtime sized in C++ actually, at least no VLAs since it
        // would break all template stuff
        return 0;
    }

    if (is_array_type(t))
    {
        return array_type_is_vla(t);
    }
    else if (is_class_type(t))
    {
        type_t* class_type = get_actual_class_type(t);
        int i;
        for (i = 0; i < class_type_get_num_nonstatic_data_members(class_type); i++)
        {
            scope_entry_t* member = class_type_get_nonstatic_data_member_num(class_type, i);

            if (type_is_runtime_sized(member->type_information))
                    return 1;
        }
    }

    return 0;
}

_size_t type_get_size(type_t* t)
{
    ERROR_CONDITION(CURRENT_CONFIGURATION->type_environment == NULL,
            "Invalid type environment!", 0);

    CXX_LANGUAGE()
    {
        ERROR_CONDITION(is_dependent_type(t), "Dependent type '%s' has got its size requested!\n",
                print_declarator(t));
    }
    ERROR_CONDITION(is_incomplete_type(t), "Incomplete type '%s' got its size requested!\n",
            print_declarator(t));

    // Note that we are not advancing typedefs because of attributes affecting types!
    if (!t->info->valid_size)
    {
        if (is_named_type(t))
        {
            type_t* alias_type = named_type_get_symbol(t)->type_information;

            type_set_size(t, type_get_size(alias_type));
            type_set_alignment(t, type_get_alignment(alias_type));

            CXX_LANGUAGE()
            {
                type_set_data_size(t, type_get_data_size(alias_type));
                class_type_set_non_virtual_size(t, 
                        class_type_get_non_virtual_size(alias_type));
                class_type_set_non_virtual_align(t, 
                        class_type_get_non_virtual_align(alias_type));
            }

            // State it valid
            type_set_valid_size(t, 1);
        }
        else
        {
            // Let's assume that every other thing is aggregated and must have its size
            // computed
            (CURRENT_CONFIGURATION->type_environment->compute_sizeof)(t);
        }

        ERROR_CONDITION(!t->info->valid_size, 
                "Valid size has not been properly computed!", 0);
    }

    return t->info->size;
}

_size_t type_get_alignment(type_t* t)
{
    // Note that we are not advancing typedefs because of attributes affecting types!
    if (!t->info->valid_size)
    {
        type_get_size(t);

        ERROR_CONDITION(!t->info->valid_size, 
                "Valid size has not been properly computed!", 0);
    }

    return t->info->alignment;
}

void type_set_size(type_t* t, _size_t size)
{
    ERROR_CONDITION(t == NULL, 
            "Invalid type", 0);

    t->info->size = size;
}

void type_set_alignment(type_t* t, _size_t alignment) 
{
    ERROR_CONDITION(t == NULL, 
            "Invalid type", 0);

    t->info->alignment = alignment;
}

void type_set_valid_size(type_t* t, char valid)
{
    ERROR_CONDITION(t == NULL,
            "Invalid type", 0);

    t->info->valid_size = valid;
}

_size_t type_get_data_size(type_t* t)
{
    C_LANGUAGE()
    {
        internal_error("This function is only for C++", 0);
    }

    if (!t->info->valid_size)
    {
        type_get_size(t);

        ERROR_CONDITION(!t->info->valid_size, 
                "Valid size has not been properly computed!", 0);
    }

    return t->info->data_size;
}

void type_set_data_size(type_t* t, _size_t data_size)
{
    C_LANGUAGE()
    {
        internal_error("This function is only for C++", 0);
    }

    t->info->data_size = data_size;
}

_size_t class_type_get_non_virtual_size(type_t* t)
{
    C_LANGUAGE()
    {
        internal_error("This function is only for C++", 0);
    }

    if (!t->info->valid_size)
    {
        type_get_size(t);

        ERROR_CONDITION(!t->info->valid_size, 
                "Valid size has not been properly computed!", 0);
    }

    if (is_class_type(t))
    {
        type_t* class_type = get_actual_class_type(t);
        return class_type->type->class_info->non_virtual_size;
    }
    return type_get_size(t);
}

void class_type_set_non_virtual_size(type_t* t, _size_t non_virtual_size)
{
    C_LANGUAGE()
    {
        internal_error("This function is only for C++", 0);
    }

    if (is_class_type(t))
    {
        type_t* class_type = get_actual_class_type(t);
        class_type->type->class_info->non_virtual_size = non_virtual_size;
    }
}

_size_t class_type_get_non_virtual_align(type_t* t)
{
    C_LANGUAGE()
    {
        internal_error("This function is only for C++", 0);
    }

    if (!t->info->valid_size)
    {
        type_get_size(t);

        ERROR_CONDITION(!t->info->valid_size, 
                "Valid size has not been properly computed!", 0);
    }

    if (is_class_type(t))
    {
        type_t* class_type = get_actual_class_type(t);
        return class_type->type->class_info->non_virtual_align;
    }

    return type_get_alignment(t);
}

void class_type_set_non_virtual_align(type_t* t, _size_t non_virtual_align)
{
    C_LANGUAGE()
    {
        internal_error("This function is only for C++", 0);
    }

    if (is_class_type(t))
    {
        type_t* class_type = get_actual_class_type(t);
        class_type->type->class_info->non_virtual_align = non_virtual_align;
    }
}

_size_t class_type_get_offset_virtual_base(type_t* t, scope_entry_t* virtual_base)
{
    C_LANGUAGE()
    {
        internal_error("This function is only for C++", 0);
    }

    ERROR_CONDITION(!is_class_type(t),
            "This is not an class type!", 0);

    type_t* class_type = get_actual_class_type(t);

    int num_virtual_bases = class_type->type->class_info->num_virtual_bases;
    int i;
    
    for (i = 0; i < num_virtual_bases; i++)
    {
        if (class_type->type->class_info->virtual_base_classes_list[i]->virtual_base == virtual_base)
        {
            return class_type->type->class_info->virtual_base_classes_list[i]->virtual_base_offset;
        }
    }

    internal_error("Unreachable code", 0);
}

void class_type_set_offset_virtual_base(type_t* t, scope_entry_t* virtual_base, _size_t offset)
{
    C_LANGUAGE()
    {
        internal_error("This function is only for C++", 0);
    }

    ERROR_CONDITION(!is_class_type(t),
            "This is not an class type!", 0);

    type_t* class_type = get_actual_class_type(t);

    int num_virtual_bases = class_type->type->class_info->num_virtual_bases;
    int i;
    
    for (i = 0; i < num_virtual_bases; i++)
    {
        if (class_type->type->class_info->virtual_base_classes_list[i]->virtual_base == virtual_base)
        {
            class_type->type->class_info->virtual_base_classes_list[i]->virtual_base_offset = offset;
            return;
        }
    }

    // Add the virtual base
    virtual_base_class_info_t* virtual_base_info = counted_calloc(
            1, sizeof(*virtual_base_info),
            &_bytes_due_to_type_system);

    virtual_base_info->virtual_base = virtual_base;
    virtual_base_info->virtual_base_offset = offset;

    P_LIST_ADD(class_type->type->class_info->virtual_base_classes_list, 
            class_type->type->class_info->num_virtual_bases,
            virtual_base_info);
}

int class_type_get_num_virtual_bases_with_offset(type_t* t)
{
    C_LANGUAGE()
    {
        internal_error("This function is only for C++", 0);
    }

    ERROR_CONDITION(!is_class_type(t),
            "This is not an class type!", 0);

    type_t* class_type = get_actual_class_type(t);

    return class_type->type->class_info->num_virtual_bases;
}

void class_type_get_virtual_base_with_offset_num(type_t* t, int num, 
        scope_entry_t** symbol, 
        _size_t* offset)
{
    C_LANGUAGE()
    {
        internal_error("This function is only for C++", 0);
    }

    ERROR_CONDITION(!is_class_type(t),
            "This is not an class type!", 0);

    type_t* class_type = get_actual_class_type(t);

    *symbol = class_type->type->class_info->virtual_base_classes_list[num]->virtual_base;
    *offset = class_type->type->class_info->virtual_base_classes_list[num]->virtual_base_offset;
}

char is_variably_modified_type(type_t* t)
{
    CXX_LANGUAGE()
    {
        return 0;
    }

    if (is_array_type(t))
    {
        return array_type_is_vla(t);
    }
    else if (is_pointer_type(t))
    {
        return is_variably_modified_type(pointer_type_get_pointee_type(t));
    }
    else if (is_class_type(t))
    {
        type_t* class_type = get_actual_class_type(t);
        int i;
        for (i = 0; i < class_type_get_num_nonstatic_data_members(class_type); i++)
        {
            scope_entry_t* member = class_type_get_nonstatic_data_member_num(class_type, i);

            if (type_is_runtime_sized(member->type_information))
                    return 1;
        }
    }

    return 0;
}

const char* print_type_str(type_t* t, decl_context_t decl_context)
{
    if (t == NULL)
    {
        return uniquestr("< unknown type >");
    }
    else
    {
        return get_declaration_string_internal(t, 
                decl_context, /* symbol_name */"", 
                /* initializer */ "", 
                /* semicolon */ 0,
                /* num_parameter_names */ NULL,
                /* parameter_names */ NULL,
                /* is_parameter */ 0);
    }
}

const char* print_decl_type_str(type_t* t, decl_context_t decl_context, const char* name)
{
    if (t == NULL)
    {
        char c[256];
        snprintf(c, 255, "< unknown type > %s\n", name);
        return uniquestr(c);
    }
    else if (is_unresolved_overloaded_type(t))
    {
        scope_entry_list_t* overload_set = unresolved_overloaded_type_get_overload_set(t);
        if (entry_list_size(overload_set) == 1)
        {
            return print_decl_type_str(entry_list_head(overload_set)->type_information, decl_context, name);
        }
        else
        {
            return uniquestr("<unresolved overload>");
        }
        entry_list_free(overload_set);
    }
    else if (is_braced_list_type(t))
    {
        return uniquestr("<brace-enclosed initializer list>");
    }
    else if (is_error_type(t))
    {
        return uniquestr("<error-type>");
    }
    else
    {
        return get_declaration_string_internal(t, 
                decl_context, /* symbol_name */ name, 
                /* initializer */ "", 
                /* semicolon */ 0,
                /* num_parameter_names */ NULL,
                /* parameter_names */ NULL,
                /* is_parameter */ 0);
    }
}

// This function, given a type returns the type-specifier related to it
//
// e.g T (*a)[3]  returns 'T'
//     const T& f(int)   returns 'const T' 
//
// so the type-specifier part of a type-id plus cv-qualifiers, if any
type_t* get_foundation_type(type_t* t)
{
    cv_qualifier_t cv = CV_NONE;
    t = advance_over_typedefs_with_cv_qualif(t, &cv);

    if (t == NULL)
    {
        return NULL;
    }
    else if (is_non_derived_type(t))
    {
        return get_cv_qualified_type(t, cv);
    }
    else if (is_function_type(t))
    {
        return get_foundation_type(function_type_get_return_type(t));
    }
    else if (is_pointer_type(t))
    {
        return get_foundation_type(pointer_type_get_pointee_type(t));
    }
    else if (is_pointer_to_member_type(t))
    {
        return get_foundation_type(pointer_type_get_pointee_type(t));
    }
    else if (is_rvalue_reference_type(t)
            || is_lvalue_reference_type(t))
    {
        return get_foundation_type(reference_type_get_referenced_type(t));
    }
    else if (is_array_type(t))
    {
        return get_foundation_type(array_type_get_element_type(t));
    }
    else if (is_vector_type(t))
    {
        return get_foundation_type(vector_type_get_element_type(t));
    }
    else if (is_unresolved_overloaded_type(t))
    {
        return t;
    }
    else if (is_error_type(t))
    {
        return _error_type;
    }
    internal_error("Cannot get foundation type of type '%s'", print_declarator(t));
}
