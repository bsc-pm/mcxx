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
#include <stdio.h>
#include <string.h>
#include "cxx-buildscope.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-prettyprint.h"
#include "cxx-driver.h"
#include "hash.h"

/*
 * --
 */

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
    // An unknown type
    STK_TEMPLATE_DEPENDENT_TYPE,
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

    // The inner decl context created by this class
    decl_context_t inner_decl_context;

    // Destructor
    struct scope_entry_tag* destructor;

    // Conversion functions info
    int num_conversion_functions;
    struct scope_entry_tag** conversion_functions;

    // Operator function info
    int num_operator_functions;
    struct scope_entry_tag** operator_function_list;

    // Class constructors info
    int num_constructors;
    struct scope_entry_tag** constructor_list;

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
    
    // Used for instantiation
    matching_pair_t* matching_pair;
} simple_type_t;

// Information of a parameter
typedef 
struct parameter_info_tag
{
    // This parameter is '...'
    char is_ellipsis;
    // Otherwise it has the type here
    struct type_tag* type_info;
    // If not null, original_type holds the original type (array or function)
    struct type_tag* original_type;
    // Default argument tree for the parameter
    //    int f(int a = 10);
    AST default_argument;
    decl_context_t default_arg_context;
} parameter_info_t;

// Function information
typedef 
struct function_tag
{
    // The returning type of the function
    struct type_tag* return_type;

    // States if this is a conversion function
    // If it is true, then 'return_type' is the destination type of the
    // conversion
    char is_conversion;

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
    decl_context_t array_expr_decl_context;

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

    // Unqualified type, itself if the type is not qualified
    struct type_tag* unqualified_type;

    // For parameter types, if not null it means some adjustement was done
    struct type_tag* original_type;
};

/*
 * --
 */

/*
 * This file contains routines destined to work with types.  Comparing two
 * types, comparing function declarations and definitions, etc.
 */
static type_t* aliased_type(type_t* t);
static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2, 
        decl_context_t decl_context);
static char equivalent_array_type(array_info_t* t1, array_info_t* t2, 
        decl_context_t decl_context);
static char equivalent_function_type(type_t* t1, type_t* t2, decl_context_t decl_context);
static char compatible_parameters(function_info_t* t1, function_info_t* t2,
        decl_context_t decl_context);
static char compare_template_dependent_types(type_t* t1, type_t* t2,
        decl_context_t decl_context);
static type_t* get_type_of_dependent_typename(type_t* t1, decl_context_t decl_context);

/* Type constructors : Builtins */

static type_t* get_simple_type(void)
{
    type_t* result = calloc(1, sizeof(*result));
    result->kind = TK_DIRECT;
    result->type = calloc(1, sizeof(*result->type));
    result->unqualified_type = result;
    return result;
}

type_t* get_char_type(void)
{
    // This might be modified by a compiler flag
    return get_signed_char_type();
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
    }

    return _type;
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

type_t* get_user_defined_type(scope_entry_t* entry)
{
    type_t* type_info = get_simple_type();

    type_info->type->kind = STK_USER_DEFINED;
    type_info->type->user_defined_type = entry;

    return type_info;
}

type_t* get_template_dependent_type(AST tree, decl_context_t decl_context)
{
    type_t* type_info = get_simple_type();

    type_info->type->kind = STK_TEMPLATE_DEPENDENT_TYPE;
    type_info->type->typeof_expr = tree;
    type_info->type->typeof_decl_context = decl_context;

    return type_info;
}

type_t* get_new_enum_type(decl_context_t decl_context)
{
    type_t* type_info = get_simple_type();

    type_info->type->enum_info = (enum_info_t*) calloc(1, sizeof(*type_info->type->enum_info));
    type_info->type->kind = STK_ENUM;
    type_info->type->type_decl_context = decl_context;

    return type_info;
}

type_t* get_new_class_type(decl_context_t decl_context)
{
    type_t* type_info = get_simple_type();

    type_info->type->class_info = calloc(1, sizeof(*type_info->type->class_info));
    type_info->type->kind = STK_CLASS;
    type_info->type->type_decl_context = decl_context;

    return type_info;
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
        qualified_type = calloc(1, sizeof(*qualified_type));
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
        pointed_type = calloc(1, sizeof(*pointed_type));
        pointed_type->kind = TK_POINTER;
        pointed_type->unqualified_type = pointed_type;
        pointed_type->pointer = calloc(1, sizeof(*pointed_type->pointer));
        pointed_type->pointer->pointee = t;

        hash_put(_pointer_types, t, pointed_type);
    }

    return pointed_type;
}

type_t* get_reference_type(type_t* t)
{
    static Hash *_reference_types = NULL;

    if (_reference_types == NULL)
    {
        _reference_types = hash_create(HASH_SIZE, HASHFUNC(pointer_hash), KEYCMPFUNC(integer_comp));
    }

    type_t* referenced_type = hash_get(_reference_types, t);

    if (referenced_type == NULL)
    {
        referenced_type = calloc(1, sizeof(*referenced_type));
        referenced_type->kind = TK_REFERENCE;
        referenced_type->unqualified_type = referenced_type;
        referenced_type->pointer = calloc(1, sizeof(*referenced_type->pointer));
        referenced_type->pointer->pointee = t;

        hash_put(_reference_types, t, referenced_type);
    }

    return referenced_type;
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
        pointer_to_member = calloc(1, sizeof(*pointer_to_member));
        pointer_to_member->kind = TK_POINTER_TO_MEMBER;
        pointer_to_member->unqualified_type = pointer_to_member;
        pointer_to_member->pointer = calloc(1, sizeof(*pointer_to_member->pointer));
        pointer_to_member->pointer->pointee = t;
        pointer_to_member->pointer->pointee_class = class_entry;

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
    if (expression != NULL)
    {
        literal_value_t literal_value = evaluate_constant_expression(expression, decl_context);

        if (literal_value.kind != LVK_DEPENDENT_EXPR)
        {
            expression = tree_from_literal_value(literal_value);
        }
    }

    type_t* result = calloc(1, sizeof(*result));
    result->kind = TK_ARRAY;
    result->unqualified_type = result;
    result->array = calloc(1, sizeof(*(result->array)));
    result->array->element_type = element_type;
    result->array->array_expr = expression;
    result->array->array_expr_decl_context = decl_context;

    return result;
}

type_t* get_conversion_type(type_t* t)
{
    type_t* result = get_function_type(t);
    result->function->is_conversion = 1;
    return result;
}

type_t* get_function_type(type_t* t)
{
    // This type is not efficiently managed
    type_t* result = calloc(1, sizeof(*result));

    result->kind = TK_FUNCTION;
    result->unqualified_type = result;
    result->function = calloc(1, sizeof(*(result->function)));
    result->function->return_type = t;

    return result;
}

int function_type_get_num_parameters(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    return function_type->function->num_parameters;
}

type_t* function_type_get_parameter_type_num(type_t* function_type, int num_param)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    return function_type->function->parameter_list[num_param]->type_info;
}

void function_type_set_template_information(type_t* function_type,
        int template_nesting,
        int num_template_parameters,
        template_parameter_t** template_parameters)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    function_type->function->template_nesting = template_nesting;
    function_type->function->num_template_parameters = num_template_parameters;
    function_type->function->template_parameter_info = template_parameters;
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

void class_type_add_operator_function(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    P_LIST_ADD(class_type->type->class_info->operator_function_list, 
            class_type->type->class_info->num_operator_functions, entry);
}

void class_type_add_conversion_function(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);
    P_LIST_ADD(class_type->type->class_info->conversion_functions, 
            class_type->type->class_info->num_conversion_functions, entry);
}

void function_type_set_is_constructor(type_t* function_type, char is_constructor)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not function type", 0);
    function_type->function->is_constructor = is_constructor;
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

char is_conversion_type(type_t* t)
{
    ERROR_CONDITION(!is_function_type(t), "This is not a function type", 0);
    return t->function->is_conversion;
}

char function_type_is_conversion(type_t* t)
{
    return is_conversion_type(t);
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
        t1 = aliased_type(t1);
        if (cv_qualif != NULL)
        {
            *cv_qualif |= t1->cv_qualifier;
        }
    }

    return t1;
}

void function_type_add_parameter(type_t* function_type, 
        type_t* prototype_type, 
        type_t* parameter_type, 
        AST default_argument, 
        decl_context_t default_arg_context)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    parameter_info_t* new_parameter = calloc(1, sizeof(*new_parameter));

    new_parameter->type_info = prototype_type;
    new_parameter->original_type = parameter_type;
    new_parameter->default_argument = default_argument;
    new_parameter->default_arg_context = default_arg_context;

    P_LIST_ADD(function_type->function->parameter_list, 
            function_type->function->num_parameters, new_parameter);
}

void function_type_set_lacking_prototype(type_t* function_type, char lacks_prototype)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);
    function_type->function->lacks_prototype = lacks_prototype;
}

char function_type_get_lacking_prototype(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);
    return function_type->function->lacks_prototype;
}

void function_type_set_no_parameters(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    // Discard parameters
    function_type->function->num_parameters = 0;
    function_type->function->parameter_list = NULL;
}

void function_type_set_has_ellipsis(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);
    parameter_info_t* new_parameter = calloc(1, sizeof(*new_parameter));
    new_parameter->is_ellipsis = 1;

    P_LIST_ADD(function_type->function->parameter_list, function_type->function->num_parameters, new_parameter);
}

char function_type_get_has_ellipsis(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    if (function_type->function->num_parameters == 0)
        return 0;

    return function_type
        ->function
        ->parameter_list[function_type->function->num_parameters - 1]
        ->is_ellipsis;
}

void function_type_set_static(type_t* function_type, char is_static)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);
    function_type->function->is_static = is_static;
}

void function_type_set_inline(type_t* function_type, char is_inline)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);
    function_type->function->is_inline = is_inline;
}

void function_type_set_virtual(type_t* function_type, char is_virtual)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);
    function_type->function->is_virtual = is_virtual;
}

void function_type_set_explicit(type_t* function_type, char is_explicit)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);
    function_type->function->is_explicit = is_explicit;
}

char function_type_get_static(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);
    return function_type->function->is_static;
}

char function_type_get_inline(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);
    return function_type->function->is_inline;
}

char function_type_get_virtual(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);
    return function_type->function->is_virtual;
}

char function_type_get_explicit(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);
    return function_type->function->is_explicit;
}

template_argument_list_t* template_type_get_template_arguments(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->type->template_arguments;
}

matching_pair_t* template_type_get_template_match_pair(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->type->matching_pair;
}

void template_type_set_template_match_pair(type_t* t, matching_pair_t* matching_pair)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    t->type->matching_pair = matching_pair;
}

void template_type_set_template_arguments(type_t* t, template_argument_list_t* list)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    t->type->template_arguments = list;
}

void function_type_set_exception_spec(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);
    function_type->function->exception_spec = calloc(1, sizeof(*function_type->function->exception_spec));
}

void function_type_add_exception_spec(type_t* function_type, type_t* exception_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    ERROR_CONDITION(function_type->function->exception_spec == NULL, "This cannot be NULL", 0);

    P_LIST_ADD(function_type->function->exception_spec->exception_type_seq, 
            function_type->function->exception_spec->num_exception_types,
            exception_type);
}

void class_type_add_base_class(type_t* class_type, scope_entry_t* base_class, char is_virtual)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "This is not a class type", 0);

    base_class_info_t* new_base_class = calloc(1, sizeof(*new_base_class));
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

type_t* advance_over_typedefs(type_t* t1)
{
    return advance_over_typedefs_with_cv_qualif(t1, NULL);
}

char function_type_get_is_constructor(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "Type is not function type", 0);

    return function_type->function->is_constructor;
}

void function_type_set_template_body(type_t* type, AST function_body)
{
    ERROR_CONDITION(!is_function_type(type), "Typs ies not function type", 0);
    type->function->function_body = function_body;
}

/*
 * States if two types are equivalent. This means that they are the same
 * (ignoring typedefs). Just plain comparison, no standard conversion is
 * performed. cv-qualifiers are relevant for comparison
 */
static char equivalent_simple_types(type_t *t1, type_t *t2, 
        decl_context_t decl_context);

char equivalent_types(type_t* t1, type_t* t2, 
        enum cv_equivalence_t cv_equiv, decl_context_t decl_context)
{
    if (t1 == NULL || t2 == NULL)
        return 0;

    cv_qualifier_t cv_qualifier_t1, cv_qualifier_t2;

    cv_qualifier_t qualif_t1 = *(get_outermost_cv_qualifier(t1));
    cv_qualifier_t qualif_t2 = *(get_outermost_cv_qualifier(t2));
    if (cv_equiv == CVE_IGNORE_OUTERMOST)
    {
        // Remove the outermost cv_qualifier
        *(get_outermost_cv_qualifier(t1)) = CV_NONE;
        *(get_outermost_cv_qualifier(t2)) = CV_NONE;
    }
    
    // Advance over typedefs
    t1 = advance_over_typedefs_with_cv_qualif(t1, &cv_qualifier_t1);
    t2 = advance_over_typedefs_with_cv_qualif(t2, &cv_qualifier_t2);

    if (t1->kind != t2->kind)
    {
        // They cannot be the same
        type_t* dependent_simple_type = NULL;
        type_t* other_type = NULL;
        cv_qualifier_t qualif_depend = CV_NONE;
        if (t1->kind == TK_DIRECT && t1->type->kind == STK_TEMPLATE_DEPENDENT_TYPE)
        {
            dependent_simple_type = t1;
            other_type = t2;

            qualif_depend = *(get_outermost_cv_qualifier(t1));
        }
        else if (t2->kind == TK_DIRECT && t2->type->kind == STK_TEMPLATE_DEPENDENT_TYPE)
        {
            dependent_simple_type = t2;
            other_type = t1;

            qualif_depend = *(get_outermost_cv_qualifier(t2));
        }

        if (dependent_simple_type != NULL)
        {
            type_t* dependent_type = get_type_of_dependent_typename(dependent_simple_type, decl_context);

            if (dependent_type != NULL)
            {
                dependent_type = advance_over_typedefs(dependent_type);
                cv_qualifier_t saved_cv_qualif = *(get_outermost_cv_qualifier(dependent_type));

                *(get_outermost_cv_qualifier(dependent_type)) = qualif_depend;
                char result = equivalent_types(other_type, dependent_type, cv_equiv, decl_context);
                *(get_outermost_cv_qualifier(dependent_type)) = saved_cv_qualif;

                return result;
            }
        }
        return 0;
    }

    char result = 0;

    switch (t1->kind)
    {
        case TK_DIRECT :
            result = equivalent_simple_types(t1, t2, decl_context);
            break;
        case TK_POINTER :
            result = equivalent_pointer_type(t1->pointer, t2->pointer, decl_context);
            break;
        case TK_REFERENCE :
            result = equivalent_pointer_type(t1->pointer, t2->pointer, decl_context);
            break;
        case TK_POINTER_TO_MEMBER :
            break;
        case TK_ARRAY :
            result = equivalent_array_type(t1->array, t2->array, decl_context);
            break;
        case TK_FUNCTION :
            result = equivalent_function_type(t1, t2, decl_context);
            break;
        default :
            internal_error("Unknown type kind (%d)\n", t1->kind);
    }

    result &= equivalent_cv_qualification(cv_qualifier_t1, cv_qualifier_t2);

    if (cv_equiv == CVE_IGNORE_OUTERMOST)
    {
        *(get_outermost_cv_qualifier(t1)) = qualif_t1;
        *(get_outermost_cv_qualifier(t2)) = qualif_t2;
    }

    return result;
}

static type_t* get_type_of_dependent_typename(type_t* p_t1, decl_context_t decl_context)
{
    simple_type_t* t1 = p_t1->type;

    if (t1->kind != STK_TEMPLATE_DEPENDENT_TYPE)
    {
        internal_error("This is not a dependent typename\n", 0);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Getting the type of the dependent typename '%s'\n", 
                prettyprint_in_buffer(t1->typeof_expr));
    }

    decl_context_t t1_decl_context = t1->typeof_decl_context;

    AST t1_expr = t1->typeof_expr;
    AST t1_global_op = ASTSon0(t1_expr);
    AST t1_nested_name_spec = ASTSon1(t1_expr);
    AST t1_symbol = ASTSon2(t1_expr);

    decl_context.decl_flags |= DF_NO_FAIL;

    scope_entry_list_t* result_t1 = query_nested_name(t1_decl_context, t1_global_op, t1_nested_name_spec, t1_symbol);

    if (result_t1 == NULL)
        return NULL;

    if (result_t1->entry->type_information == NULL)
        return NULL;

    return result_t1->entry->type_information;
}

static
char equivalent_builtin_type(simple_type_t* t1, simple_type_t *t2);

static char equivalent_named_types(scope_entry_t* s1, scope_entry_t* s2, decl_context_t decl_context)
{
    if (s1->is_template_parameter
            && s2->is_template_parameter)
    {
        return ((s1->template_parameter_nesting == s2->template_parameter_nesting)
                && (s1->template_parameter_position == s2->template_parameter_position));
    }
    else
    {
        return equivalent_types(s1->type_information, s2->type_information, 
                CVE_CONSIDER, decl_context);
    }
}

char equivalent_simple_types(type_t *p_t1, type_t *p_t2, 
        decl_context_t decl_context)
{
    simple_type_t* t1 = p_t1->type;
    simple_type_t* t2 = p_t2->type;

    char result = 0;
    if (t1->kind != t2->kind)
    {
        // typedefs have been handled in an earlier place, so 
        // this cannot be the same type unless one is a template dependent type
        type_t* simple_dependent_type = NULL;
        type_t* other_type = NULL;
        if (t1->kind == STK_TEMPLATE_DEPENDENT_TYPE)
        {
            simple_dependent_type = p_t1;
            other_type = p_t2;
        }
        else if (t2->kind == STK_TEMPLATE_DEPENDENT_TYPE)
        {
            simple_dependent_type = p_t2;
            other_type = p_t1;
        }

        if (simple_dependent_type != NULL)
        {
            type_t* dep_type = get_type_of_dependent_typename(simple_dependent_type, decl_context);
            
            if (dep_type == NULL)
            {
                return 0;
            }
            else
            {
                // Try to solve this type
                char result;
                result = equivalent_types(dep_type, other_type, CVE_CONSIDER, decl_context);
                return result;
            }
        }

        return 0;
    }

    switch (t1->kind)
    {
        case STK_BUILTIN_TYPE :
            result = equivalent_builtin_type(t1, t2);
            break;
        case STK_CLASS :
            /* Fall-through */
        case STK_ENUM :
            // Pointer comparison MUST work
            // (if not, something is broken)
            result = (t1 == t2);
            break;
        case STK_USER_DEFINED :
            result = equivalent_named_types(t1->user_defined_type, 
                    t2->user_defined_type, decl_context);
            break;
        case STK_TEMPLATE_DEPENDENT_TYPE :
            result = compare_template_dependent_types(p_t1, p_t2, decl_context);
            break;
        case STK_TYPEDEF :
            internal_error("A typedef cannot reach here", 0);
            break;
        case STK_TYPEOF :
            internal_error("__typeof__ comparison still not implemented", 0);
            break;
        case STK_VA_LIST :
            {
                // If both are __builtin_va_list, this is trivially true
                return 1;
            }
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

static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2,
        decl_context_t decl_context)
{
    if (!equivalent_types(t1->pointee, t2->pointee,
                CVE_CONSIDER, decl_context))
    {
        return 0;
    }

    return 1;
}

static char equivalent_array_type(array_info_t* t1, array_info_t* t2,
        decl_context_t decl_context)
{
    if (!equivalent_types(t1->element_type, t2->element_type,
                CVE_CONSIDER, decl_context))
        return 0;

    if (t1->array_expr != NULL
            && t2->array_expr != NULL)
    {
        literal_value_t v1 = evaluate_constant_expression(t1->array_expr, t1->array_expr_decl_context);
        literal_value_t v2 = evaluate_constant_expression(t2->array_expr, t2->array_expr_decl_context);
        if (!equal_literal_values(v1, v2, decl_context))
            return 0;
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

cv_qualifier_t* get_outermost_cv_qualifier(type_t* t)
{
    // For types that do not have a cv qualifier on their own
    static cv_qualifier_t dummy_cv_qualif = CV_NONE;

    // This will avoid accidental modifications from outside
    dummy_cv_qualif = CV_NONE;

    switch (t->kind)
    {
        case TK_FUNCTION :
        case TK_DIRECT :
        case TK_POINTER :
        case TK_POINTER_TO_MEMBER :
            {
                return (&(t->cv_qualifier));
            }
        case TK_ARRAY :
        case TK_REFERENCE :
            {
                return (&dummy_cv_qualif);
            }
        default:
            {
                internal_error("Unexpected node type %d\n", t->kind);
            }
    }
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
        case TK_REFERENCE :
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

char overloaded_function(type_t* ft1, type_t* ft2, decl_context_t decl_context)
{
    function_info_t* t1 = ft1->function;
    function_info_t* t2 = ft2->function;

    if (t1->is_conversion != t2->is_conversion)
    {
        internal_error("Type function conversion flag mismatch", 0);
        return 0;
    }

    if (!t1->is_conversion)
    {
        if (t1->template_nesting != t2->template_nesting)
            return 1;

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

        if (!equivalent_types(t1->return_type, t2->return_type, CVE_CONSIDER, decl_context))
        {
            if (!is_dependent_type(t1->return_type, decl_context)
                    && !is_dependent_type(t2->return_type, decl_context))
            {
                internal_error("You are trying to overload a function by only modifying its return type", 0);
            }
            else
            {
                return 1;
            }
        }
    }
    else
    {
        // In case of conversion functions, overloading does not have sense,
        // but we will distinguish them by the return type exclusively
        return (!equivalent_types(t1->return_type, t2->return_type, 
                    CVE_CONSIDER, decl_context));
    }

    return 0;
}

static char equivalent_function_type(type_t* ft1, type_t* ft2, 
        decl_context_t decl_context)
{
    function_info_t* t1 = ft1->function;
    function_info_t* t2 = ft2->function;

    if (!equivalent_types(t1->return_type, t2->return_type, CVE_CONSIDER,
                decl_context))
        return 0;

    if (!compatible_parameters(t1, t2, decl_context))
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

static char compatible_parameters(function_info_t* t1, function_info_t* t2,
        decl_context_t decl_context)
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

        if (!equivalent_types(par1, par2, CVE_IGNORE_OUTERMOST, decl_context))
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

                if (!equivalent_types(array_type->array->element_type, pointer_type->pointer->pointee, 
                            CVE_CONSIDER, decl_context))
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
                    if (!equivalent_types(pointer_type->pointer->pointee, function_type, CVE_CONSIDER,
                                decl_context))
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

static char compare_template_dependent_types(type_t* p_t1, type_t* p_t2,
        decl_context_t decl_context)
{
    simple_type_t *t1 = p_t1->type;
    simple_type_t *t2 = p_t2->type;

    AST t1_expr = t1->typeof_expr;
    AST t2_expr = t2->typeof_expr;
    DEBUG_CODE()
    {
        fprintf(stderr, "We are going to compare dependent type '%s' with '%s'\n",
                prettyprint_in_buffer(t1_expr),
                prettyprint_in_buffer(t2_expr));
    }

    // Shortcut
    if (t1 == t2 
            || t1_expr == t2_expr)
    {
        DEBUG_CODE()
        {
            if (t1 == t2)
            {
                fprintf(stderr, "Dependent types are the same, trivially are the same\n");
            }
            else
            {
                fprintf(stderr, "Expressions are the same, trivially the types are the same\n");
            }
        }
        return 1;
    }

    decl_context_t t1_decl_context = t1->typeof_decl_context;
    decl_context_t t2_decl_context = t2->typeof_decl_context;

    AST t1_global_op = ASTSon0(t1_expr);
    AST t2_global_op = ASTSon0(t2_expr);

    AST t1_nested_name_spec = ASTSon1(t1_expr);
    AST t2_nested_name_spec = ASTSon1(t2_expr);

    AST t1_symbol = ASTSon2(t1_expr);
    AST t2_symbol = ASTSon2(t2_expr);

    // First try to solve them via the type system
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Checking if '%s' solves to the same as '%s'\n", 
                    prettyprint_in_buffer(t1_expr), 
                    prettyprint_in_buffer(t2_expr));
        }
        decl_context_t decl_context = t1_decl_context;
        t1_decl_context.decl_flags |= DF_NO_FAIL;

        scope_entry_list_t* result_t1 = query_nested_name(decl_context, t1_global_op, t1_nested_name_spec, t1_symbol);

        if (result_t1 != NULL)
        {
            scope_entry_t* entry_t1 = result_t1->entry;
            if (entry_t1->type_information != NULL)
            {
                if (equivalent_types(entry_t1->type_information, p_t2, 
                            CVE_CONSIDER, t1_decl_context))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Type '%s' solves to the same as '%s'\n", 
                                prettyprint_in_buffer(t1_expr), 
                                prettyprint_in_buffer(t2_expr));
                    }
                    return 1;
                }
            }
        }
    }

    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Checking if '%s' solves to the same as '%s'\n", 
                    prettyprint_in_buffer(t2_expr),
                    prettyprint_in_buffer(t1_expr));
        }

        decl_context_t decl_context = t2_decl_context;
        t2_decl_context.decl_flags |= DF_NO_FAIL;

        scope_entry_list_t* result_t2 = query_nested_name(decl_context, t2_global_op, t2_nested_name_spec, t2_symbol);

        if (result_t2 != NULL)
        {
            scope_entry_t* entry_t2 = result_t2->entry;
            if (entry_t2->type_information != NULL)
            {
                if (equivalent_types(entry_t2->type_information, p_t1,
                            CVE_CONSIDER, t2_decl_context))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Type '%s' solves to the same as '%s'\n", 
                                prettyprint_in_buffer(t2_expr), 
                                prettyprint_in_buffer(t1_expr));
                    }
                    return 1;
                }
            }
        }
    }

    // Fallback to syntactical comparison
    DEBUG_CODE()
    {
        fprintf(stderr, "We fall back to syntactical comparison step-by-step\n");
    }

    // One has :: and the other not
    if ((t1_global_op == NULL && t2_global_op != NULL)
            || (t1_global_op != NULL && t2_global_op == NULL))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "One type has global qualification and the other does not, thus they are different\n");
        }
        return 0;
    }

    int qualification_level = 0;

    char dependent_qualification = 0;

    while (t1_nested_name_spec != NULL
            && t2_nested_name_spec != NULL)
    {
        AST t1_class_or_namespace = ASTSon0(t1_nested_name_spec);
        AST t2_class_or_namespace = ASTSon0(t2_nested_name_spec);

        DEBUG_CODE()
        {
            fprintf(stderr, "Considering qualification part '%s' vs '%s'\n",
                        prettyprint_in_buffer(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace));
        }

        if (ASTType(t1_class_or_namespace) != ASTType(t2_class_or_namespace))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Qualification is '%s' that is syntactically different to '%s'\n",
                        prettyprint_in_buffer(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace));
            }
            return 0;
        }

        if (dependent_qualification)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Since this is dependent, we simply compare syntactic trees '%s' and '%s'\n",
                        prettyprint_in_buffer(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace));
            }

            if (!ast_equal(t1_class_or_namespace, t2_class_or_namespace))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Dependent trees '%s' and '%s' are not equal\n",
                            prettyprint_in_buffer(t1_class_or_namespace),
                            prettyprint_in_buffer(t2_class_or_namespace));
                }
                return 0;
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Dependent trees '%s' and '%s' are identic\n",
                            prettyprint_in_buffer(t1_class_or_namespace),
                            prettyprint_in_buffer(t2_class_or_namespace));
                }
            }
        }
        else if (ASTType(t1_class_or_namespace) == AST_SYMBOL)
        {
            scope_entry_list_t* t1_name_list = query_unqualified_name_str(t1_decl_context, ASTText(t1_class_or_namespace));
            scope_entry_list_t* t2_name_list = query_unqualified_name_str(t2_decl_context, ASTText(t2_class_or_namespace));

            if (t1_name_list == NULL || t2_name_list == NULL)
            {
                internal_error("When comparing template dependent types one or both names were not found t1=%p scope_t1=%p t2=%p scope_t2=%p\n",
                        t1_name_list, t1_decl_context.current_scope, t2_name_list, t2_decl_context.current_scope);
            }

            scope_entry_t* t1_name = t1_name_list->entry;
            scope_entry_t* t2_name = t2_name_list->entry;

            if (t1_name->kind != t2_name->kind)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Kind of '%s' is different to '%s'\n", 
                            t1_name->symbol_name,
                            t2_name->symbol_name);
                }
                return 0;
            }


            if (t1_name->kind == SK_NAMESPACE
                    && t2_name->kind == SK_NAMESPACE)
            {
                if (t1_name != t2_name)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Namespace '%s' is not the same namespace '%s'\n",
                                t1_name->symbol_name,
                                t2_name->symbol_name);
                    }
                    return 0;
                }
            }
            else
            {
                t1_name = give_real_entry(t1_name);
                t2_name = give_real_entry(t2_name);

                type_t* t1_type = t1_name->type_information;
                type_t* t2_type = t2_name->type_information;

                if (t1_type != NULL)
                    t1_type = advance_over_typedefs(t1_type);

                if (t2_type != NULL)
                    t2_type = advance_over_typedefs(t2_type);

                // Template parameters do not have type information, are just
                // symbols
                if ((t1_name->kind == SK_TEMPLATE_TYPE_PARAMETER
                            && t2_name->kind == SK_TEMPLATE_TYPE_PARAMETER)
                        || (t1_name->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                            && t2_name->kind == SK_TEMPLATE_TEMPLATE_PARAMETER))
                {
                    if (!equivalent_named_types(t1_name, t2_name, decl_context))
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Type '%s' is not the same as '%s'\n",
                                    get_named_type_name(t1_name),
                                    get_named_type_name(t2_name));
                        }
                        return 0;
                    }
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "From now, this dependent type is exclusively dependent due to a template parameter\n");
                    }
                    dependent_qualification = 1;
                }
                else if (is_template_dependent_type(t1_type)
                        && is_template_dependent_type(t2_type))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "From now, this dependent type is exclusively dependent due to a template dependent type\n");
                    }
                    dependent_qualification = 1;
                }

                if (!dependent_qualification 
                        && t1_name != t2_name)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Symbol '%s' is not the same as '%s'\n",
                                t1_name->symbol_name,
                                t2_name->symbol_name);
                    }
                    return 0;
                }

                // t2_type will be the same as t1_type
                if (!dependent_qualification 
                        && is_dependent_type(t1_type, decl_context))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "From now, this dependent type is exclusively dependent due to a template dependent type\n");
                    }
                    dependent_qualification = 1;
                }
            }
            t1_decl_context = t1_name->related_decl_context;
            t2_decl_context = t2_name->related_decl_context;
        }
        else if (ASTType(t1_class_or_namespace) == AST_TEMPLATE_ID)
        {
            scope_entry_list_t* t1_template_name_list;
            scope_entry_list_t* t2_template_name_list;

            if (qualification_level > 0)
            {
                t1_template_name_list = query_in_scope(t1_decl_context, t1_class_or_namespace);
                t2_template_name_list = query_in_scope(t2_decl_context, t2_class_or_namespace);
            }
            else
            {
                t1_template_name_list = query_nested_name(t1_decl_context, 
                        NULL,
                        NULL,
                        t1_class_or_namespace);
                t2_template_name_list = query_nested_name(t2_decl_context, 
                        NULL,
                        NULL,
                        t2_class_or_namespace);
            }

            if (t1_template_name_list == NULL || t2_template_name_list == NULL)
            {
                internal_error("When comparing template dependent types one or both names were not found t1=%p t2=%p\n",
                        t1_template_name_list, t2_template_name_list);
            }

            scope_entry_t* t1_template_name = t1_template_name_list->entry;
            scope_entry_t* t2_template_name = t2_template_name_list->entry;

            if (t1_template_name->kind != t2_template_name->kind)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Template symbol '%s' is not the same as '%s'\n",
                            t1_template_name->symbol_name,
                            t2_template_name->symbol_name);
                }
                return 0;
            }

            if (t1_template_name->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                if ((t1_template_name->template_parameter_position != 
                            t2_template_name->template_parameter_position)
                        || (t1_template_name->template_parameter_nesting != 
                            t2_template_name->template_parameter_nesting))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Template template parameter '%s' (%d-%d) is not the same as the\
                                template template parameter '%s' (%d-%d)\n",
                                t1_template_name->symbol_name,
                                t1_template_name->template_parameter_nesting,
                                t1_template_name->template_parameter_position,
                                t2_template_name->symbol_name,
                                t2_template_name->template_parameter_nesting,
                                t2_template_name->template_parameter_position);
                    }
                    return 0;
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "From now this typename is dependent due to a template template parameter\n");
                }
                dependent_qualification = 1;
            }
            else
            {
                if (t1_template_name != t2_template_name)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "The template symbols '%s' and '%s' do not refer to the same entity\n",
                                t1_template_name->symbol_name,
                                t2_template_name->symbol_name);
                    }
                    return 0;
                }
                else if (is_dependent_type(t1_template_name->type_information, decl_context)
                        && is_dependent_type(t2_template_name->type_information, decl_context))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "From now this typename is dependent due to a dependent template-id\n");
                    }
                    dependent_qualification = 1;
                }
            }
        }

        t1_nested_name_spec = ASTSon1(t1_nested_name_spec);
        t2_nested_name_spec = ASTSon1(t2_nested_name_spec);
        qualification_level++;
    }

    if ((t1_nested_name_spec == NULL && t2_nested_name_spec != NULL) 
            || (t1_nested_name_spec != NULL && t2_nested_name_spec == NULL))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Types differ in qualification level\n");
        }
        return 0;
    }

    // Check the final part of the qualified-id
    {
        AST t1_class_or_namespace = ASTSon2(t1_expr);
        AST t2_class_or_namespace = ASTSon2(t2_expr);

        DEBUG_CODE()
        {
            fprintf(stderr, "Considering qualification part '%s' vs '%s'\n",
                        prettyprint_in_buffer(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace));
        }

        if (ASTType(t1_class_or_namespace) != ASTType(t2_class_or_namespace))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Qualification is '%s' that is syntactically different to '%s'\n",
                        prettyprint_in_buffer(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace));
            }
            return 0;
        }

        if (dependent_qualification)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "Since this is dependent, we simply compare syntactic trees '%s' and '%s'\n",
                        prettyprint_in_buffer(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace));
            }

            if (!ast_equal(t1_class_or_namespace, t2_class_or_namespace))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Dependent trees '%s' and '%s' are not equal\n",
                            prettyprint_in_buffer(t1_class_or_namespace),
                            prettyprint_in_buffer(t2_class_or_namespace));
                }
                return 0;
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Dependent trees '%s' and '%s' are identic\n",
                            prettyprint_in_buffer(t1_class_or_namespace),
                            prettyprint_in_buffer(t2_class_or_namespace));
                }
            }
        }
        else if (ASTType(t1_class_or_namespace) == AST_SYMBOL)
        {
            // enum cxx_symbol_kind filter_class_or_namespace[3] = {SK_NAMESPACE, SK_CLASS, SK_TEMPLATE_TYPE_PARAMETER};

            scope_entry_list_t* t1_name_list = query_unqualified_name_str(t1_decl_context, ASTText(t1_class_or_namespace));
            scope_entry_list_t* t2_name_list = query_unqualified_name_str(t2_decl_context, ASTText(t2_class_or_namespace));

            if (t1_name_list == NULL || t2_name_list == NULL)
            {
                internal_error("When comparing template dependent types one or both names were not found t1=%p scope_t1=%p t2=%p scope_t2=%p, t1='%s' in %s, t2='%s' in %s\n",
                        t1_name_list, t1_decl_context.current_scope, t2_name_list, t2_decl_context.current_scope,
                        prettyprint_in_buffer(t1_class_or_namespace), 
                        node_information(t1_class_or_namespace),
                        prettyprint_in_buffer(t2_class_or_namespace),
                        node_information(t2_class_or_namespace));
            }

            scope_entry_t* t1_name = t1_name_list->entry;
            scope_entry_t* t2_name = t2_name_list->entry;

            if (t1_name->kind != t2_name->kind)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Kind of '%s' is different to '%s'\n", 
                            t1_name->symbol_name,
                            t2_name->symbol_name);
                }
                return 0;
            }


            if (t1_name->kind == SK_NAMESPACE
                    && t2_name->kind == SK_NAMESPACE)
            {
                if (t1_name != t2_name)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Namespace '%s' is not the same namespace '%s'\n",
                                t1_name->symbol_name,
                                t2_name->symbol_name);
                    }
                    return 0;
                }
            }
            else
            {
                t1_name = give_real_entry(t1_name);
                t2_name = give_real_entry(t2_name);

                type_t* t1_type = t1_name->type_information;
                type_t* t2_type = t2_name->type_information;

                if (t1_type != NULL)
                {
                    t1_type = advance_over_typedefs(t1_type);
                }
                else 
                {
                    t2_type = advance_over_typedefs(t2_type);
                }

                if ((t1_name->kind == SK_TEMPLATE_TYPE_PARAMETER
                            && t2_name->kind == SK_TEMPLATE_TYPE_PARAMETER)
                        || (t1_name->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                            && t2_name->kind == SK_TEMPLATE_TEMPLATE_PARAMETER))
                {
                    if (!equivalent_named_types(t1_name, t2_name, decl_context))
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "Type '%s' is not the same as '%s'\n",
                                    get_named_type_name(t1_name),
                                    get_named_type_name(t2_name)
                                    );
                        }
                        return 0;
                    }
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "From now, this dependent type is exclusively dependent due to a template parameter\n");
                    }
                    dependent_qualification = 1;
                }
                else if (is_template_dependent_type(t1_type)
                        && is_template_dependent_type(t2_type))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "From now, this dependent type is exclusively dependent due to a template dependent type\n");
                    }
                    dependent_qualification = 1;
                }

                if (t1_name != t2_name)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Symbol '%s' is not the same as '%s'\n",
                                t1_name->symbol_name,
                                t2_name->symbol_name);
                    }
                    return 0;
                }
            }
            t1_decl_context = t1_name->related_decl_context;
            t2_decl_context = t2_name->related_decl_context;
        }
        else if (ASTType(t1_class_or_namespace) == AST_TEMPLATE_ID)
        {
            scope_entry_list_t* t1_template_name_list;
            scope_entry_list_t* t2_template_name_list;

            // Extend the scope with the current template scope 
            // for the first lookup
            decl_context_t extended_context1 = decl_context;
            extended_context1.template_scope = t1_decl_context.template_scope;

            decl_context_t extended_context2 = decl_context;
            extended_context2.template_scope = t2_decl_context.template_scope;

            if (qualification_level > 0)
            {
                t1_template_name_list = query_in_scope(t1_decl_context, 
                        t1_class_or_namespace);
                t2_template_name_list = query_in_scope(t2_decl_context, 
                        t2_class_or_namespace);
            }
            else
            {
                t1_template_name_list = query_nested_name(extended_context1,
                        NULL,
                        NULL,
                        t1_class_or_namespace);
                t2_template_name_list = query_nested_name(extended_context2,
                        NULL,
                        NULL,
                        t2_class_or_namespace);
            }

            if (t1_template_name_list == NULL || t2_template_name_list == NULL)
            {
                internal_error("When comparing template dependent types one or both names were not found t1=%p t2=%p\n",
                        t1_template_name_list, t2_template_name_list);
            }

            scope_entry_t* t1_template_name = t1_template_name_list->entry;
            scope_entry_t* t2_template_name = t2_template_name_list->entry;

            if (t1_template_name->kind != t2_template_name->kind)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "Template symbol '%s' is not the same as '%s'\n",
                            t1_template_name->symbol_name,
                            t2_template_name->symbol_name);
                }
                return 0;
            }

            if (t1_template_name->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                if ((t1_template_name->template_parameter_position != 
                            t2_template_name->template_parameter_position)
                        || (t1_template_name->template_parameter_nesting != 
                            t2_template_name->template_parameter_nesting))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Template template parameter '%s' (%d-%d) is not the same as the\
                                template template parameter '%s' (%d-%d)\n",
                                t1_template_name->symbol_name,
                                t1_template_name->template_parameter_nesting,
                                t1_template_name->template_parameter_position,
                                t2_template_name->symbol_name,
                                t2_template_name->template_parameter_nesting,
                                t2_template_name->template_parameter_position);
                    }
                    return 0;
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "From now this typename is dependent due to a template template parameter\n");
                }
                dependent_qualification = 1;
            }
            else
            {
                // TODO - Check this because i'm unsure this is totally true
                if (t1_template_name != t2_template_name)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "The template symbols '%s' and '%s' do not refer to the same entity\n",
                                t1_template_name->symbol_name,
                                t2_template_name->symbol_name);
                    }
                    return 0;
                }
                else if (is_dependent_type(t1_template_name->type_information, decl_context)
                        && is_dependent_type(t2_template_name->type_information, decl_context))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "From now this typename is dependent due to a dependent template-id\n");
                    }
                    dependent_qualification = 1;
                }
            }
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "Dependent types are the same\n");
    }
    return 1;
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

static type_t* aliased_type(type_t* t1)
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
    return aliased_type(t1);
}

char is_builtin_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE);
}

type_t* get_foundational_type(type_t* t1)
{
    if (t1 == NULL)
        return NULL;

    while (t1->kind != TK_DIRECT)
    {
        switch (t1->kind)
        {
            case TK_POINTER :
            case TK_REFERENCE :
            case TK_POINTER_TO_MEMBER :
                t1 = t1->pointer->pointee;
                break;
            case TK_FUNCTION :
                t1 = t1->function->return_type;
                break;
            case TK_ARRAY :
                t1 = t1->array->element_type;
                break;
            default:
                internal_error("Unknown type kind %d", t1->kind);
        }
    }

    return t1;
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

char is_integral_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT);
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
    t = advance_over_typedefs(t);

    if (t != NULL
            && t->kind == TK_FUNCTION)
    {
        return t->function->return_type;
    }
    else 
        return NULL;
}

type_t* pointer_type_get_pointee_type(type_t *t)
{
    t = advance_over_typedefs(t);

    if (t != NULL 
            && t->kind == TK_POINTER)
    {
        return t->pointer->pointee;
    }
    else 
        return NULL;
}

scope_entry_t* pointer_to_member_type_get_class(type_t *t)
{
    t = advance_over_typedefs(t);

    if (t != NULL
            && t->kind == TK_POINTER_TO_MEMBER)
    {
        return t->pointer->pointee_class;
    }
    else 
        return NULL;
}

type_t* pointer_to_member_type_get_class_type(type_t *t)
{
    scope_entry_t* entry = pointer_to_member_type_get_class(t);

    if (entry == NULL)
        return NULL;
    else return entry->type_information;
}

type_t* array_type_get_element_type(type_t* t)
{
    t = advance_over_typedefs(t);

    if (t != NULL
            && t->kind == TK_ARRAY)
    {
        return t->array->element_type;
    }
    else 
        return NULL;
}

AST array_type_get_array_size_expr(type_t* t)
{
    t = advance_over_typedefs(t);

    if (t != NULL
            && t->kind == TK_ARRAY)
    {
        return t->array->array_expr;
    }
    else 
        return NULL;
}

decl_context_t array_type_get_array_size_expr_context(type_t* t)
{
    t = advance_over_typedefs(t);

    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);

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
    return (is_pointer_type(t1) && is_class_type(t1->pointer->pointee));
}

char is_reference_to_class_type(type_t* t1)
{
    return (is_reference_type(t1) && is_class_type(t1->pointer->pointee));
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
            && t->kind == TK_DIRECT
            && ((t->type->kind == STK_USER_DEFINED
                    && t->type->user_defined_type != NULL
                    && t->type->user_defined_type->type_information->kind == TK_DIRECT
                    && t->type->user_defined_type->type_information->type->kind == STK_ENUM)
                || (t->type->kind == STK_ENUM)));
}

char is_named_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_USER_DEFINED
            && t->type->user_defined_type != NULL);
}

char is_specialized_class_type(type_t* t)
{
    if (is_named_type(t))
    {
        scope_entry_t* symbol = named_type_get_symbol(t);

        if (symbol->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
        {
            return 1;
        }
    }

    return 0;
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

char can_be_promoted_to_dest(type_t* orig, type_t* dest)
{
    if (orig == NULL
            || dest == NULL)
        return 0;

    simple_type_t* orig_simple_type = orig->type;
    simple_type_t* dest_simple_type = dest->type;

    // A float always can be promoted to double
    if (orig_simple_type->builtin_type == BT_FLOAT
            && dest_simple_type->builtin_type == BT_DOUBLE)
    {
        return 1;
    }

    // A wchar_t can be promoted to a plain int
// #warning "This depends on the exact environment"
    if (orig_simple_type->builtin_type == BT_WCHAR
            && dest_simple_type->builtin_type == BT_INT
            && !dest_simple_type->is_short
            && !dest_simple_type->is_long
            && !dest_simple_type->is_unsigned)
    {
        return 1;
    }

    // A bool can be promoted to a plain int
// #warning "This depends on the exact environment"
    if (orig_simple_type->builtin_type == BT_BOOL
            && dest_simple_type->builtin_type == BT_INT
            && !dest_simple_type->is_short
            && !dest_simple_type->is_long
            && !dest_simple_type->is_unsigned)
    {
        return 1;
    }

    // A short, either signed or unsigned, can be promoted to a plain int
// #warning "This depends on the exact environment"
    if (orig_simple_type->builtin_type == BT_INT
            && orig_simple_type->is_short
            && dest_simple_type->builtin_type == BT_INT
            && !dest_simple_type->is_short
            && !dest_simple_type->is_long
            && !dest_simple_type->is_unsigned)
    {
        return 1;
    }

    // A char, either signed or unsigned, can be promoted to a plain int
// #warning "This depends on the exact environment"
    if (orig_simple_type->builtin_type == BT_CHAR
            && dest_simple_type->builtin_type == BT_INT
            && !dest_simple_type->is_short
            && !dest_simple_type->is_long
            && !dest_simple_type->is_unsigned)
    {
        return 1;
    }

// #warning Missing the case for bitfields

    // Doesn't look promotionable to me
    return 0;
}

type_t* reference_type_get_referenced_type(type_t* t1)
{
    t1 = advance_over_typedefs(t1);

    if (t1 != NULL
            && t1->kind == TK_REFERENCE)
    {
        return t1->pointer->pointee;
    }
    else
        return NULL;
}

char is_reference_type(type_t* t1)
{
    t1 = advance_over_typedefs(t1);

    return (t1 != NULL
            && t1->kind == TK_REFERENCE);
}

char is_reference_related(type_t* t1, type_t* t2, 
        decl_context_t decl_context)
{
    if (t1 == NULL
            || t2 == NULL)
        return 0;

    // cv1 t1 and cv2 t2 are reference related if
    //
    // a) t1 == t2, or if not
    // b) t1 belongs to base(t2), provided t1 and t2 are of class type
    
    cv_qualifier_t cv1 = get_foundational_type(t1)->cv_qualifier;
    cv_qualifier_t cv2 = get_foundational_type(t2)->cv_qualifier;

    // Ignore outermost
    get_foundational_type(t1)->cv_qualifier = CV_NONE;
    get_foundational_type(t2)->cv_qualifier = CV_NONE;
    
    if (equivalent_types(t1, t2, CVE_CONSIDER, decl_context))
    {
        return 1;
    }
    else if (is_class_type(t1)
            && is_class_type(t2))
    {
        if (is_base_class_of(t1, t2))
        {
            return 1;
        }
    }

    get_foundational_type(t1)->cv_qualifier = cv1;
    get_foundational_type(t2)->cv_qualifier = cv2;

    return 0;
}

decl_context_t enum_type_get_context(type_t* t)
{
    ERROR_CONDITION(!is_enumerated_type(t), "This is not an enumerated type", 0);
    return t->type->type_decl_context;
}

decl_context_t class_type_get_context(type_t* t)
{
    ERROR_CONDITION(!is_unnamed_class_type(t), "This is not a class type", 0);
    return t->type->type_decl_context;
}

char is_reference_compatible(type_t* t1, type_t* t2, 
        decl_context_t decl_context)
{
    if (t1 == NULL
            || t2 == NULL)
        return 0;
    // cv1 t1 and cv2 t2 are reference compatible if
    //
    // a) cv1 t1 and cv2 t2 are reference related
    // b) and cv1 is greater or equal to cv2
    
    if (is_reference_related(t1, t2, decl_context))
    {
        // They are references
        // cv_qualifier_t cv1 = get_foundational_type(t1)->type->cv_qualifier;
        // cv_qualifier_t cv2 = get_foundational_type(t2)->type->cv_qualifier;

        // Fix this
        //
        // cv1 is more qualified if everything in cv2 is also in cv1
        // if ((cv1 | cv2) == cv1)
        // {
        //  return 1;
        // }
        // else
        // {
        //  return 0;
        // }
        return 1;
    }
    else
    {
        return 0;
    }
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

char is_template_dependent_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_TEMPLATE_DEPENDENT_TYPE);
}

char can_be_converted_to_dest(type_t* orig, type_t* dest)
{
    if (orig == NULL
            || dest == NULL)
        return 0;

    simple_type_t* orig_simple_type = orig->type;
    simple_type_t* dest_simple_type = dest->type;

    // Anything can be converted to anything fundamental (except for void
    // types, that in general should not appear in the code as rvalues ...)
    if (orig_simple_type->builtin_type != BT_VOID
            && dest_simple_type->builtin_type != BT_VOID)
    {
        return 1;
    }

    // Does not look convertible
    return 0;
}

type_t* get_actual_class_type(type_t* class_type)
{
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
    return (possible_class != NULL
            && possible_class->kind == TK_DIRECT
            && possible_class->type->kind == STK_CLASS);
}

char is_named_class_type(type_t* possible_class)
{
    return (possible_class != NULL
            && possible_class->kind == TK_DIRECT
            && possible_class->type->kind == STK_USER_DEFINED
            && possible_class->type->user_defined_type != NULL
            && possible_class->type->user_defined_type->type_information->kind == TK_DIRECT
            && possible_class->type->user_defined_type->type_information->type->kind == STK_CLASS);
}

char is_base_class_of(type_t* possible_base, type_t* possible_derived)
{
    if (!is_named_class_type(possible_base)
            || !is_named_class_type(possible_derived))
    {
        internal_error("This function expects named class types", 0);
    }

    simple_type_t* derived_class_info = possible_derived->type->user_defined_type->type_information->type;

    int i;
    for (i = 0; i < derived_class_info->class_info->num_bases; i++)
    {
        type_t* current_base = derived_class_info->class_info->base_classes_list[i]->class_type;
        type_t* base_class_info = current_base->type->user_defined_type->type_information;
        
        if (base_class_info == possible_base)
        {
            return 1;
        }
    }

    for (i = 0; i < derived_class_info->class_info->num_bases; i++)
    {
        type_t* current_base = derived_class_info->class_info->base_classes_list[i]->class_type;
        type_t* base_class_info = current_base;
        
        // Now search recursively in the bases of this base
        if (is_base_class_of(possible_base, base_class_info))
        {
            return 1;
        }
    }

    // Not found
    return 0;
}

static char pointer_can_be_converted_to_dest_rec(type_t* orig, type_t* dest, 
        char* all_previous_are_const, char* to_void, char* derived_to_base, char* cv_adjustment,
        decl_context_t decl_context)
{
    if (orig == NULL
            || dest == NULL)
        return 0;
    /*
     * orig is the original pointer type
     *
     *   int * * b;
     *
     * and dest is the destination pointer type
     *
     *   int * * const c;
     *
     * Example:
     *
     *   void f(int * * const c);
     *   void g()
     *   {
     *      int * * b;
     *      f(b); <-- Valid
     *   }
     *
     * dest has to be more cv-qualified in general than b
     */

    orig = advance_over_typedefs(orig);
    dest = advance_over_typedefs(dest);

    if (orig->kind != dest->kind)
    {
        return 0;
    }

    if (orig->kind != TK_POINTER) 
    {
        if (equivalent_types(orig, dest, CVE_IGNORE_OUTERMOST, decl_context)
                || (dest->type->kind == STK_BUILTIN_TYPE
                    && dest->type->builtin_type == BT_VOID))
        {
            // We should ensure that dest is equal or more cv-qualified
            cv_qualifier_t cv_qualif_dest = *(get_outermost_cv_qualifier(dest));
            cv_qualifier_t cv_qualif_orig = *(get_outermost_cv_qualifier(orig));

            *to_void = (dest->type->kind == STK_BUILTIN_TYPE 
                    && dest->type->builtin_type == BT_VOID);

            if ((cv_qualif_dest | cv_qualif_orig) == cv_qualif_dest)
            {
                return 1;
            }
            else
            {
                return 0;
            }
        }
        else if (is_named_class_type(orig) && is_named_class_type(dest))
        {
            // If both are classes check if dest is a base of orig
            // B* can be pointer qualified to A* if A is a base of B
            if (is_base_class_of(dest, orig))
            {
                *derived_to_base = 1;
                return 1;
            }
        }
    }

    // orig->kind == dest->kind == TK_POINTER
    // Example:
    //    orig:  int * * const * *       a;
    //    dest:  int * * const * const * const a;
    //
    //  (orig can be converted to dest)

    // If the orig pointer is qualified, so does have to the dest one
    if ((orig->cv_qualifier | dest->cv_qualifier) != 
            orig->cv_qualifier)
    {
        return 0;
    }

    // If the dest pointer is const-qualified every previous pointer
    // should have been const-qualified
    if ((dest->cv_qualifier & CV_CONST) == CV_CONST)
    {
        if (!(*all_previous_are_const))
        {
            return 0;
        }
        *cv_adjustment = 1;
    }
    else
    {
        *all_previous_are_const = 0;
    }

    return pointer_can_be_converted_to_dest_rec(orig->pointer->pointee, dest->pointer->pointee, 
            all_previous_are_const, to_void, derived_to_base, cv_adjustment, decl_context);
}

char pointer_can_be_converted_to_dest(type_t* orig, type_t* dest, 
        char* to_void, char* derived_to_base, char* cv_adjust,
        decl_context_t decl_context)
{
    if (orig == NULL
            || dest == NULL)
        return 0;

    // This holds for the first pointer
    char all_previous_are_const = 1;

    *to_void = 0;
    *derived_to_base = 0;
    *cv_adjust = 0;

    return pointer_can_be_converted_to_dest_rec(orig, dest, 
            &all_previous_are_const, to_void, derived_to_base, 
            cv_adjust, decl_context);
}

cv_qualifier_t get_cv_qualifier(type_t* type_info)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);
    return type_info->cv_qualifier;
}

char is_dependent_expression(AST expression, decl_context_t decl_context)
{
    ERROR_CONDITION(expression == NULL, "This cannot be null", 0);
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

                // TODO - Complete this
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
            // FIXME : "this" depends exclusively on the current context
        case AST_THIS_VARIABLE :
            {
                return 0;
            }
        case AST_SYMBOL :
        case AST_QUALIFIED_ID :
            {
                scope_entry_list_t* entry_list = 
                    query_id_expression(decl_context, expression);

                if (entry_list == NULL)
                {
                    internal_error("Symbol '%s' in '%s' not found\n", prettyprint_in_buffer(expression),
                            node_information(expression));
                }
                scope_entry_t* entry = entry_list->entry;

                if (entry->kind == SK_DEPENDENT_ENTITY
                        || entry->kind == SK_TEMPLATE_PARAMETER)
                {
                    entry->dependency_info = DI_DEPENDENT;
                    return 1;
                }

                // Maybe this is a const-variable initialized with a dependent expression
                if ((entry->kind == SK_VARIABLE
                        || entry->kind == SK_ENUMERATOR))
                {
                    if(entry->dependency_info == DI_UNKNOWN)
                    {
                        if (entry->expression_value != NULL)
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "Computing dependency of expression '%s'\n", 
                                        prettyprint_in_buffer(entry->expression_value));
                            }
                            // if (is_dependent_expression(entry->expression_value, decl_context))
                            if (is_dependent_expression(entry->expression_value, entry->decl_context))
                            {
                                entry->dependency_info = DI_DEPENDENT;
                                return 1;
                            }
                        }
                    }
                    else
                    {
                        // Dependency information has already been computed or
                        // it is being computed now
                        return (entry->dependency_info == DI_DEPENDENT);
                    }
                }

                char result;
                result = is_dependent_type(entry->type_information, decl_context);
                entry->dependency_info = result ? DI_DEPENDENT : DI_NOT_DEPENDENT;
                return result;
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
                            return 1;
                        }
                    }
                }

                return 0;
            }
        case AST_EXPLICIT_TYPE_CONVERSION :
            {
                AST type_specifier = duplicate_ast(ASTSon0(expression));

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
                            return 1;
                        }
                    }
                }

                return 0;
            }
        case AST_TYPENAME_EXPLICIT_TYPE_CONVERSION :
            {
                // This typename denotes that this will be dependent
                return 1;
            }
        case AST_TYPENAME_TEMPLATE :
        case AST_TYPENAME_TEMPLATE_TEMPLATE :
            {
                // This is always dependent
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

                if (abstract_declarator != NULL)
                {
                    type_t* declarator_type = NULL;
                    // Fix this
                    build_scope_declarator(abstract_declarator, &gather_info, simple_type_info, 
                            &declarator_type, decl_context);
                }

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

                if (abstract_declarator != NULL)
                {
                    type_t* declarator_type = NULL;
                    // Fix this
                    build_scope_declarator(abstract_declarator, &gather_info, simple_type_info, 
                            &declarator_type, decl_context);
                }

                if (is_dependent_type(simple_type_info, decl_context))
                {
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
        case AST_TEMPLATE_ID :
            {
                // You thought that  a template_id cannot designate an expression ?
                // Wrong, template functions can be explicitly called with template_id

                AST template_argument_list = ASTSon1(expression);

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
                                        return 1;
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
                                        return 1;

                                    break;
                                }
                            default:
                                break;
                        }
                    }
                }
                
                return 0;
                break;
            }
        default :
            {
                internal_error("Unexpected node '%s' %s", ast_print_node_type(ASTType(expression)), 
                        node_information(expression));
                break;
            }
            return 0;
    }
}

char is_dependent_simple_type(type_t* type_info, decl_context_t decl_context)
{
    if (type_info == NULL)
        return 0;

    if (type_info->kind != TK_DIRECT)
    {
        internal_error("This function expects a direct type\n", 0);
    }

    simple_type_t* simple_type = type_info->type;

    switch (simple_type->kind)
    {
        case STK_TEMPLATE_DEPENDENT_TYPE :
            {
                return 1;
            }
        case STK_TYPEDEF :
            {
                return is_dependent_type(simple_type->aliased_type, decl_context);
                break;
            }
        case STK_USER_DEFINED :
            {
                if (simple_type->user_defined_type->dependency_info == DI_UNKNOWN)
                {
                    // We have to compute it
                    scope_entry_t* symbol = simple_type->user_defined_type;
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
                    if (!result && symbol->is_member)
                    {
                        result |= is_dependent_type(symbol->class_type, decl_context);
                    }
                    
                    symbol->dependency_info =
                        (result ? DI_DEPENDENT : DI_NOT_DEPENDENT);
                    return result;
                }
                else
                {
                    return (simple_type->user_defined_type->dependency_info == DI_DEPENDENT);
                }
                break;
            }
        case STK_ENUM :
            {
                enum_info_t* enum_info = simple_type->enum_info;

                int i;
                for (i = 0; i < enum_info->num_enumeration; i++)
                {
                    scope_entry_t* entry = enum_info->enumeration_list[i];

                    if (entry->expression_value != NULL
                            && entry->dependency_info == DI_UNKNOWN)
                    {
                        if (is_dependent_expression(entry->expression_value, entry->decl_context))
                        {
                            entry->dependency_info = DI_DEPENDENT;
                            return 1;
                        }
                        else
                        {
                            entry->dependency_info = DI_NOT_DEPENDENT;
                        }
                    }
                    else
                    {
                        if (entry->dependency_info == DI_DEPENDENT)
                        {
                            return 1;
                        }
                    }
                }

                return 0;
                break;
            }
        case STK_CLASS :
            {
                if (simple_type->template_arguments != NULL)
                {
                    int i;
                    for (i = 0; i < simple_type->template_arguments->num_arguments; i++)
                    {
                        template_argument_t* curr_argument = simple_type->template_arguments->argument_list[i];

                        if (curr_argument->kind == TAK_TYPE)
                        {
                            if (is_dependent_type(curr_argument->type, decl_context))
                            {
                                return 1;
                            }
                        }
                        else if (curr_argument->kind == TAK_TEMPLATE)
                        {
                            // We are checking the arguments of a template
                            // where the argument is template template.
                            //
                            // E.g.
                            //
                            // template <typename>
                            // struct A {};
                            //
                            // template <template<typename> V = A>
                            // struct B {};
                            //
                            // B<> is not a dependent type, because 'V' has a nondependent 'type' value,
                            // the problem here, is that the argument is bound to a template-id rather
                            // than a full typename so we have to ensure that this type is actually
                            // a primary template. Other types are not possible, only other template template
                            // parameters like this case below
                            //
                            // template <template<typename> V = A, template <typename> W = V>
                            // struct C {};
                            type_t* arg_type = advance_over_typedefs(curr_argument->type);

                            ERROR_CONDITION(!is_named_type(arg_type), "Expecting a named type here", 0);

                            scope_entry_t* entry = named_type_get_symbol(arg_type);
                            if (entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
                            {
                                return 0;
                            }
                            else if (entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
                            {
                                return 1;
                            }
                            else
                            {
                                internal_error("Code unreachable entry->kind==%d", entry->kind);
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
                }
                return 0;
                break;
            }
        case STK_BUILTIN_TYPE :
        case STK_VA_LIST :
            {
                return 0;
            }
        case STK_TYPEOF :
            {
                AST typeof_expr = simple_type->typeof_expr;
                decl_context_t typeof_decl_context = simple_type->typeof_decl_context;

                if(simple_type->typeof_is_expr)
                {
                    return is_dependent_expression(typeof_expr, 
                            typeof_decl_context);
                }
                else
                {
                    AST type_id = typeof_expr;

                    AST type_specifier = ASTSon0(type_id);
                    AST abstract_declarator = ASTSon1(type_id);

                    gather_decl_spec_t gather_info;
                    memset(&gather_info, 0, sizeof(gather_info));

                    type_t* simple_type_info = NULL;
                    // Fix this
                    build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info, 
                            typeof_decl_context);

                    if (abstract_declarator != NULL)
                    {
                        type_t* declarator_type = NULL;
                        // Fix this
                        build_scope_declarator(abstract_declarator, &gather_info, simple_type_info, 
                                &declarator_type, typeof_decl_context);
                    }

                    return (is_dependent_type(simple_type_info, decl_context));
                }
            }
        default :
            {
                internal_error("Unknown simple type kind=%d\n", simple_type->kind);
            }
    }
}

char is_dependent_type(type_t* type, decl_context_t decl_context)
{
    ERROR_CONDITION(type == NULL, "This cannot be null", 0);

    type = advance_over_typedefs(type);

    switch (type->kind)
    {
        case TK_DIRECT :
            {
                return is_dependent_simple_type(type, decl_context);
                break;
            }
        case TK_ARRAY :
            {
                return is_dependent_type(type->array->element_type, decl_context)
                    || is_dependent_expression(type->array->array_expr,
                            type->array->array_expr_decl_context);
                break;
            }
        case TK_FUNCTION :
            {
                if (type->function->return_type != NULL
                        && is_dependent_type(type->function->return_type, decl_context))
                {
                    return 1;
                }

                int i;
                for (i = 0; i < type->function->num_parameters; i++)
                {
                    if (!type->function->parameter_list[i]->is_ellipsis
                            && is_dependent_type(type->function->parameter_list[i]->type_info, decl_context))
                    {
                        return 1;
                    }
                }

                return 0;
                break;
            }
        case TK_POINTER :
        case TK_REFERENCE :
            {
                return is_dependent_type(type->pointer->pointee, decl_context);
                break;
            }
        case TK_POINTER_TO_MEMBER :
            {
                return is_dependent_type(type->pointer->pointee, decl_context)
                    || is_dependent_type(type->pointer->pointee_class->type_information, decl_context);
                break;
            }
        default:
            {
                internal_error("Unknown type kind %d\n", type->kind);
                break;
            }
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

    if (result->injected_class_name)
    {
        result = result->injected_class_referred_symbol;
    }

    return result;
}

static char* get_cv_qualifier_string(type_t* type_info)
{
    char* result = "";

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
            || type_info->kind == TK_REFERENCE)
    {
        type_t* pointee = type_info->pointer->pointee;
        result = (pointee->kind != TK_POINTER_TO_MEMBER
                && pointee->kind != TK_POINTER
                && pointee->kind != TK_REFERENCE
                && pointee->kind != TK_DIRECT);
    }

    return result;
}

// Gives a string with the name of this simple type
static char* get_simple_type_name_string_internal(decl_context_t decl_context, simple_type_t* simple_type)
{
    ERROR_CONDITION(simple_type == NULL, "This cannot be null", 0);

    char* result = strdup("");
    switch ((int)simple_type->kind)
    {
        case STK_USER_DEFINED :
            {
                // Fix this
                scope_entry_t* entry = simple_type->user_defined_type;

                // They do not have type
                if (entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                        || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                        || entry->kind == SK_TEMPLATE_PARAMETER)
                {
                    result = entry->symbol_name;
                }
                else
                {
                    char is_dependent = 0;
                    int max_level = 0;
                    result = get_fully_qualified_symbol_name(simple_type->user_defined_type,
                            decl_context, &is_dependent, &max_level);

                    // If is a dependent name and it is qualified then it can be
                    // given a "typename" keyword (in some cases one must do that)
                    if (is_dependent && max_level > 0)
                    {
                        result = strappend("typename ", result);
                    }
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
                            result = strappend(result, "int ");
                            break;
                        }
                    case BT_CHAR :
                        {
                            result = strappend(result, "char ");
                            break;
                        }
                    case BT_WCHAR :
                        {
                            result = strappend(result, "wchar_t ");
                            break;
                        }
                    case BT_FLOAT :
                        {
                            result = strappend(result, "float ");
                            break;
                        }
                    case BT_DOUBLE :
                        {
                            result = strappend(result, "double ");
                            break;
                        }
                    case BT_BOOL :
                        {
                            result = strappend(result, "bool ");
                            break;
                        }
                    case BT_VOID :
                        {
                            result = strappend(result, "void ");
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
char* get_simple_type_name_string(decl_context_t decl_context, type_t* type_info)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);

    char* result = strdup("");
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
        case TK_REFERENCE :
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

static char* get_type_name_string(decl_context_t decl_context,
        type_t* type_info, 
        const char* symbol_name,
        int* num_parameter_names,
        char*** parameter_names,
        char is_parameter);

// Returns a declaration string given a type, a symbol name, an optional initializer
// and a semicolon
char* get_declaration_string_internal(type_t* type_info, 
        decl_context_t decl_context,
        const char* symbol_name, const char* initializer, 
        char semicolon,
        int* num_parameter_names,
        char*** parameter_names,
        char is_parameter)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);

    char* base_type_name = get_simple_type_name_string(decl_context, type_info);
    char* declarator_name = get_type_name_string(decl_context, type_info, symbol_name, 
            num_parameter_names, parameter_names, is_parameter);

    char* result;

    result = base_type_name;
    result = strappend(result, " ");

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
        char** left,
        char** right,
        int* num_parameter_names,
        char*** parameter_names,
        char is_parameter);

static char* get_type_name_string(decl_context_t decl_context,
        type_t* type_info, 
        const char* symbol_name,
        int* num_parameter_names,
        char*** parameter_names,
        char is_parameter)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);

    char* left = strdup("");
    char* right = strdup("");
    get_type_name_str_internal(decl_context, type_info, &left, &right, 
            num_parameter_names, parameter_names, is_parameter);

    char* result = strappend(left, symbol_name);
    result = strappend(result, right);

    return result;
}


// Constructs a proper declarator
static void get_type_name_str_internal(decl_context_t decl_context,
        type_t* type_info, 
        char** left,
        char** right,
        int* num_parameter_names,
        char*** parameter_names,
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
        case TK_REFERENCE :
            {
                get_type_name_str_internal(decl_context, type_info->pointer->pointee, left, right, 
                        num_parameter_names, parameter_names, is_parameter);

                if (declarator_needs_parentheses(type_info))
                {
                    (*left) = strappend((*left), "(");
                }

                (*left) = strappend((*left), "&");

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
                    char* array_expr = strdup("[0]");

                    (*right) = strappend((*right), array_expr);

                    get_type_name_str_internal(decl_context, type_info->array->element_type, left, right, 
                            num_parameter_names, parameter_names, is_parameter);
                }
                else
                {
                    char* array_expr = strappend("[", prettyprint_in_buffer(type_info->array->array_expr));
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

                char* prototype;
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
                            char* parameter_name = calloc(20, sizeof(char));
                            snprintf(parameter_name, 19, "_p_%d", i);

                            P_LIST_ADD((*parameter_names), (*num_parameter_names), parameter_name);

                            prototype = strappend(prototype,
                                    get_declaration_string_internal(type_info->function->parameter_list[i]->type_info, decl_context, 
                                        parameter_name, "", 0, NULL, NULL, 1));
                        }
                    }
                }
                prototype = strappend(prototype, ") ");
                prototype = strappend(prototype, get_cv_qualifier_string(type_info));

                (*right) = strappend((*right), prototype);
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
char *get_named_simple_type_name(scope_entry_t* user_defined_type)
{
    ERROR_CONDITION(user_defined_type == NULL, "This cannot be null", 0);

    char* result = "";
    decl_context_t decl_context = user_defined_type->decl_context;

    const int MAX_LENGTH = 1023;
    char* user_defined_str = calloc(MAX_LENGTH + 1, sizeof(char));

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
                        print_declarator(aliased_type, decl_context));
            }
            break;
        case SK_TEMPLATE_TYPE_PARAMETER :
            snprintf(user_defined_str, MAX_LENGTH, "type-template parameter (%s) (%d,%d)",
                    user_defined_type->symbol_name,
                    user_defined_type->template_parameter_nesting,
                    user_defined_type->template_parameter_position);
            break;
        case SK_TEMPLATE_TEMPLATE_PARAMETER :
            snprintf(user_defined_str, MAX_LENGTH, "template-template parameter (%s) (%d,%d)",
                    user_defined_type->symbol_name,
                    user_defined_type->template_parameter_nesting,
                    user_defined_type->template_parameter_position);
            break;
        case SK_TEMPLATE_PARAMETER :
            snprintf(user_defined_str, MAX_LENGTH, "nontype-template parameter (%s) (%d,%d)", 
                    user_defined_type->symbol_name,
                    user_defined_type->template_parameter_nesting,
                    user_defined_type->template_parameter_position);
            break;
        case SK_TEMPLATE_PRIMARY_CLASS :
            snprintf(user_defined_str, MAX_LENGTH, "primary template class %s {%p, %s:%d}", 
                    user_defined_type->symbol_name, 
                    user_defined_type,
                    user_defined_type->file,
                    user_defined_type->line);
            break;
        case SK_TEMPLATE_SPECIALIZED_CLASS :
            snprintf(user_defined_str, MAX_LENGTH, "specialized template class %s {%p, %s:%d}", 
                    user_defined_type->symbol_name, 
                    user_defined_type,
                    user_defined_type->file,
                    user_defined_type->line);
            break;
        case SK_GCC_BUILTIN_TYPE :
            snprintf(user_defined_str, MAX_LENGTH, "__builtin_va_list");
            break;
        case SK_DEPENDENT_ENTITY :
            snprintf(user_defined_str, MAX_LENGTH, "dependent entity");
            break;
        default :
            snprintf(user_defined_str, MAX_LENGTH, "¿¿¿unknown user defined type??? (kind=%d)", user_defined_type->kind);
    }
    result = strappend(result, user_defined_str);

    return result;
}

char* get_named_type_name(scope_entry_t* entry)
{
    ERROR_CONDITION(entry == NULL, "This cannot be null", 0);
    return get_named_simple_type_name(entry);
}

// Gives the name of a builtin type
char* get_builtin_type_name(simple_type_t* simple_type_info, decl_context_t decl_context)
{
    ERROR_CONDITION(simple_type_info == NULL, "This cannot be null", 0);
    char* result = "";

    if (simple_type_info->is_long)
    {
        result = strappend(result, "long ");
    }

    if (simple_type_info->is_short)
    {
        result = strappend(result, "short ");
    }

    if (simple_type_info->is_unsigned)
    {
        result = strappend(result, "unsigned ");
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
            result = strappend(result, "enum <anonymous>");
            break;
        case STK_CLASS :
            result = strappend(result, "class <anonymous>");
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
                snprintf(c, 255, "template dependent type '%s'", 
                        prettyprint_in_buffer(simple_type_info->typeof_expr));
                result = strappend(result, c);
            }
            break;
        case STK_TYPEDEF :
            result = strappend(result, print_declarator(advance_over_typedefs(simple_type_info->aliased_type), decl_context));
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
char* print_declarator(type_t* printed_declarator, decl_context_t decl_context)
{
    ERROR_CONDITION(printed_declarator == NULL, "This cannot be null", 0);
    char* tmp_result = "";

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
                    tmp_result = strappend(tmp_result, get_builtin_type_name(printed_declarator->type, decl_context));
                }
                else
                {
                    tmp_result = strappend(tmp_result, "(nothing)");
                }
                printed_declarator = NULL;
                break;
            case TK_POINTER :
                tmp_result = strappend(tmp_result, "pointer to ");
                printed_declarator = printed_declarator->pointer->pointee;
                break;
            case TK_REFERENCE :
                tmp_result = strappend(tmp_result, "reference to ");
                printed_declarator = printed_declarator->pointer->pointee;
                break;
            case TK_POINTER_TO_MEMBER :
                tmp_result = strappend(tmp_result, "pointer to member of ");
                if (printed_declarator->pointer->pointee_class != NULL)
                {
                    tmp_result = strappend(tmp_result,
                            print_declarator(printed_declarator->pointer->pointee_class->type_information, decl_context)
                          );
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
                    
                    if (printed_declarator->function->num_template_parameters > 0)
                    {
                        tmp_result = strappend(tmp_result, "<");
                        for (i = 0; i < printed_declarator->function->num_template_parameters; i++)
                        {
                            template_parameter_t* template_param = printed_declarator->function->template_parameter_info[i];
                            tmp_result = strappend(tmp_result, template_param->entry->symbol_name);

                            if ((i + 1) < printed_declarator->function->num_template_parameters)
                            {
                                tmp_result = strappend(tmp_result, ", ");
                            }
                        }
                        tmp_result = strappend(tmp_result, ">");
                    }

                    tmp_result = strappend(tmp_result, " (");
                    for (i = 0; i < printed_declarator->function->num_parameters; i++)
                    {
                        if (!printed_declarator->function->parameter_list[i]->is_ellipsis)
                        {
                            tmp_result = strappend(tmp_result, 
                                    print_declarator(printed_declarator->function->parameter_list[i]->type_info, decl_context)
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
                // GCC Extension
            default :
                internal_error("Unhandled type kind '%d'\n", printed_declarator->kind);
                break;
        }
    } while (printed_declarator != NULL);

    return tmp_result;
}
