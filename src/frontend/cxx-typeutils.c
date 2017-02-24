/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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
#include "cxx-type-trie.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-exprtype.h"
#include "cxx-prettyprint.h"
#include "cxx-driver.h"
#include "cxx-ambiguity.h"
#include "cxx-tltype.h"
#include "cxx-entrylist.h"
#include "cxx-codegen.h"
#include "cxx-nodecl-deep-copy.h"
#include "cxx-gccbuiltins.h"
#include "cxx-instantiation.h"
#include "cxx-typededuc.h"
#include "cxx-diagnostic.h"
#include "cxx-intelsupport.h"

#include "cxx-symbol-deep-copy.h"

#include "fortran03-scope.h"

#include "dhash_ptr.h"
#include "red_black_tree.h"

/*
 * --
 */

// An exception specifier used in function info
typedef struct {
    int num_exception_types;
    type_t** exception_type_seq;
} exception_spec_t;

// For type_t
enum type_kind
{
    TK_UNKNOWN = 0,
    TK_DIRECT,
    TK_POINTER,
    TK_LVALUE_REFERENCE,
    TK_RVALUE_REFERENCE,
    TK_REBINDABLE_REFERENCE,
    TK_POINTER_TO_MEMBER,
    TK_ARRAY,
    TK_FUNCTION,
    TK_OVERLOAD,
    TK_ELLIPSIS,
    TK_COMPUTED,
    TK_BRACED_LIST,
    TK_PACK,
    TK_SEQUENCE,
    TK_AUTO,
    TK_DECLTYPE_AUTO,
    TK_ERROR
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
    BT_OTHER_FLOAT,
    BT_BYTE,
    BT_CHAR,
    BT_WCHAR,
    BT_VOID,
    BT_NULLPTR_T,
    BT_CHAR16_T,
    BT_CHAR32_T,
} builtin_type_t;

typedef 
enum simple_type_kind_tag
{
    STK_UNDEFINED = 0, 
    STK_BUILTIN_TYPE, // int, float, char, wchar_t, bool, void;
    STK_COMPLEX,
    STK_CLASS, // struct {identifier};
    STK_ENUM, // enum {identifier}
    STK_INDIRECT, // A type defined after the type of another symbol
    STK_TEMPLATE_TYPE, // a template type
    STK_TEMPLATE_ALIAS, // an alias
    STK_TEMPLATE_DEPENDENT_TYPE, // 
    // Mercurium extensions
    STK_VECTOR,
    STK_MASK,
    // GCC Extensions
    STK_VA_LIST, // __builtin_va_list {identifier};
    STK_TYPEOF,  //  __typeof__(int) {identifier};
    STK_TYPE_DEP_EXPR, // unknown dependent type
    STK_UNDERLYING, // __underlying_type(E) {identifier}
    // Fortran
    STK_HOLLERITH,
} simple_type_kind_t;

// Information of enums
typedef 
struct enum_information_tag {
    int num_enumeration; // Number of enumerations declared for this enum
    scope_entry_t** enumeration_list; // The symtab entry of the enum
    type_t* underlying_type; // The underlying type of this enum type
    // The underlying type of this enum type when it concerns to conversions
    // (may be different to underlying_type if !underlying_type_is_fixed)
    type_t* underlying_type_for_conversion;
    _Bool underlying_type_is_fixed:1; // The underlying type is fixed through the syntax
    _Bool is_scoped:1; // This is a scoped enumerator (C++11)
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
    _Bool is_virtual:1;
    // A dependent base
    _Bool is_dependent:1;
    // An expansion base
    _Bool is_expansion:1;

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
struct class_info_tag {
    // Kind of class {struct, class}
    enum type_tag_t class_kind:4;

    // Currently unused
    _Bool is_local_class:1;

    // Is abstract class
    _Bool is_abstract:1;

    // Is a packed class (SEQUENCE in Fortran or __attribute__((packed)) )
    _Bool is_packed:1;

    // Was created after a lambda expression in C++
    _Bool is_lambda:1;

    // Enclosing class type
    type_t* enclosing_class_type;

    // The inner decl context created by this class
    const decl_context_t* inner_decl_context;

    // All members must be here
    scope_entry_list_t* members;
    // This is a literal list of member declarations
    // We allow it to be NULL (in this case codegen will use member and the
    // traditional algorithm), otherwise it will trust this list
    int num_member_declarations;
    member_declaration_info_t *member_declarations;

    // Destructor
    scope_entry_t* destructor;

    // Default constructor
    scope_entry_t* default_constructor;

    // Base (parent classes) info
    int num_bases;
    base_class_info_t** base_classes_list;

    // Virtual base info (used only when laying types)
    int num_virtual_bases;
    virtual_base_class_info_t** virtual_base_classes_list;

    // Friends
    scope_entry_list_t* friends;

    // Inherited constructors
    scope_entry_list_t* inherited_constructors;

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
    simple_type_kind_t kind:5;

    // if Kind == STK_BUILTIN_TYPE here we have
    // the exact builtin type
    builtin_type_t builtin_type:4;

    // This can be 0, 1 (long), 2 (long long) or 3 (__int128)
    unsigned int is_long:2;
    // short
    _Bool is_short:1;
    // unsigned
    _Bool is_unsigned:1;
    // signed
    _Bool is_signed:1;

    // States that the STK_INDIRECT is a not the last indirect
    _Bool is_indirect:1;

    // States that the STK_INDIRECT cannot be cached because the referred
    // symbol may at some point change its type. This happens in Fortran
    _Bool is_mutable:1;

    // States that the type is a transparent union (GCC extension)
    _Bool is_transparent_union:1;

    // States that this STK_TYPEOF is a decltype and not a __typeof__
    _Bool is_decltype:1;

    // Floating type model, only for BT_FLOAT, BT_DOUBLE and BT_OTHER_FLOAT
    floating_type_info_t* floating_info;

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

    // Decl environment where this type was declared if not builtin The scope
    // where this type was declared since sometimes, types do not have any name
    // related to them
    // (kind == STK_ENUM)
    // (kind == STK_CLASS)
    const decl_context_t* type_decl_context;

    // For typeof and template dependent types
    // (kind == STK_TYPEOF)
    nodecl_t typeof_expr;
    const decl_context_t* typeof_decl_context;

    // This is a STK_INDIRECT
    type_t* primary_specialization;
    // Sometimes we need the original symbol defining this template type
    scope_entry_t* related_template_symbol;

    // Specialized types
    //   Unique specialized types
    int num_unique_specialized_types;
    type_t** unique_specialized_types;
    //   All specialized types
    int num_all_specialized_types;
    type_t** all_specialized_types;

    // Template dependent types (STK_TEMPLATE_DEPENDENT_TYPE)
    scope_entry_t* dependent_entry;
    nodecl_t dependent_parts;
    enum type_tag_t dependent_entry_kind;

    // Underlying type (STK_UNDERLYING)
    type_t* underlying_type;

    // Complex types, base type of the complex type (STK_COMPLEX)
    type_t* complex_element;

    // STK_VECTOR
    // STK_MASK
    // For a mask vector_size is the number of bits of the mask
    // and vector_element is the underlying integer backing it (if any)
    type_t* vector_element;
    unsigned int vector_size;
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

    // Reference qualifier, only meaningful in C++2011
    ref_qualifier_t ref_qualifier:2;

    // States if this function has been declared or defined without prototype.
    // This is only meaningful in C but not in C++ where all functions do have
    // prototype
    _Bool lacks_prototype:1;
    // This is only meaningful in C++2011
    _Bool is_trailing:1;
} function_info_t;

// Pointers, references and pointers to members
typedef 
struct pointer_tag
{
    // The pointee type
    type_t* pointee;

    // If the type was a TK_POINTER_TO_MEMBER
    // the pointee class
    type_t* pointee_class_type;
} pointer_info_t;

typedef
struct array_region_tag 
{
    // Whole size. If the array is created using lower and upper boundaries
    // this one is upper - lower + 1
    nodecl_t whole_size;
    
    nodecl_t lower_bound; 

    // This is the upper bound. If the array is created using the size of the
    // array, this is always whole_size - 1
    nodecl_t upper_bound;
    
    // The stride of a region is 1 by default
    nodecl_t stride;
    
    // Scope of the array region expressions
    const decl_context_t* region_decl_context;
} array_region_t;

// Array information
typedef 
struct array_tag
{
    // Whole size. If the array is created using lower and upper boundaries
    // this one is upper - lower + 1
    nodecl_t whole_size;
    // This is always zero in C
    // It may be non zero in Fortran
    nodecl_t lower_bound; 

    // This is the upper bound. If the array is created using the size of the
    // array, this is always whole_size - 1
    nodecl_t upper_bound;

    // Scope of the array size expressions
    const decl_context_t* array_expr_decl_context;

    // The type of the array elements
    type_t* element_type;

    // Region information
    array_region_t* region;
    
    // Is a doped array (array with an in-memory descriptor)
    _Bool with_descriptor:1;
    // Is literal string type ?
    _Bool is_string_literal:1;
    _Bool is_vla:1;
} array_info_t;

typedef
struct braced_list_info_tag
{
    int num_types;
    type_t** types;
} braced_list_info_t;

typedef
struct common_type_info_tag
{
    // If not NULL, points to the nonvariant type
    // (if NULL this type is not a variant)
    type_t* nonvariant;
    // See below for more detailed descriptions
    _Bool is_template_specialized_type:1;
    _Bool valid_size:1;

    _Bool is_dependent:1;
    _Bool is_incomplete:1;
    _Bool is_interoperable:1;
    _Bool is_zero_type:1;
    _Bool is_atomic_type:1;

    // Other attributes that do not have specific representation
    int num_gcc_attributes;
    gcc_attribute_t* gcc_attributes;

    int num_ms_attributes;
    gcc_attribute_t* ms_attributes;

    // The sizeof and alignment of the type
    // They are only valid once 'valid_size' is true
    // --> char valid_size;
    _size_t size;
    // May be overriden by an 'aligned' attribute in gcc_attributes
    _size_t alignment;
    // This is here only for C++
    _size_t data_size;
} common_type_info_t;

typedef
struct pack_type_info_tag
{
    type_t* packed;
} pack_type_info_t;

typedef
struct sequence_type_info_tag
{
    int num_types;
    type_t** types;
} sequence_type_info_t;

// This is the basic type information, except for kind and cv-qualifier
// all fields have to be pointers, common fields that are not pointers are
// stored in the struct of the field 'info'
struct type_tag
{
    // Kind of the type
    // (all types)
    enum type_kind kind:5;

    // cv-qualifier related to this type
    // The cv-qualifier is in the type
    // (all types)
    cv_qualifier_t cv_qualifier;

    // We use a pointer so we can safely copy in cv-qualified versions
    // (all types)
    common_type_info_t* info;

    // Unqualified type, itself if the type is not qualified
    // (all types)
    type_t* unqualified_type;

    union {

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

        // Braced list type
        // (kind == TK_BRACED_LIST)
        braced_list_info_t* braced_type;

        // Computed function type
        // A parameterized function type (implemented in the compiler)
        // (kind == TK_COMPUTED)
        computed_function_type_t compute_type_function;

        // Expanded type
        // (kind == TK_PACK)
        pack_type_info_t* pack_type;

        // Sequence type
        // (kind == TK_SEQUENCE)
        sequence_type_info_t* sequence_type;
    };

    // For template specialized parameters and template types
    // (kind == TK_DIRECT && (type->kind == STK_CLASS || type->kind == STK_TEMPLATE_TYPE))
    // (kind == TK_FUNCTION)
    template_parameter_list_t* template_parameters;
    // For template specialized types and unresolved overloads
    // (kind == TK_DIRECT && type->kind == STK_CLASS)
    // (kind == TK_FUNCTION)
    template_parameter_list_t* template_arguments;
    // For template specialized types and unresolved overloads
    // (kind == TK_DIRECT && type->kind == STK_CLASS)
    // (kind == TK_FUNCTION)
    type_t* related_template_type;

    // Cache typedefs
    type_t* _advanced_type;
};

static common_type_info_t* new_common_type_info(void)
{
    common_type_info_t* result = NEW0(common_type_info_t);
    return result;
}

static common_type_info_t* copy_common_type_info(common_type_info_t* t)
{
    common_type_info_t* result = new_common_type_info();
    *result = *t;

    result->gcc_attributes = NEW_VEC(gcc_attribute_t, result->num_gcc_attributes);
    memcpy(result->gcc_attributes, t->gcc_attributes, result->num_gcc_attributes * sizeof(*result->gcc_attributes));

    result->ms_attributes = NEW_VEC(gcc_attribute_t, result->num_ms_attributes);
    memcpy(result->ms_attributes, t->ms_attributes, result->num_ms_attributes * sizeof(*result->ms_attributes));

    return result;
}

static type_t* copy_type_for_class_alias(type_t* t)
{
    type_t* result = NEW0(type_t);
    *result = *t;

    result->_advanced_type = NULL;

    result->info = copy_common_type_info(t->info);

    return result;
}

static type_t* copy_type_for_variant(type_t* t)
{
    ERROR_CONDITION(t->cv_qualifier != CV_NONE,
            "Invalid type to copy for variant: it must be unqualified", 0);

    type_t* result = NEW0(type_t);
    *result = *t;

    result->unqualified_type = result;

    result->_advanced_type = NULL;

    result->info = copy_common_type_info(t->info);

    if (result->info->nonvariant == NULL)
        result->info->nonvariant = t;

    return result;
}

extern inline char is_variant_type(type_t* t)
{
    return t->info->nonvariant != NULL;
}

extern inline type_t* variant_type_get_nonvariant(type_t* t)
{
    return t->info->nonvariant;
}

static type_t* new_empty_type_without_info(void)
{
    type_t* result = NEW0(type_t);
    return result;
}

static type_t* new_empty_type(void)
{
    type_t* result = new_empty_type_without_info();
    result->info = new_common_type_info();
    return result;
}

const standard_conversion_t no_scs_conversion = { 
    .orig = NULL,
    .dest = NULL,
    .conv = { SCI_NO_CONVERSION, SCI_NO_CONVERSION, SCI_NO_CONVERSION } 
};


size_t get_type_t_size(void)
{
    return sizeof(type_t);
}

// Forward declarations of functions used during structural type equivalence
static char equivalent_simple_types(type_t *t1, type_t *t2);
static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2);
static char equivalent_array_type(array_info_t* t1, array_info_t* t2);
static char equivalent_function_type(type_t* t1, type_t* t2);
static char compatible_parameters(function_info_t* t1, function_info_t* t2);
static char compare_template_dependent_typename_types(type_t* t1, type_t* t2);
static char equivalent_pointer_to_member_type(type_t* t1, type_t* t2);
static char equivalent_named_types(scope_entry_t* s1, scope_entry_t* s2);
static char equivalent_vector_type(type_t* t1, type_t* t2);
static char equivalent_builtin_type(type_t* t1, type_t *t2);

static char equivalent_pack_types(type_t* t1, type_t *t2);
static char equivalent_sequence_types(type_t* t1, type_t *t2);
static char equivalent_braced_types(type_t* t1, type_t *t2);


/* Type constructors : Builtins */

static type_t* get_simple_type(void)
{
    type_t* result = new_empty_type();
    result->kind = TK_DIRECT;
    result->type = NEW0(simple_type_t);
    result->unqualified_type = result;
    return result;
}

extern inline type_t* get_unsigned_byte_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_BYTE;
        _type->type->is_unsigned = 1;
        _type->info->size = 1;
        _type->info->alignment = 1;
        _type->info->valid_size = 1;
    }

    return _type;
}

extern inline type_t* get_signed_byte_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_BYTE;
        _type->type->is_signed = 1;
        _type->info->size = 1;
        _type->info->alignment = 1;
        _type->info->valid_size = 1;
    }

    return _type;
}

extern inline type_t* get_char_type(void)
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

extern inline type_t* get_char16_t_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_CHAR16_T;
        _type->info->size = 2;
        _type->info->alignment = 2;
        _type->info->valid_size = 1;
    }

    return _type;
}

extern inline type_t* get_char32_t_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_CHAR32_T;
        _type->info->size = 4;
        _type->info->alignment = 4;
        _type->info->valid_size = 1;
    }

    return _type;
}

extern inline type_t* get_signed_char_type(void)
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

extern inline type_t* get_unsigned_char_type(void)
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

extern inline type_t* get_wchar_t_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        if (IS_CXX_LANGUAGE)
        {
            _type = get_simple_type();
            _type->type->kind = STK_BUILTIN_TYPE;
            _type->type->builtin_type = BT_WCHAR;
            _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_wchar_t;
            _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_wchar_t;
            _type->info->valid_size = 1;
        }
        else
        {
            _type = (CURRENT_CONFIGURATION->type_environment->int_type_of_wchar_t)();
        }
    }

    return _type;
}

extern inline type_t* get_bool_type(void)
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

extern inline type_t* get_bool_of_integer_type(type_t* t)
{
    ERROR_CONDITION(!is_integer_type(t), "Invalid type for bool", 0);

    static type_t* _bool_types[MCXX_MAX_BYTES_INTEGER + 1] = { 0 };

    _size_t s = type_get_size(t);

    ERROR_CONDITION(s > MCXX_MAX_BYTES_INTEGER, "Integer of size %d too big for bool", s);

    if (_bool_types[s] == NULL)
    {
        _bool_types[s] = get_simple_type();
        _bool_types[s]->type->kind = STK_BUILTIN_TYPE;
        _bool_types[s]->type->builtin_type = BT_BOOL;
        // We use this field to avoid adding another one in simple_type_t
        // If you put this info elsewhere, update get_integral_type_of_bool
        _bool_types[s]->type->complex_element = t;

        _bool_types[s]->info->size = type_get_size(t);
        _bool_types[s]->info->alignment = type_get_alignment(t);
        _bool_types[s]->info->valid_size = 1;
    }

    return _bool_types[s];
}

// This function may return NULL meaning this is a bool or _Bool
static type_t* get_integral_type_of_bool(type_t* t)
{
    return t->type->complex_element;
}

extern inline type_t* get_signed_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_signed = 1;
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_signed_int;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_signed_int;
        _type->info->valid_size = 1;
    }

    return _type;
}

extern inline type_t* get_signed_short_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_signed = 1;
        _type->type->is_short = 1;
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_signed_short;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_signed_short;
        _type->info->valid_size = 1;
    }

    return _type;
}

extern inline type_t* get_signed_long_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_signed = 1;
        _type->type->is_long = 1;
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_signed_long;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_signed_long;
        _type->info->valid_size = 1;
    }

    return _type;
}

extern inline type_t* get_signed_long_long_int_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_signed = 1;
        _type->type->is_long = 2;
        _type->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_signed_long_long;
        _type->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_signed_long_long;
        _type->info->valid_size = 1;
    }

    return _type;
}


extern inline type_t* get_unsigned_int_type(void)
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

extern inline type_t* get_size_t_type(void)
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

extern inline type_t* get_ptrdiff_t_type(void)
{
    if (!CURRENT_CONFIGURATION->disable_sizeof)
    {
        return (CURRENT_CONFIGURATION->type_environment->type_of_ptrdiff_t)();
    }
    else
    {
        return get_signed_int_type();
    }
}

extern inline type_t* get_unsigned_short_int_type(void)
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

extern inline type_t* get_unsigned_long_int_type(void)
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

extern inline type_t* get_unsigned_long_long_int_type(void)
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

extern inline type_t* get_signed_int128_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_signed = 1;
        _type->info->size = 16;
        _type->type->is_long = 3;
        _type->info->alignment = 16;
        _type->info->valid_size = 1;
    }

    return _type;
}


type_t* get_unsigned_int128_type(void)
{
    static type_t* _type = NULL;

    if (_type == NULL)
    {
        _type = get_simple_type();
        _type->type->kind = STK_BUILTIN_TYPE;
        _type->type->builtin_type = BT_INT;
        _type->type->is_unsigned = 1;
        _type->type->is_long = 3;
        _type->info->size = 16;
        _type->info->alignment = 16;
        _type->info->valid_size = 1;
    }

    return _type;
}

static int num_float_types = 0;
static type_t* float_types[MAX_DIFFERENT_FLOATS];

#if 0
static char same_floating_info(floating_type_info_t* info1, floating_type_info_t* info2)
{
    ERROR_CONDITION(info1 == NULL || info2 == NULL, "Invalid floating info", 0);
    return info1->size_of == info2->size_of
        && info1->align_of == info2->align_of
        && info1->bits == info2->bits
        && info1->base == info2->base
        && info1->p == info2->p
        && info1->emin == info2->emin
        && info1->emax == info2->emax;
}
#endif

extern inline type_t* get_floating_type_from_descriptor(floating_type_info_t* info)
{
    int i;
    for (i = 0; i < num_float_types; i++)
    {
        type_t* current_type = float_types[i];

        if (current_type->type->floating_info == info)
        {
            return current_type;
        }
    }

    ERROR_CONDITION(num_float_types == MAX_DIFFERENT_FLOATS, "Too many floats!", 0);

    type_t* type = get_simple_type();
    type->type->kind = STK_BUILTIN_TYPE;
    type->type->builtin_type = BT_OTHER_FLOAT;
    type->type->floating_info = info;
    type->info->size = info->size_of;
    type->info->alignment = info->align_of;
    type->info->valid_size = 1;

    // Known types of C
    if (CURRENT_CONFIGURATION->type_environment->float_info == info)
    {
        type->type->builtin_type = BT_FLOAT;
    }
    else if (CURRENT_CONFIGURATION->type_environment->double_info == info)
    {
        type->type->builtin_type = BT_DOUBLE;
    }
    else if (CURRENT_CONFIGURATION->type_environment->long_double_info == info)
    {
        type->type->builtin_type = BT_DOUBLE;
        type->type->is_long = 1;
    }

    float_types[num_float_types] = type;
    num_float_types++;

    return type;
}

extern inline const floating_type_info_t* floating_type_get_info(type_t* t)
{
    t = advance_over_typedefs(t);
    ERROR_CONDITION(!is_floating_type(t), "This is not a floating type", 0);
    return t->type->floating_info;
}

extern inline type_t* get_float_type(void)
{
    return get_floating_type_from_descriptor(CURRENT_CONFIGURATION->type_environment->float_info);
}

extern inline type_t* get_double_type(void)
{
    return get_floating_type_from_descriptor(CURRENT_CONFIGURATION->type_environment->double_info);
}

extern inline type_t* get_long_double_type(void)
{
    return get_floating_type_from_descriptor(CURRENT_CONFIGURATION->type_environment->long_double_info);
}

// defined in cxx-typeenviron.c
extern struct floating_type_info_tag binary_float_16;

extern inline type_t* get_float16_type(void)
{
    // We do not want to register this type to the list of floating types
    // because weird things happen in the FEs
    if (CURRENT_CONFIGURATION->type_environment->float16_info == NULL)
    {
        CURRENT_CONFIGURATION->type_environment->float16_info = &binary_float_16;
    }
    return get_floating_type_from_descriptor(CURRENT_CONFIGURATION->type_environment->float16_info);
}

// defined in cxx-typeenviron.c
extern struct floating_type_info_tag binary_float_128;

extern inline type_t* get_float128_type(void)
{
    if (CURRENT_CONFIGURATION->type_environment->float128_info == NULL)
    {
        warn_printf_at(NULL,
                "the current typing environment (%s) does not define a __float128 type but "
                "Mercurium was compiled with __float128 support\n",
                CURRENT_CONFIGURATION->type_environment->environ_name);
        CURRENT_CONFIGURATION->type_environment->float128_info = &binary_float_128;
    }
    return get_floating_type_from_descriptor(CURRENT_CONFIGURATION->type_environment->float128_info);
}

extern inline type_t* get_void_type(void)
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

extern inline type_t* get_typeof_expr_dependent_type(nodecl_t nodecl_expr, const decl_context_t* decl_context,
        char is_decltype)
{
    type_t* type = get_simple_type();

    type->type->kind = STK_TYPEOF;
    type->type->typeof_expr = nodecl_expr;
    type->type->typeof_decl_context = decl_context;

    type->type->is_decltype = is_decltype;

    // We always return a dependent type
    type->info->is_dependent = 1;
    return type;
}

extern inline char is_typeof_expr(type_t* t)
{
    t = advance_over_typedefs(t);
    return t != NULL
        && t->kind == TK_DIRECT
        && t->type->kind == STK_TYPEOF;
}

extern inline nodecl_t typeof_expr_type_get_expression(type_t* t)
{
    ERROR_CONDITION(!is_typeof_expr(t), "This is not a typeof type", 0);

    t = advance_over_typedefs(t);

    return t->type->typeof_expr;
}

extern inline const decl_context_t* typeof_expr_type_get_expression_context(type_t* t)
{
    ERROR_CONDITION(!is_typeof_expr(t), "This is not a typeof type", 0);

    t = advance_over_typedefs(t);

    return t->type->typeof_decl_context;
}

extern inline char typeof_expr_type_is_decltype(type_t* t)
{
    ERROR_CONDITION(!is_typeof_expr(t), "This is not a typeof type", 0);

    t = advance_over_typedefs(t);

    return t->type->is_decltype;
}

extern inline type_t* get_gcc_builtin_va_list_type(void)
{
    static type_t* result = NULL;

    if (result == NULL)
    {
        if (CURRENT_CONFIGURATION->type_environment->builtin_va_list_type == NULL)
        {
            result = get_simple_type();

            result->type->kind = STK_VA_LIST;

            result->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_builtin_va_list;
            result->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_builtin_va_list;
            result->info->valid_size = 1;
        }
        else
        {
            result = (CURRENT_CONFIGURATION->type_environment->builtin_va_list_type)();
        }
    }

    return result;
}

extern inline char is_gcc_builtin_va_list(type_t *t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_VA_LIST);
}

extern inline type_t* get_gxx_underlying_type(type_t* t)
{
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    type_t* result = get_simple_type();

    result->type->kind = STK_UNDERLYING;
    result->type->underlying_type = t;

    // This type is used in dependent contexts always
    result->info->is_dependent = 1;

    return result;
}

extern inline char is_gxx_underlying_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return t != NULL
        && t->kind == TK_DIRECT
        && t->type->kind == STK_UNDERLYING;
}

extern inline type_t* gxx_underlying_type_get_underlying_type(type_t* t)
{
    ERROR_CONDITION(!is_gxx_underlying_type(t), "Invalid type", 0);

    t = advance_over_typedefs(t);
    return t->type->underlying_type;
}

static void null_dtor(const void* v UNUSED_PARAMETER) { }

static void* rb_tree_query_uint(rb_red_blk_tree* tree, unsigned int u)
{
    type_t* result = NULL;
    rb_red_blk_node* n = rb_tree_query(tree, &u);
    if (n != NULL)
    {
        result = rb_node_get_info(n);
    }

    return result;
}

static int intptr_t_comp(const void *v1, const void *v2)
{
    intptr_t p1 = (intptr_t)(v1);
    intptr_t p2 = (intptr_t)(v2);

    if (p1 < p2)
        return -1;
    else if (p1 > p2)
        return 1;
    else
        return 0;
}

static int uint_comp(const void *v1, const void *v2)
{
    unsigned int p1 = *(unsigned int*)(v1);
    unsigned int p2 = *(unsigned int*)(v2);

    if (p1 < p2)
        return -1;
    else if (p1 > p2)
        return 1;
    else
        return 0;
}

static inline type_t* get_indirect_type_(scope_entry_t* entry, char indirect)
{
    type_t* type_info = entry->_indirect_type[!!indirect];

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
                || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                || entry->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                || entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
        {
            type_info->info->is_dependent = 1;
        }
        else if (entry->type_information != NULL)
        {
            type_info->info->is_dependent = is_dependent_type(entry->type_information);
        }

        entry->_indirect_type[!!indirect] = type_info;
    }

    return type_info;
}


extern inline type_t* get_user_defined_type(scope_entry_t* entry)
{
    return get_indirect_type_(entry,
            /* indirect */
            entry->kind == SK_TYPEDEF
            || entry->kind == SK_TEMPLATE_ALIAS);
}

extern inline type_t* get_immutable_indirect_type(scope_entry_t* entry)
{
    return get_indirect_type_(entry, /* indirect */ 1);
}

extern inline type_t* get_mutable_indirect_type(scope_entry_t* entry)
{
    type_t* t = get_indirect_type_(entry, /* indirect */ 1);
    t->type->is_mutable = 1;
    return t;
}

static char same_template_argument_list(
        template_parameter_list_t* template_parameter_list_1,
        template_parameter_list_t* template_parameter_list_2);

static int compare_dependent_parts(const void *v1, const void *v2)
{
    nodecl_t n1 = _nodecl_wrap((AST)v1);
    nodecl_t n2 = _nodecl_wrap((AST)v2);

    if (nodecl_is_null(n1) && nodecl_is_null(n2))
        return 0;
    if (nodecl_is_null(n1))
        return -1;
    if (nodecl_is_null(n2))
        return 1;

    n1 = nodecl_get_child(n1, 0);
    n2 = nodecl_get_child(n2, 0);

    if (nodecl_is_null(n1)
            && nodecl_is_null(n2))
        return 0;
    if (nodecl_is_null(n1))
        return -1;
    if (nodecl_is_null(n2))
        return 1;

    if (nodecl_list_length(n1) < nodecl_list_length(n2))
        return -1;
    if (nodecl_list_length(n1) > nodecl_list_length(n2))
        return 1;

    int num_items1 = 0;
    nodecl_t* list1 = nodecl_unpack_list(n1, &num_items1);
    int num_items2 = 0;
    nodecl_t* list2 = nodecl_unpack_list(n2, &num_items2);

    ERROR_CONDITION(num_items1 != num_items2, "This should not happen", 0);

    int i;
    for (i = 0; i < num_items1; i++)
    {
        if (nodecl_get_kind(list1[i]) < nodecl_get_kind(list2[i]))
        {
            DELETE(list1);
            DELETE(list2);
            return -1;
        }
        else if (nodecl_get_kind(list1[i]) > nodecl_get_kind(list2[i]))
        {
            DELETE(list1);
            DELETE(list2);
            return 1;
        }
        else
        {
            switch (nodecl_get_kind(list1[i]))
            {
                case NODECL_CXX_DEP_NAME_SIMPLE:
                    {
                        int cmp = strcmp(nodecl_get_text(list1[i]), nodecl_get_text(list2[i]));
                        if (cmp < 0)
                        {
                            DELETE(list1);
                            DELETE(list2);
                            return -1;
                        }
                        else if (cmp > 0)
                        {
                            DELETE(list1);
                            DELETE(list2);
                            return 1;
                        }

                        break;
                    }
                case NODECL_CXX_DEP_TEMPLATE_ID:
                    {
                        int cmp = strcmp(nodecl_get_text(list1[i]), nodecl_get_text(list2[i]));
                        if (cmp < 0)
                        {
                            DELETE(list1);
                            DELETE(list2);
                            return -1;
                        }
                        else if (cmp > 0)
                        {
                            DELETE(list1);
                            DELETE(list2);
                            return 1;
                        }

                        template_parameter_list_t *tpl1 = 
                                    nodecl_get_template_parameters(list1[i]);
                        template_parameter_list_t *tpl2 = 
                                    nodecl_get_template_parameters(list2[i]);

                        if (!same_template_argument_list(
                                    tpl1,
                                    tpl2))
                        {
                            if (tpl1->num_parameters < tpl2->num_parameters)
                            {
                                DELETE(list1);
                                DELETE(list2);
                                return -1;
                            }
                            else if (tpl1->num_parameters > tpl2->num_parameters)
                            {
                                DELETE(list1);
                                DELETE(list2);
                                return 1;
                            }

                            int k;
                            for (k = 0; k < tpl1->num_parameters; k++)
                            {
                                if (tpl1->arguments[k] < tpl2->arguments[k])
                                {
                                    DELETE(list1);
                                    DELETE(list2);
                                    return -1;
                                }
                                else if (tpl1->arguments[k] > tpl2->arguments[k])
                                {
                                    DELETE(list1);
                                    DELETE(list2);
                                    return 1;
                                }
                            }

                            // We know they were different, we should not reach here!
                            internal_error("Code unreachable", 0);
                        }


                        nodecl_t name1 = nodecl_get_child(list1[i], 0);
                        nodecl_t name2 = nodecl_get_child(list2[i], 0);

                        if (nodecl_get_kind(name1) < nodecl_get_kind(name2))
                        {
                            DELETE(list1);
                            DELETE(list2);
                            return -1;
                        }
                        else if (nodecl_get_kind(name1) > nodecl_get_kind(name2))
                        {
                            DELETE(list1);
                            DELETE(list2);
                            return 1;
                        }

                        switch (nodecl_get_kind(name1))
                        {
                            case NODECL_CXX_DEP_NAME_SIMPLE:
                                {
                                    cmp = strcmp(nodecl_get_text(name1), nodecl_get_text(name2));
                                    if (cmp < 0)
                                    {
                                        DELETE(list1);
                                        DELETE(list2);
                                        return -1;
                                    }
                                    else if (cmp > 0)
                                    {
                                        DELETE(list1);
                                        DELETE(list2);
                                        return 1;
                                    }
                                }
                                break;
                            default:
                                internal_error("Unexpected node '%s'\n", ast_print_node_type(nodecl_get_kind(name1)));
                        }

                        break;
                    }
                case NODECL_CXX_DEP_NAME_CONVERSION:
                    {
                        type_t* conversion1 = (!nodecl_is_null(nodecl_get_child(list1[i], 0))) ? nodecl_get_type(nodecl_get_child(list1[i], 0)) : NULL;
                        type_t* conversion2 = (!nodecl_is_null(nodecl_get_child(list2[i], 0))) ? nodecl_get_type(nodecl_get_child(list2[i], 0)) : NULL;
                        
                        if (!equivalent_types(conversion1, conversion2))
                        {
                            if (conversion1 < conversion2)
                            {
                                DELETE(list1);
                                DELETE(list2);
                                return -1;
                            }
                            else if (conversion1 > conversion2)
                            {
                                DELETE(list1);
                                DELETE(list2);
                                return 1;
                            }
                            // We know they were different, we should not reach here!
                            internal_error("Code unreachable", 0);
                        }
                        break;
                    }
                default:
                    internal_error("Unexpected node '%s'\n", ast_print_node_type(nodecl_get_kind(list1[i])));
                    break;
            }
        }
    }

    DELETE(list1);
    DELETE(list2);

    return 0;
}

extern inline char is_valid_symbol_for_dependent_typename(scope_entry_t* entry)
{
    return entry->kind == SK_TEMPLATE_TYPE_PARAMETER
        || ((entry->kind == SK_CLASS
                    || entry->kind == SK_FUNCTION
                    || (IS_CXX11_LANGUAGE && entry->kind == SK_TEMPLATE_ALIAS))
                && is_dependent_type(entry->type_information))
        || (entry->kind == SK_TYPEDEF && is_typeof_expr(entry->type_information))
        || (entry->kind == SK_DECLTYPE);
}

// This function must always return a new type
extern inline type_t* get_dependent_typename_type_from_parts(scope_entry_t* dependent_entry, 
        nodecl_t dependent_parts)
{
    ERROR_CONDITION(!nodecl_is_null(dependent_parts) &&
            nodecl_get_kind(dependent_parts) != NODECL_CXX_DEP_NAME_NESTED, "Invalid nodecl", 0);

    char new_dependent_parts = 0;

    if (dependent_entry->kind == SK_DEPENDENT_ENTITY)
    {
        // Flatten dependent typenames
        type_t* indirect_dependent_type = dependent_entry->type_information;
        dependent_entry = indirect_dependent_type->type->dependent_entry;

        new_dependent_parts = 1;

        if (!nodecl_is_null(indirect_dependent_type->type->dependent_parts)
                && !nodecl_is_null(dependent_parts))
        {
            dependent_parts =
                nodecl_make_cxx_dep_name_nested(
                        nodecl_concat_lists(nodecl_shallow_copy(nodecl_get_child(indirect_dependent_type->type->dependent_parts, 0)),
                            nodecl_shallow_copy(nodecl_get_child(dependent_parts, 0))), 
                        nodecl_get_locus(indirect_dependent_type->type->dependent_parts));
        }
        else if (nodecl_is_null(indirect_dependent_type->type->dependent_parts))
        {
            dependent_parts = nodecl_shallow_copy(dependent_parts);
        }
        else // nodecl_is_null(dependent_parts)
        {
            dependent_parts = nodecl_shallow_copy(indirect_dependent_type->type->dependent_parts);
        }
    }

    if (dependent_entry->kind == SK_TYPEDEF)
    {
        type_t* t = advance_over_typedefs(dependent_entry->type_information);
        if (is_named_type(t))
            dependent_entry = named_type_get_symbol(t);
    }

    ERROR_CONDITION(!is_valid_symbol_for_dependent_typename(dependent_entry),
            "Invalid base symbol %s %s for dependent entry",
            dependent_entry->symbol_name,
            symbol_kind_name(dependent_entry));

    // Try to reuse an existing type
    static dhash_ptr_t *_dependent_entries = NULL;
    if (_dependent_entries == NULL)
    {
        _dependent_entries = dhash_ptr_new(5);
    }

    rb_red_blk_tree * dependent_entry_hash = dhash_ptr_query(_dependent_entries, (const char*)dependent_entry);

    if (dependent_entry_hash == NULL)
    {
        dependent_entry_hash = rb_tree_create(compare_dependent_parts, null_dtor, null_dtor);

        dhash_ptr_insert(_dependent_entries, (const char*)dependent_entry, dependent_entry_hash);
    }

    rb_red_blk_node* n = rb_tree_query(dependent_entry_hash, nodecl_get_ast(dependent_parts));

    type_t* result = NULL;
    if (n != NULL)
    {
        result = (type_t*)rb_node_get_info(n);

        if (new_dependent_parts)
        {
            nodecl_free(dependent_parts);
        }
    }
    else
    {
        result = get_simple_type();
        result->type->kind = STK_TEMPLATE_DEPENDENT_TYPE;
        result->info->is_dependent = 1;

        if (!new_dependent_parts)
        {
            dependent_parts = nodecl_shallow_copy(dependent_parts);
        }

        result->type->dependent_entry = dependent_entry;
        result->type->dependent_parts = dependent_parts;
        result->type->dependent_entry_kind = TT_TYPENAME;

        rb_tree_insert(dependent_entry_hash, nodecl_get_ast(dependent_parts), result);
    }

    return result;
}

extern inline char is_transparent_union(type_t* t)
{
    return (t != NULL && t->type->is_transparent_union);
}

extern inline void set_is_transparent_union(type_t* t, char is_transp_union)
{
    ERROR_CONDITION(t == NULL, "This type cannot be NULL", 0);
    t->type->is_transparent_union = is_transp_union;
}

extern inline enum type_tag_t get_dependent_entry_kind(type_t* t)
{
    ERROR_CONDITION(!is_dependent_typename_type(t),
            "This is not a dependent typename type", 0);

    return t->type->dependent_entry_kind;
}

extern inline type_t* set_dependent_entry_kind(type_t* t, enum type_tag_t kind)
{
    ERROR_CONDITION(!is_dependent_typename_type(t),
            "This is not a dependent typename type", 0);
    t->type->dependent_entry_kind = kind;

    // This function is seldomly used so it does not seem
    // necessary to keep the results in a hash
    type_t* result = get_simple_type();

    result->type->kind = STK_TEMPLATE_DEPENDENT_TYPE;
    result->info->is_dependent = 1;

    result->type->dependent_entry = t->type->dependent_entry;
    result->type->dependent_parts = t->type->dependent_parts;
    result->type->dependent_entry_kind = kind;

    return result;
}

extern inline void dependent_typename_get_components(type_t* t, 
        scope_entry_t** dependent_entry, 
        nodecl_t* dependent_parts)
{
    ERROR_CONDITION(!is_dependent_typename_type(t), "This is not a dependent typename", 0);
    t = advance_over_typedefs(t);

    *dependent_entry = t->type->dependent_entry;
    *dependent_parts = t->type->dependent_parts;
}

extern inline type_t* get_new_enum_type(const decl_context_t* decl_context, char is_scoped)
{
    type_t* type_info = get_simple_type();

    type_info->type->enum_info = NEW0(enum_info_t);
    type_info->type->kind = STK_ENUM;
    type_info->type->type_decl_context = decl_context;

    type_info->type->enum_info->is_scoped = is_scoped;

    // This is incomplete by default
    type_info->info->is_incomplete = 1;

    return type_info;
}

extern inline type_t* get_new_class_type(const decl_context_t* decl_context, enum type_tag_t class_kind)
{
    type_t* type_info = get_simple_type();

    type_info->type->class_info = NEW0(class_info_t);
    type_info->type->class_info->class_kind = class_kind;
    type_info->type->kind = STK_CLASS;
    type_info->type->type_decl_context = decl_context;

    // This is incomplete by default
    type_info->info->is_incomplete = 1;

    // Initially assume it is an aggregate

    return type_info;
}

enum type_tag_t class_type_get_class_kind(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);

    t = get_actual_class_type(t);

    return t->type->class_info->class_kind;
}

extern inline void class_type_set_class_kind(type_t* t, enum type_tag_t class_kind)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);

    t = get_actual_class_type(t);

    t->type->class_info->class_kind = class_kind;
}

static type_t* rewrite_redundant_typedefs(type_t* orig);

static type_t* simplify_types_template_arguments(type_t* t)
{
    // Canonicalize zero types
    if (is_zero_type(t))
    {
        t = variant_type_get_nonvariant(t);
    }

    // Canonicalize throw types
    if (is_throw_expr_type(t))
    {
        t = get_void_type();
    }

    // We remove nonlocal typedefs from everywhere in the type
    t = rewrite_redundant_typedefs(t);

    return t;
}

static template_parameter_list_t* simplify_template_arguments(template_parameter_list_t* template_arguments)
{
    int i;
    template_parameter_list_t* result = template_arguments;

    for (i = 0; i < result->num_parameters; i++)
    {
        if (result->arguments[i] != NULL)
        {
            switch (result->arguments[i]->kind)
            {
                case TPK_TYPE :
                    {
                        result->arguments[i]->type = simplify_types_template_arguments(result->arguments[i]->type);
                        break;
                    }
                case TPK_NONTYPE :
                    {
                        result->arguments[i]->type = simplify_types_template_arguments(result->arguments[i]->type);

                        if (result->parameters[i] != NULL
                                && nodecl_is_constant(result->arguments[i]->value)
                                && !is_dependent_type(result->parameters[i]->entry->type_information))
                        {
                            const_value_t* value = nodecl_get_constant(result->arguments[i]->value);
                            if (!is_enum_type(result->parameters[i]->entry->type_information)
                                    // We are not ready to handle these yet
                                    && !const_value_is_object(value)
                                    && !const_value_is_address(value))
                            {
                                result->arguments[i]->value =
                                    const_value_to_nodecl_with_basic_type(
                                            value,
                                            simplify_types_template_arguments(
                                                result->parameters[i]->entry->type_information
                                                ));
                            }
                        }
                        break;
                    }
                case TPK_TEMPLATE :
                    {
                        break;
                    }
                default :
                    {
                        internal_error("Invalid template parameter kind %d\n", result->parameters[i]->kind);
                    }
            }
        }
    }

    return result;
}

extern inline
template_parameter_list_t* compute_template_parameter_values_of_primary(template_parameter_list_t* template_parameter_list)
{
    int i;
    template_parameter_list_t* result = duplicate_template_argument_list(template_parameter_list);

    for (i = 0; i < template_parameter_list->num_parameters; i++)
    {
        template_parameter_t* param = result->parameters[i];
        template_parameter_value_t* new_value = NEW0(template_parameter_value_t);

        switch (param->kind)
        {
            case TPK_TYPE :
                {
                    new_value->kind = TPK_TYPE;
                    new_value->type = get_user_defined_type(param->entry);
                    break;
                }
            case TPK_TEMPLATE :
                {
                    new_value->kind = TPK_TEMPLATE;
                    new_value->type = get_user_defined_type(param->entry);
                    break;
                }
            case TPK_NONTYPE :
                {
                    nodecl_t n = nodecl_make_symbol(param->entry, param->entry->locus);
                    nodecl_expr_set_is_value_dependent(n, 1);
                    nodecl_set_type(n, param->entry->type_information);

                    new_value->kind = TPK_NONTYPE;
                    new_value->value = n;
                    new_value->type = param->entry->type_information;

                    break;
                }
            case TPK_TYPE_PACK :
                {
                    new_value->kind = TPK_TYPE;
                    new_value->type = get_pack_type(get_user_defined_type(param->entry));
                    break;
                }
            case TPK_TEMPLATE_PACK :
                {
                    new_value->kind = TPK_TEMPLATE;
                    new_value->type = get_pack_type(get_user_defined_type(param->entry));
                    break;
                }
            case TPK_NONTYPE_PACK :
                {
                    nodecl_t sym_ref =
                            nodecl_make_symbol(param->entry, param->entry->locus);
                    type_t* pack_type = get_pack_type(param->entry->type_information);
                    nodecl_set_type(sym_ref, pack_type);

                    nodecl_t n = nodecl_make_cxx_value_pack(
                            sym_ref,
                            param->entry->type_information,
                            param->entry->locus
                            );
                    nodecl_expr_set_is_type_dependent(n, is_dependent_type(pack_type));
                    nodecl_expr_set_is_value_dependent(n, 1);

                    // Do we have to handle the following case in any special way?
                    //
                    // template <typename ...T, T ...N>
                    new_value->kind = TPK_NONTYPE;
                    new_value->value = n;
                    new_value->type = param->entry->type_information;
                    break;
                }
            default :
                {
                    internal_error("Invalid template parameter kind %d\n", param->kind);
                }
        }

        result->arguments[i] = new_value;
    }

    return result;
}

extern inline type_t* get_new_template_alias_type(template_parameter_list_t* template_parameter_list, type_t* aliased_type,
        const char* template_name, const decl_context_t* decl_context, const locus_t* locus)
{
    type_t* type_info = get_simple_type();
    type_info->type->kind = STK_TEMPLATE_TYPE;
    type_info->template_parameters = template_parameter_list;

    scope_entry_t* primary_symbol = NULL;
    primary_symbol = NEW0(scope_entry_t);
    primary_symbol->symbol_name = template_name;
    primary_symbol->kind = SK_TEMPLATE_ALIAS;

    type_t* primary_type = new_empty_type_without_info();
    primary_symbol->type_information = primary_type;
    primary_symbol->decl_context = decl_context;

    primary_symbol->locus = locus;
    symbol_entity_specs_set_is_user_declared(primary_symbol, 1);
    symbol_entity_specs_set_is_instantiable(primary_symbol, 1);

    *primary_type = *aliased_type;
    primary_type->info = new_common_type_info();
    *primary_type->info = *aliased_type->info;

    primary_type->info->is_template_specialized_type = 1;
    primary_type->template_parameters = template_parameter_list;
    primary_type->template_arguments = compute_template_parameter_values_of_primary(template_parameter_list);
    primary_type->related_template_type = type_info;

    // Note that we do not set whether primary_type is dependent because we
    // will used the attribute from aliased_type. For nondependent aliased types
    // like
    //
    //    template <typename T>
    //    using Foo = int;
    //
    // this has the side effect that
    //
    // template <typename T>
    // void f()
    // {
    //   A<T> a;
    //   a = (int*)0; // will fail at "declaration of the template"-time
    //                // rather than instantiation. This should be OK
    // }

    type_info->type->primary_specialization = get_user_defined_type(primary_symbol);

    P_LIST_ADD(type_info->type->all_specialized_types,
            type_info->type->num_all_specialized_types,
            type_info->type->primary_specialization);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: %s: Creating primary specialization of template alias %p '%s'\n",
                locus_to_str(locus),
                type_info->type->primary_specialization,
                template_name);
    }

    return type_info;
}

static type_t* _get_duplicated_function_type(type_t* function_type);
static type_t* _get_duplicated_class_type(type_t* function_type);

extern inline type_t* get_new_template_type(template_parameter_list_t* template_parameter_list, type_t* primary_type,
        const char* template_name, const decl_context_t* decl_context, const locus_t* locus)
{
    // Simplify nontype template-arguments
    template_parameter_list = duplicate_template_argument_list(template_parameter_list);
    template_parameter_list = simplify_template_arguments(template_parameter_list);

    type_t* type_info = get_simple_type();
    type_info->type->kind = STK_TEMPLATE_TYPE;
    type_info->template_parameters = template_parameter_list;

    // Primary "specialization"
    scope_entry_t* primary_symbol = NULL;
    primary_symbol = NEW0(scope_entry_t);
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

    primary_symbol->locus = locus;
    symbol_entity_specs_set_is_user_declared(primary_symbol, 1);
    symbol_entity_specs_set_is_instantiable(primary_symbol, 1);

    primary_type->info->is_template_specialized_type = 1;
    primary_type->template_parameters = template_parameter_list;
    primary_type->template_arguments = compute_template_parameter_values_of_primary(template_parameter_list);
    primary_type->related_template_type = type_info;

    if (template_parameter_list->num_parameters != 0)
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

    P_LIST_ADD(type_info->type->all_specialized_types,
            type_info->type->num_all_specialized_types,
            type_info->type->primary_specialization);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: %s: Creating primary specialization of template type %p '%s'\n",
                locus_to_str(locus),
                type_info->type->primary_specialization,
                template_name);
    }

    return type_info;
}

extern inline void free_temporary_template_type(type_t* t)
{
    ERROR_CONDITION(t->kind != TK_DIRECT
            || t->type->kind != STK_TEMPLATE_TYPE, "Invalid type", 0);

    type_t* primary_specialization_type = t->type->primary_specialization;
    scope_entry_t* primary_specialization = named_type_get_symbol(primary_specialization_type);

    if (primary_specialization->type_information->kind == TK_FUNCTION)
    {
        DELETE(primary_specialization->type_information->function->parameter_list);
        DELETE(primary_specialization->type_information->function);
    }
    else if (primary_specialization->type_information->kind == TK_DIRECT
            && primary_specialization->type_information->type->kind == STK_CLASS)
    {
        internal_error("Not yet implemented", 0);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }


    free_template_parameter_list(primary_specialization->type_information->template_arguments);
    DELETE(primary_specialization->type_information->info);
    DELETE(primary_specialization->type_information);
    DELETE(primary_specialization);

    free_template_parameter_list(t->template_parameters);

    DELETE(primary_specialization_type->info);
    DELETE(primary_specialization_type->type);
    DELETE(primary_specialization_type);

    DELETE(t->info);
    DELETE(t->type);
    DELETE(t);
}

extern inline void set_as_template_specialized_type(type_t* type_to_specialize, 
        template_parameter_list_t * template_parameters, 
        type_t* template_type)
{
    ERROR_CONDITION(!is_function_type(type_to_specialize)
            && !is_unnamed_class_type(type_to_specialize), "This must be a class or function type", 0);

    if (template_type != NULL)
    {
        ERROR_CONDITION(!is_template_type(template_type), "This must be a template type", 0);
    }

    type_to_specialize->info->is_template_specialized_type = 1;
    type_to_specialize->template_parameters = template_parameters;
    type_to_specialize->template_arguments = compute_template_parameter_values_of_primary(template_parameters);
    type_to_specialize->related_template_type = template_type;
}

extern inline char is_template_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_TEMPLATE_TYPE);
}

extern inline void template_type_set_related_symbol(type_t* t, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_template_type(t), "This is not a template type", 0);

    ERROR_CONDITION(entry->kind != SK_TEMPLATE
            && entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER
            && entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER_PACK,
            "Invalid symbol for a template type", 0);

    t->type->related_template_symbol = entry;
}

extern inline scope_entry_t* template_type_get_related_symbol(type_t* t)
{
    ERROR_CONDITION(!is_template_type(t), "This is not a template type", 0);
    return t->type->related_template_symbol;
}

extern inline int template_type_get_nesting_level(type_t* t)
{
    ERROR_CONDITION(!is_template_type(t), "This is not a template type", 0);

    template_parameter_list_t *template_parameters = template_type_get_template_parameters(t);

    ERROR_CONDITION(template_parameters->num_parameters == 0,
            "Invalid template parameters", 0);

    // Use the first one since all template parameters will be in the same nesting 
    int nesting
        = symbol_entity_specs_get_template_parameter_nesting(template_parameters->parameters[0]->entry);

    // Sanity check
    int i;
    for (i = 1; i < template_parameters->num_parameters; i++)
    {
        // They must agree
        ERROR_CONDITION((symbol_entity_specs_get_template_parameter_nesting(template_parameters->parameters[i]->entry)
                    != nesting),
                "Invalid template parameters, their nesting is not the same", 0);
    }

    return nesting;
}

extern inline type_t* template_type_get_primary_type(type_t* t)
{
    ERROR_CONDITION(!is_template_type(t), "This is not a template type", 0);
    return t->type->primary_specialization;
}

static char same_template_argument_list(
        template_parameter_list_t* template_parameter_list_1,
        template_parameter_list_t* template_parameter_list_2)
{
    if (template_parameter_list_1->num_parameters !=
            template_parameter_list_2->num_parameters)
        return 0;

    int i;
    for (i = 0; i < template_parameter_list_1->num_parameters; i++)
    {
        template_parameter_value_t* targ_1 = template_parameter_list_1->arguments[i];
        template_parameter_value_t* targ_2 = template_parameter_list_2->arguments[i];

        ERROR_CONDITION(targ_1 == NULL || targ_2 == NULL, "Invalid parameter value", 0);

        if (targ_1->kind != targ_2->kind)
            return 0;

        switch (targ_1->kind)
        {
            case TPK_TYPE:
            case TPK_TEMPLATE:
                {
                    if (!equivalent_types(
                                targ_1->type,
                                targ_2->type))
                    {
                        return 0;
                    }
                    break;
                }
            case TPK_NONTYPE:
                {
                    if (!same_functional_expression(
                                targ_1->value,
                                targ_2->value))
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

extern inline char has_dependent_template_parameters(template_parameter_list_t* template_parameters)
{
    int i;
    for (i = 0; i < template_parameters->num_parameters; i++)
    {
        template_parameter_value_t* curr_argument = template_parameters->arguments[i];

        ERROR_CONDITION(curr_argument == NULL, "Invalid template parameter value", 0);

        switch (curr_argument->kind)
        {
            case TPK_TYPE:
                {
                    if (is_dependent_type(curr_argument->type))
                    {
                        return 1;
                    }
                    break;
                }
            case TPK_TEMPLATE:
                {
                    if (is_named_type(curr_argument->type)
                            && (named_type_get_symbol(curr_argument->type)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                                || named_type_get_symbol(curr_argument->type)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK))
                    {
                        return 1;
                    }
                    else if (is_pack_type(curr_argument->type)
                            && (named_type_get_symbol(
                                    pack_type_get_packed_type(curr_argument->type)
                                    )->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                                || named_type_get_symbol(
                                    pack_type_get_packed_type(curr_argument->type)
                                    )->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK))

                    {
                        return 1;
                    }
                    break;
                }
            case TPK_NONTYPE:
                {
                    if (!nodecl_is_list(curr_argument->value))
                    {
                        if (nodecl_expr_is_value_dependent(curr_argument->value))
                            return 1;
                    }
                    else
                    {
                        int n;
                        nodecl_t* list = nodecl_unpack_list(curr_argument->value, &n);
                        for (i = 0; i < n; i++)
                        {
                            if (nodecl_expr_is_value_dependent(list[i]))
                                return 1;
                        }
                        DELETE(list);
                    }
                    break;
                }
            default:
                {
                    internal_error("Invalid template parameter", 0);
                }
        }
    }
    return 0;
}

extern inline char is_template_explicit_specialization(template_parameter_list_t* template_parameters)
{
    char is_explicit_specialization = 0;
    template_parameter_list_t* tpl = template_parameters;
    while (tpl != NULL && !is_explicit_specialization)
    {
        is_explicit_specialization = tpl->is_explicit_specialization;
        tpl = tpl->enclosing;
    }
    return is_explicit_specialization;
}

static int template_arg_value_type_identical_compare(nodecl_t n1, nodecl_t n2)
{
    if (nodecl_is_null(n1) && nodecl_is_null(n2))
        return 0;

    if (nodecl_is_null(n1) != nodecl_is_null(n2))
        return nodecl_is_null(n1) ? -1 : 1;

    int k1 = nodecl_get_kind(n1);
    int k2 = nodecl_get_kind(n2);
    if (k1 < k2)
        return -1;
    else if (k1 > k2)
        return 1;

    int cmp;
    cmp = intptr_t_comp(nodecl_get_symbol(n1), nodecl_get_symbol(n2));
    if (cmp != 0)
        return cmp;

    const_value_t* cv1 = nodecl_get_constant(n1);
    const_value_t* cv2 = nodecl_get_constant(n2);

    // Ignore these as constants
    if (cv1 != NULL
            && (const_value_is_object(cv1)
                || const_value_is_address(cv1)))
        cv1 = NULL;
    if (cv2 != NULL
            && (const_value_is_object(cv2)
                || const_value_is_address(cv2)))
        cv2 = NULL;

    if (cv1 == NULL
            && cv2 != NULL)
        return -1;
    if (cv1 != NULL
            && cv2 == NULL)
        return 1;
    if (cv1 != NULL
            && cv2 != NULL)
    {
        if (const_value_is_nonzero(const_value_lt(cv1, cv2)))
            return -1;
        if (const_value_is_nonzero(const_value_gt(cv1, cv2)))
            return 1;
    }

    cmp = intptr_t_comp(nodecl_get_type(n1), nodecl_get_type(n2)); 
    if (cmp != 0)
        return cmp;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        cmp = template_arg_value_type_identical_compare(
                    nodecl_get_child(n1, i),
                    nodecl_get_child(n2, i));

        if (cmp != 0)
            return cmp;
    }

    return 0;
}

static int template_arg_identical_compare(template_parameter_value_t* targ_1, template_parameter_value_t* targ_2)
{
    ERROR_CONDITION(targ_1 == NULL || targ_2 == NULL, "Invalid parameter value", 0);

    if (targ_1->kind < targ_2->kind)
        return -1;
    else if (targ_1->kind > targ_2->kind)
        return 1;

    switch (targ_1->kind)
    {
        case TPK_TYPE:
        case TPK_TEMPLATE:
            {
                int cmp = intptr_t_comp(targ_1->type, targ_2->type);
                if (cmp != 0)
                    return cmp;
                break;
            }
        case TPK_NONTYPE:
            {
                int cmp = template_arg_value_type_identical_compare(targ_1->value, targ_2->value);
                if (cmp != 0)
                    return cmp;
                break;
            }
        default:
            {
                internal_error("Invalid template argument kind", 0);
            }
    }

    return 0;
}

static int compare_identical_template_argument_list(const void* v1, const void* v2)
{
    template_parameter_list_t* template_parameter_list_1 = (template_parameter_list_t*)v1;
    template_parameter_list_t* template_parameter_list_2 = (template_parameter_list_t*)v2;

    int m = template_parameter_list_1->num_parameters < template_parameter_list_2->num_parameters ?
        template_parameter_list_1->num_parameters : template_parameter_list_2->num_parameters;


    // Lexicographical order
    int i;
    for (i = 0; i < m; i++)
    {
        template_parameter_value_t* targ_1 = template_parameter_list_1->arguments[i];
        template_parameter_value_t* targ_2 = template_parameter_list_2->arguments[i];

        int cmp = template_arg_identical_compare(targ_1, targ_2);
        if (cmp != 0)
            return cmp;
    }

    // No one is different up to this point but maybe they share a prefix
    if (template_parameter_list_1->num_parameters < template_parameter_list_2->num_parameters)
        return -1;
    else if (template_parameter_list_1->num_parameters > template_parameter_list_2->num_parameters)
        return 1;

    return 0;
}

static int compare_identical_template_argument_list_of_named_types(
        const void *v1,
        const void *v2)
{
    type_t* t1 = *(type_t**)v1;
    type_t* t2 = *(type_t**)v2;

    t1 = named_type_get_symbol(t1)->type_information;
    t2 = named_type_get_symbol(t2)->type_information;

    return compare_identical_template_argument_list(
            template_specialized_type_get_template_arguments(t1),
            template_specialized_type_get_template_arguments(t2));
}

static type_t* template_type_get_identical_specialized_type(type_t* t,
        template_parameter_list_t* template_parameters,
        const decl_context_t* decl_context UNUSED_PARAMETER)
{
    ERROR_CONDITION(!is_template_type(t), "This is not a template type", 0);

    type_t* specialization = NULL;

    int lower = 0;
    int upper = t->type->num_all_specialized_types - 1;

    while (lower <= upper)
    {
        int middle = (lower + upper) / 2;

        type_t* current_specialization = t->type->all_specialized_types[middle];

        scope_entry_t* entry = named_type_get_symbol(current_specialization);
        template_parameter_list_t* specialization_template_parameters =
            template_specialized_type_get_template_arguments(entry->type_information);

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Checking with specialization %p: #%d in [%d<=%d, %d<=%d] '%s' (%p) at '%s'\n",
                    t->type,
                    middle,
                    0,
                    lower,
                    upper,
                    t->type->num_all_specialized_types - 1,
                    print_type_str(current_specialization, entry->decl_context),
                    entry->type_information,
                    locus_to_str(entry->locus));
        }

        int cmp = compare_identical_template_argument_list(
                template_parameters,
                specialization_template_parameters);

        if (cmp == 0)
        {
            specialization = current_specialization;
            break;
        }
        else if (cmp < 0)
        {
            upper = middle - 1;
        }
        else if (cmp > 0)
        {
            lower = middle + 1;
        }
    }

    return specialization;
}


#if 0
static int template_arg_value_expr_equivalent_compare_aux(nodecl_t n1, nodecl_t n2);
static int template_arg_value_expr_equivalent_compare(nodecl_t n1, nodecl_t n2)
{
    int cmp = template_arg_value_expr_equivalent_compare_aux(n1, n2);

    DEBUG_CODE()
    {
        if (cmp < 0)
        {
            fprintf(stderr, "EXPR |%s| < |%s|\n",
                    codegen_to_str(n1, CURRENT_COMPILED_FILE->global_decl_context),
                    codegen_to_str(n2, CURRENT_COMPILED_FILE->global_decl_context));
        }
        else if (cmp > 0)
        {
            fprintf(stderr, "EXPR |%s| > |%s|\n",
                    codegen_to_str(n1, CURRENT_COMPILED_FILE->global_decl_context),
                    codegen_to_str(n2, CURRENT_COMPILED_FILE->global_decl_context));
        }
        else 
        {
            fprintf(stderr, "EXPR |%s| == |%s|\n",
                    codegen_to_str(n1, CURRENT_COMPILED_FILE->global_decl_context),
                    codegen_to_str(n2, CURRENT_COMPILED_FILE->global_decl_context));
        }
    }

    return cmp;
}
#endif

static int template_arg_value_type_equivalent_compare(type_t* t1, type_t* t2);

#define RETURN_COMP(x, y) \
    if ((x) < (y)) return -1; \
    else if ((x) > (y)) return 1;

static int template_arg_value_expr_equivalent_compare(nodecl_t n1, nodecl_t n2)
{
    if (nodecl_is_null(n1)
            && nodecl_is_null(n2))
        return 0;
    else if (nodecl_is_null(n1)
            && !nodecl_is_null(n2))
        return -1;
    else if (!nodecl_is_null(n1)
            && nodecl_is_null(n2))
        return 1;
    else if (nodecl_get_constant(n1) == NULL
            && nodecl_get_constant(n2) != NULL)
    {
        return -1;
    }
    else if (nodecl_get_constant(n1) != NULL
            && nodecl_get_constant(n2) == NULL)
    {
        return 1;
    }
    else if (nodecl_get_constant(n1) != NULL
            && nodecl_get_constant(n2) != NULL)
    {
        if (const_value_is_nonzero(
                    const_value_lt(
                        nodecl_get_constant(n1),
                        nodecl_get_constant(n2))) )
        {
            return -1;
        }
        else if (const_value_is_nonzero(
                    const_value_gt(
                        nodecl_get_constant(n1),
                        nodecl_get_constant(n2))) )
        {
            return 1;
        }
    }
    else if (nodecl_get_symbol(n1) == NULL
            && nodecl_get_symbol(n2) != NULL)
        return -1;
    else if (nodecl_get_symbol(n1) != NULL
            && nodecl_get_symbol(n2) == NULL)
        return 1;
    else if (nodecl_get_symbol(n1) != NULL
            && nodecl_get_symbol(n2) != NULL)
    {
        scope_entry_t* s1 = nodecl_get_symbol(n1);
        scope_entry_t* s2 = nodecl_get_symbol(n2);

        RETURN_COMP(s1->kind, s2->kind);
        if (s1->kind == SK_VARIABLE)
        {
            RETURN_COMP(
                    symbol_is_parameter_of_function(s1, get_function_declaration_proxy()),
                    symbol_is_parameter_of_function(s2, get_function_declaration_proxy()));
            if (symbol_is_parameter_of_function(s1, get_function_declaration_proxy()))
            {
                RETURN_COMP(symbol_get_parameter_nesting_in_function(s1, get_function_declaration_proxy()),
                        symbol_get_parameter_nesting_in_function(s2, get_function_declaration_proxy()));
                RETURN_COMP(symbol_get_parameter_position_in_function(s1, get_function_declaration_proxy()),
                        symbol_get_parameter_position_in_function(s2, get_function_declaration_proxy()));
                int cmp = template_arg_value_type_equivalent_compare(s1->type_information, s2->type_information);
                if (cmp != 0)
                    return cmp;
            }
            else
            {
                RETURN_COMP(s1, s2);
            }
        }
        else if (s1->kind == SK_TEMPLATE_NONTYPE_PARAMETER)
        {
            RETURN_COMP(symbol_entity_specs_get_template_parameter_nesting(s1),
                    symbol_entity_specs_get_template_parameter_nesting(s2));
            RETURN_COMP(symbol_entity_specs_get_template_parameter_position(s1),
                    symbol_entity_specs_get_template_parameter_position(s2));
        }
        else if (s1->kind == SK_DEPENDENT_ENTITY)
        {
            int cmp = template_arg_value_type_equivalent_compare(s1->type_information, s2->type_information);
            if (cmp != 0)
                return cmp;
        }
        else
        {
            RETURN_COMP(s1, s2);
        }
    }
    else if (nodecl_get_kind(n1) < nodecl_get_kind(n2))
        return -1;
    else if (nodecl_get_kind(n1) > nodecl_get_kind(n2))
        return 1;
    else
    {
        int i;
        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            int cmp = template_arg_value_expr_equivalent_compare(
                    nodecl_get_child(n1, i),
                    nodecl_get_child(n2, i));

            if (cmp != 0)
                return cmp;
        }
    }

    return 0;
}

static int template_arg_value_type_equivalent_compare_cv_qualif_0(
        cv_qualifier_t cv_qualif1,
        cv_qualifier_t cv_qualif2)
{
    if (cv_qualif1 < cv_qualif2)
        return -1;
    else if (cv_qualif1 > cv_qualif2)
        return 1;
    else
        return 0;
}

#if 0
static int template_arg_value_type_equivalent_compare_cv_qualif(type_t* t1, type_t* t2)
{
    cv_qualifier_t cv_qualif1 = CV_NONE;
    cv_qualifier_t cv_qualif2 = CV_NONE;
    advance_over_typedefs_with_cv_qualif(t1, &cv_qualif1);
    advance_over_typedefs_with_cv_qualif(t2, &cv_qualif2);

    return template_arg_value_type_equivalent_compare_cv_qualif_0(cv_qualif1, cv_qualif2);
}
#endif

static int template_arg_value_type_equivalent_compare_aux(type_t* t1, type_t* t2);
static int template_arg_value_type_equivalent_compare(type_t* t1, type_t* t2)
{
    int cmp = template_arg_value_type_equivalent_compare_aux(t1, t2);

    DEBUG_CODE()
    {
        if (cmp < 0)
        {
            fprintf(stderr, "TYPE |%s| (%p) < |%s| (%p)\n",
                    print_type_str(t1, CURRENT_COMPILED_FILE->global_decl_context), t1,
                    print_type_str(t2, CURRENT_COMPILED_FILE->global_decl_context), t2);
        }
        else if (cmp > 0)
        {
            fprintf(stderr, "TYPE |%s| (%p) > |%s| (%p)\n",
                    print_type_str(t1, CURRENT_COMPILED_FILE->global_decl_context), t1,
                    print_type_str(t2, CURRENT_COMPILED_FILE->global_decl_context), t2);
        }
        else
        {
            fprintf(stderr, "TYPE |%s| (%p) == |%s| (%p)\n",
                    print_type_str(t1, CURRENT_COMPILED_FILE->global_decl_context), t1,
                    print_type_str(t2, CURRENT_COMPILED_FILE->global_decl_context), t2);
        }
    }

    return cmp;
}

static int compare_equivalent_template_argument_list(
        template_parameter_list_t* template_parameter_list_1,
        template_parameter_list_t* template_parameter_list_2);

// FIXME - Improve this function
static int template_arg_value_type_equivalent_compare_aux(type_t* t1, type_t* t2)
{
    if (t1 == t2) // fast-path
        return 0;

    if (t1 == NULL
            && t2 == NULL)
        return 0;
    else if (t1 == NULL
            && t2 != NULL)
        return -1;
    else if (t1 != NULL
            && t2 == NULL)
        return 1;

    cv_qualifier_t cv_qualifier_t1 = CV_NONE, cv_qualifier_t2 = CV_NONE;

    // Advance over typedefs
    t1 = advance_over_typedefs_with_cv_qualif(t1, &cv_qualifier_t1);
    t2 = advance_over_typedefs_with_cv_qualif(t2, &cv_qualifier_t2);

    if (t1 == t2) // fast-path
    {
        return template_arg_value_type_equivalent_compare_cv_qualif_0(cv_qualifier_t1, cv_qualifier_t2);
    }

    RETURN_COMP(t1->kind, t2->kind);
    switch (t1->kind)
    {
        case TK_DIRECT :
            {
                RETURN_COMP(t1->type->kind, t2->type->kind);

                switch (t1->type->kind)
                {
                    case STK_BUILTIN_TYPE :
                        {
                            RETURN_COMP(t1->type->builtin_type, t2->type->builtin_type);
                            RETURN_COMP(t1->type->is_signed, t2->type->is_signed);
                            RETURN_COMP(t1->type->is_unsigned, t2->type->is_unsigned);
                            RETURN_COMP(t1->type->is_long, t2->type->is_long);
                            RETURN_COMP(t1->type->is_short, t2->type->is_short);
                            break;
                        }
                    case STK_CLASS :
                        {
                            RETURN_COMP(t1->info->is_template_specialized_type,
                                    t2->info->is_template_specialized_type);

                            if (t1->info->is_template_specialized_type)
                            {
                                scope_entry_t* s1 = template_type_get_related_symbol(t1->related_template_type);
                                scope_entry_t* s2 = template_type_get_related_symbol(t2->related_template_type);

                                RETURN_COMP(s1->kind, s2->kind);
                                // specialization built using a template-template parameter (pack)
                                if (s1->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                                        || s1->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
                                {
                                    RETURN_COMP(symbol_entity_specs_get_template_parameter_nesting(s1),
                                            symbol_entity_specs_get_template_parameter_nesting(s2));
                                    RETURN_COMP(symbol_entity_specs_get_template_parameter_position(s1),
                                            symbol_entity_specs_get_template_parameter_position(s2));

                                    template_parameter_list_t* tpl1 = template_specialized_type_get_template_arguments(t1);
                                    template_parameter_list_t* tpl2 = template_specialized_type_get_template_arguments(t2);

                                    int cmp = compare_equivalent_template_argument_list(tpl1, tpl2);

                                    if (cmp != 0)
                                        return cmp;
                                }
                                else // specialized class using a class template
                                {
                                    RETURN_COMP(t1->type, t2->type);
                                }
                            }
                            else // non-specialization class
                            {
                                RETURN_COMP(t1->type, t2->type);
                            }
                            break;
                        }
                    case STK_ENUM :
                    case STK_TEMPLATE_TYPE :
                        {
                            RETURN_COMP(t1->type, t2->type);
                            break;
                        }
                    case STK_UNDERLYING:
                        {
                            int cmp = template_arg_value_type_equivalent_compare(
                                    t1->type->underlying_type,
                                    t2->type->underlying_type);
                            if (cmp != 0)
                                return cmp;
                            break;
                        }
                    case STK_VA_LIST :
                        {
                            break;
                        }
                    case STK_TYPE_DEP_EXPR:
                        {
                            break;
                        }
                    case STK_COMPLEX:
                        {
                            int cmp = template_arg_value_type_equivalent_compare(t1->type->complex_element, t2->type->complex_element);
                            if (cmp != 0)
                                return cmp;
                            break;
                        }
                    case STK_VECTOR:
                        {
                            int cmp = template_arg_value_type_equivalent_compare(t1->type->vector_element, t2->type->vector_element);
                            if (cmp != 0)
                                return cmp;

                            RETURN_COMP(t1->type->vector_size, t2->type->vector_size);
                            break;
                        }
                    case STK_MASK:
                        {
                            RETURN_COMP(t1->type->vector_size, t2->type->vector_size);
                            break;
                        }
                    case STK_INDIRECT:
                        {
                            scope_entry_t* s1 = t1->type->user_defined_type;
                            scope_entry_t* s2 = t2->type->user_defined_type;

                            RETURN_COMP(s1->kind, s2->kind);
                            if (symbol_entity_specs_get_is_template_parameter(s1)
                                    && symbol_entity_specs_get_is_template_parameter(s2))
                            {
                                RETURN_COMP(symbol_entity_specs_get_template_parameter_nesting(s1),
                                        symbol_entity_specs_get_template_parameter_nesting(s2));
                                RETURN_COMP(symbol_entity_specs_get_template_parameter_position(s1),
                                        symbol_entity_specs_get_template_parameter_position(s2));
                            }
                            else
                            {
                                int cmp = template_arg_value_type_equivalent_compare(s1->type_information, s2->type_information);
                                if (cmp != 0)
                                    return cmp;
                            }
                            break;
                        }
                    case STK_TEMPLATE_DEPENDENT_TYPE :
                        {
                            scope_entry_t* s1 = NULL;
                            nodecl_t dependent_parts_1 = nodecl_null();

                            dependent_typename_get_components(t1, 
                                    &s1,
                                    &dependent_parts_1);

                            scope_entry_t* s2 = NULL;
                            nodecl_t dependent_parts_2 = nodecl_null();

                            dependent_typename_get_components(t2, 
                                    &s2,
                                    &dependent_parts_2);

                            RETURN_COMP(s1->kind, s2->kind);
                            if (symbol_entity_specs_get_is_template_parameter(s1)
                                    && symbol_entity_specs_get_is_template_parameter(s2))
                            {
                                RETURN_COMP(symbol_entity_specs_get_template_parameter_nesting(s1),
                                        symbol_entity_specs_get_template_parameter_nesting(s2));
                                RETURN_COMP(symbol_entity_specs_get_template_parameter_position(s1),
                                        symbol_entity_specs_get_template_parameter_position(s2));
                            }
                            else
                            {
                                int cmp = template_arg_value_type_equivalent_compare(s1->type_information, s2->type_information);
                                if (cmp != 0)
                                    return cmp;
                            }

                            int num_items1 = 0;
                            nodecl_t* list1 = nodecl_unpack_list(nodecl_get_child(dependent_parts_1, 0), &num_items1);

                            int num_items2 = 0;
                            nodecl_t* list2 = nodecl_unpack_list(nodecl_get_child(dependent_parts_2, 0), &num_items2);

                            if (num_items1 < num_items2)
                            {
                                DELETE(list1);
                                DELETE(list2);
                                return -1;
                            }
                            else if (num_items1 > num_items2)
                            {
                                DELETE(list1);
                                DELETE(list2);
                                return 1;
                            }

                            int i;
                            for (i = 0; i < num_items1; i++)
                            {
                                nodecl_t item1 = list1[i];
                                nodecl_t item2 = list2[i];

                                if (nodecl_get_kind(item1) < nodecl_get_kind(item2))
                                {
                                    DELETE(list1);
                                    DELETE(list2);
                                    return -1;
                                }
                                else if (nodecl_get_kind(item1) > nodecl_get_kind(item2))
                                {
                                    DELETE(list1);
                                    DELETE(list2);
                                    return 1;
                                }

                                nodecl_t nodecl_simple_name_1 = item1;
                                nodecl_t nodecl_simple_name_2 = item2;

                                template_parameter_list_t* template_parameter_list_1 = NULL;
                                template_parameter_list_t* template_parameter_list_2 = NULL;

                                if (nodecl_get_kind(item1) == NODECL_CXX_DEP_TEMPLATE_ID)
                                {
                                    nodecl_simple_name_1 = nodecl_get_child(item1, 0);
                                    nodecl_simple_name_2 = nodecl_get_child(item2, 0);

                                    template_parameter_list_1 = nodecl_get_template_parameters(item1);
                                    template_parameter_list_2 = nodecl_get_template_parameters(item2);

                                    ERROR_CONDITION(template_parameter_list_1 == NULL
                                            || template_parameter_list_2 == NULL,
                                            "This cannot happen", 0);
                                }

                                const char* name_1 = nodecl_get_text(nodecl_simple_name_1);
                                const char* name_2 = nodecl_get_text(nodecl_simple_name_2);

                                int cmp = strcmp(name_1, name_2);
                                if (cmp != 0)
                                {
                                    DELETE(list1);
                                    DELETE(list2);
                                    return cmp < 0 ? -1 : 1;
                                }

                                if (template_parameter_list_1 != NULL)
                                {
                                    cmp = compare_equivalent_template_argument_list(
                                            template_parameter_list_1,
                                            template_parameter_list_2);
                                    if (cmp != 0)
                                    {
                                        DELETE(list1);
                                        DELETE(list2);
                                        return cmp;
                                    }
                                }
                            }

                            DELETE(list1);
                            DELETE(list2);
                            break;
                        }
                    case STK_TYPEOF:
                        {
                            int cmp = template_arg_value_expr_equivalent_compare(
                                    t1->type->typeof_expr,
                                    t2->type->typeof_expr);
                            if (cmp != 0)
                                return cmp;
                            break;
                        }
                    default :
                        {
                            internal_error("Unknown simple type kind (%d)", t1->type->kind);
                        }
                }
                break;
            }
        case TK_POINTER :
        case TK_LVALUE_REFERENCE :
        case TK_RVALUE_REFERENCE :
        case TK_REBINDABLE_REFERENCE :
            {
                int cmp = template_arg_value_type_equivalent_compare(t1->pointer->pointee, t2->pointer->pointee);
                if (cmp != 0)
                    return cmp;
                break;
            }
        case TK_POINTER_TO_MEMBER :
            {
                int cmp = template_arg_value_type_equivalent_compare(
                        t1->pointer->pointee_class_type,
                        t2->pointer->pointee_class_type);

                if (cmp != 0)
                    return cmp;

                cmp = template_arg_value_type_equivalent_compare(t1->pointer->pointee, t2->pointer->pointee);
                if (cmp != 0)
                    return cmp;
                break;
            }
        case TK_ARRAY :
            {
                int cmp = template_arg_value_type_equivalent_compare(
                        t1->array->element_type,
                        t2->array->element_type);

                if (cmp != 0)
                    return cmp;

                cmp = template_arg_value_expr_equivalent_compare(
                        t1->array->whole_size,
                        t2->array->whole_size);

                if (cmp != 0)
                    return cmp;
                break;
            }
        case TK_FUNCTION :
            {
                int cmp = template_arg_value_type_equivalent_compare(
                        t1->function->return_type,
                        t2->function->return_type);

                if (cmp != 0)
                    return cmp;

                RETURN_COMP (t1->function->num_parameters, t2->function->num_parameters);

                int i;
                for (i = 0; i < t1->function->num_parameters; i++)
                {
                    RETURN_COMP(t1->function->parameter_list[i]->is_ellipsis,
                             t2->function->parameter_list[i]->is_ellipsis);

                    cmp = template_arg_value_type_equivalent_compare(
                            t1->function->parameter_list[i]->type_info,
                            t2->function->parameter_list[i]->type_info);

                    if (cmp != 0)
                        return cmp;
                }

                RETURN_COMP(t1->function->ref_qualifier, t2->function->ref_qualifier);
                break;
            }
        case TK_PACK:
            {
                int cmp = template_arg_value_type_equivalent_compare(t1->pack_type->packed, t2->pack_type->packed);
                if (cmp != 0)
                    return cmp;
                break;
            }
        case TK_SEQUENCE:
            {
                RETURN_COMP(t1->sequence_type->num_types, t2->sequence_type->num_types);

                int i;
                for (i = 0; i < t1->sequence_type->num_types; i++)
                {
                    int cmp = template_arg_value_type_equivalent_compare(
                            t1->sequence_type->types[i],
                            t2->sequence_type->types[i]);

                    if (cmp != 0)
                        return cmp;
                }
                break;
            }
        default :
            internal_error("Unexpected type kind (%d)\n", t1->kind);
    }

    return template_arg_value_type_equivalent_compare_cv_qualif_0(cv_qualifier_t1, cv_qualifier_t2);
}
#undef RETURN_COMP

static int template_arg_value_compare(
        template_parameter_value_t* targ_1,
        template_parameter_value_t* targ_2)
{
    ERROR_CONDITION(targ_1->kind != targ_2->kind, "Invalid template arguments", 0);

    switch (targ_1->kind)
    {
        case TPK_TYPE:
        case TPK_TEMPLATE:
            {
                return template_arg_value_type_equivalent_compare(
                        targ_1->type,
                        targ_2->type);
                break;
            }
        case TPK_NONTYPE:
            {
                int cmp = template_arg_value_type_equivalent_compare(
                        targ_1->type,
                        targ_2->type);

                if (cmp != 0)
                    return cmp;

                return template_arg_value_expr_equivalent_compare(
                        targ_1->value,
                        targ_2->value);
                break;
            }
        default:
            {
                internal_error("Invalid template argument kind", 0);
            }
    }

    return 0;
}

static int template_arg_equivalent_compare(
        template_parameter_value_t* targ_1,
        template_parameter_value_t* targ_2)
{
    if (targ_1->kind < targ_2->kind)
        return -1;
    else if (targ_1->kind < targ_2->kind)
        return 1;
    else
        return template_arg_value_compare(targ_1, targ_2);
}

static int compare_equivalent_template_argument_list(
        template_parameter_list_t* template_parameter_list_1,
        template_parameter_list_t* template_parameter_list_2)
{
    if (template_parameter_list_1->num_parameters <
            template_parameter_list_2->num_parameters)
        return -1;
    else if (template_parameter_list_1->num_parameters >
            template_parameter_list_2->num_parameters)
        return 1;

    int i;
    for (i = 0; i < template_parameter_list_1->num_parameters; i++)
    {
        template_parameter_value_t* targ_1 = template_parameter_list_1->arguments[i];
        template_parameter_value_t* targ_2 = template_parameter_list_2->arguments[i];

        int cmp = template_arg_equivalent_compare(targ_1, targ_2);
        if (cmp != 0)
            return cmp;
    }

    return 0;
}

static int compare_equivalent_template_argument_list_of_named_template_specialized_types(
        const void* v1,
        const void* v2)
{
    type_t* t1 = *(type_t**)v1;
    type_t* t2 = *(type_t**)v2;

    t1 = named_type_get_symbol(t1)->type_information;
    t2 = named_type_get_symbol(t2)->type_information;

    return compare_equivalent_template_argument_list(
            template_specialized_type_get_template_arguments(t1),
            template_specialized_type_get_template_arguments(t2));
}

static type_t* template_type_get_equivalent_specialized_type(type_t* t,
        template_parameter_list_t* template_parameters,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus UNUSED_PARAMETER)
{
    ERROR_CONDITION(!is_template_type(t), "This is not a template type", 0);

    // Search an existing specialization
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: Searching an existing specialization that matches %s%s\n",
                template_type_get_related_symbol(t) != NULL ? 
                    template_type_get_related_symbol(t)->symbol_name
                    : "<<unknown-template-symbol>>",
                template_arguments_to_str(template_parameters,
                    /* first_argument_to_be_printed */ 0,
                    /* print_first_level_bracket */ 1,
                    decl_context));
        fprintf(stderr, "TYPEUTILS: There are '%d' specializations of this template type\n", 
                template_type_get_num_specializations(t));
    }

    type_t* specialization = NULL;

    int lower = 0;
    int upper = t->type->num_unique_specialized_types - 1;

    while (lower <= upper)
    {
        int middle = (lower + upper) / 2;

        type_t* current_specialization = t->type->unique_specialized_types[middle];

        scope_entry_t* entry = named_type_get_symbol(current_specialization);
        template_parameter_list_t* specialization_template_parameters =
            template_specialized_type_get_template_arguments(entry->type_information);

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Checking with specialization %p: #%d in [%d<=%d, %d<=%d] '%s' (%p) at '%s'\n",
                    t->type,
                    middle,
                    0,
                    lower,
                    upper,
                    t->type->num_unique_specialized_types - 1,
                    print_type_str(current_specialization, entry->decl_context),
                    entry->type_information,
                    locus_to_str(entry->locus));
        }

        int cmp = compare_equivalent_template_argument_list(
                template_parameters,
                specialization_template_parameters);

        if (cmp == 0)
        {
            specialization = current_specialization;
            break;
        }
        else if (cmp < 0)
        {
            upper = middle - 1;
        }
        else if (cmp > 0)
        {
            lower = middle + 1;
        }
    }

    if (specialization == NULL)
    {
        // Try with the primary which is always unsorted respect the other specializations
        type_t* current_specialization = template_type_get_primary_type(t);
        scope_entry_t* entry = named_type_get_symbol(current_specialization);

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Checking with primary specialization '%s' (%p) at '%s'\n",
                    print_type_str(current_specialization, entry->decl_context),
                    entry->type_information,
                    locus_to_str(entry->locus));
        }

        template_parameter_list_t* specialization_template_parameters =
            template_specialized_type_get_template_arguments(entry->type_information);

        if (compare_equivalent_template_argument_list(template_parameters, specialization_template_parameters) == 0)
            specialization = current_specialization;
    }

    if (specialization != NULL)
    {
        DEBUG_CODE()
        {
            scope_entry_t* entry = named_type_get_symbol(specialization);
            fprintf(stderr, "TYPEUTILS: An existing specialization matches '%s'\n", print_declarator(entry->type_information));
            fprintf(stderr, "TYPEUTILS: Returning template %s %p\n", 
                    print_type_str(specialization, entry->decl_context),
                    entry->type_information);
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: No existing specialization matches\n");
        }
    }

    return specialization;
}

static type_t* template_type_get_specialized_type_(
        type_t* template_type,
        template_parameter_list_t *template_arguments,
        type_t* type_used_as_template,
        const decl_context_t* decl_context, 
        const locus_t* locus)
{
    template_arguments = duplicate_template_argument_list(template_arguments);
    template_arguments = simplify_template_arguments(template_arguments);

    type_t* exact_match = template_type_get_identical_specialized_type(template_type,
            template_arguments,
            decl_context);

    if (exact_match != NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: %s: Found an exact match for specialization: %p '%s'\n",
                    locus_to_str(locus),
                    exact_match,
                    print_type_str(exact_match,
                        named_type_get_symbol(exact_match)->decl_context));
        }
        free_template_parameter_list(template_arguments);
        return exact_match;
    }

    type_t* equivalent_match = template_type_get_equivalent_specialized_type(template_type,
            template_arguments,
            decl_context,
            locus);

    scope_entry_t* primary_symbol = named_type_get_symbol(template_type->type->primary_specialization);

    char has_dependent_temp_args = has_dependent_template_parameters(template_arguments);

    type_t* specialized_type = NULL;
    if (primary_symbol->kind == SK_CLASS)
    {
        if (equivalent_match == NULL)
        {
            if (type_used_as_template == NULL)
            {
                specialized_type = get_new_class_type(primary_symbol->decl_context,
                        class_type_get_class_kind(
                            get_actual_class_type(primary_symbol->type_information)));

                class_type_set_enclosing_class_type(specialized_type,
                        class_type_get_enclosing_class_type(primary_symbol->type_information));
            }
            else
            {
                specialized_type = _get_duplicated_class_type(type_used_as_template);
            }
        }
        else
        {
            type_t* actual_class_type = get_actual_class_type(equivalent_match);

            // We share the same type but the template arguments
            specialized_type = new_empty_type_without_info();
            specialized_type->unqualified_type = specialized_type;
            specialized_type->kind = TK_DIRECT;
            specialized_type->type = actual_class_type->type;
            specialized_type->info = actual_class_type->info;
        }
    }
    else if (primary_symbol->kind == SK_TEMPLATE_ALIAS)
    {
        if (equivalent_match == NULL)
        {
            decl_context_t* updated_context = decl_context_clone(primary_symbol->decl_context);
            updated_context->template_parameters = template_arguments;

            specialized_type = update_type(primary_symbol->type_information,
                    updated_context,
                    locus);

            // If we cannot update the type, give up, something is probably wrong
            if (specialized_type == NULL)
            {
                free_template_parameter_list(template_arguments);
                return NULL;
            }
        }
        else
        {
            // Use the type of the equivalent_match
            specialized_type = named_type_get_symbol(equivalent_match)->type_information;
        }

        // Now duplicate the type node as there are some bits that cannot be
        // shared with the obtained specialized type. This may happen when updating
        specialized_type = copy_type_for_class_alias(specialized_type);
    }
    else if (primary_symbol->kind == SK_FUNCTION)
    {
        // For template functions, nonidentical matches are OK
        if (equivalent_match != NULL)
        {
            free_template_parameter_list(template_arguments);
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Using equivalent specialization: %s\n",
                        print_type_str(equivalent_match, named_type_get_symbol(equivalent_match)->decl_context));
            }
            return equivalent_match;
        }

        decl_context_t* updated_context = decl_context_clone(primary_symbol->decl_context);
        updated_context->template_parameters = template_arguments;

        diagnostic_context_push_buffered();
        type_t* updated_function_type = update_type(primary_symbol->type_information, updated_context,
                locus);

        // If we cannot update the type, give up, as probably this is SFINAE
        if (updated_function_type == NULL)
        {
            diagnostic_context_pop_and_discard();
            free_template_parameter_list(template_arguments);
            return NULL;
        }
        diagnostic_context_pop_and_commit();

        // This will give us a new function type
        specialized_type = _get_duplicated_function_type(updated_function_type);
    }
    else
    {
        internal_error("Invalid templated type", 0);
    }

    // State that this is a template specialized type
    specialized_type->template_parameters = template_arguments;
    specialized_type->template_arguments = template_arguments;
    specialized_type->related_template_type = template_type;

    // If there was not an existing specialization set up
    // bits for this new specialization
    if (equivalent_match == NULL)
    {
        specialized_type->info->is_template_specialized_type = 1;

        // State the class type nature
        if (primary_symbol->kind == SK_CLASS)
        {
            type_t* enclosing_class_type = class_type_get_enclosing_class_type(specialized_type);
            if (has_dependent_temp_args
                    || (template_type->type->related_template_symbol != NULL
                        && (template_type->type->related_template_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                            || template_type->type->related_template_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK))
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
        else if (primary_symbol->kind == SK_TEMPLATE_ALIAS)
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
        else if (primary_symbol->kind == SK_FUNCTION)
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
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    // Create a fake symbol with the just created specialized type
    scope_entry_t* specialized_symbol = NEW0(scope_entry_t);

    specialized_symbol->symbol_name = primary_symbol->symbol_name;
    specialized_symbol->kind = primary_symbol->kind;
    specialized_symbol->type_information = specialized_type;

    // Fix the template arguments
    decl_context_t* updated_decl_context = decl_context_clone(primary_symbol->decl_context);
    updated_decl_context->template_parameters = template_arguments;
    specialized_symbol->decl_context = updated_decl_context;

    specialized_symbol->locus = locus;

    // Keep information of the entity except for some attributes that
    // must be cleared
    symbol_entity_specs_copy_from(specialized_symbol, primary_symbol);
    symbol_entity_specs_set_is_user_declared(specialized_symbol, 0);
    symbol_entity_specs_set_is_instantiable(specialized_symbol, 0);

    // Let this be filled later
    symbol_entity_specs_free_related_symbols(specialized_symbol);

    if (equivalent_match != NULL)
    {
        symbol_entity_specs_set_alias_to(specialized_symbol, named_type_get_symbol(equivalent_match));
    }

    // Copy function extra info
    if (specialized_symbol->kind == SK_FUNCTION)
    {
        // Empty default argument info for the specialization
        symbol_entity_specs_reserve_default_argument_info(
                specialized_symbol,
                function_type_get_num_parameters(
                    specialized_symbol->type_information));

        // Do not reuse the exceptions of the primary symbol (they may need to be updated)
        symbol_entity_specs_free_exceptions(specialized_symbol);

        // Update exception specifications
        decl_context_t* updated_context = decl_context_clone(primary_symbol->decl_context);
        updated_context->template_parameters = template_arguments;

        int i, num_exceptions = symbol_entity_specs_get_num_exceptions(primary_symbol);
        for (i = 0; i < num_exceptions; i++)
        {
            type_t* exception_type = symbol_entity_specs_get_exceptions_num(primary_symbol, i);
            type_t* updated_exception_type = update_type(exception_type, updated_context,
                    locus);

            symbol_entity_specs_add_exceptions(specialized_symbol,
                    updated_exception_type);
        }

        // FIXME - noexcept?
        // Do not copy the function code because it must be first instantiated
        symbol_entity_specs_set_function_code(specialized_symbol, nodecl_null());
    }

    type_t* result = get_user_defined_type(specialized_symbol);

    if (equivalent_match == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: %s: Creating new specialization: %p '%s'\n",
                    locus_to_str(locus),
                    result,
                    print_type_str(result, decl_context));
        }

#if 0
        // Integrity verification
        {
            int i;
            for (i = 0; i < template_type->type->num_unique_specialized_types; i++)
            {
                int j;
                for (j = i + 1; j < template_type->type->num_unique_specialized_types; j++)
                {
                    int cmp1;
                    cmp1 = compare_template_argument_list_of_named_template_specialized_types(
                            &template_type->type->specialized_types[i],
                            &template_type->type->specialized_types[j]);

                    int cmp2;
                    cmp2 = compare_template_argument_list_of_named_template_specialized_types(
                            &template_type->type->specialized_types[j],
                            &template_type->type->specialized_types[i]);

                    if (cmp1 != -1 || cmp2 != 1)
                    {
                        extern void _enable_debug();
                        extern void _disable_debug();
                        _enable_debug();
                        fprintf(stderr, "i < j?\n");
                        cmp1 = compare_template_argument_list_of_named_template_specialized_types(
                                &template_type->type->specialized_types[i],
                                &template_type->type->specialized_types[j]);
                        fprintf(stderr, "i > j?\n");
                        cmp2 = compare_template_argument_list_of_named_template_specialized_types(
                                &template_type->type->specialized_types[j],
                                &template_type->type->specialized_types[i]);
                        fprintf(stderr, "====\n");

                        template_type_get_equivalent_specialized_type(template_type,
                                template_arguments,
                                decl_context,
                                locus);
                        fprintf(stderr, "*****\n");
                        _disable_debug();
                    }

                    ERROR_CONDITION(cmp1 != -1,
                            "%p: Invalid ordering cmp == %d (should be -1)\n#%d: %s\n#%d: %s\n",
                            template_type->type,
                            cmp1,
                            i, print_type_str(template_type->type->specialized_types[i], decl_context),
                            j, print_type_str(template_type->type->specialized_types[j], decl_context));

                    ERROR_CONDITION(cmp2 != 1,
                            "%p: Invalid ordering cmp == %d (should be 1)\n#%d: %s\n#%d: %s\n",
                            template_type->type,
                            cmp2,
                            i, print_type_str(template_type->type->specialized_types[i], decl_context),
                            j, print_type_str(template_type->type->specialized_types[j], decl_context));
                }
            }

            int last_failed = -1;

            for (i = 0; i < template_type->type->num_unique_specialized_types ; i++)
            {
                int cmp = compare_template_argument_list_of_named_template_specialized_types(
                        &result,
                        &template_type->type->specialized_types[i]);
                if (cmp == 0)
                    last_failed = i;
            }

            if (last_failed >= 0)
            {
                extern void _enable_debug();
                extern void _disable_debug();
                _enable_debug();
                compare_template_argument_list_of_named_template_specialized_types(
                        &result,
                        &template_type->type->specialized_types[last_failed]);
                fprintf(stderr, "====\n");
                _disable_debug();
            }

            ERROR_CONDITION(last_failed >= 0, "%p: This cannot happen #%d\n%s\n%s",
                    template_type->type,
                    last_failed,
                    print_declarator(template_type->type->specialized_types[last_failed]),
                    print_declarator(result));
        }
#endif

        // Register this new specialization in the unique specialization list
        // This is to make room
        P_LIST_ADD(template_type->type->unique_specialized_types,
                template_type->type->num_unique_specialized_types,
                result);
        // And now we make an "binary insertion"
        {
            int lower = 0;
            int upper = template_type->type->num_unique_specialized_types - 2;

            while (lower <= upper)
            {
                int middle = (lower + upper) / 2;

                int cmp = compare_equivalent_template_argument_list_of_named_template_specialized_types(
                        &result,
                        &template_type->type->unique_specialized_types[middle]);

                if (cmp < 0)
                {
                    upper = middle - 1;
                }
                else if (cmp > 0)
                {
                    lower = middle + 1;
                }
                else
                {
                    internal_error("This cannot happen %p\nnew %s\n%03d: %s",
                            template_type->type,
                            print_declarator(result),
                            middle, print_declarator(template_type->type->unique_specialized_types[middle]));
                }
            }
            // lower tells us where the new element goes

            int j;
            // Shift all items right
            for (j = template_type->type->num_unique_specialized_types - 1; j >= lower + 1; j--)
            {
                template_type->type->unique_specialized_types[j] =
                    template_type->type->unique_specialized_types[j-1];
            }
            template_type->type->unique_specialized_types[lower] = result;
#if 0
            int i;
            for (i = 0; i < template_type->type->num_unique_specialized_types - 1; i++)
            {
                int cmp = compare_template_argument_list_of_named_template_specialized_types(
                        &result,
                        &template_type->type->specialized_types[i]);

                ERROR_CONDITION(cmp == 0, "This cannot happen %p\nnew %s\n%03d: %s",
                        template_type->type,
                        print_declarator(result),
                        i, print_declarator(template_type->type->specialized_types[i]));
                if (cmp > 0)
                    continue;
                else if (cmp < 0)
                {
                    int j;
#if 0
                    // Integrity verification
                    for (j = i + 1; j < template_type->type->num_unique_specialized_types - 1; j++)
                    {
                        cmp = compare_template_argument_list_of_named_template_specialized_types(
                                &result,
                                &template_type->type->specialized_types[j]);

                        if (cmp != -1)
                        {
                            extern void _enable_debug();
                            extern void _disable_debug();
                            _enable_debug();
                            fprintf(stderr, "result < j?\n");
                            compare_template_argument_list_of_named_template_specialized_types(
                                    &result,
                                    &template_type->type->specialized_types[j]);
                            fprintf(stderr, "result > j?\n");
                            compare_template_argument_list_of_named_template_specialized_types(
                                    &template_type->type->specialized_types[j],
                                    &result);
                            fprintf(stderr, "====\n");
                            _disable_debug();
                        }

                        ERROR_CONDITION(cmp != -1, "Wrong ordering %p\nnew %s\n%03d: %s",
                                template_type->type,
                                print_declarator(result),
                                j, print_declarator(template_type->type->specialized_types[j]));
                    }
#endif

                    // Shift all items right
                    for (j = template_type->type->num_unique_specialized_types - 1; j >= i + 1; j--)
                    {
                        template_type->type->specialized_types[j] =
                            template_type->type->specialized_types[j-1];
                    }
                    template_type->type->specialized_types[i] = result;
                    break;
                }
            }
#endif
        }

#if 0
        // Integrity verification
        {
            int i;
            for (i = 0; i < template_type->type->num_unique_specialized_types; i++)
            {
                int j;
                for (j = i + 1; j < template_type->type->num_unique_specialized_types; j++)
                {
                    int cmp1;
                    cmp1 = compare_template_argument_list_of_named_template_specialized_types(
                            &template_type->type->specialized_types[i],
                            &template_type->type->specialized_types[j]);

                    int cmp2;
                    cmp2 = compare_template_argument_list_of_named_template_specialized_types(
                            &template_type->type->specialized_types[j],
                            &template_type->type->specialized_types[i]);

                    if (cmp1 != -1 || cmp2 != 1)
                    {
                        extern void _enable_debug();
                        extern void _disable_debug();
                        _enable_debug();
                        fprintf(stderr, "i < j?\n");
                        cmp1 = compare_template_argument_list_of_named_template_specialized_types(
                                &template_type->type->specialized_types[i],
                                &template_type->type->specialized_types[j]);
                        fprintf(stderr, "i > j?\n");
                        cmp2 = compare_template_argument_list_of_named_template_specialized_types(
                                &template_type->type->specialized_types[j],
                                &template_type->type->specialized_types[i]);
                        fprintf(stderr, "====\n");
                        _disable_debug();
                    }

                    ERROR_CONDITION(cmp1 != -1,
                            "%p: Invalid ordering cmp == %d (should be -1)\n#%d: %s\n#%d: %s\n",
                            template_type->type,
                            cmp1,
                            i, print_type_str(template_type->type->specialized_types[i], decl_context),
                            j, print_type_str(template_type->type->specialized_types[j], decl_context));

                    ERROR_CONDITION(cmp2 != 1,
                            "%p: Invalid ordering cmp == %d (should be 1)\n#%d: %s\n#%d: %s\n",
                            template_type->type,
                            cmp2,
                            i, print_type_str(template_type->type->specialized_types[i], decl_context),
                            j, print_type_str(template_type->type->specialized_types[j], decl_context));
                }
            }

            int last_failed = -1;

            for (i = 0; i < template_type->type->num_unique_specialized_types ; i++)
            {
                int cmp = compare_template_argument_list_of_named_template_specialized_types(
                        &result,
                        &template_type->type->specialized_types[i]);
                if (cmp == 0)
                    last_failed = i;
            }

            ERROR_CONDITION(last_failed < 0, "%p: This cannot happen\n%s",
                    template_type->type,
                    print_type_str(result, decl_context));
        }
#endif
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: %s: Creating aliased specialization: %p '%s'. Alias to: %p '%s'\n",
                    locus_to_str(locus),
                    result,
                    print_type_str(result, decl_context),
                    equivalent_match,
                    print_type_str(equivalent_match,
                        named_type_get_symbol(equivalent_match)->decl_context));
        }
    }

    // Register this specialization in the all specializations set
    {
        P_LIST_ADD(template_type->type->all_specialized_types,
                template_type->type->num_all_specialized_types,
                result);
        int lower = 0;
        int upper = template_type->type->num_all_specialized_types - 2;

        // Binary insertion
        while (lower <= upper)
        {
            int middle = (lower + upper) / 2;

            int cmp = compare_identical_template_argument_list_of_named_types(
                    &result,
                    &template_type->type->all_specialized_types[middle]);

            if (cmp < 0)
            {
                upper = middle - 1;
            }
            else if (cmp > 0)
            {
                lower = middle + 1;
            }
            else
            {
                internal_error("This cannot happen %p\nnew %s\n%03d: %s",
                        template_type->type,
                        print_declarator(result),
                        middle, print_declarator(template_type->type->all_specialized_types[middle]));
            }
        }

        int j;
        // Shift all items right
        for (j = template_type->type->num_all_specialized_types - 1; j >= lower + 1; j--)
        {
            template_type->type->all_specialized_types[j] =
                template_type->type->all_specialized_types[j-1];
        }
        template_type->type->all_specialized_types[lower] = result;
    }

    return result;
}


extern inline type_t* template_type_get_specialized_type(type_t* t, 
        template_parameter_list_t* template_parameters,
        const decl_context_t* decl_context, 
        const locus_t* locus)
{
    return template_type_get_specialized_type_(t,
            template_parameters,
            /* type_used_as_template */ NULL,
            decl_context,
            locus);
}

extern inline type_t* template_type_get_specialized_type_for_instantiation(type_t* t,
        template_parameter_list_t* template_parameters,
        type_t* type_used_as_template,
        const decl_context_t* decl_context, 
        const locus_t* locus)
{
    return template_type_get_specialized_type_(t,
            template_parameters,
            type_used_as_template,
            decl_context,
            locus);
}

extern inline template_parameter_list_t* template_type_get_template_parameters(type_t* t)
{
    ERROR_CONDITION(!is_template_type(t),
            "This is not a template type", 0);

    return t->template_parameters;
}

extern inline int template_type_get_num_specializations(type_t* t)
{
    ERROR_CONDITION(!is_template_type(t),
            "This is not a template type", 0);

    // +1 because of primary
    return t->type->num_unique_specialized_types + 1;
}

extern inline type_t* template_type_get_specialization_num(type_t* t, int i)
{
    ERROR_CONDITION(!is_template_type(t),
            "This is not a template type", 0);

    if (i == 0)
    {
        return t->type->primary_specialization;
    }
    else
    {
        return t->type->unique_specialized_types[i-1];
    }
}

extern inline void template_type_update_template_parameters(type_t* t, template_parameter_list_t* new_template_parameters)
{
    ERROR_CONDITION(!is_template_type(t),
            "This is not a template type", 0);

    template_parameter_list_t *template_parameters = t->template_parameters;

    ERROR_CONDITION(template_parameters->num_parameters 
            != new_template_parameters->num_parameters,
            "Template parameters should be of the same length", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: Updating template parameters\n");
    }

    int i;

    for (i = 0; i < template_parameters->num_parameters; i++)
    {
        template_parameter_t* template_parameter = template_parameters->parameters[i];
        template_parameter_t* new_template_parameter = new_template_parameters->parameters[i];

        ERROR_CONDITION ((new_template_parameter->kind != template_parameter->kind),
                        "Template parameter kinds do not match", 0);

        template_parameter_value_t* default_argument = template_parameters->arguments[i];
        template_parameter_value_t* new_default_argument = new_template_parameters->arguments[i];

        if (new_default_argument != NULL
                && default_argument == NULL)
        {
            template_parameters->arguments[i] = new_default_argument;
        }
    }
}

extern inline char is_template_specialized_type(type_t* t)
{
    return (t != NULL && t->info->is_template_specialized_type);
}

extern inline template_parameter_list_t* template_specialized_type_get_template_parameters(type_t* t)
{
    ERROR_CONDITION(!is_template_specialized_type(t),
            "This is not a template specialized type", 0);
    return t->template_parameters;
}

extern inline template_parameter_list_t* template_specialized_type_get_template_arguments(type_t* t)
{
    ERROR_CONDITION(!is_template_specialized_type(t),
            "This is not a template specialized type", 0);
    return t->template_arguments;
}

extern inline type_t* template_specialized_type_get_related_template_type(type_t* t)
{
    ERROR_CONDITION(!is_template_specialized_type(t),
            "This is not a template specialized type", 0);

    return t->related_template_type;
}

extern inline void template_specialized_type_update_template_parameters(type_t* t, template_parameter_list_t* template_parameters)
{
    ERROR_CONDITION(!is_template_specialized_type(t),
            "This is not a template specialized type", 0);
    t->template_parameters = template_parameters;
}

extern inline type_t* get_complex_type(type_t* t)
{
    ERROR_CONDITION(t == NULL, "Invalid base type for complex type", 0);

    static dhash_ptr_t *_complex_hash = NULL;

    if (_complex_hash == NULL)
    {
        _complex_hash = dhash_ptr_new(5);
    }

    type_t* result = dhash_ptr_query(_complex_hash, (const char*)t);

    if (result == NULL)
    {
        result = get_simple_type();

        result->type->kind = STK_COMPLEX;
        result->type->complex_element = t;
        // FIXME - A complex is always twice its base type?
        result->info->size = 2 * t->info->size;
        result->info->alignment = t->info->alignment;
        result->info->valid_size = 1;

        result->info->is_dependent = is_dependent_type(t);

        dhash_ptr_insert(_complex_hash, (const char*)t, result);
    }

    return result;
}

extern inline type_t* complex_type_get_base_type(type_t* t)
{
    ERROR_CONDITION(!is_complex_type(t), "This is not a complex type", 0);

    t = advance_over_typedefs(t);

    return t->type->complex_element;
}

static dhash_ptr_t *_qualification[(CV_CONST | CV_VOLATILE | CV_RESTRICT) + 1];
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
            _qualification[i] = dhash_ptr_new(5);
        }
        _qualif_hash_initialized = 1;
    }
}

static void _get_array_type_components(type_t* array_type, 
        nodecl_t *whole_size, nodecl_t *lower_bound, nodecl_t *upper_bound, const decl_context_t** decl_context,
        array_region_t** array_region,
        char *with_descriptor,
        char *is_string_literal);

static type_t* _get_array_type(type_t* element_type, 
        nodecl_t whole_size,
        nodecl_t lower_bound,
        nodecl_t upper_bound,
        const decl_context_t* decl_context,
        array_region_t* array_region,
        char with_descriptor,
        char is_string_literal,
        char force_dependent_type);

static type_t* _clone_array_type(type_t* array_type, type_t* new_element_type)
{
        nodecl_t whole_size = nodecl_null();
        nodecl_t lower_bound = nodecl_null();
        nodecl_t upper_bound = nodecl_null();
        char with_descriptor = 0;
        char is_string_literal = 0;
        array_region_t* array_region = NULL;

        const decl_context_t* decl_context;
        memset(&decl_context, 0, sizeof(decl_context));

        _get_array_type_components(array_type, &whole_size, &lower_bound, &upper_bound, &decl_context,
                &array_region,
                &with_descriptor,
                &is_string_literal);

        // And now rebuild the array type
        type_t* result = _get_array_type(new_element_type, 
                whole_size,
                lower_bound,
                upper_bound,
                decl_context,
                array_region,
                with_descriptor,
                is_string_literal,
                /* force_dependent_type */ 0);

        return result;
}

type_t* array_type_rebase(type_t* array_type, type_t* new_element_type)
{
    return _clone_array_type(array_type, new_element_type);
}

extern inline type_t* get_unqualified_type(type_t* t)
{
    type_t* orig_type = t;
    t = advance_over_typedefs(t);

    if (t->kind == TK_ARRAY)
    {
        type_t* element_unqualif = get_unqualified_type(t->array->element_type);
        return _clone_array_type(t, element_unqualif);
    }

    // Keep restrict attribute as it can't be discarded like const or volatile
    cv_qualifier_t cv = get_cv_qualifier(t);
    char is_restricted = (cv & CV_RESTRICT) == CV_RESTRICT;

    ERROR_CONDITION(t->unqualified_type == NULL, "This cannot be NULL", 0);

    if (!is_restricted)
    {
        if (cv == CV_NONE)
            // Return the original type
            return orig_type;
        else
            return t->unqualified_type;
    }
    else
    {
        if (cv == CV_RESTRICT)
            return get_restrict_qualified_type(orig_type);
        else
            return get_restrict_qualified_type(t->unqualified_type);
    }
}

static inline type_t *get_qualified_type(type_t *original,
                                         cv_qualifier_t cv_qualification,
                                         char qualify_arrays)
{
    // Ensure it is initialized
    init_qualification_hash();


    cv_qualifier_t old_cv_qualifier = CV_NONE;
    type_t* unchanged_type = original;

    original = advance_over_typedefs_with_cv_qualif(original, &old_cv_qualifier);

    // Try hard to preserve the type
    if (cv_qualification == old_cv_qualifier)
        return unchanged_type;

    // If we are just adding qualifiers, we can use the unchanged type
    if (original->kind != TK_ARRAY
            && is_more_cv_qualified(cv_qualification, old_cv_qualifier))
        original = unchanged_type;

    ERROR_CONDITION(original == NULL, "This cannot be NULL", 0);
    ERROR_CONDITION(original->unqualified_type == NULL, "This cannot be NULL", 0);

    // The standard forces us to do some strange things here
    if (!qualify_arrays && original->kind == TK_ARRAY)
    {
        // Now clone the type
        type_t *qualif_element_type
            = get_qualified_type(original->array->element_type,
                                 cv_qualification,
                                 /* qualify_arrays */ 0);
        return _clone_array_type(original, qualif_element_type);
    }

    if (cv_qualification == CV_NONE)
    {
        return original->unqualified_type;
    }

    // Lookup based on the unqualified type
    type_t* qualified_type = (type_t*)dhash_ptr_query(
            _qualification[(int)(cv_qualification)], 
            (const char*)original->unqualified_type);

    if (qualified_type == NULL)
    {
        qualified_type = new_empty_type();
        DELETE(qualified_type->info);
        *qualified_type = *original;
        qualified_type->cv_qualifier = cv_qualification;
        qualified_type->unqualified_type = original->unqualified_type;

        qualified_type->_advanced_type = NULL;

        dhash_ptr_insert(_qualification[(int)(cv_qualification)], 
                (const char*)original->unqualified_type, 
                qualified_type);
    }

    return qualified_type;
}

type_t *get_cv_qualified_array_type(type_t *array_type,
                                    cv_qualifier_t cv_qualifier)
{
    return get_qualified_type(array_type, cv_qualifier, /* qualify_arrays */ 1);
}


extern inline type_t *get_cv_qualified_type(type_t *t,
                                            cv_qualifier_t cv_qualifier)
{
    return get_qualified_type(t, cv_qualifier, /* qualify_arrays */ 0);
}

extern inline type_t *get_const_qualified_type(type_t *t)
{
    return get_qualified_type(
        t, (t->cv_qualifier | CV_CONST), /* qualify_arrays */ 0);
}

extern inline type_t *get_volatile_qualified_type(type_t *t)
{
    return get_qualified_type(
        t, (t->cv_qualifier | CV_VOLATILE), /* qualify_arrays */ 0);
}

extern inline type_t *get_restrict_qualified_type(type_t *t)
{
    return get_qualified_type(
        t, (t->cv_qualifier | CV_RESTRICT), /* qualify_arrays */ 0);
}

extern inline type_t* get_pointer_type(type_t* t)
{
    ERROR_CONDITION(t == NULL, "Invalid NULL type", 0);

    static dhash_ptr_t *_pointer_types = NULL;

    if (_pointer_types == NULL)
    {
        _pointer_types = dhash_ptr_new(5);
    }

    type_t* pointed_type = dhash_ptr_query(_pointer_types, (const char*)t);

    if (pointed_type == NULL)
    {
        pointed_type = new_empty_type();
        pointed_type->kind = TK_POINTER;
        pointed_type->unqualified_type = pointed_type;
        pointed_type->pointer = NEW0(pointer_info_t);
        pointed_type->pointer->pointee = t;

        if (is_array_type(t)
                && array_type_with_descriptor(t))
        {
            // This is an array with descriptor 
            // let cxx-typeenviron.c compute this size
        }
        else
        {
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
        }

        pointed_type->info->is_dependent = is_dependent_type(t);

        dhash_ptr_insert(_pointer_types, (const char*)t, pointed_type);
    }

    return pointed_type;
}

static dhash_ptr_t *_lvalue_reference_types = NULL;
static dhash_ptr_t *_rvalue_reference_types = NULL;
static dhash_ptr_t *_rebindable_reference_types = NULL;

static type_t* get_internal_reference_type(type_t* t, enum type_kind reference_kind)
{
    ERROR_CONDITION(t == NULL, "Invalid reference type", 0);

    if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        internal_error("Trying to create a reference to reference type." , 0);
    }

    ERROR_CONDITION(t == NULL,
            "Trying to create a reference of a null type", 0);

    dhash_ptr_t **reference_types = NULL;
    switch (reference_kind)
    {
        case TK_LVALUE_REFERENCE:
            {
                reference_types = &_lvalue_reference_types;
                break;
            }
        case TK_RVALUE_REFERENCE:
            {
                reference_types = &_rvalue_reference_types;
                break;
            }
        case TK_REBINDABLE_REFERENCE:
            {
                reference_types = &_rebindable_reference_types;
                break;
            }
        default:
            {
                internal_error("Invalid reference kind %d\n", reference_kind);
            }
    }

    if ((*reference_types) == NULL)
    {
        (*reference_types) = dhash_ptr_new(5);
    }

    dhash_ptr_t *reference_hash = *reference_types;

    type_t* referenced_type = dhash_ptr_query(reference_hash, (const char*)t);

    if (referenced_type == NULL)
    {
        referenced_type = new_empty_type();
        referenced_type->kind = reference_kind;
        referenced_type->unqualified_type = referenced_type;
        referenced_type->pointer = NEW0(pointer_info_t);
        referenced_type->pointer->pointee = t;

        referenced_type->info->is_dependent = is_dependent_type(t);

        dhash_ptr_insert(reference_hash, (const char*)t, referenced_type);
    }

    return referenced_type;
}

extern inline type_t* get_lvalue_reference_type(type_t* t)
{
    return get_internal_reference_type(t, TK_LVALUE_REFERENCE);
}

extern inline type_t* get_rvalue_reference_type(type_t* t)
{
    return get_internal_reference_type(t, TK_RVALUE_REFERENCE);
}

extern inline type_t* get_rebindable_reference_type(type_t* t)
{
    return get_internal_reference_type(t, TK_REBINDABLE_REFERENCE);
}

extern inline type_t* get_pointer_to_member_type(type_t* t, type_t* class_type)
{
    ERROR_CONDITION(t == NULL, "Invalid NULL type", 0);

    static dhash_ptr_t *_class_types = NULL;

    if (_class_types == NULL)
    {
        _class_types = dhash_ptr_new(5);
    }

    // First lookup using the class type
    dhash_ptr_t * class_type_hash = dhash_ptr_query(_class_types, (const char*)class_type);

    if (class_type_hash == NULL)
    {
        class_type_hash = dhash_ptr_new(5);

        dhash_ptr_insert(_class_types, (const char*)class_type, class_type_hash);
    }

    type_t* pointer_to_member = dhash_ptr_query(class_type_hash, (const char*)t);

    if (pointer_to_member == NULL)
    {
        pointer_to_member = new_empty_type();
        pointer_to_member->kind = TK_POINTER_TO_MEMBER;
        pointer_to_member->unqualified_type = pointer_to_member;
        pointer_to_member->pointer = NEW0(pointer_info_t);
        pointer_to_member->pointer->pointee = t;
        pointer_to_member->pointer->pointee_class_type = class_type;

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
            || is_dependent_type(class_type);

        dhash_ptr_insert(class_type_hash, (const char*)t, pointer_to_member);
    }

    return pointer_to_member;
}

typedef struct array_sized_hash
{
    _size_t whole_size;
    _size_t lower_bound;
    _size_t upper_bound;
    char with_descriptor;
    char is_string_literal;
    dhash_ptr_t *element_hash;
} array_sized_hash_t;

static array_sized_hash_t *_array_sized_hash = NULL;
static int _array_sized_hash_size = 0;

static dhash_ptr_t* _init_array_sized_hash(array_sized_hash_t *array_sized_hash_elem, 
        _size_t whole_size,
        _size_t lower_bound,
        _size_t upper_bound,
        char with_descriptor,
        char is_string_literal)
{
    array_sized_hash_elem->whole_size = whole_size;
    array_sized_hash_elem->lower_bound = lower_bound;
    array_sized_hash_elem->upper_bound = upper_bound;
    array_sized_hash_elem->with_descriptor = with_descriptor;
    array_sized_hash_elem->is_string_literal = is_string_literal;
    array_sized_hash_elem->element_hash = dhash_ptr_new(5);

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

static int array_hash_compar(const void* v1, const void* v2)
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
    else // (x0, y0, z0) == (x1, y1, z1)
    {
        if (a1->with_descriptor < a2->with_descriptor)
            return -1;
        else if (a1->with_descriptor > a2->with_descriptor)
            return 1;

        if (a1->is_string_literal < a2->is_string_literal)
            return -1;
        else if (a1->is_string_literal > a2->is_string_literal)
            return 1;
    }
    return 0;
}

static dhash_ptr_t* get_array_sized_hash(_size_t whole_size, _size_t lower_bound, _size_t upper_bound, 
        char with_descriptor,
        char is_string_literal)
{
    array_sized_hash_t key = { 
        .whole_size = whole_size, 
        .lower_bound = lower_bound, 
        .upper_bound = upper_bound, 
        .with_descriptor = with_descriptor,
        .is_string_literal = is_string_literal
    };

    array_sized_hash_t* sized_hash = bsearch(&key, 
            _array_sized_hash, _array_sized_hash_size, 
            sizeof(array_sized_hash_t), array_hash_compar);

    if (sized_hash == NULL)
    {
        _array_sized_hash_size++;
        _array_sized_hash = NEW_REALLOC(array_sized_hash_t, _array_sized_hash, _array_sized_hash_size);

        dhash_ptr_t* result = _init_array_sized_hash(&_array_sized_hash[_array_sized_hash_size - 1], 
                whole_size, lower_bound, upper_bound, with_descriptor, is_string_literal);

        // So we can use bsearch again
        qsort(_array_sized_hash, _array_sized_hash_size, sizeof(array_sized_hash_t), array_hash_compar);

        return result;
    }
    else
    {
        return sized_hash->element_hash;
    }
}

// This is used only for cloning array types
static void _get_array_type_components(type_t* array_type, 
        nodecl_t *whole_size, nodecl_t *lower_bound, nodecl_t *upper_bound, const decl_context_t** decl_context,
        array_region_t** array_region,
        char *with_descriptor,
        char *is_string_literal)
{
    ERROR_CONDITION((array_type->kind != TK_ARRAY), "Not an array type!", 0);

    *whole_size = array_type->array->whole_size;
    *lower_bound = array_type->array->lower_bound;
    *upper_bound = array_type->array->upper_bound;
    *decl_context = array_type->array->array_expr_decl_context;
    *array_region = array_type->array->region;
    *with_descriptor = array_type->array->with_descriptor;
    *is_string_literal = array_type->array->is_string_literal;
}

// This function owns the three trees passed to it (unless they are NULL, of
// course)
static type_t* _get_array_type(
        type_t* element_type, 
        nodecl_t whole_size,
        nodecl_t lower_bound,
        nodecl_t upper_bound,
        const decl_context_t* decl_context,
        array_region_t* array_region,
        char with_descriptor,
        char is_string_literal,
        char force_dependent_type)
{
    ERROR_CONDITION(element_type == NULL, "Invalid element type", 0);

    ERROR_CONDITION(!nodecl_is_null(whole_size)
            && (nodecl_is_null(lower_bound) || nodecl_is_null(upper_bound)),
            "Invalid definition of boundaries for array", 0);

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
    // First try to fold as many trees as possible
    if (!nodecl_is_null(whole_size))
    {
        struct {
            nodecl_t *nodecl;
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
        for (i = 0; data[i].nodecl != NULL; i++)
        {
            if (nodecl_is_null(*(data[i].nodecl)))
                continue;
            if (nodecl_is_constant(*(data[i].nodecl)))
            {
                *(data[i].pred) = 1;
                *(data[i].value) = const_value_cast_to_8(
                        nodecl_get_constant(*(data[i].nodecl)));
                // Simplify the tree now
                *(data[i].nodecl) = const_value_to_nodecl_with_basic_type_cached(
                        nodecl_get_constant(*(data[i].nodecl)),
                        get_ptrdiff_t_type());
            }
        }
    }

    type_t* result = NULL;

    if (nodecl_is_null(whole_size))
    {
        // Use the same strategy we use for pointers when all components (size,
        // lower, upper) of the array are null otherwise create a new array
        // every time (it is safer)
        static dhash_ptr_t *_undefined_array_types[2][2][2] = {
            { { NULL, NULL }, { NULL, NULL }  },
            { { NULL, NULL }, { NULL, NULL }  }
        };

        char is_dependent_array = force_dependent_type
            || is_dependent_type(element_type);

        if (_undefined_array_types[!!with_descriptor][!!is_string_literal][!!is_dependent_array] == NULL)
        {
            _undefined_array_types[!!with_descriptor][!!is_string_literal][!!is_dependent_array] = dhash_ptr_new(5);
        }

        type_t* undefined_array_type = NULL;
        if (nodecl_is_null(lower_bound)
                && nodecl_is_null(upper_bound)
                && array_region == NULL)
        {
            undefined_array_type = dhash_ptr_query(
                    _undefined_array_types[!!with_descriptor][!!is_string_literal][!!is_dependent_array],
                    (const char*)element_type);
        }
        if (undefined_array_type == NULL)
        {
            result = new_empty_type();
            result->kind = TK_ARRAY;
            result->unqualified_type = result;
            result->array = NEW0(array_info_t);
            result->array->element_type = element_type;
            result->array->whole_size = nodecl_null();

            // If we used the hash of array types, these two will be null
            result->array->lower_bound = lower_bound;
            result->array->upper_bound = upper_bound;

            result->array->with_descriptor = with_descriptor;
            result->array->is_string_literal = is_string_literal;

            result->array->region = array_region;

            // If the element_type is array propagate the 'is_vla' value
            if (is_array_type(element_type))
            {
                // If the element_type is array propagate the 'is_vla' value
                result->array->is_vla = array_type_is_vla(element_type);

                // Check that the descriptor attribute is consistent
                ERROR_CONDITION((with_descriptor != result->array->with_descriptor),
                        "Multidimensional array created with inconsistent descriptor flag", 0);
            }

            result->array->array_expr_decl_context = decl_context;

            // Arrays with descriptor have a complete type even if their
            // dimensions are not known at compile time (because the descriptor
            // is a complete type actually
            result->info->is_incomplete = !with_descriptor;

            result->info->is_dependent = is_dependent_array;

            if (nodecl_is_null(lower_bound)
                    && nodecl_is_null(upper_bound)
                    && array_region == NULL)
            {
                dhash_ptr_insert(
                        _undefined_array_types[!!with_descriptor][!!is_string_literal][!!is_dependent_array],
                        (const char*)element_type, result);
            }
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
                && upper_bound_is_constant
                && array_region == NULL)
        {
            dhash_ptr_t* array_sized_hash = get_array_sized_hash(whole_size_k,
                    lower_bound_k,
                    upper_bound_k,
                    with_descriptor,
                    is_string_literal);

            type_t* array_type = dhash_ptr_query(array_sized_hash, (const char*)element_type);

            if (array_type == NULL)
            {
                result = new_empty_type();
                result->kind = TK_ARRAY;
                result->unqualified_type = result;
                result->array = NEW0(array_info_t);
                result->array->element_type = element_type;

                result->array->with_descriptor = with_descriptor;

                if (is_array_type(element_type))
                {
                    // If the element_type is array propagate the 'is_vla' value
                    result->array->is_vla = array_type_is_vla(element_type);

                    // Check that the descriptor attribute is consistent
                    ERROR_CONDITION((with_descriptor != result->array->with_descriptor),
                            "Multidimensional array created with inconsistent descriptor flag", 0);
                }

                result->array->whole_size = whole_size;
                result->array->lower_bound = lower_bound;
                result->array->upper_bound = upper_bound;

                result->array->array_expr_decl_context = decl_context;
                result->info->is_dependent = is_dependent_type(element_type);

                result->array->is_string_literal = is_string_literal;

                dhash_ptr_insert(array_sized_hash, (const char*)element_type, result);
            }
            else
            {
                result = array_type;
            }
        }
        else
        {
            result = new_empty_type();
            result->kind = TK_ARRAY;
            result->unqualified_type = result;
            result->array = NEW0(array_info_t);
            result->array->element_type = element_type;
            result->array->whole_size = whole_size;
            result->array->lower_bound = lower_bound;
            result->array->upper_bound = upper_bound;
            result->array->array_expr_decl_context = decl_context;
            result->array->region = array_region;

            result->array->with_descriptor = with_descriptor;
            result->array->is_string_literal = is_string_literal;

            // In C This is a VLA
            if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
            {
                result->array->is_vla = 1;
            }
            // In Fortran, non constant arrays are considered VLAs unless they
            // require a descriptor
            if (IS_FORTRAN_LANGUAGE
                    && !with_descriptor)
            {
                result->array->is_vla = 1;
            }

            result->info->is_dependent = is_dependent_type(element_type);

            if (!result->info->is_dependent
                    && nodecl_expr_is_value_dependent(whole_size))
            {
                result->info->is_dependent = 1;
            }
        }
    }

    return result;
}

static nodecl_t get_zero_tree(const locus_t* locus)
{
    return nodecl_make_integer_literal(get_ptrdiff_t_type(),
            const_value_get_zero(type_get_size(get_ptrdiff_t_type()), 1),
            locus);
}

static nodecl_t get_one_tree(const locus_t* locus)
{
    return nodecl_make_integer_literal(get_ptrdiff_t_type(),
            const_value_get_one(type_get_size(get_ptrdiff_t_type()), 1),
            locus);
}

static nodecl_t convert_node_to_ptrdiff_t(nodecl_t n)
{
    if (nodecl_is_null(n))
        return n;

    if (nodecl_expr_is_value_dependent(n)
            || nodecl_expr_is_type_dependent(n))
        return n;

    if (!equivalent_types(
                get_unqualified_type(no_ref(nodecl_get_type(n))),
                get_ptrdiff_t_type()))
    {
        const_value_t* cv = nodecl_get_constant(n);
        n = nodecl_make_conversion(
                nodecl_shallow_copy(n),
                get_ptrdiff_t_type(),
                nodecl_get_locus(n));

        if (cv != NULL)
        {
            cv = const_value_cast_to_bytes(
                    cv,
                    type_get_size(get_ptrdiff_t_type()), /* sign */ 1);
            nodecl_set_constant(n, cv);
        }
    }

    return n;
}

extern inline type_t* get_array_type(type_t* element_type, nodecl_t whole_size, const decl_context_t* decl_context)
{
    whole_size = convert_node_to_ptrdiff_t(whole_size);

    nodecl_t lower_bound = nodecl_null();
    nodecl_t upper_bound = nodecl_null();
    if (!nodecl_is_null(whole_size))
    {
        lower_bound = get_zero_tree(nodecl_get_locus(whole_size));

        if (nodecl_is_constant(whole_size))
        {
            // Compute the constant
            const_value_t* c = const_value_sub(
                    const_value_cast_to_bytes(
                        nodecl_get_constant(whole_size),
                        type_get_size(get_ptrdiff_t_type()),
                        /* signed */ 1),
                    const_value_get_one(
                        /* bytes */ type_get_size(get_ptrdiff_t_type()),
                        /* signed */ 1));

            upper_bound = const_value_to_nodecl_with_basic_type_cached(c, get_ptrdiff_t_type());
        }
        else
        {
            nodecl_t temp = nodecl_shallow_copy(whole_size);

            upper_bound = nodecl_make_minus(
                    nodecl_make_parenthesized_expression(temp,
                        nodecl_get_type(temp),
                        nodecl_get_locus(whole_size)),
                    get_one_tree(nodecl_get_locus(whole_size)),
                    get_ptrdiff_t_type(),
                    nodecl_get_locus(whole_size));

            nodecl_expr_set_is_value_dependent(upper_bound,
                    nodecl_expr_is_value_dependent(whole_size));
        }
    }

    return _get_array_type(element_type, whole_size, lower_bound, upper_bound, decl_context, 
            /* array_region */ NULL,
            /* with_descriptor */ 0,
            /* is_string_literal */ 0,
            /* force_dependent_type */ 0);
}

static type_t* get_array_type_for_literal_string(type_t* element_type,
        nodecl_t whole_size,
        const decl_context_t* decl_context)
{
    whole_size = convert_node_to_ptrdiff_t(whole_size);

    nodecl_t lower_bound = nodecl_null();
    nodecl_t upper_bound = nodecl_null();
    if (!nodecl_is_null(whole_size))
    {
        lower_bound = get_zero_tree(nodecl_get_locus(whole_size));

        if (nodecl_is_constant(whole_size))
        {
            // Compute the constant
            const_value_t* c = const_value_sub(
                    nodecl_get_constant(whole_size),
                    const_value_get_one(
                        /* bytes */ type_get_size(get_ptrdiff_t_type()),
                        /* signed */ 1));

            upper_bound = const_value_to_nodecl_with_basic_type_cached(c,
                    get_ptrdiff_t_type());
        }
        else
        {
            nodecl_t temp = nodecl_shallow_copy(whole_size);

            upper_bound = nodecl_make_minus(
                    nodecl_make_parenthesized_expression(temp,
                        nodecl_get_type(temp),
                        nodecl_get_locus(whole_size)),
                    get_one_tree(nodecl_get_locus(whole_size)),
                    get_ptrdiff_t_type(),
                    nodecl_get_locus(whole_size));

            nodecl_expr_set_is_value_dependent(upper_bound,
                    nodecl_expr_is_value_dependent(whole_size));
        }
    }

    return _get_array_type(element_type, whole_size, lower_bound, upper_bound, decl_context,
            /* array_region */ NULL,
            /* with_descriptor */ 0,
            /* is_string_literal */ 1,
            /* force_dependent_type */ 0);
}

static nodecl_t compute_whole_size_given_bounds(
        nodecl_t lower_bound, 
        nodecl_t upper_bound)
{
    if(nodecl_is_null(lower_bound) 
            || nodecl_is_null(upper_bound))
        return nodecl_null();

    nodecl_t whole_size;
    if (nodecl_is_constant(lower_bound)
            && nodecl_is_constant(upper_bound))
    {
        whole_size = const_value_to_nodecl_with_basic_type_cached(
                const_value_add(
                    const_value_sub(
                        nodecl_get_constant(upper_bound),
                        nodecl_get_constant(lower_bound)),
                    const_value_get_one(type_get_size(get_ptrdiff_t_type()), 1)),
                get_ptrdiff_t_type());
    }
    else
    {
        nodecl_t one_tree = get_one_tree(nodecl_get_locus(lower_bound));

        lower_bound = nodecl_shallow_copy(lower_bound);
        upper_bound = nodecl_shallow_copy(upper_bound);

        whole_size = 
            nodecl_make_add(
                    nodecl_make_parenthesized_expression(
                        nodecl_make_minus(
                            upper_bound,
                            lower_bound,
                            get_ptrdiff_t_type(),
                            nodecl_get_locus(lower_bound)),
                        get_ptrdiff_t_type(),
                        nodecl_get_locus(lower_bound)),
                    one_tree,
                    get_ptrdiff_t_type(),
                    nodecl_get_locus(lower_bound));
    }

    return whole_size;
}

static type_t* get_array_type_bounds_common(type_t* element_type,
        nodecl_t lower_bound,
        nodecl_t upper_bound,
        const decl_context_t* decl_context,
        char with_descriptor)
{
    lower_bound = convert_node_to_ptrdiff_t(lower_bound);
    upper_bound = convert_node_to_ptrdiff_t(upper_bound);

    nodecl_t whole_size = compute_whole_size_given_bounds(lower_bound, upper_bound);

    return _get_array_type(element_type, whole_size, lower_bound, upper_bound, decl_context,
            /* array_region */ NULL,
            with_descriptor,
            /* is_string_literal */ 0,
            /* force_dependent_type */ 0);
}

extern inline type_t* get_array_type_bounds(type_t* element_type,
        nodecl_t lower_bound,
        nodecl_t upper_bound,
        const decl_context_t* decl_context)
{
    return get_array_type_bounds_common(element_type, lower_bound, upper_bound, decl_context, /* with_descriptor */ 0);
}

extern inline type_t* get_array_type_bounds_with_descriptor(type_t* element_type,
        nodecl_t lower_bound,
        nodecl_t upper_bound,
        const decl_context_t* decl_context)
{
    return get_array_type_bounds_common(element_type, lower_bound, upper_bound, decl_context, /* with_descriptor */ 1);
}

extern inline type_t* get_array_type_bounds_with_regions(type_t* element_type,
        nodecl_t lower_bound,
        nodecl_t upper_bound,
        const decl_context_t* decl_context,
        nodecl_t region,
        const decl_context_t* region_decl_context)
{
    lower_bound = nodecl_shallow_copy(lower_bound);
    upper_bound = nodecl_shallow_copy(upper_bound);

    lower_bound = convert_node_to_ptrdiff_t(lower_bound);
    upper_bound = convert_node_to_ptrdiff_t(upper_bound);

    nodecl_t whole_size = compute_whole_size_given_bounds(lower_bound, upper_bound);

    nodecl_t region_lower_bound = nodecl_get_child(region, 0);
    nodecl_t region_upper_bound = nodecl_get_child(region, 1);
    nodecl_t region_stride = nodecl_get_child(region, 2);

    region_lower_bound = convert_node_to_ptrdiff_t(region_lower_bound);
    region_upper_bound = convert_node_to_ptrdiff_t(region_upper_bound);
    region_stride = convert_node_to_ptrdiff_t(region_stride);

    nodecl_t region_whole_size = compute_whole_size_given_bounds(region_lower_bound, region_upper_bound);

    array_region_t* array_region = NEW0(array_region_t);
    array_region->lower_bound = region_lower_bound;
    array_region->upper_bound = region_upper_bound;
    array_region->stride = region_stride;
    array_region->whole_size = region_whole_size;
    array_region->region_decl_context = region_decl_context;

    return _get_array_type(element_type, whole_size, lower_bound, upper_bound, decl_context, 
            array_region,
            /* with_descriptor */ 0,
            /* is_string_literal */ 0,
            /* force_dependent_type */ 0);
}

extern inline type_t* get_array_type_unknown_size_dependent(type_t* element_type)
{
    return _get_array_type(
            element_type,
            /* whole_size */ nodecl_null(),
            /* lower_bound */ nodecl_null(),
            /* upper_bound */ nodecl_null(),
            CURRENT_COMPILED_FILE->global_decl_context,
            /* array_region */ NULL,
            /* with_descriptor */ 0,
            /* is_string_literal */ 0,
            /* force_dependent_type */ 1);
}

static dhash_ptr_t* get_vector_sized_hash(unsigned int vector_size)
{
    static rb_red_blk_tree *_vector_size_hash = NULL;

    if (_vector_size_hash == NULL)
    {
        _vector_size_hash = rb_tree_create(uint_comp, null_dtor, null_dtor);
    }

    dhash_ptr_t* result = (dhash_ptr_t*)rb_tree_query_uint(_vector_size_hash, vector_size);

    if (result == NULL)
    {
        dhash_ptr_t* new_hash = dhash_ptr_new(5);

        unsigned int *k = NEW(unsigned int);
        *k = vector_size;
        rb_tree_insert(_vector_size_hash, k, new_hash);

        result = new_hash;
    }

    return result;
}

extern inline type_t* get_vector_type_by_bytes(type_t* element_type, unsigned int vector_size)
{
    ERROR_CONDITION(element_type == NULL, "Invalid type", 0);

    dhash_ptr_t *_vector_hash = get_vector_sized_hash(vector_size);

    type_t* result = dhash_ptr_query(_vector_hash, (const char*)element_type);

    if (result == NULL)
    {
        result = get_simple_type();

        result->type->kind = STK_VECTOR;
        result->type->vector_element = element_type;
        result->type->vector_size = vector_size;

        result->info->valid_size = 1;
        result->info->size = vector_size;
        result->info->alignment = vector_size;

        result->info->is_dependent = is_dependent_type(element_type);

        dhash_ptr_insert(_vector_hash, (const char*)element_type, result);
    }

    return result;
}

extern inline type_t* get_vector_type_by_elements(type_t* element_type, unsigned int num_elements)
{
    return get_vector_type_by_bytes(
            element_type,
            num_elements * type_get_size(element_type));
}

extern inline char is_vector_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && is_non_derived_type(t)
            && t->type->kind == STK_VECTOR)
        // Compatibility with Intel vector types
        || (is_intel_vector_struct_type(t, NULL));
}

extern inline type_t* get_generic_vector_type(type_t* element_type)
{
    return get_vector_type_by_bytes(element_type, 0);
}

extern inline char is_generic_vector_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (is_vector_type(t) && t->type->vector_size == 0);
}

extern inline int vector_type_get_vector_size_in_bytes(type_t* t)
{
    ERROR_CONDITION(!is_vector_type(t), "This is not a vector type", 0);
    t = advance_over_typedefs(t);

    int intel_vector_size = 0;
    if (is_intel_vector_struct_type(t, &intel_vector_size))
    {
        return vector_type_get_vector_size_in_bytes(
                intel_vector_struct_type_get_vector_type(t));
    }
    else
    {
        return t->type->vector_size;
    }
}

extern inline type_t* vector_type_get_element_type(type_t* t)
{
    ERROR_CONDITION(!is_vector_type(t), "This is not a vector type", 0);
    t = advance_over_typedefs(t);

    if (is_intel_vector_struct_type(t, NULL))
    {
        return vector_type_get_element_type(
                intel_vector_struct_type_get_vector_type(t));
    }
    else
    {
        return t->type->vector_element;
    }
}

extern inline int vector_type_get_num_elements(type_t* t)
{
    ERROR_CONDITION(!is_vector_type(t), "This is not a vector type", 0);
    t = advance_over_typedefs(t);

    return vector_type_get_vector_size_in_bytes(t) / type_get_size(vector_type_get_element_type(t));
}

static type_t* _get_new_function_type(type_t* t,
        parameter_info_t* parameter_info, int num_parameters,
        char is_trailing,
        ref_qualifier_t ref_qualifier)
{
    type_t* result = new_empty_type();

    result->kind = TK_FUNCTION;
    result->unqualified_type = result;
    result->function = NEW0(function_info_t);
    result->function->ref_qualifier = ref_qualifier;
    result->function->is_trailing = is_trailing;
    result->function->return_type = t;

    result->function->parameter_list = NEW_VEC0(parameter_info_t*, num_parameters);
    result->function->num_parameters = num_parameters;

    int i;
    for (i = 0; i < num_parameters; i++)
    {
        parameter_info_t* new_parameter = NEW0(parameter_info_t);

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

    type_t* result = NEW0(type_t);
    *result = *class_type;

    result->unqualified_type = result;

    result->_advanced_type = NULL;

    // These are the parts relevant for duplication
    result->info = NEW0(common_type_info_t);
    *result->info = *class_type->info;

    result->type = NEW0(simple_type_t);
    *result->type = *class_type->type;

    result->type->class_info = NEW0(class_info_t);
    *result->type->class_info = *class_type->type->class_info;

    return result;
}

static type_t* _get_duplicated_function_type(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type!", 0);

    function_type = advance_over_typedefs(function_type);

    int num_parameters = function_type->function->num_parameters;
    parameter_info_t parameter_list[num_parameters + 1];
    ref_qualifier_t ref_qualifier = function_type->function->ref_qualifier;
    char is_trailing = function_type->function->is_trailing;

    int i;
    for (i = 0; i < num_parameters; i++)
    {
        parameter_list[i] = *(function_type->function->parameter_list[i]);
    }

    type_t* result = _get_new_function_type(
            function_type->function->return_type,
            parameter_list,
            num_parameters,
            is_trailing,
            ref_qualifier);

    // Preserve the cv qualifier
    result = get_cv_qualified_type(result, get_cv_qualifier(function_type));

    return result;
}

static
type_t* get_new_function_type_common(type_t* t,
        parameter_info_t* parameter_info, int num_parameters,
        char is_trailing,
        ref_qualifier_t ref_qualifier)
{
#define MY_MAX(a, b) ((a) > (b) ? (a) : (b))
#define MAX_REF_QUALIFIER MY_MAX(REF_QUALIFIER_NONE, MY_MAX(REF_QUALIFIER_RVALUE, REF_QUALIFIER_LVALUE))
    static type_trie_t* _function_tries[2][2][MAX_REF_QUALIFIER + 1] = { };
#undef MAX_REF_QUALIFIER
#undef MY_MAX

    type_trie_t* used_trie = NULL;

    if (_function_tries[!!is_trailing][!!t][ref_qualifier] == NULL)
    {
        _function_tries[!!is_trailing][!!t][ref_qualifier] = allocate_type_trie();
    }

    used_trie = _function_tries[!!is_trailing][!!t][ref_qualifier];

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
        type_t* new_funct_type = _get_new_function_type(t, parameter_info, num_parameters, is_trailing, ref_qualifier);
        insert_type_trie(used_trie, type_seq, num_parameters + 1, new_funct_type);
        function_type = new_funct_type;

        set_is_dependent_type(function_type, fun_type_is_dependent);
    }

    return function_type;
}

extern inline type_t* get_new_function_type(type_t* t,
        parameter_info_t* parameter_info, int num_parameters,
        ref_qualifier_t ref_qualifier)
{
    return get_new_function_type_common(t, parameter_info, num_parameters, /* is_trailing */ 0, ref_qualifier);
}

extern inline type_t* get_new_function_type_trailing_type(type_t* t,
        parameter_info_t* parameter_info, int num_parameters,
        ref_qualifier_t ref_qualifier)
{
    return get_new_function_type_common(t, parameter_info, num_parameters, /* is_trailing */ 1, ref_qualifier);
}

char function_type_get_has_trailing_return(type_t *t)
{
    ERROR_CONDITION(!is_function_type(t), "Invalid type", 0);

    t = advance_over_typedefs(t);

    return t->function->is_trailing;
}

extern inline ref_qualifier_t function_type_get_ref_qualifier(type_t* t)
{
    ERROR_CONDITION(!is_function_type(t), "Invalid type", 0);

    t = advance_over_typedefs(t);

    return t->function->ref_qualifier;
}


extern inline type_t* get_nonproto_function_type(type_t* t, int num_parameters)
{
    // This type is not efficiently managed
    type_t* result = new_empty_type();

    result->kind = TK_FUNCTION;
    result->unqualified_type = result;
    result->function = NEW0(function_info_t);
    result->function->return_type = t;
    result->function->lacks_prototype = 1;

    int i;
    for (i = 0; i < num_parameters; i++)
    {
        parameter_info_t* new_parameter = NEW0(parameter_info_t);

        new_parameter->type_info = get_signed_int_type();

        P_LIST_ADD(result->function->parameter_list, 
                result->function->num_parameters, new_parameter);
    }

    return result;
}

extern inline int function_type_get_num_parameters(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    function_type = advance_over_typedefs(function_type);

    return function_type->function->num_parameters;
}

extern inline type_t* function_type_get_parameter_type_num(type_t* function_type, int num_param)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    function_type = advance_over_typedefs(function_type);

    ERROR_CONDITION(num_param >= function_type->function->num_parameters, 
            "Requested parameter %d out of bounds (number of parameters is %d)", 
            num_param, function_type->function->num_parameters);

    return function_type->function->parameter_list[num_param]->type_info;
}

extern inline type_t* function_type_get_nonadjusted_parameter_type_num(type_t* function_type, int num_param)
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

extern inline char class_type_is_incomplete_dependent(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);
    return t->info->is_template_specialized_type
        && t->info->is_dependent
        && t->info->is_incomplete;
}

extern inline char class_type_is_complete_dependent(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);
    return t->info->is_template_specialized_type
        && t->info->is_dependent
        && !t->info->is_incomplete;
}

extern inline char class_type_is_incomplete_independent(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);
    return t->info->is_template_specialized_type
        && !t->info->is_dependent
        && t->info->is_incomplete;
}

char class_type_is_complete_independent(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);
    return t->info->is_template_specialized_type
        && !t->info->is_dependent
        && !t->info->is_incomplete;
}

extern inline char class_type_is_empty(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "Invalid class type", 0);

    // If the class is dynamic it cannot empty
    if (class_type_is_dynamic(t))
        return 0;

    type_t* class_type = get_actual_class_type(t);

    int num_of_non_empty_nonstatics_data_members = 0;

    scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(class_type);
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (!symbol_entity_specs_get_is_bitfield(entry)
                || const_value_is_nonzero(nodecl_get_constant(symbol_entity_specs_get_bitfield_size(entry))))
        {
            num_of_non_empty_nonstatics_data_members++;
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(nonstatic_data_members);

    char has_virtual_bases = 0;

    char has_nonempty_bases = 0;

    int i;
    for (i = 0; i < class_type_get_num_bases(class_type); i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        char is_expansion = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        scope_entry_t* base_class = class_type_get_base_num(class_type, i,
                &is_virtual, &is_dependent, &is_expansion, &access_specifier);

        if (is_dependent)
            continue;

        has_virtual_bases |= is_virtual;

        has_nonempty_bases |= !class_type_is_empty(base_class->type_information);
    }

    return (num_of_non_empty_nonstatics_data_members == 0
            && !has_virtual_bases
            && !has_nonempty_bases);
}

extern inline char class_type_is_abstract(type_t* class_type)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type!", 0);
    class_type = get_actual_class_type(class_type);
    return class_type->type->class_info->is_abstract;
}

extern inline void class_type_set_is_abstract(type_t* class_type, char is_abstract)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type!", 0);
    class_type = get_actual_class_type(class_type);
    class_type->type->class_info->is_abstract = is_abstract;
}

extern inline char class_type_is_lambda(type_t* class_type)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type!", 0);
    class_type = get_actual_class_type(class_type);
    return class_type->type->class_info->is_lambda;
}

extern inline void class_type_set_is_lambda(type_t* class_type, char is_lambda)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type!", 0);
    class_type = get_actual_class_type(class_type);
    class_type->type->class_info->is_lambda = is_lambda;
}

extern inline char class_type_is_polymorphic(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type!", 0);

    type_t* class_type = get_actual_class_type(t);

    scope_entry_list_t* virtuals = class_type_get_virtual_functions(class_type);
    // If we have virtual functions we are dynamic
    if (virtuals != NULL)
    {
        entry_list_free(virtuals);
        return 1;
    }

    return 0;
}

extern inline char class_type_is_dynamic(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type!", 0);

    type_t* class_type = get_actual_class_type(t);

    scope_entry_list_t* virtuals = class_type_get_virtual_functions(class_type);
    // If we have virtual functions we are dynamic
    if (virtuals != NULL)
    {
        entry_list_free(virtuals);
        return 1;
    }

    // If our destructor is dynamic, we are dynamic
    scope_entry_t* destructor = class_type_get_destructor(class_type);

    if (destructor != NULL
            && symbol_entity_specs_get_is_virtual(destructor))
        return 1;

    // If any of our bases is dynamic or a virtual base, we are dynamic
    int num_bases = class_type_get_num_bases(class_type);
    int i;
    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        char is_expansion = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        scope_entry_t* base_class = class_type_get_base_num(class_type, i,
                &is_virtual, &is_dependent, &is_expansion, &access_specifier);

        if (is_dependent)
            continue;

        if (is_virtual
                || class_type_is_dynamic(base_class->type_information))
            return 1;
    }

    return 0;
}

static char has_non_virtual_empty_base_class_not_zero_offset_rec(type_t* class_type,
        type_t* list[MCXX_MAX_SCOPES_NESTING], int num_elems)
{
    ERROR_CONDITION(!is_unnamed_class_type(class_type), "Invalid unnamed class type", 0);

    int i;
    for (i = 0; i < class_type_get_num_bases(class_type); i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        char is_expansion = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        scope_entry_t* base_class = class_type_get_base_num(class_type, i,
                &is_virtual, &is_dependent, &is_expansion, &access_specifier);

        if (is_dependent)
            continue;

        // If not morally virtual and empty
        if (!is_virtual
                && class_type_is_empty(base_class->type_information))
        {
            ERROR_CONDITION(num_elems == MCXX_MAX_SCOPES_NESTING,
                    "Too deep hierarchy > %d\n", MCXX_MAX_SCOPES_NESTING);

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

    type_t* list[MCXX_MAX_SCOPES_NESTING] = { 0 };

    return has_non_virtual_empty_base_class_not_zero_offset_rec(
            get_actual_class_type(class_type), 
            list, /* num_elems */ 0);
}

extern inline char class_type_is_nearly_empty(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type!", 0);

    // A nearly empty class must be dynamic
    if (!class_type_is_dynamic(t))
        return 0;

    type_t* class_type = get_actual_class_type(t);

    scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(class_type);
    scope_entry_list_iterator_t* it = NULL;
    char empty = 1;
    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it) && empty;
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (!symbol_entity_specs_get_is_bitfield(entry)
                || const_value_is_nonzero(nodecl_get_constant(symbol_entity_specs_get_bitfield_size(entry))))
        {
            // If we are not empty, we are not nearly empty either
            empty = 0;
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(nonstatic_data_members);

    if (!empty)
        return 0;

    // This is implemented likewise it is in GCC
    char seen_non_virtual_nearly_empty = 0;

    int i;
    for (i = 0; i < class_type_get_num_bases(class_type); i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        char is_expansion = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        scope_entry_t* base_class = class_type_get_base_num(class_type, i,
                &is_virtual, &is_dependent, &is_expansion, &access_specifier);

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

extern inline char class_type_get_is_dependent(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);
    return t->info->is_dependent;
}

extern inline void class_type_set_enclosing_class_type(type_t* t, type_t* enclosing_class_type)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);

    ERROR_CONDITION(enclosing_class_type != NULL 
            && !is_class_type(enclosing_class_type), "This is not a class type", 0);

    t = get_actual_class_type(t);

    t->type->class_info->enclosing_class_type = enclosing_class_type;
}

extern inline void class_type_set_is_packed(type_t* t, char is_packed)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);

    t = get_actual_class_type(t);

    t->type->class_info->is_packed = is_packed;
}

extern inline char class_type_is_packed(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);

    t = get_actual_class_type(t);

    return t->type->class_info->is_packed;
}

extern inline type_t* class_type_get_enclosing_class_type(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);

    t = get_actual_class_type(t);

    return t->type->class_info->enclosing_class_type;
}

extern inline int class_type_get_num_bases(type_t* class_type)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);
    class_info_t* class_info = class_type->type->class_info;

    return class_info->num_bases;
}

extern inline void class_type_set_destructor(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);
    class_type->type->class_info->destructor = entry;
}

extern inline scope_entry_t* class_type_get_destructor(type_t* class_type)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);
    return class_type->type->class_info->destructor;
}

extern inline void class_type_set_default_constructor(type_t* class_type, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);
    class_type->type->class_info->default_constructor = entry;
}

extern inline scope_entry_t* class_type_get_default_constructor(type_t* class_type)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);
    return class_type->type->class_info->default_constructor;
}

static scope_entry_list_t* _class_type_get_members(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    return t->type->class_info->members;
}

static scope_entry_list_t* _class_type_get_friends(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    return t->type->class_info->friends;
}

extern inline scope_entry_list_t* class_type_get_friends(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);
    scope_entry_list_t* friends = _class_type_get_friends(t);

    return entry_list_copy(friends);
}

static scope_entry_list_t* _class_type_get_inherited_constructors(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    return t->type->class_info->inherited_constructors;
}

extern inline scope_entry_list_t* class_type_get_inherited_constructors(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);
    scope_entry_list_t* friends = _class_type_get_inherited_constructors(t);

    return entry_list_copy(friends);
}

extern inline scope_entry_list_t* class_type_get_members(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);
    scope_entry_list_t* members = _class_type_get_members(t);

    return entry_list_copy(members);
}

extern inline member_declaration_info_t* class_type_get_member_declarations(type_t* t, int *num_declarations)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    member_declaration_info_t* mdi = t->type->class_info->member_declarations;

    if (mdi == NULL)
    {
        *num_declarations = 0;
        return NULL;
    }

    int num_decls = t->type->class_info->num_member_declarations;
    member_declaration_info_t* result = NEW_VEC(member_declaration_info_t, num_decls);
    memcpy(result, mdi, sizeof(*result) * num_decls);

    *num_declarations = num_decls;
    return result;
}

static scope_entry_list_t* _class_type_get_members_pred(type_t* t, void* data, char (*fun)(scope_entry_t*, void*))
{
    scope_entry_list_t* members = _class_type_get_members(t);

    scope_entry_list_t* result = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(members);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current = entry_list_iterator_current(it);
        if (fun(current, data))
        {
            result = entry_list_add(result, current);
        }
    }
    entry_list_iterator_free(it);

    return result;
}

static char _member_is_conversion(scope_entry_t* entry, void* data UNUSED_PARAMETER)
{
    return symbol_entity_specs_get_is_member(entry)
        && symbol_entity_specs_get_is_conversion(entry);
}

extern inline scope_entry_list_t* class_type_get_conversions(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    return _class_type_get_members_pred(t, NULL, _member_is_conversion);
}

static char _member_is_member_function(scope_entry_t* entry, void* data UNUSED_PARAMETER)
{
    return symbol_entity_specs_get_is_member(entry) && entry->kind == SK_FUNCTION;
}

extern inline scope_entry_list_t* class_type_get_member_functions(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    return _class_type_get_members_pred(t, NULL, _member_is_member_function);
}

static char _member_is_data_member(scope_entry_t* entry, void* data UNUSED_PARAMETER)
{
    return symbol_entity_specs_get_is_member(entry) && entry->kind == SK_VARIABLE;
}

static char _member_is_static_data_member(scope_entry_t* entry, void* data UNUSED_PARAMETER)
{
    return _member_is_data_member(entry, data) && symbol_entity_specs_get_is_static(entry);
}

static char _member_is_nonstatic_data_member(scope_entry_t* entry, void* data UNUSED_PARAMETER)
{
    return _member_is_data_member(entry, data) && !symbol_entity_specs_get_is_static(entry);
}

extern inline scope_entry_list_t* class_type_get_nonstatic_data_members(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    return _class_type_get_members_pred(t, NULL, _member_is_nonstatic_data_member);
}

extern inline scope_entry_list_t* class_type_get_static_data_members(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    return _class_type_get_members_pred(t, NULL, _member_is_static_data_member);
}

static char _member_is_move_constructor(scope_entry_t* entry, void* data UNUSED_PARAMETER)
{
    return symbol_entity_specs_get_is_move_constructor(entry);
}

extern inline scope_entry_list_t* class_type_get_move_constructors(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    return _class_type_get_members_pred(t, NULL, _member_is_move_constructor);
}

static char _member_is_copy_constructor(scope_entry_t* entry, void* data UNUSED_PARAMETER)
{
    return symbol_entity_specs_get_is_copy_constructor(entry);
}

extern inline scope_entry_list_t* class_type_get_copy_constructors(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    return _class_type_get_members_pred(t, NULL, _member_is_copy_constructor);
}

static char _member_is_move_assignment_operator(scope_entry_t* entry, void* data UNUSED_PARAMETER)
{
    return symbol_entity_specs_get_is_move_assignment_operator(entry);
}

extern inline scope_entry_list_t* class_type_get_move_assignment_operators(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    return _class_type_get_members_pred(t, NULL, _member_is_move_assignment_operator);
}

static char _member_is_constructor(scope_entry_t* entry, void* data UNUSED_PARAMETER)
{
    return symbol_entity_specs_get_is_constructor(entry);
}

extern inline scope_entry_list_t* class_type_get_constructors(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    return _class_type_get_members_pred(t, NULL, _member_is_constructor);
}

static char _member_is_copy_assignment_operator(scope_entry_t* entry, void* data UNUSED_PARAMETER)
{
    return symbol_entity_specs_get_is_copy_assignment_operator(entry);
}

extern inline scope_entry_list_t* class_type_get_copy_assignment_operators(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    return _class_type_get_members_pred(t, NULL, _member_is_copy_assignment_operator);
}

static void class_type_get_virtual_base_classes_rec(type_t* t, scope_entry_list_t** result, char canonical)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    int i, num_bases = class_type_get_num_bases(t);
    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        char is_expansion = 0;
        access_specifier_t access_spec = AS_UNKNOWN;

        scope_entry_t* base = class_type_get_base_num(t, i,
                &is_virtual, &is_dependent, &is_expansion, &access_spec);

        class_type_get_virtual_base_classes_rec(base->type_information, result, canonical);

        if (!is_virtual || is_dependent)
            continue;

        if (canonical && base->kind == SK_CLASS)
            base = class_symbol_get_canonical_symbol(base);

        *result = entry_list_add_once(*result, base);
    }
}

static scope_entry_list_t* class_type_get_virtual_base_classes_(type_t* t, char canonical)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    scope_entry_list_t* result = NULL;
    class_type_get_virtual_base_classes_rec(t, &result, canonical);

    return result;
}

extern inline scope_entry_list_t* class_type_get_virtual_base_classes(type_t* t)
{
    return class_type_get_virtual_base_classes_(t, /* canonical */ 0);
}

extern inline scope_entry_list_t* class_type_get_virtual_base_classes_canonical(type_t* t)
{
    return class_type_get_virtual_base_classes_(t, /* canonical */ 1);
}

extern inline scope_entry_list_t* class_type_get_direct_base_classes_(type_t* t, char canonical)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    scope_entry_list_t* result = NULL;
    int i, num_bases = class_type_get_num_bases(t);
    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        char is_expansion = 0;
        access_specifier_t access_spec = AS_UNKNOWN;

        scope_entry_t* base = class_type_get_base_num(t, i,
                &is_virtual, &is_dependent, &is_expansion, &access_spec);

        if (canonical && base->kind == SK_CLASS)
        {
                base = class_symbol_get_canonical_symbol(base);
        }

        if (is_virtual || is_dependent)
            continue;

        result = entry_list_add(result, base);
    }

    return result;
}

extern inline scope_entry_list_t* class_type_get_direct_base_classes(type_t* t)
{
    return class_type_get_direct_base_classes_(t, /* canonical */ 0);
}

extern inline scope_entry_list_t* class_type_get_direct_base_classes_canonical(type_t* t)
{
    return class_type_get_direct_base_classes_(t, /* canonical */ 1);
}

static char _member_is_virtual_member_function(scope_entry_t* entry, void* data UNUSED_PARAMETER)
{
    return _member_is_member_function(entry, data) && symbol_entity_specs_get_is_virtual(entry);
}

extern inline scope_entry_list_t* class_type_get_virtual_functions(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);

    int i, num_bases = class_type_get_num_bases(t);

    scope_entry_list_t* result = _class_type_get_members_pred(t, NULL, _member_is_virtual_member_function);

    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        char is_expansion = 0;
        access_specifier_t access_spec = AS_UNKNOWN;
        scope_entry_t* base_class = class_type_get_base_num(t, i,
                &is_virtual, &is_dependent, &is_expansion, &access_spec);

        result = entry_list_merge(result, class_type_get_virtual_functions(base_class->type_information));
    }

    return result;
}

static char is_same_member_declaration(member_declaration_info_t mdi1,
        member_declaration_info_t mdi2)
{
    return (mdi1.entry == mdi2.entry)
        && (mdi1.is_definition == mdi2.is_definition);
}

extern inline void class_type_add_member(type_t* class_type,
        scope_entry_t* entry,
        const decl_context_t* decl_context,
        char is_definition)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);

    // It may happen that a type is added twice (redeclared classes ...)
    class_type->type->class_info->members = entry_list_add_once(class_type->type->class_info->members, entry);

    // Keep the declaration list
    member_declaration_info_t mdi = { entry, decl_context, is_definition };
    P_LIST_ADD_ONCE_FUN(class_type->type->class_info->member_declarations,
        class_type->type->class_info->num_member_declarations,
        mdi, is_same_member_declaration);
}

extern inline void class_type_add_member_after(
        type_t* class_type,
        scope_entry_t* position,
        scope_entry_t* entry,
        const decl_context_t* decl_context,
        char is_definition)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);

    class_type->type->class_info->members = entry_list_add_after(class_type->type->class_info->members, position, entry);

    // Find from the end
    int i;
    char found = 0;
    // Note the -2 if position is the last, we do not have to do anything special
    for (i = class_type->type->class_info->num_member_declarations - 2; i >= 0 && !found; i--)
    {
        if (class_type->type->class_info->member_declarations[i].entry == position)
        {
            found = 1;
            break;
        }
    }

    member_declaration_info_t mdi = { entry, decl_context, is_definition };
    P_LIST_ADD(class_type->type->class_info->member_declarations,
            class_type->type->class_info->num_member_declarations,
            mdi);

    if (found)
    {
        // Shift right all elements right of "i"
        i++; // Now i is where we will write

        memmove(&class_type->type->class_info->member_declarations[i+1],
                &class_type->type->class_info->member_declarations[i],
                (class_type->type->class_info->num_member_declarations - i - 1)
                * sizeof(class_type->type->class_info->member_declarations[i]));

        class_type->type->class_info->member_declarations[i] = mdi;
    }
}


extern inline void class_type_add_member_before(type_t* class_type,
        scope_entry_t* position,
        scope_entry_t* entry,
        const decl_context_t* decl_context,
        char is_definition)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);

    class_type->type->class_info->members = entry_list_add_before(class_type->type->class_info->members, position, entry);

    // Find from the beginning
    int i;
    char found = 0;
    for (i = 0; i < class_type->type->class_info->num_member_declarations && !found; i++)
    {
        if (class_type->type->class_info->member_declarations[i].entry == position)
        {
            found = 1;
            break;
        }
    }

    member_declaration_info_t mdi = { entry, decl_context, is_definition };
    P_LIST_ADD(class_type->type->class_info->member_declarations,
            class_type->type->class_info->num_member_declarations,
            mdi);

    if (found)
    {
        // Shift right all elements right of "i"
        memmove(&class_type->type->class_info->member_declarations[i+1],
                &class_type->type->class_info->member_declarations[i],
                (class_type->type->class_info->num_member_declarations - i - 1)
                * sizeof(class_type->type->class_info->member_declarations[i]));

        class_type->type->class_info->member_declarations[i] = mdi;
    }
}

static scope_entry_t* get_class_symbol(scope_entry_t* entry)
{
    if (entry->kind == SK_TYPEDEF
            || entry->kind == SK_TEMPLATE_ALIAS)
    {
        type_t* t = advance_over_typedefs(entry->type_information);
        if (is_named_type(t))
            entry = named_type_get_symbol(t);
    }

    return entry;
}

extern inline void class_type_complete_if_needed(scope_entry_t* entry, const decl_context_t* decl_context, const locus_t* locus)
{
    entry = get_class_symbol(entry);

    ERROR_CONDITION(entry->kind != SK_CLASS, "Invalid symbol", 0);

    if (is_template_specialized_type(get_actual_class_type(entry->type_information)))
        instantiate_template_class_if_needed(entry, decl_context, locus);
    else if (symbol_entity_specs_get_is_member(entry)
            && symbol_entity_specs_get_emission_template(entry) != NULL)
        instantiate_nontemplate_member_class_if_needed(entry, decl_context, locus);
}

extern inline char class_type_complete_if_possible(scope_entry_t* entry, const decl_context_t* decl_context, const locus_t* locus)
{
    entry = get_class_symbol(entry);

    ERROR_CONDITION(entry->kind != SK_CLASS, "Invalid symbol", 0);

    if (is_template_specialized_type(get_actual_class_type(entry->type_information)))
        return instantiate_template_class_if_possible(entry, decl_context, locus);
    else if (symbol_entity_specs_get_is_member(entry)
            && symbol_entity_specs_get_emission_template(entry) != NULL)
        return instantiate_nontemplate_member_class_if_possible(entry, decl_context, locus);

    return 1;
}

extern inline char is_enum_type(type_t* t)
{
    return is_unnamed_enumerated_type(t)
        || is_named_enumerated_type(t);
}

extern inline char is_unscoped_enum_type(type_t* t)
{
    if (!is_enum_type(t))
        return 0;

    t = get_actual_enum_type(t);

    simple_type_t* enum_type = t->type;

    return !enum_type->enum_info->is_scoped;
}

extern inline char is_scoped_enum_type(type_t* t)
{
    if (!is_enum_type(t))
        return 0;

    t = get_actual_enum_type(t);

    simple_type_t* enum_type = t->type;

    return enum_type->enum_info->is_scoped;
}

extern inline char is_unnamed_enumerated_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_ENUM);
}

extern inline char is_named_enumerated_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (is_named_type(t)
            && is_unnamed_enumerated_type(named_type_get_symbol(t)->type_information));
}

extern inline type_t* get_actual_enum_type(type_t* t)
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

extern inline scope_entry_t* enum_type_get_enumerator_num(type_t* t, int n)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enum type", 0);

    t = get_actual_enum_type(t);

    simple_type_t* enum_type = t->type;
    return enum_type->enum_info->enumeration_list[n];
}

extern inline int enum_type_get_num_enumerators(type_t* t)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enum type", 0);
    t = get_actual_enum_type(t);

    simple_type_t* enum_type = t->type;

    return enum_type->enum_info->num_enumeration;
}

extern inline type_t* enum_type_get_underlying_type(type_t* t)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enum type", 0);

    t = get_actual_enum_type(t);
    simple_type_t* enum_type = t->type;

    return enum_type->enum_info->underlying_type;
}

// This function is used only for conversions of enum types without fixed
// underlying types
static type_t* enum_type_get_underlying_type_for_conversion(type_t* t)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enum type", 0);

    // If the enum type has a fixed underlying type, its
    // underlying_type_for_conversion is its underlying type
    if (enum_type_get_underlying_type_is_fixed(t))
        return enum_type_get_underlying_type(t);

    // Not an enum with fixed underlying type, we may have to compute the type
    // used in conversions
    t = get_actual_enum_type(t);
    simple_type_t* enum_type = t->type;

    if (enum_type->enum_info->underlying_type_for_conversion != NULL)
        return enum_type->enum_info->underlying_type_for_conversion;

    type_t* checked_types[] =
    {
        get_signed_int_type(),
        get_unsigned_int_type(),

        get_signed_long_int_type(),
        get_unsigned_long_int_type(),

        get_signed_long_long_int_type(),
        get_unsigned_long_long_int_type(),

#ifdef HAVE_INT128
        get_signed_int128_type(),
        get_unsigned_int128_type(),
#endif
        // Sentinel
        NULL
    };

    int i, N = enum_type_get_num_enumerators(t);

    if (N == 0)
        return get_signed_int_type();

    int j;

#define B_(x) const_value_is_nonzero(x)

    for (j = 0; checked_types[j] != NULL; j++)
    {
        char all_fit = 1;
        for (i = 0; i < N && all_fit; i++)
        {
            scope_entry_t* enumerator = enum_type_get_enumerator_num(t, i);

            const_value_t* enumerator_value = nodecl_get_constant(enumerator->value);
            if (enumerator_value == NULL) // This should not happen
                continue;

            all_fit = (B_(const_value_lte(integer_type_get_minimum(checked_types[j]), enumerator_value))
                    && B_(const_value_lte(enumerator_value, integer_type_get_maximum(checked_types[j]))));
        }

        if (all_fit)
        {
            enum_type->enum_info->underlying_type_for_conversion = checked_types[j];
            return checked_types[j];
        }
    }


#undef B_
    internal_error("Failure to come up with a integer for conversion of type '%s'\n",
            print_declarator(t));
}

extern inline void enum_type_set_underlying_type(type_t* t, type_t* underlying_type)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enum type", 0);

    t = get_actual_enum_type(t);
    simple_type_t* enum_type = t->type;

    enum_type->enum_info->underlying_type = underlying_type;
}

extern inline char enum_type_get_underlying_type_is_fixed(type_t* t)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enum type", 0);

    t = get_actual_enum_type(t);
    simple_type_t* enum_type = t->type;

    return enum_type->enum_info->underlying_type_is_fixed;
}

extern inline void enum_type_set_underlying_type_is_fixed(type_t* t, char is_fixed)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enum type", 0);

    t = get_actual_enum_type(t);
    simple_type_t* enum_type = t->type;

    enum_type->enum_info->underlying_type_is_fixed = is_fixed;
}

static inline type_t* advance_over_typedefs_with_cv_qualif_(type_t* t,
        cv_qualifier_t* cv_qualif,
        char *is_cacheable)
{
    if (t == NULL)
        return NULL;

    char is_cacheable_result = 1;

    type_t* result = t;
    cv_qualifier_t cv_qualifier_result = result->cv_qualifier;

    // Advance over typedefs
    while (result->kind == TK_DIRECT
            && result->type->kind == STK_INDIRECT
            && result->type->user_defined_type != NULL
            && result->type->is_indirect)
    {
        is_cacheable_result &= !(result->type->is_mutable);
        result = result->type->user_defined_type->type_information;
        cv_qualifier_result |= result->cv_qualifier;
    }

    // Arrays add the element qualification
    //
    // Note: DO NOT use is_array because it uses advance_over_typedefs which
    // ends using advance_over_typedefs_with_cv_qualif
    if (result->kind == TK_ARRAY)
    {
        advance_over_typedefs_with_cv_qualif(result->array->element_type, &cv_qualifier_result);
    }

    if (cv_qualif != NULL)
    {
        *cv_qualif |= cv_qualifier_result;
    }
    *is_cacheable = is_cacheable_result;

    return result;
}

extern inline type_t* advance_over_typedefs_with_cv_qualif(type_t* t, cv_qualifier_t* cv_qualif)
{
    char is_cacheable;
    return advance_over_typedefs_with_cv_qualif_(t, cv_qualif, &is_cacheable);
}

extern inline type_t* advance_over_typedefs(type_t* t)
{
    if (t == NULL)
        return NULL;

    if (t->kind != TK_DIRECT
            || t->type->kind != STK_INDIRECT
            || !t->type->is_indirect)
    {
        // We do not cache these and simply early return
        return t;
    }
    else
    {
        // t->kind == TK_DIRECT
        // && t->type->kind == STK_INDIRECT
        // && t->type->is_indirect
        if (t->_advanced_type == NULL)
        {
            char is_cacheable;
            cv_qualifier_t cv = CV_NONE;
            type_t* advanced_unqualif = advance_over_typedefs_with_cv_qualif_(t,
                    &cv,
                    &is_cacheable);

            type_t* result;
            if (cv != CV_NONE)
                result = get_cv_qualified_type(advanced_unqualif, cv);
            else
                result = advanced_unqualif;

            if (is_cacheable)
            {
                t->_advanced_type = result;
            }

            return result;
        }
        else
        {
            return t->_advanced_type;
        }

    }
    internal_error("Code unreachable", 0);
}

extern inline char function_type_get_lacking_prototype(type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    function_type = advance_over_typedefs(function_type);

    return function_type->function->lacks_prototype;
}

extern inline char function_type_get_has_ellipsis(type_t* function_type)
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

static type_t* function_type_replace_return_type_(type_t* t, type_t* new_return,
        type_t* (new_function_type)(type_t*, parameter_info_t*, int, ref_qualifier_t))
{
    if (function_type_get_lacking_prototype(t))
    {
        return get_nonproto_function_type(new_return,
                function_type_get_num_parameters(t));
    }

    ERROR_CONDITION(!is_function_type(t), "Invalid function type", 0);

    int num_parameters = function_type_get_num_parameters(t);

    parameter_info_t param_info[num_parameters+1];
    memset(param_info, 0, sizeof(param_info));

    cv_qualifier_t cv_qualifier = get_cv_qualifier(t);

    char has_ellipsis = function_type_get_has_ellipsis(t);
    ref_qualifier_t ref_qualifier = function_type_get_ref_qualifier(t);

    int real_parameters = num_parameters;
    if (has_ellipsis)
        real_parameters--;

    int i;
    for (i = 0; i < real_parameters; i++)
    {
        param_info[i].nonadjusted_type_info = function_type_get_nonadjusted_parameter_type_num(t, i);
        param_info[i].type_info = function_type_get_parameter_type_num(t, i);
    }

    if (has_ellipsis)
    {
        param_info[num_parameters - 1].is_ellipsis = 1;
        param_info[num_parameters - 1].type_info = get_ellipsis_type();
    }

    type_t* result = new_function_type(new_return, param_info, num_parameters, ref_qualifier);
    result = get_cv_qualified_type(result, cv_qualifier);

    return result;
}

extern inline type_t* function_type_replace_return_type(type_t* t, type_t* new_return)
{
    return function_type_replace_return_type_(t, new_return, get_new_function_type);
}

extern inline type_t* function_type_replace_return_type_with_trailing_return(type_t* t, type_t* new_return)
{
    return function_type_replace_return_type_(t, new_return, get_new_function_type_trailing_type);
}

extern inline void class_type_add_base_class(type_t* class_type, scope_entry_t* base_class, 
        char is_virtual, char is_dependent, char is_expansion,
        access_specifier_t access_specifier)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    ERROR_CONDITION(is_expansion && !is_dependent, "An expansion base class should always be dependent", 0);
    class_type = get_actual_class_type(class_type);

    if (symbol_entity_specs_get_is_injected_class_name(base_class))
        base_class = named_type_get_symbol(symbol_entity_specs_get_class_type(base_class));

    base_class_info_t* new_base_class = NEW0(base_class_info_t);
    new_base_class->class_symbol = base_class;
    /* redundant */ new_base_class->class_type = base_class->type_information;
    new_base_class->is_virtual = is_virtual;
    new_base_class->is_dependent = is_dependent;
    new_base_class->is_expansion = is_expansion;
    new_base_class->access_specifier = access_specifier;

    class_info_t* class_info = class_type->type->class_info;
    // Only add once
    P_LIST_ADD_ONCE(class_info->base_classes_list, class_info->num_bases, new_base_class);
}

void class_type_set_inner_context(type_t* class_type, const decl_context_t* decl_context)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);

    class_type->type->class_info->inner_decl_context = decl_context;
}

extern inline const decl_context_t* class_type_get_inner_context(type_t* class_type)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);

    return class_type->type->class_info->inner_decl_context;
}

extern inline const decl_context_t* class_or_enum_type_get_inner_context(type_t* class_or_enum_type)
{
    if (is_class_type(class_or_enum_type))
        return class_type_get_inner_context(class_or_enum_type);
    else if (is_enum_type(class_or_enum_type))
        return enum_type_get_context(class_or_enum_type);

    internal_error("This is not a class or enum type", 0);
}

extern inline scope_entry_t* class_type_get_base_num(type_t* class_type, int num,
        char *is_virtual, char *is_dependent, char *is_expansion, access_specifier_t* access_specifier)
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

    if (is_expansion != NULL)
    {
        *is_expansion = class_info->base_classes_list[num]->is_expansion;
    }

    if (access_specifier != NULL)
    {
        *access_specifier = class_info->base_classes_list[num]->access_specifier;
    }

    return class_info->base_classes_list[num]->class_symbol;
}

extern inline _size_t class_type_get_offset_direct_base(type_t* class_type, scope_entry_t* direct_base)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);

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

extern inline void class_type_set_offset_direct_base(type_t* class_type, scope_entry_t* direct_base, _size_t base_offset)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);

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

extern inline scope_entry_list_t* class_type_get_all_conversions(type_t* class_type, const decl_context_t* decl_context)
{
    ERROR_CONDITION(!is_class_type(class_type), "This is not a class type", 0);
    class_type = get_actual_class_type(class_type);

    // For every base class, get its conversions
    int i;
    int num_bases = class_type_get_num_bases(class_type);
    scope_entry_list_t* base_result = NULL;
    for (i = 0; i < num_bases; i++)
    {
        char is_dependent = 0;
        scope_entry_t* class_entry = class_type_get_base_num(class_type, i, 
                /* is_virtual = */ NULL,
                /* is_dependent */ &is_dependent,
                /* is_expansion */ NULL,
                /* access_specifier */ NULL);
        type_t* base_class_type = class_entry->type_information;

        if (is_dependent)
            continue;

        scope_entry_list_t* base_conversors = class_type_get_all_conversions(base_class_type, decl_context);

        base_result = entry_list_merge(base_result, base_conversors);
    }

    // Now for every conversor of this class, remove it from 'result'
    scope_entry_list_t* this_class_conversors = class_type_get_conversions(class_type);

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

/*
 * States if two types are equivalent. This means that they are the same
 * (ignoring typedefs). Just plain comparison, no standard conversion is
 * performed. cv-qualifiers are relevant for comparison
 */
extern inline char equivalent_types(type_t* t1, type_t* t2)
{
    ERROR_CONDITION( (t1 == NULL || t2 == NULL), "No type can be null here", 0);

    cv_qualifier_t cv_qualifier_t1 = CV_NONE, cv_qualifier_t2 = CV_NONE;

    // Advance over typedefs
    t1 = advance_over_typedefs_with_cv_qualif(t1, &cv_qualifier_t1);
    t2 = advance_over_typedefs_with_cv_qualif(t2, &cv_qualifier_t2);

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
        case TK_RVALUE_REFERENCE :
        case TK_REBINDABLE_REFERENCE :
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
        case TK_PACK:
            result = equivalent_pack_types(t1, t2);
            break;
        case TK_SEQUENCE:
            result = equivalent_sequence_types(t1, t2);
            break;
        case TK_BRACED_LIST:
            result = equivalent_braced_types(t1, t2);
            break;
        case TK_AUTO:
        case TK_DECLTYPE_AUTO:
        case TK_ERROR:
            // This is always true
            result = 1;
            break;
        case TK_OVERLOAD:
            // These are always false
            result = 0;
            break;
        default :
            internal_error("Unknown type kind (%d)\n", t1->kind);
    }

    result &= equivalent_cv_qualification(cv_qualifier_t1, cv_qualifier_t2);

    return result;
}

static char equivalent_named_types(scope_entry_t* s1, scope_entry_t* s2)
{
    if (symbol_entity_specs_get_is_template_parameter(s1)
            || symbol_entity_specs_get_is_template_parameter(s2))
    {
        if (symbol_entity_specs_get_is_template_parameter(s1)
                && symbol_entity_specs_get_is_template_parameter(s2))
        {
            return ((s1->kind == s2->kind)
                    && (symbol_entity_specs_get_template_parameter_nesting(s1) == symbol_entity_specs_get_template_parameter_nesting(s2))
                    && (symbol_entity_specs_get_template_parameter_position(s1) == symbol_entity_specs_get_template_parameter_position(s2)));
        }
        else
        {
            return 0;
        }
    }
    else
    {
       // Consider this case
       //
       // MODULE M1
       //   TYPE T             ! This is M1.T
       //     INTEGER :: X
       //   END TYPE T
       // END MODULE M1
       //
       // MODULE M2
       //   USE M1             ! Introduces M2.T as an alias to M1.T
       //
       //  CONTAINS
       //    SUBROUTINE S(X)
       //     TYPE(T) :: X     ! X is of type M2.T (not M1.T)
       //    END
       // END
       //
       // MODULE M3
       //   USE M1             ! Introduces M3.T as an alias to M1.T
       //   USE M2             ! Introduces M3.S as an alias to M2.S
       //                      ! (M2.T is not introduced as it would be repeat an existing alias to M1.T)
       // CONTAINS
       //   SUBROUTINE S2
       //     TYPE(T) :: X1     ! X1 is of type M3.T (not M2.T or M1.T)
       //
       //     CALL S(X1)        !<-- We need to realize that M3.T and M2.T are both aliases of M1.T
       //   END
       // END
       s1 = fortran_get_ultimate_symbol(s1);
       s2 = fortran_get_ultimate_symbol(s2);

       if (s1 == s2)
          return 1;

       return equivalent_types(s1->type_information, s2->type_information);
    }
}

static char same_template_type(type_t* t1, type_t* t2)
{
    ERROR_CONDITION(!is_template_type(t1) || !is_template_type(t2), "Invalid type", 0);

    if (t1 == t2)
        return 1;

    scope_entry_t* s1 = template_type_get_related_symbol(t1);
    scope_entry_t* s2 = template_type_get_related_symbol(t2);

    ERROR_CONDITION(s1 == NULL || s2 == NULL,
            "Template type lacks a related symbol", 0);

    if ((s1->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                && s2->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            || (s1->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK
                && s2->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK))
    {
        ERROR_CONDITION(!symbol_entity_specs_get_is_template_parameter(s1)
                || !symbol_entity_specs_get_is_template_parameter(s2),
                "Symbol is not set as a template parameter", 0);

        return (symbol_entity_specs_get_template_parameter_nesting(s1) == symbol_entity_specs_get_template_parameter_nesting(s2))
            && (symbol_entity_specs_get_template_parameter_position(s1) == symbol_entity_specs_get_template_parameter_position(s2));
    }

    return 0;
}

static char equivalent_simple_types(type_t *p_t1, type_t *p_t2)
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
            result = equivalent_builtin_type(p_t1, p_t2);
            break;
        case STK_CLASS :
            {
                result = (t1 == t2);

                // This is needed only for specializations built on top of template-template parameters
                if (!result
                        && p_t1->info->is_template_specialized_type
                        && p_t2->info->is_template_specialized_type
                        && same_template_type(p_t1->related_template_type, p_t2->related_template_type))
                {
                    template_parameter_list_t* tpl1 = template_specialized_type_get_template_arguments(p_t1);
                    template_parameter_list_t* tpl2 = template_specialized_type_get_template_arguments(p_t2);
                    result = same_template_argument_list(tpl1, tpl2);
                }
                break;
            }
        case STK_TEMPLATE_TYPE :
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
            C_LANGUAGE()
            {
                result = (t1 == t2);
            }
            CXX_LANGUAGE()
            {
                result = same_functional_expression(
                        t1->typeof_expr,
                        t2->typeof_expr);
            }
            break;
        case STK_UNDERLYING:
            {
                result = equivalent_types(
                        t1->underlying_type,
                        t2->underlying_type);
            }
            break;
        case STK_VA_LIST :
            // If both are __builtin_va_list, this is trivially true
            result = 1;
            break;
        case STK_COMPLEX:
            return equivalent_types(t1->complex_element, t2->complex_element);
            break;
        case STK_VECTOR:
            return equivalent_vector_type(p_t1, p_t2);
        case STK_MASK:
            return mask_type_get_num_bits(p_t1) == mask_type_get_num_bits(p_t2);
        case STK_TYPE_DEP_EXPR:
            // Always different
            return 0;
            break;
        default :
            internal_error("Unknown simple type kind (%d)", t1->kind);
            return 0;
    }

    return result;
}

static inline char equivalent_builtin_type(type_t* p_t1, type_t *p_t2)
{
    simple_type_t* t1 = p_t1->type;
    simple_type_t* t2 = p_t2->type;

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
            || t1->builtin_type == BT_BYTE
            || t1->builtin_type == BT_CHAR)
    {
        if (t1->is_unsigned != t2->is_unsigned)
            return 0;
    }
    
    // signed
    if (t1->builtin_type == BT_INT
            || t1->builtin_type == BT_BYTE
            || t1->builtin_type == BT_CHAR)
    {
        if (t1->is_signed != t2->is_signed)
            return 0;
    }

    // bool may be different in their sizes
    if (t1->builtin_type == BT_BOOL
            && t2->builtin_type == BT_BOOL)
    {
        if (p_t1->info->size != p_t2->info->size)
            return 0;
    }

    if (t1->builtin_type == BT_CHAR16_T
            && t2->builtin_type == BT_CHAR16_T)
        return 1;

    if (t1->builtin_type == BT_CHAR32_T
            && t2->builtin_type == BT_CHAR32_T)
        return 1;

    // Ok, nothing makes us think they might be different
    return 1;
}

static char equivalent_pointer_to_member_type(type_t* t1, type_t* t2)
{
    return equivalent_pointer_type(t1->pointer, 
            t2->pointer)
        && equivalent_types(t1->pointer->pointee_class_type, 
                t2->pointer->pointee_class_type);
}

static char equivalent_pointer_type(pointer_info_t* t1, pointer_info_t* t2)
{
    return equivalent_types(t1->pointee, t2->pointee);
}

static char equivalent_array_type(array_info_t* t1, array_info_t* t2)
{
    if (!equivalent_types(t1->element_type, t2->element_type))
        return 0;

    if (!nodecl_is_null(t1->whole_size)
            && !nodecl_is_null(t2->whole_size))
    {
        CXX_LANGUAGE()
        {
            if (!same_functional_expression(
                        t1->whole_size,
                        t2->whole_size))
                return 0;
        }
        C_LANGUAGE()
        {
            if (nodecl_is_constant(t1->whole_size)
                    && nodecl_is_constant(t2->whole_size))
            {
                if(const_value_is_zero(
                            const_value_eq(
                                nodecl_get_constant(t1->whole_size), 
                                nodecl_get_constant(t2->whole_size))))
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
        if (nodecl_is_null(t1->whole_size) 
                != nodecl_is_null(t2->whole_size))
        {
            return 0;
        }
    }
    
    return 1;
}

static char equivalent_vector_type(type_t* t1, type_t* t2)
{
    // This mimics gcc behaviour
    return ((equivalent_types(t1->type->vector_element, t2->type->vector_element)))
        && (t1->type->vector_size == t2->type->vector_size);
}

static char equivalent_function_types_may_differ_ref_qualifier_(type_t* ft1, type_t* ft2)
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

    if (t1->ref_qualifier != t2->ref_qualifier)
        return 0;

    if (!equivalent_cv_qualification(ft1->cv_qualifier, ft2->cv_qualifier))
        return 0;

    return 1;
}

static char equivalent_function_type(type_t* ft1, type_t* ft2)
{
    if (!equivalent_function_types_may_differ_ref_qualifier_(ft1, ft2))
        return 0;

    function_info_t* t1 = ft1->function;
    function_info_t* t2 = ft2->function;

    if (t1->ref_qualifier != t2->ref_qualifier)
        return 0;

    return 1;
}

extern inline char equivalent_function_types_may_differ_ref_qualifier(type_t* ft1, type_t* ft2)
{
    ERROR_CONDITION(!is_function_type(ft1) || !is_function_type(ft2), "Invalid types", 0);

    ft1 = advance_over_typedefs(ft1);
    ft2 = advance_over_typedefs(ft2);

    return equivalent_function_types_may_differ_ref_qualifier_(ft1, ft2);
}


static inline char equivalent_pack_types(type_t* t1, type_t *t2)
{
    return equivalent_types(t1->pack_type->packed,
            t2->pack_type->packed);
}

static inline char equivalent_sequence_types(type_t* t1, type_t *t2)
{
    if (t1->sequence_type->num_types != t2->sequence_type->num_types)
        return 0;

    int i;
    for (i = 0; i < t1->sequence_type->num_types; i++)
    {
        if (!equivalent_types(t1->sequence_type->types[i],
                    t2->sequence_type->types[i]))
            return 0;
    }

    return 1;
}

static inline char equivalent_braced_types(type_t* t1, type_t *t2)
{
    if (t1->braced_type->num_types != t2->braced_type->num_types)
        return 0;

    int i;
    for (i = 0; i < t1->braced_type->num_types; i++)
    {
        if (!equivalent_types(
                    t1->braced_type->types[i],
                    t2->braced_type->types[i]))
            return 0;
    }

    return 1;
}

extern inline char equivalent_cv_qualification(cv_qualifier_t cv1, cv_qualifier_t cv2)
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

        // Remove top level qualification. Note that we do not use
        // get_unqualified_type as it preserves __restrict
        type_t* par1 = get_cv_qualified_type(t1->parameter_list[i]->type_info, CV_NONE);
        type_t* par2 = get_cv_qualified_type(t2->parameter_list[i]->type_info, CV_NONE);

        if (!equivalent_types(par1, par2))
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
             *    'void f(int k(bool))' is compatible with 'void g(int (*t)(bool))'
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

static const char* get_template_parameters_list_str(template_parameter_list_t* template_parameters);

static type_t* rebuild_advanced_dependent_typename(
        type_t* original_type,
        scope_entry_t* advanced_member,
        int nested_name_index,
        int nested_name_size,
        nodecl_t* nodecl_nested_name)
{
    // We can reuse the original type because nothing was advanced actually
    if (nested_name_index == 0)
        return original_type;

    nodecl_t nodecl_list = nodecl_null();

    int i;
    for (i = nested_name_index; i < nested_name_size; i++)
    {
        nodecl_list = nodecl_append_to_list(nodecl_list, nodecl_nested_name[i]);
    }

    nodecl_t nodecl_parts = nodecl_make_cxx_dep_name_nested(nodecl_list, make_locus("", 0, 0));

    return get_dependent_typename_type_from_parts(advanced_member, nodecl_parts);
}

// This function advances just one dependent component
// [Class symbol]::Foo::Bar becomes [Foo symbol]::Bar
static type_t* advance_dependent_typename_aux(
        type_t* original_type,
        type_t* dependent_entry_type,
        nodecl_t dependent_parts)
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

    if (nodecl_is_null(dependent_parts))
    {
        return original_type;
    }

    ERROR_CONDITION(nodecl_get_kind(dependent_parts) != NODECL_CXX_DEP_NAME_NESTED, "Invalid nested parts", 0);

    scope_entry_t* current_member = dependent_entry;

    const decl_context_t* class_context;

    nodecl_t nodecl_nested_parts = nodecl_get_child(dependent_parts, 0);

    int num_items = 0;
    nodecl_t* dep_parts = nodecl_unpack_list(nodecl_nested_parts, &num_items);

    if (num_items == 1)
    {
        nodecl_t nodecl_unqualified_name = nodecl_shallow_copy(dep_parts[0]);
        // Last part
        ERROR_CONDITION(nodecl_get_kind(nodecl_unqualified_name) != NODECL_CXX_DEP_NAME_SIMPLE
                && nodecl_get_kind(nodecl_unqualified_name) != NODECL_CXX_DEP_TEMPLATE_ID,
                "Invalid unqualified name", 0);

        nodecl_t nodecl_simple_name = nodecl_unqualified_name;
        template_parameter_list_t* template_parameters = NULL;
        if (nodecl_get_kind(nodecl_unqualified_name) == NODECL_CXX_DEP_TEMPLATE_ID)
        {
            nodecl_simple_name = nodecl_get_child(nodecl_unqualified_name, 0);
            template_parameters = nodecl_get_template_parameters(nodecl_unqualified_name);
        }
        ERROR_CONDITION(nodecl_get_kind(nodecl_simple_name) != NODECL_CXX_DEP_NAME_SIMPLE,
                "Invalid nested part", 0);
        const char* name = nodecl_get_text(nodecl_simple_name);

        class_context = class_type_get_inner_context(current_member->type_information);

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Looking for last dependent-part '%s'\n", name);
        }

        scope_entry_list_t* member_list = query_nodecl_name_in_class(
                class_context,  // unused
                current_member,
                nodecl_simple_name,
                NULL);

        if (member_list == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Nothing was found for dependent-part '%s'\n", name);
            }
            return get_dependent_typename_type_from_parts(current_member, 
                    nodecl_make_cxx_dep_name_nested(nodecl_make_list_1(nodecl_unqualified_name), 
                        nodecl_get_locus(nodecl_unqualified_name)));
        }

        if (entry_list_size(member_list) > 1)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Too many symbols where found for '%s'\n", name);
            }
            return get_dependent_typename_type_from_parts(current_member, 
                    nodecl_make_cxx_dep_name_nested(nodecl_make_list_1(nodecl_unqualified_name), 
                        nodecl_get_locus(nodecl_unqualified_name)));
        }

        scope_entry_t* member = entry_list_head(member_list);

        if (member->kind == SK_CLASS
                || member->kind == SK_ENUM
                || member->kind == SK_TYPEDEF)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Got a typename when looking up dependent-part '%s'\n", name);
            }
            if (template_parameters != NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: But this part has template arguments, so it is not valid\n");
                }
                return get_dependent_typename_type_from_parts(current_member, 
                        nodecl_make_cxx_dep_name_nested(nodecl_make_list_1(nodecl_unqualified_name), 
                            nodecl_get_locus(nodecl_unqualified_name)));
            }

            if (member->kind == SK_TYPEDEF)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: Got a typedef when looking up dependent-part '%s'\n", name);
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
                        name);
            }

            if (template_parameters == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: But this part does not have template arguments, so it is not valid\n");
                }
                return get_dependent_typename_type_from_parts(current_member, 
                        nodecl_make_cxx_dep_name_nested(nodecl_make_list_1(nodecl_unqualified_name), 
                            nodecl_get_locus(nodecl_unqualified_name)));
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
                        nodecl_make_cxx_dep_name_nested(nodecl_make_list_1(nodecl_unqualified_name), 
                            nodecl_get_locus(nodecl_unqualified_name)));
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: requesting specialization '%s'\n", 
                        name);
            }

            type_t* specialized_type = template_type_get_specialized_type(
                    template_type,
                    template_parameters, // Should this be properly nested?
                    class_context, 
                    // They should not be needed
                    make_locus("", 0, 0));

            current_member = named_type_get_symbol(specialized_type);

        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Unexpected symbol for part '%s' (%s)\n", name, symbol_kind_name(member));
            }
            return get_dependent_typename_type_from_parts(current_member, 
                    nodecl_make_cxx_dep_name_nested(nodecl_make_list_1(nodecl_unqualified_name), 
                        nodecl_get_locus(nodecl_unqualified_name)));
        }

        // This looks a bit twisted
        type_t* result_type = 
            advance_over_typedefs(get_user_defined_type(current_member));

        return result_type;
    }
    else if (num_items > 1)
    {
        // Advance just one and return another dependent typename
        nodecl_t current_item = dep_parts[0];

        ERROR_CONDITION(nodecl_get_kind(current_item) != NODECL_CXX_DEP_NAME_SIMPLE
                && nodecl_get_kind(current_item) != NODECL_CXX_DEP_TEMPLATE_ID,
                "Invalid nested part", 0);

        nodecl_t nodecl_simple_name = current_item;
        template_parameter_list_t* template_parameters = NULL;
        if (nodecl_get_kind(current_item) == NODECL_CXX_DEP_TEMPLATE_ID)
        {
            nodecl_simple_name = nodecl_get_child(current_item, 0);
            template_parameters = nodecl_get_template_parameters(current_item);
        }
        ERROR_CONDITION(nodecl_get_kind(nodecl_simple_name) != NODECL_CXX_DEP_NAME_SIMPLE,
                "Invalid nested part", 0);
        const char* name = nodecl_get_text(nodecl_simple_name);

        class_context = class_type_get_inner_context(current_member->type_information);

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Looking for dependent-part '%s'\n", name);
        }

        scope_entry_list_t* member_list = query_nodecl_name_in_class(
                class_context,  // unused
                current_member,
                nodecl_simple_name,
                NULL);

        if (member_list == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Nothing was found for dependent-part '%s'\n", name);
            }

            type_t* result = rebuild_advanced_dependent_typename(original_type, 
                    current_member, 
                    0, num_items,
                    dep_parts);
            DELETE(dep_parts);
            return result;
        }

        if (entry_list_size(member_list) > 1)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Too many symbols where found for '%s'\n", name);
            }

            type_t* result = rebuild_advanced_dependent_typename(original_type, 
                    current_member, 
                    0, num_items,
                    dep_parts);
            DELETE(dep_parts);
            return result;
        }

        scope_entry_t* member = entry_list_head(member_list);

        if (member->kind == SK_TYPEDEF)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Got a typedef when looking up dependent-part '%s'\n", name);
            }
            type_t* advanced_type = advance_over_typedefs(member->type_information);

            if (is_dependent_typename_type(advanced_type))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: Got a dependent typename when looking up dependent-part '%s' '%s'\n",
                            name,
                            print_declarator(advanced_type));
                }
                scope_entry_t* new_dependent_entry = NULL;
                nodecl_t nodecl_dependent_parts = nodecl_null();

                dependent_typename_get_components(advanced_type, &new_dependent_entry, &nodecl_dependent_parts);

                nodecl_t nodecl_new_parts = nodecl_shallow_copy(nodecl_get_child(nodecl_dependent_parts, 0));

                int j;
                for (j = 1; j < num_items; j++)
                {
                    nodecl_new_parts = nodecl_append_to_list(nodecl_new_parts,
                            nodecl_shallow_copy(dep_parts[j]));
                }

                DELETE(dep_parts);

                type_t* result = get_dependent_typename_type_from_parts(
                        new_dependent_entry,
                        nodecl_make_cxx_dep_name_nested(nodecl_new_parts, NULL));
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: Advanced to '%s'\n",
                            print_declarator(result));
                }
                return result;
            }

            if (is_named_class_type(advanced_type))
            {
                member = named_type_get_symbol(advanced_type);
            }
        }

        if (member->kind == SK_CLASS)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Got a class when looking up dependent-part '%s'\n", name);
            }
            if (template_parameters != NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: But this part has template arguments, so it is not valid\n");
                }
                type_t* result = rebuild_advanced_dependent_typename(original_type, 
                        current_member, 
                        0, num_items,
                        dep_parts);
                DELETE(dep_parts);
                return result;
            }

            current_member = member;
        }
        else if (member->kind == SK_TEMPLATE)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Got a template-name when looking up dependent-part '%s'\n", 
                        name);
            }

            if (template_parameters == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: But this part does not have template arguments, so it is not valid\n");
                }

                type_t* result = rebuild_advanced_dependent_typename(original_type, 
                        current_member, 
                        0, num_items,
                        dep_parts);
                DELETE(dep_parts);
                return result;
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

                type_t* result = rebuild_advanced_dependent_typename(original_type, 
                        current_member, 
                        0, num_items,
                        dep_parts);
                DELETE(dep_parts);
                return result;
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Requesting specialization '%s'\n", 
                        name);
            }

            type_t* specialized_type = template_type_get_specialized_type(
                    template_type,
                    template_parameters,
                    class_context, 
                    // They should not be needed
                    make_locus("", 0, 0));

            if (specialized_type == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: Somehow when requesting a specialization nothing was returned");
                }

                type_t* result = rebuild_advanced_dependent_typename(original_type, 
                        current_member, 
                        0, num_items,
                        dep_parts);
                DELETE(dep_parts);
                return result;
            }

            current_member = named_type_get_symbol(specialized_type);
        }
        else 
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Unexpected symbol for part '%s' (%s)\n", name, symbol_kind_name(member));
            }

            type_t* result = rebuild_advanced_dependent_typename(original_type, 
                    current_member, 
                    0, num_items,
                    dep_parts);
            DELETE(dep_parts);
            return result;
        }

        type_t* result = rebuild_advanced_dependent_typename(
                original_type, 
                current_member, 
                1, num_items,
                dep_parts);
        DELETE(dep_parts);
        return result;
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

static type_t* advance_dependent_typename_if_in_context(type_t* t, const decl_context_t* decl_context)
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
    nodecl_t nodecl_dependent_parts = nodecl_null();

    dependent_typename_get_components(t, &dependent_entry, &nodecl_dependent_parts);

    // Only advance if the entry symbol is a class in the current lexical scope
    if (dependent_entry->kind != SK_CLASS
            || !class_is_in_lexical_scope(decl_context, dependent_entry))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Refusing to advance the dependent typename because this is not a class "
                    "or not in lexical scope\n");
        }

        return t;
    }

    type_t* dependent_entry_type = get_user_defined_type(dependent_entry);

    type_t* result = advance_dependent_typename_aux(t, dependent_entry_type, nodecl_dependent_parts);
    result = get_qualified_type(result, cv_qualif, /* qualify_arrays */ 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: Type was advanced to '%s'\n",
                print_declarator(result));
    }

    if (is_dependent_typename_type(result))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Type is a dependent typename, "
                    "we may have to advance it again\n");
        }

        scope_entry_t* new_dependent_entry = NULL;
        dependent_typename_get_components(result, &new_dependent_entry, &nodecl_dependent_parts);

        // Sanity check
        if (dependent_entry->kind == SK_CLASS)
            dependent_entry = class_symbol_get_canonical_symbol(dependent_entry);
        if (new_dependent_entry->kind == SK_CLASS)
            new_dependent_entry = class_symbol_get_canonical_symbol(new_dependent_entry);

        if (new_dependent_entry != dependent_entry)
        {
            return advance_dependent_typename_if_in_context(result, decl_context);
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: But the new dependent typename shares the dependent entry symbol, "
                        "refusing to advance it again\n");
            }
        }
    }

    return result;
}

static inline char type_contains_a_dependent_typename(type_t* t)
{
    if (t == NULL)
        return 0;

    if (is_dependent_typename_type(t))
    {
        return 1;
    }
    else if (is_named_type(t)
            && is_template_specialized_type(named_type_get_symbol(t)->type_information))
    {
        t = named_type_get_symbol(t)->type_information;

        template_parameter_list_t* tpl = template_specialized_type_get_template_arguments(t);

        int i;
        for (i = 0; i < tpl->num_parameters; i++)
        {
            if (type_contains_a_dependent_typename(tpl->arguments[i]->type))
                return 1;
        }
    }
    else if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        return type_contains_a_dependent_typename(no_ref(t));
    }
    else if (is_pointer_type(t))
    {
        return type_contains_a_dependent_typename(pointer_type_get_pointee_type(t));
    }
    else if (is_array_type(t))
    {
        return type_contains_a_dependent_typename(array_type_get_element_type(t));
    }
    else if (is_pointer_to_member_type(t))
    {
        return type_contains_a_dependent_typename(pointer_to_member_type_get_class_type(t))
            || type_contains_a_dependent_typename(pointer_type_get_pointee_type(t));
    }
    else if (is_pack_type(t))
    {
        return type_contains_a_dependent_typename(pack_type_get_packed_type(t));
    }
    else if (is_function_type(t))
    {
        type_t* return_type = function_type_get_return_type(t);
        if (type_contains_a_dependent_typename(return_type))
            return 1;

        int num_types = function_type_get_num_parameters(t);

        char has_ellipsis = function_type_get_has_ellipsis(t);
        if (has_ellipsis)
            num_types--;

        int i;
        for (i = 0; i < num_types; i++)
        {
            type_t* param_type = function_type_get_parameter_type_num(t, i);

            if (type_contains_a_dependent_typename(param_type))
                return 1;
        }
    }

    return 0;
}

static type_t* rebuild_type_advancing_dependent_typenames(type_t* t,
        const decl_context_t* decl_context,
        const locus_t* locus);

static template_parameter_list_t* rebuild_template_arguments_advancing_dependent_typenames(
        template_parameter_list_t* tpl,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    template_parameter_list_t* fixed_tpl = duplicate_template_argument_list(tpl);

    int i;
    for (i = 0; i < fixed_tpl->num_parameters; i++)
    {
        template_parameter_value_t* new_value = NEW0(template_parameter_value_t);
        *new_value = *fixed_tpl->arguments[i];
        new_value->value = nodecl_shallow_copy(fixed_tpl->arguments[i]->value);

        new_value->type = rebuild_type_advancing_dependent_typenames(
                fixed_tpl->arguments[i]->type,
                decl_context,
                locus);

        fixed_tpl->arguments[i] = new_value;
    }

    return fixed_tpl;
}

static type_t* rebuild_type_advancing_dependent_typenames(type_t* t,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    if (t == NULL)
        return NULL;

    cv_qualifier_t cv_qualif_orig = CV_NONE;
    advance_over_typedefs_with_cv_qualif(t, &cv_qualif_orig);

    type_t* result = t;

    if (is_dependent_typename_type(t))
    {
        result = advance_dependent_typename_if_in_context(t, decl_context);

        // Still a dependent typename, make sure we update the template arguments
        if (is_dependent_typename_type(result))
        {
            scope_entry_t* dependent_entry = NULL;
            nodecl_t dependent_parts = nodecl_null();
            dependent_typename_get_components(result, 
                    &dependent_entry,
                    &dependent_parts);

            nodecl_t nodecl_new_parts = nodecl_null();
            int n = 0;
            nodecl_t* dependent_parts_list = nodecl_unpack_list(
                    nodecl_get_child(dependent_parts, 0), &n);

            int i;
            for (i = 0; i < n; i++)
            {
                if (nodecl_get_kind(dependent_parts_list[i]) == NODECL_CXX_DEP_TEMPLATE_ID)
                {
                    template_parameter_list_t* template_parameter_list
                        = nodecl_get_template_parameters(dependent_parts_list[i]);

                    template_parameter_list_t *fixed_tpl =
                        rebuild_template_arguments_advancing_dependent_typenames(template_parameter_list,
                                decl_context,
                                locus);

                    nodecl_t nodecl_new_template_id = nodecl_shallow_copy(dependent_parts_list[i]);
                    nodecl_set_template_parameters(
                            nodecl_new_template_id,
                            fixed_tpl);

                    nodecl_new_parts =
                        nodecl_append_to_list(
                                nodecl_new_parts,
                                nodecl_new_template_id);
                }
                else
                {
                    nodecl_new_parts =
                        nodecl_append_to_list(
                                nodecl_new_parts,
                                nodecl_shallow_copy(dependent_parts_list[i]));
                }
            }

            DELETE(dependent_parts_list);

            result = get_dependent_typename_type_from_parts(dependent_entry,
                    nodecl_make_cxx_dep_name_nested(nodecl_new_parts, locus));
        }
    }
    else if (is_named_type(t)
        && is_template_specialized_type(named_type_get_symbol(t)->type_information))
    {
        t = named_type_get_symbol(t)->type_information;

        template_parameter_list_t *fixed_tpl = rebuild_template_arguments_advancing_dependent_typenames(
                template_specialized_type_get_template_arguments(t),
                decl_context,
                locus);

        // Reask a new specialization
        result = template_type_get_specialized_type(
                template_specialized_type_get_related_template_type(t),
                fixed_tpl,
                decl_context,
                locus);
        free_template_parameter_list(fixed_tpl);
    }
    else if (is_lvalue_reference_type(t))
    {
        type_t* fixed_type = rebuild_type_advancing_dependent_typenames(
                no_ref(t),
                decl_context,
                locus);

        result = get_lvalue_reference_type(fixed_type);
    }
    else if (is_rvalue_reference_type(t))
    {
        type_t* fixed_type = rebuild_type_advancing_dependent_typenames(
                no_ref(t),
                decl_context,
                locus);

        result = get_rvalue_reference_type(fixed_type);
    }
    else if (is_pointer_type(t))
    {
        type_t* fixed_type = rebuild_type_advancing_dependent_typenames(
                pointer_type_get_pointee_type(t),
                decl_context,
                locus);

        result = get_pointer_type(fixed_type);
    }
    else if (is_array_type(t))
    {
        type_t* fixed_type = rebuild_type_advancing_dependent_typenames(
                array_type_get_element_type(t),
                decl_context,
                locus);

        result = get_array_type(fixed_type,
                array_type_get_array_size_expr(t),
                array_type_get_array_size_expr_context(t));
    }
    else if (is_pointer_to_member_type(t))
    {
        type_t* fixed_class_type = rebuild_type_advancing_dependent_typenames(
                pointer_to_member_type_get_class_type(t),
                decl_context,
                locus);
        type_t* fixed_pointee = rebuild_type_advancing_dependent_typenames(
                pointer_type_get_pointee_type(t),
                decl_context,
                locus);

        result = get_pointer_to_member_type(
                fixed_pointee,
                fixed_class_type);
    }
    else if (is_pack_type(t))
    {
        return get_pack_type(
                rebuild_type_advancing_dependent_typenames(
                    pack_type_get_packed_type(t),
                    decl_context,
                    locus));
    }
    else if (is_vector_type(t))
    {
        return get_vector_type_by_elements(
                rebuild_type_advancing_dependent_typenames(
                    vector_type_get_element_type(t),
                    decl_context,
                    locus),
                vector_type_get_num_elements(t));
    }
    else if (is_function_type(t))
    {
        type_t* fixed_return_type = rebuild_type_advancing_dependent_typenames(
                function_type_get_return_type(t),
                decl_context,
                locus);

        int N = function_type_get_num_parameters(t), P = N;

        parameter_info_t param_info[N+1];
        memset(param_info, 0, sizeof(param_info));

        if (function_type_get_has_ellipsis(t))
        {
            param_info[N-1].is_ellipsis = 1;
            param_info[N-1].type_info = get_ellipsis_type();
            param_info[N-1].nonadjusted_type_info = NULL;
            P = N - 1;
        }

        int i;
        for (i = 0; i < P; i++)
        {
            type_t* fixed_param_type = rebuild_type_advancing_dependent_typenames(
                    function_type_get_parameter_type_num(t, i),
                    decl_context,
                    locus);
            param_info[i].type_info = fixed_param_type;
        }

        result = get_new_function_type(fixed_return_type, param_info, N, function_type_get_ref_qualifier(t));
    }

    result = get_cv_qualified_type(result, cv_qualif_orig);

    return result;
}

extern inline type_t* fix_dependent_typenames_in_context(type_t* t, const decl_context_t* decl_context, const locus_t* locus)
{
    if (!type_contains_a_dependent_typename(t))
        return t;

    type_t* rebuilt_type = rebuild_type_advancing_dependent_typenames(t, decl_context, locus);
    return rebuilt_type;
}

static char syntactic_comparison_of_one_dependent_part(
        nodecl_t dependent_part_1,
        nodecl_t dependent_part_2)
{
    if (nodecl_get_kind(dependent_part_1) != 
            nodecl_get_kind(dependent_part_2))
        return 0;

    nodecl_t nodecl_simple_name_1 = dependent_part_1;
    nodecl_t nodecl_simple_name_2 = dependent_part_2;

    template_parameter_list_t* template_parameter_list_1 = NULL;
    template_parameter_list_t* template_parameter_list_2 = NULL;

    if (nodecl_get_kind(dependent_part_1) == NODECL_CXX_DEP_TEMPLATE_ID)
    {
        nodecl_simple_name_1 = nodecl_get_child(dependent_part_1, 0);
        nodecl_simple_name_2 = nodecl_get_child(dependent_part_2, 0);

        template_parameter_list_1 = nodecl_get_template_parameters(dependent_part_1);
        template_parameter_list_2 = nodecl_get_template_parameters(dependent_part_2);
    }

    const char* name_1 = nodecl_get_text(nodecl_simple_name_1);
    const char* name_2 = nodecl_get_text(nodecl_simple_name_2);

    if (strcmp(name_1,name_2) != 0)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Mismatch of component name '%s' != '%s'\n",
                    name_1,
                    name_2);
        }
        return 0;
    }

    if ((template_parameter_list_1 == NULL
            || template_parameter_list_2 == NULL)
            && (template_parameter_list_1 != template_parameter_list_2))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Mismatch in the kind of components %s type has template arguments while %s does not\n",
                    template_parameter_list_1 != NULL ? "first" : "second",
                    template_parameter_list_1 == NULL ? "first" : "second");
        }
        return 0;
    }

    if (template_parameter_list_1 != NULL)
    {
        if (!same_template_argument_list(template_parameter_list_1,
                    template_parameter_list_2))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Template arguments do not match\n");
            }
            return 0;
        }
    }

    return 1;
}

static char syntactic_comparison_of_dependent_parts(
        nodecl_t dependent_parts_1,
        nodecl_t dependent_parts_2)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUTILS: Comparing (syntactically) nested-name parts '");
        // dependent_name_part_t* part = dependent_parts_1;
        // while (part != NULL)
        // {
        //     fprintf(stderr, "%s%s%s",
        //             part->name,
        //             part->template_arguments == NULL ? "" : get_template_parameters_list_str(part->template_arguments),
        //             part->next == NULL ? "" : "::");
        //     part = part->next;
        // }
        // fprintf(stderr, "' vs '");
        // part = dependent_parts_2;
        // while (part != NULL)
        // {
        //     fprintf(stderr, "%s%s%s",
        //             part->name,
        //             part->template_arguments == NULL ? "" : get_template_parameters_list_str(part->template_arguments),
        //             part->next == NULL ? "" : "::");
        //     part = part->next;
        // }
        fprintf(stderr, "'\n");
    }

    if (nodecl_is_null(dependent_parts_1) != nodecl_is_null(dependent_parts_2))
    {
        return 0;
    } 
    else if (nodecl_is_null(dependent_parts_1))
    {
        return 1;
    }

    int num_items1 = 0;
    nodecl_t* list1 = nodecl_unpack_list(nodecl_get_child(dependent_parts_1, 0), &num_items1);

    int num_items2 = 0;
    nodecl_t* list2 = nodecl_unpack_list(nodecl_get_child(dependent_parts_2, 0), &num_items2);

    if (num_items1 != num_items2)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: One of the nested names is longer than the other\n");
        }
        DELETE(list1);
        DELETE(list2);
        return 0;
    }

    int i;
    for (i = 0; i < num_items1; i++)
    {
        nodecl_t item1 = list1[i];
        nodecl_t item2 = list2[i];

        if (!syntactic_comparison_of_one_dependent_part(item1, item2))
        {
            DELETE(list1);
            DELETE(list2);
            return 0;
        }
    }
    DELETE(list1);
    DELETE(list2);

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
    nodecl_t dependent_parts_1 = nodecl_null();

    dependent_typename_get_components(p_t1, 
            &dependent_entry_1,
            &dependent_parts_1);

    scope_entry_t* dependent_entry_2 = NULL;
    nodecl_t dependent_parts_2 = nodecl_null();

    dependent_typename_get_components(p_t2, 
            &dependent_entry_2,
            &dependent_parts_2);


    // The dependent entries are functions, they must be the same
    if (dependent_entry_1->kind == SK_FUNCTION
            && dependent_entry_2->kind == SK_FUNCTION)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Both dependent entries are functions\n");
        }
        if (dependent_entry_1 != dependent_entry_2)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: But they are not the same\n");
            }
            return 0;
        }
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUTILS: Both functions are the same function\n");
        }
    }
    else
    {
        type_t* type_to_compare_1 = NULL;
        if (dependent_entry_1->kind == SK_TEMPLATE_TYPE_PARAMETER)
        {
            // T::x where T is a type template parameter
            type_to_compare_1 = get_user_defined_type(dependent_entry_1);
        }
        else
        {
            type_to_compare_1 = dependent_entry_1->type_information;
        }

        type_t* type_to_compare_2 = NULL;
        if (dependent_entry_2->kind == SK_TEMPLATE_TYPE_PARAMETER)
        {
            type_to_compare_2 = get_user_defined_type(dependent_entry_2);
        }
        else
        {
            type_to_compare_2 = dependent_entry_2->type_information;
        }

        if (!equivalent_types(type_to_compare_1, type_to_compare_2))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Dependent entry is different\n");
            }
            return 0;
        }
    }

    return syntactic_comparison_of_dependent_parts(dependent_parts_1, dependent_parts_2);
}

extern inline char is_builtin_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE);
}

extern inline char is_fundamental_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE);
}

extern inline char is_non_derived_type(type_t* t)
{
    return t != NULL 
        && t->kind == TK_DIRECT;
}

extern inline char is_integer_type(type_t* t)
{
    return is_integral_type(t);
}

extern inline char is_any_int_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && (t->type->builtin_type == BT_INT 
                || t->type->builtin_type == BT_BYTE));
}

extern inline char is_any_unsigned_int_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && (t->type->builtin_type == BT_INT
                || t->type->builtin_type == BT_BYTE)
            && t->type->is_unsigned);
}

extern inline char is_any_signed_int_type(type_t* t)
{
    return (is_any_int_type(t) 
            && !is_any_unsigned_int_type(t));
}

extern inline char is_integral_type(type_t* t)
{
    return (is_any_int_type(t)
            || is_bool_type(t)
            || is_character_type(t)
            || is_char16_t_type(t)
            || is_char32_t_type(t)
            || is_wchar_t_type(t)
            // In C, enumerated types are integral types
            || (is_enum_type(t) && IS_C_LANGUAGE)
            // Mercurium extension
            || is_mask_type(t));
}

extern inline char is_signed_integral_type(type_t* t)
{
    return is_signed_char_type(t)
        || is_signed_byte_type(t)
        || is_signed_short_int_type(t)
        || is_signed_int_type(t)
        || is_signed_long_int_type(t)
        || is_signed_long_long_int_type(t)
        || is_signed_int128_type(t)
        || (IS_CXX_LANGUAGE
                && is_wchar_t_type(t)
                    && is_signed_integral_type(
                        (CURRENT_CONFIGURATION->type_environment->int_type_of_wchar_t)()));
}

extern inline char is_unsigned_integral_type(type_t* t)
{
    return is_unsigned_char_type(t)
        || is_unsigned_byte_type(t)
        || is_unsigned_short_int_type(t)
        || is_unsigned_int_type(t)
        || is_unsigned_long_int_type(t)
        || is_unsigned_long_long_int_type(t)
        || is_unsigned_int128_type(t)
        || is_mask_type(t)
        || (IS_CXX_LANGUAGE
                && is_wchar_t_type(t)
                && is_unsigned_integral_type(
                    (CURRENT_CONFIGURATION->type_environment->int_type_of_wchar_t)()));
}

extern inline char is_signed_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && t->type->is_signed
            && !t->type->is_unsigned
            && !t->type->is_long
            && !t->type->is_short);
}

extern inline char is_unsigned_int_type(type_t *t)
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

extern inline char is_signed_short_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && t->type->is_signed
            && !t->type->is_unsigned
            && !t->type->is_long
            && t->type->is_short);
}

extern inline char is_unsigned_short_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && !t->type->is_signed
            && t->type->is_unsigned
            && !t->type->is_long
            && t->type->is_short);
}

extern inline char is_signed_long_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && t->type->is_signed
            && !t->type->is_unsigned
            && (t->type->is_long == 1)
            && !t->type->is_short);
}

extern inline char is_unsigned_long_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && !t->type->is_signed
            && t->type->is_unsigned
            && (t->type->is_long == 1)
            && !t->type->is_short);
}

extern inline char is_signed_long_long_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && t->type->is_signed
            && !t->type->is_unsigned
            && (t->type->is_long == 2)
            && !t->type->is_short);
}

extern inline char is_unsigned_long_long_int_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && !t->type->is_signed
            && t->type->is_unsigned
            && (t->type->is_long == 2)
            && !t->type->is_short);
}

extern inline char is_signed_int128_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && t->type->is_signed
            && !t->type->is_unsigned
            && (t->type->is_long == 3)
            && !t->type->is_short);
}

extern inline char is_unsigned_int128_type(type_t *t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_INT
            && !t->type->is_signed
            && t->type->is_unsigned
            && (t->type->is_long == 3)
            && !t->type->is_short);
}

extern inline char is_signed_byte_type(type_t *t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_BYTE
            && t->type->is_signed
            && !t->type->is_unsigned
            && !t->type->is_long
            && !t->type->is_short);
}

extern inline char is_unsigned_byte_type(type_t *t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_BYTE
            && !t->type->is_signed
            && t->type->is_unsigned
            && !t->type->is_long
            && !t->type->is_short);
}

extern inline char is_char_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_CHAR
            && !t->type->is_signed
            && !t->type->is_unsigned);
}

extern inline char is_character_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_CHAR);
}

extern inline char is_char16_t_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_CHAR16_T);
}

extern inline char is_char32_t_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_CHAR32_T);
}

extern inline char is_wchar_t_type(type_t* t)
{
    t = advance_over_typedefs(t);
    C_LANGUAGE()
    {
        // In C, there is no wchar_t but an integral type representing it
        return equivalent_types(t, get_wchar_t_type());
    }

    // C++
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_WCHAR);
}

extern inline char is_signed_char_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_CHAR
            && t->type->is_signed
            && !t->type->is_unsigned);
}

extern inline char is_unsigned_char_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_CHAR
            && !t->type->is_signed
            && t->type->is_unsigned);
}

extern inline char is_pointer_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_POINTER);
}

extern inline char is_function_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_FUNCTION);
}

extern inline type_t* function_type_get_return_type(type_t* t)
{
    ERROR_CONDITION(!is_function_type(t), "This is not a function type", 0);
    t = advance_over_typedefs(t);

    return t->function->return_type;
}

// Can be used both for pointers and pointers to members
extern inline type_t* pointer_type_get_pointee_type(type_t *t)
{
    ERROR_CONDITION(!is_pointer_type(t)
            && !is_pointer_to_member_type(t), "This is not a pointer/pointer to member type", 0);
    t = advance_over_typedefs(t);

    return t->pointer->pointee;
}

extern inline type_t* pointer_to_member_type_get_class_type(type_t *t)
{
    ERROR_CONDITION(!is_pointer_to_member_type(t), "This is not a pointer to member type", 0);
    t = advance_over_typedefs(t);

    return t->pointer->pointee_class_type;
}

extern inline type_t* array_type_get_element_type(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->element_type;
}

extern inline nodecl_t array_type_get_array_size_expr(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->whole_size;
}

extern inline char array_type_is_unknown_size(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return nodecl_is_null(t->array->whole_size);
}

extern inline nodecl_t array_type_get_array_lower_bound(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->lower_bound;
}

extern inline nodecl_t array_type_get_array_upper_bound(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->upper_bound;
}

extern inline int array_type_get_total_number_of_elements(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    int number_of_elements = 1;
    while (is_array_type(t))
    {
        if (array_type_is_unknown_size(t))
        {
            return -1;
        }

        nodecl_t whole_size = array_type_get_array_size_expr(t);
        if (!nodecl_is_constant(whole_size))
        {
            return -1;
        }

        const_value_t * size = nodecl_get_constant(whole_size);
        number_of_elements *= const_value_cast_to_signed_int(size);
        t = array_type_get_element_type(t);
    }
    return number_of_elements;
}

extern inline const decl_context_t* array_type_get_array_size_expr_context(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->array_expr_decl_context;
}


extern inline char array_type_with_descriptor(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->with_descriptor;
}

extern inline char array_type_has_region(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->region != NULL;    
}

extern inline const decl_context_t* array_type_get_region_size_expr_context(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->region->region_decl_context;
}

nodecl_t array_type_get_region_size_expr(type_t* t)
{
     ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->region->whole_size;   
}

extern inline nodecl_t array_type_get_region_lower_bound(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->region->lower_bound;
}

extern inline nodecl_t array_type_get_region_upper_bound(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->region->upper_bound;    
}

extern inline nodecl_t array_type_get_region_stride(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->region->stride;    
}

extern inline char array_type_is_vla(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "This is not an array type", 0);
    t = advance_over_typedefs(t);

    return t->array->is_vla;
}

extern inline char is_array_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL && t->kind == TK_ARRAY);
}

extern inline char is_pointer_to_class_type(type_t* t1)
{
    return (is_pointer_type(t1) 
            && is_class_type(pointer_type_get_pointee_type(t1)));
}

extern inline char is_pointer_to_function_type(type_t* t1)
{
    return (is_pointer_type(t1) 
            && is_function_type(pointer_type_get_pointee_type(t1)));
}

extern inline char is_lvalue_reference_to_class_type(type_t* t1)
{
    return (is_lvalue_reference_type(t1) 
            && is_class_type(reference_type_get_referenced_type(t1)));
}

extern inline char is_rvalue_reference_to_class_type(type_t* t1)
{
    return (is_rvalue_reference_type(t1) 
            && is_class_type(reference_type_get_referenced_type(t1)));
}

extern inline char is_rebindable_reference_to_class_type(type_t* t1)
{
    return (is_rebindable_reference_type(t1) 
            && is_class_type(reference_type_get_referenced_type(t1)));
}

extern inline char is_any_reference_type(type_t* t1)
{
    return is_lvalue_reference_type(t1)
        || is_rvalue_reference_type(t1)
        || is_rebindable_reference_type(t1);
}

extern inline char is_any_reference_to_class_type(type_t* t1)
{
    return is_lvalue_reference_to_class_type(t1)
        || is_rvalue_reference_to_class_type(t1)
        || is_rebindable_reference_to_class_type(t1);
}

extern inline char is_void_pointer_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_POINTER
            && is_void_type(t->pointer->pointee));
}

extern inline char is_void_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && t->type->builtin_type == BT_VOID);
}

extern inline char is_pointer_to_member_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_POINTER_TO_MEMBER);
}


extern inline char is_named_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_INDIRECT
            && t->type->user_defined_type != NULL);
}

extern inline char is_indirect_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_INDIRECT
            && t->type->is_indirect
            && t->type->user_defined_type != NULL);
}

extern inline char is_mutable_indirect_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_INDIRECT
            && t->type->is_indirect
            && t->type->is_mutable
            && t->type->user_defined_type != NULL);
}

extern inline scope_entry_t* named_type_get_symbol(type_t* t)
{
    ERROR_CONDITION(!is_named_type(t), "This is not a named type\n", 0);

    return t->type->user_defined_type;
}

extern inline char is_floating_type(type_t* t)
{
    // Advance over typedefs
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_BUILTIN_TYPE
            && (t->type->builtin_type == BT_FLOAT
                || t->type->builtin_type == BT_DOUBLE
                || t->type->builtin_type == BT_OTHER_FLOAT));
}

extern inline char is_arithmetic_type(type_t* t)
{
    return is_integral_type(t) || is_floating_type(t) || is_complex_type(t);
}

extern inline char is_int_or_floating_type(type_t* t)
{
    return is_any_int_type(t) || is_floating_type(t);
}

extern inline char is_double_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && is_floating_type(t) 
            && t->type->builtin_type == BT_DOUBLE
            && !t->type->is_long);
}

extern inline char is_long_double_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && is_floating_type(t) 
            && t->type->builtin_type == BT_DOUBLE
            && t->type->is_long);
}

extern inline char is_float_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && is_floating_type(t) 
            && t->type->builtin_type == BT_FLOAT);
}

extern inline char is_other_float_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && is_floating_type(t) 
            && t->type->builtin_type == BT_OTHER_FLOAT);
}

extern inline char is_float128_type(type_t* t)
{
    return (is_other_float_type(t) && floating_type_get_info(t)->size_of == 16);
}

extern inline char is_complex_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && is_non_derived_type(t)
            && t->type->kind == STK_COMPLEX);
}

extern inline type_t* reference_type_get_referenced_type(type_t* t1)
{
    ERROR_CONDITION(!is_lvalue_reference_type(t1)
            && !is_rvalue_reference_type(t1), 
            "This is not a reference type", 0);
    t1 = advance_over_typedefs(t1);

    return t1->pointer->pointee;
}

// Transforms T& or T&& into T
extern inline type_t* no_ref(type_t* t)
{
    if (t == NULL)
        return NULL;

    if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
        return reference_type_get_referenced_type(t);
    return t;
}

// Transforms T or T&& into T&
extern inline type_t* lvalue_ref(type_t* t)
{
    if (!is_any_reference_type(t))
        return get_lvalue_reference_type(t);
    else if (is_rvalue_reference_type(t))
        return get_lvalue_reference_type(
                reference_type_get_referenced_type(t));
    return t;
}

extern inline char is_lvalue_reference_type(type_t* t1)
{
    t1 = advance_over_typedefs(t1);

    return (t1 != NULL
            && (t1->kind == TK_LVALUE_REFERENCE
                // Rebindable references are considered as lvalue references
                || t1->kind == TK_REBINDABLE_REFERENCE));
}

extern inline char is_rvalue_reference_type(type_t* t1)
{
    t1 = advance_over_typedefs(t1);

    return (t1 != NULL
            && t1->kind == TK_RVALUE_REFERENCE);
}

extern inline char is_rebindable_reference_type(type_t* t1)
{
    t1 = advance_over_typedefs(t1);

    return (t1 != NULL
            && t1->kind == TK_REBINDABLE_REFERENCE);
}

extern inline const decl_context_t* enum_type_get_context(type_t* t)
{
    ERROR_CONDITION(!is_enum_type(t), "This is not an enumerated type", 0);
    t = advance_over_typedefs(t);
    if (is_named_type(t))
    {
        t = named_type_get_symbol(t)->type_information;
    }
    return t->type->type_decl_context;
}

extern inline const decl_context_t* class_type_get_context(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);
    t = get_actual_class_type(t);
    return t->type->type_decl_context;
}

extern inline char is_bool_type(type_t* t1)
{
    // Advance over typedefs
    t1 = advance_over_typedefs(t1);

    return (t1 != NULL 
            && t1->kind == TK_DIRECT
            && t1->type->kind == STK_BUILTIN_TYPE
            && t1->type->builtin_type == BT_BOOL);
}

extern inline char is_dependent_typename_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_TEMPLATE_DEPENDENT_TYPE);
}

extern inline type_t* get_actual_class_type(type_t* class_type)
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

extern inline char is_class_type(type_t* possible_class)
{
    return (is_named_class_type(possible_class) || is_unnamed_class_type(possible_class));
}

extern inline char is_union_type(type_t* possible_union)
{
    if (!is_class_type(possible_union))
        return 0;

    type_t* actual_class = get_actual_class_type(possible_union);

    return (actual_class->type->class_info->class_kind == TT_UNION);
}

extern inline char is_unnamed_class_type(type_t* possible_class)
{
    possible_class = advance_over_typedefs(possible_class);
    return (possible_class != NULL
            && possible_class->kind == TK_DIRECT
            && possible_class->type->kind == STK_CLASS);
}

extern inline char is_named_class_type(type_t* possible_class)
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

extern inline char is_class_type_or_array_thereof(type_t* t)
{
    return is_class_type(t)
        || (is_array_type(t) && is_class_type(array_type_get_element_type(t)));
}

static char class_type_is_base_(type_t* possible_base,
        type_t* possible_derived,
        char allow_incomplete_independent)
{
    possible_base = get_unqualified_type(advance_over_typedefs(possible_base));
    possible_derived = get_unqualified_type(advance_over_typedefs(possible_derived));

    ERROR_CONDITION(!is_class_type(possible_base)
            || !is_class_type(possible_derived), 
            "This function expects class types", 0);

    possible_base = get_actual_class_type(possible_base);
    possible_derived = get_actual_class_type(possible_derived);

    if (equivalent_types(possible_base, possible_derived))
        return 1;

    ERROR_CONDITION(
            !allow_incomplete_independent
            && class_type_is_incomplete_independent(possible_derived),
            "Cannot test if class type is derived of another if "
            "the potentially derived is independent incomplete\n", 0);

    // Search in bases of the derived
    int i;
    for (i = 0; i < class_type_get_num_bases(possible_derived); i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        type_t* current_base = class_type_get_base_num(possible_derived, i, 
                &is_virtual, &is_dependent,
                /* is_expansion */ NULL,
                /* access_specifier */ NULL)
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
        type_t* current_base = class_type_get_base_num(possible_derived, i,
                &is_virtual, &is_dependent,
                /* is_expansion */ NULL,
                /* access_specifier_t */ NULL)
            ->type_information;

        if (is_dependent)
            continue;

        if (class_type_is_base_(possible_base, current_base, /* allow_incomplete_independent */ 0))
            return 1;
    }

    // Not found
    return 0;
}

extern inline char class_type_is_base(type_t* possible_base, type_t* possible_derived)
{
    // Not found
    return class_type_is_base_(possible_base, possible_derived, /* allow_incomplete_independent */ 0);
}

extern inline char class_type_is_base_instantiating(type_t* possible_base, type_t* possible_derived, const locus_t* locus)
{
    CXX_LANGUAGE()
    {
        if (class_type_is_incomplete_independent(possible_derived))
        {
            if (is_named_class_type(possible_derived))
            {
                class_type_complete_if_possible(
                        named_type_get_symbol(possible_derived),
                        named_type_get_symbol(possible_derived)->decl_context,
                        locus);
            }
            else
            {
                internal_error("An unnamed incomplete template dependent type is not valid here", 0);
            }
        }
    }

    return class_type_is_base_(possible_base, possible_derived, /* allow_incomplete_independent */ 1);
}


extern inline char class_type_is_base_strict(type_t* possible_base, type_t* possible_derived)
{
    possible_base = get_unqualified_type(advance_over_typedefs(possible_base));
    possible_derived = get_unqualified_type(advance_over_typedefs(possible_derived));

    ERROR_CONDITION(!is_class_type(possible_base)
            || !is_class_type(possible_derived), 
            "This function expects class types", 0);

    if (equivalent_types(possible_base, possible_derived))
        return 0;

    return class_type_is_base(possible_base, possible_derived);
}

extern inline char class_type_is_base_strict_instantiating(type_t* possible_base, type_t* possible_derived, const locus_t* locus)
{
    possible_base = get_unqualified_type(advance_over_typedefs(possible_base));
    possible_derived = get_unqualified_type(advance_over_typedefs(possible_derived));

    ERROR_CONDITION(!is_class_type(possible_base)
            || !is_class_type(possible_derived), 
            "This function expects class types", 0);

    if (equivalent_types(possible_base, possible_derived))
        return 0;

    return class_type_is_base_instantiating(possible_base, possible_derived, locus);
}

extern inline char class_type_is_derived(type_t* possible_derived, type_t* possible_base)
{
    return class_type_is_base(possible_base, possible_derived);
}

extern inline char class_type_is_derived_instantiating(type_t* possible_derived, type_t* possible_base, const locus_t* locus)
{
    return class_type_is_base_instantiating(possible_base, possible_derived, locus);
}

extern inline char class_type_is_derived_strict(type_t* possible_derived, type_t* possible_base)
{
    return class_type_is_base_strict(possible_base, possible_derived);
}

extern inline char class_type_is_derived_strict_instantiating(type_t* possible_derived, type_t* possible_base, const locus_t* locus)
{
    return class_type_is_base_strict_instantiating(possible_base, possible_derived, locus);
}

extern inline char is_pointer_to_void_type(type_t* t)
{
    return (is_pointer_type(t)
            && is_void_type(pointer_type_get_pointee_type(t)));
}

extern inline char pointer_to_class_type_is_base(type_t* possible_pclass_base,
        type_t* possible_pclass_derived)
{
    ERROR_CONDITION(!is_pointer_to_class_type(possible_pclass_base)
            || !is_pointer_to_class_type(possible_pclass_derived),
            "Both thypes must be pointer to class", 0);

    type_t* possible_base = pointer_type_get_pointee_type(possible_pclass_base);
    type_t* possible_derived = pointer_type_get_pointee_type(possible_pclass_derived);

    return class_type_is_base(possible_base, possible_derived);
}

extern inline char pointer_to_class_type_is_base_strict(type_t* possible_pclass_base,
        type_t* possible_pclass_derived)
{
    ERROR_CONDITION(!is_pointer_to_class_type(possible_pclass_base)
            || !is_pointer_to_class_type(possible_pclass_derived),
            "Both thypes must be pointer to class", 0);

    type_t* possible_base = pointer_type_get_pointee_type(possible_pclass_base);
    type_t* possible_derived = pointer_type_get_pointee_type(possible_pclass_derived);

    return class_type_is_base_strict(possible_base, possible_derived);
}

char pointer_to_class_type_is_derived(type_t* possible_pclass_derived,
        type_t* possible_pclass_base)
{
    return pointer_to_class_type_is_base(possible_pclass_base, possible_pclass_derived);
}

extern inline char pointer_to_class_type_is_derived_strict(type_t* possible_pclass_derived,
        type_t* possible_pclass_base)
{
    return pointer_to_class_type_is_base_strict(possible_pclass_base, possible_pclass_derived);
}

extern inline cv_qualifier_t get_cv_qualifier(type_t* type_info)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);
    if (is_array_type(type_info))
    {
        return get_cv_qualifier(array_type_get_element_type(type_info));
    }
    return type_info->cv_qualifier;
}

extern inline cv_qualifier_t array_type_get_cv_qualifier(type_t *array_type)
{
    ERROR_CONDITION(!is_array_type(array_type), "This is not an array type", 0);
    return array_type->cv_qualifier;
}

extern inline type_t* canonical_type(type_t* type)
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

extern inline char is_dependent_type(type_t* type)
{
    if (type == NULL)
        return 0;

    type = canonical_type(type);

    return type->info->is_dependent;
}

extern inline void set_is_dependent_type(type_t* t, char is_dependent)
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
        result = strappend(result, "__restrict ");
    }

    return result;
}

static const char* get_gcc_attributes_string(type_t* type_info)
{
    const char* result = "";
    int i;
    for (i = 0; i < type_info->info->num_gcc_attributes; i++)
    {
        if (nodecl_is_null(type_info->info->gcc_attributes[i].expression_list))
        {
            uniquestr_sprintf(&result, "__attribute__((%s)) ",
                    type_info->info->gcc_attributes[i].attribute_name);
        }
        else
        {
            const char* expr_list_str = "";
            int num_items = 0;
            nodecl_t* n = nodecl_unpack_list(type_info->info->gcc_attributes[i].expression_list,
                    &num_items);
            int j;
            for (j = 0; j < num_items; j++)
            {
                if (j > 0)
                    expr_list_str = strappend(expr_list_str, ", ");

                // FIXME - We may need a better context
                expr_list_str = strappend(expr_list_str, codegen_to_str(n[j], CURRENT_COMPILED_FILE->global_decl_context));
            }
            DELETE(n);

            uniquestr_sprintf(&result, "__attribute__((%s(%s))) ",
                    type_info->info->gcc_attributes[i].attribute_name,
                    expr_list_str);
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

static char is_nonspecialized_function_pred(scope_entry_t* entry, void *p UNUSED_PARAMETER)
{
    entry = entry_advance_aliases(entry);
    return (entry->kind == SK_FUNCTION);
}

static char is_template_function_pred(scope_entry_t* entry, void *p UNUSED_PARAMETER)
{
    entry = entry_advance_aliases(entry);
    return (entry->kind == SK_TEMPLATE
            && (named_type_get_symbol(
                    template_type_get_primary_type(entry->type_information))->kind == SK_FUNCTION));
}

extern inline char is_function_or_template_function_name_or_extern_variable(scope_entry_t* entry, void* p UNUSED_PARAMETER)
{
    return (is_nonspecialized_function_pred(entry, NULL)
            || is_template_function_pred(entry, NULL)
            || (entry->kind == SK_VARIABLE
                && symbol_entity_specs_get_is_extern(entry)));
}

extern inline const char* get_simple_type_name_string_internal_common(scope_entry_t* entry, const decl_context_t* decl_context,
        void* data UNUSED_PARAMETER)
{
    char is_dependent = 0;
    int max_level = 0;
    const char* result = get_fully_qualified_symbol_name(entry,
            decl_context, &is_dependent, &max_level);

    // If is a dependent name and it is qualified then it can be
    // given a "typename" keyword (in some cases one must do that)
    if (is_dependent && max_level > 0)
    {
        result = strappend("typename ", result);
    }

    if (entry->kind == SK_CLASS
            || entry->kind == SK_ENUM)
    {
        // It may happen that a function is hiding our typename in this scope
        scope_entry_list_t* entry_list = query_in_scope_str(entry->decl_context, entry->symbol_name, NULL);
        entry_list = filter_symbol_using_predicate(entry_list, is_function_or_template_function_name_or_extern_variable, NULL);

        // It seems somebody is hiding our name in this scope
        if (entry_list != NULL)
        {
            if (entry->kind == SK_ENUM)
            {
                result = strappend("enum ", result);
            }
            else if (entry->kind == SK_CLASS)
            {
                switch (class_type_get_class_kind(entry->type_information))
                {
                    case TT_UNION:
                        result = strappend("union ", result); break;
                    case TT_STRUCT:
                        result = strappend("struct ", result); break;
                    case TT_CLASS:
                        result = strappend("class ", result); break;
                    default:
                        internal_error("Code unreachable", 0);
                }
            }
        }
        entry_list_free(entry_list);
    }

    return result;
}

static const char* get_simple_type_name_string_internal_impl(const decl_context_t* decl_context,
        type_t* t,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data);

static const char* get_simple_type_name_string_internal(const decl_context_t* decl_context,
        type_t* type_info,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data);

// Vector flavors

extern inline const char* print_gnu_vector_type(
        const decl_context_t* decl_context,
        type_t* t,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data)
{
    const char* typename = get_simple_type_name_string_internal_impl(decl_context,
            vector_type_get_element_type(t),
            print_symbol_fun,
            print_symbol_data);

    const char* c = NULL;

    const char* may_alias = "";

    // Workaround for i386 and x86-64 MMX and SSE vectors, which happen to be may_alias
    if ((strcmp(CURRENT_CONFIGURATION->type_environment->environ_id, "linux-x86_64") == 0
                || strcmp(CURRENT_CONFIGURATION->type_environment->environ_id, "linux-i386") == 0)
            && (vector_type_get_vector_size_in_bytes(t) == 8       // MMX
                || vector_type_get_vector_size_in_bytes(t) == 16   // SSE
                || vector_type_get_vector_size_in_bytes(t) == 32)) // AVX2
    {
        may_alias = " __attribute__((__may_alias__))";
    }

    uniquestr_sprintf(&c, "__attribute__((vector_size(%d)))%s %s",
            vector_type_get_vector_size_in_bytes(t),
            may_alias,
            typename);

    return c;
}

extern inline const char* print_intel_sse_avx_vector_type(
        const decl_context_t* decl_context,
        type_t* t,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data)
{
    type_t* element_type = vector_type_get_element_type(t);
    int size = vector_type_get_vector_size_in_bytes(t);

    switch (size)
    {
        case 8:
            {
                return "__m64";
            }
        case 16:
            {
                if (is_float_type(element_type))
                {
                    return "__m128";
                }
                else if (is_double_type(element_type))
                {
                    return "__m128d";
                }
                else if (is_integer_type(element_type))
                {
                    return "__m128i";
                }
            }
      case 32:
            {
                if (is_float_type(element_type))
                {
                    return "__m256";
                }
                else if (is_double_type(element_type))
                {
                    return "__m256d";
                }
                else if (is_integer_type(element_type))
                {
                    return "__m256i";
                }
            }
      case 64:
            {
                if (is_float_type(element_type))
                {
                    return "__m512";
                }
                else if (is_double_type(element_type))
                {
                    return "__m512d";
                }
                else if (is_integer_type(element_type))
                {
                    return "__m512i";
                }
            }
      case 128:
            {
                if (is_float_type(element_type))
                {
                    return "__m1024";
                }
                else if (is_double_type(element_type))
                {
                    return "__m1024d";
                }
                else if (is_integer_type(element_type))
                {
                    return "__m1024i";
                }
            }

    }

    const char* typename = get_simple_type_name_string_internal_impl(decl_context,
            vector_type_get_element_type(t),
            print_symbol_fun,
            print_symbol_data);
    const char* c = NULL;
    uniquestr_sprintf(&c, "<<intel-vector-%s-%d>>",
            typename,
            vector_type_get_vector_size_in_bytes(t));
    return c;
}

extern inline const char* print_altivec_vector_type(
        const decl_context_t* decl_context,
        type_t* t,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data)
{
    const char* typename = get_simple_type_name_string_internal_impl(decl_context,
            vector_type_get_element_type(t),
            print_symbol_fun,
            print_symbol_data);

    int size = vector_type_get_vector_size_in_bytes(t);

    const char* c = NULL;
    if (size == 16)
    {
        uniquestr_sprintf(&c, "vector %s",
                typename);
    }
    else
    {
        uniquestr_sprintf(&c, "<<altivec-vector-%s-%d>>",
                typename,
                vector_type_get_vector_size_in_bytes(t));
    }

    return c;
}

extern inline const char* print_opencl_vector_type(
        const decl_context_t* decl_context,
        type_t* t,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data)
{
    int size = vector_type_get_vector_size_in_bytes(t);
    type_t* element_type = vector_type_get_element_type(t);

    const char* c = NULL;
    if (is_signed_char_type(element_type))
    {
        int num_items = size;
        uniquestr_sprintf(&c, "char%d",
                num_items);
        return c;
    }
    else if (is_signed_char_type(element_type))
    {
        int num_items = size;
        uniquestr_sprintf(&c, "char%d",
                num_items);
        return c;
    }
    else if (is_signed_short_int_type(element_type))
    {
        int num_items = size / 2;
        uniquestr_sprintf(&c, "short%d",
                num_items);
        return c;
    }
    else if (is_unsigned_short_int_type(element_type))
    {
        int num_items = size / 2;
        uniquestr_sprintf(&c, "ushort%d",
                num_items);
        return c;
    }
    else if (is_signed_int_type(element_type))
    {
        int num_items = size / 4;
        uniquestr_sprintf(&c, "int%d",
                num_items);
        return c;
    }
    else if (is_unsigned_int_type(element_type))
    {
        int num_items = size / 4;
        uniquestr_sprintf(&c, "uint%d",
                num_items);
        return c;
    }
    else if (is_signed_long_int_type(element_type))
    {
        int num_items = size / 8;
        uniquestr_sprintf(&c, "long%d",
                num_items);
        return c;
    }
    else if (is_unsigned_long_int_type(element_type))
    {
        int num_items = size / 8;
        uniquestr_sprintf(&c, "ulong%d",
                num_items);
        return c;
    }
    else if (is_float_type(element_type))
    {
        int num_items = size / 4;
        uniquestr_sprintf(&c, "float%d",
                num_items);
        return c;
    }
    else if (is_double_type(element_type))
    {
        int num_items = size / 8;
        uniquestr_sprintf(&c, "double%d",
                num_items);
        return c;
    }

    const char* typename = get_simple_type_name_string_internal_impl(decl_context,
            vector_type_get_element_type(t),
            print_symbol_fun,
            print_symbol_data);
    uniquestr_sprintf(&c, "<<opencl-vector-%s-%d>>",
            typename,
            vector_type_get_vector_size_in_bytes(t));
    return c;
}

extern inline const char* print_neon_vector_type(
        const decl_context_t* decl_context,
        type_t* t,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data)
{
    int size = vector_type_get_vector_size_in_bytes(t);
    int num_elements = vector_type_get_num_elements(t);
    type_t* element_type = vector_type_get_element_type(t);

    const char *c = NULL;
    if (size == 8
            || size == 16)
    {
        if (is_integral_type(element_type))
        {
            uniquestr_sprintf(&c, "%sint%dx%d_t",
                    is_unsigned_integral_type(element_type) ? "u" : "",
                    (int)type_get_size(element_type) * 8,
                    num_elements);
            return c;
        }
        else if (is_float_type(element_type))
        {
            uniquestr_sprintf(&c, "float32x%d_t", num_elements);
            return c;
        }
        else if (is_double_type(element_type))
        {
            uniquestr_sprintf(&c, "float64x%d_t", num_elements);
            return c;
        }
    }

    const char* typename = get_simple_type_name_string_internal_impl(decl_context,
            vector_type_get_element_type(t),
            print_symbol_fun,
            print_symbol_data);
    uniquestr_sprintf(&c, "<<neon-vector-%s-%d>>",
            typename,
            vector_type_get_vector_size_in_bytes(t));
    return c;
}

// Improve this
enum { ROMOL_VECTOR_LENGTH = 64 };

extern inline const char* print_romol_vector_type(
        const decl_context_t* decl_context,
        type_t* t,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data)
{
    int num_elements = vector_type_get_num_elements(t);
    if (num_elements == ROMOL_VECTOR_LENGTH)
    {
        return "valib_vector_t";
    }

    const char* typename = get_simple_type_name_string_internal_impl(decl_context,
            vector_type_get_element_type(t),
            print_symbol_fun,
            print_symbol_data);
    const char *c = NULL;
    uniquestr_sprintf(&c, "<<romol-vector-%s-%d>>",
            typename,
            num_elements);
    return c;
}

// Arrays 'vector_flavors' and 'print_vector_functions' are parallel arrays
#define VECTOR_FLAVORS \
    VECTOR_FLAVOR(gnu, print_gnu_vector_type, NULL) \
    VECTOR_FLAVOR(intel, print_intel_sse_avx_vector_type, print_intel_mask_type) \
    VECTOR_FLAVOR(altivec, print_altivec_vector_type, NULL) \
    VECTOR_FLAVOR(opencl, print_opencl_vector_type, NULL) \
    VECTOR_FLAVOR(neon, print_neon_vector_type, NULL) \
    VECTOR_FLAVOR(romol, print_romol_vector_type, print_romol_mask_type)

#define VECTOR_FLAVOR(name, _, __) #name,
const char* vector_flavors[] = {
    VECTOR_FLAVORS
    NULL
};
#undef VECTOR_FLAVOR

#define VECTOR_FLAVOR(_, function, __) function,
const print_vector_type_fun print_vector_type_functions[] = {
    VECTOR_FLAVORS
    NULL
};
#undef VECTOR_FLAVOR

extern inline const char* print_intel_mask_type(
        const decl_context_t* decl_context UNUSED_PARAMETER,
        type_t* t,
        print_symbol_callback_t print_symbol_fun UNUSED_PARAMETER,
        void* print_symbol_data UNUSED_PARAMETER)
{
    unsigned int num_bits = mask_type_get_num_bits(t);

    const char* result = NULL;

    switch (num_bits)
    {
        // We only support Intel MIC mask at the moment:
        case 16:
            {
                result = "__mmask16";
                break;
            }
        case 8:
            {
                result = "__mmask8";
                break;
            }
        default:
            {
                uniquestr_sprintf(&result, "<<intel-vector-mask-%d>>", num_bits);
                break;
            }
    }

    return result;
}

extern inline const char* print_romol_mask_type(
        const decl_context_t* decl_context UNUSED_PARAMETER,
        type_t* t,
        print_symbol_callback_t print_symbol_fun UNUSED_PARAMETER,
        void* print_symbol_data UNUSED_PARAMETER)
{
    unsigned int num_bits = mask_type_get_num_bits(t);

    if (num_bits == ROMOL_VECTOR_LENGTH)
        return "valib_mask_t";

    const char* result = NULL;
    uniquestr_sprintf(&result, "<<romol-vector-mask-%d>>", num_bits);

    return result;
}

#define VECTOR_FLAVOR(_, __, mask_function) mask_function,
// Note that we use print_vector_type_fun as well
const print_vector_type_fun print_mask_type_functions[] = {
    VECTOR_FLAVORS
    NULL
};
#undef VECTOR_FLAVOR

extern inline void vector_types_set_flavor(const char* c)
{
    int i;
    for (i = 0; vector_flavors[i] != NULL; i++)
    {
        if (strcmp(vector_flavors[i], c) == 0)
        {
            CURRENT_CONFIGURATION->print_vector_type = print_vector_type_functions[i];
            CURRENT_CONFIGURATION->print_mask_type = print_mask_type_functions[i];
            break;
        }
    }
}

extern inline const char* vector_types_get_vector_flavor(void)
{
    int i;
    for (i = 0; vector_flavors[i] != NULL; i++)
    {
        if (CURRENT_CONFIGURATION->print_vector_type == print_vector_type_functions[i])
        {
            return vector_flavors[i];
        }
    }
    return NULL;
}

// Returns a string with the name of this simple type
static const char* get_simple_type_name_string_internal_impl(const decl_context_t* decl_context, 
        type_t* t,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data
        )
{
    simple_type_t* simple_type = t->type;
    ERROR_CONDITION(simple_type == NULL, "This cannot be null", 0);

    const char* result = "";

    if (t->info->is_atomic_type)
    {
        result = strappend(result, "_Atomic ");
    }

    switch ((int)simple_type->kind)
    {
        case STK_INDIRECT :
            {
                scope_entry_t* entry = simple_type->user_defined_type;

                result = print_symbol_fun(entry, decl_context, print_symbol_data);
                break;
            }
        case STK_TYPEOF :
            {
                if (IS_C_LANGUAGE
                        || !simple_type->is_decltype)
                {
                    result = strappend(result, "__typeof__(");
                    result = strappend(result, codegen_to_str(simple_type->typeof_expr, decl_context));
                    result = strappend(result, ")");
                }
                else 
                {
                    if (IS_CXX03_LANGUAGE)
                        result = strappend(result, "__decltype(");
                    else
                        result = strappend(result, "decltype(");
                    result = strappend(result, codegen_to_str(simple_type->typeof_expr, decl_context));
                    result = strappend(result, ")");
                }
                break;
            }
        case STK_UNDERLYING:
            {
                result = strappend(result, "__underlying_type(");
                result = strappend(result, print_type_str(simple_type->underlying_type, decl_context));
                result = strappend(result, ")");
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
                    result = strappend(result, "unsigned ");
                }
                else if (simple_type->is_signed
                        && (simple_type->builtin_type != BT_INT))
                {
                    // We emit this only for char
                    result = strappend(result, "signed ");
                }

                if (simple_type->is_long == 1)
                {
                    result = strappend(result, "long ");
                }
                else if (simple_type->is_long == 2)
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
                            if (simple_type->is_long == 3)
                            {
                                result = strappend(result, "__int128");
                            }
                            else
                            {
                                result = strappend(result, "int");
                            }
                            break;
                        }
                    case BT_CHAR :
                        {
                            result = strappend(result, "char");
                            break;
                        }
                    case BT_CHAR16_T :
                        {
                            result = strappend(result, "char16_t");
                            break;
                        }
                    case BT_CHAR32_T :
                        {
                            result = strappend(result, "char32_t");
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
                    case BT_OTHER_FLOAT:
                        {
                            if (simple_type->floating_info->bits == 16)
                            {
                                result = strappend(result, "half");
                            }
                            else if (simple_type->floating_info->bits == 128)
                            {
                                result = strappend(result, "__float128");
                            }
                            else
                            {
                                const char* current;
                                uniquestr_sprintf(&current, "<<unknown-float-type-%d-bits>>", (int)simple_type->floating_info->bits);

                                result = strappend(result, current);
                            }
                            break;
                        }
                    case BT_BOOL :
                        {
                            type_t* integer_type_of_bool = get_integral_type_of_bool(t);
                            if (IS_CXX_LANGUAGE)
                            {
                                result = strappend(result, "bool");
                            }
                            else if (IS_C_LANGUAGE
                                    || (IS_FORTRAN_LANGUAGE && t == NULL))
                            {
                                result = strappend(result, "_Bool");
                            }
                            else if (IS_FORTRAN_LANGUAGE
                                    && t != NULL)
                            {
                                // Use integers based on the kind
                                result = get_simple_type_name_string_internal_impl(decl_context,
                                        integer_type_of_bool,
                                        print_symbol_fun,
                                        print_symbol_data);
                            }
                            else
                            {
                                internal_error("code unreachable", 0);
                            }

                            break;
                        }
                    case BT_VOID :
                        {
                            result = strappend(result, "void");
                            break;
                        }
                    case BT_NULLPTR_T :
                        {
                            result = strappend(result, "decltype(nullptr)");
                            break;
                        }
                    case BT_BYTE :
                        {
                            result = strappend(result, "@byte@");
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
                if (is_float128_type(simple_type->complex_element))
                {
                    // _Complex __float128 is not valid
                    result = "__attribute__((mode(TC))) _Complex float";
                }
                else
                {
                    result = strappend(result, "_Complex ");
                    result = strappend(result,
                            get_simple_type_name_string_internal(decl_context, simple_type->complex_element, print_symbol_fun, print_symbol_data));
                }
                break;
            }
        case STK_VECTOR:
            {
                result = CURRENT_CONFIGURATION->print_vector_type(decl_context, t, print_symbol_fun, print_symbol_data);
                break;
            }
        case STK_MASK:
            {
                if (CURRENT_CONFIGURATION->print_mask_type == NULL)
                {
                    // FIXME: Devise a better fallback
                    print_intel_mask_type(decl_context, t, print_symbol_fun, print_symbol_data);
                }
                else
                {
                    result = CURRENT_CONFIGURATION->print_mask_type(decl_context, t, print_symbol_fun, print_symbol_data);
                }
                break;
            }
        case STK_CLASS :
            {
                result = UNIQUESTR_LITERAL("class <anonymous>");
                break;
            }
        case STK_ENUM :
            {
                result = UNIQUESTR_LITERAL("enum <anonymous>");
                break;
            }
        case STK_TEMPLATE_DEPENDENT_TYPE :
            {
                char is_dependent = 0;
                int max_qualif_level = 0;

                char is_local_typename = 0;
                // Local typenames start from a function so the function cannot
                // be emitted actually. 'typename' (or other elaborated type
                // prefix) cannot be emitted either
                if (simple_type->dependent_entry->kind == SK_DECLTYPE)
                {
                    result = get_simple_type_name_string_internal(
                            decl_context,
                            simple_type->dependent_entry->type_information,
                            print_symbol_fun,
                            print_symbol_data);

                    is_dependent =
                        is_dependent_type(simple_type->dependent_entry->type_information);

                }
                else if (simple_type->dependent_entry->kind != SK_FUNCTION)
                {
                    result = get_fully_qualified_symbol_name(simple_type->dependent_entry,
                            decl_context,
                            &is_dependent,
                            &max_qualif_level);
                }
                else
                {
                    is_local_typename = 1;
                }

                nodecl_t  nodecl_parts = simple_type->dependent_parts;

                if (is_dependent
                        && !is_local_typename
                        && !nodecl_is_null(nodecl_parts))
                {
                    enum type_tag_t kind = get_dependent_entry_kind(t);
                    switch(kind)
                    {
                        case TT_TYPENAME:
                            {
                                result = strappend("typename ", result);
                                break;
                            }
                        case TT_STRUCT:
                            {
                                result = strappend("struct ", result);
                                break;
                            }
                        case TT_CLASS:
                            {
                                result = strappend("class ", result);
                                break;
                            }
                        case TT_UNION:
                            {
                                result = strappend("union ", result);
                                break;
                            }
                        case TT_ENUM:
                            {
                                result = strappend("enum ", result);
                                break;
                            }
                        default:
                            {
                                internal_error("code unreachable.", 0);
                            }
                    }
                }

                if (!nodecl_is_null(nodecl_parts))
                {
                    int num_parts = 0;
                    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(nodecl_parts, 0), &num_parts);

                    int i;
                    for (i = 0; i < num_parts; i++)
                    {
                        nodecl_t current_part = list[i];

                        nodecl_t simple_current_part = current_part;
                        template_parameter_list_t* template_parameters = NULL;

                        if (nodecl_get_kind(current_part) == NODECL_CXX_DEP_TEMPLATE_ID)
                        {
                            template_parameters = nodecl_get_template_parameters(current_part);
                            simple_current_part = nodecl_get_child(current_part, 0);
                        }

                        const char* name = nodecl_get_text(simple_current_part);

                        if (!is_local_typename)
                            result = strappend(result, "::");

                        if (template_parameters != NULL && is_dependent)
                        {
                            result = strappend(result, "template ");
                        }
                        result = strappend(result, name);

                        if (template_parameters != NULL)
                        {
                            result = strappend(result, "< ");
                            int j;
                            for (j = 0; j < template_parameters->num_parameters; j++)
                            {
                                template_parameter_value_t * template_arg = template_parameters->arguments[j];

                                switch (template_arg->kind)
                                {
                                    case TPK_TYPE:
                                        {
                                            result = strappend(result, 
                                                    print_type_str(template_arg->type, decl_context));
                                            break;
                                        }
                                    case TPK_NONTYPE:
                                        {
                                            result = strappend(result, 
                                                    codegen_to_str(template_arg->value, decl_context));
                                            break;
                                        }
                                    case TPK_TEMPLATE:
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

                                if ((j + 1) < template_parameters->num_parameters)
                                {
                                    result = strappend(result, ", ");
                                }
                            }

                            if (result[strlen(result) - 1] == '>')
                            {
                                result = strappend(result, " >");
                            }
                            else
                            {
                                result = strappend(result, ">");
                            }
                        }

                        // At this moment we do not allow nesting of types in
                        // local dependent typenames
                        if (is_local_typename)
                            break;
                    }

                    DELETE(list);
                }

                break;
            }
        case STK_TEMPLATE_TYPE:
            {
                result = get_simple_type_name_string_internal(decl_context, simple_type->primary_specialization, 
                        print_symbol_fun, print_symbol_data);
                break;
            }
        case STK_TYPE_DEP_EXPR:
            {
                result = "< type dependent expression type >";
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
static const char* get_simple_type_name_string_internal(const decl_context_t* decl_context,
        type_t* type_info,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data)
{
    const char* result = "";

    if (type_info == NULL)
        return result;

    if (is_unresolved_overloaded_type(type_info))
    {
        result = UNIQUESTR_LITERAL("<unresolved overloaded function type>");
    }
    else if (is_error_type(type_info))
    {
        result = UNIQUESTR_LITERAL("<error-type>");
    }
    else if (is_sequence_of_types(type_info))
    {
        if (debug_options.show_template_packs)
        {
            result = strappend(result, " /* { */ ");
        }

        int i;
        for (i = 0; i < sequence_of_types_get_num_types(type_info); i++)
        {
            if (i > 0)
                result = strappend(result, ", ");

            result = strappend(result,
                    get_declaration_string_ex(
                        sequence_of_types_get_type_num(type_info, i),
                        decl_context, "", "",
                        0, 0, NULL, NULL, 0,
                        /* unparenthesize_ptr_operator */ 0,
                        print_symbol_fun, print_symbol_data));
        }

        if (debug_options.show_template_packs)
        {
            result = strappend(result, " /* } */ ");
        }
    }
    else if (is_auto_type(type_info))
    {
        result = UNIQUESTR_LITERAL("auto");
    }
    else if (is_decltype_auto_type(type_info))
    {
        result = UNIQUESTR_LITERAL("decltype(auto)");
    }
    else if (is_braced_list_type(type_info))
    {
        result = "<braced-initializer-list-type>";
    }
    else if (is_ellipsis_type(type_info))
    {
        result = "<ellipsis-type>";
    }
    else
    {
        result = get_cv_qualifier_string(type_info);
        result = strappend(result,
                get_simple_type_name_string_internal_impl(decl_context, type_info, print_symbol_fun, print_symbol_data));
    }

    return result;
}

static const char* get_type_name_string_internal(const decl_context_t* decl_context,
        type_t* type_info,
        const char* symbol_name,
        int num_parameter_names,
        const char** parameter_names,
        const char** parameter_attributes,
        char is_parameter,
        char unparenthesize_ptr_operator,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data);

extern inline const char* get_declarator_name_string_ex(const decl_context_t* decl_context,
        type_t* type_info,
        const char* symbol_name,
        int num_parameter_names,
        const char** parameter_names,
        const char** parameter_attributes,
        char is_parameter,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data)
{
    return get_type_name_string_internal( decl_context,
            type_info,
            symbol_name,
            num_parameter_names,
            parameter_names,
            parameter_attributes,
            is_parameter,
            /* unparenthesize_ptr_operator */ 0,
            print_symbol_fun,
            print_symbol_data);
}

static type_t* get_foundation_type(type_t* t);

extern inline const char* get_declaration_string_ex(type_t* type_info,
        const decl_context_t* decl_context,
        const char* symbol_name, const char* initializer,
        char semicolon,
        int num_parameter_names,
        const char** parameter_names,
        const char** parameter_attributes,
        char is_parameter,
        char unparenthesize_ptr_operator,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);

    while (is_indirect_type(type_info)
            // Advance indirects unless they refer to existing typedefs
            // or template-aliases
            && type_info->type->user_defined_type->kind != SK_TYPEDEF
            && type_info->type->user_defined_type->kind != SK_TEMPLATE_ALIAS)
    {
        type_info = type_info->type->user_defined_type->type_information;
    }

    type_t* base_type = get_foundation_type(type_info);

    const char* base_type_name =
        get_simple_type_name_string_internal(decl_context, base_type, print_symbol_fun, print_symbol_data);

    const char* declarator_name =
        get_type_name_string_internal(decl_context, type_info, symbol_name,
                num_parameter_names, parameter_names, parameter_attributes,
                is_parameter, unparenthesize_ptr_operator, print_symbol_fun,
                print_symbol_data);

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

// Returns a declaration string given a type, a symbol name, an optional
// initializer and a semicolon. For function types you can specify the names of
// the arguments
extern inline const char* get_declaration_string(type_t* type_info,
        const decl_context_t* decl_context,
        const char* symbol_name, const char* initializer,
        char semicolon,
        int num_parameter_names,
        const char** parameter_names,
        const char** parameter_attributes,
        char is_parameter)
{
    return get_declaration_string_ex(type_info,
        decl_context,
        symbol_name, initializer,
        semicolon,
        num_parameter_names,
        parameter_names,
        parameter_attributes,
        is_parameter,
        /* unparenthesize_ptr_operator */ 0,
        get_simple_type_name_string_internal_common,
        NULL
        );
}

static void get_type_name_string_internal_impl(const decl_context_t* decl_context,
        type_t* type_info,
        const char** left,
        const char** right,
        int num_parameter_names,
        const char** parameter_names,
        const char** parameter_attributes,
        char is_parameter,
        char unparenthesize_ptr_operator,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data);

static const char* get_type_name_string_internal(const decl_context_t* decl_context,
        type_t* type_info,
        const char* symbol_name,
        int num_parameter_names,
        const char** parameter_names,
        const char** parameter_attributes,
        char is_parameter,
        char unparenthesize_ptr_operator,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);

    const char* left = "";
    const char* right = "";
    get_type_name_string_internal_impl(decl_context, type_info, &left, &right,
            num_parameter_names,
            parameter_names,
            parameter_attributes,
            is_parameter,
            unparenthesize_ptr_operator,
            print_symbol_fun,
            print_symbol_data);

    const char* result = strappend(left, symbol_name);
    result = strappend(result, right);

    return result;
}

extern inline char is_unqualified_type(type_t* t1)
{
    cv_qualifier_t cv1 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(t1, &cv1);

    return (cv1 == CV_NONE);
}

extern inline char is_const_qualified_type(type_t* t1)
{
    cv_qualifier_t cv1 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(t1, &cv1);

    return ((cv1 & CV_CONST) == CV_CONST);
}

extern inline char is_volatile_qualified_type(type_t* t1)
{
    cv_qualifier_t cv1 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(t1, &cv1);

    return ((cv1 & CV_VOLATILE) == CV_VOLATILE);
}

extern inline char is_restrict_qualified_type(type_t* t1)
{
    cv_qualifier_t cv1 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(t1, &cv1);

    return ((cv1 & CV_RESTRICT) == CV_RESTRICT);
}

extern inline char is_const_qualified(cv_qualifier_t cv)
{
    return ((cv & CV_CONST) == CV_CONST);
}

extern inline char is_volatile_qualified(cv_qualifier_t cv)
{
    return ((cv & CV_VOLATILE) == CV_VOLATILE);
}

extern inline char is_restrict_qualified(cv_qualifier_t cv)
{
    return ((cv & CV_RESTRICT) == CV_RESTRICT);
}

extern inline char is_less_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2)
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

extern inline char is_equal_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
    return (cv1 == cv2);
}

extern inline char is_less_or_equal_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
    return is_less_cv_qualified(cv1, cv2)
        || is_equal_cv_qualified(cv1, cv2);
}

extern inline char is_more_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
    return !is_less_or_equal_cv_qualified(cv1, cv2);
}

extern inline char is_more_or_equal_cv_qualified(cv_qualifier_t cv1, cv_qualifier_t cv2)
{
    return !is_less_cv_qualified(cv1, cv2);
}

extern inline char is_less_cv_qualified_type(type_t* t1, type_t* t2)
{
    cv_qualifier_t cv1 = CV_NONE;
    cv_qualifier_t cv2 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(t1, &cv1);
    advance_over_typedefs_with_cv_qualif(t2, &cv2);

    return is_less_cv_qualified(cv1, cv2);
}

extern inline char is_equally_cv_qualified_type(type_t* t1, type_t* t2)
{
    cv_qualifier_t cv1 = CV_NONE;
    cv_qualifier_t cv2 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(t1, &cv1);
    advance_over_typedefs_with_cv_qualif(t2, &cv2);

    return is_equal_cv_qualified(cv1, cv2);
}

extern inline char is_less_or_equal_cv_qualified_type(type_t* t1, type_t* t2)
{
    return (is_less_cv_qualified_type(t1, t2)
            || is_equally_cv_qualified_type(t1, t2));
}

extern inline char is_more_cv_qualified_type(type_t* t1, type_t* t2)
{
    return !is_less_or_equal_cv_qualified_type(t1, t2);
}

extern inline char is_more_or_equal_cv_qualified_type(type_t* t1, type_t* t2)
{
    return !is_less_cv_qualified_type(t1, t2);
}

// Constructs a proper declarator
static void get_type_name_string_internal_impl(const decl_context_t* decl_context,
        type_t* type_info,
        const char** left,
        const char** right,
        int num_parameter_names,
        const char** parameter_names,
        const char** parameter_attributes,
        char is_parameter,
        char unparenthesize_ptr_operator,
        print_symbol_callback_t print_symbol_fun,
        void* print_symbol_data)
{
    ERROR_CONDITION(type_info == NULL, "This cannot be null", 0);

    char need_extra_parentheses = 0;
    // These types have specific syntax for them
    if (type_info->kind != TK_POINTER
            && type_info->kind != TK_POINTER_TO_MEMBER)
    {
        int num_attrs = 0;
        gcc_attribute_t *gcc_attrs = NULL;
        variant_type_get_gcc_attributes(type_info, &num_attrs, &gcc_attrs);

        if (num_attrs > 0)
        {
            need_extra_parentheses = 1;
            (*left) = strappend((*left), "(");
            (*left) = strappend((*left), get_gcc_attributes_string(type_info));
        }
    }

    switch (type_info->kind)
    {
        case TK_DIRECT :
            {
                if (is_named_type(type_info)
                        && named_type_get_symbol(type_info)->kind == SK_TYPEDEF
                        && symbol_entity_specs_get_is_template_parameter(named_type_get_symbol(type_info)))
                {
                    get_type_name_string_internal_impl(decl_context,
                            named_type_get_symbol(type_info)->type_information,
                            left,
                            right,
                            num_parameter_names,
                            parameter_names,
                            parameter_attributes,
                            is_parameter,
                            unparenthesize_ptr_operator,
                            print_symbol_fun,
                            print_symbol_data);
                }
                break;
            }
        case TK_POINTER :
            {
                get_type_name_string_internal_impl(decl_context, type_info->pointer->pointee, left, right,
                        num_parameter_names, parameter_names, parameter_attributes, is_parameter,
                        unparenthesize_ptr_operator, print_symbol_fun, print_symbol_data);

                char needs_parentheses = !unparenthesize_ptr_operator && declarator_needs_parentheses(type_info);
                if (needs_parentheses)
                {
                    (*left) = strappend((*left), "(");
                }

                (*left) = strappend((*left), "*");
                (*left) = strappend((*left), get_cv_qualifier_string(type_info));
                (*left) = strappend((*left), get_gcc_attributes_string(type_info));

                if (needs_parentheses)
                {
                    (*right) = strappend(")", (*right));
                }

                break;
            }
        case TK_POINTER_TO_MEMBER :
            {
                get_type_name_string_internal_impl(decl_context, type_info->pointer->pointee, left, right,
                        num_parameter_names, parameter_names, parameter_attributes, is_parameter,
                        unparenthesize_ptr_operator, print_symbol_fun, print_symbol_data);

                const char* class_name =
                        get_qualified_symbol_name(
                                named_type_get_symbol(type_info->pointer->pointee_class_type),
                                decl_context);

                char needs_parentheses = !unparenthesize_ptr_operator
                    && (declarator_needs_parentheses(type_info)
                            || (class_name != NULL && class_name[0] == ':'));

                if (needs_parentheses)
                {
                    (*left) = strappend((*left), "(");
                }

                (*left) = strappend((*left), class_name);

                (*left) = strappend((*left), "::");
                (*left) = strappend((*left), "*");
                (*left) = strappend((*left), get_cv_qualifier_string(type_info));
                (*left) = strappend((*left), get_gcc_attributes_string(type_info));

                if (needs_parentheses)
                {
                    (*right) = strappend(")", (*right));
                }

                break;
            }
        case TK_RVALUE_REFERENCE :
        case TK_LVALUE_REFERENCE :
            {
                get_type_name_string_internal_impl(decl_context, type_info->pointer->pointee, left, right,
                        num_parameter_names, parameter_names, parameter_attributes, is_parameter,
                        unparenthesize_ptr_operator, print_symbol_fun, print_symbol_data);

                char needs_parentheses = !unparenthesize_ptr_operator && declarator_needs_parentheses(type_info);

                if (needs_parentheses)
                {
                    (*left) = strappend((*left), "(");
                }
                if (type_info->kind == TK_LVALUE_REFERENCE)
                {
                    if (IS_CXX_LANGUAGE)
                    {
                        (*left) = strappend((*left), "&");
                    }
                    else
                    {
                        (*left) = strappend((*left), "@ref@");
                    }
                }
                else
                {
                    (*left) = strappend((*left), "&&");
                }

                if (needs_parentheses)
                {
                    (*right) = strappend(")", (*right));
                }

                break;
            }
        case TK_ARRAY :
            {
                get_type_name_string_internal_impl(decl_context, type_info->array->element_type, left, right,
                        num_parameter_names, parameter_names, parameter_attributes, is_parameter,
                        unparenthesize_ptr_operator, print_symbol_fun, print_symbol_data);

                const char* whole_size = NULL;
                if (is_parameter)
                {
                    if (nodecl_is_null(type_info->array->whole_size))
                    {
                        whole_size = UNIQUESTR_LITERAL("[]");
                    }
                    // If this is a saved expression and it IS a parameter we use its saved expression instead
                    else if (nodecl_get_kind(type_info->array->whole_size) == NODECL_SYMBOL
                            && symbol_entity_specs_get_is_saved_expression(
                                nodecl_get_symbol(type_info->array->whole_size)))
                    {
                        scope_entry_t* saved_expr = nodecl_get_symbol(type_info->array->whole_size);
                        const char* whole_size_str = uniquestr(codegen_to_str(saved_expr->value, decl_context));

                        whole_size = strappend("[", whole_size_str);
                        whole_size = strappend(whole_size, "]");
                    }
                    else
                    {
                        const char* whole_size_str = uniquestr(codegen_to_str(type_info->array->whole_size, decl_context));

                        whole_size = strappend("[", whole_size_str);
                        whole_size = strappend(whole_size, "]");
                    }
                }
                else
                {
                    if (nodecl_is_null(type_info->array->whole_size))
                    {
                        whole_size = UNIQUESTR_LITERAL("[]");
                    }
                    // A saved expression that is not user declared means that we have to ignore it
                    // when printing it
                    else if (nodecl_get_kind(type_info->array->whole_size) == NODECL_SYMBOL
                            && symbol_entity_specs_get_is_saved_expression(
                                nodecl_get_symbol(type_info->array->whole_size))
                            && !symbol_entity_specs_get_is_user_declared(
                                nodecl_get_symbol(type_info->array->whole_size)))
                    {
                        scope_entry_t* saved_expr = nodecl_get_symbol(type_info->array->whole_size);
                        const char* whole_size_str = uniquestr(codegen_to_str(saved_expr->value, decl_context));

                        whole_size = strappend("[", whole_size_str);
                        whole_size = strappend(whole_size, "]");
                    }
                    else
                    {
                        const char* whole_size_str = uniquestr(codegen_to_str(type_info->array->whole_size, decl_context));

                        whole_size = strappend("[", whole_size_str);
                        whole_size = strappend(whole_size, "]");
                    }
                }

                (*right) = strappend(whole_size, (*right));
                break;
            }
        case TK_FUNCTION :
            {
                if (!type_info->function->is_trailing
                        && type_info->function->return_type != NULL)
                {
                    get_type_name_string_internal_impl(decl_context, type_info->function->return_type, left, right,
                            0, NULL, NULL, 0, unparenthesize_ptr_operator, print_symbol_fun, print_symbol_data);
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
                        // Prefer the nonadjusted type
                        type_t* parameter_type = type_info->function->parameter_list[i]->nonadjusted_type_info;
                        if (parameter_type == NULL)
                        {
                            // but sometimes it is not known
                            parameter_type = type_info->function->parameter_list[i]->type_info;
                        }

                        if (parameter_names == NULL
                                || (i >= num_parameter_names)
                                || parameter_names[i] == NULL)
                        {
                            // Abstract declarator
                            if (!type_info->function->lacks_prototype)
                            {
                                prototype = strappend(prototype,
                                        get_declaration_string_ex(parameter_type, decl_context,
                                            "", "", 0, 0, NULL, NULL, 1, unparenthesize_ptr_operator,
                                            print_symbol_fun, print_symbol_data));
                            }
                            else
                            {
                                // We create a name
                                char parameter_name[20];
                                snprintf(parameter_name, 19, "_p_%d", i);
                                parameter_name[19] = '\0';

                                prototype = strappend(prototype, parameter_name);
                            }
                        }
                        else if (parameter_names != NULL
                                && parameter_names[i] != NULL)
                        {
                            if (!type_info->function->lacks_prototype)
                            {
                                prototype = strappend(prototype,
                                        get_declaration_string_ex(parameter_type, decl_context,
                                            parameter_names[i], "", 0, 0, NULL, NULL, 1,
                                            unparenthesize_ptr_operator, print_symbol_fun, print_symbol_data));
                            }
                            else
                            {
                                prototype = strappend(prototype, parameter_names[i]);
                            }
                        }
                        else // parameter_names != NULL && parameter_names[i] == NULL
                        {
                            // We create a name
                            char parameter_name[20];
                            snprintf(parameter_name, 19, "_p_%d", i);
                            parameter_name[19] = '\0';

                            if (!type_info->function->lacks_prototype)
                            {
                                parameter_names[i] = uniquestr(parameter_name);

                                prototype = strappend(prototype,
                                        get_declaration_string_ex(parameter_type, decl_context,
                                            parameter_name, "", 0, 0, NULL, NULL, 1, unparenthesize_ptr_operator,
                                            print_symbol_fun, print_symbol_data));
                            }
                            else
                            {
                                prototype = strappend(prototype, parameter_name);
                            }
                        }
                    }

                    // Add the parameter attributes
                    if (parameter_attributes != NULL
                            && i < num_parameter_names
                            && parameter_attributes[i] != NULL
                            && (strlen(parameter_attributes[i]) > 0))
                    {
                        prototype = strappend(prototype, " ");
                        prototype = strappend(prototype, parameter_attributes[i]);
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

                ref_qualifier_t ref_qualif = function_type_get_ref_qualifier(type_info);
                switch (ref_qualif)
                {
                    case REF_QUALIFIER_NONE:
                        break;
                    case REF_QUALIFIER_LVALUE:
                        prototype = strappend(prototype, " &");
                        break;
                    case REF_QUALIFIER_RVALUE:
                        prototype = strappend(prototype, " &&");
                        break;
                    default:
                        internal_error("Invalid value %d ref-qualifier\n", ref_qualif);
                }

                if (type_info->function->is_trailing
                        && type_info->function->return_type != NULL)
                {
                    prototype = strappend(prototype, " -> ");

                    const char *trailing_type = get_declaration_string_ex(
                            type_info->function->return_type,
                            decl_context,
                            "", "", 0,
                            0, NULL, NULL,
                            0,
                            unparenthesize_ptr_operator,
                            print_symbol_fun, print_symbol_data);

                    prototype = strappend(prototype, trailing_type);
                }
                (*right) = strappend(prototype, (*right));
                break;
            }
        case TK_PACK:
            {
                get_type_name_string_internal_impl(decl_context, type_info->pack_type->packed, left, right,
                        num_parameter_names, parameter_names, parameter_attributes, is_parameter,
                        unparenthesize_ptr_operator, print_symbol_fun, print_symbol_data);

                if (declarator_needs_parentheses(type_info))
                {
                    (*left) = strappend((*left), "(");
                }

                (*left) = strappend((*left), "...");

                if (declarator_needs_parentheses(type_info))
                {
                    (*right) = strappend(")", (*right));
                }
                break;
            }
        case TK_OVERLOAD:
        case TK_ERROR:
        case TK_SEQUENCE:
        case TK_AUTO:
        case TK_DECLTYPE_AUTO:
        case TK_BRACED_LIST:
            {
                break;
            }
        default:
            {
                fprintf(stderr, "Unknown type kind '%d'\n", (int)type_info->kind);
                break;
            }
    }

    if (need_extra_parentheses)
    {
        (*right) = strappend(")", (*right));
    }
}

/** 
 * Debugging functions
 * **/

static
const char *get_named_simple_type_name(scope_entry_t* user_defined_type)
{
    ERROR_CONDITION(user_defined_type == NULL, "This cannot be null", 0);

    const char* result = UNIQUESTR_LITERAL("");

    const int MAX_LENGTH = 1023;
    char* user_defined_str = NEW_VEC0(char, MAX_LENGTH);

    switch (user_defined_type->kind)
    {
        case SK_ENUM :
            {
                snprintf(user_defined_str, MAX_LENGTH, "enum %s {%s}", 
                        get_qualified_symbol_name(user_defined_type, user_defined_type->decl_context),
                        locus_to_str(user_defined_type->locus));
                break;
            }
        case SK_CLASS :
            {
                snprintf(user_defined_str, MAX_LENGTH, "class %s {%s}", 
                        get_qualified_symbol_name(user_defined_type, user_defined_type->decl_context),
                        locus_to_str(user_defined_type->locus));
                break;
            }
        case SK_TEMPLATE_ALIAS :
            {
                snprintf(user_defined_str, MAX_LENGTH, "template-alias %s {%s}", 
                        get_qualified_symbol_name(user_defined_type, user_defined_type->decl_context),
                        locus_to_str(user_defined_type->locus));
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
            snprintf(user_defined_str, MAX_LENGTH, "<type-template parameter '%s' (%d,%d) %s>",
                    user_defined_type->symbol_name,
                    symbol_entity_specs_get_template_parameter_nesting(user_defined_type),
                    symbol_entity_specs_get_template_parameter_position(user_defined_type),
                    locus_to_str(user_defined_type->locus)
                    );
            break;
        case SK_TEMPLATE_TYPE_PARAMETER_PACK :
            snprintf(user_defined_str, MAX_LENGTH, "<type-template parameter pack '%s' (%d,%d) %s>",
                    user_defined_type->symbol_name,
                    symbol_entity_specs_get_template_parameter_nesting(user_defined_type),
                    symbol_entity_specs_get_template_parameter_position(user_defined_type),
                    locus_to_str(user_defined_type->locus)
                    );
            break;
        case SK_TEMPLATE_TEMPLATE_PARAMETER :
            snprintf(user_defined_str, MAX_LENGTH, "<template-template parameter '%s' (%d,%d) %s>",
                    user_defined_type->symbol_name,
                    symbol_entity_specs_get_template_parameter_nesting(user_defined_type),
                    symbol_entity_specs_get_template_parameter_position(user_defined_type),
                    locus_to_str(user_defined_type->locus)
                    );
            break;
        case SK_TEMPLATE_TEMPLATE_PARAMETER_PACK :
            snprintf(user_defined_str, MAX_LENGTH, "<template-template parameter pack '%s' (%d,%d) %s>",
                    user_defined_type->symbol_name,
                    symbol_entity_specs_get_template_parameter_nesting(user_defined_type),
                    symbol_entity_specs_get_template_parameter_position(user_defined_type),
                    locus_to_str(user_defined_type->locus)
                    );
            break;
        case SK_TEMPLATE_NONTYPE_PARAMETER :
            snprintf(user_defined_str, MAX_LENGTH, "<nontype-template parameter '%s' (%d,%d) %s>", 
                    user_defined_type->symbol_name,
                    symbol_entity_specs_get_template_parameter_nesting(user_defined_type),
                    symbol_entity_specs_get_template_parameter_position(user_defined_type),
                    locus_to_str(user_defined_type->locus)
                    );
            break;
        case SK_TEMPLATE_NONTYPE_PARAMETER_PACK :
            snprintf(user_defined_str, MAX_LENGTH, "<nontype-template parameter pack '%s' (%d,%d) %s>", 
                    user_defined_type->symbol_name,
                    symbol_entity_specs_get_template_parameter_nesting(user_defined_type),
                    symbol_entity_specs_get_template_parameter_position(user_defined_type),
                    locus_to_str(user_defined_type->locus)
                    );
            break;
        case SK_TEMPLATE :
            snprintf(user_defined_str, MAX_LENGTH, "<template-name '%s'>", 
                    user_defined_type->symbol_name);
            break;
        case SK_GCC_BUILTIN_TYPE :
            snprintf(user_defined_str, MAX_LENGTH, "__builtin_va_list");
            break;
        case SK_DEPENDENT_ENTITY :
            snprintf(user_defined_str, MAX_LENGTH, "<dependent entity>");
            break;
        default :
            snprintf(user_defined_str, MAX_LENGTH, "<<<unknown user defined type %s>>>", symbol_kind_name(user_defined_type));
    }
    result = strappend(result, user_defined_str);

    return result;
}


// Prints the template arguments, this routine is for debugging
static const char* get_template_parameters_list_str(template_parameter_list_t* template_parameters)
{
    const char* result = "";
    result = strappend(result, "< ");
    int i;
    for (i = 0; i < template_parameters->num_parameters; i++)
    {
        template_parameter_value_t* template_parameter = 
            template_parameters->arguments[i];

        const char* c;
        uniquestr_sprintf(&c, "[[%d, %d]] ", 
                get_template_nesting_of_template_parameters(template_parameters),
                i);

        result = strappend(result, c);

        switch (template_parameter->kind)
        {
            case TPK_TYPE:
            case TPK_TEMPLATE:
                {
                    result = strappend(result,
                            print_declarator(template_parameter->type));
                    break;
                }
            case TPK_NONTYPE:
                {
                    if (nodecl_is_list(template_parameter->value))
                    {
                        int num_items;
                        nodecl_t* list = nodecl_unpack_list(template_parameter->value, &num_items);

                        int j;
                        for (j = 0; j < num_items; j++)
                        {
                            if (j > 0)
                                result = strappend(result, ", ");

                            result = strappend(result,
                                codegen_to_str(list[j],
                                    CURRENT_COMPILED_FILE->global_decl_context));
                        }

                        DELETE(list);
                    }
                    else
                    {
                        result = strappend(result,
                                codegen_to_str(template_parameter->value,
                                    CURRENT_COMPILED_FILE->global_decl_context));
                    }
                    break;
                }
            default:
                {
                    result = strappend(result,
                            " << unknown template argument >> ");
                    break;
                }
        }
        if ((i + 1) < template_parameters->num_parameters)
        {
            result = strappend(result, ", ");
        }
    }
    result = strappend(result, "> ");

    return result;
}

// Gives the name of a builtin type. This routine is for debugging
static const char* get_builtin_type_name(type_t* type_info)
{
    simple_type_t* simple_type_info = type_info->type;
    ERROR_CONDITION(simple_type_info == NULL, "This cannot be null", 0);
    const char* result = UNIQUESTR_LITERAL("");

    if (type_info->info->is_atomic_type)
    {
        result = strappend(result, "_Atomic ");
    }

    if (simple_type_info->is_long == 1)
    {
        result = strappend(result, "long ");
    }

    if (simple_type_info->is_long == 2)
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
                        if (simple_type_info->is_long == 3)
                        {
                            result = strappend(result, "__int128");
                        }
                        else
                        {
                            result = strappend(result, "int");
                        }
                        break;
                    case BT_BOOL :
                        {

                            // Mark booleans of nonregular size
                            if (type_get_size(type_info) != type_get_size(get_bool_type()))
                            {
                                const char *c;
                                uniquestr_sprintf(&c, "boolean of %zd bytes", (size_t)type_get_size(type_info));
                                result = strappend(result, c);
                            }
                            else
                            {
                                result = strappend(result, "bool");
                            }
                            break;
                        }
                    case BT_FLOAT :
                        result = strappend(result, "float");
                        break;
                    case BT_DOUBLE :
                        result = strappend(result, "double");
                        break;
                    case BT_OTHER_FLOAT:
                        {
                            if (simple_type_info->floating_info->bits == 16)
                            {
                                result = strappend(result, "half");
                            }
                            else if (simple_type_info->floating_info->bits == 128)
                            {
                                result = strappend(result, "__float128");
                            }
                            else
                            {
                                const char* c;
                                uniquestr_sprintf(&c, "<<unknown-float-type-%d-bits>>", (int)simple_type_info->floating_info->bits);

                                result = strappend(result, c);
                            }
                            break;
                        }
                    case BT_WCHAR :
                        result = strappend(result, "wchar_t");
                        break;
                    case BT_CHAR :
                        result = strappend(result, "char");
                        break;
                    case BT_CHAR16_T :
                        result = strappend(result, "char16_t");
                        break;
                    case BT_CHAR32_T :
                        result = strappend(result, "char32_t");
                        break;
                    case BT_BYTE:
                        result = strappend(result, "byte");
                        break;
                    case BT_VOID :
                        result = strappend(result, "void");
                        break;
                    case BT_NULLPTR_T :
                        result = strappend(result, "decltype(nullptr)");
                        break;
                    case BT_UNKNOWN :
                    default :
                        result = strappend(result, "???unknown builtin type???");
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
        case STK_VECTOR:
            {
                const char* c;
                int num_elements = vector_type_get_num_elements(type_info);

                uniquestr_sprintf(&c, "vector (bytes=%d) of %d elements of ", 
                        simple_type_info->vector_size,
                        num_elements);
                result = strappend(result, c);
                result = strappend(result, print_declarator(simple_type_info->vector_element));
                break;
            }
        case STK_INDIRECT :
            result = get_named_simple_type_name(simple_type_info->user_defined_type);
            break;
        case STK_ENUM :
            {
                const char* c;
                uniquestr_sprintf(&c, "enum <anonymous> %p", type_info);
                result = strappend(result, c);
            }
            break;
        case STK_CLASS :
            {
                const char *template_parameters = "";
                {
                    type_t* actual_class = type_info;
                    if (actual_class->info->is_template_specialized_type
                            && actual_class->template_arguments != NULL)
                    {
                        template_parameters = get_template_parameters_list_str(
                                actual_class->template_arguments);
                    }
                }

                const char* c = NULL;
                uniquestr_sprintf(&c, "class <anonymous>%s %p", template_parameters, type_info);
                result = strappend(result, c);
            }
            break;
        case STK_VA_LIST :
            result = strappend(result, "__builtin_va_list");
            break;
        case STK_TYPEOF :
            if (!simple_type_info->is_decltype)
                result = strappend(result, "__typeof__(");
            else
                if (IS_CXX03_LANGUAGE)
                    result = strappend(result, "__decltype(");
                else
                    result = strappend(result, "decltype(");
            result = strappend(result, codegen_to_str(simple_type_info->typeof_expr, CURRENT_COMPILED_FILE->global_decl_context));
            result = strappend(result, ")");
            break;
        case STK_UNDERLYING:
            result = strappend(result, "__underlying_type(");
            result = strappend(result, print_declarator(simple_type_info->underlying_type));
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

                nodecl_t nodecl_parts = simple_type_info->dependent_parts;
                if (!nodecl_is_null(nodecl_parts))
                {
                    int num_items = 0;
                    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(nodecl_parts, 0), &num_items);

                    int i;
                    for (i = 0; i < num_items; i++)
                    {
                        nodecl_t part = list[i];
                        nodecl_t simple_part = part;
                        template_parameter_list_t* template_parameters = NULL;

                        if (nodecl_get_kind(part) == NODECL_CXX_DEP_TEMPLATE_ID)
                        {
                            simple_part = nodecl_get_child(part, 0);
                            template_parameters = nodecl_get_template_parameters(part);
                        }

                        const char* name = nodecl_get_text(simple_part);

                        result = strappend(result, "::");
                        result = strappend(result, name);

                        if (template_parameters != NULL)
                        {
                            result = strappend(result, get_template_parameters_list_str(template_parameters));
                        }
                    }
                    DELETE(list);
                }

                result = strappend(result, ">");
            }
            break;
        case STK_TEMPLATE_TYPE :
            {
                // FIXME - this should be much more informative
                const char* c;
                uniquestr_sprintf(&c, "<template type %p>", 
                        type_info);
                result = strappend(result, c);
                break;
            }
        case STK_TYPE_DEP_EXPR:
            {
                result = strappend(result, "< dependent expression type >");
                break;
            }
        case STK_MASK:
            {
                const char* c;
                uniquestr_sprintf(&c, "mask type of %d bits", 
                        type_info->type->vector_size);
                result = strappend(result, c);
                break;
            }
        default :
            {
                const char* c;
                uniquestr_sprintf(&c, "(unknown simple type = %d)",
                        simple_type_info->kind);
                result = strappend(result, c);
                break;
            }
    }

    return result;
}

static type_t* _dependent_type = NULL;

extern inline type_t* get_unknown_dependent_type(void)
{
    if (_dependent_type == NULL)
    {
        _dependent_type = get_simple_type();
        _dependent_type->type->kind = STK_TYPE_DEP_EXPR;
        _dependent_type->info->is_dependent = 1;
    }
    return _dependent_type;
}

static char is_unknown_dependent_type(type_t* t)
{
    return (_dependent_type != NULL
            && t != NULL
            && (t->unqualified_type == _dependent_type));
}

static const char* print_dimension_of_array(nodecl_t n, const decl_context_t* decl_context)
{
    if (nodecl_is_null(n))
        return "?";
    if (nodecl_get_kind(n) == NODECL_SYMBOL
            && symbol_entity_specs_get_is_saved_expression(nodecl_get_symbol(n)))
    {
        const char* result = NULL;
        uniquestr_sprintf(&result, "%s { alias of %s }",
                nodecl_get_symbol(n)->symbol_name,
                codegen_to_str(nodecl_get_symbol(n)->value, decl_context));

        return result;
    }
    else
    {
        return codegen_to_str(n, decl_context);
    }
}

// This prints a declarator in English. It is intended for debugging purposes
extern inline const char* print_declarator(type_t* printed_declarator)
{
    if (printed_declarator == NULL)
        return "<<NULL>>";

    const char* tmp_result = "";

    if (is_ellipsis_type(printed_declarator))
    {
        tmp_result = "< ellipsis type >";
        return tmp_result;
    }
    else if (is_unknown_dependent_type(printed_declarator))
    {
        tmp_result = "< dependent expression type >";
        return tmp_result;
    }
    else if (is_unresolved_overloaded_type(printed_declarator))
    {
        tmp_result = "< unresolved overload function type >";
        return tmp_result;
    }

    char debug = debug_options.enable_debug_code;
    debug_options.enable_debug_code = 0;

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
                    tmp_result = strappend(tmp_result, " <braced-initializer-list-type>{");
                    int i;
                    for (i = 0; i < braced_list_type_get_num_types(printed_declarator); i++)
                    {
                        if (i != 0)
                        {
                            tmp_result = strappend(tmp_result, ", ");
                        }

                        tmp_result = strappend(tmp_result, 
                                print_declarator(braced_list_type_get_type_num(printed_declarator, i)));
                    }
                    tmp_result = strappend(tmp_result, "} ");
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
            case TK_REBINDABLE_REFERENCE :
                tmp_result = strappend(tmp_result, "rebindable reference to ");
                printed_declarator = printed_declarator->pointer->pointee;
                break;
            case TK_POINTER_TO_MEMBER :
                tmp_result = strappend(tmp_result, "pointer to member of ");
                if (printed_declarator->pointer->pointee_class_type != NULL)
                {
                    tmp_result = strappend(tmp_result, 
                            print_declarator(printed_declarator->pointer->pointee_class_type));
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
                if (printed_declarator->array->with_descriptor)
                {
                    tmp_result = strappend(tmp_result, "(with descriptor) ");
                }
                tmp_result = strappend(tmp_result, "of size [");
                tmp_result = strappend(tmp_result, print_dimension_of_array(printed_declarator->array->whole_size, 
                            CURRENT_COMPILED_FILE->global_decl_context));
                tmp_result = strappend(tmp_result, "] and bounds ");
                tmp_result = strappend(tmp_result, "[");
                tmp_result = strappend(tmp_result, print_dimension_of_array(printed_declarator->array->lower_bound, 
                            CURRENT_COMPILED_FILE->global_decl_context));
                tmp_result = strappend(tmp_result, ":");
                tmp_result = strappend(tmp_result, print_dimension_of_array(printed_declarator->array->upper_bound, 
                            CURRENT_COMPILED_FILE->global_decl_context));
                tmp_result = strappend(tmp_result, "]");
                if (printed_declarator->array->region != NULL)
                {
                    tmp_result = strappend(tmp_result, " with region [");
                    tmp_result = strappend(tmp_result, codegen_to_str(printed_declarator->array->region->lower_bound, 
                                CURRENT_COMPILED_FILE->global_decl_context));
                    tmp_result = strappend(tmp_result, ":");
                    tmp_result = strappend(tmp_result, codegen_to_str(printed_declarator->array->region->upper_bound, 
                                CURRENT_COMPILED_FILE->global_decl_context));
                    tmp_result = strappend(tmp_result, "]" );
                }
                tmp_result = strappend(tmp_result, " of ");
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
                        for (i = 0; i < printed_declarator->template_arguments->num_parameters; i++)
                        {
                            template_parameter_value_t* template_parameter = 
                                printed_declarator->template_arguments->arguments[i];

                            const char* c = NULL;
                            uniquestr_sprintf(&c, "[[%d, %d]] ", 
                                    get_template_nesting_of_template_parameters(printed_declarator->template_arguments),
                                    i);

                            tmp_result = strappend(tmp_result, uniquestr(c));

                            switch (template_parameter->kind)
                            {
                                case TPK_TYPE:
                                case TPK_TYPE_PACK:
                                case TPK_TEMPLATE:
                                case TPK_TEMPLATE_PACK:
                                    {
                                        tmp_result = strappend(tmp_result,
                                                print_declarator(template_parameter->type));

                                        break;
                                    }
                                case TPK_NONTYPE:
                                case TPK_NONTYPE_PACK:
                                    {
                                        tmp_result = strappend(tmp_result,
                                                codegen_to_str(template_parameter->value,
                                                    CURRENT_COMPILED_FILE->global_decl_context));
                                        break;
                                    }
                                default:
                                    {
                                        tmp_result = strappend(tmp_result,
                                                " << unknown template argument >> ");
                                        break;
                                    }
                            }
                            if ((i + 1) < printed_declarator->template_arguments->num_parameters)
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
            case TK_COMPUTED:
                {
                    const char* c = NULL;
                    uniquestr_sprintf(&c, "<computed function type>");
                    printed_declarator = NULL;
                    tmp_result = uniquestr(c);
                    break;
                }
            case TK_PACK:
                {
                    tmp_result = strappend(tmp_result, "pack type of ");
                    printed_declarator = printed_declarator->pack_type->packed;
                    break;
                }
            case TK_SEQUENCE:
                {
                    tmp_result = strappend(tmp_result, "sequence type {");
                    int i;
                    for (i = 0; i < printed_declarator->sequence_type->num_types; i++)
                    {
                        if (i > 0)
                            tmp_result = strappend(tmp_result, ", ");

                        tmp_result = strappend(tmp_result,
                                print_declarator(printed_declarator->sequence_type->types[i]));
                    }
                    tmp_result = strappend(tmp_result, "}");
                    printed_declarator = NULL;
                    break;
                }
            case TK_AUTO:
                {
                    tmp_result = strappend(tmp_result, "auto");
                    printed_declarator = NULL;
                    break;
                }
            case TK_DECLTYPE_AUTO:
                {
                    tmp_result = strappend(tmp_result, "decltype(auto)");
                    printed_declarator = NULL;
                    break;
                }
            default :
                {
                    const char* c = NULL;
                    uniquestr_sprintf(&c, "<unknown type kind %d>", printed_declarator->kind);
                    printed_declarator = NULL;
                    tmp_result = strappend(tmp_result, uniquestr(c));
                    break;
                }
        }
    } while (printed_declarator != NULL);

    debug_options.enable_debug_code = debug;

    return tmp_result;
}

extern inline standard_conversion_t get_identity_scs(type_t* t_orig, type_t* t_dest)
{
    standard_conversion_t result = {
        .orig = t_orig,
        .dest = t_dest,
        .conv = { SCI_IDENTITY, SCI_NO_CONVERSION, SCI_NO_CONVERSION }
    };

    return result;
}

extern inline standard_conversion_t get_invalid_scs(void)
{
    return no_scs_conversion;
}

const char* sci_conversion_to_str(standard_conversion_item_t e)
{
    switch (e)
    {
#define SCI_CONVERSION_ID(X) case X : return #X;
        SCI_LIST
#undef SCI_CONVERSION_ID
        default: return "<<<unknown standard conversion item kind>>>";
    }
}

extern inline char standard_conversion_is_identity(standard_conversion_t scs)
{
    return (scs.conv[0] == SCI_IDENTITY);
}

extern inline char standard_conversion_is_invalid(standard_conversion_t scs)
{
    return (scs.conv[0] == SCI_NO_CONVERSION
            && scs.conv[1] == SCI_NO_CONVERSION
            && scs.conv[2] == SCI_NO_CONVERSION);
}

extern inline type_t* standard_conversion_get_orig_type(standard_conversion_t scs)
{
    return scs.orig;
}

extern inline type_t* standard_conversion_get_dest_type(standard_conversion_t scs)
{
    return scs.dest;
}

extern inline char pointer_types_are_similar(type_t* t_orig, type_t* t_dest)
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
extern inline char pointer_types_can_be_converted(type_t* orig, type_t* dest)
{
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
    cv_qualifier_t qualifs1[MCXX_MAX_QUALIFIER_CONVERSION];
    cv_qualifier_t qualifs2[MCXX_MAX_QUALIFIER_CONVERSION];

    while ((is_pointer_type(t1)
                && is_pointer_type(t2))
            || (is_pointer_to_member_type(t1)
                && is_pointer_to_member_type(t2)))
    {
        ERROR_CONDITION(num_qualifs >= MCXX_MAX_QUALIFIER_CONVERSION, "Too many qualifiers\n", 0);
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
    ERROR_CONDITION(num_qualifs >= MCXX_MAX_QUALIFIER_CONVERSION, "Too many qualifiers\n", 0);
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

extern inline char standard_conversion_between_types(standard_conversion_t *result, type_t* t_orig, type_t* t_dest,
        const locus_t* locus)
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
        (*result) = get_identity_scs(t_orig, t_dest);
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Exactly the same type\n");
        }
        return 1;
    }

    // C only
    if (IS_C_LANGUAGE
            && is_lvalue_reference_type(dest))
    {
        if (!is_lvalue_reference_type(orig))
            return 0;

        type_t* ref_dest = reference_type_get_referenced_type(dest);
        type_t* ref_orig = reference_type_get_referenced_type(orig);

        type_t* unqualif_orig = get_unqualified_type(ref_orig);
        type_t* unqualif_ref_dest = get_unqualified_type(ref_dest);

        // T -> T @ref@
        if (equivalent_types(unqualif_orig, unqualif_ref_dest))
        {
            (*result) = get_identity_scs(t_orig, t_dest);
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Mercurium Extension for C: binding a type to a reference type\n");
            }
            return 1;
        }
        // void @ref@ -> T @ref@
        else if (is_void_type(ref_orig))
        {
            (*result) = get_identity_scs(t_orig, t_dest);
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Mercurium Extension for C: binding a void& to a reference type\n");
            }
            return 1;
        }
    }

    // cv1 T1& -> cv2 T2&
    if (is_lvalue_reference_type(orig)
            && is_lvalue_reference_type(dest))
    {
        type_t* ref_dest = reference_type_get_referenced_type(dest);
        type_t* ref_orig = reference_type_get_referenced_type(orig);

        type_t* unqualif_ref_orig = get_unqualified_type(ref_orig);
        type_t* unqualif_ref_dest = get_unqualified_type(ref_dest);

        if (equivalent_types(unqualif_ref_orig, unqualif_ref_dest)
                || (is_class_type(unqualif_ref_dest)
                    && is_class_type(unqualif_ref_orig)
                    && class_type_is_base_instantiating(unqualif_ref_dest, unqualif_ref_orig, locus)
                    && !class_type_is_ambiguous_base_of_derived_class(unqualif_ref_dest, unqualif_ref_orig)))
        {
            if (!is_more_or_equal_cv_qualified_type(ref_dest, ref_orig))
            {
                return 0;
            }

            (*result) = get_identity_scs(t_orig, t_dest);
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: This is a binding to a lvalue-reference by means of a lvalue-reference\n");
            }
            return 1;
        }
    }

    // cv1 T1&& -> cv2 T2&&
    if (is_rvalue_reference_type(orig)
            && is_rvalue_reference_type(dest))
    {
        type_t* ref_dest = reference_type_get_referenced_type(dest);
        type_t* ref_orig = reference_type_get_referenced_type(orig);

        type_t* unqualif_ref_orig = get_unqualified_type(ref_orig);
        type_t* unqualif_ref_dest = get_unqualified_type(ref_dest);

        if (equivalent_types(unqualif_ref_orig, unqualif_ref_dest)
                || (is_class_type(unqualif_ref_dest)
                    && is_class_type(unqualif_ref_orig)
                    && class_type_is_base_instantiating(unqualif_ref_dest, unqualif_ref_orig, locus)
                    && !class_type_is_ambiguous_base_of_derived_class(unqualif_ref_dest, unqualif_ref_orig)))
        {
            if (!is_more_or_equal_cv_qualified_type(ref_dest, ref_orig))
            {
                return 0;
            }

            (*result) = get_identity_scs(t_orig, t_dest);
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: This is a binding to a rvalue-reference by means of a rvalue-reference\n");
            }
            return 1;
        }
        else if (is_nullptr_type(ref_orig)
                && is_pointer_type(ref_dest))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: This is a binding to a rvalue-reference pointer from a rvalue-reference of std::nullptr_t\n");
            }
            // std::nullptr_t&& -> null pointer value is already a rvalue
            // null pointer value -> c2 T2*&&
            (*result).conv[1] = SCI_NULLPTR_TO_POINTER_CONVERSION;
            return 1;
        }
        else if (CURRENT_CONFIGURATION->enable_intel_vector_types
                && IS_CXX_LANGUAGE
                && is_more_or_equal_cv_qualified_type(ref_dest, ref_orig)
                && intel_vector_struct_to_intel_vector_struct_reinterpret_type(unqualif_ref_orig, unqualif_ref_dest))
        {
            // For C++ we allow this extra reinterpretation
            //    __mXXX{,d,i}&& <-> __mXXX{,d,i}&&
            // We do not account this as a conversion of any kind, we just let
            // these types be transparently compatible
            (*result) = get_identity_scs(t_orig, t_dest);
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: This is a binding to a rvalue-reference by means of "
                        "a rvalue-reference between Intel-compatible vector types\n");
            }
            return 1;
        }
    }

    // (A)
    // cv1 T1   -> const T2&
    // cv1 T1&& -> const T2&
    //
    // (B)
    // cv1 T1   ->   cv2 T2&&
    // [note: cv1 T1&& -> cv2 T2&& has been handled above]
    //
    // where cv2 is more or equal qualified thant cv1.
    //
    // Note that the case when both orig and dest are both references
    // of the same kind (lvalue or rvalue) has already been
    // handled above
    if (// (A)
            ((is_lvalue_reference_type(dest)
              && is_const_qualified_type(reference_type_get_referenced_type(dest)))
             // (B)
             // Make sure that orig is not a lvalue reference (it can be an rvalue-ref, though)
             || (!is_lvalue_reference_type(orig)
                 && is_rvalue_reference_type(dest)
                 && is_more_or_equal_cv_qualified_type(no_ref(dest), no_ref(orig)))))
    {
        standard_conversion_t conversion_among_lvalues = no_scs_conversion;
        // cv T1 -> T2
        (*result) = get_identity_scs(orig, dest);

        char ok = 0;
        if (is_class_type(no_ref(orig))
                && is_class_type(no_ref(dest))
                && class_type_is_base_instantiating(no_ref(dest),
                    get_unqualified_type(no_ref(orig)),
                    locus)
                && !class_type_is_ambiguous_base_of_derived_class(
                    no_ref(dest),
                    no_ref(orig)))
        {
            ok = 1;
        }
        else if (standard_conversion_between_types(&conversion_among_lvalues,
                    no_ref(orig),
                    get_unqualified_type(no_ref(dest)),
                    locus))
        {
            (*result).conv[0] = conversion_among_lvalues.conv[0];
            (*result).conv[1] = conversion_among_lvalues.conv[1];
            (*result).conv[2] = conversion_among_lvalues.conv[2];
            ok = 1;
        }

        if (ok)
        {
            DEBUG_CODE()
            {
                const char* bind_to = "an lvalue-reference";
                if (is_rvalue_reference_type(dest))
                    bind_to = "an rvalue-reference";

                const char* by_means_of = "an rvalue";
                if (is_nullptr_type(no_ref(orig)))
                {
                    if (!is_any_reference_type(orig))
                    {
                        by_means_of = "an rvalue of std::nullptr_t";
                    }
                    else if (is_lvalue_reference_type(orig))
                    {
                        by_means_of = "an lvalue-reference of std::nullptr_t";
                    }
                    // must be rvalue
                    else
                    {
                        by_means_of = "an rvalue-reference of std::nullptr_t";
                    }
                }

                fprintf(stderr, "SCS: This is a binding to %s by means of %s\n", bind_to, by_means_of);
            }

            return 1;
        }
    }

    // T&& -> T
    if (is_rvalue_reference_type(orig))
    {
        orig = no_ref(orig);
    }

    // First kind of conversion
    //
    //   lvalue-to-rvalue <-- this means 'T&' to 'T'
    //   array-to-pointer
    //   function-to-pointer
    //
    // We remember whether the original was a string because we will lose this
    // information when we drop the array type
    char is_string_literal = 0;

    if (is_array_type(no_ref(orig)))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: Applying array-to-pointer conversion\n");
        }
        is_string_literal = array_type_is_string_literal(no_ref(orig));

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
     * FIXME, enums can be promoted to different underlying types. Now assuming
     * that all are int
     *
     * Note that the types compared here must be unqualified, since we
     * don't care their qualification here
     */

    // Note: masks are unsigned integers, so now we unpack the mask to its
    // underlying integer type and proceed their conversion
    if (is_mask_type(dest))
    {
        // Make sure we preserve the cv-qualification
        dest = get_cv_qualified_type(
                mask_type_get_underlying_type(dest),
                get_cv_qualifier(dest));
    }
    if (is_mask_type(orig))
    {
        // Make sure we preserve the cv-qualification
        orig = get_cv_qualified_type(
                mask_type_get_underlying_type(orig),
                get_cv_qualifier(orig));
    }

    type_t* orig_underlying_type = NULL;
    if (is_unscoped_enum_type(orig))
    {
        orig_underlying_type = enum_type_get_underlying_type_for_conversion(orig);
    }

    if (!equivalent_types(get_unqualified_type(dest), get_unqualified_type(orig)))
    {
        if (is_signed_int_type(dest)
                && (is_char_type(orig)
                    || is_signed_char_type(orig)
                    || is_unsigned_char_type(orig)
                    || is_signed_short_int_type(orig)
                    || is_unsigned_short_int_type(orig)
                    || is_char16_t_type(orig)
                    || is_char32_t_type(orig)
                    || is_wchar_t_type(orig)
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
        else if (is_signed_int_type(dest)
                && is_unscoped_enum_type(orig)
                && (is_char_type(orig_underlying_type)
                    || is_signed_char_type(orig_underlying_type)
                    || is_unsigned_char_type(orig_underlying_type)
                    || is_signed_short_int_type(orig_underlying_type)
                    || is_unsigned_short_int_type(orig_underlying_type)
                    || is_signed_int_type(orig_underlying_type)
                    || is_char16_t_type(orig_underlying_type)
                    || is_char32_t_type(orig_underlying_type)
                    || is_wchar_t_type(orig_underlying_type)
                    || is_bool_type(orig_underlying_type)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying integral promotion from enum\n");
            }
            (*result).conv[1] = SCI_INTEGRAL_PROMOTION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_integer_type(dest)
                && is_unscoped_enum_type(orig)
                && is_integer_type(orig_underlying_type)
                && equivalent_types(orig_underlying_type, dest))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying integral promotion from enum\n");
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
                && is_integer_type(orig)
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
                && !is_bool_type(dest)
                && is_unscoped_enum_type(orig)
                && is_integer_type(orig_underlying_type)
                && !equivalent_types(orig_underlying_type, dest))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying integral conversion from enum\n");
            }
            (*result).conv[1] = SCI_INTEGRAL_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_bool_type(dest)
                && !is_bool_type(orig)
                && (is_arithmetic_type(orig)
                    || is_unscoped_enum_type(orig)
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
        else if (is_floating_type(orig)
                    && is_integer_type(dest))
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
                && is_integer_type(orig))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying floating-integral conversion\n");
            }
            (*result).conv[1] = SCI_INTEGRAL_FLOATING_CONVERSION;
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
            (*result).conv[1] = SCI_INTEGRAL_FLOATING_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_floating_type(dest)
                && is_unscoped_enum_type(orig))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying floating-integral conversion from enum\n");
            }
            (*result).conv[1] = SCI_INTEGRAL_FLOATING_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_zero_type(orig)
                && (is_pointer_type(dest)
                    || is_pointer_to_member_type(dest)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying pointer-conversion from 0 to pointer\n");
            }

            (*result).conv[1] = SCI_ZERO_TO_POINTER_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (IS_CXX_LANGUAGE
                && is_nullptr_type(orig)
                && (is_pointer_type(dest)
                    || is_pointer_to_member_type(dest)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying pointer-conversion from std::nullptr_t to pointer\n");
            }

            (*result).conv[1] = SCI_NULLPTR_TO_POINTER_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (IS_CXX_LANGUAGE
                && is_zero_type(orig)
                && is_nullptr_type(dest))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying pointer-conversion from 0 to std::nullptr_t\n");
            }

            (*result).conv[1] = SCI_ZERO_TO_NULLPTR;
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
            (*result).conv[1] = SCI_POINTER_TO_VOID_CONVERSION;

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

            (*result).conv[1] = SCI_VOID_TO_POINTER_CONVERSION;
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

            (*result).conv[1] = SCI_INTEGRAL_TO_POINTER_CONVERSION;
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

            (*result).conv[1] = SCI_POINTER_TO_INTEGRAL_CONVERSION;
            dest = get_unqualified_type(orig);
        }
        else if (is_pointer_to_class_type(orig)
                && is_pointer_to_class_type(dest)
                && class_type_is_base_strict_instantiating(
                    pointer_type_get_pointee_type(dest),
                    pointer_type_get_pointee_type(orig), locus)
                && !class_type_is_ambiguous_base_of_derived_class(
                        pointer_type_get_pointee_type(dest),
                        pointer_type_get_pointee_type(orig)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying pointer conversion to pointer to base class\n");
            }
            (*result).conv[1] = SCI_CLASS_POINTER_DERIVED_TO_BASE_CONVERSION;
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
                && class_type_is_base_strict_instantiating(pointer_to_member_type_get_class_type(orig), pointer_to_member_type_get_class_type(dest), locus)
                && !class_type_is_ambiguous_base_of_derived_class(pointer_to_member_type_get_class_type(orig),
                    pointer_to_member_type_get_class_type(dest)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying pointer-to-member conversion to pointer-to-member of derived class\n");
            }
            (*result).conv[1] = SCI_POINTER_TO_MEMBER_BASE_TO_DERIVED_CONVERSION;
            // Note that orig is converted to an unqualified version of the dest type.
            // Given dest as 'cv1 T (A::* cv2)' we will set orig to 'T (A::*)'
            orig = get_pointer_to_member_type(
                    get_unqualified_type(pointer_type_get_pointee_type(dest)), // This gives us 'T'
                    pointer_to_member_type_get_class_type(dest) // This is 'A'
                    );
        }
        // _Complex cases
        else if (is_integer_type(orig)
                && is_complex_type(dest)
                && is_floating_type(complex_type_get_base_type(dest)))
        {
            // Integral -> Complex FloatingType
            // We model this as a conversion from Integral to FloatingType
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying integer to (complex-)floating-integral conversion\n");
            }
            (*result).conv[1] = SCI_INTEGRAL_TO_COMPLEX_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_complex_type(orig)
                && is_floating_type(complex_type_get_base_type(orig))
                && is_integer_type(dest))
        {
            // Complex FloatingType -> Integral
            // We model this as a conversion from FloatingType to Integral
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying complex-float to integral conversion\n");
            }
            (*result).conv[1] = SCI_COMPLEX_TO_INTEGRAL_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_floating_type(orig)
                && !is_complex_type(orig)
                && is_complex_type(dest)
                && ((is_double_type(complex_type_get_base_type(dest))
                        && is_float_type(orig))
                    || (is_long_double_type(complex_type_get_base_type(dest))
                        && (is_float_type(orig)
                            || is_double_type(orig)))))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying float to complex promotion\n");
            }
            (*result).conv[1] = SCI_FLOAT_TO_COMPLEX_PROMOTION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_complex_type(orig)
                && is_complex_type(dest)
                && ((is_floating_type(complex_type_get_base_type(orig))
                        && is_integer_type(complex_type_get_base_type(dest)))
                    || (is_integer_type(complex_type_get_base_type(orig))
                        && is_floating_type(complex_type_get_base_type(dest)))))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying complex floating-integral conversion\n");
            }
            (*result).conv[1] = SCI_COMPLEX_FLOATING_INTEGRAL_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_floating_type(orig)
                && !is_complex_type(orig)
                && is_complex_type(dest))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying float to complex conversion\n");
            }
            (*result).conv[1] = SCI_FLOAT_TO_COMPLEX_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_complex_type(orig)
                && is_complex_type(dest)
                && ((is_double_type(complex_type_get_base_type(dest))
                        && is_float_type(complex_type_get_base_type(orig)))
                    || (is_long_double_type(complex_type_get_base_type(dest))
                        && (is_float_type(complex_type_get_base_type(orig))
                            || is_double_type(complex_type_get_base_type(orig))))))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying complex promotion\n");
            }
            (*result).conv[1] = SCI_COMPLEX_PROMOTION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_complex_type(orig)
                && is_complex_type(dest))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying complex conversion\n");
            }
            (*result).conv[1] = SCI_COMPLEX_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        else if (is_complex_type(orig)
                && is_floating_type(dest))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying complex to floating conversion\n");
            }
            (*result).conv[1] = SCI_COMPLEX_TO_FLOAT_CONVERSION;
            // Direct conversion, no cv-qualifiers can be involved here
            orig = dest;
        }
        // Vector conversions
        // scalar -->__attribute_((vector_size(X)))
        // [C++] scalar --> rvalue reference of __attribute_((vector_size(X)))
        else if (is_arithmetic_type(orig)
                && is_vector_type(dest))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Applying scalar-to-vector conversion\n");
            }
            (*result).conv[1] = SCI_SCALAR_TO_VECTOR_CONVERSION;
            dest = vector_type_get_element_type(no_ref(dest));
        }
        // Intel vector conversions
        else if (CURRENT_CONFIGURATION->enable_intel_vector_types
                // vector type
                //    -> struct __m128 / struct __m256 / struct __M512
                && (vector_type_to_intel_vector_struct_reinterpret_type(orig, dest)
                    // lvalue reference to vector type
                    //    -> lvalue reference to struct __m128 / struct __m256 / struct __M512
                    || (is_lvalue_reference_type(orig)
                        && is_lvalue_reference_type(dest)
                        && vector_type_to_intel_vector_struct_reinterpret_type(no_ref(orig), no_ref(dest)))))
        {
            // We do not account this as a conversion of any kind, we just let
            // these types be transparently compatible
            orig = dest;
        }
        else if (CURRENT_CONFIGURATION->enable_intel_vector_types
                // struct __m128 / struct __m256 / struct __M512
                // -> vector type
                && (vector_type_to_intel_vector_struct_reinterpret_type(dest, orig)
                    // lvalue reference to struct __m128 / struct __m256 / struct __M512
                    // -> lvalue reference to vector type
                    || (is_lvalue_reference_type(orig)
                        && is_lvalue_reference_type(dest)
                        && vector_type_to_intel_vector_struct_reinterpret_type(no_ref(dest), no_ref(orig)))))
        {
            // We do not account this as a conversion of any kind, we just let
            // these types be transparently compatible
            orig = dest;
        }
        else if (CURRENT_CONFIGURATION->enable_intel_vector_types
                && IS_CXX_LANGUAGE
                && (intel_vector_struct_to_intel_vector_struct_reinterpret_type(orig, dest)
                    || (is_lvalue_reference_type(orig)
                        && is_lvalue_reference_type(dest)
                        && intel_vector_struct_to_intel_vector_struct_reinterpret_type(no_ref(orig), no_ref(dest)))))
        {
            // For C++ we allow this extra reinterpretation
            //    __mXXX{,d,i} <-> __mXXX{,d,i}
            //    __mXXX{,d,i}& <-> __mXXX{,d,i}&
            // We do not account this as a conversion of any kind, we just let
            // these types be transparently compatible
            orig = dest;
        }
        // workaround for SSE types that are incorrectly declared
        else if ((strcmp(CURRENT_CONFIGURATION->type_environment->environ_id, "linux-x86_64") == 0
                    || strcmp(CURRENT_CONFIGURATION->type_environment->environ_id, "linux-i386") == 0)
                && (is_vector_type(orig)
                    && is_vector_type(dest)
                    && (vector_type_get_vector_size_in_bytes(orig) == 8      // MMX
                        || vector_type_get_vector_size_in_bytes(orig) == 16  // SSE
                        || vector_type_get_vector_size_in_bytes(orig) == 32) // AVX2
                    && (vector_type_get_vector_size_in_bytes(orig)
                        == vector_type_get_vector_size_in_bytes(dest))
                    && (is_integral_type(vector_type_get_element_type(orig)) == is_integral_type(vector_type_get_element_type(dest))
                        || is_floating_type(vector_type_get_element_type(orig)) == is_floating_type(vector_type_get_element_type(dest)))))
        {
            // We do not account this as a conversion of any kind, we just let
            // these types be transparently compatible
            orig = dest;
        }
    }

    // Third kind of conversion
    //
    //  qualification-conversion
    //
    //  T* -> const T*
    //  char* -> const char*       [only for string-literals]
    //  wchar_t* -> const wchar_t* [only for string-literals]
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
                && is_string_literal // We saved this before dropping the array
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
                && is_string_literal // We saved this before dropping the array
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
            // Note that we override conv[1] not conv[2]
            (*result).conv[1] = SCI_POINTER_TO_POINTER_CONVERSION;
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
        if (standard_conversion_is_invalid((*result)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SCS: Exactly the same type after removing cv-qualifiers of the first type\n");
            }
            (*result) = get_identity_scs(t_orig, t_dest);
        }
        DEBUG_CODE()
        {
            fprintf(stderr, "SCS: There is a standard conversion from '%s' to '%s'\n",
                    print_declarator(t_orig),
                    print_declarator(t_dest));
        }
    }

    return valid_conversion;
}

extern inline type_t* get_unresolved_overloaded_type(const scope_entry_list_t* overload_set,
        template_parameter_list_t* explicit_template_arguments)
{
    type_t* result = new_empty_type();

    result->kind = TK_OVERLOAD;

    result->unqualified_type = result;
    // Use a smarter approach for this one
    result->overload_set = entry_list_copy(overload_set);
    result->template_arguments = explicit_template_arguments;

    return result;
}

extern inline char is_unresolved_overloaded_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_OVERLOAD);
}

extern inline scope_entry_list_t *unresolved_overloaded_type_get_overload_set(type_t* t)
{
    ERROR_CONDITION(!is_unresolved_overloaded_type(t), "This is not an unresolved overloaded type", 0);

    return entry_list_copy(t->overload_set);
}

extern inline template_parameter_list_t* unresolved_overloaded_type_get_explicit_template_arguments(type_t* t)
{
    ERROR_CONDITION(!is_unresolved_overloaded_type(t), "This is not an unresolved overloaded type", 0);

    return t->template_arguments;
}

extern inline scope_entry_t* unresolved_overloaded_type_simplify_unpacked(
        scope_entry_list_t* overload_set,
        template_parameter_list_t* explicit_template_arguments,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    // Fallback case not using the target type
    if (explicit_template_arguments == NULL)
    {
        scope_entry_list_t* nonspecialized_functions = filter_symbol_using_predicate(
                overload_set,
                is_nonspecialized_function_pred,
                NULL);

        scope_entry_t* result = NULL;
        if (entry_list_size(nonspecialized_functions) == 1)
        {
            result = entry_advance_aliases(
                    entry_list_head(nonspecialized_functions)
                    );
        }
        entry_list_free(nonspecialized_functions);

        return result;
    }
    else // explicit_template_arguments != NULL
    {
        scope_entry_list_t* template_functions = filter_symbol_using_predicate(
                overload_set,
                is_template_function_pred,
                NULL);

        scope_entry_t* tested_fun = NULL;
        if (entry_list_size(template_functions) == 1)
        {
            tested_fun = entry_advance_aliases(
                    entry_list_head(template_functions)
                    );
        }
        entry_list_free(template_functions);

        if (tested_fun != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Attempt to come up with a single specialization as the named function\n");
            }

            // Try to come up with an instantiation
            template_parameter_list_t* deduced_template_arguments = NULL;
            deduction_result_t deduction_result = handle_explicit_template_arguments(
                    template_specialized_type_get_template_parameters(
                        named_type_get_symbol(
                            template_type_get_primary_type(tested_fun->type_information)
                            )->type_information),
                    explicit_template_arguments,
                    decl_context,
                    locus,
                    &deduced_template_arguments);

            if (deduction_result == DEDUCTION_FAILURE)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: Explicit template arguments failed\n");
                }
                return NULL;
            }

            deduction_set_t* deduction_set_empty = NEW0(deduction_set_t);
            deduction_result = finish_deduced_template_arguments(
                    template_type_get_template_parameters(tested_fun->type_information),
                    deduction_set_empty,
                    decl_context,
                    locus,
                    deduced_template_arguments);
            deduction_set_free(deduction_set_empty);

            if (deduction_result == DEDUCTION_FAILURE)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: Final deduction failed\n");
                }
                free_template_parameter_list(deduced_template_arguments);
                return NULL;
            }

            type_t* named_specialization_type = template_type_get_specialized_type(
                    tested_fun->type_information,
                    deduced_template_arguments,
                    decl_context,
                    locus);
            free_template_parameter_list(deduced_template_arguments);

            if (named_specialization_type == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUTILS: Substitution failed\n");
                }
                free_template_parameter_list(deduced_template_arguments);
                return NULL;
            }

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUTILS: Fallback template specialization chosen '%s'\n",
                        print_declarator(named_specialization_type));
            }

            return named_type_get_symbol(named_specialization_type);
        }
    }

    return NULL;
}

extern inline scope_entry_t* unresolved_overloaded_type_simplify(type_t* t, const decl_context_t* decl_context, const locus_t* locus)
{
    return unresolved_overloaded_type_simplify_unpacked(
            unresolved_overloaded_type_get_overload_set(t),
            unresolved_overloaded_type_get_explicit_template_arguments(t),
            decl_context,
            locus);
}

static dhash_ptr_t *_zero_types_hash = NULL;

extern inline type_t* get_variant_type_zero(type_t* t)
{
    ERROR_CONDITION (!is_integral_type(t) && !is_bool_type(t), "Base type must be integral", 0);
    if (is_zero_type(t))
        return t;

    cv_qualifier_t cv_qualif = get_cv_qualifier(t);
    t = get_cv_qualified_type(advance_over_typedefs(t), CV_NONE);

    if (_zero_types_hash == NULL)
    {
        _zero_types_hash = dhash_ptr_new(5);
    }

    type_t* result = dhash_ptr_query(_zero_types_hash, (const char*)t);

    if (result == NULL)
    {
        result = copy_type_for_variant(t);
        result->info->is_zero_type = 1;

        dhash_ptr_insert(_zero_types_hash, (const char*)t, result);
    }

    return get_cv_qualified_type(result, cv_qualif);
}

// Special variant type for '0' constants
extern inline type_t* get_zero_type(type_t* t)
{
    return get_variant_type_zero(t);
}

// Special type for 'false'
extern inline type_t* get_bool_false_type(void)
{
    return get_variant_type_zero(get_bool_type());
}

extern inline char variant_type_is_zero(type_t* t)
{
    if (t == NULL)
        return 0;

    t = advance_over_typedefs(t);
    return (t->info->is_zero_type);
}

extern inline char is_zero_type(type_t* t)
{
    return variant_type_is_zero(t);
}

static type_t* __nullptr_t = NULL;
extern inline type_t* get_nullptr_type(void)
{
    if (__nullptr_t == NULL)
    {
        __nullptr_t = get_simple_type();
        __nullptr_t->type->kind = STK_BUILTIN_TYPE;
        __nullptr_t->type->builtin_type = BT_NULLPTR_T;
        __nullptr_t->info->size = CURRENT_CONFIGURATION->type_environment->sizeof_pointer;
        __nullptr_t->info->alignment = CURRENT_CONFIGURATION->type_environment->alignof_pointer;
        __nullptr_t->info->valid_size = 1;
    }
    return __nullptr_t;
}

extern inline char is_nullptr_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return t != NULL
        && t->kind == TK_DIRECT
        && t->type->kind == STK_BUILTIN_TYPE
        && t->type->builtin_type == BT_NULLPTR_T;
}

extern inline char is_zero_type_or_nullptr_type(type_t* t)
{
    return is_zero_type(t) || is_nullptr_type(t);
}

static type_t* _error_type = NULL;
extern inline type_t* get_error_type(void)
{
    if (_error_type == NULL)
    {
        _error_type = new_empty_type();
        _error_type->kind = TK_ERROR;
        _error_type->unqualified_type = _error_type;
    }
    return _error_type;
}

extern inline char is_error_type(type_t* t)
{
    // We do not allow a NULL type here at the moment
    ERROR_CONDITION(t == NULL, "Invalid type", 0);
    t = get_unqualified_type(advance_over_typedefs(t));
    return (_error_type != NULL && t == _error_type);
}


extern inline type_t* get_literal_string_type(int length, type_t* base_type)
{
    nodecl_t integer_literal = nodecl_make_integer_literal(
            get_ptrdiff_t_type(),
            const_value_get_integer(length, type_get_size(get_ptrdiff_t_type()), /* sign */ 1),
            make_locus("", 0, 0));

    type_t* array_type = get_array_type_for_literal_string(
            base_type,
            integer_literal,
            CURRENT_COMPILED_FILE->global_decl_context);

    type_t* literal_type = get_lvalue_reference_type(
            array_type);

    return literal_type;
}

extern inline char array_type_is_string_literal(type_t* t)
{
    ERROR_CONDITION(!is_array_type(t), "Invalid type", 0);
    t = advance_over_typedefs(t);

    return t->array->is_string_literal;
}

extern inline char is_string_literal_type(type_t* t)
{
    if (!is_lvalue_reference_type(t)
            || !is_array_type(no_ref(t)))
    {
        return 0;
    }

    return array_type_is_string_literal(no_ref(t));
}

static type_t* _ellipsis_type = NULL;

extern inline type_t* get_ellipsis_type(void)
{
    if (_ellipsis_type == NULL)
    {
        _ellipsis_type = new_empty_type();
        _ellipsis_type->kind = TK_ELLIPSIS;

        _ellipsis_type->unqualified_type = _ellipsis_type;
    }

    return _ellipsis_type;
}


extern inline char is_ellipsis_type(type_t* t)
{
    return ((_ellipsis_type != NULL)
            && (t == _ellipsis_type));
}

extern inline type_t* get_braced_list_type(int num_types, type_t** type_list)
{
    ERROR_CONDITION(num_types < 0, "Invalid number of types (%d)", num_types);

    // Special case for empty braced lists
    if (num_types == 0)
    {
        static type_t* _empty_braces_type = NULL;

        if (_empty_braces_type == NULL)
        {
            _empty_braces_type = new_empty_type();
            _empty_braces_type->kind = TK_BRACED_LIST;
            _empty_braces_type->unqualified_type = _empty_braces_type;
            _empty_braces_type->braced_type = NEW0(braced_list_info_t);
        }

        return _empty_braces_type;
    }

    static type_trie_t* _braced_types_trie = NULL;
    if (_braced_types_trie == NULL)
    {
        _braced_types_trie = allocate_type_trie();
    }

    int i;

    char any_is_dependent = 0;
    for (i = 0; i < num_types && !any_is_dependent; i++)
    {
        any_is_dependent = is_dependent_type(type_list[i]);
    }

    type_t* result = (type_t*)lookup_type_trie(_braced_types_trie, (const type_t**)type_list, num_types);

    if (result == NULL)
    {
        result = new_empty_type();
        result->kind = TK_BRACED_LIST;

        result->unqualified_type = result;

        result->braced_type = NEW0(braced_list_info_t);

        result->braced_type->num_types = num_types;
        result->braced_type->types = NEW_VEC(type_t*, num_types);
        memcpy(result->braced_type->types, type_list,
                num_types* sizeof(*result->braced_type->types));

        result->info->is_dependent = any_is_dependent;
    }

    return result;
}

extern inline int braced_list_type_get_num_types(type_t* t)
{
    ERROR_CONDITION (!is_braced_list_type(t), "This is not a braced list type", 0);
    return t->braced_type->num_types;
}

extern inline type_t** braced_list_type_get_types(type_t* t)
{
    ERROR_CONDITION (!is_braced_list_type(t), "This is not a braced list type", 0);
    return t->braced_type->types;
}

extern inline type_t* braced_list_type_get_type_num(type_t* t, int num)
{
    ERROR_CONDITION (!is_braced_list_type(t), "This is not a braced list type", 0);
    return t->braced_type->types[num];
}

extern inline char is_braced_list_type(type_t* t)
{
    return ((t != NULL)
            && (t->kind == TK_BRACED_LIST));
}

static type_t* _throw_expr_type = NULL;

extern inline type_t* get_throw_expr_type(void)
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

extern inline char is_throw_expr_type(type_t* t)
{
    return ((_throw_expr_type != NULL)
            && (t == _throw_expr_type));
}

static type_t* _pseudo_destructor_call_type = NULL;

extern inline type_t* get_pseudo_destructor_call_type(void)
{
    if (_pseudo_destructor_call_type == NULL)
    {
        _pseudo_destructor_call_type = 
            get_pointer_type(get_new_function_type(get_void_type(), NULL, 0, REF_QUALIFIER_NONE));
    }

    return _pseudo_destructor_call_type;
}

extern inline char is_pseudo_destructor_call_type(type_t *t)
{
    return (_pseudo_destructor_call_type != NULL) && 
        t == _pseudo_destructor_call_type;
}

extern inline int get_sizeof_type(type_t* t)
{
    return t->info->size;
}

extern inline type_t* get_computed_function_type(computed_function_type_t compute_type_function)
{
    type_t* result = new_empty_type();

    result->kind = TK_COMPUTED;
    result->unqualified_type = result;
    result->compute_type_function = compute_type_function;

    return result;
}

extern inline char is_computed_function_type(type_t* t)
{
    return (t != NULL
            && t->kind == TK_COMPUTED);
}

extern inline computed_function_type_t computed_function_type_get_computing_function(type_t* t)
{
    ERROR_CONDITION(!is_computed_function_type(t),
            "This is not a computed function type!", 0);
    return t->compute_type_function;
}

// A literal type may be:
//  1. scalar type
//  2. a class type with:
//      2.1 a trivial copy constructor,
//      2.2 no non-trivial move constructor,
//      2.3 a trivial destructor,
//      2.4 a trivial default constructor or at least one constexpr constructor
//          other than the copy or move constructor
//      2.5 all non-static data members and base classes of literal types
//  3. an array of literal type
//
extern inline char is_literal_type(type_t* t)
{
    if (is_scalar_type(t))
    {
        return 1;
    }
    else if (is_class_type(t))
    {
        scope_entry_list_t* copy_constructors = class_type_get_copy_constructors(t);
        scope_entry_list_iterator_t* it = NULL;

        char found_bad_case = 1;
        // 2.1 a trivial copy constructors
        for (it = entry_list_iterator_begin(copy_constructors);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            if (symbol_entity_specs_get_is_trivial(entry))
            {
                found_bad_case = 0;
                break;
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(copy_constructors);

        if (found_bad_case)
            return 0;

        // 2.2 no non-trivial move constructor,
        found_bad_case = 0;
        scope_entry_list_t* move_constructors = class_type_get_move_constructors(t);
        for (it = entry_list_iterator_begin(move_constructors);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            if (!symbol_entity_specs_get_is_trivial(entry))
            {
                found_bad_case = 1;
                break;
            }
        }
        entry_list_iterator_free(it);

        if (found_bad_case)
            return 0;

        // 2.3 a trivial destructor,
        scope_entry_t* destructor = class_type_get_destructor(t);
        if (destructor != NULL 
                && !symbol_entity_specs_get_is_trivial(destructor))
        {
            return 0;
        }

        scope_entry_t* default_ctor = class_type_get_default_constructor(t);
        if (default_ctor != NULL && !symbol_entity_specs_get_is_trivial(default_ctor))
        {
            //  2.4 a trivial default constructor or at least one constexpr constructor
            //      other than the copy or move constructor
            // (We found a default constructor but it is not trivial, check for
            // one constexpr constructors that is not a copy or move
            // constructor)
            found_bad_case = 1;
            scope_entry_list_t* all_constructors = class_type_get_constructors(t);
            for (it = entry_list_iterator_begin(all_constructors);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* entry = entry_list_iterator_current(it);
                if (symbol_entity_specs_get_is_constexpr(entry)
                        && !symbol_entity_specs_get_is_move_constructor(entry)
                        && !symbol_entity_specs_get_is_copy_constructor(entry))
                {
                    found_bad_case = 0;
                    break;
                }
            }
            entry_list_iterator_free(it);

            if (found_bad_case)
                return 0;
        }

        // 2.5 all non-static data members...
        found_bad_case = 0;
        scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(t);
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* data_member = entry_list_iterator_current(it);
            if (!is_literal_type(data_member->type_information))
            {
                found_bad_case = 1;
                break;
            }
        }
        entry_list_iterator_free(it);

        if (found_bad_case)
            return 0;

        // 2.5 ...and base classes of literal types
        int i, n = class_type_get_num_bases(t);
        for (i = 0 ; i < n; i++)
        {
            char is_virtual = 0;
            char is_dependent = 0;
            char is_expansion = 0;
            scope_entry_t* base = class_type_get_base_num(t, i,
                    &is_virtual, &is_dependent, &is_expansion, NULL);
            if (!is_literal_type(base->type_information))
            {
                found_bad_case = 1;
                break;
            }
        }

        if (found_bad_case)
            return 0;

        // Everything seems fine
        return 1;
    }
    else if (is_array_type(t))
    {
        return is_literal_type(array_type_get_element_type(t));
    }
    else
    {
        return 0;
    }
}

// A trivial type may be:
//  1. scalar type
//  2. trivial class type
//  3. arrays of such types
//  4. cv-qualified versions of these types
//
extern inline char is_trivial_type(type_t* t)
{
    t = get_unqualified_type(t);
    return (is_scalar_type(t) ||
           (is_class_type(t) && class_type_is_trivial(t)) ||
           (is_array_type(t) && is_trivial_type(array_type_get_element_type(t))));
}

// A scalar type may be:
//  1. arithmetic type
//  2. enumeration type
//  3. pointer type
//  4. pointer to member type
//  5. std::nullptr_-t
//  6. cv-qualified versions of these types
//
extern inline char is_scalar_type(type_t* t)
{
    t = get_unqualified_type(t);
    return (is_arithmetic_type(t) ||
            is_enum_type(t) ||
            is_pointer_type(t) ||
            is_pointer_to_member_type(t) ||
            is_nullptr_type(t));
}

extern inline char is_incomplete_type(type_t* t)
{
    t = canonical_type(t);
    return t->info->is_incomplete;
}

extern inline char is_complete_type(type_t* t)
{
    return !is_incomplete_type(t);
}

extern inline void set_is_incomplete_type(type_t* t, char is_incomplete)
{
    t = canonical_type(t);
    t->info->is_incomplete = is_incomplete;
}

extern inline void set_is_complete_type(type_t* t, char is_complete)
{
    set_is_incomplete_type(t, !is_complete);
}

extern inline scope_entry_list_t* class_type_get_all_bases(type_t *t, char include_dependent)
{
    ERROR_CONDITION(!is_class_type(t), "This is not a class type", 0);

    scope_entry_list_t *result = NULL;

    int i; 
    int num_bases = class_type_get_num_bases(t);
    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        char is_expansion = 0;
        scope_entry_t* base_class = class_type_get_base_num(t, i,
                &is_virtual, &is_dependent,  &is_expansion,
                /* access_specifier*/ NULL);

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
    if (overrided_type == NULL
            && virtual_type == NULL)
        return 1;

    if (equivalent_types(overrided_type, virtual_type))
        return 1;

    if ((is_pointer_to_class_type(overrided_type)
            && is_pointer_to_class_type(virtual_type))
            || (is_any_reference_to_class_type(overrided_type)
                && is_any_reference_to_class_type(virtual_type)))
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

static char same_function_qualification(type_t* overrided_type, type_t* virtual_type)
{
    if (!equivalent_cv_qualification(get_cv_qualifier(overrided_type), get_cv_qualifier(virtual_type)))
        return 0;

    if (function_type_get_ref_qualifier(overrided_type) !=
            function_type_get_ref_qualifier(virtual_type))
        return 0;

    return 1;
}

extern inline char function_type_can_override(type_t* potential_overrider, type_t* function_type)
{
    ERROR_CONDITION(!is_function_type(potential_overrider), "Must be a function type", 0);
    ERROR_CONDITION(!is_function_type(function_type), "Must be a function type", 0);

    return compatible_parameters(potential_overrider->function, function_type->function)
        && covariant_return(function_type_get_return_type(potential_overrider),
                function_type_get_return_type(function_type))
        && same_function_qualification(potential_overrider, function_type);
}

extern inline char function_type_same_parameter_types_and_cv_qualif(type_t* t1, type_t* t2)
{
    return compatible_parameters(t1->function, t2->function)
        && get_cv_qualifier(t1) == get_cv_qualifier(t2);
}

extern inline char class_type_is_trivially_copiable(type_t* t)
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

    scope_entry_list_t* copy_constructors = class_type_get_copy_constructors(class_type);
    scope_entry_list_iterator_t* it = NULL;

    for (it = entry_list_iterator_begin(copy_constructors);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (!symbol_entity_specs_get_is_trivial(entry))
        {
            entry_list_iterator_free(it);
            entry_list_free(copy_constructors);
            return 0;
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(copy_constructors);

    scope_entry_list_t* move_constructors = class_type_get_move_constructors(class_type);
    for (it = entry_list_iterator_begin(move_constructors);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (!symbol_entity_specs_get_is_trivial(entry))
        {
            entry_list_iterator_free(it);
            entry_list_free(move_constructors);
            return 0;
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(move_constructors);

    scope_entry_list_t* copy_assignment_operators = class_type_get_copy_assignment_operators(class_type);
    for (it = entry_list_iterator_begin(copy_assignment_operators);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (!symbol_entity_specs_get_is_trivial(entry))
        {
            entry_list_iterator_free(it);
            entry_list_free(copy_assignment_operators);
            return 0;
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(copy_assignment_operators);

    scope_entry_list_t* move_assignment_operators = class_type_get_move_assignment_operators(class_type);
    for (it = entry_list_iterator_begin(move_assignment_operators);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (!symbol_entity_specs_get_is_trivial(entry))
        {
            entry_list_iterator_free(it);
            entry_list_free(move_assignment_operators);
            return 0;
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(move_assignment_operators);

    scope_entry_t* destructor = class_type_get_destructor(class_type);
    if (destructor != NULL
            && !symbol_entity_specs_get_is_trivial(destructor))
        return 0;

    return 1;
}

extern inline char class_type_is_trivial(type_t* t)
{
    ERROR_CONDITION(!is_class_type(t), "It must be a class type", 0);
    type_t* class_type = get_actual_class_type(t);

    scope_entry_t* default_ctr = class_type_get_default_constructor(class_type);

    if (default_ctr != NULL && !symbol_entity_specs_get_is_trivial(default_ctr))
        return 0;

    if (!class_type_is_trivially_copiable(t))
        return 0;

    return 1;
}

extern inline char class_type_is_standard_layout(type_t* t)
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

    scope_entry_list_iterator_t* it = NULL;
    scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(class_type);
    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* data_member = entry_list_iterator_current(it);

        type_t* data_member_type = data_member->type_information;

        if (is_lvalue_reference_type(data_member_type)
                || is_rvalue_reference_type(data_member_type))
        {
            entry_list_iterator_free(it);
            entry_list_free(nonstatic_data_members);
            return 0;
        }

        if (is_array_type(data_member_type))
            data_member_type = array_type_get_element_type(data_member_type);
        
        if (is_class_type(data_member_type)
                && !class_type_is_standard_layout(data_member_type))
        {
            entry_list_iterator_free(it);
            entry_list_free(nonstatic_data_members);
            return 0;
        }
    }
    entry_list_iterator_free(it);

    scope_entry_list_t* member_functions = class_type_get_member_functions(class_type);
    for (it = entry_list_iterator_begin(member_functions);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* member_function = entry_list_iterator_current(it);

        if (symbol_entity_specs_get_is_virtual(member_function))
        {
            entry_list_iterator_free(it);
            entry_list_free(member_functions);
            entry_list_free(nonstatic_data_members);
            return 0;
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(member_functions);

    int i;
    for (i = 0 ; i < class_type_get_num_bases(class_type); i++)
    {
        char is_virtual = 0, is_dependent = 0, is_expansion = 0;
        /* scope_entry_t* base = */ class_type_get_base_num(class_type, i,
                &is_virtual, &is_dependent, &is_expansion,
                /* access_specifier_t */ NULL);

        if (is_virtual)
            return 0;
    }

    access_specifier_t access = AS_UNKNOWN;

    for (it = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* data_member = entry_list_iterator_current(it);

        if (access == AS_UNKNOWN)
        {
            access = symbol_entity_specs_get_access(data_member);
        }
        else if (access != symbol_entity_specs_get_access(data_member))
        {
            entry_list_iterator_free(it);
            entry_list_free(nonstatic_data_members);
            return 0;
        }
    }
    entry_list_iterator_free(it);

    for (i = 0 ; i < class_type_get_num_bases(class_type); i++)
    {
        char is_virtual = 0, is_dependent = 0, is_expansion;
        scope_entry_t* base = class_type_get_base_num(class_type, i,
                &is_virtual, &is_dependent, &is_expansion,
                /* access_specifier */ NULL);

        if (!class_type_is_standard_layout(base->type_information))
        {
            entry_list_free(nonstatic_data_members);
            return 0;
        }
    }

    if (nonstatic_data_members == NULL)
    {
        int nonempty = 0;
        for (i = 0 ; i < class_type_get_num_bases(class_type); i++)
        {
            char is_virtual = 0, is_dependent = 0, is_expansion;
            scope_entry_t* base = class_type_get_base_num(class_type, i,
                    &is_virtual, &is_dependent, &is_expansion,
                    /* access_specifier */ NULL);

            scope_entry_list_t* base_nonstatic_data_members = class_type_get_nonstatic_data_members(base->type_information);
            nonempty += (nonstatic_data_members != NULL);
            entry_list_free(base_nonstatic_data_members);
        }

        if (nonempty > 1)
            return 0;
    }
    else
    {
        for (i = 0 ; i < class_type_get_num_bases(class_type); i++)
        {
            char is_virtual = 0, is_dependent = 0, is_expansion;
            scope_entry_t* base = class_type_get_base_num(class_type, i,
                    &is_virtual, &is_dependent, &is_expansion,
                    /* access_specifier */ NULL);

            scope_entry_list_t* base_nonstatic_data_members = class_type_get_nonstatic_data_members(base->type_information);
            if (base_nonstatic_data_members == NULL)
            {
                entry_list_free(nonstatic_data_members);
                return 0;
            }
        }

        scope_entry_t* first_nonstatic = entry_list_head(nonstatic_data_members);
        for (i = 0 ; i < class_type_get_num_bases(class_type); i++)
        {
            char is_virtual = 0, is_dependent = 0, is_expansion;
            scope_entry_t* base = class_type_get_base_num(class_type, i,
                    &is_virtual, &is_dependent, &is_expansion,
                    /* access_specifier */ NULL);

            if (equivalent_types(first_nonstatic->type_information, base->type_information))
            {
                entry_list_free(nonstatic_data_members);
                return 0;
            }
        }
    }
    entry_list_free(nonstatic_data_members);

    return 1;
}

extern inline char is_aggregate_type(type_t* t)
{
    if (is_array_type(t))
        return 1;

    // GCC seems to understand vector types as aggregates
    if (is_vector_type(t))
        return 1;

    // G++ seems to understand complex types as aggregates
    if (IS_CXX_LANGUAGE && is_complex_type(t))
        return 1;

    /*
       An aggregate is an array or a class (Clause 9) with no user-provided
       constructors (12.1), no brace-or-equal initializers for non-static data
       members (9.2), no private or protected non-static data members (Clause
       11), no base classes (Clause 10), and no virtual functions (10.3).
     */
    if (is_class_type(t))
    {
        type_t* class_type = get_actual_class_type(t);

        // No user provided constructors
        scope_entry_list_t* constructors = class_type_get_constructors(class_type);
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(constructors);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);

            if (symbol_entity_specs_get_is_user_declared(entry))
            {
                entry_list_iterator_free(it);
                entry_list_free(constructors);
                return 0;
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(constructors);

        scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(class_type);
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);

            CXX11_LANGUAGE()
            {
                // No initializer for nonstatic data member
                if (!nodecl_is_null(entry->value))
                {
                    entry_list_iterator_free(it);
                    entry_list_free(nonstatic_data_members);
                    return 0;
                }
            }

            // No private or protected non-static data members
            if (symbol_entity_specs_get_access(entry) == AS_PRIVATE
                    || symbol_entity_specs_get_access(entry) == AS_PROTECTED)
            {
                entry_list_iterator_free(it);
                entry_list_free(nonstatic_data_members);
                return 0;
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(nonstatic_data_members);

        // No base classes
        if (class_type_get_num_bases(class_type) != 0)
            return 0;

        scope_entry_list_t* member_functions = class_type_get_member_functions(class_type);
        for (it = entry_list_iterator_begin(member_functions);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);

            // No virtual functions
            if (symbol_entity_specs_get_is_virtual(entry))
            {
                entry_list_iterator_free(it);
                entry_list_free(member_functions);
                return 0;
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(member_functions);

        return 1;
    }

    return 0;
}

extern inline char class_type_is_pod(type_t* t)
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

        scope_entry_list_iterator_t* it = NULL;
        scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(class_type);
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);

            if (!is_pod_type(entry->type_information))
            {
                entry_list_iterator_free(it);
                entry_list_free(nonstatic_data_members);
                return 0;
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(nonstatic_data_members);

        scope_entry_list_t* copy_assignment_operators = class_type_get_copy_assignment_operators(class_type);
        for (it = entry_list_iterator_begin(copy_assignment_operators);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);

            if (symbol_entity_specs_get_is_user_declared(entry))
            {
                entry_list_iterator_free(it);
                entry_list_free(copy_assignment_operators);
                return 0;
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(copy_assignment_operators);

        scope_entry_t* destructor = class_type_get_destructor(class_type);
        if (destructor != NULL && symbol_entity_specs_get_is_user_declared(destructor))
            return 0;
    }

    CXX11_LANGUAGE()
    {
        if (!class_type_is_trivial(t)
                || !class_type_is_standard_layout(t))
            return 0;

        type_t* class_type = get_actual_class_type(t);

        scope_entry_list_iterator_t* it = NULL;
        scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(class_type);
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);

            if (!is_pod_type(entry->type_information))
            {
                entry_list_iterator_free(it);
                entry_list_free(nonstatic_data_members);
                return 0;
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(nonstatic_data_members);
    }

    return 1;
}

static char closure_of_simple_properties(type_t* t, char (*class_prop)(type_t*))
{
    if (is_error_type(t))
        return 0;

    if (is_scalar_type(t))
        return 1;

    if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
        return 0;

    if (is_array_type(t))
        return closure_of_simple_properties(array_type_get_element_type(t), class_prop);

    if (is_vector_type(t))
        return closure_of_simple_properties(vector_type_get_element_type(t), class_prop);

    if (is_class_type(t))
        return class_prop(t);

    internal_error("Unhandled type '%s'", print_declarator(t));
}

extern inline char is_pod_type(type_t* t)
{
    return closure_of_simple_properties(t, class_type_is_pod);
}

extern inline char is_trivially_copiable_type(type_t* t)
{
    return closure_of_simple_properties(t, class_type_is_trivially_copiable);
}

extern inline char is_standard_layout_type(type_t* t)
{
    return closure_of_simple_properties(t, class_type_is_standard_layout);
}

extern inline char type_is_runtime_sized(type_t* t)
{
    t = no_ref(t);

    if (is_array_type(t))
    {
        return array_type_is_vla(t);
    }
    else if (is_class_type(t))
    {
        type_t* class_type = get_actual_class_type(t);
            scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(class_type);
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* member = entry_list_iterator_current(it);

            if (type_is_runtime_sized(member->type_information))
            {
                entry_list_iterator_free(it);
                entry_list_free(nonstatic_data_members);
                return 1;
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(nonstatic_data_members);
    }

    return 0;
}

struct type_set_t
{
    type_t* type;
    struct type_set_t* prev;
};

static char type_depends_on_nonconstant_values_rec(type_t* t, struct type_set_t* type_set)
{
    // Avoid infinite recursion
    struct type_set_t* it_type = type_set;
    while (it_type != NULL)
    {
        if (it_type->type == t)
            return 0;
        it_type = it_type->prev;
    }

    struct type_set_t new_type_set = { t, type_set };

    if (is_array_type(t))
    {
        return array_type_is_vla(t)
               || (array_type_has_region(t)
                   && (nodecl_is_constant(array_type_get_region_lower_bound(t))
                       || nodecl_is_constant(
                              array_type_get_region_upper_bound(t))));
    }
    else if (is_class_type(t))
    {
        type_t* class_type = get_actual_class_type(t);
        scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(class_type);
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(nonstatic_data_members);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* member = entry_list_iterator_current(it);

            if (type_depends_on_nonconstant_values_rec(member->type_information, &new_type_set))
            {
                entry_list_iterator_free(it);
                entry_list_free(nonstatic_data_members);
                return 1;
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(nonstatic_data_members);
    }
    else if (is_any_reference_type(t))
    {
        return type_depends_on_nonconstant_values_rec(no_ref(t), &new_type_set);
    }
    else if (is_pointer_type(t))
    {
        return type_depends_on_nonconstant_values_rec(pointer_type_get_pointee_type(t), &new_type_set);
    }

    return 0;
}

extern inline char type_depends_on_nonconstant_values(type_t* t)
{
    return type_depends_on_nonconstant_values_rec(t, NULL);
}

extern inline _size_t type_get_size(type_t* t)
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
            scope_entry_t* named_type = named_type_get_symbol(t);
            type_t* alias_type = named_type->type_information;

            type_set_size(t, type_get_size(alias_type));

            // Do not override explicit alignments
            nodecl_t explicit_aligned = symbol_get_aligned_attribute(named_type);
            if (nodecl_is_null(explicit_aligned))
            {
                type_set_alignment(t, type_get_alignment(alias_type));
            }
            else if (nodecl_is_constant(explicit_aligned))
            {
                type_set_alignment(t,
                        const_value_cast_to_8(nodecl_get_constant(explicit_aligned)));
            }
            else
            {
                internal_error("'aligned' attribute of '%s' is not a constant when computing the size of a type",
                    get_qualified_symbol_name(named_type, named_type->decl_context));
            }

            CXX_LANGUAGE()
            {
                type_set_data_size(t, type_get_data_size(alias_type));
                class_type_set_non_virtual_size(t, 
                        class_type_get_non_virtual_size(alias_type));
                if (nodecl_is_null(explicit_aligned))
                {
                    class_type_set_non_virtual_align(t, 
                            class_type_get_non_virtual_align(alias_type));
                }
                else if (nodecl_is_constant(explicit_aligned))
                {
                    type_set_alignment(t,
                            const_value_cast_to_8(nodecl_get_constant(explicit_aligned)));
                }
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

extern inline _size_t type_get_alignment(type_t* t)
{
    // Note that we are not advancing typedefs because of attributes affecting types!
    if (!t->info->valid_size)
    {
        type_get_size(t);

        ERROR_CONDITION(!t->info->valid_size, 
                "Valid size has not been properly computed!", 0);
    }

    _size_t result = t->info->alignment;

    // It may happen that this alignment is being overridden by a type attribute
    int i;
    for (i = 0; i < t->info->num_gcc_attributes; i++)
    {
        if (strcmp(t->info->gcc_attributes[i].attribute_name, "aligned") == 0)
        {
            nodecl_t align_tree = nodecl_list_head(t->info->gcc_attributes[i].expression_list);
            if (nodecl_is_constant(align_tree))
            {
                result = (_size_t)const_value_cast_to_8(nodecl_get_constant(align_tree));
            }
        }
    }

    return result;
}

extern inline void type_set_size(type_t* t, _size_t size)
{
    ERROR_CONDITION(t == NULL, 
            "Invalid type", 0);

    t->info->size = size;
}

extern inline void type_set_alignment(type_t* t, _size_t alignment) 
{
    ERROR_CONDITION(t == NULL, 
            "Invalid type", 0);

    t->info->alignment = alignment;
}

extern inline void type_set_valid_size(type_t* t, char valid)
{
    ERROR_CONDITION(t == NULL,
            "Invalid type", 0);

    t->info->valid_size = valid;
}

extern inline _size_t type_get_data_size(type_t* t)
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

extern inline void type_set_data_size(type_t* t, _size_t data_size)
{
    C_LANGUAGE()
    {
        internal_error("This function is only for C++", 0);
    }

    t->info->data_size = data_size;
}

extern inline _size_t class_type_get_non_virtual_size(type_t* t)
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

extern inline void class_type_set_non_virtual_size(type_t* t, _size_t non_virtual_size)
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

extern inline _size_t class_type_get_non_virtual_align(type_t* t)
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

extern inline void class_type_set_non_virtual_align(type_t* t, _size_t non_virtual_align)
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

extern inline _size_t class_type_get_offset_virtual_base(type_t* t, scope_entry_t* virtual_base)
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

extern inline void class_type_set_offset_virtual_base(type_t* t, scope_entry_t* virtual_base, _size_t offset)
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
    virtual_base_class_info_t* virtual_base_info = NEW0(virtual_base_class_info_t);

    virtual_base_info->virtual_base = virtual_base;
    virtual_base_info->virtual_base_offset = offset;

    P_LIST_ADD(class_type->type->class_info->virtual_base_classes_list, 
            class_type->type->class_info->num_virtual_bases,
            virtual_base_info);
}

extern inline int class_type_get_num_virtual_bases_with_offset(type_t* t)
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

extern inline void class_type_get_virtual_base_with_offset_num(type_t* t, int num, 
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

void class_type_add_friend_symbol(type_t* t, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_class_type(t), "This is not an class type!", 0);

    type_t* class_type = get_actual_class_type(t);

    ERROR_CONDITION(
            entry->kind != SK_FRIEND_CLASS
            && entry->kind != SK_FRIEND_FUNCTION
            && entry->kind != SK_DEPENDENT_FRIEND_CLASS
            && entry->kind != SK_DEPENDENT_FRIEND_FUNCTION,
            "Invalid symbol to be registered as a friend", 0);

    class_type->type->class_info->friends = entry_list_add(class_type->type->class_info->friends, entry);
}

extern inline void class_type_add_inherited_constructor(type_t* t, scope_entry_t* entry)
{
    ERROR_CONDITION(!is_class_type(t), "This is not an class type!", 0);

    type_t* class_type = get_actual_class_type(t);

    class_type->type->class_info->inherited_constructors =
        entry_list_add(class_type->type->class_info->inherited_constructors, entry);
}

// This is like type_is_runtime_sized but allows pointers too while the former only allows arrays or classes
extern inline char is_variably_modified_type(type_t* t)
{
    CXX_LANGUAGE()
    {
        return 0;
    }

    if (is_pointer_type(t))
    {
        return is_variably_modified_type(pointer_type_get_pointee_type(t));
    }
    else
    {
        return type_is_runtime_sized(t);
    }
}

extern inline const char* print_type_str(type_t* t, const decl_context_t* decl_context)
{
    if (t == NULL)
    {
        return UNIQUESTR_LITERAL("< unknown type >");
    }
    else
    {
        return get_declaration_string(t,
                decl_context, /* symbol_name */"",
                /* initializer */ "",
                /* semicolon */ 0,
                /* num_parameter_names */ 0,
                /* parameter_names */ NULL,
                /* parameter_attributes */ NULL,
                /* is_parameter */ 0);
    }
}

extern inline const char* print_decl_type_str(type_t* t, const decl_context_t* decl_context, const char* name)
{
    if (t == NULL)
    {
        const char* c = NULL;
        uniquestr_sprintf(&c, "< unknown type > %s\n", name);
        return c;
    }
    else if (is_unresolved_overloaded_type(t))
    {
        scope_entry_list_t* overload_set = unresolved_overloaded_type_get_overload_set(t);
        if (entry_list_size(overload_set) == 1)
        {
            type_t* used_type = NULL;
            scope_entry_t* item = entry_list_head(overload_set);

            if (!symbol_entity_specs_get_is_member(item)
                    || symbol_entity_specs_get_is_static(item))
            {
                used_type = lvalue_ref(item->type_information);
            }
            else
            {
                used_type = get_pointer_to_member_type(item->type_information,
                        symbol_entity_specs_get_class_type(item));
            }
            return print_decl_type_str(used_type, decl_context, name);
        }
        else
        {
            return UNIQUESTR_LITERAL("<unresolved overload>");
        }
        entry_list_free(overload_set);
    }
    else if (is_braced_list_type(t))
    {
        return UNIQUESTR_LITERAL("<brace-enclosed initializer list>");
    }
    else if (is_error_type(t))
    {
        return UNIQUESTR_LITERAL("<error-type>");
    }
    else
    {
        return get_declaration_string(t,
                decl_context, /* symbol_name */ name,
                /* initializer */ "",
                /* semicolon */ 0,
                /* num_parameter_names */ 0,
                /* parameter_names */ NULL,
                /* parameter_attributes */ NULL,
                /* is_parameter */ 0);
    }
}

// This function, given a type returns the type-specifier related to it
//
// e.g T (*a)[3]  returns 'T'
//     const T& f(int)   returns 'const T' 
//
// so the type-specifier part of a type-id plus cv-qualifiers, if any
static type_t* get_foundation_type(type_t* t)
{
    if (t == NULL)
    {
        return NULL;
    }
    else if (is_non_derived_type(t))
    {
        if (is_named_type(t)
                && named_type_get_symbol(t)->kind == SK_TYPEDEF
                // These are the only typedefs that we always advance
                && symbol_entity_specs_get_is_template_parameter(named_type_get_symbol(t)))
        {
            return get_foundation_type(named_type_get_symbol(t)->type_information);
        }
        return t;
    }
    else if (is_function_type(t))
    {
        if (function_type_get_has_trailing_return(t))
            return get_auto_type();
        else
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
    else if (is_pack_type(t))
    {
        return get_foundation_type(pack_type_get_packed_type(t));
    }
    else if (is_unresolved_overloaded_type(t))
    {
        return t;
    }
    else if (is_sequence_of_types(t))
    {
        return t;
    }
    else if (is_error_type(t))
    {
        return _error_type;
    }
    else if (is_unknown_dependent_type(t))
    {
        return _dependent_type;
    }
    else if (is_braced_list_type(t))
    {
        return t;
    }
    else if (is_auto_type(t))
    {
        return t;
    }
    else if (is_decltype_auto_type(t))
    {
        return t;
    }
    else if (is_gxx_underlying_type(t))
    {
        return t;
    }
    else if (is_ellipsis_type(t))
    {
        return t;
    }
    internal_error("Cannot get foundation type of type '%s'", print_declarator(t));
}


// This is only for Fortran
static type_t* _implicit_none_type = NULL;

extern inline type_t* get_implicit_none_type(void)
{
    if (_implicit_none_type == NULL)
    {
        _implicit_none_type = get_simple_type();
        _implicit_none_type->type->kind = STK_BUILTIN_TYPE;
        _implicit_none_type->type->builtin_type = BT_VOID;
        _implicit_none_type->info->is_incomplete = 1;
    }

    return _implicit_none_type;
}

extern inline char is_implicit_none_type(type_t* t)
{
    return t == _implicit_none_type;
}

// Use this for embedding in a TL::Source
// This is not for prettyprinting!
extern inline const char* type_to_source(type_t* t)
{
    const char* pack = pack_pointer("type", (void*)t);

    const char* c = NULL;

    uniquestr_sprintf(&c, "%s%s%s", 
            "@TYPE-LITERAL-REF@(", pack, ")");

    return c;
}

static inline type_t* type_deep_copy_class(
        type_t* orig,
        scope_entry_t* dest,
        const decl_context_t* new_decl_context, 
        symbol_map_t* symbol_map)
{
    // Note, at this point symbol members are already mapped, here we are just
    // adding them to the class type as members
    ERROR_CONDITION(dest == NULL, "Invalid symbol", 0);

    dest->type_information = get_new_class_type(
            new_decl_context,
            class_type_get_class_kind(orig));

    // We need to abuse a bit of the scope map to retrieve the new class context
    decl_context_t* inner_decl_context = (decl_context_t*)
        symbol_map->map(symbol_map, (scope_entry_t*)class_type_get_inner_context(orig));

    class_type_set_inner_context(dest->type_information, inner_decl_context);


    // Duplicate all members
    // FIXME: duplicate them in declaration order!
    scope_entry_list_t* entry_list = class_type_get_members(orig);
    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* member = entry_list_iterator_current(it);
        scope_entry_t* new_member = symbol_map->map(symbol_map, member);

        ERROR_CONDITION(member == new_member, "Member '%s' not mapped\n",
                get_qualified_symbol_name(member, member->decl_context));

        class_type_add_member(dest->type_information, new_member,
                new_member->decl_context,
                /* is_definition */ 1);
    }
    entry_list_iterator_free(it);
    entry_list_free(entry_list);

    // Map bases
    int num_bases = class_type_get_num_bases(orig);
    int i;
    for (i = 0; i < num_bases; i++)
    {
        char is_virtual, is_dependent, is_expansion;
        access_specifier_t access_specifier;

        scope_entry_t* current_base = class_type_get_base_num(orig, i,
                &is_virtual,
                &is_dependent,
                &is_expansion,
                &access_specifier);

        scope_entry_t* mapped_base = symbol_map->map(symbol_map, current_base);

        class_type_add_base_class(
                dest->type_information,
                mapped_base,
                is_virtual,
                is_dependent,
                is_expansion,
                access_specifier);
    }

    class_type_set_is_abstract(dest->type_information, class_type_is_abstract(orig));
    class_type_set_is_lambda(dest->type_information, class_type_is_lambda(orig));
    class_type_set_is_packed(dest->type_information, class_type_is_packed(orig));

    set_is_complete_type(dest->type_information, is_complete_type(orig));

    return dest->type_information;
}

extern inline type_t* type_deep_copy_compute_maps(
    type_t* orig,
    scope_entry_t* dest,
    const decl_context_t* new_decl_context,
    symbol_map_t* symbol_map,
    nodecl_deep_copy_map_t* nodecl_deep_copy_map,
    symbol_deep_copy_map_t* symbol_deep_copy_map)
{
    if (orig == NULL)
        return NULL;

    type_t* result = orig;

    if (is_named_type(orig))
    {
        scope_entry_t* symbol = named_type_get_symbol(orig);
        symbol = symbol_map->map(symbol_map, symbol);

        if (is_indirect_type(orig))
        {
            if (is_mutable_indirect_type(orig))
            {
                result = get_mutable_indirect_type(symbol);
            }
            else
                result = get_immutable_indirect_type(symbol);
        }
        else
        {
            result = get_user_defined_type(symbol);
        }
    }
    else if (is_pointer_type(orig))
    {
        type_t* pointee = pointer_type_get_pointee_type(orig);
        pointee = type_deep_copy_compute_maps(pointee,
                /* dest */ NULL,
                new_decl_context, symbol_map,
                nodecl_deep_copy_map, symbol_deep_copy_map);
        result = get_pointer_type(pointee);
    }
    else if (is_pointer_to_member_type(orig))
    {
        type_t* pointee = pointer_type_get_pointee_type(orig);
        pointee = type_deep_copy_compute_maps(pointee,
                /* dest */ NULL,
                new_decl_context, symbol_map,
                nodecl_deep_copy_map, symbol_deep_copy_map);

        type_t* class_type = pointer_to_member_type_get_class_type(orig);
        class_type = type_deep_copy_compute_maps(class_type,
                /* dest */ NULL,
                new_decl_context, symbol_map,
                nodecl_deep_copy_map, symbol_deep_copy_map);

        result = get_pointer_to_member_type(pointee, class_type);
    }
    else if (is_rebindable_reference_type(orig))
    {
        type_t* ref_type = reference_type_get_referenced_type(orig);
        ref_type = type_deep_copy_compute_maps(ref_type,
                /* dest */ NULL,
                new_decl_context, symbol_map,
                nodecl_deep_copy_map, symbol_deep_copy_map);

        result = get_rebindable_reference_type(ref_type);
    }
    else if (is_lvalue_reference_type(orig))
    {
        type_t* ref_type = reference_type_get_referenced_type(orig);
        ref_type = type_deep_copy_compute_maps(ref_type,
                /* dest */ NULL,
                new_decl_context, symbol_map,
                nodecl_deep_copy_map, symbol_deep_copy_map);

        result = get_lvalue_reference_type(ref_type);
    }
    else if (is_rvalue_reference_type(orig))
    {
        type_t* ref_type = reference_type_get_referenced_type(orig);
        ref_type = type_deep_copy_compute_maps(ref_type,
                /* dest */ NULL,
                new_decl_context, symbol_map,
                nodecl_deep_copy_map, symbol_deep_copy_map);

        result = get_rvalue_reference_type(ref_type);
    }
    else if (is_array_type(orig))
    {
        type_t* element_type = array_type_get_element_type(orig);
        element_type = type_deep_copy_compute_maps(element_type,
                /* dest */ NULL,
                new_decl_context, symbol_map,
                nodecl_deep_copy_map, symbol_deep_copy_map);

        if (array_type_is_string_literal(orig))
        {
            nodecl_t array_size = array_type_get_array_size_expr(orig);
            array_size = nodecl_deep_copy_compute_maps(array_size, new_decl_context, symbol_map,
                    nodecl_deep_copy_map, symbol_deep_copy_map);
            get_array_type_for_literal_string(element_type, array_size, new_decl_context);
        }
        else if ((IS_C_LANGUAGE
                    || IS_CXX_LANGUAGE)
                && !array_type_has_region(orig))
        {
            nodecl_t array_size = array_type_get_array_size_expr(orig);
            array_size = nodecl_deep_copy_compute_maps(array_size, new_decl_context, symbol_map,
                    nodecl_deep_copy_map, symbol_deep_copy_map);

            result = get_array_type(
                    element_type,
                    array_size,
                    new_decl_context);
        }
        else if (IS_FORTRAN_LANGUAGE
                && !array_type_has_region(orig))
        {
            nodecl_t lower_bound = array_type_get_array_lower_bound(orig);
            nodecl_t upper_bound = array_type_get_array_upper_bound(orig);

            lower_bound = nodecl_deep_copy_compute_maps(lower_bound, new_decl_context, symbol_map,
                    nodecl_deep_copy_map, symbol_deep_copy_map);
            upper_bound = nodecl_deep_copy_compute_maps(upper_bound, new_decl_context, symbol_map,
                    nodecl_deep_copy_map, symbol_deep_copy_map);

            bool has_descriptor = array_type_with_descriptor(orig);

            if (!has_descriptor)
            {
                result = get_array_type_bounds(
                        element_type,
                        lower_bound,
                        upper_bound,
                        new_decl_context);
            }
            else
            {
                result = get_array_type_bounds_with_descriptor(
                        element_type,
                        lower_bound,
                        upper_bound,
                        new_decl_context);
            }
        }
        else if (array_type_has_region(orig))
        {
            nodecl_t lower_bound = array_type_get_array_lower_bound(orig);
            nodecl_t upper_bound = array_type_get_array_upper_bound(orig);

            lower_bound = nodecl_deep_copy_compute_maps(lower_bound, new_decl_context, symbol_map,
                    nodecl_deep_copy_map, symbol_deep_copy_map);
            upper_bound = nodecl_deep_copy_compute_maps(upper_bound, new_decl_context, symbol_map,
                    nodecl_deep_copy_map, symbol_deep_copy_map);

            nodecl_t region_lower_bound = array_type_get_region_lower_bound(orig);
            nodecl_t region_upper_bound = array_type_get_region_upper_bound(orig);
            nodecl_t region_stride = array_type_get_region_stride(orig);

            region_lower_bound = nodecl_deep_copy_compute_maps(region_lower_bound, new_decl_context, symbol_map,
                    nodecl_deep_copy_map, symbol_deep_copy_map);
            region_upper_bound = nodecl_deep_copy_compute_maps(region_upper_bound, new_decl_context, symbol_map,
                    nodecl_deep_copy_map, symbol_deep_copy_map);
            region_stride = nodecl_deep_copy_compute_maps(region_stride, new_decl_context, symbol_map,
                    nodecl_deep_copy_map, symbol_deep_copy_map);

            result = get_array_type_bounds_with_regions(element_type,
                    lower_bound,
                    upper_bound,
                    new_decl_context,
                    nodecl_make_range(region_lower_bound, region_upper_bound, region_stride,
                        get_ptrdiff_t_type(), make_locus("", 0, 0)),
                    new_decl_context);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    else if (is_function_type(orig))
    {
        type_t* return_type = function_type_get_return_type(orig);
        return_type = type_deep_copy_compute_maps(return_type,
                /* dest */ NULL,
                new_decl_context, symbol_map,
                nodecl_deep_copy_map, symbol_deep_copy_map);

        if (function_type_get_lacking_prototype(orig))
        {
            result = get_nonproto_function_type(return_type, 
                    function_type_get_num_parameters(orig));
        }
        else
        {
            int i, N = function_type_get_num_parameters(orig), P = N;

            parameter_info_t param_info[N+1];
            memset(param_info, 0, sizeof(param_info));

            if (function_type_get_has_ellipsis(orig))
            {
                param_info[N-1].is_ellipsis = 1;
                param_info[N-1].type_info = get_ellipsis_type();
                param_info[N-1].nonadjusted_type_info = NULL;
                P = N - 1;
            }

            for (i = 0; i < P; i++)
            {
                param_info[i].type_info = type_deep_copy_compute_maps(function_type_get_parameter_type_num(orig, i),
                        /* dest */ NULL,
                        new_decl_context, symbol_map,
                        nodecl_deep_copy_map,
                        symbol_deep_copy_map);
            }

            result = get_new_function_type(return_type, param_info, N, function_type_get_ref_qualifier(orig));
        }
    }
    else if (is_vector_type(orig))
    {
        type_t * element_type = vector_type_get_element_type(orig);
        element_type = type_deep_copy_compute_maps(element_type,
                /* dest */ NULL,
                new_decl_context, symbol_map,
                nodecl_deep_copy_map, symbol_deep_copy_map);

        result = get_vector_type_by_bytes(
                element_type,
                vector_type_get_vector_size_in_bytes(orig));
    }
    else if (is_class_type(orig))
    {
        result = type_deep_copy_class(
                orig,
                dest,
                new_decl_context,
                symbol_map);
    }

    // GCC attributes
    int num_attrs = 0;
    gcc_attribute_t* gcc_attrs = NULL;
    variant_type_get_gcc_attributes(orig, &num_attrs, &gcc_attrs);
    int i;
    for (i = 0; i < num_attrs; i++)
    {
        result = get_variant_type_add_gcc_attribute(result, gcc_attrs[i]);
    }

    // Microsoft
    num_attrs = 0;
    gcc_attrs = NULL;
    variant_type_get_ms_attributes(orig, &num_attrs, &gcc_attrs);
    for (i = 0; i < num_attrs; i++)
    {
        result = get_variant_type_add_ms_attribute(result, gcc_attrs[i]);
    }

    // Fortran interop
    if (variant_type_is_interoperable(orig))
    {
        result = get_variant_type_interoperable(result);
    }

    // cv-qualifiers
    result = get_cv_qualified_type(result, get_cv_qualifier(orig) | get_cv_qualifier(result));

    return result;
}

extern inline type_t* type_deep_copy(type_t* orig,
        const decl_context_t* new_decl_context, 
        symbol_map_t* symbol_map)
{
    return type_deep_copy_compute_maps(
            orig,
            /* dest */ NULL,
            new_decl_context,
            symbol_map,
            /* nodecl_deep_copy_map_t */ NULL,
            /* symbol_deep_copy_map_t */ NULL);
}

static dhash_ptr_t *_interoperable_hash = NULL;

// This function constructs an interoperable variant
// This is used only in Fortran
extern inline type_t* get_variant_type_interoperable(type_t* t)
{
    if (t == NULL)
        return NULL;

    if (t->info->is_interoperable)
        return t;

    if (_interoperable_hash == NULL)
    {
        _interoperable_hash = dhash_ptr_new(5);
    }

    type_t* result = dhash_ptr_query(_interoperable_hash, (const char*)t);

    if (result == NULL)
    {
        result = copy_type_for_variant(t);
        result->info->is_interoperable = 1;

        dhash_ptr_insert(_interoperable_hash, (const char*)t, result);
    }

    return result;
}

extern inline char variant_type_is_interoperable(type_t* t)
{
    return (t != NULL
            && t->info != NULL
            && t->info->is_interoperable);
}

static char _initialized_generics = 0;
static type_t* _generic_types[MCXX_MAX_GENERIC_TYPES];
extern inline type_t* get_generic_type(int num)
{
    if (!_initialized_generics)
    {
        int i;
        for (i = 0; i < MCXX_MAX_GENERIC_TYPES; i++)
        {
            _generic_types[i] = get_simple_type();
        }
    }

    ERROR_CONDITION((num < 0 || num > MCXX_MAX_GENERIC_TYPES), "Invalid generic number", 0);

    return _generic_types[num];
}

extern inline char is_generic_type(type_t* t)
{
    if (t == NULL)
        return 0;

    t = get_unqualified_type(t);

    int i;
    for (i = 0; i < MCXX_MAX_GENERIC_TYPES; i++)
    {
        if (_generic_types[i] == t)
            return 1;
    }

    return 0;
}

extern inline int generic_type_get_num(type_t* t)
{
    ERROR_CONDITION(!is_generic_type(t), "Invalid type", 0);

    t = get_unqualified_type(t);

    int i;
    for (i = 0; i < MCXX_MAX_GENERIC_TYPES; i++)
    {
        if (_generic_types[i] == t)
            return i;
    }

    return -1;
}

static void mask_type_compute_underlying_type(type_t* t)
{
    ERROR_CONDITION(!is_mask_type(t),
            "This is not a mask type", 0);
    t = advance_over_typedefs(t);

    type_t* unsigned_integers[] = {
        get_unsigned_char_type(),
        get_unsigned_short_int_type(),
        get_unsigned_int_type(),
        get_unsigned_long_int_type(),
        get_unsigned_long_long_int_type(),
        NULL,
    };

    type_t** it = &unsigned_integers[0];
    unsigned int num_bits = mask_type_get_num_bits(t);

    while (*it != NULL)
    {
        if (num_bits <= (8 * type_get_size(*it)))
        {
            t->type->vector_element = *it;

            // Share the sizing info with the underlying type
            DELETE(t->info);
            t->info = t->type->vector_element->info;

            break;
        }

        it++;
    }
}

extern inline type_t* get_mask_type(unsigned int mask_size_bits)
{
    static rb_red_blk_tree *_mask_hash = NULL;

    if (_mask_hash == NULL)
    {
        _mask_hash = rb_tree_create(uint_comp, null_dtor, null_dtor);
    }

    type_t* result = (type_t*)rb_tree_query_uint(_mask_hash, mask_size_bits);

    if (result == NULL)
    {
        result = get_simple_type();
        result->type->kind = STK_MASK;
        result->type->vector_size = mask_size_bits;

        int *k = NEW(int);
        *k = mask_size_bits;

        mask_type_compute_underlying_type(result);

        rb_tree_insert(_mask_hash, k, result);
    }

    return result;
}

extern inline char is_mask_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_MASK);
}


extern inline type_t* mask_type_get_underlying_type(type_t* t)
{
    ERROR_CONDITION(!is_mask_type(t),
            "This is not a mask type", 0);
    t = advance_over_typedefs(t);

    unsigned int num_bits = t->type->vector_size;

    ERROR_CONDITION(t->type->vector_element == NULL,
            "Not found a suitable unsigned integer type for a mask of '%d' bits\n",
            num_bits);

    return t->type->vector_element;
}

unsigned int mask_type_get_num_bits(type_t* t)
{
    ERROR_CONDITION(!is_mask_type(t), "This is not a mask type", 0);

    return t->type->vector_size;
}

static type_t* _hollerith_type = NULL;

extern inline type_t* get_hollerith_type(void)
{
    if (_hollerith_type == NULL)
    {
        _hollerith_type = get_simple_type();
        _hollerith_type->type->kind = STK_HOLLERITH;
    }

    return _hollerith_type;
}

extern inline char is_hollerith_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_DIRECT
            && t->type->kind == STK_HOLLERITH);
}

extern inline type_t* get_pack_type(type_t* t)
{
    ERROR_CONDITION(t == NULL, "Invalid NULL type", 0);
    ERROR_CONDITION(is_pack_type(t), "Cannot build a pack type of a pack type", 0);

    static dhash_ptr_t *_pack_types = NULL;

    if (_pack_types == NULL)
    {
        _pack_types = dhash_ptr_new(5);
    }

    type_t* pack_type = dhash_ptr_query(_pack_types, (const char*)t);

    if (pack_type == NULL)
    {
        pack_type = new_empty_type();
        pack_type->kind = TK_PACK;
        pack_type->unqualified_type = pack_type;
        pack_type->pack_type = NEW0(pack_type_info_t);
        pack_type->pack_type->packed = t;

        pack_type->info->is_dependent = is_dependent_type(t);

        dhash_ptr_insert(_pack_types, (const char*)t, pack_type);
    }

    return pack_type;
}

extern inline char is_pack_type(type_t* t)
{
    t = advance_over_typedefs(t);
    return (t != NULL
            && t->kind == TK_PACK);
}

extern inline type_t* pack_type_get_packed_type(type_t* t)
{
    ERROR_CONDITION(!is_pack_type(t), "Invalid type", 0);
    t = advance_over_typedefs(t);

    return (t->pack_type->packed);
}

extern inline type_t* get_sequence_of_types(int num_types, type_t** types)
{
    ERROR_CONDITION(num_types < 0, "Invalid number of types (%d)", num_types);

    // Special case for empty sequences
    if (num_types == 0)
    {
        static type_t* _empty_sequence = NULL;

        if (_empty_sequence == NULL)
        {
            _empty_sequence = new_empty_type();
            _empty_sequence->kind = TK_SEQUENCE;
            _empty_sequence->unqualified_type = _empty_sequence;
            _empty_sequence->sequence_type = NEW0(sequence_type_info_t);
        }

        return _empty_sequence;
    }

    static type_trie_t *_sequence_types_trie = NULL;
    if (_sequence_types_trie == NULL)
    {
        _sequence_types_trie = allocate_type_trie();
    }

    char any_is_dependent = 0;
    int i;
    for (i = 0; i < num_types; i++)
    {
        ERROR_CONDITION(is_sequence_of_types(types[i]), "Cannot have a sequence inside another sequence type", 0);
        // ERROR_CONDITION(types[i] == NULL, "Invalid NULL type", 0);
        any_is_dependent = any_is_dependent || is_dependent_type(types[i]);
    }

    type_t* result = (type_t*)lookup_type_trie(_sequence_types_trie, (const type_t**)types, num_types);

    if (result == NULL)
    {
        result = new_empty_type();
        result->unqualified_type = result;
        result->kind = TK_SEQUENCE;
        result->sequence_type = NEW0(sequence_type_info_t);
        result->sequence_type->num_types = num_types;
        result->sequence_type->types = NEW_VEC(type_t*, num_types);
        memcpy(result->sequence_type->types, types,
                sizeof(*result->sequence_type->types) * num_types);

        result->info->is_dependent = any_is_dependent;

        insert_type_trie(_sequence_types_trie, (const type_t**)types, num_types, result);
    }

    return result;
}

static void flatten_type(type_t* t, type_t*** flattened_type_seq, int* flattened_num_types)
{
    if (!is_sequence_of_types(t))
    {
        P_LIST_ADD(*flattened_type_seq, *flattened_num_types, t);
    }
    else
    {
        int i, N = sequence_of_types_get_num_types(t);

        for (i = 0; i < N; i++)
        {
            flatten_type(
                    sequence_of_types_get_type_num(t, i),
                    flattened_type_seq,
                    flattened_num_types);
        }
    }
}

extern inline type_t* get_sequence_of_types_flattened(int num_types, type_t** types)
{
    int flattened_num_types = 0;
    type_t** flattened_type_seq = NULL;

    int i;
    for (i = 0; i < num_types; i++)
    {
        flatten_type(types[i],
                &flattened_type_seq,
                &flattened_num_types);
    }

    type_t* result = get_sequence_of_types(flattened_num_types, flattened_type_seq);

    DELETE(flattened_type_seq);

    return result;
}

extern inline int sequence_of_types_get_num_types(type_t* t)
{
    t = advance_over_typedefs(t);
    ERROR_CONDITION(!is_sequence_of_types(t), "Invalid type", 0);
    return t->sequence_type->num_types;
}

extern inline char is_sequence_of_types(type_t* t)
{
    t = advance_over_typedefs(t);
    return t != NULL && t->kind == TK_SEQUENCE;
}

extern inline type_t* sequence_of_types_get_type_num(type_t* t, int num)
{
    t = advance_over_typedefs(t);
    ERROR_CONDITION(!is_sequence_of_types(t), "Invalid type", 0);
    ERROR_CONDITION ((num < 0) || (num >= t->sequence_type->num_types),
            "Invalid type number in sequence type", 0);

    return t->sequence_type->types[num];
}

extern inline type_t* get_sequence_of_types_append_type(type_t* seq_type, type_t* type)
{
    if (seq_type == NULL)
    {
        if (!is_sequence_of_types(type))
            return get_sequence_of_types(1, &type);
        else
            return type;
    }
    ERROR_CONDITION(!is_sequence_of_types(seq_type), "This is not a sequence type", 0);

    int n = sequence_of_types_get_num_types(seq_type);
    if (n == 0)
    {
        if (!is_sequence_of_types(type))
        {
            return get_sequence_of_types(1, &type);
        }
        else
        {
            return type;
        }
    }

    seq_type = advance_over_typedefs(seq_type);

    int m = 1;
    if (is_sequence_of_types(type))
        m = sequence_of_types_get_num_types(type);
    if (m == 0)
        return seq_type;

    type_t* types[n + m];
    memcpy(types, seq_type->sequence_type->types, n*sizeof(*types));
    if (is_sequence_of_types(type))
    {
        type = advance_over_typedefs(type);
        memcpy(&types[n], type->sequence_type->types, m*sizeof(types));
    }
    else
        types[n] = type;

    return get_sequence_of_types(n + m, types);
}

static type_t* _auto = NULL;
extern inline type_t* get_auto_type(void)
{
    if (_auto == NULL)
    {
        _auto = new_empty_type();
        _auto->kind = TK_AUTO;
        _auto->unqualified_type = _auto;
        _auto->info->is_dependent = 0;
    }

    return _auto;
}

static type_t* _decltype_auto = NULL;
extern inline type_t* get_decltype_auto_type(void)
{
    if (_decltype_auto == NULL)
    {
        _decltype_auto = new_empty_type();
        _decltype_auto->kind = TK_DECLTYPE_AUTO;
        _decltype_auto->unqualified_type = _decltype_auto;
        _decltype_auto->info->is_dependent = 0;
    }

    return _decltype_auto;
}

extern inline char is_auto_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return t != NULL
        && t->kind == TK_AUTO;
}

extern inline char type_is_derived_from_auto(type_t* t)
{
    if (is_auto_type(t))
    {
        return 1;
    }
    else if (is_pointer_type(t))
    {
        return type_is_derived_from_auto(pointer_type_get_pointee_type(t));
    }
    else if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        return type_is_derived_from_auto(reference_type_get_referenced_type(t));
    }
    else if (is_array_type(t))
    {
        return type_is_derived_from_auto(array_type_get_element_type(t));
    }
    else if (is_function_type(t))
    {
        return type_is_derived_from_auto(function_type_get_return_type(t));

    }
    else if (is_vector_type(t))
    {
        return type_is_derived_from_auto(vector_type_get_element_type(t));
    }
    else
    {
        return 0;
    }
}

extern inline char is_decltype_auto_type(type_t* t)
{
    t = advance_over_typedefs(t);

    return t != NULL
        && t->kind == TK_DECLTYPE_AUTO;
}

extern inline parameter_info_t get_parameter_info_for_type(type_t* t)
{
    parameter_info_t parameter_info;
    memset(&parameter_info, 0, sizeof(parameter_info));

    parameter_info.nonadjusted_type_info = t;
    if (is_function_type(t))
        t = get_pointer_type(t);
    else if (is_array_type(t))
        t = get_pointer_type(array_type_get_element_type(t));

    parameter_info.type_info = get_unqualified_type(t);

    return parameter_info;
}

extern inline type_t* get_variant_type_add_gcc_attribute(type_t* t, gcc_attribute_t attr)
{
    cv_qualifier_t cv_qualif = get_cv_qualifier(t);

    // We do not use get_unqualified_type because it preserves restrict
    t = get_cv_qualified_type(t, CV_NONE);

    type_t* result = copy_type_for_variant(t);

    ERROR_CONDITION(!nodecl_is_null(attr.expression_list) && !nodecl_is_list(attr.expression_list),
            "Attribute value if not empty must be a list", 0);

    int i;
    char found = 0;
    for (i = 0; i < result->info->num_gcc_attributes; i++)
    {
        if (strcmp(result->info->gcc_attributes[i].attribute_name, attr.attribute_name) == 0)
        {
            found = 1;
            // Update
            result->info->gcc_attributes[i] = attr;
            break;
        }
    }

    if (!found)
    {
        // Add
        P_LIST_ADD(result->info->gcc_attributes, result->info->num_gcc_attributes, attr);
    }

    result = get_cv_qualified_type(result, cv_qualif);

    return result;
}

extern inline type_t* get_variant_type_add_ms_attribute(type_t* t, gcc_attribute_t attr)
{
    cv_qualifier_t cv_qualif = get_cv_qualifier(t);

    // We do not use get_unqualified_type because it preserves restrict
    t = get_cv_qualified_type(t, CV_NONE);

    type_t* result = copy_type_for_variant(t);

    int i;
    char found = 0;
    for (i = 0; i < result->info->num_ms_attributes; i++)
    {
        if (strcmp(result->info->ms_attributes[i].attribute_name, attr.attribute_name) == 0)
        {
            found = 1;
            // Update
            result->info->ms_attributes[i] = attr;
            break;
        }
    }

    if (!found)
    {
        // Add
        P_LIST_ADD(result->info->ms_attributes, result->info->num_ms_attributes, attr);
    }

    result = get_cv_qualified_type(result, cv_qualif);

    return result;
}

extern inline void variant_type_get_gcc_attributes(type_t* t, int* num_attrs, gcc_attribute_t** attrs)
{
    *num_attrs = t->info->num_gcc_attributes;
    *attrs = t->info->gcc_attributes;
}

extern inline void variant_type_get_ms_attributes(type_t* t, int* num_attrs, gcc_attribute_t** attrs)
{
    *num_attrs = t->info->num_ms_attributes;
    *attrs = t->info->ms_attributes;
}

extern inline void get_packs_in_type(type_t* pack_type,
        scope_entry_t*** packs_to_expand,
        int *num_packs_to_expand)
{
    if (is_named_type(pack_type))
    {
        scope_entry_t* sym = named_type_get_symbol(pack_type);
        if (sym->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                || sym->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
        {
            P_LIST_ADD_ONCE(*packs_to_expand, *num_packs_to_expand, sym);
            return;
        }

        if (is_template_specialized_type(sym->type_information))
        {
            type_t* template_type =
                template_specialized_type_get_related_template_type(sym->type_information);
            template_parameter_list_t* template_parameters =
                template_specialized_type_get_template_arguments(sym->type_information);
            scope_entry_t* template_related_symbol =
                template_type_get_related_symbol(template_type);


            if (template_related_symbol != NULL
                    && template_related_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
            {
                P_LIST_ADD_ONCE(*packs_to_expand, *num_packs_to_expand, template_related_symbol);
            }

            int i;
            for (i = 0; i < template_parameters->num_parameters; i++)
            {
                template_parameter_value_t* v = template_parameters->arguments[i];

                enum template_parameter_kind k = template_parameter_kind_get_base_kind(v->kind);

                if (k == TPK_TYPE
                        || k == TPK_TEMPLATE)
                {
                    get_packs_in_type(v->type, packs_to_expand, num_packs_to_expand);
                }
                else if (k == TPK_NONTYPE)
                {
                    get_packs_in_type(v->type, packs_to_expand, num_packs_to_expand);
                    get_packs_in_expression(v->value, packs_to_expand, num_packs_to_expand);
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }
            }
        }
    }
    else if (is_pointer_type(pack_type))
    {
        get_packs_in_type(pointer_type_get_pointee_type(pack_type),
                packs_to_expand,
                num_packs_to_expand);
    }
    else if (is_rvalue_reference_type(pack_type)
            || is_lvalue_reference_type(pack_type))
    {
        get_packs_in_type(reference_type_get_referenced_type(pack_type),
                packs_to_expand,
                num_packs_to_expand);
    }
    else if (is_array_type(pack_type))
    {
        get_packs_in_expression(array_type_get_array_size_expr(pack_type),
                packs_to_expand,
                num_packs_to_expand);
        get_packs_in_type(array_type_get_element_type(pack_type),
                packs_to_expand,
                num_packs_to_expand);
    }
    else if (is_vector_type(pack_type))
    {
        get_packs_in_type(vector_type_get_element_type(pack_type),
                packs_to_expand,
                num_packs_to_expand);
    }
    else if (is_function_type(pack_type))
    {
        get_packs_in_type(function_type_get_return_type(pack_type),
                packs_to_expand,
                num_packs_to_expand);

        int last = function_type_get_num_parameters(pack_type);

        char has_ellipsis = function_type_get_has_ellipsis(pack_type);

        if (has_ellipsis)
            last--;

        int i;
        for (i = 0; i < last; i++)
        {
            type_t* param_type = function_type_get_parameter_type_num(pack_type, i);

            get_packs_in_type(param_type, packs_to_expand, num_packs_to_expand);
        }
    }
    else if (is_dependent_typename_type(pack_type))
    {
        scope_entry_t *dependent_entry = NULL;
        nodecl_t dependent_parts = nodecl_null();

        dependent_typename_get_components(pack_type,
                &dependent_entry,
                &dependent_parts);

        get_packs_in_type(
                get_user_defined_type(dependent_entry),
                packs_to_expand,
                num_packs_to_expand);

        nodecl_t nodecl_nested_parts = nodecl_get_child(dependent_parts, 0);

        int num_items = 0;
        nodecl_t* dep_parts = nodecl_unpack_list(nodecl_nested_parts, &num_items);

        int i;
        for (i = 0; i < num_items; i++)
        {
            if (nodecl_get_kind(dep_parts[i]) == NODECL_CXX_DEP_TEMPLATE_ID)
            {
                template_parameter_list_t* template_parameters =
                    nodecl_get_template_parameters(dep_parts[i]);

                int j;
                for (j = 0; j < template_parameters->num_parameters; j++)
                {
                    template_parameter_value_t* v = template_parameters->arguments[j];

                    enum template_parameter_kind k = template_parameter_kind_get_base_kind(v->kind);

                    if (k == TPK_TYPE
                            || k == TPK_TEMPLATE)
                    {
                        get_packs_in_type(v->type, packs_to_expand, num_packs_to_expand);
                    }
                    else if (k == TPK_NONTYPE)
                    {
                        get_packs_in_type(v->type, packs_to_expand, num_packs_to_expand);
                        get_packs_in_expression(v->value, packs_to_expand, num_packs_to_expand);
                    }
                    else
                    {
                        internal_error("Code unreachable", 0);
                    }
                }
            }
        }

        DELETE(dep_parts);
    }
    else if (is_sequence_of_types(pack_type))
    {
        int i, num = sequence_of_types_get_num_types(pack_type);

        for (i = 0; i < num; i++)
        {
            get_packs_in_type(sequence_of_types_get_type_num(pack_type, i), packs_to_expand, num_packs_to_expand);
        }
    }
}

static dhash_ptr_t *_atomic_types_hash = NULL;

extern inline type_t* get_variant_type_atomic(type_t* t)
{
    if (_atomic_types_hash == NULL)
    {
        _atomic_types_hash = dhash_ptr_new(5);
    }

    cv_qualifier_t cv_qualif = get_cv_qualifier(t);
    // We do not use get_unqualified_type because it preserves restrict
    t = get_cv_qualified_type(t, CV_NONE);

    type_t* result = dhash_ptr_query(_atomic_types_hash, (const char*)t);

    if (result == NULL)
    {
        result = copy_type_for_variant(t);
        result->info->is_atomic_type = 1;

        dhash_ptr_insert(_atomic_types_hash, (const char*)t, result);
    }

    return get_cv_qualified_type(result, cv_qualif);
}

extern inline char variant_type_is_atomic(type_t* t)
{
    if (t == NULL)
        return 0;

    t = advance_over_typedefs(t);
    return (t->info->is_atomic_type);
}

extern inline char is_atomic_type(type_t* t)
{
    return variant_type_is_atomic(t);
}

typedef
struct class_path_tag
{
    int length;
    type_t** class_type;
    char* is_virtual;
} class_path_t;

static void class_type_is_ambiguous_base_of_class_aux(type_t* derived_class,
        type_t* base_class,
        class_path_t *base_class_path,
        char *base_found,
        char *is_ambiguous)
{
    int i;
    int num_bases = class_type_get_num_bases(derived_class);

    class_path_t class_path[num_bases + 1];
    memset(class_path, 0, sizeof(class_path));

    char found[num_bases + 1];
    memset(found, 0, sizeof(found));

    // Check every base
    for (i = 0; i < num_bases; i++)
    {
        if (base_class_path != NULL)
        {
            // Duplicate paths
            class_path[i].length = base_class_path->length;

            class_path[i].class_type = NEW_VEC(type_t*, base_class_path->length);
            memcpy(class_path[i].class_type, base_class_path->class_type,
                    sizeof(*(class_path[i].class_type)) * base_class_path->length);

            class_path[i].is_virtual = NEW_VEC(char, base_class_path->length);
            memcpy(class_path[i].is_virtual, base_class_path->is_virtual,
                    sizeof(*(class_path[i].is_virtual)) * base_class_path->length);
        }

        char is_virtual = 0;
        char is_dependent = 0;
        char is_expansion = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        scope_entry_t* current_base = class_type_get_base_num(derived_class, i,
                &is_virtual,
                &is_dependent,
                &is_expansion,
                &access_specifier);

        // Should not happen, ignore them
        if (is_dependent || is_expansion)
            continue;

        // We add the current base and its virtual flag in the ith-path
        // P_LIST_ADD modifies its second argument
        int n = class_path[i].length;
        P_LIST_ADD(class_path[i].is_virtual, n, is_virtual);
        P_LIST_ADD(class_path[i].class_type, class_path[i].length, get_actual_class_type(base_class));

        if (equivalent_types(
                    get_actual_class_type(current_base->type_information),
                    get_actual_class_type(base_class)))
        {
            found[i] = 1;
        }
        else
        {
            class_type_is_ambiguous_base_of_class_aux(
                    get_user_defined_type(current_base),
                    base_class,
                    &(class_path[i]),
                    &found[i],
                    is_ambiguous);
        }

        *base_found = *base_found || found[i];
    }

    // Note if already determined to be ambiguous, do nothing
    if (!(*is_ambiguous))
    {
        // Count the number of paths that found the base and remember the last
        // one
        int num_found = 0, last_found = -1;
        for (i = 0; i < num_bases; i++)
        {
            if (found[i])
            {
                last_found = i;
                num_found++;
            }
        }

        // If at least one has been found, check all paths
        if (num_found >= 1)
        {
            char found_to_be_ambiguous = 0;
            for (i = 0; i < num_bases && !found_to_be_ambiguous; i++)
            {
                if (!found[i])
                    continue;

                int j;
                for (j = i + 1; j < num_bases && !found_to_be_ambiguous; j++)
                {
                    if (!found[j])
                        continue;

                    int path_i = class_path[i].length - 1;
                    int path_j = class_path[j].length - 1;

                    char different_subobjects = 1;
                    while (path_i >= 0
                            && path_j >= 0)
                    {
                        if (equivalent_types(
                                    class_path[i].class_type[path_i],
                                    class_path[j].class_type[path_j]))
                        {
                            if (class_path[i].is_virtual[path_i]
                                    && class_path[j].is_virtual[path_j])
                            {
                                different_subobjects = 0;
                            }
                        }

                        path_i--;
                        path_j--;
                    }

                    if (different_subobjects)
                    {
                        found_to_be_ambiguous = 1;
                    }
                }
            }

            if (found_to_be_ambiguous)
            {
                *is_ambiguous = 1;
            }
            else
            {
                if (base_class_path != NULL)
                {
                    // Update the base class path. Any path should be as OK as
                    // any other, keep the last
                    for (i = 0; i < class_path[last_found].length; i++)
                    {
                        int n = base_class_path->length;
                        P_LIST_ADD(base_class_path->is_virtual,
                                n,
                                class_path[last_found].is_virtual[i]);
                        P_LIST_ADD(base_class_path->class_type,
                                base_class_path->length,
                                class_path[last_found].class_type[i]);
                    }
                }
            }
        }
    }

    // Cleanup
    for (i = 0; i < num_bases; i++)
    {
        DELETE(class_path[i].class_type);
        DELETE(class_path[i].is_virtual);
    }
}

extern inline char class_type_is_ambiguous_base_of_derived_class(type_t* base_class, type_t* derived_class)
{
    ERROR_CONDITION(!is_class_type(base_class), "This is not a class type", 0);
    ERROR_CONDITION(!is_class_type(derived_class), "This is not a class type", 0);

    if (equivalent_types(
                get_actual_class_type(base_class),
                get_actual_class_type(derived_class)))
    {
        // This is a degenerate case we do not want to handle further
        return 0;
    }

    char base_found = 0;
    char is_ambiguous = 0;

    class_type_is_ambiguous_base_of_class_aux(derived_class, base_class, NULL, &base_found, &is_ambiguous);

    ERROR_CONDITION(!base_found, "Should not happen", 0);

    return is_ambiguous;
}

static char class_type_is_virtual_base_or_base_of_virtual_base_rec(
        type_t* base_type,
        type_t* derived_type,
        char seen_virtual)
{
    ERROR_CONDITION(!is_class_type(base_type) || !is_class_type(derived_type),
            "This function expects class types", 0);
    // This function assumes base_type is not an ambiguous base of derived_type
    int num_bases = class_type_get_num_bases(derived_type);

    int i;
    // Check every base
    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        char is_expansion = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        scope_entry_t* current_base = class_type_get_base_num(derived_type, i,
                &is_virtual,
                &is_dependent,
                &is_expansion,
                &access_specifier);

        if (equivalent_types(
                    get_actual_class_type(current_base->type_information),
                    get_actual_class_type(base_type)))
        {
            return (seen_virtual || is_virtual);
        }
        else if (class_type_is_virtual_base_or_base_of_virtual_base_rec(
                    base_type,
                    get_user_defined_type(current_base),
                    seen_virtual || is_virtual))
        {
            return 1;
        }
    }

    return 0;
}

extern inline char class_type_is_virtual_base_or_base_of_virtual_base(
        type_t* base_type, type_t* derived_type)
{
    return class_type_is_virtual_base_or_base_of_virtual_base_rec(
            base_type,
            derived_type,
            /* seen_virtual */ 0);
}

extern inline char type_is_reference_related_to(type_t* t1, type_t* t2)
{
    ERROR_CONDITION(is_any_reference_type(t1) || is_any_reference_type(t2),
            "Do not pass reference types to this function", 0);
    return (equivalent_types(get_unqualified_type(t1), get_unqualified_type(t2))
            || (is_class_type(t1)
                && is_class_type(t2)
                && class_type_is_base(t1, t2)));
}

extern inline char type_is_reference_compatible_to(type_t* t1, type_t* t2)
{
    ERROR_CONDITION(is_any_reference_type(t1) || is_any_reference_type(t2),
            "Do not pass reference types to this function", 0);
    return type_is_reference_related_to(t1, t2)
        && is_more_or_equal_cv_qualified_type(t1, t2);
}

static type_t* rewrite_redundant_typedefs(type_t* orig)
{
    if (orig == NULL)
        return NULL;

    if (is_dependent_type(orig))
        return orig;

    type_t* result = orig;

    if (is_named_type(orig))
    {
        scope_entry_t *sym = named_type_get_symbol(orig);
        if (sym->kind == SK_TYPEDEF && sym->decl_context->current_scope != NULL
            && sym->decl_context->current_scope->kind == BLOCK_SCOPE)
        {
            // typedefs declared inside functions are always advanced
            result = rewrite_redundant_typedefs(sym->type_information);
        }
        else if (sym->kind == SK_TYPEDEF
                 && !is_dependent_type(sym->type_information))
        {
            // Avoid advancing typedefs that point to unqualified vector types
            // because gcc does not handle them very well inside template
            // arguments
            type_t *dest = sym->type_information;
            if (!is_named_type(dest) && is_vector_type(dest)
                && !is_named_type(vector_type_get_element_type(dest)))
            {
                // do nothing (result = orig above)
            }
            else
            {
                // typedefs that are not local but are not dependent either
                result = rewrite_redundant_typedefs(dest);
            }
        }
        else if (sym->kind == SK_TEMPLATE_ALIAS
                 && !is_dependent_type(sym->type_information))
        {
            result = rewrite_redundant_typedefs(sym->type_information);
        }
        else
        {
            // Early return to avoid altering in any further way the value of
            // this named type
            return orig;
        }
    }
    else if (is_pointer_type(orig))
    {
        type_t* pointee = pointer_type_get_pointee_type(orig);
        pointee = rewrite_redundant_typedefs(pointee);
        result = get_pointer_type(pointee);
    }
    else if (is_pointer_to_member_type(orig))
    {
        type_t* pointee = pointer_type_get_pointee_type(orig);
        pointee = rewrite_redundant_typedefs(pointee);

        type_t* class_type = pointer_to_member_type_get_class_type(orig);
        class_type = rewrite_redundant_typedefs(class_type);

        result = get_pointer_to_member_type(pointee, class_type);
    }
    else if (is_rebindable_reference_type(orig))
    {
        type_t* ref_type = reference_type_get_referenced_type(orig);
        ref_type = rewrite_redundant_typedefs(ref_type);

        result = get_rebindable_reference_type(ref_type);
    }
    else if (is_lvalue_reference_type(orig))
    {
        type_t* ref_type = reference_type_get_referenced_type(orig);
        ref_type = rewrite_redundant_typedefs(ref_type);

        result = get_lvalue_reference_type(ref_type);
    }
    else if (is_rvalue_reference_type(orig))
    {
        type_t* ref_type = reference_type_get_referenced_type(orig);
        ref_type = rewrite_redundant_typedefs(ref_type);

        result = get_rvalue_reference_type(ref_type);
    }
    else if (is_array_type(orig))
    {
        type_t* element_type = array_type_get_element_type(orig);
        element_type = rewrite_redundant_typedefs(element_type);

        if (array_type_is_string_literal(orig))
        {
            nodecl_t array_size = array_type_get_array_size_expr(orig);
            get_array_type_for_literal_string(element_type, array_size,
                    array_type_get_array_size_expr_context(orig));
        }
        else if ((IS_C_LANGUAGE
                    || IS_CXX_LANGUAGE)
                && !array_type_has_region(orig))
        {
            nodecl_t array_size = array_type_get_array_size_expr(orig);

            result = get_array_type(
                    element_type,
                    array_size,
                    array_type_get_array_size_expr_context(orig));
        }
        else if (IS_FORTRAN_LANGUAGE
                && !array_type_has_region(orig))
        {
            nodecl_t lower_bound = array_type_get_array_lower_bound(orig);
            nodecl_t upper_bound = array_type_get_array_upper_bound(orig);

            bool has_descriptor = array_type_with_descriptor(orig);

            if (!has_descriptor)
            {
                result = get_array_type_bounds(
                        element_type,
                        lower_bound,
                        upper_bound,
                        array_type_get_array_size_expr_context(orig));
            }
            else
            {
                result = get_array_type_bounds_with_descriptor(
                        element_type,
                        lower_bound,
                        upper_bound,
                        array_type_get_array_size_expr_context(orig));
            }
        }
        else if (array_type_has_region(orig))
        {
            nodecl_t lower_bound = array_type_get_array_lower_bound(orig);
            nodecl_t upper_bound = array_type_get_array_upper_bound(orig);

            nodecl_t region_lower_bound = array_type_get_region_lower_bound(orig);
            nodecl_t region_upper_bound = array_type_get_region_upper_bound(orig);
            nodecl_t region_stride = array_type_get_region_stride(orig);

            result = get_array_type_bounds_with_regions(element_type,
                    lower_bound,
                    upper_bound,
                    array_type_get_array_size_expr_context(orig),
                    nodecl_make_range(region_lower_bound, region_upper_bound, region_stride,
                        get_ptrdiff_t_type(), make_locus("", 0, 0)),
                    array_type_get_array_size_expr_context(orig));
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    else if (is_function_type(orig))
    {
        type_t* return_type = function_type_get_return_type(orig);
        return_type = rewrite_redundant_typedefs(return_type);

        if (function_type_get_lacking_prototype(orig))
        {
            result = get_nonproto_function_type(return_type, 
                    function_type_get_num_parameters(orig));
        }
        else
        {
            int i, N = function_type_get_num_parameters(orig), P = N;

            parameter_info_t param_info[N+1];
            memset(param_info, 0, sizeof(param_info));

            if (function_type_get_has_ellipsis(orig))
            {
                param_info[N-1].is_ellipsis = 1;
                param_info[N-1].type_info = get_ellipsis_type();
                param_info[N-1].nonadjusted_type_info = NULL;
                P = N - 1;
            }

            for (i = 0; i < P; i++)
            {
                param_info[i].type_info = rewrite_redundant_typedefs(function_type_get_parameter_type_num(orig, i));
            }

            result = get_new_function_type(return_type, param_info, N, function_type_get_ref_qualifier(orig));
        }
    }
    else if (is_vector_type(orig))
    {
        type_t * element_type = vector_type_get_element_type(orig);
        element_type = rewrite_redundant_typedefs(element_type);

        result = get_vector_type_by_bytes(
                element_type,
                vector_type_get_vector_size_in_bytes(orig));
    }
    else if (is_sequence_of_types(orig))
    {
        int n = sequence_of_types_get_num_types(orig);
        type_t *fixed_types[n + 1];
        int i;
        for (i = 0; i < n; i++)
        {
            fixed_types[i] = rewrite_redundant_typedefs(
                sequence_of_types_get_type_num(orig, i));
        }

        result = get_sequence_of_types(n, fixed_types);
    }

    cv_qualifier_t cv_qualif_orig = CV_NONE;
    cv_qualifier_t cv_qualif_result = CV_NONE;
    advance_over_typedefs_with_cv_qualif(orig, &cv_qualif_orig);
    advance_over_typedefs_with_cv_qualif(result, &cv_qualif_result);

    result = get_cv_qualified_type(result, cv_qualif_orig | cv_qualif_result);

    ERROR_CONDITION(!equivalent_types(result, orig), "The new type (%s) is not equivalent to the original (%s)!",
            print_declarator(result),
            print_declarator(orig));

    return result;
}


