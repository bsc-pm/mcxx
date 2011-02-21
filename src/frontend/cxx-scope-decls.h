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



#ifndef CXX_SCOPE_DECLS_H
#define CXX_SCOPE_DECLS_H

#include "red_black_tree.h"
#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-ast-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-gccsupport-decls.h"
#include "cxx-typeenviron-decls.h"
#include "cxx-entrylist-decls.h"

#ifdef FORTRAN_SUPPORT
#include "fortran/fortran03-scope-decls.h"
#endif 

// Extensible schema
#include "extstruct.h"

MCXX_BEGIN_DECLS

/*
 * A scope is represented by a struct scope_tag*
 *
 * Entries in the scope are struct scope_entry_tag*
 *
 * Every entry can have a non-null type_information field struct type_tag*
 * 
 * A struct type_tag* represents a full C++ type in a hierarchical way.
 *   -> pointer
 *   -> array
 *   -> function
 *   -> type (direct type including builtin's, class, enums, typedef)
 */


#define BITMAP(x) (1 << (x))

typedef 
enum decl_flags_tag
{
    DF_NONE = 0,
    // If this is enable, declarations are under the hood of a template.  It is
    // cleared for members of a class, which in turn can be templated too
    DF_TEMPLATE = BITMAP(0), 
    // It states that the current declaration is a constructor, so no
    // type-specifier is expected and a special name 'constructor class-name'
    // is used when looking up in the scope
    DF_CONSTRUCTOR = BITMAP(1),
    // It states that the current declaration does not have any declaration.
    // It is mainly used for elaborate-type-specifiers and templated-declarations
    DF_NO_DECLARATORS = BITMAP(2),
    // It states that the declaration has a 'friend' specifier
    DF_FRIEND = BITMAP(3),
    // It states that, under the same scope as DF_TEMPLATE, the declaration
    // is under a 'template<>'
    DF_EXPLICIT_SPECIALIZATION = BITMAP(4),
    // Allows redefinition of an identifier already defined, used in compiler
    // phases since they might need to redeclare something
    DF_ALLOW_REDEFINITION = BITMAP(5),
    // We are looking up a label
    DF_LABEL = BITMAP(6), 
    // Lookup will consider only the current scope
    DF_ONLY_CURRENT_SCOPE = BITMAP(7),
    // Disables examining dependent types (used for dependent typenames)
    DF_DEPENDENT_TYPENAME = BITMAP(8),
    // Enables weird lookup for 'struct X'/'union X'/'enum X'
    DF_ELABORATED_NAME = BITMAP(9),
    // States that we are under parameter declaration
    DF_PARAMETER_DECLARATION = BITMAP(10),
    // States that the lookup should ignore injected class-names
    DF_NO_INJECTED_CLASS_NAME = BITMAP(11),
    // Updates template arguments for a given specialization, used
    // only when defining an already declared template specialization
    // (since we want the names to be updated)
    DF_UPDATE_TEMPLATE_ARGUMENTS = BITMAP(12),
    // Relaxed typechecking, ambiguity decl-expr is solved always to expr if it
    // cannot be disambiguated
    DF_AMBIGUITY_FALLBACK_TO_EXPR = BITMAP(13),
    // Explicit instantiation
    DF_EXPLICIT_INSTANTIATION = BITMAP(14)
} decl_flags_t;

#undef BITMAP

// Inherited attributes
typedef struct decl_context_tag
{
    // Several declaration flags
    // telling us some context
    decl_flags_t decl_flags;
    
    // First enclosing available namespace scope
    struct scope_tag* namespace_scope;
    // Global scope, should not change along a translation unit
    struct scope_tag* global_scope;

    // Current block scope, if any
    struct scope_tag* block_scope;

    // Current template_scope, if any
    struct scope_tag* template_scope;
    // Template parameter information without taking
    // into account the current scope
    struct template_parameter_list_tag *template_parameters;
    // Template nesting level
    int template_nesting;

    // Current class scope, if any
    struct scope_tag* class_scope;

    // For labels, if any
    struct scope_tag* function_scope;

    // Prototype scope, if any
    struct scope_tag* prototype_scope;

#ifdef FORTRAN_SUPPORT
    implicit_info_t* implicit_info;
#endif 

    // Scope of the declaration,
    // should never be null
    struct scope_tag* current_scope;
} decl_context_t;

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
    SK_TEMPLATE, // [9] this names a template (either function or class)
    SK_TEMPLATE_PARAMETER, // [10] nontype parameters like N in "template<int N>"
    SK_TEMPLATE_TYPE_PARAMETER, // [11] plain type parameters like T in "template <class T>"
    SK_TEMPLATE_TEMPLATE_PARAMETER, // [12] template template parameters like Q in "template<template<typename P> class Q>"
    // GCC Extension for builtin types
    SK_GCC_BUILTIN_TYPE, // [13]
    // Dependent entity that is named but nothing is known at the moment
    SK_DEPENDENT_ENTITY, // [14]
    // Other symbols whose use is defined elsewhere
    // These symbols should not be language-accessible
    SK_OTHER, // [15]
#ifdef FORTRAN_SUPPORT
    SK_COMMON, // [16]
    SK_NAMELIST, // [17]
    SK_MODULE, // [18]
    SK_PROGRAM, // [19]
    SK_BLOCKDATA, // [20]
#endif
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
    struct AST_tag* expression;
    decl_context_t expression_context;

    // If the template argument is a type template argument (or a template
    // template one) the type should be here
    struct type_tag* type;

    // This argument was implicitly defined by default template argument
    char implicit;

    // "Identifier" of this template argument
    int position;
    int nesting;
} template_argument_t;

// List of template arguments
typedef 
struct template_argument_list_tag {
    int num_arguments;
    template_argument_t** argument_list;
} template_argument_list_t;

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
typedef struct template_parameter_tag
{
    // Kind of the parameter
    enum template_parameter_kind kind;

    // The related symbol associated to this parameter it may be faked if the
    // symbol did not have name, we are mainly interested in their type
    struct scope_entry_tag* entry;

    char has_default_argument;
    template_argument_t* default_template_argument;
} template_parameter_t;

typedef struct template_parameter_list_tag
{
    int num_template_parameters;
    template_parameter_t** template_parameters;
} template_parameter_list_t;

// Access specifier, saved but not enforced by the compiler
typedef enum access_specifier_t
{
    AS_UNKNOWN = 0,
    AS_PUBLIC, // public
    AS_PRIVATE, // private
    AS_PROTECTED // protected
} access_specifier_t;


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

struct scope_entry_tag;


typedef
struct default_argument_info_tag
{
    struct AST_tag* argument;
    decl_context_t context;
} default_argument_info_t;

LIBMCXX_EXTERN extensible_schema_t scope_entry_extensible_schema;

#ifdef FORTRAN_SUPPORT
typedef
enum intent_kind_tag
{
    INTENT_INVALID = 0,
    INTENT_IN = 1,
    INTENT_OUT = 2,
    INTENT_INOUT = INTENT_IN | INTENT_OUT,
} intent_kind_t;

#endif

typedef struct entity_specifiers_tag
{
    // States if this a static variable
    char is_static:1;

    // Register variable
    char is_register:1;

    // States if it is an extern declaration (explicitly given)
    char is_extern:1;

    // States if it is a mutable entity of a class
    char is_mutable:1;

    // States is a exported template (unused at all)
    char is_export:1;

    // Inlined function
    char is_inline:1;

    // Virtual function
    char is_virtual:1;

    // Pure function
    char is_pure:1;

    // Visibility attributes
    char is_public:1;
    char is_private:1;
    char is_protected:1;

    // Builtin symbol
    char is_builtin:1;

    // Is deleted
    char is_deleted:1;

    // Is defaulted
    char is_defaulted:1;

    // Is a conversion function
    char is_conversion:1;

    // Is trivial special member
    // Qualifies several special members trivializing it
    char is_trivial:1;

    // Is a constructor
    char is_constructor:1;
    char is_default_constructor:1;
    // Is a copy constructor
    char is_copy_constructor:1;
    // Is a move constructor
    char is_move_constructor:1;
    // Is a conversor one
    char is_conversor_constructor:1;

    // Is a copy assignment operator
    char is_copy_assignment_operator:1;

    // Is a copy assignment operator
    char is_move_assignment_operator:1;

    // Is destructor
    char is_destructor:1;

    // Is an explicit constructor
    char is_explicit:1;

    // Is a surrogate fake symbol
    char is_surrogate_function:1;

    // Is an anonymous entity
    char is_anonymous:1;

    // Template argument, we need this to properly evaluate nontype template
    // arguments
    char is_template_argument:1;

    // The entity has really been declared by the code. Only template-names
    // and constructors have this flag enabled (at the moment)
    char is_user_declared:1;

    // Some bits have been moved here, they are repeated in comments below next
    // to their protecting fields
    char is_template_parameter:1;
    char is_parameter:1;
    char is_member:1;
    char is_bitfield:1;
    char is_unnamed_bitfield:1;
    char any_exception:1;
    char is_injected_class_name:1;
    char is_nested_unnamed_struct:1;

    // This symbol has been created because of a typedef
    // of an unnamed struct/class/enum/union type
    //
    // typedef struct { } A;
    //
    // A will be signed in as 'SK_CLASS'
    //
    // (
    // like if in C++ we had done
    //
    // struct A { };
    // )
    //
    // And sometimes we need to distinguish whether is
    // 'struct A { }' or 'typedef struct { } A';
    char after_typedef:1;

#ifdef FORTRAN_SUPPORT
    char is_implicit_basic_type:1;

    // Is allocatable
    char is_allocatable:1;

    // In common
    char is_in_common:1;

    // In namelist
    char is_in_namelist:1;

    // Is optional dummy
    char is_optional:1;

    // Is target
    char is_target:1;

    // Is value dummy
    char is_value:1;

    // Is an elemental function
    char is_elemental:1;

    // Is a recursive function
    char is_recursive:1;

    // Is the result of a function
    char is_result:1;

    // Is a statement function
    char is_stmt_function:1;

    // Is a dummy of a statement function
    char is_dummy_arg_stmt_function:1;

    // Is generic spec
    char is_generic_spec:1;
#endif

    // -- End of bits, move all bits before this point

#ifdef FORTRAN_SUPPORT
    intent_kind_t intent_kind;

    // --> char is_in_common:1;
    struct scope_entry_tag* in_common;

    // --> char is_in_namelist:1
    struct scope_entry_tag* namelist;

    // Related symbols for this symbol,
    // NAMELIST, COMMON, FUNCTION, SUBROUTINE, MODULE
    int num_related_symbols;
    struct scope_entry_tag** related_symbols;

    struct scope_entry_tag* (*builtin_check)(decl_context_t, char is_actual_argument, AST ref, AST argument_list);
#endif

    // Accessibility: public, private, protected
    access_specifier_t access : 2;
    
    // States if the symbol is a template parameter name and its nesting and
    // position
    // --> char is_template_parameter:1;
    int template_parameter_nesting;
    int template_parameter_position;

    // States if the variable is parameter of a function (kind == SK_VARIABLE)
    // and its position
    // --> char is_parameter:1;
    int parameter_position;
    
    // Is a member entity (function or data)
    // --> char is_member:1;
    // and its class 
    struct type_tag* class_type;

    // States if this is the injected class name of every class
    // --> char is_injected_class_name:1;
    // and its real symbol class
    struct scope_entry_tag* injected_class_referred_symbol;
    
    // Linkage specifier ("C" or "C++")
    // Unused field
    const char* linkage_spec;
    
    // Exception specifier for functions
    // --> char any_exception:1; // States that any exception can be thrown
    int num_exceptions;
    struct type_tag** exceptions;

    // Default arguments for functions
    int num_parameters;
    default_argument_info_t **default_argument_info;

    // Bitfields
    // char is_bitfield:1;
    // char is_unnamed_bitfield:1;
    struct AST_tag* bitfield_expr;
    decl_context_t bitfield_expr_context;

    // Only for fields that are not bitfields, their offsetof value
    _size_t field_offset;

    // GCC attributes synthesized for this symbol coming from the syntax
    int num_gcc_attributes;
    gather_gcc_attribute_t gcc_attributes[MAX_GCC_ATTRIBUTES_PER_SYMBOL];

    // For functions and classes
    AST definition_tree;
} entity_specifiers_t;

// This is an entry in the scope
typedef 
struct scope_entry_tag
{
    // Kind of this symbol
    enum cxx_symbol_kind kind;
    
    // Decl context when the symbol was declared it contains the scope where
    // the symbol was declared
    decl_context_t decl_context;

    // The symbol name
    const char* symbol_name;

    // This allows us to enforce the one-definition-rule within a translation unit
    int defined;

    // Type information of this symbol
    struct type_tag* type_information;

    // Related decl_context of this symbol. Namespaces in C++ and all program
    // units in Fortran use this field
    decl_context_t related_decl_context;
    
    // Initializations of several kind are saved here
    //  - initialization of const objects
    //  - enumerator values
    struct AST_tag* expression_value;

    // File and line where this simbol was signed up
    const char *file;
    int line;

    // Do not print this symbol (because of recursion, hiding, etc) Used
    // specially for the injected class-name, where printing it in print scope
    // routines would create an infinite recursion.
    char do_not_print;

    // All entity specifiers are in this structure
    entity_specifiers_t entity_specs;

    // Point in the AST where this symbol was declared. This is approximate, just to
    // find the simple_declaration, member_declaration or function_definition
    // holding this one
    struct AST_tag* point_of_declaration;

    // Point in the AST where this symbol was defined. This is approximate,
    // just to find the simple_declaration, member_declaration or
    // function_definition holding this one. Even if defined is true, it might
    // be NULL since builtins do not have any related AST
    struct AST_tag* point_of_definition;

    // Dependency info. It states if this symbol has a template-dependent nature
    // A value of DI_UNKNOWN means this has not been already computed
    //
    // At the moment, this is used only for variables and enumerators.  It is
    // intended to avoid an infinite recursion when computing whether an enum
    // or enumerator is dependent.  An enum will check every of its
    // enumerators, and an enumerator will check its enum type
    dependency_info_t dependency_info;

    // Extensible information of a symbol
    extensible_struct_t* extended_data;
} scope_entry_t;

// Scope kind
enum scope_kind
{
    UNDEFINED_SCOPE = 0, // Undefined scope, to early catch errors
    NAMESPACE_SCOPE, // Scope of a namespace (including the global one)
    FUNCTION_SCOPE, // Label declarations and gotos 
    PROTOTYPE_SCOPE, // Scope of a prototype
    BLOCK_SCOPE, // Corresponds to the scope of a compound statement
    CLASS_SCOPE, // Class scope
    TEMPLATE_SCOPE // Template scope, where template parameters live
};

// This is the scope
typedef 
struct scope_tag
{
    // Kind of this scope
    enum scope_kind kind;

    // Hash of scope_entry_list
    rb_red_blk_tree *hash;

    // Relationships with other scopes
    // Nesting relationship is expressed by "contained_in". This relationship is
    // valid in all kinds of scopes except for FUNCTION_SCOPE (where there is
    // not any nesting)
    struct scope_tag* contained_in; 

    // using namespace statements (using directives) will fill this
    // Only valid for BLOCK_SCOPE, CLASS_SCOPE and NAMESPACE_SCOPE
    int num_used_namespaces;
    struct scope_entry_tag** use_namespace;

    // Only valid for NAMESPACE_SCOPE, CLASS_SCOPE and BLOCK_SCOPE
    // they contain the namespace symbol (if any), the class symbol
    // and the function symbol (this last is unused)
    scope_entry_t* related_entry;
} scope_t;

MCXX_END_DECLS

#endif // CXX_SCOPE_DECLS_H
