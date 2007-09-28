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
#include "cxx-buildscope-decls.h"
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

struct type_tag;
typedef struct type_tag type_t;

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
    // GCC Extension for builtin types
    SK_GCC_BUILTIN_TYPE, // [16]
    // Dependent entity that is named but nothing is known at the moment
    SK_DEPENDENT_ENTITY // [17]
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

    // Default type for type/template template parameters
    type_t* default_type;

    // Default expression for nontype template parameters
    decl_context_t default_expr_context;
    AST default_expr;

    char has_default_argument;

} template_parameter_t;

// Access specifier, saved but not enforced by the compiler
typedef enum access_specifier_t
{
    AS_UNKNOWN = 0,
    AS_PUBLIC, // public
    AS_PRIVATE, // private
    AS_PROTECTED // protected
} access_specifier_t;

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
    AST expression;
    decl_context_t expression_context;

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

    // Decl context when the symbol was declared it contains the scope where
    // the symbol was declared
    decl_context_t decl_context;

    // Type information of this symbol
    type_t* type_information;

    // Related decl_context. This is the declarative region created
    // by either a class or a namespace
    decl_context_t related_decl_context;

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

    // File and line where this simbol was signed up
    char *file;
    int line;

    // Do not print this symbol (because of recursion, hiding, etc) Used
    // specially for the injected class-name, where printing it in print scope
    // routines would create an infinite recursion.
    char do_not_print;

    // States if this is the injected class name of every class
    char injected_class_name;
    // and its real symbol :?
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

    // States if the symbol is a template parameter name
    // and its nesting and position
    char is_template_parameter;
    int template_parameter_nesting;
    int template_parameter_position;

    // States if the variable is a parameter (kind == SK_VARIABLE)
    // and its position
    char is_parameter;
    int parameter_position;

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
    Hash* hash;

    // Qualification name of this scope this holds the name we have to prepend
    // to an entity of this scope in order to qualify it in the enclosing
    // scope.
    char* qualification_name;

    // Relationships with other scopes
    // Nesting relationship is expressed by "contained_in". This relationship is
    // valid in all kinds of scopes except for FUNCTION_SCOPE (where there is
    // not any nesting)
    struct scope_tag* contained_in; 

    // using namespace statements (using directives) will fill this
    // Only valid for BLOCK_SCOPE, CLASS_SCOPE and NAMESPACE_SCOPE
    int num_used_namespaces;
    struct scope_tag** use_namespace;

    // Base scopes
    // Only valid in CLASS_SCOPE
    int num_base_scopes;
    struct scope_tag** base_scope;
} scope_t;

/*
 * Used for function overloading and Koenig lookup
 */
typedef
struct argument_type_info_tag
{
    type_t* type;
    char is_lvalue;
} argument_type_info_t;

MCXX_END_DECLS

#endif // CXX_SCOPE_DECLS_H
