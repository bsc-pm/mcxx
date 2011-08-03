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

#include "cxx-scope-fwd.h"

#include "red_black_tree.h"
#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-ast-decls.h"
#include "cxx-gccsupport-decls.h"
#include "cxx-typeenviron-decls.h"
#include "cxx-entrylist-decls.h"
#include "cxx-type-decls.h"
#include "cxx-limits.h"
#include "cxx-nodecl-output.h"

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

// FIXME:
// Remove all but
// DF_NONE
// DF_CONSTRUCTOR
// DF_DEPENDENT_TYPENAME
// DF_ELABORATED_NAME
// DF_LABEL
// DF_NO_INJECTED_CLASS_NAME
// DF_ONLY_CURRENT_SCOPE

typedef 
enum decl_flags_tag
{
    DF_NONE = 0,
    // It states that the current declaration is a constructor, so no
    // type-specifier is expected and a special name 'constructor class-name'
    // is used when looking up in the scope
    DF_CONSTRUCTOR = BITMAP(1),
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
    // States that the lookup should ignore injected class-names
    DF_NO_INJECTED_CLASS_NAME = BITMAP(11),
    // Relaxed typechecking, ambiguity decl-expr is solved always to expr if it
    // cannot be disambiguated
    DF_AMBIGUITY_FALLBACK_TO_EXPR = BITMAP(12),
} decl_flags_t;

#undef BITMAP

// Inherited attributes
struct decl_context_tag
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

    // Template parameter of the current context
    struct template_parameter_list_tag *template_parameters;

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
};

#define SYMBOL_KIND_TABLE \
    SYMBOL_KIND(SK_CLASS, "class-name") \
    SYMBOL_KIND(SK_ENUM, "enum-name") \
    SYMBOL_KIND(SK_ENUMERATOR, "enumerator-name") \
    SYMBOL_KIND(SK_FUNCTION, "function-name") \
    SYMBOL_KIND(SK_LABEL, "label-name") \
    SYMBOL_KIND(SK_NAMESPACE, "namespace-name") \
    SYMBOL_KIND(SK_VARIABLE, "data object name") \
    SYMBOL_KIND(SK_TYPEDEF, "typedef-name") \
    SYMBOL_KIND(SK_TEMPLATE, "template-name") \
    SYMBOL_KIND(SK_TEMPLATE_PARAMETER, "nontype template parameter name") \
    SYMBOL_KIND(SK_TEMPLATE_TYPE_PARAMETER, "type template parameter name") \
    SYMBOL_KIND(SK_TEMPLATE_TEMPLATE_PARAMETER, "template template parameter") \
    SYMBOL_KIND(SK_GCC_BUILTIN_TYPE, "__builtin_va_list") \
    SYMBOL_KIND(SK_DEPENDENT_ENTITY, "template dependent name") \
    SYMBOL_KIND(SK_USING, "using declared name") \
    SYMBOL_KIND(SK_SCOPE, "<<scoping symbol>>")  \
    SYMBOL_KIND(SK_OTHER, "<<internal symbol>>") 

#ifdef FORTRAN_SUPPORT
#define SYMBOL_KIND_TABLE_FORTRAN \
    SYMBOL_KIND(SK_COMMON, "COMMON name") \
    SYMBOL_KIND(SK_NAMELIST, "NAMELIST name") \
    SYMBOL_KIND(SK_MODULE, "MODULE name") \
    SYMBOL_KIND(SK_PROGRAM, "PROGRAM name") \
    SYMBOL_KIND(SK_BLOCKDATA, "BLOCK DATA name") 
#endif

enum cxx_symbol_kind
{
    SK_UNDEFINED = 0,
#define SYMBOL_KIND(x, _) \
    x, 

    SYMBOL_KIND_TABLE

#ifdef FORTRAN_SUPPORT
    SYMBOL_KIND_TABLE_FORTRAN
#endif

#undef SYMBOL_KIND
    SK_LAST_KIND
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

struct template_parameter_value_tag
{
    // This eases some checks
    enum template_parameter_kind kind;

    // All template parameters can have this value
    // - Type and template will have the argument
    // - Nontype will have the related type of the template parameter
    struct type_tag* type;

    // Argument tree. Used only for nontype template parameters
    nodecl_t value;

    // Template, states that this is a default argument of a template parameter
    char is_default;

    // This symbol is null until lookup finds a template parameter and
    // discovers it has this value. Then a fake symbol is created to represent
    // such value and is kept here
    scope_entry_t* entry;
};
// A template parameter
//
// template <class T, int N> <-- these are parameters
struct template_parameter_tag
{
    // Kind of the parameter
    enum template_parameter_kind kind;
    scope_entry_t* entry;
};

struct template_parameter_list_tag
{
    int num_parameters;
    template_parameter_t** parameters;
    template_parameter_value_t** arguments;
    struct template_parameter_list_tag* enclosing;
};

// Access specifier, saved but not enforced by the compiler
typedef enum access_specifier_t
{
    AS_UNKNOWN = 0,
    AS_PUBLIC, // public
    AS_PRIVATE, // private
    AS_PROTECTED // protected
} access_specifier_t;


struct default_argument_info_tag
{
    nodecl_t argument;
    decl_context_t context;
};

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

enum codegen_status_tag
{
    CODEGEN_STATUS_NONE = 0,
    CODEGEN_STATUS_DECLARED = 1,
    CODEGEN_STATUS_DEFINED = 2
};
typedef enum codegen_status_tag codegen_status_t;

typedef nodecl_t (*simplify_function_t)(int num_arguments, nodecl_t *arguments);

typedef void (*emission_handler_t)(scope_entry_t*, const char* filename, int line);

// Looking for struct entity_specifiers_tag?
// Now it is declared in cxx-entity-specs.h in builddir
#include "cxx-entity-specs.h"

// This is an entry in the scope
struct scope_entry_tag
{
    // Kind of this symbol
    enum cxx_symbol_kind kind;
    
    // Decl context when the symbol was declared it contains the scope where
    // the symbol was registered
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
    struct AST_tag* language_dependent_value;
    nodecl_t value;

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

    // Extensible information of a symbol
    extensible_struct_t* extended_data;
}; 

// Scope kind
enum scope_kind
{
    UNDEFINED_SCOPE = 0, // Undefined scope, to early catch errors
    NAMESPACE_SCOPE, // Scope of a namespace (including the global one)
    FUNCTION_SCOPE, // Label declarations and gotos 
    PROTOTYPE_SCOPE, // Scope of a prototype
    BLOCK_SCOPE, // Corresponds to the scope of a compound statement
    CLASS_SCOPE, // Class scope
};

// This is the scope
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
    scope_entry_t** use_namespace;

    // Only valid for NAMESPACE_SCOPE, CLASS_SCOPE and BLOCK_SCOPE
    // they contain the namespace symbol, the class symbol
    // and the function symbol
    scope_entry_t* related_entry;
};

MCXX_END_DECLS

#endif // CXX_SCOPE_DECLS_H
