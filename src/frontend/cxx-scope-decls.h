/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#include <stdbool.h>
#include "cxx-scope-fwd.h"

#include "cxx-locus.h"

#include "red_black_tree.h"
#include "dhash_ptr.h"
#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-ast-decls.h"
#include "cxx-locus.h"
#include "cxx-instantiation-decls.h"
#include "cxx-gccsupport-decls.h"
#include "cxx-typeenviron-decls.h"
#include "cxx-entrylist-decls.h"
#include "cxx-type-decls.h"
#include "cxx-limits.h"
#include "cxx-nodecl-decls.h"

#include "fortran/fortran03-scope-decls.h"

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
// DF_ONLY_CURRENT_SCOPE

typedef 
enum decl_flags_tag
{
    DF_NONE = 0,
    // It states that the current declaration is a constructor, so no
    // type-specifier is expected and a special name 'constructor class-name'
    // is used when looking up in the scope
    DF_CONSTRUCTOR = BITMAP(1),
    // Enables special lookup for 'struct X'
    DF_STRUCT = BITMAP(2),
    // Enables special lookup for 'class X'
    DF_CLASS = BITMAP(3),
    // Enables special lookup for 'union X'
    DF_UNION = BITMAP(4),
    // Enables special lookup for 'enum X'
    DF_ENUM = BITMAP(5),
    // Allows redefinition of an identifier already defined, used in compiler
    // phases since they might need to redeclare something
    DF_ALLOW_REDEFINITION = BITMAP(6),
    // We are looking up a label
    DF_LABEL = BITMAP(7), 
    // Lookup will consider only the current scope
    DF_ONLY_CURRENT_SCOPE = BITMAP(8),
    // Disables examining dependent types (used for dependent typenames)
    DF_DEPENDENT_TYPENAME = BITMAP(9),
    // The queries will ignore the friend declarations
    DF_IGNORE_FRIEND_DECL = BITMAP(10),
    // The queries will not create dependent entities for unqualified names
    DF_DO_NOT_CREATE_UNQUALIFIED_DEPENDENT_ENTITY = BITMAP(11),
    // The query is the first unqualified-id of the nested-name-specifier
    DF_NESTED_NAME_FIRST = BITMAP(12),
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

    // Fortran IMPLICIT info
    implicit_info_t* implicit_info;

    // Scope of the declaration,
    // should never be null
    struct scope_tag* current_scope;
};

#define SYMBOL_KIND_TABLE \
    SYMBOL_KIND(SK_CLASS, "class name") \
    SYMBOL_KIND(SK_ENUM, "enum name") \
    SYMBOL_KIND(SK_ENUMERATOR, "enumerator name") \
    SYMBOL_KIND(SK_FUNCTION, "function name") \
    SYMBOL_KIND(SK_FRIEND_CLASS, "friend class") \
    SYMBOL_KIND(SK_FRIEND_FUNCTION, "friend function") \
    SYMBOL_KIND(SK_LABEL, "label name") \
    SYMBOL_KIND(SK_NAMESPACE, "namespace name") \
    SYMBOL_KIND(SK_VARIABLE, "object name") \
    SYMBOL_KIND(SK_VARIABLE_PACK, "object name pack") \
    SYMBOL_KIND(SK_TYPEDEF, "typedef name") \
    SYMBOL_KIND(SK_TYPEDEF_PACK, "typedef name pack") \
    SYMBOL_KIND(SK_TEMPLATE, "template name") \
    SYMBOL_KIND(SK_TEMPLATE_PACK, "template name pack") \
    SYMBOL_KIND(SK_TEMPLATE_ALIAS, "alias template") \
    SYMBOL_KIND(SK_TEMPLATE_NONTYPE_PARAMETER, "nontype template parameter name") \
    SYMBOL_KIND(SK_TEMPLATE_TYPE_PARAMETER, "type template parameter name") \
    SYMBOL_KIND(SK_TEMPLATE_TEMPLATE_PARAMETER, "template template parameter name") \
    SYMBOL_KIND(SK_TEMPLATE_NONTYPE_PARAMETER_PACK, "nontype template parameter pack name") \
    SYMBOL_KIND(SK_TEMPLATE_TYPE_PARAMETER_PACK, "type template parameter pack name") \
    SYMBOL_KIND(SK_TEMPLATE_TEMPLATE_PARAMETER_PACK, "template template parameter pack name") \
    SYMBOL_KIND(SK_GCC_BUILTIN_TYPE, "__builtin_va_list") \
    SYMBOL_KIND(SK_DECLTYPE, "decltype") \
    SYMBOL_KIND(SK_DEPENDENT_ENTITY, "template dependent name") \
    SYMBOL_KIND(SK_DEPENDENT_FRIEND_CLASS, "dependent friend class") \
    SYMBOL_KIND(SK_DEPENDENT_FRIEND_FUNCTION, "dependent friend function") \
    SYMBOL_KIND(SK_MEMBER_STATIC_ASSERT, "member static_assert") \
    SYMBOL_KIND(SK_NULLPTR, "nullptr") \
    SYMBOL_KIND(SK_USING, "using declared name") \
    SYMBOL_KIND(SK_USING_TYPENAME, "using typename declared name") \
    SYMBOL_KIND(SK_LAMBDA, "lambda-expression") \
    SYMBOL_KIND(SK_OTHER, "<<internal symbol>>")

#define SYMBOL_KIND_TABLE_FORTRAN \
    SYMBOL_KIND(SK_COMMON, "COMMON name") \
    SYMBOL_KIND(SK_NAMELIST, "NAMELIST name") \
    SYMBOL_KIND(SK_MODULE, "MODULE name") \
    SYMBOL_KIND(SK_PROGRAM, "PROGRAM name") \
    SYMBOL_KIND(SK_BLOCKDATA, "BLOCK DATA name") \
    SYMBOL_KIND(SK_GENERIC_NAME, "generic name specifier")

enum cxx_symbol_kind
{
    SK_UNDEFINED = 0,
#define SYMBOL_KIND(x, _) \
    x, 

    SYMBOL_KIND_TABLE
    SYMBOL_KIND_TABLE_FORTRAN

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
    TPK_TEMPLATE, // template <template <typename Q> class V > <-- 'V'
    // Pack equivalents (template arguments will never have this kind)
    TPK_NONTYPE_PACK, // template <int ...N> <-- 'N'
    TPK_TYPE_PACK, // template <class ...T> <-- 'T'
    TPK_TEMPLATE_PACK, // template <template <typename Q> class ...V > <-- 'V'
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
    // This tree is owned by this structure
    nodecl_t value;

    // States that this is a default argument of a template parameter
    char is_default:1;
    // States that this argument was not present in the original template argument
    // but was added when completing them
    char is_implicit:1;

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
    char is_explicit_specialization:1;
    char is_explicit_instantiation:1;
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
    const decl_context_t* context;
    char is_hidden;
};

// This acts as a map <function> -> information of the parameter
struct function_parameter_info_tag
{
    scope_entry_t* function;

    // Nesting in a nested function declarator
    // (Usually only relevant in C++)
    int nesting;
    // Position of the parameter
    int position;
};

typedef
enum intent_kind_tag
{
    INTENT_INVALID = 0,
    INTENT_IN = 1,
    INTENT_OUT = 2,
    INTENT_INOUT = INTENT_IN | INTENT_OUT,
} intent_kind_t;

typedef nodecl_t (*simplify_function_t)(scope_entry_t* entry, int num_arguments, nodecl_t *arguments);

typedef void (*emission_handler_t)(scope_entry_t*, const locus_t* locus);

typedef struct fortran_modules_data_set_tag fortran_modules_data_set_t;
typedef fortran_modules_data_set_t *pfortran_modules_data_set_t;

// Looking for struct entity_specifiers_tag?
// Now it is declared in cxx-entity-specs.h in builddir
#include "cxx-entity-specs.h"

// This is an entry in the scope
struct scope_entry_tag
{
    // Kind of this symbol
    enum cxx_symbol_kind kind:8;
    // This allows us to enforce the one-definition-rule within a translation unit
    bool defined:1;
    // Do not print this symbol (because of recursion, hiding, etc) Used
    // specially for the injected class-name, where printing it in print scope
    // routines would create an infinite recursion.
    bool do_not_print:1;

    // Decl context when the symbol was declared it contains the scope where
    // the symbol was registered
    const decl_context_t* decl_context;

    // The symbol name
    const char* symbol_name;

    // Type information of this symbol
    struct type_tag* type_information;
    // private: used by cxx-typeutils.c
    struct type_tag* _indirect_type[2];

    // Related decl_context of this symbol. Namespaces in C++ and all program
    // units in Fortran use this field
    const decl_context_t* related_decl_context;

    // Initializations of several kind are saved here
    //  - initialization of const objects
    //  - enumerator values
    nodecl_t value;

    // Locus where the symbol was registered
    const locus_t* locus;

    // All entity specifiers are in this structure
    union {
        // Field for transition to a sealed scope_entry_t
        DEPRECATED_REASON("use the getters/setters of cxx-entity-specs-ops.h") entity_specifiers_t entity_specs;

        // If you use this field you will be fired.
        // This is only for functions in cxx-entity-specifiers-ops.h
        entity_specifiers_t _entity_specs;
    };
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
    // rb_red_blk_tree *hash;
    dhash_ptr_t* dhash;

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

typedef const char* (*print_symbol_callback_t)(scope_entry_t*, const decl_context_t*, void*);

enum { MCXX_MAX_FIELD_PATH = 1 };

typedef
struct field_path_tag
{
    int length;
    scope_entry_t* path[MCXX_MAX_FIELD_PATH];
} field_path_t;

MCXX_END_DECLS

#endif // CXX_SCOPE_DECLS_H
