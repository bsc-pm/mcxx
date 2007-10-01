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
#ifndef CXX_BUILDSCOPE_DECLS_H
#define CXX_BUILDSCOPE_DECLS_H

// These are the unique types needed from the cxx-scope-decls.h
// but we do not need them completely defined here since we only hold pointers
struct template_parameter_tag;
struct scope_tag;

typedef 
struct gather_decl_spec_tag {
    char is_auto;
    char is_register;
    char is_static;
    char is_extern;
    char is_mutable;
    char is_thread;
    char is_friend;
    char is_typedef;
    char is_signed;
    char is_unsigned;
    char is_short;
    char is_long;
    char is_const;
    char is_volatile;
    char is_restrict;
    char is_inline;
    char is_virtual;
    char is_explicit;
    char is_complex;
} gather_decl_spec_t;

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
    // Disables failures when looking up some objects, this is used in dependent
    // contexts when doing speculative lookups that might fail
    DF_NO_FAIL = BITMAP(5),
    // Allows redefinition of an identifier already defined, used in compiler
    // phases since they might need to redeclare something
    DF_ALLOW_REDEFINITION = BITMAP(6),
    // States that a specialization must be created for the template type
    DF_ALWAYS_CREATE_SPECIALIZATION = BITMAP(7),
    // Lookup is being performed on a unqualified name
    DF_UNQUALIFIED_NAME = BITMAP(8), 
    // Lookup is being performed on a qualified name
    DF_QUALIFIED_NAME = BITMAP(9), 
    // We are looking up a label
    DF_LABEL = BITMAP(10), 
    // Lookup will consider only the current scope
    DF_ONLY_CURRENT_SCOPE = BITMAP(11),
    // The lookup is being performed to query an id-expression
    DF_EXPRESSION = BITMAP(12), 
    // Disables instantiation of symbols. Used in dependent contexts
    DF_NO_INSTANTIATE = BITMAP(13),
    // Enables weird lookup for 'struct X'/'union X'/'enum X'
    DF_ELABORATED_NAME = BITMAP(14),
    // States that we are under parameter declaration
    DF_PARAMETER_DECLARATION = BITMAP(15)
} decl_flags_t;

#undef BITMAP


// Inherited attributes
typedef 
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

    // Current template_scope, if any
    struct scope_tag* template_scope;
    // Template parameter information without taking
    // into account the current scope
    struct template_parameter_tag** template_parameters;
    int num_template_parameters;
    // Template nesting level
    int template_nesting;

    // Current class scope, if any
    struct scope_tag* class_scope;

    // For labels, if any
    struct scope_tag* function_scope;

    // Prototype scope, if any
    struct scope_tag* prototype_scope;

    // Scope of the declaration,
    // should never be null
    struct scope_tag* current_scope;
    
} decl_context_t;

#endif // CXX_BUILDSCOPE_DECLS_H
