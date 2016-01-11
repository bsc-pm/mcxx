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




#ifndef CXX_BUILDSCOPE_DECLS_H
#define CXX_BUILDSCOPE_DECLS_H

#include <stdbool.h>

#include "cxx-macros.h"
#include "cxx-scope-decls.h"
#include "cxx-gccsupport-decls.h"
#include "cxx-limits.h"

MCXX_BEGIN_DECLS

// This structure gather things of a declaration in one place so we can use
// along a whole declaration. Parts of a declaration belong just to type while
// others belong to the symbol but they do not appear syntactically in the same
// place

typedef
struct arguments_info_tag
{
    scope_entry_t* entry;
    nodecl_t argument;
    const decl_context_t* context;
} arguments_info_t;

typedef 
struct gather_decl_spec_tag {
    // context of the declaration
    bool no_declarators:1;
    bool parameter_declaration:1;
    bool is_template:1;
    bool is_explicit_specialization:1; // template<> void A<int>::f();
    bool is_explicit_instantiation:1;  // template   void A<int>::f();

    bool inside_class_specifier:1;

    // type-specifiers and decl-specifiers
    bool is_auto_storage:1;
    bool is_auto_type:1;
    bool is_decltype_auto:1;
    bool is_register:1;
    bool is_static:1;
    bool is_extern:1;
    bool is_mutable:1;
    bool is_thread:1;
    bool is_thread_local:1;
    bool is_friend:1;
    bool is_typedef:1;
    bool is_signed:1;
    bool is_unsigned:1;
    bool is_short:1;
    bool is_const:1;
    bool is_volatile:1;
    bool is_restrict:1;
    bool is_inline:1;
    bool is_virtual:1;
    bool is_explicit:1;
    bool is_complex:1;
    bool is_overriden_type:1;
    bool emit_always:1;
    bool any_exception:1; // Set to 1 if no exception specifier was seen
    bool is_vector:1;
    bool is_final:1;
    bool is_hides_member:1;
    bool is_override:1;
    bool is_constexpr:1;
    bool is_atomic:1;
    bool is_noreturn:1;

    // GCC extension
    bool is_transparent_union:1;

    // Mercurium extensions
    bool is_boolean_integer;
    bool is_mask_integer;
    bool is_mcc_hidden;

    // We are in the declarator of "new T[e]" 
    // 'e' may be non-constant, do not create a VLA entity for it
    bool is_cxx_new_declarator;

    // In some cases we allow gather_type_spec_from_simple_type_specifier to allow templates
    bool allow_class_template_names;

    // It may be 0, 1 (long), 2 (long long) o 3 (__int128)
    unsigned int is_long:2;

    // This type-spec defines (not just declares!) a new type which is
    // accessible through this symbol
    scope_entry_t* defined_type;

    // Mode type for old GCC vector syntax
    struct type_tag* mode_type;

    // dynamic exception-specifiers
    int num_exceptions;
    struct type_tag** exceptions;

    // noexception
    nodecl_t noexception;

    // Vector info
    unsigned int vector_size;

    // Argument info
    int num_arguments_info;
    arguments_info_t* arguments_info;
    
    // VLA info
    int num_vla_dimension_symbols;
    scope_entry_t** vla_dimension_symbols;

    // Attribute info
    int num_gcc_attributes;
    gcc_attribute_t* gcc_attributes;

    // __declspec info
    int num_ms_attributes;
    gcc_attribute_t* ms_attributes;

    // UPC info
    struct
    {
        bool is_shared:1;
        bool is_relaxed:1;
        bool is_strict:1;
        AST shared_layout;
    } upc;

    // CUDA info
    struct
    {
        bool is_global:1;
        bool is_device:1;
        bool is_host:1;
        bool is_shared:1;
        bool is_constant:1;
    } cuda;

    // OpenCL info
    struct
    {
        bool is_kernel:1;
        bool is_constant:1;
        bool is_global:1;
        bool is_local:1;
    } opencl;

    access_specifier_t current_access;

    AST gcc_asm_spec;

    // _Pragma("map") inlined in a declarator
    int num_xl_pragmas;
    const char** xl_pragmas;

    nodecl_t alignas_list;

} gather_decl_spec_t;

typedef
struct gather_decl_spec_list_tag
{
    int num_items;
    gather_decl_spec_t* items;
} gather_decl_spec_list_t;

MCXX_END_DECLS

#endif // CXX_BUILDSCOPE_DECLS_H
