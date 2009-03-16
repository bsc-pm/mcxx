/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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

#include "cxx-macros.h"
#include "cxx-scope-decls.h"
#include "cxx-gccsupport-decls.h"

MCXX_BEGIN_DECLS


// This structure gather things of a declaration in one place so we can use
// along a whole declaration. Parts of a declaration belong just to type while
// others belong to the symbol but they do not appear syntactically in the same
// place
typedef 
struct gather_decl_spec_tag {
    // type-specifiers and decl-specifiers
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
    char is_overriden_type;

    // FIXME
    struct type_tag* mode_type;

    // exception-specifiers
    char any_exception; // Set to 1 if no exception specifier was seen
    int num_exceptions;
    struct type_tag** exceptions;

    // Vector info
    unsigned int vector_size;
    char is_vector;
    int num_parameters;
    struct default_argument_info_tag **default_argument_info;

    // Attribute info
    int num_gcc_attributes;
    gather_gcc_attribute_t gcc_attributes[MAX_GCC_ATTRIBUTES_PER_SYMBOL];
} gather_decl_spec_t;

MCXX_END_DECLS

#endif // CXX_BUILDSCOPE_DECLS_H
