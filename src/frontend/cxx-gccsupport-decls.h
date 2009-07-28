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
#ifndef CXX_GCCSUPPORT_DECLS_H
#define CXX_GCCSUPPORT_DECLS_H

#define MAX_GCC_ATTRIBUTES_PER_SYMBOL (16)

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

typedef struct gather_gcc_attribute_tag
{
    const char *attribute_name;
    AST expression_list;
} gather_gcc_attribute_t;

MCXX_END_DECLS

#endif // CXX_GCCSUPPORT_DECLS_H
