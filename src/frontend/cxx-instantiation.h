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
#ifndef CXX_INSTANTIATION_H
#define CXX_INSTANTIATION_H

#include "cxx-scope.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

void instantiate_template(scope_entry_t* entry, decl_context_t decl_context);

scope_entry_t* create_holding_symbol_for_template(matching_pair_t* matched_template, template_argument_list_t*
        arguments, int instantiation_line, char* instantiation_file, decl_context_t decl_context);

MCXX_END_DECLS

#endif // CXX_INSTANTIATION_H
