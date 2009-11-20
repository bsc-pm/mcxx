/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#ifndef TL_OMP_UDR_HPP
#define TL_OMP_UDR_HPP

#include "tl-symbol.hpp"
#include "tl-type.hpp"
#include "tl-omp.hpp"

namespace TL
{
    void initialize_builtin_udr_reductions(Scope global_scope);
    bool udr_is_builtin_operator(const std::string &op_name);

    Symbol solve_udr_name_cxx(LangConstruct construct,
            Scope scope_of_clause,
            AST_t op_name,
            Type reduction_type,
            OpenMP::UDRInfoItem::Associativity &assoc);

    OpenMP::UDRInfoItem udr_lookup_cxx(
            const std::string& udr_name,
            ObjectList<Symbol> udr_sym_list, 
            Type type, 
            ScopeLink scope_link,
            Scope current_scope,
            const std::string& filename, int line);
}

#endif // TL_OMP_UDR_HPP
