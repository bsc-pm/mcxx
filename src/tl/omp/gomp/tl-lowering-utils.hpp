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


#ifndef TL_LOWERING_UTILS_HPP
#define TL_LOWERING_UTILS_HPP

#include "tl-symbol.hpp"

namespace TL { namespace GOMP {

    TL::Symbol new_private_symbol(TL::Symbol original_symbol, TL::Scope private_scope);
    TL::Symbol new_private_symbol(const std::string& base_name,
            TL::Type type,
            enum cxx_symbol_kind kind,
            TL::Scope private_scope);

    void gather_vla_symbols(TL::Symbol symbol,
            TL::ObjectList<TL::Symbol>& extra_symbols);


    TL::Type create_outline_struct(
            const TL::ObjectList<TL::Symbol>& all_passed_symbols,
            TL::Symbol enclosing_function,
            const locus_t* locus);

    TL::Type create_outline_struct_task(
            const TL::ObjectList<TL::Symbol>& all_passed_symbols,
            const TL::ObjectList<TL::Symbol>& firstprivate_symbols,
            TL::Symbol enclosing_function,
            const locus_t* locus);
} }

#endif // TL_LOWERING_UTILS_HPP
