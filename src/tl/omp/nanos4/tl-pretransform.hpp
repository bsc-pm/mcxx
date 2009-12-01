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

#ifndef TL_PRETRANSFORM_HPP
#define TL_PRETRANSFORM_HPP

#include "tl-omp.hpp"
#include <utility>

namespace TL
{
    namespace Nanos4
    {
        class OpenMP_PreTransform : public OpenMP::OpenMPPhase
        {
            private:
                typedef std::pair<Symbol, ObjectList<Symbol> > function_symbols_pair_t;
                typedef ObjectList<function_symbols_pair_t> function_sym_list_t;
                function_sym_list_t _function_sym_list;

                void add_local_symbol(function_sym_list_t& sym_list, Symbol function, Symbol local);

                ObjectList<Symbol> get_all_functions(const function_sym_list_t& sym_list);
                ObjectList<Symbol> get_symbols_of_function(const function_sym_list_t& sym_list, 
                        Symbol function_sym);

                void remove_symbol_declaration(Symbol sym);

                ScopeLink _scope_link;

                int _function_num;
            public:
                OpenMP_PreTransform();

                void handle_threadprivate(PragmaCustomConstruct);

                void purge_local_threadprivates();

                virtual void init(DTO& dto);
        };
    }
}

#endif // TL_PRETRANSFORM_HPP
