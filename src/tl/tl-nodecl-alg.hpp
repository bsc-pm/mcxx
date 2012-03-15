/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#ifndef TL_NODECL_ALG_HPP
#define TL_NODECL_ALG_HPP

#include "tl-nodecl.hpp"

#include <tr1/unordered_map>

namespace Nodecl
{
    namespace Utils
    {
        TL::ObjectList<TL::Symbol> get_all_symbols(Nodecl::NodeclBase);
        TL::ObjectList<TL::Symbol> get_nonlocal_symbols(Nodecl::NodeclBase);
        TL::ObjectList<TL::Symbol> get_local_symbols(Nodecl::NodeclBase);

        TL::ObjectList<Nodecl::Symbol> get_all_symbols_occurrences(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_nonlocal_symbols_occurrences(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_local_symbols_occurrences(Nodecl::NodeclBase);

        TL::ObjectList<Nodecl::Symbol> get_all_symbols_first_occurrence(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_nonlocal_symbols_first_occurrence(Nodecl::NodeclBase);
        TL::ObjectList<Nodecl::Symbol> get_local_symbols_first_occurrence(Nodecl::NodeclBase);
       
        bool equal_nodecls(Nodecl::NodeclBase n1, Nodecl::NodeclBase n2);
        struct Nodecl_hash {
            size_t operator() (const Nodecl::NodeclBase& n) const;
        };
        struct Nodecl_comp {
            bool operator() (const Nodecl::NodeclBase& n1, const Nodecl::NodeclBase& n2) const;
        };

        NodeclBase reduce_expression(Nodecl::NodeclBase n);
        NodeclBase algebraic_simplification(Nodecl::NodeclBase n);

        Nodecl::List get_all_list_from_list_node(Nodecl::List);

        void remove_from_enclosing_list(Nodecl::NodeclBase n);

        TL::Symbol get_enclosing_function(Nodecl::NodeclBase n);

        void append_to_top_level_nodecl(Nodecl::NodeclBase n);
    }
}

namespace TL
{
    struct ForStatement : Nodecl::ForStatement
    {
        public:
            ForStatement(const Nodecl::ForStatement n)
                : Nodecl::ForStatement(n) { }

            bool is_regular_loop() const;
            Symbol get_induction_variable() const;
    };
}

#endif // TL_NODECL_ALG_HPP
