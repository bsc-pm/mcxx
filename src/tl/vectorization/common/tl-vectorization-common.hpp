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

#ifndef TL_VECTORIZATION_COMMON_HPP
#define TL_VECTORIZATION_COMMON_HPP

#include <list>
#include <map>

#include "tl-symbol.hpp"

#define VECTORIZATION_DEBUG() if (debug_options.vectorization_verbose)

namespace TL
{
    namespace Vectorization
    {
        typedef std::map<TL::Symbol, int> map_tlsym_int_t;
        typedef std::map<Nodecl::NodeclBase, int> map_nodecl_int_t;
        typedef std::pair<Nodecl::NodeclBase, int> pair_nodecl_int_t;
        // To be replaced by std::tuple<int, int, int> in C++11
        typedef std::map<TL::Symbol, TL::ObjectList<Nodecl::NodeclBase> > map_tlsym_objlist_t;
        typedef std::map<TL::Symbol, TL::ObjectList<int> > map_tlsym_objlist_int_t;
        typedef TL::ObjectList<Nodecl::NodeclBase> objlist_nodecl_t;
        typedef TL::ObjectList<Nodecl::Symbol> objlist_nodecl_symbol_t;
        typedef TL::ObjectList<TL::Symbol> objlist_tlsym_t;
        typedef TL::ObjectList<int> objlist_int_t;

        typedef std::list<Nodecl::NodeclBase> stdlist_nodecl_t;
        typedef std::list<TL::Scope> stdlist_scope_t;

        enum VectorInstructionSet {
            SSE4_2_ISA, // default
            AVX2_ISA,
            KNC_ISA,
            KNL_ISA,
            NEON_ISA,
            ROMOL_ISA,
        };
    }
}

#endif //TL_VECTORIZATION_COMMON_HPP

