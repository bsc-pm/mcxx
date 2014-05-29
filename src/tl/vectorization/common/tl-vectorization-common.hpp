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

#ifndef TL_VECTORIZATION_COMMON_HPP
#define TL_VECTORIZATION_COMMON_HPP

#include <list>
#include <map>

#include "tl-symbol.hpp"

#define VECTORIZATION_DEBUG() if (CURRENT_CONFIGURATION->debug_options.vectorization_verbose)

namespace TL
{
    namespace Vectorization
    {
        typedef std::map<TL::Symbol, int> aligned_expr_map_t;
        typedef std::map<TL::Symbol, TL::ObjectList<Nodecl::NodeclBase> > nontmp_expr_map_t;
        typedef TL::ObjectList<Nodecl::NodeclBase> objlist_nodecl_t;
        typedef TL::ObjectList<Nodecl::Symbol> objlist_nodecl_symbol_t;
        typedef TL::ObjectList<TL::Symbol> objlist_tlsymbol_t;

        typedef std::list<Nodecl::NodeclBase> stdlist_nodecl_t;
        typedef std::list<TL::Scope> stdlist_scope_t;

        enum SIMDInstructionSet {SSE4_2_ISA, AVX_ISA, AVX2_ISA, AVX512_ISA, KNC_ISA};
    }
}

#endif //TL_VECTORIZATION_COMMON_HPP

