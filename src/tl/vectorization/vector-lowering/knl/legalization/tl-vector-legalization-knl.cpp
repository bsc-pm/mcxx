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

#include "tl-vector-legalization-knl.hpp"


#define NUM_8B_ELEMENTS 8
#define NUM_4B_ELEMENTS 16

namespace TL
{
namespace Vectorization
{
    KNLVectorLegalization::KNLVectorLegalization(bool prefer_gather_scatter,
            bool prefer_mask_gather_scatter)
        : KNCVectorLegalization(prefer_gather_scatter, prefer_mask_gather_scatter)
    {
        std::cerr << "--- KNL legalization phase ---" << std::endl;
    }

    void KNLVectorLegalization::visit(const Nodecl::VectorLoad& n)
    {
        const Nodecl::NodeclBase rhs = n.get_rhs();
        const Nodecl::NodeclBase mask = n.get_mask();
        const Nodecl::List flags = n.get_flags().as<Nodecl::List>();

        walk(rhs);
        walk(mask);
        walk(flags);
    }

    void KNLVectorLegalization::visit(const Nodecl::VectorStore& n)
    {
        const Nodecl::NodeclBase lhs = n.get_lhs();
        const Nodecl::NodeclBase rhs = n.get_rhs();
        const Nodecl::NodeclBase mask = n.get_mask();
        const Nodecl::List flags = n.get_flags().as<Nodecl::List>();

        walk(lhs);
        walk(rhs);
        walk(mask);
        walk(flags);
    }
}
}
