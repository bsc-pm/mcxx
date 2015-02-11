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

#include "tl-vectorizer-visitor-postprocessor.hpp"

#include "tl-vectorizer-gather-scatter-optimizer.hpp"

#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
    VectorizerVisitorPostprocessor::VectorizerVisitorPostprocessor()
    {
    }

    void VectorizerVisitorPostprocessor::visit(const Nodecl::ObjectInit& n)
    {
        TL::Symbol sym = n.get_symbol();
        Nodecl::NodeclBase init = sym.get_value();

        if(!init.is_null())
        {
            walk(init);
        }
    }
 
    void VectorizerVisitorPostprocessor::visit(const Nodecl::VectorGather& n)
    {
        VectorizerGatherScatterOptimizer gather_scatter_optimizer;
        gather_scatter_optimizer.optimize(n);
    }

    void VectorizerVisitorPostprocessor::visit(const Nodecl::VectorScatter& n)
    {
        VectorizerGatherScatterOptimizer gather_scatter_optimizer;
        gather_scatter_optimizer.optimize(n);
    }
}
}
