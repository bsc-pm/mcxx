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

#include "tl-vector-legalization-sse.hpp"
#include "tl-source.hpp"

#include "cxx-graphviz.h"

namespace TL 
{
    namespace Vectorization
    {
        SSEVectorLegalization::SSEVectorLegalization() 
        {
            std::cerr << "--- SSE legalization phase ---" << std::endl;
        }

        void SSEVectorLegalization::visit(const Nodecl::ObjectInit& node) 
        {
            TL::Source intrin_src;
            
            if(node.has_symbol())
            {
                TL::Symbol sym = node.get_symbol();

                // Vectorizing initialization
                Nodecl::NodeclBase init = sym.get_value();
                if(!init.is_null())
                {
                    walk(init);
                }
            }
        }

        void SSEVectorLegalization::fix_comparison_type(Nodecl::NodeclBase node)
        {
            // There is no mask type in SSE, __m128i is used instead
            node.set_type(TL::Type::get_int_type().get_vector_of_elements(4));
        }

        void SSEVectorLegalization::visit(const Nodecl::VectorLowerThan& n)
        {
            fix_comparison_type(n);
        }

        void SSEVectorLegalization::visit(const Nodecl::VectorLowerOrEqualThan& n)
        {
            fix_comparison_type(n);
        }

        void SSEVectorLegalization::visit(const Nodecl::VectorGreaterThan& n)
        {
            fix_comparison_type(n);
        }

        void SSEVectorLegalization::visit(const Nodecl::VectorGreaterOrEqualThan& n)
        {
            fix_comparison_type(n);
        }

        void SSEVectorLegalization::visit(const Nodecl::VectorEqual& n)
        {
            fix_comparison_type(n);
        }

        void SSEVectorLegalization::visit(const Nodecl::VectorDifferent& n)
        {
            fix_comparison_type(n);
        }
    }
}
