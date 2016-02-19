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
        namespace {
            TL::Type sse_comparison_type()
            {
                return TL::Type::get_int_type().get_vector_of_elements(4);
            }

            void fix_mask_symbol(TL::Symbol sym)
            {
                if (sym.get_type().is_mask())
                {
                    sym.set_type(sse_comparison_type());
                }
            }

            void fix_comparison_type(Nodecl::NodeclBase node)
            {
                // There is no mask type in SSE, __m128i is used instead
                if (node.get_type().is_mask())
                    node.set_type(sse_comparison_type());
                else if (node.get_type().is_lvalue_reference()
                        && node.get_type().no_ref().is_mask())
                    node.set_type(sse_comparison_type().get_lvalue_reference_to());
            }
        }

        SSEVectorLegalization::SSEVectorLegalization()
        {
            std::cerr << "--- SSE legalization phase ---" << std::endl;
        }

        void SSEVectorLegalization::visit(const Nodecl::Symbol &node)
        {
            fix_mask_symbol(node.get_symbol());
            fix_comparison_type(node);
        }

        void SSEVectorLegalization::visit(const Nodecl::ObjectInit& node) 
        {
            TL::Source intrin_src;

            TL::Symbol sym = node.get_symbol();
            fix_mask_symbol(sym);

            // Vectorizing initialization
            Nodecl::NodeclBase init = sym.get_value();
            if (!init.is_null())
            {
                walk(init);
            }
        }

#define BINARY_MASK_OPS(Node) \
        void SSEVectorLegalization::visit(const Nodecl::Node& n) \
        { \
            walk(n.get_lhs()); \
            walk(n.get_rhs()); \
            fix_comparison_type(n); \
        }

        BINARY_MASK_OPS(VectorMaskAssignment)
        BINARY_MASK_OPS(VectorLowerThan)
        BINARY_MASK_OPS(VectorLowerOrEqualThan)
        BINARY_MASK_OPS(VectorGreaterThan)
        BINARY_MASK_OPS(VectorGreaterOrEqualThan)
        BINARY_MASK_OPS(VectorEqual)
        BINARY_MASK_OPS(VectorDifferent)
        BINARY_MASK_OPS(VectorMaskOr)
        BINARY_MASK_OPS(VectorMaskAnd)
        BINARY_MASK_OPS(VectorMaskAnd1Not)
        BINARY_MASK_OPS(VectorMaskAnd2Not)
        BINARY_MASK_OPS(VectorMaskXor)

#define UNARY_MASK_OPS(Node) \
        void SSEVectorLegalization::visit(const Nodecl::Node& n) \
        { \
            walk(n.children()[0]); \
            fix_comparison_type(n); \
        }
        
        UNARY_MASK_OPS(VectorMaskNot)

        void SSEVectorLegalization::visit(
            const Nodecl::VectorMaskConversion &node)
        {
            walk(node.get_nest());

            Nodecl::VectorConversion vec_conv = Nodecl::VectorConversion::make(
                    node.get_nest().shallow_copy(),
                    Nodecl::NodeclBase::null() /* mask */,
                    node.get_type(),
                    node.get_locus());

            fix_comparison_type(vec_conv);
            //Visit new VectorConversion
            walk(vec_conv);
        
            node.replace(vec_conv);
        }
    }
}
