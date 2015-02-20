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

#ifndef TL_VECTORIZER_GATHER_SCATTER_INFO_HPP
#define TL_VECTORIZER_GATHER_SCATTER_INFO_HPP

#include "tl-vectorizer-environment.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-nodecl-base.hpp"


namespace TL
{
namespace Vectorization
{
    typedef std::pair<Nodecl::NodeclBase, Nodecl::NodeclBase>
        stride_splitter_ret_t;

    class VectorizerGatherScatterOptimizer
    {
        private:
            void optimize_gather_scatter(const Nodecl::NodeclBase& base,
                    const Nodecl::NodeclBase& strides);

        public:
            VectorizerGatherScatterOptimizer();

            void optimize(const Nodecl::VectorGather& n);
            void optimize(const Nodecl::VectorScatter& n);
    };
    
    class StrideSplitterVisitor : public Nodecl::NodeclVisitor<
                                  stride_splitter_ret_t> 
    {
        private:
            template <typename VectorNode, typename ScalarNode,
                     typename Functor>
            stride_splitter_ret_t visit_distributive_binary_op(
                    const Nodecl::NodeclBase& n,
                    Functor const_operation);
 
            template <typename VectorNode, typename ScalarNode,
                     typename Functor>
            stride_splitter_ret_t visit_non_distributive_binary_op(
                    const Nodecl::NodeclBase& n,
                    Functor const_operation);
 

        public:
            StrideSplitterVisitor();

            stride_splitter_ret_t visit(const Nodecl::VectorAdd& n);
            stride_splitter_ret_t visit(const Nodecl::VectorMinus& n);
            stride_splitter_ret_t visit(const Nodecl::Neg& n);
            stride_splitter_ret_t visit(const Nodecl::VectorBitwiseShl& n);
            stride_splitter_ret_t visit(const Nodecl::VectorBitwiseShr& n);
            stride_splitter_ret_t visit(const Nodecl::VectorGather& n);
            stride_splitter_ret_t visit(const Nodecl::VectorMul& n);
            stride_splitter_ret_t visit(const Nodecl::VectorConversion& n);
            stride_splitter_ret_t visit(const Nodecl::VectorLiteral& n);
            stride_splitter_ret_t visit(const Nodecl::Symbol& n);

            //stride_splitter_ret_t visit(const Nodecl::IntegerLiteral& n );
            //stride_splitter_ret_t visit(const Nodecl::Conversion& n);

            stride_splitter_ret_t join_list(ObjectList<stride_splitter_ret_t>& list);
            stride_splitter_ret_t unhandled_node(const Nodecl::NodeclBase& n);
    };
}
}

#endif //TL_VECTORIZER_GATHER_SCATTER_INFO_HPP

