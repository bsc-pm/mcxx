/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include "tl-vectorizer-gather-scatter-optimizer.hpp"

#include "tl-vectorization-analysis-interface.hpp"

#include "tl-nodecl-utils.hpp"
#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
    VectorizerGatherScatterOptimizer::VectorizerGatherScatterOptimizer()
    {
    }


    void VectorizerGatherScatterOptimizer::optimize(
            const Nodecl::VectorGather& n)
    {
        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER: GatherScatterInfo %s\n",
                    n.prettyprint().c_str());
        }

        optimize_gather_scatter(n.get_base(), n.get_strides());

        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER:    Base %s\n",
                    n.get_base().prettyprint().c_str());
            fprintf(stderr, "VECTORIZER:    Strides %s\n",
                    n.get_strides().prettyprint().c_str());
        }
    }

    void VectorizerGatherScatterOptimizer::optimize(
            const Nodecl::VectorScatter& n)
    {
        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER: GatherScatterInfo %s\n",
                    n.prettyprint().c_str());
        }

        optimize_gather_scatter(n.get_base(), n.get_strides());

        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER:    Base %s\n",
                    n.get_base().prettyprint().c_str());
            fprintf(stderr, "VECTORIZER:    Strides %s\n",
                    n.get_strides().prettyprint().c_str());
        }
    }

    void VectorizerGatherScatterOptimizer::optimize_gather_scatter(
            const Nodecl::NodeclBase& base,
            const Nodecl::NodeclBase& strides)
    {
        StrideSplitterVisitor stride_splitter;
        stride_splitter_ret_t result = stride_splitter.walk(strides);

        if(!result.first.is_null())
        {
            Nodecl::ArraySubscript array_base =
                Nodecl::ArraySubscript::make(base.shallow_copy(),
                        Nodecl::List::make(result.first.shallow_copy()),
                        base.get_type().basic_type());

            base.replace(Nodecl::Reference::make(array_base,
                        array_base.get_type().get_pointer_to()));

            ERROR_CONDITION(result.second.is_null(), 
                    "StrideSplitterVisitor: Strides cannot be empty", 0);

            strides.replace(result.second.shallow_copy());
        }
    }

    StrideSplitterVisitor::StrideSplitterVisitor()
    {
    }

    stride_splitter_ret_t StrideSplitterVisitor::join_list(
            ObjectList<stride_splitter_ret_t>& list)
    {
        ERROR_CONDITION(list.size() != 1, "StrideSplitterVisitor: joint list",
                0);

        return list.front();
    }

    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::VectorAdd& n)
    {
        return visit_non_distributive_binary_op<Nodecl::VectorAdd,
              Nodecl::Add>(n);

        /*
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        Nodecl::NodeclBase lhs_no_conv = Nodecl::Utils::advance_conversions(lhs);
        Nodecl::NodeclBase rhs_no_conv = Nodecl::Utils::advance_conversions(rhs);

        bool lhs_is_base_suitable = lhs_no_conv.is<Nodecl::VectorPromotion>();
        bool rhs_is_base_suitable = rhs_no_conv.is<Nodecl::VectorPromotion>();

        Nodecl::NodeclBase base = Nodecl::NodeclBase::null();
        Nodecl::NodeclBase strides = Nodecl::NodeclBase::null();

        if (lhs_is_base_suitable && rhs_is_base_suitable)
        {
            base = Nodecl::Add::make(
                    lhs_no_conv.as<Nodecl::VectorPromotion>().get_rhs(),
                    rhs_no_conv.as<Nodecl::VectorPromotion>().get_rhs(),
                    n.get_type().basic_type());
        }
        else if (lhs_is_base_suitable)
        {
            stride_splitter_ret_t rhs_ret = walk(rhs);

            base = Nodecl::Add::make(lhs_no_conv.as<Nodecl::VectorPromotion>().get_rhs(),
                    rhs_ret.first,
                    n.get_type().basic_type());

            strides = rhs_ret.second;
        }
        else if (rhs_is_base_suitable)
        {
            stride_splitter_ret_t lhs_ret = walk(lhs);

            base = Nodecl::Add::make(lhs_ret.first,
                    rhs_no_conv.as<Nodecl::VectorPromotion>().get_rhs(),
                    n.get_type().basic_type());

            strides = lhs_ret.second;
        }
        else
        {
            stride_splitter_ret_t lhs_ret = walk(lhs);
            stride_splitter_ret_t rhs_ret = walk(rhs);

            // Base
            if (!lhs_ret.first.is_null() && !rhs_ret.first.is_null())
            {
                base = Nodecl::Add::make(lhs_ret.first,
                        rhs_ret.first,
                        n.get_type().basic_type());
            }
            else if (!lhs_ret.first.is_null())
            {
                base = lhs_ret.first;
            }
            else if (!rhs_ret.first.is_null())
            {
                base = rhs_ret.first;
            }

            // Strides
            if (!lhs_ret.second.is_null() && !rhs_ret.second.is_null())
            {
                strides = Nodecl::VectorAdd::make(lhs_ret.second,
                        rhs_ret.second,
                        n.get_mask(),
                        n.get_type());
            }
            else if (!lhs_ret.second.is_null())
            {
                base = lhs_ret.second;
            }
            else if (!rhs_ret.second.is_null())
            {
                base = rhs_ret.second;
            }
        }
        */
/*
        stride_splitter_ret_t lhs_ret = walk(n.get_lhs());
        stride_splitter_ret_t rhs_ret = walk(n.get_rhs());

        // Base
        Nodecl::NodeclBase base = Nodecl::NodeclBase::null();

        if (!lhs_ret.first.is_null() && !rhs_ret.first.is_null())
        {
            base = Nodecl::Add::make(lhs_ret.first,
                    rhs_ret.first,
                    n.get_type());
        }
        else if (!lhs_ret.first.is_null())
        {
            base = lhs_ret.first;
        }
        else if (!rhs_ret.first.is_null())
        {
            base = rhs_ret.first;
        }

        // Strides
        Nodecl::NodeclBase strides = Nodecl::NodeclBase::null();

        if (!lhs_ret.second.is_null() && !rhs_ret.second.is_null())
        {
            strides = Nodecl::Add::make(lhs_ret.second,
                    rhs_ret.second,
                    n.get_type());
        }
        else if (!lhs_ret.second.is_null())
        {
            strides = lhs_ret.second;
        }
        else if (!rhs_ret.second.is_null())
        {
            strides = rhs_ret.second;
        }

        // pair<n, null>
        return stride_splitter_ret_t(base, strides);
        */

//        return stride_splitter_ret_t(base, strides);
    }

    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::Neg& n)
    {
        running_error("NEG");
        stride_splitter_ret_t rhs_ret = walk(n.get_rhs());

        // Base
        Nodecl::NodeclBase base = Nodecl::NodeclBase::null();

        if (!rhs_ret.first.is_null())
        {
            base = Nodecl::Neg::make(rhs_ret.first,
                    n.get_type());
        }

        // Strides
        Nodecl::NodeclBase strides = Nodecl::NodeclBase::null();

        if (!rhs_ret.second.is_null())
        {
            strides = Nodecl::Neg::make(rhs_ret.second,
                    n.get_type());
        }

        // pair<n, null>
        return stride_splitter_ret_t(base, strides);
    }
 
    template <typename VECTOR_NODE, typename SCALAR_NODE>
    stride_splitter_ret_t StrideSplitterVisitor::visit_non_distributive_binary_op(
            const Nodecl::NodeclBase& n)
    {
        VECTOR_NODE node = n.as<VECTOR_NODE>();
        
        /* 
        std::cerr << "Mul: [ "
            << (lhs_ret.first.is_null() ? " - " : lhs_ret.first.prettyprint()) << " , "
            << (lhs_ret.second.is_null() ? " - " : lhs_ret.second.prettyprint()) << " ] * [ "
            << (rhs_ret.first.is_null() ? " - " : rhs_ret.first.prettyprint()) << " , "
            << (rhs_ret.second.is_null() ? " - " : rhs_ret.second.prettyprint()) << " ]"
            << std::endl;
        */

        Nodecl::NodeclBase lhs = node.get_lhs();
        Nodecl::NodeclBase rhs = node.get_rhs();
        Nodecl::NodeclBase lhs_no_conv = Nodecl::Utils::advance_conversions(lhs);
        Nodecl::NodeclBase rhs_no_conv = Nodecl::Utils::advance_conversions(rhs);

        bool lhs_is_base_suitable = lhs_no_conv.is<Nodecl::VectorPromotion>();
        bool rhs_is_base_suitable = rhs_no_conv.is<Nodecl::VectorPromotion>();

        Nodecl::NodeclBase base = Nodecl::NodeclBase::null();
        Nodecl::NodeclBase strides = Nodecl::NodeclBase::null();

        if (lhs_is_base_suitable && rhs_is_base_suitable)
        {
            base = SCALAR_NODE::make(
                    lhs_no_conv.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                    rhs_no_conv.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                    n.get_type().basic_type());
        }
        else if (lhs_is_base_suitable)
        {
            stride_splitter_ret_t rhs_ret = walk(rhs);

            if (!rhs_ret.first.is_null())
            {
                base = SCALAR_NODE::make(lhs_no_conv.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        rhs_ret.first.shallow_copy(),
                        n.get_type().basic_type());
            }
            else
            {
                base = lhs_no_conv.as<Nodecl::VectorPromotion>().get_rhs();
            }

            strides = rhs_ret.second;
        }
        else if (rhs_is_base_suitable)
        {
            stride_splitter_ret_t lhs_ret = walk(lhs);

            if (!lhs_ret.first.is_null())
            {
                base = SCALAR_NODE::make(lhs_ret.first.shallow_copy(),
                        rhs_no_conv.as<Nodecl::VectorPromotion>().get_rhs().shallow_copy(),
                        n.get_type().basic_type());
            }
            else
            {
                base = rhs_no_conv.as<Nodecl::VectorPromotion>().get_rhs();
            }

            strides = lhs_ret.second;
        }
        else
        {
            stride_splitter_ret_t lhs_ret = walk(lhs);
            stride_splitter_ret_t rhs_ret = walk(rhs);

            // Base
            if (!lhs_ret.first.is_null() && !rhs_ret.first.is_null())
            {
                base = SCALAR_NODE::make(lhs_ret.first.shallow_copy(),
                        rhs_ret.first.shallow_copy(),
                        n.get_type().basic_type());
            }
            else if (!lhs_ret.first.is_null())
            {
                base = lhs_ret.first;
            }
            else if (!rhs_ret.first.is_null())
            {
                base = rhs_ret.first;
            }

            // Strides
            if (!lhs_ret.second.is_null() && !rhs_ret.second.is_null())
            {
                strides = VECTOR_NODE::make(lhs_ret.second.shallow_copy(),
                        rhs_ret.second.shallow_copy(),
                        node.get_mask().shallow_copy(),
                        n.get_type());
            }
            else if (!lhs_ret.second.is_null())
            {
                strides = lhs_ret.second;
            }
            else if (!rhs_ret.second.is_null())
            {
                strides = rhs_ret.second;
            }
        }

        // pair<n, null>
        return stride_splitter_ret_t(base, strides);
    }

    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::VectorBitwiseShl& n)
    {
        return visit_non_distributive_binary_op<Nodecl::VectorBitwiseShl,
               Nodecl::BitwiseShl>(n);
    }

    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::VectorMul& n)
    {
        return visit_non_distributive_binary_op<Nodecl::VectorMul,
               Nodecl::Mul>(n);
    }

    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::VectorConversion& n)
    {
        stride_splitter_ret_t result = walk(n.get_nest());
       
        Nodecl::NodeclBase base = Nodecl::NodeclBase::null();
        Nodecl::NodeclBase strides = Nodecl::NodeclBase::null();

        if (!result.first.is_null())
        {
            base = Nodecl::Conversion::make(result.first.shallow_copy(),
                    n.get_type().basic_type());
        }

        if (!result.second.is_null())
        {
            strides = Nodecl::VectorConversion::make(result.second.shallow_copy(),
                    n.get_mask().shallow_copy(),
                    n.get_type());
        }

        return stride_splitter_ret_t(base, strides);
    }

    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::VectorLiteral& n)
    {
        // TODO: check if constat is a scalar promotion
        return stride_splitter_ret_t(Nodecl::NodeclBase::null(), n);
    }

    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::Symbol& n)
    {
        ERROR_CONDITION(!n.get_symbol().get_type().is_vector(), 
                "StrideSplitterVisitor: TL::Symbol has no vector type", 0);
        return stride_splitter_ret_t(Nodecl::NodeclBase::null(), n);
    }


    /*
    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::IntegerLiteral& n)
    {
        // pair<n, null>
        return stride_splitter_ret_t(n, Nodecl::NodeclBase::null());

    }

    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::Symbol& n)
    {
        // SIMD IV
        if (VectorizationAnalysisInterface::_vectorizer_analysis->
                is_non_reduction_basic_induction_variable(
                    _environment._analysis_simd_scope, n))
        {
            std::cerr << "IV" << std::endl;

            // pair<n, lane_id * step>
            Nodecl::NodeclBase step = VectorizationAnalysisInterface::
                _vectorizer_analysis->get_induction_variable_increment(
                        _environment._analysis_simd_scope, n);

            return stride_splitter_ret_t(n, Nodecl::Mul::make(
                        Nodecl::VectorLaneId::make(n.get_type().no_ref(),
                            const_value_get_zero(4, 1)),
                        step.shallow_copy(),
                        n.get_type().no_ref()));
        }
        // TL::Symbol has vector type
        else if (n.get_symbol().get_type().is_vector())
        {
            // pair<null, n>
            return stride_splitter_ret_t(Nodecl::NodeclBase::null(), n);
        }
        else
        {
            // pair<n, null>
            return stride_splitter_ret_t(n, Nodecl::NodeclBase::null());
        }
    }
    
    */
    stride_splitter_ret_t StrideSplitterVisitor::unhandled_node(
            const Nodecl::NodeclBase& n)
    {
        internal_error("StrideSplitter: Unhandled node %s at %s.\n%s",
                ast_print_node_type(n.get_kind()),
                n.get_locus(),
                n.prettyprint().c_str());

        return Ret();
    }

}
}
