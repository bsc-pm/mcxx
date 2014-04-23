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

#include "tl-vectorizer-gather-scatter-info.hpp"

#include "tl-vectorization-analysis-interface.hpp"

#include "tl-nodecl-utils.hpp"

namespace TL
{
namespace Vectorization
{
    VectorizerGatherScatterInfo::VectorizerGatherScatterInfo(
            const VectorizerEnvironment& environment)
        : _environment(environment), _access(Nodecl::NodeclBase::null()),
        _base(Nodecl::NodeclBase::null()), _strides(Nodecl::NodeclBase::null())
    {
    }

    void VectorizerGatherScatterInfo::compute_info()
    {
        ERROR_CONDITION(!_access.is<Nodecl::ArraySubscript>(),
                "VectorizerGatherScatterInfo: Access is not an ArraySubscript",
                0);

        Nodecl::ArraySubscript array = Nodecl::Utils::linearize_array_subscript(
                _access.as<Nodecl::ArraySubscript>());

        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER: GatherScatterInfo %s\n",
                    array.prettyprint().c_str());
        }

        Nodecl::NodeclBase subscripted = array.get_subscripted();
        Nodecl::NodeclBase subscript = Nodecl::Utils::advance_conversions(
                array.get_subscripts().as<Nodecl::List>().front());


        StrideSplitterVisitor splitting_visitor(_environment);

        StrideSplitterVisitor::Ret splitted_stride_pair =
            splitting_visitor.walk(subscript);

        // Store base
        if (splitted_stride_pair.first.is_null())
        {
            // If there is nothing "constant" to add to the base
            _base = subscripted;
        }
        else
        {
            // &a[i]
            _base = Nodecl::Reference::make(
                    Nodecl::ArraySubscript::make(subscripted.shallow_copy(),
                        Nodecl::List::make(
                            splitted_stride_pair.first.shallow_copy()),
                        array.get_type()),
                    array.get_type().get_pointer_to());
        }

        // Store stride
        _strides = splitted_stride_pair.second;
        ERROR_CONDITION(_strides.is_null(),
                "VectorizerGatherScatterInfo: Strides null in a gather/scatter"\
                " operaration", 0);
    }

    Nodecl::NodeclBase VectorizerGatherScatterInfo::get_base(
            const Nodecl::ArraySubscript& n)
    {
        if (n.is_null() || _access != n)
        {
            _access = n;
            compute_info();
        }

        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER:    Base %s\n",
                    _base.prettyprint().c_str());
        }

        return _base.shallow_copy();
    }

    Nodecl::NodeclBase VectorizerGatherScatterInfo::get_strides(
            const Nodecl::ArraySubscript& n)
    {
        if (n.is_null() || _access != n)
        {
            _access = n;
            compute_info();
        }

        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER:    Strides %s\n",
                    _strides.prettyprint().c_str());
        }

        return _strides.shallow_copy();
    }


    StrideSplitterVisitor::StrideSplitterVisitor(
            const VectorizerEnvironment& environment)
        :_environment(environment)
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
            const Nodecl::Add& n)
    {
        stride_splitter_ret_t lhs_ret = walk(n.get_lhs());
        stride_splitter_ret_t rhs_ret = walk(n.get_rhs());

        // Base
        Nodecl::NodeclBase base = Nodecl::NodeclBase::null();

        if (!lhs_ret.first.is_null() && !rhs_ret.first.is_null())
        {
            base = Nodecl::Add::make(lhs_ret.first.shallow_copy(),
                    rhs_ret.first.shallow_copy(),
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
            strides = Nodecl::Add::make(lhs_ret.second.shallow_copy(),
                    rhs_ret.second.shallow_copy(),
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
    }

    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::Neg& n)
    {
        stride_splitter_ret_t rhs_ret = walk(n.get_rhs());

        // Base
        Nodecl::NodeclBase base = Nodecl::NodeclBase::null();

        if (!rhs_ret.first.is_null())
        {
            base = Nodecl::Neg::make(rhs_ret.first.shallow_copy(),
                    n.get_type());
        }

        // Strides
        Nodecl::NodeclBase strides = Nodecl::NodeclBase::null();

        if (!rhs_ret.second.is_null())
        {
            strides = Nodecl::Neg::make(rhs_ret.second.shallow_copy(),
                    n.get_type());
        }

        // pair<n, null>
        return stride_splitter_ret_t(base, strides);
    }
 
    template <class node>
    stride_splitter_ret_t StrideSplitterVisitor::visit_non_distributive_binary_op(
            const Nodecl::NodeclBase& n)
    {
        Nodecl::Mul mul = n.as<Nodecl::Mul>();

        stride_splitter_ret_t lhs_ret = walk(mul.get_lhs());
        stride_splitter_ret_t rhs_ret = walk(mul.get_rhs());

        // Base
        Nodecl::NodeclBase base = Nodecl::NodeclBase::null();
        // Strides
        Nodecl::NodeclBase strides = Nodecl::NodeclBase::null();
        /*
        std::cerr << "Mul: [ "
            << (lhs_ret.first.is_null() ? " - " : lhs_ret.first.prettyprint()) << " , "
            << (lhs_ret.second.is_null() ? " - " : lhs_ret.second.prettyprint()) << " ] * [ "
            << (rhs_ret.first.is_null() ? " - " : rhs_ret.first.prettyprint()) << " , "
            << (rhs_ret.second.is_null() ? " - " : rhs_ret.second.prettyprint()) << " ]"
            << std::endl;
        */
        // Too complicated. Worst case. Base will be empty
        if (!lhs_ret.second.is_null())
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "StrideSplitter: Split too complicated.\n");
            }

            running_error("StrideSplitter: Too complicated");

            strides = n.shallow_copy();
        }
        // [N+1 , null] * [i , VECTOR_LANE * STEP]
        else if (!lhs_ret.first.is_null())
        {
            if (!rhs_ret.first.is_null())
            {
                base = Nodecl::Mul::make(lhs_ret.first.shallow_copy(),
                        rhs_ret.first.shallow_copy(),
                        n.get_type());
            }
            if (!rhs_ret.second.is_null())
            {
                strides = Nodecl::Mul::make(lhs_ret.first.shallow_copy(),
                        rhs_ret.second.shallow_copy(),
                        n.get_type());
            }
        }
        else
        {
            running_error("StrideSplitter: This shouldn't have happened");
        }

        // pair<n, null>
        return stride_splitter_ret_t(base, strides);
    }

    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::BitwiseShl& n)
    {
        return visit_non_distributive_binary_op<Nodecl::BitwiseShl>(n);
    }

    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::Mul& n)
    {
        return visit_non_distributive_binary_op<Nodecl::Mul>(n);
    }

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
            // pair<n, lane_id * step>
            Nodecl::NodeclBase step = VectorizationAnalysisInterface::
                _vectorizer_analysis->get_induction_variable_increment(
                        _environment._analysis_simd_scope, n);

            return stride_splitter_ret_t(n, Nodecl::Mul::make(
                        Nodecl::VectorLaneId::make(n.get_type().no_ref()),
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

    stride_splitter_ret_t StrideSplitterVisitor::visit(
            const Nodecl::Conversion& n)
    {
        return walk(n.get_nest());
    }

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
