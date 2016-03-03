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

#ifndef TL_VECTORIZER_VISITOR_EXPRESSION_HPP
#define TL_VECTORIZER_VISITOR_EXPRESSION_HPP

#include "tl-nodecl-visitor.hpp"
#include "tl-vectorizer.hpp"

namespace TL
{
    namespace Vectorization
    {
        class VectorizerVisitorExpression : public Nodecl::NodeclVisitor<void>
        {
            protected:
                VectorizerEnvironment& _environment;

                bool process_fmul_op(const Nodecl::NodeclBase&  n);
                void symbol_type_promotion(const Nodecl::Symbol& n);

                template <typename ScalarNode, typename VectorNode>
                    void visit_binary_op(const ScalarNode& n,
                            const bool returns_mask_type);
                template <typename ScalarNode, typename VectorRegularNode, typename VectorMaskNode>
                    void visit_bitwise_binary_op(const ScalarNode& n);
         
                Nodecl::NodeclBase get_memory_vector_read(const Nodecl::NodeclBase& n);
                Nodecl::NodeclBase get_memory_vector_write(const Nodecl::NodeclBase& lhs,
                        const Nodecl::NodeclBase& rhs,
                        const Nodecl::NodeclBase& mask,
                        const TL::Type type);
                void vectorize_regular_class_member_access(const Nodecl::ClassMemberAccess &n);

            public:
                VectorizerVisitorExpression(
                        VectorizerEnvironment& environment);

                virtual void visit(const Nodecl::Add& n);
                virtual void visit(const Nodecl::Minus& n);
                virtual void visit(const Nodecl::Mul& n);
                virtual void visit(const Nodecl::Div& n);
                virtual void visit(const Nodecl::Mod& n);
                virtual void visit(const Nodecl::Neg& n);

                virtual void visit(const Nodecl::LowerThan& n);
                virtual void visit(const Nodecl::LowerOrEqualThan& n);
                virtual void visit(const Nodecl::GreaterThan& n);
                virtual void visit(const Nodecl::GreaterOrEqualThan& n);
                virtual void visit(const Nodecl::Equal& n);
                virtual void visit(const Nodecl::Different& n);
                virtual void visit(const Nodecl::BitwiseAnd& n);
                virtual void visit(const Nodecl::BitwiseOr& n);
                virtual void visit(const Nodecl::BitwiseShl& n);
                virtual void visit(const Nodecl::ArithmeticShr& n);
                virtual void visit(const Nodecl::BitwiseShr& n);
                virtual void visit(const Nodecl::LogicalAnd& n);
                virtual void visit(const Nodecl::LogicalOr& n);
                virtual void visit(const Nodecl::ConditionalExpression& n);

                virtual void visit(const Nodecl::Assignment& n);
                virtual void visit(const Nodecl::Conversion& n);
                virtual void visit(const Nodecl::ArraySubscript& n);
                virtual void visit(const Nodecl::FunctionCall& n);
                virtual void visit(const Nodecl::Symbol& n);
                virtual void visit(const Nodecl::ClassMemberAccess& n);
                virtual void visit(const Nodecl::IntegerLiteral& n);
                virtual void visit(const Nodecl::FloatingLiteral& n);

                virtual void visit(const Nodecl::Reference& n);
                virtual void visit(const Nodecl::Dereference& n);

                virtual void visit(const Nodecl::VectorLaneId& n);

                virtual void visit(const Nodecl::IntelAssume& n);
                virtual void visit(const Nodecl::IntelAssumeAligned& n);

                Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);

                void vectorize_basic_induction_variable(const Nodecl::Symbol& n);
        };
    }
}

#endif //TL_VECTORIZER_VISITOR_EXPRESSION_HPP
