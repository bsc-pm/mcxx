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

#ifndef ROMOL_VECTOR_BACKEND_HPP
#define ROMOL_VECTOR_BACKEND_HPP

#include "tl-vectorizer.hpp"
#include "tl-nodecl-base.hpp"
#include "tl-nodecl-visitor.hpp"

#include <stack>

namespace TL
{
    namespace Vectorization
    {

        class RomolVectorBackend : public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                TL::Vectorization::Vectorizer& _vectorizer;

                Nodecl::NodeclBase current_assig;

                template <typename Node>
                void visit_elementwise_binary_expression(const Node& n, const std::string& name);

                template <typename Node>
                void visit_elementwise_unary_expression(const Node& n, const std::string& name);

                template <typename Node>
                void visit_relational_expression(const Node& n, const std::string& name);

                template <typename Node>
                void visit_mask_binary_expression(const Node& n, const std::string& name);

                template <typename Node>
                void visit_mask_unary_expression(const Node& n, const std::string& name);

                void emit_mask_is_nonzero(Nodecl::NodeclBase n, Nodecl::NodeclBase mask_tmp);
                void emit_mask_is_zero(Nodecl::NodeclBase n, Nodecl::NodeclBase mask_tmp);

                template <typename Node>
                void emit_mask_comparison(const Node& n,
                        void (RomolVectorBackend::* emit_cmp_fun)(Nodecl::NodeclBase, Nodecl::NodeclBase));

                bool does_not_have_side_effects(const Nodecl::NodeclBase& n);

                bool contains_vector_nodes(Nodecl::NodeclBase n);

                typedef std::map<const_value_t*, TL::Symbol> vector_literal_pool_t;
                std::map<const_value_t*, TL::Symbol> vector_literal_pool;
            public:
                RomolVectorBackend();

                // Non vector-specific
                virtual void visit(const Nodecl::FunctionCode& n);
                virtual void visit(const Nodecl::Assignment& n);
                virtual void visit(const Nodecl::ObjectInit& n);
                virtual void visit(const Nodecl::Different& n);
                virtual void visit(const Nodecl::Equal& n);
                virtual void visit(const Nodecl::Symbol& n);

                // Vector specific
                virtual void visit(const Nodecl::VectorAdd& n);
                virtual void visit(const Nodecl::VectorAlignRight& n);
                virtual void visit(const Nodecl::VectorArithmeticShr& n);
                virtual void visit(const Nodecl::VectorAssignment& n);
                virtual void visit(const Nodecl::VectorBitwiseAnd& n);
                virtual void visit(const Nodecl::VectorBitwiseOr& n);
                virtual void visit(const Nodecl::VectorBitwiseShl& n);
                virtual void visit(const Nodecl::VectorBitwiseShr& n);
                virtual void visit(const Nodecl::VectorBitwiseXor& n);
                virtual void visit(const Nodecl::VectorCast& n);
                virtual void visit(const Nodecl::VectorConditionalExpression& n);
                virtual void visit(const Nodecl::VectorConversion& n);
                virtual void visit(const Nodecl::VectorDifferent& n);
                virtual void visit(const Nodecl::VectorDiv& n);
                virtual void visit(const Nodecl::VectorEqual& n);
                virtual void visit(const Nodecl::VectorFabs& n);
                virtual void visit(const Nodecl::VectorFmadd& n);
                virtual void visit(const Nodecl::VectorFunctionCall& n);
                virtual void visit(const Nodecl::VectorGather& n);
                virtual void visit(const Nodecl::VectorGreaterOrEqualThan& n);
                virtual void visit(const Nodecl::VectorGreaterThan& n);
                virtual void visit(const Nodecl::VectorLiteral& n);
                virtual void visit(const Nodecl::VectorLoad& n);
                virtual void visit(const Nodecl::VectorLogicalOr& n);
                virtual void visit(const Nodecl::VectorLowerOrEqualThan& n);
                virtual void visit(const Nodecl::VectorLowerThan& n);
                virtual void visit(const Nodecl::VectorMinus& n);
                virtual void visit(const Nodecl::VectorMod& n);
                virtual void visit(const Nodecl::VectorMul& n);
                virtual void visit(const Nodecl::VectorNeg& n);
                // virtual void visit(const Nodecl::VectorPrefetch& n);
                virtual void visit(const Nodecl::VectorPromotion& n);
                virtual void visit(const Nodecl::VectorRcp& n);
                virtual void visit(const Nodecl::VectorRsqrt& n);
                virtual void visit(const Nodecl::VectorScatter& n);
                virtual void visit(const Nodecl::VectorSqrt& n);
                virtual void visit(const Nodecl::VectorStore& n);
                // virtual void visit(const Nodecl::VectorSincos& n);
                // virtual void visit(const Nodecl::ParenthesizedExpression& n);
                virtual void visit(const Nodecl::VectorReductionAdd& n);
                virtual void visit(const Nodecl::VectorReductionMinus& n);
                virtual void visit(const Nodecl::VectorMaskAssignment& n);
                // virtual void visit(const Nodecl::VectorMaskConversion& n);
                virtual void visit(const Nodecl::VectorMaskOr& n);
                virtual void visit(const Nodecl::VectorMaskAnd& n);
                virtual void visit(const Nodecl::VectorMaskNot& n);
                virtual void visit(const Nodecl::VectorMaskXor& n);
                // virtual void visit(const Nodecl::VectorMaskAnd1Not& n);
                // virtual void visit(const Nodecl::VectorMaskAnd2Not& n);

                virtual void visit(const Nodecl::MaskLiteral& n);
        };
    }
}

#endif // ROMOL_VECTOR_BACKEND_HPP
