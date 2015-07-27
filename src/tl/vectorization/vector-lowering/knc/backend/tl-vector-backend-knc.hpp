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

#ifndef KNC_VECTOR_BACKEND_HPP
#define KNC_VECTOR_BACKEND_HPP

#include "tl-vector-backend-knc-extern.hpp"

#include "tl-vectorizer.hpp"
#include "tl-nodecl-base.hpp"
#include "tl-nodecl-visitor.hpp"

#include <list>

namespace TL
{
    namespace Vectorization
    {
        class KNCConfigMaskProcessing
        {
            public:
                enum config_mask_processing_t
                {
                    MASK_DEFAULT = 0x0,
                    ONLY_MASK = 0x1,        // Output string does not contain old_value
                    KEEP_OLD = 0x2,         // Pop the current old_value in the stack
                    ALWAYS_OLD = 0x4,       // Return an old_value even when the mask is null
                    NO_FINAL_COMMA = 0x8,   // Do not write a final ',' in the string 'mask_params'
                } config_mask_processing;

                KNCConfigMaskProcessing(int a)
                {
                    config_mask_processing = config_mask_processing_t(a);
                }

                config_mask_processing_t operator |(config_mask_processing_t b)
                {
                    return config_mask_processing_t(((int)this->config_mask_processing) | ((int)b));
                }

                config_mask_processing_t operator &(config_mask_processing_t b)
                {
                    return config_mask_processing_t(((int)this->config_mask_processing) & ((int)b));
                }
        };

        class KNCVectorBackend : public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                TL::Vectorization::Vectorizer& _vectorizer;
                const unsigned int _vector_length;
                std::list<Nodecl::NodeclBase> _old_m512;

                void common_binary_op_lowering(const Nodecl::NodeclBase& node,
                        const std::string& intrin_op_name);
                void common_unary_op_lowering(const Nodecl::NodeclBase& node,
                        const std::string& intrin_op_name);
                void bitwise_binary_op_lowering(const Nodecl::NodeclBase& node,
                        const std::string& intrin_op_name);
                virtual void common_comparison_op_lowering(
                        const Nodecl::NodeclBase& node,
                        const int float_cmp_flavor,
                        const std::string int_cmp_flavor);

                std::string get_casting_intrinsic(const TL::Type& type_from,
                        const TL::Type& type_to);
                std::string get_undef_intrinsic(const TL::Type& type);

                void visit_aligned_vector_load(
                        const Nodecl::VectorLoad& node);
                void visit_unaligned_vector_load(
                        const Nodecl::VectorLoad& node);
                void visit_aligned_vector_store(
                        const Nodecl::VectorStore& node,
                        const int hint);
                 void visit_aligned_vector_stream_store(
                        const Nodecl::VectorStore& node);
                void visit_unaligned_vector_store(
                        const Nodecl::VectorStore& node,
                        const int hint);
            protected:
                void process_mask_component(const Nodecl::NodeclBase& mask,
                        TL::Source& mask_prefix, TL::Source& mask_params,
                        const TL::Type& type,
                        KNCConfigMaskProcessing conf = KNCConfigMaskProcessing::MASK_DEFAULT );
                std::string get_casting_to_scalar_pointer(const TL::Type& type_to);

            public:

                KNCVectorBackend();

                virtual void visit(const Nodecl::FunctionCode& n);
                virtual void visit(const Nodecl::ObjectInit& n);

                virtual void visit(const Nodecl::VectorAdd& n);
                virtual void visit(const Nodecl::VectorMinus& n);
                virtual void visit(const Nodecl::VectorMul& n);
                virtual void visit(const Nodecl::VectorDiv& n);
                virtual void visit(const Nodecl::VectorRcp& n);
                virtual void visit(const Nodecl::VectorMod& n);
                virtual void visit(const Nodecl::VectorSqrt& n);
                virtual void visit(const Nodecl::VectorRsqrt& n);

                virtual void visit(const Nodecl::VectorFmadd& n);

                virtual void visit(const Nodecl::VectorNeg& n);

                virtual void visit(const Nodecl::VectorLowerThan& n);
                virtual void visit(const Nodecl::VectorLowerOrEqualThan& n);
                virtual void visit(const Nodecl::VectorGreaterThan& n);
                virtual void visit(const Nodecl::VectorGreaterOrEqualThan& n);
                virtual void visit(const Nodecl::VectorEqual& n);
                virtual void visit(const Nodecl::VectorDifferent& n);

                virtual void visit(const Nodecl::VectorBitwiseAnd& n);
                virtual void visit(const Nodecl::VectorBitwiseOr& n);
                virtual void visit(const Nodecl::VectorBitwiseXor& n);
                virtual void visit(const Nodecl::VectorLogicalOr& n);
                virtual void visit(const Nodecl::VectorBitwiseShl& n);
                virtual void visit(const Nodecl::VectorArithmeticShr& n);
                virtual void visit(const Nodecl::VectorBitwiseShr& n);
                virtual void visit(const Nodecl::VectorAlignRight& n);

                virtual void visit(const Nodecl::VectorConversion& n);
                virtual void visit(const Nodecl::VectorCast& n);
                virtual void visit(const Nodecl::VectorConditionalExpression& n);
                virtual void visit(const Nodecl::VectorPromotion& n);
                virtual void visit(const Nodecl::VectorLiteral& n);
                virtual void visit(const Nodecl::VectorAssignment& n);
                virtual void visit(const Nodecl::VectorPrefetch& n);
                virtual void visit(const Nodecl::VectorLoad& n);
                virtual void visit(const Nodecl::VectorStore& n);
                virtual void visit(const Nodecl::VectorGather& n);
                virtual void visit(const Nodecl::VectorScatter& n);

                virtual void visit(const Nodecl::VectorFunctionCall& n);
                virtual void visit(const Nodecl::VectorFabs& n);
                virtual void visit(const Nodecl::VectorSincos& n);

                virtual void visit(const Nodecl::ParenthesizedExpression& n);

                virtual void visit(const Nodecl::VectorReductionAdd& n);
                virtual void visit(const Nodecl::VectorReductionMinus& n);

                virtual void visit(const Nodecl::VectorMaskAssignment& n);
                virtual void visit(const Nodecl::VectorMaskConversion& n);
                virtual void visit(const Nodecl::VectorMaskOr& n);
                virtual void visit(const Nodecl::VectorMaskAnd& n);
                virtual void visit(const Nodecl::VectorMaskNot& n);
                virtual void visit(const Nodecl::VectorMaskAnd1Not& n);
                virtual void visit(const Nodecl::VectorMaskAnd2Not& n);
                virtual void visit(const Nodecl::VectorMaskXor& n);

                virtual void visit(const Nodecl::MaskLiteral& n);

                virtual Nodecl::ExhaustiveVisitor<void>::Ret unhandled_node(
                        const Nodecl::NodeclBase& n);
        };
    }
}

#endif // KNC_VECTOR_BACKEND_HPP
