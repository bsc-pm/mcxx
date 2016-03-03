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

#ifndef AVX2_VECTOR_LOWERING_HPP
#define AVX2_VECTOR_LOWERING_HPP

#include "tl-vectorizer.hpp"
#include "tl-nodecl-base.hpp"
#include "tl-nodecl-visitor.hpp"
#include <list>

namespace TL
{
    namespace Vectorization
    {
        class AVX2ConfigMaskProcessing
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

                AVX2ConfigMaskProcessing(int a)
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

        class AVX2VectorLowering : public Nodecl::ExhaustiveVisitor<void>
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

                std::string get_casting_intrinsic(const TL::Type& type_from,
                        const TL::Type& type_to,
                        const locus_t* locus);
                std::string get_casting_to_scalar_pointer(const TL::Type& type_to);

                void visit_aligned_vector_load(const Nodecl::VectorLoad& node);
                void visit_unaligned_vector_load(const Nodecl::VectorLoad& node);
                void visit_aligned_vector_store(const Nodecl::VectorStore& node);
                void visit_unaligned_vector_store(const Nodecl::VectorStore& node);

                void visit_reduction_add_4bytes_elements(const Nodecl::VectorReductionAdd& node);
                void visit_reduction_add_8bytes_elements(const Nodecl::VectorReductionAdd& node);


            public:

                AVX2VectorLowering();

                virtual void visit(const Nodecl::ObjectInit& node);

                virtual void visit(const Nodecl::VectorAdd& node);
                virtual void visit(const Nodecl::VectorMinus& node);
                virtual void visit(const Nodecl::VectorMul& node);
                virtual void visit(const Nodecl::VectorDiv& node);
                virtual void visit(const Nodecl::VectorMod& node);
                virtual void visit(const Nodecl::VectorSqrt& node);
                virtual void visit(const Nodecl::VectorRsqrt& node);

                virtual void visit(const Nodecl::VectorFmadd& node);

                virtual void visit(const Nodecl::VectorNeg& node);

                virtual void visit(const Nodecl::VectorLowerThan& node);
                virtual void visit(const Nodecl::VectorLowerOrEqualThan& node);
                virtual void visit(const Nodecl::VectorGreaterThan& node);
                virtual void visit(const Nodecl::VectorGreaterOrEqualThan& node);
                virtual void visit(const Nodecl::VectorEqual& node);
                virtual void visit(const Nodecl::VectorDifferent& node);

                virtual void visit(const Nodecl::VectorBitwiseAnd& node);
                virtual void visit(const Nodecl::VectorBitwiseOr& node);
                virtual void visit(const Nodecl::VectorBitwiseXor& node);
                virtual void visit(const Nodecl::VectorLogicalOr& node);
                virtual void visit(const Nodecl::VectorBitwiseShl& node);
                virtual void visit(const Nodecl::VectorArithmeticShr& node);
                virtual void visit(const Nodecl::VectorBitwiseShr& node);
                virtual void visit(const Nodecl::VectorAlignRight& node);

                virtual void visit(const Nodecl::VectorConversion& node);
                virtual void visit(const Nodecl::VectorCast& node);
                virtual void visit(const Nodecl::VectorConditionalExpression& node);
                virtual void visit(const Nodecl::VectorPromotion& node);
                virtual void visit(const Nodecl::VectorLiteral& node);
                virtual void visit(const Nodecl::VectorAssignment& node);
                virtual void visit(const Nodecl::VectorLoad& node);
                virtual void visit(const Nodecl::VectorStore& node);
                virtual void visit(const Nodecl::VectorGather& node);
                virtual void visit(const Nodecl::VectorScatter& node);

                virtual void visit(const Nodecl::VectorFunctionCall& node);
                virtual void visit(const Nodecl::VectorFabs& node);
                virtual void visit(const Nodecl::VectorSincos& node);

                virtual void visit(const Nodecl::ParenthesizedExpression& node);

                virtual void visit(const Nodecl::VectorReductionAdd& node);
                virtual void visit(const Nodecl::VectorReductionMinus& node);

                virtual void visit(const Nodecl::VectorMaskAssignment& node);
                virtual void visit(const Nodecl::VectorMaskConversion& node);
                virtual void visit(const Nodecl::VectorMaskOr& node);
                virtual void visit(const Nodecl::VectorMaskAnd& node);
                virtual void visit(const Nodecl::VectorMaskNot& node);
                virtual void visit(const Nodecl::VectorMaskAnd1Not& node);
                virtual void visit(const Nodecl::VectorMaskAnd2Not& node);
                virtual void visit(const Nodecl::VectorMaskXor& node);

                virtual void visit(const Nodecl::MaskLiteral& node);
        };
    }
}

#endif // AVX2_VECTOR_LOWERING_HPP
