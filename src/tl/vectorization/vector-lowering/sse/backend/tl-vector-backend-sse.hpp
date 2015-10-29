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

#ifndef SSE_VECTOR_BACKEND_HPP
#define SSE_VECTOR_BACKEND_HPP

#include "tl-nodecl-base.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL
{
    namespace Vectorization
    {
        class SSEVectorBackend : public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                std::string get_casting_intrinsic(const TL::Type& type_from,
                        const TL::Type& type_to,
                        const locus_t* locus);
                
                void visit_aligned_vector_load(
                        const Nodecl::VectorLoad& node);
                void visit_unaligned_vector_load(
                        const Nodecl::VectorLoad& node);
                void visit_aligned_vector_store(
                        const Nodecl::VectorStore& node);
                void visit_unaligned_vector_store(
                        const Nodecl::VectorStore& node);

            public:

                SSEVectorBackend();

                virtual void visit(const Nodecl::ObjectInit& node);
                
                virtual void visit(const Nodecl::VectorAdd& node);
                virtual void visit(const Nodecl::VectorMinus& node);
                virtual void visit(const Nodecl::VectorMul& node);
                virtual void visit(const Nodecl::VectorDiv& node);
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
                virtual void visit(const Nodecl::VectorAlignRight& node);
                virtual void visit(const Nodecl::VectorLogicalOr& node);


                virtual void visit(const Nodecl::VectorConversion& node);
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
                
                virtual void visit(const Nodecl::ParenthesizedExpression& node);

                virtual void visit(const Nodecl::VectorReductionAdd& node);
                virtual void visit(const Nodecl::VectorReductionMinus& node);

                virtual void visit(const Nodecl::VectorMaskAssignment& node);
                virtual void visit(const Nodecl::VectorMaskNot& node);
                virtual void visit(const Nodecl::VectorMaskConversion& node);
                virtual void visit(const Nodecl::VectorMaskAnd& node);
                virtual void visit(const Nodecl::VectorMaskOr& node);
                virtual void visit(const Nodecl::VectorMaskAnd1Not& node);
                virtual void visit(const Nodecl::VectorMaskAnd2Not& node);
                virtual void visit(const Nodecl::VectorMaskXor& node);
                virtual void visit(const Nodecl::MaskLiteral& node);

                virtual void visit(const Nodecl::VectorSqrt& node);

                virtual void visit(const Nodecl::VectorRcp& node);
                virtual void visit(const Nodecl::VectorRsqrt& node);
        };
    }
}
#endif // SSE_VECTOR_BACKEND_HPP
