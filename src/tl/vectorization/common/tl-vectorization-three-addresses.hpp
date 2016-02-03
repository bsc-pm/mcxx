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

#ifndef TL_VECTORIZATION_THREE_ADDRESSES_HPP 
#define TL_VECTORIZATION_THREE_ADDRESSES_HPP 

#include "tl-nodecl-visitor.hpp" 

namespace TL
{ 
namespace Vectorization
{ 
    class VectorizationThreeAddresses : public Nodecl::ExhaustiveVisitor<void> 
    {
        private:
            static unsigned long long int sym_counter;
            Nodecl::NodeclBase _object_init;
            
            TL::Symbol get_temporal_symbol(const Nodecl::NodeclBase& reference);

            void decomp(const Nodecl::NodeclBase& n);
            void visit_vector_unary(const Nodecl::NodeclBase& n);
            void visit_vector_binary(const Nodecl::NodeclBase& n);
            void visit_vector_ternary(const Nodecl::NodeclBase& n);
 
        public:
            VectorizationThreeAddresses();

            void visit(const Nodecl::Comma& n);
            void visit(const Nodecl::ObjectInit& n);

            void visit(const Nodecl::VectorAdd& n);
            void visit(const Nodecl::VectorMinus& n);
            void visit(const Nodecl::VectorMul& n);
            void visit(const Nodecl::VectorDiv& n);
            void visit(const Nodecl::VectorMod& n);
            void visit(const Nodecl::VectorNeg& n);
            void visit(const Nodecl::VectorSqrt& n);
            void visit(const Nodecl::VectorRsqrt& n);
            void visit(const Nodecl::VectorFabs& n);
            void visit(const Nodecl::VectorSincos& n);
            void visit(const Nodecl::VectorFunctionCall& n);
            void visit(const Nodecl::VectorAlignRight& n);
            void visit(const Nodecl::VectorLoad& n);
            void visit(const Nodecl::VectorGather& n);
            void visit(const Nodecl::VectorStore& n);
            void visit(const Nodecl::VectorScatter& n);
            void visit(const Nodecl::VectorFmadd& n);
            void visit(const Nodecl::VectorFmminus& n);
            void visit(const Nodecl::VectorArithmeticShr& n);
            void visit(const Nodecl::VectorBitwiseShr& n);
            void visit(const Nodecl::VectorBitwiseShl& n);
            void visit(const Nodecl::VectorReductionAdd& n);
            void visit(const Nodecl::VectorReductionMinus& n);
            void visit(const Nodecl::VectorReductionMul& n);
            void visit(const Nodecl::VectorEqual& n);
            void visit(const Nodecl::VectorDifferent& n);
            void visit(const Nodecl::VectorLowerThan& n);
            void visit(const Nodecl::VectorLowerOrEqualThan& n);
            void visit(const Nodecl::VectorGreaterThan& n);
            void visit(const Nodecl::VectorGreaterOrEqualThan& n);
            void visit(const Nodecl::VectorLogicalNot& n);
            void visit(const Nodecl::VectorLogicalAnd& n);
            void visit(const Nodecl::VectorLogicalOr& n);
            void visit(const Nodecl::VectorBitwiseNot& n);
            void visit(const Nodecl::VectorBitwiseAnd& n);
            void visit(const Nodecl::VectorBitwiseOr& n);
            void visit(const Nodecl::VectorBitwiseXor& n);
           
            void visit(const Nodecl::VectorMaskNot& n);
            void visit(const Nodecl::VectorMaskAnd& n);
            void visit(const Nodecl::VectorMaskOr& n);
            void visit(const Nodecl::VectorMaskXor& n);
            void visit(const Nodecl::VectorMaskAnd1Not& n);
            void visit(const Nodecl::VectorMaskAnd2Not& n);

            void visit(const Nodecl::VectorConversion& n);
            void visit(const Nodecl::VectorConditionalExpression& n);
            void visit(const Nodecl::WhileStatement& n);
            void visit(const Nodecl::LoopControl& n);

        private:
            void visit_expression(const Nodecl::NodeclBase &n);
    };
}
}
#endif //TL_VECTORIZATIONIZATION_VISITOR_POSTPROCESSOR_HPP
