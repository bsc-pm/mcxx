/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#ifndef KNC_VECTOR_LOWERING_HPP
#define KNC_VECTOR_LOWERING_HPP

#include "tl-nodecl-base.hpp"
#include "tl-nodecl-visitor.hpp"
#include <list>

namespace TL
{
    namespace Vectorization
    {
        class KNCVectorLowering : public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                std::list<Nodecl::NodeclBase> _old_m512;

            public:

                KNCVectorLowering();

                void visit(const Nodecl::ObjectInit& node);
                
                void visit(const Nodecl::VectorAdd& node);
                void visit(const Nodecl::VectorAddMask& node);
                void visit(const Nodecl::VectorMinus& node);
                void visit(const Nodecl::VectorMinusMask& node);
                void visit(const Nodecl::VectorMul& node);
                void visit(const Nodecl::VectorMulMask& node);
                void visit(const Nodecl::VectorDiv& node);
                void visit(const Nodecl::VectorDivMask& node);
                void visit(const Nodecl::VectorNeg& node);

                void visit(const Nodecl::VectorLowerThan& node);
                void visit(const Nodecl::VectorGreaterThan& node);
                void visit(const Nodecl::VectorEqual& node);

                void visit(const Nodecl::VectorBitwiseAnd& node);
                void visit(const Nodecl::VectorBitwiseOr& node);
                void visit(const Nodecl::VectorBitwiseXor& node);
                void visit(const Nodecl::VectorLogicalOr& node);

                void visit(const Nodecl::VectorConversion& node);
                void visit(const Nodecl::VectorConditionalExpression& node);
                void visit(const Nodecl::VectorPromotion& node);
                void visit(const Nodecl::VectorLiteral& node);
                void visit(const Nodecl::VectorAssignment& node);
                void visit(const Nodecl::VectorAssignmentMask& node);
                void visit(const Nodecl::VectorLoad& node);
                void visit(const Nodecl::VectorLoadMask& node);
                //void visit(const Nodecl::UnalignedVectorLoad& node);
                //void visit(const Nodecl::UnalignedVectorLoadMask& node);
                void visit(const Nodecl::VectorStore& node);
                void visit(const Nodecl::VectorStoreMask& node);
                //void visit(const Nodecl::UnalignedVectorStore& node);
                //void visit(const Nodecl::UnalignedVectorStoreMask& node);
                void visit(const Nodecl::VectorGather& node);
                void visit(const Nodecl::VectorGatherMask& node);
                void visit(const Nodecl::VectorScatter& node);
                void visit(const Nodecl::VectorScatterMask& node);

                void visit(const Nodecl::VectorFunctionCall& node);
                void visit(const Nodecl::VectorFabs& node);

                virtual void visit(const Nodecl::ParenthesizedExpression& node);

                virtual void visit(const Nodecl::VectorMaskAssignment& node);
                virtual void visit(const Nodecl::VectorMaskAnd& node);
                virtual void visit(const Nodecl::VectorMaskNot& node);
                virtual void visit(const Nodecl::VectorMaskAnd1Not& node);
                virtual void visit(const Nodecl::VectorMaskAnd2Not& node);

                Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };
    }
}
#endif // KNC_VECTOR_LOWERING_HPP
