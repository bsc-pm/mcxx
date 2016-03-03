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

#ifndef AVX2_VECTOR_LEGALIZATION_HPP
#define AVX2_VECTOR_LEGALIZATION_HPP

#include "tl-nodecl-base.hpp"
#include "tl-nodecl-visitor.hpp"
#include <list>

namespace TL
{
    namespace Vectorization
    {
        class AVX2VectorLegalization : public Nodecl::ExhaustiveVisitor<void>
        {
            public:

                AVX2VectorLegalization();

                virtual void visit(const Nodecl::Symbol& node);
                virtual void visit(const Nodecl::ObjectInit& node);
 
                virtual void visit(const Nodecl::VectorConversion& node);

                virtual void visit(const Nodecl::VectorGather& node);
                virtual void visit(const Nodecl::VectorScatter& node);

                virtual void visit(const Nodecl::VectorMaskAssignment& n);
                virtual void visit(const Nodecl::VectorLowerThan &node);
                virtual void visit(const Nodecl::VectorLowerOrEqualThan &node);
                virtual void visit(const Nodecl::VectorGreaterThan &node);
                virtual void visit(const Nodecl::VectorGreaterOrEqualThan &node);
                virtual void visit(const Nodecl::VectorEqual &node);
                virtual void visit(const Nodecl::VectorDifferent &node);
                virtual void visit(const Nodecl::VectorMaskOr &node);
                virtual void visit(const Nodecl::VectorMaskAnd &node);
                virtual void visit(const Nodecl::VectorMaskAnd1Not &node);
                virtual void visit(const Nodecl::VectorMaskAnd2Not &node);
                virtual void visit(const Nodecl::VectorMaskXor &node);

                virtual void visit(const Nodecl::VectorMaskNot& n);
                virtual void visit(const Nodecl::VectorMaskConversion& n);
        };

        class AVX2StrideVisitorConv : public Nodecl::NodeclVisitor<void>
        {
            private:
                unsigned int _vector_num_elements;

            public:
                AVX2StrideVisitorConv(unsigned int vector_num_elements);
        //        virtual void visit(const Nodecl::VectorConversion& node);

                Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);

        };
    }
}

#endif // AVX2_VECTOR_LEGALIZATION_HPP
