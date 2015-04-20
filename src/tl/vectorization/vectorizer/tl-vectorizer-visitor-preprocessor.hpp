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

#ifndef TL_VECTORIZER_VISITOR_PREPROCESSOR_HPP 
#define TL_VECTORIZER_VISITOR_PREPROCESSOR_HPP

#include "tl-nodecl-visitor.hpp" 
#include "tl-vectorizer.hpp"

namespace TL
{ 
namespace Vectorization
{ 
    class VectorizerVisitorPreprocessor : public Nodecl::ExhaustiveVisitor<void> 
    {
        private:
//            const VectorizerEnvironment& _environment;

            void visit_pre_post_increment(const Nodecl::Preincrement& n);
            void visit_pre_post_decrement(const Nodecl::Predecrement& n);

            template <typename OpAssignment, typename Op> 
                void visit_op_assignment(const OpAssignment& n);

        public:
            VectorizerVisitorPreprocessor();
 //                   const VectorizerEnvironment& environment);

            void visit(const Nodecl::ObjectInit& n);
            void visit(const Nodecl::ArraySubscript& n);

            void visit(const Nodecl::AddAssignment& n);
            void visit(const Nodecl::MinusAssignment& n);
            void visit(const Nodecl::MulAssignment& n);
            void visit(const Nodecl::DivAssignment& n);
            void visit(const Nodecl::ModAssignment& n);

            void visit(const Nodecl::Preincrement& n);
            void visit(const Nodecl::Postincrement& n);
            void visit(const Nodecl::Predecrement& n);
            void visit(const Nodecl::Postdecrement& n);

            void visit(const Nodecl::ForStatement& n);
    };
}
}

#endif //TL_VECTORIZER_VISITOR_PREPROCESSOR_HPP
