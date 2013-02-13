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

#ifndef TL_VECTORIZER_VISITOR_STATEMENT_HPP
#define TL_VECTORIZER_VISITOR_STATEMENT_HPP

#include "tl-nodecl-visitor.hpp"
#include "tl-analysis-static-info.hpp"

namespace TL 
{ 
    namespace Vectorization
    {
        class VectorizerVisitorStatement : public Nodecl::NodeclVisitor<void>
        {
            private:
                const std::string _device;
                const unsigned int _vector_length;
                const unsigned int _unroll_factor;
                const TL::Type _target_type;

                const TL::Scope& _simd_inner_scope;

            public:
                VectorizerVisitorStatement(const std::string& device,
                        const unsigned int vector_length,
                        const unsigned int unroll_factor,
                        const TL::Type& target_type,
                        const TL::Scope& simd_inner_scope);

                virtual void visit(const Nodecl::Context& n);
                virtual void visit(const Nodecl::CompoundStatement& n);
                virtual void visit(const Nodecl::ForStatement& n);
                virtual void visit(const Nodecl::ExpressionStatement& n);
                virtual void visit(const Nodecl::ObjectInit& n);
                virtual void visit(const Nodecl::ReturnStatement& n);

                Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };
    }
}

#endif //TL_VECTORIZER_VISITOR_STATEMENT_HPP
