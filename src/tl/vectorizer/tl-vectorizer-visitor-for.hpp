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

#ifndef TL_VECTORIZER_VISITOR_FOR_HPP
#define TL_VECTORIZER_VISITOR_FOR_HPP

#include "tl-nodecl-visitor.hpp"

namespace TL 
{ 
    namespace Vectorizer
    {
        class VectorizerVisitorFor : public Nodecl::NodeclVisitor<Nodecl::NodeclBase>
        {
            private:
                const unsigned int _vector_length;
                int _remain_iterations;

                void analyze_loop(const Nodecl::ForStatement& for_statement);
                Nodecl::ForStatement get_epilog(const Nodecl::ForStatement& for_statement);

            public:
                VectorizerVisitorFor(const unsigned int vector_length);

                virtual Nodecl::NodeclBase visit(const Nodecl::ForStatement& for_statement);

                NodeclVisitor<Nodecl::NodeclBase>::Ret unhandled_node(const Nodecl::NodeclBase& n); 
        };

        class VectorizerVisitorLoopHeader : public Nodecl::NodeclVisitor<void>
        {
            private:
                const unsigned int _vector_length;
 
            public:
                VectorizerVisitorLoopHeader(const unsigned int vector_length);

                void visit(const Nodecl::NodeclBase& loop_header);

                NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n); 
        };

        class VectorizerVisitorLoopInit : public Nodecl::NodeclVisitor<void>
        {
            public:
                VectorizerVisitorLoopInit();

                NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n); 
        };
 
        class VectorizerVisitorLoopCond : public Nodecl::NodeclVisitor<void>
        {
            private:
                const unsigned int _vector_length;
 
            public:
                VectorizerVisitorLoopCond(const unsigned int vector_length);

                NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n); 
        };
 
        class VectorizerVisitorLoopNext : public Nodecl::NodeclVisitor<void>
        {
            private:
                const unsigned int _vector_length;
 
            public:
                VectorizerVisitorLoopNext(const unsigned int vector_length);

                NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n); 
        };
    }
}

#endif //TL_VECTORIZER_VISITOR_FOR_HPP
