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
    namespace Vectorization
    {
        class VectorizerVisitorFor : public Nodecl::NodeclVisitor<Nodecl::NodeclBase>
        {
            private:
                const std::string _device;
                const unsigned int _vector_length;
                const TL::Type _target_type;

                unsigned int _remain_iterations;
                unsigned int _unroll_factor;

                void analyze_loop(const Nodecl::ForStatement& for_statement);
                Nodecl::ForStatement get_epilog(const Nodecl::ForStatement& for_statement);

            public:
                VectorizerVisitorFor(const std::string device,
                        const unsigned int vector_length,
                        const TL::Type& target_type);

                virtual Nodecl::NodeclBase visit(const Nodecl::ForStatement& for_statement);

                Nodecl::NodeclVisitor<Nodecl::NodeclBase>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };

        class VectorizerVisitorLoopHeader : public Nodecl::NodeclVisitor<void>
        {
            private:
                const unsigned int _unroll_factor;

            public:
                VectorizerVisitorLoopHeader(const unsigned int unroll_factor);

                void visit(const Nodecl::LoopControl& loop_header);

                Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };

        class VectorizerVisitorLoopInit : public Nodecl::NodeclVisitor<void>
        {
            public:
                VectorizerVisitorLoopInit(void);

                void visit(const Nodecl::ObjectInit& node);
                void visit(const Nodecl::Assignment& node);
                void visit(const Nodecl::Comma& node);

                Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };

        class VectorizerVisitorLoopCond : public Nodecl::NodeclVisitor<void>
        {
            private:
                const unsigned int _unroll_factor;

            public:
                VectorizerVisitorLoopCond(const unsigned int unroll_factor);

                void visit(const Nodecl::Equal& node);
                void visit(const Nodecl::LowerThan& node);
                void visit(const Nodecl::LowerOrEqualThan& node);
                void visit(const Nodecl::GreaterThan& node);
                void visit(const Nodecl::GreaterOrEqualThan& node);
                void visit_condition(const Nodecl::NodeclBase& condition);

                Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };

        class VectorizerVisitorLoopNext : public Nodecl::NodeclVisitor<void>
        {
            private:
                const unsigned int _unroll_factor;

            public:
                VectorizerVisitorLoopNext(const unsigned int unroll_factor);

                void visit(const Nodecl::Comma& node);
                void visit(const Nodecl::Preincrement& node);
                void visit(const Nodecl::Postincrement& node);
                void visit(const Nodecl::AddAssignment& node);

                Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };
    }
}

#endif //TL_VECTORIZER_VISITOR_FOR_HPP
