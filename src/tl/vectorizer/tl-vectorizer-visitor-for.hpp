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

#ifndef TL_VECTORIZER_VISITOR_FOR_HPP
#define TL_VECTORIZER_VISITOR_FOR_HPP

#include "tl-nodecl-visitor.hpp"

namespace TL
{
    namespace Vectorization
    {
        class VectorizerVisitorFor : public Nodecl::NodeclVisitor<int>
        {
            private:
                VectorizerEnvironment& _environment;

            public:
                VectorizerVisitorFor(VectorizerEnvironment& environment);
                
                virtual int join_list(ObjectList<int>& list);

                virtual int visit(const Nodecl::ForStatement& for_statement);

                Nodecl::NodeclVisitor<int>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };

        class VectorizerVisitorLoopHeader : public Nodecl::NodeclVisitor<int>
        {
            private:
                const VectorizerEnvironment& _environment;

            public:
                VectorizerVisitorLoopHeader(const VectorizerEnvironment& environment);

                int join_list(TL::ObjectList<int> &);

                virtual int visit(const Nodecl::LoopControl& loop_header);

                Nodecl::NodeclVisitor<int>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };

        class VectorizerVisitorLoopInit : public Nodecl::NodeclVisitor<Nodecl::NodeclBase>
        {
            public:
                VectorizerVisitorLoopInit(void);

                Nodecl::NodeclBase join_list(TL::ObjectList<Nodecl::NodeclBase> &);

                Nodecl::NodeclBase visit(const Nodecl::ObjectInit& node);
                Nodecl::NodeclBase visit(const Nodecl::Assignment& node);
                Nodecl::NodeclBase visit(const Nodecl::Comma& node);

                Nodecl::NodeclVisitor<Nodecl::NodeclBase>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };

        class VectorizerVisitorLoopCond : public Nodecl::NodeclVisitor<Nodecl::NodeclBase>
        {
            private:
                const VectorizerEnvironment& _environment;

            public:
                VectorizerVisitorLoopCond(const VectorizerEnvironment& environment);

                Nodecl::NodeclBase visit(const Nodecl::Equal& node);
                Nodecl::NodeclBase visit(const Nodecl::LowerThan& node);
                Nodecl::NodeclBase visit(const Nodecl::LowerOrEqualThan& node);
                Nodecl::NodeclBase visit(const Nodecl::GreaterThan& node);
                Nodecl::NodeclBase visit(const Nodecl::GreaterOrEqualThan& node);
                Nodecl::NodeclBase visit_condition(const Nodecl::NodeclBase& condition);

                Nodecl::NodeclVisitor<Nodecl::NodeclBase>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };

        class VectorizerVisitorLoopNext : public Nodecl::NodeclVisitor<Nodecl::NodeclBase>
        {
            private:
                const VectorizerEnvironment& _environment;

            public:
                VectorizerVisitorLoopNext(const VectorizerEnvironment& environment);

                Nodecl::NodeclBase visit(const Nodecl::Comma& node);
                Nodecl::NodeclBase visit(const Nodecl::Preincrement& node);
                Nodecl::NodeclBase visit(const Nodecl::Postincrement& node);
                Nodecl::NodeclBase visit(const Nodecl::AddAssignment& node);
                
                Nodecl::NodeclBase visit_increment(const Nodecl::NodeclBase& node,
                        const Nodecl::NodeclBase& lhs);

                Nodecl::NodeclVisitor<Nodecl::NodeclBase>::Ret unhandled_node(const Nodecl::NodeclBase& node);
        };

        class VectorizerVisitorForEpilog 
        {
            private:
                VectorizerEnvironment& _environment;
                int _epilog_iterations;

                void get_parallel_iv_init_for_epilog(const Nodecl::ForStatement& for_statement, 
                        Nodecl::NodeclBase &induction_variable,
                        Nodecl::NodeclBase &iv_init);

            public:
                VectorizerVisitorForEpilog(VectorizerEnvironment& environment,
                        int epilog_iterations);

                void visit(const Nodecl::ForStatement& for_statement,
                        Nodecl::NodeclBase& net_epilog_node);
                
                void visit_scalar_epilog(const Nodecl::ForStatement& for_statement,
                        Nodecl::NodeclBase& net_epilog_node);
                void visit_vector_epilog(const Nodecl::ForStatement& for_statement,
                        Nodecl::NodeclBase& net_epilog_node);
        };


    }
}

#endif //TL_VECTORIZER_VISITOR_FOR_HPP
