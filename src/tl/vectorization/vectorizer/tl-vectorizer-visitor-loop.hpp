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

#ifndef TL_VECTORIZER_VISITOR_FOR_HPP
#define TL_VECTORIZER_VISITOR_FOR_HPP

#include "tl-nodecl-visitor.hpp"

#include "tl-vectorizer-environment.hpp"

namespace TL
{
namespace Vectorization
{
    class VectorizerVisitorLoop : public Nodecl::NodeclVisitor<void>
    {
        private:
            VectorizerEnvironment& _environment;

        public:
            VectorizerVisitorLoop(VectorizerEnvironment& environment);

            virtual void visit(const Nodecl::ForStatement& for_statement);
            virtual void visit(const Nodecl::WhileStatement& while_statement);
 
            Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);
    };

    class VectorizerVisitorLoopHeader : public Nodecl::NodeclVisitor<void>
    {
        private:
            VectorizerEnvironment& _environment;

        public:
            VectorizerVisitorLoopHeader(VectorizerEnvironment& environment);

            virtual void visit(const Nodecl::LoopControl& loop_header);

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
            const VectorizerEnvironment& _environment;

            void visit_condition(const Nodecl::NodeclBase& condition);

        public:
            VectorizerVisitorLoopCond(const VectorizerEnvironment& environment);

            void visit(const Nodecl::Equal& node);
            void visit(const Nodecl::LowerThan& node);
            void visit(const Nodecl::LowerOrEqualThan& node);
            void visit(const Nodecl::GreaterThan& node);
            void visit(const Nodecl::GreaterOrEqualThan& node);

            Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);
    };

    class VectorizerVisitorLoopNext : public Nodecl::NodeclVisitor<void>
    {
        private:
            VectorizerEnvironment& _environment;

            void visit_increment(const Nodecl::NodeclBase& node,
                    const Nodecl::NodeclBase& lhs);

        public:
            VectorizerVisitorLoopNext(VectorizerEnvironment& environment);

            void visit(const Nodecl::Comma& node);
            void visit(const Nodecl::Assignment& node);

            Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& node);
    };

    class VectorizerVisitorLoopEpilog
    {
        private:
            VectorizerEnvironment& _environment;
            int _epilog_iterations;
            bool _only_epilog;
            bool _is_parallel_loop;

            void get_updated_iv_init_for_epilog(
                    const Nodecl::ForStatement& for_statement,
                    Nodecl::NodeclBase &induction_variable,
                    Nodecl::NodeclBase &iv_init);

        public:
            VectorizerVisitorLoopEpilog(VectorizerEnvironment& environment,
                    int epilog_iterations, bool only_epilog,
                    bool is_parallel_loop);

            void visit(const Nodecl::NodeclBase& loop_statement,
                    Nodecl::NodeclBase& net_epilog_node);

            void visit_scalar_epilog(const Nodecl::NodeclBase& loop_statement,
                    Nodecl::NodeclBase& net_epilog_node);
            
            void clean_up_epilog(Nodecl::NodeclBase& net_epilog_node);

            void visit_vector_epilog(const Nodecl::NodeclBase& loop_statement,
                    const Nodecl::NodeclBase& loop_cond,
                    Nodecl::NodeclBase& net_epilog_node);
    };
}
}

#endif //TL_VECTORIZER_VISITOR_FOR_HPP
