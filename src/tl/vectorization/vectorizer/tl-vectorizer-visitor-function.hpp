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

#ifndef TL_VECTORIZER_VISITOR_FUNCTION_HPP
#define TL_VECTORIZER_VISITOR_FUNCTION_HPP

#include "tl-nodecl-visitor.hpp"

#include "tl-vectorizer-environment.hpp"

namespace TL
{
    namespace Vectorization
    {
        class VectorizerVisitorFunction : public Nodecl::NodeclVisitor<void>
        {
            private:
                VectorizerEnvironment& _environment;
                const bool _masked_version;

            public:
                VectorizerVisitorFunction(VectorizerEnvironment& environment,
                        const bool masked_version);

                virtual void visit(const Nodecl::FunctionCode& function_code);

                Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };

        class VectorizerVisitorFunctionHeader : public Nodecl::NodeclVisitor<void>
        {
            private:
                VectorizerEnvironment& _environment;
                const bool _masked_version;
                TL::ObjectList<TL::Symbol> _uniform_symbols;
                std::map<TL::Symbol, int> _linear_symbols;

            public:
                VectorizerVisitorFunctionHeader(VectorizerEnvironment& environment,
                        const TL::ObjectList<TL::Symbol> &uniform_symbols,
                        const std::map<TL::Symbol, int> &linear_symbols,
                        const bool masked_version);

                virtual void visit(const Nodecl::FunctionCode& function_code);

                Nodecl::NodeclVisitor<void>::Ret unhandled_node(const Nodecl::NodeclBase& n);
        };
    }
}

#endif //TL_VECTORIZER_VISITOR_FUNCTION_HPP
