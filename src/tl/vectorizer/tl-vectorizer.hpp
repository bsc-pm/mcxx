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

#ifndef TL_VECTORIZER_HPP
#define TL_VECTORIZER_HPP

#include "tl-nodecl-base.hpp"
#include "tl-versioning.hpp"
#include "tl-vector-function-version.hpp"
#include "tl-vectorizer-visitor-expression.hpp"
#include <list>
#include <string>

namespace TL 
{ 
    namespace Vectorization
    {
        class Vectorizer
        {
            static TL::Versioning<std::string, Nodecl::NodeclBase> _vector_function_versioning;

            private:
                static Vectorizer* _vectorizer;

                Vectorizer();

            public:
                static Vectorizer& getVectorizer();

                Nodecl::NodeclBase vectorize(const Nodecl::ForStatement& for_statement, 
                        const std::string& device,
                        const unsigned int vector_length,
                        const TL::Type& target_type);
                void vectorize(const Nodecl::FunctionCode& func_code,
                        const std::string& device,
                        const unsigned int vector_length,
                        const TL::Type& target_type);

                void add_function_version(const std::string& func_name, const Nodecl::NodeclBase& version,
                        const std::string& device, const unsigned int vector_length, 
                        const TL::Type& target_type, const int priority );

                friend void VectorizerVisitorExpression::visit(const Nodecl::FunctionCall& n);
        };
    }
}

#endif // TL_VECTORIZER_HPP
