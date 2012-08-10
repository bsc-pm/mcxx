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

#include "tl-vectorizer.hpp"
#include "tl-vectorizer-visitor-for.hpp"
#include "tl-vectorizer-visitor-function.hpp"
#include "tl-source.hpp"

namespace TL 
{
    namespace Vectorization
    {
        Vectorizer* Vectorizer::_vectorizer = 0;
        TL::Versioning<std::string, Nodecl::NodeclBase> _vector_function_versioning;

        Vectorizer& Vectorizer::getVectorizer()
        {
            if(_vectorizer == 0)
                _vectorizer = new Vectorizer();

            return *_vectorizer;
        }

        Vectorizer::Vectorizer() 
        {
            // SVML SSE
//            if (svml_enable)
            {
                TL::Source svml_sse_vector_math;

                svml_sse_vector_math << "__m128 __svml_expf4(__m128);\n"
                    << "__m128 __svml_sinf4(__m128);\n"
                    ;

                // Parse SVML declarations
                TL::Scope global_scope = TL::Scope(CURRENT_COMPILED_FILE->global_decl_context);
                svml_sse_vector_math.parse_global(global_scope);

                // Add SVML math function as vector version of the scalar one
/*                _vector_function_versioning.add_version("expf", 
                        new VectorFunctionVersion(
                            global_scope.get_symbol_from_name("__svml_expf4").make_nodecl(),
                            "smp", 16, NULL, DEFAULT_FUNC_PRIORITY));
                _vector_function_versioning.add_version("sinf",
                        new VectorFunctionVersion( 
                            global_scope.get_symbol_from_name("__svml_sinf4").make_nodecl(),
                            "smp", 16, NULL, DEFAULT_FUNC_PRIORITY));
*/            }
        }

        Nodecl::NodeclBase Vectorizer::vectorize(const Nodecl::ForStatement& for_statement,
                const std::string& device,
                const unsigned int vector_length,
                const TL::Type& target_type)
        {
            VectorizerVisitorFor visitor_for(device, vector_length, target_type);

            return visitor_for.walk(for_statement);
        }

        void Vectorizer::vectorize(const Nodecl::FunctionCode& func_code,
                const std::string& device,
                const unsigned int vector_length,
                const TL::Type& target_type)
        {
            VectorizerVisitorFunction visitor_function(device, vector_length, target_type);
            visitor_function.walk(func_code);
        }
    } 
}



