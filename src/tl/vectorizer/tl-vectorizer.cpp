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
        Vectorizer *Vectorizer::_vectorizer = 0;
        
        Analysis::AnalysisStaticInfo *Vectorizer::_analysis_info = 0;
        std::list<Nodecl::NodeclBase> *Vectorizer::_analysis_scopes = 0;
        
        FunctionVersioning Vectorizer::_function_versioning;

        Vectorizer& Vectorizer::getVectorizer()
        {
            if(_vectorizer == 0)
                _vectorizer = new Vectorizer();

            return *_vectorizer;
        }

        Vectorizer::Vectorizer() : _svml_enabled(false), _ffast_math_enabled(false)
        {
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

        void Vectorizer::add_vector_function_version(const std::string& func_name, const Nodecl::NodeclBase& func_version,
                const std::string& device, const unsigned int vector_length, 
                const TL::Type& target_type, const FunctionPriority priority )
        {
            _function_versioning.add_version(func_name, 
                    VectorFunctionVersion(func_version, device, vector_length, target_type, priority));
        }

        void Vectorizer::enable_svml()
        {
            fprintf(stderr, "Enabling SVML\n");

            if (!_ffast_math_enabled)
            {
                fprintf(stderr, "SIMD Warning: SVML Math Library needs flag '-ffast-math' also enabled. SVML disabled.\n");
            }

            if (!_svml_enabled && _ffast_math_enabled)
            {
                _svml_enabled = true;

                // SVML SSE
                TL::Source svml_sse_vector_math;

                svml_sse_vector_math << "__m128 __svml_expf4(__m128);\n"
                    << "__m128 __svml_sqrtf4(__m128);\n"
                    << "__m128 __svml_logf4(__m128);\n"
                    << "__m128 __svml_sinf4(__m128);\n"
                    << "__m128 __svml_floorf4(__m128);\n"
                    ;

                // Parse SVML declarations
                TL::Scope global_scope = TL::Scope(CURRENT_COMPILED_FILE->global_decl_context);
                svml_sse_vector_math.parse_global(global_scope);

                // Add SVML math function as vector version of the scalar one
                _function_versioning.add_version("expf", 
                        VectorFunctionVersion(
                            global_scope.get_symbol_from_name("__svml_expf4").make_nodecl(),
                            "smp", 16, NULL, DEFAULT_FUNC_PRIORITY));
                _function_versioning.add_version("sqrtf", 
                        VectorFunctionVersion(
                            global_scope.get_symbol_from_name("__svml_sqrtf4").make_nodecl(),
                            "smp", 16, NULL, DEFAULT_FUNC_PRIORITY));
                _function_versioning.add_version("logf", 
                        VectorFunctionVersion(
                            global_scope.get_symbol_from_name("__svml_logf4").make_nodecl(),
                            "smp", 16, NULL, DEFAULT_FUNC_PRIORITY));
                _function_versioning.add_version("sinf",
                        VectorFunctionVersion( 
                            global_scope.get_symbol_from_name("__svml_sinf4").make_nodecl(),
                            "smp", 16, NULL, DEFAULT_FUNC_PRIORITY));
                _function_versioning.add_version("floorf",
                        VectorFunctionVersion( 
                            global_scope.get_symbol_from_name("__svml_floorf4").make_nodecl(),
                            "smp", 16, NULL, DEFAULT_FUNC_PRIORITY));
            }
        }

        void Vectorizer::enable_ffast_math()
        {
            _ffast_math_enabled = true;
        }
    } 
}



