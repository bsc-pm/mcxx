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

#include "tl-vectorizer.hpp"
#include "tl-vectorizer-visitor-for.hpp"
#include "tl-vectorizer-visitor-function.hpp"
#include "tl-source.hpp"

namespace TL 
{
    namespace Vectorization
    {
        
        VectorizerEnvironment::VectorizerEnvironment(const std::string& device,
                const unsigned int vector_length,
                const bool support_masking,
                const unsigned int mask_size,
                const TL::Type& target_type,
                const TL::Scope& local_scope,
                const Nodecl::List& suitable_expr_list) : 
           _device(device), _vector_length(vector_length), _unroll_factor(vector_length/4), //TODO
           _mask_size(mask_size), _support_masking(support_masking), _target_type(target_type), 
           _suitable_expr_list(suitable_expr_list)
        {
            _local_scope_list.push_back(local_scope);
            _mask_list.push_back(Nodecl::NodeclBase::null());
        }
        
        VectorizerEnvironment::~VectorizerEnvironment()
        {
            _local_scope_list.pop_back();
        }


        Vectorizer *Vectorizer::_vectorizer = 0;
        FunctionVersioning Vectorizer::_function_versioning;
        Analysis::AnalysisStaticInfo* Vectorizer::_analysis_info = 0;

        Vectorizer& Vectorizer::get_vectorizer()
        {
            if(_vectorizer == 0)
                _vectorizer = new Vectorizer();

            return *_vectorizer;
        }

        Vectorizer::Vectorizer() : _svml_sse_enabled(false), _svml_knc_enabled(false), _ffast_math_enabled(false)
        {
            _var_counter = 0;
        }

        Vectorizer::~Vectorizer()
        {
        }

        std::string Vectorizer::get_var_counter()
        {
            std::stringstream result;

            result << _var_counter;
            _var_counter++;

            return result.str();
        }

        bool Vectorizer::vectorize(const Nodecl::ForStatement& for_statement,
                VectorizerEnvironment& environment)
        {
            VectorizerVisitorFor visitor_for(environment);

            return visitor_for.walk(for_statement);
        }

        void Vectorizer::vectorize(const Nodecl::FunctionCode& func_code,
                VectorizerEnvironment& environment,
                const bool masked_version)
        {
            VectorizerVisitorFunction visitor_function(environment, masked_version);
            visitor_function.walk(func_code);
        }

        void Vectorizer::process_epilog(const Nodecl::ForStatement& for_statement, 
                VectorizerEnvironment& environment)
        {
            VectorizerVisitorForEpilog visitor_epilog(environment);
            visitor_epilog.walk(for_statement);
        }

        void Vectorizer::add_vector_function_version(const std::string& func_name, 
                const Nodecl::NodeclBase& func_version,
                const std::string& device, const unsigned int vector_length, 
                const TL::Type& target_type, const bool masked, const FunctionPriority priority,
                const bool is_svml)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "VECTORIZER: Adding '%s' function version (device=%s, vector_length=%u, priority=%d)\n",
                        func_name.c_str(), device.c_str(), vector_length, priority);
            }

            _function_versioning.add_version(func_name,
                    VectorFunctionVersion(func_version, device, vector_length, target_type,
                        masked, priority, is_svml));
        }

        bool Vectorizer::is_svml_function(const std::string& func_name, 
                const std::string& device, 
                const unsigned int vector_length, 
                const TL::Type& target_type, 
                const bool masked) const
        {
            return _function_versioning.is_svml_function(func_name,
                    device, vector_length, target_type, masked);
        }

        void Vectorizer::enable_svml_sse()
        {
            fprintf(stderr, "Enabling SVML SSE\n");

            if (!_ffast_math_enabled)
            {
                fprintf(stderr, "SIMD Warning: SVML Math Library needs flag '-ffast-math' also enabled. SVML disabled.\n");
            }

            if (!_svml_sse_enabled && _ffast_math_enabled)
            {
                _svml_sse_enabled = true;

                // SVML SSE
                TL::Source svml_sse_vector_math;

                svml_sse_vector_math << "__m128 __svml_expf4(__m128);\n"
                    << "__m128 __svml_sqrtf4(__m128);\n"
                    << "__m128 __svml_logf4(__m128);\n"
                    << "__m128 __svml_sinf4(__m128);\n"
                    << "__m128 __svml_sincosf4(__m128, __m128*, __m128*);\n"
                    << "__m128 __svml_floorf4(__m128);\n"
                    ;

                // Parse SVML declarations
                TL::Scope global_scope = TL::Scope(CURRENT_COMPILED_FILE->global_decl_context);
                svml_sse_vector_math.parse_global(global_scope);

                // Add SVML math function as vector version of the scalar one
                add_vector_function_version("expf", 
                        global_scope.get_symbol_from_name("__svml_expf4").make_nodecl(true),
                            "smp", 16, NULL, false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sqrtf", 
                            global_scope.get_symbol_from_name("__svml_sqrtf4").make_nodecl(true),
                            "smp", 16, NULL, false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("logf", 
                            global_scope.get_symbol_from_name("__svml_logf4").make_nodecl(true),
                            "smp", 16, NULL, false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sinf",
                            global_scope.get_symbol_from_name("__svml_sinf4").make_nodecl(true),
                            "smp", 16, NULL, false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sincosf",
                            global_scope.get_symbol_from_name("__svml_sincosf4").make_nodecl(true),
                            "smp", 16, NULL, false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("floorf",
                            global_scope.get_symbol_from_name("__svml_floorf4").make_nodecl(true),
                            "smp", 16, NULL, false, DEFAULT_FUNC_PRIORITY, true);
            }
        }

        void Vectorizer::enable_svml_knc()
        {
            fprintf(stderr, "Enabling SVML KNC\n");

            if (!_ffast_math_enabled)
            {
                fprintf(stderr, "SIMD Warning: SVML Math Library needs flag '-ffast-math' also enabled. SVML disabled.\n");
            }

            if (!_svml_sse_enabled && _ffast_math_enabled)
            {
                _svml_sse_enabled = true;

                // SVML SSE
                TL::Source svml_sse_vector_math;

                // No mask
                svml_sse_vector_math << "__m512 __svml_expf16(__m512);\n"
                    << "__m512 __svml_sqrtf16(__m512);\n"
                    << "__m512 __svml_logf16(__m512);\n"
                    << "__m512 __svml_sinf16(__m512);\n"
                    << "__m512 __svml_sincosf16(__m512, __m512*, __m512*);\n"
                    << "__m512 __svml_floorf16(__m512);\n"
                    ;

                // Mask
                svml_sse_vector_math << "__m512 __svml_mask_expf16(__m512, __mmask16, __m512);\n"
                    << "__m512 __svml_mask_sqrtf16(__m512, __mmask16, __m512);\n"
                    << "__m512 __svml_mask_logf16(__m512, __mmask16, __m512);\n"
                    << "__m512 __svml_mask_sinf16(__m512, __mmask16, __m512);\n"
                    << "__m512 __svml_mask_sincosf16(__m512, __mmask16, __m512*, __m512*);\n"
                    << "__m512 __svml_mask_floorf16(__m512, __mmask16, __m512);\n"
                    ;

                // Parse SVML declarations
                TL::Scope global_scope = TL::Scope(CURRENT_COMPILED_FILE->global_decl_context);
                svml_sse_vector_math.parse_global(global_scope);

                // Add SVML math function as vector version of the scalar one
                add_vector_function_version("expf", 
                            global_scope.get_symbol_from_name("__svml_expf16").make_nodecl(true),
                            "knc", 64, NULL, false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sqrtf", 
                            global_scope.get_symbol_from_name("__svml_sqrtf16").make_nodecl(true),
                            "knc", 64, NULL, false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("logf", 
                            global_scope.get_symbol_from_name("__svml_logf16").make_nodecl(true),
                            "knc", 64, NULL, false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sinf",
                            global_scope.get_symbol_from_name("__svml_sinf16").make_nodecl(true),
                            "knc", 64, NULL, false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sincosf",
                            global_scope.get_symbol_from_name("__svml_sincosf16").make_nodecl(true),
                            "knc", 64, NULL, false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("floorf",
                            global_scope.get_symbol_from_name("__svml_floorf16").make_nodecl(true),
                            "knc", 64, NULL, false, DEFAULT_FUNC_PRIORITY, true);
                
                // Add SVML math masked function as vector version of the scalar one
                add_vector_function_version("expf", 
                            global_scope.get_symbol_from_name("__svml_mask_expf16").make_nodecl(true),
                            "knc", 64, NULL, true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sqrtf", 
                            global_scope.get_symbol_from_name("__svml_mask_sqrtf16").make_nodecl(true),
                            "knc", 64, NULL, true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("logf", 
                            global_scope.get_symbol_from_name("__svml_mask_logf16").make_nodecl(true),
                            "knc", 64, NULL, true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sinf",
                            global_scope.get_symbol_from_name("__svml_mask_sinf16").make_nodecl(true),
                            "knc", 64, NULL, true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sincosf",
                            global_scope.get_symbol_from_name("__svml_mask_sincosf16").make_nodecl(true),
                            "knc", 64, NULL, true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("floorf",
                            global_scope.get_symbol_from_name("__svml_mask_floorf16").make_nodecl(true),
                            "knc", 64, NULL, true, DEFAULT_FUNC_PRIORITY, true);
            }
        }


        void Vectorizer::enable_ffast_math()
        {
            _ffast_math_enabled = true;
        }

        TL::Type get_qualified_vector_to(TL::Type src_type, const unsigned int size) 
        {
            cv_qualifier_t cv_qualif = get_cv_qualifier(no_ref(src_type.get_internal_type()));
            TL::Type result_type = src_type.no_ref().get_unqualified_type().get_vector_to(size);

            result_type = get_cv_qualified_type(result_type.get_internal_type(), cv_qualif);

            if (src_type.is_lvalue_reference())
            {
                result_type = result_type.get_lvalue_reference_to();
            }

            return result_type;
        }
    } 
}
