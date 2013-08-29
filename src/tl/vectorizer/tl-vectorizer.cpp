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
#include "tl-vectorizer-vector-reduction.hpp"
#include "tl-source.hpp"

namespace TL 
{
    namespace Vectorization
    {
        VectorizerEnvironment::VectorizerEnvironment(const std::string& device,
                const unsigned int vector_length,
                const bool support_masking,
                const bool is_parallel_loop,
                const unsigned int mask_size,
                const TL::Type& target_type,
                const Nodecl::List * suitable_expr_list,
                const TL::ObjectList<TL::Symbol> * reduction_list,
                std::map<TL::Symbol, TL::Symbol> * new_external_vector_symbol_map) : 
           _device(device), _vector_length(vector_length), _unroll_factor(vector_length/target_type.get_size()), 
           _mask_size(mask_size), _support_masking(support_masking), _is_parallel_loop(is_parallel_loop), 
           _target_type(target_type), _suitable_expr_list(suitable_expr_list), _reduction_list(reduction_list),
           _new_external_vector_symbol_map(new_external_vector_symbol_map)
        {
            std::cerr << "VECTORIZER: Target type size: " << _target_type.get_size()
               << " . Unroll factor: " << _unroll_factor << std::endl;

            _inside_inner_masked_bb.push_back(false);
            _mask_check_bb_cost.push_back(0);
        }
 
        VectorizerEnvironment::~VectorizerEnvironment()
        {
            _inside_inner_masked_bb.pop_back();
            _mask_check_bb_cost.pop_back();
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

        void Vectorizer::initialize_analysis(const Nodecl::FunctionCode& enclosing_function)
        {
            std::cerr << "VECTORIZER: Computing new analysis" << std::endl;

            if(Vectorizer::_analysis_info != 0)
                running_error("VECTORIZER: Analysis was previously initialize");

            Vectorizer::_analysis_info = new Analysis::AnalysisStaticInfo(
                    enclosing_function,
                    Analysis::WhichAnalysis::INDUCTION_VARS_ANALYSIS |
                    Analysis::WhichAnalysis::CONSTANTS_ANALYSIS ,
                    Analysis::WhereAnalysis::NESTED_ALL_STATIC_INFO, /* nesting level */ 100);
        }

#if 0
        void Vectorizer::initialize_analysis(const Nodecl::FunctionCode& enclosing_function)
        {
            if ((Vectorizer::_analysis_info == 0) || 
                    (Vectorizer::_analysis_info->get_nodecl_origin() != enclosing_function))
            {
                std::cerr << "VECTORIZER: Computing new analysis" << std::endl;
               
                /* 
                if (Vectorizer::_analysis_info != 0 && Vectorizer::_analysis_info->get_nodecl_origin() != enclosing_function)
                {
                    std::cerr << _analysis_info->get_nodecl_origin().prettyprint()
                        << std::endl
                        << std::endl
                        << enclosing_function.prettyprint();
                }
                */

                if(Vectorizer::_analysis_info != 0)
                    delete Vectorizer::_analysis_info;

                Vectorizer::_analysis_info = new Analysis::AnalysisStaticInfo(
                        enclosing_function,
                        Analysis::WhichAnalysis::INDUCTION_VARS_ANALYSIS |
                        Analysis::WhichAnalysis::CONSTANTS_ANALYSIS ,
                        Analysis::WhereAnalysis::NESTED_ALL_STATIC_INFO, /* nesting level */ 100);
            }
            else
            {
                std::cerr << "VECTORIZER: Reusing previous analysis" << std::endl;
            }
        }
#endif
        void Vectorizer::finalize_analysis()
        {
            std::cerr << "VECTORIZER: Finalizing analysis" << std::endl;

            if(Vectorizer::_analysis_info != 0)
            {
                delete Vectorizer::_analysis_info;
                Vectorizer::_analysis_info = 0;
            }
        }

        Vectorizer::Vectorizer() : _svml_sse_enabled(false), _svml_knc_enabled(false), _fast_math_enabled(false)
        {
        }

        Vectorizer::~Vectorizer()
        {
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

        bool Vectorizer::is_supported_reduction(bool is_builtin,
                const std::string& reduction_name,
                const TL::Type& reduction_type,
                const VectorizerEnvironment& environment)
        {
            VectorizerVectorReduction vector_reduction(environment);

            return vector_reduction.is_supported_reduction(is_builtin,
                    reduction_name, reduction_type);
        }

        void Vectorizer::vectorize_reduction(const TL::Symbol& scalar_symbol,
                TL::Symbol& vector_symbol,
                const Nodecl::NodeclBase& initializer,
                const std::string& reduction_name,
                const TL::Type& reduction_type,
                const VectorizerEnvironment& environment,
                Nodecl::List& pre_nodecls,
                Nodecl::List& post_nodecls)
        {
            VectorizerVectorReduction vector_reduction(environment);

            vector_reduction.vectorize_reduction(
                    scalar_symbol,
                    vector_symbol,
                    initializer,
                    reduction_name,
                    reduction_type,
                    pre_nodecls,
                    post_nodecls);
        }

        void Vectorizer::add_vector_function_version(const std::string& func_name, 
                const Nodecl::NodeclBase& func_version,
                const std::string& device, const unsigned int vector_length, 
                const TL::Type& target_type, const bool masked, const FunctionPriority priority,
                const bool is_svml)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "VECTORIZER: Adding '%s' function version (device=%s, vector_length=%u, target_type=%s, SVML=%d, masked=%d priority=%d)\n",
                        func_name.c_str(), device.c_str(), vector_length, 
                        target_type.get_simple_declaration(TL::Scope::get_global_scope(), "").c_str(),
                        masked, is_svml,
                        priority);
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

            if (!_fast_math_enabled)
            {
                fprintf(stderr, "SIMD Warning: SVML Math Library needs flag '--fast-math' also enabled. SVML disabled.\n");
            }

            if (!_svml_sse_enabled && _fast_math_enabled)
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
                TL::Scope global_scope = TL::Scope::get_global_scope();
                svml_sse_vector_math.parse_global(global_scope);

                // Add SVML math function as vector version of the scalar one
                add_vector_function_version("expf", 
                        global_scope.get_symbol_from_name("__svml_expf4").make_nodecl(true),
                            "smp", 16, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sqrtf", 
                            global_scope.get_symbol_from_name("__svml_sqrtf4").make_nodecl(true),
                            "smp", 16, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("logf", 
                            global_scope.get_symbol_from_name("__svml_logf4").make_nodecl(true),
                            "smp", 16, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sinf",
                            global_scope.get_symbol_from_name("__svml_sinf4").make_nodecl(true),
                            "smp", 16, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sincosf",
                            global_scope.get_symbol_from_name("__svml_sincosf4").make_nodecl(true),
                            "smp", 16, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("floorf",
                            global_scope.get_symbol_from_name("__svml_floorf4").make_nodecl(true),
                            "smp", 16, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
            }
        }

        void Vectorizer::enable_svml_knc()
        {
            fprintf(stderr, "Enabling SVML KNC\n");

            if (!_fast_math_enabled)
            {
                fprintf(stderr, "SIMD Warning: SVML Math Library needs flag '--fast-math' also enabled. SVML disabled.\n");
            }

            if (!_svml_sse_enabled && _fast_math_enabled)
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
                svml_sse_vector_math << "__m512 __svml_expf16_mask(__m512, __mmask16, __m512);\n"
                    << "__m512 __svml_sqrtf16_mask(__m512, __mmask16, __m512);\n"
                    << "__m512 __svml_logf16_mask(__m512, __mmask16, __m512);\n"
                    << "__m512 __svml_sinf16_mask(__m512, __mmask16, __m512);\n"
                    << "__m512 __svml_sincosf16_mask(__m512, __mmask16, __m512*, __m512*);\n"
                    << "__m512 __svml_floorf16_mask(__m512, __mmask16, __m512);\n"
                    ;

                // Parse SVML declarations
                TL::Scope global_scope = TL::Scope::get_global_scope();
                svml_sse_vector_math.parse_global(global_scope);

                // Add SVML math function as vector version of the scalar one
                add_vector_function_version("expf", 
                            global_scope.get_symbol_from_name("__svml_expf16").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sqrtf", 
                            global_scope.get_symbol_from_name("__svml_sqrtf16").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("logf", 
                            global_scope.get_symbol_from_name("__svml_logf16").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sinf",
                            global_scope.get_symbol_from_name("__svml_sinf16").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sincosf",
                            global_scope.get_symbol_from_name("__svml_sincosf16").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("floorf",
                            global_scope.get_symbol_from_name("__svml_floorf16").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                
                // Add SVML math masked function as vector version of the scalar one
                add_vector_function_version("expf", 
                            global_scope.get_symbol_from_name("__svml_expf16_mask").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sqrtf", 
                            global_scope.get_symbol_from_name("__svml_sqrtf16_mask").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("logf", 
                            global_scope.get_symbol_from_name("__svml_logf16_mask").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sinf",
                            global_scope.get_symbol_from_name("__svml_sinf16_mask").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sincosf",
                            global_scope.get_symbol_from_name("__svml_sincosf16_mask").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("floorf",
                            global_scope.get_symbol_from_name("__svml_floorf16_mask").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
            }
        }


        void Vectorizer::enable_fast_math()
        {
            _fast_math_enabled = true;
        }
    } 
}
