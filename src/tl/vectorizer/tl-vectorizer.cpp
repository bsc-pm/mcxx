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
#include "tl-vectorizer-visitor-preprocessor.hpp"
#include "tl-vectorizer-visitor-for.hpp"
#include "tl-vectorizer-visitor-function.hpp"
#include "tl-vectorizer-vector-reduction.hpp"
#include "tl-source.hpp"
#include "tl-optimizations.hpp"

namespace TL 
{
    namespace Vectorization
    {
        VectorizerEnvironment::VectorizerEnvironment(const std::string& device,
                const unsigned int vector_length,
                const bool support_masking,
                const unsigned int mask_size,
                const bool fast_math,
                const bool prefer_gather_scatter,
                const bool prefer_mask_gather_scatter,
                const TL::Type& target_type,
                const TL::ObjectList<Nodecl::NodeclBase> * suitable_expr_list,
                const TL::ObjectList<TL::Symbol> * reduction_list,
                const VectorizerCache& vectorizer_cache,
                std::map<TL::Symbol, TL::Symbol> * new_external_vector_symbol_map) : 
           _device(device), _vector_length(vector_length), _unroll_factor(vector_length/target_type.get_size()), 
           _support_masking(support_masking), _mask_size(mask_size), _fast_math(fast_math),  
           _prefer_gather_scatter(prefer_gather_scatter), _prefer_mask_gather_scatter(prefer_mask_gather_scatter),
           _target_type(target_type), _suitable_expr_list(suitable_expr_list), _vectorizer_cache(vectorizer_cache),
           _reduction_list(reduction_list), _new_external_vector_symbol_map(new_external_vector_symbol_map)
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
        VectorizerAnalysisStaticInfo* Vectorizer::_analysis_info = 0;

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

            Vectorizer::_analysis_info = new VectorizerAnalysisStaticInfo(
                    enclosing_function,
                    Analysis::WhichAnalysis::INDUCTION_VARS_ANALYSIS |
                    Analysis::WhichAnalysis::CONSTANTS_ANALYSIS ,
                    Analysis::WhereAnalysis::NESTED_ALL_STATIC_INFO, /* nesting level */ 100);
        }

        void Vectorizer::finalize_analysis()
        {
            std::cerr << "VECTORIZER: Finalizing analysis" << std::endl;

            if(Vectorizer::_analysis_info != 0)
            {
                delete Vectorizer::_analysis_info;
                Vectorizer::_analysis_info = 0;
            }
        }

        Vectorizer::Vectorizer() : _avx2_enabled(false), _knc_enabled(false),
        _svml_sse_enabled(false), _svml_avx2_enabled(false), _svml_knc_enabled(false), 
        _fast_math_enabled(false)
        {
        }

        Vectorizer::~Vectorizer()
        {
        }

        void Vectorizer::preprocess_code(Nodecl::NodeclBase& n)
        {
            VectorizerVisitorPreprocessor vectorizer_preproc;
            vectorizer_preproc.walk(n);

            TL::Optimizations::canonicalize_and_fold(n, _fast_math_enabled);
        }

        void Vectorizer::load_environment(const Nodecl::ForStatement& for_statement,
                VectorizerEnvironment& environment)
        {
            // Set up scopes
            environment._external_scope =
                for_statement.retrieve_context();
            environment._local_scope_list.push_back(
                    for_statement.get_statement().as<Nodecl::List>().front().retrieve_context());

            // Push ForStatement as scope for analysis
            environment._analysis_simd_scope = for_statement;
            environment._analysis_scopes.push_back(for_statement);

            // Add MaskLiteral to mask_list
            Nodecl::MaskLiteral all_one_mask =
                Nodecl::MaskLiteral::make(
                        TL::Type::get_mask_type(environment._unroll_factor),
                        const_value_get_minus_one(environment._unroll_factor, 1));
            environment._mask_list.push_back(all_one_mask);
        }

        void Vectorizer::unload_environment(VectorizerEnvironment& environment)
        {
            environment._mask_list.pop_back();
            environment._analysis_scopes.pop_back();
            environment._local_scope_list.pop_back();
        }

        void Vectorizer::vectorize(Nodecl::ForStatement& for_statement,
                VectorizerEnvironment& environment)
        {
            VectorizerVisitorFor visitor_for(environment);
            visitor_for.walk(for_statement);

            // Applying strenth reduction
            TL::Optimizations::canonicalize_and_fold(for_statement, _fast_math_enabled);
        }

        void Vectorizer::vectorize(Nodecl::FunctionCode& func_code,
                VectorizerEnvironment& environment,
                const bool masked_version)
        {
            VectorizerVisitorFunction visitor_function(environment, masked_version);
            visitor_function.walk(func_code);

            // Applying strenth reduction
            TL::Optimizations::canonicalize_and_fold(func_code, _fast_math_enabled);
        }

        void Vectorizer::process_epilog(Nodecl::ForStatement& for_statement, 
                VectorizerEnvironment& environment,
                Nodecl::NodeclBase& net_epilog_node,
                int epilog_iterations,
                bool only_epilog,
                bool is_parallel_loop)
        {
            VectorizerVisitorForEpilog visitor_epilog(environment, 
                    epilog_iterations, only_epilog, is_parallel_loop);
            visitor_epilog.visit(for_statement, net_epilog_node);

            // Applying strenth reduction
            TL::Optimizations::canonicalize_and_fold(for_statement, _fast_math_enabled);
            TL::Optimizations::canonicalize_and_fold(net_epilog_node, _fast_math_enabled);
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

        int Vectorizer::get_epilog_info(const Nodecl::ForStatement& for_statement,
                VectorizerEnvironment& environment,
                bool& only_epilog)
        {
            int remain_its = -1;
            only_epilog = false;

            TL::ForStatement tl_for(for_statement);

            Nodecl::NodeclBase lb = tl_for.get_lower_bound();
            Nodecl::NodeclBase ub = tl_for.get_upper_bound();
            Nodecl::NodeclBase step = tl_for.get_step();

            if(step.is_constant())
            {
                long long int const_lb;
                long long int const_ub;
                long long int const_step = const_value_cast_to_8(step.get_constant());


                bool ub_is_suitable = false;
                bool lb_is_suitable = false;
                int lb_vector_size_module = -1;
                int ub_vector_size_module = -1;


                if (lb.is_constant())
                {
                    const_lb = const_value_cast_to_8(lb.get_constant());
                }
                else
                {
                    // Push ForStatement as scope for analysis
                    environment._analysis_simd_scope = for_statement;
                    environment._analysis_scopes.push_back(for_statement);

                    // Suitable LB 
                    lb_is_suitable = _analysis_info->is_suitable_expression(for_statement, lb,
                            environment._suitable_expr_list, environment._unroll_factor,
                            environment._vector_length, lb_vector_size_module);

                    environment._analysis_scopes.pop_back();

                    // LB 
                    if (lb_is_suitable)
                    {
                        printf("SUITABLE LB\n");
                        const_lb = 0; // Assuming 0 for suitable LB
                    }
                    else if (lb_vector_size_module != -1) // Is not suitable but is constant in some way
                    {
                        printf("VECTOR MODULE LB EPILOG %d\n", lb_vector_size_module);
                        const_ub = lb_vector_size_module;
                    }
                    else // We cannot say anything about the number of iterations of the epilog
                    {
                        printf("DEFAULT EPILOG LB\n");
                        return remain_its; // -1
                    }
                }

                if (ub.is_constant())
                {
                    const_ub = const_value_cast_to_8(ub.get_constant()) + 1;
                }
                else
                {
                    // Push ForStatement as scope for analysis
                    environment._analysis_simd_scope = for_statement;
                    environment._analysis_scopes.push_back(for_statement);

                    // Suitable UB
                    Nodecl::NodeclBase ub_plus_one =
                        Nodecl::Add::make(ub.shallow_copy(),
                                Nodecl::IntegerLiteral::make(
                                    TL::Type::get_int_type(),
                                    const_value_get_one(4, 1)),
                                ub.get_type());

                    ub_is_suitable = _analysis_info->is_suitable_expression(for_statement, ub_plus_one,
                            environment._suitable_expr_list, environment._unroll_factor,
                            environment._vector_length, ub_vector_size_module);

                    environment._analysis_scopes.pop_back();

                    // UB
                    if (ub_is_suitable)
                    {
                        printf("SUITABLE EPILOG\n");
                        const_ub = environment._unroll_factor;
                    }
                    else if (ub_vector_size_module != -1) // Is not suitable but is constant in some way
                    {
                        printf("VECTOR MODULE EPILOG %d\n", ub_vector_size_module);
                        const_ub = ub_vector_size_module;

                        if (const_lb > const_ub) 
                            const_ub += environment._unroll_factor;
                    }
                    else // We cannot say anything about the number of iterations of the epilog
                    {
                        printf("DEFAULT EPILOG\n");
                        return remain_its; // -1
                    }
                }


                // Compute epilog its
                long long int num_its = (((const_ub - const_lb)%const_step) == 0) ? 
                    ((const_ub - const_lb)/const_step) : ((const_ub - const_lb)/const_step) + 1;
                
                if ((num_its < environment._unroll_factor) && 
                        (!ub_is_suitable) && (!lb_is_suitable) && 
                        (ub_vector_size_module == -1) &&
                        (lb_vector_size_module == -1))
                {
                    printf("ONLY EPILOG\n");
                    only_epilog = true;
                }

                remain_its = num_its % environment._unroll_factor;

                printf("CONSTANT EPILOG %d\n", remain_its);
            }

            if (remain_its < -1)
            {
                internal_error("Vectorizer: Remain iterations %d < -1", remain_its);
            }

            return remain_its;
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

            if (!_svml_sse_enabled)
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

        void Vectorizer::enable_svml_avx2()
        {
            fprintf(stderr, "Enabling SVML AVX2\n");

            if (!_svml_avx2_enabled)
            {
                _svml_avx2_enabled = true;

                // SVML AVX2
                TL::Source svml_avx2_vector_math;

                svml_avx2_vector_math << "__m256 __svml_expf8(__m256);\n"
                    << "__m256 __svml_sqrtf8(__m256);\n"
                    << "__m256 __svml_logf8(__m256);\n"
                    << "__m256 __svml_sinf8(__m256);\n"
                    << "__m256 __svml_sincosf8(__m256, __m256*, __m256*);\n"
                    << "__m256 __svml_floorf8(__m256);\n"
                    ;

                // Parse SVML declarations
                TL::Scope global_scope = TL::Scope::get_global_scope();
                svml_avx2_vector_math.parse_global(global_scope);

                // Add SVML math function as vector version of the scalar one
                add_vector_function_version("expf", 
                        global_scope.get_symbol_from_name("__svml_expf8").make_nodecl(true),
                            "smp", 32, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sqrtf", 
                            global_scope.get_symbol_from_name("__svml_sqrtf8").make_nodecl(true),
                            "smp", 32, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("logf", 
                            global_scope.get_symbol_from_name("__svml_logf8").make_nodecl(true),
                            "smp", 32, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sinf",
                            global_scope.get_symbol_from_name("__svml_sinf8").make_nodecl(true),
                            "smp", 32, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sincosf",
                            global_scope.get_symbol_from_name("__svml_sincosf8").make_nodecl(true),
                            "smp", 32, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("floorf",
                            global_scope.get_symbol_from_name("__svml_floorf8").make_nodecl(true),
                            "smp", 32, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
            }
        }

        void Vectorizer::enable_svml_knc()
        {
            fprintf(stderr, "Enabling SVML KNC\n");

            if (!_svml_knc_enabled)
            {
                _svml_knc_enabled = true;

                // SVML KNC
                TL::Source svml_knc_vector_math;

                // No mask
                svml_knc_vector_math << "__m512 _mm512_exp_ps(__m512);\n"
                    << "__m512 _mm512_sqrt_ps(__m512);\n"
                    << "__m512 _mm512_log_ps(__m512);\n"
                    << "__m512 _mm512_sin_ps(__m512);\n"
                    << "__m512 _mm512_cos_ps(__m512);\n"
//                    << "__m512 _mm512_sincos_ps(__m512*, __m512);\n"
//                    << "__m512 __svml_sincosf16_ha(__m512*, __m512);\n"
                    << "__m512 _mm512_floor_ps(__m512);\n"
                    << "__m512d _mm512_exp_pd(__m512d);\n"
                    << "__m512d _mm512_sqrt_pd(__m512d);\n"
                    << "__m512d _mm512_log_pd(__m512d);\n"
                    << "__m512d _mm512_sin_pd(__m512d);\n"
                    << "__m512d _mm512_cos_pd(__m512d);\n"
//                    << "__m512d _mm512_sincos_pd(__m512d, __m512d*);\n"
                    << "__m512d _mm512_floor_pd(__m512d);\n"
                    ;

                // Mask
                svml_knc_vector_math << "__m512 _mm512_mask_exp_ps(__m512, __mmask16, __m512);\n"
                    << "__m512 _mm512_mask_sqrt_ps(__m512, __mmask16, __m512);\n"
                    << "__m512 _mm512_mask_log_ps(__m512, __mmask16, __m512);\n"
                    << "__m512 _mm512_mask_sin_ps(__m512, __mmask16, __m512);\n"
                    << "__m512 _mm512_mask_cos_ps(__m512, __mmask16, __m512);\n"
//                    << "__m512 _mm512_mask_sincos_ps(__m512, __mmask16, __m512*, __m512);\n"
//                    << "__m512 __svml_sincosf16_ha_mask(__m512*, __mmask16, __m512);\n"
                    << "__m512 _mm512_mask_floor_ps(__m512, __mmask16, __m512);\n"
                    << "__m512d _mm512_mask_exp_pd(__m512d, __mmask8, __m512d);\n"
                    << "__m512d _mm512_mask_sqrt_pd(__m512d, __mmask8, __m512d);\n"
                    << "__m512d _mm512_mask_log_pd(__m512d, __mmask8, __m512d);\n"
                    << "__m512d _mm512_mask_sin_pd(__m512d, __mmask8, __m512d);\n"
                    << "__m512d _mm512_mask_cos_pd(__m512d, __mmask8, __m512d);\n"
//                    << "__m512d _mm512_mask_sincos_pd(__m512d, __mmask8, __m512d*);\n"
                    << "__m512d _mm512_mask_floor_pd(__m512d, __mmask8, __m512d);\n"
                    ;

                // Parse SVML declarations
                TL::Scope global_scope = TL::Scope::get_global_scope();
                svml_knc_vector_math.parse_global(global_scope);

                // Add SVML math function as vector version of the scalar one
                add_vector_function_version("expf", 
                            global_scope.get_symbol_from_name("_mm512_exp_ps").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sqrtf", 
                            global_scope.get_symbol_from_name("_mm512_sqrt_ps").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("logf", 
                            global_scope.get_symbol_from_name("_mm512_log_ps").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sinf",
                            global_scope.get_symbol_from_name("_mm512_sin_ps").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("cosf",
                            global_scope.get_symbol_from_name("_mm512_cos_ps").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
//It seems it doesn't exist in MIC
//                add_vector_function_version("sincosf",
//                            global_scope.get_symbol_from_name("_mm512_sincos_ps").make_nodecl(true),
//                            "knc", 64, TL::Type::get_float_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("floor",
                            global_scope.get_symbol_from_name("_mm512_floor_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("exp", 
                            global_scope.get_symbol_from_name("_mm512_exp_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sqrt", 
                            global_scope.get_symbol_from_name("_mm512_sqrt_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("log", 
                            global_scope.get_symbol_from_name("_mm512_log_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sin",
                            global_scope.get_symbol_from_name("_mm512_sin_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("cos",
                            global_scope.get_symbol_from_name("_mm512_cos_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), false, DEFAULT_FUNC_PRIORITY, true);
//It seems it doesn't exist in MIC
//                add_vector_function_version("sincos",
//                            global_scope.get_symbol_from_name("_mm512_sincos_pd").make_nodecl(true),
//                            "knc", 64, TL::Type::get_double_type(), false, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("floor",
                            global_scope.get_symbol_from_name("_mm512_floor_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), false, DEFAULT_FUNC_PRIORITY, true);


                // Add SVML math masked function as vector version of the scalar one
                add_vector_function_version("expf", 
                            global_scope.get_symbol_from_name("_mm512_mask_exp_ps").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sqrtf", 
                            global_scope.get_symbol_from_name("_mm512_mask_sqrt_ps").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("logf", 
                            global_scope.get_symbol_from_name("_mm512_mask_log_ps").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sinf",
                            global_scope.get_symbol_from_name("_mm512_mask_sin_ps").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("cosf",
                            global_scope.get_symbol_from_name("_mm512_mask_cos_ps").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
// It seems it doesn't exist in MIC
//                add_vector_function_version("sincosf",
//                            global_scope.get_symbol_from_name("_mm512_mask_sincos_ps").make_nodecl(true),
//                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("floorf",
                            global_scope.get_symbol_from_name("_mm512_mask_floor_ps").make_nodecl(true),
                            "knc", 64, TL::Type::get_float_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("exp", 
                            global_scope.get_symbol_from_name("_mm512_mask_exp_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sqrt", 
                            global_scope.get_symbol_from_name("_mm512_mask_sqrt_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("log", 
                            global_scope.get_symbol_from_name("_mm512_mask_log_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("sin",
                            global_scope.get_symbol_from_name("_mm512_mask_sin_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("cos",
                            global_scope.get_symbol_from_name("_mm512_mask_cos_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), true, DEFAULT_FUNC_PRIORITY, true);
// It seems it doesn't exist in MIC
//                add_vector_function_version("sincos",
//                            global_scope.get_symbol_from_name("_mm512_mask_sincos_pd").make_nodecl(true),
//                            "knc", 64, TL::Type::get_double_type(), true, DEFAULT_FUNC_PRIORITY, true);
                add_vector_function_version("floor",
                            global_scope.get_symbol_from_name("_mm512_mask_floor_pd").make_nodecl(true),
                            "knc", 64, TL::Type::get_double_type(), true, DEFAULT_FUNC_PRIORITY, true);
            }
        }


        void Vectorizer::enable_fast_math()
        {
            _fast_math_enabled = true;
        }
    } 
}
