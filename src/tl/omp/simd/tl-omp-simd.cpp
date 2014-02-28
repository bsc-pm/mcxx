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

#include "cxx-cexpr.h"
#include "tl-omp-simd.hpp"
#include "tl-omp.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-optimizations.hpp"

using namespace TL::Vectorization;

namespace TL { 
    namespace OpenMP {

        Simd::Simd()
            : PragmaCustomCompilerPhase("omp-simd"),  
            _simd_enabled(false), _svml_enabled(false), _fast_math_enabled(false), 
            _avx2_enabled(false), _knc_enabled(false), _prefer_gather_scatter(false),
            _prefer_mask_gather_scatter(false)
        {        
            set_phase_name("Vectorize OpenMP SIMD parallel IR");
            set_phase_description("This phase vectorize the OpenMP SIMD parallel IR");

            register_parameter("simd_enabled",
                    "If set to '1' enables simd constructs, otherwise it is disabled",
                    _simd_enabled_str,
                    "0").connect(functor(&Simd::set_simd, *this));


            register_parameter("svml_enabled",
                    "If set to '1' enables svml math library, otherwise it is disabled",
                    _svml_enabled_str,
                    "0").connect(functor(&Simd::set_svml, *this));

            register_parameter("fast_math_enabled",
                    "If set to '1' enables fast_math operations, otherwise it is disabled",
                    _fast_math_enabled_str,
                    "0").connect(functor(&Simd::set_fast_math, *this));

            register_parameter("mic_enabled",
                    "If set to '1' enables compilation for KNC architecture, otherwise it is disabled",
                    _knc_enabled_str,
                    "0").connect(functor(&Simd::set_knc, *this));

            register_parameter("avx2_enabled",
                    "If set to '1' enables compilation for AVX2 instruction set, otherwise it is disabled",
                    _avx2_enabled_str,
                    "0").connect(functor(&Simd::set_avx2, *this));

            register_parameter("prefer_gather_scatter",
                    "If set to '1' enables generation of gather/scatter instructions instead of unaligned memory instructions",
                    _prefer_gather_scatter_str,
                    "0").connect(functor(&Simd::set_prefer_gather_scatter, *this));

            register_parameter("prefer_mask_gather_scatter",
                    "If set to '1' enables generation of gather/scatter instructions instead of unaligned memory instructions with masks",
                    _prefer_mask_gather_scatter_str,
                    "0").connect(functor(&Simd::set_prefer_mask_gather_scatter, *this));
        }

        void Simd::set_simd(const std::string simd_enabled_str)
        {
            if (simd_enabled_str == "1")
            {
                _simd_enabled = true;
            }
        }

        void Simd::set_svml(const std::string svml_enabled_str)
        {
            if (svml_enabled_str == "1")
            {
                _svml_enabled = true;
            }
        }

        void Simd::set_fast_math(const std::string fast_math_enabled_str)
        {
            if (fast_math_enabled_str == "1")
            {
                _fast_math_enabled = true;
            }
        }

        void Simd::set_knc(const std::string knc_enabled_str)
        {
            if (knc_enabled_str == "1")
            {
                _knc_enabled = true;
            }
        }

        void Simd::set_avx2(const std::string avx2_enabled_str)
        {
            if (avx2_enabled_str == "1")
            {
                _avx2_enabled = true;
            }
        }

        void Simd::set_prefer_gather_scatter(const std::string prefer_gather_scatter_str)
        {
            if (prefer_gather_scatter_str == "1")
            {
                _prefer_gather_scatter = true;
            }
        }

        void Simd::set_prefer_mask_gather_scatter(const std::string prefer_mask_gather_scatter_str)
        {
            if (prefer_mask_gather_scatter_str == "1")
            {
                _prefer_mask_gather_scatter = true;
            }
        }

        void Simd::pre_run(TL::DTO& dto)
        {
            this->PragmaCustomCompilerPhase::pre_run(dto);
        }

        void Simd::run(TL::DTO& dto)
        {
            this->PragmaCustomCompilerPhase::run(dto);

            //RefPtr<FunctionTaskSet> function_task_set = RefPtr<FunctionTaskSet>::cast_static(dto["openmp_task_info"]);

            Nodecl::NodeclBase translation_unit = dto["nodecl"];

            if (_simd_enabled)
            {
                TL::Vectorization::SIMDInstructionSet simd_isa;

                if(_avx2_enabled)
                {
                    simd_isa = AVX2_ISA;
                }
                else if (_knc_enabled)
                {
                    simd_isa = KNC_ISA;
                }
                else
                {
                    simd_isa = SSE4_2_ISA;
                }

                if (_avx2_enabled && _knc_enabled)
                {
                    running_error("SIMD: AVX2 and KNC SIMD instruction sets enabled at the same time");
                }

                SimdVisitor simd_visitor(simd_isa, _fast_math_enabled, _svml_enabled,
                        _prefer_gather_scatter, _prefer_mask_gather_scatter);
                simd_visitor.walk(translation_unit);
            }
        }

        SimdVisitor::SimdVisitor(Vectorization::SIMDInstructionSet simd_isa, 
                bool fast_math_enabled, bool svml_enabled,  
                bool prefer_gather_scatter, bool prefer_mask_gather_scatter)
            : _vectorizer(TL::Vectorization::Vectorizer::get_vectorizer())
        {
            _prefer_gather_scatter = prefer_gather_scatter;
            _prefer_mask_gather_scatter = prefer_mask_gather_scatter;

            if (fast_math_enabled)
            {
                _fast_math_enabled = true;
                _vectorizer.enable_fast_math();
            }
            else
            {
                _fast_math_enabled = false;
            }

            switch (simd_isa)
            {
                case KNC_ISA:
                    _vector_length = 64;
                    _device_name = "knc";
                    _support_masking = true;
                    _mask_size = 16;

                    if (svml_enabled)
                        _vectorizer.enable_svml_knc();
                    break;
 
                case AVX2_ISA:
                    _vector_length = 32;
                    _device_name = "avx2";
                    _support_masking = false;
                    _mask_size = 0;

                    if (svml_enabled)
                        _vectorizer.enable_svml_avx2();
                    break;
                   
                 case SSE4_2_ISA:
                    _vector_length = 16;
                    _device_name = "smp";
                    _support_masking = false;
                    _mask_size = 0;

                    if (svml_enabled)
                        _vectorizer.enable_svml_sse();

                    break;

                 default:
                    running_error("SIMD: Unsupported SIMD ISA: %d", 
                            simd_isa);

            }
        }

        void SimdVisitor::visit(const Nodecl::OpenMP::Simd& simd_node)
        {
            Nodecl::ForStatement for_statement = simd_node.get_statement().as<Nodecl::ForStatement>();
            Nodecl::List simd_environment = simd_node.get_environment().as<Nodecl::List>();

            // Aligned clause
            std::map<TL::Symbol, int> aligned_expressions;
            process_aligned_clause(simd_environment, aligned_expressions);

            // Suitable clause
            TL::ObjectList<Nodecl::NodeclBase> suitable_expressions;
            process_suitable_clause(simd_environment, suitable_expressions);

            // Nontemporal clause
            TL::ObjectList<Nodecl::NodeclBase> nontemporal_expressions;
            process_nontemporal_clause(simd_environment, nontemporal_expressions);

            // Vectorlengthfor clause
            TL::Type vectorlengthfor_type;
            process_vectorlengthfor_clause(simd_environment, vectorlengthfor_type);

            // Cache clause
            TL::ObjectList<Nodecl::NodeclBase> cached_expressions;
            process_cache_clause(simd_environment, cached_expressions);
            VectorizerCache vectorizer_cache(cached_expressions);

            // External symbols (loop)
            std::map<TL::Symbol, TL::Symbol> new_external_vector_symbol_map;

            // Reduction and simd_reduction clauses
            TL::ObjectList<TL::Symbol> reductions;

            Nodecl::List omp_reduction_list = process_reduction_clause(simd_environment, 
                    reductions, new_external_vector_symbol_map,
                    for_statement);

            // Vectorizer Environment
            VectorizerEnvironment for_environment(
                    _device_name,
                    _vector_length, 
                    _support_masking,
                    _mask_size,
                    _fast_math_enabled,
                    _prefer_gather_scatter,
                    _prefer_mask_gather_scatter,
                    vectorlengthfor_type,
                    aligned_expressions,
                    suitable_expressions,
                    nontemporal_expressions,
                    vectorizer_cache,
                    &reductions,
                    &new_external_vector_symbol_map);

            // Get code ready for vectorisation
            _vectorizer.preprocess_code(for_statement, for_environment);

            // Add epilog before vectorization
            Nodecl::OpenMP::Simd simd_node_epilog = Nodecl::Utils::deep_copy(
                    simd_node, simd_node).as<Nodecl::OpenMP::Simd>();

            simd_node.append_sibling(simd_node_epilog);

            // Initialize analysis info
            Nodecl::NodeclBase enclosing_func = 
                Nodecl::Utils::get_enclosing_function(for_statement).get_function_code();

            _vectorizer.initialize_analysis(enclosing_func.as<Nodecl::FunctionCode>());

            // Get epilog information
            bool only_epilog;
            int epilog_iterations = _vectorizer.get_epilog_info(for_statement, 
                    for_environment, only_epilog);

            // Add scopes, default masks, etc.
            _vectorizer.load_environment(for_statement, for_environment);

            // Cache init
            vectorizer_cache.declare_cache_symbols(
                    for_statement.retrieve_context(), for_environment);
            simd_node.prepend_sibling(vectorizer_cache.get_init_statements(for_environment));

            // Call to vectorizer
            if (!only_epilog)
            {
                _vectorizer.vectorize(for_statement, for_environment); 
            }

            _vectorizer.unload_environment(for_environment);

            // Add new vector symbols
            if (!new_external_vector_symbol_map.empty())
            {
                Nodecl::List pre_for_nodecls, post_for_nodecls;

                // REDUCTIONS
                for(Nodecl::List::iterator it = omp_reduction_list.begin();
                        it != omp_reduction_list.end();
                        it++)
                {
                    // Prepare reduction information to be used in vectorizer
                    Nodecl::OpenMP::ReductionItem omp_red_item = (*it).as<Nodecl::OpenMP::ReductionItem>();
                    TL::OpenMP::Reduction omp_red = *(OpenMP::Reduction::get_reduction_info_from_symbol(
                                omp_red_item.get_reductor().get_symbol()));

                    // Symbols
                    std::map<TL::Symbol, TL::Symbol>::iterator new_external_symbol_pair = 
                        new_external_vector_symbol_map.find(omp_red_item.get_reduced_symbol().get_symbol());

                    TL::Symbol scalar_tl_symbol = new_external_symbol_pair->first;
                    TL::Symbol vector_tl_symbol = new_external_symbol_pair->second;

                    // Reduction info
                    Nodecl::NodeclBase reduction_initializer = omp_red.get_initializer();
                    std::string reduction_name = omp_red.get_name();
                    TL::Type reduction_type = omp_red.get_type();

                    // Vectorize reductions
                    if(_vectorizer.is_supported_reduction(
                                omp_red.is_builtin(reduction_name),
                                reduction_name,
                                reduction_type,
                                for_environment))
                    {
                        _vectorizer.vectorize_reduction(scalar_tl_symbol, 
                                vector_tl_symbol, 
                                reduction_initializer,
                                reduction_name,
                                reduction_type,
                                for_environment,
                                pre_for_nodecls,
                                post_for_nodecls);
                    }
                    else
                    {
                        running_error("SIMD: reduction '%s:%s' is not supported", 
                                reduction_name.c_str(), scalar_tl_symbol.get_name().c_str());
                    }
                }

                simd_node.prepend_sibling(pre_for_nodecls);
                // Final reduction after the epilog (to reduce also elements from masked epilogs)
                simd_node_epilog.append_sibling(post_for_nodecls);

                // TODO: 
                // firstprivate in SIMD
            }

            // Process epilog
            if (epilog_iterations != 0)
            {
                Nodecl::NodeclBase net_epilog_node;
                Nodecl::ForStatement for_stmt_epilog = simd_node_epilog.get_statement().as<Nodecl::ForStatement>();
                _vectorizer.process_epilog(for_stmt_epilog,
                        for_environment,
                        net_epilog_node,
                        epilog_iterations,
                        only_epilog,
                        false /*parallel loop */);
 
                // Remove Simd node from epilog
                simd_node_epilog.replace(simd_node_epilog.get_statement());

            }
            else // Remove epilog
            {
                Nodecl::Utils::remove_from_enclosing_list(simd_node_epilog);
            }

            // Remove Simd node from for_statement
            simd_node.replace(for_statement);

            // For statement is not necessary
            if (only_epilog)
            {
                Nodecl::Utils::remove_from_enclosing_list(simd_node);
            }
            else
            {
                // Unroll clause
                int unroll_clause_arg = process_unroll_clause(simd_environment);
                if (unroll_clause_arg > 0)
                {
                    std::stringstream unroll_pragma_strm;
                    unroll_pragma_strm << "unroll(";
                    unroll_pragma_strm << unroll_clause_arg;
                    unroll_pragma_strm << ")";

                    Nodecl::UnknownPragma unroll_pragma =
                        Nodecl::UnknownPragma::make(unroll_pragma_strm.str());

                    simd_node.prepend_sibling(unroll_pragma);
                }

                // Unroll and Jam clause
                int unroll_and_jam_clause_arg = process_unroll_and_jam_clause(simd_environment);
                if (unroll_and_jam_clause_arg > 0)
                {
                    std::stringstream unroll_and_jam_pragma_strm;
                    unroll_and_jam_pragma_strm << "unroll_and_jam(";
                    unroll_and_jam_pragma_strm << unroll_and_jam_clause_arg;
                    unroll_and_jam_pragma_strm << ")";

                    Nodecl::UnknownPragma unroll_and_jam_pragma =
                        Nodecl::UnknownPragma::make(unroll_and_jam_pragma_strm.str());

                    simd_node.prepend_sibling(unroll_and_jam_pragma);
                }
            }

            // Free analysis
            _vectorizer.finalize_analysis();

            // Final optimisation step
//            TL::Optimizations::canonicalize_and_fold(for_statement, _fast_math_enabled);
//            TL::Optimizations::strength_reduce(for_statement, _fast_math_enabled);
        }

        void SimdVisitor::visit(const Nodecl::OpenMP::SimdFor& simd_node)
        {
            Nodecl::OpenMP::For omp_for = simd_node.get_openmp_for().as<Nodecl::OpenMP::For>();
            Nodecl::List omp_simd_for_environment = simd_node.get_environment().as<Nodecl::List>();
            Nodecl::List omp_for_environment = omp_for.get_environment().as<Nodecl::List>();

            // Skipping AST_LIST_NODE
            Nodecl::NodeclBase loop = omp_for.get_loop();
            ERROR_CONDITION(!loop.is<Nodecl::ForStatement>(), 
                    "Unexpected node %s. Expecting a ForStatement after '#pragma omp simd for'", 
                    ast_print_node_type(loop.get_kind()));

            Nodecl::ForStatement for_statement = loop.as<Nodecl::ForStatement>();

            // Aligned clause
            std::map<TL::Symbol, int> aligned_expressions;
            process_aligned_clause(omp_simd_for_environment, aligned_expressions);

            // Suitable clause
            TL::ObjectList<Nodecl::NodeclBase> suitable_expressions;
            process_suitable_clause(omp_simd_for_environment, suitable_expressions);

            // Nontemporal clause
            TL::ObjectList<Nodecl::NodeclBase> nontemporal_expressions;
            process_nontemporal_clause(omp_simd_for_environment, nontemporal_expressions);

            // Cache clause
            TL::ObjectList<Nodecl::NodeclBase> cached_expressions;
            process_cache_clause(omp_simd_for_environment, cached_expressions);
            VectorizerCache vectorizer_cache(cached_expressions);

            // Vectorlengthfor clause
            TL::Type vectorlengthfor_type;
            process_vectorlengthfor_clause(omp_simd_for_environment, vectorlengthfor_type);

            // External symbols (loop)
            std::map<TL::Symbol, TL::Symbol> new_external_vector_symbol_map;

            // Reduction clause
            TL::ObjectList<TL::Symbol> reductions;
            Nodecl::List omp_reduction_list =
                process_reduction_clause(omp_for_environment, 
                        reductions, new_external_vector_symbol_map,
                        for_statement);

            // Vectorizer Environment
            VectorizerEnvironment for_environment(
                    _device_name,
                    _vector_length,
                    _support_masking, 
                    _mask_size,
                    _fast_math_enabled,
                    _prefer_gather_scatter,
                    _prefer_mask_gather_scatter,
                    vectorlengthfor_type,
                    aligned_expressions,
                    suitable_expressions,
                    nontemporal_expressions,
                    vectorizer_cache,
                    &reductions,
                    &new_external_vector_symbol_map);

            // Get code ready for vectorisation
            _vectorizer.preprocess_code(for_statement, for_environment);

            // Add epilog before vectorization
            Nodecl::OpenMP::SimdFor simd_node_epilog = Nodecl::Utils::deep_copy(
                    simd_node, simd_node).as<Nodecl::OpenMP::SimdFor>();

            simd_node.append_sibling(simd_node_epilog);

            // Initialize analysis info
            Nodecl::NodeclBase enclosing_func = 
                Nodecl::Utils::get_enclosing_function(for_statement).get_function_code();

            _vectorizer.initialize_analysis(enclosing_func.as<Nodecl::FunctionCode>());

            // Get epilog information
            bool only_epilog;
            int epilog_iterations = _vectorizer.get_epilog_info(for_statement, 
                    for_environment, only_epilog); 

            // Add scopes, default masks, etc.
            _vectorizer.load_environment(for_statement, for_environment);

            // Cache init
            vectorizer_cache.declare_cache_symbols(
                    for_statement.retrieve_context(), for_environment);
            simd_node.prepend_sibling(vectorizer_cache.get_init_statements(for_environment));

            // VECTORIZE FOR
            if(!only_epilog)
            {
                _vectorizer.vectorize(for_statement, for_environment); 
            }

            _vectorizer.unload_environment(for_environment);

            // Add new vector symbols
            Nodecl::List pre_for_nodecls, post_for_nodecls;

            if (!new_external_vector_symbol_map.empty())
            {
                // REDUCTIONS
                for(Nodecl::List::iterator it = omp_reduction_list.begin();
                        it != omp_reduction_list.end();
                        it++)
                {
                    // Prepare reduction information to be used in vectorizer
                    Nodecl::OpenMP::ReductionItem omp_red_item = (*it).as<Nodecl::OpenMP::ReductionItem>();
                    TL::OpenMP::Reduction omp_red = *(OpenMP::Reduction::get_reduction_info_from_symbol(
                                omp_red_item.get_reductor().get_symbol()));

                    // Symbols
                    std::map<TL::Symbol, TL::Symbol>::iterator new_external_symbol_pair = 
                        new_external_vector_symbol_map.find(omp_red_item.get_reduced_symbol().get_symbol());

                    TL::Symbol scalar_tl_symbol = new_external_symbol_pair->first;
                    TL::Symbol vector_tl_symbol = new_external_symbol_pair->second;

                    // Reduction info
                    Nodecl::NodeclBase reduction_initializer = omp_red.get_initializer();
                    std::string reduction_name = omp_red.get_name();
                    TL::Type reduction_type = omp_red.get_type();

                    // Vectorize reductions
                    if(_vectorizer.is_supported_reduction(
                                omp_red.is_builtin(reduction_name),
                                reduction_name,
                                reduction_type,
                                for_environment))
                    {
                        _vectorizer.vectorize_reduction(scalar_tl_symbol, 
                                vector_tl_symbol, 
                                reduction_initializer,
                                reduction_name,
                                reduction_type,
                                for_environment,
                                pre_for_nodecls,
                                post_for_nodecls);
                    }
                    else
                    {
                        running_error("SIMD: reduction '%s:%s' (%s) is not supported", 
                                reduction_name.c_str(), scalar_tl_symbol.get_name().c_str(),
                                reduction_type.get_simple_declaration(for_statement.retrieve_context(), "").c_str());
                    }
                }

                simd_node.prepend_sibling(pre_for_nodecls);
                // Final reduction after the epilog (to reduce also elements from masked epilogs)
                //single_epilog.append_sibling(post_for_nodecls);
            }

            Nodecl::List appendix_list;
            Nodecl::NodeclBase net_epilog_node;
            Nodecl::ForStatement epilog_for_statement;

            // Process epilog
            if (epilog_iterations != 0)
            {
                epilog_for_statement = simd_node_epilog.get_openmp_for().as<Nodecl::OpenMP::For>().
                        get_loop().as<Nodecl::ForStatement>();

                _vectorizer.process_epilog(epilog_for_statement,
                        for_environment,
                        net_epilog_node,
                        epilog_iterations,
                        only_epilog,
                        true /*parallel loop*/);

                // SINGLE
                Nodecl::List single_environment;
                // Create single node
                Nodecl::OpenMP::Single single_epilog =
                    Nodecl::OpenMP::Single::make(single_environment,
                            Nodecl::List::make(net_epilog_node.shallow_copy()), 
                            net_epilog_node.get_locus());

                net_epilog_node.replace(single_epilog);

                // Move single_epilog to its final position
                appendix_list.append(epilog_for_statement);
                //appendix_list.append(net_epilog_node);
            }

            // Remove epilog from original code
            Nodecl::Utils::remove_from_enclosing_list(simd_node_epilog);

            if(!post_for_nodecls.empty())
                appendix_list.append(post_for_nodecls);

            Nodecl::NodeclBase for_epilog;

            // For statement is not necessary
            if (only_epilog)
            {
                for_epilog = appendix_list;
            }
            else
            {
                // ForAppendix only if appendix is not empty
                if (!appendix_list.empty())
                {
                    for_epilog = 
                        Nodecl::OpenMP::ForAppendix::make(omp_for_environment.shallow_copy(),
                                loop.shallow_copy(),
                                appendix_list,
                                omp_for.get_locus());
                }
                else
                {
                    for_epilog = 
                        Nodecl::OpenMP::For::make(omp_for_environment.shallow_copy(),
                                loop.shallow_copy(),
                                omp_for.get_locus());
                }
            }

            // Remove Barrier and Flush from inner omp for
            Nodecl::NodeclBase barrier = omp_for_environment.find_first<Nodecl::OpenMP::BarrierAtEnd>();
            Nodecl::NodeclBase flush = omp_for_environment.find_first<Nodecl::OpenMP::FlushAtExit>();

            if (!barrier.is_null())
                Nodecl::Utils::remove_from_enclosing_list(barrier);

            if (!flush.is_null())
                Nodecl::Utils::remove_from_enclosing_list(flush);

            // Remove Simd nodes
            simd_node.replace(for_epilog);

            // Free analysis
            _vectorizer.finalize_analysis();

            // Final optimisation step
//            TL::Optimizations::canonicalize_and_fold(for_epilog, _fast_math_enabled);
//            TL::Optimizations::strength_reduce(for_epilog, _fast_math_enabled);
        }

        void SimdVisitor::visit(const Nodecl::OpenMP::SimdFunction& simd_node)
        {
            Nodecl::FunctionCode function_code = simd_node.get_statement()
                .as<Nodecl::FunctionCode>();

            Nodecl::List omp_environment = simd_node.get_environment().as<Nodecl::List>();

            // Remove SimdFunction node
            simd_node.replace(function_code);

            // Aligned clause
            std::map<TL::Symbol, int> aligned_expressions;
            process_aligned_clause(omp_environment, aligned_expressions);

            // Suitable clause
            TL::ObjectList<Nodecl::NodeclBase> suitable_expressions;
            process_suitable_clause(omp_environment, suitable_expressions);

            // Nontemporal clause
            TL::ObjectList<Nodecl::NodeclBase> nontemporal_expressions;
            process_nontemporal_clause(omp_environment, nontemporal_expressions);

            // Cache clause
            TL::ObjectList<Nodecl::NodeclBase> cached_expressions;
            process_cache_clause(omp_environment, cached_expressions);
            VectorizerCache vectorizer_cache(cached_expressions);

            // Vectorlengthfor clause
            TL::Type vectorlengthfor_type;
            process_vectorlengthfor_clause(omp_environment, vectorlengthfor_type);


            Nodecl::OpenMP::Mask omp_mask = omp_environment.find_first<Nodecl::OpenMP::Mask>();
            Nodecl::OpenMP::NoMask omp_nomask = omp_environment.find_first<Nodecl::OpenMP::NoMask>();

            if((!omp_mask.is_null()) && (!omp_nomask.is_null()))
            {
                running_error("SIMD: 'mask' and 'nomask' clauses are now allowed at the same time\n");
            } 

            if((!omp_mask.is_null()) && (!_support_masking))
            {
                running_error("SIMD: 'mask' clause detected. Masking is not supported by the underlying architecture\n");
            } 

            // Vectorizer Environment
            VectorizerEnvironment function_environment(
                    _device_name,
                    _vector_length, 
                    _support_masking,
                    _mask_size,
                    _prefer_gather_scatter,
                    _prefer_mask_gather_scatter,
                    _fast_math_enabled,
                    vectorlengthfor_type,
                    aligned_expressions,
                    suitable_expressions,
                    nontemporal_expressions,
                    vectorizer_cache,
                    NULL,
                    NULL);

            // Mask Version
            if (_support_masking && omp_nomask.is_null())
            {
                common_simd_function(simd_node, 
                        function_code, 
                        function_environment, 
                        true);
            }
            // Nomask Version
            if (omp_mask.is_null())
            {
                common_simd_function(simd_node, 
                        function_code,
                        function_environment,
                        false);
            }
        }

        void SimdVisitor::common_simd_function(const Nodecl::OpenMP::SimdFunction& simd_node,
                const Nodecl::FunctionCode& function_code,
                Vectorization::VectorizerEnvironment& function_environment,
                const bool masked_version)
        {
            // Get code ready for vectorisation
            _vectorizer.preprocess_code(function_code, function_environment);

            TL::Symbol func_sym = function_code.get_symbol();
            std::string orig_func_name = func_sym.get_name();

            // Set new vector function symbol
            std::stringstream vector_func_name; 

            vector_func_name <<"__" 
                << orig_func_name
                << "_" 
                << _device_name 
                << "_" 
                << _vector_length
                ;

            if (masked_version)
            {
                vector_func_name << "_mask";
            }

            TL::Symbol new_func_sym = func_sym.get_scope().
                new_symbol(vector_func_name.str());
            new_func_sym.get_internal_symbol()->kind = SK_FUNCTION;

            Nodecl::Utils::SimpleSymbolMap func_sym_map;
            func_sym_map.add_map(func_sym, new_func_sym);

            Nodecl::FunctionCode vector_func_code = 
                Nodecl::Utils::deep_copy(function_code, 
                        function_code,
                        func_sym_map).as<Nodecl::FunctionCode>();

            FunctionDeepCopyFixVisitor fix_deep_copy_visitor(func_sym, new_func_sym);
            fix_deep_copy_visitor.walk(vector_func_code.get_statements());

            // Add SIMD version to vector function versioning
            _vectorizer.add_vector_function_version(orig_func_name, vector_func_code, 
                    _device_name, _vector_length, func_sym.get_type().returns(), masked_version, 
                    TL::Vectorization::SIMD_FUNC_PRIORITY, false);

            // Append vectorized function code to scalar function
            simd_node.append_sibling(vector_func_code);

            // Initialize analysis info
            _vectorizer.initialize_analysis(vector_func_code);

            _vectorizer.vectorize(vector_func_code, function_environment, masked_version); 

            // Free analysis
            _vectorizer.finalize_analysis();

            // Final optimisation step
//            TL::Optimizations::canonicalize_and_fold(vector_func_code, _fast_math_enabled);
//            TL::Optimizations::strength_reduce(vector_func_code, _fast_math_enabled);
        }

        void SimdVisitor::process_aligned_clause(const Nodecl::List& environment,
                std::map<TL::Symbol, int>& aligned_expressions_map)
        {
            TL::ObjectList<Nodecl::OpenMP::Aligned> omp_aligned_list = 
                environment.find_all<Nodecl::OpenMP::Aligned>();

            for(TL::ObjectList<Nodecl::OpenMP::Aligned>::iterator it = omp_aligned_list.begin();
                    it != omp_aligned_list.end();
                    it++)
            {
                Nodecl::OpenMP::Aligned& omp_aligned = *it;

                if(!omp_aligned.is_null())
                {
                    TL::ObjectList<Nodecl::NodeclBase> aligned_expressions_list = 
                        omp_aligned.get_aligned_expressions().as<Nodecl::List>().to_object_list();

                    int alignment = const_value_cast_to_signed_int(
                            omp_aligned.get_alignment().as<Nodecl::IntegerLiteral>().get_constant());

                    for(TL::ObjectList<Nodecl::NodeclBase>::iterator it = aligned_expressions_list.begin();
                            it != aligned_expressions_list.end();
                            it++)
                    {

                        if(!aligned_expressions_map.insert(std::pair<TL::Symbol, int>(
                                        it->as<Nodecl::Symbol>().get_symbol(), alignment)).second)
                        {
                            running_error("SIMD: multiple instances of the same variable in the 'aligned' clause detectedn\n");
                        }
                    }
                }
            }
        }

        void SimdVisitor::process_suitable_clause(const Nodecl::List& environment,
                TL::ObjectList<Nodecl::NodeclBase>& suitable_expressions)
        {
            Nodecl::OpenMP::Suitable omp_suitable =
                environment.find_first<Nodecl::OpenMP::Suitable>();

            if(!omp_suitable.is_null())
            {
                suitable_expressions = omp_suitable.get_suitable_expressions().
                    as<Nodecl::List>().to_object_list();
            }
        }

        void SimdVisitor::process_nontemporal_clause(const Nodecl::List& environment,
                TL::ObjectList<Nodecl::NodeclBase>& nontemporal_expressions)
        {
            Nodecl::OpenMP::Nontemporal omp_nontemporal = 
                environment.find_first<Nodecl::OpenMP::Nontemporal>();

            if(!omp_nontemporal.is_null())
            {
                nontemporal_expressions = omp_nontemporal.get_nontemporal_expressions().
                    as<Nodecl::List>().to_object_list();
            }
        }

        int SimdVisitor::process_unroll_clause(const Nodecl::List& environment)
        {
            Nodecl::OpenMP::Unroll omp_unroll = 
                environment.find_first<Nodecl::OpenMP::Unroll>();

            if(!omp_unroll.is_null())
            {
                Nodecl::NodeclBase unroll_factor = omp_unroll.get_unroll_factor();

                if (unroll_factor.is_constant())
                {
                    return const_value_cast_to_4(unroll_factor.get_constant());
                }
            }

            return 0;
        }

        int SimdVisitor::process_unroll_and_jam_clause(const Nodecl::List& environment)
        {
            Nodecl::OpenMP::UnrollAndJam omp_unroll = 
                environment.find_first<Nodecl::OpenMP::UnrollAndJam>();

            if(!omp_unroll.is_null())
            {
                Nodecl::NodeclBase unroll_factor = omp_unroll.get_unroll_factor();

                if (unroll_factor.is_constant())
                {
                    return const_value_cast_to_4(unroll_factor.get_constant());
                }
            }

            return 0;
        }

        void SimdVisitor::process_vectorlengthfor_clause(const Nodecl::List& environment, 
                TL::Type& vectorlengthfor_type)
        {
            Nodecl::OpenMP::VectorLengthFor omp_vector_length_for = 
                environment.find_first<Nodecl::OpenMP::VectorLengthFor>();

            if(!omp_vector_length_for.is_null())
            {
                vectorlengthfor_type = omp_vector_length_for.get_type();
            }
            else //Float type by default //TODO
            {
                vectorlengthfor_type = TL::Type::get_float_type();
            }
        }

        void SimdVisitor::process_cache_clause(const Nodecl::List& environment,
                TL::ObjectList<Nodecl::NodeclBase>& cached_expressions)
        {
            Nodecl::OpenMP::Cache omp_cache = 
                environment.find_first<Nodecl::OpenMP::Cache>();

            if(!omp_cache.is_null())
            {
                cached_expressions = omp_cache.get_cached_expressions().
                    as<Nodecl::List>().to_object_list();
            }
        }

        Nodecl::List SimdVisitor::process_reduction_clause(const Nodecl::List& environment,
                TL::ObjectList<TL::Symbol>& reductions,
                std::map<TL::Symbol, TL::Symbol>& new_external_vector_symbol_map,
                const Nodecl::ForStatement& for_statement)
        {
            Nodecl::List omp_reduction_list;

            for(Nodecl::List::const_iterator it = environment.begin();
                    it != environment.end();
                    it++)
            {
                if(it->is<Nodecl::OpenMP::Reduction>() ||
                        it->is<Nodecl::OpenMP::SimdReduction>())
                {
                    Nodecl::OpenMP::Reduction omp_reductions = 
                        it->as<Nodecl::OpenMP::Reduction>();

                    // Extract reduced Nodecl::Symbol from ReductionItems
                    omp_reduction_list = omp_reductions.get_reductions().as<Nodecl::List>();
                    for(Nodecl::List::iterator it2 = omp_reduction_list.begin();
                            it2 != omp_reduction_list.end();
                            it2++ )
                    {
                        TL::Symbol red_sym = it2->as<Nodecl::OpenMP::ReductionItem>().
                            get_reduced_symbol().as<Nodecl::Symbol>().get_symbol();

                        reductions.append(red_sym);

                        // Add new vector TL::Symbol in the enclosing context
                        TL::Symbol new_red_sym =
                            for_statement.retrieve_context().new_symbol("__vred_" + red_sym.get_name());
                        new_red_sym.get_internal_symbol()->kind = SK_VARIABLE;
                        new_red_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
                        new_red_sym.set_type(red_sym.get_type().get_vector_to(_vector_length));

                        // Add new TL::Symbol to map
                        new_external_vector_symbol_map.insert(std::pair<TL::Symbol, TL::Symbol>(
                                    red_sym, new_red_sym));
                    }
                }
            }

            return omp_reduction_list;
        }

        FunctionDeepCopyFixVisitor::FunctionDeepCopyFixVisitor(const TL::Symbol& orig_symbol, const TL::Symbol& new_symbol)
            : _orig_symbol(orig_symbol), _new_symbol(new_symbol)
        {
        }

        void FunctionDeepCopyFixVisitor::visit(const Nodecl::Symbol& n)
        {
            if (n.get_symbol() == _new_symbol)
            {
                n.replace(_orig_symbol.make_nodecl(false, n.get_locus()));
            }
        }
    }
}

EXPORT_PHASE(TL::OpenMP::Simd)
