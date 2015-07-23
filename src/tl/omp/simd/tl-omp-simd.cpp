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

#include "tl-omp-simd.hpp"
#include "tl-vectorizer-target-type-heuristic.hpp"

#include "tl-vectorization-common.hpp"
#include "tl-omp.hpp"
#include "tl-optimizations.hpp"
#include "tl-counters.hpp"

#include "hlt-loop-unroll.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-cexpr.h"

using namespace TL::Vectorization;

namespace TL {
    namespace OpenMP {

        Simd::Simd()
            : PragmaCustomCompilerPhase("omp-simd"),
            _simd_enabled(false), _svml_enabled(false), _fast_math_enabled(false),
            _avx2_enabled(false), _knc_enabled(false), _knl_enabled(false),
            _spml_enabled(false), _only_adjacent_accesses_enabled(false), _overlap_in_place(false)
        {
            set_phase_name("Vectorize OpenMP SIMD parallel IR");
            set_phase_description("This phase vectorize the OpenMP SIMD parallel IR");

            register_parameter("simd_enabled",
                    "If set to '1' enables simd constructs, otherwise it is disabled",
                    _simd_enabled_str,
                    "0").connect(std::bind(&Simd::set_simd, this, std::placeholders::_1));

            register_parameter("svml_enabled",
                    "If set to '1' enables svml math library, otherwise it is disabled",
                    _svml_enabled_str,
                    "0").connect(std::bind(&Simd::set_svml, this, std::placeholders::_1));

            register_parameter("fast_math_enabled",
                    "If set to '1' enables fast_math operations, otherwise it is disabled",
                    _fast_math_enabled_str,
                    "0").connect(std::bind(&Simd::set_fast_math, this, std::placeholders::_1));

            register_parameter("mic_enabled",
                    "If set to '1' enables compilation for KNC architecture, otherwise it is disabled",
                    _knc_enabled_str,
                    "0").connect(std::bind(&Simd::set_knc, this, std::placeholders::_1));
            register_parameter("knl_enabled",
                    "If set to '1' enables compilation for KNL architecture, otherwise it is disabled",
                    _knl_enabled_str,
                    "0").connect(std::bind(&Simd::set_knl, this, std::placeholders::_1));

            register_parameter("avx2_enabled",
                    "If set to '1' enables compilation for AVX2 instruction set, otherwise it is disabled",
                    _avx2_enabled_str,
                    "0").connect(std::bind(&Simd::set_avx2, this, std::placeholders::_1));

            register_parameter("spml_enabled",
                    "If set to '1' enables SPML OpenMP mode, otherwise it is disabled",
                    _spml_enabled_str,
                    "0").connect(std::bind(&Simd::set_spml, this, std::placeholders::_1));

            register_parameter("only_adjacent_accesses",
                    "If set to '1' disables emission of gather/scatter vector instructions",
                    _only_adjacent_accesses_str,
                    "0").connect(std::bind(&Simd::set_only_adjcent_accesses, this, std::placeholders::_1));

            register_parameter("overlap_in_place",
                    "Enables overlap register cache update in place and not at the beginning of the BB",
                    _overlap_in_place_str,
                    "0").connect(std::bind(&Simd::set_overlap_in_place, this, std::placeholders::_1));

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

        void Simd::set_knl(const std::string knl_enabled_str)
        {
            if (knl_enabled_str == "1")
            {
                _knl_enabled = true;
            }
        }

        void Simd::set_avx2(const std::string avx2_enabled_str)
        {
            if (avx2_enabled_str == "1")
            {
                _avx2_enabled = true;
            }
        }

        void Simd::set_spml(const std::string spml_enabled_str)
        {
            if (spml_enabled_str == "1")
            {
                _spml_enabled = true;
            }
        }

        void Simd::set_only_adjcent_accesses(
                const std::string only_adjacent_accesses_str)
        {
            if (only_adjacent_accesses_str == "1")
            {
                _only_adjacent_accesses_enabled = true;
            }
        }

        void Simd::set_overlap_in_place(const std::string overlap_in_place_str)
        {
            if (overlap_in_place_str == "1")
            {
                _overlap_in_place = true;
            }
        }

        void Simd::pre_run(TL::DTO& dto)
        {
            this->PragmaCustomCompilerPhase::pre_run(dto);
        }

        void Simd::run(TL::DTO& dto)
        {
            this->PragmaCustomCompilerPhase::run(dto);

            Nodecl::NodeclBase translation_unit = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

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
                else if (_knl_enabled)
                {
                    simd_isa = KNL_ISA;
                }
                else
                {
                    simd_isa = SSE4_2_ISA;
                }

                if (_avx2_enabled && _knc_enabled)
                {
                    running_error("SIMD: AVX2 and KNC SIMD instruction sets enabled at the same time");
                }
                else if (_knl_enabled && _knc_enabled)
                {
                    running_error("SIMD: KNL and KNC SIMD instruction sets enabled at the same time");
                }
                else if (_avx2_enabled && _knl_enabled)
                {
                    running_error("SIMD: AVX2 and KNL SIMD instruction sets enabled at the same time");
                }


                if (_spml_enabled)
                {
                    fprintf(stderr, " -- SPML OpenMP enabled -- \n");
                    SimdSPMLVisitor spml_visitor(
                            simd_isa, _fast_math_enabled, _svml_enabled,
                            _only_adjacent_accesses_enabled, _overlap_in_place);
                    spml_visitor.walk(translation_unit);
                }
                else
                {
                    SimdPreregisterVisitor simd_preregister_visitor(simd_isa, _fast_math_enabled, _svml_enabled,
                            _only_adjacent_accesses_enabled, _overlap_in_place);
                    simd_preregister_visitor.walk(translation_unit);

                    SimdVisitor simd_visitor(simd_isa, _fast_math_enabled, _svml_enabled,
                            _only_adjacent_accesses_enabled, _overlap_in_place);
                    simd_visitor.walk(translation_unit);
                }
            }
        }

        void Simd::phase_cleanup(TL::DTO& dto)
        {
        }

        SimdProcessingBase::SimdProcessingBase(Vectorization::SIMDInstructionSet simd_isa,
                bool fast_math_enabled, bool svml_enabled,
                bool only_adjacent_accesses,
                bool overlap_in_place)
            : _vectorizer(TL::Vectorization::Vectorizer::get_vectorizer()), _fast_math_enabled(fast_math_enabled),
                    _overlap_in_place(overlap_in_place)
        {
            if (fast_math_enabled)
            {
                _vectorizer.enable_fast_math();
            }

            if (only_adjacent_accesses)
            {
                _vectorizer.disable_gathers_scatters();
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

                case KNL_ISA:
                    _vector_length = 64;
                    _device_name = "knl";
                    _support_masking = true;
                    _mask_size = 16;

                    if (svml_enabled)
                        _vectorizer.enable_svml_knl();
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

        SimdPreregisterVisitor::SimdPreregisterVisitor(Vectorization::SIMDInstructionSet simd_isa,
                bool fast_math_enabled, bool svml_enabled,
                bool only_adjacent_accesses, bool overlap_in_place)
            : SimdProcessingBase(simd_isa, fast_math_enabled, svml_enabled, only_adjacent_accesses, overlap_in_place)
        {
        }

        SimdPreregisterVisitor::~SimdPreregisterVisitor()
        {
            _vectorizer.finalize_analysis();
        }

        SimdVisitor::SimdVisitor(Vectorization::SIMDInstructionSet simd_isa,
                bool fast_math_enabled, bool svml_enabled,
                bool only_adjacent_accesses, bool overlap_in_place)
            : SimdProcessingBase(simd_isa, fast_math_enabled, svml_enabled, only_adjacent_accesses, overlap_in_place)
        {
        }

        SimdVisitor::~SimdVisitor()
        {
            _vectorizer.finalize_analysis();
        }

        void SimdVisitor::visit(const Nodecl::TemplateFunctionCode& n)
        {
            // TODO: Do nothing
        }

        void SimdVisitor::visit(const Nodecl::FunctionCode& n)
        {
            // Note that SimdFunction is treated specially in its visit

            // TODO::Improve! 

            TL::ObjectList<Nodecl::NodeclBase> omp_simd_list = 
                Nodecl::Utils::nodecl_get_all_nodecls_of_kind<Nodecl::OpenMP::Simd>(n);
            TL::ObjectList<Nodecl::NodeclBase> omp_simd_for_list = 
                Nodecl::Utils::nodecl_get_all_nodecls_of_kind<Nodecl::OpenMP::SimdFor>(n);
            TL::ObjectList<Nodecl::NodeclBase> omp_simd_parallel_list =
                Nodecl::Utils::nodecl_get_all_nodecls_of_kind<Nodecl::OpenMP::SimdParallel>(n);

            for (const auto& node : omp_simd_list) _vectorizer.preprocess_code(node);
            for (const auto& node : omp_simd_for_list) _vectorizer.preprocess_code(node);
            for (const auto& node : omp_simd_parallel_list) _vectorizer.preprocess_code(node);

            if (!omp_simd_list.empty()
                    || !omp_simd_for_list.empty()
                    || !omp_simd_parallel_list.empty())
            {
                _vectorizer.initialize_analysis(n);
                walk(n.get_statements());
            }

            for (const auto& node : omp_simd_list) _vectorizer.postprocess_code(node);
            for (const auto& node : omp_simd_for_list) _vectorizer.postprocess_code(node);
            for (const auto& node : omp_simd_parallel_list) _vectorizer.postprocess_code(node);
        }

        void SimdVisitor::visit(const Nodecl::OpenMP::Simd& simd_input_node)
        {
            Nodecl::NodeclBase simd_enclosing_node = simd_input_node.get_parent();
            Nodecl::OpenMP::Simd simd_node_main_loop = simd_input_node.shallow_copy().
                as<Nodecl::OpenMP::Simd>();
            Nodecl::NodeclBase loop_statement = simd_node_main_loop.get_statement();
            Nodecl::List simd_environment = simd_node_main_loop.get_environment().
                as<Nodecl::List>();

            // Aligned clause
            map_tlsym_int_t aligned_expressions;
            process_aligned_clause(simd_environment, aligned_expressions);

            // Linear clause
            map_tlsym_int_t linear_symbols;
            process_linear_clause(simd_environment, linear_symbols);

            // Uniform clause
            objlist_tlsym_t uniform_symbols;
            process_uniform_clause(simd_environment, uniform_symbols);

            // Suitable clause
            objlist_nodecl_t suitable_expressions;
            process_suitable_clause(simd_environment, suitable_expressions);

            // Nontemporal clause
            map_tlsym_objlist_t nontemporal_expressions;
            process_nontemporal_clause(simd_environment, nontemporal_expressions);

            // Vectorlengthfor clause
            TL::Type vectorlengthfor_type;
            process_vectorlengthfor_clause(simd_environment,
                    vectorlengthfor_type);

            // Overlap clause
            map_tlsym_objlist_int_t overlap_symbols;
            process_overlap_clause(simd_environment, overlap_symbols);
            
            // Unroll clause
            int unroll_clause_arg = process_unroll_clause(simd_environment);
            bool loop_unrolled = false;
 
            // Prefetch clause
            Vectorization::prefetch_info_t prefetch_info;
            process_prefetch_clause(simd_environment, prefetch_info);


            // External symbols (loop)
            std::map<TL::Symbol, TL::Symbol> new_external_vector_symbol_map;

            // Reduction and simd_reduction clauses
            objlist_tlsym_t reductions;

            Nodecl::List omp_reduction_list = process_reduction_clause(
                    simd_environment, reductions, 
                    new_external_vector_symbol_map,
                    simd_enclosing_node.retrieve_context());

            // Vectorizer Environment
            VectorizerEnvironment loop_environment(
                    _device_name,
                    _vector_length,
                    _support_masking,
                    _mask_size,
                    _fast_math_enabled,
                    vectorlengthfor_type,
                    aligned_expressions,
                    linear_symbols,
                    uniform_symbols,
                    suitable_expressions,
                    nontemporal_expressions,
                    overlap_symbols,
                    &reductions,
                    &new_external_vector_symbol_map);

            // Add scopes, default masks, etc.
            loop_environment.load_environment(loop_statement);
            // Set target type
            if (!loop_environment._target_type.is_valid())
            {
                VectorizerTargetTypeHeuristic target_type_heuristic;
                loop_environment.set_target_type(
                        target_type_heuristic.get_target_type(loop_statement));
            }

            // Add epilog before vectorization
            Nodecl::OpenMP::Simd simd_node_epilog = Nodecl::Utils::deep_copy(
                    simd_node_main_loop, simd_enclosing_node)
                .as<Nodecl::OpenMP::Simd>();

            // OUTPUT CODE STRUCTURE
            Nodecl::List output_code_list;
            output_code_list.append(simd_node_main_loop);// Main For
            output_code_list.append(simd_node_epilog);   // Epilog
            
            // Register new simd nodes in analysis
            Vectorization::Vectorizer::_vectorizer_analysis->register_identical_copy(
                    simd_input_node, simd_node_main_loop);
            Vectorization::Vectorizer::_vectorizer_analysis->register_identical_copy(
                    simd_input_node, simd_node_epilog);

            Nodecl::CompoundStatement output_code =
                Nodecl::CompoundStatement::make(
                        output_code_list, Nodecl::NodeclBase::null());

            // Replace input code by output and update output pointer
            simd_input_node.replace(output_code);
            output_code = simd_input_node.as<Nodecl::CompoundStatement>();
            
            // Get epilog information
            bool only_epilog;
            int epilog_iterations = _vectorizer.get_epilog_info(loop_statement,
                    loop_environment, only_epilog);


            // MAIN LOOP VECTORIZATION
            if (!only_epilog)
            {
                _vectorizer.vectorize_loop(
                        loop_statement, loop_environment);

                if (!loop_environment._overlap_symbols_map.empty())
                {

                    // If unroll clause and overlap clause, apply unroll before overlap
                    if (unroll_clause_arg > 0)
                    {
                        TL::HLT::LoopUnroll loop_unroller;
                        loop_unroller.set_loop(loop_statement)
                            .set_unroll_factor(unroll_clause_arg)
                            .unroll();

                        //Nodecl::NodeclBase whole_main_transformation =
                        //    loop_unroller.get_whole_transformation();

                        // Main loop is now the the unrolled version of 'n'
                        loop_statement.replace(loop_unroller.get_unrolled_loop());

                        //last_epilog = loop_unroller.get_epilog_loop()
                        //    .as<Nodecl::ForStatement>();
                        std::cerr << "Vectorized Loop Unrolled with UF=" << unroll_clause_arg << std::endl;

                        loop_unrolled = true;
                    }


                    Nodecl::List prependix;
                    
                    _vectorizer.opt_overlapped_accesses(
                            loop_statement, loop_environment,
                            false /* simd for */, false /* epilog */,
                            _overlap_in_place, prependix);

                    loop_statement.prepend_sibling(prependix);
                }

                if(prefetch_info.enabled)
                    _vectorizer.prefetcher(loop_statement,
                            prefetch_info, loop_environment);
            }

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
                                loop_environment))
                    {
                        _vectorizer.vectorize_reduction(scalar_tl_symbol,
                                vector_tl_symbol,
                                reduction_initializer,
                                reduction_name,
                                reduction_type,
                                loop_environment,
                                pre_for_nodecls,
                                post_for_nodecls);
                    }
                    else
                    {
                        running_error("SIMD: reduction '%s:%s' is not supported",
                                reduction_name.c_str(), scalar_tl_symbol.get_name().c_str());
                    }
                }

                output_code_list.prepend(pre_for_nodecls);
                // Final reduction after the epilog (to reduce also elements from masked epilogs)
                output_code_list.append(post_for_nodecls);

                // TODO:
                // firstprivate in SIMD
            }

            loop_environment.unload_environment();

            // Process epilog
            if (epilog_iterations != 0)
            {
                Nodecl::NodeclBase net_epilog_node;
                Nodecl::NodeclBase loop_stmt_epilog = simd_node_epilog.
                    get_statement();

                // Load environment epilog
                loop_environment.load_environment(loop_stmt_epilog);

                _vectorizer.process_epilog(loop_stmt_epilog,
                        loop_environment,
                        net_epilog_node,
                        epilog_iterations,
                        only_epilog,
                        false /*parallel loop */);

                // Reload environment
                // 'epilog_for_statement' could be no longer a ForStatement
                loop_environment.unload_environment();
                loop_environment.load_environment(net_epilog_node);

                // Overlap
                if (!loop_environment._overlap_symbols_map.empty())
                {
                    Nodecl::List prependix;
                    _vectorizer.opt_overlapped_accesses(net_epilog_node,
                            loop_environment, false /* simd for */,
                            true /* epilog */, _overlap_in_place,
                            prependix);

                    ERROR_CONDITION(!prependix.empty(),
                            "Prependix is not empty in the epilogue loop", 0);
                }

                // 2nd step of transformation on epilog loop
                _vectorizer.clean_up_epilog(net_epilog_node,
                        loop_environment,
                        epilog_iterations,
                        only_epilog,
                        true /*parallel loop*/);

                // Remove Simd node from epilog
                simd_node_epilog.replace(simd_node_epilog.get_statement());

                loop_environment.unload_environment();
            }
            else // Remove epilog
            {
                Nodecl::Utils::remove_from_enclosing_list(simd_node_epilog);
            }

            // For statement is not necessary
            if (only_epilog)
            {
                Nodecl::Utils::remove_from_enclosing_list(simd_node_main_loop);
            }
            else
            {
                // Remove Simd node from loop_statement
                simd_node_main_loop.replace(loop_statement);

               if (!loop_unrolled && unroll_clause_arg > 0)
                {
                    std::stringstream unroll_pragma_strm;
                    unroll_pragma_strm << "unroll(";
                    unroll_pragma_strm << unroll_clause_arg;
                    unroll_pragma_strm << ")";

                    Nodecl::UnknownPragma unroll_pragma =
                        Nodecl::UnknownPragma::make(unroll_pragma_strm.str());

                    simd_node_main_loop.prepend_sibling(unroll_pragma);
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

                    simd_node_main_loop.prepend_sibling(unroll_and_jam_pragma);
                }
            }
        }

        void SimdVisitor::visit(const Nodecl::OpenMP::SimdFor& simd_input_node)
        {
            Nodecl::NodeclBase simd_enclosing_node = simd_input_node.get_parent();

            // With SimdFor we don't need to use a CompoundStatement as output
            // because we need to use the special node ForAppendix
            Nodecl::OpenMP::SimdFor simd_node_for = simd_input_node; // No shallow_copy
            Nodecl::OpenMP::For omp_for = simd_node_for.get_openmp_for()
                .as<Nodecl::OpenMP::For>();

            Nodecl::List omp_simd_for_environment = simd_node_for.get_environment().
                as<Nodecl::List>();
            Nodecl::List omp_for_environment = omp_for.get_environment().
                as<Nodecl::List>();

            // Skipping AST_LIST_NODE
            Nodecl::NodeclBase loop_context = omp_for.get_loop();
            Nodecl::NodeclBase loop = loop_context.as<Nodecl::Context>().
                get_in_context().as<Nodecl::List>().front().as<Nodecl::ForStatement>();

            ERROR_CONDITION(!loop.is<Nodecl::ForStatement>(),
                    "Unexpected node %s. Expecting a ForStatement after '#pragma omp simd for'",
                    ast_print_node_type(loop.get_kind()));

            Nodecl::ForStatement for_statement = loop.as<Nodecl::ForStatement>();

            // Aligned clause
            map_tlsym_int_t aligned_expressions;
            process_aligned_clause(omp_simd_for_environment, aligned_expressions);

            // Linear clause
            map_tlsym_int_t linear_symbols;
            process_linear_clause(omp_simd_for_environment, linear_symbols);

            // Uniform clause
            objlist_tlsym_t uniform_symbols;
            process_uniform_clause(omp_simd_for_environment, uniform_symbols);

            // Suitable clause
            objlist_nodecl_t suitable_expressions;
            process_suitable_clause(omp_simd_for_environment, suitable_expressions);

            // Nontemporal clause
            map_tlsym_objlist_t nontemporal_expressions;
            process_nontemporal_clause(omp_simd_for_environment, nontemporal_expressions);

            // Unroll clause
            int unroll_clause_arg = process_unroll_clause(omp_simd_for_environment);
            //bool loop_unrolled = false;

            // Overlap clause
            map_tlsym_objlist_int_t overlap_symbols;
            process_overlap_clause(omp_simd_for_environment, overlap_symbols);

            // Prefetch clause
            Vectorization::prefetch_info_t prefetch_info;
            process_prefetch_clause(omp_simd_for_environment, prefetch_info);

            // Vectorlengthfor clause
            TL::Type vectorlengthfor_type;
            process_vectorlengthfor_clause(omp_simd_for_environment, vectorlengthfor_type);

            // External symbols (loop)
            std::map<TL::Symbol, TL::Symbol> new_external_vector_symbol_map;

            // Reduction clause
            objlist_tlsym_t reductions;
            Nodecl::List omp_reduction_list =
                process_reduction_clause(omp_for_environment,
                        reductions, new_external_vector_symbol_map,
                        simd_enclosing_node.retrieve_context());

            // Vectorizer Environment
            VectorizerEnvironment for_environment(
                    _device_name,
                    _vector_length,
                    _support_masking,
                    _mask_size,
                    _fast_math_enabled,
                    vectorlengthfor_type,
                    aligned_expressions,
                    linear_symbols,
                    uniform_symbols,
                    suitable_expressions,
                    nontemporal_expressions,
                    overlap_symbols,
                    &reductions,
                    &new_external_vector_symbol_map);

            Nodecl::List prependix_list;
            Nodecl::List appendix_list;

            // Add scopes, default masks, etc.
            for_environment.load_environment(for_statement);
            // Set target type
            if (!for_environment._target_type.is_valid())
            {
                VectorizerTargetTypeHeuristic target_type_heuristic;
                for_environment.set_target_type(
                        target_type_heuristic.get_target_type(for_statement));
            }

            // Add epilog before vectorization
            Nodecl::OpenMP::SimdFor simd_node_epilog = Nodecl::Utils::deep_copy(
                    simd_node_for, simd_enclosing_node).as<Nodecl::OpenMP::SimdFor>();

            simd_node_for.append_sibling(simd_node_epilog);

            // Register new simd nodes in analysis
            // Note that simd_node_for has not been shallow copied
            Vectorization::Vectorizer::_vectorizer_analysis->register_identical_copy(
                    simd_node_for, simd_node_epilog);

            // Get epilog information
            bool only_epilog;
            int epilog_iterations = _vectorizer.get_epilog_info(for_statement,
                    for_environment, only_epilog);

            // Overlap init
//            vectorizer_overlap.declare_overlap_symbols(
//                    simd_enclosing_node.retrieve_context(), for_environment);
//            simd_node_for.prepend_sibling(vectorizer_overlap.get_init_statements(for_environment));

            // VECTORIZE FOR
            if(!only_epilog)
            {
                _vectorizer.vectorize_loop(
                        for_statement, for_environment);

                if (!for_environment._overlap_symbols_map.empty())
                { 
                    _vectorizer.opt_overlapped_accesses(
                            for_statement, for_environment,
                            true /* simd for */, false /*epilog*/,
                            _overlap_in_place, prependix_list);
                }

                if (prefetch_info.enabled)
                {
                    _vectorizer.prefetcher(for_statement, prefetch_info, for_environment);

                    // Remove 'pragma noprefetch' and add it as a clause
                    Nodecl::NodeclBase previous_sibling = Nodecl::Utils::get_previous_sibling(for_statement);
                    if (!previous_sibling.is_null() && previous_sibling.is<Nodecl::UnknownPragma>() &&
                            previous_sibling.as<Nodecl::UnknownPragma>().get_text() == "noprefetch")
                    {
                        Nodecl::Utils::remove_from_enclosing_list(previous_sibling);
                        omp_for_environment.append(Nodecl::OpenMP::NoPrefetch::make());
                    }
                }

            }

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
                                reduction_type.get_simple_declaration(
                                    simd_enclosing_node.retrieve_context(), "").c_str());
                    }
                }

                simd_node_for.prepend_sibling(pre_for_nodecls);
                // Final reduction after the epilog (to reduce also elements from masked epilogs)
                //single_epilog.append_sibling(post_for_nodecls);
            }

            for_environment.unload_environment();

            Nodecl::NodeclBase net_epilog_node;
            Nodecl::ForStatement epilog_for_statement;

            // Process epilog
            if (epilog_iterations != 0)
            {
                epilog_for_statement = 
                    Nodecl::Utils::skip_contexts_and_lists(
                            simd_node_epilog.get_openmp_for().as<Nodecl::OpenMP::For>().
                            get_loop()).as<Nodecl::ForStatement>();
                // Add scopes, default masks, etc.
                for_environment.load_environment(epilog_for_statement);

                _vectorizer.process_epilog(epilog_for_statement,
                        for_environment,
                        net_epilog_node,
                        epilog_iterations,
                        only_epilog,
                        true /*parallel loop*/);

                // Reload environment
                // 'epilog_for_statement' could be no longer a ForStatement
                for_environment.unload_environment();
                for_environment.load_environment(net_epilog_node);

                Nodecl::List single_stmts_list;

                // Overlap
                if (!for_environment._overlap_symbols_map.empty())
                {
                    _vectorizer.opt_overlapped_accesses(net_epilog_node,
                            for_environment, true /* simd for */,
                            true /* epilog */, _overlap_in_place,
                            single_stmts_list);
                }

                // 2nd step of transformation on epilog loop
                _vectorizer.clean_up_epilog(net_epilog_node,
                        for_environment,
                        epilog_iterations,
                        only_epilog,
                        true /*parallel loop*/);

                single_stmts_list.append(net_epilog_node.shallow_copy());

                for_environment.unload_environment();

                // SINGLE
                Nodecl::List single_environment;
                // Create single node
                Nodecl::OpenMP::Single single_epilog =
                    Nodecl::OpenMP::Single::make(single_environment,
                            single_stmts_list,
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
                // Unroll the vectorized loop!
                if (unroll_clause_arg != 0)
                {
                    // Main Loop
                    TL::HLT::LoopUnroll loop_unroller;
                    loop_unroller.set_loop(for_statement)
                        .set_unroll_factor(unroll_clause_arg) 
                        .unroll();

                    Nodecl::NodeclBase unrolled_transformation =
                        loop_unroller.get_unrolled_loop();

                    std::cerr << "BEFORE: " << std::endl;
                    std::cerr << for_statement.prettyprint() << std::endl;

                    for_statement.replace(unrolled_transformation);

                    std::cerr << "AFTER: " << std::endl;
                    std::cerr << for_statement.prettyprint() << std::endl;

                }

                // ForAppendix only if appendix is not empty
                if (!appendix_list.empty() || !prependix_list.empty())
                {
                    for_epilog =
                        Nodecl::OpenMP::ForAppendix::make(omp_for_environment.shallow_copy(),
                                loop_context.shallow_copy(),
                                prependix_list,
                                appendix_list,
                                omp_for.get_locus());
                }
                else
                {
                    for_epilog =
                        Nodecl::OpenMP::For::make(omp_for_environment.shallow_copy(),
                                loop_context.shallow_copy(),
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
            simd_node_for.replace(for_epilog);

            // Free analysis
            //_vectorizer.finalize_analysis();
        }


        void SimdPreregisterVisitor::visit(const Nodecl::OpenMP::SimdFunction& simd_node)
        {
            Nodecl::FunctionCode function_code = simd_node.get_statement()
                .as<Nodecl::FunctionCode>();

            // Preprocess SimdFunction
            _vectorizer.preprocess_code(simd_node);
            _vectorizer.initialize_analysis(simd_node);

            Nodecl::List omp_environment = simd_node.get_environment().as<Nodecl::List>();

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

           // Mask Version
            if (_support_masking && omp_nomask.is_null())
            {
                common_simd_function_preregister(simd_node, true);
            }
            // Nomask Version
            if (omp_mask.is_null())
            {
                common_simd_function_preregister(simd_node, false);
            }

        }

        void SimdVisitor::visit(const Nodecl::OpenMP::SimdFunction& simd_node)
        {
            Nodecl::FunctionCode function_code = simd_node.get_statement()
                .as<Nodecl::FunctionCode>();

            // Preprocess SimdFunction
            _vectorizer.preprocess_code(simd_node);
            _vectorizer.initialize_analysis(simd_node);

            Nodecl::List omp_environment = simd_node.get_environment().as<Nodecl::List>();

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

           // Mask Version
            if (_support_masking && omp_nomask.is_null())
            {
                common_simd_function(simd_node, true);
            }
            // Nomask Version
            if (omp_mask.is_null())
            {
                common_simd_function(simd_node, false);
            }

            // Remove SimdFunction node
            simd_node.replace(function_code);
        }

        void SimdPreregisterVisitor::common_simd_function_preregister(
                const Nodecl::OpenMP::SimdFunction& simd_node,
                const bool masked_version)
        {
            Nodecl::FunctionCode function_code = simd_node.get_statement()
                .as<Nodecl::FunctionCode>();

            // Clone SimdFunction
            TL::Symbol func_sym = function_code.get_symbol();
            std::string orig_func_name = func_sym.get_name();

            // Set new vector function symbol
            std::stringstream vector_func_name;

            TL::Counter &counter = TL::CounterManager::get_counter("simd-function");
            vector_func_name <<"__"
                << orig_func_name
                << "_" << (int)counter
                << "_"
                << _device_name
                << "_"
                << _vector_length
                ;
            counter++;

            if (masked_version)
            {
                vector_func_name << "_mask";
            }

            // Remove template parameters, if any
            decl_context_t *new_func_decl_context = 
                decl_context_clone(
                        func_sym.get_scope().get_decl_context()
                        );
            new_func_decl_context->template_parameters = NULL;

            TL::Symbol new_func_sym = TL::Scope(new_func_decl_context).
                new_symbol(vector_func_name.str());
            new_func_sym.get_internal_symbol()->kind = SK_FUNCTION;

            Nodecl::Utils::SimpleSymbolMap func_sym_map;
            func_sym_map.add_map(func_sym, new_func_sym);

            Nodecl::OpenMP::SimdFunction simd_node_copy =
                Nodecl::Utils::deep_copy(simd_node, simd_node,
                        func_sym_map).as<Nodecl::OpenMP::SimdFunction>();

            Nodecl::FunctionCode vector_func_code =
                simd_node_copy.get_statement().as<Nodecl::FunctionCode>();

            FunctionDeepCopyFixVisitor fix_deep_copy_visitor(func_sym, new_func_sym);
            fix_deep_copy_visitor.walk(vector_func_code.get_statements());

            // Register new simd nodes in analysis
            Vectorization::Vectorizer::_vectorizer_analysis->register_identical_copy(
                    function_code, vector_func_code);

            // Process clauses FROM THE COPY
            Nodecl::List omp_environment = simd_node_copy.
                get_environment().as<Nodecl::List>();

            // Aligned clause
            map_tlsym_int_t aligned_expressions;
            process_aligned_clause(omp_environment, aligned_expressions);

            // Linear clause
            map_tlsym_int_t linear_symbols;
            process_linear_clause(omp_environment, linear_symbols);

            // Uniform clause
            objlist_tlsym_t uniform_symbols;
            process_uniform_clause(omp_environment, uniform_symbols);

            // Suitable clause
            objlist_nodecl_t suitable_expressions;
            process_suitable_clause(omp_environment, suitable_expressions);

            // Nontemporal clause
            map_tlsym_objlist_t nontemporal_expressions;
            process_nontemporal_clause(omp_environment, nontemporal_expressions);

            // Overlap clause
            map_tlsym_objlist_int_t overlap_symbols;
            process_overlap_clause(omp_environment, overlap_symbols);
//            VectorizerOverlap vectorizer_overlap(overlap_symbols);

            // Prefetch clause
            prefetch_info_t prefetch_info;
            process_prefetch_clause(omp_environment, prefetch_info);


            // Vectorlengthfor clause
            TL::Type vectorlengthfor_type;
            process_vectorlengthfor_clause(omp_environment, vectorlengthfor_type);

            // Vectorizer Environment
            VectorizerEnvironment function_environment(
                    _device_name,
                    _vector_length,
                    _support_masking,
                    _mask_size,
                    _fast_math_enabled,
                    vectorlengthfor_type,
                    aligned_expressions,
                    linear_symbols,
                    uniform_symbols,
                    suitable_expressions,
                    nontemporal_expressions,
                    overlap_symbols,
                    NULL,
                    NULL);

            // Append vectorized function code to scalar function
            // simd_node.append_sibling(vector_func_code);

            // Add scopes, default masks, etc.
            function_environment.load_environment(vector_func_code);
            // Set target type
            if (!function_environment._target_type.is_valid())
            {
                VectorizerTargetTypeHeuristic target_type_heuristic;
                function_environment.set_target_type(
                        target_type_heuristic.get_target_type(vector_func_code));
            }

            // Add SIMD version to vector function versioning
            TL::Type function_return_type = func_sym.get_type().returns();
            int function_return_type_size = function_return_type.is_void() ? 1 : function_return_type.get_size();
            _vectorizer.add_vector_function_version(
                    func_sym,
                    vector_func_code, _device_name, 
                    function_environment._vectorization_factor
                    * function_return_type_size,
                    function_return_type,
                    masked_version,
                    TL::Vectorization::SIMD_FUNC_PRIORITY, false);


            _vectorizer.vectorize_function_header(vector_func_code,
                    function_environment,
                    uniform_symbols,
                    linear_symbols,
                    masked_version);

            function_environment.unload_environment();
            _vectorizer.postprocess_code(simd_node);

            // Add extra CxxDef that may have been required during vectorization
            // std::cerr << "(2) ENV = " << &function_environment << std::endl;
            for (TL::ObjectList<VectorizerEnvironment::VectorizedClass>::iterator
                    it_classes = function_environment._vectorized_classes.begin();
                    it_classes != function_environment._vectorized_classes.end();
                    it_classes++)
            {
                Nodecl::NodeclBase translation_unit = CURRENT_COMPILED_FILE->nodecl;
                Nodecl::List top_level = translation_unit
                    .as<Nodecl::TopLevel>()
                    .get_top_level()
                    .as<Nodecl::List>();

                bool found = false;
                for (Nodecl::List::iterator it_nodes = top_level.begin();
                        it_nodes != top_level.end() && !found;
                        it_nodes++)
                {
                    if (it_nodes->is<Nodecl::CxxDef>()
                            && it_nodes->get_symbol() == it_classes->first.get_symbol())
                    {
                        it_nodes->append_sibling(
                                Nodecl::CxxDef::make(
                                    Nodecl::NodeclBase::null(),
                                    it_classes->second.get_symbol()));
                    }
                }
            }
            function_environment._vectorized_classes.clear();

// 
            // Free analysis
            //_vectorizer.finalize_analysis();

            // Prostprocess code
        }

        void SimdVisitor::common_simd_function(
                const Nodecl::OpenMP::SimdFunction& simd_node,
                const bool masked_version)
        {
            Nodecl::FunctionCode function_code = simd_node.get_statement()
                .as<Nodecl::FunctionCode>();

            // // Clone SimdFunction
            TL::Symbol func_sym = function_code.get_symbol();
            // std::string orig_func_name = func_sym.get_name();

            // // Set new vector function symbol
            // std::stringstream vector_func_name;

            // TL::Counter &counter = TL::CounterManager::get_counter("simd-function");
            // vector_func_name <<"__"
            //     << orig_func_name
            //     << "_" << (int)counter
            //     << "_"
            //     << _device_name
            //     << "_"
            //     << _vector_length
            //     ;
            // counter++;

            // if (masked_version)
            // {
            //     vector_func_name << "_mask";
            // }

            // // Remove template parameters, if any
            // decl_context_t *new_func_decl_context = 
            //     decl_context_clone(
            //             func_sym.get_scope().get_decl_context()
            //             );
            // new_func_decl_context->template_parameters = NULL;

            // TL::Symbol new_func_sym = TL::Scope(new_func_decl_context).
            //     new_symbol(vector_func_name.str());
            // new_func_sym.get_internal_symbol()->kind = SK_FUNCTION;

            // Nodecl::Utils::SimpleSymbolMap func_sym_map;
            // func_sym_map.add_map(func_sym, new_func_sym);

            // Nodecl::OpenMP::SimdFunction simd_node_copy =
            //     Nodecl::Utils::deep_copy(simd_node, simd_node,
            //             func_sym_map).as<Nodecl::OpenMP::SimdFunction>();

            // Process clauses FROM THE COPY
            Nodecl::FunctionCode vector_func_code;
            {
                VectorizerTargetTypeHeuristic target_type_heuristic;
                TL::Type target_type = target_type_heuristic.get_target_type(function_code);

                int _vectorization_factor =
                    _vector_length/target_type.get_size();

                TL::Type function_return_type = func_sym.get_type().returns();
                vector_func_code = Vectorizer::_function_versioning.get_best_version(
                            func_sym,
                            _device_name,
                            _vectorization_factor * 
                            (function_return_type.is_void() ? 1 : function_return_type.get_size()),
                            function_return_type,
                            masked_version).as<Nodecl::FunctionCode>();

                ERROR_CONDITION(vector_func_code.is_null()
                        || !vector_func_code.is<Nodecl::FunctionCode>(),
                        "This code must be a FunctionCode", 0);
            }

            Nodecl::NodeclBase simd_node_copy = vector_func_code.get_parent();
            ERROR_CONDITION(simd_node_copy.is_null()
                    || !simd_node_copy.is<Nodecl::OpenMP::SimdFunction>(),
                    "Invalid node, expecting a Nodecl::OpenMP::SimdFunction", 0);

            Nodecl::List omp_environment =
                simd_node_copy.as<Nodecl::OpenMP::SimdFunction>().get_environment().as<Nodecl::List>();

            // Aligned clause
            map_tlsym_int_t aligned_expressions;
            process_aligned_clause(omp_environment, aligned_expressions);

            // Linear clause
            map_tlsym_int_t linear_symbols;
            process_linear_clause(omp_environment, linear_symbols);

            // Uniform clause
            objlist_tlsym_t uniform_symbols;
            process_uniform_clause(omp_environment, uniform_symbols);

            // Suitable clause
            objlist_nodecl_t suitable_expressions;
            process_suitable_clause(omp_environment, suitable_expressions);

            // Nontemporal clause
            map_tlsym_objlist_t nontemporal_expressions;
            process_nontemporal_clause(omp_environment, nontemporal_expressions);

            // Overlap clause
            map_tlsym_objlist_int_t overlap_symbols;
            process_overlap_clause(omp_environment, overlap_symbols);
//            VectorizerOverlap vectorizer_overlap(overlap_symbols);

            // Prefetch clause
            prefetch_info_t prefetch_info;
            process_prefetch_clause(omp_environment, prefetch_info);


            // Vectorlengthfor clause
            TL::Type vectorlengthfor_type;
            process_vectorlengthfor_clause(omp_environment, vectorlengthfor_type);

            // Vectorizer Environment
            VectorizerEnvironment function_environment(
                    _device_name,
                    _vector_length,
                    _support_masking,
                    _mask_size,
                    _fast_math_enabled,
                    vectorlengthfor_type,
                    aligned_expressions,
                    linear_symbols,
                    uniform_symbols,
                    suitable_expressions,
                    nontemporal_expressions,
                    overlap_symbols,
                    NULL,
                    NULL);

            // Set target type
            if (!function_environment._target_type.is_valid())
            {
                VectorizerTargetTypeHeuristic target_type_heuristic;
                function_environment.set_target_type(
                        target_type_heuristic.get_target_type(function_code));
            }

            function_environment.load_environment(vector_func_code);

            // Register new simd nodes in analysis
            Vectorization::Vectorizer::_vectorizer_analysis->register_identical_copy(
                    function_code, vector_func_code);

            // Append vectorized function code to scalar function
            simd_node.append_sibling(vector_func_code);

            // // Set target type
            // if (!function_environment._target_type.is_valid())
            // {
            //     VectorizerTargetTypeHeuristic target_type_heuristic;
            //     function_environment.set_target_type(
            //             target_type_heuristic.get_target_type(vector_func_code));
            // }

            // // Add SIMD version to vector function versioning
            // TL::Type function_return_type = func_sym.get_type().returns();
            // _vectorizer.add_vector_function_version(
            //         func_sym,
            //         vector_func_code, _device_name, 
            //         function_return_type.get_size() * 
            //         function_environment._vectorization_factor, 
            //         function_return_type, masked_version,
            //         TL::Vectorization::SIMD_FUNC_PRIORITY, false);

            _vectorizer.vectorize_function(vector_func_code,
                    function_environment, masked_version);

            function_environment.unload_environment();
            _vectorizer.postprocess_code(simd_node);

            if (IS_CXX_LANGUAGE
                    && CURRENT_CONFIGURATION->explicit_instantiation)
            {
                // This is a specialization
                if (func_sym.get_type().is_template_specialized_type())
                {
                    TL::Symbol vector_func_sym = vector_func_code.get_symbol();
                    // Make sure we add an extra declaration
                    Nodecl::NodeclBase translation_unit = CURRENT_COMPILED_FILE->nodecl;
                    Nodecl::List top_level = translation_unit
                        .as<Nodecl::TopLevel>()
                        .get_top_level()
                        .as<Nodecl::List>();

                    bool found = false;
                    for (Nodecl::List::iterator it = top_level.begin();
                            it != top_level.end() && !found;
                            it++)
                    {
                        if (it->is<Nodecl::CxxDecl>()
                                && it->get_symbol() == func_sym)
                        {
                            it->append_sibling(
                                    Nodecl::CxxDecl::make(
                                        Nodecl::Context::make(
                                            Nodecl::NodeclBase::null(),
                                            func_sym.get_scope(),
                                            it->get_locus()),
                                        vector_func_sym,
                                        it->get_locus()));
                            found = true;
                        }
                    }
                    if (!found)
                    {
                        // std::cerr << "Hmmm, I did not find " << func_sym.get_qualified_name() << " anywhere in the top level..." << std::endl;
                    }
                }
            }
// 
            // Free analysis
            //_vectorizer.finalize_analysis();

            // Prostprocess code
        }

        SimdSPMLVisitor::SimdSPMLVisitor(Vectorization::SIMDInstructionSet simd_isa,
                bool fast_math_enabled, bool svml_enabled,
                bool only_adjacent_accesses, bool overlap_in_place)
            : SimdVisitor(simd_isa, fast_math_enabled, svml_enabled,
                    only_adjacent_accesses, overlap_in_place)
        {
        }
        
        void SimdSPMLVisitor::visit(const Nodecl::OpenMP::SimdParallel& simd_node)
        {
            Nodecl::OpenMP::Parallel omp_parallel = simd_node.
                get_openmp_parallel().as<Nodecl::OpenMP::Parallel>();
            Nodecl::List omp_simd_parallel_environment = simd_node.
                get_environment().as<Nodecl::List>();
            Nodecl::List omp_parallel_environment = omp_parallel.
                get_environment().as<Nodecl::List>();

            // Skipping AST_LIST_NODE
            Nodecl::NodeclBase parallel_statements = omp_parallel.get_statements().
                as<Nodecl::List>().front();

            //TODO
            walk(parallel_statements);

            // Aligned clause
            map_tlsym_int_t aligned_expressions;
            process_aligned_clause(omp_simd_parallel_environment, aligned_expressions);

            // Linear clause
            map_tlsym_int_t linear_symbols;
            process_linear_clause(omp_simd_parallel_environment, linear_symbols);

            // Uniform clause
            objlist_tlsym_t uniform_symbols;
            process_uniform_clause(omp_simd_parallel_environment, uniform_symbols);

            // Suitable clause
            objlist_nodecl_t suitable_expressions;
            process_suitable_clause(omp_simd_parallel_environment, suitable_expressions);

            // Nontemporal clause
            map_tlsym_objlist_t nontemporal_expressions;
            process_nontemporal_clause(omp_simd_parallel_environment, nontemporal_expressions);

            // Overlap clause
            map_tlsym_objlist_int_t overlap_symbols;
            process_overlap_clause(omp_simd_parallel_environment, overlap_symbols);

            // Prefetch clause
            Vectorization::prefetch_info_t prefetch_info;
            process_prefetch_clause(omp_simd_parallel_environment, prefetch_info);

            // Vectorlengthfor clause
            TL::Type vectorlengthfor_type;
            process_vectorlengthfor_clause(omp_simd_parallel_environment, vectorlengthfor_type);

            // External symbols (loop)
            std::map<TL::Symbol, TL::Symbol> new_external_vector_symbol_map;

            // Reduction clause
            objlist_tlsym_t reductions;
            Nodecl::List omp_reduction_list =
                process_reduction_clause(omp_parallel_environment,
                        reductions, new_external_vector_symbol_map,
                        parallel_statements.retrieve_context());

            // Vectorizer Environment
            VectorizerEnvironment parallel_environment(
                    _device_name,
                    _vector_length,
                    _support_masking,
                    _mask_size,
                    _fast_math_enabled,
                    vectorlengthfor_type,
                    aligned_expressions,
                    linear_symbols,
                    uniform_symbols,
                    suitable_expressions,
                    nontemporal_expressions,
                    overlap_symbols,
                    &reductions,
                    &new_external_vector_symbol_map);

            // Add scopes, default masks, etc.
            parallel_environment.load_environment(parallel_statements);
            // Set target type
            if (!parallel_environment._target_type.is_valid())
            {
                VectorizerTargetTypeHeuristic target_type_heuristic;
                parallel_environment.set_target_type(
                        target_type_heuristic.get_target_type(parallel_statements));
            }

            // Register new simd nodes in analysis //TODO?

            // Overlap init
//            vectorizer_overlap.declare_overlap_symbols(
//                    parallel_statements.retrieve_context(), parallel_environment);
//            simd_node.prepend_sibling(vectorizer_overlap.get_init_statements(
//                        parallel_environment));

            // VECTORIZE PARALLEL STATEMENTS
                _vectorizer.vectorize_parallel(parallel_statements,
                        parallel_environment);

            // Add new vector symbols
            Nodecl::List pre_parallel_nodecls, post_parallel_nodecls;

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
                                parallel_environment))
                    {
                        _vectorizer.vectorize_reduction(scalar_tl_symbol,
                                vector_tl_symbol,
                                reduction_initializer,
                                reduction_name,
                                reduction_type,
                                parallel_environment,
                                pre_parallel_nodecls,
                                post_parallel_nodecls);
                    }
                    else
                    {
                        running_error("SIMD: reduction '%s:%s' (%s) is not supported",
                                reduction_name.c_str(), scalar_tl_symbol.get_name().c_str(),
                                reduction_type.get_simple_declaration(
                                    parallel_statements.retrieve_context(), "").c_str());
                    }
                }

                simd_node.prepend_sibling(pre_parallel_nodecls);
                // Final reduction after the epilog (to reduce also elements from masked epilogs)
                //single_epilog.append_sibling(post_parallel_nodecls);
            }

            parallel_environment.unload_environment();

            // Remove SIMD node
            simd_node.replace(omp_parallel);

            // Free analysis
            //_vectorizer.finalize_analysis();
        }

        void SimdProcessingBase::process_aligned_clause(const Nodecl::List& environment,
                map_tlsym_int_t& aligned_expressions_map)
        {
            TL::ObjectList<Nodecl::OpenMP::Aligned> omp_aligned_list =
                environment.find_all<Nodecl::OpenMP::Aligned>();

            for(TL::ObjectList<Nodecl::OpenMP::Aligned>::iterator it = omp_aligned_list.begin();
                    it != omp_aligned_list.end();
                    it++)
            {
                Nodecl::OpenMP::Aligned& omp_aligned = *it;

                objlist_nodecl_t aligned_expressions_list =
                    omp_aligned.get_aligned_expressions().as<Nodecl::List>().to_object_list();

                int alignment = const_value_cast_to_signed_int(
                        omp_aligned.get_alignment().as<Nodecl::IntegerLiteral>().get_constant());

                for(objlist_nodecl_t::iterator it2 = aligned_expressions_list.begin();
                        it2 != aligned_expressions_list.end();
                        it2++)
                {

                    if(!aligned_expressions_map.insert(std::pair<TL::Symbol, int>(
                                    it2->as<Nodecl::Symbol>().get_symbol(), alignment)).second)
                    {
                        running_error("SIMD: multiple instances of the same variable in the 'aligned' clause detected\n");
                    }
                }
            }
        }

        void SimdProcessingBase::process_linear_clause(const Nodecl::List& environment,
                map_tlsym_int_t& linear_symbols_map)
        {
            TL::ObjectList<Nodecl::OpenMP::Linear> omp_linear_list =
                environment.find_all<Nodecl::OpenMP::Linear>();

            for(TL::ObjectList<Nodecl::OpenMP::Linear>::iterator it = omp_linear_list.begin();
                    it != omp_linear_list.end();
                    it++)
            {
                Nodecl::OpenMP::Linear& omp_linear = *it;

                objlist_nodecl_t linear_symbols_list =
                    omp_linear.get_linear_expressions().as<Nodecl::List>().to_object_list();

                int step = const_value_cast_to_signed_int(
                        omp_linear.get_step().as<Nodecl::IntegerLiteral>().get_constant());

                for(objlist_nodecl_t::iterator it2 = linear_symbols_list.begin();
                        it2 != linear_symbols_list.end();
                        it2++)
                {

                    if(!linear_symbols_map.insert(std::pair<TL::Symbol, int>(
                                    it2->as<Nodecl::Symbol>().get_symbol(), step)).second)
                    {
                        running_error("SIMD: multiple instances of the same variable "\
                                "in the 'linear' clause detected\n");
                    }
                }
            }
        }

        void SimdProcessingBase::process_uniform_clause(const Nodecl::List& environment,
                objlist_tlsym_t& uniform_symbols)
        {
            Nodecl::OpenMP::Uniform omp_uniform =
                environment.find_first<Nodecl::OpenMP::Uniform>();

            if(!omp_uniform.is_null())
            {
                objlist_nodecl_t uniform_symbols_list =
                    omp_uniform.get_uniform_expressions().as<Nodecl::List>().to_object_list();

                for(objlist_nodecl_t::iterator it2 = uniform_symbols_list.begin();
                        it2 != uniform_symbols_list.end();
                        it2++)
                {
                    uniform_symbols.insert(
                                it2->as<Nodecl::Symbol>().get_symbol());
                }
            }
        }

        void SimdProcessingBase::process_suitable_clause(const Nodecl::List& environment,
                objlist_nodecl_t& suitable_expressions)
        {
            Nodecl::OpenMP::Suitable omp_suitable =
                environment.find_first<Nodecl::OpenMP::Suitable>();

            if(!omp_suitable.is_null())
            {
                suitable_expressions = omp_suitable.get_suitable_expressions().
                    as<Nodecl::List>().to_object_list();
            }
        }

        void SimdProcessingBase::process_nontemporal_clause(const Nodecl::List& environment,
                map_tlsym_objlist_t& nontemporal_expressions)
        {
            TL::ObjectList<Nodecl::OpenMP::Nontemporal> omp_nontemporal_list =
                environment.find_all<Nodecl::OpenMP::Nontemporal>();

            for(const auto& omp_nontemporal : omp_nontemporal_list)
            {
                objlist_nodecl_t nontemporal_expressions_list =
                    omp_nontemporal.get_nontemporal_expressions().as<Nodecl::List>().to_object_list();

                objlist_nodecl_t nontemporal_flags = omp_nontemporal.get_flags().
                    as<Nodecl::List>().to_object_list();

                for(objlist_nodecl_t::iterator it2 = nontemporal_expressions_list.begin();
                        it2 != nontemporal_expressions_list.end();
                        it2++)
                {

                    if(!nontemporal_expressions.insert(std::make_pair(
                                    it2->as<Nodecl::Symbol>().get_symbol(), nontemporal_flags)).second)
                    {
                        running_error("SIMD: multiple instances of the same variable in the 'aligned' clause detectedn\n");
                    }
                }
            }
        }

        int SimdProcessingBase::process_unroll_clause(const Nodecl::List& environment)
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

        int SimdProcessingBase::process_unroll_and_jam_clause(const Nodecl::List& environment)
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

        void SimdProcessingBase::process_vectorlengthfor_clause(const Nodecl::List& environment,
                TL::Type& vectorlengthfor_type)
        {
            Nodecl::OpenMP::VectorLengthFor omp_vector_length_for =
                environment.find_first<Nodecl::OpenMP::VectorLengthFor>();

            if(!omp_vector_length_for.is_null())
            {
                vectorlengthfor_type = omp_vector_length_for.get_type();
            }
        }

        void SimdProcessingBase::process_overlap_clause(const Nodecl::List& environment,
                map_tlsym_objlist_int_t& overlap_symbols)
        {
            TL::ObjectList<Nodecl::OpenMP::Overlap> omp_overlap_list =
                environment.find_all<Nodecl::OpenMP::Overlap>();

            for(TL::ObjectList<Nodecl::OpenMP::Overlap>::iterator it = omp_overlap_list.begin();
                    it != omp_overlap_list.end();
                    it++)
            {
                Nodecl::OpenMP::Overlap& omp_overlap = *it;

                objlist_nodecl_t overlap_symbols_list =
                    omp_overlap.get_overlap_expressions().as<Nodecl::List>().to_object_list();

                int min_group_loads = const_value_cast_to_signed_int(
                        it->get_min_group_loads().get_constant());
                int max_group_registers = const_value_cast_to_signed_int(
                        it->get_max_group_registers().get_constant());
                int max_groups = const_value_cast_to_signed_int(
                        it->get_max_groups().get_constant());

                for(objlist_nodecl_t::iterator it2 = overlap_symbols_list.begin();
                        it2 != overlap_symbols_list.end();
                        it2++)
                {
                    objlist_int_t overlap_params(3);
                    overlap_params[0] = min_group_loads;
                    overlap_params[1] = max_group_registers;
                    overlap_params[2] = max_groups;
                    

                    if(!overlap_symbols.insert(std::pair<TL::Symbol, objlist_int_t>(
                                    it2->as<Nodecl::Symbol>().get_symbol(),
                                    overlap_params)).second)
                    {
                        running_error("SIMD: multiple instances of the same variable in the 'overlap' clause detected\n");
                    }
                }
            }
        }

        void SimdProcessingBase::process_prefetch_clause(const Nodecl::List& environment,
                Vectorization::prefetch_info_t& prefetch_info)
        {
            TL::ObjectList<Nodecl::OpenMP::Prefetch> omp_prefetch_list =
                environment.find_all<Nodecl::OpenMP::Prefetch>();

            ERROR_CONDITION(omp_prefetch_list.size() > 1, "Too many OpenMP::Prefetch nodes", 0);

            if (omp_prefetch_list.size() == 1)
            {
                Nodecl::OpenMP::Prefetch& omp_prefetch = *omp_prefetch_list.begin();

                objlist_nodecl_t prefetch_distances_list =
                    omp_prefetch.get_distances().as<Nodecl::List>().to_object_list();

                ERROR_CONDITION(prefetch_distances_list.size() != 2, "Prefetch distances must be 2", 0);

                prefetch_info.enabled = true;

                prefetch_info.distances[1] = const_value_cast_to_signed_int(prefetch_distances_list[0].get_constant()); // L2 distance
                prefetch_info.distances[0] = const_value_cast_to_signed_int(prefetch_distances_list[1].get_constant()); // L1 distance

                Nodecl::NodeclBase strategy = omp_prefetch.get_strategy();

                if (strategy.is<Nodecl::OnTopFlag>())
                    prefetch_info.in_place = false;
                else if (strategy.is<Nodecl::InPlaceFlag>())
                    prefetch_info.in_place = true;
                else
                {
                   internal_error("Prefetch strategy is neither OnTopFlag nor InPlaceFlag\n", 0); 
                }

            }
            else
            {
                prefetch_info.enabled = false;
            }
        }

        Nodecl::List SimdProcessingBase::process_reduction_clause(const Nodecl::List& environment,
                TL::ObjectList<TL::Symbol>& reductions,
                std::map<TL::Symbol, TL::Symbol>& new_external_vector_symbol_map,
                TL::Scope enclosing_scope)
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
                            enclosing_scope.new_symbol("__vred_" + red_sym.get_name());
                        new_red_sym.get_internal_symbol()->kind = SK_VARIABLE;
                        symbol_entity_specs_set_is_user_declared(new_red_sym.get_internal_symbol(), 1);
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
