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

#include "tl-omp-simd-visitor.hpp"
#include "tl-omp-simd-clauses-processor.hpp"

#include "tl-vectorizer-target-type-heuristic.hpp"
#include "tl-vectorization-utils.hpp"
#include "tl-vectorization-common.hpp"

#include "tl-counters.hpp"
#include "hlt-loop-unroll.hpp"
#include "cxx-cexpr.h"

using namespace TL::Vectorization;

namespace TL
{
namespace OpenMP
{
SimdProcessingBase::SimdProcessingBase(
    Vectorization::VectorInstructionSet vector_isa,
    bool fast_math_enabled,
    bool svml_enabled,
    bool only_adjacent_accesses,
    bool only_aligned_accesses,
    bool overlap_in_place)
    : _vectorizer(TL::Vectorization::Vectorizer::get_vectorizer()),
      _vector_isa_desc(TL::Vectorization::get_vector_isa_description(vector_isa)),
      _fast_math_enabled(fast_math_enabled),
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

    if (only_aligned_accesses)
    {
        _vectorizer.disable_unaligned_accesses();
    }

    switch (vector_isa)
    {
        case SSE4_2_ISA:
            if (svml_enabled)
                _vectorizer.enable_svml_sse();
            break;

        case KNC_ISA:
            if (svml_enabled)
                _vectorizer.enable_svml_knc();
            break;

        case KNL_ISA:
            if (svml_enabled)
                _vectorizer.enable_svml_knl();
            break;

        case AVX2_ISA:
            if (svml_enabled)
                _vectorizer.enable_svml_avx2();
            break;

        case NEON_ISA:
            break;

        case ROMOL_ISA:
            break;

        default:
            fatal_error("SIMD: Unsupported vector ISA: %d", vector_isa);
    }
}

Nodecl::UnknownPragma get_epilogue_loop_count_pragma(int epilogue_iterations,
                                                     int vec_factor)
{
    std::stringstream loop_count_pragma_strm;
    loop_count_pragma_strm << "loop_count";

    if (epilogue_iterations < 0)
    {
        loop_count_pragma_strm << " min(0) max(" << vec_factor - 1 << ")";
    }
    else
    {
        loop_count_pragma_strm << "(" << epilogue_iterations << ")";
    }

    return Nodecl::UnknownPragma::make(loop_count_pragma_strm.str());
}


SimdPreregisterVisitor::SimdPreregisterVisitor(
    Vectorization::VectorInstructionSet simd_isa,
    bool fast_math_enabled,
    bool svml_enabled,
    bool only_adjacent_accesses,
    bool only_aligned_accesses,
    bool overlap_in_place)
    : SimdProcessingBase(simd_isa,
                         fast_math_enabled,
                         svml_enabled,
                         only_adjacent_accesses,
                         only_aligned_accesses,
                         overlap_in_place)
{
}

SimdPreregisterVisitor::~SimdPreregisterVisitor()
{
    _vectorizer.finalize_analysis();
}

SimdVisitor::SimdVisitor(Vectorization::VectorInstructionSet simd_isa,
                         bool fast_math_enabled,
                         bool svml_enabled,
                         bool only_adjacent_accesses,
                         bool only_aligned_accesses,
                         bool overlap_in_place)
    : SimdProcessingBase(simd_isa,
                         fast_math_enabled,
                         svml_enabled,
                         only_adjacent_accesses,
                         only_aligned_accesses,
                         overlap_in_place)
{
}

SimdVisitor::~SimdVisitor()
{
    _vectorizer.finalize_analysis();
}

void SimdVisitor::visit(const Nodecl::TemplateFunctionCode &n)
{
    // TODO: Do nothing
}

void SimdVisitor::visit(const Nodecl::FunctionCode &n)
{
    // Note that SimdFunction is treated specially in its visit

    // TODO::Improve!

    TL::ObjectList<Nodecl::NodeclBase> omp_simd_list
        = Nodecl::Utils::nodecl_get_all_nodecls_of_kind<Nodecl::OpenMP::Simd>(
            n);
    TL::ObjectList<Nodecl::NodeclBase> omp_simd_for_list = Nodecl::Utils::
        nodecl_get_all_nodecls_of_kind<Nodecl::OpenMP::SimdFor>(n);

    for (const auto &node : omp_simd_list)
        _vectorizer.preprocess_code(node);
    for (const auto &node : omp_simd_for_list)
        _vectorizer.preprocess_code(node);

    if (!omp_simd_list.empty() || !omp_simd_for_list.empty())
    {
        _vectorizer.initialize_analysis(n);
        walk(n.get_statements());
    }

    for (const auto &node : omp_simd_list)
        _vectorizer.postprocess_code(node);
    for (const auto &node : omp_simd_for_list)
        _vectorizer.postprocess_code(node);
}

unsigned int compute_vec_factor(
    const Nodecl::NodeclBase &scalar_code,
    int vectorlength_in_elements,
    TL::Type target_type,
    const VectorIsaDescriptor &vector_isa_desc)
{
    if (vectorlength_in_elements != 0 && target_type.is_valid())
        fatal_error("SIMD: vectorlength and target_type cannot be both valid");

    if (vectorlength_in_elements != 0)
        return vectorlength_in_elements;
    if (target_type.is_valid())
        return vector_isa_desc.get_vec_factor_from_type(target_type);
    else
    {
        VectorizerTargetTypeHeuristic target_type_heuristic;
        TL::Type heuristic_type = target_type_heuristic.get_target_type(scalar_code);
        return vector_isa_desc.get_vec_factor_from_type(heuristic_type);
    }
}

void set_initial_mask(VectorizerEnvironment &environment,
                      const Nodecl::NodeclBase &sibling_ref_node)
{
    // Add MaskLiteral to mask_list
    // TODO: get_float_type
    unsigned int isa_vec_factor
        = environment._vec_isa_desc.get_vec_factor_for_type(
            TL::Type::get_float_type(), environment._vec_factor);

    // Initial mask
    Nodecl::MaskLiteral contiguous_mask
        = Vectorization::Utils::get_contiguous_mask_literal(
            isa_vec_factor, environment._vec_factor);

    // We do not create a symbol if mask is all ones
    if (Utils::is_all_one_mask(contiguous_mask))
    {
        environment._mask_list.push_back(contiguous_mask);
    }
    else
    {
        Nodecl::NodeclBase initial_mask_symbol
            = Utils::get_new_mask_symbol(environment._analysis_simd_scope,
                                         isa_vec_factor,
                                         true /*ref_type*/);

        Nodecl::ExpressionStatement initial_mask_exp
            = Nodecl::ExpressionStatement::make(
                Nodecl::VectorMaskAssignment::make(
                    initial_mask_symbol.shallow_copy(),
                    contiguous_mask,
                    initial_mask_symbol.get_type(),
                    sibling_ref_node.get_locus()));

        sibling_ref_node.prepend_sibling(initial_mask_exp);

        environment._mask_list.push_back(initial_mask_symbol);

        CXX_LANGUAGE()
        {
            sibling_ref_node.prepend_sibling(
                Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                     initial_mask_symbol.get_symbol(),
                                     initial_mask_symbol.get_locus()));
        }
    }
}

void SimdVisitor::visit(const Nodecl::OpenMP::Simd &simd_input_node)
{
    Nodecl::NodeclBase simd_enclosing_node = simd_input_node.get_parent();
    Nodecl::OpenMP::Simd simd_node_main_loop
        = simd_input_node.shallow_copy().as<Nodecl::OpenMP::Simd>();

    // This is ugly but some routines below expect this tree
    // to be usable with retrieve_context
    nodecl_set_parent(
            simd_node_main_loop.get_internal_nodecl(),
            nodecl_get_parent(simd_input_node.get_internal_nodecl()));

    Nodecl::NodeclBase loop_statement = simd_node_main_loop.get_statement();
    Nodecl::List simd_environment
        = simd_node_main_loop.get_environment().as<Nodecl::List>();

    // Process common simd clauses
    map_nodecl_int_t aligned_expressions;
    map_tlsym_int_t linear_symbols;
    objlist_tlsym_t uniform_symbols;
    objlist_nodecl_t suitable_expressions;
    map_tlsym_objlist_t nontemporal_expressions;
    unsigned int vectorlength_in_elements;
    TL::Type vectorlengthfor_type;
    map_tlsym_objlist_int_t overlap_symbols;
    Vectorization::prefetch_info_t prefetch_info;

    process_common_simd_clauses(simd_environment,
                                aligned_expressions,
                                linear_symbols,
                                uniform_symbols,
                                suitable_expressions,
                                vectorlength_in_elements,
                                vectorlengthfor_type,
                                nontemporal_expressions,
                                overlap_symbols,
                                prefetch_info);

    // Process loop-specific simd clauses
    unsigned int unroll_factor;
    unsigned int unroll_and_jam_factor;
    bool loop_unrolled = false;

    process_loop_simd_clauses(simd_environment,
            unroll_factor,
            unroll_and_jam_factor);


    // Compute the vec factor based on the information of the clauses,
    // if any, or analyzing the code
    unsigned int vec_factor = compute_vec_factor(loop_statement,
                                                 vectorlength_in_elements,
                                                 vectorlengthfor_type,
                                                 _vector_isa_desc);

    // External symbols (loop)
    std::map<TL::Symbol, TL::Symbol> new_external_vector_symbol_map;
    // Reduction and simd_reduction clauses
    objlist_tlsym_t reductions;

    Nodecl::List omp_reduction_list
        = process_reduction_clause(simd_environment,
                                   reductions,
                                   new_external_vector_symbol_map,
                                   simd_enclosing_node.retrieve_context(),
                                   vec_factor);


    // Vectorizer Environment
    VectorizerEnvironment loop_environment(_vector_isa_desc,
                                           vec_factor,
                                           _fast_math_enabled,
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

    // Add epilog before vectorization
    Nodecl::OpenMP::Simd simd_node_epilog
        = Nodecl::Utils::deep_copy(simd_node_main_loop, simd_enclosing_node)
              .as<Nodecl::OpenMP::Simd>();

    // OUTPUT CODE STRUCTURE
    Nodecl::List output_code_list;
    output_code_list.append(simd_node_main_loop); // Main For
    output_code_list.append(simd_node_epilog); // Epilog

    // Register new simd nodes in analysis
    Vectorization::Vectorizer::_vectorizer_analysis->register_identical_copy(
        simd_input_node, simd_node_main_loop);
    Vectorization::Vectorizer::_vectorizer_analysis->register_identical_copy(
        simd_input_node, simd_node_epilog);

    Nodecl::CompoundStatement output_code = Nodecl::CompoundStatement::make(
        output_code_list, Nodecl::NodeclBase::null());

    // Replace input code by output and update output pointer
    simd_input_node.replace(output_code);
    output_code = simd_input_node.as<Nodecl::CompoundStatement>();

    // Get epilog information
    bool only_epilog;
    int epilog_iterations = _vectorizer.get_epilog_info(
        loop_statement, loop_environment, only_epilog);

    // Set initial mask for vectorization
    set_initial_mask(loop_environment, simd_node_main_loop);

    // MAIN LOOP VECTORIZATION
    if (!only_epilog)
    {
        _vectorizer.vectorize_loop(loop_statement, loop_environment);

        if (!loop_environment._overlap_symbols_map.empty())
        {

            // If unroll clause and overlap clause, apply unroll before overlap
            if (unroll_factor > 0)
            {
                TL::HLT::LoopUnroll loop_unroller;
                loop_unroller.set_loop(loop_statement)
                    .set_unroll_factor(unroll_factor)
                    .unroll();

                // Nodecl::NodeclBase whole_main_transformation =
                //    loop_unroller.get_whole_transformation();

                // Main loop is now the the unrolled version of 'n'
                loop_statement.replace(loop_unroller.get_unrolled_loop());

                // last_epilog = loop_unroller.get_epilog_loop()
                //    .as<Nodecl::ForStatement>();
                std::cerr << "Vectorized Loop Unrolled with UF="
                          << unroll_factor << std::endl;

                loop_unrolled = true;
            }


            Nodecl::List prependix;

            _vectorizer.opt_overlapped_accesses(loop_statement,
                                                loop_environment,
                                                false /* simd for */,
                                                false /* epilog */,
                                                _overlap_in_place,
                                                prependix);

            loop_statement.prepend_sibling(prependix);
        }

        if (prefetch_info.enabled)
            _vectorizer.prefetcher(
                loop_statement, prefetch_info, loop_environment);
    }

    // Add new vector symbols
    if (!new_external_vector_symbol_map.empty())
    {
        Nodecl::List pre_for_nodecls, post_for_nodecls;

        // REDUCTIONS
        for (Nodecl::List::iterator it = omp_reduction_list.begin();
             it != omp_reduction_list.end();
             it++)
        {
            // Prepare reduction information to be used in vectorizer
            Nodecl::OpenMP::ReductionItem omp_red_item
                = (*it).as<Nodecl::OpenMP::ReductionItem>();
            TL::OpenMP::Reduction omp_red
                = *(OpenMP::Reduction::get_reduction_info_from_symbol(
                    omp_red_item.get_reductor().get_symbol()));

            // Symbols
            std::map<TL::Symbol, TL::Symbol>::iterator new_external_symbol_pair
                = new_external_vector_symbol_map.find(
                    omp_red_item.get_reduced_symbol().get_symbol());

            TL::Symbol scalar_tl_symbol = new_external_symbol_pair->first;
            TL::Symbol vector_tl_symbol = new_external_symbol_pair->second;

            // Reduction info
            Nodecl::NodeclBase reduction_initializer
                = omp_red.get_initializer();
            std::string reduction_name = omp_red.get_name();
            TL::Type reduction_type = omp_red.get_type();

            // Vectorize reductions
            if (_vectorizer.is_supported_reduction(omp_red.is_builtin(),
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
                fatal_error("SIMD: reduction '%s:%s' is not supported",
                            reduction_name.c_str(),
                            scalar_tl_symbol.get_name().c_str());
            }
        }

        output_code_list.prepend(pre_for_nodecls);
        // Final reduction after the epilog (to reduce also elements from masked
        // epilogs)
        output_code_list.append(post_for_nodecls);

        // TODO:
        // firstprivate in SIMD
    }

    loop_environment.unload_environment(false /*Do not clean masks*/);

    // Process epilog
    if (epilog_iterations != 0)
    {
        Nodecl::NodeclBase net_epilog_node;
        Nodecl::NodeclBase loop_stmt_epilog = simd_node_epilog.get_statement();

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
        loop_environment.unload_environment(false /*Do not clean masks*/);
        loop_environment.load_environment(net_epilog_node);

        // Overlap
        if (!loop_environment._overlap_symbols_map.empty())
        {
            Nodecl::List prependix;
            _vectorizer.opt_overlapped_accesses(net_epilog_node,
                                                loop_environment,
                                                false /* simd for */,
                                                true /* epilog */,
                                                _overlap_in_place,
                                                prependix);

            ERROR_CONDITION(!prependix.empty(),
                            "Prependix is not empty in the epilogue loop",
                            0);
        }

        // 2nd step of transformation on epilog loop
        _vectorizer.clean_up_epilog(net_epilog_node,
                                    loop_environment,
                                    epilog_iterations,
                                    only_epilog,
                                    false /*parallel loop*/);

        loop_environment.unload_environment();

        // Prevent the vectorization/unrolling of the epilogue code
        // by the native compiler
        if (net_epilog_node.is<Nodecl::ForStatement>())
        {
            Nodecl::UnknownPragma loop_count_pragma
                = get_epilogue_loop_count_pragma(
                    epilog_iterations /*aprox iterations*/,
                    vec_factor);

            net_epilog_node.prepend_sibling(loop_count_pragma);
        }

        // Remove Simd node from epilog
        simd_node_epilog.replace(simd_node_epilog.get_statement());
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

        if (!loop_unrolled && unroll_factor > 0)
        {
            std::stringstream unroll_pragma_strm;
            unroll_pragma_strm << "unroll(";
            unroll_pragma_strm << unroll_factor;
            unroll_pragma_strm << ")";

            Nodecl::UnknownPragma unroll_pragma
                = Nodecl::UnknownPragma::make(unroll_pragma_strm.str());

            simd_node_main_loop.prepend_sibling(unroll_pragma);
        }

        // Unroll and Jam clause
        if (unroll_and_jam_factor > 0)
        {
            std::stringstream unroll_and_jam_pragma_strm;
            unroll_and_jam_pragma_strm << "unroll_and_jam(";
            unroll_and_jam_pragma_strm << unroll_and_jam_factor;
            unroll_and_jam_pragma_strm << ")";

            Nodecl::UnknownPragma unroll_and_jam_pragma
                = Nodecl::UnknownPragma::make(unroll_and_jam_pragma_strm.str());

            simd_node_main_loop.prepend_sibling(unroll_and_jam_pragma);
        }
    }
}

void SimdVisitor::visit(const Nodecl::OpenMP::SimdFor &simd_input_node)
{
    Nodecl::NodeclBase simd_enclosing_node = simd_input_node.get_parent();

    // With SimdFor we don't need to use a CompoundStatement as output
    // because we need to use the special node ForAppendix
    Nodecl::OpenMP::SimdFor simd_node_for = simd_input_node; // No shallow_copy
    Nodecl::OpenMP::For omp_for
        = simd_node_for.get_openmp_for().as<Nodecl::OpenMP::For>();

    Nodecl::List omp_simd_for_environment
        = simd_node_for.get_environment().as<Nodecl::List>();
    Nodecl::List omp_for_environment
        = omp_for.get_environment().as<Nodecl::List>();

    // Skipping AST_LIST_NODE
    Nodecl::NodeclBase loop_context = omp_for.get_loop();
    Nodecl::NodeclBase loop = loop_context.as<Nodecl::Context>()
                                  .get_in_context()
                                  .as<Nodecl::List>()
                                  .front()
                                  .as<Nodecl::ForStatement>();

    ERROR_CONDITION(!loop.is<Nodecl::ForStatement>(),
                    "Unexpected node %s. Expecting a ForStatement after "
                    "'#pragma omp simd for'",
                    ast_print_node_type(loop.get_kind()));

    Nodecl::ForStatement for_statement = loop.as<Nodecl::ForStatement>();

    // Process common simd clauses
    map_nodecl_int_t aligned_expressions;
    map_tlsym_int_t linear_symbols;
    objlist_tlsym_t uniform_symbols;
    objlist_nodecl_t suitable_expressions;
    map_tlsym_objlist_t nontemporal_expressions;
    unsigned int vectorlength_in_elements;
    TL::Type vectorlengthfor_type;
    map_tlsym_objlist_int_t overlap_symbols;
    Vectorization::prefetch_info_t prefetch_info;

    process_common_simd_clauses(omp_simd_for_environment,
                             aligned_expressions,
                             linear_symbols,
                             uniform_symbols,
                             suitable_expressions,
                             vectorlength_in_elements,
                             vectorlengthfor_type,
                             nontemporal_expressions,
                             overlap_symbols,
                             prefetch_info);

    // Process loop-specific simd clauses
    unsigned int unroll_factor;
    unsigned int unroll_and_jam_factor;

    process_loop_simd_clauses(omp_for_environment,
            unroll_factor,
            unroll_and_jam_factor);


    // Compute the vec factor based on the information of the clauses,
    // if any, or analyzing the code
    unsigned int vec_factor = compute_vec_factor(for_statement,
                                                 vectorlength_in_elements,
                                                 vectorlengthfor_type,
                                                 _vector_isa_desc);

    // External symbols (loop)
    std::map<TL::Symbol, TL::Symbol> new_external_vector_symbol_map;
    // Reduction clause
    objlist_tlsym_t reductions;
    Nodecl::List omp_reduction_list
        = process_reduction_clause(omp_for_environment,
                                   reductions,
                                   new_external_vector_symbol_map,
                                   simd_enclosing_node.retrieve_context(),
                                   vec_factor);

    // Vectorizer Environment
    VectorizerEnvironment for_environment(_vector_isa_desc,
                                          vec_factor,
                                          _fast_math_enabled,
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

    // Add epilog before vectorization
    Nodecl::OpenMP::SimdFor simd_node_epilog
        = Nodecl::Utils::deep_copy(simd_node_for, simd_enclosing_node)
              .as<Nodecl::OpenMP::SimdFor>();

    simd_node_for.append_sibling(simd_node_epilog);

    // Register new simd nodes in analysis
    // Note that simd_node_for has not been shallow copied
    Vectorization::Vectorizer::_vectorizer_analysis->register_identical_copy(
        simd_node_for, simd_node_epilog);

    // Get epilog information
    bool only_epilog;
    int epilog_iterations = _vectorizer.get_epilog_info(
        for_statement, for_environment, only_epilog);

    // Overlap init
    //            vectorizer_overlap.declare_overlap_symbols(
    //                    simd_enclosing_node.retrieve_context(),
    //                    for_environment);
    //            simd_node_for.prepend_sibling(vectorizer_overlap.get_init_statements(for_environment));

    // Set initial mask for vectorization
    set_initial_mask(for_environment, simd_node_for);

    // VECTORIZE FOR
    if (!only_epilog)
    {
        _vectorizer.vectorize_loop(for_statement, for_environment);

        if (!for_environment._overlap_symbols_map.empty())
        {
            _vectorizer.opt_overlapped_accesses(for_statement,
                                                for_environment,
                                                true /* simd for */,
                                                false /*epilog*/,
                                                _overlap_in_place,
                                                prependix_list);
        }

        if (prefetch_info.enabled)
        {
            _vectorizer.prefetcher(
                for_statement, prefetch_info, for_environment);

            // Remove 'pragma noprefetch' and add it as a clause
            Nodecl::NodeclBase previous_sibling
                = Nodecl::Utils::get_previous_sibling(for_statement);
            if (!previous_sibling.is_null()
                && previous_sibling.is<Nodecl::UnknownPragma>()
                && previous_sibling.as<Nodecl::UnknownPragma>().get_text()
                       == "noprefetch")
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
        for (Nodecl::List::iterator it = omp_reduction_list.begin();
             it != omp_reduction_list.end();
             it++)
        {
            // Prepare reduction information to be used in vectorizer
            Nodecl::OpenMP::ReductionItem omp_red_item
                = (*it).as<Nodecl::OpenMP::ReductionItem>();
            TL::OpenMP::Reduction omp_red
                = *(OpenMP::Reduction::get_reduction_info_from_symbol(
                    omp_red_item.get_reductor().get_symbol()));

            // Symbols
            std::map<TL::Symbol, TL::Symbol>::iterator new_external_symbol_pair
                = new_external_vector_symbol_map.find(
                    omp_red_item.get_reduced_symbol().get_symbol());

            TL::Symbol scalar_tl_symbol = new_external_symbol_pair->first;
            TL::Symbol vector_tl_symbol = new_external_symbol_pair->second;

            // Reduction info
            Nodecl::NodeclBase reduction_initializer
                = omp_red.get_initializer();
            std::string reduction_name = omp_red.get_name();
            TL::Type reduction_type = omp_red.get_type();

            // Vectorize reductions
            if (_vectorizer.is_supported_reduction(omp_red.is_builtin(),
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
                fatal_error("SIMD: reduction '%s:%s' (%s) is not supported",
                            reduction_name.c_str(),
                            scalar_tl_symbol.get_name().c_str(),
                            reduction_type
                                .get_simple_declaration(
                                    simd_enclosing_node.retrieve_context(), "")
                                .c_str());
            }
        }

        simd_node_for.prepend_sibling(pre_for_nodecls);
        // Final reduction after the epilog (to reduce also elements from masked
        // epilogs)
        // single_epilog.append_sibling(post_for_nodecls);
    }

    for_environment.unload_environment(false /*Do not clean masks*/);

    Nodecl::NodeclBase net_epilog_node;
    Nodecl::ForStatement epilog_for_statement;

    // Process epilog
    if (epilog_iterations != 0)
    {
        epilog_for_statement = Nodecl::Utils::skip_contexts_and_lists(
                                   simd_node_epilog.get_openmp_for()
                                       .as<Nodecl::OpenMP::For>()
                                       .get_loop())
                                   .as<Nodecl::ForStatement>();
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
        for_environment.unload_environment(false /*Do not clean masks*/);
        for_environment.load_environment(net_epilog_node);

        Nodecl::List single_stmts_list;

        // Overlap
        if (!for_environment._overlap_symbols_map.empty())
        {
            _vectorizer.opt_overlapped_accesses(net_epilog_node,
                                                for_environment,
                                                true /* simd for */,
                                                true /* epilog */,
                                                _overlap_in_place,
                                                single_stmts_list);
        }

        // 2nd step of transformation on epilog loop
        _vectorizer.clean_up_epilog(net_epilog_node,
                                    for_environment,
                                    epilog_iterations,
                                    only_epilog,
                                    true /*parallel loop*/);

        // Prevent the vectorization/unrolling of the epilogue code
        // by the native compiler
        if (net_epilog_node.is<Nodecl::ForStatement>())
        {
            Nodecl::UnknownPragma loop_count_pragma
                = get_epilogue_loop_count_pragma(
                    epilog_iterations /*aprox iterations*/,
                    vec_factor);

            single_stmts_list.append(loop_count_pragma);
        }

        // This shallow copy is needed!
        single_stmts_list.append(net_epilog_node.shallow_copy());

        for_environment.unload_environment();

        // SINGLE
        Nodecl::List single_environment;
        // Create single node
        Nodecl::OpenMP::Single single_epilog = Nodecl::OpenMP::Single::make(
            single_environment, single_stmts_list, net_epilog_node.get_locus());

        net_epilog_node.replace(single_epilog);

        // Move single_epilog to its final position
        appendix_list.append(epilog_for_statement);
        // appendix_list.append(net_epilog_node);
    }

    // Remove epilog from original code
    Nodecl::Utils::remove_from_enclosing_list(simd_node_epilog);

    if (!post_for_nodecls.empty())
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
        if (unroll_factor != 0)
        {
            // Main Loop
            TL::HLT::LoopUnroll loop_unroller;
            loop_unroller.set_loop(for_statement)
                .set_unroll_factor(unroll_factor)
                .unroll();

            Nodecl::NodeclBase unrolled_transformation
                = loop_unroller.get_unrolled_loop();

            std::cerr << "BEFORE: " << std::endl;
            std::cerr << for_statement.prettyprint() << std::endl;

            for_statement.replace(unrolled_transformation);

            std::cerr << "AFTER: " << std::endl;
            std::cerr << for_statement.prettyprint() << std::endl;
        }

        // ForAppendix only if appendix is not empty
        if (!appendix_list.empty() || !prependix_list.empty())
        {
            Nodecl::List omp_for_appendix_environment
                = omp_for_environment.shallow_copy().as<Nodecl::List>();
            for (Nodecl::List::iterator it = omp_reduction_list.begin();
                 it != omp_reduction_list.end();
                 it++)
            {
                Nodecl::OpenMP::ReductionItem omp_red_item
                    = (*it).as<Nodecl::OpenMP::ReductionItem>();
                TL::OpenMP::Reduction omp_red
                    = *(OpenMP::Reduction::get_reduction_info_from_symbol(
                        omp_red_item.get_reductor().get_symbol()));

                std::map<TL::Symbol, TL::Symbol>::iterator
                    new_external_symbol_pair
                    = new_external_vector_symbol_map.find(
                        omp_red_item.get_reduced_symbol().get_symbol());

                ERROR_CONDITION(new_external_symbol_pair
                                    == new_external_vector_symbol_map.end(),
                                "Reduced symbol '%s' not found\n",
                                omp_red_item.get_reduced_symbol()
                                    .get_symbol()
                                    .get_name()
                                    .c_str());

                TL::Symbol vector_tl_symbol = new_external_symbol_pair->second;
                ERROR_CONDITION(vector_tl_symbol.get_value().is_null(),
                                "Invalid vector symbol",
                                0);

                omp_for_appendix_environment.append(
                    Nodecl::OpenMP::PrivateInit::make(
                        Nodecl::NodeclBase::null(), vector_tl_symbol));
            }

            for_epilog = Nodecl::OpenMP::ForAppendix::make(
                omp_for_appendix_environment,
                loop_context.shallow_copy(),
                prependix_list,
                appendix_list,
                omp_for.get_locus());
        }
        else
        {
            for_epilog
                = Nodecl::OpenMP::For::make(omp_for_environment.shallow_copy(),
                                            loop_context.shallow_copy(),
                                            omp_for.get_locus());
        }
    }

    // Remove Barrier and Flush from inner omp for
    Nodecl::NodeclBase barrier
        = omp_for_environment.find_first<Nodecl::OpenMP::BarrierAtEnd>();
    Nodecl::NodeclBase flush
        = omp_for_environment.find_first<Nodecl::OpenMP::FlushAtExit>();

    if (!barrier.is_null())
        Nodecl::Utils::remove_from_enclosing_list(barrier);

    if (!flush.is_null())
        Nodecl::Utils::remove_from_enclosing_list(flush);

    // Remove Simd nodes
    simd_node_for.replace(for_epilog);

    // Free analysis
    //_vectorizer.finalize_analysis();
}


void SimdPreregisterVisitor::visit(
    const Nodecl::OpenMP::SimdFunction &simd_node)
{
    Nodecl::FunctionCode function_code
        = simd_node.get_statement().as<Nodecl::FunctionCode>();

    // Preprocess SimdFunction
    _vectorizer.preprocess_code(simd_node);
    _vectorizer.initialize_analysis(simd_node);

    Nodecl::List omp_environment
        = simd_node.get_environment().as<Nodecl::List>();

    Nodecl::OpenMP::Mask omp_mask
        = omp_environment.find_first<Nodecl::OpenMP::Mask>();
    Nodecl::OpenMP::NoMask omp_nomask
        = omp_environment.find_first<Nodecl::OpenMP::NoMask>();

    if ((!omp_mask.is_null()) && (!omp_nomask.is_null()))
    {
        fatal_error(
            "SIMD: 'mask' and 'nomask' clauses are now allowed at the same "
            "time\n");
    }

    if ((!omp_mask.is_null()) && (!_vector_isa_desc.support_masking()))
    {
        fatal_error(
            "SIMD: 'mask' clause detected. Masking is not supported by the "
            "underlying architecture\n");
    }

    // Mask Version
    if (_vector_isa_desc.support_masking() && omp_nomask.is_null())
    {
        common_simd_function_preregister(simd_node, true);
    }
    // Nomask Version
    if (omp_mask.is_null())
    {
        common_simd_function_preregister(simd_node, false);
    }
}

void SimdVisitor::visit(const Nodecl::OpenMP::SimdFunction &simd_node)
{
    Nodecl::FunctionCode function_code
        = simd_node.get_statement().as<Nodecl::FunctionCode>();

    // Preprocess SimdFunction
    _vectorizer.preprocess_code(simd_node);
    _vectorizer.initialize_analysis(simd_node);

    Nodecl::List omp_environment
        = simd_node.get_environment().as<Nodecl::List>();

    Nodecl::OpenMP::Mask omp_mask
        = omp_environment.find_first<Nodecl::OpenMP::Mask>();
    Nodecl::OpenMP::NoMask omp_nomask
        = omp_environment.find_first<Nodecl::OpenMP::NoMask>();

    if ((!omp_mask.is_null()) && (!omp_nomask.is_null()))
    {
        fatal_error(
            "SIMD: 'mask' and 'nomask' clauses are now allowed at the same "
            "time\n");
    }

    if ((!omp_mask.is_null()) && (!_vector_isa_desc.support_masking()))
    {
        fatal_error(
            "SIMD: 'mask' clause detected. Masking is not supported by the "
            "underlying architecture\n");
    }

    // Mask Version
    if (_vector_isa_desc.support_masking() && omp_nomask.is_null())
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
    const Nodecl::OpenMP::SimdFunction &simd_node, const bool masked_version)
{
    Nodecl::FunctionCode function_code
        = simd_node.get_statement().as<Nodecl::FunctionCode>();

    // Clone SimdFunction
    TL::Symbol func_sym = function_code.get_symbol();
    std::string orig_func_name = func_sym.get_name();

    // Set new vector function symbol
    std::stringstream vector_func_name;

    TL::Counter &counter = TL::CounterManager::get_counter("simd-function");
    vector_func_name << "__" << orig_func_name << "_" << (int)counter << "_"
                     << _vector_isa_desc.get_id(); 
    counter++;

    if (masked_version)
    {
        vector_func_name << "_mask";
    }

    // Remove template parameters, if any
    decl_context_t *new_func_decl_context
        = decl_context_clone(func_sym.get_scope().get_decl_context());
    new_func_decl_context->template_parameters = NULL;

    TL::Symbol new_func_sym
        = TL::Scope(new_func_decl_context).new_symbol(vector_func_name.str());
    new_func_sym.get_internal_symbol()->kind = SK_FUNCTION;

    Nodecl::Utils::SimpleSymbolMap func_sym_map;
    func_sym_map.add_map(func_sym, new_func_sym);

    Nodecl::OpenMP::SimdFunction simd_node_copy
        = Nodecl::Utils::deep_copy(simd_node, simd_node, func_sym_map)
              .as<Nodecl::OpenMP::SimdFunction>();

    Nodecl::FunctionCode vector_func_code
        = simd_node_copy.get_statement().as<Nodecl::FunctionCode>();

    FunctionDeepCopyFixVisitor fix_deep_copy_visitor(func_sym, new_func_sym);
    fix_deep_copy_visitor.walk(vector_func_code.get_statements());

    // Register new simd nodes in analysis
    Vectorization::Vectorizer::_vectorizer_analysis->register_identical_copy(
        function_code, vector_func_code);

    // Process clauses FROM THE COPY
    Nodecl::List omp_environment
        = simd_node_copy.get_environment().as<Nodecl::List>();
    // Process common simd clauses
    map_nodecl_int_t aligned_expressions;
    map_tlsym_int_t linear_symbols;
    objlist_tlsym_t uniform_symbols;
    objlist_nodecl_t suitable_expressions;
    map_tlsym_objlist_t nontemporal_expressions;
    unsigned int vectorlength_in_elements;
    TL::Type vectorlengthfor_type;
    map_tlsym_objlist_int_t overlap_symbols;
    Vectorization::prefetch_info_t prefetch_info;

    process_common_simd_clauses(omp_environment,
                             aligned_expressions,
                             linear_symbols,
                             uniform_symbols,
                             suitable_expressions,
                             vectorlength_in_elements,
                             vectorlengthfor_type,
                             nontemporal_expressions,
                             overlap_symbols,
                             prefetch_info);

    // Compute the vec factor based on the information of the clauses,
    // if any, or analyzing the code
    unsigned int vec_factor = compute_vec_factor(vector_func_code,
                                                 vectorlength_in_elements,
                                                 vectorlengthfor_type,
                                                 _vector_isa_desc);

    // Vectorizer Environment
    VectorizerEnvironment function_environment(_vector_isa_desc,
                                               vec_factor,
                                               _fast_math_enabled,
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
    //set_initial_mask(_environment, loop_statement);

    // Add SIMD version to vector function versioning
    TL::Type function_return_type = func_sym.get_type().returns();

    vec_func_versioning.add_version(func_sym,
                                    vector_func_code,
                                    _vector_isa_desc.get_id(),
                                    function_environment._vec_factor,
                                    masked_version,
                                    TL::Vectorization::SIMD_FUNC_PRIORITY);

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
             it_classes
         = function_environment._vectorized_classes.begin();
         it_classes != function_environment._vectorized_classes.end();
         it_classes++)
    {
        Nodecl::NodeclBase translation_unit = CURRENT_COMPILED_FILE->nodecl;
        Nodecl::List top_level = translation_unit.as<Nodecl::TopLevel>()
                                     .get_top_level()
                                     .as<Nodecl::List>();

        for (Nodecl::List::iterator it_nodes = top_level.begin();
             it_nodes != top_level.end();
             it_nodes++)
        {
            if (it_nodes->is<Nodecl::CxxDef>()
                && it_nodes->get_symbol() == it_classes->first.get_symbol())
            {
                it_nodes->append_sibling(
                    Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
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
    const Nodecl::OpenMP::SimdFunction &simd_node, const bool masked_version)
{
    Nodecl::FunctionCode function_code
        = simd_node.get_statement().as<Nodecl::FunctionCode>();

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
        TL::Type target_type
            = target_type_heuristic.get_target_type(function_code);

        int _vec_factor
            = _vector_isa_desc.get_vec_factor_from_type(target_type);

        TL::Type function_return_type = func_sym.get_type().returns();

        vector_func_code = vec_func_versioning
                               .get_best_version(func_sym,
                                                 _vector_isa_desc.get_id(),
                                                 _vec_factor,
                                                 masked_version)
                               .as<Nodecl::FunctionCode>();

        ERROR_CONDITION(vector_func_code.is_null()
                            || !vector_func_code.is<Nodecl::FunctionCode>(),
                        "This code must be a FunctionCode",
                        0);
    }

    Nodecl::NodeclBase simd_node_copy = vector_func_code.get_parent();
    ERROR_CONDITION(simd_node_copy.is_null()
                        || !simd_node_copy.is<Nodecl::OpenMP::SimdFunction>(),
                    "Invalid node, expecting a Nodecl::OpenMP::SimdFunction",
                    0);

    Nodecl::List omp_environment
        = simd_node_copy.as<Nodecl::OpenMP::SimdFunction>()
              .get_environment()
              .as<Nodecl::List>();

    // Process common simd clauses
    map_nodecl_int_t aligned_expressions;
    map_tlsym_int_t linear_symbols;
    objlist_tlsym_t uniform_symbols;
    objlist_nodecl_t suitable_expressions;
    map_tlsym_objlist_t nontemporal_expressions;
    unsigned int vectorlength_in_elements;
    TL::Type vectorlengthfor_type;
    map_tlsym_objlist_int_t overlap_symbols;
    Vectorization::prefetch_info_t prefetch_info;

    process_common_simd_clauses(omp_environment,
                             aligned_expressions,
                             linear_symbols,
                             uniform_symbols,
                             suitable_expressions,
                             vectorlength_in_elements,
                             vectorlengthfor_type,
                             nontemporal_expressions,
                             overlap_symbols,
                             prefetch_info);

    // Compute the vec factor based on the information of the clauses,
    // if any, or analyzing the code
    unsigned int vec_factor = compute_vec_factor(vector_func_code,
                                                 vectorlength_in_elements,
                                                 vectorlengthfor_type,
                                                 _vector_isa_desc);

    // Vectorizer Environment
    VectorizerEnvironment function_environment(_vector_isa_desc,
                                               vec_factor,
                                               _fast_math_enabled,
                                               aligned_expressions,
                                               linear_symbols,
                                               uniform_symbols,
                                               suitable_expressions,
                                               nontemporal_expressions,
                                               overlap_symbols,
                                               NULL,
                                               NULL);

    function_environment.load_environment(vector_func_code);

    // Register new simd nodes in analysis
    Vectorization::Vectorizer::_vectorizer_analysis->register_identical_copy(
        function_code, vector_func_code);

    // Append vectorized function code to scalar function
    simd_node.append_sibling(vector_func_code);

    // // Add SIMD version to vector function versioning
    // TL::Type function_return_type = func_sym.get_type().returns();
    // _vectorizer.add_vector_function_version(
    //         func_sym,
    //         vector_func_code, _device_name,
    //         function_return_type.get_size() *
    //         function_environment._vec_factor,
    //         function_return_type, masked_version,
    //         TL::Vectorization::SIMD_FUNC_PRIORITY, false);

    // Set initial mask for vectorization
    set_initial_mask(function_environment,
                     Nodecl::Utils::skip_contexts_and_lists(
                         vector_func_code.get_statements()));

    _vectorizer.vectorize_function(
        vector_func_code, function_environment, masked_version);

    function_environment.unload_environment();
    _vectorizer.postprocess_code(simd_node);

    Nodecl::NodeclBase translation_unit = CURRENT_COMPILED_FILE->nodecl;
    Nodecl::List top_level = translation_unit.as<Nodecl::TopLevel>()
                                 .get_top_level()
                                 .as<Nodecl::List>();

    // Add extra CxxDef that may have been required during vectorization
    // std::cerr << "(2) ENV = " << &func_env << std::endl;
    for (TL::ObjectList<VectorizerEnvironment::VectorizedClass>::iterator
             it_classes
         = function_environment._vectorized_classes.begin();
         it_classes != function_environment._vectorized_classes.end();
         it_classes++)
    {
        for (Nodecl::List::iterator it_nodes = top_level.begin();
             it_nodes != top_level.end();
             it_nodes++)
        {
            if (it_nodes->is<Nodecl::CxxDef>()
                && it_nodes->get_symbol() == it_classes->first.get_symbol())
            {
                it_nodes->append_sibling(
                    Nodecl::CxxDef::make(Nodecl::NodeclBase::null(),
                                         it_classes->second.get_symbol()));

                break;
            }
        }
    }
    function_environment._vectorized_classes.clear();

    // Add CxxDecl of the SIMD function
    if (IS_CXX_LANGUAGE)
    {
        for (const auto &tl_node : top_level)
        {
            if (tl_node.is<Nodecl::CxxDecl>()
                && tl_node.get_symbol() == func_sym)
            {
                tl_node.append_sibling(Nodecl::CxxDecl::make(
                    Nodecl::Context::make(Nodecl::NodeclBase::null(),
                                          func_sym.get_scope(),
                                          tl_node.get_locus()),
                    vector_func_code.get_symbol(),
                    tl_node.get_locus()));
                break;
            }
        }
    }

    // Remove SimdFunction node
    // Nodecl::Utils::remove_from_enclosing_list(simd_node);
    // Free analysis
    //_vectorizer.finalize_analysis();

    // Prostprocess code
}

FunctionDeepCopyFixVisitor::FunctionDeepCopyFixVisitor(
    const TL::Symbol &orig_symbol, const TL::Symbol &new_symbol)
    : _orig_symbol(orig_symbol), _new_symbol(new_symbol)
{
}

void FunctionDeepCopyFixVisitor::visit(const Nodecl::Symbol &n)
{
    if (n.get_symbol() == _new_symbol)
    {
        n.replace(_orig_symbol.make_nodecl(false, n.get_locus()));
    }
}
}
}
