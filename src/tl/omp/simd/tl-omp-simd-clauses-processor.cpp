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

#include "tl-omp-simd-clauses-processor.hpp"

#include "tl-nodecl.hpp"
#include "cxx-cexpr.h"

using namespace TL::Vectorization;

namespace TL
{
namespace OpenMP
{

void process_aligned_clause(const Nodecl::List &environment,
                            map_nodecl_int_t &aligned_expressions_map)
{
    TL::ObjectList<Nodecl::OpenMP::Aligned> omp_aligned_list
        = environment.find_all<Nodecl::OpenMP::Aligned>();

    for (TL::ObjectList<Nodecl::OpenMP::Aligned>::iterator it
         = omp_aligned_list.begin();
         it != omp_aligned_list.end();
         it++)
    {
        Nodecl::OpenMP::Aligned &omp_aligned = *it;

        objlist_nodecl_t aligned_expressions_list
            = omp_aligned.get_aligned_expressions()
                  .as<Nodecl::List>()
                  .to_object_list();

        int alignment
            = const_value_cast_to_signed_int(omp_aligned.get_alignment()
                                                 .as<Nodecl::IntegerLiteral>()
                                                 .get_constant());

        for (const auto &it2 : aligned_expressions_list)
        {
            if (!aligned_expressions_map
                     .insert(std::pair<Nodecl::NodeclBase, int>(it2, alignment))
                     .second)
            {
                fatal_error(
                    "SIMD: multiple instances of the same variable in the "
                    "'aligned' clause detected\n");
            }
        }
    }
}

void process_linear_clause(const Nodecl::List &environment,
                           map_tlsym_int_t &linear_symbols_map)
{
    TL::ObjectList<Nodecl::OpenMP::Linear> omp_linear_list
        = environment.find_all<Nodecl::OpenMP::Linear>();

    for (TL::ObjectList<Nodecl::OpenMP::Linear>::iterator it
         = omp_linear_list.begin();
         it != omp_linear_list.end();
         it++)
    {
        Nodecl::OpenMP::Linear &omp_linear = *it;

        objlist_nodecl_t linear_symbols_list
            = omp_linear.get_linear_expressions()
                  .as<Nodecl::List>()
                  .to_object_list();

        int step = const_value_cast_to_signed_int(
            omp_linear.get_step().as<Nodecl::IntegerLiteral>().get_constant());

        for (objlist_nodecl_t::iterator it2 = linear_symbols_list.begin();
             it2 != linear_symbols_list.end();
             it2++)
        {

            if (!linear_symbols_map
                     .insert(std::pair<TL::Symbol, int>(
                         it2->as<Nodecl::Symbol>().get_symbol(), step))
                     .second)
            {
                fatal_error(
                    "SIMD: multiple instances of the same variable "
                    "in the 'linear' clause detected\n");
            }
        }
    }
}

void process_uniform_clause(const Nodecl::List &environment,
                            objlist_tlsym_t &uniform_symbols)
{
    Nodecl::OpenMP::Uniform omp_uniform
        = environment.find_first<Nodecl::OpenMP::Uniform>();

    if (!omp_uniform.is_null())
    {
        objlist_nodecl_t uniform_symbols_list
            = omp_uniform.get_uniform_expressions()
                  .as<Nodecl::List>()
                  .to_object_list();

        for (objlist_nodecl_t::iterator it2 = uniform_symbols_list.begin();
             it2 != uniform_symbols_list.end();
             it2++)
        {
            uniform_symbols.insert(it2->as<Nodecl::Symbol>().get_symbol());
        }
    }
}

void process_suitable_clause(const Nodecl::List &environment,
                             objlist_nodecl_t &suitable_expressions)
{
    Nodecl::OpenMP::Suitable omp_suitable
        = environment.find_first<Nodecl::OpenMP::Suitable>();

    if (!omp_suitable.is_null())
    {
        suitable_expressions = omp_suitable.get_suitable_expressions()
                                   .as<Nodecl::List>()
                                   .to_object_list();
    }
}

void process_nontemporal_clause(const Nodecl::List &environment,
                                map_tlsym_objlist_t &nontemporal_expressions)
{
    TL::ObjectList<Nodecl::OpenMP::Nontemporal> omp_nontemporal_list
        = environment.find_all<Nodecl::OpenMP::Nontemporal>();

    for (const auto &omp_nontemporal : omp_nontemporal_list)
    {
        objlist_nodecl_t nontemporal_expressions_list
            = omp_nontemporal.get_nontemporal_expressions()
                  .as<Nodecl::List>()
                  .to_object_list();

        objlist_nodecl_t nontemporal_flags
            = omp_nontemporal.get_flags().as<Nodecl::List>().to_object_list();

        for (objlist_nodecl_t::iterator it2
             = nontemporal_expressions_list.begin();
             it2 != nontemporal_expressions_list.end();
             it2++)
        {

            if (!nontemporal_expressions
                     .insert(
                         std::make_pair(it2->as<Nodecl::Symbol>().get_symbol(),
                                        nontemporal_flags))
                     .second)
            {
                fatal_error(
                    "SIMD: multiple instances of the same variable in the "
                    "'aligned' clause detectedn\n");
            }
        }
    }
}

void process_unroll_clause(const Nodecl::List &environment,
                           unsigned int& unroll_factor)
{
    Nodecl::OpenMP::Unroll omp_unroll
        = environment.find_first<Nodecl::OpenMP::Unroll>();

    unroll_factor = 0;
    if (!omp_unroll.is_null())
    {
        Nodecl::NodeclBase unroll_factor_node = omp_unroll.get_unroll_factor();

        if (unroll_factor_node.is_constant())
        {
            unroll_factor
                = const_value_cast_to_4(unroll_factor_node.get_constant());
        }
    }
}

void process_unroll_and_jam_clause(const Nodecl::List &environment,
                                   unsigned int& unroll_and_jam_factor)
{
    Nodecl::OpenMP::UnrollAndJam omp_unroll
        = environment.find_first<Nodecl::OpenMP::UnrollAndJam>();

    unroll_and_jam_factor = 0;
    if (!omp_unroll.is_null())
    {
        Nodecl::NodeclBase unroll_and_jam_factor_node
            = omp_unroll.get_unroll_factor();

        if (unroll_and_jam_factor_node.is_constant())
        {
            unroll_and_jam_factor = const_value_cast_to_4(
                unroll_and_jam_factor_node.get_constant());
        }
    }
}

// TODO: In the context of RoMoL, allowing a Nodecl as vectorlength has sense
void process_vectorlength_clause(const Nodecl::List &environment,
                                 unsigned int &vectorlength)
{
    Nodecl::OpenMP::VectorLength omp_vector_length
        = environment.find_first<Nodecl::OpenMP::VectorLength>();

    if (!omp_vector_length.is_null())
    {
        Nodecl::NodeclBase vectorlength_node
            = omp_vector_length.get_vector_length();

        ERROR_CONDITION(
            !vectorlength_node.is_constant(),
            "Support for non-constant vectorlength is not implemented yet.",
            0);

        vectorlength
            = const_value_cast_to_signed_int(vectorlength_node.get_constant());
    }
    else
        vectorlength = 0;
}

void process_vectorlengthfor_clause(const Nodecl::List &environment,
                                    TL::Type &vectorlengthfor_type)
{
    Nodecl::OpenMP::VectorLengthFor omp_vector_length_for
        = environment.find_first<Nodecl::OpenMP::VectorLengthFor>();

    if (!omp_vector_length_for.is_null())
    {
        vectorlengthfor_type = omp_vector_length_for.get_type();
    }
}

void process_overlap_clause(const Nodecl::List &environment,
                            map_tlsym_objlist_int_t &overlap_symbols)
{
    TL::ObjectList<Nodecl::OpenMP::Overlap> omp_overlap_list
        = environment.find_all<Nodecl::OpenMP::Overlap>();

    for (TL::ObjectList<Nodecl::OpenMP::Overlap>::iterator it
         = omp_overlap_list.begin();
         it != omp_overlap_list.end();
         it++)
    {
        Nodecl::OpenMP::Overlap &omp_overlap = *it;

        objlist_nodecl_t overlap_symbols_list
            = omp_overlap.get_overlap_expressions()
                  .as<Nodecl::List>()
                  .to_object_list();

        int min_group_loads = const_value_cast_to_signed_int(
            it->get_min_group_loads().get_constant());
        int max_group_registers = const_value_cast_to_signed_int(
            it->get_max_group_registers().get_constant());
        int max_groups = const_value_cast_to_signed_int(
            it->get_max_groups().get_constant());

        for (objlist_nodecl_t::iterator it2 = overlap_symbols_list.begin();
             it2 != overlap_symbols_list.end();
             it2++)
        {
            objlist_int_t overlap_params(3);
            overlap_params[0] = min_group_loads;
            overlap_params[1] = max_group_registers;
            overlap_params[2] = max_groups;


            if (!overlap_symbols
                     .insert(std::pair<TL::Symbol, objlist_int_t>(
                         it2->as<Nodecl::Symbol>().get_symbol(),
                         overlap_params))
                     .second)
            {
                fatal_error(
                    "SIMD: multiple instances of the same variable in the "
                    "'overlap' clause detected\n");
            }
        }
    }
}

void process_prefetch_clause(const Nodecl::List &environment,
                             Vectorization::prefetch_info_t &prefetch_info)
{
    TL::ObjectList<Nodecl::OpenMP::Prefetch> omp_prefetch_list
        = environment.find_all<Nodecl::OpenMP::Prefetch>();

    ERROR_CONDITION(
        omp_prefetch_list.size() > 1, "Too many OpenMP::Prefetch nodes", 0);

    if (omp_prefetch_list.size() == 1)
    {
        Nodecl::OpenMP::Prefetch &omp_prefetch = *omp_prefetch_list.begin();

        objlist_nodecl_t prefetch_distances_list
            = omp_prefetch.get_distances().as<Nodecl::List>().to_object_list();

        ERROR_CONDITION(prefetch_distances_list.size() != 2,
                        "Prefetch distances must be 2",
                        0);

        prefetch_info.enabled = true;

        prefetch_info.distances[1] = const_value_cast_to_signed_int(
            prefetch_distances_list[0].get_constant()); // L2 distance
        prefetch_info.distances[0] = const_value_cast_to_signed_int(
            prefetch_distances_list[1].get_constant()); // L1 distance

        Nodecl::NodeclBase strategy = omp_prefetch.get_strategy();

        if (strategy.is<Nodecl::OnTopFlag>())
            prefetch_info.in_place = false;
        else if (strategy.is<Nodecl::InPlaceFlag>())
            prefetch_info.in_place = true;
        else
        {
            internal_error(
                "Prefetch strategy is neither OnTopFlag nor InPlaceFlag\n", 0);
        }
    }
    else
    {
        prefetch_info.enabled = false;
    }
}

Nodecl::List process_reduction_clause(
    const Nodecl::List &environment,
    TL::ObjectList<TL::Symbol> &reductions,
    std::map<TL::Symbol, TL::Symbol> &new_external_vector_symbol_map,
    TL::Scope enclosing_scope,
    unsigned int vec_factor)
{
    Nodecl::List omp_reduction_list;

    for (Nodecl::List::const_iterator it = environment.begin();
         it != environment.end();
         it++)
    {
        if (it->is<Nodecl::OpenMP::Reduction>()
            || it->is<Nodecl::OpenMP::SimdReduction>())
        {
            Nodecl::OpenMP::Reduction omp_reductions
                = it->as<Nodecl::OpenMP::Reduction>();

            // Extract reduced Nodecl::Symbol from ReductionItems
            omp_reduction_list
                = omp_reductions.get_reductions().as<Nodecl::List>();
            for (Nodecl::List::iterator it2 = omp_reduction_list.begin();
                 it2 != omp_reduction_list.end();
                 it2++)
            {
                TL::Symbol red_sym = it2->as<Nodecl::OpenMP::ReductionItem>()
                                         .get_reduced_symbol()
                                         .as<Nodecl::Symbol>()
                                         .get_symbol();

                reductions.append(red_sym);

                // Add new vector TL::Symbol in the enclosing context
                TL::Symbol new_red_sym = enclosing_scope.new_symbol(
                    "__vred_" + red_sym.get_name());
                new_red_sym.get_internal_symbol()->kind = SK_VARIABLE;
                symbol_entity_specs_set_is_user_declared(
                    new_red_sym.get_internal_symbol(), 1);

                new_red_sym.set_type(
                    red_sym.get_type().get_vector_of_elements(vec_factor));

                // Add new TL::Symbol to map
                new_external_vector_symbol_map.insert(
                    std::pair<TL::Symbol, TL::Symbol>(red_sym, new_red_sym));
            }
        }
    }

    return omp_reduction_list;
}

void process_common_simd_clauses(
    const Nodecl::List &omp_environment,
    map_nodecl_int_t &aligned_expressions,
    map_tlsym_int_t &linear_symbols,
    objlist_tlsym_t &uniform_symbols,
    objlist_nodecl_t &suitable_expressions,
    unsigned int &vectorlength_in_elements,
    TL::Type &vectorlengthfor_type,
    map_tlsym_objlist_t &nontemporal_expressions,
    map_tlsym_objlist_int_t &overlap_symbols,
    prefetch_info_t &prefetch_info)
{
    process_aligned_clause(omp_environment, aligned_expressions);
    process_linear_clause(omp_environment, linear_symbols);
    process_uniform_clause(omp_environment, uniform_symbols);
    process_suitable_clause(omp_environment, suitable_expressions);
    process_vectorlength_clause(omp_environment, vectorlength_in_elements);
    process_vectorlengthfor_clause(omp_environment, vectorlengthfor_type);
    process_nontemporal_clause(omp_environment, nontemporal_expressions);
    process_overlap_clause(omp_environment, overlap_symbols);
    process_prefetch_clause(omp_environment, prefetch_info);
}

void process_loop_simd_clauses(const Nodecl::List &omp_environment,
                               unsigned int &unroll_factor,
                               unsigned int &unroll_and_jam_factor)
{
    process_unroll_clause(omp_environment, unroll_factor);
    process_unroll_and_jam_clause(omp_environment, unroll_and_jam_factor);
}

}
}
