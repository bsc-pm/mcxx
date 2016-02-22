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

#ifndef TL_OMP_SIMD_CLAUSES_PROCESSOR_HPP
#define TL_OMP_SIMD_CLAUSES_PROCESSOR_HPP

#include "tl-vectorization-common.hpp"
#include "tl-vectorization-prefetcher-common.hpp"

namespace TL
{
namespace OpenMP
{

void process_common_simd_clauses(
    const Nodecl::List &omp_environment,
    Vectorization::map_nodecl_int_t &aligned_expressions,
    Vectorization::map_tlsym_int_t &linear_symbols,
    Vectorization::objlist_tlsym_t &uniform_symbols,
    Vectorization::objlist_nodecl_t &suitable_expressions,
    unsigned int &vectorlength_in_elements,
    TL::Type &vectorlengthfor_type,
    Vectorization::map_tlsym_objlist_t &nontemporal_expressions,
    Vectorization::map_tlsym_objlist_int_t &overlap_symbols,
    Vectorization::prefetch_info_t &prefetch_info);

void process_loop_simd_clauses(const Nodecl::List &omp_environment,
                               unsigned int &unroll_factor,
                               unsigned int &unroll_and_jam_factor);

Nodecl::List process_reduction_clause(
    const Nodecl::List &environment,
    TL::ObjectList<TL::Symbol> &reductions,
    std::map<TL::Symbol, TL::Symbol> &new_external_vector_symbol_map,
    TL::Scope enclosing_scope,
    unsigned int vec_factor);
}
}


#endif // TL_OMP_SIMD_CLAUSES_PROCESSOR_HPP
