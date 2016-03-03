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

#include "tl-vectorizer-environment.hpp"
#include "tl-vectorization-utils.hpp"

#include "tl-nodecl.hpp"
#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
VectorizerEnvironment::VectorizerEnvironment(
    const VectorIsaDescriptor &vec_isa_desc,
    const unsigned int vec_factor,
    const bool fast_math,
    const map_nodecl_int_t &aligned_symbols_map,
    const map_tlsym_int_t &linear_symbols_map,
    const objlist_tlsym_t &uniform_symbols_list,
    const objlist_nodecl_t &suitable_exprs_list,
    const map_tlsym_objlist_t &nontemporal_exprs_list,
    const map_tlsym_objlist_int_t &overlap_symbols_map,
    const objlist_tlsym_t *reduction_list,
    std::map<TL::Symbol, TL::Symbol> *new_external_vector_symbol_map)
    : _vec_isa_desc(vec_isa_desc),
      _vec_factor(vec_factor),
      _fast_math(fast_math),
      _aligned_symbols_map(aligned_symbols_map),
      _linear_symbols_map(linear_symbols_map),
      _uniform_symbols_list(uniform_symbols_list),
      _suitable_exprs_list(suitable_exprs_list),
      _nontemporal_exprs_map(nontemporal_exprs_list),
      _overlap_symbols_map(overlap_symbols_map),
      _reduction_list(reduction_list),
      _new_external_vector_symbol_map(new_external_vector_symbol_map)
{
    _inside_inner_masked_bb.push_back(false);
    _mask_check_bb_cost.push_back(0);
    }

    VectorizerEnvironment::~VectorizerEnvironment()
    {
        _inside_inner_masked_bb.pop_back();
        _mask_check_bb_cost.pop_back();
    }

    void VectorizerEnvironment::load_environment(
            const Nodecl::NodeclBase& n)
    {
        // Push FunctionCode as scope for analysis
        _analysis_simd_scope = n;
        _analysis_scopes.push_back(n);
    }

    void VectorizerEnvironment::unload_environment(const bool clean_masks)
    {
        _function_return = TL::Symbol();
        _analysis_simd_scope = Nodecl::NodeclBase::null();

        if (clean_masks)
            _mask_list.clear();

        _analysis_scopes.clear();
    }
}
}
