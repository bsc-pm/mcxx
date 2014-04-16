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

#include "tl-vectorizer-environment.hpp"

#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
    VectorizerEnvironment::VectorizerEnvironment(const std::string& device,
            const unsigned int vector_length,
            const bool support_masking,
            const unsigned int mask_size,
            const bool fast_math,
            const TL::Type& target_type,
            const aligned_expr_map_t& aligned_expr_map,
            const objlist_nodecl_t& uniform_expr_list,
            const objlist_nodecl_t& suitable_expr_list,
            const nontmp_expr_map_t& nontemporal_expr_list,
            const VectorizerCache& vectorizer_cache,
            const objlist_tlsymbol_t * reduction_list,
            std::map<TL::Symbol, TL::Symbol> * new_external_vector_symbol_map) :
        _device(device), _vector_length(vector_length),
        _unroll_factor(vector_length/target_type.get_size()),
        _support_masking(support_masking),
        _mask_size(mask_size),
        _fast_math(fast_math),
        _target_type(target_type),
        _aligned_expr_map(aligned_expr_map),
        _uniform_expr_list(uniform_expr_list),
        _suitable_expr_list(suitable_expr_list),
        _nontemporal_expr_map(nontemporal_expr_list),
        _vectorizer_cache(vectorizer_cache),
        _reduction_list(reduction_list),
        _new_external_vector_symbol_map(new_external_vector_symbol_map)
    {
        std::cerr << "VECTORIZER: Target type size: " << _target_type.get_size()
            << " . Unroll factor: " << _unroll_factor << std::endl;

        _inside_inner_masked_bb.push_back(false);
        _mask_check_bb_cost.push_back(0);
    }

    void VectorizerEnvironment::load_environment(
            const Nodecl::NodeclBase& n)
    {
        // Push FunctionCode as scope for analysis
        _analysis_simd_scope = n;
        _analysis_scopes.push_back(n);

        // Add MaskLiteral to mask_list
        Nodecl::MaskLiteral all_one_mask =
            Nodecl::MaskLiteral::make(
                    TL::Type::get_mask_type(_unroll_factor),
                    const_value_get_minus_one(_unroll_factor, 1));
        _mask_list.push_back(all_one_mask);
    }

    void VectorizerEnvironment::unload_environment()
    {
        _analysis_simd_scope = Nodecl::NodeclBase::null();

        _mask_list.clear();
        _analysis_scopes.clear();
    }

    VectorizerEnvironment::~VectorizerEnvironment()
    {
        _inside_inner_masked_bb.pop_back();
        _mask_check_bb_cost.pop_back();
    }
}
}
