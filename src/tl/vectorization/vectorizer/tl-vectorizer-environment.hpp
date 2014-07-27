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

#ifndef TL_VECTORIZER_ENVIRONMENT_HPP
#define TL_VECTORIZER_ENVIRONMENT_HPP


#include "tl-vectorizer-environment-fwd.hpp"

#include "tl-vectorization-common.hpp"
#include "tl-vectorizer-overlap.hpp"

namespace TL
{
namespace Vectorization
{
    class VectorizerEnvironment
    {
        public:
            const std::string& _device;
            const unsigned int _vector_length;
            const bool _support_masking;
            const unsigned int _mask_size;
            const bool _fast_math;
            const tl_sym_int_map_t& _aligned_symbols_map;
            const tl_sym_int_map_t& _linear_symbols_map;
            const objlist_tlsymbol_t& _uniform_symbols_list;
            const objlist_nodecl_t& _suitable_exprs_list;
            const nontmp_expr_map_t& _nontemporal_exprs_map;
            const tl_sym_int_map_t& _overlap_symbols_map;

            const objlist_tlsymbol_t* _reduction_list;
            std::map<TL::Symbol, TL::Symbol>* _new_external_vector_symbol_map;

            TL::Type _target_type;
            unsigned int _vectorization_factor;

            stdlist_nodecl_t _analysis_scopes;              // Stack of useful scopes (If, FunctionCode and For) for the analysis
            Nodecl::NodeclBase _analysis_simd_scope;        // SIMD scope

            stdlist_nodecl_t _mask_list;                    // Stack of masks
            std::list<bool> _inside_inner_masked_bb;        // TBD :)
            std::list<unsigned int> _mask_check_bb_cost;    // Costs of BB for early exist heuristic

            TL::Symbol _function_return;                    // Return symbol when return statement are present in masked code

            VectorizerEnvironment(const std::string& device,
                    const unsigned int vector_length,
                    const bool support_masking,
                    const unsigned int mask_size,
                    const bool fast_math,
                    const TL::Type& target_type,
                    const tl_sym_int_map_t& aligned_symbol_map,
                    const tl_sym_int_map_t& linear_symbol_map,
                    const objlist_tlsymbol_t& uniform_expr_list,
                    const objlist_nodecl_t& suitable_expr_list,
                    const nontmp_expr_map_t& nontemporal_expr_map,
                    const tl_sym_int_map_t& overlap_symbols_map,
                    const objlist_tlsymbol_t* reduction_list,
                    std::map<TL::Symbol, TL::Symbol>* new_external_vector_symbol_map);

            ~VectorizerEnvironment();

            void set_target_type(TL::Type target_type);

            void load_environment(const Nodecl::NodeclBase& for_statement);
            void unload_environment();
    };
}
}

#endif // TL_VECTORIZER_ENVIRONMENT_HPP
