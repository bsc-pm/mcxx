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

#ifndef TL_VECTORIZER_ENVIRONMENT_HPP
#define TL_VECTORIZER_ENVIRONMENT_HPP


#include "tl-vectorization-common.hpp"
#include "tl-vector-isa-descriptor.hpp"
#include "tl-nodecl-base.hpp"

namespace TL
{
namespace Vectorization
{
    class VectorizerEnvironment
    {
        public:
            const VectorIsaDescriptor& _vec_isa_desc;
            unsigned int _vec_factor;
            const bool _fast_math;
            const map_nodecl_int_t& _aligned_symbols_map;
            const map_tlsym_int_t& _linear_symbols_map;
            const objlist_tlsym_t& _uniform_symbols_list;
            const objlist_nodecl_t& _suitable_exprs_list;
            const map_tlsym_objlist_t& _nontemporal_exprs_map;
            const map_tlsym_objlist_int_t& _overlap_symbols_map;

            const objlist_tlsym_t* _reduction_list;
            std::map<TL::Symbol, TL::Symbol>* _new_external_vector_symbol_map;

            stdlist_nodecl_t _analysis_scopes;              // Stack of useful scopes (If, FunctionCode and For) for the analysis
            Nodecl::NodeclBase _analysis_simd_scope;        // SIMD scope

            stdlist_nodecl_t _mask_list;                    // Stack of masks
            std::list<bool> _inside_inner_masked_bb;        // TBD :)
            std::list<unsigned int> _mask_check_bb_cost;    // Costs of BB for early exist heuristic

            TL::Symbol _function_return;                    // Return symbol when return statement are present in masked code

            // FIXME - find a better place for this sort of things
            typedef std::pair<TL::Type, TL::Type> VectorizedClass;
            TL::ObjectList<VectorizedClass> _vectorized_classes;

            VectorizerEnvironment(
                const VectorIsaDescriptor &vec_isa_desc,
                const unsigned int vec_factor,
                const bool fast_math,
                const map_nodecl_int_t &aligned_symbol_map,
                const map_tlsym_int_t &linear_symbol_map,
                const objlist_tlsym_t &uniform_expr_list,
                const objlist_nodecl_t &suitable_expr_list,
                const map_tlsym_objlist_t &nontemporal_expr_map,
                const map_tlsym_objlist_int_t &overlap_symbols_map,
                const objlist_tlsym_t *reduction_list,
                std::map<TL::Symbol, TL::Symbol>
                    *new_external_vector_symbol_map);

            ~VectorizerEnvironment();

            void load_environment(const Nodecl::NodeclBase& for_statement);
            void unload_environment(const bool clean_masks = true);
    };
}
}

#endif // TL_VECTORIZER_ENVIRONMENT_HPP
