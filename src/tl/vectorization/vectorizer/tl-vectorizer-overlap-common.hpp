/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#ifndef TL_VECTORIZER_OVERLAP_COMMON_HPP
#define TL_VECTORIZER_OVERLAP_COMMON_HPP

#include "tl-vectorizer-environment.hpp"
#include "tl-nodecl-visitor.hpp"
#include <map>


namespace TL
{
    namespace Vectorization
    {
        struct OverlapGroup
        {
            Nodecl::NodeclBase _subscripted;
            objlist_tlsym_t _registers;
            objlist_nodecl_t _registers_indexes;
            objlist_nodecl_t _loads;
            Nodecl::VectorLoad _leftmost_code_vload;
            Nodecl::VectorLoad _rightmost_code_vload;
            Nodecl::VectorLoad _leftmost_group_vload;
            Nodecl::VectorLoad _rightmost_group_vload;
            Nodecl::NodeclBase _loop_ind_var;
            Nodecl::NodeclBase _loop_ind_var_step;
            TL::Type _basic_type;
            TL::Type _vector_type;
            int _num_registers;
            bool _aligned_strategy;
            bool _inter_it_overlap;
            bool _is_set_in_place_update_pre;

            bool overlaps(const Nodecl::VectorLoad& vector_load,
                    const bool consider_aligned_adjacent_accesses);

            void compute_leftmost_rightmost_vloads(
                    const Vectorization::VectorizerEnvironment& environment,
                    const int max_registers);
            void leftmost_rightmost_strategy(
                    const Vectorization::VectorizerEnvironment& environment,
                    const bool aligned_strategy);
 
            void compute_basic_properties();
            void compute_inter_iteration_overlap();

            void compute_num_registers(
                    const Vectorization::VectorizerEnvironment& environment);
        };


        typedef TL::ObjectList<OverlapGroup> objlist_ogroup_t;
        
        objlist_ogroup_t get_overlap_groups(
                const objlist_nodecl_t& vector_loads,
                const int min_group_loads,
                const int max_group_registers,
                const int max_groups,
                const Nodecl::NodeclBase& loop_ind_var,
                const Nodecl::NodeclBase& loop_ind_var_step,
                const bool consider_aligned_adjacent_accesses);
    }
}

#endif //TL_VECTORIZER_OVERLAP_COMMON_HPP

