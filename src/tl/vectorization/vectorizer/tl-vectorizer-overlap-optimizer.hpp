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

#ifndef TL_VECTORIZER_OVERLAP_OPTIMIZER_HPP
#define TL_VECTORIZER_OVERLAP_OPTIMIZER_HPP

#include "tl-vectorizer-environment.hpp"
#include "tl-vectorizer-overlap-common.hpp"
#include "tl-vectorization-analysis-interface.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-nodecl.hpp"

#include <map>
#include <vector>


namespace TL
{
    namespace Vectorization
    {
        class OverlappedAccessesOptimizer : 
            public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                const VectorizerEnvironment& _environment;
                const bool _is_omp_simd_for;
                const bool _is_simd_epilog;
                const bool _in_place;

                Nodecl::List& _prependix_stmts;
                
                VectorizationAnalysisInterface* _first_analysis;

                static VectorizationAnalysisInterface* _analysis;

                void update_alignment_info(
                        const Nodecl::NodeclBase& main_loop,
                        const Nodecl::NodeclBase& epilog_loop);

                objlist_nodecl_t get_adjacent_vector_loads_not_nested_in_for(
                        const Nodecl::NodeclBase& n,
                        const TL::Symbol& sym);

                void retrieve_group_registers(
                        OverlapGroup& ogroup,
                        TL::Scope& scope,
                        const int num_group);

                bool need_init_cache(
                        const bool is_nested_loop,
                        const bool is_simd_epilog,
                        const bool is_overlap_epilog);
                bool need_update_post(
                        const bool is_nested_loop,
                        const bool is_simd_epilog,
                        const bool is_overlap_epilog);
                
                Nodecl::List get_ogroup_init_statements(
                        const OverlapGroup& ogroup,
                        const Nodecl::ForStatement& for_stmt,
                        const bool is_simd_loop,
                        const bool is_omp_simd_for) const;
                Nodecl::NodeclBase get_ogroup_iteration_update_pre(
                        const OverlapGroup& ogroup) const;
                Nodecl::List get_ogroup_iteration_update_post(
                        const OverlapGroup& ogroup) const;

                void insert_group_update_stmts(
                        OverlapGroup& ogroup,
                        const Nodecl::ForStatement& n,
                        const bool is_overlap_epilog);
                void replace_overlapped_loads(OverlapGroup& ogroup,
                        const Nodecl::NodeclBase& nesting_node);

                unsigned int get_loop_min_unroll_factor(
                        Nodecl::ForStatement n);
                Nodecl::ForStatement get_overlap_blocked_unrolled_loop(
                        const Nodecl::ForStatement& n,
                        const unsigned int block_size);

            public:
                OverlappedAccessesOptimizer(VectorizerEnvironment& environment,
                        VectorizationAnalysisInterface* analysis,
                        const bool is_omp_simd_for,
                        const bool is_epilog,
                        const bool overlap_in_place,
                        Nodecl::List& prependix_stmts);
                
                void visit(const Nodecl::ForStatement&);

            friend struct OverlapGroup;

       };
    }
}

#endif // TL_VECTORIZER_OVERLAP_HPP
