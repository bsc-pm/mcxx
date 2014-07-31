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

#ifndef TL_VECTORIZER_OVERLAP_HPP
#define TL_VECTORIZER_OVERLAP_HPP


#include <map>
#include <vector>

#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-vectorizer-overlap-fwd.hpp"
#include "tl-vectorizer-environment.hpp"


namespace TL
{
    namespace Vectorization
    {
        /*
        class OverlapInfo
        {
            private:
                Nodecl::NodeclBase _lower_bound;
                Nodecl::NodeclBase _upper_bound;
                Nodecl::NodeclBase _stride;
                const int _overlap_factor; 

                std::vector<TL::Symbol> _register_list;

            public:
                OverlapInfo(const Nodecl::NodeclBase& lower_bound,
                       const Nodecl::NodeclBase& upper_bound,
                       const Nodecl::NodeclBase& stride,
                       const int overlap_factor);

            friend class VectorizerOverlap;

        };

        class VectorizerOverlap
        {
            private:
                typedef std::map<TL::Symbol, OverlapInfo> overlap_map_t; //Second will be a list
                typedef std::pair<TL::Symbol, OverlapInfo> overlap_pair_t; //Second will be a list

                overlap_map_t _overlap_map;

            public:
                VectorizerOverlap(
                        const tl_sym_int_map_t& overlapd_expressions);

                void declare_overlap_symbols(TL::Scope scope,
                        const VectorizerEnvironment& environment);

                Nodecl::List get_init_statements(VectorizerEnvironment& environment) const;
                Nodecl::List get_iteration_update_pre(VectorizerEnvironment& environment) const;
                Nodecl::List get_iteration_update_post(VectorizerEnvironment& environment) const;

                bool is_overlapd_access(const Nodecl::ArraySubscript& n) const;
                Nodecl::NodeclBase get_load_access(const Nodecl::ArraySubscript& n) const;
        };
*/

        struct OverlapGroup
        {
            Nodecl::Symbol _group_subscripted;
            objlist_tlsymbol_t _group_registers;
            objlist_nodecl_t _group_registers_indexes;
            objlist_nodecl_t _group_loads;
            TL::Type _basic_type;
            TL::Type _vector_type;

            Nodecl::List get_init_statements(
                    const Nodecl::ForStatement& for_stmt) const;
            Nodecl::List get_iteration_update_pre() const;
            Nodecl::List get_iteration_update_post() const;
        };

        typedef TL::ObjectList<OverlapGroup> objlist_ogroup_t;
        class OverlappedAccessesOptimizer : 
            public Nodecl::ExhaustiveVisitor<void>
        {
            private:
                tl_sym_int_map_t _overlap_symbols;
                
                objlist_nodecl_t get_adjacent_vector_loads_nested_in_one_for(
                        const Nodecl::ForStatement& n,
                        const TL::Symbol& sym);
                bool overlap(const Nodecl::VectorLoad& vector_load,
                        objlist_nodecl_t group);
                objlist_ogroup_t get_overlap_groups(
                        const objlist_nodecl_t& adjacent_accesses);

                void compute_group_properties(
                        OverlapGroup& ogroup,
                        const Nodecl::ForStatement& for_stmt);
                void enable_overlap_cache(const OverlapGroup& ogroup,
                        const Nodecl::ForStatement& n);
                void replace_overlapped_loads(
                        const OverlapGroup& ogroup);

                Nodecl::NodeclBase get_vector_load_subscripted(
                        const Nodecl::VectorLoad& vl);
                Nodecl::NodeclBase get_vector_load_subscript(
                        const Nodecl::VectorLoad& vl);
 
            public:
                OverlappedAccessesOptimizer(VectorizerEnvironment& environment);
                
                void visit(const Nodecl::ForStatement&);

       };
    }
}

#endif // TL_VECTORIZER_OVERLAP_HPP
