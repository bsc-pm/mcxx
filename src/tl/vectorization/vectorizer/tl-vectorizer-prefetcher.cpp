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

#include "tl-vectorizer-prefetcher.hpp"
#include "tl-vectorizer-overlap-common.hpp"
#include "tl-vectorizer.hpp"
#include "tl-vectorization-utils.hpp"
#include "tl-nodecl-utils.hpp"
#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
    Prefetcher::Prefetcher(const prefetch_info_t& pref_info,
            const int vectorization_factor)
        : _L1_distance(pref_info.L1_distance),
        _L2_distance(pref_info.L2_distance),
        _vectorization_factor(vectorization_factor)
    {
    }

    Nodecl::NodeclBase Prefetcher::get_prefetch_node(
            const Nodecl::NodeclBase& address,
            const PrefetchKind kind,
            const int distance)
    {
        Nodecl::ExpressionStatement prefetch_stmt =
            Nodecl::ExpressionStatement::make(
                    Nodecl::VectorPrefetch::make(
                        address.shallow_copy(),
                        const_value_to_nodecl(const_value_get_signed_int(kind)),
                        address.get_type()));

        for(auto& lv : _linear_vars)
        {
            Nodecl::Add new_lv = 
                Nodecl::Add::make(lv.shallow_copy(),
                        Nodecl::Mul::make(
                            Nodecl::Mul::make(
                                Vectorizer::_vectorizer_analysis->get_linear_step(_loop, lv).shallow_copy(),
                                const_value_to_nodecl(const_value_get_signed_int(_vectorization_factor)),
                                lv.get_type()),
                            const_value_to_nodecl(const_value_get_signed_int(distance)),
                            lv.get_type()),
                        lv.get_type());

            Nodecl::Utils::nodecl_replace_nodecl_by_structure(
                    prefetch_stmt, lv, new_lv);
        }

        VECTORIZATION_DEBUG()
        {
            std::string level;
            std::string rw;

            switch (kind)
            {
                case PrefetchKind::L1_READ:
                    level = "L1";
                    rw = "READ";
                    break;

                case PrefetchKind::L1_WRITE:
                    level = "L1";
                    rw = "WRITE";
                    break;

                case PrefetchKind::L2_READ:
                    level = "L2";
                    rw = "READ";
                    break;

                case PrefetchKind::L2_WRITE:
                    level = "L2";
                    rw = "WRITE";
                    break;
            }

            std::cerr << "Prefeching " << address.prettyprint()
                << " with distance " << distance
                << " to " << level
                << " as " << rw
                << std::endl;
        }

        return prefetch_stmt;
    }

    void Prefetcher::visit(const Nodecl::ForStatement& n)
    {
        _loop = n;
        _linear_vars = Vectorizer::_vectorizer_analysis->get_linear_nodecls(n);

        ERROR_CONDITION(_linear_vars.size() != 1,
                "Linear variables != 1 in a SIMD loop", 0);

        Nodecl::NodeclBase iv = *_linear_vars.begin();
        Nodecl::NodeclBase iv_step = Vectorizer::_vectorizer_analysis->get_linear_step(n, iv);

        Nodecl::NodeclBase stmts = n.get_statement();

        // VectorStores have preference over VectorLoads
        // TODO: Overlap?
        objlist_nodecl_t vector_memory_accesses = Nodecl::Utils::
            nodecl_get_all_nodecls_of_kind<Nodecl::VectorStore>(stmts);

        objlist_nodecl_t vector_loads = Nodecl::Utils::
            nodecl_get_all_nodecls_of_kind<Nodecl::VectorLoad>(stmts);
        // Let's see if vector loads overlap

        objlist_ogroup_t overlap_groups =
            get_overlap_groups(
                    vector_loads,
                    1, //min_group_loads,
                    0, //max_group_registers,
                    0, //max_groups,
                    iv,
                    iv_step);


        for (auto& vload : vector_loads)
        {
           bool is_repeated = false;
           Nodecl::NodeclBase load_scalar_access = 
               Vectorization::Utils::get_scalar_memory_access(vload);
           
           for (auto& vaccess : vector_memory_accesses)
           {
               Nodecl::NodeclBase memory_scalar_access = 
                   Vectorization::Utils::get_scalar_memory_access(vaccess);

               if (Nodecl::Utils::structurally_equal_nodecls(
                           load_scalar_access,
                           memory_scalar_access,
                           true /*skip conv */))
               {
                   is_repeated = true;
                   break;
               }
           }

           if (!is_repeated)
               vector_memory_accesses.push_back(vload);
        }

        objlist_nodecl_t nested_for_stmts = Nodecl::Utils::
            nodecl_get_all_nodecls_of_kind<Nodecl::ForStatement>(stmts);


        //objlist_nodecl_t not_nested_vaccesses =
        _not_nested_vaccesses =
            Vectorization::Utils::get_nodecls_not_contained_in(
                    vector_memory_accesses,
                    nested_for_stmts);

        // Add #pragma noprefetch to the loop
        if (!_not_nested_vaccesses.empty())
        {
            Nodecl::UnknownPragma noprefetch_pragma =
                Nodecl::UnknownPragma::make("noprefetch");

            n.prepend_sibling(noprefetch_pragma);
        }


        /* This doesn't work because of the ObjectInit issue */
        //for (auto& vaccess : not_nested_vaccesses)
        //{
        //    walk(vaccess);
        //}

        walk(stmts);
    }

    void Prefetcher::visit(const Nodecl::VectorLoad& n)
    {
        if (_not_nested_vaccesses.contains(n))
        {
            Nodecl::NodeclBase rhs = n.get_rhs();

            Nodecl::NodeclBase ref_node;
            if (!_object_init.is_null())
                ref_node = _object_init;
            else
                ref_node = n;

            Nodecl::Utils::prepend_statement(ref_node,
                    get_prefetch_node(rhs, L2_READ, _L2_distance));
            Nodecl::Utils::prepend_statement(ref_node,
                    get_prefetch_node(rhs, L1_READ, _L1_distance));
        }
    }

    void Prefetcher::visit(const Nodecl::VectorStore& n)
    {
        if (_not_nested_vaccesses.contains(n))
        {
            Nodecl::NodeclBase lhs = n.get_lhs();
            Nodecl::NodeclBase rhs = n.get_rhs();

            Nodecl::NodeclBase ref_node;
            if (!_object_init.is_null())
                ref_node = _object_init;
            else
                ref_node = n;

            Nodecl::Utils::prepend_statement(ref_node,
                    get_prefetch_node(lhs, L2_WRITE, _L2_distance));
            Nodecl::Utils::prepend_statement(ref_node,
                    get_prefetch_node(lhs, L1_WRITE, _L1_distance));
        }
    }

    void Prefetcher::visit(const Nodecl::ObjectInit& n)
    {
        _object_init = n;

        TL::Symbol sym = n.get_symbol();
        Nodecl::NodeclBase init = sym.get_value();

        if(!init.is_null())
        {
            walk(init);
        }

        _object_init = Nodecl::NodeclBase::null(); 
    }
}
}

