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
            const VectorizerEnvironment& environment)
        : _pref_info(pref_info), _environment(environment)
    {
    }

    void Prefetcher::visit(const Nodecl::ForStatement& n)
    {
        objlist_nodecl_t linear_vars = Vectorizer::_vectorizer_analysis->get_linear_nodecls(n);

        ERROR_CONDITION(linear_vars.size() != 1,
                "Linear variables != 1 in a SIMD loop", 0);

        Nodecl::NodeclBase iv = *linear_vars.begin();
        Nodecl::NodeclBase iv_step = Vectorizer::_vectorizer_analysis->get_linear_step(n, iv);

        Nodecl::NodeclBase stmts = n.get_statement();

        objlist_nodecl_t nested_for_stmts = Nodecl::Utils::
            nodecl_get_all_nodecls_of_kind<Nodecl::ForStatement>(stmts);

        objlist_nodecl_t vector_stores = Utils::get_nodecls_not_contained_in(
                Nodecl::Utils::nodecl_get_all_nodecls_of_kind<Nodecl::VectorStore>(stmts),
                nested_for_stmts);

        objlist_nodecl_t vector_loads = Utils::get_nodecls_not_contained_in(
                Nodecl::Utils::nodecl_get_all_nodecls_of_kind<Nodecl::VectorLoad>(stmts),
                nested_for_stmts);

        map_nodecl_nodecl_t not_nested_vaccesses;

        // Process VectorStores
        // They have preference over VectorLoads
        for (auto& vstore : vector_stores)
        {
            bool is_new_access = true;
            Nodecl::NodeclBase store_scalar_access = 
                Vectorization::Utils::get_scalar_memory_access(vstore);

            // Do not emit prefetching instructions for nontemporal stores
            if (_environment._nontemporal_exprs_map.find(
                        Utils::get_subscripted_symbol(store_scalar_access.as<Nodecl::ArraySubscript>().
                            get_subscripted())) != _environment._nontemporal_exprs_map.end())
            {
                continue;
            }

            for (auto& vaccess : not_nested_vaccesses)
            {
                Nodecl::NodeclBase memory_scalar_access = 
                    Vectorization::Utils::get_scalar_memory_access(vaccess.first);

                if (Nodecl::Utils::structurally_equal_nodecls(
                            store_scalar_access,
                            memory_scalar_access,
                            true /*skip conv */))
                {
                    is_new_access = false;
                    break;
                }
            }

            if (is_new_access)
            {
                not_nested_vaccesses.insert(
                        pair_nodecl_nodecl_t(vstore, vstore));
            }
        }

        // Let's see if vector loads overlap
        // TODO: Vector Stores?
        objlist_ogroup_t overlap_groups =
            get_overlap_groups(
                    vector_loads,
                    1, //min_group_loads,
                    0, //max_group_registers,
                    0, //max_groups,
                    iv,
                    iv_step,
                    true /* consider aligned adjacent accesses */);

        for (auto& ogroup : overlap_groups)
        {
            ogroup.compute_basic_properties();
            ogroup.compute_leftmost_rightmost_vloads(
                    _environment, 0 /*max_registers*/);

            const Nodecl::NodeclBase& vload = ogroup._rightmost_group_vload;

            bool is_new_access = true;
            Nodecl::NodeclBase load_scalar_access = 
                Vectorization::Utils::get_scalar_memory_access(vload);

            for (const auto& vaccess : not_nested_vaccesses)
            {
                Nodecl::NodeclBase memory_scalar_access = 
                    Vectorization::Utils::get_scalar_memory_access(vaccess.second);

                if (Nodecl::Utils::structurally_equal_nodecls(
                            load_scalar_access,
                            memory_scalar_access,
                            true /*skip conv */))
                {
                    is_new_access = false;
                    break;
                }
            }

            if (is_new_access)
            {
                not_nested_vaccesses.insert(
                        pair_nodecl_nodecl_t(ogroup._rightmost_code_vload,
                            ogroup._rightmost_group_vload));
            }
        }


        // Add #pragma noprefetch to the loop
        if (!not_nested_vaccesses.empty())
        {
            // We visit vaccesses again to generate pref instructions in the
            // same order. If execution time is a problem, revisit this approach.
        
            GenPrefetch gen_prefetch(n, not_nested_vaccesses, _environment, _pref_info);
            gen_prefetch.walk(stmts);
            objlist_nodecl_t pref_instructions = gen_prefetch.get_prefetch_instructions();

            // Add prefetching instructions at the beginning of the compound
            Nodecl::Utils::prepend_items_in_nested_compound_statement(
                    n, Nodecl::List::make(pref_instructions));

            // Add #pragma noprefetch to loop
            Nodecl::UnknownPragma noprefetch_pragma =
                Nodecl::UnknownPragma::make("noprefetch");

            n.prepend_sibling(noprefetch_pragma);
        }

        walk(stmts);
    }

    GenPrefetch::GenPrefetch(const Nodecl::NodeclBase& loop,
            const map_nodecl_nodecl_t& vaccesses,
            const VectorizerEnvironment& environment,
            const prefetch_info_t& pref_info)
        : _loop(loop), _vaccesses(vaccesses), _environment(environment),
        _pref_info(pref_info),
        _linear_vars(Vectorizer::_vectorizer_analysis->get_linear_nodecls(_loop))
    {
    }

    void GenPrefetch::visit(const Nodecl::ForStatement& n)
    {
        // Do not explore nested ForStatement
    }

    Nodecl::NodeclBase GenPrefetch::get_prefetch_node(
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
                                const_value_to_nodecl(const_value_get_signed_int(
                                        _environment._vec_factor)),
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

    void GenPrefetch::visit(const Nodecl::VectorLoad& n)
    {
        Nodecl::NodeclBase rhs = n.get_rhs();

        walk(rhs);
        walk(n.get_flags());

        const auto& vload_it = _vaccesses.find(n);

        if (vload_it != _vaccesses.end())
        {
            Nodecl::NodeclBase pref_memory_address = vload_it->second.
                as<Nodecl::VectorLoad>().get_rhs();

            if (_pref_info.in_place)
            {
                if (_object_init.is_null())
                {
                    n.prepend_sibling(get_prefetch_node(pref_memory_address, L2_READ, _pref_info.distances[1]));
                    n.prepend_sibling(get_prefetch_node(pref_memory_address, L1_READ, _pref_info.distances[0]));
                }
                else
                {
                    _object_init.prepend_sibling(get_prefetch_node(pref_memory_address, L2_READ, _pref_info.distances[1]));
                    _object_init.prepend_sibling(get_prefetch_node(pref_memory_address, L1_READ, _pref_info.distances[0]));
                }
            }
            else
            {
                _pref_instr.push_back(get_prefetch_node(pref_memory_address, L2_READ, _pref_info.distances[1]));
                _pref_instr.push_back(get_prefetch_node(pref_memory_address, L1_READ, _pref_info.distances[0]));
            }
        }
    }

    void GenPrefetch::visit(const Nodecl::VectorStore& n)
    {
        Nodecl::NodeclBase lhs = n.get_lhs();
        Nodecl::NodeclBase rhs = n.get_rhs();
        
        walk(lhs);
        walk(rhs);
        //walk(n.get_flags());

        const auto& vstore_it = _vaccesses.find(n);

        if (vstore_it != _vaccesses.end())
        {
            Nodecl::NodeclBase pref_memory_address = vstore_it->second.
                as<Nodecl::VectorStore>().get_lhs();

            if (_pref_info.in_place)
            {
                if (_object_init.is_null())
                {
                    n.prepend_sibling(get_prefetch_node(pref_memory_address, L2_WRITE, _pref_info.distances[1]));
                    n.prepend_sibling(get_prefetch_node(pref_memory_address, L1_WRITE, _pref_info.distances[0]));
                }
                else
                {
                    _object_init.prepend_sibling(get_prefetch_node(pref_memory_address, L2_WRITE, _pref_info.distances[1]));
                    _object_init.prepend_sibling(get_prefetch_node(pref_memory_address, L1_WRITE, _pref_info.distances[0]));
                }
            }
            else
            {
                _pref_instr.push_back(get_prefetch_node(pref_memory_address, L2_WRITE, _pref_info.distances[1]));
                _pref_instr.push_back(get_prefetch_node(pref_memory_address, L1_WRITE, _pref_info.distances[0]));

            }
        }
    }

    void GenPrefetch::visit(const Nodecl::ObjectInit& n)
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

    objlist_nodecl_t GenPrefetch::get_prefetch_instructions()
    {
        return _pref_instr;
    }
}
}

