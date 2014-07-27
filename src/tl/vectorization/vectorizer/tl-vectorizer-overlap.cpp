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

#include "tl-vectorizer-overlap.hpp"

#include "tl-vectorizer-visitor-expression.hpp"
#include "tl-vectorization-utils.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-expression-reduction.hpp"
#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
#if 0    
    CacheInfo::CacheInfo(
            const Nodecl::NodeclBase &lower_bound,
            const Nodecl::NodeclBase& upper_bound,
            const Nodecl::NodeclBase& stride,
            const int overlap_factor)
        : _lower_bound(lower_bound), _upper_bound(upper_bound),
        _stride(stride), _overlap_factor(overlap_factor)
    {
    }

    VectorizerCache::VectorizerCache(
            const tl_sym_int_map_t& overlapd_symbols)
    {
        for(tl_sym_int_map_t::const_iterator it = overlapd_symbols.begin();
                it != overlapd_symbols.end();
                it++)
        {
//            ERROR_CONDITION(!it->is<Nodecl::ArraySubscript>(),
//                    "VECTORIZER: overlap clause does not contain an ArraySubscript", 0);

//            Nodecl::ArraySubscript arr_it = it->as<Nodecl::ArraySubscript>();
//            Nodecl::NodeclBase list_front = arr_it.get_subscripts().as<Nodecl::List>().front();

//            ERROR_CONDITION(!list_front.is<Nodecl::Range>(),
//                    "VECTORIZER: Range not found in overlap clause", 0);

//            Nodecl::Range range_it = list_front.as<Nodecl::Range>();


//            TL::Symbol key = arr_it.get_subscripted().as<Nodecl::Symbol>().get_symbol();

//            Nodecl::NodeclBase lower_bound = range_it.get_lower();
//            Nodecl::NodeclBase upper_bound = range_it.get_upper();
//            Nodecl::NodeclBase stride = range_it.get_stride();

            Nodecl::NodeclBase lower_bound = const_value_to_nodecl(
                    const_value_get_zero(4, 1));
            Nodecl::NodeclBase upper_bound = const_value_to_nodecl(
                    const_value_get_zero(4, 1));
            Nodecl::NodeclBase stride = const_value_to_nodecl(
                    const_value_get_zero(4, 1));

            _overlap_map.insert(overlap_pair_t(it->first,
                        CacheInfo(lower_bound, upper_bound, stride, it->second)));
        }
    }

    void VectorizerCache::declare_overlap_symbols(TL::Scope scope,
            const VectorizerEnvironment& environment)
    {
        // Map of overlaps
        for(overlap_map_t::iterator it = _overlap_map.begin();
                it != _overlap_map.end();
                it++)
        {
            // TODO # registers
            // Add registers to each overlap
            for (int i=0; i<2; i++)
            {
                std::stringstream new_sym_name;
                new_sym_name << "__overlap_" << it->first.get_name() << "_" << i;

                TL::Symbol new_sym = scope.new_symbol(new_sym_name.str());
                new_sym.get_internal_symbol()->kind = SK_VARIABLE;
                new_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
                new_sym.set_type(it->first.get_type().basic_type().
                        get_vector_of_elements(environment._vectorization_factor));

                std::cerr << it->first.get_name() << ": " << it->first.get_type().get_simple_declaration(scope, " ")
                    << it->first.get_type().basic_type().get_simple_declaration(scope, " ")
                    << it->first.get_type().basic_type().get_vector_of_elements(environment._vectorization_factor).get_simple_declaration(scope, " ")
                    << std::endl;

                it->second._register_list.push_back(new_sym);
            }
        }
    }

    Nodecl::List VectorizerCache::get_init_statements(VectorizerEnvironment& environment) const
    {
        Nodecl::List result_list;

        for(overlap_map_t::const_iterator it = _overlap_map.begin();
                it != _overlap_map.end();
                it++)
        {
            const std::vector<TL::Symbol>& register_list = it->second._register_list;
            const int size = register_list.size();

            //TODO: different strategies
            for(int i=0; i < size-1; i++)
            {
                // __overlap_X_1 = a[i];

                TL::Source src;

                src << as_expression(register_list[i].make_nodecl(/* type = */ true))
                    << " = "
                    << as_expression(it->first.make_nodecl(/* type = */true))
                    << "["
                    << as_expression(it->second._lower_bound.shallow_copy())
                    << "]"
                    ;

                Nodecl::NodeclBase assignment =
                    src.parse_expression(TL::Scope(CURRENT_COMPILED_FILE->global_decl_context));

#if 0
                Assignment::make(
                        register_list[i].make_nodecl(/* type = */ true),
                        ArraySubscript::make(
                            it->first.make_nodecl(/* type = */true),
                            Nodecl::List::make( //TODO: + VL*(i-1)
                                it->second._lower_bound.shallow_copy()),
                            it->first.get_type().basic_type()),
                        register_list[i].get_type().basic_type());
#endif
                //TODO
                //                    VectorizerVisitorExpression expr_vectorizer(environment, /* overlapd disabled */ false);
                //                    expr_vectorizer.walk(assignment.as<Nodecl::Assignment>().get_rhs());

                Nodecl::ExpressionStatement exp_stmt =
                    Nodecl::ExpressionStatement::make(assignment);

                result_list.prepend(exp_stmt);
            }
        }

        return result_list;
    }

    Nodecl::List VectorizerCache::get_iteration_update_pre(VectorizerEnvironment& environment) const
    {
        Nodecl::List result_list;

        for(overlap_map_t::const_iterator it = _overlap_map.begin();
                it != _overlap_map.end();
                it++)
        {
            const std::vector<TL::Symbol>& register_list = it->second._register_list;
            const int size = register_list.size();

            // __overlap_X_1 = load(a[i + VL]) // TODO: VL*;
            Nodecl::Assignment assignment =
                Nodecl::Assignment::make(
                        register_list[size-1].make_nodecl(/* type = */ true),
                        Nodecl::ArraySubscript::make(
                            it->first.make_nodecl(/* type = */true),
                            Nodecl::List::make(
                                Nodecl::Add::make(
                                    it->second._lower_bound.shallow_copy(),
                                    const_value_to_nodecl(const_value_get_signed_int(environment._vectorization_factor)),
                                    TL::Type::get_int_type())),
                            it->first.get_type().basic_type()),
                        register_list[size-1].get_type().basic_type());

            //                VectorizerVisitorExpression stmt_vectorizer(environment, /* overlap disabled */ false);
            //                stmt_vectorizer.walk(assignment.get_rhs());

            Nodecl::ExpressionStatement exp_stmt =
                Nodecl::ExpressionStatement::make(assignment);

            result_list.prepend(exp_stmt);
        }

        return result_list;
    }

    Nodecl::List VectorizerCache::get_iteration_update_post(VectorizerEnvironment& environment) const
    {
        Nodecl::List result_list;

        for(overlap_map_t::const_iterator it = _overlap_map.begin();
                it != _overlap_map.end();
                it++)
        {
            const std::vector<TL::Symbol>& register_list = it->second._register_list;
            const int size = register_list.size();

            //TODO: different strategies
            int i;
            for(i=0; i < (size-1); i++)
            {
                // __overlap_X_0 = __overlap_X_1;
                Nodecl::ExpressionStatement exp_stmt =
                    Nodecl::ExpressionStatement::make(
                            Nodecl::Assignment::make(
                                register_list[i].make_nodecl(/* type = */ true),
                                register_list[i+1].make_nodecl(/* type = */ true),
                                register_list[i].get_type().basic_type()));

                result_list.prepend(exp_stmt);
            }
        }

        return result_list;
    }

    bool VectorizerCache::is_overlapd_access(const Nodecl::ArraySubscript& n) const
    {
        Nodecl::NodeclBase subscripted = Nodecl::Utils::advance_conversions(n.get_subscripted());

        if(subscripted.is<Nodecl::Symbol>())
        {
            TL::Symbol sym = subscripted.as<Nodecl::Symbol>().get_symbol();
            if (_overlap_map.find(sym) != _overlap_map.end())
            {
                return true;
            }
        }

        return false;
    }

    Nodecl::NodeclBase VectorizerCache::get_load_access(const Nodecl::ArraySubscript& n) const
    {
        Nodecl::NodeclBase subscripted = Nodecl::Utils::advance_conversions(n.get_subscripted());

        if(subscripted.is<Nodecl::Symbol>())
        {
            TL::Symbol sym = subscripted.as<Nodecl::Symbol>().get_symbol();

            // Find overlap
            overlap_map_t::const_iterator overlap_it = _overlap_map.find(sym);
            if (overlap_it != _overlap_map.end())
            {
                const CacheInfo& overlap = overlap_it->second;

                // Compare subscripts
                Nodecl::NodeclBase subscript = n.get_subscripts().as<Nodecl::List>().front();

                if (Nodecl::Utils::structurally_equal_nodecls(overlap._lower_bound, subscript,
                            /* skip conversions */ true))
                {
                    return overlap._register_list[0].make_nodecl(true);
                }
                else
                {
                    return Nodecl::VectorAlignRight::make(
                            overlap._register_list[1].make_nodecl(true),
                            overlap._register_list[0].make_nodecl(true),
                            const_value_to_nodecl(const_value_get_one(4, 1)),
                            Vectorization::Utils::get_null_mask(),
                            overlap._register_list[0].get_type());
                }
            }
        }

        return n;
    }
*/

#endif



    OverlappedAccessesOptimizer::OverlappedAccessesOptimizer(
            VectorizerEnvironment& environment)
        : _overlap_symbols(environment._overlap_symbols_map)
    {
    }


    void OverlappedAccessesOptimizer::visit(const Nodecl::ForStatement& n)
    {
        for(tl_sym_int_map_t::iterator it = _overlap_symbols.begin();
                it != _overlap_symbols.end();
                it++)
        {
            TL::Symbol sym = it->first;

            objlist_nodecl_t vector_loads =
                get_adjacent_vector_loads_nested_in_one_for(n, sym);

            objlist_ogroup_t overlap_groups = 
                get_overlap_groups(vector_loads);
/*
            for(std::list<OverlapGroup>::iterator ogroup =
                    sym_overlap_groups.begin();
                    ogroup != sym_overlap_groups.end();
                    ogroup++)
            {
                enable_overlap_cache(*ogroup, n);
                replace_overlapped_loads(*ogroup, n);
            }
*/
        }
    }


    objlist_nodecl_t OverlappedAccessesOptimizer::
        get_adjacent_vector_loads_nested_in_one_for(
                const Nodecl::ForStatement& n,
                const TL::Symbol& sym)
    {
        objlist_nodecl_t result;

        objlist_nodecl_t vector_loads = Nodecl::Utils::
            nodecl_get_all_nodecls_of_kind<Nodecl::VectorLoad>(n);

        std::cerr << "Adjacent Vector Loads:" << std::endl;

        for(objlist_nodecl_t::iterator it = vector_loads.begin();
                it != vector_loads.end();
                it++)
        {
            Nodecl::NodeclBase vl_rhs= it->as<Nodecl::VectorLoad>().
                get_rhs();

            if (vl_rhs.is<Nodecl::Reference>())
                vl_rhs = vl_rhs.as<Nodecl::Reference>().get_rhs();

            if (vl_rhs.is<Nodecl::ArraySubscript>())
            {
                Nodecl::ArraySubscript array =
                    vl_rhs.as<Nodecl::ArraySubscript>();

                Nodecl::NodeclBase subscripted = 
                    array.get_subscripted().no_conv();

                if (subscripted.is<Nodecl::Symbol>() &&
                        (subscripted.get_symbol() == sym))
                {
                    result.append(*it);
                    
                    std::cerr << array.prettyprint() << std::endl;
                }
            }
        }

        return result;
    }

    Nodecl::NodeclBase OverlappedAccessesOptimizer::
        get_vector_load_subscripts(
            const Nodecl::VectorLoad& vl)
    {
        Nodecl::NodeclBase vl_rhs= vl.get_rhs();

        if (vl_rhs.is<Nodecl::Reference>())
            vl_rhs = vl_rhs.as<Nodecl::Reference>().get_rhs();

        if (vl_rhs.is<Nodecl::ArraySubscript>())
        {
            Nodecl::ArraySubscript array =
                vl_rhs.as<Nodecl::ArraySubscript>();

            return array.get_subscripts().as<Nodecl::List>().
                front();
        }

        internal_error("Invalid Vector Load\n", 0);
    }

    bool OverlappedAccessesOptimizer::overlap(
            const Nodecl::VectorLoad& vector_load,
            objlist_nodecl_t group)
    {
        Nodecl::NodeclBase vl_subscripts =
            get_vector_load_subscripts(vector_load);

        for(objlist_nodecl_t::iterator it =
                group.begin();
                it != group.end();
                it++)
        {
            Nodecl::NodeclBase it_group_subscripts =
                get_vector_load_subscripts(
                        it->as<Nodecl::VectorLoad>());

            Nodecl::Minus minus = Nodecl::Minus::make(
                    vl_subscripts.shallow_copy(),
                    it_group_subscripts.shallow_copy(),
                    vl_subscripts.get_type());

            TL::Optimizations::ReduceExpressionVisitor reduce_expr_visitor;
            reduce_expr_visitor.walk(minus);

            std::cerr << "Difference: " << vl_subscripts.prettyprint()
                << " MINUS " << it_group_subscripts.prettyprint()
                << " = "
                << minus.prettyprint()
                << std::endl;

            // if result is constant and 1, for example, return true
        }

        return true;
    }

    objlist_ogroup_t OverlappedAccessesOptimizer::
        get_overlap_groups(const objlist_nodecl_t& vector_loads)
    {
        objlist_ogroup_t result;
        objlist_nodecl_t group;
        objlist_nodecl_t::const_iterator target_load =
            vector_loads.begin();

        std::cerr << "Overlap Group:" << std::endl;
        std::cerr << target_load->prettyprint() << std::endl;

        group.append(*target_load);

        target_load++;

        while(target_load != vector_loads.end())
        {
            if(overlap(target_load->as<Nodecl::VectorLoad>(),
                        group))
            {
                std::cerr << target_load->prettyprint() << std::endl;
                group.append(*target_load);
            }
                
            target_load++;
        }

        return result;
    }

    void OverlappedAccessesOptimizer::enable_overlap_cache(
            const OverlapGroup& ogroup,
            const Nodecl::ForStatement& n)
    {
    }

    void OverlappedAccessesOptimizer::replace_overlapped_loads(
            const OverlapGroup& ogroup,
            const Nodecl::ForStatement& n)
    {
    }
}
}

