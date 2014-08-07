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
#include "tl-expression-reduction.hpp"
#include "tl-vectorization-analysis-interface.hpp"

#include "tl-nodecl-utils.hpp"
#include "hlt-loop-unroll.hpp"
#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
    Nodecl::List OverlapGroup::get_init_statements(
            const Nodecl::ForStatement& for_stmt,
            const objlist_nodecl_t& ivs_list) const
    {
        TL::Scope scope = for_stmt.retrieve_context();
        Nodecl::List result_list;

        for(objlist_tlsymbol_t::const_iterator it = _group_registers.begin();
                it != _group_registers.end()-1;
                it++)
        {
            // __overlap_X_1 = vload(&a[i]);

            Nodecl::NodeclBase vload_index =
                _group_registers_indexes[0].shallow_copy();

            // Replace IV by LB in vload_index
            for (objlist_nodecl_t::const_iterator iv = ivs_list.begin();
                    iv != ivs_list.end();
                    iv++)
            {
                Nodecl::NodeclBase iv_lb = VectorizationAnalysisInterface::
                    _vectorizer_analysis->get_induction_variable_lower_bound(
                            for_stmt,*iv);

                if (!iv_lb.is_null())
                {
                    Nodecl::Utils::nodecl_replace_nodecl_by_structure(
                            vload_index,
                            *iv,
                            iv_lb);
                }
            }


            Nodecl::VectorAssignment vassignment =
                Nodecl::VectorAssignment::make(
                        it->make_nodecl(true),
                        Nodecl::VectorLoad::make(
                            Nodecl::Reference::make(
                                Nodecl::ArraySubscript::make(
                                    _group_subscripted.shallow_copy(),
                                    Nodecl::List::make(
                                        vload_index),
                                    _basic_type),
                                _basic_type.get_pointer_to()),
                            Utils::get_null_mask(),
                            Nodecl::List::make(Nodecl::AlignedFlag::make()),
                            _vector_type),
                        Utils::get_null_mask(),
                        _vector_type);

            Nodecl::ExpressionStatement exp_stmt =
                Nodecl::ExpressionStatement::make(vassignment);

            result_list.prepend(exp_stmt);
        }

        return result_list;
    }

    Nodecl::List OverlapGroup::get_iteration_update_pre() const
    {
        Nodecl::List result_list;

        const int size = _group_registers.size();

        // __overlap_X_1 = load(a[i + VF]) 
        Nodecl::VectorAssignment vassignment =
            Nodecl::VectorAssignment::make(
                    _group_registers[size-1].make_nodecl(true),
                    Nodecl::VectorLoad::make(
                        Nodecl::Reference::make(
                            Nodecl::ArraySubscript::make(
                                _group_subscripted.shallow_copy(),
                                Nodecl::List::make(
                                    _group_registers_indexes[size-1].shallow_copy()),
                                _basic_type),
                            _basic_type.get_pointer_to()),
                        Utils::get_null_mask(),
                        Nodecl::List::make(Nodecl::AlignedFlag::make()),
                        _vector_type),
                    Utils::get_null_mask(),
                    _vector_type);

        Nodecl::ExpressionStatement exp_stmt =
            Nodecl::ExpressionStatement::make(vassignment);

        result_list.prepend(exp_stmt);

        return result_list;
    }

    Nodecl::List OverlapGroup::get_iteration_update_post() const
    {
        Nodecl::List result_list;

        const int size = _group_registers.size();

        for(int i=0; i < (size-1); i++)
        {
            // __overlap_X_0 = __overlap_X_1;
            Nodecl::ExpressionStatement exp_stmt =
                Nodecl::ExpressionStatement::make(
                        Nodecl::VectorAssignment::make(
                            _group_registers[i].make_nodecl(true),
                            _group_registers[i+1].make_nodecl(true),
                            Utils::get_null_mask(),
                            _group_registers[i].get_type()));

            result_list.prepend(exp_stmt);
        }

        return result_list;
    }

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
            const map_tl_sym_int_t& overlapd_symbols)
    {
        for(map_tl_sym_int_t::const_iterator it = overlapd_symbols.begin();
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
        : _environment(environment) 
    {
    }

    void OverlappedAccessesOptimizer::visit(const Nodecl::ForStatement& n)
    {
        Nodecl::ForStatement main_loop = n;
        Nodecl::ForStatement if_epilog;
        Nodecl::ForStatement last_epilog;

        objlist_nodecl_t ivs_list = VectorizationAnalysisInterface::
            _vectorizer_analysis->get_linear_nodecls(main_loop);

        objlist_blocks_pairs_t main_loop_blocks_pairs;
        main_loop_blocks_pairs.append(
                pair_nodecl_int_t(main_loop.get_statement(), 0));

        objlist_blocks_pairs_t if_epilog_blocks_pairs;

        int min_unroll_factor = get_loop_min_unroll_factor(
                main_loop, ivs_list);

        std::cerr << "MIN UNROLL FACTOR: " << min_unroll_factor << std::endl;
        // UNROLL
        if (min_unroll_factor > 0)
        {
            if_epilog = main_loop.shallow_copy()
                .as<Nodecl::ForStatement>();

            // Main Loop
            TL::HLT::LoopUnroll loop_unroller;
            loop_unroller.set_loop(main_loop)
                .set_unroll_factor(16)
                .unroll();

            Nodecl::NodeclBase whole_main_transformation =
                loop_unroller.get_whole_transformation();

            main_loop = loop_unroller.get_unrolled_loop()
                .as<Nodecl::ForStatement>();

            last_epilog = loop_unroller.get_epilog_loop()
                .as<Nodecl::ForStatement>();

            VectorizationAnalysisInterface::
                _vectorizer_analysis->register_copy(
                        n, main_loop);
                
            // If Epilog
            if (min_unroll_factor > 1 )
            {
                loop_unroller.set_unroll_factor(min_unroll_factor).
                    set_create_epilog(false).unroll();

                if_epilog = loop_unroller.get_unrolled_loop()
                    .as<Nodecl::ForStatement>();
            }

            VectorizationAnalysisInterface::
                _vectorizer_analysis->register_copy(
                        n, if_epilog);
 
            if_epilog_blocks_pairs = 
                apply_overlap_blocked_unrolling(if_epilog,
                        min_unroll_factor);

            last_epilog.prepend_sibling(if_epilog);
            n.replace(whole_main_transformation);
        }

        // TODO:
        TL::Scope scope = main_loop.get_parent().get_parent().get_parent().
            retrieve_context();

        // OVERLAP
        for(map_tl_sym_int_t::const_iterator it = 
                _environment._overlap_symbols_map.begin();
                it != _environment._overlap_symbols_map.end();
                it++)
        {
            TL::Symbol sym = it->first;
            int min_group_length = it->second;

            // MAIN LOOP
            objlist_nodecl_t main_loop_vector_loads =
                get_adjacent_vector_loads_not_nested_in_for(
                        main_loop.get_statement(), sym);

            if (!main_loop_vector_loads.empty())
            {
                objlist_ogroup_t overlap_groups = 
                    get_overlap_groups(
                            main_loop_vector_loads,
                            min_group_length,
                            main_loop_blocks_pairs,
                            ivs_list);

                objlist_nodecl_t if_epilog_vector_loads;
                for(objlist_ogroup_t::iterator ogroup =
                        overlap_groups.begin();
                        ogroup != overlap_groups.end();
                        ogroup++)
                {
                    // MAIN LOOP
                    compute_group_properties(*ogroup, scope,
                            /* epilog */ false);
                    insert_group_update_stmts(*ogroup, main_loop,
                            ivs_list, /* group epilog */ false);
                    replace_overlapped_loads(*ogroup);
                }
            }

            // IF EPILOG
            if (!if_epilog.is_null())
            {
                objlist_nodecl_t if_epilog_vector_loads =
                    get_adjacent_vector_loads_not_nested_in_for(
                            if_epilog.get_statement(), sym);

                if (!if_epilog_vector_loads.empty())
                {
                    objlist_ogroup_t if_epilog_overlap_groups = 
                        get_overlap_groups(
                                if_epilog_vector_loads,
                                min_group_length,
                                if_epilog_blocks_pairs,
                                ivs_list);

                    for(objlist_ogroup_t::iterator ogroup =
                            if_epilog_overlap_groups.begin();
                            ogroup != if_epilog_overlap_groups.end();
                            ogroup++)
                    {
                        compute_group_properties(*ogroup, scope,
                                /* group epilog */ true);
                        insert_group_update_stmts(*ogroup,
                                if_epilog, ivs_list,
                                /* group epilog */ true);
                        replace_overlapped_loads(*ogroup);
                    }
                }
            }
        }

        // Transform if epilogue loop into IfStatement
        if (!if_epilog.is_null())
        {
            Nodecl::NodeclBase cond = 
                TL::LoopControlAdapter(
                        if_epilog.get_loop_header()).get_cond();

            Nodecl::IfElseStatement if_stmt =
                Nodecl::IfElseStatement::make(
                        cond.shallow_copy(),
                        if_epilog.get_statement(),
                        Nodecl::NodeclBase::null());

            if_epilog.replace(if_stmt);
        }
 
        walk(main_loop.get_statement());
        walk(if_epilog);

        // Add #pragma nounroll to main_loop and epilog
        if (min_unroll_factor > 1)
        {
            Nodecl::UnknownPragma unroll_pragma =
                Nodecl::UnknownPragma::make("nounroll");

            main_loop.prepend_sibling(unroll_pragma.shallow_copy());
            last_epilog.prepend_sibling(unroll_pragma.shallow_copy());
        }
    }

    unsigned int OverlappedAccessesOptimizer::get_loop_min_unroll_factor(
            Nodecl::ForStatement n,
            const objlist_nodecl_t& ivs_list)
    {
        // We do not unroll the SIMD loop
        if (_environment._analysis_simd_scope
                == n)
            return 0;

        Nodecl::NodeclBase iv = ivs_list.front();
        
        if (Nodecl::Utils::structurally_equal_nodecls(iv,
                    VectorizationAnalysisInterface::
                    _vectorizer_analysis->get_induction_variable_lower_bound(
                        n, iv), true))
                return 0;

        unsigned int unroll_factor = 0;

        for(map_tl_sym_int_t::const_iterator it = 
                _environment._overlap_symbols_map.begin();
                it != _environment._overlap_symbols_map.end();
                it++)
        {
            TL::Symbol sym = it->first;
            unsigned int min_group_length = it->second;

            objlist_nodecl_t vector_loads =
                get_adjacent_vector_loads_not_nested_in_for(
                        n.get_statement(), sym);

            unsigned int vector_loads_size = vector_loads.size();
            if (vector_loads_size > 0 &&
                    vector_loads_size <= min_group_length &&
                    (unroll_factor * vector_loads_size)
                    < min_group_length)
            {
                unroll_factor = min_group_length /
                    vector_loads_size; 
            }
        }

        return unroll_factor;
    }

    objlist_blocks_pairs_t OverlappedAccessesOptimizer::
        apply_overlap_blocked_unrolling(
            const Nodecl::ForStatement& n,
            const unsigned int block_size)
    {
        objlist_blocks_pairs_t blocks_pairs;

        Nodecl::NodeclBase loop_header =
            n.get_loop_header();

        TL::LoopControlAdapter lc = TL::LoopControlAdapter(
                loop_header);

        Nodecl::NodeclBase cond_node = lc.get_cond();
        Nodecl::NodeclBase next_node = lc.get_next();

        Nodecl::ExpressionStatement next_update_stmt =
            Nodecl::ExpressionStatement::make(
                    next_node.shallow_copy());

        Nodecl::List outer_stmt = 
            n.get_statement().as<Nodecl::List>();
        // Add next update
        Nodecl::Utils::append_items_in_outermost_compound_statement(
                outer_stmt, next_update_stmt);

        blocks_pairs.append(pair_nodecl_int_t(outer_stmt, 0));

        for (unsigned int i=1; i<(16/block_size)-1; i++)
        {
            Nodecl::List stmts_copy =
                outer_stmt.shallow_copy().as<Nodecl::List>();

            Nodecl::IfElseStatement if_else_stmt =
                Nodecl::IfElseStatement::make(
                        cond_node.shallow_copy(),
                        stmts_copy,
                        Nodecl::NodeclBase::null());

            // Add IfStatement
            Nodecl::Utils::append_items_in_outermost_compound_statement(
                    outer_stmt, if_else_stmt);

            blocks_pairs.append(pair_nodecl_int_t(stmts_copy, i*block_size));

            outer_stmt = if_else_stmt.get_then().as<Nodecl::List>();
        }

        return blocks_pairs;
    }

    objlist_nodecl_t OverlappedAccessesOptimizer::
        get_adjacent_vector_loads_not_nested_in_for(
                const Nodecl::NodeclBase& n,
                const TL::Symbol& sym)
    {
        objlist_nodecl_t result;

        objlist_nodecl_t vector_loads = Nodecl::Utils::
            nodecl_get_all_nodecls_of_kind<Nodecl::VectorLoad>(n);

        objlist_nodecl_t nested_for_stmts = Nodecl::Utils::
            nodecl_get_all_nodecls_of_kind<Nodecl::ForStatement>(n);

        std::cerr << "Adjacent Vector Loads:" << std::endl;

        for(objlist_nodecl_t::iterator vload = vector_loads.begin();
                vload != vector_loads.end();
                vload++)
        {
            bool vload_is_nested_in_nested_for = false;

            for(objlist_nodecl_t::iterator nested_for = nested_for_stmts.begin();
                    nested_for != nested_for_stmts.end();
                    nested_for++)
            {
                if (Nodecl::Utils::nodecl_contains_nodecl_by_pointer(
                            *nested_for, *vload))
                {
                    vload_is_nested_in_nested_for = true;
                    break;
                }
            }

            if (!vload_is_nested_in_nested_for)
            {
                Nodecl::NodeclBase vl_rhs= vload->as<Nodecl::VectorLoad>().
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
                        result.append(*vload);
                    }
                }
            }
        }

        return result;
    }

    Nodecl::NodeclBase OverlappedAccessesOptimizer::
        get_vector_load_subscripted(
            const Nodecl::VectorLoad& vl)
    {
        Nodecl::NodeclBase vl_rhs = vl.get_rhs();

        if (vl_rhs.is<Nodecl::Reference>())
            vl_rhs = vl_rhs.as<Nodecl::Reference>().get_rhs();

        if (vl_rhs.is<Nodecl::ArraySubscript>())
        {
            Nodecl::ArraySubscript array =
                vl_rhs.as<Nodecl::ArraySubscript>();

            return array.get_subscripted().no_conv();
        }

        internal_error("Invalid Vector Load\n", 0);
    }

    Nodecl::NodeclBase OverlappedAccessesOptimizer::
        get_vector_load_subscript(
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
                front().no_conv();
        }

        internal_error("Invalid Vector Load\n", 0);
    }

    bool OverlappedAccessesOptimizer::overlap(
            const Nodecl::VectorLoad& vector_load,
            objlist_nodecl_t group)
    {
        Nodecl::NodeclBase vl_subscripts =
            get_vector_load_subscript(vector_load);

        for(objlist_nodecl_t::iterator it =
                group.begin();
                it != group.end();
                it++)
        {
            Nodecl::NodeclBase it_group_subscripts =
                get_vector_load_subscript(
                        it->as<Nodecl::VectorLoad>());

            Nodecl::Minus minus = Nodecl::Minus::make(
                    vl_subscripts.no_conv().shallow_copy(),
                    it_group_subscripts.no_conv().shallow_copy(),
                    vl_subscripts.get_type());


            TL::Optimizations::UnitaryReductor unitary_reductor;
            unitary_reductor.reduce(minus);

            std::cerr << "Difference: " << vl_subscripts.prettyprint()
                << " MINUS " << it_group_subscripts.prettyprint()
                << " = "
                << minus.prettyprint()
                << std::endl;

            // TODO: distance
            // if result is constant and 1, for example, return true
            if (minus.is_constant())
            {
                int length = const_value_cast_to_signed_int(
                        minus.get_constant());
                    
                if (length < 0) 
                    length = -length;

                if (length <= 1)
                    return true;
            }
        }

        return false;
    }

    objlist_ogroup_t OverlappedAccessesOptimizer::
        get_overlap_groups(const objlist_nodecl_t& vector_loads,
                const unsigned int min_group_length,
                const objlist_blocks_pairs_t& blocks_pairs,
                const objlist_nodecl_t& ivs_list)
    {
        objlist_ogroup_t result;
        OverlapGroup group;
        objlist_nodecl_t::const_iterator target_load =
            vector_loads.begin();

        //TODO: >1 groups
        std::cerr << "Overlap Group ("
            << min_group_length << ")" 
            << std::endl;
        std::cerr << target_load->prettyprint() << std::endl;

        group._group_loads.append(*target_load);

        target_load++;

        while(target_load != vector_loads.end())
        {
            Nodecl::VectorLoad target_load_copy =
                target_load->shallow_copy().as<Nodecl::VectorLoad>();

            // Apply unrolling blocks offset in index
            // Reverse iterator. Blocks are inclusive
            for(objlist_blocks_pairs_t::const_reverse_iterator block_it =
                    blocks_pairs.rbegin();
                    block_it != blocks_pairs.rend();
                    block_it++)
            {
                if (Nodecl::Utils::nodecl_contains_nodecl_by_pointer(
                            block_it->first, *target_load))
                {
//                    std::cerr << "BLOCK: " << block_it->first.prettyprint() << std::endl;

                    int block_offset = block_it->second;
                
                    if (block_offset != 0)
                    {
                        // Replace IV by IV + block offset
                        for (objlist_nodecl_t::const_iterator iv =
                                ivs_list.begin();
                                iv != ivs_list.end();
                                iv++)
                        {
//                            std::cerr << "IV: " << iv->prettyprint() << std::endl;

                            Nodecl::Add iv_plus_boffset =
                                Nodecl::Add::make(
                                        iv->shallow_copy(),
                                        const_value_to_nodecl(
                                            const_value_get_signed_int(block_offset)),
                                        TL::Type::get_int_type());

                            Nodecl::Utils::nodecl_replace_nodecl_by_structure(
                                    target_load_copy,
                                    *iv,
                                    iv_plus_boffset);
                        }
                    }

                    // Blocks are inclusive
                    break;
                }
            }

            if(overlap(target_load_copy,
                        group._group_loads))
            {
                std::cerr << target_load_copy.prettyprint() << " overlap!"<< std::endl;
                target_load->replace(target_load_copy);
                group._group_loads.append(*target_load);
            }
            else
            {
                std::cerr << target_load_copy.prettyprint() << " DOESN'T overlap!"<< std::endl;
                //Nodecl::Utils::print_ast(target_load_copy);
                internal_error("not yet implemented", 0);
            }
                
            target_load++;
        }

        // TODO: Length
        if (group._group_loads.size() >= min_group_length)
            result.append(group);

        return result;
    }


    void OverlappedAccessesOptimizer::compute_group_properties(
            OverlapGroup& ogroup,
            TL::Scope& scope,
            const bool is_group_epilog)
    {
        ogroup._vector_type = ogroup._group_loads.front().get_type().no_ref();
        ogroup._basic_type = ogroup._group_loads.front().get_type().no_ref().basic_type();
        int vectorization_factor = ogroup._vector_type.vector_num_elements();

        Nodecl::VectorLoad vload = 
            ogroup._group_loads.front().as<Nodecl::VectorLoad>();

        //TODO
        Nodecl::NodeclBase first_index =
            get_vector_load_subscript(vload);

        // TODO # registers
        // Declare group registers
        for (int i=0; i<2; i++)
        {
            std::stringstream new_sym_name;
            new_sym_name << "__overlap_" << i;//it->first.get_name() << "_" << i;

            if (is_group_epilog)
            {
                // Use previous symbols
                TL::Symbol sym = 
                    scope.get_symbol_from_name(new_sym_name.str());

                ERROR_CONDITION(!sym.is_valid(), "cache symbol is invalid.", 0);

                ogroup._group_registers.push_back(sym);
                ogroup._group_registers_indexes.push_back(
                        (i == 0) ? first_index : 
                        Nodecl::Add::make(
                            first_index.shallow_copy(),
                            const_value_to_nodecl(const_value_get_signed_int(
                                    vectorization_factor)),
                            first_index.get_type()));
            }
            else
            {
                // Create new symbols
                TL::Symbol new_sym = scope.new_symbol(new_sym_name.str());
                new_sym.get_internal_symbol()->kind = SK_VARIABLE;
                new_sym.get_internal_symbol()->entity_specs.is_user_declared = 1;
                new_sym.set_type(ogroup._vector_type);

                ogroup._group_registers.push_back(new_sym);
                ogroup._group_registers_indexes.push_back(
                        (i == 0) ? first_index : 
                        Nodecl::Add::make(
                            first_index.shallow_copy(),
                            const_value_to_nodecl(const_value_get_signed_int(
                                    vectorization_factor)),
                            first_index.get_type()));
            }
        }

        // Group subscript
        ogroup._group_subscripted = get_vector_load_subscripted(
                vload).as<Nodecl::Symbol>();
    }


    void OverlappedAccessesOptimizer::insert_group_update_stmts(
            OverlapGroup& ogroup,
            const Nodecl::ForStatement& n,
            const objlist_nodecl_t& ivs_list,
            const bool is_group_epilog)
    {
        // Init Statements
        if (!is_group_epilog)
        {
            Nodecl::List init_stmts = 
                ogroup.get_init_statements(n, ivs_list);
            n.prepend_sibling(init_stmts);

            // Update Post
            Nodecl::List post_stmts = 
                ogroup.get_iteration_update_post();
            Nodecl::Utils::append_items_in_outermost_compound_statement(
                    n.get_statement(), post_stmts);
        }

        // Update Pre
        Nodecl::List pre_stmts = 
            ogroup.get_iteration_update_pre();
        Nodecl::Utils::prepend_items_in_outermost_compound_statement(
                n.get_statement(), pre_stmts);
    }

    void OverlappedAccessesOptimizer::replace_overlapped_loads(
            const OverlapGroup& ogroup)
    {
        //TODO: init_cache_index
        Nodecl::NodeclBase init_cache_index =
            get_vector_load_subscript(
                    ogroup._group_loads.front().
                    as<Nodecl::VectorLoad>());

        for(objlist_nodecl_t::const_iterator load_it =
                ogroup._group_loads.begin();
                load_it != ogroup._group_loads.end();
                load_it++)
        {
            Nodecl::NodeclBase load_subscript =
                get_vector_load_subscript(
                        load_it->as<Nodecl::VectorLoad>());

            Nodecl::Minus shifted_elements = Nodecl::Minus::make(
                    load_subscript.shallow_copy(),
                    ogroup._group_registers_indexes[0].no_conv().shallow_copy(),
                    load_it->get_type());

            TL::Optimizations::UnitaryReductor unitary_reductor;
            unitary_reductor.reduce(shifted_elements);

            std::cerr << "Align elements: " << load_subscript.prettyprint()
                << " MINUS " << ogroup._group_registers_indexes[0].prettyprint()
                << " = "
                << shifted_elements.prettyprint()
                << std::endl;

            objlist_tlsymbol_t::const_iterator register_it =
                ogroup._group_registers.begin();

            if (shifted_elements.is_constant())
            {
                if (const_value_is_zero(
                            shifted_elements.get_constant()))
                {
                    load_it->replace(
                            register_it->make_nodecl(true));
                }
                else
                {
                    load_it->replace(Nodecl::VectorAlignRight::make(
                                register_it->make_nodecl(true),
                                (register_it++)->make_nodecl(true),
                                shifted_elements,
                                load_it->as<Nodecl::VectorLoad>().
                                get_mask().shallow_copy(),
                                register_it->get_type()));
                }
            }
            else
            {
                std::cerr << "NOT CONSTANT: "
                    << shifted_elements.prettyprint()
                    << std::endl;
            }
        }
    }
}
}

