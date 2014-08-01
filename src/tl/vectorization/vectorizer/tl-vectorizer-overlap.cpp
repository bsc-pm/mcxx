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
        : _overlap_symbols(environment._overlap_symbols_map)
    {
    }


    void OverlappedAccessesOptimizer::visit(const Nodecl::ForStatement& n)
    {
        std::cerr << std::endl << std::endl << std::endl
            << "NEW LOOP" << std::endl;

        objlist_nodecl_t ivs_list = VectorizationAnalysisInterface::
            _vectorizer_analysis->get_ivs_nodecls(n);

        objlist_blocks_pairs blocks_pairs;

        if(loop_needs_unrolling(n, ivs_list))
            blocks_pairs = apply_overlap_blocked_unrolling(n);
        else
            blocks_pairs.append(
                    pair_nodecl_int_t(n.get_statement(), 0));

        for(map_tl_sym_int_t::iterator it = _overlap_symbols.begin();
                it != _overlap_symbols.end();
                it++)
        {
            TL::Symbol sym = it->first;

            objlist_nodecl_t vector_loads =
                get_adjacent_vector_loads_nested_in_one_for(n, sym);

            if (!vector_loads.empty())
            {
                objlist_ogroup_t overlap_groups = 
                    get_overlap_groups(vector_loads,
                            blocks_pairs,
                            ivs_list);

                for(objlist_ogroup_t::iterator ogroup =
                        overlap_groups.begin();
                        ogroup != overlap_groups.end();
                        ogroup++)
                {
                    compute_group_properties(*ogroup, n);
                    enable_overlap_cache(*ogroup, n,
                            ivs_list);
                    replace_overlapped_loads(*ogroup);
                }
            }
        }

        walk(n.get_statement());
    }

    bool OverlappedAccessesOptimizer::loop_needs_unrolling(
            Nodecl::ForStatement n,
            const objlist_nodecl_t& ivs_list)
    {
        //TODO
        Nodecl::NodeclBase iv = ivs_list.front();
        // Epilog!
        
        if (Nodecl::Utils::structurally_equal_nodecls(iv,
                    VectorizationAnalysisInterface::
                    _vectorizer_analysis->get_induction_variable_lower_bound(
                        n, iv), true))
                return false;
        //

        for(map_tl_sym_int_t::iterator it = _overlap_symbols.begin();
                it != _overlap_symbols.end();
                it++)
        {
            TL::Symbol sym = it->first;

            objlist_nodecl_t vector_loads =
                get_adjacent_vector_loads_nested_in_one_for(n, sym);

            //TODO
            if (!vector_loads.empty())
                return true;
        }

        return false;
    }

    objlist_blocks_pairs OverlappedAccessesOptimizer::
        apply_overlap_blocked_unrolling(
            const Nodecl::ForStatement& n)
    {
        //TODO Unroll Loop

        objlist_blocks_pairs blocks_pairs;

        Nodecl::LoopControl loop_control =
            n.get_loop_header().as<Nodecl::LoopControl>();

        Nodecl::NodeclBase cond_node =
            loop_control.get_cond();

        Nodecl::ExpressionStatement next_update_stmt =
            Nodecl::ExpressionStatement::make(
                    loop_control.get_next());

        Nodecl::List outer_stmt = 
            n.get_statement().as<Nodecl::List>();

        blocks_pairs.append(pair_nodecl_int_t(outer_stmt, 0));

        for (int i=1; i<16/4; i++)
        {
            Nodecl::List stmts_copy =
                outer_stmt.shallow_copy().as<Nodecl::List>();

            Nodecl::IfElseStatement if_else_stmt =
                Nodecl::IfElseStatement::make(
                        cond_node.shallow_copy(),
                        stmts_copy,
                        Nodecl::NodeclBase::null());

            // Add next update
            Nodecl::Utils::append_items_in_outermost_compound_statement(
                    outer_stmt, next_update_stmt.shallow_copy());
            // Add IfStatement
            Nodecl::Utils::append_items_in_outermost_compound_statement(
                    outer_stmt, if_else_stmt);

            blocks_pairs.append(pair_nodecl_int_t(stmts_copy, i*4));

            outer_stmt = if_else_stmt.get_then().as<Nodecl::List>();
        }

        return blocks_pairs;
    }

    objlist_nodecl_t OverlappedAccessesOptimizer::
        get_adjacent_vector_loads_nested_in_one_for(
                const Nodecl::ForStatement& n,
                const TL::Symbol& sym)
    {
        objlist_nodecl_t result;

        objlist_nodecl_t vector_loads = Nodecl::Utils::
            nodecl_get_all_nodecls_of_kind<Nodecl::VectorLoad>(n);

        objlist_nodecl_t nested_for_stmts = Nodecl::Utils::
            nodecl_get_all_nodecls_of_kind<Nodecl::ForStatement>(
                    n.get_statement());

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
                const objlist_blocks_pairs& blocks_pairs,
                const objlist_nodecl_t& ivs_list)
    {
        objlist_ogroup_t result;
        OverlapGroup group;
        objlist_nodecl_t::const_iterator target_load =
            vector_loads.begin();

        //TODO: >1 groups
        std::cerr << "Overlap Group:" << std::endl;
        std::cerr << target_load->prettyprint() << std::endl;

        group._group_loads.append(*target_load);

        target_load++;

        while(target_load != vector_loads.end())
        {
            Nodecl::VectorLoad target_load_copy =
                target_load->shallow_copy().as<Nodecl::VectorLoad>();

            // Apply unrolling blocks offset in index
            // Reverse iterator. Blocks are inclusive
            for(objlist_blocks_pairs::const_reverse_iterator block_it =
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
                running_error("");
            }
                
            target_load++;
        }

        // TODO: Length
        if (group._group_loads.size() >= 4)
            result.append(group);

        return result;
    }


    void OverlappedAccessesOptimizer::compute_group_properties(
            OverlapGroup& ogroup,
            const Nodecl::ForStatement& for_stmt)
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
        TL::Scope scope = for_stmt.get_parent().get_parent().get_parent().
            retrieve_context();

        for (int i=0; i<2; i++)
        {
            std::stringstream new_sym_name;
            new_sym_name << "__overlap_" << i;//it->first.get_name() << "_" << i;

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

        // Group subscript
        ogroup._group_subscripted = get_vector_load_subscripted(
                vload).as<Nodecl::Symbol>();
    }


    void OverlappedAccessesOptimizer::enable_overlap_cache(
            const OverlapGroup& ogroup,
            const Nodecl::ForStatement& n,
            const objlist_nodecl_t& ivs_list)
    {
        // Init Statements
        Nodecl::List init_stmts = 
            ogroup.get_init_statements(n, ivs_list);
        n.prepend_sibling(init_stmts);
        
        // Update Pre
        Nodecl::List pre_stmts = 
            ogroup.get_iteration_update_pre();
        Nodecl::Utils::prepend_items_in_outermost_compound_statement(
                n.get_statement(), pre_stmts);

        // Update Post
        Nodecl::List post_stmts = 
            ogroup.get_iteration_update_post();
        Nodecl::Utils::append_items_in_outermost_compound_statement(
                n.get_statement(), post_stmts);
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

