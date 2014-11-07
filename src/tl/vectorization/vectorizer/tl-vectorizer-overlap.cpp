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

#include "tl-optimizations.hpp"
#include "tl-nodecl-utils.hpp"
#include "hlt-loop-unroll.hpp"
#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
    Nodecl::List OverlapGroup::get_init_statements(
            const Nodecl::ForStatement& for_stmt,
            const bool is_simd_loop, 
            const bool is_omp_simd_for) const 
    {
        const objlist_nodecl_t& ivs_list = OverlappedAccessesOptimizer::
            _analysis->get_linear_nodecls(for_stmt);

        TL::Scope scope = for_stmt.retrieve_context();
        Nodecl::List result_list;

        for (int i = 0; i < (_num_registers-1); i++)
        {
            // __overlap_X_1 = vload(&a[i]);

            Nodecl::NodeclBase vload_index =
                _registers_indexes[i].shallow_copy();

            // Replace IV by LB in vload_index
            for (objlist_nodecl_t::const_iterator iv = ivs_list.begin();
                    iv != ivs_list.end();
                    iv++)
            {
                Nodecl::NodeclBase iv_lb;

                // SIMD FOR keeps IV to replece it in the Intel RTL phase
                if (is_simd_loop && is_omp_simd_for)
                {
                    iv_lb = *iv;
                }
                else
                {
                    iv_lb = OverlappedAccessesOptimizer::_analysis->
                        get_induction_variable_lower_bound(
                                for_stmt,*iv);
                }

                if (!iv_lb.is_null())
                {
                    Nodecl::Utils::nodecl_replace_nodecl_by_structure(
                            vload_index, *iv, iv_lb);
                }
            }
            
            Nodecl::List flags;
            if (_aligned_strategy)
                flags = Nodecl::List::make(
                        Nodecl::AlignedFlag::make());

            Nodecl::VectorAssignment vassignment =
                Nodecl::VectorAssignment::make(
                        _registers[i].make_nodecl(true),
                        Nodecl::VectorLoad::make(
                            Nodecl::Reference::make(
                                Nodecl::ArraySubscript::make(
                                    _subscripted.shallow_copy(),
                                    Nodecl::List::make(
                                        vload_index),
                                    _basic_type),
                                _basic_type.get_pointer_to()),
                            Utils::get_null_mask(),
                            flags,
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

        const int size = _registers.size();

        Nodecl::List flags;
        if (_aligned_strategy)
            flags = Nodecl::List::make(
                    Nodecl::AlignedFlag::make());

        // __overlap_X_1 = load(a[i + VF]) 
        Nodecl::VectorAssignment vassignment =
            Nodecl::VectorAssignment::make(
                    _registers[size-1].make_nodecl(true),
                    Nodecl::VectorLoad::make(
                        Nodecl::Reference::make(
                            Nodecl::ArraySubscript::make(
                                _subscripted.shallow_copy(),
                                Nodecl::List::make(
                                    _registers_indexes[size-1].shallow_copy()),
                                _basic_type),
                            _basic_type.get_pointer_to()),
                        Utils::get_null_mask(),
                        flags,
                        _vector_type),
                    Utils::get_null_mask(),
                    _vector_type);

        Nodecl::ExpressionStatement exp_stmt =
            Nodecl::ExpressionStatement::make(vassignment);

        result_list.append(exp_stmt);

        return result_list;
    }

    Nodecl::List OverlapGroup::get_iteration_update_post() const
    {
        Nodecl::List result_list;

        const int size = _registers.size();

        for(int i=0; i < (size-1); i++)
        {
            // __overlap_X_0 = __overlap_X_1;
            Nodecl::ExpressionStatement exp_stmt =
                Nodecl::ExpressionStatement::make(
                        Nodecl::VectorAssignment::make(
                            _registers[i].make_nodecl(true),
                            _registers[i+1].make_nodecl(true),
                            Utils::get_null_mask(),
                            _registers[i].get_type()));

            result_list.append(exp_stmt);
        }

        return result_list;
    }

    void OverlapGroup::compute_leftmost_rightmost_vloads(
            const Vectorization::VectorizerEnvironment& environment,
            const int max_registers)
    {
        leftmost_rightmost_strategy(environment, 
                true /* aligned vl strategy */);
        compute_num_registers(environment);
        _aligned_strategy = true;

        if (max_registers != 0 && _num_registers > max_registers)
        {
            leftmost_rightmost_strategy(environment,
                    false /* unaligned vl strategy */);
            _aligned_strategy = false;
            compute_num_registers(environment);
        }

        ERROR_CONDITION(max_registers != 0 && _num_registers > max_registers,
                "Register limit is too low to apply overlap optimization", 0);
    }

    void OverlapGroup::leftmost_rightmost_strategy(
            const Vectorization::VectorizerEnvironment& environment,
            const bool aligned_strategy)
    {
        Nodecl::VectorLoad first_vload = 
            _loads.begin()->as<Nodecl::VectorLoad>();

        Nodecl::NodeclBase first_subscript =
            Utils::get_vector_load_subscript(first_vload);

        std::cerr << "First subscript: " 
            << first_subscript.prettyprint()
            << std::endl;

        int min_offset = 0;
        int max_offset = 0;

        Nodecl::VectorLoad min_vload = first_vload;
        Nodecl::VectorLoad max_vload = first_vload;

        // Find the leftmost (min) vload and the rightmost (max)
        // and compute their offsets (num elements) from the first_subscript
        for(objlist_nodecl_t::const_iterator load_it =
                _loads.begin();
                load_it != _loads.end();
                load_it++)
        {
            Nodecl::NodeclBase load_subscript =
                Utils::get_vector_load_subscript(
                        load_it->as<Nodecl::VectorLoad>());

            Nodecl::Minus shifted_elements = Nodecl::Minus::make(
                    load_subscript.shallow_copy(),
                    first_subscript.shallow_copy(),
                    load_subscript.get_type());

            TL::Optimizations::UnitaryReductor unitary_reductor;
            unitary_reductor.reduce(shifted_elements);

            if (shifted_elements.is_constant())
            {
                int offset = const_value_cast_to_4(
                        shifted_elements.get_constant());

                if (offset < min_offset)
                {
                    min_offset = offset;
                    min_vload = load_it->as<Nodecl::VectorLoad>();
                }

                if (offset > max_offset)
                {
                    max_offset = offset;
                    max_vload = load_it->as<Nodecl::VectorLoad>();
                }
            }
        }
      
        // ALIGNED STRATEGY 
        if (aligned_strategy)
        {
            // min_vload = the leftmost aligned load
            Nodecl::List flags = min_vload.get_flags().as<Nodecl::List>();

            bool aligned = !(flags.find_first<Nodecl::AlignedFlag>().is_null());

            if (!aligned)
            {
                Nodecl::NodeclBase alignment_node = flags.find_first<Nodecl::AlignmentInfo>();
                if (alignment_node.is_null())
                    running_error("Overlap error: There is no alignment info for %s",
                            min_vload.prettyprint().c_str());

                int alignment = const_value_cast_to_4(alignment_node.get_constant());

                int min_vload_type_size = min_vload.get_type().basic_type().get_size();
                int negative_num_elements = alignment/min_vload_type_size;

                std::cerr << "OVERLAP ALIGNMENT: " << alignment 
                    << " negative offset " << negative_num_elements
                    << " num elements" << std::endl;

                // New flags
                Nodecl::List new_flags = flags.shallow_copy().as<Nodecl::List>();
                new_flags.append(Nodecl::AlignedFlag::make());
                Nodecl::Utils::remove_from_enclosing_list(
                        new_flags.find_first<Nodecl::AlignmentInfo>());

                // New aligned array
                Nodecl::ArraySubscript new_array = Utils::get_vector_load_scalar_access(
                        min_vload).shallow_copy().as<Nodecl::ArraySubscript>();
                Nodecl::NodeclBase subscript = new_array.get_subscripts().
                    as<Nodecl::List>().front().no_conv();

                const_value_t* const_int = const_value_get_signed_int(
                        negative_num_elements);
                const_value_t* neg_int = const_value_neg(const_int);

                Nodecl::Neg neg = Nodecl::Neg::make(
                        const_value_to_nodecl(const_int),
                        subscript.get_type(),
                        subscript.get_locus());
                neg.set_constant(neg_int);


                Nodecl::NodeclBase new_subscript;
                if (subscript.is_constant())
                {
                    new_subscript = const_value_to_nodecl(
                            const_value_add(neg_int,
                                subscript.get_constant()));
                }
                else
                {
                    new_subscript = Nodecl::Add::make(
                            neg,
                            subscript.shallow_copy(),
                            subscript.get_type(),
                            subscript.get_locus());
                }

                subscript.replace(new_subscript);

                Nodecl::VectorLoad aligned_vector_load =
                    Nodecl::VectorLoad::make(
                            Nodecl::Reference::make(
                                new_array.shallow_copy(),
                                new_array.get_type().get_pointer_to(),
                                new_array.get_locus()),
                            min_vload.get_mask().shallow_copy(),
                            new_flags,
                            min_vload.get_type(),
                            min_vload.get_locus());

                min_vload = aligned_vector_load;
                min_offset = min_offset - negative_num_elements;

                Optimizations::ReduceExpressionVisitor reduce_expression_visitor;
                reduce_expression_visitor.walk(min_vload);
            }

            // max_vload = the rightmost aligned load
            flags = max_vload.get_flags().as<Nodecl::List>();
            aligned = !(flags.find_first<Nodecl::AlignedFlag>().is_null());

            if (!aligned)
            {
                Nodecl::NodeclBase alignment_node = flags.find_first<Nodecl::AlignmentInfo>();
                if (alignment_node.is_null())
                    running_error("Overlap error: There is no alignment info for %s",
                            max_vload.prettyprint().c_str());

                int alignment = const_value_cast_to_4(alignment_node.get_constant());

                int max_vload_type_size = max_vload.get_type().basic_type().get_size();
                int positive_num_elements = environment._vectorization_factor -
                    alignment/max_vload_type_size;

                std::cerr << "OVERLAP ALIGNMENT: " << alignment 
                    << " positive offset " << positive_num_elements
                    << " num elements" << std::endl;

                // New flags ***************************************
                Nodecl::List new_flags = flags.shallow_copy().as<Nodecl::List>();
                new_flags.append(Nodecl::AlignedFlag::make());
                Nodecl::Utils::remove_from_enclosing_list(
                        new_flags.find_first<Nodecl::AlignmentInfo>());

                // New aligned array
                Nodecl::ArraySubscript new_array = Utils::get_vector_load_scalar_access(
                        max_vload).shallow_copy().as<Nodecl::ArraySubscript>();
                Nodecl::NodeclBase subscript = new_array.get_subscripts().
                    as<Nodecl::List>().front().no_conv();

                const_value_t* const_int = const_value_get_signed_int(
                        positive_num_elements);

                Nodecl::NodeclBase new_subscript;
                if (subscript.is_constant())
                {
                    new_subscript = const_value_to_nodecl(
                            const_value_add(const_int,
                                subscript.get_constant()));
                }
                else
                {
                    new_subscript = Nodecl::Add::make(
                            const_value_to_nodecl(const_int),
                            subscript.shallow_copy(),
                            subscript.get_type(),
                            subscript.get_locus());
                }

                subscript.replace(new_subscript);

                Nodecl::VectorLoad aligned_vector_load =
                    Nodecl::VectorLoad::make(
                            Nodecl::Reference::make(
                                new_array.shallow_copy(),
                                new_array.get_type().get_pointer_to(),
                                new_array.get_locus()),
                            max_vload.get_mask().shallow_copy(),
                            new_flags,
                            max_vload.get_type(),
                            max_vload.get_locus());

                max_vload = aligned_vector_load;
                max_offset = max_offset + positive_num_elements;

                Optimizations::ReduceExpressionVisitor reduce_expression_visitor;
                reduce_expression_visitor.walk(max_vload);
            }
        }
        // UNALIGNED STRATEGY
        else
        {
            Nodecl::NodeclBase leftmost_index =
                Utils::get_vector_load_subscript(min_vload);
            Nodecl::NodeclBase rightmost_index =
                Utils::get_vector_load_subscript(max_vload);

            Nodecl::Minus minus = Nodecl::Minus::make(
                    rightmost_index.no_conv().shallow_copy(),
                    leftmost_index.no_conv().shallow_copy(),
                    leftmost_index.get_type());

            TL::Optimizations::UnitaryReductor unitary_reductor;
            unitary_reductor.reduce(minus);

            const_value_t* mod = const_value_mod(
                    minus.get_constant(),
                    const_value_get_signed_int(
                        environment._vectorization_factor));

            if (!const_value_is_zero(mod))
            {
                int positive_num_elements = environment._vectorization_factor -
                    const_value_cast_to_4(mod);
                
                // Max vload flags == Min vload flags
                Nodecl::List new_flags =
                    min_vload.get_flags().shallow_copy().as<Nodecl::List>();

                // New array
                Nodecl::ArraySubscript new_array = Utils::get_vector_load_scalar_access(
                        max_vload).shallow_copy().as<Nodecl::ArraySubscript>();
                Nodecl::NodeclBase subscript = new_array.get_subscripts().
                    as<Nodecl::List>().front().no_conv();

                const_value_t* const_int = const_value_get_signed_int(
                        positive_num_elements);

                Nodecl::NodeclBase new_subscript;
                if (subscript.is_constant())
                {
                    new_subscript = const_value_to_nodecl(
                            const_value_add(const_int,
                                subscript.get_constant()));
                }
                else
                {
                    new_subscript = Nodecl::Add::make(
                            const_value_to_nodecl(const_int),
                            subscript.shallow_copy(),
                            subscript.get_type(),
                            subscript.get_locus());
                }

                subscript.replace(new_subscript);

                Nodecl::VectorLoad vector_load =
                    Nodecl::VectorLoad::make(
                            Nodecl::Reference::make(
                                new_array.shallow_copy(),
                                new_array.get_type().get_pointer_to(),
                                new_array.get_locus()),
                            max_vload.get_mask().shallow_copy(),
                            new_flags,
                            max_vload.get_type(),
                            max_vload.get_locus());

                max_vload = vector_load;
                max_offset = max_offset + positive_num_elements;

                Optimizations::ReduceExpressionVisitor reduce_expression_visitor;
                reduce_expression_visitor.walk(max_vload);
            }
        }

        _leftmost_vload = min_vload;
        _rightmost_vload = max_vload;

        if (aligned_strategy)
            std::cerr << "ALIGNED STRATEGY: " << std::endl;
        else
            std::cerr << "UNALIGNED STRATEGY: " << std::endl;

        std::cerr << "Min index is " << _leftmost_vload.prettyprint()
            << " with " << min_offset << " offset" << std::endl;
        std::cerr << "Max index is " << _rightmost_vload.prettyprint()
            << " with " << max_offset << " offset" << std::endl;
    }

    void OverlapGroup::compute_num_registers(
            const Vectorization::VectorizerEnvironment& environment)
    {
        Nodecl::NodeclBase leftmost_index =
            Utils::get_vector_load_subscript(_leftmost_vload);
        Nodecl::NodeclBase rightmost_index =
            Utils::get_vector_load_subscript(_rightmost_vload);

        Nodecl::Minus minus = Nodecl::Minus::make(
                rightmost_index.no_conv().shallow_copy(),
                leftmost_index.no_conv().shallow_copy(),
                leftmost_index.get_type());

        TL::Optimizations::UnitaryReductor unitary_reductor;
        unitary_reductor.reduce(minus);

        const_value_t* mod = const_value_mod(
                minus.get_constant(),
                const_value_get_signed_int(
                    environment._vectorization_factor));
        const_value_t* div = const_value_div(
                minus.get_constant(),
                const_value_get_signed_int(
                    environment._vectorization_factor));

        std::cerr << "Rightmost: " << rightmost_index.prettyprint()
            << " MINUS Leftmost: " << leftmost_index.prettyprint()
            << " = "
            << minus.prettyprint()
            << ". Mod = " << const_value_cast_to_4(mod)
            << ". Div = " << const_value_cast_to_4(div)
            << std::endl;

        ERROR_CONDITION(!const_value_is_zero(mod),
                "Leftmost and Rightmost are not multiple of VL", 0);

        _num_registers = const_value_cast_to_4(div) + 1;
    }
 

    VectorizationAnalysisInterface *OverlappedAccessesOptimizer::_analysis = 0;

    OverlappedAccessesOptimizer::OverlappedAccessesOptimizer(
            VectorizerEnvironment& environment,
            VectorizationAnalysisInterface *analysis,
            const bool is_omp_simd_for,
            const bool is_epilog,
            Nodecl::List& prependix_stmts)
        : _environment(environment), _is_omp_simd_for(is_omp_simd_for),
        _is_epilog(is_epilog), _prependix_stmts(prependix_stmts),
        _first_analysis(analysis)
    {
        _analysis = analysis;
    }

    void OverlappedAccessesOptimizer::update_alignment_info(
            const Nodecl::NodeclBase& main_loop,
            const Nodecl::NodeclBase& if_epilog)
    {
        Nodecl::NodeclBase func_code = 
            Nodecl::Utils::get_enclosing_function(
                    main_loop).get_function_code();

        Optimizations::canonicalize_and_fold(func_code, false /*fast math*/);

        _analysis = new VectorizationAnalysisInterface(
                func_code,
                Analysis::WhichAnalysis::INDUCTION_VARS_ANALYSIS);
        /*
        std::cerr << "FUNCTION: "
            << func_code.as<Nodecl::FunctionCode>().get_statements().prettyprint()
            << std::endl
            << "END FUNCTION"
            << std::endl;
        */
        objlist_nodecl_t main_vector_loads = Nodecl::Utils::
            nodecl_get_all_nodecls_of_kind<Nodecl::VectorLoad>(main_loop);

        objlist_nodecl_t epilog_vector_loads = Nodecl::Utils::
            nodecl_get_all_nodecls_of_kind<Nodecl::VectorLoad>(if_epilog);

        // Epilog contains less vector loads
        objlist_nodecl_t::iterator main_it = main_vector_loads.begin();
        for(objlist_nodecl_t::iterator epilog_it = epilog_vector_loads.begin();
                epilog_it != epilog_vector_loads.end();
                main_it++, epilog_it++)
        {
            int alignment_output;
            Nodecl::VectorLoad main_vl = main_it->as<Nodecl::VectorLoad>();
            Nodecl::VectorLoad epilog_vl = epilog_it->as<Nodecl::VectorLoad>();
           
            Nodecl::List flags = main_vl.get_flags().as<Nodecl::List>();

            if(_analysis->is_simd_aligned_access(
                    _environment._analysis_simd_scope,
                    Utils::get_vector_load_scalar_access(main_vl),
                    _environment._aligned_symbols_map,
                    _environment._suitable_exprs_list,
                    1, //vectorization factor. The code is already vectorized
                    main_vl.get_type().get_size(),
                    alignment_output) &&
                    flags.find_first<Nodecl::AlignedFlag>().is_null())
            {
                flags.append(Nodecl::AlignedFlag::make());

                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, "%s (aligned)\n", main_vl.prettyprint().c_str());
                }
            }
            else if (alignment_output != -1 &&
                    flags.find_first<Nodecl::AlignmentInfo>().is_null())
            {
                flags.append(Nodecl::AlignmentInfo::make(
                            const_value_get_signed_int(alignment_output)));

                fprintf(stderr, "%s (alignment info = %d)\n",
                        main_vl.prettyprint().c_str(), alignment_output);
            }

            main_vl.set_flags(flags);
            epilog_vl.set_flags(flags.shallow_copy());
        }

        // Update final vector loads from main loop
        while(main_it != main_vector_loads.end())
        {
            int alignment_output;
            Nodecl::VectorLoad main_vl = main_it->as<Nodecl::VectorLoad>();
           
            Nodecl::List flags = main_vl.get_flags().as<Nodecl::List>();

            if(_analysis->is_simd_aligned_access(
                    _environment._analysis_simd_scope,
                    Utils::get_vector_load_scalar_access(main_vl),
                    _environment._aligned_symbols_map,
                    _environment._suitable_exprs_list,
                    1, //vectorization factor. The code is already vectorized
                    main_vl.get_type().get_size(),
                    alignment_output) &&
                    flags.find_first<Nodecl::AlignedFlag>().is_null())
            {
                flags.append(Nodecl::AlignedFlag::make());

                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, "%s (aligned)\n", main_vl.prettyprint().c_str());
                }
            }
            else if (alignment_output != -1 &&
                    flags.find_first<Nodecl::AlignmentInfo>().is_null())
            {
                flags.append(Nodecl::AlignmentInfo::make(
                            const_value_get_signed_int(alignment_output)));

                fprintf(stderr, "%s (alignment info = %d)\n",
                        main_vl.prettyprint().c_str(), alignment_output);
            }

            main_vl.set_flags(flags);

            main_it++;
        }
    }

    void OverlappedAccessesOptimizer::visit(const Nodecl::ForStatement& n)
    {
        Nodecl::ForStatement main_loop = n;
        Nodecl::ForStatement if_epilog;
        Nodecl::ForStatement last_epilog;

        int min_unroll_factor = get_loop_min_unroll_factor(
                main_loop);

        std::cerr << "MIN UNROLL FACTOR (FOR CONDITIONAL EPILOG): " 
            << min_unroll_factor << std::endl;

        // UNROLL
        if (min_unroll_factor > 0)
        {
            // Generate conditional blocking 
            if_epilog = get_overlap_blocked_unrolled_loop(
                    main_loop, min_unroll_factor);

            // Main Loop
            TL::HLT::LoopUnroll loop_unroller;
            loop_unroller.set_loop(main_loop)
                .set_unroll_factor(16)          //TODO: 16!
                .unroll();

            Nodecl::NodeclBase whole_main_transformation =
                loop_unroller.get_whole_transformation();

            // Main loop is now the the unrolled version of 'n'
            main_loop = loop_unroller.get_unrolled_loop()
                .as<Nodecl::ForStatement>();

            last_epilog = loop_unroller.get_epilog_loop()
                .as<Nodecl::ForStatement>();

            // Replace n with the whole unrolling transformation
            n.replace(whole_main_transformation);

            // Update alignment info of "new" vector loads after unrolling
            // THIS CALL COMPUTES A NEW ANALYSIS
            update_alignment_info(n, if_epilog);

            // Add conditional epilog before the simple epilog
            last_epilog.prepend_sibling(if_epilog);
        }

        // TODO:
        TL::Scope scope = main_loop.get_parent().get_parent().get_parent().
            retrieve_context();

        // OVERLAP
        for(map_tlsym_objlist_int_t::const_iterator it = 
                _environment._overlap_symbols_map.begin();
                it != _environment._overlap_symbols_map.end();
                it++)
        {
            TL::Symbol sym = it->first;
            objlist_int_t overlap_params = it->second;

            int min_group_loads = overlap_params[0];
            int max_group_registers = overlap_params[1];
            int max_groups = overlap_params[2];

            // MAIN LOOP
            objlist_nodecl_t main_loop_vector_loads =
                get_adjacent_vector_loads_not_nested_in_for(
                        main_loop.get_statement(), sym);

            if (!main_loop_vector_loads.empty())
            {
                objlist_ogroup_t overlap_groups = 
                    get_overlap_groups(
                            main_loop_vector_loads,
                            min_group_loads,
                            max_group_registers,
                            max_groups);

                int num_group = 0;
                for(objlist_ogroup_t::iterator ogroup =
                        overlap_groups.begin();
                        ogroup != overlap_groups.end();
                        ogroup++)
                {
                    // MAIN LOOP
                    compute_group_properties(*ogroup, scope,
                            max_group_registers, num_group);
                    insert_group_update_stmts(*ogroup, main_loop,
                            _is_omp_simd_for || !_is_epilog /*init_cache*/,
                            !_is_epilog /*update post*/);
                    replace_overlapped_loads(*ogroup);

                    num_group++;
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
                                min_group_loads,
                                max_group_registers,
                                max_groups);

                    int num_group = 0;
                    for(objlist_ogroup_t::iterator ogroup =
                            if_epilog_overlap_groups.begin();
                            ogroup != if_epilog_overlap_groups.end();
                            ogroup++)
                    {
                        compute_group_properties(*ogroup, scope,
                                max_group_registers, num_group);
                        insert_group_update_stmts(*ogroup, if_epilog,
                            false /*init_cache*/, false /*update post*/);

                        replace_overlapped_loads(*ogroup);

                        num_group++;
                    }
                }
            }
        }

        // Transform if epilogue loop into IfStatement
        /*if (!if_epilog.is_null())
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
        */
 
        // Delete new analysis and restore the previous one
        if (min_unroll_factor > 0)
        {
            delete(_analysis);
            _analysis = _first_analysis;
        }

        walk(main_loop.get_statement());
        if (!if_epilog.is_null())
            walk(if_epilog.get_statement());

        // Add #pragma nounroll to main_loop and epilog
        if (min_unroll_factor > 1)
        {
            Nodecl::UnknownPragma unroll_pragma =
                Nodecl::UnknownPragma::make("nounroll");

            main_loop.prepend_sibling(unroll_pragma.shallow_copy());
            last_epilog.prepend_sibling(unroll_pragma.shallow_copy());
        }

        // Remove if_epilog if it's not necessary
        if (min_unroll_factor == 16) //TODO: 16!
        {
            Nodecl::Utils::remove_from_enclosing_list(if_epilog);
        }
    }

    unsigned int OverlappedAccessesOptimizer::get_loop_min_unroll_factor(
            Nodecl::ForStatement n)
    {
        const objlist_nodecl_t& ivs_list = _analysis->
            get_linear_nodecls(n);

        // We do not unroll the SIMD loop
        if (_environment._analysis_simd_scope
                == n)
            return 0;

        Nodecl::NodeclBase iv = ivs_list.front();
        
        if (Nodecl::Utils::structurally_equal_nodecls(iv,
                    _analysis->get_induction_variable_lower_bound(
                        n, iv), true))
                return 0;

        int unroll_factor = 0;

        for(map_tlsym_objlist_int_t::const_iterator it = 
                _environment._overlap_symbols_map.begin();
                it != _environment._overlap_symbols_map.end();
                it++)
        {
            TL::Symbol sym = it->first;
            const int min_group_loads = it->second[0];

            objlist_nodecl_t vector_loads =
                get_adjacent_vector_loads_not_nested_in_for(
                        n.get_statement(), sym);

            const int vector_loads_size = vector_loads.size();
            if (vector_loads_size > 0 &&
                    vector_loads_size <= min_group_loads &&
                    (unroll_factor * vector_loads_size)
                    < min_group_loads)
            {
                unroll_factor = min_group_loads /
                    vector_loads_size; 
            }
        }

        return unroll_factor;
    }

    /*
    Nodecl::ForStatement OverlappedAccessesOptimizer::
        get_overlap_unrolled_loop(
            const Nodecl::ForStatement& n,
            const unsigned int unroll_factor)
    {
        Nodecl::ForStatement unrolled_loop;

        // If Epilog
        if (unroll_factor > 1 )
        {
            TL::HLT::LoopUnroll loop_unroller;
            loop_unroller.set_loop(n)
                .set_create_epilog(false)
                .set_unroll_factor(unroll_factor)
                .unroll();

            unrolled_loop = 
                loop_unroller.get_unrolled_loop()
                .as<Nodecl::ForStatement>();
        }
        else
        {
            unrolled_loop = n.shallow_copy()
                .as<Nodecl::ForStatement>();
        }

        return unrolled_loop;
    }
    */

    Nodecl::ForStatement OverlappedAccessesOptimizer::
        get_overlap_blocked_unrolled_loop(
            const Nodecl::ForStatement& n,
            const unsigned int block_size)
    {
        Nodecl::ForStatement blocked_unrolled_loop;

        const objlist_nodecl_t& ivs_list = _analysis->
            get_linear_nodecls(n);

        // If Epilog
        if (block_size > 1 )
        {
            TL::HLT::LoopUnroll loop_unroller;
            loop_unroller.set_loop(n)
                .set_create_epilog(false)
                .set_unroll_factor(block_size)
                .unroll();

            blocked_unrolled_loop = 
                loop_unroller.get_unrolled_loop()
                .as<Nodecl::ForStatement>();
        }
        else
        {
            blocked_unrolled_loop = n.shallow_copy()
                .as<Nodecl::ForStatement>();
        }

        Nodecl::NodeclBase loop_header =
            blocked_unrolled_loop.get_loop_header();

        Nodecl::List loop_stmts =
            blocked_unrolled_loop.get_statement()
            .as<Nodecl::List>();

        TL::LoopControlAdapter lc = 
            TL::LoopControlAdapter(loop_header);

        Nodecl::NodeclBase cond_node = lc.get_cond();
        Nodecl::NodeclBase next_node = lc.get_next();

        Nodecl::ExpressionStatement next_update_stmt =
            Nodecl::ExpressionStatement::make(
                    next_node.shallow_copy());

        Nodecl::NodeclBase if_statement_body = 
            blocked_unrolled_loop.get_statement()
            .as<Nodecl::List>().shallow_copy();

        // Add IV update to end of the first block
        Nodecl::Utils::append_items_in_outermost_compound_statement(
                loop_stmts,
                next_update_stmt.shallow_copy());

        // Pointer
        Nodecl::List outer_stmt = loop_stmts;

        int num_unrolled_blocks = 
            (_environment._vectorization_factor % block_size) == 0 ? 
            (_environment._vectorization_factor / block_size) -1 :
            _environment._vectorization_factor / block_size;

        for (int i=1; i<num_unrolled_blocks; i++)
        {
            // New IfStatement unrolling block
            Nodecl::IfElseStatement if_else_stmt =
                Nodecl::IfElseStatement::make(
                        cond_node.shallow_copy(),
                        if_statement_body.shallow_copy(),
                        Nodecl::NodeclBase::null());
           

            // TEMPORAL PATCH: TO BE REMOVED FROM HERE
            // Waiting for local iv increment
            objlist_nodecl_t vector_loads = Nodecl::Utils::
                nodecl_get_all_nodecls_of_kind<Nodecl::VectorLoad>(if_else_stmt);

            for (objlist_nodecl_t::const_iterator vl = vector_loads.begin();
                    vl != vector_loads.end();
                    vl++)
            {
                bool found = false;
                for (map_tlsym_objlist_int_t::const_iterator overlap_symbol =
                        _environment._overlap_symbols_map.begin();
                        overlap_symbol != _environment._overlap_symbols_map.end();
                        overlap_symbol++)
                {
                    if ((overlap_symbol->first) == Utils::get_vector_load_subscripted(
                                vl->as<Nodecl::VectorLoad>()).get_symbol())
                    {
                        // Replace IV by IV + block offset in IfStatement
                        // WATCH OUT!: The code after this replacement is invalid
                        // because there is also an IV update. But there is no
                        // other way to do it so far.
                        for (objlist_nodecl_t::const_iterator iv =
                                ivs_list.begin();
                                iv != ivs_list.end();
                                iv++)
                        {
                            Nodecl::Add iv_plus_boffset =
                                Nodecl::Add::make(
                                        iv->shallow_copy(),
                                        const_value_to_nodecl(
                                            const_value_get_signed_int(i * block_size)),
                                        TL::Type::get_int_type());

                            Nodecl::Utils::nodecl_replace_nodecl_by_structure(
                                    *vl,
                                    *iv,
                                    iv_plus_boffset);
                        }

                        found = true;
                    }
                }

                ERROR_CONDITION(!found, "Overlap: This code is not going to work without local IV increment", 0);
            }
            // REMOVE UNTIL HERE!
            
            // Add IV update to the end of each block
            Nodecl::Utils::append_items_in_outermost_compound_statement(
                    if_else_stmt.get_then(),
                    next_update_stmt.shallow_copy());

            // std::cerr << "BLOCK " << i << if_else_stmt.prettyprint() << std::endl;
            // Add IfStatement
            Nodecl::Utils::append_items_in_outermost_compound_statement(
                    outer_stmt, if_else_stmt);

            outer_stmt = if_else_stmt.get_then().as<Nodecl::List>();
        }

        // Replace loop by IfStatement
       blocked_unrolled_loop.replace(
               Nodecl::IfElseStatement::make(
                   cond_node.shallow_copy(),
                   blocked_unrolled_loop.get_statement().shallow_copy(),
                   Nodecl::NodeclBase::null()));

        return blocked_unrolled_loop;
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
                Nodecl::NodeclBase subscripted= 
                    Utils::get_vector_load_subscripted(
                            vload->as<Nodecl::VectorLoad>());

                if (subscripted.is<Nodecl::Symbol>() &&
                        (subscripted.get_symbol() == sym))
                {
                    result.append(*vload);
                }
            }
        }

        return result;
    }

    bool OverlappedAccessesOptimizer::overlap(
            const Nodecl::VectorLoad& vector_load,
            objlist_nodecl_t group)
    {
        int VF = vector_load.get_type().vector_num_elements();

        Nodecl::NodeclBase vl_subscripts =
            Utils::get_vector_load_subscript(vector_load);

        for(objlist_nodecl_t::iterator it =
                group.begin();
                it != group.end();
                it++)
        {
            Nodecl::NodeclBase it_subscripts =
                Utils::get_vector_load_subscript(
                        it->as<Nodecl::VectorLoad>());

            Nodecl::Minus minus = Nodecl::Minus::make(
                    vl_subscripts.no_conv().shallow_copy(),
                    it_subscripts.no_conv().shallow_copy(),
                    vl_subscripts.get_type());

            TL::Optimizations::UnitaryReductor unitary_reductor;
            unitary_reductor.reduce(minus);

            VECTORIZATION_DEBUG()
            {
                std::cerr << "Difference: " << vl_subscripts.prettyprint()
                    << " MINUS " << it_subscripts.prettyprint()
                    << " = "
                    << minus.prettyprint()
                    << std::endl;
            }

            if (minus.is_constant() && 
                    abs(const_value_cast_to_4(minus.get_constant())) < VF)
            {
                return true;
            }
        }

        return false;
    }

    objlist_ogroup_t OverlappedAccessesOptimizer::
        get_overlap_groups(const objlist_nodecl_t& vector_loads,
                const int min_group_loads,
                const int max_group_registers,
                const int max_groups)
    {
        objlist_ogroup_t ogroups;

        for (objlist_nodecl_t::const_iterator target_load =
                vector_loads.begin();
                target_load != vector_loads.end();
                target_load++)
        {
            Nodecl::VectorLoad target_load_copy =
                target_load->shallow_copy().as<Nodecl::VectorLoad>();

            // Apply unrolling blocks offset in index
            // Reverse iterator. Blocks are inclusive
            /*
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
            */
            bool og_found = false;
            for(objlist_ogroup_t::iterator it_ogroup =
                    ogroups.begin();
                    it_ogroup != ogroups.end();
                    it_ogroup++)
            {
                if(overlap(target_load_copy,
                            it_ogroup->_loads))
                {
                    std::cerr << target_load_copy.prettyprint() << " overlap!"<< std::endl;
                    target_load->replace(target_load_copy);
                    it_ogroup->_loads.append(*target_load);

                    og_found = true;
                    break;
                }
            }

            if (!og_found) // NEW GROUP
            {
                OverlapGroup ogroup;

                std::cerr << "Building a new Overlap Group for "
                    << target_load_copy.prettyprint()
                    << std::endl;

                target_load->replace(target_load_copy);
                ogroup._loads.append(*target_load);
                ogroups.append(ogroup);
            }
        }

        std::cerr << "Overlap Groups Summary:" << std::endl;
        std::cerr << "    - Total groups: "
            << ogroups.size() << std::endl;
        std::cerr << "    - Groups after merging: "
            << ogroups.size() << std::endl;

        // TODO: Merge overlaped groups
        

        // TODO: Length
        //if (group._loads.size() >= min_group_size)
        //    result.append(group);
        
        for(objlist_ogroup_t::iterator it_ogroup =
                ogroups.begin();
                it_ogroup != ogroups.end();)
        {
            if (it_ogroup->_loads.size() < 
                    (unsigned int) min_group_loads)
            {
                ogroups.erase(it_ogroup);
            }
            else
            {
                it_ogroup++;
            }
        }

        std::cerr << "    - Groups after min cardinality filtering: "
            << ogroups.size() << std::endl;

        return ogroups;
    }

    void OverlappedAccessesOptimizer::compute_group_properties(
            OverlapGroup& ogroup,
            TL::Scope& scope,
            const int max_registers,
            const int num_group)
    {
        ogroup._vector_type = ogroup._loads.front().
            get_type().no_ref().get_unqualified_type();
        ogroup._basic_type = ogroup._loads.front().
            get_type().no_ref().basic_type();
        int vectorization_factor = ogroup._vector_type.vector_num_elements();

        ogroup.compute_leftmost_rightmost_vloads(
                _environment, max_registers);

        // Group subscript
        ogroup._subscripted = Utils::get_vector_load_subscripted(
                ogroup._loads.front().as<Nodecl::VectorLoad>()).
                as<Nodecl::Symbol>();

        Nodecl::NodeclBase leftmost_index = 
            Utils::get_vector_load_subscript(ogroup._leftmost_vload);

        // Declare group registers
        for (int i=0; i<ogroup._num_registers; i++)
        {
            std::stringstream new_sym_name;
            new_sym_name << "__overlap_" 
                << ogroup._subscripted.get_symbol().get_name() << "_"
                << num_group << "_"
                << i;

            if (!scope.get_symbol_from_name(
                    new_sym_name.str()).is_valid())
            {
                // Create new symbols
                std::cerr << "Creating new cache symbol: "
                    << new_sym_name.str()
                    << std::endl;

                TL::Symbol new_sym = scope.new_symbol(new_sym_name.str());
                new_sym.get_internal_symbol()->kind = SK_VARIABLE;
                symbol_entity_specs_set_is_user_declared(new_sym.get_internal_symbol(), 1);
                new_sym.set_type(ogroup._vector_type);

                ogroup._registers.push_back(new_sym);
                ogroup._registers_indexes.push_back(
                        (i == 0) ? leftmost_index : 
                        Nodecl::Add::make(
                            leftmost_index.shallow_copy(),
                            const_value_to_nodecl(const_value_mul(
                                    const_value_get_signed_int(i),
                                    const_value_get_signed_int(
                                        vectorization_factor))),
                            leftmost_index.get_type()));
            }
            else
            {
                // Use previous symbols
                TL::Symbol sym = 
                    scope.get_symbol_from_name(new_sym_name.str());

                ERROR_CONDITION(!sym.is_valid(), "cache symbol is invalid.", 0);

                ogroup._registers.push_back(sym);
                ogroup._registers_indexes.push_back(
                        (i == 0) ? leftmost_index : 
                        Nodecl::Add::make(
                            leftmost_index.shallow_copy(),
                            const_value_to_nodecl(const_value_mul(
                                    const_value_get_signed_int(i),
                                    const_value_get_signed_int(
                                        vectorization_factor))),
                            leftmost_index.get_type()));
            }
        }
    }


    void OverlappedAccessesOptimizer::insert_group_update_stmts(
            OverlapGroup& ogroup,
            const Nodecl::ForStatement& n,
            const bool init_cache,
            const bool update_post)
    {
        // Init Statements
        if (init_cache)
        {
            bool is_simd_loop = _environment._analysis_simd_scope == n;

            Nodecl::NodeclBase init_stmts =
                ogroup.get_init_statements(n, is_simd_loop,
                        _is_omp_simd_for);

            if(is_simd_loop)
            {
                _prependix_stmts.prepend(init_stmts);
            }
            else
            {
                n.prepend_sibling(init_stmts);
            }
        }

        if (update_post)
        {
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
        for(objlist_nodecl_t::const_iterator load_it =
                ogroup._loads.begin();
                load_it != ogroup._loads.end();
                load_it++)
        {
            Nodecl::NodeclBase load_subscript =
                Utils::get_vector_load_subscript(
                        load_it->as<Nodecl::VectorLoad>());

            Nodecl::Minus shifted_elements = Nodecl::Minus::make(
                    load_subscript.shallow_copy(),
                    ogroup._registers_indexes[0].no_conv().shallow_copy(),
                    load_subscript.get_type());

            TL::Optimizations::UnitaryReductor unitary_reductor;
            unitary_reductor.reduce(shifted_elements);

            if (shifted_elements.is_constant())
            {
                const_value_t* mod = const_value_mod(
                            shifted_elements.get_constant(),
                            const_value_get_signed_int(
                                _environment._vectorization_factor));
                const_value_t* div = const_value_div(
                            shifted_elements.get_constant(),
                            const_value_get_signed_int(
                                _environment._vectorization_factor));

                int first_register = const_value_cast_to_4(div);
                int final_offset = const_value_cast_to_4(mod);

                if (const_value_is_zero(mod))
                {
                    load_it->replace(
                            ogroup._registers[first_register].
                            make_nodecl(true));
                }
                else
                {
                    load_it->replace(Nodecl::VectorAlignRight::make(
                                ogroup._registers[first_register+1].make_nodecl(true),
                                ogroup._registers[first_register].make_nodecl(true),
                                const_value_to_nodecl(const_value_get_signed_int(final_offset)),
                                load_it->as<Nodecl::VectorLoad>().
                                get_mask().shallow_copy(),
                                ogroup._registers[first_register].get_type()));
                }


                std::cerr << "Align elements: " << load_subscript.prettyprint()
                    << " MINUS " << ogroup._registers_indexes[0].prettyprint()
                    << " = "
                    << shifted_elements.prettyprint()
                    << ". Registers: " << first_register 
                    << ", " << first_register+1
                    << ". Offset: " << final_offset
                    << std::endl;
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

