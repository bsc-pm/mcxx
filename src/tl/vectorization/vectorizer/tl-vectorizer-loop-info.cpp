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

#include "tl-vectorizer-loop-info.hpp"

#include "tl-vectorization-utils.hpp"
#include "tl-vectorizer.hpp"
#include "tl-optimizations.hpp"

#include "cxx-cexpr.h"

namespace TL
{
namespace Vectorization
{
    VectorizerLoopInfo::VectorizerLoopInfo(
            const Nodecl::NodeclBase& loop_stmt,
            const VectorizerEnvironment& environment)
        : _environment(environment), _loop(loop_stmt),
        _ivs(Vectorizer::_vectorizer_analysis->
                get_linear_nodecls(loop_stmt))
    {
        if(loop_stmt.is<Nodecl::ForStatement>())
        {
            _condition = loop_stmt.as<Nodecl::ForStatement>()
                .get_loop_header().as<Nodecl::LoopControl>().get_cond();
        }
        else if (loop_stmt.is<Nodecl::WhileStatement>())
        {
            _condition = loop_stmt.as<Nodecl::WhileStatement>().
                get_condition();
        }
        else
        {
            fatal_error("VectorizerLoopInfo: Neither a for nor a while");
        }
    }

    bool VectorizerLoopInfo::ivs_values_are_uniform_in_simd_scope()
    {
        bool ivs_values_uniform = true;

        // It also works for Whiles
        Nodecl::NodeclBase statements = _loop.as<Nodecl::ForStatement>()
            .get_statement().as<Nodecl::List>().front();

        for(objlist_nodecl_t::const_iterator it = _ivs.begin();
                ivs_values_uniform && it != _ivs.end();
                it ++)
        {
            // Use for statements as statement
            // nodecl_value
            ivs_values_uniform = Vectorizer::
                _vectorizer_analysis->is_uniform(
                        _environment._analysis_simd_scope,
                        statements, *it);
        }

        return ivs_values_uniform;
    }

    bool VectorizerLoopInfo::condition_is_uniform_in_simd_scope()
    {
        // It also works for Whiles
        Nodecl::ForStatement for_stmt = _loop.as<Nodecl::ForStatement>();
        Nodecl::NodeclBase statements = for_stmt.get_statement().as<Nodecl::List>()
            .front();

        return Vectorizer::_vectorizer_analysis->
            is_uniform(_environment._analysis_simd_scope,
                    statements, _condition);
    }

    int VectorizerLoopInfo::get_epilog_info(bool& only_epilog)
    {
        int remain_its = -1;
        only_epilog = false;

        if (_loop.is<Nodecl::ForStatement>())
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: ----- Computing epilog info -----\n");
            }

            TL::ForStatementHelper<TL::NoNewNodePolicy> tl_for(
                    _loop.as<Nodecl::ForStatement>());

            Nodecl::NodeclBase lb = tl_for.get_lower_bound();
            Nodecl::NodeclBase step = tl_for.get_step();
            Nodecl::NodeclBase closed_ub = tl_for.get_upper_bound();

            const_value_t* one = const_value_get_one(1, 4);

            Nodecl::NodeclBase ub = Nodecl::Add::make(Vectorizer::
                    _vectorizer_analysis->shallow_copy(closed_ub),
                    const_value_to_nodecl(one),
                    closed_ub.get_type());

            // Canonicalize and register symbols
            ObjectList<Nodecl::NodeclBase> original_ub_syms = 
                Nodecl::Utils::nodecl_get_all_nodecls_of_kind<Nodecl::Symbol>(ub);
 
            TL::Optimizations::canonicalize_and_fold(ub, false /*fast math*/);

            ObjectList<Nodecl::NodeclBase> canon_ub_syms = 
                Nodecl::Utils::nodecl_get_all_nodecls_of_kind<Nodecl::Symbol>(ub);

            for (ObjectList<Nodecl::NodeclBase>::const_iterator it = canon_ub_syms.begin();
                    it != canon_ub_syms.end();
                    it++)
            {
                Nodecl::NodeclBase original_sym =
                    *Nodecl::Utils::list_get_nodecl_by_structure(
                            original_ub_syms, *it);

                Vectorizer::_vectorizer_analysis->register_identical_copy(
                        original_sym, *it);
            }
 
            
            if (closed_ub.is_constant())
                ub.set_constant(const_value_add(closed_ub.get_constant(), one));


            if(step.is_constant())
            {
                long long int const_lb;
                long long int const_ub;
                long long int const_step = const_value_cast_to_8(step.get_constant());


                bool ub_is_suitable = false;
                bool lb_is_suitable = false;
                int lb_vector_size_module = -1;
                int ub_vector_size_module = -1;


                if (lb.is_constant())
                {
                    const_lb = const_value_cast_to_8(lb.get_constant());
                }
                else
                {
                    // Push ForStatement as scope for analysis
                    //environment._analysis_simd_scope = for_statement;
                    _environment._analysis_scopes.push_back(_loop);

                    // Suitable LB
                    lb_is_suitable
                        = Vectorizer::_vectorizer_analysis
                              ->is_suitable_expression(
                                  _loop,
                                  lb,
                                  _environment._suitable_exprs_list,
                                  _environment._vec_factor,
                                  _environment._vec_factor,
                                  lb_vector_size_module);

                    _environment._analysis_scopes.pop_back();

                    // LB
                    if (lb_is_suitable)
                    {
                        //                    printf("SUITABLE LB\n");
                        const_lb = 0; // Assuming 0 for suitable LB
                    }
                    else if (lb_vector_size_module != -1) // Is not suitable but is constant in some way
                    {
                        const_lb = lb_vector_size_module;
                        //                    printf("VECTOR MODULE LB EPILOG %lld\n", const_lb);
                    }
                    else // We cannot say anything about the number of iterations of the epilog
                    {
                        //                    printf("DEFAULT EPILOG LB\n");
                        VECTORIZATION_DEBUG()
                        {
                            fprintf(stderr, "VECTORIZER: Default epilog (LB)\n\n");
                        }

                        return remain_its; // -1
                    }
                }

                if (ub.is_constant())
                {
                    const_ub = const_value_cast_to_8(ub.get_constant());
                }
                else
                {
                    // Push ForStatement as scope for analysis
                    //environment._analysis_simd_scope = for_statement;
                    _environment._analysis_scopes.push_back(_loop);

                    // Suitable UB
                    // ub is normalized to <= so +1 is needed
                    ub_is_suitable = Vectorizer::_vectorizer_analysis
                                         ->is_suitable_expression(
                                             _loop,
                                             ub,
                                             _environment._suitable_exprs_list,
                                             _environment._vec_factor,
                                             _environment._vec_factor,
                                             ub_vector_size_module);

                    _environment._analysis_scopes.pop_back();

                    // UB
                    if (ub_is_suitable)
                    {
                        //                    printf("SUITABLE EPILOG\n");
                        const_ub = _environment._vec_factor;
                    }
                    else if (ub_vector_size_module != -1) // Is not suitable but is constant in some way
                    {
                        const_ub = ub_vector_size_module;

                        if (const_lb > const_ub)
                            const_ub += _environment._vec_factor;

                        //                    printf("VECTOR MODULE EPILOG %lld\n", const_ub);
                    }
                    else // We cannot say anything about the number of iterations of the epilog
                    {
                        //                    printf("DEFAULT EPILOG\n");
                        VECTORIZATION_DEBUG()
                        {
                            fprintf(stderr, "VECTORIZER: Default epilog (UB)\n\n");
                        }

                        return remain_its; // -1
                    }
                }


                // Compute epilog its
                long long int num_its = (((const_ub - const_lb)%const_step) == 0) ?
                    ((const_ub - const_lb)/const_step) : ((const_ub - const_lb)/const_step) + 1;

                //std::cerr << num_its << " " << const_ub << " " << const_lb << " " << const_step << " " << ub_vector_size_module << " "
                //    << lb_vector_size_module << " " << ub_is_suitable << " "
                //    << lb_is_suitable << std::endl;

                if ((num_its < _environment._vec_factor) &&
                        (!ub_is_suitable) && (!lb_is_suitable) &&
                        (ub_vector_size_module == -1) &&
                        (lb_vector_size_module == -1))
                {
                    VECTORIZATION_DEBUG()
                    {
                        fprintf(stderr, "VECTORIZER: Only epilog\n");
                    }
                    only_epilog = true;
                }

                remain_its = num_its % _environment._vec_factor;

                VECTORIZATION_DEBUG()
                {
                    fprintf(stderr, "VECTORIZER: Constant epilog (%d iterations)\n", remain_its);
                }
            }

            if (remain_its < -1)
            {
                internal_error("VECTORIZER: Remain iterations %d < -1", remain_its);
            }
        }
        else if (_loop.is<Nodecl::WhileStatement>())
        {
            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Epilog analysis for WhileStatement not"\
                       " implemented yet. Using default epilog\n");
            }
        }

        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "\n");
        }

        return remain_its;
    }

/*
    DEPRECATED bool VectorizerLoopInfo::ivs_lb_depend_on_simd_iv()
    {
        bool result = false;

        for(objlist_nodecl_t::const_iterator it = _ivs.begin();
            it != _ivs.end();
            it ++)
        {
            result = result || Vectorizer::_vectorizer_analysis->
                iv_lb_depends_on_ivs_from_scope(
                        _environment._analysis_scopes.back(),
                        *it,
                        _environment._analysis_simd_scope);
        }

        return result;
    }

    DEPRECATED bool VectorizerLoopInfo::condition_depends_on_simd_iv()
    {
        return Vectorizer::_vectorizer_analysis->
           is_induction_variable_dependent_expression(
                    _environment._analysis_simd_scope, _condition);
    }

    DEPRECATED bool VectorizerLoopInfo::ivs_ub_depend_on_simd_iv()
    {
        internal_error("ivs_ub_depend_on_simd_iv is not working", 0);

        bool result = false;

        for(objlist_nodecl_t::const_iterator it = _ivs.begin();
            it != _ivs.end();
            it ++)
        {
            result = result || Vectorizer::_vectorizer_analysis->
                iv_ub_depends_on_ivs_from_scope(
                        _environment._analysis_scopes.back(),
                        *it,
                        _environment._analysis_simd_scope);
        }

        return result;
    }

    DEPRECATED bool VectorizerLoopInfo::ivs_step_depend_on_simd_iv()
    {
        bool result = false;

        for(objlist_nodecl_t::const_iterator it = _ivs.begin();
            it != _ivs.end();
            it ++)
        {
            result = result || Vectorizer::
                _vectorizer_analysis->iv_step_depends_on_ivs_from_scope(
                        _environment._analysis_scopes.back(),
                        *it,
                        _environment._analysis_simd_scope);
        }

        return result;
    }
*/
}
}
