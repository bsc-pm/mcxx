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

#include "tl-vectorizer-loop-info.hpp"

#include "cxx-cexpr.h"

#include "tl-vectorization-utils.hpp"
#include "tl-vectorization-analysis-interface.hpp"

namespace TL
{
namespace Vectorization
{
    VectorizerLoopInfo::VectorizerLoopInfo(
            const Nodecl::ForStatement& for_stmt,
            const VectorizerEnvironment& environment)
        : _environment(environment), _loop(for_stmt),
        _condition(for_stmt.get_loop_header().as<Nodecl::LoopControl>().
                get_cond()),
        _ivs(VectorizationAnalysisInterface::_vectorizer_analysis->
                get_ivs_nodecls(for_stmt, for_stmt))
    {
    }

    VectorizerLoopInfo::VectorizerLoopInfo(
        const Nodecl::WhileStatement& while_stmt,
        const VectorizerEnvironment& environment)
        : _environment(environment), _loop(while_stmt),
        _condition(while_stmt.get_condition()),
        _ivs(VectorizationAnalysisInterface::_vectorizer_analysis->
            get_ivs_nodecls(while_stmt, while_stmt))

    {
    }

    bool VectorizerLoopInfo::ivs_lb_depend_on_simd_iv()
    {
        bool result = false;

        for(objlist_nodecl_t::const_iterator it = _ivs.begin();
            it != _ivs.end();
            it ++)
        {
            result = result || VectorizationAnalysisInterface::_vectorizer_analysis->
                iv_lb_depends_on_ivs_from_scope(
                        _environment._analysis_scopes.back(),
                        *it,
                        _environment._analysis_simd_scope);
        }

        return result;
    }

    bool VectorizerLoopInfo::condition_depends_on_simd_iv()
    {
        return VectorizationAnalysisInterface::_vectorizer_analysis->
           is_induction_variable_dependent_expression(
                    _environment._analysis_simd_scope, _condition);
    }

    bool VectorizerLoopInfo::ivs_ub_depend_on_simd_iv()
    {
        internal_error("ivs_ub_depend_on_simd_iv is not working", 0);

        bool result = false;

        for(objlist_nodecl_t::const_iterator it = _ivs.begin();
            it != _ivs.end();
            it ++)
        {
            result = result || VectorizationAnalysisInterface::_vectorizer_analysis->
                iv_ub_depends_on_ivs_from_scope(
                        _environment._analysis_scopes.back(),
                        *it,
                        _environment._analysis_simd_scope);
        }

        return result;
    }

    bool VectorizerLoopInfo::ivs_step_depend_on_simd_iv()
    {
        bool result = false;

        for(objlist_nodecl_t::const_iterator it = _ivs.begin();
            it != _ivs.end();
            it ++)
        {
            result = result || VectorizationAnalysisInterface::
                _vectorizer_analysis->iv_step_depends_on_ivs_from_scope(
                        _environment._analysis_scopes.back(),
                        *it,
                        _environment._analysis_simd_scope);
        }

        return result;
    }

    int VectorizerLoopInfo::get_epilog_info(
            const Nodecl::ForStatement& for_statement,
            VectorizerEnvironment& environment,
            bool& only_epilog)
    {
        int remain_its = -1;
        only_epilog = false;

        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "VECTORIZER: ----- Computing epilog info -----\n");
        }

        TL::ForStatement tl_for(for_statement);

        Nodecl::NodeclBase lb = tl_for.get_lower_bound();
        Nodecl::NodeclBase ub = TL::Vectorization::Utils::get_denormalize_ub(
                for_statement);
        Nodecl::NodeclBase step = tl_for.get_step();


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
                environment._analysis_scopes.push_back(for_statement);

                // Suitable LB
                lb_is_suitable = VectorizationAnalysisInterface::
                    _vectorizer_analysis->is_suitable_expression(
                        for_statement, lb, environment._suitable_expr_list,
                        environment._unroll_factor,
                        environment._vector_length, lb_vector_size_module);

                environment._analysis_scopes.pop_back();

                // LB
                if (lb_is_suitable)
                {
//                    printf("SUITABLE LB\n");
                    const_lb = 0; // Assuming 0 for suitable LB
                }
                else if (lb_vector_size_module != -1) // Is not suitable but is constant in some way
                {
                    const_lb = lb_vector_size_module / environment._target_type.get_size();
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
                environment._analysis_scopes.push_back(for_statement);

                // Suitable UB
                ub_is_suitable = VectorizationAnalysisInterface::
                    _vectorizer_analysis->is_suitable_expression(
                        for_statement, ub, environment._suitable_expr_list,
                        environment._unroll_factor,environment._vector_length,
                        ub_vector_size_module);

                environment._analysis_scopes.pop_back();

                // UB
                if (ub_is_suitable)
                {
//                    printf("SUITABLE EPILOG\n");
                    const_ub = environment._unroll_factor;
                }
                else if (ub_vector_size_module != -1) // Is not suitable but is constant in some way
                {
                    const_ub = ub_vector_size_module / environment._target_type.get_size();

                    if (const_lb > const_ub)
                        const_ub += environment._unroll_factor;

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

            if ((num_its < environment._unroll_factor) &&
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

            remain_its = num_its % environment._unroll_factor;

            VECTORIZATION_DEBUG()
            {
                fprintf(stderr, "VECTORIZER: Constant epilog (%d iterations)\n", remain_its);
            }
        }

        if (remain_its < -1)
        {
            internal_error("VECTORIZER: Remain iterations %d < -1", remain_its);
        }

        VECTORIZATION_DEBUG()
        {
            fprintf(stderr, "\n");
        }

        return remain_its;
    }
}
}
