/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-omptransform.hpp"

namespace TL
{
    Source OpenMPTransform::get_critical_reduction_code(ObjectList<OpenMP::ReductionIdExpression> reduction_references)
    {
        Source reduction_code;

        if (reduction_references.empty())
        {
            // Nothing to do if the reduction set is empty
            return reduction_code;
        }

        Source reduction_gathering;

        reduction_code
            << comment("Reduction implemented with a spin lock since this construct is orphaned")
            << "{"
            <<    "static nth_word_t default_mutex;"
            //                    <<    "extern nthf_spin_lock_(void*);"
            //                    <<    "extern nthf_spin_unlock_(void*);"

            <<    "nthf_spin_lock_(&default_mutex);"
            <<    reduction_gathering
            <<    "nthf_spin_unlock_(&default_mutex);"
            << "}"
            ; 

        for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                it != reduction_references.end();
                it++)
        {
            if (!it->is_user_defined())
            {
                // get the operator involved
                std::string reduced_var_name = it->get_id_expression().mangle_id_expression();
                std::string reduction_var_name = "rdp_" + it->get_id_expression().mangle_id_expression();

                std::string op = it->get_operation().prettyprint();

                reduction_gathering 
                    << reduced_var_name << " = " << reduced_var_name << op << reduction_var_name << ";"
                    ;
            }
            else
            {
                Source one_urd_reduction = get_one_user_defined_gathering(*it);
                reduction_gathering << one_urd_reduction;
            }
        }

        return reduction_code;
    }


    Source OpenMPTransform::get_noncritical_reduction_code(ObjectList<OpenMP::ReductionIdExpression> reduction_references)
    {
        Source reduction_code;

        if (reduction_references.empty())
        {
            return reduction_code;
        }

        // Create the source code that gathers the values computed by every thread
        Source reduction_gathering;

        reduction_code
            << comment("Reduction code noncritical performed after the join")
            << "int rdv_i;"
            << "for (rdv_i = 0; rdv_i < nth_nprocs; rdv_i++)"
            << "{"
            <<    reduction_gathering
            << "}"
            ;

        Source reduction_gethering;

        reduction_gathering = get_reduction_gathering(reduction_references);

        return reduction_code;
    }

    Source OpenMPTransform::get_noncritical_inlined_reduction_code(
            ObjectList<OpenMP::ReductionIdExpression> reduction_references)
    {
        Source reduction_code;

        if (reduction_references.empty())
        {
            return reduction_code;
        }

        Source reduction_update;
        Source reduction_gathering;

        reduction_code
            << comment("Inlined reduction code since this construct is not orphaned")
            << reduction_update
            //                    << "extern void in__tone_barrier_();"
            //                    << "extern char in__tone_is_master_();"

            << "in__tone_barrier_();"
            << "if (in__tone_is_master_())"
            << "{"
            <<    "int rdv_i;"
            //                    <<    "extern int nthf_cpus_actual_();"

            <<    "int nth_nprocs = nthf_cpus_actual_();"
            <<    "for (rdv_i = 0; rdv_i < nth_nprocs; rdv_i++)"
            <<    "{"
            <<       reduction_gathering
            <<    "}"
            << "}"
            ;

        reduction_update = get_reduction_update(reduction_references);
        reduction_gathering = get_reduction_gathering(reduction_references);

        // We push them onto the stack of inner_reductions because this
        // functions is only called when this for is not orphaned
        ObjectList<OpenMP::ReductionIdExpression>& inner_reductions = inner_reductions_stack.top();
        inner_reductions.insert(reduction_references, functor(&OpenMP::ReductionIdExpression::get_symbol));

        return reduction_code;
    }

    Source OpenMPTransform::get_reduction_update(ObjectList<OpenMP::ReductionIdExpression> reduction_references)
    {
        Source reduction_update;

        // Discard those that came from inner constructions
        reduction_references = reduction_references.filter(
                not_in_set(inner_reductions_stack.top(), functor(&OpenMP::ReductionIdExpression::get_symbol)));

        if (reduction_references.empty())
        {
            return reduction_update;
        }

        reduction_update 
            << "{"
            //                        <<    "extern int in__tone_thread_id_ ();"
            <<    "int nth_thread_id = in__tone_thread_id_();"
            ;

        for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                it != reduction_references.end();
                it++)
        {
            reduction_update
                << "rdv_" << it->get_id_expression().mangle_id_expression() << "[nth_thread_id] = "
                << "rdp_" << it->get_id_expression().mangle_id_expression() << ";";
        }

        reduction_update
            << "}"
            ;


        return reduction_update;
    }

    Source OpenMPTransform::get_reduction_gathering(ObjectList<OpenMP::ReductionIdExpression> reduction_references)
    {
        Source reduction_gathering;

        // For every entity being reduced
        for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                it != reduction_references.end();
                it++)
        {
            // And reduce for this element of the reduction vector
            if (!it->is_user_defined())
            {
                // If it is not a user defined one it is easy

                // Construct the name of its related reduction vector
                std::string reduced_var_name = it->get_id_expression().prettyprint();
                std::string reduction_vector_name = "rdv_" + it->get_id_expression().mangle_id_expression();

                // get the operator involved
                std::string op = it->get_operation().prettyprint();
                reduction_gathering
                    << reduced_var_name << " = " << reduced_var_name << op << reduction_vector_name << "[rdv_i]" << ";";
            }
            else
            {
                Source one_urd_reduction = get_one_user_defined_gathering(*it);
                reduction_gathering << one_urd_reduction;
            }
        }

        return reduction_gathering;
    }

    Source OpenMPTransform::get_one_user_defined_gathering(OpenMP::ReductionIdExpression reduction_id_expr)
    {
        IdExpression reductor = reduction_id_expr.get_user_defined_reductor();
        Symbol reductor_symbol = reductor.get_symbol();

        Type reductor_type = reductor_symbol.get_type();

        if (!reductor_type.is_function())
        {
            std::cerr << "User defined reduction in " 
                << reductor.get_ast().get_locus() << " does not refer a function. Ignoring" << std::endl;
            return Source("");
        }

        // Construct the name of its related reduction vector
        std::string reduced_var_name = reduction_id_expr.get_id_expression().prettyprint();
        std::string reduction_vector_name = "rdv_" + reduction_id_expr.get_id_expression().mangle_id_expression();

        Source reduction_gathering;

        // FIXME - For C++ this is more difficult. Currently not implemented
        // Extract the unqualified part of the id-expression
        // and if it is a member construct a member-access with function call
        //
        // The id-expression
        //
        // Lets "happily" assume that if the reductor returns void is of the form
        //
        //    void f(T*, T);
        //    void f(T&, T);
        //
        // otherwise we will assume it is of type 
        //
        //    T f(T, T);
        //
        if (reductor_type.returns().is_void())
        {
            // If the first parameter is a pointer we will assume that the reductor is of this form
            //
            //    void f(T*, t);
            //
            // otherwise it will be assumed to be
            //
            //    void f(T&, t);
            //
            ObjectList<Type> parameters = reductor_type.parameters();

            if (parameters[0].is_pointer())
            {
                reduction_gathering
                    << reductor.prettyprint() << "(&" << reduced_var_name << "," << reduction_vector_name << "[rdv_i]" << ");";
            }
            else
            {
                reduction_gathering
                    << reductor.prettyprint() << "(" << reduced_var_name << "," << reduction_vector_name << "[rdv_i]" << ");";
            }
        }
        else
        {
            reduction_gathering
                << reduced_var_name << " = " << reductor.prettyprint() << "(" << reduced_var_name << "," << reduction_vector_name << "[rdv_i]" << ");";
        }

        return reduction_gathering;
    }
}
