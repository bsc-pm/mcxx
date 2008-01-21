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
    Source OpenMPTransform::get_critical_reduction_code(ObjectList<OpenMP::ReductionSymbol> reduction_references)
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

        for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_references.begin();
                it != reduction_references.end();
                it++)
        {
            // get the operator involved
            std::string reduced_var_name = it->get_symbol().get_name();
            std::string reduction_var_name = "rdp_" + it->get_symbol().get_name();

            std::string op = it->get_operation().prettyprint();

            reduction_gathering 
                << reduced_var_name << " = " << reduced_var_name << op << reduction_var_name << ";"
                ;
        }

        return reduction_code;
    }


    Source OpenMPTransform::get_noncritical_reduction_code(ObjectList<OpenMP::ReductionSymbol> reduction_references)
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
            ObjectList<OpenMP::ReductionSymbol> reduction_references,
            Statement inner_statement)
    {
        Source reduction_code;

        if (reduction_references.empty())
        {
            return reduction_code;
        }

        /*
         * We have a problem here, we are referencing a symbol that will be
         * declared in an enclosing context but at the moment we want to parse this
         * so we have to declare the enclosing symbols
         */
        {
            for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_references.begin();
                    it != reduction_references.end();
                    it++)
            {
                Source temporal_reduction_vectors;
                // create a reduction vector after the name of the mangled entity
                std::string reduction_vector_name = "rdv_" + it->get_symbol().get_name();

                // get its type
                Symbol reduction_symbol = it->get_symbol();
                Type reduction_type = reduction_symbol.get_type();

                // create a tree of expression 128
                // FIXME: hardcoded to 128 processors
                Source array_length;
                array_length << "128";
                AST_t array_length_tree = array_length.parse_expression(inner_statement.get_ast(), 
                        inner_statement.get_scope_link());

                // and get an array of 128 elements
                Type reduction_vector_type = reduction_type.get_array_to(array_length_tree, 
                        inner_statement.get_scope());

                // now get the code that declares this reduction vector
                temporal_reduction_vectors
                    << reduction_vector_type.get_declaration(inner_statement.get_scope(), 
                            reduction_vector_name) << ";";

                // Now parse this in the context of the construct
                /* AST_t unused_tree = */ temporal_reduction_vectors.parse_statement(inner_statement.get_ast(),
                        inner_statement.get_scope_link());
            }
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
        ObjectList<OpenMP::ReductionSymbol>& inner_reductions = inner_reductions_stack.top();
        inner_reductions.insert(reduction_references, functor(&OpenMP::ReductionSymbol::get_symbol));

        return reduction_code;
    }

    Source OpenMPTransform::get_reduction_update(ObjectList<OpenMP::ReductionSymbol> reduction_references)
    {
        Source reduction_update;

        // Discard those that came from inner constructions
        reduction_references = reduction_references.filter(
                not_in_set(inner_reductions_stack.top(), functor(&OpenMP::ReductionSymbol::get_symbol)));

        if (reduction_references.empty())
        {
            return reduction_update;
        }

        reduction_update 
            << "{"
            //                        <<    "extern int in__tone_thread_id_ ();"
            <<    "int nth_thread_id = in__tone_thread_id_();"
            ;

        for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_references.begin();
                it != reduction_references.end();
                it++)
        {
            reduction_update
                << "rdv_" << it->get_symbol().get_name() << "[nth_thread_id] = "
                << "rdp_" << it->get_symbol().get_name() << ";";
        }

        reduction_update
            << "}"
            ;


        return reduction_update;
    }

    Source OpenMPTransform::get_reduction_gathering(ObjectList<OpenMP::ReductionSymbol> reduction_references)
    {
        Source reduction_gathering;

        // For every entity being reduced
        for (ObjectList<OpenMP::ReductionSymbol>::iterator it = reduction_references.begin();
                it != reduction_references.end();
                it++)
        {
            // If it is not a user defined one it is easy

            // Construct the name of its related reduction vector
            std::string reduced_var_name = it->get_symbol().get_name();
            std::string reduction_vector_name = "rdv_" + it->get_symbol().get_name();

            // get the operator involved
            std::string op = it->get_operation().prettyprint();
            reduction_gathering
                << reduced_var_name << " = " << reduced_var_name << op << reduction_vector_name << "[rdv_i]" << ";";
        }

        return reduction_gathering;
    }
}
