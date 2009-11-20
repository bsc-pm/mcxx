/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include "tl-omptransform.hpp"

#include "tl-overload.hpp"

#include <utility>

namespace TL
{
    namespace Nanos4
    {
        /*
           T f ([const] T , [const] T) -> red = f(red,elem)
           T f ([const] T* , [const] T) -> red = f(&red,elem)
           T f ([const] T* , [const] T*) -> red = f(&red,&elem)
           T f ([const] T , [const] T*) -> red = f(red,&elem)
           void f (T* , [const] T) -> f(&red,elem)
           void f (T* , [const] T*) -> f(&red,&elem)

           more for C++:

           T f ([const] T& , [const] T) -> red = f(red,elem)
           T f ([const] T , [const] T&) -> red = f(red,elem)
           T f ([const] T& , [const] T&) -> red = f(red,elem)
           T f ([const] T& , [const] T*) -> red = f(red,&elem)
           T f ([const] T* , [const] T&) -> red = f(&red,elem)
           void f (T& , [const] T) -> f(red,elem)
           void f (T& , [const] T&) -> f(red,elem)
           void f (T& , [const] T*) -> f(red,&elem)
         */

        static Source perform_reduction_symbol_member_(
                const OpenMP::ReductionSymbol& reduction_symbol,
                Source reduction_var_name, 
                Source partial_reduction)
        {
            C_LANGUAGE()
            {
                internal_error("Code unreachable", 0);
            }

            Source result;
            Symbol op_sym = reduction_symbol.get_reductor_symbol();

            Type op_type = op_sym.get_type();
            std::string op_name = op_sym.get_name();

            if (!op_type.is_function())
            {
                internal_error("This is not a function type!\n", 0);
            }

            Source partial_reduction_arg;

            TL::ObjectList<TL::Type> parameters = op_type.parameters();

            if (parameters[0].is_pointer())
            {
                partial_reduction_arg << "&(" << partial_reduction << ")";
            }
            else
            {
                partial_reduction_arg << partial_reduction;
            }

            result
                << reduction_var_name << "." << op_name << "(" << partial_reduction_arg << ");"
                ;

            return result;
        }

        static Source perform_reduction_symbol_(
                const OpenMP::ReductionSymbol& reduction_symbol,
                Source reduction_var_name, 
                Source partial_reduction)
        {
            Source result;

            Symbol op_sym = reduction_symbol.get_reductor_symbol();
            Type op_type = op_sym.get_type();
            std::string op_name = op_sym.get_qualified_name();

            if (!op_type.is_function())
            {
                internal_error("This is not a function type!\n", 0);
            }

            Source reduction_arg, partial_reduction_arg, reduction_return;
            if (op_type.returns().is_void())
            {
                if (reduction_symbol.reductor_is_left_associative())
                {
                    result
                        << op_name << "(" << reduction_arg << ", " << partial_reduction_arg << ")"
                        ;
                }
                else
                {
                    result
                        << op_name << "(" << partial_reduction_arg << "," << reduction_arg << ")"
                        ;
                }
            }
            else
            {
                if (reduction_symbol.reductor_is_left_associative())
                {
                    result
                        << reduction_return << " = " << op_name << "(" << reduction_arg << ", " << partial_reduction_arg << ")"
                        ;
                }
                else
                {
                    result
                        << reduction_return << " = " << op_name << "(" << partial_reduction_arg << ", " << reduction_arg << ")"
                        ;
                }
            }

            // This could be different if we allowed returning types other than
            // T, but this is not the case so this is always the reduced entity
            reduction_return << reduction_var_name;

            TL::ObjectList<TL::Type> parameters = op_type.parameters();

            // Indexes in the argument, by default this is the left associativity
            int reduction_index = 0;
            int partial_reduction_index = 1;

            if (!reduction_symbol.reductor_is_left_associative())
            {
                // reduction_index = 1;
                // partial_reduction_index = 0;
                std::swap(reduction_index, partial_reduction_index);
            }

            // If the type related to the reduction var is pointer, pass an address
            if (parameters[reduction_index].is_pointer())
            {
                reduction_arg << "&" << reduction_var_name
                    ;
            }
            else
            {
                reduction_arg << reduction_var_name
                    ;
            }

            if (parameters[partial_reduction_index].is_pointer())
            {
                partial_reduction_arg << "&(" << partial_reduction << ")"
                    ;
            }
            else
            {
                partial_reduction_arg << partial_reduction
                    ;
            }

            // Add trailing ';'
            result
                << ";"
                ;

            return result;
        }

        static Source perform_reduction_symbol(
                const OpenMP::ReductionSymbol& reduction_symbol,
                Source reduction_var_name, 
                Source partial_reduction)
        {
            if (reduction_symbol.is_builtin_operator())
            {
                Source result;
                std::string op = reduction_symbol.get_operation();

                if (reduction_symbol.reductor_is_left_associative())
                {
                    result 
                        << reduction_var_name << " = " << reduction_var_name << op << partial_reduction << ";"
                        ;
                }
                else
                {
                    result 
                        << reduction_var_name << " = " << partial_reduction << op << reduction_var_name << ";"
                        ;
                }
                return result;
            }
            else
            {
                if (!reduction_symbol.reductor_is_member())
                {
                    return perform_reduction_symbol_(reduction_symbol,
                            reduction_var_name,
                            partial_reduction);
                }
                else
                {
                    return perform_reduction_symbol_member_(reduction_symbol,
                            reduction_var_name,
                            partial_reduction);
                }
            }
        }

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
                // We lack a scope here for the qualified name
                std::string reduction_var_name = it->get_symbol().get_qualified_name();
                std::string partial_reduction = "rdp_" + it->get_symbol().get_name();

                reduction_gathering
                    << perform_reduction_symbol(*it,
                            reduction_var_name,
                            partial_reduction)
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
                << "for (rdv_i = 0; rdv_i < nth_team_size; rdv_i++)"
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
                <<    "int nth_team_size = nth_get_num_team_players();"
                <<    "for (rdv_i = 0; rdv_i < nth_team_size; rdv_i++)"
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
                <<    "int nth_thread_id = nth_get_player_id();"
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
                // We are lacking a scope here for the qualified name
                std::string reduction_var_name = it->get_symbol().get_qualified_name();
                std::string partial_reduction_vector_name = "rdv_" + it->get_symbol().get_name();

                // get the operator involved
                Source partial_reduction;
                partial_reduction
                    << partial_reduction_vector_name << "[rdv_i]"
                    ;
                reduction_gathering
                    << perform_reduction_symbol(*it, 
                            reduction_var_name, 
                            partial_reduction)
                    ;
            }

            return reduction_gathering;
        }
    }
}
