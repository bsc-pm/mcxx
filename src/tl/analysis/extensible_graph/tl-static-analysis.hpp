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


#ifndef TL_STATIC_ANALYSIS_HPP
#define TL_STATIC_ANALYSIS_HPP


#include "tl-nodecl.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-loop-analysis.hpp"


namespace TL
{
    namespace Analysis
    {
        class LIBTL_CLASS StaticAnalysis {    
            
        private:
            LoopAnalysis* _loop_analysis;
            
            //! Computes the data-flow equation for each node in a iterative way 
            //! until the information stops changing.
            /*!
                It is mandatory to use before #gather_live_initial_information.
                */
            static void solve_live_equations(Node* node);
            
            //! Computes on iteration of the method #solve_live_equations.
            /*!
                Live out (X) = Union of all Live in (Y),
                                for all Y successors of X.
                Live in (X) = Upper exposed (X) + 
                            ( Live out (X) - Killed (X) )
                */
            static void solve_live_equations_recursive(Node* actual, bool& changed);
            
            static void substitute_reaching_definition_known_values(Node* node);

            void propagate_reaching_definitions_to_graph_node(Node* node, std::map<Symbol, Nodecl::NodeclBase> induct_vars,
                                                            const char* filename, int line);
            
            void propagate_reach_defs_among_nodes(Node* node, bool& changes);
        
            void extend_reaching_definitions_info(Node* node);
            
            static Nodecl::NodeclBase rename_nodecl(Nodecl::NodeclBase nodecl, std::map<Symbol, Nodecl::NodeclBase> rename_map);
        
            static nodecl_map compute_parents_reach_defs(Node* node);
            
            static void analyse_tasks_rec(Node* current);
            
            /*!
             * Scope all the variables appearing in the task by traversing recursively the graph starting from #current
             * \param task Node containing the task to be auto-scoped 
             * \param current Node we are analysing right now
             * \param scoped_vars List of variables that has already been auto-scoped
             */
            static void compute_auto_scoping_rec(Node* task, Node* current, ext_sym_set& scoped_vars);
            
            /*!
             * The method scopes a variable of a task node
             * \param task node where the variable has to be scoped
             * \param ei variable to be scoped
             * \param scoped_vars list of variables that has already been scoped for #task
             */
            static void scope_variable(Node* task, ExtensibleSymbol ei, ext_sym_set& scoped_vars);
            
            /*!
             * Determines whether a variable is used out of a task between the point where the task is scheduled and 
             * the point where the task is synchronized
             * \param task Node containing the task to be analysed
             * \param ei Variable we are looking for
             * \return character containing the usage of the variable: '0' => not used, '1' => only read, '2' => write
             */
            static char var_is_used_out_task(Node* task, ExtensibleSymbol ei);
            
            /*!
             * This method determines whether it can exist a race condition for a symbol in a given task node
             * \param task Node containing the task we are analysing
             * \param ei Extensible Symbol we want to check race conditions on
             */
            static bool race_condition(Node* task, ExtensibleSymbol ei);
        
        public:
            
            StaticAnalysis(LoopAnalysis* loop_analysis);
            
            //! Computes the liveness analysis of a node
            //! The method needs the use-def chains to be calculated before
            static void live_variable_analysis(Node* node);            
            
            //! Computes auto-scoping for variables appearing in a task inside #current
            static void compute_auto_scoping(Node* task);
            
            //! Computes dependences for all task node in the Extensible Graph
            static void analyse_tasks(Node* graph_node);
            
            //! Computes dependences for a node containing a task code
            static void analyse_task(Node* task_node);
            
            friend class CfgVisitor;    // Needed for IPA Analysis
            friend class LoopAnalysis;
        };
    }
}

#endif      // TL_STATIC_ANALYSIS_HPP