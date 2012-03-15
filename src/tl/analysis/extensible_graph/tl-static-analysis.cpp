/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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



#include <typeinfo>

#include "tl-cfg-analysis-visitor.hpp"
#include "tl-cfg-visitor.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-extensible-symbol.hpp"
#include "tl-loop-analysis.hpp"
#include "tl-static-analysis.hpp"


namespace TL
{
    namespace Analysis
    {
        static void compute_params_usage_in_unknown_func_call(Nodecl::NodeclBase n, ext_sym_set& undef_behaviour_vars, ext_sym_set& ue_vars);
        static ObjectList<Node*> uses_from_node_to_node(Node* current, Node* end, ExtensibleSymbol ei, Node* task);
        
        // *** NODE *** //
        
        void Node::fill_use_def_sets(Nodecl::NodeclBase n, bool defined)
        {
            ExtensibleSymbol s(n);
            if (defined)
            {
                set_killed_var(ExtensibleSymbol(n));
            }
            else
            {
                ext_sym_set killed_vars = get_killed_vars();
                if (!killed_vars.contains(s))
                {
                    set_ue_var(s);
                }
            }
        }   
    
        void Node::fill_use_def_sets(Nodecl::List n_l, bool defined)
        {
            for(Nodecl::List::iterator it = n_l.begin(); it != n_l.end(); ++it)
            {
                fill_use_def_sets(*it, defined);
            }
        }
        
        StaticAnalysis::StaticAnalysis(LoopAnalysis* loop_analysis)
            :_loop_analysis(loop_analysis), _next_sync(NULL), _simultaneous_tasks(),
            _firstprivate_vars(), _private_vars(), _shared_vars()
        {}
        
        void StaticAnalysis::live_variable_analysis(Node* node)
        {
            // Common Liveness analysis
            solve_live_equations(node);
            ExtensibleGraph::clear_visits(node);
            
            // Iterated tasks treatment
            solve_specific_live_in_tasks(node);
            ExtensibleGraph::clear_visits(node);
        }
        
        void StaticAnalysis::solve_live_equations(Node* node)
        {
            bool changed = true;
            while (changed)
            {
                changed = false;
                solve_live_equations_recursive(node, changed);
                ExtensibleGraph::clear_visits(node);
            }
        }

        void StaticAnalysis::solve_specific_live_in_tasks(Node* node)
        {
            if (!node->is_visited())
            {
                node->set_visited(true);
                if (node->get_type() == GRAPH_NODE)
                {
                    if (node->get_graph_type() == TASK)
                    {
                        if (task_is_in_loop(node))
                        {
                            ext_sym_set task_li = node->get_live_in_vars();
                            ext_sym_set task_lo = node->get_live_out_vars();
                            for (ext_sym_set::iterator it = task_li.begin(); it != task_li.end(); ++it)
                            {
                                if (ext_sym_set_contains_englobed_nodecl(*it, task_lo))
                                {
                                    delete_englobed_var_from_list(*it, task_lo);
                                    node->set_live_out(*it);
                                }
                                else if (!ext_sym_set_contains_englobing_nodecl(*it, task_lo))
                                {
                                    node->set_live_out(*it);
                                }
                            }
                        }
                    }
                    
                    solve_specific_live_in_tasks(node->get_graph_entry_node());
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    solve_specific_live_in_tasks(*it);
                }
            }
        }

        static std::string prettyprint_ext_sym_set(ext_sym_set s)
        {
            std::string result;
            
            for(ext_sym_set::iterator it = s.begin(); it != s.end(); ++it)
            {
                if (it->get_nodecl().is_null())
                {
                    result += it->get_name() + ", ";
                }
                else
                {
                    std::string nodecl_string(codegen_to_str(it->get_nodecl().get_internal_nodecl()));
                    result += nodecl_string + ", ";                
                }
            }
            
            result = result.substr(0, result.size()-2);
            
            return result;
        }

        void StaticAnalysis::solve_live_equations_recursive(Node* actual, bool& changed)
        {
            if (!actual->is_visited())
            {
                actual->set_visited(true);
                
                Node_type ntype = actual->get_type();
                if (ntype != BASIC_EXIT_NODE)
                {
                    ObjectList<Node*> children = actual->get_children();
                    
                    if (ntype == GRAPH_NODE)
                    {
                        solve_live_equations_recursive(actual->get_graph_entry_node(), changed);
                    }

                    if (ntype != BASIC_ENTRY_NODE)
                    {
                        ext_sym_set old_live_in = actual->get_live_in_vars();
                        ext_sym_set old_live_out = actual->get_live_out_vars();
                        ext_sym_set live_out, live_in, aux_live_in, aux;
                        
                        // Computing Live out
                        for(ObjectList<Node*>::iterator it = children.begin();it != children.end();++it)
                        {
                            Node_type nt = (*it)->get_type();
                            if (nt == BASIC_EXIT_NODE)
                            {
                                ObjectList<Node*> outer_children = (*it)->get_outer_node()->get_children();
                                for(ObjectList<Node*>::iterator itoc = outer_children.begin(); itoc != outer_children.end(); ++itoc)
                                {
                                    ext_sym_set outer_live_in = (*itoc)->get_live_in_vars();
                                    for (ext_sym_set::iterator itli = outer_live_in.begin(); itli != outer_live_in.end(); ++itli)
                                    {
                                        aux_live_in.insert(*itli);
                                    }
                                }
                            }
                            else
                            {
                                aux_live_in = (*it)->get_live_in_vars();
                            }
                            live_out.insert(aux_live_in);
                        }
                        
                        // Computing Live In
                        aux = sets_difference(live_out, actual->get_killed_vars());
                        live_in = sets_union(actual->get_ue_vars(), aux);
                        if (ntype == GRAPH_NODE)
                        {
                            Scope sc(actual->get_scope());
                            if (sc.is_valid())
                            {   // Delete those variables who are local to the graph
                                ext_sym_set inner_live_in = live_in;
                                live_in.clear();
                                for(ext_sym_set::iterator it = inner_live_in.begin(); it != inner_live_in.end(); ++it)
                                {
                                    ObjectList<Symbol> syms = it->get_symbols();
                                    for (ObjectList<Symbol>::iterator its = syms.begin(); its != syms.end(); ++its)
                                    {   // If one of the symbols in the expression is not local, then the whole symbol is not local
                                        if ( !its->get_scope().scope_is_enclosed_by(sc) && its->get_scope() != sc )
                                            live_in.append(*it);
                                    }
                                }
                            }
                        }
                        
                        if (!sets_equals(old_live_in, live_in) || !sets_equals(old_live_out, live_out))
                        {
                            actual->set_live_in(live_in);
                            actual->set_live_out(live_out);
                            changed = true;
                        }
                    }
                    
                    for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                    {
                        solve_live_equations_recursive(*it, changed);
                    }
                }
            }
        }
        
        static void purge_set_of_auto_scoped_array(ExtensibleSymbol ei, ext_sym_set list)
        {
            if (ext_sym_set_contains_englobed_nodecl(ei, list))
            {
                ext_sym_set fake_list(1, ei);
                for(ext_sym_set::iterator it2 = list.begin(); it2 != list.end(); ++it2)
                {
                    if (ext_sym_set_contains_englobing_nodecl(*it2, fake_list))
                    {
                        list.erase(it2);
                        it2--;
                    }
                }
            }
        }
        
        /*!
         * 4. For each use ai; i2[0::N] (where N is the number of uses) of an array variable a appearing within the task t.
         *      (a) We apply the methodology used for the scalars.
         *      (b) Since OpenMP does not allow different scopes for the subparts of a variable, then we have to mix all the results 
         *          we have get in the previous step. In order to do that we will follow the rules bellow:
         *          i. If the whole array a or all the parts ai have the same scope sc, then a is scoped as sc.
         *          ii. If there are different regions of the array with different scopes, then:
         *              A. If some ai has been scoped as UNDEFINED then a is scoped as UNDEFINED.
         *              B. If at least one ai is FIRSTPRIVATE and all aj; j2[0::N] where j! = i are PRIVATE, then a is scoped as FIRSTPRIVATE.
         *              C. If at least one ai is SHARED and all aj; j2[0::N] where j! = i are PRIVATE or FIRSTPRIVATE, then, 
         *                  fulfilling the sequential consistency rules, a is scoped as SHARED.
         */
        void StaticAnalysis::mix_array_computations(Node* task_node)
        {
            ext_sym_set new_private, new_firstprivate, new_shared, new_undef;
            
            for(ext_sym_set::iterator it = _undefined_vars.begin(); it != _undefined_vars.end(); ++it)
            {
                // Add the whole array to the new set
                new_undef.insert(ExtensibleSymbol(ExtensibleSymbol::get_nodecl_base(it->get_nodecl())));
                if (it->is_array())
                {
                    // Delete it from the other groups
                    purge_set_of_auto_scoped_array(*it, _undefined_vars);
                    purge_set_of_auto_scoped_array(*it, _firstprivate_vars);
                    purge_set_of_auto_scoped_array(*it, _private_vars);
                    purge_set_of_auto_scoped_array(*it, _shared_vars);
                }
            }
            task_node->set_undef_sc_var(new_undef); _undefined_vars.clear();
            
            for(ext_sym_set::iterator it = _shared_vars.begin(); it != _shared_vars.end(); ++it)
            {
                // Add the whole array to the new set
                new_shared.insert(ExtensibleSymbol(ExtensibleSymbol::get_nodecl_base(it->get_nodecl())));
                // Delete it from the other groups
                if (it->is_array())
                {    
                    purge_set_of_auto_scoped_array(*it, _firstprivate_vars);
                    purge_set_of_auto_scoped_array(*it, _private_vars);
                }
            }
            task_node->set_shared_var(new_shared); _shared_vars.clear();
            
            for(ext_sym_set::iterator it = _firstprivate_vars.begin(); it != _firstprivate_vars.end(); ++it)
            {
                // Add the whole array to the new set
                new_firstprivate.insert(ExtensibleSymbol(ExtensibleSymbol::get_nodecl_base(it->get_nodecl())));
                // Delete it from the other groups
                
                purge_set_of_auto_scoped_array(*it, _private_vars);
            }
            task_node->set_firstprivate_var(new_firstprivate); _firstprivate_vars.clear();
            task_node->set_private_var(_private_vars); _private_vars.clear();
        }
        
        // FIXME For the moment we assume the user has used the 'auto-deps' clause
        /*!
         * Input values al those which are Live in and are not Live out
         * Output values al those which are Live out and are not Live in
         * Inout values al those which are Live in and Live out
         */
        char StaticAnalysis::analyse_task(Node* task_node)
        {            
            // Scope variables as Private, Firstprivate, Shared or UndefinedScope
            char computed = compute_auto_scoping(task_node);
            mix_array_computations(task_node);
            
            if (computed == '1')
            {
                // Specify Shared Variables into Input, Output and Inout
                ext_sym_set in_vars = task_node->get_live_in_vars();
                ext_sym_set out_vars = task_node->get_live_out_vars();
                ext_sym_set input_deps, output_deps, inout_deps;
                ext_sym_set shared_vars = task_node->get_shared_vars();
                for (ext_sym_set::iterator it = shared_vars.begin(); it != shared_vars.end(); ++it)
                {
                    if (ext_sym_set_contains_englobing_nodecl(*it, in_vars))
                    {
                        if (ext_sym_set_contains_englobing_nodecl(*it, out_vars))
                        {
                            inout_deps.insert(*it);
                        }
                        else
                        {
                            input_deps.insert(*it);
                        }
                    }
                    else if (ext_sym_set_contains_englobing_nodecl(*it, out_vars))
                    {
                        output_deps.insert(*it);
                    }
                    else
                    {
                        std::cerr << "warning: variable " << it->get_nodecl().prettyprint() 
                                << " computed as shared but it is not LiveIn nor LiveOut" << std::endl;
                    }
                }
                
                task_node->set_input_deps(input_deps);
                task_node->set_output_deps(output_deps);
                task_node->set_inout_deps(inout_deps);
                task_node->set_undef_deps(task_node->get_undef_sc_vars());
                
                task_node->set_deps_computed();
            }
            else
            {
                std::cerr << "Task \"" << task_node->get_graph_label().prettyprint() << "\" auto-scoping cannot be computed "
                          << "because there is no synchronization point defined at the end of the task" << std::endl;
            }
            
            return computed;
        }
        
        void StaticAnalysis::analyse_tasks_rec(Node* task)
        {
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                std::cerr << std::endl << "   ==> Task '" << task->get_graph_label().prettyprint() << "'" << std::endl;
                    
            char state = analyse_task(task);

            if ( (CURRENT_CONFIGURATION->debug_options.analysis_verbose 
                    || CURRENT_CONFIGURATION->debug_options.enable_debug_code) 
                && state == '1') 
            {
                task->print_use_def_chains();
                task->print_liveness();
                task->print_auto_scoping();
                task->print_task_dependencies();
            }
            
            // Clear temporary values for task analysis
            _next_sync = NULL;
            _simultaneous_tasks.clear();
        }
        
        static ObjectList<Node*> get_graph_tasks(Node* current)
        {
            ObjectList<Node*> tasks;
            if (!current->is_visited())
            {
                current->set_visited(true);
                if (current->get_type() == GRAPH_NODE)
                {
                    if (current->get_graph_type() == TASK)
                    {
                        tasks.append(current);
                    }
                    
                    tasks.append(get_graph_tasks(current->get_graph_entry_node()));
                }
                    
                ObjectList<Node*> children = current->get_children();
                for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {    
                    tasks.append(get_graph_tasks(*it));
                }
            }
            return tasks;
        }
        
        void StaticAnalysis::analyse_tasks(Node* graph_node)
        {
            ObjectList<Node*> tasks = get_graph_tasks(graph_node);
            ExtensibleGraph::clear_visits(graph_node);
           
            for(ObjectList<Node*>::iterator it = tasks.begin(); it != tasks.end(); ++it)
            {
                analyse_tasks_rec(*it);
                ExtensibleGraph::clear_visits(*it);
            }
        }

        /*!
        * 1. Determine the different regions that interfere in the analysis of t:
        *   − One region is the one defined by the code in the encountered thread that can potentially be executed 
        *     in parallel with the task. This region is defined by two points:
        *      · Scheduling: is the point where the task is scheduled. Any previous access to v by the encountering 
        *                    thread is irrelevant when analysing the task because it is already executed.
        *      · Next_sync: is the point where the task is synchronized with the rest of the threads in execution. 
        *                   This point can only be a barrier or a taskwait. Here we take into account that taskwait 
        *                   constructs only enforces the synchronization of these tasks which are child of the current 
        *                   task region.
        *   − Other regions are the ones enclosed in tasks that can be executed in parallel with t. We will call these 
        *     tasks ti; i2[0::T ] and the region of code where we can find tasks in this condition is defined by:
        *      · Last_sync: is the immediately previous point to the scheduling point where a synchronization enforces 
        *                   all previous executions to be synchronized. We can only assure this point with a barrier and
        *                   in specific cases with a taskwait. We only can trust the taskwait if we know all the code 
        *                   executed previously and we can assure that the current task region has not generated 
        *                   grandchild tasks.
        *      · Next_sync: is the same point as explained for the analysis of the encountered thread. In order to 
        *                   simplify the reading of the algorithm bellow, from now on we will talk about the region 
        *                   defined between the scheduling point and the next_sync point and the different regions defined 
        *                   by the tasks ti; i2[0::T ] as one unique region defined by the points:
        *                   − init, referencing both scheduling and any entry point to the tasks ti; i2[0::T ].
        *                   − end, referencing both next_sync and any exit point to the tasks ti; i2[0::T ].
        * 2. For each v scalar variable appearing within the task t:
        *      (a) If we cannot determine the type of access (read or write) performed over v either within the task 
        *          or between init and end because the variable appears as a parameter in a function call that we do not
        *          have access to, then v is scoped as UNDEFINED.
        *      (b) If v is not used between init and end, then:
        *              i. If v is only read within the task, then v is scoped as FIRSTPRIVATE.
        *             ii. If v is write within the task, then:
        *                   A. If v is live after end, then v is scoped as SHARED.
        *                   B. If v is dead after end, then:
        *                       − If the first action performed in v is a write, then v is scoped as PRIVATE.
        *                       − If the first action performed in v is a read, then v is scoped as FIRSTPRIVATE.
        *              i. If v is live after end, then v is scoped as SHARED.
        *              ii. If v is dead after end, then:
        *                  A. If the first action performed in v is a write, then v is scoped as PRIVATE.
        *                  B. If the first action performed in v is a read, then v is scoped as FIRSTPRIVATE.
        *      (c) If v is used between init and end, then:
        *              i. If v is only read in both between init and end and within the task, then the v is scoped as FIRSTPRIVATE.
        *              ii. If v is written in either between init and end or within the task, then we look for data race 
        *                  conditions, thus:
        *                  A. If it can occur a data race condition, then v has to be privatized. Sic:
        *                      − If the first action performed in v within the task is a write, then v is scoped as PRIVATE.
        *                      − If the first action performed in v within the task is a read, then v is scoped as FIRSTPRIVATE.
        *                  B. If we can assure that no data race can occur, then v is scoped as SHARED.
        * 3. For each use ai; i2[0::N] (where N is the number of uses) of an array variable a appearing within the task t.
        *      (a) We apply the methodology used for the scalars.
        *      (b) Since OpenMP does not allow different scopes for the subparts of a variable, then we have to mix all the 
        *          results we have get in the previous step. In order to do that we will follow the rules bellow:
        *          i. If the whole array a or all the parts ai have the same scope sc, then a is scoped as sc.
        *          ii. If there are different regions of the array with different scopes, then:
        *              A. If some ai has been scoped as UNDEFINED then a is scoped as UNDEFINED.
        *              B. If at least one ai is FIRSTPRIVATE and all aj; j2[0::N] where j! = i are PRIVATE, then a is scoped as FIRSTPRIVATE.
        *              C. If at least one ai is SHARED and all aj; j2[0::N] where j! = i are PRIVATE or FIRSTPRIVATE, then, 
        *                 fulfilling the sequential consistency rules, a is scoped as SHARED.
        * 4. NOTE: If we cannot determine the init point, then we cannot analyze the task because we do not know which regions 
        *          of code can be executed in parallel with t.
        * 5. NOTE: If we cannot determine the end point, then we can only scope those variables that are local to the function containing t.
        * 
        * Data race conditions can appear when two threads can access to the same memory unit in the same time and at least one 
        * of these accesses is a write. In order to analyze data race conditions in the process of auto-scoping the variables
        * of a task we have to analyze the code appearing in all regions defined between the init and end points described in 
        * the previous section. Any variable v appearing in two different regions where at least one of the accesses is a write and 
        * no one of the two accesses is blocked by either and atomic construct, a critical construct or a lock routine 
        * (omp_init_lock / omp_destroy_lock, omp_set_lock / omp_unset_lock), can trigger a data race situation.
        */
        char StaticAnalysis::compute_auto_scoping(Node* task)
        {
            ObjectList<Node*> end_point = task->get_children();
            if (end_point.empty())
            {
                std::cerr << "Task \"" << task->get_graph_label().prettyprint() << "\" auto-scoping cannot be computed "
                          << "because there is no synchronization point defined at the end of the task" << std::endl;
                return '0';
            }
            else if (end_point.size() > 1)
            {    
                internal_error("The end point of a task should be one unique node representing a 'taskwait', a 'barrier' or the 'exit' of a graph. "\
                               "But task '%s' has more than one exit", task->get_graph_label().prettyprint().c_str());
            }
            
            if (end_point[0]->get_type() == BASIC_EXIT_NODE)
            {   // If it is an 'exit' we don't know anything (FIXME: we can know things)
                //FIXME: Can this happen???
            }
            else
            {
                // All variables with an undefined behaviour cannot be scoped
                ext_sym_set undef_beh = task->get_undefined_behaviour_vars();
                for (ext_sym_set::iterator it = undef_beh.begin(); it != undef_beh.end(); ++it)
                {
                    Scope sc(task->get_scope());
                    if ( !it->get_symbol().get_scope().scope_is_enclosed_by(sc) && it->get_symbol().get_scope() != sc )
                        _undefined_vars.insert(*it);
                }
                
                // Compute the regions of code that can be simultaneous with the current tasks
                _next_sync = end_point[0];
                ObjectList<Node*> task_parents = task->get_parents();
                for (ObjectList<Node*>::iterator it = task_parents.begin(); it != task_parents.end(); ++it)
                    compute_simultaneous_tasks(*it);
                for (ObjectList<Node*>::iterator it = task_parents.begin(); it != task_parents.end(); ++it)
                    ExtensibleGraph::clear_visits_backwards(*it);
                // If the current task is created in a loop, the it is simultaneous with itself
                bool is_in_loop = task_is_in_loop(task);
                    
                // Scope non-undefined behaviour variables
                ext_sym_set scoped_vars;
                compute_auto_scoping_rec(task, task->get_graph_entry_node(), is_in_loop, scoped_vars);
                ExtensibleGraph::clear_visits(task->get_graph_entry_node());
            }
            
            task->print_auto_scoping();
            
            return '1';
        }
        
        bool StaticAnalysis::task_is_in_loop(Node* current)
        {
            bool res = false;
            
            ObjectList<Edge*> entries = current->get_entry_edges();
            for(ObjectList<Edge*>::iterator it = entries.begin(); it != entries.end(); ++it)
            {
                if ((*it)->is_back_edge())
                    return true;
            }
            
            ObjectList<Node*> parents = current->get_parents();
            for(ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
            {
                res = res || task_is_in_loop(*it);
            }
            
            return res;
        }
    
        Node* StaticAnalysis::compute_simultaneous_tasks(Node* current)
        {
            if (!current->is_visited())
            {
                current->set_visited(true);
                
                Node_type ntype = current->get_type();
                if (ntype == BASIC_ENTRY_NODE)
                {
                    return NULL;
                }
                else if (ntype == BARRIER_NODE)
                {
                    return current;
                }
                else if (ntype == TASKWAIT_NODE)
                {   // Here we have to look for nested tasks in tasks which are above this node
                    // TODO
                }
                else
                {
                    Node* last_sync;
                    
                    if (ntype == GRAPH_NODE)
                    {
                        Graph_type gtype = current->get_graph_type();
                        if (gtype == TASK)
                        {   // Possible inner tasks inside 'current' are added with the addition of this node
                            _simultaneous_tasks.append(current);
                        }
                        else
                        {   // Analyse recursively the inner nodes of the graph
                            last_sync = compute_simultaneous_tasks(current->get_graph_exit_node());
                            if (last_sync != NULL)
                                return last_sync;
                        }
                    }

                    // Look for more tasks in the parents
                    ObjectList<Node*> parents = current->get_parents();
                    for (ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
                    {
                        // Since a barrier directive may not be used in place of the statement following an if, while, do, switch, or label, then
                        // If one of the parents reach a barrier, the rest will reach the same barrier
                        last_sync = compute_simultaneous_tasks(*it);
                    }
                    return last_sync;
                }
            }
            
            return NULL;
        }
    
        void StaticAnalysis::compute_auto_scoping_rec(Node* task, Node* current, bool is_in_loop, ext_sym_set& scoped_vars)
        {
            if (!current->is_visited())
            {
                current->set_visited(true);
                
                Node_type ntype = current->get_type();
                if (ntype == GRAPH_NODE)
                {
                    compute_auto_scoping_rec(task, current->get_graph_entry_node(), is_in_loop, scoped_vars);
                }
                else if (current->has_key(_NODE_STMTS))
                {
                    ext_sym_set undef = task->get_undefined_behaviour_vars();
                    Scope sc(task->get_scope());
                    
                    ext_sym_set ue = current->get_ue_vars();
                    for (ext_sym_set::iterator it = ue.begin(); it != ue.end(); ++it)
                    {  
                        Symbol s(it->get_symbol());
                        if (s.is_valid() && !s.get_scope().scope_is_enclosed_by(sc)/* && it->get_symbol().get_scope() != sc*/
                            && !ext_sym_set_contains_englobing_nodecl(*it, undef))
                            scope_variable(task, current, '1', *it, is_in_loop, scoped_vars);
                    }
                    
                    ext_sym_set killed = current->get_killed_vars();
                    for (ext_sym_set::iterator it = killed.begin(); it != killed.end(); ++it)
                    {  
                        Symbol s(it->get_symbol());
                        if (s.is_valid() && !s.get_scope().scope_is_enclosed_by(sc)/* && it->get_symbol().get_scope() != sc*/
                            && !ext_sym_set_contains_englobing_nodecl(*it, undef))
                            scope_variable(task, current, '0', *it, is_in_loop, scoped_vars);
                    }    
                }
                
                ObjectList<Node*> children = current->get_children();
                for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    compute_auto_scoping_rec(task, *it, is_in_loop, scoped_vars);
                }
            }
        }

        void StaticAnalysis::scope_variable(Node* task, Node* ei_node, char usage, ExtensibleSymbol ei, bool is_in_loop, ext_sym_set& scoped_vars)
        {
            if (!ext_sym_set_contains_englobing_nodecl(ei, scoped_vars))
            {   // The expression is not a symbol local from the task
                scoped_vars.insert(ei);
                ObjectList<Node*> uses_out = var_uses_out_task(task, ei);
//                 ObjectList<Node*> uses_in = var_uses_in_task(task, ei);
//                 ExtensibleGraph::clear_visits_aux(task);
               
                if (uses_out.empty())
                {
                    bool scoped = false;
                    if (is_in_loop)
                    {
                        scoped = scope_ie_in_iterated_task(task, ei_node, ei_node, usage, ei);
                        ExtensibleGraph::clear_visits_aux(ei_node);
                    }
                    
                    if (!scoped)
                    {   // No need to privatize the variable because of a race condition with the same task
                        ext_sym_set task_killed = task->get_killed_vars();
                        if ( !ext_sym_set_contains_englobing_nodecl(ei, task_killed)
                            && !ext_sym_set_contains_englobed_nodecl(ei, task_killed) )
                        {
                            _firstprivate_vars.append(ei);
                        }
                        else
                        {
                            ext_sym_set task_live_out = task->get_live_out_vars();
                            if ( ext_sym_set_contains_englobing_nodecl(ei, task_live_out)
                                || ext_sym_set_contains_englobed_nodecl(ei, task_live_out) 
                                || (is_in_loop && task_reads_and_writes(task, ei)) )
                            {
                                _shared_vars.append(ei);
                            }
                            else
                            {
                                ext_sym_set task_live_in = task->get_live_in_vars();
                                if ( ext_sym_set_contains_englobing_nodecl(ei, task_live_in)
                                    || ext_sym_set_contains_englobed_nodecl(ei, task_live_out) )
                                {
                                    _firstprivate_vars.append(ei);
                                }
                                else
                                {
                                    _private_vars.append(ei);
                                }
                            }
                        }
                    }
                }
                else
                {
                    if (task_and_simultaneous_only_read(task, ei))
                    {
                        _firstprivate_vars.append(ei);
                    }
                    else
                    {   // look for data race  conditions:
                        //   A. If it can occur a data race condition, then v has to be privatized. Sic:
                        //      − If the first action performed in v within the task is a write, then v is scoped as PRIVATE.
                        //      − If the first action performed in v within the task is a read, then v is scoped as FIRSTPRIVATE.
                        //   B. If we can assure that no data race can occur, then v is scoped as SHARED.
                        // TODO
                        std::cerr << "Variable within task is written, we should look for data race conditions here!!" << std::endl;
                        task->set_race_var(ei);
                    }
                }
            }
        }

        bool StaticAnalysis::task_and_simultaneous_only_read(Node* task, ExtensibleSymbol ei)
        {
            ext_sym_set task_killed = task->get_killed_vars();
            if (ext_sym_set_contains_englobing_nodecl(ei, task_killed) || ext_sym_set_contains_englobed_nodecl(ei, task_killed))
            {   // Variable is defined within the task
                return false;
            }
            else
            {
                // Look simultaneous tasks
                for (ObjectList<Node*>::iterator it = _simultaneous_tasks.begin(); it != _simultaneous_tasks.end(); ++it)
                {
                    task_killed = (*it)->get_killed_vars();
                    if (ext_sym_set_contains_englobing_nodecl(ei, task_killed) || ext_sym_set_contains_englobed_nodecl(ei, task_killed))
                    {   // Variable is defined within the task
                        return false;
                    }
                }
                
                // TODO look simultaneous code in encountering thread
                
                
            }
            return true;
        }

        static bool is_blocked(Node* current)
        {
            if (current != NULL)
            {    
                if (current->get_graph_type() == OMP_PRAGMA)
                {
                    std::string label = current->get_graph_label().get_text();
                    if (label == "atomic" || label == "critical")
                        return true;
                }
                return is_blocked(current->get_outer_node());
            }

            return false;
        }

        bool StaticAnalysis::task_reads_and_writes(Node* task, ExtensibleSymbol ei)
        {
            bool read = false, write = false;
            
            Node* entry = task->get_graph_entry_node();
            task_reads_and_writes_rec(task, entry, ei, read, write);
            ExtensibleGraph::clear_visits_aux(entry);
            
            return read && write;
        }

        void StaticAnalysis::task_reads_and_writes_rec(Node* task, Node* current, ExtensibleSymbol ei, bool& read, bool& write)
        {
            if (!current->is_visited_aux())
            {
                current->set_visited_aux(true);
                
                if (current->get_id() != task->get_graph_exit_node()->get_id())
                {
                    if (ext_sym_set_contains_englobing_nodecl(ei, current->get_ue_vars()))
                        read = true;
                    if (ext_sym_set_contains_englobing_nodecl(ei, current->get_killed_vars()))
                        write = true;
                }
                if (!read || !write)
                {
                    ObjectList<Node*> children = current->get_children();
                    for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                    {
                        task_reads_and_writes_rec(task, *it, ei, read, write);
                    }
                }
            }
        }

        bool StaticAnalysis::scope_ie_in_iterated_task(Node* task, Node* current, Node* ei_node, char usage, ExtensibleSymbol ei)
        {
            if (!current->is_visited_aux())
            {
                current->set_visited_aux(true);
                
                if (current->get_id() != task->get_graph_exit_node()->get_id())
                {
                    Node_type ntype = current->get_type();
                    if (ntype == GRAPH_NODE)
                    {
                        scope_ie_in_iterated_task(task, current->get_graph_entry_node(), ei_node, usage, ei);
                    }
                    else if (current->has_key(_NODE_STMTS))
                    {
                        ext_sym_set undef = task->get_undefined_behaviour_vars();
                        
                        if (usage == '0')
                        {
                            ext_sym_set ue = current->get_ue_vars();
                            ext_sym_set killed = current->get_killed_vars();
                            if (ext_sym_set_contains_englobing_nodecl(ei, ue) || ext_sym_set_contains_englobing_nodecl(ei, killed))
                            {
                                if (!is_blocked(ei_node->get_outer_node()) && !is_blocked(current->get_outer_node()))
                                {   // We privatize the variable to avoid a race condition
                                    _private_vars.append(ei);
                                    return true;
                                }
                            } 
                        }
                        else
                        {   // usage == '1'
                            ext_sym_set killed = current->get_killed_vars();
                            for (ext_sym_set::iterator it = killed.begin(); it != killed.end(); ++it)
                                if (!is_blocked(ei_node->get_outer_node()) && !is_blocked(current->get_outer_node()))
                                {   // We privatize the variable to avoid a race condition
                                    _firstprivate_vars.append(ei);
                                    return true;
                                }
                        }
                    }
                   
                    ObjectList<Node*> children = current->get_children();
                    for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                    {
                        scope_ie_in_iterated_task(task, *it, ei_node, usage, ei);
                    }
                }
            }
           
            return false;
        }

        ObjectList<Node*> StaticAnalysis::var_uses_out_task(Node* task, ExtensibleSymbol ei)
        {
            ObjectList<Node*> uses;
           
            // Get the uses in the simultaneous tasks
            for (ObjectList<Node*>::iterator it = _simultaneous_tasks.begin(); it != _simultaneous_tasks.end(); ++it)
            {
                uses.append(var_uses_in_task(*it, ei));
                ExtensibleGraph::clear_visits_aux(*it);
            }
            
            // Get the uses in the encountering thread from the task scheduling point till the task synchronization point
            ObjectList<Node*> parents = task->get_parents();
            // NOTE: We do not fusion these loops because parents will converge in some point and
            // keeping them separated we do not analyse the same part of the graph two times
            for (ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
            {    
                ObjectList<Node*> it_children = (*it)->get_children();
                for (ObjectList<Node*>::iterator itc = it_children.begin(); itc != it_children.end(); ++itc)
                {
                    uses.append(uses_from_node_to_node(*itc, _next_sync, ei, task));
                }
            }
            for (ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
            {    
                ExtensibleGraph::clear_visits_avoiding_branch(*it, task);
            }            
            
            return uses;
        }
        
        static ObjectList<Node*> uses_from_node_to_node(Node* current, Node* end, ExtensibleSymbol ei, Node* task)
        {
            ObjectList<Node*> uses;
            if ( (current->get_id()) != task->get_id() && (!current->is_visited()) )
            {
                current->set_visited(true);
                if (current->get_id() != end->get_id())
                {
                    ext_sym_set ue_vars = current->get_ue_vars();
                    ext_sym_set killed_vars = current->get_killed_vars();
                    ext_sym_set undef_vars = current->get_undefined_behaviour_vars();
                    
                    if ( ( ext_sym_set_contains_englobing_nodecl(ei, ue_vars) || ext_sym_set_contains_englobed_nodecl(ei, ue_vars) 
                        || ext_sym_set_contains_englobing_nodecl(ei, killed_vars) || ext_sym_set_contains_englobed_nodecl(ei, killed_vars) )
                        && ( !ext_sym_set_contains_englobing_nodecl(ei, undef_vars) && !ext_sym_set_contains_englobed_nodecl(ei, undef_vars) ) )
                    {
                        uses.append(current);
                    }
                    
                    ObjectList<Node*> children = current->get_children();
                    for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                    {
                        uses.append(uses_from_node_to_node(*it, end, ei, task));
                    }
                }
            }

            return uses;
        }
        
        ObjectList<Node*> StaticAnalysis::var_uses_in_task(Node* current, ExtensibleSymbol ei)
        {
            ObjectList<Node*> uses;
            
            if (!current->is_visited_aux())
            {
                current->set_visited_aux(true);
                
                ext_sym_set ue_vars = current->get_ue_vars();
                ext_sym_set killed_vars = current->get_killed_vars();
                ext_sym_set undef_vars = current->get_undefined_behaviour_vars();
                
                if ( ( ext_sym_set_contains_englobing_nodecl(ei, ue_vars) || ext_sym_set_contains_englobed_nodecl(ei, ue_vars) 
                    || ext_sym_set_contains_englobing_nodecl(ei, killed_vars) || ext_sym_set_contains_englobed_nodecl(ei, killed_vars) )
                    && ( !ext_sym_set_contains_englobing_nodecl(ei, undef_vars) && !ext_sym_set_contains_englobed_nodecl(ei, undef_vars) ) )
                {
                    uses.append(current);
                }
                
                ObjectList<Node*> children = current->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    uses.append(var_uses_in_task(*it, ei));
                }
            }
            
            return uses;            
        }

        bool StaticAnalysis::race_condition(Node* task, ExtensibleSymbol ei)
        {
            bool res = false;
            
            // TODO
            
            return res;
        }

        // *** CFG_VISITOR *** //
        
        static bool nodecl_is_base_of_symbol(Symbol s, Nodecl::NodeclBase n)
        {
            if (n.is<Nodecl::Symbol>())
            {
                if (n.get_symbol() == s)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            else
            {
                if (n.is<Nodecl::Derreference>())
                {
                    Nodecl::Derreference n_ = n.as<Nodecl::Derreference>();
                    return nodecl_is_base_of_symbol(s, n_.get_rhs());
                }
                else if (n.is<Nodecl::ClassMemberAccess>())
                {
                    Nodecl::ClassMemberAccess n_ = n.as<Nodecl::ClassMemberAccess>();
                    return nodecl_is_base_of_symbol(s, n_.get_lhs());
                }
                else if (n.is<Nodecl::ArraySubscript>())
                {
                    Nodecl::ArraySubscript n_ = n.as<Nodecl::ArraySubscript>();
                    return nodecl_is_base_of_symbol(s, n_.get_subscripted());
                }
                else
                {
                    internal_error("Unexpected type of node '%s' while looking for base symbol in an extensible symbol", 
                                ast_print_node_type(n.get_kind()));
                }
            }
        }
        
        static bool nodecl_is_base_of_symbol(ObjectList<Symbol> s_l, Nodecl::NodeclBase n)
        {
            for (ObjectList<Symbol>::iterator it = s_l.begin(); it != s_l.end(); ++it)
            {
                if (nodecl_is_base_of_symbol(*it, n))
                {
                    return true;
                }
            }
            return false;
        }
        
        static Symbol get_symbol_of_lvalue(Nodecl::NodeclBase n)
        {
            while (!n.is<Nodecl::Symbol>())
            {
                if (n.is<Nodecl::ClassMemberAccess>())
                {
                    Nodecl::ClassMemberAccess n_ = n.as<Nodecl::ClassMemberAccess>();
                    get_symbol_of_lvalue(n_.get_member());
                }
                else if (n.is<Nodecl::ArraySubscript>())
                {
                    Nodecl::ArraySubscript n_ = n.as<Nodecl::ArraySubscript>();
                    get_symbol_of_lvalue(n_.get_subscripted());
                }
                else if (n.is<Nodecl::FunctionCall>())
                {
                    return Symbol();
                }
                else
                {
                    internal_error("Unexpected type of node '%s' while looking for the symbol in an lvalue Nodecl",
                                    ast_print_node_type(n.get_kind()));
                }
            }
            
        }

        static bool nodecl_is_constant_value(Nodecl::NodeclBase n)
        {
            if (n.is<Nodecl::IntegerLiteral>() || n.is<Nodecl::FloatingLiteral>() 
                || n.is<Nodecl::ComplexLiteral>() || n.is<Nodecl::BooleanLiteral>()
                || n.is<Nodecl::StringLiteral>())
            {
                return true;
            }
            return false;
        }

        /*!
         * This method looks for the argument corresponding to a parameter which matches with a given nodecl
         * \param n   Nodecl being used/defined which has been stored in an ExtensibleSymbol                  -> a.b.c
         * \param s   Symbol containing a parameter of the function where de nodecl has being used/defined    -> A& a
         * \param arg Nodecl containing the argument corresponding to de parameter                        -> a' of type A&
         * \return copy of the argument 'arg' corresponding to the parameter 'n' when this parameter is the symbol 's'
         */
        static Nodecl::NodeclBase match_nodecl_with_symbol(Nodecl::NodeclBase n, Symbol s, Nodecl::NodeclBase arg)
        {
            if (!nodecl_is_constant_value(arg))
            {
                Type arg_type = arg.get_type();
                const char * arg_file = arg.get_filename().c_str();
                int arg_line = arg.get_line();
                
                if (n.is<Nodecl::IntegerLiteral>() || n.is<Nodecl::FloatingLiteral>() || n.is<Nodecl::ComplexLiteral>()
                    || n.is<Nodecl::StringLiteral>() || n.is<Nodecl::BooleanLiteral>())
                {}   // When the nodecl is a literal, no symbol is involved
                else if (n.is<Nodecl::Symbol>() || n.is<Nodecl::PointerToMember>())
                {
                    if (n.get_symbol() == s)
                    {
                        return arg.copy();
                    }
                }
                else if (n.is<Nodecl::ClassMemberAccess>())
                {
                    Nodecl::ClassMemberAccess aux = n.as<Nodecl::ClassMemberAccess>();
                    Nodecl::NodeclBase lhs = match_nodecl_with_symbol(aux.get_lhs(), s, arg);
                    if (!lhs.is_null())
                    {
                        return Nodecl::ClassMemberAccess::make(lhs, aux.get_member(), arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::ArraySubscript>())
                {
                    Nodecl::ArraySubscript aux = n.as<Nodecl::ArraySubscript>();
                    Nodecl::NodeclBase subscripted = match_nodecl_with_symbol(aux.get_subscripted(), s, arg);
                    if (!subscripted.is_null())
                    {
                        return Nodecl::ArraySubscript::make(subscripted, aux.get_subscripts(), arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Reference>())
                {
                    Nodecl::Reference aux = n.as<Nodecl::Reference>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Reference::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Derreference>())
                {
                    Nodecl::Derreference aux = n.as<Nodecl::Derreference>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Derreference::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Conversion>())
                {
                    Nodecl::Conversion aux = n.as<Nodecl::Conversion>();
                    Nodecl::NodeclBase nest = match_nodecl_with_symbol(aux.get_nest(), s, arg);
                    if (!nest.is_null())
                    {
                        return Nodecl::Conversion::make(nest, arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Cast>())
                {
                    Nodecl::Cast aux = n.as<Nodecl::Cast>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Conversion::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                // While checking parameters, we can have many types of "Extensible symbols"
                else if (n.is<Nodecl::Add>())
                {
                    Nodecl::Add aux = n.as<Nodecl::Add>();
                    Nodecl::NodeclBase lhs = match_nodecl_with_symbol(aux.get_lhs(), s, arg);
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!lhs.is_null() && !rhs.is_null())
                    {
                        internal_error("Using two parameters in the same argument expression. Not yet implemented\n", 0);
                    }
                    else if (!lhs.is_null())
                    {
                        return lhs;
                    }
                    else if (!rhs.is_null())
                    {
                        return rhs;
                    }
                }
                else if (n.is<Nodecl::Minus>())
                {
                    Nodecl::Minus aux = n.as<Nodecl::Minus>();
                    Nodecl::NodeclBase lhs = match_nodecl_with_symbol(aux.get_lhs(), s, arg);
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!lhs.is_null() || !rhs.is_null())
                    {
                        return Nodecl::Minus::make(lhs, rhs, arg_type, arg_file, arg_line);
                    }                
                }
                else if (n.is<Nodecl::Preincrement>())
                {
                    Nodecl::Preincrement aux = n.as<Nodecl::Preincrement>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Preincrement::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Postincrement>())
                {
                    Nodecl::Postincrement aux = n.as<Nodecl::Postincrement>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Postincrement::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Predecrement>())
                {
                    Nodecl::Predecrement aux = n.as<Nodecl::Predecrement>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Predecrement::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                else if (n.is<Nodecl::Postdecrement>())
                {
                    Nodecl::Postdecrement aux = n.as<Nodecl::Postdecrement>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, arg);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Postdecrement::make(rhs, arg_type, arg_file, arg_line);
                    }
                }
                else
                {
                    internal_error("Node type '%s' in node '%s' not yet implemented while parsing an Extensible symbol matching",
                                   ast_print_node_type(n.get_kind()), n.prettyprint().c_str());
                }
            }

            return Nodecl::NodeclBase::null();
        }
    
        /*!
        * This method searches a nodecl corresponding to a parameter when the corresponding argument is not a constant value
        * \param n                Nodecl we are looking for
        * \param params           List of parameters
        * \param args             List of arguments
        * \param matching_symbol  Symbol in the parameter list which matches 
        * \return Copy of the symbol, if matched, otherwise, null nodecl
        */
        static Nodecl::NodeclBase match_nodecl_in_symbol_l(Nodecl::NodeclBase n, ObjectList<Symbol> params, 
                                                           ObjectList<Nodecl::NodeclBase> args, Symbol& matching_symbol)
        {
            ObjectList<Symbol>::iterator itp = params.begin();
            ObjectList<Nodecl::NodeclBase>::iterator ita = args.begin();
            Nodecl::NodeclBase actual_nodecl;
            for (; (itp != params.end()) && (ita != args.end()); ++itp, ++ita)
            {
                actual_nodecl = match_nodecl_with_symbol(n, *itp, *ita);
                if (!actual_nodecl.is_null())
                {
                    matching_symbol = *itp;
                    break;
                }
            }
            
            return actual_nodecl;
        }
        
        //! Set the variable in #n to UE or KILLED list
        static void set_usage_to_symbol_in_node(Node* node, struct var_usage_t* n, ExtensibleGraph* called_func_graph)
        {
            char usage = n->get_usage();
            if (usage == '0')
            {
                ExtensibleSymbol ue_var(n->get_nodecl());
                node->set_ue_var(ue_var);
            }
            else if (usage == '1')
            {
                ExtensibleSymbol killed_var(n->get_nodecl());
                node->set_killed_var(killed_var);
            }
            else if (usage == '2')
            {
                ExtensibleSymbol ue_killed_var(n->get_nodecl());
                node->set_ue_var(ue_killed_var);
                node->set_killed_var(ue_killed_var);
            }
            else
            {
                internal_error("Unexpected usage value '%c' while computing global variable usage in function ", 
                            usage, called_func_graph->get_function_symbol().get_name().c_str());
            }
        }
        
        bool CfgVisitor::func_has_cyclic_calls_rec(Symbol reach_func, Symbol stop_func, ExtensibleGraph * graph)
        {
            ObjectList<Symbol> called_syms = graph->get_function_calls();
           
            for (ObjectList<Symbol>::iterator it = called_syms.begin(); it != called_syms.end(); ++it)
            {
                ExtensibleGraph* called_func_graph = find_function_for_ipa(*it, _cfgs);
                if (called_func_graph != NULL)
                {
                    if (called_func_graph->get_function_symbol() == reach_func)
                    {
                        return true;
                    }
                    else
                    {
                        if (stop_func != *it)
                            if (func_has_cyclic_calls(reach_func, called_func_graph))
                                return true;
                    }
                }
            }
            
            return false;
        }
        
        bool CfgVisitor::func_has_cyclic_calls(Symbol reach_func, ExtensibleGraph * graph)
        {
            return func_has_cyclic_calls_rec(reach_func, graph->get_function_symbol(), graph);
        }

        void CfgVisitor::set_live_initial_information(Node* node)
        {
            if (node->get_type() == BASIC_FUNCTION_CALL_NODE)
            {
                ExtensibleGraph* called_func_graph = find_function_for_ipa(node->get_function_node_symbol(), _cfgs);
                Nodecl::NodeclBase func_call = /*Function call*/ node->get_statements()[0];
                if (called_func_graph != NULL)
                {
                    // Create a map between the parameters of the called function and the current arguments of the call
                    std::map<Symbol, Nodecl::NodeclBase> ref_params_to_args = map_reference_params_to_args(func_call, called_func_graph);
                    
                    Symbol function_sym = called_func_graph->get_function_symbol();
                    if (func_has_cyclic_calls(function_sym, called_func_graph))
                    {   // Recursive analysis
                        ext_sym_set ue_vars = node->get_ue_vars();
                        ext_sym_set killed_vars = node->get_killed_vars();
                        ext_sym_set undef_vars = node->get_undefined_behaviour_vars();
                        
                        // Analyse reference parameters and global variables
                        ObjectList<var_usage_t*> glob_vars = called_func_graph->get_global_variables();
                        if (!glob_vars.empty() || !ref_params_to_args.empty())
                        {
                            // Compute liveness for global variables and parameters
                            ObjectList<Symbol> ref_params = get_reference_params(called_func_graph);
                            CfgIPAVisitor ipa_visitor(called_func_graph, _cfgs, glob_vars, ref_params, ref_params_to_args);
                            ipa_visitor.compute_usage();
                            
                            // Propagate this information to the current graph analysis
                            ObjectList<struct var_usage_t*> ipa_usage = ipa_visitor.get_usage();
                            for (ObjectList<struct var_usage_t*>::iterator it = ipa_usage.begin(); it != ipa_usage.end(); ++it)
                            {
                                char usage = (*it)->get_usage();
                                Nodecl::NodeclBase s = (*it)->get_nodecl();
                                if (usage == '0')
                                {
                                    killed_vars.insert(ExtensibleSymbol(s));
                                }
                                else if (usage == '1')
                                {
                                    ue_vars.insert(ExtensibleSymbol(s));
                                }
                                else if (usage == '2')
                                {
                                    killed_vars.insert(ExtensibleSymbol(s));
                                    ue_vars.insert(ExtensibleSymbol(s));
                                }
                                else if (usage == '3')
                                {
                                    undef_vars.insert(ExtensibleSymbol(s));
                                }
                                else
                                {
                                    internal_error("Undefined usage %s for symbol %s\n", usage, s.prettyprint().c_str())
                                }
                            }
                          
                            for (std::map<Symbol, Nodecl::NodeclBase>::iterator it = ref_params_to_args.begin(); it != ref_params_to_args.end(); ++it)
                            {
                                Nodecl::NodeclBase s = it->second;
                                
                                if (!s.is<Nodecl::FunctionCall>() && !s.is<Nodecl::VirtualFunctionCall>())
                                {
                                    ObjectList<Nodecl::NodeclBase> arg_ei;
                                    if (s.is<Nodecl::Symbol>() || s.is<Nodecl::Cast>() || s.is<Nodecl::ClassMemberAccess>()
                                        || s.is<Nodecl::Reference>() || s.is<Nodecl::Derreference>() 
                                        || s.is<Nodecl::ArraySubscript>()) 
                                    {
                                        arg_ei.append(s);
                                    }
                                    else
                                    {
                                        ExtensibleSymbolVisitor sv;
                                        sv.walk(s);
                                        ObjectList<Nodecl::NodeclBase> arg = sv.get_extensible_symbols();
                                        for (ObjectList<Nodecl::NodeclBase>::iterator its = arg.begin(); its != arg.end(); ++its)
                                        {
                                            arg_ei.insert(*its);
                                        }
                                    }
                                    
                                    for (ObjectList<Nodecl::NodeclBase>::iterator its = arg_ei.begin(); its != arg_ei.end(); ++its)
                                    {
                                        if( !usage_list_contains_englobing_nodecl(*its, ipa_usage) 
                                            && !usage_list_contains_englobed_nodecl(*its, ipa_usage) )
                                        {   // All arguments that are not already in the list, have to be set as UE
                                            ue_vars.insert(ExtensibleSymbol(*its));
                                        }
                                        else
                                        {   // If some list contains a part of the argument, then the argument have an undefined 
                                            if (ext_sym_set_contains_englobing_nodecl(*its, killed_vars) 
                                                && ext_sym_set_contains_nodecl(*its, killed_vars))
                                            {
                                                delete_englobing_var_from_list(*its, killed_vars);
                                                undef_vars.insert(ExtensibleSymbol(*its));
                                            }
                                            else if (ext_sym_set_contains_englobed_nodecl(*its, killed_vars) 
                                                && ext_sym_set_contains_nodecl(*its, killed_vars))
                                            {
                                                delete_englobed_var_from_list(*its, killed_vars);
                                                undef_vars.insert(ExtensibleSymbol(*its));                                        
                                            }
                                            else if (ext_sym_set_contains_englobing_nodecl(*its, undef_vars) 
                                                && ext_sym_set_contains_nodecl(*its, undef_vars))
                                            {
                                                delete_englobing_var_from_list(*its, undef_vars);
                                                undef_vars.insert(ExtensibleSymbol(*its));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        
                        // Non-reference parameters are always used
                        ObjectList<Nodecl::NodeclBase> non_ref_args = get_non_reference_args(func_call, called_func_graph);
                        for (ObjectList<Nodecl::NodeclBase>::iterator it = non_ref_args.begin(); it != non_ref_args.end(); ++it)
                        {
                            if (it->is<Nodecl::Symbol>() || it->is<Nodecl::Cast>() || it->is<Nodecl::ClassMemberAccess>()
                                || it->is<Nodecl::Reference>() || it->is<Nodecl::Derreference>() 
                                || it->is<Nodecl::ArraySubscript>()) 
                            {
                                ue_vars.insert(ExtensibleSymbol(*it));
                            }
                            else if (it->is<Nodecl::FunctionCall>() || it->is<Nodecl::VirtualFunctionCall>())
                            {
                                // FIXME Arguments are also used!
                            }
                            else
                            {
                                ExtensibleSymbolVisitor sv;
                                sv.walk(*it);
                                ObjectList<Nodecl::NodeclBase> arg_syms = sv.get_extensible_symbols();
                                for (ObjectList<Nodecl::NodeclBase>::iterator its = arg_syms.begin(); its != arg_syms.end(); ++its)
                                    ue_vars.insert(ExtensibleSymbol(*its));
                            }
                        }
                        
                        node->set_ue_var(ue_vars);
                        node->set_killed_var(killed_vars);
                        node->set_undefined_behaviour_var(undef_vars);
                    }
                    else
                    {
                        if (!_visited_functions.contains(called_func_graph->get_function_symbol()))
                        {
                            _visited_functions.insert(called_func_graph->get_function_symbol());
                            
                            // Compute the Use-Def information of the called function, if necessary
                            if (called_func_graph->has_use_def_computed() == '0')
                            {
                                ExtensibleGraph* actual_cfg = _actual_cfg;
                                _actual_cfg = called_func_graph;
                               
                                compute_use_def_chains(called_func_graph->get_graph());
                                if (called_func_graph->has_use_def_computed() == '2')
                                    _actual_cfg->set_use_def_computed('2');
                                else
                                    called_func_graph->set_use_def_computed('1');
                                
                                _actual_cfg = actual_cfg;
                            }
                            
                            // Filter map keeping those arguments that are constants for "constant propagation" in USE-DEF info
                            std::map<Symbol, Nodecl::NodeclBase> const_args;
                            for(std::map<Symbol, Nodecl::NodeclBase>::iterator it = ref_params_to_args.begin(); it != ref_params_to_args.end(); ++it)
                            {
                                if (it->second.is_constant())
                                {
                                    const_args[it->first] = it->second;
                                }
                            }
                            
                            // Propagate use-def chains of the function call to the current graph
                            ObjectList<Symbol> ref_params;
                            ObjectList<Nodecl::NodeclBase> ref_args = get_reference_params_and_args(func_call, called_func_graph, ref_params);

                            ext_sym_set called_func_ue_vars = called_func_graph->get_graph()->get_ue_vars();
                            ext_sym_set node_ue_vars;
                            for(ext_sym_set::iterator it = called_func_ue_vars.begin(); it != called_func_ue_vars.end(); ++it)
                            {
                                Symbol s(NULL);
                                Nodecl::NodeclBase ue_arg = match_nodecl_in_symbol_l(it->get_nodecl(), ref_params, ref_args, s);
                                if ( !ue_arg.is_null() )
                                {   // UE variable is a parameter or a part of a parameter
                                    if (ue_arg.is<Nodecl::Symbol>() || ue_arg.is<Nodecl::Cast>() || ue_arg.is<Nodecl::ClassMemberAccess>()
                                        || ue_arg.is<Nodecl::Reference>() || ue_arg.is<Nodecl::Derreference>() 
                                        || ue_arg.is<Nodecl::ArraySubscript>()) 
                                    {
                                        ExtensibleSymbol ei(ue_arg);
                                        ei.propagate_constant_values(const_args);
                                        node_ue_vars.insert(ei);
                                    }
                                    else if (ue_arg.is<Nodecl::FunctionCall>() || ue_arg.is<Nodecl::VirtualFunctionCall>())
                                    {}  // Nothing to do, we don't need to propagate the usage of a temporal value
                                    else
                                    {
                                        ExtensibleSymbolVisitor sv;
                                        sv.walk(ue_arg);
                                        ObjectList<Nodecl::NodeclBase> nodecl_symbols = sv.get_extensible_symbols();
                                        for (ObjectList<Nodecl::NodeclBase>::iterator its = nodecl_symbols.begin(); 
                                                its != nodecl_symbols.end(); ++its)
                                        {
                                            ExtensibleSymbol ei(*its);
                                            ei.propagate_constant_values(const_args);
                                            node_ue_vars.insert(ei);
                                        }
                                    }
                                }
                                else
                                {
                                    Symbol s = it->get_symbol();
                                    if (s.is_valid() && !s.get_scope().scope_is_enclosed_by(function_sym.get_scope()) 
                                             && s.get_scope() != function_sym.get_scope())
                                    {   // UE variable is global
                                        node_ue_vars.insert(*it);
                                    }
                                }
                            }
                            // For all parameters passed by value, the corresponding arguments are used in the current function call node
                            ObjectList<Nodecl::NodeclBase> non_ref_args = get_non_reference_args(func_call, called_func_graph);
                            for (ObjectList<Nodecl::NodeclBase>::iterator it = non_ref_args.begin(); it != non_ref_args.end(); ++it)
                            {
                                if (it->is<Nodecl::Symbol>() || it->is<Nodecl::Cast>() || it->is<Nodecl::ClassMemberAccess>()
                                    || it->is<Nodecl::Reference>() || it->is<Nodecl::Derreference>() 
                                    || it->is<Nodecl::ArraySubscript>()) 
                                {
                                    node_ue_vars.insert(ExtensibleSymbol(*it));
                                }
                                else if (it->is<Nodecl::FunctionCall>() || it->is<Nodecl::VirtualFunctionCall>())
                                {}  // Nothing to do, we don't need to propagate the usage of a temporal value
                                else
                                {
                                    ExtensibleSymbolVisitor sv;
                                    sv.walk(*it);
                                    ObjectList<Nodecl::NodeclBase> nodecl_symbols = sv.get_extensible_symbols();
                                    for (ObjectList<Nodecl::NodeclBase>::iterator its = nodecl_symbols.begin(); 
                                            its != nodecl_symbols.end(); ++its)
                                    {
                                        node_ue_vars.insert(ExtensibleSymbol(*its));
                                    }
                                }
                            }
                        
                            ext_sym_set called_func_killed_vars = called_func_graph->get_graph()->get_killed_vars();
                            ext_sym_set node_killed_vars;
                            for(ext_sym_set::iterator it = called_func_killed_vars.begin(); it != called_func_killed_vars.end(); ++it)
                            {
                                Symbol s(NULL);
                                Nodecl::NodeclBase killed_arg = match_nodecl_in_symbol_l(it->get_nodecl(), ref_params, ref_args, s);
                                if ( !killed_arg.is_null() )
                                {   // KILLED variable is a parameter or a part of a parameter
                                    ExtensibleSymbol ei(killed_arg);
                                    ei.propagate_constant_values(const_args);
                                    // Only reference parameters can be killed, the rest are only used to make a temporary copy
                                    s = ei.get_symbol();
                                    if (s.is_valid() && s.get_type().is_pointer())
                                    {   // FIXME We must go in this case if the Parameter is passed by reference and 
                                        // if the argument is not a temporary value
                                        // decl_context_t param_context =
                                        //        function_sym.get_internal_symbol()->entity_specs.related_symbols[0]->decl_context;
                                        // if (s.get_scope() != Scope(param_context))
                                        node_killed_vars.insert(ei);
                                    }
                                    else
                                    {
                                        node_ue_vars.insert(ei);
                                    }
                                }
                                else
                                {
                                    s = it->get_symbol();
                                    if (s.is_valid() && !it->get_symbol().get_scope().scope_is_enclosed_by(function_sym.get_scope()) 
                                        && it->get_symbol().get_scope() != function_sym.get_scope() )
                                    {   // KILLED variable is global
                                        node_killed_vars.insert(it->get_nodecl());
                                    }
                                }
                            }

                            ext_sym_set called_func_undef_vars = called_func_graph->get_graph()->get_undefined_behaviour_vars();
                            // FIXME We should delete from this list those parameters passed by value and add them to the UE_list
                            
                            node->set_ue_var(node_ue_vars);
                            node->set_killed_var(node_killed_vars);
                            node->set_undefined_behaviour_var(called_func_undef_vars);
                        }
                        else
                        {}  // We already have analysed this function and propagated its Use-Def info to the current graph
                    }
                }
                else
                {
                    std::cerr << "**************** CALL TO FUNC '" << node->get_function_node_symbol().get_name() << "' UNRECOGNIZED" << std::endl;
                    _actual_cfg->set_use_def_computed('2');
                   
                    // Set undefined_behaviour to all parameters in the function call
                    ext_sym_set undef_behaviour_vars, ue_vars;
                    Nodecl::List args = get_func_call_args(node->get_statements()[0]);
                   
                    for(Nodecl::List::iterator it = args.begin(); it != args.end(); ++it)
                    {
                        compute_params_usage_in_unknown_func_call(*it, undef_behaviour_vars, ue_vars);
                    }
                    
                    node->set_ue_var(ue_vars);
                    node->set_undefined_behaviour_var(undef_behaviour_vars);
                    
                    // Se undefined behaviour to the global variables appearing in the current cfg, only if their usage was not 'killed'
                    // FIXME Here we should take into account any global variable, 
                    // but from now we only have access to the global variables appearing in the current cfg
                    ObjectList<struct var_usage_t*> current_global_vars = _actual_cfg->get_global_variables();
                    for (ObjectList<var_usage_t*>::iterator it = current_global_vars.begin(); it != current_global_vars.end(); ++it)
                    {
                        char usage = (*it)->get_usage();
                        if (usage != '0' && usage != '2')
                          (*it)->set_usage('3');
                    }
                }
            }
            else
            {
                ObjectList<Nodecl::NodeclBase> basic_block = node->get_statements();
                for (ObjectList<Nodecl::NodeclBase>::iterator it = basic_block.begin();
                        it != basic_block.end();
                        ++it)
                {
                    CfgAnalysisVisitor cfg_analysis_visitor(node);
                    cfg_analysis_visitor.walk(*it);
                }
            }
        }
        
        static void compute_params_usage_in_unknown_func_call(Nodecl::NodeclBase n, ext_sym_set& undef_behaviour_vars, ext_sym_set& ue_vars)
        {
            if (n.is<Nodecl::Symbol>() || n.is<Nodecl::ClassMemberAccess>())
            {
                ue_vars.insert(ExtensibleSymbol(n));
            }
            else if (n.is<Nodecl::Reference>() || n.is<Nodecl::Derreference>())
            {
                ExtensibleSymbol ei(n);
                undef_behaviour_vars.insert(ei);
            }
            else if (n.is<Nodecl::Conversion>())
            {
                Nodecl::Conversion aux = n.as<Nodecl::Conversion>();
                compute_params_usage_in_unknown_func_call(aux.get_nest(), undef_behaviour_vars, ue_vars);
            }
            else if (n.is<Nodecl::FunctionCall>() || n.is<Nodecl::VirtualFunctionCall>())
            {}  // Nothing to do, we don't need to propagate the usage of a temporal value
            else
            {   // FIXME We can define a variable here passing as argument "(n = 3)"
                ExtensibleSymbolVisitor sv;
                sv.walk(n);
                ObjectList<Nodecl::NodeclBase> syms_in_arg = sv.get_extensible_symbols();
                for (ObjectList<Nodecl::NodeclBase>::iterator it = syms_in_arg.begin(); it != syms_in_arg.end(); ++it)
                    ue_vars.insert(ExtensibleSymbol(*it));
            }
        }
        
        void CfgVisitor::gather_live_initial_information(Node* node)
        {
            if (!node->is_visited())
            {
                node->set_visited(true);

                Node_type ntype = node->get_type();
                if (ntype != BASIC_EXIT_NODE)
                {
                    if (ntype == GRAPH_NODE)
                    {
                        Node* entry = node->get_graph_entry_node();
                        gather_live_initial_information(entry);
                        ExtensibleGraph::clear_visits_in_level(entry, node);
                        node->set_visited(false);
                        node->set_graph_node_use_def();
                        node->set_visited(true);
                    }
                    else if (ntype != BASIC_ENTRY_NODE)
                    {
                        set_live_initial_information(node);
                    }
                        
                    ObjectList<Edge*> exit_edges = node->get_exit_edges();
                    for (ObjectList<Edge*>::iterator it = exit_edges.begin();
                            it != exit_edges.end();
                            ++it)
                    {
                        gather_live_initial_information((*it)->get_target());
                    }
                }
                return;
            }
        }    
        
        void CfgVisitor::compute_use_def_chains(Node* node)
        {
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose)
                std::cerr << "  ==> Graph '" << _actual_cfg->get_name() << "'" << std::endl;
            gather_live_initial_information(node);
            ExtensibleGraph::clear_visits(node);
            _visited_functions.clear();
        }
        
        void CfgVisitor::analyse_loops(Node* node)
        {
            LoopAnalysis loop_analysis;
            loop_analysis.analyse_loops(node);
            
//             DEBUG_CODE()
            {
                std::cerr << "=== INDUCTION VARIABLES INFO AFTER LOOP ANALYSIS ===" << std::endl;
                loop_analysis.print_induction_vars_info();
            }
            
            StaticAnalysis static_analysis(&loop_analysis);
            static_analysis.extend_reaching_definitions_info(node);
            ExtensibleGraph::clear_visits(node);
        }
    }
}
