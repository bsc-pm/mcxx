/*--------------------------------------------------------------------
(C) Copyright 2006-2013 Barcelona Supercomputing Center             *
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


#include "tl-analysis-utils.hpp"
#include "config.h"
#include "tl-node.hpp"
#include "tl-pcfg-visitor.hpp"      // For IPA analysis
#include "tl-use-def.hpp"

namespace TL {
namespace Analysis {

    ObjectList<ExtensibleGraph*> _pcfgs;
    SizeMap _pointer_to_size_map;
    
    // **************************************************************************************************** //
    // **************************** Class implementing use-definition analysis **************************** //

namespace {
    
    /*!This method computes the usage of a node in two different cases:
     * - we are merging the usage of the children nodes with a parent node to set the usage of the enclosing graph node
     * - we are computing the usage of a node: since there may be more than one statement within the same node,
     *                                         we need to take into account the usage computed for the already treated statements
     */
    void propagate_usage_to_ancestors(Utils::ext_sym_set& ue_vars, Utils::ext_sym_set& killed_vars,
                                      Utils::ext_sym_set& undef_vars, Utils::ext_sym_set& used_addresses,
                                      const Utils::ext_sym_set& ue_children, const Utils::ext_sym_set& killed_children, 
                                      const Utils::ext_sym_set& undef_children, const Utils::ext_sym_set& used_addresses_children)
    {
        // Propagate the upwards exposed variables
        Nodecl::NodeclBase ue_previously_killed_subobject, ue_previously_undef_subobject;
        for(Utils::ext_sym_set::iterator it = ue_children.begin(); it != ue_children.end(); ++it)
        {
            Nodecl::NodeclBase n_it = it->get_nodecl();
            // UE vars can only be upwards propagated if the are not already KILLED in the parent
            // or they (or an enclosing nodecl) are not yet in the result set
            if(!Utils::ext_sym_set_contains_enclosing_nodecl(n_it, killed_vars).is_null() ||
                !Utils::ext_sym_set_contains_enclosing_nodecl(n_it, ue_vars).is_null())
                continue;

            ue_previously_killed_subobject = Utils::ext_sym_set_contains_enclosed_nodecl(n_it, killed_vars);
            ue_previously_undef_subobject = Utils::ext_sym_set_contains_enclosed_nodecl(n_it, undef_vars);
            if(ue_previously_killed_subobject.is_null() && ue_previously_undef_subobject.is_null())
            {   // Neither killed nor undef var sets contain the current ue var
                Nodecl::NodeclBase tmp = Utils::ext_sym_set_contains_enclosed_nodecl(n_it, ue_vars);
                if(!tmp.is_null())
                    ue_vars.erase(tmp);   // Delete the enclosed var from the list
                ue_vars.insert(*it);      // Insert the new containing var
            }
            else
            {   // Here a part of the nodecl is KILLED|UNDEF and a part is UE
                Nodecl::NodeclBase non_killed_ue_vars, non_undef_ue_vars;
                if(!ue_previously_killed_subobject.is_null())
                    non_killed_ue_vars = split_var_depending_on_usage(n_it, ue_previously_killed_subobject);
                if(!ue_previously_undef_subobject.is_null())
                {   // A variable marked as UNDEF can still be UE
                    if(Utils::ext_sym_set_contains_nodecl(n_it, undef_vars))
                        non_undef_ue_vars = n_it;
                    else
                        non_undef_ue_vars = split_var_depending_on_usage(n_it, ue_previously_undef_subobject);
                }
                
                if((!ue_previously_killed_subobject.is_null() && non_killed_ue_vars.is_null()) || 
                   (!ue_previously_undef_subobject.is_null() && non_undef_ue_vars.is_null()))
                {   // When the two sets are empty is because the separation has not been possible
                    // Then, we set to undef the whole object and remove the partial object from the corresponding list(s)
                    if(!ue_previously_killed_subobject.is_null())
                        delete_enclosed_var_from_list(ue_previously_killed_subobject, killed_vars);
                    if(!ue_previously_undef_subobject.is_null())
                        delete_enclosed_var_from_list(ue_previously_undef_subobject, undef_vars);
                    undef_vars.insert(n_it);
                }
                else
                {   // new_ue_varsX may be the union of different array ranges. We may want to split the union into separated nodecls
                    if(!non_killed_ue_vars.is_null())
                        ue_vars.insert(Utils::ExtendedSymbol(non_killed_ue_vars));
                    if(!non_undef_ue_vars.is_null())
                        ue_vars.insert(Utils::ExtendedSymbol(non_undef_ue_vars));
                }
            }
        }

        // Propagate the killed variables
        Nodecl::NodeclBase non_killed_var;
        for(Utils::ext_sym_set::iterator it = killed_children.begin(); it != killed_children.end(); ++it)
        {
            Nodecl::NodeclBase n_it = it->get_nodecl();
            if(!Utils::ext_sym_set_contains_enclosing_nodecl(n_it, undef_vars).is_null() ||
               !Utils::ext_sym_set_contains_enclosing_nodecl(n_it, killed_vars).is_null())
                continue;

            non_killed_var = Utils::ext_sym_set_contains_enclosed_nodecl(n_it, undef_vars);
            if(non_killed_var.is_null())
            {   // Undef var set does not contain the current killed var
                Nodecl::NodeclBase already_killed_var = Utils::ext_sym_set_contains_enclosed_nodecl(n_it, killed_vars);
                if(!already_killed_var.is_null())
                    killed_vars.erase(already_killed_var);    // A part of the variable was already killed: remove the subobject
                killed_vars.insert(*it);                      // Insert the whole enclosing object
            }
            else
            {   // Here a part of the nodecl has already been marked as undefined
                Nodecl::NodeclBase new_killed_vars;
                new_killed_vars = split_var_depending_on_usage(n_it, non_killed_var);
                if(new_killed_vars.is_null())
                {   // When the set is null is because the separation has not been possible
                    // Then, we set to undefined the whole object and remove the partial object from the killed list
                    Nodecl::NodeclBase already_killed_var = Utils::ext_sym_set_contains_enclosed_nodecl(n_it, killed_vars);
                    if(!already_killed_var.is_null())
                        killed_vars.erase(already_killed_var);    // A part of the variable was already killed: remove the subobject
                    undef_vars.insert(*it);                       // Insert the whole enclosing object
                }
                else
                {   // Insert the computed killed parts in the corresponding list
                    // new_killed_vars may be the union of different array ranges. We may want to split the union into separated nodecls
                    killed_vars.insert(new_killed_vars);
                }
            }
        }

        // Propagate the undefined behavior variables of the children
        Nodecl::NodeclBase undef_previously_ue_subobject, undef_previously_killed_subobject;
        for(Utils::ext_sym_set::iterator it = undef_children.begin(); it != undef_children.end(); ++it)
        {
            Nodecl::NodeclBase n_it = it->get_nodecl();
            // Variables marked as KILLED cannot be UNDEF
            if(!Utils::ext_sym_set_contains_enclosing_nodecl(n_it, killed_vars).is_null())
                continue;
            
            undef_previously_ue_subobject = Utils::ext_sym_set_contains_enclosed_nodecl(n_it, ue_vars);
            undef_previously_killed_subobject = Utils::ext_sym_set_contains_enclosed_nodecl(n_it, killed_vars);
            if(undef_previously_ue_subobject.is_null() && undef_previously_killed_subobject.is_null())
            {   // Neither ue nor killed var sets contain the current ue var
                if(!Utils::ext_sym_set_contains_enclosing_nodecl(n_it, undef_vars).is_null())
                    continue;
                else
                {
                    Nodecl::NodeclBase tmp = Utils::ext_sym_set_contains_enclosed_nodecl(n_it, undef_vars);
                    if(!tmp.is_null())
                        undef_vars.erase(tmp);        // Delete the enclosed var from the list
                    undef_vars.insert(*it);           // Insert the new containing var
                }
            }
            else
            {
                Nodecl::NodeclBase non_ue_undef_vars, non_killed_undef_vars;
                if(!undef_previously_ue_subobject.is_null())
                {
                    if(Utils::ext_sym_set_contains_nodecl(n_it, ue_vars))
                        non_ue_undef_vars = n_it;
                    else
                        non_ue_undef_vars = split_var_depending_on_usage(n_it, undef_previously_ue_subobject);
                }
                if(!undef_previously_killed_subobject.is_null())
                    non_killed_undef_vars = split_var_depending_on_usage(n_it, undef_previously_killed_subobject);

                if(non_ue_undef_vars.is_null() && non_killed_undef_vars.is_null())
                {   // When the two sets are null is because the separation has not been possible
                    // Then, we set to undef the whole object and remove the partial object from the corresponding list(s)
                    if(!undef_previously_ue_subobject.is_null())
                        delete_enclosed_var_from_list(undef_previously_ue_subobject, ue_vars);
                    if(!undef_previously_killed_subobject.is_null())
                        delete_enclosed_var_from_list(undef_previously_killed_subobject, killed_vars);
                    undef_vars.insert(n_it);
                }
                else
                {   // new_undef_varsX may be the union of different array ranges. We may want to split the union into separated nodecls
                    if(!non_ue_undef_vars.is_null())
                        undef_vars.insert(non_ue_undef_vars);
                    if(!non_killed_undef_vars.is_null())
                        undef_vars.insert(non_killed_undef_vars);
                }
            }
        }
        
        // Propagate the used addresses of the children
        used_addresses.insert(used_addresses_children.begin(), used_addresses_children.end());
    }

}

    UseDef::UseDef(ExtensibleGraph* graph, const ObjectList<ExtensibleGraph*>& pcfgs)
        : _graph(graph), _ipa_modif_vars()
    {
        initialize_ipa_var_usage();
        
        _pointer_to_size_map = graph->get_pointer_n_elements_map();
        _pcfgs = pcfgs;
    }

    void UseDef::initialize_ipa_var_usage()
    {
        // Initialized Reference|Pointer parameters usage to NONE
        Symbol func_sym = _graph->get_function_symbol();
        if(func_sym.is_valid())
        {   // The PCFG contains a FunctionCode
            ObjectList<TL::Symbol> params = func_sym.get_function_parameters();
            for(ObjectList<TL::Symbol>::iterator it = params.begin(); it != params.end(); ++it)
            {
                Type param_type = it->get_type();
                if(param_type.is_any_reference())
                {
                    Nodecl::Symbol s = it->make_nodecl(/*set_ref_type*/true);
                    _ipa_modif_vars[s] = Utils::UsageKind::NONE;
                }
                else if(param_type.is_pointer())
                {
                    Nodecl::Symbol s = it->make_nodecl(/*set_ref_type*/true);
                    Nodecl::Dereference n = Nodecl::Dereference::make(s, param_type.get_pointer_to());
                    _ipa_modif_vars[n] = Utils::UsageKind::NONE;
                }
            }
        }
        
        // Initialize global variables usage to NONE (for recursive calls)
        GlobalVarsSet global_vars = _graph->get_global_variables();
        for(GlobalVarsSet::iterator it = global_vars.begin(); it != global_vars.end(); ++it)
        {
            _ipa_modif_vars[*it] = Utils::UsageKind::NONE;
        }
    }
    
    void UseDef::compute_usage()
    {
        Node* graph = _graph->get_graph();
        compute_usage_rec(graph);
        ExtensibleGraph::clear_visits(graph);
    }

    // Top bottom traversal
    void UseDef::compute_usage_rec(Node* current)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);
            if(current->is_exit_node())
                return;

            if(current->is_graph_node()
                && !current->is_asm_def_node() && !current->is_asm_op_node())
            {
                // Use-def info is computed from inner nodes to outer nodes
                compute_usage_rec(current->get_graph_entry_node());

                // We need to do this here because in order to propagate the tasks usage
                // to the outer nodes where they are created,
                // all children of the task_creation node must have the use-def computed
                ObjectList<Node*> inner_tasks;
                if(ExtensibleGraph::node_contains_tasks(current, current, inner_tasks))
                    // This set is traversed from end to start because the tasks are ordered from top to bottom and
                    // we need later tasks to be analyzed before its ancestor tasks are analyzed
                    for(ObjectList<Node*>::reverse_iterator it = inner_tasks.rbegin(); it != inner_tasks.rend(); ++it)
                        propagate_task_usage_to_task_creation_node(*it);

                // Propagate usage info from inner to outer nodes
                current->set_visited(false);
                ExtensibleGraph::clear_visits_in_level(current->get_graph_entry_node(), current);
                set_graph_node_use_def(current);
            }
            else
            {
                // Treat statements in the current node
                ObjectList<Nodecl::NodeclBase> stmts = current->get_statements();
                UsageVisitor uv(current, _graph, &_ipa_modif_vars);
                for(ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin(); it != stmts.end(); ++it)
                {
                    uv.compute_statement_usage(*it);
                }
            }

            // Compute usage form children
            ObjectList<Node*> children = current->get_children();
            for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                compute_usage_rec(*it);
        }
    }

    void UseDef::propagate_task_usage_to_task_creation_node(Node* task_creation)
    {
        // Task creation children may be: created task, task synchronization, another task creation
        Utils::ext_sym_set ue_vars = task_creation->get_ue_vars();
        Utils::ext_sym_set killed_vars = task_creation->get_killed_vars();
        Utils::ext_sym_set undef_vars = task_creation->get_undefined_behaviour_vars();
        Utils::ext_sym_set child_ue_vars, child_killed_vars, child_undef_vars;

        ObjectList<Node*> children = task_creation->get_children();
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
        {
            child_ue_vars = (*it)->get_ue_vars();
            child_killed_vars = (*it)->get_killed_vars();
            child_undef_vars = (*it)->get_undefined_behaviour_vars();

            ue_vars.insert(child_ue_vars.begin(), child_ue_vars.end());
            killed_vars.insert(child_killed_vars.begin(), child_killed_vars.end());
            undef_vars.insert(child_undef_vars.begin(), child_undef_vars.end());
        }

        // Purge the sets:
        // 1.- When the same variable appears in all three sets UE, KILLED, UNDEF
        Utils::ext_sym_set::iterator it = undef_vars.begin();
        while(it != undef_vars.end())
        {
            if(!Utils::ext_sym_set_contains_enclosing_nodecl(it->get_nodecl(), ue_vars).is_null() &&
                !Utils::ext_sym_set_contains_enclosing_nodecl(it->get_nodecl(), killed_vars).is_null())
            {
                undef_vars.erase(it++);
            }
            else
                ++it;
        }
        // 2.- When a variable is UNDEF and it is either UE or KILLED, it must remain as UNDEF only
        it = ue_vars.begin();
        while(it != ue_vars.end())
        {
            if(!Utils::ext_sym_set_contains_enclosing_nodecl(it->get_nodecl(), undef_vars).is_null() &&
                Utils::ext_sym_set_contains_enclosing_nodecl(it->get_nodecl(), killed_vars).is_null())
                ue_vars.erase(it++);
            else
                ++it;
        }
        it = killed_vars.begin();
        while(it != killed_vars.end())
        {
            if(!Utils::ext_sym_set_contains_enclosing_nodecl(it->get_nodecl(), undef_vars).is_null() &&
                Utils::ext_sym_set_contains_enclosing_nodecl(it->get_nodecl(), ue_vars).is_null())
                killed_vars.erase(it++);
            else
                ++it;
        }
        // 3.- Set the purged sets as usage information of the task creation node
        task_creation->set_ue_var(ue_vars);
        task_creation->set_killed_var(killed_vars);
        task_creation->set_undefined_behaviour_var(undef_vars);
    }

    ObjectList<Utils::ext_sym_set> UseDef::get_use_def_over_nodes(Node* current)
    {
        ObjectList<Utils::ext_sym_set> use_def, use_def_aux;

        if(!current->is_visited())
        {
            current->set_visited(true);

            // Task nodes information has already been propagated to its corresponding task_creation node
            if(!current->is_omp_task_node())
            {
                // Use-Def in current node
                Utils::ext_sym_set ue_vars = current->get_ue_vars();
                Utils::ext_sym_set killed_vars = current->get_killed_vars();
                Utils::ext_sym_set undef_vars = current->get_undefined_behaviour_vars();
                Utils::ext_sym_set used_addresses = current->get_used_addresses();

                // Concatenate info from children nodes
                ObjectList<Node*> children = current->get_children();
                Utils::ext_sym_set ue_children, killed_children, undef_children, used_addresses_children;
                for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    use_def_aux = get_use_def_over_nodes(*it);
                    if(!use_def_aux.empty())
                    {
                        ue_children = ext_sym_set_union(ue_children, use_def_aux[0]);
                        killed_children = ext_sym_set_union(killed_children, use_def_aux[1]);
                        undef_children = ext_sym_set_union(undef_children, use_def_aux[2]);
                        used_addresses_children = ext_sym_set_union(used_addresses, use_def_aux[3]);
                    }
                }

                // Merge children
                merge_children_usage(ue_children, killed_children, undef_children, current->get_id());

                // Merge current node and its children usage information
                propagate_usage_to_ancestors(ue_vars, killed_vars, undef_vars, used_addresses,
                                              ue_children, killed_children, undef_children, used_addresses_children);

                // Set the new usage information to the current node
                if(!ue_vars.empty() || !killed_vars.empty() || !undef_vars.empty() || !used_addresses.empty())
                {
                    use_def.append(ue_vars);
                    use_def.append(killed_vars);
                    use_def.append(undef_vars);
                    use_def.append(used_addresses);
                }
            }
        }

        return use_def;
    }

    void UseDef::merge_children_usage(Utils::ext_sym_set& ue_vars, Utils::ext_sym_set& killed_vars,
                                       Utils::ext_sym_set& undef_vars, int node_id)
    {
        // Purge UNDEF vars from those vars that are in both UE and KILLED lists
        for(Utils::ext_sym_set::iterator it = ue_vars.begin(); it != ue_vars.end(); ++it)
        {
            if((!Utils::ext_sym_set_contains_enclosing_nodecl(it->get_nodecl(), killed_vars).is_null() ||
                  !Utils::ext_sym_set_contains_enclosed_nodecl(it->get_nodecl(), killed_vars).is_null()) &&
                !Utils::ext_sym_set_contains_enclosing_nodecl(it->get_nodecl(), undef_vars).is_null())
            {
                undef_vars.erase(*it);
            }
        }
    }

    void UseDef::set_graph_node_use_def(Node* current)
    {
        if(current->is_graph_node())
        {
            if(!current->is_visited())
            {
                current->set_visited(true);
                Utils::ext_sym_set ue_vars, killed_vars, undef_vars, used_addresses;
                ObjectList<Utils::ext_sym_set> usage = get_use_def_over_nodes(current->get_graph_entry_node());
                if(!usage.empty())
                {
                    ue_vars = usage[0];
                    killed_vars = usage[1];
                    undef_vars = usage[2];
                    used_addresses = usage[3];
                }

                Utils::ext_sym_set private_ue_vars, private_killed_vars, private_undef_vars;

                if(current->is_omp_loop_node() || current->is_omp_sections_node() || current->is_omp_single_node() ||
                    current->is_omp_parallel_node() || current->is_omp_task_node())
                {   // Take into account data-sharing clauses in Use-Def Task node computation
                    Nodecl::List environ =
                            current->get_graph_related_ast().as<Nodecl::OpenMP::Task>().get_environment().as<Nodecl::List>();
                    for(Nodecl::List::iterator it = environ.begin(); it != environ.end(); ++it)
                    {
                        if(it->is<Nodecl::OpenMP::Private>())
                        {   // Remove any usage computed in the inner nodes,
                            // because is the usage of a copy of this variable
                            Nodecl::List private_syms = it->as<Nodecl::OpenMP::Private>().get_symbols().as<Nodecl::List>();
                            for(Nodecl::List::iterator it_p = private_syms.begin(); it_p != private_syms.end(); ++it_p)
                            {
                                if(Utils::ext_sym_set_contains_nodecl(*it_p, undef_vars))
                                {
                                    undef_vars.erase(Utils::ExtendedSymbol(*it_p));
                                    private_undef_vars.insert(Utils::ExtendedSymbol(*it_p));
                                }
                                else
                                {
                                    if(Utils::ext_sym_set_contains_nodecl(*it_p, ue_vars))
                                    {
                                        ue_vars.erase(Utils::ExtendedSymbol(*it_p));
                                        private_ue_vars.insert(Utils::ExtendedSymbol(*it_p));
                                    }
                                    if(Utils::ext_sym_set_contains_nodecl(*it_p, killed_vars))
                                    {
                                        killed_vars.erase(Utils::ExtendedSymbol(*it_p));
                                        private_killed_vars.insert(Utils::ExtendedSymbol(*it_p));
                                    }
                                }
                            }
                        }
                        if(it->is<Nodecl::OpenMP::Firstprivate>())
                        {   // This variable is Upper Exposed in the task
                            Nodecl::List firstprivate_syms = it->as<Nodecl::OpenMP::Firstprivate>().get_symbols().as<Nodecl::List>();
                            for(Nodecl::List::iterator it_fp = firstprivate_syms.begin(); it_fp != firstprivate_syms.end(); ++it_fp)
                            {
                                if(Utils::ext_sym_set_contains_nodecl(*it_fp, undef_vars))
                                {
                                    undef_vars.erase(Utils::ExtendedSymbol(*it_fp));
                                    private_undef_vars.insert(Utils::ExtendedSymbol(*it_fp));
                                }
                                else if(Utils::ext_sym_set_contains_nodecl(*it_fp, killed_vars))
                                {
                                    killed_vars.erase(Utils::ExtendedSymbol(*it_fp));
                                    private_killed_vars.insert(Utils::ExtendedSymbol(*it_fp));
                                }
                            }
                        }
                    }
                }

                current->add_ue_var(ue_vars);
                current->add_killed_var(killed_vars);
                current->add_undefined_behaviour_var(undef_vars);
                current->add_used_addresses(used_addresses);

                current->add_private_ue_var(private_ue_vars);
                current->add_private_killed_var(private_killed_vars);
                current->add_private_undefined_behaviour_var(private_undef_vars);
            }
        }
        else
        {
            internal_error("Cannot propagate use-def info from inner nodes to outer nodes "\
                            "in node '%d' with type '%s'. GRAPH_NODE expected\n",
                            current->get_id(), current->get_type_as_string().c_str());
        }
    }

    // ************************** End class implementing use-definition analysis ************************** //
    // **************************************************************************************************** //
    
    
    
    // **************************************************************************************************** //
    // ***************************** Class implementing use-definition visitor **************************** //

    //! Special class for visiting Reference nodecls
    class LIBTL_CLASS ReferenceUsageVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        Nodecl::NodeclBase _current_nodecl;
        bool _store_symbol;
        Utils::ext_sym_set _used_ext_syms;
        
    public:
        // Constructor
        ReferenceUsageVisitor()
            : _current_nodecl(Nodecl::NodeclBase::null()), _store_symbol(false), _used_ext_syms()
        {}
        
        // Getters
        Utils::ext_sym_set get_ue_vars()
        {
            return _used_ext_syms;
        }
        
        // Visitors
        void visit(const Nodecl::ArraySubscript& n)
        {
            if(_store_symbol)
            {
                Nodecl::NodeclBase var_in_use = n;
                if(!_current_nodecl.is_null())
                    var_in_use = _current_nodecl;
                _used_ext_syms.insert(var_in_use);
            }
            
            // Walk the subscripts
            _store_symbol = true;
            walk(n.get_subscripts());
            _store_symbol = false;
        }
        
        void visit(const Nodecl::ClassMemberAccess& n)
        {
            if(_current_nodecl.is_null())
                _current_nodecl = n;
            walk(n.get_member());
            _current_nodecl = Nodecl::NodeclBase::null();
        }
        
        void visit(const Nodecl::Reference& n)
        {
            if(_current_nodecl.is_null())
                _current_nodecl = n;
            walk(n.get_rhs());
            _current_nodecl = Nodecl::NodeclBase::null();
        }
        
        void visit(const Nodecl::Symbol& n)
        {
            if(_store_symbol)
            {
                Nodecl::NodeclBase var_in_use = n;
                if(!_current_nodecl.is_null())
                    var_in_use = _current_nodecl;
                _used_ext_syms.insert(var_in_use);
            }
        }
    };
    
    static ExtensibleGraph* find_graph_in_list_from_function_symbol(Symbol func_sym)
    {
        ExtensibleGraph* result = NULL;
        for(ObjectList<ExtensibleGraph*>::const_iterator it = _pcfgs.begin(); it != _pcfgs.end(); ++it)
        {
            Symbol s((*it)->get_function_symbol());
            if(s.is_valid() && (s == func_sym))
            {
                result = *it;
                break;
            }
        }
        return result;
    }
    
    UsageVisitor::UsageVisitor(Node* n, ExtensibleGraph* pcfg, IpUsageMap* ipa_modifiable_vars)
        : _node(n), _define(false), _current_nodecl(Nodecl::NodeclBase::null()),
          _ipa_modif_vars(ipa_modifiable_vars), _avoid_func_calls(false), _pcfg(pcfg)
    {}
    
    void UsageVisitor::set_var_usage_to_node(const Utils::ExtendedSymbol& var, Utils::UsageKind usage_kind)
    {
        Utils::ext_sym_set ue_vars = _node->get_ue_vars();
        Utils::ext_sym_set killed_vars = _node->get_killed_vars();
        Utils::ext_sym_set undef_vars = _node->get_undefined_behaviour_vars();
        Utils::ext_sym_set empty_set;
        if(usage_kind._usage_type & Utils::UsageKind::USED)
        {
            Utils::ext_sym_set ue_tmp; ue_tmp.insert(var);
            propagate_usage_to_ancestors(ue_vars, killed_vars, undef_vars, empty_set,
                                          ue_tmp, empty_set, empty_set, empty_set);
            
            _node->set_ue_var(ue_vars);                   // Replace the set of upwards exposed variables associated to the node
        }
        else if(usage_kind._usage_type & Utils::UsageKind::DEFINED)
        {
            Utils::ext_sym_set killed_tmp; killed_tmp.insert(var);
            propagate_usage_to_ancestors(ue_vars, killed_vars, undef_vars, empty_set, 
                                          empty_set, killed_tmp, empty_set, empty_set);
            _node->set_killed_var(killed_vars);               // Replace the set of killed vars associated to the node
        }
        else
        {
            Utils::ext_sym_set undef_tmp; undef_tmp.insert(var);
            propagate_usage_to_ancestors(ue_vars, killed_vars, undef_vars, empty_set, 
                                          empty_set, empty_set, undef_tmp, empty_set);
            _node->set_undefined_behaviour_var(undef_vars);   // Replace the set of undefined behavior vars associated to the node
        }
    }

    void UsageVisitor::set_var_usage_to_node(const Utils::ext_sym_set& var_set, Utils::UsageKind usage_kind)
    {
        for(Utils::ext_sym_set::const_iterator it = var_set.begin(); it != var_set.end(); ++it)
            set_var_usage_to_node(*it, usage_kind);
    }

    void UsageVisitor::compute_statement_usage(Nodecl::NodeclBase st)
    {
        Node* outer_node = _node->get_outer_node();
        if(outer_node->is_split_statement() && !_node->is_function_call_node())
        {   // The function calls that can appear in the split statement have already been analyzed
            // We want to avoid computing the usage again. In exchange, we want to propagate the previously compute usage
            // F.i.:   int c = foo(a, b)
            //         PCFG:
            //           ______________________________________________
            //          |  [SPLIT_STMT]                                |
            //          |  __________________________________________  |
            //          | | [FUNC_CALL]                              | |
            //          | |  _______       ___________       ______  | |
            //          | | |       |     |           |     |      | | |
            //          | | | ENTRY |---->| foo(a, b) |---->| EXIT | | |
            //          | | |_______|     |___________|     |______| | |
            //          | |__________________________________________| |
            //          |               _______|_______                |
            //          |              |               |               |
            //          |              | c = foo(a, b) |               |
            //          |              |_______________|               |
            //          |______________________________________________|
            //
            //         When computing Use-Def of "c = foo(a, b)", we want to propagate
            //             the info calculated for "b=foo(a, b)" regarding to the function call
            ObjectList<Node*> parents = _node->get_parents();
            while(!parents.empty() && !parents[0]->is_entry_node())
            {
                ERROR_CONDITION(parents.size() != 1,
                                "Ancestors of a non function call node which are inside the enclosing split statement "
                                "must not have any sibling, but we have found %d siblings", parents.size());

                _node->set_ue_var(parents[0]->get_ue_vars());
                _node->set_killed_var(parents[0]->get_killed_vars());
                _node->set_undefined_behaviour_var(parents[0]->get_undefined_behaviour_vars());

                parents = parents[0]->get_parents();
            }

            _avoid_func_calls = true;
        }

        walk(st);
    }

    void UsageVisitor::unhandled_node(const Nodecl::NodeclBase& n)
    {
        WARNING_MESSAGE("Unhandled node '%s' with type '%s' during Use-Def Analysis",
                        n.prettyprint().c_str(), ast_print_node_type(n.get_kind()));
    }

    void UsageVisitor::visit_assignment(const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs)
    {
        _define = false;
        walk(rhs);
        _define = true;
        walk(lhs);
        _define = false;
    }

    void UsageVisitor::visit_binary_assignment(const Nodecl::NodeclBase& lhs, const Nodecl::NodeclBase& rhs)
    {
        // Traverse the use of both the lhs and the rhs
        walk(rhs);
        walk(lhs);

        // Traverse the definition of the lhs
        _define = true;
        walk(lhs);
        _define = false;
    }
    
    void UsageVisitor::visit_function(const Nodecl::NodeclBase& called_sym, const Nodecl::List& real_arguments)
    {
        if(_avoid_func_calls)
            return;

        // The function called must be analyzed only in case it has not been analyzed previously
        TL::Symbol func_sym = called_sym.get_symbol();
        Nodecl::List simplified_arguments = simplify_pointers(real_arguments);
        if(func_sym.is_valid())
        {   // The called function is not a pointer to function
            ObjectList<TL::Symbol> params = func_sym.get_function_parameters();
            ExtensibleGraph* called_pcfg = find_graph_in_list_from_function_symbol(func_sym);
            if(called_pcfg != NULL)
            {   // Due to the way we call the UseDef analysis, if the usage of the called function is not yet computed,
                // this means that it is a recursive call                
                if(called_pcfg->usage_is_computed())
                {   // Called function code is reachable and UseDef Analysis of the function has been calculated
                    ipa_propagate_known_function_usage(called_pcfg, simplified_arguments);
                }
                else
                {   // Recursive call
                    ipa_propagate_recursive_call_usage(params, simplified_arguments);
                }
            }
            else
            {   // Called function code's is not reachable
                ipa_propagate_unreachable_function_usage(
                        func_sym, params, simplified_arguments, _pointer_to_size_map);
            }
        }
        else
        {   // Calling a pointer to function: neither code nor prototype are reachable, thus:
            ipa_propagate_pointer_to_function_usage(simplified_arguments);
        }
    }
    
    void UsageVisitor::visit_increment(const Nodecl::NodeclBase& rhs)
    {
        // Use of the rhs
        walk(rhs);
        
        // Definition of the rhs
        _define = true;
        Nodecl::NodeclBase current_nodecl = _current_nodecl;
        _current_nodecl = Nodecl::NodeclBase::null();
        walk(rhs);
        _current_nodecl = current_nodecl;
        _define = false;
    }

    void UsageVisitor::visit(const Nodecl::AddAssignment& n)
    {
        visit_binary_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::ArithmeticShrAssignment& n)
    {
        visit_binary_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::ArraySubscript& n)
    {
        Nodecl::NodeclBase current_nodecl = _current_nodecl;
        bool define = _define;

        // Use of the subscripts
        _define = false;
        _current_nodecl = Nodecl::NodeclBase::null();
        walk(n.get_subscripts());

        // Use of the ArraySubscript
        _define = define;   // Just in case
        if(current_nodecl.is_null())
            _current_nodecl = n;
        else
            _current_nodecl = current_nodecl;
        walk(n.get_subscripted());
        _current_nodecl = Nodecl::NodeclBase::null();
        
        _node->add_used_address(Utils::ExtendedSymbol(n.get_subscripted()));
    }

    void UsageVisitor::visit(const Nodecl::Assignment& n)
    {
        visit_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::BitwiseAndAssignment& n)
    {
        visit_binary_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::BitwiseOrAssignment& n)
    {
        visit_binary_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::BitwiseShlAssignment& n)
    {
        visit_binary_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::BitwiseShrAssignment& n)
    {
        visit_binary_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::BitwiseXorAssignment& n)
    {
        visit_binary_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::ClassMemberAccess& n)
    {
        if(_current_nodecl.is_null())
            _current_nodecl = n;

        // walk(n.get_lhs());  // In a member access, the use/definition is always of the member, not the base
        walk(n.get_member());

        _current_nodecl = Nodecl::NodeclBase::null();
    }

    void UsageVisitor::visit(const Nodecl::Dereference& n)
    {
        Nodecl::NodeclBase current_nodecl = _current_nodecl;
        bool define = _define;

        // Use of the Dereferenced variable
        _define = false;
        _current_nodecl = Nodecl::NodeclBase::null();
        walk(n.get_rhs());

        // Use of the Dereference
        _define = define;
        _current_nodecl = n;
        walk(n.get_rhs());

        _current_nodecl = Nodecl::NodeclBase::null();
    }

    void UsageVisitor::visit(const Nodecl::DivAssignment& n)
    {
        visit_binary_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::FunctionCall& n)
    {
        visit_function(n.get_called(), n.get_arguments().as<Nodecl::List>());
    }

    void UsageVisitor::visit(const Nodecl::MinusAssignment& n)
    {
        visit_binary_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::ModAssignment& n)
    {
        visit_binary_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::MulAssignment& n)
    {
        visit_binary_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::ObjectInit& n)
    {
        Nodecl::Symbol n_sym = Nodecl::Symbol::make(n.get_symbol(), n.get_locus());
        set_var_usage_to_node(Utils::ExtendedSymbol(n_sym), Utils::UsageKind::DEFINED);

        // Value of initialization, in case it exists
        walk(n.get_symbol().get_value());
    }

    void UsageVisitor::visit(const Nodecl::Postdecrement& n)
    {
        visit_increment(n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::Postincrement& n)
    {
        visit_increment(n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::Predecrement& n)
    {
        visit_increment(n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::Preincrement& n)
    {
        visit_increment(n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::Range& n)
    {
        walk(n.get_lower());
        walk(n.get_upper());
        walk(n.get_stride());
    }

    void UsageVisitor::visit(const Nodecl::Reference& n)
    {
        Nodecl::NodeclBase rhs = n.get_rhs();
        
        // Insert the whole node to the list of accessed addresses
        _node->add_used_address(Utils::ExtendedSymbol(n));
        
        if(!_current_nodecl.is_null())
        {
            walk(rhs);
        }
        else
        {   // Only pointers to member are really used
            ReferenceUsageVisitor ruv;
            ruv.walk(rhs);
            _node->add_ue_var(ruv.get_ue_vars());
        }
    }
    
    void UsageVisitor::visit(const Nodecl::Symbol& n)
    {
        Nodecl::NodeclBase var_in_use = n;
        if (!_current_nodecl.is_null())
            var_in_use = _current_nodecl;

        Utils::ext_sym_set killed_vars = _node->get_killed_vars();
        Utils::ext_sym_set undef_vars = _node->get_undefined_behaviour_vars();
        
        if (Utils::ext_sym_set_contains_enclosing_nodecl(var_in_use, killed_vars).is_null() && 
            Utils::ext_sym_set_contains_enclosing_nodecl(var_in_use, undef_vars).is_null())
        {
            Utils::UsageKind usage_kind = (_define ? Utils::UsageKind::DEFINED : Utils::UsageKind::USED);
            set_var_usage_to_node(Utils::ExtendedSymbol(var_in_use), usage_kind);
            store_ipa_information(var_in_use);
        }
        
        // The the usage of the symbol to the list of accessed addresses, if necessary
        if(!var_in_use.is<Nodecl::Reference>() && n.get_symbol().get_type().is_pointer())
            _node->add_used_address(Utils::ExtendedSymbol(n));
    }

    void UsageVisitor::visit(const Nodecl::UnalignedVectorStore& n)
    {
        visit_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::VectorAssignment& n)
    {
        visit_assignment(n.get_lhs(), n.get_rhs());
    }

    // It is used: the base, the strides (if variables) and the memory positions formed by base+stride_i
    void UsageVisitor::visit(const Nodecl::VectorGather& n)
    {
        Nodecl::NodeclBase base = n.get_base();
        Nodecl::NodeclBase strides = n.get_strides();

        // Usage of the base
        walk(base);

        if(strides.is<Nodecl::VectorLiteral>())
        {
            Nodecl::List stride_list = strides.as<Nodecl::VectorLiteral>().get_scalar_values().as<Nodecl::List>();
            for(Nodecl::List::iterator it = stride_list.begin(); it != stride_list.end(); ++it)
            {
                // Usage of base+stride_i
                Nodecl::Add current_access = Nodecl::Add::make(base.shallow_copy(), it->shallow_copy(), base.get_type(), it->get_locus());
                if(!Utils::ext_sym_set_contains_nodecl(current_access, _node->get_killed_vars()))
                    set_var_usage_to_node(Utils::ExtendedSymbol(current_access), Utils::UsageKind::USED);
            }
        }
        else
        {
            // Usage of the stride
            walk(strides);

            Nodecl::Add current_access = Nodecl::Add::make(base.shallow_copy(), strides.shallow_copy(), base.get_type(), strides.get_locus());
            if(!Utils::ext_sym_set_contains_nodecl(current_access, _node->get_killed_vars()))
                set_var_usage_to_node(Utils::ExtendedSymbol(current_access), Utils::UsageKind::USED);
        }
    }

    void UsageVisitor::visit(const Nodecl::VectorMaskAssignment& n)
    {
        visit_assignment(n.get_lhs(), n.get_rhs());
    }

    // It is used: the strides (if variables). It is defined the memory positions formed by base+stride_i
    void UsageVisitor::visit(const Nodecl::VectorScatter& n)
    {
        Nodecl::NodeclBase base = n.get_base();
        Nodecl::NodeclBase strides = n.get_strides();
        Nodecl::NodeclBase source = n.get_source();

        // Usage of source and base
        walk(source);
        walk(base);

        if(strides.is<Nodecl::VectorLiteral>())
        {
            Nodecl::List stride_list = strides.as<Nodecl::VectorLiteral>().get_scalar_values().as<Nodecl::List>();
            for(Nodecl::List::iterator it = stride_list.begin(); it != stride_list.end(); ++it)
            {
                // Usage of base+stride_i
                Nodecl::Add current_access = Nodecl::Add::make(base.shallow_copy(), it->shallow_copy(), base.get_type(), it->get_locus());
                if(!Utils::ext_sym_set_contains_nodecl(current_access, _node->get_killed_vars()))
                    set_var_usage_to_node(Utils::ExtendedSymbol(current_access), Utils::UsageKind::DEFINED);
            }
        }
        else
        {
            // Usage of strides
            walk(strides);

            Nodecl::Add current_access = Nodecl::Add::make(base.shallow_copy(), strides.shallow_copy(), base.get_type(), strides.get_locus());
            if(!Utils::ext_sym_set_contains_nodecl(current_access, _node->get_killed_vars()))
                set_var_usage_to_node(Utils::ExtendedSymbol(current_access), Utils::UsageKind::DEFINED);
        }

    }

    void UsageVisitor::visit(const Nodecl::VectorSincos& n)
    {
        Nodecl::NodeclBase source = n.get_source();
        Nodecl::NodeclBase sin_ptr = n.get_sin_pointer();
        Nodecl::NodeclBase cos_ptr = n.get_cos_pointer();

        walk(source);
        walk(sin_ptr);
        walk(cos_ptr);

        // Uses the source of the operation
        if(!Utils::ext_sym_set_contains_nodecl(source, _node->get_killed_vars()))
            set_var_usage_to_node(Utils::ExtendedSymbol(source), Utils::UsageKind::USED);

        // Defines the memory pointed by the second and third parameters
        Nodecl::NodeclBase sin_ptd = Nodecl::Dereference::make(sin_ptr.shallow_copy(), sin_ptr.get_type());
        if(!Utils::ext_sym_set_contains_nodecl(sin_ptd, _node->get_killed_vars()))
            set_var_usage_to_node(Utils::ExtendedSymbol(sin_ptd), Utils::UsageKind::DEFINED);
        Nodecl::NodeclBase cos_ptd = Nodecl::Dereference::make(cos_ptr.shallow_copy(), cos_ptr.get_type());
        if(!Utils::ext_sym_set_contains_nodecl(cos_ptd, _node->get_killed_vars()))
            set_var_usage_to_node(Utils::ExtendedSymbol(cos_ptd), Utils::UsageKind::DEFINED);
    }

    void UsageVisitor::visit(const Nodecl::VectorStore& n)
    {
        visit_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::VectorStreamStore& n)
    {
        visit_assignment(n.get_lhs(), n.get_rhs());
    }

    void UsageVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        visit_function(n.get_called(), n.get_arguments().as<Nodecl::List>());
    }

}
}
