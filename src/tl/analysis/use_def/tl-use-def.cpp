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

#include <fstream>

namespace TL {
namespace Analysis {

    std::map<Symbol, ExtensibleGraph*> _pcfgs;
    SizeMap _pointer_to_size_map;
    
    // **************************************************************************************************** //
    // **************************** Class implementing use-definition analysis **************************** //

namespace {
    
    /*!This method computes the usage of a node in two different cases:
     * - we are merging the usage of the children nodes with a parent node to set the usage of the enclosing graph node
     * - we are computing the usage of a node: since there may be more than one statement within the same node,
     *                                         we need to take into account the usage computed for the already treated statements
     */
    void propagate_usage_to_ancestor(
            Node* ancestor,
            NodeclSet& ue_vars, NodeclSet& killed_vars,
            NodeclSet& undef_vars, NodeclSet& used_addresses,
            const NodeclSet& ue_children, const NodeclSet& killed_children,
            const NodeclSet& undef_children, const NodeclSet& used_addresses_children)
    {
        // Propagate the upwards exposed variables
        NBase ue_previously_killed_subobject, ue_previously_undef_subobject;
        for(NodeclSet::iterator it = ue_children.begin(); it != ue_children.end(); ++it)
        {
            NBase n_it = *it;

            // There is a special case when dealing with Tasks: even though a task may kill a variable
            // (and this usage is propagated to its task creation node), we still have to propagate the usage of the children,
            // because we do not know what is happening first
            bool is_task_creation_context = (ancestor->is_context_node()
                    && ancestor->get_graph_entry_node()->get_children().size()==1
                    && ancestor->get_graph_entry_node()->get_children()[0]->is_omp_task_creation_node());

            // UE vars can only be upwards propagated if the are not already KILLED in the parent
            // or they (or an enclosing nodecl) are not yet in the result set
            if(!is_task_creation_context
                    && (!Utils::nodecl_set_contains_enclosing_nodecl(n_it, killed_vars).is_null()
                            || !Utils::nodecl_set_contains_enclosing_nodecl(n_it, ue_vars).is_null()))
                continue;

            ue_previously_killed_subobject = Utils::nodecl_set_contains_enclosed_nodecl(n_it, killed_vars);
            ue_previously_undef_subobject = Utils::nodecl_set_contains_enclosed_nodecl(n_it, undef_vars);
            if (is_task_creation_context
                || (ue_previously_killed_subobject.is_null()
                    && ue_previously_undef_subobject.is_null()))
            {   // Neither killed nor undef var sets contain the current ue var
                // Or the ancestor is a task creation node, in which case we have to be conservative and propagate its children usage
                NBase tmp = Utils::nodecl_set_contains_enclosed_nodecl(n_it, ue_vars);
                if(!tmp.is_null())
                    ue_vars.erase(tmp);   // Delete the enclosed var from the list
                ue_vars.insert(*it);      // Insert the new containing var
            }
            else
            {   // Here a part of the nodecl is KILLED|UNDEF and a part is UE
                NBase non_killed_ue_vars, non_undef_ue_vars;
                if(!ue_previously_killed_subobject.is_null())
                    non_killed_ue_vars = split_var_depending_on_usage(n_it, ue_previously_killed_subobject);
                if(!ue_previously_undef_subobject.is_null())
                {   // A variable marked as UNDEF can still be UE
                    if(Utils::nodecl_set_contains_nodecl(n_it, undef_vars))
                        non_undef_ue_vars = n_it;
                    else
                        non_undef_ue_vars = split_var_depending_on_usage(n_it, ue_previously_undef_subobject);
                }
                
                if((!ue_previously_killed_subobject.is_null() && non_killed_ue_vars.is_null()) || 
                   (!ue_previously_undef_subobject.is_null() && non_undef_ue_vars.is_null()))
                {   // When the two sets are empty is because the separation has not been possible
                    // Then, we set to undef the whole object and remove the partial object from the corresponding list(s)
                    if(!ue_previously_killed_subobject.is_null())
                        killed_vars.erase(ue_previously_killed_subobject);
                    if(!ue_previously_undef_subobject.is_null())
                        undef_vars.erase(ue_previously_undef_subobject);
                    undef_vars.insert(n_it);
                }
                else
                {   // new_ue_varsX may be the union of different array ranges. We may want to split the union into separated nodecls
                    if(!non_killed_ue_vars.is_null())
                        ue_vars.insert(non_killed_ue_vars);
                    if(!non_undef_ue_vars.is_null())
                        ue_vars.insert(non_undef_ue_vars);
                }
            }
        }

        // Propagate the killed variables
        NBase non_killed_var;
        for(NodeclSet::iterator it = killed_children.begin(); it != killed_children.end(); ++it)
        {
            NBase n_it = *it;
            if(!Utils::nodecl_set_contains_enclosing_nodecl(n_it, undef_vars).is_null() ||
               !Utils::nodecl_set_contains_enclosing_nodecl(n_it, killed_vars).is_null())
                continue;

            non_killed_var = Utils::nodecl_set_contains_enclosed_nodecl(n_it, undef_vars);
            if(non_killed_var.is_null())
            {   // Undef var set does not contain the current killed var
                NBase already_killed_var = Utils::nodecl_set_contains_enclosed_nodecl(n_it, killed_vars);
                if(!already_killed_var.is_null())
                    killed_vars.erase(already_killed_var);    // A part of the variable was already killed: remove the subobject
                killed_vars.insert(*it);                      // Insert the whole enclosing object
            }
            else
            {   // Here a part of the nodecl has already been marked as undefined
                NBase new_killed_vars;
                new_killed_vars = split_var_depending_on_usage(n_it, non_killed_var);
                if(new_killed_vars.is_null())
                {   // When the set is null is because the separation has not been possible
                    // Then, we set to undefined the whole object and remove the partial object from the killed list
                    NBase already_killed_var = Utils::nodecl_set_contains_enclosed_nodecl(n_it, killed_vars);
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
        NBase undef_previously_ue_subobject, undef_previously_killed_subobject;
        for(NodeclSet::iterator it = undef_children.begin(); it != undef_children.end(); ++it)
        {
            NBase n_it = *it;
            // Variables marked as KILLED cannot be UNDEF
            if(!Utils::nodecl_set_contains_enclosing_nodecl(n_it, killed_vars).is_null())
                continue;
            
            undef_previously_ue_subobject = Utils::nodecl_set_contains_enclosed_nodecl(n_it, ue_vars);
            undef_previously_killed_subobject = Utils::nodecl_set_contains_enclosed_nodecl(n_it, killed_vars);
            if(undef_previously_ue_subobject.is_null() && undef_previously_killed_subobject.is_null())
            {   // Neither ue nor killed var sets contain the current ue var
                if(!Utils::nodecl_set_contains_enclosing_nodecl(n_it, undef_vars).is_null())
                    continue;
                else
                {
                    NBase tmp = Utils::nodecl_set_contains_enclosed_nodecl(n_it, undef_vars);
                    if(!tmp.is_null())
                        undef_vars.erase(tmp);        // Delete the enclosed var from the list
                    undef_vars.insert(*it);           // Insert the new containing var
                }
            }
            else
            {
                NBase non_ue_undef_vars, non_killed_undef_vars;
                if(!undef_previously_ue_subobject.is_null())
                {
                    if(Utils::nodecl_set_contains_nodecl(n_it, ue_vars))
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
                        ue_vars.erase(undef_previously_ue_subobject);
                    if(!undef_previously_killed_subobject.is_null())
                        killed_vars.erase(undef_previously_killed_subobject);
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
            : _graph(graph), _ipa_modif_vars(), _c_lib_file(""), _c_lib_sc(Scope())
    {
        // Load C lib functions
        load_c_lib_functions();

        initialize_ipa_var_usage();
        
        _pointer_to_size_map = graph->get_pointer_n_elements_map();
        
        // Convert the list of PCFG into a map of <symbol, pcfg*>
        for(ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            Symbol s((*it)->get_function_symbol());
            if(s.is_valid())
            {
                _pcfgs[s] = *it;
            }
        }
    }

    void UseDef::load_c_lib_functions()
    {
        std::string lib_file_name = IS_C_LANGUAGE ? "cLibraryFunctionList" : "cppLibraryFunctionList";
        _c_lib_file = std::string(MCXX_ANALYSIS_DATA_PATH) + "/" + lib_file_name;
        std::ifstream file(_c_lib_file.c_str());
        if (file.is_open())
        {
            // Create the scope where the C lib functions will be registered
            Symbol sym(Scope::get_global_scope().new_symbol("__CLIB_USAGE__"));
            sym.get_internal_symbol()->kind = SK_NAMESPACE;
            decl_context_t ctx = new_namespace_context(Scope::get_global_scope().get_decl_context(), sym.get_internal_symbol());
            sym.get_internal_symbol()->related_decl_context = ctx;
            _c_lib_sc = Scope(ctx);

            // Parse the file
            std::string line1, line2;
            while (file.good())
            {
                getline(file, line1);
                // Skip comented lines
                if (line1.substr(0, 13) == "__attribute__")
                {
                    getline(file, line2);
                    Source s; s << line1 << line2;
                    std::string source = s.get_source();
                    // Returned nodecl is null because declarations do not return any tree in C
                    /*const Nodecl::NodeclBase& func = */s.parse_statement(_c_lib_sc);
                }
            }
            file.close();
        }
        else
        {
            WARNING_MESSAGE("File containing C library calls Usage info cannot be opened. \n"\
                            "Path tried: '%s'", _c_lib_file.c_str());
        }
    }

    void UseDef::initialize_ipa_var_usage()
    {
        // Initialized Reference|Pointer parameters usage to NONE
        Symbol func_sym = _graph->get_function_symbol();
        if(func_sym.is_valid())
        {   // The PCFG contains a FunctionCode
            const ObjectList<TL::Symbol>& params = func_sym.get_function_parameters();
            for(ObjectList<TL::Symbol>::const_iterator it = params.begin(); it != params.end(); ++it)
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
        NodeclSet global_vars = _graph->get_global_variables();
        for(NodeclSet::iterator it = global_vars.begin(); it != global_vars.end(); ++it)
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
                if (ExtensibleGraph::node_contains_tasks(current, current, inner_tasks))
                {
                    // This set is traversed from end to start because the tasks are ordered from top to bottom and
                    // we need later tasks to be analyzed before its ancestor tasks are analyzed
                    for (ObjectList<Node*>::reverse_iterator it = inner_tasks.rbegin(); it != inner_tasks.rend(); ++it)
                        propagate_task_usage_to_task_creation_node(*it);
                }

                // Propagate usage info from inner to outer nodes
                current->set_visited(false);
                ExtensibleGraph::clear_visits_in_level_no_nest(current->get_graph_entry_node(), current);
                set_graph_node_use_def(current);
            }
            else
            {
                // Treat statements in the current node
                const NodeclList& stmts = current->get_statements();
                UsageVisitor uv(current, _graph, &_ipa_modif_vars, _c_lib_file, _c_lib_sc);
                for (NodeclList::const_iterator it = stmts.begin(); it != stmts.end(); ++it)
                {
                    uv.compute_statement_usage(*it);
                }
            }

            // Compute usage form children
            const ObjectList<Node*>& children = current->get_children();
            for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
                compute_usage_rec(*it);
        }
    }

    void UseDef::propagate_task_usage_to_task_creation_node(Node* task_creation)
    {
        // Task creation children may be: created task, task synchronization, another task creation
        NodeclSet ue_vars = task_creation->get_ue_vars();
        NodeclSet killed_vars = task_creation->get_killed_vars();
        NodeclSet undef_vars = task_creation->get_undefined_behaviour_vars();
        NodeclSet child_ue_vars, child_killed_vars, child_undef_vars;

        ObjectList<Node*> children = task_creation->get_children();
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
        {
            Node* c = *it;
            child_ue_vars = c->get_ue_vars();
            child_killed_vars = c->get_killed_vars();
            child_undef_vars = c->get_undefined_behaviour_vars();

            ue_vars.insert(child_ue_vars.begin(), child_ue_vars.end());
            killed_vars.insert(child_killed_vars.begin(), child_killed_vars.end());
            undef_vars.insert(child_undef_vars.begin(), child_undef_vars.end());
        }

        // Purge the sets:
        // 1.- When the same variable appears in all three sets UE, KILLED, UNDEF
        NodeclSet::iterator it = undef_vars.begin();
        while(it != undef_vars.end())
        {
            if(!Utils::nodecl_set_contains_enclosing_nodecl(*it, ue_vars).is_null() &&
                !Utils::nodecl_set_contains_enclosing_nodecl(*it, killed_vars).is_null())
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
            if(!Utils::nodecl_set_contains_enclosing_nodecl(*it, undef_vars).is_null() &&
                Utils::nodecl_set_contains_enclosing_nodecl(*it, killed_vars).is_null())
                ue_vars.erase(it++);
            else
                ++it;
        }
        it = killed_vars.begin();
        while(it != killed_vars.end())
        {
            if(!Utils::nodecl_set_contains_enclosing_nodecl(*it, undef_vars).is_null() &&
                Utils::nodecl_set_contains_enclosing_nodecl(*it, ue_vars).is_null())
                killed_vars.erase(it++);
            else
                ++it;
        }
        // 3.- Set the purged sets as usage information of the task creation node
        task_creation->set_ue_var(ue_vars);
        task_creation->set_killed_var(killed_vars);
        task_creation->set_undefined_behaviour_var(undef_vars);
    }

    ObjectList<NodeclSet> UseDef::get_use_def_over_nodes(Node* current)
    {
        ObjectList<NodeclSet> use_def, use_def_aux;

        if(!current->is_visited())
        {
            current->set_visited(true);

            // Task nodes information has already been propagated to its corresponding task_creation node
            if(!current->is_omp_task_node())
            {
                // Use-Def in current node
                NodeclSet ue_vars = current->get_ue_vars();
                NodeclSet killed_vars = current->get_killed_vars();
                NodeclSet undef_vars = current->get_undefined_behaviour_vars();
                NodeclSet used_addresses = current->get_used_addresses();

                // Concatenate info from children nodes
                const ObjectList<Node*>& children = current->get_children();
                NodeclSet ue_children, killed_children, undef_children, used_addresses_children;
                for(ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
                {
                    use_def_aux = get_use_def_over_nodes(*it);
                    if(!use_def_aux.empty())
                    {
                        ue_children = Utils::nodecl_set_union(ue_children, use_def_aux[0]);
                        killed_children = Utils::nodecl_set_union(killed_children, use_def_aux[1]);
                        undef_children = Utils::nodecl_set_union(undef_children, use_def_aux[2]);
                        used_addresses_children = Utils::nodecl_set_union(used_addresses, use_def_aux[3]);
                    }
                }

                // Merge children
                merge_children_usage(ue_children, killed_children, undef_children, current->get_id());

                // Merge current node and its children usage information
                propagate_usage_to_ancestor(
                        current,
                        ue_vars, killed_vars, undef_vars, used_addresses,
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

    void UseDef::merge_children_usage(NodeclSet& ue_vars, NodeclSet& killed_vars,
                                       NodeclSet& undef_vars, int node_id)
    {
        // Purge UNDEF vars from those vars that are in both UE and KILLED lists
        for(NodeclSet::iterator it = ue_vars.begin(); it != ue_vars.end(); ++it)
        {
            if((!Utils::nodecl_set_contains_enclosing_nodecl(*it, killed_vars).is_null() ||
                  !Utils::nodecl_set_contains_enclosed_nodecl(*it, killed_vars).is_null()) &&
                !Utils::nodecl_set_contains_enclosing_nodecl(*it, undef_vars).is_null())
            {
                undef_vars.erase(*it);
            }
        }
    }

    void UseDef::purge_local_variables(Scope graph_sc, NodeclSet& vars_set)
    {
        NodeclSet::iterator it = vars_set.begin(); 
        while (it != vars_set.end())
        {
            const NBase& n = it->no_conv();
            const NBase& n_base = Utils::get_nodecl_base(n);
            if (!n_base.is_null())
            {
                const ObjectList<Symbol>& func_params = _graph->get_function_parameters();
                Scope var_sc(n_base.get_symbol().get_scope());
                if (!n_base.get_symbol().get_type().is_pointer()
                        && !n_base.get_symbol().get_type().is_array()
                        && !(func_params.contains(n_base.get_symbol())
                                || n_base.get_symbol().get_type().is_any_reference())
                        && var_sc.scope_is_enclosed_by(graph_sc))
                {
                    vars_set.erase(it++);
                }
                else
                {
                    ++it;
                }
            }
            else
            {
                ++it;
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
                NodeclSet ue_vars, killed_vars, undef_vars, used_addresses;
                const ObjectList<NodeclSet>& usage = get_use_def_over_nodes(current->get_graph_entry_node());
                if(!usage.empty())
                {
                    ue_vars = usage[0];
                    killed_vars = usage[1];
                    undef_vars = usage[2];
                    used_addresses = usage[3];
                }

                NodeclSet private_ue_vars, private_killed_vars, private_undef_vars;

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
                            for(Nodecl::List::iterator itp = private_syms.begin(); itp != private_syms.end(); ++itp)
                            {
                                NBase n_itp = *itp;
                                if(Utils::nodecl_set_contains_nodecl(n_itp, undef_vars))
                                {
                                    undef_vars.erase(n_itp);
                                    private_undef_vars.insert(n_itp);
                                }
                                else
                                {
                                    if(Utils::nodecl_set_contains_nodecl(n_itp, ue_vars))
                                    {
                                        ue_vars.erase(n_itp);
                                        private_ue_vars.insert(n_itp);
                                    }
                                    if(Utils::nodecl_set_contains_nodecl(n_itp, killed_vars))
                                    {
                                        killed_vars.erase(n_itp);
                                        private_killed_vars.insert(n_itp);
                                    }
                                }
                            }
                        }
                        if(it->is<Nodecl::OpenMP::Firstprivate>())
                        {   // This variable is Upper Exposed in the task
                            Nodecl::List firstprivate_syms = it->as<Nodecl::OpenMP::Firstprivate>().get_symbols().as<Nodecl::List>();
                            for(Nodecl::List::iterator itfp = firstprivate_syms.begin(); itfp != firstprivate_syms.end(); ++itfp)
                            {
                                NBase n_itfp = *itfp;
                                if(Utils::nodecl_set_contains_nodecl(n_itfp, undef_vars))
                                {
                                    undef_vars.erase(n_itfp);
                                    private_undef_vars.insert(n_itfp);
                                }
                                else if(Utils::nodecl_set_contains_nodecl(n_itfp, killed_vars))
                                {
                                    killed_vars.erase(n_itfp);
                                    private_killed_vars.insert(n_itfp);
                                }
                            }
                        }
                    }
                }
                else
                {   // Purge variables local to the current graph
                    const Nodecl::NodeclBase ast = current->get_graph_related_ast();
                    Scope graph_sc(ast.retrieve_context());
                    purge_local_variables(graph_sc, ue_vars);
                    purge_local_variables(graph_sc, killed_vars);
                    purge_local_variables(graph_sc, undef_vars);
                    purge_local_variables(graph_sc, used_addresses);
                    purge_local_variables(graph_sc, private_ue_vars);
                    purge_local_variables(graph_sc, private_killed_vars);
                    purge_local_variables(graph_sc, private_undef_vars);
                }

                current->set_ue_var(ue_vars);
                current->set_killed_var(killed_vars);
                current->set_undefined_behaviour_var(undef_vars);
                current->set_used_addresses(used_addresses);

                current->set_private_ue_var(private_ue_vars);
                current->set_private_killed_var(private_killed_vars);
                current->set_private_undefined_behaviour_var(private_undef_vars);
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
        NBase _current_nodecl;
        bool _store_symbol;
        NodeclSet _used_ext_syms;
        
    public:
        // Constructor
        ReferenceUsageVisitor()
            : _current_nodecl(NBase::null()), _store_symbol(false), _used_ext_syms()
        {}
        
        // Getters
        NodeclSet get_ue_vars() const
        {
            return _used_ext_syms;
        }
        
        // Visitors
        void visit(const Nodecl::ArraySubscript& n)
        {
            if(_store_symbol)
            {
                NBase var_in_use = n;
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
            _current_nodecl = NBase::null();
        }
        
        void visit(const Nodecl::Reference& n)
        {
            if(_current_nodecl.is_null())
                _current_nodecl = n;
            walk(n.get_rhs());
            _current_nodecl = NBase::null();
        }
        
        void visit(const Nodecl::Symbol& n)
        {
            if(_store_symbol)
            {
                NBase var_in_use = n;
                if(!_current_nodecl.is_null())
                    var_in_use = _current_nodecl;
                _used_ext_syms.insert(var_in_use);
            }
        }
    };
    
    UsageVisitor::UsageVisitor(Node* n,
            ExtensibleGraph* pcfg,
            IpUsageMap* ipa_modifiable_vars,
            std::string c_lib_file,
            Scope c_lib_sc)
            : _node(n), _define(false), _current_nodecl(NBase::null()),
              _ipa_modif_vars(ipa_modifiable_vars), _c_lib_file(c_lib_file), _c_lib_sc(c_lib_sc),
              _avoid_func_calls(false), _pcfg(pcfg)
    {}
    
    void UsageVisitor::set_var_usage_to_node(const NBase& var, Utils::UsageKind usage_kind)
    {
        NodeclSet ue_vars = _node->get_ue_vars();
        NodeclSet killed_vars = _node->get_killed_vars();
        NodeclSet undef_vars = _node->get_undefined_behaviour_vars();
        NodeclSet empty_set;
        if(usage_kind._usage_type & Utils::UsageKind::USED)
        {
            NodeclSet ue_tmp; ue_tmp.insert(var);
            propagate_usage_to_ancestor(_node, ue_vars, killed_vars, undef_vars, empty_set,
                                         ue_tmp, empty_set, empty_set, empty_set);
            _node->set_ue_var(ue_vars);                   // Replace the set of upwards exposed variables associated to the node
        }
        else if(usage_kind._usage_type & Utils::UsageKind::DEFINED)
        {
            NodeclSet killed_tmp; killed_tmp.insert(var);
            propagate_usage_to_ancestor(_node, ue_vars, killed_vars, undef_vars, empty_set,
                                         empty_set, killed_tmp, empty_set, empty_set);
            _node->set_killed_var(killed_vars);               // Replace the set of killed vars associated to the node
        }
        else
        {
            NodeclSet undef_tmp; undef_tmp.insert(var);
            propagate_usage_to_ancestor(_node, ue_vars, killed_vars, undef_vars, empty_set,
                                         empty_set, empty_set, undef_tmp, empty_set);
            _node->set_undefined_behaviour_var(undef_vars);   // Replace the set of undefined behavior vars associated to the node
        }
    }

    void UsageVisitor::set_var_usage_to_node(const NodeclSet& var_set, Utils::UsageKind usage_kind)
    {
        for(NodeclSet::const_iterator it = var_set.begin(); it != var_set.end(); ++it)
            set_var_usage_to_node(*it, usage_kind);
    }

    void UsageVisitor::compute_statement_usage(NBase st)
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

    void UsageVisitor::unhandled_node(const NBase& n)
    {
        WARNING_MESSAGE("Unhandled node '%s' with type '%s' during Use-Def Analysis",
                        n.prettyprint().c_str(), ast_print_node_type(n.get_kind()));
    }

    void UsageVisitor::visit_assignment(const NBase& lhs, const NBase& rhs)
    {
        _define = false;
        walk(rhs);
        _define = true;
        walk(lhs);
        _define = false;
    }

    void UsageVisitor::visit_binary_assignment(const NBase& lhs, const NBase& rhs)
    {
        // Traverse the use of both the lhs and the rhs
        walk(rhs);
        walk(lhs);

        // Traverse the definition of the lhs
        _define = true;
        walk(lhs);
        _define = false;
    }
    
    void UsageVisitor::visit_function(const NBase& called_sym, const Nodecl::List& real_arguments)
    {
        if(_avoid_func_calls)
            return;

        // The function called must be analyzed only in case it has not been analyzed previously
        TL::Symbol func_sym = called_sym.get_symbol();
        Nodecl::List simplified_arguments = simplify_arguments(real_arguments);
        simplified_arguments = simplify_pointers(simplified_arguments);
        if(func_sym.is_valid())
        {   // The called function is not a pointer to function
            const ObjectList<TL::Symbol>& params = func_sym.get_function_parameters();
            if(_pcfgs.find(func_sym) != _pcfgs.end())
            {   // Due to the way we call the UseDef analysis, if the usage of the called function is not yet computed,
                // this means that it is a recursive call
                ExtensibleGraph* called_pcfg = _pcfgs[func_sym];
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
    
    void UsageVisitor::visit_increment(const NBase& rhs)
    {
        // Use of the rhs
        walk(rhs);
        
        // Definition of the rhs
        _define = true;
        NBase current_nodecl = _current_nodecl;
        _current_nodecl = NBase::null();
        walk(rhs);
        _current_nodecl = current_nodecl;
        _define = false;
    }
    
    void UsageVisitor::visit_vector_load(const NBase& rhs, const NBase& mask)
    {
        if(rhs.is<Nodecl::Reference>())
        {
            _node->add_used_address(rhs);
            walk(rhs.as<Nodecl::Reference>().get_rhs());
        }
        else
        {
            WARNING_MESSAGE("Unexpected node type '%s' in the RHS of a vector load. Reference expected." 
                            "Analysis result may be wrong", ast_print_node_type(rhs.get_kind()));
        }
        walk(mask);
    }
    
    void UsageVisitor::visit_vector_store(const NBase& lhs, const NBase& rhs, const NBase& mask)
    {
        if(lhs.is<Nodecl::Reference>())
        {
            _node->add_used_address(lhs);
            visit_assignment(lhs.as<Nodecl::Reference>().get_rhs(), rhs);
        }
        else
        {
            WARNING_MESSAGE("Unexpected node type '%s' in the LHS of a vector store. Reference expected." 
                            "Analysis result may be wrong", ast_print_node_type(lhs.get_kind()));
        }
        walk(mask);
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
        NBase current_nodecl = _current_nodecl;
        bool define = _define;

        // Use of the subscripts
        _define = false;
        _current_nodecl = NBase::null();
        walk(n.get_subscripts());

        // Use of the ArraySubscript
        _define = define;   // Just in case
        if(current_nodecl.is_null())
            _current_nodecl = n;
        else
            _current_nodecl = current_nodecl;
        walk(n.get_subscripted());
        _current_nodecl = NBase::null();
        
        _node->add_used_address(n.get_subscripted());
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

        _current_nodecl = NBase::null();
    }

    void UsageVisitor::visit(const Nodecl::Dereference& n)
    {
        NBase current_nodecl = _current_nodecl;
        bool define = _define;

        // Use of the Dereferenced variable
        _define = false;
        _current_nodecl = NBase::null();
        walk(n.get_rhs());

        // Use of the Dereference
        _define = define;
        _current_nodecl = n;
        walk(n.get_rhs());

        _current_nodecl = NBase::null();
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
        set_var_usage_to_node(n_sym, Utils::UsageKind::DEFINED);

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
        NBase rhs = n.get_rhs();
        
        // Insert the whole node to the list of accessed addresses
        _node->add_used_address(n);
        
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
        NBase var_in_use = n;
        if (!_current_nodecl.is_null())
            var_in_use = _current_nodecl;

        NodeclSet killed_vars = _node->get_killed_vars();
        NodeclSet undef_vars = _node->get_undefined_behaviour_vars();
        
        if (Utils::nodecl_set_contains_enclosing_nodecl(var_in_use, killed_vars).is_null() && 
            Utils::nodecl_set_contains_enclosing_nodecl(var_in_use, undef_vars).is_null())
        {
            Utils::UsageKind usage_kind = (_define ? Utils::UsageKind::DEFINED : Utils::UsageKind::USED);
            set_var_usage_to_node(var_in_use, usage_kind);
            store_ipa_information(var_in_use);
        }
        
        // The the usage of the symbol to the list of accessed addresses, if necessary
        if(!var_in_use.is<Nodecl::Reference>() && n.get_symbol().get_type().is_pointer())
            _node->add_used_address(n);
    }
    
    void UsageVisitor::visit(const Nodecl::VectorAssignment& n)
    {
        visit_assignment(n.get_lhs(), n.get_rhs());
    }

    // It is used: the base, the strides (if variables) and the memory positions formed by base+stride_i
    void UsageVisitor::visit(const Nodecl::VectorGather& n)
    {
        if(VERBOSE)
        {
            WARNING_MESSAGE("Usage of VectorGather nodes is not yet implemented. Analysis results may be wrong.\n", 0);
        }
//         NBase base = n.get_base();
//         NBase strides = n.get_strides();
// 
//         // Usage of the base
//         walk(base);
// 
//         if(strides.is<Nodecl::VectorLiteral>())
//         {
//             Nodecl::List stride_list = strides.as<Nodecl::VectorLiteral>().get_scalar_values().as<Nodecl::List>();
//             for(Nodecl::List::iterator it = stride_list.begin(); it != stride_list.end(); ++it)
//             {
//                 // Usage of base+stride_i
//                 Nodecl::Add current_access = Nodecl::Add::make(base.shallow_copy(), it->shallow_copy(), base.get_type(), it->get_locus());
//                 if(!Utils::nodecl_set_contains_nodecl(current_access, _node->get_killed_vars()))
//                     set_var_usage_to_node(current_access, Utils::UsageKind::USED);
//             }
//         }
//         else
//         {
//             // Usage of the stride
//             walk(strides);
// 
//             Nodecl::Add current_access = Nodecl::Add::make(base.shallow_copy(), strides.shallow_copy(), base.get_type(), strides.get_locus());
//             if(!Utils::nodecl_set_contains_nodecl(current_access, _node->get_killed_vars()))
//                 set_var_usage_to_node(current_access, Utils::UsageKind::USED);
//         }
    }
    
    void UsageVisitor::visit(const Nodecl::VectorLoad& n)
    {
        visit_vector_load(n.get_rhs(), n.get_mask());
    }
    
    void UsageVisitor::visit(const Nodecl::VectorMaskAssignment& n)
    {
        visit_assignment(n.get_lhs(), n.get_rhs());
    }

    // It is used: the strides (if variables). It is defined the memory positions formed by base+stride_i
    void UsageVisitor::visit(const Nodecl::VectorScatter& n)
    {
        if(VERBOSE)
        {
            WARNING_MESSAGE("Usage of VectorScatter nodes is not yet implemented. Analysis results may be wrong.\n", 0);
        }
//         NBase base = n.get_base();
//         NBase strides = n.get_strides();
//         NBase source = n.get_source();
// 
//         // Usage of source and base
//         walk(source);
//         walk(base);
// 
//         if(strides.is<Nodecl::VectorLiteral>())
//         {
//             Nodecl::List stride_list = strides.as<Nodecl::VectorLiteral>().get_scalar_values().as<Nodecl::List>();
//             for(Nodecl::List::iterator it = stride_list.begin(); it != stride_list.end(); ++it)
//             {
//                 // Usage of base+stride_i
//                 Nodecl::Add current_access = Nodecl::Add::make(base.shallow_copy(), it->shallow_copy(), base.get_type(), it->get_locus());
//                 if(!Utils::nodecl_set_contains_nodecl(current_access, _node->get_killed_vars()))
//                     set_var_usage_to_node(current_access, Utils::UsageKind::DEFINED);
//             }
//         }
//         else
//         {
//             // Usage of strides
//             walk(strides);
// 
//             Nodecl::Add current_access = Nodecl::Add::make(base.shallow_copy(), strides.shallow_copy(), base.get_type(), strides.get_locus());
//             if(!Utils::nodecl_set_contains_nodecl(current_access, _node->get_killed_vars()))
//                 set_var_usage_to_node(current_access, Utils::UsageKind::DEFINED);
//         }
    }

    void UsageVisitor::visit(const Nodecl::VectorSincos& n)
    {
        NBase source = n.get_source();
        NBase sin_ptr = n.get_sin_pointer();
        NBase cos_ptr = n.get_cos_pointer();

        walk(source);
        walk(sin_ptr);
        walk(cos_ptr);

        // Uses the source of the operation
        if(!Utils::nodecl_set_contains_nodecl(source, _node->get_killed_vars()))
            set_var_usage_to_node(source, Utils::UsageKind::USED);

        // Defines the memory pointed by the second and third parameters
        NBase sin_ptd = Nodecl::Dereference::make(sin_ptr.shallow_copy(), sin_ptr.get_type());
        if(!Utils::nodecl_set_contains_nodecl(sin_ptd, _node->get_killed_vars()))
            set_var_usage_to_node(sin_ptd, Utils::UsageKind::DEFINED);
        NBase cos_ptd = Nodecl::Dereference::make(cos_ptr.shallow_copy(), cos_ptr.get_type());
        if(!Utils::nodecl_set_contains_nodecl(cos_ptd, _node->get_killed_vars()))
            set_var_usage_to_node(cos_ptd, Utils::UsageKind::DEFINED);
    }

    void UsageVisitor::visit(const Nodecl::VectorStore& n)
    {
        visit_vector_store(n.get_lhs(), n.get_rhs(), n.get_mask());
    }

    void UsageVisitor::visit(const Nodecl::VirtualFunctionCall& n)
    {
        visit_function(n.get_called(), n.get_arguments().as<Nodecl::List>());
    }

}
}
