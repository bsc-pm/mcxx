/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona* Supercomputing Center             **
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
#include "tl-auto-scope.hpp"

namespace TL {
namespace Analysis {

namespace {

    Utils::UsageKind compute_usage_in_region_rec(Node* current, NBase n, Node* region)
    {
        Utils::UsageKind result(Utils::UsageKind::NONE);
        
        if(!current->is_visited_aux() && ExtensibleGraph::node_contains_node(region, current))
        {
            current->set_visited_aux(true);
            
            if(!current->is_exit_node())
            {
                if(current->is_graph_node())
                {
                    result = compute_usage_in_region_rec(current->get_graph_entry_node(), n, region);
                }
                else
                {
                    const NodeclSet& undef = current->get_undefined_behaviour_vars();
                    if (Utils::nodecl_set_contains_nodecl(n, undef))
                        result = Utils::UsageKind::UNDEFINED;
                    const NodeclSet& ue = current->get_ue_vars();
                    if (Utils::nodecl_set_contains_nodecl(n, ue))
                        result = Utils::UsageKind::USED;
                    const NodeclSet& killed = current->get_killed_vars();
                    if (Utils::nodecl_set_contains_nodecl(n, killed))
                        result = Utils::UsageKind::DEFINED;
                }
                
                if(result._usage_type & Utils::UsageKind::UNDEFINED)
                {}   // Nothing else to be done because we will not be able to say anything about this variable
                else
                {
                    ObjectList<Node*> children = current->get_children();
                    for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                    {
                        result = result | compute_usage_in_region_rec(*it, n, region);
                    }
                }
            }
        }
        
        return result;
    }
    
    Utils::UsageKind compute_usage_in_region(const NBase& n, Node* region)
    {
        Node* region_entry = region->get_graph_entry_node();
        Utils::UsageKind result = compute_usage_in_region_rec(region_entry, n, region);
        ExtensibleGraph::clear_visits_aux_in_level(region_entry, region);
        return result;
    }
    
    Utils::UsageKind compute_usage_in_regions(const NBase& n, ObjectList<Node*> regions)
    {
        Utils::UsageKind result = Utils::UsageKind::NONE;
        
        for(ObjectList<Node*>::iterator it = regions.begin(); it != regions.end(); it++)
            result = result | compute_usage_in_region(n, *it);
        
        return result;
    }
    
    bool access_are_synchronous_rec(Node* current, const NBase& n, Node* region)
    {
        bool result = true;
        
        if(!current->is_visited_aux())
        {
            current->set_visited_aux(true);
            
            if(!current->is_exit_node())
            {
                if(current->is_graph_node())
                {
                    result = access_are_synchronous_rec(current->get_graph_entry_node(), n, region);
                }
                else
                {
                    const NodeclSet& ue_vars = current->get_ue_vars();
                    const NodeclSet& killed_vars = current->get_killed_vars();
                    if ((Utils::nodecl_set_contains_nodecl(n, ue_vars) || 
                        Utils::nodecl_set_contains_nodecl(n, killed_vars)) &&
                        !ExtensibleGraph::node_is_in_synchronous_construct(current))
                    {
                        result = false;
                    }
                }
                
                if(result)
                {
                    ObjectList<Node*> children = current->get_children();
                    for(ObjectList<Node*>::iterator it = children.begin(); (it != children.end()) && result; it++)
                        result = access_are_synchronous_rec(*it, n, region);
                }
            }
        }
        
        return result;
    }
    
    bool access_are_synchronous(const NBase& n, Node* region)
    {
        Node* region_entry = region->get_graph_entry_node();
        bool result = access_are_synchronous_rec(region_entry, n, region);
        ExtensibleGraph::clear_visits_aux_in_level(region_entry, region);
        return result;
    }
    
    bool access_are_synchronous(const NBase& n, ObjectList<Node*> regions)
    {
        bool result = true;
        for(ObjectList<Node*>::iterator it = regions.begin(); (it != regions.end()) && result; ++it)
            result = result && access_are_synchronous(n, *it);
        return result;
    }
    
}
    
    AutoScoping::AutoScoping(ExtensibleGraph* pcfg)
        : _graph(pcfg), _simultaneous_tasks(), _check_only_local(false)
    {}
    
    void AutoScoping::compute_auto_scoping()
    {
        ObjectList<Node*> tasks = _graph->get_tasks_list();
        for(ObjectList<Node*>::iterator it = tasks.begin(); it != tasks.end(); ++it)
            compute_task_auto_scoping(*it);
    }

    void AutoScoping::compute_task_auto_scoping(Node* task)
    {
        _simultaneous_tasks = _graph->get_task_concurrent_tasks(task);
        
        const ObjectList<Node*>& last_sync = _graph->get_task_last_sync_for_tasks(task);
        const ObjectList<Node*>& next_sync = _graph->get_task_next_sync_for_tasks(task);
        if (VERBOSE)
        {
            std::cerr << "    Task concurrent regions limits: \n";
            if (last_sync.empty())
                std::cerr << "        * Last sync not found" << std::endl;
            else
            {
                std::cerr << "        * Last sync:  ";
                for (ObjectList<Node*>::const_iterator it = last_sync.begin(); it != last_sync.end(); )
                {
                    std::cerr << (*it)->get_id();
                    ++it;
                    if (it != last_sync.end())
                        std::cerr << ", ";
                }
                std::cerr << std::endl;
            }
            if (next_sync.empty())
                std::cerr << "        * Next sync not found" << std::endl;
            else
            {
                std::cerr << "        * Next sync: ";
                for (ObjectList<Node*>::const_iterator it = next_sync.begin(); it != next_sync.end(); )
                {
                    std::cerr << (*it)->get_id();
                    ++it;
                    if (it != next_sync.end())
                        std::cerr << ", ";
                }
                std::cerr << std::endl;
            }
        }
        if (last_sync.empty() || (next_sync.empty()))
            _check_only_local = true;
        
        // Scope variables
        NodeclSet scoped_vars;
        Node* task_entry = task->get_graph_entry_node();
        compute_task_auto_scoping_rec(task, task_entry, scoped_vars);
        ExtensibleGraph::clear_visits(task_entry);
    }

    void AutoScoping::compute_task_auto_scoping_rec(Node* task, Node* current, NodeclSet& scoped_vars)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);

            if(current->is_graph_node())
            {
                compute_task_auto_scoping_rec(task, current->get_graph_entry_node(), scoped_vars);
            }
            else if(current->has_statements())
            {
                Scope sc(task->get_graph_related_ast().retrieve_context());

                NodeclSet& ue = current->get_ue_vars();
                for(NodeclSet::iterator it = ue.begin(); it != ue.end(); ++it)
                {
                    Symbol s(it->get_symbol());
                    if(s.is_valid() && !s.get_scope().scope_is_enclosed_by(sc))
                        scope_variable(task, Utils::UsageKind::USED, *it, scoped_vars);
                }

                const NodeclSet& killed = current->get_killed_vars();
                for(NodeclSet::const_iterator it = killed.begin(); it != killed.end(); ++it)
                {
                    Symbol s(it->get_symbol());
                    if(s.is_valid() && !s.get_scope().scope_is_enclosed_by(sc))
                        scope_variable(task, Utils::UsageKind::DEFINED, *it, scoped_vars);
                }
            }

            const ObjectList<Node*>& children = current->get_children();
            for(ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
                compute_task_auto_scoping_rec(task, *it, scoped_vars);
        }
    }
    
    void AutoScoping::scope_variable(Node* task, Utils::UsageKind usage, const NBase& n,
                                     NodeclSet& scoped_vars)
    {
        if(Utils::nodecl_set_contains_enclosing_nodecl(n, scoped_vars).is_null())
        {   // The expression is not a symbol local from the task
            scoped_vars.insert(n);

            Utils::UsageKind usage_in_concurrent_regions = compute_usage_in_regions(n, _simultaneous_tasks);
            Utils::UsageKind usage_in_task = compute_usage_in_region(n, task);
            
            if((usage_in_concurrent_regions._usage_type & Utils::UsageKind::UNDEFINED) || 
                (usage_in_task._usage_type & Utils::UsageKind::UNDEFINED))
            {
                task->set_sc_undef_var(n);
            }
            else if(usage_in_concurrent_regions._usage_type & Utils::UsageKind::NONE)
            {
                if(usage_in_task._usage_type & Utils::UsageKind::DEFINED)
                {
                    NodeclSet global_vars = _graph->get_global_variables();
                    if((global_vars.find(n) != global_vars.end()) || 
                        Utils::nodecl_set_contains_nodecl(n, task->get_live_out_vars()))
                    {
                        task->set_sc_shared_var(n);
                    }
                    else
                    {
                        if(usage._usage_type & Utils::UsageKind::DEFINED)
                            task->set_sc_private_var(n);
                        else
                            task->set_sc_firstprivate_var(n);
                    }
                }
                else
                {   // SHARED_OR_FIRSTPRIVATE
                    Type t = n.get_type();
                    if(t.is_scalar_type() || t.is_any_reference())
                        task->set_sc_firstprivate_var(n);
                    else
                        task->set_sc_shared_var(n);
                }
            }
            else if ((usage_in_concurrent_regions._usage_type & Utils::UsageKind::DEFINED) || 
                     ((usage_in_concurrent_regions._usage_type & Utils::UsageKind::USED) && 
                       usage._usage_type & Utils::UsageKind::DEFINED))
            {   // The variable is used in concurrent regions and at least one of the access is a write
                // Check for data race conditions
                if(access_are_synchronous(n, _simultaneous_tasks) && access_are_synchronous(n, task))
                {
                    task->set_sc_shared_var(n);
                }
                else
                {   // Avoid data race conditions by privatizing the variable
                    task->set_sc_private_var(n);
                }
            }
            else
            {   // Either the variable not used in the concurrent regions, or, if it is, all the access are read
                // SHARED_OR_FIRSTPRIVATE
                Type t = n.get_type();
                if(t.is_scalar_type() || t.is_any_reference())
                    task->set_sc_firstprivate_var(n);
                else
                    task->set_sc_shared_var(n);
            }
        }
    }
    
}
}
