/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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

#include "tl-extensible-graph.hpp"
#include "tl-task-syncs-utils.hpp"

namespace TL {
namespace Analysis {
namespace TaskAnalysis{
    
    bool data_reference_is_modified_between_nodes_rec(Node* source, Node* target, const NBase& var, bool& is_modified)
    {
        if(source == target)
            return true;
        
        if(source->is_visited_aux())
            return false;

        source->set_visited_aux(true);

        //Call recursively first, so we ensure we find the node where it is modified before we know it is modified
        if(source->is_graph_node())
            if (data_reference_is_modified_between_nodes_rec(source->get_graph_entry_node(), target, var, is_modified))
                return true;

        // Just check whether the @source node defines the variable if
        // we have not found before another node that defines it
        // and if we are not in a graph node, because we already checked it inner nodes
        if(!is_modified && !source->is_graph_node())
        {
            NodeclSet killed_vars;
            if(source->is_omp_task_creation_node())
            {   // Variables from non-task children nodes do not count here
                Node* created_task = ExtensibleGraph::get_task_from_task_creation(source);
                ERROR_CONDITION(created_task==NULL,
                                "Task created by task creation node %d not found.\n",
                                source->get_id());
                killed_vars = created_task->get_killed_vars();
            }
            else
            {
                killed_vars = source->get_killed_vars();
            }
            if(killed_vars.find(var) != killed_vars.end())
            {
                NodeclMap reach_defs_out = source->get_reaching_definitions_out();
                NodeclMap::iterator var_out_definition = reach_defs_out.find(var);
                ERROR_CONDITION(var_out_definition==reach_defs_out.end(),
                                "No RD_OUT found in node %d for variable %s, but it is in the list of KILLED variables.\n",
                                source->get_id(), var.prettyprint().c_str());
                if (!Nodecl::Utils::structurally_equal_nodecls(var, var_out_definition->second.first))
                {   // Avoid reporting a modification for expressions like var = var;
                    is_modified = true;
                }
            }
        }

        // Keep traversing the graph to check that this path drives to @target
        const ObjectList<Node*>& children = (source->is_exit_node() ? source->get_outer_node()->get_children()
                                                                    : source->get_children());
        for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
        {
            if (!(*it)->is_omp_task_node() && !(*it)->is_omp_async_target_node())
                if (data_reference_is_modified_between_nodes_rec(*it, target, var, is_modified))
                    return true;
        }

        return false;
    }

    bool data_reference_is_modified_between_nodes(Node* source, Node* target, const NBase& var)
    {
        bool is_modified = false;
        bool target_found = false;
        if (source == target)
        {   // This may happen when a task has dependencies with itself because it is enclosed in an iterative construct
            // In this case we have to call recursively with the children of @source
            const ObjectList<Node*>& source_children = source->get_children();
            for (ObjectList<Node*>::const_iterator it = source_children.begin();
                 it != source_children.end() && !target_found; ++it)
            {
                if (!(*it)->is_omp_task_node() && !(*it)->is_omp_async_target_node())
                {
                    target_found = target_found || data_reference_is_modified_between_nodes_rec(*it, target, var, is_modified);
                    ExtensibleGraph::clear_visits_aux(*it);
                }
            }
        }
        else
        {
            target_found = data_reference_is_modified_between_nodes_rec(source, target, var, is_modified);
            ExtensibleGraph::clear_visits_aux(source);
        }

        ERROR_CONDITION(!target_found,
                        "Unable to find path between %d and %d.\n",
                        source->get_id(), target->get_id());

        return is_modified;
    }

    bool data_reference_is_modified_between_tasks(
            Node* source, 
            Node* target, 
            const Nodecl::NodeclBase& data_ref)
    {
        // Case 1: The variable is firstprivate in the source task
        //         => check the data_reference is modified between the creations of the source and the target tasks
        TL::Analysis::PCFGPragmaInfo source_pragma_info = source->get_pragma_node_info();
        TL::Analysis::PCFGPragmaInfo target_pragma_info = target->get_pragma_node_info();
        
        // Get the list of Firstprivate|Private variables of both source and target
        Nodecl::List source_vars_list;
        if (source_pragma_info.has_clause(NODECL_OPEN_M_P_FIRSTPRIVATE))
            source_vars_list = source_pragma_info.get_clause(NODECL_OPEN_M_P_FIRSTPRIVATE).as<Nodecl::OpenMP::Firstprivate>().get_symbols().shallow_copy().as<Nodecl::List>();
        if (source_pragma_info.has_clause(NODECL_OPEN_M_P_PRIVATE))
            source_vars_list.append(source_pragma_info.get_clause(NODECL_OPEN_M_P_PRIVATE).as<Nodecl::OpenMP::Private>().get_symbols().shallow_copy().as<Nodecl::List>());
        
        // Check whether the data reference is is that list
        if (!source_vars_list.empty())
        {
            NodeclSet source_vars_set(source_vars_list.begin(), source_vars_list.end());
            if (source_vars_set.find(data_ref) != source_vars_set.end())
            {
                Node* source_tc = ExtensibleGraph::get_task_creation_from_task(source);
                Node* target_tc = ExtensibleGraph::get_task_creation_from_task(target);
                return data_reference_is_modified_between_nodes(source_tc, target_tc, data_ref);
            }
        }
        
        // Case 2: Variable may be modified within the source task because the access is to the shared variable
        {   
            WARNING_MESSAGE("Data Reference %s is not firstprivate in %d. This is not yet implemented.\n", 
                            data_ref.prettyprint().c_str(), source->get_id());
            return false;
        }
    }
    
}
}
}
