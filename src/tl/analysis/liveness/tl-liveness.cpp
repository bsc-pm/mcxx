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
#include "tl-liveness.hpp"
#include "tl-node.hpp"
#include "tl-task-sync.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // ******************************* Class implementing liveness analysis ******************************* //

    Liveness::Liveness(ExtensibleGraph* graph)
            : _graph(graph)
    {}

    void Liveness::compute_liveness()
    {
        // Compute graph concurrent tasks since this information is needed to
        // properly propagate liveness information over the graph
        TaskAnalysis::TaskConcurrency tc(_graph);
        tc.compute_tasks_concurrency();
        
        Node* graph = _graph->get_graph();

        // Compute initial info (liveness only regarding the current node => UE vars)
        gather_live_initial_information(graph);
        ExtensibleGraph::clear_visits(graph);

        // Common Liveness analysis
        solve_live_equations(graph);
        ExtensibleGraph::clear_visits(graph);
    }

    void Liveness::gather_live_initial_information(Node* current)
    {
        if(current->is_visited())
            return;

        current->set_visited(true);

        if(current->is_exit_node())
            return;

        if (current->is_graph_node())
        {
            gather_live_initial_information(current->get_graph_entry_node());
            set_graph_node_liveness(current, NULL);
        }
        else if (!current->is_entry_node())
        {
            current->set_live_in(current->get_ue_vars());
        }

        const ObjectList<Edge*>& exit_edges = current->get_exit_edges();
        for (ObjectList<Edge*>::const_iterator it = exit_edges.begin(); it != exit_edges.end(); ++it)
        {
            gather_live_initial_information((*it)->get_target());
        }
    }

    void Liveness::solve_live_equations(Node* current)
    {
        bool changed = true;
        while(changed)
        {
            changed = false;
            solve_live_equations_rec(current, changed, NULL);
            ExtensibleGraph::clear_visits(current);
        }
    }

    void Liveness::solve_live_equations_rec(Node* current, bool& changed, Node* container_task)
    {
        if (current->is_visited())
            return;

        current->set_visited(true);

        if(current->is_exit_node())
            return;

        if(current->is_omp_task_node())
        {
            solve_live_equations_rec(current->get_graph_entry_node(), changed, current);
            set_graph_node_liveness(current, container_task);
        }
        else if (current->is_graph_node())
        {
            solve_live_equations_rec(current->get_graph_entry_node(), changed, container_task);
            set_graph_node_liveness(current, container_task);
        }
        else if (!current->is_entry_node())
        {
            const NodeclSet& old_live_in = current->get_live_in_vars();
            const NodeclSet& old_live_out = current->get_live_out_vars();

            // Computing Live Out
            const NodeclSet& live_out = compute_live_out(current, container_task);

            // Computing Live In
            const NodeclSet& live_in = Utils::nodecl_set_union(current->get_ue_vars(),
                    Utils::nodecl_set_difference(live_out, current->get_killed_vars()));

            if (!Utils::nodecl_set_equivalence(old_live_in, live_in) ||
                !Utils::nodecl_set_equivalence(old_live_out, live_out))
            {
                current->set_live_in(live_in);
                current->set_live_out(live_out);
                changed = true;
            }
        }

        const ObjectList<Node*>& children = current->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
        {
            solve_live_equations_rec(*it, changed, container_task);
        }
    }

    NodeclSet Liveness::compute_live_out(Node* current, Node* container_task)
    {
        NodeclSet live_out, succ_live_in;

        const ObjectList<Node*>& children = current->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
        {
            bool child_is_exit = (*it)->is_exit_node();
            if (child_is_exit)
            {
                // Iterate over outer children while we found an EXIT node
                Node* exit_outer_node = (*it)->get_outer_node();
                ObjectList<Node*> outer_children;
                while (child_is_exit)
                {
                    outer_children = exit_outer_node->get_children();
                    child_is_exit = (outer_children.size() == 1) && outer_children[0]->is_exit_node();
                    exit_outer_node = (child_is_exit ? outer_children[0]->get_outer_node() : NULL);
                }
                // Get the Live in of the current successors
                for(ObjectList<Node*>::iterator itoc = outer_children.begin(); itoc != outer_children.end(); ++itoc)
                {
                    const NodeclSet& outer_live_in = (*itoc)->get_live_in_vars();
                    succ_live_in.insert(outer_live_in.begin(), outer_live_in.end());
                }
            }
            else
            {
                succ_live_in = (*it)->get_live_in_vars();
            }

            if(container_task != NULL)
            {   // remove those variables that are killed in any task that runs concurrently
                const ObjectList<Node*>& concurrent_tasks = _graph->get_task_concurrent_tasks(container_task);
                for(ObjectList<Node*>::const_iterator itc = concurrent_tasks.begin(); itc != concurrent_tasks.end(); ++itc)
                {
                    const NodeclSet& task_killed_vars = (*itc)->get_killed_vars();
                    for(NodeclSet::const_iterator itk = task_killed_vars.begin(); itk != task_killed_vars.end(); ++itk)
                    {
                        succ_live_in.erase(*itk);
                    }
                }
            }
            else if(current->is_omp_task_creation_node())
            {   // remove those variables that are killed in the task I create
                Node* created_task = current->get_children()[0];
                const NodeclSet& task_killed_vars = created_task->get_killed_vars();
                for(NodeclSet::const_iterator itk = task_killed_vars.begin(); itk != task_killed_vars.end(); ++itk)
                {
                    succ_live_in.erase(*itk);
                }
            }
            live_out = Utils::nodecl_set_union(live_out, succ_live_in);
        }

        return live_out;
    }

    void Liveness::set_graph_node_liveness(Node* current, Node* container_task)
    {
        if (current->is_graph_node())
        {
            // 1.- LI(graph) = U LI(inner entries)
            NodeclSet live_in;
            const ObjectList<Node*>& entries = current->get_graph_entry_node()->get_children();
            for (ObjectList<Node*>::const_iterator it = entries.begin(); it != entries.end(); ++it)
            {
                live_in = Utils::nodecl_set_union(live_in, (*it)->get_live_in_vars());
            }
            // 1.1.- Delete those variables which are local to the graph
            if (current->is_context_node())
            {   // Variables declared within the current context
                Scope sc(current->get_graph_related_ast().retrieve_context());
                NodeclSet graph_li;
                for(NodeclSet::iterator it = live_in.begin(); it != live_in.end(); ++it)
                {
                    const NBase& it_base = Utils::get_nodecl_base(*it);
                    if (!it_base.retrieve_context().scope_is_enclosed_by(sc))
                        graph_li.insert(*it);
                }
                current->set_live_in(graph_li);
            }
            else if (current->is_omp_task_node())
            {   // Variables private to the task
                NodeclSet graph_li;
                NodeclSet p_vars = current->get_private_vars();
                for (NodeclSet::iterator it = live_in.begin(); it != live_in.end(); ++it)
                {
                    if (p_vars.find(*it) == p_vars.end())
                        graph_li.insert(*it);
                }
                current->set_live_in(graph_li);
            }
            else
            {
                current->set_live_in(live_in);
            }

            // 2.- LO(graph) = U LI(S), where S successor(graph)
            const NodeclSet& live_out = compute_live_out(current, container_task);
            // 2.1.- Delete those variables which are local to the graph
            if (current->is_omp_task_node())
            {   // Variables private or firstprivate to the task
                NodeclSet graph_lo;
                NodeclSet all_p_vars = current->get_all_private_vars();
                for (NodeclSet::iterator it = live_in.begin(); it != live_in.end(); ++it)
                {
                    if (all_p_vars.find(*it) == all_p_vars.end())
                        graph_lo.insert(*it);
                }
                current->set_live_out(graph_lo);
            }
            else
            {
                current->set_live_out(live_out);
            }
        }
    }

    // ***************************** END class implementing liveness analysis ***************************** //
    // **************************************************************************************************** //

}
}