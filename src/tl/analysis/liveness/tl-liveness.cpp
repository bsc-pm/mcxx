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

#include "tl-analysis-utils.hpp"
#include "tl-liveness.hpp"
#include "tl-node.hpp"
#include "tl-task-sync.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // ******************************* Class implementing liveness analysis ******************************* //

    Liveness::Liveness(ExtensibleGraph* graph, bool propagate_graph_nodes)
        : _graph(graph), _propagate_graph_nodes(propagate_graph_nodes)
    {}

    void Liveness::compute_liveness()
    {
        // Compute graph concurrent tasks since this information is needed to
        // properly propagate liveness information over the graph
        TaskAnalysis::TaskConcurrency tc(_graph);
        tc.compute_tasks_concurrency();
        
        Node* graph = _graph->get_graph();
        Node* exit = graph->get_graph_exit_node();

        // Compute initial info (liveness only regarding the current node => UE vars)
        initialize_live_sets(graph);
        Node* post_sync = _graph->get_post_sync();
        if (post_sync != NULL)
            initialize_live_sets(post_sync);
        // Note: 'n', which is the most outer node of the graph, must be cleaned up separatedly
        //       because clear_visits_backwards_in_level skips entering the first node it is called with
        //       in case it is a graph node
        ExtensibleGraph::clear_visits_backwards_in_level(exit, graph);
        if (post_sync != NULL)
            ExtensibleGraph::clear_visits_backwards_in_level(post_sync, graph);
        graph->set_visited(false);

        // Common Liveness analysis
        bool changed = true;
        while(changed)
        {
            changed = false;
            solve_live_equations_rec(graph, changed);
            if (post_sync != NULL)
                solve_live_equations_rec(post_sync, changed);
            ExtensibleGraph::clear_visits_backwards_in_level(exit, graph);
            if (post_sync != NULL)
                ExtensibleGraph::clear_visits_backwards_in_level(post_sync, graph);
            graph->set_visited(false);
        }
    }

    void Liveness::initialize_live_sets(Node* n)
    {
        if (n->is_visited())
            return;

        n->set_visited(true);

        if (n->is_entry_node())
            return;

        if (n->is_graph_node())
        {
            initialize_live_sets(n->get_graph_exit_node());
            if (_propagate_graph_nodes)
                set_graph_node_liveness(n);
        }
        else if (!n->is_exit_node())
        {
            n->set_live_in(n->get_ue_vars());
        }

        const ObjectList<Node*>& parents = n->get_parents();
        for (ObjectList<Node*>::const_iterator it = parents.begin(); it != parents.end(); ++it)
            initialize_live_sets(*it);
    }

    void Liveness::solve_live_equations_rec(Node* n, bool& changed)
    {
        if (n->is_visited())
            return;

        n->set_visited(true);

        if (n->is_entry_node())
            return;

        if (n->is_graph_node())
        {
            Node* exit = n->get_graph_exit_node();
            if (n->is_omp_task_node()
                || n->is_omp_async_target_node())
                solve_task_live_equations_rec(exit, changed, n);
            else
                solve_live_equations_rec(exit, changed);
            if (_propagate_graph_nodes)
                set_graph_node_liveness(n);
        }
        else if (!n->is_exit_node())
        {
            // 1.- Gather old liveness sets
            const NodeclSet& old_live_out = n->get_live_out_vars();
            const NodeclSet& old_live_in = n->get_live_in_vars();

            // 2.- Compute new liveness sets
            // 2.1.- Compute Live Out: LO(x) = U LI(y), forall y ∈ Succ(x)
            const NodeclSet& live_out = compute_successors_live_in(n);
            // 2.2.-Compute Live In: LI(x) = UE(x) U ( LO(x) - KILL(x) )
            const NodeclSet& live_in = Utils::nodecl_set_union(n->get_ue_vars(),
                    Utils::nodecl_set_difference(live_out, n->get_killed_vars()));

            // 3.- Compare the two sets to see whether something has changed and, if yes, set the new values
            if (!Utils::nodecl_set_equivalence(old_live_in, live_in)
                    || !Utils::nodecl_set_equivalence(old_live_out, live_out))
            {
                n->set_live_in(live_in);
                n->set_live_out(live_out);
                changed = true;
            }
        }

        const ObjectList<Node*>& parents = n->get_parents();
        for (ObjectList<Node*>::const_iterator it = parents.begin(); it != parents.end(); ++it)
            solve_live_equations_rec(*it, changed);
    }

    void Liveness::solve_task_live_equations_rec(Node* n, bool& changed, Node* task)
    {
        if (!n->is_exit_node())
            return;

        n->set_visited(true);

        // 1.- Compute the task successors LI set
        const ObjectList<Node*>& parents = n->get_parents();
        ERROR_CONDITION(parents.size()!=1,
                        "The number of parents of a task exit node must be 1 (a flush node), but %d found.\n",
                        parents.size());
        Node* exit_flush = parents[0];
        NodeclSet succ_live_in = compute_successors_live_in(exit_flush);
        // 1.2.- If the task has a post_sync successor, then all shared variables must be alive at the exit of the task
        if (ExtensibleGraph::task_synchronizes_in_post_sync(task))
        {
            const NodeclSet& shared_accesses = task->get_all_shared_accesses();
            succ_live_in.insert(shared_accesses.begin(), shared_accesses.end());
        }

        // 2.- Add to the list of successors, the flow successors of the Task Creation node of the current task
        Node* task_creation = ExtensibleGraph::get_task_creation_from_task(task);
        const ObjectList<Node*>& tc_children = task_creation->get_children();
        for (ObjectList<Node*>::const_iterator it = tc_children.begin(); it != tc_children.end(); ++it)
        {
            if (*it != task)
            {
                const NodeclSet& li = (*it)->get_live_in_vars();
                succ_live_in.insert(li.begin(), li.end());
            }
        }

        // 3.- Remove from the set of successors LI those variables private to the task
        const NodeclSet& private_vars = task->get_all_private_vars();
        for (NodeclSet::const_iterator it = private_vars.begin(); it != private_vars.end(); ++it)
            if (Utils::nodecl_set_contains_nodecl(*it, succ_live_in))
                succ_live_in.erase(*it);

        // 4.- Gather the old liveness sets
        const NodeclSet& old_live_out = exit_flush->get_live_out_vars();
        const NodeclSet& old_live_in = exit_flush->get_live_in_vars();

        // 5.- Compare the two sets to see whether something has changed and, if yes, set the new values
        if (!Utils::nodecl_set_equivalence(old_live_in, succ_live_in)
                || !Utils::nodecl_set_equivalence(old_live_out, succ_live_in))
        {
            exit_flush->set_live_out(succ_live_in);
            exit_flush->set_live_in(succ_live_in);
            changed = true;
        }

        // 6.- Keep iterating normally within the task
        exit_flush->set_visited(true);
        const ObjectList<Node*>& flush_parents = exit_flush->get_parents();
        for (ObjectList<Node*>::const_iterator it = flush_parents.begin(); it != flush_parents.end(); ++it)
            solve_live_equations_rec(*it, changed);
    }

    NodeclSet Liveness::compute_successors_live_in(Node* n)
    {
        NodeclSet succ_live_in;
        const ObjectList<Node*>& children = n->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
        {
            Node* c = *it;
            bool child_is_exit = c->is_exit_node();
            if (child_is_exit)
            {
                // Iterate over outer children while we found an EXIT node
                Node* exit_outer_node = c->get_outer_node();
                ObjectList<Node*> outer_children;
                while (child_is_exit)
                {
                    outer_children = exit_outer_node->get_children();
                    child_is_exit = (outer_children.size() == 1) && outer_children[0]->is_exit_node();
                    exit_outer_node = (child_is_exit ? outer_children[0]->get_outer_node() : NULL);
                }
                // Get the Live in of the current successors
                for (ObjectList<Node*>::iterator itoc = outer_children.begin(); itoc != outer_children.end(); ++itoc)
                {
                    const NodeclSet& outer_live_in = (*itoc)->get_live_in_vars();
                    succ_live_in.insert(outer_live_in.begin(), outer_live_in.end());
                }
            }
            else
            {
                if (!_propagate_graph_nodes && c->is_graph_node())
                {   // Gather the LiveIn variables of the graph
                    // 1.- Compute all LiveIn variables: LI(graph) = U LI(inner entries)
                    NodeclSet all_live_in;
                    const ObjectList<Node*>& grandchildren = c->get_graph_entry_node()->get_children();
                    for (ObjectList<Node*>::const_iterator itt = grandchildren.begin();
                         itt != grandchildren.end(); ++itt)
                    {
                        const NodeclSet& li = (*itt)->get_live_in_vars();
                        all_live_in.insert(li.begin(), li.end());
                    }
                    // 2.- Delete those variables which are local to the graph
                    if (c->is_context_node())
                    {   // Variables declared within the current context
                        Scope sc(c->get_graph_related_ast().retrieve_context());
                        for (NodeclSet::iterator itt = all_live_in.begin(); itt != all_live_in.end(); ++itt)
                        {
                            const NBase& it_base = Utils::get_nodecl_base(*itt);
                            if (!it_base.retrieve_context().scope_is_enclosed_by(sc))
                                succ_live_in.insert(*itt);
                        }
                    }
                    // FIXME We should include here any OpenMP|OmpSs node that may have private variables
                    else if (c->is_omp_task_node()
                            || c->is_omp_async_target_node()
                            || c->is_omp_sync_target_node())
                    {   // Variables private to the task
                        const NodeclSet& p_vars = c->get_private_vars();
                        for (NodeclSet::iterator itt = all_live_in.begin(); itt != all_live_in.end(); ++itt)
                        {
                            if (p_vars.find(*itt) == p_vars.end())
                                succ_live_in.insert(*itt);
                        }
                    }
                    else
                    {
                        succ_live_in.insert(all_live_in.begin(), all_live_in.end());
                    }
                }
                else
                {
                    const NodeclSet& li = c->get_live_in_vars();
                    succ_live_in.insert(li.begin(), li.end());
                }
            }
        }
        return succ_live_in;
    }

    void Liveness::set_graph_node_liveness(Node* n)
    {
        if (!n->is_graph_node())
            return;

        // 1.- LO(graph) = U L0(inner exits)
        NodeclSet live_out;
        const ObjectList<Node*>& parents = n->get_graph_exit_node()->get_parents();
        for (ObjectList<Node*>::const_iterator it = parents.begin(); it != parents.end(); ++it)
        {
            const NodeclSet& lo = (*it)->get_live_out_vars();
            live_out.insert(lo.begin(), lo.end());
        }
        n->set_live_out(live_out);

        // 2.- LI(graph) = U LI(inner entries)
        NodeclSet all_live_in;
        const ObjectList<Node*>& children = n->get_graph_entry_node()->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
        {
            const NodeclSet& li = (*it)->get_live_in_vars();
            all_live_in.insert(li.begin(), li.end());
        }
        // 2.1.- Delete those variables which are local to the graph
        NodeclSet live_in;
        if (n->is_context_node())
        {   // Variables declared within the current context
            Scope sc(n->get_graph_related_ast().retrieve_context());
            for(NodeclSet::iterator it = all_live_in.begin(); it != all_live_in.end(); ++it)
            {
                const NBase& it_base = Utils::get_nodecl_base(*it);
                if (!it_base.retrieve_context().scope_is_enclosed_by(sc))
                    live_in.insert(*it);
            }
        }
        else if (n->is_omp_task_node()
                || n->is_omp_async_target_node()
                || n->is_omp_sync_target_node())
        {   // Variables private to the task
            NodeclSet p_vars = n->get_private_vars();
            for (NodeclSet::iterator it = all_live_in.begin(); it != all_live_in.end(); ++it)
            {
                if (p_vars.find(*it) == p_vars.end())
                    live_in.insert(*it);
            }
        }
        else
        {
            live_in = all_live_in;
        }
        n->set_live_in(live_in);
    }

    // ***************************** END class implementing liveness analysis ***************************** //
    // **************************************************************************************************** //

}
}
