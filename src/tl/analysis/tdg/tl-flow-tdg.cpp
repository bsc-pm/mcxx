/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona Supe*rcomputing Center             *
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


#include "tl-task-dependency-graph.hpp"

namespace TL {
namespace Analysis {

    std::map<Node*, FTDGNode*> pcfg_to_ftdg;

namespace {
    FTDGNodeType get_tdg_type(Node* n)
    {
        if (n->is_ifelse_statement()) {
            return FTDGCondition;
        } else if (n->is_loop_node()) {
            return FTDGLoop;
        } else if (n->is_omp_async_target_node()) {
            return FTDGTarget;
        } else if (n->is_omp_task_node()) {
            return FTDGTask;
        } else if (n->is_omp_taskwait_node()) {
            return FTDGTaskwait;
        } else if (n->is_omp_barrier_graph_node()) {
            return FTDGBarrier;
        } else {
            std::string node_type;
            if (n->is_graph_node())
                node_type = n->get_graph_type_as_string();
            else
                node_type = n->get_type_as_string();
            internal_error("Unexpected node type %s.\n", node_type.c_str());
        }
    }

    void add_relation(
            FTDGNode* outer, FTDGNode* inner,
            bool conditional, bool true_edge)
    {
        if (!conditional || true_edge)
            outer->add_inner_true(inner);
        else
            outer->add_inner_false(inner);
        inner->add_outer(outer);
    }

    unsigned current_nesting_level = 0;
    unsigned ftdg_node_id = 1;

    // Since predecessors may be sequentially after their descendants,
    // we set this relationships at the end, when all nodes have already been created
    std::multimap<FTDGNode*, Node*> predecessors_map;
    void connect_missing_predecessors()
    {
        for (std::multimap<FTDGNode*, Node*>::iterator it = predecessors_map.begin();
             it != predecessors_map.end(); ++it)
        {
            it->first->add_predecessor(pcfg_to_ftdg[it->second]);
        }
    }
}

    FlowTaskDependencyGraph::FlowTaskDependencyGraph(ExtensibleGraph* pcfg)
        : _pcfg(pcfg), _parents(), _outermost_nodes()
    {
        std::stack<Node*> parent; // nesting level 0 => empty list of parents
        _parents.push_back(NULL);
        build_siblings_flow_tdg(_pcfg->get_graph(), parent);
        ExtensibleGraph::clear_visits(_pcfg->get_graph());
    }

    void FlowTaskDependencyGraph::build_siblings_flow_tdg(Node* n, std::stack<Node*> parent)
    {
        std::vector<FTDGNode*> control;
        std::vector<FTDGNode*> new_set_of_outermost;
        _outermost_nodes.push_back(new_set_of_outermost);
        build_flow_tdg_rec(n, parent, control, /*conditional*/ false, /*true_edge*/ false);
        connect_missing_predecessors();
    }

    /**
     * @param conditional Traversal is within a conditional branch
     * @param true_edge Traversal is within the true/false edge of a conditional branch
     */
    void FlowTaskDependencyGraph::build_flow_tdg_rec(
            Node* n, std::stack<Node*> parent,
            std::vector<FTDGNode*>& control,
            bool conditional, bool true_edge)
    {
        if (n->is_visited())
            return;
        n->set_visited(true);

        if (n->is_omp_task_node())
        {
            // Recursively generate the TDG inside the current task, if necessary
            ObjectList<Node*> nested_tasks;
            if (ExtensibleGraph::node_contains_tasks(n, n, nested_tasks))
            {
                parent.push(n);
                unsigned old_nesting_level = current_nesting_level++;
                build_siblings_flow_tdg(n->get_graph_entry_node(), parent);
                current_nesting_level = old_nesting_level;
            }

            // Do not follow synchronization edges to preserve the sequential order
            return;
        }
        else if (n->is_ifelse_statement())
        {
            FTDGNode* tdgcs = new FTDGNode(ftdg_node_id++, n, get_tdg_type(n));

            // Connect with previous control structure
            if (!control.empty())
                add_relation(control.back(), tdgcs, conditional, true_edge);
            else
                _outermost_nodes[current_nesting_level].push_back(tdgcs);

            control.push_back(tdgcs);

            n->get_graph_entry_node()->set_visited(true);
            Node* cond = n->get_condition_node();
            cond->set_visited(true);

            const ObjectList<Edge*>& exits = cond->get_exit_edges();
            for (ObjectList<Edge*>::const_iterator it = exits.begin(); it != exits.end(); ++it)
            {
                build_flow_tdg_rec((*it)->get_target(), parent, control,
                                   /*conditional*/ true, /*true_edge*/ (*it)->is_true_edge());
            }

            control.pop_back();
        }
        else if (n->is_loop_node())
        {
            FTDGNode* tdgcs = new FTDGNode(ftdg_node_id++, n, get_tdg_type(n));

            // Connect with previous control structure
            if (!control.empty())
                add_relation(control.back(), tdgcs, conditional, true_edge);
            else
                _outermost_nodes[current_nesting_level].push_back(tdgcs);

            control.push_back(tdgcs);
            build_flow_tdg_rec(n->get_graph_entry_node(), parent, control, conditional, true_edge);
            control.pop_back();
        }
        else if (n->is_graph_node() && !n->is_omp_barrier_graph_node())
        {
            build_flow_tdg_rec(n->get_graph_entry_node(), parent, control, conditional, true_edge);
        }
        else
        {
            FTDGNode* tdgn = NULL;
            Node* tdgn_related_n = NULL;
            // Create task nodes when the task creation is found to preserve the sequential order
            if (n->is_omp_task_creation_node())
            {
                Node* task = ExtensibleGraph::get_task_from_task_creation(n);
                tdgn = new FTDGNode(ftdg_node_id++, task, get_tdg_type(task));
                tdgn_related_n = task;
            }
            else if (n->is_omp_async_target_node()
                || n->is_omp_taskwait_node()
                || n->is_omp_barrier_graph_node())
            {
                tdgn = new FTDGNode(ftdg_node_id++, n, get_tdg_type(n));
                tdgn_related_n = n;
            }

            if (tdgn != NULL)
            {
                // Set nesting relationships
                if (!parent.empty())
                {
                    std::map<Node*, FTDGNode*>::iterator parent_ftdg_node_it = pcfg_to_ftdg.find(parent.top());
                    ERROR_CONDITION(parent_ftdg_node_it == pcfg_to_ftdg.end(),
                                    "No FTDG node found for PCFG node %d.\n",
                                    parent.top()->get_id());
                    tdgn->set_parent(parent_ftdg_node_it->second);
                    if (_parents.size() <= current_nesting_level)
                    {
                        _parents.push_back(parent_ftdg_node_it->second);
                    }
                }

                // Set control- and data-flow relationships with previous task/target/taskwait/barrier nodes
                ObjectList<Node*> n_parents = tdgn_related_n->get_parents();
                for (ObjectList<Node*>::iterator it = n_parents.begin();
                     it != n_parents.end(); ++it)
                {
                    // Only consider parents relevant to the TDG: tasks, targets, taskwaits and barriers
                    if (!(*it)->is_omp_task_node() && !(*it)->is_omp_taskwait_node()
                        && !(*it)->is_omp_async_target_node() && !(*it)->is_omp_barrier_graph_node())
                        continue;

                    // Add the node as a predecessor
                    predecessors_map.insert(std::pair<FTDGNode*, Node*>(tdgn, *it));

                    // If the predecessor is a task/target, and it contains other tasks that are synchronized within the task,
                    // then the synchronization must be added as a predecessor too
                    if ((*it)->is_omp_task_node() || (*it)->is_omp_async_target_node())
                    {
                        ObjectList<Node*> nested_tasks;
                        if (!ExtensibleGraph::node_contains_tasks(*it, *it, nested_tasks))
                            continue;

                        for (ObjectList<Node*>::iterator itn = nested_tasks.begin();
                             itn != nested_tasks.end(); ++itn)
                        {
                            ObjectList<Node*> children = ExtensibleGraph::get_task_from_task_creation(*itn)->get_children();
                            for (ObjectList<Node*>::iterator itc = children.begin();
                                    itc != children.end(); ++itc)
                            {
                                if ((*itc)->is_omp_taskwait_node() || (*itc)->is_omp_barrier_graph_node())
                                {   // Add the synchronization as a predecessor
                                    predecessors_map.insert(std::pair<FTDGNode*, Node*>(tdgn, *itc));
                                }
                            }
                        }
                    }
                }

                // Set control-flow relationships between the recently created simple node
                // and the control structures it belongs to
                if (!control.empty())
                    add_relation(control.back(), tdgn, conditional, true_edge);
                else
                    _outermost_nodes[current_nesting_level].push_back(tdgn);
            }
        }

        const ObjectList<Node*>& children = n->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin();
             it != children.end(); ++it)
        {
            if (!parent.empty() &&
                !ExtensibleGraph::node_contains_node(parent.top(), *it))
                parent.pop();
            build_flow_tdg_rec(*it, parent, control, conditional, true_edge);
        }
    }

    ExtensibleGraph* FlowTaskDependencyGraph::get_pcfg() const
    {
        return _pcfg;
    }

    const std::vector<FTDGNode*>& FlowTaskDependencyGraph::get_parents() const
    {
        return _parents;
    }

    const std::vector<std::vector<FTDGNode*> >& FlowTaskDependencyGraph::get_outermost_nodes() const
    {
        return _outermost_nodes;
    }
}
}