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

    std::map<Node*, FTDGNode*> pcfg_to_tdg;

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
}

    FlowTaskDependencyGraph::FlowTaskDependencyGraph(ExtensibleGraph* pcfg)
        : _pcfg(pcfg), _outermost_nodes()
    {
        build_flow_tdg();
    }

    void FlowTaskDependencyGraph::build_flow_tdg()
    {
        std::vector<FTDGNode*> control;
        build_flow_tdg_rec(_pcfg->get_graph(), control, /*conditional*/ false, /*true_edge*/ false);
        ExtensibleGraph::clear_visits(_pcfg->get_graph());
    }

    /**
     * @param conditional Traversal is within a conditional branch
     * @param true_edge Traversal is within the true/false edge of a conditional branch
     */
    void FlowTaskDependencyGraph::build_flow_tdg_rec(
            Node* n, std::vector<FTDGNode*>& control,
            bool conditional, bool true_edge)
    {
        if (n->is_visited())
            return;

        n->set_visited(true);

        if (n->is_ifelse_statement())
        {
            FTDGNode* tdgcs = new FTDGNode(n, get_tdg_type(n));

            // Connect with previous control structure
            if (!control.empty())
                add_relation(control.back(), tdgcs, conditional, true_edge);
            else
                _outermost_nodes.push_back(tdgcs);

            control.push_back(tdgcs);

            n->get_graph_entry_node()->set_visited(true);
            Node* cond = n->get_condition_node();
            cond->set_visited(true);

            const ObjectList<Edge*>& exits = cond->get_exit_edges();
            for (ObjectList<Edge*>::const_iterator it = exits.begin(); it != exits.end(); ++it)
            {
                build_flow_tdg_rec((*it)->get_target(), control,
                                   /*conditional*/ true, /*true_edge*/ (*it)->is_true_edge());
            }

            control.pop_back();
        }
        else if (n->is_loop_node())
        {
            FTDGNode* tdgcs = new FTDGNode(n, get_tdg_type(n));

            // Connect with previous control structure
            if (!control.empty())
                add_relation(control.back(), tdgcs, conditional, true_edge);
            else
                _outermost_nodes.push_back(tdgcs);

            control.push_back(tdgcs);
            build_flow_tdg_rec(n->get_graph_entry_node(), control, conditional, true_edge);
            control.pop_back();
        }
        else if (n->is_graph_node())
        {
            build_flow_tdg_rec(n->get_graph_entry_node(), control, conditional, true_edge);
        }
        else
        {
            FTDGNode* tdgn = NULL;
            // Create task nodes when the task creation is found to preserve the sequential order
            if (n->is_omp_task_creation_node())
            {
                Node* task = ExtensibleGraph::get_task_from_task_creation(n);
                tdgn = new FTDGNode(task, get_tdg_type(task));
            }
            else if (n->is_omp_async_target_node()
                || n->is_omp_taskwait_node()
                || n->is_omp_barrier_graph_node())
            {
                tdgn = new FTDGNode(n, get_tdg_type(n));
            }

            if (tdgn != NULL)
            {
                if (!control.empty())
                    add_relation(control.back(), tdgn, conditional, true_edge);
                else
                    _outermost_nodes.push_back(tdgn);
            }
        }

        // Do not follow synchronization edges to preserve the sequential order
        if (n->is_omp_task_node())
            return;

        const ObjectList<Node*>& children = n->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin();
             it != children.end(); ++it)
        {
            build_flow_tdg_rec(*it, control, conditional, true_edge);
        }
    }

    ExtensibleGraph* FlowTaskDependencyGraph::get_pcfg() const
    {
        return _pcfg;
    }

    std::vector<FTDGNode*> FlowTaskDependencyGraph::get_outermost_nodes() const
    {
        return _outermost_nodes;
    }
}
}