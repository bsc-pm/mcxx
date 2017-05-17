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


#include <algorithm>
#include <cassert>
#include <climits>
#include <list>
#include <queue>

#include "cxx-cexpr.h"
#include "tl-counters.hpp"
#include "tl-pcfg-utils.hpp"
#include "tl-task-dependency-graph.hpp"

namespace TL {
namespace Analysis {

    typedef ObjectList<Node*> Node_list;
    typedef ObjectList<Edge*> Edge_list;

    int tdg_node_id;

namespace {

    TL::Counter &control_id = TL::CounterManager::get_counter("tdg-control-id");

    //! Returns true when there is a path between 'current' and 'task'
    bool task_is_in_path(Node* control_structure, Node* current, Node* task)
    {
        if (current->is_visited())
            return false;

        current->set_visited(true);

        // Only traverse the nodes that are inside control_structure
        if (current == control_structure->get_graph_exit_node())
            return false;

        // Return true only when we find the task traversing the current path
        if (current->is_omp_task_node()
            || current->is_omp_async_target_node()
            || /*taskpart*/ (current->is_function_call_node()
                && ((current->get_statements()[0].as<Nodecl::FunctionCall>().get_called().get_symbol().get_name() == "GOMP_init_taskpart")
                    || (current->get_statements()[0].as<Nodecl::FunctionCall>().get_called().get_symbol().get_name() == "GOMP_end_taskpart"))))
        {
            if (current == task)
                return true;
            else
                return false;   // Return false because we do not want to traverse tasks depending on other tasks
                                // but only reach a task when traversing its corresponding task creation node
        }

        bool result = false;

        // Traverse the inner nodes if 'current' is a graph node
        if (current->is_graph_node())
            result = task_is_in_path(control_structure, current->get_graph_entry_node(), task);

        // Traverse the children
        const ObjectList<Node*>& children = current->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin();
             it != children.end() && !result; ++it)
            result = task_is_in_path(control_structure, *it, task);

        return result;
    }
    
    NBase get_condition_stmts(Node* cond_node)
    {
        NBase cond_stmt;
        
        if(cond_node->is_graph_node())
            cond_stmt = cond_node->get_graph_related_ast();
        else
        {
            NodeclList stmts = cond_node->get_statements();
            ERROR_CONDITION(stmts.size()!=1, "%s statements found in node %d condition. Only one statement expected.\n", 
                            stmts.size(), cond_node->get_id());
            cond_stmt = stmts[0];
        }
        
        return cond_stmt;
    }
    
    //! OldTaskDependencyGraph :: Returns a nodecl containing the condition that must fulfill 
    //! to follow the branch of an ifelse that takes to 'task'
    NBase get_ifelse_condition_and_path(
            Node* control_structure,
            Node* task,
            std::string& taken_branch)
    {
        NBase condition;

        // Get the statements that form the condition
        Node* cond_node = control_structure->get_condition_node();
        NBase cond_stmt = get_condition_stmts(cond_node);
        
        // Find which path (TRUE|FALSE) takes to the task and compute the condition accordingly
        ObjectList<Edge*> exit_edges = cond_node->get_exit_edges();
        for (ObjectList<Edge*>::iterator it = exit_edges.begin();
             it != exit_edges.end(); ++it)
        {
            if (task_is_in_path(control_structure, (*it)->get_target(), task))
            {
                condition = cond_stmt;
                taken_branch = ((*it)->is_true_edge() ? "1" : "2");
                
                break;  // Stop iterating, for we already found the task
            }
        }

        // Clean up the graph from the visits
        const ObjectList<Node*>& children = cond_node->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin();
            it != children.end(); ++it)
            ExtensibleGraph::clear_visits_in_level(*it, control_structure);

        return condition;
    }
}

    
    // ******************************************************************* //
    // ********************** Task Dependency Graph ********************** //

    OldTaskDependencyGraph::OldTaskDependencyGraph(
            ExtensibleGraph* pcfg,
            std::string json_name,
            bool taskparts_enabled)
        : _id(UINT_MAX), _pcfg(pcfg), _json_name(json_name),
          _tdg_nodes(), _syms(), _pcfg_to_cs_map(),
          _taskparts_enabled(taskparts_enabled)
    {
        Node* pcfg_node = _pcfg->get_graph();
        tdg_node_id = 0;

        // 1.- Get the identifier of the graph (from the call to GOMP_set_tdg_id inserted previously)
        //     Only graphs with tasks are accepted, and those graphs must have the following structures
        Node* pcfg_function_code = pcfg_node->get_graph_entry_node()->get_children()[0];
        Node* pcfg_ctx = pcfg_function_code->get_graph_entry_node()->get_children()[0];
        Node* pcfg_first_node = pcfg_ctx->get_graph_entry_node()->get_children()[0];
        ERROR_CONDITION(!pcfg_first_node->is_function_call_graph_node(),
                        "The first node of a PCFG containing tasks must be a Function Call. Instead, we found a %s.\n",
                        pcfg_first_node->is_graph_node() ? pcfg_first_node->get_graph_type_as_string().c_str()
                                                            : pcfg_first_node->get_type_as_string().c_str());
        Node* func_call_node = pcfg_first_node->get_graph_entry_node()->get_children()[0];
        Nodecl::FunctionCall func_call_nodecl = func_call_node->get_statements()[0].as<Nodecl::FunctionCall>();
        Symbol func_call_sym = func_call_nodecl.get_called().get_symbol();
        ERROR_CONDITION(func_call_sym.get_name() != "gomp_set_tdg",
                        "The first node of a PCFG containing tasks must be a call to gomp_set_tdg", 0);
        Nodecl::List args = func_call_nodecl.get_arguments().as<Nodecl::List>();
        _id = const_value_cast_to_unsigned_int(args[0].as<Nodecl::IntegerLiteral>().get_constant());

        // 2.- Build the nodes from the TDG
        create_tdg_task_nodes_from_pcfg(pcfg_node);
        ExtensibleGraph::clear_visits(pcfg_node);
        create_tdg_nodes_from_pcfg(pcfg_node);
        ExtensibleGraph::clear_visits(pcfg_node);
        if (_taskparts_enabled)
        {
            create_tdg_nodes_from_taskparts(pcfg_node);
            ExtensibleGraph::clear_visits(pcfg_node);
        }

        // 3.- Compute the data structures that will wrap the nodes
        set_tdg_nodes_control_structures();

        // 4.- Connect the tasks in the TDG
        connect_tdg_nodes_from_pcfg(pcfg_node);
    }

    std::string OldTaskDependencyGraph::get_name() const
    {
        return _json_name;
    }

    void OldTaskDependencyGraph::connect_tdg_nodes(
            TDG_Node* parent, TDG_Node* child,
            SyncKind sync_type, const NBase& condition)
    {
        TDG_Edge* edge = new TDG_Edge(parent, child, sync_type, condition);
        parent->_exits.insert(edge);
        child->_entries.insert(edge);
    }

    ExtensibleGraph* OldTaskDependencyGraph::get_pcfg() const
    {
        return _pcfg;
    }

    unsigned int OldTaskDependencyGraph::get_id() const
    {
        return _id;
    }

    TDG_Node* OldTaskDependencyGraph::find_tdg_node_from_pcfg_node(Node* n)
    {
        TDG_Node* result = NULL;
        for (TDG_Node_map::iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); ++it)
        {
            if (it->second->_pcfg_node == n)
            {
                result = it->second;
                break;
            }
        }
        ERROR_CONDITION(result==NULL,
                        "PCFG node with id '%d' not found in the TDG",
                        n->get_id());
        return result;
    }

    void OldTaskDependencyGraph::create_tdg_node(
            Node* current,
            int id,
            TDGNodeType type,
            Node* init_tp)
    {
        // Avoid duplicating identifiers
        // This may happen when multiple directives with a BarrierAtEnd are consecutive
        while (_tdg_nodes.find(id) != _tdg_nodes.end())
            ++id;

        // Get the parent task
        Node* parent = current->get_outer_node();
        while (parent != _pcfg->get_graph()
            && !(parent->is_omp_task_node() || parent->is_omp_async_target_node()))
            parent = parent->get_outer_node();

        // Create the TDG node
        TDG_Node* tdg_current = new TDG_Node(current, type, parent, init_tp);

        // Insert the node in the TDG
        _tdg_nodes.insert(std::pair<unsigned int, TDG_Node*>(id, tdg_current));
    }

    void OldTaskDependencyGraph::create_tdg_task_nodes_from_pcfg(Node* current)
    {
        // 1.- Base case: the node has been visited
        if (current->is_visited())
            return;
        current->set_visited(true);

        // 2.- Case 1: the node is a graph -> call recursively with inner nodes
        if (current->is_graph_node())
            create_tdg_task_nodes_from_pcfg(current->get_graph_entry_node());
        //
        // 3.- Case 2: the node is a task|target|taskwait|barrier -> create the TDG node
        // The identifier needs to be ordered as the node appears in the source code
        // due to boxer requirements (to avoid backward dependencies during expansion)
        int id;
        // Create the TDG node from the PCFG node
        if (current->is_omp_task_node())
        {
            id = current->get_graph_related_ast().get_line();
            create_tdg_node(current, id, Task);
            return; // Do not follow synchronization edges after the task
        }

        // 4.- Iterate over the children
        Node_list children = current->get_children();
        for (Node_list::iterator it = children.begin(); it != children.end(); ++it)
            create_tdg_task_nodes_from_pcfg(*it);
    }

    void OldTaskDependencyGraph::create_tdg_nodes_from_pcfg(Node* current)
    {
        // 1.- Base case: the node has been visited
        if (current->is_visited())
            return;
        current->set_visited(true);

        // 2.- Case 1: the node is a graph -> call recursively with inner nodes
        if (current->is_graph_node())
            create_tdg_nodes_from_pcfg(current->get_graph_entry_node());

        // 3.- Case 2: the node is a task|target|taskwait|barrier -> create the TDG node
        // The identifier needs to be ordered as the node appears in the source code
        // due to boxer requirements (to avoid backward dependencies during expansion)
        int id;
        TDGNodeType type = Unknown;
        // Create the TDG node from the PCFG node
        if (current->is_omp_task_node())
        {   // Nothing to do: these  nodes are treated separately
//             id = current->get_graph_related_ast().get_line();
//             type = Task;
        }
        else if (current->is_omp_async_target_node())
        {
            id = current->get_graph_related_ast().get_line();
            type = Target;
        }
        else if (current->is_omp_sync_target_node())
        {
            internal_error("Psocrates project does not support undeferred tasks.\n", 0);
        }
        else if (current->is_omp_taskwait_node())
        {
            id = current->get_statements()[0].get_line();
            type = Taskwait;
        }
        else if (current->is_omp_barrier_graph_node())
        {   // Note that the Graph Barrier Node need no traversal
            // If the barrier is implicit in a parallel or any other construct,
            // then the identifier must not be the line, but the end of the associated statement, instead
            const NBase& barrier_ast = current->get_graph_related_ast();
            if (barrier_ast.is<Nodecl::OpenMP::BarrierAtEnd>())
            {   // The barrier is implicit
                const Nodecl::List& environ = barrier_ast.get_parent().as<Nodecl::List>();
                // iterate over the environ members until it is not a list
                NBase directive = environ.get_parent();
                while (directive.is<Nodecl::List>()) {
                    directive = directive.get_parent();
                }
                if (!directive.is<Nodecl::OpenMP::Parallel>()
                        && !directive.is<Nodecl::OpenMP::For>()
                        && !directive.is<Nodecl::OpenMP::Sections>()
                        && !directive.is<Nodecl::OpenMP::Single>())
                {
                    internal_error("Unexpected node kind '%s' with an implicit barrier at end.\n",
                                    ast_print_node_type(directive.get_kind()));
                }
                else
                {
                    NBase parent = directive.get_parent();
                    if (parent.is_null())
                    {   // The directive is the last in the list, we report the line of the directive
                        if (TDG_DEBUG)
                        {
                            WARNING_MESSAGE("Found barrier node implicit to directive in line %d. "
                                            "We are not able to compute where the directive ends.\n"
                                            "Reporting barrier line as the directive line.\n", directive.get_line());
                        }
                        id = barrier_ast.get_line();
                    }
                    else
                    {
                        // Artificial statements have a default locus (line is always 0)
                        // Navigate over the statements until we find one that comes from the source code
                        while (!parent.is_null()
                            && parent.get_line() == 0)
                        {
                            parent = parent.get_parent();
                        }
                        id = parent.get_line();
                    }
                }
            }
            else
            {   // This is a BarrierFull
                id = barrier_ast.get_line();
            }
            type = Barrier;
        }

        if (type != Unknown)
        {
            create_tdg_node(current, id, type);
        }

        // 4.- Iterate over the children
        Node_list children = current->get_children();
        for (Node_list::iterator it = children.begin(); it != children.end(); ++it)
            create_tdg_nodes_from_pcfg(*it);
    }

namespace {
    void get_end_taskparts_rec(Node* n, std::list<Node*>& end_taskparts)
    {
        // 1.- Base cases
        // 1.1.- The node has already been visited: skip it
        if (n->is_visited_aux())
            return;
        n->set_visited_aux(true);
        // 1.2.- The node is a graph: traverse the inner nodes
        if (n->is_graph_node())
            get_end_taskparts_rec(n->get_graph_entry_node(), end_taskparts);

        // 2.- Searched case
        if (n->is_function_call_node())
        {
            Nodecl::FunctionCall f = n->get_statements()[0].as<Nodecl::FunctionCall>();
            if (f.get_called().get_symbol().get_name() == "GOMP_end_taskpart")
            {
                end_taskparts.push_back(n);
                return;
            }
        }

        // 3.- Keep iterating
        // Since we do not traverse the PCFG from the beginning,
        // it may happen that we arrive in an exit node
        // without having visited its graph node
        ObjectList<Node*> children;
        if ((n->is_graph_node() && n->get_graph_exit_node()->is_visited_aux())
                || (!n->is_graph_node() && !n->is_exit_node()))
            children = n->get_children();
        else if (n->is_exit_node())
            children = n->get_outer_node()->get_children();
        for (ObjectList<Node*>::const_iterator it = children.begin();
             it != children.end(); ++it)
        {
            get_end_taskparts_rec(
                /*current*/ *it,
                /*result*/ end_taskparts);
        }
    }

    void get_end_taskparts(Node* n, std::list<Node*>& end_taskparts)
    {
        get_end_taskparts_rec(n, end_taskparts);
        ExtensibleGraph::clear_visits_aux(n);
    }
}

    void OldTaskDependencyGraph::create_tdg_nodes_from_taskparts(Node* current)
    {
        // 1.- Base case: the node has been visited
        if (current->is_visited())
            return;
        current->set_visited(true);

        // 2.- Case 1: the node is a graph -> call recursively with inner nodes
        if (current->is_graph_node())
            create_tdg_nodes_from_taskparts(current->get_graph_entry_node());

        // 3.- Case 2: the node is GOMP_init_taskpart
        //     -> create the TDG node for each possible taskpart
        if (current->is_function_call_node())
        {   // Check a possible task parts
            Nodecl::FunctionCall f = current->get_statements()[0].as<Nodecl::FunctionCall>();
            if (f.get_called().get_symbol().get_name() == "GOMP_init_taskpart")
            {
                // Create a TDG node for each reachable GOMP_end_taskpart
                std::list<Node*> end_taskparts;
                get_end_taskparts(current, end_taskparts);
                for (std::list<Node*>::iterator it = end_taskparts.begin();
                     it != end_taskparts.end(); ++it)
                {
                    // Artificial statements have a default locus (line is always 0)
                    // Navigate over the statements until we find one that comes from the source code
                    NBase parent = (*it)->get_statements()[0].get_parent();
                    while (!parent.is_null()
                        && parent.get_line() == 0)
                    {
                        parent = parent.get_parent();
                    }

                    create_tdg_node(*it, /*TDG node id*/ parent.get_line(),
                                    /*type*/ Taskpart, /*init_tp*/current);
                }
            }
        }

        // 4.- Iterate over the children
        Node_list children = current->get_children();
        for (Node_list::iterator it = children.begin(); it != children.end(); ++it)
            create_tdg_nodes_from_taskparts(*it);
    }

namespace {
    enum enclosing_cs_type {
        __undefined = 0,
        __init = 1,
        __end = 2,
        __none = 3
    };

    enclosing_cs_type get_enclosing_cs_type(Node* init_cs, Node* end_cs)
    {
        enclosing_cs_type enclosing_cs = __undefined;

        while (enclosing_cs == __undefined)
        {
            // Prepare the iteration by looking for the corresponding loop control structures
            while (init_cs != NULL && !init_cs->is_loop_node())
                init_cs = ExtensibleGraph::get_enclosing_control_structure(init_cs);
            while (end_cs != NULL && !end_cs->is_loop_node())
                end_cs = ExtensibleGraph::get_enclosing_control_structure(end_cs);

            // Check whether we have found a suitable case
            if ((init_cs == NULL && end_cs == NULL)
                || (init_cs == end_cs))
            {
                enclosing_cs = __none;
            }
            if (init_cs == NULL
                || (end_cs != NULL && ExtensibleGraph::node_contains_node(init_cs, end_cs)))
            {
                enclosing_cs = __init;
            }
            else if (end_cs == NULL
                || ( init_cs != NULL && ExtensibleGraph::node_contains_node(end_cs, init_cs)))
            {
                enclosing_cs = __end;
            }
        }

        return enclosing_cs;
    }
}

    // This method creates the control structures for simple nodes and
    // for those taskparts where the initial and end points are in the same control structure
    // and the initial point is found before the end point in a sequential order of the program
    void OldTaskDependencyGraph::create_control_structure_rec(
            CS_case cs_case,
            Node* init,
            Node* n,
            Node* control_structure,
            TDG_Node* tdg_node,
            ControlStructure* last_cs)
    {
        Node* init_cs = (init == NULL
                                ? NULL
                                : ExtensibleGraph::get_enclosing_control_structure(init));

        while (control_structure != NULL)
        {
            // 1.- Get control structure type and condition
            ControlStructureType cs_t;
            NBase condition;
            std::string taken_branch;
            if (control_structure->is_loop_node())
            {
                // get the type of the Control Structure
                cs_t = Loop;

                Node* next_control_structure = ExtensibleGraph::get_enclosing_control_structure(control_structure);
                switch (cs_case)
                {
                    case same_cs_init_before_end:
                    {
//                         std::cerr << "  --> same_cs_init_before_end : "
//                                   << (init==NULL?0:init->get_id()) << " -> " << n->get_id() << std::endl;
                        // Get the condition of the loop
                        Node* cond = control_structure->get_condition_node();
                        assert(cond != NULL);
                        NodeclList stmts = cond->get_statements();
                        assert(stmts.size() == 1);
                        condition = stmts[0];
                        break;
                    }
                    case same_cs_end_before_init:
                    {
//                         std::cerr << "  --> same_cs_end_before_init : "
//                                   << (init==NULL?0:init->get_id()) << " -> " << n->get_id() << std::endl;
                        const Utils::InductionVarList& ivs = control_structure->get_induction_variables();
                        ERROR_CONDITION(ivs.size() != 1,
                                        "Psocrates does not support loops with more than one Induction Variable.\n",
                                        0);
                        const NBase iv = ivs[0]->get_variable();
                        condition =
                                Nodecl::LogicalAnd::make(
                                    Nodecl::GreaterThan::make(
                                        iv.shallow_copy(),
                                        ivs[0]->get_lb().begin()->shallow_copy(),
                                        iv.get_type()),
                                    Nodecl::LowerOrEqualThan::make(
                                        iv.shallow_copy(),
                                        ivs[0]->get_ub().begin()->shallow_copy(),
                                        iv.get_type()),
                                    iv.get_type()
                                );

                        // End being before init only matters for the most inner loop
                        cs_case = same_cs_init_before_end;
                        break;
                    }
                    case init_encloses_end_init_before_end:
                    {
//                         std::cerr << "  --> init_encloses_end_init_before_end : "
//                                   << (init==NULL?0:init->get_id()) << " -> " << n->get_id() << std::endl;
                        const Utils::InductionVarList& ivs = control_structure->get_induction_variables();
                        ERROR_CONDITION(ivs.size() != 1,
                                        "Psocrates does not support loops with more than one Induction Variable.\n",
                                        0);
                        condition =
                                Nodecl::Equal::make(
                                    ivs[0]->get_variable().shallow_copy(),
                                    ivs[0]->get_lb().begin()->shallow_copy(),
                                    ivs[0]->get_variable().get_type()
                                );

                        // Check the relation between the init control structure and the next end control structure
                        if (next_control_structure != NULL)
                        {
                            Node* next_cs = ExtensibleGraph::get_enclosing_control_structure(next_control_structure);
                            enclosing_cs_type enclosing_cs = get_enclosing_cs_type(init_cs, next_cs);
                            if (enclosing_cs == __none)
                                cs_case = same_cs_init_before_end;
                        }
                        break;
                    }
                    case init_encloses_end_end_before_init:
                    {
//                         std::cerr << "  --> init_encloses_end_end_before_init : "
//                                   << (init==NULL?0:init->get_id()) << " -> " << control_structure->get_id() << std::endl;
                        const Utils::InductionVarList& ivs = control_structure->get_induction_variables();
                        ERROR_CONDITION(ivs.size() != 1,
                                        "Psocrates does not support loops with more than one Induction Variable.\n",
                                        0);
                        condition =
                                Nodecl::Equal::make(
                                    ivs[0]->get_variable().shallow_copy(),
                                    ivs[0]->get_lb().begin()->shallow_copy(),
                                    ivs[0]->get_variable().get_type()
                                );

                        // Check the relation between the init control structure and the next end control structure
                        if (next_control_structure != NULL)
                        {
                            Node* next_cs = ExtensibleGraph::get_enclosing_control_structure(next_control_structure);
                            enclosing_cs_type enclosing_cs = get_enclosing_cs_type(init_cs, next_cs);
                            if (enclosing_cs == __none)
                                cs_case = same_cs_end_before_init;
                        }
                        break;
                    }
                    case end_encloses_init_init_before_end:
                    {
//                         std::cerr << "  --> end_encloses_init_init_before_end : "
//                                   << (init==NULL?0:init->get_id()) << " -> " << n->get_id() << std::endl;
                        const Utils::InductionVarList& ivs = control_structure->get_induction_variables();
                        ERROR_CONDITION(ivs.size() != 1,
                                        "Psocrates does not support loops with more than one Induction Variable.\n",
                                        0);
                        condition =
                                Nodecl::Equal::make(
                                    ivs[0]->get_variable().shallow_copy(),
                                    ivs[0]->get_ub().begin()->shallow_copy(),
                                    ivs[0]->get_variable().get_type()
                                );

                        // Check the relation between the init control structure and the next end control structure
                        if (next_control_structure != NULL)
                        {
                            Node* next_cs = ExtensibleGraph::get_enclosing_control_structure(next_control_structure);
                            enclosing_cs_type enclosing_cs = get_enclosing_cs_type(init_cs, next_cs);
                            if (enclosing_cs == __none)
                                cs_case = same_cs_init_before_end;
                        }
                        break;
                    }
                    case end_encloses_init_end_before_init:
                    {
//                         std::cerr << "  --> end_encloses_init_end_before_init : "
//                                   << (init==NULL?0:init->get_id()) << " -> " << n->get_id() << std::endl;
                        const Utils::InductionVarList& ivs = control_structure->get_induction_variables();
                        ERROR_CONDITION(ivs.size() != 1,
                                        "Psocrates does not support loops with more than one Induction Variable.\n",
                                        0);
                        condition =
                                Nodecl::Equal::make(
                                    ivs[0]->get_variable().shallow_copy(),
                                    ivs[0]->get_ub().begin()->shallow_copy(),
                                    ivs[0]->get_variable().get_type()
                                );

                        // Check the relation between the init control structure and the next end control structure
                        if (next_control_structure != NULL)
                        {
                            Node* next_cs = ExtensibleGraph::get_enclosing_control_structure(next_control_structure);
                            enclosing_cs_type enclosing_cs = get_enclosing_cs_type(init_cs, next_cs);
                            if (enclosing_cs == __none)
                                cs_case = same_cs_end_before_init;
                        }
                        break;
                    }
                    default:
                    {
                        internal_error("Unreachable code.\n", 0);
                    }
                }
            }
            else if (control_structure->is_ifelse_statement())
            {
                // get the type of the Control Structure
                cs_t = IfElse;

                // Check whether the statement is in the TRUE or the FALSE branch of the condition
                if (cs_case==end_encloses_init_init_before_end
                        || cs_case==end_encloses_init_end_before_init)
                {
                    condition = get_ifelse_condition_and_path(
                            control_structure,
                            init,
                            taken_branch);
                }
                else
                {
                    condition = get_ifelse_condition_and_path(
                            control_structure,
                            n,
                            taken_branch);
                }
            }
            else
            {
                internal_error("Unexpected node type %s when printing condition to TDG.\n"
                                "Expected Loop or IfElse.\n",
                                control_structure->get_type_as_string().c_str());
            }

            // 2.- Store the symbols involved in the condition in the list of used symbols in the graph
            ERROR_CONDITION(condition.is_null(),
                            "No condition has been computed for task %d in control structure %d.\n",
                            n->get_id(), control_structure->get_id());
            store_condition_list_of_symbols(condition, n->get_reaching_definitions_in());

            // 3.- Create a new control structure, if it did not exist yet
            ControlStructure* cs = NULL;
            std::pair<PCFG_to_CS::iterator, PCFG_to_CS::iterator> css =
                    _pcfg_to_cs_map.equal_range(std::pair<Node*, Node*>(control_structure, NULL));
            for (PCFG_to_CS::iterator it = css.first; it != css.second && cs == NULL; ++it)
            {
                const NBase& cond = it->second->get_condition();
                if (Nodecl::Utils::structurally_equal_nodecls(cond, condition, /*skip conversions*/true))
                {
                    cs = it->second;
                }
            }
            if (cs == NULL)
            {   // The control structure did not exist yet
                cs = new ControlStructure(++control_id, cs_t, condition, control_structure);
                _pcfg_to_cs_map.insert(std::pair<std::pair<Node*, Node*>, ControlStructure*>
                        (std::pair<Node*, Node*>(control_structure, NULL), cs));
            }

            last_cs->set_enclosing_cs(cs);
            tdg_node->add_control_structure(cs, taken_branch);

            // Prepare next iteration
            last_cs = cs;
            control_structure = ExtensibleGraph::get_enclosing_control_structure(control_structure);
        }
    }

    void OldTaskDependencyGraph::create_control_structures(
            TDG_Node* tdg_node,
            ControlStructure* last_cs)
    {
        // Case 1: the node is a taskpart => we must consider both the initial and the end points
        //                                   and the control structures involved with both nodes
        if (_taskparts_enabled && tdg_node->_type == Taskpart)
        {
            // The condition of the control structure depends on
            // the execution path differences between its initial node and its end node
            // Thus, the values of the variables to fulfill the condition will depend the next cases:
            //   * A. init is outside a loop and end is inside => loop lower bound
            //   * B. init is inside a loop and end is outside => loop upper bound + stride
            //   * C. init and end are inside a loop => same as for a regular node (lb-stride, ub+stride, stride)
            //   * D. init and end are outside any loop => there is no control structure

            Node* init = tdg_node->_init_taskpart;
            Node* end = tdg_node->_pcfg_node;
            Node* init_cs;
            Node* end_cs;

            // 1.- Check whether is the init or the end that is in a most outer level
            // regarding loop control structures
            init_cs = ExtensibleGraph::get_enclosing_control_structure(init);
            end_cs = ExtensibleGraph::get_enclosing_control_structure(end);
            enclosing_cs_type enclosing_cs = get_enclosing_cs_type(init_cs, end_cs);

            // 2.- Create the control structures involved in the existence of the taskpart
            init_cs = ExtensibleGraph::get_enclosing_control_structure(init);
            end_cs = ExtensibleGraph::get_enclosing_control_structure(end);
            if (enclosing_cs == __none)
            {
                if (ExtensibleGraph::node_is_ancestor_of_node(init, end))
                {
                    create_control_structure_rec(
                            same_cs_init_before_end, init, end, end_cs, tdg_node, last_cs);
                }
                else
                {
                    create_control_structure_rec(
                            same_cs_end_before_init, init, end, end_cs, tdg_node, last_cs);
                }
            }
            else if (enclosing_cs == __init)
            {
                if (ExtensibleGraph::node_is_ancestor_of_node(init, end))
                {
                    create_control_structure_rec(
                        init_encloses_end_init_before_end, init, end, end_cs, tdg_node, last_cs);
                }
                else
                {
                    create_control_structure_rec(
                        init_encloses_end_end_before_init, init, end, end_cs, tdg_node, last_cs);
                }
            }
            else if (enclosing_cs == __end)
            {
                if (ExtensibleGraph::node_is_ancestor_of_node(init, end))
                {
                    create_control_structure_rec(
                        end_encloses_init_init_before_end, init, end, init_cs, tdg_node, last_cs);
                }
                else
                {
                    create_control_structure_rec(
                        end_encloses_init_end_before_init, init, end, init_cs, tdg_node, last_cs);
                }
            }
            else
            {
                internal_error("Unreachable code.\n", 0);
            }
        }
        // Case 2: the node is not a taskpart => just consider the node and its control structures
        else
        {
            Node* n = tdg_node->_pcfg_node;
            Node* control_structure = ExtensibleGraph::get_enclosing_control_structure(n);

            create_control_structure_rec(
                    same_cs_init_before_end, NULL, n, control_structure, tdg_node, last_cs);
        }
    }

    void OldTaskDependencyGraph::set_tdg_nodes_control_structures()
    {
        for (TDG_Node_map::iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); ++it)
        {
            TDG_Node* tdg_node = it->second;
            Node* node = tdg_node->_pcfg_node;
            Node* init_taskpart = tdg_node->_init_taskpart;
            ControlStructure* last_cs = NULL;

            // 1.- Add the implicit control structure:
            //     this is necessary to set the values of the variables reaching a task
            {
                ControlStructure* cs = new ControlStructure(++control_id, Implicit, NBase::null(), NULL);
                std::string taken_branch;
                _pcfg_to_cs_map.insert(std::pair<std::pair<Node*, Node*>, ControlStructure*>
                        (std::pair<Node*, Node*>(node, init_taskpart), cs));
                tdg_node->add_control_structure(cs, taken_branch);
                last_cs = cs;
            }

            // 2.- Add the real control structures
            create_control_structures(tdg_node, last_cs);
        }
    }

    void OldTaskDependencyGraph::store_condition_list_of_symbols(const NBase& condition, const NodeclMap& reach_defs)
    {
        NodeclSet already_treated;
        NodeclList tmp = Nodecl::Utils::get_all_memory_accesses(condition);
        std::queue<NBase, std::deque<NBase> > vars(std::deque<NBase>(tmp.begin(), tmp.end()));
        while (!vars.empty())
        {
            NBase n = vars.front();         
            vars.pop();
            already_treated.insert(n);
            _syms.insert(std::pair<NBase, unsigned int>(n, 0));

            if (n.is_constant())
                continue;

            // Add all the variables found in the reaching definitions of the current variable
            ERROR_CONDITION(reach_defs.find(n) == reach_defs.end(), 
                            "No reaching definition found for variable '%s' while gathering all necessary symbols.\n", 
                            n.prettyprint().c_str());
            
            std::pair<NodeclMap::const_iterator, NodeclMap::const_iterator> reach_defs_map = reach_defs.equal_range(n);
            NodeclSet to_treat;
            for(NodeclMap::const_iterator it = reach_defs_map.first; it != reach_defs_map.second; ++it)
            {
                tmp = Nodecl::Utils::get_all_memory_accesses(it->second.first);
                for(NodeclList::iterator itt = tmp.begin(); itt != tmp.end(); ++itt)
                {
                    NBase var = *itt;
                    if ((already_treated.find(var) == already_treated.end()) && 
                        (to_treat.find(var) == to_treat.end()))
                    {
                        to_treat.insert(var);
                        vars.push(var);
                    }
                }
            }
        }
    }

    // Traverse the PCFG forward until:
    // - a barrier is found
    // - a taskwait in the same task region is found
    // - the current task region is exited
    void OldTaskDependencyGraph::connect_tasks_to_previous_synchronization(Node* sync)
    {
        // Start traversing the children of the synchronization node
        std::queue<Node*> worklist;
        ObjectList<Node*> children = sync->get_children();
        for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            worklist.push(*it);

        std::set<Node*> visited;
        while (!worklist.empty())
        {
            Node* n = worklist.front();
            worklist.pop();

            // Base cases:
            // - the node has already been visited
            if (visited.find(n) != visited.end())
                continue;
            // - we have found a taskwait or a barrier node
            if (n->is_omp_taskwait_node() || n->is_omp_barrier_graph_node())
                continue;

            // Treat the current node: if it is a task, then it has to be synchronized here!
            visited.insert(n);
            if (n->is_omp_task_node())
            {
                TDG_Node* tdg_sync = find_tdg_node_from_pcfg_node(sync);
                TDG_Node* tdg_child_task = find_tdg_node_from_pcfg_node(n);
                connect_tdg_nodes(tdg_sync, tdg_child_task, __Static,
                                  /*condition*/ Nodecl::NodeclBase::null());
            }

            // Prepare following iterations
            if (n->is_graph_node())
            {
                if (!n->is_omp_task_node())
                    worklist.push(n->get_graph_entry_node());
            }
            else if (n->is_exit_node())
            {
                children = n->get_outer_node()->get_children();
                for (ObjectList<Node*>::const_iterator it = children.begin();
                     it != children.end(); ++it)
                    worklist.push(*it);
            }
            else
            {
                children = n->get_children();
                for (ObjectList<Node*>::const_iterator it = children.begin();
                     it != children.end(); ++it)
                    worklist.push(*it);
            }
        }
    }

    void OldTaskDependencyGraph::connect_tdg_nodes_from_pcfg(Node* n)
    {
        connect_dependent_nodes(n);
        ExtensibleGraph::clear_visits(n);
        if (_taskparts_enabled)
            connect_taskparts();
    }

    void OldTaskDependencyGraph::connect_dependent_nodes(Node* n)
    {
        if (n->is_visited())
            return;

        n->set_visited(true);

        if (n->is_omp_task_node()
            || n->is_omp_async_target_node())
        {
            // Connect all tasks synchronized here with the new Taskwait/Barrier TDG_Node
            TDG_Node* tdg_sync = find_tdg_node_from_pcfg_node(n);
            const Edge_list& sync_exits = n->get_exit_edges();
            for (Edge_list::const_iterator it = sync_exits.begin(); it != sync_exits.end(); ++it)
            {
                Node* child = (*it)->get_target();
                if (child->is_omp_task_node()
                    || child->is_omp_async_target_node()
                    || child->is_omp_taskwait_node()
                    || child->is_omp_barrier_graph_node())
                {
                    TDG_Node* tdg_child_task = find_tdg_node_from_pcfg_node(child);
                    const NBase& cond = (*it)->get_condition();
                    connect_tdg_nodes(tdg_sync, tdg_child_task, (*it)->get_sync_kind(), cond);
                    store_condition_list_of_symbols(cond, n->get_reaching_definitions_out());
                }
            }
        }
        else if (n->is_omp_taskwait_node() || n->is_omp_barrier_graph_node())
        {
            TDG_Node* tdg_sync = find_tdg_node_from_pcfg_node(n);
            // Look for the real node to whom the current synchronization is connected
            Edge_list sync_exits = n->get_exit_edges();
            while (sync_exits.size()==1 &&
                    (sync_exits[0]->get_target()->is_omp_flush_node()
                        || sync_exits[0]->get_target()->is_exit_node()))
            {
                Node* child = sync_exits[0]->get_target();
                if (child->is_exit_node())
                    sync_exits = child->get_outer_node()->get_exit_edges();
                else
                    sync_exits = child->get_exit_edges();
            }
            // Connect the synchronization to the exit node if it is a task or another synchronization
            for (Edge_list::iterator it = sync_exits.begin(); it != sync_exits.end(); ++it)
            {
                Node* child = (*it)->get_target();
                if (child->is_omp_task_node()
                    || child->is_omp_async_target_node()
                    || child->is_omp_taskwait_node()
                    || child->is_omp_barrier_graph_node())
                {
                    TDG_Node* tdg_child_task = find_tdg_node_from_pcfg_node(child);
                    // In this case, the condition is always Static
                    // Furthermore, if we have skipped nodes in the previous while
                    // it may happen that we cannot call to get_condition method
//                     const NBase& cond = (*it)->get_condition();
//                     connect_tdg_nodes(tdg_sync, tdg_child_task, (*it)->get_sync_kind(), cond);
//                     store_condition_list_of_symbols(cond, n->get_reaching_definitions_out());
                    connect_tdg_nodes(tdg_sync, tdg_child_task,
                                        __Static,
                                        /*condition*/ Nodecl::NodeclBase::null());
                }
            }

            // It may happen that a task appears after a taskwait|barrier,
            // but they are not directly connected in the PCFG, so we have to analyze that situation here
            connect_tasks_to_previous_synchronization(n);
        }

        if (n->is_graph_node())
            connect_dependent_nodes(n->get_graph_entry_node());

        // Iterate over the children
        const Node_list& children = n->get_children();
        for (Node_list::const_iterator it = children.begin(); it != children.end(); ++it)
            connect_dependent_nodes(*it);
    }

namespace {
    // BFT on the PCFG from node end_tp
    // NOTE: Any "next init taskpart" must be in the same context as end_tp
    Node* find_next_init_taskpart(Node* end_tp)
    {
        // 1.- Initialize the list of nodes we will use for the BFT
        // Function calls are wrapped in a graph node
        // and we need to work in the context of that graph node
        Node* graph_end_tp = end_tp->get_outer_node();
        std::queue<Node*> worklist;
        ObjectList<Node*> children = graph_end_tp->get_children();
        for (ObjectList<Node*>::iterator it = children.begin();
             it != children.end(); ++it)
            worklist.push(*it);

        // 2.- Actual BFT traversal
        Node* next_init_tp = NULL;
        while (!worklist.empty())
        {
            // Consider next node
            Node* n = worklist.front();
            worklist.pop();

            // Check whether it is an "init taskpart"
            if (n->is_function_call_graph_node()
                && n->get_graph_entry_node()->get_children()[0]->is_function_call_node()
                && (n->get_graph_entry_node()->get_children()[0]->get_statements()[0].as<Nodecl::FunctionCall>().get_called().get_symbol().get_name() == "GOMP_init_taskpart"))
            {
                next_init_tp = n->get_graph_entry_node()->get_children()[0];
                break;
            }

            // Keep iterating over children avoiding:
            // - back edges
            // - exiting the current context
            const ObjectList<Edge*>& exit_edges = n->get_exit_edges();
            for (ObjectList<Edge*>::const_iterator ite = exit_edges.begin();
                 ite != exit_edges.end(); ++ite)
            {
                if ((*ite)->is_back_edge() || (*ite)->is_task_edge())
                    continue;
                worklist.push((*ite)->get_target());
            }
        }
        // NOTE next_init_tp will be NULL for the last taskpart of the code

        return next_init_tp;
    }
}

    void OldTaskDependencyGraph::connect_taskparts()
    {
        // 1.- Connect each init taskpart to its immediate next end taskparts
        for (TDG_Node_map::iterator it = _tdg_nodes.begin();
             it != _tdg_nodes.end(); ++it)
        {
            TDG_Node* tdg_n = it->second;
            if (tdg_n->_type == Taskpart)
            {
                Node* next_init_tp = find_next_init_taskpart(tdg_n->_pcfg_node);
                for (TDG_Node_map::iterator itt = _tdg_nodes.begin();
                     itt != _tdg_nodes.end(); ++itt)
                {
                    TDG_Node* tdg_m = itt->second;
                    if (tdg_m->_type == Taskpart
                        && tdg_m->_init_taskpart == next_init_tp)
                    {
                        connect_tdg_nodes(tdg_n, tdg_m, __Static,
                                          /*condition*/ Nodecl::NodeclBase::null());
                    }
                }
            }
        }

        // 2.- Connect each task/target to its immediately previous taskpart
        for (TDG_Node_map::iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); ++it)
        {
            TDG_Node* tdg_n = it->second;
            if (tdg_n->_type == Task || tdg_n->_type == Target)
            {
                // Get the parent node in the PCFG corresponding to the GOMP_end_taskpart call
                Node* task = tdg_n->_pcfg_node;
                Node* task_creation = ExtensibleGraph::get_task_creation_from_task(task);
                const ObjectList<Node*>& parents = task_creation->get_parents();
                ERROR_CONDITION(parents.size() != 1,
                                "A task creation node is expected to have exactly one parent "
                                "but task '%d' has '%d' parents.\n", task->get_id(), parents.size());
                ERROR_CONDITION(!parents[0]->is_function_call_graph_node(),
                                "The parent of a task creation node is expected to be a function call, "
                                "but parent of task '%d' is a %s",
                                task->get_id(), parents[0]->get_type_as_string().c_str());
                Node* previous_end_tp = parents[0]->get_graph_exit_node()->get_parents()[0];

                for (TDG_Node_map::iterator itt = _tdg_nodes.begin();
                     itt != _tdg_nodes.end(); ++itt)
                {
                    TDG_Node* tdg_m = itt->second;
                    if (tdg_m->_type == Taskpart
                        && tdg_m->_pcfg_node == previous_end_tp)
                        connect_tdg_nodes(tdg_m, tdg_n, __Static,
                                        /*condition*/ Nodecl::NodeclBase::null());
                }
            }
        }
    }

}
}