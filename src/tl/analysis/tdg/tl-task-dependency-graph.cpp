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

#include <cassert>
#include <fstream>
#include <unistd.h>
#include <sys/stat.h>

#include "cxx-cexpr.h"
#include "tl-pcfg-utils.hpp"
#include "tl-task-dependency-graph.hpp"

namespace TL { 
namespace Analysis {
    
    typedef ObjectList<TDG_Node*> TDG_Node_list;
    typedef ObjectList<TDG_Edge*> TDG_Edge_list;
    typedef ObjectList<Node*> Node_list;
    typedef ObjectList<Edge*> Edge_list;
    
    static int id = 0;
    std::map<Node*, int> control_struct_node_to_id;
    
namespace{
    
    //! TDG_Edge :: This method returns the clauses associated to a Nodecl::OpenMP::Task node
    NodeclList get_task_dependency_clauses(const Nodecl::OpenMP::Task& task)
    {
        NodeclList result;
        
        Nodecl::List task_environ = task.get_environment().as<Nodecl::List>();
        for(Nodecl::List::iterator it = task_environ.begin(); it != task_environ.end(); ++it)
        {
            if(it->is<Nodecl::OpenMP::DepIn>() || it->is<Nodecl::OpenMP::DepInout>() || it->is<Nodecl::OpenMP::DepOut>())
            {
                result.insert(*it);
            }
            else
            {
                if (it->is<Nodecl::OpenMP::Target>() || it->is<Nodecl::OpenMP::If>()
                        || it->is<Nodecl::OpenMP::Final>() || it->is<Nodecl::OpenMP::Untied>()
                        || it->is<Nodecl::OpenMP::Firstprivate>() || it->is<Nodecl::OpenMP::Private>()
                        || it->is<Nodecl::OpenMP::Shared>()
                        || it->is<Nodecl::OpenMP::FlushAtEntry>() || it->is<Nodecl::OpenMP::FlushAtExit>()
                        || it->is<Nodecl::OpenMP::TaskLabel>())
                {}  // Ignore them, we expect them here
                else
                {
                    WARNING_MESSAGE("Ignoring clause %s for task %s. Maybe we should do something with it...\n", 
                                     it->prettyprint().c_str());
                }
            }
        }
        
        return result;
    }
    
    TDGEdgeType get_tdg_edge_type_from_pcfg_edge_type(NBase pcfg_edge_type)
    {
        TDGEdgeType result;
        ERROR_CONDITION(!pcfg_edge_type.is<Nodecl::StringLiteral>(), 
                        "Expected StringLiteral as attribute of a task edge, but %s found.\n", 
                        ast_print_node_type(pcfg_edge_type.get_kind()));
        char is_null_ended = 0;
        std::string type_str = std::string(const_value_string_unpack_to_string(
                    pcfg_edge_type.get_constant(),
                    &is_null_ended));
        if(type_str == "strict")
            result = Strict;
        else if(type_str == "static") 
            result = Static;
        else if(type_str == "maybe")
            result = Maybe;
        else if(type_str == "post")
            result = Post;
        else
        {
            internal_error("Unexpected type of synchronization edge '%s' from PCFG. "
                           "Expected strict|static|maybe|post.", pcfg_edge_type.prettyprint().c_str());
        }
        return result;
    }
    
    //! Returns true when there is a path between 'current' and 'task'
    bool task_is_in_path(Node* control_structure, Node* current, Node* task)
    {
        if(current->is_visited())
            return false;
        
        current->set_visited(true);
        
        // Only traverse the nodes that are inside control_structure
        if(current == control_structure->get_graph_exit_node())
            return false;
        
        // Return true only when we find the task traversing the current path
        if(current == task)
            return true;
        
        bool result = false;
        
        // Traverse the inner nodes if 'current' is a graph node
        if(current->is_graph_node())
            result = task_is_in_path(control_structure, current->get_graph_entry_node(), task);
        
        // Traverse the children
        ObjectList<Node*> children = current->get_children();
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end() && !result; ++it)
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
    
    //! TaskDependencyGraph :: Returns a nodecl containing the condition that must fulfill 
    //! to follow the branch of an ifelse that takes to 'task'
    NBase get_ifelse_condition_from_path(Node* control_structure, Node* task)
    {
        NBase condition;
        
        // Get the statements that form the condition
        NBase cond_stmt = get_condition_stmts(control_structure->get_condition_node());
        
        // Find which path (TRUE|FALSE) takes to the task and compute the condition accordingly
        ObjectList<Edge*> exit_edges = control_structure->get_condition_node()->get_exit_edges();
        for(ObjectList<Edge*>::iterator it = exit_edges.begin(); it != exit_edges.end(); ++it)
        {
            if(task_is_in_path(control_structure, (*it)->get_target(), task))
            {
                // Compute the condition depending on the branch taken in the IfElseStatement
                if((*it)->is_true_edge())
                    condition = cond_stmt;
                else
                    condition = Nodecl::LogicalNot::make(cond_stmt.shallow_copy(), cond_stmt.get_type());
                
                // Stop iterating, for we already found the task
                break;
            }
        }
        
        return condition;
    }
    
    void get_cases_leading_to_task(Node* control_structure, Node* current, NodeclList& cases)
    {
        if(current->is_visited())
            return;
        
        if(current->is_switch_case_node())
        {
            ObjectList<Edge*> entry_edges = current->get_entry_edges();
            for(ObjectList<Edge*>::iterator it = entry_edges.begin(); it != entry_edges.end(); ++it )
                if((*it)->is_case_edge())
                    cases.insert((*it)->get_label());
            
            // If there is only one entry, no other case can lead to 'task'
            if(entry_edges.size()==1)
                return;
        }
        
        ObjectList<Node*> parents = (current->is_entry_node() ? ObjectList<Node*>(1, current->get_outer_node())
                                                              : current->get_parents());
        for(ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
            get_cases_leading_to_task(control_structure, *it, cases);
    }
    
    NBase get_switch_condition_from_path(Node* control_structure, Node* task)
    {
        NBase condition;
        
        // Get the statements that form the condition
        Node* cond = NULL;
        ObjectList<Node*> control_parents = control_structure->get_parents();
        for(ObjectList<Node*>::iterator it = control_parents.begin(); it != control_parents.end(); ++it)
            if((*it)->is_entry_node())
            {
                Node* switch_node = (*it);
                while(!switch_node->is_switch_statement() && switch_node != NULL)
                    switch_node = switch_node->get_outer_node();
                ERROR_CONDITION(switch_node==NULL, "No switch node found for case node %d.", control_structure->get_id());
                
                cond = switch_node->get_condition_node();
                break;
            }
        NBase cond_stmt = get_condition_stmts(cond);
        
        NodeclList cases;
        get_cases_leading_to_task(control_structure, task->get_parents()[0], cases);
        ERROR_CONDITION(cases.empty(), "No case leading to task %d has been found in control structure %d.\n", 
                        task->get_id(), control_structure->get_id());
        
        // Create the nodecl for the first case
        TL::Type cond_type = cond_stmt.get_type();
        NodeclList::iterator it = cases.begin();
        condition = Nodecl::Equal::make(cond_stmt.shallow_copy(), it->shallow_copy(), cond_type);
        // Build the others, if there is some
        ++it;
        for(; it != cases.end(); ++it)
            condition = Nodecl::LogicalOr::make(condition.shallow_copy(), 
                                                Nodecl::Equal::make(cond_stmt.shallow_copy(), it->shallow_copy(), cond_type), 
                                                cond_type);
        
        return condition;
    }
}
 
     
    
    // ******************************************************************* //
    // ************ Task Dependency Graph Control Structures ************* //
    
    ControlStructure::ControlStructure(int cs_id, ControlStructureType type, const NBase condition)
        : _id(cs_id), _type(type), _condition(condition)
    {}
    
    int ControlStructure::get_id()
    {
        return _id;
    }
    
    ControlStructureType ControlStructure::get_type()
    {
        return _type;
    }
    
    NBase ControlStructure::get_condition()
    {
        return _condition;
    }
    
    // ************ Task Dependency Graph Control Structures ************* //
    // ******************************************************************* //
    
 
    // ******************************************************************* //
    // ************** Task Dependency Graph Edges and Nodes ************** //
    
    TDG_Node::TDG_Node(Node* n, TDGNodeType type)
        : _id(++id), _pcfg_node(n), _type(type), _entries(), _exits(), _control_structures()
    {}
    
    void TDG_Node::add_control_structure(ControlStructure cs)
    {
        _control_structures.append(cs);
    }
    
    ObjectList<ControlStructure> TDG_Node::get_control_structures()
    {
        return _control_structures;
    }
    
    TDG_Edge::TDG_Edge(TDG_Node* source, TDG_Node* target, TDGEdgeType type, const NBase& condition)
        : _source(source), _target(target), _type(type), 
          _source_clauses(), _target_clauses(), _condition(condition)
    {
        // Fill source and target lists with the corresponding clauses
        if(source->_pcfg_node->is_omp_task_node())
        {
            Nodecl::OpenMP::Task task = source->_pcfg_node->get_graph_related_ast().as<Nodecl::OpenMP::Task>();
            _source_clauses = get_task_dependency_clauses(task);
        }
        if(target->_pcfg_node->is_omp_task_node())
        {
            Nodecl::OpenMP::Task task = target->_pcfg_node->get_graph_related_ast().as<Nodecl::OpenMP::Task>();
            _target_clauses = get_task_dependency_clauses(task);
        }
    }
    
    TDG_Node* TDG_Edge::get_source()
    {
        return _source;
    }
    
    TDG_Node* TDG_Edge::get_target()
    {
        return _target;
    }
    
    // ************** Task Dependency Graph Edges and Nodes ************** //
    // ******************************************************************* //

    
    // ******************************************************************* //
    // ********************** Task Dependency Graph ********************** //
    
    TaskDependencyGraph::TaskDependencyGraph(ExtensibleGraph* pcfg)
        : _pcfg(pcfg), _tdg_nodes(), _syms(), _pcfg_control_structure_to_id()
    {
        Node* pcfg_node = _pcfg->get_graph();
        
        // The whole code must be taskified, otherwise some dependences may not be shown
        taskify_graph(pcfg_node);
        
        // Compute the Task Dependency Graph from the PCFG
        create_tdg(pcfg_node);
    }
    
    std::string TaskDependencyGraph::get_name() const
    {
        std::string name;
        if(_pcfg != NULL)
            name = _pcfg->get_name();
        return name;
    }
    
    void TaskDependencyGraph::connect_tdg_nodes(TDG_Node* parent, TDG_Node* child, 
                                                NBase type, const NBase& condition)
    {    
        TDG_Edge* edge = new TDG_Edge(parent, child, get_tdg_edge_type_from_pcfg_edge_type(type), condition);
        parent->_exits.insert(edge);
        child->_entries.insert(edge);
    }
    
    TDG_Node* TaskDependencyGraph::find_task_from_tdg_nodes_list(Node* task)
    {
        TDG_Node* result = NULL;
        for(TDG_Node_list::iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); ++it)
            if((*it)->_pcfg_node == task)
            {
                result = *it;
                break;
            }
        return result;
    }
    
    // TODO
    void TaskDependencyGraph::taskify_graph(Node* current)
    {
    }
    
    void TaskDependencyGraph::create_tdg(Node* current)
    {
        create_tdg_nodes_from_pcfg(current);
        ExtensibleGraph::clear_visits(current);
        set_tdg_nodes_control_structures();
        connect_tdg_nodes_from_pcfg(current);
        ExtensibleGraph::clear_visits(current);
    }
    
    void TaskDependencyGraph::create_tdg_nodes_from_pcfg(Node* current)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);
            
            // Call recursively with inner nodes if applies
            if(current->is_graph_node())
                create_tdg_nodes_from_pcfg(current->get_graph_entry_node());
            
            // Create the TDG task node
            TDG_Node* tdg_current = NULL;
            if(current->is_omp_task_node())
                tdg_current = new TDG_Node(current, Task);
            else if(current->is_omp_taskwait_node())
                tdg_current = new TDG_Node(current, Taskwait);
            else if(current->is_omp_barrier_graph_node())   // We do not need to traverse the Graph Barrier Node
                tdg_current = new TDG_Node(current, Barrier);
            
            if(tdg_current != NULL)
                _tdg_nodes.insert(tdg_current);
            
            // Iterate over the children
            Node_list children = current->get_children();
            for(Node_list::iterator it = children.begin(); it != children.end(); ++it)
                create_tdg_nodes_from_pcfg(*it);
        }
    }
    
    void TaskDependencyGraph::set_tdg_nodes_control_structures()
    {
        for(ObjectList<TDG_Node*>::iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); ++it)
        {
            Node* node = (*it)->_pcfg_node;
            
            Node* control_structure = ExtensibleGraph::get_enclosing_control_structure(node);
            while(control_structure != NULL)
            {
                // Get control structure type and condition
                ControlStructureType cs_t;
                NBase condition;
                if(control_structure->is_loop_node())
                {
                    // get the type of the Control Structure
                    cs_t = Loop;
                    
                    // Get the condition of the loop
                    Node* cond = control_structure->get_condition_node();
                    assert(cond != NULL);
                    NodeclList stmts = cond->get_statements();
                    assert(stmts.size() == 1);
                    condition = stmts[0];
                }
                else if(control_structure->is_ifelse_statement())
                {
                    // get the type of the Control Structure
                    cs_t = Select;
                    
                    // Check whether the statement is in the TRUE or the FALSE branch of the condition
                    condition = get_ifelse_condition_from_path(control_structure, node);
                    ObjectList<Node*> children = control_structure->get_condition_node()->get_children();
                    for(ObjectList<Node*>::iterator itt = children.begin(); itt != children.end(); ++itt)
                        ExtensibleGraph::clear_visits_in_level(*itt, control_structure);
                }
                else if(control_structure->is_switch_case_node())
                {
                    // get the type of the Control Structure
                    cs_t = Select;
                    
                    // Build the condition depending on the branch where the task is created
                    condition = get_switch_condition_from_path(control_structure, node);
                    std::cerr << "Condition of switch to node " << node->get_id() << " is " << condition.prettyprint() << std::endl;
                }
                else
                {
                    internal_error("Unexpected node type %s when printing condition to TDG.\nExpected Loop, IfElse or Switch.\n", 
                                   control_structure->get_type_as_string().c_str())
                }
                
                // Store the symbols involved in the condition in the list of used symbols in the graph
                ERROR_CONDITION(condition.is_null(), "No condition has been computed for task %d in control structure %d.\n", 
                                node->get_id(), control_structure->get_id());
                store_condition_list_of_symbols(condition);
                
                // Crete the ControlStruct object and set it to the node
                int cs_id;
                if(control_struct_node_to_id.find(control_structure)!=control_struct_node_to_id.end())
                    cs_id = control_struct_node_to_id[control_structure];
                else
                {
                    cs_id = ++id;
                    control_struct_node_to_id[control_structure] = cs_id;
                }
                ControlStructure cs(cs_id, cs_t, condition);
                (*it)->add_control_structure(cs);
                
                // Prepare next iteration
                control_structure = ExtensibleGraph::get_enclosing_control_structure(control_structure);
            }
        }
    }
    
    void TaskDependencyGraph::store_condition_list_of_symbols(const NBase& condition)
    {
        ObjectList<Nodecl::Symbol> cond_syms = Nodecl::Utils::get_all_symbols_first_occurrence(condition);
        for(ObjectList<Nodecl::Symbol>::iterator it = cond_syms.begin(); it != cond_syms.end(); ++it)
            _syms.insert(std::pair<Symbol, unsigned int>(it->get_symbol(), 0));
    }
    
    void TaskDependencyGraph::connect_tdg_nodes_from_pcfg(Node* current)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);
            
            if(current->is_omp_task_node())
            {
                // Connect all tasks synchronized here with the new Taskwait/Barrier TDG_Node
                TDG_Node* tdg_sync = find_task_from_tdg_nodes_list(current);
                Edge_list sync_exits = current->get_exit_edges();
                for(Edge_list::iterator it = sync_exits.begin(); it != sync_exits.end(); ++it)
                {
                    Node* child = (*it)->get_target();
                    if(child->is_omp_task_node() || child->is_omp_taskwait_node() || child->is_omp_barrier_graph_node())
                    {
                        TDG_Node* tdg_child_task = find_task_from_tdg_nodes_list(child);
                        connect_tdg_nodes(tdg_sync, tdg_child_task, (*it)->get_label(), (*it)->get_condition());
                        store_condition_list_of_symbols((*it)->get_condition());
                    }
                }
            }
            else if(current->is_omp_taskwait_node() || current->is_omp_barrier_graph_node())
            {
                TDG_Node* tdg_sync = find_task_from_tdg_nodes_list(current);
                // Look for the real node to whom the current synchronization is connected
                Edge_list sync_exits = current->get_exit_edges();
                while(sync_exits.size()==1 && 
                      (sync_exits[0]->get_target()->is_omp_flush_node() || sync_exits[0]->get_target()->is_exit_node()))
                {
                    Node* child = sync_exits[0]->get_target();
                    if(child->is_exit_node())
                        sync_exits = child->get_outer_node()->get_exit_edges();
                    else
                        sync_exits = child->get_exit_edges();
                }
                // Connect the synchronization to the exit node if it is a task or another synchronization
                for(Edge_list::iterator it = sync_exits.begin(); it != sync_exits.end(); ++it)
                {
                    Node* child = (*it)->get_target();
                    if(child->is_omp_task_node() || child->is_omp_taskwait_node() || child->is_omp_barrier_graph_node())
                    {
                        TDG_Node* tdg_child_task = find_task_from_tdg_nodes_list(child);
                        connect_tdg_nodes(tdg_sync, tdg_child_task, (*it)->get_label(), (*it)->get_condition());
                        store_condition_list_of_symbols((*it)->get_condition());
                    }
                }
            }
            
            if(current->is_graph_node())
                connect_tdg_nodes_from_pcfg(current->get_graph_entry_node());
            
            // Iterate over the children
            Node_list children = current->get_children();
            for(Node_list::iterator it = children.begin(); it != children.end(); ++it)
                connect_tdg_nodes_from_pcfg(*it);
        }
    }
    
//     static std::string prettyprint_clauses(Nodecl_list clauses)
//     {
//         std::string result;
//         
//         for(NodeclList::iterator it = clauses.begin(); it != clauses.end();)
//         {
//             // Note: there is no codegen for OpenMP nodecls, 
//             // that is why we print it manually instead of calling prettyprint
//             std::string clause_name;
//             std::string clause_args;
//             Nodecl::List args;
//             if(it->is<Nodecl::OpenMP::DepIn>())
//             {
//                 clause_name = "in";
//                 args =  it->as<Nodecl::OpenMP::DepIn>().get_in_deps().as<Nodecl::List>();
//             }
//             else if(it->is<Nodecl::OpenMP::DepOut>())
//             {
//                 clause_name = "out";
//                 args =  it->as<Nodecl::OpenMP::DepOut>().get_out_deps().as<Nodecl::List>();
//             }
//             else if(it->is<Nodecl::OpenMP::DepInout>())
//             {
//                 clause_name = "inout";
//                 args =  it->as<Nodecl::OpenMP::DepInout>().get_inout_deps().as<Nodecl::List>();
//             }
//             
//             for(Nodecl::List::iterator it_a = args.begin(); it_a != args.end();)
//             {
//                 clause_args += it_a->prettyprint();
//                 ++it_a;
//                 if(it_a != args.end())
//                     clause_args += ", ";
//             }
//             result += clause_name + "(" + clause_args + ")";
//             
//             ++it;
//             if(it != clauses.end())
//                 result += ", ";
//         }
//         
//         return result;
//     }
    
    void TaskDependencyGraph::print_tdg_node_to_dot(TDG_Node* current, std::ofstream& dot_tdg)
    {
        std::stringstream ss; ss << current->_id;
        std::string current_id = ss.str();
        Node* n = current->_pcfg_node;
        
        // Print the control structures (subgraphs) where the node is enclosed in
        ObjectList<ControlStructure> control_structures = current->get_control_structures();
        std::string indent = "\t";
        for(ObjectList<ControlStructure>::iterator it = control_structures.begin(); it != control_structures.end(); ++it)
        {
            dot_tdg << indent << "subgraph cluster" << ++id << "{\n";
            indent += "\t";
            dot_tdg << indent << "label=\"" << it->get_condition().prettyprint() << "\";\n";
            dot_tdg << indent << "color=\"" << (it->get_type()==Loop ? "deeppink" : "deepskyblue1" ) << "\";\n";
            dot_tdg << indent << "style=\"dashed\";\n";
        }
        
        // Create the node
        std::string task_label = "";
        TDGNodeType ntype = current->_type;
        if(ntype == Task)
        {
            // Get the name of the task
            Nodecl::OpenMP::Task task = n->get_graph_related_ast().as<Nodecl::OpenMP::Task>();
            task_label = "Task :: " + task.get_locus_str();
            Nodecl::List environ = task.get_environment().as<Nodecl::List>();
            for(Nodecl::List::iterator it = environ.begin(); it != environ.end(); ++it)
                if(it->is<Nodecl::OpenMP::TaskLabel>())
                {
                    task_label = "_" + it->prettyprint();
                    break;
                }
        }
        else if(ntype == Taskwait)
        {
            NBase tw_stmt = n->get_statements()[0];
            task_label = "Taskwait :: " + tw_stmt.get_locus_str();
        }
        else if(ntype == Barrier)
        {
            NBase barrier_stmt = n->get_graph_related_ast();
            task_label = "Barrier :: " + barrier_stmt.get_locus_str();
        }
        
        // print the node
        dot_tdg << indent << current_id << " [label=\"" << task_label << "\"];\n";
        
        
        // Close the subgraphs of the control structures
        for(ObjectList<ControlStructure>::iterator it = control_structures.begin(); it != control_structures.end(); ++it)
        {
            indent = indent.substr(0, indent.size()-1);
            dot_tdg << indent << "}\n";
        }
        
        // Create the connections from the current node to its children
        std::string headlabel, taillabel, style, condition;
        for(TDG_Edge_list::iterator it = current->_exits.begin(); it != current->_exits.end(); ++it)
        {
            // Get the edge info in a string
//             headlabel = "headlabel=\"" + prettyprint_clauses((*it)->_target_clauses) + "\"";
//             taillabel = "taillabel=\"" + prettyprint_clauses((*it)->_source_clauses) + "\"";
            TDGEdgeType etype = (*it)->_type;
            style = "style=\"" + std::string((etype == Strict || etype == Static) ? "solid" : "dashed") + "\"";
            if(!(*it)->_condition.is_null())
                condition = ", label=\"" + (*it)->_condition.prettyprint() + "\"";
            else
                condition = "true";
            // Create the dot edge
            std::stringstream child_id; child_id << (*it)->_target->_id;
            dot_tdg << "\t" << current_id << " -> " << child_id.str() 
                    << "[" << style << condition /*<< headlabel << ", " << taillabel*/ << "];\n";
        }
    }
    
    void TaskDependencyGraph::print_tdg_to_dot()
    {
        // Create the directory of dot files if it has not been previously created
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if(err == NULL)
            internal_error ("An error occurred while getting the path of the current directory", 0);
        struct stat st;
        std::string directory_name = std::string(buffer) + "/dot/";
        if(stat(directory_name.c_str(), &st) != 0)
        {
            int dot_directory = mkdir(directory_name.c_str(), S_IRWXU);
            if(dot_directory != 0)
                internal_error ("An error occurred while creating the dot directory in '%s'", 
                                 directory_name.c_str());
        }
        
        // Create the file where we will store the DOT TDG
        std::string dot_file_name = directory_name + _pcfg->get_name() + "_tdg.dot";
        std::ofstream dot_tdg;
        dot_tdg.open(dot_file_name.c_str());
        if(!dot_tdg.good())
            internal_error ("Unable to open the file '%s' to store the TDG.", dot_file_name.c_str());
        
        // Create the DOT graphs
        if(VERBOSE)
            std::cerr << "- TDG DOT file '" << dot_file_name << "'" << std::endl;
        dot_tdg << "digraph TDG {\n";
            dot_tdg << "\tcompound=true;\n";
            for(TDG_Node_list::iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); ++it)
                print_tdg_node_to_dot(*it, dot_tdg);
        dot_tdg << "}\n";
        dot_tdg.close();
        if(!dot_tdg.good())
            internal_error ("Unable to close the file '%s' where TDG has been stored.", dot_file_name.c_str());
        ExtensibleGraph::clear_visits(_pcfg->get_graph());
    }
    
    void TaskDependencyGraph::print_tdg_syms_to_json(std::ofstream& json_tdg)
    {
        if(!_syms.empty())
        {
            json_tdg << "\t\t\"defvars\" : [\n" ;
            unsigned int i = 1;
            for(std::map<Symbol, unsigned int>::iterator it = _syms.begin(); it != _syms.end(); ++i)
            {
                TL::Symbol s = it->first;
                json_tdg << "\t\t\t{\n";
                    json_tdg << "\t\t\t\t\"id\" : " << i << ",\n";
                    json_tdg << "\t\t\t\t\"name\" : \"" << s.get_name() << "\",\n";
                    json_tdg << "\t\t\t\t\"locus\" : \"" << s.get_locus_str() << "\",\n";
                    json_tdg << "\t\t\t\t\"type\" : \"" << s.get_type().get_declaration(s.get_scope(), 
                                    /*no name for the symbol, so we print only the type name*/"") << "\"\n";
                _syms[it->first] = i;
                ++it;
                if(it != _syms.end())
                    json_tdg << "\t\t\t},\n";
                else
                    json_tdg << "\t\t\t}\n";
            }
            json_tdg << "\t\t],\n" ;
        }
    }
    
    // This method returns a string corresponding to the prettyprinted version of a nodecl
    // where each symbol occurrence is replaced by a $id
    // Example:
    //     The expression :         'i == j'
    //     Will return the string:  '$1 == $2'
    static std::string transform_condition_into_json_expr(const NBase& condition)
    {
        std::string result = condition.prettyprint();
        
        // Get the name of each symbol and 
        // store a map that represents the position of the last replacement
        ObjectList<std::string> sym_names;
        std::map<std::string, int> symbol_position_map;
        ObjectList<Nodecl::Symbol> syms = Nodecl::Utils::get_all_symbols_first_occurrence(condition);
        for(ObjectList<Nodecl::Symbol>::iterator it = syms.begin(); it != syms.end(); ++it)
        {
            std::string s_name = it->get_symbol().get_name();
            sym_names.append(s_name);
            symbol_position_map[s_name] = 0;
        }
        
        // Transform the condition expression into the json expression
        int i = 1;
        for(ObjectList<std::string>::iterator it = sym_names.begin(); it != sym_names.end(); ++it, ++i)
        {
            // Find the position to be replaced
            int pos = result.find(*it, symbol_position_map[*it]);
            // Replace it
            std::stringstream id_str; id_str << "$" << i;
            result.replace(pos, it->size(), id_str.str());
            // Modify the base position
            symbol_position_map[*it] = pos + (id_str.str().size() - 1);
        }
        
        return result;
    }
    
    void TaskDependencyGraph::print_condition(TDG_Edge* edge, ControlStructure* node_cs, std::ofstream& json_tdg, std::string indent)
    {
        json_tdg << indent << "\"expression\" : ";
        assert(edge!=NULL || node_cs!=NULL);
        if((edge != NULL && !edge->_condition.is_null()) || (node_cs != NULL))
        {   
            // Get the condition
            NBase condition;
            if(node_cs != NULL)
                condition = node_cs->get_condition();
            else
                condition = edge->_condition;
            ObjectList<Nodecl::Symbol> syms = Nodecl::Utils::get_all_symbols_first_occurrence(condition);
            json_tdg << "\"" << transform_condition_into_json_expr(condition) << "\",\n";
            
            // Generate the list of involved variables
            if(syms.size() > 1)
                json_tdg << indent << "\"vars\" : [\n";
            else
                json_tdg << indent << "\"vars\" : \n";
            for(ObjectList<Nodecl::Symbol>::iterator its = syms.begin(); its != syms.end();)
            {
                if(_syms.find(its->get_symbol()) == _syms.end())
                {
                    internal_error("Variable %s, found in condition %s, "
                                   "has not been found during the phase of gathering the variables", 
                                   its->prettyprint().c_str(), condition.prettyprint().c_str());
                }
                json_tdg << indent << "\t{\n";
                    json_tdg << indent << "\t\t\"id\" : " << _syms[its->get_symbol()] << ",\n";
                    json_tdg << indent << "\t\t\"values\" : \"TODO\"\n";
                    
                    // TODO: values!
                    
                    ++its;
                    if(its != syms.end())
                        json_tdg << indent << "\t},\n";
                    else
                        json_tdg << indent << "\t}\n";
            }
            if(syms.size() > 1)
                json_tdg << indent << "]\n";
        }
        else    // There is no condition => TRUE
            json_tdg << "true\n";
    }
    
    void TaskDependencyGraph::print_tdg_nodes_to_json(std::ofstream& json_tdg)
    {
        json_tdg << "\t\t\"nodes\" : [\n";
        for(TDG_Node_list::iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end();)
        {
            TDG_Node* n = *it;
            json_tdg << "\t\t\t{\n";
                
            // node identifier
                json_tdg << "\t\t\t\t\"id\" : " << n->_id << ",\n";
                
            // node locus and type
            if(n->_type == Task) 
            {
                json_tdg << "\t\t\t\t\"locus\" : \"" << n->_pcfg_node->get_graph_related_ast().get_locus_str() << "\",\n";
                json_tdg << "\t\t\t\t\"type\" : \"Task\"";
            }
            else
            {
                json_tdg << "\t\t\t\t\"locus\" : \"" << n->_pcfg_node->get_statements()[0].get_locus_str() << "\",\n";
                if(n->_type == Taskwait)
                    json_tdg << "\t\t\t\t\"type\" : \"Taskwait\"";
                else
                    json_tdg << "\t\t\t\t\"type\" : \"Barrier\"";
            }
            
            // node exit edges
            if(!n->_exits.empty())
            {
                json_tdg << ",\n";
                json_tdg << "\t\t\t\t\"edges\" : [\n";
                for(ObjectList<TDG_Edge*>::iterator ite = n->_exits.begin(); ite != n->_exits.end();)
                {
                    json_tdg << "\t\t\t\t\t{\n";
                    // The target node
                    json_tdg << "\t\t\t\t\t\t\"node\" : " << (*ite)->_target->_id << ",\n";
                    // The condition
                    json_tdg << "\t\t\t\t\t\t\"when\" : {\n";
                    print_condition(*ite, NULL, json_tdg, "\t\t\t\t\t\t\t");
                    json_tdg << "\t\t\t\t\t\t}\n";
                        
                    ++ite;
                    if(ite != n->_exits.end())
                        json_tdg << "\t\t\t\t\t},\n";
                    else
                        json_tdg << "\t\t\t\t\t}\n";
                }
                json_tdg << "\t\t\t\t]\n";
            }
            
            // node control structures
            ObjectList<ControlStructure> control_structures = n->get_control_structures();
            if(!control_structures.empty())
            {
                json_tdg << ",\n";
                json_tdg << "\t\t\t\t\"control\" : [\n";
                for(ObjectList<ControlStructure>::iterator itt = control_structures.begin(); itt != control_structures.end(); ++itt)
                {
                    json_tdg << "\t\t\t\t\t{\n";
                    json_tdg << "\t\t\t\t\t\t\"type\" : \"" << ((itt->get_type()==Loop) ? "loop" : "select") << "\",\n";
                    json_tdg << "\t\t\t\t\t\t\"id\" : \"" << itt->get_id() << "\",\n";
                    json_tdg << "\t\t\t\t\t\t\"when\" : {\n";
                    print_condition(NULL, &(*itt), json_tdg, "\t\t\t\t\t\t\t");
                    json_tdg << "\t\t\t\t\t\t}\n";
                    json_tdg << "\t\t\t\t\t}\n";
                }
                json_tdg << "\t\t\t\t]\n";
            }
            else
                json_tdg << "\n";
            
            ++it;
            if(it != _tdg_nodes.end())
                json_tdg << "\t\t\t},\n";
            else
                json_tdg << "\t\t\t}\n";
        }
        json_tdg << "\t\t]\n";
    }
    
    void TaskDependencyGraph::print_tdg_to_json()
    {
        // Create the directory of json files if it has not been previously created
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if(err == NULL)
            internal_error ("An error occurred while getting the path of the current directory", 0);
        struct stat st;
        std::string directory_name = std::string(buffer) + "/json/";
        if(stat(directory_name.c_str(), &st) != 0)
        {
            int json_directory = mkdir(directory_name.c_str(), S_IRWXU);
            if(json_directory != 0)
                internal_error ("An error occurred while creating the json directory in '%s'", 
                                 directory_name.c_str());
        }
        
        // Create the file where we will store the JSON TDG
        std::string json_file_name = directory_name + _pcfg->get_name() + "_tdg.json";
        std::ofstream json_tdg;
        json_tdg.open(json_file_name.c_str());
        if(!json_tdg.good())
            internal_error ("Unable to open the file '%s' to store the TDG.", json_file_name.c_str());
        
        // Create the JSON graphs
        if(VERBOSE)
            std::cerr << "- TDG JSON file '" << json_file_name << "'" << std::endl;
        json_tdg << "{\n";
            json_tdg << "\t\"tdg\" : {\n";
                TL::Symbol sym = _pcfg->get_function_symbol();
                json_tdg << "\t\t\"function\" : \"" << (sym.is_valid() ? sym.get_name() : "") << "\",\n";
                json_tdg << "\t\t\"locus\" : \"" << _pcfg->get_nodecl().get_locus_str() << "\",\n";
                print_tdg_syms_to_json(json_tdg);
                print_tdg_nodes_to_json(json_tdg);
            json_tdg << "\t}\n";
        json_tdg << "}\n";
        json_tdg.close();
        if(!json_tdg.good())
            internal_error ("Unable to close the file '%s' where PCFG has been stored.", json_file_name.c_str());
        ExtensibleGraph::clear_visits(_pcfg->get_graph());
    }
}
}
