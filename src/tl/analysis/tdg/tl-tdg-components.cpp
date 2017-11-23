/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona Supercomputing Center             * *
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



#include "tl-counters.hpp"
#include "tl-task-dependency-graph.hpp"


namespace TL { 
namespace Analysis {

    // ******************************************************************* //
    // ************ Task Dependency Graph Control Structures ************* //
    
    ControlStructure::ControlStructure(int cs_id, ControlStructureType type, 
                                       const NBase& condition, Node* pcfg_node)
        : _id(cs_id), _type(type), _condition(condition), _pcfg_node(pcfg_node), _enclosing(NULL)
    {}

    int ControlStructure::get_id() const
    {
        return _id;
    }

    ControlStructureType ControlStructure::get_type() const
    {
        return _type;
    }

    std::string ControlStructure::get_type_as_string() const
    {
        std::string result;
        switch(_type)
        {
            case Implicit:  result = "Implicit"; break;
            case Loop:      result = "Loop";     break;
            case IfElse:    result = "IfElse";   break;
            default:        result = "Blank";
        };
        return result;
    }

    NBase ControlStructure::get_condition() const
    {
        return _condition;
    }

    Node* ControlStructure::get_pcfg_node() const
    {
        return _pcfg_node;
    }

    ControlStructure* ControlStructure::get_enclosing_cs() const
    {
        return _enclosing;
    }

    void ControlStructure::set_enclosing_cs(ControlStructure* cs)
    {
        _enclosing = cs;
    }

    // ************ Task Dependency Graph Control Structures ************* //
    // ******************************************************************* //
    
 
    // ******************************************************************* //
    // ************** Task Dependency Graph Edges and Nodes ************** //
    
    TDG_Node::TDG_Node(Node* n, TDGNodeType type, Node* parent, Node* init_tp)
        : _id(++tdg_node_id), _pcfg_node(n), _type(type),
          _parent(parent), _entries(), _exits(),
          _control_structures(), _init_taskpart(init_tp)
    {}

    unsigned int TDG_Node::get_id() const
    {
        return _id;
    }

    Node* TDG_Node::get_pcfg_node() const
    {
        return _pcfg_node;
    }

    void TDG_Node::add_control_structure(ControlStructure* cs, std::string taken_branch)
    {
        _control_structures.push_back(std::pair<ControlStructure*, std::string>(cs, taken_branch));
    }

    ControlStList TDG_Node::get_control_structures() const
    {
        return _control_structures;
    }

    TDG_Edge::TDG_Edge(TDG_Node* source, TDG_Node* target, SyncKind kind, const NBase& condition)
        : _source(source), _target(target), _kind(kind), _condition(condition)
    {}
    
    TDG_Node* TDG_Edge::get_source() const
    {
        return _source;
    }
    
    TDG_Node* TDG_Edge::get_target() const
    {
        return _target;
    }
    
    // ************** Task Dependency Graph Edges and Nodes ************** //
    // ******************************************************************* //


    // ******************************************************************* //
    // **************** Flow and Expanded TDG components ***************** //

    FTDGNode::FTDGNode(unsigned id, Node* n, FTDGNodeType type)
        : _id(id), _n(n), _parent(NULL), _predecessors(),
          _type(type), _inner_true(), _inner_false()
    {
        pcfg_to_ftdg.insert(std::pair<Node*, FTDGNode*>(n, this));
    }

    unsigned FTDGNode::get_id() const
    {
        return _id;
    }

    Node* FTDGNode::get_pcfg_node() const
    {
        return _n;
    }

    FTDGNode* FTDGNode::get_parent() const
    {
        return _parent;
    }

    void FTDGNode::set_parent(FTDGNode* parent)
    {
        _parent = parent;
    }

    const ObjectList<FTDGNode*>& FTDGNode::get_predecessors() const
    {
        return _predecessors;
    }

    void FTDGNode::add_predecessor(FTDGNode* predecessor)
    {
        _predecessors.push_back(predecessor);
    }

    FTDGNodeType FTDGNode::get_type() const
    {
        return _type;
    }

    ObjectList<FTDGNode*> FTDGNode::get_inner() const
    {
        ObjectList<FTDGNode*> res = _inner_true;
        res.append(_inner_false);
        return res;
    }

    const ObjectList<FTDGNode*>& FTDGNode::get_inner_true() const
    {
        return _inner_true;
    }

    const ObjectList<FTDGNode*>& FTDGNode::get_inner_false() const
    {
        return _inner_false;
    }

    const ObjectList<FTDGNode*>& FTDGNode::get_outer() const
    {
        return _outer;
    }

    void FTDGNode::add_inner(FTDGNode* n)
    {
        _inner_true.insert(n);
    }

    void FTDGNode::add_inner_true(FTDGNode* n)
    {
        _inner_true.insert(n);
    }

    void FTDGNode::add_inner_false(FTDGNode* n)
    {
        _inner_false.insert(n);
    }

    void FTDGNode::add_outer(FTDGNode* n)
    {
        _outer.insert(n);
    }

    ETDGNode::ETDGNode(int id, Node* pcfg_node)
        : _id(id), _var_to_value(), _inputs(), _outputs(), _child(NULL),
          _pcfg_node(pcfg_node), _visited(false)
    {}

    int ETDGNode::get_id() const
    {
        return _id;
    }

    std::set<ETDGNode*> ETDGNode::get_inputs() const
    {
        return _inputs;
    }

    void ETDGNode::add_input(ETDGNode* n)
    {
        _inputs.insert(n);
    }

    void ETDGNode::remove_input(ETDGNode* n)
    {
        _inputs.erase(n);
    }

    std::set<ETDGNode*> ETDGNode::get_outputs() const
    {
        return _outputs;
    }

    void ETDGNode::add_output(ETDGNode* n)
    {
        _outputs.insert(n);
    }

    void ETDGNode::remove_output(ETDGNode* n)
    {
        _outputs.erase(n);
    }

    SubETDG* ETDGNode::get_child() const
    {
        return _child;
    }

    void ETDGNode::set_child(SubETDG* child)
    {
        _child = child;
    }

    std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> ETDGNode::get_vars_map() const
    {
        return _var_to_value;
    }

    void ETDGNode::set_vars_map(std::map<NBase, const_value_t*, Nodecl::Utils::Nodecl_structural_less> var_to_value)
    {
        _var_to_value = var_to_value;
    }

    Node* ETDGNode::get_pcfg_node() const
    {
        return _pcfg_node;
    }

    Nodecl::NodeclBase ETDGNode::get_source_task() const
    {
        ERROR_CONDITION(!_pcfg_node->get_graph_related_ast().is<Nodecl::OpenMP::Task>(),
                        "Cannot retrieve the source task of a node which does not represent a Task, but a %s instead.\n",
                        (_pcfg_node->is_graph_node() ? _pcfg_node->get_graph_type_as_string().c_str()
                                                     : _pcfg_node->get_type_as_string().c_str()));
        return _pcfg_node->get_graph_related_ast();
    }

    bool ETDGNode::is_visited() const
    {
        return _visited;
    }

    void ETDGNode::set_visited(bool visited)
    {
        _visited = visited;
    }

    // **************** Flow and Expanded TDG components ***************** //
    // ******************************************************************* //

}
}