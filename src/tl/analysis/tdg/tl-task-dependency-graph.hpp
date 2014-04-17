/*--------------------------------------------------------------------
 (C) Copyright 2006-2012 Barcelona Supercomputing Center             *
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

#ifndef TL_TASK_DEPENDENCY_GRAPH_HPP
#define TL_TASK_DEPENDENCY_GRAPH_HPP

#include "tl-extensible-graph.hpp"

namespace TL { 
namespace Analysis {
    
    // ******************************************************************* //
    // ************ Task Dependency Graph Control Structures ************* //
    
    enum ControlStructureType {
        Loop,
        Select
    };
    
    struct ControlStructure {
        int _id;
        ControlStructureType _type;
        Nodecl::NodeclBase _condition;
        Node* _pcfg_node;
        ObjectList<unsigned int> _branch_ids;
        
        // *** Constructor *** //
        ControlStructure(int id, ControlStructureType type, 
                         const Nodecl::NodeclBase condition, Node* pcfg_node, 
                         ObjectList<unsigned int> taken_branches);
        
        // *** Getters and setters *** //
        int get_id() const;
        ControlStructureType get_type() const;
        Nodecl::NodeclBase get_condition() const;
        Node* get_pcfg_node() const;
        std::string get_branch_ids_as_string() const;
    };
    
    // ************ Task Dependency Graph Control Structures ************* //
    // ******************************************************************* //
    
    
    // ******************************************************************* //
    // ************** Task Dependency Graph Edges and Nodes ************** //
    
    struct TDG_Edge;
    
    enum TDGNodeType {
        Task,
        Taskwait,
        Barrier
    };
    
    struct TDG_Node {
        unsigned int _id;
        Node* _pcfg_node;
        TDGNodeType _type;
        ObjectList<TDG_Edge*> _entries;
        ObjectList<TDG_Edge*> _exits;
        ObjectList<ControlStructure> _control_structures;
        
        // *** Constructor *** //
        TDG_Node(Node* n, TDGNodeType type);
        
        // *** Getters and setters *** //
        unsigned int get_id();
        Node* get_pcfg_node();
        void add_control_structure(ControlStructure cs);
        ObjectList<ControlStructure> get_control_structures();
        
    friend class TDG_Edge;
    friend class TaskDependencyGraph;
    };
    
    enum TDGEdgeType {
        Strict,
        Static,
        Maybe,
        Post
    };
    
    struct TDG_Edge {
        TDG_Node* _source;
        TDG_Node* _target;
        TDGEdgeType _type;
        ObjectList<Nodecl::NodeclBase> _source_clauses;
        ObjectList<Nodecl::NodeclBase> _target_clauses;
        Nodecl::NodeclBase _condition;
        
        // *** Constructor *** //
        TDG_Edge(TDG_Node* source, TDG_Node* target, TDGEdgeType type, const Nodecl::NodeclBase& condition);
        
        // *** Getters and setters *** //
        TDG_Node* get_source();
        TDG_Node* get_target();
        
    friend class TDG_Node;
    friend class TaskDependencyGraph;
    };
    
    // ************** Task Dependency Graph Edges and Nodes ************** //
    // ******************************************************************* //
    
    
    // ******************************************************************* //
    // ********************** Task Dependency Graph ********************** //
    
    class LIBTL_CLASS TaskDependencyGraph
    {
    private:
        // *** Class members *** //
        ExtensibleGraph* _pcfg;                                 /*!< PCFG corresponding to the graph */
        ObjectList<TDG_Node*> _tdg_nodes;                       /*!< List of nodes in the TDG */
        
        std::map<Symbol, unsigned int> _syms;                   /*!< Map of symbols appearing in the TDG associated to their identifier */
        std::map<Node*, int> _pcfg_control_structure_to_id;     /*!< Map of PCFG control structure nodes to their TDG identifier */
        
        // *** Not allowed construction methods *** //
        TaskDependencyGraph(const TaskDependencyGraph& n);
        TaskDependencyGraph& operator=(const TaskDependencyGraph&);
        
        // *** Private methods *** //
        void connect_tdg_nodes(TDG_Node* parent, TDG_Node* child, 
                               Nodecl::NodeclBase type, const Nodecl::NodeclBase& condition);
        
        TDG_Node* find_task_from_tdg_nodes_list(Node* task);
        void create_tdg_nodes_from_pcfg(Node* current);
        void set_tdg_nodes_control_structures();
        void connect_tdg_nodes_from_pcfg(Node* current);
        void store_condition_list_of_symbols(const Nodecl::NodeclBase& condition);
        
        void taskify_graph(Node* current);
        void create_tdg(Node* current);
        
        void print_tdg_node_to_dot(TDG_Node* current, std::ofstream& dot_tdg);
        void print_condition(TDG_Edge* edge, ControlStructure* node_cs, 
                             std::ofstream& json_tdg, std::string indent, 
                             Nodecl::NodeclBase& dependency_size);
        void print_tdg_syms_to_json(std::ofstream& json_tdg);
        void print_tdg_nodes_to_json(std::ofstream& json_tdg);
        void print_tdg_edges_to_json(std::ofstream& json_tdg);
        
    public:
        // *** Constructor *** //
        TaskDependencyGraph(ExtensibleGraph* pcfg);
        
        // *** Getters and Setters *** //
        std::string get_name() const;
        bool contains_nodes() const;
        
        // *** Printing methods *** //
        void print_tdg_to_dot();
        void print_tdg_to_json();
    };

    // ********************** Task Dependency Graph ********************** //
    // ******************************************************************* //
}
}

#endif  // TL_TASK_DEPENDENCY_GRAPH_HPP