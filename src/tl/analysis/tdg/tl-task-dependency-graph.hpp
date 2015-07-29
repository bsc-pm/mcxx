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

#ifndef TL_TASK_DEPENDENCY_GRAPH_HPP
#define TL_TASK_DEPENDENCY_GRAPH_HPP

#include "tl-extensible-graph.hpp"

namespace TL { 
namespace Analysis {
    
    // ******************************************************************* //
    // ************ Task Dependency Graph Control Structures ************* //
    
    enum ControlStructureType {
        Implicit,
        Loop,
        IfElse,
        Switch
    };
    
    struct ControlStructure {
        int _id;
        ControlStructureType _type;
        NBase _condition;
        Node* _pcfg_node;
        ControlStructure* _enclosing;
        
        // *** Constructor *** //
        ControlStructure(int id, ControlStructureType type, 
                         const NBase& condition, Node* pcfg_node);
        
        // *** Getters and setters *** //
        int get_id() const;
        ControlStructureType get_type() const;
        std::string get_type_as_string() const;
        Nodecl::NodeclBase get_condition() const;
        Node* get_pcfg_node() const;
        ControlStructure* get_enclosing_cs() const;
        void set_enclosing_cs(ControlStructure* cs);
    };
    
    typedef std::vector<std::pair<ControlStructure*, ObjectList<std::string> > > ControlStList;
    
    // ************ Task Dependency Graph Control Structures ************* //
    // ******************************************************************* //
    
    
    // ******************************************************************* //
    // ************** Task Dependency Graph Edges and Nodes ************** //
    
    struct TDG_Edge;
    
    enum TDGNodeType {
        Task,
        Target,
        Taskwait,
        Barrier
    };
    
    struct TDG_Node {
        unsigned int _id;
        Node* _pcfg_node;
        TDGNodeType _type;
        ObjectList<TDG_Edge*> _entries;
        ObjectList<TDG_Edge*> _exits;
        ControlStList _control_structures;
        
        // *** Constructor *** //
        TDG_Node(Node* n, TDGNodeType type);
        
        // *** Getters and setters *** //
        unsigned int get_id() const;
        Node* get_pcfg_node() const;
        void add_control_structure(ControlStructure* cs, const ObjectList<std::string>& taken_branches);
        ControlStList get_control_structures() const;

        friend struct TDG_Edge;
        friend class TaskDependencyGraph;
    };

    struct TDG_Edge {
        TDG_Node* _source;
        TDG_Node* _target;
        SyncKind _kind;
        ObjectList<NBase> _source_clauses;
        ObjectList<NBase> _target_clauses;
        NBase _condition;
        
        // *** Constructor *** //
        TDG_Edge(TDG_Node* source, TDG_Node* target, SyncKind kind, const NBase& condition);

        // *** Getters and setters *** //
        TDG_Node* get_source() const;
        TDG_Node* get_target() const;
        
    friend class TDG_Node;
    friend class TaskDependencyGraph;
    };
    
    // ************** Task Dependency Graph Edges and Nodes ************** //
    // ******************************************************************* //
    
    
    // ******************************************************************* //
    // ********************** Task Dependency Graph ********************** //
    
    typedef std::map<Node*, ControlStructure*> PCFG_to_CS;
    typedef std::map<NBase, std::string, Nodecl::Utils::Nodecl_structural_less> VarToValueMap;
    typedef std::map<NBase, unsigned int, Nodecl::Utils::Nodecl_structural_less> VarToIdMap;
    
    class LIBTL_CLASS TaskDependencyGraph
    {
    private:
        // *** Class members *** //

        ExtensibleGraph* _pcfg;                         /*!< PCFG corresponding to the graph */
        std::map<unsigned int, TDG_Node*> _tdg_nodes;   /*!< Map of nodes in the TDG */
                                                        // We have a map where the key is the locus of the original statement
                                                        // this way we can print the nodes in order
        
        std::map<NBase, unsigned int, Nodecl::Utils::Nodecl_structural_less> _syms; /*!< Map of symbols appearing in the TDG associated to their identifier */
        PCFG_to_CS _pcfg_to_cs_map;                 /*!< Map of PCFG control structure nodes to their TDG control structure object */

        // *** Not allowed construction methods *** //
        TaskDependencyGraph(const TaskDependencyGraph& n);
        TaskDependencyGraph& operator=(const TaskDependencyGraph&);

        // *** Private methods *** //
        void connect_tdg_nodes(
                TDG_Node* parent, TDG_Node* child,
                SyncKind sync_type, const NBase& condition);

        TDG_Node* find_task_from_tdg_nodes_list(Node* task);
        void create_tdg_nodes_from_pcfg(Node* current);
        void set_tdg_nodes_control_structures();
        void connect_tdg_nodes_from_pcfg(Node* current);
        void store_condition_list_of_symbols(const NBase& condition, const NodeclMap& reach_defs);
        
        void taskify_graph(Node* current);
        void create_tdg(Node* current);
        
        void print_tdg_node_to_dot(TDG_Node* current, std::ofstream& dot_tdg) const;
        void print_condition(
                TDG_Edge* edge,
                ControlStructure* node_cs,
                std::ofstream& json_tdg,
                std::string indent,
                Nodecl::NodeclBase& dependency_size) const;
        void print_tdg_control_structs_to_json(std::ofstream& json_tdg) const;
        void print_tdg_syms_to_json(std::ofstream& json_tdg);
        void print_dependency_variables_to_json(
            std::ofstream& json_tdg,
            const VarToValueMap& var_to_value_map,
            const VarToIdMap& var_to_id_map,
            const NBase& condition,
            std::string indent,
            bool is_source,
            bool add_final_comma) const;
        void print_tdg_nodes_to_json(std::ofstream& json_tdg) const;
        void print_tdg_edges_to_json(std::ofstream& json_tdg) const;
        
    public:
        // *** Constructor *** //
        TaskDependencyGraph(ExtensibleGraph* pcfg);
        
        // *** Getters and Setters *** //
        std::string get_name() const;
        bool contains_nodes() const;
        
        // *** Printing methods *** //
        void print_tdg_to_dot() const;
        void print_tdg_to_json();
    };

    // ********************** Task Dependency Graph ********************** //
    // ******************************************************************* //
}
}

#endif  // TL_TASK_DEPENDENCY_GRAPH_HPP
