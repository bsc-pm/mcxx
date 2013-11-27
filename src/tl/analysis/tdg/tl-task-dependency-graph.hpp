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

    struct TDG_Edge;
    
    enum TDGNodeType {
        Task,
        Taskwait,
        Barrier,
        Control
    };
    
    struct TDG_Node {
        unsigned int _id;
        Node* _pcfg_node;
        TDGNodeType _type;
        ObjectList<TDG_Edge*> _entries;
        ObjectList<TDG_Edge*> _exits;
        
        bool _visited;
        
        TDG_Node( Node* n );
        
        void set_entry( TDG_Edge* entry );
        void set_exit( TDG_Edge* exit );
        ObjectList<TDG_Node*> get_children( );
        
        static void clear_visits( TDG_Node* current );
        
        friend class TDG_Edge;
        friend class TaskDependencyGraph;
    };
    
    struct TDG_Edge {
        TDG_Node* _source;
        TDG_Node* _target;
        ObjectList<Nodecl::NodeclBase> _source_clauses;
        ObjectList<Nodecl::NodeclBase> _target_clauses;
        
        TDG_Edge( TDG_Node* source, TDG_Node* target );
        TDG_Node* get_source( );
        TDG_Node* get_target( );
        
        friend class TDG_Node;
        friend class TaskDependencyGraph;
    };
    
    class LIBTL_CLASS TaskDependencyGraph
    {
    private:
        TDG_Node* _entry;
        TDG_Node* _exit;
        
        TDG_Node* _last_node;
        
        ExtensibleGraph* _pcfg;
        
        // *** Not allowed construction methods *** //
        TaskDependencyGraph( const TaskDependencyGraph& n );
        TaskDependencyGraph& operator=( const TaskDependencyGraph& );
        
        void connect_tdg_nodes( TDG_Node* parent, TDG_Node* child );
        
        void taskify_graph( Node* current );
        void create_tdg( Node* current );
        
        void print_tdg_to_dot_rec( TDG_Node* current, std::ofstream& dot_tdg );
        
    public:
        // *** Constructor *** //
        TaskDependencyGraph( ExtensibleGraph* pcfg );
        
        // *** Getters and Setters *** //
        std::string get_name( ) const;
        
        // *** Printing method *** //
        void print_tdg_to_dot( );
    };

}
}

#endif  // TL_TASK_DEPENDENCY_GRAPH_HPP