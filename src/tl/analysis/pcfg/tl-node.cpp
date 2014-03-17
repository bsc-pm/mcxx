/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#include <climits>

#include "cxx-codegen.h"
#include "cxx-process.h"

#include "tl-analysis-utils.hpp"
#include "tl-extended-symbol-utils.hpp"
#include "tl-edge.hpp"
#include "tl-extended-symbol-utils.hpp"
#include "tl-iv-analysis.hpp"
#include "tl-node.hpp"

namespace TL {
namespace Analysis {

    Node::Node( )
        : _id( INT_MAX ), _entry_edges( ), _exit_edges( ), _has_assertion( false ),
        _visited( false ), _visited_aux( false ), _visited_extgraph( false ), _visited_extgraph_aux( false )
    {
        set_data( _NODE_TYPE, __UnclassifiedNode );
    }

    Node::Node( unsigned int& id, Node_type ntype, Node* outer_node )
        : _id( ++id ), _entry_edges( ), _exit_edges( ), _has_assertion( false ),
          _visited( false ), _visited_aux( false ), _visited_extgraph( false ), _visited_extgraph_aux( false )
    {
        set_data( _NODE_TYPE, ntype );
        set_data( _OUTER_NODE, outer_node );

        if( ntype == __Graph )
        {
            set_data( _ENTRY_NODE, new Node( id, __Entry, NULL ) );
            unsigned int exit_id = INT_MAX - 1;
            set_data( _EXIT_NODE, new Node( exit_id, __Exit, NULL ) );
        }
    }

    Node::Node( unsigned int& id, Node_type type, Node* outer_node, ObjectList<Nodecl::NodeclBase> nodecls )
        : _id( ++id ), _entry_edges( ), _exit_edges( ), _has_assertion( false ),
          _visited( false ), _visited_aux( false ), _visited_extgraph( false ), _visited_extgraph_aux( false )
    {
        set_data( _NODE_TYPE, type );
        set_data( _OUTER_NODE, outer_node );

        set_data( _NODE_STMTS, nodecls );
    }

    Node::Node( unsigned int& id, Node_type type, Node* outer_node, Nodecl::NodeclBase nodecl )
        : _id( ++id ), _entry_edges( ), _exit_edges( ), _has_assertion( false ),
          _visited( false ), _visited_aux( false ), _visited_extgraph( false ), _visited_extgraph_aux( false )
    {
        set_data( _NODE_TYPE, type );
        set_data( _OUTER_NODE, outer_node );

        set_data( _NODE_STMTS, ObjectList<Nodecl::NodeclBase>( 1, nodecl ) );
    }

    bool Node::operator==( const Node& node ) const
    {
        return ( _id == node._id );
    }

    void Node::erase_entry_edge( Node* source )
    {
        ObjectList<Edge*>::iterator it;
        for ( it = _entry_edges.begin( ); it != _entry_edges.end( ); ++it )
        {
            if( ( *it )->get_source( ) == source )
            {
                _entry_edges.erase( it );
                --it;   // Decrement to allow the correctness of the comparison outside the loop
                break;
            }
        }
        if( it == _entry_edges.end( ) )
        {
            internal_error( "Trying to delete an non-existent edge between nodes '%d' and '%d'", source->_id, _id );
        }
    }

    void Node::erase_exit_edge( Node* target )
    {
        ObjectList<Edge*>::iterator it;
        for( it = _exit_edges.begin( ); it != _exit_edges.end( ); ++it )
        {
            if( ( *it )->get_target( ) == target )
            {
                _exit_edges.erase( it );
                --it;   // Decrement to allow the correctness of the comparison outside the loop
                break;
            }
        }
        if( it == _exit_edges.end( ) )
        {
            internal_error( "Trying to delete an non-existent edge between nodes '%d' and '%d'", _id, target->_id );
        }
    }

    int Node::get_id( ) const
    {
        return _id;
    }

    void Node::set_id( unsigned int id )
    {
        _id = id;
    }
    
    bool Node::has_usage_assertion( ) const
    {
        return ( has_key( _ASSERT_UPPER_EXPOSED ) || has_key( _ASSERT_KILLED ) || 
                 has_key( _ASSERT_DEAD ) );
    }
    
    bool Node::has_liveness_assertion( ) const
    {
        return ( has_key( _ASSERT_LIVE_IN ) || has_key( _ASSERT_LIVE_OUT ) );
    }
    
    bool Node::has_reach_defs_assertion( ) const
    {
        return ( has_key( _ASSERT_REACH_DEFS_IN ) || has_key( _ASSERT_REACH_DEFS_OUT ) );
    }
    
    bool Node::has_induction_vars_assertion( ) const
    {
        return has_key( _ASSERT_INDUCTION_VARS );
    }
    
    bool Node::has_autoscope_assertion( ) const
    {
        return ( has_key( _ASSERT_AUTOSC_FIRSTPRIVATE ) || has_key( _ASSERT_AUTOSC_PRIVATE ) || 
                 has_key( _ASSERT_AUTOSC_SHARED ) );
    }
    
    bool Node::is_visited( ) const
    {
        return _visited;
    }

    bool Node::is_visited_aux( ) const
    {
        return _visited_aux;
    }

    bool Node::is_visited_extgraph( ) const
    {
        return _visited_extgraph;
    }

    bool Node::is_visited_extgraph_aux( ) const
    {
        return _visited_extgraph_aux;
    }
    
    void Node::set_visited( bool visited )
    {
        _visited = visited;
    }

    void Node::set_visited_aux( bool visited )
    {
        _visited_aux = visited;
    }

    void Node::set_visited_extgraph( bool visited )
    {
        _visited_extgraph = visited;
    }
    
    void Node::set_visited_extgraph_aux( bool visited )
    {
        _visited_extgraph_aux = visited;
    }
    
    bool Node::is_empty_node( )
    {
        return ( _id==-1 && is_unclassified_node( ) );
    }

    ObjectList<Edge*> Node::get_entry_edges( ) const
    {
        return _entry_edges;
    }

    void Node::set_entry_edge( Edge *entry_edge )
    {
        _entry_edges.append( entry_edge );
    }

    ObjectList<Edge_type> Node::get_entry_edge_types( )
    {
        ObjectList<Edge_type> result;
        for( ObjectList<Edge*>::iterator it = _entry_edges.begin( ); it != _entry_edges.end( ); ++it )
            result.append( ( *it )->get_type( ) );
        return result;
    }

    ObjectList<Nodecl::NodeclBase> Node::get_entry_edge_labels( )
    {
        ObjectList<Nodecl::NodeclBase> result;
        for( ObjectList<Edge*>::iterator it = _entry_edges.begin( ); it != _entry_edges.end( ); ++it )
            result.append( ( *it )->get_label( ) );
        return result;
    }

    ObjectList<Node*> Node::get_parents( )
    {
        ObjectList<Node*> result;
        for( ObjectList<Edge*>::iterator it = _entry_edges.begin( ); it != _entry_edges.end( ); ++it)
            result.append( ( *it )->get_source( ) );
        return result;
    }

    ObjectList<Edge*> Node::get_exit_edges( ) const
    {
        return _exit_edges;
    }

    void Node::set_exit_edge( Edge *exit_edge )
    {
        _exit_edges.append( exit_edge );
    }

    ObjectList<Edge_type> Node::get_exit_edge_types( )
    {
        ObjectList<Edge_type> result;
        for( ObjectList<Edge*>::iterator it = _exit_edges.begin( ); it != _exit_edges.end( ); ++it )
            result.append( ( *it )->get_type( ) );
        return result;
    }

    ObjectList<Nodecl::NodeclBase> Node::get_exit_edge_labels( )
    {
        ObjectList<Nodecl::NodeclBase> result;
        for( ObjectList<Edge*>::iterator it = _exit_edges.begin( ); it != _exit_edges.end( ); ++it )
            result.append( ( *it )->get_label( ) );
        return result;
    }

    Edge* Node::get_exit_edge( Node* target )
    {
        Edge* result = NULL;
        int id = target->get_id( );
        for( ObjectList<Edge*>::iterator it = _exit_edges.begin( ); it != _exit_edges.end( ); ++it )
        {
            if( ( *it )->get_target( )->get_id( ) == id )
            {
                result = *it;
                break;
            }
        }
        return result;
    }

    ObjectList<Node*> Node::get_children( )
    {
        ObjectList<Node*> result;
        for( ObjectList<Edge*>::iterator it = _exit_edges.begin( ); it != _exit_edges.end( ); ++it )
        {
            result.append( ( *it )->get_target( ) );
        }
        return result;
    }

    bool Node::node_is_enclosed_by( Node* potential_encloser )
    {
        Node* outer_node = get_outer_node( );
        while( ( outer_node != NULL )
               && ( outer_node->get_id( ) != potential_encloser->get_id( ) ) )
        {
            outer_node = outer_node->get_outer_node( );
        }
        if( outer_node == NULL )
        {
            return false;
        }
        else
        {
            return true;
        }
    }

    bool Node::is_basic_node( )
    {
        return ( get_type( ) != __Graph );
    }

    bool Node::is_graph_node( )
    {
        return ( get_type( ) == __Graph );
    }

    bool Node::is_extended_graph_node( )
    {
        return ( ( get_type( ) == __Graph ) && ( get_graph_type( ) == __ExtensibleGraph ) );
    }

    bool Node::is_entry_node( )
    {
        return ( get_type( ) == __Entry );
    }

    bool Node::is_exit_node( )
    {
        return ( get_type( ) == __Exit );
    }

    bool Node::is_break_node( )
    {
        return ( get_type( ) == __Break );
    }

    bool Node::is_continue_node( )
    {
        return ( get_type( ) == __Continue );
    }
    
    bool Node::is_context_node( )
    {
        return ( ( get_type( ) == __Graph ) && ( get_graph_type( ) == __Context ) );
    }

    bool Node::is_ifelse_statement( )
    {
        return ( ( get_type( ) == __Graph ) && get_graph_type( ) == __IfElse );
    }

    bool Node::is_switch_statement( )
    {
        return ( ( get_type( ) == __Graph ) && get_graph_type( ) == __Switch );
    }

    bool Node::is_switch_case_node( )
    {
        return ( ( get_type( ) == __Graph ) && get_graph_type( ) == __SwitchCase );
    }
    
    bool Node::is_goto_node( )
    {
        return ( get_type( ) == __Goto );
    }

    bool Node::is_split_statement( )
    {
        return ( is_graph_node( ) && get_graph_type( ) == __SplitStmt );
    }

    bool Node::is_unclassified_node( )
    {
        return ( get_type( ) == __UnclassifiedNode );
    }

    bool Node::is_graph_entry_node( Node* graph )
    {
        return ( _id == graph->get_graph_entry_node( )->get_id( ) );
    }

    bool Node::is_graph_exit_node( Node* graph )
    {
        return ( _id == graph->get_graph_exit_node( )->get_id( ) );
    }

    bool Node::is_loop_node( )
    {
        return ( ( get_type( ) == __Graph )
                 && ( ( get_graph_type( ) == __LoopDoWhile )
                        || ( get_graph_type( ) == __LoopFor )
                        || ( get_graph_type( ) == __LoopWhile ) ) );
    }

    bool Node::is_for_loop( )
    {
        return ( ( get_type( ) == __Graph ) && ( get_graph_type( ) == __LoopFor ) );
    }

    bool Node::is_while_loop( )
    {
        return ( ( get_type( ) == __Graph ) && ( get_graph_type( ) == __LoopWhile ) );
    }

    bool Node::is_do_loop( )
    {
        return ( ( get_type( ) == __Graph ) && ( get_graph_type( ) == __LoopDoWhile ) );
    }

    bool Node::is_loop_stride( Node* loop )
    {
        return ( loop->get_stride_node( )->get_id( ) == _id );
    }

    bool Node::is_normal_node( )
    {
        return ( get_type( ) == __Normal );
    }

    bool Node::is_labeled_node( )
    {
        return ( get_type( ) == __Labeled );
    }

    bool Node::is_function_call_graph_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __FunctionCallGraph ) );
    }
    
    bool Node::is_function_call_node( )
    {
        return ( get_type( ) == __FunctionCall );
    }
    
    bool Node::is_asm_def_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __AsmDef ) );
    }

    bool Node::is_asm_op_node( )
    {
        return ( get_type( ) == __AsmOp );
    }

    bool Node::is_omp_node( )
    {
        return ( is_omp_atomic_node( ) || is_omp_barrier_node( ) || is_omp_barrier_graph_node( ) || is_omp_critical_node( ) || 
                 is_omp_flush_node( ) || is_omp_loop_node( ) || is_omp_master_node( ) || is_omp_parallel_node( ) || 
                 is_omp_section_node( ) || is_omp_sections_node( ) || is_omp_simd_node( ) || is_omp_single_node( ) || 
                 is_omp_task_creation_node( ) || is_omp_task_node( ) || is_omp_taskwait_node( ) || is_omp_taskyield_node( ) );
    }
    
    bool Node::is_omp_atomic_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __OmpAtomic ) );
    }

    bool Node::is_omp_barrier_graph_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __OmpBarrierGraph ) );
    }
    
    bool Node::is_omp_barrier_node( )
    {
        return ( get_type( ) == __OmpBarrier );
    }

    bool Node::is_omp_critical_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __OmpCritical ) );
    }

    bool Node::is_omp_flush_node( )
    {
        return ( get_type( ) == __OmpFlush );
    }

    bool Node::is_omp_loop_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __OmpLoop ) );
    }

    bool Node::is_omp_master_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __OmpMaster ) );
    }

    bool Node::is_omp_parallel_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __OmpParallel ) );
    }

    bool Node::is_omp_section_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __OmpSection ) );
    }

    bool Node::is_omp_sections_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __OmpSections ) );
    }

    bool Node::is_omp_simd_node( )
    {
        Graph_type gt = get_graph_type( );
        return ( is_graph_node( ) 
                 && ( ( gt == __OmpSimd ) || ( gt == __OmpSimdFor ) 
                        || ( gt == __OmpSimdFunction ) || ( gt == __OmpSimdParallelFor ) ) );
    }
    
    bool Node::is_omp_single_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __OmpSingle ) );
    }

    // Fortran only
    bool Node::is_omp_workshare_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __OmpWorkshare ) );
    }

    bool Node::is_omp_task_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == __OmpTask ) );
    }

    bool Node::is_omp_task_creation_node( )
    {
        return ( get_type ( ) == __OmpTaskCreation );
    }

    bool Node::is_omp_taskwait_node( )
    {
        return ( get_type( ) == __OmpTaskwait );
    }

    bool Node::is_ompss_taskwait_on_node( )
    {
        return ( get_type( ) == __OmpWaitonDeps );
    }

    bool Node::is_omp_taskyield_node( )
    {
        return ( get_type( ) == __OmpTaskyield );
    }

    bool Node::is_omp_virtual_tasksync( )
    {
        return ( get_type( ) == __OmpVirtualTaskSync );
    }
    
    bool Node::is_vector_node( )
    {
        bool result = false;
        if( is_graph_node( ) )
        {
            Graph_type gt = get_graph_type( );
            if( ( gt == __VectorCondExpr ) || ( gt == __VectorFunctionCallGraph ) )
            {    
                result = true;
            }
        }
        else
        {
            Node_type nt = get_type( );
            if( ( nt == __VectorFunctionCall ) || ( nt == __VectorGather ) || ( nt == __VectorLoad ) || 
                ( nt == __VectorNormal ) || ( nt == __VectorReduction ) || ( nt == __VectorScatter ) || 
                ( nt == __VectorStore ) )
            {
                result = true;
            }
        }
        return result;
    }
    
    bool Node::is_connected( )
    {
        return ( !_entry_edges.empty( ) || !_exit_edges.empty( ) );
    }

    bool Node::has_child(Node* n)
    {
        bool result = false;
        int id = n->_id;

        for( ObjectList<Edge*>::iterator it = _exit_edges.begin( ); it != _exit_edges.end( ); ++it )
        {
            if( ( *it )->get_target( )->_id == id )
            {
                result = true;
                break;
            }
        }

        return result;
    }

    bool Node::has_parent( Node* n )
    {
        bool result = false;
        int id = n->_id;

        for( ObjectList<Edge*>::iterator it = _entry_edges.begin( ); it != _entry_edges.end( ); ++it )
        {
            if( ( *it )->get_source( )->_id == id )
            {
                result = true;
                break;
            }
        }

        return result;
    }

    Symbol Node::get_function_node_symbol( )
    {
        if( !is_function_call_node( ) )
            return Symbol( );

        Nodecl::NodeclBase stmt = get_statements( )[0];
        Symbol s;
        if( stmt.is<Nodecl::FunctionCall>( ) )
        {
            Nodecl::FunctionCall f = stmt.as<Nodecl::FunctionCall>( );
            s = f.get_called( ).get_symbol( );
        }
        else if( stmt.is<Nodecl::VirtualFunctionCall>( ) )
        {
            Nodecl::FunctionCall f = stmt.as<Nodecl::FunctionCall>( );
            s = f.get_called( ).get_symbol( );
        }

        return s;
    }

    bool Node::operator==( const Node* &n ) const
    {
        return ( ( _id == n->_id ) && ( _entry_edges == n->_entry_edges ) && ( _exit_edges == n->_exit_edges ) );
    }



    // ****************************************************************************** //
    // ********** Getters and setters for PCFG structural nodes and types *********** //

    Node_type Node::get_type( )
    {
        if( has_key( _NODE_TYPE ) )
            return get_data<Node_type>( _NODE_TYPE );
        else
            return __UnclassifiedNode;
    }

    void Node::set_type( Node_type t )
    {
        set_data( _NODE_TYPE, t );
    }

    //! Returns a string with the node type of the node.
    inline std::string node_type_to_str( Node_type nt )
    {
        switch( nt )
        {
            #undef NODE_TYPE
            #define NODE_TYPE(X) case __##X : return #X;
            NODE_TYPE_LIST
            #undef NODE_TYPE
            default: WARNING_MESSAGE( "Unexpected type of node '%d'", nt );
        }
        return "";
    }
    
    std::string Node::get_type_as_string( )
    {
        std::string type = "";
        if( has_key( _NODE_TYPE ) )
        {
            Node_type ntype = get_data<Node_type>( _NODE_TYPE );
            type = node_type_to_str( ntype );
        }
        else
        {
            internal_error( "The node '%s' has no type assigned, this operation is not allowed", 0 );
        }

        return type;
    }

    //! Returns a string with the graph type of the node.
    inline std::string graph_node_type_to_str( Graph_type gt )
    {
        switch( gt )
        {
            #undef GRAPH_TYPE
            #define GRAPH_TYPE(X) case __##X : return #X;
            GRAPH_NODE_TYPE_LIST
            #undef GRAPH_TYPE
            default: WARNING_MESSAGE( "Unexpected type of graph node '%d'", gt );
        }
        return "";
    }
    
    std::string Node::get_graph_type_as_string( )
    {
        std::string graph_type = "";
        if( has_key( _GRAPH_TYPE ) )
        {
            Graph_type ntype = get_data<Graph_type>( _GRAPH_TYPE );
            graph_type = graph_node_type_to_str( ntype );
        }
        else
        {
            internal_error( "The node '%s' has no graph type assigned, this operation is not allowed", 0 );
        }

        return graph_type;
    }

    Node* Node::get_graph_entry_node( )
    {
        Node* entry_node;
        if( is_graph_node( ) )
            entry_node = get_data<Node*>( _ENTRY_NODE );
        else
            internal_error( "Asking for the Entry Node of a non GRAPH node. Nodes of type '%s' do not have Entry node.",
                             get_type_as_string( ).c_str( ) );
        return entry_node;
    }

    void Node::set_graph_entry_node( Node* node )
    {
        if( !node->is_entry_node( ) )
        {
            internal_error( "Unexpected node type '%s' while setting the entry node to node '%d'. ENTRY expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
        else if( is_graph_node( ) )
            set_data( _ENTRY_NODE, node );
        else
            internal_error( "Unexpected node type '%s' while setting the entry node to node '%d'. GRAPH expected.",
                            get_type_as_string( ).c_str( ), _id );
    }

    Node* Node::get_graph_exit_node( )
    {
        Node* exit_node;
        if( is_graph_node( ) )
            exit_node = get_data<Node*>( _EXIT_NODE );
        else
            internal_error( "Asking for the Entry Node of a non GRAPH node. Nodes of type '%s' do not have Exit node.",
                             get_type_as_string( ).c_str( ) );
        return exit_node;
    }

    void Node::set_graph_exit_node( Node* node )
    {
        if( !node->is_exit_node( ) )
        {
            internal_error( "Unexpected node type '%s' while setting the exit node to node '%d'. EXIT expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
        else if( is_graph_node( ) )
            set_data( _EXIT_NODE, node );
        else
            internal_error( "Unexpected node type '%s' while setting the exit node to node '%d'. GRAPH expected.",
                            get_type_as_string( ).c_str( ), _id );
    }

    Nodecl::NodeclBase Node::get_graph_related_ast( )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == __Graph )
        {
            Nodecl::NodeclBase res = Nodecl::NodeclBase::null( );
            if( has_key( _NODE_LABEL ) )
                res = get_data<Nodecl::NodeclBase>( _NODE_LABEL, Nodecl::NodeclBase::null( ) );
            return res;
        }
        else
        {
            internal_error( "Unexpected node type '%s' while getting the label to node '%d'",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    void Node::set_graph_label( Nodecl::NodeclBase n )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == __Graph )
            set_data( _NODE_LABEL, n );
        else
            internal_error( "Unexpected node type '%s' while setting the label to node '%d'. GRAPH expected.",
                            get_type_as_string( ).c_str( ), _id );
    }

    Graph_type Node::get_graph_type( )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == __Graph )
            return get_data<Graph_type>( _GRAPH_TYPE );
        else
            internal_error("Unexpected node type '%s' while getting graph type to node '%d'. GRAPH expected.",
                           get_type_as_string( ).c_str( ), _id);
    }

    void Node::set_graph_type( Graph_type graph_type )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == __Graph )
            set_data( _GRAPH_TYPE, graph_type );
        else
            internal_error( "Unexpected node type '%s' while setting graph type to node '%d'. GRAPH expected.",
                            get_type_as_string( ).c_str( ), _id );
    }

    static bool node_is_claused_graph_omp( Graph_type type )
    {
        return ( type == __OmpAtomic || __OmpCritical
                 || type == __OmpLoop || type == __OmpParallel  || type == __OmpSections
                 || type == __OmpSingle || type == __OmpTask );
    }

    PCFGPragmaInfo Node::get_pragma_node_info( )
    {
        if( ( ( get_data<Node_type>( _NODE_TYPE ) == __Graph )
               && node_is_claused_graph_omp( get_data<Graph_type>( _GRAPH_TYPE ) ) )
            || get_data<Node_type>( _NODE_TYPE ) == __OmpFlush )
        {
            if( has_key( _OMP_INFO ) )
                return get_data<PCFGPragmaInfo>( _OMP_INFO );
            else
            {
                PCFGPragmaInfo p;
                return p;
            }
        }
        else
        {
            internal_error( "Unexpected node type '%s' while getting the OmpSs. OMP node expected.",
                            get_type_as_string( ).c_str( ) );
        }
    }

    void Node::set_pragma_node_info( PCFGPragmaInfo pragma )
    {
        if( ( ( get_data<Node_type>( _NODE_TYPE ) == __Graph )
               && node_is_claused_graph_omp( get_data<Graph_type>( _GRAPH_TYPE ) ) )
            || get_data<Node_type>( _NODE_TYPE ) == __OmpFlush )
        {
            set_data<PCFGPragmaInfo>( _OMP_INFO, pragma );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting the OmpSs node info. OMP_PRAGMA node expected",
                            get_type_as_string( ).c_str( ) );
        }
    }

    Node* Node::get_outer_node( )
    {
        Node* outer_node = NULL;
        if( has_key( _OUTER_NODE ) )
            outer_node = get_data<Node*>( _OUTER_NODE );
        return outer_node;
    }

    void Node::set_outer_node( Node* node )
    {
        set_data( _OUTER_NODE, node );
    }

    Scope Node::get_node_scope( )
    {
        // Get a nodecl included in the current node
        Nodecl::NodeclBase n = Nodecl::NodeclBase::null( );
        if( is_graph_node( ) )
        {
            n = get_graph_related_ast( );
        }
        else
        {
            ObjectList<Nodecl::NodeclBase> stmts = get_statements( );
            if( !stmts.empty( ) )
            {
                n = stmts[0];
            }
        }

        // Retrieve the context related to the nodecl
        if( !n.is_null( ) )
        {
            return n.retrieve_context( );
        }
        else
        {
            internal_error( "Node '%d' with no nodecl related. Retrieving an invalid scope", _id );
            return Scope( );
        }
    }

    bool Node::has_statements( )
    {
        return has_key( _NODE_STMTS );
    }

    ObjectList<Nodecl::NodeclBase> Node::get_statements( )
    {
        ObjectList<Nodecl::NodeclBase> stmts;
        if( has_key( _NODE_STMTS ) )
            stmts = get_data<ObjectList<Nodecl::NodeclBase> >( _NODE_STMTS );
        return stmts;
    }

    void Node::set_statements( ObjectList<Nodecl::NodeclBase> stmts )
    {
        if( is_normal_node( ) || is_function_call_node( )
            || is_labeled_node( ) || is_goto_node( )
            || is_break_node( ) || is_continue_node( ) )
        {
            set_data( _NODE_STMTS, stmts );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting the statements to node '%d'",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    Symbol Node::get_label( )
    {
        Node_type ntype = get_data<Node_type>( _NODE_TYPE );
        if( ntype == __Goto || ntype == __Labeled )
            return get_data<Symbol>( _NODE_LABEL );
        else
            internal_error( "Unexpected node type '%s' while getting the label to node '%d'. GOTO or LABELED NODES expected.",
                            get_type_as_string( ).c_str( ), _id );
    }

    void Node::set_label( Symbol s )
    {
        Node_type ntype = get_data<Node_type>( _NODE_TYPE );
        if( ntype == __Goto || ntype == __Labeled )
            set_data( _NODE_LABEL, s );
        else
            internal_error( "Unexpected node type '%s' while setting the label to node '%d'. GOTO or LABELED NODES expected.",
                            get_type_as_string( ).c_str( ), _id );
    }

    ASM_node_info Node::get_asm_info( )
    {
        Node* outer_node = get_outer_node( );
        if( get_type( ) == __AsmOp
            || ( outer_node != NULL && outer_node->get_graph_type( ) == __AsmDef ) )
        {
            return get_data<ASM_node_info>( _ASM_INFO );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while getting the ASM info from node '%d'. ASM node expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    void Node::set_asm_info( ASM_node_info inf )
    {
        Node* outer_node = get_outer_node( );
        if( get_type( ) == __AsmOp
            || ( outer_node != NULL && outer_node->get_graph_type( ) == __AsmDef ) )
        {
            set_data( _ASM_INFO, inf );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting the ASM info to node '%d'. ASM node expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    // ******** END Getters and setters for PCFG structural nodes and types ********* //
    // ****************************************************************************** //

    
    
    // ****************************************************************************** //
    // ****************** Getters and setters for PCFG analysis ********************* //
    
    AliveTaskSet& Node::get_live_in_tasks( )
    {
        return get_data<AliveTaskSet>( "live_tasks_in" );
    }
    
    AliveTaskSet& Node::get_live_out_tasks( )
    {
        return get_data<AliveTaskSet>( "live_tasks_out" );
    }

    StaticSyncTaskSet& Node::get_static_sync_in_tasks()
    {
        return get_data<StaticSyncTaskSet>("static_sync_tasks_in");
    }

    StaticSyncTaskSet& Node::get_static_sync_out_tasks()
    {
        return get_data<StaticSyncTaskSet>("static_sync_tasks_out");
    }
    
    // **************** END getters and setters for PCFG analysis ******************* //
    // ****************************************************************************** //
    

    
    // ****************************************************************************** //
    // **************** Getters and setters for constants analysis ****************** //

//     ObjectList<LatticeCellValue> Node::get_lattice_val( )
//     {
//         if( get_data<Node_type>( _NODE_TYPE ) != GRAPH )
//         {
//             ObjectList<LatticeCellValue> result;
//             if( has_key( _LATTICE_VALS ) )
//             {
//                 result = get_data<ObjectList<LatticeCellValue> >( _LATTICE_VALS );
//             }
//             return result;
//         }
//         else
//         {
//             internal_error( "Requesting Lattice Cell Values list in a GRAPH. Simple node expected.",
//                             get_type_as_string( ).c_str( ), _id );
//         }
//     }
//
//     void Node::set_lattice_val( LatticeCellValue lcv )
//     {
//         if( get_data<Node_type>( _NODE_TYPE ) != GRAPH )
//         {
//             ObjectList<LatticeCellValue> result;
//             if( has_key( _LATTICE_VALS ) )
//             {
//                 result = get_data<ObjectList<LatticeCellValue> >( _LATTICE_VALS );
//             }
//
//             result.append( lcv );
//             set_data( _LATTICE_VALS, result );
//         }
//         else
//         {
//             internal_error( "Requesting Lattice Cell Values list in a GRAPH. Simple node expected.",
//                             get_type_as_string( ).c_str( ), _id );
//         }
//     }

    // ************** END getters and setters for constants analysis **************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // *************** Getters and setters for use-definition analysis ************** //

    bool Node::usage_is_computed( )
    {
        return ( has_key( _UPPER_EXPOSED ) || has_key( _KILLED ) || has_key( _UNDEF ) );
    }
    
    bool Node::uses_var( const Nodecl::NodeclBase& n )
    {
        bool result = false;
        if( has_key( _UPPER_EXPOSED ) ) 
        {
            Utils::ext_sym_set ue_vars = get_data<Utils::ext_sym_set>( _UPPER_EXPOSED );
            if( ue_vars.find( n ) != ue_vars.end( ) )
                result = true;
        }
        if( !result && has_key( _KILLED ) )
        {
            Utils::ext_sym_set killed_vars = get_data<Utils::ext_sym_set>( _KILLED );
            if( killed_vars.find( n ) != killed_vars.end( ) )
                result = true;
        }
        if( !result && has_key( _UNDEF ) )
        {
            Utils::ext_sym_set undef_vars = get_data<Utils::ext_sym_set>( _UNDEF );
            if( undef_vars.find( n ) != undef_vars.end( ) )
                result = true;
        }
        return result;
    }
    
    Utils::ext_sym_set Node::get_ue_vars( )
    {
        Utils::ext_sym_set ue_vars;
        if( has_key( _UPPER_EXPOSED ) )
            ue_vars = get_data<Utils::ext_sym_set>( _UPPER_EXPOSED );
        return ue_vars;
    }

    void Node::add_ue_var( Utils::ExtendedSymbol new_ue_var )
    {
        Utils::ext_sym_set ue_vars = get_ue_vars( );
        if( Utils::ext_sym_set_contains_enclosing_nodecl( new_ue_var.get_nodecl( ), ue_vars).is_null( ) )
        {
            if( !Utils::ext_sym_set_contains_enclosed_nodecl( new_ue_var.get_nodecl( ), ue_vars ).is_null( ) )
                delete_enclosed_var_from_list( new_ue_var, ue_vars );
            
            ue_vars.insert(new_ue_var);
            set_data(_UPPER_EXPOSED, ue_vars);
        }
    }

    void Node::add_ue_var( Utils::ext_sym_set new_ue_vars )
    {
        Utils::ext_sym_set ue_vars = get_ue_vars( );
        Utils::ext_sym_set purged_ue_vars;
        Utils::ext_sym_set::iterator it = new_ue_vars.begin( );
        for( ; it != new_ue_vars.end( ); ++it )
        {
            if( Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl( ), ue_vars ).is_null( ) )
            {
                if( !Utils::ext_sym_set_contains_enclosed_nodecl( it->get_nodecl( ), ue_vars ).is_null( ) )
                    delete_enclosed_var_from_list( *it, ue_vars );
                
                purged_ue_vars.insert( *it );
            }
        }
        ue_vars.insert( purged_ue_vars.begin( ), purged_ue_vars.end( ) );
        set_data( _UPPER_EXPOSED, ue_vars );
    }

    void Node::set_ue_var( Utils::ext_sym_set new_ue_vars )
    {
        set_data(_UPPER_EXPOSED, new_ue_vars);
    }
    
    void Node::remove_ue_var( Utils::ExtendedSymbol old_ue_var )
    {
        Utils::ext_sym_set ue_vars = get_ue_vars( );
        ue_vars.erase( old_ue_var );
        set_data( _UPPER_EXPOSED, ue_vars );
    }

    Utils::ext_sym_set Node::get_private_ue_vars( )
    {
        Utils::ext_sym_set private_ue_vars;
        if( has_key( _PRIVATE_UPPER_EXPOSED ) )
            private_ue_vars = get_data<Utils::ext_sym_set>( _PRIVATE_UPPER_EXPOSED );
        return private_ue_vars;
    }

    void Node::add_private_ue_var( Utils::ext_sym_set new_private_ue_vars )
    {
        Utils::ext_sym_set private_ue_vars = get_private_ue_vars( );
        Utils::ext_sym_set purged_private_ue_vars;
        Utils::ext_sym_set::iterator it = new_private_ue_vars.begin( );
        for( ; it != new_private_ue_vars.end( ); ++it )
        {
            if( Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl( ), private_ue_vars ).is_null( ) )
            {
                if( !Utils::ext_sym_set_contains_enclosed_nodecl( it->get_nodecl( ), private_ue_vars ).is_null( ) )
                    delete_enclosed_var_from_list( *it, private_ue_vars );
                
                purged_private_ue_vars.insert( *it );
            }
        }
        private_ue_vars.insert( purged_private_ue_vars.begin( ), purged_private_ue_vars.end( ) );
        set_data( _PRIVATE_UPPER_EXPOSED, private_ue_vars );
    }

    Utils::ext_sym_set Node::get_killed_vars( )
    {
        Utils::ext_sym_set killed_vars;
        if( has_key( _KILLED ) )
            killed_vars = get_data<Utils::ext_sym_set>( _KILLED );
        return killed_vars;
    }

    void Node::add_killed_var( Utils::ExtendedSymbol new_killed_var )
    {
        Utils::ext_sym_set killed_vars = get_killed_vars( );
        if( Utils::ext_sym_set_contains_enclosing_nodecl( new_killed_var.get_nodecl( ), killed_vars ).is_null( ) )
        {
            if( !Utils::ext_sym_set_contains_enclosed_nodecl( new_killed_var.get_nodecl( ), killed_vars ).is_null( ) )
                delete_enclosed_var_from_list( new_killed_var, killed_vars );
            
            killed_vars.insert( new_killed_var );
            set_data( _KILLED, killed_vars );
        }
    }

    void Node::add_killed_var( Utils::ext_sym_set new_killed_vars )
    {
        Utils::ext_sym_set killed_vars = get_killed_vars( );
        Utils::ext_sym_set purged_killed_vars;
        Utils::ext_sym_set::iterator it = new_killed_vars.begin( );
        for( ; it != new_killed_vars.end( ); ++it )
        {
            if( Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl( ), killed_vars ).is_null( ) )
            {
                if( !Utils::ext_sym_set_contains_enclosed_nodecl( it->get_nodecl( ), killed_vars ).is_null( ) )
                    delete_enclosed_var_from_list( *it, killed_vars );
                purged_killed_vars.insert( *it );
            }
        }

        killed_vars.insert( purged_killed_vars.begin( ), purged_killed_vars.end( ) );
        set_data( _KILLED, killed_vars );
    }

    void Node::set_killed_var( Utils::ext_sym_set new_killed_vars )
    {
        set_data( _KILLED, new_killed_vars );
    }
    
    void Node::remove_killed_var( Utils::ExtendedSymbol old_killed_var )
    {
        Utils::ext_sym_set killed_vars = get_killed_vars( );
        killed_vars.erase( old_killed_var );
        set_data( _KILLED, killed_vars );
    }

    Utils::ext_sym_set Node::get_private_killed_vars( )
    {
        Utils::ext_sym_set private_killed_vars;
        if( has_key( _PRIVATE_KILLED ) )
            private_killed_vars = get_data<Utils::ext_sym_set>( _PRIVATE_KILLED );
        return private_killed_vars;
    }
    
    void Node::add_private_killed_var( Utils::ext_sym_set new_private_killed_vars )
    {
        Utils::ext_sym_set private_killed_vars = get_private_killed_vars( );
        Utils::ext_sym_set purged_private_killed_vars;
        Utils::ext_sym_set::iterator it = new_private_killed_vars.begin( );
        for( ; it != new_private_killed_vars.end( ); ++it )
        {
            if( Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl( ), private_killed_vars ).is_null( ) )
            {
                if( !Utils::ext_sym_set_contains_enclosed_nodecl( it->get_nodecl( ), private_killed_vars ).is_null( ) )
                    delete_enclosed_var_from_list( *it, private_killed_vars );
                purged_private_killed_vars.insert( *it );
            }
        }
        
        private_killed_vars.insert( purged_private_killed_vars.begin( ), purged_private_killed_vars.end( ) );
        set_data( _PRIVATE_KILLED, private_killed_vars );
    }
    
    Utils::ext_sym_set Node::get_undefined_behaviour_vars( )
    {
        Utils::ext_sym_set undef_vars;
        if( has_key( _UNDEF ) )
            undef_vars = get_data<Utils::ext_sym_set>( _UNDEF );
        return undef_vars;
    }

    void Node::add_undefined_behaviour_var( Utils::ExtendedSymbol new_undef_var )
    {
        Utils::ext_sym_set undef_vars = get_undefined_behaviour_vars( );
        if( Utils::ext_sym_set_contains_enclosing_nodecl( new_undef_var.get_nodecl( ), undef_vars ).is_null( ) )
        {
            if( !Utils::ext_sym_set_contains_enclosed_nodecl( new_undef_var.get_nodecl( ), undef_vars ).is_null( ) )
                delete_enclosed_var_from_list( new_undef_var, undef_vars );
            undef_vars.insert( new_undef_var );
            set_data( _UNDEF, undef_vars );
        }
    }

    void Node::add_undefined_behaviour_var( Utils::ext_sym_set new_undef_vars )
    {
        Utils::ext_sym_set undef_vars = get_undefined_behaviour_vars( );
        Utils::ext_sym_set purged_undef_vars;
        Utils::ext_sym_set::iterator it = new_undef_vars.begin( );
        for( ; it != new_undef_vars.end( ); ++it )
        {
            if( Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl( ), undef_vars ).is_null( ) )
            {
                if( !Utils::ext_sym_set_contains_enclosed_nodecl( it->get_nodecl( ), undef_vars ).is_null( ) )
                    delete_enclosed_var_from_list( *it, undef_vars );
                purged_undef_vars.insert( *it );
            }
        }
        undef_vars.insert( purged_undef_vars.begin( ), purged_undef_vars.end( ) );
        set_data( _UNDEF, undef_vars );
    }

    void Node::add_undefined_behaviour_var_and_recompute_use_and_killed_sets(
        Utils::ExtendedSymbol new_undef_var )
    {
        // Conservatively, delete the reference argument of UE and KILL sets
        if( has_key( _UPPER_EXPOSED ) )
            remove_ue_var( new_undef_var );
        if( has_key( _KILLED ) )
            remove_killed_var( new_undef_var );

        // Add the global variable to the UNDEF list
        add_undefined_behaviour_var( new_undef_var );
    }

    void Node::set_undefined_behaviour_var( Utils::ExtendedSymbol new_undef_var )
    {
        Utils::ext_sym_set new_undef_var_set;
        new_undef_var_set.insert( new_undef_var );
        set_data( _UNDEF, new_undef_var_set );
    }
    
    void Node::set_undefined_behaviour_var( Utils::ext_sym_set new_undef_vars )
    {
        set_data( _UNDEF, new_undef_vars );
    }
        
    void Node::remove_undefined_behaviour_var( Utils::ExtendedSymbol old_undef_var )
    {
        Utils::ext_sym_set undef_vars = get_undefined_behaviour_vars( );
        undef_vars.erase( old_undef_var );
        set_data( _UNDEF, undef_vars );
    }

    Utils::ext_sym_set Node::get_private_undefined_behaviour_vars( )
    {
        Utils::ext_sym_set private_undef_vars;
        if( has_key( _PRIVATE_UNDEF ) )
            private_undef_vars = get_data<Utils::ext_sym_set>( _PRIVATE_UNDEF );
        return private_undef_vars;
    }
    
    void Node::add_private_undefined_behaviour_var( Utils::ext_sym_set new_private_undef_vars )
    {
        Utils::ext_sym_set private_undef_vars = get_private_undefined_behaviour_vars( );
        Utils::ext_sym_set purged_private_undef_vars;
        Utils::ext_sym_set::iterator it = new_private_undef_vars.begin( );
        for( ; it != new_private_undef_vars.end( ); ++it )
        {
            if( Utils::ext_sym_set_contains_enclosing_nodecl( it->get_nodecl( ), private_undef_vars ).is_null( ) )
            {
                if( !Utils::ext_sym_set_contains_enclosed_nodecl( it->get_nodecl( ), private_undef_vars ).is_null( ) )
                    delete_enclosed_var_from_list( *it, private_undef_vars );
                purged_private_undef_vars.insert( *it );
            }
        }
        private_undef_vars.insert( purged_private_undef_vars.begin( ), purged_private_undef_vars.end( ) );
        set_data( _PRIVATE_UNDEF, private_undef_vars );
    }
    
    // ************* END getters and setters for use-definition analysis ************ //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ****************** Getters and setters for liveness analysis ***************** //

    Utils::ext_sym_set Node::get_live_in_vars( )
    {
        Utils::ext_sym_set live_in_vars;
        if( has_key( _LIVE_IN ) )
            live_in_vars = get_data<Utils::ext_sym_set>( _LIVE_IN );
        return live_in_vars;
    }

    void Node::set_live_in(Utils::ExtendedSymbol new_live_in_var)
    {
        Utils::ext_sym_set live_in_vars = get_live_in_vars( );
        live_in_vars.insert( new_live_in_var );
        set_data( _LIVE_IN, live_in_vars );
    }

    void Node::set_live_in(Utils::ext_sym_set new_live_in_set)
    {
        set_data(_LIVE_IN, new_live_in_set);
    }

    Utils::ext_sym_set Node::get_live_out_vars( )
    {
        Utils::ext_sym_set live_out_vars;
        if( has_key( _LIVE_OUT ) )
            live_out_vars = get_data<Utils::ext_sym_set>( _LIVE_OUT );
        return live_out_vars;
    }

    void Node::set_live_out( Utils::ExtendedSymbol new_live_out_var )
    {
        Utils::ext_sym_set live_out_vars = get_live_out_vars( );
        live_out_vars.insert( new_live_out_var );
        set_data( _LIVE_OUT, live_out_vars );
    }

    void Node::set_live_out( Utils::ext_sym_set new_live_out_set )
    {
        set_data( _LIVE_OUT, new_live_out_set );
    }

    // **************** END getters and setters for liveness analysis *************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ************ Getters and setters for reaching definitions analysis *********** //

    Utils::ext_sym_map Node::get_generated_stmts( )
    {
        Utils::ext_sym_map gen_stmts;
        if( has_key( _GEN ) )
            gen_stmts = get_data<Utils::ext_sym_map>( _GEN );
        return gen_stmts;
    }

    Utils::ext_sym_map Node::set_generated_stmts( Utils::ext_sym_map gen )
    {
        Utils::ext_sym_map gen_stmts;
        if( has_key( _GEN ) )
        {
            gen_stmts = get_data<Utils::ext_sym_map>( _GEN );
            for( Utils::ext_sym_map::iterator it = gen.begin( ); it != gen.end( ); ++it )
            {
                if( gen_stmts.find( it->first ) != gen_stmts.end( ) )
                    gen_stmts.erase( it->first );
            }
        }
        gen_stmts.insert( gen.begin( ), gen.end( ) );
        set_data( _GEN, gen_stmts );
        return gen_stmts;
    }

    Utils::ext_sym_map Node::get_reaching_definitions_in( )
    {
        Utils::ext_sym_map reaching_defs_in;
        if( has_key( _REACH_DEFS_IN ) )
            reaching_defs_in = get_data<Utils::ext_sym_map>( _REACH_DEFS_IN );
        return reaching_defs_in;
    }

    Utils::ext_sym_map Node::get_reaching_definitions_out( )
    {
        Utils::ext_sym_map reaching_defs_out;
        if( has_key( _REACH_DEFS_OUT ) )
            reaching_defs_out = get_data<Utils::ext_sym_map>( _REACH_DEFS_OUT );
        return reaching_defs_out;
    }

    void Node::set_reaching_definition_in( Utils::ExtendedSymbol var, Nodecl::NodeclBase init )
    {
        Utils::ext_sym_map reaching_defs_in = get_reaching_definitions_in( );
        reaching_defs_in.insert( std::pair<Utils::ExtendedSymbol, Nodecl::NodeclBase>( var, init ) );
        set_data( _REACH_DEFS_IN, reaching_defs_in );
    }

    void Node::set_reaching_definitions_in( Utils::ext_sym_map reach_defs_in )
    {
        set_data( _REACH_DEFS_IN, reach_defs_in );
    }

    void Node::set_reaching_definition_out( Utils::ExtendedSymbol var, Nodecl::NodeclBase init )
    {
        Utils::ext_sym_map reaching_defs_out = get_reaching_definitions_out( );
        reaching_defs_out.insert( std::pair<Utils::ExtendedSymbol, Nodecl::NodeclBase>( var, init ) );
        set_data( _REACH_DEFS_OUT, reaching_defs_out );
    }

    void Node::set_reaching_definitions_out( Utils::ext_sym_map reach_defs_out )
    {
        set_data( _REACH_DEFS_OUT, reach_defs_out );
    }

    // ********** END getters and setters for reaching definitions analysis ********* //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ******************* Getters and setters for loops analysis ******************* //

    ObjectList<Utils::InductionVariableData*> Node::get_induction_variables( )
    {
        ObjectList<Utils::InductionVariableData*> ivs;
        if( is_loop_node( ) || is_omp_loop_node( ) )
        {
            if( has_key( _INDUCTION_VARS ))
                ivs = get_data<ObjectList<Utils::InductionVariableData*> >( _INDUCTION_VARS );
        }
        else
        {
            internal_error( "Asking for induction_variables in a node '%d' of type '%s'. Loop expected",
                             _id, get_type_as_string( ).c_str( ) );
        }
        return ivs;
    }

    void Node::set_induction_variable( Utils::InductionVariableData* iv )
    {
        if( is_loop_node( ) || is_omp_loop_node( ) )
        {
            ObjectList<Utils::InductionVariableData*> ivs;
            if( has_key( _INDUCTION_VARS ) )
                ivs = get_data<ObjectList<Utils::InductionVariableData*> >( _INDUCTION_VARS );

            ivs.insert( iv );
            set_data( _INDUCTION_VARS, ivs );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting a induction variable in the graph node '%d'. LOOP expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    Node* Node::get_condition_node( )
    {
        if( is_graph_node() )
        {
            if( is_loop_node() || is_switch_statement() || is_ifelse_statement() )
                return get_data<Node*>( _CONDITION_NODE );
            
            internal_error( "Unexpected graph type '%s' while getting the condition node of loop node '%d'. LOOP|SWITCH|IFELSE expected",
                            get_graph_type_as_string( ).c_str( ), _id );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while getting condition node of loop graph node '%d'. GRAPH NODE expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }
    
    void Node::set_condition_node( Node* cond )
    {
        if( is_graph_node( ) )
        {
            if( is_loop_node() || is_switch_statement() || is_ifelse_statement() )
            {
                set_data( _CONDITION_NODE, cond );
            }
            else
            {
                internal_error( "Unexpected graph type '%s' while setting the condition node to loop node '%d'. LOOP expected",
                                get_graph_type_as_string( ).c_str( ), _id );
            }
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting condition node to loop graph node '%d'. GRAPH NODE expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }
    
    // FIXME Other loop nodes can have a stride
    Node* Node::get_stride_node( )
    {
        if( is_graph_node( ) )
        {
            if( is_for_loop( ) )
            {
                return get_data<Node*>( _STRIDE_NODE );
            }
            else
            {
                internal_error( "Unexpected graph type '%s' while getting the stride node of loop node '%d'. LOOP expected",
                                get_graph_type_as_string( ).c_str( ), _id );
            }
        }
        else
        {
            internal_error( "Unexpected node type '%s' while getting stride node of loop graph node '%d'. GRAPH NODE expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }
    
    // FIXME Other loop nodes can have a stride
    void Node::set_stride_node( Node* stride )
    {
        if( is_graph_node( ) )
        {
            if( is_for_loop( ) )
            {
                set_data( _STRIDE_NODE, stride );
            }
            else
            {
                internal_error( "Unexpected graph type '%s' while setting the stride node to loop node '%d'. LOOP expected",
                                get_graph_type_as_string( ).c_str( ), _id );
            }
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting stride node to loop graph node '%d'. GRAPH NODE expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    // FIXME Other loop nodes can have a stride
    bool Node::is_stride_node( )
    {
        bool res = false;
        Node* outer_node = get_outer_node( );
        while( outer_node != NULL && outer_node->get_graph_type( ) != __LoopFor )
        {
            outer_node = outer_node->get_outer_node( );
        }

        if( outer_node != NULL )
        {
            Node* stride = outer_node->get_stride_node( );
            res = ( stride->_id == _id );
        }
        return res;
    }

    bool Node::is_stride_node( Node* loop )
    {
        Node* stride = loop->get_stride_node( );
        return ( stride->_id == _id );
    }

    // ***************** END getters and setters for loops analysis ***************** //
    // ****************************************************************************** //
    
    
    
    // ****************************************************************************** //
    // ******************* Getters and setters for range analysis ******************* //
    
    Utils::ConstraintMap Node::get_constraints( )
    {
        Utils::ConstraintMap constraints;
        if( has_key( _CONSTRAINTS ) )
            constraints = get_data<Utils::ConstraintMap>( _CONSTRAINTS );
        return constraints;
    }
    
    Utils::ConstraintMap Node::get_propagated_constraints( )
    {
        Utils::ConstraintMap constraints;
        if( has_key( _PROPAGATED_CONSTRAINTS ) )
            constraints = get_data<Utils::ConstraintMap>( _PROPAGATED_CONSTRAINTS );
        return constraints;
    }
    
    Utils::ConstraintMap Node::get_all_constraints( )
    {
        Utils::ConstraintMap constraints;
        if( has_key( _PROPAGATED_CONSTRAINTS ) )
            constraints = get_data<Utils::ConstraintMap>( _PROPAGATED_CONSTRAINTS );
        if( has_key( _CONSTRAINTS ) )
        {
            Utils::ConstraintMap tmp = get_data<Utils::ConstraintMap>( _CONSTRAINTS );
            for(Utils::ConstraintMap::iterator it = tmp.begin(); it != tmp.end(); ++it )
                constraints[it->first] = it->second;
            
        }
        return constraints;
    }
    
    Utils::Constraint Node::get_constraint( const Nodecl::NodeclBase& var )
    {
        Utils::ConstraintMap constraints_map;
        if( has_key( _CONSTRAINTS ) )
            constraints_map = get_data<Utils::ConstraintMap>( _CONSTRAINTS );
        Utils::Constraint var_constraint;
        if( constraints_map.find( var ) != constraints_map.end( ) )
            var_constraint = constraints_map[var];
        return var_constraint;
    }
    
    void Node::set_constraints( Utils::ConstraintMap constraints )
    {
        set_data( _CONSTRAINTS, constraints );
    }
    
    void Node::set_propagated_constraints( Utils::ConstraintMap constraints )
    {
        set_data( _PROPAGATED_CONSTRAINTS, constraints );
    }
    
    Utils::RangeValuesMap Node::get_ranges_in( )
    {
        Utils::RangeValuesMap ranges_in;
        if( has_key( _RANGES_IN ) )
            ranges_in = get_data<Utils::RangeValuesMap>( _RANGES_IN );
        return ranges_in;
    }
    
    void Node::set_range_in( const Nodecl::NodeclBase& var, 
                             const ObjectList<Utils::RangeValue_tag>& values )
    {
        Utils::RangeValuesMap ranges_in = get_ranges_in( );
        ranges_in.insert( 
                std::pair<Nodecl::NodeclBase, ObjectList<Utils::RangeValue_tag> >( var, values ) );
        set_data( _RANGES_IN, ranges_in );
    }
    
    Utils::RangeValuesMap Node::get_ranges_out( )
    {
        Utils::RangeValuesMap ranges_out;
        if( has_key( _RANGES_OUT ) )
            ranges_out = get_data<Utils::RangeValuesMap>( _RANGES_OUT );
        return ranges_out;
    }
    
    void Node::set_range_out( const Nodecl::NodeclBase& var, 
                              const ObjectList<Utils::RangeValue_tag>& values )
    {
        Utils::RangeValuesMap ranges_out = get_ranges_out( );
        ranges_out.insert( 
                std::pair<Nodecl::NodeclBase, ObjectList<Utils::RangeValue_tag> >( var, values ) );
        set_data( _RANGES_OUT, ranges_out );
    }
    
    // ***************** END getters and setters for range analysis ***************** //
    // ****************************************************************************** //
    
    
    
    // ****************************************************************************** //
    // ******************* Getters and setters for OmpSs analysis ******************* //

    Nodecl::NodeclBase Node::get_task_context( )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == __Graph )
        {
            Graph_type graph_type = get_data<Graph_type>( _GRAPH_TYPE );
            if( graph_type == __OmpTask )
                return get_data<Nodecl::Context>( _TASK_CONTEXT );
            else
                internal_error( "Unexpected graph type '%s' while getting the context of the task node '%d'. " \
                                "\"task\" type expected", get_graph_type_as_string( ).c_str( ), _id );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while getting the context of the task node '%d'. \"task\" type expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    void Node::set_task_context( Nodecl::NodeclBase c )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == __Graph )
        {
            Graph_type graph_type = get_data<Graph_type>( _GRAPH_TYPE );
            if( graph_type == __OmpTask )
                return set_data( _TASK_CONTEXT, c );
            else
                internal_error( "Unexpected graph type '%s' while setting the context of the task node '%d'. " \
                                "\"task\" type expected", get_graph_type_as_string( ).c_str( ), _id );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting the label to node '%d'. GRAPH NODE expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    Symbol Node::get_task_function( )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == __Graph )
        {
            Graph_type graph_type = get_data<Graph_type>( _GRAPH_TYPE );
            if( graph_type == __OmpTask )
                return get_data<Symbol>( _TASK_FUNCTION );
            else
                internal_error( "Unexpected graph type '%s' while getting the symbol of the function embedded in the task '%s'. " \
                                "\"task\" type expected", get_graph_type_as_string( ).c_str( ),
                                get_data<Nodecl::NodeclBase>( _NODE_LABEL ).prettyprint( ).c_str( ) );
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the symbol of the function embedded in a task'. GRAPH NODE expected.",
                        get_type_as_string( ).c_str( ));
        }
    }

    void Node::set_task_function( Symbol func_sym )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == __Graph )
        {
            Graph_type graph_type = get_data<Graph_type>( _GRAPH_TYPE );
            if( graph_type == __OmpTask )
                return set_data( _TASK_FUNCTION, func_sym );
            else
                internal_error( "Unexpected graph type '%s' while setting the symbol of the function embedded in the task '%s'. " \
                                "\"task\" type expected", get_graph_type_as_string( ).c_str( ),
                                get_data<Nodecl::NodeclBase>( _NODE_LABEL ).prettyprint( ).c_str( ) );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting the symbol of the function embedded in a task. GRAPH NODE expected.",
                            get_type_as_string( ).c_str( ) );
        }
    }

    // ***************** END Getters and setters for OmpSs analysis ***************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // *************** Getters and setters for auto-scoping analysis **************** //

    bool Node::is_auto_scoping_enabled( )
    {
        return has_key( _SC_AUTO );
    }

    void Node::set_auto_scoping_enabled( )
    {
        set_data( _SC_AUTO, true );
    }

    Utils::ext_sym_set Node::get_sc_shared_vars( )
    {
        Utils::ext_sym_set sc_shared_vars;
        if( has_key( _SC_SHARED ) )
            sc_shared_vars = get_data<Utils::ext_sym_set>( _SC_SHARED );
        return sc_shared_vars;
    }

    void Node::set_sc_shared_var( Utils::ExtendedSymbol es )
    {
        Utils::ext_sym_set sc_shared_vars = get_sc_shared_vars( );
        sc_shared_vars.insert( es );
        set_data( _SC_SHARED, sc_shared_vars );
    }
    void Node::set_sc_shared_var( Utils::ext_sym_set es_list )
    {
        Utils::ext_sym_set sc_shared_vars = get_sc_shared_vars( );
        sc_shared_vars.insert( es_list.begin( ), es_list.end( ) );
        set_data( _SC_SHARED, sc_shared_vars );
    }

    Utils::ext_sym_set Node::get_sc_private_vars( )
    {
        Utils::ext_sym_set sc_private_vars;
        if( has_key( _SC_PRIVATE ) )
            sc_private_vars = get_data<Utils::ext_sym_set>( _SC_PRIVATE );
        return sc_private_vars;
    }

    void Node::set_sc_private_var( Utils::ExtendedSymbol es )
    {
        Utils::ext_sym_set sc_private_vars = get_sc_private_vars( );
        sc_private_vars.insert( es );
        set_data( _SC_PRIVATE, sc_private_vars );
    }
    void Node::set_sc_private_var( Utils::ext_sym_set es_list )
    {
        Utils::ext_sym_set sc_private_vars = get_sc_private_vars( );
        sc_private_vars.insert( es_list.begin( ), es_list.end( ) );
        set_data( _SC_PRIVATE, sc_private_vars );
    }

    Utils::ext_sym_set Node::get_sc_firstprivate_vars( )
    {
        Utils::ext_sym_set sc_firstprivate_vars;
        if( has_key( _SC_FIRSTPRIVATE ) )
            sc_firstprivate_vars = get_data<Utils::ext_sym_set>( _SC_FIRSTPRIVATE );
        return sc_firstprivate_vars;
    }

    void Node::set_sc_firstprivate_var( Utils::ExtendedSymbol es )
    {
        Utils::ext_sym_set sc_firstprivate_vars = get_sc_firstprivate_vars( );
        sc_firstprivate_vars.insert( es );
        set_data( _SC_FIRSTPRIVATE, sc_firstprivate_vars );
    }
    void Node::set_sc_firstprivate_var( Utils::ext_sym_set es_list )
    {
        Utils::ext_sym_set sc_firstprivate_vars = get_sc_firstprivate_vars( );
        sc_firstprivate_vars.insert( es_list.begin( ), es_list.end( ) );
        set_data( _SC_FIRSTPRIVATE, sc_firstprivate_vars );
    }

    Utils::ext_sym_set Node::get_sc_undef_vars( )
    {
        Utils::ext_sym_set undef_sc_vars;
        if( has_key( _SC_UNDEF ) )
            undef_sc_vars = get_data<Utils::ext_sym_set>( _SC_UNDEF );
        return undef_sc_vars;
    }

    void Node::set_sc_undef_var( Utils::ExtendedSymbol es )
    {
        Utils::ext_sym_set sc_undef_vars = get_sc_undef_vars( );
        sc_undef_vars.insert( es );
        set_data( _SC_UNDEF, sc_undef_vars );
    }
    void Node::set_sc_undef_var( Utils::ext_sym_set es_list )
    {
        Utils::ext_sym_set sc_undef_vars = get_sc_undef_vars( );
        sc_undef_vars.insert( es_list.begin( ), es_list.end( ) );
        set_data( _SC_UNDEF, sc_undef_vars );
    }

    Utils::ext_sym_set Node::get_sc_race_vars( )
    {
        Utils::ext_sym_set race_vars;
        if( has_key( _SC_RACE ) )
            race_vars = get_data<Utils::ext_sym_set>( _SC_RACE );
        return race_vars;
    }

    void Node::set_sc_race_var( Utils::ExtendedSymbol es )
    {
        Utils::ext_sym_set sc_race_vars = get_sc_race_vars( );
        sc_race_vars.insert( es );
        set_data( _SC_RACE, sc_race_vars );
    }

    Utils::AutoScopedVariables Node::get_auto_scoped_variables( )
    {
        Utils::ext_sym_set private_vars = get_sc_private_vars( );
        Utils::ext_sym_set firstprivate_vars = get_sc_firstprivate_vars( );
        Utils::ext_sym_set race_vars = get_sc_race_vars( );
        Utils::ext_sym_set shared_vars = get_sc_shared_vars( );
        Utils::ext_sym_set undef_vars = get_sc_undef_vars( );

        Utils::AutoScopedVariables res( private_vars, firstprivate_vars, race_vars,
                                        shared_vars, undef_vars );
        return res;
    }

    // ************* END getters and setters for auto-scoping analysis ************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ************** Getters and setters for task dependence analysis ************** //

    Utils::ext_sym_set Node::get_deps_private_vars( )
    {
        Utils::ext_sym_set deps_private_vars;
        if( has_key( _DEPS_PRIVATE ) )
            deps_private_vars = get_data<Utils::ext_sym_set>( _DEPS_PRIVATE );
        return deps_private_vars;
    }

    void Node::set_deps_private_vars( Utils::ext_sym_set new_deps_private_var )
    {
        Utils::ext_sym_set deps_private_vars = get_deps_private_vars( );
        deps_private_vars.insert( new_deps_private_var.begin( ), new_deps_private_var.end( ) );
        set_data( _DEPS_PRIVATE, deps_private_vars );
    }


    Utils::ext_sym_set Node::get_deps_firstprivate_vars( )
    {
        Utils::ext_sym_set deps_firstprivate_vars;
        if( has_key( _DEPS_FIRSTPRIVATE ) )
            deps_firstprivate_vars = get_data<Utils::ext_sym_set>( _DEPS_FIRSTPRIVATE );
        return deps_firstprivate_vars;
    }

    void Node::set_deps_firstprivate_vars( Utils::ext_sym_set new_deps_firstprivate_var )
    {
        Utils::ext_sym_set deps_firstprivate_vars = get_deps_firstprivate_vars( );
        deps_firstprivate_vars.insert( new_deps_firstprivate_var.begin( ), new_deps_firstprivate_var.end( ) );
        set_data( _DEPS_FIRSTPRIVATE, deps_firstprivate_vars );
    }

    Utils::ext_sym_set Node::get_deps_shared_vars( )
    {
        Utils::ext_sym_set deps_shared_vars;
        if( has_key( _DEPS_SHARED ) )
            deps_shared_vars = get_data<Utils::ext_sym_set>( _DEPS_SHARED );
        return deps_shared_vars;
    }

    void Node::set_deps_shared_vars( Utils::ext_sym_set new_deps_shared_var )
    {
        Utils::ext_sym_set deps_shared_vars = get_deps_shared_vars( );
        deps_shared_vars.insert( new_deps_shared_var.begin( ), new_deps_shared_var.end( ) );
        set_data( _DEPS_SHARED, deps_shared_vars );
    }

    Utils::ext_sym_set Node::get_deps_in_exprs( )
    {
        Utils::ext_sym_set in_deps;
        if( has_key( _DEPS_IN ) )
            in_deps = get_data<Utils::ext_sym_set>( _DEPS_IN );
        return in_deps;
    }

    void Node::set_deps_in_exprs( Utils::ext_sym_set new_in_deps )
    {
        Utils::ext_sym_set in_deps = get_deps_in_exprs( );
        in_deps.insert( new_in_deps.begin( ), new_in_deps.end( ) );
        set_data( _DEPS_IN, in_deps );
    }

    Utils::ext_sym_set Node::get_deps_out_exprs( )
    {
        Utils::ext_sym_set out_deps;
        if( has_key( _DEPS_OUT ) )
            out_deps = get_data<Utils::ext_sym_set>( _DEPS_OUT );
        return out_deps;
    }

    void Node::set_deps_out_exprs( Utils::ext_sym_set new_out_deps )
    {
        Utils::ext_sym_set out_deps = get_deps_out_exprs( );
        out_deps.insert( new_out_deps.begin( ), new_out_deps.end( ) );
        set_data( _DEPS_OUT, out_deps );
    }

    Utils::ext_sym_set Node::get_deps_inout_exprs( )
    {
        Utils::ext_sym_set inout_deps;
        if( has_key( _DEPS_INOUT ) )
            inout_deps = get_data<Utils::ext_sym_set>( _DEPS_INOUT );
        return inout_deps;
    }

    void Node::set_deps_inout_exprs( Utils::ext_sym_set new_inout_deps )
    {
        Utils::ext_sym_set inout_deps = get_deps_inout_exprs( );
        inout_deps.insert( new_inout_deps.begin( ), new_inout_deps.end( ) );
        set_data( _DEPS_INOUT, inout_deps );
    }

    Utils::ext_sym_set Node::get_deps_undef_vars( )
    {
        Utils::ext_sym_set undef_deps;
        if( has_key( _DEPS_UNDEF ) )
            undef_deps = get_data<Utils::ext_sym_set>( _DEPS_UNDEF );
        return undef_deps;
    }

    void Node::set_deps_undef_vars( Utils::ext_sym_set new_undef_deps )
    {
        Utils::ext_sym_set undef_deps = get_deps_undef_vars( );
        undef_deps.insert( new_undef_deps.begin( ), new_undef_deps.end( ) );
        set_data( _DEPS_UNDEF, undef_deps );
    }

    // ************ END getters and setters for task dependence analysis ************ //
    // ****************************************************************************** //


    
    // ****************************************************************************** //
    // **************** Getters and setters for vectorization analysis ************** //
    
    ObjectList<Symbol> Node::get_reductions( )
    {
        ObjectList<Symbol> result;
        const ObjectList<PCFGClause> clauses = this->get_pragma_node_info( ).get_clauses( );
        for( ObjectList<PCFGClause>::const_iterator it = clauses.begin( ); it != clauses.end( ); ++it )
        {
            if( it->get_clause( ) == __reduction )
            {
                Nodecl::List reductions = it->get_args( );
                for( Nodecl::List::iterator itr = reductions.begin( ); itr != reductions.end( ); ++itr )
                {
                    Symbol reduc( itr->as<Nodecl::OpenMP::ReductionItem>( ).get_reduced_symbol( ).get_symbol( ) );
                    ERROR_CONDITION( !reduc.is_valid( ), "Invalid symbol stored for Reduction argument '%s'", 
                                     itr->prettyprint( ).c_str( ) );
                    result.insert( reduc );
                }
                break;
            }
        }
        return result;
    }
    
    // ************** END getters and setters for vectorization analysis ************ //
    // ****************************************************************************** //
    
    
    
    // ****************************************************************************** //
    // ****************** Getters and setters for analysis checking ***************** //
    
    Utils::ext_sym_set Node::get_assert_ue_vars( )
    {
        Utils::ext_sym_set assert_ue_vars;
        if( has_key( _ASSERT_UPPER_EXPOSED ) )
            assert_ue_vars = get_data<Utils::ext_sym_set>( _ASSERT_UPPER_EXPOSED );
        return assert_ue_vars;
    }
    
    void Node::set_assert_ue_var( const Nodecl::List& new_assert_ue_vars )
    {
        Utils::ext_sym_set assert_ue_vars = get_assert_ue_vars( );
        for( Nodecl::List::const_iterator it = new_assert_ue_vars.begin( ); 
             it != new_assert_ue_vars.end( ); ++it )
        {
            Utils::ExtendedSymbol new_assert_ue_var( *it );
            if( Utils::ext_sym_set_contains_enclosing_nodecl( new_assert_ue_var.get_nodecl( ), assert_ue_vars ).is_null( ) )
            {
                assert_ue_vars.insert( new_assert_ue_var );
                set_data( _ASSERT_UPPER_EXPOSED, assert_ue_vars );
            }
        }
    }

    Utils::ext_sym_set Node::get_assert_killed_vars( )
    {
        Utils::ext_sym_set assert_killed_vars;
        if( has_key( _ASSERT_KILLED ) )
            assert_killed_vars = get_data<Utils::ext_sym_set>( _ASSERT_KILLED );
        return assert_killed_vars;
    }
    
    void Node::set_assert_killed_var( const Nodecl::List& new_assert_killed_vars )
    {
        Utils::ext_sym_set assert_killed_vars = get_assert_killed_vars( );
        for( Nodecl::List::const_iterator it = new_assert_killed_vars.begin( ); 
             it != new_assert_killed_vars.end( ); ++it )
        {
            Utils::ExtendedSymbol new_assert_killed_var( *it );
            if( Utils::ext_sym_set_contains_enclosing_nodecl( new_assert_killed_var.get_nodecl( ), assert_killed_vars ).is_null( ) )
            {
                assert_killed_vars.insert( new_assert_killed_var );
                set_data( _ASSERT_KILLED, assert_killed_vars );
            }
        }
    }
    
    Utils::ext_sym_set Node::get_assert_undefined_behaviour_vars( )
    {
        Utils::ext_sym_set assert_undefined_vars;
        if( has_key( _ASSERT_UNDEFINED ) )
            assert_undefined_vars = get_data<Utils::ext_sym_set>( _ASSERT_UNDEFINED );
        return assert_undefined_vars;
    }
    
    void Node::set_assert_undefined_behaviour_var( const Nodecl::List& new_assert_undefined_vars )
    {
        Utils::ext_sym_set assert_undefined_vars = get_assert_undefined_behaviour_vars( );
        for( Nodecl::List::const_iterator it = new_assert_undefined_vars.begin( ); 
             it != new_assert_undefined_vars.end( ); ++it )
        {
            Utils::ExtendedSymbol new_assert_undefined_var( *it );
            if( Utils::ext_sym_set_contains_enclosing_nodecl( new_assert_undefined_var.get_nodecl( ), assert_undefined_vars ).is_null( ) )
            {
                assert_undefined_vars.insert( new_assert_undefined_var );
                set_data( _ASSERT_UNDEFINED, assert_undefined_vars );
            }
        }
    }
    
    Utils::ext_sym_set Node::get_assert_live_in_vars( )
    {
        Utils::ext_sym_set assert_live_in_vars;
        if( has_key( _ASSERT_LIVE_IN ) )
            assert_live_in_vars = get_data<Utils::ext_sym_set>( _ASSERT_LIVE_IN );
        return assert_live_in_vars;
    }
    
    void Node::set_assert_live_in_var( const Nodecl::List& new_assert_live_in_vars )
    {
        Utils::ext_sym_set assert_live_in_vars = get_assert_live_in_vars( );
        for( Nodecl::List::const_iterator it = new_assert_live_in_vars.begin( ); 
             it != new_assert_live_in_vars.end( ); ++it )
        {
            Utils::ExtendedSymbol new_assert_live_in_var( *it );
            if( Utils::ext_sym_set_contains_enclosing_nodecl( new_assert_live_in_var.get_nodecl( ), assert_live_in_vars ).is_null( ) )
            {
                assert_live_in_vars.insert( new_assert_live_in_var );
                set_data( _ASSERT_LIVE_IN, assert_live_in_vars );
            }
        }
    }
    
    Utils::ext_sym_set Node::get_assert_live_out_vars( )
    {
        Utils::ext_sym_set assert_live_out_vars;
        if( has_key( _ASSERT_LIVE_OUT ) )
            assert_live_out_vars = get_data<Utils::ext_sym_set>( _ASSERT_LIVE_OUT );
        return assert_live_out_vars;
    }
    
    void Node::set_assert_live_out_var( const Nodecl::List& new_assert_live_out_vars )
    {
        Utils::ext_sym_set assert_live_out_vars = get_assert_live_out_vars( );
        for( Nodecl::List::const_iterator it = new_assert_live_out_vars.begin( ); 
             it != new_assert_live_out_vars.end( ); ++it )
        {
            Utils::ExtendedSymbol new_assert_live_out_var( *it );
            if( Utils::ext_sym_set_contains_enclosing_nodecl( new_assert_live_out_var.get_nodecl( ), assert_live_out_vars ).is_null( ) )
            {
                assert_live_out_vars.insert( new_assert_live_out_var );
                set_data( _ASSERT_LIVE_OUT, assert_live_out_vars );
            }
        }
    }
    
    Utils::ext_sym_set Node::get_assert_dead_vars( )
    {
        Utils::ext_sym_set assert_dead_vars;
        if( has_key( _ASSERT_DEAD ) )
            assert_dead_vars = get_data<Utils::ext_sym_set>( _ASSERT_DEAD );
        return assert_dead_vars;
    }
    
    void Node::set_assert_dead_var( const Nodecl::List& new_assert_dead_vars )
    {
        Utils::ext_sym_set assert_dead_vars = get_assert_dead_vars( );
        for( Nodecl::List::const_iterator it = new_assert_dead_vars.begin( ); 
             it != new_assert_dead_vars.end( ); ++it )
        {
            Utils::ExtendedSymbol new_assert_dead_var( *it );
            if( Utils::ext_sym_set_contains_enclosing_nodecl( new_assert_dead_var.get_nodecl( ), assert_dead_vars ).is_null( ) )
            {
                assert_dead_vars.insert( new_assert_dead_var );
                set_data( _ASSERT_DEAD, assert_dead_vars );
            }
        }
    }    
    
    Utils::ext_sym_map Node::get_assert_reaching_definitions_in( )
    {
        Utils::ext_sym_map assert_reach_defs_in;
        if( has_key( _ASSERT_REACH_DEFS_IN ) )
            assert_reach_defs_in = get_data<Utils::ext_sym_map>( _ASSERT_REACH_DEFS_IN );
        return assert_reach_defs_in;
    }

    void Node::set_assert_reaching_definitions_in( const Nodecl::List& new_assert_reach_defs_in )
    {   
        Utils::ext_sym_map assert_reach_defs_in = get_assert_reaching_definitions_in( );
        for( Nodecl::List::const_iterator it = new_assert_reach_defs_in.begin( ); 
             it != new_assert_reach_defs_in.end( ); ++it )
        {
            Nodecl::Analysis::ReachDefExpr rd = it->as<Nodecl::Analysis::ReachDefExpr>( );
            Utils::ExtendedSymbol rd_var( rd.get_expression( ) );
            assert_reach_defs_in.insert( 
                std::pair<Utils::ExtendedSymbol, Nodecl::NodeclBase>( rd_var, rd.get_value( ) ) );
        }
        set_data( _ASSERT_REACH_DEFS_IN, assert_reach_defs_in );
    }
    
    Utils::ext_sym_map Node::get_assert_reaching_definitions_out( )
    {
        Utils::ext_sym_map assert_reach_defs_out;
        if( has_key( _ASSERT_REACH_DEFS_OUT ) )
            assert_reach_defs_out = get_data<Utils::ext_sym_map>( _ASSERT_REACH_DEFS_OUT );
        return assert_reach_defs_out;
    }
    
    void Node::set_assert_reaching_definitions_out( const Nodecl::List& new_assert_reach_defs_out )
    {   
        Utils::ext_sym_map assert_reach_defs_out = get_assert_reaching_definitions_out( );
        for( Nodecl::List::const_iterator it = new_assert_reach_defs_out.begin( ); 
             it != new_assert_reach_defs_out.end( ); ++it )
        {
            Nodecl::Analysis::ReachDefExpr rd = it->as<Nodecl::Analysis::ReachDefExpr>( );
            Utils::ExtendedSymbol rd_var( rd.get_expression( ) );
            assert_reach_defs_out.insert( 
                std::pair<Utils::ExtendedSymbol, Nodecl::NodeclBase>( rd_var, rd.get_value( ) ) );
        }
        set_data( _ASSERT_REACH_DEFS_OUT, assert_reach_defs_out );
    }
    
    ObjectList<Utils::InductionVariableData*> Node::get_assert_induction_vars( )
    {
        ObjectList<Utils::InductionVariableData*> assert_induction_vars;
        if( has_key( _ASSERT_INDUCTION_VARS ) )
            assert_induction_vars = get_data<ObjectList<Utils::InductionVariableData*> >( _ASSERT_INDUCTION_VARS );
        return assert_induction_vars;
    }
    
    void Node::set_assert_induction_variables( const Nodecl::List& new_assert_induction_vars )
    {
        ObjectList<Utils::InductionVariableData*> assert_induction_vars = get_assert_induction_vars( );
        for( Nodecl::List::const_iterator it = new_assert_induction_vars.begin( ); 
            it != new_assert_induction_vars.end( ); ++it )
        {
            Nodecl::Analysis::InductionVarExpr iv = it->as<Nodecl::Analysis::InductionVarExpr>( );
            Utils::InductionVariableData* iv_data = 
                new Utils::InductionVariableData( Utils::ExtendedSymbol( iv.get_induction_variable( ) ) );
            iv_data->set_lb( iv.get_lower( ) );
            iv_data->set_ub( iv.get_upper( ) );
            iv_data->set_increment( iv.get_stride( ) );
            assert_induction_vars.insert( iv_data);
        }
        set_data( _ASSERT_INDUCTION_VARS, assert_induction_vars );
    }
    
    Utils::ext_sym_set Node::get_assert_auto_sc_firstprivate_vars( )
    {
        Utils::ext_sym_set assert_auto_sc_firstprivate_vars;
        if( has_key( _ASSERT_AUTOSC_FIRSTPRIVATE ) )
            assert_auto_sc_firstprivate_vars = get_data<Utils::ext_sym_set>( _ASSERT_AUTOSC_FIRSTPRIVATE );
        return assert_auto_sc_firstprivate_vars;
    }
    
    void Node::set_assert_auto_sc_firstprivate_var( const Nodecl::List& new_assert_auto_sc_fp )
    {
        Utils::ext_sym_set assert_auto_sc_firstprivate_vars = get_assert_auto_sc_firstprivate_vars( );
        for( Nodecl::List::const_iterator it = new_assert_auto_sc_fp.begin( ); 
             it != new_assert_auto_sc_fp.end( ); ++it )
        {
            Utils::ExtendedSymbol new_assert_auto_sc_firstprivate_vars( *it );
            if( Utils::ext_sym_set_contains_enclosing_nodecl( new_assert_auto_sc_firstprivate_vars.get_nodecl( ), 
                                                              assert_auto_sc_firstprivate_vars ).is_null( ) )
            {
                assert_auto_sc_firstprivate_vars.insert( new_assert_auto_sc_firstprivate_vars );
                set_data( _ASSERT_AUTOSC_FIRSTPRIVATE, assert_auto_sc_firstprivate_vars );
            }
        }
    }
    
    Utils::ext_sym_set Node::get_assert_auto_sc_private_vars( )
    {
        Utils::ext_sym_set assert_auto_sc_private_vars;
        if( has_key( _ASSERT_AUTOSC_PRIVATE ) )
            assert_auto_sc_private_vars = get_data<Utils::ext_sym_set>( _ASSERT_AUTOSC_PRIVATE );
        return assert_auto_sc_private_vars;
    }
    
    void Node::set_assert_auto_sc_private_var( const Nodecl::List& new_assert_auto_sc_p )
    {
        Utils::ext_sym_set assert_auto_sc_private_vars = get_assert_auto_sc_private_vars( );
        for( Nodecl::List::const_iterator it = new_assert_auto_sc_p.begin( ); 
            it != new_assert_auto_sc_p.end( ); ++it )
        {
            Utils::ExtendedSymbol new_assert_auto_sc_private_vars( *it );
            if( Utils::ext_sym_set_contains_enclosing_nodecl( new_assert_auto_sc_private_vars.get_nodecl( ), 
                                                              assert_auto_sc_private_vars ).is_null( ) )
            {
                assert_auto_sc_private_vars.insert( new_assert_auto_sc_private_vars );
                set_data( _ASSERT_AUTOSC_PRIVATE, assert_auto_sc_private_vars );
            }
        }
    }
    
    Utils::ext_sym_set Node::get_assert_auto_sc_shared_vars( )
    {
        Utils::ext_sym_set assert_auto_sc_shared_vars;
        if( has_key( _ASSERT_AUTOSC_SHARED ) )
            assert_auto_sc_shared_vars = get_data<Utils::ext_sym_set>( _ASSERT_AUTOSC_SHARED );
        return assert_auto_sc_shared_vars;
    }
    
    void Node::set_assert_auto_sc_shared_var( const Nodecl::List& new_assert_auto_sc_s )
    {
        Utils::ext_sym_set assert_auto_sc_shared_vars = get_assert_auto_sc_shared_vars( );
        for( Nodecl::List::const_iterator it = new_assert_auto_sc_s.begin( ); 
             it != new_assert_auto_sc_s.end( ); ++it )
        {
            Utils::ExtendedSymbol new_assert_auto_sc_shared_vars( *it );
            if( Utils::ext_sym_set_contains_enclosing_nodecl( new_assert_auto_sc_shared_vars.get_nodecl( ), 
                                                              assert_auto_sc_shared_vars ).is_null( ) )
            {
                assert_auto_sc_shared_vars.insert( new_assert_auto_sc_shared_vars );
                set_data( _ASSERT_AUTOSC_SHARED, assert_auto_sc_shared_vars );
            }
        }
    }
    
    // **************** END getters and setters for analysis checking *************** //
    // ****************************************************************************** //
    
    

    // ****************************************************************************** //
    // *********************************** Utils ************************************ //

    static std::string print_set( Utils::ext_sym_set es_set )
    {
        std::string result;
        for( Utils::ext_sym_set::iterator it = es_set.begin( ); it != es_set.end( ); ++it )
        {
            nodecl_t internal_n = it->get_nodecl( ).get_internal_nodecl( );
            result += std::string( codegen_to_str( internal_n, nodecl_retrieve_context( internal_n ) ) ) + ", ";
        }
        result.erase( result.end( ) - 2, result.end( ) );
        return result;
    }

    void Node::print_use_def_chains( )
    {
        if( VERBOSE )
        {
            Utils::ext_sym_set ue_vars = get_data<Utils::ext_sym_set>(_UPPER_EXPOSED);
            std::cerr << " - Upper Exposed: " << print_set( ue_vars ) << std::endl;

            Utils::ext_sym_set killed_vars = get_data<Utils::ext_sym_set>(_KILLED);
            std::cerr << " - Killed: " << print_set( killed_vars );

            Utils::ext_sym_set undef_vars = get_data<Utils::ext_sym_set>(_UNDEF);
            std::cerr << " - Undefined usage: " << print_set( undef_vars );
        }
    }

    void Node::print_liveness( )
    {
        if( VERBOSE )
        {
            Utils::ext_sym_set live_in_vars = get_data<Utils::ext_sym_set>(_LIVE_IN);
            std::cerr << " - Live in: " << print_set( live_in_vars ) << std::endl;

            Utils::ext_sym_set live_out_vars = get_data<Utils::ext_sym_set>(_LIVE_OUT);
            std::cerr << " - Live out: " << print_set( live_out_vars ) << std::endl;
        }
    }

    void Node::print_auto_scoping( )
    {
        if( VERBOSE )
        {
            Utils::ext_sym_set private_vars = get_sc_private_vars( );
            Utils::ext_sym_set firstprivate_vars = get_sc_firstprivate_vars( );
            Utils::ext_sym_set race_vars = get_sc_race_vars( );
            Utils::ext_sym_set shared_vars = get_sc_shared_vars( );
            Utils::ext_sym_set undef_vars = get_sc_undef_vars( );

            if( !private_vars.empty( ) )
                std::cerr << "   Variables autoscoped as private: "             << print_set( private_vars )      << std::endl;

            if( !firstprivate_vars.empty( ) )
                std::cerr << "   Variables autoscoped as firstprivate: "        << print_set( firstprivate_vars ) << std::endl;

            if( !race_vars.empty( ) )
                std::cerr << "   Variables autoscoped as race: "                << print_set( race_vars )         << std::endl;

            if( !shared_vars.empty( ) )
                std::cerr << "   Variables autoscoped as shared: "              << print_set( shared_vars )            << std::endl;

            if( !undef_vars.empty( ) )
                std::cerr << " Variables that cannot be automatically scoped: " << print_set( undef_vars )        << std::endl;
        }
    }

    void Node::print_task_dependencies( )
    {
        if( VERBOSE )
        {
            std::string private_s      = " - Private: "      + print_set( get_deps_private_vars( ) )      + "\n";
            std::string firstprivate_s = " - Firstprivate: " + print_set( get_deps_firstprivate_vars( ) ) + "\n";
            std::string shared_s       = " - Shared: "       + print_set( get_deps_shared_vars( ) )       + "\n";
            std::string in_s           = " - In deps: "      + print_set( get_deps_in_exprs( ) )          + "\n";
            std::string out_s          = " - Out deps: "     + print_set( get_deps_out_exprs( ) )         + "\n";
            std::string inout_s        = " - Inout deps: "   + print_set( get_deps_inout_exprs( ) )       + "\n";
            std::string undef_s        = " - Undef deps: "   + print_set( get_deps_undef_vars( ) )        + "\n";

            std::cerr << private_s << firstprivate_s << shared_s
                      << in_s << out_s << inout_s << undef_s << std::endl;
        }
    }

    // ********************************* END utils ********************************** //
    // ****************************************************************************** //
}
}
