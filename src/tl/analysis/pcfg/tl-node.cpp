/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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
            : _id( INT_MAX ), _entry_edges( ), _exit_edges( ), _visited( false ), _visited_aux( false )
    {
        set_data( _NODE_TYPE, UNCLASSIFIED_NODE );
    }

    Node::Node( unsigned int& id, Node_type ntype, Node* outer_node )
            : _id( ++id ), _entry_edges( ), _exit_edges( ), _visited( false ), _visited_aux( false )
    {
        set_data( _NODE_TYPE, ntype );
        set_data( _OUTER_NODE, outer_node );

        if( ntype == GRAPH )
        {
            set_data( _ENTRY_NODE, new Node( id, ENTRY, NULL ) );
            unsigned int exit_id = INT_MAX - 1;
            set_data( _EXIT_NODE, new Node( exit_id, EXIT, NULL ) );
        }
    }

    Node::Node( unsigned int& id, Node_type type, Node* outer_node, ObjectList<Nodecl::NodeclBase> nodecls )
            : _id( ++id ), _entry_edges( ), _exit_edges( ), _visited( false ), _visited_aux( false )
    {
        set_data( _NODE_TYPE, type );
        set_data( _OUTER_NODE, outer_node );

        set_data( _NODE_STMTS, nodecls );
    }

    Node::Node( unsigned int& id, Node_type type, Node* outer_node, Nodecl::NodeclBase nodecl )
            : _id( ++id ), _entry_edges( ), _exit_edges( ), _visited( false ), _visited_aux( false )
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
            std::cerr << " ** Node.cpp :: erase_entry_edge( ) ** "
                      << "Trying to delete an non-existent edge "
                      << "between nodes '" << source->_id << "' and '" << _id << "'" << std::endl;
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
            std::cerr << " ** Node.cpp :: exit_entry_edge( ) ** "
                      << "Trying to delete an non-existent edge "
                      << "between nodes '" << _id << "' and '" << target->_id << "'" << std::endl;

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

    bool Node::is_visited( ) const
    {
        return _visited;
    }

    bool Node::is_visited_aux( ) const
    {
        return _visited_aux;
    }

    void Node::set_visited( bool visited )
    {
        _visited = visited;
    }

    void Node::set_visited_aux( bool visited )
    {
        _visited_aux = visited;
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
        {
            result.append( ( *it )->get_type( ) );
        }

        return result;
    }

    ObjectList<std::string> Node::get_entry_edge_labels( )
    {
        ObjectList<std::string> result;

        for( ObjectList<Edge*>::iterator it = _entry_edges.begin( ); it != _entry_edges.end( ); ++it )
        {
            result.append( ( *it )->get_label( ) );
        }

        return result;
    }

    ObjectList<Node*> Node::get_parents( )
    {
        ObjectList<Node*> result;

        for( ObjectList<Edge*>::iterator it = _entry_edges.begin( ); it != _entry_edges.end( ); ++it)
        {
            result.append( ( *it )->get_source( ) );
        }

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
        {
            result.append( ( *it )->get_type( ) );
        }

        return result;
    }

    ObjectList<std::string> Node::get_exit_edge_labels( )
    {
        ObjectList<std::string> result;

        for( ObjectList<Edge*>::iterator it = _exit_edges.begin( ); it != _exit_edges.end( ); ++it )
        {
            result.append( ( *it )->get_label( ) );
        }

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
        while( ( get_outer_node( ) != NULL )
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
        return ( get_type( ) != GRAPH );
    }

    bool Node::is_graph_node( )
    {
        return ( get_type( ) == GRAPH );
    }

    bool Node::is_extended_graph_node( )
    {
        return ( ( get_type( ) == GRAPH ) && ( get_graph_type( ) == EXTENSIBLE_GRAPH ) );
    }

    bool Node::is_entry_node( )
    {
        return ( get_type( ) == ENTRY );
    }

    bool Node::is_exit_node( )
    {
        return ( get_type( ) == EXIT );
    }

    bool Node::is_break_node( )
    {
        return ( get_type( ) == BREAK );
    }

    bool Node::is_continue_node( )
    {
        return ( get_type( ) == CONTINUE );
    }

    bool Node::is_goto_node( )
    {
        return ( get_type( ) == GOTO );
    }

    bool Node::is_split_statement( )
    {
        return ( is_graph_node( ) && get_graph_type( ) ==  SPLIT_STMT );
    }

    bool Node::is_unclassified_node( )
    {
        return ( get_type( ) == UNCLASSIFIED_NODE );
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
        return ( ( get_type( ) == GRAPH )
                 && ( ( get_graph_type( ) == LOOP_DOWHILE )
                        || ( get_graph_type( ) == LOOP_FOR )
                        || ( get_graph_type( ) == LOOP_WHILE ) ) );
    }

    bool Node::is_for_loop( )
    {
        return ( ( get_type( ) == GRAPH )
                 && ( get_graph_type( ) == LOOP_FOR ) );
    }

    bool Node::is_while_loop( )
    {
        return ( ( get_type( ) == GRAPH )
                 && ( get_graph_type( ) == LOOP_WHILE ) );
    }

    bool Node::is_do_loop( )
    {
        return ( ( get_type( ) == GRAPH )
                 && ( get_graph_type( ) == LOOP_DOWHILE ) );
    }

    bool Node::is_loop_stride( Node* loop )
    {
        return ( loop->get_stride_node( )->get_id( ) == _id );
    }

    bool Node::is_normal_node( )
    {
        return ( get_type( ) == NORMAL );
    }

    bool Node::is_labeled_node( )
    {
        return ( get_type( ) == LABELED );
    }

    bool Node::is_function_call_node( )
    {
        return ( get_type( ) == FUNCTION_CALL );
    }

    bool Node::is_task_node( )
    {
        return ( is_graph_node( ) && ( get_graph_type( ) == OMP_TASK ) );
    }

    bool Node::is_connected( )
    {
        return (!_entry_edges.empty( ) || !_exit_edges.empty( ));
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
        {
            return Symbol( );
        }

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
            return UNCLASSIFIED_NODE;
    }

    std::string Node::get_type_as_string( )
    {
        std::string type = "";
        if( has_key( _NODE_TYPE ) )
        {
            Node_type ntype = get_data<Node_type>( _NODE_TYPE );
            switch( ntype )
            {
                case BREAK:              type = "BREAK";            break;
                case CONTINUE:           type = "CONTINUE";         break;
                case ENTRY:              type = "ENTRY";            break;
                case EXIT:               type = "EXIT";             break;
                case FUNCTION_CALL:      type = "FUNCTION_CALL";    break;
                case GOTO:               type = "GOTO";             break;
                case LABELED:            type = "LABELED";          break;
                case NORMAL:             type = "NORMAL";           break;
                case OMP_BARRIER:        type = "OMP_BARRIER";      break;
                case OMP_FLUSH:          type = "OMP_FLUSH";        break;
                case OMP_TASKWAIT:       type = "OMP_TASKWAIT";     break;
                case OMP_TASKYIELD:      type = "OMP_TASKYIELD";    break;
                case GRAPH:              type = "GRAPH";            break;
                case UNCLASSIFIED_NODE:  type = "UNCLASSIFIED";     break;
                default:                 WARNING_MESSAGE( "Unexpected type of node '%d'", ntype );
            };
        }
        else
        {
            internal_error( "The node '%s' has no type assigned, this operation is not allowed", 0 );
        }

        return type;
    }

    std::string Node::get_graph_type_as_string( )
    {
        std::string graph_type = "";
        if( has_key( _GRAPH_TYPE ) )
        {
            Graph_type ntype = get_data<Graph_type>( _GRAPH_TYPE );
            switch( ntype )
            {
                case COND_EXPR:         graph_type = "COND_EXPR";           break;
                case EXTENSIBLE_GRAPH:  graph_type = "EXTENSIBLE_GRAPH";    break;
                case FUNC_CALL:         graph_type = "FUNC_CALL";           break;
                case IF_ELSE:           graph_type = "IF_ELSE";             break;
                case LOOP_DOWHILE:      graph_type = "LOOP_DOWHILE";        break;
                case LOOP_FOR:          graph_type = "LOOP_FOR";            break;
                case LOOP_WHILE:        graph_type = "LOOP_WHILE";          break;
                case OMP_ATOMIC:        graph_type = "OMP_ATOMIC";          break;
                case OMP_CRITICAL:      graph_type = "OMP_CRITICAL";        break;
                case OMP_LOOP:          graph_type = "OMP_LOOP";            break;
                case OMP_PARALLEL:      graph_type = "OMP_PARALLEL";        break;
                case OMP_SECTION:       graph_type = "OMP_SECTION";         break;
                case OMP_SECTIONS:      graph_type = "OMP_SECTIONS";        break;
                case OMP_SINGLE:        graph_type = "OMP_SINGLE";          break;
                case OMP_TASK:          graph_type = "OMP_TASK";            break;
                case OTHER:             graph_type = "OTHER";               break;
                case SIMD:              graph_type = "SIMD";                break;
                case SPLIT_STMT:        graph_type = "SPLIT_STMT";          break;
                case SWITCH:            graph_type = "SWITCH";              break;
                default:                WARNING_MESSAGE( "Unexpected type of node '%d'", ntype );
            };
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
        {
            entry_node = get_data<Node*>( _ENTRY_NODE );
        }
        else
        {
            WARNING_MESSAGE( "Asking for the Entry Node of a non GRAPH node. Nodes of type '%s' do not have Entry node.",
                             get_type_as_string( ).c_str( ) );
        }
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
        {
            return set_data( _ENTRY_NODE, node );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting the entry node to node '%d'. GRAPH expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    Node* Node::get_graph_exit_node( )
    {
        Node* exit_node;
        if( is_graph_node( ) )
        {
            exit_node = get_data<Node*>( _EXIT_NODE );
        }
        else
        {
            WARNING_MESSAGE( "Asking for the Entry Node of a non GRAPH node. Nodes of type '%s' do not have Exit node.",
                             get_type_as_string( ).c_str( ) );
        }
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
        {
            set_data( _EXIT_NODE, node );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting the exit node to node '%d'. GRAPH expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    Nodecl::NodeclBase Node::get_graph_label( )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == GRAPH )
        {
            Nodecl::NodeclBase res = Nodecl::NodeclBase::null( );
            if( has_key( _NODE_LABEL ) )
            {
                res = get_data<Nodecl::NodeclBase>( _NODE_LABEL, Nodecl::NodeclBase::null( ) );
            }
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
        if( get_data<Node_type>( _NODE_TYPE ) == GRAPH )
        {
            set_data( _NODE_LABEL, n );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting the label to node '%d'. GRAPH expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    Graph_type Node::get_graph_type( )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == GRAPH )
        {
            return get_data<Graph_type>( _GRAPH_TYPE );
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting graph type to node '%d'. GRAPH expected.",
                           get_type_as_string( ).c_str( ), _id);
        }
    }

    void Node::set_graph_type( Graph_type graph_type )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == GRAPH )
        {
            set_data( _GRAPH_TYPE, graph_type );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting graph type to node '%d'. GRAPH expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    static bool node_is_claused_graph_omp( Graph_type type )
    {
        return ( type == OMP_ATOMIC || OMP_CRITICAL
        || type == OMP_LOOP || type == OMP_PARALLEL  || OMP_SECTIONS
        || type == OMP_SINGLE || type == OMP_TASK );
    }

    PCFGPragmaInfo Node::get_omp_node_info( )
    {
        if( ( ( get_data<Node_type>( _NODE_TYPE ) == GRAPH )
            && node_is_claused_graph_omp( get_data<Graph_type>( _GRAPH_TYPE ) ) )
            || get_data<Node_type>( _NODE_TYPE ) == OMP_FLUSH )
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

    void Node::set_omp_node_info( PCFGPragmaInfo pragma )
    {
        if( ( ( get_data<Node_type>( _NODE_TYPE ) == GRAPH )
            && node_is_claused_graph_omp( get_data<Graph_type>( _GRAPH_TYPE ) ) )
            || get_data<Node_type>( _NODE_TYPE ) == OMP_FLUSH )
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
        {
            outer_node = get_data<Node*>( _OUTER_NODE );
        }
        return outer_node;
    }

    void Node::set_outer_node( Node* node )
    {
        if( node->is_graph_node( ) )
        {
            set_data( _OUTER_NODE, node );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting the exit node to node '%d'. GRAPH expected.",
                            node->get_type_as_string( ).c_str( ), _id );
        }
    }

    Scope Node::get_node_scope( )
    {
        // Get a nodecl included in the current node
        Nodecl::NodeclBase n = Nodecl::NodeclBase::null( );
        if( is_graph_node( ) )
        {
            n = get_graph_label( );
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
            WARNING_MESSAGE( "Node '%d' with no nodecl related. Retrieving an invalid scope", _id );
            return Scope( );
        }
    }

    ObjectList<Nodecl::NodeclBase> Node::get_statements( )
    {
        ObjectList<Nodecl::NodeclBase> stmts;
        if( has_key( _NODE_STMTS ) )
        {
            stmts = get_data<ObjectList<Nodecl::NodeclBase> >( _NODE_STMTS );
        }
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
        if( ntype == GOTO || ntype == LABELED )
        {
            return get_data<Symbol>( _NODE_LABEL );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while getting the label to node '%d'. GOTO or LABELED NODES expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    void Node::set_label( Symbol s )
    {
        Node_type ntype = get_data<Node_type>( _NODE_TYPE );
        if( ntype == GOTO || ntype == LABELED )
        {
            set_data( _NODE_LABEL, s );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting the label to node '%d'. GOTO or LABELED NODES expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    // ******** END Getters and setters for PCFG structural nodes and types ********* //
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

    Utils::ext_sym_set Node::get_ue_vars( )
    {
        Utils::ext_sym_set ue_vars;

        if( has_key( _UPPER_EXPOSED ) )
        {
            ue_vars = get_data<Utils::ext_sym_set>( _UPPER_EXPOSED );
        }

        return ue_vars;
    }

    void Node::set_ue_var(Utils::ExtendedSymbol new_ue_var)
    {
        Utils::ext_sym_set ue_vars;

        if(this->has_key(_UPPER_EXPOSED))
        {
            ue_vars = get_data<Utils::ext_sym_set>(_UPPER_EXPOSED);
        }
        if(!Utils::ext_sym_set_contains_englobing_nodecl(new_ue_var, ue_vars))
        {
            ue_vars.insert(new_ue_var);
            set_data(_UPPER_EXPOSED, ue_vars);
        }
    }

    void Node::set_ue_var(Utils::ext_sym_set new_ue_vars)
    {
        Utils::ext_sym_set ue_vars;

        if(this->has_key(_UPPER_EXPOSED))
        {
            ue_vars = get_data<Utils::ext_sym_set>(_UPPER_EXPOSED);
        }

        Utils::ext_sym_set purged_ue_vars;
        Utils::ext_sym_set::iterator it = new_ue_vars.begin( );
        for (; it != new_ue_vars.end( ); ++it)
        {
            if(!Utils::ext_sym_set_contains_englobing_nodecl(*it, ue_vars))
            {
                purged_ue_vars.insert(*it);
            }
        }
        if(it == new_ue_vars.end( ))
        {
            ue_vars.insert( purged_ue_vars.begin( ), purged_ue_vars.end( ) );
            set_data(_UPPER_EXPOSED, ue_vars);
        }
    }

    void Node::unset_ue_var( Utils::ExtendedSymbol old_ue_var )
    {
        Utils::ext_sym_set ue_vars;

        if( has_key( _UPPER_EXPOSED ) )
        {
            ue_vars = get_data<Utils::ext_sym_set>( _UPPER_EXPOSED );
            ue_vars.erase( old_ue_var );
        }

        set_data( _UPPER_EXPOSED, ue_vars );
    }

    Utils::ext_sym_set Node::get_killed_vars( )
    {
        Utils::ext_sym_set killed_vars;

        if( has_key( _KILLED ) )
        {
            killed_vars = get_data<Utils::ext_sym_set>( _KILLED );
        }

        return killed_vars;
    }

    void Node::set_killed_var( Utils::ExtendedSymbol new_killed_var )
    {
        Utils::ext_sym_set killed_vars;

        if( has_key( _KILLED ) )
        {
            killed_vars = get_data<Utils::ext_sym_set>(_KILLED);
        }

        if( !Utils::ext_sym_set_contains_englobing_nodecl( new_killed_var, killed_vars ) )
        {
            killed_vars.insert( new_killed_var );
            set_data( _KILLED, killed_vars );
        }
    }

    void Node::set_killed_var( Utils::ext_sym_set new_killed_vars )
    {
        Utils::ext_sym_set killed_vars;

        if( has_key( _KILLED ) )
        {
            killed_vars = get_data<Utils::ext_sym_set>( _KILLED );
        }

        Utils::ext_sym_set purged_killed_vars;
        Utils::ext_sym_set::iterator it = new_killed_vars.begin( );
        for( ; it != new_killed_vars.end( ); ++it )
        {
            if( !Utils::ext_sym_set_contains_englobing_nodecl( *it, killed_vars ) )
            {
                purged_killed_vars.insert( *it );
            }
        }
        if( it == new_killed_vars.end( ) )
        {
            killed_vars.insert( purged_killed_vars.begin( ), purged_killed_vars.end( ) );
            set_data( _KILLED, killed_vars );
        }
    }

    void Node::unset_killed_var( Utils::ExtendedSymbol old_killed_var )
    {
        Utils::ext_sym_set killed_vars;

        if( has_key( _KILLED ) )
        {
            killed_vars = get_data<Utils::ext_sym_set>( _KILLED );
            killed_vars.erase( old_killed_var );
        }

        set_data(_KILLED, killed_vars);
    }

    Utils::ext_sym_set Node::get_undefined_behaviour_vars( )
    {
        Utils::ext_sym_set undef_vars;

        if( has_key( _UNDEF ) )
        {
            undef_vars = get_data<Utils::ext_sym_set>( _UNDEF );
        }

        return undef_vars;
    }

    void Node::set_undefined_behaviour_var( Utils::ExtendedSymbol new_undef_var )
    {
        Utils::ext_sym_set undef_vars;

        if( has_key( _UNDEF ) )
        {
            undef_vars = get_data<Utils::ext_sym_set>( _UNDEF );
        }

        if( !Utils::ext_sym_set_contains_englobing_nodecl( new_undef_var, undef_vars ) )
        {
            undef_vars.insert( new_undef_var );
            set_data( _UNDEF, undef_vars );
        }
    }

    void Node::set_undefined_behaviour_var( Utils::ext_sym_set new_undef_vars )
    {
        Utils::ext_sym_set undef_vars;

        if( has_key( _UNDEF ) )
        {
            undef_vars = get_data<Utils::ext_sym_set>( _UNDEF );
        }

        Utils::ext_sym_set purged_undef_vars;
        Utils::ext_sym_set::iterator it = new_undef_vars.begin( );
        for( ; it != new_undef_vars.end( ); ++it )
        {
            if( !Utils::ext_sym_set_contains_englobing_nodecl( *it, undef_vars ) )
            {
                purged_undef_vars.insert( *it );
            }
        }
        if( it == new_undef_vars.end( ) )
        {
            undef_vars.insert( purged_undef_vars.begin( ), purged_undef_vars.end( ) );
            set_data( _UNDEF, undef_vars );
        }
    }

    void Node::set_undefined_behaviour_var_and_recompute_use_and_killed_sets(
        Utils::ExtendedSymbol new_undef_var )
    {
        // Conservatively, delete the reference argument of UE and KILL sets
        if( has_key( _UPPER_EXPOSED ) )
            unset_ue_var( new_undef_var );
        if( has_key( _KILLED ) )
            unset_killed_var( new_undef_var );

        // Add the global variable to the UNDEF list
            set_undefined_behaviour_var( new_undef_var );
    }

    void Node::unset_undefined_behaviour_var( Utils::ExtendedSymbol old_undef_var )
    {
        Utils::ext_sym_set undef_vars;

        if( has_key( _UNDEF ) )
        {
            undef_vars = get_data<Utils::ext_sym_set>( _UNDEF );
            undef_vars.erase( old_undef_var );
        }

        set_data( _UNDEF, undef_vars );
    }

    // ************* END getters and setters for use-definition analysis ************ //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ****************** Getters and setters for liveness analysis ***************** //

    Utils::ext_sym_set Node::get_live_in_vars( )
    {
        Utils::ext_sym_set live_in_vars;

        if(has_key(_LIVE_IN))
        {
            live_in_vars = get_data<Utils::ext_sym_set>(_LIVE_IN);
        }

        return live_in_vars;
    }

    void Node::set_live_in(Utils::ExtendedSymbol new_live_in_var)
    {
        Utils::ext_sym_set live_in_vars;

        if(has_key(_LIVE_IN))
        {
            live_in_vars = get_data<Utils::ext_sym_set>(_LIVE_IN);
        }
        live_in_vars.insert(new_live_in_var);

        set_data(_LIVE_IN, live_in_vars);
    }

    void Node::set_live_in(Utils::ext_sym_set new_live_in_set)
    {
        set_data(_LIVE_IN, new_live_in_set);
    }

    Utils::ext_sym_set Node::get_live_out_vars( )
    {
        Utils::ext_sym_set live_out_vars;

        if(has_key(_LIVE_OUT))
        {
            live_out_vars = get_data<Utils::ext_sym_set>(_LIVE_OUT);
        }

        return live_out_vars;
    }

    void Node::set_live_out( Utils::ExtendedSymbol new_live_out_var )
    {
        Utils::ext_sym_set live_out_vars;

        if( has_key( _LIVE_OUT ) )
        {
            live_out_vars = get_data<Utils::ext_sym_set>( _LIVE_OUT);
        }
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
                {
                    gen_stmts.erase( it->first );

                }
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
        {
            reaching_defs_in = get_data<Utils::ext_sym_map>( _REACH_DEFS_IN );
        }
        return reaching_defs_in;
    }

    Utils::ext_sym_map Node::get_reaching_definitions_out( )
    {
        Utils::ext_sym_map reaching_defs_out;
        if( has_key( _REACH_DEFS_OUT ) )
        {
            reaching_defs_out = get_data<Utils::ext_sym_map>( _REACH_DEFS_OUT );
        }
        return reaching_defs_out;
    }

    void Node::set_reaching_definition_in( Utils::ExtendedSymbol var, Nodecl::NodeclBase init )
    {
        Utils::ext_sym_map reaching_defs_in;
        if( has_key( _REACH_DEFS_IN ) )
        {
            reaching_defs_in = get_data<Utils::ext_sym_map>( _REACH_DEFS_IN );
        }
        reaching_defs_in.insert( std::pair<Utils::ExtendedSymbol, Nodecl::NodeclBase>( var, init ) );
        set_data( _REACH_DEFS_IN, reaching_defs_in );
    }

    void Node::set_reaching_definitions_in( Utils::ext_sym_map reach_defs_in )
    {
        set_data( _REACH_DEFS_IN, reach_defs_in );
    }

    void Node::set_reaching_definition_out( Utils::ExtendedSymbol var, Nodecl::NodeclBase init )
    {
        Utils::ext_sym_map reaching_defs_out;
        if( has_key( _REACH_DEFS_OUT ) )
        {
            reaching_defs_out = get_data<Utils::ext_sym_map>( _REACH_DEFS_OUT );
        }
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
        if( is_loop_node( ) )
        {
            if( has_key( _INDUCTION_VARS ))
                ivs = get_data<ObjectList<Utils::InductionVariableData*> >( _INDUCTION_VARS );
        }
        else
        {
            WARNING_MESSAGE( "Asking for induction_variables in a node '%d' of type '%s'. Loop expected",
                             _id, get_type_as_string( ).c_str( ) );
        }
        return ivs;
    }

    void Node::set_induction_variable( Utils::InductionVariableData* iv )
    {
        if( is_loop_node( ) )
        {
            ObjectList<Utils::InductionVariableData*> ivs;
            if( has_key( _INDUCTION_VARS ) )
            {
                ivs = get_data<ObjectList<Utils::InductionVariableData*> >( _INDUCTION_VARS );
            }

            ivs.insert( iv );
            set_data( _INDUCTION_VARS, ivs );
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting a induction variable in the graph node '%d'. LOOP expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    // FIXME Other loop nodes can have a stride
    Node* Node::get_stride_node( )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == GRAPH )
        {
            Graph_type graph_type = get_data<Graph_type>( _GRAPH_TYPE );
            if( graph_type == LOOP_FOR )
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
        if( get_data<Node_type>( _NODE_TYPE ) == GRAPH )
        {
            Graph_type graph_type = get_data<Graph_type>( _GRAPH_TYPE );
            if( graph_type == LOOP_FOR )
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
        while( outer_node != NULL && outer_node->get_graph_type( ) != LOOP_FOR)
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
    // ******************* Getters and setters for OmpSs analysis ******************* //

    Nodecl::NodeclBase Node::get_task_context( )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == GRAPH )
        {
            Graph_type graph_type = get_data<Graph_type>( _GRAPH_TYPE );
            if( graph_type == OMP_TASK )
            {
                return get_data<Nodecl::Context>( _TASK_CONTEXT );
            }
            else
            {
                internal_error( "Unexpected graph type '%s' while getting the context of the task node '%d'. " \
                                "\"task\" type expected", get_graph_type_as_string( ).c_str( ), _id );
            }
        }
        else
        {
            internal_error( "Unexpected node type '%s' while getting the context of the task node '%d'. \"task\" type expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    void Node::set_task_context( Nodecl::NodeclBase c )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == GRAPH )
        {
            Graph_type graph_type = get_data<Graph_type>( _GRAPH_TYPE );
            if( graph_type == OMP_TASK )
            {
                return set_data( _TASK_CONTEXT, c );
            }
            else
            {
                internal_error( "Unexpected graph type '%s' while setting the context of the task node '%d'. " \
                                "\"task\" type expected", get_graph_type_as_string( ).c_str( ), _id );
            }
        }
        else
        {
            internal_error( "Unexpected node type '%s' while setting the label to node '%d'. GRAPH NODE expected.",
                            get_type_as_string( ).c_str( ), _id );
        }
    }

    Symbol Node::get_task_function( )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == GRAPH )
        {
            Graph_type graph_type = get_data<Graph_type>( _GRAPH_TYPE );
            if( graph_type == OMP_TASK )
            {
                return get_data<Symbol>( _TASK_FUNCTION );
            }
            else
            {
                internal_error( "Unexpected graph type '%s' while getting the symbol of the function embedded in the task '%s'. " \
                                "\"task\" type expected", get_graph_type_as_string( ).c_str( ),
                                get_data<Nodecl::NodeclBase>( _NODE_LABEL ).prettyprint( ).c_str( ) );
            }
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the symbol of the function embedded in a task'. GRAPH NODE expected.",
                        get_type_as_string( ).c_str( ));
        }
    }

    void Node::set_task_function( Symbol func_sym )
    {
        if( get_data<Node_type>( _NODE_TYPE ) == GRAPH )
        {
            Graph_type graph_type = get_data<Graph_type>( _GRAPH_TYPE );
            if( graph_type == OMP_TASK )
            {
                return set_data( _TASK_FUNCTION, func_sym );
            }
            else
            {
                internal_error( "Unexpected graph type '%s' while setting the symbol of the function embedded in the task '%s'. " \
                                "\"task\" type expected", get_graph_type_as_string( ).c_str( ),
                                get_data<Nodecl::NodeclBase>( _NODE_LABEL ).prettyprint( ).c_str( ) );
            }
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

    Utils::ext_sym_set Node::get_sc_shared_vars( )
    {
        Utils::ext_sym_set sc_shared_vars;
        if( has_key( _SC_SHARED ) )
            sc_shared_vars = get_data<Utils::ext_sym_set>( _SC_SHARED );
        return sc_shared_vars;
    }

    void Node::set_sc_shared_var( Utils::ExtendedSymbol es )
    {
        Utils::ext_sym_set sc_shared_vars;
        if( has_key( _SC_SHARED ) )
            sc_shared_vars = get_sc_shared_vars( );
        sc_shared_vars.insert( es );
        set_data( _SC_SHARED, sc_shared_vars );
    }
    void Node::set_sc_shared_var( Utils::ext_sym_set es_list )
    {
        Utils::ext_sym_set sc_shared_vars;
        if( has_key( _SC_SHARED ) )
            sc_shared_vars = get_sc_shared_vars( );
        sc_shared_vars.insert( es_list.begin( ), es_list.end( ) );
        set_data( _SC_SHARED, sc_shared_vars );
    }

    Utils::ext_sym_set Node::get_sc_private_vars( )
    {
        Utils::ext_sym_set sc_private_vars;
        if(has_key(_SC_PRIVATE))
            sc_private_vars = get_data<Utils::ext_sym_set>( _SC_PRIVATE );
        return sc_private_vars;
    }

    void Node::set_sc_private_var( Utils::ExtendedSymbol es )
    {
        Utils::ext_sym_set sc_private_vars;
        if( has_key( _SC_PRIVATE ) )
            sc_private_vars = get_sc_private_vars( );
        sc_private_vars.insert( es );
        set_data( _SC_PRIVATE, sc_private_vars );
    }
    void Node::set_sc_private_var( Utils::ext_sym_set es_list )
    {
        Utils::ext_sym_set sc_private_vars;
        if( has_key( _SC_PRIVATE ) )
            sc_private_vars = get_sc_private_vars( );
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
        Utils::ext_sym_set sc_firstprivate_vars;
        if( has_key( _SC_FIRSTPRIVATE ) )
            sc_firstprivate_vars = get_sc_firstprivate_vars( );
        sc_firstprivate_vars.insert( es );
        set_data( _SC_FIRSTPRIVATE, sc_firstprivate_vars );
    }
    void Node::set_sc_firstprivate_var( Utils::ext_sym_set es_list )
    {
        Utils::ext_sym_set sc_firstprivate_vars;
        if( has_key( _SC_FIRSTPRIVATE ) )
            sc_firstprivate_vars = get_sc_firstprivate_vars( );
        sc_firstprivate_vars.insert( es_list.begin( ), es_list.end( ) );
        set_data( _SC_FIRSTPRIVATE, sc_firstprivate_vars );
    }

    Utils::ext_sym_set Node::get_sc_shared_or_firstprivate_vars( )
    {
        Utils::ext_sym_set sc_shared_or_firstprivate_vars;
        if( has_key( _SC_SHARED_OR_FIRSTPRIVATE ) )
            sc_shared_or_firstprivate_vars = get_data<Utils::ext_sym_set>( _SC_SHARED_OR_FIRSTPRIVATE );
        return sc_shared_or_firstprivate_vars;
    }

    void Node::set_sc_shared_or_firstprivate_var( Utils::ExtendedSymbol es )
    {
        Utils::ext_sym_set sc_shared_or_firstprivate_vars;
        if( has_key( _SC_SHARED_OR_FIRSTPRIVATE ) )
            sc_shared_or_firstprivate_vars = get_sc_shared_or_firstprivate_vars( );
        sc_shared_or_firstprivate_vars.insert( es );
        set_data( _SC_SHARED_OR_FIRSTPRIVATE, sc_shared_or_firstprivate_vars );
    }
    void Node::set_sc_shared_or_firstprivate_var( Utils::ext_sym_set es_list )
    {
        Utils::ext_sym_set sc_shared_or_firstprivate_vars;
        if( has_key( _SC_SHARED_OR_FIRSTPRIVATE ) )
            sc_shared_or_firstprivate_vars = get_sc_shared_or_firstprivate_vars( );
        sc_shared_or_firstprivate_vars.insert( es_list.begin( ), es_list.end( ) );
        set_data( _SC_SHARED_OR_FIRSTPRIVATE, sc_shared_or_firstprivate_vars );
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
        Utils::ext_sym_set sc_undef_vars;
        if( has_key( _SC_UNDEF ) )
            sc_undef_vars = get_sc_undef_vars( );
        sc_undef_vars.insert( es );
        set_data( _SC_UNDEF, sc_undef_vars );
    }
    void Node::set_sc_undef_var( Utils::ext_sym_set es_list )
    {
        Utils::ext_sym_set sc_undef_vars;
        if( has_key( _SC_UNDEF ) )
            sc_undef_vars = get_sc_undef_vars( );
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
        Utils::ext_sym_set sc_race_vars;
        if( has_key( _SC_RACE ) )
            sc_race_vars = get_data<Utils::ext_sym_set>( _SC_RACE );
        sc_race_vars.insert( es );
        set_data( _SC_RACE, sc_race_vars );
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
    // *********************************** Utils ************************************ //

    void Node::print_use_def_chains( )
    {
        if( VERBOSE )
        {
            Utils::ext_sym_set ue_vars = get_data<Utils::ext_sym_set>(_UPPER_EXPOSED);
            std::cerr << std::endl << "      - UE VARS: ";
            for(Utils::ext_sym_set::iterator it = ue_vars.begin( ); it != ue_vars.end( ); ++it)
            {
                std::cerr << it->get_nodecl( ).prettyprint( ) << ", ";
            }
            std::cerr << std::endl;

            Utils::ext_sym_set killed_vars = get_data<Utils::ext_sym_set>(_KILLED);
            std::cerr << "      - KILLED VARS: ";
            for(Utils::ext_sym_set::iterator it = killed_vars.begin( ); it != killed_vars.end( ); ++it)
            {
                std::cerr << it->get_nodecl( ).prettyprint( ) << ", ";
            }
            std::cerr << std::endl;

            Utils::ext_sym_set undef_vars = get_data<Utils::ext_sym_set>(_UNDEF);
            std::cerr << "      - UNDEF VARS: ";
            for(Utils::ext_sym_set::iterator it = undef_vars.begin( ); it != undef_vars.end( ); ++it)
            {
                std::cerr << it->get_nodecl( ).prettyprint( ) << ", ";
            }
            std::cerr << std::endl;
        }
    }

    void Node::print_liveness( )
    {
        if( VERBOSE )
        {
            Utils::ext_sym_set live_in_vars = get_data<Utils::ext_sym_set>(_LIVE_IN);
            std::cerr << std::endl << "      - LIVE IN VARS: ";
            for(Utils::ext_sym_set::iterator it = live_in_vars.begin( ); it != live_in_vars.end( ); ++it)
            {
                std::cerr << it->get_nodecl( ).prettyprint( ) << ", ";
            }
            std::cerr << std::endl;

            Utils::ext_sym_set live_out_vars = get_data<Utils::ext_sym_set>(_LIVE_OUT);
            std::cerr << "      - LIVE OUT VARS: ";
            for(Utils::ext_sym_set::iterator it = live_out_vars.begin( ); it != live_out_vars.end( ); ++it)
            {
                std::cerr << it->get_nodecl( ).prettyprint( ) << ", ";
            }
            std::cerr << std::endl;
        }
    }

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

    void Node::print_auto_scoping( )
    {
        if( VERBOSE )
        {
            std::string private_s      = "     - Private: "      + print_set( get_sc_private_vars( ) );
            std::string firstprivate_s = "     - Firstprivate: " + print_set( get_sc_firstprivate_vars( ) );
            std::string race_s         = "     - Race: "         + print_set( get_sc_race_vars( ) );
            std::string shared_s       = "     - Shared: "       + print_set( get_sc_shared_vars( ) );
            std::string undef_s        = "     - Undef: "        + print_set( get_sc_undef_vars( ) );

            std::cerr << private_s << std::endl << firstprivate_s << std::endl << race_s << std::endl
                      << shared_s << std::endl << undef_s << std::endl;
        }
    }

    void Node::print_task_dependencies( )
    {
        if( VERBOSE )
        {
            std::string private_s      = "     - Private: "      + print_set( get_deps_private_vars( ) );
            std::string firstprivate_s = "     - Firstprivate: " + print_set( get_deps_firstprivate_vars( ) );
            std::string shared_s       = "     - Shared: "       + print_set( get_deps_shared_vars( ) );
            std::string in_s           = "     - In deps: "      + print_set( get_deps_in_exprs( ) );
            std::string out_s          = "     - Out deps: "     + print_set( get_deps_out_exprs( ) );
            std::string inout_s        = "     - Inout deps: "   + print_set( get_deps_inout_exprs( ) );
            std::string undef_s        = "     - Undef deps: "   + print_set( get_deps_undef_vars( ) );

            std::cerr << private_s << std::endl << firstprivate_s << std::endl << shared_s << std::endl
                      << in_s << std::endl << out_s << std::endl << inout_s << std::endl << undef_s << std::endl;
        }
    }

    // ********************************* END utils ********************************** //
    // ****************************************************************************** //
}
}