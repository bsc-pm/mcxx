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


#include "cxx-process.h"
#include "tl-pcfg-visitor.hpp"

#include <cassert>

namespace TL {
namespace Analysis {

    // ************************************************************************************** //
    // ************************************ Constructors ************************************ //

    PCFGVisitor::PCFGVisitor( std::string name, Nodecl::NodeclBase nodecl )
    {
        _utils = new PCFGVisitUtils( );
        _pcfg = new ExtensibleGraph( name, nodecl, _utils );
    }

    // ********************************** END constructors ********************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ******************************** Non-visiting methods ******************************** //

    void PCFGVisitor::set_actual_pcfg( ExtensibleGraph* graph )
    {
        _pcfg = graph;
    }

    ExtensibleGraph* PCFGVisitor::parallel_control_flow_graph( const Nodecl::NodeclBase& n )
    {
        // Visit the nodes in \n
        walk( n );

        // Complete the exit node
        Node* pcfg_exit = _pcfg->_graph->get_graph_exit_node( );
        pcfg_exit->set_id( ++_utils->_nid );

        // Connect the exit nodes to the exit node of the current graph
        _pcfg->connect_nodes( _utils->_last_nodes, pcfg_exit );
        _pcfg->connect_nodes( _utils->_return_nodes, pcfg_exit );
        _utils->_return_nodes.clear( );

        // Tasks post-processing
        // When a task is not synchronized within the function it is created or
        // it is synchronized in conditional situations, then we create a virtual node
        // indicating that the synchronization will occur after the finalization of the function
        synchronize_tasks( );

        _pcfg->dress_up_graph( );

        return _pcfg;
    }

    // ****************************** END non-visiting methods ****************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ******************************** Non visiting methods ******************************** //

    void PCFGVisitor::compute_catch_parents( Node* node )
    {
        while( !node->is_visited( ) )
        {
            node->set_visited( true );
            Node_type n_type = node->get_type( );
            if( n_type == GRAPH )
            {
                compute_catch_parents( node->get_graph_entry_node( ) );
            }
            else if( n_type == EXIT )
            {
                return;
            }
            else if( n_type != ENTRY && n_type != UNCLASSIFIED_NODE
                     && n_type != BREAK )
            {
                _utils->_tryblock_nodes.back( )->_handler_parents.append( node );
            }

            ObjectList<Edge*> exit_edges = node->get_exit_edges( );
            for( ObjectList<Edge*>::iterator it = exit_edges.begin( );
                 it != exit_edges.end(); it++ )
            {
                compute_catch_parents( ( *it )->get_target( ) );
            }
        }
    }

    ObjectList<Node*> PCFGVisitor::get_first_nodes(Node* actual_node)
    {
        ObjectList<Edge*> actual_entries = actual_node->get_entry_edges();
        ObjectList<Node*> actual_parents;

        if( actual_entries.empty( ) )
        {
            if( actual_node->is_entry_node( ) )
            {   // 'actual_node' parent path is already connected with the graph Entry Node
                return ObjectList<Node*>( );
            }
            else
            {
                return ObjectList<Node*>( 1, actual_node );
            }
        }
        else
        {
            for( ObjectList<Edge*>::iterator it = actual_entries.begin( ); it != actual_entries.end( ); ++it )
            {
                ObjectList<Node*> parents = get_first_nodes( ( *it )->get_source( ) );
                actual_parents.insert( parents );
            }
        }

        return actual_parents;
    }

    /*! Elements in the list 'nodes_l' may have relations between them
     * For example, the statement 'f(b) + g();' will generate:
     * - two graph nodes which will depend one on the other
     * - the 'result' node containing the whole expression
     * This last node must have as parent only the graph node containing 'g'
     * So, before iterate the list to get the parents of the new merging node
     * we are going to purge the list deleting those nodes depending on other nodes in the same list
     */
    Node* PCFGVisitor::merge_nodes( Nodecl::NodeclBase n, ObjectList<Node*> nodes_l )
    {
        Node* result;

        // Compute the type of node for the new merged node
        Node_type ntype;
        if( n.is<Nodecl::FunctionCall>( ) || n.is<Nodecl::VirtualFunctionCall>( ) )
        {
            ntype = FUNCTION_CALL;
        }
        else if( n.is<Nodecl::LabeledStatement>( ) )
        {
            ntype = LABELED;
        }
        else
        {
            ntype = NORMAL;
        }

        if( nodes_l.size() > 1
            || ( ( nodes_l.size( ) == 1 ) && ( nodes_l[0]->get_type( ) == GRAPH ) ) )
        {   // There is some node to merge. Otherwise, we only have to create the new node

        // Check whether we need to build a graph node
        bool need_graph = false;
        for( ObjectList<Node*>::iterator it = nodes_l.begin( ); it != nodes_l.end( ); ++it )
        {
            if( ( *it )->get_type( ) == GRAPH )
            {
                need_graph = true;
                break;
            }
        }

        if( need_graph )
        {
            bool found;

            // Build the new graph
            result = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, SPLIT_STMT );
            Node* entry = result->get_graph_entry_node( );

            // Get parents of the new graph node and delete the old connections
            // Parents of the nodes in the list without parents within the list will be parents of the new graph
            // Nodes in the list without parents in the list are disconnected from its parents and connected to the Entry
            ObjectList<Node*> graph_parents;
            ObjectList<int> list_pos_to_erase;
            int i = 0;
            for( ObjectList<Node*>::iterator it = nodes_l.begin( ); it != nodes_l.end( ); ++it )
            {
                found = false;
                ObjectList<Node*> actual_parents = ( *it )->get_parents( );
                ObjectList<Node*>::iterator iit;
                for( iit = nodes_l.begin( ); iit != nodes_l.end( ); ++iit )
                {
                    if( actual_parents.contains( *iit ) )
                    {
                        found = true;
                        break;
                    }
                }
                if( !found )
                {
                    // add node to the list of graph parent
                    graph_parents.append( ( *it )->get_parents( ) );

                    // disconnect those nodes of its parents
                    ObjectList<Node*> aux = ( *it )->get_parents( );
                    for(ObjectList<Node*>::iterator iit2 = aux.begin( ); iit2 != aux.end( ); ++iit2 )
                    {
                        ( *iit2 )->erase_exit_edge( *it );
                        ( *it )->erase_entry_edge( *iit2 );
                    }
                    // delete the node if it is not of Graph type, otherwise, connect it to the Entry
                    if( ( *it )->get_type( ) != GRAPH )
                    {
                        list_pos_to_erase.append( i );
                        delete ( *it );
                    }
                    else
                    {
                        _pcfg->connect_nodes( entry, *it );
                    }
                }
                i++;
            }
            if( !graph_parents.empty( ) )
            {
                int n_connects = graph_parents.size( );
                _pcfg->connect_nodes( graph_parents, result, ObjectList<Edge_type>( n_connects, ALWAYS ),
                                      ObjectList<std::string>( n_connects, "" ) );
            }

            // Erase those positions in the list that are non-Graph nodes
            for( ObjectList<int>::reverse_iterator it = list_pos_to_erase.rbegin( );
                 it != list_pos_to_erase.rend( ); ++it )
            {
                nodes_l.erase( nodes_l.begin( ) + ( *it ) );
            }

                 // New merging node is created and connected with the nodes in the list without children within the list
            Node* merged_node = new Node( _utils->_nid, ntype, result, n );
            ObjectList<Node*> merged_parents;
            for( ObjectList<Node*>::iterator it = nodes_l.begin( ); it != nodes_l.end( ); ++it )
            {
                found = false;
                ObjectList<Node*> actual_children = ( *it )->get_children( );
                for( ObjectList<Node*>::iterator iit = nodes_l.begin( ); iit != nodes_l.end( ); ++iit )
                {
                    if( actual_children.contains( *iit ) )
                    {
                        found = true;
                        break;
                    }

                }
                if( !found )
                {
                    merged_parents.append( *it );
                }

                // now, all nodes must have the new Graph node as outer node
                ( *it )->set_outer_node( result );
            }
            _pcfg->connect_nodes( merged_parents, merged_node );

            // Connect merging node with the exit of the graph
            Node* graph_exit = result->get_graph_exit_node( );
            graph_exit->set_id( ++_utils->_nid );
            _pcfg->connect_nodes( merged_node, graph_exit );
            _utils->_outer_nodes.pop( );

            _utils->_last_nodes = ObjectList<Node*>( 1, result );
        }
        else
        {
            // Delete the nodes and its connections
            for( ObjectList<Node*>::iterator it = nodes_l.begin( ); it != nodes_l.end( ); ++it )
            {
                ObjectList<Node*> aux = (*it)->get_parents();
                if( !aux.empty( ) )
                {
                    internal_error( "Deleting a non-graph node that have '%d' parents. Those nodes shouldn't have any parent'",
                                    aux.size( ) );
                }

                delete ( *it );
            }

            // Built the new node
            result = new Node( _utils->_nid, ntype, _utils->_outer_nodes.top( ), n );
        }
        }
        else
        {
            result = new Node(_utils->_nid, ntype, _utils->_outer_nodes.top(), n);
        }

        return result;
    }

    Node* PCFGVisitor::merge_nodes( Nodecl::NodeclBase n, Node* first, Node* second )
    {
        ObjectList<Node*> previous_nodes;

        previous_nodes.append( first );
        if( second != NULL )
        {   // Only second node must be NULL and it will be the case of unary operations
            previous_nodes.append( second );
        }

        return merge_nodes( n, previous_nodes );
    }

    void PCFGVisitor::synchronize_tasks( )
    {
        // Node* virtual_sync = NULL;
        // for( ObjectList<Node*>::iterator it_t = _pcfg->_task_nodes_l.begin( );
        //      it_t != _pcfg->_task_nodes_l.end( ); ++it_t )
        // {
        //     ObjectList<Node*> children = ( *it_t )->get_children( );
        //     if( children.empty( ) )
        //     {
        //         if( virtual_sync == NULL )
        //         {
        //             virtual_sync = new Node( _utils->_nid, OMP_VIRTUAL_TASKSYNC, _pcfg->get_graph( ) );
        //         }
        //         _pcfg->connect_nodes( *it_t, virtual_sync, ALWAYS, "", true );
        //     }
        //     else
        //     {
        //         ObjectList<Node*>::iterator it_c;
        //         for( it_c = children.begin( ); it_c != children.end( ); it_c++ )
        //         {
        //             if( !ExtensibleGraph::node_is_in_conditional_branch( *it_c, ( *it_t )->get_outer_node( ) ) )
        //             {
        //                 break;
        //             }
        //         }
        //         if( it_c == children.end( ) )
        //         {   // All children are in conditional branches
        //             if( virtual_sync == NULL )
        //             {
        //                 virtual_sync = new Node( _utils->_nid, OMP_VIRTUAL_TASKSYNC, _pcfg->get_graph( ) );
        //             }
        //             _pcfg->connect_nodes( *it_t, virtual_sync, ALWAYS, "", true );
        //         }
        //     }
        // }

        _utils->_tasks_to_sync.clear( );
    }

    bool PCFGVisitor::task_is_surely_synchronized( Node* task )
    {
        bool res = false;

        ObjectList<Node*> children = task->get_children( );
        if( !children.empty( ) )
        {   // Conservative decision: one of the children is not conditional
            for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                if( !ExtensibleGraph::node_is_in_conditional_branch( *it, task->get_outer_node( ) ) )
                {
                    res = true;
                    break;
                }
            }
        }

        return res;
    }

    bool PCFGVisitor::same_parent_task( Node* task_1, Node* task_2 )
    {
        bool res = false;

        Node* task_1_parent_task = task_1->get_outer_node( );
        while( ( task_1_parent_task != NULL ) && !task_1_parent_task->is_omp_task_node( ) )
        {
            task_1_parent_task = task_1_parent_task->get_outer_node( );
        }
        Node* task_2_parent_task = task_2->get_outer_node( );
        while( ( task_2_parent_task != NULL ) && !task_2_parent_task->is_omp_task_node( ) )
        {
            task_2_parent_task = task_2_parent_task->get_outer_node( );
        }

        if( ( ( task_1_parent_task == NULL) && ( task_2_parent_task == NULL ) )
            || ( ( task_1_parent_task != NULL) && ( task_2_parent_task != NULL )
                 && ( task_1_parent_task->get_id( ) == task_2_parent_task->get_id( ) ) ) )
        {
            res = true;
        }

        return res;
    }

    // ************************************************************************************** //
    // ******************************** Non visiting methods ******************************** //



    // ************************************************************************************** //
    // ********************************** Visiting methods ********************************** //

    ObjectList<Node*> PCFGVisitor::visit_barrier( )
    {
        Node* first_flush = _pcfg->create_barrier_node( _utils->_outer_nodes.top( ) );

        // // Created with previous tasks in the same level of nesting
        // unsigned level = _utils->_task_level;
        // while( level < _utils->_tasks_to_sync.size( ) )
        // {
        //     for( ObjectList<ObjectList<Node*> >::iterator it = _utils->_tasks_to_sync[level].begin( );
        //          it != _utils->_tasks_to_sync[level].end( ); ++it )
        //     {
        //         for( ObjectList<Node*>::iterator it_n = it->begin( ); it_n != it->end( ); ++it_n )
        //         {
        //             if( !task_is_surely_synchronized( *it_n ) )
        //             {
        //                 _pcfg->connect_nodes( *it_n, first_flush, ALWAYS, "", /* is task edge */ true );
        //             }
        //         }
        //     }
        //     level++;
        // }

        return ObjectList<Node*>( 1, first_flush );
    }

    ObjectList<Node*> PCFGVisitor::visit_binary_node( const Nodecl::NodeclBase& n,
                                                     const Nodecl::NodeclBase& lhs,
                                                     const Nodecl::NodeclBase& rhs )
    {
        Node* left = walk( lhs )[0];
        Node* right = walk( rhs )[0];
        return ObjectList<Node*>( 1, merge_nodes( n, left, right ) );
    }

    ObjectList<Node*> PCFGVisitor::visit_case_or_default( const Nodecl::NodeclBase& case_stmt,
                                                         const Nodecl::NodeclBase& case_val )
    {
        // Build case nodes
        ObjectList<Node*> case_stmts = walk( case_stmt );

        // Set the edge between the Case and the Switch condition
        if( !case_stmts.empty( ) )
        {
            Edge* e;
            if( case_stmts[0]->is_break_node( ) )
            {
                e = _pcfg->connect_nodes( _utils->_switch_nodes.top( )->_condition, _utils->_switch_nodes.top( )->_exit, CASE );
            }
            else
            {
                e = _pcfg->connect_nodes( _utils->_switch_nodes.top( )->_condition, case_stmts[0], CASE );
            }

            std::string label;
            if( !case_val.is_null( ) )
            {
                label = case_val.prettyprint( );
            }
            else
            {
                label = "dafault";
            }
            e->set_label( label );

            if( case_stmts.back( )->get_type( ) != BREAK )
            {
                _utils->_last_nodes = ObjectList<Node*>( 1, case_stmts.back( ) );
            }
        }
        else
        {}   // The case is empty. Nothing to do

        return case_stmts;
    }

    template <typename T>
    ObjectList<Node*> PCFGVisitor::visit_function_call( const T& n )
    {
        // Add the current Function Call to the list of called functions
        _pcfg->add_func_call_symbol( n.get_called( ).get_symbol( ) );

        // Create the new Function Call node and built it
        Node* func_graph_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, FUNC_CALL );
        if( !_utils->_last_nodes.empty( ) )
        {   // If there is any node in 'last_nodes' list, then we have to connect the new graph node
            _pcfg->connect_nodes( _utils->_last_nodes, func_graph_node );
        }
        _utils->_last_nodes = ObjectList<Node*>( 1, func_graph_node->get_graph_entry_node( ) );

        // Create the nodes for the arguments
        Node* func_node;
        Nodecl::List args = n.get_arguments( ).template as<Nodecl::List>( );
        ObjectList<Node*> arguments_l = walk( args );
        if( !arguments_l.empty( ) )
        {   // Method merge_nodes connects properly the nodes created
            func_node = merge_nodes( n, arguments_l );
        }
        else
        {
            func_node = new Node( _utils->_nid, FUNCTION_CALL, func_graph_node, n );
        }
        _pcfg->connect_nodes( _utils->_last_nodes, func_node );

        Node* graph_exit = func_graph_node->get_graph_exit_node( );
        graph_exit->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( func_node, graph_exit );

        _utils->_outer_nodes.pop( );
        _utils->_last_nodes = ObjectList<Node*>( 1, func_graph_node );

        return ObjectList<Node*>( 1, func_graph_node );
    }

    ObjectList<Node*> PCFGVisitor::visit_literal_node( const Nodecl::NodeclBase& n )
    {
        Node* basic_node = new Node( _utils->_nid, NORMAL, _utils->_outer_nodes.top( ), n );
        return ObjectList<Node*>( 1, basic_node );
    }

    ObjectList<Node*> PCFGVisitor::visit_taskwait( )
    {
        Node* taskwait_node = new Node( _utils->_nid, OMP_TASKWAIT, _utils->_outer_nodes.top( ) );
        // Connect with the last nodes created
        _pcfg->connect_nodes( _utils->_last_nodes, taskwait_node );

        // We don't do this here anymore
#if 0
        // Created with previous tasks in the same level of nesting
        if( _utils->_tasks_to_sync.size( ) > ( _utils->_task_level ) )
        {   // There is some task to synchronize
            ObjectList<ObjectList<Node*> > * tasks_in_current_level = &( _utils->_tasks_to_sync[_utils->_task_level] );
            ObjectList<Node*> * tasks_in_last_nesting_branch = &( ( *tasks_in_current_level )[tasks_in_current_level->size() - 1] );
            if( same_parent_task( (*tasks_in_last_nesting_branch)[ /*any task in this list is valid for checking*/ 0],
                                    taskwait_node ) )
            {
                int n_connects = tasks_in_last_nesting_branch->size( );
                _pcfg->connect_nodes( *tasks_in_last_nesting_branch, taskwait_node,
                                        ObjectList<Edge_type>( n_connects, ALWAYS ),
                                        ObjectList<std::string>( n_connects, "" ), /* is task edge */ true );
            }
        }
#endif

        _utils->_last_nodes = ObjectList<Node*>( 1, taskwait_node );
        return ObjectList<Node*>();
    }

    ObjectList<Node*> PCFGVisitor::visit_unary_node( const Nodecl::NodeclBase& n,
                                                    const Nodecl::NodeclBase& rhs )
    {
        Node* right = walk( rhs )[0];
        return ObjectList<Node*>( 1, merge_nodes( n, right, NULL ) );
    }

    ObjectList<Node*> PCFGVisitor::unhandled_node( const Nodecl::NodeclBase& n )
    {
        std::cerr << "Unhandled node while PCFG construction '"
                    << codegen_to_str( n.get_internal_nodecl( ),
                                        nodecl_retrieve_context( n.get_internal_nodecl( ) ) )
                    << "' of type '" << ast_print_node_type( n.get_kind( ) ) << "'" << std::endl;
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Add& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::AddAssignment& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Alignof& n )
    {
        return visit_unary_node( n, n.get_align_type( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ArithmeticShr& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ArithmeticShrAssignment& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ArraySubscript& n )
    {
        ObjectList<Node*> subscripted = walk( n.get_subscripted( ) );
        ObjectList<Node*> subscripts = walk( n.get_subscripts( ) );

        ObjectList<Node*> nodes = subscripted;
        nodes.insert( subscripts );
        return ObjectList<Node*>( 1, merge_nodes( n, nodes ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Assignment& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BitwiseAnd& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BitwiseAndAssignment& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BitwiseNot& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BitwiseOr& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BitwiseOrAssignment& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BitwiseShl& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BitwiseShlAssignment& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BitwiseShr& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BitwiseShrAssignment& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BitwiseXor& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BitwiseXorAssignment& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BooleanLiteral& n )
    {
        return visit_literal_node( n );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::BreakStatement& n )
    {
        // The only case when '_utils->_last_nodes' can be empy is when a Case Statement has no statements
        Node* break_node;
        if( _utils->_last_nodes.empty( ) )
            break_node = _pcfg->append_new_node_to_parent( _utils->_switch_nodes.top( )->_condition, n, BREAK );
        else
            break_node = _pcfg->append_new_node_to_parent( _utils->_last_nodes, n, BREAK );
        _pcfg->connect_nodes( break_node, _utils->_break_nodes.top( ) );
        _utils->_last_nodes.clear( );
        return ObjectList<Node*>( 1, break_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::CaseStatement& n )
    {
        return visit_case_or_default( n.get_statement( ), n.get_case( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Cast& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::CatchHandler& n )
    {
        PCFGTryBlock* current_tryblock = _utils->_tryblock_nodes.back( );
        current_tryblock->_nhandlers++;

        // Build the handler nodes
        _utils->_last_nodes = current_tryblock->_handler_parents;
        ObjectList<Node*> catchs = walk( n.get_statement( ) );

        current_tryblock->_handler_exits.append( catchs[0] );

        // Set the type of the edge between each handler parent and the actual handler
        std::string label;
        if( n.get_name( ).is_null( ) )
            label = "...";
        else
            label = n.get_name( ).prettyprint( );
        for( ObjectList<Node*>::iterator it = current_tryblock->_handler_parents.begin( );
              it != current_tryblock->_handler_parents.end( ); ++it )
        {
            Edge* catch_edge = ( *it )->get_exit_edge( catchs[0] );
            if( catch_edge != NULL )
            {
                catch_edge->set_catch_edge( );
                catch_edge->set_label( label );
            }
        }

        // FIXME If there is no Ellipsis, all statements within Try must be connected to the Exit of the graph
        // TODO We can reduce considerably the number of connections by analysing the kind of every exception

        return catchs;
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ClassMemberAccess& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_member( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Comma& n )
    {
        ObjectList<Node*> comma_nodes;
        comma_nodes.append( walk( n.get_rhs( ) ) );
        comma_nodes.append( walk( n.get_lhs( ) ) );
        return comma_nodes;
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ComplexLiteral& n )
    {
        return visit_literal_node( n );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::CompoundExpression& n )
    {
        return walk( n.get_nest( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::CompoundStatement& n )
    {
        return walk( n.get_statements( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Concat& n )
    {
        return visit_binary_node( n, n.get_lhs(), n.get_rhs() );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ConditionalExpression& n )
    {
        Node* cond_expr_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, COND_EXPR );
        Node* entry_node = cond_expr_node->get_graph_entry_node( );

        // Build condition node
        Node* condition_node = walk( n.get_condition( ) )[0];
        _pcfg->connect_nodes( entry_node, condition_node );
        ObjectList<Node*> exit_parents;

        // Build true node
        Node* true_node = walk( n.get_true( ) )[0];
        _pcfg->connect_nodes( condition_node, true_node );
        exit_parents.append( true_node );

        // Build false node
        Node* false_node = walk( n.get_false( ) )[0];
        _pcfg->connect_nodes( condition_node, false_node );
        exit_parents.append( false_node );

        // Set exit graph node info
        Node* exit_node = cond_expr_node->get_graph_exit_node( );
        exit_node->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( exit_parents, exit_node );
        _utils->_outer_nodes.pop( );

        return ObjectList<Node*>( 1, cond_expr_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Context& n )
    {
        _utils->_context_nodecl.push( n );
        ObjectList<Node*> in_context = walk( n.get_in_context( ) );
        _utils->_context_nodecl.pop( );
        return in_context;
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ContinueStatement& n )
    {
        Node* continue_node = _pcfg->append_new_node_to_parent( _utils->_last_nodes, n, CONTINUE );
        _pcfg->connect_nodes( continue_node, _utils->_continue_nodes.top( ) );
        _utils->_last_nodes.clear( );
        return ObjectList<Node*>( 1, continue_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::CxxDef& n )
    {   // Nothing to be done: this nodes are also represented with ObjectInits when necessary
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Conversion& n )
    {
        return walk( n.get_nest( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::DefaultStatement& n )
    {
        return visit_case_or_default( n.get_statement( ), Nodecl::NodeclBase::null( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Delete& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::DeleteArray& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Dereference& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Different& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Div& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::DivAssignment& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::DoStatement& n )
    {
        // FIXME This is not correct:
        //             do
        //             {
        //             #pragma omp task private(fX) firstprivate(i)
        //             {
        //                     fX = f(fH * ((double)i + 0.5));
        //                 #pragma omp critical
        //                     fSum += fX;
        //             }
        //             #pragma omp task
        //             {
        //                 int a = 0;
        //                 a++;
        //             }
        //             i += 1;
        //             }while(i < n);

        Node* do_graph_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, LOOP_DOWHILE );
        _pcfg->connect_nodes( _utils->_last_nodes, do_graph_node );
        Node* do_exit = do_graph_node->get_graph_exit_node( );

        // Create the condition node
        _utils->_last_nodes.clear();
        Node* condition_node = walk( n.get_condition( ) )[0];

        // Create the do statements nodes
        _utils->_last_nodes = ObjectList<Node*>( 1, do_graph_node->get_graph_entry_node( ) );
        _utils->_continue_nodes.push( condition_node );
        _utils->_break_nodes.push( do_exit );
        ObjectList<Node*> stmts = walk( n.get_statement( ) );
        _utils->_continue_nodes.pop( );
        _utils->_break_nodes.pop( );

        // Connect the statements with the condition
        _pcfg->connect_nodes( _utils->_last_nodes, condition_node );
        if( !stmts.empty( ) )
        {
            _pcfg->connect_nodes( condition_node, stmts[0], TRUE_EDGE );
        }

        // Connect the condition false side to the condition node
        do_exit->set_id( ++( _utils->_nid ) );
        do_exit->set_outer_node( _utils->_outer_nodes.top( ) );
        _pcfg->connect_nodes( condition_node, do_exit, FALSE_EDGE );
        _utils->_outer_nodes.pop( );

        _utils->_last_nodes = ObjectList<Node*>( 1, do_graph_node );
        return ObjectList<Node*>( 1, condition_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::EmptyStatement& n )
    {
        return visit_literal_node( n );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Equal& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ExpressionStatement& n )
    {
        ObjectList<Node*> expr_last_nodes = _utils->_last_nodes;
        ObjectList<Node*> expression_nodes = walk( n.get_nest( ) );

        if( expression_nodes.size( ) > 0 )
        {
            // When the size of expression_nodes list is >1, the expression contained contains a comma operator.
            // Otherwise, the expression is any other kind of expression
            Node* last_node;
            if( expression_nodes.size( ) == 1 )
            {
                last_node = expression_nodes[0];
            }
            else
            {   // expression_nodes.size() > 1
                last_node = merge_nodes( n, expression_nodes );
            }

            if( !last_node->is_empty_node( ) )
            {
                // Connect the partial node created recursively with the piece of Graph build until this moment
                ObjectList<Node*> expr_first_nodes = get_first_nodes( last_node );
                for( ObjectList<Node*>::iterator it = expr_first_nodes.begin( );
                     it != expr_first_nodes.end( ); ++it )
                {
                    _pcfg->clear_visits( *it );
                }

                if( !expr_last_nodes.empty( ) )
                {   // This will be empty when last statement visited was a Break Statement
                    int n_connects = expr_first_nodes.size( ) * expr_last_nodes.size( );
                    if( n_connects != 0 )
                    {
                        _pcfg->connect_nodes( expr_last_nodes, expr_first_nodes,
                                              ObjectList<Edge_type>( n_connects, ALWAYS ),
                                              ObjectList<std::string>( n_connects, "" ) );
                    }
                }

                // Recompute actual last nodes for the actual graph
                if( !_utils->_last_nodes.empty( ) )
                {
                    _utils->_last_nodes = ObjectList<Node*>( 1, last_node );
                }
            }
            else
            {   // do nothing; this case appears when the expression is "new"
                // In this case we don't need adding this statement to the graph because it is meaningless
            }
        }
        else
        {
            internal_error( "Parsing the expression '%s' 0 nodes has been returned, and they must be one or more\n",
                            codegen_to_str( n.get_internal_nodecl( ),
                                            nodecl_retrieve_context( n.get_internal_nodecl( ) ) ) );
        }

        return expression_nodes;
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::FloatingLiteral& n )
    {
        return visit_literal_node( n );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ForStatement& n )
    {
        // Compute the information about the loop control and keep the results in the struct '_current_loop_ctrl'
        ObjectList<Node*> actual_last_nodes = _utils->_last_nodes;
        walk( n.get_loop_header( ) );
        _utils->_last_nodes = actual_last_nodes;

        // Connect the init
        if( _utils->_nested_loop_nodes.top( )->_init != NULL )
        {
            int n_connects = _utils->_last_nodes.size( );
            _pcfg->connect_nodes( _utils->_last_nodes, _utils->_nested_loop_nodes.top( )->_init,
                                  ObjectList<Edge_type>( n_connects, ALWAYS ),
                                  ObjectList<std::string>( n_connects, "" ) );
            _utils->_last_nodes = ObjectList<Node*>( 1, _utils->_nested_loop_nodes.top( )->_init );
        }

        // Create the loop graph node
        Node* for_graph_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, LOOP_FOR );
        int n_connects = _utils->_last_nodes.size( );
        _pcfg->connect_nodes( _utils->_last_nodes, for_graph_node,
                              ObjectList<Edge_type>( n_connects, ALWAYS ),
                              ObjectList<std::string>( n_connects, "" ) );

        // Connect the conditional node
        Node* entry_node = for_graph_node->get_graph_entry_node( );
        _utils->_nested_loop_nodes.top( )->_cond->set_outer_node( for_graph_node );
        _pcfg->connect_nodes( entry_node, _utils->_nested_loop_nodes.top( )->_cond );
        _utils->_last_nodes = ObjectList<Node*>( 1, _utils->_nested_loop_nodes.top( )->_cond );

        Node* exit_node = for_graph_node->get_graph_exit_node( );

        // Create the nodes from the list of inner statements of the loop
        _utils->_continue_nodes.push( _utils->_nested_loop_nodes.top( )->_next );
        _utils->_break_nodes.push( exit_node );
        walk( n.get_statement( ) );    // This list of nodes returned here will never be used

        _utils->_continue_nodes.pop();
        _utils->_break_nodes.pop();

        // Compute the true edge from the loop condition
        Edge_type aux_etype = ALWAYS;
        ObjectList<Edge*> exit_edges = _utils->_nested_loop_nodes.top( )->_cond->get_exit_edges( );
        if( !exit_edges.empty( ) )
        {
            // The first edge and, and, if the first is a task, all the following edges being tasks and the first not being a task are TRUE_EDGE
            // If all exit exit edges are tasks, then the "Next node" of the loop is linked with a true edge as well
            bool all_tasks = true;
            ObjectList<Edge*>::iterator it = exit_edges.begin( );
            while( all_tasks && it != exit_edges.end( ) )
            {
                if( !( *it )->is_task_edge( ) )
                {
                    all_tasks = false;
                }
                ( *it )->set_true_edge( );
                ++it;
            }
            if( all_tasks )
                aux_etype = TRUE_EDGE;
        }
        else
        {   // It will be empty when the loop's body is empty.
            aux_etype = TRUE_EDGE;
        }

        exit_node->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( _utils->_nested_loop_nodes.top( )->_cond, exit_node, FALSE_EDGE );

        // Fill the empty fields of the Increment node
        _utils->_nested_loop_nodes.top( )->_next->set_outer_node( for_graph_node );
        _pcfg->connect_nodes( _utils->_last_nodes, _utils->_nested_loop_nodes.top( )->_next, aux_etype );
        _pcfg->connect_nodes( _utils->_nested_loop_nodes.top( )->_next, _utils->_nested_loop_nodes.top( )->_cond,
                              ALWAYS, "", /* is back edge */ true );

        for_graph_node->set_stride_node( _utils->_nested_loop_nodes.top( )->_next );
        _utils->_nested_loop_nodes.pop( );

        _utils->_outer_nodes.pop( );
        _utils->_last_nodes = ObjectList<Node*>( 1, for_graph_node );

        return ObjectList<Node*>( 1, for_graph_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::FunctionCall& n )
    {
        return visit_function_call( n );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::FunctionCode& n )
    {
        _pcfg->_function_sym = n.get_symbol( );

        return walk( n.get_statements( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::GccAsmDefinition& n )
    {
        // Create the asm definition graph node
        Node* asm_def_graph_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, ASM_DEF );
        _pcfg->connect_nodes( _utils->_last_nodes, asm_def_graph_node );

        Node* entry_node = asm_def_graph_node->get_graph_entry_node( );
        Node* exit_node = asm_def_graph_node->get_graph_exit_node( );

        // Build the node containing the asm function text
        Nodecl::Text text = Nodecl::Text::make( n.get_text( ) );
        Node* text_node = new Node( _utils->_nid, NORMAL, _utils->_outer_nodes.top( ), text );
        text_node->set_asm_info( ASM_DEF_TEXT );
        _pcfg->connect_nodes( entry_node, text_node );
        _utils->_last_nodes = ObjectList<Node*>( 1, text_node );

        // Build the node containing the output operands
        ObjectList<Node*> op0 = walk( n.get_operands0( ) );
        if( !op0.empty( ) )
        {
            op0[0]->set_asm_info( ASM_DEF_OUTPUT_OPS );
        }

        // Build the node containing the input operands
        ObjectList<Node*> op1 = walk( n.get_operands1( ) );
        op1[0]->set_asm_info( ASM_DEF_INPUT_OPS );

        // Build the node containing the clobbered registers
        ObjectList<Node*> op2 = walk( n.get_operands2( ) );
        if( !op2.empty( ) )
        {
            op2[0]->set_asm_info( ASM_DEF_CLOBBERED_REGS );
        }

//         walk( n.get_specs( ) );      // Specs are not used for any analysis

        // Link properly the exit node
        exit_node->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( _utils->_last_nodes, exit_node );
        _utils->_outer_nodes.pop( );

        _utils->_last_nodes = ObjectList<Node*>( 1, asm_def_graph_node );
        return ObjectList<Node*>( 1, asm_def_graph_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::GccAsmOperand& n )
    {
        Node* asm_op_node = new Node( _utils->_nid, ASM_OP, _utils->_outer_nodes.top( ), n );
        _pcfg->connect_nodes( _utils->_last_nodes, asm_op_node );

        _utils->_last_nodes = ObjectList<Node*>( 1, asm_op_node );

        return ObjectList<Node*>( 1, asm_op_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::GotoStatement& n )
    {
        Node* goto_node = _pcfg->append_new_node_to_parent( _utils->_last_nodes, n, GOTO );
        goto_node->set_label( n.get_symbol( ) );
        _pcfg->connect_nodes( _utils->_last_nodes, goto_node );

        ObjectList<Node*>::iterator it;
        for( it = _utils->_labeled_nodes.begin( ); it != _utils->_labeled_nodes.end( ); ++it )
        {
            if( ( *it )->get_label( ) == n.get_symbol( ) )
            {   // Connect the nodes
                _pcfg->connect_nodes( goto_node, *it, GOTO_EDGE, n.get_symbol( ).get_name( ) );
                break;
            }
        }
        if( it == _utils->_labeled_nodes.end( ) )
        {
            _utils->_goto_nodes.append( goto_node );
        }

        _utils->_last_nodes.clear( );
        return ObjectList<Node*>( 1, goto_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::GreaterOrEqualThan& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::GreaterThan& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::IfElseStatement& n )
    {
        Node* if_else_graph_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, IF_ELSE );
        _pcfg->connect_nodes( _utils->_last_nodes, if_else_graph_node );
        Node* if_else_exit = if_else_graph_node->get_graph_exit_node( );

        // Compose the condition node
        _utils->_last_nodes.clear();
        Node* cond_node = walk( n.get_condition( ) )[0];
        _pcfg->connect_nodes( if_else_graph_node->get_graph_entry_node( ), cond_node );
        _utils->_last_nodes = ObjectList<Node*>( 1, cond_node );

        // Compose the then node
        ObjectList<Node*> then_node_l = walk( n.get_then( ) );
        if( !cond_node->get_exit_edges( ).empty( ) )
        {
            ObjectList<Edge*> exit_edges = cond_node->get_exit_edges( );
            bool all_tasks_then = true;
            for( ObjectList<Edge*>::iterator it = exit_edges.begin( ); it != exit_edges.end( ); ++it )
            {   // More than one exit edge means that some tasks are created within 'then' statement
                ( *it )->set_true_edge( );
                if( !( *it )->is_task_edge( ) )
                    all_tasks_then = false;
            }
            _pcfg->connect_nodes( _utils->_last_nodes, if_else_exit );

            // Compose the else node, if it exists
            _utils->_last_nodes = ObjectList<Node*>( 1, cond_node );
            ObjectList<Node*> else_node_l = walk( n.get_else( ) );

            // Link the If condition with the FALSE statement (else or empty node)
            bool all_tasks_else = true;
            unsigned int false_edge_it = exit_edges.size( );
            exit_edges = cond_node->get_exit_edges( );
            for( ; false_edge_it < cond_node->get_exit_edges( ).size( ); ++false_edge_it )
            {
                exit_edges[false_edge_it]->set_false_edge( );
                if( !exit_edges[false_edge_it]->is_task_edge( ) )
                    all_tasks_else = false;
            }
            if( !else_node_l.empty( ) )
                _pcfg->connect_nodes( _utils->_last_nodes, if_else_exit );
            else
                _pcfg->connect_nodes( _utils->_last_nodes, if_else_exit, FALSE_EDGE );

            // Connect the Exit node in that cases where it has not been connected before
            if( ( all_tasks_then && all_tasks_else ) || cond_node->get_exit_edges().empty() )
            {
                _pcfg->connect_nodes( cond_node, if_else_exit );
            }
            else if( all_tasks_then )
            {
                _pcfg->connect_nodes( cond_node, if_else_exit, TRUE_EDGE );
            }
            else if( all_tasks_else )
            {
                _pcfg->connect_nodes( cond_node, if_else_exit, FALSE_EDGE );
            }
        }
        else
        {
            // Both for true and false evaluation for the if condition, we go to the exit node
            _pcfg->connect_nodes( cond_node, if_else_exit );
        }

        if_else_exit->set_id( ++( _utils->_nid ) );
        if_else_exit->set_outer_node( _utils->_outer_nodes.top() );
        _utils->_outer_nodes.pop( );

        _utils->_last_nodes = ObjectList<Node*>( 1, if_else_graph_node );
        return ObjectList<Node*>( 1, if_else_graph_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::IntegerLiteral& n )
    {
        return visit_literal_node( n );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::LabeledStatement& n )
    {
        Node* labeled_node = walk( n.get_statement( ) )[0];
        labeled_node->set_type( LABELED );
        labeled_node->set_label( n.get_symbol( ) );

        for( ObjectList<Node*>::iterator it = _utils->_goto_nodes.begin( );
              it != _utils->_goto_nodes.end( ); ++it )
        {
            if( ( *it )->get_label( ) == n.get_symbol( ) )
            {   // Connect the nodes
                _pcfg->connect_nodes( *it, labeled_node, GOTO_EDGE, n.get_symbol( ).get_name( ) );
                break;
            }
        }

        _utils->_labeled_nodes.append( labeled_node );

        // The labeled statement is the "_last_node" in case it has generated only one node (or more than one, but they are wrapped in a GRAPH)
        // For instance:
        //      l1: if (j==30)               => The parent of "l2" statement is not "l1", but the EXIT node of the IfElse statement
        //              ...
        //          else
        //              ...
        //      l2: ...
        if( labeled_node->get_exit_edges( ).empty( ) )
            _utils->_last_nodes = ObjectList<Node*>( 1, labeled_node );

        return ObjectList<Node*>( 1, labeled_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::LogicalAnd& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::LogicalNot& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::LogicalOr& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::LoopControl& n )
    {
        PCFGLoopControl* current_loop_ctrl = new PCFGLoopControl( );

        // Create initializing node
        _utils->_last_nodes.clear( );
        ObjectList<Node*> init_node_l = walk( n.get_init( ) );
        if( init_node_l.empty( ) )
        {   // The empty statement will return anything here. No node needs to be created
            current_loop_ctrl->_init = NULL;
        }
        else
        {
            current_loop_ctrl->_init = init_node_l[0];
        }

        // Create condition node
        _utils->_last_nodes.clear( );
        ObjectList<Node*> cond_node_l = walk( n.get_cond( ) );
        if( cond_node_l.empty( ) )
        {   // The condition is an empty statement.
            // In any case, we build here a node for easiness
            current_loop_ctrl->_cond = new Node( _utils->_nid, NORMAL,
                                              _utils->_outer_nodes.top( ), Nodecl::NodeclBase::null( ) );
        }
        else
        {
            current_loop_ctrl->_cond = cond_node_l[0];
        }

        // Create next node
        _utils->_last_nodes.clear( );
        ObjectList<Node*> next_node_l = walk( n.get_next( ) );
        if( next_node_l.empty( ) )
        {
            current_loop_ctrl->_next = new Node( _utils->_nid, NORMAL,
                                              _utils->_outer_nodes.top( ), Nodecl::NodeclBase::null( ) );
        }
        else
        {
            current_loop_ctrl->_next = next_node_l[0];
        }

        _utils->_nested_loop_nodes.push( current_loop_ctrl );

        return ObjectList<Node*>( );   // No return required here. '_current_loop_ctrl' contains all information.
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::LowerOrEqualThan& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::LowerThan& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Minus& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::MinusAssignment& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Mod& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ModAssignment& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Mul& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::MulAssignment& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Neg& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::New& n )
    {
        return visit_literal_node( n );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ObjectInit& n )
    {
        if( _pcfg == NULL )
        {   // do nothing: A shared variable is declared
            return ObjectList<Node*>( );
        }
        else
        {
            ObjectList<Node*> object_init_last_nodes = _utils->_last_nodes;
            Nodecl::Symbol n_sym = Nodecl::Symbol::make( n.get_symbol( ), n.get_locus() );
            ObjectList<Node*> init_sym = walk( n_sym );
            ObjectList<Node*> init_expr = walk( n.get_symbol( ).get_value( ) );

            if( init_expr.empty( ) )
            {   // do nothing: The Object Init is not initialized
                return ObjectList<Node*>( );
            }

            Node* merged_node = merge_nodes( n, init_sym[0], init_expr[0] );
            _pcfg->connect_nodes( object_init_last_nodes, merged_node );
            _utils->_last_nodes = ObjectList<Node*>( 1, merged_node );
            return ObjectList<Node*>( 1, merged_node );
        }
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Offset& n )
    {
        return visit_binary_node( n, n.get_base( ), n.get_offset( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Offsetof& n )
    {
        return visit_binary_node( n, n.get_offset_type( ), n.get_designator( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Atomic& n )
    {
        // Create the new graph node containing the atomic
        Node* atomic_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, OMP_ATOMIC );
        _pcfg->connect_nodes( _utils->_last_nodes, atomic_node );

        Node* atomic_entry = atomic_node->get_graph_entry_node( );
        Node* atomic_exit = atomic_node->get_graph_exit_node( );

        // Traverse the statements of the current atomic
        _utils->_last_nodes = ObjectList<Node*>( 1, atomic_entry );
        walk( n.get_statements( ) );

        atomic_exit->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( _utils->_last_nodes, atomic_exit );

        // Set possible implicit flushes
        _utils->_environ_entry_exit.push( std::pair<Node*, Node*>( atomic_entry, atomic_exit ) );
        walk( n.get_environment( ) );
        _utils->_environ_entry_exit.pop( );

        _utils->_outer_nodes.pop( );
        _utils->_last_nodes = ObjectList<Node*>( 1, atomic_node );
        return ObjectList<Node*>( 1, atomic_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Auto& n )
    {
        PCFGClause current_clause( AUTO, n.get_auto_symbols( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );

        // Set the task related to this clause to have auto-scoping enabled
        _utils->_environ_entry_exit.top( ).first->get_outer_node( )->set_auto_scoping_enabled( );

        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::BarrierAtEnd& n )
    {
        Node* environ_exit = _utils->_environ_entry_exit.top( ).second;
        ObjectList<Node*> exit_parents = environ_exit->get_parents( );

        // Save current info
        _pcfg->disconnect_nodes( exit_parents, environ_exit );
        ObjectList<Node*> actual_last_nodes = _utils->_last_nodes;
        _utils->_last_nodes = exit_parents;

        // Create the barrier node
        visit_barrier( );
        _pcfg->connect_nodes( _utils->_last_nodes[0], environ_exit );

        // Restore current info
        _utils->_last_nodes = actual_last_nodes;

        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::BarrierFull& n )
    {
        return visit_barrier( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::BarrierSignal& n )
    {
        internal_error( "BarrierSignal not yet implemented", 0 );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::BarrierWait& n )
    {
        internal_error( "BarrierWait not yet implemented", 0 );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::CombinedWorksharing& n )
    {   // No deeper Nodecls
        internal_error( "CombinedWorksharing not yet implemented ( %s )", codegen_to_str( n.get_internal_nodecl( ),
                                                                                          nodecl_retrieve_context( n.get_internal_nodecl( ) ) ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::CopyIn& n )
    {
        PCFGClause current_clause( COPY_IN, n.get_input_copies( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::CopyInout& n )
    {
        PCFGClause current_clause( COPY_INOUT, n.get_inout_copies( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::CopyOut& n )
    {
        PCFGClause current_clause( COPY_OUT, n.get_output_copies( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Critical& n )
    {
        // Create the new graph node containing the critical
        Node* critical_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, OMP_CRITICAL );
        _pcfg->connect_nodes( _utils->_last_nodes, critical_node );

        Node* critical_entry = critical_node->get_graph_entry_node( );
        Node* critical_exit = critical_node->get_graph_exit_node( );

        // Traverse the statements of the current critical
        _utils->_last_nodes = ObjectList<Node*>( 1, critical_entry );
        walk( n.get_statements( ) );

        critical_exit->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( _utils->_last_nodes, critical_exit );

        // Set clauses and possible implicit flushes
        PCFGPragmaInfo current_pragma;
        _utils->_pragma_nodes.push( current_pragma );
        _utils->_environ_entry_exit.push( std::pair<Node*, Node*>( critical_entry, critical_exit ) );
        walk( n.get_environment( ) );
        critical_node->set_omp_node_info( _utils->_pragma_nodes.top( ) );
        _utils->_pragma_nodes.pop( );
        _utils->_environ_entry_exit.pop( );

        _utils->_outer_nodes.pop( );
        _utils->_last_nodes = ObjectList<Node*>( 1, critical_node );
        
        return ObjectList<Node*>( 1, critical_node );

    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::CriticalName& n )
    {
        PCFGClause current_clause( NAME, n );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::DepIn& n )
    {
        PCFGClause current_clause( DEP_IN, n.get_in_deps( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::DepInout& n )
    {
        PCFGClause current_clause( DEP_INOUT, n.get_inout_deps( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::DepOut& n )
    {
        PCFGClause current_clause( DEP_OUT, n.get_out_deps( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Firstprivate& n )
    {
        PCFGClause current_clause( FIRSTPRIVATE, n.get_firstprivate_symbols( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::FlushAtEntry& n )
    {
        Node* environ_entry = _utils->_environ_entry_exit.top( ).first;
        ObjectList<Node*> entry_children = environ_entry->get_children( );

        // Save current info
        _pcfg->disconnect_nodes( environ_entry, entry_children );
        ObjectList<Node*> actual_last_nodes = _utils->_last_nodes;
        _utils->_last_nodes = ObjectList<Node*>( 1, environ_entry );

        // Create the flush node
        Node* entry_flush = _pcfg->create_flush_node( _utils->_outer_nodes.top( ) );
        int n_connects = entry_children.size( );
        _pcfg->connect_nodes( entry_flush, entry_children,
                              ObjectList<Edge_type>( n_connects, ALWAYS ),
                              ObjectList<std::string>( n_connects, "" ) );

        // Restore current info
        environ_entry = entry_flush;
        _utils->_last_nodes = actual_last_nodes;

        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::FlushAtExit& n )
    {
        Node* environ_exit = _utils->_environ_entry_exit.top( ).second;
        ObjectList<Node*> exit_parents = environ_exit->get_parents( );

        // Save current info
        _pcfg->disconnect_nodes( exit_parents, environ_exit );
        ObjectList<Node*> actual_last_nodes = _utils->_last_nodes;
        _utils->_last_nodes = exit_parents;

        // Create the flush node
        Node* exit_flush = _pcfg->create_flush_node( _utils->_outer_nodes.top( ) );
        _pcfg->connect_nodes( exit_flush, environ_exit );

        // Restore current info
        _utils->_last_nodes = actual_last_nodes;

        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::FlushMemory& n )
    {
        return ObjectList<Node*>( 1, _pcfg->create_flush_node( _utils->_outer_nodes.top( ), n.get_expressions( ) ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::For& n )
    {
        // Create the new graph node containing the for
        Node* for_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, OMP_LOOP );
        _pcfg->connect_nodes( _utils->_last_nodes, for_node );

        Node* for_entry = for_node->get_graph_entry_node( );
        Node* for_exit = for_node->get_graph_exit_node( );

        // Traverse the statements of the current sections
        _utils->_last_nodes = ObjectList<Node*>( 1, for_entry );
        walk( n.get_loop( ) );

        for_exit->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( _utils->_last_nodes, for_exit );

        // Set clauses info to the for node
        PCFGPragmaInfo current_pragma;
        _utils->_pragma_nodes.push( current_pragma );
        _utils->_environ_entry_exit.push( std::pair<Node*, Node*>( for_entry, for_exit ) );
        walk( n.get_environment( ) );
        for_node->set_omp_node_info( _utils->_pragma_nodes.top( ) );
        _utils->_pragma_nodes.pop( );
        _utils->_environ_entry_exit.pop( );

        _utils->_outer_nodes.pop( );
        _utils->_last_nodes = ObjectList<Node*>( 1, for_node );
        return ObjectList<Node*>( 1, for_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::If& n )
    {
        PCFGClause current_clause( IF, n.get_condition( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Master& n )
    {
        // Create the new graph node containing the master
        Node* master_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, OMP_MASTER );
        _pcfg->connect_nodes( _utils->_last_nodes, master_node );

        // Traverse the statements of the current master
        _utils->_last_nodes = ObjectList<Node*>( 1, master_node->get_graph_entry_node( ) );
        walk( n.get_statements( ) );

        Node* master_exit = master_node->get_graph_exit_node( );
        master_exit->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( _utils->_last_nodes, master_exit );
        
        _utils->_outer_nodes.pop( );
        _utils->_last_nodes = ObjectList<Node*>( 1, master_node );
        return ObjectList<Node*>( 1, master_node );

    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Parallel& n )
    {
        // Create the new graph node containing the parallel
        Node* parallel_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, OMP_PARALLEL );
        _pcfg->connect_nodes( _utils->_last_nodes, parallel_node );

        Node* parallel_entry = parallel_node->get_graph_entry_node( );
        Node* parallel_exit = parallel_node->get_graph_exit_node( );

        // Traverse the statements of the current sections
        _utils->_last_nodes = ObjectList<Node*>( 1, parallel_entry );
        walk( n.get_statements( ) );

        parallel_exit->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( _utils->_last_nodes, parallel_exit );

        // Set clauses info to the for node
        PCFGPragmaInfo current_pragma;
        _utils->_pragma_nodes.push( current_pragma );
        _utils->_environ_entry_exit.push( std::pair<Node*, Node*>( parallel_entry, parallel_exit ) );
        walk( n.get_environment( ) );
        parallel_node->set_omp_node_info( _utils->_pragma_nodes.top( ) );
        _utils->_pragma_nodes.pop( );
        _utils->_environ_entry_exit.pop( );

        _utils->_outer_nodes.pop( );
        _utils->_last_nodes = ObjectList<Node*>( 1, parallel_node );
        return ObjectList<Node*>( 1, parallel_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::ParallelSimdFor& n )
    {
        std::cerr << "Ignoring Pragma ParallelSimdFor '" << n.prettyprint( ) << "'" << std::endl;
        return ObjectList<Node*>( );
    }
    
    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Priority& n )
    {
        PCFGClause current_clause( PRIORITY, n.get_priority( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Private& n )
    {
        PCFGClause current_clause( PRIVATE, n.get_private_symbols( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Reduction& n )
    {
        internal_error( "Reduction not yet implemented", 0 );
//         walk( n.get_reductions( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::ReductionItem& n )
    {
        internal_error( "Reduction item not yet implemented", 0 );
//         walk( n.get_reductor( ) );
//         walk( n.get_reduced_symbol( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Schedule& n )
    {
        PCFGClause current_clause( PRIVATE, n.get_chunk( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Section& n )
    {
        ObjectList<Node*> section_last_nodes = _utils->_last_nodes;

        // Create the graph node containing the SECTION
        Node* section_node = _pcfg->create_graph_node( _utils->_outer_nodes.top(), n, OMP_SECTION );
        _pcfg->connect_nodes( _utils->_last_nodes, section_node );
        _utils->_last_nodes = ObjectList<Node*>( 1, section_node->get_graph_entry_node( ) );

        // Traverse the statements of the SECTION
        walk ( n.get_statements( ) );

        // Set the exit node of the SECTION graph node
        Node* section_exit = section_node->get_graph_exit_node( );
        section_exit->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( _utils->_last_nodes, section_exit );
        _utils->_outer_nodes.pop( );

        _utils->_section_nodes.top( ).append( section_node );

        _utils->_last_nodes = section_last_nodes;
        return ObjectList<Node*>( 1, section_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Sections& n )
    {
        // Create the new graph node containing the sections
        Node* sections_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, OMP_SECTIONS );
        _pcfg->connect_nodes( _utils->_last_nodes, sections_node );

        Node* sections_entry = sections_node->get_graph_entry_node( );
        Node* sections_exit = sections_node->get_graph_exit_node( );

        // Traverse all section blocks
        _utils->_last_nodes = ObjectList<Node*>( 1, sections_entry );
        ObjectList<Node*> section_nodes;
        _utils->_section_nodes.push( section_nodes );
        walk( n.get_sections( ) );
        _utils->_last_nodes = _utils->_section_nodes.top( );
        _utils->_section_nodes.pop( );

        sections_exit->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( _utils->_last_nodes, sections_exit );

        // Set clauses info to the for node
        PCFGPragmaInfo current_pragma;
        _utils->_pragma_nodes.push( current_pragma );
        _utils->_environ_entry_exit.push( std::pair<Node*, Node*>( sections_entry, sections_exit ) );
        walk( n.get_environment( ) );
        sections_node->set_omp_node_info( _utils->_pragma_nodes.top( ) );
        _utils->_pragma_nodes.pop( );
        _utils->_environ_entry_exit.pop( );

        _utils->_outer_nodes.pop( );
        _utils->_last_nodes = ObjectList<Node*>( 1, sections_node );
        return ObjectList<Node*>( 1, sections_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Shared& n )
    {
        PCFGClause current_clause( SHARED, n.get_shared_symbols( ) );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Simd& n )
    {
        Node* simd_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, SIMD );
        _pcfg->connect_nodes( _utils->_last_nodes, simd_node );
        _utils->_last_nodes = ObjectList<Node*>( 1, simd_node->get_graph_entry_node( ) );

        // Compose the statements nodes
        walk( n.get_statement( ) );

        // Link properly the exit node
        Node* exit_node = simd_node->get_graph_exit_node( );
        exit_node->set_id( ++( _utils->_nid ) );
        exit_node->set_outer_node( _utils->_outer_nodes.top( ) );
        _pcfg->connect_nodes( _utils->_last_nodes, exit_node );

        _utils->_last_nodes = ObjectList<Node*>( 1, simd_node );
        return ObjectList<Node*>( 1, simd_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::SimdFor& n )
    {
        std::cerr << "Ignoring Pragma SimdFor '" << n.prettyprint( ) << "'" << std::endl;
        return ObjectList<Node*>( );
    }
    
    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::SimdFunction& n )
    {
        Node* simd_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, SIMD_FUNCTION );
        _pcfg->connect_nodes( _utils->_last_nodes, simd_node );
        _utils->_last_nodes = ObjectList<Node*>( 1, simd_node->get_graph_entry_node( ) );

        // Compose the statements nodes
        walk( n.get_statement( ) );

        // Link properly the exit node
        Node* exit_node = simd_node->get_graph_exit_node( );
        exit_node->set_id( ++( _utils->_nid ) );
        exit_node->set_outer_node( _utils->_outer_nodes.top( ) );
        _pcfg->connect_nodes( _utils->_last_nodes, exit_node );

        _utils->_last_nodes = ObjectList<Node*>( 1, simd_node );
        return ObjectList<Node*>( 1, simd_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Single& n )
    {
        // Create the new graph node containing the single
        Node* single_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, OMP_SINGLE );
        _pcfg->connect_nodes( _utils->_last_nodes, single_node );

        Node* single_entry = single_node->get_graph_entry_node( );
        Node* single_exit = single_node->get_graph_exit_node( );

        // Traverse the statements of the current single
        _utils->_last_nodes = ObjectList<Node*>( 1, single_entry );
        walk( n.get_statements( ) );

        single_exit->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( _utils->_last_nodes, single_exit );

        // Set clauses info to the for node
        PCFGPragmaInfo current_pragma;
        _utils->_pragma_nodes.push( current_pragma );
        _utils->_environ_entry_exit.push( std::pair<Node*, Node*>( single_entry, single_exit ) );
        walk( n.get_environment( ) );
        single_node->set_omp_node_info( _utils->_pragma_nodes.top( ) );
        _utils->_pragma_nodes.pop( );
        _utils->_environ_entry_exit.pop( );

        _utils->_outer_nodes.pop( );
        _utils->_last_nodes = ObjectList<Node*>( 1, single_node );
        return ObjectList<Node*>( 1, single_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Target& n )
    {
        PCFGClause current_clause( TARGET );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Task& n )
    {
        // Create the new graph node containing the task
        ObjectList<Node*> task_parents = _utils->_last_nodes;
        Node* task_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, OMP_TASK, _utils->_context_nodecl.top( ) );
        int n_connects = _utils->_last_nodes.size( );
        _pcfg->connect_nodes( _utils->_last_nodes, task_node,
                              ObjectList<Edge_type>( n_connects, ALWAYS ), ObjectList<std::string>( n_connects, "" ),
                              /*is task*/ true );

        Node* task_entry = task_node->get_graph_entry_node( );
        Node* task_exit = task_node->get_graph_exit_node( );

        // Set the stack of tasks properly
        // Traverse the statements of the current task
        _utils->_task_level++;
        if( _utils->_task_level > _utils->_tasks_to_sync.size( ) )
        {
            _utils->_tasks_to_sync.push_back( ObjectList<ObjectList<Node*> >( 1, ObjectList<Node*>( 1, task_node ) ) );
        }
        else
        {
            ObjectList<ObjectList<Node*> > * tasks_in_current_level = &( _utils->_tasks_to_sync[_utils->_tasks_to_sync.size( ) - 1] );
            ObjectList<Node*> * tasks_in_last_nesting_branch = &( ( *tasks_in_current_level )[tasks_in_current_level->size() - 1] );

            if( same_parent_task( ( *tasks_in_last_nesting_branch )[/*any task in this list is valid for checking*/ 0],
                                  task_node ) )
            {
                tasks_in_last_nesting_branch->push_back( task_node );
            }
            else
            {
                tasks_in_current_level->push_back( ObjectList<Node*>( 1, task_node ) );
            }
        }
        _utils->_last_nodes = ObjectList<Node*>( 1, task_entry );
        walk( n.get_statements( ) );
        _utils->_task_level--;

        task_exit->set_id( ++( _utils->_nid ) );
        _pcfg->connect_nodes( _utils->_last_nodes, task_exit );

        // Set clauses info to the for node
        PCFGPragmaInfo current_pragma;
        _utils->_pragma_nodes.push( current_pragma );
        _utils->_environ_entry_exit.push( std::pair<Node*, Node*>( task_entry, task_exit ) );
        walk( n.get_environment( ) );
        task_node->set_omp_node_info( _utils->_pragma_nodes.top( ) );
        _utils->_pragma_nodes.pop( );
        _utils->_environ_entry_exit.pop( );

        _utils->_outer_nodes.pop( );
        _pcfg->_task_nodes_l.insert( task_node );
        _pcfg->_utils->_last_nodes = task_parents;
        return ObjectList<Node*>( 1, task_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::TaskCall& n )
    {
        internal_error( "TaskCall not yet implemented", 0 );
        walk( n.get_environment( ) );
        walk( n.get_call( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::TaskwaitDeep& n )
    {
        return visit_taskwait( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::TaskwaitShallow& n )
    {
        return visit_taskwait( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::Untied& n )
    {
        PCFGClause current_clause( UNTIED );
        _utils->_pragma_nodes.top( )._clauses.append( current_clause );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::OpenMP::WaitOnDependences& n )
    {
        Node* taskwait_node = new Node( _utils->_nid, OMP_WAITON_DEPS, _utils->_outer_nodes.top( ) );
        _pcfg->connect_nodes( _utils->_last_nodes, taskwait_node );

        _utils->_last_nodes = ObjectList<Node*>( 1, taskwait_node );
        return ObjectList<Node*>();
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ParenthesizedExpression& n )
    {
        ObjectList<Node*> current_last_nodes = _utils->_last_nodes;
        ObjectList<Node*> expression_nodes = walk( n.get_nest( ) );
        Node* parenthesized_node = merge_nodes( n, expression_nodes );
        _pcfg->connect_nodes( current_last_nodes, parenthesized_node );
        return ObjectList<Node*>( 1, parenthesized_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Plus& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    // FIXME This may not be correct
    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::PointerToMember& n )
    {
        // Tag the symbol if it is a global variable
        Scope s_sc = n.get_symbol( ).get_scope( );
        if( !s_sc.scope_is_enclosed_by( _pcfg->_sc ) )
        {
            Utils::ExtendedSymbolUsage glob_var_usage( n, Utils::undefined_usage );
            if( !Utils::usage_list_contains_sym( glob_var_usage.get_nodecl( ).get_symbol( ), _pcfg->_global_vars ) )
                _pcfg->_global_vars.insert( glob_var_usage );
        }

        // Create the node
        Node* basic_node = new Node( _utils->_nid, NORMAL, _utils->_outer_nodes.top( ), n );
        return ObjectList<Node*>( 1, basic_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Postdecrement& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Postincrement& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Power& n )
    {
        return visit_binary_node( n, n.get_lhs( ), n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::PragmaCustomStatement& n )
    {
        WARNING_MESSAGE( "Ignoring PragmaCustomStatement '%s'.",
                         n.prettyprint( ).c_str( ) );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Predecrement& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Preincrement& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Range& n )
    {
        ObjectList<Node*> lower = walk( n.get_lower( ) );
        ObjectList<Node*> upper = walk( n.get_upper( ) );
        ObjectList<Node*> stride = walk( n.get_stride( ) );

        Node* merged_limits = merge_nodes( n, lower[0], upper[0] );
        Node* merged = merge_nodes( n, merged_limits, stride[0] );
        _utils->_last_nodes = ObjectList<Node*>( 1, merged );

        return ObjectList<Node*>( 1, merged );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Reference& n )
    {
        return visit_unary_node( n, n.get_rhs( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::ReturnStatement& n )
    {
        ObjectList<Node*> return_last_nodes = _utils->_last_nodes;
        ObjectList<Node*> returned_value = walk( n.get_value( ) );
        Node* return_node = merge_nodes( n, returned_value );
        _pcfg->connect_nodes( return_last_nodes, return_node );
        _utils->_last_nodes.clear( );
        _utils->_return_nodes.append( return_node );
        return ObjectList<Node*>( 1, return_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Sizeof& n )
    {
        return visit_unary_node( n, n.get_size_type( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::StringLiteral& n )
    {
        return visit_literal_node( n );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::StructuredValue& n )
    {
        ObjectList<Node*> items = walk( n.get_items( ) );
        return ObjectList<Node*>( 1, merge_nodes( n, items ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::SwitchStatement& n )
    {
        Node* switch_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, SWITCH );
        _pcfg->connect_nodes( _utils->_last_nodes, switch_node );
        Node* entry_node = switch_node->get_graph_entry_node( );
        Node* exit_node = switch_node->get_graph_exit_node( );

        // Build condition node
        ObjectList<Node*> cond_last_nodes = _utils->_last_nodes;
        ObjectList<Node*> cond_node_l = walk( n.get_switch( ) );
        _pcfg->connect_nodes( entry_node, cond_node_l[0] );

        // Compose the statements nodes
        _utils->_last_nodes.clear( );
        _utils->_switch_nodes.push( new PCFGSwitch( cond_node_l[0], exit_node ) );
        _utils->_break_nodes.push( exit_node );
        walk( n.get_statement( ) );
        _utils->_break_nodes.pop( );
        _utils->_switch_nodes.pop( );

        // Link properly the exit node
        exit_node->set_id( ++( _utils->_nid ) );
        exit_node->set_outer_node( _utils->_outer_nodes.top( ) );
        _utils->_outer_nodes.pop( );

        // Finish computation of switch exit nodes
        if( cond_node_l[0]->get_exit_edges( ).empty( ) )
        {   // There is no node node inside the statement
            _pcfg->connect_nodes( cond_node_l[0], exit_node );
        }
        else
        {   // If there is some node in '_last_nodes' we connect it to the exit (Last Case stmt have not a Break stmt)
            _pcfg->connect_nodes( _utils->_last_nodes, exit_node );
        }

        _utils->_last_nodes = ObjectList<Node*>( 1, switch_node );
        return ObjectList<Node*>( 1, switch_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Symbol& n )
    {
        // Tag the symbol if it is a global variable
        Scope s_sc = n.get_symbol( ).get_scope( );
        if( !s_sc.scope_is_enclosed_by( _pcfg->_sc ) )
        {
            Utils::ExtendedSymbolUsage glob_var_usage( n, Utils::undefined_usage );
            if( !Utils::usage_list_contains_sym( glob_var_usage.get_nodecl( ).get_symbol( ), _pcfg->_global_vars ) )
            {
                _pcfg->_global_vars.insert( glob_var_usage );
            }
        }

        // Create the node
        return visit_literal_node( n );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Text& n )
    {
        return visit_literal_node( n );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Throw& n )
    {
        ObjectList<Node*> right = walk( n.get_rhs( ) );
        Node* throw_node;
        if( right.empty( ) )
        {   // Throw has no expression associated, we create now the throw node
            throw_node = _pcfg->append_new_node_to_parent( _utils->_last_nodes, n );
        }
        else
        {   // Some expression has been built. We merge here the node with the whole throw node
            throw_node = merge_nodes( n, right[0], NULL );
            _pcfg->connect_nodes( _utils->_last_nodes, throw_node );
        }

        if( !_utils->_tryblock_nodes.empty( ) )
        {
            for( ObjectList<PCFGTryBlock*>::reverse_iterator it = _utils->_tryblock_nodes.rbegin( );
                it != _utils->_tryblock_nodes.rend( ); ++it )
            {
                ( *it )->_handler_parents.append( throw_node );
            }
        }
        // Throw must be connected to the Graph exit as well
        _pcfg->connect_nodes( throw_node, _pcfg->_graph->get_graph_exit_node( ) );

        _utils->_last_nodes.clear( );
        return ObjectList<Node*>( 1, throw_node );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::TryBlock& n )
    {
        PCFGTryBlock* new_try_block = new PCFGTryBlock( );
        _utils->_tryblock_nodes.append( new_try_block );
        ObjectList<Node*> try_parents = _utils->_last_nodes;
        ObjectList<Node*> try_stmts = walk( n.get_statement( ) );

        Node* first_try_node = try_parents[0]->get_exit_edges( )[0]->get_target( );
        compute_catch_parents( first_try_node );
        _pcfg->clear_visits( first_try_node );

        ObjectList<Node*> handlers_l = walk( n.get_catch_handlers( ) );

        // Process the ellipsis
        ObjectList<Node*> ellipsis_parents = _utils->_last_nodes;
        PCFGTryBlock* current_tryblock = _utils->_tryblock_nodes.back( );
        _utils->_last_nodes = current_tryblock->_handler_parents;
        ObjectList<Node*> ellipsis_l = walk( n.get_any( ) );
        if( !ellipsis_l.empty( ) )
        {
            current_tryblock->_nhandlers++;
            current_tryblock->_handler_exits.append( _utils->_last_nodes );

            // Set the type of the edge between each handler parent and the actual handler
            for( ObjectList<Node*>::iterator it = current_tryblock->_handler_parents.begin( );
                 it != current_tryblock->_handler_parents.end( ); ++it )
            {
                Edge* catch_edge = ( *it )->get_exit_edges( ).back( );
                catch_edge->set_catch_edge( );
                catch_edge->set_label( "" );
            }
        }

        _utils->_last_nodes = current_tryblock->_handler_exits;

        _utils->_tryblock_nodes.pop_back( );
        if( !try_stmts.empty( ) )
            return try_stmts;
        else if( !handlers_l.empty( ) )
            return handlers_l;
        else if( !ellipsis_l.empty( ) )
            return ellipsis_l;

        return ObjectList<Node*>( );
    }

    //TODO Test this kind of node
    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Type& n )
    {
        return visit_literal_node( n );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::Typeid& n )
    {
        return visit_unary_node( n, n.get_arg( ) );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::UnknownPragma& n )
    {
        WARNING_MESSAGE( "Ignoring unknown prama '%s' during PCFG construction", n.get_text( ).c_str( ) );
        return ObjectList<Node*>( );
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::VirtualFunctionCall& n )
    {
        return visit_function_call(n);
    }

    ObjectList<Node*> PCFGVisitor::visit( const Nodecl::WhileStatement& n )
    {
        Node* while_graph_node = _pcfg->create_graph_node( _utils->_outer_nodes.top( ), n, LOOP_WHILE );
        _pcfg->connect_nodes( _utils->_last_nodes, while_graph_node );
        Node* while_exit = while_graph_node->get_graph_exit_node( );

        // Build condition node
        Node* cond_node = walk( n.get_condition( ) )[0];
        _pcfg->connect_nodes( while_graph_node->get_graph_entry_node( ), cond_node );
        _utils->_last_nodes = ObjectList<Node*>( 1, cond_node );

        // Build the while body node/s
        _utils->_continue_nodes.push( cond_node );
        _utils->_break_nodes.push( while_exit );
        walk( n.get_statement( ) );    // This list of nodes returned here will never be used
        _utils->_continue_nodes.pop( );
        _utils->_break_nodes.pop( );

        _pcfg->connect_nodes( _utils->_last_nodes, cond_node );
        ObjectList<Edge*> cond_exits = cond_node->get_exit_edges( );
        for( ObjectList<Edge*>::iterator it = cond_exits.begin( ); it != cond_exits.end( ); ++it )
            ( *it )->set_true_edge( );
        _pcfg->connect_nodes( cond_node, while_exit, FALSE_EDGE );

        // Build the exit node
        while_exit->set_id( ++( _utils->_nid ) );
        while_exit->set_outer_node( _utils->_outer_nodes.top( ) );
        _utils->_outer_nodes.pop( );

        _utils->_last_nodes = ObjectList<Node*>( 1, while_graph_node );
        return ObjectList<Node*>( 1, while_graph_node );
    }

    // ******************************** END visiting methods ******************************** //
    // ************************************************************************************** //

}
}
