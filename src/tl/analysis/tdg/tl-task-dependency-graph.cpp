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

#include <fstream>
#include <sys/stat.h>

#include "tl-pcfg-utils.hpp"
#include "tl-task-dependency-graph.hpp"

namespace TL { 
namespace Analysis {
    
    typedef ObjectList<TDG_Node*> TDG_Node_list;
    typedef ObjectList<TDG_Edge*> TDG_Edge_list;
    typedef ObjectList<Node*> Node_list;
    typedef ObjectList<Edge*> Edge_list;
    typedef ObjectList<Nodecl::NodeclBase> Nodecl_list;
    
    static int id = 0;
    
    TDG_Node::TDG_Node( Node* n )
    {
        _id = ++id;
        if( n->is_omp_task_node( ) )
        {
            _pcfg_node = n;
            _type = Task;
        }
        else if( n->is_omp_taskwait_node( ) )
        {
            _pcfg_node = n;
            _type = Taskwait;
        }
        else if( n->is_omp_barrier_node( ) )
        {
            _pcfg_node = n;
            _type = Barrier;
        }
        else
        {
            internal_error( "Unexpected node type %s when creating a TDG node", 
                            n->get_type_as_string( ).c_str( ) );
        }
        _visited = false;
    }
    
    void TDG_Node::set_entry( TDG_Edge* entry )
    {
        _entries.insert( entry );
    }
    
    void TDG_Node::set_exit( TDG_Edge* exit )
    {
        _exits.insert( exit );
    }
    
    TDG_Node_list TDG_Node::get_children( )
    {
        TDG_Node_list result;
        for( TDG_Edge_list::iterator it = _exits.begin( ); it != _exits.end( ); ++it )
            result.insert( ( *it )->get_target( ) );
        return result;
    }
    
    static Nodecl_list get_task_dependency_clauses( const Nodecl::OpenMP::Task& task )
    {
        Nodecl_list result;
        
        Nodecl::List task_environ = task.get_environment( ).as<Nodecl::List>( );
        for( Nodecl::List::iterator it = task_environ.begin( ); it != task_environ.end( ); ++it )
        {
            if( it->is<Nodecl::OpenMP::DepIn>( ) || it->is<Nodecl::OpenMP::DepInout>( ) || it->is<Nodecl::OpenMP::DepOut>( ) )
            {
                result.insert( *it );
            }
            else
            {
                if( it->is<Nodecl::OpenMP::Target>( ) || it->is<Nodecl::OpenMP::If>( ) || 
                    it->is<Nodecl::OpenMP::Final>( ) || it->is<Nodecl::OpenMP::Untied>( ) ||
                    it->is<Nodecl::OpenMP::Firstprivate>( ) || it->is<Nodecl::OpenMP::Private>( ) || 
                    it->is<Nodecl::OpenMP::Shared>( ) || 
                    it->is<Nodecl::OpenMP::FlushAtEntry>( ) || it->is<Nodecl::OpenMP::FlushAtExit>( ) )
                {}  // Ignore them, we expect them here
                else
                {
                    WARNING_MESSAGE( "Ignoring clause %s for task %s. Maybe we should do something with it...\n", 
                                     it->prettyprint( ).c_str( ) );
                }
            }
        }
        
        return result;
    }
    
    TDG_Edge::TDG_Edge( TDG_Node* source, TDG_Node* target, std::string type )
        : _source( source ), _target( target ), _type( type ), 
          _source_clauses( ), _target_clauses( ), _condition( Nodecl::NodeclBase::null( ) )
    {
        bool connecting_tasks = true;
        // Fill source and target lists with the corresponding clauses
        if( source->_pcfg_node->is_omp_task_node( ) )
        {
            Nodecl::OpenMP::Task task = source->_pcfg_node->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( );
            _source_clauses = get_task_dependency_clauses( task );
        }
        else
            connecting_tasks = false;
        if( target->_pcfg_node->is_omp_task_node( ) )
        {
            Nodecl::OpenMP::Task task = target->_pcfg_node->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( );
            _target_clauses = get_task_dependency_clauses( task );
        }
        else
            connecting_tasks = false;
        
        // Calculate the condition of the edge (only when connecting tasks, otherwise there is no condition)
        if( connecting_tasks )
        {
            for( ObjectList<Nodecl::NodeclBase>::iterator its = _source_clauses.begin( ); 
                 its != _source_clauses.end( ); ++its )
            {
                if( its->is<Nodecl::OpenMP::DepOut>( ) || its->is<Nodecl::OpenMP::DepInout>( ) )
                {
                    // We abuse here the structure of Nodecl::OpenMP::DepXXX. Since all are the same, we use Inout generically
                    Nodecl::List sargs = its->as<Nodecl::OpenMP::DepInout>( ).get_inout_deps( ).as<Nodecl::List>( );
                    for( ObjectList<Nodecl::NodeclBase>::iterator itt = _target_clauses.begin( ); 
                         itt != _target_clauses.end( ); ++itt )
                    {
                        if( itt->is<Nodecl::OpenMP::DepIn>( ) || itt->is<Nodecl::OpenMP::DepInout>( ) )
                        {
                            Nodecl::List targs = its->as<Nodecl::OpenMP::DepInout>( ).get_inout_deps( ).as<Nodecl::List>( );
                            
                            // TODO
                            
                        }
                    }
                }
            }
        }
    }
    
    TDG_Node* TDG_Edge::get_source( )
    {
        return _source;
    }
    
    TDG_Node* TDG_Edge::get_target( )
    {
        return _target;
    }
    
    TaskDependencyGraph::TaskDependencyGraph( ExtensibleGraph* pcfg )
        : _pcfg( pcfg ), _tdg_nodes( )
    {
        Node* pcfg_node = _pcfg->get_graph( );
        
        // The whole code must be taskified, otherwise some dependences may not be shown
        taskify_graph( pcfg_node );
        
        // Compute the Task Dependency Graph from the PCFG
        create_tdg( pcfg_node );
    }
    
    std::string TaskDependencyGraph::get_name( ) const
    {
        std::string name;
        if( _pcfg != NULL )
            name = _pcfg->get_name( );
        return name;
    }
    
    void TaskDependencyGraph::connect_tdg_nodes( TDG_Node* parent, TDG_Node* child, std::string type )
    {
        TDG_Edge* edge = new TDG_Edge( parent, child, type );
        parent->set_exit( edge );
        child->set_entry( edge );
    }
    
    TDG_Node* TaskDependencyGraph::find_task_from_tdg_nodes_list( Node* task )
    {
        TDG_Node* result = NULL;
        for( TDG_Node_list::iterator it = _tdg_nodes.begin( ); it != _tdg_nodes.end( ); ++it )
        {
            if( ( *it )->_pcfg_node == task )
            {
                result = *it;
                break;
            }
        }
        return result;
    }
    
    // TODO
    void TaskDependencyGraph::taskify_graph( Node* current )
    {
    }
    
    void TaskDependencyGraph::create_tdg( Node* current )
    {
        create_tdg_nodes_from_pcfg( current );
        ExtensibleGraph::clear_visits( current );
        connect_tdg_nodes_from_pcfg( current );
        ExtensibleGraph::clear_visits( current );
    }
    
    void TaskDependencyGraph::create_tdg_nodes_from_pcfg( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );
            
            if( current->is_graph_node( ) )
            {
                // Create the TDG task node if we traverse a PCFG task node
                if( current->is_omp_task_node( ) )
                {
                    TDG_Node* tdg_current = new TDG_Node( current );
                    _tdg_nodes.insert( tdg_current );
                }
                
                // Call recursively with task inner nodes
                create_tdg_nodes_from_pcfg( current->get_graph_entry_node( ) );
            }
            else if( current->is_omp_taskwait_node( ) || current->is_omp_barrier_node( ) )
            {
                TDG_Node* tdg_current = new TDG_Node( current );
                _tdg_nodes.insert( tdg_current );
            }
            
            // Iterate over the children
            Node_list children = current->get_children( );
            for( Node_list::iterator it = children.begin( ); it != children.end( ); ++it )
                create_tdg_nodes_from_pcfg( *it );
        }
    }
    
    void TaskDependencyGraph::connect_tdg_nodes_from_pcfg( Node* current )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );
            
            if( current->is_graph_node( ) )
            {
                // Connect the current node if it is a task
                if( current->is_omp_task_node( ) )
                {
                    // Connect the current task with all previous tasks it was dependent with
                    TDG_Node* tdg_task = find_task_from_tdg_nodes_list( current );
                    Edge_list task_exits = current->get_exit_edges( );
                    for( Edge_list::iterator it = task_exits.begin( ); it != task_exits.end( ); ++it )
                    {
                        Node* child = ( *it )->get_target( );
                        if( child->is_omp_task_node( ) || child->is_omp_taskwait_node( ) || child->is_omp_barrier_node( ) )
                        {
                            // TODO: Check whether or not there is a dependence here
                            if( true )
                            {   // Connect the task
                                TDG_Node* tdg_child_task = find_task_from_tdg_nodes_list( child );
                                connect_tdg_nodes( tdg_task, tdg_child_task, ( *it )->get_label( ) );
                            }
                        }
                    }
                }
                
                connect_tdg_nodes_from_pcfg( current->get_graph_entry_node( ) );
            }
            else if( current->is_omp_taskwait_node( ) || current->is_omp_barrier_node( ) )
            {
                // Connect all tasks synchronized here with the new Taskwait/Barrier TDG_Node
                TDG_Node* tdg_sync = find_task_from_tdg_nodes_list( current );
                Edge_list sync_exits = current->get_exit_edges( );
                for( Edge_list::iterator it = sync_exits.begin( ); it != sync_exits.end( ); ++it )
                {
                    Node* child = ( *it )->get_target( );
                    if( child->is_omp_task_node( ) || child->is_omp_taskwait_node( ) || child->is_omp_barrier_node( ) )
                    {
                        TDG_Node* tdg_child_task = find_task_from_tdg_nodes_list( child );
                        connect_tdg_nodes( tdg_sync, tdg_child_task, ( *it )->get_label( ) );
                    }
                }
            }
            
            // Iterate over the children
            Node_list children = current->get_children( );
            for( Node_list::iterator it = children.begin( ); it != children.end( ); ++it )
                connect_tdg_nodes_from_pcfg( *it );
        }
    }
    
    static std::string prettyprint_clauses( Nodecl_list clauses )
    {
        std::string result;
        
        for( Nodecl_list::iterator it = clauses.begin( ); it != clauses.end( ); )
        {
            // Note: there is no codegen for OpenMP nodecls, 
            // that is why we print it manually instead of calling prettyprint
            std::string clause_name;
            std::string clause_args;
            Nodecl::List args;
            if( it->is<Nodecl::OpenMP::DepIn>( ) )
            {
                clause_name = "in";
                args =  it->as<Nodecl::OpenMP::DepIn>( ).get_in_deps( ).as<Nodecl::List>( );
            }
            else if( it->is<Nodecl::OpenMP::DepOut>( ) )
            {
                clause_name = "out";
                args =  it->as<Nodecl::OpenMP::DepOut>( ).get_out_deps( ).as<Nodecl::List>( );
            }
            else if( it->is<Nodecl::OpenMP::DepInout>( ) )
            {
                clause_name = "inout";
                args =  it->as<Nodecl::OpenMP::DepInout>( ).get_inout_deps( ).as<Nodecl::List>( );
            }
            
            for( Nodecl::List::iterator it_a = args.begin( ); it_a != args.end( ); )
            {
                clause_args += it_a->prettyprint( );
                ++it_a;
                if( it_a != args.end( ) )
                    clause_args += ", ";
            }
            result += clause_name + "(" + clause_args + ")";
            
            ++it;
            if( it != clauses.end( ) )
                result += ", ";
        }
        
        return result;
    }
    
    void TaskDependencyGraph::print_tdg_node_to_dot( TDG_Node* current, std::ofstream& dot_tdg )
    {
        std::stringstream ss; ss << current->_id;
        std::string current_id = ss.str( );
        
        // Create the node
        std::string task_label = "";
        TDGNodeType type = current->_type;
        if( type == Task )
        {
            // Get the name of the task
            Nodecl::OpenMP::Task task = current->_pcfg_node->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( );
            task_label = task.get_locus_str( );
            Nodecl::List environ = task.get_environment( ).as<Nodecl::List>( );
            for( Nodecl::List::iterator it = environ.begin( ); it != environ.end( ); ++it )
                if( it->is<Nodecl::OpenMP::TaskLabel>( ) )
                {
                    task_label = "_" + it->prettyprint( );
                    break;
                }
        }
        else if( type == Taskwait )
        {
            task_label = "Taskwait";
        }
        else if( type == Barrier )
        {
            task_label = "Barrier";
        }
        else
        {
            WARNING_MESSAGE( "Unexpected TDG node type %d. Ignoring it.", type );
        }
        dot_tdg << "\t" + current_id + " [label=\"" + task_label + "\"];\n";
        
        // Create the connections from the current node to its children
        std::string headlabel, taillabel, style;
        for( TDG_Edge_list::iterator it = current->_exits.begin( ); it != current->_exits.end( ); ++it )
        {
            // Get the edge info in a string
            headlabel = "headlabel=\"" + prettyprint_clauses( ( *it )->_target_clauses ) + "\"";
            taillabel = "taillabel=\"" + prettyprint_clauses( ( *it )->_source_clauses ) + "\"";
            std::string edge_type = ( *it )->_type;
            style = "style=\""
                  + std::string( ( edge_type == "strict" || edge_type == "static" ) ? "solid" 
                                                                                    : "dashed" )
                  + "\"";
            
            // Create the dot edge
            std::stringstream child_id; child_id << ( *it )->_target->_id;
            dot_tdg << "\t" << current_id << " -> " << child_id.str( ) 
                    << "[" << style << ", " << headlabel << ", " << taillabel << "];\n";
        }
    }
    
    void TaskDependencyGraph::print_tdg_to_dot( )
    {
        // Create the directory of dot files if it has not been previously created
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if( err == NULL )
            internal_error ( "An error occurred while getting the path of the current directory", 0 );
        struct stat st;
        std::string directory_name = std::string( buffer ) + "/dot/";
        if( stat( directory_name.c_str( ), &st ) != 0 )
        {
            int dot_directory = mkdir( directory_name.c_str( ), S_IRWXU );
            if( dot_directory != 0 )
                internal_error ( "An error occurred while creating the dot files directory in '%s'", 
                                 directory_name.c_str( ) );
        }
        
        std::string dot_file_name = directory_name + _pcfg->get_name( ) + "_tdg.dot";
        
        std::ofstream dot_tdg;
        dot_tdg.open( dot_file_name.c_str( ) );
        if( !dot_tdg.good( ) )
            internal_error ("Unable to open the file '%s' to store the PCFG.", dot_file_name.c_str( ) );
        
        // Create the dot graphs
        if( VERBOSE )
            std::cerr << "- TDG File '" << dot_file_name << "'" << std::endl;
        
        dot_tdg << "digraph TDG {\n";
            dot_tdg << "\tcompound=true;\n";
            for( TDG_Node_list::iterator it = _tdg_nodes.begin( ); it != _tdg_nodes.end( ); ++it )
                print_tdg_node_to_dot( *it, dot_tdg );
        dot_tdg << "}\n";
        
        ExtensibleGraph::clear_visits( _pcfg->get_graph( ) );
        
        dot_tdg.close( );
        if( !dot_tdg.good( ) )
            internal_error ("Unable to close the file '%s' where PCFG has been stored.", dot_file_name.c_str( ) );
    }
    
}
}