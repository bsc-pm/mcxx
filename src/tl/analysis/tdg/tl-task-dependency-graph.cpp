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
    typedef ObjectList<Nodecl::NodeclBase> Nodecl_list;
    
    static TDG_Node_list unsynchronized_tasks;
    static int id = 0;
    
    TDG_Node::TDG_Node( Node* n )
    {
        _id = ++id;
        if( n == NULL )
        {
            _pcfg_node = NULL;
            _type = Control;
        }
        else if( n->is_omp_task_node( ) )
        {
            _pcfg_node = n;
            _type = Task;
        }
        else if( n->is_omp_taskwait_node( ) )
        {
            _pcfg_node = NULL;
            _type = Taskwait;
        }
        else if( n->is_omp_barrier_node( ) )
        {
            _pcfg_node = NULL;
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
    
    void TDG_Node::clear_visits( TDG_Node* current )
    {
        if( current->_visited )
        {
            current->_visited = false;
            
            TDG_Node_list children = current->get_children( );
            for( TDG_Node_list::iterator it = children.begin( ); it != children.end( ); ++it )
                clear_visits( *it );
        }
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
                {}  // Ignore them, we need them here
                else
                {
                    WARNING_MESSAGE( "Ignoring clause %s for task %s. Maybe we should do something with it...\n", 
                                     it->prettyprint( ).c_str( ) );
                }
            }
        }
        
        return result;
    }
    
    TDG_Edge::TDG_Edge( TDG_Node* source, TDG_Node* target )
        : _source( source ), _target( target ), _source_clauses( ), _target_clauses( )
    {
        if( ( source->_pcfg_node != NULL ) && source->_pcfg_node->is_omp_task_node( ) )
        {
            Nodecl::OpenMP::Task task = source->_pcfg_node->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( );
            _source_clauses = get_task_dependency_clauses( task );
        }
        if( ( target->_pcfg_node != NULL ) && target->_pcfg_node->is_omp_task_node( ) )
        {
            Nodecl::OpenMP::Task task = target->_pcfg_node->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( );
            _target_clauses = get_task_dependency_clauses( task );
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
        : _entry( NULL ), _exit( NULL ), _last_node( NULL ), _pcfg( pcfg )
    {
        _entry = new TDG_Node( NULL );
        _last_node = _entry;
        Node* pcfg_node = _pcfg->get_graph( );
        
        // The whole code must be taskified, otherwise some dependences may not be shown
        taskify_graph( pcfg_node );
        
        // Compute the Task Dependency Graph from the PCFG
        create_tdg( pcfg_node );
        ExtensibleGraph::clear_visits( pcfg_node );
        
        _exit = new TDG_Node( NULL );
        connect_tdg_nodes( _last_node, _exit );
    }
    
    std::string TaskDependencyGraph::get_name( ) const
    {
        std::string name;
        if( _pcfg != NULL )
            name = _pcfg->get_name( );
        return name;
    }
    
    void TaskDependencyGraph::connect_tdg_nodes( TDG_Node* parent, TDG_Node* child )
    {
        TDG_Edge* edge = new TDG_Edge( parent, child );
        parent->set_exit( edge );
        child->set_entry( edge );
    }
    
    static TDG_Node* find_and_remove_task_from_unsynchronized_tasks( Node* task )
    {
        TDG_Node* result = NULL;
        for( TDG_Node_list::iterator it = unsynchronized_tasks.begin( ); 
             it != unsynchronized_tasks.end( ); ++it )
        {
            if( ( *it )->_pcfg_node == task )
            {
                result = *it;
                unsynchronized_tasks.erase( it );
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
        if( !current->is_visited( ) )
        {
            current->set_visited( true );
            
            if( current->is_graph_node( ) )
            {
                if( current->is_omp_task_node( ) )
                {
                    TDG_Node* tdg_current = new TDG_Node( current );
                    if( _last_node == _entry )
                        connect_tdg_nodes( _entry, tdg_current );
                    _last_node = tdg_current;
                    unsynchronized_tasks.insert( tdg_current );
                    
                    // Connect the current task with all previous tasks it was dependent with
                    Node_list task_parents = current->get_parents( );
                    if( task_parents.size( ) > 1 )
                    {   // There are previous tasks synchronized with the current one in the PCFG
                        for( Node_list::iterator it = task_parents.begin( ); it != task_parents.end( ); ++it )
                        {
                            if( ( *it )->is_omp_task_node( ) )
                            {
                                // TODO Check whether or not there is a dependence here
                                TDG_Node* tdg_task = find_and_remove_task_from_unsynchronized_tasks( *it );
                                connect_tdg_nodes( tdg_task, tdg_current );
                            }
                        }
                    }
                }
                
                create_tdg( current->get_graph_entry_node( ) );
            }
            else if( current->is_omp_taskwait_node( ) || current->is_omp_barrier_node( ) )
            {
                // Connect all tasks synchronized here with the new Taskwait/Barrier TDG_Node
                TDG_Node* tdg_current = new TDG_Node( current );
                _last_node = tdg_current;
                Node_list parents = current->get_parents( );
                for( Node_list::iterator it = parents.begin( ); it != parents.end( ); ++it )
                {
                    TDG_Node* tdg_task = find_and_remove_task_from_unsynchronized_tasks( *it );
                    connect_tdg_nodes( tdg_task, tdg_current );
                }
            }
            
            // Iterate over the children
            Node_list children = current->get_children( );
            for( Node_list::iterator it = children.begin( ); it != children.end( ); ++it )
                create_tdg( *it );
        }
    }
    
    static std::string prettyprint_clauses( Nodecl_list clauses )
    {
        std::string result;
        
        for( Nodecl_list::iterator it = clauses.begin( ); it != clauses.end( ); )
        {
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
    
    void TaskDependencyGraph::print_tdg_to_dot_rec( TDG_Node* current, std::ofstream& dot_tdg )
    {
        if( !current->_visited )
        {
            current->_visited = true;
            std::string current_id = "";
            
            // Create the node
            Node* current_pcfg_node = current->_pcfg_node;
            std::string task_label = "";
            if( current_pcfg_node != NULL )
            {
                // Get the name of the task
                Nodecl::OpenMP::Task task = current_pcfg_node->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( );
                task_label = task.get_locus_str( );
                std::stringstream ss; ss << current->_id;
                current_id = ss.str( );
                Nodecl::List environ = task.get_environment( ).as<Nodecl::List>( );
                for( Nodecl::List::iterator it = environ.begin( ); it != environ.end( ); ++it )
                     if( it->is<Nodecl::OpenMP::TaskLabel>( ) )
                {
                    task_label = "_" + it->prettyprint( );
                    break;
                }
            }
            else
            {
                std::stringstream ss; ss << current->_id;
                current_id = ss.str( );
                if( current->_type == Control )
                {
                    if( current->_entries.empty( ) )
                        task_label = "Entry Point";
                    else
                        task_label = "Exit Point";
                }
                else
                    task_label = "SYNC";
            }
            dot_tdg << "\t" + current_id + " [label=\"" + task_label + "\"];\n";
            
            // Create the connections from the current node to its children
            TDG_Node_list children = current->get_children( );
            TDG_Edge_list::iterator edge_it = current->_exits.begin( );
            std::string label;
            for( TDG_Node_list::iterator it = children.begin( ); it != children.end( ); ++it, ++edge_it )
            {
                // Get clauses info in a string
                label += prettyprint_clauses( ( *edge_it )->_source_clauses );
                if( !( *edge_it )->_source_clauses.empty( ) && !( *edge_it )->_target_clauses.empty( ) )
                    label += "\\n";
                label += prettyprint_clauses( ( *edge_it )->_target_clauses );
                
                // Create the dot edge
                std::stringstream child_id; child_id << ( *it )->_id;
                dot_tdg << "\t" << current_id << " -> " << child_id.str( ) << "[label=\"" << label << "\"]" << "\n";
                
                // Call recursively with the current child
                print_tdg_to_dot_rec( *it, dot_tdg );
            }
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
                internal_error ( "An error occurred while creating the dot files directory in '%s'", directory_name.c_str( ) );
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
            print_tdg_to_dot_rec( _entry, dot_tdg );
        dot_tdg << "}\n";
        
        TDG_Node::clear_visits( _entry );
        ExtensibleGraph::clear_visits( _pcfg->get_graph( ) );
        
        dot_tdg.close( );
        if( !dot_tdg.good( ) )
            internal_error ("Unable to close the file '%s' where PCFG has been stored.", dot_file_name.c_str( ) );
    }
    
}
}