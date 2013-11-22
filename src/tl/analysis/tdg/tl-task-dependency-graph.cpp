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
    typedef ObjectList<Node*> Node_list;
    
    static TDG_Node_list unsynchronized_tasks;
    static int dot_current_id = 0;
    
    TDG_Node::TDG_Node( Node* n )
    {
        if( n->is_omp_task_node( ) )
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
    
    void TDG_Node::set_entry( TDG_Edge* exit )
    {
        _exits.insert( exit );
    }
    
    void TDG_Node::set_exit( TDG_Edge* exit )
    {
        _exits.insert( exit );
    }
    
    ObjectList<TDG_Node*> TDG_Node::get_children( )
    {
        ObjectList<TDG_Node*> result;
        for( ObjectList<TDG_Edge*>::iterator it = _exits.begin( ); it != _exits.end( ); ++it )
            result.insert( ( *it )->get_target( ) );
        return result;
    }
    
    void TDG_Node::clear_visits( TDG_Node* current )
    {
        if( current->_visited )
        {
            current->_visited = false;
            
            ObjectList<TDG_Node*> children = current->get_children( );
            for( ObjectList<TDG_Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                clear_visits( *it );
        }
    }
    
    TDG_Edge::TDG_Edge( TDG_Node* source, TDG_Node* target )
        : _source( source ), _target( target )
    {}
    
    TDG_Node* TDG_Edge::get_source( )
    {
        return _source;
    }
    
    TDG_Node* TDG_Edge::get_target( )
    {
        return _target;
    }
    
    TaskDependencyGraph::TaskDependencyGraph( ExtensibleGraph* pcfg )
        : _entry( NULL ), _exit( NULL ), _last_nodes( ), _pcfg( pcfg )
    {
        _entry = new TDG_Node( NULL );
        
        // TODO Compute the Task Dependency Graph from the PCFG
        Node* pcfg_node = _pcfg->get_graph( );
        create_tdg( pcfg_node );
        ExtensibleGraph::clear_visits( pcfg_node );
        
        _exit = new TDG_Node( NULL );
        connect_tdg_nodes( _last_nodes, _exit );
    }
    
    void TaskDependencyGraph::connect_tdg_nodes( TDG_Node_list parents, TDG_Node* child )
    {
        for( TDG_Node_list::iterator it = parents.begin( ); it != parents.end( ); ++it )
            connect_tdg_nodes( *it, child );
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
                    connect_tdg_nodes( _last_nodes, tdg_current );
                    unsynchronized_tasks.insert( tdg_current );
                    
                    // Connect the current task with all previous tasks it was dependent with
                    Node* task_creation = current->get_parents( )[0];
                    Node_list task_parents = task_creation->get_parents( );
                    if( task_parents.size( ) > 1 )
                    {   // There are previous tasks synchronized with the current one in the PCFG
                        for( Node_list::iterator it = task_parents.begin( ); it != task_parents.end( ); ++it )
                        {
                            TDG_Node* tdg_task = find_and_remove_task_from_unsynchronized_tasks( *it );
                            connect_tdg_nodes( tdg_task, tdg_current );
                        }
                    }
                }
                
                create_tdg( current->get_graph_entry_node( ) );
            }
            else if( current->is_omp_taskwait_node( ) || current->is_omp_barrier_node( ) )
            {
                // Connect all tasks synchronized here with the new Taskwait/Barrier TDG_Node
                TDG_Node* tdg_current = new TDG_Node( current );
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
    
    void TaskDependencyGraph::print_tdg_to_dot_rec( TDG_Node* current, std::ofstream& dot_tdg )
    {
        if( !current->_visited )
        {
            current->_visited = true;
            TDG_Node_list children = current->get_children( );
            std::string current_id = "";
            
            // Create the node
            Node* current_pcfg_node = current->_pcfg_node;
            std::string task_label = "";
            if( current_pcfg_node != NULL )
            {
                // Get the name of the task
                Nodecl::OpenMP::Task task = current_pcfg_node->get_graph_label( ).as<Nodecl::OpenMP::Task>( );
                task_label = task.get_locus_str( );
                std::stringstream ss; ss << ++dot_current_id;
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
                std::stringstream ss; ss << ++dot_current_id;
                current_id = ss.str( );
                task_label = "SYNC";
            }
            dot_tdg << "  " + current_id + " label=\"{" + task_label + "}\"\n";
            
            // Create the connections from the current node to its children
            std::string child_id;
            for( TDG_Node_list::iterator it = children.begin( ); it != children.end( ); ++it )
            {
                // TODO
                if( !( *it )->_visited )
                {
                    std::stringstream ss; ss << ++dot_current_id;
                    child_id = ss.str( );
                    dot_tdg << current_id << " -> " << child_id << "\n";
                }
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
        
        std::string date_str;
        {
            time_t t = time(NULL);
            struct tm* tmp = localtime(&t);
            if (tmp == NULL)
            {
                internal_error("localtime failed", 0);
            }
            char outstr[200];
            if (strftime(outstr, sizeof(outstr), "%s", tmp) == 0)
            {
                internal_error("strftime failed", 0);
            }
            outstr[199] = '\0';
            date_str = outstr;
        }
        
        std::ofstream dot_tdg;
        std::string dot_file_name = directory_name + _pcfg->get_name( ) + "_" + date_str + "_tdg.dot";
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