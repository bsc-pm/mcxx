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
    
    TDG_Node::TDG_Node( Node* n, TDGNodeType type )
        : _id( ++id ), _pcfg_node( n ), _type( type ), _entries( ), _exits( )
    {}
    
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
    
    TDG_Edge::TDG_Edge( TDG_Node* source, TDG_Node* target, TDGEdgeType type, const Nodecl::NodeclBase& condition )
        : _source( source ), _target( target ), _type( type ), 
          _source_clauses( ), _target_clauses( ), _condition( condition )
    {
        // Fill source and target lists with the corresponding clauses
        if( source->_pcfg_node->is_omp_task_node( ) )
        {
            Nodecl::OpenMP::Task task = source->_pcfg_node->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( );
            _source_clauses = get_task_dependency_clauses( task );
        }
        if( target->_pcfg_node->is_omp_task_node( ) )
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
        : _pcfg( pcfg ), _tdg_nodes( ), _syms( )
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
    
    static TDGEdgeType get_tdg_edge_type_from_pcfg_edge_type( std::string pcfg_edge_type )
    {
        TDGEdgeType result;
        if( pcfg_edge_type == "strict" )
            result = Strict;
        else if( pcfg_edge_type == "static" )
            result = Static;
        else if( pcfg_edge_type == "maybe" )
            result = Maybe;
        else if( pcfg_edge_type == "post" )
            result = Post;
        else
        {
            internal_error( "Unexpected type of synchronization edge '%s' from PCFG. "
                            "Expected strict|static|maybe|post.", pcfg_edge_type.c_str( ) );
        }
        return result;
    }
    
    void TaskDependencyGraph::connect_tdg_nodes( TDG_Node* parent, TDG_Node* child, 
                                                 std::string type, const Nodecl::NodeclBase& condition )
    {    
        TDG_Edge* edge = new TDG_Edge( parent, child, get_tdg_edge_type_from_pcfg_edge_type( type ), condition );
        parent->_exits.insert( edge );
        child->_entries.insert( edge );
    }
    
    TDG_Node* TaskDependencyGraph::find_task_from_tdg_nodes_list( Node* task )
    {
        TDG_Node* result = NULL;
        for( TDG_Node_list::iterator it = _tdg_nodes.begin( ); it != _tdg_nodes.end( ); ++it )
            if( ( *it )->_pcfg_node == task )
            {
                result = *it;
                break;
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
            
            // Call recursively with inner nodes if applies
            if( current->is_graph_node( ) )
                create_tdg_nodes_from_pcfg( current->get_graph_entry_node( ) );
            
            // Create the TDG task node
            TDG_Node* tdg_current = NULL;
            if( current->is_omp_task_node( ) )
                tdg_current = new TDG_Node( current, Task );
            else if( current->is_omp_taskwait_node( ) )
                tdg_current = new TDG_Node( current, Taskwait );
            else if( current->is_omp_barrier_node( ) )
                tdg_current = new TDG_Node( current, Barrier );
            
            if( tdg_current != NULL )
                _tdg_nodes.insert( tdg_current );
            
            // Iterate over the children
            Node_list children = current->get_children( );
            for( Node_list::iterator it = children.begin( ); it != children.end( ); ++it )
                create_tdg_nodes_from_pcfg( *it );
        }
    }
    
    void TaskDependencyGraph::store_condition_list_of_symbols( const Nodecl::NodeclBase& condition )
    {
        ObjectList<Nodecl::Symbol> cond_syms = Nodecl::Utils::get_all_symbols_first_occurrence( condition );
        for( ObjectList<Nodecl::Symbol>::iterator it = cond_syms.begin( ); it != cond_syms.end( ); ++it )
            _syms.insert( *it );
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
                            TDG_Node* tdg_child_task = find_task_from_tdg_nodes_list( child );
                            connect_tdg_nodes( tdg_task, tdg_child_task, ( *it )->get_label( ), ( *it )->get_condition( ) );
                            store_condition_list_of_symbols( ( *it )->get_condition( ) );
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
                        connect_tdg_nodes( tdg_sync, tdg_child_task, ( *it )->get_label( ), ( *it )->get_condition( ) );
                        store_condition_list_of_symbols( ( *it )->get_condition( ) );
                    }
                }
            }
            
            // Iterate over the children
            Node_list children = current->get_children( );
            for( Node_list::iterator it = children.begin( ); it != children.end( ); ++it )
                connect_tdg_nodes_from_pcfg( *it );
        }
    }
    
//     static std::string prettyprint_clauses( Nodecl_list clauses )
//     {
//         std::string result;
//         
//         for( Nodecl_list::iterator it = clauses.begin( ); it != clauses.end( ); )
//         {
//             // Note: there is no codegen for OpenMP nodecls, 
//             // that is why we print it manually instead of calling prettyprint
//             std::string clause_name;
//             std::string clause_args;
//             Nodecl::List args;
//             if( it->is<Nodecl::OpenMP::DepIn>( ) )
//             {
//                 clause_name = "in";
//                 args =  it->as<Nodecl::OpenMP::DepIn>( ).get_in_deps( ).as<Nodecl::List>( );
//             }
//             else if( it->is<Nodecl::OpenMP::DepOut>( ) )
//             {
//                 clause_name = "out";
//                 args =  it->as<Nodecl::OpenMP::DepOut>( ).get_out_deps( ).as<Nodecl::List>( );
//             }
//             else if( it->is<Nodecl::OpenMP::DepInout>( ) )
//             {
//                 clause_name = "inout";
//                 args =  it->as<Nodecl::OpenMP::DepInout>( ).get_inout_deps( ).as<Nodecl::List>( );
//             }
//             
//             for( Nodecl::List::iterator it_a = args.begin( ); it_a != args.end( ); )
//             {
//                 clause_args += it_a->prettyprint( );
//                 ++it_a;
//                 if( it_a != args.end( ) )
//                     clause_args += ", ";
//             }
//             result += clause_name + "(" + clause_args + ")";
//             
//             ++it;
//             if( it != clauses.end( ) )
//                 result += ", ";
//         }
//         
//         return result;
//     }
    
    void TaskDependencyGraph::print_tdg_node_to_dot( TDG_Node* current, std::ofstream& dot_tdg )
    {
        std::stringstream ss; ss << current->_id;
        std::string current_id = ss.str( );
        
        // Create the node
        std::string task_label = "";
        TDGNodeType ntype = current->_type;
        if( ntype == Task )
        {
            // Get the name of the task
            Nodecl::OpenMP::Task task = current->_pcfg_node->get_graph_related_ast( ).as<Nodecl::OpenMP::Task>( );
            task_label = "Task :: " + task.get_locus_str( );
            Nodecl::List environ = task.get_environment( ).as<Nodecl::List>( );
            for( Nodecl::List::iterator it = environ.begin( ); it != environ.end( ); ++it )
                if( it->is<Nodecl::OpenMP::TaskLabel>( ) )
                {
                    task_label = "_" + it->prettyprint( );
                    break;
                }
        }
        else if( ntype == Taskwait )
        {
            Nodecl::NodeclBase tw_stmt = current->_pcfg_node->get_statements( )[0];
            task_label = "Taskwait :: " + tw_stmt.get_locus_str( );
        }
        else if( ntype == Barrier )
        {
            Nodecl::NodeclBase barrier_stmt = current->_pcfg_node->get_statements( )[0];
            task_label = "Barrier :: " + barrier_stmt.get_locus_str( );
        }
        dot_tdg << "\t" + current_id + " [label=\"" + task_label + "\"];\n";
        
        // Create the connections from the current node to its children
        std::string headlabel, taillabel, style, condition;
        for( TDG_Edge_list::iterator it = current->_exits.begin( ); it != current->_exits.end( ); ++it )
        {
            // Get the edge info in a string
//             headlabel = "headlabel=\"" + prettyprint_clauses( ( *it )->_target_clauses ) + "\"";
//             taillabel = "taillabel=\"" + prettyprint_clauses( ( *it )->_source_clauses ) + "\"";
            TDGEdgeType etype = ( *it )->_type;
            style = "style=\"" + std::string( ( etype == Strict || etype == Static ) ? "solid" : "dashed" ) + "\"";
            if( !( *it )->_condition.is_null( ) )
                condition = ", label=\"" + ( *it )->_condition.prettyprint( ) + "\"";
            else
                condition = "true";
            // Create the dot edge
            std::stringstream child_id; child_id << ( *it )->_target->_id;
            dot_tdg << "\t" << current_id << " -> " << child_id.str( ) 
                    << "[" << style << condition /*<< headlabel << ", " << taillabel*/ << "];\n";
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
                internal_error ( "An error occurred while creating the dot directory in '%s'", 
                                 directory_name.c_str( ) );
        }
        
        // Create the file where we will store the DOT TDG
        std::string dot_file_name = directory_name + _pcfg->get_name( ) + "_tdg.dot";
        std::ofstream dot_tdg;
        dot_tdg.open( dot_file_name.c_str( ) );
        if( !dot_tdg.good( ) )
            internal_error ("Unable to open the file '%s' to store the TDG.", dot_file_name.c_str( ) );
        
        // Create the DOT graphs
        if( VERBOSE )
            std::cerr << "- TDG DOT file '" << dot_file_name << "'" << std::endl;
        dot_tdg << "digraph TDG {\n";
            dot_tdg << "\tcompound=true;\n";
            for( TDG_Node_list::iterator it = _tdg_nodes.begin( ); it != _tdg_nodes.end( ); ++it )
                print_tdg_node_to_dot( *it, dot_tdg );
        dot_tdg << "}\n";
        dot_tdg.close( );
        if( !dot_tdg.good( ) )
            internal_error ("Unable to close the file '%s' where PCFG has been stored.", dot_file_name.c_str( ) );
        ExtensibleGraph::clear_visits( _pcfg->get_graph( ) );
    }
    
    void TaskDependencyGraph::print_tdg_syms_to_json( std::ofstream& json_tdg )
    {
        if( !_syms.empty( ) )
        {
            json_tdg << "\t\t\"defvars\" : [\n" ;
            int i = 1;
            for( std::set<Nodecl::Symbol>::iterator it = _syms.begin( ); it != _syms.end( ); ++i )
            {
                TL::Symbol s = it->get_symbol( );
                json_tdg << "\t\t\t{\n";
                    json_tdg << "\t\t\t\t\"id\" : " << i << ",\n";
                    json_tdg << "\t\t\t\t\"name\" : \"" << s.get_name( ) << "\",\n";
                    json_tdg << "\t\t\t\t\"locus\" : \"" << s.get_locus_str( ) << "\",\n";
                    json_tdg << "\t\t\t\t\"type\" : \"" << s.get_type( ).get_declaration( s.get_scope( ), 
                                    /*no name for the symbol, so we print only the type name*/"" ) << "\"\n";
                ++it;
                if( it != _syms.end( ) )
                    json_tdg << "\t\t\t},\n";
                else
                    json_tdg << "\t\t\t}\n";
            }
            json_tdg << "\t\t],\n" ;
        }
    }
    
    // This method returns a string corresponding to the prettyprinted version of a nodecl
    // where each symbol occurrence is replaced by a $id
    // Example:
    //     The expression :         'i == j'
    //     Will return the string:  '$1 == $2'
    static std::string transform_condition_into_json_expr( const Nodecl::NodeclBase& condition )
    {
        std::string result = condition.prettyprint( );
        
        // Get the name of each symbol and 
        // store a map that represents the position of the last replacement
        ObjectList<std::string> sym_names;
        std::map<std::string, int> symbol_position_map;
        ObjectList<Nodecl::Symbol> syms = Nodecl::Utils::get_all_symbols_occurrences( condition );
        for( ObjectList<Nodecl::Symbol>::iterator it = syms.begin( ); it != syms.end( ); ++it )
        {
            std::string s_name = it->get_symbol( ).get_name( );
            sym_names.append( s_name );
            symbol_position_map[s_name] = 0;
        }
        
        // Transform the condition expression into the json expression
        int i = 1;
        for( ObjectList<std::string>::iterator it = sym_names.begin( ); it != sym_names.end( ); ++it, ++i )
        {
            // Find the position to be replaced
            int pos = result.find( *it, symbol_position_map[*it] );
            // Replace it
            std::stringstream id_str; id_str << "$" << i;
            result.replace( pos, it->size( ), id_str.str( ) );
            // Modify the base position
            symbol_position_map[*it] = pos + (id_str.str( ).size( ) - 1);
        }
        
        return result;
    }
    
    void TaskDependencyGraph::print_tdg_nodes_to_json( std::ofstream& json_tdg )
    {
        json_tdg << "\t\t\"nodes\" : [\n";
        for( TDG_Node_list::iterator it = _tdg_nodes.begin( ); it != _tdg_nodes.end( ); )
        {
            TDG_Node* n = ( *it );
            json_tdg << "\t\t\t{\n";
            // node identifier
                json_tdg << "\t\t\t\t\"id\" : " << n->_id << ",\n";
            // node locus and type
            if( n->_type == Task ) 
            {
                json_tdg << "\t\t\t\t\"locus\" : \"" << n->_pcfg_node->get_graph_related_ast( ).get_locus_str( ) << "\",\n";
                json_tdg << "\t\t\t\t\"type\" : \"Task\"";
            }
            else
            {
                json_tdg << "\t\t\t\t\"locus\" : \"" << n->_pcfg_node->get_statements( )[0].get_locus_str( ) << "\",\n";
                if( n->_type == Taskwait )
                    json_tdg << "\t\t\t\t\"type\" : \"Taskwait\"";
                else
                    json_tdg << "\t\t\t\t\"type\" : \"Barrier\"";
            }
            // node exit edges
            if( !n->_exits.empty( ) )
            {
                json_tdg << ",\n";
                json_tdg << "\t\t\t\t\"edges\" : [\n";
                for( ObjectList<TDG_Edge*>::iterator ite = n->_exits.begin( ); ite != n->_exits.end( ); )
                {
                    json_tdg << "\t\t\t\t\t{\n";
                    // The target node
                    json_tdg << "\t\t\t\t\t\t\"node\" : " << ( *ite )->_target->_id << ",\n";
                    // The condition
                    json_tdg << "\t\t\t\t\t\t\"when\" : {\n";
                    json_tdg << "\t\t\t\t\t\t\t\"expression\" : ";
                    if( !(*ite)->_condition.is_null() )
                    {   // Generate the condition
                        ObjectList<Nodecl::Symbol> syms = Nodecl::Utils::get_all_symbols_occurrences( (*ite)->_condition );
                        json_tdg << "\"" << transform_condition_into_json_expr( (*ite)->_condition ) << "\",\n";
                        // Generate the list of involved variables
                        if( syms.size( ) > 1 )
                            json_tdg << "\t\t\t\t\t\t\t\"vars\" : [\n";
                        else
                            json_tdg << "\t\t\t\t\t\t\t\"vars\" : \n";
                        int i = 1;
                        for( ObjectList<Nodecl::Symbol>::iterator its = syms.begin( ); its != syms.end( ); ++i)
                        {
                            json_tdg << "\t\t\t\t\t\t\t\t{\n";
                            json_tdg << "\t\t\t\t\t\t\t\t\t\"id\" : " << i << ",\n";
                            json_tdg << "\t\t\t\t\t\t\t\t\t\"values\" : \"TODO\"\n";
                            
                            // TODO: values!
                            
                            ++its;
                            if( its != syms.end( ) )
                                json_tdg << "\t\t\t\t\t\t\t\t},\n";
                            else
                                json_tdg << "\t\t\t\t\t\t\t\t}\n";
                        }
                        if( syms.size( ) > 1 )
                            json_tdg << "\t\t\t\t\t\t\t]\n";
                    }
                    else    // There is no condition => TRUE
                        json_tdg << "true\n";
                    json_tdg << "\t\t\t\t\t\t}\n";
                        
                    ++ite;
                    if( ite != n->_exits.end( ) )
                        json_tdg << "\t\t\t\t\t},\n";
                    else
                        json_tdg << "\t\t\t\t\t}\n";
                }
                json_tdg << "\t\t\t\t]\n";
            }
            else
                json_tdg << "\n";
            
            ++it;
            if( it != _tdg_nodes.end( ) )
                json_tdg << "\t\t\t},\n";
            else
                json_tdg << "\t\t\t}\n";
        }
        json_tdg << "\t\t]\n";
    }
    
    void TaskDependencyGraph::print_tdg_to_json( )
    {
        // Create the directory of json files if it has not been previously created
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if( err == NULL )
            internal_error ( "An error occurred while getting the path of the current directory", 0 );
        struct stat st;
        std::string directory_name = std::string( buffer ) + "/json/";
        if( stat( directory_name.c_str( ), &st ) != 0 )
        {
            int json_directory = mkdir( directory_name.c_str( ), S_IRWXU );
            if( json_directory != 0 )
                internal_error ( "An error occurred while creating the json directory in '%s'", 
                                 directory_name.c_str( ) );
        }
        
        // Create the file where we will store the JSON TDG
        std::string json_file_name = directory_name + _pcfg->get_name( ) + "_tdg.json";
        std::ofstream json_tdg;
        json_tdg.open( json_file_name.c_str( ) );
        if( !json_tdg.good( ) )
            internal_error ("Unable to open the file '%s' to store the TDG.", json_file_name.c_str( ) );
        
        // Create the JSON graphs
        if( VERBOSE )
            std::cerr << "- TDG JSON file '" << json_file_name << "'" << std::endl;
        json_tdg << "{\n";
            json_tdg << "\t\"tdg\" : {\n";
                json_tdg << "\t\t\"function\" : \"" << _pcfg->get_name( ) << "\",\n";
                json_tdg << "\t\t\"locus\" : \"" << _pcfg->get_nodecl( ).get_locus_str( ) << "\",\n";
                print_tdg_syms_to_json( json_tdg );
                print_tdg_nodes_to_json( json_tdg );
            json_tdg << "\t}\n";
        json_tdg << "}\n";
        json_tdg.close( );
        if( !json_tdg.good( ) )
            internal_error ("Unable to close the file '%s' where PCFG has been stored.", json_file_name.c_str( ) );
        ExtensibleGraph::clear_visits( _pcfg->get_graph( ) );
    }
}
}