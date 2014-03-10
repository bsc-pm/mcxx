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



#include <fstream>
#include <sstream>
#include <sys/stat.h>
#include <unistd.h>

#include <time.h>

#include "filename.h"

#include "cxx-codegen.h"
#include "tl-analysis-utils.hpp"
#include "tl-extensible-graph.hpp"

namespace TL {
namespace Analysis {

    static int _subgraph_id = 0;
    
    static bool _usage;
    static bool _liveness;
    static bool _reaching_defs;
    static bool _induction_vars;
    static bool _ranges;
    static bool _auto_scoping;
    static bool _auto_deps;
    
namespace {
    
    std::string print_node_usage( Node* current )
    {
        std::string usage = "";
        if( _usage )
        {
            std::string ue = prettyprint_ext_sym_set( current->get_ue_vars( ), /*dot*/ true );
            std::string killed = prettyprint_ext_sym_set( current->get_killed_vars( ), /*dot*/ true );
            std::string undef = prettyprint_ext_sym_set( current->get_undefined_behaviour_vars( ), /*dot*/ true );
            std::string assert_ue = prettyprint_ext_sym_set( current->get_assert_ue_vars( ), /*dot*/ true );
            std::string assert_killed = prettyprint_ext_sym_set( current->get_assert_killed_vars( ), /*dot*/ true );
            
            usage = ( killed.empty( )          ? "" : ( "KILL: "          + killed        + "\\n" ) )
                    + ( ue.empty( )            ? "" : ( "UE: "            + ue            + "\\n" ) )
                    + ( undef.empty( )         ? "" : ( "UNDEF: "         + undef         + "\\n" ) )
                    + ( assert_ue.empty( )     ? "" : ( "ASSERT_UE: "     + assert_ue     + "\\n" ) ) 
                    + ( assert_killed.empty( ) ? "" : ( "ASSERT_KILLED: " + assert_killed ) );
            
            int u_size = usage.size( );
            if( ( u_size > 3 ) && ( usage.substr( u_size - 2, u_size - 1 ) == "\\n" ) )
                usage = usage.substr( 0, u_size - 2 );
        }
        return usage;
    }

    std::string print_node_liveness( Node* current )
    {
        std::string liveness = "";
        if( _liveness )
        {
            std::string live_in = prettyprint_ext_sym_set( current->get_live_in_vars( ), /*dot*/ true );
            std::string live_out = prettyprint_ext_sym_set( current->get_live_out_vars( ), /*dot*/ true );
            std::string assert_live_in = prettyprint_ext_sym_set( current->get_assert_live_in_vars( ), /*dot*/ true );
            std::string assert_live_out = prettyprint_ext_sym_set( current->get_assert_live_out_vars( ), /*dot*/ true );
            std::string assert_dead = prettyprint_ext_sym_set( current->get_assert_dead_vars( ), /*dot*/ true );
        
            liveness = ( live_in.empty( )         ? "" : "LI: "          + live_in         + "\\n" )
                     + ( live_out.empty( )        ? "" : "LO: "          + live_out        + "\\n" ) 
                     + ( assert_live_in.empty( )  ? "" : "ASSERT_LI: "   + assert_live_in  + "\\n" )
                     + ( assert_live_out.empty( ) ? "" : "ASSERT_LO: "   + assert_live_out + "\\n")
                     + ( assert_dead.empty( )     ? "" : "ASSERT_DEAD: " + assert_dead );
            
            int l_size = liveness.size( );
            if( ( l_size > 3 ) && ( liveness.substr( l_size - 2, l_size - 1 ) == "\\n" ) )
                liveness = liveness.substr( 0, l_size - 2 );
        }
        
        return liveness;
    }
    
    std::string print_node_reaching_defs( Node* current )
    {
        std::string reaching_defs = "";
        if( _reaching_defs )
        {
            std::string gen = prettyprint_ext_sym_map( current->get_generated_stmts( ), /*dot*/ true );
            std::string defs_in = prettyprint_ext_sym_map( current->get_reaching_definitions_in( ), /*dot*/ true );
            std::string defs_out = prettyprint_ext_sym_map( current->get_reaching_definitions_out( ), /*dot*/ true );
            std::string assert_defs_in = prettyprint_ext_sym_map( current->get_assert_reaching_definitions_in( ), /*dot*/ true );
            std::string assert_defs_out = prettyprint_ext_sym_map( current->get_assert_reaching_definitions_out( ), /*dot*/ true );
        
            reaching_defs = ( gen.empty( )               ? "" : "GEN: "        + gen             + "\\n" )
                            + ( defs_in.empty( )         ? "" : "RDI: "        + defs_in         + "\\n" )
                            + ( defs_out.empty( )        ? "" : "RDO: "        + defs_out        + "\\n" )
                            + ( assert_defs_in.empty( )  ? "" : "ASSERT_RDI: " + assert_defs_in  + "\\n" )
                            + ( assert_defs_out.empty( ) ? "" : "ASSERT_RDO: " + assert_defs_out );
        
            int l_size = reaching_defs.size( );
            if( ( l_size > 3 ) && ( reaching_defs.substr( l_size - 2, l_size - 1 ) == "\\n" ) )
                reaching_defs = reaching_defs.substr( 0, l_size - 2 );
        }
        return reaching_defs;
    }
    
    std::string print_node_induction_variables( Node* current )
    {
        std::string induction_vars = "";
        if( _induction_vars && ( current->is_loop_node( ) || current->is_omp_loop_node( ) ) )
        {
            ObjectList<Utils::InductionVariableData*> ivs = current->get_induction_variables( );
            for( ObjectList<Utils::InductionVariableData*>::iterator it = ivs.begin( ); it != ivs.end( ); ++it )
            {
                Utils::InductionVariableData* iv = *it;
                Nodecl::NodeclBase lb = iv->get_lb( );
                Nodecl::NodeclBase ub = iv->get_ub( );
                Nodecl::NodeclBase incr = iv->get_increment( );
                ObjectList<Nodecl::NodeclBase> incr_list = iv->get_increment_list( );
                std::string list_of_incrs = "";
                for( ObjectList<Nodecl::NodeclBase>::iterator it2 = incr_list.begin( ); it2 != incr_list.end( ); ++it2 )
                {
                    list_of_incrs += it2->prettyprint( ) + ", ";
                }
                int l_incrs_size = list_of_incrs.size( );
                if( !list_of_incrs.empty( ) )
                    list_of_incrs = list_of_incrs.substr( 0, l_incrs_size - 2 );
                induction_vars += iv->get_variable( ).get_nodecl( ).prettyprint( ) 
                                  + " [ " + ( lb.is_null( )   ? "NULL" : lb.prettyprint( ) )
                                  + ":"   + ( ub.is_null( )   ? "NULL" : ub.prettyprint( ) )
                                  + ":"   + ( incr.is_null( ) ? "NULL" : incr.prettyprint( ) )
                                  + ( ( incr_list.size( ) > 1 ) ? ( "( " + list_of_incrs + " )" ) : "" )
                                  + ":"   + iv->get_type_as_string( ) + " ]\\n";
            }
        }
        int l_size = induction_vars.size( );
        if( ( l_size > 3 ) && ( induction_vars.substr( l_size - 2, l_size - 1 ) == "\\n" ) )
            induction_vars = induction_vars.substr( 0, l_size - 2 );
        return induction_vars;
    }
    
    std::string print_node_ranges( Node* current )
    {
        std::string ranges = "";
        if( _ranges )
        {
            Utils::RangeValuesMap ranges_in_ = current->get_ranges_in( );
            Utils::RangeValuesMap ranges_out_ = current->get_ranges_out( );
            if( current->get_id( ) == 9 )
            {
                std::cerr << "Node 9 has " << ranges_in_.size( ) << " ranges_in and " << ranges_out_.size( ) << " ranges_out" << std::endl;
            }
            std::string ranges_in = prettyprint_range_values_map( current->get_ranges_in( ), /*dot*/ true );
            std::string ranges_out = prettyprint_range_values_map( current->get_ranges_out( ), /*dot*/ true );
            
            ranges = ( ranges_in.empty( )    ? "" : "RI: " + ranges_in  + "\\n" )
                     + ( ranges_out.empty( ) ? "" : "RO: " + ranges_out );
            
            int l_size = ranges.size( );
            if( ( l_size > 3 ) && ( ranges.substr( l_size - 2, l_size - 1 ) == "\\n" ) )
                ranges = ranges.substr( 0, l_size - 2 );
        }
        return ranges;
    }
    
    std::string print_node_data_sharing( Node* current )
    {
        std::string auto_scope = "";
        if( _auto_scoping )
        {
            std::string sc_private = prettyprint_ext_sym_set( current->get_sc_private_vars( ), /*dot*/ true );
            std::string sc_firstprivate = prettyprint_ext_sym_set( current->get_sc_firstprivate_vars( ), /*dot*/ true );
            std::string sc_shared = prettyprint_ext_sym_set( current->get_sc_shared_vars( ), /*dot*/ true );
            std::string sc_undefined = prettyprint_ext_sym_set( current->get_sc_undef_vars( ), /*dot*/ true );
            auto_scope = ( sc_private.empty( )        ? "" : "AUTO-SC_PRIVATE: "      + sc_private      + "\\n" )
                         + ( sc_firstprivate.empty( ) ? "" : "AUTO-SC_FIRSTPRIVATE: " + sc_firstprivate + "\\n" )
                         + ( sc_shared.empty( )       ? "" : "AUTO-SC_SHARED: "       + sc_shared       + "\\n" )
                         + ( sc_undefined.empty( )    ? "" : "AUTO-SC_UNDEFINED: "    + sc_undefined );
        }
        return auto_scope;
    }

    std::string print_node_deps( Node* current )
    {
        std::string auto_deps = "";
        if( _auto_deps )
        {
            std::string deps_private = prettyprint_ext_sym_set( current->get_deps_private_vars( ), /*dot*/ true );
            std::string deps_firstprivate = prettyprint_ext_sym_set( current->get_deps_firstprivate_vars( ), /*dot*/ true );
            std::string deps_shared = prettyprint_ext_sym_set( current->get_deps_shared_vars( ), /*dot*/ true );
            std::string deps_in = prettyprint_ext_sym_set( current->get_deps_in_exprs( ), /*dot*/ true );
            std::string deps_out = prettyprint_ext_sym_set( current->get_deps_out_exprs( ), /*dot*/ true );
            std::string deps_inout = prettyprint_ext_sym_set( current->get_deps_inout_exprs( ), /*dot*/ true );
            std::string deps_undefined = prettyprint_ext_sym_set( current->get_deps_undef_vars( ), /*dot*/ true );
            auto_deps = ( deps_private.empty( )        ? "" : "AUTO-DEPS_PRIVATE: "      + deps_private      + "\\n" )
                        + ( deps_firstprivate.empty( ) ? "" : "AUTO-DEPS_FIRSTPRIVATE: " + deps_firstprivate + "\\n" )
                        + ( deps_shared.empty( )       ? "" : "AUTO-DEPS_SHARED: "       + deps_shared       + "\\n" )
                        + ( deps_in.empty( )           ? "" : "AUTO-DEPS_IN: "           + deps_in           + "\\n" )
                        + ( deps_out.empty( )          ? "" : "AUTO-DEPS_OUT: "          + deps_out          + "\\n" )
                        + ( deps_inout.empty( )        ? "" : "AUTO-DEPS_INOUT: "        + deps_inout        + "\\n" )
                        + ( deps_undefined.empty( )    ? "" : "AUTO-DEPS_UNDEFINED: "    + deps_undefined );
        }
        return auto_deps;
    }
}
    
    void ExtensibleGraph::print_graph_to_dot( bool usage, bool liveness, bool reaching_defs, bool induction_vars,
                                              bool ranges, bool auto_scoping, bool auto_deps )
    {
        std::ofstream dot_pcfg;
        
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
        
        std::string dot_file_name = directory_name + _name + "_pcfg.dot";
        dot_pcfg.open( dot_file_name.c_str( ) );
        if( !dot_pcfg.good( ) )
            internal_error ("Unable to open the file '%s' to store the PCFG.", dot_file_name.c_str( ) );
            
        // Create the dot graphs
        if( VERBOSE )
            std::cerr << "- PCFG File '" << dot_file_name << "'" << std::endl;

        _usage = usage;
        _liveness = liveness;
        _reaching_defs = reaching_defs;
        _induction_vars = induction_vars;
        _ranges = ranges;
        _auto_scoping = auto_scoping;
        _auto_deps = auto_deps;
        
        dot_pcfg << "digraph CFG {\n";
            dot_pcfg << "\tcompound=true;\n";
            std::string dot_graph = "";
            std::string graph_analysis_info = "";
            std::vector<std::string> current_outer_edges;
            std::vector< std::vector<std::string> > outer_edges( 1, current_outer_edges );
            std::vector<Node*> current_outer_nodes;
            std::vector<std::vector<Node*> > outer_nodes( 1, current_outer_nodes );
            get_nodes_dot_data( _graph, dot_graph, graph_analysis_info, outer_edges, outer_nodes, /*indent*/ "\t" );
            dot_pcfg << dot_graph;
            dot_pcfg << graph_analysis_info;
        dot_pcfg << "}\n";

        ExtensibleGraph::clear_visits( _graph );
        dot_pcfg.close( );
        if( !dot_pcfg.good( ) )
            internal_error ("Unable to close the file '%s' where PCFG has been stored.", dot_file_name.c_str( ) );
    }

    // Preorder traversal
    void ExtensibleGraph::get_nodes_dot_data( Node* current, std::string& dot_graph, std::string& dot_analysis_info,
                                              std::vector<std::vector<std::string> >& outer_edges, 
                                              std::vector<std::vector<Node*> >& outer_nodes, std::string indent )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );
            
            // Generate the node
            if( current->is_graph_node( ) )
            {
                // Calculate the name of the new dot subgraph
                std::stringstream node_id; node_id << current->get_id( );
                std::string subgraph_label = "[" + node_id.str( ) + "] " + current->get_graph_type_as_string( );
                std::stringstream ssgid; ssgid << _subgraph_id;
                std::string cluster_name = "cluster" + ssgid.str( );
                dot_graph += indent + "subgraph " + cluster_name + "{\n";
                Utils::makeup_dot_block( subgraph_label );
                dot_graph += indent + "\tlabel=\"" + subgraph_label + "\";\n";
                _subgraph_id++;

                // Print inner nodes of the graph
                outer_edges.push_back( std::vector<std::string>() );
                outer_nodes.push_back( std::vector<Node*>() );
                get_dot_subgraph( current, dot_graph, dot_analysis_info, outer_edges, outer_nodes, indent + "\t" );
                dot_graph += indent + "}\n";
                
                // Print additional information attached to the node
                dot_graph += print_pragma_node_clauses( current, indent, cluster_name );
                print_node_analysis_info( current, dot_analysis_info, cluster_name );
            }
            else
            {
                get_node_dot_data( current, dot_graph, dot_analysis_info, indent );
            }
            
            // Connect the current node and the possible inner nodes (when current is a graph) 
            // with the nodes in the current nesting level
            bool connect_current = true;
            std::stringstream ss_source_id;
            if( current->is_graph_node( ) ) {
                // It may happen that the exit of a graph node has no entry edges 
                // when there is a break point inside the graph that avoid reaching the exit of the graph.
                // In these cases, we do not need to print this edge because it will never occur
                Node* exit = current->get_graph_exit_node( );
                if( exit->get_entry_edges( ).empty( ) )
                    connect_current = false;
                
                ss_source_id << exit->get_id( );
            } else {
                ss_source_id << current->get_id( );
            }
                
            // Connect current children
            if( connect_current )
            {
                ObjectList<Node*> children = current->get_children( );
                for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
                {
                    std::stringstream ss_target_id;
                    if( ( *it )->is_graph_node( ) )
                        ss_target_id << ( *it )->get_graph_entry_node( )->get_id( );
                    else
                        ss_target_id << ( *it )->get_id( );
                    
                    std::string direction = "";
                    if( ss_source_id.str( ) == ss_target_id.str( ) )
                        direction = ", headport=n, tailport=s";
                    
                    std::string extra_edge_attrs = "";
                    Edge* current_edge = ExtensibleGraph::get_edge_between_nodes( current, *it );
                    if( current_edge->is_task_edge( ) )
                        extra_edge_attrs = ", style=dashed";
                    
                    std::string edge = ss_source_id.str( ) + " -> " + ss_target_id.str( )
                                        + " [label=\"" + current_edge->get_label_as_string( ) 
                                        + "\"" + direction + extra_edge_attrs + "];\n";
                    Node* source_outer = current->get_outer_node( );
                    Node* target_outer = ( *it )->get_outer_node( );
                    if( source_outer == target_outer )
                    {   // The edge has to be printed now
                        dot_graph += indent + edge;
                        get_nodes_dot_data( *it, dot_graph, dot_analysis_info, outer_edges, outer_nodes, indent );
                    }
                    else
                    {
                        int nest = outer_edges.size( );
                        while( source_outer != target_outer && source_outer != NULL )
                        {
                            source_outer = source_outer->get_outer_node( );
                            nest--;
                        }
                        ERROR_CONDITION( nest < 0, "Nested outer edges are not properly managed when generating the PCFG dot", 0 );
                        outer_edges[nest-1].push_back( edge );
                        outer_nodes[nest-1].push_back( *it );
                    }
                }
            }
            
            // Connect all edges and nodes corresponding to the current level of nesting
            if( current->is_graph_node( ) && !outer_edges.empty( ) )
            {
                // Printing the nodes
                std::vector<Node*> current_outer_nodes = outer_nodes[outer_nodes.size( )-1];
                for( std::vector<Node*>::iterator it = current_outer_nodes.begin( ); it != current_outer_nodes.end( ); ++it )
                    get_nodes_dot_data( *it, dot_graph, dot_analysis_info, outer_edges, outer_nodes, indent );
                // Calling recursively to get_nodes_dot_data can add new nodes to the same nesting level, 
                // so we have to iterate over again
                unsigned int last_nodes_size = current_outer_nodes.size( );
                while( current_outer_nodes.size( ) < outer_nodes[outer_nodes.size( )-1].size( ) )
                {
                    current_outer_nodes = outer_nodes[outer_nodes.size( )-1];
                    for( unsigned int i = last_nodes_size; i < current_outer_nodes.size( ); ++i )
                        get_nodes_dot_data( current_outer_nodes[i], dot_graph, dot_analysis_info, outer_edges, outer_nodes, indent );
                    last_nodes_size = current_outer_nodes.size( );
                }
                outer_nodes.pop_back( );
                
                // Printing the edges
                std::vector<std::string> current_outer_edges = outer_edges[outer_edges.size( )-1];
                for( std::vector<std::string>::iterator it = current_outer_edges.begin( ); it != current_outer_edges.end( ); ++it )
                    dot_graph += indent + ( *it );
                outer_edges.pop_back( );
            }
        }
    }

    void ExtensibleGraph::get_dot_subgraph( Node* current, std::string& dot_graph, std::string& graph_analysis_info,
                                            std::vector<std::vector< std::string> >& outer_edges, 
                                            std::vector<std::vector<Node*> >& outer_nodes, std::string indent )
    {
        switch( current->get_graph_type( ) )
        {
            case __Context:
                dot_graph += indent + "color=lightsteelblue;\n";
                break;
            case __ExtensibleGraph:
                dot_graph += indent + "color=white;\n";
                break;
            case __AsmDef:
            case __CondExpr:
            case __FunctionCallGraph:
            case __IfElse:
            case __LoopDoWhile:
            case __LoopFor:
            case __LoopWhile:
            case __SplitStmt:
            case __Switch:
            case __SwitchCase:
                dot_graph += indent + "color=grey45;\n";
                break;
            case __OmpAtomic:
            case __OmpBarrierGraph:
            case __OmpCritical:
            case __OmpLoop:
            case __OmpMaster:
            case __OmpParallel:
            case __OmpSection:
            case __OmpSections:
            case __OmpSimd:
            case __OmpSimdFor:
            case __OmpSimdParallelFor:
            case __OmpSimdFunction:
            case __OmpSingle:
            case __OmpTask:
            case __OmpWorkshare:
                dot_graph += indent + "color=red4;\n" + indent +"style=bold;\n";
                break;
            case __VectorCondExpr:
            case __VectorFunctionCallGraph:
                dot_graph += indent + "color=limegreen;\n";
            default:
                internal_error( "Unexpected node %d type while printing dot\n", current->get_graph_type() );
        };
        Node* entry_node = current->get_graph_entry_node( );
        _cluster_to_entry_map[current->get_id( )] = entry_node->get_id( );
        get_nodes_dot_data( entry_node, dot_graph, graph_analysis_info, outer_edges, outer_nodes, indent );
    }

    void ExtensibleGraph::get_node_dot_data( Node* current, std::string& dot_graph, std::string& graph_analysis_info, std::string indent )
    {
        std::stringstream ss; ss << current->get_id( );
        std::string basic_attrs = "margin=\"0.1,0.1, height=0.1, width=0.1\"";

        switch( current->get_type( ) )
        {
            case __Entry:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] ENTRY\", shape=box, fillcolor=lightgray, style=filled];\n";
                break;
            }
            case __Exit:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] EXIT\", shape=box, fillcolor=lightgray, style=filled];\n";
                break;
            }
            case __UnclassifiedNode:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] UNCLASSIFIED_NODE\"];\n";
                break;
            }
            case __OmpBarrier:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] BARRIER\", shape=diamond];\n";
                break;
            }
            case __OmpFlush:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] FLUSH\", shape=ellipse];\n";
                break;
            }
            case __OmpTaskwait:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] TASKWAIT\", shape=ellipse];\n";
                break;
            }
            case __OmpWaitonDeps:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] WAITON_DEPS\", shape=ellipse];\n";
                break;
            }
            case __OmpVirtualTaskSync:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] POST_SYNC\", shape=ellipse];\n";
                break;
            }
            case __OmpTaskCreation:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] TASK_CREATION\", shape=ellipse];\n";
                break;
            }
            case __Break:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] BREAK\", shape=diamond];\n";
                break;
            }
            case __Continue:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] CONTINUE\", shape=diamond];\n";
                break;
            }
            case __AsmOp:
            case __Goto:
            case __Normal:
            case __Labeled:
            case __FunctionCall:
            {
                // Get the Statements within the BB
                ObjectList<Nodecl::NodeclBase> node_block = current->get_statements( );
                std::string aux_str = "";
                std::string basic_block = "";
                for( ObjectList<Nodecl::NodeclBase>::iterator it = node_block.begin( ); it != node_block.end( ); it++ )
                {
                    if( it->is<Nodecl::ObjectInit>( ) )
                    {
                        Symbol it_s = it->as<Nodecl::ObjectInit>( ).get_symbol( );
                        aux_str = it_s.get_name( ) + " = " + it_s.get_value( ).prettyprint( );
                    }
                    else
                    {
                        aux_str = it->prettyprint( );
                    }

                    Utils::makeup_dot_block(aux_str);
                    basic_block += aux_str + "\\n";
                }
                basic_block = basic_block.substr( 0, basic_block.size( ) - 2 );   // Remove the last back space

                dot_graph += indent + ss.str( ) + "[label=\"{[" + ss.str( ) + "] " + basic_block + "}\", shape=record, "
                           + basic_attrs + "];\n";

                bool induction_vars = _induction_vars;  _induction_vars = false;
                bool auto_scoping = _auto_scoping;      _auto_scoping = false;
                bool auto_deps = _auto_deps;            _auto_deps = false;
                print_node_analysis_info( current, graph_analysis_info, /*cluster name*/ "" );
                _induction_vars = induction_vars;
                _auto_scoping = auto_scoping;
                _auto_deps = auto_deps;
                break;
            }
            default:
            {
                if( current->is_vector_node( ) )
                {   // No codegen for these nodes, we generate a node containing only the type in the DOT file
                    dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] " + current->get_type_as_string( ) + "\", shape=hexagon];\n";
                }
                else
                {
                    internal_error( "Undefined type of node '%s' found while printing the graph.",
                                    current->get_type_as_string( ).c_str( ) );
                }
            }
        };
    }

    static std::string pcfgclause_to_str( PCFGClause clause )
    {
        std::string clauses_str = ""; 
        int i = 0;
        Nodecl::List args = clause.get_args( );
        int n_args = args.size( );
        for( Nodecl::List::const_iterator it = args.begin( ); it != args.end( ); ++it, ++i )
        {
            if( it->is<Nodecl::OpenMP::ReductionItem>( ) )
            {
                Nodecl::OpenMP::ReductionItem red = it->as<Nodecl::OpenMP::ReductionItem>( );
                clauses_str += clause.get_clause_as_string( ) + "(" 
                             + red.get_reductor( ).prettyprint( ) + ":" + red.get_reduced_symbol( ).prettyprint( ) + ")";
            }
            else if( it->is<Nodecl::OpenMP::Final>( ) )
            {
                Nodecl::OpenMP::Final fin = it->as<Nodecl::OpenMP::Final>( );
                clauses_str += "final(" + fin.get_condition( ).prettyprint( ) + ")";
            }
            else if( it->is<Nodecl::OpenMP::Target>( ) )
            {
                Nodecl::OpenMP::Target tar = it->as<Nodecl::OpenMP::Target>( );
                // Get devices info
                Nodecl::List devices = tar.get_devices( ).as<Nodecl::List>( );
                std::string devices_str = "";
                int n_devices = devices.size( );
                int j = 0;
                for( Nodecl::List::iterator it2 = devices.begin( ); it2 != devices.end( ); ++it2, ++j )
                {
                    devices_str += it2->prettyprint( );
                    if( j < n_devices-1 )
                        devices_str += ", ";
                }
                clauses_str += "device(" + devices_str + ") ";
                // Get other target clauses ( copies )
                Nodecl::List copies = tar.get_items( ).as<Nodecl::List>( );
                int n_copies = copies.size( );
                if( n_copies != 0 )
                {
                    clauses_str += "\\n";
                }
                j = 0;
                for( Nodecl::List::iterator it2 = copies.begin( ); it2 != copies.end( ); ++it2 )
                {
                    if( it2->is<Nodecl::OpenMP::CopyIn>( ) )
                    {
                        clauses_str += "copy_in(";
                    }
                    else if( it2->is<Nodecl::OpenMP::CopyOut>( ) )
                    {
                        clauses_str += "copy_out(";
                    }
                    else if( it2->is<Nodecl::OpenMP::CopyInout>( ) )
                    {
                        clauses_str += "copy_inout(";
                    }
                    else if( it2->is<Nodecl::OpenMP::Implements>( ) )
                    {
                        clauses_str += "implements(";
                    }
                    if (it2->children( )[0].is<Nodecl::List>())
                    {
                        Nodecl::List copied_values = it2->children( )[0].as<Nodecl::List>( );
                        int n_copied_values = copied_values.size( );
                        int k = 0;
                        for( Nodecl::List::iterator it3 = copied_values.begin( ); it3 != copied_values.end( ); ++it3, ++k )
                        {
                            clauses_str += it3->prettyprint( );
                            if( k < n_copied_values-1 )
                            {
                                clauses_str += ", ";
                            }
                        }
                    }
                    else
                    {
                        clauses_str += it2->children( )[0].prettyprint( );
                    }
                    clauses_str += ") ";
                    if( j < n_copies-1 )
                    {
                        clauses_str += "\\n";
                    }
                }
            }
            else
            {
                clauses_str += clause.get_clause_as_string( ) + "(" + it->prettyprint( ) + ")";
            }   
            
            if( i < n_args-1 )
                clauses_str += ", ";
        }
        
        return clauses_str;
    }
    
    std::string ExtensibleGraph::print_pragma_node_clauses( Node* current, std::string indent, std::string cluster_name )
    {
        std::string pragma_info_str = "";
        if( current->is_graph_node( ) && current->is_omp_node( ) )
        {
            PCFGPragmaInfo pragma_info = current->get_pragma_node_info( );
            ObjectList<PCFGClause> clauses = pragma_info.get_clauses( );
            int n_clauses = clauses.size( );
            if( n_clauses > 0 )
            {
                std::stringstream node_id; node_id << current->get_id( );
                std::string id = "-0" + node_id.str( );
                std::stringstream entry_node_id; entry_node_id << current->get_graph_entry_node( )->get_id( );
                std::string current_entry_id = entry_node_id.str( );
                int i = 0;
                std::string clauses_str = "";
                for( ObjectList<PCFGClause>::const_iterator it = clauses.begin( ); it != clauses.end( ); ++it, ++i )
                {
                    clauses_str += pcfgclause_to_str( *it );
                    if( i < n_clauses-1 )
                        clauses_str += "\\n ";
                }
                pragma_info_str += indent + id + "[label=\"" + clauses_str + "\", shape=box, color=wheat3];\n";
                pragma_info_str += indent + current_entry_id + " -> " + id + " [style=dashed, color=wheat3, ltail=" + cluster_name + "];\n";
            }
        }
        return pragma_info_str;
    }
    
    void ExtensibleGraph::print_node_analysis_info( Node* current, std::string& dot_analysis_info,
                                                    std::string cluster_name )
    {
        std::stringstream node_id; node_id << current->get_id( );
        std::stringstream ssgeid;
        if( !cluster_name.empty( ) )
            ssgeid << _cluster_to_entry_map[current->get_id( )];
        else
            ssgeid << current->get_id( );
        std::string usage_str = print_node_usage( current );
        std::string liveness_str = print_node_liveness( current );
        std::string reach_defs_str = print_node_reaching_defs( current );
        std::string ranges_str = print_node_ranges( current );
        std::string induction_vars_str = print_node_induction_variables( current );
        std::string color;
        std::string common_attrs = "style=dashed";
        if( !usage_str.empty( ) )
        {
            std::string id = "-00" + node_id.str( );
            color = "blue";
            dot_analysis_info += "\t" + id + "[label=\"" + usage_str + " \", shape=box, color=" + color + "];\n";
            if( !current->is_extended_graph_node( ) )
            {
                dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color;
                if( !cluster_name.empty() )
                    dot_analysis_info += ", ltail=" + cluster_name + "]";
                else
                    dot_analysis_info += "]";
                dot_analysis_info += ";\n";
            }
        }
        if( !liveness_str.empty( ) )
        {
            std::string id = "-000" + node_id.str( );
            color = "green3";
            dot_analysis_info += "\t" + id + "[label=\"" + liveness_str + " \", shape=box, color=" + color + "];\n";
            if( !current->is_extended_graph_node( ) )
            {
                dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color;
                if( !cluster_name.empty() )
                    dot_analysis_info += ", ltail=" + cluster_name + "]";
                else
                    dot_analysis_info += "]";
                dot_analysis_info += ";\n";
            }
        }
        if( !reach_defs_str.empty( ) )
        {
            std::string id = "-0000" + node_id.str( );
            color = "red2";
            dot_analysis_info += "\t" + id + "[label=\"" + reach_defs_str + " \", shape=box, color=" + color + "];\n";
            if( !current->is_extended_graph_node( ) )
            {
                dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color;
                if( !cluster_name.empty() )
                    dot_analysis_info += ", ltail=" + cluster_name + "]";
                else
                    dot_analysis_info += "]";
                dot_analysis_info += ";\n";
            }
        }
        if( !ranges_str.empty( ) )
        {
            std::string id = "-00000" + node_id.str( );
            color = "cyan3";
            dot_analysis_info += "\t" + id + "[label=\"" + ranges_str + " \", shape=box, color=" + color + "];\n";
            if( !current->is_extended_graph_node( ) )
            {
                dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color;
                if( !cluster_name.empty() )
                    dot_analysis_info += ", ltail=" + cluster_name + "]";
                else
                    dot_analysis_info += "]";
                dot_analysis_info += ";\n";
            }
        }
        if( !induction_vars_str.empty( ) )
        {
            std::string id = "-000000" + node_id.str( );
            color = "orange2";
            dot_analysis_info += "\t" + id + "[label=\"" + induction_vars_str + " \", shape=box, color=" + color + "];\n";
            if( !current->is_extended_graph_node( ) )
            {
                dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color;
                if( !cluster_name.empty() )
                    dot_analysis_info += ", ltail=" + cluster_name + "]";
                else
                    dot_analysis_info += "]";
                dot_analysis_info += ";\n";
            }
        }
        if( current->is_omp_task_node( ) )
        {
            std::string auto_scope_str = print_node_data_sharing( current );
            if( !auto_scope_str.empty() )
            {
                std::string id = "-0000000" + node_id.str( );
                color = "darkgoldenrod1";
                dot_analysis_info += "\t" + id + "[label=\"" + auto_scope_str + " \", shape=box, color=" + color + "]\n";
                if( !current->is_extended_graph_node( ) )
                {
                    dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color;
                    if( !cluster_name.empty() )
                        dot_analysis_info += ", ltail=" + cluster_name + "]";
                    else
                        dot_analysis_info += "]";
                    dot_analysis_info += ";\n";
                }
            }

            std::string auto_deps_str = print_node_deps( current );
            if( !auto_deps_str.empty() )
            {
                std::string id = "-00000000" + node_id.str( );
                color = "skyblue3";
                dot_analysis_info += "\t" + id + "[label=\"" + auto_deps_str + " \", shape=box, color=" + color + "]\n";
                if( !current->is_extended_graph_node( ) )
                {
                    dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color;
                    if( !cluster_name.empty() )
                        dot_analysis_info += ", ltail=" + cluster_name + "]";
                    else
                        dot_analysis_info += "]";
                    dot_analysis_info += ";\n";
                }
            }
        }
    }
}
}
