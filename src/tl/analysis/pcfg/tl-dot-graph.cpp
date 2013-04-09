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

    static void makeup_dot_block( std::string& str );
    static std::string prettyprint_ext_sym_set(  Utils::ext_sym_set s );
    static std::string prettyprint_ext_sym_map( Utils::ext_sym_map s );
    static std::string print_node_usage( Node* current, bool analysis_is_performed );
    static std::string print_node_liveness( Node* current, bool analysis_is_performed );
    static std::string print_node_reaching_defs( Node* current, bool analysis_is_performed );
    static std::string print_node_data_sharing( Node* current, bool analysis_is_performed );
    static std::string print_node_induction_variables( Node* current, bool analysis_is_performed );
    static std::string print_node_deps( Node* current, bool analysis_is_performed );

    void ExtensibleGraph::print_graph_to_dot( bool usage, bool liveness, bool reaching_defs, bool induction_vars,
                                              bool auto_scoping, bool auto_deps )
    {
        std::ofstream dot_pcfg;

        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if( err == NULL )
        {
            internal_error ( "An error occurred while getting the path of the current directory", 0 );
        }

        // Create the directory of dot files if it has not been previously created
        struct stat st;
        std::string directory_name = std::string(buffer) + "/dot/";
        if( stat(directory_name.c_str( ), &st) != 0 )
        {
            int dot_directory = mkdir( directory_name.c_str( ), S_IRWXU );
            if( dot_directory != 0 )
            {
                internal_error ( "An error occurred while creating the dot files directory in '%s'", directory_name.c_str( ) );
            }
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

        Nodecl::NodeclBase node = this->get_nodecl();
        std::string filename = ::give_basename(node.get_filename().c_str());
        int line = node.get_line();

        std::stringstream ss; ss << filename << "_" << line;

        std::string dot_file_name = directory_name + ss.str() + "_" + date_str + ".dot";
        dot_pcfg.open( dot_file_name.c_str( ) );
        if( dot_pcfg.good( ) )
        {   // Create the dot graphs
            if( VERBOSE )
                std::cerr << "- File '" << dot_file_name << "'" << std::endl;

            int subgraph_id = 0;
            dot_pcfg << "digraph CFG {\n";
                dot_pcfg << "\tcompound=true;\n";
                std::string graph_data = "";
                std::string graph_analysis_info = "";
                std::vector<std::string> outer_edges;
                std::vector<Node*> outer_nodes;
                get_nodes_dot_data( _graph, graph_data, graph_analysis_info, outer_edges, outer_nodes, "\t", subgraph_id,
                                    usage, liveness, reaching_defs, induction_vars, auto_scoping, auto_deps );
                dot_pcfg << graph_data;
                dot_pcfg << graph_analysis_info;
            dot_pcfg << "}\n";

            ExtensibleGraph::clear_visits( _graph );

            dot_pcfg.close( );
            if( !dot_pcfg.good( ) )
            {
                internal_error ("Unable to close the file '%s' where PCFG has been stored.", dot_file_name.c_str( ) );
            }
        }
        else
        {
            internal_error ("Unable to open the file '%s' to store the PCFG.", dot_file_name.c_str( ) );
        }
    }

    // Preorder traversal
    void ExtensibleGraph::get_nodes_dot_data( Node* current, std::string& dot_graph, std::string& dot_analysis_info,
                                              std::vector<std::string>& outer_edges, std::vector<Node*>& outer_nodes,
                                              std::string indent, int& subgraph_id,
                                              bool usage, bool liveness, bool reaching_defs, bool induction_vars, 
                                              bool auto_scoping, bool auto_deps )
    {
        if( !current->is_visited( ) )
        {
            current->set_visited( true );
            if( current->is_graph_node( ) )
            {
                // Calculate the name of the new dot subgraph
                std::stringstream node_id; node_id << current->get_id( );
                std::string subgraph_label = "[" + node_id.str( ) + "] " + current->get_graph_type_as_string( )/* + "\n" + current->get_graph_label( ).prettyprint()*/;

                std::stringstream ssgid; ssgid << subgraph_id;
                std::string cluster_name = "cluster" + ssgid.str( );
                dot_graph += indent + "subgraph " + cluster_name + "{\n";
                makeup_dot_block( subgraph_label );
                dot_graph += indent + "\tlabel=\"" + subgraph_label + "\";\n";
                subgraph_id++;

                std::vector<std::string> new_outer_edges;
                std::vector<Node*> new_outer_nodes;
                get_dot_subgraph( current, dot_graph, dot_analysis_info, new_outer_edges, new_outer_nodes, indent, subgraph_id,
                                  usage, liveness, reaching_defs, induction_vars, auto_scoping, auto_deps );
                dot_graph += indent + "}\n";

                print_node_analysis_info( current, dot_analysis_info, cluster_name,
                                          usage, liveness, reaching_defs, induction_vars, auto_scoping, auto_deps );

                for( std::vector<Node*>::iterator it = new_outer_nodes.begin( ); it != new_outer_nodes.end( ); ++it)
                {
                    std::vector<std::string> new_outer_edges_2;
                    std::vector<Node*> new_outer_nodes_2;
                    get_nodes_dot_data( *it, dot_graph, dot_analysis_info, new_outer_edges_2, new_outer_nodes_2, indent, subgraph_id,
                                        usage, liveness, reaching_defs, induction_vars, auto_scoping, auto_deps);
                }
                for( std::vector<std::string>::iterator it = new_outer_edges.begin( ); it != new_outer_edges.end( ); ++it )
                {
                    dot_graph += indent + ( *it );
                }
            }

            if( current->is_exit_node( ) )
            {   // Ending the graph traversal, either the master graph or any subgraph
                get_node_dot_data( current, dot_graph, dot_analysis_info, indent, usage, liveness, reaching_defs );
            }
            else
            {
                bool must_print_following = true;
                std::stringstream sss;
                if( !current->is_graph_node( ) )
                {
                    if( ( !current->has_key( _OUTER_NODE ) ) ||
                        ( current->has_key( _OUTER_NODE ) &&
                            ( ( ! current->is_entry_node( ) && !current->get_entry_edges( ).empty( ) )
                              || current->is_entry_node( ) ) ) )
                    {
                        sss << current->get_id( );
                        get_node_dot_data( current, dot_graph, dot_analysis_info, indent, usage, liveness, reaching_defs );
                    }
                    else
                    {
                        must_print_following = false;
                    }
                }
                else
                {
                    Node* exit_node = current->get_graph_exit_node( );
                    if( !current->get_entry_edges( ).empty( ) && !exit_node->get_entry_edges( ).empty( ) )
                    {
                        sss << exit_node->get_id( );
                    }
                    else
                    {
                        must_print_following = false;
                    }
                }

                if( must_print_following )
                {
                    ObjectList<Edge*> exit_edges = current->get_exit_edges( );
                    for( ObjectList<Edge*>::iterator it = exit_edges.begin( ); it != exit_edges.end( ); ++it )
                    {
                        std::stringstream sst;
                        if( ( *it )->get_target( )->is_graph_node( ) )
                        {
                            sst << ( *it )->get_target( )->get_graph_entry_node( )->get_id( );
                        }
                        else
                        {
                            sst << ( *it )->get_target( )->get_id( );
                        }
                        std::string direction = "";
                        if( sss.str( ) == sst.str( ) )
                        {
                            direction = ", headport=n, tailport=s";
                        }

                        std::string extra_edge_attrs = "";
                        if( ( *it )->is_task_edge( ) )
                        {
                            extra_edge_attrs = ", style=dashed";
                        }

                        if( belongs_to_the_same_graph( *it ) )
                        {
                            dot_graph += indent + sss.str( ) + " -> " + sst.str( ) +
                                         " [label=\"" + ( *it )->get_label( ) + "\"" + direction + extra_edge_attrs + "];\n";
                            get_nodes_dot_data( ( *it )->get_target( ), dot_graph, dot_analysis_info,
                                                outer_edges, outer_nodes, indent, subgraph_id,
                                                usage, liveness, reaching_defs, induction_vars, auto_scoping, auto_deps);
                        }
                        else
                        {
//                             if( !current->is_graph_node( ) )
//                             {
//                                 get_node_dot_data( current, dot_graph, dot_analysis_info, indent, usage, liveness, reaching_defs );
//                             }
                            std::string mes = sss.str( ) + " -> " + sst.str( ) +
                                              " [label=\"" + ( *it )->get_label( ) + "\"" + direction + extra_edge_attrs + "];\n";
                            outer_edges.push_back( mes );
                            outer_nodes.push_back( ( *it )->get_target( ) );
                        }
                    }
                }
            }
        }
    }

    void ExtensibleGraph::get_dot_subgraph( Node* current, std::string& dot_graph, std::string& graph_analysis_info,
                                            std::vector<std::string>& outer_edges, std::vector<Node*>& outer_nodes,
                                            std::string indent, int& subgraph_id,
                                            bool usage, bool liveness, bool reaching_defs, bool induction_vars, 
                                            bool auto_scoping, bool auto_deps )
    {
        switch( current->get_graph_type( ) )
        {
            case ASM_DEF:
                dot_graph += "color=lightsteelblue;\n";
                break;
            case COND_EXPR:
            case FUNC_CALL:
            case IF_ELSE:
            case SPLIT_STMT:
            case SWITCH:
                dot_graph += "color=grey45;\n";
                break;
            case LOOP_DOWHILE:
            case LOOP_FOR:
            case LOOP_WHILE:
                dot_graph += "color=maroon4;\n";
                break;
            case EXTENSIBLE_GRAPH:
                dot_graph += "color=white;\n";
                break;
            case OMP_ATOMIC:
            case OMP_CRITICAL:
            case OMP_LOOP:
            case OMP_PARALLEL:
            case OMP_SECTION:
            case OMP_SECTIONS:
            case OMP_SINGLE:
            case OMP_TASK:
                dot_graph += "color=red4;\nstyle=bold;\n";
                break;
            case SIMD:
            case SIMD_FUNCTION:
                dot_graph += "color=indianred2;\n";
                break;
            default:
                internal_error( "Unexpected node type while printing dot\n", 0 );
        };
        Node* entry_node = current->get_graph_entry_node( );
        _cluster_to_entry_map[current->get_id( )] = entry_node->get_id( );
        get_nodes_dot_data( entry_node, dot_graph, graph_analysis_info, outer_edges, outer_nodes, indent+"\t", subgraph_id,
                            usage, liveness, reaching_defs, induction_vars, auto_scoping, auto_deps );
    }

    void ExtensibleGraph::get_node_dot_data( Node* current, std::string& dot_graph, std::string& graph_analysis_info, std::string indent,
                                             bool usage, bool liveness, bool reaching_defs )
    {
        std::stringstream ss; ss << current->get_id( );
        std::string basic_attrs = "margin=\"0.1,0.1, height=0.1, width=0.1\"";

        switch( current->get_type( ) )
        {
            case ENTRY:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] ENTRY \\n" + "\", shape=box, fillcolor=lightgray, style=filled];\n";
                break;
            }
            case EXIT:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] EXIT\", shape=box, fillcolor=lightgray, style=filled];\n";
                break;
            }
            case UNCLASSIFIED_NODE:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] UNCLASSIFIED_NODE\"]\n";
                break;
            }
            case OMP_BARRIER:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] BARRIER\", shape=diamond]\n";
                break;
            }
            case OMP_FLUSH:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] FLUSH\", shape=ellipse]\n";
                break;
            }
            case OMP_TASKWAIT:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] TASKWAIT\", shape=ellipse]\n";
                break;
            }
            case OMP_VIRTUAL_TASKSYNC:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] POST_SYNC\", shape=ellipse]\n";
                break;
            }
            case BREAK:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] BREAK\", shape=diamond]\n";
                break;
            }
            case CONTINUE:
            {
                dot_graph += indent + ss.str( ) + "[label=\"[" + ss.str( ) + "] CONTINUE\", shape=diamond]\n";
                break;
            }
            case ASM_OP:
            case GOTO:
            case NORMAL:
            case LABELED:
            case FUNCTION_CALL:
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

                    makeup_dot_block(aux_str);
                    basic_block += aux_str + "\\n";
                }
                basic_block = basic_block.substr( 0, basic_block.size( ) - 2 );   // Remove the last back space

                dot_graph += indent + ss.str( ) + "[label=\"{[" + ss.str( ) + "] " + basic_block + "}\", shape=record, "
                           + basic_attrs + "];\n";

                print_node_analysis_info( current, graph_analysis_info, /* cluster name */"",
                                          usage, liveness, reaching_defs, /* induction_vars */ false,
                                          /* auto_scoping */ false, /* auto_deps */ false );

                break;
            }
            default:
                internal_error( "Undefined type of node '%s' founded while printing the graph.",
                                current->get_type_as_string( ).c_str( ) );
        };
    }

    void ExtensibleGraph::print_node_analysis_info( Node* current, std::string& dot_analysis_info,
                                                    std::string cluster_name,
                                                    bool usage, bool liveness, bool reaching_defs, bool induction_vars,
                                                    bool auto_scoping, bool auto_deps )
    {
        std::stringstream node_id; node_id << current->get_id( );
        std::stringstream ssgeid;
        if( !cluster_name.empty( ) )
            ssgeid << _cluster_to_entry_map[current->get_id( )];
        else
            ssgeid << current->get_id( );
        std::string usage_str = print_node_usage( current, usage );
        std::string liveness_str = print_node_liveness( current, liveness );
        std::string reach_defs_str = print_node_reaching_defs( current, reaching_defs );
        std::string induction_vars_str = print_node_induction_variables( current, induction_vars );
        std::string color;
        std::string common_attrs = "style=dashed";
        if( !usage_str.empty( ) )
        {
            std::string id = "-0" + node_id.str( );
            color = "blue";
            dot_analysis_info += "\t" + id + "[label=\"" + usage_str + " \", shape=box, color=" + color + "]\n";
            if( !current->is_extended_graph_node( ) )
            {
                dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color + "]";
                if( !cluster_name.empty() )
                    dot_analysis_info += "[ltail=" + cluster_name + "]\n";
                dot_analysis_info += "\n";
            }
        }
        if( !liveness_str.empty( ) )
        {
            std::string id = "-00" + node_id.str( );
            color = "green3";
            dot_analysis_info += "\t" + id + "[label=\"" + liveness_str + " \", shape=box, color=" + color + "]\n";
            if( !current->is_extended_graph_node( ) )
            {
                dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color + "]";
                if( !cluster_name.empty() )
                    dot_analysis_info += "[ltail=" + cluster_name + "]\n";
                dot_analysis_info += "\n";
            }
        }
        if( !reach_defs_str.empty( ) )
        {
            std::string id = "-000" + node_id.str( );
            color = "red2";
            dot_analysis_info += "\t" + id + "[label=\"" + reach_defs_str + " \", shape=box, color=" + color + "]\n";
            if( !current->is_extended_graph_node( ) )
            {
                dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color + "]";
                if( !cluster_name.empty() )
                    dot_analysis_info += "[ltail=" + cluster_name + "]\n";
                dot_analysis_info += "\n";
            }
        }
        if( !induction_vars_str.empty( ) )
        {
            std::string id = "-0000" + node_id.str( );
            color = "orange2";
            dot_analysis_info += "\t" + id + "[label=\"" + induction_vars_str + " \", shape=box, color=" + color + "]\n";
            if( !current->is_extended_graph_node( ) )
            {
                dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color + "]";
                if( !cluster_name.empty() )
                    dot_analysis_info += "[ltail=" + cluster_name + "]\n";
                dot_analysis_info += "\n";
            }
        }
        if( current->is_omp_task_node( ) )
        {
            std::string auto_scope_str = print_node_data_sharing( current, auto_scoping );
            if( !auto_scope_str.empty() )
            {
                std::string id = "-0000" + node_id.str( );
                color = "darkgoldenrod1";
                dot_analysis_info += "\t" + id + "[label=\"" + auto_scope_str + " \", shape=box, color=" + color + "]\n";
                if( !current->is_extended_graph_node( ) )
                {
                    dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color + "]";
                    if( !cluster_name.empty() )
                        dot_analysis_info += "[ltail=" + cluster_name + "]\n";
                    dot_analysis_info += "\n";
                }
            }

            std::string auto_deps_str = print_node_deps( current, auto_deps );
            if( !auto_deps_str.empty() )
            {
                std::string id = "-00000" + node_id.str( );
                color = "skyblue3";
                dot_analysis_info += "\t" + id + "[label=\"" + auto_deps_str + " \", shape=box, color=" + color + "]\n";
                if( !current->is_extended_graph_node( ) )
                {
                    dot_analysis_info += "\t" + ssgeid.str( ) + " -> " + id + " [" + common_attrs + ", color=" + color + "]";
                    if( !cluster_name.empty() )
                        dot_analysis_info += "[ltail=" + cluster_name + "]\n";
                    dot_analysis_info += "\n";
                }
            }
        }
    }

    static void makeup_dot_block( std::string& str )
    {
        int pos;
        // Escape double quotes
        pos = 0;
        while( ( pos=str.find( "\"", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\\"" );
            pos += 2;
        }
        // Delete implicit line feeds
        pos = 0;
        while( ( pos=str.find( "\n", pos ) ) != -1 ) {
            str.replace ( pos, 1, "" );
        }
        // Delete explicit line feeds
        pos = 0;
        while( ( pos=str.find( "\\n", pos ) ) != -1 ) {
            str.replace ( pos, 2, "\\\\n" );
            pos += 3;
        }
        // Escape the comparison symbols '<' and '>'
        pos = 0;
        while( ( pos=str.find( "<", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\<" );
            pos += 2;
        }
        pos = 0;
        while( ( pos=str.find( ">", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\>" );
            pos += 2;
        }
        // Escape the brackets '{' '}'
        pos = 0;
        while( ( pos=str.find( "{", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\{" );
            pos += 2;
        }
        pos = 0;
        while( ( pos=str.find( "}", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\}" );
            pos += 2;
        }
        // Escape the OR operand
        pos = 0;
        while( ( pos=str.find( "|", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\|" );
            pos += 2;
        }
        // Escape '%' operand
        pos = 0;
        while( ( pos=str.find( "%", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\%" );
            pos += 2;
        }
        // Escape '?' token
        pos = 0;
        while( ( pos=str.find( "?", pos ) ) != -1 ) {
            str.replace ( pos, 1, "\\?" );
            pos += 2;
        }
    }

    static std::string print_node_usage( Node* current, bool analysis_is_performed )
    {
        std::string usage = "";
        if( analysis_is_performed )
        {
            std::string ue = prettyprint_ext_sym_set( current->get_ue_vars( ) );
            std::string killed = prettyprint_ext_sym_set( current->get_killed_vars( ) );
            std::string undef = prettyprint_ext_sym_set( current->get_undefined_behaviour_vars( ) );

            usage = ( killed.empty( )  ? "" : ( "KILL: "   + killed + "\\n" ) )
                    + ( ue.empty( )    ? "" : ( "UE: "     + ue     + "\\n" ) )
                    + ( undef.empty( ) ? "" : ( "UNDEEF: " + undef ) );
        }
        int u_size = usage.size( );
        if( ( u_size > 3 ) && ( usage.substr( u_size - 2, u_size - 1 ) == "\\n" ) )
            usage = usage.substr( 0, u_size - 2 );
        return usage;
    }

    static std::string print_node_liveness( Node* current, bool analysis_is_performed )
    {
        std::string liveness = "";
        if( analysis_is_performed )
        {
            std::string live_in = prettyprint_ext_sym_set( current->get_live_in_vars( ) );
            std::string live_out = prettyprint_ext_sym_set( current->get_live_out_vars( ) );
            liveness = ( live_in.empty( )    ? "" : "LI: " + live_in + "\\n" )
                       + ( live_out.empty( ) ? "" : "LO: " + live_out );
        }
        int l_size = liveness.size( );
        if( ( l_size > 3 ) && ( liveness.substr( l_size - 2, l_size - 1 ) == "\\n" ) )
            liveness = liveness.substr( 0, l_size - 2 );
        return liveness;
    }

    static std::string print_node_reaching_defs( Node* current, bool analysis_is_performed )
    {
        std::string reaching_defs = "";
        if( analysis_is_performed )
        {
            std::string gen = prettyprint_ext_sym_map( current->get_generated_stmts( ) );
            std::string defs_in = prettyprint_ext_sym_map( current->get_reaching_definitions_in( ) );
            std::string defs_out = prettyprint_ext_sym_map( current->get_reaching_definitions_out( ) );
            reaching_defs = ( gen.empty( )        ? "" : "GEN: " + gen + "\\n" )
                            + ( defs_in.empty( )  ? "" : "RDI: " + defs_in + "\\n" )
                            + ( defs_out.empty( ) ? "" : "RDO: " + defs_out );
        }
        int l_size = reaching_defs.size( );
        if( ( l_size > 3 ) && ( reaching_defs.substr( l_size - 2, l_size - 1 ) == "\\n" ) )
            reaching_defs = reaching_defs.substr( 0, l_size - 2 );
        return reaching_defs;
    }

    static std::string print_node_induction_variables( Node* current, bool analysis_is_performed )
    {
        std::string induction_vars = "";
        if( analysis_is_performed && ( current->is_loop_node( ) || current->is_omp_loop_node( ) ) )
        {
            ObjectList<Utils::InductionVariableData*> ivs = current->get_induction_variables( );
            for( ObjectList<Utils::InductionVariableData*>::iterator it = ivs.begin( ); it != ivs.end( ); ++it )
            {
                Utils::InductionVariableData* iv = *it;
                induction_vars = iv->get_variable( ).get_nodecl( ).prettyprint( ) 
                                 + " [ " + iv->get_lb( ).prettyprint( ) 
                                 + ":" + iv->get_ub( ).prettyprint( ) 
                                 + ":" + iv->get_increment( ).prettyprint( ) 
                                 + ":" + iv->get_type_as_string( ) + " ]\\n";
            }
        }
        int l_size = induction_vars.size( );
        if( ( l_size > 3 ) && ( induction_vars.substr( l_size - 2, l_size - 1 ) == "\\n" ) )
            induction_vars = induction_vars.substr( 0, l_size - 2 );
        return induction_vars;
    }
    
    static std::string print_node_data_sharing( Node* current, bool analysis_is_performed )
    {
        std::string auto_scope = "";
        if( analysis_is_performed )
        {
            std::string sc_private = prettyprint_ext_sym_set( current->get_sc_private_vars( ) );
            std::string sc_firstprivate = prettyprint_ext_sym_set( current->get_sc_firstprivate_vars( ) );
            std::string sc_shared = prettyprint_ext_sym_set( current->get_sc_shared_vars( ) );
            std::string sc_undefined = prettyprint_ext_sym_set( current->get_sc_undef_vars( ) );
            auto_scope = ( sc_private.empty( )     ? "" : "AUTO-SC_PRIVATE: "      + sc_private      + "\\n" )
                         + ( sc_private.empty( )   ? "" : "AUTO-SC_FIRSTPRIVATE: " + sc_firstprivate + "\\n" )
                         + ( sc_shared.empty( )    ? "" : "AUTO-SC_SHARED: "       + sc_shared       + "\\n" )
                         + ( sc_undefined.empty( ) ? "" : "AUTO-SC_UNDEFINED: "    + sc_undefined    + "\\n" );
        }
        return auto_scope;
    }

    static std::string print_node_deps( Node* current, bool analysis_is_performed )
    {
        std::string auto_deps = "";
        if( analysis_is_performed )
        {
            std::string deps_private = prettyprint_ext_sym_set( current->get_deps_private_vars( ) );
            std::string deps_firstprivate = prettyprint_ext_sym_set( current->get_deps_firstprivate_vars( ) );
            std::string deps_shared = prettyprint_ext_sym_set( current->get_deps_shared_vars( ) );
            std::string deps_in = prettyprint_ext_sym_set( current->get_deps_in_exprs( ) );
            std::string deps_out = prettyprint_ext_sym_set( current->get_deps_out_exprs( ) );
            std::string deps_inout = prettyprint_ext_sym_set( current->get_deps_inout_exprs( ) );
            std::string deps_undefined = prettyprint_ext_sym_set( current->get_deps_undef_vars( ) );
            auto_deps = ( deps_private.empty( )        ? "" : "AUTO-DEPS_PRIVATE: "      + deps_private      + "\\n" )
                        + ( deps_firstprivate.empty( ) ? "" : "AUTO-DEPS_FIRSTPRIVATE: " + deps_firstprivate + "\\n" )
                        + ( deps_shared.empty( )       ? "" : "AUTO-DEPS_SHARED: "       + deps_shared       + "\\n" )
                        + ( deps_in.empty( )           ? "" : "AUTO-DEPS_IN: "           + deps_in           + "\\n" )
                        + ( deps_out.empty( )          ? "" : "AUTO-DEPS_OUT: "          + deps_out          + "\\n" )
                        + ( deps_inout.empty( )        ? "" : "AUTO-DEPS_INOUT: "        + deps_inout        + "\\n" )
                        + ( deps_undefined.empty( )    ? "" : "AUTO-DEPS_UNDEFINED: "    + deps_undefined    + "\\n" );
        }
        return auto_deps;
    }

    static std::string prettyprint_ext_sym_set( Utils::ext_sym_set s)
    {
        std::string result;

        for( Utils::ext_sym_set::iterator it = s.begin( ); it != s.end( ); ++it )
        {
            result += it->get_nodecl( ).prettyprint( ) + ", ";
        }

        if( !result.empty( ) )
        {
            result = result.substr( 0, result.size( ) - 2 );
            makeup_dot_block( result );
        }

        return result;
    }

    static std::string prettyprint_ext_sym_map( Utils::ext_sym_map s )
    {
        std::string result;

        for( Utils::ext_sym_map::iterator it = s.begin( ); it != s.end( ); ++it )
        {
            nodecl_t first = it->first.get_nodecl( ).get_internal_nodecl( );
            nodecl_t second = it->second.get_internal_nodecl( );

            if( it->second.is_null( ) )
            {
                result += std::string( codegen_to_str( first, nodecl_retrieve_context( first ) ) )
                        + "=UNKNOWN VALUE; ";
            }
            else
            {
                result += std::string( codegen_to_str( first, nodecl_retrieve_context( first ) ) ) + "="
                        + std::string( codegen_to_str( second, nodecl_retrieve_context( second ) ) ) + "; ";
            }
        }

        if( !result.empty( ) )
        {
            result = result.substr( 0, result.size( ) - 2 );
            makeup_dot_block(result);
        }

        return result;
    }
}
}
