/*--------------------------------------------------------------------
(C) Copyright 2006-2009 Barcelona Supercomputing Center 
Centro Nacional de Supercomputacion

This file is part of Mercurium C/C++ source-to-source compiler.

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
#include <unistd.h>

#include "cxx-codegen.h"
#include "tl-extensible-graph.hpp"

namespace TL
{
    static std::string prettyprint_ext_sym_set(ext_sym_set s);
    static std::string prettyprint_sym_list(ObjectList<Symbol> s);
    
    void ExtensibleGraph::makeup_dot_block(std::string &str)
    {
        int pos;
        // Escape double quotes
        pos = 0;
        while ((pos=str.find("\"", pos))!=-1) {
            str.replace(pos, 1, "\\\"");
            pos += 2;
        }
        // Delete implicit line feeds
        pos = 0;
        while ((pos=str.find("\n", pos))!=-1) {
            str.replace(pos, 1, "");
        }
        // Delete explicit line feeds
        pos = 0;
        while ((pos=str.find("\\n", pos))!=-1) {
            str.replace(pos, 2, "\\\\n");
            pos += 3;
        }
        // Escape the comparison symbols '<' and '>'
        pos = 0;
        while ((pos=str.find("<", pos))!=-1) {
            str.replace(pos, 1, "\\<");
            pos += 2;
        }
        pos = 0;
        while ((pos=str.find(">", pos))!=-1) {
            str.replace(pos, 1, "\\>");
            pos += 2;
        }
        // Escape the brackets '{' '}'
        pos = 0;
        while ((pos=str.find("{", pos))!=-1) {
            str.replace(pos, 1, "\\{");
            pos += 2;
        }
        pos = 0;
        while ((pos=str.find("}", pos))!=-1) {
            str.replace(pos, 1, "\\}");
            pos += 2;
        }
        // Escape the OR operand
        pos = 0;
        while ((pos=str.find("|", pos))!=-1) {
            str.replace(pos, 1, "\\|");
            pos += 2;
        }
        // Escape '%' operand
        pos = 0;
        while ((pos=str.find("%", pos))!=-1) {
            str.replace(pos, 1, "\\%");
            pos += 2;
        }
        // Escape '?' token
        pos = 0;
        while ((pos=str.find("?", pos))!=-1) {
            str.replace(pos, 1, "\\?");
            pos += 2;
        }
    }

    void ExtensibleGraph::print_graph_to_dot()
    {
        std::ofstream dot_cfg;
        
        char buffer[1024];
        getcwd(buffer, 1024);
        std::stringstream ss; ss << rand();
        std::string dot_file_name = std::string(buffer) + "/" + _name /*+ ss.str()*/ + ".dot";
        dot_cfg.open(dot_file_name.c_str());
        
        if (dot_cfg.good())
        {
            DEBUG_CODE()
            {
                std::cerr << "=== Printing CFG to file [" << dot_file_name << "]===" << std::endl;
            }
            
            Node* entry = _graph->get_data<Node*>(_ENTRY_NODE);
            int subgraph_id = 0;
            dot_cfg << "digraph CFG {\n";
                std::string graph_data = "";
                std::vector<std::string> outer_edges;
                std::vector<Node*> outer_nodes;
                get_nodes_dot_data(entry, graph_data, outer_edges, outer_nodes, "\t", subgraph_id);
                dot_cfg << graph_data;
            dot_cfg << "}";
            
            clear_visits(entry);
            
            dot_cfg.close();
            
            if (!dot_cfg.good())
            {
                internal_error("Unable to close the file '%s' where CFG has been stored.", dot_file_name.c_str());
            }
        }
        else
        {
            internal_error("Unable to open the file '%s' to store the CFG.", dot_file_name.c_str());
        }
    }

    // Preorder traversal 
    void ExtensibleGraph::get_nodes_dot_data(Node* actual_node, std::string& dot_graph, 
                                             std::vector<std::string>& outer_edges, std::vector<Node*>& outer_nodes, 
                                             std::string indent, int& subgraph_id)
    {
        if (!actual_node->is_visited())
        {
            actual_node->set_visited(true);
            Node_type ntype = actual_node->get_data<Node_type>(_NODE_TYPE);
            if (ntype == GRAPH_NODE)
            {
                std::stringstream ssgid; ssgid << subgraph_id;
                std::stringstream ssnode; ssnode << actual_node->get_id();
                std::string subgraph_label = ssnode.str();
                Nodecl::NodeclBase actual_label(actual_node->get_data<Nodecl::NodeclBase>(_NODE_LABEL, Nodecl::NodeclBase::null()));
                if (!actual_label.is_null())
                {
                    subgraph_label += actual_label.get_text();
                }
                std::string subgr_liveness = "LI: " + prettyprint_ext_sym_set(actual_node->get_live_in_vars()) + "\\n" +
                                             "LO: " + prettyprint_ext_sym_set(actual_node->get_live_out_vars());
                std::string task_deps = "";
                if (actual_node->get_data<std::string>(_GRAPH_TYPE) == "task")
                {
                    task_deps = "\\n"
                                "input: "  + prettyprint_ext_sym_set(actual_node->get_input_deps()) + "\\n" +
                                "output: " + prettyprint_ext_sym_set(actual_node->get_output_deps()) + "\\n" +
                                "inout: "  + prettyprint_ext_sym_set(actual_node->get_inout_deps());                       
                }
                
                dot_graph += indent + "subgraph cluster" + ssgid.str() + "{\n";
                
                makeup_dot_block(subgraph_label);
                dot_graph += indent + "\tlabel=\"" + subgraph_label + "\";\n";
                subgraph_id++;
                
                std::vector<std::string> new_outer_edges;
                std::vector<Node*> new_outer_nodes;
                get_dot_subgraph(actual_node, dot_graph, new_outer_edges, new_outer_nodes, indent, subgraph_id);              
                std::stringstream ss; ss << actual_node->get_id();
                dot_graph += indent + "\t-" + ss.str() + "[label=\"" + subgr_liveness + task_deps + " \", shape=box]\n";
                dot_graph += indent + "}\n";
                
                for(std::vector<Node*>::iterator it = new_outer_nodes.begin();
                        it != new_outer_nodes.end();
                        ++it)
                {
                    std::vector<std::string> new_outer_edges_2;
                    std::vector<Node*> new_outer_nodes_2;
                    get_nodes_dot_data(*it, dot_graph, new_outer_edges_2, new_outer_nodes_2, indent, subgraph_id);
                }                
                for(std::vector<std::string>::iterator it = new_outer_edges.begin();
                        it != new_outer_edges.end();
                        ++it)
                {
                    dot_graph += indent + (*it);
                }
            }
           
            if (ntype == BASIC_EXIT_NODE)
            {   // Ending the graph traversal, either the master graph or any subgraph
                get_node_dot_data(actual_node, dot_graph, indent);
            }
            else
            {          
                bool must_print_following = true;
                std::stringstream sss;
                if (ntype != GRAPH_NODE)
                {
                    if ( (!actual_node->has_key(_OUTER_NODE)) ||
                         (actual_node->has_key(_OUTER_NODE) && 
                            ( (ntype !=BASIC_ENTRY_NODE && !actual_node->get_entry_edges().empty()) ||
                              ntype == BASIC_ENTRY_NODE ) ) )
                    {
                        sss << actual_node->get_id();
                        get_node_dot_data(actual_node, dot_graph, indent);
                    }
                    else
                    {
                        must_print_following = false;
                    }
                }
                else
                {
                    Node* exit_node = actual_node->get_data<Node*>(_EXIT_NODE);
                    if (!actual_node->get_entry_edges().empty() && !exit_node->get_entry_edges().empty())
                    {
                        sss << exit_node->get_id();   
                        ObjectList<Edge*> exit_edges = actual_node->get_exit_edges();
                    }
                    else
                    {
                        must_print_following = false;
                    }
                }
            
                if (must_print_following)
                {
                    ObjectList<Edge*> exit_edges = actual_node->get_exit_edges();                        
                    for(ObjectList<Edge*>::iterator it = exit_edges.begin();
                            it != exit_edges.end();
                            ++it)
                    {                           
                        std::stringstream sst; 
                        if ((*it)->get_target()->get_node_type() == GRAPH_NODE)
                        {    
                            sst << (*it)->get_target()->get_data<Node*>(_ENTRY_NODE)->get_id();
                        }
                        else
                        {                        
                            sst << (*it)->get_target()->get_id();
                        }                          
                        std::string direction = "";
                        if (sss.str() == sst.str())
                        {
                            direction = ", headport=n, tailport=s";
                        }
                        
                        std::string extra_edge_attrs = "";
                        if ((*it)->get_data<Edge_type>(_EDGE_TYPE) == TASK_EDGE)
                        {
                            extra_edge_attrs = ", style=dotted";
                        }                 
                        if (belongs_to_the_same_graph(*it))
                        {
                            dot_graph += indent + sss.str() + " -> " + sst.str() +
                                         " [label=\"" + (*it)->get_label() + "\"" + direction + extra_edge_attrs + "];\n";
                            get_nodes_dot_data((*it)->get_target(), dot_graph, 
                                               outer_edges, outer_nodes, 
                                               indent, subgraph_id);
                        }
                        else
                        {
                            if (ntype != GRAPH_NODE)
                            {  
                                get_node_dot_data(actual_node, dot_graph, indent);
                            }
                            std::string mes = sss.str() + " -> " + sst.str() + 
                                              " [label=\"" + (*it)->get_label() + "\"" + direction + extra_edge_attrs + "];\n";
                            outer_edges.push_back(mes);
                            outer_nodes.push_back((*it)->get_target());
                        }
                    }
                }
            }
        }
    }

    void ExtensibleGraph::get_dot_subgraph(Node* actual_node, std::string& dot_graph, 
                                           std::vector<std::string>& outer_edges, std::vector<Node*>& outer_nodes,
                                           std::string indent, int& subgraph_id)
    {
        Node* entry_node = actual_node->get_data<Node*>(_ENTRY_NODE);
        get_nodes_dot_data(entry_node, dot_graph, outer_edges, outer_nodes, indent+"\t", subgraph_id);
    }

    void ExtensibleGraph::get_node_dot_data(Node* actual_node, std::string& dot_graph, std::string indent)
    {     
        std::string basic_block = "";
        std::stringstream ss; ss << actual_node->get_id();
//         std::stringstream aux;
//             aux << ss.str();
//             aux << ", visit=" << (actual_node->is_visited()) << ". ";
        std::stringstream ss2; 
        if (actual_node->has_key(_OUTER_NODE))
            ss2 << actual_node->get_data<Node*>(_OUTER_NODE)->get_id();
        else ss2 << "0";
        
        switch(actual_node->get_data <Node_type> (_NODE_TYPE))
        {
            case BASIC_ENTRY_NODE: 
                dot_graph += indent + ss.str() + "[label=\"{" + ss.str() + " # ENTRY}\", shape=box, fillcolor=lightgray, style=filled];\n";
                break;
            case BASIC_EXIT_NODE:
                dot_graph += indent + ss.str() + "[label=\"{" + ss.str() + " # EXIT}\", shape=box, fillcolor=lightgray, style=filled];\n";
                break;
            case UNCLASSIFIED_NODE:
                dot_graph += indent + ss.str() + "[label=\"{" + ss.str() + " # UNCLASSIFIED_NODE}\"]\n";
                break;
            case BARRIER_NODE:
                dot_graph += indent + ss.str() + "[label=\"" + ss.str() + " # BARRIER\", shape=diamond]\n";
                break;
            case FLUSH_NODE:
                dot_graph += indent + ss.str() + "[label=\"" + ss.str() + " # FLUSH\", shape=ellipse]\n";
                break;
            case BASIC_PRAGMA_DIRECTIVE_NODE:
                internal_error("'%s' found while printing graph. We must think what to do with this kind of node", 
                               actual_node->get_node_type_as_string().c_str());
                break;
            case BASIC_BREAK_NODE:
                dot_graph += indent + ss.str() + "[label=\"" + ss.str() + " # BREAK\", shape=diamond]\n";
                break;
            case BASIC_CONTINUE_NODE:
                dot_graph += indent + ss.str() + "[label=\"" + ss.str() + " # CONTINUE\", shape=diamond]\n";
                break;
            case BASIC_GOTO_NODE:
            case BASIC_NORMAL_NODE:
            case BASIC_LABELED_NODE:
            case BASIC_FUNCTION_CALL_NODE:
            {
                // Get the Statements within the BB
                ObjectList<Nodecl::NodeclBase> node_block = actual_node->get_data <ObjectList<Nodecl::NodeclBase> >(_NODE_STMTS);
                std::string aux_str = "";
                for (ObjectList<Nodecl::NodeclBase>::iterator it = node_block.begin();
                        it != node_block.end();
                        it++)
                {
                    aux_str = c_cxx_codegen_to_str(it->get_internal_nodecl());
                    makeup_dot_block(aux_str);
                    basic_block += aux_str + "\\n";
                }
                basic_block = basic_block.substr(0, basic_block.size()-2);   // Remove the last back space
            
                dot_graph += indent + ss.str() + "[label=\"{" + ss.str() + " # " + basic_block +
                                " | LI: "   + prettyprint_ext_sym_set(actual_node->get_live_in_vars()) + 
                                " | KILL: " + prettyprint_ext_sym_set(actual_node->get_killed_vars()) +
                                " | UE: "   + prettyprint_ext_sym_set(actual_node->get_ue_vars()) +
                                " | LO: "   + prettyprint_ext_sym_set(actual_node->get_live_out_vars()) + "}\", shape=record];\n";          
    
                break;
            }
            default:
                internal_error("Undefined type of node '%s' founded while printing the graph.", 
                               actual_node->get_node_type_as_string().c_str());
        };
    }
    
    static std::string prettyprint_ext_sym_set(ext_sym_set s)
    {
        std::string result;
        
        for(ext_sym_set::iterator it = s.begin();
                it != s.end();
                ++it)
        {
            if (it->get_nodecl().is_null())
            {
                result += it->get_name() + ", ";
            }
            else
            {
                std::string nodecl_string(c_cxx_codegen_to_str(it->get_nodecl().get_internal_nodecl()));
                result += nodecl_string + ", ";
            }
        }
        
        return result.substr(0, result.size()-2);
    }
    
    static std::string prettyprint_sym_list(ObjectList<Symbol> s)
    {
        std::string result;
        
        for(ObjectList<Symbol>::iterator it = s.begin();
                it != s.end();
                ++it)
        {
            result += it->get_name() + ", ";
        }
        
        return result.substr(0, result.size()-2);
    }    
}