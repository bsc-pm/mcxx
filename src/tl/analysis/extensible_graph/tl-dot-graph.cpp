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
    static std::string prettyprint_set(std::set<ExtensibleSymbol, ExtensibleSymbol_comp> s);
    
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
        std::string dot_file_name = std::string(buffer) + "/" + _name + ".dot";
        dot_cfg.open(dot_file_name.c_str());
        
        Node* entry = _graph->get_data<Node*>("entry");
        int subgraph_id = 0;
        dot_cfg << "digraph CFG {\n";
            std::string graph_data = "";
            std::vector<std::string> outer_edges;
            std::vector<Node*> outer_nodes;
            get_nodes_dot_data(entry, graph_data, outer_edges, outer_nodes, "\t", subgraph_id);
            dot_cfg << graph_data;
        dot_cfg << "}";
        
        dot_cfg.close();
        
        clear_visits(entry);
    }

    // Preorder traversal 
    Node* ExtensibleGraph::get_nodes_dot_data(Node* actual_node, std::string& dot_graph, 
                                              std::vector<std::string>& outer_edges, std::vector<Node*>& outer_nodes, 
                                              std::string indent, int& subgraph_id)
    {
        if (!actual_node->is_visited())
        {
            actual_node->set_visited(true);
            Node_type ntype = actual_node->get_data <Node_type> ("type");
            if (ntype == GRAPH_NODE)
            {
                std::stringstream ssgid; ssgid << subgraph_id;
//                 std::string subgraph_label = actual_node->get_data<AST_t>("label").prettyprint();
                std::stringstream ssnode; ssnode << actual_node->get_id();
                std::string subgraph_label = ssnode.str();
                std::string subgr_liveness = "LI: "   + prettyprint_set(actual_node->get_live_in_vars()) + "\\n" +
                                             "LO: " + prettyprint_set(actual_node->get_live_out_vars());
                dot_graph += indent + "subgraph cluster" + ssgid.str() + "{\n";
                
//                 makeup_dot_block(subgraph_label);
                dot_graph += indent + "\tlabel=\"" + subgraph_label + "\";\n";
                subgraph_id++;
                
                std::vector<std::string> new_outer_edges;
                std::vector<Node*> new_outer_nodes;
                get_dot_subgraph(actual_node, dot_graph, new_outer_edges, new_outer_nodes, indent, subgraph_id);              
                std::stringstream ss; ss << actual_node->get_id();
                dot_graph += indent + "\t-" + ss.str() + "[label=\"" + subgr_liveness + "\", shape=box]\n";
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
                    if ( (!actual_node->has_key("outer_node")) ||
                         (actual_node->has_key("outer_node") && 
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
                    Node* exit_node = actual_node->get_data<Node*>("exit");
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
                            sst << (*it)->get_target()->get_data<Node*>("entry")->get_id();
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
                        
                        if (belongs_to_the_same_graph(*it))
                        {
                            dot_graph += indent + sss.str() + " -> " + sst.str() + 
                                         " [label=\"" + (*it)->get_label() + "\"" + direction +"];\n";
                            actual_node = get_nodes_dot_data((*it)->get_target(), dot_graph, 
                                                             outer_edges, outer_nodes, 
                                                             indent, subgraph_id);
                        }
                        else
                        {
                            get_node_dot_data(actual_node, dot_graph, indent);
                            std::string mes = sss.str() + " -> " + sst.str() + 
                                              " [label=\"" + (*it)->get_label() + "\"" + direction + "];\n";
                            outer_edges.push_back(mes);
                            outer_nodes.push_back((*it)->get_target());
                        }
                    }
                }
            }            
        }

        return actual_node;
    }

    void ExtensibleGraph::get_dot_subgraph(Node* actual_node, std::string& dot_graph, 
                                           std::vector<std::string>& outer_edges, std::vector<Node*>& outer_nodes,
                                           std::string indent, int& subgraph_id)
    {
        Node* entry_node = actual_node->get_data<Node*>("entry");
        get_nodes_dot_data(entry_node, dot_graph, outer_edges, outer_nodes, indent+"\t", subgraph_id);
    }

    void ExtensibleGraph::get_node_dot_data(Node* actual_node, std::string& dot_graph, std::string indent)
    {
        std::string basic_block = "";
        std::stringstream ss; ss << actual_node->get_id();
        std::stringstream ss2; 
        if (actual_node->has_key("outer_graph"))
            ss2 << actual_node->get_data<Node*>("outer_graph")->get_id();
        else ss2 << "0";
        
        Node_type nt = actual_node->get_data <Node_type> ("type");
        
        if (nt == BASIC_ENTRY_NODE)
        {
            dot_graph += indent + ss.str() + "[label=\"{" + ss.str() + " # ENTRY}\", shape=box, fillcolor=lightgray, style=filled];\n";
        }
        else if (nt == BASIC_EXIT_NODE)
        {
            dot_graph += indent + ss.str() + "[label=\"{" + ss.str() + " # EXIT}\", shape=box, fillcolor=lightgray, style=filled];\n";
        }
        else if (nt == UNCLASSIFIED_NODE)
        {
            dot_graph += indent + ss.str() + "[label=\"{" + ss.str() + " # UNCLASSIFIED_NODE}\"]\n";
        }
        else if (nt == BARRIER_NODE)
        {
            dot_graph += indent + ss.str() + "[label=\"" + ss.str() + " # BARRIER\", shape=diamond]\n";
        }
        else if (nt == FLUSH_NODE)
        {
            dot_graph += indent + ss.str() + "[label=\"" + ss.str() + " # FLUSH\", shape=ellipse]\n";
        }        
        else
        {
            // Get the Statements within the BB
            ObjectList<Nodecl::NodeclBase> node_block = actual_node->get_data <ObjectList<Nodecl::NodeclBase> >("statements");
            std::string aux_str = "";
            for (ObjectList<Nodecl::NodeclBase>::iterator it = node_block.begin();
                    it != node_block.end();
                    it++)
            {
//                 std::cerr << "Adding statement " << c_cxx_codegen_to_str(it->get_internal_nodecl()) << std::endl;
                aux_str = c_cxx_codegen_to_str(it->get_internal_nodecl());
                makeup_dot_block(aux_str);
                basic_block += aux_str + "\\n";
            }
            basic_block = basic_block.substr(0, basic_block.size()-2);   // Remove the last back space
            
            // Get the label of the node, if it has one
            std::string special_attrs;
            if (nt == BASIC_LABELED_NODE || nt == BASIC_GOTO_NODE)
            {
                special_attrs = actual_node->get_data<std::string>("label") + " |";
            }
            dot_graph += indent + ss.str() + "[label=\"{" + ss.str() + " # " + special_attrs + basic_block /*+
                            " | LI: "   + prettyprint_set(actual_node->get_live_in_vars()) + 
                            " | KILL: " + prettyprint_set(actual_node->get_killed_vars()) +
                            " | UE: "   + prettyprint_set(actual_node->get_ue_vars()) +
                            " | LO: "   + prettyprint_set(actual_node->get_live_out_vars())*/ + "}\", shape=record];\n";
        }
    }
    
    static std::string prettyprint_set(std::set<ExtensibleSymbol, ExtensibleSymbol_comp> s)
    {
        std::string result;
        
        for(std::set<ExtensibleSymbol, ExtensibleSymbol_comp>::iterator it = s.begin();
                it != s.end();
                ++it)
        {
            result += it->get_name() + ", ";
        }
        
        return result.substr(0, result.size()-2);
    }
}