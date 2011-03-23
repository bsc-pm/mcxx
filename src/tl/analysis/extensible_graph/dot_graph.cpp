#include <fstream>
#include <sstream>
#include <unistd.h>

#include "extensible_graph.hpp"

namespace TL
{
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
    }

    void ExtensibleGraph::print_graph_to_dot()
    {
        std::ofstream dot_cfg;
        
        char buffer[1024];
        getcwd(buffer, 1024);
        std::string dot_file_name = std::string(buffer) + "/" + _name + ".dot";
        dot_cfg.open(dot_file_name.c_str());
        
        int subgraph_id = 0;
        dot_cfg << "digraph CFG {\n";
            std::string graph_data = "";
            std::vector<std::string> outer_edges;
            std::vector<Node*> outer_nodes;
            Node* exit_node = get_nodes_dot_data(_entry, graph_data, outer_edges, outer_nodes, "\t", subgraph_id);
            
            get_node_dot_data(exit_node, graph_data, "\t");
            dot_cfg << graph_data;
        dot_cfg << "}";
        
        dot_cfg.close();
        
        clear_visits(_entry);
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
                if (actual_node->has_key("pragma"))
                {
                    std::stringstream ssgid; ssgid << subgraph_id;
                    dot_graph += indent + "subgraph cluster" + ssgid.str() + "{\n";
                    dot_graph += indent + "\tlabel=\"" + actual_node->get_data<AST_t>("pragma").prettyprint() + "\";";
                    subgraph_id++;
                }
                
                std::vector<std::string> new_outer_edges;
                std::vector<Node*> new_outer_nodes;
                get_dot_subgraph(actual_node, dot_graph, new_outer_edges, new_outer_nodes, indent, subgraph_id);              
                
                if (actual_node->has_key("pragma"))
                {
                    dot_graph += indent + "}\n";
                }
                
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
                return actual_node;
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
                        
                        if (belongs_to_the_same_graph(*it))
                        {
                            dot_graph += indent + sss.str() + " -> " + sst.str() + 
                                         " [label=\"" + (*it)->get_label() + "\"];\n";
                            actual_node = get_nodes_dot_data((*it)->get_target(), dot_graph, 
                                                             outer_edges, outer_nodes, 
                                                             indent, subgraph_id);
                        }
                        else
                        {
                            get_node_dot_data(actual_node, dot_graph, indent);
                            std::string mes = sss.str() + " -> " + sst.str() + 
                                              " [label=\"" + (*it)->get_label() + "\"];\n";
                                              std::cout << "Outer edge: " << mes << std::endl;
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
        
        Node* exit_node = actual_node->get_data<Node*>("exit");
        if (!exit_node->get_entry_edges().empty())
        {
            get_node_dot_data(exit_node, dot_graph, indent+"\t");
        }
    }

    void ExtensibleGraph::get_node_dot_data(Node* actual_node, std::string& dot_graph, std::string indent)
    {
        std::string basic_block = "";
        std::stringstream ss; ss << actual_node->get_id();
        Node_type nt = actual_node->get_data <Node_type> ("type");
        if (nt != BASIC_ENTRY_NODE && nt != BASIC_EXIT_NODE)
        {
            // TODO If the node is Labelled, we must add a label to the node in the dot grap
            ObjectList<AST_t> node_block = actual_node->get_data <ObjectList<AST_t> >("Statements");
            if (nt == BASIC_LABELED_NODE)
                std::cout << "node_block has " << node_block.size() << " elements" << std::endl;
            std::string aux_str = "";
            for (ObjectList<AST_t>::iterator it = node_block.begin();
                    it != node_block.end();
                    it++)
            {
                aux_str = it->prettyprint();
                makeup_dot_block(aux_str);
                basic_block += aux_str + "\\n";
            }
            basic_block = basic_block.substr(0, basic_block.size()-2);   // Remove the last back space
            dot_graph += indent + ss.str() + "[label=\"{" + ss.str() + " - " + basic_block +
                        /*  " | KILL: " + kill_var +
                        " | UE: " + ue_var +
                        " | LO: " + live_out +*/ "}\", shape=record];\n";
        }
        else if (nt == BASIC_ENTRY_NODE)
        {
            dot_graph += indent + ss.str() + "[label=" + ss.str() + "\"ENTRY\", shape=box, fillcolor=lightgray, style=filled];\n";
        }
        else if (nt == BASIC_EXIT_NODE)
        {
            dot_graph += indent + ss.str() + "[label=" + ss.str() + "\"EXIT\", shape=box, fillcolor=lightgray, style=filled];\n";
        }
    }
}