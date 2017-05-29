/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona Supercomputing Center             * *
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
#include <unistd.h>

#include "tl-task-dependency-graph.hpp"


namespace TL { 
namespace Analysis {

    void OldTaskDependencyGraph::print_tdg_node_to_dot(TDG_Node* tdg_n, std::ofstream& dot_tdg) const
    {
        // Print the control structures (subgraphs) where the node is enclosed in
        ControlStList control_structures = tdg_n->get_control_structures();
        std::string indent = "\t";
        unsigned int n_cs = 0;
        for (ControlStList::const_reverse_iterator it = control_structures.rbegin();
             it != control_structures.rend(); ++it)
             {
                 ControlStructure* cs = it->first;
                 if (cs->get_type() != Implicit)
                 {
                     dot_tdg << indent << "subgraph cluster_" << ++tdg_node_id << "{\n";
                     indent += "\t";
                     std::string label = cs->get_condition().prettyprint();
                     if (cs->get_type()==IfElse) {
                         std::string branch = it->second;
                         label += " (" + std::string(branch=="1" ? "true" : "false") + ")";
                     }
                     dot_tdg << indent << "label=\"" << label << "\";\n";
                     dot_tdg << indent << "color=\"" << (cs->get_type()==Loop ? "deeppink" : "deepskyblue1") << "\";\n";
                     dot_tdg << indent << "style=\"dashed\";\n";
                     ++n_cs;
                 }
             }
             
             // Create the node
             Node* n = tdg_n->_pcfg_node;
             std::string task_label = "";
             TDGNodeType ntype = tdg_n->_type;
             if(ntype == Task)
             {
                 // Get the name of the task
                 Nodecl::OpenMP::Task task = n->get_graph_related_ast().as<Nodecl::OpenMP::Task>();
                 task_label = "Task :: " + task.get_locus_str();
                 Nodecl::List environ = task.get_environment().as<Nodecl::List>();
                 for(Nodecl::List::iterator it = environ.begin(); it != environ.end(); ++it)
                     if(it->is<Nodecl::OmpSs::TaskLabel>())
                     {
                         task_label = "_" + it->prettyprint();
                         break;
                     }
             }
             else if (ntype == Taskpart)
             {
                 NBase tp_stmt = n->get_statements()[0];
                 std::stringstream ss;
                 ss << tdg_n->_init_taskpart->get_id();
                 ss << " -> ";
                 ss << tdg_n->_pcfg_node->get_id();
                 task_label = "Taskpart :: " + ss.str();
             }
             else if(ntype == Taskwait)
             {
                 NBase tw_stmt = n->get_statements()[0];
                 task_label = "Taskwait :: " + tw_stmt.get_locus_str();
             }
             else if(ntype == Barrier)
             {
                 NBase barrier_stmt = n->get_graph_related_ast();
                 task_label = "Barrier :: " + barrier_stmt.get_locus_str();
             }
             
             // print the node
             std::stringstream ss; ss << tdg_n->_id;
             std::string current_id = ss.str();
             dot_tdg << indent << current_id << " [label=\"[" << current_id << "] " << task_label << "\"];\n";
             
             
             // Close the subgraphs of the control structures
             for(unsigned int i = 0; i < n_cs; ++i)
             {
                 indent = indent.substr(0, indent.size()-1);
                 dot_tdg << indent << "}\n";
             }
             
             // Create the connections from the current node to its children
             std::string headlabel, taillabel, style, condition;
             for(TDG_Edge_list::iterator it = tdg_n->_exits.begin(); it != tdg_n->_exits.end(); ++it)
             {
                 // Get the edge info in a string
                 style = "style=\"" + std::string((*it)->_kind == __Static ? "solid" : "dashed") + "\"";
                 if(!(*it)->_condition.is_null())
                     condition = ", label=\"" + (*it)->_condition.prettyprint() + "\"";
                 else
                     condition = ", label=\"true\"";
                 // Create the dot edge
                 std::stringstream child_id; child_id << (*it)->_target->_id;
                 dot_tdg << "\t" << current_id << " -> " << child_id.str() 
                 << "[" << style << condition /*<< headlabel << ", " << taillabel*/ << "];\n";
             }
    }
    
    void OldTaskDependencyGraph::print_tdg_to_dot() const
    {
        // Create the directory of dot files if it has not been previously created
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if(err == NULL)
            internal_error ("An error occurred while getting the path of the current directory", 0);
        struct stat st;
        std::string directory_name = std::string(buffer) + "/dot/";
        if(stat(directory_name.c_str(), &st) != 0)
        {
            int dot_directory = mkdir(directory_name.c_str(), S_IRWXU);
            if(dot_directory != 0)
                internal_error ("An error occurred while creating the dot directory in '%s'", 
                                directory_name.c_str());
        }
        
        // Create the file where we will store the DOT TDG
        std::string dot_file_name = directory_name + _pcfg->get_name() + "_tdg.dot";
        std::ofstream dot_tdg;
        dot_tdg.open(dot_file_name.c_str());
        if(!dot_tdg.good())
            internal_error ("Unable to open the file '%s' to store the TDG.", dot_file_name.c_str());
        
        // Create the DOT graphs
        if(VERBOSE)
            std::cerr << "- TDG DOT file '" << dot_file_name << "'" << std::endl;
        dot_tdg << "digraph TDG {\n";
        dot_tdg << "\tcompound=true;\n";
        for (TDG_Node_map::const_iterator it = _tdg_nodes.begin(); it != _tdg_nodes.end(); ++it)
            print_tdg_node_to_dot(it->second, dot_tdg);
        dot_tdg << "}\n";
        dot_tdg.close();
        if(!dot_tdg.good())
            internal_error ("Unable to close the file '%s' where TDG has been stored.", dot_file_name.c_str());
        ExtensibleGraph::clear_visits(_pcfg->get_graph());
    }

    void ExpandedTaskDependencyGraph::print_tdg_to_dot_rec(ETDGNode* n, std::ofstream& dot_tdg)
    {
        if (n->is_visited())
            return;

        n->set_visited(true);
        // Print the node
        std::stringstream ss_target; ss_target << n->get_id();
        std::string target_id = ss_target.str();
        dot_tdg << "\t" << target_id << " [label=\"" << target_id << "\"];\n";

         // Print the entry edges
        const std::set<ETDGNode*>& inputs = n->get_inputs();
        for (std::set<ETDGNode*>::const_iterator it = inputs.begin(); it != inputs.end(); ++it)
        {
            std::stringstream ss_source; ss_source << (*it)->get_id();
            std::string source_id = ss_source.str();
            dot_tdg << "\t" << source_id << " -> " << target_id << ";\n";
        }

        // Keep iterating
        const std::set<ETDGNode*>& outputs = n->get_outputs();
        for (std::set<ETDGNode*>::const_iterator it = outputs.begin(); it != outputs.end(); ++it)
        {
            print_tdg_to_dot_rec(*it, dot_tdg);
        }
    }
    
    void ExpandedTaskDependencyGraph::print_tdg_to_dot()
    {
        // Create the directory of dot files if it has not been previously created
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if(err == NULL)
            internal_error ("An error occurred while getting the path of the current directory", 0);
        struct stat st;
        std::string directory_name = std::string(buffer) + "/dot/";
        if(stat(directory_name.c_str(), &st) != 0)
        {
            int dot_directory = mkdir(directory_name.c_str(), S_IRWXU);
            if(dot_directory != 0)
                internal_error ("An error occurred while creating the dot directory in '%s'",
                                directory_name.c_str());
        }

        // Create the file where we will store the DOT TDG
        std::string dot_file_name = directory_name + _ftdg->get_pcfg()->get_name() + "_etdg.dot";
        std::ofstream dot_tdg;
        dot_tdg.open(dot_file_name.c_str());
        if(!dot_tdg.good())
            internal_error ("Unable to open the file '%s' to store the ETDG.", dot_file_name.c_str());

        // Create the DOT graphs
        if(VERBOSE)
            std::cerr << "- ETDG DOT file '" << dot_file_name << "'" << std::endl;
        dot_tdg << "digraph ETDG {\n";
        dot_tdg << "\tcompound=true;\n";
        for (ObjectList<ETDGNode*>::iterator it = _roots.begin(); it != _roots.end(); ++it)
            print_tdg_to_dot_rec(*it, dot_tdg);
        dot_tdg << "}\n";
        dot_tdg.close();
        if(!dot_tdg.good())
            internal_error ("Unable to close the file '%s' where ETDG has been stored.", dot_file_name.c_str());
        clear_visits();
    }

    unsigned ftdg_node_id = 1;
    unsigned ftdg_cluster_id = 1;
    void FlowTaskDependencyGraph::print_tdg_node_to_dot(
            FTDGNode* n,
            std::string indent, std::string color,
            std::ofstream& dot_tdg)
    {
        switch (n->get_type())
        {
            case FTDGLoop:
            {
                dot_tdg << indent << "subgraph cluster_" << ftdg_cluster_id++ << " {\n";
                    dot_tdg << indent << "   label=Loop_" << n->get_pcfg_node()->get_id() << ";\n";
                    dot_tdg << indent << "   color=" << color << ";\n";
                    const ObjectList<FTDGNode*>& inner = n->get_inner();
                    for (ObjectList<FTDGNode*>::const_iterator it = inner.begin(); it != inner.end(); ++it)
                    {
                        print_tdg_node_to_dot(*it, indent+"   ", color, dot_tdg);
                    }
                dot_tdg << indent << "}\n";
                break;
            }
            case FTDGCondition:
            {
                dot_tdg << indent << "subgraph cluster_" << ftdg_cluster_id++ << " {\n";
                    dot_tdg << indent << "   label=IfElse_" << n->get_pcfg_node()->get_id() << ";\n";
                    dot_tdg << indent << "   color=" << color << ";\n";
                    const ObjectList<FTDGNode*>& inner_true = n->get_inner_true();
                    for (ObjectList<FTDGNode*>::const_iterator it = inner_true.begin(); it != inner_true.end(); ++it)
                    {
                        print_tdg_node_to_dot(*it, indent+"   ", "deepskyblue", dot_tdg);
                    }
                    const ObjectList<FTDGNode*>& inner_false = n->get_inner_false();
                    for (ObjectList<FTDGNode*>::const_iterator it = inner_false.begin(); it != inner_false.end(); ++it)
                    {
                        print_tdg_node_to_dot(*it, indent+"   ", "deeppink1", dot_tdg);
                    }
                dot_tdg << indent << "}\n";
                break;
            }
            case FTDGTarget:
            {
                dot_tdg << indent << ftdg_node_id++
                        << " [label=\"Target " << n->get_pcfg_node()->get_id()
                        << "\", fillcolor=" << color << ", style=filled];\n";
                break;
            }
            case FTDGTask:
            {
                dot_tdg << indent << ftdg_node_id++
                        << " [label=\"Task " << n->get_pcfg_node()->get_id()
                        << "\", fillcolor=" << color << ", style=filled];\n";
                break;
            }
            case FTDGTaskwait:
            {
                dot_tdg << indent << ftdg_node_id++
                        << " [label=\"Taskwait " << n->get_pcfg_node()->get_id()
                        << "\", fillcolor=" << color << ", style=filled];\n";
                break;
            }
            case FTDGBarrier:
            {
                dot_tdg << indent << ftdg_node_id++
                        << " [label=\"Barrier " << n->get_pcfg_node()->get_id()
                        << "\", fillcolor=" << color << ", style=filled];\n";
                break;
            }
            default:
            {
                internal_error("Unexpected node type %d\n.", n->get_type());
            }
        };
    }

    void FlowTaskDependencyGraph::print_tdg_to_dot()
    {
        // Create the directory of dot files if it has not been previously created
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if(err == NULL)
            internal_error ("An error occurred while getting the path of the current directory", 0);
        struct stat st;
        std::string directory_name = std::string(buffer) + "/dot/";
        if(stat(directory_name.c_str(), &st) != 0)
        {
            int dot_directory = mkdir(directory_name.c_str(), S_IRWXU);
            if(dot_directory != 0)
                internal_error ("An error occurred while creating the dot directory in '%s'",
                                directory_name.c_str());
        }

        // Create the file where we will store the DOT TDG
        std::string dot_file_name = directory_name + _pcfg->get_name() + "_ftdg.dot";
        std::ofstream dot_tdg;
        dot_tdg.open(dot_file_name.c_str());
        if(!dot_tdg.good())
            internal_error ("Unable to open the file '%s' to store the FTDG.", dot_file_name.c_str());

        // Create the DOT graph
        if(VERBOSE)
            std::cerr << "- FTDG DOT file '" << dot_file_name << "'" << std::endl;
        dot_tdg << "digraph FTDG {\n";
        dot_tdg << "   compound=true;\n";
        std::set<int> outermost_nodes_ids;
        for (std::vector<FTDGNode*>::iterator it = _outermost_nodes.begin(); it != _outermost_nodes.end(); ++it)
        {
            outermost_nodes_ids.insert(ftdg_node_id);
            print_tdg_node_to_dot(*it, /*indent*/"   ", /*color*/"black", dot_tdg);
        }
        std::set<int>::iterator it = outermost_nodes_ids.begin();
        while (it != outermost_nodes_ids.end())
        {
            int tmp = *it;
            ++it;
            if (it != outermost_nodes_ids.end())
                dot_tdg << tmp << " -> " << *it << ";\n";
        }
        dot_tdg << "}\n";
        dot_tdg.close();
        if(!dot_tdg.good())
            internal_error ("Unable to close the file '%s' where FTDG has been stored.", dot_file_name.c_str());
    }
}
}