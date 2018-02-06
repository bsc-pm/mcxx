/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona Supe*rcomputing Center             *
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
#include <unistd.h>

#include "tl-task-dependency-graph.hpp"

namespace TL {
namespace Analysis {

    TaskDependencyGraphMapper::TaskDependencyGraphMapper(
        ObjectList<ExpandedTaskDependencyGraph*> etdgs)
        : _etdgs(etdgs)
    {}

    void TaskDependencyGraphMapper::generate_runtime_tdg()
    {
        if (_etdgs.empty())
            return;

        // Get the current directory
        char buffer[1024];
        char* err = getcwd(buffer, 1024);
        if (err == NULL)
            internal_error("An error occurred while getting the path of the current directory", 0);
        std::string directory_name = std::string(buffer);

        // Create the file where we will store the TDG
        std::string source_filename_with_extension = (*_etdgs.begin())->get_ftdg()->get_pcfg()->get_graph()->get_graph_related_ast().get_filename();
        std::size_t filename_extension_position = source_filename_with_extension.find_last_of(".");
        std::string source_filename_without_extension = source_filename_with_extension.substr(0, filename_extension_position);
        std::string file_name = directory_name + "/" + source_filename_without_extension + "_tdg.c";
        std::ofstream rt_tdg;
        rt_tdg.open(file_name.c_str());
        if(!rt_tdg.good())
            internal_error ("Unable to open the file '%s' to store the runtime TDG.", file_name.c_str());

        // Declare the data structure that holds the TDG
        rt_tdg << "// File automatically generated\n";
        rt_tdg << "struct gomp_task;\n";
        rt_tdg << "struct gomp_tdg {\n";
            rt_tdg << "    unsigned long id;\n";
            rt_tdg << "    struct gomp_task *task;\n";
            rt_tdg << "    short offin;\n";
            rt_tdg << "    short offout;\n";
            rt_tdg << "    char nin;\n";
            rt_tdg << "    char nout;\n";
            rt_tdg << "    signed char cnt;\n";
            rt_tdg << "    int map;\n";
            rt_tdg << "    long task_counter;\n";
            rt_tdg << "    long task_counter_end;\n";
            rt_tdg << "    long runtime_counter;\n";
            rt_tdg << "    long taskpart_counter;\n";
        rt_tdg << "};\n";
        rt_tdg << "\n";

        unsigned n_tdg = 0;
        for (ObjectList<ExpandedTaskDependencyGraph*>::iterator it = _etdgs.begin(); it != _etdgs.end(); ++it)
        {
            if ((*it)->get_etdgs().size() != 1)
            {
                WARNING_MESSAGE("TDG mapper only supports 1 level of nesting, but %d found. Runtime TDG may be wrong.",
                                (*it)->get_etdgs().size());
            }
            SubETDG* etdg = (*it)->get_etdgs()[0];
            ObjectList<ETDGNode*> tasks = etdg->get_tasks();

            // Map tasks to their position in the data structure (this is needed to fill inputs and outputs fields)
            std::map<unsigned, unsigned> task_to_position;
            unsigned current_position = 0;
            for (ObjectList<ETDGNode*>::iterator itt = tasks.begin(); itt != tasks.end(); ++itt)
            {
                task_to_position[(*itt)->get_id()] = current_position++;
            }

            // Create the TDG data structure
            rt_tdg << "struct gomp_tdg gomp_tdg_" << n_tdg << "[" << etdg->get_nTasks() << "] = {\n";
            unsigned next_offin = 0;
            unsigned next_offout = 0;
            for (ObjectList<ETDGNode*>::iterator itt = tasks.begin(); itt != tasks.end(); )
            {
                unsigned in_size = (*itt)->get_inputs().size();
                unsigned out_size = (*itt)->get_outputs().size();

                rt_tdg << "{";
                    rt_tdg << ".id = " << (*itt)->get_id() << ",";
                    rt_tdg << ".task = 0,";
                    rt_tdg << ".offin = " << next_offin << ",";
                    rt_tdg << ".offout = " << next_offout << ",";
                    rt_tdg << ".nin = " << in_size << ",";
                    rt_tdg << ".nout = " << out_size << ",";
                    rt_tdg << ".cnt = -1,";
                    rt_tdg << ".map = -1,";
                    rt_tdg << ".task_counter = 0,";
                    rt_tdg << ".task_counter_end = 0,";
                    rt_tdg << ".runtime_counter = 0,";
                    rt_tdg << ".taskpart_counter = 0";
                rt_tdg << "}";

                ++itt;
                if (itt != tasks.end())
                    rt_tdg << ",";
                rt_tdg << "\n";

                next_offin += in_size;
                next_offout += out_size;
            }
            rt_tdg << "};\n";
            rt_tdg << "\n";

            // Create input/output dependencies data structures
            rt_tdg << "unsigned short gomp_tdg_ins_" << n_tdg << "[] = {\n    ";
            char first_in = 1;
            for (ObjectList<ETDGNode*>::iterator itt = tasks.begin(); itt != tasks.end(); ++itt)
            {
                std::set<ETDGNode*> inputs = (*itt)->get_inputs();
                for (std::set<ETDGNode*>::iterator iti = inputs.begin(); iti != inputs.end(); ++iti)
                {
                    if (first_in)
                        first_in = 0;
                    else
                        rt_tdg << ", ";
                    rt_tdg << task_to_position[(*iti)->get_id()];
                }
            }
            rt_tdg << "};\n";
            rt_tdg << "unsigned short gomp_tdg_outs_" << n_tdg << "[] = {\n    ";
            char first_out = 1;
            for (ObjectList<ETDGNode*>::iterator itt = tasks.begin(); itt != tasks.end(); ++itt)
            {
                std::set<ETDGNode*> outputs = (*itt)->get_outputs();
                for (std::set<ETDGNode*>::iterator ito = outputs.begin(); ito != outputs.end(); ++ito)
                {
                    if (first_out)
                        first_out = 0;
                    else
                        rt_tdg << ", ";
                    rt_tdg << task_to_position[(*ito)->get_id()];
                }
            }
            rt_tdg << "};\n";
            rt_tdg << "\n";

            n_tdg++;
        }

        // Create global data structures that contain all TDGs
        unsigned n_tdgs = _etdgs.size();
        rt_tdg << "// All TDGs are store in a single data structure\n";
        rt_tdg << "unsigned gomp_num_tdgs = " << n_tdgs << ";\n";
        rt_tdg << "struct gomp_tdg *gomp_tdg[" << n_tdgs << "] = {\n";
            n_tdg = 0;
            for (ObjectList<ExpandedTaskDependencyGraph*>::iterator it = _etdgs.begin(); it != _etdgs.end(); )
            {
                rt_tdg << "    gomp_tdg_" << n_tdg << "\n";
                ++it;
                if (it != _etdgs.end())
                    rt_tdg << ",";
                n_tdg++;
            }
        rt_tdg << "};\n";
        rt_tdg << "unsigned short *gomp_tdg_ins[" << n_tdgs << "] = {\n";
            n_tdg = 0;
            for (ObjectList<ExpandedTaskDependencyGraph*>::iterator it = _etdgs.begin(); it != _etdgs.end(); )
            {
                rt_tdg << "    gomp_tdg_ins_" << n_tdg;
                ++it;
                if (it != _etdgs.end())
                    rt_tdg << ",";
                rt_tdg << "\n";
                n_tdg++;
            }
        rt_tdg << "};\n";
        rt_tdg << "unsigned short *gomp_tdg_outs[" << n_tdgs << "] = {\n";
            n_tdg = 0;
            for (ObjectList<ExpandedTaskDependencyGraph*>::iterator it = _etdgs.begin(); it != _etdgs.end(); )
            {
                rt_tdg << "    gomp_tdg_outs_" << n_tdg;
                ++it;
                if (it != _etdgs.end())
                    rt_tdg << ",";
                rt_tdg << "\n";
                n_tdg++;
            }
        rt_tdg << "};\n";
        rt_tdg << "unsigned gomp_tdg_ntasks[" << n_tdgs << "] = {\n";
            for (ObjectList<ExpandedTaskDependencyGraph*>::iterator it = _etdgs.begin(); it != _etdgs.end(); )
            {
                rt_tdg << "    " << (*it)->get_etdgs()[0]->get_nTasks() << "\n";
                ++it;
                if (it != _etdgs.end())
                    rt_tdg << ",";
            }
        rt_tdg << "};\n";
        rt_tdg << "unsigned gomp_maxI[" << n_tdgs << "] = {\n";
            for (ObjectList<ExpandedTaskDependencyGraph*>::iterator it = _etdgs.begin(); it != _etdgs.end(); )
            {
                rt_tdg << "    " << (*it)->get_maxI() << "\n";
                ++it;
                if (it != _etdgs.end())
                    rt_tdg << ",";
            }
        rt_tdg << "};\n";
        rt_tdg << "unsigned gomp_maxT[" << n_tdgs << "] = {\n";
            for (ObjectList<ExpandedTaskDependencyGraph*>::iterator it = _etdgs.begin(); it != _etdgs.end(); )
            {
                rt_tdg << "    " << (*it)->get_maxT() << "\n";
                ++it;
                if (it != _etdgs.end())
                    rt_tdg << ",";
            }
        rt_tdg << "};\n";
        rt_tdg << "\n";

        // Delcare methods that are necessary to communicate the application, the compiler and the runtime
        rt_tdg << "// Initialize runtime data-strucures from here.\n";
        rt_tdg << "// This code is called from the compiler.\n";
        rt_tdg << "extern void GOMP_init_tdg(unsigned num_tdgs, struct gomp_tdg **tdg,\n";
        rt_tdg << "                          unsigned short ** tdg_ins, unsigned short ** tdg_outs,\n";
        rt_tdg << "                          unsigned *tdg_ntasks, unsigned *maxI, unsigned *maxT);\n";
        rt_tdg << "extern void GOMP_set_tdg_id(unsigned int);\n";
        rt_tdg << "void gomp_set_tdg(unsigned int tdg_id) {\n";
        rt_tdg << "    GOMP_init_tdg(gomp_num_tdgs, gomp_tdg,\n";
        rt_tdg << "                  gomp_tdg_ins, gomp_tdg_outs, gomp_tdg_ntasks,\n";
        rt_tdg << "                  gomp_maxI, gomp_maxT);\n";
        rt_tdg << "    GOMP_set_tdg_id(tdg_id);\n";
        rt_tdg << "}\n";
    }

}
}