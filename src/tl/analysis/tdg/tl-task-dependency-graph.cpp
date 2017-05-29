/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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


#include "tl-task-dependency-graph.hpp"

namespace TL { 
namespace Analysis {

    TaskDependencyGraph::TaskDependencyGraph(
            ExtensibleGraph* pcfg)
    {
        _tdg.etdg = new ExpandedTaskDependencyGraph(pcfg);
        _use_expanded = true;
    }

    TaskDependencyGraph::TaskDependencyGraph(
            ExtensibleGraph* pcfg,
            std::string json_name,
            bool taskparts_enabled)
    {
        _tdg.otdg = new OldTaskDependencyGraph(pcfg, json_name, taskparts_enabled);
        _use_expanded = false;
    }

    void TaskDependencyGraph::print_tdg_to_dot() const
    {
        if (_use_expanded)
        {
            _tdg.etdg->print_tdg_to_dot();
        }
        else
        {
            _tdg.otdg->print_tdg_to_dot();
        }
    }

    ExpandedTaskDependencyGraph* TaskDependencyGraph::get_etdg() const
    {
        ERROR_CONDITION(!_use_expanded,
                        "Requested non-expanded version of the TDG. Cannot retrieve the ETDG.\n",
                        0);
        return _tdg.etdg;
    }

    // ******************************************************************************* //
    // ******** Methods to support generating non-expanded version of the TDG ******** //

    std::string TaskDependencyGraph::get_name() const
    {
        if (_use_expanded)
        {
            return _tdg.etdg->get_ftdg()->get_pcfg()->get_name();
        }
        else
        {
            return _tdg.otdg->get_name();
        }
    }

    void TaskDependencyGraph::print_tdgs_to_json(const ObjectList<TaskDependencyGraph*>& tdgs)
    {
        ObjectList<OldTaskDependencyGraph*> otdgs;
        for (ObjectList<TaskDependencyGraph*>::const_iterator it = tdgs.begin();
             it != tdgs.end(); ++it)
        {
            otdgs.append((*it)->_tdg.otdg);
        }
        OldTaskDependencyGraph::print_tdgs_to_json(otdgs);
    }

    // ******** Methods to support generating non-expanded version of the TDG ******** //
    // ******************************************************************************* //
}
}
