/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - David Rodenas Pico
    Copyright (C) 2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "acotes-outputtasks.hpp"

namespace TL
{
    void OutputTasks::add_task(const OutputTask &output_task)
    {
        if (_map_tasks.find(output_task.filename) == _map_tasks.end())
        {
            ObjectList<OutputTask> output_task_list;

            output_task_list.append(output_task);

            _map_tasks[output_task.filename] = output_task_list;
        }
        else
        {
            _map_tasks[output_task.filename].append(output_task);
        }
    }

    ObjectList<std::string> OutputTasks::get_files() const
    {
        ObjectList<std::string> result;

        for (std::map<std::string, ObjectList<OutputTask> >::const_iterator it = _map_tasks.begin();
                it != _map_tasks.end();
                it++)
        {
            result.insert(it->first);
        }

        return result;
    }

    ObjectList<OutputTask> OutputTasks::get_file_tasks(const std::string &filename) const
    {
        if (_map_tasks.find(filename) == _map_tasks.end())
        {
            ObjectList<OutputTask> output_task_list;
            return output_task_list;
        }
        else
        {
            return _map_tasks.find(filename)->second;
        }
    }
}
