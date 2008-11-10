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
