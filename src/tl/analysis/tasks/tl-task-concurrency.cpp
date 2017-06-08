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

#include "tl-task-concurrency.hpp"

#include <queue>

namespace TL {
namespace Analysis {
namespace TaskAnalysis{

namespace {
    
    bool task_domains_task(Node* t1, Node* t2, std::set<Node*>& visited_tasks)
    {
        if(t1 == t2)
            return true;
        
        visited_tasks.insert(t1);
        
        const ObjectList<Edge*>& exits = t1->get_exit_edges();
        for (ObjectList<Edge*>::const_iterator it = exits.begin(); it != exits.end(); ++it)
        {
            Node* child = (*it)->get_target();
            if ((child->is_omp_task_node() || child->is_omp_async_target_node())
                && (visited_tasks.find(child)==visited_tasks.end()) // Avoid cycles
                && (((*it)->get_sync_kind() != __Maybe)))           // This path will be taken for sure
            {
                if(task_domains_task(child, t2, visited_tasks))
                    return true;
            }
        }
        
        return false;
    }
    
    bool tasks_are_synchronized(Node* t1, Node* t2)
    {
        std::set<Node*> visited_tasks1, visited_tasks2;
        return task_domains_task(t1, t2, visited_tasks1) || task_domains_task(t2, t1, visited_tasks2);
    }
    
    void collect_tasks_between_nodes(Node* current, Node* last, Node* skip, ObjectList<Node*>& result)
    {
        if (current->is_visited_aux() || (current == last))
            return;

        current->set_visited_aux(true);

        if (current->is_exit_node())
            return;

        if (current->is_graph_node())
        {
            // Add inner tasks recursively, if exist
            collect_tasks_between_nodes(current->get_graph_entry_node(), last, skip, result);

            // Add current node if it is a task or asynchronous target
            if ((current->is_omp_task_node()
                    || current->is_omp_async_target_node())
                && (current != skip)
                && !tasks_are_synchronized(current, skip))
            {
                result.insert(current);
            }
        }

        const ObjectList<Node*>& children = current->get_children();
        for(ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
        {
            collect_tasks_between_nodes(*it, last, skip, result);
        }
    }
    
    void collect_previous_tasks_synchronized_after_scheduling_point(
            Node* task,
            const ObjectList<Node*>& all_tasks,
            ObjectList<Node*>& concurrent_tasks)
    {
        Node* task_creation = ExtensibleGraph::get_task_creation_from_task(task);
        const ObjectList<Node*> children = task->get_children();
        for (ObjectList<Node*>::const_iterator it = all_tasks.begin(); it != all_tasks.end(); ++it)
        {
            if ((*it) == task)
                continue;

            const ObjectList<Node*>& it_children = (*it)->get_children();
            // If some children is a post_sync, then the task is concurrent
            for (ObjectList<Node*>::const_iterator itc = it_children.begin(); itc != it_children.end(); ++itc)
            {
                if ((*itc)->is_omp_virtual_tasksync())
                    concurrent_tasks.insert(*it);
            }
            // If current task is an ancestor of the task and it synchronizes after the task, then it is concurrent
            Node* it_creation = ExtensibleGraph::get_task_creation_from_task((*it));
            while (it_creation != NULL)
            {
                if (ExtensibleGraph::node_is_ancestor_of_node(it_creation, task_creation))
                {
                    // Check whether it synchronizes after the task scheduling point
                    std::queue<Node*> buff;
                    for (ObjectList<Node*>::const_iterator itc = it_children.begin(); itc != it_children.end(); ++itc)
                    {
                        if ((*itc)->is_omp_virtual_tasksync() || *itc == task)
                            continue;
                        buff.push(*itc);
                    }

                    while (!buff.empty())
                    {
                        Node* current = buff.front();
                        buff.pop();
                        
                        for (ObjectList<Node*>::const_iterator itc = children.begin(); itc != children.end(); ++itc)
                        {
                            if (*itc == task)
                                continue;

                            if (ExtensibleGraph::node_is_ancestor_of_node(*itc, current))
                            {
                                concurrent_tasks.insert(*it);
                                goto task_synchronized;
                            }
                            else if ((*itc)->is_omp_task_node()
                                || (*itc)->is_omp_async_target_node())
                            {
                                buff.push(*itc);
                            }
                        }
                    }
task_synchronized:      break;
                }
                else
                {
                    Node* enclosing_task = ExtensibleGraph::get_enclosing_task(it_creation);
                    if (enclosing_task != NULL)
                        it_creation = ExtensibleGraph::get_task_creation_from_task(enclosing_task);
                    else
                        it_creation = NULL;
                }
            }
        }
    }

    bool task_synchronizes_in_all_paths_within_loop(Node* current, Node* task, Node* loop)
    {
        if (current->is_visited())
            return false;

        current->set_visited(true);

        if ((current->is_omp_taskwait_node() || current->is_omp_barrier_graph_node())
                && task->get_children().contains(current))
            return true;

        if (current->is_graph_node())
        {
            current = current->get_graph_entry_node();
            current->set_visited(true);
        }

        // Get the exit edges of the current nodes
        ObjectList<Edge*> exits;
        if (current->is_exit_node())
        {
            Node* current_outer = current->get_outer_node();
            if (current_outer == loop)  // Avoid exiting the enclosing loop
                return false;
            exits = current_outer->get_exit_edges();
        }
        else
            exits = current->get_exit_edges();

        for (ObjectList<Edge*>::iterator it = exits.begin(); it != exits.end(); ++it)
        {
            // Avoid back edges because they are not always taken
            if ((*it)->is_task_edge())
                continue;

            Node* child = (*it)->get_target();
            if (!task_synchronizes_in_all_paths_within_loop(child, task, loop))
                return false;
        }

        return false;
    }

    //! Check whether a task is concurrent with itself
    //! This happens when the task does not have a explicit synchronization with itself and,
    //! in case the task is within a loop, it does not synchronize among iterations
    bool task_is_concurrent_across_iterations(Node* task)
    {
        const ObjectList<Edge*>& exits = task->get_exit_edges();

        // 1.- Check whether the task synchronizes statically with itself
        for (ObjectList<Edge*>::const_iterator it = exits.begin(); it != exits.end(); ++it)
        {
            Node* task_sync = (*it)->get_target();
            if (task_sync == task)
            {
                if ((*it)->get_sync_kind() == __Static)
                    return false;   // The task certainly synchronizes with itself
                break;
            }
        }

        // 2.- Check whether the task certainly synchronizes between two iterations
        Node* task_creation = ExtensibleGraph::get_task_creation_from_task(task);
        Node* task_control_st = task_creation->get_outer_node();
        while (task_control_st != NULL)
        {
            // 2.1.- Get the next loop were the task is nested
            while ((task_control_st != NULL) && !task_control_st->is_loop_node())
                task_control_st = task_control_st->get_outer_node();

            if (task_control_st != NULL)
            {
                bool sync_in_all_paths = task_synchronizes_in_all_paths_within_loop(task_creation, task, task_control_st);
                ExtensibleGraph::clear_visits(task_creation);
                if (sync_in_all_paths)
                    return false;

                task_control_st = task_control_st->get_outer_node();
            }
        }

        return true;
    }

    //! Returns the most outer loop node of a given \p task, or NULL, if no loop exists
    Node* get_most_outer_loop(Node* task)
    {
        Node* loop = NULL;
        Node* outer = task->get_outer_node();
        while (outer != NULL)
        {
            if (outer->is_loop_node())
                loop = outer;
            outer = outer->get_outer_node();
        }
        return loop;
    }
}

    TaskConcurrency::TaskConcurrency(ExtensibleGraph* graph)
            : _graph(graph)
    {}

    void TaskConcurrency::compute_tasks_concurrency()
    {
        const ObjectList<Node*>& tasks = _graph->get_tasks_list();
        for (ObjectList<Node*>::const_iterator it = tasks.begin(); it != tasks.end(); ++it)
        {
            // TODO: This computation must be done just once for each set of tasks
            // Store all tasks that have already been computed as concurrent
            // and do not analyze them again

            // Define the immediately previous and next synchronization points
            define_concurrent_regions_limits(*it);

            // Compute the regions of code that can be simultaneous with the current tasks
            compute_concurrent_tasks(*it);
        }
    }

    void TaskConcurrency::define_concurrent_regions_limits(Node* task)
    {
        // 1.- Compute the last synchronization points for tasks and for sequential code
        // -----------------------------------------------------------------------------
        Node* task_creation = ExtensibleGraph::get_task_creation_from_task(task);
        find_last_synchronization_points_for_sequential_code(task_creation, task);
        find_last_synchronization_points_for_tasks(task_creation, task);
        ExtensibleGraph::clear_visits_backwards(task_creation);

        // 2.- Compute the next synchronization points for tasks and for sequential code
        // -----------------------------------------------------------------------------
        find_next_synchronization_points(task);
    }

    void TaskConcurrency::find_last_synchronization_points_for_sequential_code(Node* task_creation, Node* task)
    {
        // The concurrent sequential code starts when the task is created
        // Unless the task is inside a loop, then the concurrent sequential code starts in the loop
        Node* most_outer_loop = get_most_outer_loop(task_creation);
        Node* last_sync;
        if (most_outer_loop != NULL)
            last_sync = most_outer_loop->get_graph_entry_node();
        else
            last_sync = task_creation;
        _graph->add_last_sync_for_sequential_code(task, last_sync);
    }

    void TaskConcurrency::find_last_synchronization_points_for_tasks(Node* current, Node* task)
    {
        if (current->is_visited())
            return;

        current->set_visited(true);

        // 1.- Get the entry edges of the current node
        ObjectList<Edge*> entries;
        if (current->is_graph_node())
        {
            Node* current_exit = current->get_graph_exit_node();
            current_exit->set_visited(true);
            entries = current_exit->get_entry_edges();
        }
        else if (current->is_entry_node())
        {
            Node* current_outer = current->get_outer_node();
            if (current_outer == NULL)
                return;
            current_outer->set_visited(true);
            entries = current_outer->get_entry_edges();
        }
        else
        {
            entries = current->get_entry_edges();
        }

        // 2.- Look for synchronization points in the entries found in step 1
        for (ObjectList<Edge*>::const_iterator it = entries.begin(); it != entries.end(); ++it)
        {
            // Check for synchronizations in current parent
            Node* parent = (*it)->get_source();
            if (parent->is_omp_barrier_graph_node() || parent->is_omp_taskwait_node())
            {
                _graph->add_last_sync_for_tasks(task, parent);
                Node* parent_cond = ExtensibleGraph::get_enclosing_control_structure(parent);
                if (parent_cond != NULL)
                {
                    // Check whether the synchronization point is a dominator of the task
                    Node* task_cond = ExtensibleGraph::get_enclosing_control_structure(task);
                    while (task_cond != NULL && parent_cond == task_cond)
                    {
                        task_cond = ExtensibleGraph::get_enclosing_control_structure(task_cond);
                    }
                    if (task_cond != NULL)
                    {   // if it is not, then keep looking for synchronizations
                        continue;
                    }
                }
            }
            else if (parent->is_entry_node() && _graph->get_graph()->get_graph_entry_node() == parent)
            {   // If we reach the entry node of the graph, then this is the last synchronization point
                _graph->set_last_sync_for_tasks(task, parent);
                break;
            }

            // Keep iterating
            find_last_synchronization_points_for_tasks(parent, task);
        }
    }

    void TaskConcurrency::find_next_synchronization_points(Node* task)
    {
        const ObjectList<Node*>& next_syncs = task->get_children();
        ERROR_CONDITION(next_syncs.empty(),
                        "%s: All tasks must have at least one synchronization point but task %d does not have any.",
                        task->get_graph_related_ast().get_locus_str().c_str(), task->get_id());

        bool has_post_sync = false;
        for (ObjectList<Node*>::const_iterator it = next_syncs.begin();
             it != next_syncs.end(); ++it)
        {
            if ((*it)->is_omp_virtual_tasksync())
            {
                has_post_sync = true;
                break;
            }
        }

        if (has_post_sync)
        {
            Node* pcfg_exit = _graph->get_graph()->get_graph_exit_node();
            _graph->add_next_sync_for_tasks(task, pcfg_exit);
            _graph->add_next_sync_for_sequential_code(task, pcfg_exit);
        }
        else
        {
            for (ObjectList<Node*>::const_iterator it = next_syncs.begin(); it != next_syncs.end(); ++it)
            {
                _graph->add_next_sync_for_tasks(task, *it);
                _graph->add_next_sync_for_sequential_code(task, *it);
            }
        }
    }

    void TaskConcurrency::compute_concurrent_tasks(Node* task)
    {
        // Collect the tasks that are concurrent with the current task
        ObjectList<Node*> concurrent_tasks;
        const ObjectList<Node*>& last_sync_for_tasks = _graph->get_task_last_sync_for_tasks(task);
        const ObjectList<Node*>& next_sync_for_tasks = _graph->get_task_next_sync_for_tasks(task);
        for (ObjectList<Node*>::const_iterator itl = last_sync_for_tasks.begin();
             itl != last_sync_for_tasks.end(); ++itl)
        {
            // Collect the tasks that are between the last and the next synchronization points
            for (ObjectList<Node*>::const_iterator itn = next_sync_for_tasks.begin();
                 itn != next_sync_for_tasks.end(); ++itn)
            {
                collect_tasks_between_nodes(*itl, *itn, task, concurrent_tasks);
                ExtensibleGraph::clear_visits_aux(*itl);
            }
            
            // Collect any nested task previous to the last synchronization point that has not been synchronized
            if ((*itl)->is_omp_taskwait_node())
            {
                collect_previous_tasks_synchronized_after_scheduling_point(
                        task, _graph->get_tasks_list(), concurrent_tasks);
            }
        }
        for (ObjectList<Node*>::const_iterator it = last_sync_for_tasks.begin();
             it != last_sync_for_tasks.end(); ++it)
            ExtensibleGraph::clear_visits(*it);
        
        // When the task is in a loop and it is not synchronized between iterations, it is be concurrent with itself
        Node* task_creation = ExtensibleGraph::get_task_creation_from_task(task);
        if (ExtensibleGraph::node_is_in_loop(task_creation)
                && task_is_concurrent_across_iterations(task))
            concurrent_tasks.insert(task);

        if (VERBOSE)
        {
            const ObjectList<Node*>& last_sync_for_seq = _graph->get_task_last_sync_for_sequential_code(task);
            const ObjectList<Node*>& next_sync_for_seq = _graph->get_task_next_sync_for_sequential_code(task);
            std::cerr << "    Task " << task->get_id() << " synchronizations information" << std::endl;
            std::cerr << "        * Last synchronizations for sequential code: " << print_node_list(last_sync_for_seq) << std::endl;
            std::cerr << "        * Last synchronizations for other tasks: " << print_node_list(last_sync_for_tasks) << std::endl;
            std::cerr << "        * Next synchronizations for sequential code: " << print_node_list(next_sync_for_seq) << std::endl;
            std::cerr << "        * Next synchronizations for other tasks: " << print_node_list(next_sync_for_tasks) << std::endl;
            std::cerr << "        * Concurrent tasks: " << print_node_list(concurrent_tasks) << std::endl;
        }
        
        // Set the information computed to the graph
        _graph->add_concurrent_task_group(task, concurrent_tasks);
    }

}
}
}