/*--------------------------------------------------------------------
 (C) Copyright 2006-2012 Barcelona Supercomputing Center             *
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

#include "tl-task-sync-analysis.hpp"

namespace TL { namespace Analysis {

TaskSynchronizations::TaskSynchronizations(ExtensibleGraph* graph)
    : _graph(graph)
{
}


#if 0

INITIALIZATION:
for each node n in graph g
   alive_tasks[n] = { } // Conjunt de tasks T vives
   points_of_sync = { }  // Aixo es un conjunt T x N (Tasca i Node, o sigui una aresta de nodes task body a taskwaits)

COMPUTATION:
while (change) // els conjunts alive_tasks i points_of_sync van canviant
   for each node n in graph g
       alive_tasks[n] = (U alive_tasks[p]) for each p in pred(N) // unim totes les tasks vives predecessores
   if n is taskwait with ON == ø
       for each task t in alive_task[n]
           points_of_sync = point_of_sync U {(t, n)}
       alive_tasks[n] = { }
   if n is taskwait with ON != ø
       current_deps = {INOUT(x) | x in ON}
       for each task t in alive_task[t]
           is_there_dependence = is_there_dependence_from_to?(DEPS(t), current_deps)
           if (is_there_dependence == YES)
              // This task is not alive anymore from this point
              alive_tasks[n] = alive_tasks[n] - {t}
              // Proceed like a taskwait without ON, static that there is a synchronization here
              points_of_sync = point_of_sync U {(t, n)}
           else if (is_there_dependence == NO)
              // Do nothing: the task is still alive and does not sync here
           else if (is_there_dependence == UNKNOWN)
              // I we can't prove statically that there is dependence, do not remove it from the alive_tasks[n] set
              // Proceed like a taskwait without ON, state that there is a synchronization here
              points_of_sync = point_of_sync U {(t, n)}
   if n is a task node // Nota: la sincronitzacio es produeix en executar, no en crear!!! (ho feia malament)
       alive_tasks[n] = alive_tasks[n] U { new_task }
       if DEPS(new_task) != ø
          for each task t in alive_task[t]
             // This is like the case taskwait with ON != ø but using DEPS(new_task)
             is_there_dependence = is_there_dependence_from_to?(DEPS(t), DEPS(new_task))
             if (is_there_dependence == YES)
                // This task is not alive anymore from this point
                alive_tasks[n] = alive_tasks[n] - {t}
                points_of_sync = point_of_sync U {(t, n)}
             else if (is_there_dependence == NO)
                // Do nothing: the task will be still alive and does not sync here
             else if (is_there_dependence == UNKNOWN)
                // I we can't prove statically that there is dependence, do not remove it from the alive_tasks[n] set
                // Proceed like a taskwait without ON, state that there is a synchronization here
                points_of_sync = point_of_sync U {(t, n)} 

#endif

static AliveTaskSet& get_alive_in(Node* n)
{
    return n->get_data<AliveTaskSet>("alive_tasks_in");
}

static AliveTaskSet& get_alive_out(Node* n)
{
    return n->get_data<AliveTaskSet>("alive_tasks_out");
}


void TaskSynchronizations::compute_task_synchronizations()
{
    Node *root = _graph->get_graph();

    PointsOfSync points_of_sync;

    bool changes;
    do
    {
        std::cerr << "Computing task synchronizations" << std::endl;
        changes = false;
        int next_domain_id = 1;
        compute_task_synchronizations_rec(root, changes, points_of_sync, 
                /* current_domain_id */ 0, next_domain_id);
        ExtensibleGraph::clear_visits( root );
        std::cerr << std::endl << std::endl;
    } while (changes);

    std::cerr << "Task synchronizations computed" << std::endl;

    for (PointsOfSync::iterator it = points_of_sync.begin();
            it != points_of_sync.end();
            it++)
    {
        for (PointOfSyncSet::iterator jt = it->second.begin();
                jt != it->second.end();
                jt++)
        {
            std::cerr << "CONNECTING " << it->first->get_id() << " -> " << (*jt)->get_id() << std::endl;
            _graph->connect_nodes(it->first, *jt, ALWAYS, "", /*is task edge*/ true);
        }
    }

    Node* post_sync = _graph->create_unconnected_node(Nodecl::NodeclBase::null());
    post_sync->set_type(OMP_VIRTUAL_TASKSYNC);

    Node* exit = root->get_graph_exit_node();
    for (AliveTaskSet::iterator it = get_alive_in(exit).begin();
            it != get_alive_in(exit).end();
            it++)
    {
        std::cerr << "CONNECTING VIRTUAL SYNC " << it->node->get_id() << " -> " << post_sync->get_id() << std::endl;
        _graph->connect_nodes(it->node, post_sync, ALWAYS, "", /*is task edge*/ true);
    }
}

static bool has_task_creation_edges(Node* n)
{
    ObjectList<Edge*> exit_edges = n->get_exit_edges();

    for (ObjectList<Edge*>::iterator edge_it = exit_edges.begin();
            edge_it != exit_edges.end();
            edge_it++)
    {
        if ((*edge_it)->is_task_edge())
            return true;
    }
    return false;
}

// Computes if task source will synchronize with the creation of the task target
TaskSyncRel compute_task_sync_relationship(Node* source, Node* target)
{
    // FIXME: Nothing implemented
    return TaskSync_Unknown;
}

bool task_has_dependences(Node* task)
{
    // FIXME: Not yet implemented
    return false;
}

static ObjectList<Edge*> get_task_creation_edges(Node* n)
{
    ObjectList<Edge*> result;
    ObjectList<Edge*> exit_edges = n->get_exit_edges();

    for (ObjectList<Edge*>::iterator edge_it = exit_edges.begin();
            edge_it != exit_edges.end();
            edge_it++)
    {
        if ((*edge_it)->is_task_edge())
            result.append(*edge_it);
    }

    return result;
}

static std::string print_set(AliveTaskSet& t)
{
    std::stringstream ss;
    ss << "{ ";

    for (AliveTaskSet::iterator it = t.begin();
            it != t.end();
            it++)
    {
        if (it != t.begin())
            ss << ", ";
        ss << "(" << (*it).node->get_id() << ", dom:" << (*it).domain << ")";
    }

    ss << " }";

    return ss.str();
}

void TaskSynchronizations::compute_task_synchronizations_rec(Node* current,
        bool &changed,
        PointsOfSync& points_of_sync,
        int current_domain_id,
        int &next_domain_id)
{
    if (current->is_visited())
        return;

    current->set_visited(true);

    // Propagate predecessors
    {
        AliveTaskSet tmp_alive_tasks_of_current = get_alive_in(current);

        ObjectList<Edge*> predecessors = current->get_entry_edges();
        for (ObjectList<Edge*>::iterator predecessor_it = predecessors.begin();
                predecessor_it != predecessors.end();
                predecessor_it++)
        {
            if ((*predecessor_it)->is_task_edge())
                continue;

            Node* predecessor = (*predecessor_it)->get_source();
            AliveTaskSet& alive_tasks_of_predecessor = get_alive_out(predecessor);

            tmp_alive_tasks_of_current.insert(alive_tasks_of_predecessor.begin(), alive_tasks_of_predecessor.end());
        }

        if (tmp_alive_tasks_of_current != get_alive_in(current))
        {
            get_alive_in(current) = tmp_alive_tasks_of_current;
            std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__ << " Propagating OUT(pred) into IN(current)" << std::endl;
            changed = true;
        }
    }

    std::cerr << "["
        << current->get_id()
        << ":" << current->get_type_as_string()
        << ":" << (current->is_graph_node() ? current->get_graph_type_as_string() : "")
        << "]"
        << "Before: IN(" << current->get_id() << ") = " << print_set(get_alive_in(current)) << " "
        << "OUT(" << current->get_id() << ") = " << print_set(get_alive_out(current)) << std::endl;

    if (current->is_omp_task_node())
    {
        // Synchronize with existing tasks of the same domain
        for (AliveTaskSet::iterator alive_tasks_it = get_alive_in(current).begin();
                alive_tasks_it != get_alive_in(current).end();
                alive_tasks_it++)
        {
            if (alive_tasks_it->domain != current_domain_id)
                continue;

            TaskSyncRel task_sync_rel = compute_task_sync_relationship(alive_tasks_it->node, current);
            switch (task_sync_rel)
            {
                case TaskSync_Unknown :
                case TaskSync_Yes : // Note that we do something slightly different below
                    {
                        if (points_of_sync.find(alive_tasks_it->node) != points_of_sync.end())
                        {
                            if (points_of_sync[alive_tasks_it->node].find(current) == points_of_sync[alive_tasks_it->node].end())
                            {
                                points_of_sync[alive_tasks_it->node].insert(current);
                                std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__
                                    << " task (among others maybe) synchronizes in this task execution" << std::endl;
                                changed = true;
                            }
                        }
                        else
                        {
                            points_of_sync[alive_tasks_it->node].insert(current);
                            std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__
                                << " task (maybe) synchronizes in this task execution" << std::endl;
                            changed = true;
                        }

                        // If we positively know that this task synchronizes here, remove it from alive_tasks
                        if (task_sync_rel == TaskSync_Yes)
                        {
                            size_t removed = get_alive_out(current).erase(*alive_tasks_it);
                            if (removed != 0)
                            {
                                std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__
                                    << " task is not alive after execution" << std::endl;
                            }
                            changed = changed || (removed != 0);
                        }
                        else
                        {
                            std::pair<AliveTaskSet::iterator, bool> res = get_alive_out(current).insert(*alive_tasks_it);
                            if (res.second)
                            {
                                std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__
                                    << " task is still alive after execution" << std::endl;
                            }
                            changed = changed || (res.second);
                        }
                        break;
                    }
                case TaskSync_No :
                    {
                        // We positively know that the task does not synchronize here
                        break;
                    }
                default:
                    {
                        internal_error("Code unreachable", 0);
                    }
            }
        }

        // Note that we do not propagate the code alive task set info inside this task
        int new_domain_id = next_domain_id++;
        compute_task_synchronizations_rec(current->get_graph_entry_node(), changed, points_of_sync, new_domain_id, next_domain_id);
    }
    else if (current->is_graph_node())
    {
        if (get_alive_in(current) != get_alive_in(current->get_graph_entry_node()))
        {
            get_alive_in(current->get_graph_entry_node()) = get_alive_in(current);
            std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__ << " Updating IN(entry) of graph node using IN(current)" << std::endl;
            changed = true;
        }

        compute_task_synchronizations_rec(current->get_graph_entry_node(), changed, points_of_sync, current_domain_id, next_domain_id);

        if (get_alive_out(current) != get_alive_out(current->get_graph_exit_node()))
        {
            get_alive_out(current) = get_alive_out(current->get_graph_exit_node());
            std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__ << " Updating OUT(current) using OUT(exit)" << std::endl;
            changed = true;
        }
    }
    else if (current->is_omp_taskwait_node()
            // FIXME - We need to extract the dependences here and perform a
            // dependency calculation like the one we do for tasks with
            // dependences
            || current->is_ompss_taskwait_on_node())
    {
        // This is a taskwait without dependences
        for (AliveTaskSet::iterator alive_tasks_it = get_alive_in(current).begin();
                alive_tasks_it != get_alive_in(current).end();
                alive_tasks_it++)
        {
            if (alive_tasks_it->domain != current_domain_id)
                continue;

            if (points_of_sync.find(alive_tasks_it->node) != points_of_sync.end())
            {
                if (points_of_sync[alive_tasks_it->node].find(current) == points_of_sync[alive_tasks_it->node].end())
                {
                    points_of_sync[alive_tasks_it->node].insert(current);
                    std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__
                        << " Task synchronizes in this taskwait (among others) of domain " << current_domain_id << std::endl;
                    changed = true;
                }
            }
            else
            {
                points_of_sync[alive_tasks_it->node].insert(current);
                std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__
                    << " Task synchronizes in this taskwait of domain " << current_domain_id << std::endl;
                changed = true;
            }
        }

        // Remove all the tasks that do sync here
        AliveTaskSet still_alive;
        for (AliveTaskSet::iterator alive_tasks_it = get_alive_in(current).begin();
                alive_tasks_it != get_alive_in(current).end();
                alive_tasks_it++)
        {
            if (alive_tasks_it->domain != current_domain_id)
                still_alive.insert(*alive_tasks_it);
        }
        if (still_alive != get_alive_out(current))
        {
            get_alive_out(current) = still_alive;
            changed = true;
        }
    }
    else if (current->is_omp_barrier_node())
    {
        // Synchronize with everything
        for (AliveTaskSet::iterator alive_tasks_it = get_alive_in(current).begin();
                alive_tasks_it != get_alive_in(current).end();
                alive_tasks_it++)
        {
            if (points_of_sync.find(alive_tasks_it->node) != points_of_sync.end())
            {
                if (points_of_sync[alive_tasks_it->node].find(current) == points_of_sync[alive_tasks_it->node].end())
                {
                    points_of_sync[alive_tasks_it->node].insert(current);
                    std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__ << " Task synchronizes in this barrier (among others)" << std::endl;
                    changed = true;
                }
            }
            else
            {
                points_of_sync[alive_tasks_it->node].insert(current);
                std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__ << " Task synchronizes in this barrier" << std::endl;
                changed = true;
            }
        }
        if (!get_alive_out(current).empty())
        {
            get_alive_out(current).clear();
            std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__ << std::endl;
            changed = true;
        }
    }
    else
    {
        // All other nodes just propagate OUT(X) = IN(X)
        for (AliveTaskSet::iterator alive_tasks_it = get_alive_in(current).begin();
                alive_tasks_it != get_alive_in(current).end();
                alive_tasks_it++)
        {
            std::pair<AliveTaskSet::iterator, bool> res = get_alive_out(current).insert(*alive_tasks_it);
            if (res.second)
            {
                std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__ 
                    << " task is still alive after creation" << std::endl;
            }
            changed = changed || (res.second);
        }
    }

    // We accept any kind of node having task creation edges
    if (has_task_creation_edges(current))
    {
        ObjectList<Edge*> task_creation = get_task_creation_edges(current);

        // Update the synchronization info
        for (ObjectList<Edge*>::iterator task_creation_it = task_creation.begin();
                task_creation_it != task_creation.end();
                task_creation_it++)
        {
            Node* task = (*task_creation_it)->get_target();

            std::pair<AliveTaskSet::iterator, bool> res = get_alive_out(current).insert(AliveTaskItem(task, current_domain_id));
            if (res.second)
            {
                std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__ 
                    << " new task alive in " << task->get_id() << " with domain " << current_domain_id << std::endl;
            }
            changed = changed || (res.second);

            // All the alive tasks at the end of the task are also alive here
            if (task->is_graph_node())
            {
                Node* exit_of_task = task->get_graph_exit_node();
                for (AliveTaskSet::iterator alive_tasks_it = get_alive_in(exit_of_task).begin();
                        alive_tasks_it != get_alive_in(exit_of_task).end();
                        alive_tasks_it++)
                {
                    res = get_alive_out(current).insert(*alive_tasks_it);
                    if (res.second)
                    {
                        std::cerr << "CHANGED " << __FILE__ << ":" << __LINE__ 
                            << " task created in task outlives its parent task" << std::endl;
                    }
                    changed = changed || (res.second);
                }
            }
        }
    }

    std::cerr << "["
        << current->get_id()
        << ":" << current->get_type_as_string()
        << ":" << (current->is_graph_node() ? current->get_graph_type_as_string() : "")
        << "]"
        << "After : IN(" << current->get_id() << ") = " << print_set(get_alive_in(current)) << " "
        << "OUT(" << current->get_id() << ") = " << print_set(get_alive_out(current)) << std::endl;

    ObjectList<Edge*> exit_edges = current->get_exit_edges();
    for (ObjectList<Edge*>::iterator edge_it = exit_edges.begin();
            edge_it != exit_edges.end();
            edge_it++)
    {
        compute_task_synchronizations_rec((*edge_it)->get_target(), changed, points_of_sync, current_domain_id, next_domain_id);
    }
}

} }
