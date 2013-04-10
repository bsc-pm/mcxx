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
   if n creates a task new_task // creates task new_task == té una aresta de sync cap a un subgraph task new_task
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

void TaskSynchronizations::compute_task_synchronizations()
{
    AliveTasks alive_tasks;
    PointsOfSync points_of_sync;

    Node *root = _graph->get_graph();

    bool changes;
    do
    {
        changes = false;
        compute_task_synchronizations_rec(root, changes, alive_tasks, points_of_sync);
        ExtensibleGraph::clear_visits( root );
    } while (changes);
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

void TaskSynchronizations::compute_task_synchronizations_rec(Node* current,
        bool &changed,
        AliveTasks& alive_tasks,
        PointsOfSync& points_of_sync)
{
    if (current->is_visited())
        return;

    current->set_visited(true);

    // Propagate predecessors
    {
        std::set<Node*> alive_tasks_of_current = alive_tasks[current];
        ObjectList<Edge*> predecessors = current->get_entry_edges();
        for (ObjectList<Edge*>::iterator predecessor_it = predecessors.begin();
                predecessor_it != predecessors.end();
                predecessor_it++)
        {
            Node* predecessor = (*predecessor_it)->get_source();
            std::set<Node*> alive_tasks_of_predecessor = alive_tasks[predecessor];

            alive_tasks_of_current.insert(alive_tasks_of_predecessor.begin(), alive_tasks_of_predecessor.end());
        }

        if (alive_tasks_of_current != alive_tasks[current])
        {
            alive_tasks[current] = alive_tasks_of_current;
            changed = true;
        }
    }

    if (current->is_graph_node())
    {
        // TODO: Propagate the current graph node info to the entry node info
        compute_task_synchronizations_rec(current->get_graph_entry_node(),
                changed,
                alive_tasks,
                points_of_sync);
        // TODO: Propagate the exit info to the current graph node info
    }
    else if (current->is_omp_taskwait_node())
    {
        // This is a taskwait without dependences
        if (!alive_tasks[current].empty())
        {
            std::set<Node*> alive_tasks_of_current = alive_tasks[current];
            for (std::set<Node*>::iterator alive_tasks_it = alive_tasks_of_current.begin();
                    alive_tasks_it != alive_tasks_of_current.end();
                    alive_tasks_it++)
            {
                if (points_of_sync.find(*alive_tasks_it) != points_of_sync.end())
                {
                    if (points_of_sync[*alive_tasks_it].find(current) == points_of_sync[*alive_tasks_it].end())
                    {
                        points_of_sync[*alive_tasks_it].insert(current);
                        changed = true;
                    }
                }
                else
                {
                    points_of_sync[*alive_tasks_it].insert(current);
                    changed = true;
                }
            }
            alive_tasks[current].clear();
            changed = true;
        }
    }
    // FIXME Not yet implemented
    // else if (current_deps->is_ompss_taskwait_on_node())
    // {
    // }
    // XXX: We are assuming that taskwaits do not have leaving edges that
    // create tasks (otherwise get rid of the 'else')
    else if (has_task_creation_edges(current))
    {
        ObjectList<Edge*> task_creation = get_task_creation_edges(current);

        // Update the synchronization info
        for (ObjectList<Edge*>::iterator task_creation_it = task_creation.begin();
                task_creation_it != task_creation.end();
                task_creation_it++)
        {
            Node* task = (*task_creation_it)->get_target();
            // Compute a synchronization from an alive task to the existing task
            if (task_has_dependences(task))
            {
                std::set<Node*> alive_tasks_of_current = alive_tasks[current];
                for (std::set<Node*>::iterator alive_tasks_it = alive_tasks_of_current.begin();
                        alive_tasks_it != alive_tasks_of_current.end();
                        alive_tasks_it++)
                {
                    TaskSyncRel task_sync_rel = compute_task_sync_relationship(*alive_tasks_it, task);
                    switch (task_sync_rel)
                    {
                        case TaskSync_Unknown :
                        case TaskSync_Yes : // Note that we do something slightly different below
                            {
                                if (points_of_sync.find(*alive_tasks_it) != points_of_sync.end())
                                {
                                    if (points_of_sync[*alive_tasks_it].find(current) == points_of_sync[*alive_tasks_it].end())
                                    {
                                        points_of_sync[*alive_tasks_it].insert(current);
                                        changed = true;
                                    }
                                }
                                else
                                {
                                    points_of_sync[*alive_tasks_it].insert(current);
                                    changed = true;
                                }

                                // If we positively know that this task synchronizes here, remove it from alive_tasks
                                if (task_sync_rel == TaskSync_Yes)
                                {
                                    size_t removed = alive_tasks[current].erase(*alive_tasks_it);
                                    changed = changed || (removed != 0);
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
            }
        }

        // Update the alive tasks
        for (ObjectList<Edge*>::iterator task_creation_it = task_creation.begin();
                task_creation_it != task_creation.end();
                task_creation_it++)
        {
            Node* task = (*task_creation_it)->get_target();
            if (alive_tasks[current].find(task) == alive_tasks[current].end())
            {
                alive_tasks[current].insert(task);
                changed = true;
            }
        }
    }


    ObjectList<Node*> children = current->get_children( );
    for( ObjectList<Node*>::iterator it = children.begin( ); it != children.end( ); ++it )
    {
        compute_task_synchronizations_rec(*it,
                changed,
                alive_tasks,
                points_of_sync);
    }
}

} }
