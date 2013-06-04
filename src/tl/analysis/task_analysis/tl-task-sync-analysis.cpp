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
#include "tl-datareference.hpp"
#include <algorithm>
#include <iterator>

namespace TL { namespace Analysis {

TaskSynchronizations::TaskSynchronizations(ExtensibleGraph* graph)
    : _graph(graph)
{
}

static AliveTaskSet& get_alive_in(Node* n)
{
    return n->get_data<AliveTaskSet>("alive_tasks_in");
}

static AliveTaskSet& get_alive_out(Node* n)
{
    return n->get_data<AliveTaskSet>("alive_tasks_out");
}

static StaticSyncTaskSet& get_static_sync_in(Node* n)
{
    return n->get_data<StaticSyncTaskSet>("static_sync_in");
}

static StaticSyncTaskSet& get_static_sync_out(Node* n)
{
    return n->get_data<StaticSyncTaskSet>("static_sync_out");
}

void TaskSynchronizations::compute_task_synchronizations()
{
    Node *root = _graph->get_graph();

    PointsOfSync points_of_sync;

    bool changes;
    do
    {
        // std::cerr << "Computing task synchronizations" << std::endl;
        changes = false;
        int next_domain_id = 1;
        compute_task_synchronizations_rec(root, changes, points_of_sync, 
                /* current_domain_id */ 0, next_domain_id);
        ExtensibleGraph::clear_visits( root );
        // std::cerr << std::endl << std::endl;
    } while (changes);

//    std::cerr << "Task synchronizations computed" << std::endl;

    for (PointsOfSync::iterator it = points_of_sync.begin();
            it != points_of_sync.end();
            it++)
    {
        for (PointOfSyncSet::iterator jt = it->second.begin();
                jt != it->second.end();
                jt++)
        {
//            std::cerr << "CONNECTING " << it->first->get_id() << " -> " << (*jt).first->get_id() << std::endl;
            Edge* edge = _graph->connect_nodes(it->first, (*jt).first, ALWAYS, "", /*is task edge*/ true);
            edge->set_label(sync_kind_to_str((*jt).second));
        }
    }

    Node* post_sync = _graph->create_unconnected_node(Nodecl::NodeclBase::null());
    post_sync->set_type(OMP_VIRTUAL_TASKSYNC);

    Node* exit = root->get_graph_exit_node();
    for (AliveTaskSet::iterator it = get_alive_in(exit).begin();
            it != get_alive_in(exit).end();
            it++)
    {
        if (get_static_sync_in(exit).find(*it) == get_static_sync_in(exit).end())
        {
//            std::cerr << "CONNECTING VIRTUAL SYNC " << it->node->get_id() << " -> " << post_sync->get_id() << std::endl;
            Edge* edge = _graph->connect_nodes(it->node, post_sync, ALWAYS, "", /*is task edge*/ true);
            edge->set_label(sync_kind_to_str(Sync_post));
        }
    }
}

TaskSyncRel operator||(const TaskSyncRel& l, const TaskSyncRel r)
{
    if ((l != TaskSync_Unknown) && (r != TaskSync_Unknown))
    {
        return ((l == TaskSync_Yes) || (r == TaskSync_Yes)) ? TaskSync_Yes : TaskSync_No;
    }
    else if (((l == TaskSync_Unknown) && (r == TaskSync_Yes))
            || ((l == TaskSync_Yes) && (r == TaskSync_Unknown)))
    {
        return TaskSync_Yes;
    }
    else if (((l == TaskSync_Unknown) && (r == TaskSync_No))
            || ((l == TaskSync_No) && (r == TaskSync_Unknown)))
    {
        return TaskSync_Unknown;
    }
    else if ((l == r) && (l == TaskSync_Unknown))
    {
        return TaskSync_Unknown;
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

TaskSyncRel operator&&(const TaskSyncRel& l, const TaskSyncRel r)
{
    if ((l != TaskSync_Unknown) && (r != TaskSync_Unknown))
    {
        return ((l == TaskSync_Yes) && (r == TaskSync_Yes)) ? TaskSync_Yes : TaskSync_No;
    }
    else if (((l == TaskSync_Unknown) && (r == TaskSync_Yes))
            || ((l == TaskSync_Yes) && (r == TaskSync_Unknown)))
    {
        return TaskSync_Unknown;
    }
    else if (((l == TaskSync_Unknown) && (r == TaskSync_No))
            || ((l == TaskSync_No) && (r == TaskSync_Unknown)))
    {
        return TaskSync_No;
    }
    else if ((l == r) && (l == TaskSync_Unknown))
    {
        return TaskSync_Unknown;
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

bool is_strict_subobject_access(Nodecl::NodeclBase& data_ref)
{
    if (data_ref.is<Nodecl::Symbol>())
        return false;

    Nodecl::NodeclBase current = data_ref;

    while (!current.is<Nodecl::Symbol>())
    {
        if (current.is<Nodecl::ArraySubscript>())
        {
            Nodecl::NodeclBase subscripted = current.as<Nodecl::ArraySubscript>().get_subscripted();
            if (subscripted.is<Nodecl::Symbol>())
            {
                // a[x]
                if (!subscripted.get_symbol().get_type().is_array())
                    return false;
            }
            else if (subscripted.is<Nodecl::ClassMemberAccess>())
            {
                // b.a[x]
                Nodecl::NodeclBase member = subscripted.as<Nodecl::ClassMemberAccess>().get_member();
                if (!member.is<Nodecl::Symbol>())
                    return false;
                if (!member.get_symbol().get_type().is_array())
                    return false;
            }

            current = subscripted;
        }
        else if (current.is<Nodecl::ClassMemberAccess>())
        {
            Nodecl::NodeclBase lhs = current.as<Nodecl::ClassMemberAccess>().get_lhs();
            if (lhs.is<Nodecl::Symbol>())
            {
                // a.b
                if (!lhs.get_symbol().get_type().is_class())
                    return false;
            }
            else if (lhs.is<Nodecl::ClassMemberAccess>())
            {
                // a.b.c
                Nodecl::NodeclBase member = lhs.as<Nodecl::ClassMemberAccess>().get_member();
                if (!member.is<Nodecl::Symbol>())
                    return false;
                if (!member.get_symbol().get_type().is_class())
                    return false;
            }

            current = lhs;
        }
        else
        {
            return false;
        }
    }

    return true;
}

// This function must be very pessimistic.
// In case of doubt it should always return TaskSync_Unknown
TaskSyncRel may_have_dependence(Nodecl::NodeclBase source, Nodecl::NodeclBase target)
{
    TL::DataReference source_data_ref(source);
    TL::DataReference target_data_ref(target);

    ERROR_CONDITION(!source_data_ref.is_valid()
            || !target_data_ref.is_valid(),
            "Data references should be valid here", 0);

    TL::Symbol source_sym = source_data_ref.get_base_symbol();
    TL::Symbol target_sym = target_data_ref.get_base_symbol();

    bool source_is_object = source.is<Nodecl::Symbol>()
        && !source.get_symbol().get_type().is_any_reference();
    bool target_is_object = target.is<Nodecl::Symbol>()
        && !target.get_symbol().get_type().is_any_reference();

    if (source_is_object && target_is_object)
    {
        // If both data references are simple objects, different names means
        // different names
        return (source_sym == target_sym) ? TaskSync_Yes : TaskSync_No;
    }

    bool source_is_subobject = is_strict_subobject_access(source_data_ref);
    bool target_is_subobject = is_strict_subobject_access(target_data_ref);

    if ((source_is_object || source_is_subobject)
            && (target_is_object || target_is_subobject))
    {
        // If one is object and the other subobject (or both subobjects),
        // if the base symbol is different, they they cannot be the same dependence
        if (source_sym != target_sym)
            return TaskSync_No;
    }

    // IMPROVEMENT: If both are subobjects we could try to deduce if really there is dependence

    // In all other cases we take conservative stance
    return TaskSync_Unknown;
}

TaskSyncRel may_have_dependence_list(Nodecl::List out_deps_source, Nodecl::List in_deps_target)
{
    TaskSyncRel result = TaskSync_No;
    for (Nodecl::List::iterator it = out_deps_source.begin();
            it != out_deps_source.end();
            it++)
    {
        for (Nodecl::List::iterator it2 = in_deps_target.begin();
                it2 != in_deps_target.end();
                it2++)
        {
            result = result || (may_have_dependence(*it, *it2));
        }
    }

    return result;
}

// Computes if task source will synchronize with the creation of the task target
TaskSyncRel compute_task_sync_relationship(Node* source, Node* target)
{
//    std::cerr << "CHECKING DEPENDENCES STATICALLY " << source << " -> " << target << std::endl;

    // TL::ObjectList<Nodecl::NodeclBase> source_statements = source->get_statements();
    // ERROR_CONDITION(source_statements.empty(), "Invalid source statement set", 0);
    Nodecl::NodeclBase task_node_source = source->get_graph_label();
    ERROR_CONDITION(task_node_source.is_null(), "Invalid source task tree", 0);
    ERROR_CONDITION(!task_node_source.is<Nodecl::OpenMP::Task>()
            && !task_node_source.is<Nodecl::OpenMP::TaskCall>(),
            "Expecting an OpenMP::Task or OpenMP::TaskCall source node here got a %s", 
            ast_print_node_type(task_node_source.get_kind()));
    Nodecl::List task_source_env;
    if (task_node_source.is<Nodecl::OpenMP::Task>())
    {
        Nodecl::OpenMP::Task task_source(task_node_source.as<Nodecl::OpenMP::Task>());
        task_source_env = task_source.get_environment().as<Nodecl::List>();
    }
    else if (task_node_source.is<Nodecl::OpenMP::TaskCall>())
    {
        Nodecl::OpenMP::TaskCall task_source(task_node_source.as<Nodecl::OpenMP::TaskCall>());
        task_source_env = task_source.get_site_environment().as<Nodecl::List>();
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    Nodecl::NodeclBase source_dep_in;
    Nodecl::NodeclBase source_dep_out;
    Nodecl::NodeclBase source_dep_inout;
    for (Nodecl::List::iterator it = task_source_env.begin();
            it != task_source_env.end();
            it++)
    {
        if (it->is<Nodecl::OpenMP::DepIn>())
            source_dep_in = *it;
        else if (it->is<Nodecl::OpenMP::DepOut>())
            source_dep_out = *it;
        else if (it->is<Nodecl::OpenMP::DepInout>())
            source_dep_inout = *it;
    }

    // TL::ObjectList<Nodecl::NodeclBase> target_statements = target->get_statements();
    // ERROR_CONDITION(target_statements.empty(), "Invalid target statement set", 0);
    Nodecl::NodeclBase task_node_target = target->get_graph_label();
    ERROR_CONDITION(task_node_source.is_null(), "Invalid target task tree", 0);
    ERROR_CONDITION(!task_node_target.is<Nodecl::OpenMP::Task>()
            && !task_node_target.is<Nodecl::OpenMP::TaskCall>(),
            "Expecting an OpenMP::Task or OpenMP::TaskCall target node here got a %s", 
            ast_print_node_type(task_node_target.get_kind()));
    Nodecl::List task_target_env;
    if (task_node_target.is<Nodecl::OpenMP::Task>())
    {
        Nodecl::OpenMP::Task task_target(task_node_target.as<Nodecl::OpenMP::Task>());
        task_target_env = task_target.get_environment().as<Nodecl::List>();
    }
    else if (task_node_target.is<Nodecl::OpenMP::TaskCall>())
    {
        Nodecl::OpenMP::TaskCall task_target(task_node_target.as<Nodecl::OpenMP::TaskCall>());
        task_target_env = task_target.get_site_environment().as<Nodecl::List>();
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    Nodecl::NodeclBase target_dep_in;
    Nodecl::NodeclBase target_dep_out;
    Nodecl::NodeclBase target_dep_inout;
    for (Nodecl::List::iterator it = task_target_env.begin();
            it != task_target_env.end();
            it++)
    {
        if (it->is<Nodecl::OpenMP::DepIn>())
            target_dep_in = *it;
        else if (it->is<Nodecl::OpenMP::DepOut>())
            target_dep_out = *it;
        else if (it->is<Nodecl::OpenMP::DepInout>())
            target_dep_inout = *it;
    }

    TaskSyncRel may_have_dep = TaskSync_No;

    // DRY
    Nodecl::NodeclBase sources[] = { source_dep_out, source_dep_inout };
    int num_sources = sizeof(sources)/sizeof(sources[0]);
    Nodecl::NodeclBase targets[] = { target_dep_in, target_dep_inout };
    int num_targets = sizeof(targets)/sizeof(targets[0]);

    for (int n_source = 0; n_source < num_sources; n_source++)
    {
        for (int n_target = 0; n_target < num_targets; n_target++)
        {
            if (sources[n_source].is_null()
                    || targets[n_target].is_null())
                continue;

            // Note we (ab)use the fact that DepIn/DepOut/DepInOut all have the
            // same physical layout
            may_have_dep = may_have_dep || may_have_dependence_list(
                    sources[n_source].as<Nodecl::OpenMP::DepOut>().get_out_deps().as<Nodecl::List>(),
                    targets[n_target].as<Nodecl::OpenMP::DepIn>().get_in_deps().as<Nodecl::List>());
        }
    }

    return may_have_dep;
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

#if 0
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
#endif

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
        // IN_{AliveTaskSet}[current] = Union_{p:pred(current)} OUT_{AliveTaskSet}[p]
        ObjectList<Edge*> predecessors = current->get_entry_edges();
        for (ObjectList<Edge*>::iterator predecessor_it = predecessors.begin();
                predecessor_it != predecessors.end();
                predecessor_it++)
        {
            if ((*predecessor_it)->is_task_edge())
                continue;

            Node* predecessor = (*predecessor_it)->get_source();
            AliveTaskSet& alive_tasks_of_predecessor = get_alive_out(predecessor);

            get_alive_in(current).insert(alive_tasks_of_predecessor.begin(), alive_tasks_of_predecessor.end());
        }

        // IN_{StaticSyncTaskSet}[current] = Intersection_{p:pred(current)} OUT_{StaticSyncTaskSet}[p]
        bool first = true;
        StaticSyncTaskSet intersection;
        for (ObjectList<Edge*>::iterator predecessor_it = predecessors.begin();
                predecessor_it != predecessors.end();
                predecessor_it++)
        {
            if ((*predecessor_it)->is_task_edge())
                continue;

            Node* predecessor = (*predecessor_it)->get_source();
            if (first)
            {
                intersection = get_static_sync_out(predecessor);
//                std::cerr << "FIRST ISECT -> " << print_set(intersection) << std::endl;
                first = false;
            }
            else
            {
                StaticSyncTaskSet tmp;
//                std::cerr << "CURRENT ISECT -> " << print_set(intersection) << std::endl;
//                std::cerr << "OPERAND ISECT -> " << print_set(get_static_sync_out(predecessor)) << std::endl;
                std::set_intersection(intersection.begin(),
                        intersection.end(),
                        get_static_sync_out(predecessor).begin(),
                        get_static_sync_out(predecessor).end(),
                        std::inserter(tmp, tmp.begin()));
                intersection = tmp;
//                std::cerr << "COMPUTED ISECT -> " << print_set(intersection) << std::endl;
            }
        }
        get_static_sync_in(current) = intersection;
    }

    AliveTaskSet initial_alive_out = get_alive_out(current);
    StaticSyncTaskSet initial_static_sync = get_static_sync_out(current);

//    std::cerr << "["
//      << current->get_id()
//      << ":" << current->get_type_as_string()
//      << ":" << (current->is_graph_node() ? current->get_graph_type_as_string() : "")
//      << "]"
//      << "Before" << std::endl
//      << "  IN[alive] = " << print_set(get_alive_in(current)) << std::endl
//      << "  OUT[alive] = " << print_set(get_alive_out(current)) << std::endl
//      << "  IN[static_sync] = " << print_set(get_static_sync_in(current)) << std::endl
//      << "  OUT[static_sync] = " << print_set(get_static_sync_out(current)) << std::endl;

    if (current->is_omp_task_node())
    {
        int new_domain_id = next_domain_id++;
        compute_task_synchronizations_rec(current->get_graph_entry_node(), changed, points_of_sync, new_domain_id, next_domain_id);
    }
    else if (current->is_graph_node())
    {
        get_alive_in(current->get_graph_entry_node()) = get_alive_in(current);
        get_static_sync_in(current->get_graph_entry_node()) = get_static_sync_in(current);

        compute_task_synchronizations_rec(current->get_graph_entry_node(), changed, points_of_sync, current_domain_id, next_domain_id);

        get_alive_out(current) = get_alive_out(current->get_graph_exit_node());
        get_static_sync_out(current) = get_static_sync_out(current->get_graph_exit_node());
    }
    else if (current->is_omp_taskwait_node())
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
                if (get_static_sync_in(current).find(*alive_tasks_it) == get_static_sync_in(current).end())
                {
                    points_of_sync[alive_tasks_it->node].insert(std::make_pair(current, Sync_strict));
//                    std::cerr << __FILE__ << ":" << __LINE__
//                        << " Task synchronizes in this taskwait (among others) of domain " << current_domain_id << std::endl;
                }
                else
                {
                    points_of_sync[alive_tasks_it->node].erase(std::make_pair(current, Sync_strict));
                }
            }
            else
            {
                if (get_static_sync_in(current).find(*alive_tasks_it) == get_static_sync_in(current).end())
                {
                    points_of_sync[alive_tasks_it->node].insert(std::make_pair(current, Sync_strict));
//                    std::cerr << __FILE__ << ":" << __LINE__
//                      << " Task synchronizes in this taskwait of domain " << current_domain_id << std::endl;
                }
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
        get_alive_out(current) = still_alive;
    }
    else if (current->is_ompss_taskwait_on_node())
    {
        internal_error("Not yet implemented", 0);
    }
    else if (current->is_omp_barrier_node())
    {
        // Synchronize with every task alive but not statically synchronized
        for (AliveTaskSet::iterator alive_tasks_it = get_alive_in(current).begin();
                alive_tasks_it != get_alive_in(current).end();
                alive_tasks_it++)
        {
            if (points_of_sync.find(alive_tasks_it->node) != points_of_sync.end())
            {
                if (get_static_sync_in(current).find(*alive_tasks_it) == get_static_sync_in(current).end())
                {
                    points_of_sync[alive_tasks_it->node].insert(std::make_pair(current, Sync_strict));
//                    std::cerr << __FILE__ << ":" << __LINE__ << " Task synchronizes in this barrier (among others)" << std::endl;
                }
                else
                {
                    points_of_sync[alive_tasks_it->node].erase(std::make_pair(current, Sync_strict));
                }
            }
            else
            {
                if (get_static_sync_in(current).find(*alive_tasks_it) == get_static_sync_in(current).end())
                {
                    points_of_sync[alive_tasks_it->node].insert(std::make_pair(current, Sync_strict));
//                    std::cerr << __FILE__ << ":" << __LINE__ << " Task synchronizes in this barrier" << std::endl;
                }
            }
        }
        get_alive_out(current).clear();
        get_static_sync_out(current).clear();
    }
    else if (current->is_omp_task_creation_node())
    {
        ObjectList<Edge*> task_creation = get_task_creation_edges(current);
        ERROR_CONDITION(task_creation.size() != 1, "Too many creation edges", 0);
        Node* task = task_creation[0]->get_target();

        // Synchronize with existing tasks of the same domain
        for (AliveTaskSet::iterator alive_tasks_it = get_alive_in(current).begin();
                alive_tasks_it != get_alive_in(current).end();
                alive_tasks_it++)
        {
            if (alive_tasks_it->domain != current_domain_id)
                continue;

            TaskSyncRel task_sync_rel = compute_task_sync_relationship(alive_tasks_it->node, task);
            switch (task_sync_rel)
            {
                case TaskSync_Unknown :
                case TaskSync_Yes :
                    {
                        SyncKind sync_kind = Sync_maybe;
                        if (task_sync_rel == TaskSync_Yes)
                            sync_kind = Sync_static;

                        if (points_of_sync.find(alive_tasks_it->node) != points_of_sync.end())
                        {
                            points_of_sync[alive_tasks_it->node].insert(std::make_pair(task, sync_kind));
//                            std::cerr << __FILE__ << ":" << __LINE__
//                                << " task (among others) maybe synchronizes in this task execution" << std::endl;
                        }
                        else
                        {
                            points_of_sync[alive_tasks_it->node].insert(std::make_pair(task, sync_kind));
//                            std::cerr << __FILE__ << ":" << __LINE__
//                                << " task maybe synchronizes in this task execution" << std::endl;
                        }

                        std::pair<AliveTaskSet::iterator, bool> res = get_alive_out(current).insert(*alive_tasks_it);
                        if (res.second)
                        {
//                            std::cerr << __FILE__ << ":" << __LINE__
//                                << " task is (potentially) still alive after execution" << std::endl;
                        }

                        if (task_sync_rel == TaskSync_Yes)
                        {
//                            std::cerr << __FILE__ << ":" << __LINE__ << " but we know it statically synchronizes" << std::endl;
                            get_static_sync_out(current).insert(*alive_tasks_it);
                        }
                        break;
                    }
                case TaskSync_No :
                    {
                        // We positively know that the task does not synchronize here
                        std::pair<AliveTaskSet::iterator, bool> res = get_alive_out(current).insert(*alive_tasks_it);
                        if (res.second)
                        {
//                            std::cerr << __FILE__ << ":" << __LINE__
//                                << " task is (for sure) still alive after execution" << std::endl;
                        }
                        break;
                    }
                default:
                    {
                        internal_error("Code unreachable", 0);
                    }
            }
        }

        // Add the newly created task as well
        std::pair<AliveTaskSet::iterator, bool> res = get_alive_out(current).insert(AliveTaskItem(task, current_domain_id));
        if (res.second)
        {
//            std::cerr << __FILE__ << ":" << __LINE__
//                << " new task alive in " << task->get_id() << " with domain " << current_domain_id << std::endl;
        }

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
//                    std::cerr << __FILE__ << ":" << __LINE__
//                        << " task created in task outlives its parent task" << std::endl;
                }
            }
        }
    }
    else
    {
        // All other nodes just propagate OUT(X) = IN(X)
        get_alive_out(current) = get_alive_in(current);
        get_static_sync_out(current) = get_static_sync_in(current);
    }

    if (initial_alive_out != get_alive_out(current)
            || initial_static_sync != get_static_sync_out(current))
    {
//        std::cerr << "[" << current->get_id() << "] OUT SET HAS CHANGED" << std::endl;
        changed = true;
    }

//    std::cerr << "["
//      << current->get_id()
//      << ":" << current->get_type_as_string()
//      << ":" << (current->is_graph_node() ? current->get_graph_type_as_string() : "")
//      << "]"
//      << "After" << std::endl
//      << "  IN[alive] = " << print_set(get_alive_in(current)) << std::endl
//      << "  OUT[alive] = " << print_set(get_alive_out(current)) << std::endl
//      << "  IN[static_sync] = " << print_set(get_static_sync_in(current)) << std::endl
//      << "  OUT[static_sync] = " << print_set(get_static_sync_out(current)) << std::endl;

    ObjectList<Edge*> exit_edges = current->get_exit_edges();
    for (ObjectList<Edge*>::iterator edge_it = exit_edges.begin();
            edge_it != exit_edges.end();
            edge_it++)
    {
        compute_task_synchronizations_rec((*edge_it)->get_target(), changed, points_of_sync, current_domain_id, next_domain_id);
    }
}

} }
